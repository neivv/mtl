use std::cell::{RefCell, RefMut};
use std::ops::Range;

use byteorder::{WriteBytesExt, LE};
use rand::distributions::{Distribution, Uniform};
use rand::SeedableRng;
use rand_xorshift::XorShiftRng;

use crate::bw;

ome2_thread_local! {
    RNG: RefCell<Rng> = rng(RefCell::new(Rng(None)));
}

pub fn get() -> RefMut<'static, Rng> {
    rng().borrow_mut()
}

pub fn set_rng(value: Rng) {
    *rng().borrow_mut() = value;
}

// Option as this should be tied to bw's seed, but I'm not sure if the seed is set
// at game init.
#[derive(Clone, Serialize, Deserialize, Debug, Default)]
pub struct Rng(Option<XorShiftRng>);

impl Rng {
    pub fn synced_rand(&mut self, range: Range<u32>) -> u32 {
        let rng = self.0.get_or_insert_with(|| {
            let mut buf = [0x42; 16];
            (&mut buf[..]).write_u32::<LE>(bw::rng_seed()).unwrap();
            XorShiftRng::from_seed(buf)
        });
        Uniform::from(range).sample(rng)
    }
}
