use arrayvec::{self, ArrayVec};

use bw_dat::Unit;

use crate::bw;

pub struct Selection {
    units: ArrayVec<[Unit; 12]>,
}

impl IntoIterator for Selection {
    type Item = Unit;
    type IntoIter = arrayvec::IntoIter<[Unit; 12]>;
    fn into_iter(self) -> Self::IntoIter {
        self.units.into_iter()
    }
}

impl Selection {
    /// Gets the selection of a player.
    /// (normal, as in when compared to replay selections or hotkey groups)
    pub fn normal(player: u8) -> Selection {
        unsafe {
            let mut units = ArrayVec::new();
            let selection = bw::selections().add(12 * player as usize);
            for i in 0..12 {
                let unit = *selection.add(i);
                if let Some(unit) = Unit::from_ptr(unit) {
                    units.push(unit);
                } else {
                    break;
                }
            }
            Selection {
                units,
            }
        }
    }
}
