#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use samase;

pub mod structs;

pub use self::structs::*;

use unit::UnitId;

pub fn game() -> *mut Game {
    samase::game()
}

pub fn frame_count() -> u32 {
    unsafe { (*game()).frame_count }
}

lazy_static! {
    static ref SAMASE_UNITS_DAT: usize =
        samase::read_file("arr\\units.dat").unwrap().as_ptr() as usize;
}

pub fn collision_rect(unit: UnitId) -> Rect {
    unsafe {
        assert!(unit.0 < 0xe4);
        let dat = *SAMASE_UNITS_DAT as *const u8;
        *(dat.offset(0x3124 + unit.0 as isize * 8) as *const Rect)
    }
}
