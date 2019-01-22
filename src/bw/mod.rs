#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::slice;

use libc::c_void;

use samase;

pub mod structs;

use bw_dat::DatTable;

pub use self::structs::*;

pub fn game() -> *mut Game {
    samase::game()
}

pub fn frame_count() -> u32 {
    unsafe { (*game()).frame_count }
}

pub fn units_dat() -> &'static [DatTable] {
    unsafe {
        assert_ne!(UNITS_DAT, !0);
        let dat = UNITS_DAT as *mut DatTable;
        slice::from_raw_parts_mut(dat, 0x35)
    }
}

whack_hooks!(stdcall, 0x00400000,
    0x00488410 => create_fow_sprite(u32, *mut c_void) -> *mut c_void;
);

static mut UNITS_DAT: usize = !0;

pub unsafe fn init_game_start_vars() {
    UNITS_DAT = samase::units_dat() as usize;
}

pub fn print_text<M: AsRef<str>>(msg: M) {
    let mut buf: Vec<u8> = msg.as_ref().as_bytes().into();
    buf.push(0);
    samase::print_text(buf.as_ptr());
}
