#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::slice;

use libc::c_void;

use crate::samase;

use bw_dat::DatTable;

pub use bw_dat::structs::*;

#[repr(C)]
pub struct CampaignMission {
    pub name_index: u16,
    pub campaign_mission: u16,
    pub cinematic: u16,
    pub race: u8,
    pub hidden: u8,
}

pub fn game() -> *mut Game {
    samase::game()
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

whack_vars!(init_vars, 0x00400000,
    0x0050CDC1 => default_grp_remap: [u8; 256];
);

pub mod storm {
    #[repr(C)]
    pub struct SCode {
        pub dc: [u8; 0x4c],
        pub code_offsets: [*mut u8; 0xa1],
    }

    whack_vars!(init_vars, 0x15000000,
        0x1505EC04 => surface_copy_code: *mut SCode;
    );
}

static mut UNITS_DAT: usize = !0;

pub unsafe fn init_game_start_vars() {
    UNITS_DAT = samase::units_dat() as usize;
}

pub fn print_text<M: AsRef<str>>(msg: M) {
    let mut buf: Vec<u8> = msg.as_ref().as_bytes().into();
    buf.push(0);
    samase::print_text(buf.as_ptr());
}
