#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::slice;
use std::ptr::null_mut;

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

whack_funcs!(init_funcs, 0x00400000,
    0x004D58B0 => set_image_draw_funcs_1161(@eax *mut Image, @ebx u32);
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

pub unsafe fn set_image_drawfuncs(image: *mut Image, drawfunc: u8) {
    if crate::is_scr() {
        (*image).drawfunc = drawfunc;
        if drawfunc == 0x11 {
            (*image).drawfunc_param = 0x230 as *mut c_void;
        }
    } else {
        set_image_draw_funcs_1161(image, drawfunc as u32);
    }
}

pub unsafe fn ai_update_attack_target(unit: *mut Unit, a1: bool, a2: bool, a3: bool) -> bool {
    samase::ai_update_attack_target(unit, a1 as u32, a2 as u32, a3 as u32) != 0
}

pub unsafe fn update_visibility_point(sprite: *mut LoneSprite) {
    samase::update_visibility_point(sprite);
}

pub fn create_lone_sprite(id: bw_dat::SpriteId, pos: &Point, player: u8) -> *mut LoneSprite {
    unsafe {
        samase::create_lone_sprite(id.0 as u32, pos.x as i32, pos.y as i32, player as u32)
    }
}

// BW algorithm
pub fn distance(a: Point, b: Point) -> u32 {
    let x = (a.x as i32).wrapping_sub(b.x as i32).abs() as u32;
    let y = (a.y as i32).wrapping_sub(b.y as i32).abs() as u32;
    let (greater, lesser) = (x.max(y), x.min(y));
    if greater / 4 > lesser {
        greater
    } else {
        greater * 59 / 64 + lesser * 99 / 256
    }
}

pub fn rect_distance(a: &Rect, b: &Rect) -> u32 {
    let horizontal_overlap = a.left < b.right && a.right > b.left;
    let vertical_overlap = a.top < b.bottom && a.bottom > b.top;
    let x_diff = match horizontal_overlap {
        true => 0,
        false => match a.left < b.left {
            true => b.left - a.right,
            false => a.left - b.right,
        },
    };
    let y_diff = match vertical_overlap {
        true => 0,
        false => match a.top < b.top {
            true => b.top - a.bottom,
            false => a.top - b.bottom,
        },
    };

    distance(
        Point {
            x: 0,
            y: 0,
        },
        Point {
            x: x_diff,
            y: y_diff,
        },
    )
}

pub unsafe fn set_iscript_animation(image: *mut Image, animation: u8) {
    let old_animation = (*image).iscript.animation;
    if (*image).flags & 0x10 == 0 && animation > 1 {
        // Use full iscript not set, only allow init/death anims
        return;
    }
    if old_animation == animation {
        if animation == 0x1 || animation == 0xb || animation == 0x13 {
            // Don't restart death/walking/isworking anims
            return;
        }
    }
    let animation = if animation == 0x5 && (old_animation != 0x5 && old_animation != 0x2) {
        // GndAttkInit instead of Rpt
        0x2
    } else if animation == 0x6 && (old_animation != 0x6 || old_animation != 0x3) {
        // AirAttkInit
        0x3
    } else {
        animation
    };

    let iscript_bin = samase::get_iscript_bin();
    let header = iscript_bin.add((*image).iscript.header as usize) as *const u16;
    (*image).iscript.pos = header.add(4 + animation as usize).read_unaligned();
    (*image).iscript.return_pos = 0;
    (*image).iscript.animation = animation;
    (*image).iscript.wait = 0;
    samase::step_iscript_frame(image, &mut (*image).iscript, 0, null_mut());
}

pub fn rng_seed() -> u32 {
    samase::rng_seed().unwrap_or_else(|| {
        // Oh well, rng.rs only uses this for the initial seed
        unsafe { (*game()).frame_count.wrapping_add(1234) }
    })
}

pub fn players() -> *mut Player {
    unsafe { samase::players() }
}

pub fn pathing() -> *mut Pathing {
    unsafe { samase::pathing() }
}
