use std::cell::RefCell;
use std::mem;
use std::ptr::null_mut;

use libc::c_void;

use bw_dat::{Game, Unit};

use crate::bw;
use crate::render_scr;
use crate::unit::{active_units, UnitExt};
use crate::upgrades;

ome2_thread_local! {
    // Cached, but needs to be reset on frame change
    SPRITE_TO_UNIT: RefCell<SpriteToUnit> = sprite_to_unit_obj(RefCell::new(SpriteToUnit {
        valid: false,
        first: null_mut(),
        map: Vec::with_capacity(1000),
    }));
}

struct SpriteToUnit {
    valid: bool,
    first: *mut bw::Sprite,
    map: Vec<*mut bw::Unit>,
}

unsafe impl Send for SpriteToUnit {}

pub fn reset_sprite_to_unit() {
    sprite_to_unit_obj().borrow_mut().valid = false;
}

// Just does active units currently, which is fine since they are only things rendered
fn sprite_to_unit(sprite: *mut bw::Sprite) -> Option<Unit> {
    let sprite_to_unit = sprite_to_unit_obj();
    let mut sprite_to_unit = sprite_to_unit.borrow_mut();
    if !sprite_to_unit.valid {
        sprite_to_unit.valid = true;
        sprite_to_unit.map.clear();
        let mut first = !0;
        let mut last = 0;
        for unit in active_units() {
            if let Some(sprite) = unit.sprite() {
                first = first.min(sprite as usize);
                last = last.max(sprite as usize);
            }
            if let Some(subunit) = unit.subunit_linked() {
                if let Some(sprite) = subunit.sprite() {
                    first = first.min(sprite as usize);
                    last = last.max(sprite as usize);
                }
            }
        }
        let len = if last < first {
            0
        } else {
            (last - first) / mem::size_of::<bw::Sprite>() + 1
        };
        sprite_to_unit.map.resize(len, null_mut());
        for unit in active_units() {
            if let Some(sprite) = unit.sprite() {
                let index = (sprite as usize - first) / mem::size_of::<bw::Sprite>();
                sprite_to_unit.map[index] = *unit;
            }
            if let Some(subunit) = unit.subunit_linked() {
                if let Some(sprite) = subunit.sprite() {
                    let index = (sprite as usize - first) / mem::size_of::<bw::Sprite>();
                    sprite_to_unit.map[index] = *unit;
                }
            }
        }
        sprite_to_unit.first = first as *mut bw::Sprite;
    }
    if sprite_to_unit.first.is_null() || sprite < sprite_to_unit.first {
        None
    } else {
        let index =
            (sprite as usize - sprite_to_unit.first as usize) / mem::size_of::<bw::Sprite>();
        sprite_to_unit.map.get(index).and_then(|&x| unsafe { Unit::from_ptr(x) })
    }
}

pub unsafe extern fn draw_image_hook(image: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    let image = image as *mut bw::Image;
    let orig: unsafe extern fn(*mut bw::Image) = mem::transmute(orig);

    let game = Game::from_ptr(bw::game());
    let config = crate::config::config();
    let unit = sprite_to_unit((*image).parent);
    let track;
    if crate::is_scr() {
        track = Some(render_scr::track_image_render());
    } else {
        track = None;
        if let Some(unit) = unit {
            if let Some(color) = upgrades::player_color_palette(&config, game, unit) {
                (&mut (*bw::default_grp_remap)[0x8..0x10]).copy_from_slice(&color);
            }
        }
    };
    let mut restore_units_dat_flags = None;
    let mut restore_units_dat_has_shields = None;
    // Hp bar
    if (*image).drawfunc == 0xb {
        if let Some(unit) = unit {
            let stats = upgrades::show_stats(&config, game, unit);
            if let Some(show) = stats.energy {
                let flags = (bw::units_dat()[0x16].data as *mut u32).offset(unit.id().0 as isize);
                let orig_flags = *flags;
                match show {
                    true => *flags |= 0x0020_0000,
                    false => *flags &= !0x0020_0000,
                }
                restore_units_dat_flags = Some((flags, orig_flags));
            }
            if let Some(show) = stats.shields {
                let has = (bw::units_dat()[0x6].data as *mut u8).offset(unit.id().0 as isize);
                let orig = *has;
                *has =  show as u8;
                restore_units_dat_has_shields = Some((has, orig));
            }
        }
    }
    orig(image);
    if let Some(track) = track {
        if track.changed() {
            if let Some(unit) = unit {
                if let Some(color) = upgrades::player_color(&config, game, unit) {
                    track.set_player_color(color);
                }
            }
        }
    }
    if let Some((ptr, val)) = restore_units_dat_flags {
        *ptr = val;
    }
    if let Some((ptr, val)) = restore_units_dat_has_shields {
        *ptr = val;
    }
}
