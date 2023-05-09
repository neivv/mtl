use std::cell::RefCell;
use std::mem;
use std::ptr::null_mut;
use std::sync::{Mutex, MutexGuard};

use libc::c_void;

use bw_dat::{Game, Unit};

use crate::bw;
use crate::config;
use crate::render_scr;
use crate::unit::{active_units};
use crate::upgrades;

ome2_thread_local! {
    // Cached, but needs to be reset on frame change
    SPRITE_TO_UNIT: RefCell<SpriteToUnit> = sprite_to_unit_obj(RefCell::new(SpriteToUnit {
        valid: false,
        first: null_mut(),
        map: Vec::with_capacity(1000),
    }));
}

static LIGHTING_STATE: Mutex<LightingState> = Mutex::new(LightingState::new());

struct SpriteToUnit {
    valid: bool,
    first: *mut bw::Sprite,
    map: Vec<*mut bw::Unit>,
}

#[derive(Serialize, Deserialize)]
#[derive(Clone)]
pub struct LightingState {
    // Increases with game frames if lighting is enabled
    frame: u32,
    // 0 = None, 1-based value at which to stop
    dest_frame: u32,
    cycle: u32,
    start: (f32, f32, f32),
    end: (f32, f32, f32),
}

impl LightingState {
    pub const fn new() -> LightingState {
        LightingState {
            frame: 0,
            dest_frame: 0,
            cycle: 0,
            start: (0.0, 0.0, 0.0),
            end: (0.0, 0.0, 0.0),
        }
    }

    pub fn load_config(&mut self, config: &config::Lighting) {
        self.start = config.start;
        self.end = config.end;
        self.cycle = config.cycle;
    }

    pub fn step(&mut self, step: u32) {
        if step == 0 {
            return;
        }
        let old_frame = self.frame;
        self.frame = self.frame.wrapping_add(step);
        if self.dest_frame != 0 {
            if self.frame >= self.dest_frame && old_frame < self.dest_frame {
                self.frame = self.dest_frame - 1;
                return;
            }
        }
        if self.cycle != 0 {
            self.frame = self.frame % self.cycle;
            if self.dest_frame != 0 {
                if self.frame >= self.dest_frame {
                    self.frame = self.dest_frame - 1;
                    return;
                }
            }
        }
    }

    pub fn get_frame(&mut self) -> u32 {
        self.frame
    }

    pub fn set_frame(&mut self, value: u32) {
        self.frame = value;
    }

    pub fn set_cycle(&mut self, value: u32) {
        self.cycle = value;
    }

    pub fn set_dest_frame(&mut self, value: u32) {
        self.dest_frame = value.wrapping_add(1);
    }

    pub fn clear_dest_frame(&mut self) {
        self.dest_frame = 0;
    }

    pub fn set_colors(&mut self, start: (f32, f32, f32), end: (f32, f32, f32)) {
        self.start = start;
        self.end = end;
    }

    fn cycle_pos(&self) -> f32 {
        if self.cycle == 0 {
            return 0.0;
        }
        (self.frame % self.cycle) as f32 / (self.cycle as f32)
    }

    pub fn global_light(&self) -> (f32, f32, f32) {
        let low = self.start;
        let high = self.end;
        if self.cycle == 0 {
            return high;
        }
        let cycle = self.cycle_pos() * 3.14 * 2.0;
        let pos = (cycle.cos() + 1.0) / 2.0;
        (
            low.0 + (high.0 - low.0) * pos,
            low.1 + (high.1 - low.1) * pos,
            low.2 + (high.2 - low.2) * pos,
        )
    }
}

pub fn lighting_state() -> MutexGuard<'static, LightingState> {
    LIGHTING_STATE.lock().unwrap()
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
                first = first.min(*sprite as usize);
                last = last.max(*sprite as usize);
            }
            if let Some(subunit) = unit.subunit_linked() {
                if let Some(sprite) = subunit.sprite() {
                    first = first.min(*sprite as usize);
                    last = last.max(*sprite as usize);
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
                let index = (*sprite as usize - first) / mem::size_of::<bw::Sprite>();
                sprite_to_unit.map[index] = *unit;
            }
            if let Some(subunit) = unit.subunit_linked() {
                if let Some(sprite) = subunit.sprite() {
                    let index = (*sprite as usize - first) / mem::size_of::<bw::Sprite>();
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
        #[cfg(target_pointer_width = "32")]
        {
            if let Some(unit) = unit {
                if let Some(color) = upgrades::player_color_palette(&config, game, unit) {
                    (&mut (*bw::default_grp_remap)[0x8..0x10]).copy_from_slice(&color);
                    let trans50 = *bw::trans50;
                    for (i, &color) in color.iter().enumerate() {
                        let output = trans50.add(0x100 * (0x8 + i));
                        let input = trans50.add(0x100 * color as usize);
                        std::ptr::copy(input, output, 0x100);
                    }
                }
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
            if config.lighting.is_some() {
                // This is no-op if RTL is enabled. RTL's lighting change
                // is done by changing deferred_blit shader's uniforms.
                if (*image).drawfunc == 0xb {
                    // Hp bar
                    track.mark_hp_bar();
                } else if (*image).drawfunc == 0x9 {
                    // Don't set lighting for 1161 remapped images
                    // as they generally are explosions that are more of their
                    // own light source.
                } else {
                    let lighting_state = lighting_state();
                    track.set_multiply(lighting_state.global_light());
                }
            }
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
