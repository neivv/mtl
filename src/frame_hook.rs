use std::cell::RefCell;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, Ordering};

use libc::c_void;

use bw_dat::{self, Game, Unit, UnitId, order};

use crate::bw;
use crate::config::config;
use crate::render;
use crate::unit::{self, SerializableUnit};
use crate::upgrades;

/// Set to true at game init so that frame hook knows
/// to do init things that were too early to do at game init hook.
static FIRST_FRAME: AtomicBool = AtomicBool::new(false);

pub fn enable_first_frame_hook() {
    FIRST_FRAME.store(true, Ordering::Relaxed);
}

#[derive(Serialize, Deserialize, Clone)]
struct TrackedUnit {
    unit: SerializableUnit,
    end_frame: u32,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct TrackedSpells {
    lockdown: Vec<TrackedUnit>,
    maelstrom: Vec<TrackedUnit>,
    matrix: Vec<TrackedUnit>,
    stim: Vec<TrackedUnit>,
    ensnare: Vec<TrackedUnit>,
    irradiate: Vec<TrackedUnit>,
    stasis: Vec<TrackedUnit>,
    plague: Vec<TrackedUnit>,
    acid_spores: [Vec<TrackedUnit>; 9],
    hallucination_death: Vec<TrackedUnit>,
    unit_deaths: Vec<(UnitId, u32, Vec<TrackedUnit>)>,
}

impl TrackedSpells {
    fn new() -> TrackedSpells {
        let config = config();
        let unit_deaths = config.timers.unit_deaths.iter().map(|x| {
            (x.0, x.1, Vec::new())
        }).collect();
        TrackedSpells {
            lockdown: Vec::new(),
            maelstrom: Vec::new(),
            matrix: Vec::new(),
            stim: Vec::new(),
            ensnare: Vec::new(),
            irradiate: Vec::new(),
            stasis: Vec::new(),
            plague: Vec::new(),
            acid_spores: [
                Vec::new(), Vec::new(), Vec::new(),
                Vec::new(), Vec::new(), Vec::new(),
                Vec::new(), Vec::new(), Vec::new(),
            ],
            hallucination_death: Vec::new(),
            unit_deaths,
        }
    }

    fn remove_dead_units(&mut self) {
        use bw_dat::order::DIE;
        let TrackedSpells {
            ref mut lockdown,
            ref mut maelstrom,
            ref mut matrix,
            ref mut stim,
            ref mut ensnare,
            ref mut irradiate,
            ref mut stasis,
            ref mut plague,
            ref mut acid_spores,
            ref mut hallucination_death,
            ref mut unit_deaths,
        } = *self;
        lockdown.retain(|x| x.unit.order() != DIE);
        maelstrom.retain(|x| x.unit.order() != DIE);
        matrix.retain(|x| x.unit.order() != DIE);
        stim.retain(|x| x.unit.order() != DIE);
        ensnare.retain(|x| x.unit.order() != DIE);
        irradiate.retain(|x| x.unit.order() != DIE);
        stasis.retain(|x| x.unit.order() != DIE);
        plague.retain(|x| x.unit.order() != DIE);
        for a in acid_spores {
            a.retain(|x| x.unit.order() != DIE);
        }
        hallucination_death.retain(|x| x.unit.order() != DIE);
        for &mut (_, _, ref mut units) in unit_deaths {
            units.retain(|x| x.unit.order() != DIE);
        }
    }
}

ome2_thread_local! {
    TRACKED: RefCell<TrackedSpells> = tracked(RefCell::new(TrackedSpells::new()));
}

pub fn init_tracked_spells() {
    let mut tracked = tracked().borrow_mut();
    *tracked = TrackedSpells::new();
}

pub fn tracked_spells() -> TrackedSpells {
    (*tracked().borrow()).clone()
}

pub fn set_tracked_spells(spells: TrackedSpells) {
    *tracked().borrow_mut() = spells;
}

pub unsafe extern fn frame_hook() {
    let game = Game::from_ptr(bw::game());
    // Not doing frame_count == 0 as there are actually 2 frame 0s
    let first_frame = FIRST_FRAME.load(Ordering::Relaxed);
    if first_frame {
        FIRST_FRAME.store(false, Ordering::Relaxed);
        crate::samase::init_config(false, Some(game));
    }
    let config = config();
    {
        if let Some(ref light) = config.lighting {
            let step = match light.bound_death {
                Some((player, unit_id)) => (**game).deaths[unit_id.0 as usize][player as usize],
                None => 1,
            };
            if step != 0 {
                let mut lighting_state = render::lighting_state();
                lighting_state.frame = lighting_state.frame.wrapping_add(step);
            }
        }
    }
    render::reset_sprite_to_unit();
    let timers = &config.timers;
    let upgrades = upgrades::global_state_changes();
    let mut tracked = tracked().borrow_mut();
    if first_frame {
        // These can't be done at init_game
        crate::unit_pcolor_fix::game_start_hook();
        if let Some(max) = config.supplies.zerg_max {
            for x in &mut (**game).supplies[0].max {
                *x = max;
            }
        }
        if let Some(max) = config.supplies.terran_max {
            for x in &mut (**game).supplies[1].max {
                *x = max;
            }
        }
        if let Some(max) = config.supplies.protoss_max {
            for x in &mut (**game).supplies[2].max {
                *x = max;
            }
        }
    }
    tracked.remove_dead_units();
    crate::rally::rally_cursor_marker_frame_hook(&config, game);
    for unit in unit::alive_units() {
        if config.has_rally(unit.id()) {
            // Remove rallying to unit if the unit has disappeared from vision or started dying
            let rally_unit = {
                Unit::from_ptr(*((**unit).rally_pylon.as_ptr().add(4) as *mut *mut bw::Unit))
            };
            if let Some(target) = rally_unit {
                let player = unit.player();
                let clear = !target.is_visible_to(player) ||
                    target.is_invisible_hidden_to(player) ||
                    target.order() == order::DIE;
                if clear {
                    *((**unit).rally_pylon.as_ptr().add(4) as *mut *mut bw::Unit) = null_mut();
                    *((**unit).rally_pylon.as_ptr() as *mut bw::Point) = target.position();
                }
            }
        }

        let t = &mut *tracked;
        timer_override(&timers.lockdown, game, unit, &mut t.lockdown, &mut (**unit).lockdown_timer);
        timer_override(&timers.maelstrom, game, unit, &mut t.maelstrom, &mut (**unit).maelstrom_timer);
        timer_override(&timers.matrix, game, unit, &mut t.matrix, &mut (**unit).matrix_timer);
        timer_override(&timers.stim, game, unit, &mut t.stim, &mut (**unit).stim_timer);
        timer_override(&timers.ensnare, game, unit, &mut t.ensnare, &mut (**unit).ensnare_timer);
        timer_override(&timers.irradiate, game, unit, &mut t.irradiate, &mut (**unit).irradiate_timer);
        timer_override(&timers.stasis, game, unit, &mut t.stasis, &mut (**unit).stasis_timer);
        timer_override(&timers.plague, game, unit, &mut t.plague, &mut (**unit).plague_timer);
        for i in 0..9 {
            timer_override(
                &timers.acid_spores,
                game,
                unit,
                &mut t.acid_spores[i],
                &mut (**unit).acid_spore_timers[i],
            );
        }
        if unit.is_hallucination() {
            if let Some(timer) = timers.hallucination_death {
                death_timer(game, timer, unit, &mut t.hallucination_death);
            }
        } else {
            for &mut (id, timer, ref mut tracked) in &mut t.unit_deaths {
                if unit.matches_id(id) {
                    death_timer(game, timer, unit, tracked);
                }
            }
        }
        let regen = upgrades::regens(&config, game, unit);
        if let Some(hp_regen) = regen.hp {
            (**unit).hitpoints = (**unit).hitpoints.saturating_add(hp_regen);
            if (**unit).hitpoints <= 0 {
                // No code for killing units yet
                (**unit).hitpoints = 1;
            }
            let max_hp = unit.id().hitpoints();
            if (**unit).hitpoints > max_hp {
                (**unit).hitpoints = max_hp;
            }
        }
        if let Some(regen) = regen.shield {
            (**unit).shields = (**unit).shields.saturating_add(regen);
            if (**unit).shields < 0 {
                (**unit).shields = 0;
            }
            let max_shields = unit.id().shields();
            if (**unit).shields > max_shields {
                (**unit).shields = max_shields;
            }
        }
        if let Some(regen) = regen.energy {
            if regen > 0 {
                (**unit).energy = (**unit).energy.saturating_add(regen as u16);
            } else {
                let neg = regen.checked_abs().unwrap_or(0);
                (**unit).energy = (**unit).energy.saturating_sub(neg as u16);
            }
            // Not handling past-the-max, bw hopefully handles that.
        }
        if (**unit).build_queue[(**unit).current_build_slot as usize] != bw_dat::unit::NONE.0 {
            upgrades.update_build_queue(unit);
        }

        // Just unconditionally redraw anything which may have dynamic color changes
        if !crate::is_scr() && config.upgrades.may_have_color_upgrade(unit.id()) {
            let mut image = (*(**unit).sprite).first_image;
            while !image.is_null() {
                (*image).flags |= 0x1;
                image = (*image).next;
            }
        }
    }
}

unsafe fn death_timer(game: Game, time_frames: u32, unit: Unit, tracked: &mut Vec<TrackedUnit>) {
    if (**unit).death_timer != 1 {
        let unit_pos = match tracked.iter().position(|x| x.unit.0 == unit) {
            Some(x) => x,
            None => {
                tracked.push(TrackedUnit {
                    unit: SerializableUnit(unit),
                    end_frame: game.frame_count() + time_frames,
                });
                (**unit).death_timer = 0;
                tracked.len() - 1
            }
        };
        // + 1 since the timer has to end at 1
        let frames_remaining =
            tracked[unit_pos].end_frame.saturating_sub(game.frame_count()) + 1;
        if frames_remaining < 65536 {
            (**unit).death_timer = frames_remaining as u16;
        }
        if frames_remaining == 1 {
            tracked.remove(unit_pos);
        }
    }
}

unsafe fn timer_override(
    timer: &Option<u32>,
    game: Game,
    unit: Unit,
    tracked: &mut Vec<TrackedUnit>,
    bw_timer: *mut u8,
) {
    // If BW timer == 0, the timer is disabled,
    // if 1, about to expire
    // if 2 or 3, overridden and waiting for tracked end_frame
    // otherwise needs to be overridden.
    // The frame has to be reset to 3 as every 8 frames it rolls down by one to 2

    if let Some(time_frames) = *timer {
        match *bw_timer {
            0 | 1 => (),
            2 | 3 => {
                let unit_pos = match tracked.iter().position(|x| x.unit.0 == unit) {
                    Some(x) => x,
                    None => {
                        debug_fatal!(
                            "Unit {:p} had timer at 2, but it wasn't tracked???", *unit
                        );
                        return;
                    }
                };
                if tracked[unit_pos].end_frame <= game.frame_count() {
                    *bw_timer = 1;
                    tracked.remove(unit_pos);
                } else {
                    *bw_timer = 3;
                }
            }
            _ => {
                // Remove the old entry if it had this spell already
                tracked.retain(|x| x.unit.0 != unit);
                tracked.push(TrackedUnit {
                    unit: SerializableUnit(unit),
                    end_frame: game.frame_count() + time_frames,
                });
                *bw_timer = 3;
            }
        }
    }
}

pub unsafe fn check_fow_sprite_creation_desync(
    unit_id: u32,
    base: *mut c_void,
    orig: unsafe extern fn(u32, *mut c_void) -> *mut c_void,
) -> *mut c_void {
    let old_seed = crate::samase::rng_seed();
    let result = orig(unit_id, base);
    if old_seed != crate::samase::rng_seed() {
        bw::print_text(
            format!("BUG: Fow sprite iscript of unit 0x{:x} has touched RNG", unit_id & 0xffff)
        );
    }
    result
}
