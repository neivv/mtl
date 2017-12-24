use std::cell::RefCell;

use bw;
use config::config;
use unit::{self, Unit, UnitId};
use order;

struct TrackedUnit {
    unit: Unit,
    end_frame: u32,
}

struct TrackedSpells {
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
        lockdown.retain(|x| x.unit.order() != order::id::DIE);
        maelstrom.retain(|x| x.unit.order() != order::id::DIE);
        matrix.retain(|x| x.unit.order() != order::id::DIE);
        stim.retain(|x| x.unit.order() != order::id::DIE);
        ensnare.retain(|x| x.unit.order() != order::id::DIE);
        irradiate.retain(|x| x.unit.order() != order::id::DIE);
        stasis.retain(|x| x.unit.order() != order::id::DIE);
        plague.retain(|x| x.unit.order() != order::id::DIE);
        for a in acid_spores {
            a.retain(|x| x.unit.order() != order::id::DIE);
        }
        hallucination_death.retain(|x| x.unit.order() != order::id::DIE);
        for &mut (_, _, ref mut units) in unit_deaths {
            units.retain(|x| x.unit.order() != order::id::DIE);
        }
    }
}

ome2_thread_local! {
    TRACKED: RefCell<TrackedSpells> = tracked(RefCell::new(TrackedSpells::new()));
}

pub unsafe extern fn frame_hook() {
    let config = config();
    let timers = &config.timers;
    let mut tracked = tracked().borrow_mut();
    tracked.remove_dead_units();
    for unit in unit::alive_units() {
        let t = &mut *tracked;
        timer_override(&timers.lockdown, unit, &mut t.lockdown, &mut (*unit.0).lockdown_timer);
        timer_override(&timers.maelstrom, unit, &mut t.maelstrom, &mut (*unit.0).maelstrom_timer);
        timer_override(&timers.matrix, unit, &mut t.matrix, &mut (*unit.0).matrix_timer);
        timer_override(&timers.stim, unit, &mut t.stim, &mut (*unit.0).stim_timer);
        timer_override(&timers.ensnare, unit, &mut t.ensnare, &mut (*unit.0).ensnare_timer);
        timer_override(&timers.irradiate, unit, &mut t.irradiate, &mut (*unit.0).irradiate_timer);
        timer_override(&timers.stasis, unit, &mut t.stasis, &mut (*unit.0).stasis_timer);
        timer_override(&timers.plague, unit, &mut t.plague, &mut (*unit.0).plague_timer);
        for i in 0..9 {
            timer_override(
                &timers.acid_spores,
                unit,
                &mut t.acid_spores[i],
                &mut (*unit.0).acid_spore_timers[i],
            );
        }
        if unit.is_hallucination() {
            if let Some(timer) = timers.hallucination_death {
                death_timer(timer, unit, &mut t.hallucination_death);
            }
        } else {
            for &mut (id, timer, ref mut tracked) in &mut t.unit_deaths {
                if unit.matches_id(id) {
                    death_timer(timer, unit, tracked);
                }
            }
        }
    }
}

unsafe fn death_timer(time_frames: u32, unit: Unit, tracked: &mut Vec<TrackedUnit>) {
    if (*unit.0).death_timer != 1 {
        let unit_pos = match tracked.iter().position(|x| x.unit.0 == unit.0) {
            Some(x) => x,
            None => {
                tracked.push(TrackedUnit {
                    unit,
                    end_frame: bw::frame_count() + time_frames,
                });
                (*unit.0).death_timer = 0;
                tracked.len() - 1
            }
        };
        // + 1 since the timer has to end at 1
        let frames_remaining =
            tracked[unit_pos].end_frame.saturating_sub(bw::frame_count()) + 1;
        if frames_remaining < 65536 {
            (*unit.0).death_timer = frames_remaining as u16;
        }
        if frames_remaining == 1 {
            tracked.remove(unit_pos);
        }
    }
}

unsafe fn timer_override(
    timer: &Option<u32>,
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
                let unit_pos = match tracked.iter().position(|x| x.unit.0 == unit.0) {
                    Some(x) => x,
                    None => {
                        debug_fatal!(
                            "Unit {:p} had timer at 2, but it wasn't tracked???", unit.0
                        );
                        return;
                    }
                };
                if tracked[unit_pos].end_frame <= bw::frame_count() {
                    *bw_timer = 1;
                    tracked.remove(unit_pos);
                } else {
                    *bw_timer = 3;
                }
            }
            _ => {
                // Remove the old entry if it had this spell already
                tracked.retain(|x| x.unit.0 != unit.0);
                tracked.push(TrackedUnit {
                    unit,
                    end_frame: bw::frame_count() + time_frames,
                });
                *bw_timer = 3;
            }
        }
    }
}
