use std::collections::BTreeMap;
use std::ptr::null_mut;

use smallvec::SmallVec;
use vec_map::VecMap;

use bw_dat::{order, UnitId, OrderId};

use config::Config;
use game::Game;
use unit::Unit;

pub struct Upgrades {
    pub upgrades: VecMap<Upgrade>,
}

#[derive(Debug)]
pub struct Upgrade {
    pub all_matching_units: Vec<UnitId>,
    pub changes: BTreeMap<Vec<State>, Vec<UpgradeChanges>>,
}

impl Upgrades {
    fn matches<F: FnMut(&Stat)>(&self, game: Game, unit: Unit, mut fun: F) {
        if unit.player() >= 0xc {
            return;
        }
        for (id, upgrade) in self.upgrades.iter() {
            if !upgrade.all_matching_units.iter().any(|&x| unit.matches_id(x)) {
                continue;
            }
            let player_level = game.upgrade_level(unit.player(), id as u8);
            for (state_reqs, changes) in upgrade.changes.iter() {
                if !state_reqs.iter().all(|x| x.matches_unit(unit)) {
                    continue;
                }
                let had_incomplete = state_reqs.iter().any(|x| *x == State::Incomplete);
                if !had_incomplete {
                    if !unit.is_completed() {
                        continue;
                    }
                }
                let mut matched_level = None;
                for changes in changes.iter().rev().skip_while(|x| x.level > player_level) {
                    if let Some(matched) = matched_level {
                        if changes.level < matched {
                            break;
                        }
                    }
                    if changes.units.iter().any(|&x| unit.matches_id(x)) {
                        matched_level = Some(changes.level);
                        for stat in &changes.changes {
                            fun(stat);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct UpgradeChanges {
    pub units: Vec<UnitId>,
    pub level: u8,
    pub changes: Vec<Stat>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum State {
    SelfCloaked,
    ArbiterCloaked,
    Burrowed,
    // Note: Completed is implied requirement if this isn't specified
    Incomplete,
    Disabled,
    Damaged,
    Order(SmallVec<[OrderId; 12]>),
    IscriptAnim(SmallVec<[u8; 12]>),
}

impl State {
    fn matches_unit(&self, unit: Unit) -> bool {
        use self::State::*;
        match self {
            SelfCloaked => {
                let cloak_order = unit.secondary_order() == order::CLOAK;
                cloak_order && unit.is_invisible() && !unit.has_free_cloak()
            }
            ArbiterCloaked => unit.has_free_cloak() && !unit.is_burrowed(),
            Burrowed => unit.is_burrowed(),
            Incomplete => !unit.is_completed(),
            Disabled => unit.is_disabled(),
            Damaged => unsafe {
                let id = unit.id();
                (*unit.0).hitpoints != id.hitpoints() || {
                    if id.has_shields() {
                        (*unit.0).shields != id.shields()
                    } else {
                        false
                    }
                }
            },
            Order(o) => o.iter().any(|&x| unit.order() == x),
            IscriptAnim(a) => unsafe {
                if (*unit.0).sprite == null_mut() {
                    return false;
                }
                let sprite = (*unit.0).sprite;
                if (*sprite).main_image == null_mut() {
                    return false;
                }
                let image = (*sprite).main_image;
                a.iter().any(|&x| (*image).iscript.animation == x)
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Stat {
    HpRegen(i32),
    ShieldRegen(i32),
    EnergyRegen(i32),
    Cooldown(u8),
    LarvaTimer(u8),
    MineralHarvestTime(u8),
    GasHarvestTime(u8),
    UnloadCooldown(u8),
    CreepSpreadTimer(u8),
}

pub fn hp_regen(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut sum = 0i32;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::HpRegen(i) => {
            sum = sum.saturating_add(i);
        }
        _ => (),
    });
    if sum != 0 {
        Some(sum)
    } else {
        None
    }
}

pub fn shield_regen(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut sum = 0i32;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::ShieldRegen(i) => {
            sum = sum.saturating_add(i);
        }
        _ => (),
    });
    if sum != 0 {
        Some(sum)
    } else {
        None
    }
}

pub fn energy_regen(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut sum = 0i32;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::EnergyRegen(i) => {
            sum = sum.saturating_add(i);
        }
        _ => (),
    });
    if sum != 0 {
        Some(sum)
    } else {
        None
    }
}

pub fn cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::Cooldown(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}

pub fn mineral_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::MineralHarvestTime(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}

pub fn gas_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::GasHarvestTime(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}

pub fn creep_spread_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::CreepSpreadTimer(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}

pub fn larva_spawn_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::LarvaTimer(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}

pub fn unload_cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = !0;
    config.upgrades.matches(game, unit, |stat| match *stat {
        Stat::UnloadCooldown(i) => {
            if i < !0 {
                value = value.min(i);
            }
        }
        _ => (),
    });
    match value != !0 {
        true => Some(value),
        false => None,
    }
}
