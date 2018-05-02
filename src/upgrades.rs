use std::collections::BTreeMap;
use std::ptr::null_mut;

use smallvec::SmallVec;
use vec_map::VecMap;

use bw_dat::{order, UnitId, UpgradeId, OrderId};

use config::Config;
use game::Game;
use parse_expr::{BoolExpr, IntExpr};
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
    fn matches<F: FnMut(&Stat, &IntExpr)>(&self, game: Game, unit: Unit, mut fun: F) {
        if unit.player() >= 0xc {
            return;
        }
        for (id, upgrade) in self.upgrades.iter() {
            if !upgrade.all_matching_units.iter().any(|&x| unit.matches_id(x)) {
                continue;
            }
            let player_level = game.upgrade_level(unit.player(), UpgradeId(id as u16));
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
                        let cond_ok =
                            changes.condition.as_ref().map(|x| check_condition(x, unit, game))
                            .unwrap_or(true);
                        if cond_ok {
                            for &(ref stat, ref val) in &changes.changes {
                                fun(stat, val);
                            }
                        }
                    }
                }
            }
        }
    }
}

fn eval_int(expr: &IntExpr, unit: Unit, game: Game) -> i32 {
    use parse_expr::IntExpr::*;
    use parse_expr::IntFunc::*;
    match expr {
        Add(x) => eval_int(&x.0, unit, game).saturating_add(eval_int(&x.1, unit, game)),
        Sub(x) => eval_int(&x.0, unit, game).saturating_sub(eval_int(&x.1, unit, game)),
        Mul(x) => eval_int(&x.0, unit, game).saturating_mul(eval_int(&x.1, unit, game)),
        Div(x) => eval_int(&x.0, unit, game) / (eval_int(&x.1, unit, game)),
        Modulo(x) => eval_int(&x.0, unit, game) % (eval_int(&x.1, unit, game)),
        Integer(i) => *i,
        Func(x) => {
            unsafe {
                match x {
                    StimTimer => (*unit.0).stim_timer as i32,
                    EnsnareTimer => (*unit.0).ensnare_timer as i32,
                    MaelstromTimer => (*unit.0).maelstrom_timer as i32,
                    DeathTimer => (*unit.0).death_timer as i32,
                    LockdownTimer => (*unit.0).lockdown_timer as i32,
                    StasisTimer => (*unit.0).stasis_timer as i32,
                    IrradiateTimer => (*unit.0).irradiate_timer as i32,
                    MatrixTimer => (*unit.0).matrix_timer as i32,
                    MatrixHitpoints => (*unit.0).defensive_matrix_dmg as i32,
                    AcidSporeCount => (*unit.0).acid_spore_count as i32,
                    Fighters => unit.fighter_amount() as i32,
                    Mines => unit.mine_amount(game) as i32,
                    Hitpoints => unit.hitpoints(),
                    HitpointsPercent => unit.hitpoints() * 100 / unit.id().hitpoints(),
                    Shields => unit.shields(),
                    ShieldsPercent => unit.shields() * 100 / unit.id().shields(),
                    Energy => unit.energy() as i32,
                    Kills => (*unit.0).kills as i32,
                    FrameCount => game.frame_count() as i32,
                    Tileset => (*game.0).tileset as i32,
                    Minerals => (*game.0).minerals[unit.player() as usize] as i32,
                    Gas => (*game.0).gas[unit.player() as usize] as i32,
                    CarriedResourceAmount => {
                        if unit.id().is_worker() {
                            (*unit.0).unit_specific[0xf] as i32
                        } else {
                            0
                        }
                    }
                }
            }
        }
    }
}

fn check_condition(cond: &BoolExpr, unit: Unit, game: Game) -> bool {
    use parse_expr::BoolExpr::*;
    use parse_expr::BoolFunc::*;
    match cond {
        And(x) => check_condition(&x.0, unit, game) && check_condition(&x.1, unit, game),
        Or(x) => check_condition(&x.0, unit, game) || check_condition(&x.1, unit, game),
        LessThan(x) => eval_int(&x.0, unit, game) < eval_int(&x.1, unit, game),
        LessOrEqual(x) => eval_int(&x.0, unit, game) <= eval_int(&x.1, unit, game),
        GreaterThan(x) => eval_int(&x.0, unit, game) > eval_int(&x.1, unit, game),
        GreaterOrEqual(x) => eval_int(&x.0, unit, game) >= eval_int(&x.1, unit, game),
        EqualInt(x) => eval_int(&x.0, unit, game) == eval_int(&x.1, unit, game),
        EqualBool(x) => check_condition(&x.0, unit, game) == check_condition(&x.1, unit, game),
        Not(x) => !check_condition(&x, unit, game),
        Func(x) => {
            unsafe {
                match x {
                    True => true,
                    False => true,
                    Parasited => (*unit.0).parasited_by_players != 0,
                    Blind => (*unit.0).is_blind != 0,
                    UnderStorm => (*unit.0).is_under_storm != 0,
                    LiftedOff => (*unit.0).flags & 0x2 == 0,
                    BuildingUnit => (*unit.0).currently_building != null_mut(),
                    InTransport => {
                        (*unit.0).flags & 0x20 == 0 && (*unit.0).flags & 0x40 != 0
                    }
                    InBunker => {
                        (*unit.0).flags & 0x20 != 0 && (*unit.0).flags & 0x40 != 0
                    }
                    CarryingPowerup => unit.powerup().is_some(),
                    CarryingMinerals => (*unit.0).carried_powerup_flags & 0x2 != 0,
                    CarryingGas => (*unit.0).carried_powerup_flags & 0x1 != 0,
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct UpgradeChanges {
    pub units: Vec<UnitId>,
    pub level: u8,
    pub changes: Vec<(Stat, IntExpr)>,
    pub condition: Option<BoolExpr>,
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
    HpRegen,
    ShieldRegen,
    EnergyRegen,
    Cooldown,
    LarvaTimer,
    MineralHarvestTime,
    GasHarvestTime,
    UnloadCooldown,
    CreepSpreadTimer,
}

fn clamp_u8(val: i32) -> u8 {
    val.max(0).min(255) as u8
}

pub fn hp_regen(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut sum = 0i32;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::HpRegen => {
            sum = sum.saturating_add(eval_int(val, unit, game));
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
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::ShieldRegen => {
            sum = sum.saturating_add(eval_int(val, unit, game));
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
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::EnergyRegen => {
            sum = sum.saturating_add(eval_int(val, unit, game));
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
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::Cooldown => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}

pub fn mineral_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::MineralHarvestTime => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}

pub fn gas_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::GasHarvestTime => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}

pub fn creep_spread_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::CreepSpreadTimer => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}

pub fn larva_spawn_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::LarvaTimer => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}

pub fn unload_cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::UnloadCooldown => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
        _ => (),
    });
    value
}
