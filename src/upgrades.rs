use std::cell::{RefCell, RefMut};
use std::collections::BTreeMap;
use std::ptr::null_mut;

use smallvec::SmallVec;
use vec_map::VecMap;

use bw_dat::{order, UnitId, UpgradeId, OrderId};

use config::Config;
use game::Game;
use parse_expr::{BoolExpr, IntExpr};
use unit::{self, Unit};

ome2_thread_local! {
    STATE_CHANGES: RefCell<UpgradeStateChanges> =
        state_changes(RefCell::new(UpgradeStateChanges::new()));
}

#[derive(Serialize, Deserialize, Clone)]
pub struct UpgradeStateChanges {
    // player, from, to
    unit_id_changes: Vec<(u8, UnitId, UnitId)>,
}

impl UpgradeStateChanges {
    pub fn new() -> UpgradeStateChanges {
        UpgradeStateChanges {
            unit_id_changes: Vec::new(),
        }
    }

    pub fn update_build_queue(&self, unit: Unit) {
        for &(player, from, to) in &self.unit_id_changes {
            if unit.player() == player {
                for i in 0..5 {
                    unsafe {
                        if (*unit.0).build_queue[i] == from.0 {
                            (*unit.0).build_queue[i] = to.0;
                        }
                    }
                }
            }
        }
    }

    pub fn upgrade_gained(
        &mut self,
        config: &Config,
        game: Game,
        player: u8,
        upgrade: UpgradeId,
        level: u8,
    ) {
        let upgrades = config.upgrades.upgrades.iter().filter(|x| x.0 == upgrade.0 as usize);
        for (_id, upgrade) in upgrades {
            for (state_reqs, changes) in &upgrade.changes {
                for changes in changes.iter().filter(|x| x.level == level) {
                    if changes.changes.iter().any(|x| x.0.is_state_change()) {
                        for unit in unit::player_units(player) {
                            let cond_ok =
                                changes.condition.as_ref().map(|x| check_condition(x, unit, game))
                                .unwrap_or(true);
                            let ok = changes.units.iter().any(|&x| unit.matches_id(x)) &&
                                state_reqs.iter().all(|x| x.matches_unit(unit)) &&
                                cond_ok;
                            if ok {
                                for &(stat, ref value) in &changes.changes {
                                    let value = eval_int(value, unit, game);
                                    match stat {
                                        Stat::SetUnitId => unit.set_unit_id(UnitId(value as u16)),
                                        _ => (),
                                    }
                                }
                            }
                        }
                        // Add permament changes only if they don't use unit-specific data
                        if changes.condition.is_none() && state_reqs.is_empty() {
                            for &(stat, ref value) in &changes.changes {
                                if let Some(value) = eval_constant_int(value) {
                                    match stat {
                                        Stat::SetUnitId => {
                                            for &from in &changes.units {
                                                let to = UnitId(value as u16);
                                                self.unit_id_changes.push((player, from, to));
                                            }
                                        }
                                        _ => (),
                                    }
                                }
                            }
                        }
                        for unit in unit::player_units(player) {
                            self.update_build_queue(unit);
                        }
                    }
                }
            }
        }
    }
}

pub fn init_state_changes() {
    set_state_changes(UpgradeStateChanges::new());
}

pub fn set_state_changes(changes: UpgradeStateChanges) {
    *state_changes().borrow_mut() = changes;
}

pub fn global_state_changes() -> RefMut<'static, UpgradeStateChanges> {
    state_changes().borrow_mut()
}

pub struct Upgrades {
    pub upgrades: VecMap<Upgrade>,
    units_with_upgrades: Vec<bool>,
}

#[derive(Debug)]
pub struct Upgrade {
    pub all_matching_units: Vec<UnitId>,
    pub changes: BTreeMap<Vec<State>, Vec<UpgradeChanges>>,
}

impl Upgrades {
    pub fn new(upgrades: VecMap<Upgrade>) -> Upgrades {
        let largest_unit_id = upgrades.iter()
            .flat_map(|x| x.1.all_matching_units.iter())
            .map(|x| x.0)
            .max()
            .unwrap_or(0);
        let mut units_with_upgrades = vec![false; largest_unit_id as usize + 1];
        {
            let units = upgrades.iter()
                .flat_map(|x| x.1.all_matching_units.iter());
            for unit in units {
                units_with_upgrades[unit.0 as usize] = true;
            }
        }
        Upgrades {
            upgrades,
            units_with_upgrades,
        }
    }

    fn matches<F: FnMut(&Stat, &IntExpr)>(&self, game: Game, unit: Unit, mut fun: F) {
        if unit.player() >= 0xc {
            return;
        }
        let id = unit.id();
        if self.units_with_upgrades.get(id.0 as usize).cloned().unwrap_or(false) == false {
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

fn eval_constant_int(expr: &IntExpr) -> Option<i32> {
    use parse_expr::IntExpr::*;
    Some(match expr {
        Add(x) => eval_constant_int(&x.0)?.saturating_add(eval_constant_int(&x.1)?),
        Sub(x) => eval_constant_int(&x.0)?.saturating_sub(eval_constant_int(&x.1)?),
        Mul(x) => eval_constant_int(&x.0)?.saturating_mul(eval_constant_int(&x.1)?),
        Div(x) => eval_constant_int(&x.0)? / (eval_constant_int(&x.1)?),
        Modulo(x) => eval_constant_int(&x.0)? % (eval_constant_int(&x.1)?),
        Integer(i) => *i,
        Func(_) => return None,
    })
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
                    GroundCooldown => (*unit.0).ground_cooldown as i32,
                    AirCooldown => (*unit.0).air_cooldown as i32,
                    SpellCooldown => (*unit.0).spell_cooldown as i32,
                    Speed => (*unit.0).speed,
                    SigOrder => (*unit.0).order_signal as i32,
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
                    Burrowed => unit.is_burrowed(),
                    Disabled => unit.is_disabled(),
                    Completed => unit.is_completed(),
                    SelfCloaked => {
                        let cloak_order = unit.secondary_order() == order::CLOAK;
                        cloak_order && unit.is_invisible() && !unit.has_free_cloak()
                    }
                    ArbiterCloaked => unit.has_free_cloak() && !unit.is_burrowed(),
                    Cloaked => unit.is_invisible() && !unit.is_burrowed(),
                    UnderDweb => unit.is_under_dweb(),
                    Hallucination => unit.is_hallucination(),
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
    MineralHarvestReduce,
    GasHarvestReduce,
    MineralHarvestCarry,
    GasHarvestCarry,
    GasHarvestCarryDepleted,
    SetUnitId,
}

impl Stat {
    fn is_state_change(&self) -> bool {
        match self {
            Stat::SetUnitId => true,
            _ => false,
        }
    }
}

fn clamp_u8(val: i32) -> u8 {
    val.max(0).min(255) as u8
}

pub struct Regens {
    pub hp: Option<i32>,
    pub shield: Option<i32>,
    pub energy: Option<i32>,
}

pub fn regens(config: &Config, game: Game, unit: Unit) -> Regens {
    let mut hp = 0i32;
    let mut shield = 0i32;
    let mut energy = 0i32;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::HpRegen => {
            hp = hp.saturating_add(eval_int(val, unit, game));
        }
        Stat::ShieldRegen => {
            shield = shield.saturating_add(eval_int(val, unit, game));
        }
        Stat::EnergyRegen => {
            energy = energy.saturating_add(eval_int(val, unit, game));
        }
        _ => (),
    });
    Regens {
        hp: if hp != 0 { Some(hp) } else { None },
        shield: if shield != 0 { Some(shield) } else { None },
        energy: if energy != 0 { Some(energy) } else { None },
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

pub fn mineral_harvest_reduce(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::MineralHarvestReduce => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(0).max(val));
        }
        _ => (),
    });
    value
}

pub fn mineral_harvest_carry(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::MineralHarvestCarry => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(0).max(val));
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

pub fn gas_harvest_reduce(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::GasHarvestReduce => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(0).max(val));
        }
        _ => (),
    });
    value
}

pub fn gas_harvest_carry(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::GasHarvestCarry => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(0).max(val));
        }
        _ => (),
    });
    value
}

pub fn gas_harvest_carry_depleted(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, val| match *stat {
        Stat::GasHarvestCarryDepleted => {
            let val = clamp_u8(eval_int(val, unit, game));
            value = Some(value.unwrap_or(0).max(val));
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
