use std::cell::{RefCell, RefMut};
use std::collections::BTreeMap;

use smallvec::SmallVec;
use vec_map::VecMap;

use bw_dat::{order, UnitId, UpgradeId, OrderId};
use bw_dat::expr::{BoolExpr, IntExpr, IntExprTree};
use bw_dat::{Game, Unit};

use crate::config::Config;
use crate::unit::{self, UnitExt};

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
                        if (**unit).build_queue[i] == from.0 {
                            (**unit).build_queue[i] = to.0;
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
                            let cond_ok = changes.condition.as_ref()
                                .map(|x| x.eval_with_unit(unit, game))
                                .unwrap_or(true);
                            let ok = changes.units.iter().any(|&x| unit.matches_id(x)) &&
                                state_reqs.iter().all(|x| x.matches_unit(unit)) &&
                                cond_ok;
                            if ok {
                                for &(stat, ref values) in changes.changes.iter() {
                                    let eval = |i| {
                                        let expr: &IntExpr = &values[i];
                                        expr.eval_with_unit(unit, game)
                                    };
                                    match stat {
                                        Stat::SetUnitId => {
                                            unit.set_unit_id(UnitId(eval(0) as u16));
                                        }
                                        _ => (),
                                    }
                                }
                            }
                        }
                        // Add permament changes only if they don't use unit-specific data
                        if changes.condition.is_none() && state_reqs.is_empty() {
                            for &(stat, ref values) in &changes.changes {
                                let eval = |i| {
                                    let expr: &IntExpr = &values[i];
                                    eval_constant_int(expr.inner())
                                };
                                match stat {
                                    Stat::SetUnitId => {
                                        if let Some(value) = eval(0) {
                                            for &from in &changes.units {
                                                let to = UnitId(value as u16);
                                                self.unit_id_changes.push((player, from, to));
                                            }
                                        }
                                    }
                                    _ => (),
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
    units_with_color_upgrades: Vec<bool>,
}

#[derive(Debug)]
pub struct Upgrade {
    pub all_matching_units: Vec<UnitId>,
    pub changes: BTreeMap<Vec<State>, Vec<UpgradeChanges>>,
}

impl Upgrades {
    pub fn new() -> Upgrades {
        Upgrades {
            upgrades: VecMap::new(),
            units_with_upgrades: Vec::new(),
            units_with_color_upgrades: Vec::new(),
        }
    }

    pub fn update(&mut self, upgrades: VecMap<Upgrade>) {
        let largest_unit_id = upgrades.iter()
            .flat_map(|x| x.1.all_matching_units.iter())
            .map(|x| x.0)
            .max()
            .unwrap_or(0)
            .max(self.units_with_upgrades.len() as u16);
        if largest_unit_id != bw_dat::unit::ANY_UNIT.0 {
            if self.units_with_upgrades.len() < largest_unit_id as usize + 1 {
                self.units_with_upgrades.resize(largest_unit_id as usize + 1, false);
            }
            let units = upgrades.iter()
                .flat_map(|x| x.1.all_matching_units.iter());
            for unit in units {
                self.units_with_upgrades[unit.0 as usize] = true;
            }
        } else {
            self.units_with_upgrades = vec![true; bw_dat::unit::NONE.0 as usize];
        };
        if self.units_with_color_upgrades.len() < largest_unit_id as usize + 1 {
            self.units_with_color_upgrades.resize(largest_unit_id as usize + 1, false);
        }
        {
            let changes = upgrades.iter()
                .flat_map(|x| x.1.changes.values())
                .flat_map(|x| x.iter());
            for change in changes {
                fn is_color_stat(stat: Stat) -> bool {
                    match stat {
                        Stat::PlayerColor | Stat::PlayerColorPalette => true,
                        _ => false,
                    }
                }
                if change.changes.iter().any(|x| is_color_stat(x.0)) {
                    for unit in &change.units {
                        self.units_with_color_upgrades[unit.0 as usize] = true;
                    }
                }
            }
        }
        self.upgrades.extend(upgrades);
    }

    fn matches<F: FnMut(&Stat, &[IntExpr])>(&self, game: Game, unit: Unit, mut fun: F) {
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
                        let cond_ok = changes.condition.as_ref()
                            .map(|x| x.eval_with_unit(unit, game))
                            .unwrap_or(true);
                        if cond_ok {
                            for &(ref stat, ref vals) in &changes.changes {
                                fun(stat, &vals);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn may_have_color_upgrade(&self, unit: UnitId) -> bool {
        self.units_with_color_upgrades.get(unit.0 as usize).cloned().unwrap_or(false)
    }
}

fn eval_constant_int(expr: &IntExprTree<bw_dat::expr::NoCustom>) -> Option<i32> {
    use bw_dat::expr::IntExprTree::*;
    Some(match expr {
        Add(x) => eval_constant_int(&x.0)?.saturating_add(eval_constant_int(&x.1)?),
        Sub(x) => eval_constant_int(&x.0)?.saturating_sub(eval_constant_int(&x.1)?),
        Mul(x) => eval_constant_int(&x.0)?.saturating_mul(eval_constant_int(&x.1)?),
        Div(x) => eval_constant_int(&x.0)? / (eval_constant_int(&x.1)?),
        Modulo(x) => eval_constant_int(&x.0)? % (eval_constant_int(&x.1)?),
        Integer(i) => *i,
        Func(_) | Custom(_) => return None,
    })
}

#[derive(Debug)]
pub struct UpgradeChanges {
    pub units: Vec<UnitId>,
    pub level: u8,
    pub changes: Vec<(Stat, SmallVec<[IntExpr; 1]>)>,
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
            Damaged => {
                let id = unit.id();
                unit.hitpoints() != id.hitpoints() || unit.shields() != id.shields()
            }
            Order(o) => o.iter().any(|&x| unit.order() == x),
            IscriptAnim(a) => unsafe {
                unit.sprite()
                    .and_then(|s| s.main_image())
                    .map(|image| {
                        a.iter().any(|&x| (**image).iscript.animation == x)
                    })
                    .unwrap_or(false)
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Stat {
    HpRegen,
    ShieldRegen,
    EnergyRegen,
    Cooldown,
    AirCooldown,
    GroundCooldown,
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
    PlayerColor,
    PlayerColorPalette,
    ShowEnergy,
    ShowShields,
}

impl Stat {
    fn is_state_change(&self) -> bool {
        match self {
            Stat::SetUnitId => true,
            _ => false,
        }
    }

    pub fn value_count(&self) -> u8 {
        match self {
            Stat::PlayerColor => 3,
            Stat::PlayerColorPalette => 8,
            _ => 1,
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
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::HpRegen => {
            hp = hp.saturating_add(vals[0].eval_with_unit(unit, game));
        }
        Stat::ShieldRegen => {
            shield = shield.saturating_add(vals[0].eval_with_unit(unit, game));
        }
        Stat::EnergyRegen => {
            energy = energy.saturating_add(vals[0].eval_with_unit(unit, game));
        }
        _ => (),
    });
    Regens {
        hp: if hp != 0 { Some(hp) } else { None },
        shield: if shield != 0 { Some(shield) } else { None },
        energy: if energy != 0 { Some(energy) } else { None },
    }
}

fn upgrade_min_u8(config: &Config, game: Game, unit: Unit, match_stat: Stat) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, vals| {
        if *stat == match_stat {
            let val = clamp_u8(vals[0].eval_with_unit(unit, game));
            value = Some(value.unwrap_or(!0).min(val));
        }
    });
    value
}

fn upgrade_max_u8(config: &Config, game: Game, unit: Unit, match_stat: Stat) -> Option<u8> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, vals| {
        if *stat == match_stat {
            let val = clamp_u8(vals[0].eval_with_unit(unit, game));
            value = Some(value.unwrap_or(0).max(val));
        }
    });
    value
}

pub fn cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::Cooldown)
}

pub fn ground_cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::GroundCooldown)
}

pub fn air_cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::AirCooldown)
}

pub fn mineral_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::MineralHarvestTime)
}

pub fn mineral_harvest_reduce(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_max_u8(config, game, unit, Stat::MineralHarvestReduce)
}

pub fn mineral_harvest_carry(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_max_u8(config, game, unit, Stat::MineralHarvestCarry)
}

pub fn gas_harvest_time(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::GasHarvestTime)
}

pub fn gas_harvest_reduce(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_max_u8(config, game, unit, Stat::GasHarvestReduce)
}

pub fn gas_harvest_carry(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_max_u8(config, game, unit, Stat::GasHarvestCarry)
}

pub fn gas_harvest_carry_depleted(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_max_u8(config, game, unit, Stat::GasHarvestCarryDepleted)
}

pub fn creep_spread_time(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::CreepSpreadTimer => {
            let val = vals[0].eval_with_unit(unit, game).max(-1).min(255);
            value = Some(value.unwrap_or(i32::max_value()).min(val));
        }
        _ => (),
    });
    value
}

pub fn larva_spawn_time(config: &Config, game: Game, unit: Unit) -> Option<i32> {
    let mut value = None;
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::LarvaTimer => {
            let val = vals[0].eval_with_unit(unit, game).max(-1).min(255);
            value = Some(value.unwrap_or(i32::max_value()).min(val));
        }
        _ => (),
    });
    value
}

pub fn unload_cooldown(config: &Config, game: Game, unit: Unit) -> Option<u8> {
    upgrade_min_u8(config, game, unit, Stat::UnloadCooldown)
}

pub fn player_color(config: &Config, game: Game, unit: Unit) -> Option<(f32, f32, f32)> {
    let mut color = None;
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::PlayerColor => {
            color = Some((
                clamp_u8(vals[0].eval_with_unit(unit, game)) as f32 / 255.0,
                clamp_u8(vals[1].eval_with_unit(unit, game)) as f32 / 255.0,
                clamp_u8(vals[2].eval_with_unit(unit, game)) as f32 / 255.0,
            ));
        }
        _ => (),
    });
    color
}

pub fn player_color_palette(config: &Config, game: Game, unit: Unit) -> Option<[u8; 8]> {
    let mut color = None;
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::PlayerColorPalette => {
            let mut result = [0; 8];
            for (val, out) in vals.iter().zip(result.iter_mut()) {
                *out = clamp_u8(val.eval_with_unit(unit, game));
            }
            color = Some(result);
        }
        _ => (),
    });
    color
}

pub struct ShowStats {
    pub energy: Option<bool>,
    pub shields: Option<bool>,
}

pub fn show_stats(config: &Config, game: Game, unit: Unit) -> ShowStats {
    let mut result = ShowStats {
        energy: None,
        shields: None,
    };
    config.upgrades.matches(game, unit, |stat, vals| match *stat {
        Stat::ShowEnergy => {
            let val = vals[0].eval_with_unit(unit, game) != 0;
            result.energy = Some(result.energy.unwrap_or(false).max(val));
        }
        Stat::ShowShields => {
            let val = vals[0].eval_with_unit(unit, game) != 0;
            result.shields = Some(result.shields.unwrap_or(false).max(val));
        }
        _ => (),
    });
    result
}
