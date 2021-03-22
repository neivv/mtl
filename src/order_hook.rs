use std::ptr::null_mut;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Mutex, MutexGuard};

use libc::c_void;

use bw_dat::unit as unit_id;
use bw_dat::{
    Game, Unit, UnitId, UpgradeId, Sprite, SpriteId, order, upgrade, WeaponId, UnitArray,
};

use crate::auras::{self, AuraState};
use crate::bw;
use crate::config::{config, Config, RallyOrder, OrderOrRclick};
use crate::upgrades::{self, Stat};
use crate::unit_search::UnitSearch;
use crate::unit::{self, UnitExt};

struct MiningOverride {
    old_amount: u16,
    carry: u8,
    resource: Unit,
}

struct BwGlobals {
    game: Game,
    players: *mut bw::Player,
    pathing: *mut bw::Pathing,
}

// Cache unit search accesses for frame, as currently it is only being used in the bunker
// order (Hidden unit order, so only unit changes should be deaths)
// CACHED_UNIT_SEARCH_FRAME has to be set to !0 on loading and game init to surely
// invalidate the cache.
// Arc for simplicity, but obviously getting the search from thread other than main is
// pretty much always racy.
lazy_static! {
    static ref CACHED_UNIT_SEARCH: Mutex<Option<UnitSearch>> = Mutex::new(None);
}
static CACHED_UNIT_SEARCH_FRAME: AtomicUsize = AtomicUsize::new(0);

pub fn invalidate_cached_unit_search() {
    CACHED_UNIT_SEARCH_FRAME.store(!0, Ordering::Relaxed);
}

struct LazyInitUnitSearch {
    valid_frame: &'static AtomicUsize,
    guard: MutexGuard<'static, Option<UnitSearch>>,
}

impl LazyInitUnitSearch {
    fn init(&mut self, game: Game) -> &UnitSearch {
        let frame = game.frame_count() as usize;
        if self.valid_frame.load(Ordering::Relaxed) != frame || self.guard.is_none() {
            let search = unsafe { UnitSearch::from_bw() };
            *self.guard = Some(search);
            self.valid_frame.store(frame, Ordering::Relaxed);
        }
        self.guard.as_ref().unwrap()
    }
}

fn apply_aura_u8(
    state: &AuraState,
    unit: Unit,
    stat: Stat,
    base: u8,
    unit_array: &UnitArray,
) -> u8 {
    let diff = state.unit_stat(unit, stat, unit_array);
    let result = (base as i32).saturating_add(diff);
    if result < 0 {
        0
    } else if result > 255 {
        255
    } else {
        result as u8
    }
}

pub unsafe extern fn order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    use bw_dat::order::*;

    let config = config();
    let unit_array = &bw::unit_array();
    let aura_state_guard = auras::aura_state();
    let aura_state = &aura_state_guard;
    let game = crate::game::get();
    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();
    let order = unit.order();
    let mut return_cargo_order = false;
    let mut return_cargo_softcoded = false;
    let mut harvest_gas_check = false;
    let mut harvest_minerals_check = false;
    let mut unload_check = false;
    let mut mining_override = None;
    let mut upgrade_check = None;
    let mut rally_check = None;
    let ground_cooldown_check = (**unit).ground_cooldown == 0;
    let air_cooldown_check = (**unit).air_cooldown == 0;
    let mut old_buttons = 0;
    let mut currently_building = None;

    match order {
        HARVEST_GAS => {
            return_cargo_order = true;
            harvest_gas_check = unit.order_state() == 0;
            if unit.order_state() == 5 && (**unit).order_timer == 0 {
                if let Some(target) = unit.target() {
                    let reduce = apply_aura_u8(
                        aura_state,
                        unit,
                        Stat::GasHarvestReduce,
                        upgrades::gas_harvest_reduce(&config, game, unit).unwrap_or(8),
                        unit_array,
                    );
                    let carry = apply_aura_u8(
                        aura_state,
                        unit,
                        Stat::GasHarvestCarry,
                        upgrades::gas_harvest_carry(&config, game, unit).unwrap_or(8),
                        unit_array,
                    );
                    let depleted = apply_aura_u8(
                        aura_state,
                        unit,
                        Stat::GasHarvestCarryDepleted,
                        upgrades::gas_harvest_carry_depleted(&config, game, unit).unwrap_or(2),
                        unit_array,
                    );
                    if reduce != 8 || carry != 8 || depleted != 2 {
                        let reduce = u16::from(reduce);
                        let current_resources = target.resource_amount();
                        // Can't mine less than 8 gas at once without it being depleted mining.
                        let (amount, carry) = if current_resources < 8 {
                            (0, depleted)
                        } else if current_resources <= reduce {
                            // Depletion message is only shown once remaining amount is
                            // between 0 and 7.
                            (8, depleted)
                        } else {
                            (current_resources - reduce + 8, carry)
                        };
                        target.set_resource_amount(amount);
                        mining_override = Some(MiningOverride {
                            old_amount: current_resources,
                            carry,
                            resource: target,
                        });
                    }
                }
            }
        }
        HARVEST_MINERALS => {
            harvest_minerals_check = unit.order_state() == 0 || unit.order_state() == 4;
            if unit.order_state() == 5 && (**unit).order_timer == 0 {
                if let Some(target) = unit.target() {
                    let reduce = apply_aura_u8(
                        aura_state,
                        unit,
                        Stat::MineralHarvestReduce,
                        upgrades::mineral_harvest_reduce(&config, game, unit).unwrap_or(8),
                        unit_array,
                    );
                    let carry = apply_aura_u8(
                        aura_state,
                        unit,
                        Stat::MineralHarvestCarry,
                        upgrades::mineral_harvest_carry(&config, game, unit).unwrap_or(8),
                        unit_array,
                    );
                    if reduce != 8 || carry != 8 {
                        let reduce = u16::from(reduce);
                        let current_resources = target.resource_amount();
                        let (amount, carry) = if current_resources <= reduce {
                            (1, current_resources as u8)
                        } else {
                            (current_resources - reduce + 8, carry)
                        };
                        target.set_resource_amount(amount);
                        mining_override = Some(MiningOverride {
                            old_amount: current_resources,
                            carry,
                            resource: target,
                        });
                    }
                }
            }
        }
        RETURN_MINERALS | RETURN_GAS | RESET_COLLISION_HARVESTER => {
            return_cargo_order = true;
        }
        UNLOAD | MOVE_UNLOAD => {
            unload_check = (**unit).order_timer == 0;
        }
        UPGRADE => {
            let upgrade = UpgradeId(u16::from((**unit).unit_specific[9]));
            let level = (**unit).unit_specific[0xd];
            if upgrade != upgrade::NONE {
                if game.upgrade_level(unit.player(), upgrade) < level {
                    upgrade_check = Some((upgrade, level));
                }
            }
        }
        BUNKER_GUARD => {
            if !config.bunker_units.is_empty() {
                let sprite_id = config.bunker_units.iter()
                    .find(|x| x.0 == unit.id())
                    .map(|x| (x.1, x.2));
                if let Some((sprite_id, directions)) = sprite_id {
                    let players = bw::players();
                    let mut search = LazyInitUnitSearch {
                        valid_frame: &CACHED_UNIT_SEARCH_FRAME,
                        guard: CACHED_UNIT_SEARCH.lock().unwrap(),
                    };
                    let globals = BwGlobals {
                        game,
                        players,
                        pathing: bw::pathing(),
                    };

                    order_bunker_guard(
                        &globals,
                        &mut search,
                        &config,
                        unit,
                        sprite_id,
                        directions,
                    );
                }
                return;
            }
        }
        ZERG_BIRTH => {
            if !unit.is_completed() {
                if let Some(parent) = unit.related() {
                    // BW actually checks for != cocoon && != lurker egg,
                    // checking for egg instead as if this is ever softcoded
                    // it would make sense to specify list of units that will
                    // rally on zerg birth (defaulting to egg),
                    // instead of a list of units that won't.
                    if UnitId((**unit).previous_unit_id) == unit_id::EGG {
                        rally_check = RallyCheck::new(unit, parent);
                    }
                }
            }
        }
        DRONE_BUILD2 => {
            old_buttons = (**unit).buttons;
        }
        SCV_BUILD | PROBE_BUILD => {
            currently_building = unit.currently_building();
        }
        _ => (),
    }
    if return_cargo_order {
        if config.return_cargo_softcode {
            return_cargo_softcoded = true;
            let player = unit.player();
            // BW only searches for resource depots if the player owns at least one of the
            // five hardcoded depots.
            // Harvest gas searches for the depots when deciding where the worker should
            // be spawned when exiting the gas mine.
            // The reset collision order is a highprio order, so it'll be able to
            // execute return cargo order immediately afterwards.
            game.set_completed_count(
                player,
                unit_id::COMMAND_CENTER,
                game.completed_count(player, unit_id::COMMAND_CENTER) + 1,
            );
        }
    }
    drop(aura_state_guard);
    orig(u);
    let aura_state_guard = auras::aura_state();
    let aura_state = &aura_state_guard;
    match order {
        DRONE_BUILD2 => {
            if (**unit).buttons != old_buttons {
                if unit.id().is_building() {
                    // Set buttonset with/without rally
                    (**unit).buttons = match config.has_rally(unit.id()) {
                        true => 0xea,
                        false => 0xe9,
                    };
                }
            }
        }
        SCV_BUILD | PROBE_BUILD => {
            if currently_building.is_none() {
                if let Some(new) = unit.currently_building() {
                    // Set buttonset with/without rally
                    (**new).buttons = match config.has_rally(new.id()) {
                        true => 0xe8,
                        false => 0xe7,
                    };
                }
            }
        }
        _ => (),
    }
    if return_cargo_softcoded {
        let player = unit.player();
        game.set_completed_count(
            player,
            unit_id::COMMAND_CENTER,
            game.completed_count(player, unit_id::COMMAND_CENTER) - 1,
        );
    }
    if harvest_gas_check {
        if unit.order_state() == 5 {
            let new_timer = apply_aura_u8(
                aura_state,
                unit,
                Stat::GasHarvestTime,
                upgrades::gas_harvest_time(&config, game, unit)
                    .unwrap_or((**unit).order_timer),
                unit_array,
            );
            (**unit).order_timer = new_timer;
        }
    }
    if harvest_minerals_check {
        if unit.order_state() == 5 {
            let new_timer = apply_aura_u8(
                aura_state,
                unit,
                Stat::MineralHarvestTime,
                upgrades::mineral_harvest_time(&config, game, unit)
                    .unwrap_or((**unit).order_timer),
                unit_array,
            );
            (**unit).order_timer = new_timer;
        }
    }
    if unload_check {
        if (**unit).order_timer > 0 {
            let new_timer = apply_aura_u8(
                aura_state,
                unit,
                Stat::UnloadCooldown,
                upgrades::unload_cooldown(&config, game, unit)
                    .unwrap_or((**unit).order_timer),
                unit_array,
            );
            (**unit).order_timer = new_timer;
        }
    }
    if ground_cooldown_check {
        if (**unit).ground_cooldown > 0 {
            let upgrade_cooldown = upgrades::ground_cooldown(&config, game, unit)
                .or_else(|| upgrades::cooldown(&config, game, unit));
            let new_cooldown = apply_aura_u8(
                aura_state,
                unit,
                Stat::GroundCooldown,
                upgrade_cooldown.unwrap_or((**unit).ground_cooldown),
                unit_array,
            );
            let new_cooldown = apply_aura_u8(
                aura_state,
                unit,
                Stat::Cooldown,
                new_cooldown,
                unit_array,
            );
            (**unit).ground_cooldown = new_cooldown;
        }
    }
    if air_cooldown_check {
        if (**unit).air_cooldown > 0 {
            let upgrade_cooldown = upgrades::air_cooldown(&config, game, unit)
                .or_else(|| upgrades::cooldown(&config, game, unit));
            let new_cooldown = apply_aura_u8(
                aura_state,
                unit,
                Stat::AirCooldown,
                upgrade_cooldown.unwrap_or((**unit).air_cooldown),
                unit_array,
            );
            let new_cooldown = apply_aura_u8(
                aura_state,
                unit,
                Stat::Cooldown,
                new_cooldown,
                unit_array,
            );
            (**unit).air_cooldown = new_cooldown;
        }
    }
    if let Some(vars) = mining_override {
        if unit.target() == Some(vars.resource) {
            vars.resource.set_resource_amount(vars.old_amount);
        } else {
            (**unit).unit_specific[0xf] = vars.carry;
        }
    }
    if let Some((upgrade, level)) = upgrade_check {
        if game.upgrade_level(unit.player(), upgrade) == level {
            let mut upgrades = upgrades::global_state_changes();
            upgrades.upgrade_gained(&config, game, unit.player(), upgrade, level);
        }
    }
    if let Some(rally) = rally_check {
        rally.rally_if_completed(game, unit_array, &config);
    }
}

pub unsafe extern fn hidden_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    order_hook(u, orig);
}

pub unsafe extern fn secondary_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    use bw_dat::order::*;

    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();
    let config = config();

    let mut spread_creep_check = false;
    let mut larva_spawn_check = false;
    let mut creep_spread_time = None;
    let mut larva_spawn_time = None;
    let mut zerg_group_flags = None;
    let mut rally_check = None;
    match unit.secondary_order() {
        TRAIN => {
            if config.zerg_building_training {
                let group_flags =
                    (bw::units_dat()[0x2c].data as *mut u8).offset(unit.id().0 as isize);
                let orig_flags = *group_flags;
                *group_flags &= !0x1;
                zerg_group_flags = Some((group_flags, orig_flags));
            }
            if let Some(child) = unit.currently_building() {
                rally_check = RallyCheck::new(child, unit);
            }
        }
        SPREAD_CREEP => {
            let game = crate::game::get();
            if let Some(new_timer) = upgrades::creep_spread_time(&config, game, unit) {
                if new_timer == -1 {
                    (**unit).unit_specific[0xc] = 10;
                } else {
                    creep_spread_time = Some(new_timer as u8);
                }
            }
            if let Some(new_timer) = upgrades::larva_spawn_time(&config, game, unit) {
                if new_timer == -1 {
                    (**unit).unit_specific[0xa] = 10;
                } else {
                    larva_spawn_time = Some(new_timer as u8);
                }
            }
            spread_creep_check = (**unit).unit_specific[0xc] == 0;
            larva_spawn_check = (**unit).unit_specific[0xa] == 0;
        }
        SPAWNING_LARVA => {
            let game = crate::game::get();
            if let Some(new_timer) = upgrades::larva_spawn_time(&config, game, unit) {
                if new_timer == -1 {
                    (**unit).unit_specific[0xa] = 10;
                } else {
                    larva_spawn_time = Some(new_timer as u8);
                }
            }
            larva_spawn_check = (**unit).unit_specific[0xa] == 0;
        }
        _ => ()
    }
    orig(u);
    if spread_creep_check {
        let unit_array = &bw::unit_array();
        let aura_state = &auras::aura_state();
        if (**unit).unit_specific[0xc] != 0 {
            if let Some(new_timer) = creep_spread_time {
                let new_timer = apply_aura_u8(
                    aura_state,
                    unit,
                    Stat::CreepSpreadTimer,
                    new_timer,
                    unit_array,
                );
                (**unit).unit_specific[0xc] = new_timer;
            }
        }
    }
    if larva_spawn_check {
        let unit_array = &bw::unit_array();
        let aura_state = &auras::aura_state();
        if (**unit).unit_specific[0xa] != 0 {
            if let Some(new_timer) = larva_spawn_time {
                let new_timer = apply_aura_u8(
                    aura_state,
                    unit,
                    Stat::LarvaTimer,
                    new_timer,
                    unit_array,
                );
                (**unit).unit_specific[0xa] = new_timer;
            }
        }
    }
    if let Some((out, orig)) = zerg_group_flags {
        *out = orig;
    }
    if let Some(rally) = rally_check {
        let units = &bw::unit_array();
        let game = crate::game::get();
        rally.rally_if_completed(game, units, &config);
    }
}

struct RallyCheck {
    unit: Unit,
    /// Second active unit after dual birth is the dual birth copy
    /// (New active units are not inserted first, but second)
    /// This is the second active unit *before* dual birth, if it
    /// has changed during the order, then the second active unit
    /// is a new unit that needs to be rallied.
    dual_birth_active1: Option<Unit>,
    rally: UnitOrPoint,
}

impl RallyCheck {
    /// Returns None if the parent has no rally point set
    pub fn new(unit: Unit, parent: Unit) -> Option<RallyCheck> {
        let rally_unit = unsafe {
            Unit::from_ptr(*((**parent).rally_pylon.as_ptr().add(4) as *mut *mut bw::Unit))
        };
        let rally_point = unsafe {
            *((**parent).rally_pylon.as_ptr() as *mut bw::Point)
        };
        if rally_unit == Some(parent) || rally_point.x == 0 {
            // Rally not set
            return None;
        }
        let rally = match rally_unit {
            Some(s) => UnitOrPoint::Unit(s),
            None => UnitOrPoint::Point(rally_point),
        };
        let dual_birth_active1 = unit::active_units().nth(1);
        Some(RallyCheck {
            unit,
            dual_birth_active1,
            rally,
        })
    }

    pub fn rally_if_completed(&self, game: Game, units: &UnitArray, config: &Config) {
        if !self.unit.is_completed() {
            return;
        }
        let rally_order = match config.rally_order(self.unit.id()) {
            Some(s) => s,
            None => RallyOrder {
                ground: order::MOVE,
                unit: OrderOrRclick::Order(order::FOLLOW),
            },
        };
        self.rally(game, units, self.unit, &rally_order);
        if self.unit.id().flags() & 0x400 != 0 {
            let other = unit::active_units().nth(1);
            if self.dual_birth_active1.is_none() || self.dual_birth_active1 != other {
                if let Some(other) = other {
                    self.rally(game, units, other, &rally_order);
                }
            }
        }
    }

    fn rally(&self, game: Game, units: &UnitArray, unit: Unit, order: &RallyOrder) {
        match self.rally {
            UnitOrPoint::Unit(target) => {
                let order = match order.unit {
                    OrderOrRclick::Rclick => unit.rclick_order(game, units, target),
                    OrderOrRclick::Order(o) => o,
                };
                unit.issue_order_unit(order, target);
            }
            UnitOrPoint::Point(point) => {
                unit.issue_order_ground(order.ground, point);
            }
        }
    }
}

enum UnitOrPoint {
    Unit(Unit),
    Point(bw::Point),
}

unsafe fn order_bunker_guard(
    globals: &BwGlobals,
    search: &mut LazyInitUnitSearch,
    config: &Config,
    unit: Unit,
    attack_sprite: SpriteId,
    directions: u8,
) {
    (**unit).flags |= 0x0800_0000;
    assert!(unit.subunit_linked().is_none());
    let attacking = attack_at_point(globals, search, config, unit, attack_sprite, directions);
    if attacking {
        (**unit).unk_move_waypoint = (**unit).order_target_pos;
    } else {
        if (**unit).order_timer == 0 {
            (**unit).order_timer = 15;
            if let Some(target) = pick_random_target(globals, search.init(globals.game), unit) {
                (**unit).target = *target;
                (**unit).order_wait = 0;
            } else {
                (**unit).target = null_mut();
            }
        }
    }
}

unsafe fn attack_at_point(
    globals: &BwGlobals,
    search: &mut LazyInitUnitSearch,
    config: &Config,
    unit: Unit,
    attack_sprite: SpriteId,
    directions: u8,
) -> bool {
    // Fixme: cooldown aura effects aren't applied below,
    // so this doesn't work properly for other orders if that isn't done
    assert!(unit.is_hidden());

    let target = match unit.target().is_some() {
        true => {
            pick_new_attack_target_if_needed(globals, search, unit);
            match unit.target() {
                Some(s) => s,
                None => return false,
            }
        }
        false => return false,
    };
    if !can_see_unit(unit, target) {
        return false;
    }
    if !can_attack_unit(globals, unit, target) {
        return false;
    }
    (**unit).order_target_pos = target.position();
    if unit.subunit_linked().map(|x| x.id().is_subunit()).unwrap_or(false) {
        // Subunit handles its attacking by itself
        return true;
    }
    let weapon;
    let cooldown;
    let animation;
    if target.is_air() {
        weapon = unit.id().air_weapon().unwrap_or(WeaponId(0));
        cooldown = (**unit).air_cooldown;
        animation = 6;
    } else {
        weapon = unit.id().ground_weapon().unwrap_or(WeaponId(0));
        cooldown = (**unit).ground_cooldown;
        animation = 5;
    }
    if cooldown != 0 {
        if (**unit).order_wait > cooldown - 1 {
            (**unit).order_wait = cooldown - 1;
        }
        return true;
    }
    if (**unit).flingy_flags & 0x8 != 0 {
        // Waiting for iscript gotorepeatattk
        return true;
    }
    let distance = bw::rect_distance(&unit.collision_rect(), &target.collision_rect());
    if let Some(range) = weapon.min_range() {
        if range.get() > distance {
            return false;
        }
    }
    if weapon_range(globals.game, unit, weapon) < distance {
        return false;
    }
    let angle = angle_to(&unit.position(), &target.position());
    if angle_diff((**unit).facing_direction, angle) > weapon.attack_angle() {
        if unit.flags() & 0x0001_0000 != 0 {
            (**unit).order_wait = 0;
            return true;
        } else {
            return false;
        }
    }

    if unit.flags() & 0x8000 != 0 {
        // Under dweb
        return false;
    }
    if unit.in_bunker() {
        create_bunker_shoot_overlay(unit, attack_sprite, directions);
    }
    // gotorepeatattk flag
    (**unit).flingy_flags |= 0x8;
    let mut rng = crate::rng::get();
    // Note: Should probably be refactored to check auras, but at the same time
    // auras won't apply to hidden units
    let cooldown = match upgrades::cooldown(&config, globals.game, unit) {
        Some(s) => s,
        None => default_cooldown(unit, weapon),
    };
    let cooldown = (cooldown as i32 + (rng.synced_rand(0..3) as i32) - 1) as u8;
    (**unit).ground_cooldown = cooldown;
    (**unit).air_cooldown = cooldown;
    if let Some(sprite) = unit.sprite() {
        set_iscript_animation(sprite, animation, true);
    }
    true
}

unsafe fn set_iscript_animation(sprite: Sprite, animation: u8, force: bool) {
    if sprite.flags() & 0x80 != 0 && !force {
        return;
    }
    for image in sprite.images() {
        bw::set_iscript_animation(*image, animation);
    }
}

unsafe fn default_cooldown(unit: Unit, weapon: WeaponId) -> u8 {
    let mut cooldown = weapon.cooldown();
    if (**unit).acid_spore_count != 0 {
        let mul = (cooldown / 8).max(3);
        cooldown += (**unit).acid_spore_count as u32 * mul;
    }
    let mut buffs = 0i32;
    if (**unit).stim_timer != 0 {
        buffs += 1;
    }
    if unit.flags() & 0x2000_0000 != 0 {
        // Attack speed upgrade
        buffs += 1;
    }
    if (**unit).ensnare_timer != 0 {
        buffs -= 1;
    }
    if buffs > 0 {
        cooldown = cooldown / 2;
    }
    if buffs < 0 {
        cooldown = cooldown + cooldown / 2;
    }
    cooldown.max(5).min(250) as u8
}

fn angle_diff(a: u8, b: u8) -> u8 {
    let diff = a.wrapping_sub(b);
    if diff <= 0x80 {
        diff
    } else {
        0xff - diff + 1
    }
}

unsafe fn pick_new_attack_target_if_needed(
    globals: &BwGlobals,
    search: &mut LazyInitUnitSearch,
    unit: Unit,
) {
    if unit.flags() & 0x0800_0000 == 0 {
        // Not allowed to switch targets (User ordered without attack move)
        return;
    }
    let mut picked_target = None;
    let mut picked_threat_level = 10;
    if let Some(target) = unit.target() {
        if unit.has_ai() {
            if !unit.is_enemy(globals.game, target) {
                // ???
                // Ai makes sure that revenging won't be interrupted??
                return;
            }
        }
        if can_attack_unit(globals, unit, target) && !ai_should_ignore_target(globals, unit, target) {
            let mut threat_level = get_threat_level(globals, unit, target);
            if threat_level == 0 {
                if is_in_attack_range(unit, target) {
                    // Just keep the current target since it is in range and a threat
                    return;
                } else {
                    threat_level = 1;
                }
            }
            picked_target = Some(target);
            picked_threat_level = threat_level;
        }
    }
    if let Some(attacker) = Unit::from_ptr((**unit).previous_attacker) {
        let prev_attacker_ok = can_attack_unit(globals, unit, attacker) &&
            unit.is_enemy(globals.game, attacker) &&
            !ai_should_ignore_target(globals, unit, attacker);
        if !prev_attacker_ok {
            (**unit).previous_attacker = null_mut();
        } else {
            let mut threat_level = get_threat_level(globals, unit, attacker);
            if threat_level == 0 && !is_in_attack_range(unit, attacker) {
                threat_level = 1;
            }
            if threat_level < picked_threat_level {
                picked_target = Some(attacker);
                picked_threat_level = threat_level;
            }
        }
    }
    if picked_threat_level > 0 {
        let updated = bw::ai_update_attack_target(*unit, false, true, false);
        if updated {
            return;
        }
        if let Some(target) = get_auto_target(globals, search.init(globals.game), unit) {
            let threat_level = get_threat_level(globals, unit, target);
            if threat_level < picked_threat_level {
                picked_target = Some(target);
            }
        }
    }

    if let Some(target) = picked_target {
        if !can_attack_unit(globals, unit, target) {
            return;
        }
        (**unit).target = *target;
        if let Some(subunit) = unit.subunit_linked() {
            let attack_order = subunit.id().attack_unit_order();
            if attack_order == subunit.order() || attack_order == order::HOLD_POSITION {
                (**subunit).target = *target;
            }
        }
    }
}

unsafe fn can_attack_unit(globals: &BwGlobals, attacker: Unit, target: Unit) -> bool {
    if attacker.is_disabled() {
        return false;
    }
    if target.is_invincible() || target.is_hidden() {
        return false;
    }
    if target.is_invisible_hidden_to(attacker.player()) {
        return false;
    }
    let turret = attacker.subunit_turret();
    match attacker.id() {
        unit_id::CARRIER | unit_id::GANTRITHOR => true,
        unit_id::REAVER | unit_id::WARBRINGER => {
            !target.is_air() && are_on_same_region_group(globals.pathing, attacker, target)
        }
        unit_id::QUEEN | unit_id::MATRIARCH => can_be_infested(target),
        _ => {
            if attacker.id() == unit_id::ARBITER && attacker.has_ai() {
                // Ai arbiters don't attack
                return false;
            }
            if target.is_air() {
                turret.id().air_weapon().is_some()
            } else {
                turret.id().ground_weapon().is_some()
            }
        }
    }
}

// Smaller value => better priority
unsafe fn get_threat_level(globals: &BwGlobals, unit: Unit, enemy: Unit) -> u8 {
    match enemy.id() {
        unit_id::BUNKER => {
            if let Some(loaded) = find_bunker_loaded_unit(enemy) {
                return get_threat_level(globals, unit, loaded) + 1;
            }
        }
        unit_id::LARVA | unit_id::EGG | unit_id::COCOON | unit_id::LURKER_EGG => return 5,
        _ => (),
    }
    if enemy.id().is_worker() {
        return 2;
    }
    let base_value = if can_attack_unit(globals, enemy, unit) {
        0
    } else if has_way_of_attacking(enemy) {
        2
    } else if enemy.flags() & 0x0002_0000 != 0 { // Smart
        3
    } else {
        4
    };
    if !enemy.is_completed() {
        base_value + 1
    } else if base_value != 0 {
        base_value
    } else {
        // Is under dweb
        if enemy.flags() & 0x8000 != 0 {
            0
        } else {
            1
        }
    }
}

unsafe fn weapon_range(game: Game, unit: Unit, weapon: WeaponId) -> u32 {
    let mut range = 0;
    if unit.in_bunker() {
        range += 0x40;
    }
    match unit.id() {
        unit_id::MARINE => {
            if game.upgrade_level(unit.player(), upgrade::U_268_SHELLS) != 0 {
                range += 0x20;
            }
        }
        unit_id::GOLIATH | unit_id::GOLIATH_TURRET => {
            if weapon.0 == 8 {
                if game.upgrade_level(unit.player(), upgrade::CHARON_BOOSTER) != 0 {
                    range += 0x60;
                }
            }
        }
        unit_id::HYDRALISK => {
            if game.upgrade_level(unit.player(), upgrade::GROOVED_SPINES) != 0 {
                range += 0x20;
            }
        }
        unit_id::DRAGOON => {
            if game.upgrade_level(unit.player(), upgrade::SINGULARITY_CHARGE) != 0 {
                range += 0x40;
            }
        }
        unit_id::FENIX_DRAGOON => {
            range += 0x20;
        }
        unit_id::ALAN_SCHEZAR | unit_id::SCHEZAR_TURRET => {
            if weapon.0 == 0xa {
                range += 0x60;
            }
        }
        _ => (),
    }
    range + weapon.max_range()
}

unsafe fn target_acquisition_range(game: Game, unit: Unit) -> u8 {
    let mut bonus = 0;
    match unit.id() {
        unit_id::GHOST | unit_id::SARAH_KERRIGAN | unit_id::SAMIR_DURAN |
            unit_id::ALEXEI_STUKOV | unit_id::INFESTED_DURAN =>
        {
            if unit.is_invisible() && unit.order() != order::HOLD_POSITION {
                return 0;
            }
        }
        unit_id::MARINE => {
            if game.upgrade_level(unit.player(), upgrade::U_268_SHELLS) != 0 {
                bonus = 1;
            }
        }
        unit_id::GOLIATH | unit_id::GOLIATH_TURRET => {
            if game.upgrade_level(unit.player(), upgrade::CHARON_BOOSTER) != 0 {
                bonus = 3;
            }
        }
        unit_id::HYDRALISK => {
            if game.upgrade_level(unit.player(), upgrade::GROOVED_SPINES) != 0 {
                bonus = 1;
            }
        }
        unit_id::DRAGOON => {
            if game.upgrade_level(unit.player(), upgrade::SINGULARITY_CHARGE) != 0 {
                bonus = 2;
            }
        }
        unit_id::FENIX_DRAGOON => {
            bonus = 2;
        }
        unit_id::ALAN_SCHEZAR | unit_id::SCHEZAR_TURRET => {
            bonus = 3;
        }
        _ => (),
    }
    unit.id().target_acquisition_range().saturating_add(bonus)
}

unsafe fn get_auto_target(
    globals: &BwGlobals,
    search: &UnitSearch,
    unit: Unit,
) -> Option<Unit> {
    let mut range = target_acquisition_range(globals.game, unit);
    if unit.in_bunker() {
        // Bunker extra range
        range += 2;
    } else if unit.has_ai() {
        range = range.max(unit.id().sight_range());
    }
    let pos = unit.position();
    let max_range = range as i16 * 32;
    let turret = unit.subunit_turret();
    let min_range = {
        let ground_weapon = turret.id().ground_weapon();
        let air_weapon = turret.id().air_weapon();
        match (ground_weapon, air_weapon) {
            (None, Some(s)) | (Some(s), None) => s.min_range().map(|x| x.get()).unwrap_or(0),
            (None, None) => 0,
            (Some(a), Some(b)) => {
                a.min_range().map(|x| x.get()).unwrap_or(0).min(
                    b.min_range().map(|x| x.get()).unwrap_or(0)
                )
            }
        }
    };
    let area = bw::Rect {
        left: pos.x.saturating_sub(max_range),
        top: pos.y.saturating_sub(max_range),
        right: pos.x.saturating_add(max_range),
        bottom: pos.y.saturating_add(max_range),
    };
    let units = search
        .search_iter(&area)
        .filter(|&x| unit.is_enemy(globals.game, x))
        .filter(|&x| x.is_visible_to(unit.player()))
        .filter(|&x| can_attack_unit(globals, unit, x));
    let ground_weapon = turret.id().ground_weapon().unwrap_or(WeaponId(0));
    let air_weapon = turret.id().air_weapon().unwrap_or(WeaponId(0));
    let can_turn = turret.flags() & 0x0001_0000 != 0;
    let mut results = (0..6).map(|_| Vec::new()).collect::<Vec<Vec<Unit>>>();

    let this_crect = unit.subunit_parent().collision_rect();
    for candidate in units {
        let other_crect = candidate.collision_rect();
        let distance = bw::rect_distance(&this_crect, &other_crect);
        if distance < min_range || distance > max_range as u32 {
            continue;
        }
        if !can_turn {
            let weapon = match candidate.is_air() {
                true => air_weapon,
                false => ground_weapon,
            };
            if !check_firing_angle(turret, &candidate.position(), weapon) {
                continue;
            }
        }
        if !ai_should_ignore_target(globals, unit, candidate) {
            let threat_level = get_threat_level(globals, unit, candidate);
            let vec = &mut results[threat_level as usize];
            if vec.len() < 16 {
                vec.push(candidate);
            }
            if threat_level == 0 && vec.len() == 16 {
                // Early exit since first vector is always used if not empty
                break;
            }
        }
    }
    if let Some(list) = results.iter().find(|x| !x.is_empty()) {
        pick_best_target(unit, &list)
    } else {
        None
    }
}

unsafe fn check_firing_angle(unit: Unit, dest: &bw::Point, weapon: WeaponId) -> bool {
    let angle = angle_to(&unit.position(), dest);
    // Bw would always succeed for lurker and sets its facing direction
    angle_diff(angle, (**unit).facing_direction) <= weapon.attack_angle()
}

fn angle_to(from: &bw::Point, to: &bw::Point) -> u8 {
    let x_diff = to.x.saturating_sub(from.x);
    let y_diff = to.y.saturating_sub(from.y);
    if x_diff == 0 {
        if y_diff > 0 {
            128
        } else {
            0
        }
    } else {
        degrees_to_bw_angle((-y_diff as f32).atan2(x_diff as f32).to_degrees())
    }
}

fn degrees_to_bw_angle(value: f32) -> u8 {
    (((value % 360.0) * 256.0 / -360.0) + 64.0) as i32 as u8
}

#[test]
fn angle_to_test() {
    let point = |x, y| bw::Point {
        x,
        y,
    };
    assert_eq!(angle_to(&point(5, 5), &point(10, 5)), 64);
    assert_eq!(angle_to(&point(15, 5), &point(10, 5)), 192);
    assert_eq!(angle_to(&point(5, 5), &point(5, 10)), 128);
    assert_eq!(angle_to(&point(5, 15), &point(5, 10)), 0);
    assert_eq!(angle_to(&point(5, 5), &point(10, 10)), 96);
    assert_eq!(angle_to(&point(10, 10), &point(5, 5)), 224);
}

unsafe fn pick_best_target(unit: Unit, list: &[Unit]) -> Option<Unit> {
    if unit.has_ai() && unit.id() == unit_id::SCOURGE {
        list.iter().min_by_key(|x| x.health()).cloned()
    } else {
        list.iter().min_by_key(|x| bw::distance(x.position(), unit.position())).cloned()
    }
}

unsafe fn create_bunker_shoot_overlay(unit: Unit, sprite: SpriteId, directions: u8) {
    if directions == 0 {
        return;
    }
    let bunker = unit.related().expect("Not in bunker");
    let bunker_image = match bunker.sprite().and_then(|s| s.main_image()) {
        Some(s) => s,
        None => return,
    };
    let unit_direction = (**unit).facing_direction as u16;
    let lo_direction = ((unit_direction + 16) / 32) as u8 & 0x7;

    // Optimized form of
    // let direction_size = 256 / directions;
    // let sprite_direction = ((unit_dir + direction_size / 2) / direction_size) as u8 &
    //      (directions - 1) * (16 / directions) * 16;
    let direction_shift_div = 7 - directions.leading_zeros();
    // 256 / directions
    let direction_size = 256u16 >> direction_shift_div;
    let sprite_direction_256 = unit_direction + direction_size / 2;
    // sprite_direction_256 / direction_size
    let a = (sprite_direction_256 >> direction_size.trailing_zeros()) as u8
        & (directions - 1);
    let sprite_direction = (a * (16 >> direction_shift_div)) * 16;

    let (x, y) = match crate::lo::images_dat_attack_overlay(*bunker_image, lo_direction) {
        Some(s) => s,
        None => return,
    };
    let pos = bw::Point {
        x: unit.position().x.saturating_add(x as i16),
        y: unit.position().y.saturating_add(y as i16),
    };
    let sprite = bw::create_lone_sprite(sprite, &pos, unit.player());
    if sprite.is_null() {
        return;
    }
    if let Some(sprite) = Sprite::from_ptr((*sprite).sprite) {
        set_sprite_direction(sprite, sprite_direction);
    }
    bw::update_visibility_point(sprite);
}

unsafe fn set_sprite_direction(sprite: Sprite, direction: u8) {
    for image in sprite.images() {
        assert!((**image).flags & 0x80 == 0, "unimplemented");
        if (**image).flags & 0x8 != 0 {
            let index_32 = ((direction as u16 + 4) / 8) as u8;
            let flip;
            let index_16;
            if index_32 > 16 {
                index_16 = 32 - index_32;
                flip = true;
            } else {
                index_16 = index_32;
                flip = false;
            }
            let image_flipped = (**image).flags & 0x2 != 0;
            if (**image).direction != index_16 || image_flipped != flip {
                (**image).flags = ((**image).flags & !0x2) | ((flip as u16) << 1);
                (**image).direction = index_16;
                bw::set_image_drawfuncs(*image, (**image).drawfunc);
                (**image).flags |= 0x1;
                let frame = (**image).frameset + index_16 as u16;
                if (**image).frame != frame {
                    (**image).frame = frame;
                }
            }
        }
    }
}

fn find_bunker_loaded_unit(bunker: Unit) -> Option<Unit> {
    unit::hidden_units().find(|x| match x.related() {
        Some(s) => s == bunker,
        None => false,
    })
}

fn can_be_infested(unit: Unit) -> bool {
    unit.is_completed() && unit.id() == unit_id::COMMAND_CENTER && unit.hp_percent() < 50
}

unsafe fn ai_should_ignore_target(globals: &BwGlobals, unit: Unit, target: Unit) -> bool {
    if (*globals.players.add(unit.player() as usize)).player_type != 1 {
        return false;
    }
    if unit.is_air() {
        return false;
    }
    if is_in_attack_range(unit, target) {
        return false;
    }
    target.flags() & 0x80 != 0
}

unsafe fn is_in_attack_range(unit: Unit, target: Unit) -> bool {
    if !can_see_unit(unit, target) {
        return false;
    }
    let turret = unit.subunit_turret();
    let weapon = if target.is_air() {
        turret.id().air_weapon()
    } else {
        turret.id().ground_weapon()
    };
    let weapon = match weapon {
        Some(s) => s,
        None => return false,
    };
    let distance = bw::rect_distance(&unit.collision_rect(), &target.collision_rect());
    if let Some(range) = weapon.min_range() {
        if distance < range.get() {
            return false;
        }
    }
    let mut extra_distance = 0;
    if (**unit).flingy_flags & 0x2 != 0 && (**target).flingy_flags & 0x2 != 0 {
        let diff = angle_diff((**unit).movement_direction, (**target).movement_direction);
        if diff > 0x20 {
            extra_distance = unit.halt_distance();
        }
    } else {
        extra_distance = unit.halt_distance();
    }
    distance <= weapon.max_range().saturating_add(extra_distance)
}

unsafe fn can_see_unit(unit: Unit, other: Unit) -> bool {
    !other.is_invisible_hidden_to(unit.player()) && other.is_visible_to(unit.player())
}

fn has_way_of_attacking(unit: Unit) -> bool {
    let unit = unit.subunit_turret();
    match unit.id() {
        unit_id::CARRIER | unit_id::GANTRITHOR | unit_id::REAVER | unit_id::WARBRINGER => {
            unit.fighter_amount() != 0
        }
        x => x.ground_weapon().is_some() || x.air_weapon().is_some(),
    }
}

// TODO: Could share code with get_auto_target
unsafe fn pick_random_target(
    globals: &BwGlobals,
    search: &UnitSearch,
    unit: Unit,
) -> Option<Unit> {
    let mut range = target_acquisition_range(globals.game, unit);
    if unit.in_bunker() {
        // Bunker extra range
        range += 2;
    } else if unit.has_ai() {
        range = range.max(unit.id().sight_range());
    }
    let pos = unit.position();
    let max_range = range as i16 * 32;
    let turret = unit.subunit_turret();
    let area = bw::Rect {
        left: pos.x.saturating_sub(max_range),
        top: pos.y.saturating_sub(max_range),
        right: pos.x.saturating_add(max_range),
        bottom: pos.y.saturating_add(max_range),
    };
    let units = search
        .search_iter(&area)
        .filter(|&x| unit.is_enemy(globals.game, x))
        .filter(|&x| x.is_visible_to(unit.player()))
        .filter(|&x| can_attack_unit(globals, unit, x));
    let ground_weapon = turret.id().ground_weapon().unwrap_or(WeaponId(0));
    let air_weapon = turret.id().air_weapon().unwrap_or(WeaponId(0));
    let can_turn = turret.flags() & 0x0001_0000 != 0;
    let mut results = (0..6).map(|_| Vec::new()).collect::<Vec<Vec<Unit>>>();

    let this_crect = unit.subunit_parent().collision_rect();
    for candidate in units {
        let other_crect = candidate.collision_rect();
        let distance = bw::rect_distance(&this_crect, &other_crect);
        if distance > max_range as u32 {
            continue;
        }
        if !can_turn {
            let weapon = match candidate.is_air() {
                true => air_weapon,
                false => ground_weapon,
            };
            if !check_firing_angle(turret, &candidate.position(), weapon) {
                continue;
            }
        }
        if !ai_should_ignore_target(globals, unit, candidate) {
            let threat_level = get_threat_level(globals, unit, candidate);
            let vec = &mut results[threat_level as usize];
            if vec.len() < 16 {
                vec.push(candidate);
            }
            if threat_level == 0 && vec.len() == 16 {
                // Early exit since first vector is always used if not empty
                break;
            }
        }
    }
    if let Some(list) = results.iter().find(|x| !x.is_empty()) {
        let mut rng = crate::rng::get();
        let index = rng.synced_rand(0..list.len() as u32);
        Some(list[index as usize])
    } else {
        None
    }
}

unsafe fn are_on_same_region_group(pathing: *mut bw::Pathing, a: Unit, b: Unit) -> bool {
    let a_region = get_region(pathing, &a.position());
    let b_region = get_region(pathing, &b.position());
    (*a_region).group == (*b_region).group
}

unsafe fn get_region(pathing: *mut bw::Pathing, position: &bw::Point) -> *mut bw::Region {
    let tile_index = position.y as usize / 32 * 0x100 + position.x as usize / 32;
    let region = (*pathing).map_tile_regions[tile_index];
    if region >= 0x2000 {
        let split = &(*pathing).split_regions[region as usize - 0x2000];
        let bit_index = ((position.y as usize / 8) & 0x3) * 4 + ((position.x as usize / 8) & 0x3);
        let region = if split.minitile_flags & (1 << bit_index) == 0 {
            split.region_false
        } else {
            split.region_true
        };
        &mut (*pathing).regions[region as usize]
    } else {
        &mut (*pathing).regions[region as usize]
    }
}
