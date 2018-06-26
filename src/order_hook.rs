use libc::c_void;

use bw_dat::unit as unit_id;
use bw;
use config::config;
use game::Game;
use unit::Unit;
use upgrades;

struct MiningOverride {
    old_amount: u16,
    carry: u8,
    resource: Unit,
}

pub unsafe extern fn order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    use bw_dat::order::*;

    let config = config();
    let game = Game::get();
    let unit = Unit(u as *mut bw::Unit);
    let order = unit.order();
    let mut return_cargo_order = false;
    let mut return_cargo_softcoded = false;
    let mut harvest_gas_check = false;
    let mut harvest_minerals_check = false;
    let mut unload_check = false;
    let mut mining_override = None;
    let ground_cooldown_check = (*unit.0).ground_cooldown == 0;
    let air_cooldown_check = (*unit.0).air_cooldown == 0;
    match order {
        HARVEST_GAS => {
            return_cargo_order = true;
            harvest_gas_check = unit.order_state() == 0;
            if unit.order_state() == 5 && (*unit.0).order_timer == 0 {
                if let Some(target) = unit.target() {
                    let reduce = upgrades::gas_harvest_reduce(&config, game, unit);
                    let carry = upgrades::gas_harvest_carry(&config, game, unit);
                    let depleted = upgrades::gas_harvest_carry_depleted(&config, game, unit);
                    if reduce.is_some() || carry.is_some() || depleted.is_some() {
                        let reduce = u16::from(reduce.unwrap_or(8));
                        let carry = carry.unwrap_or(8);
                        let depleted = depleted.unwrap_or(2);
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
            if unit.order_state() == 5 && (*unit.0).order_timer == 0 {
                if let Some(target) = unit.target() {
                    let reduce = upgrades::mineral_harvest_reduce(&config, game, unit);
                    let carry = upgrades::mineral_harvest_carry(&config, game, unit);
                    if reduce.is_some() || carry.is_some() {
                        let reduce = u16::from(reduce.unwrap_or(8));
                        let carry = carry.unwrap_or(8);
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
            unload_check = (*unit.0).order_timer == 0;
        }
        _ => (),
    }
    if return_cargo_order {
        if config.return_cargo_softcode {
            return_cargo_softcoded = true;
            let player = unit.player() as usize;
            // BW only searches for resource depots if the player owns at least one of the
            // five hardcoded depots.
            // Harvest gas searches for the depots when deciding where the worker should
            // be spawned when exiting the gas mine.
            // The reset collision order is a highprio order, so it'll be able to
            // execute return cargo order immediately afterwards.
            (*game.0).completed_units_count[unit_id::COMMAND_CENTER.0 as usize][player] += 1;
        }
    }
    orig(u);
    if return_cargo_softcoded {
        let player = unit.player() as usize;
        (*game.0).completed_units_count[unit_id::COMMAND_CENTER.0 as usize][player] -= 1;
    }
    if harvest_gas_check {
        if unit.order_state() == 5 {
            if let Some(new_timer) = upgrades::gas_harvest_time(&config, game, unit) {
                (*unit.0).order_timer = new_timer;
            }
        }
    }
    if harvest_minerals_check {
        if unit.order_state() == 5 {
            if let Some(new_timer) = upgrades::mineral_harvest_time(&config, game, unit) {
                (*unit.0).order_timer = new_timer;
            }
        }
    }
    if unload_check {
        if (*unit.0).order_timer > 0 {
            if let Some(new_time) = upgrades::unload_cooldown(&config, game, unit) {
                (*unit.0).order_timer = new_time;
            }
        }
    }
    if ground_cooldown_check {
        if (*unit.0).ground_cooldown > 0 {
            if let Some(new_time) = upgrades::cooldown(&config, game, unit) {
                (*unit.0).ground_cooldown = new_time;
            }
        }
    }
    if air_cooldown_check {
        if (*unit.0).air_cooldown > 0 {
            if let Some(new_time) = upgrades::cooldown(&config, game, unit) {
                (*unit.0).air_cooldown = new_time;
            }
        }
    }
    if let Some(vars) = mining_override {
        if unit.target() == Some(vars.resource) {
            vars.resource.set_resource_amount(vars.old_amount);
        } else {
            (*unit.0).unit_specific[0xf] = vars.carry;
        }
    }
}

pub unsafe extern fn hidden_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    order_hook(u, orig);
}

pub unsafe extern fn secondary_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    use bw_dat::order::*;

    let unit = Unit(u as *mut bw::Unit);
    let config = config();
    let mut spread_creep_check = false;
    let mut larva_spawn_check = false;
    match unit.secondary_order() {
        TRAIN => {
            if config.zerg_building_training {
                let group_flags =
                    (bw::units_dat()[0x2c].data as *mut u8).offset(unit.id().0 as isize);
                let orig_flags = *group_flags;
                *group_flags &= !0x1;
                orig(u);
                *group_flags = orig_flags;
                return;
            }
        }
        SPREAD_CREEP => {
            spread_creep_check = (*unit.0).unit_specific[0xc] == 0;
            larva_spawn_check = (*unit.0).unit_specific[0xa] == 0;
        }
        SPAWNING_LARVA => {
            larva_spawn_check = (*unit.0).unit_specific[0xa] == 0;
        }
        _ => ()
    }
    orig(u);
    if spread_creep_check {
        let game = Game::get();
        if (*unit.0).unit_specific[0xc] != 0 {
            if let Some(new_timer) = upgrades::creep_spread_time(&config, game, unit) {
                (*unit.0).unit_specific[0xc] = new_timer;
            }
        }
    }
    if larva_spawn_check {
        let game = Game::get();
        if (*unit.0).unit_specific[0xa] != 0 {
            if let Some(new_timer) = upgrades::larva_spawn_time(&config, game, unit) {
                (*unit.0).unit_specific[0xa] = new_timer;
            }
        }
    }
}
