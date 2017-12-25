use libc::c_void;

use bw;
use config::config;
use unit::{self, Unit};

pub unsafe extern fn order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    use order::id::*;

    let unit = Unit(u as *mut bw::Unit);
    match unit.order() {
        HARVEST_GAS | RETURN_MINERALS | RETURN_GAS | RESET_COLLISION_HARVESTER => {
            let config = config();
            if config.return_cargo_softcode {
                let game = bw::game();
                let player = unit.player() as usize;
                // BW only searches for resource depots if the player owns at least one of the
                // five hardcoded depots.
                // Harvest gas searches for the depots when deciding where the worker should
                // be spawned when exiting the gas mine.
                // The reset collision order is a highprio order, so it'll be able to
                // execute return cargo order immediately afterwards.
                (*game).completed_units_count[unit::id::COMMAND_CENTER.0 as usize][player] += 1;
                orig(u);
                (*game).completed_units_count[unit::id::COMMAND_CENTER.0 as usize][player] -= 1;
            } else {
                orig(u);
            }
        }
        _ => orig(u),
    }
}

pub unsafe extern fn hidden_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    order_hook(u, orig);
}
