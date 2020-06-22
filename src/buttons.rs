use std::sync::atomic::{AtomicUsize, AtomicU32, AtomicU16, AtomicU8, Ordering};

use arrayvec::ArrayVec;
use bw_dat::dialog::{Control, Dialog};
use bw_dat::{Game, TechId, Unit, UnitId, UpgradeId};

use crate::bw;
use crate::config::Config;
use crate::string_tables::stat_txt;
use crate::tooltip::{self, TooltipDrawFunc};

static CMDBTN_DIALOG: AtomicUsize = AtomicUsize::new(0);

pub fn cmdbtn_dialog_created(dialog: Dialog) {
    CMDBTN_DIALOG.store(*dialog as usize, Ordering::Relaxed);
}

pub unsafe fn draw_tooltip_hook(
    game: Game,
    config: &Config,
    ctrl: Control,
    orig: TooltipDrawFunc,
) -> bool {
    let cmdbtn_dialog = CMDBTN_DIALOG.load(Ordering::Relaxed) as *mut bw::Dialog;
    if *ctrl.dialog() != cmdbtn_dialog || !config.has_cmdbtn_tooltips() {
        return false;
    }
    let id = ctrl.id();
    // I don't think it's possible to have tooltip func called on anything but the
    // command button controls itself, but being careful when casting to button pointer.
    if id >= 1 && id <= 9 && !ctrl.is_disabled() {
        let ptr = ctrl.user_pointer();
        if !ptr.is_null() {
            let button = ptr as *const bw::Button;
            let string_id = (*button).enabled_string_id;
            if let Some(string) = stat_txt().by_index(string_id) {
                let is_train_string = string.as_bytes().get(1).filter(|&&c| c == 1).is_some();
                if is_train_string && config.cmdbtn_tooltip_half_supply {
                    let train_unit = UnitId((*button).act_var);
                    let dual_birth = train_unit.flags() & 0x400 != 0;
                    if train_unit.supply_cost() & 1 != 0 && !dual_birth {
                        // Override to display .5 supply
                        // Supply text is either 2nd/3rd/4th text line that is being rendered,
                        // depending if the unit takes minerals / gas
                        let mut index = 2;
                        if train_unit.mineral_cost() != 0 {
                            index += 1;
                        }
                        if train_unit.gas_cost() != 0 {
                            index += 1;
                        }
                        tooltip::set_text_hook_mode(1);
                        HALF_SUPPLY_INDEX.store(index, Ordering::Relaxed);
                        HALF_SUPPLY_POS.store(index, Ordering::Relaxed);
                        HALF_SUPPLY_VALUE.store(train_unit.supply_cost() / 2, Ordering::Relaxed);
                    }
                }
                if config.cmdbtn_force_stat_txt_tooltips {
                    let text_count = tooltip_text_count(game, string, button);
                    MAIN_TEXT_STRING.store(string_id, Ordering::Relaxed);
                    MAIN_TEXT_POS.store(1, Ordering::Relaxed);
                    MAIN_TEXT_INDEX.store(text_count, Ordering::Relaxed);
                    tooltip::set_text_hook_mode(1);
                }
            }
        }
    }
    orig(*ctrl);
    tooltip::set_text_hook_mode(0);
    HALF_SUPPLY_POS.store(0, Ordering::Relaxed);
    MAIN_TEXT_POS.store(0, Ordering::Relaxed);
    true
}

unsafe fn tooltip_text_count(game: Game, string: &str, button: *const bw::Button) -> u8 {
    match string.as_bytes().get(1).copied() {
        Some(1) => {
            let train_unit = UnitId((*button).act_var);
            1 + (train_unit.mineral_cost() != 0) as u8 +
                (train_unit.gas_cost() != 0) as u8 +
                (train_unit.supply_cost() != 0) as u8
        }
        Some(2) => {
            let player = bw::client_selection().get(0)
                .and_then(|&x| Unit::from_ptr(x))
                .map(|x| x.player())
                .unwrap_or(0);
            let upgrade = UpgradeId((*button).act_var);
            let needs_minerals = upgrade.mineral_cost() != 0 || (
                upgrade.mineral_factor() != 0 &&
                game.upgrade_level(player, upgrade) != 0
            );
            let needs_gas = upgrade.gas_cost() != 0 || (
                upgrade.gas_factor() != 0 &&
                game.upgrade_level(player, upgrade) != 0
            );
            1 + needs_minerals as u8 + needs_gas as u8
        }
        Some(3) => {
            let tech = TechId((*button).act_var);
            1 + (tech.energy_cost() != 0) as u8
        }
        Some(4) => {
            let tech = TechId((*button).act_var);
            1 + (tech.mineral_cost() != 0) as u8
                + (tech.gas_cost() != 0) as u8
        }
        Some(5) => {
            let unit = UnitId((*button).act_var);
            1 + (unit.mineral_cost() != 0) as u8
                + (unit.gas_cost() != 0) as u8
        }
        None | _ => 1,
    }
}

static HALF_SUPPLY_INDEX: AtomicU8 = AtomicU8::new(0);
static HALF_SUPPLY_POS: AtomicU8 = AtomicU8::new(0);
static HALF_SUPPLY_VALUE: AtomicU32 = AtomicU32::new(0);
static MAIN_TEXT_POS: AtomicU8 = AtomicU8::new(0);
static MAIN_TEXT_INDEX: AtomicU8 = AtomicU8::new(0);
static MAIN_TEXT_STRING: AtomicU16 = AtomicU16::new(0);

// Buffer doesn't need to be 0-terminated
pub fn tooltip_text_hook(buffer: &mut ArrayVec<[u8; 512]>, draw: u32) -> bool {
    use std::io::Write;
    let pos = HALF_SUPPLY_POS.load(Ordering::Relaxed);
    let mut hooked = false;
    if pos != 0 {
        if pos == 1 {
            // The func is called twice for the relevant text pieces, once to layout,
            // second time to actually draw, if not drawing yet reset value back to
            // original value
            let _ = write!(buffer, "{}.5", HALF_SUPPLY_VALUE.load(Ordering::Relaxed));
            if draw == 0 {
                let start_index = HALF_SUPPLY_INDEX.load(Ordering::Relaxed);
                HALF_SUPPLY_POS.store(start_index, Ordering::Relaxed);
            } else {
                HALF_SUPPLY_POS.store(0, Ordering::Relaxed);
            }
            hooked = true;
        } else {
            HALF_SUPPLY_POS.store(pos - 1, Ordering::Relaxed);
        }
    }
    let pos = MAIN_TEXT_POS.load(Ordering::Relaxed);
    if pos != 0 {
        if pos == 1 {
            let string_id = MAIN_TEXT_STRING.load(Ordering::Relaxed);
            let stat_txt = stat_txt();
            let string = stat_txt.by_index(string_id)
                .and_then(|x| x.get(2..))
                .unwrap_or("???");
            let _ = write!(buffer, "{}", string);
            if draw == 0 {
                let start_index = MAIN_TEXT_INDEX.load(Ordering::Relaxed);
                MAIN_TEXT_POS.store(start_index, Ordering::Relaxed);
            } else {
                MAIN_TEXT_POS.store(0, Ordering::Relaxed);
            }
            hooked = true;
        } else {
            MAIN_TEXT_POS.store(pos - 1, Ordering::Relaxed);
        }
    }
    hooked
}
