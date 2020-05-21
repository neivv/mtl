use std::sync::atomic::{AtomicUsize, AtomicU32, AtomicU8, Ordering};

use bw_dat::dialog::{Control, Dialog};
use bw_dat::UnitId;

use crate::bw;
use crate::config::Config;
use crate::string_tables::stat_txt;
use crate::tooltip::{self, TooltipDrawFunc};

static CMDBTN_DIALOG: AtomicUsize = AtomicUsize::new(0);

pub fn cmdbtn_dialog_created(dialog: Dialog) {
    CMDBTN_DIALOG.store(*dialog as usize, Ordering::Relaxed);
}

pub unsafe fn draw_tooltip_hook(config: &Config, ctrl: Control, orig: TooltipDrawFunc) -> bool {
    let cmdbtn_dialog = CMDBTN_DIALOG.load(Ordering::Relaxed) as *mut bw::Dialog;
    if *ctrl.dialog() != cmdbtn_dialog || !config.cmdbtn_tooltip_half_supply {
        return false;
    }
    let id = ctrl.id();
    // I don't think it's possible to have tooltip func called on anything but the
    // command button controls itself, but being careful when casting to button pointer.
    if id >= 1 && id <= 9 && !ctrl.is_disabled() {
        let ptr = ctrl.user_pointer();
        if !ptr.is_null() {
            let button = ptr as *const bw::Button;
            if let Some(string) = stat_txt().by_index((*button).enabled_string_id) {
                let is_train_string = string.as_bytes().get(1).filter(|&&c| c == 1).is_some();
                if is_train_string {
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
            }
        }
    }
    orig(*ctrl);
    tooltip::set_text_hook_mode(0);
    HALF_SUPPLY_POS.store(0, Ordering::Relaxed);
    true
}

static HALF_SUPPLY_INDEX: AtomicU8 = AtomicU8::new(0);
static HALF_SUPPLY_POS: AtomicU8 = AtomicU8::new(0);
static HALF_SUPPLY_VALUE: AtomicU32 = AtomicU32::new(0);

pub fn tooltip_text_hook(buffer: &mut [u8], draw: u32) -> bool {
    use std::io::Write;
    let pos = HALF_SUPPLY_POS.load(Ordering::Relaxed);
    if pos != 0 {
        if pos == 1 {
            // The func is called twice for the relevant text pieces, once to layout,
            // second time to actually draw, if not drawing yet reset value back to
            // original value
            let _ = write!(&mut buffer[..], "{}.5", HALF_SUPPLY_VALUE.load(Ordering::Relaxed));
            buffer[buffer.len() - 1] = 0;
            if draw == 0 {
                let start_index = HALF_SUPPLY_INDEX.load(Ordering::Relaxed);
                HALF_SUPPLY_POS.store(start_index, Ordering::Relaxed);
            } else {
                HALF_SUPPLY_POS.store(0, Ordering::Relaxed);
            }
            return true;
        } else {
            HALF_SUPPLY_POS.store(pos - 1, Ordering::Relaxed);
        }
    }
    false
}
