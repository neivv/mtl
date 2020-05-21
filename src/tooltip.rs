use std::mem;
use std::sync::atomic::{AtomicUsize, AtomicU8, Ordering};

use bw_dat::dialog::{Control};

use crate::bw;
use crate::samase;

static ORIG_TOOLTIP_DRAW_FUNC: AtomicUsize = AtomicUsize::new(0);
static TEXT_HOOK_MODE: AtomicU8 = AtomicU8::new(0);

pub type TooltipDrawFunc = unsafe extern fn(*mut bw::Control);

unsafe extern fn draw_tooltip(raw_ctrl: *mut bw::Control) {
    let orig: TooltipDrawFunc = mem::transmute(ORIG_TOOLTIP_DRAW_FUNC.load(Ordering::Relaxed));
    let ctrl = Control::new(raw_ctrl);
    let config = crate::config::config();
    if crate::buttons::draw_tooltip_hook(&config, ctrl, orig) {
        return;
    }
    if crate::status_screen::draw_tooltip_hook(&config, ctrl, orig) {
        return;
    }
    orig(raw_ctrl);
}

pub unsafe extern fn draw_graphic_layers_hook(param: u32, orig: unsafe extern fn(u32)) {
    // Always hook tooltip func, then have the hook check if it is being called on a
    // cmdbtn
    let old_draw_tooltip = samase::get_tooltip_draw_func();
    if let Some(old) = old_draw_tooltip {
        ORIG_TOOLTIP_DRAW_FUNC.store(old as usize, Ordering::Relaxed);
        samase::set_tooltip_draw_func(Some(draw_tooltip));
    }
    orig(param);
    if old_draw_tooltip.is_some() {
        samase::set_tooltip_draw_func(old_draw_tooltip);
    }
}

/// Mode is just a preknown constant enum
pub fn set_text_hook_mode(mode: u8) {
    TEXT_HOOK_MODE.store(mode, Ordering::Relaxed);
}

pub unsafe extern fn layout_draw_text_hook(
    a: u32,
    b: u32,
    text: *const u8,
    d: *mut u32,
    draw: u32,
    f: *mut u32,
    g: u32,
    h: u32,
    orig: unsafe extern fn(u32, u32, *const u8, *mut u32, u32, *mut u32, u32, u32) -> *const u8,
) -> *const u8 {
    let mode = TEXT_HOOK_MODE.load(Ordering::Relaxed);
    match mode {
        // Cmdbtn
        1 => {
            let mut buffer = [0u8; 32];
            if crate::buttons::tooltip_text_hook(&mut buffer[..], draw) == true {
                return orig(a, b, buffer.as_ptr(), d, draw, f, g, h);
            }
        }
        // Status screen
        2 => {
            let ptr = crate::status_screen::tooltip_text_hook();
            return orig(a, b, ptr, d, draw, f, g, h);
        }
        _ => (),
    }
    return orig(a, b, text, d, draw, f, g, h);
}
