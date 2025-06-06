#[macro_use] extern crate log;
#[macro_use] extern crate scopeguard;
#[macro_use] extern crate serde_derive;

#[cfg(target_pointer_width = "32")]
#[macro_use] extern crate whack;

#[cfg(target_pointer_width = "32")]
extern crate samase_shim;

#[cfg(target_pointer_width = "32")]
pub mod mpqdraft;
pub mod samase;

#[macro_use] mod macros;

mod auras;
mod buttons;
mod bw;
mod campaign_hook;
mod config;
mod expr;
mod frame_hook;
mod game;
mod ini;
mod lo;
mod map_dat;
mod order;
mod order_hook;
mod rally;
mod render;
mod render_scr;
mod rng;
mod selection;
mod sound_remaps;
mod status_screen;
mod string_tables;
mod tooltip;
mod unit;
mod unit_search;
mod unit_pcolor_fix;
mod upgrades;
mod windows;

#[cfg(target_pointer_width = "32")]
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(target_pointer_width = "32")]
use std::sync::Mutex;

use libc::c_void;

use bw_dat::{Game};

fn init() {
    if cfg!(debug_assertions) {
        let _ = fern::Dispatch::new()
            .format(|out, message, record| {
                let time = windows::get_local_time();
                out.finish(format_args!("[{:04}-{:02}-{:02}:{:02}:{:02}:{:02}][{}:{}][{}] {}",
                    time.wYear,
                    time.wMonth,
                    time.wDay,
                    time.wHour,
                    time.wMinute,
                    time.wSecond,
                    record.file().unwrap_or("???"),
                    record.line().unwrap_or(0),
                    record.level(),
                    message))
            })
            .level(log::LevelFilter::Trace)
            .chain(fern::log_file("mtl.log").unwrap())
            .apply();
    }
    std::panic::set_hook(Box::new(|info| {
        use std::fmt::Write;

        let mut msg = String::with_capacity(256);
        writeln!(msg, "Mtl {} panic", env!("CARGO_PKG_VERSION")).unwrap();
        match info.location() {
            Some(s) => writeln!(msg, "Panic at {}:{}", s.file(), s.line()).unwrap(),
            None => writeln!(msg, "Panic at unknown location").unwrap()
        }
        let payload = info.payload();
        let panic_msg = match payload.downcast_ref::<&str>() {
            Some(s) => s,
            None => match payload.downcast_ref::<String>() {
                Some(s) => &s[..],
                None => "(???)",
            },
        };
        writeln!(msg, "{}", panic_msg).unwrap();
        error!("{}", msg);
        samase::crash_with_message(&msg);
    }));
}


#[cfg(target_pointer_width = "32")]
static PATCHER: Mutex<whack::Patcher> = Mutex::new(whack::Patcher::new());
#[cfg(target_pointer_width = "32")]
static IS_1161: AtomicBool = AtomicBool::new(false);

#[cfg(target_pointer_width = "32")]
fn is_scr() -> bool {
    IS_1161.load(Ordering::Relaxed) == false
}

#[cfg(target_pointer_width = "64")]
fn is_scr() -> bool {
    true
}

#[cfg(target_pointer_width = "32")]
#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            IS_1161.store(true, Ordering::Relaxed);
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());

            let mut active_patcher = crate::PATCHER.lock().unwrap();

            {
                let mut exe = active_patcher.patch_exe(0x00400000);
                bw::init_vars(&mut exe);
                bw::init_funcs(&mut exe);
                exe.hook_opt(bw::create_fow_sprite, frame_hook::check_fow_sprite_creation_desync);
            }

            {
                let mut storm = active_patcher.patch_library("storm", 0x1500_0000);
                bw::storm::init_vars(&mut storm);
            }
            // Check for a storm bug where the codegen does an OOB string read and ends
            // up generating broken code.
            let surface_copy_code_ptr = *bw::storm::surface_copy_code;
            if !surface_copy_code_ptr.is_null() {
                let surface_copy_code = (*surface_copy_code_ptr).code_offsets[0xa0];
                if *surface_copy_code.add(1) != 6 {
                    for i in 0..0xa0 {
                        *surface_copy_code.add(i * 0x10 + 0x1) = 0x6;
                        *surface_copy_code.add(i * 0x10 + 0x9) = 0x7;
                    }
                }
            }
        };
        samase_shim::on_win_main(f);
    }
}

#[derive(Serialize, Deserialize)]
struct SaveData {
    tracked_spells: frame_hook::TrackedSpells,
    upgrade_state_changes: upgrades::UpgradeStateChanges,
    rng: rng::Rng,
    lighting: render::LightingState,
    // Probably not necessary to save, but jsut to take away one potential corner
    // case where saving causes sync to go off
    auras: auras::AuraState,
    extended_unit_fields: unit::ExtendedUnitFields,
}

unsafe extern "C" fn save(set_data: unsafe extern "C" fn(*const u8, usize)) {
    let save = SaveData {
        tracked_spells: frame_hook::tracked_spells(),
        upgrade_state_changes: upgrades::global_state_changes().clone(),
        rng: rng::get().clone(),
        lighting: render::lighting_state().clone(),
        auras: auras::aura_state().clone(),
        extended_unit_fields: unit::extended_field_state().clone(),
    };
    match bincode::serialize(&save) {
        Ok(o) => {
            set_data(o.as_ptr(), o.len());
        }
        Err(e) => {
            error!("Couldn't save game: {}", e);
            bw::print_text(format!("(Mtl) Couldn't save game: {}", e));
        }
    }
}

unsafe extern "C" fn load(ptr: *const u8, len: usize) -> u32 {
    let slice = std::slice::from_raw_parts(ptr, len);
    let data: SaveData = match bincode::deserialize(slice) {
        Ok(o) => o,
        Err(e) => {
            error!("Couldn't load game: {}", e);
            return 0
        }
    };
    frame_hook::set_tracked_spells(data.tracked_spells);
    upgrades::set_state_changes(data.upgrade_state_changes);
    rng::set_rng(data.rng);
    *render::lighting_state() = data.lighting;
    *auras::aura_state() = data.auras;
    *unit::extended_field_state() = data.extended_unit_fields;
    1
}

unsafe extern "C" fn init_game() {
    // NOTE: Config cannot be updated at this point to have map-specific data
    // Earliest is init_units_hook
    string_tables::init();
    let game = crate::game::get();
    order_hook::invalidate_cached_unit_search();
    *render::lighting_state() = render::LightingState::new();

    bw::init_game_start_vars();
    fix_campaign_music(game);
    frame_hook::init_tracked_spells();
    upgrades::init_state_changes();
    rng::set_rng(rng::Rng::default());
    *auras::aura_state() = auras::AuraState::new();
    frame_hook::enable_first_frame_hook();
    *unit::extended_field_state() = unit::ExtendedUnitFields::new();
}

/// Fixes a BW issue where the music was hardcoded to match Blizz campaign races
unsafe fn fix_campaign_music(game: Game) {
    const FIRST_MISSIONS: &[u8] = &[0x1, 0xc, 0x16, 0x20, 0x28, 0x31];
    let mission = (**game).campaign_mission;
    if mission != 0 {
        let starting_map = FIRST_MISSIONS.iter()
            .map(|&x| u16::from(x))
            .find(|&first| first <= mission)
            .unwrap_or(0);
        let music_id = if is_scr() {
            let song = (mission - starting_map) % 4;
            match (**game).player_race {
                0 => 1 + song,
                1 => 5 + song,
                2 | _ => 9 + song,
            }
        } else {
            let song = (mission - starting_map) % 3;
            match (**game).player_race {
                0 => 1 + song,
                1 => 4 + song,
                2 | _ => 7 + song,
            }
        };
        (**game).bgm_song = music_id;
    }
}

// This hook is at init_units, as that's a point when saves have correct map_path in game.
// During init_game pre hook that path is only valid if there is no save.
unsafe extern "C" fn init_map_specific_dat(init_units: unsafe extern "C" fn()) {
    init_units();
    crate::samase::init_config(false, true);
    let config = config::config();
    if !config.map_dat_files.enable {
        return;
    }
    map_dat::load(&config.map_dat_files);
}

unsafe extern "C" fn spawn_dialog_hook(
    raw: *mut c_void,
    unk: usize,
    event_handler: *mut c_void,
    orig: unsafe extern "C" fn(*mut c_void, usize, *mut c_void) -> u32,
) -> u32 {
    let result = orig(raw, unk, event_handler);
    let dialog = bw_dat::dialog::Dialog::new(raw as *mut bw::Dialog);
    let ctrl = dialog.as_control();
    let name = ctrl.string();
    if name == "Minimap" {
        rally::minimap_dialog_created(dialog);
    } else if name == "StatBtn" {
        buttons::cmdbtn_dialog_created(dialog);
    } else if name == "StatData" {
        status_screen::status_screen_dialog_created(dialog);
    }
    result
}

pub unsafe extern "C" fn run_dialog_hook(
    raw: *mut c_void,
    unk: usize,
    event_handler: *mut c_void,
    orig: unsafe extern "C" fn(*mut c_void, usize, *mut c_void) -> u32,
) -> u32 {
    let dialog = bw_dat::dialog::Dialog::new(raw as *mut bw::Dialog);
    let ctrl = dialog.as_control();
    let name = ctrl.string();
    if name == "RaceSelection" {
        if config::campaign().is_some() {
            return campaign_hook::run_dialog_hook(raw, unk, event_handler, orig);
        }
    } else if name == "Delete" {
        if config::config().always_bw {
            let area = (**ctrl).area;
            let seems_glu_game_mode = area.left == 0 && area.top == 0 && area.right == 359 &&
                area.bottom == 199 && dialog.children().map(|x| x.id()).eq([6, 2, 7, 3]);
            if seems_glu_game_mode {
                // Init event handler, send 2 init events and then a delete event to have the
                // dialog be properly cleaned up, then return bw button id
                match is_scr() {
                    false => {
                        (**ctrl).event_handler = Some(std::mem::transmute(event_handler));
                    }
                    true => {
                        (*(*ctrl as *mut bw::scr::Control)).event_handler =
                            Some(std::mem::transmute(event_handler));
                    }
                };
                ctrl.send_ext_event(7);
                ctrl.send_ext_event(0);
                ctrl.send_ext_event(1);
                return 7;
            }
        }
    }
    orig(raw, unk, event_handler)
}

unsafe extern "C" fn play_sound_hook(
    sound: u32,
    volume: f32,
    unit: *mut c_void,
    x: *mut i32,
    y: *mut i32,
    orig: unsafe extern "C" fn(u32, f32, *mut c_void, *mut i32, *mut i32) -> u32,
) -> u32 {
    let sound = {
        let config = config::config();
        let unit = bw_dat::Unit::from_ptr(unit as *mut bw::Unit);
        config.sound_remaps.remap(sound, unit)
    };
    if sound != 0 {
        orig(sound, volume, unit, x, y)
    } else {
        0
    }
}
