#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
#[macro_use] extern crate scopeguard;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate whack;

extern crate bw_dat;
extern crate samase_shim;

pub mod mpqdraft;
pub mod samase;

#[macro_use] mod macros;

mod auras;
mod buttons;
mod bw;
mod campaign_hook;
mod config;
mod frame_hook;
mod game;
mod ini;
mod lo;
mod order;
mod order_hook;
mod rally;
mod render;
mod render_scr;
mod rng;
mod selection;
mod status_screen;
mod string_tables;
mod tooltip;
mod unit;
mod unit_search;
mod unit_pcolor_fix;
mod upgrades;
mod windows;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

use libc::c_void;

use bw_dat::{Game, UnitId};

fn init() {
    if cfg!(debug_assertions) {
        let _ = fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!("{}[{}:{}][{}] {}",
                    chrono::Local::now()
                        .format("[%Y-%m-%d][%H:%M:%S]"),
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


lazy_static! {
    static ref PATCHER: Mutex<whack::Patcher> = Mutex::new(whack::Patcher::new());
}
static IS_1161: AtomicBool = AtomicBool::new(false);

fn is_scr() -> bool {
    IS_1161.load(Ordering::Relaxed) == false
}

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

unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    unit::init_save_mapping();
    defer!(unit::clear_save_mapping());
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

unsafe extern fn load(ptr: *const u8, len: usize) -> u32 {
    unit::init_load_mapping();
    defer!(unit::clear_load_mapping());

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

unsafe extern fn init_game() {
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

fn game_map_path(game: Game) -> Option<&'static str> {
    let bytes = unsafe {
        &((**game).map_path)[..]
    };
    // Get the null-terminated subslice
    let bytes = bytes.split(|&x| x == 0).next().unwrap();
    std::str::from_utf8(bytes).ok()
}

fn read_map_file(game: Game, filename: &str) -> Option<samase::SamaseBox> {
    use std::borrow::Cow;
    let filename = unsafe {
        if (**game).campaign_mission == 0 {
            Cow::Borrowed(filename)
        } else {
            let map_path = game_map_path(game)?;
            format!("{}\\{}", map_path, filename).into()
        }
    };
    samase::read_file(&filename)
}

// This hook is at init_units, as that's a point when saves have correct map_path in game.
// During init_game pre hook that path is only valid if there is no save.
unsafe extern fn init_map_specific_dat(init_units: unsafe extern fn()) {
    init_units();
    let game = crate::game::get();
    // NOTE: Ordering must match update_dat_table order.
    // No images.dat since it is loaded on game startup.
    // Could just have code to keep track of if this is using
    // global images.dat or not, and reload global when necessary,
    // but I'm guessing images.dat editing isn't needed.
    static FILES: &[&str] = &[
        "arr\\units.dat",
        "arr\\weapons.dat",
        "arr\\flingy.dat",
        "arr\\upgrades.dat",
        "arr\\techdata.dat",
        "arr\\sprites.dat",
        "arr\\orders.dat",
        "arr\\portdata.dat",
    ];
    let map_path = match game_map_path(game) {
        Some(s) => s,
        None => return,
    };
    if (**game).campaign_mission != 0 {
        let mut buffer = [0u8; 256];
        for (i, file) in FILES.iter().enumerate() {
            use std::io::Write;
            let _ = write!(&mut buffer[..], "{}\\{}", map_path, file);
            if let Some(file) = samase::read_file_u8(&buffer) {
                update_dat_file(i, &file);
            }
        }
    } else {
        let mut buffer = Vec::new();
        let mut mpq = match mpq::Archive::open(map_path) {
            Ok(o) => o,
            Err(e) => {
                error!("Couldn't open map MPQ: {}", e);
                return;
            }
        };
        for (i, filename) in FILES.iter().enumerate() {
            if let Ok(file) = mpq.open_file(filename) {
                buffer.resize(file.size() as usize, 0u8);
                if let Err(e) = file.read(&mut mpq, &mut buffer[..]) {
                    error!("Failed to read {}: {}", filename, e);
                } else {
                    update_dat_file(i, &buffer[..]);
                }
            }
        }
    }

    // Patch units.dat target acquisition range values
    let units_dat = samase::units_dat();
    let target_acquisition_range = units_dat.add(23);
    assert_eq!((*target_acquisition_range).entry_size, 1);
    let acq_range_data = (*target_acquisition_range).data as *mut u8;
    for i in 0..(*target_acquisition_range).entries {
        let turret_id = match UnitId(i as u16).subunit() {
            Some(s) => s,
            None => UnitId(i as u16),
        };
        let mut range = turret_id.target_acquisition_range() as u32;
        if let Some(weapon) = turret_id.ground_weapon() {
            range = range.max(weapon.max_range() / 32);
        }
        if let Some(weapon) = turret_id.air_weapon() {
            range = range.max(weapon.max_range() / 32);
        }
        *acq_range_data.add(i as usize) = range as u8;
    }
}

unsafe fn update_dat_file(index: usize, mut file: &[u8]) {
    let table_fn: unsafe fn() -> _ = match index {
        0 => samase::units_dat,
        1 => samase::weapons_dat,
        2 => samase::flingy_dat,
        3 => samase::upgrades_dat,
        4 => samase::techdata_dat,
        5 => samase::sprites_dat,
        6 => samase::orders_dat,
        7 => samase::portdata_dat,
        _ => return,
    };
    let mut table = table_fn();
    if table.is_null() {
        return;
    }
    while file.len() != 0 {
        let copy_len = (*table).entries as usize * (*table).entry_size as usize;
        assert!(file.len() >= copy_len);
        std::ptr::copy_nonoverlapping(file.as_ptr(), (*table).data as *mut u8, copy_len);
        file = &file[copy_len..];
        table = table.add(1);
    }
}

unsafe extern fn spawn_dialog_hook(
    raw: *mut c_void,
    unk: usize,
    event_handler: *mut c_void,
    orig: unsafe extern fn(*mut c_void, usize, *mut c_void) -> u32,
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

unsafe extern fn play_sound_hook(
    sound: u32,
    volume: f32,
    unit: *mut c_void,
    x: *mut i32,
    y: *mut i32,
    orig: unsafe extern fn(u32, f32, *mut c_void, *mut i32, *mut i32) -> u32,
) -> u32 {
    let sound = {
        let config = config::config();
        config.sound_remaps
            .get(sound as usize)
            .copied()
            .filter(|&x| x != !0)
            .unwrap_or(sound)
    };
    orig(sound, volume, unit, x, y)
}

trait ExprExt {
    type Ret;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> Self::Ret;
}

impl ExprExt for bw_dat::expr::BoolExpr {
    type Ret = bool;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> bool {
        let mut ctx = bw_dat::expr::EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: if self.required_context()
                .contains(bw_dat::expr::RequiredContext::MAP_TILE_FLAGS)
            {
                Some(bw::map_tile_flags())
            } else {
                None
            },
            custom: bw_dat::expr::DefaultEval,
        };
        ctx.eval_bool(self)
    }
}

impl ExprExt for bw_dat::expr::IntExpr {
    type Ret = i32;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> i32 {
        let mut ctx = bw_dat::expr::EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: if self.required_context()
                .contains(bw_dat::expr::RequiredContext::MAP_TILE_FLAGS)
            {
                Some(bw::map_tile_flags())
            } else {
                None
            },
            custom: bw_dat::expr::DefaultEval,
        };
        ctx.eval_int(self)
    }
}
