#[cfg(debug_assertions)] extern crate backtrace;
extern crate bincode;
extern crate byteorder;
extern crate chrono;
#[macro_use] extern crate combine;
#[macro_use] extern crate failure;
extern crate fern;
#[macro_use] extern crate lazy_static;
extern crate libc;
#[macro_use] extern crate log;
extern crate pcx;
#[macro_use] extern crate scopeguard;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate smallvec;
extern crate thread_local;
extern crate vec_map;
extern crate winapi;
#[macro_use] extern crate whack;

extern crate bw_dat;
extern crate samase_shim;

pub mod mpqdraft;
pub mod samase;

#[macro_use] mod macros;

mod bw;
mod config;
mod frame_hook;
mod game;
mod ini;
mod order;
mod order_hook;
mod parse_expr;
mod render;
mod render_scr;
mod unit;
mod unit_pcolor_fix;
mod upgrades;
mod windows;

use std::sync::atomic::{AtomicBool, Ordering};
use crate::game::Game;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

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

        #[cfg(debug_assertions)]
        fn backtrace() -> String {
            use std::path::Path;

            let mut backtrace = String::new();
            backtrace::trace(|frame| {
                let ip = frame.ip();
                let symbol_address = frame.symbol_address();

                backtrace::resolve(ip, |symbol| {
                    let mut line = format!("    {:p}", symbol_address);
                    if symbol_address != ip {
                        write!(line, " ({:p})", symbol_address).unwrap();
                    }
                    let module = windows::module_from_address(symbol_address as *mut _);
                    if let Some((name, base)) = module {
                        if let Some(fname) = Path::new(&name).file_name() {
                            write!(line, " {:?} {:p}", fname, base).unwrap();
                        } else {
                            write!(line, " {:?} {:p}", name, base).unwrap();
                        }
                    }
                    if let Some(name) = symbol.name() {
                        write!(line, " -- {}", name).unwrap();
                    }
                    if let Some(filename) = symbol.filename() {
                        if let Some(lineno) = symbol.lineno() {
                            write!(line, " -- {:?}:{}", filename, lineno).unwrap();
                        } else {
                            write!(line, " -- {:?}:???", filename).unwrap();
                        }
                    }
                    writeln!(backtrace, "{}", line).unwrap();
                });
                true // keep going to the next frame
            });
            backtrace
        }

        #[cfg(not(debug_assertions))]
        fn backtrace() -> String {
            "".into()
        }

        let mut msg = String::new();
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
        if cfg!(debug_assertions) {
            write!(msg, "Backtrace:\n{}", backtrace()).unwrap();
        }
        error!("{}", msg);
        windows::message_box("Mtl panic", &msg);
        unsafe { TerminateProcess(GetCurrentProcess(), 0x4230daef); }
    }));
}


lazy_static! {
    static ref PATCHER: whack::Patcher = whack::Patcher::new();
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
}

unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    unit::init_save_mapping();
    defer!(unit::clear_save_mapping());
    let save = SaveData {
        tracked_spells: frame_hook::tracked_spells(),
        upgrade_state_changes: upgrades::global_state_changes().clone(),
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
    1
}

unsafe extern fn init_game() {
    samase::init_config(false);

    let game = Game::get();
    bw::init_game_start_vars();
    fix_campaign_music(game);
    frame_hook::init_tracked_spells();
    upgrades::init_state_changes();
}

/// Fixes a BW issue where the music was hardcoded to match Blizz campaign races
unsafe fn fix_campaign_music(game: Game) {
    const FIRST_MISSIONS: &[u8] = &[0x1, 0xc, 0x16, 0x20, 0x28, 0x31];
    let mission = (*game.0).campaign_mission;
    if mission != 0 {
        let starting_map = FIRST_MISSIONS.iter()
            .map(|&x| u16::from(x))
            .find(|&first| first <= mission)
            .unwrap_or(0);
        let song = (mission - starting_map) % 3;
        let music_id = match (*game.0).player_race {
            0 => 1 + song,
            1 => 4 + song,
            2 | _ => 7 + song,
        };
        (*game.0).bgm_song = music_id;
    }
}
