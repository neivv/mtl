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
mod unit;
mod unit_pcolor_fix;
mod upgrades;
mod windows;

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


#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());
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
    bw::init_game_start_vars();
    frame_hook::init_tracked_spells();
    upgrades::init_state_changes();
}
