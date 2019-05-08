use std::mem;
use std::ops;
use std::ptr::{NonNull, null_mut};

use libc::c_void;

use winapi::um::heapapi::{GetProcessHeap, HeapFree};
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use bw_dat::{self, DatTable, UnitId};

use crate::bw;
use crate::config;
use crate::order::OrderId;
use crate::{render, render_scr};
use crate::windows;

struct GlobalFunc<T: Copy>(Option<T>);

impl<T: Copy> GlobalFunc<T> {
    fn get(&self) -> T {
        self.0.unwrap()
    }

    fn init(&mut self, val: Option<*mut c_void>, desc: &str) {
        let val = val.unwrap_or_else(|| fatal(&format!("Can't get {}", desc)));
        unsafe {
            assert_eq!(mem::size_of::<T>(), 4);
            let mut typecast_hack: T = mem::uninitialized();
            *(&mut typecast_hack as *mut T as *mut *mut c_void) = val;
            self.0 = Some(typecast_hack);
        }
    }
}

static mut RNG_SEED: GlobalFunc<fn() -> u32> = GlobalFunc(None);
pub fn rng_seed() -> Option<u32> {
    unsafe {
        if let Some(rng) = RNG_SEED.0 {
            Some(rng())
        } else {
            None
        }
    }
}

fn fatal(text: &str) -> ! {
    let msg = format!("This StarCraft version is not supported :(\n({})", text);
    windows::message_box("Mtl", &msg);
    unsafe { TerminateProcess(GetCurrentProcess(), 0x4230daef); }
    unreachable!();
}

static mut GAME: GlobalFunc<fn() -> *mut bw::Game> = GlobalFunc(None);
pub fn game() -> *mut bw::Game {
    unsafe { GAME.get()() }
}

static mut AI_REGIONS: GlobalFunc<fn() -> *mut *mut bw::AiRegion> = GlobalFunc(None);
pub fn ai_regions(player: u32) -> *mut bw::AiRegion {
    assert!(player < 8);
    unsafe { *(AI_REGIONS.get()()).offset(player as isize) }
}

static mut PLAYER_AI: GlobalFunc<fn() -> *mut bw::PlayerAiData> = GlobalFunc(None);
pub fn player_ai(player: u32) -> *mut bw::PlayerAiData {
    assert!(player < 8);
    unsafe { (PLAYER_AI.get()()).offset(player as isize) }
}

static mut FIRST_ACTIVE_UNIT: GlobalFunc<fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}
static mut FIRST_HIDDEN_UNIT: GlobalFunc<fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_hidden_unit() -> *mut bw::Unit {
    unsafe { FIRST_HIDDEN_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}


static mut GET_REGION: GlobalFunc<fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut CHANGE_AI_REGION_STATE: GlobalFunc<fn(*mut bw::AiRegion, u32)> = GlobalFunc(None);
pub fn change_ai_region_state(region: *mut bw::AiRegion, state: u32) {
    unsafe { CHANGE_AI_REGION_STATE.get()(region, state) }
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)
> = GlobalFunc(None);

static mut UNITS_DAT: GlobalFunc<unsafe extern fn() -> *mut DatTable> = GlobalFunc(None);

pub fn units_dat() -> *mut DatTable {
    unsafe { UNITS_DAT.get()() }
}

pub fn issue_order(
    unit: *mut bw::Unit,
    order: OrderId,
    x: u32,
    y: u32,
    target: *mut bw::Unit,
    fow_unit: UnitId,
) {
    assert!(x < 0x10000);
    assert!(y < 0x10000);
    assert!(unit != null_mut());
    unsafe { ISSUE_ORDER.get()(unit, order.0 as u32, x, y, target, fow_unit.0 as u32) }
}

static mut PRINT_TEXT: GlobalFunc<fn(*const u8)> = GlobalFunc(None);
pub fn print_text(msg: *const u8) {
    unsafe {
        if let Some(print) = PRINT_TEXT.0 {
            print(msg);
        }
    }
}

pub struct SamaseBox {
    data: NonNull<u8>,
    len: usize,
}

impl ops::Deref for SamaseBox {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        unsafe {
            ::std::slice::from_raw_parts(self.data.as_ptr(), self.len)
        }
    }
}

impl ops::Drop for SamaseBox {
    fn drop(&mut self) {
        unsafe {
            HeapFree(GetProcessHeap(), 0, self.data.as_ptr() as *mut _);
        }
    }
}

static mut READ_FILE: GlobalFunc<fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc(None);
pub fn read_file(name: &str) -> Option<SamaseBox> {
    // Uh, should work fine
    let cstring = format!("{}\0", name);
    let mut size = 0usize;
    let result = unsafe { READ_FILE.get()(cstring.as_ptr(), &mut size) };
    NonNull::new(result).map(|data| SamaseBox {
        data,
        len: size,
    })
}

// Just using samase_shim's definition so that there isn't duplication/unnecessary mismatches
#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const ::samase_shim::PluginApi) {
    ::init();

    let required_version = 13;
    if (*api).version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            (*api).version, required_version,
        ));
    }

    GAME.init(((*api).game)().map(|x| mem::transmute(x)), "Game object");
    FIRST_ACTIVE_UNIT.init(
        ((*api).first_active_unit)().map(|x| mem::transmute(x)),
        "First active unit",
    );
    FIRST_HIDDEN_UNIT.init(
        ((*api).first_hidden_unit)().map(|x| mem::transmute(x)),
        "First active unit",
    );

    READ_FILE.0 = Some(mem::transmute(((*api).read_file)()));
    UNITS_DAT.init(((*api).dat)(0).map(|x| mem::transmute(x)), "units_dat");
    bw_dat::init_units(units_dat());
    init_config();
    //((*api).hook_on_first_file_access)(init_config);
    let config = config::config();
    let result = ((*api).hook_step_objects)(::frame_hook::frame_hook, 0);
    if result == 0 {
        fatal("Couldn't hook step_objects");
    }
    if config.requires_order_hook() {
        ISSUE_ORDER.init(((*api).issue_order)().map(|x| mem::transmute(x)), "issue_order");
        let result = ((*api).hook_step_order)(::order_hook::order_hook);
        if result == 0 {
            fatal("Couldn't hook step_order");
        }
        let result = ((*api).hook_step_order_hidden)(::order_hook::hidden_order_hook);
        if result == 0 {
            fatal("Couldn't hook step_order_hidden");
        }
    }
    if config.requires_secondary_order_hook() {
        let result = ((*api).hook_step_secondary_order)(::order_hook::secondary_order_hook);
        if result == 0 {
            fatal("Couldn't hook step_secondary_order");
        }
    }
    let result = ((*api).extend_save)("mtl\0".as_ptr(), Some(::save), Some(::load), ::init_game);
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    if let Some(tunit) = read_file("game\\tunit.pcx") {
        if let Err(e) = ::unit_pcolor_fix::init_unit_colors(&tunit) {
            fatal(&format!("Invalid game\\tunit.pcx: {}", e));
        }
    }
    if let Some(tminimap) = read_file("game\\tminimap.pcx") {
        if let Err(e) = ::unit_pcolor_fix::init_minimap_colors(&tminimap) {
            fatal(&format!("Invalid game\\tminimap.pcx: {}", e));
        }
    }

    ((*api).hook_draw_image)(render::draw_image_hook);
    if crate::is_scr() {
        ((*api).hook_renderer)(0, mem::transmute(render_scr::draw_hook as usize));
    }
}

unsafe extern fn init_config() {
    let config_slice = read_file("samase/mtl.ini")
        .unwrap_or_else(|| {
            let msg = "Configuration file samase/mtl.ini not found, exiting";
            windows::message_box("Mtl", msg);
            TerminateProcess(GetCurrentProcess(), 0x42302aef);
            unreachable!();
        });
    let config = config::read_config(&config_slice)
        .unwrap_or_else(|e| {
            use std::fmt::Write;
            let mut msg = String::new();
            for c in e.iter_chain() {
                writeln!(msg, "{}", c).unwrap();
            }
            let msg = format!("Unable to read config:\n{}\nExiting.", msg);
            windows::message_box("Mtl", &msg);
            TerminateProcess(GetCurrentProcess(), 0x42302aef);
            unreachable!();
        });
    config::set_config(config);
}
