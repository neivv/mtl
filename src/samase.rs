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

    fn try_init(&mut self, val: Option<*mut c_void>) -> bool {
        let val = match val {
            Some(s) => s,
            None => return false,
        };
        unsafe {
            assert_eq!(mem::size_of::<T>(), mem::size_of::<*mut c_void>());
            let mut typecast_hack: mem::MaybeUninit<T> = mem::MaybeUninit::uninit();
            *(typecast_hack.as_mut_ptr() as *mut *mut c_void) = val;
            self.0 = Some(typecast_hack.assume_init());
        }
        true
    }

    fn init(&mut self, val: Option<*mut c_void>, desc: &str) {
        if !self.try_init(val) {
            fatal(&format!("Can't get {}", desc));
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

static mut FIRST_ACTIVE_UNIT: GlobalFunc<fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}
static mut FIRST_HIDDEN_UNIT: GlobalFunc<fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_hidden_unit() -> *mut bw::Unit {
    unsafe { FIRST_HIDDEN_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut AI_UPDATE_ATTACK_TARGET:
    GlobalFunc<fn(*mut bw::Unit, u32, u32, u32) -> u32> = GlobalFunc(None);
pub unsafe fn ai_update_attack_target(unit: *mut bw::Unit, a1: u32, a2: u32, a3: u32) -> u32 {
    AI_UPDATE_ATTACK_TARGET.get()(unit, a1, a2, a3)
}

static mut UPDATE_VISIBILITY_POINT: GlobalFunc<fn(*mut bw::LoneSprite)> = GlobalFunc(None);
pub unsafe fn update_visibility_point(sprite: *mut bw::LoneSprite) {
    UPDATE_VISIBILITY_POINT.get()(sprite)
}

static mut CREATE_LONE_SPRITE:
    GlobalFunc<fn(u32, i32, i32, u32) -> *mut bw::LoneSprite> = GlobalFunc(None);
pub unsafe fn create_lone_sprite(id: u32, x: i32, y: i32, player: u32) -> *mut bw::LoneSprite {
    CREATE_LONE_SPRITE.get()(id, x, y, player)
}

static mut GET_ISCRIPT_BIN: GlobalFunc<fn() -> *const u8> = GlobalFunc(None);
pub unsafe fn get_iscript_bin() -> *const u8 {
    GET_ISCRIPT_BIN.get()()
}

static mut STEP_ISCRIPT_FRAME:
    GlobalFunc<fn(*mut bw::Image, *mut bw::Iscript, u32, *mut u32)> = GlobalFunc(None);
pub unsafe fn step_iscript_frame(
    image: *mut bw::Image,
    iscript: *mut bw::Iscript,
    dry_run: u32,
    speed_out: *mut u32,
) {
    STEP_ISCRIPT_FRAME.get()(image, iscript, dry_run, speed_out);
}

static mut PLAYERS: GlobalFunc<fn() -> *mut bw::Player> = GlobalFunc(None);
pub unsafe fn players() -> *mut bw::Player {
    PLAYERS.get()()
}

static mut PATHING: GlobalFunc<fn() -> *mut bw::Pathing> = GlobalFunc(None);
pub unsafe fn pathing() -> *mut bw::Pathing {
    PATHING.get()()
}

static mut GET_REGION: GlobalFunc<fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)
> = GlobalFunc(None);

static mut UNITS_DAT: GlobalFunc<unsafe extern fn() -> *mut DatTable> = GlobalFunc(None);
pub fn units_dat() -> *mut DatTable {
    unsafe { UNITS_DAT.get()() }
}

static mut WEAPONS_DAT: GlobalFunc<fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn weapons_dat() -> *mut bw_dat::DatTable {
    unsafe { WEAPONS_DAT.get()() }
}

static mut UPGRADES_DAT: GlobalFunc<fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn upgrades_dat() -> *mut bw_dat::DatTable {
    unsafe { UPGRADES_DAT.get()() }
}

static mut TECHDATA_DAT: GlobalFunc<fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn techdata_dat() -> *mut bw_dat::DatTable {
    unsafe { TECHDATA_DAT.get()() }
}

static mut ORDERS_DAT: GlobalFunc<fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn orders_dat() -> *mut bw_dat::DatTable {
    unsafe { ORDERS_DAT.get()() }
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

unsafe impl Send for SamaseBox {}

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
    crate::init();

    let required_version = 18;
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
    WEAPONS_DAT.init(((*api).dat)(1).map(|x| mem::transmute(x)), "weapons.dat");
    bw_dat::init_weapons(weapons_dat());
    UPGRADES_DAT.init(((*api).dat)(3).map(|x| mem::transmute(x)), "upgrades.dat");
    bw_dat::init_upgrades(upgrades_dat());
    TECHDATA_DAT.init(((*api).dat)(4).map(|x| mem::transmute(x)), "techdata.dat");
    bw_dat::init_techdata(techdata_dat());
    ORDERS_DAT.init(((*api).dat)(7).map(|x| mem::transmute(x)), "orders.dat");
    bw_dat::init_orders(orders_dat());
    init_config(true);
    //((*api).hook_on_first_file_access)(init_config);
    let config = config::config();
    let result = ((*api).hook_step_objects)(crate::frame_hook::frame_hook, 0);
    if result == 0 {
        fatal("Couldn't hook step_objects");
    }
    if config.requires_order_hook() {
        ISSUE_ORDER.init(((*api).issue_order)().map(|x| mem::transmute(x)), "issue_order");
        let result = ((*api).hook_step_order)(crate::order_hook::order_hook);
        if result == 0 {
            fatal("Couldn't hook step_order");
        }
        let result = ((*api).hook_step_order_hidden)(crate::order_hook::hidden_order_hook);
        if result == 0 {
            fatal("Couldn't hook step_order_hidden");
        }
    }
    if config.requires_secondary_order_hook() {
        let result = ((*api).hook_step_secondary_order)(crate::order_hook::secondary_order_hook);
        if result == 0 {
            fatal("Couldn't hook step_secondary_order");
        }
    }
    let result = ((*api).extend_save)("mtl\0".as_ptr(), Some(crate::save), Some(crate::load), crate::init_game);
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    AI_UPDATE_ATTACK_TARGET.0 = Some(mem::transmute(((*api).ai_update_attack_target)()));
    UPDATE_VISIBILITY_POINT.0 = Some(mem::transmute(((*api).update_visibility_point)()));
    CREATE_LONE_SPRITE.0 = Some(mem::transmute(((*api).create_lone_sprite)()));
    GET_ISCRIPT_BIN.0 = Some(mem::transmute(((*api).get_iscript_bin)()));
    STEP_ISCRIPT_FRAME.0 = Some(mem::transmute(((*api).step_iscript)()));
    PLAYERS.0 = Some(mem::transmute(((*api).players)()));
    PATHING.0 = Some(mem::transmute(((*api).pathing)()));
    if let Some(tunit) = read_file("game\\tunit.pcx") {
        if let Err(e) = crate::unit_pcolor_fix::init_unit_colors(&tunit) {
            fatal(&format!("Invalid game\\tunit.pcx: {}", e));
        }
    }
    if let Some(tminimap) = read_file("game\\tminimap.pcx") {
        if let Err(e) = crate::unit_pcolor_fix::init_minimap_colors(&tminimap) {
            fatal(&format!("Invalid game\\tminimap.pcx: {}", e));
        }
    }

    ((*api).hook_draw_image)(render::draw_image_hook);
    if crate::is_scr() {
        ((*api).hook_renderer)(0, mem::transmute(render_scr::draw_hook as usize));
    }

    if let Some(campaign) = config::campaign() {
        let ok = ((*api).set_campaigns)(campaign.campaigns.as_ptr() as *const *mut c_void);
        if ok == 0 {
            ((*api).warn_unsupported_feature)(b"Campaigns\0".as_ptr());
        }
        // Ignore failure, used for campaign briefing races which shouldn't be critical
        ((*api).hook_run_dialog)(crate::campaign_hook::run_dialog_hook);
    }
}

pub unsafe extern fn init_config(exit_on_error: bool) {
    loop {
        match read_config() {
            Ok(o) => {
                config::set_config(o);
                break;
            }
            Err(msg) => {
                windows::message_box("Mtl", &msg);
                if exit_on_error {
                    TerminateProcess(GetCurrentProcess(), 0x42302aef);
                }
            }
        }
    }
    // Only load campaigns on init as they can't be patched afterwards (ithink?)
    if exit_on_error {
        loop {
            match read_campaign_ini() {
                Ok(o) => {
                    if let Some(campaign) = o {
                        config::set_campaign_ini(campaign);
                    }
                    break;
                }
                Err(msg) => {
                    windows::message_box("Mtl", &msg);
                    if exit_on_error {
                        TerminateProcess(GetCurrentProcess(), 0x42302aef);
                    }
                }
            }
        }
    }
}

fn read_config() -> Result<config::Config, String> {
    let config_slice = match read_file("samase/mtl.ini") {
        Some(s) => s,
        None => return Err(format!("Configuration file samase/mtl.ini not found.")),
    };
    match config::read_config(&config_slice) {
        Ok(o) => Ok(o),
        Err(e) => {
            use std::fmt::Write;
            let mut msg = String::new();
            for c in e.iter_chain() {
                writeln!(msg, "{}", c).unwrap();
            }
            let msg = format!("Unable to read config:\n{}", msg);
            Err(msg)
        }
    }
}

fn read_campaign_ini() -> Result<Option<config::Campaign>, String> {
    let config_slice = match read_file("samase/mtl_campaign.ini") {
        Some(s) => s,
        None => return Ok(None),
    };
    match config::read_campaign(&config_slice) {
        Ok(o) => Ok(Some(o)),
        Err(e) => {
            use std::fmt::Write;
            let mut msg = String::new();
            for c in e.iter_chain() {
                writeln!(msg, "{}", c).unwrap();
            }
            let msg = format!("Unable to read campaign ini:\n{}", msg);
            Err(msg)
        }
    }
}
