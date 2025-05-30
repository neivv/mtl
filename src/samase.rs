use std::mem;
use std::ops;
use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, AtomicU8, Ordering};

use libc::c_void;

use winapi::um::heapapi::{GetProcessHeap, HeapFree};
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use samase_plugin::{FfiStr, FuncId, VarId};
use bw_dat::{DatTable, ImageId, UnitId};

use crate::bw;
use crate::config;
use crate::order::OrderId;
use crate::{render, render_scr};
use crate::windows;

struct GlobalFunc<T: Copy>(AtomicUsize, std::marker::PhantomData<T>);

impl<T: Copy> GlobalFunc<T> {
    const fn new() -> GlobalFunc<T> {
        GlobalFunc(AtomicUsize::new(0), std::marker::PhantomData)
    }

    fn set(&self, val: T) {
        unsafe {
            self.0.store(mem::transmute_copy(&val), Ordering::Relaxed);
        }
    }

    fn set_required<U>(&self, val: Option<U>) {
        assert!(size_of::<U>() == size_of::<T>());
        if let Some(val) = val {
            unsafe {
                self.0.store(mem::transmute_copy(&val), Ordering::Relaxed);
            }
        } else {
            panic!("Required function not available");
        }
    }

    fn get(&self) -> T {
        unsafe {
            let value = self.0.load(Ordering::Relaxed);
            debug_assert!(value != 0);
            mem::transmute_copy(&value)
        }
    }

    fn get_opt(&self) -> Option<T> {
        unsafe {
            mem::transmute_copy::<usize, Option<T>>(&self.0.load(Ordering::Relaxed))
        }
    }

    fn has_value(&self) -> bool {
        self.get_opt().is_some()
    }
}

fn fatal(text: &str) -> ! {
    let msg = format!("This StarCraft version is not supported :(\n({})", text);
    windows::message_box("Mtl", &msg);
    unsafe { TerminateProcess(GetCurrentProcess(), 0x4230daef); }
    unreachable!();
}

const GLOBALS: &[VarId] = &[
    VarId::Game,
    VarId::FirstActiveUnit,
    VarId::FirstHiddenUnit,
    VarId::FirstPlayerUnit,
    VarId::IscriptBin,
    VarId::Players,
    VarId::MapTileFlags,
    VarId::Pathing,
    VarId::IsReplay,
    VarId::ClientSelection,
    VarId::Selections,
    VarId::LocalPlayerId,
    VarId::Zoom,
    VarId::ScreenX,
    VarId::ScreenY,
    VarId::FirstLoneSprite,
    VarId::SpriteHlines,
    VarId::SpriteHlinesEnd,
    VarId::IsPaused,
    VarId::IsTargeting,
    VarId::IsPlacingBuilding,
    // Writable
    VarId::DrawCursorMarker,
    // Optional
    VarId::RngSeed,
    // Writable + Optional
    VarId::TooltipDrawFunc,
];

const fn global_idx(var: VarId) -> usize {
    let mut i = 0;
    loop {
        if GLOBALS[i] as u16 == var as u16 {
            return i;
        }
        i += 1;
    }
}

const FIRST_WRITABLE_GLOBAL: usize = global_idx(VarId::DrawCursorMarker);
const FIRST_OPTIONAL_GLOBAL: usize = global_idx(VarId::RngSeed);

const ZERO_U8: AtomicU8 = AtomicU8::new(0);
static OPT_GLOBALS: [AtomicU8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL] = [
    ZERO_U8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL
];

static CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern "C" fn(*const u8) -> !> = GlobalFunc::new();
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

static CREATE_EXT_UNIT_FIELD:
    GlobalFunc<unsafe extern "C" fn(*const FfiStr) -> u32> = GlobalFunc::new();
static READ_EXT_UNIT_FIELD: GlobalFunc<unsafe extern "C" fn(u32, u32) -> u32> = GlobalFunc::new();

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct ExtFieldId(pub u32);

pub fn create_extended_unit_field(name: &[u8]) -> ExtFieldId {
    unsafe {
        let name_ffi = FfiStr::from_bytes(name);
        let result = CREATE_EXT_UNIT_FIELD.get_opt().expect("Need newer samase")(&name_ffi);
        if result == 0 {
            panic!("Failed to init ext unit field {}", String::from_utf8_lossy(name));
        }
        ExtFieldId(result)
    }
}

pub fn read_extended_unit_field(unit_index: u32, id: ExtFieldId) -> u32 {
    unsafe {
        READ_EXT_UNIT_FIELD.get()(unit_index, id.0)
    }
}

unsafe fn init_globals(api: *const samase_plugin::PluginApi) {
    let mut result = [0u8; GLOBALS.len()];
    ((*api).load_vars)(GLOBALS.as_ptr() as *const u16, result.as_mut_ptr(), GLOBALS.len());
    let mut i = 0;
    for (last, needed) in [(FIRST_WRITABLE_GLOBAL, 2), (FIRST_OPTIONAL_GLOBAL, 3)] {
        while i < last {
            if result[i] < needed {
                if result[i] == 1 {
                    fatal(
                        &format!("Newer samase is required (Failed to get variable {:?})", GLOBALS[i])
                    );
                } else {
                    fatal(&format!("Failed to get variable {:?}", GLOBALS[i]));
                }
            }
            i += 1;
        }
    }
    while i < GLOBALS.len() {
        OPT_GLOBALS[i - FIRST_OPTIONAL_GLOBAL].store(result[i], Ordering::Relaxed);
        i += 1;
    }
}

static READ_VARS:
    GlobalFunc<unsafe extern "C" fn(*const u16, *mut usize, usize)> = GlobalFunc::new();
fn read_var(var: VarId) -> usize {
    unsafe {
        let var = var as u16;
        let mut out = 0usize;
        READ_VARS.get()(&var, &mut out, 1);
        out
    }
}

static WRITE_VARS:
    GlobalFunc<unsafe extern "C" fn(*const u16, *const usize, usize)> = GlobalFunc::new();
fn write_var(var: VarId, value: usize) {
    unsafe {
        let var = var as u16;
        WRITE_VARS.get()(&var, &value, 1);
    }
}

/// Returns result from samase api (0/1 = bad, 2 = read-only, 3 = read/write)
macro_rules! opt_global {
    ($id:expr) => {{
        const IDX: usize = global_idx($id) - FIRST_OPTIONAL_GLOBAL;
        OPT_GLOBALS[IDX].load(Ordering::Relaxed)
    }}
}

pub fn rng_seed() -> Option<u32> {
    if opt_global!(VarId::RngSeed) >= 2 {
        Some(read_var(VarId::RngSeed) as u32)
    } else {
        None
    }
}

pub fn game() -> *mut bw::Game {
    read_var(VarId::Game) as _
}

pub fn first_active_unit() -> *mut bw::Unit {
    read_var(VarId::FirstActiveUnit) as _
}

pub fn first_hidden_unit() -> *mut bw::Unit {
    read_var(VarId::FirstHiddenUnit) as _
}

pub fn first_player_unit() -> *mut *mut bw::Unit {
    read_var(VarId::FirstPlayerUnit) as _
}

pub fn get_iscript_bin() -> *mut u8 {
    read_var(VarId::IscriptBin) as _
}

pub fn players() -> *mut bw::Player {
    read_var(VarId::Players) as _
}

pub fn map_tile_flags() -> *mut u32 {
    read_var(VarId::MapTileFlags) as _
}

pub fn pathing() -> *mut bw::Pathing {
    read_var(VarId::Pathing) as _
}

pub fn is_replay() -> u32 {
    read_var(VarId::IsReplay) as _
}

pub fn client_selection() -> *mut *mut bw::Unit {
    read_var(VarId::ClientSelection) as _
}

pub fn local_player_id() -> u32 {
    read_var(VarId::LocalPlayerId) as _
}

pub fn ui_scale() -> f32 {
    f32::from_bits(read_var(VarId::Zoom) as u32)
}

pub unsafe fn screen_pos(x: *mut i32, y: *mut i32) {
    let vars = [VarId::ScreenX as u16, VarId::ScreenY as u16];
    let mut result = [0usize, 0];
    READ_VARS.get()(vars.as_ptr(), result.as_mut_ptr(), 2);
    *x = result[0] as i32;
    *y = result[1] as i32;
}

pub fn first_lone_sprite() -> *mut bw::LoneSprite {
    read_var(VarId::FirstLoneSprite) as _
}

pub fn sprite_hlines() -> *mut *mut bw::Sprite {
    read_var(VarId::SpriteHlines) as _
}

pub fn sprite_hlines_end() -> *mut *mut bw::Sprite {
    read_var(VarId::SpriteHlinesEnd) as _
}

pub fn selections() -> *mut *mut bw::Unit {
    read_var(VarId::Selections) as _
}

pub fn draw_cursor_marker(draw: u32) {
    write_var(VarId::DrawCursorMarker, draw as usize);
}

pub fn misc_ui_state(out: &mut [u8]) {
    let vars = [VarId::IsPaused, VarId::IsTargeting, VarId::IsPlacingBuilding];
    let mut result = [0usize, 0, 0];
    unsafe {
        READ_VARS.get()(vars.as_ptr() as *const u16, result.as_mut_ptr(), 3);
    }
    out[0] = result[0] as u8;
    out[1] = result[1] as u8;
    out[2] = result[2] as u8;
}

pub fn get_tooltip_draw_func() -> Option<unsafe extern "C" fn(*mut bw::Control)> {
    if opt_global!(VarId::TooltipDrawFunc) >= 2 {
        unsafe { Some(mem::transmute(read_var(VarId::TooltipDrawFunc))) }
    } else {
        None
    }
}

pub fn set_tooltip_draw_func(new: Option<unsafe extern "C" fn(*mut bw::Control)>) {
    if opt_global!(VarId::TooltipDrawFunc) >= 3 {
        unsafe { write_var(VarId::TooltipDrawFunc, mem::transmute(new)); }
    }
}

static AI_UPDATE_ATTACK_TARGET:
    GlobalFunc<unsafe extern "C" fn(*mut bw::Unit, u32, u32, u32) -> u32> = GlobalFunc::new();
pub unsafe fn ai_update_attack_target(unit: *mut bw::Unit, a1: u32, a2: u32, a3: u32) -> u32 {
    AI_UPDATE_ATTACK_TARGET.get()(unit, a1, a2, a3)
}

static UPDATE_VISIBILITY_POINT:
    GlobalFunc<unsafe extern "C" fn(*mut bw::LoneSprite)> = GlobalFunc::new();
pub unsafe fn update_visibility_point(sprite: *mut bw::LoneSprite) {
    UPDATE_VISIBILITY_POINT.get()(sprite)
}

static CREATE_LONE_SPRITE:
    GlobalFunc<extern "C" fn(u32, i32, i32, u32) -> *mut bw::LoneSprite> = GlobalFunc::new();
pub unsafe fn create_lone_sprite(id: u32, x: i32, y: i32, player: u32) -> *mut bw::LoneSprite {
    CREATE_LONE_SPRITE.get()(id, x, y, player)
}

static STEP_ISCRIPT_FRAME:
    GlobalFunc<unsafe extern "C" fn(*mut bw::Image, *mut bw::Iscript, u32, *mut u32)> =
    GlobalFunc::new();
pub unsafe fn step_iscript_frame(
    image: *mut bw::Image,
    iscript: *mut bw::Iscript,
    dry_run: u32,
    speed_out: *mut u32,
) {
    STEP_ISCRIPT_FRAME.get()(image, iscript, dry_run, speed_out);
}

static SEND_COMMAND: GlobalFunc<unsafe extern "C" fn(*const u8, u32)> = GlobalFunc::new();
pub unsafe fn send_command(data: &[u8]) {
    SEND_COMMAND.get()(data.as_ptr(), data.len() as u32)
}

static IS_OUTSIDE_GAME_SCREEN:
    GlobalFunc<unsafe extern "C" fn(i32, i32) -> u32> = GlobalFunc::new();
pub unsafe fn is_outside_game_screen(x: i32, y: i32) -> u32 {
    IS_OUTSIDE_GAME_SCREEN.get()(x, y)
}

static UNIT_ARRAY_LEN:
    GlobalFunc<unsafe extern "C" fn(*mut *mut bw::Unit, *mut usize)> = GlobalFunc::new();
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    (ptr, size)
}

static GET_SPRITE_POS: GlobalFunc<unsafe extern "C" fn(*mut bw::Sprite, *mut u16)> = GlobalFunc::new();
pub unsafe fn get_sprite_pos(sprite: *mut bw::Sprite) -> bw::Point {
    let mut ret = bw::Point {
        x: 0,
        y: 0,
    };
    GET_SPRITE_POS.get()(sprite, &mut ret as *mut bw::Point as *mut u16);
    ret
}

static SET_SPRITE_POS: GlobalFunc<unsafe extern "C" fn(*mut bw::Sprite, *const u16)> =
    GlobalFunc::new();
pub unsafe fn set_sprite_pos(sprite: *mut bw::Sprite, val: &bw::Point) {
    SET_SPRITE_POS.get()(sprite, val as *const bw::Point as *const u16);
}

static ADD_OVERLAY_ISCRIPT: GlobalFunc<
    unsafe extern "C" fn(*mut bw::Image, u32, i32, i32, u32),
> = GlobalFunc::new();

pub unsafe fn add_overlay_iscript(
    base_image: *mut bw::Image,
    image: ImageId,
    x: i8,
    y: i8,
    above: bool,
) {
    ADD_OVERLAY_ISCRIPT.get()(base_image, image.0 as u32, x as i32, y as i32, above as u32)
}

static KILL_UNIT: AtomicUsize = AtomicUsize::new(0);
static UNIT_SET_HP: AtomicUsize = AtomicUsize::new(0);
static UNIT_MAX_ENERGY: AtomicUsize = AtomicUsize::new(0);

static FUNCS: &[(&AtomicUsize, FuncId)] = &[
    (&KILL_UNIT, FuncId::KillUnit),
    (&UNIT_SET_HP, FuncId::UnitSetHp),
    (&UNIT_MAX_ENERGY, FuncId::UnitMaxEnergy),
];

#[inline]
fn load_func<T: Copy>(global: &AtomicUsize) -> T {
    let value = global.load(Ordering::Relaxed);
    debug_assert!(value != 0);
    assert!(mem::size_of::<T>() == mem::size_of::<fn()>());
    unsafe {
        mem::transmute_copy(&value)
    }
}

unsafe fn init_funcs(api: *const samase_plugin::PluginApi) {
    let max_func = FUNCS.iter().map(|x| x.1 as u16).max().unwrap_or(0);
    if max_func >= (*api).max_func_id {
        fatal(&format!(
            "Newer samase is required. (Largest function id is {}, this plugin requires {})",
            (*api).max_func_id, max_func,
        ));
    }
    for &(global, func_id) in FUNCS {
        let func = ((*api).get_func)(func_id as u16);
        if let Some(f) = func {
            global.store(f as usize, Ordering::Relaxed);
        } else {
            fatal(&format!("Func {} not found", func_id as u16));
        }
    }
}

pub unsafe fn kill_unit(unit: *mut bw::Unit) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit)>(&KILL_UNIT)(unit)
}

pub unsafe fn unit_set_hp(unit: *mut bw::Unit, value: i32) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit, i32)>(&UNIT_SET_HP)(unit, value)
}

pub unsafe fn unit_max_energy(unit: *mut bw::Unit) -> u32 {
    load_func::<unsafe extern "C" fn(*mut bw::Unit) -> u32>(&UNIT_MAX_ENERGY)(unit)
}

static GET_REGION: GlobalFunc<unsafe extern "C" fn(u32, u32) -> u32> = GlobalFunc::new();
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static ISSUE_ORDER: GlobalFunc<
    unsafe extern "C" fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)
> = GlobalFunc::new();

static UNITS_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn units_dat() -> *mut DatTable {
    UNITS_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static WEAPONS_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn weapons_dat() -> *mut bw_dat::DatTable {
    WEAPONS_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static FLINGY_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn flingy_dat() -> *mut DatTable {
    FLINGY_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static UPGRADES_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn upgrades_dat() -> *mut bw_dat::DatTable {
    UPGRADES_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static TECHDATA_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn techdata_dat() -> *mut bw_dat::DatTable {
    TECHDATA_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static SPRITES_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn sprites_dat() -> *mut DatTable {
    SPRITES_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static ORDERS_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn orders_dat() -> *mut bw_dat::DatTable {
    ORDERS_DAT.load(Ordering::Relaxed) as *mut DatTable
}

static PORTDATA_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn portdata_dat() -> *mut bw_dat::DatTable {
    PORTDATA_DAT.load(Ordering::Relaxed) as *mut DatTable
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

static PRINT_TEXT: GlobalFunc<unsafe extern "C" fn(*const u8)> = GlobalFunc::new();
pub fn print_text(msg: *const u8) {
    unsafe {
        if let Some(print) = PRINT_TEXT.get_opt() {
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

static READ_FILE:
    GlobalFunc<unsafe extern "C" fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc::new();
pub fn read_file(name: &str) -> Option<SamaseBox> {
    read_file_u8(name.as_bytes())
}

pub fn read_file_u8(name: &[u8]) -> Option<SamaseBox> {
    let mut vec = Vec::with_capacity(name.len());
    vec.extend_from_slice(name);
    vec.push(0);
    let mut size = 0usize;
    let result = unsafe { READ_FILE.get()(vec.as_ptr(), &mut size) };
    NonNull::new(result).map(|data| SamaseBox {
        data,
        len: size,
    })
}

static READ_MAP_FILE: GlobalFunc<unsafe extern "C" fn(*const u8, *mut usize) -> *mut u8> =
    GlobalFunc::new();

pub fn read_map_file(name: &str) -> Option<SamaseBox> {
    read_map_file_u8(name.as_bytes())
}

pub fn read_map_file_u8(name: &[u8]) -> Option<SamaseBox> {
    let read = READ_MAP_FILE.get_opt()?;
    let mut vec = Vec::with_capacity(name.len());
    vec.extend_from_slice(name);
    vec.push(0);
    let mut size = 0usize;
    let result = unsafe { read(vec.as_ptr(), &mut size) };
    NonNull::new(result).map(|data| SamaseBox {
        data,
        len: size,
    })
}

fn check_version(api_version: u16, required_version: u16) {
    if api_version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            api_version, required_version,
        ));
    }
}

#[no_mangle]
pub unsafe extern "C" fn samase_plugin_init(api: *const samase_plugin::PluginApi) {
    bw_dat::set_is_scr(crate::is_scr());
    crate::init();

    let required_version = 40;
    let api_version = (*api).version;
    check_version(api_version, required_version);

    let mut ext_arrays = null_mut();
    let ext_arrays_len = ((*api).extended_arrays)(&mut ext_arrays);
    bw_dat::set_extended_arrays(ext_arrays as *mut _, ext_arrays_len);

    CRASH_WITH_MESSAGE.set((*api).crash_with_message);
    if (*api).version >= 43 {
        CREATE_EXT_UNIT_FIELD.set((*api).create_extended_unit_field);
        READ_EXT_UNIT_FIELD.set((*api).read_extended_unit_field);
    }
    READ_VARS.set((*api).read_vars);
    WRITE_VARS.set((*api).write_vars);
    init_globals(api);

    READ_FILE.set(mem::transmute(((*api).read_file)()));
    let mut dat_len = 0usize;
    let units_dat = ((*api).extended_dat)(0).expect("units.dat")(&mut dat_len);
    bw_dat::init_units(units_dat as *const _, dat_len as usize);
    UNITS_DAT.store(units_dat as usize, Ordering::Relaxed);
    let weapons_dat = ((*api).extended_dat)(1).expect("weapons.dat")(&mut dat_len);
    bw_dat::init_weapons(weapons_dat as *const _, dat_len);
    WEAPONS_DAT.store(weapons_dat as usize, Ordering::Relaxed);
    let flingy_dat = ((*api).extended_dat)(2).expect("flingy.dat")(&mut dat_len);
    bw_dat::init_flingy(flingy_dat as *const _, dat_len);
    FLINGY_DAT.store(flingy_dat as usize, Ordering::Relaxed);
    let upgrades_dat = ((*api).extended_dat)(3).expect("upgrades.dat")(&mut dat_len);
    bw_dat::init_upgrades(upgrades_dat as *const _, dat_len);
    UPGRADES_DAT.store(upgrades_dat as usize, Ordering::Relaxed);
    let techdata_dat = ((*api).extended_dat)(4).expect("techdata.dat")(&mut dat_len);
    bw_dat::init_techdata(techdata_dat as *const _, dat_len);
    TECHDATA_DAT.store(techdata_dat as usize, Ordering::Relaxed);
    let sprites_dat = ((*api).extended_dat)(5).expect("sprites.dat")(&mut dat_len);
    bw_dat::init_sprites(sprites_dat as *const _, dat_len);
    SPRITES_DAT.store(sprites_dat as usize, Ordering::Relaxed);
    let orders_dat = ((*api).extended_dat)(7).expect("orders.dat")(&mut dat_len);
    bw_dat::init_orders(orders_dat as *const _, dat_len);
    ORDERS_DAT.store(orders_dat as usize, Ordering::Relaxed);

    init_config(true, false);
    //((*api).hook_on_first_file_access)(init_config);
    let config = config::config();
    let result = ((*api).hook_step_objects)(crate::frame_hook::frame_hook, 0);
    if result == 0 {
        fatal("Couldn't hook step_objects");
    }

    ISSUE_ORDER.set_required(((*api).issue_order)());
    let result = ((*api).hook_step_order)(crate::order_hook::order_hook);
    if result == 0 {
        fatal("Couldn't hook step_order");
    }
    let result = ((*api).hook_step_order_hidden)(crate::order_hook::hidden_order_hook);
    if result == 0 {
        fatal("Couldn't hook step_order_hidden");
    }

    let result = ((*api).hook_step_secondary_order)(crate::order_hook::secondary_order_hook);
    if result == 0 {
        fatal("Couldn't hook step_secondary_order");
    }
    let result = ((*api).hook_step_secondary_order)(crate::order_hook::secondary_order_hook);
    if result == 0 {
        fatal("Couldn't hook step_secondary_order");
    }
    if config.requires_rclick_hook() {
        let result = ((*api).hook_game_screen_rclick)(crate::rally::game_screen_rclick);
        if result == 0 {
            fatal("Couldn't hook game_screen_rclick");
        }
    }
    let result = ((*api).extend_save)("mtl\0".as_ptr(), Some(crate::save), Some(crate::load), crate::init_game);
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    PRINT_TEXT.set_required(((*api).print_text)());
    AI_UPDATE_ATTACK_TARGET.set_required(((*api).ai_update_attack_target)());
    UPDATE_VISIBILITY_POINT.set_required(((*api).update_visibility_point)());
    CREATE_LONE_SPRITE.set_required(((*api).create_lone_sprite)());
    STEP_ISCRIPT_FRAME.set_required(((*api).step_iscript)());
    SEND_COMMAND.set_required(((*api).send_command)());
    IS_OUTSIDE_GAME_SCREEN.set_required(((*api).is_outside_game_screen)());
    UNIT_ARRAY_LEN.set_required(((*api).unit_array_len)());
    GET_SPRITE_POS.set_required(((*api).get_sprite_position)());
    SET_SPRITE_POS.set_required(((*api).set_sprite_position)());
    ADD_OVERLAY_ISCRIPT.set_required(((*api).add_overlay_iscript)());

    init_funcs(api);

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

    let _ = ((*api).hook_ingame_command)(
        0xcc,
        crate::rally::command_handler,
        Some(crate::rally::command_length),
    );
    let _ = ((*api).hook_ingame_command)(0x14, crate::rally::targeted_command_old_hook, None);
    let _ = ((*api).hook_ingame_command)(0x61, crate::rally::targeted_command_new_hook, None);
    ((*api).hook_spawn_dialog)(crate::spawn_dialog_hook);

    ((*api).hook_draw_image)(render::draw_image_hook);
    ((*api).hook_play_sound)(crate::play_sound_hook);
    if crate::is_scr() {
        ((*api).hook_renderer)(0, mem::transmute(render_scr::draw_hook as usize));
    }

    let mut do_run_dialog_hook = config.always_bw;
    if let Some(campaign) = config::campaign() {
        let ok = ((*api).set_campaigns)(campaign.campaigns.as_ptr() as *const *mut c_void);
        if ok == 0 {
            ((*api).warn_unsupported_feature)(b"Campaigns\0".as_ptr());
        }
        do_run_dialog_hook = true;
        // Ignore failure, used for campaign briefing races which shouldn't be critical
    }
    if do_run_dialog_hook {
        ((*api).hook_run_dialog)(crate::run_dialog_hook);
    }
    if !config.dont_override_shaders {
        ((*api).hook_file_read)(b"ShadersGLSL\\\0".as_ptr(), render_scr::gl_shader_hook);
        ((*api).hook_file_read)(b"ShadersHLSL\\\0".as_ptr(), render_scr::d3d_shader_hook);
        prism::override_shaders(api);
    }
    READ_MAP_FILE.set_required(((*api).read_map_file)());
    // Always hook init_units for a good point of loading mtl_map.ini
    let ok = ((*api).hook_init_units)(crate::init_map_specific_dat);
    if ok == 0 {
        ((*api).warn_unsupported_feature)(b"mtl_map.ini\0".as_ptr());
    }
    if ok != 0 && config.map_dat_files.enable {
        let ok = READ_MAP_FILE.has_value();

        if let Some(portdata_dat) = ((*api).extended_dat)(9) {
            let mut len = 0usize;
            PORTDATA_DAT.store(portdata_dat(&mut len) as usize, Ordering::Relaxed);
        }
        if !ok {
            ((*api).warn_unsupported_feature)(b"Map specific dat\0".as_ptr());
        }
    }
    if config.has_cmdbtn_tooltips() || config.has_status_screen_tooltips() {
        let ok = ((*api).hook_layout_draw_text)(crate::tooltip::layout_draw_text_hook);
        if ok != 0 {
            ((*api).hook_draw_graphic_layers)(crate::tooltip::draw_graphic_layers_hook);
        }
    }
}

mod prism {
    #[repr(C)]
    struct ShaderSetEntry {
        format: u8,
        shader_type: u8,
        unk1: u16,
        unk2: u32,
        pointer: *const u8,
        len: u32,
    }

    impl ShaderSetEntry {
        const fn pixel_sm4(wrapper: &[u8]) -> ShaderSetEntry {
            ShaderSetEntry {
                format: 0x0,
                shader_type: 0x6,
                unk1: 0,
                unk2: 0,
                pointer: wrapper.as_ptr(),
                len: wrapper.len() as u32,
            }
        }

        const fn pixel_sm5(wrapper: &[u8]) -> ShaderSetEntry {
            ShaderSetEntry {
                format: 0x4,
                shader_type: 0x6,
                unk1: 0,
                unk2: 0,
                pointer: wrapper.as_ptr(),
                len: wrapper.len() as u32,
            }
        }
    }

    unsafe impl Sync for ShaderSetEntry {}
    unsafe impl Send for ShaderSetEntry {}

    static WATER_BIN: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/water.bin"));
    static WATER_SM4_BIN: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/water.sm4.bin"));
    static WATER_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(WATER_SM4_BIN),
        ShaderSetEntry::pixel_sm5(WATER_BIN),
    ];
    static SPRITE_TILE_BIN: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile.bin"));
    static SPRITE_TILE_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile.sm4.bin"));
    static SPRITE_TILE_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(SPRITE_TILE_SM4_BIN),
        ShaderSetEntry::pixel_sm5(SPRITE_TILE_BIN),
    ];
    static SPRITE_TILE_EFFECT_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_effect.bin"));
    static SPRITE_TILE_EFFECT_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_effect.sm4.bin"));
    static SPRITE_TILE_EFFECT_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(SPRITE_TILE_EFFECT_SM4_BIN),
        ShaderSetEntry::pixel_sm5(SPRITE_TILE_EFFECT_BIN),
    ];
    static SPRITE_TILE_FISH_COLOR_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_fish_color.bin"));
    static SPRITE_TILE_FISH_COLOR_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_fish_color.sm4.bin"));
    static SPRITE_TILE_FISH_COLOR_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(SPRITE_TILE_FISH_COLOR_SM4_BIN),
        ShaderSetEntry::pixel_sm5(SPRITE_TILE_FISH_COLOR_BIN),
    ];
    static SPRITE_TILE_FISH_ALPHA_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_fish_alpha.bin"));
    static SPRITE_TILE_FISH_ALPHA_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_tile_fish_alpha.sm4.bin"));
    static SPRITE_TILE_FISH_ALPHA_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(SPRITE_TILE_FISH_ALPHA_SM4_BIN),
        ShaderSetEntry::pixel_sm5(SPRITE_TILE_FISH_ALPHA_BIN),
    ];
    static SPRITE_PART_SOLID_FRAG_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_part_solid_frag.bin"));
    static SPRITE_PART_SOLID_FRAG_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/sprite_part_solid_frag.sm4.bin"));
    static SPRITE_PART_SOLID_FRAG_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(SPRITE_PART_SOLID_FRAG_SM4_BIN),
        ShaderSetEntry::pixel_sm5(SPRITE_PART_SOLID_FRAG_BIN),
    ];
    static DEFERRED_BLIT_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/deferred_blit.bin"));
    static DEFERRED_BLIT_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/deferred_blit.sm4.bin"));
    static DEFERRED_BLIT_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(DEFERRED_BLIT_SM4_BIN),
        ShaderSetEntry::pixel_sm5(DEFERRED_BLIT_BIN),
    ];
    static HEAT_DISTORTION_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/heat_distortion.bin"));
    static HEAT_DISTORTION_SM4_BIN: &[u8] =
        include_bytes!(concat!(env!("OUT_DIR"), "/heat_distortion.sm4.bin"));
    static HEAT_DISTORTION_SET: &[ShaderSetEntry] = &[
        ShaderSetEntry::pixel_sm4(HEAT_DISTORTION_SM4_BIN),
        ShaderSetEntry::pixel_sm5(HEAT_DISTORTION_BIN),
    ];

    pub unsafe fn override_shaders(api: *const samase_plugin::PluginApi) {
        ((*api).set_prism_shaders)(1, 0xb, DEFERRED_BLIT_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0xe, SPRITE_TILE_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0xf, SPRITE_TILE_EFFECT_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0x17, SPRITE_PART_SOLID_FRAG_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0x20, WATER_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0x22, HEAT_DISTORTION_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0x28, SPRITE_TILE_FISH_COLOR_SET.as_ptr() as *const u8, 2);
        ((*api).set_prism_shaders)(1, 0x29, SPRITE_TILE_FISH_ALPHA_SET.as_ptr() as *const u8, 2);
    }
}

pub unsafe fn init_config(exit_on_error: bool, load_map_ini: bool) {
    loop {
        match read_config(load_map_ini) {
            Ok(o) => {
                config::set_config(o);
                break;
            }
            Err(msg) => {
                let retry = windows::message_box_retry("Mtl", &msg);
                if !retry {
                    if exit_on_error {
                        TerminateProcess(GetCurrentProcess(), 0x42302aef);
                    } else {
                        return;
                    }
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
                    let retry = windows::message_box_retry("Mtl", &msg);
                    if !retry {
                        if exit_on_error {
                            TerminateProcess(GetCurrentProcess(), 0x42302aef);
                        } else {
                            return;
                        }
                    }
                }
            }
        }
    }
}

fn read_config(load_map_ini: bool) -> Result<config::Config, String> {
    let config_slice = match read_file("samase/mtl.ini") {
        Some(s) => s,
        None => return Err(format!("Configuration file samase/mtl.ini not found.")),
    };
    if !load_map_ini {
        match read_file("samase\\mtl_map.ini") {
            Some(_) => return Err("Global mtl_map.ini is not allowed".into()),
            None => (),
        }
    }
    let mut config = config::Config::default();
    let result = config.update(&config_slice)
        .and_then(|()| {
            if let Some(map_ini) = read_map_file("samase\\mtl_map.ini") {
                return config.update(&map_ini);
            }
            Ok(())
        });
    match result {
        Ok(()) => Ok(config),
        Err(e) => {
            use std::fmt::Write;
            let mut msg = String::new();
            for c in e.chain() {
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
            for c in e.chain() {
                writeln!(msg, "{}", c).unwrap();
            }
            let msg = format!("Unable to read campaign ini:\n{}", msg);
            Err(msg)
        }
    }
}

pub trait SpriteExt {
    fn position(self) -> bw::Point;
    fn set_position(self, pos: bw::Point);
}

impl SpriteExt for bw_dat::Sprite {
    fn position(self) -> bw::Point {
        unsafe { get_sprite_pos(*self) }
    }

    fn set_position(self, pos: bw::Point) {
        unsafe { set_sprite_pos(*self, &pos) }
    }
}
