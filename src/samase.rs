use std::mem;
use std::ops;
use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, Ordering};

use libc::c_void;

use winapi::um::heapapi::{GetProcessHeap, HeapFree};
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use bw_dat::{self, DatTable, UnitId, Game};

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

static mut RNG_SEED: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
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

static mut CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern fn(*const u8) -> !> = GlobalFunc(None);
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

static mut GAME: GlobalFunc<extern fn() -> *mut bw::Game> = GlobalFunc(None);
pub fn game() -> *mut bw::Game {
    unsafe { GAME.get()() }
}

static mut FIRST_ACTIVE_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}
static mut FIRST_HIDDEN_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_hidden_unit() -> *mut bw::Unit {
    unsafe { FIRST_HIDDEN_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut AI_UPDATE_ATTACK_TARGET:
    GlobalFunc<extern fn(*mut bw::Unit, u32, u32, u32) -> u32> = GlobalFunc(None);
pub unsafe fn ai_update_attack_target(unit: *mut bw::Unit, a1: u32, a2: u32, a3: u32) -> u32 {
    AI_UPDATE_ATTACK_TARGET.get()(unit, a1, a2, a3)
}

static mut UPDATE_VISIBILITY_POINT: GlobalFunc<extern fn(*mut bw::LoneSprite)> = GlobalFunc(None);
pub unsafe fn update_visibility_point(sprite: *mut bw::LoneSprite) {
    UPDATE_VISIBILITY_POINT.get()(sprite)
}

static mut CREATE_LONE_SPRITE:
    GlobalFunc<extern fn(u32, i32, i32, u32) -> *mut bw::LoneSprite> = GlobalFunc(None);
pub unsafe fn create_lone_sprite(id: u32, x: i32, y: i32, player: u32) -> *mut bw::LoneSprite {
    CREATE_LONE_SPRITE.get()(id, x, y, player)
}

static mut GET_ISCRIPT_BIN: GlobalFunc<extern fn() -> *const u8> = GlobalFunc(None);
pub unsafe fn get_iscript_bin() -> *const u8 {
    GET_ISCRIPT_BIN.get()()
}

static mut STEP_ISCRIPT_FRAME:
    GlobalFunc<extern fn(*mut bw::Image, *mut bw::Iscript, u32, *mut u32)> = GlobalFunc(None);
pub unsafe fn step_iscript_frame(
    image: *mut bw::Image,
    iscript: *mut bw::Iscript,
    dry_run: u32,
    speed_out: *mut u32,
) {
    STEP_ISCRIPT_FRAME.get()(image, iscript, dry_run, speed_out);
}

static mut SEND_COMMAND: GlobalFunc<extern fn(*const u8, u32)> = GlobalFunc(None);
pub unsafe fn send_command(data: &[u8]) {
    SEND_COMMAND.get()(data.as_ptr(), data.len() as u32)
}

static mut PLAYERS: GlobalFunc<extern fn() -> *mut bw::Player> = GlobalFunc(None);
pub unsafe fn players() -> *mut bw::Player {
    PLAYERS.get()()
}

static mut MAP_TILE_FLAGS: GlobalFunc<extern fn() -> *mut u32> = GlobalFunc(None);
pub fn map_tile_flags() -> *mut u32 {
    unsafe { MAP_TILE_FLAGS.get()() }
}

static mut PATHING: GlobalFunc<extern fn() -> *mut bw::Pathing> = GlobalFunc(None);
pub unsafe fn pathing() -> *mut bw::Pathing {
    PATHING.get()()
}

static mut IS_REPLAY: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
pub unsafe fn is_replay() -> u32 {
    IS_REPLAY.get()()
}

static mut IS_OUTSIDE_GAME_SCREEN: GlobalFunc<extern fn(i32, i32) -> u32> = GlobalFunc(None);
pub unsafe fn is_outside_game_screen(x: i32, y: i32) -> u32 {
    IS_OUTSIDE_GAME_SCREEN.get()(x, y)
}

static mut CLIENT_SELECTION: GlobalFunc<extern fn() -> *mut *mut bw::Unit> = GlobalFunc(None);
pub unsafe fn client_selection() -> *mut *mut bw::Unit {
    CLIENT_SELECTION.get()()
}

static mut UNIT_ARRAY_LEN: GlobalFunc<extern fn(*mut *mut bw::Unit, *mut usize)> = GlobalFunc(None);
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    (ptr, size)
}

static mut LOCAL_PLAYER_ID: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
pub unsafe fn local_player_id() -> u32 {
    LOCAL_PLAYER_ID.get()()
}

static mut UI_SCALE: GlobalFunc<extern fn() -> f32> = GlobalFunc(None);
pub unsafe fn ui_scale() -> f32 {
    UI_SCALE.get()()
}

static mut SCREEN_POS: GlobalFunc<extern fn(*mut i32, *mut i32)> = GlobalFunc(None);
pub unsafe fn screen_pos(x: *mut i32, y: *mut i32) {
    SCREEN_POS.get()(x, y)
}

static mut FIRST_LONE_SPRITE: GlobalFunc<extern fn() -> *mut bw::LoneSprite> = GlobalFunc(None);
pub unsafe fn first_lone_sprite() -> *mut bw::LoneSprite {
    FIRST_LONE_SPRITE.get()()
}

static mut SPRITE_HLINES: GlobalFunc<extern fn() -> *mut *mut bw::Sprite> = GlobalFunc(None);
pub unsafe fn sprite_hlines() -> *mut *mut bw::Sprite {
    SPRITE_HLINES.get()()
}

static mut SPRITE_HLINES_END: GlobalFunc<extern fn() -> *mut *mut bw::Sprite> = GlobalFunc(None);
pub unsafe fn sprite_hlines_end() -> *mut *mut bw::Sprite {
    SPRITE_HLINES_END.get()()
}

static mut SELECTIONS: GlobalFunc<extern fn() -> *mut *mut bw::Unit> = GlobalFunc(None);
pub unsafe fn selections() -> *mut *mut bw::Unit {
    SELECTIONS.get()()
}

static mut DRAW_CURSOR_MARKER: GlobalFunc<extern fn(u32)> = GlobalFunc(None);
pub fn draw_cursor_marker(draw: u32) {
    unsafe { DRAW_CURSOR_MARKER.get()(draw) }
}

static mut GET_SPRITE_POS: GlobalFunc<extern fn(*mut bw::Sprite, *mut u16)> = GlobalFunc(None);
pub unsafe fn get_sprite_pos(sprite: *mut bw::Sprite) -> bw::Point {
    let mut ret = bw::Point {
        x: 0,
        y: 0,
    };
    GET_SPRITE_POS.get()(sprite, &mut ret as *mut bw::Point as *mut u16);
    ret
}

static mut SET_SPRITE_POS: GlobalFunc<extern fn(*mut bw::Sprite, *const u16)> = GlobalFunc(None);
pub unsafe fn set_sprite_pos(sprite: *mut bw::Sprite, val: &bw::Point) {
    SET_SPRITE_POS.get()(sprite, val as *const bw::Point as *const u16);
}

static mut GET_TOOLTIP_DRAW_FUNC:
    GlobalFunc<unsafe extern fn() -> Option<unsafe extern fn(*mut bw::Control)>> = GlobalFunc(None);
pub fn get_tooltip_draw_func() -> Option<unsafe extern fn(*mut bw::Control)> {
    unsafe {
        if let Some(func) = GET_TOOLTIP_DRAW_FUNC.0 {
            func()
        } else {
            None
        }
    }
}

static mut SET_TOOLTIP_DRAW_FUNC:
    GlobalFunc<unsafe extern fn(Option<unsafe extern fn(*mut bw::Control)>)> = GlobalFunc(None);
pub fn set_tooltip_draw_func(new: Option<unsafe extern fn(*mut bw::Control)>) {
    unsafe {
        if let Some(func) = SET_TOOLTIP_DRAW_FUNC.0 {
            func(new)
        }
    }
}

static mut MISC_UI_STATE: GlobalFunc<extern fn(*mut u8)> = GlobalFunc(None);
pub fn misc_ui_state(out: &mut [u8]) {
    assert_eq!(out.len(), 3);
    unsafe { MISC_UI_STATE.get()(out.as_mut_ptr()) }
}

static mut GET_REGION: GlobalFunc<extern fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)
> = GlobalFunc(None);

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

static mut PRINT_TEXT: GlobalFunc<extern fn(*const u8)> = GlobalFunc(None);
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

static mut READ_FILE: GlobalFunc<extern fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc(None);
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

// Just using samase_shim's definition so that there isn't duplication/unnecessary mismatches
#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const samase_shim::PluginApi) {
    bw_dat::set_is_scr(crate::is_scr());
    crate::init();

    let required_version = 32;
    if (*api).version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            (*api).version, required_version,
        ));
    }

    let mut ext_arrays = null_mut();
    let ext_arrays_len = ((*api).extended_arrays)(&mut ext_arrays);
    bw_dat::set_extended_arrays(ext_arrays as *mut _, ext_arrays_len);

    CRASH_WITH_MESSAGE.0 = Some((*api).crash_with_message);
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

    init_config(true, None);
    //((*api).hook_on_first_file_access)(init_config);
    let config = config::config();
    let result = ((*api).hook_step_objects)(crate::frame_hook::frame_hook, 0);
    if result == 0 {
        fatal("Couldn't hook step_objects");
    }

    ISSUE_ORDER.init(((*api).issue_order)().map(|x| mem::transmute(x)), "issue_order");
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
    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    AI_UPDATE_ATTACK_TARGET.0 = Some(mem::transmute(((*api).ai_update_attack_target)()));
    UPDATE_VISIBILITY_POINT.0 = Some(mem::transmute(((*api).update_visibility_point)()));
    CREATE_LONE_SPRITE.0 = Some(mem::transmute(((*api).create_lone_sprite)()));
    GET_ISCRIPT_BIN.0 = Some(mem::transmute(((*api).get_iscript_bin)()));
    STEP_ISCRIPT_FRAME.0 = Some(mem::transmute(((*api).step_iscript)()));
    SEND_COMMAND.0 = Some(mem::transmute(((*api).send_command)()));
    IS_OUTSIDE_GAME_SCREEN.0 = Some(mem::transmute(((*api).is_outside_game_screen)()));
    PLAYERS.0 = Some(mem::transmute(((*api).players)()));
    MAP_TILE_FLAGS.0 = Some(mem::transmute(((*api).map_tile_flags)()));
    PATHING.0 = Some(mem::transmute(((*api).pathing)()));
    IS_REPLAY.0 = Some(mem::transmute(((*api).is_replay)()));
    CLIENT_SELECTION.0 = Some(mem::transmute(((*api).client_selection)()));
    UNIT_ARRAY_LEN.0 = Some(mem::transmute(((*api).unit_array_len)()));
    LOCAL_PLAYER_ID.0 = Some(mem::transmute(((*api).local_player_id)()));
    UI_SCALE.0 = Some(mem::transmute(((*api).ui_scale)()));
    SCREEN_POS.0 = Some(mem::transmute(((*api).screen_pos)()));
    FIRST_LONE_SPRITE.0 = Some(mem::transmute(((*api).first_lone_sprite)()));
    SPRITE_HLINES.0 = Some(mem::transmute(((*api).sprite_hlines)()));
    SPRITE_HLINES_END.0 = Some(mem::transmute(((*api).sprite_hlines_end)()));
    SELECTIONS.0 = Some(mem::transmute(((*api).selections)()));
    DRAW_CURSOR_MARKER.0 = Some(mem::transmute(((*api).draw_cursor_marker)()));
    MISC_UI_STATE.0 = Some(mem::transmute(((*api).misc_ui_state)(3)));
    GET_SPRITE_POS.0 = Some(mem::transmute(((*api).get_sprite_position)()));
    SET_SPRITE_POS.0 = Some(mem::transmute(((*api).set_sprite_position)()));
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

    if let Some(campaign) = config::campaign() {
        let ok = ((*api).set_campaigns)(campaign.campaigns.as_ptr() as *const *mut c_void);
        if ok == 0 {
            ((*api).warn_unsupported_feature)(b"Campaigns\0".as_ptr());
        }
        // Ignore failure, used for campaign briefing races which shouldn't be critical
        ((*api).hook_run_dialog)(crate::campaign_hook::run_dialog_hook);
    }
    if !config.dont_override_shaders {
        ((*api).hook_file_read)(b"ShadersGLSL\\\0".as_ptr(), render_scr::gl_shader_hook);
        ((*api).hook_file_read)(b"ShadersHLSL\\\0".as_ptr(), render_scr::d3d_shader_hook);
        prism::override_shaders(api);
    }
    if config.enable_map_dat_files {
        let ok = ((*api).hook_init_units)(crate::init_map_specific_dat);
        if let Some(portdata_dat) = ((*api).extended_dat)(9) {
            let mut len = 0usize;
            PORTDATA_DAT.store(portdata_dat(&mut len) as usize, Ordering::Relaxed);
        }
        if ok == 0 {
            ((*api).warn_unsupported_feature)(b"Map specific dat\0".as_ptr());
        }
    }
    if config.has_cmdbtn_tooltips() || config.has_status_screen_tooltips() {
        let ok = ((*api).hook_layout_draw_text)(crate::tooltip::layout_draw_text_hook);
        if ok != 0 {
            ((*api).hook_draw_graphic_layers)(crate::tooltip::draw_graphic_layers_hook);
            GET_TOOLTIP_DRAW_FUNC.0 = Some(mem::transmute(((*api).get_tooltip_draw_func)()));
            SET_TOOLTIP_DRAW_FUNC.0 = Some(mem::transmute(((*api).set_tooltip_draw_func)()));
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

    pub unsafe fn override_shaders(api: *const samase_shim::PluginApi) {
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

pub unsafe fn init_config(exit_on_error: bool, game: Option<Game>) {
    loop {
        match read_config(game) {
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

fn read_config(game: Option<Game>) -> Result<config::Config, String> {
    let config_slice = match read_file("samase/mtl.ini") {
        Some(s) => s,
        None => return Err(format!("Configuration file samase/mtl.ini not found.")),
    };
    if game.is_none() {
        match read_file("samase\\mtl_map.ini") {
            Some(_) => return Err("Global mtl_map.ini is not allowed".into()),
            None => (),
        }
    }
    let mut config = config::Config::default();
    let result = config.update(&config_slice)
        .and_then(|()| {
            if let Some(game) = game {
                if let Some(map_ini) = crate::read_map_file(game, "samase\\mtl_map.ini") {
                    return config.update(&map_ini);
                }
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
