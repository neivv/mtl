use std::ffi::c_void;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr::null_mut;

use crate::bw;
use crate::config;
use crate::render::{self};

static DRAW_COMMANDS: AtomicUsize = AtomicUsize::new(0);

fn get_draw_commands() -> *mut bw::scr::DrawCommands {
    let ptr = DRAW_COMMANDS.load(Ordering::Relaxed);
    assert!(ptr != 0);
    ptr as *mut bw::scr::DrawCommands
}

pub unsafe extern "C" fn draw_hook(
    this: *mut c_void,
    commands: *mut bw::scr::DrawCommands,
    width: u32,
    height: u32,
    orig: unsafe extern "C" fn(*mut c_void, *mut bw::scr::DrawCommands, u32, u32) -> u32,
) -> u32 {
    DRAW_COMMANDS.store(commands as usize, Ordering::Relaxed);
    let len = (*commands).draw_command_count as usize;
    {
        let config = config::config();
        if config.lighting.is_some() {
            let lighting_state = render::lighting_state();
            let color = lighting_state.global_light();
            for cmd in (*commands).commands.iter_mut().take(len) {
                let is_game_shader = match cmd.shader_id {
                    // Normal game shaders
                    9 | 0xe | 0xf | 0x20 | 0x22 => true,
                    // Deferred rendering blit
                    0xb => true,
                    // Deferred rendering early shaders, they have lighting at blit shader
                    0x18 | 0x19 | 0x1a | 0x21 => false,
                    // 0xc and 0x10 are also game shaders, but they are handled in set_multiply
                    _ => false,
                };
                let is_deferred_sprite = match cmd.shader_id {
                    0x18 | 0x19 | 0x1a => true,
                    _ => false,
                };
                if cmd.shader_constants[3] == -4.0 {
                    // HP bar
                    cmd.shader_constants[3] = 1.0;
                } else if is_game_shader {
                    cmd.shader_constants[0] = color.0;
                    cmd.shader_constants[1] = color.1;
                    cmd.shader_constants[2] = color.2;
                } else if is_deferred_sprite {
                    // Deferred sprite.
                    // Set lighting enable always to 1.0 (expect for HP bars
                    // which were in the above branch).
                    // This fixes foliage not having lighting enable set,
                    // which most likely is a bug in unmodded BW.
                    // Hopefully there isn't anything else that may need it to be 0.0..
                    //
                    // There is also the foliage fish which uses shaders 0x28/0x29
                    // and those shaders won't even write to lighting enable
                    // (Making it undefined). Leaving that as is for now though.
                    cmd.shader_constants[0x12] = 1.0;
                }
            }
        }
    };

    orig(this, commands, width, height)
}

pub struct TrackImageRender {
    old_command_count: u16,
    draw_commands: *mut bw::scr::DrawCommands,
}

pub unsafe fn track_image_render() -> TrackImageRender {
    let draw_commands = get_draw_commands();
    TrackImageRender {
        draw_commands,
        old_command_count: (*draw_commands).draw_command_count,
    }
}

impl TrackImageRender {
    pub unsafe fn changed(&self) -> bool {
        (*self.draw_commands).draw_command_count != self.old_command_count
    }

    pub unsafe fn mark_hp_bar(&self) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        let commands = &mut (*self.draw_commands).commands;
        for cmd in &mut commands[old_len..new_len] {
            // Tells to not apply lighting later on at draw_hook
            cmd.shader_constants[3] = -4.0;
        }
    }

    pub unsafe fn set_multiply(&self, color: (f32, f32, f32)) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        let commands = &mut (*self.draw_commands).commands;
        for cmd in &mut commands[old_len..new_len] {
            cmd.shader_constants[0] = color.0;
            cmd.shader_constants[1] = color.1;
            cmd.shader_constants[2] = color.2;
        }
    }

    pub unsafe fn set_player_color(&self, color: (f32, f32, f32)) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        let commands = &mut (*self.draw_commands).commands;
        for cmd in &mut commands[old_len..new_len] {
            cmd.shader_constants[8] = color.0;
            cmd.shader_constants[9] = color.1;
            cmd.shader_constants[10] = color.2;
        }
    }

    pub unsafe fn new_draw_commands(&self) -> (*mut bw::scr::DrawCommand, usize) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        let start = (*self.draw_commands).commands.as_mut_ptr().add(old_len);
        (start, new_len.saturating_sub(old_len))
    }
}

unsafe fn heap_alloc(data: &[u8], out_size: *mut u32) -> *mut u8 {
    use winapi::um::heapapi::{GetProcessHeap, HeapAlloc};
    let out = HeapAlloc(GetProcessHeap(), 0, data.len()) as *mut u8;
    *out_size = data.len() as u32;
    std::ptr::copy_nonoverlapping(data.as_ptr(), out, data.len());
    out
}

pub unsafe extern "C" fn gl_shader_hook(filename: *const u8, out_size: *mut u32) -> *mut u8 {
    let len = (0..).find(|&x| *filename.add(x) == 0).unwrap();
    let filename = std::slice::from_raw_parts(filename, len);
    let filename = &filename[b"ShadersGLSL/".len()..];
    let data: &'static [u8] = if filename.eq_ignore_ascii_case(b"deferred_blit.glsl") {
        include_bytes!("shaders/gl/deferred_blit.glsl")
    } else if filename.eq_ignore_ascii_case(b"sprite_forward_lit.glsl") {
        include_bytes!("shaders/gl/sprite_forward_lit.glsl")
    } else if filename.eq_ignore_ascii_case(b"sprite_part_solid_frag.glsl") {
        include_bytes!("shaders/gl/sprite_part_solid_frag.glsl")
    } else if filename.eq_ignore_ascii_case(b"sprite_tile.glsl") {
        include_bytes!("shaders/gl/sprite_tile.glsl")
    } else if filename.eq_ignore_ascii_case(b"water.glsl") {
        include_bytes!("shaders/gl/water.glsl")
    } else if filename.eq_ignore_ascii_case(b"util_lighting.glsl") {
        include_bytes!("shaders/gl/util_lighting.glsl")
    } else if filename.eq_ignore_ascii_case(b"heat_distortion.glsl") {
        include_bytes!("shaders/gl/heat_distortion.glsl")
    } else {
        return null_mut();
    };
    heap_alloc(data, out_size)
}

pub unsafe extern "C" fn d3d_shader_hook(filename: *const u8, out_size: *mut u32) -> *mut u8 {
    let len = (0..).find(|&x| *filename.add(x) == 0).unwrap();
    let filename = std::slice::from_raw_parts(filename, len);
    let filename = &filename[b"ShadersHLSL/".len()..];
    let data: &'static [u8] = if filename.eq_ignore_ascii_case(b"sprite_part_solid_frag.hlsl9") {
        include_bytes!("shaders/d3d/sprite_part_solid_frag.hlsl9")
    } else if filename.eq_ignore_ascii_case(b"sprite_tile.hlsl9") {
        include_bytes!("shaders/d3d/sprite_tile.hlsl9")
    } else if filename.eq_ignore_ascii_case(b"water.hlsl9") {
        include_bytes!("shaders/d3d/water.hlsl9")
    } else {
        return null_mut();
    };
    heap_alloc(data, out_size)
}

