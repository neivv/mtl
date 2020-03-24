use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr::null_mut;

use libc::c_void;

use crate::bw;
use crate::config;
use crate::render::{self, LightingState};

static DRAW_COMMANDS: AtomicUsize = AtomicUsize::new(0);

fn get_draw_commands() -> *mut bw::scr::DrawCommands {
    let ptr = DRAW_COMMANDS.load(Ordering::Relaxed);
    assert!(ptr != 0);
    ptr as *mut bw::scr::DrawCommands
}

pub unsafe fn global_light(
    config: &config::Lighting,
    state: &LightingState,
) -> (f32, f32, f32) {
    let low = config.end;
    let high = config.start;
    if config.cycle == 0 {
        return high;
    }
    let cycle = (state.frame % config.cycle) as f32 / (config.cycle as f32) * 3.14 * 2.0;
    let pos = (cycle.cos() + 1.0) / 2.0;
    (
        low.0 + (high.0 - low.0) * pos,
        low.1 + (high.1 - low.1) * pos,
        low.2 + (high.2 - low.2) * pos,
    )
}

pub unsafe extern fn draw_hook(
    this: *mut c_void,
    commands: *mut bw::scr::DrawCommands,
    width: u32,
    height: u32,
    orig: unsafe extern fn(*mut c_void, *mut bw::scr::DrawCommands, u32, u32) -> u32,
) -> u32 {
    DRAW_COMMANDS.store(commands as usize, Ordering::Relaxed);
    let len = (*commands).draw_command_count as usize;
    {
        let config = config::config();
        if let Some(ref conf_light) = config.lighting {
            let lighting_state = render::lighting_state();
            let color = global_light(conf_light, &lighting_state);
            for cmd in (*commands).commands.iter_mut().take(len) {
                let is_game_shader = match cmd.shader_id {
                    // Normal game shaders
                    9 | 0xe | 0xf | 0x20 => true,
                    // Deferred rendering blit
                    0xb => true,
                    // Deferred rendering early shaders, they have lighting at blit shader
                    0x18 | 0x19 | 0x1a | 0x21 => false,
                    _ => false,
                };
                if is_game_shader {
                    cmd.uniforms[0] = color.0;
                    cmd.uniforms[1] = color.1;
                    cmd.uniforms[2] = color.2;
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

    pub unsafe fn set_multiply(&self, color: (f32, f32, f32)) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        for cmd in &mut (*self.draw_commands).commands[old_len..new_len] {
            cmd.uniforms[0] = color.0;
            cmd.uniforms[1] = color.1;
            cmd.uniforms[2] = color.2;
        }
    }

    pub unsafe fn set_player_color(&self, color: (f32, f32, f32)) {
        let old_len = self.old_command_count as usize;
        let new_len = (*self.draw_commands).draw_command_count as usize;
        for cmd in &mut (*self.draw_commands).commands[old_len..new_len] {
            cmd.uniforms[8] = color.0;
            cmd.uniforms[9] = color.1;
            cmd.uniforms[10] = color.2;
        }
    }
}

unsafe fn heap_alloc(data: &[u8], out_size: *mut u32) -> *mut u8 {
    use winapi::um::heapapi::{GetProcessHeap, HeapAlloc};
    let out = HeapAlloc(GetProcessHeap(), 0, data.len()) as *mut u8;
    *out_size = data.len() as u32;
    std::ptr::copy_nonoverlapping(data.as_ptr(), out, data.len());
    out
}

pub unsafe extern fn gl_shader_hook(filename: *const u8, out_size: *mut u32) -> *mut u8 {
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
    } else {
        return null_mut();
    };
    heap_alloc(data, out_size)
}

pub unsafe extern fn d3d_shader_hook(filename: *const u8, out_size: *mut u32) -> *mut u8 {
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

