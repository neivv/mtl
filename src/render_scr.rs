use std::sync::atomic::{AtomicUsize, Ordering};

use libc::c_void;

use crate::bw;

static DRAW_COMMANDS: AtomicUsize = AtomicUsize::new(0);

fn get_draw_commands() -> *mut bw::scr::DrawCommands {
    let ptr = DRAW_COMMANDS.load(Ordering::Relaxed);
    assert!(ptr != 0);
    ptr as *mut bw::scr::DrawCommands
}

pub unsafe extern fn draw_hook(
    this: *mut c_void,
    commands: *mut bw::scr::DrawCommands,
    width: u32,
    height: u32,
    orig: unsafe extern fn(*mut c_void, *mut bw::scr::DrawCommands, u32, u32) -> u32,
) -> u32 {
    DRAW_COMMANDS.store(commands as usize, Ordering::Relaxed);
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
