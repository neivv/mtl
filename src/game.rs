use std::ptr::null_mut;

use bw;

#[derive(Copy, Clone)]
pub struct Game(pub *mut bw::Game);

impl Game {
    pub fn get() -> Game {
        let game = bw::game();
        assert!(game != null_mut());
        Game(game)
    }

    pub fn frame_count(self) -> u32 {
        unsafe {
            (*self.0).frame_count
        }
    }

    pub fn upgrade_level(self, player: u8, upgrade: u8) -> u8 {
        unsafe {
            assert!(player < 0xc);
            assert!(upgrade < 0x3d);
            if upgrade >= 0x2e {
                return (*self.0).upgrade_level_bw[player as usize][upgrade as usize - 0x2e]
            } else {
                return (*self.0).upgrade_level_sc[player as usize][upgrade as usize]
            }
        }
    }
}
