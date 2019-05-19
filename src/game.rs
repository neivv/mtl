use bw_dat::Game;

use crate::bw;

pub fn get() -> Game {
    let game = bw::game();
    Game::from_ptr(game)
}
