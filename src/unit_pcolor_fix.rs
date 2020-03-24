use std::io::Error;
use std::sync::Mutex;

use byteorder::{ReadBytesExt, LE};

lazy_static! {
    static ref COLORS: Mutex<Option<Vec<Color>>> = Mutex::new(None);
    static ref MINIMAP_COLORS: Mutex<Option<Vec<u8>>> = Mutex::new(None);
}

struct Color([u8; 8]);

pub fn init_unit_colors(pcx: &[u8]) -> Result<(), Error> {
    let mut pcx = pcx::Reader::new(pcx)?;
    let mut buf = vec![0; pcx.width() as usize];
    let mut result = Vec::new();
    for _ in 0..pcx.height() {
        pcx.next_row_paletted(&mut buf)?;
        for color in buf.chunks(8) {
            let mut c = [0; 8];
            for (out, &val) in c.iter_mut().zip(color.iter()) {
                *out = val;
            }
            result.push(Color(c));
        }
    }
    *COLORS.lock().unwrap() = Some(result);
    Ok(())
}

pub fn init_minimap_colors(pcx: &[u8]) -> Result<(), Error> {
    let mut pcx = pcx::Reader::new(pcx)?;
    let mut buf = vec![0; pcx.width() as usize];
    let mut result = Vec::new();
    for _ in 0..pcx.height() {
        pcx.next_row_paletted(&mut buf)?;
        for &color in &buf {
            result.push(color);
        }
    }
    *MINIMAP_COLORS.lock().unwrap() = Some(result);
    Ok(())
}

pub fn game_start_hook() {
    let colors = COLORS.lock().unwrap();
    let minimap_colors = MINIMAP_COLORS.lock().unwrap();
    if colors.is_none() && minimap_colors.is_none() {
        return;
    }
    let game = crate::game::get();
    let scenario_chk = match crate::read_map_file(game, "staredit\\scenario.chk") {
        Some(s) => s,
        None => {
            error!("No scenario.chk ???");
            return;
        }
    };
    let default = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
    let colr = match scenario_chk_section(&scenario_chk, b"COLR") {
        Some(s) => s,
        None => &default[..],
    };
    for player in 0..12 {
        unsafe {
            let orig_id = (**game).player_randomization.get(player).cloned()
                .unwrap_or(player as u32);
            let color = colr.get(orig_id as usize).cloned().unwrap_or(orig_id as u8);
            if let Some(colors) = colors.as_ref().and_then(|x| x.get(color as usize)) {
                (**game).player_color_palette[player].copy_from_slice(&colors.0)
            }
            if let Some(&color) = minimap_colors.as_ref().and_then(|x| x.get(color as usize)) {
                (**game).player_minimap_color[player] = color;
            }
        }
    }

}

fn scenario_chk_section<'a>(scenario_chk: &'a [u8], section: &[u8]) -> Option<&'a [u8]> {
    let mut pos = scenario_chk;
    loop {
        let name = pos.get(..4)?;
        let len = pos.get(4..8)?.read_u32::<LE>().ok()?;
        if name == section {
            return pos.get(8..8 + len as usize);
        }
        pos = pos.get(8 + len as usize..)?;
    }
}
