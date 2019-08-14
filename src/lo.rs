use std::cell::RefCell;

use byteorder::{ReadBytesExt, LE};

use crate::bw;
use crate::samase::{self, SamaseBox};

ome2_thread_local! {
    ATTACK_OVERLAY_IDS: RefCell<Option<Vec<u32>>> = attack_overlay_ids(None.into());
    IMAGES_TBL: RefCell<Option<SamaseBox>> = images_tbl(None.into());
    ATTACK_OVERLAYS: RefCell<Vec<Option<SamaseBox>>> = attack_overlays(Vec::new().into());
}

pub unsafe fn images_dat_attack_overlay(
    image: *mut bw::Image,
    direction: u8,
) -> Option<(i8, i8)> {
    let mut images_tbl = images_tbl().borrow_mut();
    let images_tbl = images_tbl.get_or_insert_with(|| {
        samase::read_file("arr\\images.tbl").expect("Couldn't read images.tbl")
    });
    let mut ids = attack_overlay_ids().borrow_mut();
    let ids = ids.get_or_insert_with(|| {
        let images_dat = samase::read_file("arr\\images.dat").expect("Couldn't read images.dat");
        let attack_overlays_data = images_dat.get(0x463e..).and_then(|x| x.get(..4 * 999));
        match attack_overlays_data {
            Some(attack_overlays_data) => {
                attack_overlays_data
                    .chunks_exact(4)
                    .map(|mut x| x.read_u32::<LE>().unwrap())
                    .collect()
            }
            None => {
                panic!("Invalid images.dat");
            }
        }
    });
    let mut overlays = attack_overlays().borrow_mut();
    if overlays.is_empty() {
        *overlays = (0..999).map(|_| None).collect();
    }
    let image_id = (*image).image_id;
    let tbl_index = ids[image_id as usize];
    if tbl_index == 0 {
        error!("No attack overlay for image {:x}", image_id);
        return None;
    }
    if overlays[image_id as usize].is_none() {
        // Read LO to be cached
        let filename = tbl_string(&images_tbl, tbl_index as u16)?;
        let filename = &format!("unit\\{}", filename);
        let data = match samase::read_file(filename) {
            Some(s) => s,
            None => {
                error!("Couldn't read '{}' for attack overlay of image {:x}", filename, image_id);
                return None;
            }
        };
        overlays[image_id as usize] = Some(data);
    }
    let lo_data = overlays[image_id as usize].as_ref().unwrap();
    let offset = lo_data.get(8 + (*image).frame as usize..)?.read_u32::<LE>().ok()? as usize;
    let data = lo_data.get(offset.checked_add(direction as usize * 2)?..)?.get(..2)?;
    let x = data[0] as i8;
    let y = data[1] as i8;
    if (*image).flags & 0x2 != 0 {
        Some((0i8.saturating_sub(x), y))
    } else {
        Some((x, y))
    }
}

fn tbl_string(data: &[u8], index: u16) -> Option<&str> {
    let offset = data.get(index as usize * 2..)?.read_u16::<LE>().ok()? as usize;
    let len = data.get(offset..)?.iter().position(|&x| x == 0)?;
    let as_bytes = &data[offset..][..len];
    std::str::from_utf8(as_bytes).ok()
}
