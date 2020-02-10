use std::slice;

use byteorder::{ReadBytesExt, LE};
use hex_slice::AsHex;

use bw;
use bw_dat::{order, OrderId};
use selection::Selection;
use unit::{self, Unit};

fn unit_from_uid_old(uid: u16) -> Option<Unit> {
    if uid == 0 {
        return None;
    }
    unsafe {
        let index = (uid & 0x7ff) as usize - 1;
        let minor_id = (uid >> 0xb) as u8;
        let unit = bw::unit_by_index(index);
        if (*unit).minor_unique_index == minor_id {
            Some(Unit(unit))
        } else {
            None
        }
    }
}

fn unit_from_uid_new(uid: u32) -> Option<Unit> {
    if uid == 0 {
        return None;
    }
    unsafe {
        let index = (uid & 0x1fff) as usize - 1;
        let minor_id = (uid >> 0xd) as u8;
        let unit = bw::unit_by_index(index);
        if (*unit).minor_unique_index == minor_id {
            Some(Unit(unit))
        } else {
            None
        }
    }
}

pub unsafe extern fn targeted_order_old(
    raw: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    let data = slice::from_raw_parts(raw, len as usize);
    fn parse(data: &[u8]) -> Option<TargetedOrder> {
        let order = OrderId(*data.get(9)?);
        let target = unit_from_uid_old(data.get(5..)?.read_u16::<LE>().ok()?);
        let pos = bw::Point {
            x: data.get(1..)?.read_i16::<LE>().ok()?,
            y: data.get(3..)?.read_i16::<LE>().ok()?,
        };
        Some(TargetedOrder {
            order,
            target,
            pos,
        })
    }
    if let Some(cmd) = parse(data) {
        targeted_order_hook(&cmd, player, unique_player, data, orig);
    } else {
        error!("Broken targeted order? {:x}", data.as_hex());
        orig(raw, len);
    }
}

pub unsafe extern fn targeted_order_new(
    raw: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    let data = slice::from_raw_parts(raw, len as usize);
    fn parse(data: &[u8]) -> Option<TargetedOrder> {
        let order = OrderId(*data.get(0xb)?);
        let target = unit_from_uid_new(data.get(5..)?.read_u32::<LE>().ok()?);
        let pos = bw::Point {
            x: data.get(1..)?.read_i16::<LE>().ok()?,
            y: data.get(3..)?.read_i16::<LE>().ok()?,
        };
        Some(TargetedOrder {
            order,
            target,
            pos,
        })
    }
    if let Some(cmd) = parse(data) {
        targeted_order_hook(&cmd, player, unique_player, data, orig);
    } else {
        error!("Broken targeted order? {:x}", data.as_hex());
        orig(raw, len);
    }
}

struct TargetedOrder {
    order: OrderId,
    target: Option<Unit>,
    pos: bw::Point,
}

unsafe fn targeted_order_hook(
    cmd: &TargetedOrder,
    _player: u32,
    unique_player: u32,
    data: &[u8],
    orig: unsafe extern fn(*const u8, u32),
) {
    match cmd.order {
        order::RALLY_UNIT => {
            let target = if let Some(target) = cmd.target {
                unit::Rally::Unit(target)
            } else {
                unit::Rally::Position(cmd.pos)
            };
            for unit in Selection::normal(unique_player) {
                unit.set_rally(target);
            }
        }
        order::RALLY_POS => {
            for unit in Selection::normal(unique_player) {
                unit.set_rally(unit::Rally::Position(cmd.pos));
            }
        }
        _ => {
            orig(data.as_ptr(), data.len() as u32);
        }
    }
}

pub unsafe extern fn right_click_old(
    raw: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    let data = slice::from_raw_parts(raw, len as usize);
    fn parse(data: &[u8]) -> Option<RightClick> {
        let target = unit_from_uid_old(data.get(5..)?.read_u16::<LE>().ok()?);
        let pos = bw::Point {
            x: data.get(1..)?.read_i16::<LE>().ok()?,
            y: data.get(3..)?.read_i16::<LE>().ok()?,
        };
        Some(RightClick {
            target,
            pos,
        })
    }
    if let Some(cmd) = parse(data) {
        right_click_hook(&cmd, player, unique_player, data, orig);
    } else {
        error!("Broken right click command? {:x}", data.as_hex());
        orig(raw, len);
    }
}

pub unsafe extern fn right_click_new(
    raw: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    let data = slice::from_raw_parts(raw, len as usize);
    fn parse(data: &[u8]) -> Option<RightClick> {
        let target = unit_from_uid_new(data.get(5..)?.read_u32::<LE>().ok()?);
        let pos = bw::Point {
            x: data.get(1..)?.read_i16::<LE>().ok()?,
            y: data.get(3..)?.read_i16::<LE>().ok()?,
        };
        Some(RightClick {
            target,
            pos,
        })
    }
    if let Some(cmd) = parse(data) {
        right_click_hook(&cmd, player, unique_player, data, orig);
    } else {
        error!("Broken right click command? {:x}", data.as_hex());
        orig(raw, len);
    }
}

struct RightClick {
    target: Option<Unit>,
    pos: bw::Point,
}

unsafe fn right_click_hook(
    cmd: &RightClick,
    _player: u32,
    unique_player: u32,
    data: &[u8],
    orig: unsafe extern fn(*const u8, u32),
) {
    let selection = Selection::normal(unique_player);
    let set_rally = selection.iter().all(|x| {
        if !x.is_landed_building() || !x.can_rally() {
            false
        } else if x.id().ground_weapon().is_some() || x.id().air_weapon().is_some() {
            cmd.target.is_none()
        } else {
            true
        }
    });
    if set_rally {
        bw::print_text("Setting rally");
        let target = if let Some(target) = cmd.target {
            unit::Rally::Unit(target)
        } else {
            unit::Rally::Position(cmd.pos)
        };
        for unit in selection {
            bw::print_text("Setting rally for unit");
            unit.set_rally(target);
        }
    } else {
        orig(data.as_ptr(), data.len() as u32);
    }
}
