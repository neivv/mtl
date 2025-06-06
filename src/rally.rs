use std::ptr::null_mut;
use std::sync::atomic::{AtomicUsize, Ordering};

use byteorder::{ByteOrder, LittleEndian};
use libc::c_void;

use bw_dat::dialog::{Control, Dialog, Event, EventHandler};
use bw_dat::{Game, unit, Unit, UnitId, order, OrderId, Sprite};

use crate::bw;
use crate::config::Config;
use crate::unit_search::UnitSearch;
use crate::samase::SpriteExt;

pub unsafe extern "C" fn game_screen_rclick(
    raw_event: *mut c_void,
    orig: unsafe extern "C" fn(*mut c_void),
) {
    let event = Event::new(raw_event as *mut bw::ControlEvent);
    let (x, y) = event.mouse_pos();
    let local_player_id = bw::local_player_id();
    if bw::is_outside_game_screen(x, y) {
        return;
    }
    if bw::is_replay() || local_player_id >= 0x80 {
        // Let SCR handle rightclick to follow unit.
        return orig(raw_event);
    }
    let client_selection = bw::client_selection();
    if client_selection.is_empty() {
        return;
    }
    let unit = Unit::from_ptr(client_selection[0]).unwrap();
    if unit.player() != local_player_id {
        return;
    }
    // This hook only does rallying by itself
    if !unit.is_landed_building() {
        orig(raw_event);
        return;
    }

    let config = crate::config::config();
    if !config.has_rally(unit.id()) {
        if unit.id().rclick_action() == 3 {
            // Normal handling for towers which can rclick to attack
            orig(raw_event);
        }
        return;
    }
    if client_selection.len() != 1 {
        return;
    }
    let game = crate::game::get();
    let point = bw::screen_coord_to_game(x, y);
    let search = UnitSearch::from_bw();
    let target = find_clicked_unit(&search, local_player_id, &point);
    set_selected_unit_rally_to(&point, game, target);
    // Rallying cannot error or play yes anim
}

unsafe fn set_selected_unit_rally_to(
    point: &bw::Point,
    game: Game,
    target: Option<Unit>,
) {
    show_command_response(game, target, &point);

    let mut command = [0u8; 0xa];
    let units = &bw::unit_array();
    command[0] = 0xcc;
    command[1] = 0x00;
    LittleEndian::write_i16(&mut command[2..], point.x);
    LittleEndian::write_i16(&mut command[4..], point.y);
    LittleEndian::write_u32(&mut command[6..], units.to_unique_id_opt(target));
    bw::send_command(&command[..]);
}

// Bw function would return also fow id, but not relevant atm
unsafe fn show_command_response(game: Game, target: Option<Unit>, point: &bw::Point) {
    // Not going to show fow command response for rally pointing, that's rare anyway
    if let Some(target) = target {
        if let Some(sprite) = target.sprite() {
            sprite.set_selection_flash_timer(0x1f);
        }
    } else {
        let sprite = bw::cursor_marker();
        show_cursor_marker(game, sprite, point);
    }
}

unsafe fn show_cursor_marker(game: Game, sprite: Sprite, point: &bw::Point) {
    if let Some(image) = sprite.main_image() {
        image.set_hidden(false);
    }
    move_sprite(game, sprite, point);
    for image in sprite.images() {
        bw::set_iscript_animation(*image, 2);
    }
    crate::samase::draw_cursor_marker(1);
}

pub unsafe fn rally_cursor_marker_frame_hook(config: &Config, game: Game) {
    static LAST_FRAME_SELECTED_UNIT: AtomicUsize = AtomicUsize::new(0);

    let cursor_marker = bw::cursor_marker();

    let client_selection = bw::client_selection();
    let prev_selected = LAST_FRAME_SELECTED_UNIT.load(Ordering::Relaxed) as *mut bw::Unit;
    let selected_ptr = client_selection.get(0).map(|&u| u).unwrap_or(null_mut());
    LAST_FRAME_SELECTED_UNIT.store(selected_ptr as usize, Ordering::Relaxed);
    if selected_ptr != prev_selected {
        if let Some(&unit) = client_selection.get(0) {
            let unit = Unit::from_ptr(unit).unwrap();
            if config.has_rally(unit.id()) {
                let has_vanilla_rally = match unit.id().0 {
                    0x6a | 0x6f | 0x71 | 0x72 | 0x82 | 0x83 | 0x84 | 0x85 |
                        0x9a | 0xa0 | 0xa7 | 0x9b => true,
                    _ => false,
                };
                if !has_vanilla_rally {
                    let point = (**unit).rally_pylon.rally.pos;
                    if point.x != 0 {
                        show_cursor_marker(game, cursor_marker, &point);
                    }
                }
            }
        }
    }
}

unsafe fn move_sprite(game: Game, sprite: Sprite, point: &bw::Point) {
    let current_pos = sprite.position();
    if current_pos == *point {
        return;
    }
    let x = point.x.max(0).min(game.map_width_tiles() as i16 * 32 - 1);
    let y = point.y.max(0).min(game.map_height_tiles() as i16 * 32 - 1);
    let y_tile = y / 32;
    let old_y_tile = current_pos.y / 32;
    if y_tile != old_y_tile {
        // Linked list move
        let hlines = crate::samase::sprite_hlines();
        let hlines_end = crate::samase::sprite_hlines_end();

        if (**sprite).next.is_null() {
            *hlines_end.add(old_y_tile as usize) = (**sprite).prev;
        } else {
            (*(**sprite).next).prev = (**sprite).prev;
        }
        if (**sprite).prev.is_null() {
            *hlines.add(old_y_tile as usize) = (**sprite).next;
        } else {
            (*(**sprite).prev).next = (**sprite).next;
        }

        (**sprite).prev = null_mut();
        let next = *hlines.add(y_tile as usize);
        (**sprite).next = next;
        if next.is_null() {
            *hlines_end.add(y_tile as usize) = *sprite;
        } else {
            assert!((*next).prev.is_null());
            (*next).prev = *sprite;
        }
        *hlines.add(y_tile as usize) = *sprite;
    }
    sprite.set_position(bw::Point {
        x,
        y,
    });
    for image in sprite.images() {
        image.redraw();
    }
}

fn clickable_area(unit: Unit) -> bw::Rect {
    unsafe {
        let mut result: Option<bw::Rect> = None;
        if let Some(sprite) = unit.sprite() {
            for image in sprite.images() {
                // Shown flag
                if (**image).flags & 0x20 != 0 {
                    let frame = (**image).frame;
                    let grp = (**image).grp as *const u8;
                    let frame_header_size = if cfg!(target_pointer_width = "32") { 8 } else { 12 };
                    let frame = grp.add(6 + frame as usize * frame_header_size);
                    let map_position = (**image).map_position;
                    let new = bw::Rect {
                        left: map_position.x,
                        top: map_position.y,
                        right: map_position.x.saturating_add(*frame.add(2) as i16).saturating_sub(1),
                        bottom: map_position.y.saturating_add(*frame.add(3) as i16).saturating_sub(1),
                    };
                    if let Some(old) = result {
                        result = Some(bw::Rect {
                            left: old.left.min(new.left),
                            top: old.top.min(new.top),
                            right: old.right.max(new.right),
                            bottom: old.bottom.max(new.bottom),
                        })
                    } else {
                        result = Some(new);
                    }
                }
            }
        }
        result.unwrap_or_else(|| bw::Rect {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
        })
    }
}

unsafe fn find_clicked_unit(search: &UnitSearch, player: u8, point: &bw::Point) -> Option<Unit> {
    let area = bw::Rect {
        left: point.x.saturating_sub(32),
        top: point.y.saturating_sub(32),
        right: point.x.saturating_add(32),
        bottom: point.y.saturating_add(32),
    };
    let unit_iter = search.search_iter(&area)
        .filter(|&unit| unit.is_visible_to(player) && !unit.is_invisible_hidden_to(player))
        .filter(|&unit| !is_unselectable(unit.id()))
        .filter(|&unit| clickable_area(unit).contains_point(point));
    let mut units = Vec::with_capacity(0x40);
    for unit in unit_iter {
        if let Some(sprite) = unit.sprite() {
            let sort_order = sprite_sort_order(sprite, &unit.position());
            units.push((unit, sort_order));
        }
    }
    let (mut best, mut best_sort_order) = units.first()?;
    let mut best_clickable_sort_order = (0, 0);
    let mut best_clickable = None;
    if is_clickable_pixel(best, point) {
        best_clickable_sort_order = best_sort_order;
        best_clickable = Some(best);
    }
    // BW's ordering isn't total order:
    // if better_sort_is_clickable() {
    //      best = better_sort;
    // } else {
    //      best = smaller_placement_area
    //  }
    //
    //  So implement it with 2 passes:
    //  If topmost is clickable then topmost,
    //  otherwise either best clikable or anything above it, prioritizing smallest placement area
    for &(other, sort_order) in &units[1..] {
        if sort_order <= best_sort_order {
            if sort_order > best_clickable_sort_order {
                if is_clickable_pixel(best, point) {
                    best_clickable = Some(other);
                    best_clickable_sort_order = sort_order;
                }
            }
        } else {
            if is_clickable_pixel(best, point) {
                best_clickable = Some(other);
                best_clickable_sort_order = sort_order;
            }
            best = other;
            best_sort_order = sort_order;
        }
    }
    // If best is also best clickable, return it
    if best_clickable == Some(best) {
        return Some(best);
    }
    // Do a second loop checking >= best_clickable_sort_order
    // and pick the one with smallest placement box area
    let mut best_area = u32::max_value();
    for &(unit, sort_order) in &units {
        if sort_order >= best_clickable_sort_order {
            let placement = unit.id().placement();
            let area = placement.width as u32 * placement.height as u32;
            if area < best_area {
                best_area = area;
                best = unit;
            }
        }
    }
    Some(best)
}

fn is_unselectable(id: UnitId) -> bool {
    use bw_dat::unit::*;
    match id {
        NUCLEAR_MISSILE | SCARAB | DISRUPTION_WEB | DARK_SWARM | LEFT_UPPER_LEVEL_DOOR |
            RIGHT_UPPER_LEVEL_DOOR | LEFT_PIT_DOOR | RIGHT_PIT_DOOR => true,
        _ => false,
    }
}

/// NOTE: Bw uses index, this uses pointer value for simpler code.
///
/// That however makes the returned value unsaveable / synchronizable, even though
/// it won't affect comparisions as long as all sprites are in a single array
fn sprite_sort_order(sprite: Sprite, position: &bw::Point) -> (u32, usize) {
    // TODO: Arg position from caller unit just since sprite.position() isn't implemented yet
    let y = if sprite.elevation_level() <= 4 {
        position.y as u16 as u32
    } else {
        0
    };
    let key = ((sprite.elevation_level() as u32) << 24) |
        (y << 8) |
        ((sprite.flags() & 0x10) as u32);
    (key, *sprite as usize)
}

unsafe fn is_clickable_pixel(unit: Unit, point: &bw::Point) -> bool {
    let area = clickable_area(unit);
    if !area.contains_point(point) {
        return false;
    }
    // TODO Bw actually calls IsDrawnPixel, but too lazy to do that as long
    // as this is being only used for rallying
    true
}

pub unsafe extern "C" fn command_length(
    data: *const u8,
    max_len: u32,
) -> u32 {
    if max_len < 2 {
        return !0;
    }
    match *data.add(1) {
        0 => 0xa,
        _ => !0,
    }
}

pub unsafe extern "C" fn targeted_command_old_hook(
    ptr: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern "C" fn(*const u8, u32),
) {
    let player = player as u8;
    let unique_player = unique_player as u8;
    let data = std::slice::from_raw_parts(ptr, len as usize);
    if data.len() <= 9 {
        return orig(ptr, len);
    }
    let x = LittleEndian::read_i16(&data[1..]);
    let y = LittleEndian::read_i16(&data[3..]);
    let uid = LittleEndian::read_u16(&data[5..]) as u32;
    let order = OrderId(data[9]);
    if order == order::RALLY_UNIT {
        if let Some(unit) = bw::unit_array().get_by_unique_id(uid) {
            set_rally_command(player, unique_player, &unit.position(), Some(unit));
        }
    } else if order == order::RALLY_POS {
        set_rally_command(player, unique_player, &bw::Point { x, y }, None);
    } else {
        orig(ptr, len);
    }
}

pub unsafe extern "C" fn targeted_command_new_hook(
    ptr: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    orig: unsafe extern "C" fn(*const u8, u32),
) {
    let player = player as u8;
    let unique_player = unique_player as u8;
    let data = std::slice::from_raw_parts(ptr, len as usize);
    if data.len() <= 11 {
        return orig(ptr, len);
    }
    let x = LittleEndian::read_i16(&data[1..]);
    let y = LittleEndian::read_i16(&data[3..]);
    let uid = LittleEndian::read_u16(&data[5..]) as u32;
    let order = OrderId(data[11]);
    if order == order::RALLY_UNIT {
        if let Some(unit) = bw::unit_array().get_by_unique_id(uid) {
            set_rally_command(player, unique_player, &unit.position(), Some(unit));
        }
    } else if order == order::RALLY_POS {
        set_rally_command(player, unique_player, &bw::Point { x, y }, None);
    } else {
        orig(ptr, len);
    }
}

pub unsafe extern "C" fn command_handler(
    data: *const u8,
    len: u32,
    player: u32,
    unique_player: u32,
    _orig: unsafe extern "C" fn(*const u8, u32),
) {
    let player = player as u8;
    let unique_player = unique_player as u8;
    let data = std::slice::from_raw_parts(data, len as usize);
    if data[1] != 0x00 {
        return;
    }
    let x = LittleEndian::read_i16(&data[2..]);
    let y = LittleEndian::read_i16(&data[4..]);
    let uid = LittleEndian::read_u32(&data[6..]);
    let units = &bw::unit_array();
    let target = units.get_by_unique_id(uid);

    set_rally_command(player, unique_player, &bw::Point { x, y }, target);
}

unsafe fn set_rally_command(player: u8, uniq_player: u8, pos: &bw::Point, target: Option<Unit>) {
    let selected = crate::selection::Selection::normal(uniq_player);
    if let Some(unit) = selected.into_iter().next() {
        if unit.player() == player && unit.id() != unit::PYLON {
            (**unit).rally_pylon.rally.unit = target.map(|x| *x).unwrap_or_else(null_mut);
            (**unit).rally_pylon.rally.pos = *pos;
        }
    }
}

static MINIMAP_IMG_EVENT_HANDLER: EventHandler = EventHandler::new();

pub fn minimap_dialog_created(dialog: Dialog) {
    if let Some(image) = dialog.child_by_id(1) {
        image.set_event_handler(MINIMAP_IMG_EVENT_HANDLER.init(minimap_image_event_handler));
    } else {
        warn!("Couldn't find minimap dialog's image control")
    }
}

unsafe extern "C" fn minimap_image_event_handler(
    ctrl: *mut bw::Control,
    event: *mut bw::ControlEvent,
    orig: unsafe extern "C" fn(*mut bw::Control, *mut bw::ControlEvent) -> u32,
) -> u32 {
    {
        let event = Event::new(event);
        let ctrl = Control::new(ctrl);
        match event.event_type() {
            // rclick, double rclick
            7 | 9 => {
                let handled = handle_minimap_rally(ctrl, event);
                if handled {
                    return 1;
                }
            }
            _ => (),
        };
    }
    orig(ctrl, event)
}

unsafe fn handle_minimap_rally(ctrl: Control, event: Event) -> bool {
    let ui = bw::misc_ui_state();
    if ui.is_targeting || ui.is_paused || ui.is_placing_building {
        return false;
    }

    let local_player_id = bw::local_player_id();
    let client_selection = bw::client_selection();
    if client_selection.is_empty() {
        return false;
    }
    let unit = Unit::from_ptr(client_selection[0]).unwrap();
    if unit.player() != local_player_id {
        return false;
    }
    if !unit.is_landed_building() {
        return false;
    }

    let config = crate::config::config();
    if !config.has_rally(unit.id()) || client_selection.len() != 1 {
        if unit.id().rclick_action() == 3 {
            // Normal handling for towers which can rclick to attack
            return false;
        } else {
            return true;
        }
    }

    let game = crate::game::get();
    let point = minimap_screen_coord_to_game(game, ctrl, event.mouse_pos());
    set_selected_unit_rally_to(&point, game, None);
    true
}

fn minimap_screen_coord_to_game(
    game: Game,
    minimap_image: Control,
    (x, y): (i16, i16),
) -> bw::Point {
    let ctrl_screen = minimap_image.screen_coords();
    let ctrl_relative_pos = bw::Point {
        x: x as i16,
        y: y as i16,
    }.relative_to_rect_clamped(&ctrl_screen);
    let greater_dimension = game.map_width_tiles().max(game.map_height_tiles());
    let mul = match greater_dimension {
        0 ..= 64 => 16,
        65 ..= 128 => 32,
        129 ..= 256 => 64,
        _ => 64,
    };
    bw::Point {
        x: ctrl_relative_pos.x * mul,
        y: ctrl_relative_pos.y * mul,
    }
}
