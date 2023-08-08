use byteorder::{ByteOrder, LittleEndian};

use bw_dat::UnitId;

use crate::samase;

// NOTE: Ordering must match update_dat_table order.
// No images.dat since it is loaded on game startup.
// Could just have code to keep track of if this is using
// global images.dat or not, and reload global when necessary,
// but I'm guessing images.dat editing isn't needed.
static FILES: &[&str] = &[
    "arr\\units.dat",
    "arr\\weapons.dat",
    "arr\\flingy.dat",
    "arr\\upgrades.dat",
    "arr\\techdata.dat",
    "arr\\sprites.dat",
    "arr\\orders.dat",
    "arr\\portdata.dat",
];

static UNIT_ARRAY_WIDTHS: &[u8] = &[
    1, 2, 2, 2, 4, 1, 1, 2, 4, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 2, 2, 2,
    2, 2, 2, 2, 4, 4, 8, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 2, 2, 2, 1, 2,
];

static WEAPON_ARRAY_WIDTHS: &[u8] = &[
    2, 4, 1, 2, 4, 4, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 2, 2,
];

static FLINGY_ARRAY_WIDTHS: &[u8] = &[
    2, 4, 2, 4, 1, 1, 1,
];

static SPRITE_ARRAY_WIDTHS: &[u8] = &[
    2, 1, 1, 1, 1, 1,
];

static UPGRADE_ARRAY_WIDTHS: &[u8] = &[
    2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
];

static TECHDATA_ARRAY_WIDTHS: &[u8] = &[
    2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
];

static ORDER_ARRAY_WIDTHS: &[u8] = &[
    2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 1,
];

static PORTDATA_ARRAY_WIDTHS: &[u8] = &[
    4, 4, 1, 1, 1, 1,
];


#[derive(Default)]
pub struct MapDatFiles {
    pub enable: bool,
    // If empty, then patch all
    pub enabled_ids: [Vec<u16>; 8],
}

impl MapDatFiles {
    pub fn enable_list_by_config_name(&mut self, name: &str) -> Option<&mut Vec<u16>> {
        let index = match name {
            "units_dat" => 0,
            "weapons_dat" => 1,
            "flingy_dat" => 2,
            "upgrades_dat" => 3,
            "techdata_dat" => 4,
            "sprites_dat" => 5,
            "orders_dat" => 6,
            "portdata_dat" => 7,
            _ => !0,
        };
        self.enabled_ids.get_mut(index)
    }
}

pub unsafe fn load(config: &MapDatFiles) {
    for (i, file) in FILES.iter().enumerate() {
        if let Some(file) = samase::read_map_file(file) {
            update_dat_file(i, &file, &config.enabled_ids[i]);
        }
    }

    // Patch units.dat target acquisition range values
    let units_dat = samase::units_dat();
    let target_acquisition_range = units_dat.add(23);
    assert_eq!((*target_acquisition_range).entry_size, 1);
    let acq_range_data = (*target_acquisition_range).data as *mut u8;
    for i in 0..(*target_acquisition_range).entries {
        let turret_id = match UnitId(i as u16).subunit() {
            Some(s) => s,
            None => UnitId(i as u16),
        };
        let mut range = turret_id.target_acquisition_range() as u32;
        if let Some(weapon) = turret_id.ground_weapon() {
            range = range.max(weapon.max_range() / 32);
        }
        if let Some(weapon) = turret_id.air_weapon() {
            range = range.max(weapon.max_range() / 32);
        }
        *acq_range_data.add(i as usize) = range as u8;
    }
}

unsafe fn update_dat_file(index: usize, mut file: &[u8], enabled_ids: &[u16]) {
    let table_fn: unsafe fn() -> _ = match index {
        0 => samase::units_dat,
        1 => samase::weapons_dat,
        2 => samase::flingy_dat,
        3 => samase::upgrades_dat,
        4 => samase::techdata_dat,
        5 => samase::sprites_dat,
        6 => samase::orders_dat,
        7 => samase::portdata_dat,
        _ => return,
    };
    let mut table = table_fn();
    if table.is_null() {
        return;
    }
    if file.len() < 0x10 {
        return;
    }
    let is_ext_dat = LittleEndian::read_u32(file) == 0x2b746144 &&
        LittleEndian::read_u16(&file[4..]) == 1;
    if !is_ext_dat {
        let mut table_n = 0;
        while file.len() != 0 {
            // First 2 fields in portdata are usize-sized, can't just memcpy them on 64bit.
            // Though this code runs one 32bit too for consistency.
            let is_pointer_sized = index == 7 && table_n < 2;
            let entry_count = dat_legacy_entry_count(index);
            let table_input_len = if is_pointer_sized {
                entry_count as usize * 4
            } else {
                let entry_count = match (index, table_n) {
                    (0, 0x3) | (0, 0x25) => 96,
                    (0, 0x1d) | (0, 0x20) | (0, 0x21) | (0, 0x22) | (0, 0x23) => 106,
                    (5, 0x1) | (5, 0x4) | (5, 0x5) => 387,
                    _ => entry_count,
                };
                entry_count as usize * legacy_entry_size(index, table_n)
            };
            let (table_input, rest) = file.split_at(table_input_len);
            if enabled_ids.is_empty() {
                for i in 0..entry_count {
                    update_table_field_legacy(index, table_n, table, i, table_input);
                }
            } else {
                for &id in enabled_ids {
                    update_table_field_legacy(index, table_n, table, id, table_input);
                }
            }
            file = rest;
            table_n += 1;
            table = table.add(1);
        }
    } else {
        let entry_count = LittleEndian::read_u32(&file[8..]) as u16;
        let field_count = LittleEndian::read_u32(&file[0xc..]);
        let field_bytes = Some(()).and_then(|()| {
            let bytes = file.get(0x10..)?.get(..(field_count as usize).checked_mul(0xc)?)?;
            Some(bytes)
        }).unwrap_or(&[]);
        for field_decl in field_bytes.chunks_exact(0xc) {
            let table_n = LittleEndian::read_u16(&field_decl[0..]) as usize;
            let flags = LittleEndian::read_u16(&field_decl[2..]);
            let offset = LittleEndian::read_u32(&field_decl[4..]) as usize;
            let size = LittleEndian::read_u32(&field_decl[8..]) as usize;
            let table = table.add(table_n);
            let in_size = 1 << (flags & 3);
            if (*table).entry_size != in_size {
                panic!("Unexpected dat table size {index:x}/{table_n:x} {:x}", in_size);
            }
            if is_supported_ext_field(index, table_n) {
                if let Some(data) = file.get(offset..offset.wrapping_add(size)) {
                    let shift = dat_values_per_entry_shift(index, table_n);
                    let in_size = in_size << shift;
                    if enabled_ids.is_empty() {
                        for i in 0..entry_count {
                            update_table_field(index, table_n, table, i, data, 0, in_size);
                        }
                    } else {
                        for &id in enabled_ids {
                            update_table_field(index, table_n, table, id, data, 0, in_size);
                        }
                    }
                }
            }
        }
    }
}

fn is_supported_ext_field(dat: usize, table_n: usize) -> bool {
    match dat {
        0 => match table_n {
            // Req offsets
            0x2b => false,
            0x0 ..= 0x35 => true,
            0x41 ..= 0x4c => true,
            _ => false,
        }
        1 => table_n <= 0x17,
        2 => table_n <= 0x6,
        3 => match table_n {
            // Req offsets
            0x6 => false,
            0x0 ..= 0xb => true,
            _ => false,
        }
        4 => match table_n {
            // Req offsets
            0x4 | 0x5 => false,
            0x0 ..= 0xb => true,
            _ => false,
        }
        5 => table_n <= 0x5,
        6 => match table_n {
            // Req offsets
            0x11 => false,
            0x0 ..= 0x12 => true,
            _ => false,
        }
        7 => table_n <= 0x5,
        _ => false,
    }
}

fn dat_legacy_entry_count(i: usize) -> u16 {
    match i {
        0 => 228,
        1 => 130,
        2 => 209,
        3 => 61,
        4 => 44,
        5 => 517,
        6 => 189,
        7 => 110,
        _ => 0,
    }
}

// Includes dat_values_per_entry_shift multiplier too
fn legacy_entry_size(dat: usize, table_n: usize) -> usize {
    let widths = match dat {
        0 => UNIT_ARRAY_WIDTHS,
        1 => WEAPON_ARRAY_WIDTHS,
        2 => FLINGY_ARRAY_WIDTHS,
        3 => UPGRADE_ARRAY_WIDTHS,
        4 => TECHDATA_ARRAY_WIDTHS,
        5 => SPRITE_ARRAY_WIDTHS,
        6 => ORDER_ARRAY_WIDTHS,
        7 => PORTDATA_ARRAY_WIDTHS,
        _ => &[],
    };
    widths.get(table_n).copied().unwrap_or(0) as usize
}

fn dat_values_per_entry_shift(dat: usize, table_n: usize) -> u32 {
    match (dat, table_n) {
        (0, 0x24) => 1,
        (0, 0x25) => 1,
        (0, 0x26) => 2,
        _ => 0,
    }
}

unsafe fn update_table_field_legacy(
    dat: usize,
    table_n: usize,
    table: *mut bw_dat::DatTable,
    index: u16,
    data: &[u8],
) {
    let mut start_index = match (dat, table_n) {
        (0, 0x3) | (0, 0x25) => 106,
        //(0, 0x1d) | (0, 0x20) | (0, 0x21) | (0, 0x22) | (0, 0x23) => 0,
        (5, 0x1) | (5, 0x4) | (5, 0x5) => 130,
        _ => 0,
    };
    if start_index != 0 {
        if dat == 0 && (*table).entries >= 0xe4 {
            // Main dat is extended even if this map dat was not, so start index is 0
            start_index = 0;
        } else if dat == 5 && (*table).entries >= 0x204 {
            // Same
            start_index = 0;
        }
    }
    let in_size = legacy_entry_size(dat, table_n) as u32;
    update_table_field(dat, table_n, table, index, data, start_index, in_size);
}

unsafe fn update_table_field(
    dat: usize,
    table_n: usize,
    table: *mut bw_dat::DatTable,
    index: u16,
    data: &[u8],
    start_index: usize,
    // Should be already shifted for dimensionbox etc with multiple values per entry
    in_size: u32,
) {
    let index = index as usize;
    if index < start_index {
        return;
    }
    let data_index = index - start_index;
    let is_pointer_sized = dat == 7 && table_n < 2;
    let size = if is_pointer_sized { 4 } else { in_size } as usize;
    let shift = dat_values_per_entry_shift(dat, table_n);
    let out_size = ((*table).entry_size << shift) as usize;
    let start = (data_index as usize).wrapping_mul(size);
    let out_start = (data_index as usize).wrapping_mul(out_size);
    if let Some(data) = data.get(start..start.wrapping_add(size)) {
        let in_value = if size == 1 {
            data[0] as u32
        } else if size == 2 {
            LittleEndian::read_u16(data) as u32
        } else {
            LittleEndian::read_u32(data)
        };
        let out = ((*table).data as *mut u8).add(out_start);
        if out_size == 1 {
            *out = in_value as u8;
        } else if out_size == 2 {
            (out as *mut u16).write_unaligned(in_value as u16);
        } else if out_size == 4 {
            (out as *mut u32).write_unaligned(in_value as u32);
        } else if out_size == 8 {
            if is_pointer_sized {
                (out as *mut u64).write_unaligned(in_value as u64);
            } else {
                (out as *mut u64).write_unaligned(LittleEndian::read_u64(data));
            }
        }
    }
}
