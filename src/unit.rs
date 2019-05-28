use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use byteorder::{WriteBytesExt, LE};
use bw_dat::{self, UnitId, Unit};
use serde::{Serializer, Serialize, Deserializer, Deserialize};

use crate::bw;
use crate::samase;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HashableUnit(pub Unit);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SerializableUnit(pub Unit);

impl Hash for HashableUnit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write_usize((*self.0) as usize);
    }
}

impl Serialize for SerializableUnit {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;
        match save_mapping().borrow().get(&HashableUnit(self.0)) {
            Some(id) => id.serialize(serializer),
            None => Err(S::Error::custom(format!("Couldn't get id for unit {:?}", self))),
        }
    }
}

impl<'de> Deserialize<'de> for SerializableUnit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match load_mapping().borrow().get(&id) {
            Some(&unit) => Ok(SerializableUnit(unit)),
            None => Err(S::Error::custom(format!("Couldn't get unit for id {:?}", id))),
        }
    }
}

impl std::ops::Deref for SerializableUnit {
    type Target = Unit;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct SaveIdMapping {
    next: Option<Unit>,
    list: SaveIdState,
    id: u32,
    in_subunit: bool,
}

enum SaveIdState {
    ActiveUnits,
    HiddenUnits,
}

fn save_id_mapping() -> SaveIdMapping {
    SaveIdMapping {
        next: unsafe { Unit::from_ptr(samase::first_active_unit()) },
        list: SaveIdState::ActiveUnits,
        id: 0,
        in_subunit: false,
    }
}

impl Iterator for SaveIdMapping {
    type Item = (Unit, u32);
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            while self.next.is_none() {
                match self.list {
                    SaveIdState::ActiveUnits => {
                        self.next = Unit::from_ptr(samase::first_hidden_unit());
                        self.list = SaveIdState::HiddenUnits;
                    }
                    SaveIdState::HiddenUnits => return None,
                }
            }
            self.id += 1;
            let result = (self.next.unwrap(), self.id);
            let unit = *self.next.unwrap();
            if (*unit).subunit != null_mut() && !self.in_subunit {
                self.next = Unit::from_ptr((*unit).subunit);
                self.in_subunit = true;
            } else {
                if self.in_subunit {
                    self.in_subunit = false;
                    let parent = (*unit).subunit;
                    self.next = Unit::from_ptr((*parent).next);
                } else {
                    self.next = Unit::from_ptr((*unit).next);
                }
            }
            Some(result)
        }
    }
}

ome2_thread_local! {
    SAVE_ID_MAP: RefCell<HashMap<HashableUnit, u32>> = save_mapping(RefCell::new(HashMap::new()));
    LOAD_ID_MAP: RefCell<HashMap<u32, Unit>> = load_mapping(RefCell::new(HashMap::new()));
}

pub fn init_save_mapping() {
    *save_mapping().borrow_mut() = save_id_mapping().map(|(x, y)| (HashableUnit(x), y)).collect();
}

pub fn clear_save_mapping() {
    save_mapping().borrow_mut().clear();
}

pub fn init_load_mapping() {
    *load_mapping().borrow_mut() = save_id_mapping().map(|(x, y)| (y, x)).collect();
}

pub fn clear_load_mapping() {
    load_mapping().borrow_mut().clear();
}

pub trait UnitExt {
    fn set_unit_id(self, new: UnitId);
    fn sprite(self) -> Option<*mut bw::Sprite>;
    fn set_resource_amount(self, value: u16);
}

impl UnitExt for Unit {
    fn set_unit_id(self, new: UnitId) {
        // Otherwise would have to fix possearch and clip unit in map bounds
        assert!(
            self.id().dimensions() == new.dimensions(),
            "Cannot switch unit from id {:02x} to {:02x} due to different dimensions",
            self.id().0,
            new.0,
        );
        // Should also check building tile flag, but at least it isn't as unstable.
        // Also air repulse.
        // Bw sets buildings secondary order to nothing.
        // Checks multi selection changes.
        unsafe {
            let old = self.id();
            (**self).previous_unit_id = (**self).unit_id;
            (**self).previous_hp = (((**self).hitpoints >> 8) as u16).max(1);
            (**self).unit_id = new.0;
            let new_hp = (new.hitpoints() >> 8)
                .saturating_mul(i32::from((**self).previous_hp))
                .checked_div(old.hitpoints() >> 8)
                .unwrap_or(1);
            (**self).hitpoints = new_hp << 8;
            let buttons = if !self.is_completed() || self.is_disabled() {
                0xe4
            } else {
                new.0
            };
            if new.is_building() || buttons == 0xe4 {
                (**self).buttons = buttons;
            }
        }
    }

    fn sprite(self) -> Option<*mut bw::Sprite> {
        unsafe {
            match (**self).sprite == null_mut() {
                true => None,
                false => Some((**self).sprite),
            }
        }
    }

    fn set_resource_amount(self, value: u16) {
        unsafe { (&mut (**self).unit_specific2[0..]).write_u16::<LE>(value).unwrap() }
    }
}

enum AliveUnitsState {
    Active,
    Hidden,
}

pub struct AliveUnits(*mut bw::Unit, AliveUnitsState);

pub fn alive_units() -> AliveUnits {
    AliveUnits(samase::first_active_unit(), AliveUnitsState::Active)
}

impl Iterator for AliveUnits {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {
        unsafe {
            while self.0 == null_mut() {
                match self.1 {
                    AliveUnitsState::Active => {
                        self.0 = samase::first_hidden_unit();
                        self.1 = AliveUnitsState::Hidden;
                    }
                    AliveUnitsState::Hidden => return None,
                }
            }
            let unit = self.0;
            self.0 = (*unit).next;
            Some(Unit::from_ptr(unit).unwrap())
        }
    }
}

pub fn player_units(player: u8) -> impl Iterator<Item = Unit> {
    alive_units().filter(move |x| x.player() == player)
}

pub fn active_units() -> UnitListIter {
    UnitListIter(samase::first_active_unit())
}

pub struct UnitListIter(*mut bw::Unit);

impl Iterator for UnitListIter {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {
        unsafe {
            if self.0 == null_mut() {
                None
            } else {
                let result = Unit::from_ptr(self.0);
                self.0 = (*self.0).next;
                result
            }
        }
    }
}
