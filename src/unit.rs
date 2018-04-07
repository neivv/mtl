use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use bw_dat::{OrderId, UnitId, unit};

use serde::{Serializer, Serialize, Deserializer, Deserialize};

use bw;
use samase;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Unit(pub *mut bw::Unit);

unsafe impl Send for Unit {}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HashableUnit(pub Unit);

impl Hash for HashableUnit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write_usize((self.0).0 as usize);
    }
}

impl Serialize for Unit {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;
        match save_mapping().borrow().get(&HashableUnit(*self)) {
            Some(id) => id.serialize(serializer),
            None => Err(S::Error::custom(format!("Couldn't get id for unit {:?}", self))),
        }
    }
}

impl<'de> Deserialize<'de> for Unit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match load_mapping().borrow().get(&id) {
            Some(&unit) => Ok(unit),
            None => Err(S::Error::custom(format!("Couldn't get unit for id {:?}", id))),
        }
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
        next: Unit::from_ptr(samase::first_active_unit()),
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
            let unit = self.next.unwrap().0;
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

impl Unit {
    pub fn from_ptr(ptr: *mut bw::Unit) -> Option<Unit> {
        if ptr == null_mut() {
            None
        } else {
            Some(Unit(ptr))
        }
    }

    pub fn player(&self) -> u8 {
        unsafe { (*self.0).player }
    }

    pub fn id(&self) -> UnitId {
        UnitId(unsafe { (*self.0).unit_id })
    }

    pub fn position(&self) -> bw::Point {
        unsafe { (*self.0).position }
    }

    pub fn matches_id(&self, other: UnitId) -> bool {
        let id = self.id();
        if other == unit::ANY_UNIT {
            true
        } else {
            id == other
        }
    }

    pub fn collision_rect(&self) -> bw::Rect {
        let collision_rect = bw::collision_rect(self.id());
        let position = self.position();
        bw::Rect {
            left: position.x - collision_rect.left,
            right: position.x + collision_rect.right + 1,
            top: position.y - collision_rect.top,
            bottom: position.y + collision_rect.bottom + 1,
        }
    }

    pub fn order(&self) -> OrderId {
        unsafe { OrderId((*self.0).order) }
    }

    pub fn order_state(&self) -> u8 {
        unsafe { (*self.0).order_state }
    }

    pub fn secondary_order(&self) -> OrderId {
        unsafe { OrderId((*self.0).secondary_order) }
    }

    pub fn is_completed(&self) -> bool {
        unsafe { (*self.0).flags & 0x1 != 0 }
    }

    pub fn is_hallucination(&self) -> bool {
        unsafe { (*self.0).flags & 0x40000000 != 0 }
    }

    pub fn is_invisible(&self) -> bool {
        unsafe { (*self.0).flags & 0x300 != 0 }
    }

    pub fn has_free_cloak(&self) -> bool {
        unsafe { (*self.0).flags & 0x800 != 0 }
    }

    pub fn is_burrowed(&self) -> bool {
        unsafe { (*self.0).flags & 0x10 != 0 }
    }

    pub fn is_disabled(&self) -> bool {
        unsafe {
            (*self.0).flags & 0x400 != 0 ||
                (*self.0).lockdown_timer != 0 ||
                (*self.0).maelstrom_timer != 0 ||
                (*self.0).stasis_timer != 0
        }
    }
}

pub fn find_units<F: FnMut(&Unit) -> bool>(area: &bw::Rect, mut filter: F) -> Vec<Unit> {
    unsafe {
        let mut unit = samase::first_active_unit();
        let mut result = Vec::new();
        while unit != null_mut() {
            let crect = Unit(unit).collision_rect();
            if rect_overlaps(&crect, area) {
                if filter(&Unit(unit)) {
                    result.push(Unit(unit));
                }
            }
            unit = (*unit).next;
        }
        result
    }
}

fn rect_overlaps(a: &bw::Rect, b: &bw::Rect) -> bool {
    a.left < b.right &&
        a.right > b.left &&
        a.top < b.bottom &&
        a.bottom > b.top
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
            Some(Unit(unit))
        }
    }
}
