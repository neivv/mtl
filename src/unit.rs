use std::ptr::null_mut;

use bw;
use order::OrderId;
use samase;

#[derive(Copy, Clone, Debug)]
pub struct Unit(pub *mut bw::Unit);

unsafe impl Send for Unit {}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct UnitId(pub u16);

pub mod id {
    use super::UnitId;
    pub const NONE: UnitId = UnitId(0xe4);
    pub const ANY_UNIT: UnitId = UnitId(0xe5);
}

impl Unit {
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
        if other == id::ANY_UNIT {
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

    pub fn is_hallucination(&self) -> bool {
        unsafe { (*self.0).flags & 0x40000000 != 0 }
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
