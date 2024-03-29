use std::hash::{Hash, Hasher};
use std::ptr::null_mut;
use std::sync::{Mutex, MutexGuard};

use bw_dat::{Game, UnitId, Unit, UnitArray, unit, OrderId, order};
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
        let unit_array = bw::unit_array();
        let index = unit_array.to_index(self.0);
        if index as usize >= unit_array.len() {
            Err(S::Error::custom(format!("Couldn't get id for unit {:?}", self)))
        } else {
            // Not doing index + 1 since these are not-Option units
            // Would have to specialize Option<Unit> ?? idk
            index.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for SerializableUnit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let index = u32::deserialize(deserializer)?;
        let unit_array = bw::unit_array();
        if index as usize >= unit_array.len() {
            Err(S::Error::custom(format!("Couldn't get unit for id {:?}", index)))
        } else {
            unsafe {
                let unit = unit_array.ptr().add(index as usize);
                let unit = Unit::from_ptr(unit).unwrap();
                Ok(SerializableUnit(unit))
            }
        }
    }
}

impl std::ops::Deref for SerializableUnit {
    type Target = Unit;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Extended unit fields. Lazily initialized to reduce overhead if not used.
#[derive(Serialize, Deserialize, Clone)]
pub struct ExtendedUnitFields {
    // 0x00 = Resource 1/256 fraction
    arrays: Vec<Option<Vec<u8>>>,
}

impl ExtendedUnitFields {
    pub fn new() -> ExtendedUnitFields {
        ExtendedUnitFields {
            arrays: vec![
                None,
            ],
        }
    }

    pub fn get_mut(&mut self, unit_array: &UnitArray, unit: Unit, field: usize) -> &mut u8 {
        let index = (*unit as usize).wrapping_sub(unit_array.ptr() as usize)
            / std::mem::size_of::<bw::Unit>();
        let arr = &mut self.arrays[field];
        let arr = arr.get_or_insert_with(|| vec![0u8; unit_array.len()]);
        &mut arr[index]
    }
}

static EXTENDED_FIELDS: Mutex<ExtendedUnitFields> = Mutex::new(ExtendedUnitFields {
    arrays: Vec::new(),
});

pub fn extended_field_state() -> MutexGuard<'static, ExtendedUnitFields> {
    EXTENDED_FIELDS.lock().unwrap()
}

pub trait UnitExt {
    fn set_unit_id(self, new: UnitId);
    fn set_resource_amount(self, value: u16);
    fn issue_order(self, order: OrderId, pos: bw::Point, unit: Option<Unit>);
    fn issue_order_ground(self, order: OrderId, target: bw::Point);
    fn issue_order_unit(self, order: OrderId, target: Unit);
    fn can_issue_order(self, order: OrderId) -> bool;
    fn rclick_order(self, game: Game, units: &UnitArray, target: Unit) -> OrderId;
    fn can_be_infested(self) -> bool;
    fn can_harvest_gas_from(self, target: Unit) -> bool;
    fn kill(self);
    fn set_hitpoints(self, value: i32);
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
            (**self).previous_hp = ((self.hitpoints() >> 8) as u16).max(1);
            (**self).unit_id = new.0;
            let new_hp = (new.hitpoints() >> 8)
                .saturating_mul(i32::from((**self).previous_hp))
                .checked_div(old.hitpoints() >> 8)
                .unwrap_or(1);
            (**self).flingy.hitpoints = new_hp << 8;
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

    fn set_resource_amount(self, value: u16) {
        unsafe { (**self).unit_specific2.resource.amount = value; }
    }

    fn issue_order(self, order: OrderId, pos: bw::Point, unit: Option<Unit>) {
        if self.can_issue_order(order) {
            let unit_ptr = unit.map(|x| *x).unwrap_or(null_mut());
            unsafe { bw::issue_order(*self, order, pos, unit_ptr, unit::NONE) }
        }
    }

    fn issue_order_ground(self, order: OrderId, target: bw::Point) {
        self.issue_order(order, target, None)
    }

    fn issue_order_unit(self, order: OrderId, target: Unit) {
        self.issue_order(order, target.position(), Some(target));
    }

    fn can_issue_order(self, order: OrderId) -> bool {
        // This is checked by targeted command/rclick/etc command handlers, but bw accepts
        // it otherwise, but doesn't clear related unit, so things would end up buggy.
        if self.id() == unit::SCV && self.order() == bw_dat::order::CONSTRUCTING_BUILDING {
            return order == bw_dat::order::STOP;
        }
        // Technically should also check datreqs, oh well
        self.is_completed() && !self.is_disabled()
    }

    fn rclick_order(self, game: Game, units: &UnitArray, target: Unit) -> OrderId {
        if self.is_landed_building() {
            return order::NOTHING;
        }
        let unit_id = self.id();
        let action = if unit_id == unit::LURKER && self.is_burrowed() {
            3
        } else {
            unit_id.rclick_action()
        };
        let normal_rclick_order = || -> OrderId {
            if target.id().is_beacon() || target.id().is_powerup() {
                order::MOVE
            } else if self.is_enemy(game, target) {
                unit_id.attack_unit_order()
            } else if target.can_load_unit(game, units, self) {
                order::ENTER_TRANSPORT
            } else if target.is_burrowed() {
                order::MOVE
            } else {
                order::FOLLOW
            }
        };
        let worker_rclick_order = || -> Option<OrderId> {
            let target_mineral = match target.id() {
                unit::MINERAL_FIELD_1 | unit::MINERAL_FIELD_2 | unit::MINERAL_FIELD_3 => true,
                _ => false,
            };
            if target.id().is_powerup() {
                Some(order::MOVE)
            } else if target_mineral {
                if self.is_carrying_powerup() {
                    Some(order::MOVE)
                } else {
                    Some(order::HARVEST_MINERALS_MOVE)
                }
            } else if {
                self.can_harvest_gas_from(target) || target.id() == unit::VESPENE_GEYSER
            } {
                if self.is_carrying_powerup() {
                    Some(order::MOVE)
                } else {
                    Some(order::HARVEST_GAS_MOVE)
                }
            } else if target.id().is_town_hall() && target.player() == self.player() {
                if self.is_carrying_minerals() {
                    Some(order::RETURN_MINERALS)
                } else if self.is_carrying_gas() {
                    Some(order::RETURN_GAS)
                } else {
                    None
                }
            } else {
                None
            }
        };
        match action {
            0 => order::NOTHING,
            1 => normal_rclick_order(),
            2 => {
                if self.can_load_unit(game, units, target) {
                    order::LOAD_UNIT_TRANSPORT
                } else if target.can_load_unit(game, units, self) {
                    order::ENTER_TRANSPORT
                } else if target.is_burrowed() {
                    order::MOVE
                } else {
                    let is_queen = unit_id == unit::QUEEN || unit_id == unit::MATRIARCH;
                    if is_queen && !target.is_invincible() && target.can_be_infested() {
                        order::INFEST
                    } else if unit_id == unit::MEDIC {
                        order::MEDIC_MOVE
                    } else {
                        order::FOLLOW
                    }
                }
            }
            3 => {
                let not_enemy = target.id().is_beacon() ||
                    target.id().is_powerup() ||
                    !self.is_enemy(game, target);
                if not_enemy {
                    unit_id.return_to_idle_order()
                } else {
                    unit_id.attack_unit_order()
                }
            }
            4 => {
                worker_rclick_order().unwrap_or_else(|| normal_rclick_order())
            }
            5 => {
                worker_rclick_order().unwrap_or_else(|| {
                    if self.is_enemy(game, target) {
                        return normal_rclick_order();
                    }
                    if target.is_landed_building() && !target.is_completed() {
                        let can_continue = target.player() == self.player() &&
                            target.id().races() == unit_id.races() &&
                            target.related().filter(|&u| u != self).is_none();
                        if can_continue {
                            order::CONSTRUCTING_BUILDING
                        } else {
                            normal_rclick_order()
                        }
                    } else if {
                        !target.id().is_building() && target.can_load_unit(game, units, self)
                    } {
                        // Non-bunker transports
                        order::ENTER_TRANSPORT
                    } else if {
                        target.id().races() == unit_id.races() &&
                            target.id().is_mechanical() &&
                            target.id().hitpoints() != target.hitpoints()
                    } {
                        order::REPAIR
                    } else if target.can_load_unit(game, units, self) {
                        // Full hp bunker practically
                        order::ENTER_TRANSPORT
                    } else {
                        normal_rclick_order()
                    }
                })
            }
            _ => order::NOTHING,
        }
    }

    fn can_be_infested(self) -> bool {
        let hp_ok = Some(())
            .and_then(|()| {
                let max_hp = self.id().hitpoints() / 256;
                let current_hp = self.hitpoints().checked_add(255)? / 256;
                let percent = current_hp.checked_mul(100)?.checked_div(max_hp)?;
                Some(percent < 50)
            })
            .unwrap_or(true);
        self.is_completed() && self.id() == unit::COMMAND_CENTER && hp_ok
    }

    fn can_harvest_gas_from(self, target: Unit) -> bool {
        self.player() == target.player() &&
            target.is_completed() &&
            target.id().is_gas_building()
    }

    fn kill(self) {
        unsafe {
            samase::kill_unit(*self)
        }
    }

    fn set_hitpoints(self, value: i32) {
        unsafe {
            samase::unit_set_hp(*self, value)
        }
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
            self.0 = (*unit).flingy.next as *mut bw::Unit;
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

pub fn hidden_units() -> UnitListIter {
    UnitListIter(samase::first_hidden_unit())
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
                self.0 = (*self.0).flingy.next as *mut bw::Unit;
                result
            }
        }
    }
}

pub fn first_player_unit(player: u8) -> Option<Unit> {
    if player < 12 {
        unsafe { Unit::from_ptr(*samase::first_player_unit().add(player as usize)) }
    } else {
        None
    }
}
