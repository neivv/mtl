use std::sync::{Mutex, MutexGuard};

use anyhow::{anyhow, Context, Error};

use bw_dat::expr::{BoolExpr, IntExpr};
use bw_dat::{Game, Unit, UnitId, UnitArray};

use crate::bw;
use crate::config;
use crate::unit;
use crate::unit_search::UnitSearch;
use crate::upgrades::Stat;
use crate::ExprExt;

lazy_static! {
    static ref AURA_STATE: Mutex<AuraState> = Mutex::new(AuraState::new());
}

pub fn aura_state() -> MutexGuard<'static, AuraState> {
    AURA_STATE.lock().unwrap()
}

pub struct Auras {
    auras: Vec<Aura>,
    /// Faster lookup to skip units which never can have an aura.
    /// Index with UnitId.
    units_with_aura: Vec<bool>,
}

impl Auras {
    pub fn empty() -> Auras {
        Auras {
            auras: Vec::new(),
            units_with_aura: Vec::new(),
        }
    }

    pub fn parse_aura_config(&mut self, values: &[(String, String)]) -> Result<(), Error> {
        let mut radius = None;
        let mut source_units = Vec::new();
        let mut affected_players = None;
        let mut source_condition = None;
        let mut target_condition = None;
        let mut effects = Vec::new();
        for &(ref key, ref val) in values {
            match &**key {
                "radius" => {
                    radius = Some(config::parse_u16(&val).context("radius")?);
                }
                "source_unit" => {
                    source_units = config::parse_unit_list(val).context("source_unit")?;
                }
                "affected_players" => {
                    let players = config::parse_u8_list(&val)
                        .collect::<Result<Vec<_>, Error>>()
                        .context("Parsing affected_players")?;
                    let mut mask = 0;
                    for player in players {
                        mask |= 1 << (player & 0xf);
                    }
                    affected_players = Some(mask);
                }
                "source_condition" => {
                    source_condition = Some(
                        config::parse_bool_expr(&val).context("Parsing source_condition")?
                    );
                }
                "target_condition" => {
                    target_condition = Some(
                        config::parse_bool_expr(&val).context("Parsing target_condition")?
                    );
                }
                x => {
                    let stat = config::parse_stat(x).with_context(|| format!("{}", x))?;
                    let value_count = stat.value_count();
                    let val = if value_count == 1 {
                        config::parse_int_expr(&val).with_context(|| format!("{}", x))?
                    } else {
                        // Could be definitely fixed but not going to atm.
                        // If fixed, should be refactored to share code with upgrade parsing.
                        return Err(anyhow!("Can't use multi-value stats with auras"));
                    };
                    effects.push((stat, val));
                }
            }
        }
        if source_units.is_empty() {
            return Err(anyhow!("Missing source_unit"));
        }
        let max_unit = source_units.iter().map(|x| x.0).max().unwrap_or(0);
        if self.units_with_aura.len() <= max_unit as usize {
            self.units_with_aura.resize(max_unit as usize + 1, false);
        }
        for &unit in &source_units {
            self.units_with_aura[unit.0 as usize] = true;
        }
        self.auras.push(Aura {
            radius: radius.ok_or_else(|| anyhow!("Missing radius"))?,
            source_units,
            affected_players: affected_players
                .ok_or_else(|| anyhow!("Missing affected_players"))?,
            source_condition,
            target_condition,
            effects,
        });
        Ok(())
    }
}

pub struct Aura {
    radius: u16,
    source_units: Vec<UnitId>,
    // Bits, also 13 (0x2000) for self, 14 (0x4000) for enemies and 15 (0x8000) for allies
    affected_players: u16,
    source_condition: Option<BoolExpr>,
    target_condition: Option<BoolExpr>,
    effects: Vec<(Stat, IntExpr)>,
}

impl Aura {
    fn affected_mask(&self, player: u8, alliances: &AllianceMasks) -> u16 {
        let mut mask = self.affected_players;
        if let Some(allies) = alliances.allies.get(player as usize) {
            if self.affected_players & 0x2000 != 0 {
                mask |= 1 << player;
            }
            if self.affected_players & 0x4000 != 0 {
                mask |= !allies;
            }
            if self.affected_players & 0x8000 != 0 {
                mask |= allies & !(1 << player);
            }
        }
        mask & 0x0fff
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct AuraState {
    // Index by unit uid, gives index to stat_changes, !0 if not set
    units: Vec<u32>,
    // Variable len data, None means end of unit's array
    stat_changes: Vec<Option<(Stat, i32)>>,
}

impl AuraState {
    pub fn new() -> AuraState {
        AuraState {
            units: Vec::new(),
            stat_changes: Vec::new(),
        }
    }

    fn add_stat_change(
        &mut self,
        unit: Unit,
        stat: Stat,
        value: i32,
        unit_array: &UnitArray,
    ) {
        let index = (*unit as usize).wrapping_sub(unit_array.ptr() as usize)
            / std::mem::size_of::<bw::Unit>();
        if self.units.len() <= index {
            self.units.resize(index + 1, !0);
        }
        let stat_index = self.units[index] as usize;
        if stat_index == !0 {
            // Unit didn't have stat overrides this frame, append them to end
            let stat_index = self.stat_changes.len();
            self.units[index] = stat_index as u32;
            self.stat_changes.push(Some((stat, value)));
            self.stat_changes.push(None);
            return;
        }
        let mut current_index = stat_index;
        let last_index = self.stat_changes.len() - 1;
        for change_entry in &mut self.stat_changes[stat_index..] {
            if let Some((old_stat, ref mut old_value)) = *change_entry {
                if old_stat == stat {
                    // Old stat exists, just add to it
                    *old_value = old_value.saturating_add(value);
                    return;
                }
            } else {
                // End of stat sublist, if this index is equal to last stat change
                // then we can just resize in place
                if current_index == last_index {
                    *change_entry = Some((stat, value));
                    self.stat_changes.push(None);
                    return;
                } else {
                    // Else copy to end of list
                    let len = current_index - stat_index;
                    let new_stat_index = self.stat_changes.len();
                    for i in 0..len {
                        let old = self.stat_changes[stat_index + i];
                        self.stat_changes.push(old);
                    }
                    self.stat_changes.push(Some((stat, value)));
                    self.stat_changes.push(None);
                    self.units[index] = new_stat_index as u32;
                    return;
                }
            }
            current_index = current_index.wrapping_add(1);
        }
    }

    pub fn unit_stat(&self, unit: Unit, stat: Stat, unit_array: &UnitArray) -> i32 {
        let index = (*unit as usize).wrapping_sub(unit_array.ptr() as usize)
            / std::mem::size_of::<bw::Unit>();
        let stat_index = match self.units.get(index).filter(|&&x| x != !0) {
            Some(&s) => s as usize,
            None => return 0,
        };
        for change_entry in &self.stat_changes[stat_index..] {
            if let Some((stored_stat, value)) = *change_entry {
                if stored_stat == stat {
                    return value;
                }
            } else {
                break;
            }
        }
        0
    }
}

pub fn step_auras(
    auras: &Auras,
    state: &mut AuraState,
    game: Game,
    unit_search: &UnitSearch,
    unit_array: &UnitArray,
) {
    for val in &mut state.units {
        *val = !0;
    }
    state.stat_changes.clear();
    let alliance_masks = make_alliance_masks(game);
    for source_unit in unit::active_units() {
        let source_id = source_unit.id();
        let can_have_aura = auras.units_with_aura.get(source_id.0 as usize)
            .copied().unwrap_or(false);
        if !can_have_aura {
            continue;
        }
        let active_auras = auras.auras.iter()
            .filter(|x| x.source_units.iter().any(|&id| id == source_id))
            .filter(|x| match x.source_condition {
                Some(ref s) => s.eval_unit(source_unit, game),
                None => true,
            });
        for aura in active_auras {
            let affected_mask = aura.affected_mask(source_unit.player(), &alliance_masks);
            let pos = source_unit.position();
            let area = bw::Rect {
                left: pos.x.saturating_sub(aura.radius as i16),
                top: pos.y.saturating_sub(aura.radius as i16),
                right: pos.x.saturating_add(aura.radius as i16),
                bottom: pos.y.saturating_add(aura.radius as i16),
            };
            let units = unit_search.search_iter(&area)
                .filter(|&u| {
                    let mask = 1u16 << (u.player() & 0xf);
                    mask & affected_mask != 0
                })
                .filter(|&u| match aura.target_condition {
                    Some(ref s) => s.eval_unit(u, game),
                    None => true,
                });
            for target in units {
                for &(stat, ref value) in &aura.effects {
                    // Using source unit for any effect evaluation
                    let value = value.eval_unit(source_unit, game);
                    state.add_stat_change(target, stat, value, unit_array);
                }
            }
        }
    }
}

struct AllianceMasks {
    // Enemies is just inverse of this
    allies: [u16; 0x8],
}

fn make_alliance_masks(game: Game) -> AllianceMasks {
    let mut masks = AllianceMasks {
        allies: [0u16; 8],
    };
    for player in 0..8 {
        let mut mask = 0;
        let mut bit = 1;
        for other in 0..8 {
            if game.allied(player, other) {
                mask |= bit;
            }
            bit = bit << 1;
        }
        masks.allies[player as usize] = mask;
    }
    masks
}

#[test]
fn test_unit_stats() {
    unsafe {
        let mut units: Vec<bw::Unit> = Vec::new();
        for _ in 0..10 {
            units.push(std::mem::zeroed());
        }
        let ptr = units.as_mut_ptr();
        let unit_array = UnitArray::new(ptr, units.len());
        let units = (0..10).map(|i| {
            Unit::from_ptr(ptr.add(i)).unwrap()
        }).collect::<Vec<Unit>>();
        let mut state = AuraState::new();
        state.add_stat_change(units[0], Stat::HpRegen, 50, &unit_array);
        state.add_stat_change(units[0], Stat::ShieldRegen, 150, &unit_array);
        state.add_stat_change(units[0], Stat::HpRegen, -20, &unit_array);
        state.add_stat_change(units[1], Stat::HpRegen, -20, &unit_array);
        state.add_stat_change(units[6], Stat::HpRegen, -20, &unit_array);
        state.add_stat_change(units[6], Stat::HpRegen, -20, &unit_array);
        state.add_stat_change(units[6], Stat::HpRegen, -20, &unit_array);
        state.add_stat_change(units[6], Stat::MineralHarvestCarry, 20, &unit_array);
        state.add_stat_change(units[1], Stat::MineralHarvestCarry, 20, &unit_array);
        state.add_stat_change(units[6], Stat::MineralHarvestCarry, 20, &unit_array);
        assert_eq!(state.unit_stat(units[0], Stat::HpRegen, &unit_array), 30);
        assert_eq!(state.unit_stat(units[1], Stat::HpRegen, &unit_array), -20);
        assert_eq!(state.unit_stat(units[1], Stat::MineralHarvestCarry, &unit_array), 20);
        assert_eq!(state.unit_stat(units[4], Stat::MineralHarvestCarry, &unit_array), 0);
        assert_eq!(state.unit_stat(units[8], Stat::MineralHarvestCarry, &unit_array), 0);
        assert_eq!(state.unit_stat(units[6], Stat::MineralHarvestCarry, &unit_array), 40);
        assert_eq!(state.unit_stat(units[6], Stat::HpRegen, &unit_array), -60);
        assert_eq!(state.unit_stat(units[0], Stat::ShieldRegen, &unit_array), 150);
        assert_eq!(state.unit_stat(units[1], Stat::ShieldRegen, &unit_array), 0);
        assert_eq!(state.unit_stat(units[2], Stat::ShieldRegen, &unit_array), 0);
        assert_eq!(state.unit_stat(units[6], Stat::ShieldRegen, &unit_array), 0);
    }
}
