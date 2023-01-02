use std::mem;
use std::sync::{Mutex, MutexGuard};

use anyhow::{anyhow, Context, Error};

use bw_dat::expr::{BoolExpr, IntExpr};
use bw_dat::{Game, Image, ImageId, Unit, UnitId, UnitArray};

use crate::bw;
use crate::config;
use crate::samase;
use crate::unit;
use crate::unit_search::UnitSearch;
use crate::upgrades::Stat;
use crate::ExprExt;

trait BwTrait {
    fn set_iscript_animation(&self, image: Image, anim: u8);
    fn add_overlay(&self, image: Image, image_id: ImageId, above: bool);
}

struct Bw;

impl BwTrait for Bw {
    #[inline]
    fn set_iscript_animation(&self, image: Image, anim: u8) {
        unsafe {
            bw::set_iscript_animation(*image, anim);
        }
    }

    #[inline]
    fn add_overlay(&self, image: Image, image_id: ImageId, above: bool) {
        unsafe {
            samase::add_overlay_iscript(*image, image_id, 0, 0, above);
        }
    }
}

static AURA_STATE: Mutex<AuraState> = Mutex::new(AuraState::new());

pub fn aura_state() -> MutexGuard<'static, AuraState> {
    AURA_STATE.lock().unwrap()
}

pub struct Auras {
    auras: Vec<Aura>,
    /// Faster lookup to skip units which never can have an aura.
    /// Index with UnitId.
    units_with_aura: Vec<bool>,
    /// Highest bit specifies overlay instead of underlay
    overlays: Vec<u16>,
}

/// u8::MAX = None, otherwise index to overlays mapping to image id
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct OverlayId(u8);

impl OverlayId {
    const NONE: OverlayId = OverlayId(u8::MAX);
}

impl Auras {
    pub const fn empty() -> Auras {
        Auras {
            auras: Vec::new(),
            units_with_aura: Vec::new(),
            overlays: Vec::new(),
        }
    }

    pub fn parse_aura_config(&mut self, values: &[(String, String)]) -> Result<(), Error> {
        let mut radius = None;
        let mut shape = AuraShape::Square;
        let mut source_units = Vec::new();
        let mut affected_players = None;
        let mut source_condition = None;
        let mut target_condition = None;
        let mut overlay = OverlayId::NONE;
        let mut underlay = OverlayId::NONE;
        let mut effects = Vec::new();
        for &(ref key, ref val) in values {
            match &**key {
                "radius" => {
                    let (val, shape_) = if let Some(s) = val.strip_prefix("square") {
                        (s.trim_start(), AuraShape::Square)
                    } else if let Some(s) = val.strip_prefix("bw_circle") {
                        (s.trim_start(), AuraShape::BwCircle)
                    } else {
                        (&**val, AuraShape::Square)
                    };
                    shape = shape_;
                    radius = Some(config::parse_u16(val).context("radius")?);
                }
                "overlay" | "underlay" => {
                    let (val, dat_size) = match val.strip_prefix("with_dat_size") {
                        Some(s) => (s.trim_start(), true),
                        None => (&**val, false),
                    };
                    let id = config::parse_u16(&val).with_context(|| format!("{key} = {val}"))?;
                    if key == "overlay" {
                        overlay = self.add_overlay(id, true, dat_size)?;
                    } else {
                        underlay = self.add_overlay(id, false, dat_size)?;
                    }
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
            shape,
            source_units,
            affected_players: affected_players
                .ok_or_else(|| anyhow!("Missing affected_players"))?,
            source_condition,
            target_condition,
            overlay,
            underlay,
            effects,
        });
        Ok(())
    }

    fn add_overlay(
        &mut self,
        image_id: u16,
        is_overlay: bool,
        dat_size: bool,
    ) -> Result<OverlayId, Error> {
        if image_id & 0xc000 != 0 {
            return Err(anyhow!("Invalid overlay image"));
        }
        let value = match is_overlay {
            true => image_id | 0x8000,
            false => image_id,
        };
        let value = match dat_size {
            true => value | 0x4000,
            false => value,
        };
        match self.overlays.iter().position(|&x| x == value) {
            Some(s) => Ok(OverlayId(s as u8)),
            None => {
                let id = OverlayId(self.overlays.len() as u8);
                if id.0 == u8::MAX {
                    return Err(anyhow!("Too many overlays"));
                }
                self.overlays.push(value);
                Ok(id)
            }
        }
    }

    fn overlay_image(&self, overlay: OverlayId) -> (ImageId, bool, bool) {
        let value = self.overlays.get(overlay.0 as usize).copied().unwrap_or(0);
        (ImageId(value & 0x3fff), value & 0x8000 != 0, value & 0x4000 != 0)
    }
}

pub struct Aura {
    radius: u16,
    shape: AuraShape,
    source_units: Vec<UnitId>,
    // Bits, also 13 (0x2000) for self, 14 (0x4000) for enemies and 15 (0x8000) for allies
    affected_players: u16,
    source_condition: Option<BoolExpr>,
    target_condition: Option<BoolExpr>,
    overlay: OverlayId,
    underlay: OverlayId,
    effects: Vec<(Stat, IntExpr)>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum AuraShape {
    Square,
    BwCircle,
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
    /// Index by unit uid, gives index to stat_changes, !0 if not set
    units: Vec<u32>,
    /// Variable len data, None means end of unit's array
    stat_changes: Vec<Option<(Stat, i32)>>,
    overlay_state: OverlayState,
    /// Another OverlayState buffer, as frame step for auras
    /// determines which overlays need to be created / deleted from units
    /// by comparing current frame's and previous frame's state.
    /// Will be empty after frame step.
    #[serde(skip)]
    prev_frame_overlays: OverlayState,
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct OverlayState {
    /// 1 byte per unit per 8 overlays, 1bit set per overlay if the
    /// overlay was active for this frame.
    /// 0x1 = first overlay
    per_unit_buffer: Vec<u8>,
    /// (unit_index, byte_index) to per_unit_buffer for all non-zero bytes.
    /// Does not contain duplicates.
    active_units: Vec<(u16, u8)>,
}

impl AuraState {
    pub const fn new() -> AuraState {
        AuraState {
            units: Vec::new(),
            stat_changes: Vec::new(),
            overlay_state: OverlayState::new(),
            prev_frame_overlays: OverlayState::new(),
        }
    }

    fn add_stat_change(
        &mut self,
        index: usize,
        stat: Stat,
        value: i32,
        unit_array: &UnitArray,
    ) {
        if self.units.len() <= index {
            self.units.resize(unit_array.len(), !0);
            if self.units.len() <= index {
                return;
            }
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
        let index = unit_array.to_index(unit) as usize;
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

impl OverlayState {
    const fn new() -> OverlayState {
        OverlayState {
            per_unit_buffer: Vec::new(),
            active_units: Vec::new(),
        }
    }

    /// Marks overlay as being active for this frame.
    /// Does not actually spawn it, as that is done after all overlays have been added.
    fn add_overlay(
        &mut self,
        auras: &Auras,
        unit_array: &UnitArray,
        unit: usize,
        overlay: OverlayId,
    ) {
        let bytes_per_unit = auras.overlays.len().wrapping_add(7) / 8;
        let byte_offset = overlay.0 / 8;
        let bit_offset = overlay.0 & 7;
        let index = bytes_per_unit.wrapping_mul(unit)
            .wrapping_add(byte_offset as usize);
        if self.per_unit_buffer.len() <= index {
            self.per_unit_buffer.resize(unit_array.len() * bytes_per_unit, 0u8);
        }
        if let Some(byte) = self.per_unit_buffer.get_mut(index) {
            if *byte == 0 {
                if self.active_units.capacity() == 0 {
                    self.active_units.reserve(64);
                }
                self.active_units.push((unit as u16, byte_offset));
            }
            *byte |= 1u8 << bit_offset;
        }
    }

    fn spawn_remove_overlays<B: BwTrait>(
        &self,
        bw: B,
        auras: &Auras,
        unit_array: &UnitArray,
        prev_frame: &OverlayState,
    ) {
        // Spawn new overlays
        self.for_overlays_not_set_in_other(auras, prev_frame, |unit, overlay| {
            let (image_id, is_overlay, dat_size) = auras.overlay_image(overlay);
            if let Some(unit) = unit_array.get_by_index(unit as u32) {
                if !unit.is_dying() {
                    if let Some(sprite) = unit.sprite() {
                        let image_id = match dat_size {
                            false => image_id,
                            true => ImageId(image_id.0 + unit.id().overlay_size() as u16),
                        };
                        if let Some(old) = sprite.images().find(|x| x.id() == image_id) {
                            // Reset anim if image is dying.
                            // Technically different animation id would be nice but
                            // overlays can't really have more due to how
                            // "Use full iscript" is used.
                            if old.animation() == 1 {
                                bw.set_iscript_animation(old, 0);
                            }
                        } else {
                            // Spawn new.
                            // Not trying to spawn overlay if there's no main image,
                            // probably fine?
                            if let Some(base) = sprite.main_image() {
                                bw.add_overlay(base, image_id, is_overlay);
                            }
                        }
                    }
                }
            }
        });
        // Remove overlays
        prev_frame.for_overlays_not_set_in_other(auras, self, |unit, overlay| {
            let (image_id, _, dat_size) = auras.overlay_image(overlay);
            if let Some(unit) = unit_array.get_by_index(unit as u32) {
                if !unit.is_dying() {
                    if let Some(sprite) = unit.sprite() {
                        let image_id = match dat_size {
                            false => image_id,
                            true => ImageId(image_id.0 + unit.id().overlay_size() as u16),
                        };
                        if let Some(old) = sprite.images().find(|x| x.id() == image_id) {
                            bw.set_iscript_animation(old, 1);
                        }
                    }
                }
            }
        });
    }

    fn for_overlays_not_set_in_other<F>(
        &self,
        auras: &Auras,
        other: &OverlayState,
        mut func: F,
    )
    where F: FnMut(u16, OverlayId)
    {
        let bytes_per_unit = auras.overlays.len().wrapping_add(7) / 8;
        for &(unit, byte_offset) in &self.active_units {
            let index = bytes_per_unit.wrapping_mul(unit as usize)
                .wrapping_add(byte_offset as usize);
            let new = self.per_unit_buffer.get(index).copied().unwrap_or(0);
            let old = other.per_unit_buffer.get(index).copied().unwrap_or(0);
            let mut new_overlays = new & !old;
            if new_overlays != 0 {
                while new_overlays != 0 {
                    let bit = new_overlays.trailing_zeros();
                    let overlay = OverlayId((byte_offset << 3).wrapping_add(bit as u8));
                    func(unit, overlay);
                    new_overlays &= !(1u8 << bit);
                }
            }
        }
    }

    fn clear(&mut self, auras: &Auras) {
        let bytes_per_unit = auras.overlays.len().wrapping_add(7) / 8;
        for &(unit, byte_offset) in &self.active_units {
            let index = bytes_per_unit.wrapping_mul(unit as usize)
                .wrapping_add(byte_offset as usize);
            if let Some(out) = self.per_unit_buffer.get_mut(index) {
                *out = 0;
            }
        }
        self.active_units.clear();
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
    mem::swap(&mut state.overlay_state, &mut state.prev_frame_overlays);
    state.stat_changes.clear();
    let alliance_masks = make_alliance_masks(game);
    for source_unit in unit::active_units() {
        let source_id = source_unit.id();
        let can_have_aura = auras.units_with_aura.get(source_id.0 as usize)
            .copied().unwrap_or(false);
        if !can_have_aura {
            continue;
        }
        if source_unit.is_hallucination() {
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
                .filter(|&u| {
                    match aura.shape {
                        AuraShape::Square => true,
                        AuraShape::BwCircle => {
                            let distance = bw::distance(pos, u.position());
                            distance < aura.radius as u32
                        }
                    }
                })
                .filter(|&u| match aura.target_condition {
                    Some(ref s) => s.eval_unit(u, game),
                    None => true,
                });
            for target in units {
                let index = unit_array.to_index(target) as usize;
                for &(stat, ref value) in &aura.effects {
                    // Using source unit for any effect evaluation
                    let value = value.eval_unit(source_unit, game);
                    state.add_stat_change(index, stat, value, unit_array);
                }
                if aura.overlay != OverlayId::NONE {
                    state.overlay_state.add_overlay(auras, unit_array, index, aura.overlay);
                }
                if aura.underlay != OverlayId::NONE {
                    state.overlay_state.add_overlay(auras, unit_array, index, aura.underlay);
                }
            }
        }
    }
    state.overlay_state.spawn_remove_overlays(Bw, auras, unit_array, &state.prev_frame_overlays);
    state.prev_frame_overlays.clear(auras);
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

#[cfg(test)]
mod test {
    use super::*;

    use std::cell::RefCell;

    use bw_dat::Sprite;

    struct TestBw<'a> {
        events: &'a RefCell<Vec<BwEvent>>,
        image_buf: &'a RefCell<Vec<Image>>,
    }

    impl<'a> BwTrait for TestBw<'a> {
        fn set_iscript_animation(&self, image: Image, anim: u8) {
            self.events.borrow_mut().push(BwEvent::IscriptAnim(image, anim));
        }

        fn add_overlay(&self, image: Image, image_id: ImageId, above: bool) {
            let new = self.image_buf.borrow_mut().remove(0);
            unsafe {
                // Not going to actually bother with above/below here.
                assert!((**new).next.is_null());
                assert!((**new).prev.is_null());
                if (**image).next.is_null() == false {
                    (*(**image).next).prev = *new;
                }
                (**new).next = (**image).next;
                (**new).prev = *image;
                (**image).next = *new;

                (**new).image_id = image_id.0;
            }
            self.events.borrow_mut().push(BwEvent::AddOverlay(image, image_id, above));
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    enum BwEvent {
        IscriptAnim(Image, u8),
        AddOverlay(Image, ImageId, bool),
    }

    fn read_aura_config(text: &str) -> Result<Auras, Error> {
        let ini = crate::ini::Ini::open(&mut text.as_bytes())?;
        let mut auras = Auras::empty();
        for section in &ini.sections {
            let name = &section.name;
            if name == "aura" {
                auras.parse_aura_config(&section.values)?;
            }
        }
        Ok(auras)
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
            state.add_stat_change(0, Stat::HpRegen, 50, &unit_array);
            state.add_stat_change(0, Stat::ShieldRegen, 150, &unit_array);
            state.add_stat_change(0, Stat::HpRegen, -20, &unit_array);
            state.add_stat_change(1, Stat::HpRegen, -20, &unit_array);
            state.add_stat_change(6, Stat::HpRegen, -20, &unit_array);
            state.add_stat_change(6, Stat::HpRegen, -20, &unit_array);
            state.add_stat_change(6, Stat::HpRegen, -20, &unit_array);
            state.add_stat_change(6, Stat::MineralHarvestCarry, 20, &unit_array);
            state.add_stat_change(1, Stat::MineralHarvestCarry, 20, &unit_array);
            state.add_stat_change(6, Stat::MineralHarvestCarry, 20, &unit_array);
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

    #[test]
    fn overlays() {
        unsafe {
            // Global, so should make sure that no other tests rely on set_is_scr(false)
            bw_dat::set_is_scr(true);

            let auras = read_aura_config("
                [aura]
                radius = 128
                source_unit = 86
                affected_players = 13
                overlay = 1

                [aura]
                radius = 128
                source_unit = 86
                affected_players = 13
                overlay = 2

                [aura]
                radius = 128
                source_unit = 86
                affected_players = 13
                overlay = with_dat_size 4
                underlay = with_dat_size 8

                [aura]
                radius = 128
                source_unit = 86
                affected_players = 13
                underlay = 6

                [aura]
                radius = 128
                source_unit = 86
                affected_players = 13
                overlay = 7
            ").unwrap();
            let auras = &auras;
            let overlay_count = auras.overlays.len();
            assert_eq!(overlay_count, 6);

            let mut units: Vec<bw::Unit> = Vec::new();
            let mut sprites: Vec<bw::Sprite> = Vec::new();
            let mut images: Vec<bw::Image> = Vec::new();
            for _ in 0..10 {
                units.push(std::mem::zeroed());
            }
            for _ in 0..10 {
                sprites.push(std::mem::zeroed());
            }
            for _ in 0..20 {
                images.push(std::mem::zeroed());
            }
            let ptr = units.as_mut_ptr();
            let unit_array = &UnitArray::new(ptr, units.len());
            let units = (0..units.len()).map(|i| {
                Unit::from_ptr(ptr.add(i)).unwrap()
            }).collect::<Vec<Unit>>();

            let ptr = sprites.as_mut_ptr();
            let sprites = (0..sprites.len()).map(|i| {
                Sprite::from_ptr(ptr.add(i)).unwrap()
            }).collect::<Vec<Sprite>>();

            let ptr = images.as_mut_ptr();
            let images = (0..images.len()).map(|i| {
                Image::from_ptr(ptr.add(i)).unwrap()
            }).collect::<Vec<Image>>();

            for (i, &unit) in units.iter().enumerate() {
                (**unit).flingy.sprite = *sprites[i];
                (**unit).order = 0x17;
            }
            for (i, &sprite) in sprites.iter().enumerate() {
                (**sprite).version_specific.scr.main_image = *images[i];
                (**sprite).version_specific.scr.first_image = *images[i];
                (**sprite).version_specific.scr.last_image = *images[i];
            }

            let mut state = AuraState::new();
            let overlays = (0..overlay_count).map(|x| {
                auras.overlay_image(OverlayId(x as u8))
            }).collect::<Vec<_>>();

            state.overlay_state.add_overlay(auras, unit_array, 0, OverlayId(0));
            state.overlay_state.add_overlay(auras, unit_array, 0, OverlayId(4));
            state.overlay_state.add_overlay(auras, unit_array, 4, OverlayId(2));
            state.overlay_state.add_overlay(auras, unit_array, 6, OverlayId(0));

            let image_buf = &RefCell::new(Vec::from(&images[10..]));
            let events = &RefCell::new(Vec::new());
            state.overlay_state.spawn_remove_overlays(
                TestBw { events, image_buf },
                auras,
                unit_array,
                &state.prev_frame_overlays,
            );
            assert_eq!(
                events.borrow().as_slice(),
                &[
                    BwEvent::AddOverlay(images[0], overlays[0].0, overlays[0].1),
                    BwEvent::AddOverlay(images[0], overlays[4].0, overlays[4].1),
                    BwEvent::AddOverlay(images[4], overlays[2].0, overlays[2].1),
                    BwEvent::AddOverlay(images[6], overlays[0].0, overlays[0].1),
                ],
            );
            mem::swap(&mut state.prev_frame_overlays, &mut state.overlay_state);

            // Keep 0,0, remove 0,4, keep 4,2, remove 6,0
            // add 0,3 add 1,0
            state.overlay_state.add_overlay(auras, unit_array, 0, OverlayId(0));
            state.overlay_state.add_overlay(auras, unit_array, 0, OverlayId(3));
            state.overlay_state.add_overlay(auras, unit_array, 4, OverlayId(2));
            state.overlay_state.add_overlay(auras, unit_array, 1, OverlayId(0));

            let events = &RefCell::new(Vec::new());
            state.overlay_state.spawn_remove_overlays(
                TestBw { events, image_buf },
                auras,
                unit_array,
                &state.prev_frame_overlays,
            );
            assert_eq!(
                events.borrow().as_slice(),
                &[
                    BwEvent::AddOverlay(images[0], overlays[3].0, overlays[3].1),
                    BwEvent::AddOverlay(images[1], overlays[0].0, overlays[0].1),
                    BwEvent::IscriptAnim(images[10 + 1], 1),
                    BwEvent::IscriptAnim(images[10 + 3], 1),
                ],
            );
        }
    }
}
