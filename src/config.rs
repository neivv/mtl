use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use smallvec::SmallVec;

use anyhow::{anyhow, Context, Error};
use bw_dat::{UnitId, OrderId, SpriteId, WeaponId};
use bw_dat::expr::{IntExpr, BoolExpr};

use crate::auras::Auras;
use crate::bw;
use crate::ini::Ini;
use crate::status_screen;
use crate::upgrades::{Upgrades, Upgrade, UpgradeChanges, State, Stat};

/// Various timers, in frames, unlike bw's 8-frame chunks.
/// Though the 8-frameness for updates causes inaccuracy anyways.
#[derive(Default)]
pub struct Timers {
    pub hallucination_death: Option<u32>,
    pub unit_deaths: Vec<(UnitId, u32)>,
    pub matrix: Option<u32>,
    pub stim: Option<u32>,
    pub ensnare: Option<u32>,
    pub lockdown: Option<u32>,
    pub irradiate: Option<u32>,
    pub stasis: Option<u32>,
    pub plague: Option<u32>,
    pub maelstrom: Option<u32>,
    pub acid_spores: Option<u32>,
}

#[derive(Default)]
pub struct Supplies {
    pub zerg_max: Option<u32>,
    pub terran_max: Option<u32>,
    pub protoss_max: Option<u32>,
}

pub struct Config {
    pub timers: Timers,
    pub supplies: Supplies,
    pub upgrades: Upgrades,
    pub status_screen_tooltips: status_screen::Tooltips,
    pub return_cargo_softcode: bool,
    pub zerg_building_training: bool,
    pub bunker_units: Vec<(UnitId, SpriteId, u8)>,
    pub lighting: Option<Lighting>,
    pub auras: Auras,
    pub dont_override_shaders: bool,
    pub enable_map_dat_files: bool,
    pub cmdbtn_tooltip_half_supply: bool,
    pub cmdbtn_force_stat_txt_tooltips: bool,
    // !0 for no override
    pub sound_remaps: Vec<u32>,
    rallies: Rallies,
}

pub struct Lighting {
    pub start: (f32, f32, f32),
    pub end: (f32, f32, f32),
    // Frames for single cycle of start -> end -> start
    pub cycle: u32,
    // if set, only step lighting when death is nonzero at frame rate of death value
    pub bound_death: Option<(u8, UnitId)>,
}

struct Rallies {
    can_rally: Vec<UnitId>,
    default_order: Option<RallyOrder>,
    /// Contains indices to highest unit id with non-default rally order.
    unit_orders: Vec<Option<RallyOrder>>,
}

#[derive(Eq, Copy, Clone, PartialEq, Debug)]
pub struct RallyOrder {
    pub ground: OrderId,
    pub unit: OrderOrRclick,
}

#[derive(Eq, Copy, Clone, PartialEq, Debug)]
pub enum OrderOrRclick {
    Rclick,
    Order(OrderId),
}

impl Config {
    pub fn requires_rclick_hook(&self) -> bool {
        !self.rallies.can_rally.is_empty()
    }

    pub fn has_rally(&self, unit: UnitId) -> bool {
        self.rallies.can_rally.iter().any(|&x| x == unit)
    }

    pub fn rally_order(&self, unit: UnitId) -> Option<RallyOrder> {
        self.rallies.unit_orders.get(unit.0 as usize).cloned()
            .flatten()
            .or_else(|| self.rallies.default_order)
    }

    pub fn has_cmdbtn_tooltips(&self) -> bool {
        self.cmdbtn_tooltip_half_supply || self.cmdbtn_force_stat_txt_tooltips
    }

    pub fn has_status_screen_tooltips(&self) -> bool {
        self.status_screen_tooltips.has_any()
    }

    pub fn update(&mut self, mut data: &[u8]) -> Result<(), Error> {
        let error_invalid_field = |name: &'static str, val: &str| {
            anyhow!("Invalid field {} = {}", name, val)
        };

        let ini = Ini::open(&mut data)
            .context("Unable to read ini")?;
        let mut upgrades: BTreeMap<u8, BTreeMap<Vec<State>, Vec<UpgradeChanges>>> = BTreeMap::new();
        // This is empty during initial load (stat_txt is only loaded on map start),
        // so it should only be used for things visible ingame.
        // Currently only used for stat_txt tooltips.
        let stat_txt = crate::string_tables::stat_txt();
        let stat_txt = &*stat_txt;

        for section in &ini.sections {
            let name = &section.name;
            if name == "timers" {
                let timers = &mut self.timers;
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "hallucination_death" => {
                            u32_field(&mut timers.hallucination_death, &val, "hallucination_death")?
                        }
                        "matrix" => u32_field(&mut timers.matrix, &val, "matrix")?,
                        "stim" => u32_field(&mut timers.stim, &val, "stim")?,
                        "ensnare" => u32_field(&mut timers.ensnare, &val, "ensnare")?,
                        "lockdown" => u32_field(&mut timers.lockdown, &val, "lockdown")?,
                        "irradiate" => u32_field(&mut timers.irradiate, &val, "irradiate")?,
                        "stasis" => u32_field(&mut timers.stasis, &val, "stasis")?,
                        "plague" => u32_field(&mut timers.plague, &val, "plague")?,
                        "maelstrom" => u32_field(&mut timers.maelstrom, &val, "maelstrom")?,
                        "acid_spores" => u32_field(&mut timers.acid_spores, &val, "acid_spores")?,
                        "unit_deaths" => {
                            for pair in val.split(",") {
                                let mut tokens = pair.split(":");
                                let unit_id = tokens.next()
                                    .ok_or_else(|| error_invalid_field("timers.unit_deaths", val))?;
                                let unit_id = parse_u16(unit_id.trim()).map_err(|e| {
                                    let timer_index = timers.unit_deaths.len();
                                    e.context(format!(
                                        "timers.unit_deaths #{} unit id is not u16",
                                        timer_index
                                    ))
                                })?;
                                let time = tokens.next()
                                    .ok_or_else(|| error_invalid_field("timers.unit_deaths", val))?;
                                let time = parse_u32(time.trim()).map_err(|e| {
                                    let timer_index = timers.unit_deaths.len();
                                    e.context(format!(
                                        "timers.unit_deaths #{} time is not u32",
                                        timer_index
                                    ))
                                })?;
                                timers.unit_deaths.push((UnitId(unit_id), time));
                            }
                        }
                        x => return Err(anyhow!("unknown key timers.{}", x)),
                    }
                }
            } else if name == "supplies" {
                let supplies = &mut self.supplies;
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "zerg_max" => u32_field(&mut supplies.zerg_max, &val, "zerg_max")?,
                        "terran_max" => u32_field(&mut supplies.terran_max, &val, "terran_max")?,
                        "protoss_max" => {
                            u32_field(&mut supplies.protoss_max, &val, "protoss_max")?
                        }
                        x => return Err(anyhow!("unknown key supplies.{}", x)),
                    }
                }
            } else if name == "orders" {
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "return_cargo_softcode" => {
                            bool_field(
                                &mut self.return_cargo_softcode,
                                &val,
                                "return_cargo_softcode",
                            )?
                        }
                        "zerg_training" => {
                            bool_field(&mut self.zerg_building_training, &val, "zerg_training")?
                        }
                        "bunker_unit" => {
                            let mut tokens = val.split(",").map(|x| x.trim());
                            let unit_id = tokens.next().and_then(|x| parse_u16(x).ok())
                                .ok_or_else(|| error_invalid_field("orders.bunker_unit", val))?;
                            let sprite_id = tokens.next().and_then(|x| parse_u16(x).ok())
                                .ok_or_else(|| error_invalid_field("orders.bunker_unit", val))?;
                            let directions = tokens.next().and_then(|x| parse_u8(x).ok())
                                .ok_or_else(|| error_invalid_field("orders.bunker_unit", val))?;
                            self.bunker_units.push(
                                (UnitId(unit_id), SpriteId(sprite_id), directions)
                            );
                        }
                        x => return Err(anyhow!("unknown key {}", x)),
                    }
                }
            } else if name == "rally" {
                let rallies = &mut self.rallies;
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "can_rally" => {
                            rallies.can_rally = val.split(",")
                                .map(|x| parse_u16(x.trim()).map(UnitId))
                                .collect::<Result<Vec<UnitId>, _>>()
                                .context("rallies.can_rally")?;
                        }
                        "default_order" => {
                            rallies.default_order = Some(parse_rally_order(val)?);
                        }
                        x if x.starts_with("unit.") => {
                            let unit_id = parse_u16(&x[5..]).ok()
                                .ok_or_else(|| anyhow!("Invalid unit id in '{}'", x))? as usize;
                            if rallies.unit_orders.len() <= unit_id {
                                rallies.unit_orders.resize(unit_id + 1, None);
                            }
                            rallies.unit_orders[unit_id] = Some(parse_rally_order(val)?);
                        }
                        x => return Err(anyhow!("unknown key {}", x)),
                    }
                }
            } else if name == "render" {
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "dont_override_shaders" => {
                            bool_field(
                                &mut self.dont_override_shaders,
                                &val,
                                "dont_override_shaders",
                            )?
                        }
                        x => return Err(anyhow!("unknown key {}", x)),
                    }
                }
            } else if name == "map" {
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "enable_map_dat_files" => {
                            bool_field(
                                &mut self.enable_map_dat_files,
                                &val,
                                "enable_map_dat_files",
                            )?
                        }
                        x => return Err(anyhow!("unknown key {}", x)),
                    }
                }
            } else if name == "buttons" {
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "tooltip_half_supply" => {
                            bool_field(
                                &mut self.cmdbtn_tooltip_half_supply,
                                &val,
                                "tooltip_half_supply",
                            )?
                        }
                        "force_stat_txt_tooltips" => {
                            bool_field(
                                &mut self.cmdbtn_force_stat_txt_tooltips,
                                &val,
                                "force_stat_txt_tooltips",
                            )?
                        }
                        x => return Err(anyhow!("unknown key {}", x)),
                    }
                }
            } else if name == "lighting" {
                self.lighting = {
                    let mut lighting = Lighting {
                        start: (1.0, 1.0, 1.0),
                        end: (1.0, 1.0, 1.0),
                        cycle: 0,
                        bound_death: None,
                    };
                    for &(ref key, ref val) in &section.values {
                        match &**key {
                            "start" => {
                                lighting.start = parse_f32_tuple(val, 3)
                                    .map(|x| (x[0], x[1], x[2]))
                                    .context("lighting.start")?;
                            }
                            "end" => {
                                lighting.end = parse_f32_tuple(val, 3)
                                    .map(|x| (x[0], x[1], x[2]))
                                    .context("lighting.end")?;
                            }
                            "cycle" => {
                                lighting.cycle = parse_u32(val)
                                    .context("lighting.cycle")?;
                            }
                            "death" => {
                                lighting.bound_death = parse_u32_tuple(val, 2)
                                    .map(|x| Some((x[0] as u8, UnitId(x[1] as u16))))
                                    .context("lighting.death")?;
                            }
                            x => return Err(anyhow!("unknown key {}", x)),
                        }
                    }
                    Some(lighting)
                };
            } else if name == "aura" {
                self.auras.parse_aura_config(&section.values)?;
            } else if name == "sound_remaps" {
                for &(ref key, ref val) in &section.values {
                    let source = parse_u32(key)
                        .with_context(|| format!("Invalid sound id \"{}\"", key))? as usize;
                    let dest = parse_u32(val)
                        .with_context(|| format!("Invalid sound id \"{}\"", val))?;
                    if self.sound_remaps.len() <= source {
                        self.sound_remaps.resize_with(source + 1, || !0);
                    }
                    self.sound_remaps[source] = dest;
                }
            } else if name.starts_with("upgrade") {
                let mut tokens = name.split(".").skip(1);
                let generic_error = || {
                    anyhow!(
                        "Upgrade section {} isn't formatted as \"upgrade.<x>.level.<y>\"", name
                    )
                };
                let id = tokens.next().ok_or_else(generic_error)?;
                let level_str = tokens.next().ok_or_else(generic_error)?;
                let level = tokens.next().ok_or_else(generic_error)?;
                if level_str != "level" {
                    return Err(generic_error());
                }
                let id = parse_u8(id)
                    .with_context(|| format!("Invalid upgrade id \"{}\"", id))?;
                let level = parse_u8(level)
                    .with_context(|| format!("Invalid upgrade level \"{}\"", level))?;

                let mut units = Vec::new();
                let mut states = Vec::new();
                let mut stats = Vec::new();
                let mut condition = None;
                for &(ref key, ref val) in &section.values {
                    match &**key {
                        "units" => {
                            units = parse_unit_list(val)?;
                        }
                        "state" => {
                            for tok in brace_aware_split(val, ",").map(|x| x.trim()) {
                                let state = parse_state(tok)
                                    .with_context(|| format!("{} state", name))?;
                                states.push(state);
                            }
                        }
                        "condition" => {
                            if condition.is_some() {
                                return Err(anyhow!("Cannot have multiple conditions"));
                            }
                            let cond = parse_bool_expr(val)
                                .with_context(|| format!("Condition of {}", name))?;
                            condition = Some(cond);
                        }
                        x => {
                            let stat = parse_stat(x)
                                .with_context(|| format!("In {}:{}", name, x))?;
                            let value_count = stat.value_count();
                            let values = if value_count == 1 {
                                let mut vec = SmallVec::new();
                                let val = parse_int_expr(&val)
                                    .with_context(|| format!("In {}:{}", name, x))?;
                                vec.push(val);
                                vec
                            } else {
                                parse_int_expr_tuple(&val, value_count)
                                    .with_context(|| format!("In {}:{}", name, x))?
                                    .into()
                            };
                            stats.push((stat, values));
                        }
                    }
                }
                if units.is_empty() {
                    return Err(anyhow!("{} did not specify any units", name));
                }
                let changes = UpgradeChanges {
                    units,
                    level,
                    changes: stats,
                    condition,
                };
                upgrades.entry(id).or_insert_with(|| Default::default())
                    .entry(states).or_insert_with(|| Default::default())
                    .push(changes);
            } else if name.starts_with("status_screen") {
                let mut tokens = name.split(".").skip(1);
                let ty = tokens.next().ok_or_else(|| {
                    anyhow!(r#"status_screen requires either "weapon" "armor" "shields" or "special""#)
                })?;
                match ty {
                    "weapon" => self.status_screen_tooltips.add_weapon(&section.values, stat_txt),
                    "armor" => self.status_screen_tooltips.add_armor(&section.values, stat_txt),
                    "shields" => {
                        self.status_screen_tooltips.add_shields(&section.values, stat_txt)
                    }
                    "special" => {
                        self.status_screen_tooltips.add_special(&section.values, stat_txt)
                    }
                    _ => {
                        return Err(anyhow!(
                            r#"status_screen requires either "weapon" "armor" "shields" or "special", got "{}""#,
                            ty
                        ));
                    }
                }?
            } else {
                return Err(anyhow!("Unknown section name \"{}\"", name));
            }
        }
        let upgrades = upgrades.into_iter().map(|(id, mut changes)| {
            let mut all_matching_units = Vec::with_capacity(8);
            for change_lists in changes.values_mut() {
                for changes in change_lists.iter() {
                    for &u in &changes.units {
                        if !all_matching_units.iter().any(|&x| x == u) {
                            all_matching_units.push(u);
                        }
                    }
                }
                change_lists.sort_by_key(|x| x.level);
            }
            let upgrade = Upgrade {
                all_matching_units,
                changes,
            };
            (id as usize, upgrade)
        }).collect();
        self.upgrades.update(upgrades);
        Ok(())
    }
}

impl Default for Config {
    fn default() -> Config {
        Config {
            timers: Timers::default(),
            supplies: Supplies::default(),
            return_cargo_softcode: false,
            zerg_building_training: false,
            dont_override_shaders: false,
            enable_map_dat_files: false,
            cmdbtn_tooltip_half_supply: false,
            cmdbtn_force_stat_txt_tooltips: false,
            upgrades: Upgrades::new(),
            auras: Auras::empty(),
            status_screen_tooltips: status_screen::Tooltips::new(),
            lighting: None,
            bunker_units: Vec::new(),
            sound_remaps: Vec::new(),
            rallies: Rallies {
                can_rally: Vec::new(),
                default_order: None,
                unit_orders: Vec::new(),
            }
        }
    }
}

pub struct Campaign {
    // No cleanup code for these, but no way to load them after game init either atm.
    pub campaigns: [*mut bw::CampaignMission; 6],
}

unsafe impl Sync for Campaign {}
unsafe impl Send for Campaign {}

static CONFIG: Mutex<Option<Arc<Config>>> = Mutex::new(None);
static CAMPAIGN: Mutex<Option<Arc<Campaign>>> = Mutex::new(None);

fn bool_field(out: &mut bool, value: &str, field: &'static str) -> Result<(), Error> {
    match value {
        "true" | "True" | "1" | "y" | "Y" => *out = true,
        "false" | "False" | "0" | "n" | "N" => *out = true,
        _ => {
            return Err(anyhow!("Invalid value `{}` for bool {}", value, field));
        }
    }
    Ok(())
}

fn u32_field(out: &mut Option<u32>, value: &str, field_name: &'static str) -> Result<(), Error> {
    let value = parse_u32(value)
        .with_context(|| format!("{} is not u32", field_name))?;
    *out = Some(value);
    Ok(())
}

fn parse_u32(value: &str) -> Result<u32, Error> {
    Ok(if value.starts_with("0x") {
        u32::from_str_radix(&value[2..], 16)?
    } else {
        u32::from_str_radix(value, 10)?
    })
}

pub fn parse_u16(value: &str) -> Result<u16, Error> {
    Ok(if value.starts_with("0x") {
        u16::from_str_radix(&value[2..], 16)?
    } else {
        u16::from_str_radix(value, 10)?
    })
}

pub fn parse_u8(value: &str) -> Result<u8, Error> {
    Ok(if value.starts_with("0x") {
        u8::from_str_radix(&value[2..], 16)?
    } else {
        u8::from_str_radix(value, 10)?
    })
}

fn parse_race(value: &str) -> Result<u8, Error> {
    match value {
        "zerg" => Ok(0),
        "terran" => Ok(1),
        "protoss" => Ok(2),
        _ => parse_u8(value),
    }
}

pub fn parse_u8_list<'a>(values: &'a str) -> impl Iterator<Item=Result<u8, Error>> + 'a {
    values.split(",").map(|x| parse_u8(x.trim()))
}

pub fn parse_unit_list(values: &str) -> Result<Vec<UnitId>, Error> {
    let count = values.split(",").count();
    let mut out = Vec::with_capacity(count);
    for tok in values.split(",").map(|x| x.trim()) {
        let id = parse_u16(tok)
            .with_context(|| format!("{} is not an unit id", tok))?;
        out.push(UnitId(id));
    }
    Ok(out)
}

pub fn parse_weapon_list(values: &str) -> Result<Vec<WeaponId>, Error> {
    let count = values.split(",").count();
    let mut out = Vec::with_capacity(count);
    for tok in values.split(",").map(|x| x.trim()) {
        let id = parse_u16(tok)
            .with_context(|| format!("{} is not a weapon id", tok))?;
        out.push(WeaponId(id));
    }
    Ok(out)
}

fn parse_state(value: &str) -> Result<State, Error> {
    Ok(match value {
        "self_cloaked" => State::SelfCloaked,
        "arbiter_cloaked" => State::ArbiterCloaked,
        "burrowed" => State::Burrowed,
        "incomplete" => State::Incomplete,
        "disabled" => State::Disabled,
        "damaged" => State::Damaged,
        _ => {
            fn in_braces<'a>(text: &'a str, prefix: &str) -> Result<Option<&'a str>, Error> {
                if text.starts_with(prefix) {
                    let text = &text[prefix.len()..].trim();
                    let ok = text.get(..1) == Some("(") &&
                        text.get(text.len() - 1..) == Some(")");
                    if !ok {
                        Err(anyhow!("Invalid syntax"))
                    } else {
                        Ok(Some(&text[1..text.len() - 1]))
                    }
                } else {
                    Ok(None)
                }
            }
            if let Some(text) = in_braces(value, "order")? {
                let orders = parse_u8_list(text).map(|x| x.map(|x| OrderId(x)))
                    .collect::<Result<_, Error>>()?;
                State::Order(orders)
            } else if let Some(text) = in_braces(value, "animation")? {
                let anims = parse_u8_list(text).collect::<Result<_, Error>>()?;
                State::IscriptAnim(anims)
            } else {
                return Err(anyhow!("Unknown state {}", value));
            }
        }
    })
}

pub fn parse_stat(key: &str) -> Result<Stat, Error> {
    Ok(match key {
        "hp_regen" => Stat::HpRegen,
        "shield_regen" => Stat::ShieldRegen,
        "energy_regen" => Stat::EnergyRegen,
        "resource_regen" => Stat::ResourceRegen,
        "cooldown" => Stat::Cooldown,
        "air_cooldown" => Stat::AirCooldown,
        "ground_cooldown" => Stat::GroundCooldown,
        "larva_timer" => Stat::LarvaTimer,
        "mineral_harvest_time" => Stat::MineralHarvestTime,
        "gas_harvest_time" => Stat::GasHarvestTime,
        "unload_cooldown" => Stat::UnloadCooldown,
        "creep_spread_timer" => Stat::CreepSpreadTimer,
        "mineral_harvest_reduce" => Stat::MineralHarvestReduce,
        "mineral_harvest_carry" => Stat::MineralHarvestCarry,
        "gas_harvest_reduce" => Stat::GasHarvestReduce,
        "gas_harvest_carry" => Stat::GasHarvestCarry,
        "gas_harvest_carry_depleted" => Stat::GasHarvestCarryDepleted,
        "set_unit_id" => Stat::SetUnitId,
        "player_color" => Stat::PlayerColor,
        "player_color_palette" => Stat::PlayerColorPalette,
        "show_energy" => Stat::ShowEnergy,
        "show_shields" => Stat::ShowShields,
        _ => return Err(anyhow!("Unknown stat {}", key)),
    })
}

/// NOTE: Passing an empty string will not yield anything
fn brace_aware_split<'a>(text: &'a str, tok: &'a str) -> BraceSplit<'a> {
    BraceSplit(text, tok)
}

struct BraceSplit<'a>(&'a str, &'a str);

impl<'a> Iterator for BraceSplit<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            return None;
        }
        let bytes = self.0.as_bytes();
        let mut depth = 0;
        for (i, &b) in bytes.iter().enumerate() {
            if b == b'(' {
                depth += 1;
            } else if b == b')' {
                depth -= 1;
            } else if depth == 0 {
                if let Some(x) = self.0.get(i..) {
                    if x.starts_with(self.1) {
                        let result = &self.0[..i];
                        self.0 = &self.0[i + self.1.len()..];
                        return Some(result);
                    }
                }
            }
        }
        let result = &self.0[..];
        self.0 = "";
        Some(result)
    }
}

pub fn parse_bool_expr(condition: &str) -> Result<BoolExpr, Error> {
    BoolExpr::parse(condition.as_bytes())
        .map_err(|e| e.into())
}

pub fn parse_int_expr(expr: &str) -> Result<IntExpr, Error> {
    IntExpr::parse(expr.as_bytes())
        .map_err(|e| e.into())
}

fn parse_int_expr_tuple(expr: &str, count: u8) -> Result<Vec<IntExpr>, Error> {
    let expr = expr.trim();
    if !expr.starts_with("(") || !expr.ends_with(")") {
        return Err(anyhow!("Expected braced list"));
    }
    let expr = &expr[1..expr.len() - 1];
    let result = brace_aware_split(expr, ",")
        .map(|x| x.trim())
        .map(|x| parse_int_expr(x))
        .collect::<Result<Vec<_>, Error>>()?;
    if result.len() != count as usize {
        return Err(anyhow!("Expected {} items, got {}", count, result.len()));
    }
    Ok(result)
}

fn parse_u32_tuple(expr: &str, count: u8) -> Result<Vec<u32>, Error> {
    let expr = expr.trim();
    if !expr.starts_with("(") || !expr.ends_with(")") {
        return Err(anyhow!("Expected braced list"));
    }
    let expr = &expr[1..expr.len() - 1];
    let result = brace_aware_split(expr, ",")
        .map(|x| x.trim())
        .map(|x| parse_u32(x))
        .collect::<Result<Vec<_>, Error>>()?;
    if result.len() != count as usize {
        return Err(anyhow!("Expected {} items, got {}", count, result.len()));
    }
    Ok(result)
}

fn parse_f32_tuple(expr: &str, count: u8) -> Result<Vec<f32>, Error> {
    let expr = expr.trim();
    if !expr.starts_with("(") || !expr.ends_with(")") {
        return Err(anyhow!("Expected braced list"));
    }
    let expr = &expr[1..expr.len() - 1];
    let result = brace_aware_split(expr, ",")
        .map(|x| x.trim())
        .map(|x| x.parse::<f32>().map_err(|x| x.into()))
        .collect::<Result<Vec<_>, Error>>()?;
    if result.len() != count as usize {
        return Err(anyhow!("Expected {} items, got {}", count, result.len()));
    }
    Ok(result)
}

fn parse_rally_order(val: &str) -> Result<RallyOrder, Error> {
    // Accept in form 'ground:{id}, unit:{id}', unit id can be also 'rclick'
    let val = val.trim();
    let error = || {
        anyhow!("Rally must be in format 'ground:<ID>, unit:<ID|rclick>', received '{}'", val)
    };
    let val = skip_str(val, "ground:")
        .ok_or_else(error)?;
    let comma_pos = val.find(",")
        .ok_or_else(error)?;
    let order_id = (&val[..comma_pos]).trim();
    let ground = OrderId(parse_u8(order_id).map_err(|_| error())?);
    let val = (&val[comma_pos + 1..]).trim_start();
    let val = skip_str(val, "unit:")
        .ok_or_else(error)?
        .trim();
    let unit = if val == "rclick" {
        OrderOrRclick::Rclick
    } else {
        let order = OrderId(parse_u8(val).map_err(|_| error())?);
        OrderOrRclick::Order(order)
    };
    Ok(RallyOrder {
        ground,
        unit,
    })
}

fn skip_str<'a>(val: &'a str, to_skip: &str) -> Option<&'a str> {
    if val.starts_with(to_skip) {
        Some(&val[to_skip.len()..])
    } else {
        None
    }
}

pub fn read_campaign(mut data: &[u8]) -> Result<Campaign, Error> {
    // Sections [zerg, terran, protoss, expzerg, expterran, expprotoss]
    // Each section has lines (ordering matters), in form one of
    // - map = name, mapid, race
    // - hidden = name, mapid, race
    // - cinematic = name, mapid, cinematic
    let ini = Ini::open(&mut data)
        .context("Unable to read ini")?;
    let mut result = (0..6).map(|_| {
        vec![bw::CampaignMission {
            name_index: 1,
            campaign_mission: 1,
            cinematic: 0,
            race: 1,
            hidden: 0,
        }, bw::CampaignMission {
            name_index: 0,
            campaign_mission: 0,
            cinematic: 0,
            race: 0,
            hidden: 0,
        }]
    }).collect::<Vec<Vec<bw::CampaignMission>>>();

    for section in &ini.sections {
        let index = match &*section.name {
            "zerg" => 0,
            "terran" => 1,
            "protoss" => 2,
            "expzerg" => 3,
            "expterran" => 4,
            "expprotoss" => 5,
            name => return Err(anyhow!("Invalid campaign name {}", name)),
        };
        let mut missions = Vec::with_capacity(10);
        for &(ref key, ref val) in &section.values {
            let mut tokens = val.split(",").map(|x| x.trim());
            let generic_error = || {
                anyhow!("Invalid setting format '{} = {}'", key, val)
            };
            match &**key {
                "map" | "hidden" => {
                    let name = tokens.next().ok_or_else(generic_error)?;
                    let map = tokens.next().ok_or_else(generic_error)?;
                    let race = tokens.next().ok_or_else(generic_error)?;
                    let name = parse_u16(name).context("Name")?;
                    let map = parse_u16(map).context("Map")?;
                    let race = parse_race(race).context("Race")?;
                    missions.push(bw::CampaignMission {
                        name_index: name,
                        campaign_mission: map,
                        cinematic: 0,
                        race,
                        hidden: if key == "hidden" { 1 } else { 0 },
                    });
                }
                "cinematic" | "hidden_cinematic" => {
                    let name = tokens.next().ok_or_else(generic_error)?;
                    let map = tokens.next().ok_or_else(generic_error)?;
                    let cinematic = tokens.next().ok_or_else(generic_error)?;
                    let name = parse_u16(name).context("Name")?;
                    let map = parse_u16(map).context("Map")?;
                    let cinematic = parse_u16(cinematic).context("Cinematic")?;
                    missions.push(bw::CampaignMission {
                        name_index: name,
                        campaign_mission: map,
                        cinematic,
                        race: 0,
                        hidden: if key == "hidden_cinematic" { 1 } else { 0 },
                    });
                }
                x => return Err(anyhow!("unknown campaign setting {}", x)),
            }
        }
        missions.push(bw::CampaignMission {
            name_index: 0,
            campaign_mission: 0,
            cinematic: 0,
            race: 0,
            hidden: 0,
        });
        result[index] = missions;
    }
    let mut campaigns = [std::ptr::null_mut(); 6];
    for (i, mut vec) in result.into_iter().enumerate() {
        campaigns[i] = vec.as_mut_ptr();
        std::mem::forget(vec);
    }
    Ok(Campaign {
        campaigns,
    })
}

pub fn set_config(config: Config) {
    *CONFIG.lock().unwrap() = Some(Arc::new(config));
}

pub fn set_campaign_ini(campaign: Campaign) {
    *CAMPAIGN.lock().unwrap() = Some(Arc::new(campaign));
}

pub fn config() -> Arc<Config> {
    CONFIG.lock().unwrap().as_ref().unwrap().clone()
}

pub fn campaign() -> Option<Arc<Campaign>> {
    CAMPAIGN.lock().unwrap().clone()
}

#[test]
fn test_parse_states() {
    assert_eq!(parse_state("disabled").unwrap(), State::Disabled);
    assert_eq!(
        parse_state("order(4, 5, 0x40)").unwrap(),
        State::Order(vec![OrderId(4), OrderId(5), OrderId(0x40)].into())
    );
    assert_eq!(parse_state("animation(4, 0x5)").unwrap(), State::IscriptAnim(vec![4, 5].into()));
}

#[test]
fn test_parse_rally_order() {
    assert_eq!(parse_rally_order("ground:4,unit:0x7").unwrap(), RallyOrder {
        ground: OrderId(4),
        unit: OrderOrRclick::Order(OrderId(7)),
    });
    assert_eq!(parse_rally_order("ground:0x40, unit:rclick").unwrap(), RallyOrder {
        ground: OrderId(0x40),
        unit: OrderOrRclick::Rclick,
    });
}

#[test]
fn test_brace_split() {
    let mut split = brace_aware_split("a, b, c", ",");
    assert_eq!(split.next().unwrap(), "a");
    assert_eq!(split.next().unwrap(), " b");
    assert_eq!(split.next().unwrap(), " c");
    assert_eq!(split.next(), None);

    let mut split = brace_aware_split(",a, b, c,", ",");
    assert_eq!(split.next().unwrap(), "");
    assert_eq!(split.next().unwrap(), "a");
    assert_eq!(split.next().unwrap(), " b");
    assert_eq!(split.next().unwrap(), " c");
    assert_eq!(split.next(), None);

    let mut split = brace_aware_split(" a s d ", ",");
    assert_eq!(split.next().unwrap(), " a s d ");
    assert_eq!(split.next(), None);

    let mut split = brace_aware_split("asd, (1, 2, 3), 45", ",");
    assert_eq!(split.next().unwrap(), "asd");
    assert_eq!(split.next().unwrap(), " (1, 2, 3)");
    assert_eq!(split.next().unwrap(), " 45");
    assert_eq!(split.next(), None);

    let mut split = brace_aware_split("asd, (1, 2, 3, 45", ",");
    assert_eq!(split.next().unwrap(), "asd");
    assert_eq!(split.next().unwrap(), " (1, 2, 3, 45");
    assert_eq!(split.next(), None);
}

#[test]
fn aura_parse_crash() {
    let text = br###"
[aura]
radius = 128
source_unit = 86
affected_players = 13
shield_regen = 256
source_condition = (upgrade(player, 57) == 2) && (ground_cooldown == 2)
target_condition = (unit_id == 65 || unit_id == 66 || unit_id == 67 || unit_id == 68 || unit_id == 70 || unit_id == 71 || unit_id == 72 || unit_id == 73 || unit_id == 77 || unit_id == 78 || unit_id == 79 || unit_id == 80 || unit_id == 81 || unit_id == 82 || unit_id == 83 || unit_id == 86 || unit_id == 87 || unit_id == 88 || unit_id == 92)

[aura]
radius = 128
source_unit = 87
affected_players = 13
shield_regen = 256
source_condition = (upgrade(player, 57) == 2) && (ground_cooldown == 2)
target_condition = (unit_id == 65 || unit_id == 66 || unit_id == 67 || unit_id == 68 || unit_id == 70 || unit_id == 71 || unit_id == 72 || unit_id == 73 || unit_id == 77 || unit_id == 78 || unit_id == 79 || unit_id == 80 || unit_id == 81 || unit_id == 82 || unit_id == 83 || unit_id == 86 || unit_id == 87 || unit_id == 88 || unit_id == 92)
    "###;
    let mut config = Config::default();
    config.update(text).unwrap();
}
