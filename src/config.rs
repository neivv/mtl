use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use bw_dat::{UnitId, OrderId};
use failure::{Context, Error};
use ini::Ini;
use upgrades::{Upgrades, Upgrade, UpgradeChanges, State, Stat};

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
    pub return_cargo_softcode: bool,
    pub zerg_building_training: bool,
}

impl Config {
    pub fn requires_order_hook(&self) -> bool {
        let Config {
            timers: _,
            supplies: _,
            return_cargo_softcode,
            zerg_building_training: _,
            ref upgrades,
        } = *self;
        return_cargo_softcode || !upgrades.upgrades.is_empty()
    }

    pub fn requires_secondary_order_hook(&self) -> bool {
        let Config {
            timers: _,
            supplies: _,
            return_cargo_softcode: _,
            zerg_building_training,
            ref upgrades,
        } = *self;
        zerg_building_training || !upgrades.upgrades.is_empty()
    }
}

lazy_static! {
    static ref CONFIG: Mutex<Option<Arc<Config>>> = Mutex::new(None);
}

fn bool_field(out: &mut bool, value: &str, field: &'static str) -> Result<(), Error> {
    match value {
        "true" | "True" | "1" | "y" | "Y" => *out = true,
        "false" | "False" | "0" | "n" | "N" => *out = true,
        _ => {
            let msg = format!("Invalid value `{}` for bool {}", value, field);
            return Err(Context::new(msg).into());
        }
    }
    Ok(())
}

fn u32_field(out: &mut Option<u32>, value: &str, field_name: &'static str) -> Result<(), Error> {
    let value = parse_u32(value)
        .map_err(|e| e.context(format!("{} is not u32", field_name)))?;
    *out = Some(value);
    Ok(())
}

fn parse_i32(value: &str) -> Result<i32, Error> {
    Ok(if value.starts_with("0x") {
        parse_u32(value)? as i32
    } else {
        i32::from_str_radix(value, 10)?
    })
}

fn parse_u32(value: &str) -> Result<u32, Error> {
    Ok(if value.starts_with("0x") {
        u32::from_str_radix(&value[2..], 16)?
    } else {
        u32::from_str_radix(value, 10)?
    })
}

fn parse_u16(value: &str) -> Result<u16, Error> {
    Ok(if value.starts_with("0x") {
        u16::from_str_radix(&value[2..], 16)?
    } else {
        u16::from_str_radix(value, 10)?
    })
}

fn parse_u8(value: &str) -> Result<u8, Error> {
    Ok(if value.starts_with("0x") {
        u8::from_str_radix(&value[2..], 16)?
    } else {
        u8::from_str_radix(value, 10)?
    })
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
            if value.starts_with("order") {
                let start = value.find("(")
                    .ok_or_else(|| format_err!("Invalid syntax"))?;
                let end = value.rfind(")")
                    .ok_or_else(|| format_err!("Invalid syntax"))?;
                if start >= end {
                    return Err(format_err!("Invalid syntax"));
                }
                let text = (&value[start + 1..end]).trim();
                State::Order(OrderId(parse_u8(text)?))
            } else {
                return Err(format_err!("Unknown state {}", value));
            }
        }
    })
}

fn parse_stat(key: &str, value: &str) -> Result<Stat, Error> {
    Ok(match key {
        "hp_regen" => Stat::HpRegen(parse_i32(value)?),
        "shield_regen" => Stat::ShieldRegen(parse_i32(value)?),
        "energy_regen" => Stat::EnergyRegen(parse_i32(value)?),
        "cooldown" => Stat::Cooldown(parse_u8(value)?),
        "larva_timer" => Stat::LarvaTimer(parse_u8(value)?),
        "mineral_harvest_time" => Stat::MineralHarvestTime(parse_u8(value)?),
        "gas_harvest_time" => Stat::GasHarvestTime(parse_u8(value)?),
        "unload_cooldown" => Stat::UnloadCooldown(parse_u8(value)?),
        "creep_spread_timer" => Stat::CreepSpreadTimer(parse_u8(value)?),
        _ => return Err(format_err!("Unknown stat {}", key)),
    })
}

pub fn read_config(mut data: &[u8]) -> Result<Config, Error> {
    let error_invalid_field = |name: &'static str| {
        format_err!("Invalid field {}", name)
    };

    let ini = Ini::open(&mut data)
        .map_err(|e| e.context("Unable to read ini"))?;
    let mut timers: Timers = Default::default();
    let mut return_cargo_softcode = false;
    let mut zerg_building_training = false;
    let mut supplies: Supplies = Default::default();
    let mut upgrades: BTreeMap<u8, BTreeMap<Vec<State>, Vec<UpgradeChanges>>> = BTreeMap::new();
    for section in &ini.sections {
        let name = &section.name;
        if name == "timers" {
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
                                .ok_or_else(|| error_invalid_field("timers.unit_deaths"))?;
                            let unit_id = parse_u16(unit_id.trim()).map_err(|e| {
                                let timer_index = timers.unit_deaths.len();
                                e.context(format!(
                                    "timers.unit_deaths #{} unit id is not u16",
                                    timer_index
                                ))
                            })?;
                            let time = tokens.next()
                                .ok_or_else(|| error_invalid_field("timers.unit_deaths"))?;
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
                    x => return Err(Context::new(format!("unknown key timers.{}", x)).into()),
                }
            }
        } else if name == "supplies" {
            for &(ref key, ref val) in &section.values {
                match &**key {
                    "zerg_max" => u32_field(&mut supplies.zerg_max, &val, "zerg_max")?,
                    "terran_max" => u32_field(&mut supplies.terran_max, &val, "terran_max")?,
                    "protoss_max" => u32_field(&mut supplies.protoss_max, &val, "protoss_max")?,
                    x => return Err(Context::new(format!("unknown key supplies.{}", x)).into()),
                }
            }
        } else if name == "orders" {
            for &(ref key, ref val) in &section.values {
                match &**key {
                    "return_cargo_softcode" => {
                        bool_field(&mut return_cargo_softcode, &val, "return_cargo_softcode")?
                    }
                    "zerg_training" => {
                        bool_field(&mut zerg_building_training, &val, "zerg_training")?
                    }
                    x => return Err(Context::new(format!("unknown key {}", x)).into()),
                }
            }
        } else if name.starts_with("upgrade") {
            let mut tokens = name.split(".").skip(1);
            let generic_error = || {
                format_err!(
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
                .map_err(|e| e.context(format!("Invalid upgrade id \"{}\"", id)))?;
            let level = parse_u8(level)
                .map_err(|e| e.context(format!("Invalid upgrade level \"{}\"", level)))?;

            let mut units = Vec::new();
            let mut states = Vec::new();
            let mut stats = Vec::new();
            for &(ref key, ref val) in &section.values {
                match &**key {
                    "units" => {
                        for tok in val.split(",").map(|x| x.trim()) {
                            let id = parse_u16(tok)
                                .map_err(|e| e.context(format!("{} units", name)))?;
                            units.push(UnitId(id));
                        }
                    }
                    "state" => {
                        for tok in val.split(",").map(|x| x.trim()) {
                            let state = parse_state(tok)
                                .map_err(|e| e.context(format!("{} state", name)))?;
                            states.push(state);
                        }
                    }
                    x => {
                        let stat = parse_stat(x, &val)
                            .map_err(|e| e.context(format!("In {}:{}", name, x)))?;
                        stats.push(stat);
                    }
                }
            }
            if units.is_empty() {
                return Err(format_err!("{} did not specify any units", name));
            }
            let changes = UpgradeChanges {
                units,
                level,
                changes: stats,
            };
            upgrades.entry(id).or_insert_with(|| Default::default())
                .entry(states).or_insert_with(|| Default::default())
                .push(changes);
        } else {
            return Err(format_err!("Unknown section name \"{}\"", name));
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
    let upgrades = Upgrades {
        upgrades,
    };
    Ok(Config {
        timers,
        supplies,
        return_cargo_softcode,
        zerg_building_training,
        upgrades,
    })
}

pub fn set_config(config: Config) {
    *CONFIG.lock().unwrap() = Some(Arc::new(config));
}

pub fn config() -> Arc<Config> {
    CONFIG.lock().unwrap().as_ref().unwrap().clone()
}
