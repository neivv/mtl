use std::sync::{Arc, Mutex};

use failure::{Context, Error, Fail};
use ini::ini::Ini;
use unit::UnitId;

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
        } = *self;
        return_cargo_softcode
    }

    pub fn requires_secondary_order_hook(&self) -> bool {
        let Config {
            timers: _,
            supplies: _,
            return_cargo_softcode: _,
            zerg_building_training,
        } = *self;
        zerg_building_training
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

pub fn read_config(mut data: &[u8]) -> Result<Config, Error> {
    let error_invalid_field = |name: &'static str| {
        Context::new(format!("Invalid field {}", name))
    };

    let ini = Ini::read_from(&mut data)
        .map_err(|e| e.context("Unable to read ini"))?;
    let mut timers: Timers = Default::default();
    let mut return_cargo_softcode = false;
    let mut zerg_building_training = false;
    let mut supplies: Supplies = Default::default();
    if let Some(section) = ini.section(Some("timers")) {
        for (key, val) in section {
            match &**key {
                "hallucination_death" => {
                    u32_field(&mut timers.hallucination_death, val, "hallucination_death")?
                }
                "matrix" => u32_field(&mut timers.matrix, val, "matrix")?,
                "stim" => u32_field(&mut timers.stim, val, "stim")?,
                "ensnare" => u32_field(&mut timers.ensnare, val, "ensnare")?,
                "lockdown" => u32_field(&mut timers.lockdown, val, "lockdown")?,
                "irradiate" => u32_field(&mut timers.irradiate, val, "irradiate")?,
                "stasis" => u32_field(&mut timers.stasis, val, "stasis")?,
                "plague" => u32_field(&mut timers.plague, val, "plague")?,
                "maelstrom" => u32_field(&mut timers.maelstrom, val, "maelstrom")?,
                "acid_spores" => u32_field(&mut timers.acid_spores, val, "acid_spores")?,
                "unit_deaths" => {
                    for pair in val.split(",") {
                        let mut tokens = pair.split(":");
                        let unit_id = tokens.next()
                            .ok_or_else(|| error_invalid_field("timers.unit_deaths"))
                            .and_then(|x| {
                                parse_u16(x.trim())
                                    .map_err(|e| {
                                        let timer_index = timers.unit_deaths.len();
                                        e.context(format!(
                                            "timers.unit_deaths #{} unit id is not u16",
                                            timer_index
                                        ))
                                    })
                            })?;
                        let time = tokens.next()
                            .ok_or_else(|| error_invalid_field("timers.unit_deaths"))
                            .and_then(|x| {
                                parse_u32(x.trim())
                                    .map_err(|e| {
                                        let timer_index = timers.unit_deaths.len();
                                        e.context(format!(
                                            "timers.unit_deaths #{} time is not u32",
                                            timer_index
                                        ))
                                    })
                            })?;
                        timers.unit_deaths.push((UnitId(unit_id), time));
                    }
                }
                x => return Err(Context::new(format!("unknown key timers.{}", x)).into()),
            }
        }
    }
    if let Some(section) = ini.section(Some("supplies")) {
        for (key, val) in section {
            match &**key {
                "zerg_max" => u32_field(&mut supplies.zerg_max, val, "zerg_max")?,
                "terran_max" => u32_field(&mut supplies.terran_max, val, "terran_max")?,
                "protoss_max" => u32_field(&mut supplies.protoss_max, val, "protoss_max")?,
                x => return Err(Context::new(format!("unknown key supplies.{}", x)).into()),
            }
        }
    }
    if let Some(section) = ini.section(Some("orders")) {
        for (key, val) in section {
            match &**key {
                "return_cargo_softcode" => {
                    bool_field(&mut return_cargo_softcode, val, "return_cargo_softcode")?
                }
                "zerg_training" => {
                    bool_field(&mut zerg_building_training, val, "zerg_training")?
                }
                x => return Err(Context::new(format!("unknown key {}", x)).into()),
            }
        }
    }
    Ok(Config {
        timers,
        supplies,
        return_cargo_softcode,
        zerg_building_training,
    })
}

pub fn set_config(config: Config) {
    *CONFIG.lock().unwrap() = Some(Arc::new(config));
}

pub fn config() -> Arc<Config> {
    CONFIG.lock().unwrap().as_ref().unwrap().clone()
}
