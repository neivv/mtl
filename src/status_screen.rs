use std::io::Write;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use arrayvec::ArrayVec;
use anyhow::{anyhow, Context, Error};

use bw_dat::dialog::{Control, Dialog};
use bw_dat::expr::{BoolExpr};
use bw_dat::{Game, Unit, UnitId, WeaponId, unit, upgrade};

use crate::bw;
use crate::config::{self, Config};
use crate::string_tables::{self, StringTable};
use crate::tooltip::{self, TooltipDrawFunc};

static STATUS_SCREEN_DIALOG: AtomicUsize = AtomicUsize::new(0);
static TOOLTIP_TEXT_BUFFER: AtomicUsize = AtomicUsize::new(0);

pub fn status_screen_dialog_created(dialog: Dialog) {
    STATUS_SCREEN_DIALOG.store(*dialog as usize, Ordering::Relaxed);
}

pub unsafe fn draw_tooltip_hook(config: &Config, ctrl: Control, orig: TooltipDrawFunc) -> bool {
    let status_screen_dialog = STATUS_SCREEN_DIALOG.load(Ordering::Relaxed) as *mut bw::Dialog;
    if *ctrl.dialog() != status_screen_dialog {
        return false;
    }
    // Stat tooltips only
    if !matches!(ctrl.id(), 0x9 ..= 0xc) {
        return false;
    }
    let ptr = ctrl.user_pointer() as *mut bw::StatusScreenStat;
    if ptr.is_null() {
        return false;
    }
    let selected = match bw::client_selection().get(0).and_then(|&x| Unit::from_ptr(x)) {
        Some(s) => s,
        None => return false,
    };
    let mut weapon = None;
    let game = crate::game::get();
    let stat_txt = string_tables::stat_txt();
    let tooltip = match (*ptr).stat_type {
        0 => {
            let weapon_id = WeaponId((*ptr).id);
            weapon = Some(weapon_id);
            config.status_screen_tooltips.get_weapon(game, selected, weapon_id)
        }
        1 => config.status_screen_tooltips.get_armor(game, selected),
        2 => config.status_screen_tooltips.get_shields(game, selected),
        3 | _ => config.status_screen_tooltips.get_special(game, selected),
    };
    let tooltip = match tooltip {
        Some(s) => s,
        None => return false,
    };
    let mut buffer: ArrayVec<[u8; 512]> = ArrayVec::new();
    tooltip.format(game, &stat_txt, selected, weapon, &mut buffer);
    let _ = buffer.push(0);
    let len = buffer.len();
    buffer[len - 1] = 0;
    tooltip::set_text_hook_mode(2);
    TOOLTIP_TEXT_BUFFER.store(buffer.as_ptr() as usize, Ordering::Relaxed);
    orig(*ctrl);
    TOOLTIP_TEXT_BUFFER.store(0, Ordering::Relaxed);
    tooltip::set_text_hook_mode(0);
    true
}

pub fn tooltip_text_hook() -> *const u8 {
    let ptr = TOOLTIP_TEXT_BUFFER.load(Ordering::Relaxed) as *const u8;
    assert!(!ptr.is_null());
    ptr
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum UnitVariable {
    BaseArmor,
    ArmorUpgrade,
    CurrentArmorBonus,
    CurrentShieldBonus,
    ArmorType,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum WeaponVariable {
    WeaponName,
    Damage(u8),
    DamageUpgrade,
    UpgradeBonus(u8),
    DamageType,
    CurrentDamageBonus(u8),
    Cooldown,
}

#[derive(Eq, PartialEq, Debug)]
enum TooltipPart {
    Text(String),
    Unit(UnitVariable, Option<UnitId>),
    Weapon(WeaponVariable, Option<WeaponId>),
}

struct Tooltip {
    parts: Vec<TooltipPart>,
}

struct TooltipGroup {
    unconditional: Option<Arc<Tooltip>>,
    conditional: Vec<(Arc<Tooltip>, Arc<BoolExpr>)>,
}

pub struct Tooltips {
    weapon: TooltipGroup,
    weapon_by_unit: Vec<Option<Box<TooltipGroup>>>,
    weapon_by_weapon: Vec<Option<Box<TooltipGroup>>>,
    armor: TooltipGroup,
    armor_by_unit: Vec<Option<Box<TooltipGroup>>>,
    shields: TooltipGroup,
    shields_by_unit: Vec<Option<Box<TooltipGroup>>>,
    special: TooltipGroup,
    special_by_unit: Vec<Option<Box<TooltipGroup>>>,
}

impl Tooltips {
    pub fn new() -> Tooltips {
        Tooltips {
            weapon: TooltipGroup::empty(),
            weapon_by_unit: Vec::new(),
            weapon_by_weapon: Vec::new(),
            armor: TooltipGroup::empty(),
            armor_by_unit: Vec::new(),
            shields: TooltipGroup::empty(),
            shields_by_unit: Vec::new(),
            special: TooltipGroup::empty(),
            special_by_unit: Vec::new(),
        }
    }

    pub fn has_any(&self) -> bool {
        self.weapon.has_any() ||
            self.armor.has_any() ||
            self.shields.has_any() ||
            self.special.has_any() ||
            !self.weapon_by_unit.is_empty() ||
            !self.weapon_by_weapon.is_empty() ||
            !self.armor_by_unit.is_empty() ||
            !self.shields_by_unit.is_empty() ||
            !self.special_by_unit.is_empty()
    }

    pub fn add_weapon(&mut self, config: &[(String, String)]) -> Result<(), Error> {
        config_add(
            config,
            &mut self.weapon,
            &mut self.weapon_by_unit,
            Some(&mut self.weapon_by_weapon),
        )
    }

    pub fn add_armor(&mut self, config: &[(String, String)]) -> Result<(), Error> {
        config_add(config, &mut self.armor, &mut self.armor_by_unit, None)
    }

    pub fn add_shields(&mut self, config: &[(String, String)]) -> Result<(), Error> {
        config_add(config, &mut self.shields, &mut self.shields_by_unit, None)
    }

    pub fn add_special(&mut self, config: &[(String, String)]) -> Result<(), Error> {
        config_add(config, &mut self.special, &mut self.special_by_unit, None)
    }

    fn get_weapon(&self, game: Game, unit: Unit, weapon: WeaponId) -> Option<&Tooltip> {
        self.weapon_by_weapon.get(weapon.0 as usize)
            .and_then(|x| x.as_ref())
            .and_then(|x| x.get(game, unit))
            .or_else(|| {
                self.weapon_by_unit.get(unit.id().0 as usize)
                    .and_then(|x| x.as_ref())
                    .and_then(|x| x.get(game, unit))
            })
            .or_else(|| {
                self.weapon.get(game, unit)
            })
    }

    fn get_armor(&self, game: Game, unit: Unit) -> Option<&Tooltip> {
        self.armor_by_unit.get(unit.id().0 as usize)
            .and_then(|x| x.as_ref())
            .and_then(|x| x.get(game, unit))
            .or_else(|| {
                self.armor.get(game, unit)
            })
    }

    fn get_shields(&self, game: Game, unit: Unit) -> Option<&Tooltip> {
        self.shields_by_unit.get(unit.id().0 as usize)
            .and_then(|x| x.as_ref())
            .and_then(|x| x.get(game, unit))
            .or_else(|| {
                self.shields.get(game, unit)
            })
    }

    fn get_special(&self, game: Game, unit: Unit) -> Option<&Tooltip> {
        self.special_by_unit.get(unit.id().0 as usize)
            .and_then(|x| x.as_ref())
            .and_then(|x| x.get(game, unit))
            .or_else(|| {
                self.special.get(game, unit)
            })
    }
}

fn config_add(
    config: &[(String, String)],
    default: &mut TooltipGroup,
    by_unit: &mut Vec<Option<Box<TooltipGroup>>>,
    by_weapon: Option<&mut Vec<Option<Box<TooltipGroup>>>>,
) -> Result<(), Error> {
    let mut units = Vec::new();
    let mut weapons = Vec::new();
    let mut condition = None;
    let mut text = None;
    for &(ref key, ref val) in config {
        match &**key {
            "units" => {
                units = config::parse_unit_list(val)?;
            }
            "weapons" => {
                if by_weapon.is_none() {
                    return Err(anyhow!("'weapons' is not applicable"));
                }
                weapons = config::parse_weapon_list(val)?;
            }
            "condition" => {
                condition = Some(Arc::new(config::parse_bool_expr(val)?));
            }
            "text" => {
                text = Some(&**val);
            }
            _ => return Err(anyhow!("Unexpected '{}'", val)),
        }
    }
    let text = match text {
        Some(s) => s,
        None => return Err(anyhow!("Status screen tooltip had no text")),
    };
    let tooltip = Tooltip::parse(text)
        .with_context(|| format!("Parsing '{}'", text))?;
    let tooltip = Arc::new(tooltip);
    match (units.is_empty(), weapons.is_empty()) {
        (false, false) => {
            return Err(anyhow!("Cannot have both unit and weapon -specific tooltip at once"));
        }
        (true, true) => {
            add_tooltip(default, &tooltip, &condition);
        }
        (false, true) => {
            for unit_id in units {
                let index = unit_id.0 as usize;
                if by_unit.len() <= index {
                    by_unit.resize_with(index + 1, || None);
                }
                let group = by_unit[index].get_or_insert_with(|| {
                    Box::new(TooltipGroup::empty())
                });
                add_tooltip(group, &tooltip, &condition);
            }
        }
        (true, false) => {
            if let Some(by_weapon) = by_weapon {
                for weapon_id in weapons {
                    let index = weapon_id.0 as usize;
                    if by_weapon.len() <= index {
                        by_weapon.resize_with(index + 1, || None);
                    }
                    let group = by_weapon[index].get_or_insert_with(|| {
                        Box::new(TooltipGroup::empty())
                    });
                    add_tooltip(group, &tooltip, &condition);
                }
            }
        }
    }
    Ok(())
}

fn add_tooltip(
    group: &mut TooltipGroup,
    tooltip: &Arc<Tooltip>,
    condition: &Option<Arc<BoolExpr>>,
) {
    if let Some(condition) = condition {
        group.conditional.push((tooltip.clone(), condition.clone()));
    } else {
        group.unconditional = Some(tooltip.clone());
    }
}

impl TooltipGroup {
    pub fn empty() -> TooltipGroup {
        TooltipGroup {
            unconditional: None,
            conditional: Vec::new(),
        }
    }

    pub fn has_any(&self) -> bool {
        self.unconditional.is_some() || !self.conditional.is_empty()
    }

    pub fn get(&self, game: Game, unit: Unit) -> Option<&Tooltip> {
        // Go through conditionals in reverse order,
        // which lets map-specific settings override globals.
        for (tooltip, cond) in self.conditional.iter().rev() {
            if cond.eval_with_unit(unit, game) {
                return Some(&**tooltip);
            }
        }
        self.unconditional.as_ref().map(|arc| &**arc)
    }
}

impl Tooltip {
    pub fn parse(mut text: &str) -> Result<Tooltip, Error> {
        fn add_text(parts: &mut Vec<TooltipPart>, text: &str) {
            // If the last part was already text merges with that
            let last_text = parts.last_mut()
                .and_then(|part| match part {
                    TooltipPart::Text(ref mut text) => Some(text),
                    _ => None,
                });
            if let Some(last) = last_text {
                last.push_str(text);
            } else {
                parts.push(TooltipPart::Text(text.into()));
            }
        }
        let mut parts = Vec::new();
        while !text.is_empty() {
            let bytes = text.as_bytes();
            let next_special = bytes.iter().position(|&x| match x {
                b'{' | b'}' | b'\\' => true,
                _ => false,
            });
            match next_special {
                Some(0) => (),
                Some(s) => {
                    add_text(&mut parts, &text[..s]);
                    text = &text[s..];
                }
                None => {
                    add_text(&mut parts, text);
                    break;
                }
            }
            if text.len() < 2 {
                add_text(&mut parts, text);
                break;
            }
            let bytes = text.as_bytes();
            match bytes[0] {
                b'{' => {
                    if bytes[1] == b'{' {
                        add_text(&mut parts, "{");
                        text = &text[2..];
                    } else {
                        let end = match bytes.iter().position(|&x| x == b'}') {
                            Some(s) => s,
                            None => {
                                return Err(anyhow!("Unmatched {. Use {{ if you want the text to have a single {"));
                            }
                        };
                        let var = &text[1..end];
                        let var = parse_var(var)?;
                        parts.push(var);
                        text = &text[end + 1..];
                    }
                }
                b'}' => {
                    if bytes[1] == b'}' {
                        add_text(&mut parts, "}");
                        text = &text[2..];
                    } else {
                        return Err(anyhow!("Unmatched }. Use }} if you want the text to have a single }"));
                    }
                }
                b'\\' | _ => {
                    match bytes[1] {
                        b'\\' => add_text(&mut parts, "\\"),
                        b'n' => add_text(&mut parts, "\n"),
                        b'x' => {
                            if let Some(x) = text.get(2..4) {
                                if let Ok(val) = u8::from_str_radix(x, 16) {
                                    if let Ok(ascii) = std::str::from_utf8(&[val]) {
                                        add_text(&mut parts, ascii);
                                        text = &text[2..];
                                    }
                                }
                            }
                        }
                        _ => add_text(&mut parts, &text[..2]),
                    }
                    text = &text[2..];
                }
            }
        }
        Ok(Tooltip {
            parts,
        })
    }

    pub fn format<W: Write>(
        &self,
        game: Game,
        stat_txt: &StringTable,
        unit: Unit,
        weapon: Option<WeaponId>,
        out: &mut W,
    ) {
        for part in &self.parts {
            match part {
                TooltipPart::Text(text) => {
                    let _ = out.write_all(text.as_bytes());
                }
                TooltipPart::Unit(var, id) => {
                    let id = id.unwrap_or_else(|| unit.id());
                    match var {
                        UnitVariable::BaseArmor => {
                            let _ = write!(out, "{}", id.armor());
                        }
                        UnitVariable::ArmorUpgrade => {
                            let upgrade_name = id.armor_upgrade()
                                .and_then(|upgrade| stat_txt.by_index(upgrade.label() as u16))
                                .unwrap_or("???");
                            let _ = write!(out, "{}", upgrade_name);
                        }
                        UnitVariable::CurrentArmorBonus => {
                            let mut level = id.armor_upgrade()
                                .map(|upgrade| game.upgrade_level(unit.player(), upgrade))
                                .unwrap_or(0);
                            let ultralisk_upgrade = {
                                if id == unit::ULTRALISK {
                                    game.upgrade_level(
                                        unit.player(),
                                        upgrade::CHITINOUS_PLATING,
                                    ) != 0
                                } else {
                                    id == unit::TORRASQUE
                                }
                            };
                            if ultralisk_upgrade {
                                level = level.saturating_add(2);
                            }
                            if level != 0 {
                                let _ = write!(out, "+{}", level);
                            }
                        }
                        UnitVariable::CurrentShieldBonus => {
                            let level = game.upgrade_level(
                                unit.player(),
                                upgrade::PLASMA_SHIELDS,
                            );
                            if level != 0 {
                                let _ = write!(out, "+{}", level);
                            }
                        }
                        UnitVariable::ArmorType => {
                            let string = match id.armor_type() {
                                1 => "Small",
                                2 => "Medium",
                                3 => "Large",
                                _ => "???",
                            };
                            let _ = write!(out, "{}", string);
                        }
                    }
                }
                TooltipPart::Weapon(var, id) => {
                    let id = match id.or_else(|| weapon) {
                        Some(s) => s,
                        None => return,
                    };
                    match *var {
                        WeaponVariable::WeaponName => {
                            let name = stat_txt.by_index(id.label() as u16)
                                .unwrap_or("???");
                            let _ = write!(out, "{}", name);
                        }
                        WeaponVariable::Damage(factor) => {
                            let factor = Some(factor as u32).filter(|&f| f != 0)
                                .unwrap_or_else(|| id.factor());
                            let _ = write!(out, "{}", id.damage().saturating_mul(factor));
                        }
                        WeaponVariable::DamageUpgrade => {
                            let upgrade_name = id.upgrade()
                                .and_then(|upgrade| stat_txt.by_index(upgrade.label() as u16))
                                .unwrap_or("???");
                            let _ = write!(out, "{}", upgrade_name);
                        }
                        WeaponVariable::UpgradeBonus(factor) => {
                            let factor = Some(factor as u32).filter(|&f| f != 0)
                                .unwrap_or_else(|| id.factor());
                            let _ = write!(out, "{}", id.bonus().saturating_mul(factor));
                        }
                        WeaponVariable::DamageType => {
                            let string = match id.damage_type() {
                                1 => "Explosive",
                                2 => "Concussive",
                                3 => "Normal",
                                4 => "Ignores armor",
                                _ => "???",
                            };
                            let _ = write!(out, "{}", string);
                        }
                        WeaponVariable::CurrentDamageBonus(factor) => {
                            let level = id.upgrade()
                                .map(|upgrade| game.upgrade_level(unit.player(), upgrade))
                                .unwrap_or(0);
                            if level != 0 {
                                let factor = Some(factor as u32).filter(|&f| f != 0)
                                    .unwrap_or_else(|| id.factor());
                                let total = id.bonus().saturating_mul(level.into())
                                    .saturating_mul(factor);
                                let _ = write!(out, "+{}", total);
                            }
                        }
                        WeaponVariable::Cooldown => {
                            let _ = write!(out, "{}", id.cooldown());
                        }
                    }
                }
            }
        }
    }
}

fn parse_var(var: &str) -> Result<TooltipPart, Error> {
    enum Var {
        Weapon(WeaponVariable),
        Unit(UnitVariable),
    }
    let (name, id) = match var.as_bytes().iter().position(|&x| x == b'@') {
        Some(s) => (&var[..s], Some(config::parse_u16(&var[s + 1..])?)),
        None => (var, None)
    };
    let (name, factor) = match name.as_bytes().iter().position(|&x| x == b'*') {
        Some(s) => (&name[..s], config::parse_u8(&name[s + 1..])?),
        None => (name, 0),
    };
    let result = match name {
        "weapon_name" => Var::Weapon(WeaponVariable::WeaponName),
        "damage" => Var::Weapon(WeaponVariable::Damage(factor)),
        "damage_type" => Var::Weapon(WeaponVariable::DamageType),
        "base_weapon_upgrade_bonus" => Var::Weapon(WeaponVariable::UpgradeBonus(factor)),
        "weapon_upgrade_bonus" => Var::Weapon(WeaponVariable::CurrentDamageBonus(factor)),
        "weapon_upgrade_name" => Var::Weapon(WeaponVariable::DamageUpgrade),
        "cooldown" => Var::Weapon(WeaponVariable::Cooldown),
        "armor_upgrade_name" => Var::Unit(UnitVariable::ArmorUpgrade),
        "base_armor" => Var::Unit(UnitVariable::BaseArmor),
        "armor_upgrade_bonus" => Var::Unit(UnitVariable::CurrentArmorBonus),
        "shield_upgrade_bonus" => Var::Unit(UnitVariable::CurrentShieldBonus),
        "armor_type" => Var::Unit(UnitVariable::ArmorType),
        _ => return Err(anyhow!("Unknown format variable '{}'", var)),
    };
    match result {
        Var::Unit(var) => Ok(TooltipPart::Unit(var, id.map(|x| UnitId(x)))),
        Var::Weapon(var) => Ok(TooltipPart::Weapon(var, id.map(|x| WeaponId(x)))),
    }
}

#[test]
fn tooltip_escapes() {
    let tooltip = Tooltip::parse("asd k asf\\n=)\\n").unwrap();
    assert_eq!(tooltip.parts, vec![TooltipPart::Text("asd k asf\n=)\n".into())]);

    let tooltip = Tooltip::parse(r#"asd\x02 k \asf\\n=)\n\"#).unwrap();
    assert_eq!(tooltip.parts, vec![TooltipPart::Text("asd\x02 k \\asf\\n=)\n\\".into())]);

    let tooltip = Tooltip::parse(r#"asd {{}}\n}}"#).unwrap();
    assert_eq!(tooltip.parts, vec![TooltipPart::Text("asd {}\n}".into())]);
}

#[test]
fn tooltip_vars() {
    let tooltip = Tooltip::parse("Damage: {damage}{damage*5}+{cooldown}, {base_armor}").unwrap();
    assert_eq!(tooltip.parts, vec![
        TooltipPart::Text("Damage: ".into()),
        TooltipPart::Weapon(WeaponVariable::Damage(0), None),
        TooltipPart::Weapon(WeaponVariable::Damage(5), None),
        TooltipPart::Text("+".into()),
        TooltipPart::Weapon(WeaponVariable::Cooldown, None),
        TooltipPart::Text(", ".into()),
        TooltipPart::Unit(UnitVariable::BaseArmor, None),
    ]);

    let tooltip = Tooltip::parse("{damage_type}").unwrap();
    assert_eq!(tooltip.parts, vec![
        TooltipPart::Weapon(WeaponVariable::DamageType, None),
    ]);

    let tooltip = Tooltip::parse("{damage_type@50}, {base_armor@1}").unwrap();
    assert_eq!(tooltip.parts, vec![
        TooltipPart::Weapon(WeaponVariable::DamageType, Some(WeaponId(50))),
        TooltipPart::Text(", ".into()),
        TooltipPart::Unit(UnitVariable::BaseArmor, Some(UnitId(1))),
    ]);
}
