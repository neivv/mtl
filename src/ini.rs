use std::io::{BufRead, BufReader, Read};
use std::mem;

use failure::Error;

pub struct Ini {
    pub sections: Vec<Section>,
}

pub struct Section {
    pub name: String,
    pub values: Vec<(String, String)>,
}

impl Ini {
    pub fn open<R: Read>(file: R) -> Result<Ini, Error> {
        let reader = BufReader::new(file);
        let mut sections = Vec::new();
        let mut current_section = Vec::new();
        let mut current_section_name = String::new();
        for line in reader.lines() {
            let line = line?;
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if line.starts_with("[") && line.ends_with("]") {
                let new_name = (&line[1..line.len() - 1]).trim().into();
                let old_name = mem::replace(&mut current_section_name, new_name);
                if !current_section.is_empty() || !old_name.is_empty() {
                    sections.push(Section {
                        name: old_name,
                        values: mem::replace(&mut current_section, Vec::new()),
                    });
                }
            } else {
                if line.starts_with(";") || line.starts_with("#") || line.starts_with("//") {
                    // Comment
                } else {
                    let eq = match line.find("=") {
                        Some(s) => s,
                        None => return Err(format_err!("Invalid line \"{}\"", line)),
                    };
                    let key = (&line[..eq]).trim().into();
                    let value = (&line[eq + 1..]).trim().into();
                    current_section.push((key, value));
                }
            }
        }
        if !current_section.is_empty() || !current_section_name.is_empty() {
            sections.push(Section {
                name: current_section_name,
                values: current_section
            });
        }
        Ok(Ini {
            sections,
        })
    }
}

#[test]
fn test() {
    const TEXT: &str = r###"
[timers]
ensnare= 0x48
lockdown = 4800
hallucination_death = 100
unit_deaths = 2: 3000, 65: 250

[orders]
return_cargo_softcode = true
zerg_training = true


[upgrade.1.level.0]
; Rine
units = 0
; Stacks with the units.dat default regen (Which is 4)
hp_regen = 50

[upgrade.1.level.1]
; Rine, scv, lot, arbi
units = 0, 7, 0x41, 71
; Works, but can't kill units yet.
; Also currently buggy with burn overlays if the unit doesn't have native regeneration.
hp_regen = -10
; Stacks with default regen (7)
shield_regen = 50
; Stacks w/ default (8)
energy_regen = 40

[upgrade.1.level.1]
; Ghost
units = 1
state = self_cloaked
energy_regen = 40

[upgrade.1.level.1]
; Ghost
units = 1
state = arbiter_cloaked
energy_regen = -50

[upgrade.1.level.1]
; Hydra
units = 38
state = burrowed
hp_regen = 200

[upgrade.1.level.2]
; lot, arbi
units = 0x41, 71
cooldown = 10

[upgrade.1.level.2]
; rine
units = 0
cooldown = 100

[upgrade.3.level.1]
; hattu
units = 131
; Default 37
larva_timer = 130

[upgrade.3.level.2]
; hattu
units = 131
larva_timer = 6

[upgrade.3.level.2]
; scv
units = 7
; Default 75
mineral_harvest_time = 20
; Default 37
gas_harvest_time = 3

[upgrade.3.level.3]
; dropship
units = 11
; Default 15
; NOTE: Does not necessarily work all the time with AI
; Also doesn't work yet when clicking on status screen
unload_cooldown = 3

[upgrade.3.level.3]
; sunu
units = 146
; Default 15
creep_spread_timer = 1

[upgrade.15.level.1]
; Pylo
units = 156
; Note: Upgrade bonuses apply to completed units only if the state doesn't explicitly
; require incomplete
state = incomplete
shield_regen = 50

[upgrade.15.level.1]
; Cannon
units = 162
state = disabled
hp_regen = -40

[upgrade.15.level.1]
; Cannon
units = 162
; Note: All state requirements must match if there are multiple
state = disabled, incomplete
hp_regen = -400

[upgrade.15.level.2]
; Cannon
units = 162
state = damaged
hp_regen = -40

[upgrade.15.level.3]
; Cannon
units = 162
; Tower attack
state = order(0x13)
hp_regen = 400
"###;
    let read = TEXT.as_bytes();
    let ini = Ini::open(read).unwrap();
    assert_eq!(ini.sections[0].name, "timers");
    assert_eq!(ini.sections[1].name, "orders");
    assert_eq!(ini.sections[2].name, "upgrade.1.level.0");
    assert_eq!(ini.sections[3].name, "upgrade.1.level.1");
    assert_eq!(ini.sections[4].name, "upgrade.1.level.1");
    assert_eq!(ini.sections[5].name, "upgrade.1.level.1");
    assert_eq!(ini.sections[6].name, "upgrade.1.level.1");
    assert_eq!(ini.sections[7].name, "upgrade.1.level.2");
    assert_eq!(ini.sections[8].name, "upgrade.1.level.2");
    assert_eq!(ini.sections[9].name, "upgrade.3.level.1");
    assert_eq!(ini.sections[18].name, "upgrade.15.level.3");
    assert_eq!(ini.sections[8].values[0], ("units".to_string(), "0".to_string()));
    assert_eq!(ini.sections[8].values[1], ("cooldown".to_string(), "100".to_string()));
    assert_eq!(ini.sections[15].values[2], ("hp_regen".to_string(), "-40".to_string()));
    assert_eq!(
        ini.sections[16].values[1],
        ("state".to_string(), "disabled, incomplete".to_string())
    );
    assert_eq!(ini.sections[18].values[1], ("state".to_string(), "order(0x13)".to_string()));
    assert_eq!(ini.sections[18].values[2], ("hp_regen".to_string(), "400".to_string()));
}
