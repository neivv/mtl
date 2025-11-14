use anyhow::{anyhow, Context, Error};

use bw_dat::Unit;
use fxhash::FxHashMap as HashMap;

use crate::bw;
use crate::config::{ConfigNeedReinit, parse_u32};
use crate::expr::{BoolExpr, ExprExt, parse_bool_expr};
use crate::unit;
use crate::string_tables;

/// Also used for info msg remaps
pub struct SoundRemaps {
    // !0 for no remap, !1 for conditional remaps
    simple: Vec<u32>,
    conditions: HashMap<u32, Vec<(u32, BoolExpr)>>,
    name: &'static str,
}

impl SoundRemaps {
    pub fn new(name: &'static str) -> SoundRemaps {
        SoundRemaps {
            simple: Vec::new(),
            conditions: HashMap::default(),
            name,
        }
    }

    pub fn parse_config(
        &mut self,
        values: &[(String, String)],
    ) -> Result<ConfigNeedReinit, Error> {
        let mut need_reinit = ConfigNeedReinit::No;
        for &(ref key, ref val) in values {
            let source = self.parse_u32(key, &mut need_reinit)? as usize;
            let (val, condition) = match val.split_once(|x: char| x.is_whitespace()) {
                Some((val, rest)) => {
                    let rest = rest.trim();
                    if let Some(rest) = rest.strip_prefix("if ") {
                        let condition = parse_bool_expr(rest)
                            .with_context(|| {
                                format!("Invalid {} condition \"{}\"", self.name, rest)
                            })?;
                        (val, Some(condition))
                    } else {
                        return Err(anyhow!("Expected 'if' or nothing, got {rest}"));
                    }
                }
                None => (&**val, None),
            };
            let dest = self.parse_u32(val, &mut need_reinit)?;
            if self.simple.len() <= source {
                self.simple.resize_with(source + 1, || !0);
            }
            if let Some(condition) = condition {
                self.simple[source] = !1;
                self.conditions.entry(source as u32).or_default().push((dest, condition));
            } else {
                self.simple[source] = dest;
            }
        }
        Ok(need_reinit)
    }

    fn parse_u32(&self, val: &str, need_reinit: &mut ConfigNeedReinit) -> Result<u32, Error> {
        let result = match parse_u32(val) {
            Ok(o) => Ok(o),
            Err(e) => {
                if self.name == "message" {
                    // Lookup stat_txt
                    let stat_txt = string_tables::stat_txt();
                    if !stat_txt.is_inited() {
                        *need_reinit = ConfigNeedReinit::Yes;
                        return Ok(0);
                    }
                    if let Some(val) = stat_txt.key_to_index(val.as_bytes()) {
                        return Ok(val);
                    }
                } else {
                    // Could lookup sfx.json but lazy
                }
                Err(e)
            }
        };
        result.map_err(|_| anyhow!("Invalid {} id \"{}\"", self.name, val))
    }

    pub fn remap(&self, sound: u32, unit: Option<Unit>) -> u32 {
        let simple = self.simple.get(sound as usize)
            .copied()
            .unwrap_or(!0);
        if simple == !0 {
            sound
        } else if simple == !1 {
            if let Some(sounds) = self.conditions.get(&sound) {
                let game = unsafe { bw_dat::Game::from_ptr(bw::game()) };
                let unit = match unit {
                    Some(s) => s,
                    None => {
                        // Use any unit for local player (Assuming advisor sounds etc, and
                        // that people want to use `player` as condition anyway)
                        let local_player_id = bw::local_player_id();
                        match unit::first_player_unit(local_player_id) {
                            Some(s) => s,
                            // Just don't remap the sound
                            None => return sound,
                        }
                    }
                };
                // Reverse to prioritize latest rules
                // Not sure if that makes sense but probably better to have map
                // config override global config at least?
                for &(result, ref condition) in sounds.iter().rev() {
                    if condition.eval_unit(unit, game) {
                        return result;
                    }
                }
            }
            sound
        } else {
            simple
        }
    }
}
