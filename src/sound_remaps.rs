use anyhow::{anyhow, Context, Error};

use bw_dat::Unit;
use fxhash::FxHashMap as HashMap;

use crate::bw;
use crate::config::{parse_u32};
use crate::expr::{BoolExpr, ExprExt, parse_bool_expr};
use crate::unit;

pub struct SoundRemaps {
    // !0 for no remap, !1 for conditional remaps
    simple: Vec<u32>,
    conditions: HashMap<u32, Vec<(u32, BoolExpr)>>,
}

impl SoundRemaps {
    pub fn new() -> SoundRemaps {
        SoundRemaps {
            simple: Vec::new(),
            conditions: HashMap::default(),
        }
    }

    pub fn parse_config(&mut self, values: &[(String, String)]) -> Result<(), Error> {
        for &(ref key, ref val) in values {
            let source = parse_u32(key)
                .with_context(|| format!("Invalid sound id \"{}\"", key))? as usize;
            let (val, condition) = match val.split_once(|x: char| x.is_whitespace()) {
                Some((val, rest)) => {
                    let rest = rest.trim();
                    if let Some(rest) = rest.strip_prefix("if ") {
                        let condition = parse_bool_expr(rest)
                            .with_context(|| format!("Invalid sound condition \"{}\"", rest))?;
                        (val, Some(condition))
                    } else {
                        return Err(anyhow!("Expected 'if' or nothing, got {rest}"));
                    }
                }
                None => (&**val, None),
            };
            let dest = parse_u32(val)
                .with_context(|| format!("Invalid sound id \"{}\"", val))?;
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
        Ok(())
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
