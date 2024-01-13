use anyhow::{anyhow, Error};
use bw_dat::{Unit, Game};
use bw_dat::expr::{self, Expr, CustomBoolExpr, CustomIntExpr};

use crate::bw;
use crate::samase;

pub type IntExpr = CustomIntExpr<ExprState>;
pub type IntExprTree = bw_dat::expr::IntExprTree<ExprState>;
pub type BoolExpr = CustomBoolExpr<ExprState>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ExprState;

impl expr::CustomState for ExprState {
    type IntExt = Int;
    type BoolExt = Bool;
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Int {
    MaxEnergy,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Bool {
}

struct ExprParser {
}

impl expr::CustomParser for ExprParser {
    type State = ExprState;
    fn parse_int<'a>(&mut self, input: &'a [u8]) -> Option<(Int, &'a [u8])> {
        if let Some(rest) = input.strip_prefix(b"max_energy") {
            return Some((Int::MaxEnergy, rest));
        }
        None
    }

    fn parse_operator<'a>(&mut self, _input: &'a [u8]) -> Option<(u16, &'a [u8])> {
        None
    }

    fn apply_operator(
        &mut self,
        _left: Expr<Self::State>,
        _right: Expr<Self::State>,
        _oper: u16,
    ) -> Result<Expr<Self::State>, &'static str> {
        Err("")
    }

    fn parse_bool<'a>(&mut self, _input: &'a [u8]) -> Option<(Bool, &'a [u8])> {
        None
    }
}

pub fn parse_int_expr(text: &str) -> Result<IntExpr, Error> {
    let mut parser = ExprParser {
    };
    CustomIntExpr::parse_part_custom(text.as_bytes(), &mut parser)
        .map_err(|e| e.into())
        .and_then(|x| {
            if x.1.iter().any(|&x| !x.is_ascii_whitespace()) {
                Err(anyhow!("Failed to parse expression at '{}'", String::from_utf8_lossy(x.1)))
            } else {
                Ok(x.0)
            }
        })
}

pub fn parse_bool_expr(text: &str) -> Result<BoolExpr, Error> {
    let mut parser = ExprParser {
    };
    CustomBoolExpr::parse_part_custom(text.as_bytes(), &mut parser)
        .map_err(|e| e.into())
        .and_then(|x| {
            if x.1.iter().any(|&x| !x.is_ascii_whitespace()) {
                Err(anyhow!("Failed to parse expression at '{}'", String::from_utf8_lossy(x.1)))
            } else {
                Ok(x.0)
            }
        })
}

pub trait ExprExt {
    type Ret;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> Self::Ret;
}

impl ExprExt for BoolExpr {
    type Ret = bool;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> bool {
        let mut ctx = bw_dat::expr::EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: if self.required_context()
                .contains(bw_dat::expr::RequiredContext::MAP_TILE_FLAGS)
            {
                Some(bw::map_tile_flags())
            } else {
                None
            },
            custom: CustomCtx {
                unit: Some(unit),
            },
        };
        ctx.eval_bool(self)
    }
}

impl ExprExt for IntExpr {
    type Ret = i32;
    fn eval_unit(&self, unit: bw_dat::Unit, game: Game) -> i32 {
        let mut ctx = bw_dat::expr::EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: if self.required_context()
                .contains(bw_dat::expr::RequiredContext::MAP_TILE_FLAGS)
            {
                Some(bw::map_tile_flags())
            } else {
                None
            },
            custom: CustomCtx {
                unit: Some(unit),
            },
        };
        ctx.eval_int(self)
    }
}

pub struct CustomCtx {
    unit: Option<Unit>,
}

impl bw_dat::expr::CustomEval for CustomCtx {
    type State = ExprState;

    fn eval_int(&mut self, val: &Int) -> i32 {
        match val {
            Int::MaxEnergy => {
                if let Some(unit) = self.unit {
                    unsafe { samase::unit_max_energy(*unit) as i32 }
                } else {
                    0
                }
            }
        }
    }

    fn eval_bool(&mut self, val: &Bool) -> bool {
        match *val {
        }
    }
}
