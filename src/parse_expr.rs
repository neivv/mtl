use combine::{Parser, Positioned, RangeStreamOnce, many, many1, optional, skip_many, try};
use combine::byte::{alpha_num, byte, digit, hex_digit, letter, spaces};
use combine::easy;
use combine::error::{Consumed, StreamError};
use combine::parser::function;
use combine::range::{range, recognize};
use combine::stream::state::{IndexPositioner, State};

// bool_expr -> {
//  p1_bool_expr || p1_bool_expr [|| ..]
//  p1_bool_expr && p1_bool_expr [&& ..]
//  p1_bool_expr
// }
// p1_bool_expr -> {
//  int_expr < int_expr
//  int_expr <= int_expr
//  int_expr > int_expr
//  int_expr >= int_expr
//  p2_bool_expr == p2_bool_expr
//  p2_bool_expr != p2_bool_expr
//  int_expr == int_expr
//  int_expr != int_expr
//  p2_bool_expr
// }
// p2_bool_expr -> {
//  true
//  false
//  !p2_bool_expr
//  (bool_expr)
//  bool_fun[(...)]
// }
// int_expr {
//  p1_int_expr +- p1_int_expr [+- ..]
//  p1_int_expr
// }
// p1_int_expr {
//  p2_int_expr */% p2_int_expr [*/% ..]
//  p2_int_expr
// }
// p2_int_expr {
//  0xhex
//  dec
//  int_fun[(...)]
//  (int_expr)
// }
// int_fun {
//  stim_timer
//  ensnare_timer
//  maelstrom_timer
//  death_timer
//  lockdown_timer
//  irradiate_timer
//  stasis_timer
//  plague_timer
//  irradiate_timer
//  matrix_timer
//  matrix_hitpoints
//  acid_spore_count
//  fighters
//  mines
//  hitpoints
//  hitpoints_percent
//  shields
//  shields_percent
//  energy
//  kills
//  frame_count
//  tileset
//  minerals
//  gas
//  carried_resource_amount
//  ground_cooldown
//  air_cooldown
//  spell_cooldown
//  speed
//  sigorder
// }
// bool_fun {
//  parasited
//  blind
//  under_storm
//  lifted_off
//  building_unit
//  in_transport
//  in_bunker
//  carrying_powerup
//  carrying_minerals
//  carrying_gas
//  burrowed
//  disabled
//  completed
//  self_cloaked
//  arbiter_cloaked
//  cloaked
//  under_dweb
//  hallucination
// }

type Bytes<'a> = easy::Stream<State<&'a [u8], IndexPositioner>>;

/// Function arguments
/// Given "a, b, c()) + 2", returns "a, b, c()" and ") + 2"
fn func_arg_list<'a>() -> impl Parser<Input = Bytes<'a>, Output = &'a [u8]> {
    byte(b'(').with(function::parser(|input: &mut Bytes<'a>| {
        let mut parens = 1;
        let pos = input.position();
        input.uncons_while(|byte| {
            if byte == b'(' {
                parens += 1;
                true
            } else if byte == b')' {
                parens -= 1;
                parens != 0
            } else {
                true
            }
        }).map(|x| {
            (x, Consumed::Consumed(()))
        }).map_err(|e| {
            Consumed::Consumed(easy::Errors::new(pos, e).into())
        })
    })).skip(byte(b')'))
}

fn identifier<'a>() -> impl Parser<Input = Bytes<'a>, Output = &'a [u8]> {
    recognize(letter().or(byte(b'_')).with(skip_many(alpha_num().or(byte(b'_')))))
}

fn func<'a>() -> impl Parser<Input = Bytes<'a>, Output = (&'a [u8], &'a [u8])> {
    identifier()
        .and(optional(func_arg_list()))
        .skip(spaces())
        .map(|(x, y)| (x, y.unwrap_or(b"")))
}

macro_rules! decl_func {
    ($name:ident, $ename:ident, $($conf_name:expr, $variant_name:ident,)*) => {
        fn $name<'a>() -> impl Parser<Input = Bytes<'a>, Output = $ename> {
            func().and_then(|(name, params)| -> Result<_, easy::Error<_, _>> {
                if params != b"" {
                    return Err(StreamError::unexpected_message("Can't have parameters"));
                }
                let result = match name {
                    $($conf_name => $ename::$variant_name,)*
                    other => {
                        let message = format!("Invalid name {}", String::from_utf8_lossy(other));
                        return Err(StreamError::unexpected_message(message));
                    }
                };
                Ok(result)
            })
        }

        #[derive(Debug, Eq, PartialEq, Clone)]
        pub enum $ename {
            $($variant_name,)*
        }
    }
}

decl_func!(
    int_func, IntFunc,
    b"stim_timer", StimTimer,
    b"ensnare_timer", EnsnareTimer,
    b"maelstrom_timer", MaelstromTimer,
    b"death_timer", DeathTimer,
    b"lockdown_timer", LockdownTimer,
    b"stasis_timer", StasisTimer,
    b"irradiate_timer", IrradiateTimer,
    b"matrix_timer", MatrixTimer,
    b"matrix_hitpoints", MatrixHitpoints,
    b"acid_spore_count", AcidSporeCount,
    b"fighters", Fighters,
    b"mines", Mines,
    b"hitpoints", Hitpoints,
    b"hitpoints_percent", HitpointsPercent,
    b"shields", Shields,
    b"shields_percent", ShieldsPercent,
    b"energy", Energy,
    b"kills", Kills,
    b"frame_count", FrameCount,
    b"tileset", Tileset,
    b"minerals", Minerals,
    b"gas", Gas,
    b"carried_resource_amount", CarriedResourceAmount,
    b"ground_cooldown", GroundCooldown,
    b"air_cooldown", AirCooldown,
    b"spell_cooldown", SpellCooldown,
    b"speed", Speed,
    b"sigorder", SigOrder,
);

decl_func!(
    bool_func, BoolFunc,
    // Shrug, true/false work this way
    b"true", True,
    b"false", False,
    b"parasited", Parasited,
    b"blind", Blind,
    b"under_storm", UnderStorm,
    b"lifted_off", LiftedOff,
    b"building_unit", BuildingUnit,
    b"in_transport", InTransport,
    b"in_bunker", InBunker,
    b"carrying_powerup", CarryingPowerup,
    b"carrying_minerals", CarryingMinerals,
    b"carrying_gas", CarryingGas,
    b"burrowed", Burrowed,
    b"disabled", Disabled,
    b"completed", Completed,
    b"self_cloaked", SelfCloaked,
    b"arbiter_cloaked", ArbiterCloaked,
    b"cloaked", Cloaked,
    b"under_dweb", UnderDweb,
    b"hallucination", Hallucination,
);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntExpr {
    Add(Box<(IntExpr, IntExpr)>),
    Sub(Box<(IntExpr, IntExpr)>),
    Mul(Box<(IntExpr, IntExpr)>),
    Div(Box<(IntExpr, IntExpr)>),
    Modulo(Box<(IntExpr, IntExpr)>),
    Integer(i32),
    Func(IntFunc),
}

pub fn int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    p1_int_expr().and(
        many::<Vec<_>, _>(byte(b'+').or(byte(b'-')).skip(spaces()).and(p1_int_expr()))
    ).map(|(mut left, rest)| {
        for (op, right) in rest {
            match op {
                b'+' => left = IntExpr::Add(Box::new((left, right))),
                b'-' | _  => left = IntExpr::Sub(Box::new((left, right))),
            }
        }
        left
    })
}

fn p1_int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    p2_int_expr().and(
        many::<Vec<_>, _>(
            byte(b'*').or(byte(b'/')).or(byte(b'%')).skip(spaces()).and(p2_int_expr())
        )
    ).and_then(|(mut left, rest)| -> Result<_, easy::Error<_, _>> {
        for (op, right) in rest {
            match op {
                b'*' => left = IntExpr::Mul(Box::new((left, right))),
                b'/' | b'%' | _ => {
                    match right {
                        IntExpr::Integer(0) => {
                            let msg = "Cannot divide by zero";
                            return Err(StreamError::message_static_message(msg));
                        }
                        IntExpr::Integer(_) => (),
                        _ => {
                            let msg = "Can only divide by a constant";
                            return Err(StreamError::message_static_message(msg));
                        }
                    }
                    left = if op == b'/' {
                        IntExpr::Div(Box::new((left, right)))
                    } else {
                        IntExpr::Modulo(Box::new((left, right)))
                    };
                }
            }
        }
        Ok(left)
    })
}

fn p2_int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    let lazy = function::parser(|input| int_expr().parse_stream(input));
    integer().map(|x| IntExpr::Integer(x))
        .or(int_func().map(|x| IntExpr::Func(x)))
        .or(byte(b'(').skip(spaces()).with(lazy).skip(byte(b')')))
}

fn integer<'a>() -> impl Parser<Input = Bytes<'a>, Output = i32> {
    use std::str;
    optional(byte(b'-')).and(
        range(&b"0x"[..]).with({
            recognize(skip_many(hex_digit())).map(|x| {
                str::from_utf8(x).ok().and_then(|x| i32::from_str_radix(x, 16).ok())
            })
        }).or(recognize(skip_many(digit())).map(|x| {
            str::from_utf8(x).ok().and_then(|x| i32::from_str_radix(x, 10).ok())
        }))
    ).skip(spaces()).and_then(|(neg, int_parse_result)| -> Result<_, easy::Error<_, _>> {
        match int_parse_result {
            Some(o) => Ok(match neg.is_some() {
                true => 0 - o,
                false => o,
            }),
            None => Err(StreamError::unexpected_message("Integer literal out of range")),
        }
    })
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BoolExpr {
    And(Box<(BoolExpr, BoolExpr)>),
    Or(Box<(BoolExpr, BoolExpr)>),
    LessThan(Box<(IntExpr, IntExpr)>),
    LessOrEqual(Box<(IntExpr, IntExpr)>),
    GreaterThan(Box<(IntExpr, IntExpr)>),
    GreaterOrEqual(Box<(IntExpr, IntExpr)>),
    EqualInt(Box<(IntExpr, IntExpr)>),
    EqualBool(Box<(BoolExpr, BoolExpr)>),
    Not(Box<BoolExpr>),
    Func(BoolFunc),
}

pub fn bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    p1_bool_expr().and(
        many1::<Vec<_>, _>(range(&b"||"[..]).skip(spaces()).and(p1_bool_expr()))
            .or(many::<Vec<_>, _>(range(&b"&&"[..]).skip(spaces()).and(p1_bool_expr())))
    ).map(|(mut left, rest)| {
        for (op, right) in rest {
            match op {
                b"||" => left = BoolExpr::Or(Box::new((left, right))),
                b"&&" | _  => left = BoolExpr::And(Box::new((left, right))),
            }
        }
        left
    })
}

fn p1_bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    try(int_expr()).and(
        choice!(
            range(&b"<"[..]),
            range(&b"<="[..]),
            range(&b">"[..]),
            range(&b">="[..]),
            range(&b"=="[..]),
            range(&b"!="[..])
        ).skip(spaces())
    ).and(int_expr()).map(|((left, op), right)| {
        match op {
            b"<" => BoolExpr::LessThan(Box::new((left, right))),
            b"<=" => BoolExpr::LessOrEqual(Box::new((left, right))),
            b">" => BoolExpr::GreaterThan(Box::new((left, right))),
            b">=" => BoolExpr::GreaterOrEqual(Box::new((left, right))),
            b"==" => BoolExpr::EqualInt(Box::new((left, right))),
            b"!=" | _ => BoolExpr::Not(Box::new(BoolExpr::EqualInt(Box::new((left, right))))),
        }
    }).or(
        p2_bool_expr().and(
            optional(range(&b"=="[..]).skip(spaces()).and(p2_bool_expr()))
                .or(optional(range(&b"!="[..]).skip(spaces()).and(p2_bool_expr())))
        ).map(|(mut left, rest)| {
            if let Some((op, right)) = rest {
                match op {
                    b"==" => left = BoolExpr::EqualBool(Box::new((left, right))),
                    b"!=" | _ => {
                        left =
                            BoolExpr::Not(Box::new(BoolExpr::EqualBool(Box::new((left, right)))));
                    }
                }
            }
            left
        })
    )
}

fn p2_bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    let lazy = function::parser(|input| bool_expr().parse_stream(input));
    let lazy_p2 = function::parser(|input| p2_bool_expr().parse_stream(input));
    byte(b'!').with(lazy_p2).map(|x| BoolExpr::Not(Box::new(x)))
        .or(bool_func().map(|x| BoolExpr::Func(x)))
        .or(byte(b'(').skip(spaces()).with(lazy).skip(byte(b')')))
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    fn error_contains<T: Debug, A, B, C>(
        result: Result<T, easy::Errors<A, B, C>>,
        expected: &str,
    ) -> bool {
        result.unwrap_err().errors.iter().any(|x| {
            match x {
                easy::Error::Unexpected(i) |
                    easy::Error::Expected(i) |
                    easy::Error::Message(i) =>
                {
                    match i {
                        easy::Info::Owned(s) => s.contains(expected),
                        easy::Info::Borrowed(s) => s.contains(expected),
                        _ => false,
                    }
                }
                _ => false,
            }
        })
    }

    fn empty_unwrap<'a, T, A: Debug, B: Debug, C: Debug>(
        result: Result<(T, State<&'a [u8], IndexPositioner>), easy::Errors<A, B, C>>,
    ) -> T {
        let result = result.unwrap();
        assert_eq!(result.1.input.len(), 0);
        result.0
    }

    fn s<'a>(text: &'a [u8]) -> State<&'a [u8], IndexPositioner> {
        State::new(text)
    }

    #[test]
    fn test_int_func() {
        let mut parser = int_func();
        let mut parse = |text| {
            parser.easy_parse(s(text))
        };
        assert_eq!(empty_unwrap(parse(b"hitpoints")), IntFunc::Hitpoints);
        assert_eq!(empty_unwrap(parse(b"hitpoints()")), IntFunc::Hitpoints);
        assert_eq!(
            empty_unwrap(parse(b"acid_spore_count")),
            IntFunc::AcidSporeCount
        );
        assert!(error_contains(parse(b"hitpoints(s)"), "Can't have parameter"));
        assert!(error_contains(parse(b"unknown"), "Invalid"));
    }

    #[test]
    fn test_int_expr() {
        let mut parser = int_expr();
        let mut parse = |text| {
            parser.easy_parse(s(text))
        };
        assert_eq!(empty_unwrap(parse(b"20")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"0x20")), IntExpr::Integer(0x20));
        assert_eq!(empty_unwrap(parse(b"-20")), IntExpr::Integer(-20));
        assert_eq!(empty_unwrap(parse(b"-0x20")), IntExpr::Integer(-0x20));
        assert_eq!(empty_unwrap(parse(b"(20)")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"((20))")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"energy")), IntExpr::Func(IntFunc::Energy));
        assert_eq!(
            empty_unwrap(parse(b"2+2")),
            IntExpr::Add(Box::new((IntExpr::Integer(2), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"2 + 2")),
            IntExpr::Add(Box::new((IntExpr::Integer(2), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"6 * 8")),
            IntExpr::Mul(Box::new((IntExpr::Integer(6), IntExpr::Integer(8))))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 + 2 - 6")),
            IntExpr::Sub(Box::new((
                IntExpr::Add(Box::new((IntExpr::Integer(8), IntExpr::Integer(2)))),
                IntExpr::Integer(6)
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 + 2 * 6")),
            IntExpr::Add(Box::new((
                IntExpr::Integer(8),
                IntExpr::Mul(Box::new((IntExpr::Integer(2), IntExpr::Integer(6))))
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 * 2 - 6")),
            IntExpr::Sub(Box::new((
                IntExpr::Mul(Box::new((IntExpr::Integer(8), IntExpr::Integer(2)))),
                IntExpr::Integer(6)
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 * (2 - 6)")),
            IntExpr::Mul(Box::new((
                IntExpr::Integer(8),
                IntExpr::Sub(Box::new((IntExpr::Integer(2), IntExpr::Integer(6))))
            )))
        );
        assert!(error_contains(parse(b"4294967295"), "out of range"));
        assert!(error_contains(parse(b"5 / 0"), "divide by zero"));
    }

    #[test]
    fn test_bool_expr() {
        let mut parser = bool_expr();
        let mut parse = |text| {
            parser.easy_parse(s(text))
        };
        let fun_true = || BoolExpr::Func(BoolFunc::True);
        let fun_false = || BoolExpr::Func(BoolFunc::False);
        assert_eq!(empty_unwrap(parse(b"true")), fun_true());
        assert_eq!(empty_unwrap(parse(b"false")), fun_false());
        assert_eq!(empty_unwrap(parse(b"((true))")), fun_true());
        assert_eq!(empty_unwrap(parse(b"blind")), BoolExpr::Func(BoolFunc::Blind));
        assert_eq!(
            empty_unwrap(parse(b"true || false")),
            BoolExpr::Or(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && false")),
            BoolExpr::And(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && false && true")),
            BoolExpr::And(Box::new((
                BoolExpr::And(Box::new((fun_true(), fun_false()))),
                fun_true(),
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && (false || true)")),
            BoolExpr::And(Box::new((
                fun_true(),
                BoolExpr::Or(Box::new((fun_false(), fun_true()))),
            )))
        );

        assert_eq!(
            empty_unwrap(parse(b"!false")),
            BoolExpr::Not(Box::new(fun_false())),
        );
        assert_eq!(
            empty_unwrap(parse(b"!true && (false || true)")),
            BoolExpr::And(Box::new((
                BoolExpr::Not(Box::new(fun_true())),
                BoolExpr::Or(Box::new((fun_false(), fun_true()))),
            )))
        );

        assert_eq!(
            empty_unwrap(parse(b"true == false")),
            BoolExpr::EqualBool(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"1 == 2")),
            BoolExpr::EqualInt(Box::new((IntExpr::Integer(1), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"1 + 8 == 2")),
            BoolExpr::EqualInt(Box::new((
                IntExpr::Add(Box::new((IntExpr::Integer(1), IntExpr::Integer(8)))),
                IntExpr::Integer(2),
            )))
        );

        assert_eq!(parse(b"true || false && true").unwrap().1.input.len(), "&& true".len());
        parse(b"! true").unwrap_err();
    }
}
