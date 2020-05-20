use std::sync::{Mutex, MutexGuard};

use json::JsonValue;

use crate::samase;

pub struct StringTable {
    by_index: Vec<String>,
}

impl StringTable {
    pub fn by_index(&self, index: u16) -> Option<&str> {
        // 0 is always null string wherever tbl indices are used.
        self.by_index.get(index.checked_sub(1)? as usize).map(|x| &**x)
    }
}

lazy_static! {
    static ref STAT_TXT: Mutex<StringTable> = Mutex::new(StringTable {
        by_index: vec![],
    });
}

pub fn stat_txt() -> MutexGuard<'static, StringTable> {
    STAT_TXT.lock().unwrap()
}

/// SCR stores string tables in a dumb double-escaped format where
/// each backslash is doubled -- So a newline would appear as "\\n"
/// Convert double backslashes to single ones and let the json parser
/// handle rest.
fn fix_stat_txt_escapes(mut input: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(input.len());
    while input.len() != 0 {
        let backslash_index = match input.iter().position(|&c| c == b'\\') {
            Some(s) => s,
            None => {
                out.extend_from_slice(input);
                break;
            }
        };
        out.extend_from_slice(&input[..backslash_index + 1]);
        if input.get(backslash_index + 1).cloned() == Some(b'\\') {
            input = &input[backslash_index + 2..];
        } else {
            input = &input[backslash_index + 1..];
        }
    }
    out
}

#[test]
fn test_fix_stat_txt_escapes() {
    let text = br#"[
      {
        "id": "FIRST_UNIT_STRING",
        "Key": "FIRST_UNIT_STRING",
        "Value": "Terran Marine\\u0000*\\u0000Ground Units"
      },
      {
        "id": "FIRST_UNIT_STRING-1",
        "Key": "FIRST_UNIT_STRING-1",
        "Value": "Terran Ghost\\u0000*\\u0000Ground Units"
      },
    ]"#;
    let expected = br#"[
      {
        "id": "FIRST_UNIT_STRING",
        "Key": "FIRST_UNIT_STRING",
        "Value": "Terran Marine\u0000*\u0000Ground Units"
      },
      {
        "id": "FIRST_UNIT_STRING-1",
        "Key": "FIRST_UNIT_STRING-1",
        "Value": "Terran Ghost\u0000*\u0000Ground Units"
      },
    ]"#;
    let fixed = fix_stat_txt_escapes(&text[..]);
    assert_eq!(
        std::str::from_utf8(&fixed[..]).unwrap(),
        std::str::from_utf8(&expected[..]).unwrap(),
    );
}

pub fn init() {
    let path = "rez\\stat_txt.json";
    let bytes = match samase::read_file(path) {
        Some(s) => s,
        None => return,
    };
    let bytes = fix_stat_txt_escapes(&bytes);
    let string = match std::str::from_utf8(&bytes) {
        Ok(o) => o,
        Err(_) => {
            error!("{} is not utf8", path);
            return;
        }
    };
    let json = match json::parse(string) {
        Ok(o) => o,
        Err(e) => {
            error!("{} didn't parse: {}", path, e);
            return;
        }
    };
    let arr = match json {
        JsonValue::Array(vec) => vec,
        _ => {
            error!("{} wasn't array", path);
            return;
        }
    };
    let mut by_index = Vec::with_capacity(1024);
    for mut obj in arr {
        let key = obj.remove("Key").take_string();
        let val = obj.remove("Value").take_string();
        if let (Some(_key), Some(val)) = (key, val) {
            by_index.push(val);
        } else {
            by_index.push(String::new());
        }
    }
    *STAT_TXT.lock().unwrap() = StringTable {
        by_index,
    };
}
