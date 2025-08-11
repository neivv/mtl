#![allow(non_snake_case)]

//! Provides safe winapi wrappers with nicer string handling

use std::ffi::{OsString, OsStr};
use std::os::windows::ffi::{OsStringExt, OsStrExt};
use std::ptr::null_mut;

use winapi::um::minwinbase::{SYSTEMTIME};
use winapi::um::sysinfoapi::GetLocalTime;
use winapi::um::winuser::{MessageBoxW};

pub fn winapi_str<T: AsRef<OsStr>>(input: T) -> Vec<u16> {
    input.as_ref().encode_wide().chain(Some(0)).collect::<Vec<u16>>()
}

#[allow(dead_code)]
pub fn os_string_from_winapi(input: &[u16]) -> OsString {
    OsString::from_wide(input)
}

pub fn message_box(caption: &str, msg: &str) {
    unsafe {
        MessageBoxW(null_mut(), winapi_str(msg).as_ptr(), winapi_str(caption).as_ptr(), 0);
    }
}

/// Return true on retry
pub fn message_box_retry(caption: &str, msg: &str) -> bool {
    unsafe {
        MessageBoxW(null_mut(), winapi_str(msg).as_ptr(), winapi_str(caption).as_ptr(), 5) == 4
    }
}

pub fn get_local_time() -> SYSTEMTIME {
    unsafe {
        let mut out: SYSTEMTIME = std::mem::zeroed();
        GetLocalTime(&mut out);
        out
    }
}
