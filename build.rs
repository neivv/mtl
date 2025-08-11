use std::cell::RefCell;
use std::fs;
use std::ffi::c_void;
use std::io;
use std::path::{Path, PathBuf};
use std::ptr::{null, null_mut};

use windows::core::{PCSTR};
use windows::Win32::Graphics::Direct3D::*;
use windows::Win32::Graphics::Direct3D::Fxc::*;
use windows::Win32::Foundation::{E_FAIL};

static SOURCES: &[(&str, &str, &[(&str, &str)])] = &[
    ("water", "water.hlsl", &[]),
    ("sprite_tile", "sprite_tile.hlsl", &[]),
    ("sprite_tile_effect", "sprite_tile.hlsl", &[("DRAW_EFFECT", "1")]),
    ("sprite_tile_fish_color", "sprite_tile.hlsl", &[("DRAW_EFFECT", "1"), ("COLOR_DRAW", "1")]),
    ("sprite_tile_fish_alpha", "sprite_tile.hlsl", &[("DRAW_EFFECT", "1"), ("ALPHA_DRAW", "1")]),
    ("sprite_part_solid_frag", "sprite_part_solid_frag.hlsl", &[]),
    ("deferred_blit", "deferred_blit.hlsl", &[]),
    ("heat_distortion", "heat_distortion.hlsl", &[]),
];

fn main() {
    let out_path = std::env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_path);
    if !out_path.exists() {
        fs::create_dir(out_path)
            .expect("Couldn't create dir");
    }
    for &(out_name, source, defines) in SOURCES.iter() {
        let path = Path::new("src/shaders/d3d11").join(source);
        println!("cargo:rerun-if-changed={}", path.to_str().unwrap());
        let bin_filename = format!("{}.bin", out_name);
        let bin_sm4_filename = format!("{}.sm4.bin", out_name);
        let text_bytes = fs::read(path)
            .unwrap_or_else(|e| panic!("Couldn't read {}: {:?}", source, e));
        let shader_bytes = compile(&text_bytes, defines, ShaderModel::Sm5)
            .unwrap_or_else(|e| panic!("Couldn't compile {}: {:?}", source, e));
        let wrapped = wrap_shader(&shader_bytes);
        fs::write(&out_path.join(bin_filename), &wrapped)
            .unwrap_or_else(|e| panic!("Couldn't write {}: {:?}", source, e));

        // Output disassembly for comparing things with dumped disasm.
        // Not necessary for actually building.
        let disasm = disassemble(&shader_bytes)
            .unwrap_or_else(|e| panic!("Couldn't disassemble {}: {:?}", source, e));
        let disasm_filename = format!("{}.asm", out_name);
        fs::write(&out_path.join(&disasm_filename), &disasm)
            .unwrap_or_else(|e| panic!("Couldn't write {}: {:?}", disasm_filename, e));

        // SM4 shader
        let shader_bytes = compile(&text_bytes, defines, ShaderModel::Sm4)
            .unwrap_or_else(|e| panic!("Couldn't compile {}: {:?}", source, e));
        let wrapped = wrap_shader(&shader_bytes);
        fs::write(&out_path.join(bin_sm4_filename), &wrapped)
            .unwrap_or_else(|e| panic!("Couldn't write {}: {:?}", source, e));

        // SM4 disasm
        let disasm = disassemble(&shader_bytes)
            .unwrap_or_else(|e| panic!("Couldn't disassemble {}: {:?}", source, e));
        let disasm_filename = format!("{}.sm4.asm", out_name);
        fs::write(&out_path.join(&disasm_filename), &disasm)
            .unwrap_or_else(|e| panic!("Couldn't write {}: {:?}", disasm_filename, e));
    }
}

enum ShaderModel {
    Sm4,
    Sm5,
}

fn wrap_shader(bytes: &[u8]) -> Vec<u8> {
    let mut out = vec![0u8; 0x38];
    out[0] = 0x3;
    out[0x10] = 0x3;
    out[0x20] = 0x3;
    out[0x28] = 0x1;
    (&mut out[0x30..0x34]).copy_from_slice(&(bytes.len() as u32).to_le_bytes());
    out[0x34] = 0x4;
    out.extend_from_slice(bytes);
    out
}

fn compile(bytes: &[u8], in_defines: &[(&str, &str)], model: ShaderModel) -> io::Result<Vec<u8>> {
    unsafe {
        let mut defines = vec![];
        // Hold define strings for the compilation
        let mut strings = vec![];
        for &(name, val) in in_defines {
            let name = format!("{}\0", name);
            let val = format!("{}\0", val);
            defines.push(D3D_SHADER_MACRO {
                Name: PCSTR(name.as_ptr()),
                Definition: PCSTR(val.as_ptr()),
            });
            strings.push(name);
            strings.push(val);
        }
        defines.push(D3D_SHADER_MACRO {
            Name: PCSTR(null()),
            Definition: PCSTR(null()),
        });
        let mut code = None;
        let mut errors = None;
        let include = IncludeHandler::new(Path::new("src/shaders/d3d11").into());
        let include = ID3DInclude::new(&include);
        let model_string = match model {
            ShaderModel::Sm4 => windows::core::s!("ps_4_0"),
            ShaderModel::Sm5 => windows::core::s!("ps_5_0"),
        };
        let error = D3DCompile2(
            bytes.as_ptr() as *const _,
            bytes.len(),
            None,
            Some(defines.as_ptr()),
            &*include,
            windows::core::s!("main"),
            model_string,
            D3DCOMPILE_OPTIMIZATION_LEVEL3 | D3DCOMPILE_WARNINGS_ARE_ERRORS,
            0,
            0,
            None,
            0,
            &mut code,
            Some(&raw mut errors),
        );
        if let Err(error) = error {
            if let Some(errors) = errors {
                let errors = blob_to_bytes(&errors);
                println!("ERRORS:\n{}", String::from_utf8_lossy(&errors));
            }
            return Err(io::Error::from_raw_os_error(error.code().0));
        }
        Ok(blob_to_bytes(code.as_ref().unwrap()))
    }
}

fn disassemble(bytes: &[u8]) -> io::Result<Vec<u8>> {
    unsafe {
        let result = D3DDisassemble(
            bytes.as_ptr() as *const _,
            bytes.len(),
            0,
            windows::core::s!(""),
        );
        match result {
            Ok(blob) => {
                Ok(blob_to_bytes(&blob))
            }
            Err(error) => {
                return Err(io::Error::from_raw_os_error(error.code().0).into());
            }
        }
    }
}

unsafe fn blob_to_bytes(blob: &ID3DBlob) -> Vec<u8> {
    let slice = std::slice::from_raw_parts(
        blob.GetBufferPointer() as *const u8,
        blob.GetBufferSize(),
    );
    slice.into()
}

struct IncludeHandler {
    path: PathBuf,
    buffers: RefCell<Vec<Vec<u8>>>,
}

impl IncludeHandler {
    fn new(path: PathBuf) -> IncludeHandler {
        IncludeHandler {
            path,
            buffers: RefCell::new(Vec::new()),
        }
    }
}

impl ID3DInclude_Impl for IncludeHandler {
    fn Open(
        &self,
        _include_type: D3D_INCLUDE_TYPE,
        filename: &PCSTR,
        _parent_data: *const c_void,
        out_data: *mut *mut c_void,
        out_size: *mut u32,
    ) -> Result<(), windows::core::Error> {
        unsafe {
            *out_data = null_mut();
            *out_size = 0;
            let filename = match filename.to_string() {
                Ok(o) => o,
                Err(_) => return Err(E_FAIL.into()),
            };
            let path = self.path.join(&filename);
            println!("cargo:rerun-if-changed={}", path.to_str().unwrap());
            let result = match fs::read(&path) {
                Ok(o) => o,
                Err(e) => {
                    println!("Reading include {} failed: {:?}", path.display(), e);
                    return Err(E_FAIL.into());
                }
            };
            let ptr = result.as_ptr();
            let len = result.len();
            self.buffers.borrow_mut().push(result);
            *out_data = ptr as *mut _;
            *out_size = len as u32;
            Ok(())
        }
    }

    fn Close(
        &self,
        data: *const c_void,
    ) -> Result<(), windows::core::Error> {
        let mut buffers = self.buffers.borrow_mut();
        let pos = match buffers.iter().position(|x| x.as_ptr() == data as *const u8) {
            Some(s) => s,
            None => return Err(E_FAIL.into()),
        };
        buffers.swap_remove(pos);
        Ok(())
    }
}
