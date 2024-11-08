pub mod browserify;
pub mod common;
pub mod webpack;

use indexmap::IndexMap;
use normpath::PathExt;

use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_codegen::CodeGenerator;
use oxc_diagnostics::OxcDiagnostic;
use std::{
    fs,
    path::{Path, PathBuf},
};

use browserify::get_modules_form_browserify;
use webpack::get_modules_form_webpack;

pub struct Module<'a> {
    pub id: String,
    pub is_entry: bool,
    pub content: Program<'a>,
    pub code: Option<String>,
}

impl<'a> Module<'a> {
    pub fn new(id: String, is_entry: bool, content: Program<'a>) -> Self {
        Self {
            id,
            is_entry,
            content,
            code: None,
        }
    }

    pub fn write_code(&mut self) {
        self.code = Some(CodeGenerator::new().build(&self.content).code);
    }
}

impl std::fmt::Debug for Module<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Module {{id: {}, is_entry: {}, code: `{}` }}",
            self.id,
            self.is_entry,
            self.code.as_deref().unwrap_or("")
        )
    }
}

#[derive(Default)]
pub struct UnpackReturn<'a> {
    pub modules: std::vec::Vec<Module<'a>>,
    pub module_mapping: Option<IndexMap<&'a str, &'a str>>,
    pub errors: std::vec::Vec<OxcDiagnostic>,
}

pub type UnpackResult<'a> = Option<UnpackReturn<'a>>;

#[derive(Default)]
pub struct UnpackerReturn<'a> {
    pub errors: std::vec::Vec<OxcDiagnostic>,
    pub files: std::vec::Vec<PathBuf>,
    pub modules: std::vec::Vec<Module<'a>>,
}

pub struct Unpacker<'a> {
    allocator: &'a Allocator,
    program: &'a Program<'a>,
    pub source_text: &'a str,
}

impl<'a> Unpacker<'a> {
    pub fn new(allocator: &'a Allocator, program: &'a Program<'a>, source_text: &'a str) -> Self {
        Self {
            allocator,
            program,
            source_text,
        }
    }

    pub fn unpack(&self) -> UnpackReturn<'a> {
        get_modules_form_webpack(self.allocator, self.program, self.source_text)
            .or(get_modules_form_browserify(self.allocator, self.program))
            .unwrap_or_default()
    }

    pub fn build(&self, output_dir: &str) -> UnpackerReturn<'a> {
        let mut files = vec![];

        let mut unpack_result = self.unpack();

        if unpack_result.modules.is_empty() {
            return UnpackerReturn::default();
        }
        for module in unpack_result.modules.iter_mut() {
            module.write_code();
            let file_name = if let Some(mapping) = unpack_result.module_mapping.as_ref()
                && let Some(name) = mapping.get(module.id.as_str())
            {
                let name = name.to_string();

                if !name.ends_with(".js") {
                    format!("{}.js", name)
                } else {
                    name
                }
            } else if !module.id.ends_with(".js") {
                format!("{}.js", module.id)
            } else {
                module.id.to_string()
            };

            let path = Path::new(output_dir)
                .join(file_name)
                .normalize_virtually()
                .unwrap();

            if let Ok(Some(parent)) = path.parent() {
                fs::create_dir_all(parent).unwrap_or_else(|e| {
                    eprintln!("Failed to create directory {:?}: {}", parent, e);
                });
            }

            // println!("write to {:?}", path);
            fs::write(&path, module.code.as_ref().unwrap()).unwrap();
            files.push(path.into_path_buf());
        }
        UnpackerReturn {
            files,
            modules: unpack_result.modules,
            errors: unpack_result.errors,
        }
    }
}
