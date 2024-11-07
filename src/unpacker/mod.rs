use browserify::get_modules_form_browserify;
use indexmap::IndexMap;
use normpath::PathExt;

use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_codegen::CodeGenerator;
use std::{
    fs,
    path::{Path, PathBuf},
};

pub mod browserify;
pub mod common;
pub mod webpack;

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
}

pub type UnpackResult<'a> = Option<UnpackReturn<'a>>;

pub fn unpack<'a>(allocator: &'a Allocator, program: &Program<'a>) -> UnpackReturn<'a> {
    get_modules_form_webpack(allocator, program)
        .or(get_modules_form_browserify(allocator, program))
        .unwrap_or_default()
}

#[derive(Default)]
pub struct UnpackerResult<'a> {
    pub files: std::vec::Vec<PathBuf>,
    pub modules: std::vec::Vec<Module<'a>>,
}

pub fn unpacker<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
    output_dir: &str,
) -> UnpackerResult<'a> {
    let mut files = vec![];

    let mut unpack_result = unpack(allocator, program);

    if unpack_result.modules.is_empty() {
        return UnpackerResult::default();
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
        } else {
            format!("module_{}.js", module.id)
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
    UnpackerResult {
        files,
        modules: unpack_result.modules,
    }
}
