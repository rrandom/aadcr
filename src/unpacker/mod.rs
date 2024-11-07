use browserify::get_modules_form_browserify;
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

pub struct UnpackerResult<'a> {
    pub files: std::vec::Vec<PathBuf>,
    pub modules: std::vec::Vec<Module<'a>>,
}

pub fn unpack<'a>(allocator: &'a Allocator, program: &Program<'a>) -> std::vec::Vec<Module<'a>> {
    get_modules_form_webpack(allocator, program)
        .or(get_modules_form_browserify(allocator, program))
        .unwrap_or_default()
}

pub fn unpacker<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
    output_dir: &str,
) -> UnpackerResult<'a> {
    let mut files = vec![];

    let mut modules = unpack(allocator, program);

    if !modules.is_empty() {
        for module in modules.iter_mut() {
            module.write_code();
            // let file_name = format!("module_{}.js", module.id);
            let file_name = if module.id.ends_with(".js") {
                &module.id
            } else {
                &format!("module_{}.js", module.id)
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
        UnpackerResult { files, modules }
    } else {
        UnpackerResult {
            files: vec![],
            modules: vec![],
        }
    }
}
