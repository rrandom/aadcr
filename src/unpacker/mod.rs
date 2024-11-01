use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_codegen::CodeGenerator;
use std::{
    fs,
    path::{Path, PathBuf},
};

pub mod common;
pub mod webpack;

use webpack::get_modules_form_webpack;

pub struct Module<'a> {
    pub id: usize,
    pub is_entry: bool,
    pub content: Program<'a>,
    pub code: Option<String>,
}

impl<'a> Module<'a> {
    pub fn new(id: usize, is_entry: bool, content: Program<'a>) -> Self {
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
    let modules = get_modules_form_webpack(allocator, program);
    modules.unwrap_or_default()
}

pub fn unpacker<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
    output_dir: &str,
) -> UnpackerResult<'a> {
    let mut files = vec![];

    let mut modules = unpack(allocator, program);

    if !modules.is_empty() {
        fs::create_dir_all(output_dir).unwrap_or_else(|e| {
            eprintln!("Failed to create output directory: {}", e);
        });

        for module in modules.iter_mut() {
            module.write_code();
            let file_name = format!("module_{}.js", module.id);
            let path = Path::new(output_dir).join(file_name);

            // println!("write to {:?}", path);
            fs::write(&path, module.code.as_ref().unwrap()).unwrap();
            files.push(path);
        }
        UnpackerResult { files, modules }
    } else {
        UnpackerResult {
            files: vec![],
            modules: vec![],
        }
    }
}
