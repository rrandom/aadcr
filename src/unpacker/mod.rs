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
}

pub struct UnpackResult<'a> {
    pub files: std::vec::Vec<PathBuf>,
    pub modules: std::vec::Vec<Module<'a>>,
}

pub fn unpack<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
    output_dir: &str,
) -> UnpackResult<'a> {
    let mut files = vec![];
    let modules = get_modules_form_webpack(allocator, program);
    if let Some(modules) = modules {
        fs::create_dir_all(output_dir).unwrap_or_else(|e| {
            eprintln!("Failed to create output directory: {}", e);
        });

        for module in modules.iter() {
            let code = CodeGenerator::new().build(&module.content).code;
            let file_name = format!("module_{}.js", module.id);
            let path = Path::new(output_dir).join(file_name);

            // println!("write to {:?}", path);
            fs::write(&path, &code).unwrap();
            files.push(path);
        }
        UnpackResult { files, modules }
    } else {
        UnpackResult {
            files: vec![],
            modules: vec![],
        }
    }
}
