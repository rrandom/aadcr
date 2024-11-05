use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use webpack5::get_modules_form_webpack5;

pub mod webpack4;
pub mod webpack5;
mod utils;

use super::Module;
use webpack4::get_modules_form_webpack4;

pub fn get_modules_form_webpack<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    // get_modules_form_webpack4(allocator, program)
    get_modules_form_webpack5(allocator, program)
}
