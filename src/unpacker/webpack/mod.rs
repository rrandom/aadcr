use jsonp::get_modules_form_jsonp;
use oxc_allocator::Allocator;
use oxc_ast::ast::Program;

pub mod jsonp;
pub mod webpack4;
pub mod webpack5;

use super::{Module, UnpackResult, UnpackReturn};
use webpack4::get_modules_form_webpack4;
use webpack5::get_modules_form_webpack5;

pub fn get_modules_form_webpack<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> UnpackResult<'a> {
    get_modules_form_webpack5(allocator, program)
        .or(get_modules_form_webpack4(allocator, program))
        .or(get_modules_form_jsonp(allocator, program))
}
