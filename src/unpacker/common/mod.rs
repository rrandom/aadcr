use indexmap::IndexMap;
use std::cell::RefCell;

use oxc_ast::ast::Expression;
use oxc_span::Atom;

pub mod fun_to_program;
pub mod utils;

pub struct ModuleExportsStore<'a> {
    pub exports: RefCell<IndexMap<Atom<'a>, Expression<'a>>>,
}

impl Default for ModuleExportsStore<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ModuleExportsStore<'a> {
    pub fn new() -> Self {
        Self {
            exports: RefCell::new(IndexMap::default()),
        }
    }

    pub fn insert_export(&self, name: Atom<'a>, expr: Expression<'a>) {
        self.exports.borrow_mut().insert(name, expr);
    }
}
