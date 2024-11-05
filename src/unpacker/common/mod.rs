use indexmap::IndexMap;
use std::cell::RefCell;

use oxc_allocator::CloneIn;
use oxc_ast::{
    ast::{AssignmentOperator, Expression, PropertyKind, Statement},
    AstBuilder,
};
use oxc_span::{Atom, Span};

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

    pub fn gen_esm_exports(&self) -> Vec<Statement<'a>> {
        todo!()
    }

    pub fn gen_cjs_exports(&self, ast: &AstBuilder<'a>) -> Option<Statement<'a>> {
        let exports = self.exports.borrow();
        if exports.is_empty() {
            return None;
        }
        let properties = ast.vec_from_iter(exports.iter().map(|(key, value)| {
            ast.object_property_kind_object_property(
                Span::default(),
                PropertyKind::Init,
                ast.property_key_identifier_name(Span::default(), key),
                value.clone_in(ast.allocator),
                None,
                false,
                false,
                false,
            )
        }));

        let left_inner = ast.member_expression_from_static(ast.static_member_expression(
            Span::default(),
            ast.expression_identifier_reference(Span::default(), "module"),
            ast.identifier_name(Span::default(), "exports"),
            false,
        ));
        let left = ast.simple_assignment_target_member_expression(left_inner);

        let right = ast.expression_object(Span::default(), properties, None);
        let exp = ast.expression_assignment(
            Span::default(),
            AssignmentOperator::Assign,
            ast.assignment_target_simple(left),
            right,
        );
        Some(ast.statement_expression(Span::default(), exp))
    }
}
