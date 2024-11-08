use indexmap::IndexMap;
use std::cell::RefCell;

use oxc_allocator::CloneIn;
use oxc_ast::{
    ast::{self, Expression, Statement},
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

    /// Generate `export { ... }`
    pub fn gen_esm_exports(&self, ast: &AstBuilder<'a>) -> Vec<Statement<'a>> {
        use ast::{ImportOrExportKind, VariableDeclarationKind};
        use oxc_allocator::Box;

        let mut statements = Vec::new();
        let exports = self.exports.borrow();
        exports.iter().for_each(|(export_key, export_value)| {
            // export default
            if export_key.as_str() == "default" {
                let name = ast.module_export_name_identifier_reference(Span::default(), export_key);

                let export_default_kind = ast.export_default_declaration_kind_expression(
                    export_value.clone_in(ast.allocator),
                );

                let statement =
                    Statement::ExportDefaultDeclaration(ast.alloc_export_default_declaration(
                        Span::default(),
                        export_default_kind,
                        name,
                    ));
                statements.push(statement);
            } else if let Expression::Identifier(id) = export_value
                && id.name == export_key
            {
                // export id
                let local =
                    ast.module_export_name_identifier_reference(Span::default(), export_key);

                let exported =
                    ast.module_export_name_identifier_reference(Span::default(), export_key);

                let specifier = ast.export_specifier(
                    Span::default(),
                    local,
                    exported,
                    ImportOrExportKind::Value,
                );

                let declaration = ast.alloc_export_named_declaration(
                    Span::default(),
                    None,
                    ast.vec1(specifier),
                    None,
                    ImportOrExportKind::Value,
                    None::<Box<_>>,
                );
                statements.push(Statement::ExportNamedDeclaration(declaration));
            } else {
                // export { ida: idb }
                let binding_kind =
                    ast.binding_pattern_kind_binding_identifier(Span::default(), export_key);
                let binding_pattern = ast.binding_pattern(binding_kind, None::<Box<_>>, false);
                let var_declar = ast.variable_declarator(
                    Span::default(),
                    VariableDeclarationKind::Const,
                    binding_pattern,
                    Some(export_value.clone_in(ast.allocator)),
                    false,
                );
                let var_declar = ast.variable_declaration(
                    Span::default(),
                    VariableDeclarationKind::Const,
                    ast.vec1(var_declar),
                    false,
                );
                let declaration = ast.declaration_from_variable(var_declar);
                let statement = ast.alloc_export_named_declaration(
                    Span::default(),
                    Some(declaration),
                    ast.vec(),
                    None,
                    ImportOrExportKind::Value,
                    None::<Box<_>>,
                );
                let statement = Statement::ExportNamedDeclaration(statement);
                statements.push(statement);
            }
        });
        statements
    }

    /// Generate `module.exports = { ... }`
    pub fn gen_cjs_exports(&self, ast: &AstBuilder<'a>) -> Option<Statement<'a>> {
        use ast::{AssignmentOperator, PropertyKind};

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

pub struct ModuleCtx<'a> {
    pub source_text: &'a str,
    pub is_esm: RefCell<bool>,
    pub module_exports: ModuleExportsStore<'a>,
}

impl<'a> ModuleCtx<'a> {
    pub fn new(source_text: &'a str) -> Self {
        Self {
            source_text,
            is_esm: RefCell::new(false),
            module_exports: ModuleExportsStore::new(),
        }
    }
}
