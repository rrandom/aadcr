use super::UnminifyPass;
use oxc_allocator::{CloneIn, Vec};
use oxc_ast::ast::{Declaration, Statement, VariableDeclaration};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

// TO-DO: variable merging in for statement
pub struct UnVariableMerging {
    changed: bool,
}

impl UnVariableMerging {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnVariableMerging {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnVariableMerging {
    fn enter_variable_declaration(
        &mut self,
        node: &mut oxc_ast::ast::VariableDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
    }

    fn exit_export_named_declaration(
        &mut self,
        node: &mut oxc_ast::ast::ExportNamedDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // dbg!(node);
    }

    fn exit_module_declaration(
        &mut self,
        node: &mut oxc_ast::ast::ModuleDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // dbg!(node);
    }

    fn enter_statement(&mut self, node: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Statement::ExportNamedDeclaration(declaration) = node {
            // dbg!(declaration);
        }
    }

    fn exit_program(&mut self, node: &mut oxc_ast::ast::Program<'a>, ctx: &mut TraverseCtx<'a>) {
        self.un_merging_in_statements(&mut node.body, ctx);
    }
}

impl<'a> UnVariableMerging {
    fn split_declaration(
        &mut self,
        declaration: &mut VariableDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Vec<'a, Declaration<'a>>> {
        let declarations = &mut declaration.declarations;
        let mut rest = declarations.split_off(1);
        if !rest.is_empty() {
            let mut new_declarations = ctx.ast.vec();
            for declarator in rest.iter_mut() {
                let del = ctx.ast.declaration_variable(
                    SPAN,
                    declaration.kind,
                    ctx.ast.vec1(declarator.clone_in(ctx.ast.allocator)),
                    declaration.declare,
                );
                new_declarations.push(del);
            }
            self.changed = true;
            return Some(new_declarations);
        }
        None
    }

    fn un_merging_in_statements(
        &mut self,
        statements: &mut Vec<Statement<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let mut i = 0;
        while i < statements.len() {
            let result = {
                let current = statements.get_mut(i).unwrap();
                match current {
                    Statement::VariableDeclaration(decl) => {
                        if let Some(new_decls) = self.split_declaration(decl, ctx) {
                            let new_stmts = new_decls
                                .into_iter()
                                .map(|declaration| ctx.ast.statement_declaration(declaration));

                            Some(ctx.ast.vec_from_iter(new_stmts))
                        } else {
                            None
                        }
                    }
                    Statement::ExportNamedDeclaration(export_decl) => {
                        if let Some(Declaration::VariableDeclaration(declaration)) =
                            export_decl.declaration.as_mut()
                        {
                            if let Some(new_decls) = self.split_declaration(declaration, ctx) {
                                let new_stmts = new_decls.into_iter().map(|declaration| {
                                    let del = ctx.ast.module_declaration_export_named_declaration(
                                        SPAN,
                                        Some(declaration),
                                        export_decl.specifiers.clone_in(ctx.ast.allocator),
                                        export_decl.source.clone_in(ctx.ast.allocator),
                                        export_decl.export_kind,
                                        export_decl.with_clause.clone_in(ctx.ast.allocator),
                                    );
                                    ctx.ast.statement_module_declaration(del)
                                });
                                Some(ctx.ast.vec_from_iter(new_stmts))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            };

            // 在借用结束后进行 `splice` 操作
            if let Some(new_stmts) = result {
                let len = new_stmts.len();
                statements.splice(i + 1..i + 1, new_stmts);
                i += len;
            }

            i += 1;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnVariableMerging::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_variable_merging_declaration() {
        run_test(
            r#"
var a= 1, b = true, c = "hello", d = 1.2, e = [1, 2, 3], f = {a: 1, b: 2, c: 3}, g = function() { return 1; }, h = () => 1,
{ i: j } = k, [l, m] = n;
            "#,
            r#"
var a= 1;
var b = true;
var c = "hello";
var d = 1.2;
var e = [1, 2, 3];
var f = {a: 1, b: 2, c: 3};
var g = function() { return 1; };
var h = () => 1;
var { i: j } = k;
var [l, m] = n;
            "#,
        );
    }

    #[test]
    fn test_un_variable_merging_declare_kind_remains() {
        run_test(
            r#"
var a = 1, b = 2, c = 3;

let d = 1, e = 2, f = 3;

const g = 1, h = 2, i = 3;
            "#,
            r#"
var a = 1;
var b = 2;
var c = 3;
let d = 1;
let e = 2;
let f = 3;
const g = 1;
const h = 2;
const i = 3;
            "#,
        );
    }

    #[test]
    fn test_un_variable_merging_with_export() {
        run_test(
            r#"
export var a= 1, b = true, c = "hello";
            "#,
            r#"
export var a= 1;
export var b = true;
export var c = "hello";
            "#,
        );
    }
}
