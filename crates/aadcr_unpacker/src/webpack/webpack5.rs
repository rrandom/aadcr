use indexmap::IndexMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{Expression, ExpressionStatement, ObjectPropertyKind, Program, Statement},
    AstBuilder, AstKind,
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Atom, SPAN};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::{
    common::{fun_to_program::FunctionToProgram, utils, ModuleCtx},
    Module, UnpackResult, UnpackReturn,
};

use super::{RequireD5, RequireR};

pub fn get_modules_form_webpack5<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
    source_text: &'a str,
) -> UnpackResult<'a> {
    let ast = AstBuilder::new(allocator);

    let semantic = SemanticBuilder::new("").build(program).semantic;

    let program_source_type = program.source_type;
    let program_directives = &program.directives;

    let nodes = semantic.nodes();
    let root_id = nodes.root()?;

    let mut module_map = IndexMap::new();

    let mut errors = vec![];

    for node in nodes.iter() {
        match node.kind() {
            AstKind::ExpressionStatement(ExpressionStatement { expression, .. })
                if nodes.parent_id(node.id()) == Some(root_id) =>
            {
                let Some(fun_body) = utils::get_iife_callee(expression)
                    .and_then(|fun_expr| utils::get_fun_body(fun_expr))
                else {
                    continue;
                };

                for st in fun_body.statements.iter() {
                    let Statement::VariableDeclaration(vardecl) = st else {
                        continue;
                    };
                    let de = &vardecl.declarations[0];

                    let Some(init) = &de.init else {
                        continue;
                    };
                    let Expression::ObjectExpression(ob) = init.without_parentheses() else {
                        continue;
                    };
                    for prop in ob.properties.iter() {
                        let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                            continue;
                        };
                        let Some(Expression::StringLiteral(key)) = obj_prop.key.as_expression()
                        else {
                            continue;
                        };
                        let module_id = &key.value;

                        let expr = obj_prop.value.without_parentheses();

                        match expr {
                            Expression::ArrowFunctionExpression(_)
                            | Expression::FunctionExpression(_) => {
                                module_map.insert(module_id.as_str(), expr);
                            }
                            _ => {
                                continue;
                            }
                        }
                    }
                }

                let last_st = fun_body.statements.last();

                if let Some(Statement::ExpressionStatement(expr)) = last_st
                    && let Some(fun_expr) = utils::get_iife_callee(&expr.expression)
                {
                    match fun_expr {
                        Expression::ArrowFunctionExpression(_)
                        | Expression::FunctionExpression(_) => {
                            module_map.insert("entry.js", fun_expr);
                        }
                        _ => {
                            continue;
                        }
                    }
                } else {
                    errors.push(OxcDiagnostic::error(
                        "The last statement in the IIFE must be an expression",
                    ));
                }
            }
            _ => {}
        }
    }

    if module_map.is_empty() {
        return None;
    }

    let mut modules = vec![];
    for (module_id, expr) in module_map {
        let fun_statement = ast.statement_expression(SPAN, expr.clone_in(allocator));

        let mut program = ast.program(
            SPAN,
            program_source_type,
            None,
            program_directives.clone_in(allocator),
            ast.vec1(fun_statement),
        );

        let mut fun_renamer = FunctionToProgram::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        let ret = WebPack5::new(allocator, source_text).build(&mut program);

        modules.push(Module::new(
            module_id.to_string(),
            module_id == "entry.js",
            program,
        ));
        errors.extend(ret.errors);
    }

    Some(UnpackReturn {
        modules,
        module_mapping: None,
        errors,
    })
}

struct WebPack5<'a> {
    ctx: ModuleCtx<'a>,
    allocator: &'a Allocator,
}

#[derive(Debug)]
struct Webpack5Return {
    pub errors: std::vec::Vec<OxcDiagnostic>,
}

impl<'a> WebPack5<'a> {
    pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
        let ctx = ModuleCtx::new(source_text);

        Self { allocator, ctx }
    }

    pub fn build(self, program: &mut Program<'a>) -> Webpack5Return {
        let (symbols, scopes) = SemanticBuilder::new("")
            .build(program)
            .semantic
            .into_symbol_table_and_scope_tree();
        self.build_with_symbols_and_scopes(symbols, scopes, program)
    }

    pub fn build_with_symbols_and_scopes(
        self,
        symbols: SymbolTable,
        scopes: ScopeTree,
        program: &mut Program<'a>,
    ) -> Webpack5Return {
        let mut ctx = TraverseCtx::new(scopes, symbols, self.allocator);

        let mut webpack5 = Webpack5Impl::new(&self.ctx);
        webpack5.build(program, &mut ctx);
        Webpack5Return {
            errors: self.ctx.take_errors(),
        }
    }
}

struct Webpack5Impl<'a, 'ctx> {
    ctx: &'ctx ModuleCtx<'a>,
}

impl<'a, 'ctx> Webpack5Impl<'a, 'ctx> {
    pub fn new(ctx: &'ctx ModuleCtx<'a>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        oxc_traverse::walk_program(self, program, ctx);
    }
}

impl<'a> RequireR<'a> for Webpack5Impl<'a, '_> {}

impl<'a> RequireD5<'a> for Webpack5Impl<'a, '_> {
    fn handle_export(&self, export_name: Atom<'a>, export_value: Expression<'a>) {
        self.ctx
            .module_exports
            .insert_export(export_name, export_value);
    }

    fn error(&self, error: OxcDiagnostic) {
        self.ctx.error(error);
    }
}

impl<'a> Traverse<'a> for Webpack5Impl<'a, '_> {
    fn exit_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        program
            .body
            .retain(|s| !matches!(s, Statement::EmptyStatement(_)));

        if *self.ctx.is_esm.borrow() {
            let statements = self.ctx.module_exports.gen_esm_exports(&ctx.ast);
            program.body.extend(statements);
        } else if let Some(statement) = self.ctx.module_exports.gen_cjs_exports(&ctx.ast) {
            program.body.push(statement);
        }
    }

    fn enter_statement(&mut self, statement: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(es) = statement else {
            return;
        };

        let es_span = es.span;
        let expr = &mut es.expression;
        if self.is_esm(expr, ctx) {
            self.ctx.is_esm.replace(true);
            *statement = ctx.ast.statement_empty(es_span);
        } else if self.get_require_d(expr, ctx) {
            *statement = ctx.ast.statement_empty(es_span);
        } else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if self.is_esm(expr, ctx) || self.get_require_d(expr, ctx) {
                    return false;
                }
                true
            });
        }
    }

    fn exit_statement(&mut self, statement: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        // clean up empty sequence expression
        let Statement::ExpressionStatement(es) = statement else {
            return;
        };
        let Expression::SequenceExpression(expr) = &es.expression else {
            return;
        };
        if expr.expressions.len() == 0 {
            *statement = ctx.ast.statement_empty(es.span);
        }
    }
}
