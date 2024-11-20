use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        ArrayExpressionElement, AssignmentExpression, Expression, MemberExpression, Program,
        Statement,
    },
    AstBuilder, AstKind,
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Atom, GetSpan, Span, SPAN};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::{
    Module,
    common::{fun_to_program::FunctionToProgram, ModuleCtx},
    UnpackResult, UnpackReturn,
};

use super::{RequireD4, RequireR};

pub fn get_modules_form_webpack4<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> UnpackResult<'a> {
    let ast = AstBuilder::new(allocator);

    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = None;
    let mut entry_ids = vec![];
    let mut factory_callee_span = Some(SPAN);
    let mut module_funs = vec![];

    let nodes = semantic.nodes();
    let program_source_type = program.source_type;
    let program_directives = &program.directives;

    for node in nodes.iter() {
        match node.kind() {
            AstKind::CallExpression(call_expr) => {
                if let Some(Expression::ArrayExpression(arr)) = call_expr
                    .arguments
                    .first()
                    .and_then(|arg| arg.as_expression())
                    .map(|expr| expr.without_parentheses())
                    && call_expr.arguments.len() == 1
                    && factory_id.is_none()
                {
                    let all_is_fun = arr
                        .elements
                        .iter()
                        .all(|ele| matches!(ele, ArrayExpressionElement::FunctionExpression(_)));

                    if all_is_fun {
                        factory_id = Some(node.id());
                        factory_callee_span = Some(call_expr.callee.without_parentheses().span());
                        for fun in arr.elements.iter() {
                            let ArrayExpressionElement::FunctionExpression(fun) = fun else {
                                unreachable!()
                            };
                            module_funs.push(fun);
                        }
                    }
                }
            }
            AstKind::AssignmentExpression(AssignmentExpression {
                left,
                right: Expression::NumericLiteral(num),
                ..
            }) => {
                // span based comparison of factory callee
                let Some(factory_callee_span) = factory_callee_span else {
                    continue;
                };
                for parent in nodes.iter_parents(node.id()) {
                    if let AstKind::CallExpression(s) = parent.kind()
                        && let callee_span = s.callee.without_parentheses().span()
                        && callee_span == factory_callee_span
                        && let Some(MemberExpression::StaticMemberExpression(mem_expr)) =
                            left.as_member_expression()
                        && mem_expr.object.is_identifier_reference()
                        && mem_expr.property.name.as_str() == "s"
                    {
                        entry_ids.push(num.value);
                    }
                }
            }
            _ => {}
        }
    }

    if module_funs.is_empty() {
        return None;
    }

    let mut modules = vec![];
    let mut errors = vec![];

    for (module_id, fun) in module_funs.iter().enumerate() {
        let new_fun = fun.clone_in(allocator);

        let fun_statement =
            ast.statement_expression(fun.span, ast.expression_from_function(new_fun));

        let mut program = ast.program(
            SPAN,
            program_source_type,
            None,
            program_directives.clone_in(allocator),
            ast.vec1(fun_statement),
        );
        let mut fun_renamer = FunctionToProgram::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        let ret = WebPack4::new(allocator, "").build(&mut program);

        let is_entry = entry_ids.contains(&(module_id as f64));

        modules.push(Module::new(module_id.to_string(), is_entry, program));
        errors.extend(ret.errors);
    }

    Some(UnpackReturn {
        modules,
        module_mapping: None,
        errors,
    })
}

struct WebPack4<'a> {
    ctx: ModuleCtx<'a>,
    allocator: &'a Allocator,
}

#[derive(Debug)]
struct Webpack4Return {
    pub errors: std::vec::Vec<OxcDiagnostic>,
}

impl<'a> WebPack4<'a> {
    pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
        let ctx = ModuleCtx::new(source_text);

        Self { allocator, ctx }
    }

    pub fn build(self, program: &mut Program<'a>) -> Webpack4Return {
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
    ) -> Webpack4Return {
        let mut ctx = TraverseCtx::new(scopes, symbols, self.allocator);

        let mut webpack4 = Webpack4Impl::new(&self.ctx);
        webpack4.build(program, &mut ctx);
        Webpack4Return {
            errors: self.ctx.take_errors(),
        }
    }
}

struct Webpack4Impl<'a, 'ctx> {
    ctx: &'ctx ModuleCtx<'a>,
}

impl<'a, 'ctx> Webpack4Impl<'a, 'ctx> {
    pub fn new(ctx: &'ctx ModuleCtx<'a>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        oxc_traverse::walk_program(self, program, ctx);
    }
}

impl<'a> RequireR<'a> for Webpack4Impl<'a, '_> {}
impl<'a> RequireD4<'a> for Webpack4Impl<'a, '_> {
    fn handle_export(&self, export_name: Atom<'a>, export_value: Expression<'a>) {
        self.ctx
            .module_exports
            .insert_export(export_name, export_value);
    }

    fn error(&self, error: OxcDiagnostic) {
        self.ctx.error(error);
    }
}

impl<'a> Traverse<'a> for Webpack4Impl<'a, '_> {
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

    fn enter_expression_statement(
        &mut self,
        node: &mut oxc_ast::ast::ExpressionStatement<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let expr = &node.expression;
        let parent = ctx.parent();
        if self.get_require_d(expr, ctx) {
            if parent.is_expression_statement() || parent.is_sequence_expression() {
            } else {
                panic!("Found unhandled require.d: {:?}", parent);
            }
        }
    }

    fn enter_statement(&mut self, node: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(es) = node else {
            return;
        };
        let es_span: Span = es.span;
        let expr = &mut es.expression;

        if self.is_esm(expr, ctx) {
            self.ctx.is_esm.replace(true);
            *node = ctx.ast.statement_empty(es_span);
        } else if self.get_require_d(expr, ctx) {
            *node = ctx.ast.statement_empty(es_span);
        } else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if self.is_esm(expr, ctx) || self.get_require_d(expr, ctx) {
                    return false;
                }
                true
            });
        }
    }

    // clear empty sequence expression
    fn exit_statement(&mut self, node: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(es) = node else {
            return;
        };
        let Expression::SequenceExpression(expr) = &es.expression else {
            return;
        };
        if expr.expressions.len() == 0 {
            *node = ctx.ast.statement_empty(es.span);
        }
    }
}
