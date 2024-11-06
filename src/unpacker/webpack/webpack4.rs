use std::cell::RefCell;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, AssignmentExpression, Expression, IdentifierName,
        MemberExpression, Program, Statement,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{NodeId, ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Atom, GetSpan, Span};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unpacker::common::{fun_to_program::FunctionToProgram, utils, ModuleExportsStore};
use crate::unpacker::Module;

pub fn get_modules_form_webpack4<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    let ast = AstBuilder::new(allocator);

    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = None;
    let mut entry_ids = vec![];
    let mut factory_callee_span = Some(Span::default());

    let mut module_funs = vec![];
    let mut modules = vec![];

    let nodes = semantic.nodes();
    let program_source_type = nodes
        .root()
        .map(|id| nodes.get_node(id).kind().as_program().unwrap().source_type);
    let program_directives = nodes.root().map(|id| {
        nodes
            .get_node(id)
            .kind()
            .as_program()
            .unwrap()
            .directives
            .clone_in(allocator)
    });

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

    for (module_id, fun) in module_funs.iter().enumerate() {
        let new_fun = fun.clone_in(allocator);

        let fun_statement =
            ast.statement_expression(fun.span, ast.expression_from_function(new_fun));

        let directives = program_directives
            .clone_in(allocator)
            .unwrap_or_else(|| ast.vec());

        let mut program = ast.program(
            Span::default(),
            program_source_type.unwrap().clone_in(allocator),
            None,
            directives,
            ast.vec1(fun_statement),
        );
        let mut fun_renamer = FunctionToProgram::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        let _ret = WebPack4::new(allocator, "").build(&mut program);

        let is_entry = entry_ids.contains(&(module_id as f64));

        modules.push(Module::new(module_id.to_string(), is_entry, program));
    }

    Some(modules)
}

struct Webpack4Ctx<'a> {
    pub source_text: &'a str,
    pub is_esm: RefCell<bool>,
    pub module_exports: ModuleExportsStore<'a>,
}

impl<'a> Webpack4Ctx<'a> {
    pub fn new(source_text: &'a str) -> Self {
        Self {
            source_text,
            is_esm: RefCell::new(false),
            module_exports: ModuleExportsStore::new(),
        }
    }
}
struct WebPack4<'a> {
    ctx: Webpack4Ctx<'a>,
    allocator: &'a Allocator,
}

#[derive(Debug)]
struct Webpack4Return {
    pub is_esm: bool,
}

impl<'a> WebPack4<'a> {
    pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
        let ctx = Webpack4Ctx::new(source_text);

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
            is_esm: self.ctx.is_esm.take(),
        }
    }
}

struct Webpack4Impl<'a, 'ctx> {
    ctx: &'ctx Webpack4Ctx<'a>,
}

impl<'a, 'ctx> Webpack4Impl<'a, 'ctx> {
    pub fn new(ctx: &'ctx Webpack4Ctx<'a>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        oxc_traverse::walk_program(self, program, ctx);
    }
}

impl<'a> Webpack4Impl<'a, '_> {
    #[inline]
    fn is_esm(&self, expr: &Expression<'a>, _ctx: &TraverseCtx<'a>) -> bool {
        utils::is_esm_helper(expr)
    }

    /// if a call expression is `require.d`, return true, and add exports to module_exports
    /// ```js
    /// require.d(exports, "a", function() {
    ///     return moduleContent;
    /// })
    /// ```
    fn get_require_d<'b>(
        &self,
        expr: &'b Expression<'a>,
        _ctx: &TraverseCtx<'a>,
    ) -> Option<(Atom<'a>, &'b Expression<'a>)> {
        let Expression::CallExpression(call_expr) = expr else {
            return None;
        };
        let Expression::StaticMemberExpression(mem) = &call_expr.callee else {
            return None;
        };
        let (Expression::Identifier(idf), IdentifierName { name, .. }) =
            (&mem.object, &mem.property)
        else {
            return None;
        };
        if name.as_str() != "d" || idf.name != "require" {
            return None;
        };
        if call_expr.arguments.len() != 3 {
            return None;
        };
        let [Argument::Identifier(_), Argument::StringLiteral(name), Argument::FunctionExpression(f)] =
            call_expr.arguments.as_slice()
        else {
            return None;
        };
        let Some(body) = &f.body else { return None };
        if body.statements.len() != 1 {
            return None;
        };
        let Statement::ReturnStatement(ret) = &body.statements[0] else {
            return None;
        };
        let Some(arg) = &ret.argument else {
            return None;
        };

        Some((name.value.clone(), arg))
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
        let p = ctx.parent();
        if let Some((name, arg)) = self.get_require_d(expr, ctx) {
            if p.is_expression_statement() {
            } else if p.is_sequence_expression() {
                return;
            } else {
                println!("Found unhandled require.d: {:?}: {:?}, {:?}", p, name, arg);
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
        } else if let Some((name, arg)) = self.get_require_d(expr, ctx) {
            self.ctx
                .module_exports
                .insert_export(name, arg.clone_in(ctx.ast.allocator));

            *node = ctx.ast.statement_empty(es_span);
        } else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if let Some((name, arg)) = self.get_require_d(expr, ctx) {
                    self.ctx
                        .module_exports
                        .insert_export(name, arg.clone_in(ctx.ast.allocator));
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
