use std::cell::RefCell;

use oxc_allocator::{Allocator, Box, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, AssignmentExpression, AssignmentOperator, Expression,
        IdentifierName, ImportOrExportKind, MemberExpression, Program, PropertyKind, Statement,
        VariableDeclarationKind, WithClause,
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
    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = None;
    let mut entry_ids = vec![];
    let mut factory_callee_id = NodeId::DUMMY;

    let mut module_funs = vec![];

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
            // TO-DO: using `.root` should work too
            AstKind::Program(Program { .. }) => {}
            AstKind::CallExpression(call) => {
                if call.arguments.len() == 1 {
                    if let Expression::ArrayExpression(arr) =
                        call.arguments[0].as_expression().unwrap()
                    {
                        let all_is_fun = arr
                            .elements
                            .iter()
                            .all(|d| matches!(d, ArrayExpressionElement::FunctionExpression(_)));
                        if all_is_fun && factory_id.is_none() {
                            // println!("Found? {:?}", node.id());
                            factory_id = Some(node.id());
                            println!("arr len: {:?}", arr.elements.len());
                            for fun in arr.elements.iter() {
                                if let ArrayExpressionElement::FunctionExpression(fun) = fun {
                                    // println!("fun: {:#?}", fun);
                                    module_funs.push(fun);
                                }
                            }
                        }
                    }
                }
            }

            // TO-DO: using without_parentheses
            AstKind::ParenthesizedExpression(pe) => {
                if let Some(id) = nodes.parent_id(node.id()) {
                    if Some(id) == factory_id {
                        factory_callee_id = node.id();
                    }
                }
            }
            AstKind::AssignmentExpression(AssignmentExpression {
                left,
                right: Expression::NumericLiteral(s),
                ..
            }) => {
                let contains = semantic
                    .nodes()
                    .ancestors(node.id())
                    .any(|id| id == factory_callee_id);
                if !contains {
                    continue;
                }
                if let Some(MemberExpression::StaticMemberExpression(mm)) =
                    left.as_member_expression()
                {
                    if mm.object.is_identifier_reference() && mm.property.name.as_str() == "s" {
                        // println!("Member: {:?}", mm);
                        // println!(
                        //     "assign: {} with {}",
                        //     // span.source_text(semantic.source_text()),
                        //     1,
                        //     s.value
                        // );
                        entry_ids.push(s.value);
                    }
                }
            }
            _ => {}
        }
    }

    // println!(
    //     "factory: {:?} ard_id: {:?}, ae_id: {:?}, module_fun_ids: {:?}, entry_ids: {:?} and moduleFuns: {:?}",
    //     factory_id, factory_arg_id, factory_arg_ele_id, module_fun_ids, entry_ids, module_funs
    // );

    println!("module_funs len: {:?}", module_funs.len());

    let mut modules = vec![];
    for (module_id, fun) in module_funs.iter().enumerate() {
        let new_fun = fun.clone_in(allocator);

        let ast = AstBuilder::new(allocator);
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

        let ret = WebPack4::new(allocator, "").build(&mut program);
        // println!("ret: {:?}", ret);

        let is_entry = entry_ids.contains(&(module_id as f64));

        modules.push(Module::new(module_id.to_string(), is_entry, program));
    }

    Some(modules)
}

// a export is a return statement argument or a identifier reference
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
    fn is_esm(&self, expr: &Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
        utils::is_esm_helper(expr)
    }

    // require.d is a helper function defines getter functions for harmony exports, which convert esm exports to cjs
    // example:
    // require.d(exports, "a", function() {
    //     return moduleContent;
    // })
    fn get_require_d<'b>(
        &self,
        expr: &'b Expression<'a>,
        ctx: &TraverseCtx<'a>,
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
    fn enter_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        // println!("enter program");
    }

    fn exit_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        program
            .body
            .retain(|s| !matches!(s, Statement::EmptyStatement(_)));

        if *self.ctx.is_esm.borrow() {
            // Generate export { ... }
            let statements = self.ctx.module_exports.gen_esm_exports(&ctx.ast);
            program.body.extend(statements);
        } else {
            // Generate module.exports = { ... }
            if let Some(statement) = self.ctx.module_exports.gen_cjs_exports(&ctx.ast) {
                program.body.push(statement);
            }
        }
    }

    fn enter_identifier_reference(
        &mut self,
        idf: &mut oxc_ast::ast::IdentifierReference<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
    }

    fn enter_call_expression(
        &mut self,
        call_expr: &mut oxc_ast::ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // println!("call_expr: {:?}", call_expr);
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
                println!("Found unhandled require_d: {:?}: {:?}, {:?}", p, name, arg);
            }
        }
    }

    fn enter_expression(&mut self, expr: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if self.is_esm(expr, ctx) {
            // ctx.ast.empty_statement(expr.span());
            // *expr = ctx.ast.void_0(expr.span());
        }
    }

    fn enter_statement(&mut self, node: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(es) = node else {
            return;
        };
        let es_span = es.span;
        let expr = &mut es.expression;
        // if the expression is a esm helper function, set is_esm to true and replace the statement with empty statement
        if self.is_esm(expr, ctx) {
            self.ctx.is_esm.replace(true);
            *node = ctx.ast.statement_empty(es_span);
        } else if let Some((name, arg)) = self.get_require_d(expr, ctx) {
            // println!("is require_d: {:?}", expr);
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
