use std::cell::RefCell;

use indexmap::IndexMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, Expression, ExpressionStatement, IdentifierName, ObjectPropertyKind, Program,
        PropertyKey, Statement,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Span};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unpacker::{
    common::{fun_to_program::FunctionToProgram, utils, ModuleExportsStore},
    Module,
};

pub fn get_modules_form_webpack5<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    let ast = AstBuilder::new(allocator);

    let semantic = SemanticBuilder::new("").build(program).semantic;

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

    let Some(root_id) = nodes.root() else {
        return None;
    };

    let mut module_map = IndexMap::new();
    let mut modules = vec![];

    for node in nodes.iter() {
        match node.kind() {
            AstKind::ExpressionStatement(ExpressionStatement { expression, .. })
                if nodes.parent_id(node.id()) == Some(root_id) =>
            {
                let Expression::CallExpression(call_expr) = expression.without_parentheses() else {
                    continue;
                };

                let Some(fun_body) = utils::get_fun_body(call_expr.callee.without_parentheses())
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

                if let Some(Statement::ExpressionStatement(expr)) = last_st {
                    let Expression::CallExpression(expr) = expr.expression.without_parentheses()
                    else {
                        continue;
                    };

                    let expr = expr.callee.without_parentheses();

                    match expr {
                        Expression::ArrowFunctionExpression(_)
                        | Expression::FunctionExpression(_) => {
                            module_map.insert("entry.js", expr);
                        }
                        _ => {
                            continue;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    for (module_id, expr) in module_map {
        let fun_statement = ast.statement_expression(Span::default(), expr.clone_in(allocator));

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

        let _ret = WebPack5::new(allocator, "").build(&mut program);

        modules.push(Module::new(module_id.to_string(), false, program));
    }

    Some(modules)
}

struct Webpack5Ctx<'a> {
    pub source_text: &'a str,
    pub is_esm: RefCell<bool>,
    pub module_exports: ModuleExportsStore<'a>,
}

impl<'a> Webpack5Ctx<'a> {
    pub fn new(source_text: &'a str) -> Self {
        Self {
            source_text,
            is_esm: RefCell::new(false),
            module_exports: ModuleExportsStore::new(),
        }
    }
}

struct WebPack5<'a> {
    ctx: Webpack5Ctx<'a>,
    allocator: &'a Allocator,
}

#[derive(Debug)]
struct Webpack5Return {
    pub is_esm: bool,
}

impl<'a> WebPack5<'a> {
    pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
        let ctx = Webpack5Ctx::new(source_text);

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
            is_esm: self.ctx.is_esm.take(),
        }
    }
}

struct Webpack5Impl<'a, 'ctx> {
    ctx: &'ctx Webpack5Ctx<'a>,
}

impl<'a, 'ctx> Webpack5Impl<'a, 'ctx> {
    pub fn new(ctx: &'ctx Webpack5Ctx<'a>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        oxc_traverse::walk_program(self, program, ctx);
    }
}

impl<'a> Webpack5Impl<'a, '_> {
    #[inline]
    fn is_esm(&self, expr: &Expression<'a>, _ctx: &TraverseCtx<'a>) -> bool {
        utils::is_esm_helper(expr)
    }

    /**
     * This function will return a map of key and module content.
     *
     * `require.d` is a webpack helper function
     * that defines getter functions for harmony exports.
     * It's used to convert ESM exports to CommonJS exports.
     *
     * Example:
     * ```js
     * require.d(exports, {
     *   "default": getter,
     *   [key]: getter
     * })
     * ```
     */
    fn get_require_d<'b>(&self, expr: &'b Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
        let mut found = false;
        let Expression::CallExpression(call_expr) = expr else {
            return false;
        };
        let Expression::StaticMemberExpression(mem) = &call_expr.callee else {
            return false;
        };
        let (Expression::Identifier(idf), IdentifierName { name, .. }) =
            (&mem.object, &mem.property)
        else {
            return false;
        };
        if name.as_str() != "d" || idf.name != "require" {
            return false;
        };
        let [Argument::Identifier(_), Argument::ObjectExpression(obj)] =
            call_expr.arguments.as_slice()
        else {
            return false;
        };
        for prop in obj.properties.iter() {
            let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                return false;
            };
            let k = match &obj_prop.key {
                PropertyKey::StringLiteral(s) => &s.value,
                PropertyKey::StaticIdentifier(s) => &s.name,
                _ => {
                    return false;
                }
            };

            let body = match &obj_prop.value.without_parentheses() {
                Expression::FunctionExpression(s) => s.body.as_ref(),
                Expression::ArrowFunctionExpression(s) => Some(&s.body),
                _ => {
                    return false;
                }
            };

            let Some(body) = body else {
                // TO-DO
                // if (defineObject.properties.length === 0) {
                //     path.prune()
                // }
                return false;
            };
            if body.statements.len() == 1 {
                match &body.statements[0] {
                    Statement::ReturnStatement(ret) => {
                        if let Some(arg) = &ret.argument {
                            self.ctx
                                .module_exports
                                .insert_export(k.clone(), arg.clone_in(ctx.ast.allocator));
                            found = true;
                        }
                    }
                    Statement::ExpressionStatement(es) => {
                        let arg = es.expression.without_parentheses();
                        if matches!(
                            arg,
                            Expression::Identifier(_) | Expression::StaticMemberExpression(_)
                        ) {
                            self.ctx
                                .module_exports
                                .insert_export(k.clone(), arg.clone_in(ctx.ast.allocator));
                            found = true;
                        }
                    }
                    _ => {}
                }
            }
        }
        found
    }
}

impl<'a> Traverse<'a> for Webpack5Impl<'a, '_> {
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

    fn enter_statement(&mut self, statement: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(es) = statement else {
            return;
        };

        let es_span = es.span;
        let expr = &mut es.expression;
        // if the expression is a esm helper function, set is_esm to true and replace the statement with empty statement
        if self.is_esm(expr, ctx) {
            self.ctx.is_esm.replace(true);
            *statement = ctx.ast.statement_empty(es_span);
        } else if self.get_require_d(expr, ctx) {
            *statement = ctx.ast.statement_empty(es_span);
        } else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if self.get_require_d(expr, ctx) {
                    return false;
                }
                true
            });
        }
    }

    fn exit_statement(&mut self, statement: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
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
