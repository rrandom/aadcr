use std::cell::RefCell;

use indexmap::IndexMap;

use oxc_allocator::{Allocator, Box, CloneIn};
use oxc_ast::{
    ast::{
        Argument, AssignmentOperator, CallExpression, Expression, ExpressionStatement, IdentifierName, ImportOrExportKind, ObjectPropertyKind, Program, PropertyKey, PropertyKind, Statement, TSTypeAnnotation, VariableDeclarationKind, WithClause
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{NodeId, ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Atom, GetSpan, Span};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unpacker::{
    common::{fun_renamer::FunctionParamRenamer, ModuleExportsStore},
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

    let mut webpackBootstrap = NodeId::DUMMY;

    // println!("{:#?}", nodes.get_node(root_id));

    let mut moduleMap = IndexMap::new();
    let mut modules = vec![];

    for node in nodes.iter() {
        match node.kind() {
            AstKind::ExpressionStatement(ExpressionStatement { expression, .. })
                if nodes.parent_id(node.id()) == Some(root_id) =>
            {
                let Expression::CallExpression(call_expr) = expression.without_parentheses() else {
                    continue;
                };
                match call_expr.callee.without_parentheses() {
                    Expression::FunctionExpression(fun) => {
                        let Some(body) = &fun.body else {
                            continue;
                        };
                        for st in body.statements.iter() {
                            let Statement::VariableDeclaration(vardecl) = st else {
                                continue;
                            };
                            // println!("====> vardecl {:?}", vardecl);
                            let de = &vardecl.declarations[0];

                            let Some(init) = &de.init else {
                                continue;
                            };
                            let Expression::ObjectExpression(ob) = init.without_parentheses()
                            else {
                                continue;
                            };
                            for prop in ob.properties.iter() {
                                let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                                    continue;
                                };
                                // println!("====> obj_prop {:?}", obj_prop.key);
                            }
                        }
                    }
                    Expression::ArrowFunctionExpression(fun) => {
                        let body = &fun.body;
                        for st in body.statements.iter() {
                            let Statement::VariableDeclaration(vardecl) = st else {
                                continue;
                            };
                            // println!("====> vardecl {:?}", vardecl);
                            let de = &vardecl.declarations[0];

                            let Some(init) = &de.init else {
                                continue;
                            };
                            let Expression::ObjectExpression(ob) = init.without_parentheses()
                            else {
                                continue;
                            };
                            for prop in ob.properties.iter() {
                                let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                                    continue;
                                };
                                // println!("====> obj_prop {:?}", obj_prop.value);
                                let Some(Expression::StringLiteral(s)) =
                                    obj_prop.key.as_expression()
                                else {
                                    continue;
                                };
                                let module_id = &s.value;
                                // println!("====> module_id {:?}", module_id);

                                let expr = obj_prop.value.without_parentheses();

                                match expr {
                                    Expression::ArrowFunctionExpression(_)
                                    | Expression::FunctionExpression(_) => {}
                                    _ => {
                                        continue;
                                    }
                                }

                                moduleMap.insert(module_id, expr);
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    for (module_id, expr) in moduleMap.iter() {
        // println!("====> module_id {:?} {:?}", module_id, expr);

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

        let mut fun_renamer =
            FunctionParamRenamer::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        let ret = WebPack5::new(allocator, "").build(&mut program);

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
    // require.r exists
    // require.r is a webpack4 helper function defines `__esModule` an exports object
    fn is_esm(&self, expr: &Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
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
        idf.name == "require" && name.as_str() == "r"
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
        let [Argument::Identifier(_), Argument::ObjectExpression(obj)] =
            call_expr.arguments.as_slice()
        else {
            return None;
        };
        for prop in obj.properties.iter() {
            let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                return None;
            };
            let k = match &obj_prop.key {
                PropertyKey::StringLiteral(s) => &s.value,
                PropertyKey::StaticIdentifier(s) => &s.name,
                _ => {
                    return None;
                }
            };

            let body = match &obj_prop.value.without_parentheses() {
                Expression::FunctionExpression(s) => s.body.as_ref(),
                Expression::ArrowFunctionExpression(s) => Some(&s.body),
                _ => {
                    return None;
                }
            };

            let Some(body) = body else {
                // TO-DO
                // if (defineObject.properties.length === 0) {
                //     path.prune()
                // }
                return None;
            };
            if body.statements.len() == 1 {
                match &body.statements[0] {
                    Statement::ReturnStatement(ret) => {
                        if let Some(arg) = &ret.argument {
                            self.ctx
                                .module_exports
                                .insert_export(k.clone(), arg.clone_in(ctx.ast.allocator));
                            return Some((k.clone(), arg));
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
                            return Some((k.clone(), arg));
                        }
                    }
                    _ => {}
                }
            }
        }
        // TO-DO
        None
    }
}

impl<'a> Traverse<'a> for Webpack5Impl<'a, '_> {
    fn enter_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        // println!("enter program");
    }

    fn exit_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        program
            .body
            .retain(|s| !matches!(s, Statement::EmptyStatement(_)));
        if *self.ctx.is_esm.borrow() {
            // println!("is esm: {:?}", self.ctx.is_esm.borrow());
            self.ctx
                .module_exports
                .exports
                .borrow()
                .iter()
                .for_each(|(k, v)| {
                    // println!("export: {:?} => {:?}", k, v);
                    let bindingidkind = ctx.ast.binding_pattern_kind_binding_identifier(
                        Span::default(),
                        k.clone_in(ctx.ast.allocator),
                    );
                    let bd = ctx.ast.binding_pattern(
                        bindingidkind,
                        None::<Box<_>>,
                        false,
                    );
                    let de = ctx.ast.variable_declarator(
                        v.span(),
                        VariableDeclarationKind::Const,
                        bd,
                        Some(v.clone_in(ctx.ast.allocator)),
                        false,
                    );
                    let de = ctx.ast.variable_declaration(
                        v.span(),
                        VariableDeclarationKind::Const,
                        ctx.ast.vec1(de),
                        false,
                    );
                    let de = ctx.ast.declaration_from_variable(de);
                    let st = ctx.ast.alloc_export_named_declaration(
                        v.span(),
                        Some(de),
                        ctx.ast.vec(),
                        None,
                        ImportOrExportKind::Value,
                        None::<WithClause<'a>>,
                    );
                    let st = Statement::ExportNamedDeclaration(st);
                    program.body.push(st);
                });
        } else {
            if self.ctx.module_exports.exports.borrow().is_empty() {
                return;
            }

            let properties =
                ctx.ast
                    .vec_from_iter(self.ctx.module_exports.exports.borrow().iter().map(
                        |(k, v)| {
                            ctx.ast.object_property_kind_object_property(
                                Span::default(),
                                PropertyKind::Init,
                                ctx.ast.property_key_identifier_name(
                                    Span::default(),
                                    k.clone_in(ctx.ast.allocator),
                                ),
                                v.clone_in(ctx.ast.allocator),
                                None,
                                false,
                                false,
                                false,
                            )
                        },
                    ));

            let inner = ctx.ast.member_expression_from_static(
                ctx.ast.static_member_expression(
                    Span::default(),
                    ctx.ast
                        .expression_identifier_reference(Span::default(), "module"),
                    ctx.ast.identifier_name(Span::default(), "exports"),
                    false,
                ),
            );
            let inner = ctx.ast.simple_assignment_target_member_expression(inner);

            let right = ctx.ast.expression_object(Span::default(), properties, None);
            let exp = ctx.ast.expression_assignment(
                Span::default(),
                AssignmentOperator::Assign,
                ctx.ast.assignment_target_simple(inner),
                right,
            );
            let s = ctx.ast.statement_expression(Span::default(), exp);
            program.body.push(s);
        }
    }

    fn enter_expression_statement(
        &mut self,
        node: &mut oxc_ast::ast::ExpressionStatement<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // TO-DO
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
            *node = ctx.ast.statement_empty(es_span);
        }else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if let Some((name, arg)) = self.get_require_d(expr, ctx) {
                    return false;
                }
                true
            });
        }
    }

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
