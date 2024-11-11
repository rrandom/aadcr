use indexmap::IndexMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, CallExpression, Expression, LogicalOperator,
        MemberExpression, ObjectPropertyKind, Program, PropertyKey, Statement,
    },
    AstBuilder, AstKind,
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::{Atom, Span};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unpacker::{
    common::{fun_to_program::FunctionToProgram, ModuleCtx},
    Module, UnpackResult, UnpackReturn,
};

use super::{RequireD4, RequireD5, RequireR};

pub fn get_modules_form_jsonp<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> UnpackResult<'a> {
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

    nodes.root()?;

    let mut module_map = IndexMap::new();

    let self_variable_names = ["self", "window"];

    for node in nodes.iter() {
        if let AstKind::CallExpression(CallExpression {
            callee, arguments, ..
        }) = node.kind()
            && let Expression::StaticMemberExpression(callee) = callee.without_parentheses()
            && let [Argument::ArrayExpression(arg)] = arguments.as_slice()
            && arg.elements.len() >= 2
            && let [ArrayExpressionElement::ArrayExpression(_chunk_ids), ArrayExpressionElement::ObjectExpression(more_modules)] =
                &arg.elements.as_slice()[0..2]
            && let Expression::AssignmentExpression(assign_expr) =
                callee.object.without_parentheses()
            && "push" == callee.property.name.as_str()
            && more_modules.properties.len() > 0
        {
            let props = &more_modules.properties;

            if !props.iter().all(|prop| {
                let ObjectPropertyKind::ObjectProperty(prop) = prop else {
                    return false;
                };
                matches!(
                    prop.key,
                    PropertyKey::StringLiteral(_) | PropertyKey::NumericLiteral(_)
                ) && matches!(
                    prop.value.without_parentheses(),
                    Expression::FunctionExpression(_)
                )
            }) {
                continue;
            }

            if let Some(MemberExpression::StaticMemberExpression(assign_left)) =
                assign_expr.left.as_member_expression()
                && let Expression::LogicalExpression(or_expr) = &assign_expr.right
                && let Expression::Identifier(idf) = assign_left.object.without_parentheses()
                && or_expr.operator == LogicalOperator::Or
                && let Expression::StaticMemberExpression(or_left) =
                    or_expr.left.without_parentheses()
                && let Expression::ArrayExpression(or_right) = or_expr.right.without_parentheses()
                && self_variable_names.contains(&idf.name.as_str())
                && let Expression::Identifier(or_left_obj) = or_left.object.without_parentheses()
                && self_variable_names.contains(&or_left_obj.name.as_str())
                && or_right.elements.len() == 0
            {
                for prop in more_modules.properties.iter() {
                    let ObjectPropertyKind::ObjectProperty(prop) = prop else {
                        unreachable!()
                    };

                    let module_id = match &prop.key {
                        PropertyKey::StringLiteral(s) => s.value.to_string(),
                        PropertyKey::NumericLiteral(n) => n.value.to_string(),
                        _ => unreachable!(),
                    };

                    module_map.insert(module_id, &prop.value);
                }
            }
        }
    }

    if module_map.is_empty() {
        return None;
    }

    let mut errors = vec![];

    let mut modules = vec![];
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

        let ret = WebPackJsonp::new(allocator, "").build(&mut program);

        modules.push(Module::new(module_id.to_string(), false, program));

        errors.extend(ret.errors);
    }

    Some(UnpackReturn {
        modules,
        module_mapping: None,
        errors,
    })
}

struct WebPackJsonp<'a> {
    ctx: ModuleCtx<'a>,
    allocator: &'a Allocator,
}

struct WebpackJsonpReturn {
    pub errors: std::vec::Vec<OxcDiagnostic>,
}

impl<'a> WebPackJsonp<'a> {
    pub fn new(allocator: &'a Allocator, source_text: &'a str) -> Self {
        let ctx = ModuleCtx::new(source_text);

        Self { allocator, ctx }
    }

    pub fn build(self, program: &mut Program<'a>) -> WebpackJsonpReturn {
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
    ) -> WebpackJsonpReturn {
        let mut ctx = TraverseCtx::new(scopes, symbols, self.allocator);

        let mut webpack_jsonp = WebpackJsonpImpl::new(&self.ctx);
        webpack_jsonp.build(program, &mut ctx);
        WebpackJsonpReturn {
            errors: self.ctx.take_errors(),
        }
    }
}

struct WebpackJsonpImpl<'a, 'ctx> {
    ctx: &'ctx ModuleCtx<'a>,
}

impl<'a, 'ctx> WebpackJsonpImpl<'a, 'ctx> {
    pub fn new(ctx: &'ctx ModuleCtx<'a>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        oxc_traverse::walk_program(self, program, ctx);
    }
}

impl<'a> RequireR<'a> for WebpackJsonpImpl<'a, '_> {}
impl<'a> RequireD4<'a> for WebpackJsonpImpl<'a, '_> {
    fn handle_export(&self, export_name: Atom<'a>, export_value: Expression<'a>) {
        self.ctx
            .module_exports
            .insert_export(export_name, export_value);
    }

    fn error(&self, error: OxcDiagnostic) {
        self.ctx.error(error);
    }
}
impl<'a> RequireD5<'a> for WebpackJsonpImpl<'a, '_> {
    fn handle_export(&self, export_name: Atom<'a>, export_value: Expression<'a>) {
        self.ctx
            .module_exports
            .insert_export(export_name, export_value);
    }

    fn error(&self, error: OxcDiagnostic) {
        self.ctx.error(error);
    }
}

impl<'a> WebpackJsonpImpl<'a, '_> {
    fn get_require_d_webpack5<'b>(&self, expr: &'b Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
        RequireD5::get_require_d(self, expr, ctx)
    }
    fn get_require_d_webpack4<'b>(&self, expr: &'b Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
        RequireD4::get_require_d(self, expr, ctx)
    }
}

impl<'a> Traverse<'a> for WebpackJsonpImpl<'a, '_> {
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
        } else if self.get_require_d_webpack5(expr, ctx) || self.get_require_d_webpack4(expr, ctx) {
            *statement = ctx.ast.statement_empty(es_span);
        } else if let Expression::SequenceExpression(seq) = expr {
            seq.expressions.retain(|expr| {
                if self.is_esm(expr, ctx) || self.get_require_d_webpack5(expr, ctx) || self.get_require_d_webpack4(expr, ctx)
                {
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
