use rustc_hash::FxHashMap;
use std::cell::RefCell;

use oxc_allocator::{Allocator, Box, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, AssignmentExpression, AssignmentOperator,
        BindingPatternKind, Expression, IdentifierName, IdentifierReference, ImportOrExportKind,
        MemberExpression, Program, PropertyKind, Statement, TSTypeAnnotation,
        VariableDeclarationKind, WithClause,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{NodeId, ScopeTree, SemanticBuilder, SymbolId, SymbolTable};
use oxc_span::{Atom, GetSpan, Span};
use oxc_traverse::{Traverse, TraverseCtx};

pub struct Module<'a> {
    pub id: usize,
    pub is_entry: bool,
    pub content: Program<'a>,
}

pub fn get_modules_form_webpack4<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = NodeId::DUMMY;
    let mut factory_callee_id = NodeId::DUMMY;
    let mut factory_arg_id = NodeId::DUMMY;
    let mut factory_arg_ele_id = NodeId::DUMMY;

    let mut entry_ids = vec![];

    let mut module_fun_ids = vec![];
    let mut module_funs: FxHashMap<NodeId, std::vec::Vec<SymbolId>> = FxHashMap::default();

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
                        if all_is_fun {
                            // println!("Found? {:?}", node.id());
                            factory_id = node.id();
                        }
                    }
                }
            }
            AstKind::ParenthesizedExpression(pe) => {
                if let Some(id) = nodes.parent_id(node.id()) {
                    if id == factory_id {
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
            AstKind::Argument(_) => {
                if let Some(id) = nodes.parent_id(node.id()) {
                    if id == factory_id {
                        factory_arg_id = node.id();
                    }
                }
            }
            AstKind::ArrayExpression(_) => {
                if let Some(id) = nodes.parent_id(node.id()) {
                    if id == factory_arg_id {
                        factory_arg_ele_id = node.id();
                    }
                }
            }
            AstKind::Function(fun) => {
                let mut rec = node.id();
                for _ in 0..3 {
                    let ancestor_id = nodes.parent_id(rec);
                    if ancestor_id.is_none() {
                        break;
                    } else {
                        rec = ancestor_id.unwrap();
                    }
                }
                if factory_arg_ele_id == rec {
                    // println!("Got!");
                    module_fun_ids.push(node.id());

                    for param in fun.params.items.iter() {
                        if let BindingPatternKind::BindingIdentifier(param) = &param.pattern.kind {
                            // println!(
                            //     "Fun: {:?} => bd: {:?}, {:?}",
                            //     node.id(),
                            //     param.name,
                            //     param.symbol_id
                            // );

                            let fun_entry = module_funs.entry(node.id()).or_default(); // 确保获取到 Vec<SymbolId>
                            let sid = param.symbol_id.get().unwrap();
                            fun_entry.push(sid); // 现在可以安全地调用 push
                        }
                    }
                }
            }
            _ => {}
        }
    }

    println!(
        "factory: {:?} ard_id: {:?}, ae_id: {:?}, module_fun_ids: {:?}, entry_ids: {:?} and moduleFuns: {:?}",
        factory_id, factory_arg_id, factory_arg_ele_id, module_fun_ids, entry_ids, module_funs
    );

    let mut modules = vec![];
    for (module_id, fun_id) in module_fun_ids.iter().enumerate() {
        let fun = nodes.get_node(*fun_id).kind().as_function().unwrap();
        let new_fun = fun.clone_in(allocator);

        let ast = AstBuilder::new(allocator);
        let st = ast.statement_expression(fun.span, ast.expression_from_function(new_fun));

        let directives = program_directives
            .clone_in(allocator)
            .unwrap_or_else(|| ast.vec());

        let mut program = ast.program(
            Span::default(),
            program_source_type.unwrap().clone_in(allocator),
            None,
            directives,
            ast.vec1(st),
        );
        let mut fun_renamer =
            FunctionParamRenamer::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        let ret = WebPack4::new(allocator, "").build(&mut program);
        // println!("ret: {:?}", ret);

        let is_entry = entry_ids.contains(&(module_id as f64));

        modules.push(Module {
            id: module_id,
            is_entry,
            content: program,
        });
    }

    Some(modules)
}

struct FunctionParamRenamer<'a> {
    allocator: &'a Allocator,
    param_ids: RefCell<std::vec::Vec<SymbolId>>,
    rename_to: std::vec::Vec<Atom<'a>>,
}

impl<'a> FunctionParamRenamer<'a> {
    pub fn new(
        allocator: &'a Allocator,
        rename_to: impl IntoIterator<Item = impl Into<Atom<'a>>>,
    ) -> Self {
        Self {
            allocator,
            param_ids: RefCell::new(vec![]),
            rename_to: rename_to.into_iter().map(|s| s.into()).collect(),
        }
    }

    pub fn build(&mut self, program: &mut Program<'a>) {
        let (symbols, scopes) = SemanticBuilder::new("")
            .build(program)
            .semantic
            .into_symbol_table_and_scope_tree();
        self.build_with_symbols_and_scopes(symbols, scopes, program)
    }

    pub fn build_with_symbols_and_scopes(
        &mut self,
        symbols: SymbolTable,
        scopes: ScopeTree,
        program: &mut Program<'a>,
    ) {
        let Some(symbol_ids) = self.get_parameter_symbols_id(program) else {
            return;
        };
        self.param_ids.borrow_mut().extend(symbol_ids);

        let mut ctx = TraverseCtx::new(scopes, symbols, self.allocator);

        oxc_traverse::walk_program(self, program, &mut ctx);
    }

    pub fn get_parameter_symbols_id(
        &self,
        program: &Program<'a>,
    ) -> Option<std::vec::Vec<SymbolId>> {
        let mut symbol_ids = vec![];
        match &program.body[0] {
            Statement::ExpressionStatement(es) => match &es.expression {
                Expression::FunctionExpression(f) => {
                    for param in f.params.items.iter() {
                        if let BindingPatternKind::BindingIdentifier(binding) = &param.pattern.kind
                        {
                            if let Some(symbol_id) = binding.symbol_id.get() {
                                symbol_ids.push(symbol_id);
                            }
                        }
                    }
                }
                _ => return None,
            },
            _ => return None,
        }
        Some(symbol_ids)
    }

    pub fn get_symbol_name(&self, id: SymbolId) -> Option<Atom<'a>> {
        self.param_ids
            .borrow()
            .iter()
            .position(|&x| x == id)
            .map(|index| match index {
                index if index < self.rename_to.len() => self.rename_to[index].clone(),
                _ => unreachable!(),
            })
    }
}

impl<'a> Traverse<'a> for FunctionParamRenamer<'a> {
    fn enter_identifier_reference(
        &mut self,
        idf: &mut IdentifierReference<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let Some(id) = idf.reference_id.get() else {
            return;
        };
        let Some(refencer) = ctx.scoping.symbols().references.get(id) else {
            return;
        };
        let Some(s) = refencer.symbol_id() else {
            return;
        };
        if let Some(new_name) = self.get_symbol_name(s) {
            idf.name = new_name;
        }
    }

    fn exit_program(&mut self, node: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Statement::ExpressionStatement(exp) = &node.body[0] {
            let Expression::FunctionExpression(fun) = &exp.expression else {
                return;
            };
            let Some(body) = &fun.body else {
                return;
            };
            node.directives
                .extend(body.directives.clone_in(self.allocator));
            node.body = body.statements.clone_in(self.allocator);
        }
    }
}

// a export is a return statement argument or a identifier reference
struct ModuleExportsStore<'a> {
    pub exports: RefCell<FxHashMap<Atom<'a>, Expression<'a>>>,
}

impl<'a> ModuleExportsStore<'a> {
    pub fn new() -> Self {
        Self {
            exports: RefCell::new(FxHashMap::default()),
        }
    }

    pub fn insert_export(&self, name: Atom<'a>, expr: Expression<'a>) {
        self.exports.borrow_mut().insert(name, expr);
    }
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
        if name.as_str() != "d" {
            return None;
        };
        if idf.name != "require" {
            return None;
        }

        if call_expr.arguments.len() != 3 {
            return None;
        };
        let [Argument::Identifier(_), Argument::StringLiteral(name), Argument::FunctionExpression(f)] =
            call_expr.arguments.as_slice()
        else {
            return None;
        };
        let Some(fb) = &f.body else { return None };
        if fb.statements.len() != 1 {
            return None;
        };
        let Statement::ReturnStatement(ret) = &fb.statements[0] else {
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
        println!("enter program");
    }

    fn exit_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        // println!("exit program");
        // println!(
        //     "module exports: {:?}",
        //     self.ctx.module_exports.exports.borrow()
        // );
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
                        None::<Box<TSTypeAnnotation<'a>>>,
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
                    // println!("st: {:#?}", program.body);
                });
        } else {
            if self.ctx.module_exports.exports.borrow().is_empty() {
                return;
            }

            let properties =
                ctx.ast
                    .vec_from_iter(self.ctx.module_exports.exports.borrow().iter().map(
                        |(k, v)| {
                            return ctx.ast.object_property_kind_object_property(
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
                            );
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

    fn enter_expression(&mut self, expr: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if self.is_esm(expr, ctx) {
            // ctx.ast.empty_statement(expr.span());
            // *expr = ctx.ast.void_0(expr.span());
        }
    }

    fn enter_statement(&mut self, node: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        // println!("enter statement: {:?}", node);
        let Statement::ExpressionStatement(es) = node else {
            return;
        };
        let expr = &es.expression;
        let es_span = es.span;
        if self.is_esm(expr, ctx) {
            self.ctx.is_esm.replace(true);
            *node = ctx.ast.statement_empty(es_span);
        } else if let Some((name, arg)) = self.get_require_d(expr, ctx) {
            // println!("is require_d: {:?}", expr);
            self.ctx
                .module_exports
                .insert_export(name, arg.clone_in(ctx.ast.allocator));

            *node = ctx.ast.statement_empty(es_span);
        }
    }
}
