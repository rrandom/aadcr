use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell};
use std::vec;

use oxc_span::GetSpan;

use oxc_allocator::{Allocator, Vec};
use oxc_allocator::{Box, CloneIn};
use oxc_ast::ast::{
    AssignmentTarget, BindingIdentifier, BindingPatternKind, Function, IdentifierName, Statement,
    StaticMemberExpression, UnaryExpression,
};
use oxc_ast::AstBuilder;
use oxc_ast::{
    ast::{
        ArrayExpressionElement, AssignmentExpression, Directive, Expression, FormalParameter,
        FunctionBody, MemberExpression, NumericLiteral, Program,
    },
    AstKind,
};
use oxc_codegen::CodeGenerator;
use oxc_semantic::{
    NodeId, Reference, ReferenceId, ScopeId, ScopeTree, Semantic, SemanticBuilder, SymbolId,
    SymbolTable,
};
use oxc_span::{Atom, SourceType, Span};
use oxc_traverse::{Ancestor, Traverse, TraverseCtx};

use rustc_hash::FxHashMap;

pub struct Module {}

pub fn get_modules_form_webpack4(allocator: &Allocator, program: &Program) -> Option<Module> {
    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = NodeId::DUMMY;
    let mut factory_callee_id = NodeId::DUMMY;
    let mut factory_arg_id = NodeId::DUMMY;
    let mut factory_arg_ele_id = NodeId::DUMMY;

    let mut entry_ids = Vec::new_in(&allocator);
    let mut arr_expr = None;
    let mut program_source_type = None;
    let mut program_directives = None;

    let mut module_fun_ids = vec![];

    let mut module_funs: FxHashMap<NodeId, std::vec::Vec<SymbolId>> = FxHashMap::default();

    let nodes = semantic.nodes();

    for node in nodes.iter() {
        match node.kind() {
            // TO-DO: using `.root` should work too
            AstKind::Program(Program {
                source_type,
                directives,
                ..
            }) => {
                program_source_type = Some(source_type);
                program_directives = Some(directives);
            }
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
                            println!("Found? {:?}", node.id());
                            factory_id = node.id();
                            arr_expr = Some(arr);
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
                        println!(
                            "assign: {} with {}",
                            // span.source_text(semantic.source_text()),
                            1,
                            s.value
                        );
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
                    // println!("aa: {:?}", ancestor_id);
                }
                if factory_arg_ele_id == rec {
                    println!("Got!");
                    module_fun_ids.push(node.id());

                    // println!("{:#?}", node);
                    for param in fun.params.items.iter() {
                        match &param.pattern.kind {
                            BindingPatternKind::BindingIdentifier(s) => {
                                println!(
                                    "Fun: {:?} => bd: {:?}, {:?}",
                                    node.id(),
                                    s.name,
                                    s.symbol_id
                                );

                                let x = module_funs
                                    .entry(node.id())
                                    .or_insert_with(std::vec::Vec::new); // 确保获取到 Vec<SymbolId>
                                let sid = s.symbol_id.get().unwrap();
                                x.push(sid); // 现在可以安全地调用 push
                            }
                            _ => {}
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

    for fun_id in module_fun_ids {
        let f1 = nodes.get_node(fun_id).kind().as_function().unwrap();
        let mut newf = f1.clone_in(&allocator);

        // println!("{:#?}", newf);

        let ast = AstBuilder::new(allocator);
        let st = ast.statement_expression(f1.span, ast.expression_from_function(newf));

        let mut program = ast.program(
            Span::default(),
            program_source_type.unwrap().clone_in(&allocator),
            None,
            ast.vec(),
            ast.vec1(st),
        );

        let ret = WebPack4::new(allocator, "").build(&mut program);

        match &program.body[0] {
            Statement::ExpressionStatement(es) => {
                if let Expression::FunctionExpression(f) = &es.expression {
                    if let Some(body) = &f.body {
                        program.body = body.statements.clone_in(&allocator);
                    }
                }
            }
            _ => unreachable!(),
        }
        println!("ret: {:?}", ret);
        let module_str = CodeGenerator::new().build(&program).code;

        // println!("{:#?}", &program);

        println!("Program===>:\n {}", module_str);
    }

    if factory_id == NodeId::DUMMY {
        None
    } else {
        let module_factory = nodes.get_node(factory_id);

        for (module_id, e) in arr_expr.unwrap().elements.iter().enumerate() {
            // println!("Fun: {} ==: {:?}", module_id, e);

            if let Some(Expression::FunctionExpression(function_expr)) = e.as_expression() {
                let scope_id = &function_expr.scope_id;
                // dbg!(&scope_id);
                // dbg!(&function_expr.params);

                // for (it, new_name) in function_expr
                //     .params
                //     .items
                //     .iter()
                //     .zip(["module", "exports", "require"])
                // {
                //     let FormalParameter { pattern, .. } = it;
                //     let bd = pattern.get_binding_identifier().unwrap();
                //     let id = bd.symbol_id.get().unwrap();
                //     symbol_table.set_name(id, new_name.into());
                // }

                let mut function_expr1 = function_expr.clone_in(&allocator);

                // let st = Statement::ExpressionStatement(e.as_expression());

                let ast = AstBuilder::new(allocator);
                // ast.program(span, source_type, hashbang, directives, body)

                let k = ast.statement_expression(
                    function_expr1.span,
                    e.as_expression().unwrap().clone_in(&allocator),
                );

                let mut body = Vec::new_in(&allocator);

                body.push(k);

                let mut new_program = Program::new(
                    function_expr.span.clone_in(&allocator),
                    program_source_type.unwrap().clone_in(&allocator),
                    // fun.directives.clone_in(&allocator),
                    Vec::new_in(&allocator),
                    None,
                    body,
                );

                let semantic = SemanticBuilder::new("").build(&new_program).semantic;

                // require_helper(&allocator, &mut new_program);

                // println!("is fun {:?}", function_expr);
                if let Some(fun_body) = &function_expr.body {
                    // let directives = fun.directives;
                    // let statements = fun.statements;

                    let mut directives = fun_body.directives.clone_in(&allocator);
                    if let Some(d) = program_directives {
                        let mut p = d.clone_in(&allocator);
                        directives.append(&mut p);
                    }

                    let program = Program::new(
                        fun_body.span.clone_in(&allocator),
                        program_source_type.unwrap().clone_in(&allocator),
                        // fun.directives.clone_in(&allocator),
                        directives,
                        None,
                        fun_body.statements.clone_in(&allocator),
                    );

                    let printed = CodeGenerator::new().build(&program).code;
                    println!("program ===>: {:?}", printed);
                }
            }
        }
        None
    }
}

pub fn get_modules_form_webpack4_deprecated<'a>(
    allocator: &'a Allocator,
    program: &mut Program<'a>,
    source_text: &'a str,
) -> Option<Module> {
    WebPack4::new(allocator, source_text).build(program);
    None
}
#[derive(Debug)]
struct ModuleIds {
    ids: RefCell<std::vec::Vec<usize>>,
}

impl ModuleIds {
    pub fn new() -> Self {
        Self {
            ids: RefCell::new(vec![]),
        }
    }

    pub fn insert_id(&self, stmt: usize) {
        self.ids.borrow_mut().push(stmt);
    }
}

struct SymbolIds {
    ids: RefCell<std::vec::Vec<SymbolId>>,
}

impl SymbolIds {
    pub fn new() -> Self {
        Self {
            ids: RefCell::new(vec![]),
        }
    }

    pub fn insert_ids(&self, ids: std::vec::Vec<SymbolId>) {
        self.ids.borrow_mut().extend(ids);
    }

    pub fn get_symbol_index(&self, id: SymbolId) -> Option<usize> {
        self.ids.borrow().iter().position(|&x| x == id)
    }

    pub fn get_symbol_name(&self, id: SymbolId) -> Option<Atom<'static>> {
        self.get_symbol_index(id).map(|index| match index {
            0 => "module".into(),
            1 => "exports".into(),
            2 => "require".into(),
            _ => unreachable!(),
        })
    }
}

struct Webpack4Ctx<'a> {
    pub source_text: &'a str,
    pub module_ids: ModuleIds,
    pub symbol_ids: SymbolIds,
    pub is_esm: RefCell<bool>,
}

impl<'a> Webpack4Ctx<'a> {
    pub fn new(source_text: &'a str) -> Self {
        Self {
            source_text,
            module_ids: ModuleIds::new(),
            symbol_ids: SymbolIds::new(),
            is_esm: RefCell::new(false),
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

    pub fn build(mut self, program: &mut Program<'a>) -> Webpack4Return {
        let (symbols, scopes) = SemanticBuilder::new("")
            .build(program)
            .semantic
            .into_symbol_table_and_scope_tree();
        self.build_with_symbols_and_scopes(symbols, scopes, program)
    }

    pub fn get_parameter_symbols_id(&self, program: &Program<'a>) -> std::vec::Vec<SymbolId> {
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
                _ => {
                    unreachable!();
                }
            },
            _ => {
                unreachable!();
            }
        }
        symbol_ids
    }

    pub fn build_with_symbols_and_scopes(
        mut self,
        symbols: SymbolTable,
        scopes: ScopeTree,
        program: &mut Program<'a>,
    ) -> Webpack4Return {
        let symbol_ids = self.get_parameter_symbols_id(program);
        self.ctx.symbol_ids.insert_ids(symbol_ids);

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
    fn get_symbol_name(&self, id: SymbolId) -> Option<Atom<'static>> {
        self.ctx.symbol_ids.get_symbol_name(id)
    }

    fn get_reference_index(&self, id: ReferenceId, ctx: &TraverseCtx<'a>) -> Option<usize> {
        if let Some(refencer) = ctx.scoping.symbols().references.get(id) {
            if let Some(s) = refencer.symbol_id() {
                return self.ctx.symbol_ids.get_symbol_index(s);
            }
        }
        None
    }

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
        let Some(id) = idf.reference_id.get() else {
            return false;
        };
        let Some(index) = self.get_reference_index(id, ctx) else {
            return false;
        };
        name.as_str() == "r" && index == 2
    }
}

impl<'a, 'ctx> Traverse<'a> for Webpack4Impl<'a, 'ctx> {
    // fn enter_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
    //     println!("enter program");
    //     self.program_source_type = Some(program.source_type);
    //     let _program_directives = Some(&program.directives);
    // }

    // fn exit_program(&mut self, program: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
    //     println!("exit program");
    // }

    // fn enter_call_expression(
    //     &mut self,
    //     call_expr: &mut oxc_ast::ast::CallExpression<'a>,
    //     ctx: &mut TraverseCtx<'a>,
    // ) {
    //     if call_expr.arguments.len() == 1 {
    //         if let Some(Expression::ArrayExpression(args)) = call_expr.arguments[0].as_expression()
    //         {
    //             let all_is_fun = args
    //                 .elements
    //                 .iter()
    //                 .all(|arg| matches!(arg, ArrayExpressionElement::FunctionExpression(_)));
    //             if all_is_fun {
    //                 self.found_scope_id = Some(ctx.current_scope_id());
    //                 println!("{:?}", ctx.current_scope_id());
    //                 for arg in args.elements.iter() {
    //                     match arg {
    //                         ArrayExpressionElement::FunctionExpression(fe) => {
    //                             for param in fe.params.items.iter() {
    //                                 match &param.pattern.kind {
    //                                     BindingPatternKind::BindingIdentifier(s) => {
    //                                         println!("bd: {:?}", s);
    //                                     }
    //                                     _ => {}
    //                                 }
    //                             }
    //                         }
    //                         _ => {}
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    // fn enter_assignment_expression(
    //     &mut self,
    //     assign_expr: &mut AssignmentExpression<'a>,
    //     ctx: &mut TraverseCtx<'a>,
    // ) {
    //     match (self.found_scope_id, assign_expr) {
    //         (
    //             Some(factory_scope_id),
    //             AssignmentExpression {
    //                 left: AssignmentTarget::StaticMemberExpression(mem),
    //                 right: Expression::NumericLiteral(val),
    //                 ..
    //             },
    //         ) => {
    //             if mem.object.is_identifier_reference()
    //                 && mem.property.name.as_str() == "s"
    //                 && ctx.ancestor_scopes().any(|id| id == factory_scope_id)
    //             {
    //                 self.ctx.module_ids.insert_id(val.value as usize);
    //                 println!(
    //                     "{:?}, {}, {:?}, {:?}\n {:?}, {:?}",
    //                     mem,
    //                     val.value,
    //                     ctx.current_scope_id(),
    //                     ctx.scopes().get_bindings(ctx.current_scope_id()),
    //                     ctx.scopes().get_bindings(self.found_scope_id.unwrap()),
    //                     ctx.ancestor_scopes().collect::<std::vec::Vec<_>>()
    //                 );
    //             }
    //         }
    //         _ => {}
    //     }
    // }

    fn enter_identifier_reference(
        &mut self,
        idf: &mut oxc_ast::ast::IdentifierReference<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if let Some(id) = idf.reference_id.get() {
            if let Some(refencer) = ctx.scoping.symbols().references.get(id) {
                if let Some(s) = refencer.symbol_id() {
                    if let Some(new_name) = self.get_symbol_name(s) {
                        idf.name = new_name;
                    }
                }
            }
        }
    }

    fn enter_call_expression(
        &mut self,
        call_expr: &mut oxc_ast::ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        println!("call_expr: {:?}", call_expr);
        // let is_esm = match &call_expr.callee {
        //     Expression::StaticMemberExpression(mem) => match (&mem.object, &mem.property) {
        //         (
        //             Expression::Identifier(idf),
        //             IdentifierName {
        //                 name: property_name,
        //                 ..
        //             },
        //         ) => {
        //             if property_name.as_str() == "r" {
        //                 if let Some(id) = idf.reference_id.get() {
        //                     if let Some(index) = self.get_reference_index(id, ctx) {
        //                         index == 2
        //                     } else {
        //                         false
        //                     }
        //                 } else {
        //                     false
        //                 }
        //             } else {
        //                 false
        //             }
        //         }
        //         _ => false,
        //     },
        //     _ => false,
        // };

        // println!("is_ESM: {}", is_esm);

        // if is_esm {
        //     // TO-DO: replace with void_0
        // }
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
        if self.is_esm(&es.expression, ctx) {
            self.ctx.is_esm.replace(true);
            *node = ctx.ast.statement_empty(es.span);
        }
    }
}
