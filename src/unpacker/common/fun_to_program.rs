use std::cell::RefCell;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{BindingPatternKind, IdentifierReference, Program, Statement};
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolId, SymbolTable};
use oxc_span::Atom;
use oxc_traverse::{Traverse, TraverseCtx};

use super::utils::{get_fun_body, get_fun_params};

pub struct FunctionToProgram<'a> {
    allocator: &'a Allocator,
    param_ids: RefCell<std::vec::Vec<SymbolId>>,
    rename_to: std::vec::Vec<Atom<'a>>,
}

impl<'a> FunctionToProgram<'a> {
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
        let Some(stmt) = program.body.first() else {
            return None;
        };

        let Some(params) = (match stmt {
            Statement::ExpressionStatement(es) => get_fun_params(&es.expression),
            _ => return None,
        }) else {
            return None;
        };

        for param in params.iter() {
            let BindingPatternKind::BindingIdentifier(binding) = &param.pattern.kind else {
                continue;
            };
            let Some(symbol_id) = binding.symbol_id.get() else {
                continue;
            };
            symbol_ids.push(symbol_id);
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

impl<'a> Traverse<'a> for FunctionToProgram<'a> {
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

    fn exit_program(&mut self, program: &mut Program<'a>, _ctx: &mut TraverseCtx<'a>) {
        let Statement::ExpressionStatement(exp) = &program.body[0] else {
            return;
        };
        let Some(body) = get_fun_body(&exp.expression) else {
            return;
        };
        program
            .directives
            .extend(body.directives.clone_in(self.allocator));
        program.body = body.statements.clone_in(self.allocator);
    }
}
