use std::cell::RefCell;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{BindingPatternKind, Expression, IdentifierReference, Program, Statement};
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolId, SymbolTable};
use oxc_span::Atom;
use oxc_traverse::{Traverse, TraverseCtx};

pub struct FunctionParamRenamer<'a> {
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
