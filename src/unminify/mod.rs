mod passes;

use oxc_allocator::Allocator;
use oxc_ast::{
    ast::{Expression, Program},
    Trivias,
};
use oxc_semantic::{ScopeTree, SymbolTable};
use oxc_span::SourceType;
use oxc_traverse::{traverse_mut, Traverse, TraverseCtx};

pub struct UnminifyCtx<'a> {
    pub source_text: &'a str,
    pub source_type: SourceType,
    pub trivial: Trivias,
}

impl<'a> UnminifyCtx<'a> {
    pub fn new(source_text: &'a str, source_type: SourceType) -> Self {
        Self {
            source_text,
            source_type,
            trivial: Trivias::default(),
        }
    }
}

struct UnminifyOptions {}
pub struct Unminify<'a> {
    ctx: UnminifyCtx<'a>,
    options: UnminifyOptions,
    allocator: &'a Allocator,
}

impl<'a> Unminify<'a> {
    pub fn new(source_text: &'a str, allocator: &'a Allocator) -> Self {
        Self {
            ctx: UnminifyCtx::new(source_text, SourceType::default()),
            options: UnminifyOptions {},
            allocator,
        }
    }

    pub fn build_with_symbols_and_scopes(
        mut self,
        symbols: SymbolTable,
        scopes: ScopeTree,
        program: &mut Program<'a>,
    ) {
        let allocator = self.allocator;

        self.ctx.source_type = program.source_type;

        let mut unminify = UnminifyImpl {
            un_boolean: passes::un_boolean::UnBoolean::new(&self.ctx),
        };

        let (symbols, scopes) = traverse_mut(&mut unminify, allocator, program, symbols, scopes);

        traverse_mut(&mut unminify, allocator, program, symbols, scopes);
    }
}

struct UnminifyImpl<'a, 'ctx> {
    un_boolean: passes::un_boolean::UnBoolean<'a, 'ctx>,
}

impl<'a> Traverse<'a> for UnminifyImpl<'a, '_> {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        unimplemented!()
    }
}
