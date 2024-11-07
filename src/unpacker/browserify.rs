use indexmap::IndexMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, CallExpression, Expression, ObjectPropertyKind, Program,
        PropertyKey,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::SemanticBuilder;
use oxc_span::Span;

use crate::unpacker::{common::fun_to_program::FunctionToProgram, Module};

pub fn get_modules_form_browserify<'a>(
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

    let mut module_map = IndexMap::new();

    let mut entry_ids = vec![];

    for node in nodes.iter() {
        let AstKind::CallExpression(CallExpression {
            callee: _,
            arguments,
            ..
        }) = node.kind()
        else {
            continue;
        };
        let [Argument::ObjectExpression(modules_obj), Argument::ObjectExpression(_), Argument::ArrayExpression(entry_id_array)] =
            arguments.as_slice()
        else {
            continue;
        };

        if !entry_id_array
            .elements
            .iter()
            .all(|ele| matches!(ele, ArrayExpressionElement::NumericLiteral(_)))
        {
            continue;
        }

        for ele in entry_id_array.elements.iter() {
            let ArrayExpressionElement::NumericLiteral(num) = ele else {
                unreachable!();
            };
            entry_ids.push(num.raw);
        }

        for property in modules_obj.properties.iter() {
            let ObjectPropertyKind::ObjectProperty(prop) = property else {
                break;
            };
            let PropertyKey::NumericLiteral(module_id) = &prop.key else {
                break;
            };

            let Expression::ArrayExpression(arr) = &prop.value else {
                break;
            };

            let [module_factory, ArrayExpressionElement::ObjectExpression(map)] =
                arr.elements.as_slice()
            else {
                break;
            };

            let is_module_map = map.properties.iter().all(|p| {
                let ObjectPropertyKind::ObjectProperty(prop) = p else {
                    return false;
                };
                matches!(prop.key, PropertyKey::StringLiteral(_))
                    && matches!(prop.value, Expression::NumericLiteral(_))
            });
            if !is_module_map {
                break;
            }

            let Some(module_factory) = module_factory.as_expression() else {
                break;
            };

            module_map.insert(module_id.raw, module_factory);
        }
    }
    if module_map.is_empty() {
        return None;
    }

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

        let mut fun_renamer = FunctionToProgram::new(allocator, ["require", "module", "exports"]);
        fun_renamer.build(&mut program);

        modules.push(Module::new(
            module_id.to_string(),
            entry_ids.contains(&module_id),
            program,
        ));
    }
    Some(modules)
}
