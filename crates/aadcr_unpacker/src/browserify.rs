use indexmap::{map::Entry, IndexMap};

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, CallExpression, Expression, ObjectPropertyKind, Program,
        PropertyKey,
    },
    AstBuilder, AstKind,
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_semantic::SemanticBuilder;
use oxc_span::SPAN;

use crate::{common::fun_to_program::FunctionToProgram, Module};

use super::{UnpackResult, UnpackReturn};

pub fn get_modules_form_browserify<'a>(
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

    let mut module_map = IndexMap::new();

    let mut entry_ids = vec![];

    let mut module_mapping = IndexMap::new();

    let mut errors = vec![];

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

            for property in map.properties.iter() {
                let ObjectPropertyKind::ObjectProperty(prop) = property else {
                    unreachable!()
                };

                let PropertyKey::StringLiteral(short_name) = &prop.key else {
                    unreachable!()
                };

                let Expression::NumericLiteral(module_id) = &prop.value else {
                    unreachable!()
                };
                let short_name = short_name.value.as_str();

                let entry = module_mapping.entry(module_id.raw);
                match entry {
                    Entry::Occupied(mut e) if *e.get() != short_name => {
                        errors.push(OxcDiagnostic::warn(format!(
                            "Module {} has multiple short names: {} and {}",
                            module_id.raw,
                            e.get(),
                            short_name
                        )));
                        e.insert(short_name);
                    }
                    Entry::Vacant(e) => {
                        e.insert(short_name);
                    }
                    _ => {}
                }
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
        let fun_statement = ast.statement_expression(SPAN, expr.clone_in(allocator));

        let directives = program_directives
            .clone_in(allocator)
            .unwrap_or_else(|| ast.vec());

        let mut program = ast.program(
            SPAN,
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
    Some(UnpackReturn {
        modules,
        module_mapping: Some(module_mapping),
        errors,
    })
}
