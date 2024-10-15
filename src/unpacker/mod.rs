use oxc_allocator::Allocator;
use oxc_allocator::{Box, CloneIn};
use oxc_ast::{
    ast::{
        ArrayExpressionElement, AssignmentExpression, Directive, Expression, FunctionBody,
        MemberExpression, NumericLiteral, Program,
    },
    AstKind,
};
use oxc_codegen::CodeGenerator;
use oxc_semantic::{NodeId, Semantic};
pub struct Module {}

pub fn get_modules_form_webpack4(allocator: &Allocator, semantic: &Semantic) -> Option<Module> {
    let mut factory_id = NodeId::DUMMY;
    let mut entry_ids = Vec::new();
    let mut arr_expr = None;
    let mut program_source_type = None;
    let mut program_directives = None;

    for node in semantic.nodes().iter() {
        match node.kind() {
            AstKind::Program(Program {
                source_type,
                directives,
                ..
            }) => {
                program_source_type = Some(source_type);
                program_directives = Some(directives);
            }
            AstKind::CallExpression(call) => {
                // println!("found Call {:?}", node.id());
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
            AstKind::AssignmentExpression(AssignmentExpression {
                left,
                right: Expression::NumericLiteral(s),
                span,
                ..
            }) => {
                let contains = semantic
                    .nodes()
                    .ancestors(node.id())
                    .any(|id| id == factory_id);
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
                            span.source_text(semantic.source_text()),
                            s.value
                        );
                        entry_ids.push(s.value);
                    }
                }
            }
            _ => {}
        }
    }

    if factory_id == NodeId::DUMMY {
        None
    } else {
        let module_factory = semantic.nodes().get_node(factory_id);

        for (module_id, e) in arr_expr.unwrap().elements.iter().enumerate() {
            // println!("Fun: {} ==: {:?}", module_id, e);

            if let Some(Expression::FunctionExpression(function_expr)) = e.as_expression() {
                // println!("is fun {:?}", function_expr);
                if let Some(fun) = &function_expr.body {
                    // let directives = fun.directives;
                    // let statements = fun.statements;

                    let program = Program::new(
                        fun.span.clone_in(&allocator),
                        program_source_type.unwrap().clone_in(&allocator),
                        fun.directives.clone_in(&allocator),
                        None,
                        fun.statements.clone_in(&allocator),
                    );

                    let printed = CodeGenerator::new().build(&program).code;
                    println!("program ===>: {:?}", printed);
                }
            }
        }
        None
    }
}
