use oxc_allocator::{Allocator, Vec};
use oxc_allocator::{Box, CloneIn};
use oxc_ast::{
    ast::{
        ArrayExpressionElement, AssignmentExpression, Directive, Expression, FormalParameter,
        FunctionBody, MemberExpression, NumericLiteral, Program,
    },
    AstKind,
};
use oxc_codegen::CodeGenerator;
use oxc_semantic::{NodeId, Semantic, SemanticBuilder, SymbolId, SymbolTable};

pub struct Module {}

pub fn get_modules_form_webpack4(allocator: &Allocator, program: &Program) -> Option<Module> {
    let semantic = SemanticBuilder::new("").build(program).semantic;

    let mut factory_id = NodeId::DUMMY;
    let mut entry_ids = Vec::new_in(&allocator);
    let mut arr_expr = None;
    let mut program_source_type = None;
    let mut program_directives = None;

    // println!("===");

    let (mut symbol_table, _) = SemanticBuilder::new("")
        .build(program)
        .semantic
        .into_symbol_table_and_scope_tree();

    for node in semantic.nodes().iter() {
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
                            // span.source_text(semantic.source_text()),
                            1,
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
                let scope_id = &function_expr.scope_id;
                // dbg!(&scope_id);
                // dbg!(&function_expr.params);

                for (it, new_name) in function_expr
                    .params
                    .items
                    .iter()
                    .zip(["module", "exports", "require"])
                {
                    let FormalParameter { pattern, .. } = it;
                    let bd = pattern.get_binding_identifier().unwrap();
                    let id = bd.symbol_id.get().unwrap();
                    symbol_table.set_name(id, new_name.into());
                }

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
