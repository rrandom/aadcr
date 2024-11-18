use oxc_ast::ast::{Expression, Statement, UnaryOperator};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};
use super::UnminifyPass;

pub struct UnReturn {
    changed: bool,
}

impl UnReturn {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnReturn {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnReturn {
    fn exit_statements(
        &mut self,
        stmts: &mut oxc_allocator::Vec<'a, oxc_ast::ast::Statement<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if let Some(last_stmt) = stmts.last_mut()
            && let Statement::ReturnStatement(ret) = last_stmt
        {
            if !ctx.parent().is_function_body() {
                return;
            }
            match &mut ret.argument {
                Some(argument) => match argument {
                    expr if expr.is_undefined() || expr.is_void_0() => {
                        stmts.pop();
                    }
                    Expression::UnaryExpression(unary) => {
                        if unary.operator == UnaryOperator::Void {
                            *last_stmt = ctx.ast.statement_expression(
                                SPAN,
                                ctx.ast.move_expression(&mut unary.argument),
                            );
                            self.changed = true;
                        }
                    }
                    _ => {}
                },
                None => {
                    stmts.pop();
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnReturn::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_return_void_expr_to_expr() {
        run_test(
            "
function foo() {
  return void a()
}",
            "
function foo() {
  a();
}
        ",
        );
    }

    #[test]
    fn test_un_return_void_remove_redundant_return() {
        run_test(
            "
function foo() {
  const a = 1
  return undefined
}

const bar = () => {
  const a = 1
  if (a) return void 0
  return void 0
}

const baz = function () {
  const a = 1
  if (a) {
    return undefined
  }
  return undefined
}

const obj = {
  method() {
    const a = 1
    return void 0
  }
}

class A {
  method() {
    const a = 1
    return
  }
}
        ",
            "
function foo() {
  const a = 1
}

const bar = () => {
  const a = 1
  if (a) return void 0
}

const baz = function () {
  const a = 1
  if (a) {
    return undefined
  }
}

const obj = {
  method() {
    const a = 1
  }
}

class A {
  method() {
    const a = 1
  }
}
        ",
        );
    }

    #[test]
    fn test_un_return_void_double_return() {
        run_test(
            "
function foo() {
  return void 0
  return undefined
}
        ",
            "
function foo() {
  return void 0
}
  ",
        );
    }

    #[test]
    fn test_un_return_no_transform() {
        run_test(
            "
function foo() {
  const count = 5;
  while (count--) {
    return void 0;
  }

  for (let i = 0; i < 10; i++) {
    return void foo();
  }
}
        ",
            "
function foo() {
  const count = 5;
  while (count--) {
    return void 0;
  }

  for (let i = 0; i < 10; i++) {
    return void foo();
  }
}
        ",
        );
    }
}
