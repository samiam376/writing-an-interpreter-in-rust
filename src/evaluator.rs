use crate::{
    ast::{Block, Expression, IfExpression, Node, Program, Statement},
    object::{Apply, Environment, Function, Object},
    token::Token,
};

pub type EvalReturn = Result<Option<Object>, String>;

fn eval_bang(object: Object) -> EvalReturn {
    match object {
        Object::Boolean(bool) => Ok(Some(Object::Boolean(!bool))),
        Object::Null => Ok(Some(Object::Boolean(true))),
        _ => Ok(Some(Object::Boolean(false))),
    }
}

fn eval_minus_prefix(right: Object) -> EvalReturn {
    match right {
        Object::Integer(i) => Ok(Some(Object::Integer(-i))),
        _ => Err(format!("unknown operator: -{}", right)),
    }
}

fn eval_prefix(operator: Token, right: Object) -> EvalReturn {
    match operator {
        Token::Bang => eval_bang(right),
        Token::Minus => eval_minus_prefix(right),
        _ => Ok(Some(Object::Null)),
    }
}

fn eval_infix_integer(operator: Token, right: i64, left: i64) -> EvalReturn {
    match operator {
        Token::Plus => Ok(Some(Object::Integer(left + right))),
        Token::Minus => Ok(Some(Object::Integer(left - right))),
        Token::Asterisk => Ok(Some(Object::Integer(left * right))),
        Token::Slash => Ok(Some(Object::Integer(left / right))),
        Token::Lt => Ok(Some(Object::Boolean(left < right))),
        Token::Gt => Ok(Some(Object::Boolean(left > right))),
        Token::Eq => Ok(Some(Object::Boolean(left == right))),
        Token::NotEq => Ok(Some(Object::Boolean(left != right))),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_infix_boolean(operator: Token, right: bool, left: bool) -> EvalReturn {
    match operator {
        Token::Eq => Ok(Some(Object::Boolean(left == right))),
        Token::NotEq => Ok(Some(Object::Boolean(left != right))),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_infix_string(operator: Token, right: String, left: String) -> EvalReturn {
    match operator {
        Token::Plus => Ok(Some(Object::String(left + &right))),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_infix(operator: Token, right: Object, left: Object) -> EvalReturn {
    match (&left, &right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_infix_integer(operator, *right, *left)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_infix_boolean(operator, *right, *left)
        }
        (Object::String(left), Object::String(right)) => {
            eval_infix_string(operator, right.clone(), left.clone())
        }
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_if_expression(ie: IfExpression, env: &mut Environment) -> EvalReturn {
    let condition = eval((*ie.condition).into(), env)?;

    if condition.is_some() && condition.unwrap().is_truthy() {
        eval(ie.consequence.into(), env)
    } else if let Some(alt) = ie.alternative {
        eval(alt.into(), env)
    } else {
        Ok(Some(Object::Null))
    }
}
fn eval_block(block: Block, env: &mut Environment) -> EvalReturn {
    let mut result = Some(Object::Null);

    for statement in block {
        result = eval(statement.into(), env)?;

        if Some(true) == result.as_ref().map(|r| r.is_return_value()) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_program(program: Program, env: &mut Environment) -> EvalReturn {
    let mut result = None;
    for statement in program.statements {
        result = eval(statement.into(), env)?;
        if let Some(Object::ReturnValue(value)) = result {
            return Ok(Some(*value));
        }
    }
    Ok(result)
}

fn eval_statement(statement: Statement, env: &mut Environment) -> EvalReturn {
    match statement {
        Statement::Expression(expression) => eval(expression.into(), env),
        Statement::Block(block) => eval_block(block, env),
        Statement::Return(expression) => {
            let val = eval(expression.into(), env)?.expect("return value");

            Ok(Some(Object::ReturnValue(Box::new(val))))
        }
        Statement::Let { name, value } => {
            let val = eval(value.into(), env)?
                .expect("let statement value should be evaluated to an object");
            env.set(&name, val);

            Ok(None)
        }
    }
}

fn apply_function(fun: Object, args: Vec<Object>) -> EvalReturn {
    let fun = match fun {
        Object::Function(fun) => fun,
        Object::Builtin(fun) => return fun.apply(args),
        _ => panic!("not a function: {:?}", fun),
    };

    let mut env = fun.env.to_enclosed();
    for (param, arg) in fun.parameters.iter().zip(args) {
        env.set(param, arg);
    }

    let evaluated = eval(fun.body.into(), &mut env)?;

    match evaluated {
        Some(Object::ReturnValue(value)) => Ok(Some(*value)),
        Some(value) => Ok(Some(value)),
        None => Ok(None),
    }
}

fn eval_expression(expression: Expression, env: &mut Environment) -> EvalReturn {
    match expression {
        Expression::Boolean(bool) => Ok(Some(bool.into())),
        Expression::Integer(i) => Ok(Some(i.into())),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(*right, env)?
                .expect("prefix expression should be evaluated to an object");

            eval_prefix(operator, right)
        }
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left, env)?
                .expect("left side of infix expression should be evaluated to an object");
            let right = eval_expression(*right, env)?
                .expect("right side of infix expression should be evaluated to an object");

            eval_infix(operator, right, left)
        }
        Expression::If(ie) => eval_if_expression(ie, env),
        Expression::Identifier(ident) => {
            let val = env.get(&ident);

            if val.is_some() {
                return Ok(val);
            };

            let builtin = Object::lookup_builtin(&ident);
            if builtin.is_some() {
                return Ok(builtin);
            }

            Err(format!("identifier not found: {}", ident))
        }
        Expression::FunctionLiteral { parameters, body } => Ok(Some(Object::Function(
            Function::new(parameters, body, env.clone()),
        ))),
        Expression::Call {
            function,
            arguments,
        } => {
            let function = eval_expression(*function, env)?.expect(
                "function should be evaluated to an object, and that object should be a function",
            );

            let args = arguments
                .into_iter()
                .map(|arg| {
                    eval_expression(arg, env).expect(
                        "
                arguments should be evaluated to objects",
                    )
                })
                .map(|arg| arg.expect("arguments should be evaluated to objects"))
                .collect::<Vec<Object>>();

            apply_function(function, args)
        }
        Expression::String(s) => Ok(Some(s.into())),
        Expression::ArrayLiteral(elements) => {
            let elements = elements
                .into_iter()
                .map(|e| eval_expression(e, env))
                .collect::<Result<Vec<_>, _>>()?;

            let flattened = elements.into_iter().flatten().collect();

            Ok(Some(Object::Array(flattened)))
        }
        Expression::Index { left, index } => {
            let left = eval_expression(*left, env)?
                .expect("left side of index expression should be evaluated to an object");

            let index = eval_expression(*index, env)?
                .expect("index expression should be evaluated to an object");

            match (left, index) {
                (Object::Array(arr), Object::Integer(i)) => {
                    let i = i as usize;
                    if i >= arr.len() {
                        return Err(format!(
                            "index out of bounds: the len is {} but the index is {}",
                            arr.len(),
                            i
                        ));
                    }

                    Ok(Some(arr[i].clone()))
                }
                _ => Err("index operator not supported".into()),
            }
        }
        Expression::HashLiteral(_) => todo!(),
    }
}

pub fn eval(node: Node, env: &mut Environment) -> EvalReturn {
    match node {
        Node::Program(p) => eval_program(p, env),
        Node::Statement(s) => eval_statement(s, env),
        Node::Expression(exp) => eval_expression(exp, env),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    fn run_eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut env = Environment::new();

        eval(program.into(), &mut env).unwrap_or_else(|e| {
            panic!("eval error: {}", e);
        })
    }

    #[test]
    fn test_eval_int() {
        assert_eq!(run_eval("5"), Some(Object::Integer(5)));
        assert_eq!(run_eval("10"), Some(Object::Integer(10)));
        assert_eq!(run_eval("-5"), Some(Object::Integer(-5)));
        assert_eq!(run_eval("-10"), Some(Object::Integer(-10)));
        assert_eq!(run_eval("5 + 5 + 5 + 5 - 10"), Some(Object::Integer(10)));
        assert_eq!(run_eval("2 * 2 * 2 * 2 * 2"), Some(Object::Integer(32)));
        assert_eq!(run_eval("-50 + 100 + -50"), Some(Object::Integer(0)));
        assert_eq!(run_eval("5 * 2 + 10"), Some(Object::Integer(20)));
        assert_eq!(run_eval("5 + 2 * 10"), Some(Object::Integer(25)));
        assert_eq!(run_eval("20 + 2 * -10"), Some(Object::Integer(0)));
        assert_eq!(run_eval("50 / 2 * 2 + 10"), Some(Object::Integer(60)));
        assert_eq!(run_eval("2 * (5 + 10)"), Some(Object::Integer(30)));
        assert_eq!(run_eval("3 * 3 * 3 + 10"), Some(Object::Integer(37)));
        assert_eq!(run_eval("3 * (3 * 3) + 10"), Some(Object::Integer(37)));
        assert_eq!(
            run_eval("(5 + 10 * 2 + 15 / 3) * 2 + -10"),
            Some(Object::Integer(50))
        );
    }

    #[test]
    fn test_eval_bool() {
        assert_eq!(run_eval("true"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("false"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 < 2"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("1 > 2"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 < 1"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 > 1"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 == 1"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("1 != 1"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 == 2"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("1 != 2"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("true == true"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("false == false"), Some(Object::Boolean(true)));
        assert_eq!(run_eval("true == false"), Some(Object::Boolean(false)));
        assert_eq!(run_eval("true != false"), Some(Object::Boolean(true)));
    }

    #[test]
    fn test_eval_bang() {
        let case_one = "!true";

        let case_one_result = run_eval(case_one).unwrap();

        assert_eq!(case_one_result, Object::Boolean(false));

        let case_two = "!false";

        let case_two_result = run_eval(case_two).unwrap();

        assert_eq!(case_two_result, Object::Boolean(true));

        let case_three = "!5";

        let case_three_result = run_eval(case_three).unwrap();

        assert_eq!(case_three_result, Object::Boolean(false));

        let case_four = "!!true";

        let case_four_result = run_eval(case_four).unwrap();

        assert_eq!(case_four_result, Object::Boolean(true));

        let case_five = "!!false";

        let case_five_result = run_eval(case_five).unwrap();

        assert_eq!(case_five_result, Object::Boolean(false));
    }

    #[test]
    fn test_eval_if() {
        assert_eq!(run_eval("if (true) { 10 }").unwrap(), Object::Integer(10));
        assert_eq!(run_eval("if (false) { 10 }").unwrap(), Object::Null);
        assert_eq!(run_eval("if (1) { 10 }").unwrap(), Object::Integer(10));
        assert_eq!(run_eval("if (1 < 2) { 10 }").unwrap(), Object::Integer(10));
        assert_eq!(run_eval("if (1 > 2) { 10 }").unwrap(), Object::Null);
        assert_eq!(
            run_eval("if (1 > 2) { 10 } else { 20 }").unwrap(),
            Object::Integer(20)
        );
        assert_eq!(
            run_eval("if (1 < 2) { 10 } else { 20 }").unwrap(),
            Object::Integer(10)
        )
    }

    #[test]
    fn test_return_statements() {
        assert_eq!(run_eval("return 10;").unwrap(), Object::Integer(10));
        assert_eq!(run_eval("return 10; 9;").unwrap(), Object::Integer(10));
        assert_eq!(
            run_eval(
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1; 
                }",
            )
            .unwrap(),
            Object::Integer(10)
        );
    }

    #[test]
    fn test_function() {
        assert_eq!(
            run_eval("let identity = fn (x) {x}; identity(5);"),
            Some(Object::Integer(5))
        );
        assert_eq!(
            run_eval("let identity = fn (x) {return x;}; identity(5);"),
            Some(Object::Integer(5))
        );
        assert_eq!(
            run_eval("let double = fn (x) {x * 2;}; double(5);"),
            Some(Object::Integer(10))
        );

        assert_eq!(
            run_eval("let add = fn (x, y) {x + y;}; add(5, 5);"),
            Some(Object::Integer(10))
        );

        assert_eq!(
            run_eval("let add = fn (x, y) {x + y;}; add(5 + 5, add(5, 5));"),
            Some(Object::Integer(20))
        );

        assert_eq!(run_eval("fn (x) {x;}(5);"), Some(Object::Integer(5)));
    }

    #[test]
    fn test_closure() {
        assert_eq!(
            run_eval(
                "let newAdder = fn (x) {fn (y) {x + y};}; let addTwo = newAdder(2); addTwo(2);"
            ),
            Some(Object::Integer(4))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(
            run_eval("\"Hello World!\""),
            Some(Object::String("Hello World!".to_string()))
        );
        assert_eq!(
            run_eval("\"Hello\" + \" \" + \"World!\""),
            Some(Object::String("Hello World!".to_string()))
        );
    }

    #[test]
    fn test_builtin() {
        assert_eq!(
            run_eval("len(\"\")"),
            Some(Object::Integer("".len() as i64))
        );
        assert_eq!(
            run_eval("len(\"four\")"),
            Some(Object::Integer("four".len() as i64))
        );
    }

    #[test]
    fn test_array_literal() {
        assert_eq!(
            run_eval("[1, 2 * 2, 3 + 3]"),
            Some(Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6)
            ]))
        );
    }
}
