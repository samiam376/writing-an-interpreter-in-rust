use crate::{
    ast::{Expression, Node, Statement},
    object::Object,
};

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Program(p) => {
            let mut result = Object::Null;
            for statement in p.statements {
                result = eval(Node::Statement(statement))?;
            }
            Ok(result)
        }
        Node::Statement(s) => match s {
            Statement::Expression(e) => Ok(eval(Node::Expression(e))?),
            _ => Err(format!("unimplemented: {:?}", s)),
        },
        Node::Expression(exp) => match exp {
            Expression::Integer(i) => Ok(Object::Integer(i)),
            _ => Err(format!("unimplemented: {:?}", exp)),
        },
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn test_eval() {
        let input = "5";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();

        let evaluated = eval(Node::Program(program)).unwrap_or_else(|e| {
            panic!("eval error: {}", e);
        });

        assert_eq!(evaluated, Object::Integer(5));
    }
}
