use std::fmt::Display;

use crate::token::Token;

type Block = Vec<Statement>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return(Expression),
    Expression(Expression),
    Block(Block),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Return(exp) => write!(f, "return {};", exp),
            Statement::Expression(exp) => write!(f, "{}", exp),
            Statement::Block(b) => {
                let mut s = String::new();
                for stmt in b {
                    s.push_str(&format!("{}", stmt));
                }
                write!(f, "{}", s)
            }
        }
    }
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let {
                name: _name,
                value: _value,
            } => Token::Let.literal(),
            Statement::Return(_exp) => Token::Return.literal(),
            Statement::Expression(_) => todo!(),
            Statement::Block(_) => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    Prefix {
        operator: Token,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Block,
        alternative: Option<Block>,
    },
    FunctionLiteral {
        parameters: Vec<Token>,
        body: Block,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Integer(n) => write!(f, "{}", n),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Prefix { operator, right } => {
                write!(f, "({}{})", operator, right)
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let mut s = String::new();
                s.push_str(&format!("if {} ", condition));
                for stmt in consequence {
                    s.push_str(&format!("{}", stmt));
                }
                if let Some(alt) = alternative {
                    s.push_str("else ");
                    for stmt in alt {
                        s.push_str(&format!("{}", stmt));
                    }
                }
                write!(f, "{}", s)
            }
            Expression::FunctionLiteral { parameters, body } => {
                let mut s = String::new();
                s.push_str("fn(");
                for (i, param) in parameters.iter().enumerate() {
                    s.push_str(&format!("{}", param));
                    if i < parameters.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") ");
                for stmt in body {
                    s.push_str(&format!("{}", stmt));
                }
                write!(f, "{}", s)
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let mut s = String::new();
                s.push_str(&format!("{}(", function));
                for (i, arg) in arguments.iter().enumerate() {
                    s.push_str(&format!("{}", arg));
                    if i < arguments.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(')');
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}
