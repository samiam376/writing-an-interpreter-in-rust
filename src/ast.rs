use std::fmt::Display;

use crate::token::Token;

pub type Block = Vec<Statement>;

pub type Identifier = String;

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
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    String(String),
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
    If(IfExpression),
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: Block,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    ArrayLiteral(Vec<Expression>),
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
            Expression::If(exp) => {
                let mut s = String::new();
                s.push_str(&format!("if {} ", exp.condition));
                for stmt in &exp.consequence {
                    s.push_str(&format!("{}", stmt));
                }
                if let Some(alt) = &exp.alternative {
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
                    s.push_str(&param.to_string());
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
            Expression::String(s) => write!(f, "{}", s),
            Expression::ArrayLiteral(a) => {
                let mut s = String::new();
                s.push('[');
                let elements = a
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(",");

                s.push_str(&elements);
                s.push(']');
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for stmt in &self.statements {
            s.push_str(&format!("{}", stmt));
        }
        write!(f, "{}", s)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl From<Expression> for Node {
    fn from(exp: Expression) -> Self {
        Node::Expression(exp)
    }
}

impl From<Statement> for Node {
    fn from(stmt: Statement) -> Self {
        Node::Statement(stmt)
    }
}

impl From<Program> for Node {
    fn from(prog: Program) -> Self {
        Node::Program(prog)
    }
}

impl From<Block> for Node {
    fn from(block: Block) -> Self {
        Node::Statement(Statement::Block(block))
    }
}
