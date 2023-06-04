use crate::token::Token;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Let { name: String, value: Expression },
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let { name, value } => Token::Let.literal(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
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
