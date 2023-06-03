#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
}
