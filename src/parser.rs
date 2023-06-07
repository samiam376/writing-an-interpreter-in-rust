use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl Precedence {
    fn from_token(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'lexer> {
    lexer: Lexer<'lexer>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl<'lexer> Parser<'lexer> {
    pub fn new(lexer: Lexer<'lexer>) -> Parser<'lexer> {
        let mut parser = Parser {
            lexer,
            cur_token: None,
            peek_token: None,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn cur_precedence(&self) -> Option<Precedence> {
        self.cur_token
            .as_ref()
            .map(Precedence::from_token)
    }

    fn peek_precedence(&self) -> Option<Precedence> {
        self.peek_token
            .as_ref()
            .map(Precedence::from_token)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let name = match (self.cur_token.clone(), self.peek_token.clone()) {
            (Some(Token::Ident(name)), Some(Token::Assign)) => {
                self.next_token();
                self.next_token();
                name
            }
            _ => return Err("parse error: invalid tokens for let statement".to_string()),
        };

        let expression = match self.cur_token.clone() {
            Some(token) => match token {
                Token::Ident(n) => Expression::Identifier(n),
                Token::Int(n) => Expression::Integer(n.parse().unwrap()),
                Token::False => Expression::Boolean(false),
                Token::True => Expression::Boolean(true),
                _ => return Err("parse error: invalid token for expression".to_string()),
            },
            _ => return Err("parse error: no token for expression".to_string()),
        };

        while self.cur_token != Some(Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: expression,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let expression = match self.cur_token.clone() {
            Some(token) => match token {
                Token::Ident(n) => Expression::Identifier(n),
                Token::Int(n) => Expression::Integer(n.parse().unwrap()),
                Token::False => Expression::Boolean(false),
                Token::True => Expression::Boolean(true),
                _ => return Err("parse error: invalid token for expression".to_string()),
            },
            _ => return Err("parse error: no token for expression".to_string()),
        };

        while self.cur_token != Some(Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Return(expression))
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, String> {
        todo!()
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        match &self.cur_token {
            Some(token) => match token {
                Token::Ident(_) => self.parse_identifier(),
                Token::Bang | Token::Minus => {
                    let operator = token.clone();
                    self.next_token();

                    let right = self.parse_expression(Precedence::Prefix)?;

                    Ok(Expression::Prefix {
                        operator,
                        right: Box::new(right),
                    })
                }
                _ => Err(format!(
                    "parse error: no parse function for prefix {:?}",
                    token
                )),
            },
            _ => Err("parse prefix error: no token".to_string()),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        match &self.cur_token {
            Some(token) => match token {
                Token::Ident(n) => Ok(Expression::Identifier(n.into())),
                _ => Err(format!(
                    "parse identifier error: unexpected token {:?}",
                    token
                )),
            },
            _ => Err("parse error: no token for identifier".to_string()),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        println!("parse_statement: {:?}", self.cur_token);
        match &self.cur_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => Err(format!(
                "parse error: unsupported token {:?} for statement",
                self.cur_token
            )),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<String>> {
        let mut statements = Vec::new();
        while self.cur_token != Some(Token::EOF) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(msg) => self.errors.push(msg),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);

        let tests = vec!["x", "y", "foobar"];
        let values = vec![5, 10, 838383];

        for (i, test) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            assert_eq!(stmt.token_literal(), "let");
            match stmt {
                Statement::Let { name, value } => {
                    assert_eq!(name, *test);
                    let value = match &value {
                        Expression::Integer(n) => n,
                        _ => panic!("value is not integer"),
                    };

                    assert_eq!(*value, values[i]);
                }
                _ => panic!("stmt is not let statement"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parse errors");

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            assert_eq!(stmt.token_literal(), "return");
            match stmt {
                Statement::Return(_) => (),
                _ => panic!("stmt is not return statement"),
            }
        }
    }
}
