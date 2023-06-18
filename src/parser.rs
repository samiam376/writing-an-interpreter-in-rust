use crate::ast::{Expression, Identifier, IfExpression, Program, Statement};
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
    Index,       // array[index]
}

impl Precedence {
    fn from_token(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
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
        self.cur_token.as_ref().map(Precedence::from_token)
    }

    fn peek_precedence(&self) -> Option<Precedence> {
        self.peek_token.as_ref().map(Precedence::from_token)
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

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: expression,
        })
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        match self.cur_token.clone() {
            Some(Token::Int(n)) => Ok(Expression::Integer(n.parse().unwrap())),
            _ => Err("parse error: invalid token for integer literal".to_string()),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expression, String> {
        match self.cur_token.clone() {
            Some(Token::String(s)) => Ok(Expression::String(s)),
            _ => Err("parse error: invalid token for string literal".to_string()),
        }
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression, String> {
        match self.cur_token.clone() {
            Some(Token::True) => Ok(Expression::Boolean(true)),
            Some(Token::False) => Ok(Expression::Boolean(false)),
            _ => Err("parse error: invalid token for boolean literal".to_string()),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Return(expression))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, String> {
        let mut pairs = Vec::new();

        while self.peek_token != Some(Token::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if self.peek_token != Some(Token::Colon) {
                return Err("parse error: invalid token for hash literal".to_string());
            }

            self.next_token();
            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if self.peek_token != Some(Token::RBrace) && self.peek_token != Some(Token::Comma) {
                return Err("parse error: invalid token for hash literal".to_string());
            };

            if self.peek_token == Some(Token::Comma) {
                self.next_token();
            }
        }

        if self.peek_token != Some(Token::RBrace) {
            return Err("parse error: invalid token for hash literal".to_string());
        }

        self.next_token();

        println!("cur_token 8: {:?}", self.cur_token);
        println!("peek_token 8: {:?}", self.peek_token);

        Ok(Expression::HashLiteral(pairs))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let prefix = self.parse_prefix()?;

        let mut left = prefix;
        while self.peek_token != Some(Token::SemiColon)
            && self.peek_precedence().is_some()
            && precedence < self.peek_precedence().unwrap()
        {
            match &self.peek_token {
                Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Slash)
                | Some(Token::Asterisk)
                | Some(Token::Eq)
                | Some(Token::NotEq)
                | Some(Token::Lt)
                | Some(Token::Gt) => {
                    self.next_token();
                    left = self.parse_infix(left.clone())?;
                }
                Some(Token::LParen) => {
                    self.next_token();
                    left = self.parse_call_expression(left.clone())?;
                }
                Some(Token::LBracket) => {
                    self.next_token();
                    left = self.parse_index_expression(left.clone())?;
                }
                _ => return Ok(left),
            }
        }
        Ok(left)
    }

    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        let elements = self.parse_expression_list(Token::RBracket)?;
        Ok(Expression::ArrayLiteral(elements))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, String> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token != Some(Token::RBracket) {
            return Err(format!(
                "parse error: expected {:?}, got {:?}",
                Token::RBracket,
                self.peek_token,
            ));
        };
        self.next_token();

        Ok(Expression::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, String> {
        let mut list = Vec::new();

        if self.peek_token == Some(end.clone()) {
            self.next_token();
            return Ok(list);
        };

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();

            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.peek_token != Some(end.clone()) {
            return Err(format!(
                "parse error: expected {:?}, got {:?}",
                end, self.peek_token,
            ));
        };

        self.next_token();

        Ok(list)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, String> {
        let mut identifiers = Vec::new();

        if self.peek_token == Some(Token::RParen) {
            self.next_token();
            return Ok(identifiers);
        };

        self.next_token();

        let identifier = self
            .cur_token
            .clone()
            .map(|token| token.literal())
            .ok_or(format!(
                "parse error: expected identifier, got {:?}",
                self.cur_token,
            ))?;

        identifiers.push(identifier);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();

            let identifier = self
                .cur_token
                .clone()
                .map(|token| token.literal())
                .ok_or(format!(
                    "parse error: expected identifier, got {:?}",
                    self.cur_token,
                ))?;

            identifiers.push(identifier);
        }

        if self.peek_token != Some(Token::RParen) {
            return Err(format!(
                "parse error: expected ), got {:?}",
                self.peek_token
            ));
        }

        self.next_token();

        Ok(identifiers)
    }

    fn parse_function_literal(&mut self) -> Result<Expression, String> {
        if self.peek_token != Some(Token::LParen) {
            return Err(format!(
                "parse error: expected (, got {:?}",
                self.peek_token
            ));
        };

        self.next_token();

        let parameters = self.parse_function_parameters()?;

        if self.peek_token != Some(Token::LBrace) {
            return Err(format!(
                "parse error: expected {{, got {:?}",
                self.peek_token
            ));
        };

        self.next_token();

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral { parameters, body })
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
                Token::Int(_) => self.parse_integer_literal(),
                Token::String(_) => self.parse_string_literal(),
                Token::True | Token::False => self.parse_boolean_literal(),
                Token::LParen => {
                    self.next_token();
                    let expression = self.parse_expression(Precedence::Lowest)?;
                    if self.peek_token != Some(Token::RParen) {
                        return Err("parse error: no closing parenthesis".to_string());
                    }
                    self.next_token();
                    Ok(expression)
                }
                Token::If => self.parse_if_expression(),
                Token::Function => self.parse_function_literal(),
                Token::LBracket => self.parse_array_literal(),
                Token::LBrace => self.parse_hash_literal(),
                _ => Err(format!(
                    "parse error: no parse function for prefix {:?}",
                    token
                )),
            },
            _ => Err("parse prefix error: no token".to_string()),
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, String> {
        let precedence = self
            .cur_precedence()
            .ok_or("parse infix error: no precedence".to_string())?;

        let cur_token = self
            .cur_token
            .clone()
            .ok_or("parse infix error: no current token".to_string())?;

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix {
            left: Box::new(left),
            operator: cur_token,
            right: Box::new(right),
        })
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

    fn parse_block_statement(&mut self) -> Result<Vec<Statement>, String> {
        self.next_token();
        let mut statements = Vec::new();

        while self.cur_token != Some(Token::RBrace) && self.cur_token != Some(Token::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        Ok(statements)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        if self.peek_token != Some(Token::LParen) {
            return Err("parse error: no opening parenthesis for if expression".to_string());
        };

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if self.cur_token != Some(Token::RParen) {
            return Err(format!(
                "parse error: no closing parenthesis for if expression, got {:?}",
                self.peek_token
            ));
        };

        self.next_token();

        if self.cur_token != Some(Token::LBrace) {
            return Err(format!(
                "parse error: no opening brace for if expression, got {:?}",
                self.cur_token
            ));
        };

        let consequence = self.parse_block_statement()?;

        let mut alternative = None;
        if self.peek_token == Some(Token::Else) {
            self.next_token();

            if self.peek_token != Some(Token::LBrace) {
                return Err(format!(
                    "parse error: no opening brace for else expression, got {:?}",
                    self.peek_token
                ));
            };

            self.next_token();

            alternative = Some(self.parse_block_statement()?);
        }

        let if_exp = IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Ok(Expression::If(if_exp))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, String> {
        let args = self.parse_call_arguments()?;

        Ok(Expression::Call {
            function: Box::new(function),
            arguments: args,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();

        if self.peek_token == Some(Token::RParen) {
            self.next_token();
            return Ok(args);
        };

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();

            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.peek_token != Some(Token::RParen) {
            return Err(format!(
                "parse error: no closing parenthesis for call expression, got {:?}",
                self.peek_token
            ));
        };

        self.next_token();

        Ok(args)
    }
    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match &self.cur_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(_) => self.parse_expression_statement(),
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

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

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

        let parsed = parser.parse_program();
        if parsed.is_err() {
            panic!("parse error: {:?}", parsed.err());
        }

        let program = parsed.unwrap();
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

    #[test]
    fn test_prefix_expression() {
        let input = "
        !5;
        -15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parse errors");

        assert_eq!(program.statements.len(), 2);
        let stmt_one = &program.statements[0];

        match stmt_one {
            Statement::Expression(expr) => match expr {
                Expression::Prefix { operator, right } => {
                    assert_eq!(*operator, Token::Bang);
                    match **right {
                        Expression::Integer(n) => assert_eq!(n, 5),
                        _ => panic!("right is not integer"),
                    }
                }
                _ => panic!("stmt is not prefix expression"),
            },
            _ => panic!("stmt is not expression statement"),
        }

        let stmt_two = &program.statements[1];

        match stmt_two {
            Statement::Expression(expr) => match expr {
                Expression::Prefix { operator, right } => {
                    assert_eq!(*operator, Token::Minus);
                    match **right {
                        Expression::Integer(n) => assert_eq!(n, 15),
                        _ => panic!("right is not integer"),
                    }
                }
                _ => panic!("stmt is not prefix expression"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = [
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "(add((a * (b[2])), (b[1]), (2 * ([1, 2][1]))))",
            ),
        ];

        for (input, expected) in tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_else(|e| {
                panic!("parse errors for test: {} \n Parse Error: {:?}", input, e)
            });

            let actual = program.to_string();

            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parse errors");

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::If(exp) => {
                    assert_eq!(exp.condition.to_string(), "(x < y)");
                    assert_eq!(exp.consequence.len(), 1);
                    assert_eq!(exp.consequence[0].to_string(), "x");
                    assert!(exp.alternative.is_none());
                }
                _ => panic!("stmt is not if expression"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parse errors");

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::If(exp) => {
                    assert_eq!(exp.condition.to_string(), "(x < y)");
                    assert_eq!(exp.consequence.len(), 1);
                    assert_eq!(exp.consequence[0].to_string(), "x");
                    assert!(exp.alternative.is_some());
                    let alternative = exp.alternative.as_ref().unwrap();
                    assert_eq!(alternative.len(), 1);
                    assert_eq!(alternative[0].to_string(), "y");
                }
                _ => panic!("stmt is not if expression"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::FunctionLiteral { parameters, body } => {
                    assert_eq!(parameters.len(), 2);
                    assert_eq!(parameters[0].to_string(), "x");
                    assert_eq!(parameters[1].to_string(), "y");

                    assert_eq!(body.len(), 1);
                    assert_eq!(body[0].to_string(), "(x + y)");
                }
                _ => panic!("stmt is not function literal"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        assert_eq!(stmt.to_string(), "add(1, (2 * 3), (4 + 5))");
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::ArrayLiteral(elements) => {
                    assert_eq!(elements.len(), 3);
                    assert_eq!(elements[0].to_string(), "1");
                    assert_eq!(elements[1].to_string(), "(2 * 2)");
                    assert_eq!(elements[2].to_string(), "(3 + 3)");
                }
                _ => panic!("stmt is not array literal"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_parse_index_expression() {
        let input = "myArray[1 + 1]";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Index { left, index } => {
                    assert_eq!(left.to_string(), "myArray");
                    assert_eq!(index.to_string(), "(1 + 1)");
                }
                _ => panic!("stmt is not index expression"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_parse_hash_literal() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        let stmt = program.statements.first().unwrap();

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::HashLiteral(pairs) => {
                    assert_eq!(pairs.len(), 3);

                    let expected = vec![
                        ("one".to_string(), "1".to_string()),
                        ("two".to_string(), "2".to_string()),
                        ("three".to_string(), "3".to_string()),
                    ];

                    for (idx, (key, value)) in expected.iter().enumerate() {
                        let pair = pairs.get(idx).unwrap();
                        assert_eq!(pair.0.to_string(), key.to_string());
                        assert_eq!(pair.1.to_string(), value.to_string());
                    }
                }
                _ => panic!("stmt is not hash literal"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }

    #[test]
    fn test_empty_hash_literal() {
        let input = "{}";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|e| panic!("parse errors for test: {} \n Parse Error: {:?}", input, e));

        let stmt = program.statements.first().unwrap();

        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::HashLiteral(pairs) => {
                    assert_eq!(pairs.len(), 0);
                }
                _ => panic!("stmt is not hash literal"),
            },
            _ => panic!("stmt is not expression statement"),
        }
    }
}
