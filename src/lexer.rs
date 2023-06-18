use crate::token::Token;
use unicode_segmentation::UnicodeSegmentation;

fn is_letter(c: &str) -> bool {
    c.chars().all(|c| c.is_alphabetic() || c == '_')
}

fn is_digit(c: &str) -> bool {
    c.chars().all(|c| c.is_ascii_digit())
}

pub struct Lexer<'input_string_lifetime> {
    input: Vec<&'input_string_lifetime str>,
    position: usize,
    read_position: usize,
    ch: Option<&'input_string_lifetime str>,
}

impl<'input_string_lifetime> Lexer<'input_string_lifetime> {
    pub fn new(input: &'input_string_lifetime str) -> Lexer<'input_string_lifetime> {
        let graphemes = input.graphemes(true).collect::<Vec<&str>>();
        let mut lexer = Self {
            input: graphemes,
            read_position: 0,
            position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input[self.read_position])
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_some() && is_digit(self.ch.unwrap()) {
            self.read_char();
        }

        self.input[position..self.position].concat()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == Some("\"") || self.ch.is_none() {
                break;
            }
        }
        self.input[position..self.position].concat()
    }

    fn read_ident(&mut self) -> String {
        let position = self.position;
        while self.ch.is_some() && is_letter(self.ch.unwrap()) {
            self.read_char();
        }

        self.input[position..self.position].concat()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some() && self.ch.unwrap().chars().all(|c| c.is_whitespace()) {
            self.read_char();
        }
    }

    fn peek_char(&self) -> Option<&str> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            None => Token::EOF,
            Some(c) => match c {
                "\"" => Token::String(self.read_string()),
                "=" => {
                    if self.peek_char() == Some("=") {
                        self.read_char();
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                }
                ";" => Token::SemiColon,
                "(" => Token::LParen,
                ")" => Token::RParen,
                "," => Token::Comma,
                ":" => Token::Colon,
                "+" => Token::Plus,
                "-" => Token::Minus,
                "!" => {
                    if self.peek_char() == Some("=") {
                        self.read_char();
                        Token::NotEq
                    } else {
                        Token::Bang
                    }
                }
                "*" => Token::Asterisk,
                "/" => Token::Slash,
                "<" => Token::Lt,
                ">" => Token::Gt,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                "[" => Token::LBracket,
                "]" => Token::RBracket,
                t => {
                    if is_letter(t) {
                        let ident = self.read_ident();
                        return match ident.as_str() {
                            "fn" => Token::Function,
                            "let" => Token::Let,
                            "true" => Token::True,
                            "false" => Token::False,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "return" => Token::Return,
                            _ => Token::Ident(ident),
                        };
                    }

                    if is_digit(t) {
                        let number = self.read_number();
                        return Token::Int(number);
                    }
                    return Token::Illegal;
                }
            },
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};

    #[test]
    fn test_lexer() {
        let input = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
             x + y;
        };

        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;
    

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;

        \"foobar\"
        \"foo bar\"
        [1, 2]
        {\"foo\": \"bar\"}
        ";

        let expected_tokens = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::SemiColon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::SemiColon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::SemiColon,
            Token::If,
            Token::LParen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::SemiColon,
            Token::RBrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::SemiColon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::SemiColon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::LBracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::RBracket,
            Token::LBrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::RBrace,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for (idx, expected_token) in expected_tokens.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(token, *expected_token, "token at index {}", idx);
        }
    }
}
