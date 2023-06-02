use crate::token::Token;
use unicode_segmentation::UnicodeSegmentation;

pub struct Lexer<'a> {
    input: Vec<&'a str>,
    position: usize,
    read_position: usize,
    ch: Option<&'a str>,
}

impl<'input_string_lifetime> Lexer<'input_string_lifetime> {
    pub fn new(input: &'input_string_lifetime str) -> Lexer<'input_string_lifetime> {
        let graphemes = input.graphemes(true).collect::<Vec<&str>>();
        Self {
            input: graphemes,
            read_position: 0,
            position: 0,
            ch: None,
        }
    }

    pub fn read_char(self: &mut Self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input[self.read_position])
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(self: &mut Self) -> Token {
        let token = match self.ch {
            None => Token::EOF,
            Some(c) => match c {
                "=" => Token::Assign,
                ";" => Token::SemiColon,
                "(" => Token::LParen,
                ")" => Token::RParen,
                "," => Token::Comma,
                "+" => Token::Plus,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                _ => Token::Illegal,
            },
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
fn test_lexer() {
    let input = "=+(){},;";
}
