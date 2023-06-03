use std::io::Write;

use writing_an_interpreter_in_rust::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

fn main() {
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let input = &mut String::new();
        std::io::stdin().read_line(input).unwrap();
        let mut lexer = Lexer::new(input);
        loop {
            let token = lexer.next_token();
            println!("{:?}", token);
            if token == Token::EOF {
                break;
            }
        }
    }
}
