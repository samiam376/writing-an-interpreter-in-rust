use std::io::Write;

use writing_an_interpreter_in_rust::{
    ast::Node, evaluator::eval, lexer::Lexer, object::Environment, parser::Parser,
};

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'"#;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    println!("{}", MONKEY_FACE);
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let input = &mut String::new();
        std::io::stdin().read_line(input).unwrap();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        match program {
            Ok(program) => {
                let mut env = Environment::new();
                let evaluated = eval(Node::Program(program), &env);
                match evaluated {
                    Ok(evaluated) => println!("{}", evaluated),
                    Err(error) => println!("{}", error),
                }
            }
            Err(errors) => {
                println!("Woops! We ran into some monkey business here!");
                for error in errors {
                    println!("{}", error);
                }
            }
        }
    }
}
