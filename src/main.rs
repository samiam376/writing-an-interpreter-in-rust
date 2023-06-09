use std::io::Write;

use writing_an_interpreter_in_rust::{
    evaluator::eval, lexer::Lexer, object::Environment, parser::Parser,
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

    let mut env = Environment::new();

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
                let evaluated = eval(program.into(), &mut env);
                match evaluated {
                    Ok(evaluated) => {
                        if let Some(evaluated) = evaluated {
                            println!("{}", evaluated);
                        }
                    }
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
