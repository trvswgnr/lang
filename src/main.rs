mod lexer;
mod parser;
mod tokenizer;

use lexer::Lexer;
use parser::Parser;
use tokenizer::Tokenizer;

fn main() {
    let input = r#"
        def example_function(x, y) {
            if (x > y) {
                return z;
            }
        }
    "#;

    let tokenizer = Tokenizer::new(input);
    // println!("{:#?}", tokenizer.clone().collect::<Vec<_>>());
    let lexer = Lexer::new(tokenizer);
    // println!("{:#?}", lexer.clone().collect::<Vec<_>>());
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => eprintln!("Error: {}", err),
    }
}
