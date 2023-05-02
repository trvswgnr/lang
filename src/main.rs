mod lexer;
mod parser;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let input = r#"
        x = 5;
        loop {
            if x > 9 {
                break;
            }
            x = x + 1;
        }
    "#;

    let mut lexer = Lexer::new(input);
    let tokens = lexer.lex();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:#?}", ast);
}
