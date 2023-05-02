fn main() {
    let input = r#"
    // single-line comments are denoted by a double slash

    /*
        multi-line comments are denoted by a slash and asterisk
        and end with an asterisk and slash
    */

    some_int=69;
    some_string="foo";
    some_arr=[1,2,3,];
    x=4/2;
    y = some_int.sub(5);
"#;

    let mut lexer = Lexer::new(input);
    let tokens = lexer.lex();
    println!("{:#?}", tokens);
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Comment,
    Identifier,
    Integer,
    String,
    Keyword,
    Symbol,
    Array,
    Function,
    If,
    Else,
    Loop,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: std::str::CharIndices<'a>,
    current_char: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            chars: input.char_indices(),
            current_char: None,
        };
        lexer.advance();
        lexer
    }

    fn advance(&mut self) {
        self.current_char = self.chars.next();
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some((_, ch)) = self.current_char {
            let token = match ch {
                '/' => {
                    // check if the next character is a '/' or '*'
                    if let Some((_, ch)) = self.chars.clone().next() {
                        match ch {
                            '/' => self.lex_comment(),
                            '*' => self.lex_comment(),
                            _ => self.lex_symbol(),
                        }
                    } else {
                        self.lex_symbol()
                    }
                }
                'a'..='z' | 'A'..='Z' => self.lex_identifier(),
                '0'..='9' => self.lex_integer(),
                '"' => self.lex_string(),
                _ if ch.is_whitespace() => {
                    self.advance();
                    continue;
                }
                _ => self.lex_symbol(),
            };
            tokens.push(token);
        }

        tokens
    }

    fn lex_comment(&mut self) -> Token {
        let start = self.current_char.unwrap().0;
        self.advance(); // consume '/'

        if let Some((_, ch)) = self.current_char {
            if ch == '/' {
                while let Some((_, ch)) = self.current_char {
                    if ch == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else if ch == '*' {
                self.advance(); // consume '*'
                let mut last_char = ' ';
                while let Some((_, ch)) = self.current_char {
                    if last_char == '*' && ch == '/' {
                        self.advance(); // consume '/'
                        break;
                    }
                    last_char = ch;
                    self.advance();
                }
            }
        }

        Token {
            kind: TokenKind::Comment,
            value: self.input[start..self.current_char.unwrap().0].to_string(),
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.current_char.unwrap().0;
        while let Some((_, ch)) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let value = self.input[start..self.current_char.unwrap().0].to_string();
        let token_type = if value == "loop" || value == "if" || value == "break" {
            TokenKind::Keyword
        } else {
            TokenKind::Identifier
        };

        Token {
            kind: token_type,
            value,
        }
    }

    fn lex_integer(&mut self) -> Token {
        let start = self.current_char.unwrap().0;
        while let Some((_, ch)) = self.current_char {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        Token {
            kind: TokenKind::Integer,
            value: self.input[start..self.current_char.unwrap().0].to_string(),
        }
    }

    fn lex_string(&mut self) -> Token {
        let start = self.current_char.unwrap().0;
        self.advance(); // consume '"'

        while let Some((_, ch)) = self.current_char {
            if ch == '"' {
                self.advance(); // consume '"'
                break;
            } else {
                self.advance();
            }
        }

        Token {
            kind: TokenKind::String,
            value: self.input[start + 1..self.current_char.unwrap().0 - 1].to_string(),
        }
    }

    fn lex_symbol(&mut self) -> Token {
        let start = self.current_char.unwrap().0;
        self.advance();

        Token {
            kind: TokenKind::Symbol,
            value: self.input[start..self.current_char.unwrap().0].to_string(),
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_single_line_comment() {
        let input = r#"
        // single-line comments are denoted by a double slash
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(
            tokens[0].value,
            "// single-line comments are denoted by a double slash"
        );

        let input = r#"
        // this comment should be added to the tokens
        some_int=69;
        // this comment should also be added to the tokens
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[5].kind, TokenKind::Comment);
        assert_eq!(
            tokens[0].value,
            "// this comment should be added to the tokens"
        );
        assert_eq!(
            tokens[5].value,
            "// this comment should also be added to the tokens"
        );
    }

    #[test]
    fn test_multi_line_comment() {
        let input = r#"
        /* multi-line comments are denoted by a slash and an asterisk
        and are closed by an asterisk and a slash */
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(
            tokens[0].value,
            "/* multi-line comments are denoted by a slash and an asterisk
        and are closed by an asterisk and a slash */"
        );

        let input = r#"
        /* this comment should be added to the tokens */
        some_int=69;
        /*
        this comment should also be added to the tokens
        */
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[5].kind, TokenKind::Comment);
        assert_eq!(
            tokens[0].value,
            "/* this comment should be added to the tokens */"
        );
        assert_eq!(
            tokens[5].value,
            "/*
        this comment should also be added to the tokens
        */"
        );
    }

    #[test]
    fn test_identifier() {
        let input = r#"
        x=some_identifier;
        x
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].value, "x");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].value, "some_identifier");
        assert_eq!(tokens[4].kind, TokenKind::Identifier);
    }

    #[test]
    fn test_integer() {
        let input = r#"
        some_int=69;
        some_other_int=420;
        1337
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[2].kind, TokenKind::Integer);
        assert_eq!(tokens[2].value, "69");
        assert_eq!(tokens[6].kind, TokenKind::Integer);
        assert_eq!(tokens[6].value, "420");
        assert_eq!(tokens[8].kind, TokenKind::Integer);
        assert_eq!(tokens[8].value, "1337");
    }
}
