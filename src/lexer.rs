#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Namespace(Option<Modifier>),
    // Enum(Option<Modifier>),
    // Interface(Option<Modifier>),
    // Struct(Option<Modifier>),
    // TypeParameter(Option<Modifier>),
    // Type(Option<Modifier>),
    // Parameter(Option<Modifier>),
    Variable(Option<Modifier>),
    // Property(Option<Modifier>),
    // EnumMember(Option<Modifier>),
    // Decorator(Option<Modifier>),
    // Event(Option<Modifier>),
    // Function(Option<Modifier>),
    // Method(Option<Modifier>),
    // Macro(Option<Modifier>),
    // Label,
    Comment,
    String,
    Keyword(Keyword),
    Number,
    // Regexp,
    // Operator,
    Symbol(Symbol),
    // Unknown,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    Dot,
    Assign,
    Comma,
    Semicolon,
    GreaterThan,
    LessThan,
    Plus,
    Minus,
    Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    // Fn,
    If,
    Else,
    Loop,
    Break,
    // Return,
    // Continue,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Modifier {
    /// declaration of symbols
    Declaration,
    /// definition of symbols, for example, in header files
    Definition,
    /// readonly variables and member fields (constants)
    Readonly,
    /// class members (static members)
    Static,
    /// symbols that should no longer be used
    Deprecated,
    /// types and member functions that are abstract
    Abstract,
    /// functions that are marked async
    Async,
    /// variable references where the variable is assigned to
    Modification,
    /// occurrences of symbols in documentation
    Documentation,
    /// symbols that are part of the standard library
    DefaultLibrary,
}

impl From<char> for Symbol {
    fn from(ch: char) -> Self {
        match ch {
            '(' => Symbol::LeftParen,
            ')' => Symbol::RightParen,
            '[' => Symbol::LeftSquare,
            ']' => Symbol::RightSquare,
            '{' => Symbol::LeftCurly,
            '}' => Symbol::RightCurly,
            '.' => Symbol::Dot,
            '=' => Symbol::Assign,
            ',' => Symbol::Comma,
            ';' => Symbol::Semicolon,
            '>' => Symbol::GreaterThan,
            '<' => Symbol::LessThan,
            '+' => Symbol::Plus,
            '-' => Symbol::Minus,
            '!' => Symbol::Not,
            _ => panic!("Invalid symbol: {}", ch),
        }
    }
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

        if self.current_char.is_none() {
            return Token {
                kind: TokenKind::Variable(None),
                value: self.input[start..].to_string(),
            };
        }

        let value = self.input.get(start..self.current_char.unwrap().0);
        if value.is_none() {
            panic!("Invalid identifier");
        }
        let value = value.unwrap().to_string();
        let token_type = if value == "loop" || value == "if" || value == "break" {
            match value.as_str() {
                "loop" => TokenKind::Keyword(Keyword::Loop),
                "if" => TokenKind::Keyword(Keyword::If),
                "break" => TokenKind::Keyword(Keyword::Break),
                _ => unreachable!(),
            }
        } else {
            TokenKind::Variable(None)
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
            kind: TokenKind::Number,
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

        let value = self.input[start..self.current_char.unwrap().0]
            .chars()
            .next()
            .unwrap();

        Token {
            kind: TokenKind::Symbol(value.into()),
            value: value.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(tokens[0].kind, TokenKind::Variable(None));
        assert_eq!(tokens[0].value, "x");
        assert_eq!(tokens[2].kind, TokenKind::Variable(None));
        assert_eq!(tokens[2].value, "some_identifier");
        assert_eq!(tokens[4].kind, TokenKind::Variable(None));

        let input = r#"
        x;
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, TokenKind::Variable(None));
        assert_eq!(tokens[0].value, "x");
        assert_eq!(tokens[1].kind, TokenKind::Symbol(Symbol::Semicolon));
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
        assert_eq!(tokens[2].kind, TokenKind::Number);
        assert_eq!(tokens[2].value, "69");
        assert_eq!(tokens[6].kind, TokenKind::Number);
        assert_eq!(tokens[6].value, "420");
        assert_eq!(tokens[8].kind, TokenKind::Number);
        assert_eq!(tokens[8].value, "1337");
    }
}
