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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    /// Declaration or reference of a namespace, module, or package.
    Namespace(Option<Modifier>),
    /// Declaration or reference of an enumeration type.
    Enum(Option<Modifier>),
    /// Declaration or reference of an interface type.
    Interface(Option<Modifier>),
    /// Declaration or reference of a struct type.
    Struct(Option<Modifier>),
    /// Declaration or reference of a type parameter.
    TypeParameter(Option<Modifier>),
    /// Declaration or reference of a type that is not by other types.
    Type(Option<Modifier>),
    /// Declaration or reference of a function or method parameters.
    Parameter(Option<Modifier>),
    /// Declaration or reference of a local or global variable.
    Variable(Option<Modifier>),
    /// Declaration or reference of a member property, member field, or member variable.
    Property(Option<Modifier>),
    /// Declaration or reference of an enumeration property, constant, or member.
    EnumMember(Option<Modifier>),
    /// Declaration or reference of decorators and annotations.
    Decorator(Option<Modifier>),
    /// Declaration of an event property.
    Event(Option<Modifier>),
    /// Declaration of a function.
    Function(Option<Modifier>),
    /// Declaration of a member function or method.
    Method(Option<Modifier>),
    /// Declaration of a macro.
    Macro,
    /// Declaration of a label.
    Label,
    /// A comment.
    Comment,
    /// A string literal.
    String,
    /// A language keyword.
    Keyword(Keyword),
    /// A number literal.
    Number,
    /// A regular expression literal.
    Regexp,
    /// An operator.
    Operator,
    /// A symbol.
    Symbol(Symbol),
    /// An unknown token.
    Unknown,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Fn,
    If,
    Else,
    Loop,
    Break,
    Return,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Modifier {
    /// Declaration of symbols.
    Declaration,
    /// Definition of symbols, for example, in header files.
    Definition,
    /// Readonly variables and member fields (constants).
    Readonly,
    /// Class members (static members).
    Static,
    /// Symbols that should no longer be used.
    Deprecated,
    /// Types and member functions that are abstract.
    Abstract,
    /// Functions that are marked async.
    Async,
    /// Variable references where the variable is assigned to.
    Modification,
    /// Occurrences of symbols in documentation.
    Documentation,
    /// Symbols that are part of the standard library.
    DefaultLibrary,
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
        assert_eq!(tokens[0].kind, TokenKind::Variable(None));
        assert_eq!(tokens[0].value, "x");
        assert_eq!(tokens[2].kind, TokenKind::Variable(None));
        assert_eq!(tokens[2].value, "some_identifier");
        assert_eq!(tokens[4].kind, TokenKind::Variable(None));
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Closure(Vec<String>, Box<Stmt>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Loop(Box<Stmt>),
    Break,
    Return(Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
}

struct ExprOrStmt(Box<dyn ExpressionOrStatement>);

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    VarDecl(String, Expr),
    FuncDecl(String, Vec<String>, Box<Stmt>),
    Loop(Box<Stmt>),
    Break,
    Return(Expr),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
}

trait ExpressionOrStatement {
    fn is_expr(&self) -> bool;
}

impl ExpressionOrStatement for Stmt {
    fn is_expr(&self) -> bool {
        matches!(self, Stmt::Expr(_))
    }
}

impl ExpressionOrStatement for Expr {
    fn is_expr(&self) -> bool {
        matches!(
            self,
            Expr::Int(_)
                | Expr::Str(_)
                | Expr::Var(_)
                | Expr::Array(_)
                | Expr::Index(_, _)
                | Expr::Assign(_, _)
                | Expr::Call(_, _)
                | Expr::Closure(_, _)
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.pos < self.tokens.len() {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_call(&mut self) -> Expr {
        let token = self.peek_current();
        if token.is_none() {
            panic!("expected identifier")
        }

        let token = token.unwrap();
        match token.kind {
            TokenKind::Variable(_) => self.parse_func_call(),
            _ => panic!("expected identifier"),
        }
    }

    fn parse_func_call(&mut self) -> Expr {
        let token = self.tokens[self.pos].clone();
        let mut args = Vec::new();
        self.advance();
        if self.tokens[self.pos].kind == TokenKind::Symbol(Symbol::LeftParen) {
            self.advance();
            while self.tokens[self.pos].kind != TokenKind::Symbol(Symbol::RightParen) {
                let expr = self.parse_expr();
                args.push(expr);
                if self.tokens[self.pos].kind == TokenKind::Symbol(Symbol::Comma) {
                    self.advance();
                }
            }
            self.advance();
        }
        Expr::Call(token.value, args)
    }

    fn parse_stmt(&mut self) -> Stmt {
        let token = self.tokens[self.pos].clone();
        match token.kind {
            TokenKind::Variable(_) => {
                self.advance();
                if self.tokens[self.pos].kind == TokenKind::Symbol(Symbol::Assign) {
                    self.advance();
                    let expr = self.parse_expr();
                    Stmt::VarDecl(token.value, expr)
                } else {
                    panic!("expected '='")
                }
            }
            TokenKind::Keyword(Keyword::Loop) => {
                self.advance();
                let stmt = self.parse_stmt();
                Stmt::Loop(Box::new(stmt))
            }
            TokenKind::Keyword(Keyword::Break) => {
                self.advance();
                Stmt::Break
            }
            TokenKind::Keyword(Keyword::Return) => {
                self.advance();
                let expr = self.parse_expr();
                Stmt::Return(expr)
            }
            TokenKind::Keyword(Keyword::If) => {
                self.advance();
                let cond = self.parse_expr();
                let then = self.parse_stmt();
                let els = if self.tokens[self.pos].kind == TokenKind::Keyword(Keyword::Else) {
                    self.advance();
                    Some(Box::new(self.parse_stmt()))
                } else {
                    None
                };
                Stmt::If(Box::new(cond), Box::new(then), els)
            }
            TokenKind::Symbol(Symbol::LeftCurly) => {
                self.advance();
                let mut stmts = Vec::new();
                while self.tokens[self.pos].kind != TokenKind::Symbol(Symbol::RightCurly) {
                    stmts.push(self.parse_stmt());
                }
                self.advance();
                Stmt::Block(stmts)
            }
            _ => {
                let expr = self.parse_expr();
                Stmt::Expr(expr)
            }
        }
    }

    fn peek_current(&self) -> Option<Token> {
        self.tokens.get(self.pos).cloned()
    }

    fn peek_next(&self) -> Option<Token> {
        self.tokens.get(self.pos + 1).cloned()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn parse_expr(&mut self) -> Expr {
        let token = self.peek_current().unwrap();
        match token.kind {
            TokenKind::Number => {
                self.advance();
                Expr::Int(token.value.parse().unwrap())
            }
            TokenKind::String => {
                self.advance();
                Expr::Str(token.value)
            }
            TokenKind::Symbol(Symbol::LeftParen) => {
                panic!("not implemented");
                self.advance();
                let expr = self.parse_expr();
                self.advance();
                expr
            }
            // TokenKind::Symbol(Symbol::Minus) => {
            //     self.advance();
            //     let expr = self.parse_expr();
            //     Expr::Neg(Box::new(expr))
            // }
            // TokenKind::Symbol(Symbol::Not) => {
            //     self.advance();
            //     let expr = self.parse_expr();
            //     Expr::Not(Box::new(expr))
            // }
            TokenKind::Symbol(Symbol::LeftSquare) => {
                self.advance();
                let mut items = Vec::new();
                while self.peek_current().unwrap().kind != TokenKind::Symbol(Symbol::RightSquare) {
                    items.push(self.parse_array_item());
                }
                self.advance();
                Expr::Array(items)
            }
            TokenKind::Variable(_) => {
                let next = self.peek_next();

                if next.is_none() {
                    return Expr::Var(token.value);
                }

                let next_token = next.unwrap();
                self.advance();

                if next_token.kind == TokenKind::Symbol(Symbol::Assign) {
                    self.advance();
                    let expr = self.parse_expr();
                    return Expr::Assign(Box::new(Expr::Var(token.value)), Box::new(expr));
                }

                if next_token.kind == TokenKind::Symbol(Symbol::LeftParen) {
                    self.advance();
                    let mut args = Vec::new();
                    while next_token.kind != TokenKind::Symbol(Symbol::RightParen) {
                        args.push(self.parse_expr());
                    }
                    self.advance();
                    return Expr::Call(token.value, args);
                }

                if next_token.kind == TokenKind::Symbol(Symbol::LeftSquare) {
                    self.advance();
                    let index = self.parse_expr();
                    self.advance();
                    return Expr::Index(Box::new(Expr::Var(token.value)), Box::new(index));
                }

                Expr::Var(token.value)
            }
            TokenKind::Keyword(Keyword::If) => {
                self.advance();
                let cond = self.parse_expr();
                let then = self.parse_expr();
                let els = if self.tokens[self.pos].kind == TokenKind::Keyword(Keyword::Else) {
                    self.advance();
                    Some(Box::new(self.parse_expr()))
                } else {
                    None
                };
                Expr::If(Box::new(cond), Box::new(then), els)
            }
            _ => {
                self.advance();
                Expr::Var(token.value)
            }
        }
    }

    fn parse_array_item(&mut self) -> Expr {
        let token = self.peek_current().unwrap();
        match token.kind {
            TokenKind::Symbol(Symbol::Comma) => {
                self.advance();
                self.parse_array_item()
            }
            TokenKind::Number => {
                self.advance();
                Expr::Int(token.value.parse().unwrap())
            }
            TokenKind::String => {
                self.advance();
                Expr::Str(token.value)
            }
            TokenKind::Symbol(Symbol::LeftSquare) => {
                self.advance();
                let mut items = Vec::new();
                while self.peek_current().unwrap().kind != TokenKind::Symbol(Symbol::RightSquare) {
                    items.push(self.parse_array_item());
                }
                self.advance();
                Expr::Array(items)
            }
            TokenKind::Variable(_) => {
                let next = self.peek_next();
                if next.is_none() {
                    return Expr::Var(token.value);
                }
                let next_token = next.unwrap();
                self.advance();
                if next_token.kind == TokenKind::Symbol(Symbol::LeftParen) {
                    self.advance();
                    let mut args = Vec::new();
                    while next_token.kind != TokenKind::Symbol(Symbol::RightParen) {
                        args.push(self.parse_expr());
                    }
                    self.advance();
                    Expr::Call(token.value, args)
                } else if next_token.kind == TokenKind::Symbol(Symbol::LeftSquare) {
                    self.advance();
                    let index = self.parse_expr();
                    self.advance();
                    Expr::Index(Box::new(Expr::Var(token.value)), Box::new(index))
                } else {
                    Expr::Var(token.value)
                }
            }
            TokenKind::Keyword(Keyword::If) => {
                self.advance();
                let cond = self.parse_expr();
                let then = self.parse_expr();
                let els = if self.tokens[self.pos].kind == TokenKind::Keyword(Keyword::Else) {
                    self.advance();
                    Some(Box::new(self.parse_expr()))
                } else {
                    None
                };
                Expr::If(Box::new(cond), Box::new(then), els)
            }
            _ => {
                self.advance();
                Expr::Var(token.value)
            }
        }
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_parse_int() {
        let input = r#"
        69
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Int(69));
    }

    #[test]
    fn test_parse_str() {
        let input = r#"
        "hello world"
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Str("hello world".to_string()));
    }

    #[test]
    fn test_parse_var() {
        let input = r#"
        x
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Var("x".to_string()));
    }

    #[test]
    fn test_parse_array() {
        let input = r#"
        [1, 2, 3]
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Array(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3),])
        );
    }

    #[test]
    fn test_parse_index() {
        let input = r#"
        x[1]
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Index(Box::new(Expr::Var("x".to_string())), Box::new(Expr::Int(1)),)
        );
    }

    #[test]
    fn test_parse_assign() {
        let input = r#"
        x=5;
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Assign(Box::new(Expr::Var("x".to_string())), Box::new(Expr::Int(5)),)
        );
    }

    #[test]
    fn test_parse_call() {
        let input = r#"
        x(1, 2, 3)
    "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_call();
        assert_eq!(
            expr,
            Expr::Call(
                "x".to_string(),
                vec![Expr::Int(1), Expr::Int(2), Expr::Int(3),]
            )
        );
    }
}
