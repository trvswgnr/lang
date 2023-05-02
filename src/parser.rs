use crate::lexer::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    // Closure(Vec<String>, Box<Stmt>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    // Loop(Box<Stmt>),
    // Break,
    Return(Box<Expr>),
    // Neg(Box<Expr>),
    // Not(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    VarDecl(String, Expr),
    // FuncDecl(String, Vec<String>, Box<Stmt>),
    Loop(Box<Stmt>),
    Break,
    // Return(Expr),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.pos < self.tokens.len() {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_call(&mut self) -> Expr {
        let token = self.peek_current();
        if token.is_none() {
            panic!("expected identifier, found {:?}", token);
        }

        let token = token.unwrap();
        match token.kind {
            TokenKind::Variable(_) => self.parse_func_call(),
            _ => panic!("expected identifier, found {:?}", token),
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
            // TokenKind::Keyword(Keyword::Return) => {
            //     self.advance();
            //     let expr = self.parse_expr();
            //     Stmt::Return(expr)
            // }
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
                self.advance(); // consume number
                Expr::Int(token.value.parse().unwrap())
            }
            TokenKind::String => {
                let token = self.peek_current();
                let next = self.peek_next();
                if next.is_none() {
                    return Expr::Return(Box::new(Expr::Str(token.unwrap().value)));
                }
                let token = self.peek_current().unwrap();
                self.advance(); // consume string
                if self.peek_current().unwrap().kind == TokenKind::Symbol(Symbol::Semicolon) {
                    self.advance(); // consume ';'
                    return Expr::Str(token.value);
                }
                Expr::Return(Box::new(self.parse_expr()))
            }
            TokenKind::Symbol(Symbol::LeftSquare) => {
                self.advance(); // consume '['
                let mut items = Vec::new();
                while self.peek_current().unwrap().kind != TokenKind::Symbol(Symbol::RightSquare) {
                    items.push(self.parse_array_item());
                }
                self.advance(); // consume ']'
                Expr::Array(items)
            }
            TokenKind::Variable(_) => {
                let next = self.peek_next();

                if next.is_none() {
                    return Expr::Return(Box::new(Expr::Var(token.value)));
                }

                let next_token = self.peek_next().unwrap();

                if next_token.kind == TokenKind::Symbol(Symbol::Assign) {
                    self.advance(); // skip variable name
                    self.advance(); // skip '='
                    let expr = self.parse_expr();
                    return Expr::Assign(Box::new(Expr::Var(token.value)), Box::new(expr));
                }

                if next_token.kind == TokenKind::Symbol(Symbol::LeftParen) {
                    return self.parse_call();
                }

                if next_token.kind == TokenKind::Symbol(Symbol::LeftSquare) {
                    self.advance(); // consume variable name
                    self.advance(); // consume '['
                    let index = self.parse_expr();
                    self.advance(); // consume ']'
                    return Expr::Index(Box::new(Expr::Var(token.value)), Box::new(index));
                }

                if next_token.kind == TokenKind::Symbol(Symbol::Semicolon) {
                    self.advance(); // consume variable name
                    return Expr::Var(token.value);
                }

                Expr::Return(Box::new(self.parse_expr()))
            }
            TokenKind::Keyword(Keyword::If) => {
                self.advance(); // consume 'if'
                let cond = self.parse_expr();
                let then = self.parse_expr();
                let els = if self.tokens[self.pos].kind == TokenKind::Keyword(Keyword::Else) {
                    self.advance(); // skip 'else'
                    Some(Box::new(self.parse_expr()))
                } else {
                    None
                };
                Expr::If(Box::new(cond), Box::new(then), els)
            }
            _ => {
                panic!("failed to parse unexpected token {:?}", token);
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
mod tests {
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
            "hello world";
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Str("hello world".to_string()));

        let input = r#"
            "hello world"
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Return(Box::new(Expr::Str("hello world".to_string())))
        );
    }

    #[test]
    fn test_parse_var() {
        let input = r#"
            x;
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Var("x".to_string()));

        let input = r#"
        x
        "#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Return(Box::new(Expr::Var("x".to_string()))));
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
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Call(
                "x".to_string(),
                vec![Expr::Int(1), Expr::Int(2), Expr::Int(3),]
            )
        );
    }
}
