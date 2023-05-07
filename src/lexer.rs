use std::iter::Peekable;

use crate::tokenizer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Keyword(Keyword),
    Operator(Operator),
    Delimiter(Delimiter),
    Parenthesis(Parenthesis),
    Brace(Brace),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    For,
    In,
    Def,
    True,
    False,
    None,
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Delimiter {
    Comma,
    Semicolon,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Parenthesis {
    Open,
    Close,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Brace {
    Open,
    Close,
}

#[derive(Debug, Clone)]
pub struct Lexer<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Lexer {
            tokens: tokens.peekable(),
        }
    }

    fn token_to_lexeme(&mut self, token: Token) -> Option<Lexeme> {
        match token {
            Token::Identifier(name) => Some(Lexeme::Identifier(name)),
            Token::Integer(value) => Some(Lexeme::Integer(value)),
            Token::Float(value) => Some(Lexeme::Float(value)),
            Token::String(value) => Some(Lexeme::String(value)),
            Token::Keyword(keyword) => Some(Lexeme::Keyword(match keyword.as_str() {
                "if" => Keyword::If,
                "else" => Keyword::Else,
                "while" => Keyword::While,
                "for" => Keyword::For,
                "in" => Keyword::In,
                "def" => Keyword::Def,
                "True" => Keyword::True,
                "False" => Keyword::False,
                "None" => Keyword::None,
                "return" => Keyword::Return,
                _ => return None,
            })),
            Token::Operator(operator) => Some(Lexeme::Operator(match operator.as_str() {
                "+" => Operator::Add,
                "-" => Operator::Sub,
                "*" => Operator::Mul,
                "/" => Operator::Div,
                "%" => Operator::Mod,
                "**" => Operator::Pow,
                "==" => Operator::Eq,
                "!=" => Operator::Ne,
                "<" => Operator::Lt,
                ">" => Operator::Gt,
                "<=" => Operator::Le,
                ">=" => Operator::Ge,
                "and" => Operator::And,
                "or" => Operator::Or,
                "not" => Operator::Not,
                _ => return None,
            })),
            Token::Delimiter(ch) => Some(Lexeme::Delimiter(match ch {
                ',' => Delimiter::Comma,
                ';' => Delimiter::Semicolon,
                _ => return None,
            })),
            Token::Parenthesis(ch) => Some(Lexeme::Parenthesis(match ch {
                '(' => Parenthesis::Open,
                ')' => Parenthesis::Close,
                _ => return None,
            })),
            Token::Brace(ch) => Some(Lexeme::Brace(match ch {
                '{' => Brace::Open,
                '}' => Brace::Close,
                _ => return None,
            })),
        }
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = Token>,
{
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.next()?;
        self.token_to_lexeme(token)
    }
}
