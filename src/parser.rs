use crate::lexer::{Brace, Delimiter, Keyword, Lexeme, Operator, Parenthesis};
use std::iter::Peekable;

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Program(Vec<AstNode>),
    Assignment(String, Box<AstNode>),
    IfStatement(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>),
    WhileStatement(Box<AstNode>, Box<AstNode>),
    ForStatement(String, Box<AstNode>, Box<AstNode>),
    ReturnStatement(Box<AstNode>),
    FunctionDefinition(String, Vec<String>, Box<AstNode>),
    FunctionCall(String, Vec<AstNode>),
    Expression(Box<AstNode>),
    BinaryOperation(Operator, Box<AstNode>, Box<AstNode>),
    UnaryOperation(Operator, Box<AstNode>),
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

pub struct Parser<I>
where
    I: Iterator<Item = Lexeme>,
{
    lexemes: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Lexeme>,
{
    pub fn new(lexemes: I) -> Self {
        Parser {
            lexemes: lexemes.peekable(),
        }
    }

    fn expect(&mut self, expected: &Lexeme) -> Result<(), String> {
        let next = self.lexemes.next();
        if next.as_ref() == Some(expected) {
            Ok(())
        } else {
            Err(format!("Parser: expected {:?}, found {:?}", expected, next))
        }
    }

    pub fn parse_program(&mut self) -> Result<AstNode, String> {
        let mut statements = Vec::new();
        while self.lexemes.peek().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(AstNode::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.lexemes.peek() {
            Some(Lexeme::Keyword(Keyword::If)) => self.parse_if_statement(),
            Some(Lexeme::Keyword(Keyword::While)) => self.parse_while_statement(),
            Some(Lexeme::Keyword(Keyword::For)) => self.parse_for_statement(),
            Some(Lexeme::Keyword(Keyword::Def)) => self.parse_function_definition(),
            Some(Lexeme::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Lexeme::Identifier(_)) => {
                let next = self.lexemes.peek().cloned();
                if let Some(Lexeme::Identifier(name)) = next {
                    self.lexemes.next();
                    match self.lexemes.peek() {
                        Some(Lexeme::Operator(Operator::Eq)) => {
                            self.lexemes.next();
                            let expr = self.parse_expression()?;
                            self.expect(&Lexeme::Delimiter(Delimiter::Semicolon))?;
                            Ok(AstNode::Assignment(name, Box::new(expr)))
                        }
                        Some(Lexeme::Parenthesis(Parenthesis::Open)) => {
                            self.lexemes.next();
                            let args = self.parse_expression_list()?;
                            self.expect(&Lexeme::Parenthesis(Parenthesis::Close))?;
                            self.expect(&Lexeme::Delimiter(Delimiter::Semicolon))?;
                            Ok(AstNode::FunctionCall(name, args))
                        }
                        _ => Err(format!("Unexpected token: {:?}", self.lexemes.peek())),
                    }
                } else {
                    Err("Expected identifier".to_string())
                }
            }
            _ => self.parse_expression().map(|expr| {
                self.expect(&Lexeme::Delimiter(Delimiter::Semicolon))
                    .unwrap();
                AstNode::Expression(Box::new(expr))
            }),
        }
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, String> {
        println!("parse_return_statement");
        self.expect(&Lexeme::Keyword(Keyword::Return))?;
        let expr = self.parse_expression()?;
        self.expect(&Lexeme::Delimiter(Delimiter::Semicolon))?;
        Ok(AstNode::ReturnStatement(Box::new(expr)))
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        self.expect(&Lexeme::Keyword(Keyword::If))?;
        self.expect(&Lexeme::Parenthesis(Parenthesis::Open))?;
        let condition = self.parse_expression()?;
        self.expect(&Lexeme::Parenthesis(Parenthesis::Close))?;
        let if_block = self.parse_block()?;
        if let Some(Lexeme::Keyword(Keyword::Else)) = self.lexemes.peek() {
            self.lexemes.next();
            let else_block = self.parse_block()?;
            return Ok(AstNode::IfStatement(
                Box::new(condition),
                Box::new(if_block),
                Some(Box::new(else_block)),
            ));
        }

        Ok(AstNode::IfStatement(
            Box::new(condition),
            Box::new(if_block),
            None,
        ))
    }

    fn parse_while_statement(&mut self) -> Result<AstNode, String> {
        self.expect(&Lexeme::Keyword(Keyword::While))?;
        self.expect(&Lexeme::Parenthesis(Parenthesis::Open))?;
        let condition = self.parse_expression()?;
        self.expect(&Lexeme::Parenthesis(Parenthesis::Close))?;
        let block = self.parse_block()?;
        Ok(AstNode::WhileStatement(
            Box::new(condition),
            Box::new(block),
        ))
    }

    fn parse_for_statement(&mut self) -> Result<AstNode, String> {
        self.expect(&Lexeme::Keyword(Keyword::For))?;
        let iterator = match self.lexemes.next() {
            Some(Lexeme::Identifier(name)) => name,
            _ => return Err("Expected identifier".to_string()),
        };
        self.expect(&Lexeme::Keyword(Keyword::In))?;
        let iterable = self.parse_expression()?;
        let block = self.parse_block()?;
        Ok(AstNode::ForStatement(
            iterator,
            Box::new(iterable),
            Box::new(block),
        ))
    }

    fn parse_function_definition(&mut self) -> Result<AstNode, String> {
        self.expect(&Lexeme::Keyword(Keyword::Def))?;
        let name = match self.lexemes.next() {
            Some(Lexeme::Identifier(name)) => name,
            _ => return Err("Expected identifier".to_string()),
        };
        self.expect(&Lexeme::Parenthesis(Parenthesis::Open))?;
        let params = self.parse_parameter_list()?;
        self.expect(&Lexeme::Parenthesis(Parenthesis::Close))?;
        let block = self.parse_block()?;
        Ok(AstNode::FunctionDefinition(name, params, Box::new(block)))
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<String>, String> {
        let mut params = Vec::new();
        while let Some(Lexeme::Identifier(name)) = self.lexemes.peek().cloned() {
            self.lexemes.next();
            params.push(name);
            if let Some(Lexeme::Delimiter(Delimiter::Comma)) = self.lexemes.peek() {
                self.lexemes.next();
            } else {
                break;
            }
        }
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<AstNode, String> {
        self.expect(&Lexeme::Brace(Brace::Open))?;
        let mut statements = Vec::new();
        while self.lexemes.peek().is_some() {
            if let Lexeme::Brace(Brace::Close) = self.lexemes.peek().unwrap() {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        self.expect(&Lexeme::Brace(Brace::Close))?;
        Ok(AstNode::Program(statements))
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        let left = self.parse_term()?;
        if let Some(Lexeme::Operator(op)) = self.lexemes.peek().cloned() {
            self.lexemes.next();
            let right = self.parse_expression()?;
            Ok(AstNode::BinaryOperation(
                op,
                Box::new(left),
                Box::new(right),
            ))
        } else {
            Ok(left)
        }
    }

    fn parse_term(&mut self) -> Result<AstNode, String> {
        match self.lexemes.next() {
            Some(Lexeme::Identifier(name)) => Ok(AstNode::Identifier(name)),
            Some(Lexeme::Integer(value)) => Ok(AstNode::Integer(value)),
            Some(Lexeme::Float(value)) => Ok(AstNode::Float(value)),
            Some(Lexeme::String(value)) => Ok(AstNode::String(value)),
            Some(Lexeme::Keyword(Keyword::True)) => Ok(AstNode::Boolean(true)),
            Some(Lexeme::Keyword(Keyword::False)) => Ok(AstNode::Boolean(false)),
            Some(Lexeme::Keyword(Keyword::None)) => Ok(AstNode::None),
            Some(Lexeme::Parenthesis(Parenthesis::Open)) => {
                let expr = self.parse_expression()?;
                self.expect(&Lexeme::Parenthesis(Parenthesis::Close))?;
                Ok(expr)
            }
            _ => Err("Unexpected token in expression".to_string()),
        }
    }

    fn parse_expression_list(&mut self) -> Result<Vec<AstNode>, String> {
        let mut expr_list = Vec::new();
        while self.lexemes.peek().is_some() {
            if let Lexeme::Parenthesis(Parenthesis::Close) = self.lexemes.peek().unwrap() {
                break;
            }
            expr_list.push(self.parse_expression()?);
            if let Some(Lexeme::Delimiter(Delimiter::Comma)) = self.lexemes.peek() {
                self.lexemes.next();
            } else {
                break;
            }
        }
        Ok(expr_list)
    }
}
