use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Keyword(String),
    Operator(String),
    Delimiter(char),
    Parenthesis(char),
    Brace(char),
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    chars: Peekable<CharIndices<'a>>,
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            chars: input.char_indices().peekable(),
            input,
        }
    }

    fn read_while<F>(&mut self, test: F) -> Option<(usize, usize)>
    where
        F: Fn(char) -> bool,
    {
        let start = self.chars.peek()?.0;
        while let Some((_, ch)) = self.chars.peek() {
            if !test(*ch) {
                break;
            }
            self.chars.next();
        }
        let end = self.chars.peek()?.0;
        Some((start, end))
    }

    fn read_number(&mut self, start: usize) -> Token {
        let end = self.read_while(|ch| ch.is_ascii_digit()).unwrap().1;
        let int_part = &self.input[start..end];
        if let Some((_, '.')) = self.chars.peek() {
            self.chars.next();
            let float_end = self.read_while(|ch| ch.is_ascii_digit()).unwrap().1;
            let float_part = &self.input[end + 1..float_end];
            let float_value = format!("{}.{}", int_part, float_part).parse().unwrap();
            Token::Float(float_value)
        } else {
            Token::Integer(int_part.parse().unwrap())
        }
    }

    fn read_string(&mut self, quote: char) -> Token {
        let start = self.chars.next().unwrap().0;
        let end = self.read_while(|ch| ch != quote).unwrap().1;
        self.chars.next(); // Consume closing quote
        Token::String(self.input[start + 1..end].to_string())
    }

    fn read_identifier_or_keyword(&mut self, start: usize) -> Token {
        let end = self
            .read_while(|ch| ch.is_ascii_alphanumeric() || ch == '_')
            .unwrap()
            .1;
        let word = self.input[start..end].to_string();
        if is_keyword(&word) {
            Token::Keyword(word)
        } else {
            Token::Identifier(word)
        }
    }
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "if" | "else" | "while" | "for" | "in" | "def" | "True" | "False" | "None" | "return"
    )
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (_, ch) = *self.chars.peek()?;
        let token = match ch {
            ch if ch.is_ascii_digit() => {
                let start = self.chars.next().unwrap().0;
                self.read_number(start)
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let start = self.chars.next().unwrap().0;
                self.read_identifier_or_keyword(start)
            }
            '"' | '\'' => self.read_string(ch),
            '+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' => {
                let start = self.chars.next().unwrap().0;
                let end = self.read_while(|ch| ch == '=').unwrap_or((start, start)).1;
                // remove whitespace
                Token::Operator(self.input[start..end + 1].trim().to_string())
            }
            '(' | ')' => {
                self.chars.next();
                Token::Parenthesis(ch)
            }
            '{' | '}' => {
                self.chars.next();
                Token::Brace(ch)
            }
            ',' | ';' => {
                self.chars.next();
                Token::Delimiter(ch)
            }
            ch if ch.is_whitespace() => {
                self.chars.next();
                return self.next();
            }
            _ => return None,
        };
        Some(token)
    }
}
