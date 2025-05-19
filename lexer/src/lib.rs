#![allow(dead_code)]
use std::iter::Peekable;
use std::str::CharIndices;

use tokens::{Token, TokenKind};

pub mod tokens;

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Lexer<'src> {
    pub content: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(program: &'src str) -> Self {
        Lexer {
            content: program,
            chars: program.char_indices().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        use tokens::TokenKind::*;

        self.skip_whitespace();

        let Some((cur_index, cur_char)) = self.peek() else {
            return Token::new(Eof, self.content.len(), 1);
        };

        match cur_char {
            '.' => {
                self.next();
                if self.expect_char('.') && self.expect_char('.') {
                    Token::new(Variadic, cur_index, 3)
                } else {
                    self.make_single_char_token(cur_index, Dot)
                }
            }
            '"' => {
                // skip first `"`
                self.next();
                let size = self.read_string(cur_index).len();
                // skip second `"`
                self.next();
                Token::new(StringLiteral, cur_index, size + 1)
            }
            '[' => self.make_single_char_token(cur_index, LBrack),
            ']' => self.make_single_char_token(cur_index, RBrack),
            '#' => self.make_single_char_token(cur_index, Pound),
            '+' => self.make_single_char_token(cur_index, Plus),
            '-' => {
                self.next();
                if self.expect_char('>') {
                    Token::new(RArrow, cur_index, 2)
                } else if self.expect_char('#') {
                    // skip '#'
                    let (cur_index, _) = self.next().unwrap();
                    let size = self.read_comment(cur_index);
                    Token::new(DocComment, cur_index, size)
                } else if self.expect_char('-') {
                    // skip '-'
                    let (cur_index, _) = self.next().unwrap();
                    let size = self.read_comment(cur_index);
                    Token::new(Comment, cur_index, size)
                } else {
                    self.make_single_char_token(cur_index, Minus)
                }
            }
            '/' => self.make_single_char_token(cur_index, Slash),
            '*' => self.make_single_char_token(cur_index, Star),
            '&' => self.make_single_char_token(cur_index, And),
            '|' => {
                self.next();
                if self.expect_char(':') {
                    Token::new(Assignment, cur_index, 2)
                } else if self.expect_char('+') {
                    Token::new(AssignmentPlus, cur_index, 2)
                } else if self.expect_char('-') {
                    Token::new(AssignmentMinus, cur_index, 2)
                } else if self.expect_char('*') {
                    Token::new(AssignmentStar, cur_index, 2)
                } else if self.expect_char('/') {
                    Token::new(AssignmentDiv, cur_index, 2)
                } else if self.expect_char('|') {
                    Token::new(AssignmentOr, cur_index, 2)
                } else if self.expect_char('&') {
                    Token::new(AssignmentAnd, cur_index, 2)
                } else {
                    Token::new(Pipe, cur_index, 2)
                }
            }
            '^' => self.make_single_char_token(cur_index, Carrot),
            '=' => {
                self.next();
                if self.expect_char('=') {
                    Token::new(DoubleEqual, cur_index, 2)
                } else {
                    Token::new(Equal, cur_index, 1)
                }
            }
            '!' => {
                self.next();
                if self.expect_char('=') {
                    Token::new(NotEqual, cur_index, 2)
                } else {
                    Token::new(Bang, cur_index, 1)
                }
            }
            '<' => {
                self.next();
                if self
                    .chars
                    .next_if(|(_, next_char)| *next_char == '-')
                    .is_some()
                {
                    Token::new(LArrow, cur_index, 2)
                } else if self
                    .chars
                    .next_if(|(_, next_char)| *next_char == '=')
                    .is_some()
                {
                    Token::new(LessThanOrEqual, cur_index, 2)
                } else {
                    Token::new(LessThan, cur_index, 1)
                }
            }
            '>' => {
                self.next();
                if self
                    .chars
                    .next_if(|(_, next_char)| *next_char == '=')
                    .is_some()
                {
                    Token::new(GreaterThanOrEqual, cur_index, 2)
                } else {
                    Token::new(GreaterThan, cur_index, 1)
                }
            }
            '(' => self.make_single_char_token(cur_index, LParen),
            ')' => self.make_single_char_token(cur_index, RParen),
            '{' => self.make_single_char_token(cur_index, LBrace),
            '}' => self.make_single_char_token(cur_index, RBrace),
            '~' => self.make_single_char_token(cur_index, Tilde),
            ':' => self.make_single_char_token(cur_index, Colon),
            ',' => self.make_single_char_token(cur_index, Comma),
            ';' => self.make_single_char_token(cur_index, Semicolon),
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_ident(cur_index);
                match ident {
                    // keywords
                    "mut" => Token::new(Mut, cur_index, 3),
                    "val" => Token::new(Val, cur_index, 4),
                    "const" => Token::new(Const, cur_index, 5),
                    "func" => Token::new(Func, cur_index, 4),
                    "extern" => Token::new(Extern, cur_index, 6),
                    // builtin types
                    "Int" => Token::new(BuiltinType, cur_index, 3),
                    "Char" => Token::new(BuiltinType, cur_index, 4),
                    // "Float" => Token::new(BuiltinType, cur_char, 5),
                    s => Token::new(Identifier, cur_index, s.len()),
                }
            }
            // Add read float, hex, etc.
            '0'..='9' => {
                let size = self.read_int(cur_index);
                Token::new(IntLiteral, cur_index, size)
            }
            c => panic!("illegal token: {c}"),
        }
    }

    fn expect_char(&mut self, c: char) -> bool {
        self.chars
            .next_if(|(_, next_char)| *next_char == c)
            .is_some()
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn make_single_char_token(&mut self, position: usize, kind: TokenKind) -> Token {
        let tok = Token::new(kind, position, 1);
        self.next();
        tok
    }

    fn read_ident(&mut self, position: usize) -> &'src str {
        let mut last = position;
        while self
            .peek()
            .is_some_and(|(_, c)| c.is_ascii_alphanumeric() || c == '_')
        {
            let (l, _) = self.next().unwrap();
            last = l;
        }
        &self.content[position..last + 1]
    }

    // TODO: Add proper escaping!!
    fn read_string(&mut self, position: usize) -> &'src str {
        let mut last = position;
        while self.peek().is_some_and(|(_, c)| c != '"') {
            let (l, _) = self.next().unwrap();
            last = l;
        }
        &self.content[position..last + 1]
    }

    fn read_int(&mut self, position: usize) -> usize {
        let mut last = position;
        while self.peek().is_some_and(|(_, c)| c.is_ascii_digit()) {
            let (l, _) = self.next().unwrap();
            last = l;
        }
        let number = &self.content[position..last + 1];
        number.len()
    }

    fn read_comment(&mut self, position: usize) -> usize {
        let mut last = position;
        while self.peek().is_some_and(|(_, c)| c != '\n') {
            let (l, _) = self.next().unwrap();
            last = l;
        }
        let doc = &self.content[position..last + 1];
        doc.len()
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|(_, c)| c.is_whitespace()) {
            self.next();
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.kind == TokenKind::Eof {
            None
        } else {
            Some(tok)
        }
    }
}
