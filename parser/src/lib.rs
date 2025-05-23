// Top level file attribues
#![allow(dead_code)]

// Rust
use std::{iter::Peekable, vec::IntoIter};

// Vendor

// Starburst
use ast::*;
use lexer::{
    Lexer,
    tokens::{Token, TokenKind},
};
use parser_error::*;

// Mod
mod ast;
mod parser_error;

#[cfg(test)]
mod tests;

pub struct Parser<'p> {
    content: &'p str,
    tokens: Peekable<IntoIter<Token>>,
}

impl<'p> Parser<'p> {
    pub fn new(lexer: Lexer<'p>) -> Self {
        Self {
            content: lexer.content,
            tokens: lexer.collect::<Vec<_>>().into_iter().peekable(),
        }
    }

    fn next_node(&mut self) -> Result<Node, ParserError> {
        let Some(cur_token) = self.next() else {
            return Err(ParserError::Eof);
        };

        match cur_token.kind {
            TokenKind::Comment | TokenKind::DocComment => Ok(Node::Comment),
            // Variable decls
            TokenKind::Mut => Ok(Node::Expr(Expr::VariableDecl(
                VariableDecl::parse_mut(self)?.0,
            ))),
            TokenKind::Val => Ok(Node::Expr(Expr::VariableDecl(
                VariableDecl::parse_val(self)?.0,
            ))),
            TokenKind::Const => Ok(Node::Expr(Expr::VariableDecl(
                VariableDecl::parse_const(self)?.0,
            ))),
            _ => Err(ParserError::UnknownToken { token: cur_token }),
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect_token(&mut self, tk: TokenKind) -> bool {
        self.peek().is_some_and(|t| t.kind == tk)
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().copied()
    }
}

impl<'p> Iterator for Parser<'p> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_node() {
            Ok(x) => Some(x),
            Err(x) => {
                if x != ParserError::Eof {
                    eprintln!("Error: {}", x);
                    if let Some(span) = x.get_span() {
                        eprintln!("     | `{}`", &self.content[span.start..span.end])
                    }
                }
                None
            }
        }
    }
}
