// Rust
use std::fmt::Display;

// Vendor

// Starburst
use lexer::tokens::{Token, TokenKind};
use span::Span;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum ParserError {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
    UnknownToken {
        token: Token,
    },
    Eof,
}

impl ParserError {
    pub fn get_span(&self) -> Option<Span> {
        match self {
            Self::UnexpectedToken { found, .. } => Some(found.span),
            Self::UnknownToken { token } => Some(token.span),
            _ => None,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "expected {:?} found {:?} (start: {}, end: {})",
                    expected, found.kind, found.span.start, found.span.end
                )
            }
            Self::UnknownToken { token } => write!(
                f,
                "unknown token {:?} (star: {}, end: {})",
                token.kind, token.span.start, token.span.end
            ),
            Self::Eof => write!(f, "end of file"),
        }
    }
}
