use std::fmt::Display;

use span::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // keywords
    Mut,    // mut
    Val,    // val
    Const,  // const
    Func,   // func
    Or,     // or
    And,    // and
    Extern, // extern

    // syntax
    Assignment,      // |:
    AssignmentPlus,  // |+
    AssignmentMinus, // |-
    AssignmentStar,  // |*
    AssignmentDiv,   // |/
    AssignmentOr,    // ||
    AssignmentAnd,   // |&
    LParen,          // (
    RParen,          // )
    LBrace,          // {
    RBrace,          // }
    Colon,           // :
    Comma,           // ,
    Tilde,           // ~
    Semicolon,       // ;
    Equal,           // =
    Pound,           // #
    LBrack,          // [
    RBrack,          // ]
    Variadic,        // ...

    // operators:
    Bang,               // !
    Plus,               // +
    Minus,              // -
    Slash,              // /
    Star,               // *
    Ampersand,          // &
    Pipe,               // |
    Carrot,             // ^
    DoubleEqual,        // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    RArrow,             // ->
    LArrow,             // <-
    Dot,                // .

    // idents
    Identifier,  // variable names
    BuiltinType, // type names

    // literals
    StringLiteral, // string literal
    CharLiteral,   // char literal
    IntLiteral,    // int literal

    // comments
    Comment,
    DocComment, // doc comments

    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TokenKind::Mut => "mut",
            TokenKind::Val => "val",
            TokenKind::Const => "const",
            TokenKind::Func => "func",
            TokenKind::Or => "or",
            TokenKind::And => "and",
            TokenKind::Extern => "extern",
            TokenKind::Assignment => "|:",
            TokenKind::AssignmentPlus => "|+",
            TokenKind::AssignmentMinus => "|-",
            TokenKind::AssignmentStar => "|*",
            TokenKind::AssignmentDiv => "|/",
            TokenKind::AssignmentOr => "||",
            TokenKind::AssignmentAnd => "|&",
            TokenKind::Variadic => "...",
            TokenKind::Dot => ".",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrack => "[",
            TokenKind::RBrack => "]",
            TokenKind::RArrow => "->",
            TokenKind::LArrow => "<-",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Tilde => "~",
            TokenKind::Semicolon => ";",
            TokenKind::Equal => "=",
            TokenKind::Bang => "!",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Slash => "/",
            TokenKind::Pound => "#",
            TokenKind::Star => "*",
            TokenKind::Ampersand => "&",
            TokenKind::Pipe => "|",
            TokenKind::Carrot => "^",
            TokenKind::DoubleEqual => "=",
            TokenKind::NotEqual => "!=",
            TokenKind::LessThan => "<",
            TokenKind::GreaterThan => ">",
            TokenKind::LessThanOrEqual => "<=",
            TokenKind::GreaterThanOrEqual => ">=",
            TokenKind::Identifier => "identifier",
            TokenKind::IntLiteral => "integer literal",
            TokenKind::StringLiteral => "string literal",
            TokenKind::CharLiteral => "char literal",
            TokenKind::BuiltinType => "builtin type",
            TokenKind::Comment => "comment",
            TokenKind::DocComment => "doc comment",
            TokenKind::Eof => "EOF",
        };
        f.write_str(str)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, size: usize) -> Self {
        Token {
            kind,
            span: Span::from_range(start, start + size),
        }
    }

    pub fn precedence(&self) -> Precedence {
        use TokenKind::*;
        match self.kind {
            Plus | Minus => (1.0, 1.1),
            Star | Slash => (2.0, 2.1),
            Carrot => (3.1, 3.0),
            Dot => (4.0, 4.1),
            _ => (0.0, 0.0),
        }
    }
}

type Precedence = (f32, f32);
