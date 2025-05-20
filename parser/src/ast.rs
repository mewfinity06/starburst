// Rust

// Vendor

// Starburst
use span::Span;
use lexer::tokens::Token;
use lexer::tokens::TokenKind as TK;

// Crate
use crate::{Parser, parser_error::ParserError};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum Node {
    Expr(Expr),
    Comment,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expr {
    Expr(Box<Expr>),
    VariableDecl(VariableDecl),
    Identifier(Identifier),
    Literal(Literal),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    FunctionDecl,
}

impl Expr {
    // TODO: Have actual precedence, this is VERY naive
    pub fn parse_expr(parser: &mut Parser, min_bp: f32) -> Result<Self, ParserError> {
        let mut lhs = match parser.next() {
            Some(x) if matches!(x.kind, TK::Identifier) => {
                Expr::Identifier(Identifier::Identifier(x.span))
            }
            Some(x) if matches!(x.kind, TK::IntLiteral) => {
                Expr::Literal(Literal::IntLiteral(x.span))
            }
            Some(x) => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![TK::Identifier, TK::Identifier],
                    found: x,
                });
            }
            None => return Err(ParserError::Eof),
        };

        loop {
            let op = match parser.peek() {
                Some(x) if matches!(x.kind, TK::Plus | TK::Minus | TK::Star | TK::Slash) => x,
                // Some(x) => {
                //     return Err(ParserError::UnexpectedToken {
                //         expected: vec![TK::Plus, TK::Minus, TK::Star, TK::Slash],
                //         found: x,
                //     });
                // }
                _ => break,
            };
            parser.next();
            let (left_bp, right_bp) = op.precedence();
            if left_bp < min_bp {
                break;
            }
            let rhs = Expr::parse_expr(parser, right_bp)?;
            lhs = Expr::BinaryOp(BinaryOp {
                operand: op,
                rhs: Box::new(rhs),
                lhs: Box::new(lhs),
            })
        }

        Ok(lhs)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum VariableDecl {
    Val(VariableDeclBody),
    Mut(VariableDeclBody),
    Const(VariableDeclBody),
}

impl VariableDecl {
    fn parse_variable_decl_body(parser: &mut Parser) -> Result<VariableDeclBody, ParserError> {
        // get name
        let name = match parser.next() {
            Some(x) if x.kind == TK::Identifier => x.span,
            Some(x) => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![TK::Identifier],
                    found: x,
                });
            }
            None => return Err(ParserError::Eof),
        };

        let need_expl_type = match parser.next() {
            Some(x) if x.kind == TK::Assignment => false,
            Some(x) if x.kind == TK::Pipe => true,
            Some(x) => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![TK::Assignment, TK::Pipe],
                    found: x,
                });
            }
            None => return Err(ParserError::Eof),
        };

        let (expl_type, expl_type_span) = if need_expl_type {
            match parser.next() {
                Some(x) if x.kind == TK::BuiltinType => (
                    Some(Box::new(Expr::Identifier(Identifier::BuilinTypeName(
                        x.span,
                    )))),
                    Some(x.span),
                ),
                Some(x) if x.kind == TK::Identifier => (
                    Some(Box::new(Expr::Identifier(Identifier::Identifier(x.span)))),
                    Some(x.span),
                ),
                Some(x) => {
                    return Err(ParserError::UnexpectedToken {
                        expected: vec![TK::BuiltinType, TK::Identifier],
                        found: x,
                    });
                }
                None => return Err(ParserError::Eof),
            }
        } else {
            (None, None)
        };

        if need_expl_type {
            parser.next();
        }

        let expr = Box::new(Expr::parse_expr(parser, 0.)?);

        Ok(VariableDeclBody {
            name,
            expl_type,
            expl_type_span,
            expr,
        })
    }

    pub fn parse_mut(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Mut(Self::parse_variable_decl_body(parser)?))
    }

    pub fn parse_val(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Val(Self::parse_variable_decl_body(parser)?))
    }

    pub fn parse_const(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Const(Self::parse_variable_decl_body(parser)?))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct VariableDeclBody {
    name: Span,
    expl_type: Option<Box<Expr>>,
    expl_type_span: Option<Span>,
    expr: Box<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum Identifier {
    Identifier(Span),
    VariableName(Span),
    BuilinTypeName(Span),
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum Literal {
    IntLiteral(Span),
    StringLiteral(Span),
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct BinaryOp {
    operand: Token,
    rhs: Box<Expr>,
    lhs: Box<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct UnaryOp {
    operand: Token,
    rhs: Box<Expr>,
}
