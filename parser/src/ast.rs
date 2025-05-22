#![allow(unused_assignments)]
// Rust

// Vendor

use std::mem::zeroed;

// Starburst
use lexer::tokens::Token;
use lexer::tokens::TokenKind as TK;
use span::Span;

// Crate
use crate::{Parser, parser_error::ParserError};

// Macros

// TODO: Have multiple tk's in one body if they lead to the same action.
//       See parse_expr loop for example
macro_rules! expect_and_consume {
    ($parser:ident; $({$tk:expr, | $x:ident | $action:block})*) => {
        match $parser.next() {
            $( Some(val) if val.kind == $tk => {
                $x = val;
                $action
            }, )*
            Some(generic) => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![$($tk,)*],
                    found: generic,
                });
            }
            None => return Err(ParserError::Eof),
        }
    };
    ($parser:ident; $({$tk:expr, | $x:ident | $action:block})*; or | $or_x:ident | $else_branch:block) => {
        match $parser.next() {
            $( Some(val) if val.kind == $tk => {
                $x = val;
                $action
            }, )*
            Some(val) => {
                $or_x = val;
                $else_branch
            }
            None => return Err(ParserError::Eof),
        }
    };
}

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
    FunctionDecl(FunctionDecl),
}

impl Expr {
    pub fn parse_expr(parser: &mut Parser, min_bp: f32) -> Result<(Self, Span), ParserError> {
        let mut x: Token = unsafe { zeroed() };
        let mut res_span = Span::from_range(0, 0);

        let mut lhs = expect_and_consume!(parser;
            { TK::Identifier, |x| { Expr::Identifier(Identifier::Identifier(x.span)) } }
            { TK::IntLiteral, |x| { Expr::Literal(Literal::IntLiteral(x.span)) } }
        );

        loop {
            let op = match parser.peek() {
                Some(x) if matches!(x.kind, TK::Plus | TK::Minus | TK::Star | TK::Slash) => x,
                _ => break,
            };
            parser.next();
            let (left_bp, right_bp) = op.precedence();
            if left_bp < min_bp {
                break;
            }
            let (rhs, rhs_span) = Expr::parse_expr(parser, right_bp)?;
            res_span.end = rhs_span.end;
            lhs = Expr::BinaryOp(BinaryOp {
                operand: op,
                rhs: Box::new(rhs),
                lhs: Box::new(lhs),
            })
        }

        Ok((lhs, res_span))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum VariableDecl {
    Val(VariableDeclBody),
    Mut(VariableDeclBody),
    Const(VariableDeclBody),
}

impl VariableDecl {
    fn parse_variable_decl_body(
        parser: &mut Parser,
    ) -> Result<(VariableDeclBody, Span), ParserError> {
        let mut x: Token = unsafe { zeroed() };
        let mut res_span = Span::from_range(0, 0);

        // get name
        let name = expect_and_consume! {parser;
            { TK::Identifier, |x| { x.span } }
        };

        let need_expl_type = expect_and_consume! {parser;
            { TK::Assignment, |x| { false } }
            { TK::Pipe, |x| { true } }
        };

        let (expl_type, expl_type_span) = if need_expl_type {
            expect_and_consume! {parser;
                { TK::BuiltinType, |x| { (
                    Some(Box::new(Expr::Identifier(Identifier::BuilinTypeName( x.span )))),
                    Some(x.span)
                ) } }
                { TK::Identifier, |x| { (
                    Some(Box::new(Expr::Identifier(Identifier::Identifier(x.span)))),
                    Some(x.span),
                ) } }
                { TK::Func, |x| {
                    let (f_decl, f_span) = FunctionDecl::parse_function_decl(parser)?;
                    (
                        Some(Box::new(Expr::FunctionDecl(f_decl))),
                        Some(f_span)
                    )
                } }
            }
        } else {
            (None, None)
        };

        if need_expl_type {
            parser.next();
        }

        let (expr, expr_span) = Expr::parse_expr(parser, 0.0)?;

        res_span.start = name.start;
        res_span.end = expr_span.end;

        Ok((
            VariableDeclBody {
                name,
                expl_type,
                expl_type_span,
                expr: Box::new(expr),
            },
            res_span,
        ))
    }

    pub fn parse_mut(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Mut(Self::parse_variable_decl_body(parser)?.0))
    }

    pub fn parse_val(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Val(Self::parse_variable_decl_body(parser)?.0))
    }

    pub fn parse_const(parser: &mut Parser) -> Result<Self, ParserError> {
        Ok(Self::Const(Self::parse_variable_decl_body(parser)?.0))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct VariableDeclBody {
    pub name: Span,
    pub expl_type: Option<Box<Expr>>,
    pub expl_type_span: Option<Span>,
    pub expr: Box<Expr>,
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
    pub operand: Token,
    pub rhs: Box<Expr>,
    pub lhs: Box<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct UnaryOp {
    pub operand: Token,
    pub rhs: Box<Expr>,
}

// func [ arg_body ] ->return_type : { function body }
// func [ arg_body ] ->return_type :       expr
#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct FunctionDecl {
    pub arg_body: ArgBody,
    pub return_type: ReturnType,
    pub func_body: FuncBody,
}

impl FunctionDecl {
    pub fn parse_function_decl(parser: &mut Parser) -> Result<(Self, Span), ParserError> {
        let mut res_span = Span::from_range(0, 0);

        let (arg_body, arg_body_span) = ArgBody::parse_arg_body(parser)?;
        let return_type = ReturnType::parse_return_type(parser)?;
        let (func_body, func_body_span) = FuncBody::parse_func_body(parser)?;

        res_span.start = arg_body_span.start;
        res_span.end = func_body_span.end;

        Ok((
            Self {
                arg_body,
                return_type: return_type,
                func_body,
            },
            res_span,
        ))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct ArgBody {
    args: Vec<Arg>,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Arg {
    name: Span,
    arg_type: Option<Box<Expr>>,
}

impl ArgBody {
    pub fn parse_arg_body(parser: &mut Parser) -> Result<(Self, Span), ParserError> {
        let mut x: Token = unsafe { zeroed() };

        let mut res_span: Span = Span::from_range(0, 0);

        // consume LBrace
        expect_and_consume! {parser;
            { TK::LBrack, |x| { res_span.start = x.span.start + 1 } }
        };

        let mut args: Vec<Arg> = vec![];

        loop {
            let mut current_arg: Arg = unsafe { zeroed() };

            expect_and_consume!(parser;
                { TK::RBrack, |x| { break } }
                { TK::Identifier, |x| {
                    current_arg.name = x.span;

                    expect_and_consume!(parser;
                        // a, b: Int
                        { TK::Comma, |x| {
                            current_arg.arg_type = None;
                        } }
                        // a: Int, b: Int
                        { TK::Colon, |x| {

                            expect_and_consume!(parser;
                                { TK::BuiltinType, |x| {
                                    current_arg.arg_type = Some(Box::new(Expr::Identifier(Identifier::BuilinTypeName(x.span))));
                                } }
                            )
                        } }
                    );
                } }
            );

            res_span.end = current_arg.name.end;
            args.push(current_arg);
        }

        Ok((Self { args }, res_span))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct FuncBody {
    exprs: Vec<Expr>,
}

impl FuncBody {
    pub fn parse_func_body(parser: &mut Parser) -> Result<(Self, Span), ParserError> {
        let mut x: Token = unsafe { zeroed() };

        let (res, res_span) = expect_and_consume!(parser;
            { TK::LBrace, |x| {
                 todo!()
            }};
            or |x| {
                let (expr, expr_span) = Expr::parse_expr(parser, 0.0)?;
                Ok((
                    Self { exprs: vec![expr] },
                    expr_span
                ))
            }
        )?;

        Ok((res, res_span))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum ReturnType {
    ReturnType(Box<ReturnType>),
    Identifier(Identifier),
    BuiltinType(Identifier),
    Tuple(Box<ReturnType>, Box<ReturnType>),
    Expr(Box<Expr>),
}

impl ReturnType {
    pub fn parse_return_type(parser: &mut Parser) -> Result<Self, ParserError> {
        let mut x: Token = unsafe { zeroed() };

        // skip arrow
        expect_and_consume!(parser;
            { TK::RArrow, |x| {} }
        );

        expect_and_consume!(parser;
            { TK::Identifier, |x| { Ok(Self::Identifier(Identifier::Identifier(x.span))) } }
            { TK::BuiltinType, |x| { Ok(Self::BuiltinType(Identifier::BuilinTypeName(x.span))) } }
            or |x| { Ok(Self::Expr(Box::new(Expr::parse_expr(parser, 0.0)?))}
        )
    }
}
