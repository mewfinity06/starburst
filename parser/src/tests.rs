use pretty_assertions::assert_eq;
use span::Span;

use super::*;

use lexer::Lexer;

#[test]
fn test_variable_parsing() -> Result<(), ParserError> {
    let content = r#"val foo |: 4 + 1 * 7"#;

    let lex = Lexer::new(&content);
    let mut parser = Parser::new(lex);

    assert_eq!(
        parser.next_node()?,
        Node::Expr(Expr::VariableDecl(VariableDecl::Val(VariableDeclBody {
            name: Span { start: 4, end: 7 },
            expl_type: None,
            expl_type_span: None,
            expr: Box::new(Expr::BinaryOp(BinaryOp {
                operand: Token {
                    kind: TokenKind::Plus,
                    span: Span { start: 13, end: 14 }
                },
                rhs: Box::new(Expr::BinaryOp(BinaryOp {
                    operand: Token {
                        kind: TokenKind::Star,
                        span: Span { start: 17, end: 18 }
                    },
                    rhs: Box::new(Expr::Literal(Literal::IntLiteral(Span {
                        start: 19,
                        end: 20,
                    }))),
                    lhs: Box::new(Expr::Literal(Literal::IntLiteral(Span {
                        start: 15,
                        end: 16,
                    })))
                })),
                lhs: Box::new(Expr::Literal(Literal::IntLiteral(Span {
                    start: 11,
                    end: 12,
                })))
            }))
        })))
    );

    Ok(())
}
