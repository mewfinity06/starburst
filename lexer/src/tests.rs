use super::tokens::TokenKind::*;
use super::*;

use pretty_assertions::assert_eq;

// Helper functions

fn expect_tk(l: &mut Lexer, tk: TokenKind) {
    assert_eq!(l.next_token().kind, tk);
}

// Tests
#[test]
fn test_variable_decl() {
    let content = r#"
-- Mutable variable
mut foo |: 4
-- Immutable variable
val bar |: 5
-- Constant with type annotation
const FOO_BAR | Int : foo + bar 
    "#;
    let mut l = Lexer::new(content);

    expect_tk(&mut l, Comment);
    expect_tk(&mut l, Mut);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Assignment);
    expect_tk(&mut l, IntLiteral);
    expect_tk(&mut l, Comment);
    expect_tk(&mut l, Val);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Assignment);
    expect_tk(&mut l, IntLiteral);
    expect_tk(&mut l, Comment);
    expect_tk(&mut l, Const);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Pipe);
    expect_tk(&mut l, BuiltinType);
    expect_tk(&mut l, Colon);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Plus);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Eof);
}

#[test]
fn test_functions() {
    let content = r#"--function test
const add | func [a: Int, b: Int] : a + b"#;
    let mut l = Lexer::new(content);

    expect_tk(&mut l, Comment);
    expect_tk(&mut l, Const);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Pipe);
    expect_tk(&mut l, Func);
    expect_tk(&mut l, LBrack);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Colon);
    expect_tk(&mut l, BuiltinType);
    expect_tk(&mut l, Comma);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Colon);
    expect_tk(&mut l, BuiltinType);
    expect_tk(&mut l, RBrack);
    expect_tk(&mut l, Colon);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Plus);
    expect_tk(&mut l, Identifier);
    expect_tk(&mut l, Eof);
}
