use super::*;
use super::tokens::TokenKind::*;

use pretty_assertions::assert_eq;

// Helper functions

fn expect_token_kind(l: &mut Lexer, tk: TokenKind) {
    assert_eq!(l.next_token().kind, tk);
}

// Tests

#[test]
fn test_functions() {
    let content = r#"--function test
const add | func [a: Int, b: Int] : a + b;"#;
    
    let mut l = Lexer::new(content);

    expect_token_kind(&mut l, Comment);
    expect_token_kind(&mut l, Const);
    expect_token_kind(&mut l, Identifier);
    expect_token_kind(&mut l, Pipe);
    expect_token_kind(&mut l, Func);
    expect_token_kind(&mut l, LBrack);
    expect_token_kind(&mut l, Identifier);
    expect_token_kind(&mut l, Colon);
    expect_token_kind(&mut l, BuiltinType);
    expect_token_kind(&mut l, Comma);
    expect_token_kind(&mut l, Identifier);
    expect_token_kind(&mut l, Colon);
    expect_token_kind(&mut l, BuiltinType);
    expect_token_kind(&mut l, RBrack);
    expect_token_kind(&mut l, Colon);
    expect_token_kind(&mut l, Identifier);
    expect_token_kind(&mut l, Plus);
    expect_token_kind(&mut l, Identifier);
    expect_token_kind(&mut l, Semicolon);
    expect_token_kind(&mut l, Eof);
}
