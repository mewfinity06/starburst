use lexer::Lexer;
use lexer::tokens::TokenKind::*;

fn main() {
    let content = r#"--functions
const add | func [a: Int, b: Int] -> Int : {
    a + b
}

const sub | func [a, b: Int] -> Int : a - b;

val v1 |: add[a: 2, b: 8]
val v2 |: sub[3, 6]
        "#;
    let lexer = Lexer::new(content);

    println!("{}", content);

    let mut counter: usize = 0;
    for token in lexer {
        if counter % 3 == 0 {
            println!();
        }
        match token.kind {
            DocComment | Comment => continue,
            Identifier | BuiltinType | StringLiteral | CharLiteral | IntLiteral => {
                print!("'{}' ", &content[token.span.start..token.span.end])
            }
            _ => print!("{:?} ", token.kind),
        }
        counter += 1;
    }
    println!();
}
