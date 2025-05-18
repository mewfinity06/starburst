use lexer::Lexer;

fn main() {
    let content = r#"--Comment test
mut foo |: 4 --after line comment test
val bar |: 8
const BAZ | Int : foo + bar

-#Doc comment test!
        "#;
    let lexer = Lexer::new(content);

    for token in lexer {
        println!(
            "{:?} -> {}",
            token.kind,
            &content[token.span.start..token.span.end]
        );
    }
}
