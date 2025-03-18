pub fn get_hello_world() -> String {
    String::from(
        r#"use std::io::println;

func main : {
    println("Hello world!");
}
"#,
    )
}
