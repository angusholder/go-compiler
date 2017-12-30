#[macro_use]
mod utils;
mod lexer;
mod ast;
mod parser;

fn main() {
    let src = r#"
package main

import "fmt"

func main() {
    fmt.Printf("hello, world\n")
}
"#;

    match parser::parse(src) {
        Ok(source_file) => {
            println!("{:#?}", source_file);
        }
        Err(e) => {
            println!("ERROR: {}", e);
        }
    }
}
