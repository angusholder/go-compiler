#[macro_use]
mod utils;
mod lexer;
mod ast;
mod parser;

use parser::Parser;

fn main() {
    let src = r#"
package main

import "fmt"

func main() {
    fmt.Printf("hello, world\n")
}
"#;
    let mut parser = Parser::new(src);

    loop {
        match parser.parse() {
            Ok(source_file) => {
                println!("{:#?}", source_file);
            }
            Err(e) => {
                println!("ERROR: {}", e);
                break;
            }
        }
    }
}
