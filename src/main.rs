extern crate num_traits;

#[macro_use]
mod utils;
mod lexer;
mod ast;
mod parser;
mod type_check;
mod compiler;

fn main() {
    let src = r#"
package main

import "fmt"
import ("hello"; "goodbye";)

func main() {
    running := true

    for {}
    for running {}
    for a := 1; a < 10; a++ {}
    for a, b := 1, 1; a < 100; a, b = a + b, a {}
    for i, ch := range "hello world" {}

    fmt.Printf("hello, world\n")
}
"#;

    match parser::parse(src) {
        Ok(source_file) => {
            println!("{:#?}", source_file);
        }
        Err(e) => {
            println!("{}", e.fmt(src));
        }
    }
}
