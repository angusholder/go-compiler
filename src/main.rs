mod chars;
#[macro_use]
mod result;
mod ptr;
mod lexer;
mod ast;

use lexer::Lexer;

fn main() {
    let src = r#"
package main

import "fmt"

func main() {
    fmt.Printf("hello, world\n")
}
"#;
    let mut lexer = Lexer::new(src);

    loop {
        match lexer.next() {
            Ok(Some(token)) => {
                println!("{}", token.kind.fmt(src));
            }
            Ok(None) => break,
            Err(e) => {
                println!("ERROR: {}", e);
                break;
            }
        }
    }
}
