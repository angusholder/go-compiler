extern crate num_traits;
extern crate vec_map;
extern crate smallvec;
extern crate fnv;

// I tried modifying this to have just a single String, before realising that it would leave the
// HashMap key InternalStrRef's pointer dangling when the String reallocates. The optimisation is
// still possible, it would just require recreating the HashMap upon every String reallocation,
// fixing up the pointers to their new target each time. Probably not worthwhile, but would be
// interesting to try to see if it has much effect on parse time. The other approach would involve
// making my own hash map type, where I am able to use a pointer to the `String` when doing key
// comparisons and hashes, in which case the hash map key can simply be Span. If only Rust allowed
// the key comparator to be a function object like in C++, rather than just impl of PartialEq+Hash.
extern crate string_interner;

#[macro_use]
mod utils;
mod lexer;
mod ast;
mod parser;
mod types;
mod type_check;
mod compiler;
mod vm;

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
