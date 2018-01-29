#![allow(unused)]

extern crate num_traits;
extern crate vec_map;
extern crate smallvec;
extern crate fnv;
extern crate backtrace;

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
mod token;
mod lexer;
mod ast;
mod parser;
mod types;
mod scope;
mod compiler;
mod vm;

fn main() {
    let src = r#"
package main

func main() {
    a := 4
    b := 7
    c := 1

    root1 := (0-b) + (b*b - 4*a*c) / (2*a)
//    root2 := (0-b) - (b*b - 4*a*c) / (2*a)
}
"#;
    let res = parser::parse(src).and_then(|source_file| {
        compiler::Compiler::new().compile(&source_file)
    });

    match res {
        Ok(func) => {
            for opcode in func.code.into_vec() {
                println!("{:#?}", opcode);
            }
        }
        Err(e) => {
            println!("{}", e.fmt(src));
        }
    }
}
