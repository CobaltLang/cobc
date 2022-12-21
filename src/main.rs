pub mod parser;

#[macro_use]
extern crate pest_derive;

use std::fs;

fn main() {
    let source = fs::read_to_string("../examples/math.cb").expect("failed to read source");

    let output = parser::parse(&source);

    println!("{:#?}", output);
}
