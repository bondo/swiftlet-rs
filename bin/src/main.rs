use std::process::exit;

use ast::SwiftletErrorPrinter;
use interpreter::run;
use parser::parse;

fn main() {
    let source = r#"
        struct Vec2 { var x: Int, y: Int, z: Int };
        var v = Vec2 (x: 4, y: 2, z: 1);
        print(v.y); // Prints 2
    "#;
    let mut printer = SwiftletErrorPrinter::new(source);

    let ast = parse(source).unwrap_or_else(|e| {
        printer.eprint(&e);
        exit(1)
    });

    run(ast).unwrap_or_else(|e| {
        printer.eprint(&e);
        exit(1)
    });
}
