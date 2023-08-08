use parser::{self, ParserError};

fn main() {
    let source = r#"
        struct Vec2 { var x: Int, y: Int };
        var v = Vec2 (x: 4, y: 2);
        print(v.y) // Prints 2
    "#;

    parser::print_syntax_tree(source);

    match parser::parse(source) {
        Ok(ast) => println!("{:#?}", ast),
        Err(ParserError::Internal(e)) => eprintln!("{:#?}", e),
        Err(ParserError::Source(errors)) => {
            for error in errors {
                error.print(source);
            }
        }
    }
}
