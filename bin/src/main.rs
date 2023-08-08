use parser::{self, ParserError};

fn main() {
    let source = r#"
        var foo: Int = 4;
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
