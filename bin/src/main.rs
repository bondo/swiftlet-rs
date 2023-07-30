use parser::parse;

fn main() {
    let source = r#"
        var foo: Int = 4;
        let bar = foo;
        print(bar) // Prints 4
    "#;

    match parse(source) {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => println!("{}", e),
    }
}
