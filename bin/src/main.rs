// use parser::{self, ParserError};
use grammar::parse;
use interpreter::run;

fn main() {
    let source = r#"
        var a = 1;
        if (a < 10) {
            var a = 5;
            print(a);
            a = a + 1;
        }
        print(a);
    "#;

    let ast = parse(source).unwrap_or_else(|e| panic!("\n{e}\n"));
    println!("{:#?}\n\n", ast);

    run(ast);
}
