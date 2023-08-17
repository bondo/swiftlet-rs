use parser_macro::Extract;

#[test]
fn transform_root() {
    #[derive(Extract)]
    #[kind("simple_identifier")]
    pub struct SimpleIdentifier {
        pub vals: Vec<String>,
    }
}
