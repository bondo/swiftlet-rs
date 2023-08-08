use parser_macro::Extract;

#[test]
fn transform_root() {
    #[derive(Extract)]
    #[kind("simple_identifier")]
    pub struct SimpleIdentifier(String);

    #[derive(Extract)]
    pub enum Pattern {
        #[select("simple_identifier")]
        SimpleIdentifier(SimpleIdentifier),
    }
}
