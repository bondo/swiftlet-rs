use parser_macro::Extract;

#[derive(Debug, PartialEq, Extract)]
#[kind("source_file")]
pub struct Program {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Extract)]
pub enum Expr {
    #[select("call_expression")]
    CallExpression(CallExpression),

    #[select("navigation_expression")]
    NavigationExpression(NavigationExpression),

    #[select("property_declaration")]
    PropertyDeclaration(PropertyDeclaration),

    #[select("simple_identifier")]
    SimpleIdentifier(SimpleIdentifier),

    #[select("integer_literal")]
    IntegerLiteral(i32),
}

#[derive(Debug, PartialEq, Extract)]
#[kind("call_expression")]
pub struct CallExpression {
    pub callee: SimpleIdentifier,

    pub arguments: CallSuffix,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("navigation_expression")]
pub struct NavigationExpression {
    pub target: SimpleIdentifier,

    pub suffix: NavigationSuffix,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("navigation_suffix")]
pub struct NavigationSuffix(pub(super) SimpleIdentifier);

#[derive(Debug, PartialEq, Extract)]
pub enum CallSuffix {
    #[select("value_arguments")]
    ValueArguments(ValueArguments),
}

#[derive(Debug, PartialEq, Extract)]
#[kind("value_arguments")]
pub struct ValueArguments(pub(super) Vec<ValueArgument>);

#[derive(Debug, PartialEq, Extract)]
#[kind("value_argument")]
pub struct ValueArgument {
    pub label: Option<SimpleIdentifier>,

    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Extract)]
pub enum Qualifier {
    #[select("var")]
    Var,
    #[select("let")]
    Let,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("simple_identifier")]
pub struct SimpleIdentifier(pub(super) String);

#[derive(Debug, PartialEq, Extract)]
#[kind("pattern")]
pub struct Pattern {
    pub identifier: SimpleIdentifier,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("user_type")]
pub struct UserType {
    #[select("type_identifier")]
    pub name: String,
}

#[derive(Debug, PartialEq, Extract)]
pub enum TypeIdentifier {
    #[select("user_type")]
    UserType(UserType),
}

#[derive(Debug, PartialEq, Extract)]
#[kind("type_annotation")]
pub struct TypeAnnotation {
    pub colon: ColonToken,

    pub r#type: TypeIdentifier,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("property_declaration")]
pub struct PropertyDeclaration {
    pub qualifier: Qualifier,

    pub lhs: Pattern,

    pub r#type: Option<TypeAnnotation>,

    pub eq: EqToken,

    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Extract)]
#[kind("=")]
pub struct EqToken;

#[derive(Debug, PartialEq, Extract)]
#[kind(":")]
pub struct ColonToken;
