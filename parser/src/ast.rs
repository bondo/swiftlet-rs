#[derive(Debug, PartialEq)]
pub struct Program {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    CallExpression(CallExpression),
    NavigationExpression(NavigationExpression),
    PropertyDeclaration(PropertyDeclaration),
    SimpleIdentifier(SimpleIdentifier),
    IntegerLiteral(i32),
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub callee: SimpleIdentifier,
    pub arguments: CallSuffix,
}

#[derive(Debug, PartialEq)]
pub struct NavigationExpression {
    pub target: SimpleIdentifier,
    pub suffix: NavigationSuffix,
}

#[derive(Debug, PartialEq)]
pub struct NavigationSuffix(pub(super) SimpleIdentifier);

#[derive(Debug, PartialEq)]
pub enum CallSuffix {
    ValueArguments(ValueArguments),
}

#[derive(Debug, PartialEq)]
pub struct ValueArguments(pub(super) Vec<ValueArgument>);

#[derive(Debug, PartialEq)]
pub enum ValueArgument {
    Value(Box<Expr>),
    NamedValue(SimpleIdentifier, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Qualifier {
    Var,
    Let,
}

#[derive(Debug, PartialEq)]
pub struct SimpleIdentifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    SimpleIdentifier(SimpleIdentifier),
}

#[derive(Debug, PartialEq)]
pub struct UserType {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub enum TypeIdentifier {
    UserType(UserType),
}

#[derive(Debug, PartialEq)]
pub struct PropertyDeclaration {
    pub qualifier: Qualifier,
    pub lhs: Pattern,
    pub r#type: Option<TypeIdentifier>,
    pub rhs: Box<Expr>,
}
