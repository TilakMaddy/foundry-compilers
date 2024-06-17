use super::{macros::node_group, misc::SourceLocation};
use crate::artifacts::serde_helpers;
use serde::{Deserialize, Serialize};

node_group! {
    YulStatement;

    YulAssignment,
    YulBlock,
    YulBreak,
    YulContinue,
    YulExpressionStatement,
    YulLeave,
    YulForLoop,
    YulFunctionDefinition,
    YulIf,
    YulSwitch,
    YulVariableDeclaration,
}

node_group! {
    YulExpression;

    YulFunctionCall,
    YulIdentifier,
    YulLiteral,
}

/// A Yul block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulBlock {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub statements: Vec<YulStatement>, // ok
}

/// A Yul assignment statement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct YulAssignment {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub value: YulExpression, // ok
    pub variable_names: Vec<YulIdentifier>, // ok
}

/// A Yul function call.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct YulFunctionCall {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub arguments: Vec<YulExpression>, // ok
    pub function_name: YulIdentifier, // ok
}

/// A Yul identifier.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulIdentifier {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub name: String, // ok
}

/// A literal Yul value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct YulLiteral {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation,
    pub hex_value: Option<String>, // ok // TODO
    pub value: Option<String>,     // ok // TODO
    pub kind: YulLiteralKind, // ok
    pub type_name: Option<String>, // notok // TODO
}

/// Yul literal value kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum YulLiteralKind {
    /// A number literal.
    Number,
    /// A string literal.
    String,
    /// A boolean literal.
    Bool,
}

/// A Yul keyword.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulKeyword { // notok
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation,
}

/// The Yul break keyword.
pub type YulBreak = YulKeyword;
/// The Yul continue keyword.
pub type YulContinue = YulKeyword;
/// The Yul leave keyword.
pub type YulLeave = YulKeyword;

/// A Yul expression statement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulExpressionStatement {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub expression: YulExpression, // ok
}

/// A Yul for loop.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulForLoop {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // ok
    pub body: YulBlock, // ok
    pub condition: YulExpression, // ok
    pub post: YulBlock, // ok
    pub pre: YulBlock, // ok
}

/// A Yul function definition.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct YulFunctionDefinition {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub body: YulBlock, // ok
    pub name: String, // ok
    #[serde(default)]
    pub parameters: Vec<YulTypedName>, // ok
    #[serde(default)]
    pub return_variables: Vec<YulTypedName>, // notok
}

/// A Yul type name.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct YulTypedName {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub name: String, // ok
    #[serde(rename = "type")]
    pub type_name: String, // ok // TODO
}

/// A Yul if statement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulIf {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub body: YulBlock, // ok
    pub condition: YulExpression, // ok
}

/// A Yul switch statement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulSwitch {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // notok
    pub cases: Vec<YulCase>, // ok
    pub expression: YulExpression, // ok
}

/// A Yul switch statement case.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulCase {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation, // ok
    pub body: YulBlock, // ok
    pub value: YulCaseValue, // okish
}

/// A Yul switch case value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum YulCaseValue {
    /// A case defined by a literal value.
    YulLiteral(YulLiteral),
    /// The default case
    // TODO: How do we make this only match "default"?
    Default(String),
}

/// A Yul variable declaration.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YulVariableDeclaration {
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation,
    pub value: Option<YulExpression>,
    pub variables: Vec<YulTypedName>,
}
