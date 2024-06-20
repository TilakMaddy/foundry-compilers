//! Bindings for the Solidity and Yul ASTs.
//!
//! The Yul AST bindings are available in the [yul] module.
//!
//! To gain an overview of the AST, it might be helpful to start at the entry point of a complete
//! Solidity AST: the [SourceUnit] node.
//!
//! # Version Support
//!
//! These types should be compatible with at least Solidity 0.5.x and above, but may also support
//! 0.4.x-0.5.x in most cases.
//!
//! The legacy Solidity AST is not supported.

mod macros;
mod misc;
pub use misc::*;
pub mod util;
pub mod visitor;

/// A low fidelity representation of the AST.
pub(crate) mod lowfidelity;
pub use lowfidelity::{Ast, Node, NodeType, SourceLocation as LowFidelitySourceLocation};

/// Types for the Yul AST.
///
/// The Yul AST is embedded into the Solidity AST for inline assembly blocks.
pub mod yul;

use crate::artifacts::serde_helpers;
use macros::{ast_node, expr_node, node_group, stmt_node};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use yul::YulBlock;

ast_node!(
    /// The root node of a Solidity AST.
    struct SourceUnit {
        #[serde(rename = "absolutePath")]
        absolute_path: String,
        #[serde(default, rename = "exportedSymbols")]
        exported_symbols: BTreeMap<String, Vec<usize>>,
        #[serde(default)]
        license: Option<String>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        nodes: Vec<SourceUnitPart>,
    }
);

node_group! {
    SourceUnitPart;

    // ok - inspection only at the top level 
    // TODO: later dive deep into AST Node

    PragmaDirective, // ok
    ImportDirective, // ok
    UsingForDirective, // ok
    VariableDeclaration, // ok
    EnumDefinition, // ok
    ErrorDefinition, // ok
    FunctionDefinition, // ok
    StructDefinition, // ok
    UserDefinedValueTypeDefinition, // ok
    ContractDefinition, // ok
}

node_group! {         // ok
    Expression;

    Assignment,
    BinaryOperation,
    Conditional,
    ElementaryTypeNameExpression,
    FunctionCall,
    FunctionCallOptions,
    Identifier,
    IndexAccess,
    IndexRangeAccess,
    Literal,
    MemberAccess,
    NewExpression,
    TupleExpression,
    UnaryOperation,
}

node_group! {
    Statement;

    Block, // not@ok
    Break, // not@ok
    Continue, // not@ok
    DoWhileStatement, // not@ok
    EmitStatement, // ok
    ExpressionStatement, // ok
    ForStatement, // ok
    IfStatement,// ok
    InlineAssembly, // ok
    PlaceholderStatement, // not@ok
    Return, // ok
    RevertStatement, // ok
    TryStatement, // ok
    UncheckedBlock, // ok
    VariableDeclarationStatement, // ok
    WhileStatement, // ok

}

node_group! {
    ContractDefinitionPart;

    EnumDefinition, // ok
    ErrorDefinition, // ok
    EventDefinition, // ok
    FunctionDefinition, // ok
    ModifierDefinition, // ok
    StructDefinition, // ok
    UserDefinedValueTypeDefinition, // ok
    UsingForDirective, // ok
    VariableDeclaration, // ok
}

node_group! {
    TypeName; // not@ok (i think because of String(String))

    ArrayTypeName,
    ElementaryTypeName,
    FunctionTypeName,
    Mapping,
    UserDefinedTypeName,
}

// TODO: Better name
node_group! {
    UserDefinedTypeNameOrIdentifierPath;

    UserDefinedTypeName,
    IdentifierPath,
}

// TODO: Better name
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum BlockOrStatement {
    Statement(Statement),
    Block(Block),
}

// TODO: Better name
node_group! {
    ExpressionOrVariableDeclarationStatement;

    ExpressionStatement,
    VariableDeclarationStatement
}

// TODO: Better name
node_group! {
    IdentifierOrIdentifierPath;

    Identifier,
    IdentifierPath
}

ast_node!(
    /// A contract definition.
    struct ContractDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        #[serde(default, rename = "abstract")]
        is_abstract: bool, // ok
        base_contracts: Vec<InheritanceSpecifier>, // ok
        canonical_name: Option<String>, // not@ok
        contract_dependencies: Vec<usize>, // ok
        #[serde(rename = "contractKind")]
        kind: ContractKind, // ok
        documentation: Option<StructuredDocumentation>, // ok
        fully_implemented: bool, // ok
        linearized_base_contracts: Vec<usize>, // ok
        nodes: Vec<ContractDefinitionPart>, // ok
        scope: usize, // ok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        used_errors: Vec<usize>, // ok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        used_events: Vec<usize>, // not@ok
        #[serde(default, rename = "internalFunctionIDs")]
        internal_function_ids: BTreeMap<String, usize>, // not@ok
    }
);

/// All Solidity contract kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ContractKind {
    /// A normal contract.
    Contract, // ok
    /// An interface.
    Interface, // ok
    /// A library.
    Library, // ok
}

ast_node!(
    /// An inheritance specifier.
    struct InheritanceSpecifier {
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        arguments: Vec<Expression>, // ok
        base_name: UserDefinedTypeNameOrIdentifierPath, // not@ok
    }
);

expr_node!(
    /// An assignment expression.
    struct Assignment {
        #[serde(rename = "leftHandSide")] 
        lhs: Expression, // ok
        operator: AssignmentOperator, // ok
        #[serde(rename = "rightHandSide")]
        rhs: Expression, // ok
    }
);

/// Assignment operators.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssignmentOperator {
    /// Simple assignment (`=`)
    #[serde(rename = "=")]
    Assign,
    /// Add and assign (`+=`)
    #[serde(rename = "+=")]
    AddAssign,
    /// Subtract and assign (`-=`)
    #[serde(rename = "-=")]
    SubAssign,
    /// Multiply and assign (`*=`)
    #[serde(rename = "*=")]
    MulAssign,
    /// Divide and assign (`/=`)
    #[serde(rename = "/=")]
    DivAssign,
    /// Modulo and assign (`%=`)
    #[serde(rename = "%=")]
    ModAssign,
    /// Bitwise or and assign (`|=`)
    #[serde(rename = "|=")]
    OrAssign,
    /// Bitwise and and assign (`&=`)
    #[serde(rename = "&=")]
    AndAssign,
    /// Bitwise xor and assign (`^=`)
    #[serde(rename = "^=")]
    XorAssign,
    /// Right shift and assign (`>>=`)
    #[serde(rename = ">>=")]
    ShrAssign,
    /// Left shift and assign (`<<=`)
    #[serde(rename = "<<=")]
    ShlAssign,
}

ast_node!(
    /// A binary operation.
    struct BinaryOperation {
        common_type: TypeDescriptions, // ok
        #[serde(rename = "leftExpression")]
        lhs: Expression, // ok
        operator: BinaryOperator, // okish
        #[serde(rename = "rightExpression")] 
        rhs: Expression, // ok
    }
);

/// Binary operators.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOperator {
    /// Addition (`+`)
    #[serde(rename = "+")]
    Add,
    /// Subtraction (`-`)
    #[serde(rename = "-")]
    Sub,
    /// Multiplication (`*`)
    #[serde(rename = "*")]
    Mul,
    /// Division (`/`)
    #[serde(rename = "/")]
    Div,
    /// Modulo (`%`)
    #[serde(rename = "%")]
    Mod,
    /// Exponentiation (`**`)
    #[serde(rename = "**")]
    Pow,
    /// Logical and (`&&`)
    #[serde(rename = "&&")]
    And,
    /// Logical or (`||`)
    #[serde(rename = "||")]
    Or,
    /// Not equals (`!=`)
    #[serde(rename = "!=")]
    NotEqual,
    /// Equals (`==`)
    #[serde(rename = "==")]
    Equal,
    /// Less than (`<`)
    #[serde(rename = "<")]
    LessThan,
    /// Less than or equal (`<=`)
    #[serde(rename = "<=")]
    LessThanOrEqual,
    /// Greater than (`>`)
    #[serde(rename = ">")]
    GreaterThan,
    /// Greater than or equal (`>=`)
    #[serde(rename = ">=")]
    GreaterThanOrEqual,
    /// Bitwise xor (`^`)
    #[serde(rename = "^")]
    Xor,
    /// Bitwise not (`~`)
    #[serde(rename = "~")]
    BitNot,
    /// Bitwise and (`&`)
    #[serde(rename = "&")]
    BitAnd,
    /// Bitwise or (`|`)
    #[serde(rename = "|")]
    BitOr,
    /// Shift left (`<<`)
    #[serde(rename = "<<")]
    Shl,
    /// Shift right (`>>`)
    #[serde(rename = ">>")]
    Shr,
}

expr_node!(
    /// A conditional expression.
    struct Conditional {
        /// The condition.
        condition: Expression, // ok
        /// The expression to evaluate if falsy.
        false_expression: Expression, // ok
        /// The expression to evaluate if truthy.
        true_expression: Expression, // ok
    }
);

expr_node!(
    struct ElementaryTypeNameExpression {
        type_name: ElementaryOrRawTypeName, // okish
    }
);

// TODO: Better name
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ElementaryOrRawTypeName { // okish
    /// An [ElementaryTypeName] node that describes the type.
    ///
    /// This variant applies to newer compiler versions.
    ElementaryTypeName(ElementaryTypeName),
    /// A string representing the type name.
    ///
    /// This variant applies to older compiler versions.
    Raw(String),
}

ast_node!(
    struct ElementaryTypeName {
        type_descriptions: TypeDescriptions, // ok
        name: String, // ok
        state_mutability: Option<StateMutability>, // ok
    }
);

expr_node!(
    /// A function call expression.
    struct FunctionCall { // ok
        arguments: Vec<Expression>, // ok
        expression: Expression, // ok
        kind: FunctionCallKind, // ok
        names: Vec<String>, // ok
        #[serde(default)]
        try_call: bool, // ok
    }
);

/// Function call kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum FunctionCallKind {
    /// A regular function call.
    FunctionCall, // ok
    /// A type conversion (e.g. `bytes(x)`).
    TypeConversion, // ok
    /// A struct constructor call (e.g. `MyStruct({ ... })`).
    StructConstructorCall, // ok
}

expr_node!(
    /// A function call options expression (e.g. `x.f{gas: 1}`).
    struct FunctionCallOptions { // ok
        expression: Expression, // ok 
        names: Vec<String>, // ok
        options: Vec<Expression>, // ok
    }
);

ast_node!(
    /// An identifier.
    struct Identifier {
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        argument_types: Vec<TypeDescriptions>, // ok
        name: String, // ok
        overloaded_declarations: Vec<isize>, // ok
        referenced_declaration: Option<isize>, // ok
        type_descriptions: TypeDescriptions, // ok
    }
);

expr_node!(
    /// An index access.
    struct IndexAccess {
        base_expression: Expression, // okish
        index_expression: Option<Expression>, // not@ok
    }
);

expr_node!(
    /// An index range access.
    struct IndexRangeAccess {
        base_expression: Expression, // ok
        start_expression: Option<Expression>, // ok
        end_expression: Option<Expression>, // ok
    }
);

expr_node!( // ok
    /// A literal value.
    struct Literal {
        // TODO
        hex_value: String, // okish
        kind: LiteralKind, // ok
        subdenomination: Option<String>, // ok // TODO
        value: Option<String>,     // ok      // TODO
    }
);

/// Literal kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum LiteralKind {
    /// A boolean.
    Bool,
    /// A number.
    Number,
    /// A string.
    String,
    /// A hexadecimal string.
    HexString,
    /// A unicode string.
    UnicodeString, // not@ok
}

expr_node!(
    /// Member access.
    struct MemberAccess {
        expression: Expression, // ok
        member_name: String, // ok
        referenced_declaration: Option<isize>, // ok
    }
);

expr_node!(
    /// A `new` expression.
    struct NewExpression {
        type_name: TypeName, // ok
    }
);

ast_node!(
    /// An array type name.
    struct ArrayTypeName {
        type_descriptions: TypeDescriptions, // ok
        base_type: TypeName, // ok
        length: Option<Expression>, // ok
    }
);

ast_node!(
    /// A function type name.
    struct FunctionTypeName {
        type_descriptions: TypeDescriptions, // ok
        parameter_types: ParameterList, // ok
        return_parameter_types: ParameterList, // ok
        state_mutability: StateMutability, // ok
        visibility: Visibility, // ok
    }
);

ast_node!(
    /// A parameter list.
    struct ParameterList {
        parameters: Vec<VariableDeclaration>, // ok
    }
);

ast_node!(
    /// A variable declaration.
    struct VariableDeclaration {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        base_functions: Vec<usize>, // ok
        /// Marks whether or not the variable is a constant before Solidity 0.7.x.
        ///
        /// After 0.7.x you must use `mutability`. For cross-version compatibility use
        /// [`VariableDeclaration::mutability()`].
        #[serde(default)]
        constant: bool, // ok
        /// Marks whether or not the variable is a state variable before Solidity 0.7.x.
        ///
        /// After 0.7.x you must use `mutability`. For cross-version compatibility use
        /// [`VariableDeclaration::mutability()`].
        #[serde(default)]
        state_variable: bool, // ok
        documentation: Option<StructuredDocumentation>,
        function_selector: Option<String>, // TODO
        #[serde(default)]
        indexed: bool, // ok
        /// Marks the variable's mutability from Solidity 0.7.x onwards.
        /// For cross-version compatibility use [`VariableDeclaration::mutability()`].
        #[serde(default)]
        mutability: Option<Mutability>, // ok
        overrides: Option<OverrideSpecifier>, // ok
        scope: usize, // ok
        storage_location: StorageLocation, // ok
        type_descriptions: TypeDescriptions, // ok
        type_name: Option<TypeName>, // ok
        value: Option<Expression>, // ok
        visibility: Visibility, // ok
    }
);

impl VariableDeclaration { // not@ok
    /// Returns the mutability of the variable that was declared.
    ///
    /// This is a helper to check variable mutability across Solidity versions.
    pub fn mutability(&self) -> &Mutability {
        if let Some(mutability) = &self.mutability {
            mutability
        } else if self.constant {
            &Mutability::Constant
        } else if self.state_variable {
            &Mutability::Mutable
        } else {
            unreachable!()
        }
    }
}

ast_node!(
    /// Structured documentation (NatSpec).
    struct StructuredDocumentation {
        text: String,
    }
);

ast_node!(
    /// An override specifier.
    struct OverrideSpecifier {
        overrides: Vec<UserDefinedTypeNameOrIdentifierPath>, // not@ok
    }
);

ast_node!(
    /// A user defined type name.
    struct UserDefinedTypeName {
        type_descriptions: TypeDescriptions, // ok
        contract_scope: Option<String>, // not@ok // TODO
        name: Option<String>, // ok
        path_node: Option<IdentifierPath>, // ok
        referenced_declaration: isize, // ok
    }
);

ast_node!(
    /// An identifier path.
    struct IdentifierPath {
        name: String, // ok
        referenced_declaration: isize, // ok
    }
);

ast_node!(
    /// A mapping type.
    struct Mapping {
        type_descriptions: TypeDescriptions, // ok
        key_type: TypeName, // ok
        value_type: TypeName, // ok
    }
);

expr_node!(
    /// A tuple expression.
    struct TupleExpression {
        components: Vec<Option<Expression>>, // ok
        is_inline_array: bool, // ok
    }
);

expr_node!(
    /// A unary operation.
    struct UnaryOperation {
        operator: UnaryOperator, // okish
        /// Whether the unary operator is before or after the expression (e.g. `x++` vs. `++x`)
        prefix: bool, // ok
        sub_expression: Expression, // ok
    }
);

/// Unary operators.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOperator {
    /// Increment (`++`)
    #[serde(rename = "++")]
    Increment,
    /// Decrement (`--`)
    #[serde(rename = "--")]
    Decrement,
    /// Negate (`-`)
    #[serde(rename = "-")]
    Negate,
    /// Not (`!`)
    #[serde(rename = "!")]
    Not,
    /// Bitwise not (`~`)
    #[serde(rename = "~")]
    BitNot,
    /// `delete`
    #[serde(rename = "delete")]
    Delete,
}

ast_node!(
    /// An enum definition.
    struct EnumDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        canonical_name: String, // ok
        members: Vec<EnumValue>, // ok
    }
);

ast_node!(
    /// An enum value.
    struct EnumValue {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
    }
);

ast_node!(
    /// A custom error definition.
    struct ErrorDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        documentation: Option<StructuredDocumentation>, // okish
        error_selector: Option<String>, // not@ok // TODO
        parameters: ParameterList, // ok
    }
);

ast_node!(
    /// An event definition.
    struct EventDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        anonymous: bool, // ok
        event_selector: Option<String>, // not@ok // TODO
        documentation: Option<StructuredDocumentation>, // okish
        parameters: ParameterList, // ok
    }
);

ast_node!(
    /// A function definition.
    struct FunctionDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        base_functions: Vec<usize>, // ok
        body: Option<Block>, // ok
        documentation: Option<StructuredDocumentation>, // ok
        function_selector: Option<String>, // ok // TODO
        implemented: bool, // ok
        modifiers: Vec<ModifierInvocation>, // ok
        overrides: Option<OverrideSpecifier>, // ok
        parameters: ParameterList, // ok
        return_parameters: ParameterList, // ok
        scope: usize, // ok
        visibility: Visibility, // ok
        /// The kind of function this node defines. Only valid for Solidity versions 0.5.x and
        /// above.
        ///
        /// For cross-version compatibility use [`FunctionDefinition::kind()`].
        kind: Option<FunctionKind>, // not@ok
        /// The state mutability of the function.
        ///
        /// Note: This was introduced in Solidity 0.5.x. For cross-version compatibility use
        /// [`FunctionDefinition::state_mutability()`].
        #[serde(default)]
        state_mutability: Option<StateMutability>, // not@ok
        #[serde(default, rename = "virtual")]
        is_virtual: bool, // not@ok
        /// Whether or not this function is the constructor. Only valid for Solidity versions below
        /// 0.5.x.
        ///
        /// After 0.5.x you must use `kind`. For cross-version compatibility use
        /// [`FunctionDefinition::kind()`].
        #[serde(default)]
        is_constructor: bool, // not@ok
        /// Whether or not this function is constant (view or pure). Only valid for Solidity
        /// versions below 0.5.x.
        ///
        /// After 0.5.x you must use `state_mutability`. For cross-version compatibility use
        /// [`FunctionDefinition::state_mutability()`].
        #[serde(default)]
        is_declared_const: bool, // not@ok
        /// Whether or not this function is payable. Only valid for Solidity versions below
        /// 0.5.x.
        ///
        /// After 0.5.x you must use `state_mutability`. For cross-version compatibility use
        /// [`FunctionDefinition::state_mutability()`].
        #[serde(default)]
        is_payable: bool, // not@ok
    }
);

impl FunctionDefinition {
    /// The kind of function this node defines.
    pub fn kind(&self) -> &FunctionKind {
        if let Some(kind) = &self.kind {
            kind
        } else if self.is_constructor {
            &FunctionKind::Constructor
        } else {
            &FunctionKind::Function
        }
    }

    /// The state mutability of the function.
    ///
    /// Note: Before Solidity 0.5.x, this is an approximation, as there was no distinction between
    /// `view` and `pure`.
    pub fn state_mutability(&self) -> &StateMutability {
        if let Some(state_mutability) = &self.state_mutability {
            state_mutability
        } else if self.is_declared_const {
            &StateMutability::View
        } else if self.is_payable {
            &StateMutability::Payable
        } else {
            &StateMutability::Nonpayable
        }
    }
}

/// Function kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum FunctionKind {
    /// A contract function.
    Function, // ok
    /// A receive function.
    Receive, // ok
    /// A constructor.
    Constructor, // ok
    /// A fallback function.
    Fallback, // ok
    /// A free-standing function.
    FreeFunction, // ok
}

ast_node!(
    /// A block of statements.
    struct Block {
        documentation: Option<String>, // okish
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        statements: Vec<Statement>, // ok
    }
);

stmt_node!(
    /// The break keyword.
    struct Break {}
);

stmt_node!(
    /// The continue keyword.
    struct Continue {}
);

stmt_node!(
    /// A do while statement.
    struct DoWhileStatement { // not@ok
        body: Block, 
        condition: Expression,
    }
);

stmt_node!(
    /// An emit statement.
    struct EmitStatement {
        event_call: FunctionCall, // okish
    }
);

stmt_node!(
    /// An expression statement.
    struct ExpressionStatement {
        expression: Expression, // ok
    }
);

stmt_node!(
    /// A for statement.
    struct ForStatement {
        body: BlockOrStatement, // ok
        condition: Option<Expression>, // ok
        initialization_expression: Option<ExpressionOrVariableDeclarationStatement>, // okish
        loop_expression: Option<ExpressionStatement>, // not@ok
    }
);

stmt_node!(
    /// A variable declaration statement.
    struct VariableDeclarationStatement {
        assignments: Vec<Option<usize>>, // ok
        declarations: Vec<Option<VariableDeclaration>>, // ok
        initial_value: Option<Expression>, // ok
    }
);

stmt_node!(
    /// An if statement.
    struct IfStatement { 
        condition: Expression, // ok
        false_body: Option<BlockOrStatement>, // ok
        true_body: BlockOrStatement, // ok
    }
);

ast_node!(
    /// A block of inline assembly.
    ///
    /// Refer to the [yul] module for Yul AST nodes.
    struct InlineAssembly {
        documentation: Option<String>,
        #[serde(rename = "AST")]
        ast: YulBlock,
        // TODO: We need this camel case for the AST, but pascal case other places in ethers-solc
        //evm_version: EvmVersion,
        external_references: Vec<ExternalInlineAssemblyReference>, // notok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        flags: Vec<InlineAssemblyFlag>, // notok
    }
);

/// A reference to an external variable or slot in an inline assembly block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExternalInlineAssemblyReference { // notok
    #[serde(with = "serde_helpers::display_from_str")]
    pub src: SourceLocation,
    pub declaration: usize,
    #[serde(default)]
    pub offset: bool,
    #[serde(default)]
    pub slot: bool,
    #[serde(default)]
    pub length: bool,
    pub value_size: usize,
    pub suffix: Option<AssemblyReferenceSuffix>,
}

/// An assembly reference suffix.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum AssemblyReferenceSuffix {                // notok
    /// The reference refers to a storage slot.
    Slot,
    /// The reference refers to an offset.
    Offset,
    /// The reference refers to a length.
    Length,
}

/// Inline assembly flags.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum InlineAssemblyFlag {
    #[serde(rename = "memory-safe")]
    MemorySafe, // notok
}

stmt_node!(
    /// A placeholder statement (`_`)
    struct PlaceholderStatement {} // okish?
);

stmt_node!(
    /// A return statement.
    struct Return {
        expression: Option<Expression>, // ok
        function_return_parameters: usize, // ok
    }
);

stmt_node!(
    /// A revert statement.
    struct RevertStatement {
        error_call: FunctionCall, // ok
    }
);

stmt_node!(
    /// A try/catch statement.
    struct TryStatement {
        clauses: Vec<TryCatchClause>, // ok
        external_call: FunctionCall, // ok
    }
);

ast_node!(
    /// A try/catch clause.
    struct TryCatchClause {
        block: Block, // ok
        error_name: String, // okish
        parameters: Option<ParameterList>, // okish
    }
);

stmt_node!(
    /// An unchecked block.
    struct UncheckedBlock { // okish
        statements: Vec<Statement>, // okish 
    }
);

stmt_node!(
    /// A while statement.
    struct WhileStatement {
        body: BlockOrStatement, // ok
        condition: Expression, // ok
    }
);

ast_node!(
    /// A modifier or base constructor invocation.
    struct ModifierInvocation {
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        arguments: Vec<Expression>, // ok
        kind: Option<ModifierInvocationKind>, // ok
        modifier_name: IdentifierOrIdentifierPath, // not@ok
    }
);

/// Modifier invocation kinds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ModifierInvocationKind {
    /// A regular modifier invocation.
    ModifierInvocation, // ok
    /// A base constructor invocation.
    BaseConstructorSpecifier, // ok
}

ast_node!(
    /// A modifier definition.
    struct ModifierDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        base_modifiers: Vec<usize>, // not@ok
        body: Block, // ok
        documentation: Option<StructuredDocumentation>, // okish
        overrides: Option<OverrideSpecifier>, // ok
        parameters: ParameterList, // ok
        #[serde(default, rename = "virtual")]
        is_virtual: bool, // okish
        visibility: Visibility, // ok
    }
);

ast_node!(
    /// A struct definition.
    struct StructDefinition {
        name: String, // ok
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        canonical_name: String, // ok
        members: Vec<VariableDeclaration>, // ok
        scope: usize, // ok
        visibility: Visibility, // ok
    }
);

ast_node!(
    /// A user defined value type definition.
    struct UserDefinedValueTypeDefinition {
        name: String, // ok
        #[serde(default, with ="serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        canonical_name: Option<String>, // ok
        underlying_type: TypeName, // ok
    }
);

ast_node!(
    /// A using for directive.
    struct UsingForDirective {
        #[serde(default, deserialize_with = "serde_helpers::default_for_null")]
        function_list: Vec<UsingForFunctionItem>, // okish
        #[serde(default)]
        global: bool, // ok
        library_name: Option<UserDefinedTypeNameOrIdentifierPath>, // ok
        type_name: Option<TypeName>, // ok
    }
);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum UsingForFunctionItem { // ok
    Function(FunctionIdentifierPath), // ok
    OverloadedOperator(OverloadedOperator), // ok
}

/// A wrapper around [IdentifierPath] for the [UsingForDirective].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionIdentifierPath {
    pub function: IdentifierPath, // ok
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OverloadedOperator {
    pub definition: IdentifierPath, // ok
    pub operator: String, // ok
}

ast_node!( 
    /// An import directive.
    struct ImportDirective {
        absolute_path: String, // ok
        file: String, // okish
        #[serde(default, with = "serde_helpers::display_from_str_opt")]
        name_location: Option<SourceLocation>, // ok
        scope: usize, // ok
        source_unit: usize, // ok
        symbol_aliases: Vec<SymbolAlias>, // ok
        unit_alias: String, // ok
    }
);

/// A symbol alias.
///
/// Symbol aliases can be defined using the [ImportDirective].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolAlias {
    pub foreign: Identifier, // not@ok 
    pub local: Option<String>, // ok
    #[serde(default, with = "serde_helpers::display_from_str_opt")]
    pub name_location: Option<SourceLocation>, // ok
}

ast_node!(
    /// A pragma directive.
    struct PragmaDirective {
        literals: Vec<String>,
    }
);

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, path::PathBuf};

    #[test]
    fn can_parse_ast() {
        fs::read_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-data").join("ast"))
            .unwrap()
            .for_each(|path| {
                let path = path.unwrap().path();
                let path_str = path.to_string_lossy();

                let input = fs::read_to_string(&path).unwrap();
                let deserializer = &mut serde_json::Deserializer::from_str(&input);
                let result: Result<SourceUnit, _> = serde_path_to_error::deserialize(deserializer);
                match result {
                    Err(e) => {
                        println!("... {path_str} fail: {e}");
                        panic!();
                    }
                    Ok(_) => {
                        println!("... {path_str} ok");
                    }
                }
            })
    }
}
