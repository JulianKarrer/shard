use pest::iterators::Pair;

use crate::{types::Type, parse::Rule, error::CompileError};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFINE THE ABSTRACT SYNTAX TREE ~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
/// Encapsulates common properties every node in the AST has, such as
/// an optional type that is later inferred and information about it's position
/// in the source code.
pub struct AstProperties{
  pub own_type: Option<Type>,
  pub source_start: (usize, usize),
  pub source: String,
}

impl AstProperties{
  /// Create a new set of properties of the AST node parsed, including the matched pair's
  /// position in the source code and an undetermined type.
  pub fn new(pair: &Pair<'_, Rule>)->Self{
    Self { own_type: None, source_start: pair.line_col(), source: pair.as_str().to_owned() }
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ATOMS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
/// AST Node representing a uniform passed into the shader or a transformed version
/// of a built-in defined in the main function, such as uv from 
/// iResolution and glFragCoord. 
/// 
/// This is an atom of an expression.
pub enum Uniform{
  UV(AstProperties),
  Time(AstProperties)
}
#[derive(Debug, Clone)]
/// AST Node representing an identifier.
/// This is an atom of an expression.
pub struct Identifier{
  pub name: String,
  pub properties: AstProperties,
  pub redefined: Option<bool>
}


#[derive(Debug, Clone)]
/// AST Node representing a Number in one dimension.
/// This is an atom of an expression.
pub struct Number{
  pub value: f64,
  pub properties: AstProperties,
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OPERATORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
/// AST Node representing a binary operator in infix notation with no type information.
pub enum InfixBinaryOperator {
  /// Addition between numbers of equal dimensionality
  Add,
  /// Subtraction between numbers of equal dimensionality
  Subtract,
  /// Component-wise multiplication between vectors, between scalars
  /// or the product of a vector and scalar, depending on the input types.
  Multiply,
  Divide,
}

#[derive(Debug, Clone)]
/// AST Node representing a unary operator in postfix notation with no type information.
pub enum UnaryOperator{
  // prefix operators
  Negate,

  // regular operators
  ProjectX,
  ProjectY,
  ProjectZ,
  ProjectW,
  Sin,
  Fract,
  Length,
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
/// AST Node representing an expression that can be evaluated to yield a value.
/// 
/// The atoms of an expression have a value associated with them, like literal 
/// numbers and uniforms. Other expressions can be built from expressions recursively
/// by applying operators. 
/// 
/// The precedence of the operators is defined in the Pratt Parser.
pub enum Expression {
  Identifier(Identifier),
  Uniform(Uniform),
  Number(Number),
  UnaryOp{
    op: UnaryOperator,
    val: Box<Expression>,
    properties: AstProperties,
  },
  BinaryOp{
    lhs: Box<Expression>,
    op: InfixBinaryOperator,
    rhs: Box<Expression>,
    properties: AstProperties,
  },
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OTHERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
/// AST Node representing an assignment of an expression to an identifier.
pub struct Assignment {
  pub ident: String,
  pub val: Box<Expression>,
  pub properties: AstProperties,
}

#[derive(Debug, Clone)]
/// AST Node representing a function, the body of which consists of 
/// arbitrarily many assignments followed by an expression.
pub struct Function {
  pub ident: String,
  pub arg_identifiers: Vec<String>,
  pub assign: Vec<Assignment>,
  pub expr: Box<Expression>,
  pub properties: AstProperties,
}

impl Function{
  /// Get the arguments of a function as a vector of (identifier, type) pairs
  pub fn get_args_clone(&self)->Result<Vec<(String, Type)>, CompileError>{
    if let Some(Type::Function { args, returns: _ }) = &self.properties.own_type {
      Ok(self.arg_identifiers.iter().cloned()
        .zip(args.iter().cloned())
        .collect()
      )
    } else {
      Err(CompileError::Type("Internal error: function with type other than 'Function'".to_owned()))
    }
  }
}

#[derive(Debug, Clone)]
/// AST Node representing the entire program, which is a series of function
/// definitions.
pub struct Program {
  pub functions: Vec<Function>,
}
