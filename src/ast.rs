
/// A trait implemented for AST Nodes that encapsulates their ability to yield a
/// GLSL representation of their semantics.
trait Glslify {
  fn glsl(&self)->String;
}


/// Unify all types of AST node into a common enum, representing
/// a generic node in the tree.
// pub enum AstNode{
//   Scope(Scope),
//   Assignment(Assignment),
//   Expression(Expression),
//   PostfixUnaryOperator(PostfixUnaryOperator),
//   PrefixUnaryOperator(PrefixUnaryOperator),
//   InfixBinaryOperator(InfixBinaryOperator),
//   Uniform(Uniform),
// }

#[derive(Debug)]
/// AST Node representing a uniform passed into the shader or a transformed version
/// of a built-in defined in the main function, such as uv from 
/// iResolution and glFragCoord. 
/// 
/// This is an atom of an expression.
pub enum Uniform{
  UV,
  Time
}
impl Glslify for Uniform{
  fn glsl(&self)->String{
    match self {
      Uniform::UV => "uv".to_owned(),
      Uniform::Time => "iTime".to_owned(),
    }
  }
}

#[derive(Debug)]
/// AST Node representing a binary operator in infix notation with no type information.
pub enum InfixBinaryOperator {
  Add,
  Subtract, 
  Multiply, 
  Divide
}

#[derive(Debug)]
/// AST Node representing a unary operator in prefix notation with no type information.
pub enum PrefixUnaryOperator {
  Negate
}

#[derive(Debug)]
/// AST Node representing a unary operator in postfix notation with no type information.
pub enum PostfixUnaryOperator{
  ProjectX,
  ProjectY,
  ProjectZ,
  ProjectW,
  Sin,
  Fract,
  Length
}

#[derive(Debug)]
/// AST Node representing an expression that can be evaluated to yield a value.
/// 
/// The atoms of an expression have a value associated with them, like literal 
/// numbers and uniforms. Other expressions can be built from expressions recursively
/// by applying operators. 
/// 
/// The precedence of the operators is defined in the Pratt Parser.
pub enum Expression {
  Identifier(String),
  Uniform(Uniform),
  Number(f64),
  PreUnaryOp{
    op: PrefixUnaryOperator,
    val: Box<Expression>
  },
  PostUnaryOp{
    op: PostfixUnaryOperator,
    val: Box<Expression>
  },
  InfixBinaryOp{
    lhs: Box<Expression>,
    op: InfixBinaryOperator,
    rhs: Box<Expression>,
  },
}

impl Glslify for Expression{
  fn glsl(&self)->String{
    match self{
      Expression::Uniform(uniform) => uniform.glsl(),
      Expression::Identifier(name) => name.to_string(),
      Expression::Number(n) => n.to_string(),
      Expression::PreUnaryOp { op, val } => {
        match op{
          PrefixUnaryOperator::Negate => format!("(-{})", val.glsl()).to_owned(),
        }
      },
      Expression::PostUnaryOp { op, val } => {
        match op {
          PostfixUnaryOperator::ProjectX => format!("({}).x", val.glsl()).to_owned(),
          PostfixUnaryOperator::ProjectY => format!("({}).y", val.glsl()).to_owned(),
          PostfixUnaryOperator::ProjectZ => format!("({}).z", val.glsl()).to_owned(),
          PostfixUnaryOperator::ProjectW => format!("({}).w", val.glsl()).to_owned(),
          PostfixUnaryOperator::Sin => format!("sin({})", val.glsl()).to_owned(),
          PostfixUnaryOperator::Fract => format!("fract({})", val.glsl()).to_owned(),
          PostfixUnaryOperator::Length => format!("length({})", val.glsl()).to_owned(),
        }
      },
      Expression::InfixBinaryOp { lhs, op, rhs } => {
        match op{
          InfixBinaryOperator::Add => format!("({} + {})", lhs.glsl(), rhs.glsl() ).to_owned(),
          InfixBinaryOperator::Subtract => format!("({} - {})", lhs.glsl(), rhs.glsl() ).to_owned(),
          InfixBinaryOperator::Multiply => format!("({} * {})", lhs.glsl(), rhs.glsl() ).to_owned(),
          InfixBinaryOperator::Divide => format!("({} / {})", lhs.glsl(), rhs.glsl() ).to_owned(),
        }
      },
    }
  }
}

#[derive(Debug)]
/// AST Node representing an assignment of an expression to an identifier.
pub struct Assignment {
  pub ident: String,
  pub val: Box<Expression>
}

#[derive(Debug)]
/// AST Node representing a scope, consisting of arbitrarily many assignments followed
/// by an expression.
pub struct Scope {
  pub assign: Vec<Assignment>,
  pub expr: Box<Expression>
}
