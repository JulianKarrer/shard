use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "shard.pest"]
pub struct ShardParser;

/// A trait implemented for AST Nodes that encapsulates their ability to yield a
/// GLSL representation of their semantics.
trait Glslify {
  fn glsl(&self)->String;
}

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


lazy_static::lazy_static!(
  /// A pratt parser for creating an abstract syntax tree (AST) from an expression token.
  static ref PRATT_PARSER: PrattParser<Rule> = {
    use pest::pratt_parser::{Assoc::*, Op};
    use Rule::*;
    PrattParser::new()
      // Add Operators to the pratt parser in increasing order of precedence
      .op(Op::infix(add, Left) | Op::infix(sub, Left))
      .op(Op::infix(mul, Left) | Op::infix(div, Left))
      .op(Op::prefix(neg))
      .op(
        Op::postfix(x) | Op::postfix(y) | Op::postfix(z) | Op::postfix(w) |
        Op::postfix(sin)|Op::postfix(fract)|Op::postfix(length)
      )
  };
);

/// Parse an expression token, yielding an AST Node representing an expression
/// with no type information provided.
pub fn parse_expr(pairs: Pairs<Rule>) -> Expression {
  PRATT_PARSER
  // primaries
  .map_primary(|primary| match primary.as_rule() {
    Rule::num =>
      Expression::Number(primary.as_str().parse::<f64>().unwrap()), 
    Rule::uniform => Expression::Uniform( 
      match primary.as_str(){
        "uv" => Uniform::UV,
        "time" => Uniform::Time,
        uniform => unreachable!("Expr::parse expected uniform, found {:?}", uniform)
      }), 
    rule => unreachable!("Expr::parse expected atom, found {:?}", rule)
  })
  // postfix operators
  .map_postfix(|lhs, op|{
    let op = match op.as_rule() {
      Rule::x => PostfixUnaryOperator::ProjectX,
      Rule::y => PostfixUnaryOperator::ProjectY,
      Rule::z => PostfixUnaryOperator::ProjectZ,
      Rule::w => PostfixUnaryOperator::ProjectW,
      Rule::sin => PostfixUnaryOperator::Sin,
      Rule::fract => PostfixUnaryOperator::Fract,
      Rule::length => PostfixUnaryOperator::Length,
      rule => unreachable!("Expr::parse expected postfix operation, found {:?}", rule)
    };
    Expression::PostUnaryOp { op, val: Box::new(lhs) } 
  })
  // prefix operators
  .map_prefix(|op, rhs| match op.as_rule(){
    Rule::neg => Expression::PreUnaryOp { 
      op: PrefixUnaryOperator::Negate, 
      val: Box::new(rhs)
    },
    rule => unreachable!("Expr::parse expected prefix operation, found {:?}", rule),
  })
  // infix operators
  .map_infix(|lhs, op, rhs| {
      let op = match op.as_rule() {
          Rule::add => InfixBinaryOperator::Add,
          Rule::sub => InfixBinaryOperator::Subtract,
          Rule::mul => InfixBinaryOperator::Multiply,
          Rule::div => InfixBinaryOperator::Divide,
          rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
      };
      Expression::InfixBinaryOp {
          lhs: Box::new(lhs),
          op,
          rhs: Box::new(rhs),
      }
  })
  .parse(pairs)
}


const TEMPLATE:&str =   "precision highp float;
uniform vec2 iResolution;
uniform float iTime;

void main() {
  vec2 uv = gl_FragCoord.xy / iResolution;
  gl_FragColor = vec4(uv.x, uv.y, 0.5*sin(iTime)+0.5, 1);
}
";


fn compile(source: &str) -> String {
  match ShardParser::parse(Rule::expr, source) {
    Ok(mut pairs) => {
      println!(
        "Parsed: {:#?}",
        // inner of expr
        parse_expr(pairs.next().unwrap().into_inner())
      );
    }
    Err(e) => {
      eprintln!("Parse failed: {:?}", e);
    }
  }
  TEMPLATE.to_owned()
}

fn main(){
  println!("{}", compile("uv.x.sin * 5.0 + 2"))
}