use crate::ast::{Number, Identifier};
use crate::{ast, Error};
use ast::{Expression, Uniform, PostfixUnaryOperator, PrefixUnaryOperator, InfixBinaryOperator, Scope};
use ast::Assignment;
use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use lazy_static::lazy_static;

// ~~~~~~~~~~~~~~~~~~~~~~~~ DEFINE A PRATT PARSER WITH PRECEDENCES ~~~~~~~~~~~~~~~~~~~~~~

#[derive(pest_derive::Parser)]
#[grammar = "shard.pest"]
pub struct ShardParser;

lazy_static!(
  /// A pest.rs pratt parser for creating an abstract syntax tree (AST) from an expression token.
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

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARSE EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Parse an expression token, yielding an AST Node representing an expression
/// with no type information provided.
pub fn parse_expr(pairs: Pairs<Rule>) -> Result<Expression, Error> {
  PRATT_PARSER
  // primaries
  .map_primary(|primary| match primary.as_rule() {
    Rule::num =>
      Ok(Expression::Number({
        let parsed_value = primary.as_str().parse::<f64>();
        match parsed_value{
          Ok(num) => Ok(Number{ value: num }),
          Err(_) => Err(Error::throw_parse("number", primary))
        }
      }?)), 
    Rule::uniform => {
      Ok(Expression::Uniform(
        match primary.as_str(){
          "uv" => Ok(Uniform::UV),
          "time" => Ok(Uniform::Time),
          _ => Err(Error::throw_parse("uniform", primary))
        }?
      ))
    }, 
    Rule::ident => Ok(Expression::Identifier(Identifier{ name: primary.as_str().to_owned() })),
    _ => Err(Error::throw_parse("atom", primary))
  })
  // postfix operators
  .map_postfix(|lhs, op|{
    let op = match op.as_rule() {
      Rule::x => Ok(PostfixUnaryOperator::ProjectX),
      Rule::y => Ok(PostfixUnaryOperator::ProjectY),
      Rule::z => Ok(PostfixUnaryOperator::ProjectZ),
      Rule::w => Ok(PostfixUnaryOperator::ProjectW),
      Rule::sin => Ok(PostfixUnaryOperator::Sin),
      Rule::fract => Ok(PostfixUnaryOperator::Fract),
      Rule::length => Ok(PostfixUnaryOperator::Length),
      _ => Err(Error::throw_parse("unary postfix operator", op))
    };
    Ok(Expression::PostUnaryOp { op: op?, val: Box::new(lhs?) } )
  })
  // prefix operators
  .map_prefix(|op, rhs| match op.as_rule(){
    Rule::neg => Ok(Expression::PreUnaryOp { 
      op: PrefixUnaryOperator::Negate, 
      val: Box::new(rhs?)
    }),
    _ => Err(Error::throw_parse("unary prefix operator", op)),
  })
  // infix operators
  .map_infix(|lhs, op, rhs| {
    let op = match op.as_rule() {
        Rule::add => Ok(InfixBinaryOperator::Add),
        Rule::sub => Ok(InfixBinaryOperator::Subtract),
        Rule::mul => Ok(InfixBinaryOperator::Multiply),
        Rule::div => Ok(InfixBinaryOperator::Divide),
        _ => Err(Error::throw_parse("binary infix operator", op)),
    };
    Ok(Expression::InfixBinaryOp {
      lhs: Box::new(lhs?),
      op: op?,
      rhs: Box::new(rhs?),
    })
  })
  .parse(pairs)
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARSE SCOPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Parse a scope token, yielding an AST Node representing any number of
/// assignments followed by an expression with no type information provided to either.
pub fn parse_scope(pairs: Pairs<Rule>) -> Result<Scope, Error> {
  let mut assignments:Vec<Assignment> = vec![];
  let mut expression: Option<Box<Expression>> = None;
  for pair in pairs{
    match pair.as_rule(){
      Rule::assign => {
        let mut inner = pair.clone().into_inner();
        let ident = inner.next().ok_or(
          Error::throw_parse("identifier on left side of the assignment", pair.clone())
        )?.as_str().to_owned();
        let val = parse_expr(inner.next().ok_or(
          Error::throw_parse("identifier on right side of the assignment", pair)
          )?.into_inner());
        assignments.push(Assignment { ident, val: Box::new(val?) })
      },
      Rule::expr => {
        if expression.is_some() {
          // if more than one expression that is not part of an assignment is found in a scope, 
          // throw a parse error.
          return Err(Error::throw_parse("only one expression", pair))
        } else {
          expression = Some(Box::new(parse_expr(pair.into_inner())?))
        }
      },
      _ => return Err(Error::throw_parse("an assignment or expression", pair))
    }
  }
  Ok(Scope{ 
    assign: assignments,
    expr: expression.ok_or(Error::ParseError("Expected at least one expression in scope".to_owned()))?
  })
}

