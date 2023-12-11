use std::sync::OnceLock;
use crate::ast::{Number, Identifier, AstProperties, Program, FunctionCall};
use crate::glslify::identifier_utf8_to_ascii;
use crate::types::{dimension_from_str, Type};
use crate::{ast, CompileError};
use ast::{Expression, Uniform, UnaryOperator, InfixBinaryOperator, Function};
use ast::Assignment;
use pest::Parser;
use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
// ~~~~~~~~~~~~~~~~~~~~~~~~ DEFINE A PRATT PARSER WITH PRECEDENCES ~~~~~~~~~~~~~~~~~~~~~~

#[derive(pest_derive::Parser)]
#[grammar = "shard.pest"]
pub struct ShardParser;
static PRATT_PARSER:OnceLock<PrattParser<Rule>> = OnceLock::new();

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARSE EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Parse an expression token, yielding an AST Node representing an expression
/// with no type information provided.
pub fn parse_expr(pairs: Pairs<Rule>) -> Result<Expression, CompileError> {
  PRATT_PARSER.get_or_init(||{
    /// A pest.rs pratt parser for creating an abstract syntax tree (AST) from an expression token.
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
  })
  // primaries
  .map_primary(|primary: pest::iterators::Pair<'_, Rule>| 
    match primary.as_rule() {
    Rule::num =>
      Ok(Expression::Number({
        let parsed_value = primary.as_str().parse::<f64>();
        match parsed_value{
          Ok(num) => Ok(Number{ value: num, properties: AstProperties::new(&primary) }),
          Err(_) => Err(CompileError::throw_parse("number", &primary))
        }
      }?)), 
    Rule::uniform => {
      Ok(Expression::Uniform(
        match primary.as_str(){
          "uv" => Ok(Uniform::UV(AstProperties::new(&primary))),
          "time" => Ok(Uniform::Time(AstProperties::new(&primary))),
          _ => Err(CompileError::throw_parse("uniform", &primary))
        }?
      ))
    }, 
    Rule::ident => Ok(Expression::Identifier(Identifier{ 
      name: primary.as_str().to_owned(), 
      properties: AstProperties::new(&primary),
    })),
    Rule::call => {
      Ok(Expression::FunctionCall(parse_call(primary)?))
    }
    _ => Err(CompileError::throw_parse("atom", &primary))
  })
  // postfix operators
  .map_postfix(|lhs, op|{
    let properties = AstProperties::new(&op);
    let operator = match op.as_rule() {
      Rule::x => Ok(UnaryOperator::ProjectX),
      Rule::y => Ok(UnaryOperator::ProjectY),
      Rule::z => Ok(UnaryOperator::ProjectZ),
      Rule::w => Ok(UnaryOperator::ProjectW),
      Rule::sin => Ok(UnaryOperator::Sin),
      Rule::fract => Ok(UnaryOperator::Fract),
      Rule::length => Ok(UnaryOperator::Length),
      _ => Err(CompileError::throw_parse("unary postfix operator", &op))
    };
    Ok(Expression::UnaryOp { op: operator?, val: Box::new(lhs?), properties } )
  })
  // prefix operators
  .map_prefix(|op, rhs| match op.as_rule(){
    Rule::neg => Ok(Expression::UnaryOp { 
      op: UnaryOperator::Negate, 
      val: Box::new(rhs?),
      properties: AstProperties::new(&op)
    }),
    _ => Err(CompileError::throw_parse("unary prefix operator", &op)),
  })
  // infix operators
  .map_infix(|lhs, op, rhs| {
    let properties = AstProperties::new(&op);
    let operator = match op.as_rule() {
        Rule::add => Ok(InfixBinaryOperator::Add),
        Rule::sub => Ok(InfixBinaryOperator::Subtract),
        Rule::mul => Ok(InfixBinaryOperator::Multiply),
        Rule::div => Ok(InfixBinaryOperator::Divide),
        _ => Err(CompileError::throw_parse("binary infix operator", &op)),
    };
    Ok(Expression::BinaryOp {
      lhs: Box::new(lhs?),
      op: operator?,
      rhs: Box::new(rhs?),
      properties
    })
  })
  .parse(pairs)
}


/// Parse a call token, returning a FunctionCall struct, which can be used 
/// to create an expression AST node.
fn parse_call(pair: pest::iterators::Pair<'_, Rule>)->Result<FunctionCall, CompileError>{
  let missing_identifier_error = CompileError::throw_parse("function call identifier", &pair);
  let properties = AstProperties::new(&pair);

  // the first component of a call token must be the identifier of the function being called
  let mut pairs = pair.into_inner();
  let function_ident = identifier_utf8_to_ascii(pairs.next().ok_or(missing_identifier_error)?.as_str());
  // all other components of the call token are arguments, which are each an expression
  let mut args = vec![];
  for component in pairs{
    match component.as_rule() {
      Rule::expr => args.push(parse_expr(component.into_inner())?),
      _ => return Err(CompileError::throw_parse("function call argument", &component)),
    }
  }
  Ok(FunctionCall{ 
    function_ident, 
    args, 
    properties,
  })
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARSE FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Parse a function token, yielding an AST Node representing any number of
/// assignments followed by an expression with no type information provided to either.
fn parse_function(mut pairs: Pairs<Rule>) -> Result<Function, CompileError> {
  let error = CompileError::Parse(
    "Internal error parsing function.".to_owned());
  let source = pairs.as_str().to_owned();
  let source_start = pairs.peek().ok_or(error.clone())?.line_col();

  // parse the function signature
  let signature = pairs.next().ok_or(error.clone())?.into_inner();
  let mut function_name = None;
  let mut function_returns = None;
  let mut arg_types = vec![];
  let mut arg_identifiers = vec![];
  for pair in signature{
    // function signatures contain an identifier, a return numtype and multiple optional
    // args
    match pair.as_rule(){
      Rule::ident => {
        function_name = Some(identifier_utf8_to_ascii(pair.as_str()));
      }
      Rule::numtype => {
        function_returns = Some(dimension_from_str(pair.as_str())?)
      }
      Rule::arg => {
        let mut arg = pair.into_inner();
        // parse the argument identifier
        let arg_ident = arg.next().ok_or(error.clone())?;
        if arg_ident.as_rule() == Rule::ident {
          arg_identifiers.push(identifier_utf8_to_ascii(arg_ident.as_str()).to_owned());
        } else {return Err(error.clone())}
        // parse the argument type
        let arg_type = arg.next().ok_or(error.clone())?;
        if arg_type.as_rule() == Rule::numtype {
          arg_types.push(Type::Number(dimension_from_str(arg_type.as_str())?));
        } else {return Err(error.clone())}
      }
      _ => {return Err(error.clone())}
    }
  }

  // fill in the type of the function
  let scope = pairs.next().ok_or(error.clone())?.into_inner();
  let properties =  AstProperties{ 
    own_type: Some(Type::Function { 
      args: arg_types, 
      returns: function_returns.ok_or(error.clone())?
    }), 
    source_start, 
    source,
  };

  // parse the function body, or scope
  let mut assignments:Vec<Assignment> = vec![];
  let mut expression: Option<Box<Expression>> = None;
  for pair in scope{
    match pair.as_rule(){
      Rule::assign => {
        let properties = AstProperties::new(&pair);
        let mut inner = pair.clone().into_inner();
        let ident = inner.next().ok_or(
          CompileError::throw_parse("identifier on left side of the assignment", &pair.clone())
        )?.as_str().to_owned();
        let val = parse_expr(inner.next().ok_or(
          CompileError::throw_parse("identifier on right side of the assignment", &pair)
          )?.into_inner());
        assignments.push(Assignment { ident, val: Box::new(val?), properties})
      },
      Rule::expr => {
        if expression.is_some() {
          // if more than one expression that is not part of an assignment is found in a scope, 
          // throw a parse error.
          return Err(
            CompileError::throw_parse("only one expression per function body", &pair)
          )
        } else {
          expression = Some(Box::new(parse_expr(pair.into_inner())?))
        }
      },
      _ => return Err(CompileError::throw_parse("an assignment or expression", &pair))
    }
  }
  Ok(Function{ 
    assign: assignments,
    expr: expression.ok_or(
      CompileError::Parse("Expected at least one expression in scope".to_owned()))?,
    properties,
    ident: function_name.ok_or(error.clone())?,
    arg_identifiers,
  })
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARSE THE PROGRAM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pub fn parse_program(source: &str) -> Result<Program, CompileError>{
  match ShardParser::parse(Rule::program, source) {
    Ok(mut pairs) => {
      // unwrap the main program token
      let program = pairs.next()
        .ok_or(CompileError::Parse("Could not locate a function".to_owned()))?
        .into_inner();
      
      // parse each function in the program
      let mut functions = vec![];
      for pair in program{
        if let Rule::function = pair.as_rule(){
          functions.push(parse_function(pair.into_inner())?);
        }
      }
      Ok( Program { functions })
    }
    Err(e) => {Err(CompileError::Parse(e.to_string()))}
  }
}
