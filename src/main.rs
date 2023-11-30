use ast::Scope;
use parse::{ShardParser, Rule, parse_scope};
use pest::{Parser, iterators::Pair};

mod parse;
mod ast;

/// An enum representing all types of errors that may occur
/// in the compilation process. All compile passes that may fail
/// should return one of these kinds of errors as the error variant
/// of a Result<>.
pub enum Error{
  ParseError(String),
  TypeError(String),
}

impl Error{
  /// Returns a ParseError, filling a predefined template with the expected
  /// construct and properties of the construct found. These include its line
  /// and column in the input, its text and the parser rule it matched against.
  pub fn throw_parse(expected: &str, found:Pair<'_, Rule>)->Self{
    Error::ParseError(format!(
      "Expected {} at line {}, column {}. Instead found '{}', which is a '{:?}'", 
      expected, found.line_col().0, found.line_col().1, found.as_str(), found.as_rule()
    ))
  }
}

// precision highp float;
// uniform vec2 iResolution;
// uniform float iTime;

// void main() {
//   gl_FragColor = vec4(1.0)
// }


fn compile(source: &str) -> Result<Scope, Error> {
  match ShardParser::parse(Rule::scope, source) {
    Ok(mut pairs) => {
      Ok(parse_scope(pairs.next().ok_or(Error::ParseError("No scope found".to_owned()))?.into_inner())?)
    }
    Err(e) => {Err(Error::ParseError(e.to_string()))}
  }
}

fn main(){
  if let Ok(ast) = compile("
  k:= 1
  🤩:= 5
  uv.x * k + 🤩"){
    println!("{:#?}", ast)
  };
}