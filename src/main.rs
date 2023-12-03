use ast::Scope;
use error::Error;
use parse::{ShardParser, parse_scope, Rule};
use pest::Parser;

use crate::types::InfersType;

mod parse;
mod ast;
mod types;
mod error;

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
  if let Ok(mut ast) = compile("
  k:= 1
  🤩:= uv
  uv.y.sin * k + 🤩.y"){
    println!("{:#?}", ast);
    println!("TYPED: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    ast.set_own_type(&mut vec![]).unwrap();
    println!("{:#?}", ast);
  };
}