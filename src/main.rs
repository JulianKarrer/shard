use ast::Scope;
use error::Error;
use halfbrown::HashMap;
use parse::{ShardParser, parse_scope, Rule};
use pest::Parser;

use crate::types::TypedAstNode;

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
  if let Ok(ast) = compile("
  k:= 1
  🤩:= 5
  uv.x.sin * k + 🤩"){
    println!("{:#?}", ast);
    println!("TYPED: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    let typed = TypedAstNode::from_ast(ast::AstNode::Scope(ast), &mut HashMap::new()).unwrap();
    println!("{:#?}", typed);
  };
}