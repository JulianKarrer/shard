use ast::Scope;
use error::CompileError;
use parse::{ShardParser, parse_scope, Rule};
use pest::Parser;

use crate::{types::set_types, glslify::Glslify};

mod parse;
mod ast;
mod types;
mod error;
mod glslify;

// precision highp float;
// uniform vec2 iResolution;
// uniform float iTime;

// void main() {
//   gl_FragColor = vec4(1.0)
// }


fn compile(source: &str) -> Result<Scope, CompileError> {
  match ShardParser::parse(Rule::scope, source) {
    Ok(mut pairs) => {
      Ok(parse_scope(pairs.next().ok_or(CompileError::Parse("No scope found".to_owned()))?.into_inner())?)
    }
    Err(e) => {Err(CompileError::Parse(e.to_string()))}
  }
}

fn main(){
  if let Ok(mut ast) = compile("
  k:= 1
  🤩:= uv
  uv.y.sin * k + 🤩.y"){
    set_types(&mut ast).unwrap();
    println!("AST: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    println!("{:#?}", ast);
    println!("CODE: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    println!("{}", ast.to_glsl().unwrap());
  };
}