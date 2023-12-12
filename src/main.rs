use shard::compile;


const SOURCE: &str = "
fn to_centre(pos:2, eps:1)->1
  pos.length - eps

fn main()->4
  ⭕:= to_centre(uv, 0.3)
  colour := 3d(⭕*time.sin, (time+3.14159/2).sin, (1-⭕)*time.sin)
  normalized := colour/2 + 0.5
  4d(normalized.x, normalized.y, normalized.z, 1) 
";

fn main(){
  match compile(SOURCE){
    Ok(glsl) => {println!("{}", glsl)},
    Err(err) => println!("{:#?}", err),
  } 
}
