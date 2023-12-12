
<p align="center">
  <img width="512" height="512" src="./shard.jpg" alt="A prismatic, crystal shard in the style of a minimalistic logo.">
</p>

<h1 align="center">THE SHARD PROGRAMMING LANGUAGE</h1>
A purely functional programming language for writing fragment shaders and creating shader art.

Shard transpiles to GLSL and is compatible with online platforms for shader art such as [Shadertoy](https://www.shadertoy.com/) and [GLSL Sandbox](https://glslsandbox.com/), but aims to provide cleaner syntax and convenient functions to make life easier.

Your code could look something like this:
```
fn to_centre(pos:2, eps:1)->1
  pos.length - eps

fn main()->4
  ⭕:= to_centre(uv, 0.3)
  colour := 3d(⭕*time.sin, (time+3.14159/2).sin, (1-⭕)*time.sin)
  normalized := colour/2 + 0.5
  4d(normalized.x, normalized.y, normalized.z, 1) 
```

## Features
- clean syntax
- purely functional language with no side effects
- 🇫🇺🇱🇱 🇺​🇳​🇮​🇨​🇴🇩​🇪 🇸🇺🇵🇵🇴🇷🇹 🤩
- no mutable data, no recursion, no confusion
- valid programs are guaranteed to terminate

## TODO
- [X] parsing and formal grammar
- [X] transpile AST to GLSL
- [X] type checking and inference
- [X] built-in and user defined functions and calls
- [ ] overload built-in functions like with `genType` in GLSL
- [ ] more compile-time checks (has main, no overrides, no recursion)
- [ ] `cargo test` and integration
- [ ] add built-in functions for easy colours, palettes, 2D/3D SDFs, 2D drawing and possibly text
- [ ] anonymous/Lambda functions as parameters
- [ ] built-in function for 'for' loops using lambdas (reduce-like)
- [ ] **C**ommand **L**ine **I**nterface
- [ ] parse mutliple files (user libraries/imports)
- [ ] compile-time optimization
- [ ] better error messages

Nice to have:
- [ ] included OpenGL runtime with image/video export (+ Webassembly support?)
- [ ] more uniforms for default runtime (backbuffer, mouse, mp3_fft, ...)
- [ ] Language Server Protocol implementation


## Implementation
The transpiler as written in the Rust programming language. 

The formal grammar can be found in [shard.pest](shard.pest), which is used by [Pest](https://pest.rs/) to generate a Pratt parser.
