
<p align="center">
  <img width="512" height="512" src="./shard.jpg" alt="A prismatic, crystal shard in the style of a minimalistic logo.">
</p>

# THE SHARD PROGRAMMING LANGUAGE
A purely functional programming language for writing fragment shaders and creating shader art.

Shard transpiles to GLSL and is compatible with online platforms for shader art such as [Shadertoy](https://www.shadertoy.com/) and [GLSL Sandbox](https://glslsandbox.com/), but aims to provide cleaner syntax and convenient functions to make life easier.

Fragment shaders are functions that maps a position on the screen, current timestamp and other attributes to an output colour with no other side effects. This problem obviously lends itself to a purely functional programming language with no side effects.

Shard does not aim to be a general purpose language and is not Turing complete, but rather *helpfully* restrictive for the given problem: all valid Shard programs are guaranteed to terminate and produce the expected type.

Shard is currently in early development and many of the features invisioned are not a reality *yet*. Contact me for any information: jk@studioerika.de