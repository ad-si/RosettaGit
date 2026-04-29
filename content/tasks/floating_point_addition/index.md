+++
title = "Floating Point Addition"
date = 2026-04-29T00:00:00Z
[extra]
task = """
  Show how your language evaluates `0.1 + 0.2` and compares the result
  to `0.3`.
  If the language has alternative numeric types
  (e.g. arbitrary-precision decimals or rationals)
  that produce an exact result, demonstrate those as well.
"""
[taxonomies]
categories = ["task"]
tags = ["math", "numerics"]
languages = [
  "bash",
  "c",
  "common_lisp",
  "go",
  "haskell",
  "java",
  "javascript",
  "perl",
  "php",
  "python",
  "raku",
  "ruby",
  "rust",
]
+++


## Background

Most languages use IEEE 754 binary64 floating-point numbers by default.
Neither `0.1` nor `0.2` has an exact binary representation,
so their sum rounds to `0.30000000000000004` instead of `0.3`,
and a direct equality check against `0.3` returns false.

The website [0.30000000000000004.com](https://0.30000000000000004.com)
collects examples of this behaviour
across a large number of programming languages.


## Task

For each language:

1. Compute `0.1 + 0.2` using the default numeric type and print the result
    with enough precision to expose the rounding error.
2. Compare the result to `0.3` and print whether they are equal.
3. If the language provides exact decimal or rational arithmetic
    in its standard library or a common module,
    repeat the computation with that type and show that it returns `0.3` exactly.


## Related

- [Approximate Equality](@/tasks/approximate_equality.md)
- [Pathological floating point problems](@/tasks/pathological_floating_point_problems.md)
- [Extreme floating point values](@/tasks/extreme_floating_point_values.md)
- [Literals/Floating point](@/tasks/literals_floating_point.md)


## Bash


{{ code(src="content/tasks/floating_point_addition/bash.sh", lang="bash") }}



## C


{{ code(src="content/tasks/floating_point_addition/c.c", lang="c") }}



## Common Lisp


{{ code(src="content/tasks/floating_point_addition/common_lisp.lisp", lang="lisp") }}



## Go


{{ code(src="content/tasks/floating_point_addition/go.go", lang="go") }}



## Haskell


{{ code(src="content/tasks/floating_point_addition/haskell.hs", lang="haskell") }}



## Java


{{ code(src="content/tasks/floating_point_addition/java.java", lang="java") }}



## JavaScript


{{ code(src="content/tasks/floating_point_addition/javascript.js", lang="javascript") }}



## Perl


{{ code(src="content/tasks/floating_point_addition/perl.pl", lang="perl") }}



## PHP


{{ code(src="content/tasks/floating_point_addition/php.php", lang="php") }}



## Python


{{ code(src="content/tasks/floating_point_addition/python.py", lang="python") }}



## Raku

In Raku, decimal literals are `Rat` (rational) values by default,
so `0.1 + 0.2` is `0.3` exactly.
The `Num` type exposes the underlying IEEE 754 double.


{{ code(src="content/tasks/floating_point_addition/raku.raku", lang="raku") }}



## Ruby


{{ code(src="content/tasks/floating_point_addition/ruby.rb", lang="ruby") }}



## Rust


{{ code(src="content/tasks/floating_point_addition/rust.rs", lang="rust") }}
