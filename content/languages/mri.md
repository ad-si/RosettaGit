+++
title = "MRI"
description = ""
date = 2012-02-21T03:09:30Z
aliases = []
[extra]
id = 9330
[taxonomies]
categories = []
tags = []
+++
```ruby
RUBY_ENGINE == "ruby"
```
''Matz's Ruby Implementation'' or ''MRI'' refers to the original [Ruby](https://rosettacode.org/wiki/Ruby) interpreter by Yukihiro Matsumoto, the inventor of Ruby. Matz and contributors wrote the interpreter in [C language](https://rosettacode.org/wiki/implemented_in_language::C); MRI is also known as ''C Ruby'' or ''CRuby'', by analogy with [CPython](https://rosettacode.org/wiki/CPython).

The term "MRI" excludes other Ruby engines (such as [JRuby](https://rosettacode.org/wiki/JRuby) or [Rubinius](https://rosettacode.org/wiki/Rubinius)). For example, one can say that Ruby has Array#sort method, and MRI uses [quicksort](https://rosettacode.org/wiki/Sorting_algorithms/Quicksort); other Ruby engines might use different sorting algorithm.

When code works with Ruby 1.8.7, this can be MRI 1.8.7, or anything else that implements the same language.

## Advantages
* MRI is the [reference implementation](https://rosettacode.org/wiki/reference_implementation), and the first implementation of every new Ruby version.
* MRI can fork, though only for [Unix](https://rosettacode.org/wiki/Unix) clones (not for [Windows](https://rosettacode.org/wiki/Windows)).
* MRI can save and restore [continuations](https://rosettacode.org/wiki/:Category:continuation), though the implementation is slow because it copies the call stack.

## Disadvantages
* MRI has the ''Global VM Lock'', alias ''Giant VM Lock'' or ''GVL''. A thread, to run Ruby code, must hold this exclusive lock. Only one thread can hold the GVL; therefore, multiple threads can use only one CPU. (Contrast [JRuby](https://rosettacode.org/wiki/JRuby), where multiple threads can use multiple CPUs.)
* MRI 1.8 is slow. Programs for Ruby 1.8 often run faster in [JRuby](https://rosettacode.org/wiki/JRuby) or [Rubinius](https://rosettacode.org/wiki/Rubinius).

## Features
MRI has

# an interpreter,
# a mark-and-sweep [garbage collector](https://rosettacode.org/wiki/garbage_collection), and
# the core and standard libraries.

Most of the core library is in C. The standard library is a mix of Ruby code and C extensions.

MRI 1.8 has a somewhat slow interpreter. MRI 1.9 has a new interpreter called Yet Another Ruby VM (YARV); it translates Ruby source code to an internal [bytecode](https://rosettacode.org/wiki/bytecode), then interprets the bytecode. Ruby code can run a few times faster in MRI 1.9 than in MRI 1.8. Ruby code remains slower than C code. For example, MRI 1.9.3 changes its 'date' package from Ruby code to a C extension; this gives better performance.
