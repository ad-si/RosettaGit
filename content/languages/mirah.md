+++
title = "Mirah"
description = ""
date = 2011-09-20T01:21:38Z
aliases = []
[extra]
id = 10411
[taxonomies]
categories = []
tags = []
+++

Mirah is a new way of looking at [runs on vm::JVM](https://rosettacode.org/wiki/runs_on_vm::JVM) languages. In attempting to build a replacement for [Java](https://rosettacode.org/wiki/Java), we have followed a few guiding principals:
* No runtime library
Mirah does not impose any jar files upon you. YOU decide what your application's dependencies should be.
* Clean, simple syntax
We have borrowed heavily from [derived from::Ruby](https://rosettacode.org/wiki/derived_from::Ruby), but added static typing and minor syntax changes to support the JVM's type system. The result is pleasing to the eye, but as powerful as Java.
* [Metaprogramming](https://rosettacode.org/wiki/Metaprogramming) and macros
Mirah supports various mechanisms for compile-time metaprogramming and macros. Much of the "open class" feel of dynamic languages is possible in Mirah.
* No performance penalty
Because Mirah directly targets the JVM's type system and JVM [bytecode](https://rosettacode.org/wiki/bytecode), it performs exactly as well as Java.
