+++
title = "Order"
description = ""
date = 2012-06-10T09:08:30Z
aliases = []
[extra]
id = 10844
[taxonomies]
categories = []
tags = []
+++
'''Order''' is a high-level, functional programming language implemented entirely using the [C](https://rosettacode.org/wiki/C) preprocessor. It provides a full programming language for [metaprogramming](https://rosettacode.org/wiki/metaprogramming) in C (or any other environment where the C preprocessor can be used). A debugging environment and line-by-line evaluator are also included. Rather than building a library of macros to solve specific metaprogramming problems individually, Order-PP implements a continuation-based virtual machine which then interprets the high-level Order language.

Order is based primarily on lambda calculus, and is vaguely similar in semantics to languages such as ML or [Scheme](https://rosettacode.org/wiki/Scheme), offering first-class functions with partial application, lexical scope, first-class continuations, arbitrary-precision arithmetic, and built-in operators for compound types.

Order requires some components from its sister project, Chaos (included in the download), to also be installed, although it is not primarily based on it. Order also requires a standard-conforming [C99](https://rosettacode.org/wiki/C99) preprocessor, and has absolutely no workarounds for non-conformance - currently, only [GCC](https://rosettacode.org/wiki/GCC) is confirmed to work with all examples.

## See Also
* [http://chaos-pp.cvs.sourceforge.net/viewvc/chaos-pp/?view=tar Official download (Chaos, includes Order)]
