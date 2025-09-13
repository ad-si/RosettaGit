+++
title = "Lisaac"
description = ""
date = 2008-08-22T15:46:36Z
aliases = []
[extra]
id = 2996
[taxonomies]
categories = []
tags = []
+++
[http://isaacproject.u-strasbg.fr/index.html Lisaac] is a small [object](https://rosettacode.org/wiki/object-oriented_language) prototype-based programming language created to enable the implementation of the Isaac [operating system](https://rosettacode.org/wiki/operating_system) as part of Benoit Sonntag's thesis.  Since Lisaac was created to support the development of an operating system, it has strong support for implementing low level features even though it is itself a high level language.  The language was inspired by [Smalltalk](https://rosettacode.org/wiki/Smalltalk) (everything is an object), [Self](https://rosettacode.org/wiki/Self) (prototype based programming), and [Eiffel](https://rosettacode.org/wiki/Eiffel) (design by contract), but it has the distinction of being suitable to systems level programming through many techniques such as optimization of type predictions and code specialization.

In the performance benchmark [http://shootout.alioth.debian.org/ The Computer Language Benchmarks Game], [http://shootout.alioth.debian.org/gp4/benchmark.php?test=all&lang=all Lisaac currently places #5], essentially neck-and-neck with [C](https://rosettacode.org/wiki/C) and [C++](https://rosettacode.org/wiki/C++).

Lisaac compiles '.li' Lisaac source files into [ISO](https://rosettacode.org/wiki/ISO) standard C code, which can then be compiled to native machine code by any C compiler.  The current 'lisaac' compiler implementation also automatically compiles the generated '.c' file with gcc by default.
