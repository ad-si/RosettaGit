+++
title = "ATS"
description = ""
date = 2015-05-07T01:25:08Z
aliases = []
[extra]
id = 11006
[taxonomies]
categories = []
tags = []
+++
'''ATS''' is a [statically typed](https://rosettacode.org/wiki/Type_checking) programming language that unifies implementation with formal specification. It is equipped with a highly expressive [type system](https://rosettacode.org/wiki/type_system) rooted in the framework Applied Type System, which gives the language its name. In particular, both dependent types and linear types are available in ATS.



* [Functional programming](https://rosettacode.org/wiki/Functional_programming). The core of ATS is a functional language based on eager (aka. call-by-value) evaluation, which can also accommodate lazy (aka. call-by-need) evaluation. The availability of linear types in ATS often makes functional programs written in it run not only with surprisingly high efficiency (when compared to [C](https://rosettacode.org/wiki/C)) but also with surprisingly small (memory) footprint (when compared to [C](https://rosettacode.org/wiki/C) as well).

* [Imperative programming](https://rosettacode.org/wiki/Imperative_programming). The novel and unique approach to imperative programming in ATS is firmly rooted in the paradigm of programming with theorem-proving. The type system of ATS allows many features considered dangerous in other languages (e.g., explicit pointer arithmetic and explicit memory allocation/deallocation) to be safely supported in ATS, making ATS a viable programming langauge for low-level systems programming.

* [Concurrent programming](https://rosettacode.org/wiki/Concurrent_programming). ATS can support multithreaded programming through safe use of pthreads. The availability of linear types for tracking and safely manipulating resources provides an effective approach to constructing reliable programs that can take great advantage of multicore architectures.

* [Modular programming](https://rosettacode.org/wiki/Modular_programming). The module system of ATS is largely infuenced by that of [Modula-3](https://rosettacode.org/wiki/Modula-3), which is both simple and general as well as effective in supporting large scale programming.



In addition, ATS contains a subsystem ATS/LF that supports a form of (interactive) theorem-proving, where proofs are constructed as total functions. With this component, ATS advocates a programmer-centric approach to program verification that combines programming with theorem-proving in a syntactically intertwined manner. Furthermore, this component can serve as a logical framework for encoding deduction systems and their (meta-)properties.

## Citations
* [Wikipedia:ATS (programming language)](https://en.wikipedia.org/wiki/ATS_%28programming_language%29)
