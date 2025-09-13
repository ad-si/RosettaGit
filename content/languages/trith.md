+++
title = "Trith"
description = ""
date = 2011-09-10T03:12:10Z
aliases = []
[extra]
id = 7348
[taxonomies]
categories = []
tags = []
+++
'''Trith''' is an experimental stack-based, [http://concatenative.org/wiki/view/Concatenative%20language concatenative] programming language by [Arto Bendiken](https://rosettacode.org/wiki/User:Arto_Bendiken). It is dynamically typed and has a homoiconic program representation. The implementation currently consists of a virtual machine, interpreter, and compiler toolchain written in [Ruby](https://rosettacode.org/wiki/Ruby) and an in-the-works runtime targeting the [JVM](https://rosettacode.org/wiki/runs_on_vm::Java_Virtual_Machine).

Trith programs are simply nested lists of operators and operands, with the operators identified by URIs. This means that Trith code can be straightforwardly represented externally either as S-expressions or as [http://linkeddata.org/ Linked Data] in the form of [http://en.wikipedia.org/wiki/Resource_Description_Framework RDF] triples.

Trith is inspired and influenced by the author's experience with [Forth](https://rosettacode.org/wiki/Forth), [Lisp](https://rosettacode.org/wiki/Lisp) and [Scheme](https://rosettacode.org/wiki/Scheme) in general, and the concatenative languages [Joy](https://rosettacode.org/wiki/Joy), [XY](https://rosettacode.org/wiki/XY), [Factor](https://rosettacode.org/wiki/Factor) and [Cat](https://rosettacode.org/wiki/Cat) in particular.
