+++
title = "Bracmat"
description = ""
date = 2015-06-16T07:31:37Z
aliases = []
[extra]
id = 10609
[taxonomies]
categories = []
tags = []
+++

Bracmat is an interpreted programming language for symbolic computation. Originally (in the eighties) it was designed as a Computer Algebra system, but it has shown its merits in natural language processing as well. Bracmat has been used in the field of General Relativity for the algebraic computation of Ricci tensors from given space-time metrics, for the implementation of a dialogue-manager in virtual world project that allowed a user to communicate with software agents in plain English and with gestures, for the analysis of texts in a "Controlled Language"-project and for automatic error correction of hundreds of [HTML](https://rosettacode.org/wiki/HTML) pages. Bracmat has also shown its utility in some real-world applications: for example to identify persons, companies etc. in pre-tagged texts that must be anonymised. The to date most advanced application of Bracmat is as workflow planner and executor. Instead of letting the user choose between software tools, which the user may not know very well, the planner asks the user to specify what kind of output she wants. With this information the planner computes all (not necessarily sequential) combinations of tools and their parameter settings that combine into workflows that are guaranteed to produce the specified output from the given input. The computed list is condensed into a short format that highlights the differences between the workflows for the user and leaves out all that is of less importance.

Bracmat is almost unique in the combination of on the one hand allowing associative [pattern matching](https://rosettacode.org/wiki/pattern_matching), in strings as well as in tree structures, and on the other hand allowing expression evaluation during a match operation.

Bracmat is inspired by [SNOBOL4](https://rosettacode.org/wiki/SNOBOL4) (pattern matching, success/failure), by [Lisp](https://rosettacode.org/wiki/Lisp) (Bracmat programs are made of the same stuff as Bracmat data), by [Prolog](https://rosettacode.org/wiki/Prolog) (backtracking) and a little bit by [object-oriented](https://rosettacode.org/wiki/object-oriented) languages. The first implementation was for an ARM based computer. The ARM processor's 4-bit condition code selector on every instruction were the inspiration for Bracmat's flags ~ [ ! !! ` @ % > < # / ? that can be set on any node in an expression. For example, the ! and ? flags turn a symbol into a variable. When flags are combined, care has been taken that the semantics of a combination of flags is close to a combination of the semantics of each flag.

The Bracmat-interpreter is written in Standard [C](https://rosettacode.org/wiki/C) and can be compiled for many platforms, such as Epoc, [Windows](https://rosettacode.org/wiki/Windows), [Mac OS](https://rosettacode.org/wiki/Mac_OS) (including OS X), [Linux](https://rosettacode.org/wiki/Linux) and [Unix](https://rosettacode.org/wiki/Unix). The compiled code measures about 130 KB (statically linked), depending on the platform. The runtime is not very memory hungry, as compared to e.g. [Java](https://rosettacode.org/wiki/Java). Bracmat can be compiled for 32 and 64 bit systems.
