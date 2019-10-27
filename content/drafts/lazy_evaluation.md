+++
title = "Lazy evaluation"
description = ""
date = 2009-08-17T20:30:15Z
aliases = []
[extra]
id = 2929
[taxonomies]
categories = []
tags = []
+++

<!--[[Category:Programming Paradigms]] is this a paradigm?-->[[Category:Encyclopedia]]'''Lazy evaluation''' describes a strategy of expression evaluation when the evaluation is done as late as possible. The opposite to lazy evaluation is '''eager evaluation''', when the evaluation is performed as early as possible. Constant folding is an example of eager evaluation optimization. [[Haskell]]'s handling of infinite lists is an example of lazy evaluation.

Usually the programming language does not specify laziness, leaving the decision to optimization.

Lazy evaluation is best represented by short-circuit logical operations, known to many programming languages. 

There exist two major contexts where laziness of evaluation is considered:

* When the expression value does not depend on the evaluation time, which is usually the case for [[functional programming]] languages. In this context lazy evaluation can be used for optimization purposes and as light-weight [[closures]].

* When the expression value depends on the evaluation time, laziness changes the program semantics. This type of laziness is used in low-level [[concurrent programming]], when the state of an object might change asynchronously to the program. Such objects are usually marked as ''volatile'', and their values are acquired as late as possible. Further, the compiler is instructed not to store them when evaluating temporal expressions.
