+++
title = "Category:Programming paradigm/Functional"
description = ""
date = 2018-07-23T09:13:33Z
aliases = []
[extra]
id = 4388
[taxonomies]
categories = []
tags = []
+++

{{feature|Programming paradigm}}'''Functional programming''' treats functions as the fundamental first-class objects of the programming language.  In addition:

# State changes are encapsulated in function results, in this paradigm, rather than in side effects, as in [[imperative programming]]
# This makes dependencies explicit.
# This leads to [http://www.haskell.org/haskellwiki/Referential_transparency referential transparency] -- given the same arguments, a piece of code will always behave identically.
# Coding is '''compositional''' -- any two pieces of code with a matching 'interface', as specified by function domains, can be combined, because no hidden side-effects can intervene. (See [https://www.thocp.net/biographies/papers/backus_turingaward_lecture.pdf ''Can Programming Be Liberated from the von Neumann Style? A Functional Style and Its Algebra of Programs''] by John Backus, 1977 Turing Award Lecture).
# It's easy to '''refactor''' similar pieces of code, because any subexpression can be replaced by a variable bound at the outside.

This leads to a coding style which is very different from more traditional languages.  [[Iteration]] may be replaced by [[:Category:Recursion|tail-recursion]] or delegated to other functions.  Lists become a very important datatype.  Code often consists of a composition of simpler blocks, which makes it look similar to [[declarative programming]].

Most functional programming languages (FPLs) are related to the [[wp:Lambda_Calculus|lambda calculus]], which makes the specification of their formal semantics simpler.

One important characteristic of FPLs is their default evaluation order. ''Strict'' or ''eager'' FPLs will evaluate an argument as soon as possible, while [[lazy evaluation]] will do that as late as possible.

Strict FPLs often have an ''impure'' aspect that does allow functions with implicit side-effects, in order to interact with the (stateful) outside world. In non-strict FPLs, one uses [[wp:Monads_in_functional_programming|monads]] or other means like uniqueness types to guarantee correct sequencing of side-effects.

With monads, one can also do [[imperative programming]] even in a purely functional languages, which is especially helpful if the notion of state is natural to the problem space.
