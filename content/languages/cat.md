+++
title = "Cat"
description = ""
date = 2010-07-27T00:43:31Z
aliases = []
[extra]
id = 7833
[taxonomies]
categories = []
tags = []
+++

Cat is a functional stack-based programming language inspired by the [Joy](https://rosettacode.org/wiki/Joy) programming language. Cat provides a static type system with type inferencing (like [ML](https://rosettacode.org/wiki/ML) or [Haskell](https://rosettacode.org/wiki/Haskell)), and a term rewriting macro language extension language called MetaCat. Cat uses static typing, and is somewhat less flexibility but safer than Joy.

All constructs in Cat (atomic programs, user defined programs, operators, literals, lists) behave as functions which takes a single stack as input and returns a new stack. In Cat the concatenation of two functions (e.g. [f g]) has the effect of composition of both functions (e.g. g(f(x))). All new user defined functions are defined as lists of functions. Cat not only has no variable declaration, there are no argument declarations. Cat also lends itself to the higher order functional programming: the lambda operation in Cat is the list construction operator "[...]" and currying can be achieved used basic operations such as "cons".

## See Also
[Cat (programming language)](https://en.wikipedia.org/wiki/Cat_(programming_language))
