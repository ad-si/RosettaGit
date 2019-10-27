+++
title = "Talk:Monads/List monad"
description = ""
date = 2016-02-01T21:18:04Z
aliases = []
[extra]
id = 20021
[taxonomies]
categories = []
tags = []
+++

==Perhaps a specific task ?==

It can help comparison across languages if there is a specific task with a readily intelligible motivation. Providing a specific set of inputs and expected outputs can make the code even more directly comparable.

One candidate (which would also allow comparison with the with the more syntactically-oriented List Comprehension task) would be to
# Write unit/return and bind for lists.
# Generate the cartesian product of, for example, [1..5] and [6..10]
# Obtain the list of all Pythagorean triples with elements between 1 and 25, by composing the application of a monadic triple-testing function (of type '''(Number, Number, Number) -> [(Number, Number, Number)]''' where the return value is the empty list if the triple is not Pythagorean) across the cartesian product of three lists (the ranges of possible values for (x, y, z) in the triples to be tested). (See the ES5 JavaScript example on this page).
