+++
title = "Lambda Prolog"
description = ""
date = 2016-03-27T17:23:21Z
aliases = []
[extra]
id = 20684
[taxonomies]
categories = []
tags = []
+++


From [http://www.lix.polytechnique.fr/~dale/lProlog/ the principle λProlog] page:

<blockquote>
λProlog is a logic programming language based on an intuitionistic fragment of Church's Simple Theory of Types. Such a strong logical foundation provides λProlog with logically supported notions of

# modular programming,
# abstract datatypes,
# higher-order programming, and
# the lambda-tree syntax approach to the treatment of bound variables in syntax.

Implementations of λProlog contain implementations of the simply typed λ-terms as well as (of subsets) of higher-order unification.
</blockquote>

The syntax is similar to [Prolog](https://rosettacode.org/wiki/Prolog), but it extends Prolog's basis of [https://en.wikipedia.org/wiki/Horn_clause Horn clause logic] to [https://en.wikipedia.org/wiki/Harrop_formula higher-order hereditary Harrop formulas]. Its higher-order nature allows for quantifying over predicates, and its basis in lambda-tree syntax facilitates construction of terms using lambda abstraction. All λProlog predicates require explicit type signatures.

λProlog was first developed in 1986. It has had a number implementations, and is still under active development.
