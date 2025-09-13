+++
title = "NewLISP"
[extra]
website = "http://www.newlisp.org/"
+++

newLISP focuses on the core components of [LISP](https://rosettacode.org/wiki/LISP):
lists, symbols, and lambda expressions.
To these, newLISP adds [arrays](https://rosettacode.org/wiki/array),
implicit indexing on lists and arrays,
and dynamic and lexical scoping.
Lexical scoping is implemented using separate namespaces called contexts.

The result is an easier-to-learn LISP
that is even smaller than most Scheme implementations,
but which still has about 300 built-in functions.
Approximately 200k in size, newLISP is built for high portability
using only the most common [UNIX](https://rosettacode.org/wiki/UNIX) system [C](https://rosettacode.org/wiki/C)-libraries.
It loads quickly and has a small memory footprint.
newLISP is as fast or faster than other popular scripting languages
and uses very few resources.
