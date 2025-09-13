+++
title = "Arturo"
description = ""
date = 2019-10-11T15:55:41Z
aliases = []
[extra]
id = 22567
[taxonomies]
categories = []
tags = []
+++

'''Arturo''' is a general-purpose, multi-paradigm language that aims to be simple, modern and powerful, vaguely inspired by various other ones - including but not limited to [Ruby](https://rosettacode.org/wiki/Ruby), [Haskell](https://rosettacode.org/wiki/Haskell), [D](https://rosettacode.org/wiki/D), [SDL](https://rosettacode.org/wiki/SDL) (Simple Declarative Language), [Tcl](https://rosettacode.org/wiki/Tcl) and [Lisp](https://rosettacode.org/wiki/Lisp).


### Principles


It is built on some very simple and straightforward principles:


### =Everything is a simple statement=


There are no "special" language constructs (''even <code>if</code> is nothing but a simple statement''). Everything you see is a statement in the form <code>ID <expression> <expression> <expression> ...</code>


#### Code is data - and data is code
Arturo can be used both as a data-interchange format and a programming language. Basically all data structures are valid code and all code can be represented as a data structure. Think of it as [SDL](https://rosettacode.org/wiki/SDL)/[Json](https://rosettacode.org/wiki/Json)/[YAML](https://rosettacode.org/wiki/YAML)/[XML](https://rosettacode.org/wiki/XML) combined with the power of [Lisp](https://rosettacode.org/wiki/Lisp) - but without the... sea of opening and closing parentheses.



### =Each statement returns a value=


Whether what you would consider a "function" or any other statement, it will return a value. If it's a block of code (see: ''function''), the last statement's result will be return - unless specified otherwise.


#### Functions are first-class citizens
Functions - or blocks of statements enclosed in <code>{}</code> - can be anything. Assign them to a symbol/variable, pass them around as arguments to function calls, include them as a dictionary key value, or return them from a function. And of course they can be either named or anonymous/lambda.



### =Uniform syntax=


As already mentioned, everything is a statement of the form `ID <expressions>`. So, how does this work?

* Is the ID a new or non-function existing symbol? Then, the right-hand value will be assigned to it
* Is it a function? Then it will be called, with the right-hand values as arguments
* Do you want to use the result of a function call as part of an expression? Just enclose the function call in <code>$(...)</code>	E.g.: <code>print $(reverse #(1 2 3))</code>


### Implementation

The main Arturo interpreter is written in the [D](https://rosettacode.org/wiki/D) language.


### License

Arturo is released under the [MIT/X11 License](https://rosettacode.org/wiki/MIT/X11_License).


### Todo

[Reports:Tasks_not_implemented_in_Arturo](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Arturo)
