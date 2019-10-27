+++
title = "Type checking"
description = ""
date = 2009-07-08T13:14:55Z
aliases = []
[extra]
id = 2978
[taxonomies]
categories = []
tags = []
+++

[[Category:Type System]]'''Type checking''' is another aspect of the type system of a [[programming language]].

Languages are said to use "[[:Category:Typing/Checking/Static|static]]" type checking if the types of variables in assignments and function calls are checked at [[compile time]]. In other words, when programming in a language that uses static type checking, it is possible to get a type error before your program runs.

Languages are said to use "[[:Category:Typing/Checking/Dynamic|dynamic]]" type checking if the types are checked at run time.

A language may also use a combination of static and dynamic type checking at certain times. For example, type casting sometimes requires run time type checking.

Static type checking will make your language more reliable (a.k.a. act more predictably), and can result in faster execution. Dynamic type checking can make compiling and interpreting easier and faster, but can slow down execution.
