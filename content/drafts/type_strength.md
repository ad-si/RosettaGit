+++
title = "Type strength"
description = ""
date = 2008-08-08T20:30:59Z
aliases = []
[extra]
id = 2957
[taxonomies]
categories = []
tags = []
+++

[[Category:Type System]]'''Type strength''' is a vague term usually used to describe the semantics of typing. According to it languages are subdivided into:

* Strongly typed;
* Weakly typed;
* Untyped.

In strongly typed languages the semantics of an expression is determined by the types of the operands in an unambiguous and more or less complete manner. In weakly typed languages the semantics may depend on the values of the operands, the context, language implementation etc. For example in [[C]], the type ''int'' has an undefined set of values and an undefined behavior of arithmetic operations upon overflow. Typically, weakly typed languages allow implicit type conversions of ill-defined behavior. For example, when a string is implicitly converted to integer, the semantics of this conversion in the cases when the string does not contain a properly formatted number, is left to the implementation.

Modern strongly typed languages provide some mechanisms resembling weak typing, while preserving the semantics well-defined. For example, they allow [[polymorphism]] (overloading, overriding, generics), user-defined implicit conversions, types inference. In all such cases, the programmer becomes responsible to define the semantics of ambiguous constructs. For instance, an overridden procedure has to be given an implementation consistent with contract of the class. Because the semantics of such definitions is ultimately decomposed into strongly typed predefined language constructs, one hopes that the result would be in turn well-defined.

Untyped languages have a type system with only few types of the first-class objects, sometimes only one. These types are built-in and anonymous. If more than one, they are resolved syntactically. Untyped are low-level languages (e.g. [[Assembly]], [[Forth]]) and some domain-specific languages.
