+++
title = "Type algebra"
description = ""
date = 2008-12-15T15:33:48Z
aliases = []
[extra]
id = 3141
[taxonomies]
categories = []
tags = []
+++

[[Category:Type System]]
'''Type algebra''' is an [http://en.wikipedia.org/wiki/Algebraic_structure algebraic structure] of types in a typed programming language. The algebra considers a type as a value involved in some operations. These type-valued operations create new types out of the existing types. The operations may take parameters different from types. For example, parametric polymorphism also known as generics or templates allows along with types, plain values, packages, subprograms etc as parameters.

==Predefined types==
Usually the language provides some set of built-in types used as arguments for the algebraic operations. Typically they include:
* Boolean;
* [[Basic integer arithmetic | Integer]];
* Modular integer;
* Unconstrained integer;
* String;
* Floating-point number;
* Fixed-point number;
* Complex number;
* Address. 

==Algebraic operations==
The operations supported by most of the languages are:
* [[Array]] type construction. The operation takes two argument types: the array index type and the array element type;
* Record type construction. The operation takes the list of record field names, usually static constants and the list of corresponding types;
* [[Enumeration]] type construction. The operation takes the list of names, usually statically constant;
* Reference/pointer type construction. The operation takes the type of the target object;
* Parametrization mentioned above, also sometimes called '''parametric polymorphism''';
* [[Inheritance]], also called dynamic [[polymorphism]], sometimes type extension. The operation takes one ('''single inheritance''') or more ('''multiple inheritance''') types as the parameters and the parameters like in the record type construction. The first part constitute the result type's base and the second its ''extension''. The set of types bound by the corresponding relation parent-derived type is called [[classes | class]];
* Type constraining, or else subtyping (non-[http://en.wikipedia.org/wiki/Liskov_substitution_principle Liskov] subtyping), also known as specialization. The operation takes some parameters and a base type in order to constraint the set of values of the former. A typical example of constraining are ranged numeric types. Often the base type is [[abstract type | abstract]], because there is no way to provide an implementation of mathematical numbers. Another example is indefinite arrays. The constraint specifies the array index bounds; 
* Type cloning operation produces a type which is a copy of the argument type. This operation is used in the languages with [[Type compatibility | nominal]] types equivalence;
* [[classes | Class]] rooted in the type.

==Dynamic semantics==
The type-values can be first-class objects in the language. In this case the types can be created at [[run time]], which is typical for dynamically typed languages. In statically typed languages the operations of types algebra are often applied strictly at [[compile time]] like in [[C++]], or else some of their parameters required to be statically constant like in [[Ada]]. The goal of these limitations is to keep types second-class objects and, in particular, to avoid [[closure | upward closures]].
