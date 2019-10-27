+++
title = "Type compatibility"
description = ""
date = 2009-02-03T15:55:25Z
aliases = []
[extra]
id = 2926
[taxonomies]
categories = []
tags = []
+++

[[Category:Type System]]'''Type equivalence''' describes the way types are considered equivalent when matched. Types can be equivalent:

* by name (nominal equivalence);
* by structure (structural equivalence).

In a more general context type matching considers not only the relation of type equivalence (T<sub>1</sub>=T<sub>2</sub>), but also the relations of subsumption (T<sub>1</sub><:T<sub>2</sub>), subtyping, [[inheritance|subclassing]] etc. These also can be nominal or structural.

A structural matching is necessarily based on type inference. In presence of user-defined abstract data types, it is generally undecidable whether two types are semantically equivalent to each other. Even if equivalence is decidable, in complicated cases it might be very difficult for the code maintainer to determine if two types in question are indeed equivalent or not. It is also incompatible with the ''information hiding principle'', because hidden implementation details might prevent the reader to see types equivalent. For these reasons modern typed languages with elaborated type systems tend towards nominal type equivalence. At the same time for many standard types, and especially for subtypes, it becomes very tedious to use nominal equivalence. Therefore, programming languages have some pragmatic mixture of structural and nominal equivalences.
==Nominal equivalence example (in [[Ada]])==

```ada

type Integer_1 is range 1..20;
type Integer_2 is range 1..20;

X : Integer_1 := 1;
Y : Integer_2 := X; -- Type error

```

Integer_1 does not match Integer_2 though both types have equivalent sets of values and operations.
==Structural equivalence example (in [[Ada]])==

```ada

X : access String := new String'("ABC");
Y : not null access String := X; -- This is OK

```

The anonymous type of X matches the anonymous type of Y. The structure of both types is the same: "a pointer to String". Further, the constraint ''not null'' put on the type of Y does not effect equivalence because it is also structural.
