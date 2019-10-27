+++
title = "Reference"
description = ""
date = 2009-02-03T15:49:19Z
aliases = []
[extra]
id = 2573
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]A '''reference''' or '''pointer''' is a value referencing an object. Often reference values are implemented as a stored machine address to the target object's memory representation. The prerequisite of this is a flat memory model and immovable objects.

Pointers represented by plain stored addresses are often called '''thin'''. '''Fat pointers''' are references that contain some additional information. For example, pointers to unconstrained arrays may contain the array bounds, pointers to polymorphic objects may contain the actual type of, pointers to the system objects may contain a cookie, etc.

In typed languages references are type safe. The value of a reference has a distinct type and this type depends on the type of the target object. In languages like [[C]] and [[C++]], references can be interpreted as raw memory addresses. In languages like [[Java]] and [[C sharp|C#]], they cannot. In [[Ada]] references are further limited to the only objects which are declared ''aliased''. For non-aliased object the compiler can apply optimizations like moving in the physical memory etc.

The term reference is also used in '''reference semantics''' as opposed to '''value semantics''', especially with regard to the parameter passing convention. An object has reference semantics if it is identifiable through a reference to. In particular, one can compare references in order to decide if they refer to the ''same'' object. When object is not identifiable through references, it is said that it has a value semantics. This implies that the value exhaustively describes the object's state so that two objects having the same value are indistinguishable.

Parameters of a subprogram may have either referential or value semantics, in this case one talks about passing parameters by-reference or by-value. The way of parameter passing is independent on the mutability of parameters. When a mutable (in-out) parameter is passed by value, it is copied in before the call and then copied out upon return.

Referential semantics is exposed to the '''aliasing problem'''. The issue arises when the same object is passed into a subprogram, or becomes otherwise accessible through two distinct names or reference objects. In this case updating the object through one reference would implicitly change the target of another. So in presence of aliasing the program semantics might become dependent on referential or value semantics of the object, thus on an implementation. Example:

```ada

   procedure Inc (X : in out Integer; Y : Integer) is
   begin
      X := X + 1;
      X := X + Y;
   end Inc;
   Value : Integer := 1;
begin
   Inc (Value, Value); -- Value is aliased as X and Y in Inc

```

Here, when Integer is passed by value, the result will be 3, when it were passed by reference it would be 4. Aliasing is harmless when objects are immutable.

References are an integral part of referential structures such as [[Linked List|linked lists]].
