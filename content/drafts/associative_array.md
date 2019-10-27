+++
title = "Associative array"
description = ""
date = 2013-09-22T11:40:23Z
aliases = []
[extra]
id = 1773
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]][[Category:Data Structures]]
An '''associative array''' is a collection indexed by arbitrary data types, not just small integers. Whereas an [[array]] is typically implemented as many same-sized items stored in a contiguous block of memory, an associative array must be implemented via a more complex data structure, such as a hash table, a list, or some other type of map.

The terminology and semantics of these vary among different programming languages.  For example in Perl and Ruby they are called “''hashes''” (from abbreviating “hash table”, the underlying implementation) while in Python, Objective-C, and Smalltalk they are called “''dictionaries''” (by analogy to the normal English usage of the term: an indexed reference by which keys (words) are associated with values (definitions)).  In Lua they are called “''tables''”. In Java, C++, and Go they are called “''maps''”.

The semantics differ as well.  While all of these allow the programmer to associate some sort of key with some sort of value they can differ considerably in how they evaluate the key and what sorts of values can be stored.

For example in [[awk]] and [[Perl]] the keys are evaluated (as “scalars” in Perl terminology).  Thus the the keys <tt>"1"</tt> (a string) and <tt>1</tt> (an integer) and <tt>1.0</tt> (a real or floating point number) would all evaluate into equivalent keys.  By contrast these would each be distinct in Python.  In a [[Python]] dictionary any immutable object (strings, integer, floats) and any object/class which implements the <tt>__hash__</tt> special method can be used as a key.  Values can be references to any objects (including functions, classes, class methods which are all "first class objects" in that language).  In [[Lua]], a table is a complex data structure which can be used to implement arrays, objects and associative arrays (integer key values are implicitly treated like indices into a virtual array, those with values that reference functions are methods, those which reference other types of objects are attributes or members).

Associative arrays are used as the underlying data structure for objects in a number of languages.  Python objects normally have a visible <tt>__dict__</tt> attribute by which its methods and other attributes can be accessed indirectly. PHP objects can be cast into associative arrays, and vice versa, with the keys and values corresponding to property names and values of the object. Perl objects are references of (usually) hashes. And (as described above) Lua objects are implemented as tables (functions and other objects are “first class objects” which an be assigned to keys and passed around as arguments, as with Python). Similarly, in JavaScript the concepts of associative array and object are the same, since JavaScript lacks a separate "associative array" type, and object attributes can be accessed using subscript operator with the attribute name as a string.

{{Template:See also lists}}
==References==
* Free On-line Dictionary of Computing, http://foldoc.org/, Editor Denis Howe

== See also ==
* [[Creating an Associative Array]]
* [[Creating a Hash from Two Arrays]]
* [[Associative arrays/Iteration]]
