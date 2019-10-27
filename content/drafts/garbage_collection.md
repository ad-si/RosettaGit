+++
title = "Garbage collection"
description = ""
date = 2008-08-31T20:18:41Z
aliases = []
[extra]
id = 2696
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]'''Garbage collection''' (often abbreviated as '''GC''') is a technique used for management of the life cycle of the objects created [[run time|dynamically]], which scope is [[compile time|statically]] indeterminable. Sometimes GC is called "automatic memory management". Under GC the objects that are no longer in use by the program are said to be "collected." The object is finalized and then the memory allocated for it is returned to the language environment for further reuse. An object is considered not in use when there is no legal way to access it. In particular, when there are no other accessible objects [[reference|referencing]] it. Many GC algorithms differentiate in the way they determine absence of references.

Some [[programming language|programming languages]] (such as [[Java]], [[Tcl]], [[OCaml]], and [[Toka]]) have an integrated GC support. Languages like [[Ada]] allow implementations with GC, but don't mandate it. Other languages (such as [[C]] and [[C++]]) do not have GC.

GC is frequently critiqued for:

* unpredictable performance both in terms of time and space;
* distributed overhead for [[task|multi-tasking]] systems;
* difficulties in ensuring a proper finalization of collected objects;
* encouraging loose program design.

GC is a very vivid research area in computer science, focused on overwhelming the above mentioned drawbacks.
