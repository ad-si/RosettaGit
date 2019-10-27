+++
title = "Heap"
description = ""
date = 2008-10-13T13:52:03Z
aliases = []
[extra]
id = 3081
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]The '''heap''' is a pool of memory used by computer programs. Many of the variables declared in programs (like those <tt>malloc</tt>'d in [[C]] or <tt>new</tt>'d in [[Java]] or [[C++]]) are allocated on the heap. The data for these variables is accessed indirectly, usually through [[reference|references]]. Allocation on the heap is seemingly disorganized, which may be where it gets its name (a "heap" is basically just a big pile).

Memory allocated on the heap must be deallocated (or freed) once its purpose has been served if the programmer wants that particular memory location again. This may be done via automatic [[garbage collection]] or explicitly freeing the memory through references to it. "Memory leaks" are frequently instances where a branch in a program allocates memory on the heap without freeing it later, leaving unreferenced data in the heap with no way of recovering its memory location.

Memory not allocated on the heap is allocated on the [[system stack]], and includes things like subroutine return values and parameters, the return address of a subroutine, and local temporary variables, among other things.

There exist various algorithms of heap maintenance to respond the major problems of:

* fragmentation;
* unbounded allocation or else deallocation time.

Often the language uses more than one heap. One frequently used method is to have a heap for blocks of similar sizes, typically powers of two.

In typed languages objects created in the heap are usually allocated by the operator '''new''' or equivalent. Then they are '''constructed''' (or else initialized) in place. Before deallocation objects are '''destructed''' (finalized).

The heap must be interlocked when accessed from multiple [[task]]s (see also [[concurrent programming]]).

Some languages like [[Ada]] provide user-defined heaps, called '''storage pools''' (so the ''heap'' becomes merely a predefined storage pool.) The programmer may implement a better memory allocation and reclamation strategy for such pools when he knows the behavior of the objects allocated there. For example: arena, [[LIFO]] etc.
