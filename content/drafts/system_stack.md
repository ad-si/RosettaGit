+++
title = "System stack"
description = ""
date = 2008-11-12T13:33:41Z
aliases = []
[extra]
id = 3082
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]The '''system stack''' (a.k.a. '''call stack''' or just "the stack") is a place in memory for things that the [[heap]] doesn't cover. The system stack is more organized than the heap since it uses the [[stack]] data structure, where order matters. Also, the address of the next allocation is known at all times because of this organization. Allocated items are pushed on to the stack in a particular order and popped off when needed.

Most importantly, the system stack is used to store information about subroutine calls (where it gets the name "call stack"). The stack stores parameters for the function and a return address where the program should pick up when the function is finished. It also reserves a space for a return value to be popped by the system on return. The piece of stack used by a subprogram is called a '''stack frame'''.

Because of its limited size, a stack may "overflow" if too many function calls are made without returning. This situation is dangerous because, if not handled properly (usually by the program stopping and freeing all of its memory), the stack could intersect and overwrite other memory from the program, other programs, or the [[operating system]]. In high-integrity systems there are usually strict design requirements on the stack size. Use of the heap is usually prohibited, for the same reason.

Often there exists more than one ''stack'':

* In [[concurrent programming]] each [[task]] has a ''stack'' of its own. Unlike the heap, ''stacks'' need not to be shared between the tasks and thus no interlocking is required;
* The language run-time environment may maintain multiple ''stacks'' for one task. For example, the arguments and the local variables of a subprogram may be allocated on a different stack from the stack used for the return value. When the subprograms are allowed to return values of variable size, this prevents confusion between return values and local variables. Upon return stacks are swapped;
* Secondary ''stacks'' may be used for allocation of non-contiguous objects, typically varying strings (consisting of the dope and body).
