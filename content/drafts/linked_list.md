+++
title = "Linked list"
description = ""
date = 2018-11-14T23:36:05Z
aliases = []
[extra]
id = 2004
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]][[Category:Data Structures]]A '''linked list''' is a data structure which allows a great deal of flexibility in memory allocation and data sorting. Linked lists depend on [[reference|references]] for their organization. Information is stored in "nodes" which contain data (integers, strings, etc., sometimes called an "element") and one or more "links" to to other nodes. The number of links determines what type of linked list it is (one link: "singly-linked list", two links: "doubly-linked list", three links: "triply-linked list", etc.), though one or two links are most common. Linked lists have a "head" (the first node in the list) and sometimes a "tail" (the last node).

In most languages, an implementation of a linked list is given and programmers will likely never need to create their own implementation. Most programmer-defined implementations found here should not be used in programs, but rather they are here to demonstrate language features and to help show how a linked list works.

{{Template:See also lists}}


Here are examples of the two common types of linked lists:

==Singly-Linked List==

A singly-linked list allows traversal in one direction, forward. To this end, each data element contains a reference to the next data element in the sequence. Single-linked list has [[O]](1) insertion time and [[O]](n) removal time.


### See also


* [[Singly-Linked List (element)]]
* [[Singly-Linked List (element insertion)]]
* [[Singly-linked list/Element removal|Singly-Linked List (element removal)]]
* [[Singly-Linked List (traversal)]]

==Doubly-Linked List==

A doubly-linked list allows traversal in two directions, forward and back. To this end, each data element contains references to both the previous and next elements. Doubly-linked list has [[O]](1) removal and insertion times. Further, these operations do not require the list head.


### See also


* [[Doubly-Linked List (element)]]
* [[Doubly-Linked List (element insertion)]]
* [[Doubly-linked list/Element removal|Doubly-Linked List (element removal)]]
* [[Doubly-Linked List (traversal)]]
