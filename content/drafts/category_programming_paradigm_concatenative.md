+++
title = "Category:Programming paradigm/Concatenative"
description = ""
date = 2011-04-11T01:32:28Z
aliases = []
[extra]
id = 9421
[taxonomies]
categories = []
tags = []
+++

{{feature|programming paradigm}}
'''Concatenative programming''' passes a single data structure from function to function. With a ''concatenative language'', concatenation is [[function composition]]. The concatenation of two programs is a valid program that passes this single data structure from the first program to the second program.

This single data structure is probably a [[stack]]. A concatenative language is probably a ''stack-oriented language''. These languages tend to use reverse Polish notation with postfix operators. Programs push values on the data stack, then operate on those values. For example,

* <tt>3 4 +</tt> might push 7.
* <tt>9 7 -</tt> might push 2.
* The concatenation <tt>3 4 + 9 7 -</tt> might push both 2 and 7.
* The concatenation <tt>3 4 + 9 7 - *</tt> might push 14.

The major difference from other programming paradigms is that concatenative languages have combinatory logic as the predominant computational approach compared to others which use Lambda calculus, or Turing machines.

This approach is possible in other programming languages also. For example the tacit programming style of APL and point free style of haskell is similar to this.
