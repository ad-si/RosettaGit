+++
title = "Talk:Even or odd"
description = ""
date = 2016-04-17T15:46:40Z
aliases = []
[extra]
id = 10984
[taxonomies]
categories = []
tags = []
+++

== Silly recursive solution ==
I made this for Java, but maybe it's not ''that'' bad in languages with recursion optimizations:

```java
public static boolean isEven(int i){
	if(i == 0) return true;
	if(i < 0) i = -i;
	return !isEven(i - 1);
}
```
 --[[User:Mwn3d|Mwn3d]] 18:47, 1 December 2011 (UTC)
: I'd be quite surprised if anything optimized that very much, as it depends on applying an operation to the result of each recursive call which is usually a sign that the compiler ''won't'' be able to figure things out. A human could split that into a pair of functions that are the logical inverse of each other (i.e., isEven and notIsEven) which could then admit optimization, but I suspect that sort of analysis isn't done by compilers (on the grounds that it would so rarely lead to real optimizations in practice). –[[User:Dkf|Donal Fellows]] 09:48, 2 December 2011 (UTC)

== definition of even & odd numbers ==
This may be frivolous/trivial, but since the task is to determine if an integer is odd or even, a simple definition of an odd/even number could be in order.

The definition (below) defines an ODD NUMBER, and goes further than limiting the definition to an ODD INTEGER:


From MathWorld:

An   ''odd number''   is an integer of the form   '''n=2k+1'''   where   '''k'''   is an integer.

Integers which are   ''not odd''    are called   ''even''.


The above definition has the advantage that it isn't dependent upon its (say, internal binary) representation (or any base, for that matter), although that is one method to determine evenness/oddness. 

I've also seen the definition that an odd number is an integer, that when divided by two, the absolute value of the remainder is (positive) unity.

For an even number ... the remainder is zero.


[[User:Gerard Schildberger|Gerard Schildberger]] 00:46, 16 March 2012 (UTC)

== PL/I ==

This very short solution works only for Bin Fixed(n,0) variables.
Well i is Bin Fixed(15) by default.

consider also Dec Fixed, Float, and Pic variables
where mod(v,2)=0 -> v is even (if it's an integer)
:--[[User:Walterpachl|Walterpachl]] 06:21, 4 August 2012 (UTC)
