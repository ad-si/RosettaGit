+++
title = "Talk:Modular arithmetic"
description = ""
date = 2019-09-12T19:18:05Z
aliases = []
[extra]
id = 13095
[taxonomies]
categories = []
tags = []
+++

== using (or not) existing libraries==

The task description begins with:
<blockquote>''“<small> The purpose of this task is to show, if your programming language allows it, how to redefine operators so that they can be used transparently on modular integers. You can do it either by using a dedicated library, or by implementing your own class. </small>”''</blockquote>
This is a nice interesting task description - although it would be much more interesting 
if one  can't use a library. The interesting part in my view is how to redefine 
operators. Allowing implementations to use a library hides the interesting part. 

The task description ends with:
<blockquote>''“<small> It is important that the function f is agnostic about whether or not its argument is modular. It should behave the same way with normal and modular integers. In other words, the function is an algebraic expression that could be used with any ring, not just integers. </small>”''</blockquote>
For languages that represent elements in, says, Z/5Z as the integers 0,1,2,3,4 there is no way to
distinguish an element of  Z/5Z from n element in Z.  That is, one is forced not to use existing libraries.
This will make functional solutions to this task look longer than object oriented languages.

I therefore suggest that ''all'' solutions should show how to redefine operators/numbers without
using existing libraries.
<div><small>''(This was written by [[User:Soegaard|Soegaard]] ([[User_talk:Soegaard|Talk]] | [[Special:Contributions/Soegaard|contribs]]) at 21:22, 11 March 2013‎)''</small></div>

:Even using a library is not without interest imho, as it shows the reader how the library is named and how it is used.  For instance, it's not so easy to find out the name 'Mod::Int' when you never heard of it.
:As for redefining operator, I guess it could be a task by itself.--[[User:Grondilu|Grondilu]] 22:32, 11 March 2013 (UTC)
