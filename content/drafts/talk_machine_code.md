+++
title = "Talk:Machine code"
description = ""
date = 2014-12-02T18:06:17Z
aliases = []
[extra]
id = 17033
[taxonomies]
categories = []
tags = []
+++

This nice task still awaits your contributions (at least 4).

I'm pretty sure this task isn't going to get a lot of love because:

* It's architecture-specific.
* It's for a technique that few people use these days in the era of inline assembler, etc.
* It's only really usable for low-level languages (and yes, this includes Lisp: Lisp is a low level language that can be easily built up into high levels).
* It assumes C calling conventions in its very structure.

Quoting some of the guidelines:

* "[The] goal is to address a problem a programmer may face or want to think about"  (I'm pretty sure the age of BASIC programmers POKEing and PEEKing their C64s is long past.)
* "Don't require exceedingly rare features."
--[[User:Ttmrichter|Michael T. Richter]] ([[User talk:Ttmrichter|talk]]) 00:34, 14 January 2014 (UTC)


### D entry

In D, unlike in C, I have performed just a function cast on the memory of the array. Is the memory mapping used in the C entry for portability with operating systems that disallow such operation for safety?
: It is not the cast that is problematic, it's calling the function itself. Modern PCs have a feature called [http://en.wikipedia.org/wiki/NX_bit NX bit] which allows the OS to forbid execution of certain memory ranges. (A lot of other platforms have similar features as well). The general guideline tends to be that the OS forbids execution of any address unless it's known to contain executable code. Therefore, to make this example work, you need to either tell the OS that the memory you are working with should be executable, or you need to allocate new executable memory and copy the code there. Depending on your OS and its settings, your code may or may not crash without these precautions. [[User:Csaboka|Csaboka]] ([[User talk:Csaboka|talk]]) 17:19, 2 December 2014 (UTC)
:: Thank you. I understand well. I have added a note to the D entry using some of your words. Later I may add a second D entry that uses safer executable memory.
