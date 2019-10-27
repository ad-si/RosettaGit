+++
title = "Talk:String append"
description = ""
date = 2013-11-21T18:43:59Z
aliases = []
[extra]
id = 16431
[taxonomies]
categories = []
tags = []
+++

How does this differ from [[String concatenation]]? --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 09:03, 4 October 2013 (UTC)

:This task differentiates those languages that support modify-in-place from those that force a copy (notionally at least).  The other task mandates "create another string variable" whereas this one basically says "try not to create another string variable; modify in place if you can, otherwise show how you'd work around that lack of that ability".  Of course, internally, both solutions may end up copying if you didn't reserve enough space, but it's the mental convenience that is at stake here, just as in C you can say either "a = a + 1" or "a += 1", and it may or may not compile down to the same thing, but the latter represents the simpler idea of an accumulator, rather than forcing the programmer to think of one thing as both an lvalue and an rvalue.  So I'd say this entry is a little more about psychology than the other, which is more about technology.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 15:13, 4 October 2013 (UTC)

::I agree with Tim that there is a useful difference between appending to an existing value and creating a new value with expanded result, but these two tasks don't make that distinction and are in effect duplicates. This task should probably be tagged as the duplicate and later removed. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:22, 4 October 2013 (UTC)
:::I've edited the task to clarify the difference in intent from concatenation.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 17:32, 4 October 2013 (UTC)

:If you look at the code on this page, almost none of the languages are actually operating "in-place".  They are all variants of <code>s = "foo"; s += "bar"</code>, which in the underlying language implementation almost certainly allocates <code>"foo"</code> and <code>"bar"</code>, then allocates a third string and writes <code>"foobar"</code> to it (unless the whole thing is constant-folded at compile time).  The fact that only a single named string variable <code>s</code> is used is merely a syntactical illusion. [[User:Stevengj|— Steven G. Johnson]] ([[User talk:Stevengj|talk]]) 18:41, 21 November 2013 (UTC)
