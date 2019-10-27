+++
title = "Talk:Montgomery reduction"
description = ""
date = 2014-04-27T16:37:11Z
aliases = []
[extra]
id = 11087
[taxonomies]
categories = []
tags = []
+++

== Draft ==
This is my first wiki entry
Please send criticisms and suggestion for this code to me --User:Mahaju december 19
Thank you

: Hi.  Good start for a first task, I'll make some observations.  Please take them as constructive.
:* please sign your talk pages.    (checked - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
:* perhaps a reference to [[wp:Montgomery_reduction]] would help.  You might even want to borrow and cite some text.   (checked - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
:* Good ref to numerical recipes.   (still trying to find some - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
::: Oops, mental shot circuit - I meant Handbook of Applied Crypto (HAC) - sorry. --[[User:Dgamey|Dgamey]] 03:04, 21 December 2011 (UTC)
:* not sure if there are any copyright issues, say with the pseudo-code    (how do I find out about such things? - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
::: Well if it came from HAC it's likely got a copyright of some kind.  --[[User:Dgamey|Dgamey]] 03:04, 21 December 2011 (UTC)
:* Task descriptions should be as non-language specific as possible.  Once there are a bunch of examples the C++ reference in the description won't make sense.   (will try to edit this in future - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
:* On a quick read over I didn't get any sense of what this was and what is was useful for.  Always a motivating factor.          (I cannot do a detailed tutorial at the moment so have referenced the book - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
::: That was why I suggested possibly borrowing and citing from WP.  --[[User:Dgamey|Dgamey]] 03:04, 21 December 2011 (UTC)
:* Is this a duplicate/strong overlap of [[Modular_exponentiation]].  There should probably be cross-links at least.   (No, but is somewhat related - --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC))
:That's all.  Good luck. --[[User:Dgamey|Dgamey]] 13:18, 20 December 2011 (UTC)

: Made some minor changes to talk page and wiki page --[[User:Mahaju|Mahaju]] 01:40, 21 December 2011 (UTC)

:: I suggest that users not interleave comments. In the above text, I cannot know whether each word is by Dgamey, or by Mahaju. When replying to a comment, please write the reply below the comment, not in the middle of the comment. --[[User:Kernigh|Kernigh]] 00:40, 22 December 2011 (UTC)

: I've adjusted the page text so that the task is more clearly separated from the implementation of it; the key point is that we want the text to still work when there are many implementations in different languages. (For example, it's obvious that a particular language's implementation is in its own named section so you don't write that in the header material.) I've also made more use of <nowiki><math></nowiki> tags; the basic operations are mathematical after all. But don't worry that these changes were made; it takes practice to write perfect pages, and that you get by working at it and being helped by those who go before you. –[[User:Dkf|Donal Fellows]] 18:09, 22 December 2011 (UTC)

== No overlap with [[modular exponentiation]] ==
In the previous section, someone wrote that this task might overlap with [[modular exponentiation]]. There is no overlap. Montgomery reduction involves ''a<sup>-1</sup>'', the [https://duckduckgo.com/?q=modular+multiplicative+inverse modular multiplicative inverse] of ''a''. [[Modular exponentiation]], as we have it, can only do ''a<sup>b</sup>'' when ''b'' &ge; 0. --[[User:Kernigh|Kernigh]] 00:40, 22 December 2011 (UTC)

== Example numbers? ==

I would like to see some numerical examples. --[[User:Rdm|Rdm]] 15:04, 23 December 2011 (UTC)

: So, I posted some, ... [[User:Sonia|Sonia]] 22:34, 23 December 2011

: Ok... based on my current understanding (which, admittedly, doesn't have much time invested), if those numbers are right, I think that the task description should change:

:: <math>M</math> should become <math>m</math>
:: <math>n</math> = number of digits in base <math>m</math> should become <math>n</math> = number of digits <math>m</math> needs in base <math>b</math>

--[[User:Rdm|Rdm]] 15:49, 1 August 2012 (UTC)

:: However, even with these changes, it's not clear to me how your numbers correspond to the task description.

:: Present in task description and not in numbers:  A, u<sub>i</sub>, m', possibly t

:: Present in numbers but not in task description: x1, x2, possibly t1, t2 

:: In other words, it's not clear how x1 and x2 are related to the terms described in the task.  (And it doesn't help that I do not understand go well enough to read lines with what look like three argument operations are doing -- things like "x.Sub(x.Lsh(x, n), m)".

:: Note also that I can compute the desired results directly, it's figuring out what the task description is asking for that I am having problems with. --[[User:Rdm|Rdm]] 18:41, 3 August 2012 (UTC)

I'd like to concur; this task needs something in it so that we can verify the correctness of our implementations of it, even if by just comparing the outputs of different languages. (No we don't need ''exact'' equality; just comparable enough for people, not computers.) Without that, I would be unhappy with promoting this to full task status. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:37, 27 April 2014 (UTC)

== Relative Timings ==

... plus some stuff the task doesn't call for, to show some possibilities for how the task might be developed further.  Of course the timings have no place in a final version but I left them in to show that it's hard to beat library functions, even if they do use mod.  (I checked the Go library source for Exp, and it does simple binary exponentiation calling a mod function at each step.)  Also, this is an example with b = 2.  I suspect that a library implementation of Montgomery reduction would have b = the machine word size, and that it would do Montgomery multiplication rather than just reduction. &mdash;[[User:Sonia|Sonia]] 22:34, 23 December 2011 (UTC)

:: ''The task is not written procedurally, but it does state that <code>m_dash</code> is pre-computed.  So timings which incorporate the computation of m_dash with the computation of the final result conflict with the task definition. --[[User:Rdm|Rdm]] 17:35, 2 January 2012 (UTC)''

: Well, you can't make a code vs. library comparison like this, just as you can't compare apples to Yorkshire Terriers.  A serious implemention should have access to the innards to the <code>big</code> (your <code>math/big</code>--is that the weekly instead of release build?) package, which means it should be in nat.go and directly operate on the bits of the machine word slices.  As presented, the line <code>a.Rsh(a, 1)</code> alone is enough to kill performance, not to mention <code>a.Add(a, m.m)</code>. --[[User:Ledrug|Ledrug]] 05:28, 24 December 2011 (UTC)
:: As long as both the code and library tests were done on the same hardware, comparison timings shouldn't be a problem. The real difficulty with timings comes when people try to use results from different environments. (One could even get rid of 'time' output entirely and only display relative percentages for in-example comparison) As for identifying which version of dependent software is used, that's what {{tmpl|works with}} is for.  --[[User:Short Circuit|Michael Mol]] 15:38, 24 December 2011 (UTC)
: As Ludreg points out, a fair comparison, one that might ultimately show an advantage to Montgomery reduction, would be involved.  The task probably shouldn't go in that direction, so my example of exponentiation is probably too much as well.  Just showing a single multiplication would be enough to indicate that a particular implementation is correct.  Is that enough for the task?  Do people want to see the algorithm for b>2, or even arbitrary b, as in the C++ solution?  I used big numbers, since that is the application for Montgomery reduction.  Should that be a task requirement or is a solution showing m=97 and R=100 enough?  &mdash;[[User:Sonia|Sonia]] 17:54, 24 December 2011 (UTC)

: (Go has a 1.0 release coming up soon.  I've been coding for that and will begin using the works with template when Go 1 is out.)  &mdash;[[User:Sonia|Sonia]] 17:54, 24 December 2011 (UTC)
:: Thank you for adding implementation and numerical example in another language. A solution for small numbers such as m=97 and R=100 while finding just the Montgomery Reduction of one number would probably only show that that given source code has correctly implemented the algorithm. The practical advantage of Montgomery reduction is that if there are a lot of multiplications to be done, it is more efficient to perform them on numbers that been reduced, and the final results can then be reconverted back to normal numbers (we do not need to actually divide by the modulus). And by the way how did you do numericals involving such large numbers? I don't know much about Go, is it possible to do such large number operations in Go as basic data types? Do you have any idea of doing this for large numbers in C or C++?--[[User:Mahaju|Mahaju]] 11:47, 25 December 2011 (UTC)
: See [[Arbitrary-precision_integers_(included)]] for examples of big numbers in C and C++.  Go has big number support in the standard library. &mdash;[[User:Sonia|Sonia]] 18:12, 25 December 2011 (UTC)
::Thank you very much for that helpful post. A general question if you don't mind. Why am I not getting any email notifications, or any kind of notifications at all when new contents are added to the pages that I have created or edited? If I get involved with a lot of pages in the future I need to have some way of knowing which pages have been edited since last visit, without having to go look at each of them. And yes, I have checked "Watch this page" for the pages I am interested in, for example, this Talk Page. However, I have not received any kind of notification for any edit's that were made to this talk page, nor did I receive it when the original Montgomery Reduction page that I created was moved. How do I enable notifications? Thank you.--[[User:Mahaju|Mahaju]] 03:05, 26 December 2011 (UTC)
::Also, Happy New Year to you all--[[User:Mahaju|Mahaju]] 03:09, 26 December 2011 (UTC)
----
--[[User:Mahaju|Mahaju]] 03:09, 26 December 2011 (UTC)
