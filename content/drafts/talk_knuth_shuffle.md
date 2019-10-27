+++
title = "Talk:Knuth shuffle"
description = ""
date = 2016-11-15T20:14:20Z
aliases = []
[extra]
id = 4217
[taxonomies]
categories = []
tags = []
+++

==Duplicate==

Note: this is a sub-task of [[Playing Cards]]. --[[User:IanOsgood|IanOsgood]] 01:11, 20 May 2009 (UTC)

Well one motivation for this was to have another task other than Knapsack that began with the letter K, also this task is for integers, whereas the Playing Cards task is for strings (or whatever datatype you choose to implement cards).

But they do overlap...perhaps code from this task could be used in Playing Cards?  This task could be made generic I suppose.  Whatever everyone else things. *shrugs* --[[User:Mbishop|Mbishop]] 04:42, 20 May 2009 (UTC)
:I feel for you over the K task thing!
:I had even more trouble over tasks beginning with Y :-)  --[[User:Paddy3118|Paddy3118]] 06:10, 20 May 2009 (UTC)
It's also effectively a subtask of [[Bogosort]]. —[[User:Dkf|Dkf]] 10:01, 20 May 2009 (UTC)

== BASIC Integer Array? ==

The BASIC example I just entered doesn't do an integer array; instead, it's code I wrote specifically for shuffling cards. (shrug) I would've put it under the Playing Cards page, but it's just too simplistic, and I didn't feel like dumbing it down any further. (I wrote it to show someone else how to shuffle several ago.) -- [[User:Eriksiers|Eriksiers]] 01:25, 11 August 2009 (UTC)
:Would you try and make the code work with integers? It makes it hard to compare entries otherwise. Thanks.--[[User:Paddy3118|Paddy3118]] 06:20, 11 August 2009 (UTC)
::Sorry, didn't think of that. Done. -- [[User:Eriksiers|Eriksiers]] 14:44, 11 August 2009 (UTC)

== F# ==

I just happened to notice the [[Knuth shuffle#F#|F#]] entry starts off with <code>open System</code> twice. Seems a bit odd to me, but I don't know ''anything'' about F# -- it just ''looks'' weird to me. Is this right? -- [[User:Eriksiers|Eriksiers]] 22:18, 24 December 2009 (UTC)

: Anybody? Anybody? Bueller? Bueller? -- [[User:Eriksiers|Eriksiers]] 18:59, 15 January 2010 (UTC)

:: Good question. I know zilch about F# as well. -- [[User:Rldrenth|Rldrenth]] 04:02, 16 January 2010 (UTC)

== C# ==

The C# code had a serious problem in its implementation. It selected a random position from the *entire* array on each loop. This is a serious problem as it leads to a non-uniform distribution of outcomes, thereby ruining the purpose of the algorithm. Wikipedia has a nice description of the problem [http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#Implementation_errors here].

== C++ ==

The C++ example shows <code>std::random_shuffle</code>; however, <code>std::random_shuffle</code> is deprecated in C++14 (and removed in C++17) in preference to <code>std::shuffle</code>.  The conundrum is that <code>std::shuffle</code> isn't available until C++11, since it uses the new random number generation facilities that arrived with C++11. In general, <code>std::random_shuffle</code> and <code>std::shuffle</code> is a bit of a mess.

Does Rosetta Code need a pre-C++11 and post-C++11 language?  So many things have changed with the latest standards, and C++ is now actually a moving target.  (Reference: http://en.cppreference.com/w/cpp/algorithm/random_shuffle ) --[[User:Intvnut|Intvnut]] ([[User talk:Intvnut|talk]]) 06:36, 3 June 2015 (UTC)

:Standards change things in a lot of languages. If there is still a lot of users of an old standard and an example would be significantly different between standards then you could show them both - or '#ifdef ...' to accommodate both, or just add a comment stating what the change would be. Whatever is neatest. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:57, 3 June 2015 (UTC)

== Haskell ==

The haskell example doesn't run under GHC 7.8.3 and prints the following errors:
knuthshuffle.hs:9:39: Not in scope: ‘l’

knuthshuffle.hs:9:47: Not in scope: ‘x’

: This has been fixed, code now compiles and works as intended (GHC 7.8.4).


==Formulae hidden to most browsers after under-tested edits at 09:03, 5 September 2016 ==

Some under-tested task description edits made to the task page at 09:03, 5 September 2016, particularly involving the use of &lt;math&gt; tags, have left some elements of the task description formulae invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:59, 22 September 2016 (UTC)

: Repaired - pseudocode expressions should now be fully visible in all browsers [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:14, 15 November 2016 (UTC)
