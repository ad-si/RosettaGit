+++
title = "Talk:Amb"
description = ""
date = 2018-02-09T18:27:22Z
aliases = []
[extra]
id = 2775
[taxonomies]
categories = []
tags = []
+++

==A better sentence?==
"The Amb operator takes some number of expressions (or values if that's simpler in the language) and nondeterministically yields the one or fails if given no parameter, amb returns the value that doesn't lead to failure."

Can someone make this a better sentence? Should it actually be something like:

"The Amb operator takes some number of expressions (or values if that's simpler in the language), and nondeterministically returns the value that doesn't lead to failure, or fails if given no parameter."

--[[User:Mwn3d|Mwn3d]] 21:42, 22 March 2008 (MDT)

Note that the above statements suggest no relationship between the expressions and the values.  It is not possible to comprehend the requirements from the current problem statement -- the programmer must reverse engineer other examples.

I would propose:

:Given a sequence of sets, and some expression which would apply to two elements from two different sets, return some sequence of elements of sets where adjacent pair of elements satisfies this expression and where the order of the sets determines the order of elements in the result.  The sets and the required result would remain unchanged.

(This variant not really great but I believe it is accurate and complete). -- RDM 12:53, 27 August 2009 (EDT)

I've taken the plunge and clarified and expanded the description of the task.[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 15:14, 31 October 2015 (UTC)

==Nondeterministic==

Can we assume, for the purposes of the task, that the pseudo-randomness of "random" numbers available on PCs without external assistance qualify as non-deterministic? --[[User:Short Circuit|Short Circuit]] 22:47, 22 March 2008 (MDT)
:The non-determinism is a red herring. No implementation of Amb I've seen is actually random. The important feature of Amb is backtracking via capturing the continuation. The result is something closer to exception handling. (Can Amb be implemented using only exceptions?)
::non-determinism doesn't mean randomness in computer science. This is clarified in the new task description.[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 15:14, 31 October 2015 (UTC)
:This is an advanced (almost esoteric) concept covered in SICP among other places. See: http://c2.com/cgi/wiki?AmbSpecialForm [[wp:Continuation]]. --[[User:IanOsgood|IanOsgood]] 09:30, 24 March 2008 (MDT)


Nondeterminism has nothing to do with randomness.

Wouldn't it more sense just to call the task something like "nondeterministic choice" and leave it to the implementations to use whatever language feature works best for that? The SICP implenetation uses call/cc and side-effects, but to require a special operator ''amb'' for Prolog, where nondeterminism is in-built, is a bit silly. Python has generators. And it's equally silly in the Haskell example, where ''amb'' just reduces to the identity in the List-Monad. One could emulate the construction in the Continuation-and-State-Monad, but that would be even more silly :-) After all, the article "Replacing failure with a list of successes" was written long after SICP. The List-Monad construction also probably carries over more easily to other languages than call/cc. --[[User:Dirkt|Dirkt]] 02:22, 25 March 2008 (MDT)

What I think the first sentence is trying to say (and also the conventional meaning) is that Amb is a bottom-avoiding choice combinator. 

:: This task and a couple others, unfortunately, have descriptions that are pretty meaningless and I'm not sure it makes much sense for people to speculate what any one of us thinks they're supposed to do. If it isn't clear, then we shouldn't guess - we should eliminate the task and make it the burden of whoever posed it to come up with a new one that actually means something testable. [[User:Sgeier|Sgeier]] 00:28, 6 October 2009 (UTC)

Suppose you have a function <math>f</math> that works fine for some inputs <math>x</math>, but crashes or fails to terminate for others. Similarly a function <math>g</math> works only for some limited set of inputs, not necessarily the same as <math>f</math>. Then <math>\mathrm{Amb}(f,g)</math> is a function that returns either <math>f(x)</math> or <math>g(x)</math> when given an input <math>x</math>, but is guaranteed (!) to choose the one that terminates correctly if one of them does and the other doesn't. 

:: This is a pretty succinct little paragraph that outlines an understandable, verifiable, testable concept. I know how I would implement such a thing in a variety of languages. I know how I would test a given piece of code and verify that it does what you describe. Unfortunately, this paragraph does no bear the slightest resemblance to the actual task description and there is no way for me to know whether this task actually asks for what you wrote there or whether it is asking for something different entirely. Neither do I see any resemblance between your description and any of the things written in the section up there (where the only group of words that appears to form English sentences is about "sequences of elements in sets").[[User:Sgeier|Sgeier]] 00:28, 6 October 2009 (UTC)

Suppose you have a function <math>f</math> that works fine for some inputs <math>x</math>, but crashes or fails to terminate for others. Similarly a function <math>g</math> works only for some limited set of inputs, not necessarily the same as <math>f</math>. Then <math>\mathrm{Amb}(f,g)</math> is a function that returns either <math>f(x)</math> or <math>g(x)</math> when given an input <math>x</math>, but is guaranteed (!) to choose the one that terminates correctly if one of them does and the other doesn't. Once you understand this definition, you'll realize that the only right way to implement Amb for arbitrary unknown functions <math>f</math> and <math>g</math> is by multi-threading or interleaving on some level, which is where the emphasis in this task belongs. (You can't get away with using only exception handlers unless you exclude the possibility of non-termination.) Note that this semantics differs from non-deterministic choice. If <math>f</math> and <math>g</math> happen to have disjoint domains, it's completely deterministic.

--[[User:Sluggo|Sluggo]] 22:20, 6 September 2009 (UTC)
: Non-termination presents a problem for continuations and threads also; they don't solve the issue at all of when a branch of the computation fails by not terminating!  Processing is stuck there, that is all.   Exception handling isn't in an of itself a solution; rather, nonlocal control transfers can support an explicit backtracking solution by providing a convenient direct branch to some earlier "top level" in cases when failure or success are confirmed. 

For my part, and demonstrated by my VBScript entry, I don't think I understood the task at all. I don't think many did. Even after reading all this, I'm still not sure I do. I did have fun, all the same, coming up with the VBScript code. --[[User:Axtens|Axtens]] 08:47, 16 February 2010 (UTC)

== Haskell ==

The code

   unless (joins w1 w2) (amb [])

could be replaced with simply

   guard (joins w1 w2)

I didn't make the change because I don't know if you are trying to point out how to use (amb []).

==C==
Please put a complete C program, that can be compiled as it is.
:We'll need more than that. What's the error you're getting? --[[User:Mwn3d|Mwn3d]] 18:34, 27 June 2009 (UTC)

I've put an alternate C implementation at:

https://github.com/jimwise/shared/tree/master/nondeterminism/c

if that helps -- this uses setjmp/longjmp, and includes a few examples; no external code is required.

== Go ==

I added a solution inspired more by [http://www-formal.stanford.edu/jmc/basis.html McCarthy's original paper] than the SICP presentation.  McCarthy talks about functions which may be "undefined" in some cases and says, "...when the computation process fails to terminate, the result is undefined." (bottom of p. 7 in the PDF.)  The Go version handles this with channels and go routines.

: It seems they've taken down that server. Here's the Wayback Machine link: [https://web.archive.org/web/20120901194931/http://www-formal.stanford.edu/jmc/basis.html McCarthy's Paper via Wayback Machine] --[[User:Rabuf|Rabuf]] ([[User talk:Rabuf|talk]]) 17:09, 7 November 2013 (UTC)
:: It's still there, the wiki markup for the link was incorrect (I've corrected it). &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 14:19, 5 September 2014 (UTC)

He defines amb as a binary operator on x and y where either or both may be undefined.  That is, x and y are separate computations which may or may not terminate.  He talks of functions having possible values (plural) and while he doesn't specify that amb must enumerate its defined values, we assume so because we want to do backtracking.  This binary enumerating amb can be composed into a form that works with multiple values.  The result, in a context of computations that take an unknown amount of time, is a form that may yield a number of values, but perhaps not immediately, perhaps not all at once, perhaps never.  A goroutine supplying values over a channel has this capability.

The function ambString shows this and gives us the enumerating property we need for searching.  It takes a string slice and iterates over it, providing slices of length 1 on the result channel.  If preparing the result slices were truly computations of unknown and potentially infinite duration, preparation of each slice could be done as a separate goroutine, but this would be overkill for the task.  The important part is the result channel.  The function creates the channel, starts the goroutine, and returns the channel without waiting for the goroutine to do anything.  A caller of this function gets the channel back almost immediately, but then should assume that it might have to wait an unknown amount of time for an unknown number of results.

The function ambChain relies another concept of McCarthy's paper, "operator Am(x,π(x)) whose values are all x's satisfying π(x)."  Here we understand that x is a computation with multiple possible values and π is a predicate that selects a subset of them.  In ambChain, the call to ambString in the inner loop is the "x" of Am, and the boolean expression in the if statement on the next line is the predicate π.  Just as the composed amb function of ambString runs as a separate goroutine, this Am operation runs as a separate goroutine.  These goroutines started as the inner loop of the matching operation execute concurrently.  The results of the Am operation are new, longer chains of words which are returned on a channel, maintaining amb behavior.  ambChain has only a single result channel.  The concurrent goroutines all send their matches on this common channel.  The caller of ambChain does not know how many matches might be returned, but ambChain can recognize when it has enumerated all possibilities.  It uses a sync.WaitGroup to track completion of goroutines and can close the result channel as ambString does.

The driver main enumerates the results of chaining the four word lists.  If there were no matches, there would be no output.  If there were multiple matches, it would print them all.  If the computations were lengthy, results would be printed as the computations completed.  The separate goroutines of ambString and ambChain ensure that even if some computations failed to terminate, any computations which could complete would, and return any found matches. &mdash;[[User:Sonia|Sonia]] 02:22, 29 February 2012 (UTC)
