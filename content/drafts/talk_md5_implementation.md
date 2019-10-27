+++
title = "Talk:MD5/Implementation"
description = ""
date = 2010-10-12T20:48:40Z
aliases = []
[extra]
id = 8367
[taxonomies]
categories = []
tags = []
+++

==Ready for Prime Time?==
It looks done.  I'm going to leave it until the weekend and then remove draft unless something comes up here. --[[User:Dgamey|Dgamey]] 10:42, 5 October 2010 (UTC)

== The original MD5 page was not enough ==
After reviewing the original MD5 page and realizing it was all over the map, it seemed reasonable to want to show off native capabilities of languages to meet a well specified and demanding problem.  Most of the solutions on the original MD5 page will not satisfy this task description.

* Autohotkey has a largely native implementation that uses a couple of DLL calls.  However, the core of the algorithm is implemented as native code and meets the intent here.
* C, Liberty  Basic, Matlab (if fixed), Modula3 all seem to satisfy the coding part of the task 
* Common Lisp may qualify if more is shown, as may Forth
* The Python examples use hashlib which uses an external call.  However, there is Python 2 code http://www.equi4.com/md5/ that is native and a direct translation of RFC 1321 with the RSA copyrights (I think a derivative work of an RFC is fair game but haven't actually checked it out).

Basically, I think this is ready but wanted input/review.

I considered adding a 'performance' measure but was wondering how to compare apples to apples with so many different processors around.

--[[User:Dgamey|Dgamey]] 03:08, 29 September 2010 (UTC)
: "Usind native facilities" sounds like "using built-in features" which leads me to believe that if the language has a built-in library for this then that's ok. The examples show the opposite. Maybe change it to "coding the algorithm directly (not using a call to a built-in or external hashing library)" or something like that? The name should probably be changed to "MD5/Implementation" to fit with the link list tasks. --[[User:Mwn3d|Mwn3d]] 12:30, 29 September 2010 (UTC)
:: Changed the wording (clearer is always better).  And moved it.  --[[User:Dgamey|Dgamey]] 13:11, 29 September 2010 (UTC)

: I would like to see a counterpart task, where calling out to an external implementation is a requirement of the task. Then we can migrate the original code examples and retire [[MD5]]. Otherwise, we've just crufted task and created unnecessary overlap. --[[User:Short Circuit|Michael Mol]] 11:46, 5 October 2010 (UTC)
:: Mike I created this in part because I was told that the other task was too old to change.  I was going to remove my code from the old task and point here.  I'm for it BTW but it needs to be precisely defined.  We already have tasks that call other languages, system routines, etc. How would the other MD5 task be different?  What's to stop someone from calling the code they wrote here?  What would be wrong with that?  Just a thought.  --[[User:Dgamey|Dgamey]] 21:25, 5 October 2010 (UTC)
::: If they would ''really'' use their own pure-language implementation of MD5, then there's nothing implicitly wrong with linking to the code over here, as long as long as the "unimplemented in X" requirements are met. That's my primary concern. I'm going to back off on this, though, because every time I try to think through this, I second-guess myself, and I've got too much at work on my plate already. My reply in [[Talk:MD5]] was intended to be explanatory on the history of policies on external libraries, not as a statement on whether or not tasks could be changed or deprecated. --[[User:Short Circuit|Michael Mol]] 22:17, 5 October 2010 (UTC)

== Off-site code ==
The task (and RC) policy requires code to be on this site. While a link to supplemental code offsite is fine, the code satisfying the task should be on this site. --[[User:Dgamey|Dgamey]] 01:46, 30 September 2010 (UTC)

:Ok, I have grabbed the code and pasted a copy here.  However, 

:# I would like a link to this rosetta code policy.  The only policy page I can find is the (very brief) [[Rosetta Code:Privacy policy|privacy policy]] page.  If we have policies here I want to know them!
:# The task requirements did not require the code be present here (and, arguably still do not).  The task said that code present here would be accepted but I understood that to be an inclusive statement rather than an exclusive statement.  When I am fixing a draft to conform with the task specification, I feel more comfortable when the task I am implementing is explicitly stated rather than implicit but not stated (I expect that this might save other people time, later).
:# RFC 1321 includes a license which requires some specific statements when this algorithm is implemented, and a number of the implementations here do not include those statements.

:Thanks!

:--[[User:Rdm|Rdm]] 16:39, 30 September 2010 (UTC)

:: I'm the resident benevolent dictator. I don't know of a whole lot of documented policy. There's a lot of "generally speaking" comments I've made in talk pages here and there, but I don't think we have a singular place where I record these things. Generally, I watch discussions, step in where they need to be, but otherwise let the community shape the site to whatever it meshes with best. Sometimes, as the community changes, the direction and shape needs to change. Still, I don't know if I've spoken on-wiki about this topic before.
::
:: Generally speaking, code examples should be on the site. If they depend on libraries, we don't need the library source code on-site. (See AutoHotKey for an example of that; their libraries are code snippets in their forums.) However, code that's not part of the library should be on-site, with references to the libraries in question. --[[User:Short Circuit|Michael Mol]] 00:44, 1 October 2010 (UTC)

:::Ok... in this case, the code was in the library.  It was the downloadable library (roughly analogous to CPAN in character, though much smaller in scale than CPAN and much faster to install because there is no need for a build nor test phase at install time).  And I am just as confused now as I was before, about what this means in terms of presentation.  --[[User:Rdm|Rdm]] 01:02, 1 October 2010 (UTC)
:::: Presentation is going to depend on the task. If the task disallows deferring the core requirement to a library (generally, it shouldn't, but for some cases it's worthwhile. In those cases, a version of the task that defers to libraries is generally desired.), then the example solving the task should have the code specific to the task problem shown. If the task allows deferring the core requirement to a library (this is the default, as far as I'm concerned; where deferment is disallowed, it needs to be explicitly stated), then all that's needed is showing how to use the library for the purpose.  If that doesn't answer your question, I'm not certain what you mean. --[[User:Short Circuit|Michael Mol]] 02:01, 1 October 2010 (UTC)

:::: Currently, the task says:
:::::* An implementation taken from a native source library and shown (in detail) on this site is acceptable.
:::: however, based on the comments in this section, I imagine that that should be changed to
:::::* An implementation taken from a native source library is acceptable only if it is shown (in detail) on this site. 
::::Or am I completely off base here?  --[[User:Rdm|Rdm]] 12:44, 5 October 2010 (UTC)
::::: I don't understand the distinction. --[[User:Short Circuit|Michael Mol]] 02:34, 6 October 2010 (UTC)
::::: Neither did I but it is now very clear. --[[User:Dgamey|Dgamey]] 18:57, 6 October 2010 (UTC)
:::::: One case in point, one of the original MD5 tasks (Autohotkey?) is largely coded natively but uses a couple of callouts for minor functions.  I would take this as largely meeting the intent of the task as it still shows bit manipulation, etc., etc. --[[User:Dgamey|Dgamey]] 18:57, 6 October 2010 (UTC)
:::::: The distinction is that the first is permissive -- it accepts an implementation on this site for that case, but does not prohibit alternatives (RFC 2119 "MAY" or "SHOULD").  My proposed revision prohibits alternatives (RFC 2119 "MUST").  --[[User:Rdm|Rdm]] 20:48, 12 October 2010 (UTC)


###  Gimmie for C? 

Given that the IETF reference implementation is written in C, should C get a gimmie and use a link to the code? --[[User:Dgamey|Dgamey]] 21:33, 5 October 2010 (UTC)
: No; the fundamental approach of the site is in-line visual comparison of code. Links off-site break that. --[[User:Short Circuit|Michael Mol]] 02:33, 6 October 2010 (UTC)


### Compatible licensing?

The license reads:

```txt
/* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software. */
```


* How compatible is the above with the RC licenses? 
* Do we have to remove the task???!!!
* Would we be in compliance if we add the following to the task description:
 ''Data on this page is derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm''?
--[[User:Paddy3118|Paddy3118]] 17:15, 30 September 2010 (UTC)

:The RFC license seems to require explicit acknowledgement of RSA's contribution when md5 is published.  I am not aware of anything in rosettacode that conflicts with that.  --[[User:Rdm|Rdm]] 17:37, 30 September 2010 (UTC)
::Acknowledged verbatim. --[[User:Dgamey|Dgamey]] 10:40, 5 October 2010 (UTC)

== Debugging - Pseudo Code is not enough ==
Because hash functions are designed to thoroughly mix their inputs, even a small error will result in a completely different hash.  A single bit change should result in half the bits of the digest changing.  So if you encounter errors in your implementation you will likely need to break down constants, each step, sub-function, and round to find the error.

(from MD5 Talk) Just attempting to code this from the pseudo-code is likely to result in a number of errors.  The reference code in the RFC is clearer.  --[[User:Dgamey|Dgamey]] 01:38, 30 September 2010 (UTC)
