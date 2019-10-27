+++
title = "Talk:MD5"
description = ""
date = 2014-04-10T03:25:06Z
aliases = []
[extra]
id = 2329
[taxonomies]
categories = []
tags = []
+++

What is the point of this? Most languages are going to have an MD5 function.

Perhaps the algorithm should actually be implemented in each.  This would give a much better rosetta like comparison between languages, as it would involve more complicated structures than straight code blocks.

:Someone asked for MD5 on the [[Help:Request a new programming task|task request page]] and I looked up how to do it in Java. I couldn't really tell what was going on in the wikipedia explanation of the algorithm. If you know how to do the algorithm for real, then go for it. --[[User:Mwn3d|Mwn3d]] 12:36, 28 November 2007 (MST)

:This really should be a task to encode the algorithm.  There are other tasks that demonstrate calling functions and external routines.  I thing the original task description intended this, unfortunately they didn't say it clearly.  I really do think all of the tasks not in the spirit should be marked as needing improvement if not as outright incorrect.  However, since they've been allowed to sit for so long marking for improvement is probably enough. --[[User:Dgamey|Dgamey]] 03:33, 28 September 2010 (UTC)
:If an implementation of the task uses a native library then the code should be included here or excerpted (rather than just an offsite link as has been observed before) --[[User:Dgamey|Dgamey]] 04:00, 28 September 2010 (UTC)
::The task was created before we had a policy on use of external libraries. (Heck, it's three years old; it's ''going'' to be crufty.) What I tell people, now, is that if a programmer in a language would use an external library to solve a problem, then that's just fine; if use of a language involves using libraries, than that's ''de facto'' idiomatic for that language. If they wouldn't, that's also fine. Side-by-side examples comparing native and library solutions are fine, too; what isn't done in a library demonstrates native constructs. If we really want to see implementations of MD5 hashing, create a task specific to implementing it. (Actually, that might even bee a good opportunity to try the new [[Rosetta Code:Add a Task]] guideline page.) --[[User:Short Circuit|Michael Mol]] 10:53, 28 September 2010 (UTC)

==Shouldn't this be...==
...in a "Hashing" category, not encryption. The MD5 algorithm, although related to cryptography, is not an encryption function. The output of the MD5 function can not decrypted to yield the original plain text given an arbitrary cipher text (a.k.a hash).

==Pseudo Code is not enough ==
Just attempting to code this from the pseudo-code is likely to result in a number of errors.  The reference code in the RFC is clearer.  Also the task should include validating your implementation with the test values from the RFC.  Some of these will break implementations if you don't get your padding just right.  At this point this would likely have to be optional.  --[[User:Dgamey|Dgamey]] 04:00, 28 September 2010 (UTC)

== The 'C' code had a memory leak. ==

In the middle of the 'C' sample md5 function, a memory buffer is malloc'd which was never released.  I added code at the end of the function to free this msg2 buffer.
