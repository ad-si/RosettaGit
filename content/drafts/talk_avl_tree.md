+++
title = "Talk:AVL tree"
description = ""
date = 2016-07-17T08:33:43Z
aliases = []
[extra]
id = 17646
[taxonomies]
categories = []
tags = []
+++

==The Existing C++ and Java Versions (i.e. not the elaborate versions)==
The C++ code present on the main page does not define a non-generic base class for node. This means that the code regenerates the balancing routines for each data type supported by the generic. Also, the code descends the tree to calculate the node balance for each of the rotations (which is a real no-no). This would heavily impact balancing performance. The elaborate version is completely different and the methods to rotate don't update the balance factor (which incidentally is an enum). The Java version that is present on the main AVL Page (not the elaborate version) suffers from the same problem in that it also descends the tree during rotations to calculate the balance.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:57, 13 July 2016 (UTC)
: It's just example code, it's not production quality code. These codes are only intended to demonstrate the principle of an AVL tree. Secondary features such as input validation, error handling, cloning, generics etc, are often relaxed or omitted in RosettaCode entries, so as not to distract from primary features (and also to keep code length managable). [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 13:34, 13 July 2016 (UTC)
:: The code for the elaborate version is production standard. Its a bit longer but it is perfectly correct. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 18:14, 13 July 2016 (UTC)
::: I appreciate that, but you can't hold other entries on this wiki to the same standard, because many of them are deliberately simplified.  [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 18:30, 13 July 2016 (UTC)
:::: I did some testing and the abbreviated versions of C++ and Java have worse than O(N<sup>2</sup>) instead of O(log N) on insertions and deletions. The C++ commentary made the point itself that it made a second 'pass' to balance the tree. It is this that is killing its performance. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 13:42, 15 July 2016 (UTC)

==New C++ AVL==

I added the source code of AVL Trees in C++. This source code is more than 20 years old now - although it did get heavily modified in 2006, right after the C# code was created. I'll leave it to others to port this C++ code to other languages like D.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 11:45, 10 July 2016 (UTC)

I added a Managed C++ version in place of the existing code. I expect it will be moved to its own page as it is rather lengthy. [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 10:37, 13 July 2016 (UTC)

: I think the commentary from the discussion page is best placed on the task page itself, otherwise not many people are going to see it. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 11:12, 13 July 2016 (UTC)

==Java AVL==

I added the source code of AVL Trees in Java.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 13:13, 9 July 2016 (UTC)
: Your code is rather long, I've moved it to a separate page. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 14:59, 9 July 2016 (UTC)
:: Thanks [[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 11:42, 10 July 2016 (UTC)

==Here nor there==
I coded the Go solution from code at http://www.eternallyconfuzzled.com/tuts/datastructures/jsw_tut_avl.aspx.  There is complete working code there in C, it's public domain, and I found it relatively easy to port.  (I found this site from a comment in Chromium source that said they used it.)  The tutorial claimed that the version was relatively compact, as AVL code goes, which seemed good for RC, especially since AVL trees are a little on the complex side as RC tasks go.  I resisted the urge to compact or optimize the code more, which certainly could be done.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 22:39, 22 May 2014 (UTC)

: As long as the copyright issues aren't going to bite us, that won't be a problem. I advise ''not'' heavily optimising the code, unless it also makes the code easier to understand as a general programmer (of Go, of course; don't worry too much about general programmers of some other random language).  We don't aim for optimal solutions here precisely because they tend to be so hard to read, understand and compare. (We also ''can't'' compare performance between languages; matching hardware and language implementation optimisation levels is utterly impossible between different developers'  systems. Let some other site worry about that.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:40, 31 May 2014 (UTC)

:: There is some new C#, C++ and Java code, maybe you could port that Donal.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:24, 10 July 2016 (UTC)

==Basic operations==
The Go code I'm posting today just implements the operations in the original C, an insert allowing multiple keys and a delete that returns no status.  Should the task be more specific than “basic operations?”  In particular there is no find in my code.  Find first, find last, find previous, find next, other flavors of insert and delete might be interesting.  Wikipedia mentions find previous and find next as interesting.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 22:39, 22 May 2014 (UTC)

: The minimum operations have got to be <code>insert</code> and <code>lookup</code>, but <code>remove</code> and <code>print</code> (of the tree) are a good idea too. In fact, for an RC task I'd say that <code>print</code> is vital. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:40, 31 May 2014 (UTC)

==Completing the task description==
The task should say what basic operations to implement and say to demonstrate them somehow.  Demonstrate each operation should be a minimum, but it might be nice to specify some data to insert, delete, and so on, as a way of assessing correct results.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 22:39, 22 May 2014 (UTC)

: Have a care, as that can easily invalidate other people's solutions, which can make them a little grouchy. Or at least it does so to me. ;-) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:40, 31 May 2014 (UTC)

:: Of what I've seen, it can make them very   ''very''   grouchy.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:04, 15 July 2016 (UTC)
