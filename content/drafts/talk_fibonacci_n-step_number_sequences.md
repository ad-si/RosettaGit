+++
title = "Talk:Fibonacci n-step number sequences"
description = ""
date = 2013-04-28T17:58:36Z
aliases = []
[extra]
id = 11779
[taxonomies]
categories = []
tags = []
+++

==Lucas sequence redirect==
I can't remember how to do a redirect. Could someone create and redirect [[Lucas sequence]] to this task, thanks. --[[User:Paddy3118|Paddy3118]] 05:55, 25 May 2012 (UTC)
:Got it for you. --[[User:Mwn3d|Mwn3d]] 13:17, 25 May 2012 (UTC)

::Ta! --[[User:Paddy3118|Paddy3118]] 18:48, 25 May 2012 (UTC)

==Naming of sequences==

### octonacci vs. octanacci

(spelling).  The term '''octonacci''' seems to be a misspelling.  It should be '''octanacci'''. 
The misspelling in the OEIS database entry A104415 is (most likely) being changed as I typeth (as it was just edited for correction by me). -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:20, 25 May 2012 (UTC)

:Hi Gerard, I read it the other way round. From [[wp:Number_prefix#Greek_series|greek prefixes]], if we have tri-pod and octo-pod and we have trib'''o'''nacci and not trib'''a'''nacci, then surely we need oct'''o'''nacci as well. Google [http://www.googlefight.com/index.php?lang=en_GB&word1=octonacci&word2=octanacci fight] shows that there is some confusion but even here, the octonacci term seems to 'win'. --[[User:Paddy3118|Paddy3118]] 18:43, 25 May 2012 (UTC)

:I wouldn't sweat it. [http://phrontistery.info/numbers.html This reference] shows both oct, octa and octo in use!? --[[User:Paddy3118|Paddy3118]] 18:48, 25 May 2012 (UTC)

::: Yeah, in my REXX calculator, I accept both names.  OEIS uses octanacci (with a misspelled ''octoancci'' reference that points to the ''octanacci'' sequence), and Wolfram MathWorld &#8482; doesn't mention either one.  It's like the word ''hexadecimal'' --- it's wrong (mixing Greek with Latin), but it's too late to change it now.  The common usage is so ingrained that everybody uses it and knows what it means.  I suspect it will be with '''octo''' & '''octa'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:09, 25 May 2012 (UTC)

:::: This just in ... the OEIS editors corrected the "misspelled/inconsistant" word (if only to be consistant with the other uses of the ''octanacci'' words, but they mentioned that both terms appear to be correct.  That's good enough for me. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:14, 25 May 2012 (UTC)

:::::Yay! I get a kick when I have a paw in something getting fixed at a site like OEIS. --[[User:Paddy3118|Paddy3118]] 19:26, 25 May 2012 (UTC)


### undecanacci numbers

this is the name of Fibonacci 11-step numbers. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:20, 25 May 2012 (UTC)


### dodecanacci numbers

this is the name of Fibonacci 12-step numbers. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:20, 25 May 2012 (UTC)

==task clarification==

In the first part of the task description: 


: 1. Write a function to generate Fibonacci n-step number sequences ''given its initial values'' and assuming the number of initial values determines how many previous values are summed to make the next number of the series.
(italics were added by me).

Now then, do you want us to assume the initial values for any given sequence are already known (assummed), or are we (or the requestor) to supply the initial values, and then from the counting of those values, assume that's what Fibonacci sequence is wanted?

I assummed by the verb tense ''given'' that the sequence is already known and has its initial values (but can be overridden by the requestor), but from the other verb tense ''determines'', it appears you want us (or the requestor) to specify the initial values.

I took the later approach and had the requestor specify the initial values, and from that, deduced which Fibonacci sequence the requestor wanted.  -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:59, 25 May 2012 (UTC)

:Hi Gerard, the idea was to describe what I did in the Python :-)
 That is to create a function/class/... that you first call with only the necessary initial values of a series and get back something that will generate members of the particular series defined by the initial values (the number and order of them as well as their particular values). --[[User:Paddy3118|Paddy3118]] 03:08, 26 May 2012 (UTC)

== Base case 0 ==

When I learned the Fibonacci sequence, we always used the base case [0, 1] (i.e. k=0 is 0, k=1 is 1), then it follows: 0 1 1 2 3 5 8... So the [0, 1] base case is one place "before" the [1, 1] base case that other people think of. Wikipedia defines F_0 = 0 and F_1 = 1, so this is really an issue of whether to start the sequence at k=0 or k=1. I always thought that [0, 1] was more elegant.

Looking at the other sequences here, the 0 also works. For n = 3, [0, 1, 1] produces the same sequence (with the additional 0 at the beginning). For n = 4, [0, 1, 1, 2] produces the same sequence, and so on. --[[User:Spoon!|Spoon!]] 01:38, 26 May 2012 (UTC)

:This [http://www.math-cs.ucmo.edu/~curtisc/articles/howardcooper/genfib4.pdf reference, (pdf)] gives the zero too but I had found the was I reported earlier and just stuck with it. If you were to just make a note of the change in 'base' at the head of any example using this alternative then it should not be a problem. --[[User:Paddy3118|Paddy3118]] 03:21, 26 May 2012 (UTC)

==Racket function fib-n? ==
Hi Soegaard, the Racket function <code>fib-n</code> does not fit the task description (part 1); although <code>fib-list</code> does. Unfortunately you seem to have used <code>fib-n</code> for some of the output which means that with only a slight change you can make it fit the task description. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:21, 28 April 2013 (UTC)

:Hi Paddy,
:I changed the test to display the whole table.

Ta! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:58, 28 April 2013 (UTC)
