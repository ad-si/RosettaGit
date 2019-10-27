+++
title = "Talk:Binary search"
description = ""
date = 2017-12-15T16:38:55Z
aliases = []
[extra]
id = 2208
[taxonomies]
categories = []
tags = []
+++

== PARI/GP example ==
The PARI/GP example, I find it to be insufficient.  omg triggered.  Lol...  WaTcH mEhh cOdE oNe UPPPPPP.  I'll derive it from the N/t/roff example. :P  By the way, there is a URL somewhere at the bottom and my Internet is literally 56K dialup.  So, I have removed that URL out so I won't get that slow-loading CAPTCHA box.  I will regret this edit when I'm not a teenager.  I promise.

Haha!  Looks like we created the [[Binary Search|same task]] around the same time. --[[User:Short Circuit|Short Circuit]] 23:46, 7 November 2007 (MST)
:I guess we did. Sorry for the mess I made initially...it was the first time I made a new page for a task. --[[User:mwn3d|mwn3d]] 29:21, 8 November 2007 (EST)
::Nah...You did a great job.  Most of the changes I made were enhancements to the normal system, anyway. --[[User:Short Circuit|Short Circuit]] 08:46, 8 November 2007 (MST)


== Binary search or Guessing game ==
It seems to me that PHP version implements the guessing game, not binary search.
I am not sure about all the other languages. --[[User:PauliKL|PauliKL]] 10:11, 3 November 2008 (UTC)
:Marked incorrect. Needs to search an array, not guess a number. --[[User:IanOsgood|IanOsgood]] 12:46, 3 November 2008 (UTC)
::I just made an attempt at fixing it (no PHP experience...used [[Retrieving an Element of an Array]] and copied a check from the Java example). I also added a check to see if the search is complete (start and end have passed each other). Double-check it for me. --[[User:Mwn3d|Mwn3d]] 16:21, 3 November 2008 (UTC)
::: It is correct, even though the variable names (guess, secret...) are a little bit confusing; but in this regard, the fact that the task talks about the guessing game is confusing too (not too much anyway, but the task could be the same also without citing the game as analogy). --[[User:ShinTakezou|ShinTakezou]] 11:32, 17 May 2009 (UTC)

== Python ==

```txt

if l[mid] > value:
  high = mid
elif l[mid] < value:
  low = mid
why not high=mid-1
and low=mid+1
? --~~~~

```


== Recursive binary search in C ==
Is there any reason why <code>n</code> is passed as argument in <code>int bsearch_r (int *a, int n, int x, int i, int j)</code> ? --[[User:Natema|Natema]] ([[User talk:Natema|talk]]) 05:31, 10 February 2016 (UTC)
: I agree with your hint - that parameter does look to be unnecessary. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:42, 10 February 2016 (UTC)
