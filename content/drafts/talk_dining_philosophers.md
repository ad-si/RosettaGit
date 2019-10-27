+++
title = "Talk:Dining philosophers"
description = ""
date = 2019-03-09T00:37:23Z
aliases = []
[extra]
id = 3106
[taxonomies]
categories = []
tags = []
+++

== grabbing forks ==
'''Python solution''' "If a philosopher acquires one fork but can't acquire the second, he releases the first fork before waiting to acquire the other (which then becomes the first fork acquired)."

Without some additional actions this solution is exposed to livelock. That is far less probable than deadlock, yet it may happen.

'''Livelock''': all philosopher grab one fork. Then they see that another one is out of reach. They drop their forks one by one, and then start to grab other forks, in the same order. Under a scheduling policy that keeps philosophers to act in this order, this circle will repeat itself forever. Even if the circle gets actually broken, it is actually a sort of busy waiting, which is not good.

One possible solution to this could be to introduce some policy that would prevent the same fork from being dropped when it was once acquired through waiting, like in the [http://en.wikipedia.org/wiki/Dining_Philosophers dirty/clean forks solution] --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:07, 8 November 2008 (UTC)

: Yes, it is possible for this solution to experience livelock. However it can be shown that on a single processor system where threads are timesliced through the processor, that livelock can not occur if the locking loop can execute completely within one timeslice.  ( Try acquiring second lock, if fail release first lock, swap forks, loop, acquire first lock ). Since a timeslice is likely to be far longer than the time to execute this loop, you're probably OK. But you would indeed need to guarantee this in your real life system. --[[User:Rldrenth|Rldrenth]] 21:17, 12 May 2009 (UTC)


There should be examples of output ; for the novice members. --[[User:Arkapravo|Arkapravo]]19:43 , 8 April 2009 (GMT)
: Output does not add too much information: you do not achieve the task by looking at the output (that theoretically is not predictable); an example of run of the python code on my system gave:


```txt
Russel is hungry.
Russel starts eating 
Aristotle is hungry.
Marx is hungry.
Marx swaps forks
Kant is hungry.
Kant starts eating 
Budda is hungry.
Russel finishes eating and leaves to think.
Aristotle swaps forks
Marx starts eating 
Kant finishes eating and leaves to think.
Budda swaps forks
Aristotle starts eating 
Marx finishes eating and leaves to think.
Budda starts eating 
Russel is hungry.
Russel swaps forks
Aristotle finishes eating and leaves to think.
Russel starts eating 
Budda finishes eating and leaves to think.
Marx is hungry.
Marx swaps forks
Russel finishes eating and leaves to think.
Marx starts eating
Aristotle is hungry.
Aristotle starts eating 
Kant is hungry.
Marx finishes eating and leaves to think.
Budda is hungry.
Budda starts eating 
Russel is hungry.
Russel swaps forks
Budda finishes eating and leaves to think.
Aristotle finishes eating and leaves to think.
Kant starts eating 
Russel starts eating 
Budda is hungry.
Marx is hungry.
Marx swaps forks
Aristotle is hungry.
Kant finishes eating and leaves to think.
```


: And so on. --[[User:ShinTakezou|ShinTakezou]] 23:14, 11 April 2009 (UTC)

== Tcl example? ==

I'm having trouble understanding why/how the TCL example avoids deadlock. Could someone add a few words showing that?

:It works because each philosopher picks up a lower numbered fork before picking up a higher numbered fork.  (When each thread starts, the forks are placed in order according to their number) The thing about this solution is that some philosophers have a slight advantage of acquiring both forks over other philosophers.  It can't deadlock because in order for deadlock to occur, one of the philosophers would have to pick up a higher numbered fork before attempting to pick up a lower numbered fork.  You can't have the situation where all the philosophers have each picked up one fork.  At least one of them will not be able to acquire any forks (most likely the one who uses forks 4 and 0)- rldrenth

== Contradictory ==

   The question is contradictory.
"When a philosopher cannot grab both forks it sits and waits."
Since he puts the forks down at the same time, 
he will never be holding only one fork.

: What is the contradiction? (A philosopher waits until both forks are available, and then picks them up together, when done, puts them down together. If this were contradictory you should explain what parts of it contradict with which other parts, shouldn't you?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:13, 28 May 2013 (UTC)

  What am I missing?  
He can't pick up only one fork.  He can't put down only one fork.
Yet, "here exist two deadlock states when all five philosophers are sitting at the table holding one fork each."
So, how can a philosopher be holding one fork?

: Oh, yes, I should have read the task. You are right - the task is wrong. Thinking about what the problem is trying to express: philosophers are allowed to pick up individual forks (and, in fact, can only pick up one fork at a time). They just can't eat anything unless they have two forks.

: Paraphrasing: given the reality that a philospher can only pick up one fork at a time, how can we emulate the ideal that philosophers always use two forks together. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:37, 4 June 2013 (UTC)
