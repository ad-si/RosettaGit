+++
title = "Talk:Self-describing numbers"
description = ""
date = 2013-01-10T12:07:08Z
aliases = []
[extra]
id = 9569
[taxonomies]
categories = []
tags = []
+++

== What do we do? ==
Hi,

What is asked for in this task?

What does someone need to do to fulfil the goals of the task?

At the moment this reads like an encyclopedia entry rather than a task description. --[[User:Paddy3118|Paddy3118]] 12:33, 8 May 2011 (UTC)

: I believe that using a lookup table doesn't fullfill the intent of the requirement of a task, given that the task states what a self-describing number is and/or how to determine that if a number is one.  Seeing if a number is in a simple list is another deterministic method entirely. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:40, 2 August 2012 (UTC)  

: If a task where to ask to determine (or show a list) if a number is a Fibonacci prime, and all that was done was to check if the number was in a pre-defined list.  It's not strictly against the "rules" (which really aren't specified), but it sure skirts around showing how a language would solve the intent of the task, that is, use the stated algorithm(s).  I see nothing wrong, however, if there'd be an optimized version (listed as a table lookup) in addition to the use of the task's algorithm(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:40, 2 August 2012 (UTC)

==Template needed==
Now that I've added some clear goals, I find I can't remember that template that asks people to examine and modify their language examples due to a task change. Could someone add this for me please? Thanks. --[[User:Paddy3118|Paddy3118]] 13:33, 8 May 2011 (UTC)
:It was {{tmpl|clarified-review}}. You can find a list of all the templates in [[:Category:RCTemplates]]. I also made a list of hints for example-level templates [[Help:ENA Templates|here]]. --[[User:Mwn3d|Mwn3d]] 13:39, 8 May 2011 (UTC)


----
==Moved from Talk:Self-Describing Numbers==
----
This does not look like it was written as a task, so I just threw together something that looks like it might be relevant (but I am perfectly willing to believe it's not).  --[[User:Rdm|Rdm]] 22:57, 7 May 2011 (UTC)
: I actually received an email from the task creator five hours ago:

```txt
Hi, my account: "Xkrouhn"
 
I do not speak English, sorry.!

I want to write a new task in Rosetta Code. 

This task is solve  in Basic.
 
Self-describing numbers:
There are several integers numbers
called "self-describing".
Integers with the property that, 
when digit positions are labeled 0 to N-1, the
digit in each position is equal to the number of times
that that digit appears in the number.
For example 2020 is a four digit self describing number.
Position "0" has value 2 and there is two 0 in the number.
Position "1" has value 0 because there are not 1's in the number.
Position "2" has value 2 and there is two 2.
And the position "3" has value 0 and there are zero 3's.
Self-describing numbers < 100.000.000 :
1210 - 2020 - 21200 - 3211000 - 42101000
 
 
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
This is the program in Basic
 
'Self-describing numbers < 100.000.000
'1210
'2020
'21200
'3211000
'42101000
 
cls
dim v (10)
dim w (10)
input "Number = ";a$
cls
b = len(a$)
for c = 1 to b
    d$ = mid$(a$,c,1)
    v(val(d$)) = v(val(d$)) + 1
    w(c - 1) = val(d$)
next c
for n = 0 to 10
    if v(n) = w(n) then r = r + 1
next n
if r = 11 then print a$; " Yes, is a Self-describing number"
if r <> 11 then print a$; " Not is a Self-describing number"
for m = 0 to 10
    v(m) = 0
    w(m) = 0
next m
print
print "End"
end
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

I wish to know how to write / do in Rosetta Code
Task / Basic / Solve

Greetings 
```


And my reply a few hours ago:

```txt
Hi!

This is a great task idea. I think the person you should ask about
this would be Paddy3118 and/or Rdm.

I'd leave a note on http://rosettacode.org/wiki/User_talk:Paddy3118
and chat with him about it. Use Google Translate or some such if you
need to. Perhaps including the original Spanish would help, too. There
are a large number of folks on Rosetta Code who know multiple spoken
languages, so I'm sure we'd muddle through it somehow.

Paddy is one of the best task creators we have, and could help you
figure out how to build the task. Rdm knows a great deal about math,
and could quite probably provide insight for better ways of describing
the math pattern the task is about.

Alternately, take a look at
http://blog.rosettacode.org/2011/01/getting-2011-rolling.html ... That
has instructions on how to get started with task creation, quickly.
```

:--[[User:Short Circuit|Michael Mol]] 04:13, 8 May 2011 (UTC)

:Hi, I've just seen this and will see what I can do to help. --[[User:Paddy3118|Paddy3118]] 13:24, 8 May 2011 (UTC)


### Duplicate task


This task appears to be a duplicate of [[Self-describing numbers]].
(Case sensitivity is a pain!)

[[User:Markhobley|Markhobley]] 04:50, 8 May 2011 (UTC)


----
;END OF MOVED TALK DATA
----

== Autobiographical number ==

Wikipedia http://en.wikipedia.org/wiki/Self-descriptive_number describes a self descriptive number:

A self-descriptive number is an integer m that in a given base b is b digits long in which each digit d at position n (the most significant digit being at position 0 and the least significant at position b - 1) counts how many instances of digit n are in m.

The task described here is closer to that which wikipedia defines as an Autobiographical number http://en.wikipedia.org/wiki/Autobiographical_numbers
--[[User:Nigel Galloway|Nigel Galloway]] 12:07, 10 January 2013 (UTC)
