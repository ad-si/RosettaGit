+++
title = "Talk:Priority queue"
description = ""
date = 2015-12-19T00:46:55Z
aliases = []
[extra]
id = 10235
[taxonomies]
categories = []
tags = []
+++

== Lua version broken? ==

I think the Lua version is broken. Well the default list of things works just fine and makes the correct output, if you change the list of priorities to:

```lua
tasks = {
    {3, 'Clear drains'},
    {334455, 'Feed cat'},
    {441234, 'Make tea'},
    {23, 'Solve RC tasks'},
    {2, 'Tax return'}
}
```

You get what i think is completely, wrong output.
<lang>Putting: 3 - Clear drains
Putting: 334455 - Feed cat
Putting: 441234 - Make tea
Putting: 23 - Solve RC tasks
Putting: 2 - Tax return
Popped: 441234 - Make tea
Popped: 2 - Tax return
Popped: 334455 - Feed cat
Popped: 23 - Solve RC tasks
Popped: 3 - Clear drains
```

I think this happens because Lua guarantees no order to associative tables (key value pairs), the example is just lucky enough that that hashes are lining up or something.

== Refinement ==

This draft task needs more work. In particular, it needs something for people to actually do with the priority queue; just instantiating it isn't enough! What about sorting a bunch of tasks? Here's a possible set, sorted alphabetically by task name:
 '''Priority'''    '''Task'''
   3        Clear drains
   4        Feed cat
   5        Make tea
   1        Solve RC tasks
   2        Tax return
This is just a suggestion, but it shows the value of these sorts of structures. –[[User:Dkf|Donal Fellows]] 08:09, 4 August 2011 (UTC)
: I did put a test case in the draft, the add-then-dequeue sorting thingy, if somewhat vague.  I don't know if using a very small sample test case like this is a good idea, since priority queue is often used in performance critical areas like process/IO schedulers, while a small test case tend to encourage inefficient solutions--won't be long before we see someone implement O(n^2) insertion times.  Your task priorities are pretty good, though. --[[User:Ledrug|Ledrug]] 08:22, 4 August 2011 (UTC)

::Hi guys, it's very difficult for RC to tackle efficiency in execution time except by suggesting one or more algorithms to use. Some languages might be justified in using an easy to implement algo, rather than a longer, more efficient one as they would never be used where their solutions speed would be the issue.
:: +1 on the idea of a small sort example like the above. --[[User:Paddy3118|Paddy3118]] 10:26, 4 August 2011 (UTC)

:: If you're going to specify an algorithm, you end up ruling out the use of library code because you can't easily ''show'' that the library code is using the algorithm. I wanted to suggest a demonstration element to the task that would allow the key operations you mentioned in your initial draft to be used in a “natural” way. Yes, there probably ought to be some interleaving between gets and puts, but I can't be bothered right now to conceive of how to write that many tasks down. :-) I hate it when a task is just “here is some code”; I want to be able to run it for myself (and, of course, tinker with it for my own ends). –[[User:Dkf|Donal Fellows]] 13:29, 4 August 2011 (UTC)
:::I mentioned algorithms in a generic sense - not specific to this task, but as a part of the problem in adding any requirement for execution speed to the task. --[[User:Paddy3118|Paddy3118]] 16:17, 4 August 2011 (UTC)

How about asking to demonstrate the following actions on the queue, where a Pop action is presumed to display what has been popped?
 '''Action'''  '''Priority'''    '''Task'''
   Add      3        Clear drains
   Add      4        Feed cat
   Add      5        Make tea
   Pop1   
   Add      1        Solve RC tasks
   Add      2        Tax return
   PopTillEmpty

--[[User:Paddy3118|Paddy3118]] 16:17, 4 August 2011 (UTC)

==C prog. output?==
The output of the C program seems wrong. I was expecting one copy of each item sorted in priority order? --[[User:Paddy3118|Paddy3118]] 05:31, 5 August 2011 (UTC)
: I have two queues, each with randomly selected 8 items and then merged, so output is a random combination of 16 items. --[[User:Ledrug|Ledrug]] 05:45, 5 August 2011 (UTC)
::Thanks for the explanation Ledrug. --[[User:Paddy3118|Paddy3118]] 06:01, 5 August 2011 (UTC)

Is it possible to agree on a suitable example for all language implementations to follow - to aid in comparisons - and then allow implementors to do extra if they want? It could be yours; it could be something else, but one common thing to do? --[[User:Paddy3118|Paddy3118]] 06:01, 5 August 2011 (UTC)
: Ok I'll edit the C code to use 5 items later. --[[User:Ledrug|Ledrug]] 06:17, 5 August 2011 (UTC)
:: Ta! --[[User:Paddy3118|Paddy3118]] 08:00, 5 August 2011 (UTC)
: Be very, very careful about embedding too many paradigm assumptions when looking for One Prescribed Way. And, yeah, different paradigms will tend to make comparison difficult, but those are the breaks. --[[User:Short Circuit|Michael Mol]] 12:31, 5 August 2011 (UTC)

==Ready to promote?==
I think it's good enough to go out of draft. --[[User:Ledrug|Ledrug]] 03:23, 29 August 2011 (UTC)
: Aye. --[[User:Paddy3118|Paddy3118]] 07:29, 29 August 2011 (UTC)

==Python Entries==
I think there's too much Python docs shown in the Python entries.
