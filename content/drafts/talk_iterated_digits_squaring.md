+++
title = "Talk:Iterated digits squaring"
description = ""
date = 2015-09-14T03:04:54Z
aliases = []
[extra]
id = 17878
[taxonomies]
categories = []
tags = []
+++

==Comments on (a nice) task==
Hi,
# Do we need to keep the project Euler limit of 100million, wouldn't one million do? 
# It might be best to explain the limit a bit more w.r.t. inclusive/exclusive end points.
# Do we need to mention caching in the task description?

Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:09, 23 August 2014 (UTC)

: Thank you for your comments.
# The original Euler problem ha a limit of 10 millions. I think it's not a good idea to lower the limit too much (like 1 million): the point of having a high limit is to nudge people away from the very short brute-force solutions and toward a little smarter combinatorics-based solutions. To show it can be done I have added a not too much long Python solution that solves the problem with 100 millions in less than half second on a slow PC.
# Regarding the limits, I have used standard mathematical notation, but the current <= < notation is OK. 
# Regarding the caching, it can be explained in the task description, but in the solutions I'd like to see less catching and more (simple) combinatorics. --[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 08:17, 24 August 2014 (UTC)

:: The only thing I'd say about caching is don't. It may speed up a bad algorithm but it can not beat a fast solution. Sorting each number so it stands a better chance of a hit in a cache which it is pointless to use is ...... .--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:17, 11 September 2014 (UTC) 

:I took your comments on board Bearophile, and de-emphasized the one mill limit and hopefully made it unattractive enough to see more combinatorics examples.
:What do you think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:54, 24 August 2014 (UTC)
:: It's OK. --[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 09:07, 24 August 2014 (UTC)

==Name change?==
Is it worth changing this tasks name to something like Project Euler 92 which is likely to be found by search engines?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:17, 11 September 2014 (UTC)
:I would vote against. This is RC, and they are mentioned in the task description which would allow searching to some degree. I wouldn't want tasks to follow other sites, for example, sites where input and output are very constrained to allow more automated testing, or where algorithms are deliberately both expected and not given. What do others think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:02, 12 September 2014 (UTC)
:Trying Project Euler 92 I found no hits to RC in the first 10 pages. I did find Project Euler 92 Square Digit Chain. Trying Square Digit Chain I found many hits to this problem outside Project Euler. So I conclude that this problem is widely known as Square Digit Chain, and this would be acceptable to me here. Trying Iterated Digit Squaring revealed RC as the only site that calls it this. The problem is well known and I think we should use a name it is well known by.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:13, 12 September 2014 (UTC)

::Hi Nigel [http://rosettacode.org/mw/index.php?search=+Project+Euler+92&fulltext=Search&title=Special%3ASearch I found it] using RC's search. 
::[https://www.google.co.uk/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=site%3Arosettacode.org%20%22project%20euler%2092%22 On Google], when you specify the site then I got the ''only'' answer. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:16, 12 September 2014 (UTC)
::So what. If you already know it is here why are you using Google. The point is to find it without knowing it is here!!!--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:25, 12 September 2014 (UTC)
:::Can you expand on that Nigel? I was trying to show that if you wanted to find the project euler solution on RC then you could and showed two ways of doing it without having to change the name of the task. I also gave some reasons not to use the other sites title on RC. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:34, 12 September 2014 (UTC)
:::I would have thought it was obvious, I suggest that most people in the world have not even heard of rosettacode. If they are searching for info on this problem they should still be able to find it on rosettacode using Google. Note that the D version we were supplied with comes from mathblog.dk/project-euler-92-square-digits-number-chain/ which contains both Project Euler 92 and its widely known name 'square digit chain'. If your reasons for not mentioning Project Euler are valid I have suggested using its other widely known name 'square digit chain'. No one else calls it 'Iterated digits squaring'--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:54, 14 September 2014 (UTC)

:::: "Project Euler 92" I don't like. "Square digit chain" for the reasons you give seems better than than what we have. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:42, 16 September 2014 (UTC)

==The Combinatorics of Life? The Universe? This Task simple or otherwise==

### Step 1 Precompile some small values

The following is an array indexed 0 to 648. Entry n==0 means that IDS(n) translates to 89 and n==1 means that IDS(n) translates to 1. The array is a constant and may be copied into a solution or calculated by the solution as you wish.

```txt

Set N to [1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]

```

Entry 0 is set to 1, and may be used to calculate IDS(100,000,000), ignored, or deleted (if your language supports array indexing starting at 1).--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:23, 7 September 2014 (UTC)--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:23, 7 September 2014 (UTC)

### Step 2 Iterate over unique digit combinations

Note that IDS(12345678) == IDS(21436587) and indeed any other arrangement of these digits. So we only wish to determine if each of these translate to 89 or 1 once. We can determine how many of these unique digits there are from the following table which I produced earlier using Gnumeric.

```txt

1	10	55	220	715 	2002	    5005	11440
1	9	45	165	495 	1287	    3003	6435
1	8	36	120	330 	792	    1716	3432
1	7	28	84	210 	462	    924 	1716
1	6	21	56	126 	252	    462 	792
1	5	15	35	70  	126	    210	        330
1	4	10	20	35  	56	    84  	120
1	3	6	10	15  	21	    28  	36
1	2	3	4	5   	6	    7	        8
1	1	1	1	1   	1	    1	        1
10	55	220	715	2002	5005	   11440	24310

```

From which we see that we have 24310 unique digit combinations, which is a lot less than 100 million.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:33, 7 September 2014 (UTC)

Rosettacode task [[Combinations with repetitions]] may help you find these 24310 unique combinations in your language, or not!!!--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:52, 9 September 2014 (UTC)


### =Step 2.1 Count only ones=

Looking at the array in Step 1 we see that 90 out of 648 IDSs produce 1, so assuming this continues for larger values we shall do less work counting the 1s rather than the 89s. Square each of the digits in this combination and sum them. Use this value to index the array N from Step 1. If the value is 0 goto next Step 2.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:36, 7 September 2014 (UTC)


### =Step 2.2 Determine how many numbers this digit combination corresponds to=

24310*90/648 = 3376.4, so we may expect to perform this step for around 3376.4 combinations. IDS(22266666) also represents IDS(62226666) and many others. We now need to determine how many. To do this we must count the number of each digit we have. In this case 3 twos and 5 sixes, 0 for all others. So 22266666 represents 

```txt

   40320/0!*0!*3!*0!*0!*0!*5!*0!*0!*0!

```

remembering that 0! == 1. Add this value to the count of IDSs terminating in 1--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:45, 7 September 2014 (UTC)


### Step 3 Output result

The number if IDSs terminating in 89 is 10**8 - the final count from Step 2.2--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:47, 7 September 2014 (UTC)

==OK this is what I've done==
I have created a new task [[Combinations with repetitions/Square Digit Chain]] into which I think the Combinatoric solutions fit better. Note that [[Combinations with repetitions]] has 2 methods in D. I found the J solution interesting, and there are probably other non-comb things to be said in this task. Gerard Schildberger returned this task to draft on 26th., August without saying why, I added the explanation of the Combinatorics and made it a task again. I've now returned it to draft pending a clearer description of what goes here.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:09, 16 September 2014 (UTC)

: What I saw at the time of the my updating the REXX section was that this Rosetta Code entry didn't appear to be a task (or any type of task) at all (as I didn't see any TASK or DRAFT TASK statement with the   <nowiki> {{ xxx }} </nowiki>   braces around anything)   at the beginning of the Rosetta Code ... er, "task".   So, as my summary comment said, I '''added''' the draft task template (as there appeared to not be any template at all, task nor draft task).   I certainly didn't intend to promote or demote any ... thingy/task.   I'm sorry for the retrograde revision, it was not my intent to revert any task back to draft status, my intent was to just provide a template ('''draft''' seemed safer to add) where there appeared to be none at all.   It was this (apparent) lack of any template that prompted me to add a draft-task template.   Perhaps it was a stale version that I was viewing;   this wouldn't be the first time a global edit change was processed or presented incorrectly.   I again apologize for any misdeed occurring on my behalf of my good intentions, albeit done incorrectly or by happenstance. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:21, 16 September 2014 (UTC)

I downloaded a D compiler and produced my first program in D here [[Combinations with repetitions/Square Digit Chain]]. It uses the combRep procedure defined here [[Combinations with repetitions]]. I suggest this shows that the cobanatronics in this task is a duplication of work already produced in [[Combinations with repetitions]]. I would like to change the task description to require a function which when given a number returns true or false indicating that that number translates to 1 or not according to the rules for square digit number chaining, and to demonstrate the function by counting the number which translate to 1 upto the Project Euler #92 limit of 10**7. No special coding being required for speed, de-emphasizing the timing and emphasizing that a large n in the chain rapidly becomes a small n, but a small n may become a slightly larger n for a time.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:26, 18 September 2014 (UTC)

==ProjectEuler92==
The carefully worded PE challenge says that the sequence "attains" rather than "ends with" 1 or 89.

-----

Also,   ''Project Euler 92''   requested:

:: <big> How many starting numbers '''below''' ten million will arrive at 89? </big>

(Bold lettering added by me). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:38, 25 May 2015 (UTC)

== Dup? ==

While the example results asked for in this task are different, this task seems quite similar to [[Happy_numbers|Happy numbers]]. This suggests one of several possible approaches:

# Merge the tasks
# Cross reference the tasks
# Create a category for the tasks
# Pretend it didn't happen
# Pretend it did happen, but do nothing anyway.

I'm not sure which of these we should choose... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]])

: I don't think merging the tasks will simplify anything.   Especially at this late date for two bona fide, full-fledged Rosetta Code tasks.   Creating a category for the tasks, I think, would be nice.   You could add a 5<sup>th</sup> option:   Pretend it did happen, but do nothing anyway.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:01, 13 September 2015 (UTC)

:: Added. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:05, 13 September 2015 (UTC)

: Since   '''happy numbers'''   have their own name and are referenced in/on publications and other web-sites, they deserve their own algorithms (without generalizing them).   Similar to primes, K-primes, almost-primes, pseudo-primes, phi (totient), divisors (tau), proper divisors (aliquot), and the like;   they are both generalizations of   ''factoring''.   Similarly with factorials and multi-factorials, and the many variants of Fibonacci series (the Lucas series being one of them).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:22, 13 September 2015 (UTC)

:: Ok, but I do not understand what factoring has to do with sums of squares of digits... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:57, 14 September 2015 (UTC)

::: Nothing at all.   I was generalizing about various primes and factoring, not squares of digits and factoring.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:04, 14 September 2015 (UTC)
