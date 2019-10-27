+++
title = "Talk:Self-referential sequence"
description = ""
date = 2018-08-18T23:10:21Z
aliases = []
[extra]
id = 10367
[taxonomies]
categories = []
tags = []
+++

==Explanation request==
For the first sequence. I thought it was the second sequence until they diverge and I then knew I was following the wrong method of generation. --[[User:Paddy3118|Paddy3118]] 21:34, 21 August 2011 (UTC)
: The first sequence is just an example, not central to the task. In that one you generate the next term by reading out loud, if you will, the digits. 0 is one zero (10), next there is (reading the term) one one, one zero (1110) then three ones, one zero (3110) See [http://oeis.org/A001155 A001155]. I just mentioned it because it is probably the most commonly cited self-referential sequence in my experience. It is just a coincidence that they are the same for the first five elements when seeded with 0. It may be worth have generation of that sequence as part of the task (or a separate task) but I thought the second sequence was more interesting. As an aside, for the second sequence, I think there may be only one sequence that takes more than 21 steps to converge. A string of 900 9s will converge in 22 steps. There may be others but I haven't, and can't practically do an exhaustive search. --[[User:Thundergnat|Thundergnat]] 00:40, 22 August 2011 (UTC)
:: There certainly will be longer sequences.  Take a number with 900 digit 9s, its next step is 9009, so length is +1.  That number itself ends with 9, so you can construct another number with a gadzillion digits of 9s which is again length +1, and this can go on ad infinitum. --[[User:Ledrug|Ledrug]] 03:35, 22 August 2011 (UTC)
::: Erm. Obvious as soon as you pointed it out. Sigh. --[[User:Thundergnat|Thundergnat]] 10:52, 22 August 2011 (UTC)

:Okay. Is the difference that in the first sequence you say what you see whilst traveling left-to-right through the digits, whilst in the second you are summarizing how many of each digit there are from highest digit to lowest? (Maybe any description could aid comprehension by also describing the derivation of that member of the sequence where they start to differ)? --[[User:Paddy3118|Paddy3118]] 07:38, 22 August 2011 (UTC)

:: Perhaps an easier way to think of it is to take the term, say 13123110, sort the digits high to low: 33211110, then read it off as in the look-and-say sequence (which is what the first sequence is basically): 2 3s, 1 2, 4 1s, 1 0 or 23124110. In look-and-say, you don't sort the digits first. Here, you do.--[[User:Thundergnat|Thundergnat]] 10:52, 22 August 2011 (UTC)

::: Instead of look-and-say, it's more like count-and-say.  I didn't sort the digits at all (in the REXX program). -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:49, 3 May 2012 (UTC)

== Huh? ==

The task description currently says:

:Find all the positive integer seed values under 1000000, ... For this task, assume leading zeros are not permitted.

Does this mean that values under 1e5 are not permitted? --[[User:Rdm|Rdm]] 19:44, 22 August 2011 (UTC)
: It means that you shouldn't use seed values with leading zeros like 010 or 00754; They should be 10 and 754 respectively. --[[User:Thundergnat|Thundergnat]] 21:54, 22 August 2011 (UTC)

==Name change?==
So many sequences derive the next term from some function of the current term as to make the name of the task meaningless. How about "Longest A036058 sequences before repetition". --[[User:Paddy3118|Paddy3118]] 22:01, 23 August 2011 (UTC)
:We could try to make it a variation of the title they use which is "Summarize digits of preceding number". --[[User:Mwn3d|Mwn3d]] 22:47, 23 August 2011 (UTC)

::Except two such sequences are mentioned in the task header and your title could apply to Look-and-say as well. Maybe we could start a trend of mentioning the [http://oeis.org On-Line Encyclopedia of Integer Sequences] number for tasks involving sequences that don't have a strong name of their own? --[[User:Paddy3118|Paddy3118]] 23:54, 23 August 2011 (UTC)
:::I don't think that makes them very descriptive or searchable. I think we should try to use real words. --[[User:Mwn3d|Mwn3d]] 00:24, 24 August 2011 (UTC)

:: I don't have strong feelings either way. Maybe rename it "Sort-and-say sequence"? That at least gives some clue about how it is derived. --[[User:Thundergnat|Thundergnat]] 00:30, 24 August 2011 (UTC)
::: OEIS says "this kind of counting sequence", so maybe "digit counting sequence"?  If name has to be "*verb*-and say", the verb is better as "Count" IMO since sorting is not essential. --[[User:Ledrug|Ledrug]] 00:42, 24 August 2011 (UTC)
::::+1 --[[User:Paddy3118|Paddy3118]] 04:55, 25 August 2011 (UTC)

::: For the task name, the word   '''sort'''   (as in sorting) isn't necessary to the task,   '''counting'''   is (or is implied by ''looking'').   The '''REXX''' example doesn't do a sort.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:08, 18 August 2018 (UTC)

:Sequences which derive a value from the previous value have a nice, well-established name: they are called ''recurrences''.  The particular recurrences being discussed here are based on calculations that work with the base ten representation of integers. This kind of manipulation is ''typographical'': essentially it works with the printed representation, which leads us to ''typographical recurrence''. Knuth's term ''semi-numerical'' could apply here: the calculation performs counting over the base 10 typography of the previous term, and counts then turn into typography in the next term. This suggests ''semi-numerical recurrence''.[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 16:50, 20 July 2014 (UTC)

==Caching==
The perl example has a note about the calculation to a million takes a long time. For the Python solution, I took note of the comment on [http://oeis.org/A036058 the OEIS page] 
:''"This kind of counting sequence is always periodic with period 1, 2 or 3"''' 
And set up a ring buffer of the last three terms as a cache and also did not compute the series for starting values that had permuted digits of a starting number that had come before.

I have no version without those possible speed-ups, but the code ran in a minute or two for a million starting numbers and maybe Perl would run in an acceptable time as, in general, Perl 5 and Python have similar run times for similar algorithms I find. --[[User:Paddy3118|Paddy3118]] 05:06, 25 August 2011 (UTC)
:: The perl code already caches everything, so the benefit of a last-three buffer is not obvious.  As for doing permutations of seeds, most numbers run into cached sequences pretty fast anyway, so that's not going to help much, either.  Calculating only one of a permutation group runs another risk: if there's a sequence A->B->B where A and B are permutations, the result for seed A and seed B may be inconsistent.  This doesn't happen for base 10 below 1000000, though (last three numbers of sequence from seed 0 is such an example, which is what, 16 digits long?).
:: To put things into perspective, the perl code runs one million in about a minute, but my attention span is only 10 seconds or so, so it felt too long.  I really wanted to be able to cache only sorted strings which would give about 10 times speed up and reduce memory usage, but dealing with the A->B->B stuff above was a real dog.  --[[User:Ledrug|Ledrug]] 07:30, 25 August 2011 (UTC)

::: The attention span comment gave me a titter :-)
--[[User:Paddy3118|Paddy3118]] 13:51, 25 August 2011 (UTC)

::: Since we are looking for longest sequences, why does A->B->B matter? --[[User:Rdm|Rdm]] 17:32, 25 August 2011 (UTC)
:::: For this task, it doesn't.  It's just a matter of general correctness of getting the right length of any sequence.  Suppose the task asked for shortest sequences, caching only sorted strings will lead to the conclusion that sequences from A and B are of the same length, while they are not. --[[User:Ledrug|Ledrug]] 17:43, 25 August 2011 (UTC)
::::: That would be a completely different algorithm.  Furthermore, you should not want to find the sequence length for that task.  --[[User:Rdm|Rdm]] 19:36, 25 August 2011 (UTC)
:::::: Er, don't take that example so literally.  Suppose the question is, "of numbers 24132231 and 14233221, which has the longest sequence", for example (and no, don't take this example too literally, either.  I'm just saying, only caching sorted string of each permutation has its problems.) --[[User:Ledrug|Ledrug]] 21:05, 25 August 2011 (UTC)
::::::: Ok... but as a general rule, overgeneralizing code can introduces complexity and inefficiency.  --[[User:Rdm|Rdm]] 21:13, 25 August 2011 (UTC)
:::::::: That was what I was griping about... actually come to think of it, now I remember what was bugging me: the C code does <lang>function seqlen(n):
    if n in cache then: return cache[n]
    else: return 1 + seqlen(next(n))
```

:::::::: Before this I had <code>if sorted(n) in cache then: return cache[sorted(n)]</code>, which can lead to incorrect results if a sequence ends in A->B->B, where <code>sorted(A)</code> is the same as <code>sorted(B)</code>.  It so happens that it doesn't affect this task, but this consideration is not exactly overgeneralization. --[[User:Ledrug|Ledrug]] 21:26, 25 August 2011 (UTC)
:::::::: Ok, yes... that's worthy of a comment in the code. --[[User:Rdm|Rdm]] 21:31, 25 August 2011 (UTC)

== Draft for how long? ==

So when do we promote it? Is there a implementations-threshold? Do we need to be happy with the name?

== 32 bit C? ==

Is the C version working on 32 bit systems too?
: Bug fixed. Maybe it was caused by puts("") inside a for().

== Seemingly Ungrammatical Sentence ==

''"Find all the positive integer seed values under 1000000, for the above convergent self-referential sequence, that takes the largest number of iterations before converging."''

I cannot make complete sense out of this. What is the complement of the clause "takes the largest number ..."? Since the verb "takes" is singular, the complement must be some singular noun clause, so "seed values" cannot be it.

Is the task to sort the seed values in descending order, by the length of the initial non-repeating segment that they generate, and list all the seed values which tie for first place?[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 16:46, 20 July 2014 (UTC)
