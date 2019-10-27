+++
title = "Talk:Longest Common Substring"
description = ""
date = 2015-02-24T19:53:23Z
aliases = []
[extra]
id = 18732
[taxonomies]
categories = []
tags = []
+++

== Duplicate?==
It seems that this is a duplicate of [[Longest Common Subsequence]]? 

If not, is it unique enough to co-exist? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:56, 18 February 2015 (UTC)
:It looks like this one only counts consecutive letters and doesn't allow the subsequence to be split. I vote too similar to co-exist. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 21:13, 18 February 2015 (UTC)
:I know it's not a duplicate, because Longest Common Subsequence produces different results. For example, the longest common subsequence between "thisisatest" and "testing123testing" is "tsitest". The longest common sub''string'' is just "test". I was going to use my code as an example of dynamic programming in Longest Common Subsequence until I noticed the difference in the algorithms. Longest Common Subsequence skips characters in the middle of a string, whereas Longest Common Substring only considers consecutive characters. For that matter, the two problems also have separate Wikipedia articles, for what it's worth. 

:All I know is, Longest Common Subsequence didn't solve my problem, but Longest Common Substring does. That's enough reason for me for it to co-exist. --[[User:Geoffhacker|Geoffhacker]] ([[User talk:Geoffhacker|talk]]) 21:17, 18 February 2015 (UTC)
:I originally thought this was a dup, and changed my mind.  Seems quite different to me, and of much more interest to bioinformaticians than the other algorithm. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 06:16, 19 February 2015 (UTC)
::It is indeed a problem in a Coursera bioinformatics course that is currently running and solutions are expected to use suffix trees.  The strings for that exercise are like 30K in length.  I tried the current C# algorithm (coded in Go though) on one of these data sets.  It took 15 seconds or so but did produce an answer that passed the grader. &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 21:05, 19 February 2015 (UTC)

Thanks for pointing out the difference Geoffhacker. Keep those new tasks rollin' ...

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:51, 19 February 2015 (UTC)

==Generalized Suffix Trees==
I have added a reference to generalized suffix trees which is a O(n) time method for solving this for n strings.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:31, 19 February 2015 (UTC)
:Yes, generalised suffix trees would certainly provide a more efficient algorithm for solving this problem. But from what I've seen, they also take longer to code than the time I happen to have available. Still, it'd be cool to see one implemented if anyone did have the time. --[[User:Geoffhacker|Geoffhacker]] ([[User talk:Geoffhacker|talk]]) 19:43, 24 February 2015 (UTC)
:I see that you have created the first necessary subtask in that algorithm, building a suffix tree. Good idea, leverage the crowd! --[[User:Geoffhacker|Geoffhacker]] ([[User talk:Geoffhacker|talk]]) 19:53, 24 February 2015 (UTC)
