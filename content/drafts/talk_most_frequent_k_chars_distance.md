+++
title = "Talk:Most frequent k chars distance"
description = ""
date = 2014-05-30T15:21:58Z
aliases = []
[extra]
id = 17442
[taxonomies]
categories = []
tags = []
+++

== Move page? ==
I'm thinking that the page should not be a draft task/task and moved to this sub-page of the authors: "Shedai/Most frequent k chars distance" until it is ready to become a draft task. 

I would like to do this in the next three days unless there are reasoned objections. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:40, 30 May 2014 (UTC)

:Seconded --[[User:DanBron|DanBron]] ([[User talk:DanBron|talk]]) 15:21, 30 May 2014 (UTC)

== What is this? ==

This is a terrible write-up. Please try to define terms using plain English or math formulas, not some identifiers from a piece of sample code. Also this looks identical to the linked WP page except the code, which was removed by another WP editor. What's the purpose for this copy/paste? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 02:05, 23 March 2014 (UTC)

:I have asked the author to answer here. It seems to be their very first contribution to RC and seems as if it might be an attempt at self-publication. Even if it is not, I would be inclined to delete this if the author does not explain. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:25, 24 March 2014 (UTC)

== Prank Page? ==

This is quite possibly the worst "similarity" function I have ever encountered and wonder if this is just a prank to see if someone can get something completely bogus published. The results table is inconsistent with the pseudo code. It is difficult to determine an algorithm that will give those results for the given inputs. Strings like 'iiii' and 'Mississippi' would rank as having a much shorter "distance" (whatever that means in this context) than identical strings like 'abcdefghijklmnopqrstuvwxyz' and 'abcdefghijklmnopqrstuvwxyz'. Which may be useful information... but this seems like an awfully convoluted way to obtain it. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 01:31, 24 March 2014 (UTC)

:I completely agree, the algorithm is not one of the best for string similarity and also there are many cases the algorithm fails. But, only for the text mining studies, where the words like 'iiiii' or 'abcdefghijklmnopqrstuvwxyz', exceptionally appears, the algorithm gives a better success than binary string similarity functions and works faster than algorithms with higher success like levenshtein distance. Besides, some specific text mining studies, perhaps in the future algorithm can be deployed in some other studies. I also found the inconsistency between pseudo code and the table and fixing it now. Also I will spend time on rewriting the article and fix its English. [[User:Shedai|Shedai]] ([[User talk:Shedai|talk]]) 08:01, 24 March 2014 (UTC)

::Thanks for the feedback Shedai :-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:41, 26 March 2014 (UTC)

::: Leaving aside the utility of the algorithm, there are still discrepancies and disagreements between the English description of the algorithm, the pseudocode, and the test cases given. 
::::1. Minor things first: In the "Examples" section, the last sample function call, <tt>MostFreqKSDF('research', 'seeking',2) = 2</tt>, is missing a parameter (its prototype specifies an <tt>int maxDistance</tt> parameter), and its output should be <tt>8</tt>.

::::2. The English description of the scoring function has the sentence "if two characters have equal frequency, the first appearing in input will appear before at the output" which is ill-formed and hard to interpret, and the comment in the pseudocode "(if two chars have same frequency than get the first occurrence in inputString)" hardly elucidates. The combination of these specifications seem to suggest that if two different characters have the same frequency, then the frequency table ''should explicitly exclude'' the second character (i.e. the one that showed up later in the input),  except this seems to be contradicted by the examples (e.g. <tt>s2e2</tt>, <tt>n1i1</tt>, etc). So I guess the interpretation is "use a [[wp:stable sort|stable sort]] for the frequency table"?

::::3. The pseudocode gives <tt>similarity += frequency of c in inputStr1 + frequency of c in inputStr2</tt>, but the test cases don't sum the frequencies of the hashes; instead, they seem to compare them. That is, most test cases act like <tt> similarity += (frequency of c in inputStr1 = frequency of c in inputStr2) ? (frequency of c in inputStr1) : 0 </tt>. 

::::4. Except, of course, in a few of the test cases, where something completely different is happening.  For example, if I've correctly deciphered the task, and frequencies are to be ''compared'', then I would expect that <tt>MostFreqKSDF('research', 'research',2,10)</tt> to produce <tt>6</tt> because <tt>MostFreqKHashing(string,2)</tt> produces <tt>'r2e2'</tt> for both string inputs, and <tt>2+2</tt> is <tt>4</tt>, and <tt>10-4</tt> is <tt>6</tt>. But the test case specifies <tt>8</tt>. I have no idea why.

::::5. Similar discrepancies exist in <tt>MostFreqKSDF('significant';'capabilities',2,10)</tt> where the test case gives <tt>5</tt>, but a consistent implementation (where scoring is based on comparison) gives <tt>7</tt>, and the bioinformatics example <tt>MostFreqKSDF(str1,str2,2,100)</tt> specifies an answer of <tt>83</tt> but a consistent implementation gives <tt>100</tt>, because no characters share the same frequency. 

::::6. The alternative interpretation (taken by both current implementations) is that the frequencies ''are'' actually summed, and this makes <tt>MostFreqKSDF(str1,str2,2,100) = 83</tt> work, but that breaks <tt>MostFreqKSDF('night','nacht',2,10)=9</tt> (which would be <tt>8</tt>), <tt>MostFreqKSDF('research', 'research',2,10)=8</tt> (would be <tt>6</tt>), <tt>MostFreqKSDF('aaaaabbbb','ababababa',2,10)=1</tt> (would be <tt>-8</tt>), <tt>MostFreqKSDF('significant', 'capabilities', 2, 10)=5</tt> (would be <tt>4</tt>), and the original example, given (implicitly) in the preamble, <tt>MostFreqKSimilarity('r2e2','e2s1',2,10)=8</tt> (would be <tt>6</tt>).

::::7.  Just to make sure I wasn't crazy, I ran the Java code posted by the original author of the task, and as expected (for the reasons outlined just above) its outputs disagree with the values given in the test cases (for example, it gives <tt>-8</tt> for <tt>'aaaaabbbb'</tt> vs <tt>'ababababa'</tt>, and actually crashes for <tt>'my'</tt> vs <tt>'a'</tt>). 

::: Anyway, unless I'm overlooking something obvious, an implementation can not be consistent with both the pseudocode and the required examples. Anyone care to point me in the right direction?
::: (For now, I'm going to post the J code I wrote based on the "frequencies are compared" interpretation, which, IMO, accords better with both the test cases and the actual concept of "similarity".)
::: --[[User:DanBron|DanBron]] ([[User talk:DanBron|talk]]) 22:46, 29 May 2014 (UTC)

== Editorial work needed ==
The task description and encyclopædic content need work to correct things like incomplete sentences (!) and it would be far better if we could leave more of the background to Wikipedia. We're looking at implementations on RC, not general collections of algorithm descriptions (especially where they're long enough to merit their own section!)

Implementation-wise though, I think I can write one (the genomic data would seem a reasonable test case, yes?). I'd prefer to not use the specified intermediate representation though; I've got more efficient ones available to me. That means that I think this is a correctable task that should be able to become a full task with just a little more work. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 17:00, 27 April 2014 (UTC)
