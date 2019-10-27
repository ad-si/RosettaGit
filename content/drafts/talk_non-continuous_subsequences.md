+++
title = "Talk:Non-continuous subsequences"
description = ""
date = 2011-07-25T18:51:39Z
aliases = []
[extra]
id = 2787
[taxonomies]
categories = []
tags = []
+++

====Question: How does D's algorithm make sure not calculating the same subsequence twice?====
This is the algorithm description in D session:
#from the original sequence, select some continuous subsequences of length >= 3,
#from each continuous subsequences in (1), remove some in-between elements beside first and last element,
#each subsequences some elements removed in (2) is a non-continuous subsequences.

Explanation:
*I assume the '''order''' in task description mean '''index''' order(or position of elements in a sequence/list), so that values of element is not relevant to '''order''', not need to be sorted, nor need to be comparable <''Just read the remark in the bottom of D session. This part of comment is not necessary, but may keep as a record of discussion'' -- [[User:Badmadevil|badmadevil]] 05:24, 29 March 2008 (MDT) >;
*each subsequence in (1), or contseq in D's example code, is uniquely determined by its head element index and tail element index;
*when removing elements from contseq in (2), the elements indexed by head and tail is not touched, so each non-continuous sequence produced by this contseq will not overlap with other non-continuous sequence produced by another contseq, since they must have different pair of head/tail indexs;
*within those non-continuous sequence produced by the same contseq, if they have been taken out different numbers of elements, they are of course different non-continuous sequences, because they have different number of elements ;
*for those non-continuous sequence produced by taken out same number of elements from the same contseq, the uniqueness of non-continuous sequences is supported by uniqueness of set of index of elements to be removed ( removeIndexs in D's code) produced by Combi iterator.

I've not got a formal training in computer science, please try to understand casually, thank you. -- [[User:Badmadevil|badmadevil]] 05:05, 29 March 2008 (MDT)

: Looks good. I understood the "beside first and last element" incorrectly, my fault. If you don't mind, I've changed the wording of your explanation in the example a bit. --[[User:Dirkt|Dirkt]] 07:31, 29 March 2008 (MDT)
::Of course not mind -- [[User:Badmadevil|badmadevil]] 09:04, 29 March 2008 (MDT)

==Suggestions for improvement==
After [http://blog.rosettacode.org/?p=186 this post] in the RC blog, I ended up here and thought the page could be improved if references to algorithms attempted where added to the tail of the task description and implementations referred to which they were following. --[[User:Paddy3118|Paddy3118]] 06:08, 12 May 2009 (UTC)

== solution sizes ==

The following comment was added to the C solution: "Note: This specimen can only handle lists of length less than the number of bits in an '''int'''."

While true, this is not likely to be a significant limit, in practice.  The time required to calculate longer results and the space required to store them means that few implementations will be useful for longer lists.

But perhaps the C implementation should be changed to use a long?

--[[User:Rdm|Rdm]] 21:39, 6 June 2010 (UTC)
: '''long''' would, of course, be a limited solution. '''long long''' might also be incomplete.  I've put up a few C and C++ examples that use [[:Category:GMP]], though. --[[User:Short Circuit|Michael Mol]] 12:45, 7 June 2010 (UTC)
:: I think this is going overboard, but maybe I am wrong?  Given that the task asks us to return a list of lists of the results, on what kind of machine would allow you to store a list to long for a "long" based implementation (let us assume we are redirecting the output of the program to a file)?  Also, how long would it take to generate one of these "too-large" lists?  Also, is support for lists this long a requirement for all implementations of this task?  --[[User:Rdm|Rdm]] 16:14, 7 June 2010 (UTC)
::: I've posted a GMP version before reading this. Currently I am running a test with 34 arguments redirected to <tt>/dev/null</tt>... and it is killing my cpu so I'll break it. Anyway, justo to prove that theoretically it can be computed... --[[User:ShinTakezou|ShinTakezou]] 16:58, 7 June 2010 (UTC)
:::: I believe the 34 argument case will result in a list containing (2^34)-596 elements (slightly over 17 trillion elements) with an average of about 17 arguments per element. --[[User:Rdm|Rdm]] 16:04, 8 June 2010 (UTC)
::: For me, the question was mostly of abstraction of the algorithm from obvious implementation constraint. Going ''too'' far in that direction, could result in a very difficult-to-follow code example if it weren't written correctly. --[[User:Short Circuit|Michael Mol]] 18:00, 7 June 2010 (UTC)
::I'm inclined to remove the GMP example.  Its algorithm is copied from J and the first C example, while completely lost the advantage of using bit mask in the first C solution: efficiency.  As it stands, it has nothing useful to offer to readers.  I'm going to delete it later unless someone can give a good reason not to.  --[[User:Ledrug|Ledrug]] 18:51, 25 July 2011 (UTC)
