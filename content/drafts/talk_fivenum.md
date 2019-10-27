+++
title = "Talk:Fivenum"
description = ""
date = 2019-02-15T08:05:26Z
aliases = []
[extra]
id = 21738
[taxonomies]
categories = []
tags = []
+++

== The task needs clarification ==
* What the task is actually asking for is the 5 numbers used to draw a boxplot. This has nothing to do with "big data", or with producing a "smaller array". Requiring that the five numbers yield the same boxplot if they are treated as data is pretty useless: the task emphasizes space reduction, but it will not save space.
* There are several conventions for boxplot statistics. Not all statistical programs draw boxplots like R.
* Also, 5 numbers are obviously not enough to reproduce a boxplot, as outliers are usually drawn as well. This is not a problem if we admit we don't need the outliers.

All in all, the task seems to be "rewrite R's fivenum in your language". It certainly can be a task, but it's rather short-sighted: there are other languages beyond R, out there.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:35, 25 February 2018 (UTC)

:"This has nothing to do with "big data", or with producing a "smaller array". Requiring that the five numbers yield the same boxplot if they are treated as data is pretty useless: the task emphasizes space reduction, but it will not save space."
:
:Everything in this quote is objectively false.  I had a task in my job to make boxplots with huge datasets (> 120 GB of data) but all I needed were these five data points.  It made no sense whatsoever to save every data point.  I was doing this in Perl, since I don't like using R unless I have to.  That was the purpose of the page.  On the contrary, this task was very useful for me, but maybe not for you.  I wouldn't have been able to make these plots without this task, specifically the Perl translation.  "pretty useless"? on the contrary, this was essential, and I couldn't have performed the task without it.  In the spirit of generosity, I decided to make my work in translating R's fivenum function available to others in case they had the same problem I did.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 14:27, 26 February 2018 (UTC)
::You missed the point, again. That YOU had to do this in your job does not mean that computing 5 numbers is related to big data. Maybe you needed also a mean, that does not imply computing a mean is related to big data: you can take a mean of 10 values. Always your case, your job, your own particular situation, does not make a general task. The general task would say: compute these numbers, period. You can do it with small data, with big data, with gigantic data, noone cares. Besides, the task you asked is not exactly the same: you asked for data that would produce the same numbers. And THAT is useless, as it's obviously enough to store the numbers. But maybe the sentence was not clear enough? Oh, and you did not describe clearly how these five numbers are to be computed: as I already said, there is no universal convention on boxplots. All in all, you didn't address any of my questions. So much for your generosity. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:00, 26 February 2018 (UTC)
== License ==
The R function is part of the R source, hence has GPL license. Any translation of this is a derivative work. On such a simple function, I doubt it would be a problem, but please be careful next time: copy-pasting is '''not''' fine. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 23:29, 25 February 2018 (UTC)

== Large vs not large ==
I removed the requirement, as it seems unrelated to the task. We are faced with a choice here:
* Either the important part is the large dataset. But then, how large? Does the data fit in memory? On a single hard drive? Does it require multiple hard drives in a network of computers? A dataset that fits in memory does not look large to me. Of course, it's a matter of hardware: a server with 256 GB memory will be enough to do in-memory computations that would require a hard drive on most PCs. A really large file would require a network, and technology like [https://en.wikipedia.org/wiki/Apache_Hadoop Hadoop] or [https://en.wikipedia.org/wiki/Apache_Spark Spark], or other cluster-computing facility. If we insist in requiring all of this (which looks perfectly acceptable, as it would be a good exercise in managine large data), the task will be much more difficult, or impossible for most languages. And the R solution would be wrong (but I imagine there are packages to do that correctly in R).
* Either the important part is computing these numbers. Then it's all about computing the median and quartiles (min and max are trivially doable in O(n)). A much simpler task, but every language should be able to do that.

I would be happy with both possibilities, but these are entirely different tasks, and if we have to manage large data, please state how large, and adapt the current solutions accordingly. All current solutions imply the dataset lies entirely in memory. For "usual" machines, that means the dataset is actually rather small.

Hailholyghost gave his example above, here is another one: most of my work is done on a business PC with 8GB RAM and SAS/Stata/R/Python (and I suspect most professional statisticians work on a daily basis on that kind of machine, with that kind of software). Some of my work is done on a SAS VA server with 256 GB RAM. Still another part of my job is done on SAS EG in a Citrix environment connecting to large Oracle servers (health data for the entire french population). Different machines, different tasks, obviously. Work on server will usually be useful to build summary data that will then be transferred to a PC for modelling or any further computations. That could mean computing boxplot data on server to be used on PC. As an aside, I would not compute this to save space, but because it's statistically useful for some task. Also, there is an extra reason to download summary data, apart from network bandwidth limitation: it's simply forbidden to extract raw data from these servers, to protect privacy (although even on the server the datasets are anonymized).

While both tasks described above are acceptable, I personally would be reluctant to ask something on Rosetta Code that requires more than a PC, as it's by far the most widespread kind of machine, especially among students, who will likelly benefit most from such a site. But we could also have a category of "heavy" tasks, requiring specialized hardware and/or software, if there are enough users willing to contribute to such tasks (I won't, as my access to such machines does not allow any form of "entertainment" - though to be honest I did once run the Fortran 77 N-Queens program on an IBM z9).

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:12, 27 February 2018 (UTC)

==Phix test result glitch ?==

I noticed, in passing, that the first of the three test results in the Phix example shows the value 43 where other code (and a quick test just now with the the built-in R function) is returning 42.5

Perhaps some kind of edge case that might be worth checking ? Or just a variant interpretation ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:53, 13 February 2019 (UTC)

==R vs Wikipedia==
[[wp:Fivenum]] uses quartiles defined as members of set of input values. The R definition differs. Could do with a definitive definition or an explanation of the variants as part of this task, as, as others have stated, "do what R does" highlights issues. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:56, 13 February 2019 (UTC)

:[[wp:Percentile#Definitions]] shows common methods of computing percentiles. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:04, 15 February 2019 (UTC)
