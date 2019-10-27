+++
title = "Talk:Percolation/Bond percolation"
description = ""
date = 2014-03-10T13:01:25Z
aliases = []
[extra]
id = 15993
[taxonomies]
categories = []
tags = []
+++

==OK, what have I done wrong in this task?==
Hi guys. It has been some time since this, and other [[Percolation|percolation tasks]] were added and they have collectively not seen much additions from the community.

I was wondering if the tasks were created in prime holiday time and so got lost, or if they were just uninteresting to the community? I did track down the page for [http://rosettacode.org/wiki/Category:Draft_Programming_Tasks tasks still in draft mode] and found that there were quite a lot. I decided to add a reference to the draft tasks from the [http://rosettacode.org/wiki/Category:Programming_Tasks Category: Programming Tasks] page in the hope of getting more people to scan the drafts. Maybe they should be reviewed and pruned (maybe some need shifting to a new category of abandoned tasks)?

Comments please. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:26, 21 August 2013 (UTC)

: Speaking for myself, I've been busy writing a paper on optimisation and so haven't had time to do much RCing (other than battering down spam). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:03, 21 August 2013 (UTC)

: As far as I'm concerned, I'm not too keen on writing this because it just looks a bit boring to implement, frankly.  And Perl 6 is currently not very good with multi-dimensional arrays, so it would be quite a pain to write right now. --[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 12:57, 21 August 2013 (UTC)

==D Entry==
The D entry is annotated with "It does not accomplish the given task." Could someone (like Paddy) explain better what's wrong in it? Is it perhaps because the probability goes in the other way (from 1 to 0)? If this is the problem then it's easy to change, but I thin it doesn't change the meaning of the task at all. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]]) 21:07, 5 March 2014 (UTC)
: I added the error note, but somehow the reason part of the template doesn't show.  It's this: at p=.5, the result should be somewhere around .5 too, but the D code gives 0.14.  Given that it's the average of 10000 runs, it's impossible that that was a random fluctuation, so there's got be a bug in the code. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:39, 6 March 2014 (UTC)
:: Thank you Ledrug, I'll try to fix that. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
:: The D entry is a translation of the Python entry, with some refinements. It seems the Python entry gives the same wrong results: http://ideone.com/ik57yE And is the C entry having the same problems?
::: You lost me. All entries other than D gives about 0.5 at p = 0.5, including the ideone output.  Given 1000 tries, that's about 500 successes on average, so random fluctuation sigma is sqrt(500) ~ 23.  At 3 sigma, i.e. 99.7% confidence, a correct solution should show between roughly 430 to 570 pass rate.  D shows 142 out of 1000, that's 15 sigmas away, which you need lottery-winning kind of luck to get.
::: It doesn't matter how the D code comes about; it shows result different from everyone else's and which is extremely statistically implausible, so the natural conclusion, at near 100% confidence even, is there's a bug. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 22:33, 9 March 2014 (UTC)

:::Hi Bearophile, you do state in the D entry that you make performance optimisations over the Python. Maybe the problem is there? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:09, 9 March 2014 (UTC)
:::: Rewritten D entry.-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
