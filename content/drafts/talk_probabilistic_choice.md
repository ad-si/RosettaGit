+++
title = "Talk:Probabilistic choice"
description = ""
date = 2017-02-05T08:45:27Z
aliases = []
[extra]
id = 3272
[taxonomies]
categories = []
tags = []
+++

== formatting trouble==

I tried to add the SAS code, but it kept messing up the entry for tcl.  Here is it is, if anyone who can format it is watching:

data test;
do i = 1 to 1000000;
   x=rand("TABLE",(1/5),1/6,1/7,1/8,1/9,1/10,1/11);
   output;
   end;
run;

/* output from above is integer values 1,...,8 */
/* to show results with labels */
proc format;
value hebrew
  1 = "Aleph"
  2 = "Beth"
  3 = "Gimel"
  4 = "Daleth"
  5 = "He"
  6 = "Waw"
  7 = "Zayin"
  8 = "Heth";
run;

/* check results */
proc freq data = test; tables x; format x hebrew.; run;




== J solution ==

Well done, TBH.

--[[User:DanBron|DanBron]] 22:44, 24 December 2008 (UTC)

:Thanks, Dan.  As I'm sure you recognized, the local definitions are simply for improving readability. In particular, the one for partitions (prtn) exists only to document and emphasize the partition-set that is the left argument to Interval Index. Omitting that name would still keep things simple and readable: da=. (+/\pt) I. ?y # 0
:--[[User:TBH|TBH]] 18:55, 25 December 2008 (UTC)

TBH I don't know if it's worth it, but another possib. for counting is:  pa =. \:~ y%~ #/.~ da

:--[[User:Gaaijz|Gaaijz]] 18:41, 26 December 2008 (UTC)

:The problem I see with this is that it operates on the assumption that the actual proportions will fit the target proportions. This is to be tested, so cannot be assumed. The flaw is most clear when very small runs occur, in which cases the actual proportions are usually not in the same order as the targets. In such cases the code proposed above will pair proportions inaccurately. Nevertheless, I do appreciate being reminded of reflexive keyed-copy. --[[User:TBH|TBH]] 19:18, 27 December 2008 (UTC)

== Convergence ==

This task might be more interesting if the measured probabilities were displayed every (log n) runs (i.e. at 10, 100, 1000, etc.), to show the rate of convergence towards the expected hit ratio. --[[User:Short Circuit|Michael Mol]] 02:37, 8 November 2009 (UTC)

== "Random" vs. "Pseudorandom" ==
Not one solution on this page (at least among those that I have been able to read; I admit that J, for example, reads like Greek to me) uses actual random <span style="color:blue"><s>numbers</s><u>number generation</u></span>. They all use pseudorandom <span style="color:blue"><s>numbers</s><u>number generation</u></span> of one stripe or another. This distinction is more important than ever with the advent of '''actual''' random <span style="color:blue"><s>number</s><u>number generation</u></span> services online (Random.org, LavaRnd, and HotBits, among others), one might reasonably take the task specification literally and mark every solution as incorrect. What I have done instead is to change the text of the task from "generate a million items ''randomly'' subject to" into "generate a million items ''randomly'' (or, more likely, ''pseudorandomly'') subject to". I think this clarification makes things "right". Thoughts? --[[User:Balrog|Balrog]] 17:25, 16 April 2011 (UTC)

:The distinction you make doesn't seem to me to merit making as random is commonly used to include pseudo-random unless the difference matters.
:The task shouldn't distinguish between - and still, with your changes, doesn't distinguish between the use of random and pseudo-random numbers. As you have noted by the uniformity of the results, the task description was adequate for those authors of the ~30 existing language solutions. I don't think it would hinder your submission of another languages solution. --[[User:Paddy3118|Paddy3118]] 18:15, 16 April 2011 (UTC)

::"Anyone who considers arithmetical methods of producing random digits is, of course, in a state of sin." -John von Neumann :-) In any case, I pointed out above why I thought the difference matters. --[[User:Balrog|Balrog]] 20:06, 16 April 2011 (UTC)
:::And so? I take it that the above is a quote. Is it a misquote? does the context from the quote match our context? As J.C. is purported to have said "let he who is without sin, cast the first stone", maybe the quote goes against you in that the quote means everyone is doing it and so what ;-)
:::(Gosh, a bible quote from me!!?!)  --[[User:Paddy3118|Paddy3118]] 08:24, 17 April 2011 (UTC)
::::It was actually a joke. It was a joke when von Neumann first said it, and it was a joke when I quoted it. I suppose I had assumed that it would be taken as such. I'm sure if I had been speaking to you in person instead of typing at you here, it would have been. I'm sorry if you thought I was changing or cloudying the subject. --[[User:Balrog|Balrog]] 21:02, 17 April 2011 (UTC)

:So, ok, my thought was:  is 1 a random number?  --[[User:Rdm|Rdm]] 18:00, 16 April 2011 (UTC)
::HA! Good one. No, I was of course not being precise enough in my speech, and you caught me. Randomness or pseudorandomness is a characteristic of number selection or number generation, not of numbers themselves. I have corrected my original comment. --[[User:Balrog|Balrog]] 20:06, 16 April 2011 (UTC)

Hi Balrog, I've removed your change to the task description for the reasons I've stated above. --[[User:Paddy3118|Paddy3118]] 08:31, 17 April 2011 (UTC)
:That's fine. I don't care that much. --[[User:Balrog|Balrog]] 21:02, 17 April 2011 (UTC)

For all intents and purposes (at least in this task), "random" means "random or pseudo-random". If you want to add an example which uses a true random number generator service, go for it. Examples which use built-in pseudo-random number generators for most practical tasks are not incorrect (unless, of course, the nature of the task is to highlight the use of a true random number generator). Anyone who needs to be self-assured that they know the difference can make a task which uses one of these services (something like [[Random numbers/True random number service]] would be an appropriate task even without this argument going on). As far as this task goes, the distinction doesn't need to be made. People know what you mean by "random". (See [[User:Mwn3d/Word_mincing#Random_vs._Pseudo-random]] for a little bit more of my opinions on the situation). --[[User:Mwn3d|Mwn3d]] 16:50, 17 April 2011 (UTC)

===A note on "True randomness"===
I watched [https://www.youtube.com/watch?v=45Oet5qjlms this video] which points out that so called true random sources can have biases due to their measurement apparatus. In some ways, that leaves a random generator of whatever kind just as good as the randomness tests that it passes. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:45, 5 February 2017 (UTC)
