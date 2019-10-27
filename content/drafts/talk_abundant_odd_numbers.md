+++
title = "Talk:Abundant odd numbers"
description = ""
date = 2019-05-19T21:46:11Z
aliases = []
[extra]
id = 22325
[taxonomies]
categories = []
tags = []
+++

==task requirements==
'''N'''   is a   ''nice number''   if   the sum of its factors is   <big> > </big>   '''N'''.

The word   '''nice'''   shouldn't be capitalized   (unless it's name after a person or a location or somesuch).



Perhaps some requirements should be stated, such as:
::::*   show the first   '''25'''   nice numbers   (with an index).
::::*   optionally, show the sum of the nice number's factors. 
::::*   show all output here.

....  or something along those lines.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:11, 16 May 2019 (UTC)

==Abundant numbers==

Actually, n is <strike>Nice</strike> an abundant number if the sum of its factors is greater than n.

Where did the term "Nice numbers" come from? Wikipedia never heard of Nice numbers (with this definition). Neither has Google (in this context).

However, there are a multitude of sources referring to these as [[wp:Abundant_numbers|abundant numbers or excessive numbers]].  

Exactly like in the task [[Abundant,_deficient_and_perfect_number_classifications]] and the easy part of the task [[Weird_numbers]]. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:09, 16 May 2019 (UTC)

: Er, no.   Nice numbers (according to the definition used in this task) uses the word   ''factors'',   not   ''proper divisors''.   If   ''factors''   was intentionally used in this context,   the factors of   '''12'''   are:   '''1, 2, 3, 4, 6, and 12'''.   As such, both   (all, as of this time)   of the programming entries are wrong, ...   unless the task's author meant to use   ''proper divisors''   instead of   ''factors''.   In addition, this task (implies) that nice numbers are to be listed, whereas the other task only requires a programming solution to   ''count''   the three types of numbers within a range   (and not to list them).   I deferred to the other programming entry's output and mimicked it's output, but not the task's definition.   The definition for "nice numbers" will need to be re-defined or re-worded.   For instance;   
:::: '''N'''   is a   ''nice number''   if   the sum of its factors is   <big> > </big>   '''2&times;N'''
: This new definition would make the '''REXX''' programming example correct, and make the '''RING''' programming example as partly incorrect in that it doesn't list the final factor   ('''N'''   in the list of factors).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:52, 16 May 2019 (UTC)]

:: Er, yes. Did you even bother to glance at the links I provided? Sample quote from [[wp:Abundant_number#Definition|Wikipedia]]:

    Abundant number
    Definition:
    A number n for which the sum of divisors σ(n)>2n, or, equivalently, the sum of proper divisors (or aliquot sum) s(n)>n.

::This task is asking for abundant numbers, regardless of what made-up name was put on it. You (Gerard) complained earlier about how RosettaCode has poor credibility in some circles, attaching made-up names to standardized concepts certainly won't help with that.

::: I didn't complain.   I merely mentioned what I observed elsewhere.   Please don't characterize my comments as complaining, this only adds to the problem of ad hominem attacks or bad characterizations.   Your sentence (as I read it) sounds like that I attached made-up names.   I did not.   What would help is keeping this discussion civil without your added commentary about my comments, whatever you may think of them.   People should feel free to comment about problems with descriptions in the task and/or the clarity/correctness of task requirements and definitions without rebuke.   This, unfortunately, is not the case here.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:46, 17 May 2019 (UTC)  

<blockquote>(quote)... this task (implies) that nice numbers are to be listed, whereas the other task only requires a programming solution to ''count'' the three types of numbers within a range...(end quote)</blockquote>

::So? Maybe we should have a task to find the abundant numbers and list them '''right justified!''', or in '''binary!''' or in '''Roman numerals!''' How do the display parameters have anything to do with finding abundant numbers? It's just needless proliferation of pointless minuscule variations of the same task. Now, I could get behind [[PureFox]]'s suggestion of listing the first several '''odd''' abundant numbers, at least there is some other concept to be exercised (as long as the name and task is updated to reflect what it is actually asking for). But as it stands, my vote would be for deletion. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:59, 17 May 2019 (UTC)

::: The   ''how''   in displaying parameters (the numbers) was never mentioned by me, it was the actual ''displaying''   (in whatever manner) of the numbers instead of merely counting them and only showing the count.   I never stated that the ''displaying'' of parameters (numbers) is the same as ''finding'' abundant numbers   (other than one is needed for the other).   This is a strawman argument and isn't worth the discussion.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:46, 17 May 2019 (UTC)

::Well, the Ring results correspond to the first 25 terms of [https://oeis.org/A005101 A005101] so I don't think there's much doubt that 'abundant' numbers are what CalmoSoft (who's not a native English speaker) had in mind even if he's calling them by an unfamiliar name and using the expression 'factors' rather than 'proper divisors'. Possibly 'nice' is a play on the name of the Greek mathematician, Nicomachus, who appears to have been the first to classify abundant numbers etc. circa 100 AD.

::Anyway, if this task is to be retained, perhaps we could make it a bit more interesting by asking for say the first 5 'odd' nice numbers to be calculated as well. I've added a tentative Go solution on this basis. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 13:00, 17 May 2019 (UTC)

== Task name description and requirements modified. ==

I've changed the task name, added a more verbose description and added some more interesting requirements. Should be pretty easily doable for most languages. Not carved in stone though. Better suggestions welcome. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]])

:Looks good enough to me. I've updated the Go entry to match. BTW getting a different answer to you for the 1000th abundant odd number. According to my reckoning, 498225 is the 1012th.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 22:03, 17 May 2019 (UTC)

:The problem seems to be that you (and also Gerard) are assuming that all the odd numbers will end in 5 when in fact there are a few which end in 1, 3, 7 or 9.

:The first such number is 81081 whose proper divisors are:

:1 + 3 + 7 + 9 + 11 + 13 + 21 + 27 + 33 + 39 + 63 + 77 + 81 + 91 + 99 + 117 + 143 + 189 + 231 + 273 + 297 + 351 + 429 + 567 + 693 + 819 + 891 + 1001 + 1053 + 1287 + 2079 + 2457 + 3003 + 3861 + 6237 + 7371 + 9009 + 11583 + 27027, which sum to 81543. So it's definitely an abundant odd number.

: The other 11 are: 153153, 171171, 189189, 207207, 223839, 243243, 261261, 279279, 297297, 351351 and 459459. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 23:47, 17 May 2019 (UTC)

:: Thanks for the heads up concerning odd abundant numbers not ending in '''5'''.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:01, 18 May 2019 (UTC)

:: Indeed, you are correct. Poor assumption on my part. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 00:29, 18 May 2019 (UTC)

==On Number Theoretic Tasks==
Perhaps we should have a tag "This solution is lame, please make it interesting and remove this tag". RC has too many tasks which are being solved by a loop which factorize a sequence of integers and then print something based on an if condition. Better would be if the author of these tasks indicated an interesting solution in the task description based on number theory. For this task I have added a reference which proves a number of properties of Odd Abundant numbers. 3 might be of interest to this task: there are no Odd abundant numbers with fewer than 3 prime factors; if a number is odd and abundant then so too are all odd multiple of that number; and Odd abundant numbers must satisfy the condition (p1/p1-1)*(p2/p2-1)..(pn/pn-1)>2. So p1=3,p2=5 then p3 must be less than 17 because (3/2)*(5/4)*(13/12)=2.03125 and (3/2)*(5/4)*(17/16)=1.9921875. Errors in early implementations uncovered the smallest Odd abundant number not divisible by 5. Let me consider p1=3, p2=7 and p3=11. (3/2)*(7/6)*(11/10)=1.9250000000000003 so there are no Odd abundant numbers not divisible by 5 with 3 prime factors. So p1=3, p2=7, p3=11, p4=13 -> (3/2)*(7/6)*(11/10)*(13/12)=2.0854166666666667 so this is a good place to start looking. So what is the smallest Odd abundant number not divisible by 3?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 21:29, 19 May 2019 (UTC)
