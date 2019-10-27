+++
title = "Talk:Parallel calculations"
description = ""
date = 2011-01-08T06:12:43Z
aliases = []
[extra]
id = 9008
[taxonomies]
categories = []
tags = []
+++

== Python example? ==

Sure looks to me as if it is outputting the smallest factor (of a list of numbers). Which I don't think is what the task is asking for.

:Hint: look-up <code>map</code> ''in the Python reference''. P.S. please sign your edits here, thanks. --[[User:Paddy3118|Paddy3118]] 07:13, 21 December 2010 (UTC)

: I didn't doubt the parallelism. My intention for the task was that to have multiple threads/processes do some parallel calculations (factorize the numbers), and then have the main process receive the results, and proceed with other calculations based on the results.  Finding an extremum in these numbers is just an example, the main process might well want to do other things with the returned lists. The current Python solution is different in that it respect: It searches for the minimum already in the sub-process (returning a single number from ''lowest_factor''), but this is not what the task asks for: Return that number and its prime factors. --[[User:Abu|Abu]] 09:02, 21 December 2010 (UTC)

:: Hi Abu, the lowest factors of each of the numbers ''are'' returned to the main process where the minimum is then found. A full prime factorization of the numbers seems wasteful given the task description and that the smallest prime factor is computed first. 
:: Should you the modify the task to state explicitely that all prime factors should be returned or should you keep this aspect un-specified? I, (of course), think that the current task keeps the focus on the parallelism and that specifying that all factors should be computed when only the smallest is needed goes against your need for speed. --[[User:Paddy3118|Paddy3118]] 10:28, 21 December 2010 (UTC)
::: Hi Paddy, I don't want to be picky, but task already does that. It says "... and return that number and its prime factors". The two solutions so far seem to do that, and I didn't think it would be a problem to change the Python version accordingly. It isn't even necessary to write a new function, just call the solution in the [[Prime decomposition]] task. --[[User:Abu|Abu]] 10:46, 21 December 2010 (UTC)
:::: Not Picky - necessarily exacting! I missed the import of the final s in your quote above. Thanks. I will fix it.

==Ada solution==
Concerning the ADA solution: My ADA reading abilities are very poor, where does the extra 32767 in the result list come from? I would expect that just 12878611 and 47 101 2713 is the right answer. --[[User:Abu|Abu]] 13:09, 21 December 2010 (UTC)

: Yeah, you are right. The Results array is a 2D array with width of the longest factor list. The winning list is shorter and Put() outputs all values of its parameter. I changed it to only use the correct slice. --[[User:Oenone|Oenone]] 13:39, 21 December 2010 (UTC)

== "Largest minimal factor"? ==

I still don't really understand what "largest minimal factor" means. Does it mean that you have to take the smallest prime factors of each number and find the largest one of those numbers? The "that is" parenthetical doesn't really help. --[[User:Mwn3d|Mwn3d]] 05:42, 8 January 2011 (UTC)

:I read it as saying that what was needed was a number from those given, that has large prime factors. The way they find such a number is to find the lowest of the prime factors of all the numbers and select a number which has this [[largest]] of these [[lowest]] prime factors.

:As an example, the numbers 22, 33, 44, 55, and 275 have lowest prime factors of 2, 3, 2, 5, and 5 respectively. The largest of these minimum prime factors is 5. A compliant routine could return ''either'' the number 55 with its prime factors 5 and 11; ''or'' the number 275 with its prime factors 5, 5, and 11. --[[User:Paddy3118|Paddy3118]] 06:12, 8 January 2011 (UTC)
