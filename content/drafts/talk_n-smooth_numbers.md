+++
title = "Talk:N-smooth numbers"
description = ""
date = 2019-08-30T18:33:06Z
aliases = []
[extra]
id = 22497
[taxonomies]
categories = []
tags = []
+++

== Disagreement among implementations ==

There seems to be some disagreement among the various implementations about what the 30000th value is for the optional sub-task. Perl 6, Factor and Julia are the same, REXX has different answers. I've checked the Perl 6 several times and can't see where it may be wrong. (That is not to say that it ''isn't'' necessarily, I just can't find a problem yet.) And I am not familiar enough with Julia, Factor or REXX to be able to verify any of those, though having 3 of them agree seems to lean toward those being correct.

To help find the disagreement, I've uploaded several text files containing the first 30,000: [https://github.com/thundergnat/rc/blob/master/resouces/503.txt 503-smooth], [https://github.com/thundergnat/rc/blob/master/resouces/509.txt 509-smooth] and [https://github.com/thundergnat/rc/blob/master/resouces/521.txt 521-smooth] numbers as determined by Perl 6. Each file has 30,000 lines with one number on each line. The last line should be the 30,000th.

If another author could check against these files and let me know where we differ it would be appreciated. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 08:56, 29 August 2019 (UTC)

: The output of REXX for the optional sub-task is indeed incorrect, as some of the values are not really p-smooth. One such example is 38123 = 67 * 569. -- [[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 09:34, 29 August 2019 (UTC)

:: I found the problem.   Once the problem was found, it was so obvious.   I don't want to go into the embarrassing details too much,   but some of the simplest errors are so easy to overlook.   What triggered the   ''ah-ha!''   moment was the last line of the 1<sup>st</sup> batch of output,   the 10<sup>th</sup> prime (and all others above that)   were indexed incorrectly,   the program has an internal table of the first nine primes, all higher primes are generated.   Pesky little bug,   ... the primes were being generated correctly, but their   ''indices''   were incorrect,   which manifested itself only when indices for primes &gt; 23 were being used.   But many thanks for noticing the problem in the output(s).   I'm now glad that I put the (high) requirement in.   Without those ginormous numbers, the error might not have been detected.   Also, I had managed to overlook including the output for the 2<sup>nd</sup> task requirement, which was obviously missing.   Talk about the cobbler's children having no shoes.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:01, 29 August 2019 (UTC)

:: It was a fun project, albeit that I spent way too many an hour on it.   Not to mention all the coal I had to shovel to keep the steam-driven ole computer running.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:12, 29 August 2019 (UTC)

:::FWIW: Right now the REXX example is showing different outputs for the same inputs (See inputs for sub task 2 & 3) Obviously the ''code'' is  working correctly, it looks like may be a copy/paste error. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 11:33, 30 August 2019 (UTC)

:::: (To quote Charlie Brown)   Arrrrgh!   I'll get it right, eventually.       ...   Fixed.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:30, 30 August 2019 (UTC)
