+++
title = "Talk:Run-length encoding"
description = ""
date = 2018-02-22T12:12:41Z
aliases = []
[extra]
id = 4081
[taxonomies]
categories = []
tags = []
+++

"The output can be anything." might be too open-ended. I would prefer it if all the solutions did it the same way, or something. --[[Special:Contributions/76.167.241.45|76.167.241.45]] 18:32, 24 April 2009 (UTC)

I totally agree. The current description makes comparing language solutions often pointless or misleading. The encoded output should be as described in the given example in the challenge.[[User:Martinfreedman|Martinfreedman]] ([[User talk:Martinfreedman|talk]]) 

I would prefer this task to use a run length encoding which will work at least on all ASCII characters. The current encoding can't represent [0-9]. A more flexible encoding is implemented [http://wiki.forthfreak.net/index.cgi?RunLengthEncoding here]. --[[User:IanOsgood|IanOsgood]] 19:27, 24 April 2009 (UTC)
* Run code has high bit set, remaining 7 bits are run length-1.
* Bytes in stream with high bit set are always encoded as a run, perhaps with a length of only one.

: The RLE I knew was: high bit says if that byte says how many "literal bytes" follow, or if the next byte must be repeated according to the value of the previous byte (clearing the high bit); so, e.g. ABCD would be encoded as, just to say, bytes 84 "A" "B" "C" "D"; of course, this way the maximum number of repetition (or longest literal sequence) is 128 (0 means 128). I've settled down to the task specification, but tried to create more usable examples... --[[User:ShinTakezou|ShinTakezou]] 23:08, 24 April 2009 (UTC)

::Woah! I didn't expect the page to take off this fast. 
::To be honest, I was just annoyed that the Wikipedia page had too much example code, but thought it would be a waste to just delete it, so I copied it over here, where I thought it might do some good. It's true that the task as it stands isn't formulated very well -- my goal was mostly to preserve the code from Wikipedia, which differed in how much it could encode (e.g. digits) and what it did output (String vs. Nested Array/List), but if the community is this active, it may be beneficial to just change the definition to require the implementations to be able to deal with arbitrary byte sequences as input, and/or require strings as output. 
::I did not exactly specify the way to encode the input because there were four variants I was aware of then:
::# Out of Band (as the task is described currently, with digits always signifying a run-length)
::# Escape character followed by run-length and run-character (e.g. AB\C3 -> ABCCC)
::## Escape character changes after every occurance in the input (in the hope of finding an unused character)
::# Two (or three, or ''x'') characters are always followed by the run-length (e.g. AA3 -> AAAAA)
::And you can encode the run-length itself differently for very long runs (multiple escape-sequences vs. Base128 encoded run-length)
::: -- [[User:DataWraith|DataWraith]] 14:42, 25 April 2009 (UTC)

(for above) --- * ''Run code has high bit set, remaining 7 bits are run length-1.''

This would make the task (or at least, the solution) ASCII-centric, that is, it wouldn't work for EBCDIC characters, nor ASCII-8 characters. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:01, 29 December 2012 (UTC)

The challenge as is, (preferably with a consistent encoded format as in the provided example), is a standard coding challenge used elsewhere. You could say it is a toy RLE rather than the real thing but it does illustrate a language's abilities in list/sequence processing of chars (and/or regex). [[User:Martinfreedman|Martinfreedman]] ([[User talk:Martinfreedman|talk]])

== PowerBASIC format wrong ==
I hesitate to flag the PowerBASIC entry as incorrect, but it would be better to include a mode where it gives the same results as the task description, rather than omitting counts of 1, (Although I understand that this gives a more compact RLE schema). --[[User:Paddy3118|Paddy3118]] 06:18, 14 January 2010 (UTC)
: I'll rework it tomorrow. (Just noticed this today; haven't been keeping track lately... sorry.) -- [[User:Eriksiers|Eriksiers]] 00:37, 28 January 2010 (UTC)

== Require decode counterpart ==

I'd like to see one of two things happen to this task:
# Add an explicit decode requirement, and add ENA template instances to the examples which don't have decode support yet.
# Create [[Run-length encoding (decode)]] as a counterpart to this one, and have the decode examples under this task moved there.

Subsequently, a third requirement, that the decode output, given input from the encode function, produce output identical to that given as input to the encode function.

My concern is that some languages here show decode support, while other's don't, and I'd like to see a symmetric decode implementation per-language shown in this task. --[[User:Short Circuit|Michael Mol]] 00:55, 18 January 2010 (UTC)

: Would the creation of a new task for decode stop an example entry of "See encode" if the encode page already has all three, encode, decode and comparison? 
: If the code is small enough, it might be better to keep it all together as in: [[http://rosettacode.org/wiki/Run-length_encoding#Python| Python (By regular expression)]]. --[[User:Paddy3118|Paddy3118]] 05:54, 18 January 2010 (UTC)

:: From a consistency standpoint, probably not. I understand your reservations, though. I don't have a good answer for it. --[[User:Short Circuit|Michael Mol]] 15:06, 18 January 2010 (UTC)

== Is built-in on topic? ==

--[[User:Zorro1024|Zorro1024]] ([[User talk:Zorro1024|talk]]) 10:55, 12 July 2015 (UTC)I added a thin wrapper around a built-in function in Ruby, is showing a built-in on-topic?

:Hi Zorro I'd just like to begin by mentioning that the convention is to add ones signature to the ''end'' of your contribution at that time.
:Chunk seemed quite like the Python groupby example, but on reading the docs on [http://ruby-doc.org/core-2.2.2/Enumerable.html Ruby chunk], it seems to be marked as deprecated? If it is deprecated you might not want to show an example using it as it could no-longer be thought of as idiomatic Ruby?
:(Signature at end): --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:42, 12 July 2015 (UTC)


:: Paddy, chunk is indeed not deprecated. It has two ways of being used of which one is deprecated, but I use the other. --[[User:Zorro1024|Zorro1024]] ([[User talk:Zorro1024|talk]]) 12:45, 20 July 2015 (UTC)
