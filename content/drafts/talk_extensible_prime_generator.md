+++
title = "Talk:Extensible prime generator"
description = ""
date = 2019-01-06T23:26:06Z
aliases = []
[extra]
id = 17529
[taxonomies]
categories = []
tags = []
+++

==Ada==
Extensible: The task name uses the word "extensible", but the description of the task does not explain what that means. I believe the task description is quite clear, but the Ada-solution I submitted was flagged with a red box that that solution was "not extensible", while other, very similar solutions, such as the one in C, where not flagged that way. By my understanding, either solution is actually extensible. If your usage of "extensible" is different, the task description would need a proper definition what it means to be "extensible".

:Sorry. I see someone has removed the incorrect flag I put on Ada by mistake. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:49, 16 April 2014 (UTC)

== Off by one? ==

Rust and Seed7 disagree with the others regarding the 10.000th prime.--[[User:Steenslag|Steenslag]] ([[User talk:Steenslag|talk]]) 17:31, 5 January 2019 (UTC)

:According to published tables, the 10,000th prime is 104,729 so the 'other' languages are correct here.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 22.35, 6 January 2019 (UTC)

:: See the webpage   <u>[https://primes.utm.edu/lists/small/10000.txt primes.utm.edu/small/10000.txt]</u>     (at the very bottom).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:55, 6 January 2019 (UTC)

::: That just confirms that the correct value is 104,729.  The Rust code was wrong; the author forgot that enumerations are zero based in Rust, so one has to ask for item number 9,999 to get the 10,000th prime.  I've corrected Rust.
::: It appears that Seed had a slightly different logic error:  it iterated and discarded the primes until the 10,000th one, then printed the next one.  I've fixed that, too.--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 23:25, 6 January 2019 (UTC)
