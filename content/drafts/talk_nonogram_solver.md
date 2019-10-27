+++
title = "Talk:Nonogram solver"
description = ""
date = 2017-11-09T11:01:14Z
aliases = []
[extra]
id = 17469
[taxonomies]
categories = []
tags = []
+++


### C++ Entry

What are the compilation instructions please?  Which compiler is used?  g++ (Ubuntu 7.2.0-8ubuntu3) 7.2.0 won't compile the source (modified with typedef unsigned uint;).

--LambertDW 03:41, 9 November 2017 (UTC)

:It should not depend on the Linux flavour. I use g++, I would expect anything after 4.9 and probably some before to work. It does require --std=c++11. I guess you've done that as my compiler suggests it when a c++11 feature is required when I haven't asked for it. If that doesn't fix it then perhaps you could include the pertinent error messages. --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:01, 9 November 2017 (UTC)


### Python Entry

Many details in the Python entry are not pythonic (or not efficient, or both), do you accept suggestions or a link to a fixed version? -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
: It's a wiki, posted code is not my private property.  Do whatever you think is right. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:22, 31 March 2014 (UTC)
:: OK, I have updated the Python entry. But we are often collaborating on RosettaCode, so it's very good to be civil and friendly :-) I don't want to step on your toes. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
:: Most of the run time of the D entry is used by genRow function. (And generating unsigned bytes instead of ints reduces the memory a lot, but leaves the run-time unchanged). So is is possible to use a 32/64 unsigned integer to represent the inner arrays (the possible configurations for each row), something like [2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2] => 81fff? And if a more compact representation is possible, is it leading to a faster program? (if you use 64 bits you can't have rows longer than 64) -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
::: It really depends on what operations are the slowest in the program.  Row/col runs could initially generate a lot of possible patterns, so reducing memory footprint can reduce cache/page misses, which may help, but it really depends.  Also keep in mind that whatever you use to track allowed cell values must have 4 states: must be filled, must be empty, can be either, and can be neither, which means it must have at least 2 bits per cell, so 64 bit likely will only represent up to 32 cells.
::: If I were to write it in C, I'd probably first try to find some other way to manage allowable patterns instead of generating all of them at once, which could be cleaner for a lower level language.  But that's just a hunch. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 01:48, 7 April 2014 (UTC)

### Ye Olde Black Cat In A Coal Cellar

The bonus program needs a little clarification.

```txt

E E E E E
E E E E E

```

is a unique 5x5 nonogram, which I am calling "Black Cat In A Coal Cellar". This may score highly for novelty, but only low for interest or difficulty. It is probably beyond the ability of an RC task to imbue a computer with artistic temperament. It may even be beyond my ability.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:33, 28 May 2016 (UTC)
