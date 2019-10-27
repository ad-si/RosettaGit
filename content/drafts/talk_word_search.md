+++
title = "Talk:Word search"
description = ""
date = 2019-01-19T00:22:27Z
aliases = []
[extra]
id = 20674
[taxonomies]
categories = []
tags = []
+++

== Not sure how important the output format is ==

The task did not really say much about how the result should be displayed. Not sure if that matters... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:13, 26 March 2016 (UTC)

: As long as you can check the result, it's fine. -- I get the impression that in your solution there isn't much overlap, which wouldn't disqualify it, but maybe I should specify that? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 10:31, 26 March 2016 (UTC)
:: Yes, I think I will require at least 10 overlaps. (I'll have to change my own code as well) [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 10:36, 26 March 2016 (UTC)

== Overlap query ==
If you have "din" and "other", can a "not" be planted on all-existing letters, given that they are bits of different words? Would there be a difference between straight-line instances of that kind and orthogonal ones? [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 14:57, 30 June 2017 (UTC)
:Yes, that's allowed. The only restriction given is that you cannot embed a word completely in another single word, like putting "one" inside "done". [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 21:26, 30 June 2017 (UTC)
:: Maybe I should drop that restriction because it complicates things, and my own (Java) code doesn't even fully support it. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 00:11, 1 July 2017 (UTC)
::: Agreed. Maintaining lists of word indexes for each cell and checking the intersection is empty, would be overkill. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 04:30, 2 July 2017 (UTC)
:::: It's not that hard to implement though: just check if a word can be contained in the list of currently placed words. If it can, require that when it is placed that it occupy at least one previously unoccupied position. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:25, 3 July 2017 (UTC)
::::: That's what the Java code does, but it still allows a larger word to embed a smaller word later. Imagine that you've placed the word "one", then the word "done" could be laid over it.[[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 17:06, 3 July 2017 (UTC)
:::::: Oh, I overlooked that possibility. Still... you could pregenerate a list of words (maybe 50 of them) at the start and run the containment check against that list. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]])
::::::: Sure, you could check it, but the task already has plenty of constraints, I don't think this one is really necessary. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 18:37, 3 July 2017 (UTC)
== Bad link to word list ==
The task description contains a broken link to the word list 'unixdict.txt'.  Perhaps the link could be changed to a working link for the original file that can be found at the internet archive, 'The Wayback Machine', [https://web.archive.org/web/20160419090505/http://www.puzzlers.org/pub/wordlists/unixdict.txt unixdict.txt]. --[[User:Demivec|Demivec]] ([[User talk:Demivec|talk]]) 08:05, 18 January 2019 (UTC)
