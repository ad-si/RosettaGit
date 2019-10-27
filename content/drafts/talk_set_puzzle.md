+++
title = "Talk:Set puzzle"
description = ""
date = 2014-07-29T07:31:52Z
aliases = []
[extra]
id = 12915
[taxonomies]
categories = []
tags = []
+++

==What needs to be done?==
Is it the case that you need to find a deal of 9 cards that only has four sets?
It is unclear whether cards can be reused between sets, for example - with reuse:


```txt
Dealt 9 cards:
   green      one     oval     open
   green      one     oval  striped
   green      one     oval    solid
   green      one  diamond     open
   green      one  diamond  striped
   green      one  diamond    solid
   green      one squiggle     open
   green      one squiggle  striped
   green      one squiggle    solid

Found 12 sets:
   green      one     oval     open
   green      one     oval  striped
   green      one     oval    solid

   green      one     oval     open
   green      one  diamond     open
   green      one squiggle     open

   green      one     oval     open
   green      one  diamond  striped
   green      one squiggle    solid

   green      one     oval     open
   green      one  diamond    solid
   green      one squiggle  striped

   green      one     oval  striped
   green      one  diamond     open
   green      one squiggle    solid

   green      one     oval  striped
   green      one  diamond  striped
   green      one squiggle  striped

   green      one     oval  striped
   green      one  diamond    solid
   green      one squiggle     open

   green      one     oval    solid
   green      one  diamond     open
   green      one squiggle  striped

   green      one     oval    solid
   green      one  diamond  striped
   green      one squiggle     open

   green      one     oval    solid
   green      one  diamond    solid
   green      one squiggle    solid

   green      one  diamond     open
   green      one  diamond  striped
   green      one  diamond    solid

   green      one squiggle     open
   green      one squiggle  striped
   green      one squiggle    solid
```

--[[User:Paddy3118|Paddy3118]] 19:49, 10 February 2013 (UTC)

: You need the exact number of sets in the dealt cards, not more not less. Imagine you had to generate the puzzle for an applet such as [http://www.nytimes.com/ref/crosswords/setpuzzle.html this one]. And yes, you may use cards more than once. [[User:Fwend|Fwend]] 20:18, 10 February 2013 (UTC)

::Does that mean that for the 9 cards above I have found twelve sets, but I need to refine my algorithm to only deal 9 cards if they can generate a maximum of only four sets? I.e. add a post-filter? --[[User:Paddy3118|Paddy3118]] 02:20, 11 February 2013 (UTC)
::: If you deal 9 cards there should be exactly 4 sets in it, not more, not less. There are different ways of doing that. [[User:Fwend|Fwend]] 03:01, 11 February 2013 (UTC)

::Hi Fwend,I dealt 9 and got 12. How is my deal wrong? How are my sets wrong? I need help in understanding the task description, as I create a deck of 81 cards, draw 9, and get various numbers of sets out of it. Thanks. --[[User:Paddy3118|Paddy3118]] 08:01, 11 February 2013 (UTC)
:::My understanding from the task description is that there is an "arbitrary" requirement to return only deals where there are exactly 4 sets. A brute force way of doing this is to repeatedly sample 9 cards from the pack until you get a deal that contains exactly 4 sets. HTH. --[[User:Tikkanz|Tikkanz]] 09:49, 11 February 2013 (UTC)
:::: You're asked to create the dealer code for an ''existing'' puzzle game, that happens to have these rules. They may seem arbitrary, but I'm sure the puzzle designers had a reason for it, which probably has to do with consistency of presentation and degree of difficulty. [[User:Fwend|Fwend]] 10:40, 11 February 2013 (UTC)
::::: Sure. But from the point of view of someone solving the task on this site, the choice of 4 (or 6) is fairly arbitrary. I don't have any problem with that. I was just thinking that perhaps Paddy was over thinking the problem and that clarification might help. --[[User:Tikkanz|Tikkanz]] 11:20, 11 February 2013 (UTC)

== Java error msg ==

how can I fix this;

```txt

javac 1.8.0_05

SetPuzzle.java:2: error: package org.apache.commons.lang3 does not exist
import org.apache.commons.lang3.ArrayUtils;
                               ^
SetPuzzle.java:82: error: cannot find symbol
            cards = ArrayUtils.subarray(deck, 0, numCards);
                    ^
  symbol:   variable ArrayUtils
  location: class SetPuzzle
2 errors  

```
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:06, 28 July 2014 (UTC)

: I removed the dependency on a third party library, it should work now. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 22:52, 28 July 2014 (UTC)

:: ok- thanks! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:31, 29 July 2014 (UTC)
