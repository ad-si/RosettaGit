+++
title = "Talk:Generate Chess960 starting position"
description = ""
date = 2014-11-30T06:23:06Z
aliases = []
[extra]
id = 17612
[taxonomies]
categories = []
tags = []
+++

==clarifying wording==

In the first rule, the wording states   ''··· all eight pawns must be placed on the second rank''.

There are sixteen pawns. 

How about   ''A player's eight pawns must be ···''   or something similar. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:53, 8 May 2014 (UTC)
:I don't think that's necessary.  There are also two queens, two kings, four rooks and so on.  I think it's obvious enough that we're talking about the placing of the pieces for ''each'' camp.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 05:52, 8 May 2014 (UTC)

: It doesn't really matter for this task (since we only display pieces for the first rank, for one player). Hypothetically speaking, though, each player could be thought of as having a second rank, which would allow all 16 pawns to be placed. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:54, 8 May 2014 (UTC)

:: There are eight ranks on a chessboard, the 1<sup>st</sup> rank is where white's major and minor pieces are initially positioned, rank 8 is where black's major and minor pieces are initially positioned.   The 2<sup>nd</sup> rank is where (all 8) white's pawns are initially positioned, the 7<sup>th</sup> rank is where all the black's pawns are initially positioned.   The black player doesn't have a 1<sup>st</sup> or 2<sup>nd</sup> rank in that those ranks are occupied by white pieces.   Ranks aren't owned by either white or black, the ranks are numbered from the "bottom" of the chessboard, where the "bottom" is occupied by the white player's major and minor pieces.   As long as the Rosetta Code task is stating the placement of all the pawns, it should be accurate and conform to the notations and wording of the Fédération Internationale des Échecs (FIDE), aka, the World Chess Federation. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:28, 8 May 2014 (UTC)

==random starting position==
A trivial REXX program (of two statements):

```rexx
if random(0,1)  then say 'NBQRBKNR'
                else say 'QBNRBKNR'
```

would, in the strictest sense, fullfill a random Chess960 starting position   (albeit only two random positions).

However, I believe the spirit of the requirement of ''random'' be that the random position would produce ''any'' of the 960 ''possible'' starting positions. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:53, 8 May 2014 (UTC)

:[http://www.xkcd.com/221/ relevant] :-) --[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 05:49, 8 May 2014 (UTC)
:: Excellent!     ''':-)'''

To this end, I wrote a REXX program (2<sup>nd</sup> programming entry) that randomly generates all possible unique 960 Chess960 starting positions and it shows a log of the results (unique starting positions) after each one-thousand generations.

This would make a good extension to the requirements to verify that the programming examples being used to create a random Chess960 starting position do indeed produce all possible starting positions. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:42, 8 May 2014 (UTC)

:Modified task description to force entries to be able to produce one of the 960. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:52, 8 May 2014 (UTC)

:Gerard, I did a similar test (for Ruby) and it usually generates all 960 poisitions in about 7500 tries. I suspect the REXX code is biased towards rooks on the right side, but I could very well be wrong...--[[User:Steenslag|Steenslag]] ([[User talk:Steenslag|talk]]) 14:20, 12 May 2014 (UTC)

:: Yes, the placement is biased to the right because of the requirement (rule) that the king has to be placed between the two rooks, which necessitated (in the REXX code) the shifting of the placement of the 2<sup>nd</sup> rook to be placed at least two positions to the right.   I don't know if the addition of more code which would allow the placement of the 2<sup>nd</sup> rook to be also placed two positions the left of the 1<sup>st</sup> rook would provide better randomness of rook placement. I'll be looking into that possibility.   Here is a histogram of one million random placements (of both rooks):

```txt

(1st one-million trial)
file 1=166381 
### ==================

file 2=166437 
### ==================

file 3=194882 
### ======================

file 4=227628 
### ===========================

file 5=269865 
### =================================

file 6=324213 
### =========================================

file 7=243282 
### =============================

file 8=407312 
### ======================================================


(2nd one-million trial)
file 1=166404 
### ==================

file 2=166747 
### ==================

file 3=194618 
### ======================

file 4=227543 
### ===========================

file 5=269279 
### =================================

file 6=325874 
### =========================================

file 7=240176 
### =============================

file 8=409359 
### ======================================================


```
 
{''file''   as in   ''rank''   and   ''file''   on a chessboard.}

-----

(Follow-up on the random placement of the two rooks.)    I re-worked the REXX code and it now has a much more randomness in the (proper) placement of the two rooks. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:19, 12 May 2014 (UTC)

```txt

(1st one-million trial, new code)
file 1=269306 
### =====================================================

file 2=246706 
### ================================================

file 3=241984 
### ===============================================

file 4=240403 
### ===============================================

file 5=242776 
### ===============================================

file 6=242128 
### ===============================================

file 7=245850 
### ================================================

file 8=270847 
### ======================================================


(2nd one-million trial, new code)
file 1=269306 
### =====================================================

file 2=246706 
### ================================================

file 3=241984 
### ===============================================

file 4=240403 
### ===============================================

file 5=242776 
### ===============================================

file 6=242128 
### ===============================================

file 7=245850 
### ================================================

file 8=270847 
### ======================================================


```

The REXX program to produce the histograms (shown above) is:

```rexx
/*REXX program generates a histogram of 100,000 rook placement positions*/
parse arg seed times .                 /*obtain optional args from C.L. */
if times==''  then times=100000        /*use default for TIMES?  {100k} */
if seed\==''  then call random ,,seed  /*if SEED specified, use the seed*/
rooks.=0                               /*zero the rook position counters*/

  do t=1  for times  /*═════════════════════════════════════════════════*/
  r1=random(1,8)                       /*place the  first rook on rank1.*/

            do  until  r2\==r1  &  r2\==r1-1  &  r2\==r1+1
            r2=random(1,8)             /*find placement for the 2nd rook*/
            end   /*forever*/

  rooks.r1=rooks.r1+1                  /*bump rook (r1) position counter*/
  rooks.r2=rooks.r2+1                  /*  "    "  (r2)    "        "   */
  end   /*t ════════════════════════════════════════════════════════════*/

mx=0;   do j=1  for 8;  mx=max(mx,rooks.j); end   /*find max histo value*/

  do k=1  for 8                        /*display the eight files numbers*/
  say 'file' k"="rooks.k copies('=', 60 * rooks.k % mx)
  end   /*k*/                          /* [↑]  display a nice histogram.*/
                                       /*stick a fork in it, we're done.*/
```


Many thanks to (user) Steenslag for pointing out the un-randomness of the rook placement in the REXX code. 

I've updated the REXX programs.-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:19, 12 May 2014 (UTC)

Even with the new REXX code (of better rook placement), it still takes the same amount of generations to produce all 960 variations. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:52, 12 May 2014 (UTC)

-----
My own code (in the Construct section) was biased too. The rook positions histogram should look like: {1=>360, 2=>252, 3=>192, 4=>156, 5=>156, 6=>192, 7=>252, 8=>360}, as it does for the 1920 rooks in the 960 startpositions, but it did not. It was not even remotely symmetrical. It appeared I was off by one on a rand call.-[[User:Steenslag|Steenslag]] ([[User talk:Steenslag|talk]]) 20:47, 12 May 2014 (UTC)

==unviewable symbols==

When I view the task page, the Chess symbols in Unicode appear as small square boxes with numbers in them (on Firefox Aurora), or empty boxes in Microsoft Internet Explorer, both under Windows/XP.   Is there something special that needs to be done to make them viewable? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:21, 8 May 2014 (UTC)

:I also find them hard to see.  Maybe increase the font size.  Anyway that's why I also allowed to use roman letters.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 06:27, 8 May 2014 (UTC)

:: Er, no.   My screen is about two or three miles wide, and I can make the font ... well, ginormous.   The symbols (which are in a true square box) are as follows:

```txt

 ┌──┐┌──┐┌──┐┌──┐┌──┐┌──┐┌──┐┌──┐
 │26││26││26││26││26││26││26││26│
 │58││57││56││54││57││55││56││58│
 └──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘

```

::They are small in "normal" viewing mode and they occupy "one character", so you can image how small the glyphs are. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:44, 8 May 2014 (UTC)

:::The numbers are the Unicode code points in hex that have no glyph in your current font. For fonts that support these symbols see for example [http://www.alanwood.net/unicode/miscellaneous_symbols.html Test for Unicode support in Web browsers].
:::In Firefox you would need to change the Sans Serif font (Preferences - Content - Fonts&Colours/Advanced...). --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 09:36, 8 May 2014 (UTC)

As an aside, some languages don't support Unicode characters. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:44, 8 May 2014 (UTC)

As another aside, the chess symbols have been in Unicode for more than 20 years now. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 19:31, 8 May 2014 (UTC)

==Python: Correct by construction? ==
Originally wrong but corrected. This is the check:

```python
from pprint import pprint as pp

def check(start):
    'Check that B/b are on different colours'
    assert (start.index('b') % 2) != (start.index('B') % 2), ''.join(start)

def generate_all_bishpos():
    start = list('______')  # Should be all-but Bishops but any six chars will do

    starts = []
    for bishpos in range(len(start)+1):
        start2 = start[::]
        start2.insert(bishpos, 'B')
        for bishpos2 in range(bishpos+1, len(start)+2, 2):
            start3 = start2[::]
            start3.insert(bishpos2, 'b')
            check(start3)
            starts.append(tuple(start3))
    return starts

starts = sorted(generate_all_bishpos())
print('Generated all separations of Bishops:')
pp(starts, width=50)

import re
from random import choice

def random960():
    start = ['R', 'K', 'R']         # Subsequent order unchanged by insertions.
    #
    for piece in ['Q', 'N', 'N']:
        start.insert(choice(range(len(start)+1)), piece)
    #
    bishpos = choice(range(len(start)+1))
    start.insert(bishpos, 'B')
    start.insert(choice(range(bishpos + 1, len(start) + 1, 2)), 'b')
    return start
    return ''.join(start).upper()

#print(random960())

def _random960():
    "Exact copy of the one above except other pieces are all '_'"
    start = ['_', '_', '_']         # Subsequent order unchanged by insertions.
    #
    for piece in ['_', '_', '_']:
        start.insert(choice(range(len(start)+1)), piece)
    #
    bishpos = choice(range(len(start)+1))
    start.insert(bishpos, 'B')
    start.insert(choice(range(bishpos + 1, len(start) + 1, 2)), 'b')
    return start
    return ''.join(start).upper()

print('\nCheck we can generate all separations of bishops randomly:')
s, i = set(), 0
while len(s) < len(starts) and i < 9999:
    start = _random960()
    check(start)
    s.add(tuple(start))
    i += 1

s = sorted(s)

if s == starts:
    print(' Generated all the %i separations in %i attempts' % (len(starts), i))
else:
    print(' Error! could not do it in %i' % i)


print('\nCheck we can generate all 960 randomly:')
s960, i = set(), 0
while len(s960) < 960 and i < 99999:
    start = random960()
    check(start)
    s960.add(tuple(start))
    i += 1
    if not i % 500:
        print('  @%5i %5i/960' % (i, len(s960)))

s960 = sorted(s960)

if len(s960) == 960:
    print(' Generated all the %i separations in %i attempts' % (960, i))
else:
    print(' Error! could not do it in %i' % i)
```


{{out}}

```txt
Generated all separations of Bishops:
[('B', '_', '_', '_', '_', '_', '_', 'b'),
 ('B', '_', '_', '_', '_', 'b', '_', '_'),
 ('B', '_', '_', 'b', '_', '_', '_', '_'),
 ('B', 'b', '_', '_', '_', '_', '_', '_'),
 ('_', 'B', '_', '_', '_', '_', 'b', '_'),
 ('_', 'B', '_', '_', 'b', '_', '_', '_'),
 ('_', 'B', 'b', '_', '_', '_', '_', '_'),
 ('_', '_', 'B', '_', '_', '_', '_', 'b'),
 ('_', '_', 'B', '_', '_', 'b', '_', '_'),
 ('_', '_', 'B', 'b', '_', '_', '_', '_'),
 ('_', '_', '_', 'B', '_', '_', 'b', '_'),
 ('_', '_', '_', 'B', 'b', '_', '_', '_'),
 ('_', '_', '_', '_', 'B', '_', '_', 'b'),
 ('_', '_', '_', '_', 'B', 'b', '_', '_'),
 ('_', '_', '_', '_', '_', 'B', 'b', '_'),
 ('_', '_', '_', '_', '_', '_', 'B', 'b')]

Check we can generate all separations of bishops randomly:
 Generated all the 16 separations in 117 attempts

Check we can generate all 960 randomly:
  @  500   368/960
  @ 1000   573/960
  @ 1500   692/960
  @ 2000   768/960
  @ 2500   831/960
  @ 3000   871/960
  @ 3500   892/960
  @ 4000   909/960
  @ 4500   922/960
  @ 5000   933/960
  @ 5500   941/960
  @ 6000   945/960
  @ 6500   950/960
  @ 7000   951/960
  @ 7500   953/960
  @ 8000   954/960
  @ 8500   954/960
  @ 9000   955/960
  @ 9500   955/960
  @10000   955/960
  @10500   955/960
  @11000   956/960
  @11500   957/960
  @12000   959/960
  @12500   959/960
  @13000   959/960
 Generated all the 960 separations in 13166 attempts
```


== "a bit wasteful" ==

A comment on the main page classified pregenerating the 960 positions as "a bit wasteful", and I'm wondering just how wasteful it winds up being.

I did some measurements myself, and I found that the total memory consumed was a fraction of the resources consumed by the language implementation at startup. And, as a one-time cost this seems reasonable. And for more frequent use? Can we get more efficient than the table lookup approach, for this specific problem? (Approximately 7680 bytes to represent the table, and a bit more for the code, and a few microseconds to generate the values. I could see 7680 bytes being too much on a microcontroller, but not so sure about it being a problem on something bigger, like a phone.)

Anyways, it seems to me that "wasteful" would be not using the computer, but I'm also concerned that I might be overlooking an important issue.

(This is not an entirely idle question - I have been seriously wondering about the value of computers to society - you can't eat them, you can't wear them, they mostly seem fad driven, I've been frequently told how they are destroying civilization, I see billions of dollars invested in just sitting there watching them (ops, security, etc), and the high end systems tend to be unreliable in all sorts of ways. I'm aware of large teams of people basically reinventing the wheel on a daily basis in the name of "code reuse". I'm seriously wondering if I shouldn't have gone into agriculture, instead - at least there the benefits are relatively obvious. Of course, I don't expect this forum here to answer all my questions, but I do have a lot of questions about our criteria. Thanks for bearing with me.)

I suppose another way of looking at this would be to suggest that the problem itself is wasteful? Or are these questions and thoughts too harsh? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:55, 14 May 2014 (UTC)

: Er, sorry, I see that what I wrote was ambiguous.  I wasn't complaining about the pregeneration of the 960 entries, but about the generation of 40320 permutations, only to throw most of them away.  Indeed, pregenerating the 960 entries makes perfect sense, especially if you can do it at compile time, and amortize it over many runs.  I've clarified my comment, and added a faster way to pregen the 960 entries via construction.  Thanks for pointing out the ambiguity in what I wrote.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 18:47, 14 May 2014 (UTC)

:: Fair enough. (Though perhaps this could also be taken as an indication that generating permutations is not implemented efficiently enough? I honestly don't know. I think I was mostly wondering because the permutation approach seemed to me to be efficient enough for this problem (60 milliseconds on my overloaded laptop, though granted I wasn't using perl in this particular case), and I did not know what the issue was. And you have clarified that.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:18, 14 May 2014 (UTC)

:I make the opposite view about computers. Every winter I find myself marvelling at the reduction in the amount of cars seen with their hood up left broken down by the side of the road. I put that down to modern engine management systems. 
:I also see our double decker busses just going about their business. When I was a lad, they would cough and wheeze belching smoke as they laboured slowly up hills. Now their engines have engine management systems that get more from the fuel and help diagnose problems before they lead to breakdowns.
:As you can see I think of computers as having a net positive effect. (Guided bombs and drones notwithstanding). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:45, 14 May 2014 (UTC)

:: Perhaps, indeed. (I'm not quite sure that my amish relatives would really appreciate that line of thinking, but I can certainly see its validity.) Still, from a personal contribution point of view, I feel I should be capable of doing a lot more than what I have (if only I could better identify a reasonable set of priorities and motivations for myself which fit the fragmented aspects of society which I happen to admire). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:18, 14 May 2014 (UTC)

:Wow that's pretty wide a topic you're stepping in here.  Since you're mentioning the Amish, it seems that you're willing to go as far as questioning the benefits of technology, not just computers, for society.  If we were to have this debate, we'd be aligning an endless list of pros and cons (some of them have already started to be listed above) and most people nowadays would conclude that the list of pros overwhelms the list of cons.   I'm not even willing to go this road:  I'm not learning a computer language because I think it makes the world better.  I'm doing it because it's fun, because it's interesting and because when I write a nice program (which is sadly quite rare), it's gratifying.   I'm personally not at all attracted to a simpler, agrarian way of life, mostly because I tend to think it would be ''dull''.  I may be wrong, which would be tragic, but that's how I see it and there are very little reasonable ways for me to find out anyway.  I'm pretty sure when someone plays angry bird on his smartphone or when a teenage girl saves a 1GB high-definition recording of Justin Bieber concert on her hard drive, there are good reasons to believe this is a waste of resources.  But that will always be a subjective point of view because we as humans do enjoy a wide variety of things and certainly not all of them are related to food or clothes.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 20:54, 14 May 2014 (UTC)

::You are right about going wide afield here. Of course, I'm not amish myself, though I'm also in some ways not too far from that. I happen to like indoor plumbing, for example, despite having significant practical experience in doing without. Then again, I remember reading a writeup of about the amish some years ago, in mit technology review which pointed out that in some ways the amish are more technically advanced than most of the rest of society (with an example of repurposing a truck's differential transmission for some project which I forget the details of). They do question the value of things and they do reject various aspects of technology ruthlessly and sometimes seemingly arbitrarily, but they are also innovators, in their own way. You sort of have to be, with their lifestyle and background. But for myself, with an appreciation of their perspective, of your perspective, of the perspectives of the people caught up in the tragedies of smart bombs and drones, and so on, I'm left questioning things a lot, which other people take for granted. I'm not sure that that's the best use of my time, but what is? (If I knew that, I wouldn't be asking.) Anyways, I'm sure I'm unimaginably dull, myself, to many people, and it helps to understand other people's subjective views of things. Or, I think it does. Thank you. (Hopefully that's enough background to understand why I was asking about the concept of wastefulness here? My perspective requires I question some things that other people take for granted, and I probably take for granted things which other people routinely question. I hope I'm not coming across as being too negative about any of this.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:43, 15 May 2014 (UTC)
