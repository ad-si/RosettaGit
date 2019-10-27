+++
title = "Talk:Mind boggling card trick"
description = ""
date = 2018-09-19T10:29:12Z
aliases = []
[extra]
id = 21978
[taxonomies]
categories = []
tags = []
+++

==Original blogpost==
I had originally written a [http://paddy3118.blogspot.com/2016/02/mind-boggling-card-trick-in-python.html blog post] on the video and thought now would be a good time to see if it makes an RC task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:48, 25 August 2018 (UTC)

: I think it's a humdinger of a Rosetta Code task;   I spent some time in thinking of which way to go:   treat the card deck as a    ''list'',   or treat it as an   ''array''.   In the end, I thought that (for REXX), a list would be more idiomatic and easier to code.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:52, 26 August 2018 (UTC)

:: Thanks Gerard. In February 2016 I could not think of a way to turn it into a suitable RC task. The blog post got a fair amount of views however, so I tried again. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:23, 26 August 2018 (UTC)

==thoroughness of shuffling the deck==
I found (using the REXX program) that when doing any number of shuffles (including zero), the assertion holds.

I did some digging into my (REXX) code, and found that the creation of a "new" card deck (as it would come out of the manufacturer's box), was flawed. 

An almost perfect shuffle was being created by generating a sequence of numbers such that it appeared that there was a red card, a black card, a red card, a black card ...

I rewrote the   '''create'''   subroutine to mimic a new playing card deck     13 spades in order,   13 hearts,   13 clubs,   13 diamonds.

This was a bit of overkill, but now one could divide by   '''4'''   and use the remainder   (same as modulus in this case)   and find which suit the card was:

   (0 = spaces,    1 = hearts,   2=clubs,   and   3 = diamonds).

The pip could be found out by dividing by   '''13'''   and adding   '''1''',   but the pips weren't important for this card trick.

After finding out that it didn't make a difference, I left the new method of creating a new playing deck in,   as it didn't hurt, and I would probably need a creation routine to generate a new playing card deck in the future. 

Or, of course, you could just divide by   '''2'''   to test if it was a red or black card     red = '''1''' (or odd),   black = '''0''' (or even).

Then, I had the REXX program do various number of shuffles, including zero.   It didn't matter.   

<big>'''All that was necessary was a deck of playing cards that were   (exactly)   half red,   half black'''</big>, 

no matter what the order of the playing cards were and no matter how many shuffles (if any) were performed.

The REXX program allows an odd-numbered amount of playing cards,   and then the assertion starts failing   (with interesting, if not amusing, results).

Even if the shuffled (or not shuffled) card deck 1<sup>st</sup> half was all read, and the 2<sup>nd</sup> half was all black.

By the way, this could produce a situation where one of the "red" or "black" piles had no cards in it,   and cause my random swap cards routine to fail when generating a random number from '''1'''  to the number of cards, and the REXX '''random''' BIF routine coughed up an error code.   I fixed that by testing for a "null" (empty) pile of cards.   This won't happen if the playing card deck was already randomized by generating a neat sequence of card (odd/even, odd/even ... repeated,   or   red/black, red/black, ...    repeated). 


A single shuffle (as used in the REXX program) exchanged two random cards,   two shuffles exchanged four random cards,   the shuffle was performed two cards at a time, so it is possible to shuffle a card that was previously shuffled, and in somewhat rare occasion, exchange the same card (that is, no effective shuffle).    So, for a very few shuffles, instead of 26 shuffles, maybe 25 were effectively performed.   Meh.     It wasn't worth the (CPU) time to check for that special case, as the REXX program was performing a hundred thousand simulations at a time,   and   ''each''   simulation performed   (at that time)   26 shuffles.   (for the initial program test).   I wonder how many electrons I killed for this Rosetta Code task.       But no trees were felled, by gum!     An atom walked up to another atom and said, "Did you know scientists found out that electrons have mass?   "No", said the other, I didn't know they were Catholic!"   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:07, 29 August 2018 (UTC)

: (posting to myself):   I fixed the REXX code to not do a (random) shuffle on the same card.   "Anything worth doing is worth doing right".   (Hunter S Thompson).     "Anything worth doing is worth overdoing".    (Mike Jagger).   I ended up overdoing it for some   (well, maybe more then some)   of my REXX programs   (witness the boilerplate code for the REXX entry for this Rosetta Code task),   but what the hey!!!       I should stop talking to myself.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:43, 29 August 2018 (UTC)

==Jonathan Creek Investigates==

          '''''Jonathan Creek'''''  is a British mystery fiction and crime drama series produced by the 
          BBC and written by David Renwick.               (note added by Gerard Schildberger.)
 


I once read a book on optimizing code. It started "Obviously no programmer would deliberately waste system resources, but ....". Then there came this task. Let us forget the randomness, my tame card shark is able to shuffle any deck such that the result is B R B R ... B R. Each examined card is therefore black. All the unseen cards must be red. So the final result is a black stack of 26 red cards and an empty red stack. So no black cards in either stack and no red cards in the red stack. He now shuffles it so all the examined cards are red. Result an empty black stack and 26 black cards in the red stack. So no black cards in the red stack and no red cards in the black stack. He now shuffles the pack so that the first 26 cards are black and the last 26 cards are red. Result 13 black cards in the black stack and 13 red cards in the red stack (13=13). Let us not move a random number of cards but agree that moving 3 cards is essentially the same as moving 1 card 3 time, exactly the same if we avoid moving the same card more than once. Also swapping a black card in one pile with a black card in the other is pointless, also if both are red. So if I move a red card I now have 1 red card in the black stack and 1 black card in the red stack. For the general case let us say there are n black cards and g red cards in the examined set of cards, obviously g=26-n. In the unseen set there must be 26-n,which equals g black cards, and 26-(26-n), which equals n red cards. My tame card shark can arrange things so that the result is a black stack with n red cards and a red stack with g black cards. The result of any other initial assignment can be achieved by swapping red and black cards in the two piles, which as shown above always leaves the same number of red cards in the black pile as there are black cards in the red pile. Never before have I seen so many system resources deliberately wasted attempting to randomly shuffle cards for no purpose.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:04, 29 August 2018 (UTC)

:I guess I know that the magician isn't about to saw the person in half... It's all about the show! The theatrics! And maybe knowing a bit more than your neighbour ;-)
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:02, 5 September 2018 (UTC)

::See the 1998 Christmas Special, The Black Canary. In which the eponymous artist is cut in half, fatally, while strapped to a circular rip saw, or is she? At least James Bond escaped the laser. When several years later she commits suicide, fatally, in clear sight of a witness, or does she? Enter Jonathan...--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:28, 19 September 2018 (UTC)
