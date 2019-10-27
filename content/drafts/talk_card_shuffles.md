+++
title = "Talk:Card shuffles"
description = ""
date = 2019-04-17T04:18:33Z
aliases = []
[extra]
id = 19068
[taxonomies]
categories = []
tags = []
+++

== task needs more definition ==
Task needs more definition so we can know whether an implementation is suitable or not. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:31, 28 April 2015 (UTC)
:It's a draft so OK. What do you want to see? --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 16:21, 28 April 2015 (UTC)

:+1 to Rdm. It Could do with the algorithm being on the page. Remember we want to be able to compare language examples which can be better done if the algorithm is shown, and maybe restricted to just the one method? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:24, 28 April 2015 (UTC)

::So the two I did in Java are the riffle and the overhand. I can explain them both on the page but if you think it'd be better to just require one and leave the rest for bonus then I'll explain them both here to make it easier to decide. The riffle is where you cut the deck in half and "flip" the two halves together in an alternating fashion (sort of like the merge in a merge sort). The "human imperfections" that I thought of with this are an imperfect initial cut and accidentally "flipping" more than one card from one half at a time. The overhand is where you hold the deck in one hand and cut part of the deck (I guessed around 20%) from the top of the deck and move it to your other hand (the top of the pile in that hand if there are already cards there). Repeat until the whole deck has been moved to the other hand. The "human imperfections" I thought of for that one are randomly varying cut sizes and occasionally choosing to add the next cut to the bottom of the deck in the "other hand". Does either one of those sound like a better main method to implement? --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 16:40, 28 April 2015 (UTC)

::: For example, let's say the language provides built in "shuffle" or "riffle shuffle" primitives. What else would you want, besides the use of these hypothetical primitive operations? Or, if their unadorned use is acceptable, how would we determine whether or not the language spec satisfies this task? 

::: Note that we already have some "Shuffle" tasks, and that "[[Knuth shuffle]]" is very different from "[[Perfect shuffle]]" and that neither of those two in any way matches "[[Best shuffle]]".

::: So one issue is: are you going for a "random process" (which has its own awkwardness) or are you going for a "deterministic process" or"semi-deterministic" process (such as a card shark might want to employ)? A deterministic "one card from each side" riffle shuffle is going to be different from a "one or two cards randomly" from each side shuffle (and that's going to often have a lump at the end which is more than two cards). Another variant might involve lumps of one to four cards. Another variant might instead do a "0 or 1 card from each side, with 50% odds until one side runs out" type of approach. 

::: Or, if you want overhand shuffle, how many times should the deck be cut? Also, do you want to support stacking the deck? Or do you want to leave that as a potential modification for later?

::: Anyways, it's not really my place to tell you what it is that you wanted to say in your definition of this task. All I can do is point out some of the possibilities which the current task suggests to me, and ask you what it was you really wanted to say. 

::: It sounds like you've got a decent idea of what you're going for here, though maybe you should split this into two tasks - overhand and riffle shuffles are different algorithms? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:43, 28 April 2015 (UTC)

::::I find myself saying "yep" a lot reading this. I guess I was thinking it'd be more fun if some of those things were left open but I can see how a little bit more specification might make it better.

::::For the question on if a language has one built in I guess once we hammer out the details we want for whatever shuffle method is in this task(s) it will be easier to decide if the built-in satisfies it. I'm all for leaving it unadorned if it does what we ask.

::::For the riffle-specific questions I think I spelled out the "perfect" version of it in my response to Paddy. That can be reworked to add whatever details we decide we would need. I thought the "perfect" version of whatever sort we have would be pretty deterministic or have clear places where random numbers are needed. In the case of the riffle shuffle I described, the "perfect" version is 100% deterministic while the bonus "human imperfections" allow for randomness.

::::For the overhand-specific questions it isn't really described anywhere how many times you need to cut so I picked 5 (20% each time). I think that's a reasonable choice for a deck of cards so that would be my choice for the "perfect" version of that. That also makes it deterministic. Once again the randomness comes in with the imperfect cuts humans make. So that's where the random numbers come in.

::::A split isn't out of the question either. I guess we could start with both of the shuffles I described and see if anyone makes any others. I think we should make them subpages of [[Card shuffles]] if we do that though (e.g. [[Card shuffles/Overhand]] and [[Card shuffles/Riffle]]). Since these are based on shuffles used for playing cards I think it would be good to group them separately from more CS-oriented shuffles like [[Knuth shuffle]]. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:28, 28 April 2015 (UTC)

:::::I'll note also that the java implementation uses 1 iteration and 10 iterations for its examples, and uses an example "deck" of 20 integers. Anyways, as much non-procedural detail as makes sense probably goes in the task description. And, I guess also think about what if the language built-in shuffle is Knuth - it's sort of sounding like you don't want that - that you want shuffles with imperfections. So that "needs to be imperfect" belongs in the task description, I think. Like, maybe these numbers are for use in a statistics class, or something... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:09, 28 April 2015 (UTC)

== Overhand shuffle? ==

I tried an implementation of overhand shuffle, as described at [[wp:Shuffling#Overhand_shuffle]], and it looks like repeated cuts (the "overhand shuffle") do not actually accomplish any shuffling: An overhand cut looks to be equivalent to a rotate operation.

Looking at the current implementations, the C++ implementation of overhand shuffle does something different - it reverses the order of one part of the cut (repeatedly using deque's push_back primitive). Meanwhile, the Java implementation doesn't cut the deck back to itself, but to a new deck (with at least five of these "cut operations" per "cut iteration") - something that would be near impossible to do single handedly (which is what the wikipedia description seems to suggest should be done).

I'm going to implement it "by the book" - which is something of a joke as a shuffle, since it always retains the order of the deck - until we get a better specification. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:32, 18 May 2015 (UTC)
:The overhand shuffle is implemented in Java as the first few seconds of this video: https://youtu.be/x5tLNHuvf6s. Impossible singlehandedly probably but luckily most people have two hands. The "new" deck is just in the other hand in real life. The way it's explained on Wikipedia does seem a bit off. It should probably say the cuts are taken from the top of the deck and placed on top of the new deck. Neither the riffle nor overhand shuffle are good shuffles in computer science terms. They're just easy for meatbags to do in real life. Don't worry about it actually accomplishing a good shuffle. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 03:42, 1 June 2015 (UTC)

== Added definitions ==

I have added definitions of the riffle and overhand shuffles straight on the page so that we can standardize better here. Let me know if anything sounds unclear or incorrect. I'm not rushing to get this task ready for prime time so any corrections can still happen. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 14:25, 4 June 2015 (UTC)

== state of the deck before shuffling ==
No mention (unless inferred) was made of the state of the card deck   ''before''   shuffling.   I had assumed it would be sorted as if the card deck was taken from it's box (from the manufacture), and the cards are always in some sort of order by suits in a very specific order ('''A 2 3 4 5 6 7 8 9 10 J Q K''')   for each suit in alternating colors.   Normally, one shuffles the   ''heck''   out of new deck, and then some standard method is used to shuffle it.   Of course, if that was done, there wouldn't be a good way to measure how well (efficient) any one shuffle is after the   ''heck''   shuffle,   since it is already in a very shuffled state at that point.   I had assumed that shuffle effectiveness was a possible goal for this Rosetta Code task.   But could it be that the goal is to just program a method of shuffling to compare how to code an algorithm, with a method to force multiple shuffles?   So, we're back to a brand new spanking card deck, and a riffle shuffle doesn't do that much, the cards still appear to be in some kind of noticeable and obvious order, but certainly not random.   If a single cut would be performed before the shuffle, as is the case for most riffle shuffles, that would help quite a bit.   Observing real world riffle shuffles, usually there are:   [cut, riffle,  ... two more times ... at a minimum].   Some German card games require no more than three riffle shuffles, because three cards are dealt at a time, hoping for clusters of suited cards, thereby increasing the odds of a good bidding hand, but very unlike, say, a bridge game, where the cards are supposed to be very well shuffled.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:01, 29 August 2018 (UTC)

Furthermore, most shuffling (in the real world) starts with a barn yard of cards (from playing a hand or game of cards), and the cards are already in some sort of random order as the cards are picked up and pulled together.   But of course, that leaves us with the same problem as having a more-or-less random order in the first place, with no obvious way to measure the effectiveness of a particular sort.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:01, 29 August 2018 (UTC)

I should mention that a barn yard (or washing machine shuffle) is where all the cards are more-or-less dumped face down in the center of the card table and ... swished around with both hands in some sort of circular pattern, with one hand moving clockwise, the other hand, counter-clockwise, so that most of the cards are interleaved randomly, much like a washing machine might mix cards if it had two agitators   (without the water, of course).     It has an advantage that many hands can help, but most often, slowing the process a bit.   But many hands make light work.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:01, 29 August 2018 (UTC)

== riffle shuffle ==
The Rosetta Code task's preamble essentially mentions a perfect riffle shuffle, taking one card from a halved pile, effectively interleaving the 1st card from one pile with the 1st card from the other pile  (or the last card ...), and so on with the 2nd two cards, etc.    Almost all riffles (in real life) are interleaved in bunches, where a bunch of cards could be one, two, or three, or more, if the cards are well used, and there isn't a good edge for the shuffler's fingers.   Using a one, two, or three card
riffle shuffle would be very "real world".   Even my mechanical riffle shuffler has a randomizer built-in (caused by unevenness in the playing cards), with the advantage that it doesn't apply hand oils.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:01, 29 August 2018 (UTC)
:There are still 2 possibilities. After the perfect split you can perform a perfect riffle such that the previous top card is still on top or is now second. It is well known that if you keep the top card on top each time the deck returns to its original state after 8 iterations.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:50, 29 August 2018 (UTC)
