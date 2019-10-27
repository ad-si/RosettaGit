+++
title = "Talk:Poker hand analyser"
description = ""
date = 2014-06-10T21:41:38Z
aliases = []
[extra]
id = 16860
[taxonomies]
categories = []
tags = []
+++

==Suggestions==
Hi, just some suggestions on the task. (Nice one by the way):

# You might ask people to show their output on this page.
# You might want to flesh out the examples so that there is an example of each type of hand

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:28, 10 December 2013 (UTC)

:I made those changes so Perl 6 will need those (slight), updates. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:23, 10 December 2013 (UTC)

Another example to add would be a bicycle (to compliment the royal flush) to show that an ace can be used as ''two different values''. 

A ''bicycle'' (or ''bike'') is a straight consisting of:   '''ace''', '''deuce''', '''trey''', '''4''', and a '''5'''   (the lowest straight possible). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:40, 12 December 2013 (UTC)


-----

I would like the allowing of expanding '''invalid''' responses with (possibly, say)
:::: invalid --- ''the reason for invalid flagging here.''
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:30, 12 December 2013 (UTC)

-----
 
In the task's preamble, it says that ''Each input card has two characters indicating face and suit'', but then goes on to show a '''10''' for a pip (which is two characters).

I suggest that program accept both '''10''' and a '''T'''   (and a '''t''').

Also, all pips (on every card deck that I remember) use capital letters for (lettered) pips:   '''A''' (ace), '''J''' (jack), '''Q''' (queen), and '''K''' (for king).   Of course, their lowercase versions should also be accepted.   Jokers may or may not be capitalized, but most decks seem to use capital letters. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:47, 20 December 2013 (UTC)

-----

Amend the question to ''"Which of these two poker hands wins?"''. The present analysis, which classifies hands into a coarse ranking, is inadequate for playing the game.

==extra credit suggestion==

It would be real interesting to program (for extra credit) adding two jokers to the deck,
adding a few complications:
:::*   duplicates would be allowed   (for jokers only)
:::*   five-of-a-kind would be the highest hand
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:58, 11 December 2013 (UTC)

Thanks Gerard, have added Jokers

--[[User:Dwarring|Dwarring]] ([[User talk:Dwarring|talk]]) 01:20, 13 December 2013 (UTC)

:But their doesn't seem to be an explanation of the action of jokers? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:32, 13 December 2013 (UTC)

-----

It's a good thing I didn't mention ''bugs'' or ''wild cards'' --- or playing with more than five cards per hand (as in some stud poker games).

''Bugs'' are jokers than can represent:

:::*   any generic (non-specific) ace
:::*   any card in a flush or straight (that isn't already in that hand)

''Bugs'' (as in a type of ''jokers'') are easy to program for when analyzing poker hands.

I once played a stud poker game with (all) red cards wild.

I'm still trying to figure out the programming required to support analyzing poker hands where there're more wild cards than non-wild cards (in addition to the jokers, of course).   Figuring out if there is a straight with three wild cards is daunting, especially when playing something like 7-card stud. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:28, 13 December 2013 (UTC)

---

Gerard, I've taken you extra credit example as sample input. Thanks! Also added a few two joker test cases (sorry to move the goal-posts!)

Also, I notice that a "t♣" has crept into you v2 and v3 tests, as in: q♣  t♣  7♣  6♣  4♣

: Yes, I wanted to show that the REXX program example could handle a '''t''' as well as a '''10'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:27, 20 December 2013 (UTC)

ok, that makes sense --[[User:Dwarring|Dwarring]] ([[User talk:Dwarring|talk]]) 21:23, 20 December 2013 (UTC)

-----

@Tipon - Nice work with the Prolog example.

As a point of Trivia the 'Poker Hand Analyser' task was originally given to us as an assignment, by the lecturer in a 3rd year computer science Software Engineering class. That was a good few years ago.

--[[User:Dwarring|Dwarring]] ([[User talk:Dwarring|talk]]) 23:23, 9 June 2014 (UTC)
