+++
title = "Talk:Flipping bits game"
description = ""
date = 2019-06-12T05:14:48Z
aliases = []
[extra]
id = 15112
[taxonomies]
categories = []
tags = []
+++

==[http://www.reddit.com/r/programming/comments/1rpk3t/flipping_bits_game_is_it_original/ Reddit] joins in!==
I asked Reddit if the game was original and they managed to find some similar games. User Strilanc went further and seems to have found an easy algorithm for solving it too. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:43, 30 November 2013 (UTC)

==References?==
I doubt that this game is unique. If anyone has any links to the game elsewhere then please tell. Thanks. 

Oh, I am also looking for strategies to finish the game. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:19, 10 July 2013 (UTC)

: I'm trying to work out if it is even ''possible'' to finish the game starting from an arbitrary position. I suspect it isn't. Consider a <math>2\times 2</math> grid with an initial position with one cell set, and where the target position is where all cells are unset; all sequences of game operations either keep one cell set (equivalent by rotation) or transition the state to one with three cells set (equivalent to the one-cell-set case by a trivial negation and rotation). Now, it might be that I'm missing something and that larger grids are in fact always soluble, but I suspect not right now. (I can't work it out for the <nowiki>3\times 3</nowiki> case but my investigations do run into the sand in a telling way, and larger grids feel like more of the same.)
: If my intuition is right, surely we should only be posing ''solvable'' games? And if that's true, what is the safety condition? (If it's some sort of parity, I'm not seeing it, but then I'm perhaps just being obtuse.) Or should we just do the creation of the starting position by construction (i.e., by generating a random sequence of moves from the target position — target/start are swappable of course as all moves are self-inverses)? –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:28, 11 July 2013 (UTC)

::Hi Donal. I thought that a general target position might ''not'' be solvable from a given start position so although I generate the target randomly, I generate the start position by random legal flips which are reversible. Maybe I should add this strategy to the task description? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:56, 11 July 2013 (UTC)
::: Added! P.S. have you seen the game before? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:04, 11 July 2013 (UTC)
:::: Don't think so. I feel I've seen something like it (enough to wonder about parity checking as a method of verification) but not the same. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 14:17, 16 July 2013 (UTC)

:: You're right -- there are actually more start-end pairs that cannot be completed than start-end pairs that can. I'll give a proof that even-sized grids cannot change parity via the legal moves, and from there, I'll show what else that means.

:: Let's define the parity of a grid to be whether the number of ones in the grid is odd or even. With this definition, I can show that '''for grids of size MxN, where M and N are both even, an ending position cannot be reached if the starting position has a different parity.'''

::* Every row or column either contains an even number of ones, or an odd number of ones. Since the grid is even-sized, if the row or column contains an even number of ones, there must be an even number of zeros as well. Similar can be said if there is an odd number of ones.
::* A move consists of replacing all ones with zeros and all zeros with ones in a single row or column.
::* Make an move on an arbitrary row (or column). If there was an even number of ones in the row, then after the move, there is still an even number of ones in the row, and the board's parity is retained. If there was an odd number of ones in the row, then after the move, there is still an odd number of ones in the row, and the board's parity is retained.
::* Since no move can change the parity of the board, then the ending position cannot be reached if its parity is different than the starting position's.

:: With this argument, you can show that '''from a starting position on an NxN grid (N > 1), you cannot reach all ending positions of like parity''', because for some you would have to change the parity of an even-sized subgrid in order to do it. As an example, try the following transformation on a 4x4 grid:
::<code>1100  =>  0000
::1100  =>  0110
::0000  =>  0110
::0000  =>  0000</code>
:: You can't possibly do it with the legal moves set up in the description. This proof highly restricts the start-end pairs that are actually solvable. -- [[User:AnonymousJohn|AnonymousJohn]] ([[User talk:AnonymousJohn|talk]]) 20:04, 28 November 2013 (UTC)

:::Thanks AnonymousJohn for the effort you have put into that answer. More proof that generation of the start position from legal random flips from the target position was necessary. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:14, 29 November 2013 (UTC)

::::Glad to help. I wish I could definitively prove a relationship between the starting and ending position that guarantees a solution. Alas, I don't think my proof skills are quite that good. I suspect that if the starting and ending positions share the same parity for each even-sized subgrid, there is a solution, but I'm not sure I could prove that. [[User:AnonymousJohn|AnonymousJohn]] ([[User talk:AnonymousJohn|talk]]) 01:50, 4 December 2013 (UTC)

:This game is similar to the [https://en.wikipedia.org/wiki/Lights_Out_(game) "All-out"] game... --[[User:Simple9371|Simple9371]] ([[User talk:Simple9371|talk]]) 13:48, 16 June 2015 (UTC)

==Generating a Solvable Game==
I am quite sure that this has been shown already by someone, but I will still share a 'method' of generating a solvable flipping bits game, with some proofs. Here it is: [https://pastebin.com/WZfMeeGw PasteBin]

If there is any error there, just reply.

: You should sign (and date) your comments <nowiki>--~~~~</nowiki> will autogenerate a timestamped signature for you. Also, generating all solvable games would be a different task. (If you feel up to it, you could create a task page for that task...). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:53, 13 May 2017 (UTC)

:: Sorry, I forgot the signature. Just ignore the proof, because it will just prove that the 'fastest' way to generate a solvable flipping bits game is to do what the page recommends you to do (''One possible way to do this is to generate the start position by legal flips from a random target position.''). I mean, no parity tests, etc. --[[User:Simple9371|Simple9371]] ([[User talk:Simple9371|talk]]) 07:45, 14 May 2017 (UTC)

==Why the Random Goal State?==
Wouldn't it be the exact same game (but a bit easier to display and play) if we said the goal state was always all zeros?  Obviously we'd still need to generate a valid starting state.  But then the player wouldn't have to look back and forth between the two states to see which bits are wrong; she would be just trying to eliminate all the 1's. --[[User:JoeStrout|JoeStrout]] ([[User talk:JoeStrout|talk]]) 05:14, 12 June 2019 (UTC)
