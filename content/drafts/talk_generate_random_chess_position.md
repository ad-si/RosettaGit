+++
title = "Talk:Generate random chess position"
description = ""
date = 2019-02-16T20:11:30Z
aliases = []
[extra]
id = 19831
[taxonomies]
categories = []
tags = []
+++

Can there be a white pawn in the first rank (and black in the last)?
I was never taught that pawns have a reverse gear. --[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 08:56, 14 December 2015 (UTC)
: Task description says ''The picking of pieces does not have to comply to a regular chess set'' and then goes on to give a variety of examples of "illegal" boards. It is also very specific about the constraints it places on pawns. Rather than excluding pawns from the first and last row of the board, it is careful to exclude each color from only one of those rows. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:36, 14 December 2015 (UTC)

:To clarify: yes, putting a white pawn on the first rank or a black pawn on the eighth rank is allowed, even if it's impossible in a normal chess game.  The reason is that on [http://en.lichess.org lichess] sometimes players challenge each other on custom starting positions and it's not rare that they chose to place pawns there.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 11:42, 14 December 2015 (UTC)

:: Having a pawn on the eigth rank is <u>not</u> impossible.   Normally, when a pawn reaches the eighth rank, it can be promoted.   However, promotion is not   ''required'',   and if left unpromoted, it remains a (dead) pawn.   I don't know of any normal (timed) game that a pawn has been left unprompted, but this has happened when playing speed (blitz) chess.   Once you "hit the clock", that pawn is forever a pawn.    -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:25, 18 July 2018 (UTC)

::: I think the point was: white pawns <u>start</u> on the 2nd rank. There is no legal move in standard chess that can result in them moving "backwards" to the 1st. Conversely, black pawn <u>start</u> on the 7th rank and have no legal move that can result with one on the 8th. However both positions are legal for this task despite them being impossible in actual chess. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 10:01, 27 September 2018 (UTC)

::
:::Gerard, actually the promotion is required by the [https://www.fide.com/fide/handbook.html?id=208&view=article Laws of Chess] given by the FIDE: 3.7.5.1 ''When a player, having the move, plays a pawn to the rank furthest from its starting position, he must exchange that pawn as part of the same move for a new queen, rook, bishop or knight of the same colour on the intended square of arrival. This is called the square of ‘promotion’''. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 18:24, 16 February 2019 (UTC)

:::: The task currently stipulates producing a "sample of all possible positions"; it's arguable that white pawns on the first or black pawns on the eighth ranks do not represent a "possible position" according to the rules of chess and should therefore be blocked. (Further, if it's White to move as the task says, legally Black can't be in check - not stipulated, but again a requirement for a "possible position".) [[User:Kinitawowi|Kinitawowi]] ([[User talk:Kinitawowi|talk]]) 00:52, 16 February 2019 (UTC)
:::::All possible positions given the constraints in the tasks, which do not inclue this. However, I agree that this should be added to the constraints, as there is already something about the promotion row. There is no point in applying the rules in one case and not in the other. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 18:29, 16 February 2019 (UTC)

::::::I'd guess it'd get left out due to determining a "possible" position quickly becoming a horrible mess of retrograde analysis way beyond the scope of a challenge of code-fu... which only makes the extant constraints more arbitrary (neighbouring kings are specifically blocked, for example). [[User:Kinitawowi|Kinitawowi]] ([[User talk:Kinitawowi|talk]]) 20:11, 16 February 2019 (UTC)
