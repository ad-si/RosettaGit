+++
title = "Talk:Mastermind"
description = ""
date = 2019-07-13T13:39:37Z
aliases = []
[extra]
id = 21186
[taxonomies]
categories = []
tags = []
+++

== Duplicate of Bulls and cows task ==

Mastermind is just a modern name for the game [[Bulls_and_cows]], for which there is already a page.

(unsigned)

-----

: Except that the game of   ''Bull and Cows'':
:::*   limits the characters to nine decimal digits   (usually one ───► nine), 
:::*   the number of characters (decimal digits) in the guess is mandated to be four, 
:::*   has scoring, 
:::*   has a limit for the number of guesses, 
:::*   doesn't allow duplication (replication/repeats) of any decimal digit, 
:::*   and has less   (and different)   requirements for the display.

  -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:47, 31 January 2017 (UTC)

== Concerns about Julia (maybe other) implementation(s) ==

Looking over the Julia implementation (and I'm not familiar enough with all of the other languages on this page to say it's an exclusive problem, just able to read Julia to notice it), it seems that there is potential for more white circles (or O's for text version) to be shown than there are occurrences of some letter in the original code string. For example, with code ABCD, guess AAAA should output X--- and I believe this implementation will output (again, with graphics instead of lettering) XOOO.


[[User:Zacharymatson|Zacharymatson]] ([[User talk:Zacharymatson|talk]]) 03:01, 12 July 2019 (UTC)

See official rules for the board and peg game: http://www.boardgamecapital.com/game_rules/mastermind.pdf
<br /> The relevant line in the pdf is "No Key Peg to indicate a color that does not appear at all in the secret code." So a - is not ok there. Inspecting the code for the case when there were that many duplicate colors, I did find a problem with displaying XXXO when the program should show XXXX when the code is AAAA, though :)
--[[User:Wherrera|Wherrera]] ([[User talk:Wherrera|talk]]) 19:59, 12 July 2019 (UTC)

Well, it appears my implementation was the incorrect one, sorry about that! I read the rules on Wikipedia which seemed to suggest that there is a max of one peg per color/letter in the guess per color/letter in the original code. Will change the Python one to match the duplicates allowed rules. Curious which way the other implementations are going on these rules now.
[[User:Zacharymatson|Zacharymatson]] ([[User talk:Zacharymatson|talk]]) 13:39, 13 July 2019 (UTC)
