+++
title = "Talk:Dice game probabilities"
description = ""
date = 2015-06-20T04:50:18Z
aliases = []
[extra]
id = 18513
[taxonomies]
categories = []
tags = []
+++

== Test runs ==

Test runs with 10000 samples show

0.5751 player 1 wins  (agrees with shown probabilities)

0.3535 player 2 wins

0.0714 draws

and for the second part:

0.6405 player 1 wins  (differs considerably)

0.3147 player 2 wins

0.0448 draws    

Can somebody show the pseudo code ?

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 15:14, 16 January 2015 (UTC)

: <s>Second part is ten 5-sided dice vs seven 6-sided dice, no?</s> Never mind, everyone else (i.e. Bearophile and I) used the wrong numbers.  Your result is correct. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 05:38, 18 January 2015 (UTC)
:: :-) Still: Is your solution built analytically? Pseudocode? My attempt to translate it to REXX got stuck in the recursion. :-( --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:34, 18 January 2015 (UTC)

::: Oh well. REXX and ooRexx show now the algorithm. What have we learned? Testing is always a good idea! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:40, 19 January 2015 (UTC)

Change of D's results: I don't have D, so I can't test. How did the corrected (or the previous, incorrect) results come about? Just being curious: --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:40, 21 January 2015 (UTC)

== Results compared ==


```txt

Numeric Digits 130
Say 3781171969/5882450000
/*
0.642788628717626159168373721835
C         0.6427886287176260
D         0.6427886287176262
ooRexx    0.642788628717626159168373721835
REXX      0.642788628717626159168373721835
PL/I      0.642703175544738770
Python    0.642788628718
Python v2 0.6427886287176262
Python v3 0.6427886287176262
Racket    0.6427886287176261591683737218335897457691948082856632865557718297648088806534692177579069945345901793
          0.6427886287176261591683737218335897457691948082856632865557718297648088806534692177579069945345901792620421763040909824987887699853
ooRexx (*)0.64278862871762615916837372183358974576919480828566328655577182976480888065346921775790699453459017926204217630409098249878876998529524262849

Racket    (3781171969/5882450000)
*/

```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:20, 22 January 2015 (UTC)

== Rational Arithmetic ==

Inspired by Racket I boosted ooRexx and PL/I by using Rational Arithmetic. Actually I should have used the implementation that can be found here on RC! but I rolled my own before looking. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:57, 22 January 2015 (UTC)

== Possible extension? ==

Nice task.  While implementing it, I found myself briefly misunderstanding the "draw" scenario and an interesting alternative occurred to me.  What if a "draw" resulted in the game being re-run?  This changes the probabilities fairly significantly (not just <tt>W/(W+L)</tt>, but <tt>(W + D*k)/(W+L+D)</tt> for some <tt>k</tt>, which can be determined by running lots of trials or by calculus.  That probably puts it more in the realm of a Project Euler task than RosettaCode, but might make a fun extension.  --[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 04:50, 20 June 2015 (UTC)
