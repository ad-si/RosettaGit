+++
title = "Talk:24 game"
description = ""
date = 2019-01-06T19:26:40Z
aliases = []
[extra]
id = 4939
[taxonomies]
categories = []
tags = []
+++

==Purpose==
What's the theoretical or practical interest in this task? I'd be interested in a program that enumerated all the 4-tuples that have solutions or determined whether a given 4-tuple had a solution, but we've already got tasks for getting input from the user, parsing arithmetic expressions, and so on. —[[User:Underscore|Underscore]] 19:22, 31 October 2009 (UTC)

:Umm,
# Playing the game.
# We have a gazillion sorts for example, (some are very impractical); so we can stand some repetition.
# We don't have many games, (of any description). Some site grazers might be attracted just by the word 'game'.
# The input checking is novel.
# The task is more than the some of its parts! (We have tasks covering most statement types, and it would not make sense to use that as a reason for not doing any composite task).
# Prelude to possibly another task to solve the game. 
:(I don't like to write a task until I have a solution and so can better gauge its suitability, and so write a better task description). --[[User:Paddy3118|Paddy3118]] 20:57, 31 October 2009 (UTC)
::Without any official guidelines as to what's an appropriate Rosetta task and what isn't, I suppose it's all completely subjective, so I can't very well argue with you. Adding more games isn't such a bad idea; [[RCRPG]] is interesting from an implementation point of view but sorely lacking in the fun department. I wonder how hard it is to write a minimal Pong clone. —[[User:Underscore|Underscore]] 22:24, 31 October 2009 (UTC)
:::''Very'' easy, with modern hardware and programming languages.
:::FWIW, this task does strike me as being a somewhat useful one. I used to have a similar <s>time waster</s> game when I was a teen where you got 4 numbers and a goal, the end. (Except for the variable goal, identical to this task.) -- [[User:Eriksiers|Eriksiers]] 02:23, 1 November 2009 (UTC)
::: Since there really is no body of multiple individuals to give any sort of official policy, any concept of "official" falls to whatever I say by fiat.  Which sucks, because I'm nowhere near as knowledgeable on programming as many of RC's contributors are, regular or not.  All I ''really'' know is that I have a good mental picture of the kinds of roles I want to the site to fill.  Beyond that, I don't pretend to have expertise; Many of my early ideas and attempts at trying to come up with task descriptions were too specific, enforcing my own limited view of programming on the examples, so I've learned to stand back. I step in from time to time if I see discussion heading in a direction counter to where I'd like the site to go, but that's about it.
::: As for "redundant" tasks, it doesn't bother me. In fact, I can think of ways to take advantage of it, such as category tagging individual examples with techniques, features or principles they may illustrate, to offer another way to browse and search the site, and to be more illustrative of alternate approaches. --[[User:Short Circuit|Michael Mol]] 17:12, 1 November 2009 (UTC)

:::: Methinks you're turning into a Python-esque [[wp:BDFL|BDFL]] ;-)
:::: --[[User:Paddy3118|Paddy3118]] 18:49, 1 November 2009 (UTC) 
::::: It's also practical; I've got one paid 50hr/wk job coding, one 30+hr/wk job taking care of an elderly family member, and one 50hr/wk job sleeping. Not much time left to micromanage RC, as well as have any [other] hobbies. :) --[[User:Short Circuit|Michael Mol]] 23:13, 1 November 2009 (UTC)

A game player was not too difficult so I created it as a separate (but linked), task. --[[User:Paddy3118|Paddy3118]] 04:52, 1 November 2009 (UTC)

==Autohotkey and Untested==
Does untested mean that it has not been shown to run? If so, could we restrict ourselves to examples that have at least been run? (Or do you mean: "insufficiently tested but has at least been run")? --[[User:Paddy3118|Paddy3118]] 05:37, 11 November 2009 (UTC)

: I also agree (that the program should at least run (execute).   There have been entries that do not work at all for the computer programming language (category) entry that it was entered for.   It's a whole 'nother can of worms.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:56, 10 May 2016 (UTC)

== Floating point ==

Is recommending floating point wise?  Precision and rounding errors would seem to defeat the purpose of preserving remainders. --[[User:Short Circuit|Michael Mol]] 03:58, 8 December 2009 (UTC)
: It isn't rigorous, (I'm an engineer, not a theoretical mathematician captain), but with the restricted range of single digit numbers, the given operators, and the precision of most FP implementations on a PC; I don't think it will matter. --[[User:Paddy3118|Paddy3118]] 07:12, 8 December 2009 (UTC)
: Certainly rationals are a better choice than floating point for this type of task, but I think you should be able to use floating point safely so long as you say something like <code>if abs(ans - 24) < 1e-6</code> instead of <code>if ans == 24</code>. It's considered good practice, in general, to never test a floating-point variable for exact equality. That said, I'm at a loss for an input that the Python program would wrongly reject, so maybe floating point is better than I think it is. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 12:49, 8 December 2009 (UTC)
:: If someone can exhibit a solution that a floating-point version won't find, that's when action becomes necessary. However, when I try out likely problems then I find I still cannot trigger any issues. For example:
::
```tcl
$ tclsh8.5
% expr {(((1.0/3.0)*9.0)*8.0) - 24.0}
0.0
```
Despite a definitely non-representable intermediate (I know the implementation) the answer is exact. I just can't trigger the problem for anything that might be a solution. (Non-solutions… who cares about them?) Rationals might be theoretically better, but IEEE double seems good enough here. –[[User:Dkf|Donal Fellows]] 13:32, 8 December 2009 (UTC)
:::  You mentioned in [[Talk:24_game_Player#Should_we_enumerate_all_solutions.3F]] that it was practicable to identify all 7,860 solutions. That sounds like a pretty small test set. (Or am I misunderstanding your description of the enumeration?) --[[User:Short Circuit|Michael Mol]] 18:49, 8 December 2009 (UTC)

::: Hi Donal, you might try [[Talk:24_game_Player#Use_3_3_8_8_in_example_runs.3F|3 3 8 8]] in the TCL example. Floating point won't work for this in Python. --[[User:Paddy3118|Paddy3118]] 14:18, 15 February 2011 (UTC)

::: 3 3 8 8 has different values depending on the Tcl versions it seems <lang>:> /opt/tcltk_8.0.3/bin/tclsh8.0
% expr 8.0 / ( 3.0 - 8.0 / 3.0 )
24.0
%
:> /opt/tcltk_8.3.4/bin/tclsh
% expr 8.0 / ( 3.0 - 8.0 / 3.0 )
24.0
%
:> /opt/tcltk_8.4.13/bin/tclsh
% expr 8.0 / ( 3.0 - 8.0 / 3.0 )
24.0
%
:> /opt/TWWfsw/bin/tclsh8.5
% expr 8.0 / ( 3.0 - 8.0 / 3.0 )
23.99999999999999
%
```
 --[[User:Paddy3118|Paddy3118]] 14:34, 15 February 2011 (UTC)

:One potential problem with floating point, when you get to more than 4 digits, is that in some languages (not Python), division by 0.0 evaluates to infinity (which can be divided by again to get 0), instead of raising an error, so for example, this in Ruby: <code>3.0 * (8.0 + 4.0 / (2.0 / (5.0 - 5.0)))</code> results in 24.0, which you might not want to allow. But I agree that this is not an issue with only 4 digits. --[[User:Spoon!|Spoon!]] 14:16, 8 December 2009 (UTC)
:: Precisely. That's why it matters that it is a “24-game player”. A solution with computation using rationals would be interesting though. –[[User:Dkf|Donal Fellows]] 14:57, 8 December 2009 (UTC)

== Does the Perl code work? ==

I have tryed to run the Perl code, and it seems that it doesn't recognise when the correct answer is given. --[[User:Blue Prawn|Blue Prawn]] 00:42, 26 January 2010 (UTC)

==Scala only presenting numbers with solutions==
I note that the impressive Scala example seems to only present a set of numbers guaranteed to have a solution. i can't help but think that from playing the game, some of the fun is ''lost'', as we quite enjoyed trying the toughies - not knowing if they actually had a solution or not as most numbers do. I guess they don't have to add much for a solution to [[24 game Player]]. --[[User:Paddy3118|Paddy3118]] 06:43, 28 April 2010 (UTC)

: I couldn't quite understand fully the meaning of the penultimate statement as it appears it may be missing an Oxford comma.    Most numbers <u>don't</u> have a solution.   (See the next talk section).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:04, 15 March 2017 (UTC)


==REXX only presenting digits with at least one solution==
I have programmed the REXX example to also only present a set of digits that have a solution   (as does '''Scala''').  

From the task preamble:

 ''The goal is for the player to enter an expression that (numerically) evaluates to '''24'''. ''

Nowhere does it imply that an impossible set of digits is to be determined and, somehow, the user is
supposed to   ''not''   enter a solution;   as it is, no computer programming entry checks for a   ''not possible''   answer for a solution.

When presented with the digits     '''1 7 6 8''',     what should a carbon-based life form (user) respond with for an answer (which is a non-solution)?

 No such animal?
 N/A
 there ain't one
 not possible
 nope.
 I give up.
 I don't know
 Please tell me the answer!!!
 bupkis
Some of the solvable digits (numbers) are hard enough to find a solution, and presenting an unsolvable problem doesn't seem to be fair or realistic, since this is a game, not a riddle, and the game is to find a solution, not   "none-solutions". 

==unsolvable solutions for the '''24''' game==
Of the   6,561   legal/valid numbers (with four digits) that can be presented   (numbers without any zeroes),   <strike>2,501</strike>   1,263   are   unsolvable   (that represents about   <strike>'''38%'''</strike>   '''19%'''   unsolvable).   This presumes that the REXX program correctly solved all possible (allowable) numbers. 

<strike>

(I have a complete list of the 1,501 unsolvable numbers for the '''24''' game.)   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:04, 15 March 2017 (UTC)
</strike>

I found an error in the way the output file was massaged and created a list from the post-edited output file. 

I have an updated complete list of the   <strike>1,263</strike>   757   unsolvable numbers for the '''24''' game.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:59, 6 January 2019 (UTC)

Here are the formatted (indexed) unsolvable numbers (the order of the digits are preserved) for the '''24''' game:
<pre style="font-size:85%">
  1► 1111 1112 1113 1114 1115 1116 1117 1119 1121 1122 1123 1124 1125 1131 1132 1133 1141 1142 1151 1152 1159 1161 1167 1171 1176
 26► 1177 1178 1179 1187 1189 1191 1195 1197 1198 1199 1211 1212 1213 1214 1215 1221 1222 1223 1231 1232 1241 1251 1299 1311 1312
 51► 1313 1321 1322 1331 1355 1411 1412 1421 1499 1511 1512 1519 1521 1535 1553 1557 1558 1575 1577 1585 1591 1611 1617 1667 1671
 76► 1676 1677 1678 1687 1711 1716 1717 1718 1719 1755 1757 1761 1766 1767 1768 1771 1775 1776 1777 1778 1781 1786 1787 1791 1817
101► 1819 1855 1867 1871 1876 1877 1891 1899 1911 1915 1917 1918 1919 1929 1949 1951 1971 1981 1989 1991 1992 1994 1998 1999 2111
126► 2112 2113 2114 2115 2121 2122 2123 2131 2132 2141 2151 2199 2211 2212 2213 2221 2222 2226 2231 2262 2279 2297 2299 2311 2312
151► 2321 2334 2343 2411 2433 2511 2555 2556 2565 2599 2622 2655 2677 2729 2767 2776 2777 2779 2792 2797 2799 2919 2927 2929 2959
176► 2972 2977 2979 2991 2992 2995 2997 2999 3111 3112 3113 3121 3122 3131 3155 3211 3212 3221 3234 3243 3311 3324 3342 3358 3385
201► 3423 3432 3467 3476 3488 3515 3538 3551 3555 3577 3583 3647 3674 3746 3757 3764 3775 3835 3848 3853 3884 4111 4112 4121 4199
226► 4211 4233 4323 4332 4367 4376 4388 4459 4466 4467 4476 4495 4499 4549 4594 4637 4646 4647 4664 4673 4674 4736 4746 4763 4764
251► 4779 4797 4838 4883 4919 4945 4949 4954 4977 4991 4994 4999 5111 5112 5119 5121 5135 5153 5157 5158 5175 5177 5185 5191 5211
276► 5255 5256 5265 5299 5315 5338 5351 5355 5377 5383 5449 5494 5513 5517 5518 5525 5526 5531 5535 5552 5553 5557 5558 5562 5569
301► 5571 5575 5579 5581 5585 5596 5597 5625 5652 5659 5695 5715 5717 5737 5751 5755 5759 5771 5773 5777 5778 5787 5795 5799 5815
326► 5833 5851 5855 5877 5899 5911 5929 5944 5956 5957 5965 5975 5979 5989 5992 5997 5998 5999 6111 6117 6167 6171 6176 6177 6178
351► 6187 6222 6255 6277 6347 6374 6437 6446 6447 6464 6473 6474 6525 6552 6559 6595 6617 6644 6667 6671 6676 6677 6678 6687 6699
376► 6711 6716 6717 6718 6727 6734 6743 6744 6761 6766 6767 6768 6771 6772 6776 6777 6778 6779 6781 6786 6787 6788 6797 6817 6867
401► 6871 6876 6877 6878 6887 6955 6969 6977 6996 6999 7111 7116 7117 7118 7119 7155 7157 7161 7166 7167 7168 7171 7175 7176 7177
426► 7178 7181 7186 7187 7191 7229 7267 7276 7277 7279 7292 7297 7299 7346 7357 7364 7375 7436 7446 7463 7464 7479 7497 7515 7517
451► 7537 7551 7555 7559 7571 7573 7577 7578 7587 7595 7599 7611 7616 7617 7618 7627 7634 7643 7644 7661 7666 7667 7668 7671 7672
476► 7676 7677 7678 7679 7681 7686 7687 7688 7697 7711 7715 7716 7717 7718 7726 7727 7729 7735 7749 7751 7753 7757 7758 7761 7762
501► 7766 7767 7768 7769 7771 7772 7775 7776 7777 7778 7779 7781 7785 7786 7787 7788 7789 7792 7794 7796 7797 7798 7799 7811 7816
526► 7817 7857 7861 7866 7867 7868 7871 7875 7876 7877 7878 7879 7886 7887 7888 7897 7899 7911 7922 7927 7929 7947 7955 7959 7967
551► 7972 7974 7976 7977 7978 7979 7987 7989 7992 7995 7997 7998 7999 8117 8119 8155 8167 8171 8176 8177 8191 8199 8335 8348 8353
576► 8384 8438 8483 8515 8533 8551 8555 8577 8599 8617 8667 8671 8676 8677 8678 8687 8711 8716 8717 8757 8761 8766 8767 8768 8771
601► 8775 8776 8777 8778 8779 8786 8787 8788 8797 8799 8834 8843 8867 8876 8877 8878 8887 8888 8889 8898 8899 8911 8919 8959 8977
626► 8979 8988 8989 8991 8995 8997 8998 8999 9111 9115 9117 9118 9119 9129 9149 9151 9171 9181 9189 9191 9192 9194 9198 9199 9219
651► 9227 9229 9259 9272 9277 9279 9291 9292 9295 9297 9299 9419 9445 9449 9454 9477 9491 9494 9499 9511 9529 9544 9556 9557 9565
676► 9575 9579 9589 9592 9597 9598 9599 9655 9669 9677 9696 9699 9711 9722 9727 9729 9747 9755 9759 9767 9772 9774 9776 9777 9778
701► 9779 9787 9789 9792 9795 9797 9798 9799 9811 9819 9859 9877 9879 9888 9889 9891 9895 9897 9898 9899 9911 9912 9914 9918 9919
726► 9921 9922 9925 9927 9929 9941 9944 9949 9952 9957 9958 9959 9966 9969 9972 9975 9977 9978 9979 9981 9985 9987 9988 9989 9991
751► 9992 9994 9995 9996 9997 9998 9999

```

I'd be interested if anyone would verify that all the numbers above are unsolvable for the '''24''' game.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:31, 3 January 2019 (UTC)

:'''Edited:''' I accidentally listed the groups with only one solution as unsolvable. Fixed now.

:Um. You may want to re-read the task rules, specifically the one stating "The order of the digits when given does not have to be preserved."


:: I don't need to re-read the task rules, I programmed the REXX program which does NOT preserve the order of digits when presenting solutions, but it does honor the original number being processed and shows (all) the solutions for that number, for any order of its digits.   The task's requirement said that it   ''does not not have to be preserved'',   it didn't say   ''should not be preserved''.   I choose to preserve the order of digits for the REXX entry.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:27, 5 January 2019 (UTC) 

:::Who said anything about REXX? Or even the actual task code? My point was: if say, 1678 is unsolvable, then  so is 1687, 1768, 1786, 1867, 1876, 6178, 6187, 6718, 6781, 6817, 6871, 7168, 7186, 7618, 7681, 7816, 7861, 8167, 8176, 8617, 8671, 8716 & 8761, and there isn't any point in listing all of them '''unless the solver is only checking numbers with preserved order'''. Like I said, there is only a total of 495 unique combinations of 4 non-zero digits. Of those, 91 are unsolvable for 24. Even if you count all possible permutations of each combination, there are only 757 unsolvable "numbers". Not sure where you are getting the 1263 from. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:11, 5 January 2019 (UTC)
:::: I said about REXX.   It is the REXX entry that I was referring to.   As for the 1,263 number, that was in error and the number that I got for unsolvable numbers is 757   (see the above updated list).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:59, 6 January 2019 (UTC)

:::Some further exposition about digit reordering. OK, you say the REXX version preserves order because the instructions permit reordering but don't mandate it. But those instructions '''ARE FOR THE PLAYER, NOT THE PROGRAMMER'''. If the solver can't or won't handle digit reordering, you'll run into situation like this: you offer the player 1399, and she says: "Aha! I am permitted to reorder digits so: (9 - 1) / 3) * 9 = 24". The REXX entry will say '''NOPE! INCORRECT, 1399 is UNSOLVABLE.''' (There ''are'' no solutions for those digits in that order.) The player is going to say "Are you f'n kidding me?" and go investigate some other language. If it is permitted, you must make allowances for it. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 15:14, 5 January 2019 (UTC)

:::: Er, no.   You are mistaken about what you say the REXX program will say.   For your clarification, here is a screenshot of an actual '''24''' game   (with   1139   being the digits to use),   and note that   '''24'''   is the name of the REXX program,   and that the   '''+1139'''   argument forces the REXX program to use   '''1139'''   (instead of some random digits)   for the required digits to be used,   and, of course, the digits may be   ''in any order''.   My (auto-generated to fit the screen) DOS "prompt" is two (wrapped) lines:

```txt

─────────────────────────────────────────────────────────────────────────────01/05/2019 18:33:44
c:\►24  +1139             ◄■■■■■■■■■■■ what the non-cussing user entered.                                       

Using the digits 1399, enter an expression that equals 24   (? or QUIT):
(9-1)/3) * 9              ◄■■■■■■■■■■■ what the non-cussing user entered.  
mismatched ()

Using the digits 1399, enter an expression that equals 24   (? or QUIT):
(9-1)/3 * 9               ◄■■■■■■■■■■■ what the non-cussing user entered.  

                            ┌─────────────────────┐
                            │                     │
                            │  congratulations !  │
                            │                     │
                            └─────────────────────┘


─────────────────────────────────────────────────────────────────────────────01/05/2019 18:34:16
c:\►

```

:::: The REXX didn't get too excited about the mismatched parenthesis and kept it's cool.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:59, 6 January 2019 (UTC)


:That being the case, there are only 495 unique combinations of 4 non-zero digits. This is ALL of them.

    Unsolvable:
    1111 1112 1113 1114 1115 1116 1117 1119 1122 1123 1124 1125 1133 1159 1167 1177
    1178 1179 1189 1199 1222 1223 1299 1355 1499 1557 1558 1577 1667 1677 1678 1777
    1778 1899 1999 2222 2226 2279 2299 2334 2555 2556 2599 2677 2777 2779 2799 2999
    3358 3467 3488 3555 3577 4459 4466 4467 4499 4779 4999 5557 5558 5569 5579 5777
    5778 5799 5899 5999 6667 6677 6678 6699 6777 6778 6779 6788 6999 7777 7778 7779
    7788 7789 7799 7888 7899 7999 8888 8889 8899 8999 9999

    Solvable:
    1118 1126 1127 1128 1129 1134 1135 1136 1137 1138 1139 1144 1145 1146 1147 1148
    1149 1155 1156 1157 1158 1166 1168 1169 1188 1224 1225 1226 1227 1228 1229 1233
    1234 1235 1236 1237 1238 1239 1244 1245 1246 1247 1248 1249 1255 1256 1257 1258
    1259 1266 1267 1268 1269 1277 1278 1279 1288 1289 1333 1334 1335 1336 1337 1338
    1339 1344 1345 1346 1347 1348 1349 1356 1357 1358 1359 1366 1367 1368 1369 1377
    1378 1379 1388 1389 1399 1444 1445 1446 1447 1448 1449 1455 1456 1457 1458 1459
    1466 1467 1468 1469 1477 1478 1479 1488 1489 1555 1556 1559 1566 1567 1568 1569
    1578 1579 1588 1589 1599 1666 1668 1669 1679 1688 1689 1699 1779 1788 1789 1799
    1888 1889 2223 2224 2225 2227 2228 2229 2233 2234 2235 2236 2237 2238 2239 2244
    2245 2246 2247 2248 2249 2255 2256 2257 2258 2259 2266 2267 2268 2269 2277 2278
    2288 2289 2333 2335 2336 2337 2338 2339 2344 2345 2346 2347 2348 2349 2355 2356
    2357 2358 2359 2366 2367 2368 2369 2377 2378 2379 2388 2389 2399 2444 2445 2446
    2447 2448 2449 2455 2456 2457 2458 2459 2466 2467 2468 2469 2477 2478 2479 2488
    2489 2499 2557 2558 2559 2566 2567 2568 2569 2577 2578 2579 2588 2589 2666 2667
    2668 2669 2678 2679 2688 2689 2699 2778 2788 2789 2888 2889 2899 3333 3334 3335
    3336 3337 3338 3339 3344 3345 3346 3347 3348 3349 3355 3356 3357 3359 3366 3367
    3368 3369 3377 3378 3379 3388 3389 3399 3444 3445 3446 3447 3448 3449 3455 3456
    3457 3458 3459 3466 3468 3469 3477 3478 3479 3489 3499 3556 3557 3558 3559 3566
    3567 3568 3569 3578 3579 3588 3589 3599 3666 3667 3668 3669 3677 3678 3679 3688
    3689 3699 3777 3778 3779 3788 3789 3799 3888 3889 3899 3999 4444 4445 4446 4447
    4448 4449 4455 4456 4457 4458 4468 4469 4477 4478 4479 4488 4489 4555 4556 4557
    4558 4559 4566 4567 4568 4569 4577 4578 4579 4588 4589 4599 4666 4667 4668 4669
    4677 4678 4679 4688 4689 4699 4777 4778 4788 4789 4799 4888 4889 4899 5555 5556
    5559 5566 5567 5568 5577 5578 5588 5589 5599 5666 5667 5668 5669 5677 5678 5679
    5688 5689 5699 5779 5788 5789 5888 5889 6666 6668 6669 6679 6688 6689 6789 6799
    6888 6889 6899 7889

    Solvable but only one solution:
    1277 1346 1668 3355 3388 5555 5588 5599

:But even putting that aside, with a quick peek at your "unsolvable" list, I pick out 1164 (1*1*6*4), 1183 (1*1*8*3), 2232 (2*2*3*2) and many other that don't require digit reordering. I think you need to revisit your filtering algorithm. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 02:22, 4 January 2019 (UTC)

:: It wasn't the filtering that caused the error, but my post-editing process.   The REXX program did, in fact, find solutions for   1164   (and it's variants).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:27, 5 January 2019 (UTC)

Hi guys, I checked <strike>Gerards</strike>Thundergnats results with one of the Python solvers and seems fine to me :-)

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:33, 5 January 2019 (UTC)

:Really!? --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:11, 5 January 2019 (UTC)

I got:

```txt
 Thundergnat Stated UNsolvables: 0 solved, 91 unsolved
 Thundergnat Stated Solvables: 404 solved, 0 unsolved
 My  determination of all order-independent 4 digit numbers: 404 solved, 91 unsolved
```

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:39, 5 January 2019 (UTC)

: Oh. Yeah, except that isn't the list Gerard posted, that was mine. Gerards list (which he has since deleted) had 2501 "unsolvable" numbers in it where '''at least''' 1744 of them were, in fact, solvable.

:: My apologies; attribution changed. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:25, 6 January 2019 (UTC)
