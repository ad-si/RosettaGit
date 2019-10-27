+++
title = "Talk:Integer roots"
description = ""
date = 2016-05-20T00:04:34Z
aliases = []
[extra]
id = 20907
[taxonomies]
categories = []
tags = []
+++

==as a test...==
Does     <big> ''As a test, you <u>can</u> calculate ···'' </big>     to be taken literally, or should it be inferred that it   '''is'''   to be the test?   The underscoring was added by me.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:47, 11 May 2016 (UTC)

Do you mean the '''test''' is the algorithm to be used, or just a suggestion that that's '''the''' method   (or '''a''' method)   to be used?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:47, 11 May 2016 (UTC)

:Perhaps this is misleading. It certainly should not be the '''only''' test.

How should entries handle the case of a   zero   root?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:47, 11 May 2016 (UTC) 

:Uncertain about that - ideas?

:: If you change the task's requirements that the root be a positive integer, that kills two birds with one stone.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:37, 11 May 2016 (UTC) 


In the task's requirements, it's mentioned that   '''N'''   is an integer.   If   '''N'''   is negative, it means that the resultant root is the reciprocal of the   abs('''N''')<sup>th</sup>   root.   Is this the intent of the task?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:56, 11 May 2016 (UTC)

:I would change that to "non-negative integer".

In the interim, for the REXX computer language entry, I used a general-purpose integer root (of any number) which bypasses the problem of multiplying a number by a gihugeic number to get around the problems with handling a decimal fraction.   The numbers being passed to the   '''iRoot'''   function could mimic the suggested method if required. 

The REXX entry also handles negative roots, which are allowed by the current task requirements.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:47, 11 May 2016 (UTC)

==principal N<sup>th</sup> root==
I assume that this task wants the   ''principal''   '''N'''<sup>th</sup>   root?   It usually goes without saying, but ya never know.   -1.41421···   is one of the square roots.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:08, 11 May 2016 (UTC)

:That is in fact what I meant. How do I sign my comments?
::[[mw:Help:Signatures|Help page on signatures]] --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 04:08, 11 May 2016 (UTC)
:::Thanks! [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 04:18, 11 May 2016 (UTC)

==could the task description be made nicer?==
I wrote it but I can just imagine people's eyes glazing over. Maybe there is a simpler or more accessible way to convey the same information? [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 05:19, 11 May 2016 (UTC)

: A usual way to deal with dense description is to show an example. Another is to describe it from a different perspective, or a second time, consciously choosing different words (some tasks here go overboard and specify specific algorithms - sometimes to the detriment of the task, but that's another issue). If that gets too involved, another variant includes a reference to some off-site material (often we use wikipedia). Many tasks here include specific cases - in fact, many tasks require specific examples be illustrated for some more general code. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:48, 11 May 2016 (UTC)

::Would you (or someone else) be willing to improve the description? I'm not really much of a writer. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 06:22, 11 May 2016 (UTC)

:::Maybe - I need to sleep on this. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:36, 11 May 2016 (UTC)

== Precision ==

The desired precision should either be parameterized or specified in the task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:41, 11 May 2016 (UTC)

:I think the precision is already specified. It is to the nearest integer. The Python example demonstrates that integer values are sufficient to emulate real numbers. The trick with square roots is to multiply the number by 100 for every additional digit of accuracy after the decimal point. For a 7th root you would multiply by 10,000,000 for every additional digit of accuracy. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 06:07, 11 May 2016 (UTC)

:: Yeah - I should have phrased this as a specifying the value you are taking the root of. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:37, 11 May 2016 (UTC)

==is it possible to fix "R^N=X" in the task description?==
I want it to look like math notation not computer code. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 22:13, 11 May 2016 (UTC)

: It looks like math markup is having some problems, when I try it out. You probably can use that approach but it will probably take some fiddling. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:27, 12 May 2016 (UTC)

::What problems do you have? Can you see the following? <math>R^N=X</math> --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 12:09, 12 May 2016 (UTC)

::: That math markup renders as blank, for me. (But the math markup on [[Combinations]] looks fine.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:42, 12 May 2016 (UTC)

::::What browser do you use? What setting do you have under "Preferences/Appearance/Math? --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 16:34, 12 May 2016 (UTC)

::::: Chrome, and "MathML with SVG or PNG fallback" --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:20, 12 May 2016 (UTC)

::::::Ok, I have a theory why it is failing. [[wp:MathML#Web_browsers|Chrome doesn't support MathML]] (I've also tried with another WebKit browser called Vimb). So the fallback image is used. But for some reasons (I guess caching due to Cloudfare) it isn't shown. But the direct link to the fallback image should work: http://rosettacode.org/mw/index.php?title=Special:MathShowImage&hash=b6db46185bb825b2df70bfd507239446&mode=mathml
::::::I had another caching issue yesterday when I fixed an overwritten example output image. The task page where the image was included still showed the wrong image. But today everything was alright.
::::::So if my theory is correct, you should see the fallback image in a few hours. If not, something else is wrong. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 19:31, 12 May 2016 (UTC)

:::::::Something else is wrong. Math markup is failing on pages where it previously worked. This happens in Chrome, in Internet Explorer and in Safari (Firefox is the only browser where math markup works for me, on the two computers I tried). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:10, 16 May 2016 (UTC)

:::::::: I noticed that the last change introduced a "cross product" symbol/glyph, but it does not render correctly (under Firefox Aurora or MS Internet Explorer).   This has to do with the 3rd (and last example) in the task's preamble.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:42, 16 May 2016 (UTC)

:::::::::: You could use the     &amp;<nowiki>times;</nowiki>     symbol, it's not quite a large at the cross-product glyph, but it's more accurate.   There is a similar character ⨯ at U+2A2F, but this is not always considered identical to U+00D7, as U+2A2F is intended to ''explicitly'' denote the [[cross product]] of two vectors (last sentence as quoted in full from Wiki: [https://en.wikipedia.org/wiki/Multiplication_sign Multiplication sign]).   I like the expression (for the exponentiation)     100<sup>2000</sup>     or     100<sup>2,000</sup>     better though.     I.E.:   X=2&times;100<sup>2,000</sup>     or, for ease of reading:     <big> X=2&times;100<sup>2,000</sup> </big>     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:30, 16 May 2016 (UTC)

==if we have successfully moved to the new task page...==
Can we delete "2001 Digits Of Root Two"? [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 22:29, 12 May 2016 (UTC)
: Did you want to include a suggestion about representing a lot of digits? Or is that now silly? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:55, 15 May 2016 (UTC)

:: I was hoping we could get rid of the previous draft task page called "2001 Digits Of Root Two". It would be a shame if it has to stay around forever. I do not know how to delete it myself. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 23:24, 15 May 2016 (UTC)

::: That's not really an answer to that question. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:46, 16 May 2016 (UTC)

:::: Sorry. We could require more significant digits than is usually representable by floating point numbers if that is what you mean. You could change the task description if that seems like a good idea. I don't really know how we reach a consensus on such changes but it would definitely be okay with me. I believe the only affected solution would be the first Go language entry. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 03:37, 16 May 2016 (UTC)

::::I made a change in the task description to, hopefully, address your concern but if there are still some problems then please let me know. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 03:54, 16 May 2016 (UTC)

My original question is still outstanding. If it is impossible to remove a draft task then I will drop the issue. [[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 03:37, 16 May 2016 (UTC)

==Perl 6 example==
I tried running it here: https://ideone.com/PzMU1C
[[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 23:33, 15 May 2016 (UTC)

: The version of Perl 6 on IDEOne is ancient, pre-pre-alpha. For what it is worth, all that needs to change to make it work there is to expand the unicode sequence operator (…) in line 10 to its ASCII equivalent (...). See https://ideone.com/dUbfhi with the change applied. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 00:39, 17 May 2016 (UTC)

::I mentioned the out of date compiler version to IDEOne. They have promised to fix this issue. --[[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 21:21, 19 May 2016 (UTC)
