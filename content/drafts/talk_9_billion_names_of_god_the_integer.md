+++
title = "Talk:9 billion names of God the integer"
description = ""
date = 2013-05-08T12:31:54Z
aliases = []
[extra]
id = 13415
[taxonomies]
categories = []
tags = []
+++

==task clarification==

I presume from the task requirement that the output is to more-or-less look like the (partial) number triangle shown, that is, a symmetric isosceles triangle in the manner of Pascal's triangle.   Producing a left-justified triangle doesn't look or feel right. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:35, 2 May 2013 (UTC)
:That's really kinda silly, y'know?  And you can't do it perfectly symmetrical anyway unless you can half-space.  In any case, it's arguably <em>not</em> a symmetrical triangle after row 4... --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 01:57, 3 May 2013 (UTC)
:More to the point, the <em>algorithm</em> isn't symmetrical; the values are not derived from the two values above. The visual identity with Pascal's triangle is completely vacuous, and the ancestors the algorithm is visiting are, in fact, easier to see with the left-justified form!  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:09, 3 May 2013 (UTC)

:: Sorry, I didn't mean to get anybody all worked up and start with the name calling.   I meant to say ''isosceles'' triangle, not ''symmetric isosceles''.   I never intended to imply that the values in the triangle were (perfectly?) symmetrical.   The visual comparison with Pascal's triangle was referring to the shape, not the values of the numbers in the triangle, and not also, of course, how the numbers are derived.   Indeed, the task's author chose to show the first ten rows in an isolsceles triangle shape, and I followed (or mimiced) that manner. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:15, 3 May 2013 (UTC)
:::Gerard, don't appologize to offensive people like this it only encourages them. Vacuous 'Having or showing a lack of thought or intelligence; mindless: "a vacuous smile"'. Insulting people does not invalidate their ideas and is unacceptable. I think that the solutions which have attempted a full solution of the task demonstrate the symmetry of OTT. There is more than a visual similarity to Pascal's Triangle.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:31, 8 May 2013 (UTC)

The 2nd part of the task's requirement states that the   ''integer partition function''   ('''IPF''')   is the same as the sum of the ''n''-th row of the number triangle (constructed above), and furthermore, this is to be demonstrated.   None of the examples (so far) has shown the last line of any of the ''P''(23), ''P''(123), ''P''(1234), and ''P''(12345) for this purpose.   Indeed, it's doable, but the last line of the bigger number triangles would be huge.   Are the program examples supposed to sum the last row of the number triangle   ''and''   verify via calculating the '''IPF''' via formulaic means? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:49, 2 May 2013 (UTC)
:The origional task description did not mention the IPF and called for 25 lines of the triangle. The purpose of G(n) is to show that you can generate the nth line of this triangle without having to display all 12345 elements of it. Other formulations are interesting but not solutions to this task. I have added a child task http://rosettacode.org/wiki/9_billion_names_of_God_the_integer/Worship_of_false_idols where I think that these related triangles should be discussed.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:12, 3 May 2013 (UTC)
:Note that http://rosettacode.org/wiki/9_billion_names_of_God_the_integer#Full_Solution does exactly that which you say no example does.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:12, 3 May 2013 (UTC)

:: I don't see where it displays the last line of the ''P''(23), ''P''(123), ''P''(1234), and ''P''(12345) number triangles.   Most of the programming examples did show the sums of the last lines, of course. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:26, 3 May 2013 (UTC)
:::See http://rosettacode.org/wiki/Talk:9_billion_names_of_God_the_integer#The_One_True_Triangle.2C_Three_Triangles_in_One_Eternal_Trinity for a full explanation.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:26, 4 May 2013 (UTC)

==generating function for P(n)==
If the formula shown under the '''C''' example is Euler's generating function, is it missing a   '''½'''   multiplier? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:35, 2 May 2013 (UTC)
: It's not a generating function, but yes, both the subscripts in the displayed equation were missing a "/2". --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:15, 3 May 2013 (UTC)
==The One True Triangle, Three Triangles in One Eternal Trinity==
[[File:ThreeTrianglesInOneEternalTrinity.png]]
:Clearly the 1's are symetric, and can be found easily as G(n,1), G(n,n-C<2).
:The grey triangle is symetric and can be found by index to ina1 (see below).
:The vertical edge of the green triangle is ina1 in [http://rosettacode.org/wiki/9_billion_names_of_God_the_integer#Full_Solution Ruby Full Solution] and is the horizontal edge of the grey triangle.
:The horizontal edge of the green triangle is ina2 in [http://rosettacode.org/wiki/9_billion_names_of_God_the_integer#Full_Solution Ruby Full Solution].

So all the action takes place in and it is only necessary to solve the green triangle.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:15, 4 May 2013 (UTC)

==The Green Triangle==

[[File:My Green Triangle.png]]

Solving consecutive hypotenuses for the green triangle starting with the longest and moving in does this easily. ina1 and ina2 may be obtained by taking the first and last entry in each vector calculated.

3 + sum over ina1 + sum over ina2 is the sum of row n. For rows less than n/2 it is available in ina1.

Technically I could solve the task simply by summing ina2 as we go, but saving ina1, ina2 and the last calculated hypoternuse is all that is required to stop and restart the algorithm at any point.

The memory required in terms of number of integers is largest at the start of the algorithm, as each hypotenuse except the last can be discarded after obtaining ina1 and ina2. It is less than 3 time the number of rows in integers, though the actual memory required per integer will depend on your language or library.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:15, 4 May 2013 (UTC)
