+++
title = "Talk:Floyd's triangle"
description = ""
date = 2017-09-12T06:57:05Z
aliases = []
[extra]
id = 11907
[taxonomies]
categories = []
tags = []
+++

==Inspiration==
I need to add a link to [http://www.youtube.com/watch?v=Yhlv5Aeuo_k Vihart].

Can the colour of the font of just the task description be changed to pink? (A [http://upload.wikimedia.org/wikipedia/en/3/3b/Dark_Side_of_the_Moon.png Dark side of the moon] joke). --[[User:Paddy3118|Paddy3118]] 19:13, 24 June 2012 (UTC)

==Output==
* Has to be shown
* Has to be formatted just-so.
--[[User:Paddy3118|Paddy3118]] 19:17, 24 June 2012 (UTC)

I am changing the output from print n=5 and n=15 to '''n=5 and n=14''' to highlight the case of what should be done when the number of digits in the numbers of the last row changes, and is not the last digit itself. In this case, column widths need to change. Sorry Go language (but thanks for the quick example). The reason is, that I am trying to be more rigourous about the output format than in [[Pascal's triangle]]. --[[User:Paddy3118|Paddy3118]] 06:52, 25 June 2012 (UTC)

: The current format requirement may be rigorous, but not intuitive.  You go through all this trouble to align digits, yet because widths of numbers vary within each line, the slanted right edge of the triangle is not a straight line and looks unpleasant.  What's the point of the complicated formatting, really? --[[User:Ledrug|Ledrug]] 23:03, 25 June 2012 (UTC)

:: Partially a reaction to the issues seen in [[Pascal's triangle]] where there was not enough rigor. No doubt someone could still find a way to adhere to the letter of the task rather than the spirit, but that too could be interesting to see. (I pity the people that have to write documentation for their work who don't try doing something similar for an open source type project where lessons learned can be rapid and sometimes brutal, but on RC - kind-a nice).
:: Partially because some specs are just as particular; with non-intuitive requirements that are even more hidden. --[[User:Paddy3118|Paddy3118]] 02:07, 26 June 2012 (UTC)

:: P.S. Should I add an optional goal: ''to add an option to your routine that can produce equi-width columns'' ? --[[User:Paddy3118|Paddy3118]] 02:07, 26 June 2012 (UTC)

::: The last requirement (2nd requirement) --- ''that only one space separates numbers of the last row'' --- this is an incorrect (impossible) requirement as the last line may have different width (length) numbers and some may have one blank, others will have two blanks, and that would preclude the numbers lining up as shown in the example. Case in point would be for a four or fifteen-row Floyd's triangle. If this assumption isn't correct, then the author has to change line four of the example to match their own requirement.  For now, I'm programming the 2nd REXX version as it is shown in the example (''The first few lines of a Floyd triangle looks like this''), and that's what the programming example does (and displays), and what almost all examples have shown. -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:33, 17 July 2012 (UTC)

::::Hi Gerard, 
::::* First arrange that the columns are of such a width that all the numbers of the last row are separated by one space only.
::::* Then ensure that each column of numbers is packed to the right of its column width.
:::: Et voilà! --[[User:Paddy3118|Paddy3118]] 06:58, 17 July 2012 (UTC)

::::: My bad, I wasn't seeing what I should've been seeing when looking at the other examples. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:43, 17 July 2012 (UTC)
::::: I recently learned the name what I had was   ''virtual scotoma''.   ───   ''The mind sees what it chooses to see''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:56, 12 September 2017 (UTC)

Is Version 1 correct?
Wide columns only when required by the last line's values.
--[[User:Walterpachl|Walterpachl]] 08:03, 17 July 2012 (UTC)
:Looks OK, but output for n=5 is missing. --[[User:Paddy3118|Paddy3118]] 12:23, 17 July 2012 (UTC)

addad it. thanks. Version 2 (Gerard's) needs to be fixed --[[User:Walterpachl|Walterpachl]] 13:18, 17 July 2012 (UTC)

--The run BASIC, erlang, and FORTRAN results look incorrect.  I'll try to fix the FORTRAN entry.  --LambertDW 02:41, 22 May 2013 (UTC)

:Thanks LambertDW, I have marked the other two incorrect and hopefully they will be updated. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:21, 22 May 2013 (UTC)

The output I had included for the Common Lisp examples included the NIL the REPL displays when the function is run from there (format t ALWAYS returns NIL). I have removed the NILs from the output section as they aren't "extra," and won't display when the function isn't run from the toplevel.--Sovietologist 20:05, 18 September 2013 (UTC)

:Thanks Sovietologist. As you can read above, the format specs. are a little more rigorous as a reaction to the [[Pascal's triangle]] task, where you had right triangles being submitted rather than the triangles with symmetry across the vertical centre.--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:59, 19 September 2013 (UTC)
