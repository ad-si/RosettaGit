+++
title = "Talk:Solve the no connection puzzle"
description = ""
date = 2016-04-11T13:46:53Z
aliases = []
[extra]
id = 18019
[taxonomies]
categories = []
tags = []
+++

== Video ==
According to the video, A and B, G and H are also connected. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 13:46, 11 April 2016 (UTC)

==REXX formatting seems off==
Near the top where it starts "aid=...", the lines after that seem mid aligned? (But I am viewing on my tab, but then again, it is usually OK)? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:48, 5 October 2014 (UTC)

: The REXX program no longer has that statement, perhaps you were viewing it whilst it was being updated?   Does the current version still have "off" formatting?   It looks OK to me, but then, it always did view OK to me at this end. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:48, 5 October 2014 (UTC)

Hi Gerard, I was talking specifically about the second section of code starting with the do-while loop that is all indented around sixteen spaces?

```rexx
/**REXX program solves the  "no-connection"  puzzle  (with eight pegs).*/
@.  =                                 
@.1 = 'A   C D E'          ;         @.5 = 'E   A B D F'
@.2 = 'B   D E F'          ;         @.6 = 'F   B E G'
@.3 = 'C   A D G'          ;         @.7 = 'G   C D E'
@.4 = 'D   A C E G'        ;         @.8 = 'H   D E F'

                   do nodes=1  while @.nodes\=='';   _=word(@.nodes,1)
                   subs=0              /* [↓]  create list of node paths*/
                              do #=1  for words(@.nodes)-1
                              __=word(@.nodes,#+1);  if __>_  then iterate
                              subs=subs+1;           !._.subs=__
                              end  /*#*/
                   !._.0=subs          /*assign the number of node paths*/
                   end   /*nodes*/

```
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:06, 5 October 2014 (UTC)

: <strike> (No '''L''' in Gerard). </strike>     Yes, the   '''DO NODES=1 ...'''   starts in column 20, the   '''DO #=1 ...'''   starts in column 31, the indentation structure depending on what (I think) looks better (and not using any hard and fast rules) and conveys the logic structure and/or purpose of the DO loops.   I sometimes use a great-indent (as used here) for some smaller DO loops, but normally I only indent (each) DO loop and SELECT structures two spaces. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:46, 5 October 2014 (UTC)

::Hi Gerard, what is the indentation aligned to that makes it look better? Is see nothing immediately above the "do nodes =1" and nothing immediately below the "end  /*nodes*/" that continues the visual alignment as well as no indication that that block is a sub block of the preceding code line? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:47, 5 October 2014 (UTC)

::: The whole point of my indentation is to indent (offset) the code to ''not'' continue the visual alignment with the code before and after the (greatly) indented code (that is, to make it very distinctive from the "mainline" code).   Another alternative would be to write a subroutine for the indented code (which has its own drawbacks).   The indentation of the DO loops is itself an indication of what it is, a block of code using a DO loop construct (and in this case, with another DO loop within it, also greatly indented).   Some programmers don't indent the DO statement at all; that isn't my style of coding.   I prefer to use indentation instead of relying on the DO and END statements standing on their own --- to me, the indentation (in this case, great indentations) serve that purpose better.   It's all very subjective, of course, and I tried to keep the reader of unfamiliar code forefront in mind.   I'm a firm believer in matching (perfectly aligned) indented code (as opposed to Egyptian indentations).   Matching/aligned indented code (which is evident in the programs that I code) makes it easier for persons unfamiliar with the code to peruse the program logic.   Using (matching/aligned) indented code helps the reader to bypass (eyeballing) that section of code on re-examinations of the program, making it easier to "jump or skip over" the DO loop construct on subsequent re-readings.   The idea/concept of (program logic) indentation is a very volatile and can be an argumentative subject.   Any discussion on program code indentations (akin to a religious or political debate, editor wars come to mind) can be very confrontational when being discussed or its use validated. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:02, 5 October 2014 (UTC)

::Hi Gerard, if you see a lot of this type of indentation style in REXX code from ''other'' authors too then that makes it ''idiomatic'' code which is the best sort for RC. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:55, 6 October 2014 (UTC)

==Task Name==
When I wrote the Hidato Puzzle task I was "encouraged" by them what be to have it named "Solve a Hidato puzzle". Should this tasks name not follow this convention?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:21, 6 October 2014 (UTC)

:Fixed, thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:15, 10 October 2014 (UTC)

==REXX annotated vs not examples.==
If the algorithm and features are the same then you should make the choice as to which to add as the most idiomatic for the language I think. You can add more explanation  after the program, and yet more in the talk pages -for example see the Knights tour talk page.--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:26, 10 October 2014 (UTC)

: The (REXX) algorithm to solve the puzzle is identical in both versions, but the feature showing the annotated graph is not.   This feature was added (to show the graph) is a fair amount of program code that was included in the second version which (I thought) distracted from the algorithm solving the problem.   So, I kept the annotation version as a separate entry to make it easier to view and understand what the first version was doing without the clutter of showing a prettier version of the output.   I felt the REXX code that showed the graph essentially wasn't part of the task requirement, and it bloated (somewhat) the first version a bit, but I admit the output is much easier on the eyes.   To me, it wasn't about which is most idiomatic, but which was easier to read because of more code to merely display the (annotated) graph. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:59, 10 October 2014 (UTC)
::Hi Gerard, I did similar in the Python entry but did not have to duplicate code - I just said append this to get this extra output. I see your point now, thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:34, 10 October 2014 (UTC)
