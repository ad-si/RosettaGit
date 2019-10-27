+++
title = "Talk:Zebra puzzle"
description = ""
date = 2018-01-19T16:16:33Z
aliases = []
[extra]
id = 11003
[taxonomies]
categories = []
tags = []
+++

== Who has the zebra, indeed? ==
f
Firstly, I can't find a solution as stated.  The closed I got sastified all but one condition. It could be logic error on my part, but it would help if a list of all properties of all house as solved can be listed here for doublechecking.
: This is an old problem and I've done it by hand.  The version I recall was identical except that the missing pet was fish.  I had started some time ago to write a task for this as an elaboration of the Dinesman solution and then I put it aside and forgot it - oh well. So I suspect you've missed something. If I recall when I did it by hand I used a matrix to solve it.  --[[User:Dgamey|Dgamey]] 04:31, 5 December 2011 (UTC)
:: It was a typo on my part, disregard my above comment about no solution; though a listing would still be helpful. --[[User:Ledrug|Ledrug]] 04:41, 5 December 2011 (UTC)
::: Maybe an addition to the task description is in order: ''"Additionally, show the solution for all the houses. Optionally, show the soltion is unique."'' What do you think? [[User:WillNess|WillNess]] 11:33, 7 December 2011 (UTC)

Secondly, the Prolog solution took some shortcuts ("Norwegian in first house" and "Norwegian next to blue house" effectively rolled into one as "the second house is blue").  That's trivial deduction by a person, but it's better to leave the input conditions exactly as stated in the task, to minimize human involvement in this process. --[[User:Ledrug|Ledrug]] 02:19, 5 December 2011 (UTC)
: I noticed that too --[[User:Dgamey|Dgamey]] 04:31, 5 December 2011 (UTC)
:: Anything with fewer lines than the number of conditions would seem to me to have been artificially compressed --[[User:Dgamey|Dgamey]] 12:49, 5 December 2011 (UTC)
::: I've untangled and fixed that thing with the blue house in the Prolog code now. The rest of shortcuts seems acceptable to me - i.e. the point I wanted to make was about mutually-exclusive choices, like with nationalities - it is a programmer's choice how to write the spec down - whether one by one, or in one list if possible. [[User:WillNess|WillNess]] 11:33, 7 December 2011 (UTC)

I had always known this as the Einstein Logic Puzzle. I suggest a redirect from there to Zebra.  --[[User:Dgamey|Dgamey]] 04:31, 5 December 2011 (UTC)
:There seem to be a few names on the WP page. Make as many redirects as you think are necessary. I don't think anyone will confuse it with anything else. --[[User:Mwn3d|Mwn3d]] 05:20, 5 December 2011 (UTC)
:: Added one for Einstien Logic Puzzle --[[User:Dgamey|Dgamey]] 12:50, 5 December 2011 (UTC)

== C# lines ==

The C# code has lines 190+ chars long. -- [[User:Bearophile|Bearophile]] 18:52, 31 May 2012
:I added the "lines too long" template to the C# code. [[User:Dchapes|Dchapes]] ([[User talk:Dchapes|talk]]) 18:31, 2 August 2014 (UTC)
:I added my much shorter solution and made it the first one in the c# section --[[User:Martinfreedman|Martinfreedman]] ([[User talk:Martinfreedman|talk]]) 13:32, 17 January 2018 (UTC)
:I am new here but I think the old Solver solution is an embarrassment. It is too long even as a Solver solution - compare to C# solver in the [[Sudoku#.E2.80.9CAutomatic.E2.80.9D_Solution|Sudoku]] task. I humbly suggest a moderator deletes it? I can add a far shorter Solver solution too boot.--[[User:Martinfreedman|Martinfreedman]] ([[User talk:Martinfreedman|talk]]) 08:48, 19 January 2018 (UTC)
Tried to refactor it was was truly awful c# code and an awful implementation of Solver too. Replaced it --[[User:Martinfreedman|Martinfreedman]] ([[User talk:Martinfreedman|talk]]) 16:16, 19 January 2018 (UTC)

== FormulaOne ==
A new addition, but an effective one. A typical characteristic of FormulaOne is the ease with which one can transpose the literal (English) text of the constraints into code: 

// The Englishman lives in the red house and the Swede has a dog and the Dane drinks tea
   houseColour(Englishman) = Red & pet(Swede) = Dog & drinks(Dane) = Tea
Not many (?) languages know this ease of coding. This is chiefly due to the implementation in FormulaOne of the (mathematical) injection -- represented by an indexed array with distinctive elements, the use of relations, and the use of unknown indices. Cf. Example 2 (to be added on the Content Page). -- [[User:snwi|Will Snellen]] 17:52, 31 March 2015 (UTC+1)

:Hi Will, you state "Output in FormulaOne (a bit formatted):" which makes me think how much extra formatting has been done that is not in the program... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:00, 31 March 2015 (UTC)
::Paddy, misleading on my part. I used some formatting for this page... --[[User:snwi|Will Snellen]] 14:45, 1 April 2015 (no joke!) (UTC+1)

:Hi again Will. tips on formatting: We normally put all program source in a <nowiki>
```formulaone>
```
</nowiki
 tag, then add <nowiki>{{out}}</nowiki> which expands ot an output header then put program output between <nowiki>
```txt

```
</nowiki> tags. Maybe your frist line is not code and should precede the lang tags?
: On ''"By designing some 'PrettyPrint'-predicate, one could produce output like..."'', it is best to delete that section of output. If you don't show code then it is wrong to put the prettier output up for comparison with other language output.
:Lastly, I note you have added what seems to be a run time of 0. If that is part of the output shown from running the program then fine, (although it doesn't add much). 
:Please don't take these criticisms the wrong way Will, its' just me trying to pass on what good advice I got when I first joined RC :-)
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:50, 2 April 2015 (UTC)
:
:Paddy, better this way? Please discuss, if need be..
:--[[User:snwi|Will Snellen]] 21:00, 18 April 2015 (UTC+1)

:: Yep. Brill :-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:35, 18 April 2015 (UTC)
