+++
title = "Talk:Special variables"
description = ""
date = 2014-12-20T21:46:52Z
aliases = []
[extra]
id = 9854
[taxonomies]
categories = []
tags = []
+++

==On what to include==
I would think that a Perl entry would be very long. Should a Python entry include things like sys.argv? Again, if it should, then there would be a large number of such and it probably wouldn't help when comparing languages as not much could be got from comparing Pythons list with AWKs for example. --[[User:Paddy3118|Paddy3118]] 19:44, 2 June 2011 (UTC)

:I would have expected sys.argv to be listed, but I am not familiar with python, so I don't know what such an entry would look like. If sys just a single variable with an element argv, then we could probably just list it as sys, and provide some explanatory notes and maybe a cross reference to some other documentation.

--[[User:Markhobley|Markhobley]] 21:46, 2 June 2011 (UTC)

Should special function names be included (e.g. "main" for lots of languages)? --[[User:Mwn3d|Mwn3d]] 19:46, 2 June 2011 (UTC)

:I hadn't planned to include function names here. My initial thought is stay within the scope of "special variables". We could possibly cross reference to tasks relating to function names, or possibly just makes some comments if one affects the other, or the distinction is blurred. 

--[[User:Markhobley|Markhobley]] 21:46, 2 June 2011 (UTC)

Every definition of the Python standard library could be taken as a "special variable". I think this task is fine forAWK, but could lead to too much work to be practical for Python. --[[User:Paddy3118|Paddy3118]] 19:51, 2 June 2011 (UTC)

:I think we could just place a comment that this is so without listing the contents of the library.

--[[User:Markhobley|Markhobley]] 21:46, 2 June 2011 (UTC)

If a distinction can be made between variables defined by the core language and variables defined in standard libraries, maybe only the ones for the core language should be listed.  Also, the variable part could be stressed.  We might want to exclude predefined constants.  &mdash;[[User:Sonia|Sonia]] 02:48, 3 June 2011 (UTC)

:Predefined system constants probably fall under the category of informative special variables for this task and should be included. Mathematical constants are not special variables and should not be included. --[[User:Markhobley|Markhobley]] 00:01, 8 June 2011 (UTC)

:: Actually, the mathematical constants may be referred to by a predefined or reserved identifier in some languages. If this is the case, then I suggest that we include those too. --[[User:Markhobley|Markhobley]] 16:37, 12 June 2011 (UTC)

== REXX Special variable RETURN ==

Just a summary of a long offline discussion and the facts as I see them

 REXX version 1 states   RESULT     [the result RETURNed from a subroutine or function] 

 The REXX standard and several implementations (Regina, VM, TSO) state that RETURN is set after subroutine calls and not after function invocations.

 Some other (older) implementations set it also after function invocations, thereby violating the standard (which was, I think, only created at a later point in time).
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:38, 20 December 2014 (UTC)

: The 2nd statement (... and not after function invocations.) isn't written as such  --- there is no ''not'' or ''except'' used in the IBM pubs regarding this subject.   Of what I read of the current IBM publications, at best, it is possibly inferred and the wording ''could be'' interpreted that way.   If the current IBM publications (VM REXX reference, and the TSO/E version) had stated it as you said, there wouldn't be any disagreement.   The Regina REXX publication is the only place that mentions explicitly that if subroutine was invoked via the CALL verb, that the RESULT special variable was set/defined from the expression of the RETURN statement, or DROPped if there wasn't an expression.   I didn't read anywhere in the current IBM publications that RESULT is set/define and NOT for function invocations.   If it did, there wouldn't be a discussion regarding this matter.   That other authors of other REXXes (not mentioned here in this talk section) implemented it differently speaks to the manner of interpreting what the publications state (or more probably, what they didn't state).   That the "standard" was written after some of the older REXXes where written and implemented shouldn't make the older REXX interpreters incorrect (in my opinion).   It is important to note that various REXX interpreters have different ... er, results regarding the issue of setting/defining the RESULT special variable (or not setting it). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:46, 20 December 2014 (UTC)
