+++
title = "Talk:Parsing/Shunting-yard algorithm"
description = ""
date = 2015-11-30T04:32:13Z
aliases = []
[extra]
id = 11005
[taxonomies]
categories = []
tags = []
+++

==C and missing functionality==
What is the process for a task missing functionality when it could, but isn't yet provided. I can't remember the tag we have for "Please improve", but I guess it would go well with the current C entry that seems to be not written with the full task goals in mind. --[[User:Paddy3118|Paddy3118]] 19:32, 5 December 2011 (UTC)
:Are you looking for {{tmpl|improve}}? If you're ever looking for a template, try [[:Category:Example attention templates]] and [[:Category:RCTemplates]] first. What is the C example missing? --[[User:Mwn3d|Mwn3d]] 19:39, 5 December 2011 (UTC)
::It ''was'' missing either a prominent explanation of why it cannot provide the output requested in the task description or it needs that output; it also lacked the example from the task description, the lexing of which is designed to be straight-forward and so allow code to concentrate on the shunting-yard part. 
::There is no indication of the form of any output - the task requires a space separated string of tokens and this feature directly links to the other two tasks linked to from the task description that take that space separated RPN and either calculate the value or convert it back to an infix expression (whilst asking that you display what is happening in key data structures). --[[User:Paddy3118|Paddy3118]] 19:55, 5 December 2011 (UTC)

<nowiki>{{improved}}</nowiki>! --[[User:Paddy3118|Paddy3118]] 05:47, 6 December 2011 (UTC)

==incorrect programs?==

I noticed that at least one programming example is treating the input string as characters instead of tokens.   This works as long as all the numbers (and/or operators) are single characters.   Would it be possible to change (shudder) the test input string specification (or add another) to include a (say) multi-digit number?   If it's not reasonable to add another test case, how about changing the   <big>'''4''' </big>   to   <big>'''004'''</big>   which wouldn't affect the calculated output.   Another possibility would be to change (say) the   <big>'''5'''</big>   to   <big>'''5.'''</big>   (for instance).   And yet another possibility would be to add   <big>'''**'''</big>   as an additional exponentiation symbol (or some other compound operator).   Changing the test case (or adding one) will eliminate the need for flagging the program as   ''incorrect''   which I probably won't commit to doing as I really don't have any practical way to verify if the suspect program will work correctly with tokens instead of characters.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:42, 14 December 2012 (UTC)

== Improve Description ==
This seems to be about infix to RPN but it doesn't say it explicitly.  --[[User:Dgamey|Dgamey]] 04:00, 13 December 2011 (UTC)

:Hi Dgamey, from the wikipedia article:
::''"In computer science, the shunting-yard algorithm is a method for parsing mathematical expressions specified in infix notation. It can be used to produce output in Reverse Polish notation (RPN) or as an abstract syntax tree (AST)."''
:The title reflects that it is this shunting-yard algorithm that is to be used in transforming infix to, in this case, RPN. --[[User:Paddy3118|Paddy3118]] 09:07, 13 December 2011 (UTC)
:: It's a minor nit I know but it didn't say it.  I made a slight tweak in the description.  Not everybody knows algorithms by name or remembers if they have broader application.  Nor does everyone read the WP article. (Good set of tasks though). --[[User:Dgamey|Dgamey]] 14:45, 13 December 2011 (UTC)

:::Thanks for the great criticism Dgamey. Ideally I'd want to put enough on the RC page to make it complete in itself, but sometimes the linked-to wikipedia page is quite rich (and I feel a little lazy ...) --[[User:Paddy3118|Paddy3118]] 06:23, 14 December 2011 (UTC)
:::: And then there are those lazy people who don't want to read the WP article :)  --[[User:Dgamey|Dgamey]] 12:26, 14 December 2011 (UTC)
