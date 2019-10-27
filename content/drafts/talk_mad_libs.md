+++
title = "Talk:Mad Libs"
description = ""
date = 2016-10-26T02:11:04Z
aliases = []
[extra]
id = 10814
[taxonomies]
categories = []
tags = []
+++

==Task name==
Hi, Why Madlibs? Does that name have some history to it? (Normally we try and be short and descriptive). --[[User:Paddy3118|Paddy3118]] 07:48, 7 November 2011 (UTC)
: see [[wp:Mad Libs|Mad Libs]] on wikipedia. maybe a reference to this should be added to the task description.--[[User:EMBee|eMBee]] 08:43, 7 November 2011 (UTC)
:: We ought to work that reference onto the task page somehow. –[[User:Dkf|Donal Fellows]] 10:49, 7 November 2011 (UTC)
::: copied the first sentence from wikipedia. i think that works as introduction with a reference.--[[User:EMBee|eMBee]] 11:34, 7 November 2011 (UTC)

== Repeated substitutions? ==

Most Mad Libs templates have many nouns, verbs, etc., which are all independent.  How should we decide when a replacment is meant to be repeated, like "name" in the example?  --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 02:09, 23 May 2015 (UTC)
: Well to comply with the instructions only as given, you’d replace all instances repetitively, so that’s what most people have done. If an independent name was needed, it would need to be called name2 for example.--[[User:Jnd|Jnd]] ([[User talk:Jnd|talk]]) 05:47, 23 May 2015 (UTC)

== Program specification ==

The program specification say that the template should come from input, but many programs take input from a file or just a variable instead. Since the interesting bit is the parsing and string manipulation, I think that's fine, but in that case the specification should be reworded. (I can't come up with a concise, precise wording for it myself, so I'll leave that to someone else.)

: I think you are interpreting "input" as something like C's "standard input", but "input" has been a general concept that has included files for decades. (And, in fact, the OS abstraction that we think of when we say "standard input" was strongly influenced by this more general usage.)

: Anyways, it's probably good to use console input for this, but I think we can forgive people who went with a more general interpretation of that word. 

: That said, it might be interesting to think about a "[[User_input/Text|user input's text]]" task, and what it would take to wire it up to a mad-lib implementation here which uses some other form of input. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 08:45, 25 October 2016 (UTC)

:: Makes sense. Does it make sense for this task to take the replacements from anything other than console input though? On the one hand, that's not really part of the core string manipulation. On the other hand, hard coding the questions and answers is against the spirit of the problem. [[User:MagiMaster|MagiMaster]] ([[User talk:MagiMaster|talk]]) 01:42, 26 October 2016 (UTC)

::: Are you asking about the entries which have been marked as incorrect, or something else? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:10, 26 October 2016 (UTC)
