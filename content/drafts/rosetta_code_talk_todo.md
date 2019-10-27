+++
title = "Rosetta Code talk:TODO"
description = ""
date = 2009-07-06T20:40:54Z
aliases = []
[extra]
id = 2216
[taxonomies]
categories = []
tags = []
+++

I can do the headers...I've already started sort of. --[[User:mwn3d|mwn3d]] 15:00, 11 November, 2007
==String Length==
String Byte Length and String Character Length were split because many examples in String Length didn't differentiate between 8-bit characters and wide characters, which is an important distinction for low-level languages on modern systems.  If they get merged, this distinction needs to be preserved. --[[User:Short Circuit|Short Circuit]] 13:20, 2 December 2007 (MST)
:Agreed. Currently, many of the string tasks have the same code between the two pages, so you can't tell which one is correct for each page. Once merged, there won't be duplication, and we can tag the language with only one string operation with the "incorrect" template. --[[User:IanOsgood|IanOsgood]] 09:08, 3 December 2007 (MST)
::That might not help; the distinction between a "string of bytes" and a "string of characters" might be hidden. For example, [[Tcl]] does not expose its actual low-level string interpretation scheme to the script level because its internally polymorphic according to what is the most efficient way to handle the operations being applied to the string. To get the real "byte length" of a string, you first have to state what encoding you want the string in… —[[User:Dkf|Donal Fellows]] 13:17, 13 June 2009 (UTC)

== HOPL ==

Does the HOPL have a regular URL pattern that we can take advantage of with the links? It's very slow to load and difficult to find these relevant pages. --[[User:Mwn3d|Mwn3d]] 21:06, 2 March 2008 (MST)
:It may be possible to link directly to a [http://hopl.murdoch.edu.au/findlanguages2.prx?language=Ada&which=byname search query] by using PAGETITLE. --[[User:Short Circuit|Short Circuit]] 19:54, 4 March 2008 (MST)

==Language Highlighting==
The current language highlighter does a very poor job for Ada. It does not highlight most of the Ada reserved words. How can I fix the reserve word list for Ada in the highlighter?--[[User:Waldorf|Waldorf]] 21:44, 3 March 2008 (MST)
: It's hardcoded in the highlighter PHP.  Thankfully, that code looks easy to work with.  Can you point me to a complete list of reserved words?--[[User:Short Circuit|Short Circuit]] 19:50, 4 March 2008 (MST)

== "Less than" categories ==

Should we remove links to those categories? Will there be another solution? Should we start putting more links in? --[[User:Mwn3d|Mwn3d]] 12:13, 5 May 2008 (MDT)

== Propose changing the task penetration target ==

There are a lot of really obscure programming languages on RC that have only implemented a few tasks, which inflates the ‘25%’ level making it hard to get a reasonable penetration. Perhaps a fixed penetration level would work better? I suppose we could have 5 languages as a minimum level and 20 as a "highly penetrated". Of course, this could go wrong if a task is implemented by lots of obscure langs, but that's a problem I'd love to have! Having been through the whole list in the recent past, we're in no danger there at all...

I picked the levels above because they mark real perceived differences between obscure/difficult stuff (like [[Rendezvous]]) and normal tasks, and when 20 langs have done a task, it's because it is ''very'' widely done. Few tasks are anywhere near that level right now. —[[User:Dkf|Donal Fellows]] 20:40, 6 July 2009 (UTC)
