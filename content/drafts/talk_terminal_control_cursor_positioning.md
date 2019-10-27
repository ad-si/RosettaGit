+++
title = "Talk:Terminal control/Cursor positioning"
description = ""
date = 2011-08-24T02:55:01Z
aliases = []
[extra]
id = 9754
[taxonomies]
categories = []
tags = []
+++

==Origin?==
The specification is flawed. If the top left-hand corner at (0,0) or (1,1) ?

:The top left hand corner would be row 1, column 1. This may be represented as (0,0) or (1,1) depending on the language used. For the purpose of this task, solutions implemented in languages that use (0,0) to represent the top left corner would need to subtract one from the row and column numbers to place the cursor at the correct position. --[[User:Markhobley|Markhobley]] 01:04, 4 June 2011 (UTC)

:: I used (0,0) as the top-left corner, for [[{{SUBJECTPAGENAME}}#Ruby]] and for [[Terminal control/Positional read#C]]. There is no standard yet; [[:Category:curses|curses]] puts the top-left corner at (0,0), but ANSI escape sequences put it at (1,1). I will not become too surprised if someone uses (0,0) as the ''bottom''-left corner. I imagine that all the world will adopt a standard coordinate system next January 0 at midnight 01:01. If these tasks decide to use (1,1), then I will want to change my code. --[[User:Kernigh|Kernigh]] 02:55, 24 August 2011 (UTC)
