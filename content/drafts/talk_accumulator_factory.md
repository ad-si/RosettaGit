+++
title = "Talk:Accumulator factory"
description = ""
date = 2010-06-08T16:01:38Z
aliases = []
[extra]
id = 5256
[taxonomies]
categories = []
tags = []
+++

==Motivation, Links, etc. ==
Part of my motivation for making this task is frustration that I can't enter the original challenge! On the other hand, it also illustrates a number of interesting techniques (closures, stateful functions) that aren't well-demonstrated on RC yet so it's still useful. And I'm pretty pleased with how well I managed to distill my Tcl solution too.

There are existing solutions at http://www.paulgraham.com/accgen.html and the original essay is at http://www.paulgraham.com/icad.html; overall, this should help people get started with producing implementations. However I also think that this page needs more editing of the heading material so that we're less reliant on Paul's content and instead better linked in with everything else. That's why this is still a draft task.

My thanks to my fellow Tclers (especially those who have worked on http://wiki.tcl.tk/25449) for bringing this to my attention. –[[User:Dkf|Donal Fellows]] 23:34, 28 December 2009 (UTC)

== Nit-Picks ==

* You don't have to print the accumulator function. That's just a peculiarity of how some languages work in interactive mode.
:: It seems too pointless to create a new accumulator without doing ''something'' with it. --[[User:Glennj|glennj]] 12:50, 29 December 2009 (UTC)
* Strictly according to the rules, the Ocaml code is incorrect, since it isn't polymorphic in the type of number. I'm happier than Paul to let that slide. –[[User:Dkf|Donal Fellows]] 07:47, 29 December 2009 (UTC)
:: How about an insistence on examples stating such differences from the spec up-front? --[[User:Paddy3118|Paddy3118]] 08:50, 29 December 2009 (UTC)
::: It's still a draft. Feel free to update the wording yourself. –[[User:Dkf|Donal Fellows]] 10:57, 29 December 2009 (UTC)

== spec needs fixing ==

The specification currently requires that our posted solutions take functions which return one value.  But no one does this and it does not make sense here.  (I would fix this myself but I want a sanity check on my reading.)  --11:55, 1 June 2010 (UTC)

: I don't quite understand what you mean here. –[[User:Dkf|Donal Fellows]] 14:19, 8 June 2010 (UTC)
:: The current task says "Before you submit an example, make sure the function  1. Takes, and returns functions that take, exactly one argument."  But, to my knowledge, none of the implementations takes a function [as an argument] -- instead they all seem to be taking a number [as an argument] and returning a function of one argument [as a result].  --[[User:Rdm|Rdm]] 14:55, 8 June 2010 (UTC)

:Updated using info from [http://paulgraham.com/accgen.html this page]. --[[User:Paddy3118|Paddy3118]] 16:01, 8 June 2010 (UTC)
