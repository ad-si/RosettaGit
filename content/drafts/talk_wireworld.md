+++
title = "Talk:Wireworld"
description = ""
date = 2010-07-26T06:23:10Z
aliases = []
[extra]
id = 4834
[taxonomies]
categories = []
tags = []
+++

==Ruby Problem==
Consider the top left cell in the initial generation which is a 't'. The only thing that a 't' can change to is a '.', yet the next generation shows it as a 'H' ? --[[User:Paddy3118|Paddy3118]] 20:09, 9 September 2009 (UTC)

P.S. Does the Forth or TCL output agree with the Python? It would be good to know that at least two agree :-)
--[[User:Paddy3118|Paddy3118]] 20:14, 9 September 2009 (UTC)

Python and Forth are correct. I've marked Ruby incorrect. --[[User:IanOsgood|IanOsgood]] 20:22, 9 September 2009 (UTC)

: At a guess, the Ruby code is changing a tail into a head directly. Not quite what the transition table states... —[[User:Dkf|Donal Fellows]] 20:37, 9 September 2009 (UTC)
:: Indeed.  I read the transition table incorrectly. --[[User:Glennj|glennj]] 00:57, 10 September 2009 (UTC)

== Python libraries ==

I tried running the python implementation, but I don't seem to have the requisite libraries installed on my Ubuntu laptop.  Use of [[Template:Libheader]] and filling of the subsequent links would be helpful for visitors in the same boat. --[[User:Short Circuit|Michael Mol]] 06:41, 10 September 2009 (UTC)
:Hi Michael, check your installation as I just executed it on Python 2.6 and 3.1 on Windows. It uses only standard modules, and I would expect it to work, unchanged, on full installations of those versions of Python on Mac and *nix too. --[[User:Paddy3118|Paddy3118]] 07:05, 10 September 2009 (UTC)
::It does use the namedtuple class factory though, which is new to 2.6 and 3.x. --[[User:Paddy3118|Paddy3118]] 07:14, 10 September 2009 (UTC)

== Performance ==

Many years ago, I built an implementation of this task in [[Pascal]] (specifically Turbo Pascal) that used a number of tricks to go much faster. The source to that code is now long lost, but IIRC it used a ring-buffer of cell coordinates so that scans through the set of cells only needed to handle those cells with potential to change. This meant that the code could rapidly (15-20fps) process problems up to about the 300k cell mark (i.e., 640x480) which was a size capable of performing interesting calculations. Not bad for a 16MHz 386SX computer with less than 640kB available memory. :-) I do ''not'' remember all the details, but I presume that someone will be able to use this sort of hint/memory to come up with something equally amazing. —[[User:Dkf|Donal Fellows]] 19:45, 10 September 2009 (UTC)

: Well, for a start You only ever need to keep track of the positions of electron heads and tails. You can implement the whole thing quite efficiently with just two lists, swapping their purpose every time. Mark all electron tails as wire and empty the list. Scan around the electron heads for wire to change it to electron head, note each position you do this in in the first list and change the original electron head to tail. Then swap the two lists. Iirc a WW simulator implemented in C I've seen worked like this. —[[User:Hypftier|Johannes Rössel]] 21:10, 14 July 2010 (UTC)

== Common Lisp Formatting ==
It could have done with some separation between each frame? --[[User:Paddy3118|Paddy3118]] 06:25, 21 December 2009 (UTC)
