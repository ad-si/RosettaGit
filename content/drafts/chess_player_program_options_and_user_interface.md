+++
title = "Chess player/Program options and user interface"
description = ""
date = 2015-05-18T19:21:54Z
aliases = []
[extra]
id = 19143
[taxonomies]
categories = []
tags = []
+++

{{draft task|Program options and user interface}} This is a sub-task of the [[Chess player]] task.

Using previously created [[Chess player/Move generation]] and [[Chess player/Search and evaluation]] components, write a ''driver'' or ''main loop'' which will allow a user to play a game against the computer. This can use either a simple ASCII command-line interface or a more elaborate graphical interface. Implement the basic features given below, and optionally some of the more advanced ones


Basic implementation:
#Allow player to choose colour
#Validate user-entered moves
#Show updated board position after each move
#Detect and show Checkmate and Stalemate

Advanced implementation:
#Time controls
#Detect drawn positions
#Chess engine interface: UCI, CECP or XBoard
#Use an opening book
#Show principal variation during search
#Computer thinking on opponent's time
#Save and replay games in a standard format
#Analyze positions, eg mate-in-N
