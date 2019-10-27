+++
title = "Talk:Readline interface"
description = ""
date = 2019-03-27T23:37:15Z
aliases = []
[extra]
id = 11026
[taxonomies]
categories = []
tags = []
+++

== What is meant by "history"? ==

Is history simply not removing what is in the console when a new command is run? --[[User:Crazyfirex|Crazyfirex]] 21:26, 8 December 2011 (UTC)
: No. History is pressing the up arrow to retrieve a previously-typed command (one of a sequence of them in fact) that you can then run again, or edit before submitting. It ''greatly'' improves the productivity and usability of a command-line interface. –[[User:Dkf|Donal Fellows]] 03:26, 9 December 2011 (UTC)

:: ''History'' shouldn't be limited to the ''up arrow'' key.  On earlier VM/CMS systems, system programmers (almost everywhere) added a   RYO   [<u>R</u>oll <u>Y</u>our <u>O</u>wn]     (by way of a modification to the VM control program [CP]) implementation [of the latter-named RETRIEVE function]  to the  '''SysReq''' key  (so as to not preclude taking over any "useful" key --- there weren't any ''arrow'' keys at that time.  But the '''SysReq''' key was later purloined by VTAM, but by that time, IBM VM/CMS system had implemented the RETRIEVE function to any PFK (program function key) that the user choose, and it was later assigned by some programs to PF6/PF18 such as XEDIT and others, so most users choose that key which made it easier to remember which key to use for retrieving commands. Nowadays, it seems almost silly to even discuss the usefulness of it, but at one time, there was no such animal. Even much much later, DOS seemed to add it as an afterthought. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:32, 1 June 2012 (UTC)

==a simple program ...==
I think anyone attempting this task will soon find that's it not simple to provide a "history" suitable for re-doing commands. Witness Microsoft's DOS with its ''up arrow'' (retrieve/re-do) function along with all the F1 --> F12 functions, the latter being too complicated to use and hard to remember which function key does what (I still ponder, what where those coders thinking?).-- [[User:Gerard Schildberger|Gerard Schildberger]] 17:12, 1 June 2012 (UTC)
:i don't get what you mean. what exactly is the issue with the up arrow? it works fine in about any shell or readline interface i know.--[[User:EMBee|eMBee]] 13:30, 18 June 2012 (UTC)

:: I have no issue with the UP arrow.  I never said or implied that I did.  If you can define what half of the function keys that make up the command history :"support structure", I'd be amazed. I stated that it's a complicated set of commands, of which almost everybody only used the UP arrow key, and hardly anybody even bothers to configure it (at least, that's my experience when observing other people's computers). 
::: i plead guilty as charged :-)~~----
:: I also never implied that it doesn't work fine.  I said it was complicated and hard to remember which function key does what.
::: you are right, i did actually miss that you wrote: "''the '''latter''' being too complicated''", sorry.
:::but you do say that providing a history that is suitable for redoing commands is not simple. and then you cite dos to substantiate that. if we assume that the up arrow key is suitable then why not just ignore the other stuff which most people don't use anyways. wouldn't that make it simpler? (now actually implementing the uparrow support is likely still not trivial but i'd consider it the simplest version that would actually feel like what i'd expect from a readline interface: working arrow keys.
:::however this task is still young, and i'd wait to see more opinions from others. as i said before, i expected this task to focus on existing realine implementations, anything beyond that, like your work, i consider a bonus, and i'd be happy to leave it as it is. thanks for the contribution.--[[User:EMBee|eMBee]] 03:32, 20 June 2012 (UTC)
 
::  It still is surprising when you look at the properties of the "prompt" window, that the UP arrow is never mentioned, just the command history, buffer size, # of buffers (what the heck is a buffer, why not call it a command or some such?), and a check-box for discarding duplicates  (duplicate commands that are sequential, or entered at anytime?).  This is in reference to Windows. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:23, 19 June 2012 (UTC)

I was going to add such functionality to the REXX example, but the code would've been far from simple. 
:well, i can see that writing the code to actually implement all that functionality may not be trivial. but this task was mostly intended to get examples of using already existing libraries or modules which should not be hard.--[[User:EMBee|eMBee]] 13:30, 18 June 2012 (UTC)
So I elected to just have the option for the user to list the history of all commands entered, and let the user pick which one (by number), or just choose the last command to re-execute.  I couldn't added more support to just limit the last ''n'' entries (for the command history), and/or let the user choose how many commands to list.  But again, that would've added more complexity to an already burgeoning program, and the goal of the task was to implement a '''simple''' (ha!, good luck with that!) program, and in that, I fear, I may have failed.
:simple was referring to what the program does besides having a readline interface. consider the C example, which only supports one command: help. that's simple. if you have to implement the whole readline functionality yourself because the language doesn't already provide an implementation, then that's of course not simple. and it wasn't expected to be. --[[User:EMBee|eMBee]] 13:30, 18 June 2012 (UTC)
I hope I showed how to add commands, handle aliases, and full abbreviations (modeled after IBM VM/CMS version of what abbreviations are).  Of course, one man's simple program is another's monstrosity. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:12, 1 June 2012 (UTC)
