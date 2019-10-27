+++
title = "Talk:Rosetta Code/Run examples"
description = ""
date = 2015-03-24T20:47:01Z
aliases = []
[extra]
id = 10927
[taxonomies]
categories = []
tags = []
+++

== Output extra credit ==

Since it's just extra credit I don't think this should keep it from being a full task, but I'm concerned about the output checking EC. It might just end up being too problematic because of the different ways languages show certain things. The first example I thought of was lists. Here's how the same list might look in different languages:

Scheme/LISP(s):
 (1 2 3 4 5)

Java:
 [1, 2, 3, 4, 5]

J:
 1 2 3 4 5

Prolog:
 [1,2,3,4,5]
All of those are the same list, but they look pretty different. I'm also a little worried about someone taking a solution that does this output checking and using it to mark examples incorrect, but that doesn't seem very likely at all so I'm not that worried--only a little bit. Like I said, it shouldn't keep this from becoming a full task, but people should make sure they think ahead a lot if they try to do this EC. --[[User:Mwn3d|Mwn3d]] 17:51, 22 November 2011 (UTC)
: yes, that is very true, and the more reason why this is extra credit.--[[User:EMBee|eMBee]] 18:02, 22 November 2011 (UTC)

==Some points==
I do have some points for consideration:
# Layout of examples: examples have not been created to be auto-run and massaging a wide selection of examples to run could make for too long a program. You might consider something like ''"Assume that code inside the first <nowiki><lang></nowiki> tag below the languages  <nowiki>{{header|}}</nowiki> tag is all that need run"''.
#: that sounds like a good idea. in my mind i expected each <nowiki><lang></nowiki> tag to be a separate solution, so the downloading could would have to pick one or run all of them on by one.--[[User:EMBee|eMBee]] 18:51, 22 November 2011 (UTC)
#:: That's not universally true. Sometimes answers are structured as two parts: the core of the solution, and a driver/testbed that applies the solution to a particular case. Other languages sometimes have multiple <nowiki><lang></nowiki> sections, one per file needed to create the solution (I've seen this quite a bit with the Ada solutions). –[[User:Dkf|Donal Fellows]] 07:59, 16 April 2012 (UTC)
# Would this lead to high server load when developing and testing a solution - especially for the extra credit part of the task?
#: Running every example's going to be impractical anyway. Some require extra command line arguments. Some require user interaction. Running a single example for a single language would be enough (where they are nominated at runtime; no hard-coding!) and wouldn't cause problems with excessive server load. –[[User:Dkf|Donal Fellows]] 15:38, 23 November 2011 (UTC)
# You might want to just have language A download and run examples from language A. Rosetta Code normally allows a task to be fulfilled in one language without necessarily knowing another to any degree, (if at all).
--[[User:Paddy3118|Paddy3118]] 18:36, 22 November 2011 (UTC)
: well, the knowledge needed for some of the languages is minimal, eg:  to run a [[Python]] solution, only <code>python task.py</code> is needed. surely anyone can implement support for that without knowing python. the main point here is to write the program in a way so that support for additional languages can be added easily because the framework is already there. and once more solutions appear, taking them from one language and porting them to another should be easy.--[[User:EMBee|eMBee]] 18:51, 22 November 2011 (UTC)

== Overly ambitious ==

The task currently says: "The program should verify that the tools needed to compile or run the solution are present before running it."  But this is not possible, in the general case.  For example, one task may need opengl installed, with the proper drivers for the hardware (and some platforms may have multiple sets of drivers installed only some of which are useful with the installed hardware), while another needs the user to be running a browser and yet another may require that the user be running a tty window and another may require a certain version of a certain library and another may require a certain virtual machine to be installed and configured properly... and these requirements will change both from task to task but also from implementation language to implementation language.  --[[User:Rdm|Rdm]] 19:33, 5 December 2011 (UTC)
: quite right. good catch. when i wrote this i was only thinking of the executable needed to run the code or the compiler. that is, before the program attempts to run any command it should check if the command is available, or catch the error in some way with a message to the user. but come to think of it, it should probably catch any error and present it to the user, so the error can be fixed.--[[User:EMBee|eMBee]] 13:47, 8 December 2011 (UTC)
:: If you want to do something like this, you should require sandboxing, or at least remind users of risks.  What if I snuck in stuff to the effect of "rm -rf /" right before some hapless one blindly pulls the code and runs it?  It's an open wiki, and should by no means be considered trusted. --[[User:Ledrug|Ledrug]] 14:03, 8 December 2011 (UTC)
::: also true. this should come with sufficient warnings. and the more reason to present the source to the user and wait for confirmation before running, as the task states.--[[User:EMBee|eMBee]] 16:08, 8 December 2011 (UTC)

== Self reference ==

If this task runs itself, where will it stop? [[User:André van Delft|André van Delft]] 00:30, 15 December 2011 (UTC)

- It asks for user input, so if the user does not input it stops.--[[User:Zorro1024|Zorro1024]] ([[User talk:Zorro1024|talk]]) 20:46, 24 March 2015 (UTC)
