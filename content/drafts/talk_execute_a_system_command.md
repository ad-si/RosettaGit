+++
title = "Talk:Execute a system command"
description = ""
date = 2011-08-31T23:44:19Z
aliases = []
[extra]
id = 4660
[taxonomies]
categories = []
tags = []
+++

==Shell command vs arguments array==
I propose that we extend this task to include explicitly executing
# a "shell command", i.e. a string passed to an interpreter such as /bin/sh which parses it
# the command directly, i.e. specifying an array of arguments
(As applicable to the platform, of course; I'm thinking of this from a POSIX perspective.) The reason is that a common source of bugs is to use a "shell command" interface and fail to quote arguments which may have spaces in them, when the simple and reliable solution is to execute the command directly (POSIX exec(2)). It would therefore be good to promote awareness of the difference and knowledge of how to do either one as desired. 

If there is agreement, I will revise the task to discuss this. --[[User:Kevin Reid|Kevin Reid]] 18:00, 6 August 2009 (UTC)

== Visual Basic - does it suck as much as this implies? ==

Okay, leaving aside the inflammatory title (yeah, I hate VB, let's move on). With the example code, waiting for a process is done by spinning; would it be possible instead to use WaitForSingleObject or MsgWaitForMultipleObjects[Ex]? If I were doing it in C++, I would use something like that.

I'm not an expert VB programmer, so is someone else interested in picking up the idea and moving with it?

[[User:Rosuav|Rosuav]] 12:26, 11 January 2010 (UTC)
: You certainly can use the WaitFor* functions (and any of the other NT-supported synchronization objects) and wait on the handle returned by CreateProcess.  The reason is that CreateProcess and WaitFor functions are exported by NT's kernel DLL; You can get access to them the same way you can access any other DLL export.  The code for showing how to do that ''should'' be shown as part of a VB example to [[Call foreign language function]], but it hasn't been added yet.  A walkthrough can be found in [http://support.microsoft.com/kb/106553 KB106553].

: It's worth noting, though, that if you block your VB6 thread with WFSO, you're blocking your message processing thread, and Windows will notice your process has stopped responding (until the child process terminates). The normal resolution would likely be to have a timer trigger a WFSO(hHandle,0) at a reasonable interval. (This assumes that threaded concurrency isn't possible within VB6. If it is, I don't know how.) --[[User:Short Circuit|Michael Mol]] 14:45, 11 January 2010 (UTC)

== Task Extension ==

How about a version of this task in which you process the input in some way? Maybe get a PID from ps or find a file with a certain extension from ls? It should probably be separate from this task since it seems a lot of these just execute the command and the output is automatically directed to stdout. --[[User:Mwn3d|Mwn3d]] 20:57, 23 September 2010 (UTC)

== UNIX Shell Command Substitution ==

Odd that the UNIX Shell examples attribute the "backtick" notation [ `command` ] to the C Shell and the Korn Shell, first since it was the Korn Shell that '''introduced''' the more modern "parenthetical" notation [ $(command) ], and second since the original "backtick" notation was introduced in the Bourne Shell and then only copied by other shells such as the C Shell. --[[User:Balrog|Balrog]] 21:22, 26 February 2011 (UTC)

: Fixed. --[[User:Kernigh|Kernigh]] 23:44, 31 August 2011 (UTC)
