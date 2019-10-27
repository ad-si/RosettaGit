+++
title = "Talk:Determine if only one instance is running"
description = ""
date = 2011-04-04T20:47:15Z
aliases = []
[extra]
id = 3126
[taxonomies]
categories = []
tags = []
+++

Except for possibly a few languages, this task is highly operating system dependent.
It's probably not a good task for rosettacode.--[[User:64.238.49.65|64.238.49.65]] 20:30, 19 November 2008 (UTC)
: It's a very common problem, and platform-specific code isn't inappropriate, as long as the platform is identified in the example. --[[User:Short Circuit|Short Circuit]] 02:05, 21 November 2008 (UTC)

== C Solution ==

I wonder why the file system method was considered a better solution. I liked the older code that used semaphores.

[[User:Markhobley|Markhobley]] 06:56, 3 April 2011 (UTC)

: I run [[OpenBSD]]. This system has [http://www.openbsd.org/cgi-bin/man.cgi?query=sem_open&apropos=0&sektion=3&manpath=OpenBSD+Current&arch=i386&format=html a very broken ''sem_open()''] which always fails with ENOSYS. I changed this program from a named semaphore to a regular file, so that the program would work with my system. Now I can add a SIGINT handler.

: I did read some ''sem_open()'' manual pages. I believed that a name of a named semaphore must start with "/"; but the program had "MyUniqueName", which starts not with "/". I also believed that ''sem_open()'' fails by returning SEM_FAILED; but the program checked NULL, not SEM_FAILED.

: I noticed that the program never used the semaphore as a semaphore. A program can do the same thing with shared memory (''shm_open()'' and ''shm_unlink()'') or with a regular file (''open()'' and ''unlink()''). So the solution with a regular file is as good as the solution with a semaphore. --[[User:Kernigh|Kernigh]] 02:43, 4 April 2011 (UTC)

== VB solution ==

It looks like the VB solution only checks for previously run instances of the application. What about instances started after this one? Should this be a problem and should we choose to keep this task, it should be corrected. --[[User:Mwn3d|Mwn3d]] 21:36, 19 November 2008 (UTC)
: The common case requiring detection of multiple instances are running is to prevent multiple instances of a program from accessing app-global resources.  That's probably what the author of the VB solution had in mind.  The task author should probably clarify the task's intent and requirements. --[[User:Short Circuit|Short Circuit]] 02:05, 21 November 2008 (UTC)

The purpose is as you say for the detection of multiple instances for the purpose of preventing conflicting access to global resources. I would expect the first instance of the task to not detect another instance, whereas subsequent instances (duplicate instances of the same task) detect a previous instance already running and abort with an error message.

[[User:Markhobley|Markhobley]] 20:47, 4 April 2011 (UTC)
