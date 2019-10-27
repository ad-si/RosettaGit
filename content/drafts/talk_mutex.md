+++
title = "Talk:Mutex"
description = ""
date = 2011-04-01T12:15:01Z
aliases = []
[extra]
id = 3100
[taxonomies]
categories = []
tags = []
+++

I added some C/C++ code for the Win32 API.  My reasoning is that--parallelization algorithms aside--how to access synchronization objects in a language is becoming almost as important as that language's conditional and loop processing facilities.  I think code should be offered for other languages (and APIs...I don't know how to do it in pthreads or C++0x) as well.--[[User:Short Circuit|Short Circuit]] 04:57, 3 November 2008 (UTC)
: With Win32 API CRITICAL_SECTION objects are often used as local mutexes, non-reentrant, if I correctly remember. They are less heavy weighted than mutex objects, which can serve as global mutexes. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:25, 3 November 2008 (UTC)
:: I know of CRITICAL_SECTIONs and of some of their tradeoffs, but I haven't used them enough to write code from memory. I also don't have a Win32 programming environment readily available at home, and I'm prohibited from working on Rosetta Code on company hardware.  If someone wanted to add setup, lock, unlock and destruction code for CRITICAL_SECTIONs, that would be entirely appropriate, though. --[[User:Short Circuit|Short Circuit]] 03:58, 4 November 2008 (UTC)
:::If anybody is interested to add critical sections, the API is
:::* InitializeCriticalSection creates the mutex,
:::* DeleteCriticalSection destroys it,
:::* EnterCriticalSection acquires it,
:::* LeaveCriticalSection releases it.
:::Source: [http://msdn.microsoft.com/en-us/library/ms682530(VS.85).aspx MSN] --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:59, 4 November 2008 (UTC)

==Is this a Task?==
Then it should follow the format of the other tasks (headings). If it is an encyclopaedic entry then it should loose the task box? --[[User:Paddy3118|Paddy3118]] 05:57, 14 July 2009 (UTC)

: It appears to be both a task and an encycloypædic entry. (Wiki pages can have multiple inheritance…) —[[User:Dkf|Donal Fellows]] 07:45, 14 July 2009 (UTC)

:: If it is a task, it should contain a clear description what the examples should do/show. --[[User:Ce|Ce]] 10:46, 18 October 2009 (UTC)

==Task Format==
The normal format for a task is to not have headings within the task description, before the langiage examples. I thought this was to keep a consistant look to the page and the table of contents.

This seems to be the only(?) task were this is not the case. Should it be changed to comply? --[[User:Paddy3118|Paddy3118]] 06:40, 1 April 2011 (UTC)
: When I originally wrote it, it was supposed to be an encyclopedic entry. Later, people added examples, and it became more task-like. I would really, really like to preserve its encyclopedic nature. Perhaps those sections can be made into subpages. For example, [[Mutex/Global]], [[Mutex/Local]], [[Mutex/Reentrant]] (which would spec coding reentrant vs non-reentrant examples), and a "see also" pointing to [[Dining philosophers]]. --[[User:Short Circuit|Michael Mol]] 12:15, 1 April 2011 (UTC)
