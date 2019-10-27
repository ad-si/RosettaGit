+++
title = "Talk:Fork"
description = ""
date = 2018-03-14T19:58:37Z
aliases = []
[extra]
id = 1997
[taxonomies]
categories = []
tags = []
+++

This appears unclear to me -- is the forked process supposed to be the same as the forking process or just any odd old process? How does this differ from [[Simple_concurrent_actions]]? At what level is the fork supposed to happen (does it have to have a different OS-level pid or can it run as a parallel script on the same interpreter, for example). I guess my biggest conceptual trouble is understanding what that process is supposed to <i>do</i>: "Fork a process" is like "build a machine" - without any spec as to the machines <i>purpose</i>, I wouldn't know what to do. On some level, every GUI program on modern hardware consists of many parallel processes already (mouse and kbd interactions, video display, file i/o, all possibly at the same time) so I'm guessing the new process should do something beyond merely "existing" - but what?[[User:Sgeier|Sgeier]] 20:04, 14 March 2007 (EDT)
*Simple Concurrent Actions is a bit broader than Fork Process because it includes the possibility of using threads for the concurrent behavior, not just processes.[[User:Waldorf|Waldorf]] 18:56, 14 March 2007 (MDT)

:In my opinion, this task definition is ambiguous.  Traditionally (C/Unix), a forked process inherits its context from the parent process.  However, the task uses this term "fork" without actually specifying anything about what should or should not be inherited.  This means that calling out to the OS to run a new process could satisfy this task -- but if this were truly the case the task should be re-named.  Alternatively, the task should be rewritten, to specify what is meant by "fork" and the name should be kept.  --[[User:Rdm|Rdm]] 04:30, 8 February 2010 (UTC)
::Agreed. And there is already [[Execute a system command]], so IMO this task should be limited to a true "UNIX fork". [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 19:58, 14 March 2018 (UTC)

== Clojure and Haskell examples ==

I marked the Clojure and Haskell examples as incorrect. forkIO creates a new thread, not a process. A clojure agent runs on one of the threads in a thread pool, within the same process. -- [[User:Wmeyer|Wmeyer]] 08:58, 17 January 2010 (UTC)
