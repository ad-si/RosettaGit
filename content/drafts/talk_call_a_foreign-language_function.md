+++
title = "Talk:Call a foreign-language function"
description = ""
date = 2016-11-11T20:56:50Z
aliases = []
[extra]
id = 4764
[taxonomies]
categories = []
tags = []
+++

I've modified the task slightly (making it a bit less strict) as part of merging in the content that was in [[C FFI]], as that was the same task by a different, weaker name and with slightly variant non-important conditions. --[[User:Dkf|Donal Fellows]] 20:36, 17 August 2009 (UTC)

== strdup is part of POSIX, not standard C. ==

Not all standard-compliant C89 implementations will have strdup available. As long as we're using C as a universal intermediate, we should stick to something that's going to be present in all C89-compliant versions. Suggested fixes for the task? --[[User:Short Circuit|Michael Mol]] 20:08, 5 October 2010 (UTC)

:What qualities do we want the function to have?  rand() would be simple to use, but would not demonstrate argument passing.  catan2() would demonstrate argument passing, but would not deal character data.  qsort() would require a callback.  Etc... --[[User:Rdm|Rdm]] 20:19, 5 October 2010 (UTC)

== Fortran ==

I removed the Fortran program, which was a copy of the program found in the task ''[[Call a function in a shared library]]'' (which is, by the way, quite large for RC, with 8537 bytes). This is a different task. I added an example showing the use of the ISO_C_BINDING standard module.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 20:54, 11 November 2016 (UTC)
