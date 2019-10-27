+++
title = "Talk:Hello world/Standard error"
description = ""
date = 2014-07-13T18:06:59Z
aliases = []
[extra]
id = 17766
[taxonomies]
categories = []
tags = []
+++

== REXX stderr vs. stdout ==

I tried to but couldn't convince the author of Rexx versions 1 to 3 that these, in my view, do not address the task at hand. The two Rexx implementations that I know (ooRexx and Regina) allow to send error messages to the stderr stream which is also used for Trace output and error messages. I do not know of any other Rexx that would support this feature that allows you to redirect output to separate files for the two streams stdout (used by Say) and stderr. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:57, 11 July 2014 (UTC)
: versions 2 and 3 were changed now. thanks. An interesting observation: Regina has an option 

```rexx
options stdout_for_stderr
```
. This directs output that normally would go to stderr (i.e. Trace output and error messages) to stdout. Call lineout 'stderr','some text' is NOT redirected. ooRexx has a feature to direct all stdout/stderr output to a file with these instructions within the program:

```rexx
-- now one can redirect from within ooRexx !
allout=.stream~new("all.txt")~~open("write replace")
.output~destination(allout)   -- redirect ooRexx standard output to new stream
.error~destination(allout)    -- redirect ooRexx standard error output to new stream
```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:46, 12 July 2014 (UTC)
