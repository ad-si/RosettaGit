+++
title = "Talk:Handle a signal"
description = ""
date = 2017-04-30T22:20:27Z
aliases = []
[extra]
id = 4040
[taxonomies]
categories = []
tags = []
+++

== Error in C example ==
The C example uses <code>printf</code> in the signal handler. However, printf isn't signal-safe, that is, if the signal happens during the printf call in the main program, it might fail. The correct way would be to set a flag of type <code>volatile sig_atomic_t</code> in the signal handler, and test that flag in the main program. Note that usleep returns when interrupted by a signal, so handling of that signal will not be delayed that way. --[[User:Ce|Ce]] 08:38, 31 March 2009 (UTC)==

== Many poor examples ==
Many of these examples demonstrate a very poor algorithm for the case of user initiated signals ( INT, QUIT, INFO...),
Specifically those that set a flag within the signal handler and then exiting the signal handler (taking no other action).

If the code within the looping construct gets stuck in a (nearly) infinite loop,
the test of the flag will not be performed.

Hopefully someone with a knowledge of the language syntax will revise those examples.
Sincerely, 
[[User:Dgerman|Dgerman]] ([[User talk:Dgerman|talk]]) 22:20, 30 April 2017 (UTC)
