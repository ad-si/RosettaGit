+++
title = "Talk:Terminal control/Preserve screen"
description = ""
date = 2012-03-06T13:00:29Z
aliases = []
[extra]
id = 9381
[taxonomies]
categories = []
tags = []
+++

Does "screen" include character decorations, fonts, kerning, and so on, or not?

These terminal control tasks have long bothered me, because of the heavy ambiguity in their descriptions, and when we do not even have a working example my imagination runs wild.  --[[User:Rdm|Rdm]] 14:53, 21 March 2011 (UTC)

:There is no requirement to change the font or kerning in this task, however character decorations and attributes are expected to be preserved. If the task implementer decides to change the font or kerning during the display of the temporary screen, then these settings need to be restored prior to exit.

:Working examples of applications that preserve the screen are the unix less command and sidekick for msdos.

:[[User:Markhobley|Markhobley]] 19:23, 21 March 2011 (UTC)

:: So the Javascript code (which preserves the browser state, not a terminal) is way off? It's rare for Javascript engines to be connected to a real terminal in any useful way. –[[User:Dkf|Donal Fellows]] 22:03, 2 May 2011 (UTC)

I don't think it is way off. We could just add a note against that language that Javascript typically only runs only in the browser provided terminal emulation.
	
[[User:Markhobley|Markhobley]] 23:17, 2 May 2011 (UTC)
	
== The ''Unix'' solution ==

The Tcl solution is really a solution for Unix. On that platform, either use the curses library (or one of its successors) or run the program “tput smcup” to save the screen (the documentation says “enable cursor positioning mode” but that's less than helpful) and run “tput rmcup” to restore the old one. And you wouldn't believe how ''awkward'' it was to search for the right termcap code; huge amounts of irrelevant noise from people talking about how to control xterm and how to use GNU screen. Use the knowledge I have gathered for good, not evil! –[[User:Dkf|Donal Fellows]] 23:14, 2 May 2011 (UTC)

== Python example ==

I've added a Python sample translated from C. A more interesting example would be based off the curses module or such, but this works.
If someone wants to re-write/add more Python, please do, but it's another task knocked off the Python todo list. [[User:AbstractBeliefs|AbstractBeliefs]] 13:00, 6 March 2012 (UTC)
