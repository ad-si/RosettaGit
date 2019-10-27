+++
title = "Talk:Keyboard input/Obtain a Y or N response"
description = ""
date = 2013-03-11T04:39:49Z
aliases = []
[extra]
id = 8497
[taxonomies]
categories = []
tags = []
+++

== Scope questions ==

This task has some rather specialized hardware and OS interface requirements.  For example, consider what this means for a language implementation which talks to stdin/stdout instead of to device drivers.  Or, consider a language implementation which provides a (server side) web interface.  Or consider a language which is used inside an emacs shell buffer.  Or... all of the above with other cases as well (which is the case I would normally be dealing with).  

Would a solution which includes a specific platform restriction be acceptable from a language which is not normally restricted to that platform?  (For example, assuming a specific operating system so that it can link to device drivers provided by that OS?  Or, for example, assuming a specific web interface mechanism so that it can ship relevant bits of javascript [or vbscript, if the author was so inclined] down to a specific version of a specific browser?)  --[[User:Rdm|Rdm]] 19:30, 15 October 2010 (UTC)

: This is a task that typically requires some kind of terminal. A lot of languages, but not all, work in such environments and have their standard input and output directed to such by default. There are nuances, but they're still portable in principle. As such it's worth having the task. Restrictions on applicability of solutions should be noted, of course; that's normal rules for RC. (I'd expect most solutions to work on either Unix or Windows, but not both.) –[[User:Dkf|Donal Fellows]] 18:44, 18 October 2010 (UTC)
::So we are requiring [[User input/Text|console input]] rather than [[User input/Graphical|graphical input]]? That should be clarified, shouldn't it? I know I can do this in Java for graphical input, but for console input I'm not sure. I haven't been able to find a library that does well with console manipulation like this (though I haven't had enough need to look too hard). --[[User:Mwn3d|Mwn3d]] 14:54, 28 June 2011 (UTC)
:::This task is really to demonstrate operational facilities through the use of a keyboard on computers that have a keyboard attached. A solution using only touchscreen widgets would not fulfil this task, unless these widgets also responded to keypresses from an attached keyboard (ie they operate in parallel with the keyboard). We could always create a separate task for obtaining responses through keyboardless touchscreens, if this is required in future. [[User:Markhobley|Markhobley]] 17:30, 28 June 2011 (UTC)
::::No touchscreens were mentioned. I was asking about console/shell input versus text boxes (like the one I'm typing in now or those in on-screen dialogs). It seems that you will allow graphical input based on that answer as long as the keypresses come from a physical keyboard. Am I correct? --[[User:Mwn3d|Mwn3d]] 17:40, 28 June 2011 (UTC)
:::::Yes. The method of display is not important for this task. [[User:Markhobley|Markhobley]] 18:26, 28 June 2011 (UTC)
:::::If the language has different keyboard input mechanisms depending on whether this is a console keyboard or not, then it would be good to see both methods demonstrated. [[User:Markhobley|Markhobley]] 18:34, 28 June 2011 (UTC)

== common lisp ==

started working on fixing that but don't have time to continue now.

hints to possible solutions can be found here:
http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2009-11/msg00568.html
and here:
http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2009-11/msg00666.html

--[[User:EMBee|eMBee]] 04:39, 11 March 2013 (UTC)
