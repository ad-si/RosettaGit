+++
title = "Talk:Active object"
description = ""
date = 2008-11-05T21:20:51Z
aliases = []
[extra]
id = 3101
[taxonomies]
categories = []
tags = []
+++

I think this task is flawed: as my note at the bottom of the E example shows, its result can be achieved in a simpler and more efficient way by not using any continuing activity at all.

I propose that the task be modified to either:
* discuss only dependence on time, and not "a task that updates the object's state". This would also allow e.g. functional-reactive systems to meet the criteria, which I think would be good.
* be something which, unlike the current one, actually requires such an active process. One way that comes to mind to do this would be to make the integrated input be ''requested by'' the integrator each timeslice (polling), rather than put in by an external event.

After having written them, I like both of these options; but it seems unreasonable to have two slightly different "integrator" tasks. Opinions? --[[User:Kevin Reid|Kevin Reid]] 01:40, 4 November 2008 (UTC)

: Time dependency was not meant as one on clock readings. The state of an active object changes asynchronously to the object's observers. Unfortunately in this example it can be traced down to clock readings, but that was unintentional.

:* A better example could be an object with the task performing some I/O, like querying a database or making 3D animation. But that would make it too complicated.
:* Alternatively we could use a more complex function of time, in order to make it impossible to evaluate accumulated state change over any considerably larger period of time. Actually all numeric simulations are like that. An easy way could be to make the input ''K'' a subprogram, rather than constant. The integrator will integrate ''K''(''t'')d''t''. Would it be better? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:50, 4 November 2008 (UTC)
:: Your second option (subprogram) is the same as my second option, in different terms. Go ahead and change the task; I'll update my example afterward. --[[User:Kevin Reid|Kevin Reid]] 00:35, 5 November 2008 (UTC)
::: OK, I have changed it. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:24, 5 November 2008 (UTC)
:::: I have updated my example to match. Note that the input function is not an explicit function of time; I feel this is appropriate both as good E programming style, and because other input functions might use something other than time; e.g. the current value of some variable, or some external input, etc. --[[User:Kevin Reid|Kevin Reid]] 21:20, 5 November 2008 (UTC)
