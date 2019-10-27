+++
title = "Talk:Approximate Equality"
description = ""
date = 2019-09-03T15:40:04Z
aliases = []
[extra]
id = 22503
[taxonomies]
categories = []
tags = []
+++

==Can of worms==
uh-oh, you've opened a can of worms here. I can remember in the far distant past, being lectured to about floating point/equality/closeness and I resolved to run and hide if I could in the future :-)

Firstly, you can't leave people to find out how Python does it, if you want them to use a specific method then you need to add the description to the task.

Second. Approximation depends on circumstances. If the compared vary exponentially ar non-linearly, or ... then one may end up with different, but more usable definitions of approximately equal.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:08, 2 September 2019 (UTC)
:Agreed. You need to be very precise about your imprecision. Admittedly my gut instinct was that 100.01 and 100.011 are not approximately equal, like you said, but in fact they are better than 99.999% approximately equal and less than 0.001% different! I just wrote down "what I usually do" and on reflection that is not really likely to meet any task requirements very well. Perhaps explicitly specifying the accuracy (as a fraction, percentage, number of significant digits, decimal places, or whatever) with all the explicitly required answers for each of the various precision settings might help. Also, the test cases should probably be numbered rather than bullet pointed, if you're going to refer to them by number. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 01:25, 3 September 2019 (UTC)

:: I've just done the update to the task's preamble (as far as numbers instead of bullets).   However, some programming language entries have added some of their (or other's) pairs of numbers to be compared, so their   ''outputs''   don't match the examples given in the task's preamble.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:41, 3 September 2019 (UTC)

The desired task use case is to have a method that allows tests on floating point calculations to be tested against a non-integer decimal constant, to verify correctness of the calculation, even when code changes change the floating point result in its non-significant portion.<br /><br /> Beyond that, the "can of worms" probably surrounds a) whether there should be an absolute difference that matters, versus just a relative difference, and b) whether 0.0 is different from all other floating point, because only 1/0.0 is NaN. Those "wormy" issues should not matter here.--[[User:Wherrera|Wherrera]] ([[User talk:Wherrera|talk]]) 02:25, 3 September 2019 (UTC)

==Clarify the task==
It is not clear to me if the task is asking for a function which compares two floating point values to be within a given tolerance or looking at languages implementation of floating point values. If the latter maybe [https://0.30000000000000004.com/ does 0.1+0.2=0.3] might be a useful ref.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:40, 3 September 2019 (UTC)
