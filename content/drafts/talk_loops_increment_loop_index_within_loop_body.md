+++
title = "Talk:Loops/Increment loop index within loop body"
description = ""
date = 2019-03-22T07:55:09Z
aliases = []
[extra]
id = 22229
[taxonomies]
categories = []
tags = []
+++

== increments the index such that the new index is now that prime==
I must start with the index i 42.

I must increment i until it is prime, so 43.
I must now increment i such that the new i is now 43?

I interpreted the sentence ' increments the index such that the new index is now that prime ' to mean you wanted me to set i to 43+43 but this is not what the task specification says.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:41, 17 March 2019 (UTC)



-----



Yes, I see your point.   How about: 
════════════════════════════════════════════════════════════════════════════════
;Task:
Write a loop which:
::*   starts the index (variable) at   '''42'''
::*   (at iteration time)   increments the index by unity
::*   if the index is prime:
::::*   displays the count of primes found (so far) and the prime   (to the terminal)
::::*   increments the index such that the new index is now the (old) index plus that prime
::*   terminates the loop when   '''42'''   primes are shown
════════════════════════════════════════════════════════════════════════════════

Would this be satisfactory?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:29, 19 March 2019 (UTC)

:Works for me.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:25, 21 March 2019 (UTC)

I had (originally) had wording in the task's requirements to make it clear(er)   (ha!, not so much)   that the index was to be (possibly ''also'') incremented   (by the computer program)   by the prime just found, but if done/interpreted another way and the index was incremented by the another "extra"   ''+1'',   (by   '''do'''   loop structure mechanism)   the new index   (a ''prime+prime'')   is never a prime, and if the index was incremented   (''+1'')   by the   '''do'''   loop structure mechanism,   no harm was done   (that is, at the worst, an extra check for primality was performed for the new index   ''prime+prime''   instead of   ''prime+prime+1''.   So the extra wording, as it turns out, wasn't necessary, but I wanted the incrementation to be clear.   So this almost (did?) became a   "Who's on first?"   sort of a word mess.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:29, 19 March 2019 (UTC)

:Well, I suppose the advantage of this description is that it indicates why you should not do this!, +1 for those imperative languages that prevent it--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:25, 21 March 2019 (UTC)

:: The task's requirements has been updated (to the above).      -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:54, 22 March 2019 (UTC)
