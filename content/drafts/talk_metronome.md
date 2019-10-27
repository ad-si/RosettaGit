+++
title = "Talk:Metronome"
description = ""
date = 2011-09-26T14:38:09Z
aliases = []
[extra]
id = 10463
[taxonomies]
categories = []
tags = []
+++

Thoughts:
* Allow audio-only, visual-only and/or both; some languages can't provide audio outputs, but the principle components of a metronome can still be represented.
* Reword "However, the playing of the sounds should not interfere with the timing of the metronome, so a separation of the playing and timing processes may be required." Just say something simpler such as "the beat indicator should remain true to the selected tempo." Anything more there is either vague, overspecification or unnecessary, IMO.
--[[User:Short Circuit|Michael Mol]] 14:33, 7 September 2011 (UTC)
: Given that metronomes are aimed at helping people when playing music, the timings really don't need to be perfect. A millisecond timer should do fine provided it is coupled properly to an absolute time source so the workload of sound and <s>fury</s> GUI doesn't interfere. (On the other hand, a timer on the scale of a second — usually the most available on the system — is definitely not sufficient.) –[[User:Dkf|Donal Fellows]] 08:11, 11 September 2011 (UTC)
:: What I was trying to get at with the point about timings is that you can't allow the work involved in display or audio updates to cause the timer to drift. That seemed the obvious intent of the "separate processes" wording. Certainly, the timings don't need to be perfect; &lt;2ms jitter is probably unnoticeable, but that amount of jitter should hold true comparing events 600 seconds apart as well as 250ms apart. --[[User:Short Circuit|Michael Mol]] 18:14, 11 September 2011 (UTC)

==Notes on the Tcl version==
It only plays the bell on the main beat of the bar, because anything more often and my ''terminal'' lags in its sound playing. The metronome itself keeps time though. Using the system bell, while annoying, had the advantage of not requiring me to find a pair of sound files to play and also keeps the code clear of external dependencies. (I probably ought to write a more fancy version sometime.) –[[User:Dkf|Donal Fellows]] 14:38, 26 September 2011 (UTC)
