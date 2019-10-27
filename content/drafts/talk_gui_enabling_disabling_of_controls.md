+++
title = "Talk:GUI enabling/disabling of controls"
description = ""
date = 2010-08-24T21:21:41Z
aliases = []
[extra]
id = 8067
[taxonomies]
categories = []
tags = []
+++

== A little convoluted ==

I see parts of input validation and control manipulation here, and I suspect they really ought to be slightly more separate, or at least presented in a more common fashion. For example, by keeping the input, increment and decrement controls, and having an "Accept" button that closes the program, but only if the value is within a valid range. The input field should reject values outside its configured range.
: Right. I thought about that, but that would make the task even more convoluted, by adding another rule. --[[User:Abu|Abu]] 18:11, 23 August 2010 (UTC)
:: I was thinking of it as more of a rule replacement; rather than enabling/disabling the input control, leave it enabled full-time, and enable/disable an Accept button. As for the input control's input, validating that control's data could be an optional rule, sometimes called "extra credit". (Catching and validating input on-the-fly is indeed a complication.)
::
:: Really, though, if the real goal is to demonstrate enabling/disabling controls, then a purer example would be to have two buttons, each capable of enabling/disabling the other. GUI input validation probably needs its own separate, purer task. --[[User:Short Circuit|Michael Mol]] 19:23, 23 August 2010 (UTC)

Also, some platforms (Win32 at least) have the potential for message spoofing, which means it's possible for a sufficiently trusted program to cause the appearance of, e.g. a button press, even if the button control is disabled using EnableWindow(FALSE), so the programmer should seriously consider reverifying the input data on the "Accept" press. --[[User:Short Circuit|Michael Mol]] 14:27, 23 August 2010 (UTC)
: That's also correct. "Normal" programs would do that, i.e. calling the same enable/disable function once more when the event appears. Again, this would complicate the example even more. I would not mind adding such rules, but is it worth the price of greater complexity in the task's specification? --[[User:Abu|Abu]] 18:11, 23 August 2010 (UTC)
:: This was more a personal observation on platform peculiarities. It's more footnote material than worth making required behavior in a task like this; many Win32 programmers write code that assumes their message queues are more protected than they really are. --[[User:Short Circuit|Michael Mol]] 19:23, 23 August 2010 (UTC)
::: OK. In fact, in case of the proposed PicoLisp solution (also on Win32 under Cygwin) this is not an issue as the +Button component takes care of that --[[User:Abu|Abu]] 05:26, 24 August 2010 (UTC)
:::: It would still be of note for some other languages. Like I said, thought\, it was more an observation than something requiring special attention. -- [[User:Short Circuit|Michael Mol]] 07:46, 24 August 2010 (UTC)
