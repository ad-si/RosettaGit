+++
title = "Talk:Date manipulation"
description = ""
date = 2016-10-05T04:58:42Z
aliases = []
[extra]
id = 4159
[taxonomies]
categories = []
tags = []
+++

==Task is not atomic==
This task mixes two entirely different things: date format, date computation.
Is that intended?
(This issue is touched further down.)
[[User:Marius|Marius]] 09:34, 13 February 2012 (UTC)

Actually, this task is mislabeled: it is called "Date manipulation", but should be called "DateTime manipulation", or perhaps "Time manipulation", especially since the task says to provide an offset in _hours_, components that exist only in a DateTime or Time value.

==Task is Region specific==
I live in the UK and tried the Tcl example:

```tcl
loading history file ... 29 events added
buffer line limit: 512   max line length: unlimited
Main console display active (Tcl8.5.7 / Tk8.5.7)
(HP DV8025EA) 30 % set date "March 7 2009 7:30pm EST"
March 7 2009 7:30pm EST
(HP DV8025EA) 31 % set epoch [clock scan $date -format "%B %d %Y %I:%M%p %z"]
1236472200
(HP DV8025EA) 32 % set later [clock add $epoch 12 hours]
1236515400
(HP DV8025EA) 33 % puts [clock format $later]
Sun Mar 08 12:30:00 GMT 2009
(HP DV8025EA) 34 % 
```

As you can see, this is different from the answer given in the article. 

How about making the task work for any region? --[[User:Paddy3118|Paddy3118]] 03:15, 14 May 2009 (UTC)
: Better yet, use Date or Time objects. --IanO

: The task itself does not specify what the answer should be -- only the answers may/will be region specific.  The result of the Tcl solution merely happens to show that I live in the Eastern time zone. --[[User:Glennj|glennj]] 10:36, 14 May 2009 (UTC)

:: You can always use the <tt>-timezone</tt> option to print in another format.
::
```tcl
% set date "March 7 2009 7:30pm EST"
March 7 2009 7:30pm EST
% set epoch [clock scan $date -format "%B %d %Y %I:%M%p %z"]
1236472200
% set later [clock add $epoch 12 hours]
1236515400
% clock format $later
Sun Mar 08 12:30:00 GMT 2009
% clock format $later -timezone America/New_York
Sun Mar 08 08:30:00 EDT 2009
% clock format $epoch
Sun Mar 08 00:30:00 GMT 2009
```

::The last one is just to check that it really is doing the time math right. Forgot that there was a DST change at that point in time... —[[User:Dkf|Dkf]] 10:58, 14 May 2009 (UTC)

::: Indeed I've tried to implement something not region specific... but coping with timezones and DST [http://www.timeanddate.com/time/dst2009.html is a mess] if not "directly" supported by a library... (currently I believe POSIX is not too much interested in doing conversions ''to'' any timezones; the results are driven by a environment variable; maybe one can setenv it properly... anyway EST alone gives not the required information directly...) --[[User:ShinTakezou|ShinTakezou]] 11:07, 14 May 2009 (UTC)

:::: Date and time parsing and formatting really ''are'' inherently region-specific. Don't kid yourself otherwise. And feel free to show off the library in conjunction with the language; that'd make for the most valuable kind of contribution to this site. —[[User:Dkf|Dkf]] 12:11, 14 May 2009 (UTC)


Maybe the task should be changed so the example time given is in terms of a base string plus some indication of your local timezone, such as that given, for EST. If the fact that the calculation should include a change in daylight saving time, then that should be added to the task description too. --[[User:Paddy3118|Paddy3118]] 12:42, 14 May 2009 (UTC)

: I would like to modify the task so you can initialize the date in the most natural way for the language, not via timestamp string parsing.  (Date parsing is already presented in some of the data munging tasks.)  For example, Java would use a Date or Calendar constructor. Hopefully, that would eliminate the timezone confusion and make the task more language inclusive. --[[User:IanOsgood|IanOsgood]] 14:51, 26 June 2009 (UTC)
::I don't understand why there's confusion. It doesn't say to print in any particular timezone and receiving a date stamp with a timezone is fairly common. The important part of the task is that the time is 12 hours later no matter what timezone it is. --[[User:Mwn3d|Mwn3d]] 18:35, 26 June 2009 (UTC)
:::The confusion is in the task it self, that mixes two entirely different things, formatting and computation.[[User:Marius|Marius]] 09:34, 13 February 2012 (UTC)

== Rexx solution ==

should be moved to category ooRexx! May I do that? --[[User:Walterpachl|Walterpachl]] 07:50, 4 August 2012 (UTC)

:Hmmm, there is already an ooRexx solution in that Category.
:Who are the authors?
:And what's the point of isTrue and isFalse??
:I suggest these changes:

```txt

 DEL-> -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 DEL-> isTrue: Procedure; Return (1 == 1)
 DEL->
 DEL-> -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 DEL-> isFalse: Procedure; Return \isTrue()
 DEL->
 DEL-> -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 OLD->   !reading  = isFalse()
 NEW->   !reading  = 0

 OLD->         !reading = isFalse()
 NEW->         !reading = 0

 OLD->         !reading = isTrue()
 NEW->         !reading = 1

```

--[[User:Walterpachl|Walterpachl]] 07:53, 5 August 2012 (UTC)
