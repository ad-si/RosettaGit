+++
title = "Talk:Averages/Mean time of day"
description = ""
date = 2013-08-28T13:05:15Z
aliases = []
[extra]
id = 12012
[taxonomies]
categories = []
tags = []
+++

It seems redundant to have a task that is exactly the same as [[Averages/Mean angle]] except that 360 degrees is replaced with 24 hours. --[[User:Spoon!|Spoon!]] 08:57, 12 July 2012 (UTC)
: A bit.  I agree, except I suspect most of the code for this task will involve going to and from the printable time format.  I added it to the "Date and time" category.  &mdash;[[User:Sonia|Sonia]] 17:39, 21 September 2012 (UTC)
:You have to factor in minutes and hours here, so the task may not be completely redundant. [[User:Markhobley|Markhobley]] 19:32, 11 February 2013 (UTC)
I think this is ready to promote to task. [[User:Markhobley|Markhobley]] 19:32, 11 February 2013 (UTC)

==TCL and rounding==
TCL has a different output than others, of 23:47:44 instead of  23:47:43. It seems to be somewhere in the calculation of the mean time rather than in the split into HH:MM:SS as I tried the TCL way in my python - inserting the two lines before the return statement below and got 43 seconds and not 44 again.

```python
def mean_time(times):
    t = (time.split(':') for time in times)
    seconds = ((float(s) + int(m) * 60 + int(h) * 3600) 
               for h, m, s in t)
    day = 24 * 60 * 60
    to_angles = [s * 360. / day for s in seconds]
    mean_as_angle = mean_angle(to_angles)
    mean_seconds = mean_as_angle * day / 360.
    if mean_seconds < 0:
        mean_seconds += day
    h, m = divmod(mean_seconds, 3600)
    m, s = divmod(m, 60)
    a = mean_seconds
    print("%02d:%02d:%02d" % (a / 60 / 60 % 24, a / 60 % 60, a % 60))
    return '%02i:%02i:%02i' % (h, m, s)
```

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:57, 2 July 2013 (UTC)
: Was a rounding issue; <tt>int()</tt> rounds to zero whereas <tt>round()</tt> rounds to nearest. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 13:05, 28 August 2013 (UTC)

I'm not convinced that whole methodology is correct in the first place. For example, when I try an alternative mechanism for time averaging (with times being either “pre” or “post” midnight so as to minimise the deltas):

```tcl
% set t [clock scan 23:00:17 -base 0]
79217
% incr t [clock scan 23:40:20 -base 0]
160837
% incr t [clock add [clock scan 00:12:45 -base 0] 1 day]
244402
% incr t [clock add [clock scan 00:17:19 -base 0] 1 day]
328241
% expr $t/4
82060
% clock format [expr $t/4] -format %H:%M:%S
23:47:40
```

As you can see, I get a different answer (several seconds out) and that's using exact arithmetic. (You might get different intermediate values — they're local-timezone-dependant without the <tt>-gmt true</tt> option — but the final formatted result should be the same.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 12:59, 28 August 2013 (UTC)
