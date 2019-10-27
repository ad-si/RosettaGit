+++
title = "Talk:Bitmap/Bresenham's line algorithm"
description = ""
date = 2012-03-21T19:38:59Z
aliases = []
[extra]
id = 3847
[taxonomies]
categories = []
tags = []
+++

On a related note, I'd like to see [[wp:Xiaolin_Wu's_line_algorithm|Xiaolin Wu's line algorithm]] get added as a task. --[[User:Short Circuit|Short Circuit]] 21:40, 23 February 2009 (UTC)

== Clipping ==

[http://virtualdub.org/blog/pivot/entry.php?id=341 This] popped up in my RSS feeds today, and may be a worthwhile additional consideration in this task. (Possibly as an extra-credit goal). Also, it's be good to have a better description of the algorithm itself on-page. --[[User:Short Circuit|Michael Mol]] 16:09, 7 July 2011 (UTC)

== Korn Shell ==

I actually needed a Korn shell implementation of Bresenham's line algorithm today, so I decided to share it.  No, I wasn't plotting a line; anytime I need to smoothly interpolate between pairs of values, I reach for Bresenham.  In this particular case, I wanted to run N programs at evenly-spaced intervals in a cron job.
<lang>line 0 0 1440 $N | uniq -f 1 | tail +2 | awk '{print $1%60, int($1/60), "* * *"}'
```

It takes two-and-a-half seconds to run (and I throw away 99% of the data), but I only need it once in a blue moon.

: I'm not sure what this is doing.  My /usr/bin/line reads one line from stdin. --[[User:Rdm|Rdm]] 19:38, 21 March 2012 (UTC)
