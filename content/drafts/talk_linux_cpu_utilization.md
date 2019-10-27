+++
title = "Talk:Linux CPU utilization"
description = ""
date = 2016-07-21T00:10:31Z
aliases = []
[extra]
id = 19187
[taxonomies]
categories = []
tags = []
+++

I think this task is pretty much ready to go... but as its the first task I've added to Rosetta Code, I'd like to get someone to review it first :)
--[[User:Paul|Paul]] ([[User talk:Paul|talk]]) 09:49, 27 May 2015 (UTC)

== OS specific task ==

I don't think OS specific tasks are a good idea.
Does this really need to be OS specific?

For starters, <tt>/proc</tt> is '''not''' unique to Linux,
although other OSes may provide other/better ways to get similar information
(e.g. <code>getloadavg(3)</code>).

Some programming languages may provide OS agnostic routines/libraries to get similar information
(which would be superior to grepping around in <tt>/proc/stat</tt> IMO).
&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 13:35, 27 May 2015‎ (UTC)

: Actually, it's probably better to have something like this be OS-specific. I've seen plenty of other tasks which are written in a way which implies they are OS-agnostic but where the difficulty of implementation is trivial for implementations which assume a specific OS context and near to implement portably without such assumptions.  It'll never be perfect. And this one seems simple enough. 
: That said, this one can be a pain if you don't have access to a linux implementation of your language. You can approximate, though, by using a surrogate file (representing /proc/stat) and code against that (changing it between reads). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:04, 27 May 2015 (UTC)

::I fired up virtualbox with an Ubuntu image to create the Python version. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:59, 27 May 2015 (UTC)

::: It's easy for you to say (''using a surrogate file''), but I don't have access to a Linux system.   But if somebody would include such a (sample) file here, it would be a snap to code this Rosetta Code task in REXX.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:10, 21 July 2016 (UTC)
