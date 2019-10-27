+++
title = "Rosetta Code:Village Pump/Email protection in code"
description = ""
date = 2019-07-16T16:42:59Z
aliases = []
[extra]
id = 22431
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Email protection in code
|summary=Sometimes parts of the source code is replaced with "email protected".
}}
With 'sometimes' I mean the Forth language and the standardized words <code>R&#64; 2R&#64; C&#64; 2&#64; F&#64; SF&#64; DF&#64;</code>, but in principle it is possible, and not very unlikely, to define Forth words of the form something&#64;, or even something&#64;somethingelse.
I suppose there is a good reason for hiding email addresses, even if those who edit the pages put them there by themselves, but would it be possible to exclude &lt;lang&gt;...&lt;/lang&gt;-sections from this protection.
It ''is'' possible to replace a triggering @ with &amp;#64; and most browsers should display the code alright, and even copy-paste should work, but it's an inconvenience to mangle the code like that, and it doesn't feel right.
--[[User:Tommy|Tommy]] 12:24, 16 July 2019 (UTC)

: I had the same problem with ASCII-art (as included within the    <big> <nowiki> 
```txt
 </nowiki> </big>   <small>and</small>   <big> <nowiki> 
```
 </nowiki> </big>   HTML language tags,    where the art had, among other characters, random characters   (such as the    <big> '''@''' </big>   character)    used for dithering.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:59, 16 July 2019 (UTC)

:: I think I remember that (the above) was due to something that Cloudflare was doing, but I'm not sure, it was a while ago.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:42, 16 July 2019 (UTC)
