+++
title = "Rosetta Code:Village Pump/RC thinks cookies are disabled on Chrome"
description = ""
date = 2019-09-16T00:23:20Z
aliases = []
[extra]
id = 22303
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=RC thinks cookies are disabled on Chrome, preventing login
|summary=Self-explanatory. I had to log in on Firefox just to file this page.
}}
:Are you sure that cookies '''aren't''' disabled? I am posting this using Chrome in a low permissions account and it seems to work fine. --[[User:Thunderbot|Thunderbot]] ([[User talk:Thunderbot|talk]]) 10:11, 3 May 2019 (UTC) (nominally [[User:Thundergnat|Thundergnat]])

:I've checked Chrome's settings to verify that cookies are enabled, I've made a specific exception to say that cookies are *always* enabled on RC, and I've verified using the padlock next to the URL that cookies are indeed being created. I've tried deleting cookies and having it recreate them just in case the cookies it saw were corrupted somehow. I've tried disabling uBlock Origin and Dashlane on this website just in case they were messing with it. None of those fixed it; what ended up fixing it was apparently just waiting a day. I'm assuming the problem was specific to a certain version of Chrome that I've now updated past. [[User:ThecnoNSMB|ThecnoNSMB]] ([[User talk:ThecnoNSMB|talk]]) 12:32, 3 May 2019 (UTC)

:: I too, have been plagued at various times with this bogus warning of not having cookies enabled.   (I am using FireFox).   It is a hit-and-miss sort of thing.   I'd be running along for days, and then, Rosetta Code is displaying pink messages stating that I don't have cookies enabled.   Once I do get these messages,   I do have to close out the FireFox sessions and restart FireFox.   I finally narrowed it down to   (in my case)   that something in my Windows "system" wasn't responding fast enough   (or in a timely manner).   One of the things I eventually tried was to limit FireFox   (via the setting of   ''affinity'')   to just a single processor   (I have two).   When FireFox is allowed to use all processors,   it can apparently preempt other processors just long enough so that it (or something) appears to fail in such a manner to cause something to think that cookies aren't enabled (or whatever).   This problem is nothing about cookies   (as per what I was observing).   One other symptom was that when my (Windows) system was really memory constrained at times   (i.e.,   when I had a lot of FireFox tabs (and/or windows) open.   Once I had FireFox   (or, I suspect, any web browser)   restrained to just one processor,   I've not had a problem since.   A pain in the ole neck-hole, but it solved my memory constrained PC.   Every once in a while,   I'd start up FireFox and forget to set the ''affinity'',   and the problem would again manifest itself,   but only when attempting do perform an ''edit'' or some other update.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:54, 13 September 2019 (UTC)

: I have a similar problem. When I access http://rosettacode.org, I can't even login due to the cookie issue. I instead need to access https://rosettacode.org (the https protocol). --[[User:Sorawee|Sorawee]] ([[User talk:Sorawee|talk]]) 15:41, 13 September 2019 (UTC)

:: After a few months without a failure, Rosetta Code is again failing for me (using FireFox under Windows).   I found that when I manually change   (by adding a prefix)   the Rosetta Code (URL) address to use   <big> '''HTTPS://''',</big>   the "cookies" failure goes away.   A sort of a pain in the neck-hole to get around the   ''cookies are disabled''   message.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:22, 16 September 2019 (UTC)
