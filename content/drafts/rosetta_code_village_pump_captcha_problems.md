+++
title = "Rosetta Code:Village Pump/CAPTCHA problems"
description = ""
date = 2019-08-10T02:50:19Z
aliases = []
[extra]
id = 4011
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=CAPTCHA problems
|summary=CAPTCHA problems are not being attended to.
}}

(1) It is August 2019 and I see an issue being raised in 2016, "There are still/again problems with the captchas, when saving an article with a new link."  I can confirm this problem either still persists or has reared its ugly head again, with the possible difference that right now, solving the CAPTCHA problem eventually results in a timeout, and that this behavior has been observed using BOTH Google Chrome and Safari. [[User:Peak|Peak]] ([[User talk:Peak|talk]])


(2) Original Description.

I've recently come across problems with the CAPTCHA. When you use something that needs you to pass the CAPTCHA (e.g. editing anonymously, or after you enter your password wrong when you login), the CAPTCHA displays correctly, but when you enter the response and submit, you get a mostly blank page with just the words "Could not open socket". The case when you enter your password wrong is particularly annoying, as you are stuck in a Catch-22 (you normally don't need to pass a CAPTCHA to login; but once you enter your password wrong, you can't even login by entering your password correctly anymore, as the CAPTCHA won't work). I don't know if this is just me. Thanks. --[[User:Spoon!|Spoon!]] 05:38, 15 March 2009 (UTC)
: Likely not.  Mwn3d reported that the Twitter feed on my user page was reporting similar errors.  I'll look into it. --[[User:Short Circuit|Short Circuit]] 18:47, 15 March 2009 (UTC)
:Looks like there was some sort of server glitch going on.  I don't know if it was Slicehost-related, or if it was a problem with my version/configuration combination for Apache and PHP.  I upgraded all the distro software, reloaded apache a few times, and it appears to work. --[[User:Short Circuit|Short Circuit]] 19:43, 15 March 2009 (UTC)

There are still/again problems with the captchas, when saving an article with a new link.
It looks like google just times out before displaying the graphic,
so nothing to solve, so the edit cannot be saved with the link (see [[Hello_world/Newbie#Lua |here]]). 

I tried with different browsers (Firefox/Palemoon, Opera, Mozilla/Seamonkey), but the problem remains the same with all of them.
-- [[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 13:32, 5 October 2016 (UTC)

I keep getting "...contains new external links..." even though I haven't added any. Then the "captcha" process begins, first with the message, then the display is adjusted to allow space for the images, then nothing. I note "waiting on google.com" is shown and then finally the box for the images appears with "Sorry, something went wrong" and a retry does it all again. This has happened in the past (I'm using Firefox) except that the process has also worked in the past. Blocked for a day, now. Later, another try with the same result but then I noticed "Privacy Badger" was muttering (for instance, "red" for stats.g.doubleclick.net and "orange" for www.google.com), so I set its barriers to "green", and tried again. This time the final message was "Please upgrade to a supported browser", except that Firefox is one of those declared approved. And the "About Firefox" reports version 52.5.2 (32-bit) and that "Firefox is up to date" Similarly, I have just tried via gnuLinux (Mint) on the alternate use of the same computer, with the same prod "Please upgrade to a supported browser", and this is Firefox 57.0.1 (64-bit) There is no badger on this system.  So, should I try moving the rodent with my other hand? [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 00:00, 24 December 2017 (UTC)
:And then I tried clearing Cached Web Content and Site Data, and on logging-in afresh the captcha stuff manifests, and endlessly requires another trial (and the blue tick appears even for squares obviously not involving the described feature as well as for dubious images, so I suppose that trial&error won't work, ha ha) and after robotically slogging onwards, eventually it is allowed that I'm not a robot. Ho hum [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 09:06, 25 December 2017 (UTC)
