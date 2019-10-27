+++
title = "Rosetta Code:Village Pump/Image uploads"
description = ""
date = 2019-06-12T18:02:12Z
aliases = []
[extra]
id = 3352
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Image uploads
|summary=Relating to upload of images to Rosetta Code
}}
I enabled image uploads yesterday, for ''all logged in users''.  Please try to limit uploads to original and/or programmatically-generated material.  If you feel it necessary to upload an image retrieved from elsewhere, please ensure that uploading it here does not violate the original copyright.  I'm not going to depend on Fair Use to get around more restrictive licenses, because I don't have the legal resources to defend against an infringement claim. --[[User:Short Circuit|Short Circuit]] 23:50, 3 February 2009 (UTC)
:I'm a little late with this, I suppose, but I've been dinking around with [http://www.dangermouse.net/esoteric/piet.html Piet] ([http://esolangs.org/wiki/Piet Esolangs entry]) over the past few days, and its programs are ''entirely'' images. (From the Piet page: "Piet is a programming language in which programs look like abstract paintings.") I like the language (as a toy), but it's possible (although somewhat unlikely) that the images could get... '''big'''. (Biggest one known ''so far'': [Image:http://www.dangermouse.net/esoteric/piet/pietquest.png])
:So... What's the upper limit on images? Not "what will Mediawiki support", but "what do you consider too big?" -- [[User:Eriksiers|Erik Siers]] 17:11, 17 May 2010 (UTC)

:: I think for such images a codel size of 1 should be mandatory but apart from that it's not that large. There are certainly longer examples in other languages, even non-esoteric ones. If they get too large you can do the same as for other languages: Put it on a separate subpage. —[[User:Hypftier|Johannes Rössel]] 18:43, 17 May 2010 (UTC)

:::My concern isn't the on-screen dimensions of the image, but rather the file size limits that [[User:Short Circuit|Short Circuit]] wants to stick to. The image upload page says "Maximum file size: 10 MB", but if I upload a 10MB file for each example on the site, disk space and bandwidth would likely be adversely affected. -- [[User:Eriksiers|Erik Siers]] 18:54, 17 May 2010 (UTC)

:::: Let me keep an eye on disk and bandwidth usage. If it becomes a problem, I'll kick, curse, swear, and then finally put up a donate link. :) --[[User:Short Circuit|Michael Mol]] 19:32, 17 May 2010 (UTC)

:::: Piet being a graphical programming language, I would expect that it requires lossless image file formats. I recommend trying both GIF and PNG+pngcrush, and uploading whichever file winds up being smaller. --[[User:Short Circuit|Michael Mol]] 19:35, 17 May 2010 (UTC)

::::: I haven't done anything yet, but yes, PNG + pngout (via Irfanview) is what I have in mind. -- [[User:Eriksiers|Erik Siers]] 19:42, 17 May 2010 (UTC)

::::: While we're at it. Ideas for a naming convention? For the first task I did in Piet (so far the only one – [[:File:Piet A+B.png]]) I used ''Piet «task name»''. If that needs work, it can surely be renamed. Better ideas maybe? —[[User:Hypftier|Johannes Rössel]] 20:21, 17 May 2010 (UTC)

A new day - a new start. Is uploading an image file still disabled? If not, how does one go about doint this?? ~~----

: +1. For example, I would prefer to replace the HTML+CSS+Unicode-art diagrams on [[Brace_expansion]] with images, because in their current form they don't render quite as they should on all browsers and platforms. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:20, 4 July 2014 (UTC)

==Broken with Safari==
Does anyone have any idea why upload of images doesn't work with Safari? I create an image (e.g., a GIF or PNG) and try to upload it, but I get a red error message saying that only a few formats are supported, ''including GIF and PNG''. That's bizarre! It works if I use Firefox. (Yes, I'm logged in in both cases.) To me, it smells like a bug… –[[User:Dkf|Donal Fellows]] 13:35, 29 November 2011 (UTC)
: And reading Mike's talk page, it appears to be related to Javascript. Thank goodness for NoScript, which makes it all work at all… –[[User:Dkf|Donal Fellows]] 13:49, 29 November 2011 (UTC)
::People have reported the same problem across multiple browsers. I've had it myself on Firefox and IE. I found a [http://irclog.perlgeek.de/rosettacode/2011-07-06#i_4069398 really weird workaround]. We're hoping that a MediaWiki update can fix it. --[[User:Mwn3d|Mwn3d]] 13:52, 29 November 2011 (UTC)
: I disabled AJAX uploads serverside, but you may still need to disable it in your user profile settings. --[[User:Short Circuit|Michael Mol]] 15:12, 29 November 2011 (UTC)
::I can't find any options in my preferences related to image uploads. Which option are you talking about? --[[User:Mwn3d|Mwn3d]] 15:28, 29 November 2011 (UTC)
::: Hm. I can't find it, either. It may be hidden because I've disabled the feature serverside, or it may not be there at all. Grr. I *really* need to step on that MW update. --[[User:Short Circuit|Michael Mol]] 17:10, 29 November 2011 (UTC)

Looks like uploads have been broken since May 2013. Bummer!
--[[User:Loren|Loren]] ([[User talk:Loren|talk]]) 04:01, 24 September 2013 (UTC)
----
--[[User:Loren|Loren]] ([[User talk:Loren|talk]]) 04:01, 24 September 2013 (UTC)
:They are not so much broken as disabled on purpose, see [[Special:Upload]]. A spam issue, probably... --[[User:Morn|Morn]] ([[User talk:Morn|talk]]) 12:50, 3 October 2013 (UTC)
::That excludes the possibility to contribute with tasks made in any graphical language, e.g. Scratch, VEE, LabVIEW etc. Thats a pitty 'cause for example Labview has a feature called snippets where code can be saved in a .png image. And if you drag this image into Labview, it inserts the code, not the image. --[[User:MaViMi|MaViMi]] ([[User talk:MaViMi|talk]]) 19:09, 13 February 2015 (UTC)


== Trusted users ==
Would it be possible to give certain users a "trusted" status so that they can upload images (and perhaps also post external links, make new pages without having to negotiate an almost unreadable CAPTCHA), after they have made a certain number of serious contributions over a certain amount of time? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 13:20, 22 April 2015 (UTC)

: I wholeheartedly endorse that idea! 

CAPTCHA is a royal pain in the neckhole and for most phrase presentations, it is truly almost unreadable and it just causes multiple attempts, followed by more attempts to find a phrase that can be decyphered/readable. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:40, 2 May 2015 (UTC)

:I don't like CAPTCHAs but I hate the spammers and wouldn't like to make it too easy for them. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:37, 2 May 2015 (UTC)

:: I was thinking along the lines of bypassing CAPTCHAs for   ''trusted''   Rosetta Code users, and of course, the keyword would be   ''trusted''   users.   Since spammers do what spammers do, but the people who need to upload images and create links (legitimately), could trusted RC users could be defined as those users who have entered (say) a half-dozen entries (or some defined quota).   I wouldn't object to even a score or thereabouts.   Like ya said, don't make it too easy for spammers.   I don't envision spammers taking the time (or the effort) to enter ... well, entries (as in computer programs). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:01, 2 May 2015 (UTC)


== This is still a problem (June 2015) ==
There is much discussion above.  What do contributors who need to upload an image do today?  [[Special:Upload]] says "File uploads are disabled."  I would like to add some [[Blockly]] entries, which are fundamentally graphical. --[[User:Chkno|Chkno]] ([[User talk:Chkno|talk]]) 23:07, 19 June 2015 (UTC)

:I am working on Piet examples for this site, but I see that I cannot upload any images. Even larger programs would not need that much space, especially if one uses compressed PNG files. And I am not talking about huge Piet programs like “Pietquest” that wastes a lot of image space due to the structure of the compiler it was created with. What do I do now? Is there any possibility to upload pictures?
:Just as an example: The Piet program for the Integer Sequence task [Rosettacode link:http://rosettacode.org/wiki/Integer_sequence] needs only 180 bytes at a codel size of 20. The image itself is only 140x60 pixels (7x3 codels) large. You can have a look at it here: [Image:https://copy.com/TQuwy3dwBRl7nEOL] (--[[User:Albedo|Albedo]] ([[User talk:Albedo|talk]]) 21:58, 26 June 2015 (UTC)
:: Small images can be rendered as a wiki table.  This is not ideal, since actual image formats are more compact. However, it works around the current restrictions (which we can thank our friendly neighborhood spammers for).  For an example of an image rendered this way, see [[Mandelbrot_set/J/Output]]. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:06, 7 July 2015 (UTC)
:::Thank you, Rdm. It’s certainly feasible for small images, although manual editing is a bit tedious, even for this example(binary digits): [[http://rosettacode.org/wiki/Binary_digits#.7B.7Bheader.7CPiet.7D.7D]]  --[[User:Albedo|Albedo]] ([[User talk:Albedo|talk]]) 15:42, 10 July 2015 (UTC)
:::: Yeah, if you were doing a lot of them, I'd suggest starting with a [[Bitmap]] implementation and then whatever other support you want from [[Raster_graphics_operations]] so you can read an image file. Once you have that, you could use imagemagick to convert to your supported file format. Then build a wiki-table renderer and use that for your Piet code.  And I guess some of us should also start using one of the [http://www.dangermouse.net/esoteric/piet/tools.html piet interpreters] so we can see your code in action ... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:47, 11 July 2015 (UTC)
::::: I‘m writing my own Piet interpreter in Julia, especially for debugging more complicated code. And I use npiet to check if my code works. The PietDev on Rapaping.com does not work according to specification because it does not use integer division, but every division operation produces a floating point value. Nevertheless it’s useful for creating smaller examples and stack inspection.
::::: I guess it’s time for me to write a PNG to wikitable translator of sorts. --[[User:Albedo|Albedo]] ([[User talk:Albedo|talk]]) 15:31, 11 July 2015 (UTC)
:::::: Done. I wrote a little Julia function that converts PNG files with codel size 1 to wikitables. The code is on my user page.  --[[User:Albedo|Albedo]] ([[User talk:Albedo|talk]]) 02:06, 12 July 2015 (UTC)

== This is still a problem (June - July 2016) ==
Uploading images doesn't work for me at all.[[User:NNcNannara|NNcNannara]] ([[User talk:NNcNannara|talk]]) 12:23, 18 July 2016 (UTC)
:I went to [http://rosettacode.org/wiki/Special:ListFiles File list] and discovered that all file uploads are blocked since 6/2/2016, even admins have no uploads. Is it sort of technical glitch? Or result of fighting spam too hard?? I'm planning 3 new tasks with 7 pictures. Without picture uploads it is useless. Now this is an old Q (late June): Another interesting issue: Why nobody complains? --AnatolV 19:34, 21 July 2016 (UTC)

== This is still a problem (August 15, 2016) ==
Sure not working here. [[User:KenS|KenS]] Upload complains that the image is not the proper mime type, but it is. Neither jpg or png are accepted. Sure would like to add some images to my code -- it really helps the reader understand what the output should be. --[[User:KenS|KenS]] ([[User talk:Kens|talk]]) 02:06, 15 August 2016 (UTC)

== This is still a problem (July 2017) ==
'''@NNcNannara, KenS''': U r so naive guys... I've complaint about fake massages 
many years ago! See  [[User_talk:Short_Circuit|M. Mol page]].  LOL
Also, find there fake explanations and fake promises (this January). --AnatolV 19:38, 22 July 2017 (UTC)

== This is still a problem (December 2018) ==
'''@M. Mol''': Send me email please. I have a realistic suggestion: how to find a big sponsor 
helping to upgrade RC servers. It helped my friend to upgrade dying website. --AnatolV 19:38, 22 July 2017 (UTC)

== Can't upload an image (July 2019) ==
I'm trying to upload my output for the "Munching Squares" task.  The file is called xor_pattern_miniscript.png, 41 KB in size, but when I try to upload it I get errors like this:


```txt
Upload warning

    Could not read or write file "mwstore://local-backend/local-public/5/5b/Xor_pattern_miniscript.png" due to insufficient permissions or missing directories/containers.
    Could not store file "/tmp/phpK6HWOb" at "mwstore://local-backend/local-public/5/5b/Xor_pattern_miniscript.png".
    Could not delete lock file for "mwstore://local-backend/local-public/archive/5/5b".
    Could not delete lock file for "mwstore://local-backend/local-public/5/5b".
    Could not delete lock file for "mwstore://local-backend/local-public/archive/5/5b/20190612175726!Xor_pattern_miniscript.png".
```


I'm not sure if this is the same problem discussed above, or something new.  Any idea how I can upload my image?  Or, should I just link to an image uploaded elsewhere? --[[User:JoeStrout|JoeStrout]] ([[User talk:JoeStrout|talk]]) 18:01, 12 June 2019 (UTC)
