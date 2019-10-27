+++
title = "Talk:Voronoi diagram"
description = ""
date = 2019-05-13T02:52:14Z
aliases = []
[extra]
id = 10115
[taxonomies]
categories = []
tags = []
+++

==Links in JavaScript Version #1 not working!==
Both the Chrome and the IE refuse to show this '''bl.ocks.org''' website.

Here are the Chrome msgs:

This site can’t provide a secure connection

bl.ocks.org uses an unsupported protocol.

ERR_SSL_VERSION_OR_CIPHER_MISMATCH

The client and server don't support a common SSL protocol version or cipher suite. 
This is likely to be caused when the server needs RC4, which is no longer 
considered secure. [end of msg]


Please notify this website (if you can) and update links or delete them. --AnatolV 22:08, 15 June 2017 (UTC)

: Those are ssl error messages, but the links are http: links. So I imagine what's happening is something (perhaps your browser) is automagically changing them to https links. You'll need to change them back if you want them to work. For now, at least. It's possible that the web site owner will decide to support https, but as a general rule activity on web sites comes in bursts with long delays in between. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:40, 15 June 2017 (UTC)

: Seems to be working fine from here [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:14, 15 June 2017 (UTC)

'''@ Rdm & Hout.''' With all do respect to you two: why do you afraid of saying:
* I have Linux with XXX browser.
* I have Mac OS 10 with YYY browser.
* Etc.
Such answer would help RC users.

I have Win 10 with latest versions of Google Chrome and IE. Both not working.

'''@Rdm:''' You just "imagine", guessing, but '''I've tested'''.
In addition, RC has http://rosettacode.org/wiki/Rosetta_Code. NO https, no problem.
Same http://oeis.org/wiki/User:XXX and hundreds of other sites. NO problem.

'''@Hout''' If you have no problem, then contact webmaster and point him to our discussion, 
but tell him OS/browser you are using.
It would really help. Or as I already said: upgrade links. --AnatolV 21:08, 16 June 2017 (UTC)
:: Both links are responding instantly here with Chrome (Version 58.0.3029.110 (64-bit)) Firefox (53.0.3 (64-bit)) and Safari (Version 10.1.1 (12603.2.4)) as well as Safari Technology Preview (Release 33 (Safari 11.0, WebKit 12604.1.25.0.2)). The OS on which Chrome, Firefox and Safari are running here is macOS Sierra. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:20, 16 June 2017 (UTC)
::: I notice, however, that https versions are now available on that site, so I will update the links. I hope that helps your workstation to read the pages successfully. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:23, 16 June 2017 (UTC)

::: '''@Hout''' TYVM Nice complete answer.

::: Many years ago (15?) a bug in IE for Mac OS X was discovered and reported by me to MS. 
::: 2 months later they fixed it. It was done fast, because it was related to security.

::: I'm telling this story to stress, that the same browser on different OS can work differently.

::: My main concern was (and is) to find the way for all RC users to reach D3 library.

::: I've found the way through "North Pole", i.e., Github, etc. Here is a link to the amazing Gallery:
::: https[://]github[.com]/d3/d3/wiki/Gallery. Best regards. --AnatolV 20:04, 17 June 2017 (UTC)

:::: Good ! I'm glad you found a way to the d3/d3js galleries. They seem a wonderful body of work to me. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:27, 17 June 2017 (UTC)

==Task?==
The task as given in the sample Python code is rather boring.  How about calculating vertices of the Voronoi tessellation, or draw a map of it, or some such? --[[User:Ledrug|Ledrug]] 05:18, 19 July 2011 (UTC)
:Whether a task is interesting shouldn't be cause for criticism. Start with the basics and if you want more fun add more complicated, related tasks. --[[User:Mwn3d|Mwn3d]] 12:12, 19 July 2011 (UTC)

Also the task description does not say what needs to be accomplished. --[[User:Paddy3118|Paddy3118]] 05:45, 19 July 2011 (UTC)
:Judging by the example code, it's probably this: given a number of sites and a set of grid points, for every site find all the grid points that are closest to it, which boils down to a distance comparison. --[[User:Ledrug|Ledrug]] 05:56, 19 July 2011 (UTC)

==Metrics==
It's fun to compare the diagrams induced by different metrics (e.g., the [[wp:Taxicab geometry|taxicab metric]]). All it takes is a change of measurement function. –[[User:Dkf|Donal Fellows]] 14:52, 21 July 2011 (UTC)
: Taxicab metric does give some interesting result.  [[File:voronoi-taxicab.png|thumb]] --[[User:Ledrug|Ledrug]] 00:41, 22 July 2011 (UTC)
[[file:voronoi-taxicab-small.png|left]] More about taxicabs: since the PureBasic solution provided a taxicab version, I'll have to point out that this metric has a special case not handled by it.  When two sites are aligned at exactly 45 degrees, there may be a region (instead of a line) that's equal distance to both sites, as seen in the small image to the left: every point in the gray area is same distance from both orange and blue site.
:: This is a solid point, but it is likely also true for any Euclidean version. Especially if the picture/map is based on a x/y-coordinates in integer form. --[[User:Jofur|&lt;Jofur&gt;]] 05:39, 23 July 2011 (UTC)
::: For Euclidean metric it's not a problem, because if a point has the same distance to two distinct sites, it must lie on the central dividing line between them, so you'll never have a 2d area from that. --[[User:Ledrug|Ledrug]] 18:35, 23 July 2011 (UTC)

==Problem==
I want to illustrate my Prolog code with 3 pictures but I can't upload these images from my computer !? I tried .png, .gif and .jpeg I'll try to-morrow [[User:Trap D|Trap D]] 00:48, 1 September 2011.<BR>
Fixed ! I change FF for IE  [[User:Trap D|Trap D]] 01:05, 1 September 2011.

== More on Minkowski distance ==

I can seem to be a bore, but the mathematician's surname was Minkowski, not Minkovski.

Euclidean and Manhattan distances are the special cases of his distance, so we can create something in-between or something entirely different by playing with this metric.

For example, it can create a pattern like a Manhattan distance and add some [https://i.imgur.com/5xwed9M.png rounding] to the corners. (Upload error happened and I wasn't able to add image here, so I used Imgur.)

There's an [https://harryrschwartz.com/2015/09/29/voronoi-experiments article on different metrics] by Harry R. Schwartz, which shows even more examples.
