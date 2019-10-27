+++
title = "Category talk:UNIX Shell"
description = ""
date = 2011-08-30T02:42:31Z
aliases = []
[extra]
id = 2163
[taxonomies]
categories = []
tags = []
+++

==Implementation==
Additions to the "implementation" section too long? Separate into a separate page somewhere?

I have mixed feelings about the section I just added to this section.  On the one hand I know (from years of scripting, and supporting other shell scripters and occasionally teaching classes in the topic) that this information is important.  On the other hand it seem like it should go into its own page somewhere.

On another tentacle, I'm not sure how much this relates to the core focus of this site.  The details of how various Bourne-compatible shells handle their subshells, pipelines and variables have little or no analogue in other programming languages.

Thoughts?

[[User:JimD|JimD]] 12:17, 12 October 2007 (MDT)

:I think this is really meaty information that would be wonderful to have on this site, but I agree that it is too much for this page.  I think we strive for just a couple paragraph summary on each language category page, so that you don't have to scroll down to see the tasks solved. How about making a new page and link to it from here? Can't think of a good name for it off the top of my head.  --[[User:IanOsgood|IanOsgood]] 19:44, 12 October 2007 (MDT)

==&lt;lang bash&gt; versus &lt;lang sh&gt;==
I use &lt;lang bash&gt; for all Bourne-compatible examples, including [[pdksh]] and plain [[Bourne Shell]] code. Some other contributors use &lt;lang sh&gt;. Which is better, &lt;lang bash&gt; or &lt;lang sh&gt;? I prefer &lt;lang bash&gt;, because the page at [[Rosetta Code:Village Pump/Syntax Highlighting]] says to use "bash" for "UNIX Shell". --[[User:Kernigh|Kernigh]] 16:09, 22 August 2011 (UTC)

== When it works with Bourne Shell ==

* ''Do not make [http://rosettacode.org/mw/index.php?title=Loops/Infinite&oldid=114610#UNIX_Shell a long list of Bourne-compatible shells] above each Bourne-compatible solution.'' A single <nowiki>{{works with|Bourne Shell}}</nowiki> is enough. Do not add works with Almquist Shell, works with Korn Shell, works with Bash, ...
* ''Do not post two copies of the same script.'' There should not be [http://rosettacode.org/mw/index.php?title=User_input/Text&oldid=118994#UNIX_Shell an extra copy] that only has #!/bin/bash instead of #!/bin/sh, only because the script also works with Bash.

--[[User:Kernigh|Kernigh]] 02:42, 30 August 2011 (UTC)
