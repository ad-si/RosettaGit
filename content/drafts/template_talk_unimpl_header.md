+++
title = "Template talk:Unimpl header"
description = ""
date = 2009-08-26T06:39:58Z
aliases = []
[extra]
id = 4061
[taxonomies]
categories = []
tags = []
+++

What are all these extra arguments for? Is the first one the count of the number of tasks that are unimplemented in {{{1}}}?--[[User:Mwn3d|Mwn3d]] 18:37, 7 April 2009 (UTC)
: It's in ImplSearchBot's source.  They're supposed to let you describe how far a language has penetrated, but I forget which number means what. --[[User:Short Circuit|Short Circuit]] 22:30, 7 April 2009 (UTC)
:: It would be nice to have three levels of penetration (red/yellow/green) so as to indicate languages that are widely supporting tasks (e.g., have done 75% of tasks) as well as languages that are not (the current 25% threshold). And then some of us can (and will be motivated to) work to bring more up to the "highly penetrated" threshold. —[[User:Dkf|Donal Fellows]] 13:01, 27 June 2009 (UTC)
:::I thought about that (and I had a color code picked out), but I didn't want to make the crazy if statement for it. I was really only trying it out when I put the box in and no one complained. The expression right now is:

```txt
{{#ifexpr: {{{4|0}}} >= 25|background:#90ff90;color:black;">[[Rosetta_Code:TODO#Languages|Minimum penetration]] met. This language has| background:#ff9090;color:black;">[[Rosetta_Code:TODO#Languages|Minimum penetration]] not met. This language only has}}
```
 It would take another <nowiki>{{#ifexpr:</nowiki> block right after ">= 25|". I can try it out later today, but if you want to try some wikicode then go ahead.--[[User:Mwn3d|Mwn3d]] 15:58, 27 June 2009 (UTC)
:::I set it at 50% for now. It can be changed easily. --[[User:Mwn3d|Mwn3d]] 23:37, 27 June 2009 (UTC)

== Percentiles ==

I think the concept of "minimum penetration" might be the wrong way to describe things.  From the social perspective, it caries a foreboding tone, which is the opposite of what I'd like to present to contributors for languages not yet seen.  From the technical perspective, well, we've got several percentile categories.  I think the template ought to be used to populate those, and those same concepts come packaged with a competitive air. --[[User:Short Circuit|Short Circuit]] 06:39, 26 August 2009 (UTC)
