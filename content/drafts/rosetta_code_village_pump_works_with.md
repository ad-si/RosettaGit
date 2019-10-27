+++
title = "Rosetta Code:Village Pump/Works with"
description = ""
date = 2010-11-28T17:29:33Z
aliases = []
[extra]
id = 5395
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Works with
|summary=Works with template - usage
}}
Question: How am I supposed to be using the [[:Template:works with|works with]] template? In my JavaScript examples, I've put "<nowiki>{{</nowiki>works with|JavaScript|1.6<nowiki>}}</nowiki>" so far, since in my opinion, the language version gives more information than in which implementations it works. Should I change it, or is it all right to use it that way? (at the moment, only Firefox supports anything >1.5, as far as I know [of web browser implementations, that is]). ~[[User:FireFly|FireFly]] <sup>[[User talk:FireFly|t]]</sup><sub style="margin-left:-4px">[[Special:Contributions/FireFly|c]]</sub> 14:42, 30 January 2010 (UTC)
:In this case you should use it when you use features of JavaScript 1.6 that aren't compatible with JavaScript 1.5 and lower. You should also use it if you use a specific third-party library or even a special compiler that might accept different syntax. --[[User:Mwn3d|Mwn3d]] 16:10, 30 January 2010 (UTC)
:: Ah, yes, that makes sense. Thanks for the information. ~[[User:FireFly|FireFly]] <sup>[[User talk:FireFly|t]]</sup><sub style="margin-left:-4px">[[Special:Contributions/FireFly|c]]</sub> 20:01, 30 January 2010 (UTC)

: That template is primarily for showing that an example depends on something. It started out as a set of different templates for compiler, interpreter, language, platform and operating system versions, but trying to cleanly classify things on those grounds is a contentious and muddy matter. It boils down to identifying a set of requirements so that when someone says, "Hey, this doesn't work for me," it's easier to identify why. (Your HTML+CSS Quine example would be a good place, for example, to show that it works with Opera, Firefox and Safari.) --[[User:Short Circuit|Michael Mol]] 17:46, 30 January 2010 (UTC)
:: Ah, I see. Yes, didn't think of it when I added the HTML+CSS one :P, added it now. ~[[User:FireFly|FireFly]] <sup>[[User talk:FireFly|t]]</sup><sub style="margin-left:-4px">[[Special:Contributions/FireFly|c]]</sub> 20:01, 30 January 2010 (UTC)
