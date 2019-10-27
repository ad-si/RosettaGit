+++
title = "Rosetta Code:Village Pump/CSS problem with Progress language"
description = ""
date = 2016-07-09T01:43:47Z
aliases = []
[extra]
id = 20812
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=CSS problem with Progress language
|summary=The CSS for the Chamelion Theme has a conflicting CSS directive
}}

The display properties for the [[OpenEdge/Progress|OpenEdge/Progress]] languge examples get overwritten when using the Chamelion (default) theme. In the source, the Progress examples are enclosed in <nowiki><pre class="progress highlighted_source"> 
```
</nowiki> markup, specifying the highlighter to use the syntax settings for Progress. Unfortunately, the load.css file has a separate .progress directive that specifies (among other thing,) a height of 20px. (Apparently it is for some kind of progress bar?) So the Progress examples all only show approximately the first line of code. At any rate, if I switch to the Vector theme, Progress examples display correctly; or, if I manually edit the CSS to disable height parameter for the .progress directive they display correctly.

Any chance the load.css file can be edited to comment out or remove the height parameter from the .progress style? It seems to function fine without it, (it isn't in the Vector theme at all,) and it really makes it difficult to view the [[OpenEdge/Progress|OpenEdge/Progress]] examples with the default (not logged in) theme. Failing that, is there a way to change the name used internally by the highlighter to not conflict with the directive? Thanks --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 19:33, 18 April 2016 (UTC)
