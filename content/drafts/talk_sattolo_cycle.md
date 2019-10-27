+++
title = "Talk:Sattolo cycle"
description = ""
date = 2017-04-03T15:39:37Z
aliases = []
[extra]
id = 21093
[taxonomies]
categories = []
tags = []
+++

== Yet another task needing a description ==

Linking to a wikipedia page really is not good enough. Those pages change over time, and are not written specifically for our site. Anyways, this task needs an adequate description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:21, 5 September 2016 (UTC)

: Done. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 08:40, 5 September 2016 (UTC)

== Not sure this is a worthwhile issue ==

Given that the Sattolo cycle is almost identical to the Knuth Shuffle - with the only difference being an otherwise invisible implementation detail - what makes this task worth doing?

Should we be looking forward to tasks like "bubble sort with descending iteration"? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:52, 5 September 2016 (UTC)

: It's not invisible, as it affects the algorithm's behavior. For example, for <code>[10, 20, 30]</code> the Knuth shuffle has six possible outputs, but the Sattolo cycle only two, because it cannot output permutations that have elements in their original place. That makes it interesting, at least. I suppose the two algorithms could have been covered by a single combined task, but it's too late for that now that [[Knuth shuffle]] has existed for so long and has so many solutions. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 17:19, 7 September 2016 (UTC)


==Some formulae in Spec notes section invisible to most browsers==

The majority of browsers (those which display server side graphic files for formulae, rather than using local MathML processing and locally installed fonts) are unable to display various formula elements in the Specification, particularly in its notes section. Unexpected content within &lt;math&gt; tags has choked the MediaWiki processor, leading it to generate syntactically ill-formed HTML - specifically tags which lack a semicolon between the height and vertical-align attributes.
The unexpected content may include the use of naked &lt; or &gt; characters, or other anomalies such as redundant flanking space, or literal elements which do not constitute well-formed Latex. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:09, 21 September 2016 (UTC)

: Visibility of task description math expressions now restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:24, 24 November 2016 (UTC)

== each element ends up in a new position ==

You need an array with at least two elements to make sure each element ends up in a new position. The single element in an array with one element cannot end up in new position. The possible output array shown for the test case [10] doesn't meet the task description, as all elements in that array did end up in their original position. [[User:Dedalus|Dedalus]] ([[User talk:Dedalus|talk]]) 12:29, 3 April 2017 (UTC)

: The pseudocode in the task description performs this operation zero times for that case. The reason for this is the same reason it does not shuffle the two element array twice (resulting in an identity operation). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:39, 3 April 2017 (UTC)
