+++
title = "Rosetta Code:Village Pump/Task page subsections"
description = ""
date = 2014-07-23T18:00:06Z
aliases = []
[extra]
id = 17748
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Task page subsections
|summary=Solving the problems caused by per-language subheaders on task pages
}}

'''''Problem:'''''

On many task pages, people have separated some language sections into multiple subsection - often for multiple alternative solutions or approaches to the problem.

That's great and all, but it causes two problems:

<ol>
<li><p>'''Unstable links'''</p>
If you link to <code><nowiki>[[SomeTask#Simple_Solution]]</nowiki></code>, and then someone adds another "Simple Solution" subsection for another language on the same page but higher up than the first, then your link will suddenly point to the wrong place (the section that it used to point at would now be referred to as <code>#Simple_Solution_2</code>, due to MediaWiki's consecutive numbering of <code>id</code> attributes).</li>
<li><p>'''Less informative "Recent changes" page'''</p>
Some people like to watch the [[Special:RecentChanges]] page for edits to specific programming languages. That's possible because when the person who made the edit clicked the "Edit" link of the corresponding language header, the summary message will show something like:

:  ''(→‎<span class="autocomment"><nowiki>{{header|Perl}}</nowiki>: </span> use more descriptive variable names)''

However, when they clicked the "Edit" link of a <i>sub</i>section, the summary will be something like:


:  ''(→‎<span class="autocomment">Recursive Solution: </span> use more descriptive variable names)''

...which doesn't say anything about which language the edit was for.
</li>
</ol>

'''''Possible Solution:'''''

To fix those problems, I suggest the addition of a new template called '''section''', and making this the officially recommended way to introduce language-specific subsections:

: <code><nowiki>==== {{section|Perl|Simple Solution}} ====</nowiki></code>

...With the following features:

* people would see the language name in edit summaries ''(that's a given)''
* the template would output its third argument, so readers of task pages won't even see a difference ''(that's easy)''
* the template would force the language name to appear in the subsection's link name (which is used in the table-of-contents), e.g. <code>SomeTask#Perl__Simple_Solution</code> instead of <code>SomeTask#Simple_Solution</code> ''(that's the part that requires further research)''

Any MediaWiki experts out there who can comment on the feasibility of implementing the third feature (without breaking the first)?

Any other opinions/suggestions?

Cheers,

--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 12:08, 4 July 2014 (UTC)

:I too had seen the problem of not knowing what had been edited from the RecentChanges page that I use all the time and have in some cases prefixed subheadings with 'Python:' which is similar to your solution above. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:48, 4 July 2014 (UTC)
::We have {{tmpl|anchor}} which adds an anchor link and has a display text option. You could try that out in the [[Sandbox]] to see if it might work. You could make the section template add its arguments in the correct places to make it work the way you want. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 18:00, 23 July 2014 (UTC)
