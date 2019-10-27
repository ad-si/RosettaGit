+++
title = "Template:Language/InlineSemantics"
description = ""
date = 2013-03-30T16:19:56Z
aliases = []
[extra]
id = 8517
[taxonomies]
categories = []
tags = []
+++

<includeonly>'''Sampling of Users:'''<br/>
{{#ask:[[knows language::{{{1}}}]]
|format=list
|limit=5
|offset=0
|default=''(none [[Template:Mylang|registered]])''
}}<br/>
'''Sampling of Implementations:'''<br/>
{{#ask:[[implementation of::{{{1}}}]]
|format=list
|limit=5
|offset=0
|default=''(none [[Template:implementation|documented]])''
}}<br/>
'''Sampling of Broken Examples:'''<br/>
{{#ask:[[example requires attention::{{{1}}}]]
|format=list
|limit=3
|offset=0
|default=''(none [[Template:incorrect|flagged]])''
}}</includeonly><noinclude>

Here's what the template looks like, when applied to Perl, within an infobox:
<blockquote>{{infobox_begin}}{{Language/InlineSemantics|Perl}}{{infobox_end}}</blockquote>

{{template}}
[[Category:Possible Redundant Templates]]
