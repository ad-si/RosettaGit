+++
title = "Template:BookLink/Testing"
description = ""
date = 2013-03-30T16:03:57Z
aliases = []
[extra]
id = 8601
[taxonomies]
categories = []
tags = []
+++

<includeonly>

{{#if:{{{2|}}}|<amazon keywords="{{{1}}}"><span class="plainlinks">''[%url% {{{2}}}]''</span></amazon>|<amazon keywords="{{{1}}}"><span class="plainlinks">''[%url% %title%]'' by '''%author%'''</span></amazon>}}</includeonly><noinclude>{{template}}

{{BookLink/Testing|9780596000271|camel book}}
{{BookLink/Testing|9780596000271}}

[[Category:Possible Redundant Templates]]
