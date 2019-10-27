+++
title = "Template:Capability"
description = ""
date = 2010-08-19T13:32:38Z
aliases = []
[extra]
id = 7992
[taxonomies]
categories = []
tags = []
+++

{{infobox_begin}}'''{{PAGENAME}}'''<br/>
Capabilities characterize abstract requirements of tasks and abilities of languages and libraries.<br/>
<includeonly>[[Category:Capability]][[Special:Browse/{{FULLPAGENAME}}|Browse this capability's properties]]</includeonly><noinclude>[[:Category:Dummy Link|Browse this capability's properties]]</noinclude>.{{infobox_end}}<noinclude>
{{template}}</noinclude>
{{#ask:[[requires::{{PAGENAME}}]]
|format=template
|limit=20
|link=none
|template=required by
|offset=0
}}
{{#ask:[[provides::{{PAGENAME}}]]
|format=template
|limit=20
|link=none
|template=provided by
|offset=0
}}
{{#ask:[[allows::{{PAGENAME}}]]
|format=template
|limit=20
|link=none
|template=allowed by
|offset=0
}}
