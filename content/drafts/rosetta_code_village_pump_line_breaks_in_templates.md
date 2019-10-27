+++
title = "Rosetta Code:Village Pump/Line breaks in templates"
description = ""
date = 2010-11-10T10:59:30Z
aliases = []
[extra]
id = 8666
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Line breaks in templates
|summary=Discussion of how to handle multiple templates next to each other.
}}
I've seen people go back and forth in [[Template:Works with]], [[Template:Libheader]], [[Template:Trans]], and other example-level templates on whether they want the line break in the template or on the page. We should just make a decision and go with it. I vote for one line break in the template (not a <nowiki><br/></nowiki>) so that people can type something like this:

```txt
{{works with|libraryx|2.1}}
{{works with|language Y|3.4+}}
{{works with|OS Z|8.2}}
```

and it with show up like this:

'''Works with''': libraryx version 2.1

'''Works with''': language Y version 3.4+

'''Works with''': OS Z version 8.2

If they type this:

```txt
{{works with|libraryx|2.1}}{{works with|language Y|3.4+}}{{works with|OS Z|8.2}}
```

it will show up like this:

'''Works with''': libraryx version 2.1
'''Works with''': language Y version 3.4+
'''Works with''': OS Z version 8.2

Thoughts? --[[User:Mwn3d|Mwn3d]] 21:00, 2 November 2010 (UTC)

: If you put a leading <nowiki>
</nowiki> in, you break (hah!) a lot of simple cases where there's only a single <nowiki>{{works with}}</nowiki> directly below a <nowiki>=={{header}}==</nowiki> line. That's the most important use case right now! While it would be nice to have fancy formatting between a sequence of these, it's far less important than making the basic case work right. Well, IMO anyway. –[[User:Dkf|Donal Fellows]] 11:03, 3 November 2010 (UTC)
:: Ok, what if we were to put a <nowiki><div></nowiki> around the content? Mwn3d's example would come out as:
<div>'''Works with''': libraryx version 2.1</div>
<div>'''Works with''': language Y version 3.4+</div>
<div>'''Works with''': OS Z version 8.2</div>
:: (block-level styling to the rescue...) It would also be trivial to add per-template CSS styling, for easier visual recognition of the data. --[[User:Short Circuit|Michael Mol]] 12:10, 3 November 2010 (UTC)

::: That doesn't work so well in the cases where someone's done <nowiki>{{works with|...}}
{{works with|...}}</nowiki> but I guess they're less common so we can just fix them. Reasonable trade-off. –[[User:Dkf|Donal Fellows]] 13:56, 3 November 2010 (UTC)
:::: Done. Also, I added 'examplemeta' CSS classes to each, and an additional CSS class per template. --[[User:Short Circuit|Michael Mol]] 16:19, 3 November 2010 (UTC)
:::: Damn, it took a lot of effort to go through and fix all the places in Tcl examples that were impacted on this. 440 times 30 seconds (average) is a best part of 4 hours… :-) I think I'm going to have to write some extra templates to cope with getting the layout I want in the case of complex multipart libraries. That's for the future though. –[[User:Dkf|Donal Fellows]] 10:16, 8 November 2010 (UTC)
::::: I saw you going through and cleaning up whitespace all over the place. I'd be careful about depending too much on the formatting of the metadata, though; it's subject to change. (Particularly as we move toward example-per-page) --[[User:Short Circuit|Michael Mol]] 13:58, 8 November 2010 (UTC)
:::::: It's now done (e.g., <nowiki>{{tcllib|math}}</nowiki> indicates that the math package of the tcllib library is used) though perhaps it's an area where other languages might want to do something similar. I've also taken steps to mark up the information in that template so it is reflected in the SMW tagging. This makes it possible to find all the examples where a particular tcllib package was used (a new capability; it couldn't be done before).
:::::: I know about the need to keep things synchronized. For now, I'll do it as a manual action. –[[User:Dkf|Donal Fellows]] 14:15, 9 November 2010 (UTC)
::::::: ... I really need to buy you a drink or something; that's a lot of nice cleanup work you've put in over the last few days. I wonder if {{tmpl|libheader}} could be adjusted for similar functionality. Say, to have it read "Uses '''component''' from '''library''', tacking on semantic properties in the process. Hm. I suppose that could probably just be put in a new template, {{tmpl|uses from}} --[[User:Short Circuit|Michael Mol]] 15:13, 9 November 2010 (UTC)
::::::: Oh, that's really cool! Now that RC has rebuilt all the semantic properties, I can do queries like [http://rosettacode.org/wiki/Special:SearchByProperty/Uses-20tcllib-20package/struct::list this] to list all the uses of a particular package. I like this semantic stuff; now that we've got some data, you can actually use it to find out interesting things… (BTW, what's the best way of writing queries as links? Maybe that's something for a different Village Pump page though.) –[[User:Dkf|Donal Fellows]] 22:44, 9 November 2010 (UTC)

You guys are awesome. Thanks for getting this fixed so quickly. --[[User:Mwn3d|Mwn3d]] 18:24, 3 November 2010 (UTC)
