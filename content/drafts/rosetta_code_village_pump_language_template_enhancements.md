+++
title = "Rosetta Code:Village Pump/Language template enhancements"
description = ""
date = 2010-11-10T02:03:19Z
aliases = []
[extra]
id = 3374
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Language template enhancements
|summary=Adding features to {{tmpl|language}}
}}
What kinds of things would be useful to add to the language template?  Official website?  Largest IRC support channel? --[[User:Short Circuit|Short Circuit]] 02:48, 14 February 2009 (UTC)
:Official website would be cool. You should use named arguments when you add these things. --[[User:Mwn3d|Mwn3d]] 03:06, 14 February 2009 (UTC)
:How about "syntax highlighting tag" so that we can standardize unimplemented languages and let people know what to use for supported ones? --[[User:Mwn3d|Mwn3d]] 22:35, 14 February 2009 (UTC)

=Language template beta=
I created [[Template:Language_beta]], so folks can experiment with the language template and make it better.  Make changes and additions there, and we'll move over whatever works well over to the main template.  Think of the beta template as a staging ground for ideas and features, to see what works well.

Don't be obsessed with keeping information in the language template confined to the boxes on the right; If it warrants space in the main body of the page, put it there.  The idea is to come up with way of uniformly presenting all types of information that should be available for any given language.

Don't be terrified of including templates within templates, either.  If there's a segment or section you want to add to the language template, create a template for that particular section, and do any additional nesting you might need in that template.  If it makes the information easier to organize and edit, that's not a problem.  But don't go overboard; If the maintenance of the segment is a pain because of deeply nested templates, it will eventually have to be removed or replaced when a change needs to be made.--[[User:Short Circuit|Short Circuit]] 02:38, 15 May 2009 (UTC)

=Language template wishlist=
* Official site
* Appropriate HOPL link (There's already a HOPL link, but most RC template links don't take you where you would have wanted to go.)
* Ancillary/support sites, forums and appropriate newsgroups
* Helpful literature (I'm hoping to help fund RC's server and service expenses based referral/affiliate links, so be aware that this might get changed to that purpose.)
* Language tag (If someone has a good idea how to do this, I'm all ears.  It's non-trivial to map backwards from a language name to the appropriate GeSHi language file, as some languages have duplicate files, and most languages on Rosetta Code aren't supported at all.)
** The simplest solution would be to just have an optional list of language tags passed as optional template argument. That is, you could e.g. include the template in the Java category as <nowiki>{{language|java, java5}}</nowiki>, and the result would be that a line "Language tags: java, java5" appeared in the box. --[[User:Ce|Ce]] 07:02, 15 May 2009 (UTC)
:These can mostly be done with named parameters. The official site and language tags would be easiest. After all the parameters are passed in they can be added to a more spiffy looking div box with <nowiki><small> text</nowiki> showing all of the parameters like this:
<div class="infobox" style="width: 2in">
<big>'''Language name'''</big>

Text about the language.

<small>Official site: [http://example.com link]

Type checking: static/dynamic

Garbage collected: yes/no

Lang tags: option1, option2

More features...</small>
</div>
:They could all be conditional. The language template code will probably look ugly, but the div box will look sweet.--[[User:Mwn3d|Mwn3d]] 15:33, 5 June 2009 (UTC)
:I put the idea into the language beta template with test values. [[Template:Language beta|Check it out]]. --[[User:Mwn3d|Mwn3d]] 17:35, 5 June 2009 (UTC)
=Citations=
I just added [[Template:Cite]] which could be used for citations in anything, but in particular I wanted to use it for citations in language template parameters. I'm thinking about adding "expresscite", "gccite", and other similar (optional) parameters to the template to allow people to link to their sources right next to the information. I'm worried that it will not be used much and that the language template is blowing up now. Thoughts? --[[User:Mwn3d|Mwn3d]] 17:30, 12 April 2010 (UTC)
: Regarding the template "blowing up," What about structuring [[Template:Language]] in a fashion similar to [[Template:mylang]]? That would allow the addition of arbitrary key-value pairs with a basic member template, and more specific ones for specialized keys like standards, paradigms, presence of garbage collection, etc. I'll admit I don't muck with the template's parameters much because I find the ParserFunctions code generally daunting. Breaking that apart would make it easier to add/remove components to the description block. --[[User:Short Circuit|Michael Mol]] 19:13, 12 April 2010 (UTC)
: I created [[Template:Language/Start]], [[Template:Language/End]], [[Template:Language/Property/Generic]] and [[Template:Language/Property/URL]]. Obviously, that doesn't come close to representing the full set of features that [[Template:Language]] currently manages, but it's a start, it may be easier to use, and it's easily extensible. Thoughts? --[[User:Short Circuit|Michael Mol]] 04:49, 13 April 2010 (UTC)
