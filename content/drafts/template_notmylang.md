+++
title = "Template:Notmylang"
description = ""
date = 2014-09-14T21:48:10Z
aliases = []
[extra]
id = 2884
[taxonomies]
categories = []
tags = []
+++

|-
| style="clear:both;background: #D8C090;padding:0 4px 0 4px;" | '''[[:Category:{{{1}}}|{{{3|{{{1}}}}}}]]'''
| style="clear:both;background: #F0D8A8;padding:0 4px 0 4px;" |'''{{#if: {{{2|}}}|{{{2}}}|---}}'''
<noinclude><br clear="all">
'''Usage''':
<nowiki>{{notmylang|language|reason|altname}}</nowiki>

Where "language" is the name of the programming language you use, and "reason" (optional) is something short. "altname", if present, is a display name for the language (for C#, F#, and other similar languages that cannot have their proper name as a link). Try to use the correct spelling and case (spelling and case everyone else uses) for the language. If you are the first person to declare that you can program in a particular language, please create the user group page (link at the bottom of your user page) with <nowiki>{{langgroup|language}}</nowiki>.

Be sure to put this between <nowiki>{{notmylangbegin}} and {{notmylangend}}</nowiki> templates or things will just look weird.

{{template}}</noinclude>
