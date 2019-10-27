+++
title = "Template:Mylang"
description = ""
date = 2017-04-07T05:28:01Z
aliases = []
[extra]
id = 2764
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{mylangbegin}}<!-- for demonstration purposes -->
</noinclude>|-
| style="clear:both;background: #6ef7a7;padding:0 4px 0 4px;" | '''[[{{{1}}}|{{{3|{{{1}}}}}}]]'''
| style="clear:both;background: #c5fcdc;padding:0 4px 0 4px;" |'''{{#if: {{{2|}}}|{{{2}}}|---}}'''
[[Category:{{{1}}} User|{{PAGENAME}}]]{{#set:knows language={{{1}}}}}<noinclude>
{{mylangend}}
'''Usage''':
<nowiki>{{mylang|language|level|altname}}</nowiki>

Where "language" is the name of the programming language you use, and "level" (optional) is something like "beginner", "novice", "advanced", etc. "altname", if present, is a display name for the language (for C#, F#, and other similar languages that cannot have their proper name as a link). Try to use the correct spelling and case (spelling and case everyone else uses) for the language. If you are the first person to declare that you can program in a particular language, please create the user group page (link at the bottom of your user page) with [[Template:langgroup|<nowiki>{{langgroup|language}}</nowiki>]].
Feel free to list programming languages you use, even if they are not yet listed on our list of languages (see the [[:Category:Programming Languages | "Languages"]] link in the left sidebar).

'''Be sure to put this between {{Tl|mylangbegin}} and {{Tl|mylangend}} templates or [[Rosetta_Code:MylangBadExample|things will just look weird]].'''

{{template}}</noinclude>
