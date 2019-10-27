+++
title = "Rosetta Code:Village Pump/Automated Infrastructure"
description = ""
date = 2010-11-28T17:27:06Z
aliases = []
[extra]
id = 6206
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Automated Infrastructure
|summary=A list of things that should be automated on Rosetta Code
}}
A list of things that should be automated on Rosetta Code, and some pseudocode for describing it. It's remarkable how similar most of these are. It makes me wonder if we couldn't create a scripting language to describe it, and have a bot (or, in the future, a MW extension) manage it by reading the script. --[[User:Short Circuit|Michael Mol]] 19:22, 28 February 2010 (UTC)

:Okay, I've got
<blockquote>
```bash
while true; do perl underbot_infrastructure; sleep 86400; done;
```
</blockquote>
:running in a <code>screen</code> session on <code>rosettacode.org</code>. The only problem is that it'll need to be restarted whenever the server is. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 13:05, 16 March 2010 (UTC)

=Creation of language pages=
#Watch the changelog for the User namespace
#Check user namespace pages for mylang template usage.
##For each mylang template item, check if the language page exists.
###For each mylang linked language page that doesn't exist, create it, with the content <nowiki>{{langauge}}{{stub}}</nowiki>, and note the user page it was observed from in the change description.
=Creation of Language User pages=
#Enumerate the contents of [[:Category:Programming Languages]] and the contents of [[:Category:Language users]].
##For every member of the former that doesn't have a corresponding entry in the latter, create the entry for the latter with the name of "Category:$Language User", and content of <nowiki>{{langgroup|$Language}}</nowiki>
=Creation of Unimpl Pages=
#Enumerate the contents of [[:Category:Programming Languages]] and [[:Category:Unimplemented tasks by language]].
##For every member of the former that doesn't have a corresponding entry in the latter, create the entry for the latter with the name of "$Reports:Reports:Tasks not implemented in $Language", and content of <nowiki>{{unimpl_Page|$Language}}</nowiki>.
=Creation of ENA Pages=
#Enumerate the contents of [[:Category:Programming Languages]] and [[:Category:Category:Examples needing attention]].
##For every member of the former that doesn't have a corresponding entry in the latter, create the entry for the latter with the name of "Category:$Language examples needing attention" and content of <nowiki>{{enacat_body|$Language}}</nowiki>.
=Creation of Language Implementation Pages=
#Enumerate the contents of [[:Category:Programming Languages]] and [[:Category:Language Implementations]].
##For every member of the former that doesn't have a corresponding entry in the latter, create the entry for the latter with the name of "Category:$Language Implementations", and content of <nowiki>{{implementation cat|$Language}}</nowiki>
