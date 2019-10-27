+++
title = "Rosetta Code:Village Pump/Renaming a Language"
description = ""
date = 2015-09-02T17:10:11Z
aliases = []
[extra]
id = 19553
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Renaming a Language
|summary=Questions about how to rename a language on the site
}}
Protium has changed its name to Peloton. How do I effect this change on RC? [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 10:23, 2 September 2015 (UTC)
:The basic process would be:
:#Move <nowiki>[[Category:old name]] to [[Category:new name]]</nowiki> (and move the talk page and leave behind redirects)
:#Redirect <nowiki>[[new name]] to [[Category:new name]]</nowiki>
:#Change <nowiki>[[old name]] to redirect to [[Category:new name]]</nowiki> (this is to get rid of double redirects)
:#Go through all of the pages in <nowiki>[[Category:old name]] and change them so that they are in [[Category:new name]]</nowiki> (for tasks this means changing the language in the header templates)
:You might be able to script that last one using the wiki APIs. Unfortunately this will be a long process if there are a lot of tasks using the old name. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:10, 2 September 2015 (UTC)
