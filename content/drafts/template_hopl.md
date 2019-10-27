+++
title = "Template:HOPL"
description = ""
date = 2015-12-16T21:48:36Z
aliases = []
[extra]
id = 2670
[taxonomies]
categories = []
tags = []
+++

{{#if: {{{id|}}}
|[http://hopl.info/showlanguage.prx?exp={{urlencode: {{{id}}} }} {{{1| {{PAGENAME}} }}} on the HOPL]
|[http://hopl.info/findlanguages2.prx?language={{urlencode: {{{1| {{PAGENAME}} }}} }}&which=byname {{{1| {{PAGENAME}} }}} on the HOPL]
}}<noinclude>

== Usage ==
* <nowiki>{{HOPL}}</nowiki> creates a link to the HOPL search for the title of the page it is placed on.
* <nowiki>{{HOPL|Ruby}}</nowiki> searches for "Ruby", not the page title.
* <nowiki>{{HOPL|J|id=1558}} or {{HOPL|id=1558}}</nowiki> creates a direct link to language 1558 "J", bypassing the search page.

{{template}}</noinclude>
