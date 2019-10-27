+++
title = "Template:Citation/make link"
description = ""
date = 2011-01-24T09:15:21Z
aliases = []
[extra]
id = 9183
[taxonomies]
categories = []
tags = []
+++

{{#if:{{{1|}}}
 |[{{{1}}} {{{2}}}]
 |{{{2}}}
}}<noinclude><!--
  Code notes (here so that people /read/ it)
  1. Parameter #2 is always nonempty when called from {{Citation/core}}.
  2. It's up to {{Citation/core}} to stop italicized "'foo' & 'bar'" from becoming
     "'''foo' & 'bar'''". Citation/core does this by having a <nowiki></nowiki>
     at both beginning and end of the call to this template. Since this is only
     needed once, it's more efficient to do it in {{Citation/core}} than here.
-->{{documentation}}</noinclude>
