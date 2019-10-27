+++
title = "Template:Template other"
description = ""
date = 2010-10-11T19:41:55Z
aliases = []
[extra]
id = 7117
[taxonomies]
categories = []
tags = []
+++

{{#switch:
  <!--If no or empty "demospace" parameter then detect namespace-->
  {{#if:{{{demospace|}}}
  | {{lc: {{{demospace}}} }}    <!--Use lower case "demospace"-->
  | {{#ifeq:{{NAMESPACE}}|{{ns:Template}}
    | template
    | other
    }}
  }}
| template = {{{1|}}}
| other
| #default = {{{2|}}}
}}<!--End switch--><noinclude>
Taken from Wikipedia for Reflist

{{pp-template}}
{{documentation}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
