+++
title = "Template:Documentation"
description = ""
date = 2010-10-11T14:58:51Z
aliases = []
[extra]
id = 7115
[taxonomies]
categories = []
tags = []
+++

<noinclude>
Copied from WP to support Reflist.
</noinclude><!--
  Automatically add {{template sandbox notice}} when on a /sandbox page.
-->{{#ifeq: {{SUBPAGENAME}} | sandbox
| <div style="clear: both;"></div>{{template sandbox notice}}
}}<!--
  Automatically add {{pp-template}} to protected templates.
-->{{template other
| {{#ifeq: {{PROTECTIONLEVEL:move}} | sysop
  | {{pp-template}}
  | {{#if: {{PROTECTIONLEVEL:edit}}
    | {{pp-template}}
    | <!--Not protected, or only semi-move-protected-->
    }}
  }}
}}<!--
  Start of green doc box.
-->{{documentation/core2
| heading = {{{heading|¬}}}   <!--Empty but defined means no header-->
| heading-style = {{{heading-style|}}}
| content = {{{content|}}}
| link box = {{{link box|}}}   <!--So "link box=off" works-->

<!--Some namespaces must have the /doc, /sandbox and /testcases 
    in talk space-->
| docspace =
  {{#switch: {{SUBJECTSPACE}}
  | {{ns:0}}
  | {{ns:File}}
  | {{ns:MediaWiki}}
  | {{ns:Category}} = {{TALKSPACE}}
  | #default = {{SUBJECTSPACE}}
  }}

| 1 = {{{1|}}}   <!--Other docname, if fed-->

<!--The namespace is added in /core2-->
| template page = 
  {{#switch: {{SUBPAGENAME}}
  | sandbox
  | testcases = {{BASEPAGENAME}}
  | #default = {{PAGENAME}}
  }}

}}<!--End of green doc box--><noinclude>

<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
