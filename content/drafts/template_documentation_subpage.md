+++
title = "Template:Documentation subpage"
description = ""
date = 2010-10-11T15:26:34Z
aliases = []
[extra]
id = 7124
[taxonomies]
categories = []
tags = []
+++

{{#if:{{{Original-recipe|}}}{{{Orig|}}}
|<!-- reconstruction of original  Template:Template doc page pattern (now the redirected to " Template:Documentation subpage") template... ca mid-November 2007 -->
<table  class="messagebox" style="line-height:1.1em;" style=" background:#f9f9b7;">
<tr> <td rowspan="3" style="width:60px;text-align:center;" > [[Image:Edit-paste.svg|40px]]</td>
<td> This is the [[Wikipedia:Template documentation|template documentation]]<!--
--> page for [[{{NAMESPACE}}:{{{1|{{BASEPAGENAME}}}}}]].</td></tr>
<tr><td><small>This page may not be intended to be viewed directly. <br/
>Links using [[Help:Variable|variable]]s may appear broken; do not replace these with [[hardcoded]] page names or URLs.</small></td></tr>
</table>{{#if:{{{inhib|x}}}{{{inhibit|}}}|<!-- skip --->|<includeonly>[[Category:Template documentation|{{PAGENAME}}]]</includeonly>
}}<!-- Please retain the above original template... 

There are templates formulated to [[WP:DPP]] that need this.

Of course, if you all want to start updating all the interwiki exported templates, go on making things more incompatible... queries to User:Fabartus.

--->
|<includeonly>{{#ifeq: {{lc:{{SUBPAGENAME}}}} | {{{override|doc}}}
  | <!-- doc page -->
</includeonly>{{
    #ifeq: {{{doc-notice|show}}} | show
    | {{mbox
      | type = notice
      | image = [[File:Edit-copy green.svg|40px]]
      | text = 
'''This is a [[Wikipedia:Template documentation|documentation]] [[Wikipedia:Subpages|subpage]] for {{{1|[[:{{SUBJECTSPACE}}:{{BASEPAGENAME}}]]}}}''' <small>(see that page for the {{ #if: {{{text1|}}} | {{{text1}}} | {{ #ifeq: {{SUBJECTSPACE}} | {{ns:User}} | {{lc:{{SUBJECTSPACE}}}} template | {{ #if: {{SUBJECTSPACE}} | {{lc:{{SUBJECTSPACE}}}} | article }}}}}} itself)</small>.<br />It contains usage information, [[Wikipedia:Categorization|categories]], [[Help:Interlanguage links|interlanguage links]] and other content that is not part of the original {{ #if: {{{text2|}}} | {{{text2}}} | {{ #if: {{{text1|}}} | {{{text1}}} | {{ #ifeq: {{SUBJECTSPACE}} | {{ns:User}} | {{lc:{{SUBJECTSPACE}}}} template page | {{ #if: {{SUBJECTSPACE}} |{{lc:{{SUBJECTSPACE}}}} page|article}}}}}}}}. 
      }}
    }}{{DEFAULTSORT:{{{defaultsort|{{PAGENAME}}}}}}}{{
    #if: {{{inhibit|}}}
    | <!-- skip -->
    | [[Category:{{
      #if: {{SUBJECTSPACE}}
      | {{SUBJECTSPACE}}
      | Article
    }} documentation<noinclude>| </noinclude>]]
  }}<includeonly>
| <!-- if not on a /doc subpage, do nothing -->
}}</includeonly><noinclude>
Copied from WP for Reflist

{{documentation}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
}}
