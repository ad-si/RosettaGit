+++
title = "Template:Documentation/core"
description = ""
date = 2010-10-11T15:23:50Z
aliases = []
[extra]
id = 7119
[taxonomies]
categories = []
tags = []
+++

<!--
  Start of green doc box
--><div id="template-documentation" class="template-documentation iezoomfix"><!--
  Add the heading at the top of the doc box:
-->{{#ifeq: {{{heading|¬}}} | <!--Defined but empty-->
| <!--"heading=", do nothing-->
| <div style="padding-bottom: 3px; border-bottom: 1px solid #aaa; margin-bottom: 1ex;">{{
  #if: {{{content|}}}
  | 
  | <!--Add the [edit][purge] or [create] links-->
    <span class="editsection plainlinks" id="doc_editlinks">{{
    #if: {{{doc exist|yes}}}
    | [[{{fullurl:{{{docpage|{{FULLPAGENAME}}/doc}}}|action=edit}} edit]] [{{purge|purge}}] 
    | <!--/doc doesn't exist-->
      [[{{fullurl:{{{docpage|{{FULLPAGENAME}}/doc}}}| action=edit&preload={{
        #ifeq: {{SUBJECTSPACE}} | {{ns:File}}
        | Template:Documentation/preload-filespace
        | Template:Documentation/preload
        }} }} create]]
    }}</span>
  }} <span style="{{#if: {{{heading-style|}}}
  | {{{heading-style|}}}
  | {{#ifeq: {{SUBJECTSPACE}} | {{ns:Template}}
    | font-weight: bold; font-size: 125%
    | font-size: 150%
    }}
  }}">{{#switch: {{{heading|¬}}}
  | ¬ =   
    <!--"heading" not defined in this or previous level-->
    {{#switch: {{SUBJECTSPACE}} 
    | {{ns:Template}} = [[Image:Template-info.svg|50px|alt=]] Template documentation
    | {{ns:File}} = Summary
    | #default = Documentation
    }}
  | #default = 
    <!--"heading" has data or is empty but defined-->
    {{{heading|}}}
  }}</span></div>
}}<!--
  Load the /doc content:
  Note: The line breaks between this comment and the if-case
  and between the if-case and the following div are necessary so 
  "
###  Headings 
" at the start and end of docs are interpreted.
-->
{{#if: {{{content|}}}
| {{{content|}}}
| {{#if: {{{doc exist|yes}}}
  | {{ {{{docpage|{{FULLPAGENAME}}/doc}}} }}
  }}
}}
<div style="clear: both;"></div><!--So right or left floating items don't stick out of the doc box.-->
</div><!--End of green doc box--><!--
  Link box below for the doc meta-data:
-->{{#if: 
  <!--Check if we should show the link box-->
  {{#ifeq: {{{link box|}}} | off
  |
  | {{{doc exist|yes}}}{{
    #switch: {{SUBJECTSPACE}}
    | {{ns:User}}
    | {{ns:Template}} = yes
    }}
  }}

| {{fmbox
  | id = documentation-meta-data
  | image = none
  | style = background-color: #ecfcf4;
  | textstyle = font-style: italic;
  | text = 
    {{#if: {{{link box|}}}
    | {{{link box}}}   <!--Use custom link box content-->
    | {{#if: {{{doc exist|yes}}}
      | <!--/doc exists, link to it-->
        The above [[Wikipedia:Template documentation|documentation]] is [[Wikipedia:Transclusion|transcluded]] from [[{{{docpage|{{FULLPAGENAME}}/doc}}}]]. <small style="font-style: normal">([{{fullurl:{{{docpage|{{FULLPAGENAME}}/doc}}}|action=edit}} edit] &#124; [{{fullurl:{{{docpage|{{FULLPAGENAME}}/doc}}}|action=history}} history])</small> 

      }}<!-- 
        Add links to /sandbox and /testcases when appropriate:
   -->{{#switch: {{SUBJECTSPACE}}
      | {{ns:User}}
      | {{ns:Template}} = 
        Editors can experiment in this template's {{
        #ifexist: {{{sandbox| {{FULLPAGENAME}}/sandbox }}}
        | [[{{{sandbox| {{FULLPAGENAME}}/sandbox }}}|sandbox]] <small style="font-style: normal">([{{fullurl: {{{sandbox| {{FULLPAGENAME}}/sandbox }}} | action=edit }} edit])</small> 
        | sandbox <small style="font-style: normal">([{{fullurl: {{{sandbox| {{FULLPAGENAME}}/sandbox }}} | action=edit&preload=Template:Documentation/preload-sandbox }} create])</small> 
        }} and {{
        #ifexist: {{{testcases| {{FULLPAGENAME}}/testcases }}}
        | [[{{{testcases| {{FULLPAGENAME}}/testcases }}}|testcases]] <small style="font-style: normal">([{{fullurl: {{{testcases| {{FULLPAGENAME}}/testcases }}} | action=edit }} edit])</small>
        | testcases <small style="font-style: normal">([{{fullurl: {{{testcases| {{FULLPAGENAME}}/testcases }}} | action=edit&preload=Template:Documentation/preload-testcases }} create])</small>
        }} pages. 

      }}<!--
        Show the cats and interwiki text, but not 
        if "content" fed or "docname fed" since then it is 
        unclear where to add the cats and interwikis.
   -->{{#if: {{{content|}}} {{{docname fed|}}}
      | 
      | Please add categories and interwikis to the [[{{{docpage|{{FULLPAGENAME}}/doc}}}|/doc]] subpage.
      }}<!--
        Show the "Subpages" link:
   -->{{#switch: {{SUBJECTSPACE}}
      | {{ns:File}} =   <!--Don't show it-->
      | {{ns:Template}} = &#32;[[Special:PrefixIndex/{{{template page|{{FULLPAGENAME}}}}}/|Subpages of this template]].
      | #default = &#32;[[Special:PrefixIndex/{{{template page|{{FULLPAGENAME}}}}}/|Subpages of this page]].
      }}
    }}{{#ifexist:{{FULLPAGENAME}}/Print
     |</br>A [[Help:Books/for experts#Improving the book layout|print version]] of this template exists at [[/Print]]. If you make a change to this template, please update the print version as well.[[Category:Templates with print versions]]
    }}
  }}
}}<!--End link box--><!--

  Detect and report strange usage:
-->{{#if:

  <!--Check if {{documentation}} is transcluded 
      on a /doc or /testcases page-->
  {{#switch: {{SUBPAGENAME}}
  | doc
  | testcases = strange
  }}
  <!--More checks can be added here, just return anything
      to make the surrounding if-case trigger-->

| <includeonly>[[Category:Wikipedia pages with strange ((documentation)) usage|{{main other|Main:}}{{FULLPAGENAME}}]]<!-- Sort on namespace --></includeonly>

}}<noinclude>

Copied from WP in support of Reflist

{{pp-template}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
