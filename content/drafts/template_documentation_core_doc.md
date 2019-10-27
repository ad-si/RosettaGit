+++
title = "Template:Documentation/core/doc"
description = ""
date = 2010-10-11T15:58:24Z
aliases = []
[extra]
id = 7123
[taxonomies]
categories = []
tags = []
+++

Copied from WP for Reflist

{{Documentation subpage}}
<!-- PLEASE ADD CATEGORIES AND INTERWIKIS AT THE BOTTOM OF THIS PAGE -->

This is the {{tl|documentation/core}} sub-template.

Do not use this template directly, use {{tl|documentation}} instead.

{{tlf|documentation}} calls {{tl|documentation/core2}} which in turn calls this template. This template holds most of the code for {{tlf|documentation}}, while {{tlf|documentation}} and {{tlf|documentation/core2}} do parameter preprocessing. Thus simplifying the code.


###  Technical details 


This sub-template currently expects these parameters:


```txt

{{documentation/core
| heading = {{{heading|¬}}}   <!--Note that the "¬" is necessary 
     so we can detect the difference between empty and undefined-->
| heading-style = {{{heading-style|}}}
| content = {{{content|}}}    <!--Text instead of a /doc page-->
| link box = {{{link box|}}}  <!--So "link box=off" works-->

| docpage =     <!--Full pagename of the doc page-->
| doc exist =   <!--"yes" if the doc page exists, empty string if not-->
| docname fed = <!--"yes" if a docname was manually fed-->

| sandbox =     <!--Full pagename of the /sandbox-->
| testcases =   <!--Full pagename of the /testcases-->

| template page =   <!--Full pagename where the {{documentation}}
    template is placed, but without ending /sandbox or /testcases.
    Note: Unfortunately might not be the "correct" namespace if 
    the template is in subject space and the {{documentation}} 
    template is in talk space or the other way around.-->
}}

```


For more documentation see {{tl|documentation}}.

<includeonly>
<!-- CATEGORIES AND INTERWIKIS HERE, THANKS -->
[[Category:Template documentation]]

</includeonly>
<noinclude>{{template}}</noinclude>
