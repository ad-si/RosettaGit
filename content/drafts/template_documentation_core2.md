+++
title = "Template:Documentation/core2"
description = ""
date = 2010-10-11T15:00:18Z
aliases = []
[extra]
id = 7118
[taxonomies]
categories = []
tags = []
+++

{{documentation/core
| heading = {{{heading|¬}}}   <!--Empty but defined means no header-->
| heading-style = {{{heading-style|}}}
| content = {{{content|}}}
| link box = {{{link box|}}}   <!--So "link box=off" works-->

| docpage = 
  {{#if: {{{1|}}}
  | {{{1|}}}
  | {{{docspace|{{NAMESPACE}}}}}:{{{template page|{{PAGENAME}}}}}/doc
  }}
| doc exist = 
  {{#ifexist: 
    {{#if: {{{1|}}}
    | {{{1|}}}   <!--Other docname fed-->
    | {{{docspace|{{NAMESPACE}}}}}:{{{template page|{{PAGENAME}}}}}/doc
    }}
  | yes
  }}
| docname fed =
  {{#if: {{{1|}}}
  | yes
  }}

| sandbox = 
  {{{docspace|{{NAMESPACE}}}}}:{{{template page|{{PAGENAME}}}}}/sandbox
| testcases = 
  {{{docspace|{{NAMESPACE}}}}}:{{{template page|{{PAGENAME}}}}}/testcases

| template page = 
  {{NAMESPACE}}:{{{template page|{{PAGENAME}}}}}

}}<noinclude>

Copied from WP in support of Reflist

{{pp-template}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
