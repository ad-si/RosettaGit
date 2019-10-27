+++
title = "Template:Navbox"
description = ""
date = 2010-10-11T15:51:32Z
aliases = []
[extra]
id = 7126
[taxonomies]
categories = []
tags = []
+++

<!--

Please do not edit without discussion first as this is a VERY complex template.

-->{{#switch:{{{border|{{{1|}}}}}}|subgroup|child=</div>|none=|#default=<table class="navbox {{{bodyclass|}}}" cellspacing="0" <!--
 -->style="{{{bodystyle|}}};{{{style|}}}"><tr><td style="padding:2px;">}}<!--

--><table cellspacing="0" class="nowraplinks {{#if:{{{title|}}}|{{#switch:{{{state|}}}|plain|off=|<!--
 -->#default=collapsible {{#if:{{{state|}}}|{{{state}}}|autocollapse}}}}}} {{#switch:{{{border|{{{1|}}}}}}|<!--
 -->subgroup|child|none=navbox-subgroup" style="width:100%;{{{bodystyle|}}};{{{style|}}}|<!--
 -->#default=" style="width:100%;background:transparent;color:inherit}};{{{innerstyle|}}};"><!--



---Title and Navbar---
-->{{#if:{{{title|}}}|<tr>{{#if:{{{titlegroup|}}}|<!--
 --><td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{titlegroupstyle|}}}">{{{titlegroup|}}}</td><!--
 --><th style="border-left:2px solid #fdfdfd;width:100%;|<th style="}}{{{basestyle|}}};{{{titlestyle|}}}" <!--
 -->colspan={{#expr:2{{#if:{{{imageleft|}}}|+1}}{{#if:{{{image|}}}|+1}}{{#if:{{{titlegroup|}}}|-1}}}} <!--
 -->class="navbox-title"><!--

-->{{#if:{{#switch:{{{navbar|}}}|plain|off=1}}<!--
 -->{{#if:{{{name|}}}||{{#switch:{{{border|{{{1|}}}}}}|subgroup|child|none=1}}}}|<!--
 -->{{#ifeq:{{{navbar|}}}|off|{{#ifeq:{{{state|}}}|plain|<div style="float:right;width:6em;"> </div>}}|<!--
 -->{{#ifeq:{{{state|}}}|plain||<div style="float:left; width:6em;text-align:left;"> </div>}}}}|<!--
 --><div style="float:left; width:6em;text-align:left;"><!--
 -->{{Navbar|{{{name}}}|fontstyle={{{basestyle|}}};{{{titlestyle|}}};border:none;|mini=1}}<!--
 --></div>{{#ifeq:{{{state|}}}|plain|<div style="float:right;width:6em;"> </div>}}}}<!--

 --><span class="{{{titleclass|}}}" style="font-size:{{#switch:{{{border|{{{1|}}}}}}|subgroup|child|none=100|#default=110}}%;"><!--
 -->{{{title}}}</span></th></tr>}}<!--



---Above---
-->{{#if:{{{above|}}}|<!--
 -->{{#if:{{{title|}}}|<tr style="height:2px;"><td></td></tr>}}<!--
 --><tr><td class="navbox-abovebelow" style="{{{basestyle|}}};{{{abovestyle|}}}" <!--
 -->colspan="{{#expr:2{{#if:{{{imageleft|}}}|+1}}{{#if:{{{image|}}}|+1}}}}">{{{above}}}</td></tr>}}<!--



---Body---

---First group/list and images---
-->{{#if:{{{list1|}}}|{{#if:{{{title|}}}{{{above|}}}|<tr style="height:2px;"><td></td></tr>}}<tr><!--

-->{{#if:{{{imageleft|}}}|<!--
 --><td style="width:0%;padding:0px 2px 0px 0px;{{{imageleftstyle|}}}" <!--
 -->rowspan={{#expr:1{{#if:{{{list2|}}}|+2}}{{#if:{{{list3|}}}|+2}}{{#if:{{{list4|}}}|+2}}<!--
 -->{{#if:{{{list5|}}}|+2}}{{#if:{{{list6|}}}|+2}}{{#if:{{{list7|}}}|+2}}{{#if:{{{list8|}}}|+2}}<!--
 -->{{#if:{{{list9|}}}|+2}}{{#if:{{{list10|}}}|+2}}{{#if:{{{list11|}}}|+2}}{{#if:{{{list12|}}}|+2}}<!--
 -->{{#if:{{{list13|}}}|+2}}{{#if:{{{list14|}}}|+2}}{{#if:{{{list15|}}}|+2}}{{#if:{{{list16|}}}|+2}}<!--
 -->{{#if:{{{list17|}}}|+2}}{{#if:{{{list18|}}}|+2}}{{#if:{{{list19|}}}|+2}}{{#if:{{{list20|}}}|+2}}}}><!--
 -->{{{imageleft}}}</td>}}<!--

 -->{{#if:{{{group1|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group1style|}}}"><!--
 -->{{{group1}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list1style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{list1padding|{{{listpadding|0em 0.25em}}}}}}">
{{{list1}}}</div></td><!--

-->{{#if:{{{image|}}}|<!--
 --><td style="width:0%;padding:0px 0px 0px 2px;{{{imagestyle|}}}" <!--
 -->rowspan={{#expr:1{{#if:{{{list2|}}}|+2}}{{#if:{{{list3|}}}|+2}}{{#if:{{{list4|}}}|+2}}<!--
 -->{{#if:{{{list5|}}}|+2}}{{#if:{{{list6|}}}|+2}}{{#if:{{{list7|}}}|+2}}{{#if:{{{list8|}}}|+2}}<!--
 -->{{#if:{{{list9|}}}|+2}}{{#if:{{{list10|}}}|+2}}{{#if:{{{list11|}}}|+2}}{{#if:{{{list12|}}}|+2}}<!--
 -->{{#if:{{{list13|}}}|+2}}{{#if:{{{list14|}}}|+2}}{{#if:{{{list15|}}}|+2}}{{#if:{{{list16|}}}|+2}}<!--
 -->{{#if:{{{list17|}}}|+2}}{{#if:{{{list18|}}}|+2}}{{#if:{{{list19|}}}|+2}}{{#if:{{{list20|}}}|+2}}}}><!--
 -->{{{image}}}</td>}}<!--

--></tr>}}<!--



---Remaining groups/lists---

-->{{#if:{{{list2|}}}|<!--
 -->{{#if:{{{title|}}}{{{above|}}}{{{list1|}}}|<tr style="height:2px"><td></td></tr>}}<tr><!--
 -->{{#if:{{{group2|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group2style|}}}"><!--
 -->{{{group2}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list2style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list2}}}</div></td></tr>}}<!--

-->{{#if:{{{list3|}}}|<!--
 -->{{#if:{{{title|}}}{{{above|}}}{{{list1|}}}{{{list2|}}}|<tr style="height:2px"><td></td></tr>}}<tr><!--
 -->{{#if:{{{group3|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group3style|}}}"><!--
 -->{{{group3}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list3style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list3}}}</div></td></tr>}}<!--

-->{{#if:{{{list4|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group4|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group4style|}}}"><!--
 -->{{{group4}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list4style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list4}}}</div></td></tr>}}<!--

-->{{#if:{{{list5|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group5|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group5style|}}}"><!--
 -->{{{group5}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list5style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list5}}}</div></td></tr>}}<!--

-->{{#if:{{{list6|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group6|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group6style|}}}"><!--
 -->{{{group6}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list6style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list6}}}</div></td></tr>}}<!--

-->{{#if:{{{list7|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group7|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group7style|}}}"><!--
 -->{{{group7}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list7style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list7}}}</div></td></tr>}}<!--

-->{{#if:{{{list8|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group8|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group8style|}}}"><!--
 -->{{{group8}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list8style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list8}}}</div></td></tr>}}<!--

-->{{#if:{{{list9|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group9|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group9style|}}}"><!--
 -->{{{group9}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list9style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list9}}}</div></td></tr>}}<!--

-->{{#if:{{{list10|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group10|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group10style|}}}"><!--
 -->{{{group10}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list10style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list10}}}</div></td></tr>}}<!--

-->{{#if:{{{list11|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group11|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group11style|}}}"><!--
 -->{{{group11}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list11style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list11}}}</div></td></tr>}}<!--

-->{{#if:{{{list12|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group12|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group12style|}}}"><!--
 -->{{{group12}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list12style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list12}}}</div></td></tr>}}<!--

-->{{#if:{{{list13|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group13|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group13style|}}}"><!--
 -->{{{group13}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list13style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list13}}}</div></td></tr>}}<!--

-->{{#if:{{{list14|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group14|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group14style|}}}"><!--
 -->{{{group14}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list14style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list14}}}</div></td></tr>}}<!--

-->{{#if:{{{list15|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group15|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group15style|}}}"><!--
 -->{{{group15}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list15style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list15}}}</div></td></tr>}}<!--

-->{{#if:{{{list16|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group16|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group16style|}}}"><!--
 -->{{{group16}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list16style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list16}}}</div></td></tr>}}<!--

-->{{#if:{{{list17|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group17|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group17style|}}}"><!--
 -->{{{group17}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list17style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list17}}}</div></td></tr>}}<!--

-->{{#if:{{{list18|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group18|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group18style|}}}"><!--
 -->{{{group18}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list18style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list18}}}</div></td></tr>}}<!--

-->{{#if:{{{list19|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group19|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group19style|}}}"><!--
 -->{{{group19}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{oddstyle|}}};{{{list19style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|even|{{{evenodd|odd}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list19}}}</div></td></tr>}}<!--

-->{{#if:{{{list20|}}}|<!--
 --><tr style="height:2px"><td></td></tr><tr><!--
 -->{{#if:{{{group20|}}}|<td class="navbox-group" style="{{{basestyle|}}};{{{groupstyle|}}};{{{group20style|}}}"><!--
 -->{{{group20}}}</td><td style="text-align:left;border-left-width:2px;border-left-style:solid;|<td colspan=2 style="}}<!--
 -->width:100%;padding:0px;{{{liststyle|}}};{{{evenstyle|}}};{{{list20style|}}}" <!--
 -->class="navbox-list navbox-{{#ifeq:{{{evenodd|}}}|swap|odd|{{{evenodd|even}}}}}"><!--
 --><div style="padding:{{{listpadding|0em 0.25em}}}">
{{{list20}}}</div></td></tr>}}<!--


---Below---
-->{{#if:{{{below|}}}|<!--
 -->{{#if:{{{title|}}}{{{above|}}}{{{list1|}}}{{{list2|}}}{{{list3|}}}|<tr style="height:2px;"><td></td></tr>}}<!--
 --><tr><td class="navbox-abovebelow" style="{{{basestyle|}}};{{{belowstyle|}}}" <!--
 -->colspan="{{#expr:2{{#if:{{{imageleft|}}}|+1}}{{#if:{{{image|}}}|+1}}}}">{{{below}}}</td></tr>}}<!--


--></table>{{#switch:{{{border|{{{1|}}}}}}|subgroup|child=<div>|none=|#default=</td></tr></table>}}<!--

--><noinclude>
Copied from WP for Reflist

{{pp-template|small=yes}}

{{documentation}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
{{template}}</noinclude>
