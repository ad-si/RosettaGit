+++
title = "Help talk:Formatting"
description = ""
date = 2012-01-28T02:15:43Z
aliases = []
[extra]
id = 6815
[taxonomies]
categories = []
tags = []
+++


###  Display Title Bug ? 


I noticed in some references:

<nowiki>
[URL|word1 word2]
</nowiki>

That this showed in the text as a hyperlink with word2 (word1 was missing).  A space after the bar fixed it.  --[[User:Dgamey|Dgamey]] 21:34, 8 April 2010 (UTC)

: There is no bar. This wiki uses <code><nowiki>[URL word1 word2]</nowiki></code>. Only internal links have a bar: <code><nowiki>[[Internal link|Display title]]</nowiki></code>. This is different from some other wikis where all links have a bar. --[[User:Kernigh|Kernigh]] 21:28, 30 June 2011 (UTC)



###  Text Color? 


The help page points to Wikipedia help pages, but things that work in Wikpedia don't work here. How can you make some text a different color? Wikipedia documents this: <code><nowiki>{{color|red| this text is red}}</nowiki></code>, but here that syntax is interpreted as a template. Background: I'd like to manually color some code for which there is no GeShi definition.[[Special:Contributions/192.139.122.42|192.139.122.42]] 02:03, 28 January 2012 (UTC)
:To follow up to my own comment, here is one way to get color: using math! <math>{\color{Blue}x^2}+{\color{YellowOrange}2x}-{\color{OliveGreen}1}</math>. Any other ideas/clues? [[Special:Contributions/192.139.122.42|192.139.122.42]] 02:08, 28 January 2012 (UTC)
:: No wait, that is dumb since it produces an image. What am I thinking? :) [[Special:Contributions/192.139.122.42|192.139.122.42]] 02:12, 28 January 2012 (UTC)
::: <span style="color:#FF0000"> This text will be red </span> <span style="color:#0000FF"> and this blue</span>. span tags are the key. Bingo! [[Special:Contributions/192.139.122.42|192.139.122.42]] 02:15, 28 January 2012 (UTC)
