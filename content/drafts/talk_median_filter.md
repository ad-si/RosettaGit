+++
title = "Talk:Median filter"
description = ""
date = 2009-11-27T11:40:53Z
aliases = []
[extra]
id = 3255
[taxonomies]
categories = []
tags = []
+++

Articles in this category use code from other articles. It would be nice to provide full program samples somewhere. Maybe in the article in expandable boxes. Is there a way to do it? Any ideas? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:55, 14 December 2008 (UTC)

:It seems to me it would be a good idea &mdash;expecially for the Raster Graphics category; but maybe linking would be enough? Don't know. --[[User:ShinTakezou|ShinTakezou]] 22:59, 14 December 2008 (UTC)

Seems like here - and on the linked Wikipedia page - nothing is said about how to get a median value of colored pixels, i.e., those having more than one channel. Converting pixels to 1-dimensional gray pixels, sorting and then applying the sorted order to colored pixels could be a possible approache. [[User:Avmich|Avmich]] 02:04, 27 November 2009 (UTC)
: There's several techniques, but the simplest is to just do each RGB channel independently. It's probably<sup><nowiki>[unsupported assertion!]</nowiki></sup> better to convert pixels to HSV first and then apply the filter to that before converting back (since HSV models the human visual system better than RGB), but that's more expensive. –[[User:Dkf|Donal Fellows]] 11:40, 27 November 2009 (UTC)
