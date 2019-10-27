+++
title = "Rosetta Code:Village Pump/Recent Changes broadcast"
description = ""
date = 2010-11-10T02:01:33Z
aliases = []
[extra]
id = 3376
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Recent Changes broadcast
|summary=Question about UDP and multicast
}}
A week or two ago, I discovered that Mediawiki supports live streaming of the Recent Changes feed [http://www.mediawiki.org/wiki/Manual:Configuration_settings#UDP_updates via UDP].  This sounds promising, as it means that I could set up a bot to do just about anything I wanted based on that feed.  ''However'', I can only have MediaWiki target a single IP/port combination.  I was considering targeting a multicast IP/port combination, to allow anyone to pick up the feed who was interested.  I realize that multicast is typically used for audio/video streaming, but it seems like it would be suited for this as well.  Unfortunately, I haven't found much information on how to set up a multicast.  Do I target any IP/port in the multicast range, or do I have to get one allocated?  If I post this, does anyone else think they might have a use for the feed? --[[User:Short Circuit|Short Circuit]] 09:08, 14 February 2009 (UTC)
