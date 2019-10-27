+++
title = "Rosetta Code:Village Pump/Special:AllPages broken"
description = ""
date = 2010-11-28T17:14:05Z
aliases = []
[extra]
id = 8115
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=AllPages broken
|summary=MW Extension for TextLinkAds caused a bug with AllPages
}}
Hi, it seems that [[Special:AllPages]] is broken. I now only get a completely empty page. --[[User:Ce|Ce]] 08:54, 26 August 2010 (UTC)
: Fixed. It was a bug in the MW extension I wrote for TextLinkAds. Apparently, $wgParser->GetTitle returns something that's not an object when called while building that page. --[[User:Short Circuit|Michael Mol]] 11:54, 26 August 2010 (UTC)
