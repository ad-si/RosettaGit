+++
title = "File talk:Electronic cigarette 698.jpg"
description = ""
date = 2011-06-03T14:18:12Z
aliases = []
[extra]
id = 9859
[taxonomies]
categories = []
tags = []
+++

It occurs to me that I can probably disallow JPG uploads in favor of GIF or PNG; very, very little on Rosetta Code would actually find JPG useful. And it would probably kill off the majority of image spam. --[[User:Short Circuit|Michael Mol]] 04:31, 3 June 2011 (UTC)

Well, another one just got posted, so here's a good opportunity to kick someone and their pictures at the same time [[User:Axtens|Axtens]] 05:29, 3 June 2011 (UTC)
: I think I've done it. MW by default allows several ''extensions'', including ".jpg" and ".jpeg". Thus far, all of the uploads have been '.jpg', so I've removed that and left '.jpeg' in the allowed upload formats. I've done that by placing this in LocalSettings.php:

```php
# Remove .jpg, but *not* .jpeg. .jpg is favored for spammers.
$wgFileExtensions = array_diff(array('jpg'), $wgFileExtensions);
```

--[[User:Short Circuit|Michael Mol]] 14:18, 3 June 2011 (UTC)
