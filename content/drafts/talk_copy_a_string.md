+++
title = "Talk:Copy a string"
description = ""
date = 2009-11-18T05:34:07Z
aliases = []
[extra]
id = 4985
[taxonomies]
categories = []
tags = []
+++

==Objective-C Example==
Well I don't know Objective-C, but looking at [[Copy_a_string#Objective-C|the examples]] isn't there an error on this line:

```objc
NSString *newMutable = [original mutableCopy];
```

shoudn't it be this below ?

```objc
NSMutableString *newMutable = [original mutableCopy];
```

:Well, it's not wrong -- since NSMutableString is a subclass of NSString -- it just makes it less useful. --[[Special:Contributions/164.67.199.150|164.67.199.150]] 05:34, 18 November 2009 (UTC)
