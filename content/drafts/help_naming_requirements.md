+++
title = "Help:Naming Requirements"
description = ""
date = 2008-01-18T19:34:32Z
aliases = []
[extra]
id = 1865
[taxonomies]
categories = []
tags = []
+++

Pages must not have certain special characters in their article titles.  Of note is the characters #.

==Reasons==
[[HTML]] reserves the symbol # for intra-page navigation.

==Workaround==

For now, all pages and links to C# and J# should be written "C sharp" and "J sharp", respectively. When using the {header} template, use the second optional parameter to distinguish between the display name and link name:

```txt
<nowiki>
## C#
</nowiki>
```


Until Rosetta Code gets some bots, please keep an eye out for links written which point to forbidden article titles, and point them to their renamed pages.  An appropriate link for C# would be <nowiki>"[[C sharp|C#]]"</nowiki>.  This will point to the correct page, but will still show a link title of "C#".
