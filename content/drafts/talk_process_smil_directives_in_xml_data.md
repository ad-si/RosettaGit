+++
title = "Talk:Process SMIL directives in XML data"
description = ""
date = 2014-05-27T08:14:50Z
aliases = []
[extra]
id = 5339
[taxonomies]
categories = []
tags = []
+++

== Namespaces? ==

Are we meant to do this task taking into account the namespaces for X3D and SMIL, or are we doing this in namespace-free form? –[[User:Dkf|Donal Fellows]] 14:27, 19 January 2010 (UTC)

: When I have written this task I thought it would be very easy to implement, and it is indeed quite easy but it requires more code that what I thought. If you look at the OCaml implementation it requires about 240 lines of code, I thought at the beginning that it could be done with half of this. So ''IMHO'' I would suggest to keep the task as simple as possible (so without namespaces).
: Also maybe I'm wrong but it seems that the SMIL examples I have seen are without namespace (or maybe it was to keep the examples simple).
: Also from my point of view the task is mainly about storing geometry and animation to a file, and loading and playing it into OpenGL (or Directx or else), and not about XML parsing, so perhaps we could consider keeping it not too strict. (If I chose to use real formats with specs and not a fake format it was to make the task also a bit pedagogical, so people can have a little taste of X3D and SMIL)
: But this is only a suggestion, as you have seen I have written the task as a draft, so if you have suggestions to modify  the task I would be happy to talk about it.
: [[User:Blue Prawn|Blue Prawn]] 16:58, 19 January 2010 (UTC)

==Simplification==
OK, in order to take into account the tag '''difficult task''', I've tryed to simplify it by removing everything related to OpenGL and X3D, and to keep only the SMIL part of the task. Let's see if the task will be more successful now. [[User:Blue Prawn|Blue Prawn]] ([[User talk:Blue Prawn|talk]]) 14:40, 25 May 2014 (UTC)
: Maybe we should also change the example for something simplier, SVG instead of X3D? [[User:Blue Prawn|Blue Prawn]] ([[User talk:Blue Prawn|talk]]) 14:43, 25 May 2014 (UTC)
:: This is now definitely much more tractable; that rendering was something I was really quite concerned about how much effort it would have taken. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 07:21, 26 May 2014 (UTC)
:: OK, try that. Does it look like the sort of thing you were thinking about? (I can expand it to cover more, but would rather not; I don't want to write a full SMIL library…) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 08:13, 27 May 2014 (UTC)
