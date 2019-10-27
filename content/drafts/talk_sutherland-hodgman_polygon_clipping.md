+++
title = "Talk:Sutherland-Hodgman polygon clipping"
description = ""
date = 2016-11-21T20:44:16Z
aliases = []
[extra]
id = 6638
[taxonomies]
categories = []
tags = []
+++

==Draft Task Status==
Still needs description of the task. –[[User:Dkf|Donal Fellows]] 11:15, 23 March 2010 (UTC)
: OK, it's now good enough to be a task. Further cleanup can be done within that framework. –[[User:Dkf|Donal Fellows]] 15:25, 23 March 2010 (UTC)

== Winding ==

Implementations probably should be required to test if the clipper is winding clockwise or not.  Some of the current code crash if the rectangle is reversed.  Testing direction of a convex poly is pretty easy anyway, just checking any one vertex should be enough. --[[User:Ledrug|Ledrug]] 01:23, 7 July 2011 (UTC)

== Ruby ==

With Ruby 1.9, a block parameter must be a local variable.


```txt
$ ruby scratch.rb                                                              
scratch.rb:22: formal argument cannot be an instance variable
    clipPolygon.each do |@cp2| # WP clipEdge is cp1,cp2 here
                             ^
scratch.rb:26: formal argument cannot be an instance variable
      inputList.each do |@e|
                           ^
```


[http://rosettacode.org/mw/index.php?title=Sutherland-Hodgman_polygon_clipping&diff=118024&oldid=118006 My change to the Ruby code] fixes it for Ruby 1.9 but keeps it working with Ruby 1.8. --[[User:Kernigh|Kernigh]] 19:31, 19 August 2011 (UTC)


==Point lists hidden to most browsers by cosmetic edits at 06:57, 16 May 2016 ==

Under-tested cosmetic edits made to the task page at 06:57, 16 May 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the task description point lists completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:50, 22 September 2016 (UTC)

: Now repaired – visibility of task description point lists restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:43, 21 November 2016 (UTC)
