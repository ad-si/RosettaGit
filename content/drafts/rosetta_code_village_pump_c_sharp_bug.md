+++
title = "Rosetta Code:Village Pump/C sharp bug"
description = ""
date = 2012-09-02T18:27:58Z
aliases = []
[extra]
id = 9999
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=C sharp bug
|summary=Some problems arising with C# pages, especially headers
}}

* I noticed several users adding C# to their preferred language, but they appear as C tasks : comes from a syntax peculiarity of wiki code.

* Worse, C# tasks don't appear at all. I strongly suspect a problem with the notation "C sharp|C#" in headers.

The first problem is documented in C# comment at the top of the page. As a matter of fact, this comment is wrong: one should
not write <nowiki>{{header|C sharp|C#}} but {{header|C sharp}}</nowiki>, as in the only five tasks that appear in C#.
I'll change the comment and, some day, correct the headers, but I thought it should be noticed here first, for discussion.

[[User:Toucan|Toucan]] 20:09, 30 June 2011 (UTC)
:I don't quite understand the problem with the header template. The arguments for the template are the category name first (which should be "C sharp") and the optional display text second. The display text should be "C#" because that's how people type the language name. Recently someone changed the header template to automatically add "C#" as the display text when the first argument is "C sharp", so making these changes to the header template should not do anything. Where are you looking for "C#" tasks? Can you provide links to pages with problems? --[[User:Mwn3d|Mwn3d]] 20:42, 30 June 2011 (UTC)
::[[:Category:C sharp]] : here there are 42 tasks for now, yesterday there were 5 tasks (I changed some headers). I don't know why, but adding "|C#" to the header prevents the task to be shown in the language page. Just try [[Reports:Tasks not implemented in C sharp]] and pick anything at random, for example [[Hello world/Text]]. [[User:Toucan|Toucan]] 05:46, 1 July 2011 (UTC)
:::I fixed the header template. It should correct itself eventually. The site needs to update each page with the new template content. You can check the progress in the "job queue length" section [[Special:Statistics|here]]. It should be done when it gets down to 0 (or close to 0...I forget if there are background jobs that constantly go in the queue). I did a stupid and fixed it incorrectly, so it may take extra time because I had to correct that. In the meantime you should see the number of tasks in the C# category going up slowly (and the F# category because I suspect it had the same problem). --[[User:Mwn3d|Mwn3d]] 12:51, 1 July 2011 (UTC)
::::Muuuuch faster than one by one ! Thanks [[User:Toucan|Toucan]] 19:30, 1 July 2011 (UTC)

=== Should <nowiki>{{header|C#}}</nowiki> work? ===
Since Mwn3d fixed the template, <nowiki>{{header|C sharp|C#}} and {{header|C sharp}}</nowiki> are now both correct. This is because the template has a special case for "C sharp" where <nowiki>{{header|C sharp}}</nowiki> acts like <nowiki>{{header|C sharp|C#}}</nowiki>.

Should there be another special case for <nowiki>{{header|C#}}</nowiki>? I have put the code for this in [[Template:Header beta]]:


```txt
{{#if: {{{2|}}}
 | <span id="{{{1}}}">[[:Category:{{{1}}}|{{{2}}}]]</span> [[Category:{{{1}}}]] {{
   #set:implemented in language={{{1}}} }}
 |{{#switch: {{ucfirst: {{{1|}}} }}
   |C#|C sharp=<span id="C sharp">[[:Category:C sharp|C#]]</span> [[Category:C sharp]] {{
               #set:implemented in language=C sharp}}
   |F#|F Sharp=<span id="F Sharp">[[:Category:F Sharp|F#]]</span> [[Category:F Sharp]] {{
               #set:implemented in language=F Sharp}}
   | [[:Category:{{{1}}}|{{{1}}}]] [[Category:{{{1}}}]] {{
     #set:implemented in language={{{1}}} }}
  }}
}}
```


This would cause <nowiki>{{header|C#}}</nowiki> to act like <nowiki>{{header|C sharp|C#}}</nowiki>. However, plain <nowiki>[[C#]]</nowiki> links would still be broken. I now think that <nowiki>{{header|C#}}</nowiki> should remain broken like <nowiki>[[C#]]</nowiki>. --[[User:Kernigh|Kernigh]] 20:50, 15 July 2011 (UTC)
:I think we're OK now. I don't think any users have tried to do <nowiki>{{header|C#}}</nowiki>. If we see it then we can think about it. It's best to only change the header template when we need to since it's used in so many places and it's slow to update. --[[User:Mwn3d|Mwn3d]] 22:23, 15 July 2011 (UTC)


### other mixed names of languages


This may not be the appropriate place for this topic, but there are about a dozen or so languages that are ''mis-cased'' (sic) in their '''<nowiki>{{</nowiki>header|xxx<nowiki>}}</nowiki>''' --- that is, the names of the languages are in different case than the majority-use case of a language; the worst case has only three mis-caseings (sic):

*  ANT, ant
*  AutoIt, AutoIT
*  BASIC, Basic
*  Bc, BC
*  F Sharp, F sharp
*  Gdl, GDL
*  HaXe, Haxe
*  Maple, MAPLE
*  MATLAB, Matlab
*  NewLISP, Newlisp
*  OoRexx, OOREXX
*  OpenEdge/Progress, Openedge/Progress
*  Run BASIC, Run Basic 

(a more complete and updated list (along with their counts) is listed at or near the bottom (end) of http://rosettacode.org/wiki/Talk:Rosetta_Code/Rank_languages_by_popularity ('''case of names of programming languages''' section) if anybody wants to take a crack at fixing a few of them.  Because the case is the problem here, I don't know how to identify what (or where) the program example (entry) is --- that is, searching for them is useless. I fixed a small handful of errors because I knew the identify of the guy entering the bad mixed case of ''Rexx'' instead of ''REXX''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:21, 2 September 2012 (UTC)
