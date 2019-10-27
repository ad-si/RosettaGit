+++
title = "Talk:Circles of given radius through two points"
description = ""
date = 2017-12-28T20:17:41Z
aliases = []
[extra]
id = 13320
[taxonomies]
categories = []
tags = []
+++

==More special cases==
There may be more special cases.  If p1==p2 and r==0, there is one unique answere that's a zero radius circle.  If tow points are separated by exactly double the radius, there's only one answer.  The latter can be treated as two identical circles, but then so can the former.

```python
from math import sqrt

def find_center(p1, p2, r):
    if p1 == p2:
        if r == 0: return [p1] # special special case
        # maybe we can return a generator that yields random circles, eh?
        raise ValueError("infinite many answers")

    (x1,y1), (x2,y2) = p1, p2
    x, y = (x1 + x2)/2.0, (y1 + y2)/2.0
    dx, dy = x1 - x, y1 - y
    a = r*r / (dx*dx + dy*dy) - 1

    if not a: return [(x0, y0)]
    if a < 0: return []
    a = sqrt(a)
    return [(x + a*dy, y - a*dx), (x - a*dy, y + a*dx)]

print find_center((0, 0), (1, 1), 1) # normal case
print find_center((0, 0), (0, 0), 0) # special case 1
print find_center((0, 0), (0, 2), 1) # special case 2
```


:Hi Ledrug. I have one of those covered - two points on a diameter is tested by the current second set of inputs. I'll have to adjust for the two coincident points with r == 0.0 case. Thanks. 
:Hmm r==0.0 might be treated as an exception too as it is the circle as a point, (If you don't want points). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:02, 17 April 2013 (UTC)
:: A circle with zero radius is still a perfectly valid circle, I don't see why it should be excluded. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:55, 17 April 2013 (UTC)
::: But it does require the points to be coincident or it has no solution. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 20:28, 17 April 2013 (UTC)
:::: I was talking about when you do get a zero circle as a solution. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 22:59, 17 April 2013 (UTC)
:::I could change it or leave it. Either case would work as the task explicitly states what to do with the r == 0.0 case, currently. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:11, 17 April 2013 (UTC)
::::The task states two mutually-incompatible things in that case. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 12:35, 27 April 2013 (UTC)
::::And based on that, I've removed the marking of the Tcl version as incorrect. Inconsistencies in the spec mean that I can do that. (Anyone arguing that there always has to be two circles returned is just confusing restrictions caused by the way ''their'' language wants to do things; the natural thing in Tcl is just to return a list of all the circles that there are.) –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 13:53, 27 April 2013 (UTC)
:::And dealing with nearly "normal" specifications are an important part of programming ;-)
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:15, 17 April 2013 (UTC)

== XPL0 ==

REXX referred to XPL0. 
This section was removed at some point in time.
Unfortunately the History does not show when and by whom.
I restored now the old section --Walter Pachl 08:35, 15 October 2017 (UTC)

: By   ''referred to'',   it was meant that REXX entry was a translation   <nowiki> {{trans|XPL0}} </nowiki>   of the   '''XPL0'''.   entry.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:43, 15 October 2017 (UTC)

:: What's that other than a reference? --Walter Pachl 08:52, 15 October 2017 (UTC)

::: A reference could be anything that referenced (mentioned) something else by name but not necessarily contained herein on Rosetta Code   --- for instance, the language   '''xyz'''   used/uses the assumption(s) yadda-yadda-yadda,   or entry   '''xyz'''   used a similar approach, logic, data (for input), ...   or any other number of things, most often (but not limited to) somewhere in the entry's preamble (or comments made elsewhere).   However, a   <nowiki> {{trans|XPL0}} </nowiki>   means that (in this case) the   '''REXX'''   entry used (most likely) the   '''XPL0'''   entry (the source code here on this Rosetta Code task) as a basis or prototype for the computer programming language translation.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:52, 15 October 2017 (UTC)
:Walter, the XPL0 was actually never ''removed'': [http://rosettacode.org/mw/index.php?title=Circles_of_given_radius_through_two_points&diff=209250&oldid=208439 this edit] adding the entry on Visual FoxPro mistakenly ended with a ''pre'' HTML tag, which made the XPL0 entry show up at the end of the VisualFox Pro entry. I'll remove it now. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:17, 28 December 2017 (UTC)
