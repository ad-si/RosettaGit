+++
title = "Talk:Sorting algorithms/Comb sort"
description = ""
date = 2016-11-09T14:28:53Z
aliases = []
[extra]
id = 7319
[taxonomies]
categories = []
tags = []
+++


### Faulty Code

After discussion on [http://www.autohotkey.com/forum/ AHK forum], and thoroughly checking the findings, I believe the published pseudocode is faulty, and so are the implementations that follow the pseudocode. I don't have the ability to check all languages, but these '''look suspicious''' to me: ActionScript, C#, Io, PL/I, PureBasic (2nd example), Python, Ruby, Tcl, TI-83 BASIC. I appologize for suspecting any code incorrectly, but I do have a point with python.

This example for Python:
```txt
...

x = [88, 18, 31, 44, 4, 0, 8, 81, 14, 78, 20, 76, 84, 33, 73, 75, 82, 5, 62, 70]
combsort(x)
print x

```

gives this incorrect output: (check the position of 62 und 70)

```txt
[0, 4, 5, 8, 14, 18, 20, 31, 33, 44, 70, 62, 73, 75, 76, 78, 81, 82, 84, 88]
```

I claim no credit for the discovery for myself, as my original code (for AutoHotkey) fell into the same trap, and got corrected by a fellow forum member.

--[[User:Wolf|Wolf]] 13:44, 16 May 2010 (UTC)

'''PS:''' The pseudocode might be fixed by changing this:

```txt
        //update the gap value for a next comb. Below is an example
        gap := int(gap / 1.25)
```
to this:

```txt
        //update the gap value for a next comb. Below is an example
        if gap > 1
            gap := int(gap / 1.25)
        end if
```

--[[User:Wolf|Wolf]] 14:08, 16 May 2010 (UTC)

:After a [[User_talk:Paddy3118#Hi_Paddy.2C_I.27m_Wolf|most polite request]] from Wolf, I had a look at what might be an "authoritative" comb sort site from the US [http://www.itl.nist.gov/div897/sqg/dads/HTML/combSort.html National Institute of Standards and Technology (NIST)] which links to [http://people.cs.ubc.ca/~harrison/Java/CombSort11Algorithm.java this java] example implementation, which does indeed ensure that the gaps minimum value is 1 with this section of its code:
::
```java
            gap = (int) ((float) gap / SHRINKFACTOR);
            switch (gap) {
            case 0: /* the smallest gap is 1 - bubble sort */
                gap = 1;
                break;
```

:In short - Wolf is right: the pseudo-code is wrong. (The counter-example above does not lie). --[[User:Paddy3118|Paddy3118]] 05:23, 27 May 2010 (UTC)
:: Correct the task and mark the examples as ENA? Someone should also make the relevant corrections at Wikipedia. --[[User:Short Circuit|Michael Mol]] 12:48, 27 May 2010 (UTC)

:::This task marked clarified-review; pseudo-code updated (again); WP modified; Python example modified and checked with example data above. --[[User:Paddy3118|Paddy3118]] 19:00, 27 May 2010 (UTC)

Thank you guys! :) --[[User:Wolf|Wolf]] 23:15, 27 May 2010 (UTC)

==Formulae hidden to most browsers by under-tested cosmetic edits at 09:12, 4 May 2016==

Under-tested cosmetic edits made to the task page at 09:12, 4 May 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:11, 24 September 2016 (UTC)

: Visibility of formulae restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:28, 9 November 2016 (UTC)
