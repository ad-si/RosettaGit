+++
title = "Talk:Find the intersection of two lines"
description = ""
date = 2018-04-20T05:39:51Z
aliases = []
[extra]
id = 21378
[taxonomies]
categories = []
tags = []
+++

==a REXX version of a REXX version==
This REXX version is a re-write of version 2 of the REXX entry,   with:
::*   added the required comment so that this REXX version would execute on VM/CMS and MVS/TSO
::*   aligned indentation for all   '''do-end'''   blocks   (and encapsulated statements)
::*   elided distracting superfluous zeros in integers
::*   elided superfluous decimal points in integers
::*   elides the superfluous and distracting use of concatenation   ('''││''')
::*   a unique symbol instead of a null literal for a special case
::*   aligns the data points and results in the output
::*   adds whitespace to make arithmetic computations more perusable
::*   eschews title-case capitalizations
::*   maintains the same line for the   '''then'''   clause and the   '''if'''   clause   (no split statements)
::*   uses indentations for all REXX statements in the function
::*   has the result on the same line as the input   (data points)
::*   a different quoted literal style   (for easier reading of multiple literals on the same clause)
::*   REXX variables to hold long literals that would otherwise cause excessive wide REXX statements
::*   a comma   (instead of a slash)   to separate the   <big> '''x   y''' </big>   coördinates of the data points.
::*   elides superfluous   '''do-end'''   block structures;   less clutter, easier to read
::*   added more whitespace within some REXX statements and the REXX program's output
::*   tests all data possibilities   (for showing all the tested non-intersecting conditions)
::*   for viewing the calculation in its entirity, all program logic was kept within single viewable screen

{{trans|REXX (version 2)}}

```rexx
/*REXX program finds (possibly) the intersection of two lines  (with diagnostic errors).*/
say iSect(   4  0      6 10      0 3      10 7    )
say iSect(   0  0      0 10      0 3      10 7    )
say iSect(   0  0      0 10      0 3      10 7    )
say iSect(   0  0      0  1      1 0       1 7    )
say iSect(   0  0      0  0      0 3      10 7    )
say iSect(   0  0      3  3      0 0       6 6    )
say iSect(   0  0      3  3      0 1       6 7    )
say iSect(   0  0      3  3      8 8       8 8    )
exit   /*  ═══a═══   ═══b═══   ═══c═══   ═══c═══   stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSect: procedure; parse arg  xa ya  xb yb  xc yc  xd yd /*optain args from invocation.  */
       $=.                                              /*the intersection or error msg.*/
       if xa=xb  then do;  k1=.                         /*the slope is infinite.        */
                           x1=xa                        /*the  X's  intersection is  XA */
                           if ya=yb  then $= 'points  A  and  B   are identical'
                      end                               /* [↑]  AB  is a vertical line. */
                 else do;  k1=(yb-ya) / (xb-xa)         /*compute the slope of  AB      */
                           d1=ya - k1 * xa              /*calc. intersection with Y axis*/
                      end                               /* [↑]  AB isn't a vertical line*/
       if xc=xd  then do;  k2=.                         /*the slope is infinite.        */
                           x2=xc                        /*the  C's  intersection is  XC */
                           if yc=yd  then $= 'points  C  and  D   are identical'
                      end                               /* [↑]  CD  is a vertical line. */
                 else do;  k2=(yd-yc) / (xd-xc)         /*compute the slope of  CD      */
                           d2=yc - k2 * xc              /*calc. intersection with Y axis*/
                      end                               /* [↑]  CD isn't a vertical line*/
       @ident= 'lines  AB  and  CD  are identical'      /*literal to help shorten a line*/
       @paral= 'lines  AB  and  CD  are  parallel'      /*   "     "   "     "    "   " */
                                                        /* [↓] no special case so far···*/
       if $=.  then  if k1=. then  if k2=. then  if x1=x2  then  $=@ident  /*identical. */
                                                           else  $=@paral  /*parallel.  */
                                           else  do;  x=x1                 /*use  X1    */
                                                      y=k2 * x + d2        /*Y from CD  */
                                                 end
                             else  if k2=. then  do;  x=x2                 /*X from CD  */
                                                      y=k1 * x + d1        /*Y from AB  */
                                                 end
                                           else  if k1=k2  then  if d1=d2  then  $= @ident
                                                                           else  $= @paral
                                                           else  do;  x=(d2-d1) / (k1-k2)
                                                                      y=k1 * x + d1
                                                                 end       /* [↑] normal*/
       if $=.  then $= 'intersection is at  (' || x","y')'                 /*$ ¬defined?*/
       @ = left( 'a=('xa","ya')', 12)      left( 'b=('xb","yb')', 12),     /*whitespace.*/
           left( 'c=('xc","yc')', 12)      left( 'd=('xd","yd')', 12)      /*     "     */
       return left(@, max(51, length(@) ) )          ' ───► '     $        /*return str.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

a=(4,0)      b=(6,10)     c=(0,3)      d=(10,7)      ───►  intersection is at  (5,5)
a=(0,0)      b=(0,10)     c=(0,3)      d=(10,7)      ───►  intersection is at  (0,3)
a=(0,0)      b=(0,10)     c=(0,3)      d=(10,7)      ───►  intersection is at  (0,3)
a=(0,0)      b=(0,1)      c=(1,0)      d=(1,7)       ───►  lines  AB  and  CD  are  parallel
a=(0,0)      b=(0,0)      c=(0,3)      d=(10,7)      ───►  points  A  and  B   are identical
a=(0,0)      b=(3,3)      c=(0,0)      d=(6,6)       ───►  lines  AB  and  CD  are identical
a=(0,0)      b=(3,3)      c=(0,1)      d=(6,7)       ───►  lines  AB  and  CD  are  parallel
a=(0,0)      b=(3,3)      c=(8,8)      d=(8,8)       ───►  points  C  and  D   are identical

```



### ========================================================================================

: If I were to translate many of the Rexx programs of GS to my liking I'd be busy for months.
::*  Here is the Rexx program as I wrote it in my style:::*  
http://rosettacode.org/wiki/Find_the_intersection_of_two_lines#version_2
::*  Any opinions from any peers? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:01, 19 May 2017 (UTC)

-----

::: There is no reason to take it personally.   It wasn't meant as a criticism, it is just another version in a different style, albeit a fair number of (style) differences.   <u>Everybody's</u> code can be improved (as least, the style can be changed).   In this case, I elided a few superfluous statements, which, in my opinion, didn't add anything to the REXX program or make it easier to understand/peruse.   I didn't appreciate your style of capitalization, misaligned DO-END statements (and the intervening/encapsulating REXX statements), split IF-THEN clauses, and much more.     But, that's only my opinion and preferences,   I merely added a version that I found easier to read and understand (and I hoped others will appreciate this version), and I also removed superfluous DO-END blocks and such.   Note that this re-written REXX version was added in the   ''discussion''   section, not on the   ''page''   section so as to not clutter up the main page.   I found that that particular REXX version was so difficult to follow and understand (the IF logic) with all the multiple misaligned DO-END and compound IF THEN-ELSE statements.   The version (above) that I re-wrote speaks to style and understandability.   There are many styles to write REXX programs in, and this is just one of them.   Nobody's style is everybody's cup of tea.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:22, 19 May 2017 (UTC)

-----
:::: The only thing I liked about this variation is the presentation of the result.
:::: The a= etc. should be A= etc. to be consistent. 
:::: I dislike that the variation can no longer be used with ooRexx :-( 
:::: and i cannot appreciate the landscape formatting!
:::: But let's agree that our taste as far as formatting is concerned is vastly different.
:::: And I added commentary to my version. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:29, 19 May 2017 (UTC)
