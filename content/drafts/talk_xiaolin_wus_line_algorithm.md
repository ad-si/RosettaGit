+++
title = "Talk:Xiaolin Wu's line algorithm"
description = ""
date = 2019-02-19T23:14:53Z
aliases = []
[extra]
id = 9081
[taxonomies]
categories = []
tags = []
+++

== '''C#'''  solution for '''Xiaolin Wu's line algorithm''' ? == 
Any guy can give me '''Xiaolin Wu's line algorithm'''s code in C#.
Than you.

== Bug in the C code? ==


```c

} else {
    if ( y2 < y1 ) {
      swap_(x1, x2);
      swap_(y1, y2);
    }
    double gradient = dx / dy;
    double yend = round_(y1);
    double xend = x1 + gradient*(yend - y1);
    double ygap = rfpart_(y1 + 0.5);
    int ypxl1 = yend;
    int xpxl1 = ipart_(xend);
    plot_(xpxl1, ypxl1, rfpart_(xend)*ygap);
    plot_(xpxl1, ypxl1+1, fpart_(xend)*ygap);
    double interx = xend + gradient;
 
    yend = round_(y2);
    xend = x2 + gradient*(yend - y2);
    ygap = fpart_(y2+0.5);
    int ypxl2 = yend;
    int xpxl2 = ipart_(xend);
    plot_(xpxl2, ypxl2, rfpart_(xend) * ygap);
    plot_(xpxl2, ypxl2 + 1, fpart_(xend) * ygap);
 
    int y;
    for(y=ypxl1+1; y <= (ypxl2-1); y++) {
      plot_(ipart_(interx), y, rfpart_(interx));
      plot_(ipart_(interx) + 1, y, fpart_(interx));
      interx += gradient;
    }
  }
}

```


The start and end pixels here in the else where fabs(dx) <= fabs(dy), should have a secondary plot point +1 in the x-direction, not y-direction. It's correct for the main loop and the first if, but not the two end points, for any drawn line with a slope greater or equal to 1.

So where it says:

```c

    plot_(xpxl2, ypxl2, rfpart_(xend) * ygap);
    plot_(xpxl2, ypxl2 + 1, fpart_(xend) * ygap);

```
 
it should say:

```c

    plot_(xpxl2, ypxl2, rfpart_(xend) * ygap);
    plot_(xpxl2 + 1, ypxl2, fpart_(xend) * ygap);
 
```

[[Special:Contributions/76.175.220.55|76.175.220.55]] 11:20, 3 July 2011 (UTC)

== REXX comment ==

The new REXX entry currently says "Apparently, there may be an error in the definition of the algorithm (which only manifests itself with negative numbers): use of the IPART function should probably be FLOOR"

But, in the sort of context where this line algorithm would be useful that I have seen, screen coordinates are always have been non-negative integers, so in practice this kind of ambiguity should not matter.   --[[User:Rdm|Rdm]] 15:52, 4 June 2012 (UTC)

: Yuppers, that's when the trouble starts.  Still, an error is an error (even if only by omission or via ambiguousness), and it's especially troublesome when the pseudo-code in the Wikipedia entry doesn't mention the limitations.  After I adjusted the IPART to FLOOR, it seems to work, but without others trying negative coödinates (which weren't specially mentioned that negative coördinates weren't allowed), we'll never know until the fat lady sings, er, I mean, actual output from the various language examples are shown. Since Xiaolin Wu's line algorithm can be used as a replacement for Bresenham's line algorithm (which was developed to be used with a CalComp plotter attached to an IBM 1401 in the 1960's), I assumed that it would also handle negative coördinates. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:23, 4 June 2012 (UTC)

:: Ok, but I am not sure how to tell if "floor is the required definition for 'integer part'" is the right way to characterize this issue.  I took my implementation and modified the draw point routine so that it adds 100 to the coordinate before rendering to the screen.  This gives me a coordinate range for my algorithm which can include negative coordinates.  Then, I took my original coordinate range and subtracted 100 from every value.  Then, I ran the code -- I did not see any visual artifacts resulting from this set of changes.  When I look at my code, I see that I used floor to implement "integer part", but I do not know if I see no visual artifacts because I "just happened to use the right definition for integer part" or whether it's because the visual effect of this issue is too small to notice.  I am also wondering whether the issue might really be that the code which extracts the fractional part has to be consistent with the code that extracts the integer part.  --[[User:Rdm|Rdm]] 16:34, 4 June 2012 (UTC)

::: Once you make all numbers non-negative, FLOOR and IPART are synonymous.  It's only when using negative numbers that the issue (any difference) is raised. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:51, 4 June 2012 (UTC)

:::: Yes, but I made some of them negative for the algorithm (just translating them to positive when they were rendered).  --[[User:Rdm|Rdm]] 17:02, 4 June 2012 (UTC)

Concerning another issue: '''FPART'''.  

When handling negative coödinates, I was wondering if FPART is doing what it's supposed to do, but I couldn't find a good definition of FPART (with regarding to negative numbers) --- but I'm still rooting around the 'net.  For non-negative numbers, the simple definition of FPART (even if wrong), still gives the correct answer.  

FPART of 6.78 is +0.78.   But what about -8.91 ?  Is the fractional part +0.91 or -0.91 ? 

I'm still trying to locate a good definition for it --- I hope the definition isn't tied (or restricted to) any particular computer language.  

Perhaps someone could find a complete/good definition of FPART in their appropriate language reference documentation. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:46, 4 June 2012 (UTC)

: The integer part and the fractional part should add together to yield your original number.  --[[User:Rdm|Rdm]] 17:03, 4 June 2012 (UTC)

:: Tank qew.  Makes sense, concise, succent.  So, -0.91 it is. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:13, 4 June 2012 (UTC)

== Problem In Java Code ==

The Java code doesn't plot the line in all directions.
