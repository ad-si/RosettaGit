+++
title = "Talk:Maximum triangle path sum"
description = ""
date = 2014-04-01T11:39:16Z
aliases = []
[extra]
id = 17267
[taxonomies]
categories = []
tags = []
+++

== numbers in the triangle ==

Just a curiosity, where those numbers in the given triangle chosen at random? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:02, 25 February 2014 (UTC)
:Correct-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

==efficiency of the REXX solution==

To find the efficiency of the REXX solution, a system of defining the lines (rows) of the triangle was chosen such that each row could be easily be generated, and more importantly, have a verifiable answer (for some largish even number of rows).
  
The algorithm used was to start each row (the left end) with unity, and increase each column number by one, so that the 53<sup>rd</sup> number on row 53 (and every other higher row) is 53.   Verifications of the maximum triangle path sum (for some triangles) can be easily seen by the results (below). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:23, 4 March 2014 (UTC)

A triangle of nine rows looks like:

```txt

             1
            1 2
           1 2 3
          1 2 3 4
         1 2 3 4 5
        1 2 3 4 5 6
       1 2 3 4 5 6 7
      1 2 3 4 5 6 7 8
     1 2 3 4 5 6 7 8 9

```

The REXX program used to generate the above monotonous (above) triangles and find the maximum triangle path sum is:

```rexx
/*REXX program finds the max sum of a "column" of numbers in a triangle.*/
numeric digits 10                      /*increase this for big triangles*/
parse arg L .                          /*get number of lines in triangle*/
if L==''  then L=1000                  /*Not given? Then use the default*/
#.=0
      do     r=1  for L;   q=0         /*construct lines of the triangle*/
          do c=1  for r;   q=q+1       /*leftmost number is always unity*/
          #.r.c=q                      /*build a triangle line number.  */
          end   /*c*/                  /*each line is:  1 2 3 4 5 6 ··· */
      end       /*r*/

      do     r=L  by -1  to 2;  p =r-1 /*traipse through triangle rows. */
          do k=1  for p;        kn=k+1 /*re-calculate the previous row. */
          #.p.k=max(#.r.k, #.r.kn) + #.p.k    /*replace the previous #. */
          end   /*k*/
      end       /*r*/

say 'The maximum path sum:' right(#.1.1,digits()), /*display top row #  */
    " for "    L    ' rows.'                       /* and the # of rows.*/
                                       /*stick a fork in it, we're done.*/
```

'''output''' for various runs are   (multiple runs have their output consolidated):

```txt

The maximum path sum:       5050  for  100  rows.
The maximum path sum:      20100  for  200  rows.
The maximum path sum:      45150  for  300  rows.
The maximum path sum:      80200  for  400  rows.
The maximum path sum:     125250  for  500  rows.
The maximum path sum:     180300  for  600  rows.
The maximum path sum:     245350  for  700  rows.
The maximum path sum:     320400  for  800  rows.
The maximum path sum:     405450  for  900  rows.
The maximum path sum:     500500  for  1000  rows.
The maximum path sum:    2001000  for  2000  rows.
The maximum path sum:    4501500  for  3000  rows.
The maximum path sum:    8002000  for  4000  rows.
The maximum path sum:   50005000  for  10000  rows.

```

The last entry was obtained via a specialized version of the REXX program   (due to a constraint on virtual memory). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:23, 4 March 2014 (UTC)

-----
