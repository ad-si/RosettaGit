+++
title = "Talk:Gauss-Jordan matrix inversion"
description = ""
date = 2018-11-03T19:58:54Z
aliases = []
[extra]
id = 22054
[taxonomies]
categories = []
tags = []
+++

== what's wrong (with my REXX program)? ==

<strike>
::: I suggest to change header to:    '''what's wrong with my REXX program?'''</strike>   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:30, 3 November 2018 (UTC)
:::: Done.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:56, 3 November 2018 (UTC)


My REXX algorithm works nicely most of the time.
But sometimes it fails. E.g., with this matrix:

```txt

  3  1  8  9  6
  6  2  8 10  1
  5  7  2 10  3
  3  2  7  7  9
  3  5  6  1  1

```

The first elimination sets a.2.2=0 and how could this become 1 furtheron?
Help! Thanks!! --Walter Pachl 08:52, 3 November 2018 (UTC)



-----



I don't see your failure,   '''a.2.2'''   becomes zero and stays a zero.

Also, where it says <big> "Main diagonal has all ones" </big> isn't true.

With:  
        a=  3 1 8 9 6     6 2 8 10 1     5 7 2 10 3     3 2 7 7 9     3 5 6 1 1 
{{out}}

```txt

show 1 The given matrix
  3  1  8  9  6  1  0  0  0  0
  6  2  8 10  1  0  1  0  0  0
  5  7  2 10  3  0  0  1  0  0
  3  2  7  7  9  0  0  0  1  0
  3  5  6  1  1  0  0  0  0  1

show 2 1
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0  16/5 -34/5    -3 -21/5    -1     0   3/5     0     0
     0     1    -1    -2     3    -1     0     0     1     0
     0     4    -2    -8    -5    -1     0     0     0     1

show 2 2
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0

show 2 3
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0

show 2 4
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0

Lower part has all zeros

show 3
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0

show 4 5
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0

show 4 4
     3     1     8     9     6     1     0     0     0     0
     0     0    -4    -4 -11/2    -1   1/2     0     0     0
     0     0     4     4  11/2     1  -1/2     0     0     0
     0     0     0     0     0     0     0     0     0     0
     0     0     0     0     0     0     0     0     0     0

show 4 3
    3    1    0    1   -5   -1    1    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    4    4 11/2    1 -1/2    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0    0    0    0    0    0

show 4 2
    3    1    0    1   -5   -1    1    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    4    4 11/2    1 -1/2    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0    0    0    0    0    0

Upper half has all zeros

show 5
    1  1/3    0  1/3 -5/3 -1/3  1/3    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    1    1 11/8  1/4 -1/8    0    0    0
    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0    0    0    0    0    0

Main diagonal has all ones

show 6 The inverse matrix
 -1/3  1/3    0    0    0
    0    0    0    0    0
  1/4 -1/8    0    0    0
    0    0    0    0    0
    0    0    0    0    0

The product of input and inverse matrix
      1    1/3      0    1/3   -5/3
      0      0      0      0      0
      0      0      1      1   11/8
      0      0      0      0      0
      0      0      0      0      0

```

I would add the statement   (at least for now):

   signal on noValue

as you have a piece of dead code (possibly a debugging leftover?):

   ss=sigl 


I used three different REXX interpreters, all yielded the same results.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:25, 3 November 2018 (UTC)


-----


:::Can any of the other programs handle this input  correctly? --Walter Pachl 15:22, 3 November 2018 (UTC)

::::I ran it through the Perl 6 implementation and it works and is nominally correct. (If I invert the inverted matrix, I end up with the original.)  I added it as another test case. See the last two tests under [[Gauss-Jordan_matrix_inversion#Perl_6]] --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 18:29, 3 November 2018 (UTC)

:::::Thank you, that helped. I fixed my program for the case that a.2.2=0. 
My inverse matrix is the same as Perl 6's

```txt

show 6 The inverse matrix
 -4525/6238  2529/6238  -233/3119  1481/3119  -639/6238
  1033/6238 -1075/6238   342/3119  -447/3119   871/6238
  1299/6238  -289/6238  -204/3119  -390/3119   739/6238
   782/3119  -222/3119   237/3119  -556/3119  -177/3119
  -474/3119   -17/3119   -24/3119   688/3119  -140/3119

```



Generalization of the REXX algorithm for ANY pivot=0 must be worked out (tbd) --Walter Pachl 19:58, 3 November 2018 (UTC)
