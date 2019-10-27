+++
title = "CHANGESTR.REX"
description = ""
date = 2018-10-15T20:23:45Z
aliases = []
[extra]
id = 12798
[taxonomies]
categories = []
tags = []
+++

[[Category:REXX_library_routines]]

== the CHANGESTR function ==
This a RYO version of the REXX function   '''changestr'''   (<u>change</u> <u>str</u>ing).

It is included in some of the more modern Classic REXXes, but older versions of 

Classic REXX don't have this function as a   BIF   (<u>B</u>uilt-<u>I</u>n-<u>F</u>unction). 



This version of the   '''changestr'''   BIF has more functionality than the standard BIF.


__TOC__



===the  CHANGESTR  (external) program source===
The following   CHANGESTR   program can be coded as is when the intention is to be an external routine (function). 

```rexx
/*REXX program emulates the  CHANGESTR  BIF  (built-in function)  for older REXXes.*/
/*──────────── This version has more functionality:   limit the number of changes. */
/*────────────                                        start of change occurrence#. */
/*────────────                                        start of change position.    */

  /* ╔═══════════════════════════ CHANGESTR function ════════════════════════╗
   ╔═╩═══════════════════════════════════════════════════════════════════════╩═╗
   ║ The  CHANGESTR  function is used to replace some or all occurrences of an ║
   ║ (old)  string in a haystack  with  a new string.   The changed string is  ║
   ║ returned.    If the haystack  doesn't  contain the  old  string,  the     ║
   ║ original haystack is returned.   If the old string is a   null   string,  ║
   ║ then the  original string  is  prefixed  with the  new string.            ║
   ║                                                                           ║
   ║        new string to be used ►──────┐ ┌──────◄ limit of # changes (times).║
   ║   original string (haystack) ►────┐ │ │         [default:  ≈ one billion] ║
   ║    old string to be replaced ►──┐ │ │ │ ┌────◄ begin at this occurrence # ║
   ║ {O, H, and  N  can be null.}    │ │ │ │ │ ┌──◄ start position (default=1) ║
   ╚═╦═════════════════════════════╗ │ │ │ │ │ │ ╔═══════════════════════════╦═╝
     ╚═════════════════════════════╝ │ │ │ │ │ │ ╚═══════════════════════════╝
                                     ↓ ↓ ↓ ↓ ↓ ↓                                   */
                         parse arg   o,h,n,t,b,p,  ,$ f  /*  ◄────────────────────┐*/
                                                         /*T, B, P are optional ►─┘*/
           t=word(t 999999999, 1)                        /*maybe use the default?  */
           b=word(b 1        , 1)                        /*  "    "   "     "      */
           p=word(p 1        , 1)                        /*  "    "   "     "      */
           if arg() < 3             then signal syntax   /*not enough arguments.   */
           if arg() > 6             then signal syntax   /*too  many  arguments.   */
           if \datatype(t, 'W')     then signal syntax   /*4th arg not an integer. */
           if \datatype(b, 'W')     then signal syntax   /*5th  "   "   "    "     */
           if \datatype(p, 'W')     then signal syntax   /*6th  "   "   "    "     */
           if t<0                   then signal syntax   /*4th  "   " non-negative.*/
           if b<1                   then signal syntax   /*5th  "   "   positive.  */
           if p<1                   then signal syntax   /*6th  "   "      "       */
           L=length(o)                                   /*length of  OLD  string. */
           if L==0  &  t\=0         then return n || h   /*changing a [null] char? */
                                                         /* [↓]  check for position*/
           if p\=1  then do                              /*P¬=1?  Then ajust F & H.*/
                         f=left(h, min(p-1, length(h)))  /*keep first part intact. */
                         h=substr(h, p)                  /*only use this part of H.*/
                         end                             /*now, proceed as usual.  */
           #=0                                           /*# of changed occurrences*/
                    do j=1   while  # < t                /*keep changing, T times. */
                    parse var  h  y  (o)  _  +(L)  h     /*parse the haystack ···  */
                    if _==''  then return f || $ || y    /*no more left,  return.  */
                    $=$ || y                             /*append the residual txt.*/
                                                         /* [↓]  check if too soon.*/
                    if j<b    then $=$ || o              /*append OLD if too soon. */
                              else do                    /*met the occurrence test.*/
                                   $=$ || n              /*append the  NEW  string.*/
                                   #=#+1                 /*bump occurrence number. */
                                   end                   /* [↑]  append new string.*/
                    end   /*j*/                          /*Note:  most REXX  ···   */
                                                         /* CHANGESTR BIFs only ···*/
           return  f || $ || h                           /* support three options. */
```


=== the  CHANGESTR  (internal) procedure source ===
The following CHANGESTR program can be coded as is when the intention is to be an internal routine (function). 

Only the '''changestr:''' statement is changed   (by adding a   '''procedure'''   to the label).

```rexx
/*REXX program emulates the  CHANGESTR  BIF  (built-in function)  for older REXXes.*/
/*──────────── This version has more functionality:   limit the number of changes. */
/*────────────                                        start of change occurrence#. */
/*────────────                                        start of change position.    */

  /* ╔═══════════════════════════ CHANGESTR function ════════════════════════╗
   ╔═╩═══════════════════════════════════════════════════════════════════════╩═╗
   ║ The  CHANGESTR  function is used to replace some or all occurrences of an ║
   ║ (old)  string in a haystack  with  a new string.   The changed string is  ║
   ║ returned.    If the haystack  doesn't  contain the  old  string,  the     ║
   ║ original haystack is returned.   If the old string is a   null   string,  ║
   ║ then the  original string  is  prefixed  with the  new string.            ║
   ║                                                                           ║
   ║        new string to be used ►──────┐ ┌──────◄ limit of # changes (times).║
   ║   original string (haystack) ►────┐ │ │         [default:  ≈ one billion] ║
   ║    old string to be replaced ►──┐ │ │ │ ┌────◄ begin at this occurrence # ║
   ║ {O, H, and  N  can be null.}    │ │ │ │ │ ┌──◄ start position (default=1) ║
   ╚═╦═════════════════════════════╗ │ │ │ │ │ │ ╔═══════════════════════════╦═╝
     ╚═════════════════════════════╝ │ │ │ │ │ │ ╚═══════════════════════════╝
                                     ↓ ↓ ↓ ↓ ↓ ↓                                   */
changestr: procedure;    parse arg   o,h,n,t,b,p,  ,$ f  /*  ◄────────────────────┐*/
                                                         /*T, B, P are optional ►─┘*/
           t=word(t 999999999, 1)                        /*maybe use the default?  */
           b=word(b 1        , 1)                        /*  "    "   "     "      */
           p=word(p 1        , 1)                        /*  "    "   "     "      */
           if arg() < 3             then signal syntax   /*not enough arguments.   */
           if arg() > 6             then signal syntax   /*too  many  arguments.   */
           if \datatype(t, 'W')     then signal syntax   /*4th arg not an integer. */
           if \datatype(b, 'W')     then signal syntax   /*5th  "   "   "    "     */
           if \datatype(p, 'W')     then signal syntax   /*6th  "   "   "    "     */
           if t<0                   then signal syntax   /*4th  "   " non-negative.*/
           if b<1                   then signal syntax   /*5th  "   "   positive.  */
           if p<1                   then signal syntax   /*6th  "   "      "       */
           L=length(o)                                   /*length of  OLD  string. */
           if L==0  &  t\=0         then return n || h   /*changing a [null] char? */
                                                         /* [↓]  check for position*/
           if p\=1  then do                              /*P¬=1?  Then ajust F & H.*/
                         f=left(h, min(p-1, length(h)))  /*keep first part intact. */
                         h=substr(h, p)                  /*only use this part of H.*/
                         end                             /*now, proceed as usual.  */
           #=0                                           /*# of changed occurrences*/
                    do j=1   while  # < t                /*keep changing, T times. */
                    parse var  h  y  (o)  _  +(L)  h     /*parse the haystack ···  */
                    if _==''  then return f || $ || y    /*no more left,  return.  */
                    $=$ || y                             /*append the residual txt.*/
                                                         /* [↓]  check if too soon.*/
                    if j<b    then $=$ || o              /*append OLD if too soon. */
                              else do                    /*met the occurrence test.*/
                                   $=$ || n              /*append the  NEW  string.*/
                                   #=#+1                 /*bump occurrence number. */
                                   end                   /* [↑]  append new string.*/
                    end   /*j*/                          /*Note:  most REXX  ···   */
                                                         /* CHANGESTR BIFs only ···*/
           return  f || $ || h                           /* support three options. */
```

