+++
title = "Talk:Permutations with repetitions"
description = ""
date = 2016-09-22T06:49:22Z
aliases = []
[extra]
id = 13480
[taxonomies]
categories = []
tags = []
+++

==Mathematica solution incorrect?==
The task description says combination must be produced one at a time.
The Mathematica solution uses Tuples, which return a list of all permutations at once.
Is this a correct solution?
--[[User:Soegaard|Soegaard]] ([[User talk:Soegaard|talk]]) 14:16, 21 May 2013 (UTC)

==REXX implementation limits==

Most REXX interpreters have implementation limits on the length of a REXX clause.

For Regina REXX, it depends on the release level.

For Regina REXX 3.3,   3.4,   3.5, REXX gives a SYNTAX error:

:::::: Error 12 running "C:\xxxxxx.REX", line 29: [Clause &amp; 1024 characters]

For Regina REXX 3.6,   3.7, REXX "crashes" somewhere less than a statement length of 1932,

I haven't pursued this to find the exact byte count.

{By crashing, I mean Microsoft Windows displays a small error "window":

:::::: Regina Rexx Launcher (x86) has encountered a problem
:::::: and needs to close.   We are sorry for the inconvenience.
:::::: (more boilerplate follows).

For what's it worth, I believe REXX should raise the SYNTAX condition.

[The reason I mention the older versions of Regina REXX is that Regina 3.3 REXX uses less

virtual memory for those REXX programs which use a ''lot'' of stemmed variables (possibly a

memory leak), so the use of the older Regina release is forced in lieu of a fix.   

For PC/REXX and Personal REXX, it depends on what the RXISA ('''SET''' environmental variable).

These two REXXes either give:
:::::: Error 11 on line 29 of c:\xxxxxx.REX" Control stack full


:::::: Memory full: symbol
:::::: Error 5 on line 29 of C:\xxxxxx.REX: Machine resources exhausted

somewhere between 568 and 597 bytes.  

It should be noted that most people use the maximum for the RXISA   (which is 40K).

As far as I can tell, R4 and ROO have no (real) limitation for the length of an INTERPRET instruction,

and that is probably true of CMS REXX and TSO REXX   (it's been too long since I used those

two flavors of REXX).   

::::::::::: -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:2013 (UTC)

----

==another variant of the REXX example==
This REXX version:
* simplifies many statements
* minimizes the length of the INTERPRET statement
* eliminates the SELECT statements
* eliminates a DO loop
* uses both kinds of quotes strings to make it easier to peruse
* moves a couple of statements out of the INTERPRET clause
* uses a positive (logic) instead of a negative IF statement
* eliminates an extra   ''';'''   (semicolon) from the INTERPRET clause
* eliminates the need for counting the permutations

If any of these improvements could/would be of use, that's fine.   I plan to delete this entry in a couple of weeks. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:2013 (UTC)

```rexx
/*REXX*/ parse arg n m s names
if n=''  then n=3
if m=''  then m=2
if datatype(s,'N')  then s=left('',s)
 
        do k=1 to n
        if names=''   then e.k=k
                      else parse var names e.k names
        end   /*k*/
a=''
                    do i=1 to m;   a=a'do d'i"=1 to n;";   end  /*i*/
a=a'z=e.d1'
                    do j=2 to m;   a=a"||s||e.d"j;         end  /*j*/
 
a=a';say z'copies(";end",m)
if m==0  then do; say '1 permutation, a "null".'; exit; end
interpret a
say  n**m  'permutations'
```


== Isn't it just similar to "Increment_a_numerical_string" ==

You set the length of the string to n fill it completly with "0" and set the base to k.
http://rosettacode.org/wiki/Increment_a_numerical_string#Pascal
Of course you can use an array of integer instead.
