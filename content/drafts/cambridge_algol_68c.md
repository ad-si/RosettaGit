+++
title = "Cambridge ALGOL 68C"
description = ""
date = 2009-02-03T15:25:57Z
aliases = []
[extra]
id = 2418
[taxonomies]
categories = []
tags = []
+++

{{implementation|ALGOL 68}}

### The <tt>ENVIRON</tt> and <tt>USING</tt> clauses.

These clauses are kind of the ''inverse'' of the '''#include''' found in the [[C|C programming language]], or '''import''' found in [[Python]]. The purpose of the <tt>ENVIRON</tt> mechanism is to allow a program source to be broken into manageable sized pieces. Note that it is only necessary to parse the shared source file once, unlike a '''#include''' found in the C programming language where the include file needs to be parsed for each source file that includes it.

### = Example of <tt>ENVIRON</tt> clause =

A file called ''mylib.a68'':

```txt

BEGIN
   INT dim = 3; # a constant #
   INT a number := 120; # a variable #
   ENVIRON EXAMPLE1;
   MODE MATRIX = [dim, dim]REAL; # a type definition #
   MATRIX m1;
   a number := ENVIRON EXAMPLE2;
   print((a number))
END

```


### = Example of <tt>USING</tt> clause =

A file called ''usemylib.a68'':

```txt

USING EXAMPLE2 FROM mylib
BEGIN
  MATRIX m2; # example only #
  print((a number)); # declared in mylib.a68 #
  print((2 UPB m1)); # also declared in mylib.a68 #
  ENVIRON EXAMPLE3;  # ENVIRONs can be nested #
  666
END

```


== Restrictions to the language from the standard '''ALGOL 68''' ==
* no algol68 FLEX and variable length arrays.
* <tt>MODE STRING</tt> implemented without FLEX.
* The PAR parallel clause was not implemented.
* nonstandard transput. 
* others...

A translator/compiler for ALGOL 68C was available for the PDP-10 and System/360 as well as a number of other computers.

==External links==
* [http://hopl.murdoch.edu.au/showlanguage.prx?exp=667 Cambridge Algol 68: on the historical roster of computer languages] - includes 10+ publication references.
* [http://portal.acm.org/ft_gateway.cfm?id=807148&type=pdf A TRANSPORTATION OF ALGOL68C - PJ Gardner, University of Essex] - March 1977 (From 370 to DECsystem-10)
