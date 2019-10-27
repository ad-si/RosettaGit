+++
title = "Template:Prelude/is prime.a68"
description = ""
date = 2010-10-11T15:39:57Z
aliases = []
[extra]
id = 4121
[taxonomies]
categories = []
tags = []
+++

COMMENT
   This routine is used in more than one place, and is essentially a
   template that can by used for many different types, eg INT, LONG INT...
 USAGE
   MODE ISPRIMEINT = INT, LONG INT, etc
   PR READ "prelude/is_prime.a68" PR
 END COMMENT

 PROC is prime = ( ISPRIMEINT p )BOOL:
   IF p <= 1 OR ( NOT ODD p AND p/= 2) THEN
     FALSE
   ELSE
     BOOL prime := TRUE;
     FOR i FROM 3 BY 2 TO ENTIER sqrt(p)
       WHILE prime := p MOD i /= 0 DO SKIP OD;
     prime
   FI
<noinclude>{{template}}</noinclude>
