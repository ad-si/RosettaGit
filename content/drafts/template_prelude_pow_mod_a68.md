+++
title = "Template:Prelude/pow mod.a68"
description = ""
date = 2010-10-11T15:40:58Z
aliases = []
[extra]
id = 4114
[taxonomies]
categories = []
tags = []
+++

COMMENT
   This routine is used in more than one place, and is essentially a
   template that can by used for many different types, eg INT, LONG INT...
 USAGE
   MODE POWMODSTRUCT = INT, LONG INT, COMPL, FRAC, MODULAS, MATRIX etc
   PR READ "prelude/pow_mod.a68" PR
 END COMMENT

 PROC pow mod = (POWMODSTRUCT b,in e, mod)POWMODSTRUCT: (
   POWMODSTRUCT sq := b, e := in e;
   POWMODSTRUCT out:= IF ODD e THEN b ELSE 1 FI;
   e:=e OVER 2;
   WHILE e /= 0 DO
     sq := sq * sq %* mod;
     IF ODD e THEN out := out * sq %* mod FI ;
     e:=e OVER 2
   OD;
   out
 )
<noinclude>{{template}}</noinclude>
