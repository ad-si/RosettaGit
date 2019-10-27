+++
title = "MNumerical Encryption/Decryption"
description = ""
date = 2009-05-26T15:13:54Z
aliases = []
[extra]
id = 1573
[taxonomies]
categories = []
tags = []
+++

These [[mIRC]] code snippets are not associated with any programming task.

=Number Encode/Decode=


```txt

  /*_____________________________________________________________________________
  |/
  /  Number Encode/Decode snippet by Haso "sm0kie_" Keric <Osah@comcast.net>
  |  Version 2.0, released 12/06 -- support on #script/irc.gamesurge.net
  |  Use/modify however you want, but please keep my name in it. Thank you!
  |
  |  Keywords: Security, Encryption, Decryption
  |
  |  This add-on provides a set of identifiers that can be useful to scripts
  |  that deal with security.
  |
  |  The script defines the following commands/identifiers:
  |
  |  $nEncode(Your String Here)
  |  Example: $nEncode(Hello my username is fred)
  |
  |    Encodes the text into numbers.
  |
  |  $nDecode(Your number string)
  |  Example: $nDecode(000-21-080808)
  |
  |    Decodes the numbers into letters, reaveling the encrypted string.
  |
  |
  |  Efficiency Test
  |
  |  200 Encryptions == 100% Efficient.
  |  200 Decryptions == 100% Efficient.
  |
  \
  _\_____________________________________________________________________________
  */

  alias nEncode {
    var %mystring = $1- | var %mystring.a = $replace(%mystring,A,1) | var %mystring.b = $replace(%mystring.a,B,2) | var %mystring.c = $replace(%mystring.b,C,3) | var %mystring.d = $replace(%mystring.c,D,4) | var %mystring.e = $replace(%mystring.d,E,5) | var %mystring.f = $replace(%mystring.e,F,6) | var %mystring.g = $replace(%mystring.f,G,7) | var %mystring.h = $replace(%mystring.g,H,8) | var %mystring.i = $replace(%mystring.h,I,9) | var %mystring.j = $replace(%mystring.i,J,01) | var %mystring.k = $replace(%mystring.j,K,02) | var %mystring.l = $replace(%mystring.k,L,03) | var %mystring.m = $replace(%mystring.l,M,04) | var %mystring.n = $replace(%mystring.m,N,05) | var %mystring.o = $replace(%mystring.n,O,06) | var %mystring.p = $replace(%mystring.o,P,07) | var %mystring.q = $replace(%mystring.p,Q,08) | var %mystring.r = $replace(%mystring.q,R,09)
    var %mystring.s = $replace(%mystring.r,S,001) | var %mystring.t = $replace(%mystring.s,T,002) | var %mystring.v = $replace(%mystring.t,V,003) | var %mystring.w = $replace(%mystring.v,W,004) | var %mystring.x = $replace(%mystring.w,X,005) | var %mystring.y = $replace(%mystring.x,Y,006) | var %mystring.z = $replace(%mystring.y,Z,007) | var %mystring.last = $replace(%mystring.z,u,008) | var %mystring.new = $replace(%mystring.last,$chr(32),-) | return %mystring.new
  }
  alias nDecode {
    var %mystring = $1- | var %mystring.last = $replace(%mystring,008,U) | var %mystring.z = $replace(%mystring.last,007,Z) | var %mystring.y = $replace(%mystring.z,006,Y) | var %mystring.x = $replace(%mystring.y,005,X) | var %mystring.w = $replace(%mystring.x,004,W) | var %mystring.v = $replace(%mystring.w,003,V) | var %mystring.t = $replace(%mystring.v,002,T) | var %mystring.s = $replace(%mystring.t,001,S) | var %mystring.r = $replace(%mystring.s,09,R) | var %mystring.q = $replace(%mystring.r,08,Q) | var %mystring.p = $replace(%mystring.q,07,P) | var %mystring.o = $replace(%mystring.p,06,O) | var %mystring.n = $replace(%mystring.o,05,N) | var %mystring.m = $replace(%mystring.n,04,M) | var %mystring.l = $replace(%mystring.m,03,L)
    var %mystring.k = $replace(%mystring.l,02,K) | var %mystring.j = $replace(%mystring.k,01,J) | var %mystring.i = $replace(%mystring.j,9,I) | var %mystring.h = $replace(%mystring.i,8,H) | var %mystring.g = $replace(%mystring.h,7,G) | var %mystring.f = $replace(%mystring.g,6,F) | var %mystring.e = $replace(%mystring.f,5,E) | var %mystring.d = $replace(%mystring.e,4,D) | var %mystring.c = $replace(%mystring.d,3,C) | var %mystring.b = $replace(%mystring.c,2,B) | var %mystring.a = $replace(%mystring.b,1,A) | return $replace(%mystring.a,-,$chr(32))
  }

```

