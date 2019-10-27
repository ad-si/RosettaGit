+++
title = "Talk:Playfair cipher"
description = ""
date = 2014-05-28T19:22:36Z
aliases = []
[extra]
id = 17654
[taxonomies]
categories = []
tags = []
+++

== Inadequate spec ==

How should one encode a sentence containing "xxx"? Or should the implementation crash for that case? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:04, 28 May 2014 (UTC)

-----

The REXX program handles these two examples (below) without problems.

Note the   '''('''   [left parenthesis] is used to parse the plaintext from any specified options.

'''output''' when using the input of:   <tt> ( I like XXX better than XX beer. </tt>

```txt

old cipher key:  Playfair example.    ◄───using the default.
new cipher key:  PLAYFIREXM
     omit char:  J
   double char:  X
 original text:  I like XXX better then XX beer.

 cleansed text:  IL IK EX XX BE TT ER TH EN XX BE ER
                 ILIKEXXXBETTERTHENXXBEER

encrypted text:  RP BT XM MM MM DI WI VI IU DM QR MM DI MX EM
                 RPBTXMMMMMDIWIVIIUDMQRMMDIMXEM

    plain text:  IL IK EX XX XX BE TX TE RT HE NX XX BE XE RX
                 ILIKEXXXXXBETXTERTHENXXXBEXERX

 possible text:  IL IK EX XX BE TT ER TH EN XX BE ER
                 ILIKEXXXBETTERTHENXXBEER
 original text:  ILIKEXXXBETTERTHENXXBEER

════════════════Playfair encryption──► decryption──► encryption worked.

```

'''output''' when the input is:   <tt> (triple xxx is like, thirty, dude.  No XXXX though. Bummer. </tt>

```txt

old cipher key:  Playfair example.    ◄───using the default.
new cipher key:  PLAYFIREXM
     omit char:  J
   double char:  X
 original text:  triple xxx is like, thirty, dude. No XXXX though. Bummer.

 cleansed text:  TR IP LE XX XI SL IK ET HI RT YD UD EN OX XX XT HO UG HB UM ME R
                 TRIPLEXXXISLIKETHIRTYDUDENOXXXXTHOUGHBUMMER

encrypted text:  UI BI AR MM MM MR NF BT IV BM IU AG VC RO QE MM MM MM ZB NV HB CT IM IX EM
                 UIBIARMMMMMRNFBTIVBMIUAGVCROQEMMMMMMZBNVHBCTIMIXEM

    plain text:  TR IP LE XX XX XI SL IK ET HI RT YD UD EN OX XX XX XX TH OU GH BU MX ME RX
                 TRIPLEXXXXXISLIKETHIRTYDUDENOXXXXXXXTHOUGHBUMXMERX

 possible text:  TR IP LE XX XI SL IK ET HI RT YD UD EN OX XX XT HO UG HB UM ME R
                 TRIPLEXXXISLIKETHIRTYDUDENOXXXXTHOUGHBUMMER
 original text:  TRIPLEXXXISLIKETHIRTYDUDENOXXXXTHOUGHBUMMER

════════════════Playfair encryption──► decryption──► encryption worked.

```


::Of course this kind of thing is doable. But it's also unspecified behavior. I adopted something which I felt was closer to the spirit of the spec, for the J implementation, mangling some repeated X's in the process. Here's how that looks using your test cases:

::
```J
   choose 'IJ'

   setkey 'playfirexm'
   encrypt 'I like XXX better than XX beer.'
RPBTXMGWDIWIXEZBLOGWDIXE
   decrypt encrypt 'I like XXX better than XX beer.'
ILIKEXXQBETXERTHANXQBEER
   encrypt 'triple xxx is like, thirty, dude. No XXXX though. Bummer.'
UIBIARGWMRNFBTIVBMIUAGVCROQEGWIWDSWCBCZRIXEM
   decrypt encrypt 'triple xxx is like, thirty, dude. No XXXX though. Bummer.'
TRIPLEXQXISLIKETHIRTYDUDENOXXQXTHOUGHBUMMERX
```


::I'm not saying that this is right, nor that yours is wrong. I am saying that the spec is not clear about how to handle this case. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:57, 28 May 2014 (UTC)

::: Right or wrong, the REXX version does encrypt/decrypt the above two messages correctly.   It's not 100% successful in handling all cases that contain replicated characters however.   As to the specs, yes, I agree, it's rather fuzzy, especially about the part that states "... dropping any extra "X"es, or "Q"s that do not make sense in the final message...     That's pretty hard to do problematically. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:22, 28 May 2014 (UTC)
