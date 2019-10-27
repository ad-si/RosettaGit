+++
title = "ALGOL 68R"
description = ""
date = 2015-08-30T09:08:43Z
aliases = []
[extra]
id = 3143
[taxonomies]
categories = []
tags = []
+++

{{stub}}
{{wikipedia}}{{implementation|ALGOL 68}}
ALGOL 68R from [[wp:Royal_Radar_Establishment|RRE]] was the first [[ALGOL 68]] subset implementation, running on the [[wp:ICL_1900|ICL 1900]].
Based on the original language, the main subset restrictions were ''definition before use'' and no parallel processing.
This compiler was popular in [[wp:United_Kingdom|UK]] universities in the 1970s, where many [[wp:computer_science|computer science]] students learnt [[ALGOL 68]] as their first programming language; the compiler was renowned for good error messages.

== Differences between Algol 68R and the Revised Report ==

Algol 68R was designed to implement a dialect of the language of the original report so there are many language differences compared to the revised report.
Some of the more visibly obvious include:

* Loops do not end with OD, the loop body (probably an encloaed clause) follows DO, e.g. TO 10 DO BEGIN something; something else END
* The CASE conformity clause (used to get values out of a UNION) is significantly different.
* ELSF is used instead of ELIF.
* OUSE (contraction of OUT CASE) is not available.
* The mode COMPL is spelt COMPLEX.

There are many other differences, not all of which are syntatic.

== Download ==

Details of how to run Algol 68R under a George 3 emulator can be found at the links below. The emulator and Algol 68R can be run on a number of systems, including the Raspberry Pi.

* http://sw.ccs.bcs.org/CCs/g3/index.html 
* http://sw.ccs.bcs.org/CCs/g3/g3pi.htm

Note that this software is strictly for non-commercial use.
