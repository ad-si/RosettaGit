+++
title = "SCRSIZE.HEL"
description = ""
date = 2017-09-07T00:03:19Z
aliases = []
[extra]
id = 12935
[taxonomies]
categories = []
tags = []
+++

The   '''SCRSIZE.HEL'''   is the HELp documentation for the   '''SCRSIZE.REX'''   (REXX) program.

```txt

 ╔══════════════════════════════════════════════════════════════════════╗
 ║ REXX function to return the screen depth and screen width (as a REXX ║
 ║ RESULT).                                                             ║
 ║                                                                      ║
 ║ The two Environment variables   COLUMNS  and   LINES   are examined, ║
 ║ and if present,  their values are extracted and returned.            ║
 ║                                                                      ║
 ║ If the  EnvVar variables aren't defined,  the DOS command   MODE CON ║
 ║ is issued and it's output is filtered (with FIND)  and written to a  ║
 ║ temporary file, and is scanned for the lines:  Columns [:=] nn       ║
 ║                                         and:   Lines   [:=] nn       ║
 ║                                                                      ║
 ║ If the values  aren't  available  or  invalid,  a value of  zero(s)  ║
 ║ is/are returned.                                                     ║
 ║                                                                      ║
 ║ If program is running under  PC/REXX  or  R4,  this routine should   ║
 ║ never be invoked as  PC/REXX  and  R4  have their own built-in       ║
 ║ function (BIF)    SCRSIZE.                                           ║
 ║                                                                      ║
 ║ This program shouldn't be "compiled" with PC/REXX with the  /L  opt. ║
 ╚══════════════════════════════════════════════════════════════════════╝

                                      Ω

```


[[Category:REXX library routines]]
