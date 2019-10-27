+++
title = "LINESIZE.HEL"
description = ""
date = 2017-09-07T00:02:23Z
aliases = []
[extra]
id = 12934
[taxonomies]
categories = []
tags = []
+++

The   '''LINESIZE.HEL'''   is the HELp documentation for the   '''LINESIZE.REX'''   (REXX) program.

```txt

 ╔══════════════════════════════════════════════════════════════════════╗
 ║ REXX function to return the screen width (linesize) as a REXX result.║
 ║                                                                      ║
 ║ The Environment variable   COLUMNS  is examined,  and  if present,   ║
 ║ its value is extracted and returned.                                 ║
 ║                                                                      ║
 ║ If the  EnvVar  variable  isn't defined,  the DOS command   MODE CON ║
 ║ is issued  and it's output is filtered  (with FIND)  and written to  ║
 ║ a temporary file,  and is scanned for the line:    Columns [:=] nn   ║
 ║                                                                      ║
 ║ If the value isn't available or invalid, a value of zero is returned.║
 ║                                                                      ║
 ║ If program is running under  PC/REXX,  CMS,  or  R3,  this routine   ║
 ║ should never be invoked  as  those REXXes have their own built─in    ║
 ║ function  LINESIZE.                                                  ║
 ║                                                                      ║
 ║ This program shouldn't be "compiled" with PC/REXX with the  /L  opt. ║
 ╚══════════════════════════════════════════════════════════════════════╝

                                      Ω

```


[[Category:REXX library routines]]
