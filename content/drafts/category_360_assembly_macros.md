+++
title = "Category:360 Assembly macros"
description = ""
date = 2018-07-17T08:58:55Z
aliases = []
[extra]
id = 21903
[taxonomies]
categories = []
tags = []
+++

This category uses the form [[Has default form::Solution]].
[[Category:360 Assembly]]
==360 Assembly Structured Macros==
There is a long history of IBM Assembler 370 Structured Macros. An early very well known version is found in JES328X product. Now there as part of SPM (Structured Programming Macros) concept. A recent version is HLASM Toolkit Features User's Guide (GC26-8710-10) http://publibz.boulder.ibm.com/epubs/pdf/asmtug21.pdf .
You will found : 

```360asm

IF ... THEN
ELSE
ENDIF
DO WHILE=(...)
ENDDO
DO UNTIL=(...)
ENDDO

```

The classic macro REGEQU for refrerencing the registers is found in all good MACLIBs.


PROLOG and EPILOG are not part of this set my own version is:

```360asm
         MACRO
&N       PROLOG          
&N       CSECT  
         USING &N,13
@SAVEAR  B     @STM-@SAVEAR(15)
         DC    17F'0'
         DC    CL8'&N'
@STM     STM   14,12,12(13)
         ST    13,4(15)
         ST    15,8(13)
         LR    13,15
         MEND
```


```360asm
         MACRO
&N       EPILOG          
&N       CNOP  0,4
         L     13,4(0,13)
         LM    14,12,12(13)
         XR    15,15
         BR    14
         MEND
```


==See also==
* HLASM Programmer's Guide (SC26-4941-06)
http://publibz.boulder.ibm.com/epubs/pdf/asmp1021.pdf
* HLASM Language Reference (SC26-4940-06)
http://publibz.boulder.ibm.com/epubs/pdf/asmr1021.pdf
* HLASM Toolkit Features User's Guide (GC26-8710-10)
http://publibz.boulder.ibm.com/epubs/pdf/asmtug21.pdf



http://rosettacode.org/wiki/Category:360_Assembly_macros
