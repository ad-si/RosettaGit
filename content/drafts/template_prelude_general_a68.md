+++
title = "Template:Prelude/general.a68"
description = ""
date = 2011-11-15T04:46:22Z
aliases = []
[extra]
id = 10385
[taxonomies]
categories = []
tags = []
+++


```algol68
# -*- coding: utf-8 -*- #

COMMENT
  This is an ALGOL 68 prelude file called prelude/general.
  It contains small routine that are not part of any standard, 
  but are none the less useful and widely used.
USAGE
  PR READ "prelude/general.a68" PR
END COMMENT

##########################################
# Define some general routines and MODES #
##########################################
PRIO MIN=8, MAX=8;
OP MIN = (INT a,b)INT: (a<b|a|b),
   MAX = (INT a,b)INT: (a>b|a|b);
OP MIN = (REAL a,b)REAL: (a<b|a|b),
   MAX = (REAL a,b)REAL: (a>b|a|b);

MODE IUR = UNION(INT, REAL);
OP R = (IUR x)REAL: (x|(REAL r):r, (INT i):REAL(i));

MODE IURUC = UNION(INT, REAL, COMPL);
OP C = (IURUC x)COMPL: (x|(COMPL c): c, (REAL r):COMPL(r), (INT i):COMPL(i));

MODE CUCCUS = UNION(CHAR, #[]CHAR,# STRING);
OP S = (CUCCUS x)STRING: (x|(STRING s):s, ([]CHAR cc): STRING(cc), (CHAR c):STRING(c));

MODE 
  UCHAR =  STRING, LONGCHAR = UCHAR, USTRING=FLEX[0]LONGCHAR,
  LBITS =  UNION(SHORT SHORT BITS,  SHORT BITS,  BITS,  LONG BITS,  LONG LONG BITS),
  LBYTES = UNION(SHORT SHORT BYTES, SHORT BYTES, BYTES, LONG BYTES, LONG LONG BYTES),
  LINT =   UNION(SHORT SHORT INT,   SHORT INT,   INT,   LONG INT,   LONG LONG INT),
  LREAL =  UNION(SHORT SHORT REAL,  SHORT REAL,  REAL,  LONG REAL,  LONG LONG REAL),
  LCOMPL = UNION(SHORT SHORT COMPL, SHORT COMPL, COMPL, LONG COMPL, LONG LONG COMPL);
MODE SIMPLEOUT = [0]UNION(BOOL, CHAR, STRING, USTRING, LBITS, # LBYTES,# LINT, LREAL, LCOMPL);
MODE SIMPLEOUTF = [0]UNION(FORMAT, SIMPLEOUT);
MODE USIMPLEOUT = SIMPLEOUT # UNION(SIMPLEOUT, SIMPLEOUTF) #;

PRIO REPR = 8;

# Use MOID with "*:=" OPerators who's where the returned MODE is often VOIDed for convenience #
MODE MOID = VOID;

PROC raise exception = (STRING type, USIMPLEOUT argv)VOID:(
  BOOL exception = FALSE;
  putf(stand error, ($g$, "Exception"," ",type, ": "));
  #CASE argv IN#
  #  (SIMPLEOUSTRING argv):putf(stand error, argv),#
  #  (SIMPLEOUT argv):#
        putf(stand error, ($g" "$, argv))
  #ESAC#;
  putf(stand error, $l$);
  ASSERT (exception)
);

# Python style exceptions #
PROC raise undefined =   (USIMPLEOUT argv)VOID: raise exception("Undefined", argv);
PROC raise unimplemented=(USIMPLEOUT argv)VOID: raise exception("Unimplemented", argv);
PROC raise value error = (USIMPLEOUT argv)VOID: raise exception("Value Error",argv);
PROC raise index error = (USIMPLEOUT argv)VOID: raise exception("Index Error",argv);
PROC raise type error =  (USIMPLEOUT argv)VOID: raise exception("Type Error",argv);

BOOL debug := FALSE, trace := FALSE;

PRIO INKEYS = 5, INVALUES = 5, NOTINKEYS = 5, NOTINVALUES = 5;

# Algol68G specific declarations #
INT match=0, no match=1, out of memory error=2, other error=3; # for RegEx searches #
```

<noinclude>{{template}}</noinclude>
