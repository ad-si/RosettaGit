+++
title = "ALGOL 68/prelude/errata.a68"
description = ""
date = 2012-10-07T01:16:28Z
aliases = []
[extra]
id = 12360
[taxonomies]
categories = []
tags = []
+++


```algol68
# -*- coding: utf-8 -*- #

##########################################################
# Errata: A collections of OPerators, MODES and variables#
#     that are "kind of" implied by Algol68's definition #
##########################################################

# Standard Diadic OPerator to initialise an object #
PRIO INIT = 1; # should be 0 like assignment #

INT extra = -5; # to remove or display additional decimal places in output #

COMMENT 
  #L# - MODE/type may also be prefixed with SHORT or LONG
  #l# - PROC/variable may also be prefixed with 'short' or 'long'
  #L# - MODE/type may also be prefixed with LONG for unicode
  #l# - PROC/variable may also be prefixed with 'long' for unicode
  #S# - Diadic OPerators concurrently involving LONG and SHORT
  #s# - Diadic+ PROCedure concurrently involving LONG and SHORT
  #LENG# & #SHORTEN# widening operators
END COMMENT

FORMAT 
  #l# bits repr := $g$,
  #l# int repr := $g(-0)$,
  #l# real repr := $g(-#l# real width-extra, #l# real width-2+extra)$,
  #l# compl repr := $f(#l# real repr)"⊥"f(#l# real repr)$,
  #u# string repr := $g$,
  #u# char repr := $g$,
      bool repr := $c("Yes","No")$;

FORMAT 
  fs := $", "$, # insert a field separator #
  #l# real repr fs := $f(#l# real repr)f(fs)$,
  nl := $l$, # insert a new line #
  #l# real item repr := $g"="f(#l# real repr)$, # e.g. "value=1.00000; " #
  #l# int item repr := $g"="f(#l# int repr)$, # e.g. "value=1; " #
  item repr := $g"="g$; # e.g. "value=1; " #

FORMAT hr = $68"-"l$;

MODE SLICE = FLEX[0]STRUCT(INT lwb, upb, by); # for tensor slicing #
FORMAT slice repr = $"["g(-0)":"g(-0)":"g(-0)"]"$;

MODE BOUNDS = FLEX[0]STRUCT(INT lwb, upb); # for tensor slicing #
FORMAT bounds repr = $"["g(-0)":"g(-0)"]"$;
OP LWBUPB = ([]INT x)BOUNDS: STRUCT(INT lwb, upb)(LWB x, UPB x);
OP LWBUPB = ([,]INT x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x));
OP LWBUPB = ([,,]INT x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x),(3 LWB x, 3 UPB x));
OP LWBUPB = ([,,,]INT x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x),(3 LWB x, 3 UPB x),(4 LWB x, 4 UPB x));
OP LWBUPB = ([]REAL x)BOUNDS: STRUCT(INT lwb, upb)(LWB x, UPB x);
OP LWBUPB = ([,]REAL x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x));
OP LWBUPB = ([,,]REAL x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x),(3 LWB x, 3 UPB x));
OP LWBUPB = ([,,,]REAL x)[]BOUNDS: BOUNDS((LWB x, UPB x),(2 LWB x, 2 UPB x),(3 LWB x, 3 UPB x),(4 LWB x, 4 UPB x));
# SHORT/LONG COMPL etc #

# Some base routined for generators: #
MODE
  #L#BITSYIELD=  PROC(#L#BITS)VOID,  #L#BITSGEN=  PROC(#L#BITSYIELD)VOID,
  #L#BYTESYIELD= PROC(#L#BYTES)VOID, #L#BYTESGEN= PROC(#L#BYTESYIELD)VOID,
  #L#INTYIELD=   PROC(#L#INT)VOID,   #L#INTGEN=   PROC(#L#INTYIELD)VOID,
  #L#REALYIELD=  PROC(#L#REAL)VOID,  #L#REALGEN=  PROC(#L#REALYIELD)VOID,
  #L#COMPLYIELD= PROC(#L#COMPL)VOID, #L#COMPLGEN= PROC(#L#COMPLYIELD)VOID,
  #L#STRINGYIELD=PROC(#L#STRING)VOID,#L#STRINGGEN=PROC(#L#STRINGYIELD)VOID,
  #U#CHARYIELD=  PROC(#U#CHAR)VOID,  #U#CHARGEN=  PROC(#U#CHARYIELD)VOID,
     BOOLYIELD=  PROC(   BOOL)VOID,     BOOLGEN=  PROC(   BOOLYIELD)VOID;

# Manage optionally uninitialised variables #
MODE
  #L#BITSOPT   = UNION(VOID, #L#BITS),
  #L#BYTESOPT  = UNION(VOID, #L#BYTES),
  #L#INTOPT    = UNION(VOID, #L#INT),
  #L#REALOPT   = UNION(VOID, #L#REAL),
  #L#COMPLOPT  = UNION(VOID, #L#COMPL),
  #L#STRINGOPT = UNION(VOID, #L#STRING),
  #U#CHAROPT   = UNION(VOID, #U#CHAR),
     BOOLOPT   = UNION(VOID,    BOOL);

PRIO ORELSE = 2;
# OPerator to return a "default" value if the OPTion is undefined #
OP
  ORELSE=(#L#BITSOPT   val,#L#BITS   def)BITS:  (val|(#L#BITS   out):out|def),
  ORELSE=(#L#BYTESOPT  val,#L#BYTES  def)BYTES: (val|(#L#BYTES  out):out|def),
  ORELSE=(#L#INTOPT    val,#L#INT    def)INT:   (val|(#L#INT    out):out|def),
  ORELSE=(#L#REALOPT   val,#L#REAL   def)REAL:  (val|(#L#REAL   out):out|def),
  ORELSE=(#L#COMPLOPT  val,#L#COMPL  def)COMPL: (val|(#L#COMPL  out):out|def),
  ORELSE=(#L#STRINGOPT val,#L#STRING def)STRING:(val|(#L#STRING out):out|def),
  ORELSE=(#U#CHAROPT   val,#U#CHAR   def)CHAR:  (val|(#U#CHAR   out):out|def),
  ORELSE=(   BOOLOPT   val,   BOOL   def)BOOL:  (val|(   BOOL   out):out|def);
# SHORT/LONG etc. #

OP
# OPerator to determin is an OPTion is defined #
  HASOPT = (#L#BITSOPT   val)BOOL: ( val | (#L#BITS   out): TRUE | FALSE),
  HASOPT = (#L#BYTESOPT  val)BOOL: ( val | (#L#BYTES  out): TRUE | FALSE),
  HASOPT = (#L#INTOPT    val)BOOL: ( val | (#L#INT    out): TRUE | FALSE),
  HASOPT = (#L#REALOPT   val)BOOL: ( val | (#L#REAL   out): TRUE | FALSE),
  HASOPT = (#L#COMPLOPT  val)BOOL: ( val | (#L#COMPL  out): TRUE | FALSE),
  HASOPT = (#L#STRINGOPT val)BOOL: ( val | (#L#STRING out): TRUE | FALSE),
  HASOPT = (#U#CHAROPT   val)BOOL: ( val | (#U#CHAR   out): TRUE | FALSE),
  HASOPT = (   BOOLOPT   val)BOOL: ( val | (   BOOL   out): TRUE | FALSE);
# SHORT/LONG etc. #

# Note: ℵ indicates attribute is "private", and 
        should not be used outside of this prelude #

MODE  # limited to 4 dimensions #
  REFBITSARRAY =UNION(#L#REF BITS, []#L#REF BITS, [,]#L#REF BITS, [,,]#L#REF BITS, [,,,]#L#REF BITS),
  REFINTARRAY  =UNION(#L#REF INT,  []#L#REF INT,  [,]#L#REF INT,  [,,]#L#REF INT,  [,,,]#L#REF INT),
  REFREALARRAY =UNION(#L#REF REAL, []#L#REF REAL, [,]#L#REF REAL, [,,]#L#REF REAL, [,,,]#L#REF REAL),
  REFCOMPLARRAY=UNION(#L#REF COMPL,[]#L#REF COMPL,[,]#L#REF COMPL,[,,]#L#REF COMPL,[,,,]#L#REF COMPL),
  REFCHARARRAY =UNION(#U#REF CHAR, []#U#REF CHAR, [,]#U#REF CHAR, [,,]#U#REF CHAR, [,,,]#U#REF CHAR),
  REFBOOLARRAY =UNION(   REF BOOL, []   REF BOOL, [,]   REF BOOL, [,,]   REF BOOL, [,,,]   REF BOOL);

# n.b. cannot handle STRUCTs #
MODE #ℵ#SIMPLEIN = UNION(
  REFBITSARRAY,REFINTARRAY,REFREALARRAY,REFCOMPLARRAY,REFCHARARRAY,REFBOOLARRAY
);

MODE  # limited to 4 dimensions #
  BITSARRAY =UNION(#L#BITS, []#L#BITS, [,]#L#BITS, [,,]#L#BITS, [,,,]#L#BITS),
  INTARRAY  =UNION(#L#INT,  []#L#INT,  [,]#L#INT,  [,,]#L#INT,  [,,,]#L#INT),
  REALARRAY =UNION(#L#REAL, []#L#REAL, [,]#L#REAL, [,,]#L#REAL, [,,,]#L#REAL),
  COMPLARRAY=UNION(#L#COMPL,[]#L#COMPL,[,]#L#COMPL,[,,]#L#COMPL,[,,,]#L#COMPL),
  CHARARRAY =UNION(#U#CHAR, []#U#CHAR, [,]#U#CHAR, [,,]#U#CHAR, [,,,]#U#CHAR),
  BOOLARRAY =UNION(   BOOL, []   BOOL, [,]   BOOL, [,,]   BOOL, [,,,]   BOOL);

# n.b. cannot handle STRUCTs #
MODE #ℵ#SIMPLEOUT = UNION(
  BITSARRAY, INTARRAY, REALARRAY, COMPLARRAY, CHARARRAY, BOOLARRAY
);

MODE NEWIO = PROC(REF FILE)VOID;

MODE # limited to 4 dimensions #
  #ℵ#SIMPLEOUTA = [0]SIMPLEOUT,
  #ℵ#SIMPLEOUTB = [0]UNION(SIMPLEOUT, SIMPLEOUTA),
  #ℵ#SIMPLEOUTC = [0]UNION(SIMPLEOUT, SIMPLEOUTA, SIMPLEOUTB),
  OUTMODE       = [0]UNION(SIMPLEOUT, SIMPLEOUTA, SIMPLEOUTB, SIMPLEOUTC, NEWIO),
  OUTMODEF      = [0]UNION(SIMPLEOUT, SIMPLEOUTA, SIMPLEOUTB, SIMPLEOUTC, FORMAT),

  #ℵ#SIMPLEINA = [0]SIMPLEIN,
  #ℵ#SIMPLEINB = [0]UNION(SIMPLEIN, SIMPLEINA),
  #ℵ#SIMPLEINC = [0]UNION(SIMPLEIN, SIMPLEINA, SIMPLEINB),
  INMODE       = [0]UNION(SIMPLEIN, SIMPLEINA, SIMPLEINB, SIMPLEINC, NEWIO),
  INMODEF      = [0]UNION(SIMPLEIN, SIMPLEINA, SIMPLEINB, SIMPLEINC, FORMAT);

COMMENT
PROC sget = (STRING in s, INMODE list)VOID:  raise unimplemented("sget");

PROC sgetf = (STRING in s, INMODEF list)VOID:  (
  FILE file; 
  STRING s := in s; 
  associate(file, s);
  getf(file,list);
  close(file)
);
END COMMENT

PROC type of = (OUTMODEF list)STRING: (
  STRING out := "(";
  STRING sep := "";
  
  FOR i TO UPB list DO
    out +:= sprint(i);
    CASE list[i] IN
      (#L# FORMAT):print("#L#FORMAT"),
CO    (#L# PROC(#L#REF #L#FILE)#L#VOID):print("#L#NEWIO"), CO
      (#L# BITS  v):sprint(("#L#BITS=",v)),
      (#L# INT   v):sprint(("#L#INT=",v)),
      (#L# REAL  v):sprint(("#L#REAL=",v)),
      (#L# COMPL v):sprint(("#L#COMPL=",v)),
      (#U# CHAR  v):sprint(("#U#CHAR=",v)),
      (    BOOL  v):sprint(("BOOL=",v)),
      ([]#L# BITS  v):sprint(("[]#L#BITS=",v)),
      ([]#L# INT   v):sprint(("[]#L#INT=",v)),
      ([]#L# REAL  v):sprint(("[]#L#REAL=",v)),
      ([]#L# COMPL v):sprint(("[]#L#COMPL=",v)),
      ([]#U# CHAR  v):sprint(("[]#U#CHAR=",v)),
      ([]    BOOL  v):sprint(("[]BOOL=",v)),
      ([,]#L# BITS  v):sprint(("[,]#L#BITS=",v)),
      ([,]#L# INT   v):sprint(("[,]#L#INT=",v)),
      ([,]#L# REAL  v):sprint(("[,]#L#REAL=",v)),
      ([,]#L# COMPL v):sprint(("[,]#L#COMPL=",v)),
      ([,]#U# CHAR  v):sprint(("[,]#U#CHAR=",v)),
      ([,]    BOOL  v):sprint(("[,]BOOL=",v)),
      ([,,]#L# BITS  v):sprint(("[,,]#L#BITS=",v)),
      ([,,]#L# INT   v):sprint(("[,,]#L#INT=",v)),
      ([,,]#L# REAL  v):sprint(("[,,]#L#REAL=",v)),
      ([,,]#L# COMPL v):sprint(("[,,]#L#COMPL=",v)),
      ([,,]#U# CHAR  v):sprint(("[,,]#U#CHAR=",v)),
      ([,,]    BOOL  v):sprint(("[,,]BOOL=",v))
CO    (#L# BYTES v):print(("#L#BYTES",[]#L#CHAR(v)))CO
    OUT
      sprint("REF[]STRUCT or SHORT/LONG etc")
    ESAC;
    sep := ","
  OD;
  out+")"
);

PROC sput = (REF STRING out, OUTMODE list)VOID: (
  FILE file; 
  associate(file, out);
  put(file,list);
  close(file);
  out
);

PROC sputf = (REF STRING out, OUTMODEF list)STRING:  (
  FILE file; 
  associate(file, out);
  CASE list[1] IN
    (FORMAT f):putf(file, (list[1], list[2:])) #BF#
  OUT  
    putf(file,list)
  ESAC;
  close(file);
  out
);

PROC sprint = (OUTMODE list)STRING: (
  STRING out;
  sput(out, list);
  out
);

PROC sprintf = (OUTMODEF list)STRING:  (
  STRING out;
  sputf(out, list);
  out
);

SKIP
```

