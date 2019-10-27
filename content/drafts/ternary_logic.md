+++
title = "Ternary logic"
description = ""
date = 2019-10-18T20:28:35Z
aliases = []
[extra]
id = 10384
[taxonomies]
categories = []
tags = []
+++

{{task|Logic}}
{{wikipedia|Ternary logic}}



In [[wp:logic|logic]], a '''three-valued logic''' (also '''trivalent''', '''ternary''', or  '''trinary logic''', sometimes abbreviated '''3VL''') is any of several [[wp:many-valued logic|many-valued logic]] systems in which there are three [[wp:truth value|truth value]]s indicating ''true'', ''false'' and some indeterminate third value.  

This is contrasted with the more commonly known [[wp:Principle of bivalence|bivalent]] logics (such as classical sentential or [[wp:boolean logic|boolean logic]]) which provide only for ''true'' and ''false''. 

Conceptual form and basic ideas were initially created by [[wp:Jan Łukasiewicz|Łukasiewicz]], [[wp:C. I. Lewis|Lewis]]  and [[wp:Sulski|Sulski]]. 

These were then re-formulated by [[wp:Grigore Moisil|Grigore Moisil]] in an axiomatic algebraic form, and also extended to ''n''-valued logics in 1945.
{|
|+'''Example ''Ternary Logic Operators'' in ''Truth Tables'':'''
|-
|
{| class=wikitable
|+''not'' a
|-
! colspan=2 | &not;
|-
| True || False
|-
| Maybe || Maybe
|-
| False || True
|}
||
{| class=wikitable
|+a ''and'' b
|-
! &and;
| True || Maybe || False
|-
| True || True || Maybe || False
|-
| Maybe || Maybe || Maybe || False
|-
| False || False || False || False
|}
||
{| class=wikitable
|-
|+a ''or'' b
|-
! &or;
| True || Maybe || False
|-
| True || True || True || True
|-
| Maybe || True || Maybe || Maybe
|-
| False || True || Maybe || False
|}
|-
||
{| class=wikitable
|-
|+''if'' a ''then'' b
|-
! ⊃
| True || Maybe || False
|-
| True || True || Maybe || False
|-
| Maybe || True || Maybe || Maybe
|-
| False || True || True || True
|}
||
{| class=wikitable
|-
|+a ''is equivalent to'' b
|-
! ≡
| True || Maybe || False
|-
| True || True || Maybe || False
|-
| Maybe || Maybe || Maybe || Maybe
|-
| False || False || Maybe || True
|}
|}


;Task:
* Define a new type that emulates ''ternary logic'' by storing data '''trits'''.
* Given all the binary logic operators of the original programming language, reimplement these operators for the new ''Ternary logic'' type '''trit'''.
* Generate a sampling of results using '''trit''' variables.
* [[wp:Kudos|Kudos]] for actually thinking up a test case algorithm where ''ternary logic'' is intrinsically useful, optimises the test case algorithm and is preferable to binary logic.



Note:   '''[[wp:Setun|Setun]]'''   (Сетунь) was a   [[wp:balanced ternary|balanced ternary]]   computer developed in 1958 at   [[wp:Moscow State University|Moscow State University]].   The device was built under the lead of   [[wp:Sergei Sobolev|Sergei Sobolev]]   and   [[wp:Nikolay Brusentsov|Nikolay Brusentsov]].   It was the only modern   [[wp:ternary computer|ternary computer]],   using three-valued [[wp:ternary logic|ternary logic]]





## Ada


We first specify a package "Logic" for three-valued logic. Observe that predefined Boolean functions, "and" "or" and "not" are overloaded:

```Ada
package Logic is
   type Ternary is (True, Unknown, False); 

   -- logic functions
   function "and"(Left, Right: Ternary) return Ternary;
   function "or"(Left, Right: Ternary) return Ternary;
   function "not"(T: Ternary) return Ternary;
   function Equivalent(Left, Right: Ternary) return Ternary;
   function Implies(Condition, Conclusion: Ternary) return Ternary;

   -- conversion functions
   function To_Bool(X: Ternary) return Boolean;
   function To_Ternary(B: Boolean) return Ternary;
   function Image(Value: Ternary) return Character;
end Logic;
```


Next, the implementation of the package:


```Ada
package body Logic is
   -- type Ternary is (True, Unknown, False);

   function Image(Value: Ternary) return Character is
   begin
      case Value is
         when True    => return 'T';
         when False   => return 'F';
         when Unknown => return '?';
      end case;
   end Image;

   function "and"(Left, Right: Ternary) return Ternary is
   begin
      return Ternary'max(Left, Right);
   end "and";

   function "or"(Left, Right: Ternary) return Ternary is
   begin
      return Ternary'min(Left, Right);
   end "or";

   function "not"(T: Ternary) return Ternary is
   begin
      case T is
         when False   => return True;
         when Unknown => return Unknown;
         when True    => return False;
      end case;
   end "not";

   function To_Bool(X: Ternary) return Boolean is
   begin
      case X is
         when True  => return True;
         when False => return False;
         when Unknown => raise Constraint_Error;
      end case;
   end To_Bool;

   function To_Ternary(B: Boolean) return Ternary is
   begin
      if B then
         return True;
      else
         return False;
      end if;
   end To_Ternary;

   function Equivalent(Left, Right: Ternary) return Ternary is
   begin
      return To_Ternary(To_Bool(Left) = To_Bool(Right));
   exception
      when Constraint_Error => return Unknown;
   end Equivalent;

   function Implies(Condition, Conclusion: Ternary) return Ternary is
   begin
      return (not Condition) or Conclusion;
   end Implies;

end Logic;
```


Finally, a sample program:

```Ada
with Ada.Text_IO, Logic;

procedure Test_Tri_Logic is

   use Logic;

   type F2 is access function(Left, Right: Ternary) return Ternary;
   type F1 is access function(Trit: Ternary) return Ternary;

   procedure Truth_Table(F: F1; Name: String) is
   begin
      Ada.Text_IO.Put_Line("X | " & Name & "(X)");
      for T in Ternary loop
         Ada.Text_IO.Put_Line(Image(T) & " |  " & Image(F(T)));
      end loop;
   end Truth_Table;

   procedure Truth_Table(F: F2; Name: String) is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("X | Y | " & Name & "(X,Y)");
      for X in Ternary loop
         for Y in Ternary loop
            Ada.Text_IO.Put_Line(Image(X) & " | " & Image(Y) & " |  " & Image(F(X,Y)));
         end loop;
      end loop;
   end Truth_Table;

begin
   Truth_Table(F => "not"'Access, Name => "Not");
   Truth_Table(F => "and"'Access, Name => "And");
   Truth_Table(F => "or"'Access, Name => "Or");
   Truth_Table(F => Equivalent'Access, Name => "Eq");
   Truth_Table(F => Implies'Access, Name => "Implies");
end Test_Tri_Logic;
```


{{out}}

```txt
X | Not(X)
T |  F
? |  ?
F |  T

X | Y | And(X,Y)
T | T |  T
T | ? |  ?
T | F |  F
? | T |  ?
? | ? |  ?
? | F |  F
F | T |  F
F | ? |  F
F | F |  F

... (and so on)
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, like C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: Ternary_logic.a68'''

```algol68
# -*- coding: utf-8 -*- #

INT trit width = 1, trit base = 3;
MODE TRIT = STRUCT(BITS trit);
CO FORMAT trit fmt = $c("?","⌈","⌊",#|"~"#)$; CO

# These values treated are as per "Balanced ternary" #
# eg true=1, maybe=0, false=-1 #
TRIT true =INITTRIT 4r1, maybe=INITTRIT 4r0,
     false=INITTRIT 4r2;

# Warning: redefines standard builtins flip & flop #
LONGCHAR flap="?", flip="⌈", flop="⌊";

OP REPR = (TRIT t)LONGCHAR:
  []LONGCHAR(flap, flip, flop)[1+ABS trit OF t];

############################################
# Define some OPerators for coercing MODES #
############################################
OP INITTRIT = (BOOL in)TRIT:
  (in|true|false);

OP INITBOOL = (TRIT in)BOOL:
  (trit OF in=trit OF true|TRUE|:trit OF in=trit OF false|FALSE|
    raise value error(("vague TRIT to BOOL coercion: """, REPR in,""""));~
  );
OP B = (TRIT in)BOOL: INITBOOL in;

# These values treated are as per "Balanced ternary" #
# n.b true=1, maybe=0, false=-1 #
# Warning: BOOL ABS FALSE (0) is not the same as TRIT ABS false (-1) #

OP INITINT = (TRIT t)INT:
  CASE 1+ABS trit OF t
  IN #maybe# 0, #true # 1, #false#-1
  OUT raise value error(("invalid TRIT value",REPR t)); ~
  ESAC;

OP INITTRIT = (INT in)TRIT: (
  TRIT out;
  trit OF out:= trit OF
    CASE 2+in
    IN false, maybe, true
    OUT raise value error(("invalid TRIT value",in)); ~
    ESAC;
  out
);

OP INITTRIT = (BITS b)TRIT:
  (TRIT out; trit OF out:=b; out);

##################################################
# Define the LOGICAL OPerators for the TRIT MODE #
##################################################
MODE LOGICAL = TRIT;
PR READ "Template_operators_logical_mixin.a68" PR

COMMENT
  Kleene logic truth tables:
END COMMENT

OP AND = (TRIT a,b)TRIT: (
  [,]TRIT(
    # ∧  ##  false, maybe, true  #
    #false# (false, false, false),
    #maybe# (false, maybe, maybe),
    #true # (false, maybe, true )
  )[@-1,@-1][INITINT a, INITINT b]
);

OP OR = (TRIT a,b)TRIT: (
  [,]TRIT(
    # ∨  ##  false, maybe, true #
    #false# (false, maybe, true),
    #maybe# (maybe, maybe, true),
    #true # (true,  true,  true)
  )[@-1,@-1][INITINT a, INITINT b]
);

PRIO IMPLIES = 1; # PRIO = 1.9 #
OP IMPLIES = (TRIT a,b)TRIT: (
  [,]TRIT(
    # ⊃   ## false, maybe, true #
    #false# (true,  true,  true),
    #maybe# (maybe, maybe, true),
    #true # (false, maybe, true)
  )[@-1,@-1][INITINT a, INITINT b]
);

PRIO EQV = 1; # PRIO = 1.8 #
OP EQV = (TRIT a,b)TRIT: (
  [,]TRIT(
    # ≡   ## false, maybe, true #
    #false# (true,  maybe, false),
    #maybe# (maybe, maybe, maybe),
    #true # (false, maybe, true )
  )[@-1,@-1][INITINT a, INITINT b]
);
```
'''File: Template_operators_logical_mixin.a68'''

```algol68
# -*- coding: utf-8 -*- #

OP & = (LOGICAL a,b)LOGICAL: a AND b;
CO # not included as they are treated as SCALAR #
OP EQ = (LOGICAL a,b)LOGICAL: a = b,
   NE = (LOGICAL a,b)LOGICAL: a /= b,
   ≠ = (TRIT a,b)TRIT: a /= b,
   ¬= = (TRIT a,b)TRIT: a /= b;
END CO

#IF html entities possible THEN
¢ "parked" operators for completeness ¢
OP ¬ = (LOGICAL a)LOGICAL: NOT a,
   ∧  = (LOGICAL a,b)LOGICAL: a AND b,
   /\ = (LOGICAL a,b)LOGICAL: a AND b,
   ∨  = (LOGICAL a,b)LOGICAL: a OR b,
   \/ = (LOGICAL a,b)LOGICAL: a OR b,
   ⊃ = (TRIT a,b)TRIT: a IMPLIES b,
   ≡ = (TRIT a,b)TRIT: a EQV b;
FI#

#IF algol68c THEN
OP ~ = (LOGICAL a)LOGICAL: NOT a,
   ~= = (LOGICAL a,b)LOGICAL: a /= b; SCALAR!
FI#
```
'''File: test_Ternary_logic.a68'''

```algol68
#!/usr/local/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR READ "prelude/general.a68" PR
PR READ "Ternary_logic.a68" PR

[]TRIT trits = (false, maybe, true);

FORMAT 
  col fmt = $" "g" "$,
  row fmt = $l3(f(col fmt)"|")f(col fmt)$,
  row sep fmt = $l3("---+")"---"l$;

PROC row sep = VOID:
  printf(row sep fmt);

PROC title = (STRING op name, LONGCHAR op char)VOID:(
  print(("Operator: ",op name));
  printf((row fmt,op char,REPR false, REPR maybe, REPR true))
);

PROC print trit op table = (LONGCHAR op char, STRING op name, PROC(TRIT,TRIT)TRIT op)VOID: (
  printf($l$);
  title(op name, op char);
  FOR i FROM LWB trits TO UPB trits DO
    row sep;
    TRIT ti = trits[i];
    printf((col fmt, REPR ti));
    FOR j FROM LWB trits TO UPB trits DO
      TRIT tj = trits[j];
      printf(($"|"$, col fmt, REPR op(ti,tj)))
    OD
  OD;
  printf($l$)
);

printf((
  $"Comparitive table of coercions:"l$,
  $"  TRIT BOOL         INT"l$
));

FOR it FROM LWB trits TO UPB trits DO
  TRIT t = trits[it];
  printf(( $"  "g"  "$, REPR t, 
    IF trit OF t = trit OF maybe THEN " " ELSE B t FI,
    INITINT t, $l$))
OD;

printf((
  $l"Specific test of the IMPLIES operator:"l$,
  $"  "g" implies "g" is "b("not ","")"a contradiction!"l$,
    B false,    B false,    B(false IMPLIES false),
    B false,    B true,     B(false IMPLIES true),
    B false,    REPR maybe, B(false IMPLIES maybe),
    B true,     B false,    B(true  IMPLIES false),
    B true,     B true,     B(true  IMPLIES true),
    REPR maybe, Btrue,      B(maybe IMPLIES true),
  $"  "g" implies "g" is "g" a contradiction!"l$,
    B true,     REPR maybe, REPR (true  IMPLIES maybe),
    REPR maybe, B false,    REPR (maybe IMPLIES false),
    REPR maybe, REPR maybe, REPR (maybe IMPLIES maybe),
  $l$
));

printf($"Kleene logic truth table samples:"l$);

print trit op table("≡","EQV",     (TRIT a,b)TRIT: a EQV b);
print trit op table("⊃","IMPLIES", (TRIT a,b)TRIT: a IMPLIES b);
print trit op table("∧","AND",     (TRIT a,b)TRIT: a AND b);
print trit op table("∨","OR",      (TRIT a,b)TRIT: a OR b)
```

{{out}}

```txt

Comparitive table of coercions:
  TRIT BOOL         INT
  ⌊    F             -1  
  ?                  +0  
  ⌈    T             +1  

Specific test of the IMPLIES operator:
  F implies F is not a contradiction!
  F implies T is not a contradiction!
  F implies ? is not a contradiction!
  T implies F is a contradiction!
  T implies T is not a contradiction!
  ? implies T is not a contradiction!
  T implies ? is ? a contradiction!
  ? implies F is ? a contradiction!
  ? implies ? is ? a contradiction!

Kleene logic truth table samples:

Operator: EQV
 ≡ | ⌊ | ? | ⌈ 
---+---+---+---
 ⌊ | ⌈ | ? | ⌊ 
---+---+---+---
 ? | ? | ? | ? 
---+---+---+---
 ⌈ | ⌊ | ? | ⌈ 

Operator: IMPLIES
 ⊃ | ⌊ | ? | ⌈ 
---+---+---+---
 ⌊ | ⌈ | ⌈ | ⌈ 
---+---+---+---
 ? | ? | ? | ⌈ 
---+---+---+---
 ⌈ | ⌊ | ? | ⌈ 

Operator: AND
 ∧ | ⌊ | ? | ⌈ 
---+---+---+---
 ⌊ | ⌊ | ⌊ | ⌊ 
---+---+---+---
 ? | ⌊ | ? | ? 
---+---+---+---
 ⌈ | ⌊ | ? | ⌈ 

Operator: OR
 ∨ | ⌊ | ? | ⌈ 
---+---+---+---
 ⌊ | ⌊ | ? | ⌈ 
---+---+---+---
 ? | ? | ? | ⌈ 
---+---+---+---
 ⌈ | ⌈ | ⌈ | ⌈ 

```



## AutoHotkey


```AutoHotkey
Ternary_Not(a){
	SetFormat, Float, 2.1
	return Abs(a-1)
}

Ternary_And(a,b){
	return a<b?a:b
}

Ternary_Or(a,b){
	return a>b?a:b
}

Ternary_IfThen(a,b){
	return a=1?b:a=0?1:a+b>1?1:0.5
}

Ternary_Equiv(a,b){
	return a=b?1:a=1?b:b=1?a:0.5
}
```

Examples:
```AutoHotkey
aa:=[1,0.5,0]
bb:=[1,0.5,0]

for index, a in aa
	Res .= "`tTernary_Not`t" a "`t=`t" Ternary_Not(a) "`n"
Res .= "-------------`n"

for index, a in aa
	for index, b in bb
		Res .= a "`tTernary_And`t" b "`t=`t" Ternary_And(a,b) "`n"
Res .= "-------------`n"

for index, a in aa
	for index, b in bb
		Res .= a "`tTernary_or`t" b "`t=`t" Ternary_Or(a,b) "`n"
Res .= "-------------`n"

for index, a in aa
	for index, b in bb
		Res .= a "`tTernary_then`t" b "`t=`t" Ternary_IfThen(a,b) "`n"
Res .= "-------------`n"

for index, a in aa
	for index, b in bb
		Res .= a "`tTernary_equiv`t" b "`t=`t" Ternary_Equiv(a,b) "`n"

StringReplace, Res, Res, 1, true, all
StringReplace, Res, Res, 0.5, maybe, all
StringReplace, Res, Res, 0, false, all
MsgBox % Res
return
```

{{out}}

```txt
	Ternary_Not	true	=	false
	Ternary_Not	maybe	=	maybe
	Ternary_Not	false	=	true
-------------
true	Ternary_And	true	=	true
true	Ternary_And	maybe	=	maybe
true	Ternary_And	false	=	false
maybe	Ternary_And	true	=	maybe
maybe	Ternary_And	maybe	=	maybe
maybe	Ternary_And	false	=	false
false	Ternary_And	true	=	false
false	Ternary_And	maybe	=	false
false	Ternary_And	false	=	false
-------------
true	Ternary_or	true	=	true
true	Ternary_or	maybe	=	true
true	Ternary_or	false	=	true
maybe	Ternary_or	true	=	true
maybe	Ternary_or	maybe	=	maybe
maybe	Ternary_or	false	=	maybe
false	Ternary_or	true	=	true
false	Ternary_or	maybe	=	maybe
false	Ternary_or	false	=	false
-------------
true	Ternary_then	true	=	true
true	Ternary_then	maybe	=	maybe
true	Ternary_then	false	=	false
maybe	Ternary_then	true	=	true
maybe	Ternary_then	maybe	=	maybe
maybe	Ternary_then	false	=	maybe
false	Ternary_then	true	=	true
false	Ternary_then	maybe	=	true
false	Ternary_then	false	=	true
-------------
true	Ternary_equiv	true	=	true
true	Ternary_equiv	maybe	=	maybe
true	Ternary_equiv	false	=	false
maybe	Ternary_equiv	true	=	maybe
maybe	Ternary_equiv	maybe	=	true
maybe	Ternary_equiv	false	=	maybe
false	Ternary_equiv	true	=	false
false	Ternary_equiv	maybe	=	maybe
false	Ternary_equiv	false	=	true
```




## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$ + "CLASSLIB"
      
      REM Create a ternary class:
      DIM trit{tor, tand, teqv, tnot, tnor, s, v}
      DEF PRIVATE trit.s (t&) LOCAL t$():DIM t$(2):t$()="FALSE","MAYBE","TRUE":=t$(t&)
      DEF PRIVATE trit.v (t$) = INSTR("FALSE MAYBE TRUE", t$) DIV 6
      DEF trit.tnot (t$) = FN(trit.s)(2 - FN(trit.v)(t$))
      DEF trit.tor (a$,b$) LOCAL t:t=FN(trit.v)(a$)ORFN(trit.v)(b$):=FN(trit.s)(t+(t>2))
      DEF trit.tnor (a$,b$) = FN(trit.tnot)(FN(trit.tor)(a$,b$))
      DEF trit.tand (a$,b$) = FN(trit.tnor)(FN(trit.tnot)(a$),FN(trit.tnot)(b$))
      DEF trit.teqv (a$,b$) = FN(trit.tor)(FN(trit.tand)(a$,b$),FN(trit.tnor)(a$,b$))
      PROC_class(trit{})
      
      PROC_new(mytrit{}, trit{})
      
      REM Test it:
      PRINT "Testing NOT:"
      PRINT "NOT FALSE = " FN(mytrit.tnot)("FALSE")
      PRINT "NOT MAYBE = " FN(mytrit.tnot)("MAYBE")
      PRINT "NOT TRUE  = " FN(mytrit.tnot)("TRUE")
      
      PRINT '"Testing OR:"
      PRINT "FALSE OR FALSE = " FN(mytrit.tor)("FALSE","FALSE")
      PRINT "FALSE OR MAYBE = " FN(mytrit.tor)("FALSE","MAYBE")
      PRINT "FALSE OR TRUE  = " FN(mytrit.tor)("FALSE","TRUE")
      PRINT "MAYBE OR MAYBE = " FN(mytrit.tor)("MAYBE","MAYBE")
      PRINT "MAYBE OR TRUE  = " FN(mytrit.tor)("MAYBE","TRUE")
      PRINT "TRUE  OR TRUE  = " FN(mytrit.tor)("TRUE","TRUE")
      
      PRINT '"Testing AND:"
      PRINT "FALSE AND FALSE = " FN(mytrit.tand)("FALSE","FALSE")
      PRINT "FALSE AND MAYBE = " FN(mytrit.tand)("FALSE","MAYBE")
      PRINT "FALSE AND TRUE  = " FN(mytrit.tand)("FALSE","TRUE")
      PRINT "MAYBE AND MAYBE = " FN(mytrit.tand)("MAYBE","MAYBE")
      PRINT "MAYBE AND TRUE  = " FN(mytrit.tand)("MAYBE","TRUE")
      PRINT "TRUE  AND TRUE  = " FN(mytrit.tand)("TRUE","TRUE")
      
      PRINT '"Testing EQV (similar to EOR):"
      PRINT "FALSE EQV FALSE = " FN(mytrit.teqv)("FALSE","FALSE")
      PRINT "FALSE EQV MAYBE = " FN(mytrit.teqv)("FALSE","MAYBE")
      PRINT "FALSE EQV TRUE  = " FN(mytrit.teqv)("FALSE","TRUE")
      PRINT "MAYBE EQV MAYBE = " FN(mytrit.teqv)("MAYBE","MAYBE")
      PRINT "MAYBE EQV TRUE  = " FN(mytrit.teqv)("MAYBE","TRUE")
      PRINT "TRUE  EQV TRUE  = " FN(mytrit.teqv)("TRUE","TRUE")
      
      PROC_discard(mytrit{})
```

{{out}}

```txt

Testing NOT:
NOT FALSE = TRUE
NOT MAYBE = MAYBE
NOT TRUE  = FALSE

Testing OR:
FALSE OR FALSE = FALSE
FALSE OR MAYBE = MAYBE
FALSE OR TRUE  = TRUE
MAYBE OR MAYBE = MAYBE
MAYBE OR TRUE  = TRUE
TRUE  OR TRUE  = TRUE

Testing AND:
FALSE AND FALSE = FALSE
FALSE AND MAYBE = FALSE
FALSE AND TRUE  = FALSE
MAYBE AND MAYBE = MAYBE
MAYBE AND TRUE  = MAYBE
TRUE  AND TRUE  = TRUE

Testing EQV (similar to EOR):
FALSE EQV FALSE = TRUE
FALSE EQV MAYBE = MAYBE
FALSE EQV TRUE  = FALSE
MAYBE EQV MAYBE = MAYBE
MAYBE EQV TRUE  = MAYBE
TRUE  EQV TRUE  = TRUE

```



## C


### Implementing logic using lookup tables


```c>#include <stdio.h

 
typedef enum {
  TRITTRUE,  /* In this enum, equivalent to integer value 0 */
  TRITMAYBE, /* In this enum, equivalent to integer value 1 */
  TRITFALSE  /* In this enum, equivalent to integer value 2 */
} trit;
 
/* We can trivially find the result of the operation by passing
   the trinary values as indeces into the lookup tables' arrays. */
trit tritNot[3] = {TRITFALSE , TRITMAYBE, TRITTRUE};
trit tritAnd[3][3] = { {TRITTRUE, TRITMAYBE, TRITFALSE},
                       {TRITMAYBE, TRITMAYBE, TRITFALSE},
                       {TRITFALSE, TRITFALSE, TRITFALSE} };
 
trit tritOr[3][3] = { {TRITTRUE, TRITTRUE, TRITTRUE},
                      {TRITTRUE, TRITMAYBE, TRITMAYBE},
                      {TRITTRUE, TRITMAYBE, TRITFALSE} };
 
trit tritThen[3][3] = { { TRITTRUE, TRITMAYBE, TRITFALSE},
                        { TRITTRUE, TRITMAYBE, TRITMAYBE},
                        { TRITTRUE, TRITTRUE, TRITTRUE } };
 
trit tritEquiv[3][3] = { { TRITTRUE, TRITMAYBE, TRITFALSE},
                         { TRITMAYBE, TRITMAYBE, TRITMAYBE},
                         { TRITFALSE, TRITMAYBE, TRITTRUE } };

/* Everything beyond here is just demonstration */

const char* tritString[3] = {"T", "?", "F"};

void demo_binary_op(trit operator[3][3], const char* name)
{
  trit operand1 = TRITTRUE; /* Declare. Initialize for CYA */
  trit operand2 = TRITTRUE; /* Declare. Initialize for CYA */

  /* Blank line */
  printf("\n");

  /* Demo this operator */
  for( operand1 = TRITTRUE; operand1 <= TRITFALSE; ++operand1 )
  {
    for( operand2 = TRITTRUE; operand2 <= TRITFALSE; ++operand2 )
    {
      printf("%s %s %s: %s\n", tritString[operand1],
                               name,
                               tritString[operand2],
                               tritString[operator[operand1][operand2]]);
    }
  }

}

int main()
{
  trit op1 = TRITTRUE; /* Declare. Initialize for CYA */
  trit op2 = TRITTRUE; /* Declare. Initialize for CYA */
 
  /* Demo 'not' */
  for( op1 = TRITTRUE; op1 <= TRITFALSE; ++op1 )
  {
    printf("Not %s: %s\n", tritString[op1], tritString[tritNot[op1]]);
  }
  demo_binary_op(tritAnd, "And");
  demo_binary_op(tritOr, "Or");
  demo_binary_op(tritThen, "Then");
  demo_binary_op(tritEquiv, "Equiv");

 
  return 0;
}
```


{{out}}

```txt
Not T: F
Not ?: ?
Not F: T

T And T: T
T And ?: ?
T And F: F
? And T: ?
? And ?: ?
? And F: F
F And T: F
F And ?: F
F And F: F

T Or T: T
T Or ?: T
T Or F: T
? Or T: T
? Or ?: ?
? Or F: ?
F Or T: T
F Or ?: ?
F Or F: F

T Then T: T
T Then ?: ?
T Then F: F
? Then T: T
? Then ?: ?
? Then F: ?
F Then T: T
F Then ?: T
F Then F: T

T Equiv T: T
T Equiv ?: ?
T Equiv F: F
? Equiv T: ?
? Equiv ?: ?
? Equiv F: ?
F Equiv T: F
F Equiv ?: ?
F Equiv F: T
```



### Using functions


```c>#include <stdio.h


typedef enum { t_F = -1, t_M, t_T } trit;

trit t_not  (trit a) { return -a; }
trit t_and  (trit a, trit b) { return a < b ? a : b; }
trit t_or   (trit a, trit b) { return a > b ? a : b; }
trit t_eq   (trit a, trit b) { return a * b; }
trit t_imply(trit a, trit b) { return -a > b ? -a : b; }
char t_s(trit a) { return "F?T"[a + 1]; }

#define forall(a) for(a = t_F; a <= t_T; a++)
void show_op(trit (*f)(trit, trit), const char *name) {
	trit a, b;
	printf("\n[%s]\n    F ? T\n  -------", name);
	forall(a) {
		printf("\n%c |", t_s(a));
		forall(b) printf(" %c", t_s(f(a, b)));
	}
	puts("");
}

int main(void)
{
	trit a;

	puts("[Not]");
	forall(a) printf("%c | %c\n", t_s(a), t_s(t_not(a)));

	show_op(t_and,   "And");
	show_op(t_or,    "Or");
	show_op(t_eq,    "Equiv");
	show_op(t_imply, "Imply");

	return 0;
}
```

{{out}}

```txt
[Not]
F | T
? | ?
T | F

[And]
    F ? T
  -------
F | F F F
? | F ? ?
T | F ? T

[Or]
    F ? T
  -------
F | F ? T
? | ? ? T
T | T T T

[Equiv]
    F ? T
  -------
F | T ? F
? | ? ? ?
T | F ? T

[Imply]
    F ? T
  -------
F | T T T
? | ? ? T
T | F ? T
```



### Variable truthfulness

Represent each possible truth value as a floating point value x, 
where the var has x chance of being true and 1 - x chance of being false. 
When using <code>if3</code> conditional on a potential truth varible, 
the result is randomly sampled to true or false according to the chance.  
(This description is definitely very confusing perhaps).

```c>#include <stdio.h

#include <stdlib.h>

typedef double half_truth, maybe;

inline maybe not3(maybe a) { return 1 - a; }

inline maybe
and3(maybe a, maybe b) { return a * b; }

inline maybe
or3(maybe a, maybe b) { return a + b - a * b; }

inline maybe
eq3(maybe a, maybe b) { return 1 - a - b + 2 * a * b; }

inline maybe
imply3(maybe a, maybe b) { return or3(not3(a), b); }

#define true3(x) ((x) * RAND_MAX > rand())
#define if3(x) if (true3(x))

int main()
{
	maybe roses_are_red = 0.25; /* they can be white or black, too */
	maybe violets_are_blue = 1; /* aren't they just */
	int i;

	puts("Verifying flowery truth for 40 times:\n");

	puts("Rose is NOT red:"); /* chance: .75 */
	for (i = 0; i < 40 || !puts("\n"); i++)
		printf( true3( not3(roses_are_red) ) ? "T" : "_");

	/* pick a rose and a violet; */
	puts("Rose is red AND violet is blue:");
	/* chance of rose being red AND violet being blue is .25 */
	for (i = 0; i < 40 || !puts("\n"); i++)
		printf( true3( and3(roses_are_red, violets_are_blue) )
			? "T" : "_");

	/* chance of rose being red OR violet being blue is 1 */
	puts("Rose is red OR violet is blue:");
	for (i = 0; i < 40 || !puts("\n"); i++)
		printf( true3( or3(roses_are_red, violets_are_blue) )
			? "T" : "_");

	/* pick two roses; chance of em being both red or both not red is .625 */
	puts("This rose is as red as that rose:");
	for (i = 0; i < 40 || !puts("\n"); i++)
		if3(eq3(roses_are_red, roses_are_red)) putchar('T');
		else putchar('_');

	return 0;
}
```



## C++

Essentially the same logic as the [[#Using functions|Using functions]] implementation above, but using class-based encapsulation and overridden operators.

```cpp>#include <iostream

#include <stdlib.h>

class trit {
public:
    static const trit False, Maybe, True;

    trit operator !() const {
        return static_cast<Value>(-value);
    }

    trit operator &&(const trit &b) const {
        return (value < b.value) ? value : b.value;
    }

    trit operator ||(const trit &b) const {
        return (value > b.value) ? value : b.value;
    }

    trit operator >>(const trit &b) const {
        return -value > b.value ? static_cast<Value>(-value) : b.value;
    }

    trit operator ==(const trit &b) const {
        return static_cast<Value>(value * b.value);
    }

    char chr() const {
        return "F?T"[value + 1];
    }

protected:
    typedef enum { FALSE=-1, MAYBE, TRUE } Value;

    Value value;

    trit(const Value value) : value(value) { }
};

std::ostream& operator<<(std::ostream &os, const trit &t)
{
    os << t.chr();
    return os;
}

const trit trit::False = trit(trit::FALSE);
const trit trit::Maybe = trit(trit::MAYBE);
const trit trit::True = trit(trit::TRUE);

int main(int, char**) {
    const trit trits[3] = { trit::True, trit::Maybe, trit::False };

#define for_each(name) \
    for (size_t name=0; name<3; ++name)

#define show_op(op) \
    std::cout << std::endl << #op << " "; \
    for_each(a) std::cout << ' ' << trits[a]; \
    std::cout << std::endl << "  -------"; \
    for_each(a) { \
        std::cout << std::endl << trits[a] << " |"; \
        for_each(b) std::cout << ' ' << (trits[a] op trits[b]); \
    } \
    std::cout << std::endl;

    std::cout << "! ----" << std::endl;
    for_each(a) std::cout << trits[a] << " | " << !trits[a] << std::endl;

    show_op(&&);
    show_op(||);
    show_op(>>);
    show_op(==);
    return EXIT_SUCCESS;
}
```

{{out}}

```txt
! ----
T | F
? | ?
F | T

&&  T ? F
  -------
T | T ? F
? | ? ? F
F | F F F

||  T ? F
  -------
T | T T T
? | T ? ?
F | T ? F

>>  T ? F
  -------
T | T ? F
? | T ? ?
F | T T T

==  T ? F
  -------
T | T ? F
? | ? ? ?
F | F ? T
```


=={{header|C sharp|C#}}==

```csharp
using System;

/// <summary>
/// Extension methods on nullable bool.
/// </summary>
/// <remarks>
/// The operators !, & and | are predefined.
/// </remarks>
public static class NullableBoolExtension
{
    public static bool? Implies(this bool? left, bool? right)
    {
        return !left | right;
    }

    public static bool? IsEquivalentTo(this bool? left, bool? right)
    {
        return left.HasValue && right.HasValue ? left == right : default(bool?);
    }

    public static string Format(this bool? value)
    {
        return value.HasValue ? value.Value.ToString() : "Maybe";
    }
}

public class Program
{
    private static void Main()
    {
        var values = new[] { true, default(bool?), false };

        foreach (var left in values)
        {
            Console.WriteLine("¬{0} = {1}", left.Format(), (!left).Format());
            foreach (var right in values)
            {
                Console.WriteLine("{0} & {1} = {2}", left.Format(), right.Format(), (left & right).Format());
                Console.WriteLine("{0} | {1} = {2}", left.Format(), right.Format(), (left | right).Format());
                Console.WriteLine("{0} → {1} = {2}", left.Format(), right.Format(), left.Implies(right).Format());
                Console.WriteLine("{0} ≡ {1} = {2}", left.Format(), right.Format(), left.IsEquivalentTo(right).Format());
            }
        }
    }
}
```

{{out}}

```txt
¬True = False
True & True = True
True | True = True
True → True = True
True ≡ True = True
True & Maybe = Maybe
True | Maybe = True
True → Maybe = Maybe
True ≡ Maybe = Maybe
True & False = False
True | False = True
True → False = False
True ≡ False = False
¬Maybe = Maybe
Maybe & True = Maybe
Maybe | True = True
Maybe → True = True
Maybe ≡ True = Maybe
Maybe & Maybe = Maybe
Maybe | Maybe = Maybe
Maybe → Maybe = Maybe
Maybe ≡ Maybe = Maybe
Maybe & False = False
Maybe | False = Maybe
Maybe → False = Maybe
Maybe ≡ False = Maybe
¬False = True
False & True = False
False | True = True
False → True = True
False ≡ True = False
False & Maybe = False
False | Maybe = Maybe
False → Maybe = True
False ≡ Maybe = Maybe
False & False = False
False | False = False
False → False = True
False ≡ False = True
```



## Common Lisp


```lisp
(defun tri-not (x) (- 1 x))
(defun tri-and (&rest x) (apply #'* x))
(defun tri-or (&rest x) (tri-not (apply #'* (mapcar #'tri-not x))))
(defun tri-eq (x y) (+ (tri-and x y) (tri-and (- 1 x) (- 1 y))))
(defun tri-imply (x y) (tri-or (tri-not x) y))

(defun tri-test (x) (< (random 1e0) x))
(defun tri-string (x) (if (= x 1) "T" (if (= x 0) "F" "?")))

;; to say (tri-if (condition) (yes) (no))
(defmacro tri-if (tri ifcase &optional elsecase)
  `(if (tri-test ,tri) ,ifcase ,elsecase))

(defun print-table (func header)
  (let ((vals '(1 .5 0)))
    (format t "~%~a:~%" header)
    (format t "    ~{~a ~^~}~%---------~%" (mapcar #'tri-string vals))
    (loop for row in vals do
	  (format t "~a | " (tri-string row))
	  (loop for col in vals do
		(format t "~a " (tri-string (funcall func row col))))
	  (write-line ""))))

(write-line "NOT:")
(loop for row in '(1 .5 0) do
      (format t "~a | ~a~%" (tri-string row) (tri-string (tri-not row))))

(print-table #'tri-and   "AND")
(print-table #'tri-or    "OR")
(print-table #'tri-imply "IMPLY")
(print-table #'tri-eq    "EQUAL")
```

{{out}}

```txt
NOT:
T | F
? | ?
F | T

AND:
    T ? F 
---------
T | T ? F 
? | ? ? F 
F | F F F 

OR:
    T ? F 
---------
T | T T T 
? | T ? ? 
F | T ? F 

IMPLY:
    T ? F 
---------
T | T ? F 
? | T ? ? 
F | T T T 

EQUAL:
    T ? F 
---------
T | T ? F 
? | ? ? ? 
F | F ? T
```



## D

Partial translation of a C entry:

```d
import std.stdio;

struct Trit {
    private enum Val : byte { F = -1, M, T }
    private Val t;
    alias t this;
    static immutable Trit[3] vals = [{Val.F}, {Val.M}, {Val.T}];
    static immutable F = Trit(Val.F); // Not necessary but handy.
    static immutable M = Trit(Val.M);
    static immutable T = Trit(Val.T);

    string toString() const pure nothrow {
        return "F?T"[t + 1  .. t + 2];
    }

    Trit opUnary(string op)() const pure nothrow
    if (op == "~") {
        return Trit(-t);
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "&") {
        return t < b ? this : b;
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "|") {
        return t > b ? this : b;
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "^") {
        return ~(this == b);
    }

    Trit opEquals(in Trit b) const pure nothrow {
        return Trit(cast(Val)(t * b));
    }

    Trit imply(in Trit b) const pure nothrow {
        return -t > b ? ~this : b;
    }
}

void showOperation(string op)(in string opName) {
    writef("\n[%s]\n    F ? T\n  -------", opName);
    foreach (immutable a; Trit.vals) {
        writef("\n%s |", a);
        foreach (immutable b; Trit.vals)
            static if (op == "==>")
                writef(" %s", a.imply(b));
            else
                writef(" %s", mixin("a " ~ op ~ " b"));
    }
    writeln();
}

void main() {
    writeln("[Not]");
    foreach (const a; Trit.vals)
        writefln("%s | %s", a, ~a);

    showOperation!"&"("And");
    showOperation!"|"("Or");
    showOperation!"^"("Xor");
    showOperation!"=="("Equiv");
    showOperation!"==>"("Imply");
}
```

{{out}}

```txt
[Not]
F | T
? | ?
T | F

[And]
    F ? T
  -------
F | F F F
? | F ? ?
T | F ? T

[Or]
    F ? T
  -------
F | F ? T
? | ? ? T
T | T T T

[Xor]
    F ? T
  -------
F | F ? T
? | ? ? ?
T | T ? F

[Equiv]
    F ? T
  -------
F | T ? F
? | ? ? ?
T | F ? T

[Imply]
    F ? T
  -------
F | T T T
? | ? ? T
T | F ? T
```



## Delphi


```delphi
unit TrinaryLogic;

interface

//Define our own type for ternary logic.
//This is actually still a Boolean, but the compiler will use distinct RTTI information.
type
    TriBool = type Boolean;

const
    TTrue:TriBool = True;
    TFalse:TriBool = False;
    TMaybe:TriBool = TriBool(2);

function TVL_not(Value: TriBool): TriBool;
function TVL_and(A, B: TriBool): TriBool;
function TVL_or(A, B: TriBool): TriBool;
function TVL_xor(A, B: TriBool): TriBool;
function TVL_eq(A, B: TriBool): TriBool;

implementation

Uses
    SysUtils;

function TVL_not(Value: TriBool): TriBool;
begin
    if Value = True Then
        Result := TFalse
    else If Value = False Then
        Result := TTrue
    else
        Result := Value;
end;

function TVL_and(A, B: TriBool): TriBool;
begin
    Result := TriBool(Iff(Integer(A * B) > 1, Integer(TMaybe), A * B));
end;

function TVL_or(A, B: TriBool): TriBool;
begin
    Result := TVL_not(TVL_and(TVL_not(A), TVL_not(B)));
end;

function TVL_xor(A, B: TriBool): TriBool;
begin
    Result := TVL_and(TVL_or(A, B), TVL_not(TVL_or(A, B)));
end;

function TVL_eq(A, B: TriBool): TriBool;
begin
    Result := TVL_not(TVL_xor(A, B));
end;

end.
```


And that's the reason why you never on no account ''ever'' should compare against the values of True or False unless you intent ternary logic!

An alternative version would be using an enum type

```delphi
type TriBool = (tbFalse, tbMaybe, tbTrue);
```

and defining a set of constants implementing the above tables:

```delphi
const
    tvl_not: array[TriBool] = (tbTrue, tbMaybe, tbFalse);
    tvl_and: array[TriBool, TriBool] = (
        (tbFalse, tbFalse, tbFalse),
        (tbFalse, tbMaybe, tbMaybe),
        (tbFalse, tbMaybe, tbTrue),
        );
    tvl_or: array[TriBool, TriBool] = (
        (tbFalse, tbMaybe, tbTrue),
        (tbMaybe, tbMaybe, tbTrue),
        (tbTrue, tbTrue, tbTrue),
        );
    tvl_xor: array[TriBool, TriBool] = (
        (tbFalse, tbMaybe, tbTrue),
        (tbMaybe, tbMaybe, tbMaybe),
        (tbTrue, tbMaybe, tbFalse),
        );
    tvl_eq: array[TriBool, TriBool] = (
        (tbTrue, tbMaybe, tbFalse),
        (tbMaybe, tbMaybe, tbMaybe),
        (tbFalse, tbMaybe, tbTrue),
        );

```


That's no real fun, but lookup can then be done with

```delphi
Result := tvl_and[A, B];
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'collections;
 
sealed class Trit
{
    bool _value;
 
    bool cast() = _value;
 
    constructor(v)
    {
        if (v != nil)
        {
            _value := cast bool(v);
        }        
    }
 
    Trit equivalent(b)
        = _value.equal(cast bool(b)) \ back:nilValue;
 
    Trit Inverted
        = _value.Inverted \ back:nilValue;
 
    Trit and(b)
    {
        if (nil == _value)
        {
            ^ b.and:nil \ back:nilValue
        }
        else
        {
            ^ _value.and(lazy::(cast bool(b))) \ back:nilValue
        }
    }
 
    Trit or(b)
    {
        if (nil == _value)
        {
            ^ b.or:nilValue \ back:nilValue
        }
        else
        {
            ^ _value.or(lazy::(cast bool(b))) \ back:nilValue
        }
    }
 
    Trit implies(b)
        = self.Inverted.or(b);
 
    string Printable = _value.Printable \ back:"maybe";
}
 
public program()
{
    List<Trit> values := new Trit[]::(true, nilValue, false);
    values.forEach:(left)
    {
        console.printLine("¬",left," = ", left.Inverted);
        values.forEach:(right)
        {
            console.printLine(left, " & ", right, " = ", left && right);
            console.printLine(left, " | ", right, " = ", left || right);
            console.printLine(left, " → ", right, " = ", left.implies:right);
            console.printLine(left, " ≡ ", right, " = ", left.equivalent:right)
        }
    }
}
```

{{out}}

```txt

¬ true = false
true & true = true
true | true = true
true →  true = true
true ≡  true = true
true & maybe = maybe
true | maybe = true
true →  maybe = maybe
true ≡  maybe = maybe
true & false = false
true | false = true
true →  false = false
true ≡  false = false
¬ maybe = maybe
maybe & true = maybe
maybe | true = true
maybe →  true = true
maybe ≡  true = maybe
maybe & maybe = maybe
maybe | maybe = maybe
maybe →  maybe = maybe
maybe ≡  maybe = maybe
maybe & false = false
maybe | false = maybe
maybe →  false = maybe
maybe ≡  false = maybe
¬ false = true
false & true = false
false | true = true
false →  true = true
false ≡  true = false
false & maybe = false
false | maybe = maybe
false →  maybe = true
false ≡  maybe = maybe
false & false = false
false | false = false
false →  false = true
false ≡  false = true

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(ternary).
-export([main/0, nott/1, andd/2,orr/2, then/2, equiv/2]).

main() ->
	{ok, [A]} = io:fread("Enter A: ","~s"),
	{ok, [B]} = io:fread("Enter B: ","~s"),
	andd(A,B).

nott(S) ->
	if 
		S=="T" ->
			io : format("F\n");

	 	S=="F" ->
			io : format("T\n");

		true ->
			io: format("?\n")
	end.	 
	
andd(A, B) ->
	if 
		A=="T", B=="T" ->
			io : format("T\n");
		
		A=="F"; B=="F" ->
			io : format("F\n");	

		true ->
			io: format("?\n")
	end.	


orr(A, B) ->
	if 
		A=="T"; B=="T" ->
			io : format("T\n");
		
		A=="?"; B=="?" ->
			io : format("?\n");	

		true ->
			io: format("F\n")
	end.
	

then(A, B) ->
	if 
		B=="T" ->
			io : format("T\n");
		
		A=="?" ->
			io : format("?\n");	

		A=="F" ->
			io :format("T\n");
		B=="F" ->
			io:format("F\n");	
		true ->
			io: format("?\n")
	end.	

equiv(A, B) ->
	if 
		A=="?" ->
			io : format("?\n");
		
		A=="F" ->
			io : format("~s\n", [nott(B)]);	

		true ->
			io: format("~s\n", [B])
	end.			

```



## Factor

For boolean logic, Factor uses ''t'' and ''f'' with the words ''>boolean'', ''not'', ''and'', ''or'', ''xor''. For ternary logic, we add ''m'' and define the words ''>trit'', ''tnot'', ''tand'', ''tor'', ''txor'' and ''t=''. Our new class, ''trit'', is the union class of ''t'', ''m'' and ''f''.


```factor
! rosettacode/ternary/ternary.factor
! http://rosettacode.org/wiki/Ternary_logic
USING: combinators kernel ;
IN: rosettacode.ternary

SINGLETON: m
UNION: trit t m POSTPONE: f ;

GENERIC: >trit ( object -- trit )
M: trit >trit ;

: tnot ( trit1 -- trit )
    >trit { { t [ f ] } { m [ m ] } { f [ t ] } } case ;

: tand ( trit1 trit2 -- trit )
    >trit {
        { t [ >trit ] }
        { m [ >trit { { t [ m ] } { m [ m ] } { f [ f ] } } case ] }
        { f [ >trit drop f ] }
    } case ;

: tor ( trit1 trit2 -- trit )
    >trit {
        { t [ >trit drop t ] }
        { m [ >trit { { t [ t ] } { m [ m ] } { f [ m ] } } case ] }
        { f [ >trit ] }
    } case ;

: txor ( trit1 trit2 -- trit )
    >trit {
        { t [ tnot ] }
        { m [ >trit drop m ] }
        { f [ >trit ] }
    } case ;

: t= ( trit1 trit2 -- trit )
    {
        { t [ >trit ] }
        { m [ >trit drop m ] }
        { f [ tnot ] }
    } case ;
```


Example use:

```factor
( scratchpad ) CONSTANT: trits { t m f }
( scratchpad ) trits [ tnot ] map .
{ f m t }
( scratchpad ) trits [ trits swap [ tand ] curry map ] map .
{ { t m f } { m m f } { f f f } }
( scratchpad ) trits [ trits swap [ tor ] curry map ] map .
{ { t t t } { t m m } { t m f } }
( scratchpad ) trits [ trits swap [ txor ] curry map ] map .
{ { f m t } { m m m } { t m f } }
( scratchpad ) trits [ trits swap [ t= ] curry map ] map .
{ { t m f } { m m m } { f m t } }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Finite-valued_logic this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.

The solution shown uses [https://en.wikipedia.org/wiki/Finite-valued_logic finite-valued logic], where the n-valued logic can be represented as n equally spaced values between 0 (pure false) and 1 (pure true). As an example, the traditional logic values are represented as 0 and 1, and ternary logic is represented with the values 0 (false), 1/2 (maybe) and 1 (true), and so on.

The solution also shows how to ''redefine'' the logic operations, and how to show the values and the operations using colors.


## Fortran


Please find the demonstration and compilation with gfortran at the start of the code.  A module contains the ternary logic for easy reuse.  Consider input redirection from unixdict.txt as vestigial.  Or I could delete it.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Mon May 20 23:05:46
!
!a=./f && make $a && $a < unixdict.txt
!gfortran -std=f2003 -Wall -ffree-form f.f03 -o f
!
!ternary not
! 1.0 0.5 0.0
!
!
!ternary and
! 0.0 0.0 0.0
! 0.0 0.5 0.5
! 0.0 0.5 1.0
!
!
!ternary or
! 0.0 0.5 1.0
! 0.5 0.5 1.0
! 1.0 1.0 1.0
!
!
!ternary if
! 1.0 1.0 1.0
! 0.5 0.5 1.0
! 0.0 0.5 1.0
!
!
!ternary eq
! 1.0 0.5 0.0
! 0.5 0.5 0.5
! 0.0 0.5 1.0
!
!
!Compilation finished at Mon May 20 23:05:46


!This program is based on the j implementation
!not=: -.
!and=: <.
!or =: >.
!if =: (>. -.)"0~
!eq =: (<.&-. >. <.)"0

module trit

  real, parameter :: true = 1,  false = 0, maybe = 0.5

contains

  real function tnot(y)
    real, intent(in) :: y
    tnot = 1 - y
  end function tnot

  real function tand(x, y)
    real, intent(in) :: x, y
    tand = min(x, y)
  end function tand

  real function tor(x, y)
    real, intent(in) :: x, y
    tor = max(x, y)
  end function tor

  real function tif(x, y)
    real, intent(in) :: x, y
    tif = tor(y, tnot(x))
  end function tif

  real function teq(x, y)
    real, intent(in) :: x, y
    teq = tor(tand(tnot(x), tnot(y)), tand(x, y))
  end function teq

end module trit

program ternaryLogic
  use trit
  integer :: i
  real, dimension(3) :: a = [false, maybe, true] ! (/ ... /)
  write(6,'(/a)')'ternary not' ; write(6, '(3f4.1/)') (tnot(a(i)), i = 1 , 3)
  write(6,'(/a)')'ternary and' ; call table(tand, a, a)
  write(6,'(/a)')'ternary or' ; call table(tor, a, a)
  write(6,'(/a)')'ternary if' ; call table(tif, a, a)
  write(6,'(/a)')'ternary eq' ; call table(teq, a, a)

contains

  subroutine table(u, x, y) ! for now, show the table.
    real, external :: u
    real, dimension(3), intent(in) :: x, y
    integer :: i, j
    write(6, '(3(3f4.1/))') ((u(x(i), y(j)), j=1,3), i=1,3)
  end subroutine table

end program ternaryLogic

```



## Free Pascal

Free Pascal version with lookup.
Note equivalence and implication are used as proof, they are solved using the basic set instead of a lookup.
Note Since we use a balanced range -1,0,1 multiplication equals EQU

```pascal
{$mode objfpc}
unit ternarylogic;

interface
type
  { ternary type, balanced }
  trit = (tFalse=-1, tMaybe=0, tTrue=1);

 { ternary operators }
  
  { equivalence = multiplication }
  operator * (const a,b:trit):trit;
  operator and (const a,b:trit):trit;inline;
  operator or (const a,b:trit):trit;inline;
  operator not (const a:trit):trit;inline;
  operator xor (const a,b:trit):trit;
  { imp ==>}
  operator >< (const a,b:trit):trit;
                

implementation

  operator and (const a,b:trit):trit;inline;
    const lookupAnd:array[trit,trit] of trit =
                    ((tFalse,tFalse,tFalse),
                     (tFalse,tMaybe,tMaybe),
                     (tFalse,tMaybe,tTrue));
  begin
    Result:= LookupAnd[a,b];
  end;
         
  operator or (const a,b:trit):trit;inline;
    const lookupOr:array[trit,trit] of trit =
                   ((tFalse,tMaybe,tTrue),
                    (tMaybe,tMaybe,tTrue),
                    (tTrue,tTrue,tTrue));
  begin
    Result := LookUpOr[a,b];
  end;
 
  operator not (const a:trit):trit;inline;
    const LookupNot:array[trit] of trit =(tTrue,tMaybe,tFalse);
  begin
     Result:= LookUpNot[a];
  end;
 
  operator xor (const a,b:trit):trit;
    const LookupXor:array[trit,trit] of trit =
                    ((tFalse,tMaybe,tTrue),
                     (tMaybe,tMaybe,tMaybe),
                     (tTrue,tMaybe,tFalse));
  begin
    Result := LookupXor[a,b];
  end;

  operator * (const a,b:trit):trit;
  begin
    result := not (a xor b);
  end;

  { imp ==>}
  operator >< (const a,b:trit):trit;
  begin
     result := not(a) or b;
  end;
end.

```


```pascal
program ternarytests;
{$mode objfpc}
uses 
  ternarylogic;   
begin
  writeln(' a AND b');
  writeln('F':7,'U':7, 'T':7);
  writeln('F|',tFalse and tFalse:7,tFalse and tMaybe:7,tFalse and tTrue:7);
  writeln('U|',tMaybe and tFalse:7,tMaybe and tMaybe:7,tMaybe and tTrue:7);
  writeln('T|',tTrue and tFalse:7,tTrue and tMaybe:7,tTrue and tTrue:7);
  writeln;
 
  writeln(' a OR b');
  writeln('F':7,'U':7, 'T':7);
  writeln('F|',tFalse or tFalse:7,tFalse or tMaybe:7,tFalse or tTrue:7);
  writeln('U|',tMaybe or tFalse:7,tMaybe or tMaybe:7,tMaybe or tTrue:7);
  writeln('T|',tTrue or tFalse:7,tTrue or tMaybe:7,tTrue or tTrue:7);
  writeln;
 
  writeln(' NOT a');
  writeln('F|',not tFalse:7);
  writeln('U|',not tMaybe:7);
  writeln('T|',not tTrue:7);
  writeln;
 
  writeln(' a XOR b');
  writeln('F':7,'U':7, 'T':7);
  writeln('F|',tFalse xor tFalse:7,tFalse xor tMaybe:7,tFalse xor tTrue:7);
  writeln('U|',tMaybe xor tFalse:7,tMaybe xor tMaybe:7,tMaybe xor tTrue:7);
  writeln('T|',tTrue xor tFalse:7,tTrue xor tMaybe:7,tTrue xor tTrue:7);
  writeln;
 
  writeln('equality/equivalence and multiplication');
  writeln('F':7,'U':7, 'T':7);
  writeln('F|', tFalse * tFalse:7,tFalse * tMaybe:7, tFalse * tTrue:7);
  writeln('U|', tMaybe * tFalse:7,tMaybe * tMaybe:7,tMaybe * tTrue:7);
  writeln('T|', tTrue * tFalse:7, tTrue * tMaybe:7, tTrue * tTrue:7);
   writeln;
 
  writeln('IMP. a.k.a. IfThen -> not(a) or b');
  writeln('F':7,'U':7, 'T':7);
  writeln('T|',tTrue >< tTrue:7,tTrue >< tMaybe:7,tTrue >< tFalse:7);
  writeln('U|',tMaybe >< tTrue:7,tMaybe >< tMaybe:7,tMaybe >< tFalse:7);
  writeln('F|',tFalse >< tTrue:7, tFalse >< tMaybe:7,tFalse >< tFalse:7);
  writeln;
end.
```


```txt

Output:
a AND b
      F      U      T
F|tFalse tFalse tFalse 
U|tFalse tMaybe tMaybe 
T|tFalse tMaybe tTrue  

 a OR b
      F      U      T
F|tFalse tMaybe tTrue  
U|tMaybe tMaybe tTrue  
T|tTrue  tTrue  tTrue  

 NOT a
F|tTrue  
U|tMaybe 
T|tFalse 

 a XOR b
      F      U      T
F|tFalse tMaybe tTrue  
U|tMaybe tMaybe tMaybe 
T|tTrue  tMaybe tFalse 

equality/equivalence and multiplication
      F      U      T
F|tTrue  tMaybe tFalse 
U|tMaybe tMaybe tMaybe 
T|tFalse tMaybe tTrue  

IMP. a.k.a. IfThen -> not(a) or b
      F      U      T
T|tTrue  tMaybe tFalse 
U|tTrue  tMaybe tMaybe 
F|tTrue  tTrue  tTrue

```



## Go

Go has four operators for the bool type: ==, &&, ||, and !.

```go
package main

import "fmt"

type trit int8

const (
    trFalse trit = iota - 1
    trMaybe
    trTrue
)

func (t trit) String() string {
    switch t {
    case trFalse:
        return "False"
    case trMaybe:
        return "Maybe"
    case trTrue:
        return "True "
    }
    panic("Invalid trit")
}

func trNot(t trit) trit {
    return -t
}

func trAnd(s, t trit) trit {
    if s < t {
        return s
    }
    return t
}

func trOr(s, t trit) trit {
    if s > t {
        return s
    }
    return t
}

func trEq(s, t trit) trit {
    return s * t
}

func main() {
    trSet := []trit{trFalse, trMaybe, trTrue}

    fmt.Println("t     not t")
    for _, t := range trSet {
        fmt.Println(t, trNot(t))
    }

    fmt.Println("\ns     t     s and t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trAnd(s, t))
        }
    }

    fmt.Println("\ns     t     s or t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trOr(s, t))
        }
    }

    fmt.Println("\ns     t     s eq t")
    for _, s := range trSet {
        for _, t := range trSet {
            fmt.Println(s, t, trEq(s, t))
        }
    }
}
```

{{out}}

```txt

t     not t
False True 
Maybe Maybe
True  False

s     t     s and t
False False False
False Maybe False
False True  False
Maybe False False
Maybe Maybe Maybe
Maybe True  Maybe
True  False False
True  Maybe Maybe
True  True  True 

s     t     s or t
False False False
False Maybe Maybe
False True  True 
Maybe False Maybe
Maybe Maybe Maybe
Maybe True  True 
True  False True 
True  Maybe True 
True  True  True 

s     t     s eq t
False False True 
False Maybe Maybe
False True  False
Maybe False Maybe
Maybe Maybe Maybe
Maybe True  Maybe
True  False False
True  Maybe Maybe
True  True  True 

```



## Groovy

Solution:

```groovy
enum Trit {
    TRUE, MAYBE, FALSE
  
    private Trit nand(Trit that) {
        switch ([this,that]) {
            case { FALSE in it }: return TRUE
            case { MAYBE in it }: return MAYBE
            default             : return FALSE
        }
    }
    private Trit nor(Trit that) { this.or(that).not() }
  
    Trit and(Trit that)   { this.nand(that).not() }
    Trit or(Trit that)    { this.not().nand(that.not()) }
    Trit not()            { this.nand(this) }
    Trit imply(Trit that) { this.nand(that.not()) }
    Trit equiv(Trit that) { this.and(that).or(this.nor(that)) }
}
```


Test:

```groovy
printf 'AND\n         '
Trit.values().each { b -> printf ('%6s', b) }
println '\n          ----- ----- -----'
Trit.values().each { a ->
    printf ('%6s | ', a)
    Trit.values().each { b -> printf ('%6s', a.and(b)) }
    println()
}

printf '\nOR\n         '
Trit.values().each { b -> printf ('%6s', b) }
println '\n          ----- ----- -----'
Trit.values().each { a ->
    printf ('%6s | ', a)
    Trit.values().each { b -> printf ('%6s', a.or(b)) }
    println()
}

println '\nNOT'
Trit.values().each {
    printf ('%6s | %6s\n', it, it.not())
}

printf '\nIMPLY\n         '
Trit.values().each { b -> printf ('%6s', b) }
println '\n          ----- ----- -----'
Trit.values().each { a ->
    printf ('%6s | ', a)
    Trit.values().each { b -> printf ('%6s', a.imply(b)) }
    println()
}

printf '\nEQUIV\n         '
Trit.values().each { b -> printf ('%6s', b) }
println '\n          ----- ----- -----'
Trit.values().each { a ->
    printf ('%6s | ', a)
    Trit.values().each { b -> printf ('%6s', a.equiv(b)) }
    println()
}
```


{{out}}

```txt
AND
           TRUE MAYBE FALSE
          ----- ----- -----
  TRUE |   TRUE MAYBE FALSE
 MAYBE |  MAYBE MAYBE FALSE
 FALSE |  FALSE FALSE FALSE

OR
           TRUE MAYBE FALSE
          ----- ----- -----
  TRUE |   TRUE  TRUE  TRUE
 MAYBE |   TRUE MAYBE MAYBE
 FALSE |   TRUE MAYBE FALSE

NOT
  TRUE |  FALSE
 MAYBE |  MAYBE
 FALSE |   TRUE

IMPLY
           TRUE MAYBE FALSE
          ----- ----- -----
  TRUE |   TRUE MAYBE FALSE
 MAYBE |   TRUE MAYBE MAYBE
 FALSE |   TRUE  TRUE  TRUE

EQUIV
           TRUE MAYBE FALSE
          ----- ----- -----
  TRUE |   TRUE MAYBE FALSE
 MAYBE |  MAYBE MAYBE MAYBE
 FALSE |  FALSE MAYBE  TRUE
```



## Haskell


All operations given in terms of NAND, the functionally-complete operation.


```Haskell
import Prelude hiding (Bool(..), not, (&&), (||), (==))

main = mapM_ (putStrLn . unlines . map unwords)
    [ table "not"     $ unary not
    , table "and"     $ binary (&&)
    , table "or"      $ binary (||)
    , table "implies" $ binary (=->)
    , table "equals"  $ binary (==)
    ]

data Trit = False | Maybe | True deriving (Show)

False `nand` _     = True
_     `nand` False = True
True  `nand` True  = False
_     `nand` _     = Maybe

not a = nand a a

a && b = not $ a `nand` b

a || b = not a `nand` not b

a =-> b = a `nand` not b

a == b = (a && b) || (not a && not b)

inputs1 = [True, Maybe, False]
inputs2 = [(a,b) | a <- inputs1, b <- inputs1]

unary f = map (\a -> [a, f a]) inputs1
binary f = map (\(a,b) -> [a, b, f a b]) inputs2

table name xs = map (map pad) . (header :) $ map (map show) xs
    where header = map (:[]) (take ((length $ head xs) - 1) ['A'..]) ++ [name]

pad s = s ++ replicate (5 - length s) ' '
```


{{out}}

```txt
A     not
True  False
Maybe Maybe
False True

A     B     and
True  True  True
True  Maybe Maybe
True  False False
Maybe True  Maybe
Maybe Maybe Maybe
Maybe False False
False True  False
False Maybe False
False False False

A     B     or
True  True  True
True  Maybe True
True  False True
Maybe True  True
Maybe Maybe Maybe
Maybe False Maybe
False True  True
False Maybe Maybe
False False False

A     B     implies
True  True  True
True  Maybe Maybe
True  False False
Maybe True  True
Maybe Maybe Maybe
Maybe False Maybe
False True  True
False Maybe True
False False True

A     B     equals
True  True  True
True  Maybe Maybe
True  False False
Maybe True  Maybe
Maybe Maybe Maybe
Maybe False Maybe
False True  False
False Maybe Maybe
False False True
```


=={{header|Icon}} and {{header|Unicon}}==
The following example works in both Icon and Unicon.  
There are a couple of comments on the code that pertain to the task requirements:
* Strictly speaking there are no binary values in Icon and Unicon. There are a number of flow control operations that result in expression success (and a result) or failure which affects flow.  As a result there really isn't a set of binary operators to map into ternary.  The example provides the minimum required by the task plus xor.
* The code below does not define a data type as it doesn't really make sense in this case.  Icon and Unicon can create records which would be overkill and clumsy in this case.  Unicon can create objects which would also be overkill.  The only remaining option is to reinterpret one of the existing types as ternary values.  The code below implements balanced ternary values as integers in order to simplify several of the functions.  
* The use of integers doesn't really support strings of trits well.  While there is a function showtrit to ease display a converse function to decode character trits in a string is not included.



```Icon
$define TRUE    1
$define FALSE  -1
$define UNKNOWN 0 

invocable all
link printf

procedure main()  # demonstrate ternary logic

ufunc := ["not3"]
bfunc := ["and3", "or3", "xor3", "eq3", "ifthen3"]

every f := !ufunc  do {   # display unary functions
   printf("\nunary function=%s:\n",f)
   every t1 := (TRUE | FALSE | UNKNOWN) do
      printf(" %s : %s\n",showtrit(t1),showtrit(not3(t1)))
   }

   
every f :=  !bfunc do {   # display binary functions
   printf("\nbinary function=%s:\n     ",f)
   every t1 := (&null | TRUE | FALSE | UNKNOWN) do { 
      printf(" %s : ",showtrit(\t1))
      every t2 := (TRUE | FALSE | UNKNOWN | &null) do {
         if /t1 then printf("  %s",showtrit(\t2)|"\n")
         else printf("  %s",showtrit(f(t1,\t2))|"\n")
         }
      }
   }
end

procedure showtrit(a)   #: return printable trit of error if invalid
return case a of {TRUE:"T";FALSE:"F";UNKNOWN:"?";default:runerr(205,a)}
end

procedure istrit(a)     #: return value of trit or error if invalid
return (TRUE|FALSE|UNKNOWN|runerr(205,a)) = a 
end

procedure not3(a)       #: not of trit or error if invalid
return FALSE * istrit(a)
end

procedure and3(a,b)     #: and of two trits or error if invalid
return min(istrit(a),istrit(b))
end

procedure or3(a,b)      #: or of two trits or error if invalid
return max(istrit(a),istrit(b))
end

procedure eq3(a,b)      #: equals of two trits or error if invalid
return istrit(a) * istrit(b)
end

procedure ifthen3(a,b)  #: if trit then trit or error if invalid
return case istrit(a) of { TRUE: istrit(b) ; UNKNOWN: or3(a,b); FALSE: TRUE }
end

procedure xor3(a,b)     #: xor of two trits or error if invalid
return not3(eq3(a,b))
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides support for the printf family of functions] 

{{out}}

```txt
unary function=not3:
 T : F
 F : T
 ? : ?

binary function=and3:
       T  F  ?
 T :   T  F  ?
 F :   F  F  F
 ? :   ?  F  ?

binary function=or3:
       T  F  ?
 T :   T  T  T
 F :   T  F  ?
 ? :   T  ?  ?

binary function=xor3:
       T  F  ?
 T :   F  T  ?
 F :   T  F  ?
 ? :   ?  ?  ?

binary function=eq3:
       T  F  ?
 T :   T  F  ?
 F :   F  T  ?
 ? :   ?  ?  ?

binary function=ifthen3:
       T  F  ?
 T :   T  F  ?
 F :   T  T  T
 ? :   T  ?  ?
```



## J


The designers of J felt that user defined types were harmful, so that part of the task will not be supported here.

Instead:

true: 1
false: 0
maybe: 0.5


```j
not=: -.
and=: <.
or =: >.
if =: (>. -.)"0~
eq =: (<.&-. >. <.)"0
```


Example use:


```j
   not 0 0.5 1
1 0.5 0

   0 0.5 1 and/ 0 0.5 1
0   0   0
0 0.5 0.5
0 0.5   1

   0 0.5 1 or/ 0 0.5 1
  0 0.5 1
0.5 0.5 1
  1   1 1

   0 0.5 1 if/ 0 0.5 1
  1   1 1
0.5 0.5 1
  0 0.5 1

   0 0.5 1 eq/ 0 0.5 1
  1 0.5   0
0.5 0.5 0.5
  0 0.5   1
```


Note that this implementation is a special case of "[[wp:fuzzy logic|fuzzy logic]]" (using a limited set of values).

Note that while <code>>.</code> and <code><.</code> could be used for boolean operations instead of J's <code>+.</code> and <code>*.</code>, the identity elements for >. and <. are not boolean values, but are negative and positive infinity.  See also: [[wp:Boolean ring|Boolean ring]]

Note that we might instead define values between 0 and 1 to represent independent probabilities:


```J
not=: -.
and=: *
or=: *&.-.
if =: (or -.)"0~
eq =: (*&-. or *)"0
```


However, while this might be a more intellectually satisfying approach, this gives us some different results from the task requirement, for the combination of two "maybe" values (we could "fix" this by adding some "logic" which replaced any non-integer value with "0.5" - this would satisfy literal compliance with the task specification and might even be a valid engineering choice if we are implementing in hardware, for example):


```J
   not 0 0.5 1
1 0.5 0

   0 0.5 1 and/ 0 0.5 1
0    0   0
0 0.25 0.5
0  0.5   1

   0 0.5 1 or/ 0 0.5 1
  0  0.5 1
0.5 0.75 1
  1    1 1

   0 0.5 1 if/ 0 0.5 1
  1    1 1
0.5 0.75 1
  0  0.5 1

   0 0.5 1 eq/ 0 0.5 1
  1    0.5   0
0.5 0.4375 0.5
  0    0.5   1
```


Another interesting possibility would involve using George Boole's original operations.  This leaves us without any "not", (if we include the definition of logical negation which was later added to the definition of Boolean algebra, then the only numbers which can be used with Boolean algebra are 1 and 0).  So, it's not clear how we would implement "if" or "eq".  However, "and" and "or" would look like this:


```J
and=: *.
or=: +.
```


And, the boolean result tables would look like this:


```J
   0 0.5 1 and/ 0 0.5 1
0   0 0
0 0.5 1
0   1 1

   0 0.5 1 or/ 0 0.5 1
  0 0.5   1
0.5 0.5 0.5
  1 0.5   1
```



## Java

{{works with|Java|1.5+}}

```java5
public class Logic{
	public static enum Trit{
		TRUE, MAYBE, FALSE;
		
		public Trit and(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return (other == FALSE) ? FALSE : MAYBE;
			}else{
				return FALSE;
			}
		}
		
		public Trit or(Trit other){
			if(this == TRUE){
				return TRUE;
			}else if(this == MAYBE){
				return (other == TRUE) ? TRUE : MAYBE;
			}else{
				return other;
			}
		}
		
		public Trit tIf(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return (other == TRUE) ? TRUE : MAYBE;
			}else{
				return TRUE;
			}
		}
		
		public Trit not(){
			if(this == TRUE){
				return FALSE;
			}else if(this == MAYBE){
				return MAYBE;
			}else{
				return TRUE;
			}
		}
		
		public Trit equals(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return MAYBE;
			}else{
				return other.not();
			}
		}
	}
	public static void main(String[] args){
		for(Trit a:Trit.values()){
			System.out.println("not " + a + ": " + a.not());
		}
		for(Trit a:Trit.values()){
			for(Trit b:Trit.values()){
				System.out.println(a+" and "+b+": "+a.and(b)+
						"\t "+a+" or "+b+": "+a.or(b)+
						"\t "+a+" implies "+b+": "+a.tIf(b)+
						"\t "+a+" = "+b+": "+a.equals(b));
			}
		}
	}
}
```

{{out}}

```txt
not TRUE: FALSE
not MAYBE: MAYBE
not FALSE: TRUE
TRUE and TRUE: TRUE	 TRUE or TRUE: TRUE	 TRUE implies TRUE: TRUE	 TRUE = TRUE: TRUE
TRUE and MAYBE: MAYBE	 TRUE or MAYBE: TRUE	 TRUE implies MAYBE: MAYBE	 TRUE = MAYBE: MAYBE
TRUE and FALSE: FALSE	 TRUE or FALSE: TRUE	 TRUE implies FALSE: FALSE	 TRUE = FALSE: FALSE
MAYBE and TRUE: MAYBE	 MAYBE or TRUE: TRUE	 MAYBE implies TRUE: TRUE	 MAYBE = TRUE: MAYBE
MAYBE and MAYBE: MAYBE	 MAYBE or MAYBE: MAYBE	 MAYBE implies MAYBE: MAYBE	 MAYBE = MAYBE: MAYBE
MAYBE and FALSE: FALSE	 MAYBE or FALSE: MAYBE	 MAYBE implies FALSE: MAYBE	 MAYBE = FALSE: MAYBE
FALSE and TRUE: FALSE	 FALSE or TRUE: TRUE	 FALSE implies TRUE: TRUE	 FALSE = TRUE: FALSE
FALSE and MAYBE: FALSE	 FALSE or MAYBE: MAYBE	 FALSE implies MAYBE: TRUE	 FALSE = MAYBE: MAYBE
FALSE and FALSE: FALSE	 FALSE or FALSE: FALSE	 FALSE implies FALSE: TRUE	 FALSE = FALSE: TRUE
```



## JavaScript

Let's use the trit already available in JavaScript:
true, false (both boolean) and undefined…

```JavaScript
var L3 = new Object();

L3.not = function(a) {
  if (typeof a == "boolean") return !a;
  if (a == undefined) return undefined;
  throw("Invalid Ternary Expression.");
}

L3.and = function(a, b) {
  if (typeof a == "boolean" && typeof b == "boolean") return a && b;
  if ((a == true && b == undefined) || (a == undefined && b == true)) return undefined;
  if ((a == false && b == undefined) || (a == undefined && b == false)) return false;
  if (a == undefined && b == undefined) return undefined;
  throw("Invalid Ternary Expression.");
}

L3.or = function(a, b) {
  if (typeof a == "boolean" && typeof b == "boolean") return a || b;
  if ((a == true && b == undefined) || (a == undefined && b == true)) return true;
  if ((a == false && b == undefined) || (a == undefined && b == false)) return undefined;
  if (a == undefined && b == undefined) return undefined;
  throw("Invalid Ternary Expression.");
}

// A -> B is equivalent to -A or B
L3.ifThen = function(a, b) {
  return L3.or(L3.not(a), b);
}

// A <=> B is equivalent to (A -> B) and (B -> A)
L3.iff = function(a, b) {
  return L3.and(L3.ifThen(a, b), L3.ifThen(b, a));
}

```

… and try these:
<lang>
L3.not(true)         // false
L3.not(var a)        // undefined

L3.and(true, a)      // undefined

L3.or(a, 2 == 3)     // false

L3.ifThen(true, a)   // undefined

L3.iff(a, 2 == 2)    // undefined     

```



## jq

jq itself does not have an extensible type system, so we'll use false, "maybe", and true
as the three values since ternary logic agrees with Boolean logic for true and false, and because jq prints these three values consistently.

For consistency, all the ternary logic operators are defined here with the prefix "ternary_", but such a prefix is only needed for "not", "and", and "or", as these are jq keywords. 
```jq
def ternary_nand(a; b):
  if a == false or b == false then true
  elif a == "maybe" or b == "maybe" then "maybe"
  else false
  end ;

def ternary_not(a):    ternary_nand(a; a);

def ternary_or(a; b):  ternary_nand( ternary_not(a); ternary_not(b) );

def ternary_nor(a; b): ternary_not( ternary_or(a;b) );

def ternary_and(a; b): ternary_not( ternary_nand(a; b) );

def ternary_imply(this; that):
  ternary_nand(this, ternary_not(that));

def ternary_equiv(this; that): 
  ternary_or( ternary_and(this; that); ternary_nor(this; that) );

def display_and(a; b):
  a as $a | b as $b 
  | "\($a) and \($b) is \( ternary_and($a; $b) )";
def display_equiv(a; b):
  a as $a | b as $b
  | "\($a) equiv \($b) is \( ternary_equiv($a; $b) )";
# etc etc

# Invoke the display functions:
display_and( (false, "maybe", true );  (false, "maybe", true) ),
display_equiv( (false, "maybe", true );  (false, "maybe", true) ),
"etc etc"

```

{{out}}

```txt
"false and false is false"
"false and maybe is false"
"false and true is false"
"maybe and false is false"
"maybe and maybe is maybe"
"maybe and true is maybe"
"true and false is false"
"true and maybe is maybe"
"true and true is true"
"false equiv false is true"
"false equiv maybe is maybe"
"false equiv true is false"
"maybe equiv false is maybe"
"maybe equiv maybe is maybe"
"maybe equiv true is maybe"
"true equiv false is false"
"true equiv maybe is maybe"
"true equiv true is true"
"etc etc"

```



## Julia

{{works with|Julia|0.6}}


```julia
@enum Trit False Maybe True
const trits = (False, Maybe, True)

Base.:!(a::Trit) = a == False ? True : a == Maybe ? Maybe : False
∧(a::Trit, b::Trit) = a == b == True ? True : (a, b) ∋ False ? False : Maybe
∨(a::Trit, b::Trit) = a == b == False ? False : (a, b) ∋ True ? True : Maybe
⊃(a::Trit, b::Trit) = a == False || b == True ? True : (a, b) ∋ Maybe ? Maybe : False
≡(a::Trit, b::Trit) = (a, b) ∋ Maybe ? Maybe : a == b ? True : False

println("Not (!):")
println(join(@sprintf("%10s%s is %5s", "!", t, !t) for t in trits))
println("And (∧):")
for a in trits
    println(join(@sprintf("%10s ∧ %5s is %5s", a, b, a ∧ b) for b in trits))
end
println("Or (∨):")
for a in trits
    println(join(@sprintf("%10s ∨ %5s is %5s", a, b, a ∨ b) for b in trits))
end
println("If Then (⊃):")
for a in trits
    println(join(@sprintf("%10s ⊃ %5s is %5s", a, b, a ⊃ b) for b in trits))
end
println("Equivalent (≡):")
for a in trits
    println(join(@sprintf("%10s ≡ %5s is %5s", a, b, a ≡ b) for b in trits))
end
```


{{out}}

```txt
Not (!):
         !False is  True         !Maybe is Maybe         !True is False
And (∧):
     False ∧ False is False     False ∧ Maybe is False     False ∧  True is False
     Maybe ∧ False is False     Maybe ∧ Maybe is Maybe     Maybe ∧  True is Maybe
      True ∧ False is False      True ∧ Maybe is Maybe      True ∧  True is  True
Or (∨):
     False ∨ False is False     False ∨ Maybe is Maybe     False ∨  True is  True
     Maybe ∨ False is Maybe     Maybe ∨ Maybe is Maybe     Maybe ∨  True is  True
      True ∨ False is  True      True ∨ Maybe is  True      True ∨  True is  True
If Then (⊃):
     False ⊃ False is  True     False ⊃ Maybe is  True     False ⊃  True is  True
     Maybe ⊃ False is Maybe     Maybe ⊃ Maybe is Maybe     Maybe ⊃  True is  True
      True ⊃ False is False      True ⊃ Maybe is Maybe      True ⊃  True is  True
Equivalent (≡):
     False ≡ False is  True     False ≡ Maybe is Maybe     False ≡  True is False
     Maybe ≡ False is Maybe     Maybe ≡ Maybe is Maybe     Maybe ≡  True is Maybe
      True ≡ False is False      True ≡ Maybe is Maybe      True ≡  True is  True
```



== Alternative version ==
{{works with|Julia|0.7}}

With Julia 1.0 and the new type <tt>missing</tt>, three-value logic is implemented by default

```julia
# built-in: true, false and missing

using Printf

const tril = (true, missing, false)

@printf("\n%8s | %8s\n", "A", "¬A")
for A in tril
    @printf("%8s | %8s\n", A, !A)
end

@printf("\n%8s | %8s | %8s\n", "A", "B", "A ∧ B")
for (A, B) in Iterators.product(tril, tril)
    @printf("%8s | %8s | %8s\n", A, B, A & B)
end

@printf("\n%8s | %8s | %8s\n", "A", "B", "A ∨ B")
for (A, B) in Iterators.product(tril, tril)
    @printf("%8s | %8s | %8s\n", A, B, A | B)
end

@printf("\n%8s | %8s | %8s\n", "A", "B", "A ≡ B")
for (A, B) in Iterators.product(tril, tril)
    @printf("%8s | %8s | %8s\n", A, B, A == B)
end

⊃(A, B) = B | !A

@printf("\n%8s | %8s | %8s\n", "A", "B", "A ⊃ B")
for (A, B) in Iterators.product(tril, tril)
    @printf("%8s | %8s | %8s\n", A, B, A ⊃ B)
end
```


{{out}}

```txt
       A |       ¬A
    true |    false
 missing |  missing
   false |     true

       A |        B |    A ∧ B
    true |     true |     true
 missing |     true |  missing
   false |     true |    false
    true |  missing |  missing
 missing |  missing |  missing
   false |  missing |    false
    true |    false |    false
 missing |    false |    false
   false |    false |    false

       A |        B |    A ∨ B
    true |     true |     true
 missing |     true |     true
   false |     true |     true
    true |  missing |     true
 missing |  missing |  missing
   false |  missing |  missing
    true |    false |     true
 missing |    false |  missing
   false |    false |    false

       A |        B |    A ≡ B
    true |     true |     true
 missing |     true |  missing
   false |     true |    false
    true |  missing |  missing
 missing |  missing |  missing
   false |  missing |  missing
    true |    false |    false
 missing |    false |  missing
   false |    false |     true

       A |        B |    A ⊃ B
    true |     true |     true
 missing |     true |     true
   false |     true |     true
    true |  missing |  missing
 missing |  missing |  missing
   false |  missing |     true
    true |    false |    false
 missing |    false |  missing
```



## Kotlin


```scala
// version 1.1.2

enum class Trit {
    TRUE, MAYBE, FALSE;
 
    operator fun not() = when (this) {
        TRUE  -> FALSE
        MAYBE -> MAYBE
        FALSE -> TRUE
    }

    infix fun and(other: Trit) = when (this) {
        TRUE  -> other
        MAYBE -> if (other == FALSE) FALSE else MAYBE
        FALSE -> FALSE
    }

    infix fun or(other: Trit) = when (this) {
        TRUE  -> TRUE
        MAYBE -> if (other == TRUE) TRUE else MAYBE
        FALSE -> other
    }

    infix fun imp(other: Trit) = when (this) {
        TRUE  -> other
        MAYBE -> if (other == TRUE) TRUE else MAYBE
        FALSE -> TRUE
    }

    infix fun eqv(other: Trit) = when (this) {
        TRUE  -> other
        MAYBE -> MAYBE
        FALSE -> !other
    }

    override fun toString() = this.name[0].toString()
}

fun main(args: Array<String>) {
    val ta = arrayOf(Trit.TRUE, Trit.MAYBE, Trit.FALSE)

    // not
    println("not")
    println("-------")
    for (t in ta) println(" $t  | ${!t}")
    println()

    // and
    println("and | T  M  F")
    println("-------------")
    for (t in ta) {
        print(" $t  | ")
        for (tt in ta) print("${t and tt}  ")
        println()
    }
    println()

    // or
    println("or  | T  M  F")
    println("-------------")
    for (t in ta) {
        print(" $t  | ")
        for (tt in ta) print("${t or tt}  ")
        println()
    }
    println()

    // imp
    println("imp | T  M  F")
    println("-------------")
    for (t in ta) {
        print(" $t  | ")
        for (tt in ta) print("${t imp tt}  ")
        println()
    }
    println()

    // eqv
    println("eqv | T  M  F")
    println("-------------")
    for (t in ta) {
        print(" $t  | ")
        for (tt in ta) print("${t eqv tt}  ")
        println()
    }
}
```


{{out}}

```txt

not
-------
 T  | F
 M  | M
 F  | T

and | T  M  F
-------------
 T  | T  M  F  
 M  | M  M  F  
 F  | F  F  F  

or  | T  M  F
-------------
 T  | T  T  T  
 M  | T  M  M  
 F  | T  M  F  

imp | T  M  F
-------------
 T  | T  M  F  
 M  | T  M  M  
 F  | T  T  T  

eqv | T  M  F
-------------
 T  | T  M  F  
 M  | M  M  M  
 F  | F  M  T  

```



## Liberty BASIC


```lb

'ternary logic
'0 1 2
'F ? T
'False Don't know True
'LB has NOT AND OR XOR, so we implement them.
'LB has no EQ, but XOR could be expressed via EQ. In 'normal' boolean at least.

global tFalse, tDontKnow, tTrue
tFalse = 0
tDontKnow = 1
tTrue = 2

print "Short and long names for ternary logic values"
for i = tFalse to tTrue
    print shortName3$(i);" ";longName3$(i)
next
print

print "Single parameter functions"
print "x";" ";"=x";"  ";"not(x)"
for i = tFalse to tTrue
    print shortName3$(i);"  ";shortName3$(i);"    ";shortName3$(not3(i))
next
print

print "Double  parameter fuctions"
print "x";" ";"y";"  ";"x AND y";"  ";"x OR y";"  ";"x EQ y";"  ";"x XOR y"
for a = tFalse to tTrue
    for b = tFalse to tTrue
        print shortName3$(a);" ";shortName3$(b);"     "; _
            shortName3$(and3(a,b));"       "; shortName3$(or3(a,b));"       "; _
            shortName3$(eq3(a,b));"        "; shortName3$(xor3(a,b))
    next
next

function and3(a,b)
    and3 = min(a,b)
end function

function or3(a,b)
    or3 = max(a,b)
end function

function eq3(a,b)
    select case
    case a=tDontKnow or b=tDontKnow
        eq3 = tDontKnow
    case a=b
        eq3 = tTrue
    case else
        eq3 = tFalse
    end select
end function

function xor3(a,b)
    xor3 = not3(eq3(a,b))
end function

function not3(b)
    not3 = 2-b
end function

'------------------------------------------------
function shortName3$(i)
   shortName3$ = word$("F ? T", i+1)
end function

function longName3$(i)
    longName3$ = word$("False,Don't know,True", i+1, ",")
end function
 
```


{{out}}

```txt

Short and long names for ternary logic values
F False
? Don't know
T True

Single parameter functions
x =x  not(x)
F  F    T
?  ?    ?
T  T    F

Double  parameter fuctions
x y  x AND y  x OR y  x EQ y  x XOR y
F F     F       F       T        F
F ?     F       ?       ?        ?
F T     F       T       F        T
? F     F       ?       ?        ?
? ?     ?       ?       ?        ?
? T     ?       T       ?        ?
T F     F       T       F        T
T ?     ?       T       ?        ?
T T     T       T       T        F

```



## Maple

The logic system in Maple is implicitly ternary with truth values '''true''', '''false''', and '''FAIL'''.

The following script generates all truth tables for Maple logical operations.  Note that in addition to the usual built-in logical operators for '''not''', '''or''', '''and''', and '''xor''', Maple also has '''implies'''.


```Maple
tv := [true, false, FAIL];
NotTable     := Array(1..3, i->not tv[i] );
AndTable     := Array(1..3, 1..3, (i,j)->tv[i] and tv[j] );
OrTable      := Array(1..3, 1..3, (i,j)->tv[i] or tv[j] );
XorTable     := Array(1..3, 1..3, (i,j)->tv[i] xor tv[j] );
ImpliesTable := Array(1..3, 1..3, (i,j)->tv[i] implies tv[j] );
```


{{Out}}


```Maple>
 tv := [true, false, FAIL];
                                     tv := [true, false, FAIL]

> NotTable := Array(1..3, i->not tv[i] );
                                  NotTable := [false, true, FAIL]

> AndTable := Array(1..3, 1..3, (i,j)->tv[i] and tv[j] );
                                           [true     false    FAIL ]
                                           [                       ]
                               AndTable := [false    false    false]
                                           [                       ]
                                           [FAIL     false    FAIL ]

> OrTable := Array(1..3, 1..3, (i,j)->tv[i] or tv[j] );
                                           [true    true     true]
                                           [                     ]
                                OrTable := [true    false    FAIL]
                                           [                     ]
                                           [true    FAIL     FAIL]

> XorTable := Array(1..3, 1..3, (i,j)->tv[i] xor tv[j] );
                                           [false    true     FAIL]
                                           [                      ]
                               XorTable := [true     false    FAIL]
                                           [                      ]
                                           [FAIL     FAIL     FAIL]

> ImpliesTable := Array(1..3, 1..3, (i,j)->tv[i] implies tv[j] );
                                              [true    false    FAIL]
                                              [                     ]
                              ImpliesTable := [true    true     true]
                                              [                     ]
                                              [true    FAIL     FAIL]
```



## Mathematica

Type definition is not allowed in Mathematica. We can just use the build-in symbols "True" and "False", and add a new symbol "Maybe".

```mathematica
Maybe /: ! Maybe = Maybe;
Maybe /: (And | Or | Nand | Nor | Xor | Xnor | Implies | Equivalent)[Maybe, Maybe] = Maybe;
```

Example:

```mathematica
trits = {True, Maybe, False};
Print@Grid[
   ArrayFlatten[{{{{Not}}, {{Null}}}, {List /@ trits, 
      List /@ Not /@ trits}}]];
Do[Print@Grid[
   ArrayFlatten[{{{{operator}}, {{Null, Null, 
        Null}}}, {{{Null}}, {trits}}, {List /@ trits, 
      Outer[operator, trits, trits]}}]], {operator, {And, Or, Nand, 
   Nor, Xor, Xnor, Implies, Equivalent}}]
```

{{out}}

```txt
Not	
True	False
Maybe	Maybe
False	True



And			
	True	Maybe	False
True	True	Maybe	False
Maybe	Maybe	Maybe	False
False	False	False	False



Or			
	True	Maybe	False
True	True	True	True
Maybe	True	Maybe	Maybe
False	True	Maybe	False



Nand			
	True	Maybe	False
True	False	Maybe	True
Maybe	Maybe	Maybe	True
False	True	True	True



Nor			
	True	Maybe	False
True	False	False	False
Maybe	False	Maybe	Maybe
False	False	Maybe	True



Xor			
	True	Maybe	False
True	False	Maybe	True
Maybe	Maybe	Maybe	Maybe
False	True	Maybe	False



Xnor			
	True	Maybe	False
True	True	Maybe	False
Maybe	Maybe	Maybe	Maybe
False	False	Maybe	True



Implies			
	True	Maybe	False
True	True	Maybe	False
Maybe	True	Maybe	Maybe
False	True	True	True



Equivalent			
	True	Maybe	False
True	True	Maybe	False
Maybe	Maybe	Maybe	Maybe
False	False	Maybe	True


```


=={{header|МК-61/52}}==
<lang>П0	Сx	С/П	^	1	+	3	*	+	1
+	3	x^y	ИП0	<->	/	[x]	^	^	3
/	[x]	3	*	-	1	-	С/П	1	5
6	3	3	БП	00	1	9	5	6	9
БП	00	1	5	9	2	9	БП	00	1
5	6	6	5	БП	00	/-/	ЗН	С/П
```


<u>Instruction</u>:

''БП <u>XX</u> С/П a ^ b С/П'',

where <u>XX</u> = 28 for ''AND''; 35 for ''OR''; 42 for ''implies''; 49 for ''equivalent''; 56 for ''NOT'';

''a'', ''b'' ∈ {-1, 0, 1}.


## Nim


```nim
type Trit* = enum ttrue, tmaybe, tfalse

proc `$`*(a: Trit): string =
  case a
  of ttrue: "T"
  of tmaybe: "?"
  of tfalse: "F"

proc `not`*(a: Trit): Trit =
  case a
  of ttrue: tfalse
  of tmaybe: tmaybe
  of tfalse: ttrue

proc `and`*(a, b: Trit): Trit =
  const t: array[Trit, array[Trit, Trit]] =
    [ [ttrue,  tmaybe, tfalse]
    , [tmaybe, tmaybe, tfalse]
    , [tfalse, tfalse, tfalse] ]
  t[a][b]

proc `or`*(a, b: Trit): Trit =
  const t: array[Trit, array[Trit, Trit]] =
    [ [ttrue, ttrue,  ttrue]
    , [ttrue, tmaybe, tmaybe]
    , [ttrue, tmaybe, tfalse] ]
  t[a][b]

proc then*(a, b: Trit): Trit =
  const t: array[Trit, array[Trit, Trit]] =
    [ [ttrue, tmaybe, tfalse]
    , [ttrue, tmaybe, tmaybe]
    , [ttrue, ttrue,  ttrue] ]
  t[a][b]

proc equiv*(a, b: Trit): Trit =
  const t: array[Trit, array[Trit, Trit]] =
    [ [ttrue,  tmaybe, tfalse]
    , [tmaybe, tmaybe, tmaybe]
    , [tfalse, tmaybe, ttrue] ]
  t[a][b]

import strutils

var
  op1 = ttrue
  op2 = ttrue

for t in Trit:
  echo "Not ", t , ": ", not t

for op1 in Trit:
  for op2 in Trit:
    echo "$# and   $#: $#".format(op1, op2, op1 and op2)
    echo "$# or    $#: $#".format(op1, op2, op1 or op2)
    echo "$# then  $#: $#".format(op1, op2, op1.then op2)
    echo "$# equiv $#: $#".format(op1, op2, op1.equiv op2)
```

{{out}}

```txt
Not T: F
Not ?: ?
Not F: T
T and   T: T
T or    T: T
T then  T: T
T equiv T: T
T and   ?: ?
T or    ?: T
T then  ?: ?
T equiv ?: ?
T and   F: F
T or    F: T
T then  F: F
T equiv F: F
? and   T: ?
? or    T: T
? then  T: T
? equiv T: ?
? and   ?: ?
? or    ?: ?
? then  ?: ?
? equiv ?: ?
? and   F: F
? or    F: ?
? then  F: ?
? equiv F: ?
F and   T: F
F or    T: T
F then  T: T
F equiv T: F
F and   ?: F
F or    ?: ?
F then  ?: T
F equiv ?: ?
F and   F: F
F or    F: F
F then  F: T
F equiv F: T
```



## OCaml



```ocaml
type trit = True | False | Maybe

let t_not = function
  | True -> False
  | False -> True
  | Maybe -> Maybe

let t_and a b = match (a,b) with
   | (True,True) -> True
   | (False,_)  | (_,False) -> False
   | _ -> Maybe

let t_or a b = t_not (t_and (t_not a) (t_not b))

let t_eq a b = match (a,b) with
   | (True,True) | (False,False) -> True
   | (False,True) | (True,False) -> False
   | _ -> Maybe

let t_imply a b = t_or (t_not a) b

let string_of_trit = function
  | True -> "True"
  | False -> "False"
  | Maybe -> "Maybe"

let () =
  let values = [| True; Maybe; False |] in
  let f = string_of_trit in
  Array.iter (fun v -> Printf.printf "Not %s: %s\n" (f v) (f (t_not v))) values;
  print_newline ();
  let print op str =
    Array.iter (fun a ->
      Array.iter (fun b ->
        Printf.printf "%s %s %s: %s\n" (f a) str (f b) (f (op a b))
      ) values
    ) values;
    print_newline ()
  in
  print t_and "And";
  print t_or "Or";
  print t_imply "Then";
  print t_eq "Equiv";
;;
```


{{out}}

```txt

Not True: False
Not Maybe: Maybe
Not False: True

True And True: True
True And Maybe: Maybe
True And False: False
Maybe And True: Maybe
Maybe And Maybe: Maybe
Maybe And False: False
False And True: False
False And Maybe: False
False And False: False

True Or True: True
True Or Maybe: True
True Or False: True
Maybe Or True: True
Maybe Or Maybe: Maybe
Maybe Or False: Maybe
False Or True: True
False Or Maybe: Maybe
False Or False: False

True Then True: True
True Then Maybe: Maybe
True Then False: False
Maybe Then True: True
Maybe Then Maybe: Maybe
Maybe Then False: Maybe
False Then True: True
False Then Maybe: True
False Then False: True

True Equiv True: True
True Equiv Maybe: Maybe
True Equiv False: False
Maybe Equiv True: Maybe
Maybe Equiv Maybe: Maybe
Maybe Equiv False: Maybe
False Equiv True: False
False Equiv Maybe: Maybe
False Equiv False: True

```

=== Using a general binary -> ternary transform ===
Instead of writing all of the truth-tables by hand, we can construct a general binary -> ternary transform and apply it to any logical function we want:

```OCaml
type trit = True | False | Maybe

let to_bin = function True -> [true] | False -> [false] | Maybe -> [true;false]

let eval f x =
   List.fold_left (fun l c -> List.fold_left (fun m d -> ((d c) :: m)) l f) [] x

let rec from_bin =
   function [true] -> True | [false] -> False
   | h :: t -> (match (h, from_bin t) with
      (true,True) -> True | (false,False) -> False | _ -> Maybe)
   | _ -> Maybe

let to_ternary1 uop = fun x -> from_bin (eval [uop] (to_bin x))
let to_ternary2 bop = fun x y -> from_bin (eval (eval [bop] (to_bin x)) (to_bin y))

let t_not   = to_ternary1 (not)
let t_and   = to_ternary2 (&&)
let t_or    = to_ternary2 (||)
let t_equiv = to_ternary2 (=)
let t_imply = to_ternary2 (fun p q -> (not p) || q)

let str = function True -> "True " | False -> "False" | Maybe -> "Maybe"
let iterv f = List.iter f [True; False; Maybe]

let table1 s u =
   print_endline ("\n"^s^":");
   iterv (fun v -> print_endline ("  "^(str v)^" -> "^(str (u v))));;

let table2 s b =
   print_endline ("\n"^s^":");
   iterv (fun u ->
      iterv (fun v ->
         print_endline ("  "^(str u)^" "^(str v)^" -> "^(str (b u v)))));;

table1 "not" t_not;;
table2 "and" t_and;;
table2 "or" t_or;;
table2 "equiv" t_equiv;;
table2 "implies" t_imply;;
```

{{out}}

```txt

not:
  True  -> False
  False -> True 
  Maybe -> Maybe

and:
  True  True  -> True 
  True  False -> False
  True  Maybe -> Maybe
  False True  -> False
  False False -> False
  False Maybe -> False
  Maybe True  -> Maybe
  Maybe False -> False
  Maybe Maybe -> Maybe

or:
  True  True  -> True 
  True  False -> True 
  True  Maybe -> True 
  False True  -> True 
  False False -> False
  False Maybe -> Maybe
  Maybe True  -> True 
  Maybe False -> Maybe
  Maybe Maybe -> Maybe

equiv:
  True  True  -> True 
  True  False -> False
  True  Maybe -> Maybe
  False True  -> False
  False False -> True 
  False Maybe -> Maybe
  Maybe True  -> Maybe
  Maybe False -> Maybe
  Maybe Maybe -> Maybe

implies:
  True  True  -> True 
  True  False -> False
  True  Maybe -> Maybe
  False True  -> True 
  False False -> True 
  False Maybe -> True 
  Maybe True  -> True 
  Maybe False -> Maybe
  Maybe Maybe -> Maybe
```



## ooRexx


```ooRexx

tritValues = .array~of(.trit~true, .trit~false, .trit~maybe)
tab = '09'x

say "not operation (\)"
loop a over tritValues
    say "\"a":" (\a)
end

say
say "and operation (&)"
loop aa over tritValues
    loop bb over tritValues
        say (aa" & "bb":" (aa&bb))
    end
end

say
say "or operation (|)"
loop aa over tritValues
    loop bb over tritValues
        say (aa" | "bb":" (aa|bb))
    end
end

say
say "implies operation (&&)"
loop aa over tritValues
    loop bb over tritValues
        say (aa" && "bb":" (aa&&bb))
    end
end

say
say "equals operation (=)"
loop aa over tritValues
    loop bb over tritValues
        say (aa" = "bb":" (aa=bb))
    end
end

::class trit
-- making this a private method so we can control the creation
-- of these.  We only allow 3 instances to exist
::method new class private
  forward class(super)

::method init class
  expose true false maybe
  -- delayed creation
  true = .nil
  false = .nil
  maybe = .nil

-- read only attribute access to the instances.
-- these methods create the appropriate singleton on the first call
::attribute true class get
  expose true
  if true == .nil then true = self~new("True")
  return true

::attribute false class get
  expose false
  if false == .nil then false = self~new("False")
  return false

::attribute maybe class get
  expose maybe
  if maybe == .nil then maybe = self~new("Maybe")
  return maybe

-- create an instance
::method init
  expose value
  use arg value

-- string method to return the value of the instance
::method string
  expose value
  return value

-- "and" method using the operator overload
::method "&"
  use strict arg other
  if self == .trit~true then return other
  else if self == .trit~maybe then do
      if other == .trit~false then return .trit~false
      else return .trit~maybe
  end
  else return .trit~false

-- "or" method using the operator overload
::method "|"
  use strict arg other
  if self == .trit~true then return .trit~true
  else if self == .trit~maybe then do
      if other == .trit~true then return .trit~true
      else return .trit~maybe
  end
  else return other

-- implies method...using the XOR operator for this
::method "&&"
  use strict arg other
  if self == .trit~true then return other
  else if self == .trit~maybe then do
      if other == .trit~true then return .trit~true
      else return .trit~maybe
  end
  else return .trit~true

-- "not" method using the operator overload
::method "\"
  if self == .trit~true then return .trit~false
  else if self == .trit~maybe then return .trit~maybe
  else return .trit~true

-- "equals" using the "=" override.  This makes a distinction between
-- the "==" operator, which is real equality and the "=" operator, which
-- is trinary equality.
::method "="
  use strict arg other
  if self == .trit~true then return other
  else if self == .trit~maybe then return .trit~maybe
  else return \other

```



```txt

not operation (\)
\True: False
\False: True
\Maybe: Maybe

and operation (&)
True & True: True
True & False: False
True & Maybe: Maybe
False & True: False
False & False: False
False & Maybe: False
Maybe & True: Maybe
Maybe & False: False
Maybe & Maybe: Maybe

or operation (|)
True | True: True
True | False: True
True | Maybe: True
False | True: True
False | False: False
False | Maybe: Maybe
Maybe | True: True
Maybe | False: Maybe
Maybe | Maybe: Maybe

implies operation (&&)
True && True: True
True && False: False
True && Maybe: Maybe
False && True: True
False && False: True
False && Maybe: True
Maybe && True: True
Maybe && False: Maybe
Maybe && Maybe: Maybe

equals operation (=)
True = True: True
True = False: False
True = Maybe: Maybe
False = True: False
False = False: True
False = Maybe: Maybe
Maybe = True: Maybe
Maybe = False: Maybe
Maybe = Maybe: Maybe

```



## Pascal


```pascal
Program TernaryLogic (output);

type
  trit = (terTrue, terMayBe, terFalse);

function terNot (a: trit): trit;
  begin
    case a of
      terTrue:  terNot := terFalse;
      terMayBe: terNot := terMayBe;
      terFalse: terNot := terTrue;
    end;
  end;

function terAnd (a, b: trit): trit;
  begin
    terAnd := terMayBe;
    if (a = terFalse) or (b = terFalse) then
      terAnd := terFalse
    else
      if (a = terTrue) and (b = terTrue) then
        terAnd := terTrue;
  end;

function terOr (a, b: trit): trit;
  begin
    terOr := terMayBe;
    if (a = terTrue) or (b = terTrue) then
      terOr := terTrue
    else
      if (a = terFalse) and (b = terFalse) then
        terOr := terFalse;
  end;

function terEquals (a, b: trit): trit;
  begin
    if a = b then
      terEquals := terTrue
    else
      if a <> b then
        terEquals := terFalse;
    if (a = terMayBe) or (b = terMayBe) then
      terEquals := terMayBe;
  end;

function terIfThen (a, b: trit): trit;
  begin
    terIfThen := terMayBe;
    if (a = terTrue) or (b = terFalse)  then
      terIfThen := terTrue
    else
      if (a = terFalse) and (b = terTrue) then
        terIfThen := terFalse;
  end;

function terToStr(a: trit): string;
  begin
    case a of
      terTrue:  terToStr := 'True ';
      terMayBe: terToStr := 'Maybe';
      terFalse: terToStr := 'False';
    end;
  end;

begin
  writeln('Ternary logic test:');
  writeln;
  writeln('NOT ', ' True ', ' Maybe', ' False');
  writeln('     ', terToStr(terNot(terTrue)), ' ', terToStr(terNot(terMayBe)), ' ', terToStr(terNot(terFalse)));
  writeln;
  writeln('AND   ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terAnd(terTrue,terTrue)),  ' ', terToStr(terAnd(terMayBe,terTrue)),  ' ', terToStr(terAnd(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terAnd(terTrue,terMayBe)), ' ', terToStr(terAnd(terMayBe,terMayBe)), ' ', terToStr(terAnd(terFalse,terMayBe)));
  writeln('False  ', terToStr(terAnd(terTrue,terFalse)), ' ', terToStr(terAnd(terMayBe,terFalse)), ' ', terToStr(terAnd(terFalse,terFalse)));
  writeln;
  writeln('OR    ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terOR(terTrue,terTrue)),  ' ', terToStr(terOR(terMayBe,terTrue)),  ' ', terToStr(terOR(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terOR(terTrue,terMayBe)), ' ', terToStr(terOR(terMayBe,terMayBe)), ' ', terToStr(terOR(terFalse,terMayBe)));
  writeln('False  ', terToStr(terOR(terTrue,terFalse)), ' ', terToStr(terOR(terMayBe,terFalse)), ' ', terToStr(terOR(terFalse,terFalse)));
  writeln;
  writeln('IFTHEN', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terIfThen(terTrue,terTrue)),  ' ', terToStr(terIfThen(terMayBe,terTrue)),  ' ', terToStr(terIfThen(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terIfThen(terTrue,terMayBe)), ' ', terToStr(terIfThen(terMayBe,terMayBe)), ' ', terToStr(terIfThen(terFalse,terMayBe)));
  writeln('False  ', terToStr(terIfThen(terTrue,terFalse)), ' ', terToStr(terIfThen(terMayBe,terFalse)), ' ', terToStr(terIfThen(terFalse,terFalse)));
  writeln;
  writeln('EQUAL ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terEquals(terTrue,terTrue)),  ' ', terToStr(terEquals(terMayBe,terTrue)),  ' ', terToStr(terEquals(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terEquals(terTrue,terMayBe)), ' ', terToStr(terEquals(terMayBe,terMayBe)), ' ', terToStr(terEquals(terFalse,terMayBe)));
  writeln('False  ', terToStr(terEquals(terTrue,terFalse)), ' ', terToStr(terEquals(terMayBe,terFalse)), ' ', terToStr(terEquals(terFalse,terFalse)));
  writeln;
end.
```

{{out}}

```txt

:> ./TernaryLogic
Ternary logic test:

NOT  True  Maybe False
     False Maybe True 

AND    True  Maybe False
True   True  Maybe False
Maybe  Maybe Maybe False
False  False False False

OR     True  Maybe False
True   True  True  True 
Maybe  True  Maybe Maybe
False  True  Maybe False

IFTHEN True  Maybe False
True   True  Maybe False
Maybe  True  Maybe Maybe
False  True  True  True 

EQUAL  True  Maybe False
True   True  Maybe False
Maybe  Maybe Maybe Maybe
False  False Maybe True 


```



## Perl

File <TT>Trit.pm</TT>:

```perl
package Trit;

# -1 = false ; 0 = maybe ; 1 = true

use Exporter 'import';

our @EXPORT_OK = qw(TRUE FALSE MAYBE is_true is_false is_maybe);
our %EXPORT_TAGS = (
	all => \@EXPORT_OK,
	const => [qw(TRUE FALSE MAYBE)],
	bool => [qw(is_true is_false is_maybe)],
);

use List::Util qw(min max);

use overload
'='  => sub { $_[0]->clone() },
'<=>'=> sub { $_[0]->cmp($_[1]) },
'cmp'=> sub { $_[0]->cmp($_[1]) },
'==' => sub { ${$_[0]} == ${$_[1]} },
'eq' => sub { $_[0]->equiv($_[1]) },
'>'  => sub { ${$_[0]} > ${$_[1]} },
'<'  => sub { ${$_[0]} < ${$_[1]} },
'>=' => sub { ${$_[0]} >= ${$_[1]} },
'<=' => sub { ${$_[0]} <= ${$_[1]} },
'|'  => sub { $_[0]->or($_[1]) },
'&'  => sub { $_[0]->and($_[1]) },
'!'  => sub { $_[0]->not() },
'~'  => sub { $_[0]->not() },
'""' => sub { $_[0]->tostr() },
'0+' => sub { $_[0]->tonum() },
;

sub new
{
	my ($class, $v) = @_;
	my $ret =
		!defined($v) ? 0 :
		$v eq 'true' ? 1 :
		$v eq 'false'? -1 :
		$v eq 'maybe'? 0 :
		$v > 0 ? 1 :
		$v < 0 ? -1 :
		0;
	return bless \$ret, $class;
}

sub TRUE()  { new Trit( 1) }
sub FALSE() { new Trit(-1) }
sub MAYBE() { new Trit( 0) }

sub clone
{
	my $ret = ${$_[0]};
	return bless \$ret, ref($_[0]);
}

sub tostr { ${$_[0]} > 0 ? "true" : ${$_[0]} < 0 ? "false" : "maybe" }
sub tonum { ${$_[0]} }

sub is_true { ${$_[0]} > 0 }
sub is_false { ${$_[0]} < 0 }
sub is_maybe { ${$_[0]} == 0 }

sub cmp { ${$_[0]} <=> ${$_[1]} }
sub not { new Trit(-${$_[0]}) }
sub and { new Trit(min(${$_[0]}, ${$_[1]}) ) }
sub or  { new Trit(max(${$_[0]}, ${$_[1]}) ) }

sub equiv { new Trit( ${$_[0]} * ${$_[1]} ) }
```

File <TT>test.pl</TT>:

```perl
use Trit ':all';

my @a = (TRUE(), MAYBE(), FALSE());

print "\na\tNOT a\n";
print "$_\t".(!$_)."\n" for @a;	# Example of use of prefix operator NOT. Tilde ~ also can be used.


print "\nAND\t".join("\t",@a)."\n";
for my $a (@a) {
	print $a;
	for my $b (@a) {
		print "\t".($a & $b);	# Example of use of infix & (and)
	}
	print "\n";
}

print "\nOR\t".join("\t",@a)."\n";
for my $a (@a) {
	print $a;
	for my $b (@a) {
		print "\t".($a | $b);	# Example of use of infix | (or)
	}
	print "\n";
}

print "\nEQV\t".join("\t",@a)."\n";
for my $a (@a) {
	print $a;
	for my $b (@a) {
		print "\t".($a eq $b);	# Example of use of infix eq (equivalence)
	}
	print "\n";
}

print "\n==\t".join("\t",@a)."\n";
for my $a (@a) {
	print $a;
	for my $b (@a) {
		print "\t".($a == $b);	# Example of use of infix == (equality)
	}
	print "\n";
}
```

{{out}}

```txt
a	NOT a
true	false
maybe	maybe
false	true

AND	true	maybe	false
true	true	maybe	false
maybe	maybe	maybe	false
false	false	false	false

OR	true	maybe	false
true	true	true	true
maybe	true	maybe	maybe
false	true	maybe	false

EQV	true	maybe	false
true	true	maybe	false
maybe	maybe	maybe	maybe
false	false	maybe	true

==	true	maybe	false
true	1		
maybe		1	
false			1
```


## Perl 6

{{Works with|rakudo|2018.03}}

The precedence of each operator is specified as equivalent to an existing operator.  We've taken the liberty of using a double arrow for implication, to avoid confusing it with <tt>⊃</tt>, (U+2283 SUPERSET OF).


```perl6
# Implementation:
enum Trit <Foo Moo Too>;

sub prefix:<¬> (Trit $a) { Trit(1-($a-1)) }

sub infix:<∧> (Trit $a, Trit $b) is equiv(&infix:<*>) { $a min $b }
sub infix:<∨> (Trit $a, Trit $b) is equiv(&infix:<+>) { $a max $b }

sub infix:<⇒> (Trit $a, Trit $b) is equiv(&infix:<..>) { ¬$a max $b }
sub infix:<≡> (Trit $a, Trit $b) is equiv(&infix:<eq>) { Trit(1 + ($a-1) * ($b-1)) }

# Testing:
say '¬';
say "Too {¬Too}";
say "Moo {¬Moo}";
say "Foo {¬Foo}";

sub tbl (&op,$name) {
    say '';
    say "$name   Too Moo Foo";
    say "   ╔═══════════";
    say "Too║{op Too,Too} {op Too,Moo} {op Too,Foo}";
    say "Moo║{op Moo,Too} {op Moo,Moo} {op Moo,Foo}";
    say "Foo║{op Foo,Too} {op Foo,Moo} {op Foo,Foo}";
}

tbl(&infix:<∧>, '∧');
tbl(&infix:<∨>, '∨');
tbl(&infix:<⇒>, '⇒');
tbl(&infix:<≡>, '≡');

say '';
say 'Precedence tests should all print "Too":';
say ~(
    Foo ∧ Too ∨ Too ≡ Too,
    Foo ∧ (Too ∨ Too) ≡ Foo,
    Too ∨ Too ∧ Foo ≡ Too,
    (Too ∨ Too) ∧ Foo ≡ Foo,

    ¬Too ∧ Too ∨ Too ≡ Too,
    ¬Too ∧ (Too ∨ Too) ≡ ¬Too,
    Too ∨ Too ∧ ¬Too ≡ Too,
    (Too ∨ Too) ∧ ¬Too ≡ ¬Too,
 
    Foo ∧ Too ∨ Foo ⇒ Foo ≡ Too,
    Foo ∧ Too ∨ Too ⇒ Foo ≡ Foo,
);
```


{{out}}

```txt
¬
Too Foo
Moo Moo
Foo Too

∧   Too Moo Foo
   ╔═══════════
Too║Too Moo Foo
Moo║Moo Moo Foo
Foo║Foo Foo Foo

∨   Too Moo Foo
   ╔═══════════
Too║Too Too Too
Moo║Too Moo Moo
Foo║Too Moo Foo

⇒   Too Moo Foo
   ╔═══════════
Too║Too Moo Foo
Moo║Too Moo Moo
Foo║Too Too Too

≡   Too Moo Foo
   ╔═══════════
Too║Too Moo Foo
Moo║Moo Moo Moo
Foo║Foo Moo Too

Precedence tests should all print "Too":
Too Too Too Too Too Too Too Too Too Too
```



## Phix


```Phix
enum type ternary T, M, F end type

function t_not(ternary a)
    return F+1-a
end function

function t_and(ternary a, ternary b)
    return iff(a=T and b=T?T:iff(a=F or b=F?F:M))   
end function

function t_or(ternary a, ternary b)
    return iff(a=T or b=T?T:iff(a=F and b=F?F:M))   
end function

function t_xor(ternary a, ternary b)
    return iff(a=M or b=M?M:iff(a=b?F:T))
end function

function t_implies(ternary a, ternary b)
    return iff(a=F or b=T?T:iff(a=T and b=F?F:M))   
end function

function t_equal(ternary a, ternary b)
    return iff(a=M or b=M?M:iff(a=b?T:F))
end function

function t_string(ternary a)
    return iff(a=T?"T":iff(a=M?"?":"F"))
end function

procedure show_truth_table(integer rid, integer unary, string name)
    printf(1,"%-3s |%s\n",{name,iff(unary?"":" T | ? | F")})
    printf(1,"----+---%s\n",{iff(unary?"":"+---+---")})
    for x=T to F do
        printf(1," %s ",{t_string(x)})
        if unary then
            printf(1," | %s",{t_string(call_func(rid,{x}))})
        else
            for y=T to F do
                printf(1," | %s",{t_string(call_func(rid,{x,y}))})
            end for
        end if
        printf(1,"\n")
    end for
    printf(1,"\n")
end procedure

show_truth_table(routine_id("t_not"),1,"not")
show_truth_table(routine_id("t_and"),0,"and")
show_truth_table(routine_id("t_or"),0,"or")
show_truth_table(routine_id("t_xor"),0,"xor")
show_truth_table(routine_id("t_implies"),0,"imp")
show_truth_table(routine_id("t_equal"),0,"eq")
```

{{out}}

```txt

not |
----+---
 T  | F
 ?  | ?
 F  | T

and | T | ? | F
----+---+---+---
 T  | T | ? | F
 ?  | ? | ? | F
 F  | F | F | F

or  | T | ? | F
----+---+---+---
 T  | T | T | T
 ?  | T | ? | ?
 F  | T | ? | F

xor | T | ? | F
----+---+---+---
 T  | F | ? | T
 ?  | ? | ? | ?
 F  | T | ? | F

imp | T | ? | F
----+---+---+---
 T  | T | ? | F
 ?  | T | ? | ?
 F  | T | T | T

eq  | T | ? | F
----+---+---+---
 T  | T | ? | F
 ?  | ? | ? | ?
 F  | F | ? | T

```



## PHP

Save the sample code as executable shell script on your *nix system:

```PHP
#!/usr/bin/php
<?php

# defined as numbers, so I can use max() and min() on it
if (! define('triFalse',0))  trigger_error('Unknown error defining!', E_USER_ERROR);
if (! define('triMaybe',1))  trigger_error('Unknown error defining!', E_USER_ERROR);
if (! define('triTrue', 2))  trigger_error('Unknown error defining!', E_USER_ERROR);

$triNotarray = array(triFalse=>triTrue, triMaybe=>triMaybe, triTrue=>triFalse);

# output helper
function triString ($tri) {
    if ($tri===triFalse) return 'false  ';
    if ($tri===triMaybe) return 'unknown';
    if ($tri===triTrue)  return 'true   ';
    trigger_error('triString: parameter not a tri value', E_USER_ERROR);
}

function triAnd() {
    if (func_num_args() < 2) 
       trigger_error('triAnd needs 2 or more parameters', E_USER_ERROR);
    return min(func_get_args());
}

function triOr() {
    if (func_num_args() < 2) 
       trigger_error('triOr needs 2 or more parameters', E_USER_ERROR);
    return max(func_get_args());
}

function triNot($t) {
    global $triNotarray; # using result table
    if (in_array($t, $triNotarray)) return $triNotarray[$t];
    trigger_error('triNot: Parameter is not a tri value', E_USER_ERROR);
}

function triImplies($a, $b) {
    if ($a===triFalse || $b===triTrue)  return triTrue;
    if ($a===triMaybe || $b===triMaybe) return triMaybe;
    # without parameter type check I just would return triFalse here
    if ($a===triTrue &&  $b===triFalse) return triFalse;
    trigger_error('triImplies: parameter type error', E_USER_ERROR);
}

function triEquiv($a, $b) {
    if ($a===triTrue)  return $b;
    if ($a===triMaybe) return $a;
    if ($a===triFalse) return triNot($b);
    trigger_error('triEquiv: parameter type error', E_USER_ERROR);
}

# data sampling

printf("--- Sample output for a equivalent b ---\n\n");

foreach ([triTrue,triMaybe,triFalse] as $a) {
    foreach ([triTrue,triMaybe,triFalse] as $b) {
        printf("for a=%s and b=%s a equivalent b is %s\n",
               triString($a), triString($b), triString(triEquiv($a, $b)));
    }
}


```

Sample output:

```txt
--- Sample output for a equivalent b ---

for a=true    and b=true    a equivalent b is true   
for a=true    and b=unknown a equivalent b is unknown
for a=true    and b=false   a equivalent b is false  
for a=unknown and b=true    a equivalent b is unknown
for a=unknown and b=unknown a equivalent b is unknown
for a=unknown and b=false   a equivalent b is unknown
for a=false   and b=true    a equivalent b is false  
for a=false   and b=unknown a equivalent b is unknown
for a=false   and b=false   a equivalent b is true   

```



## PicoLisp

In addition for the standard T (for "true") and NIL (for "false") we define 0 (zero, for "maybe").

```PicoLisp
(de 3not (A)
   (or (=0 A) (not A)) )

(de 3and (A B)
   (cond
      ((=T A) B)
      ((=0 A) (and B 0)) ) )

(de 3or (A B)
   (cond
      ((=T A) T)
      ((=0 A) (or (=T B) 0))
      (T B) ) )

(de 3impl (A B)
   (cond
      ((=T A) B)
      ((=0 A) (or (=T B) 0))
      (T T) ) )

(de 3equiv (A B)
   (cond
      ((=T A) B)
      ((=0 A) 0)
      (T (3not B)) ) )
```

Test:

```PicoLisp
(for X '(T 0 NIL)
   (println 'not X '-> (3not X)) )

(for Fun '((and . 3and) (or . 3or) (implies . 3impl) (equivalent . 3equiv))
   (for X '(T 0 NIL)
      (for Y '(T 0 NIL)
         (println X (car Fun) Y '-> ((cdr Fun) X Y)) ) ) )
```

{{out}}
<pre style="height:20em;overflow:scroll">not T -> NIL
not 0 -> 0
not NIL -> T
T and T -> T
T and 0 -> 0
T and NIL -> NIL
0 and T -> 0
0 and 0 -> 0
0 and NIL -> NIL
NIL and T -> NIL
NIL and 0 -> NIL
NIL and NIL -> NIL
T or T -> T
T or 0 -> T
T or NIL -> T
0 or T -> T
0 or 0 -> 0
0 or NIL -> 0
NIL or T -> T
NIL or 0 -> 0
NIL or NIL -> NIL
T implies T -> T
T implies 0 -> 0
T implies NIL -> NIL
0 implies T -> T
0 implies 0 -> 0
0 implies NIL -> 0
NIL implies T -> T
NIL implies 0 -> T
NIL implies NIL -> T
T equivalent T -> T
T equivalent 0 -> 0
T equivalent NIL -> NIL
0 equivalent T -> 0
0 equivalent 0 -> 0
0 equivalent NIL -> 0
NIL equivalent T -> NIL
NIL equivalent 0 -> 0
NIL equivalent NIL -> T
```



## Python

In Python, the keywords 'and', 'not', and 'or' are coerced to always work as boolean operators. I have therefore overloaded the boolean bitwise operators &, |, ^ to provide the required functionality.

```python
class Trit(int):
    def __new__(cls, value):
        if value == 'TRUE':
            value = 1
        elif value == 'FALSE':
            value = 0
        elif value == 'MAYBE':
            value = -1
        return super(Trit, cls).__new__(cls, value // (abs(value) or 1)) 

    def __repr__(self):
        if self > 0:
            return 'TRUE'
        elif self == 0:
            return 'FALSE'
        return 'MAYBE'

    def __str__(self):
        return repr(self)

    def __bool__(self):
        if self > 0:
            return True
        elif self == 0:
            return False
        else:
            raise ValueError("invalid literal for bool(): '%s'" % self)

    def __or__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][1]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][1]
            except:
                return NotImplemented

    def __ror__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][1]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][1]
            except:
                return NotImplemented

    def __and__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][0]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][0]
            except:
                return NotImplemented

    def __rand__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][0]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][0]
            except:
                return NotImplemented

    def __xor__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][2]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][2]
            except:
                return NotImplemented

    def __rxor__(self, other):
        if isinstance(other, Trit):
            return _ttable[(self, other)][2]
        else:
            try:
                return _ttable[(self, Trit(bool(other)))][2]
            except:
                return NotImplemented

    def __invert__(self):
        return _ttable[self]
    
    def __getattr__(self, name):
        if name in ('_n', 'flip'):
            # So you can do x._n == x.flip; the inverse of x
            # In Python 'not' is strictly boolean so we can't write `not x`
            # Same applies to keywords 'and' and 'or'.
            return _ttable[self]
        else:
            raise AttributeError 


        
TRUE, FALSE, MAYBE = Trit(1), Trit(0), Trit(-1)

_ttable = {
    #    A: -> flip_A
         TRUE: FALSE,
        FALSE:  TRUE,
        MAYBE: MAYBE,
    #     (A, B): -> (A_and_B, A_or_B, A_xor_B)
        (MAYBE, MAYBE): (MAYBE, MAYBE, MAYBE),
        (MAYBE, FALSE): (FALSE, MAYBE, MAYBE),
        (MAYBE,  TRUE): (MAYBE,  TRUE, MAYBE),
        (FALSE, MAYBE): (FALSE, MAYBE, MAYBE),
        (FALSE, FALSE): (FALSE, FALSE, FALSE),
        (FALSE,  TRUE): (FALSE,  TRUE,  TRUE),
        ( TRUE, MAYBE): (MAYBE,  TRUE, MAYBE),
        ( TRUE, FALSE): (FALSE,  TRUE,  TRUE),
        ( TRUE,  TRUE): ( TRUE,  TRUE, FALSE),
    }


values = ('FALSE', 'TRUE ', 'MAYBE')

print("\nTrit logical inverse, '~'")
for a in values:
    expr = '~%s' % a
    print('  %s = %s' % (expr, eval(expr)))

for op, ophelp in (('&', 'and'), ('|', 'or'), ('^', 'exclusive-or')):
    print("\nTrit logical %s, '%s'" % (ophelp, op))
    for a in values:
        for b in values:
            expr = '%s %s %s' % (a, op, b)
            print('  %s = %s' % (expr, eval(expr)))
```


{{out}}

```txt

Trit logical inverse, '~'
  ~FALSE = TRUE
  ~TRUE  = FALSE
  ~MAYBE = MAYBE

Trit logical and, '&'
  FALSE & FALSE = FALSE
  FALSE & TRUE  = FALSE
  FALSE & MAYBE = FALSE
  TRUE  & FALSE = FALSE
  TRUE  & TRUE  = TRUE
  TRUE  & MAYBE = MAYBE
  MAYBE & FALSE = FALSE
  MAYBE & TRUE  = MAYBE
  MAYBE & MAYBE = MAYBE

Trit logical or, '|'
  FALSE | FALSE = FALSE
  FALSE | TRUE  = TRUE
  FALSE | MAYBE = MAYBE
  TRUE  | FALSE = TRUE
  TRUE  | TRUE  = TRUE
  TRUE  | MAYBE = TRUE
  MAYBE | FALSE = MAYBE
  MAYBE | TRUE  = TRUE
  MAYBE | MAYBE = MAYBE

Trit logical exclusive-or, '^'
  FALSE ^ FALSE = FALSE
  FALSE ^ TRUE  = TRUE
  FALSE ^ MAYBE = MAYBE
  TRUE  ^ FALSE = TRUE
  TRUE  ^ TRUE  = FALSE
  TRUE  ^ MAYBE = MAYBE
  MAYBE ^ FALSE = MAYBE
  MAYBE ^ TRUE  = MAYBE
  MAYBE ^ MAYBE = MAYBE
```


;Extra doodling in the Python shell:

```txt
>>> values = (TRUE, FALSE, MAYBE)
>>> for a in values:
	for b in values:
		assert (a & ~b) | (b & ~a) == a ^ b

		
>>> 
```



## Racket


```racket
#lang typed/racket

; to avoid the hassle of adding a maybe value that is as special as
; the two standard booleans, we'll use symbols to make our own
(define-type trit (U 'true 'false 'maybe))

(: not (trit -> trit))
(define (not a)
  (case a
    [(true) 'false]
    [(maybe) 'maybe]
    [(false) 'true]))

(: and (trit trit -> trit))
(define (and a b)
  (case a
    [(false) 'false]
    [(maybe) (case b
               [(false) 'false]
               [else 'maybe])]
    [(true) (case b
              [(true) 'true]
              [(maybe) 'maybe]
              [(false) 'false])]))

(: or (trit trit -> trit))
(define (or a b)
  (case a
    [(true) 'true]
    [(maybe) (case b
               [(true) 'true]
               [else 'maybe])]
    [(false) (case b
               [(true) 'true]
               [(maybe) 'maybe]
               [(false) 'false])]))

(: ifthen (trit trit -> trit))
(define (ifthen a b)
  (case b
    [(true) 'true]
    [(maybe) (case a
               [(false) 'true]
               [else 'maybe])]
    [(false) (case a
               [(true) 'false]
               [(maybe) 'maybe]
               [(false) 'true])]))

(: iff (trit trit -> trit))
(define (iff a b)
  (case a
    [(maybe) 'maybe]
    [(true) b]
    [(false) (not b)]))

(for: : Void ([a (in-list '(true maybe false))])
      (printf "~a ~a = ~a~n" (object-name not) a (not a)))
(for: : Void ([proc (in-list (list and or ifthen iff))])
  (for*: : Void ([a (in-list '(true maybe false))]
                 [b (in-list '(true maybe false))])
         (printf "~a ~a ~a = ~a~n" a (object-name proc) b (proc a b))))
```


{{out}}

```txt
not true = false
not maybe = maybe
not false = true
true and true = true
true and maybe = maybe
true and false = false
maybe and true = maybe
maybe and maybe = maybe
maybe and false = false
false and true = false
false and maybe = false
false and false = false
true or true = true
true or maybe = true
true or false = true
maybe or true = true
maybe or maybe = maybe
maybe or false = maybe
false or true = true
false or maybe = maybe
false or false = false
true ifthen true = true
true ifthen maybe = maybe
true ifthen false = false
maybe ifthen true = true
maybe ifthen maybe = maybe
maybe ifthen false = maybe
false ifthen true = true
false ifthen maybe = true
false ifthen false = true
true iff true = true
true iff maybe = maybe
true iff false = false
maybe iff true = maybe
maybe iff maybe = maybe
maybe iff false = maybe
false iff true = false
false iff maybe = maybe
false iff false = true

```



## REXX

This REXX program is a re-worked version of the REXX program used for the Rosetta Code task:   ''truth table''.

```rexx
/*REXX program displays a ternary truth table  [true, false, maybe]   for the variables */
/*──── and one or more expressions.                                                     */
/*──── Infix notation is supported with one character propositional constants.          */
/*──── Variables (propositional constants) allowed:    A ──► Z,     a ──► z   except  u.*/
/*──── All propositional constants are case insensative  (except lowercase  v).         */
parse arg $express                               /*obtain optional argument from the CL.*/
if $express\=''  then do                         /*Got one?  Then show user's expression*/
                      call truthTable $express   /*display the user's truth table──►term*/
                      exit                       /*we're all done with the truth table. */
                      end

call truthTable  "a & b ; AND"
call truthTable  "a | b ; OR"
call truthTable  "a ^ b ; XOR"
call truthTable  "a ! b ; NOR"
call truthTable  "a ¡ b ; NAND"
call truthTable  "a xnor b ; XNOR"               /*XNOR  is the same as  NXOR.          */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
truthTable: procedure; parse arg $ ';' comm 1 $o;        $o=strip($o)
      $=translate(strip($), '|', "v");                   $u=$;        upper $u
      $u=translate($u, '()()()', "[]{}«»");              $$.=0;       PCs=;        hdrPCs=
      @abc= 'abcdefghijklmnopqrstuvwxyz';                @abcU=@abc;  upper @abcU
      @= 'ff'x                                         /*─────────infix operators───────*/
      op.=                                             /*a single quote (') wasn't      */
                                                       /*     implemented for negation. */
      op.0  = 'false  boolFALSE'                       /*unconditionally  FALSE         */
      op.1  = 'and    and & *'                         /* AND, conjunction              */
      op.2  = 'naimpb NaIMPb'                          /*not A implies B                */
      op.3  = 'boolb  boolB'                           /*B  (value of)                  */
      op.4  = 'nbimpa NbIMPa'                          /*not B implies A                */
      op.5  = 'boola  boolA'                           /*A  (value of)                  */
      op.6  = 'xor    xor && % ^'                      /* XOR, exclusive OR             */
      op.7  = 'or     or | + v'                        /*  OR, disjunction              */
      op.8  = 'nor    nor ! ↓'                         /* NOR, not OR, Pierce operator  */
      op.9  = 'xnor   xnor nxor'                       /*NXOR, not exclusive OR, not XOR*/
      op.10 = 'notb   notB'                            /*not B (value of)               */
      op.11 = 'bimpa  bIMPa'                           /*    B implies A                */
      op.12 = 'nota   notA'                            /*not A (value of)               */
      op.13 = 'aimpb  aIMPb'                           /*    A implies B                */
      op.14 = 'nand   nand ¡ ↑'                        /*NAND, not AND, Sheffer operator*/
      op.15 = 'true   boolTRUE'                        /*unconditionally   TRUE         */
                                                       /*alphabetic names need changing.*/
      op.16 = '\   NOT ~ ─ . ¬'                        /* NOT, negation                 */
      op.17 = '>   GT'                                 /*conditional greater than       */
      op.18 = '>=  GE ─> => ──> ==>' "1a"x             /*conditional greater than or eq.*/
      op.19 = '<   LT'                                 /*conditional less than          */
      op.20 = '<=  LE <─ <= <── <=='                   /*conditional less then or equal */
      op.21 = '\=  NE ~= ─= .= ¬='                     /*conditional not equal to       */
      op.22 = '=   EQ EQUAL EQUALS =' "1b"x            /*biconditional  (equals)        */
      op.23 = '0   boolTRUE'                           /*TRUEness                       */
      op.24 = '1   boolFALSE'                          /*FALSEness                      */

      op.25 = 'NOT NOT NEG'                            /*not, neg  (negative)           */

        do jj=0  while  op.jj\=='' | jj<16             /*change opers──►what REXX likes.*/
        new=word(op.jj,1)
          do kk=2  to words(op.jj)                     /*handle each token separately.  */
          _=word(op.jj, kk);     upper _
          if wordpos(_, $u)==0   then iterate          /*no such animal in this string. */
          if datatype(new, 'm')  then new!=@           /*expresion needs transcribing.  */
                                 else new!=new
          $u=changestr(_, $u, new!)                    /*transcribe the function (maybe)*/
          if new!==@  then $u=changeFunc($u, @, new)   /*use the internal boolean name. */
          end   /*kk*/
        end     /*jj*/

      $u=translate($u, '()', "{}")                     /*finish cleaning up transcribing*/
            do jj=1  for length(@abcU)                 /*see what variables are used.   */
            _=substr(@abcU, jj, 1)                     /*use available upercase alphabet*/
            if pos(_,$u)==0  then iterate              /*found one?   No, keep looking. */
            $$.jj=2                                    /*found:  set upper bound for it.*/
            PCs=PCs _                                  /*also, add to propositional cons*/
            hdrPCs=hdrPCS  center(_, length('false'))  /*build a propositional cons hdr.*/
            end   /*jj*/
      $u=PCs  '('$u")"                                 /*sep prop. cons. from expression*/
      ptr='_────►_'                                    /*a pointer for the truth table. */
      hdrPCs=substr(hdrPCs,2)                          /*create a header for prop. cons.*/
      say hdrPCs left('', length(ptr) -1)   $o         /*show prop cons hdr +expression.*/
      say copies('───── ', words(PCs))   left('', length(ptr)-2)   copies('─', length($o))
                                                       /*Note: "true"s:  right─justified*/
              do a=0  to $$.1
               do b=0  to $$.2
                do c=0  to $$.3
                 do d=0  to $$.4
                  do e=0  to $$.5
                   do f=0  to $$.6
                    do g=0  to $$.7
                     do h=0  to $$.8
                      do i=0  to $$.9
                       do j=0  to $$.10
                        do k=0  to $$.11
                         do l=0  to $$.12
                          do m=0  to $$.13
                           do n=0  to $$.14
                            do o=0  to $$.15
                             do p=0  to $$.16
                              do q=0  to $$.17
                               do r=0  to $$.18
                                do s=0  to $$.19
                                 do t=0  to $$.20
                                  do u=0  to $$.21
                                   do !=0  to $$.22
                                    do w=0  to $$.23
                                     do x=0  to $$.24
                                      do y=0  to $$.25
                                       do z=0  to $$.26
                                       interpret '_=' $u             /*evaluate truth T.*/
                                       _=changestr(0, _, 'false')    /*convert 0──►false*/
                                       _=changestr(1, _, '_true')    /*convert 1──►_true*/
                                       _=changestr(2, _, 'maybe')    /*convert 2──►maybe*/
                                       _=insert(ptr, _, wordindex(_, words(_)) -1) /*──►*/
                                       say translate(_, , '_')       /*display truth tab*/
                                       end   /*z*/
                                      end    /*y*/
                                     end     /*x*/
                                    end      /*w*/
                                   end       /*v*/
                                  end        /*u*/
                                 end         /*t*/
                                end          /*s*/
                               end           /*r*/
                              end            /*q*/
                             end             /*p*/
                            end              /*o*/
                           end               /*n*/
                          end                /*m*/
                         end                 /*l*/
                        end                  /*k*/
                       end                   /*j*/
                      end                    /*i*/
                     end                     /*h*/
                    end                      /*g*/
                   end                       /*f*/
                  end                        /*e*/
                 end                         /*d*/
                end                          /*c*/
               end                           /*b*/
              end                            /*a*/
      say
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
scan: procedure; parse arg x,at;    L=length(x);    t=L;     lp=0;     apost=0;    quote=0
      if at<0  then do;   t=1;   x= translate(x, '()', ")(");    end

                      do j=abs(at)  to t  by sign(at);  _=substr(x,j,1);  __=substr(x,j,2)
                      if quote           then do; if _\=='"'  then iterate
                                              if __=='""'     then do; j=j+1; iterate; end
                                              quote=0;  iterate
                                              end
                      if apost           then do; if _\=="'"  then iterate
                                              if __=="''"     then do; j=j+1; iterate; end
                                              apost=0;  iterate
                                              end
                      if _=='"'          then do;  quote=1;                   iterate; end
                      if _=="'"          then do;  apost=1;                   iterate; end
                      if _==' '          then iterate
                      if _=='('          then do;  lp=lp+1;                   iterate; end
                      if lp\==0          then do;  if _==')'  then lp=lp-1;   iterate; end
                      if datatype(_,'U') then return j - (at<0)
                      if at<0            then return j + 1
                      end   /*j*/
      return min(j,L)
/*──────────────────────────────────────────────────────────────────────────────────────*/
changeFunc: procedure;  parse arg z,fC,newF;       funcPos= 0
            do forever
            funcPos= pos(fC, z, funcPos + 1);      if funcPos==0  then return z
            origPos= funcPos
                  z= changestr(fC, z, ",'"newF"',")
            funcPos= funcPos + length(newF) + 4
              where= scan(z, funcPos)     ;        z= insert(    '}',  z,  where)
              where= scan(z, 1 - origPos) ;        z= insert('trit{',  z,  where)
            end   /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
trit: procedure; arg a,$,b;   v= \(a==2 | b==2);    o= (a==1 | b==1);     z= (a==0 | b==0)
            select
            when $=='FALSE'   then            return 0
            when $=='AND'     then if v  then return a & b;      else return 2
            when $=='NAIMPB'  then if v  then return \(\a & \b); else return 2
            when $=='BOOLB'   then            return b
            when $=='NBIMPA'  then if v  then return \(\b & \a); else return 2
            when $=='BOOLA'   then            return a
            when $=='XOR'     then if v  then return a && b    ; else return 2
            when $=='OR'      then if v  then return a | b     ; else  if o  then return 1
                                                                             else return 2
            when $=='NOR'     then if v  then return \(a | b)  ; else return 2
            when $=='XNOR'    then if v  then return \(a && b) ; else return 2
            when $=='NOTB'    then if v  then return \b        ; else return 2
            when $=='NOTA'    then if v  then return \a        ; else return 2
            when $=='AIMPB'   then if v  then return \(a & \b) ; else return 2
            when $=='NAND'    then if v  then return \(a &  b) ; else  if z  then return 1
                                                                             else return 2
            when $=='TRUE'    then           return   1
            otherwise                        return -13       /*error, unknown function.*/
            end   /*select*/

```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here    ──►    [[CHANGESTR.REX]].



'''output'''

```txt

  A     B          a & b ; AND
───── ─────        ───────────
false false  ────► false
false  true  ────► false
false maybe  ────► maybe
 true false  ────► false
 true  true  ────►  true
 true maybe  ────► maybe
maybe false  ────► maybe
maybe  true  ────► maybe
maybe maybe  ────► maybe

  A     B          a | b ; OR
───── ─────        ──────────
false false  ────► false
false  true  ────►  true
false maybe  ────► maybe
 true false  ────►  true
 true  true  ────►  true
 true maybe  ────►  true
maybe false  ────► maybe
maybe  true  ────►  true
maybe maybe  ────► maybe

  A     B          a ^ b ; XOR
───── ─────        ───────────
false false  ────► false
false  true  ────►  true
false maybe  ────► maybe
 true false  ────►  true
 true  true  ────► false
 true maybe  ────► maybe
maybe false  ────► maybe
maybe  true  ────► maybe
maybe maybe  ────► maybe

  A     B          a ! b ; NOR
───── ─────        ───────────
false false  ────►  true
false  true  ────► false
false maybe  ────► maybe
 true false  ────► false
 true  true  ────► false
 true maybe  ────► maybe
maybe false  ────► maybe
maybe  true  ────► maybe
maybe maybe  ────► maybe

  A     B          a ¡ b ; NAND
───── ─────        ────────────
false false  ────►  true
false  true  ────►  true
false maybe  ────►  true
 true false  ────►  true
 true  true  ────► false
 true maybe  ────► maybe
maybe false  ────►  true
maybe  true  ────► maybe
maybe maybe  ────► maybe

  A     B          a xnor b ; XNOR
───── ─────        ───────────────
false false  ────►  true
false  true  ────► false
false maybe  ────► maybe
 true false  ────► false
 true  true  ────►  true
 true maybe  ────► maybe
maybe false  ────► maybe
maybe  true  ────► maybe
maybe maybe  ────► maybe

```



## Ruby

Ruby, like Smalltalk, has two boolean classes: TrueClass for <code>true</code> and FalseClass for <code>false</code>. We add a third class, MaybeClass for <code>MAYBE</code>, and define ternary logic for all three classes.

We keep <code>!a</code>, <code>a & b</code> and so on for binary logic. We add <code>!a.trit</code>, <code>a.trit & b</code> and so on for ternary logic. The code for <code>!a.trit</code> uses <code>def !</code>, which works with Ruby 1.9, but fails as a syntax error with Ruby 1.8.

{{works with|Ruby|1.9}}

```ruby
# trit.rb - ternary logic
# http://rosettacode.org/wiki/Ternary_logic

require 'singleton'

# MAYBE, the only instance of MaybeClass, enables a system of ternary
# logic using TrueClass#trit, MaybeClass#trit and FalseClass#trit.
#
#  !a.trit      # ternary not
#  a.trit & b   # ternary and
#  a.trit | b   # ternary or
#  a.trit ^ b   # ternary exclusive or
#  a.trit == b  # ternary equal
#
# Though +true+ and +false+ are internal Ruby values, +MAYBE+ is not.
# Programs may want to assign +maybe = MAYBE+ in scopes that use
# ternary logic. Then programs can use +true+, +maybe+ and +false+.
class MaybeClass
  include Singleton

  #  maybe.to_s  # => "maybe"
  def to_s; "maybe"; end
end

MAYBE = MaybeClass.instance

class TrueClass
  TritMagic = Object.new
  class << TritMagic
    def index; 0; end
    def !; false; end
    def & other; other; end
    def | other; true; end
    def ^ other; [false, MAYBE, true][other.trit.index]; end
    def == other; other; end
  end

  # Performs ternary logic. See MaybeClass.
  #  !true.trit        # => false
  #  true.trit & obj   # => obj
  #  true.trit | obj   # => true
  #  true.trit ^ obj   # => false, maybe or true
  #  true.trit == obj  # => obj
  def trit; TritMagic; end
end

class MaybeClass
  TritMagic = Object.new
  class << TritMagic
    def index; 1; end
    def !; MAYBE; end
    def & other; [MAYBE, MAYBE, false][other.trit.index]; end
    def | other; [true, MAYBE, MAYBE][other.trit.index]; end
    def ^ other; MAYBE; end
    def == other; MAYBE; end
  end

  # Performs ternary logic. See MaybeClass.
  #  !maybe.trit        # => maybe
  #  maybe.trit & obj   # => maybe or false
  #  maybe.trit | obj   # => true or maybe
  #  maybe.trit ^ obj   # => maybe
  #  maybe.trit == obj  # => maybe
  def trit; TritMagic; end
end

class FalseClass
  TritMagic = Object.new
  class << TritMagic
    def index; 2; end
    def !; true; end
    def & other; false; end
    def | other; other; end
    def ^ other; other; end
    def == other; [false, MAYBE, true][other.trit.index]; end
  end

  # Performs ternary logic. See MaybeClass.
  #  !false.trit        # => true
  #  false.trit & obj   # => false
  #  false.trit | obj   # => obj
  #  false.trit ^ obj   # => obj
  #  false.trit == obj  # => false, maybe or true
  def trit; TritMagic; end
end
```


This IRB session shows ternary not, and, or, equal.


```ruby
$ irb
irb(main):001:0> require './trit'
=> true
irb(main):002:0> maybe = MAYBE
=> maybe
irb(main):003:0> !true.trit       
=> false
irb(main):004:0> !maybe.trit
=> maybe
irb(main):005:0> maybe.trit & false
=> false
irb(main):006:0> maybe.trit | true
=> true
irb(main):007:0> false.trit == true       
=> false
irb(main):008:0> false.trit == maybe
=> maybe
```


This program shows all 9 outcomes from <code>a.trit ^ b</code>.


```ruby
require 'trit'
maybe = MAYBE

[true, maybe, false].each do |a|
  [true, maybe, false].each do |b|
    printf "%5s ^ %5s => %5s\n", a, b, a.trit ^ b
  end
end
```



```txt
$ ruby -I. trit-xor.rb
 true ^  true => false
 true ^ maybe => maybe
 true ^ false =>  true
maybe ^  true => maybe
maybe ^ maybe => maybe
maybe ^ false => maybe
false ^  true =>  true
false ^ maybe => maybe
false ^ false => false
```



## Run BASIC


```runbasic
testFalse	= 0  ' F
testDoNotKnow	= 1  ' ?
testTrue	= 2  ' T
 
print "Short and long names for ternary logic values"
for i = testFalse to testTrue
    print shortName3$(i);" ";longName3$(i)
next i
print
 
print "Single parameter functions"
print "x";" ";"=x";"  ";"not(x)"
for i = testFalse to testTrue
    print shortName3$(i);"  ";shortName3$(i);"    ";shortName3$(not3(i))
next
print
 
print "Double  parameter fuctions"
html "<table border=1><TR align=center bgcolor=wheat><TD>x</td><td>y</td><td>x AND y</td><td>x OR y</td><td>x EQ y</td><td>x XOR y</td></tr>"
for a	= testFalse to testTrue
    for b	= testFalse to testTrue
      html "<TR align=center><td>"
      html shortName3$(a);        "</td><td>";shortName3$(b);        "</td><td>"
      html shortName3$(and3(a,b));"</td><td>";shortName3$(or3(a,b)); "</td><td>"
      html shortName3$(eq3(a,b)); "</td><td>";shortName3$(xor3(a,b));"</td></tr>"
    next
next
html "</table>"
function and3(a,b)
    and3	= min(a,b)
end function
 
function or3(a,b)
    or3	= max(a,b)
end function
 
function eq3(a,b)
    eq3 	= testFalse
    if a	= tDontKnow or b	= tDontKnow then eq3	= tDontKnow
    if a	= b then eq3	= testTrue
end function
 
function xor3(a,b)
    xor3	= not3(eq3(a,b))
end function
 
function not3(b)
    not3	= 2-b
end function
 
'------------------------------------------------
function shortName3$(i)
   shortName3$	= word$("F ? T", i+1)
end function
 
function longName3$(i)
    longName3$	= word$("False,Don't know,True", i+1, ",")
end function
```

```txt
Short and long names for ternary logic values
F False
? Don't know
T True

Single parameter functions
x =x  not(x)
F  F  T
?  ?  ?
T  T  F

Double  parameter fuctions
```

<table border=1><TR align=center bgcolor=wheat><TD>x</td><td>y</td><td>x AND y</td><td>x OR y</td><td>x EQ y</td><td>x XOR y</td></tr><TR align=center><td>F</td><td>F</td><td>F</td><td>F</td><td>T</td><td>F</td></tr><TR align=center><td>F</td><td>?</td><td>F</td><td>?</td><td>F</td><td>T</td></tr><TR align=center><td>F</td><td>T</td><td>F</td><td>T</td><td>F</td><td>T</td></tr><TR align=center><td>?</td><td>F</td><td>F</td><td>?</td><td>F</td><td>T</td></tr><TR align=center><td>?</td><td>?</td><td>?</td><td>?</td><td>T</td><td>F</td></tr><TR align=center><td>?</td><td>T</td><td>?</td><td>T</td><td>F</td><td>T</td></tr><TR align=center><td>T</td><td>F</td><td>F</td><td>T</td><td>F</td><td>T</td></tr><TR align=center><td>T</td><td>?</td><td>?</td><td>T</td><td>F</td><td>T</td></tr><TR align=center><td>T</td><td>T</td><td>T</td><td>T</td><td>T</td><td>F</td></tr></table>

=={{Header|Rust}}==
{{trans|Kotlin}}

```Rust
use std::{ops, fmt};

#[derive(Copy, Clone, Debug)]
enum Trit {
    True,
    Maybe,
    False,
}

impl ops::Not for Trit {
    type Output = Self;
    fn not(self) -> Self {
        match self {
            Trit::True => Trit::False,
            Trit::Maybe => Trit::Maybe,
            Trit::False => Trit::True,
        }
    }
}

impl ops::BitAnd for Trit {
    type Output = Self;
    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (Trit::True, Trit::True) => Trit::True,
            (Trit::False, _) | (_, Trit::False) => Trit::False,
            _ => Trit::Maybe,
        }
    }
}

impl ops::BitOr for Trit {
    type Output = Self;
    fn bitor(self, other: Self) -> Self {
        match (self, other) {
            (Trit::True, _) | (_, Trit::True) => Trit::True,
            (Trit::False, Trit::False) => Trit::False,
            _ => Trit::Maybe,
        }
    }
}

impl Trit {
    fn imp(self, other: Self) -> Self {
        match self {
            Trit::True => other,
            Trit::Maybe => {
                if let Trit::True = other {
                    Trit::True
                } else {
                    Trit::Maybe
                }
            }
            Trit::False => Trit::True,
        }
    }

    fn eqv(self, other: Self) -> Self {
        match self {
            Trit::True => other,
            Trit::Maybe => Trit::Maybe,
            Trit::False => !other,
        }
    }
}

impl fmt::Display for Trit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Trit::True => 'T',
                Trit::Maybe => 'M',
                Trit::False => 'F',
            }
        )
    }
}

static TRITS: [Trit; 3] = [Trit::True, Trit::Maybe, Trit::False];

fn main() {
    println!("not");
    println!("-------");
    for &t in &TRITS {
        println!(" {}  | {}", t, !t);
    }
    
    table("and", |a, b| a & b);
    table("or", |a, b| a | b);
    table("imp", |a, b| a.imp(b));
    table("eqv", |a, b| a.eqv(b));
}

fn table(title: &str, f: impl Fn(Trit, Trit) -> Trit) {
    println!();
    println!("{:3} | T  M  F", title);
    println!("-------------");
    for &t1 in &TRITS {
        print!(" {}  | ", t1);
        for &t2 in &TRITS {
            print!("{}  ", f(t1, t2));
        }
        println!();
    }
}
```


{{out}}

```txt
not
-------
 T  | F
 M  | M
 F  | T

and | T  M  F
-------------
 T  | T  M  F  
 M  | M  M  F  
 F  | F  F  F  

or  | T  M  F
-------------
 T  | T  T  T  
 M  | T  M  M  
 F  | T  M  F  

imp | T  M  F
-------------
 T  | T  M  F  
 M  | T  M  M  
 F  | T  T  T  

eqv | T  M  F
-------------
 T  | T  M  F  
 M  | M  M  M  
 F  | F  M  T  
```


=={{Header|Scala}}==

```scala
sealed trait Trit { self =>
  def nand(that:Trit):Trit=(this,that) match {
    case (TFalse, _) => TTrue
    case (_, TFalse) => TTrue
    case (TMaybe, _) => TMaybe
    case (_, TMaybe) => TMaybe
    case _ => TFalse
  }
		
  def nor(that:Trit):Trit = this.or(that).not()
  def and(that:Trit):Trit = this.nand(that).not()
  def or(that:Trit):Trit = this.not().nand(that.not())
  def not():Trit = this.nand(this)
  def imply(that:Trit):Trit = this.nand(that.not())
  def equiv(that:Trit):Trit = this.and(that).or(this.nor(that))
}
case object TTrue extends Trit
case object TMaybe extends Trit
case object TFalse extends Trit

object TernaryLogic extends App {
  val v=List(TTrue, TMaybe, TFalse)
  println("- NOT -")
  for(a<-v) println("%6s => %6s".format(a, a.not))
  println("\n- AND -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a and b))
  println("\n- OR -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a or b))
  println("\n- Imply -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a imply b))
  println("\n- Equiv -")
  for(a<-v; b<-v) println("%6s : %6s => %6s".format(a, b, a equiv b))		
}
```

{{out}}

```txt
- NOT -
 TTrue => TFalse
TMaybe => TMaybe
TFalse =>  TTrue

- AND -
 TTrue :  TTrue =>  TTrue
 TTrue : TMaybe => TMaybe
 TTrue : TFalse => TFalse
TMaybe :  TTrue => TMaybe
TMaybe : TMaybe => TMaybe
TMaybe : TFalse => TFalse
TFalse :  TTrue => TFalse
TFalse : TMaybe => TFalse
TFalse : TFalse => TFalse

- OR -
 TTrue :  TTrue =>  TTrue
 TTrue : TMaybe =>  TTrue
 TTrue : TFalse =>  TTrue
TMaybe :  TTrue =>  TTrue
TMaybe : TMaybe => TMaybe
TMaybe : TFalse => TMaybe
TFalse :  TTrue =>  TTrue
TFalse : TMaybe => TMaybe
TFalse : TFalse => TFalse

- Imply -
 TTrue :  TTrue =>  TTrue
 TTrue : TMaybe => TMaybe
 TTrue : TFalse => TFalse
TMaybe :  TTrue =>  TTrue
TMaybe : TMaybe => TMaybe
TMaybe : TFalse => TMaybe
TFalse :  TTrue =>  TTrue
TFalse : TMaybe =>  TTrue
TFalse : TFalse =>  TTrue

- Equiv -
 TTrue :  TTrue =>  TTrue
 TTrue : TMaybe => TMaybe
 TTrue : TFalse => TFalse
TMaybe :  TTrue => TMaybe
TMaybe : TMaybe => TMaybe
TMaybe : TFalse => TMaybe
TFalse :  TTrue => TFalse
TFalse : TMaybe => TMaybe
TFalse : TFalse =>  TTrue
```



## Seed7

The type [http://seed7.sourceforge.net/manual/types.htm#boolean boolean] does not define
separate '''xor''', '''implies''' and '''equiv''' operators. But there are replacements for them:
{| border="1"
! Instead of !! Use
|-
| p '''xor''' q || p <> q
|-
| p '''implies''' q || p <= q
|-
| p '''equiv''' q || p = q
|}
Since ternary logic needs '''xor''', '''implies''' and '''equiv''' with a ''trit'' result
they are introduced as the operators '''xor''', '''->''' and '''=='''.
The ''trit'' operators '''and''' and '''or''' are defined as short circuit operators.
A short circuit operator evaluates the second parameter only when necessary.
This is analogous to the ''boolean'' operators '''and''' and '''or''',
which use also short circuit evaluation.


```seed7
$ include "seed7_05.s7i";

const type: trit is new enum
    False, Maybe, True
  end enum;

# Enum types define comparisons (=, <, >, <=, >=, <>) and
# the conversions ord and conv.

const func string: str (in trit: aTrit) is
  return [] ("False", "Maybe", "True")[succ(ord(aTrit))];

enable_output(trit);  # Allow writing trit values

const array trit: tritNot is [] (True, Maybe, False);
const array array trit: tritAnd is [] (
    [] (False, False, False),
    [] (False, Maybe, Maybe),
    [] (False, Maybe, True ));
const array array trit: tritOr is [] (
    [] (False, Maybe, True ),
    [] (Maybe, Maybe, True ),
    [] (True,  True,  True ));
const array array trit: tritXor is [] (
    [] (False, Maybe, True ),
    [] (Maybe, Maybe, Maybe),
    [] (True,  Maybe, False));
const array array trit: tritImplies is [] (
    [] (True,  True,  True ),
    [] (Maybe, Maybe, True ),
    [] (False, Maybe, True ));
const array array trit: tritEquiv is [] (
    [] (True,  Maybe, False),
    [] (Maybe, Maybe, Maybe),
    [] (False, Maybe, True ));

const func trit: not (in trit: aTrit) is
  return tritNot[succ(ord(aTrit))];

const func trit: (in trit: aTrit1) and (in trit: aTrit2) is
  return tritAnd[succ(ord(aTrit1))][succ(ord(aTrit2))];
 
const func trit: (in trit: aTrit1) and (ref func trit: aTrit2) is func
  result
    var trit: res is False;
  begin
    if aTrit1 = True then
      res := aTrit2;
    elsif aTrit1 = Maybe and aTrit2 <> False then
      res := Maybe;
    end if;
  end func;

const func trit: (in trit: aTrit1) or (in trit: aTrit2) is
  return tritOr[succ(ord(aTrit1))][succ(ord(aTrit2))];
 
const func trit: (in trit: aTrit1) or (ref func trit: aTrit2) is func
  result
    var trit: res is True;
  begin
    if aTrit1 = False then
      res := aTrit2;
    elsif aTrit1 = Maybe and aTrit2 <> True then
      res := Maybe;
    end if;
  end func;

$ syntax expr: .().xor.() is -> 15;
const func trit: (in trit: aTrit1) xor (in trit: aTrit2) is
  return tritImplies[succ(ord(aTrit1))][succ(ord(aTrit2))];

const func trit: (in trit: aTrit1) -> (in trit: aTrit2) is
  return tritImplies[succ(ord(aTrit1))][succ(ord(aTrit2))];

const func trit: (in trit: aTrit1) == (in trit: aTrit2) is
  return tritEquiv[succ(ord(aTrit1))][succ(ord(aTrit2))];

const func trit: rand (in trit: low, in trit: high) is
  return trit conv (rand(ord(low), ord(high)));

# Begin of test code

var trit: operand1 is False;
var trit: operand2 is False;

const proc: writeTable (ref func trit: tritExpr, in string: name) is func
  begin
    writeln;
    writeln(" " <& name rpad 7 <& " | False  Maybe  True");
    writeln("---------+---------------------");
    for operand1 range False to True do
      write(" " <& operand1 rpad 7 <& " | ");
      for operand2 range False to True do
        write(tritExpr rpad 7);
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  begin
    writeln(" not" rpad 8 <& " | False  Maybe  True");
    writeln("---------+---------------------");
    write("         | ");
    for operand1 range False to True do
      write(not operand1 rpad 7);
    end for;
    writeln;
    writeTable(operand1 and operand2, "and");
    writeTable(operand1 or operand2,  "or");
    writeTable(operand1 xor operand2, "xor");
    writeTable(operand1 -> operand2,  "->");
    writeTable(operand1 == operand2,  "==");
  end func;
```


{{out}}

```txt

 not     | False  Maybe  True
---------+---------------------
         | True   Maybe  False  

 and     | False  Maybe  True
---------+---------------------
 False   | False  False  False  
 Maybe   | False  Maybe  Maybe  
 True    | False  Maybe  True   

 or      | False  Maybe  True
---------+---------------------
 False   | False  Maybe  True   
 Maybe   | Maybe  Maybe  True   
 True    | True   True   True   

 xor     | False  Maybe  True
---------+---------------------
 False   | True   True   True   
 Maybe   | Maybe  Maybe  True   
 True    | False  Maybe  True   

 ->      | False  Maybe  True
---------+---------------------
 False   | True   True   True   
 Maybe   | Maybe  Maybe  True   
 True    | False  Maybe  True   

 ==      | False  Maybe  True
---------+---------------------
 False   | True   Maybe  False  
 Maybe   | Maybe  Maybe  Maybe  
 True    | False  Maybe  True   

```



## Tcl

The simplest way of doing this is by constructing the operations as truth tables. The code below uses an abbreviated form of truth table.

```tcl
package require Tcl 8.5
namespace eval ternary {
    # Code generator
    proc maketable {name count values} {
	set sep ""
	for {set i 0; set c 97} {$i<$count} {incr i;incr c} {
	    set v [format "%c" $c]
	    lappend args $v; append key $sep "$" $v
	    set sep ","
	}
	foreach row [split $values \n] {
	    if {[llength $row]>1} {
		lassign $row from to
		lappend table $from [list return $to]
	    }
	}
	proc $name $args \
	    [list ckargs $args]\;[concat [list switch -glob --] $key [list $table]]
	namespace export $name
    }
    # Helper command to check argument syntax
    proc ckargs argList {
	foreach var $argList {
	    upvar 1 $var v
	    switch -exact -- $v {
		true - maybe - false {
		    continue
		}
		default {
		    return -level 2 -code error "bad ternary value \"$v\""
		}
	    }
	}
    }

    # The "truth" tables; “*” means “anything”
    maketable not 1 {
	true false
	maybe maybe
	false true
    }
    maketable and 2 {
	true,true true
	false,* false
	*,false false
	* maybe
    }
    maketable or 2 {
	true,* true
	*,true true
	false,false false
	* maybe
    }
    maketable implies 2 {
	false,* true
	*,true true
	true,false false
	* maybe
    }
    maketable equiv 2 {
	*,maybe maybe
	maybe,* maybe
	true,true true
	false,false true
	* false
    }
}
```

Demonstrating:

```tcl
namespace import ternary::*
puts "x /\\ y == x \\/ y"
puts " x     | y     || result"
puts "-------+-------++--------"
foreach x {true maybe false} {
    foreach y {true maybe false} {
	set z [equiv [and $x $y] [or $x $y]]
	puts [format " %-5s | %-5s || %-5s" $x $y $z]
    }
}
```

{{out}}

```txt

x /\ y == x \/ y
 x     | y     || result
-------+-------++--------
 true  | true  || true 
 true  | maybe || maybe
 true  | false || false
 maybe | true  || maybe
 maybe | maybe || maybe
 maybe | false || maybe
 false | true  || false
 false | maybe || maybe
 false | false || true 

```


{{omit from|GUISS}}
