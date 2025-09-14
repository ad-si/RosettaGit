+++
title = "Ackermann function"
description = ""
date = 2019-10-17T04:41:12Z
aliases = []
[extra]
id = 3050
[taxonomies]
categories = ["Recursion", "Memoization", "Classic CS problems and programs", "task"]
tags = []
languages = [
  "360_assembly",
  "68000_assembly",
  "8th",
  "abap",
  "actionscript",
  "ada",
  "agda",
  "algol_68",
  "apl",
  "applescript",
  "applesoft_basic",
  "argile",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "babel",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "bc",
  "bcpl",
  "beeswax",
  "befunge",
  "bracmat",
  "brat",
  "c",
  "chapel",
  "clay",
  "clips",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "component_pascal",
  "coq",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dart",
  "dc",
  "delphi",
  "dwscript",
  "dylan",
  "e",
  "easylang",
  "egel",
  "eiffel",
  "ela",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euler_math_toolbox",
  "euphoria",
  "ezhil",
  "factor",
  "falcon",
  "false",
  "fantom",
  "fbsl",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "funl",
  "futhark",
  "futurebasic",
  "gambas",
  "gap",
  "genyris",
  "gml",
  "gnuplot",
  "go",
  "golfscript",
  "groovy",
  "haskell",
  "haxe",
  "idris",
  "ioke",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "jsish",
  "julia",
  "k",
  "kdf9_usercode",
  "klong",
  "kotlin",
  "lasso",
  "lfe",
  "liberty_basic",
  "livecode",
  "logo",
  "logtalk",
  "lolcode",
  "lua",
  "lucid",
  "luck",
  "m2000_interpreter",
  "m4",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "mercury",
  "min",
  "miniscript",
  "ml_i",
  "mlite",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "nit",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ooc",
  "oorexx",
  "order",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "piet",
  "pike",
  "pl_i",
  "pl_sql",
  "postscript",
  "potion",
  "powerbasic",
  "powershell",
  "processing",
  "prolog",
  "pure",
  "pure_data",
  "purebasic",
  "purity",
  "python",
  "qbasic",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "setl",
  "shen",
  "sidef",
  "simula",
  "slate",
  "smalltalk",
  "smilebasic",
  "snobol4",
  "snusp",
  "spad",
  "sql_pl",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "torquescript",
  "tse_sal",
  "txr",
  "unix_shell",
  "ursala",
  "v",
  "vba",
  "vbscript",
  "visual_basic",
  "vlang",
  "wart",
  "wdte",
  "wren",
  "xlisp",
  "xpl0",
  "xslt",
  "yabasic",
  "yorick",
  "zed",
  "zx_spectrum_basic",
]
+++

## Task

The '''[[wp:Ackermann function|Ackermann function]]''' is a classic example of a recursive function, notable especially because it is not a [[wp:Primitive_recursive_function|primitive recursive function]]. It grows very quickly in value, as does the size of its call tree.


The Ackermann function is usually defined as follows:

<big>
:<math> A(m, n) =
 \begin{cases}
 n+1 & \mbox{if } m = 0 \\
 A(m-1, 1) & \mbox{if } m > 0 \mbox{ and } n = 0 \\
 A(m-1, A(m, n-1)) & \mbox{if } m > 0 \mbox{ and } n > 0.
 \end{cases}
</math>
</big>

<!-- <table><tr><td width=12><td><td><math>n+1</math><td>if <math>m=0</math> <tr><td> <td><math>A(m, n) =</math> <td><math>A(m-1, 1)</math> <td>if <math>m>0</math> and <math>n=0</math> <tr><td><td><td><math>A(m-1, A(m, n-1))</math>  <td> if <math>m>0</math> and <math>n>0</math></table> -->

Its arguments are never negative and it always terminates. Write a function which returns the value of <math>A(m, n)</math>. Arbitrary precision is preferred (since the function grows so quickly), but not required.


;See also:
* [[wp:Conway_chained_arrow_notation#Ackermann_function|Conway chained arrow notation]] for the Ackermann function.





## 360 Assembly

{{trans|AWK}}
The OS/360 linkage is a bit tricky with the S/360 basic instruction set.
To simplify, the program is recursive not reentrant.

```360asm
*        Ackermann function        07/09/2015
&LAB     XDECO &REG,&TARGET
.*-----------------------------------------------------------------*
.*       THIS MACRO DISPLAYS THE REGISTER CONTENTS AS A TRUE       *
.*       DECIMAL VALUE. XDECO IS NOT PART OF STANDARD S360 MACROS! *
*------------------------------------------------------------------*
         AIF   (T'&REG EQ 'O').NOREG
         AIF   (T'&TARGET EQ 'O').NODEST
&LAB     B     I&SYSNDX               BRANCH AROUND WORK AREA
W&SYSNDX DS    XL8                    CONVERSION WORK AREA
I&SYSNDX CVD   &REG,W&SYSNDX          CONVERT TO DECIMAL
         MVC   &TARGET,=XL12'402120202020202020202020'
         ED    &TARGET,W&SYSNDX+2     MAKE FIELD PRINTABLE
         BC    2,*+12                 BYPASS NEGATIVE
         MVI   &TARGET+12,C'-'        INSERT NEGATIVE SIGN
         B     *+8                    BYPASS POSITIVE
         MVI   &TARGET+12,C'+'        INSERT POSITIVE SIGN
         MEXIT
.NOREG   MNOTE 8,'INPUT REGISTER OMITTED'
         MEXIT
.NODEST  MNOTE 8,'TARGET FIELD OMITTED'
         MEXIT
         MEND
ACKERMAN CSECT
         USING  ACKERMAN,R12       r12 : base register
         LR     R12,R15            establish base register
         ST     R14,SAVER14A       save r14
         LA     R4,0               m=0
LOOPM    CH     R4,=H'3'           do m=0 to 3
         BH     ELOOPM
         LA     R5,0               n=0
LOOPN    CH     R5,=H'8'           do n=0 to 8
         BH     ELOOPN
         LR     R1,R4              m
         LR     R2,R5              n
         BAL    R14,ACKER          r1=acker(m,n)
         XDECO  R1,PG+19
         XDECO  R4,XD
         MVC    PG+10(2),XD+10
         XDECO  R5,XD
         MVC    PG+13(2),XD+10
         XPRNT  PG,44              print buffer
         LA     R5,1(R5)           n=n+1
         B      LOOPN
ELOOPN   LA     R4,1(R4)           m=m+1
         B      LOOPM
ELOOPM   L      R14,SAVER14A       restore r14
         BR     R14                return to caller
SAVER14A DS     F                  static save r14
PG       DC     CL44'Ackermann(xx,xx) = xxxxxxxxxxxx'
XD       DS     CL12
ACKER    CNOP   0,4                function r1=acker(r1,r2)
         LR     R3,R1              save argument r1 in r3
         LR     R9,R10             save stackptr (r10) in r9 temp
         LA     R1,STACKLEN        amount of storage required
         GETMAIN RU,LV=(R1)        allocate storage for stack
         USING  STACK,R10          make storage addressable
         LR     R10,R1             establish stack addressability
         ST     R14,SAVER14B       save previous r14
         ST     R9,SAVER10B        save previous r10
         LR     R1,R3              restore saved argument r1
START    ST     R1,M               stack m
         ST     R2,N               stack n
IF1      C      R1,=F'0'           if m<>0
         BNE    IF2                then goto if2
         LR     R11,R2             n
         LA     R11,1(R11)         return n+1
         B      EXIT
IF2      C      R2,=F'0'           else if m<>0
         BNE    IF3                then goto if3
         BCTR   R1,0               m=m-1
         LA     R2,1               n=1
         BAL    R14,ACKER          r1=acker(m)
         LR     R11,R1             return acker(m-1,1)
         B      EXIT
IF3      BCTR   R2,0               n=n-1
         BAL    R14,ACKER          r1=acker(m,n-1)
         LR     R2,R1              acker(m,n-1)
         L      R1,M               m
         BCTR   R1,0               m=m-1
         BAL    R14,ACKER          r1=acker(m-1,acker(m,n-1))
         LR     R11,R1             return acker(m-1,1)
EXIT     L      R14,SAVER14B       restore r14
         L      R9,SAVER10B        restore r10 temp
         LA     R0,STACKLEN        amount of storage to free
         FREEMAIN A=(R10),LV=(R0)  free allocated storage
         LR     R1,R11             value returned
         LR     R10,R9             restore r10
         BR     R14                return to caller
         LTORG
         DROP   R12                base no longer needed
STACK    DSECT                     dynamic area
SAVER14B DS     F                  saved r14
SAVER10B DS     F                  saved r10
M        DS     F                  m
N        DS     F                  n
STACKLEN EQU    *-STACK
         YREGS
         END    ACKERMAN
```

{{out}}
<pre style="height:20ex">
Ackermann( 0, 0) =            1
Ackermann( 0, 1) =            2
Ackermann( 0, 2) =            3
Ackermann( 0, 3) =            4
Ackermann( 0, 4) =            5
Ackermann( 0, 5) =            6
Ackermann( 0, 6) =            7
Ackermann( 0, 7) =            8
Ackermann( 0, 8) =            9
Ackermann( 1, 0) =            2
Ackermann( 1, 1) =            3
Ackermann( 1, 2) =            4
Ackermann( 1, 3) =            5
Ackermann( 1, 4) =            6
Ackermann( 1, 5) =            7
Ackermann( 1, 6) =            8
Ackermann( 1, 7) =            9
Ackermann( 1, 8) =           10
Ackermann( 2, 0) =            3
Ackermann( 2, 1) =            5
Ackermann( 2, 2) =            7
Ackermann( 2, 3) =            9
Ackermann( 2, 4) =           11
Ackermann( 2, 5) =           13
Ackermann( 2, 6) =           15
Ackermann( 2, 7) =           17
Ackermann( 2, 8) =           19
Ackermann( 3, 0) =            5
Ackermann( 3, 1) =           13
Ackermann( 3, 2) =           29
Ackermann( 3, 3) =           61
Ackermann( 3, 4) =          125
Ackermann( 3, 5) =          253
Ackermann( 3, 6) =          509
Ackermann( 3, 7) =         1021
Ackermann( 3, 8) =         2045

```



## 68000 Assembly

This implementation is based on the code shown in the computerphile episode in the youtube link at the top of this page (time index 5:00).


```68000devpac
;
; Ackermann function for Motorola 68000 under AmigaOs 2+ by Thorham
;
; Set stack space to 60000 for m = 3, n = 5.
;
; The program will print the ackermann values for the range m = 0..3, n = 0..5
;
_LVOOpenLibrary equ -552
_LVOCloseLibrary equ -414
_LVOVPrintf equ -954

m equ 3 ; Nr of iterations for the main loop.
n equ 5 ; Do NOT set them higher, or it will take hours to complete on
        ; 68k, not to mention that the stack usage will become astronomical.
        ; Perhaps n can be a little higher... If you do increase the ranges
        ; then don't forget to increase the stack size.

execBase=4

start
    move.l  execBase,a6

    lea     dosName,a1
    moveq   #36,d0
    jsr     _LVOOpenLibrary(a6)
    move.l  d0,dosBase
    beq     exit

    move.l  dosBase,a6
    lea     printfArgs,a2

    clr.l   d3 ; m
.loopn
    clr.l   d4 ; n
.loopm
    bsr     ackermann

    move.l  d3,0(a2)
    move.l  d4,4(a2)
    move.l  d5,8(a2)
    move.l  #outString,d1
    move.l  a2,d2
    jsr     _LVOVPrintf(a6)

    addq.l  #1,d4
    cmp.l   #n,d4
    ble     .loopm

    addq.l  #1,d3
    cmp.l   #m,d3
    ble     .loopn

exit
    move.l  execBase,a6
    move.l  dosBase,a1
    jsr     _LVOCloseLibrary(a6)
    rts
;
; ackermann function
;
; in:
;
; d3 = m
; d4 = n
;
; out:
;
; d5 = ans
;
ackermann
    move.l  d3,-(sp)
    move.l  d4,-(sp)

    tst.l   d3
    bne     .l1
    move.l  d4,d5
    addq.l  #1,d5
    bra     .return
.l1
    tst.l   d4
    bne     .l2
    subq.l  #1,d3
    moveq   #1,d4
    bsr     ackermann
    bra     .return
.l2
    subq.l  #1,d4
    bsr     ackermann
    move.l  d5,d4
    subq.l  #1,d3
    bsr     ackermann

.return
    move.l  (sp)+,d4
    move.l  (sp)+,d3
    rts
;
; variables
;
dosBase
    dc.l    0

printfArgs
    dcb.l   3
;
; strings
;
dosName
    dc.b    "dos.library",0

outString
    dc.b    "ackermann (%ld,%ld) is: %ld",10,0
```



## 8th


```Forth

\ Ackermann function, illustrating use of "memoization".

\ Memoization is a technique whereby intermediate computed values are stored
\ away against later need.  It is particularly valuable when calculating those
\ values is time or resource intensive, as with the Ackermann function.

\ make the stack much bigger so this can complete!
100000 stack-size

\ This is where memoized values are stored:
{} var, dict

\ Simple accessor words
: dict! \ "key" val --
  dict @ -rot m:! drop ;

: dict@ \ "key" -- val
  dict @ swap m:@ nip ;

defer: ack1

\ We just jam the string representation of the two numbers together for a key:
: makeKey  \ m n -- m n key
	2dup >s swap >s s:+ ;

: ack2 \ m n -- A
	makeKey dup
	dict@ null?
	if \ can't find key in dict
		\ m n key null
		drop \ m n key
		-rot \ key m n
		ack1 \ key A
		tuck \ A key A
		dict! \ A
	else \ found value
		\ m n key value
		>r drop 2drop r>
	then ;

: ack \ m n -- A
	over not
	if
		nip n:1+
	else
		dup not
		if
			drop n:1- 1 ack2
		else
			over swap n:1- ack2
			swap n:1- swap ack2
		then
	then ;

' ack is ack1

: ackOf \ m n --
        2dup
        "Ack(" . swap . ", " . . ") = " . ack . cr ;


0 0 ackOf
0 4 ackOf
1 0 ackOf
1 1 ackOf
2 1 ackOf
2 2 ackOf
3 1 ackOf
3 3 ackOf
4 0 ackOf

\ this last requires a very large data stack.  So start 8th with a parameter '-k 100000'
4 1 ackOf

bye
```


{{out|The output}}

```txt

Ack(0, 0) = 1
Ack(0, 4) = 5
Ack(1, 0) = 2
Ack(1, 1) = 3
Ack(2, 1) = 5
Ack(2, 2) = 7
Ack(3, 1) = 13
Ack(3, 3) = 61
Ack(4, 0) = 13
Ack(4, 1) = 65533

```



## ABAP



```ABAP

REPORT  zhuberv_ackermann.

CLASS zcl_ackermann DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS ackermann IMPORTING m TYPE i
                                      n TYPE i
                            RETURNING value(v) TYPE i.
ENDCLASS.            "zcl_ackermann DEFINITION


CLASS zcl_ackermann IMPLEMENTATION.

  METHOD: ackermann.

    DATA: lv_new_m TYPE i,
          lv_new_n TYPE i.

    IF m = 0.
      v = n + 1.
    ELSEIF m > 0 AND n = 0.
      lv_new_m = m - 1.
      lv_new_n = 1.
      v = ackermann( m = lv_new_m n = lv_new_n ).
    ELSEIF m > 0 AND n > 0.
      lv_new_m = m - 1.

      lv_new_n = n - 1.
      lv_new_n = ackermann( m = m n = lv_new_n ).

      v = ackermann( m = lv_new_m n = lv_new_n ).
    ENDIF.

  ENDMETHOD.                    "ackermann

ENDCLASS.                    "zcl_ackermann IMPLEMENTATION


PARAMETERS: pa_m TYPE i,
            pa_n TYPE i.

DATA: lv_result TYPE i.

START-OF-SELECTION.
  lv_result = zcl_ackermann=>ackermann( m = pa_m n = pa_n ).
  WRITE: / lv_result.

```



## ActionScript


```actionscript
public function ackermann(m:uint, n:uint):uint
{
    if (m == 0)
    {
        return n + 1;
    }
    if (n == 0)
    {
        return ackermann(m - 1, 1);
    }

    return ackermann(m - 1, ackermann(m, n - 1));
}
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Ackermann is
   function Ackermann (M, N : Natural) return Natural is
   begin
      if M = 0 then
         return N + 1;
      elsif N = 0 then
         return Ackermann (M - 1, 1);
      else
         return Ackermann (M - 1, Ackermann (M, N - 1));
      end if;
   end Ackermann;
begin
   for M in 0..3 loop
      for N in 0..6 loop
         Put (Natural'Image (Ackermann (M, N)));
      end loop;
      New_Line;
   end loop;
end Test_Ackermann;
```

The implementation does not care about arbitrary precision numbers because the Ackermann function does not only grow, but also slow quickly, when computed recursively.
{{out}} the first 4x7 Ackermann's numbers:

```txt
 1 2 3 4 5 6 7
 2 3 4 5 6 7 8
 3 5 7 9 11 13 15
 5 13 29 61 125 253 509
```



## Agda

{{works with|Agda|2.5.2}}
{{libheader|agda-stdlib v0.13}}

```agda

open import Data.Nat
open import Data.Nat.Show
open import IO

module Ackermann where

ack : ℕ -> ℕ -> ℕ
ack zero n = n + 1
ack (suc m) zero = ack m 1
ack (suc m) (suc n) = ack m (ack (suc m) n)

main = run (putStrLn (show (ack 3 9)))

```


Note the unicode ℕ characters, they can be input in emacs agda mode using "\bN". Running in bash:


```bash

agda --compile Ackermann.agda
./Ackermann

```


{{out}}

```txt

4093

```



## ALGOL 68

{{trans|Ada}}
{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
PROC test ackermann = VOID:
BEGIN
   PROC ackermann = (INT m, n)INT:
   BEGIN
      IF m = 0 THEN
         n + 1
      ELIF n = 0 THEN
         ackermann (m - 1, 1)
      ELSE
         ackermann (m - 1, ackermann (m, n - 1))
      FI
   END # ackermann #;

   FOR m FROM 0 TO 3 DO
      FOR n FROM 0 TO 6 DO
         print(ackermann (m, n))
      OD;
      new line(stand out)
   OD
END # test ackermann #;
test ackermann
```

{{out}}

```txt

         +1         +2         +3         +4         +5         +6         +7
         +2         +3         +4         +5         +6         +7         +8
         +3         +5         +7         +9        +11        +13        +15
         +5        +13        +29        +61       +125       +253       +509

```



## APL

{{works with|Dyalog APL}}

```APL
ackermann←{
     0=1⊃⍵:1+2⊃⍵
     0=2⊃⍵:∇(¯1+1⊃⍵)1
     ∇(¯1+1⊃⍵),∇(1⊃⍵),¯1+2⊃⍵
 }
```



## AppleScript



```AppleScript
on ackermann(m, n)
    if m is equal to 0 then return n + 1
    if n is equal to 0 then return ackermann(m - 1, 1)
    return ackermann(m - 1, ackermann(m, n - 1))
end ackermann
```



## Argile

{{works with|Argile|1.0.0}}

```Argile
use std

for each (val nat n) from 0 to 6
  for each (val nat m) from 0 to 3
    print "A("m","n") = "(A m n)

.:A <nat m, nat n>:. -> nat
  return (n+1)				if m == 0
  return (A (m - 1) 1)			if n == 0
  A (m - 1) (A m (n - 1))
```



## ATS


```ATS
fun ackermann
  {m,n:nat} .<m,n>.
  (m: int m, n: int n): Nat =
  case+ (m, n) of
  | (0, _) => n+1
  | (_, 0) =>> ackermann (m-1, 1)
  | (_, _) =>> ackermann (m-1, ackermann (m, n-1))
// end of [ackermann]
```



## AutoHotkey


```AutoHotkey
A(m, n) {
  If (m > 0) && (n = 0)
    Return A(m-1,1)
  Else If (m > 0) && (n > 0)
    Return A(m-1,A(m, n-1))
  Else If (m=0)
    Return n+1
}

; Example:
MsgBox, % "A(1,2) = " A(1,2)
```



## AutoIt


###  Classical version


```autoit
Func Ackermann($m, $n)
    If ($m = 0) Then
        Return $n+1
    Else
        If ($n = 0) Then
            Return Ackermann($m-1, 1)
        Else
            return Ackermann($m-1, Ackermann($m, $n-1))
        EndIf
    EndIf
EndFunc
```



###  Classical + cache implementation


This version works way faster than the classical one: Ackermann(3, 5) runs in 12,7 ms, while the classical version takes 402,2 ms.


```autoit
Global $ackermann[2047][2047] ; Set the size to whatever you want
Func Ackermann($m, $n)
	If ($ackermann[$m][$n] <> 0) Then
		Return $ackermann[$m][$n]
	Else
		If ($m = 0) Then
			$return = $n + 1
		Else
			If ($n = 0) Then
				$return = Ackermann($m - 1, 1)
			Else
				$return = Ackermann($m - 1, Ackermann($m, $n - 1))
			EndIf
		EndIf
		$ackermann[$m][$n] = $return
		Return $return
	EndIf
EndFunc   ;==>Ackermann
```



## AWK


```awk
function ackermann(m, n)
{
  if ( m == 0 ) {
    return n+1
  }
  if ( n == 0 ) {
    return ackermann(m-1, 1)
  }
  return ackermann(m-1, ackermann(m, n-1))
}

BEGIN {
  for(n=0; n < 7; n++) {
    for(m=0; m < 4; m++) {
      print "A(" m "," n ") = " ackermann(m,n)
    }
  }
}
```



## Babel


```babel
main:
        {((0 0) (0 1) (0 2)
        (0 3) (0 4) (1 0)
        (1 1) (1 2) (1 3)
        (1 4) (2 0) (2 1)
        (2 2) (2 3) (3 0)
        (3 1) (3 2) (4 0))

        { dup
        "A(" << { %d " " . << } ... ") = " <<
    reverse give
    ack
    %d cr << } ... }

ack!:
    { dup zero?
        { <-> dup zero?
            { <->
                cp
                1 -
                <- <- 1 - ->
                ack ->
                ack }
            { <->
                1 -
                <- 1 ->
                ack }
        if }
        { zap 1 + }
    if }

zero?!: { 0 = }

```

{{out}}

```txt

A(0 0 ) = 1
A(0 1 ) = 2
A(0 2 ) = 3
A(0 3 ) = 4
A(0 4 ) = 5
A(1 0 ) = 2
A(1 1 ) = 3
A(1 2 ) = 4
A(1 3 ) = 5
A(1 4 ) = 6
A(2 0 ) = 3
A(2 1 ) = 5
A(2 2 ) = 7
A(2 3 ) = 9
A(3 0 ) = 5
A(3 1 ) = 13
A(3 2 ) = 29
A(4 0 ) = 13
```



## BASIC

=
## Applesoft BASIC
=

```basic
100 DIM R%(2900),M(2900),N(2900)
110 FOR M = 0 TO 3
120     FOR N = 0 TO 4
130         GOSUB 200"ACKERMANN
140         PRINT "ACK("M","N") = "ACK
150 NEXT N, M
160 END

200 M(SP) = M
210 N(SP) = N

REM A(M - 1, A(M, N - 1))
220 IF M > 0 AND N > 0 THEN N = N - 1 : R%(SP) = 0 : SP = SP + 1 : GOTO 200

REM A(M - 1, 1)
230 IF M > 0 THEN M = M - 1 : N = 1 : R%(SP) = 1 : SP = SP + 1 : GOTO 200

REM N + 1
240 ACK = N + 1

REM RETURN
250 M = M(SP) : N = N(SP) : IF SP = 0 THEN RETURN
260 FOR SP = SP - 1 TO 0 STEP -1 : IF R%(SP) THEN M = M(SP) : N = N(SP) : NEXT SP : SP = 0 : RETURN
270 M = M - 1 : N = ACK : R%(SP) = 1 : SP = SP + 1 : GOTO 200
```


=
## BASIC256
=

```basic256
dim stack(5000, 3)	# BASIC-256 lacks functions (as of ver. 0.9.6.66)
stack[0,0] = 3		# M
stack[0,1] = 7		# N
lev = 0

gosub ackermann
print "A("+stack[0,0]+","+stack[0,1]+") = "+stack[0,2]
end

ackermann:
	if stack[lev,0]=0 then
		stack[lev,2] = stack[lev,1]+1
		return
	end if
	if stack[lev,1]=0 then
		lev = lev+1
		stack[lev,0] = stack[lev-1,0]-1
		stack[lev,1] = 1
		gosub ackermann
		stack[lev-1,2] = stack[lev,2]
		lev = lev-1
		return
	end if
	lev = lev+1
	stack[lev,0] = stack[lev-1,0]
	stack[lev,1] = stack[lev-1,1]-1
	gosub ackermann
	stack[lev,0] = stack[lev-1,0]-1
	stack[lev,1] = stack[lev,2]
	gosub ackermann
	stack[lev-1,2] = stack[lev,2]
	lev = lev-1
	return
```

{{out}}

```txt

 A(3,7) = 1021

```



```basic256
# BASIC256 since 0.9.9.1 supports functions
for m = 0 to 3
   for n = 0 to 4
      print m + " " + n + " " + ackermann(m,n)
   next n
next m
end

function ackermann(m,n)
   if m = 0 then
      ackermann = n+1
   else
      if n = 0 then
         ackermann = ackermann(m-1,1)
      else
         ackermann = ackermann(m-1,ackermann(m,n-1))
      endif
   end if
end function
```


{{out}}

```txt

0 0 1
0 1 2
0 2 3
0 3 4
0 4 5
1 0 2
1 1 3
1 2 4
1 3 5
1 4 6
2 0 3
2 1 5
2 2 7
2 3 9
2 4 11
3 0 5
3 1 13
3 2 29
3 3 61
3 4 125
```


=
## BBC BASIC
=

```bbcbasic
      PRINT FNackermann(3, 7)
      END

      DEF FNackermann(M%, N%)
      IF M% = 0 THEN = N% + 1
      IF N% = 0 THEN = FNackermann(M% - 1, 1)
      = FNackermann(M% - 1, FNackermann(M%, N%-1))
```


=
## QuickBasic
=
{{works with|QuickBasic|4.5}}
BASIC runs out of stack space very quickly.
The call <tt>ack(3, 4)</tt> gives a stack error.

```qbasic
DECLARE FUNCTION ack! (m!, n!)

FUNCTION ack (m!, n!)
       IF m = 0 THEN ack = n + 1

       IF m > 0 AND n = 0 THEN
               ack = ack(m - 1, 1)
       END IF
       IF m > 0 AND n > 0 THEN
               ack = ack(m - 1, ack(m, n - 1))
       END IF
END FUNCTION
```



## Batch File

Had trouble with this, so called in the gurus at [http://stackoverflow.com/questions/2680668/what-is-wrong-with-this-recursive-windows-cmd-script-it-wont-do-ackermann-prope StackOverflow]. Thanks to Patrick Cuff for pointing out where I was going wrong.

```dos
::Ackermann.cmd
@echo off
set depth=0
:ack
if %1==0 goto m0
if %2==0 goto n0

:else
set /a n=%2-1
set /a depth+=1
call :ack %1 %n%
set t=%errorlevel%
set /a depth-=1
set /a m=%1-1
set /a depth+=1
call :ack %m% %t%
set t=%errorlevel%
set /a depth-=1
if %depth%==0 ( exit %t% ) else ( exit /b %t% )

:m0
set/a n=%2+1
if %depth%==0 ( exit %n% ) else ( exit /b %n% )

:n0
set /a m=%1-1
set /a depth+=1
call :ack %m% 1
set t=%errorlevel%
set /a depth-=1
if %depth%==0 ( exit %t% ) else ( exit /b %t% )
```

Because of the <code>exit</code> statements, running this bare closes one's shell, so this test routine handles the calling of Ackermann.cmd

```dos
::Ack.cmd
@echo off
cmd/c ackermann.cmd %1 %2
echo Ackermann(%1, %2)=%errorlevel%
```

A few test runs:

```txt
D:\Documents and Settings\Bruce>ack 0 4
Ackermann(0, 4)=5

D:\Documents and Settings\Bruce>ack 1 4
Ackermann(1, 4)=6

D:\Documents and Settings\Bruce>ack 2 4
Ackermann(2, 4)=11

D:\Documents and Settings\Bruce>ack 3 4
Ackermann(3, 4)=125
```



## bc

Requires a <tt>bc</tt> that supports long names and the <tt>print</tt> statement.
{{works with|OpenBSD bc}}
{{Works with|GNU bc}}

```bc
define ack(m, n) {
   if ( m == 0 ) return (n+1);
   if ( n == 0 ) return (ack(m-1, 1));
   return (ack(m-1, ack(m, n-1)));
}

for (n=0; n<7; n++) {
  for (m=0; m<4; m++) {
     print "A(", m, ",", n, ") = ", ack(m,n), "\n";
  }
}
quit
```



## BCPL


```BCPL
GET "libhdr"

LET ack(m, n) = m=0 -> n+1,
                n=0 -> ack(m-1, 1),
                ack(m-1, ack(m, n-1))

LET start() = VALOF
{ FOR i = 0 TO 6 FOR m = 0 TO 3 DO
    writef("ack(%n, %n) = %n*n", m, n, ack(m,n))
  RESULTIS 0
}
```



## beeswax


Iterative slow version:


```beeswax

                         >M?f@h@gMf@h3yzp            if m>0 and n>0 => replace m,n with m-1,m,n-1
                    >@h@g'b?1f@h@gM?f@hzp            if m>0 and n=0 => replace m,n with m-1,1
_ii>Ag~1?~Lpz1~2h@g'd?g?Pfzp                         if m=0         => replace m,n with n+1
           >I;
     b                     <            <

```


A functional and recursive realization of the version above. Functions are realized by direct calls of functions via jumps (instruction <code>J</code>) to the entry points of two distinct functions:

1st function <code>_ii</code> (input function) with entry point at (row,col) = (4,1)

2nd function <code>Ag~1....</code> (Ackermann function) with entry point at (row,col) = (1,1)

Each block of <code>1FJ</code> or <code>1fFJ</code> in the code is a call of the Ackermann function itself.


```beeswax
Ag~1?Lp1~2@g'p?g?Pf1FJ                               Ackermann function.  if m=0 => run Ackermann function (m, n+1)
      xI;    x@g'p??@Mf1fFJ                                               if m>0 and n=0 => run Ackermann (m-1,1)
                 xM@~gM??f~f@f1FJ                                         if m>0 and n>0 => run Ackermann(m,Ackermann(m-1,n-1))
_ii1FJ                                               input function. Input m,n, then execute Ackermann(m,n)
```


Highly optimized and fast version, returns A(4,1)/A(5,0) almost instantaneously:

```beeswax

                             >Mf@Ph?@g@2h@Mf@Php     if m>4 and n>0 => replace m,n with m-1,m,n-1
                 >~4~L#1~2hg'd?1f@hgM?f2h      p     if m>4 and n=0 => replace m,n with m-1,1
                #            q      <                                                   /n+3 times  \
                #X~4K#?2Fg?PPP>@B@M"pb               if m=4         => replace m,n with 2^(2^(....)))-3
                 # >~3K#?g?PPP~2BMMp>@MMMp           if m=3         => replace m,n with 2^(n+3)-3
_ii>Ag~1?~Lpz1~2h@gX'#?g?P      p  M                 if m=0         => replace m,n with n+1
    z      I       ~>~1K#?g?PP  p                    if m=1         => replace m,n with n+2
     f     ;       >2K#?g?~2.PPPp                    if m=2         => replace m,n with 2n+3
   z  b                         <  <     <
   d                                           <

```


Higher values than A(4,1)/(5,0) lead to UInt64 wraparound, support for numbers bigger than 2^64-1 is not implemented in these solutions.


## Befunge


===Befunge-93===
{{trans|ERRE}}
Since Befunge-93 doesn't have recursive capabilities we need to use an iterative algorithm.

```befunge
&>&>vvg0>#0\#-:#1_1v
@v:\<vp0    0:-1<\+<
^>00p>:#^_$1+\:#^_$.

```


===Befunge-98===
{{works with|CCBI|2.1}}

```befunge
r[1&&{0
>v
 j
u>.@
1>  \:v
^  v:\_$1+
\^v_$1\1-
u^>1-0fp:1-\0fg101-
```

The program reads two integers (first m, then n) from command line, idles around funge space, then outputs the result of the Ackerman function.
Since the latter is calculated truly recursively, the execution time becomes unwieldy for most m>3.


## Bracmat

Three solutions are presented here.
The first one is a purely recursive version, only using the formulas at the top of the page.
The value of A(4,1) cannot be computed due to stack overflow.
It can compute A(3,9) (4093), but probably not A(3,10)

```bracmat
( Ack
=   m n
  .   !arg:(?m,?n)
    & ( !m:0&!n+1
      | !n:0&Ack$(!m+-1,1)
      | Ack$(!m+-1,Ack$(!m,!n+-1))
      )
);
```

The second version is a purely non-recursive solution that easily can compute A(4,1).
The program uses a stack for Ackermann function calls that are to be evaluated, but that cannot be computed given the currently known function values - the "known unknowns".
The currently known values are stored in a hash table.
The Hash table also contains incomplete Ackermann function calls, namely those for which the second argument is not known yet - "the unknown unknowns".
These function calls are associated with "known unknowns" that are going to provide the value of the second argument. As soon as such an associated known unknown becomes known, the unknown unknown becomes a known unknown and is pushed onto the stack.

Although all known values are stored in the hash table, the converse is not true: an element in the hash table is either a "known known" or an "unknown unknown" associated with an "known unknown".

```bracmat
  ( A
  =     m n value key eq chain
      , find insert future stack va val
    .   ( chain
        =   key future skey
          .   !arg:(?key.?future)
            & str$!key:?skey
            & (cache..insert)$(!skey..!future)
            &
        )
      & (find=.(cache..find)$(str$!arg))
      & ( insert
        =   key value future v futureeq futurem skey
          .   !arg:(?key.?value)
            & str$!key:?skey
            & (   (cache..find)$!skey:(?key.?v.?future)
                & (cache..remove)$!skey
                & (cache..insert)$(!skey.!value.)
                & (   !future:(?futurem.?futureeq)
                    & (!futurem,!value.!futureeq)
                  |
                  )
              | (cache..insert)$(!skey.!value.)&
              )
        )
      & !arg:(?m,?n)
      & !n+1:?value
      & :?eq:?stack
      &   whl
        ' ( (!m,!n):?key
          &     (   find$!key:(?.#%?value.?future)
                  & insert$(!eq.!value) !future
                |   !m:0
                  & !n+1:?value
                  & ( !eq:&insert$(!key.!value)
                    |   insert$(!key.!value) !stack:?stack
                      & insert$(!eq.!value)
                    )
                |   !n:0
                  &   (!m+-1,1.!key)
                      (!eq:|(!key.!eq))
                |   find$(!m,!n+-1):(?.?val.?)
                  & (   !val:#%
                      & (   find$(!m+-1,!val):(?.?va.?)
                          & !va:#%
                          & insert$(!key.!va)
                        |   (!m+-1,!val.!eq)
                            (!m,!n.!eq)
                        )
                    |
                    )
                |   chain$(!m,!n+-1.!m+-1.!key)
                  &   (!m,!n+-1.)
                      (!eq:|(!key.!eq))
                )
                !stack
            : (?m,?n.?eq) ?stack
          )
      & !value
  )
& new$hash:?cache
```

{{out|Some results}}

```txt

A$(0,0):1
A$(3,13):65533
A$(3,14):131069
A$(4,1):65533

```

The last solution is a recursive solution that employs some extra formulas, inspired by the Common Lisp solution further down.

```bracmat
( AckFormula
=   m n
  .   !arg:(?m,?n)
    & ( !m:0&!n+1
      | !m:1&!n+2
      | !m:2&2*!n+3
      | !m:3&2^(!n+3)+-3
      | !n:0&AckFormula$(!m+-1,1)
      | AckFormula$(!m+-1,AckFormula$(!m,!n+-1))
      )
)
```

{{Out|Some results}}

```txt
AckFormula$(4,1):65533
AckFormula$(4,2):2003529930406846464979072351560255750447825475569751419265016973.....22087777506072339445587895905719156733

```

The last computation costs about 0,03 seconds.


## Brat


```brat
ackermann = { m, n |
	when { m == 0 } { n + 1 }
		{ m > 0 && n == 0 } { ackermann(m - 1, 1) }
		{ m > 0 && n > 0 } { ackermann(m - 1, ackermann(m, n - 1)) }
}

p ackermann 3, 4  #Prints 125
```



## C

Straightforward implementation per Ackermann definition:

```c
#include <stdio.h>

int ackermann(int m, int n)
{
        if (!m) return n + 1;
        if (!n) return ackermann(m - 1, 1);
        return ackermann(m - 1, ackermann(m, n - 1));
}

int main()
{
        int m, n;
        for (m = 0; m <= 4; m++)
                for (n = 0; n < 6 - m; n++)
                        printf("A(%d, %d) = %d\n", m, n, ackermann(m, n));

        return 0;
}
```

{{out}}

```txt
A(0, 0) = 1
A(0, 1) = 2
A(0, 2) = 3
A(0, 3) = 4
A(0, 4) = 5
A(0, 5) = 6
A(1, 0) = 2
A(1, 1) = 3
A(1, 2) = 4
A(1, 3) = 5
A(1, 4) = 6
A(2, 0) = 3
A(2, 1) = 5
A(2, 2) = 7
A(2, 3) = 9
A(3, 0) = 5
A(3, 1) = 13
A(3, 2) = 29
A(4, 0) = 13
A(4, 1) = 65533
```

Ackermann function makes <i>a lot</i> of recursive calls, so the above program is a bit naive.  We need to be slightly less naive, by doing some simple caching:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int m_bits, n_bits;
int *cache;

int ackermann(int m, int n)
{
        int idx, res;
        if (!m) return n + 1;

        if (n >= 1<<n_bits) {
                printf("%d, %d\n", m, n);
                idx = 0;
        } else {
                idx = (m << n_bits) + n;
                if (cache[idx]) return cache[idx];
        }

        if (!n) res = ackermann(m - 1, 1);
        else    res = ackermann(m - 1, ackermann(m, n - 1));

        if (idx) cache[idx] = res;
        return res;
}
int main()
{
        int m, n;

        m_bits = 3;
        n_bits = 20;  /* can save n values up to 2**20 - 1, that's 1 meg */
        cache = malloc(sizeof(int) * (1 << (m_bits + n_bits)));
        memset(cache, 0, sizeof(int) * (1 << (m_bits + n_bits)));

        for (m = 0; m <= 4; m++)
                for (n = 0; n < 6 - m; n++)
                        printf("A(%d, %d) = %d\n", m, n, ackermann(m, n));

        return 0;
}
```

{{out}}

```txt
A(0, 0) = 1
A(0, 1) = 2
A(0, 2) = 3
A(0, 3) = 4
A(0, 4) = 5
A(0, 5) = 6
A(1, 0) = 2
A(1, 1) = 3
A(1, 2) = 4
A(1, 3) = 5
A(1, 4) = 6
A(2, 0) = 3
A(2, 1) = 5
A(2, 2) = 7
A(2, 3) = 9
A(3, 0) = 5
A(3, 1) = 13
A(3, 2) = 29
A(4, 0) = 13
A(4, 1) = 65533
```

Whee.  Well, with some extra work, we calculated <i>one more</i> n value, big deal, right?
But see, <code>A(4, 2) = A(3, A(4, 1)) = A(3, 65533) = A(2, A(3, 65532)) = ...</code> you can see how fast it blows up.  In fact, no amount of caching will help you calculate large m values;  on the machine I use A(4, 2) segfaults because the recursions run out of stack space--not a whole lot I can do about it.  At least it runs out of stack space <i>quickly</i>, unlike the first solution...


## C#



### Basic Version


```c#
using System;
class Program
{
    public static long Ackermann(long m, long n)
    {
        if(m > 0)
        {
            if (n > 0)
                return Ackermann(m - 1, Ackermann(m, n - 1));
            else if (n == 0)
                return Ackermann(m - 1, 1);
        }
        else if(m == 0)
        {
            if(n >= 0)
                return n + 1;
        }

        throw new System.ArgumentOutOfRangeException();
    }

    static void Main()
    {
        for (long m = 0; m <= 3; ++m)
        {
            for (long n = 0; n <= 4; ++n)
            {
                Console.WriteLine("Ackermann({0}, {1}) = {2}", m, n, Ackermann(m, n));
            }
        }
    }
}
```

{{out}}

```txt

Ackermann(0, 0) = 1
Ackermann(0, 1) = 2
Ackermann(0, 2) = 3
Ackermann(0, 3) = 4
Ackermann(0, 4) = 5
Ackermann(1, 0) = 2
Ackermann(1, 1) = 3
Ackermann(1, 2) = 4
Ackermann(1, 3) = 5
Ackermann(1, 4) = 6
Ackermann(2, 0) = 3
Ackermann(2, 1) = 5
Ackermann(2, 2) = 7
Ackermann(2, 3) = 9
Ackermann(2, 4) = 11
Ackermann(3, 0) = 5
Ackermann(3, 1) = 13
Ackermann(3, 2) = 29
Ackermann(3, 3) = 61
Ackermann(3, 4) = 125

```



### Efficient Version


```c#

using System;
using System.Numerics;
using System.IO;
using System.Diagnostics;

namespace Ackermann_Function
{
    class Program
    {
        static void Main(string[] args)
        {
            int _m = 0;
            int _n = 0;
            Console.Write("m = ");
            try
            {
                _m = Convert.ToInt32(Console.ReadLine());
            }
            catch (Exception)
            {
                Console.WriteLine("Please enter a number.");
            }
            Console.Write("n = ");
            try
            {
                _n = Convert.ToInt32(Console.ReadLine());
            }
            catch (Exception)
            {
                Console.WriteLine("Please enter a number.");
            }
            //for (long m = 0; m <= 10; ++m)
            //{
            //    for (long n = 0; n <= 10; ++n)
            //    {
            //        DateTime now = DateTime.Now;
            //        Console.WriteLine("Ackermann({0}, {1}) = {2}", m, n, Ackermann(m, n));
            //        Console.WriteLine("Time taken:{0}", DateTime.Now - now);
            //    }
            //}

            DateTime now = DateTime.Now;
            Console.WriteLine("Ackermann({0}, {1}) = {2}", _m, _n, Ackermann(_m, _n));
            Console.WriteLine("Time taken:{0}", DateTime.Now - now);
            File.WriteAllText("number.txt", Ackermann(_m, _n).ToString());
            Process.Start("number.txt");
            Console.ReadKey();
        }
        public class OverflowlessStack<T>
        {
            internal sealed class SinglyLinkedNode
            {
                private const int ArraySize = 2048;
                T[] _array;
                int _size;
                public SinglyLinkedNode Next;
                public SinglyLinkedNode()
                {
                    _array = new T[ArraySize];
                }
                public bool IsEmpty { get { return _size == 0; } }
                public SinglyLinkedNode Push(T item)
                {
                    if (_size == ArraySize - 1)
                    {
                        SinglyLinkedNode n = new SinglyLinkedNode();
                        n.Next = this;
                        n.Push(item);
                        return n;
                    }
                    _array[_size++] = item;
                    return this;
                }
                public T Pop()
                {
                    return _array[--_size];
                }
            }
            private SinglyLinkedNode _head = new SinglyLinkedNode();

            public T Pop()
            {
                T ret = _head.Pop();
                if (_head.IsEmpty && _head.Next != null)
                    _head = _head.Next;
                return ret;
            }
            public void Push(T item)
            {
                _head = _head.Push(item);
            }
            public bool IsEmpty
            {
                get { return _head.Next == null && _head.IsEmpty; }
            }
        }
        public static BigInteger Ackermann(BigInteger m, BigInteger n)
        {
            var stack = new OverflowlessStack<BigInteger>();
            stack.Push(m);
            while (!stack.IsEmpty)
            {
                m = stack.Pop();
            skipStack:
                if (m == 0)
                    n = n + 1;
                else if (m == 1)
                    n = n + 2;
                else if (m == 2)
                    n = n * 2 + 3;
                else if (n == 0)
                {
                    --m;
                    n = 1;
                    goto skipStack;
                }
                else
                {
                    stack.Push(m - 1);
                    --n;
                    goto skipStack;
                }
            }
            return n;
        }
    }
}

```

Possibly the most efficient implementation of Ackermann in C#. It successfully runs Ack(4,2) when executed in Visual Studio. Don't forget to add a reference to System.Numerics.


## C++



### Basic version


```cpp
#include <iostream>

unsigned int ackermann(unsigned int m, unsigned int n) {
  if (m == 0) {
    return n + 1;
  }
  if (n == 0) {
    return ackermann(m - 1, 1);
  }
  return ackermann(m - 1, ackermann(m, n - 1));
}

int main() {
  for (unsigned int m = 0; m < 4; ++m) {
    for (unsigned int n = 0; n < 10; ++n) {
      std::cout << "A(" << m << ", " << n << ") = " << ackermann(m, n) << "\n";
    }
  }
}

```



### Efficient version

{{trans|D}}
C++11 with boost's big integer type. Compile with:
 g++ -std=c++11 -I /path/to/boost ackermann.cpp.

```cpp
#include <iostream>
#include <sstream>
#include <string>
#include <boost/multiprecision/cpp_int.hpp>

using big_int = boost::multiprecision::cpp_int;

big_int ipow(big_int base, big_int exp) {
  big_int result(1);
  while (exp) {
    if (exp & 1) {
      result *= base;
    }
    exp >>= 1;
    base *= base;
  }
  return result;
}

big_int ackermann(unsigned m, unsigned n) {
  static big_int (*ack)(unsigned, big_int) =
      [](unsigned m, big_int n)->big_int {
    switch (m) {
    case 0:
      return n + 1;
    case 1:
      return n + 2;
    case 2:
      return 3 + 2 * n;
    case 3:
      return 5 + 8 * (ipow(big_int(2), n) - 1);
    default:
      return n == 0 ? ack(m - 1, big_int(1)) : ack(m - 1, ack(m, n - 1));
    }
  };
  return ack(m, big_int(n));
}

int main() {
  for (unsigned m = 0; m < 4; ++m) {
    for (unsigned n = 0; n < 10; ++n) {
      std::cout << "A(" << m << ", " << n << ") = " << ackermann(m, n) << "\n";
    }
  }

  std::cout << "A(4, 1) = " << ackermann(4, 1) << "\n";

  std::stringstream ss;
  ss << ackermann(4, 2);
  auto text = ss.str();
  std::cout << "A(4, 2) = (" << text.length() << " digits)\n"
            << text.substr(0, 80) << "\n...\n"
            << text.substr(text.length() - 80) << "\n";
}
```


```txt


```txt

A(0, 0) = 1
A(0, 1) = 2
A(0, 2) = 3
A(0, 3) = 4
A(0, 4) = 5
A(0, 5) = 6
A(0, 6) = 7
A(0, 7) = 8
A(0, 8) = 9
A(0, 9) = 10
A(1, 0) = 2
A(1, 1) = 3
A(1, 2) = 4
A(1, 3) = 5
A(1, 4) = 6
A(1, 5) = 7
A(1, 6) = 8
A(1, 7) = 9
A(1, 8) = 10
A(1, 9) = 11
A(2, 0) = 3
A(2, 1) = 5
A(2, 2) = 7
A(2, 3) = 9
A(2, 4) = 11
A(2, 5) = 13
A(2, 6) = 15
A(2, 7) = 17
A(2, 8) = 19
A(2, 9) = 21
A(3, 0) = 5
A(3, 1) = 13
A(3, 2) = 29
A(3, 3) = 61
A(3, 4) = 125
A(3, 5) = 253
A(3, 6) = 509
A(3, 7) = 1021
A(3, 8) = 2045
A(3, 9) = 4093
A(4, 1) = 65533
A(4, 2) = (19729 digits)
2003529930406846464979072351560255750447825475569751419265016973710894059556311
...
4717124577965048175856395072895337539755822087777506072339445587895905719156733

```



## Chapel


```chapel
proc A(m:int, n:int):int {
        if m == 0 then
                return n + 1;
        else if n == 0 then
                return A(m - 1, 1);
        else
                return A(m - 1, A(m, n - 1));
}
```



## Clay


```Clay
ackermann(m, n) {
    if(m == 0)
      return n + 1;
    if(n == 0)
      return ackermann(m - 1, 1);

    return ackermann(m - 1, ackermann(m, n - 1));
}
```



## CLIPS

'''Functional solution'''

```clips
(deffunction ackerman
  (?m ?n)
  (if (= 0 ?m)
    then (+ ?n 1)
    else (if (= 0 ?n)
      then (ackerman (- ?m 1) 1)
      else (ackerman (- ?m 1) (ackerman ?m (- ?n 1)))
    )
  )
)
```

{{out|Example usage}}

```txt
CLIPS> (ackerman 0 4)
5
CLIPS> (ackerman 1 4)
6
CLIPS> (ackerman 2 4)
11
CLIPS> (ackerman 3 4)
125

```

'''Fact-based solution'''

```clips
(deffacts solve-items
  (solve 0 4)
  (solve 1 4)
  (solve 2 4)
  (solve 3 4)
)

(defrule acker-m-0
  ?compute <- (compute 0 ?n)
  =>
  (retract ?compute)
  (assert (ackerman 0 ?n (+ ?n 1)))
)

(defrule acker-n-0-pre
  (compute ?m&:(> ?m 0) 0)
  (not (ackerman =(- ?m 1) 1 ?))
  =>
  (assert (compute (- ?m 1) 1))
)

(defrule acker-n-0
  ?compute <- (compute ?m&:(> ?m 0) 0)
  (ackerman =(- ?m 1) 1 ?val)
  =>
  (retract ?compute)
  (assert (ackerman ?m 0 ?val))
)

(defrule acker-m-n-pre-1
  (compute ?m&:(> ?m 0) ?n&:(> ?n 0))
  (not (ackerman ?m =(- ?n 1) ?))
  =>
  (assert (compute ?m (- ?n 1)))
)

(defrule acker-m-n-pre-2
  (compute ?m&:(> ?m 0) ?n&:(> ?n 0))
  (ackerman ?m =(- ?n 1) ?newn)
  (not (ackerman =(- ?m 1) ?newn ?))
  =>
  (assert (compute (- ?m 1) ?newn))
)

(defrule acker-m-n
  ?compute <- (compute ?m&:(> ?m 0) ?n&:(> ?n 0))
  (ackerman ?m =(- ?n 1) ?newn)
  (ackerman =(- ?m 1) ?newn ?val)
  =>
  (retract ?compute)
  (assert (ackerman ?m ?n ?val))
)

(defrule acker-solve
  (solve ?m ?n)
  (not (compute ?m ?n))
  (not (ackerman ?m ?n ?))
  =>
  (assert (compute ?m ?n))
)

(defrule acker-solved
  ?solve <- (solve ?m ?n)
  (ackerman ?m ?n ?result)
  =>
  (retract ?solve)
  (printout t "A(" ?m "," ?n ") = " ?result crlf)
)
```

When invoked, each required A(m,n) needed to solve the requested (solve ?m ?n) facts gets generated as its own fact. Below shows the invocation of the above, as well as an excerpt of the final facts list. Regardless of how many input (solve ?m ?n) requests are made, each possible A(m,n) is only solved once.

```txt
CLIPS> (reset)
CLIPS> (facts)
f-0     (initial-fact)
f-1     (solve 0 4)
f-2     (solve 1 4)
f-3     (solve 2 4)
f-4     (solve 3 4)
For a total of 5 facts.
CLIPS> (run)
A(3,4) = 125
A(2,4) = 11
A(1,4) = 6
A(0,4) = 5
CLIPS> (facts)
f-0     (initial-fact)
f-15    (ackerman 0 1 2)
f-16    (ackerman 1 0 2)
f-18    (ackerman 0 2 3)
...
f-632   (ackerman 1 123 125)
f-633   (ackerman 2 61 125)
f-634   (ackerman 3 4 125)
For a total of 316 facts.
CLIPS>
```



## Clojure


```clojure
(defn ackermann [m n]
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ackermann.

       DATA DIVISION.
       LINKAGE SECTION.
       01  M          USAGE UNSIGNED-LONG.
       01  N          USAGE UNSIGNED-LONG.

       01  Return-Val USAGE UNSIGNED-LONG.

       PROCEDURE DIVISION USING M N Return-Val.
           EVALUATE M ALSO N
               WHEN 0 ALSO ANY
                   ADD 1 TO N GIVING Return-Val

               WHEN NOT 0 ALSO 0
                   SUBTRACT 1 FROM M
                   CALL "Ackermann" USING BY CONTENT M BY CONTENT 1
                       BY REFERENCE Return-Val

               WHEN NOT 0 ALSO NOT 0
                   SUBTRACT 1 FROM N
                   CALL "Ackermann" USING BY CONTENT M BY CONTENT N
                       BY REFERENCE Return-Val

                   SUBTRACT 1 FROM M
                   CALL "Ackermann" USING BY CONTENT M
                       BY CONTENT Return-Val BY REFERENCE Return-Val
           END-EVALUATE

           GOBACK
           .
```



## CoffeeScript


```coffeescript
ackermann = (m, n) ->
  if m is 0 then n + 1
  else if m > 0 and n is 0 then ackermann m - 1, 1
  else ackermann m - 1, ackermann m, n - 1
```



## Common Lisp


```lisp
(defun ackermann (m n)
  (cond ((zerop m) (1+ n))
        ((zerop n) (ackermann (1- m) 1))
        (t         (ackermann (1- m) (ackermann m (1- n))))))
```

More elaborately:

```lisp
(defun ackermann (m n)
  (case m ((0) (1+ n))
    ((1) (+ 2 n))
    ((2) (+ n n 3))
    ((3) (- (expt 2 (+ 3 n)) 3))
    (otherwise (ackermann (1- m) (if (zerop n) 1 (ackermann m (1- n)))))))

(loop for m from 0 to 4 do
      (loop for n from (- 5 m) to (- 6 m) do
	    (format t "A(~d, ~d) = ~d~%" m n (ackermann m n))))
```

{{out}}
```txt
A(0, 5) = 6
A(0, 6) = 7
A(1, 4) = 6
A(1, 5) = 7
A(2, 3) = 9
A(2, 4) = 11
A(3, 2) = 29
A(3, 3) = 61
A(4, 1) = 65533
A(4, 2) = 2003529930 <... skipping a few digits ...> 56733
```


## Coq


```coq
Require Import Arith.
Fixpoint A m := fix A_m n :=
  match m with
    | 0 => n + 1
    | S pm =>
      match n with
        | 0 => A pm 1
        | S pn => A pm (A_m pn)
      end
  end.
```



```coq
Require Import Utf8.

Section FOLD.
  Context {A: Type} (f: A → A) (a: A).
  Fixpoint fold (n: nat) : A :=
    match n with
    | O => a
    | S n' => f (fold n')
    end.
End FOLD.

Definition ackermann : nat → nat → nat :=
  fold (λ g, fold g (g (S O))) S.

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE NpctAckerman;

IMPORT  StdLog;

VAR
	m,n: INTEGER;

PROCEDURE Ackerman (x,y: INTEGER):INTEGER;

BEGIN
  IF    x = 0  THEN  RETURN  y + 1
  ELSIF y = 0  THEN  RETURN  Ackerman (x - 1 , 1)
  ELSE
    RETURN  Ackerman (x - 1 , Ackerman (x , y - 1))
  END
END Ackerman;

PROCEDURE Do*;
BEGIN
  FOR  m := 0  TO  3  DO
    FOR  n := 0  TO  6  DO
      StdLog.Int (Ackerman (m, n));StdLog.Char (' ')
    END;
    StdLog.Ln
  END;
  StdLog.Ln
END Do;

END NpctAckerman.

```

Execute: ^Q NpctAckerman.Do<br/>

```txt


```txt

 1  2  3  4  5  6  7
 2  3  4  5  6  7  8
 3  5  7  9  11  13  15
 5  13  29  61  125  253  509

```



## Crystal

{{trans|Ruby}}

```ruby
def ack(m, n)
  if m == 0
    n + 1
  elsif n == 0
    ack(m-1, 1)
  else
    ack(m-1, ack(m, n-1))
  end
end

#Example:
(0..3).each do |m|
  puts (0..6).map { |n| ack(m, n) }.join(' ')
end

```


{{out}}

```txt

1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509

```



## D


### Basic version


```d
ulong ackermann(in ulong m, in ulong n) pure nothrow @nogc {
    if (m == 0)
        return n + 1;
    if (n == 0)
        return ackermann(m - 1, 1);
    return ackermann(m - 1, ackermann(m, n - 1));
}

void main() {
    assert(ackermann(2, 4) == 11);
}
```



### More Efficient Version

{{trans|Mathematica}}

```d
import std.stdio, std.bigint, std.conv;

BigInt ipow(BigInt base, BigInt exp) pure nothrow {
    auto result = 1.BigInt;
    while (exp) {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        base *= base;
    }

    return result;
}

BigInt ackermann(in uint m, in uint n) pure nothrow
out(result) {
    assert(result >= 0);
} body {
    static BigInt ack(in uint m, in BigInt n) pure nothrow {
        switch (m) {
            case 0: return n + 1;
            case 1: return n + 2;
            case 2: return 3 + 2 * n;
            //case 3: return 5 + 8 * (2 ^^ n - 1);
            case 3: return 5 + 8 * (ipow(2.BigInt, n) - 1);
            default: return (n == 0) ?
                        ack(m - 1, 1.BigInt) :
                        ack(m - 1, ack(m, n - 1));
        }
    }

    return ack(m, n.BigInt);
}

void main() {
    foreach (immutable m; 1 .. 4)
        foreach (immutable n; 1 .. 9)
            writefln("ackermann(%d, %d): %s", m, n, ackermann(m, n));
    writefln("ackermann(4, 1): %s", ackermann(4, 1));

    immutable a = ackermann(4, 2).text;
    writefln("ackermann(4, 2)) (%d digits):\n%s...\n%s",
             a.length, a[0 .. 94], a[$ - 96 .. $]);
}
```


{{out}}

```txt
ackermann(1, 1): 3
ackermann(1, 2): 4
ackermann(1, 3): 5
ackermann(1, 4): 6
ackermann(1, 5): 7
ackermann(1, 6): 8
ackermann(1, 7): 9
ackermann(1, 8): 10
ackermann(2, 1): 5
ackermann(2, 2): 7
ackermann(2, 3): 9
ackermann(2, 4): 11
ackermann(2, 5): 13
ackermann(2, 6): 15
ackermann(2, 7): 17
ackermann(2, 8): 19
ackermann(3, 1): 13
ackermann(3, 2): 29
ackermann(3, 3): 61
ackermann(3, 4): 125
ackermann(3, 5): 253
ackermann(3, 6): 509
ackermann(3, 7): 1021
ackermann(3, 8): 2045
ackermann(4, 1): 65533
ackermann(4, 2)) (19729 digits):
2003529930406846464979072351560255750447825475569751419265016973710894059556311453089506130880...
699146577530041384717124577965048175856395072895337539755822087777506072339445587895905719156733
```



## Dart

no caching, the implementation takes ages even for A(4,1)

```dart
int A(int m, int n) => m==0 ? n+1 : n==0 ? A(m-1,1) : A(m-1,A(m,n-1));

main() {
  print(A(0,0));
  print(A(1,0));
  print(A(0,1));
  print(A(2,2));
  print(A(2,3));
  print(A(3,3));
  print(A(3,4));
  print(A(3,5));
  print(A(4,0));
}
```



## Dc

This needs a modern Dc with <code>r</code> (swap) and <code>#</code> (comment). It easily can be adapted to an older Dc, but it will impact readability a lot.

```dc
[               # todo: n 0 -- n+1 and break 2 levels
  + 1 +         # n+1
  q
] s1

[               # todo: m 0 -- A(m-1,1) and break 2 levels
  + 1 -         # m-1
  1             # m-1 1
  lA x          # A(m-1,1)
  q
] s2

[               # todo: m n -- A(m,n)
  r d 0=1       # n m(!=0)
  r d 0=2       # m(!=0) n(!=0)
  Sn            # m(!=0)
  d 1 - r       # m-1 m
  Ln 1 -        # m-1 m n-1
  lA x          # m-1 A(m,n-1)
  lA x          # A(m-1,A(m,n-1))
] sA

3 9 lA x f
```

{{out}}

```txt

4093

```



## Delphi


```delphi
function Ackermann(m,n:Int64):Int64;
begin
    if m = 0 then
        Result := n + 1
    else if n = 0 then
        Result := Ackermann(m-1, 1)
    else
        Result := Ackermann(m-1, Ackermann(m, n - 1));
end;
```



## DWScript


```delphi
function Ackermann(m, n : Integer) : Integer;
begin
    if m = 0 then
        Result := n+1
    else if n = 0 then
        Result := Ackermann(m-1, 1)
    else Result := Ackermann(m-1, Ackermann(m, n-1));
end;
```



## Dylan


```dylan
define method ack(m == 0, n :: <integer>)
   n + 1
end;
define method ack(m :: <integer>, n :: <integer>)
   ack(m - 1, if (n == 0) 1 else ack(m, n - 1) end)
end;
```



## E


```e
def A(m, n) {
    return if (m <=> 0)          { n+1              } \
      else if (m > 0 && n <=> 0) { A(m-1, 1)        } \
      else                       { A(m-1, A(m,n-1)) }
}
```



## EasyLang

<lang>func ackerm m n . r .
  if m = 0
    r = n + 1
  elif n = 0
    call ackerm m - 1 1 r
  else
    call ackerm m n - 1 h
    call ackerm m - 1 h r
  .
.
call ackerm 3 6 r
print r
```



## Egel


```Egel

def ackermann =
    [ 0 N -> N + 1
    | M 0 -> ackermann (M - 1) 1
    | M N -> ackermann (M - 1) (ackermann M (N - 1)) ]

```



## Eiffel


```Eiffel

note
	description: "Example of Ackerman function"
	synopsis: "[
		The EIS link below (in Eiffel Studio) will launch in either an in-IDE browser or
		and external browser (your choice). The protocol informs Eiffel Studio about what
		program to use to open the `src' reference, which can be URI, PDF, or DOC. See
		second EIS for more information.
		]"
	EIS: "name=Ackermann_function", "protocol=URI", "tag=rosetta_code",
		"src=http://rosettacode.org/wiki/Ackermann_function"
	EIS: "name=eis_protocols", "protocol=URI", "tag=eiffel_docs",
		"src=https://docs.eiffel.com/book/eiffelstudio/protocols"

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		do
			print ("%N A(0,0):" + ackerman (0, 0).out)
			print ("%N A(1,0):" + ackerman (1, 0).out)
			print ("%N A(0,1):" + ackerman (0, 1).out)
			print ("%N A(1,1):" + ackerman (1, 1).out)
			print ("%N A(2,0):" + ackerman (2, 0).out)
			print ("%N A(2,1):" + ackerman (2, 1).out)
			print ("%N A(2,2):" + ackerman (2, 2).out)
			print ("%N A(0,2):" + ackerman (0, 2).out)
			print ("%N A(1,2):" + ackerman (1, 2).out)
			print ("%N A(3,3):" + ackerman (3, 3).out)
			print ("%N A(3,4):" + ackerman (3, 4).out)
		end

feature -- Access

	ackerman (m, n: NATURAL): NATURAL
		do
			if m = 0 then
				Result := n + 1
			elseif n = 0 then
				Result := ackerman (m - 1, 1)
			else
				Result := ackerman (m - 1, ackerman (m, n - 1))
			end
		end
end

```



## Ela


```ela
ack 0 n = n+1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) <| ack m <| n - 1
```



## Elena

ELENA 4.x :

```elena
import extensions;

ackermann(m,n)
{
    if(n < 0 || m < 0)
    {
        InvalidArgumentException.raise()
    };

    m =>
       0 { ^n + 1 }
       : {
              n =>
                0 { ^ackermann(m - 1,1) }
                : { ^ackermann(m - 1,ackermann(m,n-1)) }
           }
}

public program()
{
    for(int i:=0, i <= 3, i += 1)
    {
        for(int j := 0, j <= 5, j += 1)
        {
            console.printLine("A(",i,",",j,")=",ackermann(i,j))
        }
    };

    console.readChar()
}
```

{{out}}

```txt

A(0,0)=1
A(0,1)=2
A(0,2)=3
A(0,3)=4
A(0,4)=5
A(0,5)=6
A(1,0)=2
A(1,1)=3
A(1,2)=4
A(1,3)=5
A(1,4)=6
A(1,5)=7
A(2,0)=3
A(2,1)=5
A(2,2)=7
A(2,3)=9
A(2,4)=11
A(2,5)=13
A(3,0)=5
A(3,1)=13
A(3,2)=29
A(3,3)=61
A(3,4)=125
A(3,5)=253

```



## Elixir


```elixir
defmodule Ackermann do
  def ack(0, n), do: n + 1
  def ack(m, 0), do: ack(m - 1, 1)
  def ack(m, n), do: ack(m - 1, ack(m, n - 1))
end

Enum.each(0..3, fn m ->
  IO.puts Enum.map_join(0..6, " ", fn n -> Ackermann.ack(m, n) end)
end)
```


{{out}}

```txt

1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509

```



## Emacs Lisp


```lisp
(defun ackermann (m n)
  (cond ((zerop m) (1+ n))
	((zerop n) (ackermann (1- m) 1))
	(t         (ackermann (1- m)
			      (ackermann m (1- n))))))
```



## Erlang



```erlang

-module(ackermann).
-export([ackermann/2]).

ackermann(0, N) ->
  N+1;
ackermann(M, 0) ->
  ackermann(M-1, 1);
ackermann(M, N) when M > 0 andalso N > 0 ->
  ackermann(M-1, ackermann(M, N-1)).

```



## ERRE

Iterative version, using a stack. First version used various GOTOs statement, now removed and
substituted with the new ERRE control statements.


```erre

PROGRAM ACKERMAN

!
! computes Ackermann function
! (second version for rosettacode.org)
!

!$INTEGER

DIM STACK[10000]

!$INCLUDE="PC.LIB"

PROCEDURE ACK(M,N->N)
  LOOP
    CURSOR_SAVE(->CURX%,CURY%)
    LOCATE(8,1)
    PRINT("Livello Stack:";S;"  ")
    LOCATE(CURY%,CURX%)
    IF M<>0 THEN
       IF N<>0 THEN
           STACK[S]=M
           S+=1
           N-=1
        ELSE
           M-=1
           N+=1
        END IF
        CONTINUE LOOP
     ELSE
        N+=1
        S-=1
    END IF
    IF S<>0 THEN
        M=STACK[S]
        M-=1
        CONTINUE LOOP
      ELSE
        EXIT PROCEDURE
    END IF
  END LOOP
END PROCEDURE

BEGIN
   PRINT(CHR$(12);)
   FOR X=0 TO 3 DO
     FOR Y=0 TO 9 DO
        S=1
        ACK(X,Y->ANS)
        PRINT(ANS;)
     END FOR
     PRINT
   END FOR
END PROGRAM

```


Prints a list of Ackermann function values: from A(0,0) to A(3,9). Uses a stack to avoid
overflow.
Formating options to make this pretty are available,
but for this example only basic output is used.

```txt

 1  2  3  4  5  6  7  8  9  10
 2  3  4  5  6  7  8  9  10  11
 3  5  7  9  11  13  15  17  19  21
 5  13  29  61  125  253  509  1021  2045  4093


Stack Level: 1

```



## Euphoria

This is based on the [[VBScript]] example.

```Euphoria
function ack(atom m, atom n)
    if m = 0 then
        return n + 1
    elsif m > 0 and n = 0 then
        return ack(m - 1, 1)
    else
        return ack(m - 1, ack(m, n - 1))
    end if
end function

for i = 0 to 3 do
    for j = 0 to 6 do
        printf( 1, "%5d", ack( i, j ) )
    end for
    puts( 1, "\n" )
end for
```



## Euler Math Toolbox



```Euler Math Toolbox

>M=zeros(1000,1000);
>function map A(m,n) ...
$  global M;
$  if m==0 then return n+1; endif;
$  if n==0 then return A(m-1,1); endif;
$  if m<=cols(M) and n<=cols(M) then
$    M[m,n]=A(m-1,A(m,n-1));
$    return M[m,n];
$  else return A(m-1,A(m,n-1));
$  endif;
$endfunction
>shortestformat; A((0:3)',0:5)
         1         2         3         4         5         6
         2         3         4         5         6         7
         3         5         7         9        11        13
         5        13        29        61       125       253

```



## Ezhil


```Ezhil

நிரல்பாகம் அகெர்மன்(முதலெண், இரண்டாமெண்)

  @((முதலெண் < 0) || (இரண்டாமெண் < 0)) ஆனால்

    பின்கொடு -1

  முடி

  @(முதலெண் == 0) ஆனால்

    பின்கொடு இரண்டாமெண்+1

  முடி

  @((முதலெண் > 0) && (இரண்டாமெண் == 00)) ஆனால்

    பின்கொடு அகெர்மன்(முதலெண் - 1, 1)

  முடி

  பின்கொடு அகெர்மன்(முதலெண் - 1, அகெர்மன்(முதலெண், இரண்டாமெண் - 1))

முடி

அ = int(உள்ளீடு("ஓர் எண்ணைத் தாருங்கள், அது பூஜ்ஜியமாகவோ, அதைவிடப் பெரியதாக இருக்கலாம்: "))
ஆ = int(உள்ளீடு("அதேபோல் இன்னோர் எண்ணைத் தாருங்கள், இதுவும் பூஜ்ஜியமாகவோ, அதைவிடப் பெரியதாகவோ இருக்கலாம்: "))

விடை = அகெர்மன்(அ, ஆ)

@(விடை < 0) ஆனால்

  பதிப்பி "தவறான எண்களைத் தந்துள்ளீர்கள்!"

இல்லை

  பதிப்பி "நீங்கள் தந்த எண்களுக்கான அகர்மென் மதிப்பு: ", விடை

முடி

```


=={{header|F_Sharp|F#}}==
The following program implements the Ackermann function in F# but is not tail-recursive and so runs out of stack space quite fast.

```fsharp
let rec ackermann m n =
    match m, n with
    | 0, n -> n + 1
    | m, 0 -> ackermann (m - 1) 1
    | m, n -> ackermann (m - 1) ackermann m (n - 1)

do
    printfn "%A" (ackermann (int fsi.CommandLineArgs.[1]) (int fsi.CommandLineArgs.[2]))
```

Transforming this into continuation passing style avoids limited stack space by permitting tail-recursion.

```fsharp
let ackermann M N =
    let rec acker (m, n, k) =
        match m,n with
            | 0, n -> k(n + 1)
            | m, 0 -> acker ((m - 1), 1, k)
            | m, n -> acker (m, (n - 1), (fun x -> acker ((m - 1), x, k)))
    acker (M, N, (fun x -> x))
```



## Factor


```factor
USING: kernel math locals combinators ;
IN: ackermann

:: ackermann ( m n -- u )
    {
        { [ m 0 = ] [ n 1 + ] }
        { [ n 0 = ] [ m 1 - 1 ackermann ] }
        [ m 1 - m n 1 - ackermann ackermann ]
    } cond ;
```



## Falcon


```falcon
function ackermann( m, n )
 if m == 0:  return( n + 1 )
 if n == 0:  return( ackermann( m - 1, 1 ) )
 return( ackermann( m - 1, ackermann( m, n - 1 ) ) )
end

for M in [ 0:4 ]
 for N in [ 0:7 ]
   >> ackermann( M, N ), " "
 end
 >
end
```

The above will output the below.
Formating options to make this pretty are available,
but for this example only basic output is used.

```txt

1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509

```



## FALSE


```false
[$$[%
  \$$[%
     1-\$@@a;!  { i j -> A(i-1, A(i, j-1)) }
  1]?0=[
     %1         { i 0 -> A(i-1, 1) }
   ]?
  \1-a;!
1]?0=[
  %1+           { 0 j -> j+1 }
 ]?]a: { j i }

3 3 a;! .  { 61 }
```



## Fantom


```fantom
class Main
{
  // assuming m,n are positive
  static Int ackermann (Int m, Int n)
  {
    if (m == 0)
      return n + 1
    else if (n == 0)
      return ackermann (m - 1, 1)
    else
      return ackermann (m - 1, ackermann (m, n - 1))
  }

  public static Void main ()
  {
    (0..3).each |m|
    {
      (0..6).each |n|
      {
        echo ("Ackerman($m, $n) = ${ackermann(m, n)}")
      }
    }
  }
}
```

{{out}}

```txt

Ackerman(0, 0) = 1
Ackerman(0, 1) = 2
Ackerman(0, 2) = 3
Ackerman(0, 3) = 4
Ackerman(0, 4) = 5
Ackerman(0, 5) = 6
Ackerman(0, 6) = 7
Ackerman(1, 0) = 2
Ackerman(1, 1) = 3
Ackerman(1, 2) = 4
Ackerman(1, 3) = 5
Ackerman(1, 4) = 6
Ackerman(1, 5) = 7
Ackerman(1, 6) = 8
Ackerman(2, 0) = 3
Ackerman(2, 1) = 5
Ackerman(2, 2) = 7
Ackerman(2, 3) = 9
Ackerman(2, 4) = 11
Ackerman(2, 5) = 13
Ackerman(2, 6) = 15
Ackerman(3, 0) = 5
Ackerman(3, 1) = 13
Ackerman(3, 2) = 29
Ackerman(3, 3) = 61
Ackerman(3, 4) = 125
Ackerman(3, 5) = 253
Ackerman(3, 6) = 509

```


## FBSL

Mixed-language solution using pure FBSL, Dynamic Assembler, and Dynamic C layers of FBSL v3.5 concurrently. '''The following is a single script'''; the breaks are caused by switching between RC's different syntax highlighting schemes:

```qbasic
#APPTYPE CONSOLE

TestAckermann()

PAUSE

SUB TestAckermann()
	FOR DIM m = 0 TO 3
		FOR DIM n = 0 TO 10
			PRINT AckermannF(m, n), " ";
		NEXT
		PRINT
	NEXT
END SUB

FUNCTION AckermannF(m AS INTEGER, n AS INTEGER) AS INTEGER
	IF NOT m THEN RETURN n + 1
	IF NOT n THEN RETURN AckermannA(m - 1, 1)
	RETURN AckermannC(m - 1, AckermannF(m, n - 1))
END FUNCTION

DYNC AckermannC(m AS INTEGER, n AS INTEGER) AS INTEGER
```

```C

	int Ackermann(int m, int n)
	{
		if (!m) return n + 1;
		if (!n) return Ackermann(m - 1, 1);
		return Ackermann(m - 1, Ackermann(m, n - 1));
	}

	int main(int m, int n)
	{
		return Ackermann(m, n);
	}
```

```qbasic

END DYNC

DYNASM AckermannA(m AS INTEGER, n AS INTEGER) AS INTEGER
```

```asm

	ENTER 0, 0
	INVOKE Ackermann, m, n
	LEAVE
	RET

	@Ackermann
	ENTER 0, 0

	.IF DWORD PTR [m] .THEN
		JMP @F
	.ENDIF
	MOV EAX, n
	INC EAX
	JMP xit

	@@
	.IF DWORD PTR [n] .THEN
		JMP @F
	.ENDIF
	MOV EAX, m
	DEC EAX
	INVOKE Ackermann, EAX, 1
	JMP xit

	@@
	MOV EAX, n
	DEC EAX
	INVOKE Ackermann, m, EAX
	MOV ECX, m
	DEC ECX
	INVOKE Ackermann, ECX, EAX

	@xit
	LEAVE
	RET 8

```

```qbasic>END DYNASM</lang


{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 11
2 3 4 5 6 7 8 9 10 11 12
3 5 7 9 11 13 15 17 19 21 23
5 13 29 61 125 253 509 1021 2045 4093 8189

Press any key to continue...

```



## Forth


```forth
: acker ( m n -- u )
	over 0= IF  nip 1+ EXIT  THEN
	swap 1- swap ( m-1 n -- )
	dup  0= IF  1+  recurse EXIT  THEN
	1- over 1+ swap recurse recurse ;
```

{{out|Example of use}}

```txt
FORTH> 0 0 acker . 1  ok
FORTH> 3 4 acker . 125  ok
```

An optimized version:

```forth
: ackermann                            ( m n -- u )
  over                                 ( case statement)
  0 over = if drop nip 1+     else
  1 over = if drop nip 2 +    else
  2 over = if drop nip 2* 3 + else
  3 over = if drop swap 5 + swap lshift 3 - else
    drop swap 1- swap dup
    if
      1- over 1+ swap recurse recurse exit
    else
      1+ recurse exit                  \ allow tail recursion
    then
  then then then then
;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM EXAMPLE
  IMPLICIT NONE

  INTEGER :: i, j

  DO i = 0, 3
    DO j = 0, 6
       WRITE(*, "(I10)", ADVANCE="NO") Ackermann(i, j)
    END DO
    WRITE(*,*)
  END DO

CONTAINS

  RECURSIVE FUNCTION Ackermann(m, n) RESULT(ack)
    INTEGER :: ack, m, n

    IF (m == 0) THEN
      ack = n + 1
    ELSE IF (n == 0) THEN
      ack = Ackermann(m - 1, 1)
    ELSE
      ack = Ackermann(m - 1, Ackermann(m, n - 1))
    END IF
  END FUNCTION Ackermann

END PROGRAM EXAMPLE
```



## Free Pascal

See [[#Delphi]] or [[#Pascal]].

## FreeBASIC


```freebasic
' version 28-10-2016
' compile with: fbc -s console
' to do A(4, 2) the stack size needs to be increased
' compile with: fbc -s console -t 2000

Function ackerman (m As Long, n As Long) As Long

    If m = 0 Then ackerman = n +1

    If m > 0 Then
        If n = 0 Then
            ackerman = ackerman(m -1, 1)
        Else
            If n > 0 Then
                ackerman = ackerman(m -1, ackerman(m, n -1))
            End If
        End If
    End If
End Function

' ------=< MAIN >=------

Dim As Long m, n
Print

For m = 0 To 4
    Print Using "###"; m;
    For n = 0 To 10
        ' A(4, 1) or higher will run out of stack memory (default 1M)
        ' change n = 1 to n = 2 to calculate A(4, 2), increase stack!
        If m = 4 And n = 1 Then Exit For
        Print Using "######"; ackerman(m, n);
    Next
    Print
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
  0     1     2     3     4     5     6     7     8     9    10    11
  1     2     3     4     5     6     7     8     9    10    11    12
  2     3     5     7     9    11    13    15    17    19    21    23
  3     5    13    29    61   125   253   509  1021  2045  4093  8189
  4    13
```



## FunL


```funl
def
  ackermann( 0, n ) = n + 1
  ackermann( m, 0 ) = ackermann( m - 1, 1 )
  ackermann( m, n ) = ackermann( m - 1, ackermann(m, n - 1) )

for m <- 0..3, n <- 0..4
  printf( 'Ackermann( %d, %d ) = %d\n', m, n, ackermann(m, n) )
```


{{out}}


```txt

Ackermann( 0, 0 ) = 1
Ackermann( 0, 1 ) = 2
Ackermann( 0, 2 ) = 3
Ackermann( 0, 3 ) = 4
Ackermann( 0, 4 ) = 5
Ackermann( 1, 0 ) = 2
Ackermann( 1, 1 ) = 3
Ackermann( 1, 2 ) = 4
Ackermann( 1, 3 ) = 5
Ackermann( 1, 4 ) = 6
Ackermann( 2, 0 ) = 3
Ackermann( 2, 1 ) = 5
Ackermann( 2, 2 ) = 7
Ackermann( 2, 3 ) = 9
Ackermann( 2, 4 ) = 11
Ackermann( 3, 0 ) = 5
Ackermann( 3, 1 ) = 13
Ackermann( 3, 2 ) = 29
Ackermann( 3, 3 ) = 61
Ackermann( 3, 4 ) = 125

```



## Futhark



```Futhark

fun ackermann(m: int, n: int): int =
  if m == 0 then n + 1
  else if n == 0 then ackermann(m-1, 1)
  else ackermann(m - 1, ackermann(m, n-1))

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 1

begin globals
dim as container gC
end globals

def fn Ackerman( m as long, n as long ) as long

local fn Ackerman( m as long, n as long ) as long
dim as long result

if m == 0 then result = n + 1 : exit fn

if ( n == 0 )
   result = fn Ackerman( m - 1, 1 )
   exit fn
end if

result = fn Ackerman( m - 1, fn Ackerman(m, n - 1) )
end fn = result

dim as long n, m

/*
  Cache response in global string container to speed
  processing rather printing each iteration.
*/
for m = 0 to 3
   for n = 0 to 10
      gC += "fn ackerman(" + str$(m) + "," + str$(n) + " ) =" + Str$( fn Ackerman( m, n ) ) + chr$(13)
   next
next

print gC

```


Output:

```txt

fn ackerman( 0, 0 ) = 1
fn ackerman( 0, 1 ) = 2
fn ackerman( 0, 2 ) = 3
fn ackerman( 0, 3 ) = 4
fn ackerman( 0, 4 ) = 5
fn ackerman( 0, 5 ) = 6
fn ackerman( 0, 6 ) = 7
fn ackerman( 0, 7 ) = 8
fn ackerman( 0, 8 ) = 9
fn ackerman( 0, 9 ) = 10
fn ackerman( 0, 10 ) = 11
fn ackerman( 1, 0 ) = 2
fn ackerman( 1, 1 ) = 3
fn ackerman( 1, 2 ) = 4
fn ackerman( 1, 3 ) = 5
fn ackerman( 1, 4 ) = 6
fn ackerman( 1, 5 ) = 7
fn ackerman( 1, 6 ) = 8
fn ackerman( 1, 7 ) = 9
fn ackerman( 1, 8 ) = 10
fn ackerman( 1, 9 ) = 11
fn ackerman( 1, 10 ) = 12
fn ackerman( 2, 0 ) = 3
fn ackerman( 2, 1 ) = 5
fn ackerman( 2, 2 ) = 7
fn ackerman( 2, 3 ) = 9
fn ackerman( 2, 4 ) = 11
fn ackerman( 2, 5 ) = 13
fn ackerman( 2, 6 ) = 15
fn ackerman( 2, 7 ) = 17
fn ackerman( 2, 8 ) = 19
fn ackerman( 2, 9 ) = 21
fn ackerman( 2, 10 ) = 23
fn ackerman( 3, 0 ) = 5
fn ackerman( 3, 1 ) = 13
fn ackerman( 3, 2 ) = 29
fn ackerman( 3, 3 ) = 61
fn ackerman( 3, 4 ) = 125
fn ackerman( 3, 5 ) = 253
fn ackerman( 3, 6 ) = 509
fn ackerman( 3, 7 ) = 1021
fn ackerman( 3, 8 ) = 2045
fn ackerman( 3, 9 ) = 4093
fn ackerman( 3, 10 ) = 8189

```



## Gambas


```gambas
Public Function Ackermann(m As Float, n As Float) As Float
  If m = 0 Then
    Return n + 1
  End If
  If n = 0 Then
    Return Ackermann(m - 1, 1)
  End If
  Return Ackermann(m - 1, Ackermann(m, n - 1))
End

Public Sub Main()
  Dim m, n As Float
  For m = 0 To 3
    For n = 0 To 4
      Print "Ackermann("; m; ", "; n; ") = "; Ackermann(m, n)
    Next
  Next
End
```



## GAP


```gap
ack := function(m, n)
  if m = 0 then
    return n + 1;
  elif (m > 0) and (n = 0) then
    return ack(m - 1, 1);
  elif (m > 0) and (n > 0) then
    return ack(m - 1, ack(m, n - 1));
  else
    return fail;
  fi;
end;
```



## Genyris


```genyris
def A (m n)
   cond
      (equal? m 0)
          + n 1
      (equal? n 0)
          A (- m 1) 1
      else
          A (- m 1)
             A m (- n 1)
```



## GML

Define a script resource named ackermann and paste this code inside:

```GML
///ackermann(m,n)
var m, n;
m = argument0;
n = argument1;
if(m=0)
    {
    return (n+1)
    }
else if(n == 0)
    {
    return (ackermann(m-1,1,1))
    }
else
    {
    return (ackermann(m-1,ackermann(m,n-1,2),1))
    }
```



## gnuplot


```gnuplot
A (m, n) = m == 0 ? n + 1 : n == 0 ? A (m - 1, 1) : A (m - 1, A (m, n - 1))
print A (0, 4)
print A (1, 4)
print A (2, 4)
print A (3, 4)
```

{{out}}
 5
 6
 11
 stack overflow


## Go


### Classic version


```go
func Ackermann(m, n uint) uint {
	switch 0 {
	case m:
		return n + 1
	case n:
		return Ackermann(m - 1, 1)
	}
	return Ackermann(m - 1, Ackermann(m, n - 1))
}
```


### Expanded version


```go
func Ackermann2(m, n uint) uint {
  switch {
    case m == 0:
      return n + 1
    case m == 1:
      return n + 2
    case m == 2:
      return 2*n + 3
    case m == 3:
      return 8 << n - 3
    case n == 0:
      return Ackermann2(m - 1, 1)
  }
  return Ackermann2(m - 1, Ackermann2(m, n - 1))
}
```


### Expanded version with arbitrary precision


```go
package main

import (
	"fmt"
	"math/big"
	"math/bits" // Added in Go 1.9
)

var one = big.NewInt(1)
var two = big.NewInt(2)
var three = big.NewInt(3)
var eight = big.NewInt(8)

func Ackermann2(m, n *big.Int) *big.Int {
	if m.Cmp(three) <= 0 {
		switch m.Int64() {
		case 0:
			return new(big.Int).Add(n, one)
		case 1:
			return new(big.Int).Add(n, two)
		case 2:
			r := new(big.Int).Lsh(n, 1)
			return r.Add(r, three)
		case 3:
			if nb := n.BitLen(); nb > bits.UintSize {
				// n is too large to represent as a
				// uint for use in the Lsh method.
				panic(TooBigError(nb))

				// If we tried to continue anyway, doing
				// 8*2^n-3 as bellow, we'd use hundreds
				// of megabytes and lots of CPU time
				// without the Exp call even returning.
				r := new(big.Int).Exp(two, n, nil)
				r.Mul(eight, r)
				return r.Sub(r, three)
			}
			r := new(big.Int).Lsh(eight, uint(n.Int64()))
			return r.Sub(r, three)
		}
	}
	if n.BitLen() == 0 {
		return Ackermann2(new(big.Int).Sub(m, one), one)
	}
	return Ackermann2(new(big.Int).Sub(m, one),
		Ackermann2(m, new(big.Int).Sub(n, one)))
}

type TooBigError int

func (e TooBigError) Error() string {
	return fmt.Sprintf("A(m,n) had n of %d bits; too large", int(e))
}

func main() {
	show(0, 0)
	show(1, 2)
	show(2, 4)
	show(3, 100)
	show(3, 1e6)
	show(4, 1)
	show(4, 2)
	show(4, 3)
}

func show(m, n int64) {
	defer func() {
		// Ackermann2 could/should have returned an error
		// instead of a panic. But here's how to recover
		// from the panic, and report "expected" errors.
		if e := recover(); e != nil {
			if err, ok := e.(TooBigError); ok {
				fmt.Println("Error:", err)
			} else {
				panic(e)
			}
		}
	}()

	fmt.Printf("A(%d, %d) = ", m, n)
	a := Ackermann2(big.NewInt(m), big.NewInt(n))
	if a.BitLen() <= 256 {
		fmt.Println(a)
	} else {
		s := a.String()
		fmt.Printf("%d digits starting/ending with: %s...%s\n",
			len(s), s[:20], s[len(s)-20:],
		)
	}
}
```

{{out}}

```txt

A(0, 0) = 1
A(1, 2) = 4
A(2, 4) = 11
A(3, 100) = 10141204801825835211973625643005
A(3, 1000000) = 301031 digits starting/ending with: 79205249834367186005...39107225301976875005
A(4, 1) = 65533
A(4, 2) = 19729 digits starting/ending with: 20035299304068464649...45587895905719156733
A(4, 3) = Error: A(m,n) had n of 65536 bits; too large

```



## Golfscript


```golfscript
{
  :_n; :_m;
  _m 0= {_n 1+}
        {_n 0= {_m 1- 1 ack}
               {_m 1- _m _n 1- ack ack}
               if}
        if
}:ack;
```



## Groovy


```groovy
def ack ( m, n ) {
    assert m >= 0 && n >= 0 : 'both arguments must be non-negative'
    m == 0 ? n + 1 : n == 0 ? ack(m-1, 1) : ack(m-1, ack(m, n-1))
}
```

Test program:

```groovy
def ackMatrix = (0..3).collect { m -> (0..8).collect { n -> ack(m, n) } }
ackMatrix.each { it.each { elt -> printf "%7d", elt }; println() }
```

{{out}}

```txt
      1      2      3      4      5      6      7      8      9
      2      3      4      5      6      7      8      9     10
      3      5      7      9     11     13     15     17     19
      5     13     29     61    125    253    509   1021   2045
```

Note: In the default groovyConsole configuration for WinXP, "ack(4,1)" caused a stack overflow error!


## Haskell


```haskell
ack :: Int -> Int -> Int
ack 0 n = succ n
ack m 0 = ack (pred m) 1
ack m n = ack (pred m) (ack m (pred n))

main :: IO ()
main = mapM_ print $ uncurry ack <$> [(0, 0), (3, 4)]
```

{{out}}

```txt
1
125
```


Generating a list instead:

```haskell
import Data.List (mapAccumL)

-- everything here are [Int] or [[Int]], which would overflow
-- * had it not overrun the stack first *
ackermann = iterate ack [1..] where
	ack a = s where
		s = snd $ mapAccumL f (tail a) (1 : zipWith (-) s (1:s))
	f a b = (aa, head aa) where aa = drop b a

main = mapM_ print $ map (\n -> take (6 - n) $ ackermann !! n) [0..5]
```



## Haxe


```haxe
class RosettaDemo
{
    static public function main()
    {
        Sys.print(ackermann(3, 4));
    }

    static function ackermann(m : Int, n : Int)
    {
        if (m == 0)
        {
            return n + 1;
        }
        else if (n == 0)
        {
            return ackermann(m-1, 1);
        }
        return ackermann(m-1, ackermann(m, n-1));
    }
}
```


=={{header|Icon}} and {{header|Unicon}}==
{{libheader|Icon Programming Library}}
Taken from the public domain Icon Programming Library's [http://www.cs.arizona.edu/icon/library/procs/memrfncs.htm acker in memrfncs],
written by Ralph E. Griswold.

```Icon
procedure acker(i, j)
   static memory

   initial {
      memory := table()
      every memory[0 to 100] := table()
      }

   if i = 0 then return j + 1

   if j = 0 then /memory[i][j] := acker(i - 1, 1)
   else /memory[i][j] := acker(i - 1, acker(i, j - 1))

   return memory[i][j]

end

procedure main()
   every m := 0 to 3 do {
      every n := 0 to 8 do {
         writes(acker(m, n) || " ")
         }
      write()
      }
end
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9
2 3 4 5 6 7 8 9 10
3 5 7 9 11 13 15 17 19
5 13 29 61 125 253 509 1021 2045
```



## Idris


```idris
A : Nat -> Nat -> Nat
A Z n = S n
A (S m) Z = A m (S Z)
A (S m) (S n) = A m (A (S m) n)
```



## Ioke

{{trans|Clojure}}

```ioke
ackermann = method(m,n,
  cond(
    m zero?, n succ,
    n zero?, ackermann(m pred, 1),
    ackermann(m pred, ackermann(m, n pred)))
)
```



## J

As posted at the [[j:Essays/Ackermann%27s%20Function|J wiki]]

```j
ack=: c1`c1`c2`c3 @. (#.@,&*) M.
c1=: >:@]                        NB. if 0=x, 1+y
c2=: <:@[ ack 1:                 NB. if 0=y, (x-1) ack 1
c3=: <:@[ ack [ ack <:@]         NB. else,   (x-1) ack x ack y-1
```

{{out|Example use}}

```j
   0 ack 3
4
   1 ack 3
5
   2 ack 3
9
   3 ack 3
61
```

J's stack was too small for me to compute <tt>4 ack 1</tt>.

### Alternative Primitive Recursive Version

This version works by first generating verbs (functions) and then applying them to compute the rows of the related Buck function; then the Ackermann function is obtained in terms of the Buck function. It uses extended precision to be able to compute 4 Ack 2.

The Ackermann function derived in this fashion is primitive recursive.  This is possible because in J (as in some other languages) functions, or representations of them, are first-class values.

```j
   Ack=. 3 -~ [ ({&(2 4$'>:  2x&+') ::(,&'&1'&'2x&*'@:(-&2))"0@:[ 128!:2 ]) 3 + ]
```

{{out|Example use}}

```j
   0 1 2 3 Ack 0 1 2 3 4 5 6 7
1  2  3  4   5   6   7    8
2  3  4  5   6   7   8    9
3  5  7  9  11  13  15   17
5 13 29 61 125 253 509 1021

   3 4 Ack 0 1 2
 5    13                                                                                                                                                                                                                                                        ...
13 65533 2003529930406846464979072351560255750447825475569751419265016973710894059556311453089506130880933348101038234342907263181822949382118812668869506364761547029165041871916351587966347219442930927982084309104855990570159318959639524863372367203002916...

   4 # @: ": @: Ack 2 NB. Number of digits of 4 Ack 2
19729

   5 Ack 0
65533

```


A structured derivation of Ack follows:


```j
   o=. @: NB. Composition of verbs (functions)
   x=. o[ NB. Composing the left noun (argument)

   (rows2up=. ,&'&1'&'2x&*') o i. 4
2x&*
2x&*&1
2x&*&1&1
2x&*&1&1&1
   NB. 2's multiplication, exponentiation, tetration, pentation, etc.

   0 1 2 (BuckTruncated=. (rows2up  x apply ]) f.) 0 1 2 3 4 5
0 2 4  6     8                                                                                                                                                                                                                                                  ...
1 2 4  8    16                                                                                                                                                                                                                                                  ...
1 2 4 16 65536 2003529930406846464979072351560255750447825475569751419265016973710894059556311453089506130880933348101038234342907263181822949382118812668869506364761547029165041871916351587966347219442930927982084309104855990570159318959639524863372367203...
   NB. Buck truncated function (missing the first two rows)

   BuckTruncated NB. Buck truncated function-level code
,&'&1'&'2x&*'@:[ 128!:2 ]

   (rows01=. {&('>:',:'2x&+')) 0 1 NB. The missing first two rows
>:
2x&+

   Buck=. (rows01 :: (rows2up o (-&2)))"0 x apply ]

   (Ack=. (3 -~ [ Buck 3 + ])f.)  NB. Ackermann function-level code
3 -~ [ ({&(2 4$'>:  2x&+') ::(,&'&1'&'2x&*'@:(-&2))"0@:[ 128!:2 ]) 3 + ]
```



## Java

[[Category:Arbitrary precision]]

```java
import java.math.BigInteger;

public static BigInteger ack(BigInteger m, BigInteger n) {
    return m.equals(BigInteger.ZERO)
            ? n.add(BigInteger.ONE)
            : ack(m.subtract(BigInteger.ONE),
                        n.equals(BigInteger.ZERO) ? BigInteger.ONE : ack(m, n.subtract(BigInteger.ONE)));
}
```


{{works with|Java|8+}}

```java5
@FunctionalInterface
public interface FunctionalField<FIELD extends Enum<?>> {
  public Object untypedField(FIELD field);

  @SuppressWarnings("unchecked")
  public default <VALUE> VALUE field(FIELD field) {
    return (VALUE) untypedField(field);
  }
}
```


```java5
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

public interface TailRecursive {
  public static <INPUT, INTERMEDIARY, OUTPUT> Function<INPUT, OUTPUT> new_(Function<INPUT, INTERMEDIARY> toIntermediary, UnaryOperator<INTERMEDIARY> unaryOperator, Predicate<INTERMEDIARY> predicate, Function<INTERMEDIARY, OUTPUT> toOutput) {
    return input ->
      $.new_(
        Stream.iterate(
          toIntermediary.apply(input),
          unaryOperator
        ),
        predicate,
        toOutput
      )
    ;
  }

  public static <INPUT1, INPUT2, INTERMEDIARY, OUTPUT> BiFunction<INPUT1, INPUT2, OUTPUT> new_(BiFunction<INPUT1, INPUT2, INTERMEDIARY> toIntermediary, UnaryOperator<INTERMEDIARY> unaryOperator, Predicate<INTERMEDIARY> predicate, Function<INTERMEDIARY, OUTPUT> toOutput) {
    return (input1, input2) ->
      $.new_(
        Stream.iterate(
          toIntermediary.apply(input1, input2),
          unaryOperator
        ),
        predicate,
        toOutput
      )
    ;
  }

  public enum $ {
    $$;

    private static <INTERMEDIARY, OUTPUT> OUTPUT new_(Stream<INTERMEDIARY> stream, Predicate<INTERMEDIARY> predicate, Function<INTERMEDIARY, OUTPUT> function) {
      return stream
        .filter(predicate)
        .map(function)
        .findAny()
        .orElseThrow(RuntimeException::new)
      ;
    }
  }
}
```


```java5
import java.math.BigInteger;
import java.util.Stack;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public interface Ackermann {
  public static Ackermann new_(BigInteger number1, BigInteger number2, Stack<BigInteger> stack, boolean flag) {
    return $.new_(number1, number2, stack, flag);
  }
  public static void main(String... arguments) {
    $.main(arguments);
  }
  public BigInteger number1();
  public BigInteger number2();

  public Stack<BigInteger> stack();

  public boolean flag();

  public enum $ {
    $$;

    private static final BigInteger ZERO = BigInteger.ZERO;
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TWO = BigInteger.valueOf(2);
    private static final BigInteger THREE = BigInteger.valueOf(3);
    private static final BigInteger FOUR = BigInteger.valueOf(4);

    private static Ackermann new_(BigInteger number1, BigInteger number2, Stack<BigInteger> stack, boolean flag) {
      return (FunctionalAckermann) field -> {
        switch (field) {
          case number1: return number1;
          case number2: return number2;
          case stack: return stack;
          case flag: return flag;
          default: throw new UnsupportedOperationException(
            field instanceof Field
              ? "Field checker has not been updated properly."
              : "Field is not of the correct type."
          );
        }
      };
    }

    private static final BinaryOperator<BigInteger> ACKERMANN =
      TailRecursive.new_(
        (BigInteger number1, BigInteger number2) ->
          new_(
            number1,
            number2,
            Stream.of(number1).collect(
              Collectors.toCollection(Stack::new)
            ),
            false
          )
        ,
        ackermann -> {
          BigInteger number1 = ackermann.number1();
          BigInteger number2 = ackermann.number2();
          Stack<BigInteger> stack = ackermann.stack();
          if (!stack.empty() && !ackermann.flag()) {
            number1 = stack.pop();
          }
          switch (number1.intValue()) {
            case 0:
              return new_(
                number1,
                number2.add(ONE),
                stack,
                false
              );
            case 1:
              return new_(
                number1,
                number2.add(TWO),
                stack,
                false
              );
            case 2:
              return new_(
                number1,
                number2.multiply(TWO).add(THREE),
                stack,
                false
              );
            default:
              if (ZERO.equals(number2)) {
                return new_(
                  number1.subtract(ONE),
                  ONE,
                  stack,
                  true
                );
              } else {
                stack.push(number1.subtract(ONE));
                return new_(
                  number1,
                  number2.subtract(ONE),
                  stack,
                  true
                );
              }
          }
        },
        ackermann -> ackermann.stack().empty(),
        Ackermann::number2
      )::apply
    ;

    private static void main(String... arguments) {
      System.out.println(ACKERMANN.apply(FOUR, TWO));
    }

    private enum Field {
      number1,
      number2,
      stack,
      flag
    }

    @FunctionalInterface
    private interface FunctionalAckermann extends FunctionalField<Field>, Ackermann {
      @Override
      public default BigInteger number1() {
        return field(Field.number1);
      }

      @Override
      public default BigInteger number2() {
        return field(Field.number2);
      }

      @Override
      public default Stack<BigInteger> stack() {
        return field(Field.stack);
      }

      @Override
      public default boolean flag() {
        return field(Field.flag);
      }
    }
  }
}
```

{{Iterative version}}

```java5
/*
 * Source https://stackoverflow.com/a/51092690/5520417
 */

package matematicas;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Stack;

/**
 * @author rodri
 *
 */

public class IterativeAckermannMemoryOptimization extends Thread {

  /**
   * Max percentage of free memory that the program will use. Default is 10% since
   * the majority of the used devices are mobile and therefore it is more likely
   * that the user will have more opened applications at the same time than in a
   * desktop device
   */
  private static Double SYSTEM_MEMORY_LIMIT_PERCENTAGE = 0.1;

  /**
   * Attribute of the type IterativeAckermann
   */
  private IterativeAckermann iterativeAckermann;

  /**
   * @param iterativeAckermann
   */
  public IterativeAckermannMemoryOptimization(IterativeAckermann iterativeAckermann) {
    super();
    this.iterativeAckermann = iterativeAckermann;
  }

  /**
   * @return
   */
  public IterativeAckermann getIterativeAckermann() {
    return iterativeAckermann;
  }

  /**
   * @param iterativeAckermann
   */
  public void setIterativeAckermann(IterativeAckermann iterativeAckermann) {
    this.iterativeAckermann = iterativeAckermann;
  }

  public static Double getSystemMemoryLimitPercentage() {
    return SYSTEM_MEMORY_LIMIT_PERCENTAGE;
  }

  /**
   * Principal method of the thread. Checks that the memory used doesn't exceed or
   * equal the limit, and informs the user when that happens.
   */
  @Override
  public void run() {
    String operating_system = System.getProperty("os.name").toLowerCase();
    if ( operating_system.equals("windows") || operating_system.equals("linux") || operating_system.equals("macintosh") ) {
      SYSTEM_MEMORY_LIMIT_PERCENTAGE = 0.25;
    }

    while ( iterativeAckermann.getConsumed_heap() >= SYSTEM_MEMORY_LIMIT_PERCENTAGE * Runtime.getRuntime().freeMemory() ) {
      try {
        wait();
      }
      catch ( InterruptedException e ) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    if ( ! iterativeAckermann.isAlive() )
      iterativeAckermann.start();
    else
      notifyAll();

  }

}


public class IterativeAckermann extends Thread {

  /*
   * Adjust parameters conveniently
   */
  /**
   *
   */
  private static final int HASH_SIZE_LIMIT = 636;

  /**
   *
   */
  private BigInteger m;

  /**
   *
   */
  private BigInteger n;

  /**
   *
   */
  private Integer hash_size;

  /**
   *
   */
  private Long consumed_heap;

  /**
   * @param m
   * @param n
   * @param invalid
   * @param invalid2
   */
  public IterativeAckermann(BigInteger m, BigInteger n, Integer invalid, Long invalid2) {
    super();
    this.m = m;
    this.n = n;
    this.hash_size = invalid;
    this.consumed_heap = invalid2;
  }

  /**
   *
   */
  public IterativeAckermann() {
    // TODO Auto-generated constructor stub
    super();
    m = null;
    n = null;
    hash_size = 0;
    consumed_heap = 0l;
  }

  /**
   * @return
   */
  public static BigInteger getLimit() {
    return LIMIT;
  }

  /**
   * @author rodri
   *
   * @param <T1>
   * @param <T2>
   */
  /**
   * @author rodri
   *
   * @param <T1>
   * @param <T2>
   */
  static class Pair<T1, T2> {

    /**
     *
     */
    /**
     *
     */
    T1 x;

    /**
     *
     */
    /**
     *
     */
    T2 y;

    /**
     * @param x_
     * @param y_
     */
    /**
     * @param x_
     * @param y_
     */
    Pair(T1 x_, T2 y_) {
      x = x_;
      y = y_;
    }

    /**
     *
     */
    /**
     *
     */
    @Override
    public int hashCode() {
      return x.hashCode() ^ y.hashCode();
    }

    /**
     *
     */
    /**
     *
     */
    @Override
    public boolean equals(Object o_) {

      if ( o_ == null ) {
        return false;
      }
      if ( o_.getClass() != this.getClass() ) {
        return false;
      }
      Pair<?, ?> o = (Pair<?, ?>) o_;
      return x.equals(o.x) && y.equals(o.y);
    }
  }

  /**
   *
   */
  private static final BigInteger LIMIT = new BigInteger("6");

  /**
   * @param m
   * @param n
   * @return
   */

  /**
   *
   */
  @Override
  public void run() {
    while ( hash_size >= HASH_SIZE_LIMIT ) {
      try {
        this.wait();
      }
      catch ( InterruptedException e ) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    for ( BigInteger i = BigInteger.ZERO; i.compareTo(LIMIT) == - 1; i = i.add(BigInteger.ONE) ) {
      for ( BigInteger j = BigInteger.ZERO; j.compareTo(LIMIT) == - 1; j = j.add(BigInteger.ONE) ) {
        IterativeAckermann iterativeAckermann = new IterativeAckermann(i, j, null, null);
        System.out.printf("Ackmermann(%d, %d) = %d\n", i, j, iterativeAckermann.iterative_ackermann(i, j));

      }
    }
  }

  /**
   * @return
   */
  public BigInteger getM() {
    return m;
  }

  /**
   * @param m
   */
  public void setM(BigInteger m) {
    this.m = m;
  }

  /**
   * @return
   */
  public BigInteger getN() {
    return n;
  }

  /**
   * @param n
   */
  public void setN(BigInteger n) {
    this.n = n;
  }

  /**
   * @return
   */
  public Integer getHash_size() {
    return hash_size;
  }

  /**
   * @param hash_size
   */
  public void setHash_size(Integer hash_size) {
    this.hash_size = hash_size;
  }

  /**
   * @return
   */
  public Long getConsumed_heap() {
    return consumed_heap;
  }

  /**
   * @param consumed_heap
   */
  public void setConsumed_heap(Long consumed_heap) {
    this.consumed_heap = consumed_heap;
  }

  /**
   * @param m
   * @param n
   * @return
   */
  public BigInteger iterative_ackermann(BigInteger m, BigInteger n) {
    if ( m.compareTo(BigInteger.ZERO) != - 1 && m.compareTo(BigInteger.ZERO) != - 1 )
      try {
        HashMap<Pair<BigInteger, BigInteger>, BigInteger> solved_set = new HashMap<Pair<BigInteger, BigInteger>, BigInteger>(900000);
        Stack<Pair<BigInteger, BigInteger>> to_solve = new Stack<Pair<BigInteger, BigInteger>>();
        to_solve.push(new Pair<BigInteger, BigInteger>(m, n));

        while ( ! to_solve.isEmpty() ) {
          Pair<BigInteger, BigInteger> head = to_solve.peek();
          if ( head.x.equals(BigInteger.ZERO) ) {
            solved_set.put(head, head.y.add(BigInteger.ONE));
            to_solve.pop();
          }
          else if ( head.y.equals(BigInteger.ZERO) ) {
            Pair<BigInteger, BigInteger> next = new Pair<BigInteger, BigInteger>(head.x.subtract(BigInteger.ONE), BigInteger.ONE);
            BigInteger result = solved_set.get(next);
            if ( result == null ) {
              to_solve.push(next);
            }
            else {
              solved_set.put(head, result);
              to_solve.pop();
            }
          }
          else {
            Pair<BigInteger, BigInteger> next0 = new Pair<BigInteger, BigInteger>(head.x, head.y.subtract(BigInteger.ONE));
            BigInteger result0 = solved_set.get(next0);
            if ( result0 == null ) {
              to_solve.push(next0);
            }
            else {
              Pair<BigInteger, BigInteger> next = new Pair<BigInteger, BigInteger>(head.x.subtract(BigInteger.ONE), result0);
              BigInteger result = solved_set.get(next);
              if ( result == null ) {
                to_solve.push(next);
              }
              else {
                solved_set.put(head, result);
                to_solve.pop();
              }
            }
          }
        }
        this.hash_size = solved_set.size();
        System.out.println("Hash Size: " + hash_size);
        consumed_heap = (Runtime.getRuntime().totalMemory() / (1024 * 1024));
        System.out.println("Consumed Heap: " + consumed_heap + "m");
        setHash_size(hash_size);
        setConsumed_heap(consumed_heap);
        return solved_set.get(new Pair<BigInteger, BigInteger>(m, n));

      }
      catch ( OutOfMemoryError e ) {
        // TODO: handle exception
        e.printStackTrace();
      }
    throw new IllegalArgumentException("The arguments must be non-negative integers.");
  }

  /**
   * @param args
   */
  /**
   * @param args
   */
  public static void main(String[] args) {
    IterativeAckermannMemoryOptimization iterative_ackermann_memory_optimization = new IterativeAckermannMemoryOptimization(
        new IterativeAckermann());
    iterative_ackermann_memory_optimization.start();
  }
}


```



## JavaScript


### ES5


```javascript
function ack(m, n) {
 return m === 0 ? n + 1 : ack(m - 1, n === 0  ? 1 : ack(m, n - 1));
}
```



### ES6


```javascript
(() => {
    'use strict';

    // ackermann :: Int -> Int -> Int
    const ackermann = m => n => {
        const go = (m, n) =>
            0 === m ? (
                succ(n)
            ) : go(pred(m), 0 === n ? (
                1
            ) : go(m, pred(n)));
        return go(m, n);
    };

    // TEST -----------------------------------------------
    const main = () => console.log(JSON.stringify(
        [0, 1, 2, 3].map(
            flip(ackermann)(3)
        )
    ));


    // GENERAL FUNCTIONS ----------------------------------

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        x => y => f(y)(x);

    // pred :: Enum a => a -> a
    const pred = x => x - 1;

    // succ :: Enum a => a -> a
    const succ = x => 1 + x;


    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[4,5,9,61]
```



## Joy

From [http://www.latrobe.edu.au/phimvt/joy/jp-nestrec.html here]

```joy
DEFINE ack == [ [ [pop null]  popd succ ]
                [ [null]  pop pred 1 ack ]
                [ [dup pred swap] dip pred ack ack ] ]
              cond.
```

another using a combinator

```joy
DEFINE ack == [ [ [pop null]  [popd succ] ]
		[ [null]  [pop pred 1]  [] ]
		[ [[dup pred swap] dip pred] [] [] ] ]
              condnestrec.
```

Whenever there are two definitions with the same name, the last one is the one that is used, when invoked.


## jq

{{ works with|jq|1.4}}

### Without Memoization


```jq
# input: [m,n]
def ack:
  .[0] as $m | .[1] as $n
  | if $m == 0 then $n + 1
    elif $n == 0 then [$m-1, 1] | ack
    else [$m-1, ([$m, $n-1 ] | ack)] | ack
    end ;
```

'''Example:'''

```jq
range(0;5) as $i
| range(0; if $i > 3 then 1 else 6 end) as $j
| "A(\($i),\($j)) = \( [$i,$j] | ack )"
```

{{out}}

```sh
# jq -n -r -f ackermann.jq
A(0,0) = 1
A(0,1) = 2
A(0,2) = 3
A(0,3) = 4
A(0,4) = 5
A(0,5) = 6
A(1,0) = 2
A(1,1) = 3
A(1,2) = 4
A(1,3) = 5
A(1,4) = 6
A(1,5) = 7
A(2,0) = 3
A(2,1) = 5
A(2,2) = 7
A(2,3) = 9
A(2,4) = 11
A(2,5) = 13
A(3,0) = 5
A(3,1) = 13
A(3,2) = 29
A(3,3) = 61
A(3,4) = 125
A(3,5) = 253
A(4,0) = 13
```


### With Memoization and Optimization


```jq
# input: [m,n, cache]
# output [value, updatedCache]
def ack:

  # input: [value,cache]; output: [value, updatedCache]
  def cache(key): .[1] += { (key): .[0] };

  def pow2: reduce range(0; .) as $i (1; .*2);

  .[0] as $m | .[1] as $n | .[2] as $cache
  | if   $m == 0 then [$n + 1, $cache]
    elif $m == 1 then [$n + 2, $cache]
    elif $m == 2 then [2 * $n + 3, $cache]
    elif $m == 3 then [8 * ($n|pow2) - 3, $cache]
    else
    (.[0:2]|tostring) as $key
    | $cache[$key] as $value
    | if $value then [$value, $cache]
      elif $n == 0 then
        ([$m-1, 1, $cache] | ack)
        | cache($key)
      else
        ([$m, $n-1, $cache ] | ack)
        | [$m-1, .[0], .[1]] | ack
        | cache($key)
      end
    end;

def A(m;n): [m,n,{}] | ack | .[0];

```

'''Example:'''
```jq
A(4,1)
```

{{out}}

```sh>65533</lang



## Jsish

From javascript entry.

```javascript
/* Ackermann function, in Jsish */

function ack(m, n) {
 return m === 0 ? n + 1 : ack(m - 1, n === 0  ? 1 : ack(m, n - 1));
}

if (Interp.conf('unitTest')) {
    Interp.conf({maxDepth:4096});
;    ack(1,3);
;    ack(2,3);
;    ack(3,3);
;    ack(1,5);
;    ack(2,5);
;    ack(3,5);
}

/*
=!EXPECTSTART!=
ack(1,3) ==> 5
ack(2,3) ==> 9
ack(3,3) ==> 61
ack(1,5) ==> 7
ack(2,5) ==> 13
ack(3,5) ==> 253
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U Ackermann.jsi
ack(1,3) ==> 5
ack(2,3) ==> 9
ack(3,3) ==> 61
ack(1,5) ==> 7
ack(2,5) ==> 13
ack(3,5) ==> 253
```



## Julia


```julia
function ack(m,n)
    if m == 0
        return n + 1
    elseif n == 0
        return ack(m-1,1)
    else
        return ack(m-1,ack(m,n-1))
    end
end
```


'''One-liner:'''

```julia
ack2(m::Integer, n::Integer) = m == 0 ? n + 1 : n == 0 ? ack2(m - 1, 1) : ack2(m - 1, ack2(m, n - 1))
```


'''Using memoization''', [https://github.com/simonster/Memoize.jl source]:

```julia
using Memoize
@memoize ack3(m::Integer, n::Integer) = m == 0 ? n + 1 : n == 0 ? ack3(m - 1, 1) : ack3(m - 1, ack3(m, n - 1))
```


'''Benchmarking''':

```txt
julia> @time ack2(4,1)
elapsed time: 71.98668457 seconds (96 bytes allocated)
65533

julia> @time ack3(4,1)
elapsed time: 0.49337724 seconds (30405308 bytes allocated)
65533
```



## K

See [https://github.com/kevinlawler/kona/wiki the K wiki]

```k
ack:{:[0=x;y+1;0=y;_f[x-1;1];_f[x-1;_f[x;y-1]]]}
ack[2;2]
```



## Kdf9 Usercode


```kdf9 usercode
V6; W0;
YS26000;
RESTART; J999; J999;
PROGRAM;                   (main program);
   V1 = B1212121212121212; (radix 10 for FRB);
   V2 = B2020202020202020; (high bits for decimal digits);
   V3 = B0741062107230637; ("A[3,"  in Flexowriter code);
   V4 = B0727062200250007; ("7] = " in Flexowriter code);
   V5 = B7777777777777777;

      ZERO; NOT; =M1;      (Q1 := 0/0/-1);
      SETAYS0; =M2; I2=2;  (Q2 := 0/2/AYS0: M2 is the stack pointer);
      SET 3; =RC7;         (Q7 := 3/1/0: C7 = m);
      SET 7; =RC8;         (Q8 := 7/1/0: C8 = n);
   JSP1;                   (call Ackermann function);
      V1; REV; FRB;        (convert result to base 10);
      V2; OR;              (convert decimal digits to characters);
      V5; REV;
      SHLD+24; =V5; ERASE; (eliminate leading zeros);
      SETAV5; =RM9;
      SETAV3; =I9;
      POAQ9;               (write result to Flexowriter);

999;  ZERO; OUT;           (terminate run);

P1; (To compute A[m, n]);

   99;
      J1C7NZ;           (to 1 if m ± 0);
         I8; =+C8;      (n := n + 1);
         C8;            (result to NEST);
      EXIT 1;           (return);
   *1;
      J2C8NZ;           (to 2 if n ± 0);
         I8; =C8;       (n := 1);
         DC7;           (m := m - 1);
      J99;              (tail recursion for A[m-1, 1]);
   *2;
         LINK; =M0M2;   (push return address);
         C7; =M0M2QN;   (push m);
         DC8;           (n := n - 1);
      JSP1;             (full recursion for A[m, n-1]);
         =C8;           (n := A[m, n-1]);
         M1M2; =C7;     (m := top of stack);
         DC7;           (m := m - 1);
         M-I2;          (pop stack);
         M0M2; =LINK;   (return address := top of stack);
      J99;              (tail recursion for A[m-1, A[m, n-1]]);

FINISH;
```



## Klong


```k

ack::{:[0=x;y+1:|0=y;.f(x-1;1);.f(x-1;.f(x;y-1))]}
ack(2;2)
```



## Kotlin


```scala
fun A(m: Long, n: Long): Long = when {
    m == 0L -> n + 1
    m > 0L -> when {
        n == 0L -> A(m - 1, 1)
        n > 0L -> A(m - 1, A(m, n - 1))
        else -> throw IllegalArgumentException("illegal n")
    }
    else -> throw IllegalArgumentException("illegal m")
}

fun main(args: Array<String>) {
    val M: Long = 4
    val N: Long = 20
    val r = 0..N
    for (m  in 0..M) {
        print("\nA($m, $r) =")
        var able = true
        r.forEach {
            try {
                if (able) {
                    val a = A(m, it)
                    print(" %6d".format(a))
                } else
                    print("      ?")
            } catch(e: Throwable) {
                print("      ?")
                able = false
            }
        }
    }
}
```

{{out}}

```txt

A(0, 0..20) =      1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21
A(1, 0..20) =      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16     17     18     19     20     21     22
A(2, 0..20) =      3      5      7      9     11     13     15     17     19     21     23     25     27     29     31     33     35     37     39     41     43
A(3, 0..20) =      5     13     29     61    125    253    509   1021   2045   4093   8189  16381  32765      ?      ?      ?      ?      ?      ?      ?      ?
A(4, 0..20) =     13      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?
```



## Lasso


```lasso
#!/usr/bin/lasso9

define ackermann(m::integer, n::integer) => {
  if(#m == 0) => {
    return ++#n
  else(#n == 0)
    return ackermann(--#m, 1)
  else
    return ackermann(#m-1, ackermann(#m, --#n))
  }
}

with x in generateSeries(1,3),
     y in generateSeries(0,8,2)
do stdoutnl(#x+', '#y+': ' + ackermann(#x, #y))

```

{{out}}

```txt
1, 0: 2
1, 2: 4
1, 4: 6
1, 6: 8
1, 8: 10
2, 0: 3
2, 2: 7
2, 4: 11
2, 6: 15
2, 8: 19
3, 0: 5
3, 2: 29
3, 4: 125
3, 6: 509
3, 8: 2045
```



## LFE


```lisp
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
```



## Liberty BASIC


```lb
Print Ackermann(1, 2)

    Function Ackermann(m, n)
        Select Case
            Case (m < 0) Or (n < 0)
                Exit Function
            Case (m = 0)
                Ackermann = (n + 1)
            Case (m > 0) And (n = 0)
                Ackermann = Ackermann((m - 1), 1)
            Case (m > 0) And (n > 0)
                Ackermann = Ackermann((m - 1), Ackermann(m, (n - 1)))
        End Select
    End Function
```



## LiveCode


```LiveCode
function ackermann m,n
    switch
        Case m = 0
            return n + 1
        Case (m > 0 And n = 0)
            return ackermann((m - 1), 1)
        Case (m > 0 And n > 0)
            return ackermann((m - 1), ackermann(m, (n - 1)))
    end switch
end ackermann
```




## Logo


```logo
to ack :i :j
  if :i = 0 [output :j+1]
  if :j = 0 [output ack :i-1 1]
  output ack :i-1 ack :i :j-1
end
```



## Logtalk


```logtalk
ack(0, N, V) :-
    !,
    V is N + 1.
ack(M, 0, V) :-
    !,
    M2 is M - 1,
    ack(M2, 1, V).
ack(M, N, V) :-
    M2 is M - 1,
    N2 is N - 1,
    ack(M, N2, V2),
    ack(M2, V2, V).
```



## LOLCODE

{{trans|C}}

```LOLCODE
HAI 1.3

HOW IZ I ackermann YR m AN YR n
    NOT m, O RLY?
        YA RLY, FOUND YR SUM OF n AN 1
    OIC

    NOT n, O RLY?
        YA RLY, FOUND YR I IZ ackermann YR DIFF OF m AN 1 AN YR 1 MKAY
    OIC

    FOUND YR I IZ ackermann YR DIFF OF m AN 1 AN YR...
     I IZ ackermann YR m AN YR DIFF OF n AN 1 MKAY MKAY
IF U SAY SO

IM IN YR outer UPPIN YR m TIL BOTH SAEM m AN 5
    IM IN YR inner UPPIN YR n TIL BOTH SAEM n AN DIFF OF 6 AN m
        VISIBLE "A(" m ", " n ") = " I IZ ackermann YR m AN YR n MKAY
    IM OUTTA YR inner
IM OUTTA YR outer

KTHXBYE
```



## Lua


```lua
function ack(M,N)
    if M == 0 then return N + 1 end
    if N == 0 then return ack(M-1,1) end
    return ack(M-1,ack(M, N-1))
end
```



## Lucid


```lucid
ack(m,n)
 where
  ack(m,n) = if m eq 0 then n+1
                       else if n eq 0 then ack(m-1,1)
                                      else ack(m-1, ack(m, n-1)) fi
                       fi;
 end
```



## Luck


```luck
function ackermann(m: int, n: int): int = (
   if m==0 then n+1
   else if n==0 then ackermann(m-1,1)
   else ackermann(m-1,ackermann(m,n-1))
)
```




## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Def ackermann(m,n) =If(m=0-> n+1, If(n=0-> ackermann(m-1,1), ackermann(m-1,ackermann(m,n-1))))
      For m = 0 to 3 {For n = 0 to 4 {Print m;" ";n;" ";ackermann(m,n)}}
}
Checkit


Module Checkit {
      Module Inner (ack) {
            For m = 0 to 3 {For n = 0 to 4 {Print m;" ";n;" ";ack(m,n)}}
      }
      Inner lambda (m,n) ->If(m=0-> n+1, If(n=0-> lambda(m-1,1),lambda(m-1,lambda(m,n-1))))
}
Checkit

```



## M4


```M4
define(`ack',`ifelse($1,0,`incr($2)',`ifelse($2,0,`ack(decr($1),1)',`ack(decr($1),ack($1,decr($2)))')')')dnl
ack(3,3)
```

{{out}}

```txt
61
```



## Maple

Strictly by the definition given above, we can code this as follows.

```Maple

Ackermann := proc( m :: nonnegint, n :: nonnegint )
  option remember; # optional automatic memoization
  if m = 0 then
    n + 1
  elif n = 0 then
    thisproc( m - 1, 1 )
  else
    thisproc( m - 1, thisproc( m, n - 1 ) )
  end if
end proc:

```

In Maple, the keyword
```Maple>thisproc</lang
 refers to the currently executing procedure (closure) and is used when writing recursive procedures.  (You could also use the name of the procedure, Ackermann in this case, but then a concurrently executing task or thread could re-assign that name while the recursive procedure is executing, resulting in an incorrect result.)

To make this faster, you can use known expansions for small values of <math>m</math>. (See [[wp:Ackermann_function|Wikipedia:Ackermann function]])

```Maple

Ackermann := proc( m :: nonnegint, n :: nonnegint )
  option remember; # optional automatic memoization
  if m = 0 then
    n + 1
  elif m = 1 then
    n + 2
  elif m = 2 then
    2 * n + 3
  elif m = 3 then
    8 * 2^n - 3
  elif n = 0 then
    thisproc( m - 1, 1 )
  else
    thisproc( m - 1, thisproc( m, n - 1 ) )
  end if
end proc:

```

This makes it possible to compute <code>Ackermann( 4, 1 )</code> and <code>Ackermann( 4, 2 )</code> essentially instantly, though <code>Ackermann( 4, 3 )</code> is still out of reach.

To compute Ackermann( 1, i ) for i from 1 to 10 use

```Maple

> map2( Ackermann, 1, [seq]( 1 .. 10 ) );
               [3, 4, 5, 6, 7, 8, 9, 10, 11, 12]

```

To get the first 10 values for m = 2 use

```Maple

> map2( Ackermann, 2, [seq]( 1 .. 10 ) );
               [5, 7, 9, 11, 13, 15, 17, 19, 21, 23]

```

For Ackermann( 4, 2 ) we get a very long number with

```Maple

> length( Ackermann( 4, 2 ) );
                      19729

```

digits.

=={{header|Mathematica}} / {{header|Wolfram Language}}==
Two possible implementations would be:

```Mathematica
$RecursionLimit=Infinity
Ackermann1[m_,n_]:=
 If[m==0,n+1,
  If[ n==0,Ackermann1[m-1,1],
   Ackermann1[m-1,Ackermann1[m,n-1]]
  ]
 ]

 Ackermann2[0,n_]:=n+1;
 Ackermann2[m_,0]:=Ackermann1[m-1,1];
 Ackermann2[m_,n_]:=Ackermann1[m-1,Ackermann1[m,n-1]]
```

Note that the second implementation is quite a bit faster, as doing 'if' comparisons is slower than the built-in pattern matching algorithms.
Examples:

```Mathematica
Flatten[#,1]&@Table[{"Ackermann2["<>ToString[i]<>","<>ToString[j]<>"] =",Ackermann2[i,j]},{i,3},{j,8}]//Grid
```

gives back:

```Mathematica
Ackermann2[1,1] =	3
Ackermann2[1,2] =	4
Ackermann2[1,3] =	5
Ackermann2[1,4] =	6
Ackermann2[1,5] =	7
Ackermann2[1,6] =	8
Ackermann2[1,7] =	9
Ackermann2[1,8] =	10
Ackermann2[2,1] =	5
Ackermann2[2,2] =	7
Ackermann2[2,3] =	9
Ackermann2[2,4] =	11
Ackermann2[2,5] =	13
Ackermann2[2,6] =	15
Ackermann2[2,7] =	17
Ackermann2[2,8] =	19
Ackermann2[3,1] =	13
Ackermann2[3,2] =	29
Ackermann2[3,3] =	61
Ackermann2[3,4] =	125
Ackermann2[3,5] =	253
Ackermann2[3,6] =	509
Ackermann2[3,7] =	1021
Ackermann2[3,8] =	2045
```

If we would like to calculate Ackermann[4,1] or Ackermann[4,2] we have to optimize a little bit:

```Mathematica
Clear[Ackermann3]
$RecursionLimit=Infinity;
Ackermann3[0,n_]:=n+1;
Ackermann3[1,n_]:=n+2;
Ackermann3[2,n_]:=3+2n;
Ackermann3[3,n_]:=5+8 (2^n-1);
Ackermann3[m_,0]:=Ackermann3[m-1,1];
Ackermann3[m_,n_]:=Ackermann3[m-1,Ackermann3[m,n-1]]
```

Now computing Ackermann[4,1] and Ackermann[4,2] can be done quickly (<0.01 sec):
Examples 2:

```Mathematica
Ackermann3[4, 1]
Ackermann3[4, 2]
```

gives back:
<div style="width:full;overflow:scroll">
```Mathematica
65533
2003529930406846464979072351560255750447825475569751419265016973710894059556311453089506130880........699146577530041384717124577965048175856395072895337539755822087777506072339445587895905719156733
```
</div>
Ackermann[4,2] has 19729 digits, several thousands of digits omitted in the result above for obvious reasons. Ackermann[5,0] can be computed also quite fast, and is equal to 65533.
Summarizing Ackermann[0,n_], Ackermann[1,n_], Ackermann[2,n_], and Ackermann[3,n_] can all be calculated for n>>1000. Ackermann[4,0], Ackermann[4,1], Ackermann[4,2] and Ackermann[5,0]  are only possible now. Maybe in the future we can calculate higher Ackermann numbers efficiently and fast. Although showing the results will always be a problem.


## MATLAB


```MATLAB
function A = ackermannFunction(m,n)
    if m == 0
        A = n+1;
    elseif (m > 0) && (n == 0)
        A = ackermannFunction(m-1,1);
    else
        A = ackermannFunction( m-1,ackermannFunction(m,n-1) );
    end
end
```



## Maxima


```maxima
ackermann(m, n) := if integerp(m) and integerp(n) then ackermann[m, n] else 'ackermann(m, n)$

ackermann[m, n] := if m = 0 then n + 1
                   elseif m = 1 then 2 + (n + 3) - 3
                   elseif m = 2 then 2 * (n + 3) - 3
                   elseif m = 3 then 2^(n + 3) - 3
                   elseif n = 0 then ackermann[m - 1, 1]
                   else ackermann[m - 1, ackermann[m, n - 1]]$

tetration(a, n) := if integerp(n) then block([b: a], for i from 2 thru n do b: a^b, b) else 'tetration(a, n)$

/* this should evaluate to zero */
ackermann(4, n) - (tetration(2, n + 3) - 3);
subst(n = 2, %);
ev(%, nouns);
```



## MAXScript

Use with caution. Will cause a stack overflow for m > 3.

```maxscript
fn ackermann m n =
(
    if m == 0 then
    (
        return n + 1
    )
    else if n == 0 then
    (
        ackermann (m-1) 1
    )
    else
    (
        ackermann (m-1) (ackermann m (n-1))
    )
)
```



## min

{{works with|min|0.19.3}}

```min
(
  :n :m
  (
    ((m 0 ==) (n 1 +))
    ((n 0 ==) (m 1 - 1 ackermann))
    ((true) (m 1 - m n 1 - ackermann ackermann))
  ) case
) :ackermann
```



## MiniScript



```MiniScript
ackermann = function(m, n)
    if m == 0 then return n+1
    if n == 0 then return ackermann(m - 1, 1)
    return ackermann(m - 1, ackermann(m, n - 1))
end function

for m in range(0, 3)
    for n in range(0, 4)
        print "(" + m + ", " + n + "): " + ackermann(m, n)
    end for
end for
```


=={{header|MK-61/52}}==
<lang>П1	<->	П0	ПП	06	С/П	ИП0	x=0	13	ИП1
1	+	В/О	ИП1	x=0	24	ИП0	1	П1	-
П0	ПП	06	В/О	ИП0	П2	ИП1	1	-	П1
ПП	06	П1	ИП2	1	-	П0	ПП	06	В/О
```



## mLite


```haskell
fun ackermann( 0, n ) = n + 1
	| ( m, 0 ) = ackermann( m - 1, 1 )
	| ( m, n ) = ackermann( m - 1, ackermann(m, n - 1) )
```

Test code providing tuples from (0,0) to (3,8)

```haskell
fun jota x = map (fn x = x-1) ` iota x

fun test_tuples (x, y) = append_map (fn a = map (fn b = (b, a)) ` jota x) ` jota y

map ackermann (test_tuples(4,9))
```

Result

```txt
[1, 2, 3, 5, 2, 3, 5, 13, 3, 4, 7, 29, 4, 5, 9, 61, 5, 6, 11, 125, 6, 7, 13, 253, 7, 8, 15, 509, 8, 9, 17, 1021, 9, 10, 19, 2045]
```



## ML/I

ML/I loves recursion, but runs out of its default amount of storage with larger numbers than those tested here!

### Program


```ML/I
MCSKIP "WITH" NL
"" Ackermann function
"" Will overflow when it reaches implementation-defined signed integer limit
MCSKIP MT,<>
MCINS %.
MCDEF ACK WITHS ( , )
AS <MCSET T1=%A1.
MCSET T2=%A2.
MCGO L1 UNLESS T1 EN 0
%%T2.+1.MCGO L0
%L1.MCGO L2 UNLESS T2 EN 0
ACK(%%T1.-1.,1)MCGO L0
%L2.ACK(%%T1.-1.,ACK(%T1.,%%T2.-1.))>
"" Macro ACK now defined, so try it out
a(0,0) => ACK(0,0)
a(0,1) => ACK(0,1)
a(0,2) => ACK(0,2)
a(0,3) => ACK(0,3)
a(0,4) => ACK(0,4)
a(0,5) => ACK(0,5)
a(1,0) => ACK(1,0)
a(1,1) => ACK(1,1)
a(1,2) => ACK(1,2)
a(1,3) => ACK(1,3)
a(1,4) => ACK(1,4)
a(2,0) => ACK(2,0)
a(2,1) => ACK(2,1)
a(2,2) => ACK(2,2)
a(2,3) => ACK(2,3)
a(3,0) => ACK(3,0)
a(3,1) => ACK(3,1)
a(3,2) => ACK(3,2)
a(4,0) => ACK(4,0)
```

{{out}}

```ML/I
a(0,0) => 1
a(0,1) => 2
a(0,2) => 3
a(0,3) => 4
a(0,4) => 5
a(0,5) => 6
a(1,0) => 2
a(1,1) => 3
a(1,2) => 4
a(1,3) => 5
a(1,4) => 6
a(2,0) => 3
a(2,1) => 5
a(2,2) => 7
a(2,3) => 9
a(3,0) => 5
a(3,1) => 13
a(3,2) => 29
a(4,0) => 13
```



## Mercury

This is the Ackermann function with some (obvious) elements elided.  The <code>ack/3</code> predicate is implemented in terms of the <code>ack/2</code> function.  The <code>ack/2</code> function is implemented in terms of the <code>ack/3</code> predicate.  This makes the code both more concise and easier to follow than would otherwise be the case.  The <code>integer</code> type is used instead of <code>int</code> because the problem statement stipulates the use of bignum integers if possible.

```mercury
:- func ack(integer, integer) = integer.
ack(M, N) = R :- ack(M, N, R).

:- pred ack(integer::in, integer::in, integer::out) is det.
ack(M, N, R) :-
	( ( M < integer(0)
	  ; N < integer(0) ) -> throw(bounds_error)
	; M = integer(0)     -> R = N + integer(1)
	; N = integer(0)     -> ack(M - integer(1), integer(1), R)
	;                       ack(M - integer(1), ack(M, N - integer(1)), R) ).
```


=={{header|Modula-2}}==

```modula2
MODULE ackerman;

IMPORT  ASCII, NumConv, InOut;

VAR     m, n            : LONGCARD;
        string          : ARRAY [0..19] OF CHAR;
        OK              : BOOLEAN;

PROCEDURE Ackerman (x, y   : LONGCARD) : LONGCARD;

BEGIN
  IF    x = 0  THEN  RETURN  y + 1
  ELSIF y = 0  THEN  RETURN  Ackerman (x - 1 , 1)
  ELSE
    RETURN  Ackerman (x - 1 , Ackerman (x , y - 1))
  END
END Ackerman;

BEGIN
  FOR  m := 0  TO  3  DO
    FOR  n := 0  TO  6  DO
      NumConv.Num2Str (Ackerman (m, n), 10, string, OK);
      IF  OK  THEN
        InOut.WriteString (string)
      ELSE
        InOut.WriteString ("* Error in number * ")
      END;
      InOut.Write (ASCII.HT)
    END;
    InOut.WriteLn
  END;
  InOut.WriteLn
END ackerman.
```

{{out}}
```txt
jan@Beryllium:~/modula/rosetta$ ackerman
1       2       3       4       5       6       7
2       3       4       5       6       7       8
3       5       7       9       11      13      15
5       13      29      61      125     253     509
```


=={{header|Modula-3}}==
The type CARDINAL is defined in Modula-3 as [0..LAST(INTEGER)], in other words, it can hold all positive integers.

```modula3
MODULE Ack EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Int;

PROCEDURE Ackermann(m, n: CARDINAL): CARDINAL =
  BEGIN
    IF m = 0 THEN
      RETURN n + 1;
    ELSIF n = 0 THEN
      RETURN Ackermann(m - 1, 1);
    ELSE
      RETURN Ackermann(m - 1, Ackermann(m, n - 1));
    END;
  END Ackermann;

BEGIN
  FOR m := 0 TO 3 DO
    FOR n := 0 TO 6 DO
      Put(Int(Ackermann(m, n)) & " ");
    END;
    Put("\n");
  END;
END Ack.
```

{{out}}

```txt
1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509
```



## MUMPS


```MUMPS
Ackermann(m,n)	;
	If m=0 Quit n+1
	If m>0,n=0 Quit $$Ackermann(m-1,1)
	If m>0,n>0 Quit $$Ackermann(m-1,$$Ackermann(m,n-1))
	Set $Ecode=",U13-Invalid parameter for Ackermann: m="_m_", n="_n_","

Write $$Ackermann(1,8) ; 10
Write $$Ackermann(2,8) ; 19
Write $$Ackermann(3,5) ; 253
```



## Nemerle

In Nemerle, we can state the Ackermann function as a lambda. By using pattern-matching, our definition strongly resembles the mathematical notation.

```Nemerle

using System;
using Nemerle.IO;


def ackermann(m, n) {
    def A = ackermann;
    match(m, n) {
        | (0, n) => n + 1
        | (m, 0) when m > 0 => A(m - 1, 1)
        | (m, n) when m > 0 && n > 0 => A(m - 1, A(m, n - 1))
        | _ => throw Exception("invalid inputs");
    }
}


for(mutable m = 0; m < 4; m++) {
    for(mutable n = 0; n < 5; n++) {
        print("ackermann($m, $n) = $(ackermann(m, n))\n");
    }
}

```

A terser version using implicit <code>match</code> (which doesn't use the alias <code>A</code> internally):

```Nemerle

def ackermann(m, n) {
    | (0, n) => n + 1
    | (m, 0) when m > 0 => ackermann(m - 1, 1)
    | (m, n) when m > 0 && n > 0 => ackermann(m - 1, ackermann(m, n - 1))
    | _ => throw Exception("invalid inputs");
}

```

Or, if we were set on using the <code>A</code> notation, we could do this:

```Nemerle

def ackermann = {
    def A(m, n) {
        | (0, n) => n + 1
        | (m, 0) when m > 0 => A(m - 1, 1)
        | (m, n) when m > 0 && n > 0 => A(m - 1, A(m, n - 1))
        | _ => throw Exception("invalid inputs");
    }
    A
}

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

numeric digits 66

parse arg j_ k_ .
if j_ = '' | j_ = '.' | \j_.datatype('w') then j_ = 3
if k_ = '' | k_ = '.' | \k_.datatype('w') then k_ = 5

loop m_ = 0 to j_
  say
  loop n_ = 0 to k_
    say 'ackermann('m_','n_') =' ackermann(m_, n_).right(5)
    end n_
  end m_
return

method ackermann(m, n) public static
  select
    when m = 0 then rval = n + 1
    when n = 0 then rval = ackermann(m - 1, 1)
    otherwise       rval = ackermann(m - 1, ackermann(m, n - 1))
    end
  return rval

```



## NewLISP


```newlisp

#! /usr/local/bin/newlisp

(define (ackermann m n)
  (cond ((zero? m) (inc n))
        ((zero? n) (ackermann (dec m) 1))
        (true (ackermann (- m 1) (ackermann m (dec n))))))

```



```txt

In case of stack overflow error, you have to start your program with a proper "-s <value>" flag
as "newlisp -s 100000 ./ackermann.lsp".
See http://www.newlisp.org/newlisp_manual.html#stack_size

```



## Nim


```nim
from strutils import parseInt

proc ackermann(m, n: int64): int64 =
  if m == 0:
    result = n + 1
  elif n == 0:
    result = ackermann(m - 1, 1)
  else:
    result = ackermann(m - 1, ackermann(m, n - 1))

proc getNumber(): int =
  try:
    result = stdin.readLine.parseInt
  except ValueError:
    echo "An integer, please!"
    result = getNumber()
  if result < 0:
    echo "Please Enter a non-negative Integer: "
    result = getNumber()

echo "First non-negative Integer please: "
let first = getNumber()
echo "Second non-negative Integer please: "
let second = getNumber()
echo "Result: ", $ackermann(first, second)

```



## Nit


Source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/ackermann_function.nit the official Nit’s repository].


```nit
# Task: Ackermann function
#
# A simple straightforward recursive implementation.
module ackermann_function

fun ack(m, n: Int): Int
do
	if m == 0 then return n + 1
	if n == 0 then return ack(m-1,1)
	return ack(m-1, ack(m, n-1))
end

for m in [0..3] do
	for n in [0..6] do
		print ack(m,n)
	end
	print ""
end
```


Output:


```txt
1
2
3
4
5
6
7

2
3
4
5
6
7
8

3
5
7
9
11
13
15

5
13
29
61
125
253
509


```



## Objeck

{{trans|C#|C sharp}}

```objeck
class Ackermann {
  function : Main(args : String[]) ~ Nil {
    for(m := 0; m <= 3; ++m;) {
      for(n := 0; n <= 4; ++n;) {
        a := Ackermann(m, n);
        if(a > 0) {
          "Ackermann({$m}, {$n}) = {$a}"->PrintLine();
        };
      };
    };
  }

  function : Ackermann(m : Int, n : Int) ~ Int {
    if(m > 0) {
      if (n > 0) {
        return Ackermann(m - 1, Ackermann(m, n - 1));
      }
      else if (n = 0) {
        return Ackermann(m - 1, 1);
      };
    }
    else if(m = 0) {
      if(n >= 0) {
        return n + 1;
      };
    };

    return -1;
  }
}
```


```txt

Ackermann(0, 0) = 1
Ackermann(0, 1) = 2
Ackermann(0, 2) = 3
Ackermann(0, 3) = 4
Ackermann(0, 4) = 5
Ackermann(1, 0) = 2
Ackermann(1, 1) = 3
Ackermann(1, 2) = 4
Ackermann(1, 3) = 5
Ackermann(1, 4) = 6
Ackermann(2, 0) = 3
Ackermann(2, 1) = 5
Ackermann(2, 2) = 7
Ackermann(2, 3) = 9
Ackermann(2, 4) = 11
Ackermann(3, 0) = 5
Ackermann(3, 1) = 13
Ackermann(3, 2) = 29
Ackermann(3, 3) = 61
Ackermann(3, 4) = 125

```



## OCaml


```ocaml
let rec a m n =
  if m=0 then (n+1) else
  if n=0 then (a (m-1) 1) else
  (a (m-1) (a m (n-1)))
```

or:

```ocaml
let rec a = function
  | 0, n -> (n+1)
  | m, 0 -> a(m-1, 1)
  | m, n -> a(m-1, a(m, n-1))
```

with memoization using an hash-table:

```ocaml
let h = Hashtbl.create 4001

let a m n =
  try Hashtbl.find h (m, n)
  with Not_found ->
    let res = a (m, n) in
    Hashtbl.add h (m, n) res;
    (res)
```

taking advantage of the memoization we start calling small values of '''m''' and '''n''' in order to reduce the recursion call stack:

```ocaml
let a m n =
  for _m = 0 to m do
    for _n = 0 to n do
      ignore(a _m _n);
    done;
  done;
  (a m n)
```


###  Arbitrary precision

With arbitrary-precision integers ([http://caml.inria.fr/pub/docs/manual-ocaml/libref/Big_int.html Big_int module]):

```ocaml
open Big_int
let one  = unit_big_int
let zero = zero_big_int
let succ = succ_big_int
let pred = pred_big_int
let eq = eq_big_int

let rec a m n =
  if eq m zero then (succ n) else
  if eq n zero then (a (pred m) one) else
  (a (pred m) (a m (pred n)))
```

compile with:
 ocamlopt -o acker nums.cmxa acker.ml
=== Tail-Recursive ===
Here is a [[:Category:Recursion|tail-recursive]] version:

```ocaml
let rec find_option h v =
  try Some(Hashtbl.find h v)
  with Not_found -> None

let rec a bounds caller todo m n =
  match m, n with
  | 0, n ->
      let r = (n+1) in
      ( match todo with
        | [] -> r
        | (m,n)::todo ->
            List.iter (fun k ->
              if not(Hashtbl.mem bounds k)
              then Hashtbl.add bounds k r) caller;
            a bounds [] todo m n )

  | m, 0 ->
      a bounds caller todo (m-1) 1

  | m, n ->
      match find_option bounds (m, n-1) with
      | Some a_rec ->
          let caller = (m,n)::caller in
          a bounds caller todo (m-1) a_rec
      | None ->
          let todo = (m,n)::todo
          and caller = [(m, n-1)] in
          a bounds caller todo m (n-1)

let a = a (Hashtbl.create 42 (* arbitrary *) ) [] [] ;;
```

This one uses the arbitrary precision, the tail-recursion, and the optimisation explain on the Wikipedia page about <tt>(m,n) = (3,_)</tt>.

```ocaml
open Big_int
let one  = unit_big_int
let zero = zero_big_int
let succ = succ_big_int
let pred = pred_big_int
let add = add_big_int
let sub = sub_big_int
let eq = eq_big_int
let three = succ(succ one)
let power = power_int_positive_big_int

let eq2 (a1,a2) (b1,b2) =
  (eq a1 b1) && (eq a2 b2)

module H = Hashtbl.Make
  (struct
     type t = Big_int.big_int * Big_int.big_int
     let equal = eq2
     let hash (x,y) = Hashtbl.hash
       (Big_int.string_of_big_int x ^ "," ^
          Big_int.string_of_big_int y)
       (* probably not a very good hash function *)
   end)

let rec find_option h v =
  try Some (H.find h v)
  with Not_found -> None

let rec a bounds caller todo m n =
  let may_tail r =
    let k = (m,n) in
    match todo with
    | [] -> r
    | (m,n)::todo ->
        List.iter (fun k ->
                     if not (H.mem bounds k)
                     then H.add bounds k r) (k::caller);
        a bounds [] todo m n
  in
  match m, n with
  | m, n when eq m zero ->
      let r = (succ n) in
      may_tail r

  | m, n when eq n zero ->
      let caller = (m,n)::caller in
      a bounds caller todo (pred m) one

  | m, n when eq m three ->
      let r = sub (power 2 (add n three)) three in
      may_tail r

  | m, n ->
      match find_option bounds (m, pred n) with
      | Some a_rec ->
          let caller = (m,n)::caller in
          a bounds caller todo (pred m) a_rec
      | None ->
          let todo = (m,n)::todo in
          let caller = [(m, pred n)] in
          a bounds caller todo m (pred n)

let a = a (H.create 42 (* arbitrary *)) [] [] ;;

let () =
  let m, n =
    try
      big_int_of_string Sys.argv.(1),
      big_int_of_string Sys.argv.(2)
    with _ ->
      Printf.eprintf "usage: %s <int> <int>\n" Sys.argv.(0);
      exit 1
  in
  let r = a m n in
  Printf.printf "(a %s %s) = %s\n"
      (string_of_big_int m)
      (string_of_big_int n)
      (string_of_big_int r);
;;
```


=={{header|Oberon-2}}==

```oberon2
MODULE ackerman;

IMPORT  Out;

VAR     m, n    : INTEGER;

PROCEDURE Ackerman (x, y   : INTEGER) : INTEGER;

BEGIN
  IF    x = 0  THEN  RETURN  y + 1
  ELSIF y = 0  THEN  RETURN  Ackerman (x - 1 , 1)
  ELSE
    RETURN  Ackerman (x - 1 , Ackerman (x , y - 1))
  END
END Ackerman;

BEGIN
  FOR  m := 0  TO  3  DO
    FOR  n := 0  TO  6  DO
      Out.Int (Ackerman (m, n), 10);
      Out.Char (9X)
    END;
    Out.Ln
  END;
  Out.Ln
END ackerman.
```



## Octave


```octave
function r = ackerman(m, n)
  if ( m == 0 )
    r = n + 1;
  elseif ( n == 0 )
    r = ackerman(m-1, 1);
  else
    r = ackerman(m-1, ackerman(m, n-1));
  endif
endfunction

for i = 0:3
  disp(ackerman(i, 4));
endfor
```



## Oforth


```Oforth
: A( m n -- p )
   m ifZero: [ n 1+ return ]
   m 1- n ifZero: [ 1 ] else: [ A( m, n 1- ) ] A
;
```



## OOC


```ooc

ack: func (m: Int, n: Int) -> Int {
  if (m == 0) {
    n + 1
  } else if (n == 0) {
    ack(m - 1, 1)
  } else {
    ack(m - 1, ack(m, n - 1))
  }
}

main: func {
  for (m in 0..4) {
    for (n in 0..10) {
      "ack(#{m}, #{n}) = #{ack(m, n)}" println()
    }
  }
}

```



## ooRexx


```ooRexx

loop m = 0 to 3
    loop n = 0 to 6
        say "Ackermann("m", "n") =" ackermann(m, n)
    end
end

::routine ackermann
  use strict arg m, n
  -- give us some precision room
  numeric digits 10000
  if m = 0 then return n + 1
  else if n = 0 then return ackermann(m - 1, 1)
  else return ackermann(m - 1, ackermann(m, n - 1))

```

{{out}}

```txt

Ackermann(0, 0) = 1
Ackermann(0, 1) = 2
Ackermann(0, 2) = 3
Ackermann(0, 3) = 4
Ackermann(0, 4) = 5
Ackermann(0, 5) = 6
Ackermann(0, 6) = 7
Ackermann(1, 0) = 2
Ackermann(1, 1) = 3
Ackermann(1, 2) = 4
Ackermann(1, 3) = 5
Ackermann(1, 4) = 6
Ackermann(1, 5) = 7
Ackermann(1, 6) = 8
Ackermann(2, 0) = 3
Ackermann(2, 1) = 5
Ackermann(2, 2) = 7
Ackermann(2, 3) = 9
Ackermann(2, 4) = 11
Ackermann(2, 5) = 13
Ackermann(2, 6) = 15
Ackermann(3, 0) = 5
Ackermann(3, 1) = 13
Ackermann(3, 2) = 29
Ackermann(3, 3) = 61
Ackermann(3, 4) = 125
Ackermann(3, 5) = 253
Ackermann(3, 6) = 509

```



## Order


```c
#include <order/interpreter.h>


#define ORDER_PP_DEF_8ack ORDER_PP_FN(    \
8fn(8X, 8Y,                               \
    8cond((8is_0(8X), 8inc(8Y))           \
          (8is_0(8Y), 8ack(8dec(8X), 1))  \
          (8else, 8ack(8dec(8X), 8ack(8X, 8dec(8Y)))))))

ORDER_PP(8to_lit(8ack(3, 4)))      // 125
```



## Oz

Oz has arbitrary precision integers.

```oz
declare

  fun {Ack M N}
     if     M == 0 then N+1
     elseif N == 0 then {Ack M-1 1}
     else               {Ack M-1 {Ack M N-1}}
     end
  end

in

  {Show {Ack 3 7}}
```



## PARI/GP

Naive implementation.

```parigp
A(m,n)={
  if(m,
    if(n,
      A(m-1, A(m,n-1))
    ,
      A(m-1,1)
    )
  ,
    n+1
  )
};
```



## Pascal


```pascal
Program Ackerman;

function ackermann(m, n: Integer) : Integer;
begin
   if m = 0 then
      ackermann := n+1
   else if n = 0 then
      ackermann := ackermann(m-1, 1)
   else
      ackermann := ackermann(m-1, ackermann(m, n-1));
end;

var
   m, n	: Integer;

begin
   for n := 0 to 6 do
      for m := 0 to 3 do
	 WriteLn('A(', m, ',', n, ') = ', ackermann(m,n));
end.
```



## Perl

We memoize calls to ''A'' to make ''A''(2, ''n'') and ''A''(3, ''n'') feasible for larger values of ''n''.

```perl
{
    my @memo;
    sub A {
        my( $m, $n ) = @_;
        $memo[ $m ][ $n ] and return $memo[ $m ][ $n ];
        $m or return $n + 1;
        return $memo[ $m ][ $n ] = (
            $n
               ? A( $m - 1, A( $m, $n - 1 ) )
               : A( $m - 1, 1 )
        );
    }
}
```


An implementation using the conditional statements 'if', 'elsif' and 'else':

```perl
sub A {
    my ($m, $n) = @_;
    if    ($m == 0) { $n + 1 }
    elsif ($n == 0) { A($m - 1, 1) }
    else            { A($m - 1, A($m, $n - 1)) }
}
```


An implementation using ternary chaining:

```perl
sub A {
  my ($m, $n) = @_;
  $m == 0 ? $n + 1 :
  $n == 0 ? A($m - 1, 1) :
            A($m - 1, A($m, $n - 1))
}
```


Adding memoization and extra terms:

```perl
use Memoize;  memoize('ack2');
use bigint try=>"GMP";

sub ack2 {
   my ($m, $n) = @_;
   $m == 0 ? $n + 1 :
   $m == 1 ? $n + 2 :
   $m == 2 ? 2*$n + 3 :
   $m == 3 ? 8 * (2**$n - 1) + 5 :
   $n == 0 ? ack2($m-1, 1)
           : ack2($m-1, ack2($m, $n-1));
}
print "ack2(3,4) is ", ack2(3,4), "\n";
print "ack2(4,1) is ", ack2(4,1), "\n";
print "ack2(4,2) has ", length(ack2(4,2)), " digits\n";
```

{{output}}

```txt
ack2(3,4) is 125
ack2(4,1) is 65533
ack2(4,2) has 19729 digits
```


An optimized version, which uses <code>@_</code> as a stack,
instead of recursion.  Very fast.

```Perl
use strict;
use warnings;
use Math::BigInt;

use constant two => Math::BigInt->new(2);

sub ack {
	my $n = pop;
	while( @_ ) {
		my $m = pop;
		if( $m > 3 ) {
			push @_, (--$m) x $n;
			push @_, reverse 3 .. --$m;
			$n = 13;
		} elsif( $m == 3 ) {
			if( $n < 29 ) {
				$n = ( 1 << ( $n + 3 ) ) - 3;
			} else {
				$n = two ** ( $n + 3 ) - 3;
			}
		} elsif( $m == 2 ) {
			$n = 2 * $n + 3;
		} elsif( $m >= 0 ) {
			$n = $n + $m + 1;
		} else {
			die "negative m!";
		}
	}
	$n;
}

print "ack(3,4) is ", ack(3,4), "\n";
print "ack(4,1) is ", ack(4,1), "\n";
print "ack(4,2) has ", length(ack(4,2)), " digits\n";


```



## Perl 6

{{works with|Rakudo|2018.03}}


```perl6
sub A(Int $m, Int $n) {
    if    $m == 0 { $n + 1 }
    elsif $n == 0 { A($m - 1, 1) }
    else          { A($m - 1, A($m, $n - 1)) }
}
```

An implementation using multiple dispatch:

```perl6
multi sub A(0,      Int $n) { $n + 1                   }
multi sub A(Int $m, 0     ) { A($m - 1, 1)             }
multi sub A(Int $m, Int $n) { A($m - 1, A($m, $n - 1)) }
```

Note that in either case, Int is defined to be arbitrary precision in Perl 6.

Here's a caching version of that, written in the sigilless style, with liberal use of Unicode, and the extra optimizing terms to make A(4,2) possible:

```perl6
proto A(Int \𝑚, Int \𝑛) { (state @)[𝑚][𝑛] //= {*} }

multi A(0,      Int \𝑛) { 𝑛 + 1 }
multi A(1,      Int \𝑛) { 𝑛 + 2 }
multi A(2,      Int \𝑛) { 3 + 2 * 𝑛 }
multi A(3,      Int \𝑛) { 5 + 8 * (2 ** 𝑛 - 1) }

multi A(Int \𝑚, 0     ) { A(𝑚 - 1, 1) }
multi A(Int \𝑚, Int \𝑛) { A(𝑚 - 1, A(𝑚, 𝑛 - 1)) }

# Testing:
say A(4,1);
say .chars, " digits starting with ", .substr(0,50), "..." given A(4,2);
```

{{out}}

```txt
65533
19729 digits starting with 20035299304068464649790723515602557504478254755697...
```



## Phix


###  native version


```Phix
function ack(integer m, integer n)
    if m=0 then
        return n+1
    elsif m=1 then
        return n+2
    elsif m=2 then
        return 2*n+3
    elsif m=3 then
        return power(2,n+3)-3
    elsif m>0 and n=0 then
        return ack(m-1,1)
    else
        return ack(m-1,ack(m,n-1))
    end if
end function

constant limit = 23,
         fmtlens = {1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7,7,7,8,8,8}

atom t0 = time()
printf(1,"   0")
for j=1 to limit do
    string fmt = sprintf(" %%%dd",fmtlens[j+1])
    printf(1,fmt,j)
end for
printf(1,"\n")
for i=0 to 5 do
    printf(1,"%d:",i)
    for j=0 to iff(i>=4?5-i:limit) do
        string fmt = sprintf(" %%%dd",fmtlens[j+1])
        printf(1,fmt,{ack(i,j)})
    end for
    printf(1,"\n")
end for
```

{{out}}

```txt

   0  1  2  3   4   5   6    7    8    9   10    11    12    13     14     15     16      17      18      19      20       21       22       23
0: 1  2  3  4   5   6   7    8    9   10   11    12    13    14     15     16     17      18      19      20      21       22       23       24
1: 2  3  4  5   6   7   8    9   10   11   12    13    14    15     16     17     18      19      20      21      22       23       24       25
2: 3  5  7  9  11  13  15   17   19   21   23    25    27    29     31     33     35      37      39      41      43       45       47       49
3: 5 13 29 61 125 253 509 1021 2045 4093 8189 16381 32765 65533 131069 262141 524285 1048573 2097149 4194301 8388605 16777213 33554429 67108861
4: 13 65533
5: 65533

```

ack(4,2) and above fail with power function overflow. ack(3,100) will get you an answer, but only accurate to 16 or so digits.


###  gmp version

{{trans|Go}}
{{libheader|mpfr}}

```Phix
-- demo\rosetta\Ackermann.exw
include mpfr.e

procedure ack(integer m, mpz n)
    if m=0 then
        mpz_add_ui(n, n, 1)                     -- return n+1
    elsif m=1 then
        mpz_add_ui(n, n, 2)                     -- return n+2
    elsif m=2 then
        mpz_mul_si(n, n, 2)
        mpz_add_ui(n, n, 3)                     -- return 2*n+3
    elsif m=3 then
        if not mpz_fits_integer(n) then
            -- As per Go: 2^MAXINT would most certainly run out of memory.
            -- (think about it: a million digits is fine but pretty daft;
            --  however a billion digits requires > addressable memory.)
            integer bn = mpz_sizeinbase(n, 2)
            throw(sprintf("A(m,n) had n of %d bits; too large",bn))
        end if
        integer ni = mpz_get_integer(n)
        mpz_set_si(n, 8)
        mpz_mul_2exp(n, n, ni) -- (n:=8*2^ni)
        mpz_sub_ui(n, n, 3)                     -- return power(2,n+3)-3
    elsif mpz_cmp_si(n,0)=0 then
        mpz_set_si(n, 1)
        ack(m-1,n)                              -- return ack(m-1,1)
    else
        mpz_sub_ui(n, n, 1)
        ack(m,n)
        ack(m-1,n)                              -- return ack(m-1,ack(m,n-1))
    end if
end procedure

constant limit = 23,
         fmtlens = {1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7,7,7,8,8,8},
         extras = {{3,100},{3,1e6},{4,2},{4,3}}

procedure ackermann_tests()
    atom t0 = time()
    atom n = mpz_init()
    printf(1,"   0")
    for j=1 to limit do
        string fmt = sprintf(" %%%dd",fmtlens[j+1])
        printf(1,fmt,j)
    end for
    printf(1,"\n")
    for i=0 to 5 do
        printf(1,"%d:",i)
        for j=0 to iff(i>=4?5-i:limit) do
            mpz_set_si(n, j)
            ack(i,n)
            string fmt = sprintf(" %%%ds",fmtlens[j+1])
            printf(1,fmt,{mpz_get_str(n)})
        end for
        printf(1,"\n")
    end for
    printf(1,"\n")
    for i=1 to length(extras) do
        integer {em, en} = extras[i]
        mpz_set_si(n, en)
        string res
        try
            ack(em,n)
            res = mpz_get_str(n)
            integer lr = length(res)
            if lr>50 then
                res[21..-21] = "..."
                res &= sprintf(" (%d digits)",lr)
            end if
        catch e
            -- ack(4,3), ack(5,1) and ack(6,0) all fail,
            --                   just as they should do
            res = "***ERROR***: "&e[E_USER]
        end try
        printf(1,"ack(%d,%d) %s\n",{em,en,res})
    end for
    n = mpz_free(n)
    printf(1,"\n")
    printf(1,"ackermann_tests completed (%s)\n\n",{elapsed(time()-t0)})
end procedure

ackermann_tests()
```

{{out}}

```txt

   0  1  2  3   4   5   6    7    8    9   10    11    12    13     14     15     16      17      18      19      20       21       22       23
0: 1  2  3  4   5   6   7    8    9   10   11    12    13    14     15     16     17      18      19      20      21       22       23       24
1: 2  3  4  5   6   7   8    9   10   11   12    13    14    15     16     17     18      19      20      21      22       23       24       25
2: 3  5  7  9  11  13  15   17   19   21   23    25    27    29     31     33     35      37      39      41      43       45       47       49
3: 5 13 29 61 125 253 509 1021 2045 4093 8189 16381 32765 65533 131069 262141 524285 1048573 2097149 4194301 8388605 16777213 33554429 67108861
4: 13 65533
5: 65533

ack(3,100) 10141204801825835211973625643005
ack(3,1000000) 79205249834367186005...39107225301976875005 (301031 digits)
ack(4,2) 20035299304068464649...45587895905719156733 (19729 digits)
ack(4,3) ***ERROR***: A(m,n) had n of 65536 bits; too large

ackermann_tests completed (0.2s)

```



## PHP


```php
function ackermann( $m , $n )
{
    if ( $m==0 )
    {
        return $n + 1;
    }
    elseif ( $n==0 )
    {
        return ackermann( $m-1 , 1 );
    }
    return ackermann( $m-1, ackermann( $m , $n-1 ) );
}

echo ackermann( 3, 4 );
// prints 125
```



## PicoLisp


```PicoLisp
(de ack (X Y)
   (cond
      ((=0 X) (inc Y))
      ((=0 Y) (ack (dec X) 1))
      (T (ack (dec X) (ack X (dec Y)))) ) )
```



## Piet


Rendered as wikitable:

{| style="border-collapse: collapse; border-spacing: 0; font-family: courier-new,courier,monospace; font-size: 10px; line-height: 1.2em; padding: 0px"
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#c00000; color:#c00000;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#00ffff; color:#00ffff;" | ww
| style="background-color:#00ffff; color:#00ffff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#c0ffff; color:#c0ffff;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#c00000; color:#c00000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#c0c000; color:#c0c000;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c00000; color:#c00000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c00000; color:#c00000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#00ffff; color:#00ffff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0ffff; color:#c0ffff;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#00ffff; color:#00ffff;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffc0ff; color:#ffc0ff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#00c000; color:#00c000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0c000; color:#c0c000;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c00000; color:#c00000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#c0ffff; color:#c0ffff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ffc0ff; color:#ffc0ff;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#c0c000; color:#c0c000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffc0ff; color:#ffc0ff;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#c0ffff; color:#c0ffff;" | ww
| style="background-color:#00c000; color:#00c000;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#c0ffff; color:#c0ffff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
|-

|}

This is a naive implementation that does not use any optimization. Find the explanation at [[http://rosettacode.org/wiki/User:Albedo]]. Computing the Ackermann function for (4,1) is possible, but takes quite a while because the stack grows very fast to large dimensions.

Example output:

    ? 3
    ? 5
    253


## Pike


```pike
int main(){
   write(ackermann(3,4) + "\n");
}

int ackermann(int m, int n){
   if(m == 0){
      return n + 1;
   } else if(n == 0){
      return ackermann(m-1, 1);
   } else {
      return ackermann(m-1, ackermann(m, n-1));
   }
}
```



## PL/I


```PL/I
Ackerman: procedure (m, n) returns (fixed (30)) recursive;
   declare (m, n) fixed (30);
   if m = 0 then return (n+1);
   else if m > 0 & n = 0 then return (Ackerman(m-1, 1));
   else if m > 0 & n > 0 then return (Ackerman(m-1, Ackerman(m, n-1)));
   return (0);
end Ackerman;
```



## PL/SQL


```PLSQL
DECLARE

  FUNCTION ackermann(pi_m IN NUMBER,
                     pi_n IN NUMBER) RETURN NUMBER IS
  BEGIN
    IF pi_m = 0 THEN
      RETURN pi_n + 1;
    ELSIF pi_n = 0 THEN
      RETURN ackermann(pi_m - 1, 1);
    ELSE
      RETURN ackermann(pi_m - 1, ackermann(pi_m, pi_n - 1));
    END IF;
  END ackermann;

BEGIN
  FOR n IN 0 .. 6 LOOP
    FOR m IN 0 .. 3 LOOP
      dbms_output.put_line('A(' || m || ',' || n || ') = ' || ackermann(m, n));
    END LOOP;
  END LOOP;
END;

```

{{out}}

```txt

A(0,0) = 1
A(1,0) = 2
A(2,0) = 3
A(3,0) = 5
A(0,1) = 2
A(1,1) = 3
A(2,1) = 5
A(3,1) = 13
A(0,2) = 3
A(1,2) = 4
A(2,2) = 7
A(3,2) = 29
A(0,3) = 4
A(1,3) = 5
A(2,3) = 9
A(3,3) = 61
A(0,4) = 5
A(1,4) = 6
A(2,4) = 11
A(3,4) = 125
A(0,5) = 6
A(1,5) = 7
A(2,5) = 13
A(3,5) = 253
A(0,6) = 7
A(1,6) = 8
A(2,6) = 15
A(3,6) = 509

```



## PostScript


```postscript
/ackermann{
/n exch def
/m exch def %PostScript takes arguments in the reverse order as specified in the function definition
m 0 eq{
n 1 add
}if
m 0 gt n 0 eq and
{
m 1 sub 1 ackermann
}if
m 0 gt n 0 gt and{
m 1 sub m n 1 sub ackermann ackermann
}if
}def
```

{{libheader|initlib}}

```postscript
/A {
[/.m /.n] let
{
    {.m 0 eq} {.n succ} is?
    {.m 0 gt .n 0 eq and} {.m pred 1 A} is?
    {.m 0 gt .n 0 gt and} {.m pred .m .n pred A A} is?
} cond
end}.
```



## Potion


```Potion
ack = (m, n):
  if (m == 0): n + 1
. elsif (n == 0): ack(m - 1, 1)
. else: ack(m - 1, ack(m, n - 1)).
.

4 times(m):
  7 times(n):
    ack(m, n) print
    " " print.
  "\n" print.
```



## PowerBASIC


```powerbasic
FUNCTION PBMAIN () AS LONG
    DIM m AS QUAD, n AS QUAD

    m = ABS(VAL(INPUTBOX$("Enter a whole number.")))
    n = ABS(VAL(INPUTBOX$("Enter another whole number.")))

    MSGBOX STR$(Ackermann(m, n))
END FUNCTION

FUNCTION Ackermann (m AS QUAD, n AS QUAD) AS QUAD
    IF 0 = m THEN
        FUNCTION = n + 1
    ELSEIF 0 = n THEN
        FUNCTION = Ackermann(m - 1, 1)
    ELSE    ' m > 0; n > 0
        FUNCTION = Ackermann(m - 1, Ackermann(m, n - 1))
    END IF
END FUNCTION
```




## PowerShell

{{trans|PHP}}

```powershell
function ackermann ([long] $m, [long] $n) {
    if ($m -eq 0) {
        return $n + 1
    }

    if ($n -eq 0) {
        return (ackermann ($m - 1) 1)
    }

    return (ackermann ($m - 1) (ackermann $m ($n - 1)))
}
```

Building an example table (takes a while to compute, though, especially for the last three numbers; also it fails with the last line in Powershell v1 since the maximum recursion depth is only 100 there):

```powershell
foreach ($m in 0..3) {
    foreach ($n in 0..6) {
        Write-Host -NoNewline ("{0,5}" -f (ackermann $m $n))
    }
    Write-Host
}
```

{{out}}

```txt
    1    2    3    4    5    6    7
    2    3    4    5    6    7    8
    3    5    7    9   11   13   15
    5   13   29   61  125  253  509
```


===A More "PowerShelly" Way===

```PowerShell

function Get-Ackermann ([int64]$m, [int64]$n)
{
    if ($m -eq 0)
    {
        return $n + 1
    }

    if ($n -eq 0)
    {
        return Get-Ackermann ($m - 1) 1
    }

    return (Get-Ackermann ($m - 1) (Get-Ackermann $m ($n - 1)))
}

```

Save the result to an array (for possible future use?), then display it using the <code>Format-Wide</code> cmdlet:

```PowerShell

$ackermann = 0..3 | ForEach-Object {$m = $_; 0..6 | ForEach-Object {Get-Ackermann $m  $_}}

$ackermann | Format-Wide {"{0,3}" -f $_} -Column 7 -Force

```

{{Out}}

```txt

  1                   2                  3                  4                  5                  6                  7
  2                   3                  4                  5                  6                  7                  8
  3                   5                  7                  9                 11                 13                 15
  5                  13                 29                 61                125                253                509

```



## Processing


```java
int ackermann(int m, n)
{
   if (m == 0)
      return n + 1;
   else if (m > 0 && n == 0)
      return ackermann(m - 1, 1);
   else
      return ackermann( m - 1, ackermann(m, n - 1) );
}
```



## Prolog

{{works with|SWI Prolog}}

```prolog
:- table ack/3. % memoization reduces the execution time of ack(4,1,X) from several
                % minutes to about one second on a typical desktop computer.
ack(0, N, Ans) :- Ans is N+1.
ack(M, 0, Ans) :- M>0, X is M-1, ack(X, 1, Ans).
ack(M, N, Ans) :- M>0, N>0, X is M-1, Y is N-1, ack(M, Y, Ans2), ack(X, Ans2, Ans).
```



## Pure


```pure
A 0 n = n+1;
A m 0 = A (m-1) 1 if m > 0;
A m n = A (m-1) (A m (n-1)) if m > 0 && n > 0;
```



## PureBasic


```PureBasic
Procedure.q Ackermann(m, n)
  If m = 0
    ProcedureReturn n + 1
  ElseIf  n = 0
    ProcedureReturn Ackermann(m - 1, 1)
  Else
    ProcedureReturn Ackermann(m - 1, Ackermann(m, n - 1))
  EndIf
EndProcedure

Debug Ackermann(3,4)
```



## Pure Data


```Pure Data

#N canvas 741 265 450 436 10;
#X obj 83 111 t b l;
#X obj 115 163 route 0;
#X obj 115 185 + 1;
#X obj 83 380 f;
#X obj 161 186 swap;
#X obj 161 228 route 0;
#X obj 161 250 - 1;
#X obj 161 208 pack;
#X obj 115 314 t f f;
#X msg 161 272 \$1 1;
#X obj 115 142 t l;
#X obj 207 250 swap;
#X obj 273 271 - 1;
#X obj 207 272 t f f;
#X obj 207 298 - 1;
#X obj 207 360 pack;
#X obj 239 299 pack;
#X obj 83 77 inlet;
#X obj 83 402 outlet;
#X connect 0 0 3 0;
#X connect 0 1 10 0;
#X connect 1 0 2 0;
#X connect 1 1 4 0;
#X connect 2 0 8 0;
#X connect 3 0 18 0;
#X connect 4 0 7 0;
#X connect 4 1 7 1;
#X connect 5 0 6 0;
#X connect 5 1 11 0;
#X connect 6 0 9 0;
#X connect 7 0 5 0;
#X connect 8 0 3 1;
#X connect 8 1 15 1;
#X connect 9 0 10 0;
#X connect 10 0 1 0;
#X connect 11 0 13 0;
#X connect 11 1 12 0;
#X connect 12 0 16 1;
#X connect 13 0 14 0;
#X connect 13 1 16 0;
#X connect 14 0 15 0;
#X connect 15 0 10 0;
#X connect 16 0 10 0;
#X connect 17 0 0 0;
```



## Purity


```Purity>data Iter = f =
 FoldNat <const $f One, $f>
data Ackermann = FoldNat <const Succ, Iter>
```



## Python

{{works with|Python|2.5}}

```python
def ack1(M, N):
   return (N + 1) if M == 0 else (
      ack1(M-1, 1) if N == 0 else ack1(M-1, ack1(M, N-1)))
```

Another version:

```python
from functools import lru_cache

@lru_cache(None)
def ack2(M, N):
    if M == 0:
        return N + 1
    elif N == 0:
        return ack2(M - 1, 1)
    else:
        return ack2(M - 1, ack2(M, N - 1))
```

{{out|Example of use}}

```python>>>
 import sys
>>> sys.setrecursionlimit(3000)
>>> ack1(0,0)
1
>>> ack1(3,4)
125
>>> ack2(0,0)
1
>>> ack2(3,4)
125
```

From the Mathematica ack3 example:

```python
def ack2(M, N):
   return (N + 1)   if M == 0 else (
          (N + 2)   if M == 1 else (
          (2*N + 3) if M == 2 else (
          (8*(2**N - 1) + 5) if M == 3 else (
          ack2(M-1, 1) if N == 0 else ack2(M-1, ack2(M, N-1))))))
```

Results confirm those of Mathematica for ack(4,1) and ack(4,2)


## R


```R
ackermann <- function(m, n) {
  if ( m == 0 ) {
    n+1
  } else if ( n == 0 ) {
    ackermann(m-1, 1)
  } else {
    ackermann(m-1, ackermann(m, n-1))
  }
}
```


```R
for ( i in 0:3 ) {
  print(ackermann(i, 4))
}
```



## Racket


```racket

#lang racket
(define (ackermann m n)
  (cond [(zero? m) (add1 n)]
        [(zero? n) (ackermann (sub1 m) 1)]
        [else (ackermann (sub1 m) (ackermann m (sub1 n)))]))

```



## REBOL


```txt
ackermann: func [m n] [
    case [
        m = 0 [n + 1]
        n = 0 [ackermann m - 1 1]
        true [ackermann m - 1 ackermann m n - 1]
    ]
]
```



## REXX


### no optimization


```rexx
/*REXX program  calculates and displays  some values for the  Ackermann function.       */
            /*╔════════════════════════════════════════════════════════════════════════╗
              ║  Note:  the Ackermann function  (as implemented here)  utilizes deep   ║
              ║         recursive and is limited by the largest number that can have   ║
              ║         "1"  (unity) added to a number  (successfully and accurately). ║
              ╚════════════════════════════════════════════════════════════════════════╝*/
high=24
         do     j=0  to 3;                    say
             do k=0  to high % (max(1, j))
             call tell_Ack  j, k
             end   /*k*/
         end       /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell_Ack:  parse arg mm,nn;   calls=0            /*display an echo message to terminal. */
           #=right(nn,length(high))
           say 'Ackermann('mm", "#')='right(ackermann(mm, nn), high),
                                      left('', 12)     'calls='right(calls, high)
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
ackermann: procedure expose calls                /*compute value of Ackermann function. */
           parse arg m,n;   calls=calls+1
           if m==0  then return n+1
           if n==0  then return ackermann(m-1, 1)
                         return ackermann(m-1, ackermann(m, n-1) )
```

'''output'''
<pre style="height:60ex">
Ackermann(0, 0)=                       1              calls=                       1
Ackermann(0, 1)=                       2              calls=                       1
Ackermann(0, 2)=                       3              calls=                       1
Ackermann(0, 3)=                       4              calls=                       1
Ackermann(0, 4)=                       5              calls=                       1
Ackermann(0, 5)=                       6              calls=                       1
Ackermann(0, 6)=                       7              calls=                       1
Ackermann(0, 7)=                       8              calls=                       1
Ackermann(0, 8)=                       9              calls=                       1
Ackermann(0, 9)=                      10              calls=                       1
Ackermann(0,10)=                      11              calls=                       1
Ackermann(0,11)=                      12              calls=                       1
Ackermann(0,12)=                      13              calls=                       1
Ackermann(0,13)=                      14              calls=                       1
Ackermann(0,14)=                      15              calls=                       1
Ackermann(0,15)=                      16              calls=                       1
Ackermann(0,16)=                      17              calls=                       1
Ackermann(0,17)=                      18              calls=                       1
Ackermann(0,18)=                      19              calls=                       1
Ackermann(0,19)=                      20              calls=                       1
Ackermann(0,20)=                      21              calls=                       1
Ackermann(0,21)=                      22              calls=                       1
Ackermann(0,22)=                      23              calls=                       1
Ackermann(0,23)=                      24              calls=                       1
Ackermann(0,24)=                      25              calls=                       1

Ackermann(1, 0)=                       2              calls=                       2
Ackermann(1, 1)=                       3              calls=                       4
Ackermann(1, 2)=                       4              calls=                       6
Ackermann(1, 3)=                       5              calls=                       8
Ackermann(1, 4)=                       6              calls=                      10
Ackermann(1, 5)=                       7              calls=                      12
Ackermann(1, 6)=                       8              calls=                      14
Ackermann(1, 7)=                       9              calls=                      16
Ackermann(1, 8)=                      10              calls=                      18
Ackermann(1, 9)=                      11              calls=                      20
Ackermann(1,10)=                      12              calls=                      22
Ackermann(1,11)=                      13              calls=                      24
Ackermann(1,12)=                      14              calls=                      26
Ackermann(1,13)=                      15              calls=                      28
Ackermann(1,14)=                      16              calls=                      30
Ackermann(1,15)=                      17              calls=                      32
Ackermann(1,16)=                      18              calls=                      34
Ackermann(1,17)=                      19              calls=                      36
Ackermann(1,18)=                      20              calls=                      38
Ackermann(1,19)=                      21              calls=                      40
Ackermann(1,20)=                      22              calls=                      42
Ackermann(1,21)=                      23              calls=                      44
Ackermann(1,22)=                      24              calls=                      46
Ackermann(1,23)=                      25              calls=                      48
Ackermann(1,24)=                      26              calls=                      50

Ackermann(2, 0)=                       3              calls=                       5
Ackermann(2, 1)=                       5              calls=                      14
Ackermann(2, 2)=                       7              calls=                      27
Ackermann(2, 3)=                       9              calls=                      44
Ackermann(2, 4)=                      11              calls=                      65
Ackermann(2, 5)=                      13              calls=                      90
Ackermann(2, 6)=                      15              calls=                     119
Ackermann(2, 7)=                      17              calls=                     152
Ackermann(2, 8)=                      19              calls=                     189
Ackermann(2, 9)=                      21              calls=                     230
Ackermann(2,10)=                      23              calls=                     275
Ackermann(2,11)=                      25              calls=                     324
Ackermann(2,12)=                      27              calls=                     377

Ackermann(3, 0)=                       5              calls=                      15
Ackermann(3, 1)=                      13              calls=                     106
Ackermann(3, 2)=                      29              calls=                     541
Ackermann(3, 3)=                      61              calls=                    2432
Ackermann(3, 4)=                     125              calls=                   10307
Ackermann(3, 5)=                     253              calls=                   42438
Ackermann(3, 6)=                     509              calls=                  172233
Ackermann(3, 7)=                    1021              calls=                  693964
Ackermann(3, 8)=                    2045              calls=                 2785999

```


===optimized for m ≤ 2===

```rexx
/*REXX program  calculates and displays  some values for the  Ackermann function.       */
high=24
         do     j=0  to 3;                    say
             do k=0  to high % (max(1, j))
             call tell_Ack  j, k
             end   /*k*/
         end       /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell_Ack:  parse arg mm,nn;   calls=0            /*display an echo message to terminal. */
           #=right(nn,length(high))
           say 'Ackermann('mm", "#')='right(ackermann(mm, nn), high),
                                      left('', 12)     'calls='right(calls, high)
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
ackermann: procedure expose calls                /*compute value of Ackermann function. */
           parse arg m,n;   calls=calls+1
           if m==0  then return n + 1
           if n==0  then return ackermann(m-1, 1)
           if m==2  then return n + 3 + n
                         return ackermann(m-1, ackermann(m, n-1) )
```

'''output'''
<pre style="height:60ex">
Ackermann(0, 0)=                       1              calls=         1
Ackermann(0, 1)=                       2              calls=         1
Ackermann(0, 2)=                       3              calls=         1
Ackermann(0, 3)=                       4              calls=         1
Ackermann(0, 4)=                       5              calls=         1
Ackermann(0, 5)=                       6              calls=         1
Ackermann(0, 6)=                       7              calls=         1
Ackermann(0, 7)=                       8              calls=         1
Ackermann(0, 8)=                       9              calls=         1
Ackermann(0, 9)=                      10              calls=         1
Ackermann(0,10)=                      11              calls=         1
Ackermann(0,11)=                      12              calls=         1
Ackermann(0,12)=                      13              calls=         1
Ackermann(0,13)=                      14              calls=         1
Ackermann(0,14)=                      15              calls=         1
Ackermann(0,15)=                      16              calls=         1
Ackermann(0,16)=                      17              calls=         1
Ackermann(0,17)=                      18              calls=         1
Ackermann(0,18)=                      19              calls=         1
Ackermann(0,19)=                      20              calls=         1
Ackermann(0,20)=                      21              calls=         1
Ackermann(0,21)=                      22              calls=         1
Ackermann(0,22)=                      23              calls=         1
Ackermann(0,23)=                      24              calls=         1
Ackermann(0,24)=                      25              calls=         1

Ackermann(1, 0)=                       2              calls=         2
Ackermann(1, 1)=                       3              calls=         4
Ackermann(1, 2)=                       4              calls=         6
Ackermann(1, 3)=                       5              calls=         8
Ackermann(1, 4)=                       6              calls=        10
Ackermann(1, 5)=                       7              calls=        12
Ackermann(1, 6)=                       8              calls=        14
Ackermann(1, 7)=                       9              calls=        16
Ackermann(1, 8)=                      10              calls=        18
Ackermann(1, 9)=                      11              calls=        20
Ackermann(1,10)=                      12              calls=        22
Ackermann(1,11)=                      13              calls=        24
Ackermann(1,12)=                      14              calls=        26
Ackermann(1,13)=                      15              calls=        28
Ackermann(1,14)=                      16              calls=        30
Ackermann(1,15)=                      17              calls=        32
Ackermann(1,16)=                      18              calls=        34
Ackermann(1,17)=                      19              calls=        36
Ackermann(1,18)=                      20              calls=        38
Ackermann(1,19)=                      21              calls=        40
Ackermann(1,20)=                      22              calls=        42
Ackermann(1,21)=                      23              calls=        44
Ackermann(1,22)=                      24              calls=        46
Ackermann(1,23)=                      25              calls=        48
Ackermann(1,24)=                      26              calls=        50

Ackermann(2, 0)=                       3              calls=         5
Ackermann(2, 1)=                       5              calls=         1
Ackermann(2, 2)=                       7              calls=         1
Ackermann(2, 3)=                       9              calls=         1
Ackermann(2, 4)=                      11              calls=         1
Ackermann(2, 5)=                      13              calls=         1
Ackermann(2, 6)=                      15              calls=         1
Ackermann(2, 7)=                      17              calls=         1
Ackermann(2, 8)=                      19              calls=         1
Ackermann(2, 9)=                      21              calls=         1
Ackermann(2,10)=                      23              calls=         1
Ackermann(2,11)=                      25              calls=         1
Ackermann(2,12)=                      27              calls=         1

Ackermann(3, 0)=                       5              calls=         2
Ackermann(3, 1)=                      13              calls=         4
Ackermann(3, 2)=                      29              calls=         6
Ackermann(3, 3)=                      61              calls=         8
Ackermann(3, 4)=                     125              calls=        10
Ackermann(3, 5)=                     253              calls=        12
Ackermann(3, 6)=                     509              calls=        14
Ackermann(3, 7)=                    1021              calls=        16
Ackermann(3, 8)=                    2045              calls=        18

```


===optimized for m ≤ 4===
This REXX version takes advantage that some of the lower numbers for the Ackermann function have direct formulas.


If the   '''numeric digits 100'''   were to be increased to   '''20000''',   then the value of   '''Ackermann(4,2)'''

(the last line of output)   would be presented with the full   '''19,729'''   decimal digits.

```rexx
/*REXX program  calculates and displays  some values for the  Ackermann function.       */
numeric digits 100                               /*use up to 100 decimal digit integers.*/
                       /*╔═════════════════════════════════════════════════════════════╗
                         ║ When REXX raises a number to an integer power  (via the  ** ║
                         ║ operator,  the power can be positive, zero, or negative).   ║
                         ║ Ackermann(5,1)   is a bit impractical to calculate.         ║
                         ╚═════════════════════════════════════════════════════════════╝*/
high=24
         do     j=0  to 4;                   say
             do k=0  to high % (max(1, j))
             call tell_Ack  j, k
             if j==4 & k==2  then leave          /*there's no sense in going overboard. */
             end   /*k*/
         end       /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell_Ack:  parse arg mm,nn;   calls=0            /*display an echo message to terminal. */
           #=right(nn,length(high))
           say 'Ackermann('mm", "#')='right(ackermann(mm, nn), high),
                                      left('', 12)     'calls='right(calls, high)
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
ackermann: procedure expose calls                /*compute value of Ackermann function. */
           parse arg m,n;   calls=calls+1
           if m==0  then return n + 1
           if m==1  then return n + 2
           if m==2  then return n + 3 + n
           if m==3  then return 2**(n+3) - 3
           if m==4  then do; #=2                 /* [↓]  Ugh!  ···  and still more ughs.*/
                                      do (n+3)-1 /*This is where the heavy lifting is.  */
                                      #=2**#
                                      end
                             return #-3
                         end
           if n==0  then return ackermann(m-1, 1)
                         return ackermann(m-1, ackermann(m, n-1) )
```

Output note:   none of the numbers shown below use recursion to compute.

'''output'''
<pre style="height:60ex">
Ackermann(0, 0)=                       1              calls=         1
Ackermann(0, 1)=                       2              calls=         1
Ackermann(0, 2)=                       3              calls=         1
Ackermann(0, 3)=                       4              calls=         1
Ackermann(0, 4)=                       5              calls=         1
Ackermann(0, 5)=                       6              calls=         1
Ackermann(0, 6)=                       7              calls=         1
Ackermann(0, 7)=                       8              calls=         1
Ackermann(0, 8)=                       9              calls=         1
Ackermann(0, 9)=                      10              calls=         1
Ackermann(0,10)=                      11              calls=         1
Ackermann(0,11)=                      12              calls=         1
Ackermann(0,12)=                      13              calls=         1
Ackermann(0,13)=                      14              calls=         1
Ackermann(0,14)=                      15              calls=         1
Ackermann(0,15)=                      16              calls=         1
Ackermann(0,16)=                      17              calls=         1
Ackermann(0,17)=                      18              calls=         1
Ackermann(0,18)=                      19              calls=         1
Ackermann(0,19)=                      20              calls=         1
Ackermann(0,20)=                      21              calls=         1
Ackermann(0,21)=                      22              calls=         1
Ackermann(0,22)=                      23              calls=         1
Ackermann(0,23)=                      24              calls=         1
Ackermann(0,24)=                      25              calls=         1

Ackermann(1, 0)=                       2              calls=         1
Ackermann(1, 1)=                       3              calls=         1
Ackermann(1, 2)=                       4              calls=         1
Ackermann(1, 3)=                       5              calls=         1
Ackermann(1, 4)=                       6              calls=         1
Ackermann(1, 5)=                       7              calls=         1
Ackermann(1, 6)=                       8              calls=         1
Ackermann(1, 7)=                       9              calls=         1
Ackermann(1, 8)=                      10              calls=         1
Ackermann(1, 9)=                      11              calls=         1
Ackermann(1,10)=                      12              calls=         1
Ackermann(1,11)=                      13              calls=         1
Ackermann(1,12)=                      14              calls=         1
Ackermann(1,13)=                      15              calls=         1
Ackermann(1,14)=                      16              calls=         1
Ackermann(1,15)=                      17              calls=         1
Ackermann(1,16)=                      18              calls=         1
Ackermann(1,17)=                      19              calls=         1
Ackermann(1,18)=                      20              calls=         1
Ackermann(1,19)=                      21              calls=         1
Ackermann(1,20)=                      22              calls=         1
Ackermann(1,21)=                      23              calls=         1
Ackermann(1,22)=                      24              calls=         1
Ackermann(1,23)=                      25              calls=         1
Ackermann(1,24)=                      26              calls=         1

Ackermann(2, 0)=                       3              calls=         1
Ackermann(2, 1)=                       5              calls=         1
Ackermann(2, 2)=                       7              calls=         1
Ackermann(2, 3)=                       9              calls=         1
Ackermann(2, 4)=                      11              calls=         1
Ackermann(2, 5)=                      13              calls=         1
Ackermann(2, 6)=                      15              calls=         1
Ackermann(2, 7)=                      17              calls=         1
Ackermann(2, 8)=                      19              calls=         1
Ackermann(2, 9)=                      21              calls=         1
Ackermann(2,10)=                      23              calls=         1
Ackermann(2,11)=                      25              calls=         1
Ackermann(2,12)=                      27              calls=         1

Ackermann(3, 0)=                       5              calls=         1
Ackermann(3, 1)=                      13              calls=         1
Ackermann(3, 2)=                      29              calls=         1
Ackermann(3, 3)=                      61              calls=         1
Ackermann(3, 4)=                     125              calls=         1
Ackermann(3, 5)=                     253              calls=         1
Ackermann(3, 6)=                     509              calls=         1
Ackermann(3, 7)=                    1021              calls=         1
Ackermann(3, 8)=                    2045              calls=         1

Ackermann(4, 0)=                      13              calls=         1
Ackermann(4, 1)=                   65533              calls=         1
Ackermann(4, 2)=89506130880933368E+19728              calls=         1

```



## Ring

{{trans|C#}}

```ring
for m = 0 to 3
        for n = 0 to 4
                see "Ackermann(" + m + ", " + n + ") = " + Ackermann(m, n) + nl
         next
next

func Ackermann m, n
        if m > 0
           if n > 0
                return Ackermann(m - 1, Ackermann(m, n - 1))
            but n = 0
                return Ackermann(m - 1, 1)
            ok
        but m = 0
            if n >= 0
                return n + 1
            ok
        ok
Raise("Incorrect Numerical input !!!")
```

{{out}}

```txt
Ackermann(0, 0) = 1
Ackermann(0, 1) = 2
Ackermann(0, 2) = 3
Ackermann(0, 3) = 4
Ackermann(0, 4) = 5
Ackermann(1, 0) = 2
Ackermann(1, 1) = 3
Ackermann(1, 2) = 4
Ackermann(1, 3) = 5
Ackermann(1, 4) = 6
Ackermann(2, 0) = 3
Ackermann(2, 1) = 5
Ackermann(2, 2) = 7
Ackermann(2, 3) = 9
Ackermann(2, 4) = 11
Ackermann(3, 0) = 5
Ackermann(3, 1) = 13
Ackermann(3, 2) = 29
Ackermann(3, 3) = 61
Ackermann(3, 4) = 125
```

=={{header|Risc-V}}==
the basic recursive function, because memorization and other improvements would blow the clarity.
<lang Risc-V>ackermann: 	#x: a1, y: a2, return: a0
beqz a1, npe #case m = 0
beqz a2, mme #case m > 0 & n = 0
addi sp, sp, -8 #case m > 0 & n > 0
sw ra, 8(sp)
sw a1, 4(sp)
addi a2, a2, -1
jal ackermann
lw a1, 4(sp)
addi a1, a1, -1
mv a2, a0
jal ackermann
lw t0, 8(sp)
addi sp, sp, 8
jr t0, 0
npe:
addi a0, a2, 1
jr ra, 0
mme:
addi sp, sp, -4
sw ra, 4(sp)
addi a1, a1, -1
li a2, 1
jal ackermann
lw t0, 4(sp)
addi sp, sp, 4
jr t0, 0

```



## Ruby

{{trans|Ada}}

```ruby
def ack(m, n)
  if m == 0
    n + 1
  elsif n == 0
    ack(m-1, 1)
  else
    ack(m-1, ack(m, n-1))
  end
end
```

Example:

```ruby
(0..3).each do |m|
  puts (0..6).map { |n| ack(m, n) }.join(' ')
end
```

{{out}}

```txt
 1 2 3 4 5 6 7
 2 3 4 5 6 7 8
 3 5 7 9 11 13 15
 5 13 29 61 125 253 509
```



## Run BASIC


```runbasic
print ackermann(1, 2)

function ackermann(m, n)
   if (m = 0)             then ackermann = (n + 1)
   if (m > 0) and (n = 0) then ackermann = ackermann((m - 1), 1)
   if (m > 0) and (n > 0) then ackermann = ackermann((m - 1), ackermann(m, (n - 1)))
end function
```



## Rust


```rust
fn ack(m: isize, n: isize) -> isize {
    if m == 0 {
        n + 1
    } else if n == 0 {
        ack(m - 1, 1)
    } else {
        ack(m - 1, ack(m, n - 1))
    }
}

fn main() {
    let a = ack(3, 4);
    println!("{}", a); // 125
}

```


Or:


```rust

fn ack(m: u64, n: u64) -> u64 {
	match (m, n) {
		(0, n) => n + 1,
		(m, 0) => ack(m - 1, 1),
		(m, n) => ack(m - 1, ack(m, n - 1)),
	}
}

```



## Sather


```sather
class MAIN is

  ackermann(m, n:INT):INT
    pre m >= 0 and n >= 0
  is
    if m = 0 then return n + 1; end;
    if n = 0 then return ackermann(m-1, 1); end;
    return ackermann(m-1, ackermann(m, n-1));
  end;

  main is
    n, m :INT;
    loop n := 0.upto!(6);
      loop m := 0.upto!(3);
        #OUT + "A(" + m + ", " + n + ") = " + ackermann(m, n) + "\n";
      end;
    end;
  end;
end;
```

Instead of <code>INT</code>, the class <code>INTI</code> could be used, even though we need to use a workaround since in the GNU Sather v1.2.3 compiler the INTI literals are not implemented yet.

```sather
class MAIN is

  ackermann(m, n:INTI):INTI is
    zero ::= 0.inti; -- to avoid type conversion each time
    one  ::= 1.inti;
    if m = zero then return n + one; end;
    if n = zero then return ackermann(m-one, one); end;
    return ackermann(m-one, ackermann(m, n-one));
  end;

  main is
    n, m :INT;
    loop n := 0.upto!(6);
      loop m := 0.upto!(3);
        #OUT + "A(" + m + ", " + n + ") = " + ackermann(m.inti, n.inti) + "\n";
      end;
    end;
  end;
end;
```



## Scala



```scala
def ack(m: BigInt, n: BigInt): BigInt = {
  if (m==0) n+1
  else if (n==0) ack(m-1, 1)
  else ack(m-1, ack(m, n-1))
}
```

{{out|Example}}

```txt

scala> for ( m <- 0 to 3; n <- 0 to 6 ) yield ack(m,n)
res0: Seq.Projection[BigInt] = RangeG(1, 2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 7, 8, 3, 5, 7, 9, 11, 13, 15, 5, 13, 29, 61, 125, 253, 509)

```

Memoized version using a mutable hash map:

```scala
val ackMap = new mutable.HashMap[(BigInt,BigInt),BigInt]
def ackMemo(m: BigInt, n: BigInt): BigInt = {
  ackMap.getOrElseUpdate((m,n), ack(m,n))
}
```



## Scheme


```scheme
(define (A m n)
    (cond
        ((= m 0) (+ n 1))
        ((= n 0) (A (- m 1) 1))
        (else (A (- m 1) (A m (- n 1))))))
```


An improved solution that uses a lazy data structure, streams, and defines [[Knuth up-arrow]]s to calculate iterative exponentiation:


```scheme
(define (A m n)
  (letrec ((A-stream
    (cons-stream
      (ints-from 1) ;; m = 0
      (cons-stream
        (ints-from 2) ;; m = 1
        (cons-stream
          ;; m = 2
          (stream-map (lambda (n)
                        (1+ (* 2 (1+ n))))
                      (ints-from 0))
          (cons-stream
            ;; m = 3
            (stream-map (lambda (n)
                          (- (knuth-up-arrow 2 (- m 2) (+ n 3)) 3))
                        (ints-from 0))
             ;; m = 4...
            (stream-tail A-stream 3)))))))
    (stream-ref (stream-ref A-stream m) n)))

(define (ints-from n)
  (letrec ((ints-rec (cons-stream n (stream-map 1+ ints-rec))))
    ints-rec))

(define (knuth-up-arrow a n b)
  (let loop ((n n) (b b))
    (cond ((= b 0) 1)
          ((= n 1) (expt a b))
          (else    (loop (-1+ n) (loop n (-1+ b)))))))
```



## Scilab

<lang>clear
function acker=ackermann(m,n)
    global calls
    calls=calls+1
    if m==0 then     acker=n+1
    else
        if n==0 then acker=ackermann(m-1,1)
                else acker=ackermann(m-1,ackermann(m,n-1))
        end
    end
endfunction
function printacker(m,n)
    global calls
    calls=0
    printf('ackermann(%d,%d)=',m,n)
    printf('%d  calls=%d\n',ackermann(m,n),calls)
endfunction
maxi=3; maxj=6
for i=0:maxi
   for j=0:maxj
       printacker(i,j)
   end
end
```

{{out}}
<pre style="height:20ex">ackermann(0,0)=1  calls=1
ackermann(0,1)=2  calls=1
ackermann(0,2)=3  calls=1
ackermann(0,3)=4  calls=1
ackermann(0,4)=5  calls=1
ackermann(0,5)=6  calls=1
ackermann(0,6)=7  calls=1
ackermann(1,0)=2  calls=2
ackermann(1,1)=3  calls=4
ackermann(1,2)=4  calls=6
ackermann(1,3)=5  calls=8
ackermann(1,4)=6  calls=10
ackermann(1,5)=7  calls=12
ackermann(1,6)=8  calls=14
ackermann(2,0)=3  calls=5
ackermann(2,1)=5  calls=14
ackermann(2,2)=7  calls=27
ackermann(2,3)=9  calls=44
ackermann(2,4)=11  calls=65
ackermann(2,5)=13  calls=90
ackermann(2,6)=15  calls=119
ackermann(3,0)=5  calls=15
ackermann(3,1)=13  calls=106
ackermann(3,2)=29  calls=541
ackermann(3,3)=61  calls=2432
ackermann(3,4)=125  calls=10307
ackermann(3,5)=253  calls=42438
ackermann(3,6)=509  calls=172233
```



## Seed7


```seed7
const func integer: ackermann (in integer: m, in integer: n) is func
  result
    var integer: result is 0;
  begin
    if m = 0 then
      result := succ(n);
    elsif n = 0 then
      result := ackermann(pred(m), 1);
    else
      result := ackermann(pred(m), ackermann(m, pred(n)));
    end if;
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/math.htm#ackermann]


## SETL


```SETL
program ackermann;

(for m in [0..3])
  print(+/ [rpad('' + ack(m, n), 4): n in [0..6]]);
end;

proc ack(m, n);
  return {[0,n+1]}(m) ? ack(m-1, {[0,1]}(n) ? ack(m, n-1));
end proc;

end program;
```


## Shen


```shen
(define ack
  0 N -> (+ N 1)
  M 0 -> (ack (- M 1) 1)
  M N -> (ack (- M 1)
              (ack M (- N 1))))
```



## Sidef


```ruby
func A(m, n) {
    m == 0 ? (n + 1)
           : (n == 0 ? (A(m - 1, 1))
                     : (A(m - 1, A(m, n - 1))));
}
```


Alternatively, using multiple dispatch:

```ruby
func A((0), n) { n + 1 }
func A(m, (0)) { A(m - 1, 1) }
func A(m,  n)  { A(m-1, A(m, n-1)) }
```


Calling the function:

```ruby
say A(3, 2);     # prints: 29
```


## Simula

as modified by R. Péter and R. Robinson:

```Simula
 BEGIN
    INTEGER procedure
    Ackermann(g, p); SHORT INTEGER g, p;
        Ackermann:= IF g = 0 THEN p+1
            ELSE Ackermann(g-1, IF p = 0 THEN 1
                         ELSE Ackermann(g, p-1));

    INTEGER g, p;
    FOR p := 0 STEP 3 UNTIL 13 DO BEGIN
    	g := 4 - p/3;
        outtext("Ackermann("); outint(g, 0);
        outchar(','); outint(p, 2); outtext(") = ");
        outint(Ackermann(g, p), 0); outimage
    END
END
```

{{Output}}

```txt
Ackermann(4, 0) = 13
Ackermann(3, 3) = 61
Ackermann(2, 6) = 15
Ackermann(1, 9) = 11
Ackermann(0,12) = 13

```



## Slate


```slate
m@(Integer traits) ackermann: n@(Integer traits)
[
  m isZero
    ifTrue: [n + 1]
    ifFalse:
      [n isZero
	 ifTrue: [m - 1 ackermann: n]
	 ifFalse: [m - 1 ackermann: (m ackermann: n - 1)]]
].
```



## Smalltalk


```smalltalk
|ackermann|
ackermann := [ :n :m |
  (n = 0) ifTrue: [ (m + 1) ]
          ifFalse: [
           (m = 0) ifTrue: [ ackermann value: (n-1) value: 1 ]
                   ifFalse: [
                        ackermann value: (n-1)
                                  value: ( ackermann value: n
                                                     value: (m-1) )
                   ]
          ]
].

(ackermann value: 0 value: 0) displayNl.
(ackermann value: 3 value: 4) displayNl.
```



## SmileBASIC


```smilebasic
DEF ACK(M,N)
 IF M==0 THEN
  RETURN N+1
 ELSEIF M>0 AND N==0 THEN
  RETURN ACK(M-1,1)
 ELSE
  RETURN ACK(M-1,ACK(M,N-1))
 ENDIF
END
```



## SNOBOL4

{{works with|Macro Spitbol}}
Both Snobol4+ and CSnobol stack overflow, at ack(3,3) and ack(3,4), respectively.

```SNOBOL4
define('ack(m,n)') :(ack_end)
ack     ack = eq(m,0) n + 1 :s(return)
        ack = eq(n,0) ack(m - 1,1) :s(return)
        ack = ack(m - 1,ack(m,n - 1)) :(return)
ack_end

*       # Test and display ack(0,0) .. ack(3,6)
L1      str = str ack(m,n) ' '
        n = lt(n,6) n + 1 :s(L1)
        output = str; str = ''
        n = 0; m = lt(m,3) m + 1 :s(L1)
end
```

{{out}}

```txt
1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509
```



## SNUSP


```snusp
   /==!/==atoi=@@@-@-----#
   |   |                          Ackermann function
   |   |       /
### ===
\!==\!====\  recursion:
$,@/>,@/==ack=!\?\<+#    |   |     |   A(0,j) -> j+1
 j   i           \<?\+>-@/#  |     |   A(i,0) -> A(i-1,1)
                    \@\>@\->@/@\<-@/#  A(i,j) -> A(i-1,A(i,j-1))
                      |  |     |
            #      #  |  |     |             /+<<<-\
            /-<<+>>\!=/  \=====|==!/
### ==
?\>>>=?/<<#
            ?      ?           |   \<<<+>+>>-/
            \>>+<<-/!
### ====
/
            #      #
```

One could employ [[:Category:Recursion|tail recursion]] elimination by replacing "@/#" with "/" in two places above.


## SPAD

{{works with|FriCAS, OpenAxiom, Axiom}}

```SPAD

NNI ==> NonNegativeInteger

A:(NNI,NNI) -> NNI

A(m,n) ==
  m=0 => n+1
  m>0 and n=0 => A(m-1,1)
  m>0 and n>0 => A(m-1,A(m,n-1))

-- Example
matrix [[A(i,j) for i in 0..3] for j in 0..3]

```


{{out}}

```txt


        +1  2  3  5 +
        |           |
        |2  3  5  13|
   (1)  |           |
        |3  4  7  29|
        |           |
        +4  5  9  61+
                                             Type: Matrix(NonNegativeInteger)

```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON@

CREATE OR REPLACE FUNCTION ACKERMANN(
  IN M SMALLINT,
  IN N BIGINT
 ) RETURNS BIGINT
 BEGIN
  DECLARE RET BIGINT;
  DECLARE STMT STATEMENT;

  IF (M = 0) THEN
   SET RET = N + 1;
  ELSEIF (N = 0) THEN
   PREPARE STMT FROM 'SET ? = ACKERMANN(? - 1, 1)';
   EXECUTE STMT INTO RET USING M;
  ELSE
   PREPARE STMT FROM 'SET ? = ACKERMANN(? - 1, ACKERMANN(?, ? - 1))';
   EXECUTE STMT INTO RET USING M, M, N;
  END IF;
  RETURN RET;
 END @

BEGIN
 DECLARE M SMALLINT DEFAULT 0;
 DECLARE N SMALLINT DEFAULT 0;
 DECLARE MAX_LEVELS CONDITION FOR SQLSTATE '54038';
 DECLARE CONTINUE HANDLER FOR MAX_LEVELS BEGIN END;

 WHILE (N <= 6) DO
  WHILE (M <= 3) DO
   CALL DBMS_OUTPUT.PUT_LINE('ACKERMANN(' || M || ', ' || N || ') = ' || ACKERMANN(M, N));
   SET M = M + 1;
  END WHILE;
  SET M = 0;
  SET N = N + 1;
 END WHILE;
END @

```

Output:

```txt

db2 -td@
db2 => CREATE OR REPLACE FUNCTION ACKERMANN(
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.
db2 => BEGIN
db2 (cont.) => END
...
DB20000I  The SQL command completed successfully.

ACKERMANN(0, 0) = 1
ACKERMANN(1, 0) = 2
ACKERMANN(2, 0) = 3
ACKERMANN(3, 0) = 5
ACKERMANN(0, 1) = 2
ACKERMANN(1, 1) = 3
ACKERMANN(2, 1) = 5
ACKERMANN(3, 1) = 13
ACKERMANN(0, 2) = 3
ACKERMANN(1, 2) = 4
ACKERMANN(2, 2) = 7
ACKERMANN(3, 2) = 29
ACKERMANN(0, 3) = 4
ACKERMANN(1, 3) = 5
ACKERMANN(2, 3) = 9
ACKERMANN(3, 3) = 61
ACKERMANN(0, 4) = 5
ACKERMANN(1, 4) = 6
ACKERMANN(2, 4) = 11
ACKERMANN(0, 5) = 6
ACKERMANN(1, 5) = 7
ACKERMANN(2, 5) = 13
ACKERMANN(0, 6) = 7
ACKERMANN(1, 6) = 8
ACKERMANN(2, 6) = 15

```


The maximum levels of cascade calls in Db2 are 16, and in some cases when executing the Ackermann function, it arrives to this limit (SQL0724N). Thus, the code catches the exception and continues with the next try.


## Standard ML


```sml
fun a (0, n) = n+1
  | a (m, 0) = a (m-1, 1)
  | a (m, n) = a (m-1, a (m, n-1))
```



## Stata


```stata
mata
function ackermann(m,n) {
	if (m==0) {
		return(n+1)
	} else if (n==0) {
		return(ackermann(m-1,1))
	} else {
		return(ackermann(m-1,ackermann(m,n-1)))
	}
}

for (i=0; i<=3; i++) printf("%f\n",ackermann(i,4))
5
6
11
125
end
```



## Swift


```swift
func ackerman(m:Int, n:Int) -> Int {
    if m == 0 {
        return n+1
    } else if n == 0 {
        return ackerman(m-1, 1)
    } else {
        return ackerman(m-1, ackerman(m, n-1))
    }
}
```



## Tcl


### Simple

{{trans|Ruby}}

```tcl
proc ack {m n} {
    if {$m == 0} {
        expr {$n + 1}
    } elseif {$n == 0} {
        ack [expr {$m - 1}] 1
    } else {
        ack [expr {$m - 1}] [ack $m [expr {$n - 1}]]
    }
}
```


### With Tail Recursion

With Tcl 8.6, this version is preferred (though the language supports tailcall optimization, it does not apply it automatically in order to preserve stack frame semantics):

```tcl
proc ack {m n} {
    if {$m == 0} {
        expr {$n + 1}
    } elseif {$n == 0} {
        tailcall ack [expr {$m - 1}] 1
    } else {
        tailcall ack [expr {$m - 1}] [ack $m [expr {$n - 1}]]
    }
}
```

===To Infinity… and Beyond!===
If we want to explore the higher reaches of the world of Ackermann's function, we need techniques to really cut the amount of computation being done.
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# A memoization engine, from http://wiki.tcl.tk/18152
oo::class create cache {
    filter Memoize
    variable ValueCache
    method Memoize args {
        # Do not filter the core method implementations
        if {[lindex [self target] 0] eq "::oo::object"} {
            return [next {*}$args]
        }

        # Check if the value is already in the cache
        set key [self target],$args
        if {[info exist ValueCache($key)]} {
            return $ValueCache($key)
        }

        # Compute value, insert into cache, and return it
        return [set ValueCache($key) [next {*}$args]]
    }
    method flushCache {} {
        unset ValueCache
        # Skip the cacheing
        return -level 2 ""
    }
}

# Make an object, attach the cache engine to it, and define ack as a method
oo::object create cached
oo::objdefine cached {
    mixin cache
    method ack {m n} {
        if {$m==0} {
            expr {$n+1}
        } elseif {$m==1} {
            # From the Mathematica version
            expr {$m+2}
        } elseif {$m==2} {
            # From the Mathematica version
            expr {2*$n+3}
        } elseif {$m==3} {
            # From the Mathematica version
            expr {8*(2**$n-1)+5}
        } elseif {$n==0} {
            tailcall my ack [expr {$m-1}] 1
        } else {
            tailcall my ack [expr {$m-1}] [my ack $m [expr {$n-1}]]
        }
    }
}

# Some small tweaks...
interp recursionlimit {} 100000
interp alias {} ack {} cacheable ack
```

But even with all this, you still run into problems calculating <math>\mathit{ack}(4,3)</math> as that's kind-of large…


## TSE SAL


```TSESAL>// library: math: get: ackermann: recursive <description></description> <version>1.0.0.0.5</version> <version control></version control
 (filenamemacro=getmaare.s) [kn, ri, tu, 27-12-2011 14:46:59]
INTEGER PROC FNMathGetAckermannRecursiveI( INTEGER mI, INTEGER nI )
 IF ( mI == 0 )
  RETURN( nI + 1 )
 ENDIF
 IF ( nI == 0 )
  RETURN( FNMathGetAckermannRecursiveI( mI - 1, 1 ) )
 ENDIF
 RETURN( FNMathGetAckermannRecursiveI( mI - 1, FNMathGetAckermannRecursiveI( mI, nI - 1 ) ) )
END

PROC Main()
STRING s1[255] = "2"
STRING s2[255] = "3"
IF ( NOT ( Ask( "math: get: ackermann: recursive: m = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
IF ( NOT ( Ask( "math: get: ackermann: recursive: n = ", s2, _EDIT_HISTORY_ ) ) AND ( Length( s2 ) > 0 ) ) RETURN() ENDIF
 Message( FNMathGetAckermannRecursiveI( Val( s1 ), Val( s2 ) ) ) // gives e.g. 9
END
```


=={{header|TI-83 BASIC}}==

This program assumes the variables N and M are the arguments of the function, and that the list L1 is empty. It stores the result in the system variable ANS. (Program names can be no longer than 8 characters, so I had to truncate the function's name.)


```ti83b
PROGRAM:ACKERMAN
:If not(M
:Then
:N+1→N
:Return
:Else
:If not(N
:Then
:1→N
:M-1→M
:prgmACKERMAN
:Else
:N-1→N
:M→L1(1+dim(L1
:prgmACKERMAN
:Ans→N
:L1(dim(L1))-1→M
:dim(L1)-1→dim(L1
:prgmACKERMAN
:End
:End
```


Here is a handler function that makes the previous function easier to use. (You can name it whatever you want.)


```ti83b
PROGRAM:AHANDLER
:0→dim(L1
:Prompt M
:Prompt N
:prgmACKERMAN
:Disp Ans
```


=={{header|TI-89 BASIC}}==

```ti89b
Define A(m,n) = when(m=0, n+1, when(n=0, A(m-1,1), A(m-1, A(m, n-1))))
```



## TorqueScript


```TorqueScript
function ackermann(%m,%n)
{
   if(%m==0)
      return %n+1;
   if(%m>0&&%n==0)
      return ackermann(%m-1,1);
   if(%m>0&&%n>0)
      return ackermann(%m-1,ackermann(%m,%n-1));
}
```



## TXR


{{trans|Scheme}}
with memoization.


```txrlisp
(defmacro defmemofun (name (. args) . body)
  (let ((hash (gensym "hash-"))
        (argl (gensym "args-"))
        (hent (gensym "hent-"))
        (uniq (copy-str "uniq")))
    ^(let ((,hash (hash :equal-based)))
       (defun ,name (,*args)
         (let* ((,argl (list ,*args))
                (,hent (inhash ,hash ,argl ,uniq)))
           (if (eq (cdr ,hent) ,uniq)
             (set (cdr ,hent) (block ,name (progn ,*body)))
             (cdr ,hent)))))))

(defmemofun ack (m n)
  (cond
    ((= m 0) (+ n 1))
    ((= n 0) (ack (- m 1) 1))
    (t (ack (- m 1) (ack m (- n 1))))))

(each ((i (range 0 3)))
  (each ((j (range 0 4)))
    (format t "ack(~a, ~a) = ~a\n" i j (ack i j))))
```


{{out}}


```txt
ack(0, 0) = 1
ack(0, 1) = 2
ack(0, 2) = 3
ack(0, 3) = 4
ack(0, 4) = 5
ack(1, 0) = 2
ack(1, 1) = 3
ack(1, 2) = 4
ack(1, 3) = 5
ack(1, 4) = 6
ack(2, 0) = 3
ack(2, 1) = 5
ack(2, 2) = 7
ack(2, 3) = 9
ack(2, 4) = 11
ack(3, 0) = 5
ack(3, 1) = 13
ack(3, 2) = 29
ack(3, 3) = 61
ack(3, 4) = 125
```



## UNIX Shell

{{works with|Bash}}

```bash
ack() {
  local m=$1
  local n=$2
  if [ $m -eq 0 ]; then
    echo -n $((n+1))
  elif [ $n -eq 0 ]; then
    ack $((m-1)) 1
  else
    ack $((m-1)) $(ack $m $((n-1)))
  fi
}
```

Example:

```bash
for ((m=0;m<=3;m++)); do
  for ((n=0;n<=6;n++)); do
    ack $m $n
    echo -n " "
  done
  echo
done
```

{{out}}

```txt
1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509
```



## Ursala

Anonymous recursion is the usual way of doing things like this.

```Ursala
#import std
#import nat

ackermann =

~&al^?\successor@ar ~&ar?(
   ^R/~&f ^/predecessor@al ^|R/~& ^|/~& predecessor,
   ^|R/~& ~&\1+ predecessor@l)
```

test program for the first 4 by 7 numbers:

```Ursala
#cast %nLL

test = block7 ackermann*K0 iota~~/4 7
```

{{out}}

```txt

<
   <1,2,3,4,5,6,7>,
   <2,3,4,5,6,7,8>,
   <3,5,7,9,11,13,15>,
   <5,13,29,61,125,253,509>>
```



## V

{{trans|Joy}}

```v
[ack
       [ [pop zero?] [popd succ]
         [zero?]     [pop pred 1 ack]
         [true]      [[dup pred swap] dip pred ack ack ]
       ] when].
```

using destructuring view

```v
[ack
       [ [pop zero?] [ [m n : [n succ]] view i]
         [zero?]     [ [m n : [m pred 1 ack]] view i]
         [true]      [ [m n : [m pred m n pred ack ack]] view i]
       ] when].
```



## VBA


```vb
Private Function Ackermann_function(m As Variant, n As Variant) As Variant
    Dim result As Variant
    Debug.Assert m >= 0
    Debug.Assert n >= 0
    If m = 0 Then
        result = CDec(n + 1)
    Else
        If n = 0 Then
            result = Ackermann_function(m - 1, 1)
        Else
            result = Ackermann_function(m - 1, Ackermann_function(m, n - 1))
        End If
    End If
    Ackermann_function = CDec(result)
End Function
Public Sub main()
    Debug.Print "           n=",
    For j = 0 To 7
        Debug.Print j,
    Next j
    Debug.Print
    For i = 0 To 3
        Debug.Print "m=" & i,
        For j = 0 To 7
            Debug.Print Ackermann_function(i, j),
        Next j
        Debug.Print
    Next i
End Sub
```
{{out}}

```txt
           n=  0             1             2             3             4             5             6             7
m=0            1             2             3             4             5             6             7             8
m=1            2             3             4             5             6             7             8             9
m=2            3             5             7             9             11            13            15            17
m=3            5             13            29            61            125           253           509           1021
```


## VBScript

Based on BASIC version. Uncomment all the lines referring to <code>depth</code> and see just how deep the recursion goes.
;Implementation

```vb
option explicit
'~ dim depth
function ack(m, n)
	'~ wscript.stdout.write depth & " "
	if m = 0 then
		'~ depth = depth + 1
		ack = n + 1
		'~ depth = depth - 1
	elseif m > 0 and n = 0 then
		'~ depth = depth + 1
		ack = ack(m - 1, 1)
		'~ depth = depth - 1
	'~ elseif m > 0 and n > 0 then
	else
		'~ depth = depth + 1
		ack = ack(m - 1, ack(m, n - 1))
		'~ depth = depth - 1
	end if

end function
```

;Invocation

```vb
wscript.echo ack( 1, 10 )
'~ depth = 0
wscript.echo ack( 2, 1 )
'~ depth = 0
wscript.echo ack( 4, 4 )
```

{{out}}

```txt

12
5
C:\foo\ackermann.vbs(16, 3) Microsoft VBScript runtime error: Out of stack space: 'ack'

```



## Visual Basic

{{trans|Rexx}}
{{works with|Visual Basic|VB6 Standard}}

```vb

Option Explicit
Dim calls As Long
Sub main()
    Const maxi = 4
    Const maxj = 9
    Dim i As Long, j As Long
    For i = 0 To maxi
        For j = 0 To maxj
            Call print_acker(i, j)
        Next j
    Next i
End Sub 'main
Sub print_acker(m As Long, n As Long)
    calls = 0
    Debug.Print "ackermann("; m; ","; n; ")=";
    Debug.Print ackermann(m, n), "calls="; calls
End Sub 'print_acker
Function ackermann(m As Long, n As Long) As Long
    calls = calls + 1
    If m = 0 Then
        ackermann = n + 1
    Else
        If n = 0 Then
            ackermann = ackermann(m - 1, 1)
        Else
            ackermann = ackermann(m - 1, ackermann(m, n - 1))
        End If
    End If
End Function 'ackermann
```

{{Out}}
<pre style="height:20ex">ackermann( 0 , 0 )= 1       calls= 1
ackermann( 0 , 1 )= 2       calls= 1
ackermann( 0 , 2 )= 3       calls= 1
ackermann( 0 , 3 )= 4       calls= 1
ackermann( 0 , 4 )= 5       calls= 1
ackermann( 0 , 5 )= 6       calls= 1
ackermann( 0 , 6 )= 7       calls= 1
ackermann( 0 , 7 )= 8       calls= 1
ackermann( 0 , 8 )= 9       calls= 1
ackermann( 0 , 9 )= 10      calls= 1
ackermann( 1 , 0 )= 2       calls= 2
ackermann( 1 , 1 )= 3       calls= 4
ackermann( 1 , 2 )= 4       calls= 6
ackermann( 1 , 3 )= 5       calls= 8
ackermann( 1 , 4 )= 6       calls= 10
ackermann( 1 , 5 )= 7       calls= 12
ackermann( 1 , 6 )= 8       calls= 14
ackermann( 1 , 7 )= 9       calls= 16
ackermann( 1 , 8 )= 10      calls= 18
ackermann( 1 , 9 )= 11      calls= 20
ackermann( 2 , 0 )= 3       calls= 5
ackermann( 2 , 1 )= 5       calls= 14
ackermann( 2 , 2 )= 7       calls= 27
ackermann( 2 , 3 )= 9       calls= 44
ackermann( 2 , 4 )= 11      calls= 65
ackermann( 2 , 5 )= 13      calls= 90
ackermann( 2 , 6 )= 15      calls= 119
ackermann( 2 , 7 )= 17      calls= 152
ackermann( 2 , 8 )= 19      calls= 189
ackermann( 2 , 9 )= 21      calls= 230
ackermann( 3 , 0 )= 5       calls= 15
ackermann( 3 , 1 )= 13      calls= 106
ackermann( 3 , 2 )= 29      calls= 541
ackermann( 3 , 3 )= 61      calls= 2432
ackermann( 3 , 4 )= 125     calls= 10307
ackermann( 3 , 5 )= 253     calls= 42438
ackermann( 3 , 6 )= 509     calls= 172233
ackermann( 3 , 7 )= 1021    calls= 693964
ackermann( 3 , 8 )= 2045    calls= 2785999
ackermann( 3 , 9 )= 4093    calls= 11164370
ackermann( 4 , 0 )= 13      calls= 107
ackermann( 4 , 1 )= out of stack space
```



## Vlang


```vlang
fn ackermann(m int, n int) int {
  switch 0 {
    case m:
      return n + 1

    case n:
      return ackermann(m - 1, 1)
  }

  return ackermann(m - 1, ackermann(m, n - 1))
}
```



## Wart


```wart
def (ackermann m n)
  (if m=0
        n+1
      n=0
        (ackermann m-1 1)
      :else
        (ackermann m-1 (ackermann m n-1)))
```



## WDTE


```WDTE>let memo a m n =
 true {
	== m 0 => + n 1;
	== n 0 => a (- m 1) 1;
	true => a (- m 1) (a m (- n 1));
};
```



## Wren


```wren

// To use recursion definition and declaration must be on separate lines
var Ackermann
Ackermann = Fn.new {|m, n|
    if (m == 0) return n + 1
    if (n == 0) return Ackermann.call(m - 1, 1)
    return Ackermann.call(m - 1, Ackermann.call(m, n - 1))
}


```



## XLISP


```lisp
(defun ackermann (m n)
    (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (t (ackermann (- m 1) (ackermann m (- n 1))))))
```

Test it:

```lisp
(print (ackermann 3 9))
```

Output (after a very perceptible pause):

```txt
4093
```

That worked well. Test it again:

```lisp
(print (ackermann 4 1))
```

Output (after another pause):

```txt
Abort: control stack overflow
happened in: #<Code ACKERMANN>
```



## XPL0


```XPL0
include c:\cxpl\codes;

func Ackermann(M, N);
int M, N;
[if M=0 then return N+1;
 if N=0 then return Ackermann(M-1, 1);
return Ackermann(M-1, Ackermann(M, N-1));
]; \Ackermann

int M, N;
[for M:= 0 to 3 do
    [for N:= 0 to 7 do
        [IntOut(0, Ackermann(M, N));  ChOut(0,9\tab\)];
    CrLf(0);
    ];
]
```

Recursion overflows the stack if either M or N is extended by a single count.
{{out}}

```txt

1       2       3       4       5       6       7       8
2       3       4       5       6       7       8       9
3       5       7       9       11      13      15      17
5       13      29      61      125     253     509     1021

```



## XSLT


The following named template calculates the Ackermann function:

```xml

  <xsl:template name="ackermann">
    <xsl:param name="m"/>
    <xsl:param name="n"/>

    <xsl:choose>
      <xsl:when test="$m = 0">
        <xsl:value-of select="$n+1"/>
      </xsl:when>
      <xsl:when test="$n = 0">
        <xsl:call-template name="ackermann">
          <xsl:with-param name="m" select="$m - 1"/>
          <xsl:with-param name="n" select="'1'"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="p">
          <xsl:call-template name="ackermann">
            <xsl:with-param name="m" select="$m"/>
            <xsl:with-param name="n" select="$n - 1"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:call-template name="ackermann">
          <xsl:with-param name="m" select="$m - 1"/>
          <xsl:with-param name="n" select="$p"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

```


Here it is as part of a template

```xml

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="arguments">
      <xsl:for-each select="args">
        <div>
          <xsl:value-of select="m"/>, <xsl:value-of select="n"/>:
          <xsl:call-template name="ackermann">
            <xsl:with-param name="m" select="m"/>
            <xsl:with-param name="n" select="n"/>
          </xsl:call-template>
        </div>
      </xsl:for-each>
  </xsl:template>

  <xsl:template name="ackermann">
    <xsl:param name="m"/>
    <xsl:param name="n"/>

    <xsl:choose>
      <xsl:when test="$m = 0">
        <xsl:value-of select="$n+1"/>
      </xsl:when>
      <xsl:when test="$n = 0">
        <xsl:call-template name="ackermann">
          <xsl:with-param name="m" select="$m - 1"/>
          <xsl:with-param name="n" select="'1'"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="p">
          <xsl:call-template name="ackermann">
            <xsl:with-param name="m" select="$m"/>
            <xsl:with-param name="n" select="$n - 1"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:call-template name="ackermann">
          <xsl:with-param name="m" select="$m - 1"/>
          <xsl:with-param name="n" select="$p"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

```


Which will transform this input

```xml

<?xml version="1.0" ?>
<?xml-stylesheet type="text/xsl" href="ackermann.xslt"?>
<arguments>
  <args>
    <m>0</m>
    <n>0</n>
  </args>
  <args>
    <m>0</m>
    <n>1</n>
  </args>
  <args>
    <m>0</m>
    <n>2</n>
  </args>
  <args>
    <m>0</m>
    <n>3</n>
  </args>
  <args>
    <m>0</m>
    <n>4</n>
  </args>
  <args>
    <m>0</m>
    <n>5</n>
  </args>
  <args>
    <m>0</m>
    <n>6</n>
  </args>
  <args>
    <m>0</m>
    <n>7</n>
  </args>
  <args>
    <m>0</m>
    <n>8</n>
  </args>
  <args>
    <m>1</m>
    <n>0</n>
  </args>
  <args>
    <m>1</m>
    <n>1</n>
  </args>
  <args>
    <m>1</m>
    <n>2</n>
  </args>
  <args>
    <m>1</m>
    <n>3</n>
  </args>
  <args>
    <m>1</m>
    <n>4</n>
  </args>
  <args>
    <m>1</m>
    <n>5</n>
  </args>
  <args>
    <m>1</m>
    <n>6</n>
  </args>
  <args>
    <m>1</m>
    <n>7</n>
  </args>
  <args>
    <m>1</m>
    <n>8</n>
  </args>
  <args>
    <m>2</m>
    <n>0</n>
  </args>
  <args>
    <m>2</m>
    <n>1</n>
  </args>
  <args>
    <m>2</m>
    <n>2</n>
  </args>
  <args>
    <m>2</m>
    <n>3</n>
  </args>
  <args>
    <m>2</m>
    <n>4</n>
  </args>
  <args>
    <m>2</m>
    <n>5</n>
  </args>
  <args>
    <m>2</m>
    <n>6</n>
  </args>
  <args>
    <m>2</m>
    <n>7</n>
  </args>
  <args>
    <m>2</m>
    <n>8</n>
  </args>
  <args>
    <m>3</m>
    <n>0</n>
  </args>
  <args>
    <m>3</m>
    <n>1</n>
  </args>
  <args>
    <m>3</m>
    <n>2</n>
  </args>
  <args>
    <m>3</m>
    <n>3</n>
  </args>
  <args>
    <m>3</m>
    <n>4</n>
  </args>
  <args>
    <m>3</m>
    <n>5</n>
  </args>
  <args>
    <m>3</m>
    <n>6</n>
  </args>
  <args>
    <m>3</m>
    <n>7</n>
  </args>
  <args>
    <m>3</m>
    <n>8</n>
  </args>
</arguments>

```


into this output

```txt

0, 0: 1
0, 1: 2
0, 2: 3
0, 3: 4
0, 4: 5
0, 5: 6
0, 6: 7
0, 7: 8
0, 8: 9
1, 0: 2
1, 1: 3
1, 2: 4
1, 3: 5
1, 4: 6
1, 5: 7
1, 6: 8
1, 7: 9
1, 8: 10
2, 0: 3
2, 1: 5
2, 2: 7
2, 3: 9
2, 4: 11
2, 5: 13
2, 6: 15
2, 7: 17
2, 8: 19
3, 0: 5
3, 1: 13
3, 2: 29
3, 3: 61
3, 4: 125
3, 5: 253
3, 6: 509
3, 7: 1021
3, 8: 2045

```



## Yabasic


```Yabasic
sub ack(M,N)
    if M = 0 return N + 1
    if N = 0 return ack(M-1,1)
    return ack(M-1,ack(M, N-1))
end sub

print ack(3, 4)

```


What smart code can get. Fast as lightning!
{{trans|Phix}}

```Yabasic
sub ack(m, n)
    if m=0 then
        return n+1
    elsif m=1 then
        return n+2
    elsif m=2 then
        return 2*n+3
    elsif m=3 then
        return 2^(n+3)-3
    elsif m>0 and n=0 then
        return ack(m-1,1)
    else
        return ack(m-1,ack(m,n-1))
    end if
end sub

sub Ackermann()
    local i, j
    for i=0 to 3
        for j=0 to 10
            print ack(i,j) using "#####";
        next
        print
    next
    print "ack(4,1) ";: print ack(4,1) using "#####"
end sub

Ackermann()
```



## Yorick


```yorick
func ack(m, n) {
    if(m == 0)
        return n + 1;
    else if(n == 0)
        return ack(m - 1, 1);
    else
        return ack(m - 1, ack(m, n - 1));
}
```

Example invocation:

```yorick
for(m = 0; m <= 3; m++) {
    for(n = 0; n <= 6; n++)
        write, format="%d ", ack(m, n);
    write, "";
}
```

{{out}}

```txt
1 2 3 4 5 6 7
2 3 4 5 6 7 8
3 5 7 9 11 13 15
5 13 29 61 125 253 509
```


{{omit from|LaTeX}}
{{omit from|Make}}
{{omit from|PlainTeX}}


## ZED

Source -> http://ideone.com/53FzPA
Compiled -> http://ideone.com/OlS7zL

```zed
(A) m n
comment:
(=) m 0
(add1) n

(A) m n
comment:
(=) n 0
(A) (sub1) m 1

(A) m n
comment:
#true
(A) (sub1) m (A) m (sub1) n

(add1) n
comment:
#true
(003) "+" n 1

(sub1) n
comment:
#true
(003) "-" n 1

(=) n1 n2
comment:
#true
(003) "=" n1 n2
```



## ZX Spectrum Basic

{{trans|BASIC256}}

```zxbasic
10 DIM s(2000,3)
20 LET s(1,1)=3: REM M
30 LET s(1,2)=7: REM N
40 LET lev=1
50 GO SUB 100
60 PRINT "A(";s(1,1);",";s(1,2);") = ";s(1,3)
70 STOP
100 IF s(lev,1)=0 THEN LET s(lev,3)=s(lev,2)+1: RETURN
110 IF s(lev,2)=0 THEN LET lev=lev+1: LET s(lev,1)=s(lev-1,1)-1: LET s(lev,2)=1: GO SUB 100: LET s(lev-1,3)=s(lev,3): LET lev=lev-1: RETURN
120 LET lev=lev+1
130 LET s(lev,1)=s(lev-1,1)
140 LET s(lev,2)=s(lev-1,2)-1
150 GO SUB 100
160 LET s(lev,1)=s(lev-1,1)-1
170 LET s(lev,2)=s(lev,3)
180 GO SUB 100
190 LET s(lev-1,3)=s(lev,3)
200 LET lev=lev-1
210 RETURN
```

{{out}}

```txt
A(3,7) = 1021
```

