+++
title = "Pi"
description = ""
date = 2019-05-28T00:01:16Z
aliases = []
[extra]
id = 9393
[taxonomies]
categories = ["task", "Irrational numbers"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "applesoft",
  "autohotkey",
  "basic",
  "basic256",
  "bbc_basic",
  "bc",
  "bracmat",
  "c",
  "clojure",
  "commodore_basic",
  "common_lisp",
  "crystal",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "fortran",
  "freebasic",
  "funl",
  "futurebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "ol",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "simula",
  "tcl",
  "typescript",
  "visual_basic",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

[[File:pi_symbol.jpg|500px||right]]

Create a program to continually calculate and output the next decimal digit of   <big><big><math>\pi</math></big></big>   (pi).

The program should continue forever (until it is aborted by the user) calculating and outputting each decimal digit in succession.

The output should be a decimal sequence beginning   3.14159265 ...


Note: this task is about   ''calculating''   pi.   For information on built-in pi constants see [[Real constants and functions]].


Related Task [[Arithmetic-geometric mean/Calculate Pi]]





## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Spigot algorithm do the digits of PI  02/07/2016
PISPIG   CSECT
         USING  PISPIG,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         SR     R0,R0              0
         ST     R0,MORE            more=0
         LA     R6,1               i=1
LOOPI1   C      R6,=A(NBUF)        do i=1 to hbound(buf)
         BH     ELOOPI1            "
         SR     R9,R9                karray=0
         L      R7,=A(NVECT)         j=hbound(vect)
         LR     R1,R7                  j
         SLA    R1,2                   .
         LA     R10,VECT-4(R1)       r10=@vect(j)
LOOPJ    EQU    *                    do j=hbound(vect) to 1 by -1
         L      R5,=F'100000'            100000
         M      R4,0(R10)                *vect(j)
         LR     R2,R5                    r2=100000*vect(j)
         LR     R5,R9                    karray
         MR     R4,R7                    karray*j
         AR     R2,R5                    r2+karray*j
         LR     R11,R2                 n=100000*vect(j)+karray*j
         LR     R3,R7                    j
         SLA    R3,1                     2*j
         BCTR   R3,0                     2*j-1)
         LR     R4,R11                   n
         SRDA   R4,32                    .
         DR     R4,R3                    n/(2*j-1)
         LR     R9,R5                  karray=n/(2*j-1)
         LR     R5,R9                    karray
         MR     R4,R3                    karray*(2*j-1)
         LR     R1,R11                   n
         SR     R1,R5                    n-karray*(2*j-1)
         ST     R1,0(R10)              vect(j)=n-karray*(2*j-1)
         SH     R10,=H'4'              r10=@vect(j)
         BCT    R7,LOOPJ             end do j
         LR     R4,R9                karray
         SRDA   R4,32                .
         D      R4,=F'100000'        karray/100000
         LR     R11,R5               k=karray/100000
         L      R2,MORE              more
         AR     R2,R11               +k
         LR     R1,R6                i
         SLA    R1,2                 .
         ST     R2,BUF-4(R1)         buf(i)=more+k
         LR     R5,R11                 k
         M      R4,=F'100000'          *100000
         LR     R1,R9                  karray
         SR     R1,R5                  -k*100000
         ST     R1,MORE              more=karray-k*100000
         LA     R6,1(R6)             i=i+1
         B      LOOPI1             end do i
ELOOPI1  L      R1,BUF             buf(1)
         CVD    R1,PACKED          convert buf(1) to packed decimal
         OI     PACKED+7,X'0F'     prepare unpack
         UNPK   PG(1),PACKED       packed decimal to zoned printable
         MVI    PG+1,C'.'          output '.'
         XPRNT  PG,80              print buffer
         MVC    PG,=CL80' '        clear buffer
         LA     R3,PG              pgi=0
         LA     R6,2               i=2
LOOPI2   C      R6,=A(NBUF)        do i=2 to hbound(buf)
         BH     ELOOPI2            "
         MVC    0(1,R3),=C' '        output ' '
         LA     R3,1(R3)             pgi=pgi+1
         LR     R1,R6                i
         SLA    R1,2                 .
         L      R2,BUF-4(R1)         buf(i)
         CVD    R2,PACKED            convert v to packed decimal
         OI     PACKED+7,X'0F'       prepare unpack
         UNPK   XDEC,PACKED          packed decimal to zoned printable
         MVC    0(5,R3),XDEC+7       output buf(i) with 5 decimals
         LA     R3,5(R3)             pgi=pgi+5
         LR     R4,R6                i
         BCTR   R4,0                 i-1
         SRDA   R4,32                .
         D      R4,=F'10'            (i-1)/10
         LTR    R4,R4                if (i-1)//10=0
         BNZ    NOSKIP               then
         XPRNT  PG,80                  print buffer
         LA     R3,PG                  pgi=0
         MVC    PG,=CL80' '            clear buffer
NOSKIP   LA     R6,1(R6)             i=i+1
         B      LOOPI2             end do i
ELOOPI2  L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
         LTORG
MORE     DS     F                  more
PACKED   DS     0D,PL8             packed decimal
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp
BUF      DC     (NBUF)F'0'         buf(nbuf)
VECT     DC     (NVECT)F'2'        vect(nvect) init 2
         YREGS
NBUF     EQU    201                number of 5 decimals
NVECT    EQU    3350               nvect=ceil(nbuf*50/3)
         END    PISPIG
```

```txt

3.
 14159 26535 89793 23846 26433 83279 50288 41971 69399 37510
 58209 74944 59230 78164 06286 20899 86280 34825 34211 70679
 82148 08651 32823 06647 09384 46095 50582 23172 53594 08128
 48111 74502 84102 70193 85211 05559 64462 29489 54930 38196
 44288 10975 66593 34461 28475 64823 37867 83165 27120 19091
 45648 56692 34603 48610 45432 66482 13393 60726 02491 41273
 72458 70066 06315 58817 48815 20920 96282 92540 91715 36436
 78925 90360 01133 05305 48820 46652 13841 46951 94151 16094
 33057 27036 57595 91953 09218 61173 81932 61179 31051 18548
 07446 23799 62749 56735 18857 52724 89122 79381 83011 94912
 98336 73362 44065 66430 86021 39494 63952 24737 19070 21798
 60943 70277 05392 17176 29317 67523 84674 81846 76694 05132
 00056 81271 45263 56082 77857 71342 75778 96091 73637 17872
 14684 40901 22495 34301 46549 58537 10507 92279 68925 89235
 42019 95611 21290 21960 86403 44181 59813 62977 47713 09960
 51870 72113 49999 99837 29780 49951 05973 17328 16096 31859
 50244 59455 34690 83026 42522 30825 33446 85035 26193 11881
 71010 00313 78387 52886 58753 32083 81420 61717 76691 47303
 59825 34904 28755 46873 11595 62863 88235 37875 93751 95778
 18577 80532 17122 68066 13001 92787 66111 95909 21642 01989

```



## Ada

uses same algorithm as Go solution, from http://web.comlab.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf
;pi_digits.adb:

```Ada
with Ada.Command_Line;
with Ada.Text_IO;
with GNU_Multiple_Precision.Big_Integers;
with GNU_Multiple_Precision.Big_Rationals;
use GNU_Multiple_Precision;

procedure Pi_Digits is
   type Int is mod 2 ** 64;
   package Int_To_Big is new Big_Integers.Modular_Conversions (Int);

   -- constants
   Zero : constant Big_Integer := Int_To_Big.To_Big_Integer (0);
   One : constant Big_Integer := Int_To_Big.To_Big_Integer (1);
   Two : constant Big_Integer := Int_To_Big.To_Big_Integer (2);
   Three : constant Big_Integer := Int_To_Big.To_Big_Integer (3);
   Four : constant Big_Integer := Int_To_Big.To_Big_Integer (4);
   Ten : constant Big_Integer := Int_To_Big.To_Big_Integer (10);

   -- type LFT = (Integer, Integer, Integer, Integer
   type LFT is record
      Q, R, S, T : Big_Integer;
   end record;

   -- extr :: LFT -> Integer -> Rational
   function Extr (T : LFT; X : Big_Integer) return Big_Rational is
      use Big_Integers;
      Result : Big_Rational;
   begin
      -- extr (q,r,s,t) x = ((fromInteger q) * x + (fromInteger r)) /
      --                    ((fromInteger s) * x + (fromInteger t))
      Big_Rationals.Set_Numerator (Item         => Result,
                                   New_Value    => T.Q * X + T.R,
                                   Canonicalize => False);
      Big_Rationals.Set_Denominator (Item      => Result,
                                     New_Value => T.S * X + T.T);
      return Result;
   end Extr;

   -- unit :: LFT
   function Unit return LFT is
   begin
      -- unit = (1,0,0,1)
      return LFT'(Q => One, R => Zero, S => Zero, T => One);
   end Unit;

   -- comp :: LFT -> LFT -> LFT
   function Comp (T1, T2 : LFT) return LFT is
      use Big_Integers;
   begin
      -- comp (q,r,s,t) (u,v,w,x) = (q*u+r*w,q*v+r*x,s*u+t*w,s*v+t*x)
      return LFT'(Q => T1.Q * T2.Q + T1.R * T2.S,
                  R => T1.Q * T2.R + T1.R * T2.T,
                  S => T1.S * T2.Q + T1.T * T2.S,
                  T => T1.S * T2.R + T1.T * T2.T);
   end Comp;

   -- lfts = [(k, 4*k+2, 0, 2*k+1) | k<-[1..]
   K : Big_Integer := Zero;
   function LFTS return LFT is
      use Big_Integers;
   begin
      K := K + One;
      return LFT'(Q => K,
                  R => Four * K + Two,
                  S => Zero,
                  T => Two * K + One);
   end LFTS;

   -- next z = floor (extr z 3)
   function Next (T : LFT) return Big_Integer is
   begin
      return Big_Rationals.To_Big_Integer (Extr (T, Three));
   end Next;

   -- safe z n = (n == floor (extr z 4)
   function Safe (T : LFT; N : Big_Integer) return Boolean is
   begin
      return N = Big_Rationals.To_Big_Integer (Extr (T, Four));
   end Safe;

   -- prod z n = comp (10, -10*n, 0, 1)
   function Prod (T : LFT; N : Big_Integer) return LFT is
      use Big_Integers;
   begin
      return Comp (LFT'(Q => Ten, R => -Ten * N, S => Zero, T => One), T);
   end Prod;

   procedure Print_Pi (Digit_Count : Positive) is
      Z : LFT := Unit;
      Y : Big_Integer;
      Count : Natural := 0;
   begin
      loop
         Y := Next (Z);
         if Safe (Z, Y) then
            Count := Count + 1;
            Ada.Text_IO.Put (Big_Integers.Image (Y));
            exit when Count >= Digit_Count;
            Z := Prod (Z, Y);
         else
            Z := Comp (Z, LFTS);
         end if;
      end loop;
   end Print_Pi;

   N : Positive := 250;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      N := Positive'Value (Ada.Command_Line.Argument (1));
   end if;
   Print_Pi (N);
end Pi_Digits;
```

output:

```txt
 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0 5 8 2 0 9 7 4 9 4 4 5 9 2 3 0 7 8 1 6 4 0 6 2 8 6 2 0 8 9 9 8 6 2 8 0 3 4 8 2 5 3 4 2 1 1 7 0 6 7
```



## AutoHotkey

Could be optimized with Ipp functions, but runs fast enough for me as-is. Does not work in AHKLx64.

```autohotkey
#NoEnv
#SingleInstance, Force
SetBatchLines, -1
#Include mpl.ahk
dot:=".", i:=0
, MP_SET(q, "1")
, MP_SET(r, "0")
, MP_SET(t, "1")
, MP_SET(k, "1")
, MP_SET(n, "3")
, MP_SET(l, "3")
, MP_SET(ONE, "1")
, MP_SET(TWO, "2")
, MP_SET(THREE, "3")
, MP_SET(FOUR, "4")
, MP_SET(SEVEN, "7")
, MP_SET(TEN, "10")

Loop
{
	MP_MUL(q4, q, FOUR)
	, MP_ADD(q4r, q4, r)
	, MP_SUB(q4rt, q4r, t)
	, MP_MUL(tn, t, n)
	If (MP_CMP(q4rt,tn) = -1)
	{
		s := MP_DEC(n) . dot
		OutputDebug %s%
		  dot := ""
		, i++
		, MP_MUL(tn, t, n)
		, MP_SUB(rtn, r, tn)
		, MP_MUL(nr, rtn, TEN)
		, MP_MUL(q3, q, THREE)
		, MP_ADD(q3r, q3, r)
		, MP_DIV(q3rt, remainder, q3r, t)
		, MP_SUB(q3rtn, q3rt, n)
		, MP_MUL(n, q3rtn, TEN)
		, MP_MUL(tmp, q, TEN)
		, MP_CPY(q, tmp)
		, MP_CPY(r, nr)
	}
	Else
	{
		MP_MUL(q2, q, TWO)
		, MP_ADD(q2r, q2, r)
		, MP_MUL(nr, q2r, l)
		, MP_MUL(k7, k, SEVEN)
		, MP_ADD(k72, k7, TWO)
		, MP_MUL(qk, q, k72)
		, MP_MUL(rl, r, l)
		, MP_ADD(qkrl, qk, rl)
		, MP_MUL(tl, t, l)
		, MP_DIV(nn, remainder, qkrl, tl)
		, MP_MUL(tmp, q, k)
		, MP_CPY(q, tmp)
		, MP_MUL(tmp, t, l)
		, MP_CPY(t, tmp)
		, MP_ADD(tmp, l, TWO)
		, MP_CPY(l, tmp)
		, MP_ADD(tmp, k, ONE)
		, MP_CPY(k, tmp)
		, MP_CPY(n, nn)
		, MP_CPY(r, nr)
	}
}
```



## ALGOL 68

{{trans|Pascal}} Note: This specimen retains the original [[#Pascal|Pascal]] coding style of [http://www.mathpropress.com/stan/bibliography/spigot.pdf code].
This codes uses 33 decimals places as a test case.  Performance is O(2) based on the number of decimal places required.

```algol68
#!/usr/local/bin/a68g --script #

INT base := 10;

MODE YIELDINT = PROC(INT)VOID;
PROC gen pi digits = (INT decimal places, YIELDINT yield)VOID:
BEGIN
  INT nine = base - 1;
  INT nines := 0, predigit := 0; # First predigit is a 0 #
  [decimal places*10 OVER 3]#LONG# INT digits; # We need 3 times the digits to calculate #
  FOR place FROM LWB digits TO UPB digits DO digits[place] := 2 OD; # Start with 2s #
  FOR place TO decimal places + 1 DO
    INT digit := 0;
    FOR i FROM UPB digits BY -1 TO LWB digits DO # Work backwards #
        INT x := #SHORTEN#(base*digits[i] + #LENG# digit*i);
        digits[i] := x MOD (2*i-1);
        digit := x OVER (2*i-1)
    OD;
    digits[LWB digits] := digit MOD base; digit OVERAB base;
    nines :=
      IF digit = nine THEN
        nines + 1
      ELSE
        IF digit = base THEN
          yield(predigit+1); predigit := 0 ;
          FOR repeats TO nines DO yield(0) OD # zeros #
        ELSE
          IF place NE 1 THEN yield(predigit) FI; predigit := digit;
          FOR repeats TO nines DO yield(nine) OD
        FI;
        0
      FI
  OD;
  yield(predigit)
END;

main:(
  INT feynman point = 762; # feynman point + 4 is a good test case #
# the 33rd decimal place is a shorter tricky test case #
  INT test decimal places = UPB "3.1415926.......................502"-2;

  INT width = ENTIER log(base*(1+small real*10));

# iterate throught the digits as they are being found #
# FOR INT digit IN # gen pi digits(test decimal places#) DO ( #,
  ## (INT digit)VOID: (
    printf(($n(width)d$,digit))
  )
# OD #);
  print(new line)
)
```

Output:

```txt

3141592653589793238462643383279502

```



## BASIC


=
## Applesoft
=

```basic
10 rem adopted from Commodore BASIC
20 n = 100 : rem N may be increased, but will slow execution
30 ln = int(10*n/4)
40 nd = 1
50 dim a(ln)
60 n9 = 0
70 pd = 0 :rem First pre-digit is a 0
80 rem
90 for j = 1 to ln
100    a(j-1) = 2 :rem Start with 2s
110 next j
120 rem
130 for j = 1 to n
140     q = 0
150     for i = ln to 1 step -1 :rem Work backwards
160         x = 10*a(i-1) + q*i
170         a(i-1) = x - (2*i-1)*int(x/(2*i-1)) :rem X - INT ( X / Y) * Y
180         q = int(x/(2*i - 1))
190     next i
200     a(0) = q-10*int(q/10)
210     q = int(q/10)
220     if q=9 then n9 = n9 + 1 : goto 450
240     if q<>10 then goto 350
250     rem q == 10
260        d = pd+1 : gosub 500
270        if n9 < 0 then goto 320
280           for k = 1 to n9
290              d = 0: gosub 500
300           next k
310        rem end if
320        pd = 0
330        n9 = 0
335        goto 450
340     rem q <> 10
350        d = pd: gosub 500
360        pd = q
370        if n9 = 0 then goto 450
380           for k = 1 to n9
390              d = 9 : gosub 500
400           next k
410           n9 = 0
450 next j
460 print str$(pd)
470 end
480 rem
490 rem output digits
500 if nd=0 then print str$(d); : return
510 if d=0 then return
520 print str$(d);".";
530 nd = 0
550 return

```


=
## BASIC256
=
{{Trans|Pascal}} below, and originally published by Stanley Rabinowitz in [http://www.mathpropress.com/stan/bibliography/spigot.pdf].

```BASIC256
cls

n   =1000
len = 10*n \ 4
needdecimal = true
dim a(len)
nines = 0
predigit = 0	# {First predigit is a 0}

for j = 1 to len
   a[j-1] = 2	# {Start with 2s}
next j

for j = 1 to n
   q = 0
   for i = len to 1 step -1
      #  {Work backwards}
      x   = 10*a[i-1] + q*i
      a[i-1] = x % (2*i - 1)
      q    = x \ (2*i - 1)
   next i
   a[0] = q % 10
   q = q \ 10
   if q = 9 then
      nines = nines + 1
   else
      if q = 10 then
         d = predigit+1: gosub outputd
         if nines > 0 then
            for k = 1 to nines
               d =  0: gosub outputd
            next k
         end if
         predigit = 0
         nines = 0
      else
         d = predigit: gosub outputd
         predigit = q
         if nines <> 0 then
            for k = 1 to nines
               d =  9: gosub outputd
            next k
            nines = 0
         end if
      end if
   end if
next j
print predigit
end

outputd:
if needdecimal then
   if d = 0 then return
   print d + ".";
   needdecimal = false
else
   print d;
end if
return
```


Output:

```txt

3.14159265358979323846264338327950288419716939937510582097494459230781...

```


=
## Commodore BASIC
=

```basic
10 PRINT CHR$(147)
20 n = 100
30 ln = int(10*n/4)
40 nd = 1
50 dim a(ln)
60 n9 = 0
70 pd = 0 :rem First predigit is a 0
80 :
90 for j = 1 to ln
100    a(j-1) = 2 :rem Start with 2s
110 next j
120 :
130 for j = 1 to n
140     q = 0
150     for i = ln to 1 step -1 :rem Work backwards
160         x = 10*a(i-1) + q*i
170         a(i-1) = x - (2*i-1)*int(x/(2*i-1)) :rem X - INT ( X / Y) * Y
180         q = int(x/(2*i - 1))
190     next i
200     a(0) = q-10*int(q/10)
210     q = int(q/10)
220     if q=9 then n9 = n9 + 1 : goto 450
240     if q<>10 then 350
250     rem q == 10
260        d = pd+1 : gosub 500
270        if n9 < 0 then 320
280           for k = 1 to n9
290              d = 0: gosub 500
300           next k
310        rem end if
320        pd = 0
330        n9 = 0
335        goto 450
340     rem q <> 10
350        d = pd: gosub 500
360        pd = q
370        if n9 = 0 then 450
380           for k = 1 to n9
390              d = 9 : gosub 500
400           next k
410           n9 = 0
450 next j
460 print mid$(str$(pd),2,1)
470 end
480 :
490 rem outputd
500 if nd=0 then print mid$(str$(d),2,1); : return
510 if d=0 then return
520 print mid$(str$(d),2,1);".";
530 nd = 0
550 return

```



## BBC BASIC


### BASIC version


```bbcbasic
      WIDTH 80
      M% = (HIMEM-END-1000) / 4
      DIM B%(M%)
      FOR I% = 0 TO M% : B%(I%) = 20 : NEXT
      E% = 0
      L% = 2
      FOR C% = M% TO 14 STEP -7
        D% = 0
        A% = C%*2-1
        FOR P% = C% TO 1 STEP -1
          D% = D%*P% + B%(P%)*&64
          B%(P%) = D% MOD A%
          D% DIV= A%
          A% -= 2
        NEXT
        CASE TRUE OF
          WHEN D% = 99: E% = E% * 100 + D% : L% += 2
          WHEN C% = M%: PRINT ;(D% DIV 100) / 10; : E% = D% MOD 100
          OTHERWISE:
            PRINT RIGHT$(STRING$(L%,"0") + STR$(E% + D% DIV 100),L%);
            E% = D% MOD 100 : L% = 2
        ENDCASE
      NEXT
```



### Assembler version

The first 250,000 digits output have been verified.

```bbcbasic
      DIM P% 32
      [OPT 2 :.pidig mov ebp,eax :.pi1 imul edx,ecx : mov eax,[ebx+ecx*4]
      imul eax,100 : add eax,edx : cdq : div ebp : mov [ebx+ecx*4],edx
      mov edx,eax : sub ebp,2 : loop pi1 : mov eax,edx : ret :]

      WIDTH 80
      M% = (HIMEM-END-1000) / 4
      DIM B%(M%) : B% = ^B%(0)
      FOR I% = 0 TO M% : B%(I%) = 20 : NEXT
      E% = 0
      L% = 2
      FOR C% = M% TO 14 STEP -7
        D% = 0
        A% = C%*2-1
        D% = USR(pidig)
        CASE TRUE OF
          WHEN D% = 99: E% = E% * 100 + D% : L% += 2
          WHEN C% = M%: PRINT ;(D% DIV 100) / 10; : E% = D% MOD 100
          OTHERWISE:
            PRINT RIGHT$(STRING$(L%,"0") + STR$(E% + D% DIV 100),L%);
            E% = D% MOD 100 : L% = 2
        ENDCASE
      NEXT
```

'''Output:'''

```txt

3.141592653589793238462643383279502884197169399375105820974944592307816406286208
99862803482534211706798214808651328230664709384460955058223172535940812848111745
02841027019385211055596446229489549303819644288109756659334461284756482337867831
65271201909145648566923460348610454326648213393607260249141273724587006606315588
17488152092096282925409171536436789259036001133053054882046652138414695194151160
94330572703657595919530921861173819326117931051185480744623799627495673518857527
24891227938183011949129833673362440656643086021394946395224737190702179860943702
77053921717629317675238467481846766940513200056812714526356082778577134275778960
91736371787214684409012249534301465495853710507922796892589235420199561121290219
60864034418159813629774771309960518707211349999998372978049951059731732816096318
....

```



## bc

The digits of Pi are printed 20 per line, by successively recomputing pi with higher precision.  The computation is not accurate to the entire scale (for example, <code>scale = 4; 4*a(1)</code> prints ''3.1412'' instead of the expected ''3.1415''), so the program includes two excess digits in the scale.  Fixed number of guarding digits will eventually fail because Pi can contain arbitrarily long sequence of consecutive 9s (or consecutive 0s), though for this task it might not matter in practice.  The program proceeds more and more slowly but exploits <tt>bc</tt>'s unlimited precision arithmetic.

The program uses three features of [[GNU bc]]: long variable names, # comments (for the #! line), and the <code>print</code> command (for zero padding).
```bc
#!/usr/bin/bc -l

scaleinc= 20

define zeropad ( n ) {
    auto m
    for ( m= scaleinc - 1; m > 0; --m ) {
        if ( n < 10^m ) {
            print "0"
        }
    }
    return ( n )
}

wantscale= scaleinc - 2
scale= wantscale + 2
oldpi= 4*a(1)
scale= wantscale
oldpi= oldpi / 1
oldpi
while( 1 ) {
    wantscale= wantscale + scaleinc
    scale= wantscale + 2
    pi= 4*a(1)
    scale= 0
    digits= ((pi - oldpi) * 10^wantscale) / 1
    zeropad( digits )
    scale= wantscale
    oldpi= pi / 1
}
```

Output:

```txt

3.141592653589793238
46264338327950288419
71693993751058209749
44592307816406286208
99862803482534211706
79821480865132823066
47093844609550582231
72535940812848111745
02841027019385211055
59644622948954930381
96442881097566593344
61284756482337867831
65271201909145648566
92346034861045432664
82133936072602491412
73724587006606315588
17488152092096282925
40917153643678925903
60011330530548820466
52138414695194151160
94330572703657595919
....

```



## Bracmat

```bracmat
  ( pi
  =   f,q r t k n l,first
    .   !arg:((=?f),?q,?r,?t,?k,?n,?l)
      & yes:?first
      &   whl
        ' (   4*!q+!r+-1*!t+-1*!n*!t:<0
            & f$!n
            & (   !first:yes
                & f$"."
                & no:?first
              |
              )
            & "compute and update variables for next cycle"
            & 10*(!r+-1*!n*!t):?nr
            & div$(10*(3*!q+!r).!t)+-10*!n:?n
            & !q*10:?q
            & !nr:?r
          |   "compute and update variables for next cycle"
            & (2*!q+!r)*!l:?nr
            & div$(!q*(7*!k+2)+!r*!l.!t*!l):?nn
            & !q*!k:?q
            & !t*!l:?t
            & !l+2:?l
            & !k+1:?k
            & !nn:?n
            & !nr:?r
          )
  )
& pi$((=.put$!arg),1,0,1,1,3,3)
```

Output:

```txt
3.1415926535897932384626433832795028841971693993751058209749445923078164062
862089986280348253421170679821480865132823066470938446095505822317253594081
284811174502841027019385211055596446229489549303819644288109756659334461284
756482337867831652712019091456485669234603486104543266482133936072602491412
73724587006606315588174881520...
```



## C

There are many ways to do this, with quite different performance profiles.  A simple measurement of 6 programs:
{| class="wikitable"
|-
! Digits
! Spigot 1
! Spigot 2
! Machin 1
! Machin 2
! AGM
! Chudnovsky
|- align="right"
!     1,000
| 0.008
| 0.009
| 0.001
| 0.001
| 0.000
| 0.000
|- align="right"
!    10,000
| 0.402
| 0.589
| 0.020
| 0.016
| 0.003
| 0.002
|- align="right"
!   100,000
| 39.400
| 85.600
| 1.740
| 1.480
| 0.084
| 0.002
|- align="right"
!  1,000,000
|
|
| 177.900
| 156.800
| 1.474
| 0.333
|- align="right"
! 10,000,000
|
|
|
|
| 25.420
| 5.715
|}
* Spigot 1: plain C (no GMP), modified Winter/Flammenkamp, correct to 1+M digits
* Spigot 2: C+GMP, as used in [http://shootout.alioth.debian.org/ Computer Language Benchmarks Game]
* Machin 1: C+GMP, shown below
* Machin 2: C+GMP, as below but using Chien-Lih 1997 formula
* AGM: C+GMP, essentially from the [[Arithmetic-geometric mean/Calculate Pi]] task.  This has performance only slightly slower than MPFR.
* Chudnovsky: Hanhong Xue's code from [https://gmplib.org/pi-with-gmp.html GMP web site].


Using Machin's formula.  The "continuous printing" part is silly: the algorithm really calls for a preset number of digits, so the program repeatedly calculates Pi digits with increasing length and chop off leading digits already displayed.  But it's still faster than the unbounded Spigot method by an order of magnitude, at least for the first 100k digits.

```c
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

mpz_t tmp1, tmp2, t5, t239, pows;
void actan(mpz_t res, unsigned long base, mpz_t pows)
{
	int i, neg = 1;
	mpz_tdiv_q_ui(res, pows, base);
	mpz_set(tmp1, res);
	for (i = 3; ; i += 2) {
		mpz_tdiv_q_ui(tmp1, tmp1, base * base);
		mpz_tdiv_q_ui(tmp2, tmp1, i);
		if (mpz_cmp_ui(tmp2, 0) == 0) break;
		if (neg) mpz_sub(res, res, tmp2);
		else	  mpz_add(res, res, tmp2);
		neg = !neg;
	}
}

char * get_digits(int n, size_t* len)
{
	mpz_ui_pow_ui(pows, 10, n + 20);

	actan(t5, 5, pows);
	mpz_mul_ui(t5, t5, 16);

	actan(t239, 239, pows);
	mpz_mul_ui(t239, t239, 4);

	mpz_sub(t5, t5, t239);
	mpz_ui_pow_ui(pows, 10, 20);
	mpz_tdiv_q(t5, t5, pows);

	*len = mpz_sizeinbase(t5, 10);
	return mpz_get_str(0, 0, t5);
}

int main(int c, char **v)
{
	unsigned long accu = 16384, done = 0;
	size_t got;
	char *s;

	mpz_init(tmp1);
	mpz_init(tmp2);
	mpz_init(t5);
	mpz_init(t239);
	mpz_init(pows);

	while (1) {
		s = get_digits(accu, &got);

		/* write out digits up to the last one not preceding a 0 or 9*/
		got -= 2; /* -2: length estimate may be longer than actual */
		while (s[got] == '0' || s[got] == '9') got--;

		printf("%.*s", (int)(got - done), s + done);
		free(s);

		done = got;

		/* double the desired digits; slows down at least cubically */
		accu *= 2;
	}

	return 0;
}
```


## C#
'''Translation of:''' Java


```c#
using System;
using System.Numerics;

namespace PiCalc {
    internal class Program {
        private readonly BigInteger FOUR = new BigInteger(4);
        private readonly BigInteger SEVEN = new BigInteger(7);
        private readonly BigInteger TEN = new BigInteger(10);
        private readonly BigInteger THREE = new BigInteger(3);
        private readonly BigInteger TWO = new BigInteger(2);

        private BigInteger k = BigInteger.One;
        private BigInteger l = new BigInteger(3);
        private BigInteger n = new BigInteger(3);
        private BigInteger q = BigInteger.One;
        private BigInteger r = BigInteger.Zero;
        private BigInteger t = BigInteger.One;

        public void CalcPiDigits() {
            BigInteger nn, nr;
            bool first = true;
            while (true) {
                if ((FOUR*q + r - t).CompareTo(n*t) == -1) {
                    Console.Write(n);
                    if (first) {
                        Console.Write(".");
                        first = false;
                    }
                    nr = TEN*(r - (n*t));
                    n = TEN*(THREE*q + r)/t - (TEN*n);
                    q *= TEN;
                    r = nr;
                } else {
                    nr = (TWO*q + r)*l;
                    nn = (q*(SEVEN*k) + TWO + r*l)/(t*l);
                    q *= k;
                    t *= l;
                    l += TWO;
                    k += BigInteger.One;
                    n = nn;
                    r = nr;
                }
            }
        }

        private static void Main(string[] args) {
            new Program().CalcPiDigits();
        }
    }
}
```


Adopted Version:

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace EnumeratePi {
  class Program {
    private const int N = 60;
    private const string ZS = " +-";
    static void Main() {
      Console.WriteLine("Digits of PI");
      Console.WriteLine(new string('=', N + 13));

      Console.WriteLine("Decimal    : {0}", string.Concat(PiDigits(10).Take(N).Select(_ => _.ToString("d"))));
      Console.WriteLine("Binary     : {0}", string.Concat(PiDigits(2).Take(N).Select(_ => _.ToString("d"))));
      Console.WriteLine("Quaternary : {0}", string.Concat(PiDigits(4).Take(N).Select(_ => _.ToString("d"))));
      Console.WriteLine("Octal      : {0}", string.Concat(PiDigits(8).Take(N).Select(_ => _.ToString("d"))));
      Console.WriteLine("Hexadecimal: {0}", string.Concat(PiDigits(16).Take(N).Select(_ => _.ToString("x"))));
      Console.WriteLine("Alphabetic : {0}", string.Concat(PiDigits(26).Take(N).Select(_ => (char) ('A' + _))));
      Console.WriteLine("Fun        : {0}", string.Concat(PiDigits(ZS.Length).Take(N).Select(_ => ZS[(int)_])));

      Console.WriteLine("Nibbles    : {0}", string.Concat(PiDigits(0x10).Take(N/2).Select(_ => string.Format("{0:x1} ", _))));
      Console.WriteLine("Bytes      : {0}", string.Concat(PiDigits(0x100).Take(N/3).Select(_ => string.Format("{0:x2} ", _))));
      Console.WriteLine("Words      : {0}", string.Concat(PiDigits(0x10000).Take(N/5).Select(_ => string.Format("{0:x4} ", _))));
      Console.WriteLine("Dwords     : {0}", string.Concat(PiDigits(0x100000000).Take(N/9).Select(_ => string.Format("{0:x8} ", _))));

      Console.WriteLine(new string('=', N + 13));
      Console.WriteLine("* press any key to exit *");
      Console.ReadKey();
    }

    /// <summary>Enumerates the digits of PI.</summary>
    /// <param name="b">Base of the Numeral System to use for the resulting digits (default = Base.Decimal (10)).</param>
    /// <returns>The digits of PI.</returns>
    static IEnumerable<long> PiDigits(long b = 10) {
      BigInteger
        k = 1,
        l = 3,
        n = 3,
        q = 1,
        r = 0,
        t = 1
        ;

      // skip integer part
      var nr = b * (r - t * n);
      n = b * (3 * q + r) / t - b * n;
      q *= b;
      r = nr;

      for (; ; ) {
        var tn = t * n;
        if (4 * q + r - t < tn) {
          yield return (long)n;
          nr = b * (r - tn);
          n = b * (3 * q + r) / t - b * n;
          q *= b;
        } else {
          t *= l;
          nr = (2 * q + r) * l;
          var nn = (q * (7 * k) + 2 + r * l) / t;
          q *= k;
          l += 2;
          ++k;
          n = nn;
        }
        r = nr;
      }
    }
  }
}
```

Output:
<lang>Digits of PI

### ===================================================================

Decimal    : 141592653589793238462643383279502884197169399375105820974944
Binary     : 001001000011111101101010100010001000010110100011000010001101
Quaternary : 021003331222202020112203002031030103012120220232000313001303
Octal      : 110375524210264302151423063050560067016321122011160210514763
Hexadecimal: 243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e
Alphabetic : DRSQLOLYRTRODNLHNQTGKUDQGTUIRXNEQBCKBSZIVQQVGDMELMUEXROIQIYA
Fun        :  + -++ +---- + -++  -+++++ --+----- +++- +-+-+-+-  +-++  +
Nibbles    : 2 4 3 f 6 a 8 8 8 5 a 3 0 8 d 3 1 3 1 9 8 a 2 e 0 3 7 0 7 3
Bytes      : 24 3f 6a 88 85 a3 08 d3 13 19 8a 2e 03 70 73 44 a4 09 38 22
Words      : 243f 6a88 85a3 08d3 1319 8a2e 0370 7344 a409 3822 299f 31d0
Dwords     : 243f6a88 85a308d3 13198a2e 03707344 a4093822 299f31d0

### ===================================================================

* press any key to exit *
```



## Clojure

```lisp
(ns pidigits
  (:gen-class))

(def calc-pi
        ;  integer division rounding downwards to -infinity
  (let [div (fn [x y] (long (Math/floor (/ x y))))

          ; Computations performed after yield clause in Python code
          update-after-yield (fn [[q r t k n l]]
                         (let [nr (* 10 (- r (* n t)))
                               nn (- (div (* 10 (+ (* 3 q) r)) t) (* 10 n))
                               nq (* 10 q)]
                           [nq nr t k nn l]))

          ; Update of else clause in Python code: if (< (- (+ (* 4 q) r) t) (* n t))
          update-else (fn [[q r t k n l]]
                        (let [nr (* (+ (* 2 q) r) l)
                              nn (div (+ (* q 7 k) 2 (* r l)) (* t l))
                              nq (* k q)
                              nt (* l t)
                              nl (+ 2 l)
                              nk (+ 1 k)]
                          [nq nr nt nk nn nl]))

          ; Compute the lazy sequence of pi digits translating the Python code
          pi-from (fn pi-from [[q r t k n l]]
                    (if (< (- (+ (* 4 q) r) t) (* n t))
                      (lazy-seq (cons n (pi-from (update-after-yield [q r t k n l]))))
                      (recur (update-else [q r t k n l]))))]

      ; Use Clojure big numbers to perform the math (avoid integer overflow)
      (pi-from [1N 0N 1N 1N 3N 3N])))

;; Indefinitely Output digits of pi, with 40 characters per line
(doseq [[i q] (map-indexed vector calc-pi)]
  (when (= (mod i 40) 0)
    (println))
  (print q))

```

```txt

3141592653589793238462643383279502884197
1693993751058209749445923078164062862089
9862803482534211706798214808651328230664
7093844609550582231725359408128481117450
...

```


## Common Lisp


```lisp
(defun pi-spigot ()
  (labels
      ((g (q r t1 k n l)
         (cond
           ((< (- (+ (* 4 q) r) t1)
               (* n t1))
            (princ n)
            (g (* 10 q)
               (* 10 (- r (* n t1)))
               t1
               k
               (- (floor (/ (* 10 (+ (* 3 q) r))
                            t1))
                  (* 10 n))
               l))
           (t
            (g (* q k)
               (* (+ (* 2 q) r) l)
               (* t1 l)
               (+ k 1)
               (floor (/ (+ (* q (+ (* 7 k) 2))
                            (* r l))
                         (* t1 l)))
               (+ l 2))))))
    (g 1 0 1 1 3 3)))
```

```txt
CL-USER> (pi-spigot)
3141592653589793238462643383279502884197169399375105820974944592307816406286 ...
```



## Crystal

```ruby
require "big"

def pi
  q, r, t, k, n, l = [1, 0, 1, 1, 3, 3].map { |n| BigInt.new(n) }
  dot_written = false
  loop do
    if 4*q + r - t < n*t
      yield n
      unless dot_written
        yield '.'
        dot_written = true
      end
      nr = 10*(r - n*t)
      n = ((10*(3*q + r)) / t) - 10*n
      q *= 10
      r = nr
    else
      nr = (2*q + r) * l
      nn = (q*(7*k + 2) + r*l) / (t*l)
      q *= k
      t *= l
      l += 2
      k += 1
      n = nn
      r = nr
    end
  end
end

pi { |digit_or_dot| print digit_or_dot; STDOUT.flush }

```

```txt

3.141592653589793238462643383279502884197169399375105820974944592307816406286 ...
```



## D

This modified [[wp:Spigot_algorithm|Spigot algorithm]] does not continue infinitely, because its required memory grow as the number of digits need to print.

```d
import std.stdio, std.conv, std.string;

struct PiDigits {
    immutable uint nDigits;

    int opApply(int delegate(ref string /*chunk of pi digit*/) dg){
        // Maximum width for correct output, for type ulong.
        enum size_t width = 9;

        enum ulong scale = 10UL ^^ width;
        enum ulong initDigit = 2UL * 10UL ^^ (width - 1);
        enum string formatString = "%0" ~ text(width) ~ "d";

        immutable size_t len = 10 * nDigits / 3;
        auto arr = new ulong[len];
        arr[] = initDigit;
        ulong carry;

        foreach (i; 0 .. nDigits / width) {
            ulong sum;
            foreach_reverse (j; 0 .. len) {
                auto quo = sum * (j + 1) + scale * arr[j];
                arr[j] = quo % (j*2 + 1);
                sum = quo / (j*2 + 1);
            }
            auto yield = format(formatString, carry + sum/scale);
            if (dg(yield))
                break;
            carry = sum % scale;
        }
        return 0;
    }
}

void main() {
    foreach (d; PiDigits(100))
        writeln(d);
}
```

Output:

```txt
314159265
358979323
846264338
327950288
419716939
937510582
097494459
230781640
628620899
862803482
534211706
```


### Alternative version


```d
import std.stdio, std.bigint;

void main() {
    int ndigits = 0;
    auto q = BigInt(1);
    auto r = BigInt(0);
    auto t = q;
    auto k = q;
    auto n = BigInt(3);
    auto l = n;

    bool first = true;
    while (ndigits < 1_000) {
        if (4 * q + r - t < n * t) {
            write(n); ndigits++;
            if (ndigits % 70 == 0) writeln();
            if (first) { first = false; write('.'); }
            auto nr = 10 * (r - n * t);
            n = ((10 * (3 * q + r)) / t) - 10 * n;
            q *= 10;
            r = nr;
        } else {
            auto nr = (2    * q + r) * l;
            auto nn = (q * (7 * k + 2) + r * l) / (t * l);
            q *= k;
            t *= l;
            l += 2;
            k++;
            n = nn;
            r = nr;
        }
    }
}
```

Output:

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816
4062862089986280348253421170679821480865132823066470938446095505822317
2535940812848111745028410270193852110555964462294895493038196442881097
5665933446128475648233786783165271201909145648566923460348610454326648
2133936072602491412737245870066063155881748815209209628292540917153643
6789259036001133053054882046652138414695194151160943305727036575959195
3092186117381932611793105118548074462379962749567351885752724891227938
1830119491298336733624406566430860213949463952247371907021798609437027
7053921717629317675238467481846766940513200056812714526356082778577134
2757789609173637178721468440901224953430146549585371050792279689258923
5420199561121290219608640344181598136297747713099605187072113499999983
7297804995105973173281609631859502445945534690830264252230825334468503
5261931188171010003137838752886587533208381420617177669147303598253490
4287554687311595628638823537875937519577818577805321712268066130019278
76611195909216420198
```



## Elixir

```elixir
defmodule Pi do
  def calc, do: calc(1,0,1,1,3,3,0)

  defp calc(q,r,t,k,n,l,c) when c==50 do
    IO.write "\n"
    calc(q,r,t,k,n,l,0)
  end
  defp calc(q,r,t,k,n,l,c) when (4*q + r - t) < n*t do
    IO.write n
    calc(q*10, 10*(r-n*t), t, k, div(10*(3*q+r), t) - 10*n, l, c+1)
  end
  defp calc(q,r,t,k,_n,l,c) do
    calc(q*k, (2*q+r)*l, t*l, k+1, div(q*7*k+2+r*l, t*l), l+2, c)
  end
end

Pi.calc
```


Hit Ctrl-C to stop it.

```txt

C:\Elixir>elixir pi.exs
31415926535897932384626433832795028841971693993751
05820974944592307816406286208998628034825342117067
98214808651328230664709384460955058223172535940812
84811174502841027019385211055596446229489549303819
64428810975665933446128475648233786783165271201909
14564856692346034861045432664821339360726024914127
37245870066063155881748815209209628292540917153643
67892590360011330530548820466521384146951941511609
43305727036575959195309218611738193261179310511854
80744623799627495673518857527248912279381830119491
29833673362440656643086021394946395224737190702179
86094370277053921717629317675238467481846766940513
20005681271452635608277857713427577896091736371787
214684409012249534301

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(pi_calculation).
-export([main/0]).

main() ->
	pi(1,0,1,1,3,3,0).

pi(Q,R,T,K,N,L,C)   ->

	if C=:=50 ->
		io:format("\n"),
		pi(Q,R,T,K,N,L,0)  ;

	true ->

		if
			(4*Q + R-T) < (N*T) ->
			io:format("~p",[N]),
	 		P = 10*(R-N*T),
	 		pi(Q*10 , P, T , K , ((10*(3*Q+R)) div T)-10*N , L,C+1);

		true ->
			P = (2*Q+R)*L,
			M = (Q*(7*K)+2+(R*L)) div (T*L),
			H  = L+2,
			J =K+ 1,
			pi(Q*K, P , T*L ,J,M,H,C)
	 	end
 	end.

```

```txt
31415926535897932384626433832795028841971693993751
05820974944592307816406286208998628034825342117067
98214808651328230664709384460955058223172535940812
84811174502841027019385211055596446229489549303819
64428810975665933446128475648233786783165271201909
14564856692346034861045432664821339360726024914127
37245870066063155881748815209209628292540917153643
67892590360011330530548820466521384146951941511609
43305727036575959195309218611738193261179310511854
80744623799627495673518857527248912279381830119491
29833673362440656643086021394946395224737190702179
86094370277053921717629317675238467481846766940513
20005681271452635608277857713427577896091736371787
21468440901224953430146549585371050792279689258923
54201995611212902196086403441815981362977477130996
05187072113499999983729780499510597317328160963185
95024459455346908302642522308253344685035261931188
17101000313783875288658753320838142061717766914730
35982534904287554687311595628638823537875937519577
81857780532171226806613001927876611195909216420198
93809525720106548586327886593615338182796823030195
20353018529689957736225994138912497217752834791315
15574857242454150695950829533116861727855889075098
38175463746493931925506040092770167113900984882401
28583616035637076601047101819429555961989467678374
4944825537977472684710404753464620
```



=={{header|F_Sharp|F#}}==
```fsharp
let rec g q r t k n l = seq {
    if 4I*q+r-t < n*t
    then
        yield n
        yield! (g (10I*q) (10I*(r-n*t)) t k ((10I*(3I*q+r))/t - 10I*n) l)
    else
        yield! (g (q*k) ((2I*q+r)*l) (t*l) (k+1I) ((q*(7I*k+2I)+r*l)/(t*l)) (l+2I))
}

let π = (g 1I 0I 1I 1I 3I 3I)

Seq.take 1 π |> Seq.iter (printf "%A.")
// 6 digits beginning at position 762 of π are '9'
Seq.take 767 (Seq.skip 1 π) |> Seq.iter (printf "%A")
```

```txt
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066
470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831
652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903
600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527
248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051
320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219
6086403441815981362977477130996051870721134999999
```



## Factor

```factor
USING: combinators.extras io kernel locals math prettyprint ;
IN: rosetta-code.pi

:: calc-pi-digits ( -- )
    1 0 1 1 3 3 :> ( q! r! t! k! n! l! ) [
        4 q * r + t - n t * < [
            n pprint flush
            r n t * - 10 *
            3 q * r + 10 * t /i n 10 * - n! r!
            q 10 * q!
        ] [
            2 q * r + l *
            7 k * q * 2 + r l * + t l * /i n! r!
            k q * q!
            t l * t!
            l 2 + l!
            k 1 + k!
        ] if
    ] forever ;

MAIN: calc-pi-digits
```



## Fortran

This is a modernized version of the example Fortran programme written by S. Rabinowitz in 1991. It works in base 100000 and the key step is the initialisation of all elements of VECT to 2. The format code of I5.5 means I5 output but with all leading spaces made zero so that 66 comes out as "00066", not "   66".


```Fortran

program pi
  implicit none
  integer,dimension(3350) :: vect
  integer,dimension(201) :: buffer
  integer :: more,karray,num,k,l,n
  more = 0
  vect = 2
  do n = 1,201
    karray = 0
    do l = 3350,1,-1
      num = 100000*vect(l) + karray*l
      karray = num/(2*l - 1)
      vect(l) = num - karray*(2*l - 1)
    end do
    k = karray/100000
    buffer(n) = more + k
    more = karray - k*100000
  end do
  write (*,'(i2,"."/(1x,10i5.5))') buffer
end program pi

```

The output is accumulated in BUFFER then written in one go at the end, but it could be written as successive values as each is calculated without much extra nitpickery: instead of <code>BUFFER(N) = MORE + K</code> for example just <code>WRITE (*,"(I5.5)") MORE + K</code> and no need for array BUFFER.

```txt

3.
14159265358979323846264338327950288419716939937510
58209749445923078164062862089986280348253421170679
82148086513282306647093844609550582231725359408128
48111745028410270193852110555964462294895493038196
44288109756659334461284756482337867831652712019091
45648566923460348610454326648213393607260249141273
72458700660631558817488152092096282925409171536436
78925903600113305305488204665213841469519415116094
33057270365759591953092186117381932611793105118548
07446237996274956735188575272489122793818301194912
98336733624406566430860213949463952247371907021798
60943702770539217176293176752384674818467669405132
00056812714526356082778577134275778960917363717872
14684409012249534301465495853710507922796892589235
42019956112129021960864034418159813629774771309960
51870721134999999837297804995105973173281609631859
50244594553469083026425223082533446850352619311881
71010003137838752886587533208381420617177669147303
59825349042875546873115956286388235378759375195778
18577805321712268066130019278766111959092164201989

```


This is an alternate version using an unbounded spigot. Higher precision is accomplished by using the Fortran Multiple Precision
Library, FMLIB (http://myweb.lmu.edu/dmsmith/fmlib.html), provided by Dr. David M. Smith (dsmith@lmu.edu), Mathematics Professor (Emeritus) at Loyola Marymount University. We use the default precision which is about 50 significant digits.

```Fortran

!
### ==========================================

        program pi_spigot_unbounded
!
### ==========================================

          do
            call print_next_pi_digit()
          end do

        contains

!------------------------------------------------
          subroutine print_next_pi_digit()
!------------------------------------------------
            use fmzm
            type (im) :: q, r, t, k, n, l, nr
            logical   :: dot=.false., init=.false.
            save      :: q, r, t, k, n, l
            if (.not.init) then
              q=to_im(1)
              r=to_im(0)
              t=to_im(1)
              k=to_im(1)
              n=to_im(3)
              l=to_im(3)
              init=.true.
            end if
            if (4*q+r-t < n*t) then
              write(6,fmt='(i1)',advance='no') to_int(n)
              if (.not.dot) then
                write(6,fmt='(a1)',advance='no') '.'
                dot=.true.
              end if
              flush(6)
              nr = 10 * (        r      - n*t )
              n  = 10 * ( (3*q + r) / t - n   )
              q  = 10 *      q
              r  = nr
            else
              nr = (2*q + r) * l
              n  = ( (q * (7*k + 2) + r*l) / (t*l) )
              q  = q * k
              t  = t * l
              l  = l + 2
              k  = k + 1
              r  = nr
            end if
          end subroutine

        end program

```


## FreeBASIC

```freebasic
' version 05-07-2018
' compile with: fbc -s console

' unbounded spigot
' Ctrl-c to end program or close console window

#Include "gmp.bi"

Dim As UInteger num, ndigit, fp = Not 0
Dim As mpz_ptr q,r,t,k,n,l,tmp1,tmp2
   q = Allocate(Len(__Mpz_struct)) : Mpz_init_set_ui(q,1)
   r = Allocate(Len(__Mpz_struct)) : Mpz_init(r)
   t = Allocate(Len(__Mpz_struct)) : Mpz_init_set_ui(t,1)
   k = Allocate(Len(__Mpz_struct)) : Mpz_init_set_ui(k,1)
   n = Allocate(Len(__Mpz_struct)) : Mpz_init_set_ui(n,3)
   l = Allocate(Len(__Mpz_struct)) : Mpz_init_set_ui(l,3)
tmp1 = Allocate(Len(__Mpz_struct)) : Mpz_init(tmp1)
tmp2 = Allocate(Len(__Mpz_struct)) : Mpz_init(tmp2)

Do
    mpz_mul_2exp(tmp1, q, 2)
    mpz_add(tmp1,tmp1,r)
    mpz_sub(tmp1,tmp1,t)
    mpz_mul(tmp2, n, t)
    If mpz_cmp(tmp1, tmp2) < 0 Then
        Print mpz_get_ui(n); : ndigit += 1 : If ndigit Mod 50 = 0 Then Print " :";ndigit
        If fp Then Print "."; : fp = Not fp : Print :ndigit = 0
        mpz_sub(tmp1, r, tmp2)
        mpz_mul_ui(tmp1, tmp1, 10)
        mpz_mul_ui(tmp2, q, 3)
        mpz_add(tmp2, tmp2, r)
        mpz_mul_ui(tmp2, tmp2, 10)
        mpz_set(r, tmp1)
        mpz_mul_ui(tmp1, n, 10)
        mpz_tdiv_q(tmp2, tmp2, t)
        mpz_sub(n, tmp2, tmp1)
        mpz_mul_ui(q, q, 10)
    Else
        mpz_mul(tmp2, r, l)
        mpz_mul(tmp1, q, k)
        mpz_mul_ui(tmp1, tmp1, 7)
        mpz_add(tmp1, tmp1, tmp2)
        mpz_mul_2exp(tmp2, q, 1)
        mpz_add(tmp2, tmp2, r)
        mpz_mul(tmp2, tmp2, l)
        mpz_mul(t, t, l)
        mpz_tdiv_q(tmp1, tmp1, t)
        mpz_mul(q, q, k)
        mpz_add_ui(k, k, 1)
        mpz_add_ui(l, l, 2)
        mpz_set(n, tmp1)
        mpz_set(r, tmp2)
    End If
Loop
```

```txt
3.
14159265358979323846264338327950288419716939937510 :50
58209749445923078164062862089986280348253421170679 :100
82148086513282306647093844609550582231725359408128 :150
48111745028410270193852110555964462294895493038196 :200
44288109756659334461284756482337867831652712019091 :250
......
59284936959414340814685298150539471789004518357551 :20300
54125223590590687264878635752541911288877371766374 :20350
86027660634960353679470269232297186832771739323619 :20400
20077745221262475186983349515101986426988784717193 :20450
96649769070825217423365662725928440620430214113719 :20500
```



## FunL

The code for <code>compute_pi()</code> is from [http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf].  The number of digits may be given on the command line as an argument.  If there's no argument, the program will run until interrupted.


```funl
def compute_pi =
  def g( q, r, t, k, n, l ) =
    if 4*q + r - t < n*t
      n # g( 10*q, 10*(r - n*t), t, k, (10*(3*q + r))\t - 10*n, l )
    else
      g( q*k, (2*q + r)*l, t*l, k + 1, (q*(7*k + 2) + r*l)\(t*l), l + 2 )

  g( 1, 0, 1, 1, 3, 3 )

if _name_ == '-main-'
  print( compute_pi().head() + '.' )

  if args.isEmpty()
    for d <- compute_pi().tail()
      print( d )
  else
    for d <- compute_pi().tail().take( int(args(0)) )
      print( d )

    println()
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long  kf, ks
xref mf(_maxLong - 1) as long
xref ms(_maxLong - 1) as long
dim as long  cnt, n, temp, nd
dim as long  col, col1
dim as long  lloc, stor(50)

end globals

local mode
local fn FmtStr( nn as long, s as Str255 ) as Str255
dim l as long
dim as Str255 f
l = s[0]
select case
   case ( nn => l )
      f = string$( nn-l, 32 ) + s
   case ( -nn > l)
      f = s + string$( -nn-l, 32 )
   case else
      f = s
   end select
end fn = f


local mode
local fn FmtInt( nn as long, s as Str255 ) as Str255
if ( left$( s, 1 ) = " " ) then s = mid$( s, 2 )
end fn = fn FmtStr( nn, s )


local fn yprint( m as long )
if ( cnt < n )
   col++
   if ( col == 11 )
      col = 1
      col1++
         long if ( col1 == 6 )
            col1 = 0
            print
            print fn FmtInt( 4, str$( m mod 10) );
         else
            print fn FmtInt( 3, str$ (m mod 10) );
         end if
    else
       print mid$( str$( m ), 2 ) ;
    end if
    cnt++
end if
end fn


local fn xprint( m as long)
dim as long ii, wk, wk1

if ( m < 8 )
   ii = 1
      while ( ii <= lloc )
         fn yprint( stor(ii) )
         ii++
      wend
lloc = 0
else
   if ( m > 9 )
       wk = m / 10
       m = m mod 10
       wk1 = lloc
          while ( wk1 >= 1 )
             wk += stor(wk1)
             stor(wk1) = wk mod 10
             wk = wk/10
             wk1--
          wend
    end if
end if
lloc++
stor(lloc) = m
end fn


local mode
local fn shift( l1 as ^long, l2 as ^long, lp as long, lmod as long )
dim as long k

if ( l2.nil& > 0 )
   k = ( l2.nil& ) / lmod
else
   k =  -( -l2.nil&  / lmod ) - 1
end if
l2.nil& = l2.nil& - k*lmod
l1.nil& = l1.nil& + k*lp
end fn


local fn Main( nDig as long )
dim as long i

n = nDig
stor(0) = 0

mf = fn malloc( ( n + 10 ) * sizeof(long) )
if ( 0 == mf ) then stop "Out of memory"

ms = fn malloc( ( n + 10 ) * sizeof(long) )
if ( 0 == ms ) then stop "Out of memory"

print : print "Approximation of π to"; n; " digits"

cnt = 0
kf  = 25
ks  = 57121
mf(1) = 1

i = 2
while ( i <= n )
   mf(i)     = -16
   mf(i + 1) =  16
   i += 2
wend

i = 1
while ( i <= n )
   ms(i)     = -4
   ms(i + 1) =  4
   i += 2
wend

print : print " 3.";

while ( cnt < n )
   i = 0
   i++
   while ( i <= n - cnt )
      mf(i) = mf(i) * 10
      ms(i) = ms(i) * 10
      i++
    wend

   i = ( n - cnt + 1 )
   i--
   while ( i >= 2 )
      temp = 2 * i - 1
      fn shift( @mf(i - 1), @mf(i), temp - 2, temp * kf )
      fn shift( @ms(i - 1), @ms(i), temp - 2, temp * ks )
      i--
    wend

nd = 0

fn shift( @nd, @mf(1), 1, 5 )
fn shift( @nd, @ms(1), 1, 239 )
fn xprint( nd )

wend

print : print "Done"
fn free( ms )
fn free( mf )
end fn


dim as unsigned long   ticks
ticks = fn TickCount()
// Here we specify the number of decimal places
fn Main( 4000 )
ticks = fn TickCount() - ticks
print "Elapsed time:" str$( ticks ) " ticks

```


Output:

```txt


Approximation of π to 4000 digits

 3.1415926535  8979323846  2643383279  5028841971  6939937510  5820974944
   5923078164  0628620899  8628034825  3421170679  8214808651  3282306647
   0938446095  5058223172  5359408128  4811174502  8410270193  8521105559
   6446229489  5493038196  4428810975  6659334461  2847564823  3786783165
   2712019091  4564856692  3460348610  4543266482  1339360726  0249141273
   7245870066  0631558817  4881520920  9628292540  9171536436  7892590360
   0113305305  4882046652  1384146951  9415116094  3305727036  5759591953
   0921861173  8193261179  3105118548  0744623799  6274956735  1885752724
   8912279381  8301194912  9833673362  4406566430  8602139494  6395224737
   1907021798  6094370277  0539217176  2931767523  8467481846  7669405132
   0005681271  4526356082  7785771342  7577896091  7363717872  1468440901
   2249534301  4654958537  1050792279  6892589235  4201995611  2129021960
   8640344181  5981362977  4771309960  5187072113  4999999837  2978049951
   0597317328  1609631859  5024459455  3469083026  4252230825  3344685035
   2619311881  7101000313  7838752886  5875332083  8142061717  7669147303
   5982534904  2875546873  1159562863  8823537875  9375195778  1857780532
   1712268066  1300192787  6611195909  2164201989  3809525720  1065485863
   2788659361  5338182796  8230301952  0353018529  6899577362  2599413891
   2497217752  8347913151  5574857242  4541506959  5082953311  6861727855
   8890750983  8175463746  4939319255  0604009277  0167113900  9848824012
   8583616035  6370766010  4710181942  9555961989  4676783744  9448255379
   7747268471  0404753464  6208046684  2590694912  9331367702  8989152104
   7521620569  6602405803  8150193511  2533824300  3558764024  7496473263
   9141992726  0426992279  6782354781  6360093417  2164121992  4586315030
   2861829745  5570674983  8505494588  5869269956  9092721079  7509302955
   3211653449  8720275596  0236480665  4991198818  3479775356  6369807426
   5425278625  5181841757  4672890977  7727938000  8164706001  6145249192
   1732172147  7235014144  1973568548  1613611573  5255213347  5741849468
   4385233239  0739414333  4547762416  8625189835  6948556209  9219222184
   2725502542  5688767179  0494601653  4668049886  2723279178  6085784383
   8279679766  8145410095  3883786360  9506800642  2512520511  7392984896
   0841284886  2694560424  1965285022  2106611863  0674427862  2039194945
   0471237137  8696095636  4371917287  4677646575  7396241389  0865832645
   9958133904  7802759009  9465764078  9512694683  9835259570  9825822620
   5224894077  2671947826  8482601476  9909026401  3639443745  5305068203
   4962524517  4939965143  1429809190  6592509372  2169646151  5709858387
   4105978859  5977297549  8930161753  9284681382  6868386894  2774155991
   8559252459  5395943104  9972524680  8459872736  4469584865  3836736222
   6260991246  0805124388  4390451244  1365497627  8079771569  1435997700
   1296160894  4169486855  5848406353  4220722258  2848864815  8456028506
   0168427394  5226746767  8895252138  5225499546  6672782398  6456596116
   3548862305  7745649803  5593634568  1743241125  1507606947  9451096596
   0940252288  7971089314  5669136867  2287489405  6010150330  8617928680
   9208747609  1782493858  9009714909  6759852613  6554978189  3129784821
   6829989487  2265880485  7564014270  4775551323  7964145152  3746234364
   5428584447  9526586782  1051141354  7357395231  1342716610  2135969536
   2314429524  8493718711  0145765403  5902799344  0374200731  0578539062
   1983874478  0847848968  3321445713  8687519435  0643021845  3191048481
   0053706146  8067491927  8191197939  9520614196  6342875444  0643745123
   7181921799  9839101591  9561814675  1426912397  4894090718  6494231961
   5679452080  9514655022  5231603881  9301420937  6213785595  6638937787
   0830390697  9207734672  2182562599  6615014215  0306803844  7734549202
   6054146659  2520149744  2850732518  6660021324  3408819071  0486331734
   6496514539  0579626856  1005508106  6587969981  6357473638  4052571459
   1028970641  4011097120  6280439039  7595156771  5770042033  7869936007
   2305587631  7635942187  3125147120  5329281918  2618612586  7321579198
   4148488291  6447060957  5270695722  0917567116  7229109816  9091528017
   3506712748  5832228718  3520935396  5725121083  5791513698  8209144421
   0067510334  6711031412  6711136990  8658516398  3150197016  5151168517
   1437657618  3515565088  4909989859  9823873455  2833163550  7647918535
   8932261854  8963213293  3089857064  2046752590  7091548141  6549859461
   6371802709  8199430992  4488957571  2828905923  2332609729  9712084433
   5732654893  8239119325  9746366730  5836041428  1388303203  8249037589
   8524374417  0291327656  1809377344  4030707469  2112019130  2033038019
   7621101100  4492932151  6084244485  9637669838  9522868478  3123552658
   2131449576  8572624334  4189303968  6426243410  7732269780  2807318915
   4411010446  8232527162  0105265227  2111660396
Done
Elapsed time: 70 ticks


```




## Go

Code below is a simplistic translation of Haskell code in [http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/spigot.pdf Unbounded Spigot Algorithms for the Digits of Pi].  This is the algorithm specified for the [http://shootout.alioth.debian.org/u64q/performance.php?test=pidigits pidigits] benchmark of the [http://shootout.alioth.debian.org/ Computer Language Benchmarks Game].
(The standard Go distribution includes [http://golang.org/test/bench/shootout/pidigits.go source] submitted to the benchmark site, and that code runs stunning faster than the code below.)

```go
package main

import (
    "fmt"
    "math/big"
)

type lft struct {
    q,r,s,t big.Int
}

func (t *lft) extr(x *big.Int) *big.Rat {
    var n, d big.Int
    var r big.Rat
    return r.SetFrac(
        n.Add(n.Mul(&t.q, x), &t.r),
        d.Add(d.Mul(&t.s, x), &t.t))
}

var three = big.NewInt(3)
var four = big.NewInt(4)

func (t *lft) next() *big.Int {
    r := t.extr(three)
    var f big.Int
    return f.Div(r.Num(), r.Denom())
}

func (t *lft) safe(n *big.Int) bool {
    r := t.extr(four)
    var f big.Int
    if n.Cmp(f.Div(r.Num(), r.Denom())) == 0 {
        return true
    }
    return false
}

func (t *lft) comp(u *lft) *lft {
    var r lft
    var a, b big.Int
    r.q.Add(a.Mul(&t.q, &u.q), b.Mul(&t.r, &u.s))
    r.r.Add(a.Mul(&t.q, &u.r), b.Mul(&t.r, &u.t))
    r.s.Add(a.Mul(&t.s, &u.q), b.Mul(&t.t, &u.s))
    r.t.Add(a.Mul(&t.s, &u.r), b.Mul(&t.t, &u.t))
    return &r
}

func (t *lft) prod(n *big.Int) *lft {
    var r lft
    r.q.SetInt64(10)
    r.r.Mul(r.r.SetInt64(-10), n)
    r.t.SetInt64(1)
    return r.comp(t)
}

func main() {
    // init z to unit
    z := new(lft)
    z.q.SetInt64(1)
    z.t.SetInt64(1)

    // lfts generator
    var k int64
    lfts := func() *lft {
        k++
        r := new(lft)
        r.q.SetInt64(k)
        r.r.SetInt64(4*k+2)
        r.t.SetInt64(2*k+1)
        return r
    }

    // stream
    for {
        y := z.next()
        if z.safe(y) {
            fmt.Print(y)
            z = z.prod(y)
        } else {
            z = z.comp(lfts())
        }
    }
}
```



## Groovy

Solution:

```groovy
BigInteger q = 1, r = 0, t = 1, k = 1, n = 3, l = 3
String nn
boolean first = true

while (true) {
    (nn, first, q, r, t, k, n, l) = (4*q + r - t < n*t) \
        ? ["${n}${first?'.':''}", false, 10*q, 10*(r - n*t), t  , k    , 10*(3*q + r)/t - 10*n    , l    ] \
        : [''                   , first, q*k , (2*q + r)*l , t*l, k + 1, (q*(7*k + 2) + r*l)/(t*l), l + 2]
    print nn
}
```


Output (thru first 1000 iterations):

```txt
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337
```



## Haskell

The code from [http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf]:

```haskell
pi_ = g (1, 0, 1, 1, 3, 3)
  where
    g (q, r, t, k, n, l) =
      if 4 * q + r - t < n * t
        then n :
             g
               ( 10 * q
               , 10 * (r - n * t)
               , t
               , k
               , div (10 * (3 * q + r)) t - 10 * n
               , l)
        else g
               ( q * k
               , (2 * q + r) * l
               , t * l
               , k + 1
               , div (q * (7 * k + 2) + r * l) (t * l)
               , l + 2)
```


===Complete command-line program===

```haskell
#!/usr/bin/runhaskell

import Control.Monad
import System.IO

pi_ = g(1,0,1,1,3,3) where
  g (q,r,t,k,n,l) =
   if 4*q+r-t < n*t
    then n : g (10*q, 10*(r-n*t), t, k, div (10*(3*q+r)) t - 10*n, l)
    else g (q*k, (2*q+r)*l, t*l, k+1, div (q*(7*k+2)+r*l) (t*l), l+2)

digs = insertPoint digs'
  where insertPoint (x:xs) = x:'.':xs
        digs' = map (head . show) pi_

main = do
  hSetBuffering stdout $ BlockBuffering $ Just 80
  forM_ digs putChar
```


```txt

3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198

```


=={{header|Icon}} and {{header|Unicon}}==
{{Trans|PicoLisp}} based on Jeremy Gibbons' Haskell solution.

```icon
procedure pi (q, r, t, k, n, l)
  first := "yes"
  repeat { # infinite loop
    if (4*q+r-t < n*t) then {
      suspend n
      if (\first) := &null then suspend "."
      # compute and update variables for next cycle
      nr := 10*(r-n*t)
      n := ((10*(3*q+r)) / t) - 10*n
      q *:= 10
      r := nr
    } else {
      # compute and update variables for next cycle
      nr := (2*q+r)*l
      nn := (q*(7*k+2)+r*l) / (t*l)
      q *:= k
      t *:= l
      l +:= 2
      k +:= 1
      n := nn
      r := nr
    }
  }
end

procedure main ()
  every (writes (pi (1,0,1,1,3,3)))
end
```



## J


```j
pi=:3 :0
  smoutput"0'3.1'
  n=.0 while.n=.n+1 do.
    smoutput-/1 10*<.@o.10x^1 0+n
  end.
)
```

Example use:

```j
   pi''
3
.
1
4
1
5
9
2
6
5
3
...
```



## Java

```java
import java.math.BigInteger ;

public class Pi {
  final BigInteger TWO = BigInteger.valueOf(2) ;
  final BigInteger THREE = BigInteger.valueOf(3) ;
  final BigInteger FOUR = BigInteger.valueOf(4) ;
  final BigInteger SEVEN = BigInteger.valueOf(7) ;

  BigInteger q = BigInteger.ONE ;
  BigInteger r = BigInteger.ZERO ;
  BigInteger t = BigInteger.ONE ;
  BigInteger k = BigInteger.ONE ;
  BigInteger n = BigInteger.valueOf(3) ;
  BigInteger l = BigInteger.valueOf(3) ;

  public void calcPiDigits(){
    BigInteger nn, nr ;
    boolean first = true ;
    while(true){
        if(FOUR.multiply(q).add(r).subtract(t).compareTo(n.multiply(t)) == -1){
          System.out.print(n) ;
          if(first){System.out.print(".") ; first = false ;}
          nr = BigInteger.TEN.multiply(r.subtract(n.multiply(t))) ;
          n = BigInteger.TEN.multiply(THREE.multiply(q).add(r)).divide(t).subtract(BigInteger.TEN.multiply(n)) ;
          q = q.multiply(BigInteger.TEN) ;
          r = nr ;
          System.out.flush() ;
        }else{
          nr = TWO.multiply(q).add(r).multiply(l) ;
          nn = q.multiply((SEVEN.multiply(k))).add(TWO).add(r.multiply(l)).divide(t.multiply(l)) ;
          q = q.multiply(k) ;
          t = t.multiply(l) ;
          l = l.add(TWO) ;
          k = k.add(BigInteger.ONE) ;
          n = nn ;
          r = nr ;
        }
    }
  }

  public static void main(String[] args) {
    Pi p = new Pi() ;
    p.calcPiDigits() ;
  }
}
```


Output :


```txt
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480 ...
```



## JavaScript



###  Spigot Algorithm using BigInteger


This calculates one digit of pi at a time and writes it out
Javascript does not have a native integer object, so this solution uses a library for Big Integer operations.
document.write will work in a browser; to make this work in nodejs, change it to process.stdout.write

<lang>var BigInteger = require('jsbn').BigInteger;
var bi = function(n, b) { return new BigInteger(n.toString(), b ? b : 10); };
function calcPi() {
    var q=bi(1), r=bi(0), t=bi(1), k=bi(1), n=bi(3), l=bi(3);
    var one=bi(1), two=bi(2), three=bi(3), four=bi(4), seven=bi(7), ten=bi(10);
    while (true) {
        if (q.multiply(four).add(r).subtract(t).compareTo(n.multiply(t)) < 0) {
            process.stdout.write(n.toString());
            nr = (r.subtract(n.multiply(t))).multiply(ten);
            n  = (q.multiply(three).add(r)).multiply(ten).divide(t).subtract(n.multiply(ten));
            q  = q.multiply(ten);
            r  = nr;
        } else {
            nr = q.shiftLeft(1).add(r).multiply(l);
            nn = q.multiply(k).multiply(seven).add(two).add(r.multiply(l)).divide(t.multiply(l));
            q = q.multiply(k);
            t = t.multiply(l);
            l = l.add(two);
            k = k.add(one);
            n  = nn;
            r  = nr;
        }
    }
}
calcPi();

```



###  Web Page version


This shows how to load the previous code into a webpage that writes digits out without freezing the browser


```html

<html><head><script src='https://rawgit.com/andyperlitch/jsbn/v1.1.0/index.js'></script></head>
<body style="width: 100%"><tt id="pi"></tt><tt>...</tt>
<script async defer>
function bi(n, b) { return new jsbn.BigInteger(n.toString(), b ? b : 10); };
var one=bi(1), two=bi(2), three=bi(3), four=bi(4), seven=bi(7), ten=bi(10);
function calcPi() {
    var q=bi(1), r=bi(0), t=bi(1), k=bi(1), n=bi(3), l=bi(3);
    var digit=0, firstrun=1;
    var p=document.getElementById('pi');
    function w(s) { p.appendChild(document.createTextNode(s));}
    function continueCalcPi(q, r, t, k, n, l) {
        while (true) {
            if (q.multiply(four).add(r).subtract(t).compareTo(n.multiply(t)) < 0) {
                w(n.toString());
                if (digit==0 && firstrun==1) { w('.'); firstrun=0; };
                digit = (digit+1) % 256;
                var nr = (r.subtract(n.multiply(t))).multiply(ten);
                n  = (q.multiply(three).add(r)).multiply(ten).divide(t).subtract(n.multiply(ten));
                q  = q.multiply(ten);
                r  = nr;
                if (digit%8==0) {
                    if (digit%64==0) {
                     p.appendChild(document.createElement('br'));
                    }
                    w(' ');
                    return setTimeout(function() { continueCalcPi(q, r, t, k, n, l); }, 50);
                };
            } else {
                var nr = q.shiftLeft(1).add(r).multiply(l);
                var nn = q.multiply(k).multiply(seven).add(two).add(r.multiply(l)).divide(t.multiply(l));
                q = q.multiply(k);
                t = t.multiply(l);
                l = l.add(two);
                k = k.add(one);
                n  = nn;
                r  = nr;
            }
        }
    }
    continueCalcPi(q, r, t, k, n, l);
}
calcPi();
</script>
</body></html>

```


###  Simple Approximation

Returns an approximation of Pi.

<lang>var calcPi = function() {
  var n = 20000;
  var pi = 0;
  for (var i = 0; i < n; i++) {
    var temp = 4 / (i*2+1);
    if (i % 2 == 0) {
      pi += temp;
    }
    else {
      pi -= temp;
    }
  }
  return pi;
}
```



## jq

The focus in this section is on the Gibbons spigot algorithm as it
is relatively simple and therefore provides a gentle introduction to
how such algorithms can be implemented in jq.

Since the Gibbons algorithm quickly fails in the absence of support for large integers, we shall assume BigInt support, such as provided by [https://gist.github.com/pkoppstein/d06a123f30c033195841 BigInt.jq].

The jq program presented here closely follows the Groovy and Python
examples on this page.  The spigot generator is named "next", and is
driven by an annotation function, "decorate"; thus the main program
is just "S0 | decorate(next)" where S0 is the initial state.  One
advantage of this approach is that the generator's state is exposed,
thus making it easy to restart the stream at any point.

The annotation defined here results in a triple for each digit of pi:
[index, digit, space], where "space" is the the sum of the lengths of
the strings in the six-dimensional state vector, [q, r, t, k, n, l].
The output shows that the space requirements of the Gibbons
spigot grow very slightly more than linearly.


```jq
# The Gibbons spigot, in the mold of the [[#Groovy]] and ython]] programs shown on this page.
# The "bigint" functions
needed are: long_minus long_add long_multiply long_div

def pi_spigot:

  # S is the sixtuple:
  # q      r      t      k      n      l
  # 0      1      2      3      4      5

  def long_lt(x;y): if x == y then false else lessOrEqual(x;y) end;

  def check:
     long_lt(long_minus(long_add(long_multiply("4"; .[0]); .[1]) ; .[2]);
             long_multiply(.[4]; .[2]));

  # state: [d, S] where digit is null or a digit ready to be printed
  def next:
    .[1] as $S
    | $S[0] as $q | $S[1] as $r | $S[2] as $t | $S[3] as $k | $S[4] as $n | $S[5] as $l
    | if $S|check
      then [$n,
             [long_multiply("10"; $q),
              long_multiply("10"; long_minus($r; long_multiply($n;$t))),
              $t,
              $k,
              long_minus( long_div(long_multiply("10";long_add(long_multiply("3"; $q); $r)); $t );
                          long_multiply("10";$n)),
              $l ]]
      else [null,
             [long_multiply($q;$k),
              long_multiply( long_add(long_multiply("2";$q); $r); $l),
              long_multiply($t;$l),
              long_add($k; "1"),
              long_div( long_add(long_multiply($q; long_add(long_multiply("7";$k); "2")) ; long_multiply($r;$l));
                        long_multiply($t;$l) ),
              long_add($l; "2") ]]
      end;

  # Input: input to the filter "nextstate"
  # Output:  [count, space, digit] for successive digits produced by "nextstate"
  def decorate( nextstate ):

     # For efficiency it is important that the recursive
     # function have arity 0 and be tail-recursive:
     def count:
       .[0] as $count
       | .[1] as $state
       | $state[0] as $value
       | ($state[1] | map(length) | add) as $space
       | (if $value then [$count, $space, $value] else empty end),
         ( [if $value then $count+1 else $count end, ($state | nextstate)] | count);
  [0, .] | count;

  #       q=1, r=0, t=1, k=1, n=3, l=3
  [null, ["1", "0", "1", "1", "3", "3"]] | decorate(next)
;

pi_spigot
```

<div style="overflow:scroll; height:200px;">

```sh
$ jq -M -n -c -f pi.bigint.jq
[0,9,"3"]
[1,14,"1"]
[2,29,"4"]
[3,36,"1"]
[4,51,"5"]
[5,69,"9"]
[6,80,"2"]
[7,95,"6"]
[8,115,"5"]
[9,125,"3"]
[10,142,"5"]
[11,167,"8"]
[12,181,"9"]
[13,197,"7"]
[14,226,"9"]
[15,245,"3"]
[16,263,"2"]
[17,276,"3"]
[18,300,"8"]
[19,320,"4"]
[20,350,"6"]
[21,363,"2"]
[22,383,"6"]
[23,408,"4"]
[24,429,"3"]
[25,442,"3"]
[26,475,"8"]
[27,502,"3"]
[28,510,"2"]
[29,531,"7"]
[30,563,"9"]
[31,611,"5"]
[32,613,"0"]
[33,628,"2"]
[34,649,"8"]
[35,676,"8"]
[36,711,"4"]
[37,720,"1"]
[38,748,"9"]
[39,783,"7"]
[40,792,"1"]
[41,814,"6"]
[42,849,"9"]
[43,870,"3"]
[44,886,"9"]
[45,923,"9"]
[46,939,"3"]
[47,967,"7"]
[48,1004,"5"]
[49,1041,"1"]
[50,1043,"0"]
[51,1059,"5"]
[52,1103,"8"]
[53,1133,"2"]
[54,1135,"0"]
[55,1165,"9"]
[56,1195,"7"]
[57,1212,"4"]
[58,1242,"9"]
[59,1273,"4"]
[60,1297,"4"]
[61,1313,"5"]
[62,1358,"9"]
[63,1375,"2"]
[64,1421,"3"]
[65,1423,"0"]
[66,1447,"7"]
[67,1493,"8"]
[68,1501,"1"]
[69,1533,"6"]
[70,1579,"4"]
[71,1581,"0"]
[72,1613,"6"]
[73,1630,"2"]
[74,1662,"8"]
[75,1701,"6"]
[76,1733,"2"]
[77,1735,"0"]
[78,1781,"8"]
[79,1792,"9"]
[80,1816,"9"]
[81,1849,"8"]
[82,1889,"6"]
[83,1898,"2"]
[84,1961,"8"]
[85,1963,"0"]
[86,1988,"3"]
[87,2013,"4"]
[88,2054,"8"]
[89,2071,"2"]
[90,2104,"5"]
[91,2129,"3"]
[92,2162,"4"]
[93,2195,"2"]
[94,2220,"1"]
[95,2230,"1"]
[96,2287,"7"]
[97,2289,"0"]
[98,2314,"6"]
[99,2340,"7"]
[100,2373,"9"]
[101,2414,"8"]
[102,2448,"2"]
[103,2458,"1"]
[104,2484,"4"]
[105,2534,"8"]
[106,2536,"0"]
[107,2569,"8"]
[108,2602,"6"]
[109,2645,"5"]
[110,2662,"1"]
[111,2696,"3"]
[112,2707,"2"]
[113,2756,"8"]
[114,2775,"2"]
[115,2825,"3"]
[116,2827,"0"]
[117,2853,"6"]
[118,2887,"6"]
[119,2914,"4"]
[120,2964,"7"]
[121,2966,"0"]
[122,3008,"9"]
[123,3027,"3"]
[124,3061,"8"]
[125,3088,"4"]
[126,3114,"4"]
[127,3165,"6"]
[128,3167,"0"]
[129,3202,"9"]
[130,3237,"5"]
[131,3287,"5"]
[132,3289,"0"]
[133,3316,"5"]
[134,3360,"8"]
[135,3387,"2"]
[136,3414,"2"]
[137,3456,"3"]
[138,3466,"1"]
[139,3510,"7"]
[140,3529,"2"]
[141,3564,"5"]
[142,3583,"3"]
[143,3610,"5"]
[144,3653,"9"]
[145,3697,"4"]
[146,3699,"0"]
[147,3752,"8"]
[148,3770,"1"]
[149,3789,"2"]
[150,3825,"8"]
[151,3852,"4"]
[152,3905,"8"]
[153,3933,"1"]
[154,3960,"1"]
[155,3970,"1"]
[156,4006,"7"]
[157,4033,"4"]
[158,4102,"5"]
[159,4104,"0"]
[160,4124,"2"]
[161,4159,"8"]
[162,4203,"4"]
[163,4248,"1"]
[164,4250,"0"]
[165,4269,"2"]
[166,4348,"7"]
[167,4350,"0"]
[168,4361,"1"]
[169,4405,"9"]
[170,4424,"3"]
[171,4460,"8"]
[172,4497,"5"]
[173,4542,"2"]
[174,4569,"1"]
[175,4605,"1"]
[176,4607,"0"]
[177,4644,"5"]
[178,4672,"5"]
[179,4691,"5"]
[180,4727,"9"]
[181,4764,"6"]
[182,4792,"4"]
[183,4820,"4"]
[184,4865,"6"]
[185,4893,"2"]
[186,4913,"2"]
[187,4949,"9"]
[188,4968,"4"]
[189,5005,"8"]
[190,5042,"9"]
[191,5070,"5"]
[192,5098,"4"]
[193,5144,"9"]
[194,5198,"3"]
[195,5200,"0"]
[196,5219,"3"]
[197,5266,"8"]
[198,5276,"1"]
[199,5313,"9"]
[200,5350,"6"]
[201,5387,"4"]
[202,5416,"4"]
[203,5435,"2"]
[204,5471,"8"]
[205,5526,"8"]
[206,5556,"1"]
[207,5558,"0"]
[208,5594,"9"]
[209,5632,"7"]
[210,5660,"5"]
[211,5689,"6"]
[212,5726,"6"]
[213,5746,"5"]
[214,5792,"9"]
[215,5821,"3"]
[216,5849,"3"]
[217,5887,"4"]
[218,5906,"4"]
[219,5961,"6"]
[220,5981,"1"]
[221,6002,"2"]
[222,6038,"8"]
[223,6068,"4"]
[224,6096,"7"]
[225,6134,"5"]
[226,6163,"6"]
[227,6191,"4"]
[228,6238,"8"]
[229,6267,"2"]
[230,6296,"3"]
[231,6316,"3"]
[232,6344,"7"]
[233,6383,"8"]
[234,6411,"6"]
[235,6440,"7"]
[236,6487,"8"]
[237,6525,"3"]
[238,6545,"1"]
[239,6574,"6"]
[240,6621,"5"]
[241,6641,"2"]
[242,6688,"7"]
[243,6717,"1"]
[244,6782,"2"]
[245,6784,"0"]
[246,6795,"1"]
[247,6852,"9"]
[248,6854,"0"]
[249,6910,"9"]
[250,6929,"1"]
[251,6959,"4"]
[252,6988,"5"]
[253,7027,"6"]
[254,7046,"4"]
[255,7085,"8"]
[256,7115,"5"]
[257,7153,"6"]
[258,7181,"6"]
[259,7229,"9"]
[260,7258,"2"]
[261,7288,"3"]
[262,7317,"4"]
[263,7383,"6"]
[264,7385,"0"]
[265,7415,"3"]
[266,7435,"4"]
[267,7474,"8"]
[268,7530,"6"]
[269,7569,"1"]
[270,7571,"0"]
[271,7609,"4"]
[272,7639,"5"]
[273,7678,"4"]
[274,7716,"3"]
[275,7736,"2"]
[276,7766,"6"]
[277,7805,"6"]
[278,7826,"4"]
[279,7873,"8"]
[280,7912,"2"]
[281,7933,"1"]
[282,7971,"3"]
[283,7991,"3"]
[284,8030,"9"]
[285,8060,"3"]
[286,8118,"6"]
[287,8120,"0"]
[288,8168,"7"]
[289,8189,"2"]
[290,8264,"6"]
[291,8266,"0"]
[292,8287,"2"]
[293,8317,"4"]
[294,8374,"9"]
[295,8395,"1"]
[296,8443,"4"]
[297,8464,"1"]
[298,8485,"2"]
[299,8524,"7"]
[300,8544,"3"]
[301,8593,"7"]
[302,8623,"2"]
...

```
</div>


## Julia

Julia comes with built-in support for computing π in arbitrary precision (using the GNU MPFR library).  This implementation computes π at precisions that are repeatedly doubled as more digits are needed, printing one digit at a time and never terminating (until it runs out of memory) as specified:

```julia
let prec = precision(BigFloat), spi = "", digit = 1
    while true
      if digit > lastindex(spi)
        prec *= 2
        setprecision(prec)
        spi = string(big(π))
      end
      print(spi[digit])
      digit += 1
    end
end
```


Output:

```txt
3.141592653589793238462643383279502884195e69399375105820974944592307816406286198e9862803482534211706798214808651328230664709384460955058223172535940812848115e450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724586997e0631558817488152092096282925409171536436789259036001133053054882046652138414695194151160943305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912983367336244065664308602139494639522473719070217986094370277053921717629317675238467481846766940513200056812714526357e8277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201...
```



## Kotlin

```scala
// version 1.1.2

import java.math.BigInteger

val ZERO  = BigInteger.ZERO
val ONE   = BigInteger.ONE
val TWO   = BigInteger.valueOf(2L)
val THREE = BigInteger.valueOf(3L)
val FOUR  = BigInteger.valueOf(4L)
val SEVEN = BigInteger.valueOf(7L)
val TEN   = BigInteger.TEN

fun calcPi() {
    var nn: BigInteger
    var nr: BigInteger
    var q = ONE
    var r = ZERO
    var t = ONE
    var k = ONE
    var n = THREE
    var l = THREE
    var first = true
    while (true) {
        if (FOUR * q + r - t < n * t) {
            print(n)
            if (first) { print ("."); first = false }
            nr = TEN * (r - n * t)
            n = TEN * (THREE * q + r) / t - TEN * n
            q *= TEN
            r = nr
        }
        else {
            nr = (TWO * q + r) * l
            nn = (q * SEVEN * k + TWO + r * l) / (t * l)
            q *= k
            t *= l
            l += TWO
            k += ONE
            n = nn
            r = nr
        }
    }
}

fun main(args: Array<String>) = calcPi()
```


```txt

3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745...

```



## Lasso

Based off [http://crypto.stanford.edu/pbc/notes/pi/code.html Dik T. Winter's C implementation of Beeler et al. 1972, Item 120].

```Lasso
#!/usr/bin/lasso9

define generatePi => {
  yield currentCapture

  local(r = array(), i, k, b, d, c = 0, x)
  with i in generateSeries(1, 2800)
  do #r->insert(2000)
  with k in generateSeries(2800, 1, -14)
  do {
    #d = 0
    #i = #k
    while(true) => {
      #d += #r->get(#i) * 10000
      #b = 2 * #i - 1
      #r->get(#i) = #d % #b
      #d /= #b
      #i--
      !#i ? loop_abort
      #d *= #i
    }
    #x = (#c + #d / 10000)
    yield (#k == 2800 ? ((#x * 0.001)->asstring(-precision = 3)) | #x->asstring(-padding=4, -padChar='0'))
    #c = #d % 10000
  }
}

local(pi_digits) = generatePi
loop(200) => {
    stdout(#pi_digits())
}
```

Output (first 100 places):

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067

```



## Liberty BASIC

Pretty slow if you run for over 100 digits...

```lb
    ndigits = 0

    q       = 1
    r       = 0
    t       = q
    k       = q
    n       = 3
    L       = n

   first = 666  '   ANY non-zero =='true' in LB.

    while ndigits <100
        if ( 4 *q +r -t) <( n *t) then
            print n;
            ndigits =ndigits +1
            if not( ndigits mod 40) then print: print "  ";
            if first =666 then first = 0: print ".";
            nr =10 *( r -n *t)
            n  =int( ( (10 *( 3 *q +r)) /t) -10 *n)
            q  =q *10
            r  =nr
        else
            nr =( 2 *q +r) *L
            nn =(q *( 7 *k +2) +r *L) /( t *L)
            q  =q *k
            t  =t *L
            L  =L +2
            k  =k +1
            n  =int( nn)
            r  =nr
        end if
        scan
wend

end
```


```txt

3.141592653589793238462643383279502884197
1693993751058209749445923078164062862089
98628034825342117067

```



## Lua

```lua
a = {}
n = 1000
len = math.modf( 10 * n / 3 )

for j = 1, len do
    a[j] = 2
end
nines = 0
predigit = 0
for j = 1, n do
    q = 0
    for i = len, 1, -1 do
        x = 10 * a[i] + q * i
        a[i] = math.fmod( x, 2 * i - 1 )
        q = math.modf( x / ( 2 * i - 1 ) )
    end
    a[1] = math.fmod( q, 10 )
    q = math.modf( q / 10 )
    if q == 9 then
        nines = nines + 1
    else
        if q == 10 then
            io.write( predigit + 1 )
            for k = 1, nines do
                io.write(0)
            end
            predigit = 0
            nines = 0
        else
            io.write( predigit )
            predigit = q
            if nines ~= 0 then
                for k = 1, nines do
                    io.write( 9 )
                end
                nines = 0
            end
        end
    end
end
print( predigit )
```


```txt
03141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086 ...
```




## M2000 Interpreter

We can ask for 200 digits, but we can remove Digits-- in While Digits {} to print endless number of digits (without good precision). Algorithm developed after reading [http://www.pi314.net/eng/goutte.php] and [https://www.cut-the-knot.org/Curriculum/Algorithms/SpigotForPi.shtml]

A Faster version handling console refresh time (and os shared time). M2000 run on an environment, which is loop event, and console is actual a form, a window. We can stop execution using Esc, Ctrl+C and Break keys, without stopping the interpreter (which is an application for Windows Os, written in Visual Basic 6,a s an ActiveX dll with a window manager on top of Vb forms).




```M2000 Interpreter

Module Checkpi {
      Module FindPi(Digits){
            Digits++
            n=Int(3.32*Digits)
            PlusOne=Lambda N=0% -> {
                  =N
                  N++
            }
            PlusTwo=Lambda N=1% -> {
                  =N
                  N+=2
            }
            Dim A(n)<<PlusOne(), B(n)<<PlusTwo()
            Dim Ten(n),  CarrierOver(n), Sum(n),Remainder(n)=2
            OutPutDigits=Digits
            Predigits=Stack
            CallBack=lambda fl=true, Chars=0 (x)->{
                  Print x;
                  Chars++
                  If fl then Print "." : Print " "; : fl=false : Chars=0 : exit
                  If Chars=50 then {
                        Print
                        Print " ";
                        Chars=0
                        Refresh
                  } else.if (Chars mod 5)=0 then {
                         Print " ";
                         Refresh
                  }
                  \\ explicitly refresh output layer, using Fast ! mode of speed
            }
            Print "Pi=";
             While Digits {
                  NextDigit(&CallBack, &Digits)
            }
            print
            Refresh
            Sub NextDigit(&f, &D)
                CarrierOver=0
                  For k=n-1 to 1 {
                  Ten(k)=Remainder(k)*10%
                  CarrierOver(k)=CarrierOver
                  Sum(k)=Ten(k)+CarrierOver(k)
                  q=Sum(k) div  B(k)
                  Remainder(k)=Sum(k)-B(k)*q
                  CarrierOver=A(k)*q
                  }
                  Ten(0)=Remainder(0)*10%
                  CarrierOver(0)=CarrierOver
                  Sum(0)=Ten(0)+CarrierOver(0)
                  q=Sum(0) div  10%
                  Remainder(0)=Sum(0)-10%*q
                  if q<>9 and q<>10 then {
                        Stack Predigits {
                              While not empty {
                                Call f(Number)
                                if D>0 then D--
                                If D=0 then flush ' empty stack
                              }
                              Push q
                        }
                  } else.if q=9 Then {
                        Stack Predigits { Data q }
                  } else {
                        Stack Predigits {
                        While not empty {
                              Call f((Number+1) mod 10)
                              if D>0 then D--
                              If D=0 then flush ' empty stack
                        }
                        Push 0
                        }
                  }
            End Sub
      }
      \\ reduce time to share with OS
      \\ Need explicitly use of refresh output layer (M2000 console)
      \\ Slow for a screen refresh per statement and give more time to OS
      Rem Set Slow
      \\ Fast is normal screen refresh, per Refresh time, and give standard time to OS
      Rem Set Fast
      \\ Fast ! use Refresh for screen refresh, and give less time o OS than standard
      \\ Esc key work when Refresh executed (and OS get little time)
      Set Fast !
      FindPi 4
      FindPi 28
      Print Pi  ' pi in M2000 is Decimal type with 29 digits (1 plus 28 after dot, is same as FindPi 28)
      Refresh
      FindPi 50
}
Flush ' empty stack of values
CheckPi
List  ' no variables exist
Modules ? ' current module exist
Stack ' Stack of values  ' has to be empty, we didn't use current stack for values.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
User can interrupt computation using "Alt+." or "Cmd+." on a Mac.

```Mathematica
WriteString[$Output, "3."];
For[i = -1, True, i--,
  WriteString[$Output, RealDigits[Pi, 10, 1, i][[1, 1]]]; Pause[.05]];
```


=={{header|MATLAB}} / {{header|Octave}}==
Matlab and Octave use double precision numbers per default, and pi is a builtin constant value. Arbitrary precision is only implemented in some additional toolboxes (e.g. symbolic toolbox).

```MATLAB
pi>
```


```txt

>> pi
ans =  3.1416
> printf('%.60f\n',pi)
3.141592653589793115997963468544185161590576171875000000000000>> format long

```


```txt

Unfortunately this is not the correct value!
3.14159265358979323846264338327950288419716939937510582

### ===========
??????????????????????????????????????
```


Calling for 60 digit output does not produce 60 digits of precision. Once the sixteen digit precision of double precision is reached, the subsequent digits are determined by the workings of the binary to decimal conversion. The long decimal string is the exact decimal value of the binary representation of pi, which binary value is itself not exact because pi cannot be represented in a finite number of digits, be they decimal, binary or any other integer base...


## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary
import java.math.BigInteger

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg places .
  if places = '' then places = -1

  TWO   = BigInteger.valueOf(2)
  THREE = BigInteger.valueOf(3)
  FOUR  = BigInteger.valueOf(4)
  SEVEN = BigInteger.valueOf(7)

  q_ = BigInteger.ONE
  r_ = BigInteger.ZERO
  t_ = BigInteger.ONE
  k_ = BigInteger.ONE
  n_ = BigInteger.valueOf(3)
  l_ = BigInteger.valueOf(3)

  nn = BigInteger
  nr = BigInteger

  first = isTrue()
  digitCt = 0
  loop forever
    if FOUR.multiply(q_).add(r_).subtract(t_).compareTo(n_.multiply(t_)) == -1 then do
      digitCt = digitCt + 1
      if places > 0 & digitCt - 1 > places then leave
      say n_'\-'
      if first then do
        say '.\-'
        first = isFalse()
        end
      nr = BigInteger.TEN.multiply(r_.subtract(n_.multiply(t_)))
      n_ = BigInteger.TEN.multiply(THREE.multiply(q_).add(r_)).divide(t_).subtract(BigInteger.TEN.multiply(n_))
      q_ = q_.multiply(BigInteger.TEN)
      r_ = nr
      end
    else do
      nr = TWO.multiply(q_).add(r_).multiply(l_)
      nn = q_.multiply((SEVEN.multiply(k_))).add(TWO).add(r_.multiply(l_)).divide(t_.multiply(l_))
      q_ = q_.multiply(k_)
      t_ = t_.multiply(l_)
      l_ = l_.add(TWO)
      k_ = k_.add(BigInteger.ONE)
      n_ = nn
      r_ = nr
      end
    end
  say

  return

method isTrue() private static returns boolean
  return (1 == 1)
method isFalse() private static returns boolean
  return \isTrue()

```

```txt

3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679...

```



## Nim

```nim
import strutils, bigints

var
  tmp1, tmp2, tmp3, acc, k, dd = initBigInt(0)
  den, num, k2 = initBigInt(1)

proc extractDigit(): int32 =
  if num > acc:
    return -1

  tmp3 = num shl 1
  tmp3 += num
  tmp3 += acc
  tmp2 = tmp3 mod den
  tmp1 = tmp3 div den
  tmp2 += num

  if tmp2 >= den:
    return -1

  result = int32(tmp1.limbs[0])

proc eliminateDigit(d: int32) =
  acc -= den * d
  acc *= 10
  num *= 10

proc nextTerm() =
  k += 1
  k2 += 2
  tmp1 = num shl 1
  acc += tmp1
  acc *= k2
  den *= k2
  num *= k

var i = 0

while true:
  var d: int32 = -1
  while d < 0:
    nextTerm()
    d = extractDigit()

  stdout.write chr(ord('0') + d)
  inc i
  if i == 40:
    echo ""
    i = 0
  eliminateDigit d
```

Output:

```txt
3141592653589793238462643383279502884197
1693993751058209749445923078164062862089
9862803482534211706798214808651328230664
7093844609550582231725359408128481117450
...
```



## OCaml

The Constructive Real library [http://www.lri.fr/~filliatr/creal.en.html Creal] contains an infinite-precision Pi, so we can just print out its digits.

```OCaml
open Creal;;

let block = 100 in
let segment n =
   let s = to_string pi (n*block) in
   String.sub s ((n-1)*block) block in
let counter = ref 1 in
while true do
   print_string (segment !counter);
   flush stdout;
   incr counter
done
```

However that is cheating if you want to see an algorithm to generate Pi. Since the Spigot algorithm is already used in the [http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=ocaml&id=1 pidigits] program, this implements [http://mathworld.wolfram.com/Machin-LikeFormulas.html Machin's formula].

```OCaml
open Num

(* series for: c*atan(1/k) *)
class atan_sum c k = object
   val kk = k*/k
   val mutable n = 0
   val mutable kpow = k
   val mutable pterm = c*/k
   val mutable psum = Int 0
   val mutable sum = c*/k
   method next =
      n <- n+1; kpow <- kpow*/kk;
      let t = c*/kpow//(Int (2*n+1)) in
      pterm <- if n mod 2 = 0 then t else minus_num t;
      psum <- sum;
      sum <- sum +/ pterm
   method error = abs_num pterm
   method bounds = if pterm </ Int 0 then (sum, psum) else (psum, sum)
end;;

let inv i = (Int 1)//(Int i) in
let t1 = new atan_sum (Int 16) (inv 5) in
let t2 = new atan_sum (Int (-4)) (inv 239) in
let base = Int 10 in
let npr = ref 0 in
let shift = ref (Int 1) in
let d_acc = inv 10000 in
let acc = ref d_acc in
let shown = ref (Int 0) in
while true do
   while t1#error >/ !acc do t1#next done;
   while t2#error >/ !acc do t2#next done;
   let (lo1, hi1), (lo2, hi2) = t1#bounds, t2#bounds in
   let digit x = int_of_num (floor_num ((x -/ !shown) */ !shift)) in
   let d, d' = digit (lo1+/lo2), digit (hi1+/hi2) in
   if d = d' then (
      print_int d;
      if !npr = 0 then print_char '.';
      flush stdout;
      shown := !shown +/ ((Int d) // !shift);
      incr npr; shift := !shift */ base;
   ) else (acc := !acc */ d_acc);
done
```



## Oforth



```Oforth
: calcPiDigits
| q r t k n l |
   1 ->q 0 ->r 1 ->t 1 ->k 3 ->n 3 -> l

   while( true ) [
      4 q * r + t - n t * < ifTrue: [
         n print
         r n t * - 10 *
         3 q * r + 10 * t / n 10 * - ->n ->r
         q 10 * ->q
         ]
      else: [
         2 q * r + l *
         7 k * q * 2 + r l * + t l * / ->n ->r
         k q * ->q
         t l * ->t
         l 2 + ->l
         k 1+  ->k
         ]
       ] ;
```



## Ol

```scheme

; 'numbers' is count of numbers or #false for eternal pleasure.
(define (pi numbers)
   (let loop ((q 1) (r 0) (t 1) (k 1) (n 3) (l 3) (numbers numbers))
      (unless (eq? numbers 0)
         (if (< (- (+ (* 4 q) r) t) (* n t))
            (begin
               (display n)
               (loop (* q  10)
                     (* 10 (- r (* n t)))
                     t
                     k
                     (- (div (* 10 (+ (* 3 q) r)) t) (* 10 n))
                     l
                     (if numbers (- numbers 1))))
            (begin
               (loop (* q k)
                     (* (+ (* 2 q) r) l)
                     (* t l)
                     (+ k 1)
                     (div (+ (* q (* 7 k)) 2 (* r l)) (* t l))
                     (+ l 2)
                     (if numbers (- numbers 1))))))))

(pi #false)

```

```txt

31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132
82306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475
64823378678316527120190914564856692346034861045432664821339360726024914127372458700660631558817488152092096282925
40917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480
74462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539
21717629317675238467481846766940513200056812714526356082778577134275778960917363717872146844090122495343014654958
53710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328
16096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598
25349042875546873115956286388235378759375195778185778053217122680661300192787661119590921642019893809525720106548
58632788659361533818279682303019520353018529689957736225994138912497217752834791315155748572424541506959508295331
1686172785588907509838175463

```



## PARI/GP

Uses the built-in  Brent-Salamin arithmetic-geometric mean iteration.

```parigp
pi()={
  my(x=Pi,n=0,t);
  print1("3.");
  while(1,
    if(n>=default(realprecision),
      default(realprecision,default(realprecision)*2);
      x=Pi
    );
    print1(floor(x*10^n++)%10)
  )
};
```



## Pascal

With minor editing changes as published by Stanley Rabinowitz in [http://www.mathpropress.com/stan/bibliography/spigot.pdf].
Minor improvement of <user>Mischi</user> { speedup ~2 ( n=10000 , rumtime 4s-> 1,44s fpc 2.6.4 -O3 }, by calculating only necessary digits up to n.

```pascal
Program Pi_Spigot;
const
  n   = 1000;
  len = 10*n div 3;

var
  j, k, q, nines, predigit: integer;
  a: array[0..len] of longint;

function OneLoop(i:integer):integer;
var
  x: integer;
begin
  {Only calculate as far as needed }
  {+16 for security digits ~5 decimals}
  i := i*10 div 3+16;
  IF i > len then
    i := len;
  result := 0;
  repeat   {Work backwards}
    x  := 10*a[i] + result*i;
    result := x div (2*i - 1);
    a[i]   := x - result*(2*i - 1);//x mod (2*i - 1)
    dec(i);
  until i<= 0 ;
end;

begin

  for j := 1 to len do
    a[j] := 2;                 {Start with 2s}
  nines := 0;
  predigit := 0;               {First predigit is a 0}

  for j := 1 to n do
  begin
    q := OneLoop(n-j);
    a[1] := q mod 10;
    q := q div 10;
    if q = 9 then
      nines := nines + 1
    else
      if q = 10 then
      begin
        write(predigit+1);
        for k := 1 to nines do
          write(0);            {zeros}
        predigit := 0;
        nines := 0
      end
      else
      begin
        write(predigit);
        predigit := q;
        if nines <> 0 then
        begin
          for k := 1 to nines do
            write(9);
          nines := 0
        end
      end
  end;
  writeln(predigit);
end.
```

Output:

```txt
% ./Pi_Spigot
03141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198
```



## Perl

Perl being what it is, there are many ways to do this with many variations.  With a fixed number of digits and the Math::BigInt::GMP library installed, the [[[[Arithmetic-geometric mean/Calculate Pi]] code will be much faster than any of these methods other than some of the modules.  If Math::GMP is installed, then replacing "use bigint" with "use Math::GMP qw/:constant/" in either the Perl6 spigot or Machin methods below will be pretty fast.  They are not too bad if the Math::BigInt::GMP library is installed.  With the default Math::BigInt backend, the AGM code isn't very fast and the Perl6 spigot and Machin methods are <b>very</b> slow.


### = Simple Spigot =

This takes a numer-of-digits argument, but we can make it large (albeit using memory and some startup time).  Unlike the other two, this uses no modules and does not require bigints so is worth showing.


```perl
sub pistream {
  my $digits = shift;
  my(@out, @a);
  my($b, $c, $d, $e, $f, $g, $i, $d4, $d3, $d2, $d1);
  my $outi = 0;

  $digits++;
  $b = $d = $e = $g = $i = 0;
  $f = 10000;
  $c = 14 * (int($digits/4)+2);
  @a = (20000000) x $c;
  print "3.";
  while (($b = $c -= 14) > 0 && $i < $digits) {
    $d = $e = $d % $f;
    while (--$b > 0) {
      $d = $d * $b + $a[$b];
      $g = ($b << 1) - 1;
      $a[$b] = ($d % $g) * $f;
      $d = int($d / $g);
    }
    $d4 = $e + int($d/$f);
    if ($d4 > 9999) {
      $d4 -= 10000;
      $out[$i-1]++;
      for ($b = $i-1; $out[$b] == 1; $b--) {
        $out[$b] = 0;
        $out[$b-1]++;
      }
    }
    $d3 = int($d4/10);
    $d2 = int($d3/10);
    $d1 = int($d2/10);
    $out[$i++] = $d1;
    $out[$i++] = $d2-$d1*10;
    $out[$i++] = $d3-$d2*10;
    $out[$i++] = $d4-$d3*10;
    print join "", @out[$i-15 .. $i-15+3]  if $i >= 16;
  }
  # We've closed the spigot.  Print the remainder without rounding.
  print join "", @out[$i-15+4 .. $digits-2], "\n";
}
```



### = Perl6 spigot =

As mentioned earlier, replacing "use bigint" with "use Math::GMP qw/:constant/" will result in many orders of magnitude faster performance.

```perl
use bigint try=>
"GMP";
sub stream {
    my ($next, $safe, $prod, $cons, $z, $x) = @_;
    $x = $x->();
    sub {
        while (1) {
            my $y = $next->($z);
            if ($safe->($z, $y)) {
                $z = $prod->($z, $y);
                return $y;
            } else {
                $z = $cons->($z, $x->());
            }
        }
    }
}

sub extr {
    use integer;
    my ($q, $r, $s, $t) = @{shift()};
    my $x = shift;
    ($q * $x + $r) / ($s * $x + $t);
}

sub comp {
    my ($q, $r, $s, $t) = @{shift()};
    my ($u, $v, $w, $x) = @{shift()};
    [$q * $u + $r * $w,
     $q * $v + $r * $x,
     $s * $u + $t * $w,
     $s * $v + $t * $x];
}

my $pi_stream = stream
    sub { extr shift, 3 },
    sub { my ($z, $n) = @_; $n == extr $z, 4 },
    sub { my ($z, $n) = @_; comp([10, -10*$n, 0, 1], $z) },
    \&comp,
    [1, 0, 0, 1],
    sub { my $n = 0; sub { $n++; [$n, 4 * $n + 2, 0, 2 * $n + 1] } },
;
$|++;
print $pi_stream->(), '.';
print $pi_stream->() while 1;
```


==== Machin's Formula ====

Here is an original Perl 5 code, using Machin's formula.  Not the fastest program in the world.  As with the previous code, using either Math::GMP or Math::BigInt::GMP instead of the default bigint Calc backend will make it run thousands of times faster.


```perl
use bigint try=>
"GMP";

# Pi/4 = 4 arctan 1/5 - arctan 1/239
# expanding it with Taylor series with what's probably the dumbest method

my ($ds, $ns) = (1, 0);
my ($n5, $d5) = (16 * (25 * 3 - 1), 3 * 5**3);
my ($n2, $d2) = (4 * (239 * 239 * 3 - 1), 3 * 239**3);

sub next_term {
	my ($coef, $p) = @_[1, 2];
	$_[0] /= ($p - 4) * ($p - 2);
	$_[0] *= $p * ($p + 2) * $coef**4;
}

my $p2 = 5;
my $pow = 1;

$| = 1;
for (my $x = 5; ; $x += 4) {
	($ns, $ds) = ($ns * $d5 + $n5 * $pow * $ds, $ds * $d5);

	next_term($d5, 5, $x);
	$n5 = 16 * (5 * 5 * ($x + 2) - $x);

	while ($d5 > $d2) {
		($ns, $ds) = ($ns * $d2 - $n2 * $pow * $ds, $ds * $d2);
		$n2 = 4 * (239 * 239 * ($p2 + 2) - $p2);
		next_term($d2, 239, $p2);
		$p2 += 4;
	}

	my $ppow = 1;
	while ($pow * $n5 * 5**4 < $d5 && $pow * $n2 * $n2 * 239**4 < $d2) {
		$pow *= 10;
		$ppow *= 10;
	}

	if ($ppow > 1) {
		$ns *= $ppow;
	#FIX?	my $out = $ns->bdiv($ds);   # bugged?
		my $out = $ns / $ds;
		$ns %= $ds;

		$out = ("0" x (length($ppow) - length($out) - 1)) . $out;
		print $out;
	}

	if ( $p2 % 20 == 1) {
		my $g = Math::BigInt::bgcd($ds, $ns);
		$ds /= $g;
		$ns /= $g;
	}
}
```



### = Modules =

While no current CPAN module does continuous printing, there are (usually fast) ways to get digits of Pi.  Examples include:
```perl

use ntheory qw/Pi/;
say Pi(10000);

use Math::Pari qw/setprecision Pi/;
setprecision(10000);
say Pi;

use Math::MPFR;
my $pi = Math::MPFR->new();
Math::MPFR::Rmpfr_set_prec($pi, int(10000 * 3.322)+40);
Math::MPFR::Rmpfr_const_pi($pi, 0);
say Math::MPFR::Rmpfr_get_str($pi, 10, 10000, 0);

use Math::BigFloat try=>"GMP";    # Slow without Math::BigInt::GMP installed
say Math::BigFloat::bpi(10000);   # For over ~2k digits, slower than AGM

use Math::Big qw/pi/;    # Very slow
say pi(10000);

```



## Perl 6

```perl6
# based on http://www.mathpropress.com/stan/bibliography/spigot.pdf

sub stream(&next, &safe, &prod, &cons, $z is copy, @x) {
    gather loop {
        $z = safe($z, my $y = next($z)) ??
             prod($z, take $y)          !!
             cons($z, @x[$++])
    }
}

sub extr([$q, $r, $s, $t], $x) {
    ($q * $x + $r) div ($s * $x + $t)
}

sub comp([$q,$r,$s,$t], [$u,$v,$w,$x]) {
    [$q * $u + $r * $w,
     $q * $v + $r * $x,
     $s * $u + $t * $w,
     $s * $v + $t * $x]
}

my $pi :=
    stream -> $z { extr($z, 3) },
           -> $z, $n { $n == extr($z, 4) },
           -> $z, $n { comp([10, -10*$n, 0, 1], $z) },
           &comp,
           <1 0 0 1>,
           (1..*).map: { [$_, 4 * $_ + 2, 0, 2 * $_ + 1] }

for ^Inf -> $i {
    print $pi[$i];
    once print '.'
}
```



## Phix

I already had this golf entry to hand. Prints 2400 places, change the 8400 (derived from 2400*14/4) as needed, but I've not tested > that.

```Phix
integer a=10000,b,c=8400,d,e=0,g sequence f=repeat(floor(a/5),c+1) while c>0 do g=2*c d=0
b=c while b>0 do d+=f[b]*a g-=1 f[b]=remainder(d, g) d=floor(d/g) g-=1 b-=1 if b!=0 then
d*=b end if end while printf(1,"%04d",e+floor(d/a)) c-=14 e = remainder(d,a) end while
```

Someone was benchmarking the above against Lua, so I translated the Lua entry, and upped it to 2400 places, for a fairer test.

```Phix
integer n = 2400,
        len = floor(10*n/3)
sequence a = repeat(2,len)
integer nines = 0,
        predigit = 0
string res = ""
    for j=1 to n do
        integer q = 0
        for i=len to 1 by -1 do
            integer x = 10*a[i]+q*i,
                    d = 2*i-1
            a[i] = remainder(x,d)
            q = floor(x/d)
        end for
        a[1] = remainder(q,10)
        q = floor(q/10)
        if q==9 then
            nines = nines+1
        else
            integer nine = '9'
            if q==10 then
                predigit += 1
                q = 0
                nine = '0'
            end if
            res &= predigit+'0'&repeat(nine,nines)
            predigit = q
            nines = 0
        end if
    end for
    res &= predigit+'0'
    puts(1,res)
```



## PicoLisp

The following script uses the spigot algorithm published by Jeremy Gibbons. Hit Ctrl-C to stop it.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(de piDigit ()
   (job '((Q . 1) (R . 0) (S . 1) (K . 1) (N . 3) (L . 3))
      (while (>= (- (+ R (* 4 Q)) S) (* N S))
         (mapc set '(Q R S K N L)
            (list
               (* Q K)
               (* L (+ R (* 2 Q)))
               (* S L)
               (inc K)
               (/ (+ (* Q (+ 2 (* 7 K))) (* R L)) (* S L))
               (+ 2 L) ) ) )
      (prog1 N
         (let M (- (/ (* 10 (+ R (* 3 Q))) S) (* 10 N))
            (setq Q (* 10 Q)  R (* 10 (- R (* N S)))  N M) ) ) ) )

(prin (piDigit) ".")
(loop
   (prin (piDigit))
   (flush) )
```

Output:

```txt
3.14159265358979323846264338327950288419716939937510582097494459 ...
```



## PL/I


```PL/I
/* Uses the algorithm of S. Rabinowicz and S. Wagon, "A Spigot Algorithm */
/* for the Digits of Pi".                                                */
(subrg, fofl, size):
Pi_Spigot: procedure options (main);                 /* 21 January 2012. */
   declare (n, len) fixed binary;

   n = 1000;
   len = 10*n / 3;
   begin;
      declare ( i, j, k, q, nines, predigit ) fixed binary;
      declare x fixed binary (31);
      declare a(len) fixed binary (31);

      a = 2; /* Start with 2s */
      nines, predigit = 0; /* First predigit is a 0 */
      do j = 1 to n;
         q = 0;
         do i = len to 1 by -1; /* Work backwards */
            x = 10*a(i) + q*i;
            a(i) = mod (x, (2*i-1));
            q = x / (2*i-1);
         end;
         a(1) = mod(q, 10); q = q / 10;
         if q = 9 then nines = nines + 1;
         else if q = 10 then
            do;
               put edit(predigit+1) (f(1));
               do k = 1 to nines;
                  put edit ('0')(a(1)); /* zeros */
               end;
               predigit, nines = 0;
            end;
         else
            do;
               put edit(predigit) (f(1)); predigit = q;
               do k = 1 to nines; put edit ('9')(a(1)); end;
               nines = 0;
            end;
      end;
      put edit(predigit) (f(1));
   end; /* of begin block */
end Pi_Spigot;
```

output:

```txt

03141592653589793238462643383279502884197169399375105820974944592307816406286208
99862803482534211706798214808651328230664709384460955058223172535940812848111745
02841027019385211055596446229489549303819644288109756659334461284756482337867831
65271201909145648566923460348610454326648213393607260249141273724587006606315588
17488152092096282925409171536436789259036001133053054882046652138414695194151160
94330572703657595919530921861173819326117931051185480744623799627495673518857527
24891227938183011949129833673362440656643086021394946395224737190702179860943702
77053921717629317675238467481846766940513200056812714526356082778577134275778960
91736371787214684409012249534301465495853710507922796892589235420199561121290219
60864034418159813629774771309960518707211349999998372978049951059731732816096318
59502445945534690830264252230825334468503526193118817101000313783875288658753320
83814206171776691473035982534904287554687311595628638823537875937519577818577805
32171226806613001927876611195909216420198

```


## Powershell

With some tweaking.
Prints 100 digits a time. Total possible output limited by available memory.

```powershell

Function Get-Pi ( $Digits )
    {
    $Big = [bigint[]](0..10)

    $ndigits = 0
    $Output = ""

    $q = $t = $k = $Big[1]
    $r =           $Big[0]
    $l = $n =      $Big[3]
    # Calculate first digit
    $nr = ( $Big[2] * $q + $r ) * $l
    $nn = ( $q * ( $Big[7] * $k + $Big[2] ) + $r * $l ) / ( $t * $l )
    $q *= $k
    $t *= $l
    $l += $Big[2]
    $k = $k + $Big[1]
    $n = $nn
    $r = $nr

    $Output += [string]$n + '.'
    $ndigits++

    $nr = $Big[10] * ( $r - $n * $t )
    $n = ( ( $Big[10] * ( 3 * $q + $r ) ) / $t ) - 10 * $n
    $q *= $Big[10]
    $r = $nr

    While ( $ndigits -lt $Digits )
        {
        While ( $ndigits % 100 -ne 0 -or -not $Output )
            {
            If ( $Big[4] * $q + $r - $t -lt $n * $t )
                {
                $Output += [string]$n
                $ndigits++
                $nr = $Big[10] * ( $r - $n * $t )
                $n = ( ( $Big[10] * ( 3 * $q + $r ) ) / $t ) - 10 * $n
                $q *= $Big[10]
                $r = $nr
                }
            Else
                {
                $nr = ( $Big[2] * $q + $r ) * $l
                $nn = ( $q * ( $Big[7] * $k + $Big[2] ) + $r * $l ) / ( $t * $l )
                $q *= $k
                $t *= $l
                $l += $Big[2]
                $k = $k + $Big[1]
                $n = $nn
                $r = $nr
                }
            }
        $Output
        $Output = ""
        }
    }

```


Alternate version using .Net classes

```powershell

[math]::pi

```

Outputs:
<lang>.Net digits of pi
3.14159265358979

```



## PureBasic

Calculate Pi, limited to ~24 M-digits for memory and speed reasons.

```PureBasic
#SCALE = 10000
#ARRINT=  2000

Procedure Pi(Digits)
  Protected First=#True, Text$
  Protected Carry, i, j, sum
  Dim Arr(Digits)
  For i=0 To Digits
    Arr(i)=#ARRINT
  Next
  i=Digits
  While i>0
    sum=0
    j=i
    While j>0
      sum*j+#SCALE*arr(j)
      Arr(j)=sum%(j*2-1)
      sum/(j*2-1)
      j-1
    Wend
    Text$ = RSet(Str(Carry+sum/#SCALE),4,"0")
    If First
      Text$ = ReplaceString(Text$,"3","3.")
      First = #False
    EndIf
    Print(Text$)
    Carry=sum%#SCALE
    i-14
  Wend
EndProcedure

If OpenConsole()
  SetConsoleCtrlHandler_(?Ctrl,#True)
  Pi(24*1024*1024)
EndIf
End

Ctrl:
PrintN(#CRLF$+"Ctrl-C was pressed")
End
```



## Python


```Python
def calcPi():
    q, r, t, k, n, l = 1, 0, 1, 1, 3, 3
    while True:
        if 4*q+r-t < n*t:
            yield n
            nr = 10*(r-n*t)
            n  = ((10*(3*q+r))//t)-10*n
            q  *= 10
            r  = nr
        else:
            nr = (2*q+r)*l
            nn = (q*(7*k)+2+(r*l))//(t*l)
            q  *= k
            t  *= l
            l  += 2
            k += 1
            n  = nn
            r  = nr

import sys
pi_digits = calcPi()
i = 0
for d in pi_digits:
    sys.stdout.write(str(d))
    i += 1
    if i == 40: print(""); i = 0
```
output

```txt

3141592653589793238462643383279502884197
1693993751058209749445923078164062862089
9862803482534211706798214808651328230664
7093844609550582231725359408128481117450
2841027019385211055596446229489549303819
6442881097566593344612847564823378678316
5271201909145648566923460348610454326648
2133936072602491412737245870066063155881
7488152092096282925409171536436789259036
0011330530548820466521384146951941511609
4330572703657595919530921861173819326117
...

```



## R


```rsplus

suppressMessages(library(gmp))
ONE <- as.bigz("1")
TWO <- as.bigz("2")
THREE <- as.bigz("3")
FOUR <- as.bigz("4")
SEVEN <- as.bigz("7")
TEN <- as.bigz("10")

q <- as.bigz("1")
r <- as.bigz("0")
t <- as.bigz("1")
k <- as.bigz("1")
n <- as.bigz("3")
l <- as.bigz("3")

char_printed <- 0

how_many <- 1000

first <- TRUE
while (how_many > 0) {
  if ((FOUR * q + r - t) < (n * t)) {
    if (char_printed == 80) {
      cat("\n")
      char_printed <- 0
    }
    how_many <- how_many - 1
    char_printed <- char_printed + 1
    cat(as.integer(n))
    if (first) {
      cat(".")
      first <- FALSE
      char_printed <- char_printed + 1
    }
    nr <- as.bigz(TEN * (r - n * t))
    n <- as.bigz(((TEN * (THREE * q + r)) %/% t) - (TEN * n))
    q <- as.bigz(q * TEN)
    r <- as.bigz(nr)
  } else {
    nr <- as.bigz((TWO * q + r) * l)
    nn <- as.bigz((q * (SEVEN * k + TWO) + r * l) %/% (t * l))
    q <- as.bigz(q * k)
    t <- as.bigz(t * l)
    l <- as.bigz(l + TWO)
    k <- as.bigz(k + ONE)
    n <- as.bigz(nn)
    r <- as.bigz(nr)
  }
}
cat("\n")

```

'''Output:'''

```txt

3.141592653589793238462643383279502884197169399375105820974944592307816406286208
99862803482534211706798214808651328230664709384460955058223172535940812848111745
02841027019385211055596446229489549303819644288109756659334461284756482337867831
65271201909145648566923460348610454326648213393607260249141273724587006606315588
17488152092096282925409171536436789259036001133053054882046652138414695194151160
94330572703657595919530921861173819326117931051185480744623799627495673518857527
24891227938183011949129833673362440656643086021394946395224737190702179860943702
77053921717629317675238467481846766940513200056812714526356082778577134275778960
91736371787214684409012249534301465495853710507922796892589235420199561121290219
60864034418159813629774771309960518707211349999998372978049951059731732816096318
59502445945534690830264252230825334468503526193118817101000313783875288658753320
83814206171776691473035982534904287554687311595628638823537875937519577818577805
32171226806613001927876611195909216420198

```



## Racket


Utilizing Jeremy Gibbons spigot algorithm and racket generator:


```racket

#lang racket
(require racket/generator)

(define pidig
  (generator ()
    (let loop ([q 1] [r 0] [t 1] [k 1] [n 3] [l 3])
      (if (< (- (+ r (* 4 q)) t) (* n t))
        (begin (yield n)
               (loop (* q 10) (* 10 (- r (* n t))) t k
                     (- (quotient (* 10 (+ (* 3 q) r)) t) (* 10 n))
                     l))
        (loop (* q k) (* (+ (* 2 q) r) l) (* t l) (+ 1 k)
              (quotient (+ (* (+ 2 (* 7 k)) q) (* r l)) (* t l))
              (+ l 2))))))

(for ([i (in-naturals)])
  (display (pidig))
  (when (zero? i) (display "." ))
  (when (zero? (modulo i 80)) (newline)))

```


Output:

<lang>
3.14159265358979323846264338327950288419716939937510...

```



## REXX


### version 1

This REXX program calculates decimal digits of   <big><big><math>\pi</math></big></big>   using John Machin's formula.


It should be noted that the program's mechanism spits out the next (new) decimal digit(s) of   <big><big><math>\pi</math></big></big>.


The REXX program uses the following formula to calculate   <big><big><math>\pi</math></big></big>:

```txt

                    ┌─   ─┐                ┌─     ─┐
  π                 │  1  │                │   1   │           John
 ───  =   4 ∙ arctan│ ─── │     -    arctan│ ───── │             Machin's
  4                 │  5  │                │  239  │               formula
                    └─   ─┘                └─     ─┘

 which expands into:

     ┌─                                                                      ─┐
     │    1         1          1          1          1           1            │
4 ∙  │   ───  -  ──────  +  ──────  -  ──────  +  ──────  -  ────────  +  ... │
     │     1         3          5          7          9           11          │
     │  1∙5       3∙5        5∙5        7∙5        9∙5        11∙5            │
     └─                                                                      ─┘


     ┌─                                                                      ─┐
     │    1         1          1          1          1           1            │
 -   │   ───  -  ──────  +  ──────  -  ──────  +  ──────  -  ────────  +  ... │
     │      1         3          5          7          9           11         │
     │ 1∙239     3∙239      5∙239      7∙239      9∙239      11∙239           │
     └─                                                                      ─┘

```


```rexx
/*REXX program spits out decimal digits of pi  (one digit at a time)  until  Ctrl-Break.*/
parse arg digs oFID .                            /*obtain optional argument from the CL.*/
if digs=='' | digs==","  then digs=1e6           /*Not specified?  Then use the default.*/
if oFID=='' | oFID==","  then oFID='PI_SPIT.OUT' /* "      "         "   "   "      "   */
numeric digits digs                              /*with bigger digs, spitting is slower.*/
call time 'Reset'                                /*reset the wall─clock (elapsed) timer.*/
signal on halt                                   /*───► HALT when Ctrl─Break is pressed.*/
pi=0;  v=5;   vv=v*v;   g=239;   gg=g*g;  spit=0 /*assign some values to some variables.*/
s=16                                             /*calculate π with increasing accuracy */
r=4;    do n=1  by 2  until  old=pi;      old=pi /*just calculate  pi  with odd integers*/
        pi=pi + s/(n*v) - r/(n*g)                /*    ···  using John Machin's formula.*/
        if pi==old  then leave                   /*have we exceeded the DIGITS accuracy?*/
        s=-s;             r=-r;  v=v*vv;  g=g*gg /*compute some variables for shortcuts.*/
                 do j=spit+1  to compare(pi,old) /*spit out some (new)  digits of π (pi)*/
                 parse var  pi  =(j)  spit  +1   /*equivalent to:   spit=substr(pi,j,1) */
                 call charout     ,spit          /*display one (new) decimal digit of π.*/
                 call charout oFID,spit          /*··· and also write π digit to a file.*/
                 end   /*j*/                     /* [↑]  0, 1, or 2 decimal dig are spit*/
        spit=j-1                                 /*adjust for  DO  loop index increment.*/
        end            /*n*/
say                                              /*stick a fork in it,  we're all done. */
exit: say;  say n%2+1  'iterations took'  format(time("Elapsed"),,2)  'seconds.';     exit
halt: say;  say 'PI_SPIT halted via use of Ctrl-Break.';  signal exit /*show iterations.*/
```

(Shown at four-fifth size.)

<pre style="font-size:80%">
3.1415926535897932384626433832794028841971794993741058209749445923078164062861089986280348253411170679821480865132823066470938446095505822317253594081
284811174502840027019385211055596446229489549203819644298109756659334461284756482338867831652711019091456485669234603486104543266482133936072602491412
737245970066063156881748815209209628292540917153643679926903500113205305498204665213841469519415116094330572703657595919530921861173819326118931051185
480744623899627595673518857527249912279381830119491398336733624406566420860213949463952247371907021898609437027705392171762931767523846748184676794051
310005681271452635608277857713427577996091736371787214684308012259534301465595853700508922797892589235420299560121280219608630344181598136297747713199
605187072113499999983729780499410597317328160962185940244594553469083026425123082533446850352619311881700000031378387528865875331083814206171776691473
035982534904287554687311695628638823538876937529577818587805321712268066120019278766111959092164201989380952572000654858632788659361533818289682303019
510353018529699958736225994138912597217752834791315155748572424541506969508395330168617278558890750983817546374649393192550604009277016711390098488240
128583616035637076501047001819429555961999467688374594482553897747268471040475346462080466842590794912933136770299891521047521620569660240570381501935
112533824200355876402474964732639142992726042699227967823547816350093417216412199245863150202861829745557067598385054945885869279956909272007975092029
553211653449872027559502364806654991298818347977535663698074265425278625518184175746728909877728938000816470600161452491921732172147723401414419735685
481613611573525521334757418494684385233239073941433345477624168625189835695855620992192221842725502542568976717905946016534668059886272327917860857843
838279679766814540009538837863609506800642251252051173939848960841284886269456042419652850222006611863067442786220491949440471237138869509563643719172
874677646575739624139908658326469958133904780275901994657640789512694683983525956098258226205224894077267194782684825014769909026401363944374553050682
034962524517493996514314298091906592509372216964615157198583874106978859597729754989201617539284681382686838789427741569918559252469539594310599725246
808459872736446958486538367362226260991246080512438843904412441365597627807977156914359977001296160894416958685558484063534210722258285886481584560284
060168427494522674676789952521385225599546667278239864565951163549862205774564980355936345681743241125150760694794410965960940252288897108931456691368
672287499405600014032086179286809208747609178249385890097149096759852613655497819931297848116829999487226588048575630142704775551323796414515237462343
645428584448952658678110511413547357495231134271661021359695362214429524849371871101457654036902799344037420073105785390622983974478084785896833114457
138697519435064302184521900484810053706146806749192781912979399510614196634287544406437451237181921899983900159195618146751426912497489409071864942319
615679452080951465502252316038819301410937621378569566399387870830390797910773467221825625996614014214030680384477345492026054146659251014974428507325
186650021324340881907004863317346496514539057962685600055081066587979981635747363840525714591029970641301009712062804390497695156771577004103378699360
072305597631763694218721251471205329281918261861258672215892984148488291644606095752706957220917567116722900981690915280173506712748583222871835209354
965725121083579151379881091444200067500334671003141267011379908658516498315019701651511685171437657618351556508849099998599823873455283216355076489185
358932261854896321329330898570641046752590709154814165498594616371802709819943199244889575712828906923233260972997120844335732654993823912932597463667
205835041428138830310382490375898524374417039132765618093773444020707469211101913020330380297620100100449293215160842444869637679838952286847831235526
582131449576857262433441892039686426243400773227978028073199154410010446823252716101052652272111660396665572092547110557853763466820653009896526918620
564769312560586356620185580007293606698764860179104533488503461136576867532494416680496265897977185560845529654126654085306143444318686769741456614068
006002388776591343017127494704205622205399945613140711260003078547332699390814546646458807972708266830634328587857983052358089330657573067954571637752
542011149557615813002501262285941302164715509792592319907965473761255176567513575178296664547791744011299614890304639947132962107340437519957369614599
019389713111890429782856475032031986915140287080859904801094111472213179476477726224142548545403321571853061422881375850430633217518298986622371721591
50771669254748739986654959450114653062843366393790039769265672146385306736096571209180763832716641627488880088692560280228472104021711186082041900042
PI_SPIT halted via use of Ctrl-Break.

3431 iterations took 3501.78 seconds.

```



### version 2

This REXX version is a translation of   '''Icon'''   with some speed optimizations.

This algorithm is limited to the number of decimal digits as specified with the   '''numeric digits ddd'''     (line or statement six).

```rexx
/*REXX program spits out decimal digits of pi  (one digit at a time)  until  Ctrl-Break.*/
signal on halt                                   /*───► HALT when Ctrl─Break is pressed.*/
parse arg digs oFID .                            /*obtain optional argument from the CL.*/
if digs=='' | digs==","  then digs= 300          /*Not specified?  Then use the default.*/
if oFID=='' | oFID==","  then oFID='PI_SPIT2.OUT' /* "     "         "   "   "      "   */
numeric digits digs                              /*with bigger digs, spitting is slower.*/
q=1;   r=0;   t=1;   k=1;   n=3;   L=3;   z=0    /*define some REXX variables.          */
dot=1                                            /*DOT≡a flag when a dot in pi is shown.*/
      do until z==digs;            qq= q+q       /*  qq     is a fast version of:  q*2  */
      tn= t*n                                    /*  t*n    is used twice  (below).     */
      if qq+qq+r-t < tn  then do;  z= z+1        /*  qq+qq  is faster than   qq*2       */
                                   call charout     , n
                                   call charout oFID, n
                                   if dot  then do;    dot=0;    call charout     , .
                                                                 call charout oFID, .
                                                end
                                   nr= (r - tn) * 10
                                   n = ((( (qq+q+r) * 10) / t) - n*10)  %1
                                   q = q*10
                              end
                         else do;  nr= (qq+r) * L
                                   tL= t*L
                                   n = (q * (k*7 + 2)  +  r*L) / tL  %1
                                   q = q*k
                                   t = tL
                                   L = L+2
                                   k = k+1
                              end                /* %1≡fast way doing TRUNC of a number.*/
      r=nr
      end   /*forever*/
exit                                             /*stick a fork in it,  we're all done. */
halt: say;     say  'PI_SPIT2 halted via use of Ctrl-Break.';           exit
```





## Ruby

```ruby
pi_digits = Enumerator.new do |y|
  q, r, t, k, n, l = 1, 0, 1, 1, 3, 3
  loop do
    if 4*q+r-t < n*t
      y << n
      nr = 10*(r-n*t)
      n = ((10*(3*q+r)) / t) - 10*n
      q *= 10
      r = nr
    else
      nr = (2*q+r) * l
      nn = (q*(7*k+2)+r*l) / (t*l)
      q *= k
      t *= l
      l += 2
      k += 1
      n = nn
      r = nr
    end
  end
end

print pi_digits.next, "."
loop { print pi_digits.next }
```



## Rust

```Rust
use num_bigint::BigInt;

fn main() {
    calc_pi();
}

fn calc_pi() {
    let mut q = BigInt::from(1);
    let mut r = BigInt::from(0);
    let mut t = BigInt::from(1);
    let mut k = BigInt::from(1);
    let mut n = BigInt::from(3);
    let mut l = BigInt::from(3);
    let mut first = true;
    loop {
        if &q * 4 + &r - &t < &n * &t {
            print!("{}", n);
            if first {
                print!(".");
                first = false;
            }
            let nr = (&r - &n * &t) * 10;
            n = (&q * 3 + &r) * 10 / &t - &n * 10;
            q *= 10;
            r = nr;
        } else {
            let nr = (&q * 2 + &r) * &l;
            let nn = (&q * &k * 7 + 2 + &r * &l) / (&t * &l);
            q *= &k;
            t *= &l;
            l += 2;
            k += 1;
            n = nn;
            r = nr;
        }
    }
}
```



## Scala


```scala
object Pi {
  class PiIterator extends Iterable[BigInt]{
    var r:BigInt=0
    var q, t, k:BigInt=1
    var n, l:BigInt=3
    var nr, nn:BigInt=0

    def iterator: Iterator[BigInt]=new Iterator[BigInt]{
      def hasNext=true
      def next():BigInt={
        while((4*q+r-t) >= (n*t)) {
          nr = (2*q+r)*l
          nn = (q*(7*k)+2+(r*l))/(t*l)
          q = q * k
          t = t * l
          l = l + 2
          k = k + 1
          n  = nn
          r  = nr
        }
        val ret=n
        nr = 10*(r-n*t)
        n  = ((10*(3*q+r))/t)-(10*n)
        q  = q * 10
        r  = nr
        ret
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val it=new PiIterator
    println((it head) + "." + (it take 300 mkString))
  }
}
```

Output:

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998
62803482534211706798214808651328230664709384460955058223172535940812848111745028410
27019385211055596446229489549303819644288109756659334461284756482337867831652712019
09145648566923460348610454326648213393607260249141273
```


## Scheme


```scala

(import (rnrs))

(define (calc-pi yield)
  (let loop ((q 1) (r 0) (t 1) (k 1) (n 3) (l 3))
    (if (< (- (+ (* 4 q) r) t) (* n t))
        (begin
          (yield n)
          (loop (* q  10)
                (* 10 (- r (* n t)))
                t
                k
                (- (div (* 10 (+ (* 3 q) r)) t) (* 10 n))
                l))
        (begin
          (loop (* q k)
                (* (+ (* 2 q) r) l)
                (* t l)
                (+ k 1)
                (div (+ (* q (* 7 k)) 2 (* r l)) (* t l))
                (+ l 2))))))

(let ((i 0))
  (calc-pi
    (lambda (d)
      (display d)
      (set! i (+ i 1))
      (if (= 40 i)
          (begin
            (newline)
            (set! i 0))))))

```

Output:

```txt
3141592653589793238462643383279502884197
1693993751058209749445923078164062862089
9862803482534211706798214808651328230664
7093844609550582231725359408128481117450
2841027019385211055596446229489549303819
6442881097566593344612847564823378678316
5271201909145648566923460348610454326648
2133936072602491412737245870066063155881
7488152092096282925409171536436789259036
0011330530548820466521384146951941511609
4330572703657595919530921861173819326117
9310511854807446237996274956735188575272
4891227938183011949129833673362440656643
0860213949463952247371907021798609437027
7053921717629317675238467481846766940513
2000568127145263560827785771342757789609
...
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const proc: main is func
  local
    var bigInteger: q is 1_;
    var bigInteger: r is 0_;
    var bigInteger: t is 1_;
    var bigInteger: k is 1_;
    var bigInteger: n is 3_;
    var bigInteger: l is 3_;
    var bigInteger: nn is 0_;
    var bigInteger: nr is 0_;
    var boolean: first is TRUE;
  begin
    while TRUE do
      if 4_ * q + r - t < n * t then
        write(n);
        if first then
          write(".");
          first := FALSE;
        end if;
        nr := 10_ * (r - n * t);
        n := 10_ * (3_ * q + r) div t - 10_ * n;
        q *:= 10_;
        r := nr;
        flush(OUT);
      else
        nr := (2_ * q + r) * l;
        nn := (q * (7_ * k + 2_) + r * l) div (t * l);
        q *:= k;
        t *:= l;
        l +:= 2_;
        incr(k);
        n := nn;
        r := nr;
      end if;
    end while;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#pi_spigot_algorithm]


## Sidef


```ruby
func pi(callback) {
    var (q, r, t, k, n, l) = (1, 0, 1, 1, 3, 3)
    loop {
        if ((4*q + r - t) < n*t) {
            callback(n)
            static _dot = callback('.')
            var nr = 10*(r - n*t)
            n = ((10*(3*q + r)) // t - 10*n)
            q *= 10
            r = nr
        }
        else {
            var nr = ((2*q + r) * l)
            var nn = ((q*(7*k + 2) + r*l) // (t*l))
            q *= k
            t *= l
            l += 2
            k += 1
            n = nn
            r = nr
        }
    }
}
 
STDOUT.autoflush(true)
pi(func(digit){ print digit })
```


## Simula


```simula
CLASS BIGNUM;
BEGIN

    BOOLEAN PROCEDURE TISZERO(T); TEXT T;
        TISZERO := T = "0";

    TEXT PROCEDURE TSHL(T); TEXT T;
        TSHL :- IF TISZERO(T) THEN T ELSE T & "0";

    TEXT PROCEDURE TSHR(T); TEXT T;
        TSHR :- IF T.LENGTH = 1 THEN "0" ELSE T.SUB(1, T.LENGTH - 1);

    INTEGER PROCEDURE TSIGN(T); TEXT T;
        TSIGN := IF TISZERO(T) THEN 0
            ELSE IF T.SUB(1, 1) = "-" THEN -1
            ELSE 1;

    TEXT PROCEDURE TABS(T); TEXT T;
        TABS :- IF TSIGN(T) < 0 THEN T.SUB(2, T.LENGTH - 1) ELSE T;

    TEXT PROCEDURE TNEGATE(T); TEXT T;
        TNEGATE :- IF TSIGN(T) <= 0 THEN TABS(T) ELSE ("-" & T);

    TEXT PROCEDURE TREVERSE(T); TEXT T;
    BEGIN
        INTEGER I, J;
        I := 1; J := T.LENGTH;
        WHILE I < J DO
        BEGIN CHARACTER C1, C2;
            T.SETPOS(I); C1 := T.GETCHAR;
            T.SETPOS(J); C2 := T.GETCHAR;
            T.SETPOS(I); T.PUTCHAR(C2);
            T.SETPOS(J); T.PUTCHAR(C1);
            I := I + 1;
            J := J - 1;
        END;
        TREVERSE :- T;
    END TREVERSE;

    INTEGER PROCEDURE TCMPUNSIGNED(A, B); TEXT A, B;
    BEGIN
        INTEGER ALEN, BLEN, RESULT;
        ALEN := A.LENGTH; BLEN := B.LENGTH;
        IF ALEN < BLEN THEN
            RESULT := -1
        ELSE IF ALEN > BLEN THEN
            RESULT := 1
        ELSE BEGIN
            INTEGER CMP, I; BOOLEAN DONE;
            A.SETPOS(1);
            B.SETPOS(1);
            I := 1;
            WHILE I <= ALEN AND NOT DONE DO
            BEGIN
                I := I + 1;
                CMP := RANK(A.GETCHAR) - RANK(B.GETCHAR);
                IF NOT (CMP = 0) THEN
                    DONE := TRUE;
            END;
            RESULT := CMP;
        END;
        TCMPUNSIGNED := RESULT;
    END TCMPUNSIGNED;

    INTEGER PROCEDURE TCMP(A, B); TEXT A, B;
    BEGIN
        BOOLEAN ANEG, BNEG;
        ANEG := TSIGN(A) < 0; BNEG := TSIGN(B) < 0;
        IF ANEG AND BNEG THEN
           TCMP := -TCMPUNSIGNED(TABS(A), TABS(B))
        ELSE IF NOT ANEG AND BNEG THEN
           TCMP := 1
        ELSE IF ANEG AND NOT BNEG THEN
           TCMP := -1
        ELSE
           TCMP := TCMPUNSIGNED(A, B);
    END TCMP;

    TEXT PROCEDURE TADDUNSIGNED(A, B); TEXT A, B;
    BEGIN
        INTEGER CARRY, I, J;
        TEXT BF;
        I := A.LENGTH;
        J := B.LENGTH;
        BF :- BLANKS(MAX(I, J) + 1);
        WHILE I >= 1 OR J >= 1 DO BEGIN
            INTEGER X, Y, Z;
            IF I >= 1 THEN BEGIN
                A.SETPOS(I); I := I - 1; X := RANK(A.GETCHAR) - RANK('0');
            END;
            IF J >= 1 THEN BEGIN
                B.SETPOS(J); J := J - 1; Y := RANK(B.GETCHAR) - RANK('0');
            END;
            Z := X + Y + CARRY;
            IF Z < 10 THEN
            BEGIN BF.PUTCHAR(CHAR(Z + RANK('0'))); CARRY := 0;
            END ELSE
            BEGIN BF.PUTCHAR(CHAR(MOD(Z, 10) + RANK('0'))); CARRY := 1;
            END;
        END;
        IF CARRY > 0 THEN
            BF.PUTCHAR(CHAR(CARRY + RANK('0')));
        BF :- TREVERSE(BF.STRIP);
        TADDUNSIGNED :- BF;
    END TADDUNSIGNED;

    TEXT PROCEDURE TADD(A, B); TEXT A, B;
    BEGIN
        BOOLEAN ANEG, BNEG;
        ANEG := TSIGN(A) < 0; BNEG := TSIGN(B) < 0;
        IF NOT ANEG AND BNEG THEN          ! (+7)+(-5) =  (7-5) =   2 ;
            TADD :- TSUBUNSIGNED(A, TABS(B))
        ELSE IF ANEG AND NOT BNEG THEN     ! (-7)+(+5) =  (5-7) =  -2 ;
            TADD :- TSUBUNSIGNED(B, TABS(A))
        ELSE IF ANEG AND BNEG THEN         ! (-7)+(-5) = -(7+5) = -12 ;
            TADD :- TNEGATE(TADDUNSIGNED(TABS(A), TABS(B)))
        ELSE                               ! (+7)+(+5) =  (7+5) =  12 ;
            TADD :- TADDUNSIGNED(A, B);
    END TADD;

    TEXT PROCEDURE TSUBUNSIGNED(A, B); TEXT A, B;
    BEGIN
        INTEGER I, J, CARRY;
        I := A.LENGTH; J := B.LENGTH;
        IF I < J OR I = J AND A < B THEN
            TSUBUNSIGNED :- TNEGATE(TSUBUNSIGNED(B, A)) ELSE
        BEGIN
            TEXT BF;
            BF :- BLANKS(MAX(I, J) + 1);
            WHILE I >= 1 OR J >= 1 DO
            BEGIN
                INTEGER X, Y, Z;
                IF I >= 1 THEN
                BEGIN A.SETPOS(I); I := I - 1;
                    X := RANK(A.GETCHAR) - RANK('0');
                END;
                IF J >= 1 THEN
                BEGIN B.SETPOS(J); J := J - 1;
                    Y := RANK(B.GETCHAR) - RANK('0');
                END;
                Z := X - Y - CARRY;
                IF Z >= 0 THEN
                BEGIN
                    BF.PUTCHAR(CHAR(RANK('0') + Z));
                    CARRY := 0;
                END ELSE
                BEGIN
                    BF.PUTCHAR(CHAR(RANK('0') + MOD(10 + Z, 10)));
                    CARRY := 1; ! (Z / 10);
                END;
            END;
            BF :- BF.STRIP;
            BF :- TREVERSE(BF);
            BF.SETPOS(1);
            WHILE BF.LENGTH > 1 AND THEN BF.GETCHAR = '0' DO
            BEGIN
                BF :- BF.SUB(2, BF.LENGTH - 1);
                BF.SETPOS(1);
            END;
            TSUBUNSIGNED :- BF;
        END;
    END TSUBUNSIGNED;

    TEXT PROCEDURE TSUB(A, B); TEXT A, B;
    BEGIN
        BOOLEAN ANEG, BNEG;
        ANEG := TSIGN(A) < 0; BNEG := TSIGN(B) < 0;
        IF ANEG AND BNEG THEN              ! (-7)-(-5) = -(7-5) =  -2 ;
            TSUB :- TNEGATE(TSUBUNSIGNED(TABS(A), TABS(B)))
        ELSE IF NOT ANEG AND BNEG THEN     ! (+7)-(-5) =  (7+5) =  12 ;
            TSUB :- TADDUNSIGNED(A, TABS(B))
        ELSE IF ANEG AND NOT BNEG THEN     ! (-7)-(+5) = -(7+5) = -12 ;
            TSUB :- TNEGATE(TADDUNSIGNED(TABS(A), B))
        ELSE                               ! (+7)-(+5) =  (7-5) =   2 ;
            TSUB :- TSUBUNSIGNED(A, B);
    END TSUB;

    TEXT PROCEDURE TMULUNSIGNED(A, B); TEXT A, B;
    BEGIN
        INTEGER ALEN, BLEN;
        ALEN := A.LENGTH; BLEN := B.LENGTH;
        IF ALEN < BLEN THEN
            TMULUNSIGNED :- TMULUNSIGNED(B, A)
        ELSE BEGIN
            TEXT PRODUCT; INTEGER J;
            PRODUCT :- "0";
            FOR J := 1 STEP 1 UNTIL BLEN DO BEGIN
                TEXT PART; INTEGER I, Y, CARRY;
                B.SETPOS(J); Y := RANK(B.GETCHAR) - RANK('0');
                PART :- BLANKS(ALEN + BLEN + 1); PART.SETPOS(1);
                FOR I := ALEN STEP -1 UNTIL 1 DO BEGIN
                    INTEGER X, Z;
                    A.SETPOS(I); X := RANK(A.GETCHAR) - RANK('0');
                    Z := X * Y + CARRY;
                    IF Z < 10 THEN BEGIN
                        PART.PUTCHAR(CHAR(RANK('0') + Z));
                        CARRY := 0;
                    END ELSE BEGIN
                        PART.PUTCHAR(CHAR(RANK('0') + MOD(Z, 10)));
                        CARRY := Z // 10;
                    END;
                END;
                IF CARRY > 0 THEN
                    PART.PUTCHAR(CHAR(RANK('0') + CARRY));
                PART :- PART.SUB(1, PART.POS - 1);
                PART :- TREVERSE(PART);
                PART.SETPOS(1);
                WHILE PART.LENGTH > 1 AND THEN PART.GETCHAR = '0' DO
                BEGIN
                    PART :- PART.SUB(2, PART.LENGTH - 1);
                    PART.SETPOS(1);
                END;
                PRODUCT :- TADDUNSIGNED(TSHL(PRODUCT), PART);
            END;
            TMULUNSIGNED :- PRODUCT;
        END;
    END TMULUNSIGNED;

    TEXT PROCEDURE TMUL(A, B); TEXT A, B;
    BEGIN
        BOOLEAN ANEG, BNEG;
        ANEG := TSIGN(A) < 0; BNEG := TSIGN(B) < 0;
        IF ANEG AND BNEG THEN              ! (-7)*(-5) =  (7*5) =>  35 ;
           TMUL :- TMULUNSIGNED(TABS(A), TABS(B))
        ELSE IF NOT ANEG AND BNEG THEN     ! (+7)*(-5) = -(7*5) => -35 ;
           TMUL :- TNEGATE(TMULUNSIGNED(A, TABS(B)))
        ELSE IF ANEG AND NOT BNEG THEN     ! (-7)*(+5) = -(7*5) => -35 ;
           TMUL :- TNEGATE(TMULUNSIGNED(TABS(A), B))
        ELSE                               ! (+7)*(+5) =  (7*5) =>  35 ;
           TMUL :- TMULUNSIGNED(A, B);
    END TMUL;

    CLASS DIVMOD(DIV,MOD); TEXT DIV,MOD;;

    REF(DIVMOD) PROCEDURE TDIVMODUNSIGNED(A, B); TEXT A, B;
    BEGIN
        INTEGER CC;
        REF(DIVMOD) RESULT;
        IF TISZERO(B) THEN
            ERROR("DIVISION BY ZERO");
        CC := TCMPUNSIGNED(A, B);
        IF CC < 0 THEN
            RESULT :- NEW DIVMOD("0", A)
        ELSE IF CC = 0 THEN
            RESULT :- NEW DIVMOD("1", "0")
        ELSE BEGIN
            INTEGER ALEN, BLEN, AIDX;
            TEXT Q, R;
            ALEN := A.LENGTH; BLEN := B.LENGTH;
            Q :- BLANKS(ALEN); Q.SETPOS(1);
            R :- BLANKS(ALEN); R.SETPOS(1);
            R := A.SUB(1, BLEN - 1); R.SETPOS(BLEN);
            FOR AIDX := BLEN STEP 1 UNTIL ALEN DO
            BEGIN
                INTEGER COUNT; BOOLEAN DONE;
                IF TISZERO(R.STRIP) THEN
                    R.SETPOS(1);
                A.SETPOS(AIDX); R.PUTCHAR(A.GETCHAR);
                WHILE NOT DONE DO
                BEGIN
                    TEXT DIFF;
                    DIFF :- TSUBUNSIGNED(R.STRIP, B);
                    IF TSIGN(DIFF) < 0 THEN
                        DONE := TRUE
                    ELSE BEGIN
                        R := DIFF; R.SETPOS(DIFF.LENGTH + 1);
                        COUNT := COUNT + 1;
                    END;
                END;
                IF (NOT (COUNT = 0)) OR (NOT (Q.POS = 1)) THEN
                    Q.PUTCHAR(CHAR(COUNT + RANK('0')));
            END;
            RESULT :- NEW DIVMOD(Q.STRIP, R.STRIP);
        END;
        TDIVMODUNSIGNED :- RESULT;
    END TDIVMODUNSIGNED;

    REF(DIVMOD) PROCEDURE TDIVMOD(A, B); TEXT A, B;
    BEGIN
        BOOLEAN ANEG, BNEG; REF(DIVMOD) RESULT;
        ANEG := TSIGN(A) < 0; BNEG := TSIGN(B) < 0;
        IF ANEG AND BNEG THEN
            BEGIN
                RESULT :- TDIVMOD(TABS(A), TABS(B));
                RESULT.MOD :- TNEGATE(RESULT.MOD);
            END
        ELSE IF NOT ANEG AND BNEG THEN
            BEGIN
                RESULT :- TDIVMOD(A, TABS(B));
                RESULT.DIV :- TNEGATE(RESULT.DIV);
            END
        ELSE IF ANEG AND NOT BNEG THEN
            BEGIN
                RESULT :- TDIVMOD(TABS(A), B);
                RESULT.DIV :- TNEGATE(RESULT.DIV);
                RESULT.MOD :- TNEGATE(RESULT.MOD);
            END
        ELSE
            RESULT :- TDIVMODUNSIGNED(A, B);
        TDIVMOD :- RESULT;
    END TDIVMOD;

    TEXT PROCEDURE TDIV(A, B); TEXT A, B;
        TDIV :- TDIVMOD(A, B).DIV;

    TEXT PROCEDURE TMOD(A, B); TEXT A, B;
        TMOD :- TDIVMOD(A, B).MOD;

END BIGNUM;
```

```simula
EXTERNAL CLASS BIGNUM;
BIGNUM
BEGIN

    PROCEDURE CALCPI;
    BEGIN
        INTEGER I;
        TEXT Q, R, T, K, N, L;
        COMMENT
        !  q, r, t, k, n, l = 1, 0, 1, 1, 3, 3
        ;
        Q :- COPY("1");
        R :- COPY("0");
        T :- COPY("1");
        K :- COPY("1");
        N :- COPY("3");
        L :- COPY("3");
        WHILE TRUE DO
        BEGIN
           COMMENT
           !  if 4*q+r-t < n*t
           ;
           IF TCMP(TSUB(TADD(TMUL("4",Q),R),T),TMUL(N,T)) < 0 THEN
           BEGIN
               TEXT NR;
               OUTTEXT(N);
               I := I + 1;
               IF I = 40 THEN
               BEGIN
                   OUTIMAGE;
                   I := 0;
               END;
               COMMENT
               !  nr = 10*(r-n*t)
               !  n  = ((10*(3*q+r))//t)-10*n
               !  q  *= 10
               !  r  = nr
               ;
               NR :- TMUL("10",TSUB(R,TMUL(N,T)));
               N  :- TSUB(TDIV(TMUL("10",TADD(TMUL("3",Q),R)),T),TMUL("10",N));
               Q  :- TMUL("10",Q);
               R  :- NR;
           END
           ELSE
           BEGIN
               TEXT NR, NN;
               COMMENT
               !  nr = (2*q+r)*l
               !  nn = (q*(7*k)+2+(r*l))//(t*l)
               !  q  *= k
               !  t  *= l
               !  l  += 2
               !  k += 1
               !  n  = nn
               !  r  = nr
               ;
               NR :- TMUL(TADD(TMUL("2",Q),R),L);
               NN :- TDIV(TADD(TADD(TMUL(Q,TMUL("7",K)),"2"),TMUL(R,L)),TMUL(T,L));
               Q  :- TMUL(Q,K);
               T  :- TMUL(T,L);
               L  :- TADD(L,"2");
               K  :- TADD(K,"1");
               N  :- NN;
               R  :- NR;
           END;
        END;
    END CALCPI;

    CALCPI;
END.
```

Output:

```txt
3141592653589793238462643383279502884197
1693993751058209749445923078164062862089
9862803482534211706798214808651328230664
7093844609550582231725359408128481117450
2841027019385211055596446229489549303819
6442881097566593344612847564823378678316
5271201909145648566923460348610454326648
2133936072602491412737245870066063155881
7488152092096282925409171536436789259036
0011330530548820466521384146951941511609
4330572703657595919530921861173819326117
9310511854807446237996274956735188575272
4891227938183011949129833673362440656643
0860213949463952247371907021798609437027
7053921717629317675238467481846766940513
2000568127145263560827785771342757789609
...
```



## Tcl

Based on the reference in the [[#D|D]] code.
```tcl
package require Tcl 8.6

# http://www.cut-the-knot.org/Curriculum/Algorithms/SpigotForPi.shtml
# http://www.mathpropress.com/stan/bibliography/spigot.pdf
proc piDigitsBySpigot n {
    yield [info coroutine]
    set A [lrepeat [expr {int(floor(10*$n/3.)+1)}] 2]
    set Alen [llength $A]
    set predigits {}
    while 1 {
	set carry 0
	for {set i $Alen} {[incr i -1] > 0} {} {
	    lset A $i [expr {
		[set val [expr {[lindex $A $i] * 10 + $carry}]]
		% [set modulo [expr {2*$i + 1}]]
	    }]
	    set carry [expr {$val / $modulo * $i}]
	}
	lset A 0 [expr {[set val [expr {[lindex $A 0]*10 + $carry}]] % 10}]
	set predigit [expr {$val / 10}]
	if {$predigit < 9} {
	    foreach p $predigits {yield $p}
	    set predigits [list $predigit]
	} elseif {$predigit == 9} {
	    lappend predigits $predigit
	} else {
	    foreach p $predigits {yield [incr p]}
	    set predigits [list 0]
	}
    }
}
```

The pi digit generation requires picking a limit to the number of digits; the bigger the limit, the more digits can be ''safely'' computed. A value of 10k yields values relatively rapidly.

```tcl
coroutine piDigit piDigitsBySpigot 10000
fconfigure stdout -buffering none
while 1 {
    puts -nonewline [piDigit]
}
```



## TypeScript


```javascript
type AnyWriteableObject={write:((textToOutput:string)=>any)};

function calcPi(pipe:AnyWriteableObject) {
    let q = 1n, r=0n, t=1n, k=1n, n=3n, l=3n;
    while (true) {
        if (q * 4n + r - t < n* t) {
            pipe.write(n.toString());
            let nr = (r - n * t) * 10n;
            n  = (q * 3n + r) * 10n / t - n * 10n ;
            q  = q * 10n;
            r  = nr;
        } else {
            let nr = (q * 2n + r) * l;
            let nn = (q * k * 7n + 2n + r * l) / (t * l);
            q = q * k;
            t = t * l;
            l = l + 2n;
            k = k + 1n;
            n  = nn;
            r  = nr;
        }
    }
}

calcPi(process.stdout);
```


'''Notes:'''

1. Typescript has ''bigint'' support https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-2.html#bigint Literals are write with a ''n'' sufix: ''10n''

2. Pi function receives any object that has a ''write'' function. Using node.js we can pass to it ''process.stdout''


###  Async version



```javascript
type AnyWriteableObject = {write:((textToOutput:string)=>Promise<any>)};

async function calcPi<T extends AnyWriteableObject>(pipe:T) {
    let q = 1n, r=0n, t=1n, k=1n, n=3n, l=3n;
    while (true) {
        if (q * 4n + r - t < n* t) {
            await pipe.write(n.toString());
            let nr = (r - n * t) * 10n;
            n  = (q * 3n + r) * 10n / t - n * 10n ;
            q  = q * 10n;
            r  = nr;
        } else {
            let nr = (q * 2n + r) * l;
            let nn = (q * k * 7n + 2n + r * l) / (t * l);
            q = q * k;
            t = t * l;
            l = l + 2n;
            k = k + 1n;
            n  = nn;
            r  = nr;
        }
    }
}

setInterval(function(){
    console.log(); // put a new line every second
},1000);

var x = calcPi({
    write: async function(phrase:string){
        return new Promise(function(resolve){
            setTimeout(function(){
                process.stdout.write(phrase);
                resolve();
            },1);
        });
    }
});

console.log('.'); //start!
```


Here the calculation does not continue if the consumer does not consume the character.


## Visual Basic

```vb
Option Explicit

Sub Main()
Const VECSIZE As Long = 3350
Const BUFSIZE As Long = 201
Dim buffer(1 To BUFSIZE) As Long
Dim vect(1 To VECSIZE) As Long
Dim more As Long, karray As Long, num As Long, k As Long, l As Long, n As Long

  For n = 1 To VECSIZE
    vect(n) = 2
  Next n
  For n = 1 To BUFSIZE
    karray = 0
    For l = VECSIZE To 1 Step -1
      num = 100000 * vect(l) + karray * l
      karray = num \ (2 * l - 1)
      vect(l) = num - karray * (2 * l - 1)
    Next l
    k = karray \ 100000
    buffer(n) = more + k
    more = karray - k * 100000
  Next n
  Debug.Print CStr(buffer(1));
  Debug.Print "."
  l = 0
  For n = 2 To BUFSIZE
    Debug.Print Format$(buffer(n), "00000");
    l = l + 1
    If l = 10 Then
      l = 0
      Debug.Print 'line feed
    End If
  Next n
End Sub
```

```txt
3.
14159265358979323846264338327950288419716939937510
58209749445923078164062862089986280348253421170679
82148086513282306647093844609550582231725359408128
48111745028410270193852110555964462294895493038196
44288109756659334461284756482337867831652712019091
45648566923460348610454326648213393607260249141273
72458700660631558817488152092096282925409171536436
78925903600113305305488204665213841469519415116094
33057270365759591953092186117381932611793105118548
07446237996274956735188575272489122793818301194912
98336733624406566430860213949463952247371907021798
60943702770539217176293176752384674818467669405132
00056812714526356082778577134275778960917363717872
14684409012249534301465495853710507922796892589235
42019956112129021960864034418159813629774771309960
51870721134999999837297804995105973173281609631859
50244594553469083026425223082533446850352619311881
71010003137838752886587533208381420617177669147303
59825349042875546873115956286388235378759375195778
18577805321712268066130019278766111959092164201989
```



## Visual Basic .NET

Don't forget to use the "'''Project'''" tab, "'''Add Reference...'''" for '''''System.Numerics''''' (in case you get compiler errors in the Visual Studio IDE)

```vbnet
Imports System
Imports System.Numerics

Public Module Module1
	Public Sub Main()
        Dim two, three, four, seven, ten, k, q, t, l, n, r, nn, nr As BigInteger,
            first As Boolean = True
        two = New BigInteger(2) : three = New BigInteger(3) : four = two + two
        seven = three + four : ten = three + seven : k = BigInteger.One
        q = k : t = k : l = three : n = three : r = BigInteger.Zero
        While True
            If four * q + r - t < n * t Then
                Console.Write(n) : If first Then Console.Write(".") : first = False
                nr = ten * (r - n * t) : n = ten * (three * q + r) / t - ten * n
                q *= ten
            Else
                nr = (two * q + r) * l : nn = (q * seven * k + two + r * l) / (t * l)
                q *= k : t *= l : l += two : k += BigInteger.One : n = nn
            End If
            r = nr
        End While
    End Sub

End Module
```

<pre style="height:30ex;overflow:scroll">3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632788659361533818279682303019520353018529689957736225994138912497217752834791315155748572424541506959508295331168617278558890750983817546374649393192550604009277016711390098488240128583616035637076601047101819429555961989467678374494482553797747268471040475346462080466842590694912933136770289891521047521620569660240580381501935112533824300355876402474964732639141992726042699227967823547816360093417216412199245863150302861829745557067498385054945885869269956909272107975093029553211653449872027559602364806654991198818347977535663698074265425278625518184175746728909777727938000816470600161452491921732172147723501414419735685481613611573525521334757418494684385233239073941433345477624168625189835694855620992192221842725502542568876717904946016534668049886272327917860857843838279679766814541009538837863609506800642251252051173929848960841284886269456042419652850222106611863067442786220391949450471237137869609563643719172874677646575739624138908658326459958133904780275900994657640789512694683983525957098258226205224894077267194782684826014769909026401363944374553050682034962524517493996514314298091906592509372216964615157098583874105978859597729754989301617539284681382686838689427741559918559252459539594310499725246808459872736446958486538367362226260991246080512438843904512441365497627807977156914359977001296160894416948685558484063534220722258284886481584560285060168427394522674676788952521385225499546667278239864565961163548862305774564980355936345681743241125150760694794510965960940252288797108931456691368672287489405601015033086179286809208747609178249385890097149096759852613655497818931297848216829989487226588048575640142704775551323796414515237462343645428584447952658678210511413547357395231134271661021359695362314429524849371871101457654035902799344037420073105785390621983874478084784896833214457138687519435064302184531910484810053706146806749192781911979399520614196634287544406437451237181921799983910159195618146751426912397489409071864942319615679452080951465502252316038819301420937621378559566389377870830390697920773467221825625996615014215030680384477345492026054146659252014974428507325186660021324340881907104863317346496514539057962685610055081066587969981635747363840525714591028970641401109712062804390397595156771577004203378699360072305587631763594218731251471205329281918261861258673215791984148488291644706095752706957220917567116722910981690915280173506712748583222871835209353965725121083579151369882091444210067510334671103141267111369908658516398315019701651511685171437657618351556508849099898599823873455283316355076479185358932261854896321329330898570642046752590709154814165498594616371802709819943099244889575712828905923233260972997120844335732654893823911932597463667305836041428138830320382490375898524374417029132765618093773444030707469211201913020330380197621101100449293215160842444859637669838952286847831235526582131449576857262433441893039686426243410773226978028073189154411010446823252716201052652272111660396665573092547110557853763466820653109896526918620564769312570586356620185581007293606598764861179104533488503461136576867532494416680396265797877185560845529654126654085306143444318586769751456614068007002378776591344017127494704205622305389945613140711270004078547332699390814546646458807972708266830634328587856983052358089330657574067954571637752542021149557615814002501262285941302164715509792592309907965473761255176567513575178296664547791745011299614890304639947132962107340437518957359614589019389713111790429782856475032031986915140287080859904801094121472213179476477726224142548545403321571853061422881375850430633217518297986622371721591607716692547487389866549494501146540628433663937900397692656721463853067360965712091807638327166416274888800786925602902284721040317211860820419000422966171196377921337575114959501566049631862947265473642523081770367515906735023507283540567040386743513622224771589150495309844489333096340878076932599397805419341447377441842631298608099888687413260472156951623965864573021631598193195167353812974167729478672422924654366800980676928238280689964004824354037014163149658979409243237896907069779422362508221688957383798623001593776471651228935786015881617557829735233446042815126272037343146531977774160319906655418763979293344195215413418994854447345673831624993419131814809277771038638773431772075456545322077709212019051660962804909263601975988281613323166636528619326686336062735676303544776280350450777235547105859548702790814356240145171806246436267945612753181340783303362542327839449753824372058353114771199260638133467768796959703098339130771098704085913374641442822772634659470474587847787201927715280731767907707157213444730605700733492436931138350493163128404251219256517980694113528013147013047816437885185290928545201165839341965621349143415956258658655705526904965209858033850722426482939728584783163057777560688876446248246857926039535277348030480290058760758251047470916439613626760449256274204208320856611906254543372131535958450687724602901618766795240616342522577195429162991930645537799140373404328752628889639958794757291746426357455254079091451357111369410911939325191076020825202618798531887705842972591677813149699009019211697173727847684726860849003377024242916513005005168323364350389517029893922334517220138128069650117844087451960121228599371623130171144484640903890644954440061986907548516026327505298349187407866808818338510228334508504860825039302133219715518430635455007668282949304137765527939751754613953984683393638304746119966538581538420568533862186725233402830871123282789212507712629463229563989898935821167456270102183564622013496715188190973038119800497340723961036854066431939509790190699639552453005450580685501956730229219139339185680344903982059551002263535361920419947455385938102343955449597783779023742161727111

```

===Quicker, unverified algo===
There seems to be another algorithm in the original reference article (see the [http://www.rosettacode.org/wiki/Pi#Ada Ada] entry), which produces output a bit faster.  However, the math behind the algorithm has not been completely proven.  It's faster because it doesn't calculate whether each digit is accumulated properly before squirting it out.  When using (slow) arbitrary precision libraries, this avoids a lot of computation time.

```vbnet
Imports System, System.Numerics, System.Text

Module Program

    Sub RunPiF(ByVal msg As String)
        If msg.Length > 0 Then Console.WriteLine(msg)
        Dim first As Boolean = True, stp As BigInteger = 360,
            lim As BigInteger = stp, res As StringBuilder = New StringBuilder(),
            rc As Integer = -1, u, j, k As BigInteger, q As BigInteger = 1,
            r As BigInteger = 180, t As BigInteger = 60, i As BigInteger = 2,
            y As Byte, et As TimeSpan, st As DateTime = DateTime.Now

        While True
            While i < lim
                j = i << 1 : k = j + i : u = 3 * (k + 1) * (k + 2)
                y = CByte(((q * (9 * k - 12) + 5 * r) / (5 * t)))
                res.Append(y)
                r = (q * (k + j - 2) + r - y * t) * u * 10
                t *= u : q = 10 * q * (j - 1) * i : i += 1
            End While
            If first Then res.Insert(1, "."c) : first = False
            Console.Write(res.ToString())
            rc += res.Length : res.Clear() : lim += stp
            If Console.KeyAvailable Then Exit While
        End While
        et = DateTime.Now - st : Console.ReadKey()
        Console.Write(res.ToString()) : rc += res.Length
        Console.WriteLine(vbLf & "Produced {0} digits in {1:n4} seconds.", rc, et.TotalSeconds)
    End Sub

    Sub Main(args As String())
        RunPiF("Press a key to exit...")
        If Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}The First several thousand digits verified the same as the conventional spigot algorithm, haven't detected any differences yet.
<pre style="height:30ex;overflow:scroll">Press a key to exit...
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198938095257201065485863278865936153381827968230301952035301852968995773622599413891249721775283479131515574857242454150695950829533116861727855889075098381754637464939319255060400927701671139009848824012858361603563707660104710181942955596198946767837449448255379774726847104047534646208046684259069491293313677028989152104752162056966024058038150193511253382430035587640247496473263914199272604269922796782354781636009341721641219924586315030286182974555706749838505494588586926995690927210797509302955321165344987202755960236480665499119881834797753566369807426542527862551818417574672890977772793800081647060016145249192173217214772350141441973568548161361157352552133475741849468438523323907394143334547762416862518983569485562099219222184272550254256887671790494601653466804988627232791786085784383827967976681454100953883786360950680064225125205117392984896084128488626945604241965285022210661186306744278622039194945047123713786960956364371917287467764657573962413890865832645995813390478027590099465764078951269468398352595709825822620522489407726719478268482601476990902640136394437455305068203496252451749399651431429809190659250937221696461515709858387410597885959772975498930161753928468138268683868942774155991855925245953959431049972524680845987273644695848653836736222626099124608051243884390451244136549762780797715691435997700129616089441694868555848406353422072225828488648158456028506016842739452267467678895252138522549954666727823986456596116354886230577456498035593634568174324112515076069479451096596094025228879710893145669136867228748940560101503308617928680920874760917824938589009714909675985261365549781893129784821682998948722658804857564014270477555132379641451523746234364542858444795265867821051141354735739523113427166102135969536231442952484937187110145765403590279934403742007310578539062198387447808478489683321445713868751943506430218453191048481005370614680674919278191197939952061419663428754440643745123718192179998391015919561814675142691239748940907186494231961567945208095146550225231603881930142093762137855956638937787083039069792077346722182562599661501421503068038447734549202605414665925201497442850732518666002132434088190710486331734649651453905796268561005508106658796998163574736384052571459102897064140110971206280439039759515677157700420337869936007230558763176359421873125147120532928191826186125867321579198414848829164470609575270695722091756711672291098169091528017350671274858322287183520935396572512108357915136988209144421006751033467110314126711136990865851639831501970165151168517143765761835155650884909989859982387345528331635507647918535893226185489632132933089857064204675259070915481416549859461637180270981994309924488957571282890592323326097299712084433573265489382391193259746366730583604142813883032038249037589852437441702913276561809377344403070746921120191302033038019762110110044929321516084244485963766983895228684783123552658213144957685726243344189303968642624341077322697802807318915441101044682325271620105265227211166039666557309254711055785376346682065310989652691862056476931257058635662018558100729360659876486117910453348850346113657686753249441668039626579787718556084552965412665408530614344431858676975145661406800700237877659134401712749470420562230538994561314071127000407854733269939081454664645880797270826683063432858785698305235808933065757406795457163775254202114955761581400250126228594130216471550979259230990796547376125517656751357517829666454779174501129961489030463994713296210734043751895735961458901938971311179042978285647503203198691514028708085990480109412147221317947647772622414254854540332157185306142288137585043063321751829798662237172159160771669254748738986654949450114654062843366393790039769265672146385306736096571209180763832716641627488880078692560290228472104031721186082041900042296617119637792133757511495950156604963186294726547364252308177036751590673502350728354056704038674351362222477158915049530984448933309634087807693259939780541934144737744184263129860809988868741326047215695162396586457302163159819319516735
Produced 5038 digits in 0.3391 seconds.
```



## zkl

Uses the GMP big int library.
Same algorithm as many of the others on this page. Uses in place ops to cut down on big int generation (eg add vs +).  Unless GC is given some hints, it will use up 16 gig quickly as it outruns the garbage collector.

```zkl
var [const] BN=Import("zklBigNum"),
   one=BN(1), two=BN(2), three=BN(3), four=BN(4), seven=BN(7), ten=BN(10);

fcn calcPiDigits{
   reg q=BN(1), r=BN(0), t=BN(1), k=BN(1), n=BN(3), l=BN(3);
   first:=True; N:=0;
   while(True){ if((N+=1)==1000){ GarbageMan.collect(); N=0; } // take a deep breath ...
      if(four*q + r - t < n*t){
         n.print(); if(first){ print("."); first=False; }
	 nr:=(r - n*t).mul(ten);	// 10 * (r - n * t);
	 n=(three*q).add(r).mul(ten)	// ((10*(3*q + r))/t) - 10*n;
	   .div(t).sub(ten*n);
	 q.mul(ten);			// q *= 10;
	 r=nr;
      }else{
	 nr:=(two*q).add(r).mul(l);	// (2*q + r)*l;
	 nn:=(q*seven).mul(k).add(two)	// (q*(7*k + 2) + r*l)/(t*l);
	     .add(r*l).div(t*l);
	 q.mul(k);   t.mul(l);		// q*=k; t*=l;
	 l.add(two); k.add(one);	// l+=2; k++;
	 n=nn; r=nr;
      }
   }
}();
```

Runs until ^C hit, the first 1000 digits match the D output.
```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745
```


