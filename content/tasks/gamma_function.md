+++
title = "Gamma function"
description = ""
date = 2019-09-02T18:53:36Z
aliases = []
[extra]
id = 3996
[taxonomies]
categories = ["Mathematical operations", "Mathematics", "task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "ansi_standard_basic",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "crystal",
  "csharp",
  "d",
  "elixir",
  "f_sharp",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "limbo",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "maxima",
  "nim",
  "ocaml",
  "octave",
  "oforth",
  "pari_gp",
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
  "ring",
  "rlab",
  "ruby",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "stata",
  "tcl",
  "visual_foxpro",
  "yabasic",
  "zkl",
]
+++

## Task
Implement one algorithm (or more) to compute the [[wp:Gamma function|Gamma]] (<math>\Gamma</math>) function (in the real field only).

If your language has the function as built-in or you know a library which has it, compare your implementation's results with the results of the built-in/library function.

The Gamma function can be defined as:

:::::: <big><big> <math>\Gamma(x) = \displaystyle\int_0^\infty t^{x-1}e^{-t} dt</math></big></big>

This suggests a straightforward (but inefficient) way of computing the <math>\Gamma</math> through numerical integration.


Better suggested methods:
* [[wp:Lanczos approximation|Lanczos approximation]]
* [[wp:Stirling's approximation|Stirling's approximation]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
GAMMAT   CSECT
         USING GAMMAT,R13
SAVEAR   B     STM-SAVEAR(R15)
         DC    17F'0'
         DC    CL8'GAMMAT'
STM      STM   R14,R12,12(R13)
         ST    R13,4(R15)
         ST    R15,8(R13)
         LR    R13,R15
*        ----  CODE
         LE    F4,=E'0'
         LH    R2,NI
LOOPI    EQU   *
         AE    F4,=E'1'         xi=xi+1
         LER   F0,F4
         DE    F0,=E'10'        x=xi/10
         STE   F0,X
         LE    F6,X
         SE    F6,=E'1'         xx=x-1.0
         LH    R4,NT
         BCTR  R4,0
         SLA   R4,2
         LE    F0,T(R4)
         STE   F0,SUM           sum=t(nt)
         LH    R3,NT
         BCTR  R3,0
         SH    R4,=H'4'
LOOPJ    CH    R3,=H'1'         for j=nt-1 downto 1
         BL    ENDLOOPJ
         LE    F0,SUM
         MER   F0,F6            sum*xx
         LE    F2,T(R4)         t(j)
         AER   F0,F2
         STE   F0,SUM           sum=sum*xx+t(j)
         BCTR  R3,0
         SH    R4,=H'4'
         B     LOOPJ
ENDLOOPJ EQU   *
         LE    F0,=E'1'
         DE    F0,SUM
         STE   F0,GAMMA         gamma=1/sum
         LE    F0,X
         BAL   R14,CONVERT
         MVC   BUF(8),CONVERTM
         LE    F0,GAMMA
         BAL   R14,CONVERT
         MVC   BUF+9(13),CONVERTM
         WTO   MF=(E,WTOMSG)
         BCT   R2,LOOPI
*        ----  END CODE
         CNOP  0,4
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         XR    R15,R15
         BR    R14
*        ----  DATA
NI       DC    H'30'
NT       DC    AL2((TEND-T)/4)
T        DC    E'1.00000000000000000000'
         DC    E'0.57721566490153286061'
         DC    E'-0.65587807152025388108'
         DC    E'-0.04200263503409523553'
         DC    E'0.16653861138229148950'
         DC    E'-0.04219773455554433675'
         DC    E'-0.00962197152787697356'
         DC    E'0.00721894324666309954'
         DC    E'-0.00116516759185906511'
         DC    E'-0.00021524167411495097'
         DC    E'0.00012805028238811619'
         DC    E'-0.00002013485478078824'
         DC    E'-0.00000125049348214267'
         DC    E'0.00000113302723198170'
         DC    E'-0.00000020563384169776'
         DC    E'0.00000000611609510448'
         DC    E'0.00000000500200764447'
         DC    E'-0.00000000118127457049'
         DC    E'0.00000000010434267117'
         DC    E'0.00000000000778226344'
         DC    E'-0.00000000000369680562'
         DC    E'0.00000000000051003703'
         DC    E'-0.00000000000002058326'
         DC    E'-0.00000000000000534812'
         DC    E'0.00000000000000122678'
         DC    E'-0.00000000000000011813'
         DC    E'0.00000000000000000119'
         DC    E'0.00000000000000000141'
         DC    E'-0.00000000000000000023'
         DC    E'0.00000000000000000002'
TEND     DS    0E
X        DS    E
SUM      DS    E
GAMMA    DS    E
WTOMSG   DS    0F
         DC    AL2(L'BUF),XL2'0000'
BUF      DC    CL80' '
*        Subroutine             Convertion Float->Display
CONVERT  CNOP  0,4
         ME    F0,CONVERTC
         STE   F0,CONVERTF
         MVI   CONVERTS,X'00'
         L     R9,CONVERTF
         CH    R9,=H'0'
         BZ    CONVERT7
         BNL   CONVERT1         is negative?
         MVI   CONVERTS,X'80'
         N     R9,=X'7FFFFFFF'  sign bit
CONVERT1 LR    R8,R9
         N     R8,=X'00FFFFFF'
         BNZ   CONVERT2
         SR    R9,R9
         B     CONVERT7
CONVERT2 LR    R8,R9
         N     R8,=X'FF000000'  characteristic
         SRL   R8,24
         CH    R8,=H'64'
         BH    CONVERT3
         SR    R9,R9
         B     CONVERT7
CONVERT3 CH    R8,=H'72'        2**32
         BNH   CONVERT4
         L     R9,=X'7FFFFFFF'  biggest
         B     CONVERT6
CONVERT4 SR    R8,R8
         SLDL  R8,8
         CH    R8,=H'72'
         BL    CONVERT5
         CH    R9,=H'0'
         BP    CONVERT5
         L     R9,=X'7FFFFFFF'
         B     CONVERT6
CONVERT5 SH    R8,=H'72'
         LCR   R8,R8
         SLL   R8,2
         SRL   R9,0(R8)
CONVERT6 SR    R8,R8
         IC    R8,CONVERTS
         CH    R8,=H'0'         sign bit set?
         BZ    CONVERT7
         LCR   R9,R9
CONVERT7 ST    R9,CONVERTB
         CVD   R9,CONVERTP
         MVC   CONVERTD,=X'402020202120202020202020'
         ED    CONVERTD,CONVERTP+2
         MVC   CONVERTM(6),CONVERTD
         MVI   CONVERTM+6,C'.'
         MVC   CONVERTM+7(6),CONVERTD+6
         BR    R14
*
CONVERTC DC    E'1E6'           X'45F42400'
CONVERTF DS    F
CONVERTB DS    F
CONVERTS DS    X
CONVERTM DS    CL13
CONVERTD DS    CL12
CONVERTP DS    PL8
*
         EQUREGS
         EQUREGS REGS=FPR
         END   GAMMAT
```

{{out}}
<pre style="height:20ex">
     0.1      9.513504
     0.2      4.590844
     0.3      2.991569
     0.4      2.218160
     0.5      1.772453
     0.6      1.489192
     0.7      1.298056
     0.8      1.164229
     0.9      1.068628
     1.0      1.000000
     1.1      0.951350
     1.2      0.918168
     1.3      0.897470
     1.4      0.887263
     1.5      0.886227
     1.6      0.893515
     1.7      0.908638
     1.8      0.931383
     1.9      0.961766
     2.0      1.000000
     2.1      1.046486
     2.2      1.101803
     2.3      1.166712
     2.4      1.242169
     2.5      1.329341
     2.6      1.429626
     2.7      1.544686
     2.8      1.676492
     2.9      1.827354
     3.0      1.999999


```



## Ada

The implementation uses [[wp:Taylor series|Taylor series]] coefficients of <span style="font-family:serif;">&Gamma;(x+1)<sup>-1</sup>, |x| &lt; &infin;</span>.
The coefficients are taken from ''Mathematical functions and their approximations''
by [[wp:Yudell Luke|Yudell L. Luke]].

```ada
function Gamma (X : Long_Float) return Long_Float is
   A : constant array (0..29) of Long_Float :=
       (  1.00000_00000_00000_00000,
          0.57721_56649_01532_86061,
         -0.65587_80715_20253_88108,
         -0.04200_26350_34095_23553,
          0.16653_86113_82291_48950,
         -0.04219_77345_55544_33675,
         -0.00962_19715_27876_97356,
          0.00721_89432_46663_09954,
         -0.00116_51675_91859_06511,
         -0.00021_52416_74114_95097,
          0.00012_80502_82388_11619,
         -0.00002_01348_54780_78824,
         -0.00000_12504_93482_14267,
          0.00000_11330_27231_98170,
         -0.00000_02056_33841_69776,
          0.00000_00061_16095_10448,
          0.00000_00050_02007_64447,
         -0.00000_00011_81274_57049,
          0.00000_00001_04342_67117,
          0.00000_00000_07782_26344,
         -0.00000_00000_03696_80562,
          0.00000_00000_00510_03703,
         -0.00000_00000_00020_58326,
         -0.00000_00000_00005_34812,
          0.00000_00000_00001_22678,
         -0.00000_00000_00000_11813,
          0.00000_00000_00000_00119,
          0.00000_00000_00000_00141,
         -0.00000_00000_00000_00023,
          0.00000_00000_00000_00002
       );
   Y   : constant Long_Float := X - 1.0;
   Sum : Long_Float := A (A'Last);
begin
   for N in reverse A'First..A'Last - 1 loop
      Sum := Sum * Y + A (N);
   end loop;
   return 1.0 / Sum;
end Gamma;
```

Test program:

```ada
with Ada.Text_IO;  use Ada.Text_IO;
with Gamma;

procedure Test_Gamma is
begin
   for I in 1..10 loop
      Put_Line (Long_Float'Image (Gamma (Long_Float (I) / 3.0)));
   end loop;
end Test_Gamma;
```

{{Out}}

```txt

 2.67893853470775E+00
 1.35411793942640E+00
 1.00000000000000E+00
 8.92979511569249E-01
 9.02745292950934E-01
 1.00000000000000E+00
 1.19063934875900E+00
 1.50457548825154E+00
 1.99999999999397E+00
 2.77815847933858E+00

```



## ALGOL 68

{{trans|C}} - Stirling & Spouge methods.

{{trans|python}} - Lanczos method.

<!-- {{does not work with|ALGOL 68|Standard - the gamma function is an extension}} -->
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - formatted transput is missing, gamma is an extension, and various LONG vs INT operators missing}} -->

```algol68
# Coefficients used by the GNU Scientific Library #
[]LONG REAL p = ( LONG    0.99999 99999 99809 93,
                  LONG  676.52036 81218 851,
                 -LONG 1259.13921 67224 028,
                  LONG  771.32342 87776 5313,
                 -LONG  176.61502 91621 4059,
                  LONG   12.50734 32786 86905,
                 -LONG    0.13857 10952 65720 12,
                  LONG    9.98436 95780 19571 6e-6,
                  LONG    1.50563 27351 49311 6e-7);

PROC lanczos gamma = (LONG REAL in z)LONG REAL: (
  # Reflection formula #
  LONG REAL z := in z;
  IF z < LONG 0.5 THEN
    long pi / (long sin(long pi*z)*lanczos gamma(1-z))
  ELSE
    z -:= 1;
    LONG REAL x := p[1];
    FOR i TO UPB p - 1 DO x +:= p[i+1]/(z+i) OD;
    LONG REAL t = z + UPB p - LONG 1.5;
    long sqrt(2*long pi) * t**(z+LONG 0.5) * long exp(-t) * x
  FI
);

PROC taylor gamma = (LONG REAL x)LONG REAL:
BEGIN # good for values between 0 and 1 #
    []LONG REAL a =
        ( LONG 1.00000 00000 00000 00000,
          LONG 0.57721 56649 01532 86061,
         -LONG 0.65587 80715 20253 88108,
         -LONG 0.04200 26350 34095 23553,
          LONG 0.16653 86113 82291 48950,
         -LONG 0.04219 77345 55544 33675,
         -LONG 0.00962 19715 27876 97356,
          LONG 0.00721 89432 46663 09954,
         -LONG 0.00116 51675 91859 06511,
         -LONG 0.00021 52416 74114 95097,
          LONG 0.00012 80502 82388 11619,
         -LONG 0.00002 01348 54780 78824,
         -LONG 0.00000 12504 93482 14267,
          LONG 0.00000 11330 27231 98170,
         -LONG 0.00000 02056 33841 69776,
          LONG 0.00000 00061 16095 10448,
          LONG 0.00000 00050 02007 64447,
         -LONG 0.00000 00011 81274 57049,
          LONG 0.00000 00001 04342 67117,
          LONG 0.00000 00000 07782 26344,
         -LONG 0.00000 00000 03696 80562,
          LONG 0.00000 00000 00510 03703,
         -LONG 0.00000 00000 00020 58326,
         -LONG 0.00000 00000 00005 34812,
          LONG 0.00000 00000 00001 22678,
         -LONG 0.00000 00000 00000 11813,
          LONG 0.00000 00000 00000 00119,
          LONG 0.00000 00000 00000 00141,
         -LONG 0.00000 00000 00000 00023,
          LONG 0.00000 00000 00000 00002
        );
    LONG REAL y = x - 1;
    LONG REAL sum := a [UPB a];
    FOR n FROM UPB a - 1 DOWNTO LWB a DO
       sum := sum * y + a [n]
    OD;
    1/sum
END # taylor gamma #;

LONG REAL long e = long exp(1);

PROC sterling gamma = (LONG REAL n)LONG REAL:
( # improves for values much greater then 1 #
  long sqrt(2*long pi/n)*(n/long e)**n
);

PROC factorial = (LONG INT n)LONG REAL:
(
  IF n=0 OR n=1 THEN 1
  ELIF n=2 THEN 2
  ELSE n*factorial(n-1) FI
);

REF[]LONG REAL fm := NIL;

PROC sponge gamma = (LONG REAL x)LONG REAL:
(
  INT a = 12; # alter to get required precision #
  REF []LONG REAL fm := NIL;
  LONG REAL res;

  IF fm :=: REF[]LONG REAL(NIL) THEN
    fm := HEAP[0:a-1]LONG REAL;
    fm[0] := long sqrt(LONG 2*long pi);
    FOR k TO a-1 DO
      fm[k] := (((k-1) MOD 2=0) | 1 | -1) * long exp(a-k) *
	(a-k) **(k-LONG 0.5) / factorial(k-1)
    OD
  FI;
  res := fm[0];
  FOR k TO a-1 DO
    res +:= fm[k] / ( x + k )
  OD;
  res *:= long exp(-(x+a)) * (x+a)**(x + LONG 0.5);
  res/x
);

FORMAT real fmt = $g(-real width, real width - 2)$;
FORMAT long real fmt16 = $g(-17, 17 - 2)$; # accurate to about 16 decimal places #

[]STRING methods = ("Genie", "Lanczos", "Sponge", "Taylor","Stirling");

printf(($11xg12xg12xg13xg13xgl$,methods));

FORMAT sample fmt = $"gamma("g(-3,1)")="f(real fmt)n(UPB methods-1)(", "f(long real fmt16))l$;
FORMAT sqr sample fmt = $"gamma("g(-3,1)")**2="f(real fmt)n(UPB methods-1)(", "f(long real fmt16))l$;
FORMAT sample exp fmt = $"gamma("g(-3)")="g(-15,11,0)n(UPB methods-1)(","g(-18,14,0))l$;

PROC sample = (LONG REAL x)[]LONG REAL:
  (gamma(SHORTEN x), lanczos gamma(x), sponge gamma(x), taylor gamma(x), sterling gamma(x));

FOR i FROM 1 TO 20 DO
  LONG REAL x = i / LONG 10;
  printf((sample fmt, x, sample(x)));
  IF i = 5 THEN # insert special case of a half #
    printf((sqr sample fmt,
            x, gamma(SHORTEN x)**2,  lanczos gamma(x)**2, sponge gamma(x)**2,
            taylor gamma(x)**2, sterling gamma(x)**2))
  FI
OD;
FOR x FROM 10 BY 10 TO 70 DO
  printf((sample exp fmt, x, sample(x)))
OD
```

{{out}}

```txt

           Genie            Lanczos            Sponge             Taylor             Stirling
gamma(0.1)=9.5135076986687, 9.513507698668730, 9.513507698668731, 9.513509522249043, 5.697187148977169
gamma(0.2)=4.5908437119988, 4.590843711998802, 4.590843711998803, 4.590843743037192, 3.325998424022393
gamma(0.3)=2.9915689876876, 2.991568987687590, 2.991568987687590, 2.991568988322729, 2.362530036269620
gamma(0.4)=2.2181595437577, 2.218159543757688, 2.218159543757688, 2.218159543764845, 1.841476335936235
gamma(0.5)=1.7724538509055, 1.772453850905517, 1.772453850905516, 1.772453850905353, 1.520346901066281
gamma(0.5)**2=3.1415926535898, 3.141592653589795, 3.141592653589793, 3.141592653589216, 2.311454699581843
gamma(0.6)=1.4891922488128, 1.489192248812817, 1.489192248812817, 1.489192248812758, 1.307158857448356
gamma(0.7)=1.2980553326476, 1.298055332647558, 1.298055332647558, 1.298055332647558, 1.159053292113920
gamma(0.8)=1.1642297137253, 1.164229713725304, 1.164229713725303, 1.164229713725303, 1.053370968425609
gamma(0.9)=1.0686287021193, 1.068628702119320, 1.068628702119319, 1.068628702119319, 0.977061507877695
gamma(1.0)=1.0000000000000, 1.000000000000000, 1.000000000000000, 1.000000000000000, 0.922137008895789
gamma(1.1)=0.9513507698669, 0.951350769866873, 0.951350769866873, 0.951350769866873, 0.883489953168704
gamma(1.2)=0.9181687423998, 0.918168742399761, 0.918168742399760, 0.918168742399761, 0.857755335396591
gamma(1.3)=0.8974706963063, 0.897470696306277, 0.897470696306277, 0.897470696306277, 0.842678259448392
gamma(1.4)=0.8872638175031, 0.887263817503076, 0.887263817503075, 0.887263817503064, 0.836744548637082
gamma(1.5)=0.8862269254528, 0.886226925452758, 0.886226925452758, 0.886226925452919, 0.838956552526496
gamma(1.6)=0.8935153492877, 0.893515349287691, 0.893515349287690, 0.893515349288799, 0.848693242152574
gamma(1.7)=0.9086387328533, 0.908638732853291, 0.908638732853290, 0.908638732822421, 0.865621471793884
gamma(1.8)=0.9313837709802, 0.931383770980243, 0.931383770980242, 0.931383769950169, 0.889639635287994
gamma(1.9)=0.9617658319074, 0.961765831907388, 0.961765831907387, 0.961765815012982, 0.920842721894229
gamma(2.0)=1.0000000000000, 1.000000000000000, 0.999999999999999, 1.000000010045742, 0.959502175744492
gamma( 10)= 3.6288000000e5, 3.6288000000000e5, 3.6288000000000e5, 4.051218760300e-7, 3.5986956187410e5
gamma( 20)= 1.216451004e17, 1.216451004088e17, 1.216451004088e17, 1.07701514977e-18, 1.211393423381e17
gamma( 30)= 8.841761994e30, 8.841761993740e30, 8.841761993739e30, 7.98891286318e-23, 8.817236530765e30
gamma( 40)= 2.039788208e46, 2.039788208120e46, 2.039788208120e46, 6.97946184592e-25, 2.035543161237e46
gamma( 50)= 6.082818640e62, 6.082818640343e62, 6.082818640342e62, 1.81016585713e-26, 6.072689187876e62
gamma( 60)= 1.386831185e80, 1.386831185457e80, 1.386831185457e80, 9.27306839649e-28, 1.384906385829e80
gamma( 70)= 1.711224524e98, 1.711224524281e98, 1.711224524281e98, 7.57303907062e-29, 1.709188578191e98

```


## ANSI Standard BASIC


{{trans|BBC Basic}} - Lanczos method.

```ANSI Standard BASIC
100 DECLARE EXTERNAL FUNCTION FNlngamma
110
120 DEF FNgamma(z) = EXP(FNlngamma(z))
130
140 FOR x = 0.1 TO 2.05 STEP 0.1
150    PRINT USING$("#.#",x), USING$("##.############", FNgamma(x))
160 NEXT x
170 END
180
190 EXTERNAL FUNCTION FNlngamma(z)
200 DIM lz(0 TO 6)
210 RESTORE
220 MAT READ lz
230 DATA 1.000000000190015, 76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.0012086509738662, -0.000005395239385
240 IF z < 0.5 THEN
250    LET FNlngamma = LOG(PI / SIN(PI * z)) - FNlngamma(1.0 - z)
260    EXIT FUNCTION
270 END IF
280 LET z = z - 1.0
290 LET b = z + 5.5
300 LET a = lz(0)
310 FOR i = 1 TO 6
320    LET a  = a + lz(i) / (z + i)
330 NEXT i
340 LET FNlngamma = (LOG(SQR(2*PI)) + LOG(a) - b) + LOG(b) * (z+0.5)
350 END FUNCTION
```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}
Source: [http://www.autohotkey.com/forum/topic44657.html AutoHotkey forum] by Laszlo

```autohotkey
/*
Here is the upper incomplete Gamma function. Omitting or setting
the second parameter to 0 we get the (complete) Gamma function.
The code is based on: "Computation of Special Functions" Zhang and Jin,
John Wiley and Sons, 1996
*/

SetFormat FloatFast, 0.9e

Loop 10
   MsgBox % GAMMA(A_Index/3) "`n" GAMMA(A_Index*10)

GAMMA(a,x=0) {  ; upper incomplete gamma: Integral(t**(a-1)*e**-t, t = x..inf)
   If (a > 171 || x < 0)
      Return 2.e308   ; overflow

   xam := x > 0 ? -x+a*ln(x) : 0
   If (xam > 700)
      Return 2.e308   ; overflow

   If (x > 1+a) {     ; no need for gamma(a)
      t0 := 0, k := 60
      Loop 60
          t0 := (k-a)/(1+k/(x+t0)), --k
      Return exp(xam) / (x+t0)
   }

   r := 1, ga := 1.0  ; compute ga = gamma(a) ...
   If (a = round(a))  ; if integer: factorial
      If (a > 0)
         Loop % a-1
            ga *= A_Index
      Else            ; negative integer
         ga := 1.7976931348623157e+308 ; Dmax
   Else {             ; not integer
      If (abs(a) > 1) {
         z := abs(a)
         m := floor(z)
         Loop %m%
             r *= (z-A_Index)
         z -= m
      }
      Else
         z := a

      gr := (((((((((((((((((((((((       0.14e-14
          *z - 0.54e-14)             *z - 0.206e-13)          *z + 0.51e-12)
          *z - 0.36968e-11)          *z + 0.77823e-11)        *z + 0.1043427e-9)
          *z - 0.11812746e-8)        *z + 0.50020075e-8)      *z + 0.6116095e-8)
          *z - 0.2056338417e-6)      *z + 0.1133027232e-5)    *z - 0.12504934821e-5)
          *z - 0.201348547807e-4)    *z + 0.1280502823882e-3) *z - 0.2152416741149e-3)
          *z - 0.11651675918591e-2)  *z + 0.7218943246663e-2) *z - 0.9621971527877e-2)
          *z - 0.421977345555443e-1) *z + 0.1665386113822915) *z - 0.420026350340952e-1)
          *z - 0.6558780715202538)   *z + 0.5772156649015329) *z + 1

      ga := 1.0/(gr*z) * r
      If (a < -1)
         ga := -3.1415926535897931/(a*ga*sin(3.1415926535897931*a))
   }

   If (x = 0)         ; complete gamma requested
      Return ga

   s := 1/a           ; here x <= 1+a
   r := s
   Loop 60 {
      r *= x/(a+A_Index)
      s += r
      If (abs(r/s) < 1.e-15)
         break
   }
   Return ga - exp(xam)*s
}

/*
The 10 results shown:
2.678938535e+000  1.354117939e+000  1.0               8.929795115e-001  9.027452930e-001
3.628800000e+005  1.216451004e+017  8.841761994e+030  2.039788208e+046  6.082818640e+062

1.000000000e+000  1.190639348e+000  1.504575489e+000  2.000000000e+000  2.778158479e+000
1.386831185e+080  1.711224524e+098  8.946182131e+116  1.650795516e+136  9.332621544e+155
*/
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses the Lanczos approximation.

```bbcbasic
      *FLOAT64
      INSTALL @lib$+"FNUSING"

      FOR x = 0.1 TO 2.05 STEP 0.1
        PRINT FNusing("#.#",x), FNusing("##.############", FNgamma(x))
      NEXT
      END

      DEF FNgamma(z) = EXP(FNlngamma(z))

      DEF FNlngamma(z)
      LOCAL a, b, i%, lz()
      DIM lz(6)
      lz() = 1.000000000190015, 76.18009172947146, -86.50532032941677, \
      \ 24.01409824083091, -1.231739572450155, 0.0012086509738662, -0.000005395239385
      IF z < 0.5 THEN = LN(PI / SIN(PI * z)) - FNlngamma(1.0 - z)
      z -= 1.0
      b = z + 5.5
      a = lz(0)
      FOR i% = 1 TO 6
        a += lz(i%) / (z + i%)
      NEXT
      = (LNSQR(2*PI) + LN(a) - b) + LN(b) * (z+0.5)
```

'''Output:'''

```txt

0.1        9.513507698670
0.2        4.590843712000
0.3        2.991568987689
0.4        2.218159543760
0.5        1.772453850902
0.6        1.489192248811
0.7        1.298055332647
0.8        1.164229713725
0.9        1.068628702119
1.0        1.000000000000
1.1        0.951350769867
1.2        0.918168742400
1.3        0.897470696306
1.4        0.887263817503
1.5        0.886226925453
1.6        0.893515349288
1.7        0.908638732853
1.8        0.931383770980
1.9        0.961765831907
2.0        1.000000000000

```



## C

{{libheader|GNU Scientific Library}}

This implements [[wp:Stirling's approximation|Stirling's approximation]] and [[wp:Spouge's approximation|Spouge's approximation]].


```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_sf_gamma.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* very simple approximation */
double st_gamma(double x)
{
  return sqrt(2.0*M_PI/x)*pow(x/M_E, x);
}

#define A 12
double sp_gamma(double z)
{
  const int a = A;
  static double c_space[A];
  static double *c = NULL;
  int k;
  double accm;

  if ( c == NULL ) {
    double k1_factrl = 1.0; /* (k - 1)!*(-1)^k with 0!==1*/
    c = c_space;
    c[0] = sqrt(2.0*M_PI);
    for(k=1; k < a; k++) {
      c[k] = exp(a-k) * pow(a-k, k-0.5) / k1_factrl;
	  k1_factrl *= -k;
    }
  }
  accm = c[0];
  for(k=1; k < a; k++) {
    accm += c[k] / ( z + k );
  }
  accm *= exp(-(z+a)) * pow(z+a, z+0.5); /* Gamma(z+1) */
  return accm/z;
}

int main()
{
  double x;


  printf("%15s%15s%15s%15s\n", "Stirling", "Spouge", "GSL", "libm");
  for(x=1.0; x <= 10.0; x+=1.0) {
    printf("%15.8lf%15.8lf%15.8lf%15.8lf\n", st_gamma(x/3.0), sp_gamma(x/3.0),
	   gsl_sf_gamma(x/3.0), tgamma(x/3.0));
  }
  return 0;
}
```

{{out}}

```txt
       Stirling         Spouge            GSL           libm
     2.15697602     2.67893853     2.67893853     2.67893853
     1.20285073     1.35411794     1.35411794     1.35411794
     0.92213701     1.00000000     1.00000000     1.00000000
     0.83974270     0.89297951     0.89297951     0.89297951
     0.85919025     0.90274529     0.90274529     0.90274529
     0.95950218     1.00000000     1.00000000     1.00000000
     1.14910642     1.19063935     1.19063935     1.19063935
     1.45849038     1.50457549     1.50457549     1.50457549
     1.94540320     2.00000000     2.00000000     2.00000000
     2.70976382     2.77815848     2.77815848     2.77815848

```


## C#

This is just rewritten from the Wikipedia Lanczos article.  Works with complex numbers as well as reals.

```c#
using System;
using System.Numerics;

static int g = 7;
static double[] p = {0.99999999999980993, 676.5203681218851, -1259.1392167224028,
	     771.32342877765313, -176.61502916214059, 12.507343278686905,
	     -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7};

Complex Gamma(Complex z)
{
    // Reflection formula
    if (z.Real < 0.5)
	{
        return Math.PI / (Complex.Sin( Math.PI * z) * Gamma(1 - z));
	}
    else
	{
        z -= 1;
        Complex x = p[0];
        for (var i = 1; i < g + 2; i++)
		{
            x += p[i]/(z+i);
		}
        Complex t = z + g + 0.5;
        return Complex.Sqrt(2 * Math.PI) * (Complex.Pow(t, z + 0.5)) * Complex.Exp(-t) * x;
	}
}

```



## Clojure


```clojure
(defn gamma
  "Returns Gamma(z + 1 = number) using Lanczos approximation."
  [number]
  (if (< number 0.5)
       (/ Math/PI (* (Math/sin (* Math/PI number))
	             (gamma (- 1 number))))
       (let [n (dec number)
      	     c [0.99999999999980993 676.5203681218851 -1259.1392167224028
	        771.32342877765313 -176.61502916214059 12.507343278686905
	        -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]]
         (* (Math/sqrt (* 2 Math/PI))
	    (Math/pow (+ n 7 0.5) (+ n 0.5))
	    (Math/exp (- (+ n 7 0.5)))
            (+ (first c)
               (apply + (map-indexed #(/ %2 (+ n %1 1)) (next c))))))))
```

{{out}}

```clojure
(map #(printf "%.1f %.4f\n" % (gamma %)) (map #(float (/ % 10)) (range 1 31)))
```


```txt

0.1 9.5135
0.2 4.5908
0.3 2.9916
0.4 2.2182
0.5 1.7725
0.6 1.4892
0.7 1.2981
0.8 1.1642
0.9 1.0686
1.0 1.0000
1.1 0.9514
1.2 0.9182
1.3 0.8975
1.4 0.8873
1.5 0.8862
1.6 0.8935
1.7 0.9086
1.8 0.9314
1.9 0.9618
2.0 1.0000
2.1 1.0465
2.2 1.1018
2.3 1.1667
2.4 1.2422
2.5 1.3293
2.6 1.4296
2.7 1.5447
2.8 1.6765
2.9 1.8274
3.0 2.0000

```



## Common Lisp


```lisp
; Taylor series coefficients
(defconstant tcoeff
  '( 1.00000000000000000000  0.57721566490153286061 -0.65587807152025388108
    -0.04200263503409523553  0.16653861138229148950 -0.04219773455554433675
    -0.00962197152787697356  0.00721894324666309954 -0.00116516759185906511
    -0.00021524167411495097  0.00012805028238811619 -0.00002013485478078824
    -0.00000125049348214267  0.00000113302723198170 -0.00000020563384169776
     0.00000000611609510448  0.00000000500200764447 -0.00000000118127457049
     0.00000000010434267117  0.00000000000778226344 -0.00000000000369680562
     0.00000000000051003703 -0.00000000000002058326 -0.00000000000000534812
     0.00000000000000122678 -0.00000000000000011813  0.00000000000000000119
     0.00000000000000000141 -0.00000000000000000023  0.00000000000000000002))

; number of coefficients
(defconstant numcoeff (length tcoeff))

(defun gamma (x)
  (let ((y (- x 1.0))
        (sum (nth (- numcoeff 1) tcoeff)))
    (loop for i from (- numcoeff 2) downto 0 do
          (setf sum (+ (* sum y) (nth i tcoeff))))
    (/ 1.0 sum)))

(loop for i from 1 to 10
   do (
     format t "~12,10f~%" (gamma (/ i 3.0))))
```

{{out|Produces}}

```txt

2.6789380000
1.3541179000
1.0000000000
0.8929794500
0.9027453000
1.0000000000
1.1906393000
1.5045753000
1.9999995000
2.7781580000

```



## Crystal

====Taylor Series | Lanczos Method | Builtin Function====
{{trans|Taylor Series from Ruby; Lanczos Method from C#}}

```ruby
# Taylor Series
def a
   [ 1.00000_00000_00000_00000,  0.57721_56649_01532_86061, -0.65587_80715_20253_88108,
      -0.04200_26350_34095_23553,  0.16653_86113_82291_48950, -0.04219_77345_55544_33675,
      -0.00962_19715_27876_97356,  0.00721_89432_46663_09954, -0.00116_51675_91859_06511,
      -0.00021_52416_74114_95097,  0.00012_80502_82388_11619, -0.00002_01348_54780_78824,
      -0.00000_12504_93482_14267,  0.00000_11330_27231_98170, -0.00000_02056_33841_69776,
       0.00000_00061_16095_10448,  0.00000_00050_02007_64447, -0.00000_00011_81274_57049,
       0.00000_00001_04342_67117,  0.00000_00000_07782_26344, -0.00000_00000_03696_80562,
       0.00000_00000_00510_03703, -0.00000_00000_00020_58326, -0.00000_00000_00005_34812,
       0.00000_00000_00001_22678, -0.00000_00000_00000_11813,  0.00000_00000_00000_00119,
       0.00000_00000_00000_00141, -0.00000_00000_00000_00023,  0.00000_00000_00000_00002 ]
end

def taylor_gamma(x)
  y = x.to_f - 1
  1.0 / a.reverse.reduce(0) { |sum, an| sum * y + an }
end

# Lanczos Method
def p
  [ 0.99999_99999_99809_93, 676.52036_81218_851, -1259.13921_67224_028,
    771.32342_87776_5313, -176.61502_91621_4059,  12.50734_32786_86905,
    -0.13857_10952_65720_12, 9.98436_95780_19571_6e-6, 1.50563_27351_49311_6e-7 ]
end

def lanczos_gamma(z)
  # Reflection formula
  z = z.to_f
  if z < 0.5
    Math::PI / (Math.sin(Math::PI * z) * lanczos_gamma(1 - z))
  else
    z -= 1
    x = p[0]
    (1..p.size - 1).each { |i| x += p[i] / (z + i) }
    t = z + p.size - 1.5
    Math.sqrt(2 * Math::PI) * t**(z + 0.5) * Math.exp(-t) * x
  end
end

puts "                Taylor Series         Lanczos Method        Builtin Function"
(1..27).each { |i| n = i/3.0; puts "gamma(%.2f) = %.14e  %.14e  %.14e" % [n, taylor_gamma(n), lanczos_gamma(n), Math.gamma(n)] }

```

{{out}}

```txt

                Taylor Series         Lanczos Method        Builtin Function
gamma(0.33) = 2.67893853470775e+00  2.67893853470775e+00  2.67893853470775e+00
gamma(0.67) = 1.35411793942640e+00  1.35411793942640e+00  1.35411793942640e+00
gamma(1.00) = 1.00000000000000e+00  1.00000000000000e+00  1.00000000000000e+00
gamma(1.33) = 8.92979511569249e-01  8.92979511569249e-01  8.92979511569249e-01
gamma(1.67) = 9.02745292950934e-01  9.02745292950935e-01  9.02745292950934e-01
gamma(2.00) = 1.00000000000000e+00  1.00000000000000e+00  1.00000000000000e+00
gamma(2.33) = 1.19063934875900e+00  1.19063934875900e+00  1.19063934875900e+00
gamma(2.67) = 1.50457548825154e+00  1.50457548825156e+00  1.50457548825156e+00
gamma(3.00) = 1.99999999999397e+00  2.00000000000000e+00  2.00000000000000e+00
gamma(3.33) = 2.77815847933857e+00  2.77815848043767e+00  2.77815848043766e+00
gamma(3.67) = 4.01220118377482e+00  4.01220130200415e+00  4.01220130200415e+00
gamma(4.00) = 5.99999141007240e+00  6.00000000000001e+00  6.00000000000000e+00
gamma(4.33) = 9.26006653812473e+00  9.26052826812555e+00  9.26052826812554e+00
gamma(4.67) = 1.46918499266721e+01  1.47114047740152e+01  1.47114047740152e+01
gamma(5.00) = 2.33327665969918e+01  2.40000000000000e+01  2.40000000000000e+01
gamma(5.33) = 2.65211050660964e+01  4.01289558285441e+01  4.01289558285440e+01
gamma(5.67) = 7.70471336505311e+00  6.86532222787379e+01  6.86532222787377e+01
gamma(6.00) = 1.10934146590517e+00  1.20000000000000e+02  1.20000000000000e+02
gamma(6.33) = 1.64621072447163e-01  2.14021097752236e+02  2.14021097752235e+02
gamma(6.67) = 2.72102446536397e-02  3.89034926246181e+02  3.89034926246180e+02
gamma(7.00) = 4.98014348954507e-03  7.20000000000002e+02  7.20000000000000e+02
gamma(7.33) = 9.98845907123850e-04  1.35546695243082e+03  1.35546695243082e+03
gamma(7.67) = 2.17513475446479e-04  2.59356617497454e+03  2.59356617497454e+03
gamma(8.00) = 5.10217006678528e-05  5.04000000000001e+03  5.04000000000000e+03
gamma(8.33) = 1.28035516395359e-05  9.94009098449271e+03  9.94009098449270e+03
gamma(8.67) = 3.41689149138074e-06  1.98840073414715e+04  1.98840073414714e+04
gamma(9.00) = 9.64721467591131e-07  4.03200000000001e+04  4.03200000000000e+04

```



## D


```d
import std.stdio, std.math, std.mathspecial;

real taylorGamma(in real x) pure nothrow @safe @nogc {
    static immutable real[30] table = [
     0x1p+0,                    0x1.2788cfc6fb618f4cp-1,
    -0x1.4fcf4026afa2dcecp-1,  -0x1.5815e8fa27047c8cp-5,
     0x1.5512320b43fbe5dep-3,  -0x1.59af103c340927bep-5,
    -0x1.3b4af28483e214e4p-7,   0x1.d919c527f60b195ap-8,
    -0x1.317112ce3a2a7bd2p-10, -0x1.c364fe6f1563ce9cp-13,
     0x1.0c8a78cd9f9d1a78p-13, -0x1.51ce8af47eabdfdcp-16,
    -0x1.4fad41fc34fbb2p-20,    0x1.302509dbc0de2c82p-20,
    -0x1.b9986666c225d1d4p-23,  0x1.a44b7ba22d628acap-28,
     0x1.57bc3fc384333fb2p-28, -0x1.44b4cedca388f7c6p-30,
     0x1.cae7675c18606c6p-34,   0x1.11d065bfaf06745ap-37,
    -0x1.0423bac8ca3faaa4p-38,  0x1.1f20151323cd0392p-41,
    -0x1.72cb88ea5ae6e778p-46, -0x1.815f72a05f16f348p-48,
     0x1.6198491a83bccbep-50,  -0x1.10613dde57a88bd6p-53,
     0x1.5e3fee81de0e9c84p-60,  0x1.a0dc770fb8a499b6p-60,
    -0x1.0f635344a29e9f8ep-62,  0x1.43d79a4b90ce8044p-66];

    immutable real y = x - 1.0L;
    real sm = table[$ - 1];
    foreach_reverse (immutable an; table[0 .. $ - 1])
        sm = sm * y + an;
    return 1.0L / sm;
}

real lanczosGamma(real z) pure nothrow @safe @nogc {
    // Coefficients used by the GNU Scientific Library.
    // http://en.wikipedia.org/wiki/Lanczos_approximation
    enum g = 7;
    static immutable real[9] table =
        [    0.99999_99999_99809_93,
           676.52036_81218_851,
         -1259.13921_67224_028,
           771.32342_87776_5313,
          -176.61502_91621_4059,
            12.50734_32786_86905,
            -0.13857_10952_65720_12,
             9.98436_95780_19571_6e-6,
             1.50563_27351_49311_6e-7];

    // Reflection formula.
    if (z < 0.5L) {
        return PI / (sin(PI * z) * lanczosGamma(1 - z));
    } else {
        z -= 1;
        real x = table[0];
        foreach (immutable i; 1 .. g + 2)
            x += table[i] / (z + i);
        immutable real t = z + g + 0.5L;
        return sqrt(2 * PI) * t ^^ (z + 0.5L) * exp(-t) * x;
    }
}

void main() {
    foreach (immutable i; 1 .. 11) {
        immutable real x = i / 3.0L;
        writefln("%f: %20.19e %20.19e %20.19e", x,
                 x.taylorGamma, x.lanczosGamma, x.gamma);
    }
}
```

{{out}}

```txt
0.333333: 2.6789385347077476335e+00 2.6789385347077470551e+00 2.6789385347077476339e+00
0.666667: 1.3541179394264004169e+00 1.3541179394264007092e+00 1.3541179394264004170e+00
1.000000: 1.0000000000000000000e+00 1.0000000000000002126e+00 1.0000000000000000000e+00
1.333333: 8.9297951156924921124e-01 8.9297951156924947465e-01 8.9297951156924921132e-01
1.666667: 9.0274529295093361132e-01 9.0274529295093396555e-01 9.0274529295093361123e-01
2.000000: 1.0000000000000000000e+00 1.0000000000000004903e+00 1.0000000000000000000e+00
2.333333: 1.1906393487589989474e+00 1.1906393487589996490e+00 1.1906393487589989482e+00
2.666667: 1.5045754882515545787e+00 1.5045754882515570474e+00 1.5045754882515560190e+00
3.000000: 1.9999999999992207405e+00 2.0000000000000015575e+00 2.0000000000000000000e+00
3.333333: 2.7781584802531739378e+00 2.7781584804376666336e+00 2.7781584804376642124e+00
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Gamma do
  @a [ 1.00000_00000_00000_00000,  0.57721_56649_01532_86061, -0.65587_80715_20253_88108,
      -0.04200_26350_34095_23553,  0.16653_86113_82291_48950, -0.04219_77345_55544_33675,
      -0.00962_19715_27876_97356,  0.00721_89432_46663_09954, -0.00116_51675_91859_06511,
      -0.00021_52416_74114_95097,  0.00012_80502_82388_11619, -0.00002_01348_54780_78824,
      -0.00000_12504_93482_14267,  0.00000_11330_27231_98170, -0.00000_02056_33841_69776,
       0.00000_00061_16095_10448,  0.00000_00050_02007_64447, -0.00000_00011_81274_57049,
       0.00000_00001_04342_67117,  0.00000_00000_07782_26344, -0.00000_00000_03696_80562,
       0.00000_00000_00510_03703, -0.00000_00000_00020_58326, -0.00000_00000_00005_34812,
       0.00000_00000_00001_22678, -0.00000_00000_00000_11813,  0.00000_00000_00000_00119,
       0.00000_00000_00000_00141, -0.00000_00000_00000_00023,  0.00000_00000_00000_00002 ]
     |> Enum.reverse
  def taylor(x) do
    y = x - 1
    1 / Enum.reduce(@a, 0, fn a,sum -> sum * y + a end)
  end
end

Enum.each(Enum.map(1..10, &(&1/3)), fn x ->
  :io.format "~f  ~18.15f~n", [x, Gamma.taylor(x)]
end)
```


{{out}}

```txt

0.333333   2.678938534707748
0.666667   1.354117939426401
1.000000   1.000000000000000
1.333333   0.892979511569249
1.666667   0.902745292950934
2.000000   1.000000000000000
2.333333   1.190639348758999
2.666667   1.504575488251540
3.000000   1.999999999993968
3.333333   2.778158479338573

```



## Factor


```factor
! built in
USING: picomath prettyprint ;
0.1 gamma .  ! 9.513507698668723
2.0 gamma .  ! 1.0
10. gamma .  ! 362880.0
```



## Forth

Cristinel Mortici describes this method in Applied Mathematics Letters. "A substantial improvement of the Stirling formula". This algorithm is said to give about 7 good digits, but becomes more inaccurate close to zero. Therefore, a "shift" is performed to move the value returned into the more accurate domain.

```forth
8 constant (gamma-shift)

: (mortici)                            ( f1 -- f2)
  -1 s>f f+ 1 s>f
  fover 271828183e-8 f* 12 s>f f* f/
  fover 271828183e-8 f/ f+
  fover f** fswap
  628318530e-8 f* fsqrt f*             \ 2*pi
;

: gamma                                ( f1 -- f2)
  fdup f0< >r fdup f0= r> or abort" Gamma less or equal to zero"
  fdup (gamma-shift) s>f f+ (mortici) fswap
  1 s>f (gamma-shift) 0 do fover i s>f f+ f* loop fswap fdrop f/
;
```


```txt

0.1e gamma f. 9.51348888533932  ok
2e gamma f. 0.999999031674546  ok
10e gamma f. 362879.944850072  ok
70e gamma fe. 171.122444600510E96  ok

```

This is a word, based on a formula of Ramanujan's famous "lost notebook", which was rediscovered in 1976. His formula contained a constant, which had a value between 1/100 and 1/30. In 2008, E.A. Karatsuba described the function, which determines the value of this constant. Since it contains the gamma function itself, it can't be used in a word calculating the gamma function, so here it is emulated by two symmetrical sigmoidals.

```forth
2 constant (gamma-shift)               \ don't change this
                                       \ an approximation of the d(x) function
: ~d(x)                                ( f1 -- f2)
  fdup 10 s>f f<                       \ use first symmetrical sigmoidal
  if                                   \ for range 1-10
    -2705443e-8 fswap 2280802e-6 f/ 1428045e-6 f** 1 s>f f+ f/ 3187831e-8 f+
  else                                 \ use second symmetrical sigmoidal
    -29372563e-9 fswap 1841693e-6 f/ 1052779e-6 f** 1 s>f f+ f/ 3330828e-8 f+
  then 333333333e-10 fover f< if fdrop 1 s>f 30 s>f f/ then
;                                      \ perform some sane clipping to infinity

: (ramanujan)                          ( f1 -- f2)
  fdup fdup f* 4 s>f f*                ( n 4n2)
  fover fover f* fdup f+ f+ fover f+   ( n 8n3+4n2+n)
  fover ~d(x) f+                       ( n 8n3+4n2+n+d[x])
  1 s>f 6 s>f f/ f**                   ( n 8n3+4n2+n+d[x]^1/6)
  fswap fdup 2.7182818284590452353e f/ ( 8n3+4n2+n+d[x]^1/6 n n/e)
  fswap f** f* pi fsqrt f*             ( f)
;

: gamma                                ( f1 -- f2)
  fdup f0< >r fdup f0= r> or abort" Gamma less or equal to zero"
  fdup (gamma-shift) 1- s>f f+ (ramanujan) fswap
  1 s>f (gamma-shift) 0 do fover i s>f f+ f* loop fswap fdrop f/
;
```


```txt

0.1e gamma f. 9.51351721918848  ok
2e gamma f. 0.999999966026125  ok
10e gamma f. 362879.999559333  ok
70e gamma fe. 171.122452428147E96  ok

```



## Fortran

This code shows two methods: [[Numerical Integration]] through Simpson formula, and [[wp:Lanczos approximation|Lanczos approximation]]. The results of testing are printed altogether with the values given by the function <tt>gamma</tt>; this function is defined in the Fortran 2008 standard, and supported by GNU Fortran (and other vendors) as extension; if not present in your compiler, you can remove the last part of the print in order to get it compiled with any Fortran 95 compliant compiler.
{{works with|Fortran|2008}}
{{works with|Fortran|95 with extensions}}

```fortran
program ComputeGammaInt

  implicit none

  integer :: i

  write(*, "(3A15)") "Simpson", "Lanczos", "Builtin"
  do i=1, 10
     write(*, "(3F15.8)") my_gamma(i/3.0), lacz_gamma(i/3.0), gamma(i/3.0)
  end do

contains

  pure function intfuncgamma(x, y) result(z)
    real :: z
    real, intent(in) :: x, y

    z = x**(y-1.0) * exp(-x)
  end function intfuncgamma


  function my_gamma(a) result(g)
    real :: g
    real, intent(in) :: a

    real, parameter :: small = 1.0e-4
    integer, parameter :: points = 100000

    real :: infty, dx, p, sp(2, points), x
    integer :: i
    logical :: correction

    x = a

    correction = .false.
    ! value with x<1 gives \infty, so we use
    ! \Gamma(x+1) = x\Gamma(x)
    ! to avoid the problem
    if ( x < 1.0 ) then
       correction = .true.
       x = x + 1
    end if

    ! find a "reasonable" infinity...
    ! we compute this integral indeed
    ! \int_0^M dt t^{x-1} e^{-t}
    ! where M is such that M^{x-1} e^{-M} ≤ \epsilon
    infty = 1.0e4
    do while ( intfuncgamma(infty, x) > small )
       infty = infty * 10.0
    end do

    ! using simpson
    dx = infty/real(points)
    sp = 0.0
    forall(i=1:points/2-1) sp(1, 2*i) = intfuncgamma(2.0*(i)*dx, x)
    forall(i=1:points/2) sp(2, 2*i - 1) = intfuncgamma((2.0*(i)-1.0)*dx, x)
    g = (intfuncgamma(0.0, x) + 2.0*sum(sp(1,:)) + 4.0*sum(sp(2,:)) + &
         intfuncgamma(infty, x))*dx/3.0

    if ( correction ) g = g/a

  end function my_gamma


  recursive function lacz_gamma(a) result(g)
    real, intent(in) :: a
    real :: g

    real, parameter :: pi = 3.14159265358979324
    integer, parameter :: cg = 7

    ! these precomputed values are taken by the sample code in Wikipedia,
    ! and the sample itself takes them from the GNU Scientific Library
    real, dimension(0:8), parameter :: p = &
         (/ 0.99999999999980993, 676.5203681218851, -1259.1392167224028, &
         771.32342877765313, -176.61502916214059, 12.507343278686905, &
         -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7 /)

    real :: t, w, x
    integer :: i

    x = a

    if ( x < 0.5 ) then
       g = pi / ( sin(pi*x) * lacz_gamma(1.0-x) )
    else
       x = x - 1.0
       t = p(0)
       do i=1, cg+2
          t = t + p(i)/(x+real(i))
       end do
       w = x + real(cg) + 0.5
       g = sqrt(2.0*pi) * w**(x+0.5) * exp(-w) * t
    end if
  end function lacz_gamma

end program ComputeGammaInt
```

{{out}}

```txt
        Simpson        Lanczos        Builtin
     2.65968132     2.67893744     2.67893839
     1.35269761     1.35411859     1.35411787
     1.00000060     1.00000024     1.00000000
     0.88656044     0.89297968     0.89297950
     0.90179849     0.90274525     0.90274531
     0.99999803     1.00000036     1.00000000
     1.19070935     1.19063985     1.19063926
     1.50460517     1.50457609     1.50457561
     2.00000286     2.00000072     2.00000000
     2.77815390     2.77816010     2.77815843
```



## FreeBASIC

{{trans|Java}}

```freebasic
' FB 1.05.0 Win64

Const pi = 3.1415926535897932
Const e  = 2.7182818284590452

Function gammaStirling (x As Double) As Double
  Return Sqr(2.0 * pi / x) * ((x / e) ^ x)
End Function

Function gammaLanczos (x As Double) As Double
  Dim p(0 To 8) As Double = _
  { _
       0.99999999999980993, _
     676.5203681218851, _
   -1259.1392167224028, _
     771.32342877765313, _
    -176.61502916214059, _
      12.507343278686905, _
      -0.13857109526572012, _
       9.9843695780195716e-6, _
       1.5056327351493116e-7 _
  }

  Dim As Integer g = 7
  If x < 0.5 Then Return pi / (Sin(pi * x) * gammaLanczos(1-x))
  x -= 1
  Dim a As Double = p(0)
  Dim t As Double = x + g + 0.5

  For i As Integer = 1 To 8
    a += p(i) / (x + i)
  Next

  Return Sqr(2.0 * pi) * (t ^ (x + 0.5)) * Exp(-t) * a
End Function

Print " x", "    Stirling",, "    Lanczos"
Print
For i As Integer = 1 To 20
   Dim As Double d = i / 10.0
   Print   Using "#.##"; d;
   Print , Using "#.###############"; gammaStirling(d);
   Print , Using "#.###############"; gammaLanczos(d)
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 x                Stirling                    Lanczos

0.10          5.697187148977170           9.513507698668738
0.20          3.325998424022393           4.590843711998803
0.30          2.362530036269620           2.991568987687590
0.40          1.841476335936235           2.218159543757687
0.50          1.520346901066281           1.772453850905516
0.60          1.307158857448356           1.489192248812818
0.70          1.159053292113920           1.298055332647558
0.80          1.053370968425609           1.164229713725303
0.90          0.977061507877695           1.068628702119319
1.00          0.922137008895789           1.000000000000000
1.10          0.883489953168704           0.951350769866874
1.20          0.857755335396591           0.918168742399761
1.30          0.842678259448392           0.897470696306278
1.40          0.836744548637082           0.887263817503076
1.50          0.838956552526496           0.886226925452759
1.60          0.848693242152574           0.893515349287691
1.70          0.865621471793884           0.908638732853291
1.80          0.889639635287995           0.931383770980243
1.90          0.920842721894229           0.961765831907388
2.00          0.959502175744492           1.000000000000000

```



## F Sharp


Solved using the Lanczos Coefficients described in Numerical Recipes (Press et al)


```F Sharp


open System

let gamma z =
    let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
    let rec sumCoefficients acc i coefficients =
        match coefficients with
        | []   -> acc
        | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
    let gamma = 5.0
    let x = z - 1.0
    Math.Pow(x + gamma + 0.5, x + 0.5) * Math.Exp( -(x + gamma + 0.5) ) * Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients

seq { for i in 1 .. 20 do yield ((double)i/10.0) } |> Seq.iter ( fun v -> System.Console.WriteLine("{0} : {1}", v, gamma v ) )
seq { for i in 1 .. 10 do yield ((double)i*10.0) } |> Seq.iter ( fun v -> System.Console.WriteLine("{0} : {1}", v, gamma v ) )


```


{{out}}

```txt

0.1 : 9.51350769855015
0.2 : 4.59084371196153
0.3 : 2.99156898767207
0.4 : 2.21815954375051
0.5 : 1.77245385090205
0.6 : 1.48919224881114
0.7 : 1.29805533264677
0.8 : 1.16422971372497
0.9 : 1.06862870211921
1 : 1
1.1 : 0.951350769866919
1.2 : 0.91816874239982
1.3 : 0.897470696306335
1.4 : 0.887263817503124
1.5 : 0.886226925452797
1.6 : 0.893515349287718
1.7 : 0.908638732853309
1.8 : 0.931383770980253
1.9 : 0.961765831907391
2 : 1
10 : 362880.000000085
20 : 1.21645100409886E+17
30 : 8.84176199395902E+30
40 : 2.03978820820436E+46
50 : 6.08281864068541E+62
60 : 1.38683118555266E+80
70 : 1.71122452441801E+98
80 : 8.94618213157899E+116
90 : 1.65079551625067E+136
100 : 9.33262154536104E+155

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    fmt.Println("    x               math.Gamma                 Lanczos7")
    for _, x := range []float64{-.5, .1, .5, 1, 1.5, 2, 3, 10, 140, 170} {
        fmt.Printf("%5.1f %24.16g %24.16g\n", x, math.Gamma(x), lanczos7(x))
    }
}

func lanczos7(z float64) float64 {
    t := z + 6.5
    x := .99999999999980993 +
        676.5203681218851/z -
        1259.1392167224028/(z+1) +
        771.32342877765313/(z+2) -
        176.61502916214059/(z+3) +
        12.507343278686905/(z+4) -
        .13857109526572012/(z+5) +
        9.9843695780195716e-6/(z+6) +
        1.5056327351493116e-7/(z+7)
    return math.Sqrt2 * math.SqrtPi * math.Pow(t, z-.5) * math.Exp(-t) * x
}
```

{{out}}

```txt

    x               math.Gamma                 Lanczos7
 -0.5       -3.544907701811032       -3.544907701811087
  0.1        9.513507698668732        9.513507698668752
  0.5        1.772453850905516        1.772453850905517
  1.0                        1                        1
  1.5       0.8862269254527579       0.8862269254527587
  2.0                        1                        1
  3.0                        2                        2
 10.0                   362880        362880.0000000015
140.0    9.61572319694107e+238   9.615723196940201e+238
170.0   4.269068009004746e+304                     +Inf

```



## Groovy

{{trans|Ada}}

```groovy
a = [ 1.00000000000000000000, 0.57721566490153286061, -0.65587807152025388108,
     -0.04200263503409523553, 0.16653861138229148950, -0.04219773455554433675,
     -0.00962197152787697356, 0.00721894324666309954, -0.00116516759185906511,
     -0.00021524167411495097, 0.00012805028238811619, -0.00002013485478078824,
     -0.00000125049348214267, 0.00000113302723198170, -0.00000020563384169776,
      0.00000000611609510448, 0.00000000500200764447, -0.00000000118127457049,
      0.00000000010434267117, 0.00000000000778226344, -0.00000000000369680562,
      0.00000000000051003703, -0.00000000000002058326, -0.00000000000000534812,
      0.00000000000000122678, -0.00000000000000011813, 0.00000000000000000119,
      0.00000000000000000141, -0.00000000000000000023, 0.00000000000000000002].reverse()

def gamma = { 1.0 / a.inject(0) { sm, a_i -> sm * (it - 1) + a_i } }

(1..10).each{ printf("%  1.9e\n", gamma(it / 3.0)) }

```

{{out}}

```txt
  2.678938535e+00
  1.354117939e+00
  1.000000000e+00
  8.929795116e-01
  9.027452930e-01
  1.000000000e+00
  1.190639349e+00
  1.504575488e+00
  2.000000000e+00
  2.778158479e+00
```



## Haskell

Based on [http://www.haskell.org/haskellwiki/?title=Gamma_and_Beta_function&oldid=25546 HaskellWiki] ([http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights compatible license]):
:The Gamma and Beta function as described in 'Numerical Recipes in C++', the approximation is taken from [Lanczos, C. 1964 SIAM Journal on Numerical Analysis, ser. B, vol. 1, pp. 86-96]

```haskell
cof :: [Double]
cof =
  [ 76.18009172947146
  , -86.50532032941677
  , 24.01409824083091
  , -1.231739572450155
  , 0.001208650973866179
  , -0.000005395239384953
  ]

ser :: Double
ser = 1.000000000190015

gammaln :: Double -> Double
gammaln xx =
  let tmp_ = (xx + 5.5) - (xx + 0.5) * log (xx + 5.5)
      ser_ = ser + sum (zipWith (/) cof [xx + 1 ..])
  in -tmp_ + log (2.5066282746310005 * ser_ / xx)

main :: IO ()
main = mapM_ print $ gammaln <$> [0.1,0.2 .. 1.0]
```


Or equivalently, as a point-free applicative expression:

```haskell
import Control.Applicative

cof :: [Double]
cof =
  [ 76.18009172947146
  , -86.50532032941677
  , 24.01409824083091
  , -1.231739572450155
  , 0.001208650973866179
  , -0.000005395239384953
  ]

gammaln :: Double -> Double
gammaln =
  ((+) . negate . (((-) . (5.5 +)) <*> (((*) . (0.5 +)) <*> (log . (5.5 +))))) <*>
  (log .
   ((/) =<<
    (2.5066282746310007 *) .
    (1.000000000190015 +) . sum . zipWith (/) cof . enumFrom . (1 +)))

main :: IO ()
main = mapM_ print $ gammaln <$> [0.1,0.2 .. 1.0]
```

{{Out}}

```txt
2.252712651734255
1.5240638224308496
1.09579799481814
0.7966778177018394
0.572364942924743
0.3982338580692666
0.2608672465316877
0.15205967839984869
6.637623973474716e-2
-4.440892098500626e-16
```


==Icon and {{header|Unicon}}==

This works in Unicon.  Changing the <tt>!10</tt> into <tt>(1 to 10)</tt> would enable it
to work in Icon.

```unicon
procedure main()
    every write(left(i := !10/10.0,5),gamma(.i))
end

procedure gamma(z)	# Stirling's approximation
    return (2*&pi/z)^0.5 * (z/&e)^z
end
```


{{Out}}

```txt

->gamma
0.1  5.69718714897717
0.2  3.325998424022393
0.3  2.36253003626962
0.4  1.841476335936235
0.5  1.520346901066281
0.6  1.307158857448356
0.7  1.15905329211392
0.8  1.053370968425609
0.9  0.9770615078776954
1.0  0.9221370088957891
->

```



## J

This code shows the built-in method, which works for any value (positive, negative and complex numbers -- but note that negative integer arguments give infinite results).

```j
gamma=: !@<:
```

Note that <: subtracts one from a number.  It's sort of like <code>--lvalue</code> in C, except it always accepts an "rvalue" as an argument (which means it does not modify that argument).  And <code>!value</code> finds the factorial of value if value is a positive integer.  This illustrates the close relationship between the factorial and gamma functions.

The following direct coding of the task comes from the [[J:Essays/Stirling's%20Approximation|Stirling's approximation essay]] on the J wiki:

```j
sbase =: %:@(2p1&%) * %&1x1 ^ ]
scorr =: 1 1r12 1r288 _139r51840 _571r2488320&p.@%
stirlg=: sbase * scorr
```

Checking against <code>!@<:</code> we can see that this approximation loses accuracy for small arguments

```j
   (,. stirlg ,. gamma) 10 1p1 1x1 1.5 1
     10   362880   362880
3.14159  2.28803  2.28804
2.71828  1.56746  1.56747
    1.5 0.886155 0.886227
      1 0.999499        1
```

(Column 1 is the argument, column 2 is the stirling approximation and column 3 uses the builtin support for gamma.)


## Java

Implementation of Stirling's approximation and Lanczos approximation.

```java
public class GammaFunction {

	public double st_gamma(double x){
		return Math.sqrt(2*Math.PI/x)*Math.pow((x/Math.E), x);
	}

	public double la_gamma(double x){
		double[] p = {0.99999999999980993, 676.5203681218851, -1259.1392167224028,
			     	  771.32342877765313, -176.61502916214059, 12.507343278686905,
			     	  -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7};
		int g = 7;
		if(x < 0.5) return Math.PI / (Math.sin(Math.PI * x)*la_gamma(1-x));

		x -= 1;
		double a = p[0];
		double t = x+g+0.5;
		for(int i = 1; i < p.length; i++){
			a += p[i]/(x+i);
		}

		return Math.sqrt(2*Math.PI)*Math.pow(t, x+0.5)*Math.exp(-t)*a;
	}

	public static void main(String[] args) {
		GammaFunction test = new GammaFunction();
		System.out.println("Gamma \t\tStirling \t\tLanczos");
		for(double i = 1; i <= 20; i += 1){
			System.out.println("" + i/10.0 + "\t\t" + test.st_gamma(i/10.0) + "\t" + test.la_gamma(i/10.0));
		}
	}
}
```

{{out}}

```txt

Gamma 		Stirling 		Lanczos
0.1		5.697187148977169	9.513507698668734
0.2		3.3259984240223925	4.590843711998803
0.3		2.3625300362696198	2.9915689876875904
0.4		1.8414763359362354	2.218159543757687
0.5		1.5203469010662807	1.7724538509055159
0.6		1.307158857448356	1.489192248812818
0.7		1.15905329211392	1.2980553326475577
0.8		1.0533709684256085	1.1642297137253035
0.9		0.9770615078776954	1.0686287021193193
1.0		0.9221370088957891	0.9999999999999998
1.1		0.8834899531687038	0.9513507698668735
1.2		0.8577553353965909	0.9181687423997607
1.3		0.8426782594483921	0.8974706963062777
1.4		0.8367445486370817	0.8872638175030757
1.5		0.8389565525264963	0.8862269254527586
1.6		0.8486932421525738	0.8935153492876909
1.7		0.865621471793884	0.9086387328532916
1.8		0.8896396352879945	0.9313837709802425
1.9		0.9208427218942293	0.9617658319073877
2.0		0.9595021757444916	1.0000000000000002

```



## JavaScript

Implementation of Lanczos approximation.

```javascript
function gamma(x) {
    var p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ];

    var g = 7;
    if (x < 0.5) {
        return Math.PI / (Math.sin(Math.PI * x) * gamma(1 - x));
    }

    x -= 1;
    var a = p[0];
    var t = x + g + 0.5;
    for (var i = 1; i < p.length; i++) {
        a += p[i] / (x + i);
    }

    return Math.sqrt(2 * Math.PI) * Math.pow(t, x + 0.5) * Math.exp(-t) * a;
}
```



## jq

{{works with|jq|1.4}}

### =Taylor Series=

{{trans|Ada}}

```jq
def gamma:
  [
    1.00000000000000000000,  0.57721566490153286061,  -0.65587807152025388108, -0.04200263503409523553,
    0.16653861138229148950, -0.04219773455554433675,  -0.00962197152787697356,  0.00721894324666309954,
   -0.00116516759185906511, -0.00021524167411495097,   0.00012805028238811619, -0.00002013485478078824,
   -0.00000125049348214267,  0.00000113302723198170,  -0.00000020563384169776,  0.00000000611609510448,
    0.00000000500200764447, -0.00000000118127457049,   0.00000000010434267117,  0.00000000000778226344,
   -0.00000000000369680562,  0.00000000000051003703,  -0.00000000000002058326, -0.00000000000000534812,
    0.00000000000000122678, -0.00000000000000011813,   0.00000000000000000119,  0.00000000000000000141,
   -0.00000000000000000023,  0.00000000000000000002
  ] as $a
  | (. - 1) as $y
  | ($a|length) as $n
  | reduce range(2; 1+$n) as $an
      ($a[$n-1]; (. * $y) + $a[$n - $an])
  | 1 / . ;
```


### =Lanczos Approximation=


```jq
# for reals, but easily extended to complex values
def gamma_by_lanczos:
  def pow(x): if x == 0 then 1 elif x == 1 then . else x * log | exp end;
  . as $x
  | ((1|atan) * 4) as $pi
  | if $x < 0.5 then $pi / ((($pi * $x) | sin) * ((1-$x)|gamma_by_lanczos ))
    else
      [   0.99999999999980993, 676.5203681218851,     -1259.1392167224028,
        771.32342877765313,   -176.61502916214059,       12.507343278686905,
         -0.13857109526572012,   9.9843695780195716e-6,   1.5056327351493116e-7] as $p
    | ($x - 1) as $x
    | ($x + 7.5) as $t
    |  reduce range(1; $p|length) as $i
          ($p[0]; . + ($p[$i]/($x + $i) ))
       * ((2*$pi) | sqrt) * ($t | pow($x+0.5)) * ((-$t)|exp)
    end;
```

====Stirling's Approximation====

```jq
def gamma_by_stirling:
  def pow(x): if x == 0 then 1 elif x == 1 then . else x * log | exp end;
  ((1|atan) * 8) as $twopi
  | . as $x
  | (($twopi/$x) | sqrt) * ( ($x / (1|exp)) | pow($x));
```


### =Examples=

Stirling's method produces poor results, so to save space, the examples
contrast the Taylor series and Lanczos methods with built-in tgamma:

```jq
def pad(n): tostring | . + (n - length) * " ";

"                 i:      gamma                lanczos              tgamma",
(range(1;11)
 | . / 3.0
 | "\(pad(18)): \(gamma|pad(18)) : \(gamma_by_lanczos|pad(18)) : \(tgamma)")
```

{{Out}}

```sh
$ jq -M -r -n -f Gamma_function_Stirling.jq
                 i:      gamma                lanczos              tgamma
0.3333333333333333: 2.6789385347077483 : 2.6789385347077483 : 2.678938534707748
0.6666666666666666: 1.3541179394264005 : 1.3541179394263998 : 1.3541179394264005
1                 : 1                  : 0.9999999999999998 : 1
1.3333333333333333: 0.8929795115692493 : 0.8929795115692494 : 0.8929795115692493
1.6666666666666667: 0.9027452929509336 : 0.9027452929509342 : 0.9027452929509336
2                 : 1                  : 1.0000000000000002 : 1
2.3333333333333335: 1.190639348758999  : 1.1906393487589995 : 1.190639348758999
2.6666666666666665: 1.5045754882515399 : 1.5045754882515576 : 1.5045754882515558
3                 : 1.9999999999939684 : 2.0000000000000013 : 2
3.3333333333333335: 2.778158479338573  : 2.778158480437665  : 2.7781584804376647
```



## Jsish

{{trans|Javascript}}

```javascript
#!/usr/bin/env jsish
/* Gamma function, in Jsish, using the Lanczos approximation */
function gamma(x) {
    var p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ];

    var g = 7;
    if (x < 0.5) {
        return Math.PI / (Math.sin(Math.PI * x) * gamma(1 - x));
    }

    x -= 1;
    var a = p[0];
    var t = x + g + 0.5;
    for (var i = 1; i < p.length; i++) {
        a += p[i] / (x + i);
    }

    return Math.sqrt(2 * Math.PI) * Math.pow(t, x + 0.5) * Math.exp(-t) * a;
}

if (Interp.conf('unitTest')) {
    for (var i=-5.5; i <= 5.5; i += 0.5) {
        printf('%2.1f %+e\n', i, gamma(i));
    }
}

/*
=!EXPECTSTART!=
-5.5 +1.091265e-02
-5.0 -4.275508e+13
-4.5 -6.001960e-02
-4.0 +2.672193e+14
-3.5 +2.700882e-01
-3.0 -1.425169e+15
-2.5 -9.453087e-01
-2.0 +6.413263e+15
-1.5 +2.363272e+00
-1.0 -2.565305e+16
-0.5 -3.544908e+00
0.0 +inf
0.5 +1.772454e+00
1.0 +1.000000e+00
1.5 +8.862269e-01
2.0 +1.000000e+00
2.5 +1.329340e+00
3.0 +2.000000e+00
3.5 +3.323351e+00
4.0 +6.000000e+00
4.5 +1.163173e+01
5.0 +2.400000e+01
5.5 +5.234278e+01
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U gammaFunction.jsi
-5.5 +1.091265e-02
-5.0 -4.275508e+13
-4.5 -6.001960e-02
-4.0 +2.672193e+14
-3.5 +2.700882e-01
-3.0 -1.425169e+15
-2.5 -9.453087e-01
-2.0 +6.413263e+15
-1.5 +2.363272e+00
-1.0 -2.565305e+16
-0.5 -3.544908e+00
0.0 +inf
0.5 +1.772454e+00
1.0 +1.000000e+00
1.5 +8.862269e-01
2.0 +1.000000e+00
2.5 +1.329340e+00
3.0 +2.000000e+00
3.5 +3.323351e+00
4.0 +6.000000e+00
4.5 +1.163173e+01
5.0 +2.400000e+01
5.5 +5.234278e+01

prompt$ jsish -u gammaFunction.jsi
[PASS] gammaFunction.jsi
```



## Julia

{{works with|Julia|0.6}}

'''Built-in function''':

```julia
@show gamma(1)
```


'''By adaptive Gauss-Kronrod integration''':

```julia
using QuadGK
gammaquad(t::Float64) = first(quadgk(x -> x ^ (t - 1) * exp(-x), zero(t), Inf, reltol = 100eps(t)))
@show gammaquad(1.0)
```


{{out}}

```txt
gamma(1) = 1.0
gammaquad(1.0) = 0.9999999999999999
```


{{works with|Julia|1.0}}
'''Library function''':

```julia
using SpecialFunctions
gamma(1/2) - sqrt(pi)
```


{{out}}

```txt
2.220446049250313e-16
```



## Kotlin


```scala
// version 1.0.6

fun gammaStirling(x: Double): Double = Math.sqrt(2.0 * Math.PI / x) * Math.pow(x / Math.E, x)

fun gammaLanczos(x: Double): Double {
    var xx = x
    val p = doubleArrayOf(
        0.99999999999980993,
      676.5203681218851,
    -1259.1392167224028,
      771.32342877765313,
     -176.61502916214059,
       12.507343278686905,
       -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7
    )
    val g = 7
    if (xx < 0.5) return Math.PI / (Math.sin(Math.PI * xx) * gammaLanczos(1.0 - xx))
    xx--
    var a = p[0]
    val t = xx + g + 0.5
    for (i in 1 until p.size) a += p[i] / (xx + i)
    return Math.sqrt(2.0 * Math.PI) * Math.pow(t, xx + 0.5) * Math.exp(-t) * a
}

fun main(args: Array<String>) {
    println(" x\tStirling\t\tLanczos\n")
    for (i in 1 .. 20) {
        val d = i / 10.0
        print("%4.2f\t".format(d))
        print("%17.15f\t".format(gammaStirling(d)))
        println("%17.15f".format(gammaLanczos(d)))
    }
}
```


{{out}}

```txt

 x      Stirling                Lanczos

0.10    5.697187148977170       9.513507698668736
0.20    3.325998424022393       4.590843711998803
0.30    2.362530036269620       2.991568987687590
0.40    1.841476335936235       2.218159543757687
0.50    1.520346901066281       1.772453850905516
0.60    1.307158857448356       1.489192248812818
0.70    1.159053292113920       1.298055332647558
0.80    1.053370968425609       1.164229713725304
0.90    0.977061507877695       1.068628702119319
1.00    0.922137008895789       1.000000000000000
1.10    0.883489953168704       0.951350769866874
1.20    0.857755335396591       0.918168742399761
1.30    0.842678259448392       0.897470696306278
1.40    0.836744548637082       0.887263817503076
1.50    0.838956552526496       0.886226925452759
1.60    0.848693242152574       0.893515349287691
1.70    0.865621471793884       0.908638732853292
1.80    0.889639635287995       0.931383770980243
1.90    0.920842721894229       0.961765831907388
2.00    0.959502175744492       1.000000000000000

```



## Limbo

{{trans|Go}}

A fairly straightforward port of the Go code.  (It could almost have been done with sed).  A few small differences are in the use of a tuple as a return value for the builtin gamma function, and we import a few functions from the math library so that we don't have to qualify them.


```Limbo
implement Lanczos7;

include "sys.m"; sys: Sys;
include "draw.m";
include "math.m"; math: Math;
	lgamma, exp, pow, sqrt: import math;

Lanczos7: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	math = load Math Math->PATH;
	# We ignore some floating point exceptions:
	math->FPcontrol(0, Math->OVFL|Math->UNFL);
	ns : list of real = -0.5 :: 0.1 :: 0.5 :: 1.0 :: 1.5 :: 2.0 :: 3.0 :: 10.0 :: 140.0 :: 170.0 :: nil;

	sys->print("%5s %24s %24s\n", "x", "math->lgamma", "lanczos7");
	while(ns != nil) {
		x := hd ns;
		ns = tl ns;
		# math->lgamma returns a tuple.
		(i, r) := lgamma(x);
		g := real i * exp(r);
		sys->print("%5.1f %24.16g %24.16g\n", x, g, lanczos7(x));
	}
}

lanczos7(z: real): real
{
	t := z + 6.5;
	x := 0.99999999999980993 +
		676.5203681218851/z -
		1259.1392167224028/(z+1.0) +
		771.32342877765313/(z+2.0) -
		176.61502916214059/(z+3.0) +
		12.507343278686905/(z+4.0) -
		0.13857109526572012/(z+5.0) +
		9.9843695780195716e-6/(z+6.0) +
		1.5056327351493116e-7/(z+7.0);
	return sqrt(2.0) * sqrt(Math->Pi) * pow(t, z - 0.5) * exp(-t) * x;
}

```


{{output}}

```txt
    x             math->lgamma                 lanczos7
 -0.5       -3.544907701811032       -3.544907701811089
  0.1        9.513507698668729         9.51350769866875
  0.5        1.772453850905516        1.772453850905516
  1.0                        1       0.9999999999999999
  1.5       0.8862269254527581       0.8862269254527587
  2.0                        1                        1
  3.0                        2        2.000000000000001
 10.0        362880.0000000005        362880.0000000015
140.0   9.615723196940553e+238   9.615723196940235e+238
170.0   4.269068009004526e+304                 Infinity

```



## Lua

Uses the [[wp:Reciprocal gamma function]] to calculate small values.

```lua
gamma, coeff, quad, qui, set = 0.577215664901, -0.65587807152056, -0.042002635033944, 0.16653861138228,	-0.042197734555571
function recigamma(z)
  return z + gamma * z^2 + coeff * z^3 + quad * z^4 + qui * z^5 + set * z^6
end

function gammafunc(z)
  if z == 1 then return 1
  elseif math.abs(z) <= 0.5 then return 1 / recigamma(z)
  else return (z - 1) * gammafunc(z-1)
  end
end
```



## M2000 Interpreter


```M2000 Interpreter

Module PrepareLambdaFunctions {
      Const e = 2.7182818284590452@
      Exp= Lambda e (x) -> e^x
      gammaStirling=lambda e (x As decimal)->Sqrt(2.0 * pi / x) * ((x / e) ^ x)
      Rad2Deg =Lambda pidivby180=pi/180 (RadAngle)->RadAngle / pidivby180
      Dim p(9)
      p(0)=0.99999999999980993@, 676.5203681218851@,   -1259.1392167224028@,  771.32342877765313@
      p(4)=-176.61502916214059@,  12.507343278686905@,  -0.13857109526572012@,  0.0000099843695780195716@
      p(8)=0.00000015056327351493116@
      gammaLanczos =Lambda p(), Rad2Deg, Exp (x As decimal) -> {
            Def Decimal a, t
            If x < 0.5 Then =pi / (Sin(Rad2Deg(pi * x)) *Lambda(1-x)) : Exit
            x -= 1@
            a=p(0)
            t = x + 7.5@
            For i= 1@ To 8@ {
                  a += p(i) / (x + i)
            }
             = Sqrt(2.0 * pi) * (t ^ (x + 0.5)) * Exp(-t) * a
      }
      Push gammaStirling, gammaLanczos
}
Call PrepareLambdaFunctions
Read gammaLanczos, gammaStirling
Font "Courier New"
Form 120, 40
document doc$="     χ                        Stirling                     Lanczos"+{
}
Print $(2,20),"x", "Stirling",@(55),"Lanczos", $(0)
Print
For d = 0.1 To 2 step 0.1
      Print   $("0.00"), d,
      Print  $("0.000000000000000"), gammaStirling(d),
      Print  $("0.0000000000000000000000000000"), gammaLanczos(d)
      doc$=format$("{0:-10}  {1:-30}   {2:-34}",str$(d,"0.00"), str$(gammaStirling(d),"0.000000000000000"), str$(gammaLanczos(d),"0.0000000000000000000000000000"))+{
      }
Next d
Print $("")
clipboard doc$

```

     χ                        Stirling                     Lanczos
      0.10               5.697187148977170       9.5135076986687024462927178610
      0.20               3.325998424022390       4.5908437119987955107204909409
      0.30               2.362530036269620       2.9915689876875914865114179656
      0.40               1.841476335936240       2.2181595437576816416854441034
      0.50               1.520346901066280       1.7724538509055147387430498835
      0.60               1.307158857448360       1.4891922488128208508983507496
      0.70               1.159053292113920       1.2980553326475564892857625396
      0.80               1.053370968425610       1.1642297137253055422419914101
      0.90               0.977061507877695       1.0686287021193206646594133376
      1.00               0.922137008895789       1.0000000000000007024882980221
      1.10               0.883489953168704       0.9513507698668745807357371716
      1.20               0.857755335396591       0.9181687423997605348002977483
      1.30               0.842678259448392       0.8974706963062785326402091223
      1.40               0.836744548637082       0.8872638175030748314253582066
      1.50               0.838956552526496       0.8862269254527587632845492097
      1.60               0.848693242152574       0.8935153492876912865293624528
      1.70               0.865621471793884       0.9086387328532921150064803085
      1.80               0.889639635287995       0.9313837709802428420608295699
      1.90               0.920842721894229       0.9617658319073891431109375442
      2.00               0.959502175744492       1.0000000000000015609456469406


## Maple

Built-in method that accepts any value.

```Maple
GAMMA(17/2);
GAMMA(7*I);
M := Matrix(2, 3, 'fill' = -3.6);
MTM:-gamma(M);
```

{{Out|Output}}

```txt
2027025*sqrt(Pi)*(1/256)
GAMMA(7*I)
Matrix(2, 3, [[.2468571430, .2468571430, .2468571430], [.2468571430, .2468571430, .2468571430]])
```


## Mathematica

This code shows the built-in method, which works for any value (positive, negative and complex numbers).

```mathematica
Gamma[x]
```

Output integers and half-integers (a space is multiplication in Mathematica):

```txt

1/2	Sqrt[pi]
1	1
3/2	Sqrt[pi]/2
2	1
5/2	(3 Sqrt[pi])/4
3	2
7/2	(15 Sqrt[pi])/8
4	6
9/2	(105 Sqrt[pi])/16
5	24
11/2	(945 Sqrt[pi])/32
6	120
13/2	(10395 Sqrt[pi])/64
7	720
15/2	(135135 Sqrt[pi])/128
8	5040
17/2	(2027025 Sqrt[pi])/256
9	40320
19/2	(34459425 Sqrt[pi])/512
10	362880

```

Output approximate numbers:

```txt

0.1	9.51351
0.2	4.59084
0.3	2.99157
0.4	2.21816
0.5	1.77245
0.6	1.48919
0.7	1.29806
0.8	1.16423
0.9	1.06863
1.	1.

```

Output complex numbers:

```txt

I	-0.15495-0.498016 I
2 I	0.00990244-0.075952 I
3 I	0.0112987-0.00643092 I
4 I	0.00173011+0.00157627 I
5 I	-0.000271704+0.000339933 I

```



## Maxima


```maxima
fpprec: 30$

gamma_coeff(n) := block([a: makelist(1, n)],
   a[2]: bfloat(%gamma),
   for k from 3 thru n do
      a[k]: bfloat((sum((-1)^j * zeta(j) * a[k - j], j, 2, k - 1) - a[2] * a[k - 1]) / (1 - k * a[1])),
   a)$

poleval(a, x) := block([y: 0],
   for k from length(a) thru 1 step -1 do
      y: y * x + a[k],
   y)$

gc: gamma_coeff(20)$

gamma_approx(x) := block([y: 1],
   while x > 2 do (x: x - 1, y: y * x),
   y / (poleval(gc, x - 1)))$

gamma_approx(12.3b0) - gamma(12.3b0);
/* -9.25224705314470500985141176997b-15 */
```


=={{header|Modula-3}}==
{{trans|Ada}}

```modula3
MODULE Gamma EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Extended, Style;

PROCEDURE Taylor(x: EXTENDED): EXTENDED =
  CONST a = ARRAY [0..29] OF EXTENDED {
    1.00000000000000000000X0, 0.57721566490153286061X0,
    -0.65587807152025388108X0, -0.04200263503409523553X0,
    0.16653861138229148950X0, -0.04219773455554433675X0,
    -0.00962197152787697356X0, 0.00721894324666309954X0,
    -0.00116516759185906511X0, -0.00021524167411495097X0,
    0.00012805028238811619X0, -0.00002013485478078824X0,
    -0.00000125049348214267X0, 0.00000113302723198170X0,
    -0.00000020563384169776X0, 0.00000000611609510448X0,
    0.00000000500200764447X0, -0.00000000118127457049X0,
    0.00000000010434267117X0, 0.00000000000778226344X0,
    -0.00000000000369680562X0, 0.00000000000051003703X0,
    -0.00000000000002058326X0, -0.00000000000000534812X0,
    0.00000000000000122678X0, -0.00000000000000011813X0,
    0.00000000000000000119X0, 0.00000000000000000141X0,
    -0.00000000000000000023X0, 0.00000000000000000002X0 };
  VAR y := x - 1.0X0;
      sum := a[LAST(a)];

  BEGIN
    FOR i := LAST(a) - 1 TO FIRST(a) BY -1 DO
      sum := sum * y + a[i];
    END;
    RETURN 1.0X0 / sum;
  END Taylor;

BEGIN
  FOR i := 1 TO 10 DO
    Put(Extended(Taylor(FLOAT(i, EXTENDED) / 3.0X0), style := Style.Sci) & "\n");
  END;
END Gamma.
```

{{out}}

```txt

 2.6789385347077490e+000
 1.3541179394264005e+000
 1.0000000000000000e+000
 8.9297951156924930e-001
 9.0274529295093360e-001
 1.0000000000000000e+000
 1.1906393487589992e+000
 1.5045754882515399e+000
 1.9999999999939684e+000
 2.7781584793385790e+000

```


=={{header|MK-61/52}}==

```txt

П9	9	П0	ИП9	ИП9	1	+	*	Вx	L0
05	1	+	П9	^	ln	1	-	*	ИП9
1	2	*	1/x	+	e^x	<->	/	2	пи
*	ИП9	/	КвКор	*	^	ВП	3	+	Вx
-	С/П

```



## Nim

{{trans|Ada}}

```nim
const a = [
 1.00000000000000000000,  0.57721566490153286061, -0.65587807152025388108,
-0.04200263503409523553,  0.16653861138229148950, -0.04219773455554433675,
-0.00962197152787697356,  0.00721894324666309954, -0.00116516759185906511,
-0.00021524167411495097,  0.00012805028238811619, -0.00002013485478078824,
-0.00000125049348214267,  0.00000113302723198170, -0.00000020563384169776,
 0.00000000611609510448,  0.00000000500200764447, -0.00000000118127457049,
 0.00000000010434267117,  0.00000000000778226344, -0.00000000000369680562,
 0.00000000000051003703, -0.00000000000002058326, -0.00000000000000534812,
 0.00000000000000122678, -0.00000000000000011813,  0.00000000000000000119,
 0.00000000000000000141, -0.00000000000000000023,  0.00000000000000000002 ]

proc gamma(x: float): float =
  let y = x.float - 1.0
  result = a[a.high]
  for n in countdown(high(a) - 1, low(a)):
    result = result * y + a[n]
  result = 1.0 / result

for i in 1..10:
  echo gamma(i.float / 3.0)
```

{{Out}}

```txt
2.678938534707748
1.3541179394264
1.0
0.8929795115692493
0.9027452929509336
1.0
1.190639348758999
1.50457548825154
1.999999999993968
2.778158479338573
```



## OCaml


```ocaml
let e = exp 1.
let pi = 4. *. atan 1.
let sqrttwopi = sqrt (2. *. pi)

module Lanczos = struct
  (* Lanczos method *)
  (* Coefficients used by the GNU Scientific Library *)
  let g = 7.
  let c = [|0.99999999999980993; 676.5203681218851; -1259.1392167224028;
	    771.32342877765313; -176.61502916214059; 12.507343278686905;
	    -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7|]

  let rec ag z d =
    if d = 0 then c.(0) +. ag z 1
    else if d < 8 then c.(d) /. (z +. float d) +. ag z (succ d)
    else c.(d) /. (z +. float d)

  let gamma z =
    let z = z -. 1. in
    let p = z +. g +. 0.5 in
    sqrttwopi *. p ** (z +. 0.5) *. exp (-. p) *. ag z 0
end

module Stirling = struct
  (* Stirling method *)
  let gamma z =
    sqrttwopi /. sqrt z *. (z /. e) ** z

end

module Stirling2 = struct
  (* Extended Stirling method seen in Abramowitz and Stegun *)
  let d = [|1./.12.; 1./.288.; -139./.51840.; -571./.2488320.|]

  let rec corr z x n =
    if n < Array.length d - 1 then d.(n) /. x +. corr z (x *. z) (succ n)
    else d.(n) /. x

  let gamma z = Stirling.gamma z *. (1. +. corr z z 0)
end

let mirror gma z =
  if z > 0.5 then gma z
  else pi /. sin (pi *. z) /. gma (1. -. z)

let _ =
  Printf.printf "z\t\tLanczos\t\tStirling\tStirling2\n";
  for i = 1 to 20 do
    let z = float i /. 10. in
    Printf.printf "%-10.8g\t%10.8e\t%10.8e\t%10.8e\n"
    		  z
		  (mirror Lanczos.gamma z)
		  (mirror Stirling.gamma z)
		  (mirror Stirling2.gamma z)
  done;
  for i = 1 to 7 do
    let z = 10. *. float i in
    Printf.printf "%-10.8g\t%10.8e\t%10.8e\t%10.8e\n"
    		  z
		  (Lanczos.gamma z)
		  (Stirling.gamma z)
		  (Stirling2.gamma z)
  done
```

{{out}}

```txt

z               Lanczos         Stirling        Stirling2
0.1             9.51350770e+00  1.04050843e+01  9.52104183e+00
0.2             4.59084371e+00  5.07399275e+00  4.59686230e+00
0.3             2.99156899e+00  3.35033954e+00  2.99844028e+00
0.4             2.21815954e+00  2.52705781e+00  2.22775889e+00
0.5             1.77245385e+00  2.06636568e+00  1.78839014e+00
0.6             1.48919225e+00  1.30715886e+00  1.48277536e+00
0.7             1.29805533e+00  1.15905329e+00  1.29508068e+00
0.8             1.16422971e+00  1.05337097e+00  1.16270541e+00
0.9             1.06862870e+00  9.77061508e-01  1.06778308e+00
1               1.00000000e+00  9.22137009e-01  9.99499469e-01
1.1             9.51350770e-01  8.83489953e-01  9.51037997e-01
1.2             9.18168742e-01  8.57755335e-01  9.17964058e-01
1.3             8.97470696e-01  8.42678259e-01  8.97331287e-01
1.4             8.87263818e-01  8.36744549e-01  8.87165485e-01
1.5             8.86226925e-01  8.38956553e-01  8.86155384e-01
1.6             8.93515349e-01  8.48693242e-01  8.93461840e-01
1.7             9.08638733e-01  8.65621472e-01  9.08597702e-01
1.8             9.31383771e-01  8.89639635e-01  9.31351590e-01
1.9             9.61765832e-01  9.20842722e-01  9.61740068e-01
2               1.00000000e+00  9.59502176e-01  9.99978981e-01
10              3.62880000e+05  3.59869562e+05  3.62879997e+05
20              1.21645100e+17  1.21139342e+17  1.21645100e+17
30              8.84176199e+30  8.81723653e+30  8.84176199e+30
40              2.03978821e+46  2.03554316e+46  2.03978821e+46
50              6.08281864e+62  6.07268919e+62  6.08281864e+62
60              1.38683119e+80  1.38490639e+80  1.38683119e+80
70              1.71122452e+98  1.70918858e+98  1.71122452e+98

```



## Octave


```octave
function g = lacz_gamma(a, cg=7)
  p = [ 0.99999999999980993, 676.5203681218851, -1259.1392167224028, \
        771.32342877765313, -176.61502916214059, 12.507343278686905, \
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7 ];
  x=a;
  if ( x < 0.5 )
    g = pi / ( sin(pi*x) * lacz_gamma(1.0-x) );
  else
    x = x - 1.0;
    t = p(1);
    for i=1:(cg+1)
      t = t + p(i+1)/(x+double(i));
    endfor
    w = x + double(cg) + 0.5;
    g = sqrt(2.0*pi) * w**(x+0.5) * exp(-w) * t;
  endif
endfunction


for i = 1:10
  printf("%f %f\n", gamma(i/3.0), lacz_gamma(i/3.0));
endfor
```

{{out}}

```txt
2.678939 2.678939
1.354118 1.354118
1.000000 1.000000
0.892980 0.892980
0.902745 0.902745
1.000000 1.000000
1.190639 1.190639
1.504575 1.504575
2.000000 2.000000
2.778158 2.778158
```

Which suggests that the built-in gamma uses the same approximation.


## Oforth



```oforth
import: math

[
   676.5203681218851,  -1259.1392167224028, 771.32342877765313,
  -176.61502916214059, 12.507343278686905, -0.13857109526572012,
   9.9843695780195716e-6, 1.5056327351493116e-7
] const: Gamma.Lanczos

: gamma(z)
| i t |
   z 0.5 < ifTrue: [ Pi dup z * sin 1.0 z - gamma * / return ]
   z 1.0 - ->z
   0.99999999999980993 Gamma.Lanczos size loop: i [ i Gamma.Lanczos at z i + / + ]
   z Gamma.Lanczos size + 0.5 - ->t
   2 Pi * sqrt *
   t z 0.5 + powf *
   t neg exp * ;
```


{{out}}

```txt
>20 seq apply(#[ 10.0 / dup . gamma .cr ])
0.1 9.51350769866874
0.2 4.5908437119988
0.3 2.99156898768759
0.4 2.21815954375769
0.5 1.77245385090552
0.6 1.48919224881282
0.7 1.29805533264756
0.8 1.1642297137253
0.9 1.06862870211932
1 1
1.1 0.951350769866874
1.2 0.918168742399761
1.3 0.897470696306277
1.4 0.887263817503076
1.5 0.886226925452759
1.6 0.893515349287691
1.7 0.908638732853292
1.8 0.931383770980243
1.9 0.961765831907388
2 1
```



## PARI/GP

===Built-in===

```parigp
gamma(x)
```


===Double-exponential integration===
<code>[[+oo],k]</code> means that the function approaches <math>+\infty</math> as <math>\exp(-kx).</math>

```parigp
Gamma(x)=intnum(t=0,[+oo,1],t^(x-1)/exp(t))
```



### Romberg integration


```parigp
Gamma(x)=intnumromb(t=0,9,t^(x-1)/exp(t),0)+intnumromb(t=9,max(x,99)^9,t^(x-1)/exp(t),2)
```



### Stirling approximation


```parigp
Stirling(x)=x--;sqrt(2*Pi*x)*(x/exp(1))^x
```



## Perl


```perl
use strict;
use warnings;
use constant pi => 4*atan2(1, 1);
use constant e  => exp(1);

# Normally would be:  use Math::MPFR
# but this will use it if it's installed and ignore otherwise
my $have_MPFR = eval { require Math::MPFR; Math::MPFR->import(); 1; };

sub Gamma {
    my $z = shift;
    my $method = shift // 'lanczos';
    if ($method eq 'lanczos') {
        use constant g => 9;
        $z <  .5 ?  pi / sin(pi * $z) / Gamma(1 - $z, $method) :
        sqrt(2* pi) *
        ($z + g - .5)**($z - .5) *
        exp(-($z + g - .5)) *
        do {
            my @coeff = qw{
            1.000000000000000174663
        5716.400188274341379136
      -14815.30426768413909044
       14291.49277657478554025
       -6348.160217641458813289
        1301.608286058321874105
        -108.1767053514369634679
           2.605696505611755827729
          -0.7423452510201416151527e-2
           0.5384136432509564062961e-7
          -0.4023533141268236372067e-8
            };
            my ($sum, $i) = (shift(@coeff), 0);
            $sum += $_ / ($z + $i++) for @coeff;
            $sum;
        }
    } elsif ($method eq 'taylor') {
        $z <  .5 ? Gamma($z+1, $method)/$z     :
        $z > 1.5 ? ($z-1)*Gamma($z-1, $method) :
	do {
	    my $s = 0; ($s *= $z-1) += $_ for qw{
	    0.00000000000000000002 -0.00000000000000000023 0.00000000000000000141
	    0.00000000000000000119 -0.00000000000000011813 0.00000000000000122678
	    -0.00000000000000534812 -0.00000000000002058326 0.00000000000051003703
	    -0.00000000000369680562 0.00000000000778226344 0.00000000010434267117
	    -0.00000000118127457049 0.00000000500200764447 0.00000000611609510448
	    -0.00000020563384169776 0.00000113302723198170 -0.00000125049348214267
	    -0.00002013485478078824 0.00012805028238811619 -0.00021524167411495097
	    -0.00116516759185906511 0.00721894324666309954 -0.00962197152787697356
	    -0.04219773455554433675 0.16653861138229148950 -0.04200263503409523553
	    -0.65587807152025388108 0.57721566490153286061 1.00000000000000000000
	    }; 1/$s;
	}
    } elsif ($method eq 'stirling') {
        no warnings qw(recursion);
        $z < 100 ? Gamma($z + 1, $method)/$z :
        sqrt(2*pi*$z)*($z/e + 1/(12*e*$z))**$z / $z;
    } elsif ($method eq 'MPFR') {
        my $result = Math::MPFR->new();
        Math::MPFR::Rmpfr_gamma($result, Math::MPFR->new($z), 0);
        $result;
    } else { die "unknown method '$method'" }
}

for my $method (qw(MPFR lanczos taylor stirling)) {
    next if $method eq 'MPFR' && !$have_MPFR;
    printf "%10s: ", $method;
    print join(' ', map { sprintf "%.12f", Gamma($_/3, $method) } 1 .. 10);
    print "\n";
}
```

{{out}}

```txt
      MPFR: 2.678938534708 1.354117939426 1.000000000000 0.892979511569 0.902745292951 1.000000000000 1.190639348759 1.504575488252 2.000000000000 2.778158480438
   lanczos: 2.678938534708 1.354117939426 1.000000000000 0.892979511569 0.902745292951 1.000000000000 1.190639348759 1.504575488252 2.000000000000 2.778158480438
    taylor: 2.678938534708 1.354117939426 1.000000000000 0.892979511569 0.902745292951 1.000000000000 1.190639348759 1.504575488252 2.000000000000 2.778158480438
  stirling: 2.678938532866 1.354117938504 0.999999999306 0.892979510955 0.902745292336 0.999999999306 1.190639347940 1.504575487227 1.999999998611 2.778158478527
```



## Perl 6


```perl6
sub Γ(\z) {
    constant g = 9;
    z < .5 ?? pi/ sin(pi * z) / Γ(1 - z) !!
    sqrt(2*pi) *
    (z + g - 1/2)**(z - 1/2) *
    exp(-(z + g - 1/2)) *
    [+] <
        1.000000000000000174663
     5716.400188274341379136
   -14815.30426768413909044
    14291.49277657478554025
    -6348.160217641458813289
     1301.608286058321874105
     -108.1767053514369634679
        2.605696505611755827729
       -0.7423452510201416151527e-2
        0.5384136432509564062961e-7
       -0.4023533141268236372067e-8
    > Z* 1, |map 1/(z + *), 0..*
}

say Γ($_) for 1/3, 2/3 ... 10/3;
```

{{out}}

```txt
2.67893853470775
1.3541179394264
1
0.892979511569248
0.902745292950934
1
1.190639348759
1.50457548825155
2
2.77815848043766
```



## Phix

{{trans|C}}

```Phix
sequence c = repeat(0,12)

function gamma(atom z)
    atom accm = c[1]
    if accm=0 then
        accm = sqrt(2*PI)
        c[1] = accm
        atom k1_factrl = 1  -- (k - 1)!*(-1)^k with 0!==1
        for k=2 to 12 do
            c[k] = exp(13-k)*power(13-k,k-1.5)/k1_factrl
            k1_factrl *= -(k-1)
        end for
    end if
    for k=2 to 12 do
        accm += c[k]/(z+k-1)
    end for
    accm *= exp(-(z+12))*power(z+12,z+0.5) -- Gamma(z+1)
    return accm/z
end function

procedure sq(atom x, atom mul)
atom p = x*mul
    printf(1,"%18.16g,%18.15g\n",{x,p*p})
end procedure

procedure si(atom x)
    printf(1,"%18.15f\n",{x})
end procedure

sq(gamma(-3/2),3/4)
sq(gamma(-1/2),-1/2)
sq(gamma(1/2),1)
si(gamma(1))
sq(gamma(3/2),2)
si(gamma(2))
sq(gamma(5/2),4/3)
si(gamma(3))
sq(gamma(7/2),8/15)
si(gamma(4))
```

{{out}}

```txt

 2.363271801207354,  3.14159265358979
-3.544907701811032,  3.14159265358979
 1.772453850905515,  3.14159265358979
 1.000000000000001
0.8862269254527643,  3.14159265358984
 1.000000000000010
 1.329340388179146,  3.14159265358984
 2.000000000000024
 3.323350970447942,  3.14159265358998
 6.000000000000175

```


###  mpfr version

Above translated to mpfr, with higher accuracy and more iterations as per REXX, and compared against the builtin.
{{libheader|mpfr}}

```Phix
include mpfr.e
mpfr_set_default_prec(-87) -- 87 decimal places.

sequence c = mpfr_inits(40)

function gamma(atom z)
    mpfr accm = c[1]
    if mpfr_cmp_si(accm,0)=0 then
        -- c[1] := sqrt(2*PI)
        mpfr_const_pi(accm)
        mpfr_mul_si(accm,accm,2)
        mpfr_sqrt(accm,accm)
        -- k1_factrl = (k - 1)!*(-1)^k with 0!==1
        mpfr k1_factrl = mpfr_init(1),
             tmk = mpfr_init(),
             p = mpfr_init()
        for k=2 to length(c) do
            -- c[k] = exp(13-k)*power(13-k,k-1.5)/k1_factrl
            mpfr_set_si(tmk,length(c)+1-k)
            mpfr_exp(c[k],tmk)
            mpfr_set_d(p,k-1.5)
            mpfr_pow(p,tmk,p)
            mpfr_div(p,p,k1_factrl)
            mpfr_mul(c[k],c[k],p)
            -- k1_factrl *= -(k-1)
            mpfr_mul_si(k1_factrl,k1_factrl,-(k-1))
        end for
    end if
    accm = mpfr_init_set(accm)
    for k=2 to length(c) do
        -- accm += c[k]/(z+k-1)
        mpfr ck = mpfr_init_set(c[k]),
             zk = mpfr_init(z+k-1)
        mpfr_div(ck,ck,zk)
        mpfr_add(accm,accm,ck)
    end for
    atom zc = z+length(c)
    -- accm *= exp(-zc)*power(zc,z+0.5) -- Gamma(z+1)
    mpfr ez = mpfr_init(-zc),
         p = mpfr_init(zc),
         zh = mpfr_init(z+0.5)
    mpfr_exp(ez,ez)
    mpfr_pow(p,p,zh)
    mpfr_mul(accm,accm,ez)
    mpfr_mul(accm,accm,p)
    -- return accm/z
    mpfr_set_d(ez,z)
    mpfr_div(accm,accm,ez)
    return accm
end function

function gamma2(atom z)
    mpfr r = mpfr_init(z)
    mpfr_gamma(r,r)
    return r
end function

constant FMT = "%43.40Rf"

procedure sq(mpfr x, integer n, d=1)
    mpfr p = mpfr_init_set(x)
    mpfr_mul_si(p,p,n)
    mpfr_div_si(p,p,d)
    mpfr_mul(p,p,p)
    string xs = mpfr_sprintf(FMT,x),
           ps = mpfr_sprintf(FMT,p)
    printf(1,"%s,%s\n",{xs,ps})
end procedure

procedure si(mpfr x)
    string xs = mpfr_sprintf(FMT,x)
    printf(1,"%s\n",trim_tail(xs,".0"))
end procedure

sq(gamma(-3/2),3,4)
sq(gamma(-1/2),-1,2)
sq(gamma(1/2),1)
si(gamma(1))
sq(gamma(3/2),2)
si(gamma(2))
sq(gamma(5/2),4,3)
si(gamma(3))
sq(gamma(7/2),8,15)
si(gamma(4))
puts(1,"mpfr_gamma():\n")
sq(gamma2(-3/2),3,4)
sq(gamma2(-1/2),-1,2)
sq(gamma2(1/2),1)
si(gamma2(1))
sq(gamma2(3/2),2)
si(gamma2(2))
sq(gamma2(5/2),4,3)
si(gamma2(3))
sq(gamma2(7/2),8,15)
si(gamma2(4))
```

{{out}}

```txt

 2.3632718012073547030642233111215269103967, 3.1415926535897932384626433832795028841972
-3.5449077018110320545963349666822903655951, 3.1415926535897932384626433832795028841972
 1.7724538509055160272981674833411451827975, 3.1415926535897932384626433832795028841972
 1
 0.8862269254527580136490837416705725913988, 3.1415926535897932384626433832795028841972
 1
 1.3293403881791370204736256125058588870982, 3.1415926535897932384626433832795028841972
 2
 3.3233509704478425511840640312646472177454, 3.1415926535897932384626433832795028841972
 6
mpfr_gamma():
 2.3632718012073547030642233111215269103967, 3.1415926535897932384626433832795028841972
-3.5449077018110320545963349666822903655951, 3.1415926535897932384626433832795028841972
 1.7724538509055160272981674833411451827975, 3.1415926535897932384626433832795028841972
 1
 0.8862269254527580136490837416705725913988, 3.1415926535897932384626433832795028841972
 1
 1.3293403881791370204736256125058588870982, 3.1415926535897932384626433832795028841972
 2
 3.3233509704478425511840640312646472177454, 3.1415926535897932384626433832795028841972
 6

```



## PicoLisp

{{trans|Ada}}

```PicoLisp
(scl 28)

(de *A
   ~(flip
      (1.00000000000000000000  0.57721566490153286061 -0.65587807152025388108
      -0.04200263503409523553  0.16653861138229148950 -0.04219773455554433675
      -0.00962197152787697356  0.00721894324666309954 -0.00116516759185906511
      -0.00021524167411495097  0.00012805028238811619 -0.00002013485478078824
      -0.00000125049348214267  0.00000113302723198170 -0.00000020563384169776
       0.00000000611609510448  0.00000000500200764447 -0.00000000118127457049
       0.00000000010434267117  0.00000000000778226344 -0.00000000000369680562
       0.00000000000051003703 -0.00000000000002058326 -0.00000000000000534812
       0.00000000000000122678 -0.00000000000000011813  0.00000000000000000119
       0.00000000000000000141 -0.00000000000000000023  0.00000000000000000002 ) ) )

(de gamma (X)
   (let (Y (- X 1.0)  Sum (car *A))
      (for A (cdr *A)
         (setq Sum (+ A (*/ Sum Y 1.0))) )
      (*/ 1.0 1.0 Sum) ) )
```

{{out}}

```txt
: (for I (range 1 10)
   (prinl (round (gamma (*/ I 1.0 3)) 14)) )
2.67893853470775
1.35411793942640
1.00000000000000
0.89297951156925
0.90274529295093
1.00000000000000
1.19063934875900
1.50457548825154
1.99999999999397
2.77815847933858
```



## PL/I


```PL/I
/* From Rosetta Fortran */
test: procedure options (main);

  declare i fixed binary;

  on underflow ;

  put skip list ('Lanczos', 'Builtin' );
  do i = 1 to 10;
     put skip list (lanczos_gamma(i/3.0q0), gamma(i/3.0q0) );
  end;


lanczos_gamma: procedure (a) returns (float (18)) recursive;
    declare a float (18);
    declare pi float (18) value (3.14159265358979324E0);
    declare cg fixed binary initial ( 7 );

    /* these precomputed values are taken by the sample code in Wikipedia, */
    /* and the sample itself takes them from the GNU Scientific Library */
    declare p(0:8) float (18) static initial
         ( 0.99999999999980993e0, 676.5203681218851e0, -1259.1392167224028e0,
         771.32342877765313e0, -176.61502916214059e0, 12.507343278686905e0,
         -0.13857109526572012e0, 9.9843695780195716e-6, 1.5056327351493116e-7 );

    declare ( t, w, x ) float (18);
    declare i fixed binary;

    x = a;

    if x < 0.5 then
       return ( pi / ( sin(pi*x) * lanczos_gamma(1.0-x) ) );
    else
       do;
          x = x - 1.0;
          t = p(0);
          do i = 1 to cg+2;
             t = t + p(i)/(x+i);
          end;
          w = x + float(cg) + 0.5;
          return ( sqrt(2*pi) * w**(x+0.5) * exp(-w) * t );
       end;
  end lanczos_gamma;

end test;
```

{{out}}

```txt

Lanczos                 Builtin
 2.67893853470774706E+0000           2.678938534707747630E+0000
 1.35411793942640071E+0000           1.354117939426400420E+0000
 1.00000000000000021E+0000           1.000000000000000000E+0000
 8.92979511569249470E-0001           8.929795115692492110E-0001
 9.02745292950933961E-0001           9.027452929509336110E-0001
 1.00000000000000048E+0000           1.000000000000000000E+0000
 1.19063934875899964E+0000           1.190639348758998950E+0000
 1.50457548825155704E+0000           1.504575488251556020E+0000
 2.00000000000000154E+0000           2.000000000000000000E+0000
 2.77815848043766660E+0000           2.778158480437664210E+0000

```



## PowerShell

I would download the Math.NET Numerics dll(s).  Documentation and download at: http://cyber-defense.sans.org/blog/2015/06/27/powershell-for-math-net-numerics/comment-page-1/

```PowerShell

Add-Type -Path "C:\Program Files (x86)\Math\MathNet.Numerics.3.12.0\lib\net40\MathNet.Numerics.dll"

1..20 | ForEach-Object {[MathNet.Numerics.SpecialFunctions]::Gamma($_ / 10)}

```


{{Out}}

```txt

9.51350769866874
4.5908437119988
2.99156898768759
2.21815954375769
1.77245385090552
1.48919224881282
1.29805533264756
1.1642297137253
1.06862870211932
1
0.951350769866874
0.918168742399759
0.897470696306277
0.887263817503075
0.88622692545276
0.89351534928769
0.908638732853289
0.931383770980245
0.961765831907388
1

```



## PureBasic

Below is PureBasic code for:
*    Complete Gamma function
*    Natural Logarithm of the Complete Gamma function
*    Factorial function

```PureBasic
Procedure.d Gamma(x.d) ; AKJ  01-May-10
; Complete Gamma function for x>0 and x<2 (approx)
; Extended outside this range via: Gamma(x+1) = x*Gamma(x)
; Based on http://rosettacode.org/wiki/Gamma_function  [Ada]
Protected Dim A.d(28)
A(0) = 1.0
A(1) = 0.5772156649015328606
A(2) =-0.6558780715202538811
A(3) =-0.0420026350340952355
A(4) = 0.1665386113822914895
A(5) =-0.0421977345555443368 ; was ...33675
A(6) =-0.0096219715278769736
A(7) = 0.0072189432466630995
A(8) =-0.0011651675918590651
A(9) =-0.0002152416741149510
A(10) = 0.0001280502823881162
A(11) =-0.0000201348547807882
A(12) =-0.0000012504934821427
A(13) = 0.0000011330272319817
A(14) =-0.0000002056338416978
A(15) = 0.0000000061160951045
A(16) = 0.0000000050020076445
A(17) =-0.0000000011812745705
A(18) = 0.0000000001043426712
A(19) = 0.0000000000077822634
A(20) =-0.0000000000036968056
A(21) = 0.0000000000005100370
A(22) =-0.0000000000000205833
A(23) =-0.0000000000000053481
A(24) = 0.0000000000000012268
A(25) =-0.0000000000000001181
A(26) = 0.0000000000000000012
A(27) = 0.0000000000000000014
A(28) =-0.0000000000000000002
;A(29) = 0.00000000000000000002
Protected y.d, Prod.d, Sum.d, N
If x<=0.0: ProcedureReturn 0.0: EndIf ; Error
y = x-1.0: Prod = 1.0
While y>=1.0
  Prod*y: y-1.0 ; Recurse using Gamma(x+1) = x*Gamma(x)
Wend
Sum= A(28)
For N = 27 To 0 Step -1
  Sum*y+A(N)
Next N
If Sum=0.0: ProcedureReturn Infinity(): EndIf
ProcedureReturn Prod / Sum
EndProcedure

Procedure.d GammLn(x.d) ; AKJ  01-May-10
; Returns Ln(Gamma()) or 0 on error
; Based on Numerical Recipes gamma.h
Protected j, tmp.d, y.d, ser.d
Protected Dim cof.d(13)
cof(0) = 57.1562356658629235
cof(1) = -59.5979603554754912
cof(2) = 14.1360979747417471
cof(3) = -0.491913816097620199
cof(4) = 0.339946499848118887e-4
cof(5) = 0.465236289270485756e-4
cof(6) = -0.983744753048795646e-4
cof(7) = 0.158088703224912494e-3
cof(8) = -0.210264441724104883e-3
cof(9) = 0.217439618115212643e-3
cof(10) = -0.164318106536763890e-3
cof(11) = 0.844182239838527433e-4
cof(12) = -0.261908384015814087e-4
cof(13) = 0.368991826595316234e-5
If x<=0: ProcedureReturn 0: EndIf ; Bad argument
y = x
tmp = x+5.2421875
tmp = (x+0.5)*Log(tmp)-tmp
ser = 0.999999999999997092
For j=0 To 13
  y + 1: ser + cof(j)/y
Next j
ProcedureReturn tmp+Log(2.5066282746310005*ser/x)
EndProcedure

Procedure Factorial(x) ; AKJ  01-May-10
  ProcedureReturn Gamma(x+1)
EndProcedure
```

;Examples

```PureBasic
Debug "Gamma()"
For i = 12 To 15
  Debug StrD(i/3.0, 3)+"   "+StrD(Gamma(i/3.0))
Next i
Debug ""
Debug "Ln(Gamma(5.0)) = "+StrD(GammLn(5.0), 16) ; Ln(24)
Debug ""
Debug "Factorial 6 = "+StrD(Factorial(6), 0) ; 72
```

{{out}}

```txt
[Debug] Gamma():
[Debug] 4.000   6.0000000000
[Debug] 4.333   9.2605282681
[Debug] 4.667   14.7114047740
[Debug] 5.000   24.0000000000
[Debug]
[Debug] Ln(Gamma(5.0)) = 3.1780538303479458
[Debug]
[Debug] Factorial 6 = 720
```



## Python


### Procedural

{{trans|Ada}}

```python
_a =    ( 1.00000000000000000000, 0.57721566490153286061, -0.65587807152025388108,
         -0.04200263503409523553, 0.16653861138229148950, -0.04219773455554433675,
         -0.00962197152787697356, 0.00721894324666309954, -0.00116516759185906511,
         -0.00021524167411495097, 0.00012805028238811619, -0.00002013485478078824,
         -0.00000125049348214267, 0.00000113302723198170, -0.00000020563384169776,
          0.00000000611609510448, 0.00000000500200764447, -0.00000000118127457049,
          0.00000000010434267117, 0.00000000000778226344, -0.00000000000369680562,
          0.00000000000051003703, -0.00000000000002058326, -0.00000000000000534812,
          0.00000000000000122678, -0.00000000000000011813, 0.00000000000000000119,
          0.00000000000000000141, -0.00000000000000000023, 0.00000000000000000002
       )
def gamma (x):
   y  = float(x) - 1.0;
   sm = _a[-1];
   for an in _a[-2::-1]:
      sm = sm * y + an;
   return 1.0 / sm;


if __name__ == '__main__':
    for i in range(1,11):
        print "  %20.14e" % gamma(i/3.0)

```

{{out}}

```txt
  2.67893853470775e+00
  1.35411793942640e+00
  1.00000000000000e+00
  8.92979511569249e-01
  9.02745292950934e-01
  1.00000000000000e+00
  1.19063934875900e+00
  1.50457548825154e+00
  1.99999999999397e+00
  2.77815847933857e+00
```



### Functional

In terms of fold/reduce:
{{Works with|Python|3.7}}

```python
'''Gamma function'''

from functools import reduce


# gamma_ :: [Float] -> Float -> Float
def gamma_(tbl):
    '''Gamma function.'''
    def go(x):
        y = float(x) - 1.0
        return 1.0 / reduce(
            lambda a, x: a * y + x,
            tbl[-2::-1],
            tbl[-1]
        )
    return lambda x: go(x)


# TBL :: [Float]
TBL = [
    1.00000000000000000000, 0.57721566490153286061,
    -0.65587807152025388108, -0.04200263503409523553,
    0.16653861138229148950, -0.04219773455554433675,
    -0.00962197152787697356, 0.00721894324666309954,
    -0.00116516759185906511, -0.00021524167411495097,
    0.00012805028238811619, -0.00002013485478078824,
    -0.00000125049348214267, 0.00000113302723198170,
    -0.00000020563384169776, 0.00000000611609510448,
    0.00000000500200764447, -0.00000000118127457049,
    0.00000000010434267117, 0.00000000000778226344,
    -0.00000000000369680562, 0.00000000000051003703,
    -0.00000000000002058326, -0.00000000000000534812,
    0.00000000000000122678, -0.00000000000000011813,
    0.00000000000000000119, 0.00000000000000000141,
    -0.00000000000000000023, 0.00000000000000000002
]


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Gamma function over a range of values.'''

    gamma = gamma_(TBL)
    print(
        fTable(' i -> gamma(i/3):\n')(repr)(lambda x: "%0.7e" % x)(
            lambda x: gamma(x / 3.0)
        )(enumFromTo(1)(10))
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# FORMATTING -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
 i -> gamma(i/3):

 1 -> 2.6789385e+00
 2 -> 1.3541179e+00
 3 -> 1.0000000e+00
 4 -> 8.9297951e-01
 5 -> 9.0274529e-01
 6 -> 1.0000000e+00
 7 -> 1.1906393e+00
 8 -> 1.5045755e+00
 9 -> 2.0000000e+00
10 -> 2.7781585e+00
```



## R

Lanczos' approximation is loosely converted from the Octave code.
{{trans|Octave}}

```r
stirling <- function(z) sqrt(2*pi/z) * (exp(-1)*z)^z

nemes <- function(z) sqrt(2*pi/z) * (exp(-1)*(z + (12*z - (10*z)^-1)^-1))^z

lanczos <- function(z)
{
   if(length(z) > 1)
   {
      sapply(z, lanczos)
   } else
   {
     g <- 7
      p <- c(0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)
      z <- as.complex(z)
      if(Re(z) < 0.5)
      {
         pi / (sin(pi*z) * lanczos(1-z))
      } else
      {
         z <- z - 1
         x <- p[1] + sum(p[-1]/seq.int(z+1, z+g+1))
         tt <- z + g + 0.5
         sqrt(2*pi) * tt^(z+0.5) * exp(-tt) * x
      }
   }
}

spouge <- function(z, a=49)
{
   if(length(z) > 1)
   {
      sapply(z, spouge)
   } else
   {
      z <- z-1
      k <- seq.int(1, a-1)
      ck <- rep(c(1, -1), len=a-1) / factorial(k-1) * (a-k)^(k-0.5) * exp(a-k)
      (z + a)^(z+0.5) * exp(-z - a) * (sqrt(2*pi) + sum(ck/(z+k)))
   }
}

# Checks
z <- (1:10)/3
all.equal(gamma(z), stirling(z))             # Mean relative difference: 0.07181942
all.equal(gamma(z), nemes(z))                # Mean relative difference: 0.003460549
all.equal(as.complex(gamma(z)), lanczos(z))  # TRUE
all.equal(gamma(z), spouge(z))               # TRUE
data.frame(z=z, stirling=stirling(z), nemes=nemes(z), lanczos=lanczos(z), spouge=spouge(z), builtin=gamma(z))
```

{{out}}
           z  stirling     nemes      lanczos    spouge   builtin
 1  0.3333333 2.1569760 2.6290752 2.6789385+0i 2.6789385 2.6789385
 2  0.6666667 1.2028507 1.3515736 1.3541179+0i 1.3541179 1.3541179
 3  1.0000000 0.9221370 0.9996275 1.0000000+0i 1.0000000 1.0000000
 4  1.3333333 0.8397427 0.8928835 0.8929795+0i 0.8929795 0.8929795
 5  1.6666667 0.8591902 0.9027098 0.9027453+0i 0.9027453 0.9027453
 6  2.0000000 0.9595022 0.9999831 1.0000000+0i 1.0000000 1.0000000
 7  2.3333333 1.1491064 1.1906296 1.1906393+0i 1.1906393 1.1906393
 8  2.6666667 1.4584904 1.5045690 1.5045755+0i 1.5045755 1.5045755
 9  3.0000000 1.9454032 1.9999951 2.0000000+0i 2.0000000 2.0000000
 10 3.3333333 2.7097638 2.7781544 2.7781585+0i 2.7781585 2.7781585


## Racket


```Racket
#lang racket
(define (gamma number)
  (if (> 1/2 number)
      (/ pi (* (sin (* pi number))
               (gamma (- 1.0 number))))
      (let ((n (sub1 number))
            (c '(0.99999999999980993 676.5203681218851 -1259.1392167224028
                 771.32342877765313 -176.61502916214059 12.507343278686905
	             -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7)))
        (* (sqrt (* pi 2))
           (expt (+ n 7 0.5) (+ n 0.5))
           (exp (- (+ n 7 0.5)))
           (+ (car c)
              (apply +
                (for/list ((i (in-range (length (cdr c)))) (x (in-list (cdr c))))
                  (/ x (+ 1 n i)))))))))

(map gamma '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
;->
;'(9.513507698668736
;  4.590843711998802
;  2.9915689876875904
;  2.218159543757687
;  1.7724538509055159
;  1.489192248812818
;  1.2980553326475577
;  1.1642297137253037
;  1.068628702119319
;  1.0)
```



## REXX

===Taylor series, 80-digit coefficients===
This version uses a Taylor series with 80-digits coefficients with much more accuracy.

As a result, the gamma value for   <big> ½ </big>   is now   25   decimal digits more accurate than the previous version

(which only used   20   digit coefficients).

Note:   The Taylor series isn't much good above values of   <big>'''6½'''</big>.

```rexx
/*REXX program calculates GAMMA using the Taylor series coefficients; ≈80 decimal digits*/
                            /*The GAMMA function symbol is the Greek capital letter:  Γ */
numeric digits 90                                /*be able to handle extended precision.*/
parse arg LO HI .                                /*allow specification of gamma arg/args*/
                                                 /* [↓]  either show a range or a ···   */
        do j=word(LO 1, 1)  to word(HI LO 9, 1)  /*              ··· single gamma value.*/
        say 'gamma('j") ="  gamma(j)             /*compute gamma of J and display value.*/
        end   /*j*/                              /* [↑]  default LO is one;  HI is nine.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gamma:  procedure;   parse arg x;    xm=x-1;    sum=0
                                /*coefficients thanks to: Arne Fransén & Staffan Wrigge.*/
 #.1 =  1                       /* [↓]    #.2   is the   Euler-Mascheroni  constant.    */
 #.2 =  0.57721566490153286060651209008240243104215933593992359880576723488486772677766467
 #.3 = -0.65587807152025388107701951514539048127976638047858434729236244568387083835372210
 #.4 = -0.04200263503409523552900393487542981871139450040110609352206581297618009687597599
 #.5 =  0.16653861138229148950170079510210523571778150224717434057046890317899386605647425
 #.6 = -0.04219773455554433674820830128918739130165268418982248637691887327545901118558900
 #.7 = -0.00962197152787697356211492167234819897536294225211300210513886262731167351446074
 #.8 =  0.00721894324666309954239501034044657270990480088023831800109478117362259497415854
 #.9 = -0.00116516759185906511211397108401838866680933379538405744340750527562002584816653
#.10 = -0.00021524167411495097281572996305364780647824192337833875035026748908563946371678
#.11 =  0.00012805028238811618615319862632816432339489209969367721490054583804120355204347
#.12 = -0.00002013485478078823865568939142102181838229483329797911526116267090822918618897
#.13 = -0.00000125049348214267065734535947383309224232265562115395981534992315749121245561
#.14 =  0.00000113302723198169588237412962033074494332400483862107565429550539546040842730
#.15 = -0.00000020563384169776071034501541300205728365125790262933794534683172533245680371
#.16 =  0.00000000611609510448141581786249868285534286727586571971232086732402927723507435
#.17 =  0.00000000500200764446922293005566504805999130304461274249448171895337887737472132
#.18 = -0.00000000118127457048702014458812656543650557773875950493258759096189263169643391
#.19 =  0.00000000010434267116911005104915403323122501914007098231258121210871073927347588
#.20 =  0.00000000000778226343990507125404993731136077722606808618139293881943550732692987
#.21 = -0.00000000000369680561864220570818781587808576623657096345136099513648454655443000
#.22 =  0.00000000000051003702874544759790154813228632318027268860697076321173501048565735
#.23 = -0.00000000000002058326053566506783222429544855237419746091080810147188058196444349
#.24 = -0.00000000000000534812253942301798237001731872793994898971547812068211168095493211
#.25 =  0.00000000000000122677862823826079015889384662242242816545575045632136601135999606
#.26 = -0.00000000000000011812593016974587695137645868422978312115572918048478798375081233
#.27 =  0.00000000000000000118669225475160033257977724292867407108849407966482711074006109
#.28 =  0.00000000000000000141238065531803178155580394756670903708635075033452562564122263
#.29 = -0.00000000000000000022987456844353702065924785806336992602845059314190367014889830
#.30 =  0.00000000000000000001714406321927337433383963370267257066812656062517433174649858
#.31 =  0.00000000000000000000013373517304936931148647813951222680228750594717618947898583
#.32 = -0.00000000000000000000020542335517666727893250253513557337960820379352387364127301
#.33 =  0.00000000000000000000002736030048607999844831509904330982014865311695836363370165
#.34 = -0.00000000000000000000000173235644591051663905742845156477979906974910879499841377
#.35 = -0.00000000000000000000000002360619024499287287343450735427531007926413552145370486
#.36 =  0.00000000000000000000000001864982941717294430718413161878666898945868429073668232
#.37 = -0.00000000000000000000000000221809562420719720439971691362686037973177950067567580
#.38 =  0.00000000000000000000000000012977819749479936688244144863305941656194998646391332
#.39 =  0.00000000000000000000000000000118069747496652840622274541550997151855968463784158
#.40 = -0.00000000000000000000000000000112458434927708809029365467426143951211941179558301
#.41 =  0.00000000000000000000000000000012770851751408662039902066777511246477487720656005
#.42 = -0.00000000000000000000000000000000739145116961514082346128933010855282371056899245
#.43 =  0.00000000000000000000000000000000001134750257554215760954165259469306393008612196
#.44 =  0.00000000000000000000000000000000004639134641058722029944804907952228463057968680
#.45 = -0.00000000000000000000000000000000000534733681843919887507741819670989332090488591
#.46 =  0.00000000000000000000000000000000000032079959236133526228612372790827943910901464
#.47 = -0.00000000000000000000000000000000000000444582973655075688210159035212464363740144
#.48 = -0.00000000000000000000000000000000000000131117451888198871290105849438992219023663
#.49 =  0.00000000000000000000000000000000000000016470333525438138868182593279063941453996
#.50 = -0.00000000000000000000000000000000000000001056233178503581218600561071538285049997
#.51 =  0.00000000000000000000000000000000000000000026784429826430494783549630718908519485
#.52 =  0.00000000000000000000000000000000000000000002424715494851782689673032938370921241
#=52;                           do k=#  by -1  for #
                                sum=sum*xm  +  #.k
                                end   /*k*/
return 1/sum
```

{{out|output|text=  when using the input of:     <tt> 0.5 </tt>}}

```txt

gamma(0.5) = 1.77245385090551602729816748334114518279754945612238712821380509003635917689651032047826593

```


<!--
Note that:   <span style="font-family:serif;"> <b><big><big>&Gamma;(&frac12;)</big></big> = <big><big> &radic;<math>\overline{\pi}</math> </big></big></b> =
   won't display properly with Chrome.
-->

Note that:   <span style="font-family:serif;"> <b><big><big>&Gamma;(&frac12;)</big></big> = <big><big> &radic;<math>\pi</math> </big></big></b> =

1.77245 38509 05516 02729 81674 83341 14518 27975 49456 12238 71282 1380{{overline|7}} 78985 29112 84591 03218 13749 50656 73854 46654 16226 82362 +
</span>


to 110 digits past the decimal point,   the vinculum (overbar) marks the   ''difference digit''   from the computed value (by this REXX program) of   gamma(<b><big>½</big></b>).



===Spouge's approximation, using 87 digit coefficients===
{{trans|Phix}}
{{trans|C}}

This REXX version is a translation of   '''Phix'''   but with more (decimal digits) precision and more ''steps''.

Many of the "normal" high-level mathematical functions aren't available in REXX, so some of them (RYO) are included here.

```rexx
/*REXX program calculates the gamma function using Spouge's approximation with 87 digits*/
e=2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138
numeric digits length(e)   -  length(.)          /*use the number of decimal digits in E*/
c.=  0
# = 40                                           /*#:  the number of steps in GAMMA func*/
                    call sq gamma(-3/2),  3/4
                    call sq gamma(-1/2), -1/2
                    call sq gamma( 1/2),   1
                    call si gamma(  1 )
                    call sq gamma( 3/2),   2
                    call si gamma(  2 )
                    call sq gamma( 5/2),  4/3
                    call si gamma(  3 )
                    call sq gamma( 7/2),  8/15
                    call si gamma(  4 )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gamma: procedure expose c. e #; parse arg z;         #p= # + 1
       accm = c.1
       if accm==0  then do;  accm= sqrt( 2*pi() )
                             c.1 = accm
                             kfact = 1
                                         do k=2  to #
                                         c.k= exp(#p-k) * pow(#p-k, k-1.5) / kfact
                                         kfact = kfact  *  -(k-1)
                                         end   /*k*/
                        end

           do j=2  to #;   accm = accm   +   c.j / (z+j-1)
           end   /*k*/

       return (accm * exp(-(z+#)) * pow(z+#, z+0.5) ) / z
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi: return 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348
fmt:    parse arg n,p,a;  _= format(n,p,a);  L= length(_);      return left( strip0(_), L)
isInt:  return datatype(arg(1), 'W')                      /*is the argument an integer? */
sq:     procedure expose #; parse arg x,mu; say fmt(x,9,#)  fmt((x*mu)**2,9,#);   return
si:     procedure expose #; parse arg x;    say fmt(x,9,#);                       return
strip0: procedure; arg _; if pos(., _)\==0  then _= strip(strip(_,'T',0),'T',.);  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
exp: procedure expose e; arg x; ix= x%1; if abs(x-ix)>.5  then ix=ix+sign(x); x= x-ix; z=1
     _=1;  w=1;    do j=1;  _= _*x/j;    z= (z+_)/1;      if z==w  then leave;         w=z
                   end  /*j*/;           if z\==0  then z= e**ix * z;             return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
ln:     procedure; parse arg x; call e; ig= x>1.5; is= 1-2*(ig\==1); ii= 0; xx= x
          do while ig & xx>1.5 | \ig & xx<.5; _=e
        do k=-1; iz=xx*_**-is; if k>=0&(ig&iz<1|\ig&iz>.5)  then leave; _=_*_; izz=iz; end
        xx= izz; ii= ii+is*2**k;   end   /*while*/;      x= x*e**-ii-1;  z=0;  _= -1;  p=z
          do k=1; _=-_*x;  z=z+_/k;  if z=p  then leave;  p=z; end;  /*k*/;    return z+ii
/*──────────────────────────────────────────────────────────────────────────────────────*/
pow:    procedure; parse arg x,y;  if y=0  then return 1;  if x=0  then return 0
        if isInt(y)  then return x**y;          if isInt(1/y)  then return root(x, 1/y)
        if abs(y//1)=.5  then return sqrt(x)**sign(y)*x**(y%1);     return exp( y*ln(x) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
root:   procedure; parse arg x 1 ox,y 1 oy;     if x=0 | y=1  then return x/1
        if \isInt(y)  then return $pow(x, 1/y)
        if y==2  then return sqrt(x); if y==-2  then return 1/sqrt(x); return rooti(x,y)/1
/*──────────────────────────────────────────────────────────────────────────────────────*/
rooti:  x=abs(x); y=abs(y); a= digits() + 5;  m= y-1;  d= 5
        parse value format(x,2,1,,0) 'E0'  with  ? 'E' _ .;   g= (?/y'E'_ % y) + (x>1)
          do until d==a;   d=min(d+d, a);  numeric digits d;  o=0
            do until o=g;  o=g;  g= format((m*g**y+x)/y/g**m,,d-2);  end;  end
        _= g*sign(ox);  if oy<0  then _= 1/_;                                     return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .; g=g *.5'e'_ %2
            do j=0  while h>9;        m.j=h;                 h=h%2+1;          end  /*j*/
            do k=j+5  to 0  by -1;    numeric digits m.k;    g=(g+x/g)*.5;     end  /*k*/
      numeric digits d;     return g/1
```

{{out|output|text=  when using the default input:}}

```txt

        2.3632718012073547030642233111215269103967         3.1415926535897932384626433832795028841972
       -3.5449077018110320545963349666822903655951         3.1415926535897932384626433832795028841972
        1.7724538509055160272981674833411451827975         3.1415926535897932384626433832795028841972
        1
        0.8862269254527580136490837416705725913988         3.1415926535897932384626433832795028841972
        1
        1.3293403881791370204736256125058588870982         3.1415926535897932384626433832795028841972
        2
        3.3233509704478425511840640312646472177454         3.1415926535897932384626433832795028841972
        6

```



## Ring


```ring

decimals(3)
gamma = 0.577
coeff = -0.655
quad = -0.042
qui = 0.166
set = -0.042

for i=1 to 10
    see gammafunc(i / 3.0) + nl
next

func recigamma z
     return z + gamma * pow(z,2) + coeff * pow(z,3) + quad * pow(z,4) + qui * pow(z,5) + set * pow(z,6)

func gammafunc z
     if z = 1 return 1
     but fabs(z) <= 0.5 return 1 / recigamma(z)
     else return (z - 1) * gammafunc(z-1) ok

```



## RLaB

RLaB through GSL has the following functions related to the Gamma function,
namely, ''Gamma'', ''GammaRegularizedC'', ''LogGamma'', ''RecGamma'', and ''Pochhammer, where
:<math>Gamma(a) = \Gamma(a)</math>, the Gamma function;
:<math>Gamma(a,x) = \frac{1}{\Gamma(a)} \int_x^{\infty} dt \, t^{a-1} \, exp(-t) </math>, the regularized Gamma function which is also known as the normalized incomplete Gamma function;
:<math>GammaRegularizedC(a,x) = \frac{1}{\Gamma(a)} \int_0^x dt \, t^{a-1} \, exp(-t)</math>, which the GSL calls the complementary normalized Gamma function;
:<math>LogGamma(a) = \ln \Gamma(a)</math>;
:<math>RecGamma(a) = \frac{1}{\Gamma(a)}</math>;
:<math>Pochhammer(a,x) = \frac{\Gamma(a+x)}{\Gamma(x)}</math>.


## Ruby


### =Taylor series=

{{trans|Ada}}

```ruby
$a = [ 1.00000_00000_00000_00000,  0.57721_56649_01532_86061, -0.65587_80715_20253_88108,
      -0.04200_26350_34095_23553,  0.16653_86113_82291_48950, -0.04219_77345_55544_33675,
      -0.00962_19715_27876_97356,  0.00721_89432_46663_09954, -0.00116_51675_91859_06511,
      -0.00021_52416_74114_95097,  0.00012_80502_82388_11619, -0.00002_01348_54780_78824,
      -0.00000_12504_93482_14267,  0.00000_11330_27231_98170, -0.00000_02056_33841_69776,
       0.00000_00061_16095_10448,  0.00000_00050_02007_64447, -0.00000_00011_81274_57049,
       0.00000_00001_04342_67117,  0.00000_00000_07782_26344, -0.00000_00000_03696_80562,
       0.00000_00000_00510_03703, -0.00000_00000_00020_58326, -0.00000_00000_00005_34812,
       0.00000_00000_00001_22678, -0.00000_00000_00000_11813,  0.00000_00000_00000_00119,
       0.00000_00000_00000_00141, -0.00000_00000_00000_00023,  0.00000_00000_00000_00002 ]

def gamma(x)
  y = Float(x) - 1
  1.0 / $a.reverse.inject {|sum, an| sum * y + an}
end

(1..10).each {|i| puts format("%.14e", gamma(i/3.0))}
```

{{out}}

```txt
2.67893853470775e+00
1.35411793942640e+00
1.00000000000000e+00
8.92979511569249e-01
9.02745292950934e-01
1.00000000000000e+00
1.19063934875900e+00
1.50457548825154e+00
1.99999999999397e+00
2.77815847933857e+00
```


### =Built in=


```ruby
(1..10).each{|i| puts Math.gamma(i/3.0)}
```

{{out}}

```txt
2.678938534707748
1.3541179394264005
1.0
0.8929795115692493
0.9027452929509336
1.0
1.190639348758999
1.5045754882515558
2.0
2.7781584804376647

```



## Scala


```scala
import java.util.Locale._

object Gamma {
  def stGamma(x:Double):Double=math.sqrt(2*math.Pi/x)*math.pow((x/math.E), x)

  def laGamma(x:Double):Double={
    val p=Seq(676.5203681218851, -1259.1392167224028, 771.32342877765313,
             -176.61502916214059, 12.507343278686905, -0.13857109526572012,
                9.9843695780195716e-6, 1.5056327351493116e-7)

    if(x < 0.5) {
      math.Pi/(math.sin(math.Pi*x)*laGamma(1-x))
    } else {
      val x2=x-1
      val t=x2+7+0.5
      val a=p.zipWithIndex.foldLeft(0.99999999999980993)((r,v) => r+v._1/(x2+v._2+1))
      math.sqrt(2*math.Pi)*math.pow(t, x2+0.5)*math.exp(-t)*a
    }
  }

  def main(args: Array[String]): Unit = {
    println("Gamma    Stirling             Lanczos")
    for(x <- 0.1 to 2.0 by 0.1)
      println("%.1f  ->  %.16f   %.16f".formatLocal(ENGLISH, x, stGamma(x), laGamma(x)))
  }
}
```

{{out}}

```txt
Gamma    Stirling             Lanczos
0.1  ->  5.6971871489771690   9.5135076986687340
0.2  ->  3.3259984240223925   4.5908437119988030
0.3  ->  2.3625300362696198   2.9915689876875904
0.4  ->  1.8414763359362354   2.2181595437576870
0.5  ->  1.5203469010662807   1.7724538509055159
0.6  ->  1.3071588574483560   1.4891922488128180
0.7  ->  1.1590532921139200   1.2980553326475577
0.8  ->  1.0533709684256085   1.1642297137253035
0.9  ->  0.9770615078776956   1.0686287021193193
1.0  ->  0.9221370088957892   1.0000000000000002
1.1  ->  0.8834899531687038   0.9513507698668728
1.2  ->  0.8577553353965909   0.9181687423997607
1.3  ->  0.8426782594483921   0.8974706963062777
1.4  ->  0.8367445486370817   0.8872638175030760
1.5  ->  0.8389565525264964   0.8862269254527583
1.6  ->  0.8486932421525738   0.8935153492876904
1.7  ->  0.8656214717938840   0.9086387328532912
1.8  ->  0.8896396352879945   0.9313837709802430
1.9  ->  0.9208427218942294   0.9617658319073875
2.0  ->  0.9595021757444918   1.0000000000000010
```



## Scheme


{{trans|Scala}} for Lanczos and Stirling
{{trans|Ruby}} for Taylor


```scheme

(import (scheme base)
        (scheme inexact)
        (scheme write))

(define PI 3.14159265358979323846264338327950)
(define e 2.7182818284590452353602875)

(define gamma-lanczos
  (let ((p '(676.5203681218851 -1259.1392167224028 771.32342877765313
             -176.61502916214059 12.507343278686905 -0.13857109526572012
             9.9843695780195716e-6 1.5056327351493116e-7)))
    (lambda (x)
      (if (< x 0.5)
        (/ PI (* (sin (* PI x)) (gamma-lanczos (- 1 x))))
        (let* ((x2 (- x 1))
               (t (+ x2 7 0.5))
               (a (do ((ps p (cdr ps))
                       (idx 0 (+ 1 idx))
                       (res 0.99999999999980993 (+ res
                                                   (/ (car ps)
                                                      (+ x2 idx 1)))))
                    ((null? ps) res))))
          (* (sqrt (* 2 PI)) (expt t (+ x2 0.5)) (exp (- t)) a))))))

(define (gamma-stirling x)
  (* (sqrt (* 2 (/ PI x))) (expt (/ x e) x)))

(define gamma-taylor
  (let ((a (reverse
             '(1.00000000000000000000  0.57721566490153286061
               -0.65587807152025388108 -0.04200263503409523553
               0.16653861138229148950 -0.04219773455554433675
               -0.00962197152787697356  0.00721894324666309954
               -0.00116516759185906511 -0.00021524167411495097
               0.00012805028238811619 -0.00002013485478078824
               -0.00000125049348214267  0.00000113302723198170
               -0.00000020563384169776 0.00000000611609510448
               0.00000000500200764447 -0.00000000118127457049
               0.00000000010434267117 0.00000000000778226344
               -0.00000000000369680562 0.00000000000051003703
               -0.00000000000002058326 -0.00000000000000534812
               0.00000000000000122678 -0.00000000000000011813
               0.00000000000000000119 0.00000000000000000141
               -0.00000000000000000023  0.00000000000000000002))))
    (lambda (x)
      (let ((y (- x 1)))
        (do ((as a (cdr as))
             (res 0 (+ (car as) (* res y))))
          ((null? as) (/ 1 res)))))))

(do ((i 0.1 (+ i 0.1)))
  ((> i 2.01) )
  (display (string-append "Gamma ("
                          (number->string i)
                          "): "
                          "\n --- Lanczos : "
                          (number->string (gamma-lanczos i))
                          "\n --- Stirling: "
                          (number->string (gamma-stirling i))
                          "\n --- Taylor  : "
                          (number->string (gamma-taylor i))
                          "\n")))

```


{{out}}

```txt

Gamma (0.1):
 --- Lanczos : 9.513507698668736
 --- Stirling: 5.69718714897717
 --- Taylor  : 9.513507698668734
Gamma (0.2):
 --- Lanczos : 4.590843711998803
 --- Stirling: 3.3259984240223925
 --- Taylor  : 4.5908437119988035
Gamma (0.30000000000000004):
 --- Lanczos : 2.9915689876875904
 --- Stirling: 2.3625300362696198
 --- Taylor  : 2.991568987687591
Gamma (0.4):
 --- Lanczos : 2.218159543757687
 --- Stirling: 1.8414763359362354
 --- Taylor  : 2.2181595437576886
Gamma (0.5):
 --- Lanczos : 1.7724538509055159
 --- Stirling: 1.5203469010662807
 --- Taylor  : 1.772453850905516
Gamma (0.6):
 --- Lanczos : 1.489192248812818
 --- Stirling: 1.307158857448356
 --- Taylor  : 1.489192248812817
Gamma (0.7):
 --- Lanczos : 1.2980553326475577
 --- Stirling: 1.15905329211392
 --- Taylor  : 1.298055332647558
Gamma (0.7999999999999999):
 --- Lanczos : 1.1642297137253035
 --- Stirling: 1.0533709684256085
 --- Taylor  : 1.1642297137253033
Gamma (0.8999999999999999):
 --- Lanczos : 1.0686287021193193
 --- Stirling: 0.9770615078776956
 --- Taylor  : 1.0686287021193195
Gamma (0.9999999999999999):
 --- Lanczos : 1.0000000000000002
 --- Stirling: 0.9221370088957892
 --- Taylor  : 1.0000000000000002
Gamma (1.0999999999999999):
 --- Lanczos : 0.9513507698668728
 --- Stirling: 0.8834899531687039
 --- Taylor  : 0.9513507698668733
Gamma (1.2):
 --- Lanczos : 0.9181687423997607
 --- Stirling: 0.8577553353965909
 --- Taylor  : 0.9181687423997608
Gamma (1.3):
 --- Lanczos : 0.8974706963062777
 --- Stirling: 0.842678259448392
 --- Taylor  : 0.8974706963062773
Gamma (1.4000000000000001):
 --- Lanczos : 0.8872638175030759
 --- Stirling: 0.8367445486370818
 --- Taylor  : 0.8872638175030753
Gamma (1.5000000000000002):
 --- Lanczos : 0.8862269254527583
 --- Stirling: 0.8389565525264964
 --- Taylor  : 0.886226925452758
Gamma (1.6000000000000003):
 --- Lanczos : 0.8935153492876904
 --- Stirling: 0.8486932421525738
 --- Taylor  : 0.8935153492876904
Gamma (1.7000000000000004):
 --- Lanczos : 0.9086387328532912
 --- Stirling: 0.865621471793884
 --- Taylor  : 0.9086387328532904
Gamma (1.8000000000000005):
 --- Lanczos : 0.931383770980243
 --- Stirling: 0.8896396352879945
 --- Taylor  : 0.9313837709802427
Gamma (1.9000000000000006):
 --- Lanczos : 0.9617658319073875
 --- Stirling: 0.9208427218942294
 --- Taylor  : 0.9617658319073876
Gamma (2.0000000000000004):
 --- Lanczos : 1.000000000000001
 --- Stirling: 0.9595021757444918
 --- Taylor  : 1.0000000000000002

```



## Scilab

<lang>function x=gammal(z)  // Lanczos'
    lz=[  1.000000000190015 ..
          76.18009172947146  -86.50532032941677      24.01409824083091    ..
         -1.231739572450155   1.208650973866179e-3  -5.395239384953129e-6 ]
    if z < 0.5 then
        x=%pi/sin(%pi*z)-gammal(1-z)
    else
        z=z-1.0
        b=z+5.5
        a=lz(1)
        for i=1:6
            a=a+(lz(i+1)/(z+i))
        end
        x=exp((log(sqrt(2*%pi))+log(a)-b)+log(b)*(z+0.5))
    end
endfunction

printf("%4s %-9s %-9s\n","x","gamma(x)","gammal(x)")
for i=1:30
    x=i/10
    printf("%4.1f %9f %9f\n",x,gamma(x),gammal(x))
end
```

{{out}}
<pre style="height:20ex">
   x gamma(x)  gammal(x)
 0.1  9.097779  9.097779
 0.2  4.180567  4.180567
 0.3  2.585167  2.585167
 0.4  1.814074  1.814074
 0.5  1.772454  1.772454
 0.6  1.489192  1.489192
 0.7  1.298055  1.298055
 0.8  1.164230  1.164230
 0.9  1.068629  1.068629
 1.0  1.000000  1.000000
 1.1  0.951351  0.951351
 1.2  0.918169  0.918169
 1.3  0.897471  0.897471
 1.4  0.887264  0.887264
 1.5  0.886227  0.886227
 1.6  0.893515  0.893515
 1.7  0.908639  0.908639
 1.8  0.931384  0.931384
 1.9  0.961766  0.961766
 2.0  1.000000  1.000000
 2.1  1.046486  1.046486
 2.2  1.101802  1.101802
 2.3  1.166712  1.166712
 2.4  1.242169  1.242169
 2.5  1.329340  1.329340
 2.6  1.429625  1.429625
 2.7  1.544686  1.544686
 2.8  1.676491  1.676491
 2.9  1.827355  1.827355
 3.0  2.000000  2.000000

```



## Seed7

{{trans|Ada}}

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: gamma (in float: X) is func
  result
    var float: result is 0.0;
  local
    const array float: A is [] (
         1.00000000000000000000,  0.57721566490153286061,
        -0.65587807152025388108, -0.04200263503409523553,
         0.16653861138229148950, -0.04219773455554433675,
        -0.00962197152787697356,  0.00721894324666309954,
        -0.00116516759185906511, -0.00021524167411495097,
         0.00012805028238811619, -0.00002013485478078824,
        -0.00000125049348214267,  0.00000113302723198170,
        -0.00000020563384169776,  0.00000000611609510448,
         0.00000000500200764447, -0.00000000118127457049,
         0.00000000010434267117,  0.00000000000778226344,
        -0.00000000000369680562,  0.00000000000051003703,
        -0.00000000000002058326, -0.00000000000000534812,
         0.00000000000000122678, -0.00000000000000011813,
         0.00000000000000000119,  0.00000000000000000141,
        -0.00000000000000000023,  0.00000000000000000002);
    var float: Y is 0.0;
    var float: Sum is A[maxIdx(A)];
    var integer: N is 0;
  begin
    Y := X - 1.0;
    for N range pred(maxIdx(A)) downto minIdx(A) do
      Sum := Sum * Y + A[N];
    end for;
    result := 1.0 / Sum;
  end func;

const proc: main is func
  local
    var integer: I is 0;
  begin
    for I range 1 to 10 do
      writeln((gamma(flt(I) / 3.0)) digits 15);
    end for;
  end func;
```

{{out}}

```txt
2.678937911987305
1.354117870330811
1.000000000000000
0.892979443073273
0.902745306491852
1.000000000000000
1.190639257431030
1.504575252532959
1.999999523162842
2.778157949447632
```



## Sidef

{{trans|Ruby}}

```ruby
var a = [ 1.00000_00000_00000_00000,  0.57721_56649_01532_86061, -0.65587_80715_20253_88108,
         -0.04200_26350_34095_23553,  0.16653_86113_82291_48950, -0.04219_77345_55544_33675,
         -0.00962_19715_27876_97356,  0.00721_89432_46663_09954, -0.00116_51675_91859_06511,
         -0.00021_52416_74114_95097,  0.00012_80502_82388_11619, -0.00002_01348_54780_78824,
         -0.00000_12504_93482_14267,  0.00000_11330_27231_98170, -0.00000_02056_33841_69776,
          0.00000_00061_16095_10448,  0.00000_00050_02007_64447, -0.00000_00011_81274_57049,
          0.00000_00001_04342_67117,  0.00000_00000_07782_26344, -0.00000_00000_03696_80562,
          0.00000_00000_00510_03703, -0.00000_00000_00020_58326, -0.00000_00000_00005_34812,
          0.00000_00000_00001_22678, -0.00000_00000_00000_11813,  0.00000_00000_00000_00119,
          0.00000_00000_00000_00141, -0.00000_00000_00000_00023,  0.00000_00000_00000_00002 ]

func gamma(x) {
    var y = (x - 1)
    1 / a.reverse.reduce {|sum, an| sum*y + an}
}

for i in 1..10 {
    say ("%.14e" % gamma(i/3))
}
```

{{out}}

```txt

2.67893853470775e+00
1.35411793942640e+00
1.00000000000000e+00
8.92979511569249e-01
9.02745292950934e-01
1.00000000000000e+00
1.19063934875900e+00
1.50457548825154e+00
1.99999999999397e+00
2.77815847933858e+00

```


Lanczos approximation:

```ruby
func gamma(z) {
    var epsilon = 0.0000001
    func withinepsilon(x) {
        abs(x - abs(x)) <= epsilon
    }
 
    var p = [
        676.5203681218851,     -1259.1392167224028,
        771.32342877765313,    -176.61502916214059,
        12.507343278686905,    -0.13857109526572012,
        9.9843695780195716e-6,  1.5056327351493116e-7,
    ]
 
    var result = 0
    const pi = Num.pi
 
    if (z.re < 0.5) {
        result = (pi / (sin(pi * z) * gamma(1 - z)))
    }
    else {
        z -= 1
        var x = 0.99999999999980993
 
        p.each_kv { |i, v|
            x += v/(z + i + 1)
        }
 
        var t = (z + p.len - 0.5)
        result = (sqrt(pi*2) * t**(z+0.5) * exp(-t) * x)
    }
 
    withinepsilon(result.im) ? result.re : result
}
 
for i in 1..10 {
    say ("%.14e" % gamma(i/3))
}
```


{{out}}

```txt

2.67893853470774e+00
1.35411793942640e+00
1.00000000000000e+00
8.92979511569252e-01
9.02745292950931e-01
1.00000000000000e+00
1.19063934875900e+00
1.50457548825155e+00
2.00000000000000e+00
2.77815848043767e+00

```


A simpler implementation:

```ruby
define ℯ = Num.e
define τ = Num.tau
 
func Γ(t) {
    t < 20 ? (__FUNC__(t + 1) / t)
           : (sqrt(τ*t) * pow(t/ℯ + 1/(12*ℯ*t), t) / t)
}
 
for i in (1..10) {
    say ("%.14e" % Γ(i/3))
}
```


{{out}}

```txt

2.67893831294932e+00
1.35411783267848e+00
9.99999913007168e-01
8.92979437649773e-01
9.02745221785653e-01
9.99999913007168e-01
1.19063925019970e+00
1.50457536964275e+00
1.99999982601434e+00
2.77815825046596e+00

```



## Stata

This implementation uses the Taylor expansion of 1/gamma(1+x). The coefficients were computed with Maxima (see the Maxima implementation above).
The results are compared to Mata's '''[https://www.stata.com/help.cgi?mf_gamma gamma]''' function for each real between 1/100 and 100, by steps of 1/100.


```stata
mata
_gamma_coef = 1.0,
5.7721566490153286061e-1,-6.5587807152025388108e-1,
-4.2002635034095235529e-2,1.665386113822914895e-1,
-4.2197734555544336748e-2,-9.6219715278769735621e-3,
7.2189432466630995424e-3,-1.1651675918590651121e-3,
-2.1524167411495097282e-4,1.2805028238811618615e-4,
-2.0134854780788238656e-5,-1.2504934821426706573e-6,
1.1330272319816958824e-6,-2.0563384169776071035e-7,
6.1160951044814158179e-9,5.0020076444692229301e-9,
-1.1812745704870201446e-9,1.0434267116911005105e-10,
7.782263439905071254e-12,-3.6968056186422057082e-12,
5.100370287454475979e-13,-2.0583260535665067832e-14,
-5.3481225394230179824e-15

function gamma_(x_) {
	external _gamma_coef
	x = x_
	for (y=1; x>2;) y = --x*y
	z = _gamma_coef[24]
	x--
	for (i=23; i>=1; i--) z = z*x+_gamma_coef[i]
	return(y/z)
}

function map(f,a) {
	n = rows(a)
	p = cols(a)
	b = J(n,p,.)
	for (i=1; i<=n; i++) {
		for (j=1; j<=p; j++) {
			b[i,j] = (*f)(a[i,j])
		}
	}
	return(b)
}

x=(1::10000)/100
u=map(&gamma(),x)
v=map(&gamma_(),x)
max(abs((u-v):/u))
end
```


'''Output'''


```txt
1.27664e-13
```



## Tcl

{{works with|Tcl|8.5}}
{{tcllib|math}}
{{tcllib|math::calculus}}

```tcl
package require math
package require math::calculus

# gamma(1) and gamma(1.5)

set f 1.0
set f2 [expr {sqrt(acos(-1.))/2.}]

for {set x 1.0} {$x <= 10.0} {set x [expr {$x + 0.5}]} {

    # method 1 - numerical integration, Romberg's method, special
    #            case for an improper integral

    set g1 [math::calculus::romberg  \
                [list apply {{x t} {expr {$t ** ($x-1) * exp(-$t)}}} $x] \
                0 1 -relerror 1e-8]
    set g2 [math::calculus::romberg_infinity \
                [list apply {{x t} {expr {$t ** ($x-1) * exp(-$t)}}} $x] \
                1 Inf -relerror 1e-8]
    set gamma [expr {[lindex $g1 0] + [lindex $g2 0]}]

    # method 2 - library function

    set libgamma [expr {exp([math::ln_Gamma $x])}]

    # method 3 - special forms for integer and half-integer arguments

    if {$x == entier($x)} {
        puts [format {%4.1f %13.6f %13.6f %13.6f} $x $gamma $libgamma $f]
        set f [expr $f * $x]
    } else {
        puts [format {%4.1f %13.6f %13.6f %13.6f} $x $gamma $libgamma $f2]
        set f2 [expr $f2 * $x]
    }
}
```

{{out}}

```txt
 1.0      1.000000      1.000000      1.000000
 1.5      0.886228      0.886227      0.886227
 2.0      1.000000      1.000000      1.000000
 2.5      1.329340      1.329340      1.329340
 3.0      2.000000      2.000000      2.000000
 3.5      3.323351      3.323351      3.323351
 4.0      6.000000      6.000000      6.000000
 4.5     11.631731     11.631728     11.631728
 5.0     24.000009     24.000000     24.000000
 5.5     52.342778     52.342778     52.342778
 6.0    120.000000    120.000000    120.000000
 6.5    287.885278    287.885278    287.885278
 7.0    720.000001    720.000000    720.000000
 7.5   1871.254311   1871.254305   1871.254306
 8.0   5040.000032   5039.999999   5040.000000
 8.5  14034.298267  14034.407291  14034.407293
 9.0  40320.000705  40319.999992  40320.000000
 9.5 119292.464880 119292.461971 119292.461995
10.0 362880.010950 362879.999927 362880.000000
```


=={{header|TI-83 BASIC}}==
There is an hidden Gamma function in TI-83. Factorial (!) is implemented in increments of 0.5 :
 .5! -> .8862269255
As far as Gamma(n)=(n-1)! , we have everything needed.
===Stirling's approximation===

```ti83b
for(I,1,10)
I/2→X
X^(X-1/2)e^(-X)√(2π)→Y
Disp X,(X-1)!,Y
Pause
End
```

{{out}}
The output display for x=0.5 to 5 by 0.5 : x, (x-1)!, Y(x) .
Y(x) is Stirling's approximation of Gamma.
<pre style="height:20ex">
        0.5
1.772453851
1.520346901
          1
          1
.9221370089
        1.5
.8862269255
.8389565525
          2
          1
.9595021757
        2.5
1.329340388
1.285978615
          3
          2
1.945403197
        3.5
 3.32335097
3.245363748
          4
          6
5.876543783
        4.5
 11.6317284
11.41865156
          5
         24
23.60383359

```


===Lanczos' approximation===

```ti83b
for(I,1,10)
I/2→X
e^(ln((1.0
+76.18009173/(X+1)
-86.50532033/(X+2)
+24.01409824/(X+3)
-1.231739572/(X+4)
+1.208650974E-3/(X+5)
-5.395239385E-6/(X+6)
)√(2π)/X)
+(X+.5)ln(X+5.5)-X-5.5)->Y
Disp X,(X-1)!,Y
Pause
End
```

{{out}}
The output display for x=0.5 to 5 by 0.5 : x, (x-1)!, Y(x) .
Y(x) is Lanczos's approximation of Gamma.
<pre style="height:20ex">
        0.5
1.772453851
1.772453851
          1
          1
          1
        1.5
.8862269255
.8862269254
          2
          1
          1
        2.5
1.329340388
1.329340388
          3
          2
          2
        3.5
 3.32335097
 3.32335097
          4
          6
          6
        4.5
 11.6317284
 11.6317284
          5
         24
         24

```



## Visual FoxPro

Translation of BBC Basic but with OOP extensions. Also some ideas from Numerical Methods (Press ''et al'').

```vfp

LOCAL i As Integer, x As Double, o As lanczos
CLOSE DATABASES ALL
CLEAR
CREATE CURSOR results (ZVal B(1), GamVal B(15))
INDEX ON zval TAG ZVal COLLATE "Machine"
SET ORDER TO 0
o = CREATEOBJECT("lanczos")
FOR i = 1 TO 20
x = i/10
    INSERT INTO results VALUES (x, o.Gamma(x))
ENDFOR
UPDATE results SET GamVal = ROUND(GamVal, 0) WHERE ZVal = INT(ZVal)
*!* This just creates the output text - it is not part of the algorithm
DO cursor2txt.prg WITH "results", .T.

DEFINE CLASS lanczos As Session
#DEFINE FPF 5.5
#DEFINE HALF 0.5
#DEFINE PY PI()
DIMENSION LanCoeff[7]
nSize = 0

PROCEDURE Init
DODEFAULT()
WITH THIS
    .LanCoeff[1] = 1.000000000190015
    .LanCoeff[2] = 76.18009172947146
    .LanCoeff[3] = -86.50532032941677
    .LanCoeff[4] = 24.01409824083091
    .LanCoeff[5] = -1.231739572450155
    .LanCoeff[6] = 0.0012086509738662
    .LanCoeff[7] = -0.000005395239385
    .nSize = ALEN(.LanCoeff)
ENDWITH
ENDPROC

FUNCTION Gamma(z)
RETURN EXP(THIS.LogGamma(z))
ENDFUNC

FUNCTION LogGamma(z)
LOCAL a As Double, b As Double, i As Integer, j As Integer, lg As Double
IF z < 0.5
    lg = LOG(PY/SIN(PY*z)) - THIS.LogGamma(1 - z)
ELSE
    WITH THIS
	z = z - 1
	b = z + FPF
	a = .LanCoeff[1]
	FOR i = 2 TO .nSize
	    j = i - 1
	    a = a + .LanCoeff[i]/(z + j)
	ENDFOR
	lg = (LOG(SQRT(2*PY)) + LOG(a) - b) + LOG(b)*(z + HALF)
    ENDWITH
ENDIF
RETURN lg
ENDFUNC

ENDDEFINE

```

{{out}}

```txt

zval	gamval
0.1	9.513507698669704
0.2	4.590843712000122
0.3	2.991568987689402
0.4	2.218159543760185
0.5	1.772453850902053
0.6	1.489192248811141
0.7	1.298055332646772
0.8	1.164229713724969
0.9	1.068628702119210
1.0	1.000000000000000
1.1	0.951350769866919
1.2	0.918168742399821
1.3	0.897470696306335
1.4	0.887263817503125
1.5	0.886226925452796
1.6	0.893515349287718
1.7	0.908638732853309
1.8	0.931383770980253
1.9	0.961765831907391
2.0	1.000000000000000

```



## Yabasic

{{trans|Phix}}

```Yabasic
dim c(12)

sub gamma(z)
    local accm, k, k1_factrl

    accm = c(1)
    if accm=0 then
        accm = sqrt(2*PI)
        c(1) = accm
        k1_factrl = 1
        for k=2 to 12
            c(k) = exp(13-k)*(13-k)^(k-1.5)/k1_factrl
            k1_factrl = k1_factrl * -(k-1)
        next
    end if
    for k=2 to 12
        accm = accm + c(k)/(z+k-1)
    next
    accm = accm * exp(-(z+12))*(z+12)^(z+0.5)
    return accm/z
end sub

sub si(x)
    print x using "%18.13f"
end sub


for i = 0.1 to 2.1 step .1
    print i, " = "; : si(gamma(i))
next
```



## zkl

{{trans|D}} but without a built in gamma function.

```zkl
fcn taylorGamma(x){
   var table = T(
     0x1p+0,                    0x1.2788cfc6fb618f4cp-1,
    -0x1.4fcf4026afa2dcecp-1,  -0x1.5815e8fa27047c8cp-5,
     0x1.5512320b43fbe5dep-3,  -0x1.59af103c340927bep-5,
    -0x1.3b4af28483e214e4p-7,   0x1.d919c527f60b195ap-8,
    -0x1.317112ce3a2a7bd2p-10, -0x1.c364fe6f1563ce9cp-13,
     0x1.0c8a78cd9f9d1a78p-13, -0x1.51ce8af47eabdfdcp-16,
    -0x1.4fad41fc34fbb2p-20,    0x1.302509dbc0de2c82p-20,
    -0x1.b9986666c225d1d4p-23,  0x1.a44b7ba22d628acap-28,
     0x1.57bc3fc384333fb2p-28, -0x1.44b4cedca388f7c6p-30,
     0x1.cae7675c18606c6p-34,   0x1.11d065bfaf06745ap-37,
    -0x1.0423bac8ca3faaa4p-38,  0x1.1f20151323cd0392p-41,
    -0x1.72cb88ea5ae6e778p-46, -0x1.815f72a05f16f348p-48,
     0x1.6198491a83bccbep-50,  -0x1.10613dde57a88bd6p-53,
     0x1.5e3fee81de0e9c84p-60,  0x1.a0dc770fb8a499b6p-60,
    -0x1.0f635344a29e9f8ep-62,  0x1.43d79a4b90ce8044p-66).reverse();

    y  := x.toFloat() - 1.0;
    sm := table[1,*].reduce('wrap(sm,an){ sm * y + an },table[0]);

    return(1.0 / sm);
}
```


```zkl
fcn lanczosGamma(z) { z = z.toFloat();
    // Coefficients used by the GNU Scientific Library.
    // http://en.wikipedia.org/wiki/Lanczos_approximation
    const g = 7, PI = (0.0).pi;
    exp := (0.0).e.pow;
    var table = T(
             0.99999_99999_99809_93,
           676.52036_81218_851,
         -1259.13921_67224_028,
           771.32342_87776_5313,
          -176.61502_91621_4059,
            12.50734_32786_86905,
            -0.13857_10952_65720_12,
             9.98436_95780_19571_6e-6,
             1.50563_27351_49311_6e-7);

    // Reflection formula.
    if (z < 0.5) {
        return(PI / ((PI * z).sin() * lanczosGamma(1.0 - z)));
    } else {
        z -= 1;
        x := table[0];
        foreach i in ([1 .. g + 1]){
            x += table[i] / (z + i); }
        t := z + g + 0.5;
        return((2.0 * PI).sqrt() * t.pow(z + 0.5) * exp(-t) * x);
    }
}
```

{{out}}

```txt

foreach i in ([1.0 .. 10]) {
   x := i / 3.0;
   println("%f: %20.19e %20.19e %e".fmt( x,
	   a:=taylorGamma(x), b:=lanczosGamma(x),(a-b).abs()));
}

```


```txt

0.333333: 2.6789385347077483424e+00 2.6789385347077474542e+00 8.881784e-16
0.666667: 1.3541179394264004632e+00 1.3541179394264002411e+00 2.220446e-16
1.000000: 1.0000000000000000000e+00 1.0000000000000002220e+00 2.220446e-16
1.333333: 8.9297951156924926241e-01 8.9297951156924970650e-01 4.440892e-16
1.666667: 9.0274529295093364212e-01 9.0274529295093353110e-01 1.110223e-16
2.000000: 1.0000000000000000000e+00 1.0000000000000006661e+00 6.661338e-16
2.333333: 1.1906393487589990166e+00 1.1906393487589996827e+00 6.661338e-16
2.666667: 1.5045754882515545159e+00 1.5045754882515582906e+00 3.774758e-15
3.000000: 1.9999999999992210675e+00 2.0000000000000017764e+00 7.807088e-13
3.333333: 2.7781584802531797962e+00 2.7781584804376668885e+00 1.844871e-10

```

