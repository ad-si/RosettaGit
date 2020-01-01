+++
title = "Nth root"
description = ""
date = 2019-10-09T18:17:09Z
aliases = []
[extra]
id = 4622
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
[[Category:Simple]]

;Task:
Implement the algorithm to compute the principal   [[wp:Nth root|<big>''n''<sup>th</sup></big>   root]]   <big><big><big><math>\sqrt[n]A</math></big></big></big>   of a positive real number   <big>''A''</big>,   as explained at the   [[wp:Nth root algorithm|Wikipedia page]].






## 360 Assembly

An example of converting integer floating-point using unnormalized short format.
The 'include' file FORMAT, to format a floating point number,  can be found in:
[[360_Assembly_include|Include files 360 Assembly]].

```360asm
*        Nth root - x**(1/n)       - 29/07/2018
NTHROOT  CSECT
         USING  NTHROOT,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         BAL    R14,ROOTN          call rootn(x,n)
         LE     F0,XN              xn=rootn(x,n)
         LA     R0,6               decimals=6
         BAL    R14,FORMATF        edit xn
         MVC    PG(13),0(R1)       output xn
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
ROOTN    MVC    ZN,=E'0'           zn=0  ----------------------------
         MVC    ZN,N               n
         MVI    ZN,X'46'           zn=unnormalize(n)
         LE     F0,ZN              zn
         AE     F0,=E'0'           normalized
         STE    F0,ZN              zn=normalize(n)
         LE     F6,=E'0'           xm=0
         LE     F0,X               x
         DE     F0,ZN              /zn
         STE    F0,XN              xn=x/zn
WHILEA   LE     F0,XN              xn
         SER    F0,F6              xn-xm
         LPER   F0,F0              abs((xn-xm)
         DE     F0,XN              /xn
         CE     F0,EPSILON         while abs((xn-xm)/xn)>epsilon
         BNH    EWHILEA            ~
         LE     F6,XN                xm=xn
         LE     F0,ZN                zn
         SE     F0,=E'1'             zn-1
         MER    F0,F6                f0=(zn-1)*xm
         L      R2,N                 n
         BCTR   R2,0                 n-1
         LE     F2,=E'1'             xm
POW      MER    F2,F6                *xm
         BCT    R2,POW               f2=xm**(n-1)
         LE     F4,X                 x
         DER    F4,F2                x/xm**(n-1)
         AER    F0,F4                (zn-1)*xm+x/xm**(n-1)
         DE     F0,ZN                /zn
         STE    F0,XN                xn=((zn-1)*xm+x/xm**(n-1))/zn
         B      WHILEA             endwhile
EWHILEA  LE     F0,XN              xn
         BR     R14                return ---------------------------
         COPY   FORMATF            format a float
X        DC     E'2'               x  <== input
N        DC     F'2'               n  <== input
EPSILON  DC     E'1E-6'            imprecision
XN       DS     E                  xn :: output
ZN       DS     E                  zn=float(n)
PG       DC     CL80' '            buffer
         REGEQU
         END    NTHROOT
```

{{out}}

```txt
     1.414213
```



## Ada

The implementation is generic and supposed to work with any floating-point type. There is no result accuracy argument of Nth_Root, because the iteration is supposed to be monotonically descending to the root when starts at ''A''. Thus it should converge when this condition gets violated, i.e. when ''x''<sub>''k''+1</sub>''&ge;''x''<sub>''k''</sub>''.

```Ada

with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Nth_Root is
   generic
      type Real is digits <>;
   function Nth_Root (Value : Real; N : Positive) return Real;

   function Nth_Root (Value : Real; N : Positive) return Real is
      type Index is mod 2;
      X : array (Index) of Real := (Value, Value);
      K : Index := 0;
   begin
      loop
         X (K + 1) := ( (Real (N) - 1.0) * X (K) + Value / X (K) ** (N-1) ) / Real (N);
         exit when X (K + 1) >= X (K);
         K := K + 1;
      end loop;
      return X (K + 1);
   end Nth_Root;

   function Long_Nth_Root is new Nth_Root (Long_Float);
begin
   Put_Line ("1024.0 10th  =" & Long_Float'Image (Long_Nth_Root (1024.0, 10)));
   Put_Line ("  27.0 3rd   =" & Long_Float'Image (Long_Nth_Root (27.0, 3)));
   Put_Line ("   2.0 2nd   =" & Long_Float'Image (Long_Nth_Root (2.0, 2)));
   Put_Line ("5642.0 125th =" & Long_Float'Image (Long_Nth_Root (5642.0, 125)));
end Test_Nth_Root;

```

Sample output:

```txt

1024.0 10th  = 2.00000000000000E+00
  27.0 3rd   = 3.00000000000000E+00
   2.0 2nd   = 1.41421356237310E+00
5642.0 125th = 1.07154759194477E+00

```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - missing transput, and missing extended precision}}

```algol68
REAL default p = 0.001;

PROC nth root = (INT n, LONG REAL a, p)LONG REAL:
(
  [2]LONG REAL x := (a, a/n);

  WHILE ABS(x[2] - x[1]) > p DO
    x := (x[2], ((n-1)*x[2] + a/x[2]**(n-1))/n )
  OD;
  x[2]
);

PRIO ROOT = 8;
OP ROOT = (INT n, LONG REAL a)LONG REAL: nth root(n, a, default p);
OP ROOT = (INT n, INT a)LONG REAL: nth root(n, a, default p);

main:
(
  printf(($2(" "gl)$,
         nth root(10, LONG 7131.5 ** 10, default p),
         nth root(5, 34, default p)));
  printf(($2(" "gl)$,
         10 ROOT ( LONG 7131.5 ** 10 ),
         5 ROOT 34))
)
```

Output:

```txt

 +7.131500000000000000001144390e  +3
 +2.024397462171090138953733623e  +0
 +7.131500000000000000001144390e  +3
 +2.024397462171090138953733623e  +0


```



## ALGOL W


```algolw
begin
    % nth root algorithm                                              %
    % returns the nth root of A, A must be > 0                        %
    %         the required precision should be specified in precision %
    long real procedure nthRoot( long real value A
                               ; integer   value n
                               ; long real value precision
                               ) ;
        begin
            long real xk, xd;
            integer   n1;
            n1 := n - 1;
            xk := A / n;
            while begin
                xd := ( ( A / ( xk ** n1 ) ) - xk ) / n;
                xk := xk + xd;
                abs( xd ) > precision
            end do begin end;
            xk
        end nthRoot ;
    % test cases %
    r_format := "A"; r_w := 15; r_d := 6; % set output format %
    write( nthRoot( 7131.5 ** 10, 10, 1'-5 ) );
    write( nthRoot(           64,  6, 1'-5 ) );
end.
```

{{out}}

```txt

    7131.500000
       2.000000

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program nroot.s   */
/* compile with option -mfpu=vfpv3 -mfloat-abi=hard */
/* link with gcc. Use C function for display float */

/* Constantes               */
.equ EXIT,   1                         @ Linux syscall

/* Initialized data */
.data
szFormat1:         .asciz " %+09.15f\n"
.align 4
iNumberA:          .int 1024

/* UnInitialized data */
.bss
.align 4

/*  code section */
.text
.global main
main:                                   @ entry of program
    push {fp,lr}                        @ saves registers

    /* root 10ieme de 1024  */
    ldr r0,iAdriNumberA                 @ number address
    ldr r0,[r0]
    vmov s0,r0                          @
    vcvt.f64.s32 d0, s0                 @conversion in float single précision (32 bits)
    mov r0,#10                          @ N
    bl nthRoot
    ldr r0,iAdrszFormat1                @ format
    vmov r2,r3,d0
    bl printf                           @ call C function !!!
                                        @ Attention register dn lost !!!
    /* square root of 2   */
    vmov.f64 d1,#2.0                    @ conversion 2 in float register d1
    mov r0,#2                           @ N
    bl nthRoot
    ldr r0,iAdrszFormat1                @ format
    vmov r2,r3,d0
    bl printf                           @ call C function !!!

100:                                    @ standard end of the program
    mov r0, #0                          @ return code
    pop {fp,lr}                         @restaur  registers
    mov r7, #EXIT                       @ request to exit program
    swi 0                               @ perform the system call

iAdrszFormat1:           .int szFormat1
iAdriNumberA:            .int iNumberA

/******************************************************************/
/*     compute  nth root                                          */
/******************************************************************/
/* r0 contains N   */
/* d0 contains the value                 */
/* d0 return result                      */
nthRoot:
    push {r1,r2,lr}                    @ save  registers
    vpush {d1-d8}                         @ save float registers
    FMRX    r1,FPSCR                   @ copy FPSCR into r1
    BIC     r1,r1,#0x00370000          @ clears STRIDE and LEN
    FMXR    FPSCR,r1                   @ copy r1 back into FPSCR

    vmov s2,r0                         @
    vcvt.f64.s32 d6, s2                @ N conversion in float double précision (64 bits)
    sub r1,r0,#1                       @ N - 1
    vmov s8,r1                         @
    vcvt.f64.s32 d4, s8                @conversion in float double précision (64 bits)
    vmov.f64 d2,d0                     @ a = A
    vdiv.F64 d3,d0,d6                  @ b = A/n
    adr r2,dfPrec                      @ load précision
    vldr d8,[r2]
1:                                     @ begin loop
    vmov.f64 d2,d3                     @ a <- b
    vmul.f64 d5,d3,d4                  @ (N-1)*b

    vmov.f64 d1,#1.0                   @ constante 1 -> float
    mov r2,#0                          @ loop indice
2:                                     @ compute pow (n-1)
    vmul.f64 d1,d1,d3                  @
    add r2,#1
    cmp r2,r1                          @ n -1 ?
    blt 2b                             @ no -> loop
    vdiv.f64 d7,d0,d1                  @ A / b pow (n-1)
    vadd.f64 d7,d7,d5                  @ + (N-1)*b
    vdiv.f64 d3,d7,d6                  @ / N -> new b
    vsub.f64 d1,d3,d2                  @ compute gap
    vabs.f64 d1,d1                     @ absolute value
    vcmp.f64 d1,d8                     @ compare float maj FPSCR
    fmstat                             @ transfert FPSCR -> APSR
                                       @ or use VMRS APSR_nzcv, FPSCR
    bgt 1b                             @ if gap > précision -> loop
    vmov.f64 d0,d3                     @ end return result in d0

100:
    vpop {d1-d8}                       @ restaur float registers
    pop {r1,r2,lr}                     @ restaur arm registers
    bx lr
dfPrec:            .double 0f1E-10     @ précision


```



## AutoHotkey


```autohotkey
p := 0.000001

MsgBox, % nthRoot( 10, 7131.5**10, p) "`n"
        . nthRoot(  5, 34.0      , p) "`n"
        . nthRoot(  2, 2         , p) "`n"
        . nthRoot(0.5, 7         , p) "`n"


;---------------------------------------------------------------------------
nthRoot(n, A, p) { ; http://en.wikipedia.org/wiki/Nth_root_algorithm
;---------------------------------------------------------------------------
    x1 := A
    x2 := A / n
    While Abs(x1 - x2) > p {
        x1 := x2
        x2 := ((n-1)*x2+A/x2**(n-1))/n
    }
    Return, x2
}
```

Message box shows:

```txt
7131.500000
2.024397
1.414214
49.000000
```



## AutoIt


```AutoIt
;AutoIt Version: 3.2.10.0
$A=4913
$n=3
$x=20
ConsoleWrite ($n& " root of "& $A & " is " &nth_root_it($A,$n,$x))
ConsoleWrite ($n& " root of "& $A & " is " &nth_root_rec($A,$n,$x))

;Iterative
Func nth_root_it($A,$n,$x)
   $x0="0"
   While StringCompare(string($x0),string($x))
      ConsoleWrite ($x&@CRLF)
      $x0=$x
      $x=((($n-1)*$x)+($A/$x^($n-1)))/$n
   WEnd
   Return $x
EndFunc

;Recursive
Func nth_root_rec($A,$n,$x)
   ConsoleWrite ($x&@CRLF)
   If $x==((($n-1)*$x)+($A/$x^($n-1)))/$n Then
      Return $x
   EndIf
   Return nth_root_rec($A,$n,((($n-1)*$x)+($A/$x^($n-1)))/$n)
EndFunc
```

output :

```txt
20
17.4275
17.0104009124137
17.0000063582823
17.0000000000024
17
3 root of 4913 is 17
```



## AWK



```awk

#!/usr/bin/awk -f
BEGIN {
        # test
	print nthroot(8,3)
	print nthroot(16,2)
	print nthroot(16,4)
	print nthroot(125,3)
	print nthroot(3,3)
	print nthroot(3,2)
}

function nthroot(y,n) {
        eps = 1e-15;   # relative accuracy
        x   = 1;
	do {
		d  = ( y / ( x^(n-1) ) - x ) / n ;
		x += d;
		e = eps*x;   # absolute accuracy
	} while ( d < -e  || d > e )

	return x
}

```


Sample output:
  2
  4
  2
  5
  1.44225
  1.73205


## BASIC

{{works with|QBasic}}
{{works with|FreeBASIC}}
{{works with|PowerBASIC}}
{{works with|Visual Basic}}

This function is fairly generic MS BASIC. It could likely be used in most modern BASICs with little or no change.


```qbasic
FUNCTION RootX (tBase AS DOUBLE, tExp AS DOUBLE, diffLimit AS DOUBLE) AS DOUBLE
    DIM tmp1 AS DOUBLE, tmp2 AS DOUBLE
    ' Initial guess:
    tmp1 = tBase / tExp
    DO
        tmp2 = tmp1
        ' 1# tells compiler that "1" is a double, not an integer
        tmp1 = (((tExp - 1#) * tmp2) + (tBase / (tmp2 ^ (tExp - 1#)))) / tExp
    LOOP WHILE (ABS(tmp1 - tmp2) > diffLimit)
    RootX = tmp1
END FUNCTION
```


Note that for the above to work in QBasic, the function definition needs to be changed like so:

```qbasic
FUNCTION RootX# (tBase AS DOUBLE, tExp AS DOUBLE, diffLimit AS DOUBLE)
```


The function is called like so:


```qbasic
PRINT "The "; e; "th root of "; b; " is "; RootX(b, e, .000001)
```


Sample output:
 The  4th root of  16 is  2

For BASICs without the '''^''' operator, it would be trivial to write a function to reproduce it (as is done in the [[#C|C]] example below).

See also the [[#Liberty BASIC|Liberty BASIC]] and [[#PureBasic|PureBasic]] solutions.


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT 64
      @% = &D0D
      PRINT "Cube root of 5 is "; FNroot(3, 5, 0)
      PRINT "125th root of 5643 is "; FNroot(125, 5643, 0)
      END

      DEF FNroot(n%, a, d)
      LOCAL x0, x1 : x0 = a / n% : REM Initial guess
      REPEAT
        x1 = ((n% - 1)*x0 + a/x0^(n%-1)) / n%
        SWAP x0, x1
      UNTIL ABS (x0 - x1) <= d
      = x0
```

'''Output:'''

```txt

Cube root of 5 is 1.709975946677
125th root of 5643 is 1.071549111198

```



## bc


```bc
/* Take the nth root of 'a' (a positive real number).
 * 'n' must be an integer.
 * Result will have 'd' digits after the decimal point.
 */
define r(a, n, d) {
    auto e, o, x, y, z

    if (n == 0) return(1)
    if (a == 0) return(0)

    o = scale
    scale = d
    e = 1 / 10 ^ d

    if (n < 0) {
        n = -n
        a = 1 / a
    }

    x = 1
    while (1) {
        y = ((n - 1) * x + a / x ^ (n - 1)) / n
        z = x - y
        if (z < 0) z = -z
        if (z < e) break
        x = y
    }
    scale = o
    return(y)
}
```



## Bracmat

Bracmat does not have floating point numbers as primitive type. Instead we have to use rational numbers. This code is not fast!

```bracmat
( ( root
  =   n a d x0 x1 d2 rnd 10-d
    .   ( rnd       { For 'rounding' rational numbers = keep number of digits within bounds. }
        =   N r
          .   !arg:(?N.?r)
            & div$(!N*!r+1/2.1)*!r^-1
        )
      & !arg:(?n,?a,?d)
      & !a*!n^-1:?x0
      & 10^(-1*!d):?10-d
      &   whl
        ' (   ( rnd$(((!n+-1)*!x0+!a*!x0^(1+-1*!n))*!n^-1.10^!d)
              . !x0
              )
            : (?x0.?x1)
          & (!x0+-1*!x1)^2:~<!10-d   { Exit loop when required precision is reached. }
          )
      & flt$(!x0,!d)      { Convert rational number to floating point representation. }
  )
& ( show
  =   N A precision
    .   !arg:(?N,?A,?precision)
      & out$(str$(!A "^(" !N^-1 ")=" root$(!N,!A,!precision)))
  )
& show$(10,1024,20)
& show$(3,27,20)
& show$(2,2,100)
& show$(125,5642,20)
)
```

Output:

```txt
1024^(1/10)=2,00000000000000000000*10E0
27^(1/3)=3,00000000000000000000*10E0
2^(1/2)=1,4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727*10E0
5642^(1/125)=1,07154759194476751170*10E0
```



## C

Implemented without using math library, because if we were to use <code>pow()</code>, the whole exercise wouldn't make sense.

```c
#include <stdio.h>
#include <float.h>

double pow_ (double x, int e) {
    int i;
    double r = 1;
    for (i = 0; i < e; i++) {
        r *= x;
    }
    return r;
}

double root (int n, double x) {
    double d, r = 1;
    if (!x) {
        return 0;
    }
    if (n < 1 || (x < 0 && !(n&1))) {
        return 0.0 / 0.0; /* NaN */
    }
    do {
        d = (x / pow_(r, n - 1) - r) / n;
        r += d;
    }
    while (d >= DBL_EPSILON * 10 || d <= -DBL_EPSILON * 10);
    return r;
}

int main () {
    int n = 15;
    double x = pow_(-3.14159, 15);
    printf("root(%d, %g) = %g\n", n, x, root(n, x));
    return 0;
}

```


## C#
Almost exactly how C works.

```c#

static void Main(string[] args)
{
	Console.WriteLine(NthRoot(81,2,.001));
        Console.WriteLine(NthRoot(1000,3,.001));
        Console.ReadLine();
}

public static double NthRoot(double A,int n,  double p)
{
	double _n= (double) n;
	double[] x = new double[2];
	x[0] = A;
	x[1] = A/_n;
	while(Math.Abs(x[0] -x[1] ) > p)
	{
		x[1] = x[0];
		x[0] = (1/_n)*(((_n-1)*x[1]) + (A/Math.Pow(x[1],_n-1)));

	}
	return x[0];
}

```



## C++



```cpp
double NthRoot(double m_nValue, double index, double guess, double pc)
   {
       double result = guess;
       double result_next;
       do
       {
           result_next = (1.0/index)*((index-1.0)*result+(m_nValue)/(pow(result,(index-1.0))));
           result = result_next;
           pc--;
       }while(pc>1);
       return result;
   };

```



```cpp
double NthRoot(double value, double degree)
{
    return pow(value, (double)(1 / degree));
};

```



## Clojure



```clojure

(ns test-project-intellij.core
  (:gen-class))

;; define abs & power to avoid needing to bring in the clojure Math library
(defn abs [x]
  " Absolute value"
  (if (< x 0) (- x) x))

(defn power [x n]
  " x to power n, where n = 0, 1, 2, ... "
  (apply * (repeat n x)))

(defn calc-delta [A x n]
  " nth rooth algorithm delta calculation "
  (/ (- (/ A (power x (- n 1))) x) n))

(defn nth-root
  " nth root of algorithm: A = numer, n = root"
  ([A n] (nth-root A n 0.5 1.0))  ; Takes only two arguments A, n and calls version which takes A, n, guess-prev, guess-current
  ([A n guess-prev guess-current] ; version take takes in four arguments (A, n, guess-prev, guess-current)
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur A n guess-current (+ guess-current (calc-delta A guess-current n)))))) ; iterate answer using tail recursion


```



## CoffeeScript



```coffeescript

nth_root = (A, n, precision=0.0000000000001) ->
  x = 1
  while true
    x_new = (1 / n) * ((n - 1) * x + A / Math.pow(x, n - 1))
    return x_new if Math.abs(x_new - x) < precision
    x = x_new

# tests
do ->
  tests = [
    [8, 3]
    [16, 4]
    [32, 5]
    [343, 3]
    [1024, 10]
    [1000000000, 3]
    [1000000000, 9]
    [100, 2]
    [100, 3]
    [100, 5]
    [100, 10]
  ]
  for test in tests
    [x, n] = test
    root = nth_root x, n
    console.log "#{x} root #{n} = #{root} (root^#{n} = #{Math.pow root, n})"

```

output
<lang>
> coffee nth_root.coffee
8 root 3 = 2 (root^3 = 8)
16 root 4 = 2 (root^4 = 16)
32 root 5 = 2 (root^5 = 32)
343 root 3 = 7 (root^3 = 343)
1024 root 10 = 2 (root^10 = 1024)
1000000000 root 3 = 1000 (root^3 = 1000000000)
1000000000 root 9 = 10 (root^9 = 1000000000)
100 root 2 = 10 (root^2 = 100)
100 root 3 = 4.641588833612778 (root^3 = 99.99999999999997)
100 root 5 = 2.5118864315095806 (root^5 = 100.0000000000001)
100 root 10 = 1.5848931924611134 (root^10 = 99.99999999999993)

```



## Common Lisp


This version does not check for cycles in <var>x<sub>i</sub></var> and <var>x<sub>i+1</sub></var>, but finishes when the difference between them drops below <var>ε</var>.  The initial guess can be provided, but defaults to <var>n-1</var>.


```lisp
(defun nth-root (n a &optional (epsilon .0001) (guess (1- n)))
  (assert (and (> n 1) (> a 0)))
  (flet ((next (x)
           (/ (+ (* (1- n) x)
                 (/ a (expt x (1- n))))
              n)))
    (do* ((xi guess xi+1)
          (xi+1 (next xi) (next xi)))
         ((< (abs (- xi+1 xi)) epsilon) xi+1))))
```


<code>nth-root</code> may return rationals rather than floating point numbers, so easy checking for correctness may require coercion to floats.  For instance,


```lisp
(let* ((r (nth-root 3 10))
       (rf (coerce r 'float)))
  (print (* r r r ))
  (print (* rf rf rf)))
```


produces the following output.


```txt
1176549099958810982335712173626176/117654909634627320192156007194483
10.0
```



## D


```d
import std.stdio, std.math;

real nthroot(in int n, in real A, in real p=0.001) pure nothrow {
    real[2] x = [A, A / n];
    while (abs(x[1] - x[0]) > p)
        x = [x[1], ((n - 1) * x[1] + A / (x[1] ^^ (n-1))) / n];
    return x[1];
}

void main() {
    writeln(nthroot(10, 7131.5 ^^ 10));
    writeln(nthroot(6, 64));
}
```

{{out}}

```txt
7131.5
2
```



## Delphi


```delphi

USES
   Math;

function NthRoot(A, Precision: Double; n: Integer): Double;
var
   x_p, X: Double;
begin
   x_p := Sqrt(A);
   while Abs(A - Power(x_p, n)) > Precision do
   begin
      x := (1/n) * (((n-1) * x_p) + (A/(Power(x_p, n - 1))));
      x_p := x;
   end;
   Result := x_p;
end;

```



## E


Rather than choosing an arbitrary precision, this implementation continues until a cycle in the iterated result is found, thus producing an answer almost as precise as the number type.

(Disclaimer: This was not written by a numerics expert; there may be reasons this is a bad idea. Also, it might be that cycles are always of length 2, which would reduce the amount of calculation needed by 2/3.)


```e
def nthroot(n, x) {
  require(n > 1 && x > 0)
  def np := n - 1
  def iter(g) { return (np*g + x/g**np) / n }
  var g1 := x
  var g2 := iter(g1)
  while (!(g1 <=> g2)) {
    g1 := iter(g1)
    g2 := iter(iter(g2))
  }
  return g1
}
```



## EasyLang


<lang>floatvars
func power x n% . r .
  r = 1
  for i% range n%
    r *= x
  .
.
func nth_root x n% . r .
  r = 2
  repeat
    call power r n% - 1 p
    d = (x / p - r) / n%
    r += d
    until absf d < 0.0001
  .
.
call power 3.1416 10 x
call nth_root x 10 r
print r
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def nth_root(n, x, precision \\ 1.0e-5) do
    f = fn(prev) -> ((n - 1) * prev + x / :math.pow(prev, (n-1))) / n end
    fixed_point(f, x, precision, f.(x))
  end

  defp fixed_point(_, guess, tolerance, next) when abs(guess - next) < tolerance, do: next
  defp fixed_point(f, _, tolerance, next), do: fixed_point(f, next, tolerance, f.(next))
end

Enum.each([{2, 2}, {4, 81}, {10, 1024}, {1/2, 7}], fn {n, x} ->
  IO.puts "#{n} root of #{x} is #{RC.nth_root(n, x)}"
end)
```


{{out}}

```txt

2 root of 2 is 1.4142135623746899
4 root of 81 is 3.0
10 root of 1024 is 2.00000000022337
0.5 root of 7 is 48.99999999999993

```



## Erlang

Done by finding the fixed point of a function, which aims to find a value of <var>x</var> for which <var>f(x)=x</var>:

```erlang
fixed_point(F, Guess, Tolerance) ->
    fixed_point(F, Guess, Tolerance, F(Guess)).
fixed_point(_, Guess, Tolerance, Next) when abs(Guess - Next) < Tolerance ->
    Next;
fixed_point(F, _, Tolerance, Next) ->
    fixed_point(F, Next, Tolerance, F(Next)).
```

The nth root function algorithm defined on the wikipedia page linked above can advantage of this:

```erlang
nth_root(N, X) -> nth_root(N, X, 1.0e-5).
nth_root(N, X, Precision) ->
    F = fun(Prev) -> ((N - 1) * Prev + X / math:pow(Prev, (N-1))) / N end,
    fixed_point(F, X, Precision).
```



## Excel

This will work in any spreadsheet that uses Excel-compatible expressions -- i.e. [[wp:KOffice|KOffice]]'s KCells (formerly KSpread), [[wp:Calligra Tables|Calligra Tables]], and [[wp:OpenOffice.org Calc|OpenOffice.org Calc]].

Beside the obvious;

```Excel
=A1^(1/B1)
```

*Cell A1 is the base.
*Cell B1 is the exponent.
*Cell A2 is the first guess (any non-zero number will do).
In cell A3, enter this formula:
 =((($B$1-1)*A2)+($A$1/(A2^($B$1-1))))/$B$1
Copy A3 down until you get 2 cells with the same value. (Once there are two visibly-identical cells, all cells below those two will also be identical.)

For example, here we calculate the cube root of 100:
{| class="wikitable"
|-
|
| '''A'''
| '''B'''
|-
| '''1'''
| 100
| 3
|-
| '''2'''
| 7
| (first guess)
|-
| '''3'''
| 5.346938776
|
|-
| '''4'''
| 4.730544697
|
|-
| '''5'''
| 4.643251125
|
|-
| '''6'''
| 4.641589429
|
|-
| '''7'''
| 4.641588834
|
|-
| '''8'''
| 4.641588834
|
|}

Alternately, Excel could use the [[#BASIC|BASIC]] example above as VBA code, deleting A2 and replacing A3's formula with something like this:
 =RootX(A1,B1,.00000001)

=={{header|F_Sharp|F#}}==

```fsharp

let nthroot n A =
    let rec f x =
        let m = n - 1.
        let x' = (m * x + A/x**m) / n
        match abs(x' - x) with
        | t when t < abs(x * 1e-9) -> x'
        | _ -> f x'
    f (A / double n)

[<EntryPoint>]
let main args =
    if args.Length <> 2 then
        eprintfn "usage: nthroot n A"
        exit 1
    let (b, n) = System.Double.TryParse(args.[0])
    let (b', A) = System.Double.TryParse(args.[1])
    if (not b) || (not b') then
        eprintfn "error: parameter must be a number"
        exit 1
    printf "%A" (nthroot n A)
    0

```


Compiled using <em>fsc nthroot.fs</em> example output:
```txt

nthroot 0.5 7
49.0
```



## Factor

{{trans|Forth}}

```factor
USING: kernel locals math math.functions prettyprint ;

:: th-root ( a n -- a^1/n )
    a [
        a over n 1 - ^ /f
          over n 1 - *
        + n /f
        swap over 1e-5 ~ not
    ] loop ;

34 5 th-root .   ! 2.024397458499888
34 5 recip ^ .   ! 2.024397458499888
```



## Forth


```forth
: th-root { F: a F: n -- a^1/n }
  a
  begin
    a fover n 1e f- f** f/
      fover n 1e f- f*
    f+ n f/
    fswap fover 1e-5 f~
  until ;

34e 5e th-root f.   \ 2.02439745849989
34e 5e 1/f f** f.   \ 2.02439745849989
```



## Fortran


```fortran
program NthRootTest
  implicit none

  print *, nthroot(10, 7131.5**10)
  print *, nthroot(5, 34.0)

contains

  function nthroot(n, A, p)
    real :: nthroot
    integer, intent(in)        :: n
    real, intent(in)           :: A
    real, intent(in), optional :: p

    real :: rp, x(2)

    if ( A < 0 ) then
       stop "A < 0"       ! we handle only real positive numbers
    elseif ( A == 0 ) then
       nthroot = 0
       return
    end if

    if ( present(p) ) then
       rp = p
    else
       rp = 0.001
    end if

    x(1) = A
    x(2) = A/n   ! starting "guessed" value...

    do while ( abs(x(2) - x(1)) > rp )
       x(1) = x(2)
       x(2) = ((n-1.0)*x(2) + A/(x(2) ** (n-1.0)))/real(n)
    end do

    nthroot = x(2)

  end function nthroot

end program NthRootTest
```


## FreeBASIC


```freebasic
' version 14-01-2019
' compile with: fbc -s console

Function nth_root(n As Integer, number As Double) As Double

    Dim As Double a1 = number / n, a2 , a3

    Do
        a3 = Abs(a2 - a1)
        a2 = ((n -1) * a1 + number / a1 ^ (n -1)) / n
        Swap a1, a2
    Loop Until Abs(a2 - a1) = a3

    Return a1

End Function

' ------=< MAIN >=------

Dim As UInteger n
Dim As Double tmp

Print
Print "   n    5643 ^ 1 / n     nth_root ^ n"
Print " ------------------------------------"
For n = 3 To 11 Step 2
    tmp = nth_root(n, 5643)
    Print Using " ###    ###.########    ####.########"; n; tmp; tmp ^ n
Next

Print
For n = 25 To 125 Step 25
    tmp = nth_root(n, 5643)
    Print Using " ###    ###.########    ####.########"; n; tmp; tmp ^ n
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
   n    5643 ^ 1 / n     nth_root ^ n
 ------------------------------------
   3     17.80341642    5643.00000000
   5      5.62732516    5643.00000000
   7      3.43502583    5643.00000000
   9      2.61116581    5643.00000000
  11      2.19303907    5643.00000000

  25      1.41273402    5643.00000000
  50      1.18858488    5643.00000000
  75      1.12207047    5643.00000000
 100      1.09022240    5643.00000000
 125      1.07154911    5643.00000000
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 8

local fn NthRoot( root as long, a as long, precision as double ) as double
dim as double x0, x1

x0 = a : x1 = a /root
while ( abs( x1 - x0 ) > precision )
x0 = x1
x1 = ( ( root -1.0 ) * x1 + a / x1 ^ ( root -1.0 ) ) /root
wend
end fn = x1

print " 125th Root of 5643 Precision .001",   using "#.###############";  fn NthRoot( 125, 5642, 0.001   )
print " 125th Root of 5643 Precision .001",   using "#.###############";  fn NthRoot( 125, 5642, 0.001   )
print " 125th Root of 5643 Precision .00001", using "#.###############";  fn NthRoot( 125, 5642, 0.00001 )
print "  Cube Root of   27 Precision .00001", using "#.###############";  fn NthRoot(   3,   27, 0.00001 )
print "Square Root of    2 Precision .00001", using "#.###############";  fn NthRoot(   2,    2, 0.00001 )
print "Square Root of    2 Precision .00001", using "#.###############";  sqr(2)  // Processor floating point calc deviation
print "  10th Root of 1024 Precision .00001", using "#.###############";  fn NthRoot(  10, 1024, 0.00001 )
print "   5th Root of   34 Precision .00001", using "#.###############";  fn NthRoot(   5,   34, 0.00001 )

```

Output:

```txt

 125th Root of 5643 Precision .001      1.071559602191682
 125th Root of 5643 Precision .001      1.071559602191682
 125th Root of 5643 Precision .00001    1.071547591944772
  Cube Root of   27 Precision .00001    3.000000000000002
Square Root of    2 Precision .00001    1.414213562374690
Square Root of    2 Precision .00001    1.414213562373095
  10th Root of 1024 Precision .00001    2.000000000000000
   5th Root of   34 Precision .00001    2.024397458499885

```



## Go


```go
func root(a float64, n int) float64 {
    n1 := n - 1
    n1f, rn := float64(n1), 1/float64(n)
    x, x0 := 1., 0.
    for {
        potx, t2 := 1/x, a
        for b := n1; b > 0; b >>= 1 {
            if b&1 == 1 {
                t2 *= potx
            }
            potx *= potx
        }
        x0, x = x, rn*(n1f*x+t2)
        if math.Abs(x-x0)*1e15 < x {
            break
        }
    }
    return x
}
```


The above version is for 64 bit wide floating point numbers. The following uses `math/big` Float to implement this same function with 256 bits of precision.

''A set of wrapper functions around the somewhat muddled big math library functions is used to make the main function more readable, and also it was necessary to create a power function (Exp) as the library also lacks this function.'' '''The exponent in the limit must be at least one less than the number of bits of precision of the input value or the function will enter an infinite loop!'''


```go

import "math/big"

func Root(a *big.Float, n uint64) *big.Float {
	limit := Exp(New(2), 256)
	n1 := n-1
	n1f, rn := New(float64(n1)), Div(New(1.0), New(float64(n)))
	x, x0 := New(1.0), Zero()
	_ = x0
	for {
		potx, t2 := Div(New(1.0), x), a
		for b:=n1; b>0; b>>=1 {
			if b&1 == 1 {
				t2 = Mul(t2, potx)
			}
			potx = Mul(potx, potx)
		}
		x0, x = x, Mul(rn, Add(Mul(n1f, x), t2) )
		if Lesser(Mul(Abs(Sub(x, x0)), limit), x) { break }
	}
	return x
}

func Abs(a *big.Float) *big.Float {
	return Zero().Abs(a)
}

func Exp(a *big.Float, e uint64) *big.Float {
	result := Zero().Copy(a)
	for i:=uint64(0); i<e-1; i++ {
		result = Mul(result, a)
	}
	return result
}

func New(f float64) *big.Float {
	r := big.NewFloat(f)
	r.SetPrec(256)
	return r
}

func Div(a, b *big.Float) *big.Float {
	return Zero().Quo(a, b)
}

func Zero() *big.Float {
	r := big.NewFloat(0.0)
	r.SetPrec(256)
	return r
}

func Mul(a, b *big.Float) *big.Float {
	return Zero().Mul(a, b)
}

func Add(a, b *big.Float) *big.Float {
	return Zero().Add(a, b)
}

func Sub(a, b *big.Float) *big.Float {
	return Zero().Sub(a, b)
}

func Lesser(x, y *big.Float) bool {
	return x.Cmp(y) == -1
}

```



## Groovy

Solution:

```groovy
import static Constants.tolerance
import static java.math.RoundingMode.HALF_UP

def root(double base, double n) {
    double xOld = 1
    double xNew = 0
    while (true) {
        xNew = ((n - 1) * xOld + base/(xOld)**(n - 1))/n
    if ((xNew - xOld).abs() < tolerance) { break }
        xOld = xNew
    }
    (xNew as BigDecimal).setScale(7, HALF_UP)
}

```


Test:

```groovy
class Constants {
    static final tolerance = 0.00001
}

print '''
   Base   Power  Calc'd Root  Actual Root
-------  ------  -----------  -----------
'''
def testCases = [
    [b:32.0, n:5.0, r:2.0],
    [b:81.0, n:4.0, r:3.0],
    [b:Math.PI**2, n:4.0, r:Math.PI**(0.5)],
    [b:7.0, n:0.5, r:49.0],
]

testCases.each {
    def r = root(it.b, it.n)
    printf('%7.4f  %6.4f  %11.4f  %11.4f\n',
        it.b, it.n, r, it.r)
    assert (r - it.r).abs() <= tolerance
}
```


Output:

```txt
   Base   Power  Calc'd Root  Actual Root
-------  ------  -----------  -----------
32.0000  5.0000       2.0000       2.0000
81.0000  4.0000       3.0000       3.0000
 9.8696  4.0000       1.7725       1.7725
 7.0000  0.5000      49.0000      49.0000
```



## Haskell


Function exits when there's no difference between two successive values.

```Haskell
n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)
```

Use:

```txt
*Main> 2 `nthRoot` 2
1.414213562373095

*Main> 5 `nthRoot` 34
2.024397458499885

*Main> 10 `nthRoot` (734^10)
734.0

*Main> 0.5 `nthRoot` 7
49.0
```



Or, in applicative terms, with formatted output:


```haskell
nthRoot :: Double -> Double -> Double
nthRoot n x =
  fst $
  until
    (uncurry (==))
    (((,) <*> ((/ n) . ((+) <$> ((n - 1) *) <*> (x /) . (** (n - 1))))) . snd)
    (x, x / n)

-- TESTS --------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Nth roots:"
    (\(a, b) -> show a ++ " `nthRoot` " ++ show b)
    show
    (uncurry nthRoot)
    [(2, 2), (5, 34), (10, 734 ^ 10), (0.5, 7)]

-- FORMAT OF RESULTS --------------------------------------
fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum (length . xShow <$> xs)
      rjust n c = drop . length <*> (replicate n c ++)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs
```

{{Out}}

```txt
Nth roots:
                  2.0 `nthRoot` 2.0 -> 1.414213562373095
                 5.0 `nthRoot` 34.0 -> 2.0243974584998847
10.0 `nthRoot` 4.539004352165717e28 -> 734.0
                  0.5 `nthRoot` 7.0 -> 49.0
```



## HicEst


```HicEst
WRITE(Messagebox) NthRoot(5, 34)
WRITE(Messagebox) NthRoot(10, 7131.5^10)

FUNCTION NthRoot(n, A)
   REAL :: prec = 0.001

   IF( (n > 0) * (A > 0) ) THEN
       NthRoot = A / n
       DO i = 1, 1/prec
         x = ((n-1)*NthRoot + A/(NthRoot^(n-1))) / n
         IF( ABS(x - NthRoot) <= prec ) THEN
             RETURN
         ENDIF
         NthRoot = x
       ENDDO
   ENDIF

   WRITE(Messagebox, Name) 'Cannot solve problem for:', prec, n, A
END
```


=={{header|Icon}} and {{header|Unicon}}==
All Icon/Unicon reals are double precision.

```Icon
procedure main()
   showroot(125,3)
   showroot(27,3)
   showroot(1024,10)
   showroot(39.0625,4)
   showroot(7131.5^10,10)
end

procedure showroot(a,n)
   printf("%i-th root of %i = %i\n",n,a,root(a,n))
end

procedure root(a,n,p) #: finds the n-th root of the number a to precision p
   if n < 0 | type(n) !== "integer" then runerr(101,n)
   if a < 0 then runerr(205,a)
   /p := 1e-14                  # precision
   xn := a / real(n)            # initial guess
   while abs(a - xn^n) > p do
      xn := ((n - 1) * (xi := xn) + a / (xi ^ (n-1))) / real(n)
   return xn
end

link printf
```


Output:
```txt
3-th root of 125 = 5.0
3-th root of 27 = 3.0
10-th root of 1024 = 2.0
4-th root of 39.0625 = 2.5
10-th root of 3.402584077894253e+038 = 7131.5
```



## J

{{trans|E}}

J has a built in Nth root primitive, '''<tt>%:</tt>'''.  For example, '''<tt>7131.5 = 10 %: 7131.5^10</tt>'''.  Also, the exponentiation primitive supports exponents < 1, e.g. '''<tt>7131.5 = (7131.5^10)^(1%10)</tt>'''.

But, since the [[Talk:Nth_root_algorithm#Comparison_to_Non-integer_Exponentiation|talk page discourages]] using built-in facilities, here is a reimplementation, using the [[#E|E]] algorithm:


```j
   '`N X NP' =.  (0 { [)`(1 { [)`(2 { [)
   iter      =.  N %~ (NP * ]) + X % ] ^ NP
   nth_root  =:  (, , _1+[) iter^:_ f. ]
   10 nth_root 7131.5^10
7131.5
```



## Java

{{trans|Fortran}}

```java
public static double nthroot(int n, double A) {
	return nthroot(n, A, .001);
}
public static double nthroot(int n, double A, double p) {
	if(A < 0) {
		System.err.println("A < 0");// we handle only real positive numbers
		return -1;
	} else if(A == 0) {
		return 0;
	}
	double x_prev = A;
	double x = A / n;  // starting "guessed" value...
	while(Math.abs(x - x_prev) > p) {
		x_prev = x;
		x = ((n - 1.0) * x + A / Math.pow(x, n - 1.0)) / n;
	}
	return x;
}
```

{{trans|E}}

```java
public static double nthroot(int n, double x) {
  assert (n > 1 && x > 0);
  int np = n - 1;
  double g1 = x;
  double g2 = iter(g1, np, n, x);
  while (g1 != g2) {
    g1 = iter(g1, np, n, x);
    g2 = iter(iter(g2, np, n, x), np, n, x);
  }
  return g1;
}

private static double iter(double g, int np, int n, double x) {
  return (np * g + x / Math.pow(g, np)) / n;
}
```



## JavaScript

Gives the ''n'':nth root of ''num'', with precision ''prec''. (''n'' defaults to 2 [e.g. sqrt], ''prec'' defaults to 12.)


```javascript
function nthRoot(num, nArg, precArg) {
  var n = nArg || 2;
  var prec = precArg || 12;

  var x = 1; // Initial guess.
  for (var i=0; i<prec; i++) {
    x = 1/n * ((n-1)*x + (num / Math.pow(x, n-1)));
  }

  return x;
}
```



## jq


```jq
# An iterative algorithm for finding: self ^ (1/n) to the given
# absolute precision if "precision" > 0, or to within the precision
# allowed by IEEE 754 64-bit numbers.

# The following implementation handles underflow caused by poor estimates.
def iterative_nth_root(n; precision):
  def abs: if . < 0 then -. else . end;
  def sq: .*.;
  def pow(p): . as $in | reduce range(0;p) as $i (1; . * $in);
    def _iterate: # state: [A, x1, x2, prevdelta]
      .[0] as $A | .[1] as $x1 | .[2] as $x2 | .[3] as $prevdelta
      | ( $x2 | pow(n-1)) as $power
      | if $power <= 2.155094094640383e-309
        then  [$A, $x1, ($x1 + $x2)/2, n] | _iterate
	else (((n-1)*$x2 + ($A/$power))/n) as $x1
	| (($x1 - $x2)|abs) as $delta
        | if (precision == 0 and $delta == $prevdelta and $delta < 1e-15)
             or (precision > 0 and $delta <= precision) or $delta == 0 then $x1
          else [$A, $x2, $x1, $delta] | _iterate
          end
        end
    ;
    if n == 1 then .
    elif . == 0 then 0
    elif . < 0 then error("iterative_nth_root: input \(.) < 0")
    elif n != (n|floor) then error("iterative_nth_root: argument \(n) is not an integer")
    elif n == 0 then error("iterative_nth_root(0): domain error")
    elif n < 0 then 1/iterative_nth_root(-n; precision)
    else [., ., (./n), n, 0]  | _iterate
    end
;
```

'''Example''':
Compare the results of iterative_nth_root and nth_root implemented using builtins

```jq
def demo(x):
  def nth_root(n): log / n | exp;
  def lpad(n): tostring | (n - length) * " " + .;
  . as $in
  | "\(x)^(1/\(lpad(5))): \(x|nth_root($in)|lpad(18)) vs \(x|iterative_nth_root($in; 1e-10)|lpad(18)) vs \(x|iterative_nth_root($in; 0))"
;

# 5^m for various values of n:
"5^(1/   n):             builtin       precision=1e-10           precision=0",
( (1,-5,-3,-1,1,3,5,1000,10000) | demo(5))
```

{{Out}}

```sh
$ jq -n -r -f nth_root_machine_precision.jq
5^(1/   n):             builtin       precision=1e-10           precision=0
5^(1/    1):  4.999999999999999 vs                  5 vs 5
5^(1/   -5): 0.7247796636776955 vs 0.7247796636776956 vs 0.7247796636776955
5^(1/   -3): 0.5848035476425733 vs 0.5848035476425731 vs 0.5848035476425731
5^(1/   -1):                0.2 vs                0.2 vs 0.2
5^(1/    1):  4.999999999999999 vs                  5 vs 5
5^(1/    3):  1.709975946676697 vs  1.709975946676697 vs 1.709975946676697
5^(1/    5): 1.3797296614612147 vs 1.3797296614612147 vs 1.379729661461215
5^(1/ 1000): 1.0016107337527294 vs 1.0016107337527294 vs 1.0016107337527294
5^(1/10000): 1.0001609567433902 vs 1.0001609567433902 vs 1.0001609567433902

```




## Julia

{{works with|Julia|1.2}}
Julia has a built-in exponentiation function <code>A^(1 / n)</code>, but the specification calls for us to use Newton's method (which we iterate until the limits of machine precision are reached):


```julia
function nthroot(n::Integer, r::Real)
    r < 0 || n == 0 && throw(DomainError())
    n < 0 && return 1 / nthroot(-n, r)
    r > 0 || return 0
    x = r / n
    prevdx = r
    while true
        y = x ^ (n - 1)
        dx = (r - y * x) / (n * y)
        abs(dx) ≥ abs(prevdx) && return x
        x += dx
        prevdx = dx
    end
end

@show nthroot.(-5:2:5, 5.0)
@show nthroot.(-5:2:5, 5.0) - 5.0 .^ (1 ./ (-5:2:5))
```


{{out}}

```txt
nthroot.(-5:2:5, 5.0) = [0.7247796636776955, 0.5848035476425731, 0.2, 5.0, 1.709975946676697, 1.379729661461215]
nthroot.(-5:2:5, 5.0) - 5.0 .^ (1 ./ (-5:2:5)) = [0.0, -1.1102230246251565e-16, 0.0, 0.0, 0.0, 0.0]
```



## Kotlin

{{trans|E}}

```scala
// version 1.0.6

fun nthRoot(x: Double, n: Int): Double {
    if (n < 2) throw IllegalArgumentException("n must be more than 1")
    if (x <= 0.0) throw IllegalArgumentException("x must be positive")
    val np = n - 1
    fun iter(g: Double) = (np * g + x / Math.pow(g, np.toDouble())) / n
    var g1 = x
    var g2 = iter(g1)
    while (g1 != g2) {
        g1 = iter(g1)
        g2 = iter(iter(g2))
    }
    return g1
}

fun main(args: Array<String>) {
   val numbers = arrayOf(1728.0 to 3, 1024.0 to 10, 2.0 to 2)
   for (number in numbers)
       println("${number.first} ^ 1/${number.second}\t = ${nthRoot(number.first, number.second)}")
}
```


{{out}}

```txt

1728.0 ^ 1/3     = 12.0
1024.0 ^ 1/10    = 2.0
2.0 ^ 1/2        = 1.414213562373095

```



## Liberty BASIC


```lb

print "First estimate is: ",        using( "#.###############",  NthRoot( 125, 5642, 0.001  ));
print "    ... and better is: ",    using( "#.###############",  NthRoot( 125, 5642, 0.00001))
print "125'th root of 5642 by LB's exponentiation operator is "; using( "#.###############", 5642^(1 /125))

print "27^(1 / 3)",                 using( "#.###############",  NthRoot(   3,   27, 0.00001))
print "2^(1 / 2)",                  using( "#.###############",  NthRoot(   2,    2, 0.00001))
print "1024^(1 /10)",               using( "#.###############",  NthRoot(  10, 1024, 0.00001))

wait

function NthRoot( n, A, p)
  x( 0) =A
  x( 1) =A /n
  while abs( x( 1) -x( 0)) >p
    x( 0) =x( 1)
    x( 1) =( ( n -1.0) *x( 1) +A /x( 1)^( n -1.0)) /n
  wend
  NthRoot =x( 1)
end function

end


```

 First estimate is:          1.071559602191682    ... and better is:   1.071547591944771
 125'th root of 5642 by LB's exponentiation operator is 1.071547591944767
 27^(1 / 3)    3.000000000000002
 2^(1 / 2)     1.414213562374690
 1024^(1 /10)  2.000000000000000


## Lingo


```lingo
on nthRoot (x, root)
  return power(x, 1.0/root)
end
```


```lingo
the floatPrecision = 8 -- only about display/string cast of floats
put nthRoot(4, 4)
-- 1.41421356
```



## Logo


```logo
to about :a :b
  output and [:a - :b < 1e-5] [:a - :b > -1e-5]
end

to root :n :a [:guess :a]
  localmake "next ((:n-1) * :guess + :a / power :guess (:n-1)) / n
  if about :guess :next [output :next]
  output (root :n :a :next)
end

show root 5 34   ; 2.02439745849989
```



## Lua


```Lua

function nroot(root, num)
  return num^(1/root)
end

```



## M2000 Interpreter

Using stack statements PUSH, READ, OVER, SHIFT, DROP, NUMBER, FLUSH

```txt

Flush empty stack
Over 2 copy 2nd as new top (so 2nd now is 3rd)
Over 2,2 repeat Over 2 two times.
Shift 2 send top to 2nd, and 2nd to top (1st) (there is a SHFITBACK to revesre action)
Drop drop top
Number get top if is number, else raise error
Read, read a variable form top.
Functions parameters works with a read too
     Function Root {
             Read a, n%, d as double=1.e-4
      ......
      }
because we can send any type and number if function, interpreter can make conversions if we declare that,
or if it not possible (no conversion done to a numeric variable if a string is in top of stack) we get an error.
Also if we send less values, and we didn't initialize variable before, we get error too.
Here we need to flush stack for other parameters if from an error anyone put more arguments.
(interpreter never count before call a user function, except for calling events by using event object,
so there there is a signature to follow)


n% is double inside.

```




```M2000 Interpreter

Module Checkit {
      Function Root (a, n%, d as double=1.e-4) {
             if n%=0 then Error "Division by zero: 1/0"
             if a<=0 then Error "Negative or zero number"
             if n%=1 then = a : exit
             Flush
             n2=1-1/n%:a/=n%:n%--:Push a
             {    Push 1: For i=1 to n% {Over 2 :Push Number*Number}
                  Over 2 : Push n2*Number + a/Number
                  Shift 2: Over 2, 2 :if Abs(Number-Number)>d Then loop
                  Drop
             }  Read a : = a
      }
      Print "square root single"
      Print root(1.3346767~, 2, 1.e-9)
      Print "square double"
      Print root(1.3346767, 2, 1.e-9)
      Print "square root decimal"
      Print root(1.3346767@, 2, 1.e-9)
      Print "internal square root, double"
      Print  1.3346767^(1/2)
      Print sqrt(1.3346767)
}
Checkit

```



## Maple


The <code>root</code> command performs this task.

```Maple

root(1728, 3);

root(1024, 10);

root(2.0, 2);

```


Output:

```txt

                                     12

                                      2

                                 1.414213562

```



## Mathematica


```Mathematica
Root[A,n]
```



## MATLAB


```MATLAB
function answer = nthRoot(number,root)

    format long

    answer = number / root;
    guess = number;

    while not(guess == answer)
       guess = answer;
       answer = (1/root)*( ((root - 1)*guess) + ( number/(guess^(root - 1)) ) );
    end

end
```


Sample Output:

```MATLAB>>
 nthRoot(2,2)

ans =

   1.414213562373095
```



## Maxima


```maxima
nth_root(a, n) := block(
   [x, y, d, p: fpprec],
   fpprec: p + 10,
   x: bfloat(a),
   eps: 10.0b0^-p,
   y: do (
      d: bfloat((a / x^(n - 1) - x) / n),
      if abs(d) < eps * x then return(x),
      x: x + d
   ),
   fpprec: p,
   bfloat(y)
)$
```



## Metafont

Metafont does not use IEEE floating point and we can't go beyond 0.0001 or it will loop forever.

```metafont
vardef mnthroot(expr n, A) =
  x0 := A / n;
  m := n - 1;
  forever:
    x1 := (m*x0 + A/(x0 ** m)) / n;
    exitif abs(x1 - x0) < abs(x0 * 0.0001);
    x0 := x1;
  endfor;
  x1
enddef;

primarydef n nthroot A = mnthroot(n, A) enddef;

show 5 nthroot 34;  % 2.0244
show 0.5 nthroot 7; % 49.00528

bye
```


=={{header|МК-61/52}}==
<lang>1/x	<->	x^y	С/П
```

Instruction: ''number'' ^ ''degree'' В/О С/П


## NetRexx

{{trans|REXX}}

```netrexx

/*NetRexx program to calculate the  Nth root of  X,  with  DIGS  accuracy. */
class nth_root

  method main(args=String[]) static
    if args.length < 2 then
      do
	say "at least 2 arguments expected"
	exit
      end
    x = args[0]
    root = args[1]
    if args.length > 2 then digs = args[2]

    if root=='' then root=2
    if digs = null, digs = '' then digs=20
    numeric digits digs
    say '     x	= ' x
    say '  root	= ' root
    say 'digits	= ' digs
    say 'answer	= ' root(x,root,digs)

  method root(x,r,digs) static --procedure; parse arg x,R 1 oldR  /*assign 2nd arg-->r and rOrig.  */
    /*this subroutine will use the   */
    /*digits from the calling prog.  */
    /*The default digits is  9.      */
    R = r
    oldR = r
    if r=0 then do
      say
      say '*** error! ***'
      say "a root of zero can't be specified."
      say
      return '[n/a]'
    end

    R=R.abs()                              /*use absolute value of root.    */

    if x<0 & (R//2==0) then do
      say
      say '*** error! ***'
      say "an even root can't be calculated for a" -
      'negative number,'
      say 'the result would be complex.'
      say
      return '[n/a]'
    end

    if x=0 | r=1 then return x/1           /*handle couple of special cases.*/
    Rm1=R-1                                /*just a fast version of  ROOT-1 */
    oldDigs=digs                           /*get the current number of digs.*/
    dm=oldDigs+5                           /*we need a little guard room.   */
    ax=x.abs()                             /*the absolute value of  X.      */
    g=(ax+1)/r**r                          /*take a good stab at 1st guess. */
 --   numeric fuzz 3                         /*fuzz digits for higher roots.  */
    d=5                                    /*start with only five digits.   */
    /*each calc doubles precision.   */

    loop forever

      d=d+d
      if d>dm then d = dm                        /*double the digits, but not>DM. */
      numeric digits d                     /*tell REXX to use   D   digits. */
      old=0                                /*assume some kind of old guess. */

      loop forever
	_=(Rm1*g**R+ax)/R/g**rm1           /*this is the nitty-gritty stuff.*/
	if _=g | _=old then leave          /*computed close to this before? */
	old=g                              /*now, keep calculation for OLD. */
	g=_                                /*set calculation to guesstimate.*/
      end

      if d==dm then leave                  /*found the root for DM digits ? */
    end

    _=g*x.sign()                           /*correct the sign (maybe).      */
    if oldR<0 then return _=1/_            /*root < 0 ?    Reciprocal it is.*/
    numeric digits oldDigs                 /*re-instate the original digits.*/
    return _/1                             /*normalize the number to digs.  */


```



## NewLISP


```NewLISP
(define (nth-root n a)
  (let ((x1 a)
	(x2 (div a n)))
    (until (= x1 x2)
      (setq x1 x2
	    x2 (div
		(add
		 (mul x1 (- n 1))
		 (div a (pow x1 (- n 1))))
		n)))
    x2))
```



## Nim


```nim
import math

proc nthroot(a, n): float =
  var n = float(n)
  result = a
  var x = a / n
  while abs(result-x) > 10e-15:
    x = result
    result = (1.0/n) * (((n-1)*x) + (a / pow(x, n-1)))

echo nthroot(34.0, 5)
echo nthroot(42.0, 10)
echo nthroot(5.0, 2)
```

Output:

```txt
2.0243974584998852e+00
1.4531984602822678e+00
2.2360679774997898e+00
```



## Objeck

{{trans|C}}

```objeck
class NthRoot {
  function : Main(args : String[]) ~ Nil {
    NthRoot(5, 34, .001)->PrintLine();
  }

  function : NthRoot(n : Int, A: Float, p : Float) ~ Float {
    x := Float->New[2];
    x[0] := A;
    x[1] := A / n;

    while((x[1] - x[0])->Abs() > p) {
      x[0] := x[1];
      x[1] := ((n - 1.0) * x[1] + A / x[1]->Power(n - 1.0)) / n;
    };

    return x[1];
  }
}
```



## OCaml

{{trans|C}}

```ocaml
let nthroot ~n ~a ?(tol=0.001) () =
   let nf = float n in let nf1 = nf -. 1.0 in
   let rec iter x =
      let x' = (nf1 *. x +. a /. (x ** nf1)) /. nf in
      if tol > abs_float (x -. x') then x' else iter x' in
   iter 1.0
;;

let () =
  Printf.printf "%g\n" (nthroot 10 (7131.5 ** 10.0) ());
  Printf.printf "%g\n" (nthroot 5 34.0 ());
;;
```



## Octave

Octave has it's how <tt>nthroot</tt> function.

```octave

  r = A.^(1./n)

```


Here it is another implementation (after Tcl)

{{trans|Tcl}}

```octave
function r = m_nthroot(n, A)
  x0 = A / n;
  m  = n - 1;
  while(1)
    x1 = (m*x0 + A./ x0 .^ m) / n;
    if ( abs(x1-x0) < abs(x0 * 1e-9) )
      r = x1;
      return
    endif
    x0 = x1;
  endwhile
endfunction
```


Here is an more elegant way by computing the successive differences in an explicit way:

```octave
function r = m_nthroot(n, A)
  r = A / n;
  m = n - 1;
  do
    d = (A ./ r .^ m - r) / n;
    r+= d;
  until (abs(d) < abs(r * 1e-9))
endfunction
```


Show its usage and the built-in <tt>nthroot</tt> function


```octave
m_nthroot(10, 7131.5 .^ 10)
nthroot(7131.5 .^ 10, 10)
m_nthroot(5, 34)
nthroot(34, 5)
m_nthroot(0.5, 7)
nthroot(7, .5)
```



## Oforth



```Oforth
Float method: nthroot(n)
   1.0 doWhile: [ self over n 1 - pow / over - n / tuck + swap 0.0 <> ] ;
```


{{out}}

```txt

734 10.0 powf nthroot(10) println
734

2.0 nthroot(2) println
1.41421356237309

34.0 nthroot(5) println
2.02439745849989

```



## Oz


```oz
declare
  fun {NthRoot NInt A}
     N = {Int.toFloat NInt}

     fun {Next X}
        ( (N-1.0)*X + A / {Pow X N-1.0} ) / N
     end
  in
     {Until Value.'==' Next A/N}
  end

  fun {Until P F X}
     case {F X}
     of NX andthen {P NX X} then X
     [] NX then {Until P F NX}
     end
  end
in
  {Show {NthRoot 2 2.0}}
```



## PARI/GP


```parigp
root(n,A)=A^(1/n);
```



## Pascal

See [[Nth_root#Delphi | Delphi]]


## Perl

{{trans|Tcl}}

```perl
use strict;

sub nthroot ($$)
{
    my ( $n, $A ) = @_;

    my $x0 = $A / $n;
    my $m = $n - 1.0;
    while(1) {
	my $x1 = ($m * $x0 + $A / ($x0 ** $m)) / $n;
	return $x1 if abs($x1 - $x0) < abs($x0 * 1e-9);
	$x0 = $x1;
    }
}
```



```perl
print nthroot(5, 34), "\n";
print nthroot(10, 7131.5 ** 10), "\n";
print nthroot(0.5, 7), "\n";
```



## Perl 6



```perl6
sub nth-root ($n, $A, $p=1e-9)
{
    my $x0 = $A / $n;
    loop {
        my $x1 = (($n-1) * $x0 + $A / ($x0 ** ($n-1))) / $n;
        return $x1 if abs($x1-$x0) < abs($x0 * $p);
        $x0 = $x1;
    }
}

say nth-root(3,8);
```



## Phix

{{trans|AWK}}
(main loop)
{{trans|C}}
(use of pow_ instead of power)


```Phix
function pow_(atom x, integer e)
    atom r = 1
    for i=1 to e do
        r *= x
    end for
    return r
end function

function nth_root(atom y,n)
atom eps = 1e-15   -- relative accuracy
atom x = 1
    while 1 do
--      atom d = ( y / power(x,n-1) - x ) / n
        atom d = ( y / pow_(x,n-1) - x ) / n
        x += d
        atom e = eps*x   -- absolute accuracy
        if d > -e and d < e then exit end if
    end while
    return {y,n,x,power(y,1/n)}
end function

?nth_root(1024,10)
?nth_root(27,3)
?nth_root(2,2)
?nth_root(5642,125)
--?nth_root(7,0.5)  -- needs power(), not pow_()
?nth_root(4913,3)
?nth_root(8,3)
?nth_root(16,2)
?nth_root(16,4)
?nth_root(125,3)
?nth_root(1000000000,3)
?nth_root(1000000000,9)
```

{{out}}
Shows inputs and both the iterative and builtin results.

```txt

{1024,10,2,2}
{27,3,3,3}
{2,2,1.414213562,1.414213562}
{5642,125,1.071547592,1.071547592}
{4913,3,17,17.0}
{8,3,2,2}
{16,2,4,4}
{16,4,2,2}
{125,3,5,5}
{1000000000,3,1000,1000.0}
{1000000000,9,10,10.0}

```



## PHP


```PHP
function nthroot($number, $root, $p = P)
{
    $x[0] = $number;
    $x[1] = $number/$root;
    while(abs($x[1]-$x[0]) > $p)
    {
        $x[0] = $x[1];
        $x[1] = (($root-1)*$x[1] + $number/pow($x[1], $root-1))/$root;
    }
    return $x[1];
}
```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de nthRoot (N A)
   (let (X1 A  X2 (*/ A N))
      (until (= X1 X2)
         (setq
            X1 X2
            X2 (*/
               (+
                  (* X1 (dec N))
                  (*/ A 1.0 (pow X1 (* (dec N) 1.0))) )
               N ) ) )
      X2 ) )

(prinl (format (nthRoot 2  2.0) *Scl))
(prinl (format (nthRoot 3 12.3) *Scl))
(prinl (format (nthRoot 4 45.6) *Scl))
```

Output:

```txt
1.414214
2.308350
2.598611
```



## PL/I


```PL/I
/* Finds the N-th root of the number A */
root: procedure (A, N) returns (float);
   declare A float, N fixed binary;
   declare (xi, xip1) float;

   xi = 1; /* An initial guess */
   do forever;
      xip1 = ((n-1)*xi + A/xi**(n-1) ) / n;
      if abs(xip1-xi) < 1e-5 then leave;
      xi = xip1;
   end;
   return (xi);
end root;
```

Results:

```txt

The 2-th root of 4.00000E+0000 is  2.00000E+0000
The 5-th root of 3.20000E+0001 is  2.00000E+0000
The 3-th root of 2.70000E+0001 is  3.00000E+0000
The 2-th root of 2.00000E+0000 is  1.41422E+0000
The 3-th root of 1.00000E+0002 is  4.64159E+0000

```



## PowerShell

This sample implementation does not use <code>[System.Math]</code> classes.

```powershell
#NoTeS: This sample code does not validate inputs
#	Thus, if there are errors the 'scary' red-text
#	error messages will appear.
#
#	This code will not work properly in floating point values of n,
#	and negative values of A.
#
#	Supports negative values of n by reciprocating the root.

$epsilon=1E-10		#Sample Epsilon (Precision)

function power($x,$e){	#As I said in the comment
	$ret=1
	for($i=1;$i -le $e;$i++){
		$ret*=$x
	}
	return $ret
}
function root($y,$n){					#The main Function
	if (0+$n -lt 0){$tmp=-$n} else {$tmp=$n}	#This checks if n is negative.
	$ans=1

	do{
		$d = ($y/(power $ans ($tmp-1)) - $ans)/$tmp
		$ans+=$d
	} while ($d -lt -$epsilon -or $d -gt $epsilon)

	if (0+$n -lt 0){return 1/$ans} else {return $ans}
}

#Sample Inputs
root 625 2
root 2401 4
root 2 -2
root 1.23456789E-20 34
root 9.87654321E20 10	#Quite slow here, I admit...

((root 5 2)+1)/2	#Extra: Computes the golden ratio
((root 5 2)-1)/2
```

{{Out}}

```txt
PS> .\NTH.PS1
25
7
0.707106781186548
0.259690655650288
125.736248016373
1.61803398874989
0.618033988749895
PS>
```



## PureBasic


```PureBasic
#Def_p=0.001

Procedure.d Nth_root(n.i, A.d, p.d=#Def_p)
  Protected Dim x.d(1)
  x(0)=A: x(1)=A/n
  While Abs(x(1)-x(0))>p
    x(0)=x(1)
    x(1)=((n-1.0)*x(1)+A/Pow(x(1),n-1.0))/n
  Wend
  ProcedureReturn x(1)
EndProcedure

;//////////////////////////////
Debug "125'th root of 5642 is"
Debug Pow(5642,1/125)
Debug "First estimate is:"
Debug Nth_root(125,5642)
Debug "And better:"
Debug Nth_root(125,5642,0.00001)
```

'''Outputs
 125'th root of 5642 is
 1.0715475919447675
 First estimate is:
 1.0715596021916822
 And better:
 1.0715475919447714


## Python


```python
from decimal import Decimal, getcontext

def nthroot (n, A, precision):
    getcontext().prec = precision

    n = Decimal(n)
    x_0 = A / n #step 1: make a while guess.
    x_1 = 1     #need it to exist before step 2
    while True:
        #step 2:
        x_0, x_1 = x_1, (1 / n)*((n - 1)*x_0 + (A / (x_0 ** (n - 1))))
        if x_0 == x_1:
            return x_1
```



```python
print nthroot(5, 34, 10)
print nthroot(10,42, 20)
print nthroot(2, 5, 400)
```



## R


```R
nthroot <- function(A, n, tol=sqrt(.Machine$double.eps))
{
   ifelse(A < 1, x0 <- A * n, x0 <- A / n)
   repeat
   {
      x1 <- ((n-1)*x0 + A / x0^(n-1))/n
      if(abs(x1 - x0) > tol) x0 <- x1 else break
   }
   x1
}
nthroot(7131.5^10, 10)   # 7131.5
nthroot(7, 0.5)          # 49
```



## Racket


```Racket
#lang racket

(define (nth-root number root (tolerance 0.001))
  (define (acceptable? next current)
    (< (abs (- next current)) tolerance))

  (define (improve current)
    (/ (+ (* (- root 1) current) (/ number (expt current (- root 1)))) root))

  (define (loop current)
    (define next-guess (improve current))
    (if (acceptable? next-guess current)
        next-guess
        (loop next-guess)))
  (loop 1.0))
```



## REXX


```rexx
/*REXX program calculates the  Nth root  of  X,  with  DIGS  (decimal digits) accuracy. */
parse arg x root digs .                          /*obtain optional arguments from the CL*/
if    x=='' |    x==","   then    x= 2           /*Not specified?  Then use the default.*/
if root=='' | root==","   then root= 2           /* "       "        "   "   "      "   */
if digs=='' | digs==","   then digs=65           /* "       "        "   "   "      "   */
numeric digits digs                              /*set the  decimal digits  to   DIGS.  */
say '       x = '    x                           /*echo the value of   X.               */
say '    root = '    root                        /*  "   "    "    "   ROOT.            */
say '  digits = '    digs                        /*  "   "    "    "   DIGS.            */
say '  answer = '    root(x, root)               /*show the value of   ANSWER.          */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
root: procedure;  parse arg x 1 Ox, r 1 Or             /*arg1 ──► x & Ox, 2nd ──► r & Or*/
      if r==''  then r=2                               /*Was root specified?  Assume √. */
      if r=0    then return '[n/a]'                    /*oops-ay!  Can't do zeroth root.*/
      complex= x<0 & R//2==0                           /*will the result be complex?    */
      oDigs=digits()                                   /*get the current number of digs.*/
      if x=0 | r=1  then return x/1                    /*handle couple of special cases.*/
      dm=oDigs+5                                       /*we need a little guard room.   */
      r=abs(r);   x=abs(x)                             /*the absolute values of R and X.*/
      rm=r-1                                           /*just a fast version of  ROOT -1*/
      numeric form                                     /*take a good guess at the root─┐*/
      parse value format(x,2,1,,0) 'E0' with ? 'E' _ . /* ◄────────────────────────────┘*/
      g= (? / r'E'_ % r)  +  (x>1)                     /*kinda uses a crude "logarithm".*/
      d=5                                              /*start with five decimal digits.*/
           do until d==dm;   d=min(d+d,dm)             /*each time,  precision doubles. */
           numeric digits d                            /*tell REXX to use   D   digits. */
           old=-1                                      /*assume some kind of old guess. */
                  do until old=g;   old=g              /*where da rubber meets da road─┐*/
                  g=format((rm*g**r+x)/r/g**rm,, d-2)  /* ◄────── the root computation─┘*/
                  end   /*until old=g*/                /*maybe until the cows come home.*/
           end          /*until d==dm*/                /*and wait for more cows to come.*/

      if g=0        then return 0                      /*in case the jillionth root = 0.*/
      if Or<0       then g=1/g                         /*root < 0 ?   Reciprocal it is! */
      if \complex   then g=g*sign(Ox)                  /*adjust the sign  (maybe).      */
      numeric digits oDigs                             /*reinstate the original digits. */
      return (g/1)  ||  left('j', complex)             /*normalize # to digs, append j ?*/
```

'''output'''   when using the default inputs:

```txt

       x =  2
    root =  2
  digits =  65
  answer =  1.414213562373095048801688724209698078569671875376948073176679738

```

'''output'''   when using for input:   <tt> 10   3 </tt>

```txt

       x =  10
    root =  3
  digits =  65
  answer =  2.1544346900318837217592935665193504952593449421921085824892355063

```

'''output'''   when using for input:   <tt> 625   -4 </tt>

```txt

       x =  625
    root =  -4
  digits =  65
  answer =  0.2

```

'''output'''   when using for input:   <tt> 100.666   47 </tt>

```txt

       x =  100.666
    root =  47
  digits =  65
  answer =  1.1030990940616109102886569991014966919115206420386192403152621652

```

'''output'''   when using for input:   <tt> -256   8 </tt>

```txt

       x =  -256
    root =  8
  digits =  65
  answer =  2j

```

'''output'''   when using for input:   <tt> 12345678900098765432100.00987654321000123456789e333   19 </tt>

```txt

       x =  12345678900098765432100.00987654321000123456789e333
    root =  19
  digits =  65
  answer =  4886828567991886455.3257854108687610458584138783288904955196401434

```



## Ring


```ring

decimals(12)
see "cube root of 5 is : " + root(3, 5, 0) + nl

func root n, a, d
y = 0 x = a / n
while fabs (x - y) > d
      y = ((n - 1)*x + a/pow(x,(n-1))) / n
      temp = x
      x = y
      y = temp
end
return x

```

Output:

```txt

cube root of 5 is : 1.709975946677

```



## Ruby


```ruby
def nthroot(n, a, precision = 1e-5)
  x = Float(a)
  begin
    prev = x
    x = ((n - 1) * prev + a / (prev ** (n - 1))) / n
  end while (prev - x).abs > precision
  x
end

p nthroot(5,34)  # => 2.02439745849989
```



## Run BASIC


```runbasic
print "Root 125th Root of 5643 Precision .001   ";using( "#.###############",  NthRoot( 125, 5642, 0.001  ))
print "125th Root of 5643 Precision .001   ";using( "#.###############",  NthRoot( 125, 5642, 0.001  ))
print "125th Root of 5643 Precision .00001 ";using( "#.###############",  NthRoot( 125, 5642, 0.00001))
print "  3rd Root of   27 Precision .00001 ";using( "#.###############",  NthRoot(   3,   27, 0.00001))
print "  2nd Root of    2 Precision .00001 ";using( "#.###############",  NthRoot(   2,    2, 0.00001))
print " 10th Root of 1024 Precision .00001 ";using( "#.###############",  NthRoot(  10, 1024, 0.00001))

wait

function NthRoot( root, A, precision)
  x0 = A
  x1 = A /root
  while abs( x1 -x0) >precision
    x0 = x1
    x1 = x1 / 1.0                                ' force float
    x1 = (( root -1.0) *x1 +A /x1^( root -1.0)) /root
  wend
  NthRoot =x1
end function

end
```


```txt
125th Root of 5643 Precision .001   1.071559602456735
125th Root of 5643 Precision .00001 1.071547591944771
  3rd Root of   27 Precision .00001 3.000000000000001
  2nd Root of    2 Precision .00001 1.414213562374690
 10th Root of 1024 Precision .00001 2.000000000000000
```



## Sather

{{trans|Octave}}

```sather
class MATH is
  nthroot(n:INT, a:FLT):FLT
    pre n > 0
  is
    x0 ::= a / n.flt;
    m  ::= n - 1;
    loop
      x1 ::= (m.flt * x0 + a/(x0^(m.flt))) / n.flt;
      if (x1 - x0).abs < (x0 * 1.0e-9).abs then
        return x1;
      end;
      x0 := x1;
    end;
  end;

end;
```



```sather
class MAIN is
  main is
    a:FLT := 2.5 ^ 10.0;
    #OUT + MATH::nthroot(10, a) + "\n";
  end;
end;
```



## Scala

Using tail recursion:

```Scala
def nroot(n: Int, a: Double): Double = {
  @tailrec
  def rec(x0: Double) : Double = {
    val x1 = ((n - 1) * x0 + a/math.pow(x0, n-1))/n
    if (x0 <= x1) x0 else rec(x1)
  }

  rec(a)
}
```


Alternatively, you can implement the iteration with an iterator like so:

```scala
def fallPrefix(itr: Iterator[Double]): Iterator[Double] = itr.sliding(2).dropWhile(p => p(0) > p(1)).map(_.head)
def nrootLazy(n: Int)(a: Double): Double = fallPrefix(Iterator.iterate(a){r => (((n - 1)*r) + (a/math.pow(r, n - 1)))/n}).next
```



## Scheme


```scheme
(define (root number degree tolerance)
  (define (good-enough? next guess)
    (< (abs (- next guess)) tolerance))
  (define (improve guess)
    (/ (+ (* (- degree 1) guess) (/ number (expt guess (- degree 1)))) degree))
  (define (*root guess)
    (let ((next (improve guess)))
      (if (good-enough? next guess)
          guess
          (*root next))))
  (*root 1.0))

(display (root (expt 2 10) 10 0.1))
(newline)
(display (root (expt 2 10) 10 0.01))
(newline)
(display (root (expt 2 10) 10 0.001))
(newline)
```

Output:
 2.04732932236839
 2.00463204835482
 2.00004786858167


## Seed7


The nth root of the number 'a' can be computed with the exponentiation operator: 'a ** (1 / n)'.
An alternate function which uses Newton's method is:


```seed7
const func float: nthRoot (in integer: n, in float: a) is func
  result
    var float: x1 is 0.0;
  local
    var float: x0 is 0.0;
  begin
    x0 := a;
    x1 := a / flt(n);
    while abs(x1 - x0) >= abs(x0 * 1.0E-9) do
      x0 := x1;
      x1 := (flt(pred(n)) * x0 + a / x0 ** pred(n)) / flt(n);
    end while;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#nthRoot]


## Sidef

{{trans|Ruby}}

```ruby
func nthroot(n, a, precision=1e-5) {
  var x    = 1.float
  var prev = 0.float
  while ((prev-x).abs > precision) {
    prev = x;
    x = (((n-1)*prev + a/(prev**(n-1))) / n)
  }
  return x
}

say nthroot(5, 34)  # => 2.024397458501034082599817835297912829678314204
```


A minor optimization would be to calculate the successive ''int(n-1)'' square roots of a number, then raise the result to the power of ''2**(int(n-1) / n)''.


```ruby
func nthroot_fast(n, a, precision=1e-5) {
  { a = nthroot(2, a, precision) } * int(n-1)
  a ** (2**int(n-1) / n)
}

say nthroot_fast(5, 34, 1e-64)  # => 2.02439745849988504251081724554193741911462170107
```



## Smalltalk

{{works with|GNU Smalltalk}}

{{trans|Tcl}}

```smalltalk
Number extend [
    nthRoot: n [
	|x0 m x1|
	x0 := (self / n) asFloatD.
	m := n - 1.
	[true] whileTrue: [
	    x1 := ( (m * x0) + (self/(x0 raisedTo: m))) / n.
	    ((x1 - x0) abs) < ((x0 * 1e-9) abs)
		ifTrue: [ ^ x1 ].
	    x0 := x1
	]
    ]
].
```



```smalltalk
(34 nthRoot: 5) displayNl.
((7131.5 raisedTo: 10) nthRoot: 10) displayNl.
(7 nthRoot: 0.5) displayNl.
```



## SPL


```spl
nthr(n,r) <= n^(1/r)

nthroot(n,r)=
  a = n/r
  g = n
  > g!=a
    g = a
    a = (1/r)*(((r-1)*g)+(n/(g^(r-1))))
  <
  <= a
.

#.output(nthr(2,2))
#.output(nthroot(2,2))
```

{{out}}

```txt

1.4142135623731
1.41421356237309

```



## Tcl

The easiest way is to just use the <code>pow</code> function (or exponentiation operator) like this:

```tcl
proc nthroot {n A} {
    expr {pow($A, 1.0/$n)}
}
```

However that's hardly tackling the problem itself. So here's how to do it using Newton-Raphson and a self-tuning termination test.

{{works with|Tcl|8.5}}

```tcl
proc nthroot {n A} {
    set x0 [expr {$A / double($n)}]
    set m [expr {$n - 1.0}]
    while 1 {
        set x1 [expr {($m*$x0 + $A/$x0**$m) / $n}]
        if {abs($x1 - $x0) < abs($x0 * 1e-9)} {
            return $x1
        }
        set x0 $x1
    }
}
```

Demo:

```tcl
puts [nthroot 2 2]
puts [nthroot 5 34]
puts [nthroot 5 [expr {34**5}]]
puts [nthroot 10 [expr 7131.5**10]]
puts [nthroot 0.5 7]; # Squaring!
```

Output:

```txt
1.414213562373095
2.0243974584998847
34.0
7131.5
49.0
```



## Ursala

The nthroot function defined below takes a natural number n to the
function that returns the n-th root of its floating point argument.
Error is on the order of machine precision because the stopping
criterion is either a fixed point or a repeating cycle.

```Ursala
#import nat
#import flo

nthroot =

-+
   ("n","n-1"). "A". ("x". div\"n" plus/times("n-1","x") div("A",pow("x","n-1")))^== 1.,
   float^~/~& predecessor+-
```

This implementation is unnecessary in practice due to the availability of the library
function pow, which performs exponentiation and allows fractional exponents.
Here is a test program.

```Ursala
#cast %eL

examples =

<
   nthroot2 2.,
   nthroot5 34.,
   nthroot5 pow(34.,5.),
   nthroot10 pow(7131.5,10.)>
```

output:

```txt

<
   1.414214e+00,
   2.024397e+00,
   3.400000e+01,
   7.131500e+03>

```



## VBA

{{trans|Phix}}
The internal power operator "^" is used in stead of an auxiliary pow_ function and the accuracy has been reduced.

```vb
Private Function nth_root(y As Double, n As Double)
    Dim eps As Double: eps = 0.00000000000001 '-- relative accuracy
    Dim x As Variant: x = 1
    Do While True
        d = (y / x ^ (n - 1) - x) / n
        x = x + d
        e = eps * x '-- absolute accuracy
        If d > -e And d < e Then
            Exit Do
        End If
    Loop
    Debug.Print y; n; x; y ^ (1 / n)
End Function
Public Sub main()
    nth_root 1024, 10
    nth_root 27, 3
    nth_root 2, 2
    nth_root 5642, 125
    nth_root 7, 0.5
    nth_root 4913, 3
    nth_root 8, 3
    nth_root 16, 2
    nth_root 16, 4
    nth_root 125, 3
    nth_root 1000000000, 3
    nth_root 1000000000, 9
End Sub
```
{{out}}

```txt
 1024  10  2  2
 27  3  3  3
 2  2  1,41421356237309  1,4142135623731
 5642  125  1,07154759194477  1,07154759194477
 7  0,5  49  49
 4913  3  17  17
 8  3  2  2
 16  2  4  4
 16  4  2  2
 125  3  5  5
 1000000000  3  1000  1000
 1000000000  9  10  10

```


## XPL0


```XPL0
include c:\cxpl\stdlib;

func real NRoot(A, N);  \Return the Nth root of A
real A, N;
real X, X0, Y;
int  I;
[X:= 1.0;               \initial guess
repeat  X0:= X;
        Y:= 1.0;
        for I:= 1 to fix(N)-1 do Y:= Y*X0;
        X:= ((N-1.0)*X0 + A/Y) / N;
until   abs(X-X0) < 1.0E-15;            \(until X=X0 doesn't always work)
return X;
];

[Format(5, 15);
RlOut(0, NRoot( 2., 2.));  CrLf(0);
RlOut(0, Power( 2., 0.5)); CrLf(0);     \for comparison
RlOut(0, NRoot(27., 3.));  CrLf(0);
RlOut(0, NRoot(1024.,10.)); CrLf(0);
]
```


Output:

```txt

    1.414213562373100
    1.414213562373100
    3.000000000000000
    2.000000000000000

```



## Yabasic

{{trans|AWK}}

```Yabasic
data 10, 1024, 3, 27, 2, 2, 125, 5642, 4, 16, 0, 0

do
    read e, b
    if e = 0 break
    print "The ", e, "th root of ", b, " is ", b^(1/e), " (", nthroot(b, e), ")"
loop


sub nthroot(y, n)
    local eps, x, d, e

    eps = 1e-15     // relative accuracy
    x   = 1
    repeat
        d  = ( y / ( x^(n-1) ) - x ) / n
        x = x + d
        e = eps * x // absolute accuracy

    until(not(d < -e or d > e ))

    return x
end sub
```



## zkl

{{trans|Ruby}}

```zkl
fcn nthroot(nth,a,precision=1.0e-5){
   x:=prev:=a=a.toFloat(); n1:=nth-1;
   do{
      prev=x;
      x=( prev*n1 + a/prev.pow(n1) ) / nth;
   }
   while( not prev.closeTo(x,precision) );
   x
}

nthroot(5,34) : "%.20f".fmt(_).println()  # => 2.02439745849988828041
```



{{omit from|M4}}
