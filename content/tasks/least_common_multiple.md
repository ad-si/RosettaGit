+++
title = "Least common multiple"
description = ""
date = 2019-04-23T22:43:51Z
aliases = []
[extra]
id = 9406
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "8th",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "applesoft_basic",
  "arendelle",
  "assembly",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "brat",
  "c",
  "c_shell",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dwscript",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "excel",
  "ezhil",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "gap",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lasso",
  "liberty_basic",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "microsoft_small_basic",
  "ml",
  "mlite",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "order",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "qi",
  "r",
  "racket",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "sparkling",
  "swift",
  "tcl",
  "tse_sal",
  "txr",
  "ubasic_4th",
  "unix_shell",
  "ursa",
  "vala",
  "vba",
  "vbscript",
  "wortel",
  "x86_assembly",
  "xbasic",
  "xpl0",
  "yabasic",
  "zkl",
  "இந_த_ந_ரல_இர_எண_கள_க_க_இட_ய_ல_ன_ம_ச_ச_ற_ப_த_மடங_க_lcm_ம_ப_ப_ர_ப_த_வக_த_த_gcd_என_ன_என_ற_கணக_க_ட_ம",
]
+++

## Task

Compute the least common multiple of two integers.

Given   ''m''   and   ''n'',   the least common multiple is the smallest positive integer that has both   ''m''   and   ''n''   as factors.


;Example:
The least common multiple of 12 and 18 is 36, because 12 is a factor (12 &times; 3 = 36), and 18 is a factor (18 &times; 2 = 36), and there is no positive integer less than 36 that has both factors.   As a special case, if either   ''m''   or   ''n''   is zero, then the least common multiple is zero.

One way to calculate the least common multiple is to iterate all the multiples of   ''m'',   until you find one that is also a multiple of   ''n''.

If you already have   ''gcd''   for [[greatest common divisor]],   then this formula calculates   ''lcm''.

<big>
:::: <math>\operatorname{lcm}(m, n) = \frac{|m \times n|}{\operatorname{gcd}(m, n)}</math>
</big>

One can also find   ''lcm''   by merging the [[prime decomposition]]s of both   ''m''   and   ''n''.


## See also

*   MathWorld entry:   [http://mathworld.wolfram.com/LeastCommonMultiple.html Least Common Multiple].
*   Wikipedia entry:   [[wp:Least common multiple|Least common multiple]].





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
with 2 ASSIST macros (XDECO,XPRNT).

```360asm
LCM      CSECT
         USING  LCM,R15            use calling register
         L      R6,A               a
         L      R7,B               b
         LR     R8,R6              c=a
LOOPW    LR     R4,R8                c
         SRDA   R4,32                shift to next reg
         DR     R4,R7                c/b
         LTR    R4,R4              while c mod b<>0
         BZ     ELOOPW               leave while
         AR     R8,R6                c+=a
         B      LOOPW              end while
ELOOPW   LPR    R9,R6              c=abs(u)
         L      R1,A               a
         XDECO  R1,XDEC            edit a
         MVC    PG+4(5),XDEC+7     move a to buffer
         L      R1,B               b
         XDECO  R1,XDEC            edit b
         MVC    PG+10(5),XDEC+7    move b to buffer
         XDECO  R8,XDEC            edit c
         MVC    PG+17(10),XDEC+2   move c to buffer
         XPRNT  PG,80              print buffer
         XR     R15,R15            return code =0
         BR     R14                return to caller
A        DC     F'1764'            a
B        DC     F'3920'            b
PG       DC     CL80'lcm(00000,00000)=0000000000'  buffer
XDEC     DS     CL12               temp for edit
         YREGS
         END    LCM
```

```txt

lcm( 1764, 3920)=     35280

```




## 8th


```forth

: gcd \ a b -- gcd
	dup 0 n:= if drop ;; then
	tuck \ b a b
	n:mod \ b a-mod-b
	recurse ;

: lcm \ m n
	2dup \ m n m n
	n:* \ m n m*n
	n:abs \ m n abs(m*n)
	-rot \ abs(m*n) m n
	gcd \ abs(m*n) gcd(m.n)
	n:/mod \ abs / gcd
	nip \ abs div gcd
;

: demo \ n m --
	2dup "LCM of " . . " and " . . " = " . lcm . ;

12 18 demo cr
-6 14 demo cr
35  0 demo cr


bye
```

```txt
LCM of 18 and 12 = 36
LCM of 14 and -6 = 42
LCM of 0 and 35 = 0

```



## Ada

lcm_test.adb:

```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Lcm_Test is
   function Gcd (A, B : Integer) return Integer is
      M : Integer := A;
      N : Integer := B;
      T : Integer;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop;
      return M;
   end Gcd;

   function Lcm (A, B : Integer) return Integer is
   begin
      if A = 0 or B = 0 then
         return 0;
      end if;
      return abs (A) * (abs (B) / Gcd (A, B));
   end Lcm;
begin
   Put_Line ("LCM of 12, 18 is" & Integer'Image (Lcm (12, 18)));
   Put_Line ("LCM of -6, 14 is" & Integer'Image (Lcm (-6, 14)));
   Put_Line ("LCM of 35, 0 is" & Integer'Image (Lcm (35, 0)));
end Lcm_Test;
```


Output:

```txt
LCM of 12, 18 is 36
LCM of -6, 14 is 42
LCM of 35, 0 is 0
```



## ALGOL 68


```algol68

BEGIN
   PROC gcd = (INT m, n) INT :
   BEGIN
      INT a := ABS m, b := ABS n;
      IF a=0 OR b=0 THEN 0 ELSE
	 WHILE b /= 0 DO INT t = b; b := a MOD b; a := t OD;
	 a
      FI
   END;
   PROC lcm = (INT m, n) INT : ( m*n = 0 | 0 | ABS (m*n) % gcd (m, n));
   INT m=12, n=18;
   printf (($gxg(0)3(xgxg(0))l$,
	    "The least common multiple of", m, "and", n, "is", lcm(m,n),
	    "and their greatest common divisor is", gcd(m,n)))
END

```

```txt

The least common multiple of 12 and 18 is 36 and their greatest common divisor is 6


```


Note that either or both PROCs could just as easily be implemented as OPs but then the operator priorities would also have to be declared.


## ALGOL W


```algolw
begin
    integer procedure gcd ( integer value a, b ) ;
        if b = 0 then a else gcd( b, a rem abs(b) );

    integer procedure lcm( integer value a, b ) ;
        abs( a * b ) div gcd( a, b );

    write( lcm( 15, 20  ) );
end.
```



## APL

APL provides this function.

```apl
      12^18
36
```

If for any reason we wanted to reimplement it, we could do so in terms of the greatest common divisor by transcribing the formula set out in the task specification into APL notation:

```apl
      LCM←{(|⍺×⍵)÷⍺∨⍵}
      12 LCM 18
36
```



## AppleScript



```AppleScript
-- LEAST COMMON MULTIPLE -----------------------------------------------------

-- lcm :: Integral a => a -> a -> a
on lcm(x, y)
    if x = 0 or y = 0 then
        0
    else
        abs(x div (gcd(x, y)) * y)
    end if
end lcm


-- TEST ----------------------------------------------------------------------
on run

    lcm(12, 18)

    --> 36
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- abs :: Num a => a -> a
on abs(x)
    if x < 0 then
        -x
    else
        x
    end if
end abs

-- gcd :: Integral a => a -> a -> a
on gcd(x, y)
    script
        on |λ|(a, b)
            if b = 0 then
                a
            else
                |λ|(b, a mod b)
            end if
        end |λ|
    end script

    result's |λ|(abs(x), abs(y))
end gcd
```

```AppleScript
36>
```



## Arendelle


For GCD function check out [http://rosettacode.org/wiki/Greatest_common_divisor#Arendelle here]


```txt
&lt; a , b &gt;

( return ,

        abs ( @a * @b ) /
        !gcd( @a , @b )

)
```




## Assembly

=
## x86 Assembly
=

```asm

; lcm.asm: calculates the least common multiple
; of two positive integers
;
; nasm x86_64 assembly (linux) with libc
; assemble: nasm -felf64 lcm.asm; gcc lcm.o
; usage: ./a.out [number1] [number2]

    global main
    extern printf ; c function: prints formatted output
    extern strtol ; c function: converts strings to longs

    section .text

main:
    push rbp    ; set up stack frame

    ; rdi contains argc
    ; if less than 3, exit
    cmp rdi, 3
    jl incorrect_usage

    ; push first argument as number
    push rsi
    mov rdi, [rsi+8]
    mov rsi, 0
    mov rdx, 10 ; base 10
    call strtol
    pop rsi
    push rax

    ; push second argument as number
    push rsi
    mov rdi, [rsi+16]
    mov rsi, 0
    mov rdx, 10 ; base 10
    call strtol
    pop rsi
    push rax

    ; pop arguments and call get_gcd
    pop rdi
    pop rsi
    call get_gcd

    ; print value
    mov rdi, print_number
    mov rsi, rax
    call printf

    ; exit
    mov rax, 0  ; 0--exit success
    pop rbp
    ret

incorrect_usage:
    mov rdi, bad_use_string
    ; rsi already contains argv
    mov rsi, [rsi]
    call printf
    mov rax, 0  ; 0--exit success
    pop rbp
    ret

bad_use_string:
    db "Usage: %s [number1] [number2]",10,0

print_number:
    db "%d",10,0

get_gcd:
    push rbp    ; set up stack frame
    mov rax, 0
    jmp loop

loop:
    ; keep adding the first argument
    ; to itself until a multiple
    ; is found. then, return
    add rax, rdi
    push rax
    mov rdx, 0
    div rsi
    cmp rdx, 0
    pop rax
    je gcd_found
    jmp loop

gcd_found:
    pop rbp
    ret


```




## AutoHotkey


```autohotkey
LCM(Number1,Number2)
{
 If (Number1 = 0 || Number2 = 0)
  Return
 Var := Number1 * Number2
 While, Number2
  Num := Number2, Number2 := Mod(Number1,Number2), Number1 := Num
 Return, Var // Number1
}

Num1 = 12
Num2 = 18
MsgBox % LCM(Num1,Num2)
```



## AutoIt


```AutoIt

Func _LCM($a, $b)
	Local $c, $f, $m = $a, $n = $b
	$c = 1
	While $c <> 0
		$f = Int($a / $b)
		$c = $a - $b * $f
		If $c <> 0 Then
			$a = $b
			$b = $c
		EndIf
	WEnd
	Return $m * $n / $b
EndFunc   ;==>_LCM

```

Example

```AutoIt

ConsoleWrite(_LCM(12,18) & @LF)
ConsoleWrite(_LCM(-5,12) & @LF)
ConsoleWrite(_LCM(13,0)  & @LF)

```


```txt

36
60
0

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 14:32, 15 November 2013 (UTC)

## AWK


```awk
# greatest common divisor
function gcd(m, n,    t) {
	# Euclid's method
	while (n != 0) {
		t = m
		m = n
		n = t % n
	}
	return m
}

# least common multiple
function lcm(m, n,    r) {
	if (m == 0 || n == 0)
		return 0
	r = m * n / gcd(m, n)
	return r < 0 ? -r : r
}

# Read two integers from each line of input.
# Print their least common multiple.
{ print lcm($1, $2) }
```


Example input and output:
```txt
$ awk -f lcd.awk
12 18
36
-6 14
42
35 0
0

```



## BASIC


=
## Applesoft BASIC
=
ported from BBC BASIC

```ApplesoftBasic
10 DEF FN MOD(A) = INT((A / B - INT(A / B)) * B + .05) * SGN(A / B)
20 INPUT"M=";M%
30 INPUT"N=";N%
40 GOSUB 100
50 PRINT R
60 END

100 REM LEAST COMMON MULTIPLE M% N%
110 R = 0
120 IF M% = 0 OR N% = 0 THEN RETURN
130 A% = M% : B% = N% : GOSUB 200"GCD
140 R = ABS(M%*N%)/R
150 RETURN

200 REM GCD ITERATIVE EUCLID A% B%
210 FOR B = B% TO 0 STEP 0
220     C% = A%
230     A% = B
240     B = FN MOD(C%)
250 NEXT B
260 R = ABS(A%)
270 RETURN
```


=
## BBC BASIC
=
```BBC BASIC

      DEF FN_LCM(M%,N%)
      IF M%=0 OR N%=0 THEN =0 ELSE =ABS(M%*N%)/FN_GCD_Iterative_Euclid(M%, N%)

      DEF FN_GCD_Iterative_Euclid(A%, B%)
      LOCAL C%
      WHILE B%
        C% = A%
        A% = B%
        B% = C% MOD B%
      ENDWHILE
      = ABS(A%)

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DEF LCM(A,B)=(A*B)/GCD(A,B)
110 DEF GCD(A,B)
120   DO WHILE B>0
130     LET T=B:LET B=MOD(A,B):LET A=T
140   LOOP
150   LET GCD=A
160 END DEF
170 PRINT LCM(12,18)
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
set num1=12
set num2=18

call :lcm %num1% %num2%
exit /b

:lcm <input1> <input2>
if %2 equ 0 (
	set /a lcm = %num1%*%num2%/%1
	echo LCM = !lcm!
	pause>nul
	goto :EOF
)
set /a res = %1 %% %2
call :lcm %2 %res%
goto :EOF
```

```txt
LCM = 36
```



## bc

```bc
/* greatest common divisor */
define g(m, n) {
	auto t

	/* Euclid's method */
	while (n != 0) {
		t = m
		m = n
		n = t % n
	}
	return (m)
}

/* least common multiple */
define l(m, n) {
	auto r

	if (m == 0 || n == 0) return (0)
	r = m * n / g(m, n)
	if (r < 0) return (-r)
	return (r)
}
```



## Befunge


Inputs are limited to signed 16-bit integers.


```befunge
&>:0`2*1-*:&>:#@!#._:0`2*1v
>28*:*:**+:28*>:*:*/\:vv*-<
|<:%/*:*:*82\%*:*:*82<<>28v
>$/28*:*:*/*.@^82::+**:*:*<
```


```txt
12345
-23044
```


```txt
345660
```



## Bracmat

We utilize the fact that Bracmat simplifies fractions (using Euclid's algorithm). The function <code>den$<i>number</i></code> returns the denominator of a number.

```bracmat
(gcd=
  a b
.   !arg:(?a.?b)
  &   den$(!a*!b^-1)
    * (!a:<0&-1|1)
    * !a
);
out$(gcd$(12.18) gcd$(-6.14) gcd$(35.0) gcd$(117.18))
```

Output:

```txt
36 42 35 234
```



## Brat


```brat

gcd = { a, b |
  true? { a == 0 }
    { b }
    { gcd(b % a, a) }
}

lcm = { a, b |
  a * b / gcd(a, b)
}

p lcm(12, 18) # 36
p lcm(14, 21) # 42

```



## C


```c
#include <stdio.h>

int gcd(int m, int n)
{
        int tmp;
        while(m) { tmp = m; m = n % m; n = tmp; }
        return n;
}

int lcm(int m, int n)
{
        return m / gcd(m, n) * n;
}

int main()
{
        printf("lcm(35, 21) = %d\n", lcm(21,35));
        return 0;
}
```



## C++

```cpp
#include <boost/math/common_factor.hpp>
#include <iostream>

int main( ) {
   std::cout << "The least common multiple of 12 and 18 is " <<
      boost::math::lcm( 12 , 18 ) << " ,\n"
      << "and the greatest common divisor " << boost::math::gcd( 12 , 18 ) << " !" << std::endl ;
   return 0 ;
}
```


```txt
The least common multiple of 12 and 18 is 36 ,
and the greatest common divisor 6 !

```



###  Alternate solution

```cpp

#include <cstdlib>
#include <iostream>
#include <tuple>

int gcd(int a, int b) {
    a = abs(a);
    b = abs(b);
    while (b != 0) {
        std::tie(a, b) = std::make_tuple(b, a % b);
    }
    return a;
}

int lcm(int a, int b) {
    int c = gcd(a, b);
    return c == 0 ? 0 : a / c * b;
}

int main() {
    std::cout << "The least common multiple of 12 and 18 is " << lcm(12, 18) << ",\n"
        << "and their greatest common divisor is " << gcd(12, 18) << "!"
        << std::endl;
    return 0;
}

```


## C#

```c#
Using System;
class Program
{
    static int gcd(int m, int n)
    {
        return n == 0 ? Math.Abs(m) : gcd(n, n % m);
    }
    static int lcm(int m, int n)
    {
        return Math.Abs(m * n) / gcd(m, n);
    }
    static void Main()
    {
        Console.WriteLine("lcm(12,18)=" + lcm(12,18));
    }
}

```

```txt
lcm(12,18)=36
```



## Clojure


```Clojure
(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm
      [a b]
      (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. show-lcm.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION lcm
           .
       PROCEDURE DIVISION.
           DISPLAY "lcm(35, 21) = " FUNCTION lcm(35, 21)
           GOBACK
           .
       END PROGRAM show-lcm.

       IDENTIFICATION DIVISION.
       FUNCTION-ID. lcm.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION gcd
           .
       DATA DIVISION.
       LINKAGE SECTION.
       01  m                       PIC S9(8).
       01  n                       PIC S9(8).
       01  ret                     PIC S9(8).

       PROCEDURE DIVISION USING VALUE m, n RETURNING ret.
           COMPUTE ret = FUNCTION ABS(m * n) / FUNCTION gcd(m, n)
           GOBACK
           .
       END FUNCTION lcm.

       IDENTIFICATION DIVISION.
       FUNCTION-ID. gcd.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  temp                    PIC S9(8).

       01  x                       PIC S9(8).
       01  y                       PIC S9(8).

       LINKAGE SECTION.
       01  m                       PIC S9(8).
       01  n                       PIC S9(8).
       01  ret                     PIC S9(8).

       PROCEDURE DIVISION USING VALUE m, n RETURNING ret.
           MOVE m to x
           MOVE n to y

           PERFORM UNTIL y = 0
               MOVE x TO temp
               MOVE y TO x
               MOVE FUNCTION MOD(temp, y) TO Y
           END-PERFORM

           MOVE FUNCTION ABS(x) TO ret
           GOBACK
           .
       END FUNCTION gcd.
```



## Common Lisp

Common Lisp provides the <tt>lcm</tt> function. It can accept two or more (or less) parameters.


```lisp
CL-USER> (lcm 12 18)
36
CL-USER> (lcm 12 18 22)
396
```


Here is one way to reimplement it.


```lisp
CL-USER> (defun my-lcm (&rest args)
	   (reduce (lambda (m n)
		     (cond ((or (= m 0) (= n 0)) 0)
			   (t (abs (/ (* m n) (gcd m n))))))
		   args :initial-value 1))
MY-LCM
CL-USER> (my-lcm 12 18)
36
CL-USER> (my-lcm 12 18 22)
396
```


In this code, the <tt>lambda</tt> finds the least common multiple of two integers, and the <tt>reduce</tt> transforms it to accept any number of parameters. The <tt>reduce</tt> operation exploits how ''lcm'' is associative, <tt>(lcm a b c) == (lcm (lcm a b) c)</tt>; and how 1 is an identity, <tt>(lcm 1 a) == a</tt>.


## D


```d
import std.stdio, std.bigint, std.math;

T gcd(T)(T a, T b) pure nothrow {
    while (b) {
        immutable t = b;
        b = a % b;
        a = t;
    }
    return a;
}

T lcm(T)(T m, T n) pure nothrow {
    if (m == 0) return m;
    if (n == 0) return n;
    return abs((m * n) / gcd(m, n));
}

void main() {
    lcm(12, 18).writeln;
    lcm("2562047788015215500854906332309589561".BigInt,
        "6795454494268282920431565661684282819".BigInt).writeln;
}
```

```txt
36
15669251240038298262232125175172002594731206081193527869
```



## DWScript


```delphi
PrintLn(Lcm(12, 18));
```

Output:

```txt
36
```


## Dart


```dart

main() {
	int x=8;
  int y=12;
int z= gcd(x,y);
  var lcm=(x*y)/z;
  print('$lcm');
  }

int gcd(int a,int b)
{
  if(b==0)
    return a;
  if(b!=0)
    return gcd(b,a%b);
}


```


## EchoLisp

(lcm a b) is already here as a two arguments function. Use foldl to find the lcm of a list of numbers.

```lisp

(lcm 0 9) → 0
(lcm 444 888)→ 888
(lcm 888 999) → 7992

(define (lcm* list) (foldl lcm (first list) list)) → lcm*
(lcm* '(444 888 999)) → 7992

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'math;

gcd = (m,n => (n == 0) ? (m.Absolute) : (gcd(n,n.mod:m)));

lcm = (m,n => (m * n).Absolute / gcd(m,n));

public program()
{
    console.printLine("lcm(12,18)=",lcm(12,18))
}
```

```txt

lcm(12,18)=36

```



## Elixir


```elixir
defmodule RC do
  def gcd(a,0), do: abs(a)
  def gcd(a,b), do: gcd(b, rem(a,b))

  def lcm(a,b), do: div(abs(a*b), gcd(a,b))
end

IO.puts RC.lcm(-12,15)
```


```txt

60

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(lcm).
-export([main/0]).

main() ->
	lcm(-3,4).

gcd(A, 0) ->
	A;

gcd(A, B) ->
	gcd(B, A rem B).

lcm(A,B) ->
	abs(A*B div gcd(A,B)).
```


```txt
12

```



## ERRE


```ERRE
PROGRAM LCM

PROCEDURE GCD(A,B->GCD)
    LOCAL C
    WHILE B DO
        C=A
        A=B
        B=C MOD B
    END WHILE
    GCD=ABS(A)
END PROCEDURE

PROCEDURE LCM(M,N->LCM)
    IF M=0 OR N=0 THEN
        LCM=0
        EXIT PROCEDURE
      ELSE
        GCD(M,N->GCD)
        LCM=ABS(M*N)/GCD
    END IF
END PROCEDURE

BEGIN
    LCM(18,12->LCM)
    PRINT("LCM of 18 AND 12 =";LCM)
    LCM(14,-6->LCM)
    PRINT("LCM of 14 AND -6 =";LCM)
    LCM(0,35->LCM)
    PRINT("LCM of 0 AND 35 =";LCM)
END PROGRAM
```


```txt
LCM of 18 and 12 = 36
LCM of 14 and -6 = 42
LCM of 0 and 35 = 0

```



## Euphoria


```euphoria
function gcd(integer m, integer n)
    integer tmp
    while m do
        tmp = m
        m = remainder(n,m)
        n = tmp
    end while
    return n
end function

function lcm(integer m, integer n)
    return m / gcd(m, n) * n
end function
```



## Excel

Excel's LCM can handle multiple values. Type in a cell:

```excel
=LCM(A1:J1)
```

This will get the LCM on the first 10 cells in the first row. Thus :

```txt
12	3	5	23	13	67	15	9	4	2

3605940
```



## Ezhil

<lang src="Ezhil">
## இந்த நிரல் இரு எண்களுக்கு இடையிலான மீச்சிறு பொது மடங்கு (LCM), மீப்பெரு பொது வகுத்தி (GCD) என்ன என்று கணக்கிடும்

நிரல்பாகம் மீபொம(எண்1, எண்2)

	@(எண்1 == எண்2) ஆனால்

  ## இரு எண்களும் சமம் என்பதால், மீபொம அந்த எண்ணேதான்

		பின்கொடு எண்1

	@(எண்1 > எண்2) இல்லைஆனால்

		சிறியது = எண்2
		பெரியது = எண்1

	இல்லை

		சிறியது = எண்1
		பெரியது = எண்2

	முடி

	மீதம் = பெரியது % சிறியது

	@(மீதம் == 0) ஆனால்

  ## பெரிய எண்ணில் சிறிய எண் மீதமின்றி வகுபடுவதால், பெரிய எண்தான் மீபொம

		பின்கொடு பெரியது

	இல்லை

		தொடக்கம் = பெரியது + 1
		நிறைவு = சிறியது * பெரியது

		@(எண் = தொடக்கம், எண் <= நிறைவு, எண் = எண் + 1) ஆக

    ## ஒவ்வோர் எண்ணாக எடுத்துக்கொண்டு தரப்பட்ட இரு எண்களாலும் வகுத்துப் பார்க்கின்றோம். முதலாவதாக இரண்டாலும் மீதமின்றி வகுபடும் எண்தான் மீபொம

			மீதம்1 = எண் % சிறியது
			மீதம்2 = எண் % பெரியது

			@((மீதம்1 == 0) && (மீதம்2 == 0)) ஆனால்
				பின்கொடு எண்
			முடி

		முடி

	முடி

முடி

அ = int(உள்ளீடு("ஓர் எண்ணைத் தாருங்கள் "))
ஆ = int(உள்ளீடு("இன்னோர் எண்ணைத் தாருங்கள் "))

பதிப்பி "நீங்கள் தந்த இரு எண்களின் மீபொம (மீச்சிறு பொது மடங்கு, LCM) = ", மீபொம(அ, ஆ)

```



=={{header|F_Sharp|F#}}==

```fsharp
let rec gcd x y = if y = 0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)
```



## Factor

The vocabulary ''math.functions'' already provides ''lcm''.


```factor
USING: math.functions prettyprint ;
26 28 lcm .
```


This program outputs ''364''.

One can also reimplement ''lcm''.


```factor
USING: kernel math prettyprint ;
IN: script

: gcd ( a b -- c )
    [ abs ] [
        [ nip ] [ mod ] 2bi gcd
    ] if-zero ;

: lcm ( a b -- c )
    [ * abs ] [ gcd ] 2bi / ;

26 28 lcm .
```



## Forth


```forth
: gcd ( a b -- n )
  begin dup while tuck mod repeat drop ;

: lcm ( a b -- n )
  over 0= over 0= or if 2drop 0 exit then
  2dup gcd abs */ ;
```



## Fortran

This solution is written as a combination of 2 functions, but a subroutine implementation would work great as well.

```Fortran

    integer function lcm(a,b)
    integer:: a,b
        lcm = a*b / gcd(a,b)
    end function lcm

    integer function gcd(a,b)
    integer :: a,b,t
        do while (b/=0)
            t = b
            b = mod(a,b)
            a = t
        end do
        gcd = abs(a)
    end function gcd

```


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function lcm (m As Integer, n As Integer) As Integer
  If m = 0 OrElse n = 0 Then Return 0
  If m < n Then Swap m, n '' to minimize iterations needed
  Var count = 0
  Do
    count +=1
  Loop Until (m * count) Mod n  = 0
  Return m * count
End Function

Print "lcm(12, 18) ="; lcm(12, 18)
Print "lcm(15, 12) ="; lcm(15, 12)
Print "lcm(10, 14) ="; lcm(10, 14)
Print
Print "Press any key to quit"
Sleep
```


```txt

lcm(12, 18) = 36
lcm(15, 12) = 60
lcm(10, 14) = 70

```



## Frink

Frink has a built-in LCM function that handles arbitrarily-large integers.

```frink

println[lcm[2562047788015215500854906332309589561, 6795454494268282920431565661684282819]]

```



## FunL

FunL has function <code>lcm</code> in module <code>integers</code> with the following definition:


```funl
def
  lcm( _, 0 ) =  0
  lcm( 0, _ ) =  0
  lcm( x, y ) =  abs( (x\gcd(x, y)) y )
```



## GAP


```gap
# Built-in
LcmInt(12, 18);
# 36
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

var m, n, z big.Int

func init() {
    m.SetString("2562047788015215500854906332309589561", 10)
    n.SetString("6795454494268282920431565661684282819", 10)
}

func main() {
    fmt.Println(z.Mul(z.Div(&m, z.GCD(nil, nil, &m, &n)), &n))
}
```

```txt

15669251240038298262232125175172002594731206081193527869

```



## Groovy


```groovy
def gcd
gcd = { m, n -> m = m.abs(); n = n.abs(); n == 0 ? m : m%n == 0 ? n : gcd(n, m % n) }

def lcd = { m, n -> Math.abs(m * n) / gcd(m, n) }

[[m: 12, n: 18, l: 36],
 [m: -6, n: 14, l: 42],
 [m: 35, n: 0, l: 0]].each { t ->
    println "LCD of $t.m, $t.n is $t.l"
    assert lcd(t.m, t.n) == t.l
}
```

```txt
LCD of 12, 18 is 36
LCD of -6, 14 is 42
LCD of 35, 0 is 0
```


=={{header|GW-BASIC}}==
```qbasic

10 PRINT "LCM(35, 21) = ";
20 LET MLCM = 35
30 LET NLCM = 21
40 GOSUB 200: ' Calculate LCM
50 PRINT LCM
60 END

195 ' Calculate LCM
200 LET MGCD = MLCM
210 LET NGCD = NLCM
220 GOSUB 400: ' Calculate GCD
230 LET LCM = MLCM / GCD * NLCM
240 RETURN

395 ' Calculate GCD
400 WHILE MGCD <> 0
410  LET TMP = MGCD
420  LET MGCD = NGCD MOD MGCD
430  LET NGCD = TMP
440 WEND
450 LET GCD = NGCD
460 RETURN

```



## Haskell


That is already available as the function ''lcm'' in the Prelude. Here's the implementation:


```haskell
lcm :: (Integral a) => a -> a -> a
lcm _ 0 =  0
lcm 0 _ =  0
lcm x y =  abs ((x `quot` (gcd x y)) * y)
```


=={{header|Icon}} and {{header|Unicon}}==
The lcm routine from the Icon Programming Library uses gcd.  The routine is


```Icon
link numbers
procedure main()
write("lcm of 18, 36 = ",lcm(18,36))
write("lcm of 0, 9 36 = ",lcm(0,9))
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers provides lcm and gcd] and looks like this:

```Icon
procedure lcm(i, j)		#: least common multiple
   if (i =  0) | (j = 0) then return 0
   return abs(i * j) / gcd(i, j)
end
```



## J

J provides the dyadic verb <code>*.</code> which returns the least common multiple of its left and right arguments.


```j
      12 *. 18
36
   12 *. 18 22
36 132
   *./ 12 18 22
396
   0 1 0 1 *. 0 0 1 1  NB. for truth valued arguments (0 and 1) it is equivalent to "and"
0 0 0 1
   *./~ 0 1
0 0
0 1
```


Note: least common multiple is the original boolean multiplication. Constraining the universe of values to 0 and 1 allows us to additionally define logical negation (and boolean algebra was redefined to include this constraint in the early 1900s - the original concept of boolean algebra is now known as a boolean ring).


## Java


```java
import java.util.Scanner;

public class LCM{
   public static void main(String[] args){
      Scanner aScanner = new Scanner(System.in);

      //prompts user for values to find the LCM for, then saves them to m and n
      System.out.print("Enter the value of m:");
      int m = aScanner.nextInt();
      System.out.print("Enter the value of n:");
      int n = aScanner.nextInt();
      int lcm = (n == m || n == 1) ? m :(m == 1 ? n : 0);
      /* this section increases the value of mm until it is greater
      / than or equal to nn, then does it again when the lesser
      / becomes the greater--if they aren't equal. If either value is 1,
      / no need to calculate*/
      if (lcm == 0) {
         int mm = m, nn = n;
         while (mm != nn) {
             while (mm < nn) { mm += m; }
             while (nn < mm) { nn += n; }
         }
         lcm = mm;
      }
      System.out.println("lcm(" + m + ", " + n + ") = " + lcm);
   }
}
```



## JavaScript



### ES5

Computing the least common multiple of an integer array, using the associative law:

<math>\operatorname{lcm}(a,b,c)=\operatorname{lcm}(\operatorname{lcm}(a,b),c),</math>

<math>\operatorname{lcm}(a_1,a_2,\ldots,a_n) = \operatorname{lcm}(\operatorname{lcm}(a_1,a_2,\ldots,a_{n-1}),a_n).</math>


```javascript
function LCM(A)  // A is an integer array (e.g. [-50,25,-45,-18,90,447])
{
    var n = A.length, a = Math.abs(A[0]);
    for (var i = 1; i < n; i++)
     { var b = Math.abs(A[i]), c = a;
       while (a && b){ a > b ? a %= b : b %= a; }
       a = Math.abs(c*A[i])/(a+b);
     }
    return a;
}

/* For example:
   LCM([-50,25,-45,-18,90,447]) -> 67050
*/
```




### ES6

```JavaScript
(() => {
    'use strict';

    // gcd :: Integral a => a -> a -> a
    let gcd = (x, y) => {
        let _gcd = (a, b) => (b === 0 ? a : _gcd(b, a % b)),
            abs = Math.abs;
        return _gcd(abs(x), abs(y));
    }

    // lcm :: Integral a => a -> a -> a
    let lcm = (x, y) =>
        x === 0 || y === 0 ? 0 : Math.abs(Math.floor(x / gcd(x, y)) * y);

    // TEST
    return lcm(12, 18);

})();
```


```txt
36
```



## jq

Direct method

```jq
# Define the helper function to take advantage of jq's tail-recursion optimization
def lcm(m; n):
  def _lcm:
    # state is [m, n, i]
    if (.[2] % .[1]) == 0 then .[2] else (.[0:2] + [.[2] + m]) | _lcm end;
  [m, n, m] | _lcm;
```


## Julia

Built-in function:

```julia
lcm(m,n)
```



## K


```K
   gcd:{:[~x;y;_f[y;x!y]]}
   lcm:{_abs _ x*y%gcd[x;y]}

   lcm .'(12 18; -6 14; 35 0)
36 42 0

   lcm/1+!20
232792560
```



## Kotlin


```scala
fun main(args: Array<String>) {
    fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    fun lcm(a: Int, b: Int) = a * b / gcd(a, b)
    println(lcm(15, 9))
}

```



## LabVIEW

Requires [[Greatest common divisor#LabVIEW|GCD]]. {{VI solution|LabVIEW_Least_common_multiple.png}}




## Lasso


```Lasso
define gcd(a,b) => {
	while(#b != 0) => {
		local(t = #b)
		#b = #a % #b
		#a = #t
	}
	return #a
}
define lcm(m,n) => {
	 #m == 0 || #n == 0 ? return 0
	 local(r = (#m * #n) / decimal(gcd(#m, #n)))
	 return integer(#r)->abs
}

lcm(-6, 14)
lcm(2, 0)
lcm(12, 18)
lcm(12, 22)
lcm(7, 31)
```

```txt
42
0
36
132
217
```



## Liberty BASIC


```lb
print "Least Common Multiple of 12 and 18 is "; LCM(12, 18)
end

function LCM(m, n)
    LCM = abs(m * n) / GCD(m, n)
end function

function GCD(a, b)
    while b
        c = a
        a = b
        b = c mod b
    wend
    GCD = abs(a)
end function

```



## Logo


```logo
to abs :n
  output sqrt product :n :n
end

to gcd :m :n
  output ifelse :n = 0 [ :m ] [ gcd :n modulo :m :n ]
end

to lcm :m :n
  output quotient (abs product :m :n) gcd :m :n
end
```


Demo code:


```logo
print lcm 38 46>
```


Output:


```txt
874
```



## Lua


```lua
function gcd( m, n )
    while n ~= 0 do
        local q = m
        m = n
        n = q % n
    end
    return m
end

function lcm( m, n )
    return ( m ~= 0 and n ~= 0 ) and m * n / gcd( m, n ) or 0
end

print( lcm(12,18) )
```



## Maple

The least common multiple of two integers is computed by the built-in procedure ilcm in Maple.  This should not be confused with lcm, which computes the least common multiple of polynomials.

```Maple>
 ilcm( 12, 18 );
                                   36

```



## Mathematica



```Mathematica
LCM[18,12]
-> 36
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
 lcm(a,b)
```



## Maxima


```maxima
lcm(a, b);   /* a and b may be integers or polynomials */

/* In Maxima the gcd of two integers is always positive, and a * b = gcd(a, b) * lcm(a, b),
so the lcm may be negative. To get a positive lcm, simply do */

abs(lcm(a, b))
```



## Microsoft Small Basic

```microsoftsmallbasic

Textwindow.Write("LCM(35, 21) = ")
mlcm = 35
nlcm = 21
CalculateLCM()
TextWindow.WriteLine(lcm)

Sub CalculateLCM
  mgcd = mlcm
  ngcd = nlcm
  CalculateGCD()
  lcm = mlcm / gcd * nlcm
EndSub

Sub CalculateGCD
  While mgcd <> 0
    tmp = mgcd
    mgcd = Math.Remainder(ngcd, mgcd)
    ngcd = tmp
  EndWhile
  gcd = ngcd
EndSub

```


=={{header|MK-61/52}}==
<lang>ИПA	ИПB	*	|x|	ПC	ИПA	ИПB	/	[x]	П9
ИПA	ИПB	ПA	ИП9	*	-	ПB	x=0	05	ИПC
ИПA	/	С/П
```



## ML

=
## mLite
=

```ocaml
fun gcd (a, 0) = a
      | (0, b) = b
      | (a, b) where (a < b)
               = gcd (a, b rem a)
      | (a, b) = gcd (b, a rem b)

fun lcm (a, b) = let val d = gcd (a, b)
                 in a * b div d
                 end

```


=={{header|Modula-2}}==
```modula2

MODULE LeastCommonMultiple;

FROM STextIO IMPORT
  WriteString, WriteLn;
FROM SWholeIO IMPORT
  WriteInt;

PROCEDURE GCD(M, N: INTEGER): INTEGER;
VAR
  Tmp: INTEGER;
BEGIN
  WHILE M <> 0 DO
    Tmp := M;
    M := N MOD M;
    N := Tmp;
  END;
  RETURN N;
END GCD;

PROCEDURE LCM(M, N: INTEGER): INTEGER;
BEGIN
  RETURN M / GCD(M, N) * N;
END LCM;

BEGIN
  WriteString("LCM(35, 21) = ");
  WriteInt(LCM(35, 21), 1);
  WriteLn;
END LeastCommonMultiple.

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 3000

runSample(arg)
return

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method lcm(m_, n_) public static
  L_ = m_ * n_ % gcd(m_, n_)
  return L_

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- Euclid's algorithm - iterative implementation
method gcd(m_, n_) public static
  loop while n_ > 0
    c_ = m_ // n_
    m_ = n_
    n_ = c_
    end
  return m_

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg samples
  if samples = '' | samples = '.' then
    samples = '-6 14 =    42 |' -
               '3  4 =    12 |' -
              '18 12 =    36 |' -
               '2  0 =     0 |' -
               '0 85 =     0 |' -
              '12 18 =    36 |' -
               '5 12 =    60 |' -
              '12 22 =   132 |' -
               '7 31 =   217 |' -
             '117 18 =   234 |' -
              '38 46 =   874 |' -
           '18 12 -5 =   180 |' -
           '-5 18 12 =   180 |' - -- confirm that other permutations work
           '12 -5 18 =   180 |' -
        '18 12 -5 97 = 17460 |' -
              '30 42 =   210 |' -
              '30 42 =     . |' - -- 210; no verification requested
              '18 12'             -- 36

  loop while samples \= ''
    parse samples sample '|' samples
    loop while sample \= ''
      parse sample mnvals '=' chk sample
      if chk = '' then chk = '.'
      mv = mnvals.word(1)
      loop w_ = 2 to mnvals.words mnvals
        nv = mnvals.word(w_)
        mv = mv.abs
        nv = nv.abs
        mv = lcm(mv, nv)
        end w_
      lv = mv
      select case chk
        when '.' then state = ''
        when lv  then state = '(verified)'
        otherwise     state = '(failed)'
        end
      mnvals = mnvals.space(1, ',').changestr(',', ', ')
      say 'lcm of' mnvals.right(15.max(mnvals.length)) 'is' lv.right(5.max(lv.length)) state
      end
    end

  return

```

```txt

lcm of          -6, 14 is    42 (verified)
lcm of            3, 4 is    12 (verified)
lcm of          18, 12 is    36 (verified)
lcm of            2, 0 is     0 (verified)
lcm of           0, 85 is     0 (verified)
lcm of          12, 18 is    36 (verified)
lcm of           5, 12 is    60 (verified)
lcm of          12, 22 is   132 (verified)
lcm of           7, 31 is   217 (verified)
lcm of         117, 18 is   234 (verified)
lcm of          38, 46 is   874 (verified)
lcm of      18, 12, -5 is   180 (verified)
lcm of      -5, 18, 12 is   180 (verified)
lcm of      12, -5, 18 is   180 (verified)
lcm of  18, 12, -5, 97 is 17460 (verified)
lcm of          30, 42 is   210 (verified)
lcm of          30, 42 is   210
lcm of          18, 12 is    36

```



## Nim


```nim
proc gcd(u, v): auto =
  var
    t = 0
    u = u
    v = v
  while v != 0:
    t = u
    u = v
    v = t %% v
  abs(u)

proc lcm(a, b): auto = abs(a * b) div gcd(a, b)

echo lcm(12, 18)
echo lcm(-6, 14)
```



## Objeck

```objeck

class LCM {
  function : Main(args : String[]) ~ Nil {
    IO.Console->Print("lcm(35, 21) = ")->PrintLine(lcm(21,35));
  }

  function : lcm(m : Int, n : Int) ~ Int {
    return m / gcd(m, n) * n;
  }

  function : gcd(m : Int, n : Int) ~ Int {
    tmp : Int;
    while(m <> 0) { tmp := m; m := n % m; n := tmp; };
    return n;
  }
}

```



## OCaml



```ocaml
let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let () =
  Printf.printf "lcm(35, 21) = %d\n" (lcm 21 35)
```




## Oforth


lcm is already defined into Integer class :

```oforth
12 18 lcm>
```



## ooRexx


```ooRexx

say lcm(18, 12)

-- calculate the greatest common denominator of a numerator/denominator pair
::routine gcd private
  use arg x, y

  loop while y \= 0
      -- check if they divide evenly
      temp = x // y
      x = y
      y = temp
  end
  return x

-- calculate the least common multiple of a numerator/denominator pair
::routine lcm private
  use arg x, y
  return x / gcd(x, y) * y

```



## Order

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8gcd ORDER_PP_FN( \
8fn(8U, 8V,                            \
    8if(8isnt_0(8V), 8gcd(8V, 8remainder(8U, 8V)), 8U)))

#define ORDER_PP_DEF_8lcm ORDER_PP_FN( \
8fn(8X, 8Y,                            \
    8if(8or(8is_0(8X), 8is_0(8Y)),     \
        0,                             \
        8quotient(8times(8X, 8Y), 8gcd(8X, 8Y)))))
// No support for negative numbers

ORDER_PP( 8to_lit(8lcm(12, 18)) )   // 36
```



## PARI/GP

Built-in function:

```parigp
lcm>
```



## Pascal


```pascal
Program LeastCommonMultiple(output);

function lcm(a, b: longint): longint;
  begin
    lcm := a;
    while (lcm mod b) <> 0 do
      inc(lcm, a);
  end;

begin
  writeln('The least common multiple of 12 and 18 is: ', lcm(12, 18));
end.
```

Output:

```txt
The least common multiple of 12 and 18 is: 36

```



## Perl

Using GCD:

```Perl
sub gcd {
	my ($x, $y) = @_;
	while ($x) { ($x, $y) = ($y % $x, $x) }
	$y
}

sub lcm {
	my ($x, $y) = @_;
	($x && $y) and $x / gcd($x, $y) * $y or 0
}

print lcm(1001, 221);
```

Or by repeatedly increasing the smaller of the two until LCM is reached:
```perl
sub lcm {
	use integer;
	my ($x, $y) = @_;
	my ($f, $s) = @_;
	while ($f != $s) {
		($f, $s, $x, $y) = ($s, $f, $y, $x) if $f > $s;
		$f = $s / $x * $x;
		$f += $x if $f < $s;
	}
	$f
}

print lcm(1001, 221);
```



## Perl 6

This function is provided as an infix so that it can be used productively with various metaoperators.

```perl6
say 3 lcm 4;            # infix
say [lcm] 1..20;        # reduction
say ~(1..10 Xlcm 1..10) # cross
```

```txt
12
232792560
1 2 3 4 5 6 7 8 9 10 2 2 6 4 10 6 14 8 18 10 3 6 3 12 15 6 21 24 9 30 4 4 12 4 20 12 28 8 36 20 5 10 15 20 5 30 35 40 45 10 6 6 6 12 30 6 42 24 18 30 7 14 21 28 35 42 7 56 63 70 8 8 24 8 40 24 56 8 72 40 9 18 9 36 45 18 63 72 9 90 10 10 30 20 10 30 70 40 90 10
```



## Phix


```Phix
function lcm(integer m, integer n)
    return m / gcd(m, n) * n
end function
```



## PHP

```php
echo lcm(12, 18) == 36;

function lcm($m, $n) {
    if ($m == 0 || $n == 0) return 0;
    $r = ($m * $n) / gcd($m, $n);
    return abs($r);
}

function gcd($a, $b) {
    while ($b != 0) {
        $t = $b;
        $b = $a % $b;
        $a = $t;
    }
    return $a;
}
```



## PicoLisp

Using 'gcd' from [[Greatest common divisor#PicoLisp]]:

```PicoLisp
(de lcm (A B)
   (abs (*/ A B (gcd A B))) )
```



## PL/I


```PL/I

/* Calculate the Least Common Multiple of two integers. */

LCM: procedure options (main);          /* 16 October 2013 */
   declare (m, n) fixed binary (31);

   get (m, n);
   put edit ('The LCM of ', m, ' and ', n, ' is', LCM(m, n)) (a, x(1));

LCM: procedure (m, n) returns (fixed binary (31));
   declare (m, n) fixed binary (31) nonassignable;

   if m = 0 | n = 0 then return (0);
   return (abs(m*n) / GCD(m, n));
end LCM;

GCD: procedure (a, b) returns (fixed binary (31)) recursive;
   declare (a, b) fixed binary (31);

   if b = 0 then return (a);

   return (GCD (b, mod(a, b)) );

end GCD;
end LCM;

```


```txt

The LCM of              14  and              35  is             70

```



## PowerShell


### version 1


```PowerShell

function gcd ($a, $b)  {
    function pgcd ($n, $m)  {
        if($n -le $m) {
            if($n -eq 0) {$m}
            else{pgcd $n ($m-$n)}
        }
        else {pgcd $m $n}
    }
    $n = [Math]::Abs($a)
    $m = [Math]::Abs($b)
    (pgcd $n $m)
}
function lcm ($a, $b)  {
    [Math]::Abs($a*$b)/(gcd $a $b)
}
lcm 12 18

```



### version 2

version2 is faster than version1


```PowerShell

function gcd ($a, $b)  {
    function pgcd ($n, $m)  {
        if($n -le $m) {
            if($n -eq 0) {$m}
            else{pgcd $n ($m%$n)}
        }
        else {pgcd $m $n}
    }
    $n = [Math]::Abs($a)
    $m = [Math]::Abs($b)
    (pgcd $n $m)
}
function lcm ($a, $b)  {
    [Math]::Abs($a*$b)/(gcd $a $b)
}
lcm 12 18

```


<b>Output:</b>

```txt

36

```



## Prolog

SWI-Prolog knows gcd.

```Prolog
lcm(X, Y, Z) :-
	Z is abs(X * Y) / gcd(X,Y).
```


Example:

```txt
 ?- lcm(18,12, Z).
Z = 36.

```


## PureBasic


```PureBasic
Procedure GCDiv(a, b); Euclidean algorithm
  Protected r
  While b
    r = b
    b = a%b
    a = r
  Wend
  ProcedureReturn a
EndProcedure

Procedure LCM(m,n)
  Protected t
  If m And n
    t=m*n/GCDiv(m,n)
  EndIf
  ProcedureReturn t*Sign(t)
EndProcedure
```



## Python


### Functional


### =gcd=

Using the fractions libraries [http://docs.python.org/library/fractions.html?highlight=fractions.gcd#fractions.gcd gcd] function:

```python
>>>
 import fractions
>>> def lcm(a,b): return abs(a * b) / fractions.gcd(a,b) if a and b else 0

>>> lcm(12, 18)
36
>>> lcm(-6, 14)
42
>>> assert lcm(0, 2) == lcm(2, 0) == 0
>>>
```


Or, for compositional flexibility, a curried '''lcm''', expressed in terms of our own '''gcd''' function:

```python
'''Least common multiple'''

from inspect import signature


# lcm :: Int -> Int -> Int
def lcm(x):
    '''The smallest positive integer divisible
       without remainder by both x and y.
    '''
    return lambda y: 0 if 0 in (x, y) else abs(
        y * (x // gcd_(x)(y))
    )


# gcd_ :: Int -> Int -> Int
def gcd_(x):
    '''The greatest common divisor in terms of
       the divisibility preordering.
    '''
    def go(a, b):
        return go(b, a % b) if 0 != b else a
    return lambda y: go(abs(x), abs(y))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    print(
        fTable(
            __doc__ + 's of 60 and [12..20]:'
        )(repr)(repr)(
            lcm(60)
        )(enumFromTo(12)(20))
    )

    pairs = [(0, 2), (2, 0), (-6, 14), (12, 18)]
    print(
        fTable(
            '\n\n' + __doc__ + 's of ' + repr(pairs) + ':'
        )(repr)(repr)(
            uncurry(lcm)
        )(pairs)
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple, derived from
       a vanilla or curried function.
    '''
    if 1 < len(signature(f).parameters):
        return lambda xy: f(*xy)
    else:
        return lambda xy: f(xy[0])(xy[1])


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


# FORMATTING ----------------------------------------------

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

```txt
Least common multiples of 60 and [12..20]:
12 -> 60
13 -> 780
14 -> 420
15 -> 60
16 -> 240
17 -> 1020
18 -> 180
19 -> 1140
20 -> 60

Least common multiples of [(0, 2), (2, 0), (-6, 14), (12, 18)]:
  (0, 2) -> 0
  (2, 0) -> 0
(-6, 14) -> 42
(12, 18) -> 36
```



### Procedural


### =Prime decomposition=

This imports [[Prime decomposition#Python]]

```python
from prime_decomposition import decompose
try:
    reduce
except NameError:
    from functools import reduce

def lcm(a, b):
    mul = int.__mul__
    if a and b:
        da = list(decompose(abs(a)))
        db = list(decompose(abs(b)))
        merge= da
        for d in da:
            if d in db: db.remove(d)
        merge += db
        return reduce(mul, merge, 1)
    return 0

if __name__ == '__main__':
    print( lcm(12, 18) )    # 36
    print( lcm(-6, 14) )    # 42
    assert lcm(0, 2) == lcm(2, 0) == 0
```



### =Iteration over multiples=


```python
>>>
 def lcm(*values):
	values = set([abs(int(v)) for v in values])
	if values and 0 not in values:
		n = n0 = max(values)
		values.remove(n)
		while any( n % m for m in values ):
			n += n0
		return n
	return 0

>>> lcm(-6, 14)
42
>>> lcm(2, 0)
0
>>> lcm(12, 18)
36
>>> lcm(12, 18, 22)
396
>>>
```



### =Repeated modulo=

```python
>>>
 def lcm(p,q):
	p, q = abs(p), abs(q)
	m = p * q
	if not m: return 0
	while True:
		p %= q
		if not p: return m // q
		q %= p
		if not q: return m // p


>>> lcm(-6, 14)
42
>>> lcm(12, 18)
36
>>> lcm(2, 0)
0
>>>
```



## Qi


```qi

(define gcd
  A 0 -> A
  A B -> (gcd B (MOD A B)))

(define lcm A B -> (/ (* A B) (gcd A B)))

```


## R


```R

"%gcd%" <- function(u, v) {ifelse(u %% v != 0, v %gcd% (u%%v), v)}

"%lcm%" <- function(u, v) { abs(u*v)/(u %gcd% v)}

print (50 %lcm% 75)

```



## Racket

Racket already has defined both lcm and gcd funtions:

```Racket
#lang racket
(lcm 3 4 5 6)        ;returns 60
(lcm 8 108)          ;returns 216
(gcd 8 108)          ;returns 4
(gcd 108 216 432)    ;returns 108
```



## Retro

This is from the math extensions library included with Retro.


```Retro
: gcd ( ab-n ) [ tuck mod dup ] while drop ;
: lcm ( ab-n ) 2over gcd [ * ] dip / ;
```



## REXX


### version 1

The   '''lcm'''   subroutine can handle any number of integers and/or arguments.

The integers (negative/zero/positive) can be (as per the   '''numeric digits''')   up to ten thousand digits.

Usage note:   the integers can be expressed as a list and/or specified as individual arguments   (or as mixed).

```rexx
/*REXX program finds the  LCM  (Least Common Multiple)  of any number of integers.      */
numeric digits 10000                             /*can handle 10k decimal digit numbers.*/
say 'the LCM of      19  and   0                   is ───►  '     lcm(19    0            )
say 'the LCM of       0  and  85                   is ───►  '     lcm( 0   85            )
say 'the LCM of      14  and  -6                   is ───►  '     lcm(14,  -6            )
say 'the LCM of      18  and  12                   is ───►  '     lcm(18   12            )
say 'the LCM of      18  and  12  and  -5          is ───►  '     lcm(18   12,   -5      )
say 'the LCM of      18  and  12  and  -5  and  97 is ───►  '     lcm(18,  12,   -5,   97)
say 'the LCM of 2**19-1  and  2**521-1             is ───►  '     lcm(2**19-1    2**521-1)
                                                 /* [↑]   7th  &  13th  Mersenne primes.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
lcm: procedure; parse arg $,_; $=$ _;           do i=3  to arg();  $=$ arg(i);  end  /*i*/
     parse var $ x $                                  /*obtain the first value in args. */
     x=abs(x)                                         /*use the absolute value of  X.   */
               do  while $\==''                       /*process the remainder of args.  */
               parse var $ ! $;    if !<0  then !=-!  /*pick off the next arg (ABS val).*/
               if !==0  then return 0                 /*if zero, then LCM is also zero. */
               d=x*!                                  /*calculate part of the LCM here. */
                      do  until !==0;    parse  value   x//!  !     with     !  x
                      end   /*until*/                 /* [↑]  this is a short & fast GCD*/
               x=d%x                                  /*divide the pre─calculated value.*/
               end   /*while*/                        /* [↑]  process subsequent args.  */
     return x                                         /*return with the LCM of the args.*/
```

'''output'''   when using the (internal) supplied list:

```txt

the LCM of      19  and   0                   is ───►   0
the LCM of       0  and  85                   is ───►   0
the LCM of      14  and  -6                   is ───►   42
the LCM of      18  and  12                   is ───►   36
the LCM of      18  and  12  and  -5          is ───►   180
the LCM of      18  and  12  and  -5  and  97 is ───►   17460
the LCM of 2**19-1  and  2**521-1             is ───►   3599124170836896975638715824247986405702540425206233163175195063626010878994006898599180426323472024265381751210505324617708575722407440034562999570663839968526337

```



### version 2

{{trans|REXX version 0}} using different argument handling-
Use as lcm(a,b,c,---)

```rexx
lcm2: procedure
x=abs(arg(1))
do k=2 to arg() While x<>0
  y=abs(arg(k))
  x=x*y/gcd2(x,y)
  end
return x

gcd2: procedure
x=abs(arg(1))
do j=2 to arg()
  y=abs(arg(j))
  If y<>0 Then Do
    do until z==0
      z=x//y
      x=y
      y=z
      end
    end
  end
return x
```



## Ring

<lang>
see lcm(24,36)

func lcm m,n
     lcm = m*n / gcd(m,n)
     return lcm

func gcd gcd, b
     while b
           c   = gcd
           gcd = b
           b   = c % b
     end
     return gcd

```



## Ruby

Ruby has an <tt>Integer#lcm</tt> method, which finds the least common multiple of two integers.


```ruby
irb(main):001:0> 12.lcm 18
=> 36
```


I can also write my own <tt>lcm</tt> method. This one takes any number of arguments.


```ruby
def gcd(m, n)
  m, n = n, m % n until n.zero?
  m.abs
end

def lcm(*args)
  args.inject(1) do |m, n|
    return 0 if n.zero?
    (m * n).abs / gcd(m, n)
  end
end

p lcm 12, 18, 22
p lcm 15, 14, -6, 10, 21
```


```txt

396
210

```



## Run BASIC

```runbasic
print lcm(22,44)

function lcm(m,n)
 while n
   t = m
   m = n
   n = t mod n
 wend
lcm = m
end function
```



## Rust

This implementation uses a recursive implementation of Stein's algorithm to calculate the gcd.

```rust
use std::cmp::{max, min};

fn gcd(a: usize, b: usize) -> usize {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y => y,
        ((0, x), _) | ((x, 0), _) => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1)) => {
            let (x, y) = (min(x, y), max(x, y));
            gcd((y - x) >> 1, x)
        }
        _ => unreachable!(),
    }
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn main() {
    println!("{}", lcm(6324, 234))
}
```



## Scala


```scala
def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)
```


```scala
lcm(12, 18)   // 36
lcm( 2,  0)   // 0
lcm(-6, 14)   // 42
```



## Scheme


```scheme>
 (lcm 108 8)
216
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: gcd (in var integer: a, in var integer: b) is func
  result
    var integer: gcd is 0;
  local
    var integer: help is 0;
  begin
    while a <> 0 do
      help := b rem a;
      b := a;
      a := help;
    end while;
    gcd := b;
  end func;

const func integer: lcm (in integer: a, in integer: b) is
  return a div gcd(a, b) * b;

const proc: main is func
  begin
    writeln("lcm(35, 21) = " <& lcm(21, 35));
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#lcm]


## Sidef

Built-in:

```ruby
say Math.lcm(1001, 221)
```


Using GCD:

```ruby
func gcd(a, b) {
    while (a) { (a, b) = (b % a, a) }
    return b
}

func lcm(a, b) {
    (a && b) ? (a / gcd(a, b) * b) : 0
}

say lcm(1001, 221)
```

```txt

17017

```



## Smalltalk

Smalltalk has a built-in <code>lcm</code> method on <code>SmallInteger</code>:

```smalltalk
12 lcm: 18>
```



## Sparkling


```sparkling
function factors(n) {
	var f = {};

	for var i = 2; n > 1; i++ {
		while n % i == 0 {
			n /= i;
			f[i] = f[i] != nil ? f[i] + 1 : 1;
		}
	}

	return f;
}

function GCD(n, k) {
	let f1 = factors(n);
	let f2 = factors(k);

	let fs = map(f1, function(factor, multiplicity) {
		let m = f2[factor];
		return m == nil ? 0 : min(m, multiplicity);
	});

	let rfs = {};
	foreach(fs, function(k, v) {
		rfs[sizeof rfs] = pow(k, v);
	});

	return reduce(rfs, 1, function(x, y) { return x * y; });
}

function LCM(n, k) {
	return n * k / GCD(n, k);
}
```


## Swift

Using the Swift GCD function.

```Swift
func lcm(a:Int, b:Int) -> Int {
    return abs(a * b) / gcd_rec(a, b)
}
```



## Tcl


```tcl
proc lcm {p q} {
    set m [expr {$p * $q}]
    if {!$m} {return 0}
    while 1 {
	set p [expr {$p % $q}]
	if {!$p} {return [expr {$m / $q}]}
	set q [expr {$q % $p}]
	if {!$q} {return [expr {$m / $p}]}
    }
}
```

Demonstration

```tcl
puts [lcm 12 18]
```

Output:
 36

=={{header|TI-83 BASIC}}==

```ti83b
lcm(12,18
               36
```



## TSE SAL


```TSESAL
// library: math: get: least: common: multiple <description></description> <version control></version control> <version>1.0.0.0.2</version> <version control></version control>
 (filenamemacro=getmacmu.s) [<Program>] [<Research>] [kn, ri, su, 20-01-2013 14:36:11]
INTEGER PROC FNMathGetLeastCommonMultipleI( INTEGER x1I, INTEGER x2I )
 //
 RETURN( x1I * x2I / FNMathGetGreatestCommonDivisorI( x1I, x2I ) )
 //
END

// library: math: get: greatest: common: divisor <description>greatest common divisor whole numbers. Euclid's algorithm. Recursive version</description> <version control></version control> <version>1.0.0.0.3</version> <version control></version control> (filenamemacro=getmacdi.s) [<Program>] [<Research>] [kn, ri, su, 20-01-2013 14:22:41]
INTEGER PROC FNMathGetGreatestCommonDivisorI( INTEGER x1I, INTEGER x2I )
 //
 IF ( x2I == 0 )
  //
  RETURN( x1I )
  //
 ENDIF
 //
 RETURN( FNMathGetGreatestCommonDivisorI( x2I, x1I MOD x2I ) )
 //
END

PROC Main()
 //
 STRING s1[255] = "10"
 STRING s2[255] = "20"
 REPEAT
  IF ( NOT ( Ask( "math: get: least: common: multiple: x1I = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
  IF ( NOT ( Ask( "math: get: least: common: multiple: x2I = ", s2, _EDIT_HISTORY_ ) ) AND ( Length( s2 ) > 0 ) ) RETURN() ENDIF
  Warn( FNMathGetLeastCommonMultipleI( Val( s1 ), Val( s2 ) ) ) // gives e.g. 10
 UNTIL FALSE
END
```




## TXR



```bash
$ txr -p '(lcm (expt 2 123) (expt 6 49) 17)'
43259338018880832376582582128138484281161556655442781051813888
```



## uBasic/4tH

<lang>Print "LCM of 12 : 18 = "; FUNC(_LCM(12,18))

End


_GCD_Iterative_Euclid Param(2)
  Local (1)
  Do While b@
    c@ = a@
    a@ = b@
    b@ = c@ % b@
  Loop
Return (ABS(a@))


_LCM Param(2)
If a@*b@
  Return (ABS(a@*b@)/FUNC(_GCD_Iterative_Euclid(a@,b@)))
Else
  Return (0)
EndIf
```

```txt
LCM of 12 : 18 = 36

0 OK, 0:330
```



## UNIX Shell

<math>\operatorname{lcm}(m, n) = \left | \frac{m \times n}{\operatorname{gcd}(m, n)} \right |</math>

```bash
gcd() {
	# Calculate $1 % $2 until $2 becomes zero.
	until test 0 -eq "$2"; do
		# Parallel assignment: set -- 1 2
		set -- "$2" "`expr "$1" % "$2"`"
	done

	# Echo absolute value of $1.
	test 0 -gt "$1" && set -- "`expr 0 - "$1"`"
	echo "$1"
}

lcm() {
	set -- "$1" "$2" "`gcd "$1" "$2"`"
	set -- "`expr "$1" \* "$2" / "$3"`"
	test 0 -gt "$1" && set -- "`expr 0 - "$1"`"
	echo "$1"
}

lcm 30 -42
# => 210
```


=
## C Shell
=

```csh
alias gcd eval \''set gcd_args=( \!*:q )	\\
	@ gcd_u=$gcd_args[2]			\\
	@ gcd_v=$gcd_args[3]			\\
	while ( $gcd_v != 0 )			\\
		@ gcd_t = $gcd_u % $gcd_v	\\
		@ gcd_u = $gcd_v		\\
		@ gcd_v = $gcd_t		\\
	end					\\
	if ( $gcd_u < 0 ) @ gcd_u = - $gcd_u	\\
	@ $gcd_args[1]=$gcd_u			\\
'\'

alias lcm eval \''set lcm_args=( \!*:q )	\\
	@ lcm_m = $lcm_args[2]			\\
	@ lcm_n = $lcm_args[3]			\\
	gcd lcm_d $lcm_m $lcm_n			\\
	@ lcm_r = ( $lcm_m * $lcm_n ) / $lcm_d	\\
	if ( $lcm_r < 0 ) @ lcm_r = - $lcm_r	\\
	@ $lcm_args[1] = $lcm_r			\\
'\'

lcm result 30 -42
echo $result
# => 210
```



## Ursa


```ursa
import "math"
out (lcm 12 18) endl console
```

```txt
36
```



## Vala


```vala

int lcm(int a, int b){
    /*Return least common multiple of two ints*/
    // check for 0's
    if (a == 0 || b == 0)
	return 0;

    // Math.abs(x) only works for doubles, Math.absf(x) for floats
    if (a < 0)
        a *= -1;
    if (b < 0)
	b *= -1;

    int x = 1;
    while (true){
        if (a * x % b == 0)
            return a*x;
        x++;
    }
}

void main(){
    int	a = 12;
    int	b = 18;

    stdout.printf("lcm(%d, %d) = %d\n",	a, b, lcm(a, b));
}

```



## VBA


```vb
Function gcd(u As Long, v As Long) As Long
    Dim t As Long
    Do While v
        t = u
        u = v
        v = t Mod v
    Loop
    gcd = u
End Function
Function lcm(m As Long, n As Long) As Long
    lcm = Abs(m * n) / gcd(m, n)
End Function
```


## VBScript


```vb
Function LCM(a,b)
	LCM = POS((a * b)/GCD(a,b))
End Function

Function GCD(a,b)
	Do
		If a Mod b > 0 Then
			c = a Mod b
			a = b
			b = c
		Else
			GCD = b
			Exit Do
		End If
	Loop
End Function

Function POS(n)
	If n < 0 Then
		POS = n * -1
	Else
		POS = n
	End If
End Function

i = WScript.Arguments(0)
j = WScript.Arguments(1)

WScript.StdOut.Write "The LCM of " & i & " and " & j & " is " & LCM(i,j) & "."
WScript.StdOut.WriteLine
```


```txt

C:\>cscript /nologo lcm.vbs 12 18
The LCM of 12 and 18 is 36.

C:\>cscript /nologo lcm.vbs 14 -6
The LCM of 14 and -6 is 42.

C:\>cscript /nologo lcm.vbs 0 35
The LCM of 0 and 35 is 0.

C:\>
```



## Wortel

Operator

```wortel
@lcm a b>
```

Number expression

```wortel
!#~km a b
```

Function (using gcd)

```wortel
&[a b] *b /a @gcd a b
```



## XBasic

```xbasic

PROGRAM "leastcommonmultiple"
VERSION "0.0001"

DECLARE FUNCTION Entry()
INTERNAL FUNCTION Gcd(m&, n&)
INTERNAL FUNCTION Lcm(m&, n&)

FUNCTION Entry()
  PRINT "LCM(35, 21) ="; Lcm(35, 21)
END FUNCTION

FUNCTION Gcd(m&, n&)
  DO WHILE m& <> 0
    tmp& = m&
    m& = n& MOD m&
    n& = tmp&
  LOOP
  RETURN n&
END FUNCTION

FUNCTION Lcm(m&, n&)
  RETURN m& / Gcd(m&, n&) * n&
END FUNCTION

END PROGRAM

```

```txt

LCM(35, 21) = 105

```



## XPL0


```XPL0
include c:\cxpl\codes;

func GCD(M,N);  \Return the greatest common divisor of M and N
int  M, N;
int  T;
[while N do     \Euclid's method
    [T:= M;  M:= N;  N:= rem(T/N)];
return M;
];

func LCM(M,N);  \Return least common multiple
int  M, N;
return abs(M*N) / GCD(M,N);

\Display the LCM of two integers entered on command line
IntOut(0, LCM(IntIn(8), IntIn(8)))
```



## Yabasic


```Yabasic
sub gcd(u, v)
    local t

    u = int(abs(u))
    v = int(abs(v))
    while(v)
        t = u
        u = v
        v = mod(t, v)
    wend
    return u
end sub

sub lcm(m, n)
    return m / gcd(m, n) * n
end sub

print "Least common multiple: ", lcm(12345, 23044)
```



## zkl


```zkl
fcn lcm(m,n){ (m*n).abs()/m.gcd(n) }  // gcd is a number method
```

```txt

zkl: lcm(12,18)
36
zkl: lcm(-6,14)
42
zkl: lcm(35,0)
0

```

