+++
title = "Fibonacci sequence"
description = ""
date = 2019-10-18T10:04:11Z
aliases = []
[extra]
id = 2667
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "0815",
  "360_assembly",
  "6502_assembly",
  "8080_assembly",
  "8th",
  "abap",
  "acl2",
  "actionscript",
  "ada",
  "advpl",
  "aime",
  "algol_68",
  "algol_w",
  "alore",
  "antlang",
  "apex",
  "apl",
  "applescript",
  "applesoft_basic",
  "arendelle",
  "arm_assembly",
  "arnoldc",
  "arturo",
  "asciidots",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "axe",
  "babel",
  "bash",
  "basic",
  "basic256",
  "batch_file",
  "battlestar",
  "bbc_basic",
  "bc",
  "beeswax",
  "befunge",
  "bracmat",
  "brat",
  "burlesque",
  "c",
  "cat",
  "chapel",
  "chef",
  "clio",
  "clojure",
  "cmake",
  "cobol",
  "coffeescript",
  "comefrom0x10",
  "commodore_basic",
  "common_lisp",
  "computer_zero_assembly",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dc",
  "delphi",
  "dwscript",
  "dyalect",
  "dyalog_apl",
  "e",
  "easylang",
  "echolisp",
  "ecl",
  "edsac_order_code",
  "eiffel",
  "ela",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "falcon",
  "false",
  "fancy",
  "fantom",
  "fexl",
  "focal",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "frink",
  "frisc_assembly",
  "funl",
  "futhark",
  "futurebasic",
  "gap",
  "gecho",
  "gfa_basic",
  "gml",
  "gnu_apl_dyalog_apl",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "haxe",
  "hicest",
  "hope",
  "hy",
  "idl",
  "idris",
  "integer_basic",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "k",
  "kabap",
  "kotlin",
  "l",
  "labview",
  "lambdatalk",
  "lang5",
  "langur",
  "lasso",
  "latitude",
  "lfe",
  "liberty_basic",
  "lingo",
  "lisaac",
  "livecode",
  "llvm",
  "logo",
  "lolcode",
  "lsl",
  "lua",
  "luck",
  "lush",
  "m2000_interpreter",
  "m4",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "mercury",
  "metafont",
  "microsoft_small_basic",
  "min",
  "miniscript",
  "mips_assembly",
  "mirah",
  "ml",
  "ml_i",
  "mlite",
  "monicelli",
  "montilang",
  "mumps",
  "nanoquery",
  "nemerle",
  "nesl",
  "netrexx",
  "newlisp",
  "ngs",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "opl",
  "order",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pir",
  "pl_i",
  "pl_pgsql",
  "pl_sql",
  "pop11",
  "postscript",
  "potion",
  "powerbasic",
  "powershell",
  "processing",
  "prolog",
  "pure",
  "purebasic",
  "purity",
  "python",
  "qbasic",
  "qi",
  "r",
  "ra",
  "racket",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "rockstar",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "sed",
  "seed7",
  "sequencel",
  "setl",
  "shen",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "skookumscript",
  "slate",
  "smalltalk",
  "smart_basic",
  "snobol4",
  "snusp",
  "softbridge_basic",
  "spin",
  "spl",
  "sql",
  "ssem",
  "standard_ml",
  "stata",
  "streamit",
  "supercollider",
  "swift",
  "tailspin",
  "tcl",
  "tern",
  "tse_sal",
  "tuscript",
  "unix_shell",
  "unixpipes",
  "ursa",
  "ursala",
  "v",
  "vala",
  "vax_assembly",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic",
  "visual_basic_dotnet",
  "wart",
  "wdte",
  "whitespace",
  "wrapl",
  "x86_assembly",
  "xeec",
  "xlisp",
  "xojo",
  "xquery",
  "zkl",
  "zx_spectrum_basic",
]
+++

The '''Fibonacci sequence''' is a sequence   <big> F<sub>n</sub> </big>    of natural numbers defined recursively:

      <big><big> F<sub>0</sub> = 0 </big></big>
      <big><big> F<sub>1</sub> = 1 </big></big>
      <big><big> F<sub>n</sub> = F<sub>n-1</sub> + F<sub>n-2</sub>, if n>1 </big></big>


## Task

Write a function to generate the   <big> n<sup>th</sup> </big>   Fibonacci number.

Solutions can be iterative or recursive (though recursive solutions are generally considered too slow and are mostly used as an exercise in recursion).

The sequence is sometimes extended into negative numbers by using a straightforward inverse of the positive definition:

      <big><big> F<sub>n</sub> = F<sub>n+2</sub> - F<sub>n+1</sub>, if n<0   </big></big>

support for negative     <big> n </big>     in the solution is optional.


## Related tasks

*   [[Fibonacci n-step number sequences‎]]
*   [[Leonardo numbers]]


## References

*   [[wp:Fibonacci number|Wikipedia, Fibonacci number]]
*   [[wp:Lucas number|Wikipedia, Lucas number]]
*   [http://mathworld.wolfram.com/FibonacciNumber.html MathWorld, Fibonacci Number]
*   [http://www.math-cs.ucmo.edu/~curtisc/articles/howardcooper/genfib4.pdf Some identities for r-Fibonacci numbers]
*   [[oeis:A000045|OEIS Fibonacci numbers]]
*   [[oeis:A000032|OEIS Lucas numbers]]





## 0815


```0815

%<:0D:>~$<:01:~%>=<:a94fad42221f2702:>~>
}:_s:{x{={~$x+%{=>~>x~-x<:0D:~>~>~^:_s:?

```



## 360 Assembly

For maximum compatibility, programs use only the basic instruction set.

### using fullword integers


```360asm
*        Fibonacci sequence    05/11/2014
*        integer (31 bits) = 10 decimals -> max fibo(46)
FIBONACC CSECT
         USING FIBONACC,R12    base register
SAVEAREA B     STM-SAVEAREA(R15) skip savearea
         DC    17F'0'          savearea
         DC    CL8'FIBONACC'   eyecatcher
STM      STM   R14,R12,12(R13) save previous context
         ST    R13,4(R15)      link backward
         ST    R15,8(R13)      link forward
         LR    R12,R15         set addressability
*        ----
         LA    R1,0            f(n-2)=0
         LA    R2,1            f(n-1)=1
         LA    R4,2            n=2
         LA    R6,1            step
         LH    R7,NN           limit
LOOP     EQU   *               for n=2 to nn
         LR    R3,R2             f(n)=f(n-1)
         AR    R3,R1             f(n)=f(n-1)+f(n-2)
         CVD   R4,PW             n  convert binary to packed (PL8)
         UNPK  ZW,PW             packed (PL8) to zoned (ZL16)
         MVC   CW,ZW             zoned (ZL16) to  char (CL16)
         OI    CW+L'CW-1,X'F0'   zap sign
         MVC   WTOBUF+5(2),CW+14 output
         CVD   R3,PW             f(n) binary to packed decimal (PL8)
         MVC   ZN,EM             load mask
         ED    ZN,PW             packed dec (PL8) to char (CL20)
         MVC   WTOBUF+9(14),ZN+6 output
         WTO   MF=(E,WTOMSG)     write buffer
         LR    R1,R2             f(n-2)=f(n-1)
         LR    R2,R3             f(n-1)=f(n)
         BXLE  R4,R6,LOOP      endfor n
*        ----
         LM    R14,R12,12(R13) restore previous savearea pointer
         XR    R15,R15         return code set to 0
         BR    R14             return to caller
*        ----  DATA
NN       DC    H'46'           nn max n
PW       DS    PL8             15num
ZW       DS    ZL16
CW       DS    CL16
ZN       DS    CL20
*                  ' b 0 0 0 , 0 0 0 , 0 0 0 , 0 0 0 , 0 0 0'  15num
EM       DC    XL20'402020206B2020206B2020206B2020206B202120'  mask
WTOMSG   DS    0F
         DC    H'80',XL2'0000'
*                   fibo(46)=1836311903
WTOBUF   DC    CL80'fibo(12)=1234567890'
         REGEQU
         END   FIBONACC
```

```txt

...
fibo(41)=   165,580,141
fibo(42)=   267,914,296
fibo(43)=   433,494,437
fibo(44)=   701,408,733
fibo(45)= 1,134,903,170
fibo(46)= 1,836,311,903

```


### using packed decimals


```360asm
*        Fibonacci sequence        31/07/2018
*        packed dec (PL8) = 15 decimals => max fibo(73)
FIBOWTOP CSECT
         USING  FIBOWTOP,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
*        ----
         ZAP    FNM2,=P'0'         f(0)=0
         ZAP    FNM1,=P'1'         f(1)=1
         LA     R4,2               n=2
         LA     R6,1               step
         LH     R7,NN              limit
LOOP     EQU    *                  for n=2 to nn
         ZAP    FN,FNM1              f(n)=f(n-2)
         AP     FN,FNM2              f(n)=f(n-1)+f(n-2)
         CVD    R4,PW                n
         MVC    ZN,EM                load mask
         ED     ZN,PW                packed dec (PL8) to char (CL16)
         MVC    WTOBUF+5(2),ZN+L'ZN-2  output
         MVC    ZN,EM                load mask
         ED     ZN,FN                packed dec (PL8) to char (CL16)
         MVC    WTOBUF+9(L'ZN),ZN        output
         WTO    MF=(E,WTOMSG)        write buffer
         ZAP    FNM2,FNM1            f(n-2)=f(n-1)
         ZAP    FNM1,FN              f(n-1)=f(n)
         BXLE   R4,R6,LOOP         endfor n
*        ----
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
*        ----   DATA
NN       DC     H'73'              nn
FNM2     DS     PL8                f(n-2)
FNM1     DS     PL8                f(n-1)
FN       DS     PL8                f(n)
PW       DS     PL8                15num
ZN       DS     CL20
*                   ' b 0 0 0 , 0 0 0 , 0 0 0 , 0 0 0 , 0 0 0'  15num
EM       DC     XL20'402020206B2020206B2020206B2020206B202120'  mask
WTOMSG   DS     0F
         DC     H'80',XL2'0000'
*                    fibo(73)=806515533049393
WTOBUF   DC     CL80'fibo(12)=123456789012345 '
         REGEQU
         END    FIBOWTOP
```

```txt

...
fibo(68)=  72,723,460,248,141
fibo(69)= 117,669,030,460,994
fibo(70)= 190,392,490,709,135
fibo(71)= 308,061,521,170,129
fibo(72)= 498,454,011,879,264
fibo(73)= 806,515,533,049,393

```



## 6502 Assembly

This subroutine stores the first <i>n</i>—by default the first ten—Fibonacci numbers in memory, beginning (because, why not?) at address 3867 decimal = F1B hex. Intermediate results are stored in three sequential addresses within the low 256 bytes of memory, which are the most economical to access.

The results are calculated and stored, but are not output to the screen or any other physical device: how to do that would depend on the hardware and the operating system.

```6502asm
       LDA  #0
       STA  $F0     ; LOWER NUMBER
       LDA  #1
       STA  $F1     ; HIGHER NUMBER
       LDX  #0
LOOP:  LDA  $F1
       STA  $0F1B,X
       STA  $F2     ; OLD HIGHER NUMBER
       ADC  $F0
       STA  $F1     ; NEW HIGHER NUMBER
       LDA  $F2
       STA  $F0     ; NEW LOWER NUMBER
       INX
       CPX  #$0A    ; STOP AT FIB(10)
       BMI  LOOP
       RTS          ; RETURN FROM SUBROUTINE
```



## 8080 Assembly

This subroutine expects to be called with the value of <math>n</math> in register <tt>A</tt>, and returns <math>f(n)</math> also in <tt>A</tt>. You may want to take steps to save the previous contents of <tt>B</tt>, <tt>C</tt>, and <tt>D</tt>. The routine only works with fairly small values of <math>n</math>.

```8080asm
FIBNCI: MOV  C,  A  ; C will store the counter
        DCR  C      ; decrement, because we know f(1) already
        MVI  A,  1
        MVI  B,  0
LOOP:   MOV  D,  A
        ADD  B      ; A := A + B
        MOV  B,  D
        DCR  C
        JNZ  LOOP   ; jump if not zero
        RET         ; return from subroutine
```



## 8th

An iterative solution:

```forth

: fibon \ n -- fib(n)
  >r 0 1
  ( tuck n:+ ) \ fib(n-2) fib(n-1) -- fib(n-1) fib(n)
  r> n:1- times ;

: fib \ n -- fib(n)
  dup 1 n:= if 1 ;; then
  fibon nip ;

```



## ABAP


### Iterative


```ABAP
FORM fibonacci_iter USING index TYPE i
                    CHANGING number_fib TYPE i.
  DATA: lv_old type i,
        lv_cur type i.
  Do index times.
    If sy-index = 1 or sy-index = 2.
      lv_cur = 1.
      lv_old = 0.
    endif.
    number_fib = lv_cur + lv_old.
    lv_old = lv_cur.
    lv_cur = number_fib.
  enddo.
ENDFORM.
```



### Impure Functional

```ABAP
cl_demo_output=>display( REDUCE #( INIT fibnm = VALUE stringtab( ( |0| ) ( |1| ) )
                                        n TYPE string
                                        x = `0`
                                        y = `1`
                                      FOR i = 1 WHILE i <= 100
                                     NEXT n = ( x + y )
                                          fibnm = VALUE #( BASE fibnm ( n ) )
                                          x = y
                                          y = n ) ).
```



## ACL2

Fast, tail recursive solution:

```Lisp
(defun fast-fib-r (n a b)
   (if (or (zp n) (zp (1- n)))
       b
       (fast-fib-r (1- n) b (+ a b))))

(defun fast-fib (n)
   (fast-fib-r n 1 1))

(defun first-fibs-r (n i)
   (declare (xargs :measure (nfix (- n i))))
   (if (zp (- n i))
       nil
       (cons (fast-fib i)
             (first-fibs-r n (1+ i)))))

(defun first-fibs (n)
   (first-fibs-r n 0))
```


```txt
&gt;(first-fibs 20)
(1 1 2 3 5 8 13 21 34 55 89
   144 233 377 610 987 1597 2584 4181 6765)

```



## ActionScript


```actionscript
public function fib(n:uint):uint
{
    if (n < 2)
        return n;

    return fib(n - 1) + fib(n - 2);
}
```



## Ada



### Recursive


```Ada
with Ada.Text_IO, Ada.Command_Line;

procedure Fib is

   X: Positive := Positive'Value(Ada.Command_Line.Argument(1));

   function Fib(P: Positive) return Positive is
   begin
      if P <= 2 then
         return 1;
      else
         return Fib(P-1) + Fib(P-2);
      end if;
   end Fib;

begin
   Ada.Text_IO.Put("Fibonacci(" & Integer'Image(X) & " ) = ");
   Ada.Text_IO.Put_Line(Integer'Image(Fib(X)));
end Fib;
```


===Iterative, build-in integers===

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Fibonacci is
   function Fibonacci (N : Natural) return Natural is
      This : Natural := 0;
      That : Natural := 1;
      Sum  : Natural;
   begin
      for I in 1..N loop
         Sum  := This + That;
         That := This;
         This := Sum;
      end loop;
      return This;
   end Fibonacci;
begin
   for N in 0..10 loop
      Put_Line (Positive'Image (Fibonacci (N)));
   end loop;
end Test_Fibonacci;
```

```txt

 0
 1
 1
 2
 3
 5
 8
 13
 21
 34
 55

```


===Iterative, long integers===

Using the big integer implementation from a cryptographic library [https://github.com/cforler/Ada-Crypto-Library/].


```Ada
with Ada.Text_IO, Ada.Command_Line, Crypto.Types.Big_Numbers;

procedure Fibonacci is

   X: Positive := Positive'Value(Ada.Command_Line.Argument(1));

   Bit_Length: Positive := 1 + (696 * X) / 1000;
   -- that number of bits is sufficient to store the full result.

   package LN is new Crypto.Types.Big_Numbers
     (Bit_Length + (32 - Bit_Length mod 32));
     -- the actual number of bits has to be a multiple of 32
   use LN;

   function Fib(P: Positive) return Big_Unsigned is
      Previous: Big_Unsigned := Big_Unsigned_Zero;
      Result:   Big_Unsigned := Big_Unsigned_One;
      Tmp:      Big_Unsigned;
   begin
      -- Result = 1 = Fibonacci(1)
      for I in 1 .. P-1 loop
         Tmp := Result;
         Result := Previous + Result;
         Previous := Tmp;
         -- Result = Fibonacci(I+1))
      end loop;
      return Result;
   end Fib;

begin
   Ada.Text_IO.Put("Fibonacci(" & Integer'Image(X) & " ) = ");
   Ada.Text_IO.Put_Line(LN.Utils.To_String(Fib(X)));
end Fibonacci;
```


```txt
> ./fibonacci 777
Fibonacci( 777 ) = 1081213530912648191985419587942084110095342850438593857649766278346130479286685742885693301250359913460718567974798268702550329302771992851392180275594318434818082
```



### Fast method using fast matrix exponentiation



```Ada

with ada.text_io;
use  ada.text_io;

procedure fast_fibo is
	-- We work with biggest natural integers in a 64 bits machine
	type Big_Int is mod 2**64;

	-- We provide an index type for accessing the fibonacci sequence terms
	type Index is new Big_Int;

	-- fibo is a generic function that needs a modulus type since it will return
	-- the n'th term of the fibonacci sequence modulus this type (use Big_Int to get the
	-- expected behaviour in this particular task)
	generic
		type ring_element is mod <>;
		with function "*" (a, b : ring_element) return ring_element is <>;
		function fibo (n : Index) return ring_element;
	function fibo (n : Index) return ring_element is

		type matrix is array (1 .. 2, 1 .. 2) of ring_element;

		-- f is the matrix you apply to a column containing (F_n, F_{n+1}) to get
		-- the next one containing (F_{n+1},F_{n+2})
		-- could be a more general matrix (given as a generic parameter) to deal with
		-- other linear sequences of order 2
		f : constant matrix := (1 => (0, 1), 2 => (1, 1));

		function "*" (a, b : matrix) return matrix is
		(1 => (a(1,1)*b(1,1)+a(1,2)*b(2,1), a(1,1)*b(1,2)+a(1,2)*b(2,2)),
		 2 => (a(2,1)*b(1,1)+a(2,2)*b(2,1), a(2,1)*b(1,2)+a(2,2)*b(2,2)));

		function square (m : matrix) return matrix is (m * m);

		-- Fast_Pow could be non recursive but it doesn't really matter since
		-- the number of calls is bounded up by the size (in bits) of Big_Int (e.g 64)
		function fast_pow (m : matrix; n : Index) return matrix is
		(if n = 0 then (1 => (1, 0), 2 => (0, 1)) -- = identity matrix
		 elsif n mod 2 = 0 then square (fast_pow (m, n / 2))
		 else m * square (fast_pow (m, n / 2)));

	begin
		return fast_pow (f, n)(2, 1);
	end fibo;

	function Big_Int_Fibo is new fibo (Big_Int);
begin
	-- calculate instantly F_n with n=10^15 (modulus 2^64 )
	put_line (Big_Int_Fibo (10**15)'img);
end fast_fibo;
```



## AdvPL


### Recursive


```AdvPL

#include "totvs.ch"
User Function fibb(a,b,n)
return(if(--n>0,fibb(b,a+b,n),a))

```



### Iterative


```AdvPL

#include "totvs.ch"
User Function fibb(n)
	local fnow:=0, fnext:=1, tempf
	while (--n>0)
		tempf:=fnow+fnext
		fnow:=fnext
		fnext:=tempf
	end while
return(fnext)

```



## Aime


```aime
integer
fibs(integer n)
{
    integer w;

    if (n == 0) {
        w = 0;
    } elif (n == 1) {
        w = 1;
    } else {
        integer a, b, i;

        i = 1;
        a = 0;
        b = 1;
        while (i < n) {
            w = a + b;
            a = b;
            b = w;
            i += 1;
        }
    }

    return w;
}

```



## ALGOL 68

<!-- harvested from my own contribution: http://en.literateprograms.org/Fibonacci_numbers_(ALGOL_68) -->

### Analytic

```algol68
PROC analytic fibonacci = (LONG INT n)LONG INT:(
  LONG REAL sqrt 5 = long sqrt(5);
  LONG REAL p = (1 + sqrt 5) / 2;
  LONG REAL q = 1/p;
  ROUND( (p**n + q**n) / sqrt 5 )
);

FOR i FROM 1 TO 30 WHILE
  print(whole(analytic fibonacci(i),0));
# WHILE # i /= 30 DO
  print(", ")
OD;
print(new line)
```

```txt

1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040

```



### Iterative

```algol68
PROC iterative fibonacci = (INT n)INT:
  CASE n+1 IN
    0, 1, 1, 2, 3, 5
  OUT
    INT even:=3, odd:=5;
    FOR i FROM odd+1 TO n DO
      (ODD i|odd|even) := odd + even
    OD;
    (ODD n|odd|even)
  ESAC;

FOR i FROM 0 TO 30 WHILE
  print(whole(iterative fibonacci(i),0));
# WHILE # i /= 30 DO
  print(", ")
OD;
print(new line)
```

```txt

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040

```


### Recursive

```algol68
PROC recursive fibonacci = (INT n)INT:
  ( n < 2 | n | fib(n-1) + fib(n-2));
```


### Generative

```algol68
MODE YIELDINT = PROC(INT)VOID;

PROC gen fibonacci = (INT n, YIELDINT yield)VOID: (
  INT even:=0, odd:=1;
  yield(even);
  yield(odd);
  FOR i FROM odd+1 TO n DO
    yield( (ODD i|odd|even) := odd + even )
  OD
);

main:(
  # FOR INT n IN # gen fibonacci(30, # ) DO ( #
  ##   (INT n)VOID:(
        print((" ",whole(n,0)))
  # OD # ));
    print(new line)
)
```

```txt

1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040

```


===Array (Table) Lookup===
This uses a pre-generated list, requiring much less run-time processor usage, but assumes that INT is only 31 bits wide.

```algol68
[]INT const fibonacci = []INT( -1836311903, 1134903170,
  -701408733, 433494437, -267914296, 165580141, -102334155,
  63245986, -39088169, 24157817, -14930352, 9227465, -5702887,
  3524578, -2178309, 1346269, -832040, 514229, -317811, 196418,
  -121393, 75025, -46368, 28657, -17711, 10946, -6765, 4181,
  -2584, 1597, -987, 610, -377, 233, -144, 89, -55, 34, -21, 13,
  -8, 5, -3, 2, -1, 1, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
  144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711,
  28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040,
  1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817,
  39088169, 63245986, 102334155, 165580141, 267914296, 433494437,
  701408733, 1134903170, 1836311903
)[@-46];

PROC VOID value error := stop;

PROC lookup fibonacci = (INT i)INT: (
  IF LWB const fibonacci <= i AND i<= UPB const fibonacci THEN
    const fibonacci[i]
  ELSE
    value error; SKIP
  FI
);

FOR i FROM 0 TO 30 WHILE
  print(whole(lookup fibonacci(i),0));
# WHILE # i /= 30 DO
  print(", ")
OD;
print(new line)
```

```txt

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040

```


=={{header|ALGOL-M}}==
Note that the 21st Fibonacci number (= 10946) is the largest that can be calculated without overflowing ALGOL-M's integer data type.

### =Iterative=


```algol
INTEGER FUNCTION FIBONACCI( X ); INTEGER X;
BEGIN
    INTEGER M, N, A, I;
    M := 0;
    N := 1;
    FOR I := 2 STEP 1 UNTIL X DO
    BEGIN
        A := N;
        N := M + N;
        M := A;
    END;
    FIBONACCI := N;
END;
```



### =Naively recursive=


```algol
INTEGER FUNCTION FIBONACCI( X ); INTEGER X;
BEGIN
    IF X < 3 THEN
        FIBONACCI := 1
    ELSE
        FIBONACCI := FIBONACCI( X - 2 ) + FIBONACCI( X - 1 );
END;
```



## ALGOL W


```algolw
begin
    % return the nth Fibonacci number %
    integer procedure Fibonacci( integer value n ) ;
        begin
            integer fn, fn1, fn2;
            fn2 := 1;
            fn1 := 0;
            fn  := 0;
            for i := 1 until n do begin
                fn  := fn1 + fn2;
                fn2 := fn1;
                fn1 := fn
            end ;
            fn
        end Fibonacci ;

    for i := 0 until 10 do writeon( i_w := 3, s_w := 0, Fibonacci( i ) )

end.
```

```txt

  0  1  1  2  3  5  8 13 21 34 55

```



## Alore



```Alore
def fib(n as Int) as Int
   if n < 2
      return 1
   end
   return fib(n-1) + fib(n-2)
end
```



## AntLang


```AntLang
/Sequence
fib:{<0;1> {x,<x[-1]+x[-2]>}/ range[x]}
/nth
fibn:{fib[x][x]}
```



## Apex



```Apex

/*
 author: snugsfbay
 date: March 3, 2016
 description: Create a list of x numbers in the Fibonacci sequence.
     - user may specify the length of the list
     - enforces a minimum of 2 numbers in the sequence because any fewer is not a sequence
     - enforces a maximum of 47 because further values are too large for integer data type
     - Fibonacci sequence always starts with 0 and 1 by definition
*/
public class FibNumbers{

final static Integer MIN = 2; //minimum length of sequence
final static Integer MAX = 47; //maximum length of sequence

/*
  description: method to create a list of numbers in the Fibonacci sequence
  param: user specified integer representing length of sequence should be 2-47, inclusive.
      - Sequence starts with 0 and 1 by definition so the minimum length could be as low as 2.
      - For 48th number in sequence or greater, code would require a Long data type rather than an Integer.
  return: list of integers in sequence.
*/
public static List<Integer> makeSeq(Integer len){

  List<Integer> fib = new List<Integer>{0,1}; // initialize list with first two values
  Integer i;

  if(len<MIN || len==null || len>MAX) {
      if (len>MAX){
          len=MAX; //set length to maximum if user entered too high a value
      }else{
          len=MIN; //set length to minimum if user entered too low a value or none
      }
  } //This could be refactored using teneray operator, but we want code coverage to be reflected for each condition

  //start with initial list size to find previous two values in the sequence, continue incrementing until list reaches user defined length
  for(i=fib.size(); i<len; i++){
    fib.add(fib[i-1]+fib[i-2]); //create new number based on previous numbers and add that to the list
  }

  return fib;
  }

}

```



## APL


=
## Dyalog APL
=


### =Naive Recursive=



```APL

fib←{⍵≤1:⍵ ⋄ (∇ ⍵-1)+∇ ⍵-2}

```


Read this as: In the variable "fib", store the function that says, if the argument is less than or equal to 1, return the argument.  Else, calculate the value you get when you recursively call the current function with the argument of the current argument minus one and add that to the value you get when you recursively call the current function with the argument of the current function minus two.

This naive solution requires Dyalog APL because GNU APL does not support this syntax for conditional guards.

=
## GNU APL/Dyalog APL
=

### =Array=


Since APL is an array language we'll use the following identity:
:<math>\begin{pmatrix} 1 & 1 \\ 1 & 0 \end{pmatrix}^n = \begin{pmatrix} F_{n+1} & F_n \\ F_n & F_{n-1} \end{pmatrix}.</math>
In APL:

```APL

↑+.×/N/⊂2 2⍴1 1 1 0

```

Plugging in 4 for N gives the following result:
:<math>\begin{pmatrix} 5 & 3 \\ 3 & 2 \end{pmatrix}</math>
Here's what happens:
We replicate the 2-by-2 matrix N times and then apply inner product-replication.
The ''First'' removes the shell from the ''Enclose''.
At this point we're basically done, but we need to pick out only <math>F_n</math> in order to complete the task. Here's one way:

```APL

↑0 1↓↑+.×/N/⊂2 2⍴1 1 1 0

```



### =Analytic=

An alternative approach, using Binet's formula (which was apparently known long before Binet):

```apl
⌊.5+(((1+PHI)÷2)*⍳N)÷PHI←5*.5
```



## AppleScript




### Imperative



```applescript
set fibs to {}
set x to (text returned of (display dialog "What fibbonaci number do you want?" default answer "3"))
set x to x as integer
repeat with y from 1 to x
	if (y = 1 or y = 2) then
		copy 1 to the end of fibs
	else
		copy ((item (y - 1) of fibs) + (item (y - 2) of fibs)) to the end of fibs
	end if
end repeat
return item x of fibs
```




### Functional



The simple recursive version is famously slow:


```AppleScript
on fib(n)
    if n < 1 then
        0
    else if n < 3 then
        1
    else
        fib(n - 2) + fib(n - 1)
    end if
end fib
```


but we can combine '''enumFromTo(m, n)''' with the accumulator of a higher-order '''fold/reduce''' function to memoize the series:

{{Trans|JavaScript}} (ES6 memoized fold example)
{{Trans|Haskell}} (Memoized fold example)

```AppleScript
-- fib :: Int -> Int
on fib(n)

    -- lastTwo : (Int, Int) -> (Int, Int)
    script lastTwo
        on |λ|([a, b])
            [b, a + b]
        end |λ|
    end script

    item 1 of foldl(lastTwo, {0, 1}, enumFromTo(1, n))
end fib


-- TEST -----------------------------------------------------------------------
on run

    fib(32)

    --> 2178309
end run

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

```txt
2178309
```



## Arendelle


```txt
( fibonacci , 1; 1 )

[ 98 , // 100 numbers of fibonacci

	( fibonacci[ @fibonacci? ] ,

		@fibonacci[ @fibonacci - 1 ] + @fibonacci[ @fibonacci - 2 ]

	)

	"Index: | @fibonacci? | => | @fibonacci[ @fibonacci? - 1 ] |"
]
```



## ARM Assembly

Expects to be called with <math>n</math> in R0, and will return <math>f(n)</math> in the same register.

```armasm
fibonacci:
        push  {r1-r3}
        mov   r1,  #0
        mov   r2,  #1

fibloop:
        mov   r3,  r2
        add   r2,  r1,  r2
        mov   r1,  r3
        sub   r0,  r0,  #1
        cmp   r0,  #1
        bne   fibloop

        mov   r0,  r2
        pop   {r1-r3}
        mov   pc,  lr
```



## ArnoldC



```ArnoldC
IT'S SHOWTIME

HEY CHRISTMAS TREE f1
YOU SET US UP @I LIED
TALK TO THE HAND f1

HEY CHRISTMAS TREE f2
YOU SET US UP @NO PROBLEMO

HEY CHRISTMAS TREE f3
YOU SET US UP @I LIED

STICK AROUND @NO PROBLEMO

GET TO THE CHOPPER f3
HERE IS MY INVITATION f1
GET UP f2
ENOUGH TALK
TALK TO THE HAND f3

GET TO THE CHOPPER f1
HERE IS MY INVITATION f2
ENOUGH TALK

GET TO THE CHOPPER f2
HERE IS MY INVITATION f3
ENOUGH TALK

CHILL

YOU HAVE BEEN TERMINATED
```



## Arturo



### Recursive



```arturo
Fib [x]{
	if x<2 { 1 }{
		$(Fib x-1) + $(Fib x-2)
	}
}
```



### Recursive with Memoization



```arturo
Fib $(memoize [x]{
	if x<2 { 1 }{
		$(Fib x-1) + $(Fib x-2)
	}
})
```



## AsciiDots



```AsciiDots


/--#$--\
|      |
>-*>{+}/
| \+-/
1  |
#  1
|  #
|  |
.  .


```



## ATS


### Recursive


```ATS

fun fib_rec(n: int): int =
  if n >= 2 then fib_rec(n-1) + fib_rec(n-2) else n

```



### Iterative


```ATS

(*
** This one is also referred to as being tail-recursive
*)
fun
fib_trec(n: int): int =
if
n > 0
then (fix loop (i:int, r0:int, r1:int): int => if i > 1 then loop (i-1, r1, r0+r1) else r1)(n, 0, 1)
else 0

```



### Iterative and Verified


```ATS

(*
** This implementation is verified!
*)

dataprop FIB (int, int) =
  | FIB0 (0, 0) | FIB1 (1, 1)
  | {n:nat} {r0,r1:int} FIB2 (n+2, r0+r1) of (FIB (n, r0), FIB (n+1, r1))
// end of [FIB] // end of [dataprop]

fun
fibats{n:nat}
  (n: int (n))
: [r:int] (FIB (n, r) | int r) = let
  fun loop
    {i:nat | i <= n}{r0,r1:int}
  (
    pf0: FIB (i, r0), pf1: FIB (i+1, r1)
  | ni: int (n-i), r0: int r0, r1: int r1
  ) : [r:int] (FIB (n, r) | int r) =
    if (ni > 0)
      then loop{i+1}(pf1, FIB2 (pf0, pf1) | ni - 1, r1, r0 + r1)
      else (pf0 | r0)
    // end of [if]
  // end of [loop]
in
  loop {0} (FIB0 (), FIB1 () | n, 0, 1)
end // end of [fibats]

```


===Matrix-based===

```ATS

(* ****** ****** *)
//
// How to compile:
// patscc -o fib fib.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)
//
abst@ype
int3_t0ype =
  (int, int, int)
//
typedef int3 = int3_t0ype
//
(* ****** ****** *)

extern
fun int3 : (int, int, int) -<> int3
extern
fun int3_1 : int3 -<> int
extern
fun mul_int3_int3: (int3, int3) -<> int3

(* ****** ****** *)

local

assume
int3_t0ype = (int, int, int)

in (* in-of-local *)
//
implement
int3 (x, y, z) = @(x, y, z)
//
implement int3_1 (xyz) = xyz.1
//
implement
mul_int3_int3
(
  @(a,b,c), @(d,e,f)
) =
  (a*d + b*e, a*e + b*f, b*e + c*f)
//
end // end of [local]

(* ****** ****** *)
//
implement
gnumber_int<int3> (n) = int3(n, 0, n)
//
implement gmul_val<int3> = mul_int3_int3
//
(* ****** ****** *)
//
fun
fib (n: intGte(0)): int =
  int3_1(gpow_int_val<int3> (n, int3(1, 1, 0)))
//
(* ****** ****** *)

implement
main0 () =
{
//
val N = 10
val () = println! ("fib(", N, ") = ", fib(N))
val N = 20
val () = println! ("fib(", N, ") = ", fib(N))
val N = 30
val () = println! ("fib(", N, ") = ", fib(N))
val N = 40
val () = println! ("fib(", N, ") = ", fib(N))
//
} (* end of [main0] *)

```



## AutoHotkey

### Iterative

```AutoHotkey
Loop, 5
  MsgBox % fib(A_Index)
Return

fib(n)
{
  If (n < 2)
    Return n
  i := last := this := 1
  While (i <= n)
  {
    new := last + this
    last := this
    this := new
    i++
  }
  Return this
}
```



### Recursive and iterative

Source: [http://www.autohotkey.com/forum/topic44657.html AutoHotkey forum] by Laszlo

```AutoHotkey
/*
Important note: the recursive version would be very slow
without a global or static array. The iterative version
handles also negative arguments properly.
*/

FibR(n) {       ; n-th Fibonacci number (n>=0, recursive with static array Fibo)
   Static
   Return n<2 ? n : Fibo%n% ? Fibo%n% : Fibo%n% := FibR(n-1)+FibR(n-2)
}

Fib(n) {        ; n-th Fibonacci number (n < 0 OK, iterative)
   a := 0, b := 1
   Loop % abs(n)-1
      c := b, b += a, a := c
   Return n=0 ? 0 : n>0 || n&1 ? b : -b
}
```


## AutoIt


### Iterative


```AutoIt
#AutoIt Version: 3.2.10.0
$n0 = 0
$n1 = 1
$n = 10
MsgBox (0,"Iterative Fibonacci ", it_febo($n0,$n1,$n))

Func it_febo($n_0,$n_1,$N)
   $first = $n_0
   $second = $n_1
   $next = $first + $second
   $febo = 0
   For $i = 1 To $N-3
      $first = $second
      $second = $next
      $next = $first + $second
   Next
   if $n==0 Then
      $febo = 0
   ElseIf $n==1 Then
      $febo = $n_0
   ElseIf $n==2 Then
      $febo = $n_1
   Else
      $febo = $next
   EndIf
   Return $febo
EndFunc

```


### Recursive


```AutoIt
#AutoIt Version: 3.2.10.0
$n0 = 0
$n1 = 1
$n = 10
MsgBox (0,"Recursive Fibonacci ", rec_febo($n0,$n1,$n))
Func rec_febo($r_0,$r_1,$R)
   if  $R<3 Then
      if $R==2 Then
	 Return $r_1
      ElseIf $R==1 Then
	 Return $r_0
      ElseIf $R==0 Then
	 Return 0
      EndIf
      Return $R
   Else
      Return rec_febo($r_0,$r_1,$R-1) + rec_febo($r_0,$r_1,$R-2)
   EndIf
EndFunc

```



## AWK

As in many examples, this one-liner contains the function as well as testing with input from stdin, output to stdout.

```awk
$ awk 'func fib(n){return(n<2?n:fib(n-1)+fib(n-2))}{print "fib("$1")="fib($1)}'
10
fib(10)=55
```



## Axe


A recursive solution is not practical in Axe because there is no concept of variable scope in Axe.

Iterative solution:

```axe
Lbl FIB
r₁→N
0→I
1→J
For(K,1,N)
 I+J→T
 J→I
 T→J
End
J
Return
```




## bash


### Iterative


```bash

$ fib=1;j=1;while((fib<100));do echo $fib;((k=fib+j,fib=j,j=k));done

```


```txt

1
1
2
3
5
8
13
21
34
55
89

```



### Recursive


```bash
fib()
{
  if [ $1 -le 0 ]
  then
    echo 0
    return 0
  fi
  if [ $1 -le 2 ]
  then
    echo 1
  else
    a=$(fib $[$1-1])
    b=$(fib $[$1-2])
    echo $(($a+$b))
  fi
}

```



## Babel

In Babel, we can define fib using a stack-based approach that is not recursive:


```babel
fib { <- 0 1 { dup <- + -> swap } -> times zap } <
```


foo x < puts x in foo. In this case, x is the code list between the curly-braces. This is how you define callable code in Babel. The definition works by initializing the stack with 0, 1. On each iteration of the times loop, the function duplicates the top element. In the first iteration, this gives 0, 1, 1. Then it moves down the stack with the <- operator, giving 0, 1 again. It adds, giving 1. then it moves back up the stack, giving 1, 1. Then it swaps. On the next iteration this gives:


```txt
1, 1, 1 (dup)
1, 1, (<-)
2 (+)
2, 1 (->)
1, 2 (swap)
```


And so on. To test fib:


```babel
{19 iter - fib !} 20 times collect ! lsnum !
```


```txt
( 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 )
```



## BASIC

=
## Applesoft BASIC
=
Same code as [[#Commodore_BASIC|Commodore BASIC]]

Entering a value of N > 183, produces an error message:

```txt
?OVERFLOW ERROR IN 40
```


=
## BASIC256
=

```BASIC256

# Basic-256 ver 1.1.4
# iterative Fibonacci sequence
# Matches sequence A000045 in the OEIS, https://oeis.org/A000045/list

# Return the Nth Fibonacci number

input "N = ",f
limit = 500                        # set upper limit - can be changed, removed
f = int(f)
if f > limit then f = limit
a = 0 : b = 1 : c = 0 : n = 0      # initial values


while n < f
    print n + chr(9) + c   # chr(9) = tab
    a = b
    b = c
    c = a + b
    n += 1

end while

print " "
print n + chr(9) + c

```


=
## Commodore BASIC
=

```basic
10 INPUT "ENTER VALUE OF N"; N
20 N1 = 0 : N2 = 1
30 FOR K=1 TO N
40   SUM = N1+N2
50   N1 = N2
60   N2 = SUM
70 NEXT K
80 PRINT N1
```


=
## Integer BASIC
=
Only works with quite small values of <math>n</math>.

```basic
 10 INPUT N
 20 A=0
 30 B=1
 40 FOR I=2 TO N
 50 C=B
 60 B=A+B
 70 A=C
 80 NEXT I
 90 PRINT B
100 END
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Fibonac.bas"
110 FOR I=0 TO 20
120   PRINT "F";I,FIB(I)
130 NEXT
140 DEF FIB(N)
150   NUMERIC I
160   LET A=0:LET B=1
170   FOR I=1 TO N
180     LET T=A+B:LET A=B:LET B=T
190   NEXT
200   LET FIB=A
210 END DEF
```


=
## QBasic
=
### =Iterative=


```qbasic
FUNCTION itFib (n)
    n1 = 0
    n2 = 1
    FOR k = 1 TO ABS(n)
        sum = n1 + n2
        n1 = n2
        n2 = sum
    NEXT k
    IF n < 0 THEN
        itFib = n1 * ((-1) ^ ((-n) + 1))
    ELSE
        itFib = n1
    END IF
END FUNCTION
```


Next version calculates each value once, as needed, and stores the results in an array for later retreival (due to the use of <code>REDIM PRESERVE</code>, it requires [[QuickBASIC]] 4.5 or newer):


```qbasic
DECLARE FUNCTION fibonacci& (n AS INTEGER)

REDIM SHARED fibNum(1) AS LONG

fibNum(1) = 1

'*****sample inputs*****
PRINT fibonacci(0)      'no calculation needed
PRINT fibonacci(13)     'figure F(2)..F(13)
PRINT fibonacci(-42)    'figure F(14)..F(42)
PRINT fibonacci(47)     'error: too big
'*****sample inputs*****

FUNCTION fibonacci& (n AS INTEGER)
    DIM a AS INTEGER
    a = ABS(n)
    SELECT CASE a
        CASE 0 TO 46
            SHARED fibNum() AS LONG
            DIM u AS INTEGER, L0 AS INTEGER
            u = UBOUND(fibNum)
            IF a > u THEN
                REDIM PRESERVE fibNum(a) AS LONG
                FOR L0 = u + 1 TO a
                    fibNum(L0) = fibNum(L0 - 1) + fibNum(L0 - 2)
                NEXT
            END IF
            IF n < 0 THEN
                fibonacci = fibNum(a) * ((-1) ^ (a + 1))
            ELSE
                fibonacci = fibNum(n)
            END IF
        CASE ELSE
            'limited to signed 32-bit int (LONG)
            'F(47)=&hB11924E1
            ERROR 6 'overflow
    END SELECT
END FUNCTION
```


{{out}} (unhandled error in final input prevents output):

```txt

 0
 233
-267914296

```



### =Recursive=

This example can't handle n < 0.


```qbasic
FUNCTION recFib (n)
    IF (n < 2) THEN
	recFib = n
    ELSE
	recFib = recFib(n - 1) + recFib(n - 2)
    END IF
END FUNCTION
```


====Array (Table) Lookup====

This uses a pre-generated list, requiring much less run-time processor usage. (Since the sequence never changes, this is probably the best way to do this in "the real world". The same applies to other sequences like prime numbers, and numbers like pi and e.)


```qbasic
DATA -1836311903,1134903170,-701408733,433494437,-267914296,165580141,-102334155
DATA 63245986,-39088169,24157817,-14930352,9227465,-5702887,3524578,-2178309
DATA 1346269,-832040,514229,-317811,196418,-121393,75025,-46368,28657,-17711
DATA 10946,-6765,4181,-2584,1597,-987,610,-377,233,-144,89,-55,34,-21,13,-8,5,-3
DATA 2,-1,1,0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765
DATA 10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269
DATA 2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986
DATA 102334155,165580141,267914296,433494437,701408733,1134903170,1836311903

DIM fibNum(-46 TO 46) AS LONG

FOR n = -46 TO 46
    READ fibNum(n)
NEXT

'*****sample inputs*****
FOR n = -46 TO 46
    PRINT fibNum(n),
NEXT
PRINT
'*****sample inputs*****
```


=
## Sinclair ZX81 BASIC
=

### =Analytic=


```basic
 10 INPUT N
 20 PRINT INT (0.5+(((SQR 5+1)/2)**N)/SQR 5)
```



### =Iterative=


```basic
 10 INPUT N
 20 LET A=0
 30 LET B=1
 40 FOR I=2 TO N
 50 LET C=B
 60 LET B=A+B
 70 LET A=C
 80 NEXT I
 90 PRINT B
```



### =Tail recursive=


```basic
 10 INPUT N
 20 LET A=0
 30 LET B=1
 40 GOSUB 70
 50 PRINT B
 60 STOP
 70 IF N=1 THEN RETURN
 80 LET C=B
 90 LET B=A+B
100 LET A=C
110 LET N=N-1
120 GOSUB 70
130 RETURN
```



## Batch File

Recursive version

```dos
::fibo.cmd
@echo off
if "%1" equ "" goto :eof
call :fib %1
echo %errorlevel%
goto :eof

:fib
setlocal enabledelayedexpansion
if %1 geq 2 goto :ge2
exit /b %1

:ge2
set /a r1 = %1 - 1
set /a r2 = %1 - 2
call :fib !r1!
set r1=%errorlevel%
call :fib !r2!
set r2=%errorlevel%
set /a r0 = r1 + r2
exit /b !r0!
```


```txt

>for /L %i in (1,5,20) do fibo.cmd %i

>fibo.cmd 1
1

>fibo.cmd 6
8

>fibo.cmd 11
89

>fibo.cmd 16
987
```




## Battlestar


<!--- works with C syntax highlighting --->

```c

// Fibonacci sequence, recursive version
fun fibb
    loop
        a = funparam[0]
        break (a < 2)

        a--

        // Save "a" while calling fibb
        a -> stack

        // Set the parameter and call fibb
        funparam[0] = a
        call fibb

        // Handle the return value and restore "a"
        b = funparam[0]
        stack -> a

        // Save "b" while calling fibb again
        b -> stack

        a--

        // Set the parameter and call fibb
        funparam[0] = a
        call fibb

        // Handle the return value and restore "b"
        c = funparam[0]
        stack -> b

        // Sum the results
        b += c
        a = b

        funparam[0] = a

        break
    end
end

// vim: set syntax=c ts=4 sw=4 et:

```



## BBC BASIC


```bbcbasic
      PRINT FNfibonacci_r(1),  FNfibonacci_i(1)
      PRINT FNfibonacci_r(13), FNfibonacci_i(13)
      PRINT FNfibonacci_r(26), FNfibonacci_i(26)
      END

      DEF FNfibonacci_r(N)
      IF N < 2 THEN = N
      = FNfibonacci_r(N-1) + FNfibonacci_r(N-2)

      DEF FNfibonacci_i(N)
      LOCAL F, I, P, T
      IF N < 2 THEN = N
      P = 1
      FOR I = 1 TO N
        T = F
        F += P
        P = T
      NEXT
      = F

```

```txt
         1         1
       233       233
    121393    121393
```



## bc


###  iterative


```bc
#! /usr/bin/bc -q

define fib(x) {
    if (x <= 0) return 0;
    if (x == 1) return 1;

    a = 0;
    b = 1;
    for (i = 1; i < x; i++) {
        c = a+b; a = b; b = c;
    }
    return c;
}
fib(1000)
quit
```




## beeswax



```beeswax>                        #
'#{;
_`Enter n: `TN`Fib(`{`)=`X~P~K#{;
                         #>~P~L#MM@>+@'q@{;
                                    b~@M<
```


Example output:

Notice the UInt64 wrap-around at <code>Fib(94)</code>!


```julia>julia
 beeswax("n-th Fibonacci number.bswx")
Enter n: i0

Fib(0)=0
Program finished!

julia> beeswax("n-th Fibonacci number.bswx")
Enter n: i10

Fib(10)=55
Program finished!

julia> beeswax("n-th Fibonacci number.bswx")
Enter n: i92

Fib(92)=7540113804746346429
Program finished!

julia> beeswax("n-th Fibonacci number.bswx")
Enter n: i93

Fib(93)=12200160415121876738
Program finished!

julia> beeswax("n-th Fibonacci number.bswx")
Enter n: i94

Fib(94)=1293530146158671551
Program finished!
```



## Befunge


```befunge>00:.1:.
:"@"8**++\1+:67+`#@_v
       ^ .:\/*8"@"\%*8"@":\ <
```


=={{header|Brainfuck}}==
The first cell contains ''n'' (10), the second cell will contain ''fib(n)'' (55), and the third cell will contain ''fib(n-1)'' (34).

```bf
++++++++++
>>+<<[->[->+>+<<]>[-<+>]>[-<+>]<<<]
```


The following generates n fibonacci numbers and prints them, though not in ascii.
It does have a limit due to the cells usually being 1 byte in size.

```bf
+++++ +++++	#0 set to n
>> +		Init #2 to 1
<<
[
	-	#Decrement counter in #0
	>>.	Notice: This doesn't print it in ascii
		To look at results you can pipe into a file and look with a hex editor

		Copying sequence to save #2 in #4 using #5 as restore space
	>>[-]	Move to #4 and clear
	>[-]	Clear #5
	<<<	#2
	[	Move loop
		- >> + > + <<<	Subtract #2 and add #4 and #5
	]
	>>>
	[	Restore loop
		- <<< + >>>	Subtract from #5 and add to #2
	]

	<<<<	Back to #1
		Non destructive add sequence using #3 as restore value
	[	Loop to add
		- > + > + <<	Subtract #1 and add to value #2 and restore space #3
	]
	>>
	[	Loop to restore #1 from #3
		- << + >>	Subtract from restore space #3 and add in #1
	]

	<< [-]	Clear #1
	>>>
	[	Loop to move #4 to #1
		- <<< + >>>	Subtract from #4 and add to #1
	]
	<<<<	Back to #0
]
```



## Bracmat


### Recursive


```bracmat
fib=.!arg:<2|fib$(!arg+-2)+fib$(!arg+-1)
```


  fib$30
  832040


### Iterative


```bracmat
(fib=
  last i this new
.   !arg:<2
  |   0:?last:?i
    & 1:?this
    &   whl
      ' ( !i+1:<!arg:?i
        & !last+!this:?new
        & !this:?last
        & !new:?this
        )
    & !this
)
```


  fib$777
  1081213530912648191985419587942084110095342850438593857649766278346130479286685742885693301250359913460718567974798268702550329302771992851392180275594318434818082


## Brat


### Recursive


```brat
fibonacci = { x |
        true? x < 2, x, { fibonacci(x - 1) + fibonacci(x - 2) }
}
```



### Tail Recursive


```brat
fib_aux = { x, next, result |
        true? x == 0,
                result,
                { fib_aux x - 1, next + result, next }
}

fibonacci = { x |
  fib_aux x, 1, 0
}
```



### Memoization


```brat
cache = hash.new

fibonacci = { x |
  true? cache.key?(x)
    { cache[x] }
    {true? x < 2, x, { cache[x] = fibonacci(x - 1) + fibonacci(x - 2) }}
}
```



## Burlesque



```burlesque

{0 1}{^^++[+[-^^-]\/}30.*\[e!vv

```



```burlesque

0 1{{.+}c!}{1000.<}w!

```



## C


### Recursive


```c
long long fibb(long long a, long long b, int n) {
    return (--n>0)?(fibb(b, a+b, n)):(a);
}
```



### Iterative


```c
long long int fibb(int n) {
	int fnow = 0, fnext = 1, tempf;
	while(--n>0){
		tempf = fnow + fnext;
		fnow = fnext;
		fnext = tempf;
		}
		return fnext;
}
```



### Analytic


```c
#include <tgmath.h>
#define PHI ((1 + sqrt(5))/2)

long long unsigned fib(unsigned n) {
    return floor( (pow(PHI, n) - pow(1 - PHI, n))/sqrt(5) );
}
```



### Generative

```c
#include <stdio.h>
typedef enum{false=0, true=!0} bool;
typedef void iterator;

#include <setjmp.h>
/* declare label otherwise it is not visible in sub-scope */
#define LABEL(label) jmp_buf label; if(setjmp(label))goto label;
#define GOTO(label) longjmp(label, true)

/* the following line is the only time I have ever required "auto" */
#define FOR(i, iterator) { auto bool lambda(i); yield_init = (void *)&lambda; iterator; bool lambda(i)
#define DO {
#define     YIELD(x) if(!yield(x))return
#define     BREAK    return false
#define     CONTINUE return true
#define OD CONTINUE; } }

static volatile void *yield_init; /* not thread safe */
#define YIELDS(type) bool (*yield)(type) = yield_init

iterator fibonacci(int stop){
    YIELDS(int);
    int f[] = {0, 1};
    int i;
    for(i=0; i<stop; i++){
        YIELD(f[i%2]);
        f[i%2]=f[0]+f[1];
    }
}

main(){
  printf("fibonacci: ");
  FOR(int i, fibonacci(16)) DO
    printf("%d, ",i);
  OD;
  printf("...\n");
}
```

```txt

fibonacci: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...

```



### Fast method for a single large value


```cpp
#include <iostream>
#include <stdio.h>
#include <gmp.h>

typedef struct node node;
struct node {
	int n;
	mpz_t v;
	node *next;
};

#define CSIZE 37
node *cache[CSIZE];

// very primitive linked hash table
node * find_cache(int n)
{
	int idx = n % CSIZE;
	node *p;

	for (p = cache[idx]; p && p->n != n; p = p->next);
	if (p) return p;

	p = malloc(sizeof(node));
	p->next = cache[idx];
	cache[idx] = p;

	if (n < 2) {
		p->n = n;
		mpz_init_set_ui(p->v, 1);
	} else {
		p->n = -1; // -1: value not computed yet
		mpz_init(p->v);
	}
	return p;
}

mpz_t tmp1, tmp2;
mpz_t *fib(int n)
{
	int x;
	node *p = find_cache(n);

	if (p->n < 0) {
		p->n = n;
		x = n / 2;

		mpz_mul(tmp1, *fib(x-1), *fib(n - x - 1));
		mpz_mul(tmp2, *fib(x), *fib(n - x));
		mpz_add(p->v, tmp1, tmp2);
	}
	return &p->v;
}

int main(int argc, char **argv)
{
	int i, n;
	if (argc < 2) return 1;

	mpz_init(tmp1);
	mpz_init(tmp2);

	for (i = 1; i < argc; i++) {
		n = atoi(argv[i]);
		if (n < 0) {
			printf("bad input: %s\n", argv[i]);
			continue;
		}

		// about 75% of time is spent in printing
		gmp_printf("%Zd\n", *fib(n));
	}
	return 0;
}
```

```txt

% ./a.out 0 1 2 3 4 5
1
1
2
3
5
8
% ./a.out 10000000 | wc -c    # count length of output, including the newline
1919488

```



## C++

Using unsigned int, this version only works up to 48 before fib overflows.

```cpp
#include <iostream>

int main()
{
        unsigned int a = 1, b = 1;
        unsigned int target = 48;
        for(unsigned int n = 3; n <= target; ++n)
        {
                unsigned int fib = a + b;
                std::cout << "F("<< n << ") = " << fib << std::endl;
                a = b;
                b = fib;
        }

        return 0;
}
```



This version does not have an upper bound.


```cpp
#include <iostream>
#include <gmpxx.h>

int main()
{
        mpz_class a = mpz_class(1), b = mpz_class(1);
        mpz_class target = mpz_class(100);
        for(mpz_class n = mpz_class(3); n <= target; ++n)
        {
                mpz_class fib = b + a;
                if ( fib < b )
                {
                        std::cout << "Overflow at " << n << std::endl;
                        break;
                }
                std::cout << "F("<< n << ") = " << fib << std::endl;
                a = b;
                b = fib;
        }
        return 0;
}
```


Version using transform:

```cpp
#include <algorithm>
#include <vector>
#include <functional>
#include <iostream>

unsigned int fibonacci(unsigned int n) {
  if (n == 0) return 0;
  std::vector<int> v(n+1);
  v[1] = 1;
  transform(v.begin(), v.end()-2, v.begin()+1, v.begin()+2, std::plus<int>());
  // "v" now contains the Fibonacci sequence from 0 up
  return v[n];
}
```


Far-fetched version using adjacent_difference:

```cpp
#include <numeric>
#include <vector>
#include <functional>
#include <iostream>

unsigned int fibonacci(unsigned int n) {
  if (n == 0) return 0;
  std::vector<int> v(n, 1);
  adjacent_difference(v.begin(), v.end()-1, v.begin()+1, std::plus<int>());
  // "array" now contains the Fibonacci sequence from 1 up
  return v[n-1];
}

```


Version which computes at compile time with metaprogramming:

```cpp
#include <iostream>

template <int n> struct fibo
{
    enum {value=fibo<n-1>::value+fibo<n-2>::value};
};

template <> struct fibo<0>
{
    enum {value=0};
};

template <> struct fibo<1>
{
    enum {value=1};
};


int main(int argc, char const *argv[])
{
    std::cout<<fibo<12>::value<<std::endl;
    std::cout<<fibo<46>::value<<std::endl;
    return 0;
}
```


The following version is based on fast exponentiation:

```cpp
#include <iostream>

inline void fibmul(int* f, int* g)
{
  int tmp = f[0]*g[0] + f[1]*g[1];
  f[1] = f[0]*g[1] + f[1]*(g[0] + g[1]);
  f[0] = tmp;
}

int fibonacci(int n)
{
  int f[] = { 1, 0 };
  int g[] = { 0, 1 };
  while (n > 0)
  {
    if (n & 1) // n odd
    {
      fibmul(f, g);
      --n;
    }
    else
    {
      fibmul(g, g);
      n >>= 1;
    }
  }
  return f[1];
}

int main()
{
  for (int i = 0; i < 20; ++i)
    std::cout << fibonacci(i) << " ";
  std::cout << std::endl;
}
```

 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181

### Using Zeckendorf Numbers

The nth fibonacci is represented as Zeckendorf 1 followed by n-1 zeroes. [[Zeckendorf number representation#Using a C++11 User Defined Literal|Here]] I define a class N which defines the operations increment ++() and comparison <=(other N) for Zeckendorf Numbers.

```cpp

// Use Zeckendorf numbers to display Fibonacci sequence.
// Nigel Galloway October 23rd., 2012
int main(void) {
  char NG[22] = {'1',0};
  int x = -1;
  N G;
  for (int fibs = 1; fibs <= 20; fibs++) {
   for (;G <= N(NG); ++G) x++;
   NG[fibs] = '0';
   NG[fibs+1] = 0;
   std::cout << x << " ";
  }
  std::cout << std::endl;
  return 0;
}

```

```txt

1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946

```



### Using Standard Template Library

Possibly less "Far-fetched version".

```cpp

// Use Standard Template Library to display Fibonacci sequence.
// Nigel Galloway March 30th., 2013
#include <algorithm>
#include <iostream>
#include <iterator>
int main()
{
   int x = 1, y = 1;
   generate_n(std::ostream_iterator<int>(std::cout, " "), 21, [&]{int n=x; x=y; y+=n; return n;});
   return 0;
}

```

1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946

## C#


###  Recursive


```c#

public static ulong Fib(uint n) {
    return (n < 2)? n : Fib(n - 1) + Fib(n - 2);
}

```


=== Tail-Recursive ===

```c#

public static ulong Fib(uint n) {
    return Fib(0, 1, n);
}

private static ulong Fib(ulong a, ulong b, uint n) {
    return (n < 1)? a :(n == 1)?  b : Fib(b, a + b, n - 1);
}

```



###  Iterative


```c#

public static ulong Fib(uint x) {
    if (x == 0) return 0;

    ulong prev = 0;
    ulong next = 1;
    for (int i = 1; i < x; i++)
    {
        ulong sum = prev + next;
        prev = next;
        next = sum;
    }
    return next;
}

```


=== Eager-Generative ===

```c#

public static IEnumerable<long> Fibs(uint x) {
    IList<ulong> fibs = new List<ulong>();

    ulong prev = -1;
    ulong next = 1;
    for (int i = 0; i < x; i++)
    {
     long sum = prev + next;
        prev = next;
        next = sum;
        fibs.Add(sum);
    }
    return fibs;
}

```


=== Lazy-Generative ===

```c#

public static IEnumerable<ulong> Fibs(uint x) {
    ulong prev = -1;
    ulong next = 1;
    for (uint i = 0; i < x; i++) {
        ulong sum = prev + next;
        prev = next;
        next = sum;
        yield return sum;
    }
}

```



###  Analytic

Only works to the 92<sup>th</sup> fibonacci number.

```c#

private static double Phi = ((1d + Math.Sqrt(5d))/2d);
private static double D = 1d/Math.Sqrt(5d);

ulong Fib(uint n) {
    if(n > 92) throw new ArgumentOutOfRangeException("n", n, "Needs to be smaller than 93.");
    return (ulong)((Phi^n) - (1d - Phi)^n))*D);
}

```



###  Matrix

Algorithm is based on
:<math>\begin{pmatrix}1&1\\1&0\end{pmatrix}^n = \begin{pmatrix}F(n+1)&F(n)\\F(n)&F(n-1)\end{pmatrix}</math>.

Needs <code>System.Windows.Media.Matrix</code> or similar Matrix class.
Calculates in <math>O(n)</math>.

```c#

public static ulong Fib(uint n) {
    var M = new Matrix(1,0,0,1);
    var N = new Matrix(1,1,1,0);
    for (uint i = 1; i < n; i++) M *= N;
    return (ulong)M[0][0];
}

```

Needs <code>System.Windows.Media.Matrix</code> or similar Matrix class.
Calculates in <math>O(\log{n})</math>.

```c#

private static Matrix M;
private static readonly Matrix N = new Matrix(1,1,1,0);

public static ulong Fib(uint n) {
    M = new Matrix(1,0,0,1);
    MatrixPow(n-1);
    return (ulong)M[0][0];
}

private static void MatrixPow(double n){
    if (n > 1) {
        MatrixPow(n/2);
        M *= M;
    }
    if (n % 2 == 0) M *= N;
}

```


=== Array (Table) Lookup ===

```c#

private static int[] fibs = new int[]{ -1836311903, 1134903170,
  -701408733, 433494437, -267914296, 165580141, -102334155,
  63245986, -39088169, 24157817, -14930352, 9227465, -5702887,
  3524578, -2178309, 1346269, -832040, 514229, -317811, 196418,
  -121393, 75025, -46368, 28657, -17711, 10946, -6765, 4181,
  -2584, 1597, -987, 610, -377, 233, -144, 89, -55, 34, -21, 13,
  -8, 5, -3, 2, -1, 1, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
  144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711,
  28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040,
  1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817,
  39088169, 63245986, 102334155, 165580141, 267914296, 433494437,
  701408733, 1134903170, 1836311903};

public static int Fib(int n) {
    if(n < -46 || n > 46) throw new ArgumentOutOfRangeException("n", n, "Has to be between -46 and 47.")
    return fibs[n+46];
}

```


### Arbitrary Precision

This large step recurrence routine can calculate the two millionth Fibonacci number in under 1 / 5 second at tio.run.  This routine can generate the fifty millionth Fibonacci number in under 30 seconds at tio.run.  The unused conventional iterative method times out at two million on tio.run, you can only go to around 1,290,000 or so to keep the calculation time (plus string conversion time) under the 60 second timeout limit there.  When using this large step recurrence method, it takes around 5 seconds to convert the two millionth Fibonacci number (417975 digits) into a string (so that one may count those digits).


```c#
using System;
using System.Collections.Generic;
using System.Numerics;

static class QuikFib
{
    // A sparse array of values calculated along the way
    private static SortedList<int, BigInteger> sl = new SortedList<int, BigInteger>();

    // Square a BigInteger
    public static BigInteger sqr(BigInteger n)
    {
        return n * n;
    }

    // Helper routine for Fsl(). It adds an entry to the sorted list when necessary
    public static void IfNec(int n)
    {
        if (!sl.ContainsKey(n)) sl.Add(n, Fsl(n));
    }

    // This routine is semi-recursive, but doesn't need to evaluate every number up to n.
    // Algorithm from here: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html#section3
    public static BigInteger Fsl(int n)
    {
        if (n < 2) return n;
        int n2 = n >> 1, pm = n2 + ((n & 1) << 1) - 1; IfNec(n2); IfNec(pm);
        return n2 > pm ? (2 * sl[pm] + sl[n2]) * sl[n2] : sqr(sl[n2]) + sqr(sl[pm]);
    }

    // Conventional iteration method (not used here)
    public static BigInteger Fm(BigInteger n)
    {
        if (n < 2) return n; BigInteger cur = 0, pre = 1;
        for (int i = 0; i <= n - 1; i++) { BigInteger sum = cur + pre; pre = cur; cur = sum; }
        return cur;
    }

    public static void Main()
    {
        int num = 2_000_000;
        DateTime st = DateTime.Now;
        BigInteger v = Fsl(num);
        Console.WriteLine("{0:n3} ms to calculate the {1:n0}th Fibonacci number,",
                          (DateTime.Now - st).TotalMilliseconds, num);
        st = DateTime.Now;
        string vs = v.ToString();
        Console.WriteLine("{0:n3} seconds to convert to a string.", (DateTime.Now - st).TotalSeconds);
        Console.WriteLine("number of digits is {0}", vs.Length);
        if (vs.Length < 10000)
        {
            st = DateTime.Now;
            Console.WriteLine(vs);
            Console.WriteLine("{0:n3} ms to write it to the console.", (DateTime.Now - st).TotalMilliseconds);
        }
        else
            Console.WriteLine("partial: {0}...{1}", vs.Substring(1, 35), vs.Substring(vs.Length - 35));
    }
}
```

```txt
179.978 ms to calculate the 2,000,000th Fibonacci number,
4.728 seconds to convert to a string.
number of digits is 417975
partial: 53129491750764154305166065450382516...91799493108960825129188777803453125

```



## Cat


```cat
define fib {
  dup 1 <=
    []
    [dup 1 - fib swap 2 - fib +]
  if
}
```



## Chapel


```chapel
iter fib() {
        var a = 0, b = 1;

        while true {
                yield a;
                (a, b) = (b, b + a);
        }
}
```



## Chef


```chef
Stir-Fried Fibonacci Sequence.

An unobfuscated iterative implementation.
It prints the first N + 1 Fibonacci numbers,
where N is taken from standard input.

Ingredients.
0 g last
1 g this
0 g new
0 g input

Method.
Take input from refrigerator.
Put this into 4th mixing bowl.
Loop the input.
Clean the 3rd mixing bowl.
Put last into 3rd mixing bowl.
Add this into 3rd mixing bowl.
Fold new into 3rd mixing bowl.
Clean the 1st mixing bowl.
Put this into 1st mixing bowl.
Fold last into 1st mixing bowl.
Clean the 2nd mixing bowl.
Put new into 2nd mixing bowl.
Fold this into 2nd mixing bowl.
Put new into 4th mixing bowl.
Endloop input until looped.
Pour contents of the 4th mixing bowl into baking dish.

Serves 1.
```



## CMake

Iteration uses a while() loop. Memoization uses global properties.


```cmake
set_property(GLOBAL PROPERTY fibonacci_0 0)
set_property(GLOBAL PROPERTY fibonacci_1 1)
set_property(GLOBAL PROPERTY fibonacci_next 2)

# var = nth number in Fibonacci sequence.
function(fibonacci var n)
  # If the sequence is too short, compute more Fibonacci numbers.
  get_property(next GLOBAL PROPERTY fibonacci_next)
  if(NOT next GREATER ${n})
    # a, b = last 2 Fibonacci numbers
    math(EXPR i "${next} - 2")
    get_property(a GLOBAL PROPERTY fibonacci_${i})
    math(EXPR i "${next} - 1")
    get_property(b GLOBAL PROPERTY fibonacci_${i})

    while(NOT next GREATER ${n})
      math(EXPR i "${a} + ${b}")  # i = next Fibonacci number
      set_property(GLOBAL PROPERTY fibonacci_${next} ${i})
      set(a ${b})
      set(b ${i})
      math(EXPR next "${next} + 1")
    endwhile()
    set_property(GLOBAL PROPERTY fibonacci_next ${next})
  endif()

  get_property(answer GLOBAL PROPERTY fibonacci_${n})
  set(${var} ${answer} PARENT_SCOPE)
endfunction(fibonacci)
```



```cmake
# Test program: print 0th to 9th and 25th to 30th Fibonacci numbers.
set(s "")
foreach(i RANGE 0 9)
  fibonacci(f ${i})
  set(s "${s} ${f}")
endforeach(i)
set(s "${s} ... ")
foreach(i RANGE 25 30)
  fibonacci(f ${i})
  set(s "${s} ${f}")
endforeach(i)
message(${s})
```



```txt
 0 1 1 2 3 5 8 13 21 34 ... 75025 121393 196418 317811 514229 832040
```



## Clio


Clio is pure and functions are lazy and memoized by default


```clio
fn fib n:
  if n < 2: n
  else: (n - 1 -> fib) + (n - 2 -> fib)

[0:100] -> * fib -> * print
```



## Clojure



### Lazy Sequence

This is implemented idiomatically as an infinitely long, lazy sequence of all Fibonacci numbers:

```Clojure
(defn fibs []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))
```

Thus to get the nth one:

```Clojure
(nth (fibs) 5)
```

So long as one does not hold onto the head of the sequence, this is unconstrained by length.

The one-line implementation may look confusing at first, but on pulling it apart it actually solves the problem more "directly" than a more explicit looping construct.

```Clojure
(defn fibs []
  (map first ;; throw away the "metadata" (see below) to view just the fib numbers
       (iterate ;; create an infinite sequence of [prev, curr] pairs
         (fn [[a b]] ;; to produce the next pair, call this function on the current pair
           [b (+ a b)]) ;; new prev is old curr, new curr is sum of both previous numbers
         [0 1]))) ;; recursive base case: prev 0, curr 1
```


A more elegant solution is inspired by the Haskell implementation of an infinite list of Fibonacci numbers:

```Clojure
(def fib (lazy-cat [0 1] (map + fib (rest fib))))
```

Then, to see the first ten,

```Clojure>user
 (take 10 fib)
(0 1 1 2 3 5 8 13 21 34)
```



### Iterative


Here's a simple interative process (using a recursive function) that carries state along with it (as args) until it reaches a solution:

```Clojure
;; max is which fib number you'd like computed (0th, 1st, 2nd, etc.)
;; n is which fib number you're on for this call (0th, 1st, 2nd, etc.)
;; j is the nth fib number (ex. when n = 5, j = 5)
;; i is the nth - 1 fib number
(defn- fib-iter
  [max n i j]
  (if (= n max)
    j
    (recur max
           (inc n)
           j
           (+ i j))))

(defn fib
  [max]
  (if (< max 2)
    max
    (fib-iter max 1 0N 1N)))
```

"defn-" means that the function is private (for use only inside this library). The "N" suffixes on integers tell Clojure to use arbitrary precision ints for those.

===Doubling Algorithm (Fast)===
Based upon the doubling algorithm which computes in O(log (n)) time as described here https://www.nayuki.io/page/fast-fibonacci-algorithms
Implementation credit: https://stackoverflow.com/questions/27466311/how-to-implement-this-fast-doubling-fibonacci-algorithm-in-clojure/27466408#27466408

```clojure

(defn fib [n]
  (letfn [(fib* [n]
            (if (zero? n)
              [0 1]
              (let [[a b] (fib* (quot n 2))
                    c (*' a (-' (*' 2 b) a))
                    d (+' (*' b b) (*' a a))]
                (if (even? n)
                  [c d]
                  [d (+' c d)]))))]
    (first (fib* n))))

```



### Recursive


A naive slow recursive solution:


```Clojure
(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1))
       (fib (- n 2)))))
```


This can be improved to an O(n) solution, like the iterative solution, by memoizing the function so that numbers that have been computed are cached. Like a lazy sequence, this also has the advantage that subsequent calls to the function use previously cached results rather than recalculating.


```Clojure
(def fib
  (memoize
    (fn [n]
      (case n
        0 0
        1 1
        (+ (fib (- n 1))
           (fib (- n 2)))))))
```



###  Using core.async


```Clojure
(ns fib.core)
(require '[clojure.core.async
           :refer [<! >! >!! <!! timeout chan alt! go]])

(defn fib [c]
  (loop [a 0 b 1]
    (>!! c a)
    (recur b (+ a b))))


(defn -main []
  (let [c (chan)]
    (go (fib c))
    (dorun
      (for [i (range 10)]
        (println (<!! c))))))

```



## COBOL


### Iterative


```cobol
Program-ID. Fibonacci-Sequence.
Data Division.
Working-Storage Section.
  01  FIBONACCI-PROCESSING.
    05  FIBONACCI-NUMBER  PIC 9(36)   VALUE 0.
    05  FIB-ONE           PIC 9(36)   VALUE 0.
    05  FIB-TWO           PIC 9(36)   VALUE 1.
  01  DESIRED-COUNT       PIC 9(4).
  01  FORMATTING.
    05  INTERM-RESULT     PIC Z(35)9.
    05  FORMATTED-RESULT  PIC X(36).
    05  FORMATTED-SPACE   PIC x(35).
Procedure Division.
  000-START-PROGRAM.
    Display "What place of the Fibonacci Sequence would you like (<173)? " with no advancing.
    Accept DESIRED-COUNT.
    If DESIRED-COUNT is less than 1
      Stop run.
    If DESIRED-COUNT is less than 2
      Move FIBONACCI-NUMBER to INTERM-RESULT
      Move INTERM-RESULT to FORMATTED-RESULT
      Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT
      Display FORMATTED-RESULT
      Stop run.
    Subtract 1 from DESIRED-COUNT.
    Move FIBONACCI-NUMBER to INTERM-RESULT.
    Move INTERM-RESULT to FORMATTED-RESULT.
    Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT.
    Display FORMATTED-RESULT.
    Perform 100-COMPUTE-FIBONACCI until DESIRED-COUNT = zero.
    Stop run.
  100-COMPUTE-FIBONACCI.
    Compute FIBONACCI-NUMBER = FIB-ONE + FIB-TWO.
    Move FIB-TWO to FIB-ONE.
    Move FIBONACCI-NUMBER to FIB-TWO.
    Subtract 1 from DESIRED-COUNT.
    Move FIBONACCI-NUMBER to INTERM-RESULT.
    Move INTERM-RESULT to FORMATTED-RESULT.
    Unstring FORMATTED-RESULT delimited by all spaces into FORMATTED-SPACE,FORMATTED-RESULT.
    Display FORMATTED-RESULT.
```



### Recursive

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. fibonacci-main.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  num                                 PIC 9(6) COMP.
01  fib-num                             PIC 9(6) COMP.

PROCEDURE DIVISION.
    ACCEPT num
    CALL "fibonacci" USING CONTENT num RETURNING fib-num
    DISPLAY fib-num
    .
END PROGRAM fibonacci-main.

IDENTIFICATION DIVISION.
PROGRAM-ID. fibonacci RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  1-before                            PIC 9(6) COMP.
01  2-before                            PIC 9(6) COMP.

LINKAGE SECTION.
01  num                                 PIC 9(6) COMP.

01  fib-num                             PIC 9(6) COMP BASED.

PROCEDURE DIVISION USING num RETURNING fib-num.
    ALLOCATE fib-num
    EVALUATE num
        WHEN 0
            MOVE 0 TO fib-num
        WHEN 1
            MOVE 1 TO fib-num
        WHEN OTHER
            SUBTRACT 1 FROM num
            CALL "fibonacci" USING CONTENT num RETURNING 1-before
            SUBTRACT 1 FROM num
            CALL "fibonacci" USING CONTENT num RETURNING 2-before
            ADD 1-before TO 2-before GIVING fib-num
    END-EVALUATE
    .
END PROGRAM fibonacci.
```



## CoffeeScript


### Analytic


```coffeescript
fib_ana = (n) ->
    sqrt = Math.sqrt
    phi = ((1 + sqrt(5))/2)
    Math.round((Math.pow(phi, n)/sqrt(5)))
```



### Iterative


```coffeescript
fib_iter = (n) ->
    return n if n < 2
    [prev, curr] = [0, 1]
    [prev, curr] = [curr, curr + prev] for i in [1..n]
    curr
```



### Recursive


```coffeescript
fib_rec = (n) ->
  if n < 2 then n else fib_rec(n-1) + fib_rec(n-2)
```



## Comefrom0x10


Recursion is is not possible in Comefrom0x10.


### Iterative


```cf0x10
stop = 6
a = 1
i = 1  # start
a      # print result

fib
  comefrom if i is 1  # start
  b = 1
  comefrom fib        # start of loop
  i = i + 1
  next_b = a + b
  a = b
  b = next_b

  comefrom fib if i > stop
```



## Common Lisp

Note that Common Lisp uses bignums, so this will never overflow.

### Iterative


```lisp
(defun fibonacci-iterative (n &aux (f0 0) (f1 1))
  (case n
    (0 f0)
    (1 f1)
    (t (loop for n from 2 to n
             for a = f0 then b and b = f1 then result
             for result = (+ a b)
             finally (return result)))))
```


Simpler one:

```lisp
(defun fibonacci (n)
  (let ((a 0) (b 1) (c n))
    (loop for i from 2 to n do
	 (setq c (+ a b)
	       a b
	       b c))
    c))
```


Not a function, just printing out the entire (for some definition of "entire") sequence with a <code>for var = </code> loop:
```lisp
(loop for x = 0 then y and y = 1 then (+ x y) do (print x))
```



### Recursive


```lisp
(defun fibonacci-recursive (n)
  (if (< n 2)
      n
     (+ (fibonacci-recursive (- n 2)) (fibonacci-recursive (- n 1)))))
```




```lisp
(defun fibonacci-tail-recursive ( n &optional (a 0) (b 1))
  (if (= n 0)
      a
      (fibonacci-tail-recursive (- n 1) b (+ a b))))
```


Tail recursive and squaring:

```lisp
(defun fib (n &optional (a 1) (b 0) (p 0) (q 1))
    (if (= n 1) (+ (* b p) (* a q))
     (fib (ash n -1)
          (if (evenp n) a (+ (* b q) (* a (+ p q))))
          (if (evenp n) b (+ (* b p) (* a q)))
          (+ (* p p) (* q q))
          (+ (* q q) (* 2 p q))))) ;p is Fib(2^n-1), q is Fib(2^n).

(print (fib 100000))
```


###  Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Fibonacci sequence

(defun fibonacci (nr)
           (cond ((= nr 0) 1)
           ((= nr 1) 1)
           (t (+ (fibonacci (- nr 1))
           (fibonacci (- nr 2))))))
(format t "~a" "First 10 Fibonacci numbers")
(dotimes (n 10)
(if (< n 1) (terpri))
(if (< n 9) (format t "~a" " "))
(write(+ n 1)) (format t "~a" ": ")
(write (fibonacci n)) (terpri))

```

Output:

```txt

First 10 Fibonacci numbers
 1: 1
 2: 1
 3: 2
 4: 3
 5: 5
 6: 8
 7: 13
 8: 21
 9: 34
10: 55

```



###  Solution with methods and eql specializers


```lisp

(defmethod fib (n)
  (declare ((integer 0 *) n))
  (+ (fib (- n 1))
     (fib (- n 2))))

(defmethod fib ((n (eql 0))) 0)

(defmethod fib ((n (eql 1))) 1)

```



## Computer/zero Assembly

To find the <math>n</math>th Fibonacci number, set the initial value of <tt>count</tt> equal to <math>n</math>–2 and run the program. The machine will halt with the answer stored in the accumulator. Since Computer/zero's word length is only eight bits, the program will not work with values of <math>n</math> greater than 13.

```czasm
loop:   LDA  y      ; higher No.
        STA  temp
        ADD  x      ; lower No.
        STA  y
        LDA  temp
        STA  x

        LDA  count
        SUB  one
        BRZ  done

        STA  count
        JMP  loop

done:   LDA  y
        STP

one:         1
count:       8      ; n = 10
x:           1
y:           1
temp:        0
```



## D

Here are four versions of Fibonacci Number calculating functions. ''FibD'' has an argument limit of magnitude 84 due to floating point precision, the others have a limit of 92 due to overflow (long).The traditional recursive version is inefficient. It is optimized by supplying a static storage to store intermediate results. A Fibonacci Number generating function is added.
All functions have support for negative arguments.

```d
import std.stdio, std.conv, std.algorithm, std.math;

long sgn(alias unsignedFib)(int n) { // break sign manipulation apart
    immutable uint m = (n >= 0) ? n : -n;
    if (n < 0 && (n % 2 == 0))
        return -unsignedFib(m);
    else
        return unsignedFib(m);
}

long fibD(uint m) { // Direct Calculation, correct for abs(m) <= 84
    enum sqrt5r =  1.0L / sqrt(5.0L);         //  1 / sqrt(5)
    enum golden = (1.0L + sqrt(5.0L)) / 2.0L; // (1 + sqrt(5)) / 2
    return roundTo!long(pow(golden, m) * sqrt5r);
}

long fibI(in uint m) pure nothrow { // Iterative
    long thisFib = 0;
    long nextFib = 1;
    foreach (i; 0 .. m) {
        long tmp = nextFib;
        nextFib += thisFib;
        thisFib  = tmp;
    }
    return thisFib;
}

long fibR(uint m) { // Recursive
    return (m < 2) ? m : fibR(m - 1) + fibR(m - 2);
}

long fibM(uint m) { // memoized Recursive
    static long[] fib = [0, 1];
    while (m >= fib.length )
        fib ~= fibM(m - 2) + fibM(m - 1);
    return fib[m];
}

alias sgn!fibD sfibD;
alias sgn!fibI sfibI;
alias sgn!fibR sfibR;
alias sgn!fibM sfibM;

auto fibG(in int m) { // generator(?)
    immutable int sign = (m < 0) ? -1 : 1;
    long yield;

    return new class {
        final int opApply(int delegate(ref int, ref long) dg) {
            int idx = -sign; // prepare for pre-increment
            foreach (f; this)
                if (dg(idx += sign, f))
                    break;
            return 0;
        }

        final int opApply(int delegate(ref long) dg) {
            long f0, f1 = 1;
            foreach (p; 0 .. m * sign + 1) {
                if (sign == -1 && (p % 2 == 0))
                    yield = -f0;
                else
                    yield = f0;
                if (dg(yield)) break;
                auto temp = f1;
                f1 = f0 + f1;
                f0 = temp;
            }
            return 0;
        }
    };
}

void main(in string[] args) {
    int k = args.length > 1 ? to!int(args[1]) : 10;
    writefln("Fib(%3d) = ", k);
    writefln("D : %20d <- %20d + %20d",
             sfibD(k), sfibD(k - 1), sfibD(k - 2));
    writefln("I : %20d <- %20d + %20d",
             sfibI(k), sfibI(k - 1), sfibI(k - 2));
    if (abs(k) < 36 || args.length > 2)
        // set a limit for recursive version
        writefln("R : %20d <- %20d + %20d",
                 sfibR(k), sfibM(k - 1), sfibM(k - 2));
    writefln("O : %20d <- %20d + %20d",
             sfibM(k), sfibM(k - 1), sfibM(k - 2));
    foreach (i, f; fibG(-9))
        writef("%d:%d | ", i, f);
}
```

{{out}} for n = 85:

```txt
Fib( 85) =
D :   259695496911122586 <-   160500643816367088 +    99194853094755497
I :   259695496911122585 <-   160500643816367088 +    99194853094755497
O :   259695496911122585 <-   160500643816367088 +    99194853094755497
0:0 | -1:1 | -2:-1 | -3:2 | -4:-3 | -5:5 | -6:-8 | -7:13 | -8:-21 | -9:34 |
```


### Matrix Exponentiation Version


```d
import std.bigint;

T fibonacciMatrix(T=BigInt)(size_t n) {
    int[size_t.sizeof * 8] binDigits;
    size_t nBinDigits;
    while (n > 0) {
        binDigits[nBinDigits] = n % 2;
        n /= 2;
        nBinDigits++;
    }

    T x=1, y, z=1;
    foreach_reverse (b; binDigits[0 .. nBinDigits]) {
        if (b) {
            x = (x + z) * y;
            y = y ^^ 2 + z ^^ 2;
        } else {
            auto x_old = x;
            x = x ^^ 2 + y ^^ 2;
            y = (x_old + z) * y;
        }
        z = x + y;
    }

    return y;
}

void main() {
    10_000_000.fibonacciMatrix;
}
```



### Faster Version

For N = 10_000_000 this is about twice faster (run-time about 2.20 seconds) than the matrix exponentiation version.

```d
import std.bigint, std.math;

// Algorithm from: Takahashi, Daisuke,
// "A fast algorithm for computing large Fibonacci numbers".
// Information Processing Letters 75.6 (30 November 2000): 243-246.
// Implementation from:
// pythonista.wordpress.com/2008/07/03/pure-python-fibonacci-numbers
BigInt fibonacci(in ulong n)
in {
    assert(n > 0, "fibonacci(n): n must be > 0.");
} body {
    if (n <= 2)
        return 1.BigInt;
    BigInt F = 1;
    BigInt L = 1;
    int sign = -1;
    immutable uint n2 = cast(uint)n.log2.floor;
    auto mask = 2.BigInt ^^ (n2 - 1);
    foreach (immutable i; 1 .. n2) {
        auto temp = F ^^ 2;
        F = (F + L) / 2;
        F = 2 * F ^^ 2 - 3 * temp - 2 * sign;
        L = 5 * temp + 2 * sign;
        sign = 1;
        if (n & mask) {
            temp = F;
            F = (F + L) / 2;
            L = F + 2 * temp;
            sign = -1;
        }
        mask /= 2;
    }
    if ((n & mask) == 0) {
        F *= L;
    } else {
        F = (F + L) / 2;
        F = F * L - sign;
    }
    return F;
}

void main() {
    10_000_000.fibonacci;
}
```



## Dart


```dart
int fib(int n) {
  if (n==0 || n==1) {
    return n;
  }
  var prev=1;
  var current=1;
  for (var i=2; i<n; i++) {
    var next = prev + current;
    prev = current;
    current = next;
  }
  return current;
}

int fibRec(int n) => n==0 || n==1 ? n : fibRec(n-1) + fibRec(n-2);

main() {
  print(fib(11));
  print(fibRec(11));
}
```



## Dc

This needs a modern Dc with <code>r</code> (swap) and <code>#</code> (comment).
It easily can be adapted to an older Dc, but it will impact readability a lot.

```dc
[               # todo: n(<2) -- 1 and break 2 levels
  d -           # 0
  1 +           # 1
  q
] s1

[               # todo: n(>-1) -- F(n)
  d 0=1         # n(!=0)
  d 1=1         # n(!in {0,1})
  2 - d 1 +     # (n-2) (n-1)
  lF x          # (n-2) F(n-1)
  r             # F(n-1) (n-2)
  lF x          # F(n-1)+F(n-2)
  +
] sF

33 lF x f
```

```txt

5702887

```



## Delphi



###  Iterative


```Delphi

function FibonacciI(N: Word): UInt64;
var
  Last, New: UInt64;
  I: Word;
begin
  if N < 2 then
    Result := N
  else begin
    Last := 0;
    Result := 1;
    for I := 2 to N do
    begin
      New := Last + Result;
      Last := Result;
      Result := New;
    end;
  end;
end;

```



###  Recursive


```Delphi

function Fibonacci(N: Word): UInt64;
begin
  if N < 2 then
    Result := N
  else
   Result := Fibonacci(N - 1) + Fibonacci(N - 2);
end;

```



###  Matrix

Algorithm is based on
:<math>\begin{pmatrix}1&1\\1&0\end{pmatrix}^n = \begin{pmatrix}F(n+1)&F(n)\\F(n)&F(n-1)\end{pmatrix}</math>.

```Delphi

function fib(n: Int64): Int64;

  type TFibMat = array[0..1] of array[0..1] of Int64;

  function FibMatMul(a,b: TFibMat): TFibMat;
  var i,j,k: integer;
      tmp: TFibMat;
  begin
    for i := 0 to 1 do
      for j := 0 to 1 do
      begin
	tmp[i,j] := 0;
	for k := 0 to 1 do tmp[i,j] := tmp[i,j] + a[i,k] * b[k,j];
      end;
    FibMatMul := tmp;
  end;

  function FibMatExp(a: TFibMat; n: Int64): TFibmat;
  begin
    if n <= 1 then fibmatexp := a
    else if (n mod 2 = 0) then FibMatExp := FibMatExp(FibMatMul(a,a), n div 2)
    else if (n mod 2 = 1) then FibMatExp := FibMatMul(a, FibMatExp(FibMatMul(a,a), n div 2));
  end;

var
  matrix: TFibMat;

begin
  matrix[0,0] := 1;
  matrix[0,1] := 1;
  matrix[1,0] := 1;
  matrix[1,1] := 0;
  if n > 1 then
    matrix := fibmatexp(matrix,n-1);
  fib := matrix[0,0];
end;

```



## DWScript



```Delphi
function fib(N : Integer) : Integer;
begin
  if N < 2 then Result := 1
  else Result := fib(N-2) + fib(N-1);
End;
```



## Dyalect



```Dyalect
func fib(n) {
    if n < 2 {
        return n
    } else {
        return fib(n - 1) + fib(n - 2)
    }
}

print(fib(30))
```



## E


```e
def fib(n) {
    var s := [0, 1]
    for _ in 0..!n {
        def [a, b] := s
        s := [b, a+b]
    }
    return s[0]
}
```


(This version defines <tt>fib(0) = 0</tt> because [http://www.research.att.com/~njas/sequences/A000045 OEIS A000045] does.)


## EasyLang


<lang>n = number input
a = 0
b = 1
while n > 1
  h = a + b
  a = b
  b = h
  n -= 1
.
print b
```



## EchoLisp

Use '''memoization''' with the recursive version.

```scheme

(define (fib n)
    (if (< n 2) n
    (+ (fib (- n 2)) (fib (1- n)))))

(remember 'fib #(0 1))

(for ((i 12)) (write (fib i)))
0 1 1 2 3 5 8 13 21 34 55 89

```



## ECL



### Analytic



```ECL
//Calculates Fibonacci sequence up to n steps using Binet's closed form solution


FibFunction(UNSIGNED2 n) := FUNCTION
	REAL Sqrt5 := Sqrt(5);
	REAL Phi := (1+Sqrt(5))/2;
	REAL Phi_Inv := 1/Phi;
	UNSIGNED FibValue := ROUND( ( POWER(Phi,n)-POWER(Phi_Inv,n) ) /Sqrt5);
	RETURN FibValue;
	END;

 FibSeries(UNSIGNED2 n) := FUNCTION

 Fib_Layout := RECORD
 UNSIGNED5 FibNum;
 UNSIGNED5 FibValue;
 END;

 FibSeq := DATASET(n+1,
  TRANSFORM
 ( Fib_Layout
 , SELF.FibNum := COUNTER-1
 , SELF.FibValue := IF(SELF.FibNum<2,SELF.FibNum, FibFunction(SELF.FibNum) )
 )
 );

 RETURN FibSeq;

 END; }
```



## EDSAC order code

This program calculates the <i>n</i>th—by default the tenth—number in the Fibonacci sequence and displays it (in binary) in the first word of storage tank 3.

```edsac
[ Fibonacci sequence

### ============


  A program for the EDSAC

  Calculates the nth Fibonacci
  number and displays it at the
  top of storage tank 3

  The default value of n is 10

  To calculate other Fibonacci
  numbers, set the starting value
  of the count to n-2

  Works with Initial Orders 2 ]


        T56K  [ set load point  ]
        GK    [ set theta       ]

[ Orders ]

[  0 ]  T20@  [ a = 0           ]
        A17@  [ a += y          ]
        U18@  [ temp = a        ]
        A16@  [ a += x          ]
        T17@  [ y = a; a = 0    ]
        A18@  [ a += temp       ]
        T16@  [ x = a; a = 0    ]

        A19@  [ a = count       ]
        S15@  [ a -= 1          ]
        U19@  [ count = a       ]
        E@    [ if a>=0 go to θ ]

        T20@  [ a = 0           ]
        A17@  [ a += y          ]
        T96F  [ C(96) = a; a = 0]

        ZF    [ halt ]

[ Data ]

[ 15 ]  P0D   [ const: 1        ]
[ 16 ]  P0F   [ var: x = 0      ]
[ 17 ]  P0D   [ var: y = 1      ]
[ 18 ]  P0F   [ var: temp = 0   ]
[ 19 ]  P4F   [ var: count = 8  ]
[ 20 ]  P0F   [ used to clear a ]

        EZPF  [ begin execution ]
```

```txt
00000000000110111
```



## Eiffel


```eiffel

class
	APPLICATION

create
	make

feature

	fibonacci (n: INTEGER): INTEGER
		require
			non_negative: n >= 0
		local
			i, n2, n1, tmp: INTEGER
		do
			n2 := 0
			n1 := 1
			from
				i := 1
			until
				i >= n
			loop
				tmp := n1
				n1 := n2 + n1
				n2 := tmp
				i := i + 1
			end
			Result := n1
			if n = 0 then
				Result := 0
			end
		end

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print (fibonacci (0))
			print (" ")
			print (fibonacci (1))
			print (" ")
			print (fibonacci (2))
			print (" ")
			print (fibonacci (3))
			print (" ")
			print (fibonacci (4))
			print ("%N")
		end

end

```



## Ela

Tail-recursive function:

```Ela
fib = fib' 0 1
      where fib' a b 0 = a
            fib' a b n = fib' b (a + b) (n - 1)
```

Infinite (lazy) list:

```Ela
fib = fib' 1 1
      where fib' x y = & x :: fib' y (x + y)
```


## Elena

ELENA 4.1 :

```elena
import extensions;

fibu(n)
{
    int[] ac := new int[]::( 0,1 );
    if (n < 2)
    {
        ^ ac[n]
    }
    else
    {
        for(int i := 2, i <= n, i+=1)
        {
            int t := ac[1];
            ac[1] := ac[0] + ac[1];
            ac[0] := t
        };

        ^ ac[1]
    }
}

public program()
{
    for(int i := 0, i <= 10, i+=1)
    {
        console.printLine(fibu(i))
    }
}
```

```txt

0
1
1
2
3
5
8
13
21
34
55

```



## Elixir


```Elixir
defmodule Fibonacci do
    def fib(0), do: 0
    def fib(1), do: 1
    def fib(n), do: fib(0, 1, n-2)

    def fib(_, prv, -1), do: prv
    def fib(prvprv, prv, n) do
        next = prv + prvprv
        fib(prv, next, n-1)
    end
end

IO.inspect Enum.map(0..10, fn i-> Fibonacci.fib(i) end)
```


Using Stream:

```Elixir

Stream.unfold({0,1}, fn {a,b} -> {a,{b,a+b}} end) |> Enum.take(10)

```


```txt

[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

```



## Elm

Naïve recursive implementation.

```haskell
fibonacci : Int -> Int
fibonacci n = if n < 2 then
        n
    else
        fibonacci(n - 2) + fibonacci(n - 1)
```



## Emacs Lisp


### version 1


```Emacs Lisp

(defun fib (n a b c)
  (if (< c n) (fib n b (+ a b) (+ 1 c) )
    (if (= c n) b a) ))

(defun fibonacci (n) (if (< n 2) n (fib n 0 1 1) ))

```


### version 2


```Emacs Lisp

(defun fibonacci (n)
  (let ( (vec) (i) (j) (k) )
    (if (< n 2) n
      (progn
	(setq vec (make-vector (+ n 1) 0) i 0 j 1 k 2)
	(setf (aref vec 1) 1)
	(while (<= k n)
	  (setf (aref vec k) (+ (elt vec i) (elt vec j) ))
	  (setq i (+ 1 i) j (+ 1 j) k (+ 1 k) ))
	(elt vec n) ))))

```

<b>Eval:</b>

```Emacs Lisp

(insert
 (mapconcat '(lambda (n) (format "%d" (fibonacci n) ))
	    (number-sequence 0 15) " ") )

```

<b>Output:</b>

```txt

0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610

```



## Erlang


### Recursive


```Erlang

-module(fib).
-export([fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

```



### Iterative


```Erlang

-module(fiblin).
-export([fib/1])

fib(0) -> 0;
fib(1) -> 1;
fib(2) -> 1;
fib(3) -> 2;
fib(4) -> 3;
fib(5) -> 5;

fib(N) when is_integer(N) -> fib(N - 6, 5, 8).
fib(N, A, B) -> if N < 1 -> B; true -> fib(N-1, B, A+B) end.


```


<b> Evaluate:</b>

```Erlang

io:write([fiblin:fib(X) || X <- lists:seq(1,10) ]).

```


<b> Output:</b>

```txt

[1,1,2,3,5,8,13,21,34,55]ok

```



### Iterative 2


```Erlang

fib(N) -> fib(N, 0, 1).

fib(0, Result, _Next) -> Result;
fib(Iter, Result, Next) -> fib(Iter-1, Next, Result+Next).


```



## ERRE


```ERRE
!-------------------------------------------
! derived from my book "PROGRAMMARE IN ERRE"
! iterative solution
!-------------------------------------------

PROGRAM FIBONACCI

!$DOUBLE

!VAR F1#,F2#,TEMP#,COUNT%,N%

BEGIN    !main
   INPUT("Number",N%)
   F1=0
   F2=1
   REPEAT
      TEMP=F2
      F2=F1+F2
      F1=TEMP
      COUNT%=COUNT%+1
   UNTIL COUNT%=N%
   PRINT("FIB(";N%;")=";F2)

   ! Obviously a FOR loop or a WHILE loop can
   ! be used to solve this problem

END PROGRAM

```

```txt

Number? 20
FIB( 20 )= 6765

```



## Euphoria

==='Recursive' version===
```Euphoria

function fibor(integer n)
  if n<2 then return n end if
  return fibor(n-1)+fibor(n-2)
end function

```


==='Iterative' version===
```Euphoria

function fiboi(integer n)
integer f0=0, f1=1, f
  if n<2 then return n end if
  for i=2 to n do
    f=f0+f1
    f0=f1
    f1=f
  end for
  return f
end function

```


==='Tail recursive' version===
```Euphoria

function fibot(integer n, integer u = 1, integer s = 0)
  if n < 1 then
    return s
  else
    return fibot(n-1,u+s,u)
  end if
end function

-- example:
? fibot(10) -- says 55

```


==='Paper tape' version===
```Euphoria

include std/mathcons.e -- for PINF constant

enum ADD, MOVE, GOTO, OUT, TEST, TRUETO

global sequence tape = { 0,
			 1,
		       { ADD, 2, 1 },
		       { TEST, 1, PINF },
		       { TRUETO, 0 },
		       { OUT, 1, "%.0f\n" },
		       { MOVE, 2, 1 },
		       { MOVE, 0, 2 },
		       { GOTO, 3  } }

global integer ip
global integer test
global atom accum

procedure eval( sequence cmd )
	atom i = 1
	while i <= length( cmd ) do
		switch cmd[ i ] do
			case ADD then
				accum = tape[ cmd[ i + 1 ] ] + tape[ cmd[ i + 2 ] ]
				i += 2

			case OUT then
				printf( 1, cmd[ i + 2], tape[ cmd[ i + 1 ] ] )
				i += 2

			case MOVE then
				if cmd[ i + 1 ] = 0 then
					tape[ cmd[ i + 2 ] ] = accum
				else
					tape[ cmd[ i + 2 ] ] = tape[ cmd[ i + 1 ] ]
				end if
				i += 2

			case GOTO then
				ip = cmd[ i + 1 ] - 1 -- due to ip += 1 in main loop
				i += 1

			case TEST then
				if tape[ cmd[ i + 1 ] ] = cmd[ i + 2 ] then
					test = 1
				else
					test = 0
				end if
				i += 2

			case TRUETO then
				if test then
					if cmd[ i + 1 ] = 0 then
						abort(0)
					else
						ip = cmd[ i + 1 ] - 1
					end if
				end if

		end switch
		i += 1
	end while
end procedure

test = 0
accum = 0
ip = 1

while 1 do

	-- embedded sequences (assumed to be code) are evaluated
	-- atoms (assumed to be data) are ignored

	if sequence( tape[ ip ] ) then
		eval( tape[ ip ] )
	end if
	ip += 1
end while


```



## FALSE


```false
[[$0=~][1-@@\$@@+\$44,.@]#]f:
20n: {First 20 numbers}
0 1 n;f;!%%44,. {Output: "0,1,1,2,3,5..."}
```



## Factor


### Iterative


```factor
: fib ( n -- m )
    dup 2 < [
        [ 0 1 ] dip [ swap [ + ] keep ] times
        drop
    ] unless ;
```



### Recursive


```factor
: fib ( n -- m )
    dup 2 < [
        [ 1 - fib ] [ 2 - fib ] bi +
    ] unless ;
```


===Tail-Recursive===

```factor
: fib2 ( x y n -- a )
  dup 1 <
    [ 2drop ]
    [ [ swap [ + ] keep ] dip 1 - fib2 ]
  if ;
: fib ( n -- m ) [ 0 1 ] dip fib2 ;
```



### Matrix

```factor
USE: math.matrices

: fib ( n -- m )
    dup 2 < [
        [ { { 0 1 } { 1 1 } } ] dip 1 - m^n
        second second
    ] unless ;
```



## Fancy


```fancy
class Fixnum {
  def fib {
    match self -> {
      case 0 -> 0
      case 1 -> 1
      case _ -> self - 1 fib + (self - 2 fib)
    }
  }
}

15 times: |x| {
  x fib println
}

```



## Falcon


### Iterative


```falcon
function fib_i(n)

    if n < 2: return n

    fibPrev = 1
    fib = 1
    for i in [2:n]
        tmp = fib
        fib += fibPrev
        fibPrev = tmp
    end
    return fib
end
```


### Recursive


```falcon
function fib_r(n)
    if n < 2 :  return n
    return fib_r(n-1) + fib_r(n-2)
end
```


### Tail Recursive


```falcon
function fib_tr(n)
    return fib_aux(n,0,1)
end
function fib_aux(n,a,b)
   switch n
      case 0 : return a
      default: return fib_aux(n-1,a+b,a)
   end
end
```



## Fantom


Ints have a limit of 64-bits, so overflow errors occur after computing Fib(92) = 7540113804746346429.


```fantom

class Main
{
  static Int fib (Int n)
  {
    if (n < 2) return n
    fibNums := [1, 0]
    while (fibNums.size <= n)
    {
      fibNums.insert (0, fibNums[0] + fibNums[1])
    }
    return fibNums.first
  }

  public static Void main ()
  {
    20.times |n|
    {
      echo ("Fib($n) is ${fib(n)}")
    }
  }
}

```




## Fexl



```Fexl

# (fib n) = the nth Fibonacci number
\fib=
    (
    \loop==
        (\x\y\n
        le n 0 x;
        \z=(+ x y)
        \n=(- n 1)
        loop y z n
        )
    loop 0 1
    )


# Now test it:
for 0 20 (\n say (fib n))

```


```txt

0
1
1
2
3
5
8
13
21
34
55
89
144
233
377
610
987
1597
2584
4181
6765

```



## FOCAL


```focal
01.10 TYPE "FIBONACCI NUMBERS" !
01.20 ASK "N =", N
01.30 SET A=0
01.40 SET B=1
01.50 FOR I=2,N; DO 2.0
01.60 TYPE "F(N) ", %8, B, !
01.70 QUIT

02.10 SET T=B
02.20 SET B=A+B
02.30 SET A=T
```

```txt
FIBONACCI NUMBERS
N =:20
F(N) =    6765
```



## Forth


```forth
: fib ( n -- fib )
  0 1 rot 0 ?do  over + swap  loop drop ;
```


Since there are only a fixed and small amount of Fibonacci numbers that fit in a machine word, this FORTH version creates a table of Fibonacci numbers at compile time.  It stops compiling numbers when there is arithmetic overflow (the number turns negative, indicating overflow.)


```forth
: F-start,  here 1 0 dup , ;
: F-next,   over + swap
            dup 0> IF  dup , true  ELSE  false  THEN ;

: computed-table  ( compile: 'start 'next / run: i -- x )
   create
      >r execute
      BEGIN  r@ execute not  UNTIL  rdrop
   does>
       swap cells + @ ;

' F-start, ' F-next,  computed-table fibonacci 2drop
here swap - cell/ Constant #F/64   \ # of fibonacci numbers generated

16 fibonacci . 987  ok
#F/64 . 93  ok
92 fibonacci . 7540113804746346429  ok   \ largest number generated.
```



## Fortran


### FORTRAN IV


```fortran
C     FIBONACCI SEQUENCE - FORTRAN IV
      NN=46
      DO 1 I=0,NN
    1 WRITE(*,300) I,IFIBO(I)
  300 FORMAT(1X,I2,1X,I10)
      END
C
      FUNCTION IFIBO(N)
      IF(N) 9,1,2
    1 IFN=0
      GOTO 9
    2 IF(N-1) 9,3,4
    3 IFN=1
      GOTO 9
    4 IFNM1=0
      IFN=1
      DO 5 I=2,N
      IFNM2=IFNM1
      IFNM1=IFN
    5 IFN=IFNM1+IFNM2
    9 IFIBO=IFN
      END
```

```txt

  0          0
  1          1
  2          1
  3          2
  4          3
  5          5
  6          8
  7         13
  8         21
  9         34
 10         55
...
 45 1134903170
 46 1836311903

```


### FORTRAN 77


```fortran

      FUNCTION IFIB(N)
      IF (N.EQ.0) THEN
        ITEMP0=0
      ELSE IF (N.EQ.1) THEN
        ITEMP0=1
      ELSE IF (N.GT.1) THEN
        ITEMP1=0
        ITEMP0=1
        DO 1 I=2,N
          ITEMP2=ITEMP1
          ITEMP1=ITEMP0
          ITEMP0=ITEMP1+ITEMP2
    1   CONTINUE
      ELSE
        ITEMP1=1
        ITEMP0=0
        DO 2 I=-1,N,-1
          ITEMP2=ITEMP1
          ITEMP1=ITEMP0
          ITEMP0=ITEMP2-ITEMP1
    2   CONTINUE
      END IF
      IFIB=ITEMP0
      END

```

Test program

```fortran

      EXTERNAL IFIB
      CHARACTER*10 LINE
      PARAMETER ( LINE = '----------' )
      WRITE(*,900) 'N', 'F[N]', 'F[-N]'
      WRITE(*,900) LINE, LINE, LINE
      DO 1 N = 0, 10
        WRITE(*,901) N, IFIB(N), IFIB(-N)
    1 CONTINUE
  900 FORMAT(3(X,A10))
  901 FORMAT(3(X,I10))
      END

```

```txt

          N       F[N]      F[-N]
 ---------- ---------- ----------
          0          0          0
          1          1          1
          2          1         -1
          3          2          2
          4          3         -3
          5          5          5
          6          8         -8
          7         13         13
          8         21        -21
          9         34         34
         10         55        -55

```



### Recursive

In ISO Fortran 90 or later, use a RECURSIVE function:

```fortran
module fibonacci
contains
    recursive function fibR(n) result(fib)
        integer, intent(in) :: n
        integer             :: fib

        select case (n)
            case (:0);      fib = 0
            case (1);       fib = 1
            case default;   fib = fibR(n-1) + fibR(n-2)
        end select
    end function fibR
```


### Iterative

In ISO Fortran 90 or later:

```fortran
    function fibI(n)
        integer, intent(in) :: n
        integer, parameter :: fib0 = 0, fib1 = 1
        integer            :: fibI, back1, back2, i

        select case (n)
            case (:0);      fibI = fib0
            case (1);       fibI = fib1

            case default
                fibI = fib1
                back1 = fib0
                do i = 2, n
                    back2 = back1
                    back1 = fibI
                    fibI   = back1 + back2
                end do
         end select
    end function fibI
end module fibonacci
```


Test program

```fortran
program fibTest
    use fibonacci

    do i = 0, 10
        print *, fibr(i), fibi(i)
    end do
end program fibTest
```


```txt

0 0
1 1
1 1
2 2
3 3
5 5
8 8
13 13
21 21
34 34
55 55

```



## FreeBASIC

Extended sequence coded big integer.

```FreeBASIC
'Fibonacci extended
'Freebasic version 24  Windows
Dim Shared ADDQmod(0 To 19) As Ubyte
Dim Shared ADDbool(0 To 19) As Ubyte

For z As Integer=0 To 19
    ADDQmod(z)=(z Mod 10+48)
    ADDbool(z)=(-(10<=z))
Next z

Function plusINT(NUM1 As String,NUM2 As String) As String
    Dim As Byte flag
    #macro finish()
    three=Ltrim(three,"0")
    If three="" Then Return "0"
    If flag=1 Then Swap NUM2,NUM1
    Return three
    Exit Function
    #endmacro
    var lenf=Len(NUM1)
    var lens=Len(NUM2)
    If lens>lenf Then
        Swap NUM2,NUM1
        Swap lens,lenf
        flag=1
    End If

    var diff=lenf-lens-Sgn(lenf-lens)
    var three="0"+NUM1
    var two=String(lenf-lens,"0")+NUM2
    Dim As Integer n2
    Dim As Ubyte addup,addcarry

    addcarry=0

    For n2=lenf-1 To diff Step -1
        addup=two[n2]+NUM1[n2]-96
        three[n2+1]=addQmod(addup+addcarry)
        addcarry=addbool(addup+addcarry)
    Next n2
    If addcarry=0 Then
        finish()
    End If
    If n2=-1 Then
        three[0]=addcarry+48
        finish()
    End If

    For n2=n2 To 0 Step -1
        addup=two[n2]+NUM1[n2]-96
        three[n2+1]=addQmod(addup+addcarry)
        addcarry=addbool(addup+addcarry)
    Next n2
    three[0]=addcarry+48
    finish()
End Function

Function  fibonacci(n As Integer) As String
    Dim As String sl,l,term
    sl="0": l="1"
    If n=1 Then Return "0"
    If n=2 Then Return "1"
    n=n-2
    For x As Integer= 1 To n
        term=plusINT(l,sl)
        sl=l
        l=term
    Next x
    Function =term
End Function

'
### ===========  EXAMPLE ============

print "THE SEQUENCE TO 10:"
print
For n As Integer=1 To 10
    Print "term";n;": "; fibonacci(n)
Next n
print
print "Selected Fibonacci number"
print "Fibonacci 500"
print
print fibonacci(500)
Sleep
```

```txt
THE SEQUENCE TO 10:

term 1: 0
term 2: 1
term 3: 1
term 4: 2
term 5: 3
term 6: 5
term 7: 8
term 8: 13
term 9: 21
term 10: 34

Selected Fibonacci number
Fibonacci 500

86168291600238450732788312165664788095941068326060883324529903470149056115823592
713458328176574447204501
```



## Free Pascal

''See also: [[#Pascal|Pascal]]''

```Pascal
type
	/// domain for Fibonacci function
	/// where result is within nativeUInt
	// You can not name it fibonacciDomain,
	// since the Fibonacci function itself
	// is defined for all whole numbers
	// but the result beyond F(n) exceeds high(nativeUInt).
	fibonacciLeftInverseRange =
		{$ifdef CPU64} 0..93 {$else} 0..47 {$endif};

{**
	implements Fibonacci sequence iteratively

	\param n the index of the Fibonacci number to calculate
	\returns the Fibonacci value at n
}
function fibonacci(const n: fibonacciLeftInverseRange): nativeUInt;
type
	/// more meaningful identifiers than simple integers
	relativePosition = (previous, current, next);
var
	/// temporary iterator variable
	i: longword;
	/// holds preceding fibonacci values
	f: array[relativePosition] of nativeUInt;
begin
	f[previous] := 0;
	f[current] := 1;

	// note, in Pascal for-loop-limits are inclusive
	for i := 1 to n do
	begin
		f[next] := f[previous] + f[current];
		f[previous] := f[current];
		f[current] := f[next];
	end;

	// assign to previous, bc f[current] = f[next] for next iteration
	fibonacci := f[previous];
end;
```



## Frink

All of Frink's integers can be arbitrarily large.

```frink

fibonacciN[n] :=
{
   a = 0
   b = 1
   count = 0
   while count < n
   {
      [a,b] = [b, a + b]
      count = count + 1
   }
   return a
}

```



## FRISC Assembly

To find the nth Fibonacci number, call this subroutine with n in register R0: the answer will be returned in R0 too. Contents of other registers are preserved.

```friscasm
FIBONACCI   PUSH   R1
            PUSH   R2
            PUSH   R3

            MOVE   0,  R1
            MOVE   1,  R2

FIB_LOOP    SUB    R0,  1, R0
            JP_Z   FIB_DONE

            MOVE   R2, R3
            ADD    R1, R2, R2
            MOVE   R3, R1

            JP     FIB_LOOP

FIB_DONE    MOVE   R2, R0

            POP    R3
            POP    R2
            POP    R1

            RET
```


=={{header|F_Sharp|F#}}==
This is a fast [tail-recursive] approach using the F# big integer support:

```fsharp

let fibonacci n : bigint =
  let rec f a b n =
    match n with
    | 0 -> a
    | 1 -> b
    | n -> (f b (a + b) (n - 1))
  f (bigint 0) (bigint 1) n
> fibonacci 100;;
val it : bigint = 354224848179261915075I
```

Lazy evaluated using sequence workflow:

```fsharp
let rec fib = seq { yield! [0;1];
                    for (a,b) in Seq.zip fib (Seq.skip 1 fib) -> a+b}
```


The above is extremely slow due to the nested recursions on sequences, which aren't very efficient at the best of times.  The above takes seconds just to compute the 30th Fibonacci number!

Lazy evaluation using the sequence unfold anamorphism is much much better as to efficiency:

```fsharp
let fibonacci = Seq.unfold (fun (x, y) -> Some(x, (y, x + y))) (0I,1I)
fibonacci |> Seq.nth 10000

```


Approach similar to the Matrix algorithm in C#, with some shortcuts involved.
Since it uses exponentiation by squaring, calculations of fib(n) where n is a power of 2 are particularly quick.
Eg. fib(2^20) was calculated in a little over 4 seconds on this poster's laptop.

```fsharp

open System
open System.Diagnostics
open System.Numerics

/// Finds the highest power of two which is less than or equal to a given input.
let inline prevPowTwo (x : int) =
    let mutable n = x
    n <- n - 1
    n <- n ||| (n >>> 1)
    n <- n ||| (n >>> 2)
    n <- n ||| (n >>> 4)
    n <- n ||| (n >>> 8)
    n <- n ||| (n >>> 16)
    n <- n + 1
    match x with
    | x when x = n -> x
    | _ -> n/2

/// Evaluates the nth Fibonacci number using matrix arithmetic and
/// exponentiation by squaring.
let crazyFib (n : int) =
    let powTwo = prevPowTwo n

    /// Applies 2n rule repeatedly until another application of the rule would
    /// go over the target value (or the target value has been reached).
    let rec iter1 i q r s =
        match i with
        | i when i < powTwo ->
            iter1 (i*2) (q*q + r*r) (r * (q+s)) (r*r + s*s)
        | _ -> i, q, r, s

    /// Applies n+1 rule until the target value is reached.
    let rec iter2 (i, q, r, s) =
        match i with
        | i when i < n ->
            iter2 ((i+1), (q+r), q, r)
        | _ -> q

    match n with
    | 0 -> 1I
    | _ ->
        iter1 1 1I 1I 0I
        |> iter2

```



## FunL


###  Recursive


```funl
def
  fib( 0 ) = 0
  fib( 1 ) = 1
  fib( n ) = fib( n - 1 ) + fib( n - 2 )
```



###  Tail Recursive


```funl
def fib( n ) =
  def
    _fib( 0, prev, _ )    = prev
    _fib( 1, _,    next ) = next
    _fib( n, prev, next ) = _fib( n - 1, next, next + prev )

  _fib( n, 0, 1 )
```



###  Lazy List


```funl
val fib =
  def _fib( a, b ) = a # _fib( b, a + b )

  _fib( 0, 1 )

println( fib(10000) )
```


```txt

33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875

```



###  Iterative


```funl
def fib( n ) =
  a, b = 0, 1

  for i <- 1..n
    a, b = b, a+b

  a
```


=== Binet's Formula ===

```funl
import math.sqrt

def fib( n ) =
  phi = (1 + sqrt( 5 ))/2
  int( (phi^n - (-phi)^-n)/sqrt(5) + .5 )
```



###  Matrix Exponentiation


```funl
def mul( a, b ) =
  res = array( a.length(), b(0).length() )

  for i <- 0:a.length(), j <- 0:b(0).length()
    res( i, j ) = sum( a(i, k)*b(k, j) | k <- 0:b.length() )

  vector( res )

def
  pow( _, 0 ) = ((1, 0), (0, 1))
  pow( x, 1 ) = x
  pow( x, n )
    | 2|n = pow( mul(x, x), n\2 )
    | otherwise = mul(x, pow( mul(x, x), (n - 1)\2 ) )

def fib( n ) = pow( ((0, 1), (1, 1)), n )(0, 1)

for i <- 0..10
  println( fib(i) )
```


```txt

0
1
1
2
3
5
8
13
21
34
55

```



## Futhark



###  Iterative



```Futhark

fun main(n: int): int =
  loop((a,b) = (0,1)) = for _i < n do
    (b, a + b)
  in a

```



## FutureBasic


###  Iterative


```futurebasic

include "Tlbx Timer.incl"
include "ConsoleWindow"

local fn Fibonacci( n as long ) as Long
begin globals
dim as long s1, s2// static
end globals

dim as long temp

if ( n < 2 )
  s1 = n
  exit fn
else
  temp = s1 + s2
  s2 = s1
  s1 = temp
  exit fn
end if
end fn = s1

dim as long i
dim as UnsignedWide  start, finish

Microseconds( @start )
for i = 0 to 40
print i; ". "; fn Fibonacci(i)
next i
Microseconds( @finish )
print "Compute time:"; (finish.lo - start.lo ) / 1000; " ms"

```

Output:

```txt

 0.  0
 1.  1
 2.  1
 3.  2
 4.  3
 5.  5
 6.  8
 7.  13
 8.  21
 9.  34
 10.  55
 11.  89
 12.  144
 13.  233
 14.  377
 15.  610
 16.  987
 17.  1597
 18.  2584
 19.  4181
 20.  6765
 21.  10946
 22.  17711
 23.  28657
 24.  46368
 25.  75025
 26.  121393
 27.  196418
 28.  317811
 29.  514229
 30.  832040
 31.  1346269
 32.  2178309
 33.  3524578
 34.  5702887
 35.  9227465
 36.  14930352
 37.  24157817
 38.  39088169
 39.  63245986
 40.  102334155
 41.  165580141
 42.  267914296
 43.  433494437
 44.  701408733
 45.  1134903170
 46.  1836311903
 47.  2971215073
 48.  4.80752698e+9
 49.  7.77874205e+9
 50.  1.2586269e+10
 51.  2.03650111e+10
 52.  3.29512801e+10
 53.  5.33162912e+10
 54.  8.62675713e+10
 55.  1.39583862e+11
 56.  2.25851434e+11
 57.  3.65435296e+11
 58.  5.9128673e+11
 59.  9.56722026e+11
 60.  1.54800876e+12
 61.  2.50473078e+12
 62.  4.05273954e+12
 63.  6.55747032e+12
 64.  1.06102099e+13
 65.  1.71676802e+13
 66.  2.777789e+13
 67.  4.49455702e+13
 68.  7.27234602e+13
 69.  1.1766903e+14
 70.  1.90392491e+14
 71.  3.08061521e+14
 72.  4.98454012e+14
 73.  8.06515533e+14
 74.  1.30496954e+15
 75.  2.11148508e+15
 76.  3.41645462e+15
 77.  5.5279397e+15
 78.  8.94439432e+15
 79.  1.4472334e+16
 80.  2.34167283e+16
 81.  3.78890624e+16
 82.  6.13057907e+16
 83.  9.91948531e+16
 84.  1.60500644e+17
 85.  2.59695497e+17
 86.  4.20196141e+17
 87.  6.79891638e+17
 88.  1.10008778e+18
 89.  1.77997942e+18
 90.  2.88006719e+18
 91.  4.66004661e+18
 92.  7.5401138e+18
 93.  1.22001604e+19
 94.  1.97402742e+19
 95.  3.19404346e+19
 96.  5.16807089e+19
 97.  8.36211435e+19
 98.  1.35301852e+20
 99.  2.18922996e+20
 100.  3.54224848e+20
Compute time: 15 ms

```



## GAP


```gap
fib := function(n)
  local a;
  a := [[0, 1], [1, 1]]^n;
  return a[1][2];
end;
```

GAP has also a buit-in function for that.

```gap
Fibonacci(n);
```



## Gecho


```gecho>0 1 dup wover + dup wover + dup wover + dup wover +</lang

Prints the first several fibonacci numbers...


## GFA Basic


<lang>
'
' Compute nth Fibonacci number
'
' open a window for display
OPENW 1
CLEARW 1
' Display some fibonacci numbers
' Fib(46) is the largest number GFA Basic can reach
' (long integers are 4 bytes)
FOR i%=0 TO 46
  PRINT "fib(";i%;")=";@fib(i%)
NEXT i%
' wait for a key press and tidy up
~INP(2)
CLOSEW 1
'
' Function to compute nth fibonacci number
' n must be in range 0 to 46, inclusive
'
FUNCTION fib(n%)
  LOCAL n0%,n1%,nn%,i%
  n0%=0
  n1%=1
  SELECT n%
  CASE 0
    RETURN n0%
  CASE 1
    RETURN n1%
  DEFAULT
    FOR i%=2 TO n%
      nn%=n0%+n1%
      n0%=n1%
      n1%=nn%
    NEXT i%
    RETURN nn%
  ENDSELECT
ENDFUNC

```



## GML



```gml
///fibonacci(n)
//Returns the nth fibonacci number

var n, numb;
n = argument0;

if (n == 0)
    {
    numb = 0;
    }
else
    {
    var fm2, fm1;
    fm2 = 0;
    fm1 = 1;
    numb = 1;
    repeat(n-1)
        {
        numb = fm2+fm1;
        fm2 = fm1;
        fm1 = numb;
        }
    }

return numb;
```



## Go


###  Recursive


```go
func fib(a int) int {
  if a < 2 {
    return a
  }
  return fib(a - 1) + fib(a - 2)
}
```


###  Iterative


```go
import (
	"math/big"
)

func fib(n uint64) *big.Int {
	if n < 2 {
		return big.NewInt(int64(n))
	}
	a, b := big.NewInt(0), big.NewInt(1)
	for n--; n > 0; n-- {
		a.Add(a, b)
		a, b = b, a
	}
	return b
}
```


###  Iterative using a closure


```go
func fibNumber() func() int {
	fib1, fib2 := 0, 1
	return func() int {
		fib1, fib2 = fib2, fib1 + fib2
		return fib1
	}
}

func fibSequence(n int) int {
	f := fibNumber()
	fib := 0
	for i := 0; i < n; i++ {
		fib = f()
	}
	return fib
}
```


###  Using a goroutine and channel


```go
func fib(c chan int) {
	a, b := 0, 1
	for {
		c <- a
		a, b = b, a+b
	}
}

func main() {
	c := make(chan int)
	go fib(c)
	for i := 0; i < 10; i++ {
		fmt.Println(<-c)
	}
}

```



## Groovy

Full "extra credit" solutions.

###  Recursive

A recursive closure must be ''pre-declared''.

```groovy
def rFib
rFib = {
    it == 0   ? 0
    : it == 1 ? 1
    : it > 1  ? rFib(it-1) + rFib(it-2)
    /*it < 0*/: rFib(it+2) - rFib(it+1)

}
```



###  Iterative


```groovy
def iFib = {
    it == 0   ? 0
    : it == 1 ? 1
    : it > 1  ? (2..it).inject([0,1]){i, j -> [i[1], i[0]+i[1]]}[1]
    /*it < 0*/: (-1..it).inject([0,1]){i, j -> [i[1]-i[0], i[0]]}[0]
}
```



###  Analytic


```groovy
final φ = (1 + 5**(1/2))/2
def aFib = { (φ**it - (-φ)**(-it))/(5**(1/2)) as BigInteger }
```


Test program:

```groovy
def time = { Closure c ->
    def start = System.currentTimeMillis()
    def result = c()
    def elapsedMS = (System.currentTimeMillis() - start)/1000
    printf '(%6.4fs elapsed)', elapsedMS
    result
}

print "  F(n)      elapsed time   "; (-10..10).each { printf ' %3d', it }; println()
print "--------- -----------------"; (-10..10).each { print ' ---' }; println()
[recursive:rFib, iterative:iFib, analytic:aFib].each { name, fib ->
    printf "%9s ", name
    def fibList = time { (-10..10).collect {fib(it)} }
    fibList.each { printf ' %3d', it }
    println()
}
```


```txt
  F(n)      elapsed time    -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10
--------- ----------------- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
recursive (0.0080s elapsed) -55  34 -21  13  -8   5  -3   2  -1   1   0   1   1   2   3   5   8  13  21  34  55
iterative (0.0040s elapsed) -55  34 -21  13  -8   5  -3   2  -1   1   0   1   1   2   3   5   8  13  21  34  55
 analytic (0.0030s elapsed) -55  34 -21  13  -8   5  -3   2  -1   1   0   1   1   2   3   5   8  13  21  34  55
```



## Harbour


### Recursive


```Harbour

#include "harbour.ch"
Function fibb(a,b,n)
return(if(--n>0,fibb(b,a+b,n),a))

```



### Iterative


```Harbour

#include "harbour.ch"
Function fibb(n)
	local fnow:=0, fnext:=1, tempf
	while (--n>0)
		tempf:=fnow+fnext
		fnow:=fnext
		fnext:=tempf
	end while
return(fnext)

```



## Haskell


### Analytic


```haskell
main :: IO ()
main =
  print
    [ floor (0.01 + (1 / p ** n + p ** n) / sqrt 5)
    | let p = (1 + sqrt 5) / 2
    , n <- [0 .. 42] ]
```



### Recursive

Simple definition, very inefficient.


```haskell
fib x =
  if x < 1
    then 0
    else if x < 2
           then 1
           else fib (x - 1) + fib (x - 2)
```



### Recursive with Memoization

Very fast.

```haskell
fib x =
  if x < 1
    then 0
    else if x == 1
           then 1
           else fibs !! (x - 1) + fibs !! (x - 2)
  where
    fibs = map fib [0 ..]
```



### Recursive with Memoization using memoized library

Even faster and simpler is to use a defined memoizer (e.g. from MemoTrie package):

```haskell
import Data.MemoTrie
fib :: Integer -> Integer
fib = memo f where
   f 0 = 0
   f 1 = 1
   f n = fib (n-1) + fib (n-2)
```

You can rewrite this without introducing f explicitly

```haskell
import Data.MemoTrie
fib :: Integer -> Integer
fib = memo $ \x -> case x of
   0 -> 0
   1 -> 1
   n -> fib (n-1) + fib (n-2)
```

Or using LambdaCase extension you can write it even shorter:

```haskell
{-# Language LambdaCase #-}
import Data.MemoTrie
fib :: Integer -> Integer
fib = memo $ \case
   0 -> 0
   1 -> 1
   n -> fib (n-1) + fib (n-2)
```

The version that supports negative numbers:

```haskell
{-# Language LambdaCase #-}
import Data.MemoTrie
fib :: Integer -> Integer
fib = memo $ \case
   0 -> 0
   1 -> 1
   n | n>0 -> fib (n-1) + fib (n-2)
     | otherwise -> fib (n+2) - fib (n+1)
```



### Iterative


```haskell
fib n = go n 0 1
  where
    go n a b
      | n == 0 = a
      | otherwise = go (n - 1) b (a + b)
```



### = With lazy lists =

This is a standard example how to use lazy lists. Here's the (infinite) list of all Fibonacci numbers:


```haskell
fib = 0 : 1 : zipWith (+) fib (tail fib)
```

Or alternatively:

```haskell
fib = 0 : 1 : (zipWith (+) <*> tail) fib
```


The ''n''th Fibonacci number is then just <code plang="haskell">fib !! n</code>. The above is equivalent to


```haskell
fib = 0 : 1 : next fib where next (a: t@(b:_)) = (a+b) : next t
```


Also


```haskell
fib = 0 : scanl (+) 1 fib
```



### = As a fold =

Accumulator holds last two members of the series:


```haskell
import Data.List (foldl') --'

fib :: Integer -> Integer
fib n =
  fst $
  foldl' --'
    (\(a, b) _ -> (b, a + b))
    (0, 1)
    [1 .. n]
```



###  With matrix exponentiation

Adapting the (rather slow) code from [[Matrix exponentiation operator]],
we can simply write:


```haskell
import Data.List (transpose)

fib
  :: (Integral b, Num a)
  => b -> a
fib 0 = 0 -- this line is necessary because "something ^ 0" returns "fromInteger 1", which unfortunately
-- in our case is not our multiplicative identity (the identity matrix) but just a 1x1 matrix of 1
fib n = (last . head . unMat) (Mat [[1, 1], [1, 0]] ^ n)

-- Code adapted from Matrix exponentiation operator task ---------------------
(<+>)
  :: Num c
  => [c] -> [c] -> [c]
(<+>) = zipWith (+)

(<*>)
  :: Num a
  => [a] -> [a] -> a
(<*>) = (sum .) . zipWith (*)

newtype Mat a = Mat
  { unMat :: [[a]]
  } deriving (Eq)

instance Show a =>
         Show (Mat a) where
  show xm = "Mat " ++ show (unMat xm)

instance Num a =>
         Num (Mat a) where
  negate xm = Mat $ map (map negate) $ unMat xm
  xm + ym = Mat $ zipWith (<+>) (unMat xm) (unMat ym)
  xm * ym =
    Mat
      [ [ xs Main.<*> ys -- to distinguish from standard applicative operator
        | ys <- transpose $ unMat ym ]
      | xs <- unMat xm ]
  fromInteger n = Mat [[fromInteger n]]
  abs = undefined
  signum = undefined

-- TEST ----------------------------------------------------------------------
main :: IO ()
main = (print . take 10 . show . fib) (10 ^ 5)
```


So, for example, the hundred-thousandth Fibonacci number starts with the digits:
```txt
"2597406934"
```



###  With recurrence relations

Using <code>Fib[m=3n+r]</code> [http://en.wikipedia.org/wiki/Fibonacci_number#Other_identities recurrence identities]:

```haskell
import Control.Arrow ((&&&))

fibstep :: (Integer, Integer) -> (Integer, Integer)
fibstep (a, b) = (b, a + b)

fibnums :: [Integer]
fibnums = map fst $ iterate fibstep (0, 1)

fibN2 :: Integer -> (Integer, Integer)
fibN2 m
  | m < 10 = iterate fibstep (0, 1) !! fromIntegral m
fibN2 m = fibN2_next (n, r) (fibN2 n)
  where
    (n, r) = quotRem m 3

fibN2_next (n, r) (f, g)
  | r == 0 = (a, b) -- 3n  ,3n+1
  | r == 1 = (b, c) -- 3n+1,3n+2
  | r == 2 = (c, d) -- 3n+2,3n+3   (*)
  where
    a =
      5 * f ^ 3 +
      if even n
        then 3 * f
        else (-3 * f) -- 3n
    b = g ^ 3 + 3 * g * f ^ 2 - f ^ 3 -- 3n+1
    c = g ^ 3 + 3 * g ^ 2 * f + f ^ 3 -- 3n+2
    d =
      5 * g ^ 3 +
      if even n
        then (-3 * g)
        else 3 * g -- 3(n+1)   (*)

main :: IO ()
main = print $ (length &&& take 20) . show . fst $ fibN2 (10 ^ 2)
```

```txt
(21,"35422484817926191507")
```


<code>(fibN2 n)</code> directly calculates a pair <code>(f,g)</code> of two consecutive Fibonacci numbers, <code>(Fib[n], Fib[n+1])</code>, from recursively calculated such pair at about <code>n/3</code>:

```haskell
 *Main> (length &&& take 20) . show . fst $ fibN2 (10^6)
(208988,"19532821287077577316")
```

The above should take less than 0.1s to calculate on a modern box.

Other identities that could also be used are [http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form here]. In particular, for <i>(n-1,n) ---> (2n-1,2n)</i> transition which is equivalent to the matrix exponentiation scheme, we have


```haskell
f (n,(a,b)) = (2*n,(a*a+b*b,2*a*b+b*b))     -- iterate f (1,(0,1)) ; b is nth
```


and for <i>(n,n+1) ---> (2n,2n+1)</i> (derived from d'Ocagne's identity, for example),


```haskell
g (n,(a,b)) = (2*n,(2*a*b-a*a,a*a+b*b))     -- iterate g (1,(1,1)) ; a is nth
```



## Haxe


###  Iterative



```haxe
static function fib(steps:Int, handler:Int->Void)
{
	var current = 0;
	var next = 1;

	for (i in 1...steps)
	{
		handler(current);

		var temp = current + next;
		current = next;
		next = temp;
	}
	handler(current);
}
```



###  As Iterator


```haxe
class FibIter
{
	private var current = 0;
	private var nextItem = 1;
	private var limit:Int;

	public function new(limit) this.limit = limit;

	public function hasNext() return limit > 0;

	public function next() {
		limit--;
		var ret = current;
		var temp = current + nextItem;
		current = nextItem;
		nextItem = temp;
		return ret;
	}
}
```


Used like:

```haxe
for (i in new FibIter(10))
	Sys.println(i);
```



## Hope


### Recursive


```hope
dec f : num -> num;
--- f 0 <= 0;
--- f 1 <= 1;
--- f(n+2) <= f n + f(n+1);
```


===Tail-recursive===

```hope
dec fib : num -> num;
--- fib n <= l (1, 0, n)
    whererec l == \(a,b,succ c) => if c<1 then a else l((a+b),a,c)
                  |(a,b,0) => 0;
```



###  With lazy lists

This language, being one of Haskell's ancestors, also has lazy lists. Here's the (infinite) list of all Fibonacci numbers:

```hope
dec fibs : list num;
--- fibs <= fs whererec fs == 0::1::map (+) (tail fs||fs);
```

The ''n''th Fibonacci number is then just <code plang="hope">fibs @ n</code>.


## HicEst


```hicest
REAL :: Fibonacci(10)

Fibonacci = ($==2) + Fibonacci($-1) + Fibonacci($-2)
WRITE(ClipBoard) Fibonacci ! 0 1 1 2 3 5 8 13 21 34
```



## Hy

Recursive implementation.

```clojure
(defn fib [n]
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1)))))
```


=={{header|Icon}} and {{header|Unicon}}==
Icon has built-in support for big numbers.  First, a simple recursive solution augmented by caching for non-negative input.  This examples
computes fib(1000) if there is no integer argument.


```Icon
procedure main(args)
    write(fib(integer(!args) | 1000)
end

procedure fib(n)
    static fCache
    initial {
        fCache := table()
        fCache[0] := 0
        fCache[1] := 1
        }
    /fCache[n] := fib(n-1) + fib(n-2)
    return fCache[n]
end
```

The above solution is similar to the one provided [http://www.cs.arizona.edu/icon/library/src/procs/memrfncs.icn fib in memrfncs]

Now, an O(logN) solution.
For large N, it takes far longer to convert the result to a string for output
than to do the actual computation.
This example computes fib(1000000) if there is no integer argument.


```Icon
procedure main(args)
    write(fib(integer(!args) | 1000000))
end

procedure fib(n)
    return fibMat(n)[1]
end

procedure fibMat(n)
    if n <= 0 then return [0,0]
    if n  = 1 then return [1,0]
    fp := fibMat(n/2)
    c := fp[1]*fp[1] + fp[2]*fp[2]
    d := fp[1]*(fp[1]+2*fp[2])
    if n%2 = 1 then return [c+d, d]
    else return [d, c]
end
```



## IDL


### Recursive


```idl
function fib,n
   if n lt 3 then return,1L else return, fib(n-1)+fib(n-2)
end
```


Execution time O(2^n) until memory is exhausted and your machine starts swapping. Around fib(35) on a 2GB Core2Duo.


### Iterative


```idl
function fib,n
  psum = (csum = 1uL)
  if n lt 3 then return,csum
  for i = 3,n do begin
    nsum = psum + csum
    psum = csum
    csum = nsum
  endfor
  return,nsum
end
```


Execution time O(n). Limited by size of uLong to fib(49)


### Analytic


```idl
function fib,n
  q=1/( p=(1+sqrt(5))/2 )
  return,round((p^n+q^n)/sqrt(5))
end
```


Execution time O(1), only limited by the range of LongInts to fib(48).


## Idris



### Analytic


```idris
fibAnalytic : Nat -> Double
fibAnalytic n =
    floor $ ((pow goldenRatio n) - (pow (-1.0/goldenRatio) n))  / sqrt(5)
  where goldenRatio : Double
        goldenRatio = (1.0 + sqrt(5)) / 2.0
```


### Recursive


```idris
fibRecursive : Nat -> Nat
fibRecursive Z = Z
fibRecursive (S Z) = (S Z)
fibRecursive (S (S n)) = fibRecursive (S n) + fibRecursive n
```


### Iterative


```idris
fibIterative : Nat -> Nat
fibIterative n = fibIterative' n Z (S Z)
  where fibIterative' : Nat -> Nat -> Nat -> Nat
        fibIterative' Z a _ = a
        fibIterative' (S n) a b = fibIterative' n b (a + b)
```


### Lazy


```idris
fibLazy : Lazy (List Nat)
fibLazy = 0 :: 1 :: zipWith (+) fibLazy (
              case fibLazy of
                (x::xs) => xs
                [] => [])
```



## J

The [[j:Essays/Fibonacci_Sequence|Fibonacci Sequence essay]] on the J Wiki presents a number of different ways of obtaining the nth Fibonacci number. Here is one:

```j
   fibN=: (-&2 +&$: -&1)^:(1&<) M."0
```

'''Examples:'''

```j
   fibN 12
144
   fibN  i.31
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
```


(This implementation is doubly recursive except that results are cached across function calls.)


## Java


### Iterative


```java
public static long itFibN(int n)
{
 if (n < 2)
  return n;
 long ans = 0;
 long n1 = 0;
 long n2 = 1;
 for(n--; n > 0; n--)
 {
  ans = n1 + n2;
  n1 = n2;
  n2 = ans;
 }
 return ans;
}
```



```java

/**
 * O(log(n))
 */
public static long fib(long n) {
    if (n <= 0)
	return 0;

    long i = (int) (n - 1);
    long a = 1, b = 0, c = 0, d = 1, tmp1,tmp2;

    while (i > 0) {
	if (i % 2 != 0) {
            tmp1 = d * b + c * a;
	    tmp2 = d * (b + a) + c * b;
	    a = tmp1;
	    b = tmp2;
	}

        tmp1 = (long) (Math.pow(c, 2) + Math.pow(d, 2));
        tmp2 = d * (2 * c + d);

        c = tmp1;
        d = tmp2;

        i = i / 2;
    }
    return a + b;
}

```



### Recursive


```java
public static long recFibN(final int n)
{
 return (n < 2) ? n : recFibN(n - 1) + recFibN(n - 2);
}
```


### Analytic

This method works up to the 92<sup>nd</sup> Fibonacci number. After that, it goes out of range.

```java
public static long anFibN(final long n)
{
 double p = (1 + Math.sqrt(5)) / 2;
 double q = 1 / p;
 return (long) ((Math.pow(p, n) + Math.pow(q, n)) / Math.sqrt(5));
}
```

===Tail-recursive===

```java
public static long fibTailRec(final int n)
{
 return fibInner(0, 1, n);
}

private static long fibInner(final long a, final long b, final int n)
{
 return n < 1 ? a : n == 1 ?  b : fibInner(b, a + b, n - 1);
}
```


### Streams


```java5

import java.util.function.LongUnaryOperator;
import java.util.stream.LongStream;

public class FibUtil {
 public static LongStream fibStream() {
  return LongStream.iterate( 1l, new LongUnaryOperator() {
   private long lastFib = 0;
   @Override public long applyAsLong( long operand ) {
    long ret = operand + lastFib;
    lastFib = operand;
    return ret;
   }
  });
 }
 public static long fib(long n) {
  return fibStream().limit( n ).reduce((prev, last) -> last).getAsLong();
 }
}

```



## JavaScript


### ES5


### =Recursive=

Basic recursive function:

```javascript
function fib(n) {
  return n<2?n:fib(n-1)+fib(n-2);
}
```

Can be rewritten as:

```javascript
function fib(n) {
 if (n<2) { return n; } else { return fib(n-1)+fib(n-2); }
}
```


One possibility familiar to Scheme programmers is to define an internal function for iteration through anonymous tail recursion:

```javascript
function fib(n) {
  return function(n,a,b) {
    return n>0 ? arguments.callee(n-1,b,a+b) : a;
  }(n,0,1);
}
```



### Iterative


```javascript
function fib(n) {
  var a = 0, b = 1, t;
  while (n-- > 0) {
    t = a;
    a = b;
    b += t;
    console.log(a);
  }
  return a;
}
```



### =Memoization=


With the keys of a dictionary,


```javascript
var fib = (function(cache){
    return cache = cache || {}, function(n){
        if (cache[n]) return cache[n];
        else return cache[n] = n == 0 ? 0 : n < 0 ? -fib(-n)
            : n <= 2 ? 1 : fib(n-2) + fib(n-1);
    };
})();

```


with the indices of an array,


```javascript
(function () {
    'use strict';

    function fib(n) {
        return Array.apply(null, Array(n + 1))
            .map(function (_, i, lst) {
                return lst[i] = (
                    i ? i < 2 ? 1 :
                    lst[i - 2] + lst[i - 1] :
                    0
                );
            })[n];
    }

    return fib(32);

})();
```




```txt
2178309
```


====Y-Combinator====

```javascript
function Y(dn) {
    return (function(fn) {
        return fn(fn);
    }(function(fn) {
        return dn(function() {
            return fn(fn).apply(null, arguments);
        });
    }));
}
var fib = Y(function(fn) {
    return function(n) {
        if (n === 0 || n === 1) {
            return n;
        }
        return fn(n - 1) + fn(n - 2);
    };
});
```



### =Generators=


```javascript
function* fibonacciGenerator() {
    var prev = 0;
    var curr = 1;
    while (true) {
        yield curr;
        curr = curr + prev;
        prev = curr - prev;
    }
}
var fib = fibonacciGenerator();
```



### ES6



### =Memoized=


If we want access to the whole preceding series, as well as a memoized route to a particular member,
we can use an accumulating fold.


```JavaScript
(() => {
    'use strict';

    // Nth member of fibonacci series

    // fib :: Int -> Int
    function fib(n) {
        return mapAccumL(([a, b]) => [
            [b, a + b], b
        ], [0, 1], range(1, n))[0][0];
    };

    // GENERIC FUNCTIONS

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    let mapAccumL = (f, acc, xs) => {
        return xs.reduce((a, x) => {
            let pair = f(a[0], x);

            return [pair[0], a[1].concat(pair[1])];
        }, [acc, []]);
    }

    // range :: Int -> Int -> Maybe Int -> [Int]
    let range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);


    // TEST
    return fib(32);

    // --> 2178309
})();
```


Otherwise, a simple fold will suffice.

{{Trans|Haskell}} (Memoized fold example)


```JavaScript
(() => {
    'use strict';

    // fib :: Int -> Int
    let fib = n => range(1, n)
        .reduce(([a, b]) => [b, a + b], [0, 1])[0];


    // GENERIC [m..n]

    // range :: Int -> Int -> [Int]
    let range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);


    // TEST
    return fib(32);

    // --> 2178309
})();
```


```txt
2178309
```



## Joy


### Recursive


```joy
DEFINE fib == [small] [] [pred dup pred] [+] binrec.
```



### Iterative


```joy
DEFINE fib == [1 0] dip [swap [+] unary] times popd.
```



## jq

jq does not (yet) have infinite-precision integer arithmetic, and
currently the following algorithms only give exact answers up to fib(78).  At a certain point, integers are converted to floats,
but floating point precision for fib(n) fails after n = 1476:
in jq, fib(1476) evaluates to 1.3069892237633987e+308


### Recursive


```jq
def nth_fib_naive(n):
  if (n < 2) then n
  else nth_fib_naive(n - 1) + nth_fib_naive(n - 2)
  end;
```


### Tail Recursive

Recent versions of jq (after July 1, 2014) include basic optimizations for tail recursion, and nth_fib is defined here to take advantage of TCO. For example, nth_fib(10000000) completes with only 380KB (that's K) of memory.  However nth_fib can also be used with earlier versions of jq.

```jq
def nth_fib(n):
  # input: [f(i-2), f(i-1), countdown]
  def fib: (.[0] + .[1]) as $sum
    | .[2] as $n
    | if ($n <= 0) then $sum
      else [ .[1], $sum, $n - 1 ]
    | fib end;
  [-1, 1, n] | fib;

```


Example:
```jq

(range(0;5), 50) | [., nth_fib(.)]

```

yields:
```jq
[0,0]
[1,1]
[2,1]
[3,2]
[4,3]
[50,12586269025]
```


===Binet's Formula===

```jq
def fib_binet(n):
  (5|sqrt) as $rt
  | ((1 + $rt)/2) as $phi
  | (($phi | log) * n | exp) as $phin
  | (if 0 == (n % 2) then 1 else -1 end) as $sign
  | ( ($phin - ($sign / $phin) ) / $rt ) + .5
  | floor;
```



### Generator

The following is a jq generator which produces the first n terms of the Fibonacci sequence efficiently, one by one.  Notice that it is simply a variant of the above tail-recursive function.  The function is in effect turned into a generator by changing "( _ | fib )" to "$sum, (_ | fib)".
```jq
# Generator
def fibonacci(n):
  # input: [f(i-2), f(i-1), countdown]
  def fib: (.[0] + .[1]) as $sum
           | if .[2] == 0 then $sum
             else $sum, ([ .[1], $sum, .[2] - 1 ] | fib)
             end;
  [-1, 1, n] | fib;
```



## Julia


### Recursive


```Julia
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
```


### Iterative


```Julia
function fib(n)
  x,y = (0,1)
  for i = 1:n x,y = (y, x+y) end
  x
end
```


### Matrix form


```Julia
fib(n) = ([1 1 ; 1 0]^n)[1,2]
```



## K



### Recursive


```K
{:[x<3;1;_f[x-1]+_f[x-2]]}
```



### Recursive with memoization

Using a (global) dictionary c.


```K
{c::.();{v:c[a:`$$x];:[x<3;1;:[_n~v;c[a]:_f[x-1]+_f[x-2];v]]}x}
```



### Analytic


```K
phi:(1+_sqrt(5))%2
{_((phi^x)-((1-phi)^x))%_sqrt[5]}
```



### Sequence to n


```K
{(x(|+\)\1 1)[;1]}
```


```K
{x{x,+/-2#x}/!2}
```



## Kabap



### Sequence to n


```Kabap

// Calculate the $n'th Fibonacci number

// Set this to how many in the sequence to generate
$n = 10;

// These are what hold the current calculation
$a = 0;
$b = 1;

// This holds the complete sequence that is generated
$sequence = "";

// Prepare a loop
$i = 0;
:calcnextnumber;
	$i = $i++;

	// Do the calculation for this loop iteration
	$b = $a + $b;
	$a = $b - $a;

	// Add the result to the sequence
	$sequence = $sequence << $a;

	// Make the loop run a fixed number of times
	if $i < $n; {
		$sequence = $sequence << ", ";
		goto calcnextnumber;
	}

// Use the loop counter as the placeholder
$i--;

// Return the sequence
return = "Fibonacci number " << $i << " is " << $a << " (" << $sequence << ")";

```



## Kotlin


```scala
enum class Fibonacci {
    ITERATIVE {
        override fun invoke(n: Long) = if (n < 2) {
            n
        } else {
            var n1 = 0L
            var n2 = 1L
            var i = n
            do {
                val sum = n1 + n2
                n1 = n2
                n2 = sum
            } while (i-- > 1)
            n1
        }
    },
    RECURSIVE {
        override fun invoke(n: Long): Long = if (n < 2) n else this(n - 1) + this(n - 2)
    };

    abstract operator fun invoke(n: Long): Long
}

fun main(a: Array<String>) {
    val r = 0..30L
    Fibonacci.values().forEach {
        print("${it.name}: ")
        r.forEach { i -> print(" " + it(i)) }
        println()
    }
}
```

```txt
ITERATIVE:  0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
RECURSIVE:  0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
```



## L++


```lisp
(defn int fib (int n) (return (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(main (prn (fib 30)))
```



## LabVIEW

## Lang5


```lang5
[] '__A set : dip swap __A swap 2 compress collapse '__A set execute
    __A -1 extract nip ;  : nip swap drop ;  : tuck swap over ;
: -rot rot rot ; : 0= 0 == ; : 1+ 1 + ; : 1- 1 - ; : sum '+ reduce ;
: bi 'keep dip execute ;  : keep over 'execute dip ;

: fib dup 1 > if dup 1- fib swap 2 - fib + then ;
: fib  dup 1 > if "1- fib" "2 - fib" bi + then ;
```



## Langur


```Langur
val .fibonacci = f if(.x < 2: .x ; self(.x - 1) + self(.x - 2))

writeln map .fibonacci, series 2..20
```


```txt
[1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
```



## lambdatalk



```scheme

1) basic version
{def fib1
 {lambda {:n}
  {if {< :n 3}
   then 1
   else {+ {fib1 {- :n 1}} {fib1 {- :n 2}}} }}}

{fib1 16} -> 987   (CPU ~ 16ms)
{fib1 30} = 832040 (CPU > 12000ms)

2) tail-recursive version
{def fib2
 {def fib2.r
  {lambda {:a :b :i}
   {if {< :i 1}
    then :a
    else {fib2.r :b {+ :a :b} {- :i 1}} }}}
 {lambda {:n} {fib2.r 0 1 :n}}}

{fib2 16} -> 987    (CPU ~ 1ms)
{fib2 30} -> 832040 (CPU ~2ms)
{fib2 1000} -> 4.346655768693743e+208 (CPU ~ 22ms)

3) Dijkstra Algorithm
{def fib3
 {def fib3.r
  {lambda {:a :b :p :q :count}
   {if {= :count 0}
    then :b
    else {if {= {% :count 2} 0}
    then {fib3.r :a :b
                {+ {* :p :p} {* :q :q}}
                {+ {* :q :q} {* 2 :p :q}}
                {/ :count 2}}
    else {fib3.r {+ {* :b :q} {* :a :q} {* :a :p}}
                {+ {* :b :p} {* :a :q}}
                :p :q
                {- :count 1}} }}}}
 {lambda {:n}
  {fib3.r 1 0 0 1 :n} }}

{fib3 16} -> 987    (CPU ~ 2ms)
{fib3 30} -> 832040 (CPU ~ 2ms)
{fib3 1000} -> 4.346655768693743e+208 (CPU ~ 3ms)

4) memoization
{def fib4
 {def fib4.m {array.new}}    // init an empty array
 {def fib4.r {lambda {:n}
  {if {< :n 2}
   then {array.get {array.set! {fib4.m} :n 1} :n}      // init with 1,1
   else {if {equal? {array.get {fib4.m} :n} undefined} // if not exists
   then {array.get {array.set! {fib4.m} :n
                        {+ {fib4.r {- :n 1}}
                           {fib4.r {- :n 2}}}} :n}   // compute it
   else {array.get {fib4.m} :n} }}}}                   // else get it
 {lambda {:n}
  {fib4.r :n}
  {fib4.m} }} // display the number AND all its predecessors
-> fib4
{fib4 90}
-> 4660046610375530000
[1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,
317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,
165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,
20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,
1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,
72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,
2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676220,23416728348467684,
37889062373143900,61305790721611580,99194853094755490,160500643816367070,259695496911122560,420196140727489660,
679891637638612200,1100087778366101900,1779979416004714000,2880067194370816000,4660046610375530000]

5) Binet's formula (non recursive)
{def fib5
 {lambda {:n}
  {let { {:n :n} {:sqrt5 {sqrt 5}} }
   {round {/ {- {pow {/ {+ 1 :sqrt5} 2} :n}
                {pow {/ {- 1 :sqrt5} 2} :n}} :sqrt5}}} }}

{fib5 16} -> 987    (CPU ~ 1ms)
{fib5 30} -> 832040 (CPU ~ 1ms)
{fib5 1000} -> 4.346655768693743e+208 (CPU ~ 1ms)


```



## Lasso


```Lasso

define fibonacci(n::integer) => {

	#n < 1 ? return false

	local(
		swap	= 0,
		n1		= 0,
		n2		= 1
	)

	loop(#n) => {
        #swap = #n1 + #n2;
        #n2 = #n1;
        #n1 = #swap;
	}
	return #n1

}

fibonacci(0) //->output false
fibonacci(1) //->output 1
fibonacci(2) //->output 1
fibonacci(3) //->output 2

```



## Latitude



### Recursive


```latitude
fibo := {
  takes '[n].
  if { n <= 1. } then {
    n.
  } else {
    fibo (n - 1) + fibo (n - 2).
  }.
}.
```



### Memoization


```latitude
fibo := {
  takes '[n].
  cache := here cache.
  { cache slot? (n ordinal). } ifFalse {
    cache slot (n ordinal) =
      if { n <= 1. } then {
        n.
      } else {
        fibo (n - 1) + fibo (n - 2).
      }.
  }.
  cache slot (n ordinal).
} tap {
  ;; Attach the cache to the method object itself.
  #'self cache := Object clone.
}.
```



## LFE



### Recursive



```lisp

(defun fib
  ((0) 0)
  ((1) 1)
  ((n)
    (+ (fib (- n 1))
       (fib (- n 2)))))

```



### Iterative



```lisp

(defun fib
  ((n) (when (>= n 0))
    (fib n 0 1)))

(defun fib
  ((0 result _)
    result)
  ((n result next)
    (fib (- n 1) next (+ result next))))


```



## Liberty BASIC


### Iterative/Recursive


```lb

for i = 0 to 15
    print fiboR(i),fiboI(i)
next i

function fiboR(n)
    if n <= 1 then
        fiboR = n
    else
        fiboR = fiboR(n-1) + fiboR(n-2)
    end if
end function

function fiboI(n)
    a = 0
    b = 1
    for i = 1 to n
        temp = a + b
        a = b
        b = temp
    next i
    fiboI = a
end function

```

```txt

0             0
1             1
1             1
2             2
3             3
5             5
8             8
13            13
21            21
34            34
55            55
89            89
144           144
233           233
377           377
610           610

```


### Iterative/Negative


```lb

print "Rosetta Code - Fibonacci sequence": print
print "  n             Fn"
for x=-12 to 12 '68 max
    print using("### ", x); using("##############", FibonacciTerm(x))
next x
print
[start]
input "Enter a term#: "; n$
n$=lower$(trim$(n$))
if n$="" then print "Program complete.": end
print FibonacciTerm(val(n$))
goto [start]

function FibonacciTerm(n)
    n=int(n)
    FTa=0: FTb=1: FTc=-1
    select case
        case n=0  : FibonacciTerm=0  : exit function
        case n=1  : FibonacciTerm=1  : exit function
        case n=-1 : FibonacciTerm=-1 : exit function
        case n>1
            for x=2 to n
                FibonacciTerm=FTa+FTb
                FTa=FTb: FTb=FibonacciTerm
            next x
            exit function
        case n<-1
            for x=-2 to n step -1
                FibonacciTerm=FTa+FTc
                FTa=FTc: FTc=FibonacciTerm
            next x
            exit function
    end select
end function

```

```txt

Rosetta Code - Fibonacci sequence

  n             Fn
-12           -144
-11            -89
-10            -55
 -9            -34
 -8            -21
 -7            -13
 -6             -8
 -5             -5
 -4             -3
 -3             -2
 -2             -1
 -1             -1
  0              0
  1              1
  2              1
  3              2
  4              3
  5              5
  6              8
  7             13
  8             21
  9             34
 10             55
 11             89
 12            144

Enter a term#: 12
144
Enter a term#:
Program complete.

```



## Lingo



### Recursive



```lingo
on fib (n)
  if n<2 then return n
  return fib(n-1)+fib(n-2)
end
```



### Iterative



```lingo
on fib (n)
  if n<2 then return n
  fibPrev = 0
  fib = 1
  repeat with i = 2 to n
    tmp = fib
    fib = fib + fibPrev
    fibPrev = tmp
  end repeat
  return fib
end
```



### Analytic



```lingo
on fib (n)
  sqrt5 = sqrt(5.0)
  p = (1+sqrt5)/2
  q = 1 - p
  return integer((power(p,n)-power(q,n))/sqrt5)
end
```



## Lisaac


```Lisaac
- fib(n : UINTEGER_32) : UINTEGER_64 <- (
  + result : UINTEGER_64;
  (n < 2).if {
    result := n;
  } else {
    result := fib(n - 1) + fib(n - 2);
  };
  result
);
```



## LiveCode


```LiveCode
-- Iterative, translation of the basic version.
function fibi n
    put 0 into aa
    put 1 into b
    repeat with i = 1 to n
        put aa + b into temp
        put b into aa
        put temp into b
    end repeat
    return aa
end fibi

-- Recursive
function fibr n
     if n <= 1 then
        return n
    else
        return fibr(n-1) + fibr(n-2)
    end if
end fibr
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

$"PRINT_LONG" = comdat any
@"PRINT_LONG" = linkonce_odr unnamed_addr constant [5 x i8] c"%ld\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

;--------------------------------------------------------------------
;-- Function for calculating the nth fibonacci numbers
;--------------------------------------------------------------------
define i32 @fibonacci(i32) {
    %2 = alloca i32, align 4            ;-- allocate local copy of n
    %3 = alloca i32, align 4            ;-- allocate a
    %4 = alloca i32, align 4            ;-- allocate b
    store i32 %0, i32* %2, align 4      ;-- store copy of n
    store i32 0, i32* %3, align 4       ;-- a := 0
    store i32 1, i32* %4, align 4       ;-- b := 1
    br label %loop

loop:
    %5 = load i32, i32* %2, align 4     ;-- load n
    %6 = icmp sgt i32 %5, 0             ;-- n > 0
    br i1 %6, label %loop_body, label %exit

loop_body:
    %7 = load i32, i32* %3, align 4     ;-- load a
    %8 = load i32, i32* %4, align 4     ;-- load b
    %9 = add nsw i32 %7, %8             ;-- t = a + b
    store i32 %8, i32* %3, align 4      ;-- store a = b
    store i32 %9, i32* %4, align 4      ;-- store b = t
    %10 = load i32, i32* %2, align 4    ;-- load n
    %11 = add nsw i32 %10, -1           ;-- decrement n
    store i32 %11, i32* %2, align 4     ;-- store n
    br label %loop

exit:
    %12 = load i32, i32* %3, align 4    ;-- load a
    ret i32 %12                         ;-- return a
}

;--------------------------------------------------------------------
;-- Main function for printing successive fibonacci numbers
;--------------------------------------------------------------------
define i32 @main() {
    %1 = alloca i32, align 4            ;-- allocate index
    store i32 0, i32* %1, align 4       ;-- index := 0
    br label %loop

loop:
    %2 = load i32, i32* %1, align 4     ;-- load index
    %3 = icmp sle i32 %2, 12            ;-- index <= 12
    br i1 %3, label %loop_body, label %exit

loop_body:
    %4 = load i32, i32* %1, align 4     ;-- load index
    %5 = call i32 @fibonacci(i32 %4)
    %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"PRINT_LONG", i32 0, i32 0), i32 %5)
    %7 = load i32, i32* %1, align 4     ;-- load index
    %8 = add nsw i32 %7, 1              ;-- increment index
    store i32 %8, i32* %1, align 4      ;-- store index
    br label %loop

exit:
    ret i32 0                           ;-- return EXIT_SUCCESS
}
```

```txt
0
1
1
2
3
5
8
13
21
34
55
89
144
```



## Logo


```logo
to fib :n [:a 0] [:b 1]
  if :n < 1 [output :a]
  output (fib :n-1 :b :a+:b)
end
```



## LOLCODE


```LOLCODE

HAI 1.2
HOW DUZ I fibonacci YR N
  EITHER OF BOTH SAEM N AN 1 AN BOTH SAEM N AN 0
  O RLY?
    YA RLY, FOUND YR 1
    NO WAI
      I HAS A N1
      I HAS A N2
      N1 R DIFF OF N AN 1
      N2 R DIFF OF N AN 2
      N1 R fibonacci N1
      N2 R fibonacci N2
      FOUND YR SUM OF N1 AN N2
  OIC
IF U SAY SO
KTHXBYE

```



## Lua


### Recursive


```lua

--calculates the nth fibonacci number. Breaks for negative or non-integer n.
function fibs(n)
  return n < 2 and n or fibs(n - 1) + fibs(n - 2)
end

```



### Pedantic Recursive


```lua

--more pedantic version, returns 0 for non-integer n
function pfibs(n)
  if n ~= math.floor(n) then return 0
  elseif n < 0 then return pfibs(n + 2) - pfibs(n + 1)
  elseif n < 2 then return n
  else return pfibs(n - 1) + pfibs(n - 2)
  end
end

```



### Tail Recursive


```lua

function a(n,u,s) if n<2 then return u+s end return a(n-1,u+s,u) end
function trfib(i) return a(i-1,1,0) end

```



### Table Recursive


```lua

fib_n = setmetatable({1, 1}, {__index = function(z,n) return n<=0 and 0 or z[n-1] + z[n-2] end})

```



### Table Recursive 2


```lua

-- table recursive done properly (values are actually saved into table;
-- also the first element of Fibonacci sequence is 0, so the initial table should be {0, 1}).
fib_n = setmetatable({0, 1}, {
  __index = function(t,n)
    if n <= 0 then return 0 end
    t[n] = t[n-1] + t[n-2]
    return t[n]
  end
})

```



### Iterative


```lua

function ifibs(n)
  local p0,p1=0,1
  for _=1,n do p0,p1 = p1,p0+p1 end
  return p0
end

```



## Luck


```luck
function fib(x: int): int = (
   let cache = {} in
   let fibc x = if x<=1 then x else (
      if x not in cache then
      cache[x] = fibc(x-1) + fibc(x-2);
      cache[x]
   ) in fibc(x)
);;
for x in range(10) do print(fib(x))
```



## Lush


```lush
(de fib-rec (n)
  (if (< n 2)
      n
     (+ (fib-rec (- n 2)) (fib-rec (- n 1)))))
```



## LSL

Rez a box on the ground, and add the following as a New Script.

```LSL
integer Fibonacci(integer n) {
	if(n<2) {
		return n;
	} else {
		return Fibonacci(n-1)+Fibonacci(n-2);
	}
}
default {
	state_entry() {
		integer x = 0;
		for(x=0 ; x<35 ; x++) {
			llOwnerSay("Fibonacci("+(string)x+")="+(string)Fibonacci(x));
		}
	}
}
```

Output:

```txt

Fibonacci(0)=0
Fibonacci(1)=1
Fibonacci(2)=1
Fibonacci(3)=2
Fibonacci(4)=3
Fibonacci(5)=5
Fibonacci(6)=8
Fibonacci(7)=13
Fibonacci(8)=21
Fibonacci(9)=34
Fibonacci(10)=55
Fibonacci(11)=89
Fibonacci(12)=144
Fibonacci(13)=233
Fibonacci(14)=377
Fibonacci(15)=610
Fibonacci(16)=987
Fibonacci(17)=1597
Fibonacci(18)=2584
Fibonacci(19)=4181
Fibonacci(20)=6765
Fibonacci(21)=10946
Fibonacci(22)=17711
Fibonacci(23)=28657
Fibonacci(24)=46368
Fibonacci(25)=75025
Fibonacci(26)=121393
Fibonacci(27)=196418
Fibonacci(28)=317811
Fibonacci(29)=514229
Fibonacci(30)=832040
Fibonacci(31)=1346269
Fibonacci(32)=2178309
Fibonacci(33)=3524578
Fibonacci(34)=5702887

```



## M2000 Interpreter

Return decimal type and use an Inventory (as closure) to store known return values. All closures are in scope in every recursive call (we use here lambda(), but we can use fib(), If we make Fib1=fib then we have to use lambda() for recursion.

```M2000 Interpreter

Inventory K=0:=0,1:=1
fib=Lambda K (x as decimal)-> {
      If Exist(K, x) Then =Eval(K) :Exit
      Def Ret as Decimal
      Ret=If(x>1->Lambda(x-1)+Lambda(x-2), x)
      Append K, x:=Ret
      =Ret
}
\\ maximum 139
For i=1 to 139 {
      Print Fib(i)
}

```


Here an example where we use a BigNum class to make a Group which hold a stack of values, and take 14 digits per item in stack. We can use inventory to hold groups, so we use the fast fib() function from code above, where we remove the type definition of Ret variable, and set two first items in inventory as groups.


```M2000 Interpreter


Class BigNum {
      a=stack
      Function Digits {
            =len(.a)*14-(14-len(str$(stackitem(.a,len(.a)) ,"")))
      }
      Operator "+" (n) {
            \\ we get a copy, but .a is pointer
             \\ we make a copy, and get a new pointer
            .a<=stack(.a)
            acc=0
            carry=0
            const d=100000000000000@
                  k=min.data(Len(.a), len(n.a))
                  i=each(.a, 1,k )
                  j=each(n.a, 1,k)
                  while  i, j {
                        acc=stackitem(i)+stackitem(j)+carry
                        carry= acc div d
                        return .a, i^+1:=acc mod d
                  }
                  if len(.a)<len(n.a) Then  {
                        i=each(n.a, k+1, -1)
                        while i {
                              acc=stackitem(i)+carry
                              carry= acc div d
                              stack .a  {data acc mod d}
                        }
                  } ELse.if len(.a)>len(n.a) Then  {
                        i=each(.a, k+1, -1)
                        while i {
                              acc=stackitem(i)+carry
                              carry= acc div d
                              Return .a, i^+1:=acc mod d
                              if carry else exit
                        }
                  }
                  if carry then stack .a { data carry}
      }
      Function tostring$ {
            if len(.a)=0 then ="0" : Exit
            if len(.a)=1 then =str$(Stackitem(.a),"") : Exit
            document buf$=str$(Stackitem(.a, len(.a)),"")
            for i=len(.a)-1 to  1 {
                  Stack .a {
                        buf$=str$(StackItem(i), "00000000000000")
                  }
            }
            =buf$
      }
      class:
      Module BigNum (s$) {
            s$=filter$(s$,"+-.,")
            if s$<>""  Then {
                  repeat {
                        If len(s$)<14 then Stack .a { Data  val(s$) }: Exit
                        Stack .a { Data  val(Right$(s$, 14)) }
                        S$=Left$(S$, len(S$)-14)
                  } Until S$=""
            }
      }
}

Inventory K=0:=BigNum("0"),1:=BigNum("1")
fib=Lambda K (x as decimal)-> {
      If Exist(K, x) Then =Eval(K) :Exit
      Ret=If(x>1->Lambda(x-1)+Lambda(x-2), bignum(str$(x,"")))
      Append K, x:=Ret
      =Ret
}
\\ Using this to handle form  refresh by code
Set Fast!
For i=1 to 4000 {
      N=Fib(i)
      Print i
      Print N.tostring$()
      Refresh
}



```



## M4


```m4
define(`fibo',`ifelse(0,$1,0,`ifelse(1,$1,1,
`eval(fibo(decr($1)) + fibo(decr(decr($1))))')')')dnl
define(`loop',`ifelse($1,$2,,`$3($1) loop(incr($1),$2,`$3')')')dnl
loop(0,15,`fibo')
```



## Maple


```maple

> f := n -> ifelse(n<3,1,f(n-1)+f(n-2));
> f(2);
  1
> f(3);
  2

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The Wolfram Language already has a built-in function <tt>Fibonacci</tt>, but a simple recursive implementation would be


```mathematica
fib[0] = 0
fib[1] = 1
fib[n_Integer] := fib[n - 1] + fib[n - 2]
```


An optimization is to cache the values already calculated:


```mathematica
fib[0] = 0
fib[1] = 1
fib[n_Integer] := fib[n] = fib[n - 1] + fib[n - 2]
```


The above implementations may be too simplistic, as the first is incredibly slow for any reasonable range due to nested recursions and while the second is faster it uses an increasing amount of memory.  The following uses recursion much more effectively while not using memory:


```mathematica
fibi[prvprv_Integer, prv_Integer, rm_Integer] :=
  If[rm < 1, prvprv, fibi[prv, prvprv + prv, rm - 1]]
fib[n_Integer] := fibi[0, 1, n]
```


However, the recursive approaches in Mathematica are limited by the limit set for recursion depth (default 1024 or 4096 for the above cases), limiting the range for 'n' to about 1000 or 2000.  The following using an iterative approach has an extremely high limit (greater than a million):


```mathematica
fib[n_Integer] := Block[{tmp, prvprv = 0, prv = 1},
  For[i = 0, i < n, i++, tmp = prv; prv += prvprv; prvprv = tmp];
  Return[prvprv]]
```


If one wanted a list of Fibonacci numbers, the following is quite efficient:


```mathematica
fibi[{prvprv_Integer, prv_Integer}] := {prv, prvprv + prv}
fibList[n_Integer] := Map[Take[#, 1] &, NestList[fibi, {0, 1}, n]] // Flatten
```


Output from the last with "fibList[100]":


```mathematica
{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, \
1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, \
196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, \
9227465, 14930352, 24157817, 39088169, 63245986, 102334155, \
165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903, \
2971215073, 4807526976, 7778742049, 12586269025, 20365011074, \
32951280099, 53316291173, 86267571272, 139583862445, 225851433717, \
365435296162, 591286729879, 956722026041, 1548008755920, \
2504730781961, 4052739537881, 6557470319842, 10610209857723, \
17167680177565, 27777890035288, 44945570212853, 72723460248141, \
117669030460994, 190392490709135, 308061521170129, 498454011879264, \
806515533049393, 1304969544928657, 2111485077978050, \
3416454622906707, 5527939700884757, 8944394323791464, \
14472334024676221, 23416728348467685, 37889062373143906, \
61305790721611591, 99194853094755497, 160500643816367088, \
259695496911122585, 420196140727489673, 679891637638612258, \
1100087778366101931, 1779979416004714189, 2880067194370816120, \
4660046610375530309, 7540113804746346429, 12200160415121876738, \
19740274219868223167, 31940434634990099905, 51680708854858323072, \
83621143489848422977, 135301852344706746049, 218922995834555169026, \
354224848179261915075}
```


The Wolfram Language can also solve recurrence equations using the built-in function <tt>RSolve</tt>


```mathematica
fib[n] /. RSolve[{fib[n] == fib[n - 1] + fib[n - 2], fib[0] == 0,
    fib[1] == 1}, fib[n], n][[1]]
```


which evaluates to the built-in function <tt>Fibonacci[n]</tt>

This function can also be expressed as


```mathematica
Fibonacci[n] // FunctionExpand // FullSimplify
```


which evaluates to


```mathematica
(2^-n ((1 + Sqrt[5])^n - (-1 + Sqrt[5])^n Cos[n π]))/Sqrt[5]
```


and is defined for all real or complex values of n.


## MATLAB



### Matrix

```MATLAB
function f = fib(n)

	f = [1 1 ; 1 0]^(n-1);
	f = f(1,1);

end
```



### Iterative


```MATLAB
function F = fibonacci(n)

    Fn = [1 0]; %Fn(1) is F_{n-2}, Fn(2) is F_{n-1}
    F = 0; %F is F_{n}

    for i = (1:abs(n))
        Fn(2) = F;
        F = sum(Fn);
        Fn(1) = Fn(2);
    end

    if n < 0
        F = F*((-1)^(n+1));
    end

end
```



### Dramadah Matrix Method

The MATLAB help file suggests an interesting method of generating the Fibonacci numbers. Apparently the determinate of the Dramadah Matrix of type 3 (MATLAB designation) and size n-by-n is the nth Fibonacci number. This method is implimented below.


```MATLAB
function number = fibonacci2(n)

    if n == 1
        number = 1;
    elseif n == 0
        number = 0;
    elseif n < 0
        number = ((-1)^(n+1))*fibonacci2(-n);;
    else
        number = det(gallery('dramadah',n,3));
    end

end
```



### Tartaglia/Pascal Triangle Method


```Matlab

function number = fibonacci(n)
%construct the Tartaglia/Pascal Triangle
    pt=tril(ones(n));
    for r = 3 : n
    % Every element is the addition of the two elements
    % on top of it. That means the previous row.
        for c = 2 : r-1
            pt(r, c) = pt(r-1, c-1) + pt(r-1, c);
        end
    end
    number=trace(rot90(pt));
end

```



## Maxima


```maxima
/* fib(n) is built-in; here is an implementation */
fib2(n) := (matrix([0, 1], [1, 1])^^n)[1, 2]$

fib2(100)-fib(100);
0

fib2(-10);
-55
```



## MAXScript


### Iterative


```maxscript
fn fibIter n =
(
    if n < 2 then
    (
        n
    )
    else
    (
        fib = 1
        fibPrev = 1
        for num in 3 to n do
        (
            temp = fib
            fib += fibPrev
            fibPrev = temp
        )
        fib
    )
)
```


### Recursive


```maxscript
fn fibRec n =
(
    if n < 2 then
    (
        n
    )
    else
    (
        fibRec (n - 1) + fibRec (n - 2)
    )
)
```



## Mercury

Mercury is both a logic language and a functional language.  As such there are two possible interfaces for calculating a Fibonacci number.  This code shows both styles.  Note that much of the code here is ceremony put in place to have this be something which can actually compile.  The actual Fibonacci number generation is contained in the predicate <code>fib/2</code> and in the function <code>fib/1</code>.  The predicate <code>main/2</code> illustrates first the unification semantics of the predicate form and the function call semantics of the function form.

The provided code uses a very naive form of generating a Fibonacci number.  A more realistic implementation would use memoization to cache previous results, exchanging time for space.  Also, in the case of supplying both a function implementation and a predicate implementation, one of the two would be implemented in terms of the other.  Examples of this are given as comments below.


### fib.m


```mercury

% The following code is derived from the Mercury Tutorial by Ralph Becket.
% http://www.mercury.csse.unimelb.edu.au/information/papers/book.pdf
:- module fib.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- pred fib(int::in, int::out) is det.
fib(N, X) :-
    ( if N =< 2
          then X = 1
          else fib(N - 1, A), fib(N - 2, B), X = A + B ).

:- func fib(int) = int is det.
fib(N) = X :- fib(N, X).

main(!IO) :-
    fib(40, X),
    write_string("fib(40, ", !IO),
    write_int(X, !IO),
    write_string(")\n", !IO),

    write_string("fib(40) = ", !IO),
    write_int(fib(40), !IO),
    write_string("\n", !IO).

```



###  Iterative algorithm


The much faster iterative algorithm can be written as:


```mercury

:- pred fib_acc(int::in, int::in, int::in, int::in, int::out) is det.

fib_acc(N, Limit, Prev2, Prev1, Res) :-
    ( N < Limit ->
        % limit not reached, continue computation.
        ( N =< 2 ->
            Res0 = 1
        ;
            Res0 = Prev2 + Prev1
        ),
        fib_acc(N+1, Limit, Prev1, Res0, Res)
    ;
        % Limit reached, return the sum of the two previous results.
        Res = Prev2 + Prev1
    ).

```


This predicate can be called as
```mercury
fib_acc(1, 40, 1, 1, Result)
```

It has several inputs which form the loop, the first is the current number, the second is a limit, ie when to stop counting.  And the next two are accumulators for the last and next-to-last results.


### Memoization


But what if you want the speed of the fib_acc with the recursive (more declarative) definition of fib?  Then use memoization, because Mercury is a pure language fib(N, F) will always give the same F for the same N, guaranteed.  Therefore memoization asks the compiler to use a table to remember the value for F for any N, and it's a one line change:


```mercury

:- pragma memo(fib/2).
:- pred fib(int::in, int::out) is det.
fib(N, X) :-
    ( if N =< 2
          then X = 1
          else fib(N - 1, A), fib(N - 2, B), X = A + B ).

```


We've shown the definition of fib/2 again, but the only change here is the memoization pragma (see the reference manual).  This is not part of the language specification and different Mercury implementations are allowed to ignore it, however there is only one implementation so in practice memoization is fully supported.

Memoization trades speed for space, a table of results is constructed and kept in memory.  So this version of fib consumes more memory than than fib_acc.  It is also slightly slower than fib_acc since it must manage its table of results but it is much much faster than without memoization.  Memoization works very well for the Fibonacci sequence because in the naive version the same results are calculated over and over again.


## Metafont


```metafont
vardef fibo(expr n) =
if n=0: 0
elseif n=1: 1
else:
  fibo(n-1) + fibo(n-2)
fi
enddef;

for i=0 upto 10: show fibo(i); endfor
end
```



## Microsoft Small Basic


### Iterative


```smallbasic
' Fibonacci sequence - 31/07/2018
  n = 139
  f1 = 0
  f2 = 1
  TextWindow.WriteLine("fibo(0)="+f1)
  TextWindow.WriteLine("fibo(1)="+f2)
  For i = 2 To n
    f3 = f1 + f2
    TextWindow.WriteLine("fibo("+i+")="+f3)
    f1 = f2
    f2 = f3
  EndFor
```

```txt

fibo(139)=50095301248058391139327916261

```

===Binet's Formula===

```smallbasic
' Fibonacci sequence - Binet's Formula - 31/07/2018
  n = 69
  sq5=Math.SquareRoot(5)
  phi1=(1+sq5)/2
  phi2=(1-sq5)/2
  phi1n=phi1
  phi2n=phi2
  For i = 2 To n
    phi1n=phi1n*phi1
    phi2n=phi2n*phi2
    TextWindow.Write(Math.Floor((phi1n-phi2n)/sq5)+" ")
  EndFor
```

```txt

1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155 165580141 267914296 433494437 701408733 1134903170 1836311903 2971215073 4807526976 7778742049 12586269025 20365011074 32951280099 53316291173 86267571272 139583862445 225851433717 365435296162 591286729879 956722026041 1548008755920 2504730781961 4052739537881 6557470319842 10610209857723 17167680177565 27777890035288 44945570212853 72723460248141 117669030460994

```



## min

```min
(
  (2 <)
  ((0 1 (dup rollup +)) dip pred times nip)
  unless
) :fib
```



## MiniScript


```MiniScript
fibonacci = function(n)
    if n < 2 then return n
    ans = 0
    n1 = 0
    n2 = 1
    for i in range(n-1, 1)
        ans = n1 + n2
        n1 = n2
        n2 = ans
    end for
    return ans
end function

print fibonacci(6)
```



## Mirah


```mirah
def fibonacci(n:int)
    return n if n < 2
    fibPrev = 1
    fib = 1
    3.upto(Math.abs(n)) do
        oldFib = fib
        fib = fib + fibPrev
        fibPrev = oldFib
    end
    fib * (n<0 ? int(Math.pow(n+1, -1)) : 1)
end

puts fibonacci 1
puts fibonacci 2
puts fibonacci 3
puts fibonacci 4
puts fibonacci 5
puts fibonacci 6
puts fibonacci 7

```



## MIPS Assembly


This is the iterative approach to the Fibonacci sequence.

```MIPS

	.text
main:	li	$v0, 5		# read integer from input. The read integer will be stroed in $v0
	syscall

	beq	$v0, 0, is1
	beq	$v0, 1,	is1

	li	$s4, 1		# the counter which has to equal to $v0

	li	$s0, 1
	li	$s1, 1

loop:	add	$s2, $s0, $s1
	addi	$s4, $s4, 1
	beq	$v0, $s4, iss2

	add	$s0, $s1, $s2
	addi	$s4, $s4, 1
	beq	$v0, $s4, iss0

	add	$s1, $s2, $s0
	addi	$s4, $s4, 1
	beq	$v0, $s4, iss1

	b 	loop

iss0:	move	$a0, $s0
	b	print

iss1:	move	$a0, $s1
	b	print

iss2:	move	$a0, $s2
	b	print


is1:	li	$a0, 1
	b 	print

print:	li	$v0, 1
	syscall
	li	$v0, 10
	syscall

```


=={{header|MK-61/52}}==
<lang>П0	1	lg	Вx	<->	+	L0	03	С/П	БП
03
```


Instruction: ''n'' В/О С/П, where ''n'' is serial number of the number of Fibonacci sequence; С/П for the following numbers.


## ML

=
## Standard ML
=

### =Recursion=

This version is tail recursive.

```sml
fun fib n =
    let
	fun fib' (0,a,b) = a
	  | fib' (n,a,b) = fib' (n-1,a+b,a)
    in
	fib' (n,0,1)
    end
```

=
## MLite
=

### =Recursion=

Tail recursive.

```ocaml
fun fib
        (0, x1, x2) = x2
      | (n, x1, x2) = fib (n-1, x2, x1+x2)
      | n = fib (n, 0, 1)
```



## ML/I


```ML/I
MCSKIP "WITH" NL
"" Fibonacci - recursive
MCSKIP MT,<>
MCINS %.
MCDEF FIB WITHS ()
AS <MCSET T1=%A1.
MCGO L1 UNLESS 2 GR T1
%T1.<>MCGO L0
%L1.%FIB(%T1.-1)+FIB(%T1.-2).>
fib(0) is FIB(0)
fib(1) is FIB(1)
fib(2) is FIB(2)
fib(3) is FIB(3)
fib(4) is FIB(4)
fib(5) is FIB(5)
```


=={{header|Modula-2}}==

```modula2
MODULE Fibonacci;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Fibonacci(n : LONGINT) : LONGINT;
VAR
    a,b,c : LONGINT;
BEGIN
    IF n<0 THEN RETURN 0 END;

    a:=1;
    b:=1;

    WHILE n>0 DO
        c := a + b;
        a := b;
        b := c;
        DEC(n)
    END;

    RETURN a
END Fibonacci;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i : INTEGER;
    r : LONGINT;
BEGIN
    FOR i:=0 TO 10 DO
        r := Fibonacci(i);

        FormatString("%l\n", buf, r);
        WriteString(buf);
    END;

    ReadChar
END Fibonacci.
```


=={{header|Modula-3}}==

### Recursive


```modula3
PROCEDURE Fib(n: INTEGER): INTEGER =
  BEGIN
    IF n < 2 THEN
      RETURN n;
    ELSE
      RETURN Fib(n-1) + Fib(n-2);
    END;
  END Fib;
```


=== Iterative (with negatives) ===


```modula3
PROCEDURE IterFib(n: INTEGER): INTEGER =

VAR

  limit := ABS(n);
  prev := 0;
  curr, next: INTEGER;

BEGIN

  (* trivial case *)
  IF n = 0 THEN RETURN 0; END;

  IF n > 0 THEN (* positive case *)

    curr := 1;
    FOR i := 2 TO limit DO
      next := prev + curr;
      prev := curr;
      curr := next;
    END;

  ELSE (* negative case *)

    curr := -1;
    FOR i := 2 TO limit DO
      next := prev - curr;
      prev := curr;
      curr := next;
    END;

  END;

  RETURN curr;

END IterFib;
```



## Monicelli

Recursive version. It includes a main that reads a number N from standard input and prints the Nth Fibonacci number.

```monicelli

# Main
Lei ha clacsonato
voglio un nonnulla, Necchi mi porga un nonnulla
il nonnulla come se fosse brematurata la supercazzola bonaccia con il nonnulla o scherziamo?
un nonnulla a posterdati

# Fibonacci function 'bonaccia'
blinda la supercazzola Necchi bonaccia con antani Necchi o scherziamo? che cos'è l'antani?
minore di 3: vaffanzum 1! o tarapia tapioco: voglio unchiamo, Necchi come se fosse brematurata
la supercazzola bonaccia con antani meno 1 o scherziamo? voglio duechiamo,
Necchi come se fosse brematurata la supercazzola bonaccia con antani meno 2 o scherziamo? vaffanzum
unchiamo più duechiamo! e velocità di esecuzione

```



## MontiLang

Reads number from standard input and prints to that number in the fibonacci sequence

```MontiLang
0 VAR a .
1 VAR b .
INPUT TOINT
FOR :
    a b + VAR c .
    a PRINT .
    b VAR a .
    c VAR b .
ENDFOR
```


Forth-style solution


```MontiLang
def over
    swap dup rot swap
enddef

|Enter a number to obtain Fibonacci sequence: | input nip var count .
0 1
FOR count
    over out |, | out . + swap
ENDFOR
. print
input
clear
```


Simpler


```MontiLang
|Enter a number to obtain Fibonacci sequence: | input nip 1 - var count .
0 1
FOR count
    out |, | out . dup rot +
ENDFOR
print
input   /# wait until press ENTER #/
clear   /# empties the stack #/
```



## MUMPS


### Iterative


```MUMPS
FIBOITER(N)
 ;Iterative version to get the Nth Fibonacci number
 ;N must be a positive integer
 ;F is the tree containing the values
 ;I is a loop variable.
 QUIT:(N\1'=N)!(N<0) "Error: "_N_" is not a positive integer."
 NEW F,I
 SET F(0)=0,F(1)=1
 QUIT:N<2 F(N)
 FOR I=2:1:N SET F(I)=F(I-1)+F(I-2)
 QUIT F(N)
```



```txt

USER>W $$FIBOITER^ROSETTA(30)
832040

```



## Nanoquery


### Iterative


```nanoquery
def fibIter($n)
        if ($n < 2)
                return $n
        end if

        $fib = 1
        $fibPrev = 1

        for ($num = 2) ($num < $n) ($num = $num+1)
                $fib = ($fib + $fibPrev)
                $fibPrev = ($fib - $fibPrev)
        end for

        return $fib
end fibIter
```



## Nemerle


### Recursive


```Nemerle
using System;
using System.Console;

module Fibonacci
{
    Fibonacci(x : long) : long
    {
        |x when x < 2 => x
        |_ => Fibonacci(x - 1) + Fibonacci(x - 2)
    }

    Main() : void
    {
        def num = Int64.Parse(ReadLine());
        foreach (n in $[0 .. num])
            WriteLine("{0}: {1}", n, Fibonacci(n));
    }
}
```


### Tail Recursive


```Nemerle
Fibonacci(x : long, current : long, next : long) : long
{
    match(x)
    {
        |0 => current
        |_ => Fibonacci(x - 1, next, current + next)
    }
}

Fibonacci(x : long) : long
{
    Fibonacci(x, 0, 1)
}
```



## NESL


### Recursive


```nesl
function fib(n) = if n < 2 then n else fib(n - 2) + fib(n - 1);
```



## NetRexx

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

numeric digits 210000                  /*prepare for some big 'uns.     */
parse arg x y .                        /*allow a single number or range.*/
if x == '' then do                     /*no input? Then assume -30-->+30*/
  x = -30
  y = -x
  end

if y == '' then y = x             /*if only one number, show fib(n)*/
loop k = x to y                   /*process each Fibonacci request.*/
  q = fib(k)
  w = q.length                    /*if wider than 25 bytes, tell it*/
  say 'Fibonacci' k"="q
  if w > 25 then say 'Fibonacci' k "has a length of" w
  end k
exit

/*-------------------------------------FIB subroutine (non-recursive)---*/
method fib(arg) private static
  parse arg n
  na = n.abs

  if na < 2 then return na             /*handle special cases.          */
  a = 0
  b = 1

  loop j = 2 to na
    s = a + b
    a = b
    b = s
    end j

  if n > 0 | na // 2 == 1 then return  s /*if positive or odd negative... */
                          else return -s /*return a negative Fib number.  */

```



## NewLISP


### Iterative


```newLISP
(define (fibonacci n)
    (let (L '(0 1))
        (dotimes (i n)
            (setq L (list (L 1) (apply + L))))
        (L 1)) )

```



### Recursive


```newLISP
(define (fibonacci n)
(if (< n 2) 1
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

```



### Matrix multiplication


```newLISP
(define (fibonacci n)
  (letn (f '((0 1) (1 1)) fib f)
    (dotimes (i n)
        (set 'fib (multiply fib f)))
    (fib 0 1)) )

(print(fibonacci 10)) ;;89
```



## NGS


### Iterative

```NGS
F fib(n:Int) {
	n < 2 returns n
	local a = 1, b = 1
	# i is automatically local because of for()
	for(i=2; i<n; i=i+1) {
		local next = a + b
		a = b
		b = next
	}
	b
}
```




## Nial


### Iterative

On my machine, about 1.7s for 100,000 iterations, n=92.
Maybe a few percent faster than iterative Python.
Note that n>92 produces overflow; Python keeps going -
single iteration with n=1,000,000 takes it about 15s.


```nial
fibi is op n {
  if n<2 then
    n
  else
    x1:=0; x2:=1;
    for i with tell (n - 1) do
      x:=x1+x2;
      x1:=x2;
      x2:=x;
    endfor;
    x2
  endif};
```


Iterative using fold.  Slightly faster, <1.6s:


```nial
fibf is op n {1 pick ((n- 1) fold [1 pick, +] 0 1)};
```


Tacit verion of above.  Slightly faster still, <1.4s:


```nial
fibf2 is 1 pick fold [1 pick, +] reverse (0 1 hitch) (-1+);
```



### Recursive

Really slow (over 8s for single iteration, n=33).
(Similar to time for recursive python version with n=37.)


```nial
fibr is op n {fork [2>, +, + [fibr (-1 +), fibr (-2 +)]] n};
```


...or tacit version. More than twice as fast (?) but still slow:


```nial>fibr2 is fork [2
, +, + [fibr2 (-1 +), fibr2 (-2 +)]];
```



### Matrix

Matrix inner product (ip).
This appears to be the fastest, about 1.0s for 100,000 iterations, n=92:
Note that n>92 produces negative result.


```nial
fibm is op n {floor (0 1 pick (reduce ip (n reshape [2 2 reshape 1 1 1 0])))};
```


Could it look a little more like J?
(Maybe 5% slower than above.)


```nial
$ is reshape;
~ is tr f op a b {b f a}; % Goes before verb, rather than after like in J;
_ is floor; % Not really J, but J-ish? (Cannot redefine "<.".);

fibm2 is _(0 1 pick reduce ip([2 2$1 1 1 0](~$)));
```


Alternate, not involving replicating matrix n times, but maybe 50% slower
than the fastest matrix version above - similar speed to iterative:


```nial
fibm3 is op n {a:=2 2$1 1 1 0; _(0 1 pick ((n- 1) fold (a ip) a))};
```



## Nim


### Analytic


```nim
proc Fibonacci(n: int): int64 =
  var fn = float64(n)
  var p: float64 = (1.0 + sqrt(5.0)) / 2.0
  var q: float64 = 1.0 / p
  return int64((pow(p, fn) + pow(q, fn)) / sqrt(5.0))
```



### Iterative


```nim
proc Fibonacci(n: int): int =
  var
    first = 0
    second = 1

  for i in 0 .. <n:
    swap first, second
    second += first

  result = first
```



### Recursive


```nim
proc Fibonacci(n: int): int64 =
  if n <= 2:
    result = 1
  else:
    result = Fibonacci(n - 1) + Fibonacci(n - 2)
```


===Tail-recursive===

```nim
proc Fibonacci(n: int, current: int64, next: int64): int64 =
  if n == 0:
    result = current
  else:
    result = Fibonacci(n - 1, next, current + next)
proc Fibonacci(n: int): int64 =
  result = Fibonacci(n, 0, 1)
```



### Continuations


```nim
iterator fib: int {.closure.} =
  var a = 0
  var b = 1
  while true:
    yield a
    swap a, b
    b = a + b

var f = fib
for i in 0.. <10:
  echo f()
```


=={{header|Oberon-2}}==
```oberon2

MODULE Fibonacci;
IMPORT
  Out := NPCT:Console;

PROCEDURE Fibs(VAR r: ARRAY OF LONGREAL);
VAR
  i: LONGINT;
BEGIN
  r[0] := 1.0; r[1] := 1.0;
  FOR i := 2 TO LEN(r) - 1 DO
    r[i] := r[i - 2] + r[i - 1];
  END
END Fibs;

PROCEDURE FibsR(n: LONGREAL): LONGREAL;
BEGIN
  IF n < 2. THEN
    RETURN n
  ELSE
    RETURN FibsR(n - 1) + FibsR(n - 2)
  END
END FibsR;

PROCEDURE Show(r: ARRAY OF LONGREAL);
VAR
  i: LONGINT;
BEGIN
  Out.String("First ");Out.Int(LEN(r),0);Out.String(" Fibonacci numbers");Out.Ln;
  FOR i := 0 TO LEN(r) - 1 DO
    Out.LongRealFix(r[i],8,0)
  END;
  Out.Ln
END Show;

PROCEDURE Gen(s: LONGINT);
VAR
  x: POINTER TO ARRAY OF LONGREAL;
BEGIN
  NEW(x,s);
  Fibs(x^);
  Show(x^)
END Gen;

PROCEDURE GenR(s: LONGINT);
VAR
  i: LONGINT;
BEGIN
  Out.String("First ");Out.Int(s,0);Out.String(" Fibonacci numbers (Recursive)");Out.Ln;
  FOR i := 1 TO s DO
    Out.LongRealFix(FibsR(i),8,0)
  END;
  Out.Ln
END GenR;

BEGIN
  Gen(10);
  Gen(20);
  GenR(10);
  GenR(20);
END Fibonacci.

```

```txt

First 10 Fibonacci numbers
      1.      1.      2.      3.      5.      8.     13.     21.     34.     55.
First 20 Fibonacci numbers
      1.      1.      2.      3.      5.      8.     13.     21.     34.     55.     89.    144.    233.    377.    610.    987.   1597.   2584.   4181.   6765.
First 10 Fibonacci numbers (Recursive)
      1.      1.      2.      3.      5.      8.     13.     21.     34.     55.
First 20 Fibonacci numbers (Recursive)
      1.      1.      2.      3.      5.      8.     13.     21.     34.     55.     89.    144.    233.    377.    610.    987.   1597.   2584.   4181.   6765.

```



## Objeck


### Recursive


```objeck
bundle Default {
  class Fib {
    function : Main(args : String[]), Nil {
      for(i := 0; i <= 10; i += 1;) {
        Fib(i)->PrintLine();
      };
    }

    function : native : Fib(n : Int), Int {
      if(n < 2) {
        return n;
      };

      return Fib(n-1) + Fib(n-2);
    }
  }
}
```


=={{header|Objective-C}}==

### Recursive


```objc
-(long)fibonacci:(int)position
{
    long result = 0;
    if (position < 2) {
        result = position;
    } else {
        result = [self fibonacci:(position -1)] + [self fibonacci:(position -2)];
    }
    return result;
}
```


### Iterative


```objc
+(long)fibonacci:(int)index {
    long beforeLast = 0, last = 1;
    while (index > 0) {
        last += beforeLast;
        beforeLast = last - beforeLast;
        --index;
    }
    return last;
}
```



## OCaml


### Iterative


```ocaml
let fib_iter n =
  if n < 2 then
    n
  else let fib_prev = ref 1
  and fib = ref 1 in
    for num = 2 to n - 1 do
      let temp = !fib in
        fib := !fib + !fib_prev;
        fib_prev := temp
    done;
    !fib
```



### Recursive


```ocaml
let rec fib_rec n =
  if n < 2 then
   n
  else
    fib_rec (n - 1) + fib_rec (n - 2)

let rec fib = function
    0 -> 0
  | 1 -> 1
  | n -> if n > 0 then fib (n-1) + fib (n-2)
         else fib (n+2) - fib (n+1)

```



### Arbitrary Precision

Using OCaml's [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Num.html Num] module.


```ocaml
open Num

let fib =
  let rec fib_aux f0 f1 = function
    | 0 -> f0
    | 1 -> f1
    | n -> fib_aux f1 (f1 +/ f0) (n - 1)
  in
  fib_aux (num_of_int 0) (num_of_int 1)

(* support for negatives *)
let fib n =
      if n < 0 && n mod 2 = 0 then minus_num (fib (abs n))
      else fib (abs n)
;;
(* It can be called from the command line with an argument *)
(* Result is send to standart output *)
let n = int_of_string Sys.argv.(1) in
print_endline (string_of_num (fib n))

```


compile with:
 ocamlopt nums.cmxa -o fib fib.ml

Output:
```txt

$ ./fib 0
0
$ ./fib 10
55
$ ./fib 399
108788617463475645289761992289049744844995705477812699099751202749393926359816304226
$ ./fib -6
-8

```


=== O(log(n)) with arbitrary precision ===
This performs log2(N) matrix multiplys. Each multiplication is not constant-time but increases sub-linearly, about O(log(N)).

```ocaml
open Num

let mul (a,b,c) (d,e,f) = let bxe = b*/e in
  (a*/d +/ bxe, a*/e +/ b*/f, bxe +/ c*/f)

let id = (Int 1, Int 0, Int 1)
let rec pow a n =
  if n=0 then id else
    let b = pow a (n/2) in
    if (n mod 2) = 0 then mul b b else mul a (mul b b)

let fib n =
  let (_,y,_) = (pow (Int 1, Int 1, Int 0) n) in
  string_of_num y
;;
Printf.printf "fib %d = %s\n" 300 (fib 300)
```

Output:
```txt
fib 300 = 222232244629420445529739893461909967206666939096499764990979600
```



## Octave

'''Recursive'''

```octave
% recursive
function fibo = recfibo(n)
  if ( n < 2 )
    fibo = n;
  else
    fibo = recfibo(n-1) + recfibo(n-2);
  endif
endfunction
```


'''Iterative'''

```octave
% iterative
function fibo = iterfibo(n)
  if ( n < 2 )
    fibo = n;
  else
    f = zeros(2,1);
    f(1) = 0;
    f(2) = 1;
    for i = 2 : n
      t = f(2);
      f(2) = f(1) + f(2);
      f(1) = t;
    endfor
    fibo = f(2);
  endif
endfunction
```


'''Testing'''

```octave
% testing
for i = 0 : 20
  printf("%d %d\n", iterfibo(i), recfibo(i));
endfor
```



## Oforth


```Oforth
: fib   0 1 rot #[ tuck + ] times drop ;
```



## OPL


```opl
FIBON:
REM Fibonacci sequence is generated to the Organiser II floating point variable limit.
REM CLEAR/ON key quits.
REM Mikesan - http://forum.psion2.org/
LOCAL A,B,C
A=1 :B=1 :C=1
PRINT A,
DO
  C=A+B
  A=B
  B=C
  PRINT A,
UNTIL GET=1
```



## Order


### Recursive


```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8fib_rec                     \
ORDER_PP_FN(8fn(8N,                               \
                8if(8less(8N, 2),                 \
                    8N,                           \
                    8add(8fib_rec(8sub(8N, 1)),   \
                         8fib_rec(8sub(8N, 2))))))

ORDER_PP(8fib_rec(10))
```


Tail recursive version (example supplied with language):

```c
#include <order/interpreter.h>


#define ORDER_PP_DEF_8fib                                         \
ORDER_PP_FN(8fn(8N,                                               \
                8fib_iter(8N, 0, 1)))

#define ORDER_PP_DEF_8fib_iter                                    \
ORDER_PP_FN(8fn(8N, 8I, 8J,                                       \
                8if(8is_0(8N),                                    \
                    8I,                                           \
                    8fib_iter(8dec(8N), 8J, 8add(8I, 8J)))))

ORDER_PP(8to_lit(8fib(8nat(5,0,0))))
```



### Memoization


```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8fib_memo                                    \
ORDER_PP_FN(8fn(8N,                                               \
                8tuple_at(0, 8fib_memo_inner(8N, 8seq))))


#define ORDER_PP_DEF_8fib_memo_inner                                            \
ORDER_PP_FN(8fn(8N, 8M,                                                         \
                8cond((8less(8N, 8seq_size(8M)), 8pair(8seq_at(8N, 8M), 8M))    \
                      (8equal(8N, 0), 8pair(0, 8seq(0)))                        \
                      (8equal(8N, 1), 8pair(1, 8seq(0, 1)))                     \
                      (8else,                                                   \
                        8lets((8S, 8fib_memo_inner(8sub(8N, 2), 8M))            \
                              (8T, 8fib_memo_inner(8dec(8N), 8tuple_at(1, 8S))) \
                              (8U, 8add(8tuple_at(0, 8S), 8tuple_at(0, 8T))),   \
                              8pair(8U,                                         \
                                    8seq_append(8tuple_at(1, 8T), 8seq(8U))))))))


ORDER_PP(
8for_each_in_range(8fn(8N,
                       8print(8to_lit(8fib_memo(8N)) (,) 8space)),
                   1, 21)
)
```



## Oz



### Iterative

Using mutable references (cells).

```oz
fun{FibI N}
  Temp = {NewCell 0}
  A = {NewCell 0}
  B = {NewCell 1}
in
  for I in 1..N do
    Temp := @A + @B
    A := @B
    B := @Temp
  end
  @A
end
```



### Recursive

Inefficient (blows up the stack).

```oz
fun{FibR N}
  if N < 2 then N
  else {FibR N-1} + {FibR N-2}
  end
end
```


===Tail-recursive===
Using accumulators.

```oz
fun{Fib N}
   fun{Loop N A B}
      if N == 0 then
	 B
      else
	 {Loop N-1 A+B A}
      end
   end
in
   {Loop N 1 0}
end
```


===Lazy-recursive===

```oz
declare
  fun lazy {FiboSeq}
     {LazyMap
      {Iterate fun {$ [A B]} [B A+B] end [0 1]}
      Head}
  end

  fun {Head A|_} A end

  fun lazy {Iterate F I}
     I|{Iterate F {F I}}
  end

  fun lazy {LazyMap Xs F}
     case Xs of X|Xr then {F X}|{LazyMap Xr F}
     [] nil then nil
     end
  end
in
  {Show {List.take {FiboSeq} 8}}
```



## PARI/GP

===Built-in===

```parigp
fibonocci(n)
```



### Matrix


```parigp
fib(n)=([1,1;1,0]^n)[1,2]
```



### Analytic

This uses the Binet form.

```parigp
fib(n)=my(phi=(1+sqrt(5))/2);round((phi^n-phi^-n)/sqrt(5))
```

The second term can be dropped since the error is always small enough to be subsumed by the rounding.

```parigp
fib(n)=round(((1+sqrt(5))/2)^n/sqrt(5))
```



### Algebraic

This is an exact version of the above formula. <code>quadgen(5)</code> represents <math>\phi</math> and the number is stored in the form <math>a+b\phi</math>.  <code>imag</code> takes the coefficient of <math>\phi</math>.  This uses the relation
:<math>\phi^n=F_{n-1}+F_n\phi</math>
and hence <code>real(quadgen(5)^n)</code> would give the (n-1)-th Fibonacci number.


```parigp
fib(n)=imag(quadgen(5)^n)
```


A more direct translation (note that <math>\sqrt5=2\phi-1</math>) would be

```parigp
fib(n)=my(phi=quadgen(5));(phi^n-(-1/phi)^n)/(2*phi-1)
```



### Combinatorial

This uses the generating function. It can be trivially adapted to give the first n Fibonacci numbers.

```parigp
fib(n)=polcoeff(x/(1-x-x^2)+O(x^(n+1)),n)
```



### Binary powering


```parigp
fib(n)={
  if(n<=0,
    if(n,(-1)^(n+1)*fib(n),0)
  ,
    my(v=lucas(n-1));
    (2*v[1]+v[2])/5
  )
};
lucas(n)={
  if (!n, return([2,1]));
  my(v=lucas(n >> 1), z=v[1], t=v[2], pr=v[1]*v[2]);
  n=n%4;
  if(n%2,
    if(n==3,[v[1]*v[2]+1,v[2]^2-2],[v[1]*v[2]-1,v[2]^2+2])
  ,
    if(n,[v[1]^2+2,v[1]*v[2]+1],[v[1]^2-2,v[1]*v[2]-1])
  )
};
```



### Recursive


```parigp
fib(n)={
  if(n<2,
    n
  ,
    fib(n-1)+fib(n)
  )
};
```



### Anonymous recursion

This uses <code>self()</code> which gives a self-reference.

```parigp
fib(n)={
  if(n<2,
    n
  ,
    my(s=self());
    s(n-2)+s(n-1)
  )
};
```


It can be used without being named:

```parigp
apply(n->if(n<2,n,my(s=self());s(n-2)+s(n-1)), [1..10])
```

gives
```txt
%1 = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```



### Memoization


```parigp
F=[];
fib(n)={
  if(n>#F,
    F=concat(F, vector(n-#F));
    F[n]=fib(n-1)+fib(n-2)
  ,
    if(n<2,
      n
    ,
      if(F[n],F[n],F[n]=fib(n-1)+fib(n-2))
    )
  );
}
```



### Iterative


```parigp
fib(n)={
  if(n<0,return((-1)^(n+1)*fib(n)));
  my(a=0,b=1,t);
  while(n,
    t=a+b;
    a=b;
    b=t;
    n--
  );
  a
};
```



### Chebyshev

This solution uses Chebyshev polynomials of the second kind (Chyebyshev U-polynomials).

```parigp
fib(n)=n--;polchebyshev(n,2,I/2)*I^n;
```

or

```parigp
fib(n)=abs(polchebyshev(n-1,2,I/2));
```


===Anti-Hadamard matrix===
All n×n [https://en.wikipedia.org/wiki/Logical_matrix (0,1)] lower [https://en.wikipedia.org/wiki/Hessenberg_matrix Hessenberg matrices] have determinant at most F(n). The n×n anti-Hadamard matrix<ref>
R. L. Graham and N. J. A. Sloane, [http://www.math.ucsd.edu/~ronspubs/84_03_anti_hadamard.pdf Anti-Hadamard matrices], Linear Algebra Appl. 62 (1984), 113–137.</ref> matches this upper bound, and hence can be used as an inefficient method for computing Fibonacci numbers of positive index. These matrices are the same as Matlab's type-3 "Dramadah" matrices, following a naming suggestion of C. L. Mallows according to Graham & Sloane.


```parigp
matantihadamard(n)={
  matrix(n,n,i,j,
    my(t=j-i+1);
    if(t<1,t%2,t<3)
  );
}
fib(n)=matdet(matantihadamard(n))
```




### Testing adjacent bits

The Fibonacci numbers can be characterized (for n > 0) as the number of n-bit strings starting and ending with 1 without adjacent 0s. This inefficient, exponential-time algorithm demonstrates:

```parigp
fib(n)=
{
  my(g=2^(n+1)-1);
  sum(i=2^(n-1),2^n-1,
    bitor(i,i<<1)==g
  );
}
```


===One-by-one===
This code is purely for amusement and requires n > 1. It tests numbers in order to see if they are Fibonacci numbers, and waits until it has seen ''n'' of them.

```parigp
fib(n)=my(k=0);while(n--,k++;while(!issquare(5*k^2+4)&&!issquare(5*k^2-4),k++));k
```



## Pascal


### Analytic


```pascal
function fib(n: integer):longInt;
const
  Sqrt5 = sqrt(5.0);
  C1 = ln((Sqrt5+1.0)*0.5);//ln( 1.618..)
//C2 = ln((1.0-Sqrt5)*0.5);//ln(-0.618 )) tsetsetse
  C2 = ln((Sqrt5-1.0)*0.5);//ln(+0.618 ))
begin
  IF n>0 then
  begin
    IF odd(n) then
      fib := round((exp(C1*n) + exp(C2*n) )/Sqrt5)
    else
      fib := round((exp(C1*n) - exp(C2*n) )/Sqrt5)
  end
  else
    Fibdirekt := 0
end;
```



### Recursive


```pascal
function fib(n: integer): integer;
 begin
  if (n = 0) or (n = 1)
   then
    fib := n
   else
    fib := fib(n-1) + fib(n-2)
 end;
```



### Iterative


```pascal
function fib(n: integer): integer;
var
  f0, f1, tmpf0, k: integer;
begin
  f1 := n;
  IF f1 >1 then
  begin
    k := f1-1;
    f0 := 0;
    f1 := 1;
    repeat
      tmpf0 := f0;
      f0 := f1;
      f1 := f1+tmpf0;
      dec(k);
    until k = 0;
  end
  else
    IF f1 < 0 then
      f1 := 0;
  fib := f1;
end;
```



### Analytic2


```pascal
function FiboMax(n: integer):Extended;  //maXbox
begin
   result:= (pow((1+SQRT5)/2,n)-pow((1-SQRT5)/2,n))/SQRT5
end;
```




```pascal
function Fibo_BigInt(n: integer): string;  //maXbox
  var tbig1, tbig2, tbig3: TInteger;
  begin
    result:= '0'
    tbig1:= TInteger.create(1);  //temp
    tbig2:= TInteger.create(0);  //result (a)
    tbig3:= Tinteger.create(1);  //b
    for it:= 1 to n do begin
    	tbig1.assign(tbig2)
	   tbig2.assign(tbig3);
	   tbig1.add(tbig3);
	   tbig3.assign(tbig1);
	 end;
    result:= tbig2.toString(false)
    tbig3.free;
    tbig2.free;
    tbig1.free;
  end;
```


writeln(floattoStr(FiboMax(555)))
>>>4.3516638122555E115

writeln(Fibo_BigInt(555))
>>>43516638122555047989641805373140394725407202037260729735885664398655775748034950972577909265605502785297675867877570


## Perl


### Iterative


```perl
sub fib_iter {
  my $n = shift;
  use bigint try => "GMP,Pari";
  my ($v2,$v1) = (-1,1);
  ($v2,$v1) = ($v1,$v2+$v1) for 0..$n;
  $v1;
}
```



### Recursive


```perl
sub fibRec {
    my $n = shift;
    $n < 2 ? $n : fibRec($n - 1) + fibRec($n - 2);
}
```



### Modules

Quite a few modules have ways to do this.  Performance is not typically an issue with any of these until 100k or so.  With GMP available, the first three are ''much'' faster at large values.

```perl
# Uses GMP method so very fast
use Math::AnyNum qw/fibonacci/;
say fibonacci(10000);

# Uses GMP method, so also very fast
use Math::GMP;
say Math::GMP::fibonacci(10000);

# Binary ladder, GMP if available, Pure Perl otherwise
use ntheory qw/lucasu/;
say lucasu(1, -1, 10000);

# All Perl
use Math::NumSeq::Fibonacci;
my $seq = Math::NumSeq::Fibonacci->new;
say $seq->ith(10000);

# All Perl
use Math::Big qw/fibonacci/;
say 0+fibonacci(10000);  # Force scalar context

# Perl, gives floating point *approximation*
use Math::Fibonacci qw/term/;
say term(10000);
```



## Perl 6



### List Generator


This constructs the fibonacci sequence as a lazy infinite list.

```perl6
constant @fib = 0, 1, *+* ... *;
```


If you really need a function for it:

```perl6
sub fib ($n) { @fib[$n] }
```


To support negative indices:

```perl6
constant @neg-fib = 0, 1, *-* ... *;
sub fib ($n) { $n >= 0 ?? @fib[$n] !! @neg-fib[-$n] }
```



### Iterative


```perl6
sub fib (Int $n --> Int) {
    $n > 1 or return $n;
    my ($prev, $this) = 0, 1;
    ($prev, $this) = $this, $this + $prev for 1 ..^ $n;
    return $this;
}
```



### Recursive


```perl6
proto fib (Int $n --> Int) {*}
multi fib (0)  { 0 }
multi fib (1)  { 1 }
multi fib ($n) { fib($n - 1) + fib($n - 2) }
```



### Analytic


```perl6
sub fib (Int $n --> Int) {
    constant φ1 = 1 / constant φ = (1 + sqrt 5)/2;
    constant invsqrt5 = 1 / sqrt 5;

    floor invsqrt5 * (φ**$n + φ1**$n);
}
```



## Phix


```Phix
function fibonacci(integer n)     -- iterative, works for -ve numbers
atom a=0, b=1
    if n=0 then return 0 end if
    if abs(n)>=79 then ?9/0 end if  -- inaccuracies creep in above 78
    for i=1 to abs(n)-1 do
        {a,b} = {b,a+b}
    end for
    if n<0 and remainder(n,2)=0 then return -fcache[absn] end if
    return fcache[absn]
end function

for i=0 to 28 do
    if i then puts(1,", ") end if
    printf(1,"%d", fibonacci(i))
end for
puts(1,"\n")
```

```txt

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811

```

Using native integers/atoms, errors creep in above 78, so the same program converted to use mpfr:
```Phix
-- demo\rosetta\fibonacci.exw
include mpfr.e

mpz res = NULL, prev, next
integer lastn
atom t0 = time()

function fibonampz(integer n) -- resumable, works for -ve numbers, yields mpz
integer absn = abs(n)
    if res=NULL or absn!=abs(lastn)+1 then
        if res=NULL then
            prev = mpz_init(0)
            res = mpz_init(1)
            next = mpz_init()
        else
            if n==lastn then return res end if
        end if
        mpz_fib2_ui(res,prev,absn)
    else
        if lastn<0 and remainder(lastn,2)=0 then
            mpz_mul_si(res,res,-1)
        end if
        mpz_add(next,res,prev)
        {prev,res,next} = {res,next,prev}
    end if
    if n<0 and remainder(n,2)=0 then
        mpz_mul_si(res,res,-1)
    end if
    lastn = n
    return res
end function

for i=0 to 28 do
    if i then puts(1,", ") end if
    printf(1,"%s", {mpz_get_str(fibonampz(i))})
end for
puts(1,"\n")
printf(1,"%s\n", {mpz_get_str(fibonampz(705))})

string s = mpz_get_str(fibonampz(4784969))
integer l = length(s)
s[40..-40] = "..."
?{l,s}
?elapsed(time()-t0)

```

```txt

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811
970066202977562212558683426760773016559904631977220423547980211057068777324159443678590358026859129109599109446646966713225742014317926940054191330
{1000000,"107273956418004772293648135962250043219...407167474856539211500699706378405156269"}
"2.1s"

```



## PHP


### Iterative


```php
function fibIter($n) {
    if ($n < 2) {
        return $n;
    }
    $fibPrev = 0;
    $fib = 1;
    foreach (range(1, $n-1) as $i) {
        list($fibPrev, $fib) = array($fib, $fib + $fibPrev);
    }
    return $fib;
}
```


### Recursive


```php
function fibRec($n) {
    return $n < 2 ? $n : fibRec($n-1) + fibRec($n-2);
}
```



## PicoLisp


### Recursive


```PicoLisp
(de fibo (N)
   (if (>= 2 N)
      1
      (+ (fibo (dec N)) (fibo (- N 2))) ) )
```



### Recursive with Cache

Using a recursive version doesn't need to be slow, as the following shows:

```PicoLisp
(de fibo (N)
   (cache '(NIL) N  # Use a cache to accelerate
      (if (>= 2 N)
         N
         (+ (fibo (dec N)) (fibo (- N 2))) ) ) )

(bench (fibo 1000))
```

Output:

```PicoLisp
0.012 sec
-> 43466557686937456435688527675040625802564660517371780402481729089536555417949
05189040387984007925516929592259308032263477520968962323987332247116164299644090
6533187938298969649928516003704476137795166849228875
```



### Iterative


```PicoLisp
(de fib (N)
   (let (A 0  B 1)
      (do N
         (prog1 B (setq B (+ A B) A @)) ) ) )
```



### Coroutines


```PicoLisp
(co 'fibo
   (let (A 0  B 1)
      (yield 'ready)
      (while
         (yield
            (swap 'B (+ (swap 'A B) B)) ) ) ) )

(do 15
   (printsp (yield 'next 'fibo)) )
(prinl)
(yield NIL 'fibo)
```

```txt
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
```



## PIR

Recursive:
```pir
.sub fib
  .param int n
  .local int nt
  .local int ft
  if n < 2 goto RETURNN
  nt = n - 1
  ft = fib( nt )
  dec nt
  nt = fib(nt)
  ft = ft + nt
  .return( ft )
RETURNN:
  .return( n )
  end
.end

.sub main :main
  .local int counter
  .local int f
  counter=0
LOOP:
  if counter > 20 goto DONE
  f = fib(counter)
  print f
  print "\n"
  inc counter
  goto LOOP
DONE:
  end
.end
```


Iterative (stack-based):
```pir
.sub fib
  .param int n
  .local int counter
  .local int f
  .local pmc fibs
  .local int nmo
  .local int nmt

  fibs = new 'ResizableIntegerArray'
  if n == 0 goto RETURN0
  if n == 1 goto RETURN1
  push fibs, 0
  push fibs, 1
  counter = 2
FIBLOOP:
  if counter > n goto DONE
  nmo = pop fibs
  nmt = pop fibs
  f = nmo + nmt
  push fibs, nmt
  push fibs, nmo
  push fibs, f
  inc counter
  goto FIBLOOP
RETURN0:
  .return( 0 )
  end
RETURN1:
  .return( 1 )
  end
DONE:
  f = pop fibs
  .return( f )
  end
.end

.sub main :main
  .local int counter
  .local int f
  counter=0
LOOP:
  if counter > 20 goto DONE
  f = fib(counter)
  print f
  print "\n"
  inc counter
  goto LOOP
DONE:
  end
.end
```



## Pike



### Iterative


```pike
int
fibIter(int n) {
    int fibPrev, fib, i;
    if (n < 2) {
        return 1;
    }
    fibPrev = 0;
    fib = 1;
    for (i = 1; i < n; i++) {
        int oldFib = fib;
        fib += fibPrev;
        fibPrev = oldFib;
    }
    return fib;
}
```



### Recursive


```pike
int
fibRec(int n) {
    if (n < 2) {
        return(1);
    }
    return( fib(n-2) + fib(n-1) );
}
```



## PL/I


```pli
/* Form the n-th Fibonacci number, n > 1. */
get list(n);
f1 = 0; f2 = 1;
do i = 2 to n;
   f3 = f1 + f2;
   put skip edit('fibo(',i,')=',f3)(a,f(5),a,f(5));
   f1 = f2;
   f2 = f3;
end;
```



## PL/pgSQL



###  Recursive


```SQL
CREATE OR REPLACE FUNCTION fib(n INTEGER) RETURNS INTEGER AS $$
BEGIN
  IF (n < 2) THEN
    RETURN n;
  END IF;
  RETURN fib(n - 1) + fib(n - 2);
END;
$$ LANGUAGE plpgsql;
```



###  Calculated


```SQL
CREATE OR REPLACE FUNCTION fibFormula(n INTEGER) RETURNS INTEGER AS $$
BEGIN
  RETURN round(pow((pow(5, .5) + 1) / 2, n) / pow(5, .5));
END;
$$ LANGUAGE plpgsql;
```



###  Linear


```SQL
CREATE OR REPLACE FUNCTION fibLinear(n INTEGER) RETURNS INTEGER AS $$
DECLARE
  prevFib INTEGER := 0;
  fib INTEGER := 1;
BEGIN
  IF (n < 2) THEN
    RETURN n;
  END IF;

  WHILE n > 1 LOOP
    SELECT fib, prevFib + fib INTO prevFib, fib;
    n := n - 1;
  END LOOP;

  RETURN fib;
END;
$$ LANGUAGE plpgsql;
```



###  Tail recursive


```SQL
CREATE OR REPLACE FUNCTION fibTailRecursive(n INTEGER, prevFib INTEGER DEFAULT 0, fib INTEGER DEFAULT 1)
RETURNS INTEGER AS $$
BEGIN
  IF (n = 0) THEN
    RETURN prevFib;
  END IF;
  RETURN fibTailRecursive(n - 1, fib, prevFib + fib);
END;
$$ LANGUAGE plpgsql;
```



## PL/SQL


```PL/SQL
Create or replace Function fnu_fibonnaci(p_iNumber integer)
return integer
is
  nuFib  integer;
  nuP  integer;
  nuQ  integer;
Begin
  if p_iNumber is not null then
     if p_iNumber=0 then
        nuFib:=0;
     Elsif p_iNumber=1 then
            nuFib:=1;
     Else
        nuP:=0;
        nuQ:=1;
        For nuI in 2..p_iNumber loop
            nuFib:=nuP+nuQ;
            nuP:=nuQ;
            nuQ:=nuFib;
        End loop;
     End if;
  End if;
  return(nuFib);
End fnu_fibonnaci;
```



## Pop11


```pop11
define fib(x);
lvars a , b;
    1 -> a;
    1 -> b;
    repeat x - 1 times
         (a + b, b) -> (b, a);
    endrepeat;
    a;
enddefine;
```



## PostScript

Enter the desired number for "n" and run through your favorite postscript previewer or send to your postscript printer:


```postscript
%!PS

% We want the 'n'th fibonacci number
/n 13 def

% Prepare output canvas:
/Helvetica findfont 20 scalefont setfont
100 100 moveto

%define the function recursively:
/fib { dup
       3 lt
         { pop 1 }
         { dup 1 sub fib exch 2 sub fib add }
       ifelse
    } def

    (Fib\() show n (....) cvs show (\)=) show n fib (.....) cvs show

showpage
```



## Potion



### Recursive

Starts with int and upgrades on-the-fly to doubles.

```potion
recursive = (n):
  if (n <= 1): 1. else: recursive (n - 1) + recursive (n - 2)..

n = 40
("fib(", n, ")= ", recursive (n), "\n") join print
```



```txt

recursive(40)= 165580141
real	0m2.851s

```



### Iterative


```potion
iterative = (n) :
   curr = 0
   prev = 1
   tmp = 0
   n times:
      tmp = curr
      curr = curr + prev
      prev = tmp
   .
   curr
.
```



### Matrix based


```potion
sqr = (x): x * x.

# Based on the fact that
# F2n = Fn(2Fn+1 - Fn)
# F2n+1 = Fn ^2 + Fn+1 ^2
matrix = (n) :
   algorithm = (n) :
      "computes (Fn, Fn+1)"
      if (n < 2): return ((0, 1), (1, 1)) at(n).

      # n = e + {0, 1}
      q = algorithm(n / 2)  # q = (Fe/2, Fe/2+1)
      q = (q(0) * (2 * q(1) - q(0)), sqr(q(0)) + sqr(q(1)))  # q => (Fe, Fe+1)
      if (n % 2 == 1) :  # q => (Fe+{0, 1}, Fe+1+{0,1}) = (Fn, Fn+1)
         q = (q(1), q(1) + q(0))
      .
      q
   .
   algorithm(n)(0)
.
```



### Handling negative values

<lang>fibonacci = (n) :
   myFavorite = matrix
   if (n >= 0) :
      myFavorite(n)
   .  else :
      n = n * -1
      if (n % 2 == 1) :
         myFavorite(n)
      . else :
         myFavorite(n) * -1
      .
   .
.
```



## PowerBASIC

There seems to be a limitation (dare I say, bug?) in PowerBASIC regarding how large numbers are stored. 10E17 and larger get rounded to the nearest 10. For F(n), where ABS(n) > 87, is affected like this:
       actual:             displayed:
 F(88) 1100087778366101931 1100087778366101930
 F(89) 1779979416004714189 1779979416004714190
 F(90) 2880067194370816120 2880067194370816120
 F(91) 4660046610375530309 4660046610375530310
 F(92) 7540113804746346429 7540113804746346430


```powerbasic
FUNCTION fibonacci (n AS LONG) AS QUAD
    DIM u AS LONG, a AS LONG, L0 AS LONG, outP AS QUAD
    STATIC fibNum() AS QUAD

    u = UBOUND(fibNum)
    a = ABS(n)

    IF u < 1 THEN
        REDIM fibNum(1)
        fibNum(1) = 1
        u = 1
    END IF

    SELECT CASE a
        CASE 0 TO 92
            IF a > u THEN
                REDIM PRESERVE fibNum(a)
                FOR L0 = u + 1 TO a
                    fibNum(L0) = fibNum(L0 - 1) + fibNum(L0 - 2)
                    IF 88 = L0 THEN fibNum(88) = fibNum(88) + 1
                NEXT
            END IF
            IF n < 0 THEN
                fibonacci = fibNum(a) * ((-1)^(a+1))
            ELSE
                fibonacci = fibNum(a)
            END IF
        CASE ELSE
            'Even without the above-mentioned bug, we're still limited to
            'F(+/-92), due to data type limits. (F(93) = &hA94F AD42 221F 2702)
            ERROR 6
    END SELECT
END FUNCTION

FUNCTION PBMAIN () AS LONG
    DIM n AS LONG
    #IF NOT %DEF(%PB_CC32)
        OPEN "out.txt" FOR OUTPUT AS 1
    #ENDIF
    FOR n = -92 TO 92
        #IF %DEF(%PB_CC32)
            PRINT STR$(n); ": "; FORMAT$(fibonacci(n), "#")
        #ELSE
            PRINT #1, STR$(n) & ": " & FORMAT$(fibonacci(n), "#")
        #ENDIF
    NEXT
    CLOSE
END FUNCTION
```



## PowerShell


### Iterative


```powershell

function FibonacciNumber ( $count )
{
    $answer = @(0,1)
    while ($answer.Length -le $count)
    {
        $answer += $answer[-1] + $answer[-2]
    }
    return $answer
}

```


An even shorter version that eschews function calls altogether:


```powershell

$count = 8
$answer = @(0,1)
0..($count - $answer.Length) | Foreach { $answer += $answer[-1] + $answer[-2] }
$answer

```



### Recursive


```powershell
function fib($n) {
    switch ($n) {
        0            { return 0 }
        1            { return 1 }
        { $_ -lt 0 } { return [Math]::Pow(-1, -$n + 1) * (fib (-$n)) }
        default      { return (fib ($n - 1)) + (fib ($n - 2)) }
    }
}
```



## Processing

```processing
void setup() {
  size(400, 400);
  fill(255, 64);
  frameRate(2);
}
void draw() {
  int num = fibonacciNum(frameCount);
  println(frameCount, num);
  rect(0,0,num, num);
  if(frameCount==14) frameCount = -1; // restart
}
int fibonacciNum(int n) {
  return (n < 2) ? n : fibonacciNum(n - 1) + fibonacciNum(n - 2);
}
```


On the nth frame, the nth Fibonacci number is printed to the console and a square of that size is drawn on the sketch surface. The sketch restarts to keep drawing within the window size.

```txt
1
1
2
3
5
8
13
21
34
55
89
144
233
377
```



## Prolog

```prolog

fib(1, 1) :- !.
fib(0, 0) :- !.
fib(N, Value) :-
  A is N - 1, fib(A, A1),
  B is N - 2, fib(B, B1),
  Value is A1 + B1.

```


This naive implementation works, but is very slow for larger values of N.  Here are some simple measurements (in SWI-Prolog):

```prolog
?- time(fib(0,F)).
% 2 inferences, 0.000 CPU in 0.000 seconds (88% CPU, 161943 Lips)
F = 0.

?- time(fib(10,F)).
% 265 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 1458135 Lips)
F = 55.

?- time(fib(20,F)).
% 32,836 inferences, 0.016 CPU in 0.016 seconds (99% CPU, 2086352 Lips)
F = 6765.

?- time(fib(30,F)).
% 4,038,805 inferences, 1.122 CPU in 1.139 seconds (98% CPU, 3599899 Lips)
F = 832040.

?- time(fib(40,F)).
% 496,740,421 inferences, 138.705 CPU in 140.206 seconds (99% CPU, 3581264 Lips)
F = 102334155.
```


As you can see, the calculation time goes up exponentially as N goes higher.

===Poor man's memoization===
The performance problem can be readily fixed by the addition of two lines of code (the first and last in this version):

```prolog
%:- dynamic fib/2.  % This is ISO, but GNU doesn't like it.
:- dynamic(fib/2).  % Not ISO, but works in SWI, YAP and GNU unlike the ISO declaration.
fib(1, 1) :- !.
fib(0, 0) :- !.
fib(N, Value) :-
  A is N - 1, fib(A, A1),
  B is N - 2, fib(B, B1),
  Value is A1 + B1,
  asserta((fib(N, Value) :- !)).
```


Let's take a look at the execution costs now:


```prolog
?- time(fib(0,F)).
% 2 inferences, 0.000 CPU in 0.000 seconds (90% CPU, 160591 Lips)
F = 0.

?- time(fib(10,F)).
% 37 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 552610 Lips)
F = 55.

?- time(fib(20,F)).
% 41 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 541233 Lips)
F = 6765.

?- time(fib(30,F)).
% 41 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 722722 Lips)
F = 832040.

?- time(fib(40,F)).
% 41 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 543572 Lips)
F = 102334155.
```


In this case by asserting the new N,Value pairing as a rule in the database we're making the classic time/space tradeoff.  Since the space costs are (roughly) linear by N and the time costs are exponential by N, the trade-off is desirable.  You can see the poor man's memoizing easily:


```prolog
?- listing(fib).
:- dynamic fib/2.

fib(40, 102334155) :- !.
fib(39, 63245986) :- !.
fib(38, 39088169) :- !.
fib(37, 24157817) :- !.
fib(36, 14930352) :- !.
fib(35, 9227465) :- !.
fib(34, 5702887) :- !.
fib(33, 3524578) :- !.
fib(32, 2178309) :- !.
fib(31, 1346269) :- !.
fib(30, 832040) :- !.
fib(29, 514229) :- !.
fib(28, 317811) :- !.
fib(27, 196418) :- !.
fib(26, 121393) :- !.
fib(25, 75025) :- !.
fib(24, 46368) :- !.
fib(23, 28657) :- !.
fib(22, 17711) :- !.
fib(21, 10946) :- !.
fib(20, 6765) :- !.
fib(19, 4181) :- !.
fib(18, 2584) :- !.
fib(17, 1597) :- !.
fib(16, 987) :- !.
fib(15, 610) :- !.
fib(14, 377) :- !.
fib(13, 233) :- !.
fib(12, 144) :- !.
fib(11, 89) :- !.
fib(10, 55) :- !.
fib(9, 34) :- !.
fib(8, 21) :- !.
fib(7, 13) :- !.
fib(6, 8) :- !.
fib(5, 5) :- !.
fib(4, 3) :- !.
fib(3, 2) :- !.
fib(2, 1) :- !.
fib(1, 1) :- !.
fib(0, 0) :- !.
fib(A, D) :-
	B is A+ -1,
	fib(B, E),
	C is A+ -2,
	fib(C, F),
	D is E+F,
	asserta((fib(A, D):-!)).
```


All of the interim N/Value pairs have been asserted as facts for quicker future use, speeding up the generation of the higher Fibonacci numbers.


### Continuation passing style

Works with <b>SWI-Prolog</b> and module lambda, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(lambda).
fib(N, FN) :-
	cont_fib(N, _, FN, \_^Y^_^U^(U = Y)).

cont_fib(N, FN1, FN, Pred) :-
	(   N < 2 ->
	    call(Pred, 0, 1, FN1, FN)
	;
	    N1 is N - 1,
	    P = \X^Y^Y^U^(U is X + Y),
	    cont_fib(N1, FNA, FNB, P),
	    call(Pred, FNA, FNB, FN1, FN)
	).

```



### With lazy lists

Works with <b>SWI-Prolog</b> and others that support <code>freeze/2</code>.


```Prolog
fib([0,1|X]) :-
    ffib(0,1,X).
ffib(A,B,X) :-
    freeze(X, (C is A+B, X=[C|Y], ffib(B,C,Y)) ).
```


The predicate <code>fib(Xs)</code> unifies <code>Xs</code> with an infinite list whose values are the Fibonacci sequence.  The list can be used like this:


```Prolog
?- fib(X), length(A,15), append(A,_,X), writeln(A).
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
```



### Generators idiom



```Prolog
take( 0, Next, Z-Z, Next).
take( N, Next, [A|B]-Z, NZ):- N>0, !, next( Next, A, Next1),
  N1 is N-1,
  take( N1, Next1, B-Z, NZ).

next( fib(A,B), A, fib(B,C)):- C is A+B.

%% usage: ?- take(15, fib(0,1), _X-[], G), writeln(_X).
%% [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
%% G = fib(610, 987)
```



###  Yet another implementation

One of my favorites; loosely similar to the first example, but without the performance penalty, and needs nothing special to implement.  Not even a dynamic database predicate. Attributed to M.E. for the moment, but simply because I didn't bother to search for the many people who probably did it like this long before I did.  If someone knows who came up with it first, please let us know.


```Prolog
% Fibonacci sequence generator
fib(C, [P,S], C, N)  :- N is P + S.
fib(C, [P,S], Cv, V) :- succ(C, Cn), N is P + S, !, fib(Cn, [S,N], Cv, V).

fib(0, 0).
fib(1, 1).
fib(C, N) :- fib(2, [0,1], C, N). % Generate from 3rd sequence on
```

Looking at performance:

```txt
 ?- time(fib(30,X)).
% 86 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 832040
 ?- time(fib(40,X)).
% 116 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 102334155
 ?- time(fib(100,X)).
% 296 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
X = 354224848179261915075

```

What I really like about this one, is it is also a generator- i.e. capable of generating all the numbers in sequence needing no bound input variables or special Prolog predicate support (such as freeze/3 in the previous example):


```txt
?- time(fib(X,Fib)).
% 0 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = Fib, Fib = 0 ;
% 1 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = Fib, Fib = 1 ;
% 3 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 2,
Fib = 1 ;
% 5 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 3,
Fib = 2 ;
% 5 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 4,
Fib = 3 ;
% 5 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = Fib, Fib = 5 ;
% 5 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 6,
Fib = 8
...etc.
```

It stays at 5 inferences per iteration after X=3. Also, quite useful:

```txt
 ?- time(fib(100,354224848179261915075)).
% 296 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
true .

?- time(fib(X,354224848179261915075)).
% 394 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
X = 100 .
```



## Pure


### Tail Recursive


```pure
fib n = loop 0 1 n with
  loop a b n = if n==0 then a else loop b (a+b) (n-1);
end;
```



## PureBasic



### Macro based calculation


```PureBasic
Macro Fibonacci (n)
	Int((Pow(((1+Sqr(5))/2),n)-Pow(((1-Sqr(5))/2),n))/Sqr(5))
EndMacro
```


### Recursive


```PureBasic
Procedure FibonacciReq(n)
  If n<2
    ProcedureReturn n
  Else
    ProcedureReturn FibonacciReq(n-1)+FibonacciReq(n-2)
  EndIf
EndProcedure
```


===Recursive & optimized with a static hash table===
This will be much faster on larger n's, this as it uses a table to store known parts instead of recalculating them.
On my machine the speedup compares to above code is
 Fib(n) Speedup
 20           2
 25          23
 30         217
 40       25847
 46     1156741

```PureBasic
Procedure Fibonacci(n)
  Static NewMap Fib.i()
  Protected FirstRecursion

  If MapSize(Fib())= 0        ; Init the hash table the first run
    Fib("0")=0: Fib("1")=1
    FirstRecursion = #True
  EndIf

  If n >= 2
    Protected.s s=Str(n)
    If Not FindMapElement(Fib(),s)  ; Calculate only needed parts
      Fib(s)= Fibonacci(n-1)+Fibonacci(n-2)
    EndIf
    n = Fib(s)
  EndIf
  If FirstRecursion ; Free the memory when finalizing the first call
    ClearMap(Fib())
  EndIf
  ProcedureReturn n
EndProcedure
```


'''Example'''
 Fibonacci(0)= 0
 Fibonacci(1)= 1
 Fibonacci(2)= 1
 Fibonacci(3)= 2
 Fibonacci(4)= 3
 Fibonacci(5)= 5

 FibonacciReq(0)= 0
 FibonacciReq(1)= 1
 FibonacciReq(2)= 1
 FibonacciReq(3)= 2
 FibonacciReq(4)= 3
 FibonacciReq(5)= 5


## Purity

The following takes a natural number and generates an initial segment of the Fibonacci sequence of that length:


```Purity

data Fib1 = FoldNat
            <
              const (Cons One (Cons One Empty)),
              (uncurry Cons) . ((uncurry Add) . (Head, Head . Tail), id)
            >

```


This following calculates the Fibonacci sequence as an infinite stream of natural numbers:


```Purity

type (Stream A?,,,Unfold) = gfix X. A? . X?
data Fib2 = Unfold ((outl, (uncurry Add, outl))) ((curry id) One One)

```


As a histomorphism:


```Purity

import Histo

data Fib3 = Histo . Memoize
            <
              const One,
              (p1 =>
              <
                const One,
                (p2 => Add (outl $p1) (outl $p2)). UnmakeCofree
              > (outr $p1)) . UnmakeCofree
            >

```



## Python


### Iterative positive and negative


```python
def fib(n,x=[0,1]):
   for i in range(abs(n)-1): x=[x[1],sum(x)]
   return x[1]*pow(-1,abs(n)-1) if n<0 else x[1] if n else 0

for i in range(-30,31): print fib(i),
```

Output:

```txt

-832040 514229 -317811 196418 -121393 75025 -46368 28657 -17711 10946 -6765 4181 -2584 1597 -987
610 -377 233 -144 89 -55 34 -21 13 -8 5 -3 2 -1 1 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987
1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040

```


### Analytic

Binet's formula:

```python
from math import *

def analytic_fibonacci(n):
  sqrt_5 = sqrt(5);
  p = (1 + sqrt_5) / 2;
  q = 1/p;
  return int( (p**n + q**n) / sqrt_5 + 0.5 )

for i in range(1,31):
  print analytic_fibonacci(i),
```

Output:

```txt

1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040

```



### Iterative


```python
def fibIter(n):
    if n < 2:
        return n
    fibPrev = 1
    fib = 1
    for num in xrange(2, n):
        fibPrev, fib = fib, fib + fibPrev
    return fib
```


### Recursive


```python
def fibRec(n):
    if n < 2:
        return n
    else:
        return fibRec(n-1) + fibRec(n-2)
```



### Recursive with Memoization



```python
def fibMemo():
    pad = {0:0, 1:1}
    def func(n):
        if n not in pad:
            pad[n] = func(n-1) + func(n-2)
        return pad[n]
    return func

fm = fibMemo()
for i in range(1,31):
    print fm(i),
```


Output:

```txt

1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
```




===Better Recursive doesn't need Memoization===

The recursive code as written two sections above is incredibly slow and inefficient due to the nested recursion calls.  Although the memoization above makes the code run faster, it is at the cost of extra memory use.  The below code is syntactically recursive but actually encodes the efficient iterative process, and thus doesn't require memoization:


```python
def fibFastRec(n):
    def fib(prvprv, prv, c):
        if c < 1:
            return prvprv
        else:
            return fib(prv, prvprv + prv, c - 1)
    return fib(0, 1, n)
```


However, although much faster and not requiring memory, the above code can only work to a limited 'n' due to the limit on stack recursion depth by Python; it is better to use the iterative code above or the generative one below.


### Generative


```python
def fibGen(n):
    a, b = 0, 1
    while n>0:
        yield a
        a, b, n = b, a+b, n-1
```


### =Example use=


```python

>>> [i for i in fibGen(11)]

[0,1,1,2,3,5,8,13,21,34,55]

```


===Matrix-Based===
Translation of the matrix-based approach used in F#.

```python

def prevPowTwo(n):
    'Gets the power of two that is less than or equal to the given input'
    if ((n & -n) == n):
        return n
    else:
        n -= 1
        n |= n >> 1
        n |= n >> 2
        n |= n >> 4
        n |= n >> 8
        n |= n >> 16
        n += 1
        return (n/2)

def crazyFib(n):
    'Crazy fast fibonacci number calculation'
    powTwo = prevPowTwo(n)

    q = r = i = 1
    s = 0

    while(i < powTwo):
        i *= 2
        q, r, s = q*q + r*r, r * (q + s), (r*r + s*s)

    while(i < n):
        i += 1
        q, r, s = q+r, q, r

    return q

```



### Large step recurrence

This is much faster for a single, large value of n:

```python
def fib(n, c={0:1, 1:1}):
    if n not in c:
        x = n // 2
        c[n] = fib(x-1) * fib(n-x-1) + fib(x) * fib(n - x)
    return c[n]

fib(10000000)  # calculating it takes a few seconds, printing it takes eons
```



### Same as above but slightly faster

Putting the dictionary outside the function makes this about 2 seconds faster, could just make a wrapper:

```python
F = {0: 0, 1: 1, 2: 1}
def fib(n):
    if n in F:
        return F[n]
    f1 = fib(n // 2 + 1)
    f2 = fib((n - 1) // 2)
    F[n] = (f1 * f1 + f2 * f2 if n & 1 else f1 * f1 - f2 * f2)
    return F[n]
```



### Generative with Recursion

This can get very slow and uses a lot of memory. Can be sped up by caching the generator results.

```python
def fib():
    """Yield fib[n+1] + fib[n]"""
    yield 1  # have to start somewhere
    lhs, rhs = fib(), fib()
    yield next(lhs) # move lhs one iteration ahead
    while True:
        yield next(lhs)+next(rhs)

f=fib()
print [next(f) for _ in range(9)]
```


Output:

```txt
[1, 1, 2, 3, 5, 8, 13, 21, 34]
```


'''Another version of recursive generators solution, starting from 0'''

```Python
from itertools import islice

def fib():
    yield 0
    yield 1
    a, b = fib(), fib()
    next(b)
    while True:
        yield next(a)+next(b)

print(tuple(islice(fib(), 10)))
```



### As a scan or a fold


### =itertools.accumulate=

The Fibonacci series can be defined quite simply and efficiently as a scan or accumulation, in which the accumulator is a pair of the two last numbers.
```python
'''Fibonacci accumulation'''

from itertools import (accumulate, chain)


# fibs :: Integer :: [Integer]
def fibs(n):
    '''An accumulation of the first n integers in
       the Fibonacci series. The accumulator is a
       pair of the two preceding numbers.
    '''
    def go(ab, _):
        a, b = ab
        return (b, a + b)

    return [xy[1] for xy in accumulate(
        chain(
            [(0, 1)],
            range(1, n)
        ),
        go
    )]


# MAIN ---
if __name__ == '__main__':
    print(
        'First twenty: ' + repr(
            fibs(20)
        )
    )
```

```txt
First twenty: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
```




### =functools.reduce=


A fold can be understood as an amnesic scan, and functools.reduce can provide a useful and efficient re-write of the scanning version above, if we only need the Nth term in the series:
```python
'''Nth Fibonacci term (by folding)'''

from functools import (reduce)


# nthFib :: Integer -> Integer
def nthFib(n):
    '''Nth integer in the Fibonacci series.'''
    def go(ab, _):
        a, b = ab
        return (b, a + b)
    return reduce(go, range(1, n), (0, 1))[1]


# MAIN ---
if __name__ == '__main__':
    print(
        '1000th term: ' + repr(
            nthFib(1000)
        )
    )
```

```txt
1000th term: 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
```



## Qi


### Recursive


```qi

(define fib
  0 -> 0
  1 -> 1
  N -> (+ (fib-r (- N 1))
          (fib-r (- N 2))))

```


### Iterative


```qi

(define fib-0
  V2 V1 0 -> V2
  V2 V1 N -> (fib-0 V1 (+ V2 V1) (1- N)))

(define fib
  N -> (fib-0 0 1 N))

```



## R


### Iterative positive and negative


```python
fib=function(n,x=c(0,1)) {
   if (abs(n)>1) for (i in seq(abs(n)-1)) x=c(x[2],sum(x))
   if (n<0) return(x[2]*(-1)^(abs(n)-1)) else if (n) return(x[2]) else return(0)
}

sapply(seq(-31,31),fib)
```

Output:

```txt

 [1] 1346269 -832040  514229 -317811  196418 -121393   75025  -46368   28657
[10]  -17711   10946   -6765    4181   -2584    1597    -987     610    -377
[19]     233    -144      89     -55      34     -21      13      -8       5
[28]      -3       2      -1       1       0       1       1       2       3
[37]       5       8      13      21      34      55      89     144     233
[46]     377     610     987    1597    2584    4181    6765   10946   17711
[55]   28657   46368   75025  121393  196418  317811  514229  832040 1346269

```


### Other methods


```R
# recursive
recfibo <- function(n) {
  if ( n < 2 ) n
  else Recall(n-1) + Recall(n-2)
}

# print the first 21 elements
print.table(lapply(0:20, recfibo))

# iterative
iterfibo <- function(n) {
  if ( n < 2 )
    n
  else {
    f <- c(0, 1)
    for (i in 2:n) {
      t <- f[2]
      f[2] <- sum(f)
      f[1] <- t
    }
    f[2]
  }
}

print.table(lapply(0:20, iterfibo))

# iterative but looping replaced by map-reduce'ing
funcfibo <- function(n) {
  if (n < 2)
    n
  else {
    generator <- function(f, ...) {
      c(f[2], sum(f))
    }
    Reduce(generator, 2:n, c(0,1))[2]
  }
}

print.table(lapply(0:20, funcfibo))
```


Note that an idiomatic way to implement such low level, basic arithmethic operations in R is to implement them C and then call the compiled code.

All three solutions print

```txt
 [1] 0    1    1    2    3    5    8    13   21   34   55   89   144  233  377
[16] 610  987  1597 2584 4181 6765
```



## Ra


```Ra

class FibonacciSequence
	**Prints the nth fibonacci number**

	on start

		args := program arguments

		if args empty
			print .fibonacci(8)

		else

			try
				print .fibonacci(integer.parse(args[0]))

			catch FormatException
				print to Console.error made !, "Input must be an integer"
				exit program with error code

			catch OverflowException
				print to Console.error made !, "Number too large"
				exit program with error code

	define fibonacci(n as integer) as integer is shared
		**Returns the nth fibonacci number**

		test
			assert fibonacci(0) = 0
			assert fibonacci(1) = 1
			assert fibonacci(2) = 1
			assert fibonacci(3) = 2
			assert fibonacci(4) = 3
			assert fibonacci(5) = 5
			assert fibonacci(6) = 8
			assert fibonacci(7) = 13
			assert fibonacci(8) = 21


		body
			a, b := 0, 1

			for n
				a, b := b, a + b

			return a

```



## Racket



### Tail Recursive


```Racket

(define (fib n)
  (let loop ((cnt 0) (a 0) (b 1))
    (if (= n cnt)
        a
        (loop (+ cnt 1) b (+ a b)))))

```



```Racket

(define (fib n (a 0) (b 1))
  (if (< n 2)
      1
      (+ a (fib (- n 1) b (+ a b)))))

```



### Matrix Form


```Racket

#lang racket

(require math/matrix)

(define (fibmat n) (matrix-ref
                    (matrix-expt (matrix ([1 1]
                                          [1 0]))
                                 n)
                    1 0))

(fibmat 1000)

```



## Retro


### Recursive


```Retro
: fib ( n-m ) dup [ 0 = ] [ 1 = ] bi or if; [ 1- fib ] sip [ 2 - fib ] do + ;
```



### Iterative


```Retro
: fib ( n-N )
  [ 0 1 ] dip [ over + swap ] times drop ;
```



## REXX

With   210,000   numeric decimal digits, this REXX program can handle Fibonacci numbers past one million.

[Generally speaking, some REXX interpreters can handle up to around eight million decimal digits.]

This version of the REXX program can also handle   ''negative''   Fibonacci numbers.

```rexx
/*REXX program calculates the  Nth  Fibonacci number,   N   can be  zero  or  negative. */
numeric digits 210000                            /*be able to handle ginormous numbers. */
parse  arg  x y .                                /*allow a  single number  or  a range. */
if x=='' | x==","  then do;  x=-40;  y=+40;  end /*No input?  Then use range -40 ──► +40*/
if y=='' | y==","  then y=x                      /*if only one number, display   fib(X).*/
w= max(length(x),  length(y) )                   /*W:  used for making formatted output.*/
fw= 10                                           /*Minimum maximum width. Sounds ka─razy*/
      do j=x  to y;          q= fib(j)           /*process all of the Fibonacci requests*/
      L= length(q)                               /*obtain the length (decimal digs) of Q*/
      fw= max(fw, L)                             /*fib number length, or the max so far.*/
      say 'Fibonacci('right(j,w)") = "  right(q,fw)                   /*right justify Q.*/
      if L>10  then  say    'Fibonacci('right(j, w)") has a length of"     L
      end   /*j*/                                /* [↑]  list a Fib. sequence of  x──►y */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fib:  procedure; parse arg n;        an= abs(n)  /*use  │n│   (the absolute value of N).*/
      a= 0;    b= 1;   if an<2  then return an   /*handle two special cases: zero & one.*/
                                                 /* [↓]   this method is non─recursive. */
         do k=2  to an;   $= a+b;   a= b;   b= $ /*sum  the numbers  up to   │n│        */
         end   /*k*/                             /* [↑]  (only positive Fibs nums used).*/
                                                 /* [↓]  an//2   [same as]   (an//2==1).*/
      if n>0 | an//2   then  return  $           /*Positive or even?   Then return sum. */
                             return -$           /*Negative and odd? Return negative sum*/
```

<pre style="height:63ex">
Fibonacci(-40) =  -102334155
Fibonacci(-39) =    63245986
Fibonacci(-38) =   -39088169
Fibonacci(-37) =    24157817
Fibonacci(-36) =   -14930352
Fibonacci(-35) =     9227465
Fibonacci(-34) =    -5702887
Fibonacci(-33) =     3524578
Fibonacci(-32) =    -2178309
Fibonacci(-31) =     1346269
Fibonacci(-30) =     -832040
Fibonacci(-29) =      514229
Fibonacci(-28) =     -317811
Fibonacci(-27) =      196418
Fibonacci(-26) =     -121393
Fibonacci(-25) =       75025
Fibonacci(-24) =      -46368
Fibonacci(-23) =       28657
Fibonacci(-22) =      -17711
Fibonacci(-21) =       10946
Fibonacci(-20) =       -6765
Fibonacci(-19) =        4181
Fibonacci(-18) =       -2584
Fibonacci(-17) =        1597
Fibonacci(-16) =        -987
Fibonacci(-15) =         610
Fibonacci(-14) =        -377
Fibonacci(-13) =         233
Fibonacci(-12) =        -144
Fibonacci(-11) =          89
Fibonacci(-10) =         -55
Fibonacci( -9) =          34
Fibonacci( -8) =         -21
Fibonacci( -7) =          13
Fibonacci( -6) =          -8
Fibonacci( -5) =           5
Fibonacci( -4) =          -3
Fibonacci( -3) =           2
Fibonacci( -2) =          -1
Fibonacci( -1) =           1
Fibonacci(  0) =           0
Fibonacci(  1) =           1
Fibonacci(  2) =           1
Fibonacci(  3) =           2
Fibonacci(  4) =           3
Fibonacci(  5) =           5
Fibonacci(  6) =           8
Fibonacci(  7) =          13
Fibonacci(  8) =          21
Fibonacci(  9) =          34
Fibonacci( 10) =          55
Fibonacci( 11) =          89
Fibonacci( 12) =         144
Fibonacci( 13) =         233
Fibonacci( 14) =         377
Fibonacci( 15) =         610
Fibonacci( 16) =         987
Fibonacci( 17) =        1597
Fibonacci( 18) =        2584
Fibonacci( 19) =        4181
Fibonacci( 20) =        6765
Fibonacci( 21) =       10946
Fibonacci( 22) =       17711
Fibonacci( 23) =       28657
Fibonacci( 24) =       46368
Fibonacci( 25) =       75025
Fibonacci( 26) =      121393
Fibonacci( 27) =      196418
Fibonacci( 28) =      317811
Fibonacci( 29) =      514229
Fibonacci( 30) =      832040
Fibonacci( 31) =     1346269
Fibonacci( 32) =     2178309
Fibonacci( 33) =     3524578
Fibonacci( 34) =     5702887
Fibonacci( 35) =     9227465
Fibonacci( 36) =    14930352
Fibonacci( 37) =    24157817
Fibonacci( 38) =    39088169
Fibonacci( 39) =    63245986
Fibonacci( 40) =   102334155

```

'''output'''   when the following was used as input:   <tt> 10000 </tt>

```txt

Fibonacci(10000) =  3364476487643178326662161200510754331030214846068006390656476997468008144216666236815559551363373402558206533268083615937373479048386526826304089246305643188735454436955982749160660209988418393386465273130008883026923567361313511757929743785441375213052050434770160226475831890652789085515436615958298727968298751063120057542878345321551510387081829896979161312785626503319548714021428753269818796204693609787990035096230229102636813149319527563022783762844154036058440257211433496118002309120828704608892396232883546150577658327125254609359112820392528539343462090424524892940390
170623388899108584106518317336043747073790855263176432573399371287193758774689747992630583706574283016163740896917842637862421283525811282051637029808933209990570792006436742620238978311147005407499845925036063356093388383192338678305613643535189213327973290813373264265263398976392272340788292817795358057099369104917547080893184105614632233821746563732124822638309210329770164805472624384237486241145309381220656491403275108664339451751216152654536133311131404243685480510676584349352383695965342807176877532834823434555736671973139274627362910821067928078471803532913117677892465908993863545932789
452377767440619224033763867400402133034329749690202832814593341882681768389307200363479562311710310129195316979460763273758925353077255237594378843450406771555577905645044301664011946258097221672975861502696844314695203461493229110597067624326851599283470989128470674086200858713501626031207190317208609408129832158107728207635318662461127824553720853236530577595643007251774431505153960090516860322034916322264088524885243315805153484962243484829938090507048348244932745373262456775587908918719080366205800959474315005240253270974699531877072437682590741993963226598414749819360928522394503970716544
3156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875
Fibonacci(10000) has a length of  2090  decimal digits

```



## Rockstar

===Iterative (minimized)===

```Rockstar

Fibonacci takes Number
  FNow is 0
  FNext is 1
  While FNow is less than Number
    Say FNow
    Put FNow into Temp
    Put FNow into FNext
    Put FNext plus Temp into FNext

Say Fibonacci taking 1000 (prints out highest number in Fibonacci sequence less than 1000)

```


===Iterative (idiomatic)===

```Rockstar

Love takes Time
My love was addictions
Put my love into your heart
Build it up
Until my love is as strong as Time
Whisper my love
Put my love into a river
Put your heart into my love
Put it with a river into your heart


Shout; Love taking 1000 (years, years)

```


The semicolon and the comment <code>(years, years)</code> in this version are there only for poetic effect


## Ring


```ring

give n
x = fib(n)
see n + " Fibonacci is : " + x

func fib nr if nr = 0 return 0 ok
            if nr = 1 return 1 ok
            if nr > 1 return fib(nr-1) + fib(nr-2) ok

```



## Ruby


### Iterative


```ruby
def fib(n, sequence=[1])
  n.times do
    current_number, last_number = sequence.last(2)
    sequence << current_number + (last_number or 0)
  end

  sequence.last
end
```



### Recursive


```ruby
def fib(n, sequence=[1])
  return sequence.last if n == 0

  current_number, last_number = sequence.last(2)
  sequence << current_number + (last_number or 0)

  fib(n-1, sequence)
end

```



### Recursive with Memoization


```ruby
# Use the Hash#default_proc feature to
# lazily calculate the Fibonacci numbers.

fib = Hash.new do |f, n|
  f[n] = if n <= -2
           (-1)**(n + 1) * f[n.abs]
         elsif n <= 1
           n.abs
         else
           f[n - 1] + f[n - 2]
         end
end
# examples: fib[10] => 55, fib[-10] => (-55/1)
```



### Matrix


```ruby
require 'matrix'

# To understand why this matrix is useful for Fibonacci numbers, remember
# that the definition of Matrix.**2 for any Matrix[[a, b], [c, d]] is
# is [[a*a + b*c, a*b + b*d], [c*a + d*b, c*b + d*d]].  In other words, the
# lower right element is computing F(k - 2) + F(k - 1) every time M is multiplied
# by itself (it is perhaps easier to understand this by computing M**2, 3, etc, and
# watching the result march up the sequence of Fibonacci numbers).

M = Matrix[[0, 1], [1,1]]

# Matrix exponentiation algorithm to compute Fibonacci numbers.
# Let M be Matrix [[0, 1], [1, 1]].  Then, the lower right element of M**k is
# F(k + 1).  In other words, the lower right element of M is F(2) which is 1, and the
# lower right element of M**2 is F(3) which is 2, and the lower right element
# of M**3 is F(4) which is 3, etc.
#
# This is a good way to compute F(n) because the Ruby implementation of Matrix.**(n)
# uses O(log n) rather than O(n) matrix multiplications.  It works by squaring squares
# ((m**2)**2)... as far as possible
# and then multiplying that by by M**(the remaining number of times).  E.g., to compute
# M**19, compute partial = ((M**2)**2) = M**16, and then compute partial*(M**3) = M**19.
# That's only 5 matrix multiplications of M to compute M*19.
def self.fib_matrix(n)
  return 0 if n <= 0 # F(0)
  return 1 if n == 1 # F(1)
  # To get F(n >= 2), compute M**(n - 1) and extract the lower right element.
  return CS::lower_right(M**(n - 1))
end

# Matrix utility to return
# the lower, right-hand element of a given matrix.
def self.lower_right matrix
  return nil if matrix.row_size == 0
  return matrix[matrix.row_size - 1, matrix.column_size - 1]
end
```



### Generative


```ruby
fib = Enumerator.new do |y|
  f0, f1 = 0, 1
  loop do
    y <<  f0
    f0, f1 = f1, f0 + f1
  end
end
```


Usage:


```txt
p fib.lazy.drop(8).next # => 21
```

"Fibers are primitives for implementing light weight cooperative concurrency in Ruby. Basically they are a means of creating code blocks that can be paused and resumed, much like threads. The main difference is that they are never preempted and that the scheduling must be done by the programmer and not the VM." [http://www.ruby-doc.org/ruby-1.9/classes/Fiber.html]


```ruby
fib = Fiber.new do
  a,b = 0,1
  loop do
    Fiber.yield a
    a,b = b,a+b
  end
end
9.times {puts fib.resume}
```


using a lambda

```ruby
def fib_gen
    a, b = 1, 1
    lambda {ret, a, b = a, b, a+b; ret}
end
```



```txt

irb(main):034:0> fg = fib_gen
=> #<Proc:0xb7cdf750@(irb):22>
irb(main):035:0> 9.times { puts fg.call}
1
1
2
3
5
8
13
21
34
=> 9

```


===Binet's Formula===

```ruby
def fib
    phi = (1 + Math.sqrt(5)) / 2
    ((phi**self - (-1 / phi)**self) / Math.sqrt(5)).to_i
end
```


```txt

1.9.3p125 :001 > def fib
1.9.3p125 :002?>   phi = (1 + Math.sqrt(5)) / 2
1.9.3p125 :003?>   ((phi**self - (-1 / phi)**self) / Math.sqrt(5)).to_i
1.9.3p125 :004?>   end
 => nil
1.9.3p125 :005 > (0..10).map(&:fib)
 => [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

```



## Run BASIC


```runbasic
for i = 0 to 10
 print i;" ";fibR(i);" ";fibI(i)
next i
end

 function fibR(n)
 if n < 2 then fibR = n else fibR = fibR(n-1) + fibR(n-2)
 end function

 function fibI(n)
   b = 1
   for i = 1 to n
       t = a + b
       a = b
       b = t
   next i
fibI = a
end function
```



## Rust


### Iterative


```rust
use std::mem;
fn main() {
    let mut prev = 0;
    // Rust needs this type hint for the checked_add method
    let mut curr = 1usize;

    while let Some(n) = curr.checked_add(prev) {
        prev = curr;
        curr = n;
        println!("{}", n);
    }
}
```



### Recursive


```rust
use std::mem;
fn main() {
    fibonacci(0,1);
}

fn fibonacci(mut prev: usize, mut curr: usize) {
    mem::swap(&mut prev, &mut curr);
    if let Some(n) = curr.checked_add(prev) {
        println!("{}", n);
        fibonacci(prev, n);
    }
}
```


===Recursive (with pattern matching)===

```rust
fn fib(n: u32) -> u32 {
    match n {
        0 => 0,
        1 => 1,
        n => fib(n - 1) + fib(n - 2),
    }
}
```


===Tail recursive (with pattern matching)===

```rust
fn fib_tail_recursive(nth: usize) -> usize {
  fn fib_tail_iter(n: usize, prev_fib: usize, fib: usize) -> usize {
    match n {
      0 => prev_fib,
      n => fib_tail_iter(n - 1, fib, prev_fib + fib),
    }
  }
  fib_tail_iter(nth, 0, 1)
}
```



### Analytic

This uses a feature from nightly Rust which makes it possible to (cleanly) return an iterator without the additional overhead of putting it on the heap. In stable Rust, we'd need to return a <code>Box<Iterator<Item=u64>></code> which has the cost of an additional allocation and the overhead of dynamic dispatch. The version below does not require the use of the heap and is done entirely through static dispatch.


```rust
#![feature(conservative_impl_trait)]

fn main() {
    for num in fibonacci_gen(10) {
        println!("{}", num);
    }
}

fn fibonacci_gen(terms: i32) -> impl Iterator<Item=u64> {
    let sqrt_5 = 5.0f64.sqrt();
    let p  = (1.0 + sqrt_5) / 2.0;
    let q = 1.0/p;
    (1..terms).map(move |n| ((p.powi(n) + q.powi(n)) / sqrt_5 + 0.5) as u64)
}
```



### Using an Iterator

Iterators are very idiomatic in rust, though they may be overkill for such a simple problem.

```rust
use std::mem;

struct Fib {
    prev: usize,
    curr: usize,
}

impl Fib {
    fn new() -> Self {
        Fib {prev: 0, curr: 1}
    }
}

impl Iterator for Fib {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item>{
        mem::swap(&mut self.curr, &mut self.prev);
        self.curr.checked_add(self.prev).map(|n| {
            self.curr = n;
            n
        })
    }
}

fn main() {
    for num in Fib::new() {
        println!("{}", num);
    }
}
```



## SAS



###  Iterative


This code builds a table <code>fib</code> holding the first few values of the Fibonacci sequence.


```sas
data fib;
    a=0;
    b=1;
    do n=0 to 20;
       f=a;
       output;
       a=b;
       b=f+a;
    end;
    keep n f;
run;
```



###  Naive recursive


This code provides a simple example of defining a function and using it recursively. One of the members of the sequence is written to the log.


```sas
options cmplib=work.f;

proc fcmp outlib=work.f.p;
    function fib(n);
    if n = 0 or n = 1
        then return(1);
        else return(fib(n - 2) + fib(n - 1));
    endsub;
run;

data _null_;
    x = fib(5);
    put 'fib(5) = ' x;
run;
```



## Sather

The implementations use the arbitrary precision class INTI.

```sather
class MAIN is

  -- RECURSIVE --
  fibo(n :INTI):INTI
    pre n >= 0
  is
    if n < 2.inti then return n; end;
    return fibo(n - 2.inti) + fibo(n - 1.inti);
  end;

  -- ITERATIVE --
  fibo_iter(n :INTI):INTI
    pre n >= 0
  is
    n3w :INTI;

    if n < 2.inti then return n; end;
    last ::= 0.inti; this ::= 1.inti;
    loop (n - 1.inti).times!;
      n3w := last + this;
      last := this;
      this := n3w;
    end;
    return this;
  end;

  main is
    loop i ::= 0.upto!(16);
      #OUT + fibo(i.inti) + " ";
      #OUT + fibo_iter(i.inti) + "\n";
    end;
  end;

end;
```


=={{header|S-BASIC}}==
Note that the 23rd Fibonacci number (=28657) is the largest that can be generated without overflowing S-BASIC's integer data type.

```basic

rem - iterative function to calculate nth fibonacci number
function fibonacci(n = integer) = integer
var f, i, p1, p2 = integer
p1 = 0
p2 = 1
if n = 0 then
   f = 0
else
   for i = 1 to n
      f = p1 + p2
      p2 = p1
      p1 = f
   next i
end = f

rem - exercise the function
var i = integer
for i = 0 to 10
   print fibonacci(i);
next i

end

```

```txt
 0 1 1 2 3 5 8 13 21 34 55

```



## Scala


### Recursive


```scala
def fib(i:Int):Int = i match{
    case 0 => 0
    case 1 => 1
    case _ => fib(i-1) + fib(i-2)
}
```



### Lazy sequence


```scala
lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map{case (a,b) => a + b}
```



### Tail recursive


```scala
def fib(x:Int, prev: BigInt = 0, next: BigInt = 1):BigInt = x match {
    case 0 => prev
    case _ => fib(x-1, next, next + prev)
 }
```



### foldLeft


```scala
// Fibonacci using BigInt with Stream.foldLeft optimized for GC (Scala v2.9 and above)
// Does not run out of memory for very large Fibonacci numbers
def fib(n:Int) = {

  def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j, i+j)

  series(1,0).take(n).foldLeft(BigInt("0"))(_+_)
}

// Small test
(0 to 13) foreach {n => print(fib(n).toString + " ")}

// result: 0 1 1 2 3 5 8 13 21 34 55 89 144 233

```



### Iterator


```scala
val it = Iterator.iterate((0,1)){case (a,b) => (b,a+b)}.map(_._1)
//example:
println(it.take(13).mkString(",")) //prints: 0,1,1,2,3,5,8,13,21,34,55,89,144
```



## Scheme


### Iterative


```scheme
(define (fib-iter n)
  (do ((num 2 (+ num 1))
       (fib-prev 1 fib)
       (fib 1 (+ fib fib-prev)))
      ((>= num n) fib)))
```



### Recursive


```scheme
(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1))
         (fib-rec (- n 2)))))
```


This version is tail recursive:

```scheme
(define (fib n)
  (let loop ((a 0) (b 1) (n n))
    (if (= n 0) a
        (loop b (+ a b) (- n 1)))))

```



### Recursive Sequence Generator


Although the tail recursive version above is quite efficient, it only generates the final nth Fibonacci number and not the sequence up to that number without wasteful repeated calls to the procedure/function.

The following procedure generates the sequence of Fibonacci numbers using a simplified version of a lazy list/stream - since no memoization is requried, it just implements future values by using a zero parameter lambda "thunk" with a closure containing the last and the pre-calculated next value of the sequence; in this way it uses almost no memory during the sequence generation other than as required for the last and the next values of the sequence (note that the test procedure does not generate a linked list to contain the elements of the sequence to show, but rather displays each one by one in sequence):


```scheme

(define (fib)
  (define (nxt lv nv) (cons nv (lambda () (nxt nv (+ lv nv)))))
  (cons 0 (lambda () (nxt 0 1))))

;;; test...
(define (show-stream-take n strm)
  (define (shw-nxt n strm) (begin (display (car strm))
                                  (if (> n 1) (begin (display " ") (shw-nxt (- n 1) ((cdr strm)))) (display ")"))))
  (begin (display "(") (shw-nxt n strm)))
(show-stream-take 30 (fib))
```


```txt
(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229)
```



### Dijkstra Algorithm


```scheme
;;; Fibonacci numbers using Edsger Dijkstra's algorithm
;;; http://www.cs.utexas.edu/users/EWD/ewd06xx/EWD654.PDF

(define (fib n)
  (define (fib-aux a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-aux a
                    b
                    (+ (* p p) (* q q))
                    (+ (* q q) (* 2 p q))
                    (/ count 2)))
          (else
           (fib-aux (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)))))
  (fib-aux 1 0 0 1 n))
```



## Scilab

<lang>    clear
    n=46
    f1=0; f2=1
    printf("fibo(%d)=%d\n",0,f1)
    printf("fibo(%d)=%d\n",1,f2)
    for i=2:n
        f3=f1+f2
        printf("fibo(%d)=%d\n",i,f3)
        f1=f2
        f2=f3
    end
```

```txt
...
fibo(43)=433494437
fibo(44)=701408733
fibo(45)=1134903170
fibo(46)=1836311903
```



## sed


```sed
#!/bin/sed -f

# First we need to convert each number into the right number of ticks
# Start by marking digits
s/[0-9]/<&/g

# We have to do the digits manually.
s/0//g; s/1/|/g; s/2/||/g; s/3/|||/g; s/4/||||/g; s/5/|||||/g
s/6/||||||/g; s/7/|||||||/g; s/8/||||||||/g; s/9/|||||||||/g

# Multiply by ten for each digit from the front.
:tens
s/|</<||||||||||/g
t tens

# Done with digit markers
s/<//g

# Now the actual work.
:split
# Convert each stretch of n >= 2 ticks into two of n-1, with a mark between
s/|\(|\+\)/\1-\1/g
# Convert the previous mark and the first tick after it to a different mark
# giving us n-1+n-2 marks.
s/-|/+/g
# Jump back unless we're done.
t split
# Get rid of the pluses, we're done with them.
s/+//g

# Convert back to digits
:back
s/||||||||||/</g
s/<\([0-9]*\)$/<0\1/g
s/|||||||||/9/g;
s/|||||||||/9/g; s/||||||||/8/g; s/|||||||/7/g; s/||||||/6/g;
s/|||||/5/g; s/||||/4/g; s/|||/3/g; s/||/2/g; s/|/1/g;
s/</|/g
t back
s/^$/0/
```



## Seed7


### Recursive


```seed7
const func integer: fib (in integer: number) is func
  result
    var integer: result is 1;
  begin
    if number > 2 then
      result := fib(pred(number)) + fib(number - 2);
    elsif number = 0 then
      result := 0;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#fib]


### Iterative

This funtion uses a bigInteger result:


```seed7
const func bigInteger: fib (in integer: number) is func
  result
    var bigInteger: result is 1_;
  local
    var integer: i is 0;
    var bigInteger: a is 0_;
    var bigInteger: c is 0_;
  begin
    for i range 1 to pred(number) do
      c := a;
      a := result;
      result +:= c;
    end for;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#iterative_fib]


## SequenceL


### Recursive


```sequencel
fibonacci(n) :=
		n when n < 2
	else
		fibonacci(n - 1) + fibonacci(n - 2);
```

Based on: [https://www.youtube.com/watch?v=5JVC5dDtnyg]


### Tail Recursive


```sequencel
fibonacci(n) := fibonacciHelper(0, 1, n);

fibonacciHelper(prev, next, n) :=
		prev when n < 1
	else
		next when n = 1
	else
		fibonacciHelper(next, next + prev, n - 1);
```



### Matrix


```sequencel
fibonacci(n) := fibonacciHelper([[1,0],[0,1]], n);

fibonacciHelper(M(2), n) :=
	let
		N := [[1,1],[1,0]];
	in
		M[1,1] when n <= 1
	else
		fibonacciHelper(matmul(M, N), n - 1);

matmul(A(2), B(2)) [i,j] := sum( A[i,all] * B[all,j] );
```

Based on the C# version: [http://rosettacode.org/wiki/Fibonacci_sequence#C.23]

Using the SequenceL Matrix Multiply solution: [http://rosettacode.org/wiki/Matrix_multiplication#SequenceL]


## SETL


```setl
$ Print out the first ten Fibonacci numbers
$ This uses Set Builder Notation, it roughly means
$ 'collect fib(n) forall n in {0,1,2,3,4,5,6,7,8,9,10}'
print({fib(n) : n in {0..10}});

$ Iterative Fibonacci function
proc fib(n);
    A := 0; B := 1; C := n;
    for i in {0..n} loop
        C := A + B;
        A := B;
        B := C;
    end loop;
    return C;
end proc;
```




## Shen


```Shen
(define fib
  0 -> 0
  1 -> 1
  N -> (+ (fib (+ N 1)) (fib (+ N 2)))
       where (< N 0)
  N -> (+ (fib (- N 1)) (fib (- N 2))))
```



## Sidef


### Iterative


```ruby
func fib_iter(n) {
    var (a, b) = (0, 1)
    { (a, b) = (b, a+b) } * n
    return a
}
```



### Recursive


```ruby
func fib_rec(n) {
    n < 2 ? n : (__FUNC__(n-1) + __FUNC__(n-2))
}
```



### Recursive with memoization


```ruby
func fib_mem (n) is cached {
    n < 2 ? n : (__FUNC__(n-1) + __FUNC__(n-2))
}
```


===Closed-form===

```ruby
func fib_closed(n) {
    define S = (1.25.sqrt + 0.5)
    define T = (-S + 1)
    (S**n - T**n) / (-T + S) -> round
}
```


===Built-in===

```ruby
say fib(12)    #=> 144
```



## Simula

Straightforward iterative implementation.

```simula
INTEGER PROCEDURE fibonacci(n);
INTEGER n;
BEGIN
    INTEGER lo, hi, temp, i;
    lo := 0;
    hi := 1;
    FOR i := 1 STEP 1 UNTIL n - 1 DO
    BEGIN
        temp := hi;
        hi := hi + lo;
        lo := temp
    END;
    fibonacci := hi
END;
```



## SkookumScript


===Built-in===

SkookumScript's <code>Integer</code> class has a fast built-in <code>fibonnaci()</code> method.


```javascript>42.fibonacci</lang


SkookumScript is designed to work in tandem with C++ and its strength is at the high-level stage-direction of things. So when confronted with [http://skookumscript.com/blog/2016/07-11-fibonacci/ benchmarking scripting systems] it is genrally better to make a built-in call. Though in most practical cases this isn't necessary.


### Recursive


Simple recursive method in same <code>42.fibonacci</code> form as built-in form above.


```javascript
// Assuming code is in Integer.fibonacci() method
() Integer
  [
  if this < 2 [this] else [[this - 1].fibonacci + [this - 2].fibonacci]
  ]
```


Recursive procedure in <code>fibonacci(42)</code> form.


```javascript
// Assuming in fibonacci(n) procedure
(Integer n) Integer
  [
  if n < 2 [n] else [fibonacci(n - 1) + fibonacci(n - 2)]
  ]
```



### Iterative


Iterative method in <code>42.fibonacci</code> form.


```javascript
// Assuming code is in Integer.fibonacci() method
() Integer
  [
  if this < 2
    [this]
  else
    [
    !prev: 1
    !next: 1
    2.to_pre this
      [
      !sum :  prev + next
      prev := next
      next := sum
      ]

    next
    ]
  ]
```


Optimized iterative method in <code>42.fibonacci</code> form.
Though the best optimiation is to write it in C++ as with the built-in form that comes with SkookumScript.


```javascript
// Bind : is faster than assignment :=
// loop is faster than to_pre (which uses a closure)
() Integer
  [
  if this < 2
    [this]
  else
    [
    !prev: 1
    !next: 1
    !sum
    !count: this - 2
    loop
      [
      if count = 0 [exit]
      count--
      sum  : prev + next
      prev : next
      next : sum
      ]

    next
    ]
  ]
```




## Slate


```slate
n@(Integer traits) fib
[
  n <= 0 ifTrue: [^ 0].
  n = 1 ifTrue: [^ 1].
  (n - 1) fib + (n - 2) fib
].

slate[15]> 10 fib = 55.
True
```



## Smalltalk


```smalltalk
|fibo|
fibo := [ :i |
   |ac t|
   ac := Array new: 2.
   ac at: 1 put: 0 ; at: 2 put: 1.
   ( i < 2 )
     ifTrue: [ ac at: (i+1) ]
     ifFalse: [
        2 to: i do: [ :l |
          t := (ac at: 2).
          ac at: 2 put: ( (ac at: 1) + (ac at: 2) ).
          ac at: 1 put: t
        ].
        ac at: 2.
     ]
].

0 to: 10 do: [ :i |
  (fibo value: i) displayNl
]
```




## smart BASIC


The Iterative method is slow (relatively) and the Recursive method doubly so since it references the Iterative function twice.

The N-th Term (fibN) function is much faster as it utilizes Binet's Formula.

<ul>
<li>fibR: Fibonacci Recursive</li>
<li>fibI: Fibonacci Iterative</li>
<li>fibN: Fibonacci N-th Term</li>
</ul>


```qbasic
FOR i = 0 TO 15
    PRINT fibR(i),fibI(i),fibN(i)
NEXT i

/* Recursive Method */
DEF fibR(n)
    IF n <= 1 THEN
        fibR = n
    ELSE
        fibR = fibR(n-1) + fibR(n-2)
    ENDIF
END DEF

/* Iterative Method */
DEF fibI(n)
    a = 0
    b = 1
    FOR i = 1 TO n
        temp = a + b
        a = b
        b = temp
    NEXT i
    fibI = a
END DEF

/* N-th Term Method */
DEF fibN(n)
    uphi = .5 + SQR(5)/2
    lphi = .5 - SQR(5)/2
    fibN = (uphi^n-lphi^n)/SQR(5)
END DEF
```



## SNOBOL4



### Recursive


```snobol
	define("fib(a)")	:(fib_end)
fib	fib = lt(a,2) a	:s(return)
	fib = fib(a - 1) + fib(a - 2)	:(return)
fib_end

while	a = trim(input)	:f(end)
	output = a " " fib(a)	:(while)
end
```


===Tail-recursive===

```SNOBOL4
        define('trfib(n,a,b)') :(trfib_end)
trfib   trfib = eq(n,0) a :s(return)
        trfib = trfib(n - 1, a + b, a) :(return)
trfib_end
```



### Iterative


```SNOBOL4
        define('ifib(n)f1,f2') :(ifib_end)
ifib    ifib = le(n,2) 1 :s(return)
        f1 = 1; f2 = 1
if1     ifib = gt(n,2) f1 + f2 :f(return)
        f1 = f2; f2 = ifib; n = n - 1 :(if1)
ifib_end
```



### Analytic

Note: Snobol4+ lacks built-in sqrt( ) function.


```SNOBOL4
        define('afib(n)s5') :(afib_end)
afib    s5 = sqrt(5)
        afib = (((1 + s5) / 2) ^ n - ((1 - s5) / 2) ^ n) / s5
        afib = convert(afib,'integer') :(return)
afib_end
```


Test and display all, Fib 1 .. 10


```SNOBOL4
loop    i = lt(i,10) i + 1 :f(show)
        s1 = s1 fib(i) ' ' ; s2 = s2 trfib(i,0,1) ' '
        s3 = s3 ifib(i) ' '; s4 = s4 afib(i) ' ' :(loop)
show    output = s1; output = s2; output = s3; output = s4
end
```


Output:

```txt
1 1 2 3 5 8 13 21 34 55
1 1 2 3 5 8 13 21 34 55
1 1 2 3 5 8 13 21 34 55
1 1 2 3 5 8 13 21 34 55
```



## SNUSP

This is modular SNUSP (which introduces @ and # for threading).


### Iterative


```snusp
 @!\+++++++++#  /<<+>+>-\
fib\==>>+<<?!/>!\      ?/\
  #<</?\!>/@>\?-<<</@>/@>/>+<-\
     \-/  \       !\ !\ !\   ?/#
```



### Recursive


```snusp>             /
### ==
\    />
+<<-\  />+<-\
fib==!/?!\-?!\->+>+<<?/>>-@\
### ==?/<@\
?/<#
      |  #+==/     fib(n-2)|+fib(n-1)|
      \
### ==recursion===
/!
### ==
/
```



## Softbridge BASIC


### Iterative


```basic

Function Fibonacci(n)
   x = 0
   y = 1
   i = 0
   n = ABS(n)
   If n < 2 Then
   Fibonacci = n
   Else
   Do Until (i = n)
      sum = x+y
      x=y
      y=sum
      i=i+1
   Loop
   Fibonacci = x
   End If

End Function

```



## Spin


###  Iterative

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | i
  ser.start(31, 30, 0, 115200)

  repeat i from 0 to 10
    ser.dec(fib(i))
    ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)

pub fib(i) : b | a
  b := a := 1
  repeat i
    a := b + (b := a)
```

```txt
1 1 2 3 5 8 13 21 34 55 89
```



## SPL


###  Analytic


```spl
fibo(n)=
  s5 = #.sqrt(5)
  <= (((1+s5)/2)^n-((1-s5)/2)^n)/s5
.
```


###  Iterative


```spl
fibo(n)=
  ? n<2, <= n
  f2 = 0
  f1 = 1
  > i, 2..n
    f = f1+f2
    f2 = f1
    f1 = f
  <
  <= f
.
```


###  Recursive


```spl
fibo(n)=
  ? n<2, <= n
  <= fibo(n-1)+fibo(n-2)
.
```



## SQL


###  Analytic

As a running sum:

```SQL

select round ( exp ( sum (ln ( ( 1 + sqrt( 5 ) ) / 2)
        ) over ( order by level ) ) / sqrt( 5 ) ) fibo
from dual
connect by level <= 10;

```


```txt

       FIB
----------
         1
         1
         2
         3
         5
         8
        13
        21
        34
        55

10 rows selected.

```

As a power:

```SQL

select round ( power( ( 1 + sqrt( 5 ) ) / 2, level ) / sqrt( 5 ) ) fib
from dual
connect by level <= 10;

```


```txt

       FIB
----------
         1
         1
         2
         3
         5
         8
        13
        21
        34
        55

10 rows selected.

```


###  Recursive


Oracle 12c required

```sql

SQL> with fib(e,f) as (select 1, 1 from dual union all select e+f,e from fib where e <= 55) select f from fib;

         F
----------
         1
         1
         2
         3
         5
         8
        13
        21
        34
        55

10 rows selected.

```


```postgresql
CREATE FUNCTION fib(n int) RETURNS numeric AS $$
    -- This recursive with generates endless list of Fibonacci numbers.
    WITH RECURSIVE fibonacci(current, previous) AS (
        -- Initialize the current with 0, so the first value will be 0.
        -- The previous value is set to 1, because its only goal is not
        -- special casing the zero case, and providing 1 as the second
        -- number in the sequence.
        --
        -- The numbers end with dots to make them numeric type in
        -- Postgres. Numeric type has almost arbitrary precision
        -- (technically just 131,072 digits, but that's good enough for
        -- most purposes, including calculating huge Fibonacci numbers)
        SELECT 0., 1.
    UNION ALL
        -- To generate Fibonacci number, we need to add together two
        -- previous Fibonacci numbers. Current number is saved in order
        -- to be accessed in the next iteration of recursive function.
        SELECT previous + current, current FROM fibonacci
    )
    -- The user is only interested in current number, not previous.
    SELECT current FROM fibonacci
    -- We only need one number, so limit to 1
    LIMIT 1
    -- Offset the query by the requested argument to get the correct
    -- position in the list.
    OFFSET n
$$ LANGUAGE SQL RETURNS NULL ON NULL INPUT IMMUTABLE;
```



## SSEM

Calculates the tenth Fibonacci number. To calculate the <i>n</i>th, change the initial value of the counter to <i>n</i>-1 (subject to the restriction that the answer must be small enough to fit in a signed 32-bit integer, the SSEM's only data type). The algorithm is basically straightforward, but the absence of an Add instruction makes the implementation a little more complicated than it would otherwise be.

```ssem
10101000000000100000000000000000    0. -21 to c    acc = -n
01101000000001100000000000000000    1. c   to 22   temp = acc
00101000000001010000000000000000    2. Sub. 20     acc -= m
10101000000001100000000000000000    3. c   to 21   n = acc
10101000000000100000000000000000    4. -21 to c    acc = -n
10101000000001100000000000000000    5. c   to 21   n = acc
01101000000000100000000000000000    6. -22 to c    acc = -temp
00101000000001100000000000000000    7. c   to 20   m = acc
11101000000000100000000000000000    8. -23 to c    acc = -count
00011000000001010000000000000000    9. Sub. 24     acc -= -1
00000000000000110000000000000000   10. Test        skip next if acc<0
10011000000000000000000000000000   11. 25  to CI   goto (15 + 1)
11101000000001100000000000000000   12. c   to 23   count = acc
11101000000000100000000000000000   13. -23 to c    acc = -count
11101000000001100000000000000000   14. c   to 23   count = acc
00011000000000000000000000000000   15. 24  to CI   goto (-1 + 1)
10101000000000100000000000000000   16. -21 to c    acc = -n
10101000000001100000000000000000   17. c   to 21   n = acc
10101000000000100000000000000000   18. -21 to c    acc = -n
00000000000001110000000000000000   19. Stop
00000000000000000000000000000000   20. 0           var m = 0
10000000000000000000000000000000   21. 1           var n = 1
00000000000000000000000000000000   22. 0           var temp = 0
10010000000000000000000000000000   23. 9           var count = 9
11111111111111111111111111111111   24. -1          const -1
11110000000000000000000000000000   25. 15          const 15
```



## Stata



```stata
program fib
args n
clear
qui set obs `n'
qui gen a=1
qui replace a=a[_n-1]+a[_n-2] in 3/l
end
```


An implementation using '''[https://www.stata.com/help.cgi?dyngen dyngen]'''.


```stata
program fib
args n
clear
qui set obs `n'
qui gen a=.
dyngen {
	update a=a[_n-1]+a[_n-2], missval(1)
}
end

fib 10
list
```


'''Output'''


```txt
     +----+
     |  a |
     |----|
  1. |  1 |
  2. |  1 |
  3. |  2 |
  4. |  3 |
  5. |  5 |
     |----|
  6. |  8 |
  7. | 13 |
  8. | 21 |
  9. | 34 |
 10. | 55 |
     +----+
```



###  Mata


```stata
. mata
: function fib(n) {
        return((((1+sqrt(5))/2):^n-((1-sqrt(5))/2):^n)/sqrt(5))
}

: fib(0..10)
        1    2    3    4    5    6    7    8    9   10   11
    +--------------------------------------------------------+
  1 |   0    1    1    2    3    5    8   13   21   34   55  |
    +--------------------------------------------------------+

: end
```



## StreamIt


```streamit
void->int feedbackloop Fib {
    join roundrobin(0,1);
    body in->int filter {
        work pop 1 push 1 peek 2 { push(peek(0) + peek(1)); pop(); }
    };
    loop Identity<int>;
    split duplicate;
    enqueue(0);
    enqueue(1);
}
```



## SuperCollider


### Recursive

nth fibonacci term for positive n

```SuperCollider

f = { |n| if(n < 2) { n } { f.(n-1) + f.(n-2) } };
(0..20).collect(f)

```


nth fibonacci term for positive and negative n.

```SuperCollider

f = { |n| var u = neg(sign(n)); if(abs(n) < 2) { n } { f.(2 * u + n) + f.(u + n) } };
(-20..20).collect(f)

```



### Analytic


```SuperCollider
(
	f = { |n|
		var sqrt5 = sqrt(5);
		var p = (1 + sqrt5) / 2;
		var q = reciprocal(p);
		((p ** n) + (q ** n) / sqrt5 + 0.5).trunc
	};
	(0..20).collect(f)
)
```



### Iterative


```SuperCollider

f = { |n| var a = [1, 1]; n.do { a = a.addFirst(a[0] + a[1]) }; a.reverse };
f.(18)

```



## Swift


### Analytic


```Swift
import Cocoa

func fibonacci(n: Int) -> Int {
    let square_root_of_5 = sqrt(5.0)
    let p = (1 + square_root_of_5) / 2
    let q = 1 / p
    return Int((pow(p,CDouble(n)) + pow(q,CDouble(n))) / square_root_of_5 + 0.5)
}

for i in 1...30 {
    println(fibonacci(i))
}
```



### Iterative


```Swift
func fibonacci(n: Int) -> Int {
    if n < 2 {
        return n
    }
    var fibPrev = 1
    var fib = 1
    for num in 2...n {
        (fibPrev, fib) = (fib, fib + fibPrev)
    }
    return fib
}
```

Sequence:

```swift
func fibonacci() -> SequenceOf<UInt> {
  return SequenceOf {() -> GeneratorOf<UInt> in
    var window: (UInt, UInt, UInt) = (0, 0, 1)
    return GeneratorOf {
      window = (window.1, window.2, window.1 + window.2)
      return window.0
    }
  }
}
```



### Recursive


```Swift
func fibonacci(n: Int) -> Int {
    if n < 2 {
        return n
    } else {
        return fibonacci(n-1) + fibonacci(n-2)
    }
}

println(fibonacci(30))
```



## Tailspin


### Recursive simple

The simplest exponential-time recursive algorithm only handling positive N. Note that the "#" is the tailspin internal recursion which sends the value to the matchers. In this case where there is no initial block and no templates state, we could equivalently write the templates name "nthFibonacci" in place of the "#" to do a normal recursion.

```tailspin

templates nthFibonacci
  <0|1> $ !
  <> ($ - 1 -> #) + ($ - 2 -> #) !
end nthFibonacci

```

===Iterative, mutable state===
We could use the templates internal mutable state, still only positive N.

```tailspin

templates nthFibonacci
  @: {n0: 0, n1: 1};
  1..$ -> @: {n0: $@.n1, n1: $@.n0 + $@.n1};
  $@.n0!
end nthFibonacci

```

To handle negatives, we can keep track of the sign and send it to the matchers.

```tailspin

templates nthFibonacci
  @: {n0: 0, n1: 1};
  def sign: $ -> (<0..> 1! <> -1!);
  1..$*$sign -> $sign -> #
  $@.n0!
  <1>
    @: {n0: $@.n1, n1: $@.n0 + $@.n1};
  <-1>
    @: {n0: $@.n1 - $@.n0, n1: $@.n0};
end nthFibonacci

```


### State machine

Instead of mutating state, we could just recurse internally on a state structure.

```tailspin

templates nthFibonacci
  { N: $, n0: 0, n1: 1 } -> #
  <{ N: <0> }>
    $.n0 !
  <{ N: <1..>}>
    { N: $.N - 1, n0: $.n1, n1: $.n0 + $.n1} -> #
  <>
    { N: $.N + 1, n1: $.n0, n0: $.n1 - $.n0} -> #
end nthFibonacci

8 -> nthFibonacci -> '$;
' -> !OUT::write
-5 -> nthFibonacci -> '$;
' -> !OUT::write
-6 -> nthFibonacci -> '$;
' -> !OUT::write

```

```txt

21
5
-8

```



## Tcl


### Simple Version

These simple versions do not handle negative numbers -- they will return N for N < 2

### =Iterative=

```tcl
proc fibiter n {
    if {$n < 2} {return $n}
    set prev 1
    set fib 1
    for {set i 2} {$i < $n} {incr i} {
        lassign [list $fib [incr fib $prev]] prev fib
    }
    return $fib
}
```


### =Recursive=


```tcl
proc fib {n} {
    if {$n < 2} then {expr {$n}} else {expr {[fib [expr {$n-1}]]+[fib [expr {$n-2}]]} }
}
```


The following {{works with|Tcl|8.5}}: defining a procedure in the <code>::tcl::mathfunc</code> namespace allows that proc to be used as a function in <code>[http://www.tcl.tk/man/tcl8.5/TclCmd/expr.htm expr]</code> expressions.

```tcl
proc tcl::mathfunc::fib {n} {
    if { $n < 2 } {
        return $n
    } else {
        return [expr {fib($n-1) + fib($n-2)}]
    }
}

# or, more tersely

proc tcl::mathfunc::fib {n} {expr {$n<2 ? $n : fib($n-1) + fib($n-2)}}
```


E.g.:


```tcl
expr {fib(7)} ;# ==> 13

namespace path tcl::mathfunc #; or, interp alias {} fib {} tcl::mathfunc::fib
fib 7 ;# ==> 13
```


====Tail-Recursive====
In Tcl 8.6 a ''tailcall'' function is available to permit writing tail-recursive functions in Tcl. This makes deeply recursive functions practical. The availability of large integers also means no truncation of larger numbers.

```tcl
proc fib-tailrec {n} {
    proc fib:inner {a b n} {
        if {$n < 1} {
            return $a
        } elseif {$n == 1} {
            return $b
        } else {
            tailcall fib:inner $b [expr {$a + $b}] [expr {$n - 1}]
        }
    }
    return [fib:inner 0 1 $n]
}
```

 % fib-tailrec 100
 354224848179261915075


### Handling Negative Numbers


### =Iterative=


```tcl
proc fibiter n {
    if {$n < 0} {
        set n [expr {abs($n)}]
        set sign [expr {-1**($n+1)}]
    } else {
        set sign 1
    }
    if {$n < 2} {return $n}
    set prev 1
    set fib 1
    for {set i 2} {$i < $n} {incr i} {
        lassign [list $fib [incr fib $prev]] prev fib
    }
    return [expr {$sign * $fib}]
}
fibiter -5 ;# ==> 5
fibiter -6 ;# ==> -8
```


### =Recursive=


```tcl
proc tcl::mathfunc::fib {n} {expr {$n<-1 ? -1**($n+1) * fib(abs($n)) : $n<2 ? $n : fib($n-1) + fib($n-2)}}
expr {fib(-5)} ;# ==> 5
expr {fib(-6)} ;# ==> -8
```


### For the Mathematically Inclined

This works up to <math>fib(70)</math>, after which the limited precision of IEEE double precision floating point arithmetic starts to show.

```tcl
proc fib n {expr {round((.5 + .5*sqrt(5)) ** $n / sqrt(5))}}
```



## Tern



### Recursive


```tern
func fib(n) {
   if (n < 2) {
      return 1;
   }
   return fib(n - 1) + fib(n - 2);
}
```



### Coroutine


```tern
func fib(n) {
   let a = 1;
   let b = 2;

   until(n-- <= 0) {
      yield a;
      (a, b) = (b, a + b);
   }
}
```


=={{header|TI-83 BASIC}}==
Iterative:

```ti83b
{0,1
While 1
Disp Ans(1
{Ans(2),sum(Ans
End
```


Binet's formula:

```ti83b
Prompt N
.5(1+√(5    //golden ratio
(Ans^N–(-Ans)^-N)/√(5
```


=={{header|TI-89 BASIC}}==

### Recursive

Optimized implementation (too slow to be usable for ''n'' higher than about 12).


```ti89b
fib(n)
when(n<2, n, fib(n-1) + fib(n-2))
```



### Iterative

Unoptimized implementation (I think the for loop can be eliminated, but I'm not sure).


```ti89b
fib(n)
Func
Local a,b,c,i
0→a
1→b
For i,1,n
  a→c
  b→a
  c+b→b
EndFor
a
EndFunc
```



## TSE SAL


```TSE SAL


// library: math: get: series: fibonacci <description></description> <version control></version control> <version>1.0.0.0.3</version> <version control></version control> (filenamemacro=getmasfi.s) [<Program>] [<Research>] [kn, ri, su, 20-01-2013 22:04:02]
INTEGER PROC FNMathGetSeriesFibonacciI( INTEGER  nI )
 //
 // Method:
 //
 // 1. Take the sum of the last 2 terms
 //
 // 2. Let the sum be the last term
 //    and goto step 1
 //
 INTEGER I = 0
 INTEGER minI = 1
 INTEGER maxI = nI
 INTEGER term1I = 0
 INTEGER term2I = 1
 INTEGER term3I = 0
 //
 FOR I = minI TO maxI
  //
  // make value 3 equal to sum of two previous values 1 and 2
  //
  term3I = term1I + term2I
  //
  // make value 1 equal to next value 2
  //
  term1I = term2I
  //
  // make value 2 equal to next value 3
  //
  term2I = term3I
  //
  ENDFOR
  //
 RETURN( term3I )
 //
END

PROC Main()
 STRING s1[255] = "3"
 REPEAT
  IF ( NOT ( Ask( " = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
  Warn( FNMathGetSeriesFibonacciI( Val( s1 ) ) ) // gives e.g. 3
 UNTIL FALSE
END


```




## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ASK "What fibionacci number do you want?": searchfib=""
IF (searchfib!='digits') STOP
Loop n=0,{searchfib}
 IF (n==0) THEN
   fib=fiba=n
 ELSEIF (n==1) THEN
   fib=fibb=n
 ELSE
   fib=fiba+fibb, fiba=fibb, fibb=fib
 ENDIF
 IF (n!=searchfib) CYCLE
 PRINT "fibionacci number ",n,"=",fib
ENDLOOP

```

Output:

```txt

What fibionacci number do you want? >12
fibionacci number 12=144

```

Output:

```txt

What fibionacci number do you want? >31
fibionacci number 31=1346269

```

Output:

```txt

What fibionacci number do you want? >46
fibionacci 46=1836311903

```



## UnixPipes

```bash
echo 1 |tee last fib ; tail -f fib | while read x
do
   cat last | tee -a fib | xargs -n 1 expr $x + |tee last
done
```



## UNIX Shell

```bash
#!/bin/bash

a=0
b=1
max=$1

for (( n=1; "$n" <= "$max"; $((n++)) ))
do
  a=$(($a + $b))
  echo "F($n): $a"
  b=$(($a - $b))
done
```


Recursive:
```bash
fib() {
  local n=$1
  [ $n -lt 2 ] && echo -n $n || echo -n $(( $( fib $(( n - 1 )) ) + $( fib $(( n - 2 )) ) ))
}
```



## Ursa

### Iterative


```ursa
def fibIter (int n)
	if (< n 2)
		return n
	end if
	decl int fib fibPrev num
	set fib (set fibPrev 1)
	for (set num 2) (< num n) (inc num)
		set fib (+ fib fibPrev)
		set fibPrev (- fib fibPrev)
	end for
	return fib
end
```




## Ursala

All three methods are shown here, and all have unlimited precision.

```Ursala
#import std
#import nat

iterative_fib = ~&/(0,1); ~&r->ll ^|\predecessor ^/~&r sum

recursive_fib = {0,1}^?<a/~&a sum^|W/~& predecessor^~/~& predecessor

analytical_fib =

%np+ -+
   mp..round; ..mp2str; sep`+; ^CNC/~&hh take^\~&htt %np@t,
   (mp..div^|\~& mp..sub+ ~~ @rlX mp..pow_ui)^lrlPGrrPX/~& -+
      ^\~& ^(~&,mp..sub/1.E0)+ mp..div\2.E0+ mp..add/1.E0,
      mp..sqrt+ ..grow/5.E0+-+-
```

The analytical method uses arbitrary precision floating point
arithmetic from the mpfr library and then converts the result to
a natural number. Sufficient precision for an exact result is
always chosen based on the argument. This test program computes the first
twenty Fibonacci numbers by all three methods.

```Ursala
#cast %nLL

examples = <.iterative_fib,recursive_fib,analytical_fib>* iota20
```

output:

```txt

<
   <0,0,0>,
   <1,1,1>,
   <1,1,1>,
   <2,2,2>,
   <3,3,3>,
   <5,5,5>,
   <8,8,8>,
   <13,13,13>,
   <21,21,21>,
   <34,34,34>,
   <55,55,55>,
   <89,89,89>,
   <144,144,144>,
   <233,233,233>,
   <377,377,377>,
   <610,610,610>,
   <987,987,987>,
   <1597,1597,1597>,
   <2584,2584,2584>,
   <4181,4181,4181>>
```



## V

Generate n'th fib by using binary recursion


```v
[fib
   [small?] []
     [pred dup pred]
     [+]
   binrec].
```



## Vala



### Recursive

Using int, but could easily replace with double, long, ulong, etc.

```vala

int fibRec(int n){
	if (n < 2)
		return n;
	else
		return fibRec(n - 1) + fibRec(n - 2);
}

```



### Iterative

Using int, but could easily replace with double, long, ulong, etc.

```vala

int fibIter(int n){
	if (n < 2)
		return n;

	int last = 0;
	int cur = 1;
	int next;

	for (int i = 1; i < n; ++i){
		next = last + cur;
		last = cur;
		cur = next;
	}

	return cur;
}

```



## VAX Assembly


```VAX Assembly
                               0000  0000     1 .entry	main,0
                            7E 7CFD  0002     2 	clro	-(sp)			;result buffer
                            5E   DD  0005     3 	pushl	sp			;pointer to buffer
                            10   DD  0007     4 	pushl	#16			;descriptor: len of buffer
                       5B   5E   D0  0009     5 	movl	sp, r11			;-> descriptor
                                     000C     6
                       7E   01   7D  000C     7 	movq	#1, -(sp)		;init 0,1
                                     000F     8 loop:
               7E   6E   04 AE   C1  000F     9 	addl3	4(sp), (sp), -(sp)	;next element on stack
                            17   1D  0014    10 	bvs	ret			;vs - overflow set, exit
                                     0016    11
                            5B   DD  0016    12 	pushl	r11			;-> descriptor by ref
                         04 AE   DF  0018    13 	pushal	4(sp)			;-> fib on stack by ref
              00000000'GF   02   FB  001B    14 	calls	#2, g^ots$cvt_l_ti	;convert integer to string
                            5B   DD  0022    15 	pushl	r11			;
              00000000'GF   01   FB  0024    16 	calls	#1, g^lib$put_output	;show result
                            E2   11  002B    17 	brb	loop
                                     002D    18 ret:
                                 04  002D    19 	ret
                                     002E    20 .end	main
$ run fib
...
        14930352
        24157817
        39088169
        63245986
       102334155
       165580141
       267914296
       433494437
       701408733
      1134903170
      1836311903
$

```



## VBA

Like Visual Basic .NET, but with keyword "Public" and type Variant (subtype Currency) instead of Decimal:

```vb
Public Function Fib(ByVal n As Integer) As Variant
    Dim fib0 As Variant, fib1 As Variant, sum As Variant
    Dim i As Integer
    fib0 = 0
    fib1 = 1
    For i = 1 To n
        sum = fib0 + fib1
        fib0 = fib1
        fib1 = sum
    Next i
    Fib = fib0
End Function
```

With Currency type, maximum value is fibo(73).

The (slow) recursive version:


```VBA

Public Function RFib(Term As Integer) As Long
  If Term < 2 Then RFib = Term Else RFib = RFib(Term - 1) + RFib(Term - 2)
End Function

```


With Long type, maximum value is fibo(46).


## VBScript

===Non-recursive, object oriented, generator===
Defines a generator class, with a default Get property. Uses Currency for larger-than-Long values. Tests for overflow and switches to Double. Overflow information also available from class.


### =Class Definition:=


```vb
class generator
	dim t1
	dim t2
	dim tn
	dim cur_overflow

	Private Sub Class_Initialize
		cur_overflow = false
		t1 = ccur(0)
		t2 = ccur(1)
		tn = ccur(t1 + t2)
	end sub

	public default property get generated
		on error resume next

		generated = ccur(tn)
		if err.number <> 0 then
			generated = cdbl(tn)
			cur_overflow = true
		end if
		t1 = ccur(t2)
		if err.number <> 0 then
			t1 = cdbl(t2)
			cur_overflow = true
		end if
		t2 = ccur(tn)
		if err.number <> 0 then
			t2 = cdbl(tn)
			cur_overflow = true
		end if
		tn = ccur(t1+ t2)
		if err.number <> 0 then
			tn = cdbl(t1) + cdbl(t2)
			cur_overflow = true
		end if
		on error goto 0
	end property

	public property get overflow
		overflow = cur_overflow
	end property

end class
```



### =Invocation:=


```vb
dim fib
set fib = new generator
dim i
for i = 1 to 100
	wscript.stdout.write " " & fib
	if fib.overflow then
		wscript.echo
		exit for
	end if
next
```



### =Output:=


```vbscript> 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155 165580141 267914296 433494437 701408733 1134903170 1836311903 2971215073 4807526976 7778742049 12586269025 20365011074 32951280099 53316291173 86267571272 139583862445 225851433717 365435296162 591286729879 956722026041 1548008755920 2504730781961 4052739537881 6557470319842 10610209857723 17167680177565 27777890035288 44945570212853 72723460248141 117669030460994 190392490709135 308061521170129 498454011879264 806515533049393</lang



## Vedit macro language

'''Iterative'''

Calculate fibonacci(#1). Negative values return 0.

```vedit
:FIBONACCI:
#11 = 0
#12 = 1
Repeat(#1) {
    #10 = #11 + #12
    #11 = #12
    #12 = #10
}
Return(#11)
```



## Visual Basic

Maximum integer value (7*10^28) can be obtained by using decimal type, but decimal type is only a sub type of the variant type.

```vb
Sub fibonacci()
    Const n = 139
    Dim i As Integer
    Dim f1 As Variant, f2 As Variant, f3 As Variant 'for Decimal
    f1 = CDec(0): f2 = CDec(1) 'for Decimal setting
    Debug.Print "fibo("; 0; ")="; f1
    Debug.Print "fibo("; 1; ")="; f2
    For i = 2 To n
        f3 = f1 + f2
        Debug.Print "fibo("; i; ")="; f3
        f1 = f2
        f2 = f3
    Next i
End Sub 'fibonacci
```

```txt
fibo( 0 )= 0
fibo( 1 )= 1
fibo( 2 )= 1
...
fibo( 137 )= 19134702400093278081449423917
fibo( 138 )= 30960598847965113057878492344
fibo( 139 )= 50095301248058391139327916261
```



## Visual Basic .NET

'''Platform:''' [[.NET]]

### Iterative

With Decimal type, maximum value is fibo(139).

```vbnet
Function Fib(ByVal n As Integer) As Decimal
    Dim fib0, fib1, sum As Decimal
    Dim i As Integer
    fib0 = 0
    fib1 = 1
    For i = 1 To n
        sum = fib0 + fib1
        fib0 = fib1
        fib1 = sum
    Next
    Fib = fib0
End Function
```


### Recursive

```vbnet
Function Seq(ByVal Term As Integer)
        If Term < 2 Then Return Term
        Return Seq(Term - 1) + Seq(Term - 2)
End Function
```


### BigInteger

There is no real maximum value of BigInterger class, except the memory to store the number.
Within a minute, fibo(2000000) is a number with 417975 digits.

```vbnet
    Function FiboBig(ByVal n As Integer) As BigInteger
        ' Fibonacci sequence with BigInteger
        Dim fibn2, fibn1, fibn As BigInteger
        Dim i As Integer
        fibn = 0
        fibn2 = 0
        fibn1 = 1
        If n = 0 Then
            Return fibn2
        ElseIf n = 1 Then
            Return fibn1
        ElseIf n >= 2 Then
            For i = 2 To n
                fibn = fibn2 + fibn1
                fibn2 = fibn1
                fibn1 = fibn
            Next i
            Return fibn
        End If
        Return 0
    End Function 'FiboBig

    Sub fibotest()
        Dim i As Integer, s As String
        i = 2000000  ' 2 millions
        s = FiboBig(i).ToString
        Console.WriteLine("fibo(" & i & ")=" & s & " - length=" & Len(s))
    End Sub 'fibotest
```


===BigInteger, speedier method===

This method doesn't need to iterate the entire list, and is much faster.  The 2000000 (two millionth) Fibonacci number can be found in a fraction of a second.<br/>Algorithm from [http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html here, see section 3, ''Finding Fibonacci Numbers Fully''.]

```vbnet
Imports System
Imports System.Collections.Generic
Imports System.Numerics

Module Module1

    ' A sparse array of values calculated along the way
    Dim sl As SortedList(Of Integer, BigInteger) = New SortedList(Of Integer, BigInteger)()

    ' Square a BigInteger
    Function sqr(ByVal n As BigInteger) As BigInteger
        Return n * n
    End Function

    ' Helper routine for Fsl(). It adds an entry to the sorted list when necessary
    Sub IfNec(n as integer)
        If Not sl.ContainsKey(n) Then sl.Add(n, Fsl(n))
    End Sub

    ' This routine is semi-recursive, but doesn't need to evaluate every number up to n.
    ' Algorithm from here: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html#section3
    Function Fsl(ByVal n As Integer) As BigInteger
        If n < 2 Then Return n
        Dim n2 As Integer = n >> 1, pm As Integer = n2 + ((n And 1) << 1) - 1 : IfNec(n2) : IfNec(pm)
        Return If(n2 > pm, (2 * sl(pm) + sl(n2)) * sl(n2), sqr(sl(n2)) + sqr(sl(pm)))
    End Function

    ' Conventional iteration method (not used here)
    Function Fm(ByVal n As BigInteger) As BigInteger
        If n < 2 Then Return n
        Dim cur As BigInteger = 0, pre As BigInteger = 1
        For i As Integer = 0 To n - 1
            Dim sum As BigInteger = cur + pre
            pre = cur : cur = sum
        Next : Return cur
    End Function

    Sub Main()
        Dim num As Integer = 2_000_000
        Dim st As DateTime = DateTime.Now
        Dim v As BigInteger = Fsl(num)
        Console.WriteLine("{0:n3} ms to calculate the {1:n0}th Fibonacci number,",
                          (DateTime.Now - st).TotalMilliseconds, num)
        st = DateTime.Now
        Dim vs As String = v.ToString()
        Console.WriteLine("{0:n3} seconds to convert to a string.", (DateTime.Now - st).TotalSeconds)
        Console.WriteLine("number of digits is {0}", vs.Length)
        If vs.Length < 10000 Then
            st = DateTime.Now
            Console.WriteLine(vs)
            Console.WriteLine("{0:n3} ms to write it to the console.", (DateTime.Now - st).TotalMilliseconds)
        Else
            Console.WriteLine("partial: {0}...{1}", vs.Substring(1, 35), vs.Substring(vs.Length - 35))
        End If
    End Sub
End Module
```

```txt
177.831 ms to calculate the 2,000,000th Fibonacci number,
4.649 seconds to convert to a string.
number of digits is 417975
partial: 53129491750764154305166065450382516...91799493108960825129188777803453125

```



## Wart


===Recursive, all at once===

```python
def (fib n)
  if (n < 2)
    n
    (+ (fib n-1) (fib n-2))
```


===Recursive, using cases===

```python
def (fib n)
  (+ (fib n-1) (fib n-2))

def (fib n) :case (n < 2)
  n
```


===Recursive, using memoization===

```python
def (fib n saved)
  # all args in Wart are optional, and we expect callers to not provide `saved`
  default saved :to (table 0 0 1 1)  # pre-populate base cases
  default saved.n :to
    (+ (fib n-1 saved) (fib n-2 saved))
  saved.n
```



## WDTE



### Memoized Recursive


```WDTE>let memo fib n =
 n { > 1 => + (fib (- n 1)) (fib (- n 2)) };
```



### Iterative


```WDTE>let s =
 import 'stream';
let a => import 'arrays';

let fib n => (
	let reducer p n => [a.at p 1; + (a.at p 0) (a.at p 1)];
	s.range 1 n
	-> s.reduce [0; 1] reducer
	-> a.at 1
	;
);
```



## Whitespace



### Iterative

This program generates Fibonacci numbers until it is [http://ideone.com/VBDLzk forced to terminate].

```Whitespace













```


It was generated from the following pseudo-Assembly.

```asm
push 0
push 1

0:
    swap
    dup
    onum
    push 10
    ochr
    copy 1
    add
    jump 0
```

```txt
$ wspace fib.ws | head -n 6
0
1
1
2
3
5
```



### Recursive


This program takes a number ''n'' on standard input and outputs the ''n''th member of the Fibonacci sequence.

```Whitespace





























```


```asm
; Read n.
push 0
dup
inum
load

; Call fib(n), ouput the result and a newline, then exit.
call 0
onum
push 10
ochr
exit

0:
    dup
    push 2
    sub
    jn 1   ; Return if n < 2.
    dup
    push 1
    sub
    call 0 ; Call fib(n - 1).
    swap   ; Get n back into place.
    push 2
    sub
    call 0 ; Call fib(n - 2).
    add    ; Leave the sum on the stack.
1:
    ret
```


```txt
$ echo 10 | wspace fibrec.ws
55
```



## Wrapl


### Generator


```wrapl
DEF fib() (
    VAR seq <- [0, 1]; EVERY SUSP seq:values;
    REP SUSP seq:put(seq:pop + seq[1])[-1];
);
```

To get the 17th number:

```wrapl
16 SKIP fib();
```

To get the list of all 17 numbers:

```wrapl
ALL 17 OF fib();
```


### Iterator

Using type match signature to ensure integer argument:

```wrapl
TO fib(n @ Integer.T) (
    VAR seq <- [0, 1];
    EVERY 3:to(n) DO seq:put(seq:pop + seq[1]);
    RET seq[-1];
);
```



## x86 Assembly

```asm
TITLE i hate visual studio 4			(Fibs.asm)
;       __         __/--------\
;      >__ \      /  |        |\
;         \  \___/ @  \      /   \__________________
;           \____       \  /                         \\\
;                \____         Coded with love by:    |||
;                      \      Alexander Alvonellos    |||
;                       |          9/29/2011         / ||
;                       |                           |  MM
;                       |      |--------------|     |
;                       |<     |              |<    |
;                       |      |              |     |
;                       |mmmmmm|              |mmmmm|
;; Epic Win.

INCLUDE Irvine32.inc

.data
	BEERCOUNT = 48;
	Fibs dd 0, 1, BEERCOUNT DUP(0);

.code
main PROC
; I am not responsible for this code.
; They made me write it, against my will.
	;Here be dragons
	mov esi, offset Fibs; offset array;  ;;were to start (start)
	mov ecx, BEERCOUNT; 		;;count of items (how many)
	mov ebx, 4; 		;;size (in number of bytes)
	call DumpMem;

	mov ecx, BEERCOUNT; 	;//http://www.wolframalpha.com/input/?i=F ib%5B47%5D+%3E+4294967295
	mov esi, offset Fibs
	NextPlease:;
		mov eax, [esi]; 	;//Get me the data from location at ESI
		add eax, [esi+4];	;//add into the eax the data at esi + another double (next mem loc)
		mov [esi+8], eax;	;//Move that data into the memory location after the second number
		add esi, 4;			;//Update the pointer
	loop NextPlease;	;//Thank you sir, may I have another?


	;Here be dragons
	mov esi, offset Fibs; offset array;  ;;were to start (start)
	mov ecx, BEERCOUNT; 		;;count of items (how many)
	mov ebx, 4; 		;;size (in number of bytes)
	call DumpMem;

	exit		; exit to operating system
main ENDP

END main
```



## xEec

This will display the first 93 numbers of the sequence.

```xEec

h#1 h#1 h#1 o#
h#10 o$ p
>f
  o# h#10 o$ p
  ma h? jnext p
  t
jnf

```



## XLISP


### Analytic

Uses Binet's method, based on the golden ratio, which almost feels like cheating—but the task specification doesn't require any particular algorithm, and this one is straightforward and fast.

```lisp
(DEFUN FIBONACCI (N)
    (FLOOR (+ (/ (EXPT (/ (+ (SQRT 5) 1) 2) N) (SQRT 5)) 0.5)))
```

To test it, we'll define a <tt>RANGE</tt> function and ask for the first 50 numbers in the sequence:

```lisp
(DEFUN RANGE (X Y)
    (IF (<= X Y)
        (CONS X (RANGE (+ X 1) Y))))

(PRINT (MAPCAR FIBONACCI (RANGE 1 50)))
```

```txt
(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155 165580141 267914296 433494437 701408733 1134903170 1836311903 2971215073 4807526976 7778742049 12586269025)
```



### Tail recursive

Alternatively, this approach is reasonably efficient:

```lisp
(defun fibonacci (x)
    (defun fib (a b n)
        (if (= n 2)
            b
            (fib b (+ a b) (- n 1)) ) )
    (if (< x 2)
        x
        (fib 1 1 x) ) )
```



## Xojo

Pass n to this function where n is the desired number of iterations. This example uses the UInt64 datatype which is as unsigned 64 bit integer. As such, it overflows after the 93rd iteration.

```vb
Function fibo(n As Integer) As UInt64

  Dim noOne As UInt64 = 1
  Dim noTwo As UInt64 = 1
  Dim sum As UInt64

  For i As Integer = 3 To n
      sum = noOne + noTwo
      noTwo = noOne
      noOne = sum
  Next

  Return noOne
End Function
```



## XQuery


```xquery
declare function local:fib($n as xs:integer) as xs:integer {
  if($n < 2)
  then $n
  else local:fib($n - 1) + local:fib($n - 2)
};
```



## zkl

A slight tweak to the task; creates a function that continuously generates fib numbers

```zkl
var fibShift=fcn(ab){ab.append(ab.sum()).pop(0)}.fp(L(0,1));
```


```txt

zkl: do(15){ fibShift().print(",") }
0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,

zkl: do(5){ fibShift().print(",") }
610,987,1597,2584,4181,

```



## ZX Spectrum Basic


### =Iterative=


```zxbasic
10 REM Only positive numbers
20 LET n=10
30 LET n1=0: LET n2=1
40 FOR k=1 TO n
50 LET sum=n1+n2
60 LET n1=n2
70 LET n2=sum
80 NEXT k
90 PRINT n1
```



### =Analytic=


```zxbasic
10 DEF FN f(x)=INT (0.5+(((SQR 5+1)/2)^x)/SQR 5)
```


