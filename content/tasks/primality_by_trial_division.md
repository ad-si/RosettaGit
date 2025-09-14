+++
title = "Primality by trial division"
description = ""
date = 2019-09-12T14:09:40Z
aliases = []
[extra]
id = 2254
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "360_assembly",
  "abap",
  "acl2",
  "actionscript",
  "ada",
  "algol_68",
  "algol_w",
  "awk",
  "b",
  "b_as_on_pdp7_unix0",
  "basic",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "burlesque",
  "c",
  "chapel",
  "clojure",
  "cmake",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "eiffel",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "false",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "funl",
  "futurebasic",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "k",
  "kotlin",
  "langur",
  "liberty_basic",
  "lingo",
  "logo",
  "lse64",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
  "min",
  "mumps",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oz",
  "panda",
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
  "rebol",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "snobol4",
  "sql",
  "standard_ml",
  "swift",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "v",
  "vba",
  "vbscript",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Write a boolean function that tells whether a given integer is prime.


Remember that   '''1'''   and all non-positive numbers are not prime.

Use trial division.

Even numbers over two may be eliminated right away.

A loop from   '''3'''   to   '''&radic;{{overline| n }}  ''' will suffice,   but other loops are allowed.


## Related tasks

*   [[count in factors]]
*   [[prime decomposition]]
*   [[AKS test for primes]]
*   [[factors of an integer]]
*   [[Sieve of Eratosthenes]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]
*   [[sequence of primes by Trial Division]]





## 360 Assembly


```360asm
*        Primality by trial division  26/03/2017
PRIMEDIV CSECT
         USING  PRIMEDIV,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,PG             pgi=0
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=F'50')   do i=1 to 50
         LR     R1,R6                i
         BAL    R14,ISPRIME          call isprime(i)
       IF C,R0,EQ,=F'1' THEN         if isprime(i) then
         XDECO  R6,XDEC                edit i
         MVC    0(4,R10),XDEC+8        output i
         LA     R10,4(R10)             pgi+=4
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
*------- ----   ----------------------------------------
ISPRIME  EQU    *                  function isprime(n)
       IF C,R1,LE,=F'1' THEN       if n<=1 then
         LA     R0,0                 return(0)
         BR     R14                  return
       ENDIF    ,                  endif
       IF C,R1,EQ,=F'2' THEN       if n=2 then
         LA     R0,1                 return(1)
         BR     R14                  return
       ENDIF    ,                  endif
         LR     R4,R1              n
         N      R4,=X'00000001'    n and 1
       IF LTR,R4,Z,R4 THEN         if mod(n,2)=0 then
         LA     R0,0                 return(0)
         BR     R14                  return
       ENDIF    ,                  endif
         LA     R7,3               j=3
         LA     R5,9               r5=j*j
       DO WHILE=(CR,R5,LE,R1)      do j=3 by 2 while j*j<=n
         LR     R4,R1                n
         SRDA   R4,32                ~
         DR     R4,R7                /j
       IF LTR,R4,Z,R4 THEN           if mod(n,j)=0 then
         LA     R0,0                   return(0)
         BR     R14                    return
       ENDIF    ,                    endif
         LA     R7,2(R7)             j+=2
         LR     R5,R7                j
         MR     R4,R7                r5=j*j
       ENDDO    ,                  enddo j
         LA     R0,1               return(1)
         BR     R14                return
*------- ----   ----------------------------------------
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp for xdeco
         YREGS
         END    PRIMEDIV
```

   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47


## ABAP


```ABAP
class ZMLA_ROSETTA definition
  public
  create public .

public section.

  types:
    enumber         TYPE          N  LENGTH 60 .
  types:
    listof_enumber  TYPE TABLE OF enumber .

  class-methods IS_PRIME
    importing
      value(N) type ENUMBER
    returning
      value(OFLAG) type ABAP_BOOL .
  class-methods IS_PRIME_I
    importing
      value(N) type I
    returning
      value(OFLAG) type ABAP_BOOL .
  protected section.
  private section.
ENDCLASS.



CLASS ZMLA_ROSETTA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>IS_PRIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        ENUMBER
* | [<-()] OFLAG                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IS_PRIME.
    IF n < 2.
      oflag = abap_false.
      RETURN.
    ENDIF.
    IF n = 2 or n = 3.
      oflag = abap_true.
      RETURN.
    ENDIF.
    IF n mod 2 = 0 or n mod 3 = 0.
      oflag = abap_false.
      RETURN.
    ENDIF.
    DATA: lim type enumber,
          d   type enumber,
          i   TYPE i        VALUE 2.
    lim = sqrt( n ).
    d   = 5.
    WHILE d <= lim.
      IF n mod d = 0.
        oflag = abap_false.
        RETURN.
      ENDIF.
      d = d + i.
      i = 6 - i.  " this modifies 2 into 4 and viceversa
    ENDWHILE.
    oflag = abap_true.
    RETURN.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>IS_PRIME_I
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        I
* | [<-()] OFLAG                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IS_PRIME_I.
    IF n < 2.
      oflag = abap_false.
      RETURN.
    ENDIF.
    IF n = 2 or n = 3.
      oflag = abap_true.
      RETURN.
    ENDIF.
    IF n mod 2 = 0 or n mod 3 = 0.
      oflag = abap_false.
      RETURN.
    ENDIF.
    DATA: lim type i,
          d   type i,
          i   TYPE i        VALUE 2.
    lim = sqrt( n ).
    d   = 5.
    WHILE d <= lim.
      IF n mod d = 0.
        oflag = abap_false.
        RETURN.
      ENDIF.
      d = d + i.
      i = 6 - i.  " this modifies 2 into 4 and viceversa
    ENDWHILE.
    oflag = abap_true.
    RETURN.
  endmethod.
ENDCLASS.
```



## ACL2


```Lisp
(defun is-prime-r (x i)
   (declare (xargs :measure (nfix (- x i))))
   (if (zp (- (- x i) 1))
       t
       (and (/= (mod x i) 0)
            (is-prime-r x (1+ i)))))

(defun is-prime (x)
   (or (= x 2)
       (is-prime-r x 2)))
```



## ActionScript


```ActionScript
function isPrime(n:int):Boolean
{
	if(n < 2) return false;
	if(n == 2) return true;
	if((n & 1) == 0) return false;
	for(var i:int = 3; i <= Math.sqrt(n); i+= 2)
		if(n % i == 0) return false;
	return true;
}
```



## Ada


```ada
function Is_Prime(Item : Positive) return Boolean is
   Result : Boolean := True;
   Test : Natural;
begin
   if Item /= 2 and Item mod 2 = 0 then
      Result := False;
   else
      Test := 3;
      while Test < Integer(Sqrt(Float(Item))) loop
         if Item mod Test = 0 then
            Result := False;
            exit;
         end if;
         Test := Test + 2;
      end loop;
  end if;
  return Result;
end Is_Prime;
```


As an alternative, one can use the generic function Prime_Numbers.Is_Prime, as specified in [[Prime decomposition#Ada]], which also implements trial division.

```Ada
with Prime_Numbers;

procedure Test_Prime is

   package Integer_Numbers is new
     Prime_Numbers (Natural, 0, 1, 2);
   use Integer_Numbers;

begin
   if Is_Prime(12) or (not Is_Prime(13)) then
      raise Program_Error with "Test_Prime failed!";
   end if;
end Test_Prime;
```



## ALGOL 68

{{prelude/is_prime.a68}};

```algol68
main:(
  INT upb=100;
  printf(($" The primes up to "g(-3)" are:"l$,upb));
  FOR i TO upb DO
    IF is prime(i) THEN
      printf(($g(-4)$,i))
    FI
  OD;
  printf($l$)
)
```

 The primes up to 100 are:
   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97


## ALGOL W


```algolw
% returns true if n is prime, false otherwise %
% uses trial division                         %
logical procedure isPrime ( integer value n ) ;
    if n < 3 or not odd( n ) then n = 2
    else begin
        % odd number > 2 %
        integer f, rootN;
        logical havePrime;
        f         := 3;
        rootN     := entier( sqrt( n ) );
        havePrime := true;
        while f <= rootN and havePrime do begin
            havePrime := ( n rem f ) not = 0;
            f         := f + 2
        end;
        havePrime
    end isPrime ;
```

Test program:

```algolw
begin
    logical procedure isPrime ( integer value n ) ; algol "isPrime" ;
    for i := 0 until 32 do if isPrime( i ) then writeon( i_w := 1,s_w := 1, i )
end.
```

 2 3 5 7 11 13 17 19 23 29 31

=={{Header|ATS}}==

```ATS
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
#staload M = "libats/libc/SATS/math.sats"
//
(* ****** ****** *)
//
fun
isqrt(n: intGte(0)): intGte(0) =
  $UNSAFE.cast($M.sqrt_double(g0i2f(n)))
//
fun
is_prime
(
n : intGte(2)
) : bool =
(
if
(n = 2)
then true
else (
  if n % 2 = 0
    then false
    else (1, (isqrt(n)+1)/2).forall()(lam i => n % (2*i+1) != 0)
) (* else *)
) (* end of [is_prime] *)
//
(* ****** ****** *)
```


=={{Header|AutoHotkey}}==
[http://www.autohotkey.com/forum/topic44657.html Discussion]

```autohotkey
MsgBox % IsPrime(1995937)
Loop
   MsgBox % A_Index-3 . " is " . (IsPrime(A_Index-3) ? "" : "not ") . "prime."

IsPrime(n,k=2) { ; testing primality with trial divisors not multiple of 2,3,5, up to sqrt(n)
   d := k+(k<7 ? 1+(k>2) : SubStr("6-----4---2-4---2-4---6-----2",Mod(k,30),1))
   Return n < 3 ? n>1 : Mod(n,k) ? (d*d <= n ? IsPrime(n,d) : 1) : 0
}
```


=={{Header|AutoIt}}==

```autoit
#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.8.1
 Author:         Alexander Alvonellos


 Script Function:
	Perform primality test on a given integer $number.
	RETURNS: TRUE/FALSE

#ce ----------------------------------------------------------------------------
Func main()
	ConsoleWrite("The primes up to 100 are: " & @LF)
	For $i = 1 To 100 Step 1
		If(isPrime($i)) Then
			If($i <> 97) Then
				ConsoleWrite($i & ", ")
			Else
				ConsoleWrite($i)
			EndIf
		EndIf
	Next
EndFunc

Func isPrime($n)
	If($n < 2) Then Return False
	If($n = 2) Then Return True
	If(BitAnd($n, 1) = 0) Then Return False
	For $i = 3 To Sqrt($n) Step 2
		If(Mod($n, $i) = 0) Then Return False
	Next
	Return True
EndFunc
main()
```



## AWK

 $ awk 'func prime(n){for(d=2;d<=sqrt(n);d++)if(!(n%d)){return 0};return 1}{print prime($1)}'

Or more legibly, and with n <= 1 handling


```AWK
function prime(n) {
    if (n <= 1) return 0
    for (d = 2; d <= sqrt(n); d++)
        if (!(n % d)) return 0
    return 1
}

{print prime($1)}
```



## B

=
## B as on PDP7/UNIX0
=
```B
isprime(n) {
  auto p;
  if(n<2) return(0);
  if(!(n%2)) return(n==2);
  p=3;
  while(n/p>p) {
    if(!(n%p)) return(0);
    p=p+2;
  }
  return(1);
}
```



## BASIC

Returns 1 for prime, 0 for non-prime

```QBasic
FUNCTION prime% (n!)
  STATIC i AS INTEGER
  IF n = 2 THEN
    prime = 1
  ELSEIF n <= 1 OR n MOD 2 = 0 THEN
    prime = 0
  ELSE
    prime = 1
    FOR i = 3 TO INT(SQR(n)) STEP 2
      IF n MOD i = 0 THEN
        prime = 0
        EXIT FUNCTION
      END IF
    NEXT i
  END IF
END FUNCTION

' Test and display primes 1 .. 50
DECLARE FUNCTION prime% (n!)
FOR n = 1 TO 50
  IF prime(n) = 1 THEN PRINT n;
NEXT n
```

 2  3  5  7  11  13  17  19  23  29  31  37  41  43  47

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Prime.bas"
110 FOR X=0 TO 100
120   IF PRIME(X) THEN PRINT X;
130 NEXT
140 DEF PRIME(N)
150   IF N=2 THEN
160     LET PRIME=-1
170   ELSE IF N<=1 OR MOD(N,2)=0 THEN
180     LET PRIME=0
190   ELSE
200     LET PRIME=-1
210     FOR I=3 TO CEIL(SQR(N)) STEP 2
220       IF MOD(N,I)=0 THEN LET PRIME=0:EXIT FOR
230     NEXT
240   END IF
250 END DEF
```


=
## ZX Spectrum Basic
=

```ZXBasic
10 LET n=0: LET p=0
20 INPUT "Enter number: ";n
30 IF n>1 THEN GO SUB 1000
40 IF p=0 THEN PRINT n;" is not prime!"
50 IF p<>0 THEN PRINT n;" is prime!"
60 GO TO 10
1000 REM ***************
1001 REM * PRIME CHECK *
1002 REM ***************
1010 LET p=0
1020 IF n/2=INT (n/2) THEN RETURN
1030 LET p=1
1040 FOR i=3 TO SQR (n) STEP 2
1050 IF n/i=INT (n/i) THEN LET p=0: LET i= SQR (n)
1060 NEXT i
1070 RETURN

```

```txt

15 is not prime!
17 is prime!
119 is not prime!
137 is prime!
```



## BBC BASIC


```bbcbasic
      FOR i% = -1 TO 100
        IF FNisprime(i%) PRINT ; i% " is prime"
      NEXT
      END

      DEF FNisprime(n%)
      IF n% <= 1 THEN = FALSE
      IF n% <= 3 THEN = TRUE
      IF (n% AND 1) = 0 THEN = FALSE
      LOCAL t%
      FOR t% = 3 TO SQR(n%) STEP 2
        IF n% MOD t% = 0 THEN = FALSE
      NEXT
      = TRUE
```

```txt
2 is prime
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
```



## bc


```bc
/* Return 1 if n is prime, 0 otherwise */
define p(n) {
    auto i

    if (n < 2) return(0)
    if (n == 2) return(1)
    if (n % 2 == 0) return(0)
    for (i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return(0)
    }
    return(1)
}
```



## Befunge

Reads the value to test from stdin and outputs ''Yes'' if prime and ''No'' if not.

To avoid dealing with Befunge's limited data cells, the implementation is entirely stack-based. However, this requires compressing multiple values into a single stack cell, which imposes an upper limit of 1,046,529 (1023<sup>2</sup>), thus a maximum testable prime of 1,046,527.


```befunge
&>:48*:**       \1`!#^_2v
v_v#`\*:%*:*84\/*:*84::+<
v >::48*:*/\48*:*%%!#v_1^
>0"seY" >:#,_@#: "No">#0<
```


{{out}} (multiple runs)

```txt
0
No
17
Yes
49
No
97
Yes
1042441
No
1046527
Yes
```



## Bracmat


```bracmat
  ( prime
  =   incs n I inc
    .   4 2 4 2 4 6 2 6:?incs
      & 2:?n
      & 1 2 2 !incs:?I
      &   whl
        ' ( !n*!n:~>!arg
          & div$(!arg.!n)*!n:~!arg
          & (!I:%?inc ?I|!incs:%?inc ?I)
          & !n+!inc:?n
          )
      & !n*!n:>!arg
  )
& 100000000000:?p
&   whl
  ' ( !p+1:<100000000100:?p
    & (   prime$!p
        & out$!p
      |
      )
    )
& ;
```

```txt
100000000003
100000000019
100000000057
100000000063
100000000069
100000000073
100000000091
```



## Burlesque


```burlesque>fcL[2==</lang


Implicit trial division is done by the ''fc'' function. It checks if the number has exactly two divisors.

Version not using the ''fc'' function:


```burlesque
blsq ) 11^^2\/?dr@.%{0==}ayn!
1
blsq ) 12^^2\/?dr@.%{0==}ayn!
0
blsq ) 13^^2\/?dr@.%{0==}ayn!
1
```

Explanation. Given ''n'' generates a block containing ''2..n-1''. Calculates a block of modolus and check if it contains ''0''. If it contains ''0''
it is not a prime.


## C


```c
int is_prime(unsigned int n)
{
	unsigned int p;
	if (!(n & 1) || n < 2 ) return n == 2;

	/* comparing p*p <= n can overflow */
	for (p = 3; p <= n/p; p += 2)
		if (!(n % p)) return 0;
	return 1;
}
```



## C++


```cpp
#include <cmath>

bool is_prime(unsigned int n)
{
    if (n <= 1)
        return false;
    if (n == 2)
        return true;
    for (unsigned int i = 2; i <= sqrt(n); ++i)
        if (n % i == 0)
            return false;
    return true;
}
```


## C#

```c#
static bool isPrime(int n)
        {
            if (n <= 1) return false;
            for (int i = 2; i * i <= n; i++)
                if (n % i == 0) return false;
            return true;
        }
```



## Chapel

```chapel
proc is_prime(n)
{
    if n == 2 then
        return true;
    if n <= 1 || n % 2 == 0 then
        return false;
    for i in 3..floor(sqrt(n)):int by 2 do
        if n % i == 0 then
            return false;
    return true;
}
```



## Clojure

The function used in both versions:

```clojure
(defn divides? [k n] (zero? (mod k n)))
```


Testing divisors are in range from '''3''' to '''&radic;{{overline| n }}  ''' with step '''2''':

```clojure
(defn prime? [x]
  (or (= 2 x)
      (and (< 1 x)
           (odd? x)
           (not-any? (partial divides? x)
                     (range 3 (inc (Math/sqrt x)) 2)))))

```


Testing only prime divisors:

```clojure
(declare prime?)

(def primes (filter prime? (range)))

(defn prime? [x]
  (and (integer? x)
       (< 1 x)
       (not-any? (partial divides? x)
                 (take-while (partial >= (Math/sqrt x)) primes))))

```



## CMake


```cmake
# Prime predicate: does n be a prime number? Sets var to true or false.
function(primep var n)
  if(n GREATER 2)
    math(EXPR odd "${n} % 2")
    if(odd)
      # n > 2 and n is odd.
      set(factor 3)
      # Loop for odd factors from 3, while factor <= n / factor.
      math(EXPR quot "${n} / ${factor}")
      while(NOT factor GREATER quot)
        math(EXPR rp "${n} % ${factor}")        # Trial division
        if(NOT rp)
          # factor divides n, so n is not prime.
          set(${var} false PARENT_SCOPE)
          return()
        endif()
        math(EXPR factor "${factor} + 2")       # Next odd factor
        math(EXPR quot "${n} / ${factor}")
      endwhile(NOT factor GREATER quot)
      # Loop found no factor, so n is prime.
      set(${var} true PARENT_SCOPE)
    else()
      # n > 2 and n is even, so n is not prime.
      set(${var} false PARENT_SCOPE)
    endif(odd)
  elseif(n EQUAL 2)
    set(${var} true PARENT_SCOPE)       # 2 is prime.
  else()
    set(${var} false PARENT_SCOPE)      # n < 2 is not prime.
  endif()
endfunction(primep)
```



```cmake
# Quick example.
foreach(i -5 1 2 3 37 39)
  primep(b ${i})
  if(b)
    message(STATUS "${i} is prime.")
  else()
    message(STATUS "${i} is _not_ prime.")
  endif(b)
endforeach(i)
```



## COBOL


```cobol
       Identification Division.
       Program-Id. Primality-By-Subdiv.

       Data Division.
       Working-Storage Section.
       78  True-Val  Value 0.
       78  False-Val Value 1.

       Local-Storage Section.
       01  lim Pic 9(10).
       01  i   Pic 9(10).

       Linkage Section.
       01  num    Pic 9(10).
       01  result Pic 9.

       Procedure Division Using num result.
       Main.
           If num <= 1
               Move False-Val To result
               Goback
           Else If num = 2
               Move True-Val To result
               Goback
           End-If

           Compute lim = Function Sqrt(num) + 1
           Perform Varying i From 3 By 1 Until lim < i
               If Function Mod(num, i) = 0
                   Move False-Val To result
                   Goback
               End-If
           End-Perform

           Move True-Val To Result

           Goback
           .
```



## CoffeeScript


```coffeescript
is_prime = (n) ->
  # simple prime detection using trial division, works
  # for all integers
  return false if n <= 1 # by definition
  p = 2
  while p * p <= n
    return false if n % p == 0
    p += 1
  true

for i in [-1..100]
  console.log i if is_prime i
```



## Common Lisp


```Lisp
(defun primep (n)
  "Is N prime?"
  (and (> n 1)
       (or (= n 2) (oddp n))
       (loop for i from 3 to (isqrt n) by 2
	  never (zerop (rem n i)))))
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp
;; Project : Primality by trial division

(defun prime(n)
         (setq flag 0)
         (loop for i from 2 to (- n 1) do
                 (if (= (mod n i) 0)
                     (setq flag 1)))
                 (if (= flag 0)
                     (format t "~d is a prime number" n)
                     (format t "~d is not a prime number" n)))
(prime 7)
(prime 8)
```

Output:
 7 is a prime number
 8 is not a prime number


## D


### Simple Version


```d
import std.stdio, std.algorithm, std.range, std.math;

bool isPrime1(in int n) pure nothrow {
    if (n == 2)
        return true;
    if (n <= 1 || (n & 1) == 0)
        return false;

    for(int i = 3; i <= real(n).sqrt; i += 2)
        if (n % i == 0)
            return false;
    return true;
}

void main() {
    iota(2, 40).filter!isPrime1.writeln;
}
```

 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

### Version with excluded multiplies of 2 and 3

Same output.

```d
bool isPrime2(It)(in It n) pure nothrow {
    // Adapted from: http://www.devx.com/vb2themax/Tip/19051
    // Test 1, 2, 3 and multiples of 2 and 3:
    if (n == 2 || n == 3)
        return true;
    else if (n < 2 || n % 2 == 0 || n % 3 == 0)
        return false;

    // We can now avoid to consider multiples of 2 and 3. This
    // can be done really simply by starting at 5 and
    // incrementing by 2 and 4 alternatively, that is:
    //   5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35, 37, ...
    // We don't need to go higher than the square root of the n.
    for (It div = 5, inc = 2; div ^^ 2 <= n; div += inc, inc = 6 - inc)
        if (n % div == 0)
            return false;

    return true;
}

void main() {
    import std.stdio, std.algorithm, std.range;

    iota(2, 40).filter!isPrime2.writeln;
}
```



### Two Way Test

Odd divisors is generated both from increasing and decreasing sequence, may improve performance for numbers that have large minimum factor.
Same output.

```d
import std.stdio, std.algorithm, std.range, std.math;

bool isPrime3(T)(in T n) pure nothrow {
    if (n % 2 == 0 || n <= 1)
        return n == 2;
    T head = 3, tail = (cast(T)real(n).sqrt / 2) * 2 + 1;
    for ( ; head <= tail ; head +=2, tail -= 2)
        if ((n % head) == 0 || (n % tail) == 0)
            return false;
    return true;
}

void main() {
    iota(2, 40).filter!isPrime3.writeln;
}
```



## Delphi


###  First


```Delphi
function IsPrime(aNumber: Integer): Boolean;
var
  I: Integer;
begin
  Result:= True;
  if(aNumber = 2) then Exit;

  Result:= not ((aNumber mod 2 = 0)  or
                (aNumber <= 1));
  if not Result then Exit;

  for I:=3 to Trunc(Sqrt(aNumber)) do
  if(aNumber mod I = 0) then
  begin
    Result:= False;
    Break;
  end;
end;
```



###  Second


```Delphi
function IsPrime(const x: integer): Boolean;
var
  i: integer;
begin
  i := 2;
  repeat
    if X mod i = 0 then
    begin
      Result := False;
      Exit;
    end;
    Inc(i);
  until i > Sqrt(x);
  Result := True;
end;
```



## E

```e
def isPrime(n :int) {
    if (n == 2) {
        return true
    } else if (n <= 1 || n %% 2 == 0) {
        return false
    } else {
        def limit := (n :float64).sqrt().ceil()
        var divisor := 1
        while ((divisor += 2) <= limit) {
            if (n %% divisor == 0) {
                return false
            }
        }
        return true
    }
}
```



## EchoLisp


```scheme
(lib 'sequences)

;; Try divisors iff n = 2k + 1
(define (is-prime? p)
	(cond
	[(< p 2) #f]
	[(zero? (modulo p 2)) (= p 2)]
	[else
	(for/and ((d [3 5 .. (1+ (sqrt p))] ))  (!zero? (modulo p d)))]))

(filter is-prime? (range 1 100))
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)

;; Improve performance , try divisors iff  n = 6k+1 or n = 6k+5
(define (is-prime? p)
	(cond
	[(< p 2) #f]
	[(zero? (modulo p 2)) (= p 2)]
	[(zero? (modulo p 3)) (= p 3)]
	[(zero? (modulo p 5)) (= p 5)]
	[else  ;; step 6 : try divisors 6n+1 or 6n+5
		(for ((d [7 13 .. (1+ (sqrt p))] ))
			#:break (zero? (modulo p d)) => #f
			#:break (zero? (modulo p (+ d 4))) => #f
			#t )]))

(filter is-prime? (range 1 100))
    → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```



## Eiffel


```Eiffel
class
	APPLICATION

create
	make

feature

	make
                -- Tests the feature is_prime.
		do
			io.put_boolean (is_prime (1))
			io.new_line
			io.put_boolean (is_prime (2))
			io.new_line
			io.put_boolean (is_prime (3))
			io.new_line
			io.put_boolean (is_prime (4))
			io.new_line
			io.put_boolean (is_prime (97))
			io.new_line
			io.put_boolean (is_prime (15589))
			io.new_line
		end

	is_prime (n: INTEGER): BOOLEAN
                -- Is 'n' a prime number?
		require
			positiv_input: n > 0
		local
			i: INTEGER
			max: REAL_64
			math: DOUBLE_MATH
		do
			create math
			if n = 2 then
				Result := True
			elseif n <= 1 or n \\ 2 = 0 then
				Result := False
			else
				Result := True
				max := math.sqrt (n)
				from
					i := 3
				until
					i > max
				loop
					if n \\ i = 0 then
						Result := False
					end
					i := i + 2
				end
			end
		end

end
```


```txt
False
True
True
False
True
False
```



## Elixir

```elixir
defmodule RC do
  def is_prime(2), do: true
  def is_prime(n) when n<2 or rem(n,2)==0, do: false
  def is_prime(n), do: is_prime(n,3)

  def is_prime(n,k) when n<k*k, do: true
  def is_prime(n,k) when rem(n,k)==0, do: false
  def is_prime(n,k), do: is_prime(n,k+2)
end

IO.inspect for n <- 1..50, RC.is_prime(n), do: n
```

 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]


## Emacs Lisp

Use <tt>cl.el</tt> library.

```lisp
(defun prime (a)
  (not (or (< a 2)
           (loop for x from 2 to (sqrt a)
                 when (zerop (% a x))
                 return t))))
```

More concise, a little bit faster:

```lisp
(defun prime2 (a)
  (and (> a 1)
       (loop for x from 2 to (sqrt a)
             never (zerop (% a x)))))
```

A little bit faster:

```lisp
(defun prime3 (a)
  (and (> a 1)
       (or (= a 2) (oddp a))
       (loop for x from 3 to (sqrt a) by 2
             never (zerop (% a x)))))
```

More than 2 times faster, than the previous, doesn't use <tt>loop</tt> macro:

```lisp
(defun prime4 (a)
  (not (or (< a 2)
           (some (lambda (x) (zerop (% a x))) (number-sequence 2 (sqrt a))))))
```

Almost 2 times faster, than the previous:

```lisp
(defun prime5 (a)
  (not (or (< a 2)
           (and (/= a 2) (evenp a))
           (some (lambda (x) (zerop (% a x))) (number-sequence 3 (sqrt a) 2)))))
```



## Erlang


```erlang
is_prime(N) when N == 2 -> true;
is_prime(N) when N < 2 orelse N rem 2 == 0 -> false;
is_prime(N) -> is_prime(N,3).

is_prime(N,K) when K*K > N -> true;
is_prime(N,K) when N rem K == 0 -> false;
is_prime(N,K) -> is_prime(N,K+2).
```



## ERRE


```ERRE
PROGRAM PRIME_TRIAL

PROCEDURE ISPRIME(N%->OK%)
      LOCAL T%
      IF N%<=1 THEN OK%=FALSE  EXIT PROCEDURE END IF
      IF N%<=3 THEN OK%=TRUE EXIT PROCEDURE END IF
      IF (N% AND 1)=0 THEN OK%=FALSE EXIT PROCEDURE END IF
      FOR T%=3 TO SQR(N%) STEP 2 DO
        IF N% MOD T%=0 THEN OK%=FALSE EXIT PROCEDURE END IF
      END FOR
      OK%=TRUE
END PROCEDURE

BEGIN

      FOR I%=1 TO 100 DO
         ISPRIME(I%->OK%)
         IF OK% THEN PRINT(i%;"is prime") END IF
      END FOR

END PROGRAM
```

 2 is prime
 3 is prime
 5 is prime
 7  is prime
 11 is prime
 13 is prime
 17 is prime
 19 is prime
 23 is prime
 29 is prime
 31 is prime
 37 is prime
 41 is prime
 43 is prime
 47 is prime
 53 is prime
 59 is prime
 61 is prime
 67 is prime
 71 is prime
 73 is prime
 79 is prime
 83 is prime
 89 is prime
 97 is prime


## Euphoria


```euphoria
function is_prime(integer n)
    if n<=2 or remainder(n,2)=0 then
        return 0
    else
        for i=3 to sqrt(n) by 2 do
            if remainder(n,i)=0 then
                return 0
            end if
        end for
        return 1
    end if
end function
```



## Factor


```factor
USING: combinators kernel math math.functions math.ranges sequences ;

: prime? ( n -- ? )
    {
        { [ dup 2 < ] [ drop f ] }
        { [ dup even? ] [ 2 = ] }
        [ 3 over sqrt 2 <range> [ mod 0 > ] with all? ]
    } cond ;
```



## FALSE


```false
[0\$2=$[@~@@]?~[$$2>\1&&[\~\
   3[\$@$@1+\$*>][\$@$@$@$@\/*=[%\~\$]?2+]#%
]?]?%]p:
```



## FBSL

The second function (included by not used) I would have thought would be faster because it lacks the SQR() function. As it happens, the first is over twice as fast.

```qbasic
#APPTYPE CONSOLE

FUNCTION ISPRIME(n AS INTEGER) AS INTEGER
    IF n = 2 THEN
        RETURN TRUE
    ELSEIF n <= 1 ORELSE n MOD 2 = 0 THEN
        RETURN FALSE
    ELSE
        FOR DIM i = 3 TO SQR(n) STEP 2
            IF n MOD i = 0 THEN
                RETURN FALSE
            END IF
        NEXT
        RETURN TRUE
    END IF
END FUNCTION

FUNCTION ISPRIME2(N AS INTEGER) AS INTEGER
    IF N <= 1 THEN RETURN FALSE
    DIM I AS INTEGER = 2
    WHILE I * I <= N
        IF N MOD I = 0 THEN
            RETURN FALSE
        END IF
        I = I + 1
    WEND
    RETURN TRUE
END FUNCTION

' Test and display primes 1 .. 50
DIM n AS INTEGER

FOR n = 1 TO 50
    IF ISPRIME(n) THEN
        PRINT n, " ";
    END IF
NEXT

PAUSE
```

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
 Press any key to continue...


## Forth



```forth
: prime? ( n -- f )
        dup 2 < if      drop false
    else dup 2 = if      drop true
    else dup 1 and 0= if drop false
    else 3
        begin 2dup dup * >=
        while 2dup mod 0=
              if       2drop false exit
              then 2 +
        repeat         2drop true
    then then then ;
```



## Fortran

```fortran
 FUNCTION isPrime(number)
   LOGICAL :: isPrime
   INTEGER, INTENT(IN) :: number
   INTEGER :: i

   IF(number==2) THEN
      isPrime = .TRUE.
   ELSE IF(number < 2 .OR. MOD(number,2) == 0) THEN
      isPRIME = .FALSE.
   ELSE
      isPrime = .TRUE.
      DO i = 3, INT(SQRT(REAL(number))), 2
         IF(MOD(number,i) == 0) THEN
            isPrime = .FALSE.
            EXIT
         END IF
      END DO
   END IF
 END FUNCTION
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isPrime(n As Integer) As Boolean
  If n < 2 Then Return False
  If n = 2 Then Return True
  If n Mod 2  = 0 Then Return False
  Dim limit As Integer = Sqr(n)
  For i As Integer = 3 To limit Step 2
    If n Mod i = 0 Then Return False
  Next
  Return True
End Function

' To test this works, print all primes under 100
For i As Integer = 1 To 99
  If isPrime(i) Then
    Print Str(i); " ";
  End If
Next

Print : Print
Print "Press any key to quit"
Sleep
```


 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

=={{header|F Sharp|F#}}==

```fsharp
open NUnit.Framework
open FsUnit
let inline isPrime n = not ({uint64 2..uint64 (sqrt (double n))} |> Seq.exists (fun (i:uint64) -> uint64 n % i = uint64 0))
[<Test>]
let ``Validate that 2 is prime`` () =
  isPrime 2 |> should equal true

[<Test>]
let ``Validate that 4 is not prime`` () =
  isPrime 4 |> should equal false

[<Test>]
let ``Validate that 3 is prime`` () =
  isPrime 3 |> should equal true

[<Test>]
let ``Validate that 9 is not prime`` () =
  isPrime 9 |> should equal false

[<Test>]
let ``Validate that 5 is prime`` () =
  isPrime 5 |> should equal true

[<Test>]
let ``Validate that 277 is prime`` () =
  isPrime 277 |> should equal true
```

 > isPrime 1111111111111111111UL;;
 val it : bool = true

and if you want to test really big numbers, use System.Numerics.BigInteger, but it's slower:

```fsharp

let isPrimeI x =
    if x < 2I then false else
    if x = 2I then true else
    if x % 2I = 0I then false else
    let rec test n =
      if n * n > x then true else
      if x % n = 0I then false else test (n + 2I) in test 3I
```


If you have a lot of prime numbers to test, caching a sequence of primes can speed things up considerably, so you only have to do the divisions against prime numbers.

```fsharp
let rec primes = Seq.cache(Seq.append (seq[ 2; 3; 5 ]) (Seq.unfold (fun state -> Some(state, state + 2)) 7 |> Seq.filter (fun x -> IsPrime x)))

and IsPrime number =
    let rec IsPrimeCore number current limit =
        let cprime = primes |> Seq.nth current
        if cprime >= limit then
            true
        else if number % cprime = 0 then
            false
        else
            IsPrimeCore number (current + 1) (number/cprime)

    if number = 2 then
        true
    else if number < 2 then
        false
    else
        IsPrimeCore number 0 number
```



## FunL


```funl
import math.sqrt

def
  isPrime( 2 )      =  true
  isPrime( n )
    | n < 3 or 2|n  =  false
    | otherwise     =  (3..int(sqrt(n)) by 2).forall( (/|n) )

(10^10..10^10+50).filter( isPrime ).foreach( println )
```

 10000000019
 10000000033


## FutureBasic


```futurebasic
include "ConsoleWindow"

def tab 6

local fn isPrime( n as long ) as Boolean
dim as long i
dim as Boolean result

if n < 2 then result = _false : exit fn
if n = 2 then result = _true  : exit fn
if n mod 2 == 0  then result = _false : exit fn
result = _true
for i = 3 to int( n^.5 ) step 2
  if n mod i == 0 then result = _false : exit fn
next i
end fn = result

dim as long i, j

print "Prime numbers between 0 and 1000:"
for i = 0 to 1000
  if ( fn isPrime(i) != _false )
    print i, : j++
    if j mod 10 == 0 then print
  end if
next
```

Output:
 Prime numbers between 0 and 1000:
 2     3     5     7     11    13    17    19    23    29
 31    37    41    43    47    53    59    61    67    71
 73    79    83    89    97    101   103   107   109   113
 127   131   137   139   149   151   157   163   167   173
 179   181   191   193   197   199   211   223   227   229
 233   239   241   251   257   263   269   271   277   281
 283   293   307   311   313   317   331   337   347   349
 353   359   367   373   379   383   389   397   401   409
 419   421   431   433   439   443   449   457   461   463
 467   479   487   491   499   503   509   521   523   541
 547   557   563   569   571   577   587   593   599   601
 607   613   617   619   631   641   643   647   653   659
 661   673   677   683   691   701   709   719   727   733
 739   743   751   757   761   769   773   787   797   809
 811   821   823   827   829   839   853   857   859   863
 877   881   883   887   907   911   919   929   937   941
 947   953   967   971   977   983   991   997


## Gambas

'''[https://gambas-playground.proko.eu/?gist=85fbc7936b17b3009af282752aa29df7 Click this link to run this code]'''

```gambas
'Reworked from the BBC Basic example

Public Sub Main()
Dim iNum As Integer

For iNum = 1 To 100
  If FNisprime(iNum) Then Print iNum & " is prime"
Next

End
'___________________________________________________
Public Sub FNisprime(iNum As Integer) As Boolean
Dim iLoop As Integer
Dim bReturn As Boolean = True

If iNum <= 1 Then bReturn = False
If iNum <= 3 Then bReturn = True
If (iNum And 1) = 0 Then bReturn = False

For iLoop = 3 To Sqr(iNum) Step 2
  If iNum Mod iLoop = 0 Then bReturn = False
Next

Return bReturn

End
```

Output:

```txt
1 is prime
3 is prime
5 is prime
7 is prime
11 is prime
......
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
```



## GAP


```gap
IsPrimeTrial := function(n)
   local k, m;
   if n < 5 then
      return (n = 2) or (n = 3);
   fi;
   if RemInt(n, 2) = 0 then
      return false;
   fi;
   m := RootInt(n);
   k := 3;
   while k <= m do
      if RemInt(n, k) = 0 then
         return false;
      fi;
      k := k + 2;
   od;
   return true;
end;

Filtered([1 .. 100], IsPrimeTrial);
# [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
```



## Go


```go
func IsPrime(n int) bool {
	if n < 0 { n = -n }
	switch {
        case n == 2:
		return true
	case n < 2 || n % 2 == 0:
		return false

	default:
		for i = 3; i*i <= n; i += 2 {
			if n % i == 0 { return false }
		}
	}
	return true
}
```


Or, using recursion:


```go
func IsPrime(n int) bool {
	if n < 0 { n = -n }
	if n <= 2 {
		return n == 2
	}
	return n % 2 != 0 && isPrime_r(n, 3)
}

func isPrime_r(n, i int) bool {
	if i*i <= n {
		return n % i != 0 && isPrime_r(n, i+2)
	}
	return true
}
```



## Groovy


```groovy
def isPrime = {
    it == 2 ||
    it > 1 &&
    (2..Math.max(2, (int) Math.sqrt(it))).every{ k -> it % k != 0 }
}

(0..20).grep(isPrime)
```

 [2, 3, 5, 7, 11, 13, 17, 19]


## Haskell

(used [[Emirp_primes#List-based|here]] and [[Sequence_of_primes_by_Trial_Division#Haskell|here]]). The basic divisibility test by odd numbers:

```haskell
isPrime n = n==2 || n>2 && all ((> 0).rem n) (2:[3,5..floor.sqrt.fromIntegral $ n+1])
```


Testing by prime numbers only is faster. Primes list is saved for reuse. Precalculation of primes pays off if testing more than just a few numbers, and/or primes are generated efficiently enough:

```haskell
noDivsBy factors n = foldr (\f r-> f*f>n || ((rem n f /= 0) && r)) True factors

-- primeNums = filter (noDivsBy [2..]) [2..]
--           = 2 : filter (noDivsBy [3,5..]) [3,5..]
primeNums = 2 : 3 : filter (noDivsBy $ tail primeNums) [5,7..]

isPrime n = n > 1 && noDivsBy primeNums n
```

Any increasing ''unbounded'' sequence of numbers that includes all primes (above the first few, perhaps) could be used with the testing function <code>noDivsBy</code> to define the <code>isPrime</code> function -- but using just primes is better, produced e.g. by [[Sieve of Eratosthenes#Haskell|Sieve of Eratosthenes]], or <code>noDivsBy</code> itself can be used to define <code>primeNums</code> as shown above, because it stops when the square root is reached (so there's no infinite recursion, no "vicious circle").

A less efficient, more basic variant:

```haskell>isPrime n = n
 1 && []==[i | i <- [2..n-1], rem n i == 0]
```


The following is an attempt at improving it, resulting in absolutely worst performing prime testing code I've ever seen, ever. A curious oddity.

```haskell>isPrime n = n
 1 && []==[i | i <- [2..n-1], isPrime i && rem n i == 0]
```



## HicEst


```HicEst
   DO n = 1, 1E6
     Euler = n^2 + n + 41
     IF( Prime(Euler) == 0 ) WRITE(Messagebox) Euler, ' is NOT prime for n =', n
   ENDDO                            ! e.g.    1681 = 40^2 + 40 + 41 is NOT prime
END

FUNCTION Prime(number)
   Prime = number == 2
   IF( (number > 2) * MOD(number,2) ) THEN
     DO i = 3, number^0.5, 2
       IF(MOD(number,i) == 0) THEN
         Prime = 0
         RETURN
       ENDIF
     ENDDO
     Prime = 1
   ENDIF
END
```


=={{header|Icon}} and {{header|Unicon}}==
Procedure shown without a main program.

```Icon
procedure isprime(n)                            #: return n if prime (using trial division) or fail
if not n = integer(n) | n < 2 then fail         # ensure n is an integer greater than 1
every if 0 = (n % (2 to sqrt(n))) then fail
return n
end
```



## J

```j
isprime=: 3 : 'if. 3>:y do. 1<y else. 0 *./@:< y|~2+i.<.%:y end.'
```



## Java


```java
public static boolean prime(long a){
   if(a == 2){
      return true;
   }else if(a <= 1 || a % 2 == 0){
      return false;
   }
   long max = (long)Math.sqrt(a);
   for(long n= 3; n <= max; n+= 2){
      if(a % n == 0){ return false; }
   }
   return true;
}
```


### By Regular Expression


```java
public static boolean prime(int n) {
    return !new String(new char[n]).matches(".?|(..+?)\\1+");
}
```



## JavaScript


```javascript
function isPrime(n) {
  if (n == 2 || n == 3 || n == 5 || n == 7) {
    return true;
  } else if ((n < 2) || (n % 2 == 0)) {
    return false;
  } else {
    for (var i = 3; i <= Math.sqrt(n); i += 2) {
      if (n % i == 0)
        return false;
    }
    return true;
  }
}
```



## Joy

From [http://www.latrobe.edu.au/phimvt/joy/jp-imper.html here]

```joy
DEFINE prime ==
        2
        [ [dup * >] nullary  [rem 0 >] dip  and ]
        [ succ ]
        while
        dup * < .
```



## jq

<tt> def is_prime:
   if . == 2 then true
   else 2 < . and . % 2 == 1 and
        . as $in
        | (($in + 1) | sqrt) as $m
        | (((($m - 1) / 2) | floor) + 1) as $max
        | all( range(1; $max) ; $in % ((2 * .) + 1) > 0 )
   end;</tt>

Example -- the command line is followed by alternating lines of input and output:
<tt> $ jq -f is_prime.jq
 -2
 false
 1
 false
 2
 true
 100
 false</tt>

''Note: if your jq does not have <tt>all</tt>, the following will suffice:''

 def all(generator; condition):
   reduce generator as $i (true; . and ($i|condition));


## Julia

Julia already has an <tt>isprime</tt> function, so this function has the verbose name <tt>isprime_trialdivision</tt> to avoid overriding the built-in function.  Note this function relies on the fact that Julia skips <tt>for</tt>-loops having invalid ranges.  Otherwise the function would have to include additional logic to check the odd numbers less than 9.

```Julia
function isprime_trialdivision{T<:Integer}(n::T)
    1 < n || return false
    n != 2 || return true
    isodd(n) || return false
    for i in 3:isqrt(n)
        n%i != 0 || return false
    end
    return true
end

n = 100
a = filter(isprime_trialdivision, [1:n])

if all(a .== primes(n))
    println("The primes <= ", n, " are:\n    ", a)
else
    println("The function does not accurately calculate primes.")
end
```

 The primes <= 100 are:
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


## K


```K
   isprime:{(x>1)&&/x!'2_!1+_sqrt x}
   &isprime'!100
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```



## Kotlin


```scala
// version 1.1.2
fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    val limit = Math.sqrt(n.toDouble()).toInt()
    return (3..limit step 2).none { n % it == 0 }
}

fun main(args: Array<String>) {
    // test by printing all primes below 100 say
    (2..99).filter { isPrime(it) }.forEach { print("$it ") }
}
```

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97


## Langur


###  Recursive

```Langur
val .isPrime = f(.i) {
    val .n = abs(.i)
    if .n <= 2 {
        return .n == 2
    }

    val .chkdiv = f(.n, .i) {
        if .i x .i <= .n {
            return .n rem .i != 0 and self(.n, .i+2)
        }
        return true
    }

    return .n rem 2 != 0 and .chkdiv(.n, 3)
}

writeln where .isPrime, series 100
```



###  Functional

Following the Perl 6 example, which states, "Integer $i is prime if it is greater than one and is divisible by none of 2, 3, up to the square root of $i" (plus an adjustment for the prime number 2).

```Langur>val .isPrime = f .i == 2 or .i
 2 and not any f(.x) .i rem .x == 0, pseries 2 to .i ^/ 2

writeln where .isPrime, series 100
```


```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



## Liberty BASIC


```lb
print "Rosetta Code - Primality by trial division"
print
[start]
input "Enter an integer: "; x
if x=0 then print "Program complete.": end
if isPrime(x) then print x; " is prime" else print x; " is not prime"
goto [start]

function isPrime(p)
    p=int(abs(p))
    if p=2 or then isPrime=1: exit function 'prime
    if p=0 or p=1 or (p mod 2)=0 then exit function 'not prime
    for i=3 to sqr(p) step 2
        if (p mod i)=0 then exit function 'not prime
    next i
    isPrime=1
end function
```

```txt
Rosetta Code - Primality by trial division

Enter an integer: 1
1 is not prime
Enter an integer: 2
2 is prime
Enter an integer:
Program complete.
```



## Lingo


```Lingo
on isPrime (n)
    if n<=1 or (n>2 and n mod 2=0) then return FALSE
    sq = sqrt(n)
    repeat with i = 3 to sq
        if n mod i = 0 then return FALSE
    end repeat
    return TRUE
end
```


```Lingo
primes = []
repeat with i = 0 to 100
    if isPrime(i) then primes.add(i)
end repeat
put primes
```

 -- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]


## Logo


```logo
to prime? :n
   if :n < 2 [output "false]
   if :n = 2 [output "true]
   if equal? 0 modulo :n 2 [output "false]
   for [i 3 [sqrt :n] 2] [if equal? 0 modulo :n :i [output "false]]
   output "true
end
```



## LSE64


```LSE64
over : 2 pick
 2dup : over over
 even? : 1 & 0 =

 # trial n d yields "n d 0/1 false" or "n d+2 true"
 trial : 2 +                 true
 trial : 2dup % 0 =   then 0 false
 trial : 2dup dup * < then 1 false
 trial-loop : trial &repeat

 # prime? n yields flag
 prime? : 3 trial-loop >flag drop drop
 prime? : dup even? then drop false
 prime? : dup 2 =   then drop true
 prime? : dup 2 <   then drop false
```



## Lua


```Lua
function IsPrime( n )
    if n <= 1 or ( n ~= 2 and n % 2 == 0 ) then
        return false
    end

    for i = 3, math.sqrt(n), 2 do
	if n % i == 0 then
  	    return false
	end
    end

    return true
end
```

Type of number Decimal.

## M2000 Interpreter


```M2000 Interpreter

      Inventory Known1=2@, 3@
      IsPrime=lambda  Known1 (x as decimal) -> {
            =0=1
            if exist(Known1, x) then =1=1 : exit
            if x<=5 OR frac(x) then {if x == 2 OR x == 3 OR x == 5 then Append Known1, x  : =1=1
            Break}
            if frac(x/2) else exit
            if frac(x/3) else exit
            x1=sqrt(x):d = 5@
            {if frac(x/d ) else exit
                  d += 2: if d>x1 then Append Known1, x : =1=1 : exit
                  if frac(x/d) else exit
                  d += 4: if d<= x1 else Append Known1, x :  =1=1: exit
             loop}
      }

      i=2
      While Len(Known1)<20 {
            dummy=Isprime(i)
            i++
      }
      Print "first ";len(known1);" primes"
      Print Known1
      Print "From 110 to 130"
      count=0
      For i=110 to 130 {
            If isPrime(i) Then Print i, : count++
      }
      Print
      Print "Found ";count;" primes"

```


## M4


```M4
define(`testnext',
   `ifelse(eval($2*$2>$1),1,
      1,
      `ifelse(eval($1%$2==0),1,
         0,
         `testnext($1,eval($2+2))')')')
define(`isprime',
   `ifelse($1,2,
      1,
      `ifelse(eval($1<=1 || $1%2==0),1,
         0,
         `testnext($1,3)')')')

isprime(9)
isprime(11)
```

 0
 1


## Maple

This could be coded in myriad ways; here is one.

```Maple
TrialDivision := proc( n :: integer )
        if n <= 1 then
                false
        elif n = 2 then
                true
        elif type( n, 'even' ) then
                false
        else
                for local i from 3 by 2 while i * i <= n do
                        if irem( n, i ) = 0 then
                                return false
                        end if
                end do;
                true
        end if
end proc:
```

Using this to pick off the primes up to 30, we get:

```Maple>
 select( TrialDivision, [seq]( 1 .. 30 ) );
                  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```

Here is a way to check that TrialDivision above agrees with Maple's built-in primality test (isprime).

```Maple>
 N := 10000: evalb( select( TrialDivision, [seq]( 1 .. N ) ) = select( isprime, [seq]( 1 .. N ) ) );
                                  true
```



## Mathematica


```mathematica
IsPrime[n_Integer] := Block[{},
  If[n <= 1, Return[False]];
  If[n == 2, Return[True]]; If[Mod[n, 2] == 0, Return[False]];
  For[k = 3, k <= Sqrt[n], k += 2, If[Mod[n, k] == 0, Return[False]]];
  Return[True]]
```



## MATLAB


```MATLAB
function isPrime = primalityByTrialDivision(n)

    if n == 2
        isPrime = true;
        return
    elseif (mod(n,2) == 0) || (n <= 1)
        isPrime = false;
        return
    end

    %First n mod (3 to sqrt(n)) is taken. This will be a vector where the
    %first element is equal to n mod 3 and the last element is equal to n
    %mod sqrt(n). Then the all function is applied to that vector. If all
    %of the elements of this vector are non-zero (meaning n is prime) then
    %all() returns true. Otherwise, n is composite, so it returns false.
    %This return value is then assigned to the variable isPrime.
    isPrime = all(mod(n, (3:round(sqrt(n))) ));

end
```

```MATLAB>>
 arrayfun(@primalityByTrialDivision,(1:14))

ans =

     0     1     1     0     1     0     1     0     0     0     1     0     1     0
```



## Maxima


```Maxima
isprme(n):= catch(
  for k: 2 thru sqrt(n) do if mod(n, k)=0 then throw(false),
  true);

map(isprme, [2, 3, 4, 65, 100, 181, 901]);
/* [true, true, false, false, false, true, false] */
```



## min

```min
(
  :n 3 :i n sqrt :m true :p
  (i m <=) (
    (n i mod 0 ==) (m @i false @p) when
    i 2 + @i
  ) while p
) :_prime?  ; helper function

(
  (
    ((2 <) (false))
    ((dup even?) (2 ==))
    ((true) (_prime?))
  ) case
) :prime?
```


=={{header|MK-61/52}}==
<lang>П0	1	-	x#0	34	2	-	/-/	x<0	32
ИП0	2	/	{x}	x#0	34
3	П4	ИП0	ИП4	/	{x}	x#0	34	КИП4	КИП4
ИП0	КвКор	ИП4	-	x<0	16	1	С/П	0	С/П
```



## MUMPS


```MUMPS
ISPRIME(N)
 QUIT:(N=2) 1
 NEW I,R
 SET R=N#2
 IF R FOR I=3:2:(N**.5) SET R=N#I Q:'R
 KILL I
 QUIT R
```

Usage (0 is false, nonzero is true):

```txt
USER>W $$ISPRIME^ROSETTA(2)
1
USER>W $$ISPRIME^ROSETTA(4)
0
USER>W $$ISPRIME^ROSETTA(7)
1
USER>W $$ISPRIME^ROSETTA(97)
7
USER>W $$ISPRIME^ROSETTA(99)
0
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

parse arg nbr rangeBegin rangeEnd .

if nbr = '' | nbr = '.' then do
  if rangeBegin = '' | rangeBegin = '.' then rangeBegin = 1
  if rangeEnd   = '' | rangeEnd   = '.' then rangeEnd   = 100
  if rangeEnd > rangeBegin then direction = 1
                           else direction = -1

  say 'List of prime numbers from' rangeBegin 'to' rangeEnd':'
  primes = ''
  loop nn = rangeBegin to rangeEnd by direction
    if isPrime(nn) then primes = primes nn
    end nn
    primes = primes.strip
    say '  'primes.changestr(' ', ',')
    say '  Total number of primes:' primes.words
  end
else do
  if isPrime(nbr) then say nbr.right(20) 'is prime'
                  else say nbr.right(20) 'is not prime'
  end

return

method isPrime(nbr = long) public static binary returns boolean

  ip = boolean

  select
    when nbr < 2 then do
      ip = isFalse
      end
    when '2 3 5 7'.wordpos(Rexx(nbr)) \= 0 then do
      -- crude shortcut ripped from the Rexx example
      ip = isTrue
      end
    when  nbr // 2 == 0 | nbr // 3 == 0 then do
      -- another shortcut permitted by the one above
      ip = isFalse
      end
    otherwise do
      nn = long
      nnStartTerm = long 3 -- a reasonable start term - nn <= 2 is never prime
      nnEndTerm = long Math.ceil(Math.sqrt(nbr)) -- a reasonable end term
      ip = isTrue -- prime the pump (pun intended)
      loop nn = nnStartTerm to nnEndTerm by 2
         -- Note: in Rexx and NetRexx "//" is the 'remainder of division operator' (which is not the same as modulo)
        if nbr // nn = 0 then do
          ip = isFalse
          leave nn
          end
        end nn
      end
    end

  return ip

method isTrue public static returns boolean
  return 1 == 1

method isFalse public static returns boolean
  return \isTrue
```

```txt
$ java -cp . RCPrimality
List of prime numbers from 1 to 100:
  2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
  Total number of primes: 25

$ java -cp . RCPrimality 91
                  91 is not prime

$ java -cp . RCPrimality 101
                 101 is prime

$ java -cp . RCPrimality . . 25
List of prime numbers from 1 to 25:
  2,3,5,7,11,13,17,19,23
  Total number of primes: 9

$ java -cp . RCPrimality . 9900 10010
List of prime numbers from 9900 to 10010:
  9901,9907,9923,9929,9931,9941,9949,9967,9973,10007,10009
  Total number of primes: 11

$ java -cp . RCPrimality . -57 1
List of prime numbers from -57 to 1:

  Total number of primes: 0

$ java -cp . RCPrimality . 100 -57
List of prime numbers from 100 to -57:
  97,89,83,79,73,71,67,61,59,53,47,43,41,37,31,29,23,19,17,13,11,7,5,3,2
  Total number of primes: 25
```

===[[#REXX|Rexx]] version reimplemented in [[NetRexx]]===
```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

/*REXX program tests for primality using (kinda smartish) trial division*/

parse arg n .                          /*let user choose how many, maybe*/
if n=='' then n=10000                  /*if not, then assume the default*/
p=0                                    /*a count of primes  (so far).   */
                                       /*I like Heinz's 57 varieties... */
  loop j=-57 to n                      /*start in the cellar and work up*/
  if \isprime(j) then iterate          /*if not prime, keep looking.    */
  p=p+1                                /*bump the jelly bean counter.   */
  if j.length>2 then iterate           /*only show two-digit primes.    */
  say j.right(20) 'is prime.'          /*Just blab about the wee primes.*/
  end

say
say "there're" p "primes up to" n '(inclusive).'
exit

/*-------------------------------------ISPRIME subroutine---------------*/
method isprime(x) public static returns boolean
--isprime: procedure; arg x                   /*get the number in question*/
if '2 3 5 7'.wordpos(x)\==0 then return 1   /*is it a teacher's pet?    */
if x<2 | x//2==0 | x//3==0  then return 0   /*weed out the riff-raff.   */
                                            /*Note:   //  is modulus.   */
   loop j=5 by 6 until j*j>x                /*skips multiples of three. */
   if x//j==0 | x//(j+2)==0 then return 0   /*do a pair of divides (mod)*/
   end

return 1                                    /*I'm exhausted, it's prime!*/
```



## Nim


```nim
import sequtils, math

proc prime(a: int): bool =
  if a == 2: return true
  if a < 2 or a mod 2 == 0: return false
  for i in countup(3, sqrt(a.float).int, 2):
    if a mod i == 0:
      return false
  return true

template any(sequence, operation: expr): expr =
  var result {.gensym.}: bool = false
  for i in 0 .. <sequence.len:
    let it {.inject.} = sequence[i]
    result = operation
    if result:
      break
  result

proc prime2(a: int): bool =
  result = not (a < 2 or any(toSeq(2 .. sqrt(a.float).int), a mod it == 0))

proc prime3(a: int): bool =
  if a == 2: return true
  if a < 2 or a mod 2 == 0: return false
  return not any(toSeq countup(3, sqrt(a.float).int, 2), a mod it == 0)

for i in 2..30:
  echo i, " ", prime(i)
```



## Objeck


```objeck
function : IsPrime(n : Int) ~ Bool {
  if(n <= 1) {
    return false;
  };

  for(i := 2; i * i <= n; i += 1;) {
    if(n % i = 0) {
      return false;
    };
  };

  return true;
}
```



## OCaml


```ocaml
let is_prime n =
  if n = 2 then true
  else if n < 2 || n mod 2 = 0 then false
  else
    let rec loop k =
      if k * k > n then true
      else if n mod k = 0 then false
      else loop (k+2)
    in loop 3
```



## Octave

This function works on vectors and matrix.

```octave
function b = isthisprime(n)
  for r = 1:rows(n)
    for c = 1:columns(n)
      b(r,c) = false;
      if ( n(r,c) == 2 )
	b(r,c) = true;
      elseif ( (n(r,c) < 2) || (mod(n(r,c),2) == 0) )
	b(r,c) = false;
      else
	b(r,c) = true;
	for i = 3:2:sqrt(n(r,c))
	  if ( mod(n(r,c), i) == 0 )
	    b(r,c) = false;
	    break;
	  endif
	endfor
      endif
    endfor
  endfor
endfunction

% as test, print prime numbers from 1 to 100
p = [1:100];
pv = isthisprime(p);
disp(p( pv ));
```



## Oforth


```Oforth
Integer method: isPrime
| i |
   self 1 <= ifTrue: [ false return ]
   self 3 <= ifTrue: [ true return ]
   self isEven ifTrue: [ false return ]
   3 self sqrt asInteger for: i [ self i mod ifzero: [ false return ] ]
   true ;
```

```txt
#isPrime 1000 seq filter println
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 8
9, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181
, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281
, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397
, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503
, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619
, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743
, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863
, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997
]
```



## Oz


```oz
   fun {Prime N}
      local IPrime in
	 fun {IPrime N Acc}
	    if N < Acc*Acc then true
	    elseif (N mod Acc) == 0 then false
	    else {IPrime N Acc+1}
	    end
	 end
	 if N < 2 then false
	 else {IPrime N 2} end
      end
   end
```



## PARI/GP


```parigp
trial(n)={
  if(n < 4, return(n > 1)); /* Handle negatives */
  forprime(p=2,sqrt(n),
    if(n%p == 0, return(0))
  );
  1
};
```



## Panda

In Panda you write a boolean function by making it filter, either returning it's input or nothing.

```panda
fun prime(p) type integer->integer
  p.gt(1) where q=p.sqrt NO(p.mod(2..q)==0)

1..100.prime
```



## Pascal

```Pascal
program primes;

function prime(n: integer): boolean;
var
  i: integer; max: real;
begin
  if n = 2 then
    prime := true
  else if (n <= 1) or (n mod 2 = 0) then
    prime := false
  else begin
    prime := true; i := 3; max := sqrt(n);
    while i <= max do begin
      if n mod i = 0 then begin
        prime := false; exit
      end;
      i := i + 2
    end
  end
end;

{ Test and display primes 0 .. 50 }
var
  n: integer;
begin
  for n := 0 to 50 do
    if (prime(n)) then
      write(n, ' ');
end.
```

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

### improved using number wheel

```pascal
program TestTrialDiv;
{$IFDEF FPC}
  {$MODE DELPHI}{$Smartlink ON}
{$ELSE}
  {$APPLICATION CONSOLE}// for Delphi
{$ENDIF}
uses
  primtrial;
{ Test and display primes 0 .. 50 }
begin
  repeat
    write(actPrime,' ');
  until nextPrime > 50;
end.
```

;Output:
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47


## Perl

A simple idiomatic solution:

```perl
sub prime { my $n = shift || $_;
    $n % $_ or return for 2 .. sqrt $n;
    $n > 1
}

print join(', ' => grep prime, 1..100), "\n";
```



### Excluding multiples of 2 and 3

One of many ways of writing trial division using a mod-6 wheel.  Almost 2x faster than the simple method shown earlier.

```perl
sub isprime {
  my $n = shift;
  return ($n >= 2) if $n < 4;
  return unless $n % 2  &&  $n % 3;
  my $sqrtn = int(sqrt($n));
  for (my $i = 5; $i <= $sqrtn; $i += 6) {
    return unless $n % $i && $n % ($i+2);
  }
  1;
}
my $s = 0;
$s += !!isprime($_) for 1..100000;
print "Pi(100,000) = $s\n";
```



### By Regular Expression

JAPH by Abigail 1999 [http://diswww.mit.edu/bloom-picayune.mit.edu/perl/12606] in conference slides 2000 [http://www.perlmonks.org/?node_id=21580].

While this is extremely clever and often used for [[wp:Code golf|Code golf]], it should never be used for real work (it is extremely slow and uses lots of memory).

```perl
sub isprime {
  ('1' x shift) !~ /^1?$|^(11+?)\1+$/
}
print join(', ', grep(isprime($_), 0..39)), "\n";
```



## Perl 6

Here we use a "none" junction which will autothread through the <tt>%%</tt> "is divisible by" operator to assert that <tt>$i</tt> is not divisible by 2 or any of the numbers up to its square root.  Read it just as you would English: "Integer <tt>$i</tt> is prime if it is greater than one and is divisible by none of 2, 3, up to the square root of <tt>$i</tt>."

```perl6
sub prime (Int $i --> Bool) {
    $i > 1 and so $i %% none 2..$i.sqrt;
}
```


This can easily be improved in two ways.  First, we generate the primes so we only divide by those, instead of all odd numbers.  Second, we memoize the result using the <tt>//=</tt> idiom of Perl, which does the right-hand calculation and assigns it only if the left side is undefined.  We avoid recalculating the square root each time. Note the mutual recursion that depends on the implicit laziness of list evaluation:

```perl6
my @primes = 2, 3, 5, -> $p { ($p+2, $p+4 ... &prime)[*-1] } ... *;
my @isprime = False,False;   # 0 and 1 are not prime by definition
sub prime($i) {
    my \limit = $i.sqrt.floor;
    @isprime[$i] //= so ($i %% none @primes ...^ { $_ > limit })
}

say "$_ is{ "n't" x !prime($_) } prime." for 1 .. 100;
```



## Phix


```Phix
function is_prime(integer n)
    if n<2 then return 0 end if
    if n=2 then return 1 end if
    if remainder(n,2)=0 then return 0 end if
    for i=3 to floor(sqrt(n)) by 2 do
        if remainder(n,i)=0 then
            return 0
        end if
    end for
    return 1
end function
```



## PHP


```php
<?php
function prime($a) {
  if (($a % 2 == 0 && $a != 2) || $a < 2)
    return false;
  $limit = sqrt($a);
  for ($i = 2; $i <= $limit; $i++)
    if ($a % $i == 0)
      return false;
  return true;
}

foreach (range(1, 100) as $x)
  if (prime($x)) echo "$x\n";

?>
```


### By Regular Expression


```php
<?php
function prime($a) {
  return !preg_match('/^1?$|^(11+?)\1+$/', str_repeat('1', $a));
}
?>
```



## PicoLisp


```PicoLisp
(de prime? (N)
   (or
      (= N 2)
      (and
         (> N 1)
         (bit? 1 N)
         (let S (sqrt N)
            (for (D 3  T  (+ D 2))
               (T (> D S) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )
```



## PL/I


```PL/I
is_prime: procedure (n) returns (bit(1));
   declare n fixed (15);
   declare i fixed (10);

   if n < 2 then return ('0'b);
   if n = 2 then return ('1'b);
   if mod(n, 2) = 0 then return ('0'b);

   do i = 3 to sqrt(n) by 2;
      if mod(n, i) = 0 then return ('0'b);
   end;
   return ('1'b);
end is_prime;
```



## PowerShell


```PowerShell

function isPrime ($n) {
    if ($n -eq 1) {$false}
    elseif ($n -eq 2) {$true}
    elseif ($n -eq 3) {$true}
    else{
        $m = [Math]::Floor([Math]::Sqrt($n))
        (@(2..$m | where {($_ -lt $n)  -and ($n % $_ -eq 0) }).Count -eq 0)
    }
}
1..15 | foreach{"isPrime $_ : $(isPrime $_)"}
```

<b>Output:</b>

```txt
isPrime 1 : False
isPrime 2 : True
isPrime 3 : True
isPrime 4 : False
isPrime 5 : True
isPrime 6 : False
isPrime 7 : True
isPrime 8 : False
isPrime 9 : False
isPrime 10 : False
isPrime 11 : True
isPrime 12 : False
isPrime 13 : True
isPrime 14 : False
isPrime 15 : False
```



## Prolog

The following predicate showcases Prolog's support for writing predicates suitable for both testing and generating. In this case, assuming the Prolog implemenation supports indefinitely large integers, prime(N) can be used to generate primes until memory is exhausted.

```Prolog
prime(2).
prime(N) :-
  between(3, inf, N),
  1 is N mod 2,             % odd
  M is floor(sqrt(N+1)),    % round-off paranoia
  Max is (M-1) // 2,        % integer division
  forall( between(1, Max, I), N mod (2*I+1) > 0 ).
```

Example using SWI-Prolog:

```txt
?- time( (bagof( P, (prime(P), ((P > 100000, !, fail); true)), Bag),
       length(Bag,N),
       last(Bag,Last),
       writeln( (N,Last) ) )).

% 1,724,404 inferences, 1.072 CPU in 1.151 seconds (93% CPU, 1607873 Lips)
Bag = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 9592,
Last = 99991.

?-  time( prime(99991) ).
% 165 inferences, 0.000 CPU in 0.000 seconds (92% CPU, 1213235 Lips)
true.

?-
```



## PureBasic


```PureBasic
Procedure.i IsPrime(n)
   Protected k

   If n = 2
      ProcedureReturn #True
   EndIf

   If n <= 1 Or n % 2 = 0
      ProcedureReturn #False
   EndIf

   For k = 3 To Int(Sqr(n)) Step 2
      If n % k = 0
         ProcedureReturn #False
      EndIf
   Next

   ProcedureReturn #True
EndProcedure
```



## Python

The simplest primality test, using trial division:
```python
def prime(a):
    return not (a < 2 or any(a % x == 0 for x in xrange(2, int(a**0.5) + 1)))
```

Another test. Exclude even numbers first:

```python
def prime2(a):
    if a == 2: return True
    if a < 2 or a % 2 == 0: return False
    return not any(a % x == 0 for x in xrange(3, int(a**0.5) + 1, 2))
```

Yet another test. Exclude multiples of 2 and 3, see http://www.devx.com/vb2themax/Tip/19051:
```python
def prime3(a):
    if a < 2: return False
    if a == 2 or a == 3: return True # manually test 2 and 3
    if a % 2 == 0 or a % 3 == 0: return False # exclude multiples of 2 and 3

    maxDivisor = a**0.5
    d, i = 5, 2
    while d <= maxDivisor:
        if a % d == 0: return False
        d += i
        i = 6 - i # this modifies 2 into 4 and viceversa

    return True
```


### By Regular Expression

Regular expression by "Abigail".

(An explanation is given in "[http://paddy3118.blogspot.com/2009/08/story-of-regexp-and-primes.html The Story of the Regexp and the Primes]").

```python>>>
 import re
>>> def isprime(n):
    return not re.match(r'^1?$|^(11+?)\1+$', '1' * n)

>>> # A quick test
>>> [i for i in range(40) if isprime(i)]
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
```



## Qi


```Qi
(define prime?-0
  K N -> true where (> (* K K) N)
  K N -> false where (= 0 (MOD N K))
  K N -> (prime?-0 (+ K 2) N))

(define prime?
  1 -> false
  2 -> true
  N -> false where (= 0 (MOD N 2))
  N -> (prime?-0 3 N))
```



## R


```R
isPrime <- function(n) {
  if (n == 2) return(TRUE)
  if ( (n <= 1) || ( n %% 2 == 0 ) ) return(FALSE)
  for( i in 3:sqrt(n) ) {
    if ( n %% i == 0 ) return(FALSE)
  }
  TRUE
}
```



```R
print(lapply(1:100, isPrime))
```



## Racket


```Racket
#lang racket

(define (prime? number)
  (cond ((not (positive? number)) #f)
        ((= 1 number) #f)
        ((even? number) (= 2 number))
        (else (for/and ((i (in-range 3 (ceiling (sqrt number)))))
                (not (zero? (remainder number i)))))))
```



## REBOL


```REBOL
prime?: func [n] [
    case [
        n = 2 [ true  ]
        n <= 1 or (n // 2 = 0) [ false ]
        true [
            for i 3 round square-root n 2 [
                if n // i = 0 [ return false ]
            ]
            true
        ]
    ]
]
```



```REBOL
repeat i 100 [ print [i prime? i]]
```



## REXX


### compact version

This version uses a technique which increments by six for testing primality   (up to the   √{{overline| n }}).

Programming note:   all the REXX programs below show all primes up and including the number specified.
:::   If the number is negative, the absolute value of it is used for the upper limit, but no primes are shown.
:::   The   ''number''   of primes found is always shown.

Also, it was determined that computing the (integer) square root of the number to be tested in the   '''isPrime'''

function slowed up the function   (for all three REXX versions),   however, for larger numbers of   '''N''',   it would

be faster.

```rexx
/*REXX program tests for  primality by using  (kinda smartish)  trial division.         */
parse arg n .;  if n==''  then n=10000           /*let the user choose the upper limit. */
tell=(n>0);     n=abs(n)                         /*display the primes  only if   N > 0. */
p=0                                              /*a count of the primes found (so far).*/
      do j=-57  to n                             /*start in the cellar and work up.     */
      if \isPrime(j)  then iterate               /*if not prime,  then keep looking.    */
      p=p+1                                      /*bump the jelly bean counter.         */
      if tell  then say right(j,20) 'is prime.'  /*maybe display prime to the terminal. */
      end   /*j*/
say
say  "There are "      p       " primes up to "        n        ' (inclusive).'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure;    parse arg x                       /*get the number to be tested. */
         if wordpos(x, '2 3 5 7')\==0  then return 1     /*is number a teacher's pet?   */
         if x<2 | x//2==0 | x//3==0    then return 0     /*weed out the riff-raff.      */
            do k=5  by  6  until k*k>x                   /*skips odd multiples of  3.   */
            if x//k==0 | x//(k+2)==0   then return 0     /*a pair of divides.      ___  */
            end   /*k*/                                  /*divide up through the  √ x   */
                                                         /*Note:  //   is  ÷  remainder.*/
         return 1                                        /*done dividing, it's prime.   */
```

```txt

                   2 is prime.
                   3 is prime.
                   5 is prime.
                   7 is prime.
                  11 is prime.
                  13 is prime.
                  17 is prime.
                  19 is prime.
                  23 is prime.
                  29 is prime.
                  31 is prime.
                  37 is prime.
                  41 is prime.
                  43 is prime.
                  47 is prime.
                  53 is prime.
                  59 is prime.
                  61 is prime.
                  67 is prime.
                  71 is prime.
                  73 is prime.
                  79 is prime.
                  83 is prime.
                  89 is prime.
                  97 is prime.

There are  25  primes up to  100 (inclusive).
```



### optimized version

This version separates multiple-clause   '''if'''   statements, and also tests for low primes,

making it about 8% faster.

```rexx
/*REXX program tests for  primality by using  (kinda smartish)  trial division.         */
parse arg n .;  if n==''  then n=10000           /*let the user choose the upper limit. */
tell=(n>0);     n=abs(n)                         /*display the primes  only if   N > 0. */
p=0                                              /*a count of the primes found (so far).*/
      do j=-57  to n                             /*start in the cellar and work up.     */
      if \isPrime(j)  then iterate               /*if not prime,  then keep looking.    */
      p=p+1                                      /*bump the jelly bean counter.         */
      if tell  then say right(j,20) 'is prime.'  /*maybe display prime to the terminal. */
      end   /*j*/
say
say  "There are "      p       " primes up to "        n        ' (inclusive).'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure;  parse arg x                       /*get integer to be investigated.*/
         if x<11     then return wordpos(x, '2 3 5 7')\==0         /*is it a wee prime? */
         if x//2==0  then return 0                     /*eliminate all the even numbers.*/
         if x//3==0  then return 0                     /* ··· and eliminate the triples.*/
                  do k=5  by 6  until k*k>x            /*this skips odd multiples of 3. */
                  if x//k    ==0  then return 0        /*perform a divide (modulus),    */
                  if x//(k+2)==0  then return 0        /* ··· and the next umpty one.   */
                  end   /*k*/                          /*Note: REXX  //  is ÷ remainder.*/
         return 1                                      /*did all divisions, it's prime. */
```

### unrolled version

This version uses an ''unrolled'' version (of the 2<sup>nd</sup> version) of some multiple-clause   '''if'''   statements, and

also an optimized version of the testing of low primes is used, making it about 22% faster.


Note that the   '''do ... until ...'''   was changed to   '''do ... while ...'''.

```rexx
/*REXX program tests for  primality by using  (kinda smartish)  trial division.         */
parse arg n .;  if n==''  then n=10000           /*let the user choose the upper limit. */
tell=(n>0);     n=abs(n)                         /*display the primes  only if   N > 0. */
p=0                                              /*a count of the primes found (so far).*/
      do j=-57  to n                             /*start in the cellar and work up.     */
      if \isPrime(j)  then iterate               /*if not prime,  then keep looking.    */
      p=p+1                                      /*bump the jelly bean counter.         */
      if tell  then say right(j,20) 'is prime.'  /*maybe display prime to the terminal. */
      end   /*j*/
say
say  "There are "      p       " primes up to "        n        ' (inclusive).'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure;    parse arg x               /*get the integer to be investigated.  */
         if x<107  then return wordpos(x, '2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53',
                           '59 61 67 71 73 79 83 89 97 101 103')\==0  /*some low primes.*/
         if x// 2 ==0  then return 0             /*eliminate all the even numbers.      */
         if x// 3 ==0  then return 0             /* ··· and eliminate the triples.      */
         parse var  x  ''  -1  _                 /*          obtain the rightmost digit.*/
         if     _ ==5  then return 0             /* ··· and eliminate the nickels.      */
         if x// 7 ==0  then return 0             /* ··· and eliminate the luckies.      */
         if x//11 ==0  then return 0
         if x//13 ==0  then return 0
         if x//17 ==0  then return 0
         if x//19 ==0  then return 0
         if x//23 ==0  then return 0
         if x//29 ==0  then return 0
         if x//31 ==0  then return 0
         if x//37 ==0  then return 0
         if x//41 ==0  then return 0
         if x//43 ==0  then return 0
         if x//47 ==0  then return 0
         if x//53 ==0  then return 0
         if x//59 ==0  then return 0
         if x//61 ==0  then return 0
         if x//67 ==0  then return 0
         if x//71 ==0  then return 0
         if x//73 ==0  then return 0
         if x//79 ==0  then return 0
         if x//83 ==0  then return 0
         if x//89 ==0  then return 0
         if x//97 ==0  then return 0
         if x//101==0  then return 0
         if x//103==0  then return 0             /*Note:  REXX   //   is  ÷  remainder. */
                   do k=107  by 6  while k*k<=x  /*this skips odd multiples of three.   */
                   if x//k    ==0  then return 0 /*perform a divide (modulus),          */
                   if x//(k+2)==0  then return 0 /* ··· and the next also.   ___        */
                   end   /*k*/                   /*divide up through the    √ x         */
         return 1                                /*after all that,  ··· it's a prime.   */
```

## Ring


```ring
give n
flag = isPrime(n)
if flag = 1 see n + " is a prime number"
else see n + " is not a prime number" ok

func isPrime num
     if (num <= 1) return 0 ok
     if (num % 2 = 0 and num != 2) return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1
```



## Ruby


```ruby
def prime(a)
  if a == 2
    true
  elsif a <= 1 || a % 2 == 0
    false
  else
    divisors = (3..Math.sqrt(a)).step(2)
    divisors.none? { |d| a % d == 0 }
  end
end
p (1..50).select{|i| prime(i)}
```


The '''prime''' package in the stdlib for Ruby contains this compact <code>Prime#prime?</code> method:

```ruby
require "prime"
def prime?(value, generator = Prime::Generator23.new)
  return false if value < 2
  for num in generator
    q,r = value.divmod num
    return true if q < num
    return false if r == 0
  end
end
p (1..50).select{|i| prime?(i)}
```


Without any fancy stuff:

```ruby
def primes(limit)
  (enclose = lambda { |primes|
    primes.last.succ.upto(limit) do |trial_pri|
      if primes.none? { |pri| (trial_pri % pri).zero? }
        return enclose.call(primes << trial_pri)
      end
    end
    primes
  }).call([2])
end
p primes(50)
```


 [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]


### By Regular Expression


```ruby
def isprime(n)
  '1'*n !~ /^1?$|^(11+?)\1+$/
end
```



## Run BASIC


```runbasic
' Test and display primes 1 .. 50
for i = 1 TO 50
  if prime(i) <> 0 then print i;" ";
next i

FUNCTION prime(n)
if n < 2         then prime = 0 : goto [exit]
if n = 2         then prime = 1 : goto [exit]
if n mod 2 = 0   then prime = 0 : goto [exit]
prime = 1
for i = 3 to int(n^.5) step 2
 if n mod i = 0 then  prime = 0 : goto [exit]
next i
[exit]
END FUNCTION
```

 2 3 5 7 11 13 17 19 23 25 29 31 37 41 43 47 49


## Rust


```rust
fn is_prime(n: u64) -> bool {
    match n {
        0 | 1 => false,
        2 => true,
        _even if n % 2 == 0 => false,
        _ => {
            let sqrt_limit = (n as f64).sqrt() as u64;
            (3..=sqrt_limit).step_by(2).find(|i| n % i == 0).is_none()
        }
    }
}

fn main() {
    for i in 1..30 {
        if is_prime(i) {
            println!("{} is prime!", i);
        }
    }
}
```

```txt
2 is prime!
3 is prime!
5 is prime!
7 is prime!
11 is prime!
13 is prime!
17 is prime!
19 is prime!
23 is prime!
29 is prime!
```


=={{header|S-lang}}==
<lang S-lang>define is_prime(n)
{
   if (n == 2) return(1);
   if (n <= 1) return(0);
   if ((n & 1) == 0) return(0);

   variable mx = int(sqrt(n)), i;

   _for i (3, mx, 1) {
     if ((n mod i) == 0)
       return(0);
   }
   return(1);
}

define ptest()
{
   variable lst = {};

   _for $1 (1, 64, 1)
     if (is_prime($1))
       list_append(lst, string($1));
   print(strjoin(list_to_array(lst), ", "));
}
ptest();
```

 "2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61"


## SAS


```sas
data primes;
do n=1 to 1000;
  link primep;
  if primep then output;
end;
stop;

primep:
if n < 4 then do;
  primep=n=2 or n=3;
  return;
end;
primep=0;
if mod(n,2)=0 then return;
do k=3 to sqrt(n) by 2;
  if mod(n,k)=0 then return;
end;
primep=1;
return;
keep n;
run;
```



## Scala


### Simple version


```Scala
  def isPrime(n: Int) =
    n > 1 && (Iterator.from(2) takeWhile (d => d * d <= n) forall (n % _ != 0))
```

===Accelerated version [[functional_programming|FP]] and parallel runabled===
// {{Out}}Best seen running in your browser [https://scastie.scala-lang.org/1RLimJrRQUqkXWkUwUxgYg Scastie (remote JVM)].

```Scala
object IsPrimeTrialDivision extends App {
  def isPrime(n: Long) =
    n > 1 && ((n & 1) != 0 || n == 2) && (n % 3 != 0 || n == 3) &&
      (5 to math.sqrt(n).toInt by 6).par.forall(d => n % d != 0 && n % (d + 2) != 0)

  assert(!isPrime(-1))
  assert(!isPrime(1))
  assert(isPrime(2))
  assert(isPrime(100000000003L))
  assert(isPrime(100000000019L))
  assert(isPrime(100000000057L))
  assert(isPrime(100000000063L))
  assert(isPrime(100000000069L))
  assert(isPrime(100000000073L))
  assert(isPrime(100000000091L))
  println("10 Numbers tested. A moment please …\nNumber crunching biggest 63-bit prime …")
  assert(isPrime(9223372036854775783L)) // Biggest 63-bit prime
  println("All done")

}
```

===Accelerated version [[functional_programming|FP]], tail recursion===
Tests 1.3 M numbers against OEIS prime numbers.

```Scala
import scala.annotation.tailrec
import scala.io.Source

object PrimesTestery extends App {
  val rawText = Source.fromURL("https://oeis.org/A000040/a000040.txt")
  val oeisPrimes = rawText.getLines().take(100000).map(_.split(" ")(1)).toVector

  def isPrime(n: Long) = {
    @tailrec
    def inner(d: Int, end: Int): Boolean = {
      if (d > end) true
      else if (n % d != 0 && n % (d + 2) != 0) inner(d + 6, end) else false
    }

    n > 1 && ((n & 1) != 0 || n == 2) &&
      (n % 3 != 0 || n == 3) && inner(5, math.sqrt(n).toInt)
  }

  println(oeisPrimes.size)
  for (i <- 0 to 1299709) assert(isPrime(i) == oeisPrimes.contains(i.toString), s"Wrong $i")

}
```



## Scheme

```scheme
(define (prime? number)
  (define (*prime? divisor)
    (or (> (* divisor divisor) number)
        (and (> (modulo number divisor) 0)
             (*prime? (+ divisor 1)))))
  (and (> number 1)
       (*prime? 2)))
```



```scheme
; twice faster, testing only odd divisors
(define (prime? n)
  (if (< n 4) (> n 1)
      (and (odd? n)
	   (let loop ((k 3))
	     (or (> (* k k) n)
		 (and (positive? (remainder n k))
		      (loop (+ k 2))))))))
```



## Seed7


```seed7
const func boolean: isPrime (in integer: number) is func
  result
    var boolean: prime is FALSE;
  local
    var integer: upTo is 0;
    var integer: testNum is 3;
  begin
    if number = 2 then
      prime := TRUE;
    elsif odd(number) and number > 2 then
      upTo := sqrt(number);
      while number rem testNum <> 0 and testNum <= upTo do
        testNum +:= 2;
      end while;
      prime := testNum > upTo;
    end if;
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/math.htm#is_prime]


## Sidef


```ruby
func is_prime(a) {
  given (a) {
    when (2)                   { true  }
    case (a <= 1 || a.is_even) { false }
    default                    { 3 .. a.isqrt -> any { .divides(a) } -> not }
  }
}
```

Alternative version, excluding multiples of 2 and 3:

```ruby
func is_prime(n) {
    return (n >= 2) if (n < 4)
    return false if (n%%2 || n%%3)
    for k in (5 .. n.isqrt -> by(6)) {
        return false if (n%%k || n%%(k+2))
    }
    return true
}
```



## Smalltalk


```smalltalk
| isPrime |
isPrime := [:n |
    n even ifTrue: [ ^n=2 ]
    ifFalse: [
        3 to: n sqrt do: [:i |
            (n \\ i = 0) ifTrue: [ ^false ]
        ].
        ^true
    ]
]
```



## SNOBOL4


```SNOBOL4
define('isprime(n)i,max') :(isprime_end)
isprime isprime = n
        le(n,1) :s(freturn)
        eq(n,2) :s(return)
        eq(remdr(n,2),0) :s(freturn)
        max = sqrt(n); i = 1
isp1    i = le(i + 2,max) i + 2 :f(return)
        eq(remdr(n,i),0) :s(freturn)f(isp1)
isprime_end
```


### By Patterns

Using the Abigail regex transated to Snobol patterns.

```SNOBOL4
        define('rprime(n)str,pat1,pat2,m1') :(end_rprime)
rprime  str = dupl('1',n); rprime = n
        pat1 = ('1' | '')
        pat2 = ('11' arbno('1')) $ m1 (*m1 arbno(*m1))
        str pos(0) (pat1 | pat2) rpos(0) :s(freturn)f(return)
end_rprime

*       # Test and display primes 0 .. 50
loop    rprimes = rprimes rprime(n)  ' '
        n = lt(n,50) n + 1 :s(loop)
        output = rprimes
end
```

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47


## SQL

```tsql
declare @number int
set @number = 514229 -- number to check

;with cte(number) as
(
 select 2
 union all
 select number+1
 from cte
 where number+1 < @number
)
select
      cast(@number as varchar(100)) +
      case
          when exists
				  (
					select *
					from
					(
						select number, @number % number modNumber
						from cte
					) tmp
					where tmp.modNumber = 0
				  )
				          then ' is composite'
		  else
						 ' is prime'
	  end primalityTest
option (maxrecursion 0)
```



## Standard ML


```sml
fun is_prime n =
  if n = 2 then true
  else if n < 2 orelse n mod 2 = 0 then false
  else let
    fun loop k =
      if k * k > n then true
      else if n mod k = 0 then false
      else loop (k+2)
    in loop 3
  end
```



## Swift


```swift
import Foundation

extension Int {
  func isPrime() -> Bool {

    switch self {
    case let x where x < 2:
      return false
    case 2:
      return true
    default:
      return
        self % 2 != 0 &&
        !stride(from: 3, through: Int(sqrt(Double(self))), by: 2).contains {self % $0 == 0}
    }
  }
}
```



## Tcl


```tcl
proc is_prime n {
    if {$n <= 1} {return false}
    if {$n == 2} {return true}
    if {$n % 2 == 0} {return false}
    for {set i 3} {$i <= sqrt($n)} {incr i 2} {
        if {$n % $i == 0} {return false}
    }
    return true
}
```


=={{header|TI-83 BASIC}}==
 Prompt A
 If A=2:Then
 Disp "PRIME"
 Stop
 End

 If (fPart(A/2)=0 and A>0) or A<2:Then
 Disp "NOT PRIME"
 Stop
 End

 1→P
 For(B,3,int(√(A)))
 If FPart(A/B)=0:Then
 0→P
 √(A)→B
 End
 B+1→B
 End

 If P=1:Then
 Disp "PRIME"
 Else
 Disp "NOT PRIME"
 End


## uBasic/4tH

<lang>10 LET n=0: LET p=0
20 INPUT "Enter number: ";n
30 LET p=0 : IF n>1 THEN GOSUB 1000
40 IF p=0 THEN PRINT n;" is not prime!"
50 IF p#0 THEN PRINT n;" is prime!"
60 GOTO 10
1000 REM ***************
1001 REM * PRIME CHECK *
1002 REM ***************
1010 LET p=0
1020 IF (n%2)=0 THEN RETURN
1030 LET p=1 : PUSH n,0 : GOSUB 9030
1040 FOR i=3 TO POP() STEP 2
1050 IF (n%i)=0 THEN LET p=0: PUSH n,0 : GOSUB 9030 : LET i=POP()
1060 NEXT i
1070 RETURN
9030 Push ((10^(Pop()*2))*Pop()) : @(255)=Tos()
9040 Push (@(255) + (Tos()/@(255)))/2
     If Abs(@(255)-Tos())<2 Then @(255)=Pop() : If Pop() Then Push @(255) : Return
     @(255) = Pop() : Goto 9040
     REM ** This is an integer SQR subroutine. Output is scaled by 10^(TOS()).
```


## UNIX Shell

```bash
function primep {
	typeset n=$1 p=3
	(( n == 2 )) && return 0	# 2 is prime.
	(( n & 1 )) || return 1		# Other evens are not prime.
	(( n >= 3 )) || return 1

	# Loop for odd p from 3 to sqrt(n).
	# Comparing p * p <= n can overflow.
	while (( p <= n / p )); do
		# If p divides n, then n is not prime.
		(( n % p )) || return 1
		(( p += 2 ))
	done
	return 0	# Yes, n is prime.
}
```

```bash
primep() {
	set -- "$1" 3
	test "$1" -eq 2 && return 0		# 2 is prime.
	expr "$1" \% 2 >/dev/null || return 1	# Other evens are not prime.
	test "$1" -ge 3 || return 1

	# Loop for odd p from 3 to sqrt(n).
	# Comparing p * p <= n can overflow. We use p <= n / p.
	while expr $2 \<= "$1" / $2 >/dev/null; do
		# If p divides n, then n is not prime.
		expr "$1" % $2 >/dev/null || return 1
		set -- "$1" `expr $2 + 2`
	done
	return 0	# Yes, n is prime.
}
```



## Ursala

Excludes even numbers, and loops only up to the approximate square root or until a factor is found.

```Ursala
#import std
#import nat

prime = ~<{0,1}&& -={2,3}!| ~&h&& (all remainder)^Dtt/~& iota@K31
```

Test program to try it on a few numbers:

```Ursala
#cast %bL

test = prime* <5,6,7>
```

 <true,false,true>


## V

```v
[prime?
     2
     [[dup * >] [true] [false] ifte [% 0 >] dip and]
       [succ]
     while
     dup * <].
```

```v
|11 prime?
=true
|4 prime?
=false
```



## VBA


```vb
Option Explicit

Sub FirstTwentyPrimes()
Dim count As Integer, i As Long, t(19) As String
   Do
      i = i + 1
      If IsPrime(i) Then
         t(count) = i
         count = count + 1
      End If
   Loop While count <= UBound(t)
   Debug.Print Join(t, ", ")
End Sub

Function IsPrime(Nb As Long) As Boolean
   If Nb = 2 Then
      IsPrime = True
   ElseIf Nb < 2 Or Nb Mod 2 = 0 Then
      Exit Function
   Else
      Dim i As Long
      For i = 3 To Sqr(Nb) Step 2
         If Nb Mod i = 0 Then Exit Function
      Next
      IsPrime = True
   End If
End Function
```

 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71


## VBScript

```vb
Function IsPrime(n)
	If n = 2 Then
		IsPrime = True
	ElseIf n <= 1 Or n Mod 2 = 0 Then
		IsPrime = False
	Else
		IsPrime = True
		For i = 3 To Int(Sqr(n)) Step 2
			If n Mod i = 0 Then
				IsPrime = False
				Exit For
			End If
		Next
	End If
End Function

For n = 1 To 50
	If IsPrime(n) Then
		WScript.StdOut.Write n & " "
	End If
Next
```

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47


## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func Prime(N);          \Return 'true' if N is a prime number
int  N;
int  I;
[if N <= 1 then return false;
for I:= 3 to sqrt(N) do
        if rem(N/I) = 0 then return false;
return true;
]; \Prime

int  Num;
repeat  Num:= IntIn(0);
        Text(0, if Prime(Num) then "is " else "not ");
        Text(0, "prime^M^J");
until   Num = 0
```

```txt
777777777
not prime
777777773
is prime
0
not prime
```



## zkl

The Method filter1 stops at the first non False result, which, if there is one, is the first found diviser, thus short cutting the rest of the test

```zkl
fcn isPrime(n){
   if(n.isEven or n<2) return(n==2);
   (not [3..n.toFloat().sqrt().toInt(),2].filter1('wrap(m){n%m==0}))
}
```

```txt
zkl: [1..].filter(20,isPrime)
L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71)
zkl: isPrime(777777773)
True
zkl: isPrime(777777777)
False
```

