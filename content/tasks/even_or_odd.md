+++
title = "Even or odd"
description = ""
date = 2019-10-15T09:53:28Z
aliases = []
[extra]
id = 10866
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Test whether an integer is even or odd.

There is more than one way to solve this task:

* Use the even and odd predicates, if the language provides them.
* Check the least significant digit. With binary integers, ''i [[bitwise operations|bitwise-and]] 1'' equals 0 [[wikt:iff|iff]] ''i'' is even, or equals 1 iff ''i'' is odd.
* Divide ''i'' by 2. The remainder equals 0 iff ''i'' is even. The remainder equals +1 or -1 iff ''i'' is odd.
* Use modular congruences:
** ''i'' &equiv; 0 (mod 2) iff ''i'' is even.
** ''i'' &equiv; 1 (mod 2) iff ''i'' is odd.





## 0815


```0815

}:s:|=<:2:x~#:e:=/~%~<:20:~$=<:73:x<:69:~$~$~<:20:~$=^:o:<:65:
x<:76:=$=$~$<:6E:~$<:a:~$^:s:}:o:<:6F:x<:64:x~$~$$<:a:~$^:s:

```



## 11l


```11l
F is_even(i)
   R i % 2 == 0

F is_odd(i)
   R i % 2 == 1
```



## 6502 Assembly


```6502 assembly

        .lf  evenodd6502.lst
        .cr  6502
        .tf  evenodd6502.obj,ap1
;------------------------------------------------------
; Even or Odd for the 6502 by barrym95838 2014.12.10
; Thanks to sbprojects.com for a very nice assembler!
; The target for this assembly is an Apple II with
;   mixed-case output capabilities.  Apple IIs like to
;   work in '+128' ascii, and this version is tailored
;   to that preference.
; Tested and verified on AppleWin 1.20.0.0
;------------------------------------------------------
; Constant Section
;
CharIn   =   $fd0c      ;Specific to the Apple II
CharOut  =   $fded      ;Specific to the Apple II
;------------------------------------------------------
; The main program
;
main    ldy  #sIntro-sbase
        jsr  puts       ;Print Intro
loop    jsr  CharIn     ;Get a char from stdin
        cmp  #$83       ;Ctrl-C?
        beq  done       ;  yes:  end program
        jsr  CharOut    ;Echo char
        ldy  #sOdd-sbase ;Pre-load odd string
        lsr             ;LSB of char to carry flag
        bcs  isodd
        ldy  #sEven-sbase
isodd   jsr  puts       ;Print appropriate response
        beq  loop       ;Always taken
; Output NUL-terminated string @ offset Y
;
puts    lda  sbase,y    ;Get string char
        beq  done       ;Done if NUL
        jsr  CharOut    ;Output the char
        iny             ;Point to next char
        bne  puts       ;Loop up to 255 times
done    rts             ;Return to caller
;------------------------------------------------------
; String Constants (in '+128' ascii, Apple II style)
;
sbase:                  ;String base address
sIntro  .az     -"Hit any key (Ctrl-C to quit):",-#13
sEven   .az     -" is even.",-#13
sOdd    .az     -" is odd.",-#13
;------------------------------------------------------
        .en

```



## 8th

The 'mod' method also works, but the bit method is fastest.

```forth
: odd? \ n -- boolean
    dup 1 n:band 1 n:= ;
: even? \ n -- boolean
    odd? not ;
```


This could be shortened to:

```forth

: even? \ n -- f
  1 n:band not ;
: odd? \ n -- f
  even? not ;

```



## ABAP


```ABAP

cl_demo_output=>display(
  VALUE string_table(
    FOR i = -5 WHILE i < 6 (
      COND string(
        LET r = i MOD 2 IN
        WHEN r = 0 THEN |{ i } is even|
        ELSE |{ i } is odd|
      )
    )
  )
).

```


```txt

Table
-5 is odd
-4 is even
-3 is odd
-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd
4 is even
5 is odd

```



## Ada


```ada
-- Ada has bitwise operators in package Interfaces,
-- but they work with Interfaces.Unsigned_*** types only.
-- Use rem or mod for Integer types, and let the compiler
-- optimize it.
declare
   N : Integer := 5;
begin
   if N rem 2 = 0 then
      Put_Line ("Even number");
   elseif N rem 2 /= 0 then
      Put_Line ("Odd number");
   else
      Put_Line ("Something went really wrong!");
   end if;
end;
```



## Agda


```Agda
even : ℕ → Bool
odd  : ℕ → Bool

even zero    = true
even (suc n) = odd n

odd zero    = false
odd (suc n) = even n
```



## Aime


```aime
if (x & 1) {
    # x is odd
} else {
    # x is even
}
```



## ALGOL 68

```algol68
# Algol 68 has a standard operator: ODD which returns TRUE if its integer  #
# operand is odd and FALSE if it is even                                   #
# E.g.:                                                                    #

INT n;
print( ( "Enter an integer: " ) );
read( ( n ) );
print( ( whole( n, 0 ), " is ", IF ODD n THEN "odd" ELSE "even" FI, newline ) )

```



## ALGOL W


```algolw
begin
    % the Algol W standard procedure odd returns true if its integer  %
    % parameter is odd, false if it is even                           %
    for i := 1, 1702, 23, -26
    do begin
        write( i, " is ", if odd( i ) then "odd" else "even" )
    end for_i
end.
```

```txt

             1   is odd
          1702   is even
            23   is odd
           -26   is even

```



## AntLang


```AntLang
odd: {x mod 2}
even: {1 - x mod 2}
```



## APL

The easiest way is probably to use modulo.

```apl
      2|28
0
      2|37
1
```



## AppleScript


```AppleScript
set nList to {3, 2, 1, 0, -1, -2, -3}
repeat with n in nList
    if (n / 2) = n / 2 as integer then
        log "Value " & n & " is even."
    else
        log "Value " & n & " is odd."
    end if
end repeat
```

```txt
(*Value 3 is odd.*)
(*Value 2 is even.*)
(*Value 1 is odd.*)
(*Value 0 is even.*)
(*Value -1 is odd.*)
(*Value -2 is even.*)
(*Value -3 is odd.*)
```



Or, packaging reusable functions that can serve as arguments to '''filter''' etc (deriving '''even''' from mod, and '''odd''' from even):

```AppleScript
-- even :: Integral a => a -> Bool
on even(n)
    n mod 2 = 0
end even

-- odd :: Integral a => a -> Bool
on odd(n)
    not even(n)
end odd


-- GENERIC FUNCTIONS FOR TEST ----------------------------------

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if lambda(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property lambda : f
        end script
    end if
end mReturn


-- TEST ---------------------------------------------------------
on run
    set xs to [-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6]

    {filter(even, xs), filter(odd, xs)}
end run
```


```AppleScript
```



## Arendelle



```txt
( input , "Please enter a number: " )

{ @input % 2 = 0 ,

	"| @input | is even!"
,
	"| @input | is odd!"
}
```



## Arturo



```arturo
loop $(range 0-5 5) {
	if $(even &) {
		print $(padLeft $(toString &) 4) + ": even"
	} {
		print $(padLeft $(toString &) 4) + ": odd"
	}
}
```


```txt
-5  : odd
-4  : even
-3  : odd
-2  : even
-1  : odd
0   : even
1   : odd
2   : even
3   : odd
4   : even
5   : odd
```



## AutoHotkey

Bitwise ops are probably most efficient:

```AHK
if ( int & 1 ){
	; do odd stuff
}else{
	; do even stuff
}
```




## AWK


```AWK
function isodd(x) {
	return (x%2)!=0;
}

function iseven(x) {
	return (x%2)==0;
}
```



## BaCon


```freebasic
' Even or odd
OPTION MEMTYPE int
SPLIT ARGUMENT$ BY " " TO arg$ SIZE dim
n = IIF$(dim < 2, 0, VAL(arg$[1]))
PRINT n, " is ", IIF$(EVEN(n), "even", "odd")
```


```txt
prompt$ ./even-or-odd 42
42 is even
prompt$ ./even-or-odd 41
41 is odd
```



## BASIC

=
## Applesoft BASIC
=


```basic
10 INPUT "ENTER A NUMBER: ";N
20 IF N/2 <> INT(N/2) THEN PRINT "THE NUMBER IS ODD":GOTO 40
30 PRINT "THE NUMBER IS EVEN"
40 END
```

=
## Commodore BASIC
=

Uses bitwise AND as suggested.


```gwbasic
10 rem determine if integer is even or odd
20 print "Enter an integer:";
30 input i%
35 print
40 eo$="even"
50 if (i% and 1)=1 then eo$="odd"
60 print "The number ";i%;"is ";eo$;"."
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DEF ODD(X)=MOD(X,2)
110 INPUT PROMPT "Enter a number: ":X
120 IF ODD(X) THEN
130   PRINT X;"is odd."
140 ELSE
150   PRINT X;"is even."
160 END IF
```



## Batch File


```dos

@echo off
set /p i=Insert number:

::bitwise and
set /a "test1=%i%&1"

::divide last character by 2
set /a test2=%i:~-1%/2

::modulo
set /a test3=%i% %% 2

set test
pause>nul

```



## BBC BASIC

Solutions using AND or MOD are restricted to 32-bit integers, so an alternative solution is given which works with a larger range of values.

```bbcbasic
      IF FNisodd%(14) PRINT "14 is odd" ELSE PRINT "14 is even"
      IF FNisodd%(15) PRINT "15 is odd" ELSE PRINT "15 is even"
      IF FNisodd#(9876543210#) PRINT "9876543210 is odd" ELSE PRINT "9876543210 is even"
      IF FNisodd#(9876543211#) PRINT "9876543211 is odd" ELSE PRINT "9876543211 is even"
      END

      REM Works for -2^31 <= n% < 2^31
      DEF FNisodd%(n%) = (n% AND 1) <> 0

      REM Works for -2^53 <= n# <= 2^53
      DEF FNisodd#(n#) = n# <> 2 * INT(n# / 2)
```

```txt

14 is even
15 is odd
9876543210 is even
9876543211 is odd

```



## bc

There are no bitwise operations, so this solution compares a remainder with zero. Calculation of ''i % 2'' only works when ''scale = 0''.

```bc
i = -3

/* Assumes that i is an integer. */
scale = 0
if (i % 2 == 0) "i is even
"
if (i % 2) "i is odd
"
```



## Befunge


```befunge
&2%52**"E"+,@
```


Outputs E if even, O if odd.


## Bracmat

Not the simplest solution, but the cheapest if the number that must be tested has thousands of digits.

```bracmat
( ( even
  =
    . @( !arg
       :   ?
           [-2
           ( 0
           | 2
           | 4
           | 6
           | 8
           )
       )
  )
& (odd=.~(even$!arg))
& ( eventest
  =
    .   out
      $ (!arg is (even$!arg&|not) even)
  )
& ( oddtest
  =
    .   out
      $ (!arg is (odd$!arg&|not) odd)
  )
& eventest$5556
& oddtest$5556
& eventest$857234098750432987502398457089435
& oddtest$857234098750432987502398457089435
)
```

```txt
5556 is even
5556 is not odd
857234098750432987502398457089435 is not even
857234098750432987502398457089435 is odd
```


=={{header|Brainfuck}}==
Assumes that input characters are an ASCII representation of a valid integer.
Output is input <tt>mod</tt> 2.

```bf
,[>,----------] Read until newline
++<             Get a 2 and move into position
[->-[>+>>]>     Do
[+[-<+>]>+>>]   divmod
<<<<<]          magic
>[-]<++++++++   Clear and get an 8
[>++++++<-]     to get a 48
>[>+<-]>.       to get n % 2 to ASCII and print
```


If one need only determine rather than act on the parity of the input,
the following is sufficient; it terminates either quickly or never.

```bf
,[>,----------]<[--]
```



## Burlesque


```burlesque
2.%
```



## C

Test by bitwise and'ing 1, works for any builtin integer type as long as it's 2's complement (it's always so nowadays):

```c
if (x & 1) {
    /* x is odd */
} else {
    /* or not */
}
```

If using long integer type from GMP (<code>mpz_t</code>), there are provided macros:

```c
mpz_t x;
...
if (mpz_even_p(x)) { /* x is even */ }
if (mpz_odd_p(x))  { /* x is odd */ }
```

The macros evaluate <code>x</code> more than once, so it should not be something with side effects.

## C#

```c#
namespace RosettaCode
{
    using System;

    public static class EvenOrOdd
    {
        public static bool IsEvenBitwise(this int number)
        {
            return (number & 1) == 0;
        }

        public static bool IsOddBitwise(this int number)
        {
            return (number & 1) != 0;
        }

        public static bool IsEvenRemainder(this int number)
        {
            int remainder;
            Math.DivRem(number, 2, out remainder);
            return remainder == 0;
        }

        public static bool IsOddRemainder(this int number)
        {
            int remainder;
            Math.DivRem(number, 2, out remainder);
            return remainder != 0;
        }

        public static bool IsEvenModulo(this int number)
        {
            return (number % 2) == 0;
        }

        public static bool IsOddModulo(this int number)
        {
            return (number % 2) != 0;
        }
    }
}
```



## C++

Test using the modulo operator, or use the C example from above.

```cpp
bool isOdd(int x)
{
    return x % 2;
}

bool isEven(int x)
{
    return !(x % 2);
}
```


A slightly more type-generic version, for C++11 and later.  This should theoretically work for any type convertible to <code>int</code>:


```cpp

template < typename T >
constexpr inline bool isEven( const T& v )
{
    return isEven( int( v ) );
}

template <>
constexpr inline bool isEven< int >( const int& v )
{
    return (v & 1) == 0;
}

template < typename T >
constexpr inline bool isOdd( const T& v )
{
    return !isEven(v);
}

```



## Clojure

Standard predicates:

```clojure
(if (even? some-var) (do-even-stuff))
(if (odd? some-var) (do-odd-stuff))
```



## COBOL


```cobol
       IF FUNCTION REM(Num, 2) = 0
           DISPLAY Num " is even."
       ELSE
           DISPLAY Num " is odd."
       END-IF
```



## CoffeeScript


```coffeescript
isEven = (x) -> !(x%2)
```



## ColdFusion


```cfm

function f(numeric n) {
   return n mod 2?"odd":"even"
}

```



## Common Lisp

Standard predicates:

```lisp
(if (evenp some-var) (do-even-stuff))
(if (oddp some-other-var) (do-odd-stuff))
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Even or odd

(defun evenodd (nr)
          (cond ((evenp nr) "even")
          ((oddp nr) "odd")))
(dotimes (n 10)
(if (< n 1) (terpri))
(if (< n 9) (format t "~a" " "))
(write(+ n 1)) (format t "~a" ": ")
(format t "~a" (evenodd (+ n 1))) (terpri))

```

Output:

```txt

1: odd
 2: even
 3: odd
 4: even
 5: odd
 6: even
 7: odd
 8: even
 9: odd
10: even

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE EvenOdd;
IMPORT StdLog,Args,Strings;

PROCEDURE BitwiseOdd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN 0 IN BITS(i)
END BitwiseOdd;

PROCEDURE Odd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN (i MOD 2) # 0
END Odd;

PROCEDURE CongruenceOdd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN ((i -1) MOD 2) = 0
END CongruenceOdd;

PROCEDURE Do*;
VAR
	p: Args.Params;
	i,done,x: INTEGER;
BEGIN
	Args.Get(p);
	StdLog.String("Builtin function: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF ODD(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Bitwise: ");StdLog.Ln;i:= 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF BitwiseOdd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Module: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF Odd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Congruences: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF CongruenceOdd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
END Do;

```

Execute: ^Q EvenOdd.Do 10 11 0 57 34 -23 -42~<br/>
```txt

Builtin function:
10  is:> even
11  is:> odd
0  is:> even
57  is:> odd
34  is:> even
-23  is:> odd
-42 is:> even
Bitwise:
10  is:> even
11  is:> odd
0  is:> even
57  is:> odd
34  is:> even
-23  is:> odd
-42 is:> even
Module:
10  is:> even
11  is:> odd
0  is:> even
57  is:> odd
34  is:> even
-23  is:> odd
-42 is:> even
Congruences:
10  is:> even
11  is:> odd
0  is:> even
57  is:> odd
34  is:> even
-23  is:> odd
-42 is:> even

```



## Crystal


```crystal
#Using bitwise shift
  def isEven_bShift(n)
    n == ((n >> 1) << 1)
  end
  def isOdd_bShift(n)
    n != ((n >> 1) << 1)
  end
#Using modulo operator
  def isEven_mod(n)
    (n % 2) == 0
  end
  def isOdd_mod(n)
    (n % 2) != 0
  end
# Using bitwise "and"
  def isEven_bAnd(n)
    (n & 1) ==  0
  end
  def isOdd_bAnd(n)
    (n & 1) != 0
  end

puts isEven_bShift(7)
puts isOdd_bShift(7)

puts isEven_mod(12)
puts isOdd_mod(12)

puts isEven_bAnd(21)
puts isOdd_bAnd(21)

```

```txt
false
true
true
false
false
true

```



## D


```d
void main() {
    import std.stdio, std.bigint;

    foreach (immutable i; -5 .. 6)
        writeln(i, " ", i & 1, " ", i % 2, " ", i.BigInt % 2);
}
```

```txt
-5 1 -1 -1
-4 0 0 0
-3 1 -1 -1
-2 0 0 0
-1 1 -1 -1
0 0 0 0
1 1 1 1
2 0 0 0
3 1 1 1
4 0 0 0
5 1 1 1
```



## DCL


```DCL
$! in DCL, for integers, the least significant bit determines the logical value, where 1 is true and 0 is false
$
$ i = -5
$ loop1:
$  if i then $ write sys$output i, " is odd"
$  if .not. i then $ write sys$output i, " is even"
$  i = i + 1
$  if i .le. 6 then $ goto loop1
```

```txt
$ @even_odd
-5 is odd
-4 is even
-3 is odd
-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd
4 is even
5 is odd
6 is even
```



## DWScript

Predicate:

```delphi
var isOdd := Odd(i);
```

Bitwise and:

```delphi
var isOdd := (i and 1)<>0;
```

Modulo:

```delphi
var isOdd := (i mod 2)=1;
```


=={{header|Déjà Vu}}==

```dejavu
even n:
    = 0 % n 2

odd:
    not even

!. odd 0
!. even 0
!. odd 7
!. even 7

```

```txt
false
true
true
false
```



## EDSAC order code

This implementation uses the <code>C</code> (logical AND multiplier register with memory) order. It will cause the machine to print an <tt>E</tt> if the number stored at address <i>θ</i>+15 is even, or an <tt>O</tt> if it is odd. As an example, we shall test the number 37 (<code>P18D</code> in EDSAC encoding).

```edsac
[ Even or odd

### =====


  A program for the EDSAC

  Determines whether the number stored at
  address 15@ is even or odd, and prints
  'E' or 'O' accordingly

  Works with Initial Orders 2 ]

       T56K   [ load point ]
       GK     [ base address ]

       O11@   [ print letter shift ]
       T10@   [ clear accumulator ]
       H15@   [ multiplier := n ]
       C12@   [ acc +:= mult AND 1 ]
       S12@   [ acc -:= 1 ]
       G8@    [ branch on negative ]
       O14@   [ print 'O' ]
       ZF     [ halt ]
[ 8 ]  O13@   [ print 'E' ]
       ZF     [ halt ]

[ 10 ] P0F    [ used to clear acc ]
[ 11 ] *F     [ letter shift character ]
[ 12 ] P0D    [ const: 1 ]
[ 13 ] EF     [ character 'E' ]
[ 14 ] OF     [ character 'O' ]
[ 15 ] P18D   [ number to test: 37 ]

       EZPF   [ branch to load point ]
```

```txt
O
```



## Eiffel


```Eiffel
--bit testing
if i.bit_and (1) = 0 then
	-- i is even
end

--built-in bit testing (uses bit_and)
if i.bit_test (0) then
	-- i is odd
end

--integer remainder (modulo)
if i \\ 2 = 0 then
	-- i is even
end
```



## Elixir


```elixir
defmodule RC do
  import Integer

  def even_or_odd(n) when is_even(n), do: "#{n} is even"
  def even_or_odd(n)                , do: "#{n} is odd"
      # In second "def", the guard clauses of "is_odd(n)" is unnecessary.

  # Another definition way
  def even_or_odd2(n) do
    if is_even(n), do: "#{n} is even", else: "#{n} is odd"
  end
end

Enum.each(-2..3, fn n -> IO.puts RC.even_or_odd(n) end)
```


```txt

-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd

```

Other ways to test even-ness:

```elixir
rem(n,2) == 0
```



## Emacs Lisp


### With evenp and oddp


```Emacs Lisp

(defun odd (n)
  (if (oddp n) (format "%d is odd\n" n)
    (format "%d is even\n" n)))

(defun even (n)
  (if (evenp n) (format "%d is even\n" n)
    (format "%d is odd\n" n)))

(progn
  (insert (even 3) )
  (insert (odd 2) )))

```



### With mod


```Emacs Lisp

(defun odd (n)
  (if (= 1 (mod n 2) ) (format "%d is odd\n" n)
    (format "%d is even\n" n)))

(defun even (n)
  (if (= 0 (mod n 2) ) (format "%d is even\n" n)
    (format "%d is odd\n" n)))

(progn
  (insert (even 3) )
  (insert (odd 2) ))

```

<b>Output:</b>

```txt

3 is odd
2 is even

```



## Erlang


### Using Division by 2 Method


```erlang
%% Implemented by Arjun Sunel
-module(even_odd).
-export([main/0]).

main()->
	test(8).

test(N) ->
	if (N rem 2)==1 ->
		io:format("odd\n");
	true ->
		io:format("even\n")
	end.

```

===Using the least-significant bit method===

```erlang
 %% Implemented by Arjun Sunel
-module(even_odd2).
-export([main/0]).

main()->
	test(10).

test(N) ->
	if (N band 1)==1 ->
		io:format("odd\n");
	true ->
		io:format("even\n")
	end.

```



## ERRE


```ERRE
PROGRAM ODD_EVEN

! works for -2^15 <= n% < 2^15

FUNCTION ISODD%(N%)
      ISODD%=(N% AND 1)<>0
END FUNCTION

! works for -2^38 <= n# <= 2^38
FUNCTION ISODD#(N#)
      ISODD#=N#<>2*INT(N#/2)
END FUNCTION

BEGIN
  IF ISODD%(14) THEN PRINT("14 is odd") ELSE PRINT("14 is even") END IF
  IF ISODD%(15) THEN PRINT("15 is odd") ELSE PRINT("15 is even") END IF
  IF ISODD#(9876543210) THEN PRINT("9876543210 is odd") ELSE PRINT("9876543210 is even") END IF
  IF ISODD#(9876543211) THEN PRINT("9876543211 is odd") ELSE PRINT("9876543211 is even") END IF
END PROGRAM

```

```txt

14 is even
15 is odd
9876543210 is even
9876543211 is odd

```



## Euphoria

Using standard function

```Euphoria

include std/math.e

for i = 1 to 10 do
        ? {i, is_even(i)}
end for

```

```txt

{1,0}
{2,1}
{3,0}
{4,1}
{5,0}
{6,1}
{7,0}
{8,1}
{9,0}
{10,1}

```



## Excel

Use the MOD function

```Excel

=MOD(33;2)
=MOD(18;2)

```


```txt

1
0

```


Use the ISEVEN function, returns TRUE or FALSE

```Excel

=ISEVEN(33)
=ISEVEN(18)

```


```txt

FALSE
TRUE

```


Use the ISODD function, returns TRUE or FALSE

```Excel

=ISODD(33)
=ISODD(18)

```


```txt

TRUE
FALSE

```


=={{header|F Sharp|F#}}==

Bitwise and:

```fsharp
let isEven x =
  x &&& 1 = 0
```


Modulo:

```fsharp
let isEven x =
  x % 2 = 0
```



## Factor

The ''math'' vocabulary provides ''even?'' and ''odd?'' predicates. This example runs at the listener, which already uses the ''math'' vocabulary.

 ( scratchpad ) '''20 even? .'''
 t
 ( scratchpad ) '''35 even? .'''
 f
 ( scratchpad ) '''20 odd? .'''
 f
 ( scratchpad ) '''35 odd? .'''
 t


## Fish

This example assumes that the input command ''i'' returns an integer when one was inputted and that the user inputs a valid positive integer terminated by a newline.

```Fish
<v"Please enter a number:"a
 >l0)?!vo     v          <                        v    o<
^      >i:a=?v>i:a=?v$a*+^>"The number is even."ar>l0=?!^>
             >      >2%0=?^"The number is odd."ar ^
```

The actual computation is the 2%0= part. The rest is either user interface or parsing input.


## Forth


```forth
: odd? ( n -- ? ) 1 and ;
```



## Fortran


Please find the compilation and example run in the comments at the beginning of the FORTRAN 2008 source.  Separating the bit 0 parity module from the main program enables reuse of the even and odd functions.  Even and odd, with scalar and vector interfaces demonstrate the generic function capability of FORTRAN 90.  Threading, stdin, and all-intrinsics are vestigial and have no influence here other than to confuse you.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Tue May 21 20:22:56
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a < unixdict.txt
!gfortran -std=f2008 -Wall -ffree-form -fall-intrinsics f.f08 -o f
! n     odd    even
!-6    F    T
!-5    T    F
!-4    F    T
!-3    T    F
!-2    F    T
!-1    T    F
! 0    F    T
! 1    T    F
! 2    F    T
! 3    T    F
! 4    F    T
! 5    T    F
! 6    F    T
! -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6       n
!  F  T  F  T  F  T  F  T  F  T  F  T  F     odd
!  T  F  T  F  T  F  T  F  T  F  T  F  T    even
!
!Compilation finished at Tue May 21 20:22:56


module bit0parity

  interface odd
    module procedure odd_scalar, odd_list
  end interface

  interface even
    module procedure even_scalar, even_list
  end interface

contains

  logical function odd_scalar(a)
    implicit none
    integer, intent(in) :: a
    odd_scalar = btest(a, 0)
  end function odd_scalar

  logical function even_scalar(a)
    implicit none
    integer, intent(in) :: a
    even_scalar = .not. odd_scalar(a)
  end function even_scalar

  function odd_list(a) result(rv)
    implicit none
    integer, dimension(:), intent(in) :: a
    logical, dimension(size(a)) :: rv
    rv = btest(a, 0)
  end function odd_list

  function even_list(a) result(rv)
    implicit none
    integer, dimension(:), intent(in) :: a
    logical, dimension(size(a)) :: rv
    rv = .not. odd_list(a)
  end function even_list

end module bit0parity

program oe
  use bit0parity
  implicit none
  integer :: i
  integer, dimension(13) :: j
  write(6,'(a2,2a8)') 'n', 'odd', 'even'
  write(6, '(i2,2l5)') (i, odd_scalar(i), even_scalar(i), i=-6,6)
  do i=-6, 6
    j(i+7) = i
  end do
  write(6, '((13i3),a8/(13l3),a8/(13l3),a8)') j, 'n', odd(j), 'odd', even(j), 'even'
end program oe

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim n As Integer

Do
  Print "Enter an integer or 0 to finish : ";
  Input "", n
  If n = 0 Then
    Exit Do
  ElseIf n Mod 2 = 0 Then
    Print "Your number is even"
    Print
  Else
    Print "Your number is odd"
    Print
  End if
Loop

End
```



## Futhark

```Futhark

fun main(x: int): bool = (x & 1) == 0

```



## Gambas


```gambas
Public Sub Form_Open()
Dim sAnswer, sMessage As String

sAnswer = InputBox("Input an integer", "Odd or even")

If IsInteger(sAnswer) Then
  If Odd(Val(sAnswer)) Then sMessage = "' is an odd number"
  If Even(Val(sAnswer)) Then sMessage = "' is an even number"
Else
  sMessage = "' does not compute!!"
Endif

Print "'" & sAnswer & sMessage

End
```


Output:

```txt

'25' is an odd number
'100' is an even number
'Fred' does not compute!!

```



## GAP


```gap
IsEvenInt(n);
IsOddInt(n);
```



## Genie

Using bitwise AND of the zero-bit.

```genie
[indent = 4]
/*
   Even or odd, in Genie
   valac even_or_odd.gs
*/

def parity(n:int):bool
    return ((n & 1) == 0)

def show_parity(n:int):void
    print "%d is %s", n, parity(n) ? "even" : "odd"

init
    show_parity(0)
    show_parity(1)
    show_parity(2)
    show_parity(-2)
    show_parity(-1)
```


```txt
prompt$ valac even_or_odd.gs
prompt$ ./even_or_odd
0 is even
1 is odd
2 is even
-2 is even
-1 is odd
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    test(-2)
    test(-1)
    test(0)
    test(1)
    test(2)
    testBig("-222222222222222222222222222222222222")
    testBig("-1")
    testBig("0")
    testBig("1")
    testBig("222222222222222222222222222222222222")
}

func test(n int) {
    fmt.Printf("Testing integer %3d:  ", n)
    // & 1 is a good way to test
    if n&1 == 0 {
        fmt.Print("even ")
    } else {
        fmt.Print(" odd ")
    }
    // Careful when using %: negative n % 2 returns -1.  So, the code below
    // works, but can be broken by someone thinking they can reverse the
    // test by testing n % 2 == 1.  The valid reverse test is n % 2 != 0.
    if n%2 == 0 {
        fmt.Println("even")
    } else {
        fmt.Println(" odd")
    }
}

func testBig(s string) {
    b, _ := new(big.Int).SetString(s, 10)
    fmt.Printf("Testing big integer %v:  ", b)
    // the Bit function is the only sensible test for big ints.
    if b.Bit(0) == 0 {
        fmt.Println("even")
    } else {
        fmt.Println("odd")
    }
}
```

```txt

Testing integer  -2:  even even
Testing integer  -1:   odd  odd
Testing integer   0:  even even
Testing integer   1:   odd  odd
Testing integer   2:  even even
Testing big integer -222222222222222222222222222222222222:  even
Testing big integer -1:  odd
Testing big integer 0:  even
Testing big integer 1:  odd
Testing big integer 222222222222222222222222222222222222:  even

```



## Groovy

Solution:

```groovy
def isOdd = { int i -> (i & 1) as boolean }
def isEven = {int i -> ! isOdd(i) }
```

Test:

```groovy
1.step(20, 2) { assert isOdd(it) }

50.step(-50, -2) { assert isEven(it) }
```



## Haskell

<code>even</code> and <code>odd</code> functions are already included in the standard Prelude.

```haskell>Prelude
 even 5
False
Prelude> even 42
True
Prelude> odd 5
True
Prelude> odd 42
False
```


Where '''even''' is derived from '''rem''', and '''odd''' is derived from even:

```haskell
import Prelude hiding (even, odd)

even, odd
  :: (Integral a)
  => a -> Bool
even = (0 ==) . (`rem` 2)

odd = not . even

main :: IO ()
main = print (even <$> [0 .. 9])
```

```txt
[True,False,True,False,True,False,True,False,True,False]
```


=={{header|Icon}} and {{header|Unicon}}==
One way is to check the remainder:

```unicon
procedure isEven(n)
    return n%2 = 0
end
```



## J

Modulo:

```j
   2 | 2 3 5 7
0 1 1 1
   2|2 3 5 7 + (2^89x)-1
1 0 0 0
```

Remainder:

```j
   (= <.&.-:) 2 3 5 7
1 0 0 0
   (= <.&.-:) 2 3 5 7+(2^89x)-1
0 1 1 1
```

Last bit in bit representation:

```j
   {:"1@#: 2 3 5 7
0 1 1 1
   {:"1@#: 2 3 5 7+(2^89x)-1
1 0 0 0
```

Bitwise and:

```j
   1 (17 b.) 2 3 5 7
0 1 1 1
```

Note: as a general rule, the simplest expressions in J should be preferred over more complex approaches.


## Java

Bitwise and:

```java
public static boolean isEven(int i){
    return (i & 1) == 0;
}
```

Modulo:

```java
public static boolean isEven(int i){
    return (i % 2) == 0;
}
```

Arbitrary precision bitwise:

```java
public static boolean isEven(BigInteger i){
    return i.and(BigInteger.ONE).equals(BigInteger.ZERO);
}
```

Arbitrary precision bit test (even works for negative numbers because of the way <code>BigInteger</code> represents the bits of numbers):

```java
public static boolean isEven(BigInteger i){
    return !i.testBit(0);
}
```

Arbitrary precision modulo:

```java
public static boolean isEven(BigInteger i){
    return i.mod(BigInteger.valueOf(2)).equals(BigInteger.ZERO);
}
```



## JavaScript


### ES5

Bitwise:

```javascript
function isEven( i ) {
  return (i & 1) === 0;
}

```

Modulo:

```javascript
function isEven( i ) {
  return i % 2 === 0;
}

// Alternative
function isEven( i ) {
  return !(i % 2);
}
```



### ES6

Lambda:

```javascript
// EMCAScript 6
const isEven = x => !(x % 2)
```


or, avoiding type coercion:

```javascript
(() => {
    'use strict';

    // even : Integral a => a -> Bool
    const even = x => (x % 2) === 0;

    // odd : Integral a => a -> Bool
    const odd = x => !even(x);


    // TEST ----------------------------------------
    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // show :: a -> String
    const show = JSON.stringify;

    // xs :: [Int]
    const xs = range(-6, 6);

    return show([xs.filter(even), xs.filter(odd)]);
})();
```


```txt
[[-6,-4,-2,0,2,4,6],[-5,-3,-1,1,3,5]]
```



## jq


In practice, to test whether an integer, i, is even or odd in jq, one would typically use: i % 2

For example, if it were necessary to have a strictly boolean function that would test if its input is an even integer, one could define:

```jq
def is_even: type == "number" and floor == 0 and . % 2 == 0;
```


The check that the floor is 0 is necessary as % is defined on floating point numbers.

"is_odd" could be similarly defined:


```jq
def is_odd: type == "number" and floor == 0 and . % 2 == 1;
```



## Jsish

Using bitwise and of low bit.


```javascript
#!/usr/bin/env jsish
/* Even or Odd, in Jsish */
function isEven(n:number):boolean { return (n & 1) === 0; }

provide('isEven', 1);

if (Interp.conf('unitTest')) {
;    isEven(0);
;    isEven(1);
;    isEven(2);
;    isEven(-13);
}

/*
=!EXPECTSTART!=
isEven(0) ==> true
isEven(1) ==> false
isEven(2) ==> true
isEven(-13) ==> false
=!EXPECTEND!=
*/
```

```txt
$ jsish --U isEven.jsi
isEven(0) ==> true
isEven(1) ==> false
isEven(2) ==> true
isEven(-13) ==> false
```



## Julia

Built-in functions:

```julia
iseven(i), isodd(i)
```



## K

The following implementation uses the modulo of
division by 2

```K

oddp: {:[x!2;1;0]} /Returns 1 if arg. is odd
evenp: {~oddp[x]}  /Returns 1 if arg. is even

Examples:
   oddp 32
0
   evenp 32
1

```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    while (true) {
        print("Enter an integer or 0 to finish : ")
        val n = readLine()!!.toInt()
        when {
            n == 0     -> return
            n % 2 == 0 -> println("Your number is even")
            else       -> println("Your number is odd")
        }
    }
}
```



## L++


```lisp
(defn bool isEven (int x) (return (% x 2)))
```



## LabVIEW

Using bitwise And<br/>
## Lang5


```lang5
: even?  2 % not ;
: odd?  2 % ;
1 even? .   # 0
1 odd? .    # 1
```



## Lasso


```Lasso
define isoddoreven(i::integer) => {
	#i % 2 ? return 'odd'
	return 'even'
}
isoddoreven(12)
```



## LC3 Assembly

Prints <tt>EVEN</tt> if the number stored in <tt>NUM</tt> is even, otherwise <tt>ODD</tt>.

```lc3asm
      .ORIG      0x3000

      LD         R0,NUM
      AND        R1,R0,1
      BRZ        EVEN

      LEA        R0,ODD
      BRNZP      DISP

EVEN  LEA        R0,EVN

DISP  PUTS

      HALT

NUM   .FILL      0x1C

EVN   .STRINGZ   "EVEN\n"
ODD   .STRINGZ   "ODD\n"

      .END
```



## Liberty BASIC


```lb
n=12

if n mod 2 = 0 then print "even" else print "odd"
```



## Lingo



```lingo
on even (n)
  return n mod 2 = 0
end

on odd (n)
  return n mode 2 <> 0
end
```



## LiveCode


```LiveCode
function odd n
    return (n bitand 1) = 1
end odd

function notEven n
    return (n mod 2) = 1
end notEven
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

;--- The declarations for the external C functions
declare i32 @printf(i8*, ...)

$"EVEN_STR" = comdat any
$"ODD_STR" = comdat any

@"EVEN_STR" = linkonce_odr unnamed_addr constant [12 x i8] c"%d is even\0A\00", comdat, align 1
@"ODD_STR" = linkonce_odr unnamed_addr constant [11 x i8] c"%d is odd\0A\00", comdat, align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4          ;-- allocate i
  store i32 0, i32* %1, align 4     ;-- store 0 in i
  br label %loop

loop:
  %2 = load i32, i32* %1, align 4   ;-- load i
  %3 = icmp ult i32 %2, 4           ;-- i < 4
  br i1 %3, label %loop_body, label %exit

loop_body:
  %4 = load i32, i32* %1, align 4   ;-- load i
  %5 = and i32 %4, 1                ;-- i & 1
  %6 = icmp eq i32 %5, 0            ;-- (i & 1) == 0
  br i1 %6, label %even_branch, label %odd_branch

even_branch:
  %7 = load i32, i32* %1, align 4   ;-- load i
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @"EVEN_STR", i32 0, i32 0), i32 %7)
  br label %loop_increment

odd_branch:
  %9 = load i32, i32* %1, align 4   ;-- load i
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @"ODD_STR", i32 0, i32 0), i32 %9)
  br label %loop_increment

loop_increment:
  %11 = load i32, i32* %1, align 4  ;-- load i
  %12 = add i32 %11, 1              ;-- increment i
  store i32 %12, i32* %1, align 4   ;-- store i
  br label %loop

exit:
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
```

```txt
0 is even
1 is odd
2 is even
3 is odd
```



## Logo


```logo
to even? :num
    output equal? 0 modulo :num 2
end
```



## Logtalk


```logtalk

:- object(even_odd).

    :- public(test_mod/1).
    test_mod(I) :-
        (   I mod 2 =:= 0 ->
            write(even), nl
        ;   write(odd), nl
        ).

    :- public(test_bit/1).
    test_bit(I) :-
        (   I /\ 1 =:= 1 ->
            write(odd), nl
        ;   write(even), nl
        ).

:- end_object.

```

```text

| ?- even_odd::test_mod(1).
odd
yes

| ?- even_odd::test_mod(2).
even
yes

| ?- even_odd::test_bit(1).
odd
yes

| ?- even_odd::test_bit(2).
even
yes

```



## Lua



```lua
-- test for even number
if n % 2 == 0 then
  print "The number is even"
end

-- test for odd number
if not (n % 2 == 0) then
  print "The number is odd"
end
```



## M2000 Interpreter

Binary.Add take any numeric type, but value must be in range of 0 to 0xFFFFFFFF
So Mod if a perfect choice, using it with Decimals (character @ indicate a Decimal type or literal).
Variable a take the type of input. There is no reason here to write it as def Odd(a as decimal)= binary.and(Abs(a), 1)=1

Def used to define variables (an error occur if same variable exist), or to define one line local functions. If a function exist then replace code. This is the same for modules/functions, a newer definition alter an old definition with same name, in current module if they are local, or global if they defined as global, like this Function Global F(x) { code block here}.

A function F(x) {} is same as
<pre >
Function F {
      Read x
      code here
}
</pre >

The same hold for Def Odd(a)=binary.and(Abs(a), 1)=1
Interpreter execute this:
<pre >
Function Odd {
      Read a
      =binary.and(Abs(a), 1)=1
}
</pre >

So here is the task. Show an overflow from a decimal, then change function.


```M2000 Interpreter

Module CheckOdd {
      Def Odd(a)= binary.and(Abs(a), 1)=1
      Print Odd(-5), Odd(6), Odd(11)
      Try {
            Print Odd(21212121212122122122121@)
      }
      Print Error$    ' overflow

      def Odd(a)= Int(Abs(a)) mod 2 =1
      Print Odd(21212121212122122122121@)
      Print Odd(-5), Odd(6), Odd(11)
}
CheckOdd

```



## M4


```M4
define(`even', `ifelse(eval(`$1'%2),0,True,False)')
define(`odd',  `ifelse(eval(`$1'%2),0,False,True)')

even(13)
even(8)

odd(5)
odd(0)
```



## Maple


```Maple
EvenOrOdd := proc( x::integer )
   if x mod 2 = 0 then
      print("Even"):
   else
      print("Odd"):
   end if:
end proc:
EvenOrOdd(9);
```


```txt
"Odd"
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
EvenQ[8]
```


=={{header|MATLAB}} / {{header|Octave}}==
Bitwise And:

```Matlab
   isOdd  =  logical(bitand(N,1));
   isEven = ~logical(bitand(N,1));
```

Remainder of division by two

```Matlab
   isOdd  =  logical(rem(N,2));
   isEven = ~logical(rem(N,2));
```

Modulo: 2

```Matlab
   isOdd  =  logical(mod(N,2));
   isEven = ~logical(mod(N,2));
```



## Maxima


```maxima
evenp(n);
oddp(n);
```



## MAXScript


```maxscript
-- MAXScript : Even or Odd : N.H. 2019
-- Open the MAXScript Listener for input and output
userInt = getKBValue prompt:"Enter an integer and i will tell you if its Even or Odd : "
if classOf userInt != Integer then print "The value you enter must be an integer"
else if (Mod userInt 2) == 0 Then Print "Your number is even"
else Print "Your number is odd"

```



## Mercury

Mercury's 'int' module provides tests for even/odd, along with all the operators that would be otherwise used to implement them.

```Mercury
even(N)  % in a body, suceeeds iff N is even.
odd(N).  % in a body, succeeds iff N is odd.

% rolling our own:
:- pred even(int::in) is semidet.

% It's an error to have all three in one module, mind; even/1 would fail to check as semidet.
even(N) :- N mod 2 = 0.   % using division that truncates towards -infinity
even(N) :- N rem 2 = 0.   % using division that truncates towards zero
even(N) :- N /\ 1 = 0.    % using bit-wise and.
```



## min

```min
3 even?
4 even?
5 odd?
get-stack print
```

```txt

(false true true)

```



## MiniScript


```MiniScript
for i in range(-4, 4)
    if i % 2 == 0 then print i + " is even" else print i + " is odd"
end for
```

```txt
-4 is even
-3 is odd
-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd
4 is even
```



## MIPS Assembly

This uses bitwise AND

```mips

.data
	even_str: .asciiz "Even"
	odd_str: .asciiz "Odd"

.text
	#set syscall to get integer from user
	li $v0,5
	syscall

	#perform bitwise AND and store in $a0
	and $a0,$v0,1

	#set syscall to print dytomh
	li $v0,4

	#jump to odd if the result of the AND operation
	beq $a0,1,odd
even:
	#load even_str message, and print
	la $a0,even_str
	syscall

	#exit program
	li $v0,10
	syscall

odd:
	#load odd_str message, and print
	la $a0,odd_str
	syscall

	#exit program
	li $v0,10
	syscall

```


=={{header|MK-61/52}}==
<lang>/	2	{x}	ЗН
```


''Result:'' "0" - number is even; "1" - number is odd.


## ML

=
## mLite
=

```ocaml
fun odd
		(x rem 2 = 1) = true
	| 	_ 	      = false
;

fun even
		(x rem 2 = 0) = true
	| 	_ 	      = false
;


```


=={{header|Modula-2}}==

```modula2
MODULE EvenOrOdd;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i : INTEGER;
BEGIN
    FOR i:=-5 TO 5 DO
        FormatString("%i is even: %b\n", buf, i, i MOD 2 = 0);
        WriteString(buf)
    END;

    ReadChar
END EvenOrOdd.
```



## Neko


```neko
var number = 6;

if(number % 2 == 0) {
	$print("Even");
} else {
	$print("Odd");
}
```


```txt
Even
```



## NESL

NESL provides <tt>evenp</tt> and <tt>oddp</tt> functions, but they wouldn't be hard to reimplement.

```nesl
function even(n) = mod(n, 2) == 0;

% test the function by applying it to the first ten positive integers: %
{even(n) : n in [1:11]};
```

```txt
it = [F, T, F, T, F, T, F, T, F, T] : [bool]
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

say 'Val'.right(5)': mod  - ver  - pos  - bits'
say '---'.right(5)': ---- + ---- + ---- + ----'
loop nn = -15 to 15 by 3
  say nn.right(5)':' eo(isEven(nn)) '-' eo(isEven(nn, 'v')) '-' eo(isEven(nn, 'p')) '-' eo(isEven(nn, 'b'))
  end nn
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Overloaded method.  Default is to use the remainder specialization below
method isEven(anInt, meth = 'R') public static returns boolean
  select case meth.upper().left(1)
    when 'R' then eo = isEvenRemainder(anInt)
    when 'V' then eo = isEvenVerify(anInt)
    when 'P' then eo = isEvenPos(anInt)
    when 'B' then eo = isEvenBits(anInt)
    otherwise     eo = isEvenRemainder(anInt) -- default
    end
  return eo

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isEvenRemainder(anInt) public static returns boolean
  return anInt // 2 == 0

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isEvenVerify(anInt) public static returns boolean
  return anInt.right(1).verify('02468') == 0

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isEvenPos(anInt) public static returns boolean
  return '13579'.pos(anInt.right(1)) == 0

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isEvenBits(anInt) public static returns boolean
  return \(anInt.d2x(1).x2b().right(1))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method eo(state = boolean) public static
  if state then sv = 'Even'
           else sv = 'Odd'
  return sv.left(4)

```

```txt

  Val: mod  - ver  - pos  - bits
  ---: ---- + ---- + ---- + ----
  -15: Odd  - Odd  - Odd  - Odd
  -12: Even - Even - Even - Even
   -9: Odd  - Odd  - Odd  - Odd
   -6: Even - Even - Even - Even
   -3: Odd  - Odd  - Odd  - Odd
    0: Even - Even - Even - Even
    3: Odd  - Odd  - Odd  - Odd
    6: Even - Even - Even - Even
    9: Odd  - Odd  - Odd  - Odd
   12: Even - Even - Even - Even
   15: Odd  - Odd  - Odd  - Odd

```



## Never


```Never

func isOdd(n : int) -> int {
    n % 2 == 1
}

func isEven(n : int) -> int {
    n % 2 == 0
}

```



## NewLISP


```NewLISP
(odd? 1)
(even? 2)
```



## Nim


```nim
# Least signficant bit:
proc isOdd(i: int): bool = (i and 1) != 0
proc isEven(i: int): bool = (i and 1) == 0

# Modulo:
proc isOdd2(i: int): bool = (i mod 2) != 0
proc isEven2(i: int): bool = (i mod 2) == 0

# Bit Shifting:
proc isOdd3(n: int): bool = n != ((n shr 1) shl 1)
proc isEven3(n: int): bool = n == ((n shr 1) shl 1)

echo isEven(1)
echo isOdd2(5)
```


=={{header|Oberon-2}}==
```oberon2

MODULE EvenOrOdd;
IMPORT
  S := SYSTEM,
  Out;
VAR
  x: INTEGER;
  s: SET;

BEGIN
  x := 10;Out.Int(x,0);
  IF ODD(x) THEN Out.String(" odd") ELSE Out.String(" even") END;
  Out.Ln;

  x := 11;s := S.VAL(SET,LONG(x));Out.Int(x,0);
  IF 0 IN s THEN Out.String(" odd") ELSE Out.String(" even") END;
  Out.Ln;

  x := 12;Out.Int(x,0);
  IF x MOD 2 # 0 THEN Out.String(" odd") ELSE Out.String(" even") END;
  Out.Ln
END EvenOrOdd.

```

```txt

10 even
11 odd
12 even

```



## Objeck


```objeck
a := Console->ReadString()->ToInt();
if(a % 2 = 0) {
  "even"->PrintLine();
}
else {
  "odd"->PrintLine();
};
```



## OCaml

Modulo:

```ocaml
let is_even d =
  (d mod 2) = 0

let is_odd d =
  (d mod 2) <> 0
```

Bitwise and:

```ocaml
let is_even d =
  (d land 1) = 0

let is_odd d =
  (d land 1) <> 0
```


An instructive view on functional programming and recursion:

```ocaml
(* hmm, only valid for N0 *)
let rec myeven = function
  | 0 -> true
  | 1 -> false
  | n -> myeven (n - 2)

(* and here we have the not function in if form *)
let myodd n = if myeven n then false else true
```



## Oforth



```Oforth
12 isEven
12 isOdd
```



## OOC


```ooc

// Using the modulo operator
even: func (n: Int) -> Bool {
  (n % 2) == 0
}

// Using bitwise and
odd: func (n: Int) -> Bool {
  (n & 1) == 1
}

```



## PARI/GP

GP does not have a built-in predicate for testing parity, but it's easy to code:

```parigp
odd(n)=n%2;
```

Alternately:

```parigp
odd(n)=bitand(n,1);
```

PARI can use the same method as [[#C|C]] for testing individual words. For multiprecision integers (t_INT), use <code>mpodd</code>.  If the number is known to be nonzero, <code>mod2</code> is (insignificantly) faster.


## Pascal

Built-in boolean function odd:

```pascal
isOdd := odd(someIntegerNumber);
```

bitwise and:

```pascal
function isOdd(Number: integer): boolean
begin
  isOdd := boolean(Number and 1)
end;
```

Dividing and multiplying by 2 and test on equality:

```pascal
function isEven(Number: integer): boolean
begin
  isEven := (Number = ((Number div 2) * 2))
end;
```

Using built-in modulo

```pascal
function isOdd(Number: integer): boolean
begin
  isOdd := boolean(Number mod 2)
end;
```



## Perl


```perl
for(0..10){
    print "$_ is ", qw(even odd)[$_ % 2],"\n";
}
```

or

```perl
print 6 % 2  ? 'odd' : 'even';   # prints even
```



## Perl 6

Perl 6 doesn't have a built-in for this, but with subsets it's easy to define a predicate for it.

```perl6
subset Even of Int where * %% 2;
subset Odd of Int where * % 2;

say 1 ~~ Even; # false
say 1 ~~ Odd;  # true
say 1.5 ~~ Odd # false ( 1.5 is not an Int )
```



## Phix

and_bits(i,1) returns 1(true) for odd integers and 0(false) for even integers. remainder(i,2) could also validly be used, however "true" for odd numbers is actually 1 for positive odd integers and -1 for negative odd integers.

```Phix
for i = -5 to 5 do
    ? {i, and_bits(i,1), remainder(i,2)}
end for
```

```txt

{-5,1,-1}
{-4,0,0}
{-3,1,-1}
{-2,0,0}
{-1,1,-1}
{0,0,0}
{1,1,1}
{2,0,0}
{3,1,1}
{4,0,0}
{5,1,1}

```



## PHP


```php

// using bitwise and to check least significant digit
echo (2 & 1) ? 'odd' : 'even';
echo (3 & 1) ? 'odd' : 'even';

// using modulo
echo (3 % 2) ? 'odd' : 'even';
echo (4 % 2) ? 'odd' : 'even';

```


```txt
even
odd
odd
even
```



## PicoLisp

PicoLisp doesn't have a built-in predicate for that. Using '[http://software-lab.de/doc/refB.html#bit? bit?]' is the easiest and most efficient. The bit test with 1 will return NIL if the number is even.

```PicoLisp
: (bit? 1 3)
-> 1  # Odd

: (bit? 1 4)
-> NIL  # Even
```



## Pike


```Pike>
 int i = 73;
> (i&1);
Result: 1
> i%2;
Result: 1
```



## PL/I


```PL/I
i = iand(i,1)
```

The result is 1 when i is odd, and 0 when i is even.


## PowerShell

### Predicate

A predicate can be used with BigInteger objects. Even/odd predicates to not exist for basic value types. Type accelerator [bigint] can be used in place of [System.Numerics.BigInteger].

```PowerShell

$IsOdd  = -not ( [bigint]$N ).IsEven
$IsEven =      ( [bigint]$N ).IsEven

```


### Least significant digit


```PowerShell

$IsOdd  = [boolean]( $N -band 1 )
$IsEven = [boolean]( $N -band 0 )

```


### Remainder

Despite being known as a modulus operator, the % operator in PowerShell actually returns a remainder. As such, when testing negative numbers it returns the true modulus result minus M. In this specific case, it returns -1 for odd negative numbers. Thus we test for not zero for odd numbers.

```PowerShell

$IsOdd  = $N % 2 -ne 0
$IsEven = $N % 2 -eq 0

```



## Processing


```Processing

boolean isEven(int i){
  return i%2 == 0;
}

boolean isOdd(int i){
  return i%2 == 1;
}

```



## Prolog

Prolog does not provide special even or odd predicates as one can simply write "0 is N mod 2"
to test whether the integer N is even.  To illustrate, here is a predicate that can
be used both to test whether an integer is even and to generate the non-negative even numbers:

```prolog

  even(N) :-
     (between(0, inf, N); integer(N) ),
     0 is N mod 2.

```


### Least Significant Bit

If N is a positive integer, then lsb(N) is the offset of its least significant bit, so we could write:

```prolog

  odd(N) :- N = 0 -> false; 0 is lsb(abs(N)).

```



## PureBasic


```PureBasic
;use last bit method
isOdd = i & 1         ;isOdd is non-zero if i is odd
isEven = i & 1 ! 1    ;isEven is non-zero if i is even

;use modular method
isOdd = i % 2         ;isOdd is non-zero if i is odd
isEven = i % 2 ! 1    ;isEven is non-zero if i is even
```



## Python

===Python: Using the least-significant bit method===

```python>>>
 def is_odd(i): return bool(i & 1)

>>> def is_even(i): return not is_odd(i)

>>> [(j, is_odd(j)) for j in range(10)]
[(0, False), (1, True), (2, False), (3, True), (4, False), (5, True), (6, False), (7, True), (8, False), (9, True)]
>>> [(j, is_even(j)) for j in range(10)]
[(0, True), (1, False), (2, True), (3, False), (4, True), (5, False), (6, True), (7, False), (8, True), (9, False)]
>>>
```



### Python: Using modular congruences


```python>>
 def is_even(i):
        return (i % 2) == 0

>>> is_even(1)
False
>>> is_even(2)
True
>>>
```



## R


```R
is.even <- function(x) !is.odd(x)

is.odd <- function(x) intToBits(x)[1] == 1
#or
is.odd <- function(x) x %% 2 == 1
```



## Racket

With built in predicates:

```Racket
(even? 6) ; -> true
(even? 5) ; -> false
(odd? 6) ; -> false
(odd? 5) ; -> true
```


With modular arithmetic:

```Racket
(define (my-even? x)
  (= (modulo x 2) 0))

(define (my-odd? x)
  (= (modulo x 2) 1))
```



## Rascal


```rascal
public bool isEven(int n) = (n % 2) == 0;
public bool isOdd(int n) = (n % 2) == 1;
```

Or with block quotes:

```rascal
public bool isEven(int n){return (n % 2) == 0;}
public bool isOdd(int n){return (n % 2) == 1;}
```



## REXX

Programming note:   division by   <big> '''1''' </big>   (one)   in REXX is a way to normalize a number:
:::* by removing a superfluous leading   '''+'''   sign
:::* by removing superfluous leading  zeroes
:::* by removing superfluous trailing zeroes
:::* by removing a trailing decimal point
:::* possible converting an exponentiated number
:::* possible rounding the number to the current ''digits''

'''Programming note''':   the last method is the fastest method in REXX to determine oddness/evenness.

It requires a sparse stemmed array     '''!.'''     be defined in the program's prologue (or elsewhere).

This method gets its speed from   ''not''   using any BIF and   ''not''   performing any (remainder) division.

'''Some notes on programming styles''':
If (execution) speed isn't an issue, then the 1<sup>st</sup> test method

shown would be the simplest   (in terms of coding the concisest/tightest/smallest code).   The other test

methods differ mostly in programming techniques, mostly depending on the REXX programmer's style.

The last method shown is the fastest algorithm, albeit it might be a bit obtuse (without comments) to a

novice reader of the REXX language   (and it requires additional REXX statement baggage).

```rexx
/*REXX program tests and displays if an integer is  even or odd  using different styles.*/
!.=0;   do j=0  by 2  to 8;   !.j=1;   end       /*assign  0,2,4,6,8  to a "true" value.*/
                                                 /* [↑]  assigns even digits to  "true".*/
numeric digits 1000                              /*handle most huge numbers from the CL.*/
parse arg x _ .                                  /*get an argument from the command line*/
if x==''               then call terr "no integer input (argument)."
if _\=='' | arg()\==1  then call terr "too many arguments: "          _  arg(2)
if \datatype(x, 'N')   then call terr "argument isn't numeric: "      x
if \datatype(x, 'W')   then call terr "argument isn't an integer: "   x
y=abs(x)/1                                       /*in case  X  is negative or malformed,*/
                                                 /* [↑]  remainder of neg # might be -1.*/
                                                 /*malformed #s: 007  9.0  4.8e1  .21e2 */
call tell 'remainder method (oddness)'
if y//2  then say  x  'is odd'
         else say  x  'is even'
                                                 /* [↑]  uses division to get remainder.*/

call tell 'rightmost digit using BIF (not evenness)'
_=right(y, 1)
if pos(_, 86420)==0  then say x 'is odd'
                     else say x 'is even'
                                                 /* [↑]  uses 2 BIF (built─in functions)*/

call tell 'rightmost digit using BIF (evenness)'
_=right(y, 1)
if pos(_, 86420)\==0  then say x 'is even'
                      else say x 'is odd'
                                                 /* [↑]  uses 2 BIF (built─in functions)*/

call tell 'even rightmost digit using array (evenness)'
_=right(y, 1)
if !._  then say x 'is even'
        else say x 'is odd'
                                                 /* [↑]  uses a BIF (built─in function).*/

call tell 'remainder of division via function invoke (evenness)'
if even(y)  then say x 'is even'
            else say x 'is odd'
                                                 /* [↑]  uses (even) function invocation*/

call tell 'remainder of division via function invoke (oddness)'
if odd(y)  then say x 'is odd'
           else say x 'is even'
                                                 /* [↑]  uses (odd)  function invocation*/

call tell 'rightmost digit using BIF (not oddness)'
_=right(y, 1)
if pos(_, 13579)==0  then say x 'is even'
                     else say x 'is odd'
                                                 /* [↑]  uses 2 BIF (built─in functions)*/

call tell 'rightmost (binary) bit (oddness)'
if right(x2b(d2x(y)), 1)  then say x 'is odd'
                          else say x 'is even'
                                                 /* [↑]  requires extra numeric digits. */

call tell 'parse statement using BIF (not oddness)'
parse var  y   ''  -1  _                         /*obtain last decimal digit of the Y #.*/
if pos(_, 02468)==0  then say x 'is odd'
                     else say x 'is even'
                                                 /* [↑]  uses a BIF (built─in function).*/

call tell 'parse statement using array (evenness)'
parse var  y   ''  -1  _                         /*obtain last decimal digit of the Y #.*/
if !._  then say  x  'is even'
        else say  x  'is odd'
                                                 /* [↑]  this is the fastest algorithm. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
even:                     return \( arg(1)//2 )  /*returns "evenness" of arg, version 1.*/
even:                     return    arg(1)//2==0 /*   "         "      "  "      "    2.*/
even: parse arg '' -1 _;  return !._             /*   "         "      "  "      "    3.*/
                                                 /*last version shown is the fastest.   */
odd:                      return   arg(1)//2     /*returns  "oddness" of the argument.  */
tell: say;   say center('using the' arg(1), 79, "═");                    return
terr: say;   say '***error***';     say;    say arg(1);    say;          exit 13
```

'''output'''   when using the input of:   <tt> 0 </tt>

```txt

═════════════════════using the remainder method (oddness)══════════════════════
0 is even

══════════════using the rightmost digit using BIF (not evenness)═══════════════
0 is even

════════════════using the rightmost digit using BIF (evenness)═════════════════
0 is even

═════════════using the even rightmost digit using array (evenness)═════════════
0 is even

════════using the remainder of division via function invoke (evenness)═════════
0 is even

═════════using the remainder of division via function invoke (oddness)═════════
0 is even

═══════════════using the rightmost digit using BIF (not oddness)═══════════════
0 is even

══════════════════using the rightmost (binary) bit (oddness)═══════════════════
0 is even

═══════════════using the parse statement using BIF (not oddness)═══════════════
0 is even

═══════════════using the parse statement using array (evenness)════════════════
0 is even

```

'''output'''   when using the input of:   <tt> 9876543210987654321098765432109876543210987654321 </tt>

```txt

═════════════════════using the remainder method (oddness)══════════════════════
9876543210987654321098765432109876543210987654321 is odd

   (rest of the output was elided.)

```

'''output'''   when using the input of:   <tt> .6821e4 </tt>

```txt

═════════════════════using the remainder method (oddness)══════════════════════
.8621e4 is odd

   (rest of the output was elided.)

```


'''output'''   when using the input of:   <tt> -9411 </tt>

```txt

═════════════════════using the remainder method (oddness)══════════════════════
-9411 is odd

   (rest of the output was elided.)

```



## Ring


```ring

size = 10
for i = 1 to size
    if i % 2 = 1 see "" + i + " is odd" + nl
    else see "" + i + " is even" + nl ok
next

```



## Ruby



```ruby
print "evens: "
p -5.upto(5).select(&:even?)
print "odds: "
p -5.upto(5).select(&:odd?)
```

```txt
evens: [-4, -2, 0, 2, 4]
odds: [-5, -3, -1, 1, 3, 5]
```

Other ways to test even-ness:

```ruby
n & 1 == 0
quotient, remainder = n.divmod(2); remainder == 0

# The next way only works when n.to_f/2 is exact.
# If Float is IEEE double, then -2**53 .. 2**53 must include n.
n.to_f/2 == n/2

# You can use the bracket operator to access the i'th bit
# of a Fixnum or Bignum (i = 0 means least significant bit)
n[0].zero?
```



## Run BASIC


```runbasic
for i = 1 to 10
  if i and 1 then print i;" is odd" else print i;" is even"
next i
```


```txt

1 is odd
2 is even
3 is odd
4 is even
5 is odd
6 is even
7 is odd
8 is even
9 is odd
10 is even

```



## Rust

Checking the last significant digit:

```rust
let is_odd = |x: i32| x & 1 == 1;
let is_even = |x: i32| x & 1 == 0;
```


Using modular congruences:

```rust
let is_odd = |x: i32| x % 2 != 0;
let is_even = |x: i32| x % 2 == 0;
```



## Scala


```scala
def isEven( v:Int ) : Boolean = v % 2 == 0
def isOdd( v:Int ) : Boolean = v % 2 != 0
```

Accept any numeric type as an argument:

```scala
def isEven( v:Number ) : Boolean = v.longValue % 2 == 0
def isOdd( v:Number ) : Boolean = v.longValue % 2 != 0
```

```txt
isOdd( 81 )                     // Results in true
isEven( BigInt(378) )           // Results in true
isEven( 234.05003513013145 )    // Results in true
```



## Scheme

<code>even?</code> and <code>odd?</code> functions are built-in (R<sup>4</sup>RS, R<sup>5</sup>RS, and R<sup>6</sup>RS):

```scheme>
 (even? 5)
#f
> (even? 42)
#t
> (odd? 5)
#t
> (odd? 42)
#f
```



## Seed7

Test whether an integer or bigInteger is odd:

```seed7
odd(aNumber)
```

Test whether an integer or bigInteger is even:

```seed7
not odd(aNumber)
```



## SequenceL


```sequencel
even(x) := x mod 2 = 0;
odd(x) := x mod 2 = 1;
```


<pre style="height: 25ex; overflow: scroll">
cmd:>even(1 ... 10)
[false,true,false,true,false,true,false,true,false,true]
cmd:>odd(1 ... 10)
[true,false,true,false,true,false,true,false,true,false]

```



## SETL

SETL provides built-in <tt>even</tt> and <tt>odd</tt> functions. This short program illustrates their use.

```setl
xs := {1..10};
evens := {x in xs | even( x )};
odds := {x in xs | odd( x )};
print( evens );
print( odds );
```

```txt
{2 4 6 8 10}
{1 3 5 7 9}
```



## Shen


Mutual Recursion:

```shen
(define even?
    0 -> true
    X -> (odd? (- X 1)))

(define odd?
    0 -> false
    X -> (even? (- X 1)))
```


Modulo:

```shen
(define even? X -> (= 0 (shen.mod X 2)))

(define odd? X -> (not (= 0 (shen.mod X 2))))
```



## Sidef

Built-in methods:

```ruby
var n = 42;
say n.is_odd;       # false
say n.is_even;      # true
```


Checking the last significant digit:

```ruby
func is_odd(n)  { n&1 == 1 };
func is_even(n) { n&1 == 0 };
```


Using modular congruences:

```ruby
func is_odd(n)  { n%2 == 1 };
func is_even(n) { n%2 == 0 };
```



## Smalltalk


Using the built in methods on Number class:


```smalltalk
5 even
5 odd
```


even is implemented as follows:

```smalltalk>Number>
even
	^((self digitAt: 1) bitAnd: 1) = 0

```



## SNOBOL4

```SNOBOL4
      DEFINE('even(n)')                         :(even_end)
even  even = (EQ(REMDR(n, 2), 0) 'even', 'odd') :(RETURN)
even_end

      OUTPUT = "-2 is " even(-2)
      OUTPUT = "-1 is " even(-1)
      OUTPUT = "0 is " even(0)
      OUTPUT = "1 is " even(1)
      OUTPUT = "2 is " even(2)
END
```

```txt
-2 is even
-1 is odd
0 is even
1 is odd
2 is even

```



## SNUSP


```SNUSP

$====!/?\==even#
      - -
#odd==\?/

```



## SPL


```spl>
 n, 0..9
  ? #.even(n), #.output(n," even")
  ? #.odd(n), #.output(n," odd")
<
```

```txt

0 even
1 odd
2 even
3 odd
4 even
5 odd
6 even
7 odd
8 even
9 odd

```



## SQL

Database vendors can't agree on how to get a remainder. This should work for many, including Oracle. For others, including MS SQL Server, try "int % 2" instead of "mod(int, 2)".

```sql
-- Setup a table with some integers
create table ints(int integer);
insert into ints values (-1);
insert into ints values (0);
insert into ints values (1);
insert into ints values (2);

-- Are they even or odd?
select
  int,
  case mod(int, 2) when 0 then 'Even' else 'Odd' end
from
  ints;
```


```txt

       INT CASE
---------- ----
        -1 Odd
         0 Even
         1 Odd
         2 Even
```



## SSEM

The SSEM doesn't provide <tt>AND</tt>, but for once the instruction set does allow the problem to be solved quite elegantly (albeit extravagantly slowly). Load the value of <math>n</math> into storage address 15. The first three instructions test whether <math>n</math> is positive, and replace it with its negation if it isn't. We then loop, subtracting 2 each time and testing whether we have got down either to 0 or to 1. When we have, the computer will halt with the accumulator storing 0 if <math>n</math> was even or 1 if it was odd.

Note that the constant 2, stored at address 14, does double service: it is the operand for the <tt>Sub.</tt> instruction at address 6 and also the jump target returning to the top of the main loop (which is at address 2 + 1 = 3).

For larger positive or smaller negative values of <math>n</math>, you should be ready with something else to do while the machine is working: a test run took several minutes to confirm that 32,769 was odd.

```ssem
11110000000000100000000000000000   0. -15 to c
00000000000000110000000000000000   1. Test
11110000000001100000000000000000   2. c to 15
11110000000000100000000000000000   3. -15 to c
00001000000001100000000000000000   4. c to 16
00001000000000100000000000000000   5. -16 to c
01110000000000010000000000000000   6. Sub. 14
11110000000001100000000000000000   7. c to 15
10110000000000010000000000000000   8. Sub. 13
00000000000000110000000000000000   9. Test
01110000000000000000000000000000  10. 14 to CI
11110000000000100000000000000000  11. -15 to c
00000000000001110000000000000000  12. Stop
10000000000000000000000000000000  13. 1
01000000000000000000000000000000  14. 2
```



## Standard ML


```sml
fun even n =
  n mod 2 = 0;

fun odd n =
  n mod 2 <> 0;

(* bitwise and *)

type werd = Word.word;

fun evenbitw(w: werd) =
  Word.andb(w, 0w2) = 0w0;

fun oddbitw(w: werd) =
  Word.andb(w, 0w2) <> 0w0;
```



## Stata


```stata
mata
function iseven(n) {
	return(mod(n,2)==0)
}

function isodd(n) {
	return(mod(n,2)==1)
}
end
```



## Swift


```Swift
func isEven(n:Int) -> Bool {

    // Bitwise check
    if (n & 1 != 0) {
        return false
    }

    // Mod check
    if (n % 2 != 0) {
        return false
    }
    return true
}
```



## Symsyn


```symsyn

n : 23

 if n bit 0
    'n is odd' []
 else
    'n is even' []

```



## Tcl


```tcl
package require Tcl 8.5

# Bitwise test is the most efficient
proc tcl::mathfunc::isOdd x  { expr {$x & 1} }
proc tcl::mathfunc::isEven x { expr {!($x & 1)} }

puts " # O E"
puts 24:[expr isOdd(24)],[expr isEven(24)]
puts 49:[expr isOdd(49)],[expr isEven(49)]
```

```txt

 # O E
24:0,1
49:1,0

```


=={{header|TI-83 BASIC}}==
TI-83 BASIC does not have a modulus operator.

```ti83b

If fPart(.5Ans
Then
Disp "ODD
Else
Disp "EVEN
End

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP n=-5,5
x=MOD(n,2)
SELECT x
CASE 0
PRINT n," is even"
DEFAULT
PRINT n," is odd"
ENDSELECT
ENDLOOP
```

```txt

-5 is odd
-4 is even
-3 is odd
-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd
4 is even
5 is odd

```



## UNIX Shell


```shell
iseven() {
    [[ $(($1%2)) -eq 0 ]] && return 0
    return 1
}
```



## Ursa


```ursa
decl int input
set input (in int console)
if (= (mod input 2) 1)
        out "odd" endl console
else
        out "even" endl console
end if
```

Output:

```txt
123
odd
```


=={{header|உயிர்/Uyir}}==
<lang உயிர்/Uyir>முதன்மை என்பதின் வகை எண் பணி {{
        எ இன் வகை எண்{$5} = 0;
        படை வகை சரம்;

        "எண்ணைக் கொடுங்கள்? ") ஐ திரை.இடு;

        எ = எண்{$5} ஐ விசை.எடு;

        ஒருக்கால் (எ.இருமம்(0) == 1) ஆகில் {
                படை = "ஒற்றை";
        } இல்லையேல் {
                படை = "இரட்டை ";
        }

        {எ, " ஒரு ", படை, "ப்படை எண் ஆகும்"} என்பதை திரை.இடு;

        முதன்மை  = 0;
}};
```



## VBA


```txt

4 ways = 4 Functions :
IsEven ==> Use the even and odd predicates
IsEven2 ==> Check the least significant digit. With binary integers, i bitwise-and 1 equals 0 iff i is even
IsEven3 ==> Divide i by 2. The remainder equals 0 if i is even.
IsEven4 ==> Use modular congruences
```



```vb

Option Explicit

Sub Main_Even_Odd()
Dim i As Long

    For i = -50 To 48 Step 7
        Debug.Print i & " : IsEven ==> " & IIf(IsEven(i), "is even", "is odd") _
         & " " & Chr(124) & " IsEven2 ==> " & IIf(IsEven2(i), "is even", "is odd") _
         & " " & Chr(124) & " IsEven3 ==> " & IIf(IsEven3(i), "is even", "is odd") _
         & " " & Chr(124) & " IsEven4 ==> " & IIf(IsEven4(i), "is even", "is odd")
    Next
End Sub

Function IsEven(Number As Long) As Boolean
'Use the even and odd predicates
    IsEven = (WorksheetFunction.Even(Number) = Number)
End Function

Function IsEven2(Number As Long) As Boolean
'Check the least significant digit.
'With binary integers, i bitwise-and 1 equals 0 iff i is even, or equals 1 iff i is odd.
Dim lngTemp As Long
    lngTemp = CLng(Right(CStr(Number), 1))
    If (lngTemp And 1) = 0 Then IsEven2 = True
End Function

Function IsEven3(Number As Long) As Boolean
'Divide i by 2.
'The remainder equals 0 if i is even.
Dim sngTemp As Single
    sngTemp = Number / 2
    IsEven3 = ((Int(sngTemp) - sngTemp) = 0)
End Function

Function IsEven4(Number As Long) As Boolean
'Use modular congruences
    IsEven4 = (Number Mod 2 = 0)
End Function

```

```txt
-50 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
-43 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
-36 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
-29 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
-22 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
-15 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
-8 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
-1 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
6 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
13 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
20 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
27 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
34 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
41 : IsEven ==> is odd | IsEven2 ==> is odd | IsEven3 ==> is odd | IsEven4 ==> is odd
48 : IsEven ==> is even | IsEven2 ==> is even | IsEven3 ==> is even | IsEven4 ==> is even
```



## VBScript


```vb

Function odd_or_even(n)
	If n Mod 2 = 0 Then
		odd_or_even = "Even"
	Else
		odd_or_even = "Odd"
	End If
End Function

WScript.StdOut.Write "Please enter a number: "
n = WScript.StdIn.ReadLine
WScript.StdOut.Write n & " is " & odd_or_even(CInt(n))
WScript.StdOut.WriteLine

```


```txt

C:\>cscript /nologo odd_or_even.vbs
Please enter a number: 6
6 is Even

C:\>cscript /nologo odd_or_even.vbs
Please enter a number: 9
9 is Odd

C:\>cscript /nologo odd_or_even.vbs
Please enter a number: -1
-1 is Odd

```



## Visual Basic .NET

```vbnet
Module Module1

    Sub Main()
        Dim str As String
        Dim num As Integer
        While True
            Console.Write("Enter and integer or 0 to finish: ")
            str = Console.ReadLine()
            If Integer.TryParse(str, num) Then
                If num = 0 Then
                    Exit While
                End If
                If num Mod 2 = 0 Then
                    Console.WriteLine("Even")
                Else
                    Console.WriteLine("Odd")
                End If
            Else
                Console.WriteLine("Bad input.")
            End If
        End While
    End Sub

End Module
```



## WDTE


```WDTE>let s =
 import 'stream';
let str => import 'strings';

let evenOrOdd n => (
	let even n => == (% n 2) 0;
	switch n {
		even => 'even';
		default => 'odd';
	};
);

s.range 10
-> s.map (@ s n => str.format '{} is {}.' n (evenOrOdd n))
-> s.map (io.writeln io.stdout)
-> s.drain;
```


=={{header|x86_64 Assembly}}==
<lang x86_64 Assembly>
evenOdd:
mov  rax,1
and  rax,rdi
ret

```



## xEec


```xEec

>100 p i# jz-1 o# t h#1 ms jz2003 p >0110 h#2 r ms t h#1 ms p
jz1002 h? jz2003 p jn0110 h#10 o$ p jn100 >2003 p p h#0 h#10
h$d h$d h$o h#32 h$s h$i h#32 jn0000 >1002 p p h#0 h#10
h$n h$e h$v h$e h#32 h$s h$i h#32 >0000 o$ p jn0000 jz100

```



## XLISP

XLISP provides <tt>EVENP</tt> and <tt>ODDP</tt>, or, if you prefer, <tt>EVEN?</tt> and <tt>ODD?</tt>; if one wanted to reimplement them, it could be done like this (or in other ways).

```lisp
(defun my-evenp (x)
    (= (logand x 1) 0) )

(defun my-oddp (x)
    (/= (logand x 1) 0) )
```



## Xojo


```vb

For num As Integer = 1 To 5
  If num Mod 2 = 0 Then
    MsgBox(Str(num) + " is even.")
  Else
    MsgBox(Str(num) + " is odd.")
  End If
Next

```


```txt

1 is odd.
2 is even.
3 is odd.
4 is even.
5 is odd.

```



## XPL0


```XPL0
include c:\cxpl\codes;
int I;
[for I:= -4 to +3 do
        [IntOut(0, I);
        Text(0, if I&1 then " is odd   " else " is even  ");
        Text(0, if rem(I/2)#0 then "odd" else "even");
        CrLf(0);
        ];
]
```


```txt

-4 is even  even
-3 is odd   odd
-2 is even  even
-1 is odd   odd
0 is even  even
1 is odd   odd
2 is even  even
3 is odd   odd

```



## Yabasic

```Yabasic
for i = -5 to 5
    print i, and(i,1), mod(i,2)
next

```



## zkl


```zkl
[-3..4].pump(fcn(n){ println(n," is ",n.isEven and "even" or "odd") })
```

Ints have isEven and isOdd properties. pump, in this case, is the same as apply/map without aggregating a result.
```txt

-3 is odd
-2 is even
-1 is odd
0 is even
1 is odd
2 is even
3 is odd
4 is even

```


```zkl
[-3..4].apply("isEven").println();
```

```txt
L(False,True,False,True,False,True,False,True)
```



## zonnon


```zonnon

module Main;
var
	x: integer;
	s: set;
begin
	x := 10;writeln(x:3," is odd?",odd(x));
	s := set(s);writeln(x:3," is odd?",0 in s); (* check right bit *)
	x := 11;writeln(x:3," is odd?",odd(x));
	s := set(x);writeln(x:3," is odd?",0 in s); (* check right bit *)
end Main.

```

```txt

 10 is odd? false
 10 is odd? false
 11 is odd?  true
 11 is odd?  true

```


## ZX Spectrum Basic


```zxbasic
10 FOR n=-3 TO 4: GO SUB 30: NEXT n
20 STOP
30 LET odd=FN m(n,2)
40 PRINT n;" is ";("Even" AND odd=0)+("Odd" AND odd=1)
50 RETURN
60 DEF FN m(a,b)=a-INT (a/b)*b
```

