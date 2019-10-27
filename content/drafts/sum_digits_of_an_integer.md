+++
title = "Sum digits of an integer"
description = ""
date = 2019-10-16T18:36:38Z
aliases = []
[extra]
id = 12054
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Take a   [[wp:Natural_number|Natural Number]]   in a given base and return the sum of its digits:
:*   '''1'''<sub>10</sub>                  sums to   '''1'''
:*   '''1234'''<sub>10</sub>       sums to   '''10'''
:*   '''fe'''<sub>16</sub>              sums to   '''29'''
:*   '''f0e'''<sub>16</sub>           sums to   '''29'''





## 360 Assembly

{{trans|REXX}}
The program uses two ASSIST macro (XDECO,XPRNT) to keep the code as short as possible. 

```360asm
*        Sum digits of an integer  08/07/2016
SUMDIGIN CSECT
         USING  SUMDIGIN,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         LA     R11,NUMBERS        @numbers
         LA     R8,1               k=1
LOOPK    CH     R8,=H'4'           do k=1 to hbound(numbers)
         BH     ELOOPK             "
         SR     R10,R10              sum=0
         LA     R7,1                 j=1
LOOPJ    CH     R7,=H'8'             do j=1 to length(number)
         BH     ELOOPJ               "
         LR     R4,R11                 @number
         BCTR   R4,0                   -1
         AR     R4,R7                  +j
         MVC    D,0(R4)                d=substr(number,j,1)
         SR     R9,R9                  ii=0
         SR     R6,R6                  i=0
LOOPI    CH     R6,=H'15'              do i=0 to 15
         BH     ELOOPI                 "
         LA     R4,DIGITS                @digits
         AR     R4,R6                    i
         MVC    C,0(R4)                  c=substr(digits,i+1,1)
         CLC    D,C                      if d=c
         BNE    NOTEQ                    then
         LR     R9,R6                      ii=i
         B      ELOOPI                     leave i
NOTEQ    LA     R6,1(R6)                 i=i+1
         B      LOOPI                  end do i
ELOOPI   AR     R10,R9                 sum=sum+ii
         LA     R7,1(R7)               j=j+1
         B      LOOPJ                end do j
ELOOPJ   MVC    PG(8),0(R11)         number
         XDECO  R10,XDEC             edit sum
         MVC    PG+8(8),XDEC+4       output sum
         XPRNT  PG,L'PG              print buffer
         LA     R11,8(R11)           @number=@number+8
         LA     R8,1(R8)             k=k+1
         B      LOOPK              end do k
ELOOPK   L      R13,4(0,R13)       epilog 
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
DIGITS   DC     CL16'0123456789ABCDEF'
NUMBERS  DC     CL8'1',CL8'1234',CL8'FE',CL8'F0E'
C        DS     CL1
D        DS     CL1
PG       DC     CL16' '            buffer
XDEC     DS     CL12               temp
         YREGS
         END    SUMDIGIN
```

{{out}}

```txt

1              1
1234          10
FE            29
F0E           29

```



## Ada


Numeric constants in Ada are either decimal or written as B#Digits#. Here B is the base, written as a decimal number, and 
Digits is a base-B number. E.g., 30, 10#30# 2#11110#, and 16#1E# are the same number -- either written in decimal, binary or hexadecimal notation. 


```Ada
with Ada.Integer_Text_IO;

procedure Sum_Digits is
   -- sums the digits of an integer (in whatever base)
   -- outputs the sum (in base 10)

   function Sum_Of_Digits(N: Natural; Base: Natural := 10) return Natural is
      Sum: Natural := 0;
      Val: Natural := N;
   begin
      while Val > 0 loop
         Sum := Sum + (Val mod Base);
         Val := Val / Base;
      end loop;
      return Sum;
   end Sum_Of_Digits;

   use Ada.Integer_Text_IO;

begin -- main procedure Sum_Digits
   Put(Sum_OF_Digits(1));            --   1
   Put(Sum_OF_Digits(12345));        --  15
   Put(Sum_OF_Digits(123045));       --  15
   Put(Sum_OF_Digits(123045,  50));  -- 104
   Put(Sum_OF_Digits(16#fe#,  10));  --  11
   Put(Sum_OF_Digits(16#fe#,  16));  --  29
   Put(Sum_OF_Digits(16#f0e#, 16));  --  29
end Sum_Digits;
```


{{out}}


```txt
          1         15         15        104         11         29         29
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68

# operator to return the sum of the digits of an integer value in the #
# specified base                                                      #
PRIO SUMDIGITS = 1;
OP   SUMDIGITS = ( INT value, INT base )INT:
     IF base < 2
     THEN
         # invalid base #
         print( ( "Base for digit sum must be at least 2", newline ) );
         stop
     ELSE
         # the base is OK #
         INT    result := 0;
         INT    rest   := ABS value;

         WHILE rest /= 0
         DO
             result PLUSAB ( rest MOD base );
             rest   OVERAB base
         OD;

         result
     FI; # SUMDIGITS #

# additional operator so we can sum the digits of values expressed in #
# other than base 10, e.g. 16ra is a hex lteral with value 10         #
# (Algol 68 allows bases 2, 4, 8 and 16 for non-base 10 literals)     #
# however as such literals are BITS values, not INTs, we need this    #
# second operator                                                     #
OP   SUMDIGITS = ( BITS value, INT base )INT: ABS value SUMDIGITS base;

main:(

    # test the SUMDIGITS operator #

    print( ( "value\base base digit-sum", newline ) );
    print( ( "      1\10   10 ", whole(      1 SUMDIGITS 10, -9 ), newline ) );
    print( ( "   1234\10   10 ", whole(   1234 SUMDIGITS 10, -9 ), newline ) );
    print( ( "     fe\16   16 ", whole(  16rfe SUMDIGITS 16, -9 ), newline ) );
    print( ( "    f0e\16   16 ", whole( 16rf0e SUMDIGITS 16, -9 ), newline ) );

    # of course, we don't have to express the number in the base we sum #
    # the digits in...                                                  #
    print( ( "     73\10   71 ", whole(     73 SUMDIGITS 71, -9 ), newline ) )

)

```

{{out}}

```txt

value\base base digit-sum
      1\10   10         1
   1234\10   10        10
     fe\16   16        29
    f0e\16   16        29
     73\10   71         3

```



==AppleScript==

```AppleScript
-- INTEGER DIGITS SUMMED -----------------------------------------------------

-- digitsSummed :: (Int | String) -> Int
on digitsSummed(n)
    
    -- digitAdded :: Int -> String -> Int
    script digitAdded
        
        -- Numeric values of known glyphs: 0-9 A-Z a-z
        -- digitValue :: String -> Int
        on digitValue(s)
            set i to id of s
            if i > 47 and i < 123 then -- 0-z
                if i < 58 then -- 0-9
                    i - 48
                else if i > 96 then -- a-z
                    i - 87
                else if i > 64 and i < 91 then -- A-Z
                    i - 55
                else -- unknown glyph
                    0
                end if
            else -- unknown glyph
                0
            end if
        end digitValue
        
        on |λ|(accumulator, strDigit)
            accumulator + digitValue(strDigit)
        end |λ|
    end script
    
    foldl(digitAdded, 0, splitOn("", n as string))
end digitsSummed


-- TEST ----------------------------------------------------------------------
on run
    -- showDigitSum :: Int -> String
    script showDigitSum
        on |λ|(n)
            (n as string) & " -> " & digitsSummed(n)
        end |λ|
    end script
    
    intercalate(linefeed, ¬
        map(showDigitSum, [1, 1234, "254", "fe", "f0e", "999ABCXYZ"]))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

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

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set xs to text items of strMain
    set my text item delimiters to dlm
    return xs
end splitOn
```


{{Out}}

```txt
1 -> 1
1234 -> 10
254 -> 11
fe -> 29
f0e -> 29
999ABCXYZ -> 162
```



## ATS


```ATS

(* ****** ****** *)
//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o SumDigits SumDigits.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

extern
fun{a:t@ype}
SumDigits(n: a, base: int): a

implement
{a}(*tmp*)
SumDigits(n, base) = let
//
val base = gnumber_int(base)
//
fun
loop (n: a, res: a): a =
  if gisgtz_val<a> (n)
    then loop (gdiv_val<a>(n, base), gadd_val<a>(res, gmod_val<a>(n, base)))
    else res
//
in
  loop (n, gnumber_int(0))
end // end of [SumDigits]

(* ****** ****** *)

val SumDigits_int = SumDigits<int>

(* ****** ****** *)

implement
main0 () =
{
//
val n = 1
val () = println! ("SumDigits(1, 10) = ", SumDigits_int(n, 10))
val n = 12345
val () = println! ("SumDigits(12345, 10) = ", SumDigits_int(n, 10))
val n = 123045
val () = println! ("SumDigits(123045, 10) = ", SumDigits_int(n, 10))
val n = 0xfe
val () = println! ("SumDigits(0xfe, 16) = ", SumDigits_int(n, 16))
val n = 0xf0e
val () = println! ("SumDigits(0xf0e, 16) = ", SumDigits_int(n, 16))
//
} (* end of [main0] *)

```

{{out}}

```txt

SumDigits(1, 10) = 1
SumDigits(12345, 10) = 15
SumDigits(123045, 10) = 15
SumDigits(0xfe, 16) = 29
SumDigits(0xf0e, 16) = 29

```



## AutoHotkey


''Translated from the C version.''


```AutoHotkey
MsgBox % sprintf("%d %d %d %d %d`n"
	,SumDigits(1, 10)
	,SumDigits(12345, 10)
	,SumDigits(123045, 10)
	,SumDigits(0xfe, 16)
	,SumDigits(0xf0e, 16) )

SumDigits(n,base) {
	sum := 0
	while (n)
	{
		sum += Mod(n,base)
		n /= base
	}
	return sum
}

sprintf(s,fmt*) {
	for each, f in fmt
		StringReplace,s,s,`%d, % f
	return s
}
```

{{out}}

```txt
1 15 15 29 29
```



## AWK

MAWK only support base 10 numeric constants, so a conversion function is necessary.

Will sum digits in numbers from base 2 to base 16.

The output is in decimal. Output in other bases would require a function to do the conversion because MAWK's printf() does not support bases other than 10.

Other versions of AWK may not have these limitations.


```AWK
#!/usr/bin/awk -f

BEGIN {
    print sumDigits("1")
    print sumDigits("12")
    print sumDigits("fe")
    print sumDigits("f0e")
}

function sumDigits(num,    nDigs, digits, sum, d, dig, val, sum) {
    nDigs = split(num, digits, "")
    sum = 0
    for (d = 1; d <= nDigs; d++) {
        dig = digits[d]
        val = digToDec(dig)
        sum += val
    }
    return sum
}

function digToDec(dig) {
    return index("0123456789abcdef", tolower(dig)) - 1
}

```

{{out}}

```txt

 1
 3
 29
 29

```



## BASIC

{{works with|QBasic}}
{{works with|PowerBASIC}}
{{trans|Visual Basic}}

Note that in order for this to work with the Windows versions of PowerBASIC, the test code (the block at the end containing the PRINT lines) needs to be inside <code>FUNCTION PBMAIN</code>.


```qbasic
FUNCTION sumDigits(num AS STRING, bas AS LONG) AS LONG
    'can handle up to base 36
    DIM outp AS LONG
    DIM validNums AS STRING, tmp AS LONG, x AS LONG, lennum AS LONG, L0 AS LONG
    'ensure num contains only valid characters
    validNums = LEFT$("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", bas)
    lennum = LEN(num)
    FOR L0 = lennum TO 1 STEP -1
        x = INSTR(validNums, UCASE$(MID$(num, L0, 1))) - 1
        IF -1 = x THEN EXIT FUNCTION
        tmp = tmp + (x * (bas ^ (lennum - L0)))
    NEXT
    WHILE tmp
        outp = outp + (tmp MOD bas)
        tmp = tmp \ bas
    WEND
    sumDigits = outp
END FUNCTION

PRINT sumDigits(LTRIM$(STR$(1)), 10)
PRINT sumDigits(LTRIM$(STR$(1234)), 10)
PRINT sumDigits(LTRIM$(STR$(&HFE)), 16)
PRINT sumDigits(LTRIM$(STR$(&HF0E)), 16)
PRINT sumDigits("2", 2)
```


{{out}}

```txt

 1
 10
 11
 20
 0

```

See also: [[#BBC BASIC|BBC BASIC]], [[#Run BASIC|Run BASIC]], [[#Visual Basic|Visual Basic]]

=
## Applesoft BASIC
=

```ApplesoftBasic
10 BASE = 10
20 N$ = "1" : GOSUB 100 : PRINT N
30 N$ = "1234" : GOSUB 100 : PRINT N
40 BASE = 16
50 N$ = "FE" : GOSUB 100 : PRINT N
60 N$ = "F0E" : GOSUB 100 : PRINT N
90 END

100 REM SUM DIGITS OF N$, BASE
110 IF BASE = 1 THEN N = LEN(N$) : RETURN
120 IF BASE < 2 THEN BASE = 10
130 N = 0 : V$ = LEFT$("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", BASE)
140 FOR I = 1 TO LEN(N$) : C$ = MID$(N$, I, 1)
150     FOR J = 1 TO LEN(V$)
160         IF C$ <> MID$(V$, J, 1) THEN NEXT J : N = SQR(-1) : STOP
170     N = N + J - 1
180 NEXT I
190 RETURN
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This solution deliberately avoids MOD and DIV so it is not restricted to 32-bit integers.

```bbcbasic
      *FLOAT64
      PRINT "Digit sum of 1 (base 10) is "; FNdigitsum(1, 10)
      PRINT "Digit sum of 12345 (base 10) is "; FNdigitsum(12345, 10)
      PRINT "Digit sum of 9876543210 (base 10) is "; FNdigitsum(9876543210, 10)
      PRINT "Digit sum of FE (base 16) is "; ~FNdigitsum(&FE, 16) " (base 16)"
      PRINT "Digit sum of F0E (base 16) is "; ~FNdigitsum(&F0E, 16) " (base 16)"
      END
      
      DEF FNdigitsum(n, b)
      LOCAL q, s
      WHILE n <> 0
        q = INT(n / b)
        s += n - q * b
        n = q
      ENDWHILE
      = s
```

{{out}}

```txt

Digit sum of 1 (base 10) is 1
Digit sum of 12345 (base 10) is 15
Digit sum of 9876543210 (base 10) is 45
Digit sum of FE (base 16) is 1D (base 16)
Digit sum of F0E (base 16) is 1D (base 16)

```



## bc


```bc
define s(n) {
    auto i, o, s
    
    o = scale
    scale = 0

    for (i = n; i > 0; i /= ibase) {
        s += i % ibase
    }
    
    scale = o
    return(s)
}

ibase = 10
s(1)
s(1234)
ibase = 16
s(FE)
s(F0E)
```


{{Out}}

```txt
1
10
29
29
```



## Befunge


This solution reads the number and base as integers from stdin (in base 10). There doesn't seem any point in accepting input in other bases, because it would then have to be processed as a string and the base would be irrelevant, defeating the point of this exercise.


```befunge
" :rebmuN">:#,_&0v
|_,#!>#:<"Base: "<
<>10g+\00g/:v:p00&
v^\p01<%g00:_55+\>
>" :muS">:#,_$\.,@
```


{{out}}


```txt
Number: 1234
Base: 10
Sum: 10
```



## C


```c>#include <stdio.h


int SumDigits(unsigned long long n, const int base) {
    int sum = 0;
    for (; n; n /= base)
    	sum += n % base;
    return sum;
}
 
int main() {
    printf("%d %d %d %d %d\n",
        SumDigits(1, 10),
        SumDigits(12345, 10),
        SumDigits(123045, 10),
        SumDigits(0xfe, 16),
        SumDigits(0xf0e, 16) );
    return 0;
}
```

{{out}}

```txt
1 15 15 29 29
```


=={{header|C sharp|C#}}==

```csharp
namespace RosettaCode.SumDigitsOfAnInteger
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    internal static class Program
    {
        /// <summary>
        ///     Enumerates the digits of a number in a given base.
        /// </summary>
        /// <param name="number"> The number. </param>
        /// <param name="base"> The base. </param>
        /// <returns> The digits of the number in the given base. </returns>
        /// <remarks>
        ///     The digits are enumerated from least to most significant.
        /// </remarks>
        private static IEnumerable<int> Digits(this int number, int @base = 10)
        {
            while (number != 0)
            {
                int digit;
                number = Math.DivRem(number, @base, out digit);
                yield return digit;
            }
        }

        /// <summary>
        ///     Sums the digits of a number in a given base.
        /// </summary>
        /// <param name="number"> The number. </param>
        /// <param name="base"> The base. </param>
        /// <returns> The sum of the digits of the number in the given base. </returns>
        private static int SumOfDigits(this int number, int @base = 10)
        {
            return number.Digits(@base).Sum();
        }

        /// <summary>
        ///     Demonstrates <see cref="SumOfDigits" />.
        /// </summary>
        private static void Main()
        {
            foreach (var example in
                new[]
                {
                    new {Number = 1, Base = 10},
                    new {Number = 12345, Base = 10},
                    new {Number = 123045, Base = 10},
                    new {Number = 0xfe, Base = 0x10},
                    new {Number = 0xf0e, Base = 0x10}
                })
            {
                Console.WriteLine(example.Number.SumOfDigits(example.Base));
            }
        }
    }
}
```

{{out}}

```txt
1
15
15
29
29
```



## C++


```cpp>#include <iostream

#include <cmath>
int SumDigits(const unsigned long long int digits, const int BASE = 10) {
    int sum = 0;
    unsigned long long int x = digits;
    for (int i = log(digits)/log(BASE); i>0; i--){
        const double z = std::pow(BASE,i);
	  const unsigned long long int t = x/z;
	  sum += t;
	  x -= t*z;
    }
    return x+sum;
}

int main() {
        std::cout << SumDigits(1) << ' '
                  << SumDigits(12345) << ' '
                  << SumDigits(123045) << ' '
                  << SumDigits(0xfe, 16) << ' '
                  << SumDigits(0xf0e, 16) << std::endl;
        return 0;
}
```

{{out}}

```txt
1 15 15 29 29
```



### Template metaprogramming version

Tested with g++-4.6.3 (Ubuntu).

```cpp

// Template Metaprogramming version by Martin Ettl
#include <iostream>
#include <cmath>

typedef unsigned long long int T;
template <typename T, T i> void For(T &sum, T &x, const T &BASE)
{
    const double z(std::pow(BASE,i));
    const T t = x/z;
    sum += t;
    x -= t*z; 
    For<T, i-1>(sum,x,BASE);
}
template <> void For<T,0>(T &, T &, const T &){}

template <typename T, T digits, int BASE> T SumDigits()
 {
    T sum(0);
    T x(digits);
    const T end(log(digits)/log(BASE));
    For<T,end>(sum,x,BASE);
    return x+sum;
}

int main() 
{
        std::cout << SumDigits<T, 1     , 10>()  << ' '
                  << SumDigits<T, 12345 , 10>()  << ' '
                  << SumDigits<T, 123045, 10>()  << ' '
                  << SumDigits<T, 0xfe  , 16>()  << ' '
                  << SumDigits<T, 0xf0e , 16>()  << std::endl;
        return 0;
}

```

{{out}}

```txt
1 15 15 29 29
```



## Clojure


```clojure
(defn sum-digits [n base] 
  (let [number (if-not (string? n) (Long/toString n base) n)]
    (reduce + (map #(Long/valueOf (str %) base) number))))
```


{{out}}

```txt
user=> (sum-digits 1 10)
1
user=> (sum-digits 1234 10)
10
user=> (sum-digits "fe" 16)
29
user=> (sum-digits "f0e" 16)
29
user=> (sum-digits 254 16)
29
user=> (sum-digits 3854 16)
29
user=> (sum-digits 16rfe 16)
29
user=> (sum-digits 16rf0e 16)
29
user=> (sum-digits "clojure" 32)
147
```



## Common Lisp


```lisp
(defun sum-digits (number base)
  (loop for n = number then q
        for (q r) = (multiple-value-list (truncate n base))
        sum r until (zerop q)))
```

Example:

```lisp
(loop for (number base) in '((1 10) (1234 10) (#xfe 16) (#xf0e 16))
      do (format t "(~a)_~a = ~a~%" number base (sum-digits number base)))
```

{{out}}

```txt
(1)_10 = 1
(1234)_10 = 10
(254)_16 = 29
(3854)_16 = 29

```



## Crystal


```ruby
class String
  def sum_digits(base : Int) : Int32
  	self.chars.reduce(0) { |acc, c|
  		value = c.to_i(base)
  		acc += value
  	}
  end
end

puts("1".sum_digits 10)
puts("1234".sum_digits 10)
puts("fe".sum_digits 16)
puts("f0e".sum_digits 16)
```

{{out}}

```txt
1
10
29
29

```



## D


```d
import std.stdio, std.bigint;

uint sumDigits(T)(T n, in uint base=10) pure nothrow
in {
    assert(base > 1);
} body {
    typeof(return) total = 0;
    for ( ; n; n /= base)
        total += n % base;
    return total;
}

void main() {
    1.sumDigits.writeln;
    1_234.sumDigits.writeln;
    sumDigits(0xfe, 16).writeln;
    sumDigits(0xf0e, 16).writeln;
    1_234.BigInt.sumDigits.writeln;
}
```

{{out}}

```txt
1
10
29
29
10
```



## Dc


```d
[ I ~ S! d 0!=S L! + ] sS

1 lS x p
1234 lS x p
16 i
FE lS x p
F0E lS x p
```

{{out}}

```txt

1
10
29
29

```



## Elixir


```elixir
defmodule RC do
  def sumDigits(n, base\\10)
  def sumDigits(n, base) when is_integer(n) do
    Integer.digits(n, base) |> Enum.sum
  end
  def sumDigits(n, base) when is_binary(n) do
    String.codepoints(n) |> Enum.map(&String.to_integer(&1, base)) |> Enum.sum
  end
end

Enum.each([{1, 10}, {1234, 10}, {0xfe, 16}, {0xf0e, 16}], fn {n,base} ->
  IO.puts "#{Integer.to_string(n,base)}(#{base}) sums to #{ RC.sumDigits(n,base) }"
end)
IO.puts ""
Enum.each([{"1", 10}, {"1234", 10}, {"fe", 16}, {"f0e", 16}], fn {n,base} ->
  IO.puts "#{n}(#{base}) sums to #{ RC.sumDigits(n,base) }"
end)
```


{{out}}

```txt

1(10) sums to 1
1234(10) sums to 10
FE(16) sums to 29
F0E(16) sums to 29

1(10) sums to 1
1234(10) sums to 10
fe(16) sums to 29
f0e(16) sums to 29

```



## Emacs Lisp


```Emacs Lisp

(defun digit-sum (n)
  (apply '+
        (mapcar 'string-to-number
                (cdr (butlast (split-string (number-to-string n) "") )))))

(insert (format "%d\n" (digit-sum 1234) ))

```

<b>Output:</b>

```txt
 
10

```



## Erlang


```erlang

-module(sum_digits).
-export([sum_digits/2, sum_digits/1]).

sum_digits(N) ->
    sum_digits(N,10).

sum_digits(N,B) ->
    sum_digits(N,B,0).

sum_digits(0,_,Acc) ->
    Acc;
sum_digits(N,B,Acc) when N < B ->
    Acc+N;
sum_digits(N,B,Acc) ->
    sum_digits(N div B, B, Acc + (N rem B)).

```


Example usage:


```txt

2> sum_digits:sum_digits(1).
1
3> sum_digits:sum_digits(1234).
10
4> sum_digits:sum_digits(16#fe,16).
29
5> sum_digits:sum_digits(16#f0e,16).
29

```


## Ezhil


```Python

# இது ஒரு எழில் தமிழ் நிரலாக்க மொழி உதாரணம்

# sum of digits of a number
# எண்ணிக்கையிலான இலக்கங்களின் தொகை

நிரல்பாகம் எண்_கூட்டல்( எண் )
  தொகை = 0
  @( எண் > 0 ) வரை
     d = எண்%10;
     பதிப்பி "digit = ",d
     எண் = (எண்-d)/10;
     தொகை  = தொகை  + d
  முடி
  பின்கொடு தொகை 
முடி


பதிப்பி எண்_கூட்டல்( 1289)#20
பதிப்பி எண்_கூட்டல்( 123456789)# 45

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let digsum b n =
    let rec loop acc = function
        | n when n > 0 ->
            let m, r = Math.DivRem(n, b)
            loop (acc + r) m
        | _ -> acc
    loop 0 n

[<EntryPoint>]
let main argv =
    let rec show = function 
        | n :: b :: r -> printf " %d" (digsum b n); show r
        | _ -> ()

    show [1; 10; 1234; 10; 0xFE; 16; 0xF0E; 16]     // ->  1 10 29 29
    0
```



### or Generically

In order to complete the [[Digital root]] task I require a function which can handle numbers larger than 32 bit integers.

```fsharp

//Sum Digits of An Integer - Nigel Galloway: January 31st., 2015
//This code will work with any integer type
let inline sumDigits N BASE =
  let rec sum(g, n) = if n < BASE then n+g else sum(g+n%BASE, n/BASE)
  sum(LanguagePrimitives.GenericZero<_>,N)

```

{{out}}

```txt

> sumDigits 254 2;;
val it : int = 7
> sumDigits 254 10;;
val it : int = 11
> sumDigits 254 16;;
val it : int = 29
> sumDigits 254 23;;
val it : int = 12

```

so let's try it with a big integer

```txt

> sumDigits 123456789123456789123456789123456789123456789I 10I;;
val it : System.Numerics.BigInteger = 225 {IsEven = false;
                                           IsOne = false;
                                           IsPowerOfTwo = false;
                                           IsZero = false;
                                           Sign = 1;}

```


## Factor


```factor
: sum-digits ( base n -- sum ) 0 swap [ dup zero? ] [ pick /mod swapd + swap ] until drop nip ;

{ 10 10 16 16 } { 1 1234 0xfe 0xf0e } [ sum-digits ] 2each
```

{{out}}

```txt
--- Data stack:
1
10
29
29
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Sum_digits_of_an_integer this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

This is an easy task for Forth, that has built in support for radices up to 36. You set the radix by storing the value in variable BASE.

```forth
: sum_int 0 begin over while swap base @ /mod swap rot + repeat nip ;

 2 base ! 11110 sum_int decimal  . cr
10 base ! 12345 sum_int decimal  . cr
16 base ! f0e   sum_int decimal  . cr
```



## Fortran

Please find GNU/linux compilation instructions along with the sample output within the comments at the start of this FORTRAN 2008 source.  Thank you.  
Review of this page shows a solution to this task with the number input as text.  
The solution is the sum of index positions in an ordered list of digit characters. (awk).  
Other solutions ignore the representations of the input, encode digits using the base, then sum the encoding.  
Both methods appear in this implementation.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Fri Jun  7 21:00:12
!
!a=./f && make $a && $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!f.f08:57.29:
!
!  subroutine process1(fmt,s,b)
!                             1
!Warning: Unused dummy argument 'b' at (1)
!digit sum       n
!        1 1
!       10 1234
!       29 fe
!       29 f0e
! sum of digits of n expressed in base is...
!      n   base    sum
!      1     10      1
!   1234     10     10
!    254     16     29
!   3854     16     29
!
!Compilation finished at Fri Jun  7 21:00:12

module base_mod
  private :: reverse
contains
  subroutine reverse(a)
    integer, dimension(:), intent(inout) :: a
    integer :: i, j, t
    do i=1,size(a)/2
       j = size(a) - i + 1
       t = a(i)
       a(i) = a(j)
       a(j) = t
    end do
  end subroutine reverse  

  function antibase(b, n) result(a)
    integer, intent(in) :: b,n
    integer, dimension(32) :: a
    integer :: m, i
    a = 0
    m = n
    i = 1
    do while (m .ne. 0)
       a(i) = mod(m, b)
       m = m/b
       i = i+1
    end do
    call reverse(a)
  end function antibase
end module base_mod

program digit_sum
  use base_mod
  call still
  call confused
contains
  subroutine still
    character(len=6),parameter :: fmt = '(i9,a)'
    print'(a9,a8)','digit sum','n'
    call process1(fmt,'1',10)
    call process1(fmt,'1234',10)
    call process1(fmt,'fe',16)
    call process1(fmt,'f0e',16)
  end subroutine still

  subroutine process1(fmt,s,b)
    character(len=*), intent(in) :: fmt, s
    integer, intent(in), optional :: b
    integer :: i
    print fmt,sum((/(index('123456789abcdef',s(i:i)),i=1,len(s))/)),' '//s
  end subroutine process1

  subroutine confused
    character(len=5),parameter :: fmt = '(3i7)'
    print*,'sum of digits of n expressed in base is...'
    print'(3a7)','n','base','sum'
    call process0(10,1,fmt)
    call process0(10,1234,fmt)
    call process0(16,254,fmt)
    call process0(16,3854,fmt)
  end subroutine confused

  subroutine process0(b,n,fmt)
    integer, intent(in) :: b, n
    character(len=*), intent(in) :: fmt
    print fmt,n,b,sum(antibase(b, n))
  end subroutine process0
end program digit_sum

```



## FreeBASIC

{{trans|PureBasic}}

```freebasic
' FB 1.05.0 Win64

Function SumDigits(number As Integer, nBase As Integer) As Integer
  If number < 0 Then number = -number  ' convert negative numbers to positive
  If nBase < 2 Then nBase = 2   ' nBase can't be less than 2
  Dim As Integer sum = 0
  While number > 0
    sum += number Mod nBase
    number \= nBase
  Wend
  Return sum
End Function
 
Print "The sums of the digits are:"
Print
Print "1    base 10 :"; SumDigits(1, 10)
Print "1234 base 10 :"; SumDigits(1234, 10)
Print "fe   base 16 :"; SumDigits(&Hfe, 16)
Print "f0e  base 16 :"; SumDigits(&Hf0e, 16) 
Print
Print "Press any key to quit the program"
Sleep
```


{{out}}

```txt

The sums of the digits are:

1    base 10 : 1
1234 base 10 : 10
fe   base 16 : 29
f0e  base 16 : 29

```



## Go

Handling numbers up to 2^64-1 and bases from 2 to 36 is pretty easy, larger values can be handled using the <code>math/big</code> package (but it's still limited to base<=36).

```go
// File digit.go

package digit

import (
	"math/big"
	"strconv"
)

func SumString(n string, base int) (int, error) {
	i, ok := new(big.Int).SetString(n, base)
	if !ok {
		return 0, strconv.ErrSyntax
	}
	if i.Sign() < 0 {
		return 0, strconv.ErrRange
	}
	if i.BitLen() <= 64 {
		return Sum(i.Uint64(), base), nil
	}
	return SumBig(i, base), nil
}

func Sum(i uint64, base int) (sum int) {
	b64 := uint64(base)
	for ; i > 0; i /= b64 {
		sum += int(i % b64)
	}
	return
}

func SumBig(n *big.Int, base int) (sum int) {
	i := new(big.Int).Set(n)
	b := new(big.Int).SetUint64(uint64(base))
	r := new(big.Int)
	for i.BitLen() > 0 {
		i.DivMod(i, b, r)
		sum += int(r.Uint64())
	}
	return
}
```


```go
// File digit_test.go

package digit

import "testing"

type testCase struct {
	n    string
	base int
	dSum int
}

var testData = []testCase{
	{"1", 10, 1},
	{"1234", 10, 10},
	{"fe", 16, 29},
	{"f0e", 16, 29},
	{"18446744073709551615", 10, 87},
	{"abcdefghijklmnopqrstuvwzuz0123456789", 36, 628},
}

func TestSumString(t *testing.T) {
	for _, tc := range testData {
		ds, err := SumString(tc.n, tc.base)
		if err != nil {
			t.Error("test case", tc, err)
			continue
		}
		if ds != tc.dSum {
			t.Error("test case", tc, "got", ds, "expected", tc.dSum)
		}
	}
}

func TestErrors(t *testing.T) {
	for _, tc := range []struct {
		n    string
		base int
	}{
		{"1234", 37},
		{"0", 1},
		{"1234", 4},
		{"-123", 10},
	} {
		_, err := SumString(tc.n, tc.base)
		if err == nil {
			t.Error("expected error for", tc)
		}
		t.Log("got expected error:", err)
	}
}
```



## Groovy

Solution:

```groovy
def digitsum = { number, radix = 10 ->
    Integer.toString(number, radix).collect { Integer.parseInt(it, radix) }.sum()
}
```


Test:

```groovy
[[30, 2], [30, 10], [1, 10], [12345, 10], [123405, 10], [0xfe, 16], [0xf0e, 16]].each {
    println """
    Decimal value:     ${it[0]}
    Radix:             ${it[1]}
    Radix value:       ${Integer.toString(it[0], it[1])}
    Decimal Digit Sum: ${digitsum(it[0], it[1])}
    Radix Digit Sum:   ${Integer.toString(digitsum(it[0], it[1]), it[1])}
    """
}
```


{{out}}

```txt
    Decimal value:     30
    Radix:             2
    Radix value:       11110
    Decimal Digit Sum: 4
    Radix Digit Sum:   100
    

    Decimal value:     30
    Radix:             10
    Radix value:       30
    Decimal Digit Sum: 3
    Radix Digit Sum:   3
    

    Decimal value:     1
    Radix:             10
    Radix value:       1
    Decimal Digit Sum: 1
    Radix Digit Sum:   1
    

    Decimal value:     12345
    Radix:             10
    Radix value:       12345
    Decimal Digit Sum: 15
    Radix Digit Sum:   15
    

    Decimal value:     123405
    Radix:             10
    Radix value:       123405
    Decimal Digit Sum: 15
    Radix Digit Sum:   15
    

    Decimal value:     254
    Radix:             16
    Radix value:       fe
    Decimal Digit Sum: 29
    Radix Digit Sum:   1d
    

    Decimal value:     3854
    Radix:             16
    Radix value:       f0e
    Decimal Digit Sum: 29
    Radix Digit Sum:   1d
```



## Haskell


```haskell
digsum
  :: Integral a
  => a -> a -> a
digsum base = f 0
  where
    f a 0 = a
    f a n = f (a + r) q
      where
        (q, r) = n `quotRem` base

main :: IO ()
main = print $ digsum 16 255 -- "FF": 15 + 15 = 30
```

{{Out}}

```txt
30
```



Or, we could write '''sum . fmap digitToInt''', or the equivalent but more efficient fusion of it to a single fold: '''foldr ((+) . digitToInt) 0'''

```haskell
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.List (transpose, intersperse)
import Numeric (showIntAtBase, readInt)

digitSum :: String -> Int
digitSum = foldr ((+) . digitToInt) 0

intDigitSum :: Int -> Int -> Int
intDigitSum base n = digitSum (showIntAtBase base intToDigit n [])

-- TESTS ---------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  unwords <$>
  transpose
    ((fmap =<< flip justifyRight ' ' . succ . maximum . fmap length) <$>
     transpose
       ([ "Base"
        , "Digits"
        , "Value"
        , "digit string -> sum"
        , "integer value -> sum"
        ] :
        ((\(s, b) ->
             let v = readBase b s
             in [ show b -- base
                , show s -- digits
                , show v -- value
                , show (digitSum s) -- sum from digit string
                , show (intDigitSum b v) -- sum from base and value
                ]) <$>
         [("1", 10), ("1234", 10), ("fe", 16), ("f0e", 16)])))
  where
    justifyRight n c s = drop (length s) (replicate n c ++ s)
    readBase b s =
      let [(n, _)] = readInt b isHexDigit digitToInt s
      in n
```

{{Out}}

```txt
 Base  Digits  Value  digit string -> sum  integer value -> sum
   10     "1"      1                    1                     1
   10  "1234"   1234                   10                    10
   16    "fe"    254                   29                    29
   16   "f0e"   3854                   29                    29
```


=={{header|Icon}} and {{header|Unicon}}==

This solution works in both languages.  This solution assumes the input number
is expressed in the indicated base.  This assumption differs from that made in
some of the other solutions.


```unicon
procedure main(a)
    write(dsum(a[1]|1234,a[2]|10))
end

procedure dsum(n,b)
    n := integer((\b|10)||"r"||n)
    sum := 0
    while sum +:= (0 < n) % b do n /:= b
    return sum
end
```


Sample runs:


```txt

->sdi 1
1
->sdi 1234
10
->sdi fe 16
29
->sdi f0e 16
29
->sdi ff 16
30
->sdi 255 16
12
->sdi fffff 16
75
->sdi 254 16
11
->

```



## J



```j
digsum=: 10&$: : (+/@(#.inv))
```


Example use:


```J
   digsum 1234
10
   10 digsum 254
11
   16 digsum 254
29
```


Illustration of mechanics:


```j
   10 #. 1 2 3 4
1234
  10 #.inv 1234
1 2 3 4
  10 +/ 1 2 3 4
10
  10 +/@(#.inv) 1234
10
```


So #.inv gives us the digits, +/ gives us the sum, and @ glues them together with +/ being a "post processor" for #.inv or, as we say in the expression: (#.inv).  We need the parenthesis or inv will try to look up the inverse of +/@#. and that's not well defined.

The rest of it is about using 10 as the default left argument when no left argument is defined.  A J verb has a monadic definition (for use with one argument) and a dyadic definition (for use with two arguments) and : derives a new verb where the monadic definition is used from the verb on the left and the dyadic definition is used from the verb on the right.  $: is a self reference to the top-level defined verb.

Full examples:


```j
   digsum 1
1
   digsum 1234
10
   16 digsum 16bfe
29
   16 digsum 16bf0e
29
```


Note that J implements numeric types -- J tries to ensure that the semantics of numbers match their mathematical properties.  So it doesn't matter how we originally obtained a number.


```j
   200+54
254
   254
254
   2.54e2
254
   16bfe
254
   254b10 , 1r254b0.1  NB. 10 in base 254 , 0.1 in base 1/254
254 254
```



## Java


```java
import java.math.BigInteger;
public class SumDigits {
    public static int sumDigits(long num) {
	return sumDigits(num, 10);
    }
    public static int sumDigits(long num, int base) {
	String s = Long.toString(num, base);
	int result = 0;
	for (int i = 0; i < s.length(); i++)
	    result += Character.digit(s.charAt(i), base);
	return result;
    }
    public static int sumDigits(BigInteger num) {
	return sumDigits(num, 10);
    }
    public static int sumDigits(BigInteger num, int base) {
	String s = num.toString(base);
	int result = 0;
	for (int i = 0; i < s.length(); i++)
	    result += Character.digit(s.charAt(i), base);
	return result;
    }

    public static void main(String[] args) {
	System.out.println(sumDigits(1));
	System.out.println(sumDigits(12345));
	System.out.println(sumDigits(123045));
	System.out.println(sumDigits(0xfe, 16));
	System.out.println(sumDigits(0xf0e, 16));
	System.out.println(sumDigits(new BigInteger("12345678901234567890")));
    }
}
```

{{out}}

```txt

1
15
15
29
29
90

```


## JavaScript



### Imperative



```JavaScript
function sumDigits(n) {
	n += ''
	for (var s=0, i=0, e=n.length; i<e; i+=1) s+=parseInt(n.charAt(i),36)
	return s
}
for (var n of [1, 12345, 0xfe, 'fe', 'f0e', '999ABCXYZ']) document.write(n, ' sum to ', sumDigits(n), '
')

```

{{out}}

```txt

1 sum to 1
12345 sum to 15
254 sum to 11
fe sum to 29
f0e sum to 29
999ABCXYZ sum to 162

```


===Functional (ES 5)===


```JavaScript
(function () {
    'use strict';

    // digitsSummed :: (Int | String) -> Int
    function digitsSummed(number) {
    
        // 10 digits + 26 alphabetics
        // give us glyphs for up to base 36
        var intMaxBase = 36;
    
        return number
            .toString()
            .split('')
            .reduce(function (a, digit) { 
                return a + parseInt(digit, intMaxBase);
            }, 0);
    }

    // TEST

    return [1, 12345, 0xfe, 'fe', 'f0e', '999ABCXYZ']
        .map(function (x) {
            return x + ' -> ' + digitsSummed(x);
        })
        .join('\n');

})();

```




```txt
1 -> 1
12345 -> 15
254 -> 11
fe -> 29
f0e -> 29
999ABCXYZ -> 162
```



## jq

The following pipeline will have the desired effect if numbers and/or strings are presented as input:

```jq
tostring | explode | map(tonumber - 48) | add
```

For example:
```sh

$ jq -M 'tostring | explode | map(tonumber - 48) | add'
123
6
"123"
6
```


## Julia

Using the built-in <code>digits</code> function:

```julia
sumdigits(n, base=10) = sum(digits(n, base))
```



## Kotlin


```scala
// version 1.1.0

const val digits = "0123456789abcdefghijklmnopqrstuvwxyz"

fun sumDigits(ns: String, base: Int): Int {
    val n = ns.toLowerCase().trim()
    if (base !in 2..36) throw IllegalArgumentException("Base must be between 2 and 36")
    if (n.isEmpty())    throw IllegalArgumentException("Number string can't be blank or empty")
    var sum = 0
    for (digit in n) {
        val index = digits.indexOf(digit)
        if (index == -1 || index >= base) throw IllegalArgumentException("Number string contains an invalid digit")
        sum += index
    }
    return sum
}

fun main(args: Array<String>) {
    val numbers = mapOf("1" to 10, "1234" to 10, "fe" to 16, "f0e" to 16, "1010" to 2, "777" to 8, "16xyz" to 36)
    println("The sum of digits is:")
    for ((number, base) in numbers) println("$number\tbase $base\t-> ${sumDigits(number, base)}")
}
```


{{out}}

```txt

The sum of digits is:
1       base 10 -> 1
1234    base 10 -> 10
fe      base 16 -> 29
f0e     base 16 -> 29
1010    base 2  -> 2
777     base 8  -> 21
16xyz   base 36 -> 109

```



## Lasso


```Lasso>define br =
 '<br />\n'

define sumdigits(int, base = 10) => {
	fail_if(#base < 2, -1, 'Base need to be at least 2')
	local(
		out		= integer,
		divmod
	)
	while(#int) => {
		 #divmod = #int -> div(#base)
		 #int = #divmod -> first
		 #out += #divmod -> second
	}
	return #out
}

sumdigits(1)
br
sumdigits(12345)
br
sumdigits(123045)
br
sumdigits(0xfe, 16)
br
sumdigits(0xf0e, 16)
```

{{out}}

```txt
1
15
15
29
29
```



## Lingo


```lingo
on sum_digits (n, base)
  sum = 0
  repeat while n
    m = n / base
    sum = sum + n - m * base
    n = m
  end repeat
  return sum
end
```



```lingo
put sum_digits(1, 10)
-- 1
put sum_digits(1234, 10)
-- 10
put sum_digits(254, 16) -- 0xfe
-- 29
put sum_digits(3854, 16) -- 0xf0e
-- 29
```



## LiveCode


```LiveCode
function sumDigits n, base
    local numb
    if base is empty then put 10 into base
    repeat for each char d in n
        add baseConvert(d,base,10) to numb
    end repeat
    return numb
end sumDigits
```

Example

```LiveCode
put sumdigits(1,10) & comma & \
    sumdigits(1234,10) & comma & \
    sumdigits(fe,16) & comma & \
    sumdigits(f0e,16)
```
Output
```LiveCode
1,10,29,29
```



## Logo


```logo
make "digits "0123456789abcdefghijklmnopqrstuvwxyz

to digitvalue :digit
   output difference find [equal? :digit item ? :digits] iseq 1 count :digits 1
end

to sumdigits :number [:base 10]
  output reduce "sum map.se "digitvalue :number
end

foreach [1 1234 fe f0e] [print (se ? "-> sumdigits ?)]
```


{{Out}}

```txt
1 -> 1
1234 -> 10
fe -> 29
f0e -> 29
```



## Lua


```lua
function sum_digits(n, base)
    sum = 0
    while n > 0.5 do
        m = math.floor(n / base)
        digit = n - m * base
        sum = sum + digit
        n = m
    end
    return sum
end

print(sum_digits(1, 10))
print(sum_digits(1234, 10))
print(sum_digits(0xfe, 16))
print(sum_digits(0xf0e, 16))
```

{{out}}

```txt
1
10
29
29
```



## Maple


```maple
sumDigits := proc( num )
	local digits, number_to_string, i;
	number_to_string := convert( num, string );
	digits := [ seq( convert( h, decimal, hex ), h in seq( parse( i ) , i in number_to_string ) ) ];
	return add( digits ); 
end proc:
sumDigits( 1234 );
sumDigits( "fe" );
```

{{out}}

```txt

10
29

```



## Mathematica


```Mathematica
Total[IntegerDigits[1234]]
Total[IntegerDigits[16^^FE, 16]]
```

{{out}}

```txt
10
29
```


=={{header|МК-61/52}}==
<lang>П0	<->	П1	Сx	П2	ИП1	^	ИП0	/	[x]
П3	ИП0	*	-	ИП2	+	П2	ИП3	П1	x=0
05	ИП2	С/П
```



## ML

=
## mLite
=
Left in the to_radix even though not used in the solution.

```ocaml
exception :radix_out_of_range and :unknown_digit;

fun to_radix (0, radix, result) = implode result
           | (n, radix > 36, result) = raise :radix_out_of_range
           | (n rem radix > 10, radix, result) =
               to_radix (n div radix, radix,
                         chr (n rem radix + ord #"a" - 10) :: result)
           | (n, radix, result) =
               to_radix (n div radix, radix,
                         chr (n rem radix + ord #"0") :: result)
           | (n, radix) = to_radix (n, radix, [])
;
fun from_radix (s, radix) =
      let val digits = explode "0123456789abcdefghijklmnopqrstuvwxyz";
          val len_digits = len digits;
          fun index (_, n >= radix, c) = raise :unknown_digit
                  | (h :: t, n, c = h) = n
                  | (_ :: t, n, c) = index (t, n + 1, c)
                  | c = index (digits, 0, c)
          and conv ([], radix, power, n) = n
                 | (h :: t, radix, power, n) =
                     conv (t, radix, power * radix, index h * power + n)
                 | (s, radix) = conv (rev ` explode s, radix, 1, 0)
          in
            conv (s, radix)
          end

;
fun sumdig
		([], base, n) = n
	|	(h :: t, base, n) = sumdig (t, base, from_radix (implode [h], base) + n)
	|	(s, base) = sumdig (explode s, base, 0)

;
fun shosum (s, b) = (print "sum of digits of "; print s; print " (base "; print b; print ") = "; println ` sumdig (s, b))
;

shosum ("10fg",17);
shosum ("deadbeef",16);
shosum ("1101010101010101010101010101010101010101010101010101010101010101010101010101010101010101",2);
shosum ("thequickbrownfoxjumpsoverthelazydog",36);

```

Output

```txt
sum of digits of 10fg (base 17) = 32
sum of digits of deadbeef (base 16) = 104
sum of digits of 1101010101010101010101010101010101010101010101010101010101010101010101010101010101010101 (base 2) = 45
sum of digits of thequickbrownfoxjumpsoverthelazydog (base 36) = 788
```


=={{header|Modula-2}}==
{{trans|Pascal}}
{{works with|ADW Modula-2|any (Compile with the linker option ''Console Application'').}}

```modula2

MODULE SumOFDigits;
FROM STextIO IMPORT
  WriteString, WriteLn;
FROM SWholeIO IMPORT
  WriteInt;
FROM Conversions IMPORT
  StrBaseToLong;

PROCEDURE SumOfDigitBase(N: LONGCARD; Base: CARDINAL): CARDINAL;
VAR
  Tmp, LBase: LONGCARD;
  Digit, Sum : CARDINAL;
BEGIN
  Digit := 0;
  Sum   := 0;
  LBase := Base;
  WHILE N > 0 DO
    Tmp := N / LBase;
    Digit := N - LBase * Tmp;
    N := Tmp;
    INC(Sum, Digit);
  END;
  RETURN Sum;
END SumOfDigitBase;

VAR
  Num: LONGCARD;

BEGIN
  WriteString('   1 sums to '); 
  WriteInt(SumOfDigitBase(1, 10), 1); 
  WriteLn;
  WriteString('1234 sums to '); 
  WriteInt(SumOfDigitBase(1234, 10), 1); 
  WriteLn;
  IF StrBaseToLong('FE', 16, Num) THEN
    WriteString(' $FE sums to '); 
    WriteInt(SumOfDigitBase(Num, 16), 1); 
    WriteLn;
  END;
  IF StrBaseToLong('F0E', 16, Num) THEN
    WriteString('$F0E sums to '); 
    WriteInt(SumOfDigitBase(Num, 16), 1); 
    WriteLn;
  END;
  WriteString('MAX(LONGCARD) (in dec) sums to '); 
  WriteInt(SumOfDigitBase(MAX(LONGCARD), 10), 1); 
  WriteLn;
END SumOFDigits.

```

{{out}}

```txt

   1 sums to 1
1234 sums to 10
 $FE sums to 29
$F0E sums to 29
MAX(LONGCARD) (in dec) sums to 87

```



## NetRexx


### Strings

Processes data as text from the command line.  Provides a representative sample if no input is supplied:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg input
inputs = ['1234', '01234', '0xfe', '0xf0e', '0', '00', '0,2' '1', '070', '77, 8' '0xf0e, 10', '070, 16', '0xf0e, 36', '000999ABCXYZ, 36', 'ff, 16', 'f, 10', 'z, 37'] -- test data
if input.length() > 0 then inputs = [input] -- replace test data with user input
loop i_ = 0 to inputs.length - 1
  in = inputs[i_]
  parse in val . ',' base .
  dSum = sumDigits(val, base)
  say 'Sum of digits for integer "'val'" for a given base of "'base'":' dSum'\-'
  -- Carry the exercise to it's logical conclusion and sum the results to give a single digit in range 0-9
  loop while dSum.length() > 1 & dSum.datatype('n')
    dSum = sumDigits(dSum, 10)
    say ',' dSum'\-'
    end
  say
  end i_

-- Sum digits of an integer
method sumDigits(val = Rexx, base = Rexx '') public static returns Rexx

  rVal = 0
  parse normalizeValue(val, base) val base .
  loop label digs for val.length()
    -- loop to extract digits from input and sum them
    parse val dv +1 val
    do
      rVal = rVal + Integer.valueOf(dv.toString(), base).intValue()
    catch ex = NumberFormatException
      rVal = 'NumberFormatException:' ex.getMessage()
      leave digs
    end
    end digs
  return rVal

-- Clean up the input, normalize the data and determine which base to use
method normalizeValue(inV = Rexx, base = Rexx '') private static returns Rexx
  inV = inV.strip('l')
  base = base.strip()
  parse inV xpref +2 . -
         =0 opref +1 . -
         =0 . '0x' xval . ',' . -
         =0 . '0'  oval . ',' . -
         =0 dval .

  select
    when xpref = '0x' & base.length() = 0 then do
      -- value starts with '0x' and no base supplied.  Assign hex as base
      inval = xval
      base = 16
      end
    when opref = '0'  & base.length() = 0 then do
      -- value starts with '0' and no base supplied.  Assign octal as base
      inval = oval
      base = 8
      end
    otherwise do
      inval = dval
      end
    end
  if base.length() = 0 then base = 10 -- base not set.  Assign decimal as base
  if inval.length() <= 0 then inval = 0 -- boundary condition.  Invalid input or a single zero
  rVal = inval base

  return rVal

```

{{out}}

```txt

Sum of digits for integer "1234" for a given base of "": 10, 1
Sum of digits for integer "01234" for a given base of "": 10, 1
Sum of digits for integer "0xfe" for a given base of "": 29, 11, 2
Sum of digits for integer "0xf0e" for a given base of "": 29, 11, 2
Sum of digits for integer "0" for a given base of "": 0
Sum of digits for integer "00" for a given base of "": 0
Sum of digits for integer "0" for a given base of "2": 0
Sum of digits for integer "070" for a given base of "": 7
Sum of digits for integer "77" for a given base of "8": 14, 5
Sum of digits for integer "070" for a given base of "16": 7
Sum of digits for integer "0xf0e" for a given base of "36": 62, 8
Sum of digits for integer "000999ABCXYZ" for a given base of "36": 162, 9
Sum of digits for integer "ff" for a given base of "16": 30, 3
Sum of digits for integer "f" for a given base of "10": NumberFormatException: For input string: "f"
Sum of digits for integer "z" for a given base of "37": NumberFormatException: radix 37 greater than Character.MAX_RADIX

```



### Type <tt>int</tt>

Processes sample data as <tt>int</tt> arrays:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

inputs = [[int 1234, 10], [octal('01234'), 8], [0xfe, 16], [0xf0e,16], [8b0, 2], [16b10101100, 2], [octal('077'), 8]] -- test data
loop i_ = 0 to inputs.length - 1
  in = inputs[i_, 0]
  ib = inputs[i_, 1]
  dSum = sumDigits(in, ib)
  say 'Sum of digits for integer "'Integer.toString(in, ib)'" for a given base of "'ib'":' dSum'\-'
  -- Carry the exercise to it's logical conclusion and sum the results to give a single digit in range 0-9
  loop while dSum.length() > 1 & dSum.datatype('n')
    dSum = sumDigits(dSum, 10)
    say ',' dSum'\-'
    end
  say
  end i_

-- Sum digits of an integer
method sumDigits(val = int, base = int 10) public static returns Rexx
  rVal = Rexx 0
  sVal = Rexx(Integer.toString(val, base))
  loop label digs for sVal.length()
    -- loop to extract digits from input and sum them
    parse sVal dv +1 sVal
    do
      rVal = rVal + Integer.valueOf(dv.toString(), base).intValue()
    catch ex = NumberFormatException
      rVal = 'NumberFormatException:' ex.getMessage()
      leave digs
    end
    end digs
  return rVal

-- if there's a way to insert octal constants into an int in NetRexx I don't remember it
method octal(oVal = String) private constant returns int signals NumberFormatException
  iVal = Integer.valueOf(oVal, 8).intValue()
  return iVal

```

{{out}}

```txt

Sum of digits for integer "1234" for a given base of "10": 10, 1
Sum of digits for integer "1234" for a given base of "8": 10, 1
Sum of digits for integer "fe" for a given base of "16": 29, 11, 2
Sum of digits for integer "f0e" for a given base of "16": 29, 11, 2
Sum of digits for integer "0" for a given base of "2": 0
Sum of digits for integer "10101100" for a given base of "2": 4
Sum of digits for integer "77" for a given base of "8": 14, 5

```



## Never


```Never

func sum_digits(n : int, base : int) -> int {
    var sum = 0;
    
    do
    {
        sum = sum + n % base;
        n = n / base
    }
    while (n != 0);
    
    sum
}

func main() -> int {
    print(sum_digits(1, 10));
    print(sum_digits(12345, 10));
    print(sum_digits(123045, 10));
    print(sum_digits(0xfe, 16));
    print(sum_digits(0Xf0e, 16));
    
    0
}

```

{{output}}

```txt

1
15
15
29
29

```



## Nim


```nim
proc sumdigits(n, base: Natural): Natural =
  var n = n
  while n > 0:
    result += n mod base
    n = n div base

echo sumDigits(1, 10)
echo sumDigits(12345, 10)
echo sumDigits(123045, 10)
echo sumDigits(0xfe, 16)
echo sumDigits(0xf0e, 16)
```

{{out}}

```txt
1
15
15
29
29
```


=={{header|Oberon-2}}==

```oberon2

MODULE SumDigits;
IMPORT Out;
PROCEDURE Sum(n: LONGINT;base: INTEGER): LONGINT;
VAR
	sum: LONGINT;
BEGIN
	sum := 0;
	WHILE (n > 0) DO
		INC(sum,(n MOD base));
		n := n DIV base
	END;
	RETURN sum
END Sum;
BEGIN
	Out.String("1     : ");Out.LongInt(Sum(1,10),10);Out.Ln;
	Out.String("1234  : ");Out.LongInt(Sum(1234,10),10);Out.Ln;
	Out.String("0FEH  : ");Out.LongInt(Sum(0FEH,16),10);Out.Ln;
	Out.String("OF0EH : ");Out.LongInt(Sum(0F0EH,16),10);Out.Ln
END SumDigits.

```

{{out}}

```txt

1     :          1
1234  :         10
0FEH  :         29
OF0EH :         29

```



## OCaml



```ocaml
let sum_digits ~digits ~base =
  let rec aux sum x =
    if x <= 0 then sum else
    aux (sum + x mod base) (x / base)
  in
  aux 0 digits
 
let () =
  Printf.printf "%d %d %d %d %d\n"
    (sum_digits 1 10)
    (sum_digits 12345 10)
    (sum_digits 123045 10)
    (sum_digits 0xfe 16)
    (sum_digits 0xf0e 16)
```


{{out}}

```txt
1 15 15 29 29
```



## Oforth



```Oforth
: sumDigits(n, base)  0 while( n ) [ n base /mod ->n + ] ;
```


Usage :

```Oforth
sumDigits(1, 10) println
sumDigits(1234, 10) println
sumDigits(0xfe, 16) println
sumDigits(0xf0e, 16) println
```


{{out}}

```txt

1
10
29
29

```



## Ol


```scheme

(define (sum n base)
   (if (zero? n)
      n
      (+ (mod n base) (sum (div n base) base))))

(print (sum 1 10))
; ==> 1

(print (sum 1234 10))
; ==> 10

(print (sum #xfe 16))
; ==> 29

(print (sum #xf0e 16))
; ==> 29

```



## PARI/GP


```parigp
dsum(n,base)=my(s); while(n, s += n%base; n \= base); s
```


Also the built-in <code>sumdigits</code> can be used for base 10.


## Pascal


```pascal
Program SumOFDigits;

function SumOfDigitBase(n:UInt64;base:LongWord): LongWord;
var
  tmp: Uint64;
  digit,sum : LongWord;
Begin
  digit := 0;
  sum   := 0;
  While n > 0 do
  Begin
    tmp := n div base;
    digit := n-base*tmp;
    n := tmp;
    inc(sum,digit);
  end;
  SumOfDigitBase := sum;  
end;
Begin
  writeln('   1 sums to ', SumOfDigitBase(1,10)); 
  writeln('1234 sums to ', SumOfDigitBase(1234,10));  
  writeln(' $FE sums to ', SumOfDigitBase($FE,16)); 
  writeln('$FOE sums to ', SumOfDigitBase($F0E,16));   
  
  writeln('18446744073709551615 sums to ', SumOfDigitBase(High(Uint64),10));  

end.
```

;output:

```txt

   1 sums to 1
1234 sums to 10
 $FE sums to 29
$FOE sums to 29
18446744073709551615 sums to 87
```




## Perl


```Perl
#!/usr/bin/perl
use strict;
use warnings;

my %letval = map { $_ => $_ } 0 .. 9;
$letval{$_} = ord($_) - ord('a') + 10 for 'a' .. 'z';
$letval{$_} = ord($_) - ord('A') + 10 for 'A' .. 'Z';

sub sumdigits {
  my $number = shift;
  my $sum = 0;
  $sum += $letval{$_} for (split //, $number);
  $sum;
}

print "$_ sums to " . sumdigits($_) . "\n"
  for (qw/1 1234 1020304 fe f0e DEADBEEF/);
```

{{out}}
<PRE>1 sums to 1
1234 sums to 10
1020304 sums to 10
fe sums to 29
f0e sums to 29
DEADBEEF sums to 104</PRE>

The ntheory module also does this, for a solution similar to Perl 6, with identical output.{{libheader|ntheory}}

```Perl
use ntheory "sumdigits";
say sumdigits($_,36) for (qw/1 1234 1020304 fe f0e DEADBEEF/);
```



## Perl 6

This will handle input numbers in any base from 2 to 36. 
The results are in base 10.

```perl6
say Σ $_ for <1 1234 1020304 fe f0e DEADBEEF>;

sub Σ { [+] $^n.comb.map: { :36($_) } }
```

{{out}}

```txt
1
10
10
29
29
104
```



## Phix


```Phix
function sum_digits(integer n, integer base)
integer res = 0
    while n do
        res += remainder(n,base)
        n = floor(n/base)
    end while
    return res
end function

?sum_digits(1,10)
?sum_digits(1234,10)
?sum_digits(#FE,16)
?sum_digits(#F0E,16)
```

{{out}}

```txt

1
10
29
29

```



## PHP


```php
<?php
function sumDigits($num, $base = 10) {
    $s = base_convert($num, 10, $base);
    foreach (str_split($s) as $c)
        $result += intval($c, $base);
    return $result;
}
echo sumDigits(1), "\n";
echo sumDigits(12345), "\n";
echo sumDigits(123045), "\n";
echo sumDigits(0xfe, 16), "\n";
echo sumDigits(0xf0e, 16), "\n";
?>
```

{{out}}

```txt

1
15
15
29
29

```



## PicoLisp


```PicoLisp
(de sumDigits (N Base)
   (or
      (=0 N)
      (+ (% N Base) (sumDigits (/ N Base) Base)) ) )
```

Test:

```PicoLisp
: (sumDigits 1 10)
-> 1

: (sumDigits 1234 10)
-> 10

: (sumDigits (hex "fe") 16)
-> 29

: (sumDigits (hex "f0e") 16)
-> 29
```



## PL/I


```PL/I

sum_digits: procedure options (main);   /* 4/9/2012 */
   declare ch character (1);
   declare (k, sd) fixed;

   on endfile (sysin) begin; put skip data (sd); stop; end;
   sd = 0;
   do forever;
      get edit (ch) (a(1)); put edit (ch) (a);
      k = index('abcdef', ch);
      if k > 0 then /* we have a base above 10 */
         sd = sd + 9 + k;
      else
         sd = sd + ch;
   end;
end sum_digits;

```

results:

```txt

5c7e
SD=      38;
10111000001
SD=       5;

```



## PowerShell


```Powershell

function Get-DigitalSum ([string] $number, $base = 10)
{
    if ($number.ToCharArray().Length -le 1) { [Convert]::ToInt32($number, $base) }
    else 
    {
        $result = 0
        foreach ($character in $number.ToCharArray())
        {
            $digit = [Convert]::ToInt32(([string]$character), $base)
            $result += $digit
        }
        return $result
    }
}

```


{{out}}

```txt

PS C:\> Get-DigitalSum 1
1

PS C:\> Get-DigitalSum 1234
10

PS C:\> Get-DigitalSum fe 16
29

PS C:\> Get-DigitalSum f0e 16
29

```


=
## Alternative Implementation
=

```Powershell

function Get-DigitalSum ([string] $number, $base = 10)
{
    Invoke-Expression (($number.ToCharArray() | ForEach-Object {[string][convert]::ToInt16($_, $base)}) -join "+")
}

```


{{out}}

```txt

PS C:\> Get-DigitalSum 1
1

PS C:\> Get-DigitalSum 1234
10

PS C:\> Get-DigitalSum fe 16
29

PS C:\> Get-DigitalSum f0e 16
29

```



## PureBasic


```PureBasic

EnableExplicit

Procedure.i SumDigits(Number.q, Base)
  If Number < 0 : Number = -Number : EndIf; convert negative numbers to positive
  If Base < 2 : Base = 2 : EndIf ; base can't be less than 2
  Protected sum = 0
  While Number > 0
    sum + Number % Base
    Number / Base
  Wend
  ProcedureReturn sum
EndProcedure
  
If OpenConsole()
  PrintN("The sums of the digits are:")
  PrintN("")
  PrintN("1    base 10 : " + SumDigits(1, 10))
  PrintN("1234 base 10 : " + SumDigits(1234, 10))
  PrintN("fe   base 16 : " + SumDigits($fe, 16))
  PrintN("f0e  base 16 : " + SumDigits($f0e, 16)) 
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

The sums of the digits are:

1    base 10 : 1
1234 base 10 : 10
fe   base 16 : 29
f0e  base 16 : 29

```



## Python



```python

from numpy import base_repr

def sumDigits(num, base=10):
    return sum(int(x, base) for x in list(base_repr(num, base)))

```


or    


```python
def toBaseX(num, base):
    output = []
    while num:
        num, rem = divmod(num, base)
        output.append(rem)
    return output

def sumDigits(num, base=10):
    if base < 2:
        print "Error: Base must be at least 2"
        return
    return sum(toBaseX(num, base))

print sumDigits(1)
print sumDigits(12345)
print sumDigits(123045)
print sumDigits(0xfe, 16)
print sumDigits(0xf0e, 16)
```

{{out}}

```txt

1
15
15
29
29

```

The following does no error checking and requires non-base 10 numbers passed as string arguments:

```python

def sumDigits(num, base=10):
    return sum([int(x, base) for x in list(str(num))])

print sumDigits(1)
print sumDigits(12345)
print sumDigits(123045)
print sumDigits('fe', 16)
print sumDigits("f0e", 16)
```

Each digit is base converted as it's summed.


Or, as a composition of re-usable abstractions:

```python
from functools import (reduce)


# baseDigitSum :: Int -> Int -> Int
def baseDigitSum(base):
    return lambda n: sumFromDigitString(
        showIntAtBase(base)(
            intToDigit
        )(n)('')
    )


# sumFromDigitString :: String -> Int
def sumFromDigitString(s):
    return reduce(
        lambda a, c: a + digitToInt(c),
        list(s), 0
    )


# TESTS ---------------------------------------------------
def main():
    print('Base 10:')
    print (unlines(map(
        showfx('d', 'd')(
            baseDigitSum(10)
        ),
        [1, 1234, 1235, 123045]
    )))

    print('\nBase 16:')
    print (unlines(map(
        showfx('x', 'd')(
            baseDigitSum(16)
        ),
        [0xfe, 0xf0e]
    )))


# GENERIC -------------------------------------------------


# digitToInt :: Char -> Int
def digitToInt(c):
    oc = ord(c)
    if 48 > oc or 102 < oc:
        return None
    else:
        dec = oc - ord('0')
        hexu = oc - ord('A')
        hexl = oc - ord('a')
    return dec if 9 >= dec else (
        10 + hexu if 0 <= hexu and 5 >= hexu else (
            10 + hexl if 0 <= hexl and 5 >= hexl else None
        )
    )


# intToDigit :: Int -> Char
def intToDigit(n):
    return '0123456789ABCDEF'[n] if (
        n >= 0 and n < 16
    ) else '?'


# showfx :: (String, String) -> (a -> b) -> a -> String
def showfx(fmt, fmt2):
    return lambda f: lambda x: format(x, fmt) + (
        ' -> ' + format(f(x), fmt2)
    )


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    def wrap(toChr, n, rs):
        def go(nd, r):
            n, d = nd
            r_ = toChr(d) + r
            return go(divmod(n, base), r_) if 0 != n else r_
        return 'unsupported base' if 1 >= base else (
            'negative number' if 0 > n else (
                go(divmod(n, base), rs))
        )
    return lambda toChr: lambda n: lambda rs: (
        wrap(toChr, n, rs)
    )


# unlines :: [String] -> String
def unlines(xs):
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Base 10:
1 -> 1
1234 -> 10
1235 -> 11
123045 -> 15

Base 16:
fe -> 29
f0e -> 29
```



## R

{{trans|Python}}

```rsplus
change.base <- function(n, base)
{
  ret <- integer(as.integer(logb(x=n, base=base))+1L)
  
  for (i in 1:length(ret))
  {
    ret[i] <- n %% base
    n <- n %/% base
    
  }
  
  return(ret)
}

sum.digits <- function(n, base=10)
{
  if (base < 2)
    stop("base must be at least 2")
  
  return(sum(change.base(n=n, base=base)))
}

sum.digits(1)
sum.digits(12345)
sum.digits(123045)
sum.digits(0xfe, 16)
sum.digits(0xf0e, 16)
```





## Racket


```Racket
#lang racket
(define (sum-of-digits n base (sum 0))
  (if (= n 0)
      sum
      (sum-of-digits (quotient n base)
                     base
                     (+ (remainder n base) sum))))

(for-each
 (lambda (number-base-pair)
   (define number (car number-base-pair))
   (define base (cadr number-base-pair))
   (displayln (format "(~a)_~a = ~a" number base (sum-of-digits number base))))
 '((1 10) (1234 10) (#xfe 16) (#xf0e 16)))



;  outputs:
;    (1)_10 = 1
;    (1234)_10 = 10
;    (254)_16 = 29
;    (3854)_16 = 29
```



## REXX


### version 1


```rexx

/* REXX ************************************************************** 
* 04.12.2012 Walter Pachl                                               
**********************************************************************/ 
digits='0123456789ABCDEF'                                               
Do i=1 To length(digits)                                                
  d=substr(digits,i,1)                                                  
  value.d=i-1                                                           
  End                                                                   
Call test '1'                                                           
Call test '1234'                                                        
Call test 'FE'                                                          
Call test 'F0E'                                                         
Exit                                                                    
test:                                                                   
  Parse Arg number                                                      
  res=right(number,4)                                                   
  dsum=0                                                                
  Do While number<>''                                                   
    Parse Var number d +1 number                                        
    dsum=dsum+value.d                                                   
    End                                                                 
  Say res '->' right(dsum,2)                                            
  Return
```

{{out}}

```txt

   1 ->  1
1234 -> 10
  FE -> 29
 F0E -> 29

```



### version 2

This REXX version allows:
::*   leading signs   (<big>'''+ -'''</big>)
::*   decimal points
::*   leading and/or trailing whitespace
::*   numerals may be in mixed case
::*   numbers may include commas   (<big>''','''</big>)
::*   numbers may be expressed up to base 36
::*   numbers may be any length (size)

```rexx
/*REXX program  sums  the  decimal digits  of natural numbers in any base up to base 36.*/
parse arg z                                      /*obtain optional argument from the CL.*/
if z='' | z=","  then z= '1 1234 fe f0e +F0E -666.00 11111112222222333333344444449'
        do j=1  for words(z);     _=word(z, j)   /*obtain a number from the list.       */
        say right(sumDigs(_), 9)    ' is the sum of the digits for the number '    _
        end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumDigs: procedure;  arg x;  @=123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ;  $=0
                        do k=1  to length(x);   $=$ + pos( substr(x, k, 1), @);  end /*k*/
         return $
```

'''output'''   when using the default input:

```txt

        1  is the sum of the digits for the number  1
       10  is the sum of the digits for the number  1234
       29  is the sum of the digits for the number  fe
       29  is the sum of the digits for the number  f0e
       29  is the sum of the digits for the number  +F0E
       18  is the sum of the digits for the number  -666.00
       79  is the sum of the digits for the number  11111112222222333333344444449

```



### version 3

This REXX version is an optimized version limited to base ten integers only   (for fast decomposing of a decimal number's numerals).

The function makes use of REXX's   '''parse'''   statement

```rexx
/*REXX program  sums  the  decimal digits  of  integers  expressed in base ten.         */
parse arg z                                      /*obtain optional argument from the CL.*/
if z='' | z=","  then z=copies(7, 108)           /*let's generate a pretty huge integer.*/
numeric digits 1 + max( length(z) )              /*enable use of gigantic numbers.      */

     do j=1  for words(z);    _=abs(word(z, j))  /*ignore any leading sign,  if present.*/
     say sumDigs(_)      ' is the sum of the digits for the number '    _
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumDigs: procedure;  parse arg N 1 $ 2 ?         /*use first decimal digit for the sum. */
                             do  while ?\=='';  parse var ? _ 2 ?;  $=$+_;  end  /*while*/
         return $
```

'''output'''   when using the default input:

```txt

756  is the sum of the digits for the number  777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777

```



## Ring


```ring

see "sum digits of 1 = " + sumDigits(1) + nl
see "sum digits of 1234 = " + sumDigits(1234) + nl

func sumDigits n
     sum = 0
     while n > 0.5 
           m = floor(n / 10)
           digit = n - m * 10
           sum = sum + digit
           n = m
     end
     return sum

```



## Ruby



```ruby
def sum_digits(num, base = 10)
  num.digits(base).sum
end
```



## Rust


### Using an Iterator

This solution creates an iterator which yields the digits of a given number using a given base and then utilizes the `sum` method which is implemented automatically on iterators.

```rust
struct DigitIter(usize, usize);

impl Iterator for DigitIter {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            None
        } else {
            let ret = self.0 % self.1;
            self.0 /= self.1;
            Some(ret)
        }
    }
}

fn main() {
    println!("{}", DigitIter(1234,10).sum::<usize>());
}
```



## Scala


```scala
def sumDigits(x:BigInt, base:Int=10):BigInt=sumDigits(x.toString(base), base)
def sumDigits(x:String, base:Int):BigInt = x map(_.asDigit) sum
```

Test:

```scala
sumDigits(0)                                // => 0
sumDigits(0, 2)                             // => 0
sumDigits(0, 16)                            // => 0
sumDigits("00", 2)                          // => 0
sumDigits("00", 10)                         // => 0
sumDigits("00", 16)                         // => 0
sumDigits(1234)                             // => 10
sumDigits(0xfe)                             // => 11
sumDigits(0xfe, 16)                         // => 29
sumDigits(0xf0e, 16)                        // => 29
sumDigits(077)                              // => 9
sumDigits(077, 8)                           // => 14
sumDigits("077", 8)                         // => 14
sumDigits("077", 10)                        // => 14
sumDigits("077", 16)                        // => 14
sumDigits("0xf0e", 36)                      // => 62
sumDigits("000999ABCXYZ", 36)               // => 162
sumDigits(BigInt("12345678901234567890"))   // => 90
sumDigits("12345678901234567890", 10)       // => 90
```



## Scheme


This requires taking an input number (which may be input in any supported base), and a required target base to represent the number (as numbers entered in a given base do not preserve that base internally, and we may want to use unsupported bases).

The output is the sum of the digits in the target base, displayed in base 10.


```scheme

(import (scheme base)
        (scheme write))

;; convert number to a list of digits, in desired base
(define (number->list n base) 
  (let loop ((res '())
             (num n))
    (if (< num base) 
      (cons num res)
      (loop (cons (remainder num base) res)
            (quotient num base)))))

;; return the sum of digits of n in given base
(define (sum-digits n base)
  (apply + (number->list n base)))

;; test cases: 
;; -- this displays each number in its original, given-base, for comparison
;; -- target-base is the base in which to consider each number represented, for summing the digits
(define (test-case n given-base target-base)
  (display (string-append (number->string n given-base)
                          " base "
                          (number->string given-base)
                          " has decimal value "
                          (number->string n)
                          " => sum of digits in base "
                          (number->string target-base)
                          " is "
                          (number->string (sum-digits n target-base))))
  (newline))

(test-case 1 10 10)
(test-case 1234 10 10)
(test-case #o1234 8 10)
(test-case #xFE 16 16)
(test-case #xFE 16 10)
(test-case #xF0E 16 16)
(test-case #b1101010101010101010101010101010101 2 2)
(test-case #b1101010101010101010101010101010101 2 10)
(test-case #b1101010101010101010101010101010101 2 1000)

```


{{out}}

The final sum is always in base 10:


```txt

1 base 10 has decimal value 1 => sum of digits in base 10 is 1
1234 base 10 has decimal value 1234 => sum of digits in base 10 is 10
1234 base 8 has decimal value 668 => sum of digits in base 10 is 20
fe base 16 has decimal value 254 => sum of digits in base 16 is 29
fe base 16 has decimal value 254 => sum of digits in base 10 is 11
f0e base 16 has decimal value 3854 => sum of digits in base 16 is 29
1101010101010101010101010101010101 base 2 has decimal value 14316557653 => sum of digits in base 2 is 18
1101010101010101010101010101010101 base 2 has decimal value 14316557653 => sum of digits in base 10 is 46
1101010101010101010101010101010101 base 2 has decimal value 14316557653 => sum of digits in base 1000 is 1540

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: sumDigits (in var integer: num, in integer: base) is func
  result
     var integer: sum is 0;
  begin
    while num > 0 do
      sum +:= num rem base;
      num := num div base;
    end while;
  end func;

const proc: main is func
  begin
    writeln(sumDigits(1,      10));
    writeln(sumDigits(12345,  10));
    writeln(sumDigits(123045, 10));
    writeln(sumDigits(123045, 50));
    writeln(sumDigits(16#fe,  10));
    writeln(sumDigits(16#fe,  16));
    writeln(sumDigits(16#f0e, 16));
  end func;
```


{{out}}

```txt

1
15
15
104
11
29
29

```



## Sidef

{{trans|Perl 6}}

```ruby
func Σ(String str, base=36) {
    str.chars.map{ Num(_, base) }.sum
}

<1 1234 1020304 fe f0e DEADBEEF>.each { |n|
    say "Σ(#{n}) = #{Σ(n)}"
}
```

{{out}}

```txt

Σ(1) = 1
Σ(1234) = 10
Σ(1020304) = 10
Σ(fe) = 29
Σ(f0e) = 29
Σ(DEADBEEF) = 104

```



## Stata


```stata
function sumdigits(s) {
	a = ascii(strupper(s)):-48
	return(sum(a:-(a:>9)*7))
}

sumdigits("1")
  1

sumdigits("1234")
  10

sumdigits("fe")
  29

sumdigits("f0e")
  29

sumdigits(inbase(16, 254, 10))
  29
```



## Swift

{{works with|Swift|4.0}}

```Swift

extension String: Error {
    func sumDigits(withBase base: Int) throws -> Int {
        func characterToInt(_ base: Int) -> (Character) -> Int? {
            return { char in
                return Int(String(char), radix: base)
            }
        }
        
        return try self.map(characterToInt(base))
            .flatMap {
                guard $0 != nil else { throw "Invalid input" }
                return $0
            }
            .reduce(0, +)
    }
}

print(try! "1".sumDigits(withBase: 10))
print(try! "1234".sumDigits(withBase: 10))
print(try! "fe".sumDigits(withBase: 16))
print(try! "f0e".sumDigits(withBase: 16))

```

{{out}}

```txt

1
10
29
29

```



## Tcl

Supporting arbitrary bases makes this primarily a string operation.

```tcl
proc sumDigits {num {base 10}} {
    set total 0
    foreach d [split $num ""] {
	if {[string is alpha $d]} {
	    set d [expr {[scan [string tolower $d] %c] - 87}]
	} elseif {![string is digit $d]} {
	    error "bad digit: $d"
	}
	if {$d >= $base} {
	    error "bad digit: $d"
	}
	incr total $d
    }
    return $total
}
```

Demonstrating:

```tcl
puts [sumDigits 1]
puts [sumDigits 12345]
puts [sumDigits 123045]
puts [sumDigits fe 16]
puts [sumDigits f0e 16]
puts [sumDigits 000999ABCXYZ 36]
```

{{out}}

```txt

1
15
15
29
29
162

```



## Ursa

The function:

```ursa
def sumDigits (string val, int base)
	decl int ret
	for (decl int i) (< i (size val)) (inc i)
		set ret (+ ret (int val<i> base))
	end for
	return ret
end sumDigits
```


Calling the function: (This could be done on a single line, but it's split up for clarity.)

```ursa
out (sumDigits "1" 10) endl console
out (sumDigits "1234" 10) endl console
out (sumDigits "fe" 16) endl console
out (sumDigits "f0e" 16) endl console
```

{{out}}

```txt
1
10
29
29
```



## Visual Basic


This version checks that only valid digits for the indicated base are passed in, exiting otherwise.


```vb
Function sumDigits(num As Variant, base As Long) As Long
    'can handle up to base 36
    Dim outp As Long
    Dim validNums As String, tmp As Variant, x As Long, lennum As Long
    'ensure num contains only valid characters
    validNums = Left$("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", base)
    lennum = Len(num)
    For L0 = lennum To 1 Step -1
        x = InStr(validNums, Mid$(num, L0, 1)) - 1
        If -1 = x Then Exit Function
        tmp = tmp + (x * (base ^ (lennum - L0)))
    Next
    While tmp
        outp = outp + (tmp Mod base)
        tmp = tmp \ base
    Wend
    sumDigits = outp
End Function

Sub tester()
    Debug.Print sumDigits(1, 10)
    Debug.Print sumDigits(1234, 10)
    Debug.Print sumDigits(&HFE, 16)
    Debug.Print sumDigits(&HF0E, 16)
    Debug.Print sumDigits("2", 2)
End Sub
```


{{out}} (in the debug window):

```txt

 1
 10
 11
 20
 0

```



## XPL0


```XPL0
code    ChOut=8, CrLf=9, IntOut=11;

func    SumDigits(N, Base);
int     N, Base, Sum;
[Sum:= 0;
repeat  N:= N/Base;
        Sum:= Sum + rem(0);
until   N=0;
return Sum;
];

[IntOut(0, SumDigits(1,      10));  ChOut(0, ^ );
 IntOut(0, SumDigits(12345,  10));  ChOut(0, ^ );
 IntOut(0, SumDigits(123045, 10));  ChOut(0, ^ );
 IntOut(0, SumDigits($FE,    10));  ChOut(0, ^ );
 IntOut(0, SumDigits($FE,    16));  ChOut(0, ^ );
 IntOut(0, SumDigits($F0E,   16));  CrLf(0);
]
```


{{out}}

```txt

1 15 15 11 29 29

```



## zkl


```zkl
fcn sum(n,b=10){ 
   if(b==10) n.split().sum(0);  // digits to list
   else      n.toString(b).split("").apply("toInt",b).sum(0);
}
```

If not base 10, convert the int into a string (in the proper base, ie
0xfe-->"fe"), blow it apart into a list of digits/characters, convert
each character back into a int (from the base, ie ("c"/16-->12) and add
them up.
{{out}}

```txt

sum(1,10);     //--> 1
sum(1234,10);  //--> 10
sum(0xfe,16);  //--> 29
sum(0xf0e,16); //--> 29
sum(0b1101,2); //--> 3

```

