+++
title = "Find palindromic numbers in both binary and ternary bases"
description = ""
date = 2019-05-15T18:48:09Z
aliases = []
[extra]
id = 17454
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "c",
  "common_lisp",
  "csharp",
  "d",
  "elixir",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "mathematica",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "sidef",
  "tcl",
  "vba",
  "zkl",
]
+++

## Task

*   Find and show (in decimal) the first six numbers (non-negative integers) that are   [[Palindrome detection|palindromes]]   in   ''both'':
:::*   base 2
:::*   base 3
*   Display   '''0'''   (zero) as the first number found, even though some other definitions ignore it.
*   Optionally, show the decimal number found in its binary and ternary form.
*   Show all output here.


It's permissible to assume the first two numbers and simply list them.


## See also

*   [[oeis:A060792|Sequence A60792]],   numbers that are palindromic in bases 2 and 3 on ''The On-Line Encyclopedia of Integer Sequences''.





## Ada

===Simple Technique (Brute Force)===

```Ada
with Ada.Text_IO, Base_Conversion;

procedure Brute is

   type Long is range 0 .. 2**63-1;
   package BC is new Base_Conversion(Long);

   function Palindrome (S : String) return Boolean is
      (if S'Length < 2 then True
      elsif S(S'First) /= S(S'Last) then False
      else Palindrome(S(S'First+1 .. S'Last-1)));

   function Palindrome(N: Long; Base: Natural) return Boolean is
      (Palindrome(BC.Image(N, Base =>Base)));

  package IIO is new Ada.Text_IO.Integer_IO(Long);

begin
   for I in Long(1) .. 10**8 loop
      if Palindrome(I, 3) and then Palindrome(I, 2) then
	 IIO.Put(I, Width => 12); -- prints I (Base 10)
	 Ada.Text_IO.Put_Line(": " & BC.Image(I, Base => 2) & "(2)" &
			      ", " & BC.Image(I, Base => 3) & "(3)");
	 -- prints I (Base 2 and Base 3)
      end if;
      end loop;
end Brute;
```

```txt
           0: 0(2), 0(3)
           1: 1(2), 1(3)
        6643: 1100111110011(2), 100010001(3)
     1422773: 101011011010110110101(2), 2200021200022(3)
     5415589: 10100101010001010100101(2), 101012010210101(3)
```

For larger numbers, this is a bit slow.


### Advanced Technique

To speed this up, we directly generate palindromes to the base 3 and then check if these are also
palindromes to the base 2. We use the fact that these palindromes (to the base 3) must have an odd number of digits (to the base 2 and 3) and that the number in the middle is a 1 (to the base 3).
We use unsigned 64 bits integers that we consider as array of 64 bits thanks to a function Int_To_Bits (an instantiation of a generic conversion function) that goes from one type to the other immediately. We can now access the i'th bit of an Int in a very efficient way (since it is chosen by the compiler itself). This trick gives us a very efficient function to test if a number is a base-2 palindrome.

The code is then very fast and also very much readable than if we had done the bit manipulations by hand.


```Ada
with Ada.Text_IO, Ada.Unchecked_Conversion;use Ada.Text_IO;
procedure Palindromic is
  type Int is mod 2**64; -- the size of the unsigned values we will test doesn't exceed 64 bits

  type Bits is array (0..63) of Boolean;
    for Bits'Component_Size use 1;

  -- This function allows us to get the i'th bit of an Int k by writing Int_To_Bits(k)(i)
  function Int_To_Bits is new Ada.Unchecked_Conversion(Int,Bits);

  -- an inline function to test if k is palindromic in a very efficient way since we leave the loop
  -- as soon as two bits are not symmetric). Number_Of_Digits is the number of digits (in base 2) of k minus 1
  function Is_Pal2 (k : Int;Number_Of_Digits : Natural) return Boolean is
  (for all i in 0..Number_Of_Digits=>Int_To_Bits(k)(i)=Int_To_Bits(k)(Number_Of_Digits-i));

  function Reverse_Number (k : Int) return Int is --returns the symmetric representation of k (base-3)
    n : Int := 0;
    p : Int := k;
  begin
    while 0<p loop
      n := n * 3 + p mod 3;
      p := p / 3;
    end loop;
    return n;
  end reverse_number;

  procedure Print (n : Int) is
    package BC is new Ada.Text_IO.Modular_IO (Int); use BC; -- allows us to express a variable of modular type in a given base
  begin
     Put (n, Base=>2, Width=>65); Put (n, Base=>3, Width=>45); put_line (" " & n'Img);
  end Print;

  p3, n, bound, count_pal: Int := 1;
begin
  Print (0); -- because 0 is the special case asked to be treated, that is why count_pal=1
  Process_Each_Power_Of_4 : for p in 0..31 loop -- because 4^p < 2^64
    -- adjust the 3-power of the number to test so that the palindrome built with it has an odd number of digits in base-2
    while (3*p3+1)*p3 < 2**(2*p) loop p3 := 3*p3;end loop;
    bound := 2**(2*p)/(3*p3);
    for k in Int range Int'Max(p3/3, bound) .. Int'Min (2*bound,p3-1) loop
      n := (3*k+1)*p3 + Reverse_Number (k); -- n is a 2p+1 digits number in base 2 and is a palindrome in base 3.
      if Is_Pal2 (n, 2*p) then
        Print (n);
        count_pal := count_pal + 1;
        exit Process_Each_Power_Of_4 when count_pal = 7;
      end if;
    end loop;
  end loop Process_Each_Power_Of_4;
end Palindromic;
```

On a modern machine, (core i5 for example), this code, compiled with the -O3 and -gnatp options, takes less than 5 seconds to give the seven first palindromes smaller than 2^64.

```txt
                                                             2#0#                                         3#0#  0
                                                             2#1#                                         3#1#  1
                                                 2#1100111110011#                                 3#100010001#  6643
                                         2#101011011010110110101#                             3#2200021200022#  1422773
                                       2#10100101010001010100101#                           3#101012010210101#  5415589
                         2#1010100001100000100010000011000010101#                   3#22122022220102222022122#  90396755477
   2#10101001100110110110001110011011001110001101101100110010101#     3#2112200222001222121212221002220022112#  381920985378904469
```



## C

Per the observations made by the Ruby code (which are correct), the numbers must have odd number of digits in base 3 with a 1 at the middle, and must have odd number of digits in base 2.

```c
#include <stdio.h>
typedef unsigned long long xint;

int is_palin2(xint n)
{
	xint x = 0;
	if (!(n&1)) return !n;
	while (x < n) x = x<<1 | (n&1), n >>= 1;
	return n == x || n == x>>1;
}

xint reverse3(xint n)
{
	xint x = 0;
	while (n) x = x*3 + (n%3), n /= 3;
	return x;
}

void print(xint n, xint base)
{
	putchar(' ');
	// printing digits backwards, but hey, it's a palindrome
	do { putchar('0' + (n%base)), n /= base; } while(n);
	printf("(%lld)", base);
}

void show(xint n)
{
	printf("%llu", n);
	print(n, 2);
	print(n, 3);
	putchar('\n');
}

xint min(xint a, xint b) { return a < b ? a : b; }
xint max(xint a, xint b) { return a > b ? a : b; }

int main(void)
{
	xint lo, hi, lo2, hi2, lo3, hi3, pow2, pow3, i, n;
	int cnt;

	show(0);
	cnt = 1;

	lo = 0;
	hi = pow2 = pow3 = 1;

	while (1) {
		for (i = lo; i < hi; i++) {
			n = (i * 3 + 1) * pow3 + reverse3(i);
			if (!is_palin2(n)) continue;
			show(n);
			if (++cnt >= 7) return 0;
		}

		if (i == pow3)
			pow3 *= 3;
		else
			pow2 *= 4;

		while (1) {
			while (pow2 <= pow3) pow2 *= 4;

			lo2 = (pow2 / pow3 - 1) / 3;
			hi2 = (pow2 * 2 / pow3 - 1) / 3 + 1;
			lo3 = pow3 / 3;
			hi3 = pow3;

			if (lo2 >= hi3)
				pow3 *= 3;
			else if (lo3 >= hi2)
				pow2 *= 4;
			else {
				lo = max(lo2, lo3);
				hi = min(hi2, hi3);
				break;
			}
		}
	}
	return 0;
}
```

```txt
0 0(2) 0(3)
1 1(2) 1(3)
6643 1100111110011(2) 100010001(3)
1422773 101011011010110110101(2) 2200021200022(3)
5415589 10100101010001010100101(2) 101012010210101(3)
90396755477 1010100001100000100010000011000010101(2) 22122022220102222022122(3)
381920985378904469 10101001100110110110001110011011001110001101101100110010101(2) 2112200222001222121212221002220022112(3)
```



## C#

No strings involved. Ternary numbers (only of odd length and with a 1 in the middle) are generated by permutating powers of 3<br/>
and then checked to see if they are palindromic in binary.<br/>
The first 6 numbers take about 1/10th of a second. The 7th number takes about 3 and a half minutes.

```c#
using System;
using System.Collections.Generic;
using System.Linq;

public class FindPalindromicNumbers
{
    static void Main(string[] args)
    {
        var query =
            PalindromicTernaries()
            .Where(IsPalindromicBinary)
            .Take(6);
        foreach (var x in query) {
            Console.WriteLine("Decimal: " + x);
            Console.WriteLine("Ternary: " + ToTernary(x));
            Console.WriteLine("Binary: " + Convert.ToString(x, 2));
            Console.WriteLine();
        }
    }

    public static IEnumerable<long> PalindromicTernaries() {
        yield return 0;
        yield return 1;
        yield return 13;
        yield return 23;

        var f = new List<long> {0};
        long fMiddle = 9;
        while (true) {
            for (long edge = 1; edge < 3; edge++) {
                int i;
                do {
                    //construct the result
                    long result = fMiddle;
                    long fLeft = fMiddle * 3;
                    long fRight = fMiddle / 3;
                    for (int j = f.Count - 1; j >= 0; j--) {
                        result += (fLeft + fRight) * f[j];
                        fLeft *= 3;
                        fRight /= 3;
                    }
                    result += (fLeft + fRight) * edge;
                    yield return result;

                    //next permutation
                    for (i = f.Count - 1; i >= 0; i--) {
                        if (f[i] == 2) {
                            f[i] = 0;
                        } else {
                            f[i]++;
                            break;
                        }
                    }
                } while (i >= 0);
            }
            f.Add(0);
            fMiddle *= 3;
        }
    }

    public static bool IsPalindromicBinary(long number) {
        long n = number;
        long reverse = 0;
        while (n != 0) {
            reverse <<= 1;
            if ((n & 1) == 1) reverse++;
            n >>= 1;
        }
        return reverse == number;
    }

    public static string ToTernary(long n)
    {
        if (n == 0) return "0";
        string result = "";
        while (n > 0) {        {
            result = (n % 3) + result;
            n /= 3;
        }
        return result;
    }

}
```

```txt
Decimal: 0
Ternary: 0
Binary: 0

Decimal: 1
Ternary: 1
Binary: 1

Decimal: 6643
Ternary: 100010001
Binary: 1100111110011

Decimal: 1422773
Ternary: 2200021200022
Binary: 101011011010110110101

Decimal: 5415589
Ternary: 101012010210101
Binary: 10100101010001010100101

Decimal: 90396755477
Ternary: 22122022220102222022122
Binary: 1010100001100000100010000011000010101
```


## Common Lisp

Unoptimized version

```lisp
(defun palindromep (str)
  (string-equal str (reverse str)) )

(loop
  for i from 0
  with results = 0
  until (>= results 6)
  do
    (when (and (palindromep (format nil "~B" i))
               (palindromep (format nil "~3R" i)) )
      (format t "n:~a~:*  [2]:~B~:*  [3]:~3R~%" i)
      (incf results) ))
```

```txt
n:0  [2]:0  [3]:0
n:1  [2]:1  [3]:1
n:6643  [2]:1100111110011  [3]:100010001
n:1422773  [2]:101011011010110110101  [3]:2200021200022
n:5415589  [2]:10100101010001010100101  [3]:101012010210101
n:90396755477 [2]:1010100001100000100010000011000010101 [3]:22122022220102222022122
n:381920985378904469 [2]:10101001100110110110001110011011001110001101101100110010101 [3]:2112200222001222121212221002220022112
```



## D

```d
import core.stdc.stdio, std.ascii;

bool isPalindrome2(ulong n) pure nothrow @nogc @safe {
    ulong x = 0;
    if (!(n & 1))
        return !n;
    while (x < n) {
        x = (x << 1) | (n & 1);
        n >>= 1;
    }
    return n == x || n == (x >> 1);
}

ulong reverse3(ulong n) pure nothrow @nogc @safe {
    ulong x = 0;
    while (n) {
        x = x * 3 + (n % 3);
        n /= 3;
    }
    return x;
}

void printReversed(ubyte base)(ulong n) nothrow @nogc {
    ' '.putchar;
    do {
        digits[n % base].putchar;
        n /= base;
    } while(n);

    printf("(%d)", base);
}

void main() nothrow @nogc {
    ulong top = 1, mul = 1, even = 0;
    uint count = 0;

    for (ulong i = 0; true; i++) {
        if (i == top) {
            if (even ^= 1)
                top *= 3;
            else {
                i = mul;
                mul = top;
            }
        }

        immutable n = i * mul + reverse3(even ? i / 3 : i);

        if (isPalindrome2(n)) {
            printf("%llu", n);
            printReversed!3(n);
            printReversed!2(n);
            '\n'.putchar;

            if (++count >= 6) // Print first 6.
                break;
        }
    }
}
```

```txt
0 0(3) 0(2)
1 1(3) 1(2)
6643 100010001(3) 1100111110011(2)
1422773 2200021200022(3) 101011011010110110101(2)
5415589 101012010210101(3) 10100101010001010100101(2)
90396755477 22122022220102222022122(3) 1010100001100000100010000011000010101(2)
```



## Elixir

```elixir
defmodule Palindromic do
  import Integer, only: [is_odd: 1]

  def number23 do
    Stream.concat([0,1], Stream.unfold(1, &number23/1))
  end
  def number23(i) do
    n3 = Integer.to_string(i,3)
    n = (n3 <> "1" <> String.reverse(n3)) |> String.to_integer(3)
    n2 = Integer.to_string(n,2)
    if is_odd(String.length(n2)) and n2 == String.reverse(n2),
      do:   {n, i+1},
      else: number23(i+1)
  end

  def task do
    IO.puts "     decimal          ternary                          binary"
    number23()
    |> Enum.take(6)
    |> Enum.each(fn n ->
      n3 = Integer.to_charlist(n,3) |> :string.centre(25)
      n2 = Integer.to_charlist(n,2) |> :string.centre(39)
      :io.format "~12w ~s ~s~n", [n, n3, n2]
    end)
  end
end

Palindromic.task
```

```txt
     decimal          ternary                          binary
           0             0                                0
           1             1                                1
        6643         100010001                      1100111110011
     1422773       2200021200022                101011011010110110101
     5415589      101012010210101              10100101010001010100101
 90396755477  22122022220102222022122   1010100001100000100010000011000010101
```


=={{header|F_Sharp|F#}}==

```fsharp

// Find palindromic numbers in both binary and ternary bases. December 19th., 2018
let fG(n,g)=(Seq.unfold(fun(g,e)->if e<1L then None else Some((g%3L)*e,(g/3L,e/3L)))(n,g/3L)|>Seq.sum)+g+n*g*3L
Seq.concat[seq[0L;1L;2L];Seq.unfold(fun(i,e)->Some (fG(i,e),(i+1L,if i=e-1L then e*3L else e)))(1L,3L)]
  |>Seq.filter(fun n->let n=System.Convert.ToString(n,2).ToCharArray() in n=Array.rev n)|>Seq.take 6|>Seq.iter (printfn "%d")

```

Finding 6 takes no time.

```txt

0
1
6643
1422773
5415589
90396755477
Real: 00:00:00.482, CPU: 00:00:00.490, GC gen0: 77, gen1: 0

```

Finding 7 takes a little longer.

```txt

0
1
6643
1422773
5415589
90396755477
381920985378904469
Real: 00:23:09.114, CPU: 00:23:26.430, GC gen0: 209577, gen1: 1

```


## Factor

This implementation uses the methods for reducing the search space discussed in the Ruby example.

```factor
USING: combinators.short-circuit formatting io kernel lists
lists.lazy literals math math.parser sequences tools.time ;
IN: rosetta-code.2-3-palindromes

CONSTANT: info $[
    "The first 6 numbers which are palindromic in both binary "
    "and ternary:" append
]

: expand ( n -- m ) 3 >base dup <reversed> "1" glue 3 base> ;

: 2-3-pal? ( n -- ? )
    expand >bin
    { [ length odd? ] [ dup <reversed> sequence= ] } 1&& ;

: first6 ( -- seq )
    4 0 lfrom [ 2-3-pal? ] lfilter ltake list>array
    [ expand ] map { 0 1 } prepend ;

: main ( -- )
    info print nl first6 [
        dup [ >bin ] [ 3 >base ] bi
        "Decimal : %d\nBinary  : %s\nTernary : %s\n\n" printf
    ] each ;

[ main ] time
```

```txt
The first 6 numbers which are palindromic in both binary and ternary:

Decimal : 0
Binary  : 0
Ternary : 0

Decimal : 1
Binary  : 1
Ternary : 1

Decimal : 6643
Binary  : 1100111110011
Ternary : 100010001

Decimal : 1422773
Binary  : 101011011010110110101
Ternary : 2200021200022

Decimal : 5415589
Binary  : 10100101010001010100101
Ternary : 101012010210101

Decimal : 90396755477
Binary  : 1010100001100000100010000011000010101
Ternary : 22122022220102222022122

Running time: 0.555949118 seconds
```



## FreeBASIC

As using a brute force approach will be too slow for this task we instead create ternary palindromes
and check if they are also binary palindromes using the optimizations which have been noted in some
of the other language solutions :

```freebasic
' FB 1.05.0 Win64

'converts decimal "n" to its ternary equivalent
Function Ter(n As UInteger) As String
  If n = 0 Then Return "0"
  Dim result As String = ""
  While n > 0
    result = (n Mod 3) & result
    n \= 3
  Wend
  Return result
End Function

' check if a binary or ternary numeric string "s" is palindromic
Function isPalindromic(s As String) As Boolean
  ' we can assume "s" will have an odd number of digits, so can ignore the middle digit
  Dim As UInteger length = Len(s)
  For i As UInteger = 0 To length \ 2 - 1
    If s[i] <> s[length - 1 - i] Then Return False
  Next
  Return True
End Function

' print a number which is both a binary and ternary palindrome in all three bases
Sub printPalindrome(n As UInteger)
  Print "Decimal : "; Str(n)
  Print "Binary  : "; bin(n)
  Print "Ternary : "; ter(n)
  Print
End Sub

' create a ternary palindrome whose left part is the ternary equivalent of "n" and return its decimal equivalent
Function createPalindrome3(n As UInteger) As UInteger
  Dim As String ternary = Ter(n)
  Dim As UInteger power3 = 1, sum = 0, length = Len(ternary)
  For i As Integer = 0 To Length - 1 ''right part of palindrome is mirror image of left part
    If ternary[i] > 48 Then  '' i.e. non-zero
      sum += (ternary[i] - 48) * power3
    End If
    power3 *= 3
  Next
  sum += power3 '' middle digit must be 1
  power3 *= 3
  sum += n * power3  '' value of left part is simply "n" multiplied by appropriate power of 3
  Return sum
End Function

Dim t As Double = timer
Dim As UInteger i = 1, p3, count = 2
Dim As String binStr
Print "The first 6 numbers which are palindromic in both binary and ternary are :"
Print
' we can assume the first two palindromic numbers as per the task description
printPalindrome(0) '' 0 is a palindrome in all 3 bases
printPalindrome(1) '' 1 is a palindrome in all 3 bases
Do
  p3 = createPalindrome3(i)
  If p3 Mod 2 > 0 Then ' cannot be even as binary equivalent would end in zero
    binStr = Bin(p3)  '' Bin function is built into FB
    If Len(binStr) Mod 2 = 1 Then  '' binary palindrome must have an odd number of digits
      If isPalindromic(binStr) Then
        printPalindrome(p3)
        count += 1
      End If
    End If
  End If
  i += 1
Loop Until count = 6
Print "Took ";
Print Using "#.###"; timer - t;
Print " seconds on i3 @ 2.13 GHz"
Print "Press any key to quit"
Sleep
```

```txt
The first 6 numbers which are palindromic in both binary and ternary are :

Decimal : 0
Binary  : 0
Ternary : 0

Decimal : 1
Binary  : 1
Ternary : 1

Decimal : 6643
Binary  : 1100111110011
Ternary : 100010001

Decimal : 1422773
Binary  : 101011011010110110101
Ternary : 2200021200022

Decimal : 5415589
Binary  : 10100101010001010100101
Ternary : 101012010210101

Decimal : 90396755477
Binary  : 1010100001100000100010000011000010101
Ternary : 22122022220102222022122

Took 0.761 seconds on i3 @ 2.13 GHz
```



## Go

On my modest machine (Intel Celeron @1.6ghz) this takes about 30 seconds to produce the 7th palindrome. Curiously, the C version (GCC 5.4.0, -O3) takes about 55 seconds on the same machine. As it's a faithful translation, I have no idea why.

```go
package main

import (
    "fmt"
    "strconv"
    "time"
)

func isPalindrome2(n uint64) bool {
    x := uint64(0)
    if (n & 1) == 0 {
        return n == 0
    }
    for x < n {
        x = (x << 1) | (n & 1)
        n >>= 1
    }
    return n == x || n == (x>>1)
}

func reverse3(n uint64) uint64 {
    x := uint64(0)
    for n != 0 {
        x = x*3 + (n % 3)
        n /= 3
    }
    return x
}

func show(n uint64) {
    fmt.Println("Decimal :", n)
    fmt.Println("Binary  :", strconv.FormatUint(n, 2))
    fmt.Println("Ternary :", strconv.FormatUint(n, 3))
    fmt.Println("Time    :", time.Since(start))
    fmt.Println()
}

func min(a, b uint64) uint64 {
    if a < b {
        return a
    }
    return b
}

func max(a, b uint64) uint64 {
    if a > b {
        return a
    }
    return b
}

var start time.Time

func main() {
    start = time.Now()
    fmt.Println("The first 7 numbers which are palindromic in both binary and ternary are :\n")
    show(0)
    cnt := 1
    var lo, hi, pow2, pow3 uint64 = 0, 1, 1, 1
    for {
        i := lo
        for ; i < hi; i++ {
            n := (i*3+1)*pow3 + reverse3(i)
            if !isPalindrome2(n) {
                continue
            }
            show(n)
            cnt++
            if cnt >= 7 {
                return
            }
        }

        if i == pow3 {
            pow3 *= 3
        } else {
            pow2 *= 4
        }

        for {
            for pow2 <= pow3 {
                pow2 *= 4
            }

            lo2 := (pow2/pow3 - 1) / 3
            hi2 := (pow2*2/pow3-1)/3 + 1
            lo3 := pow3 / 3
            hi3 := pow3

            if lo2 >= hi3 {
                pow3 *= 3
            } else if lo3 >= hi2 {
                pow2 *= 4
            } else {
                lo = max(lo2, lo3)
                hi = min(hi2, hi3)
                break
            }
        }
    }
}
```


Sample run:

```txt
The first 7 numbers which are palindromic in both binary and ternary are :

Decimal : 0
Binary  : 0
Ternary : 0
Time    : 1.626245ms

Decimal : 1
Binary  : 1
Ternary : 1
Time    : 3.076839ms

Decimal : 6643
Binary  : 1100111110011
Ternary : 100010001
Time    : 4.026575ms

Decimal : 1422773
Binary  : 101011011010110110101
Ternary : 2200021200022
Time    : 5.014413ms

Decimal : 5415589
Binary  : 10100101010001010100101
Ternary : 101012010210101
Time    : 5.949399ms

Decimal : 90396755477
Binary  : 1010100001100000100010000011000010101
Ternary : 22122022220102222022122
Time    : 24.878073ms

Decimal : 381920985378904469
Binary  : 10101001100110110110001110011011001110001101101100110010101
Ternary : 2112200222001222121212221002220022112
Time    : 30.090048188s
```



## Haskell


```haskell
import Data.List (transpose, unwords)
import Numeric (showIntAtBase, readInt)
import Data.Char (intToDigit, isDigit, digitToInt)

-- Member of ternary palindrome series.
base3Palindrome :: Integer -> String
base3Palindrome n =
  let s = showBase 3 n
  in s ++ ('1' : reverse s)

-- Test for binary palindrome.
isBinPal :: Integer -> Bool
isBinPal n =
  let s = showIntAtBase 2 intToDigit n []
      (q, r) = quotRem (length s) 2
  in (r == 1) && drop (succ q) s == reverse (take q s)

-- Integer value of ternary string.
readBase3 :: String -> Integer
readBase3 = fst . head . readInt 3 isDigit digitToInt

showBase :: Integer -> Integer -> String
showBase base n = showIntAtBase base intToDigit n []

solutions :: [Integer]
solutions =
  0 : 1 : take 4 (filter isBinPal ((readBase3 . base3Palindrome) <$> [1 ..]))

main :: IO ()
main =
  mapM_
    putStrLn
    (unwords <$>
     transpose
       ((fmap =<< flip justifyLeft ' ' . succ . maximum . fmap length) <$>
        transpose
          (["Decimal", "Ternary", "Binary"] :
           fmap ((<*>) [show, showBase 3, showBase 2] . return) solutions)))
  where
    justifyLeft n c s = take n (s ++ replicate n c)
```

```txt
Decimal      Ternary                  Binary
0            0                        0
1            1                        1
6643         100010001                1100111110011
1422773      2200021200022            101011011010110110101
5415589      101012010210101          10100101010001010100101
90396755477  22122022220102222022122  1010100001100000100010000011000010101
```



## J

'''Solution:'''

```j
isPalin=: -: |.                          NB. check if palindrome
toBase=: #.inv"0                         NB. convert to base(s) in left arg
filterPalinBase=: ] #~ isPalin@toBase/   NB. palindromes for base(s)
find23Palindromes=: 3 filterPalinBase 2 filterPalinBase ]  NB. palindromes in both base 2 and base 3

showBases=: [: ;:inv@|: <@({&'0123456789ABCDEFGH')@toBase/ NB. display numbers in bases

NB.*getfirst a Adverb to get first y items returned by verb u
getfirst=: adverb define
  100000x u getfirst y
:
  res=. 0$0
  start=. 0
  blk=. i.x
  whilst. y > #res do.
    tmp=. u start + blk
    start=. start + x
    res=. res, tmp
  end.
  y{.res
)
```

'''Usage:'''

```j
   find23Palindromes i. 2e6                       NB. binary & ternary palindromes less than 2,000,000
0 1 6643 1422773
   10 2 3 showBases find23Palindromes getfirst 6  NB. first 6 binary & ternary palindomes
0 0 0
1 1 1
6643 1100111110011 100010001
1422773 101011011010110110101 2200021200022
5415589 10100101010001010100101 101012010210101
90396755477 1010100001100000100010000011000010101 22122022220102222022122
```



## Java

This takes a while to get to the 6th one (I didn't time it precisely, but it was less than 2 hours on an i7)

```java
public class Pali23 {
	public static boolean isPali(String x){
		return x.equals(new StringBuilder(x).reverse().toString());
	}

	public static void main(String[] args){

		for(long i = 0, count = 0; count < 6;i++){
			if((i & 1) == 0 && (i != 0)) continue; //skip non-zero evens, nothing that ends in 0 in binary can be in this sequence
			//maybe speed things up through short-circuit evaluation by putting toString in the if
			//testing up to 10M, base 2 has slightly fewer palindromes so do that one first
			if(isPali(Long.toBinaryString(i)) && isPali(Long.toString(i, 3))){
				System.out.println(i + ", " + Long.toBinaryString(i) + ", " + Long.toString(i, 3));
				count++;
			}
		}
	}
}
```

```txt
0, 0, 0
1, 1, 1
6643, 1100111110011, 100010001
1422773, 101011011010110110101, 2200021200022
5415589, 10100101010001010100101, 101012010210101
90396755477, 1010100001100000100010000011000010101, 22122022220102222022122
```



## JavaScript


### ES6

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // compose :: (b -> c) -> (a -> b) -> (a -> c)
    const compose = (f, g) => x => f(g(x));

    // listApply :: [(a -> b)] -> [a] -> [b]
    const listApply = (fs, xs) =>
        [].concat.apply([], fs.map(f =>
        [].concat.apply([], xs.map(x => [f(x)]))));

    // pure :: a -> [a]
    const pure = x => [x];

    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // reverse :: [a] -> [a]
    const reverse = xs =>
        typeof xs === 'string' ? (
            xs.split('')
            .reverse()
            .join('')
        ) : xs.slice(0)
        .reverse();

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // drop :: Int -> [a] -> [a]
    const drop = (n, xs) => xs.slice(n);

    // maximum :: [a] -> a
    const maximum = xs =>
        xs.reduce((a, x) => (x > a || a === undefined ? x : a), undefined);

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // length :: [a] -> Int
    const length = xs => xs.length;

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // BASES AND PALINDROMES

    // show, showBinary, showTernary :: Int -> String
    const show = n => n.toString(10);
    const showBinary = n => n.toString(2);
    const showTernary = n => n.toString(3);

    // readBase3 :: String -> Int
    const readBase3 = s => parseInt(s, 3);

    // base3Palindrome :: Int -> String
    const base3Palindrome = n => {
        const s = showTernary(n);
        return s + '1' + reverse(s);
    };

    // isBinPal :: Int -> Bool
    const isBinPal = n => {
        const
            s = showBinary(n),
            [q, r] = quotRem(s.length, 2);
        return (r !== 0) && drop(q + 1, s) === reverse(take(q, s));
    };

    // solutions :: [Int]
    const solutions = [0, 1].concat(range(1, 10E5)
        .map(compose(readBase3, base3Palindrome))
        .filter(isBinPal));

    // TABULATION

    // cols :: [[Int]]
    const cols = transpose(
        [
            ['Decimal', 'Ternary', 'Binary']
        ].concat(
            solutions.map(
                compose(
                    xs => listApply([show, showTernary, showBinary], xs),
                    pure
                )
            )
        )
    );

    return unlines(
        transpose(cols.map(col => col.map(
            curry(justifyLeft)(maximum(col.map(length)) + 1, ' ')
        )))
        .map(unwords));
})();
```

```txt
Decimal      Ternary                  Binary
0            0                        0
1            1                        1
6643         100010001                1100111110011
1422773      2200021200022            101011011010110110101
5415589      101012010210101          10100101010001010100101
90396755477  22122022220102222022122  1010100001100000100010000011000010101
```



## Julia

```julia
ispalindrome(n, bas) = (s = string(n, base=bas); s == reverse(s))
prin3online(n) = println(lpad(n, 15), lpad(string(n, base=2), 40), lpad(string(n, base=3), 30))
reversebase3(n) = (x = 0; while n != 0 x = 3x + (n %3); n = div(n, 3); end; x)

function printpalindromes(N)
    lo, hi, pow2, pow3, count, i = 0, 1, 1, 1, 1, 0
    println(lpad("Number", 15), lpad("Base 2", 40), lpad("Base 3", 30))
    prin3online(0)
    while true
        for j in lo:hi-1
            i = j
            n = (3 * j + 1) * pow3 + reversebase3(j)
            if ispalindrome(n, 2)
                prin3online(n)
                count += 1
                if count >= N
                    return
                end
            end
        end
        if i == pow3
            pow3 *= 3
        else
            pow2 *= 4
        end

        while true
            while pow2 <= pow3
                pow2 *= 4
            end
            lo2 = div(div(pow2, pow3) - 1, 3)
            hi2 = div(div(pow2 * 2, pow3), 3) + 1
            lo3 = div(pow3, 3)
            hi3 = pow3

            if lo2 >= hi3
                pow3 *= 3
            elseif lo3 >= hi2
                pow2 *= 4
            else
                lo = max(lo2, lo3)
                hi = min(hi2, hi3)
                break
            end
        end
    end
end

printpalindromes(6)

```
```txt

         Number                                  Base 2                        Base 3
              0                                       0                             0
              1                                       1                             1
           6643                           1100111110011                     100010001
        1422773                   101011011010110110101                 2200021200022
        5415589                 10100101010001010100101               101012010210101
    90396755477   1010100001100000100010000011000010101       22122022220102222022122

```



## Kotlin

```scala
// version 1.0.5-2

/** converts decimal 'n' to its ternary equivalent */
fun Long.toTernaryString(): String = when {
    this < 0L  -> throw IllegalArgumentException("negative numbers not allowed")
    this == 0L -> "0"
    else   -> {
        var result = ""
        var n = this
        while (n > 0) {
            result += n % 3
            n /= 3
        }
        result.reversed()
    }
}

/** wraps java.lang.Long.toBinaryString in a Kotlin extension function */
fun Long.toBinaryString(): String = java.lang.Long.toBinaryString(this)

/** check if a binary or ternary numeric string 's' is palindromic */
fun isPalindromic(s: String): Boolean = (s == s.reversed())

/** print a number which is both a binary and ternary palindrome in all three bases */
fun printPalindrome(n: Long) {
    println("Decimal : $n")
    println("Binary  : ${n.toBinaryString()}")
    println("Ternary : ${n.toTernaryString()}")
    println()
}

/** create a ternary palindrome whose left part is the ternary equivalent of 'n' and return its decimal equivalent */
fun createPalindrome3(n: Long): Long {
    val ternary = n.toTernaryString()
    var power3 = 1L
    var sum = 0L
    val length = ternary.length
    for (i in 0 until length) {  // right part of palindrome is mirror image of left part
        if (ternary[i] > '0') sum += (ternary[i].toInt() - 48) * power3
        power3 *= 3L
    }
    sum += power3 // middle digit must be 1
    power3 *= 3L
    sum += n * power3  // value of left part is simply 'n' multiplied by appropriate power of 3
    return sum
}

fun main(args: Array<String>) {
    var i = 1L
    var p3: Long
    var count = 2
    var binStr: String
    println("The first 6 numbers which are palindromic in both binary and ternary are:\n")
    // we can assume the first two palindromic numbers as per the task description
    printPalindrome(0L)  // 0 is a palindrome in all 3 bases
    printPalindrome(1L)  // 1 is a palindrome in all 3 bases

    do {
        p3 = createPalindrome3(i)
        if (p3 % 2 > 0L)  { // cannot be even as binary equivalent would end in zero
            binStr = p3.toBinaryString()
            if (binStr.length % 2 == 1) { // binary palindrome must have an odd number of digits
                if (isPalindromic(binStr)) {
                    printPalindrome(p3)
                    count++
                }
            }
        }
        i++
    }
    while (count < 6)
}
```

```txt
The first 6 numbers which are palindromic in both binary and ternary are:

Decimal : 0
Binary  : 0
Ternary : 0

Decimal : 1
Binary  : 1
Ternary : 1

Decimal : 6643
Binary  : 1100111110011
Ternary : 100010001

Decimal : 1422773
Binary  : 101011011010110110101
Ternary : 2200021200022

Decimal : 5415589
Binary  : 10100101010001010100101
Ternary : 101012010210101

Decimal : 90396755477
Binary  : 1010100001100000100010000011000010101
Ternary : 22122022220102222022122
```



## Mathematica


```Mathematica
palindromify3[n_] :=
    Block[{digits},
      If[Divisible[n, 3], {},
         digits = IntegerDigits[n, 3];
         FromDigits[#, 3] & /@
            {Join[Reverse[digits], digits], Join[Reverse[Rest[digits]], {First[digits]}, Rest[digits]]}
      ]
    ];
base2PalindromeQ[n_] := IntegerDigits[n, 2] === Reverse[IntegerDigits[n, 2]];

Select[Flatten[palindromify3 /@ Range[1000000]], base2PalindromeQ]
```


```txt
{1, 6643, 1422773, 5415589, 90396755477}
```



## PARI/GP


```parigp
check(n)={ \\ Check for 2n+1-digit palindromes in base 3
  my(N=3^n);
  forstep(i=N+1,2*N,[1,2],
    my(base2,base3=digits(i,3),k);
    base3=concat(Vecrev(base3[2..n+1]), base3);
    k=subst(Pol(base3),'x,3);
    base2=binary(k);
    if(base2==Vecrev(base2), print1(", "k))
  )
};
print1("0, 1"); for(i=1,11,check(i))
```

```txt
0, 1, 6643, 1422773, 5415589, 90396755477
```



## Perl

```perl
use ntheory qw/fromdigits todigitstring/;

print "0  0  0\n";  # Hard code the 0 result
for (0..2e5) {
  # Generate middle-1-palindrome in base 3.
  my $pal = todigitstring($_, 3);
  my $b3 = $pal . "1" . reverse($pal);
  # Convert base 3 number to base 2
  my $b2 = todigitstring(fromdigits($b3, 3), 2);
  # Print results (including base 10) if base-2 palindrome
  print fromdigits($b2,2),"  $b3  $b2\n"   if $b2 eq reverse($b2);
}
```

```txt
0  0  0
1  1  1
6643  100010001  1100111110011
1422773  2200021200022  101011011010110110101
5415589  101012010210101  10100101010001010100101
90396755477  22122022220102222022122  1010100001100000100010000011000010101
```



## Perl 6

Instead of searching for numbers that are palindromes in one base then checking the other, generate palindromic trinary numbers directly, then check to see if they are also binary palindromes (with additional simplifying constraints as noted in other entries). Outputs the list in decimal, binary and trinary.


```perl6
constant palindromes = 0, 1, |gather for 1 .. * -> $p {
    my $pal = $p.base(3);
    my $n = :3($pal ~ '1' ~ $pal.flip);
    next if $n %% 2;
    my $b2 = $n.base(2);
    next if $b2.chars %% 2;
    next unless $b2 eq $b2.flip;
    take $n;
}

printf "%d, %s, %s\n", $_, .base(2), .base(3) for palindromes[^6];
```

```txt
0, 0, 0
1, 1, 1
6643, 1100111110011, 100010001
1422773, 101011011010110110101, 2200021200022
5415589, 10100101010001010100101, 101012010210101
90396755477, 1010100001100000100010000011000010101, 22122022220102222022122
```



## Phix

Alternative approach. Works by finding the next palindrome, in either base, in an attempt to
skip fairly large chunks of the search space. Prints the first 6 palindromes (the limit on
32 bit) in about a second, but the 7th (on 64 bit only) takes just over half an hour.

Theoretically it could be made a fair bit faster by replacing the string handling (which I
hope you will find very easy to follow) with maths/bit-fiddling, however my attempts at
that turned out noticeably slower.


```Phix
-- widths and limits for 32/64 bit running (see output below):
constant {dsize,w3,w2,limit} = iff(machine_bits()=32?{12,23,37,6}
                                                    :{18,37,59,7}),
-- [atoms on 32-bit have only 53 bits of precision, but 7th ^^^^ requires 59]
         dfmt = sprintf("%%%dd",dsize), -- ie "%12d" or "%18d"
         esc = #1B

function center(string s, integer l)
    l = max(0,floor((l-length(s))/2))
    string pad = repeat(' ',l)
    s = pad & s & pad
    return s
end function

integer count = 1

procedure show(atom n, string p2, p3)
    if count=1 then
        printf(1,"    %s %s %s\n",{pad_head("decimal",dsize),center("ternary",w3),center(" binary",w2)})
    end if
    string ns = sprintf(dfmt,n)
    printf(1,"%2d: %s %s %s\n",{count, ns, center(p3,w3), center(p2,w2)})
    count += 1
end procedure

procedure progress(string e, p2, p3)
    e = pad_head(e,dsize)
    printf(1,"--: %s %s %s\r",{e, center(p3,w3), center(p2,w2)})
end procedure

function to_base(atom i, integer base)
    string s = ""
    while i>0 do
        s = append(s,remainder(i,base)+'0')
        i = floor(i/base)
    end while
    s = reverse(s)
    if s="" then
        s = "0"
    end if
    return s
end function

function from_base(string s, integer base)
    atom res = 0
    for i=1 to length(s) do
        res = res*base+s[i]-'0'
    end for
    return res
end function

function sn(string s, integer f, base)
-- helper function, return s mirrored (if f!=0)
--       and as a (decimal) number (if base!=0)
-- all returns from next_palindrome() get fed through here.
    if f then
        s[f+2..$] = reverse(s[1..f])
    end if
    atom n = iff(base?from_base(s,base):0)
    return {s,n}
end function

function next_palindrome(integer base, object s)
--
-- base is 2 or 3
-- s is not usually a palindrome, but derived from one in <5-base>
--
-- all done with very obvious string manipulations, plus a few
-- less obvious optimisations (odd length, middle 1 in base 3).
--
-- example: next_palindrome(2,"10001000100") -> "10001010001"
--
    if not string(s) then s = to_base(s,base) end if
    integer l = length(s),
            f = floor(l/2),
            m = f+1, c
    if mod(l,2) then    -- optimisation: palindromes must be odd-length
        -- 1) is a plain mirror greater? (as in the example just given)
        {string r} = sn(s,f,0)
        -- optimisation: base 3 palindromes have '1' in the middle
        if base=3 and r[m]!='1' then r[m] = '1' end if
        if r>s then return sn(r,0,base) end if
        -- 2) can we (just) increment the middle digit?
        c = s[m]-'0'+1
        if base=2 or c=1 then
            if c<base then
                s[m] = c+'0'
                return sn(s,f,base)
            end if
            s[m] = '0'
        elsif base=3 then
            s[m] = '1'
        end if
        -- 3) can we increment left half (or is it all <base-1>s?)
        for i=f to 1 by -1 do
            if s[i]<base-1+'0' then
                s[i] += 1
                return sn(s,f,base)
            else
                s[i] = '0'
            end if
        end for
        l += 2  -- (stay odd)
    else
        l += 1  -- (even->odd)
    end if
    -- 4) well then, next palindrome is longer, 1000..0001-style
    s = sprintf("1%s1",{repeat('0',l-2)})
    -- optimisation: base 3 palindromes have '1' in the middle
    if base=3 then
        m = (l+1)/2
        s[m] = '1'
    end if
    return sn(s,0,base)
end function

string p2 = "0", p3 = "0"   -- palindromes as strings in base 2 and 3
atom n2 = 0, n3 = 0,        -- decimal equivalents of the above.
     t0 = time(),
     t1 = time()+1

while count<=limit do
    if n2=n3 then
        show(n2,p2,p3)
        {p2,n2} = next_palindrome(2,p2)
        {p3,n3} = next_palindrome(3,p3)
    elsif n2<n3 then
        {p2,n2} = next_palindrome(2,n3-1)
    elsif n2>n3 then
        {p3,n3} = next_palindrome(3,n2-1)
    end if
    if time()>t1 then
        progress(elapsed_short(time()-t0),p2,p3)
        t1 = time()+1
        if find(get_key(),{'q','Q',esc}) then exit end if
    end if
end while
?elapsed(time()-t0)
```

32 bit:

```txt

         decimal         ternary                         binary
 1:            0            0                              0
 2:            1            1                              1
 3:         6643        100010001                    1100111110011
 4:      1422773      2200021200022              101011011010110110101
 5:      5415589     101012010210101            10100101010001010100101
 6:  90396755477 22122022220102222022122 1010100001100000100010000011000010101
"1.0s"

```

64-bit:

```txt

               decimal                ternary                                           binary
 1:                  0                   0                                                0
 2:                  1                   1                                                1
 3:               6643               100010001                                      1100111110011
 4:            1422773             2200021200022                                101011011010110110101
 5:            5415589            101012010210101                              10100101010001010100101
 6:        90396755477        22122022220102222022122                   1010100001100000100010000011000010101
--:                56s    2001210110001221221000110121002         1100000101111010101010010100101010101111010000011
 7: 381920985378904469 2112200222001222121212221002220022112 10101001100110110110001110011011001110001101101100110010101
"33 minutes and 08s"

```

between 6 and 7 I have shown progress() in action, which is constantly overwritten, and mesmerising to watch.


###  much simpler version

(slightly but not alot faster)

```Phix
function to_base(atom n, integer base)
    string result = ""
    while true do
        result &= remainder(n,base)
        n = floor(n/base)
        if n=0 then exit end if
    end while
    return result
end function

procedure show(integer count, atom n)
    string n2 = sq_add('0',to_base(n,2)),
           n3 = sq_add('0',to_base(n,3)),
           p2 = repeat(' ',(37-length(n2))/2),
           p3 = repeat(' ',(23-length(n3))/2)
    printf(1,"%2d: %12d %s%s%s %s%s\n",{count, n, p3,n3,p3, p2,n2})
end procedure

function createpalindrome3(integer n)
    atom tot = 0, power3 = 1
    string ternary = to_base(n,3)
    for i=length(ternary) to 1 by -1 do
        tot += ternary[i] * power3
        power3 *= 3
    end for
    return tot + power3 + n*power3*3
end function

atom t0 = time()
printf(1,"%16s %15s %30s\n",{"decimal","ternary","binary"})
show(0,0)
show(1,1)
integer count = 2, n = 1
while count<6 do
    atom n3 = createpalindrome3(n)
    if remainder(n3,2) then
        string n2 = to_base(n3,2)
        if n2[$]=1 and n2=reverse(n2) then
            show(count,n3)
            count += 1
        end if
    end if
    n += 1
end while
?elapsed(time()-t0)
```

```txt

         decimal         ternary                         binary
 0:            0            0                              0
 1:            1            1                              1
 2:         6643        100010001                    1100111110011
 3:      1422773      2200021200022              101011011010110110101
 4:      5415589     101012010210101            10100101010001010100101
 5:  90396755477 22122022220102222022122 1010100001100000100010000011000010101
"0.6s"

```



###  much faster version

Inspired by Scala 😏

```Phix
function to_base(string s, integer base)
-- convert decimal string s to specified base
    string res = ""
    while length(s) do
        integer q, r = 0
        for i=1 to length(s) do
            q = r*10+s[i]-'0'
            s[i] = floor(q/base)+'0'
            r = mod(q,base)
        end for
        res &= r+'0'
        while length(s) and s[1]='0' do
            s = s[2..$]
        end while
    end while
    return res
end function

procedure center(string s, integer l)
    l = max(0,floor((l-length(s))/2))
    string pad = repeat(' ',l)
    puts(1,pad & s & pad & "\n")
end procedure

constant A = {"0","1","6643","1422773","5415589","90396755477",
              "381920985378904469","1922624336133018996235",
              "2004595370006815987563563",
              "8022581057533823761829436662099",
              "392629621582222667733213907054116073",
              "32456836304775204439912231201966254787",
              "428027336071597254024922793107218595973",
              "1597863243206403857787246920544522912361",
              "30412638162199251273509758127730300026189",
              "32345684491703244980406880704479906642045",
              "24014998963383302600955162866787153652444049"}

for i=1 to length(A) do
    center(to_base(A[i],2),145)
    center(to_base(A[i],3),145)
end for
```

```txt

                                                                        0
                                                                        0
                                                                        1
                                                                        1
                                                                  1100111110011
                                                                    100010001
                                                              101011011010110110101
                                                                  2200021200022
                                                             10100101010001010100101
                                                                 101012010210101
                                                      1010100001100000100010000011000010101
                                                             22122022220102222022122
                                           10101001100110110110001110011011001110001101101100110010101
                                                      2112200222001222121212221002220022112
                                     11010000011100111000101110001110011011001110001110100011100111000001011
                                                  122120102102011212112010211212110201201021221
                                110101000011111010101010100101111011110111011110111101001010101010111110000101011
                                               221010112100202002120002212200021200202001211010122
                     1100101010000100101101110000011011011111111011000011100001101111111101101100000111011010010000101010011
                                        21000020210011222122220212010000100001021202222122211001202000012
             10010111001111000100010100010100000011011011000101011011100000111011010100011011011000000101000101000100011110011101001
                                   122102120011102000101101000002010021111120010200000101101000201110021201221
          11000011010101111010110010100010010011011010101001101000001000100010000010110010101011011001001000101001101011110101011000011
                                 1222100201002211120110022121002012121101011212102001212200110211122001020012221
        101000010000000110001000011111100101011110011100001110100011100010001110001011100001110011110101001111110000100011000000010000101
                                222001200110022102121001000200200202022111220202002002000100121201220011002100222
       10010110010000110010100010001000111010010000111000010010100010111011101000101001000011100001001011100010001000101001100001001101001
                               10121021220121202021201220210001211020122122102011210001202210212020212102212012101
     101100101011111111011010000110101101100101010100101101010000001000000000100000010101101001010101001101101011000010110111111110101001101
                              2112120210211212121000000011202022210210101010120122202021100000001212121120120212112
     101111100001110001100000011101111000001111001000110100111001010101101101010100111001011000100111100000111101110000001100011100001111101
                              2200221111210202000010122020002221112212101012122111222000202210100002020121111220022
1000100111010110110111101001100011100110100000011011001010100101011001100011001101010010101001101100000010110011100011001011110110110101110010001
                           2202021211210100110100002202101000110000220121210220000110001012022000010110010121121202022

```



## PicoLisp


```PicoLisp
(de ternary (N)
   (if (=0 N)
      (cons N)
      (make
         (while (gt0 N)
            (yoke (% (swap 'N (/ N 3)) 3)) ) ) ) )
(de p? (L1 L2)
   (and
      (= L1 (reverse L1))
      (= L2 (reverse L2)) ) )

(zero N)
(for (I 0 (> 6 I))
   (let (B2 (chop (bin N))  B3 (ternary N))
      (when (p? B2 B3)
         (println N (pack B2) (pack B3))
         (inc 'I) )
      (inc 'N) ) )
```

```txt
0 "0" "0"
1 "1" "1"
6643 "1100111110011" "100010001"
1422773 "101011011010110110101" "2200021200022"
5415589 "10100101010001010100101" "101012010210101"
90396755477 "1010100001100000100010000011000010101" "22122022220102222022122"
```


=={{Header|Python}}==

### Imperative


```python
from itertools import islice

digits = "0123456789abcdefghijklmnopqrstuvwxyz"

def baseN(num,b):
  if num == 0: return "0"
  result = ""
  while num != 0:
    num, d = divmod(num, b)
    result += digits[d]
  return result[::-1] # reverse

def pal2(num):
    if num == 0 or num == 1: return True
    based = bin(num)[2:]
    return based == based[::-1]

def pal_23():
    yield 0
    yield 1
    n = 1
    while True:
        n += 1
        b = baseN(n, 3)
        revb = b[::-1]
        #if len(b) > 12: break
        for trial in ('{0}{1}'.format(b, revb), '{0}0{1}'.format(b, revb),
                      '{0}1{1}'.format(b, revb), '{0}2{1}'.format(b, revb)):
            t = int(trial, 3)
            if pal2(t):
                yield t

for pal23 in islice(pal_23(), 6):
    print(pal23, baseN(pal23, 3), baseN(pal23, 2))
```

```txt
0 0 0
1 1 1
6643 100010001 1100111110011
1422773 2200021200022 101011011010110110101
5415589 101012010210101 10100101010001010100101
90396755477 22122022220102222022122 1010100001100000100010000011000010101
```



### Functional

```Python
'''Numbers with palindromic digit strings in both binary and ternary'''

from itertools import (islice)


# palinBoth :: Generator [Int]
def palinBoth():
    '''Non finite stream of dually palindromic integers.'''
    yield (0, '0', '0')
    ibt = (1, '1', '1')

    yield ibt
    while True:
        ibt = until(isBoth)(psucc)(psucc(ibt))
        yield (int(ibt[2], 3), ibt[1], ibt[2])


# isBoth :: (Int, String, String) -> Bool
def isBoth(ibt):
    '''True if the binary string is palindromic (as
       the ternary string is already known to be).
    '''
    b = ibt[1]
    return b == b[::-1]


# psucc :: (Int, String, String) -> (Int, String, String)
def psucc(ibt):
    '''The next triple of index, binary
       and (palindromic) ternary string
    '''
    d = 1 + ibt[0]
    s = showBase3(d)
    pal = s + '1' + s[::-1]
    return (d, bin(int(pal, 3))[2:], pal)


# showBase3 :: Int -> String
def showBase3(n):
    '''Ternary digit string for integer n.'''
    return showIntAtBase(3)(
        lambda i: '012'[i]
    )(n)('')


# TEST ----------------------------------------------------
def main():
    '''Integers with palindromic digits in both binary and ternary'''

    xs = take(6)(palinBoth())
    d, b, t = xs[-1]
    bw = len(b)
    tw = len(t)

    print(
        fTable(
            label('rjust')(('Decimal', len(str(d)))) +
            ''.join(map(
                label('center'),
                [('Binary', bw), ('Ternary', tw)]
            )) + '\n'
        )(compose(str)(fst))(
            lambda p: p[1].center(bw, ' ') +
            '    ' + p[2].center(tw, ' ')
        )(identity)(xs)
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    '''String representation of an integer in a given base,
       using a supplied function for the string representation
       of digits.
    '''
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


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# FORMATTING ----------------------------------------------

# label :: Method String -> (String, Int)
def label(k):
    '''Stringification, using the named justification
       method (ljust|centre|rjust) of the label,
       and the specified amount of white space.
    '''
    def go(sw):
        s, w = sw
        return getattr(s, k)(w, ' ') + '    '
    return lambda sw: go(sw)


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
    Decimal                    Binary                           Ternary

          0 ->                   0                                 0
          1 ->                   1                                 1
       6643 ->             1100111110011                       100010001
    1422773 ->         101011011010110110101                 2200021200022
    5415589 ->        10100101010001010100101               101012010210101
90396755477 -> 1010100001100000100010000011000010101    22122022220102222022122
```



## Racket


```racket
#lang racket
(require racket/generator)

(define (digital-reverse/base base N)
  (define (inr n r)
    (if (zero? n) r (inr (quotient n base) (+ (* r base) (modulo n base)))))
  (inr N 0))

(define (palindrome?/base base N)
  (define (inr? n m)
    (if (= n 0)
        (= m N)
        (inr? (quotient n base) (+ (* m base) (modulo n base)))))
  (inr? N 0))

(define (palindrome?/3 n)
  (palindrome?/base 3 n))

(define (b-palindromes-generator b)
  (generator
   ()
   ;; it's a bit involved getting the initial palindroms, so we do them manually
   (for ((p (in-range b))) (yield p))
   (let loop ((rhs 1) (mx-rhs b) (mid #f) (mx-rhs*b (* b b)))
     (cond
       [(= rhs mx-rhs)
        (cond
          [(not mid) (loop (quotient mx-rhs b) mx-rhs 0 mx-rhs*b)]
          [(zero? mid) (loop mx-rhs mx-rhs*b #f (* mx-rhs*b b))])]
       [else
        (define shr (digital-reverse/base b rhs))
        (cond
          [(not mid)
           (yield (+ (* rhs mx-rhs) shr))
           (loop (add1 rhs) mx-rhs #f mx-rhs*b)]
          [(= mid (- b 1))
           (yield (+ (* rhs mx-rhs*b) (* mid mx-rhs) shr))
           (loop (+ 1 rhs) mx-rhs 0 mx-rhs*b)]
          [else
           (yield (+ (* rhs mx-rhs*b) (* mid mx-rhs) shr))
           (loop rhs mx-rhs (add1 mid) mx-rhs*b)])]))))

(define (number->string/base n b)
  (define (inr acc n)
    (if (zero? n) acc
        (let-values (((q r) (quotient/remainder n b)))
          (inr (cons (number->string r) acc) q))))
  (if (zero? n) "0" (apply string-append (inr null n))))

(module+ main
  (for ((n (sequence-filter palindrome?/3 (in-producer (b-palindromes-generator 2))))
        (i (in-naturals))
        #:final (= i 5))
    (printf "~a: ~a_10 ~a_3 ~a_2~%"
            (~a #:align 'right #:min-width  3 (add1 i))
            (~a #:align 'right #:min-width 11 n)
            (~a #:align 'right #:min-width 23 (number->string/base n 3))
            (~a #:align 'right #:min-width 37 (number->string/base n 2)))))

(module+ test
  (require rackunit)
  (check-true  (palindrome?/base 2 #b0))
  (check-true  (palindrome?/base 2 #b10101))
  (check-false (palindrome?/base 2 #b1010))
  (define from-oeis:A060792
    (list 0 1 6643 1422773 5415589 90396755477 381920985378904469
          1922624336133018996235 2004595370006815987563563
          8022581057533823761829436662099))
  (check-match from-oeis:A060792
               (list (? (curry palindrome?/base 2)
                        (? (curry palindrome?/base 3))) ...))

  (check-eq? (digital-reverse/base 2 #b0)        #b0)
  (check-eq? (digital-reverse/base 2 #b1)        #b1)
  (check-eq? (digital-reverse/base 2 #b10)      #b01)
  (check-eq? (digital-reverse/base 2 #b1010)  #b0101)

  (check-eq? (digital-reverse/base 10 #d0)       #d0)
  (check-eq? (digital-reverse/base 10 #d1)       #d1)
  (check-eq? (digital-reverse/base 10 #d10)     #d01)
  (check-eq? (digital-reverse/base 10 #d1010) #d0101)

  (define pg ((b-palindromes-generator 2)))
  (check-match
   (map (curryr number->string 2) (for/list ((i 16) (p (in-producer (b-palindromes-generator 2)))) p))
   (list "0" "1" "11" "101" "111" "1001" "1111" "10001" "10101" "11011"
         "11111" "100001" "101101" "110011" "111111" "1000001")))
```

```txt
  1:           0_10                       0_3                                     0_2
  2:           1_10                       1_3                                     1_2
  3:        6643_10               100010001_3                         1100111110011_2
  4:     1422773_10           2200021200022_3                 101011011010110110101_2
  5:     5415589_10         101012010210101_3               10100101010001010100101_2
  6: 90396755477_10 22122022220102222022122_3 1010100001100000100010000011000010101_2
```



## REXX


### version 1

Programming note:   This version is quite a bit faster than the previous REXX program that was entered.

For this REXX program, a few deterministic assumptions were made:
::*   for the requirement of binary palindromes, the number of binary digits have to be odd.
::*   for the requirement of ternary palindromes, the numbers can't end in zero (in base 3).

The method used is to   (not find, but)   ''construct''   a binary palindrome by:
::*   using the binary version of a number (abcdef),   which may end in binary zeroes,
::*   flipping the binary digits (fedcba)       [note that   '''a'''   is always   '''1'''   (one)],
::*   constructing two binary palindromes:
::::*   abcdef   <big>||</big>   '''0'''   <big>||</big>   fedcba           and
::::*   abcdef   <big>||</big>   '''1'''   <big>||</big>   fedcba
::*   (the above two concatenation (  <big>||</big>  ) steps ensures an odd number of binary digits),
::*   ensure the decimal versions are not evenly divisible by 3,
::*   convert the decimal numbers to base 3,
::*   ensure that the numbers in base 3 are palindromic.

```rexx
/*REXX program finds  numbers  that are  palindromic  in both  binary  and  ternary.    */
digs=50;  numeric digits digs                    /*biggest known B2B3 palindrome: 44 dig*/
parse arg maxHits .;   if maxHits==''  then maxHits=6              /*use six as a limit.*/
hits=0;                      #= 'fiat'           /*the number of palindromes  (so far). */
call show 0,0,0;       call show 1,1,1           /*show the first two palindromes (fiat)*/
!.=                                              /* [↓]  build list of powers of three. */
     do i=1  until !.i>10**digs;  !.i=3**i;  end /*compute powers of  three  for radix3.*/
p=1                                              /* [↓]  primary search: bin palindromes*/
     do #=digs                                   /*use all numbers, however, DEC is odd.*/
     binH=x2b( d2x(#) )   + 0                    /*convert some decimal number to binary*/
     binL=reverse(binH)                          /*reverse the binary digits  (or bits).*/
     dec=x2d( b2x( binH'0'binL) );   if dec//3\==0  then call radix3
     dec=x2d( b2x( binH'1'binL) );   if dec//3\==0  then call radix3
     end   /*#*/                                 /* [↑]  crunch 'til found 'nuff numbers*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
radix3: parse var dec x 1 $,q                    /* [↓] convert decimal  # ──►  ternary.*/
                 do j=p  while !.j<=x;   end     /*find upper limit of power of three.  */
        p=j-1                                    /*use this power of three for next time*/
                 do k=p  by -1  for p;   _=!.k;   d=x%_;   q=q || d;   x=x//_;   end /*k*/
        t=q || x                                 /*handle residual of ternary conversion*/
        if t\==reverse(t)  then return           /*is T ternary number not palindromic? */
        call show $, t, strip(x2b(d2x($)), , 0)  /*show number: decimal, ternary, binary*/
        return                                   /* [↑]   RADIX3 subroutine is sluggish.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: hits=hits+1;         say                   /*bump the number of palindromes found.*/
      say right('['hits"]", 5)   right( arg(1), digs)   '(decimal),   ternary='  arg(2)
      say right('', 5+1+ digs)                          '             binary ='  arg(3)
      if hits>2  then  if hits//2  then #=#'0'
      if hits<maxHits  then return               /*Not enough palindromes?  Keep looking*/
      exit                                       /*stick a fork in it,  we're all done. */
```

```txt
  [1]                                                  0 (decimal),   ternary= 0
                                                                      binary = 0

  [2]                                                  1 (decimal),   ternary= 1
                                                                      binary = 1

  [3]                                               6643 (decimal),   ternary= 100010001
                                                                      binary = 1100111110011

  [4]                                            1422773 (decimal),   ternary= 2200021200022
                                                                      binary = 101011011010110110101

  [5]                                            5415589 (decimal),   ternary= 101012010210101
                                                                      binary = 10100101010001010100101

  [6]                                        90396755477 (decimal),   ternary= 22122022220102222022122
                                                                      binary = 1010100001100000100010000011000010101

  [7]                                 381920985378904469 (decimal),   ternary= 2112200222001222121212221002220022112
                                                                      binary = 10101001100110110110001110011011001110001101101100110010101
```

[Output note:   the   6<sup>th</sup>   number (above) took a couple of seconds to compute.]


### version 2

This REXX version takes advantage that the palindromic numbers (in both binary and ternary bases)   ''seem''   to only have a modulus nine residue of   1, 5, 7, or 8.   With this assumption, the following REXX program is about 25% faster.

```rexx
/*REXX program finds  numbers  that are  palindromic  in both  binary  and  ternary.    */
digs=50;  numeric digits digs                    /*biggest known B2B3 palindrome: 44 dig*/
parse arg maxHits .;   if maxHits==''  then maxHits=6              /*use six as a limit.*/
hits=0;                      #= 'fiat'           /*the number of palindromes  (so far). */
call show 0,0,0;       call show 1,1,1           /*show the first two palindromes (fiat)*/
#.=0;    #.1=1;  #.5=1;  #.7=1;  #.8=1           /*modulus nine results that are OK.    */
!.=                                              /* [↓]  build list of powers of three. */
     do i=1  until !.i>10**digs;  !.i=3**i;  end /*compute powers of  three  for radix3.*/
p=1                                              /* [↓]  primary search: bin palindromes*/
     do #=digs                                   /*use all numbers, however, DEC is odd.*/
     binH=x2b( d2x(#) )   + 0                    /*convert some decimal number to binary*/
     binL=reverse(binH)                          /*reverse the binary digits  (or bits).*/
     dec=x2d( b2x( binH'0'binL) ); _=dec//9;  if #._  then if dec//3\==0  then call radix3
     dec=x2d( b2x( binH'1'binL) ); _=dec//9;  if #._  then if dec//3\==0  then call radix3
     end   /*#*/                                 /* [↑]  crunch 'til found 'nuff numbers*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
radix3: parse var dec x 1 $,q                    /* [↓] convert decimal  # ──►  ternary.*/
                 do j=p  while !.j<=x;   end     /*find upper limit of power of three.  */
        p=j-1                                    /*use this power of three for next time*/
                 do k=p  by -1  for p;   _=!.k;   d=x%_;   q=q || d;   x=x//_;   end /*k*/
        t=q || x                                 /*handle residual of ternary conversion*/
        if t\==reverse(t)  then return           /*is T ternary number not palindromic? */
        call show $, t, strip(x2b(d2x($)), , 0)  /*show number: decimal, ternary, binary*/
        return                                   /* [↑]   RADIX3 subroutine is sluggish.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: hits=hits+1;         say                   /*bump the number of palindromes found.*/
      say right('['hits"]", 5)   right( arg(1), digs)   '(decimal),   ternary='  arg(2)
      say right('', 5+1+ digs)                          '             binary ='  arg(3)
      if hits>2  then  if hits//2  then #=#'0'
      if hits<maxHits  then return               /*Not enough palindromes?  Keep looking*/
      exit                                       /*stick a fork in it,  we're all done. */
```

## Ring


```ring

# Project: Find palindromic numbers in both binary and ternary bases

max = 6
nr = 0
pal = 0
see "working..." + nl
see "wait for done..." + nl
while true
      binpal = basedigits(nr,2)
      terpal = basedigits(nr,3)
      bool1 = ispalindrome(binpal)
      bool2 = ispalindrome(terpal)
      if bool1 = 1 and bool2 = 1
         pal = pal + 1
         see string(nr) + " " + binpal + "(2) " + terpal + "(3)" + nl
         if pal = max
            exit
         ok
      ok
      nr = nr + 1
end
see "done..." + nl

func basedigits(n,base)
     if n = 0
        return "0"
     ok
     result = ""
     while n > 0
           result = string(n % base) + result
           n = floor(n/base)
     end
     return result

func ispalindrome(astring)
     if astring = "0"
        return 1
     ok
     bString = ""
     for i=len(aString) to 1 step -1
         bString = bString + aString[i]
     next
     if aString = bString
        return 1
     else
        return 0
     ok

```

```txt

working...
wait for done...
0 0(2) 0(3)
1 1(2) 1(3)
6643 1100111110011(2) 100010001(3)
1422773 101011011010110110101(2) 2200021200022(3)
5415589 10100101010001010100101(2) 101012010210101(3)
90396755477 1010100001100000100010000011000010101(2) 22122022220102222022122(3)
done...

```



## Ruby

This program is based on the fact that the double palindromic numbers in base 3 all have a "1" right in the middle. Also, both base 2 and base 3 representations have an odd number of digits.

# 1 digit under the number of the palindromic doesn't become zero.
# As for the N numbering-system, at the time of the multiple of N, 1 digit below becomes zero.
# Palindromic by the even-number digit binary system is 3 multiples.
# Palindromic by the even-number digit ternary-system is 4 multiples.
# In palindromic by the ternary-system of the odd digit, the value of the center position is an even number in case of "0" or "2".

This program constructs base 3 palindromes using the above "rules" and checks if they happen to be binary palindromes.


```ruby
pal23 = Enumerator.new do |y|
  y << 0
  y << 1
  for i in 1 .. 1.0/0.0                 # 1.step do |i|  (Ruby 2.1+)
    n3 = i.to_s(3)
    n = (n3 + "1" + n3.reverse).to_i(3)
    n2 = n.to_s(2)
    y << n  if n2.size.odd? and n2 == n2.reverse
  end
end

puts "         decimal          ternary                          binary"
6.times do |i|
  n = pal23.next
  puts "%2d: %12d %s %s" % [i, n, n.to_s(3).center(25), n.to_s(2).center(39)]
end
```

```txt
         decimal          ternary                          binary
 0:            0             0                                0
 1:            1             1                                1
 2:         6643         100010001                      1100111110011
 3:      1422773       2200021200022                101011011010110110101
 4:      5415589      101012010210101              10100101010001010100101
 5:  90396755477  22122022220102222022122   1010100001100000100010000011000010101
```



## Scala

===Functional programmed, (tail) recursive===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/ZYCqm7p/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/WIL3oAwYSRy4Kl918u13CA Scastie (remote JVM)].

```Scala
import scala.annotation.tailrec
import scala.compat.Platform.currentTime

object Palindrome23 extends App {
  private val executionStartTime = currentTime
  private val st: Stream[(Int, Long)] = (0, 1L) #:: st.map(xs => nextPalin3(xs._1))

  @tailrec
  private def nextPalin3(n: Int): (Int, Long) = {

    @inline
    def isPali2(i: BigInt): Boolean = {
      val s = i.toString(2)
      if ((s.length & 1) == 0) false else s == s.reverse
    }

    def palin3(i: BigInt): Long = {
      val n3 = i.toString(3)
      java.lang.Long.parseLong(n3 + "1" + n3.reverse, 3)
    }

    val actual: Long = palin3(n)
    if (isPali2(actual)) (n + 1, actual) else nextPalin3(n + 1)
  }

  println(f"${"Decimal"}%18s${"Binary"}%35s${"Ternary"}%51s")
  (Stream(0L) ++ st.map(_._2)).take(6).foreach(n => {
    val bigN = BigInt(n)
    val (bin, ter) = (bigN.toString(2), bigN.toString(3))

    println(f"${n}%18d, ${
      bin + " " * ((60 - bin.length) / 2)}%60s, ${
      ter + " " * ((37 - ter.length) / 2)}%37s")
  })

  println(s"Successfully completed without errors. [total ${currentTime - executionStartTime} ms]")

}
```


===Fastest and high yields (17) solution 😏===
{{Out}}Best seen running in your browser either by [https://scastie.scala-lang.org/en0ZiqDETCuWO6avhTi9YQ Scastie (remote JVM)].

```Scala
import scala.io.Source

object FastPalindrome23 extends App {

  val rawText = Source.fromURL("http://oeis.org/A060792/b060792.txt")
  var count = 0

  rawText.getLines().map(_.split(" "))
    .foreach(s => {
      val n = BigInt(s(1))
      val (bin, ter) = (n.toString(2), n.toString(3))

      count += 1
      println(
        f"Decimal : ${n}%-44d , Central binary digit: ${bin(bin.length / 2)}")
      println(f"Binary  : ${bin}")
      println(f"Ternary : ${ter + " " * ((91 - ter.length) / 2)}%91s")
      println(f"Central : ${"^"}%46s%n---%n")
    })

  println(s"${count} palindromes found.")

}
```

```txt
Decimal : 0                                            , Central binary digit: 0
Binary  : 0
Ternary :                                              0
Central :                                              ^
---
Decimal : 1                                            , Central binary digit: 1
Binary  : 1
Ternary :                                              1
Central :                                              ^
---
Decimal : 6643                                         , Central binary digit: 1
Binary  : 1100111110011
Ternary :                                          100010001
Central :                                              ^
---
Decimal : 1422773                                      , Central binary digit: 1
Binary  : 101011011010110110101
Ternary :                                        2200021200022
Central :                                              ^
---
Decimal : 5415589                                      , Central binary digit: 0
Binary  : 10100101010001010100101
Ternary :                                       101012010210101
Central :                                              ^
---
Decimal : 90396755477                                  , Central binary digit: 0
Binary  : 1010100001100000100010000011000010101
Ternary :                                   22122022220102222022122
Central :                                              ^
---
Decimal : 381920985378904469                           , Central binary digit: 0
Binary  : 10101001100110110110001110011011001110001101101100110010101
Ternary :                            2112200222001222121212221002220022112
Central :                                              ^
---
Decimal : 1922624336133018996235                       , Central binary digit: 0
Binary  : 11010000011100111000101110001110011011001110001110100011100111000001011
Ternary :                        122120102102011212112010211212110201201021221
Central :                                              ^
---
Decimal : 2004595370006815987563563                    , Central binary digit: 1
Binary  : 110101000011111010101010100101111011110111011110111101001010101010111110000101011
Ternary :                     221010112100202002120002212200021200202001211010122
Central :                                              ^
---
Decimal : 8022581057533823761829436662099              , Central binary digit: 1
Binary  : 1100101010000100101101110000011011011111111011000011100001101111111101101100000111011010010000101010011
Ternary :              21000020210011222122220212010000100001021202222122211001202000012
Central :                                              ^
---
Decimal : 392629621582222667733213907054116073         , Central binary digit: 0
Binary  : 10010111001111000100010100010100000011011011000101011011100000111011010100011011011000000101000101000100011110011101001
Ternary :         122102120011102000101101000002010021111120010200000101101000201110021201221
Central :                                              ^
---
Decimal : 32456836304775204439912231201966254787       , Central binary digit: 1
Binary  : 11000011010101111010110010100010010011011010101001101000001000100010000010110010101011011001001000101001101011110101011000011
Ternary :       1222100201002211120110022121002012121101011212102001212200110211122001020012221
Central :                                              ^
---
Decimal : 428027336071597254024922793107218595973      , Central binary digit: 1
Binary  : 101000010000000110001000011111100101011110011100001110100011100010001110001011100001110011110101001111110000100011000000010000101
Ternary :      222001200110022102121001000200200202022111220202002002000100121201220011002100222
Central :                                              ^
---
Decimal : 1597863243206403857787246920544522912361     , Central binary digit: 0
Binary  : 10010110010000110010100010001000111010010000111000010010100010111011101000101001000011100001001011100010001000101001100001001101001
Ternary :     10121021220121202021201220210001211020122122102011210001202210212020212102212012101
Central :                                              ^
---
Decimal : 30412638162199251273509758127730300026189    , Central binary digit: 0
Binary  : 101100101011111111011010000110101101100101010100101101010000001000000000100000010101101001010101001101101011000010110111111110101001101
Ternary :    2112120210211212121000000011202022210210101010120122202021100000001212121120120212112
Central :                                              ^
---
Decimal : 32345684491703244980406880704479906642045    , Central binary digit: 0
Binary  : 101111100001110001100000011101111000001111001000110100111001010101101101010100111001011000100111100000111101110000001100011100001111101
Ternary :    2200221111210202000010122020002221112212101012122111222000202210100002020121111220022
Central :                                              ^
---
Decimal : 24014998963383302600955162866787153652444049 , Central binary digit: 0
Binary  : 1000100111010110110111101001100011100110100000011011001010100101011001100011001101010010101001101100000010110011100011001011110110110101110010001
Ternary : 2202021211210100110100002202101000110000220121210220000110001012022000010110010121121202022
Central :                                              ^
---
17 palindromes found.
```


## Scheme


```scheme
(import (scheme base)
        (scheme write)
        (srfi 1 lists)) ; use 'fold' from SRFI 1

;; convert number to a list of digits, in desired base
(define (r-number->list n base)
  (let loop ((res '())
             (num n))
    (if (< num base)
      (cons num res)
      (loop (cons (remainder num base) res)
            (quotient num base)))))

;; convert number to string, in desired base
(define (r-number->string n base)
  (apply string-append
         (map number->string
              (r-number->list n base))))

;; test if a list of digits is a palindrome
(define (palindrome? lst)
  (equal? lst (reverse lst)))

;; based on Perl/Ruby's insight
;; -- construct the ternary palindromes in order
;;    using fact that their central number is always a 1
;; -- convert into binary, and test if result is a palindrome too
(define (get-series size)
  (let loop ((results '(1 0))
             (i 1))
    (if (= size (length results))
      (reverse results)
      (let* ((n3 (r-number->list i 3))
             (n3-list (append n3 (list 1) (reverse n3)))
             (n10 (fold (lambda (d t) (+ d (* 3 t))) 0 n3-list))
             (n2 (r-number->list n10 2)))
        (loop (if (palindrome? n2)
                (cons n10 results)
                results)
              (+ 1 i))))))

;; display final results, in bases 10, 2 and 3.
(for-each
  (lambda (n)
    (display
      (string-append (number->string n)
                     " in base 2: "
                     (r-number->string n 2)
                     " in base 3: "
                     (r-number->string n 3)))
    (newline))
  (get-series 6))
```


```txt
0 in base 2: 0 in base 3: 0
1 in base 2: 1 in base 3: 1
6643 in base 2: 1100111110011 in base 3: 100010001
1422773 in base 2: 101011011010110110101 in base 3: 2200021200022
5415589 in base 2: 10100101010001010100101 in base 3: 101012010210101
90396755477 in base 2: 1010100001100000100010000011000010101 in base 3: 22122022220102222022122
```



## Sidef

```ruby
var format = "%11s %24s %38s\n"
format.printf("decimal", "ternary", "binary")
format.printf(0, 0, 0)

for n in (0 .. 2e5) {
    var pal = n.base(3)||''
    var b3 = (pal + '1' + pal.flip)
    var b2 = Num(b3, 3).base(2)
    if (b2 == b2.flip) {
        format.printf(Num(b2, 2), b3, b2)
    }
}
```

```txt
    decimal                  ternary                                 binary
          0                        0                                      0
          1                        1                                      1
       6643                100010001                          1100111110011
    1422773            2200021200022                  101011011010110110101
    5415589          101012010210101                10100101010001010100101
90396755477  22122022220102222022122  1010100001100000100010000011000010101
```



## Tcl

We can use <tt>[format %b]</tt> to format a number as binary, but ternary requires a custom proc:

```Tcl
proc format_%t {n} {
    while {$n} {
        append r [expr {$n % 3}]
        set n [expr {$n / 3}]
    }
    if {![info exists r]} {set r 0}
    string reverse $r
}
```


Identifying palindromes is simple.  This form is O(n) with a large constant factor, but good enough:


```Tcl
proc pal? {s} {expr {$s eq [string reverse $s]}}
```


The naive approach turns out to be very slow:

```Tcl
proc task {{find 6}} {
    for {set i 0} {$find} {incr i} {
        set b [format %b $i]
        set t [format_%t $i]
        if {[pal? $b] && [pal? $t]} {
            puts "Palindrome: $i ($b) ($t)"
            incr find -1
        }
    }
}

puts [time {task 4}]
```

```txt
Palindrome: 0 (0) (0)
Palindrome: 1 (1) (1)
Palindrome: 6643 (1100111110011) (100010001)
Palindrome: 1422773 (101011011010110110101) (2200021200022)
21944474 microseconds per iteration
```


22 seconds for only the first four elements .. not good enough!
We can do much better than that by naively iterating the binary palindromes.  This is nice to do in a coroutine:


```Tcl
package require Tcl 8.5 ;# for coroutines

proc 2pals {} {
    yield 0
    yield 1
    while 1 {
        incr i
        set a [format %b $i]
        set b [string reverse $a]
        yield ${a}$b
        yield ${a}0$b
        yield ${a}1$b
    }
}
```


The binary strings emitted by this generator are not in increasing order, but for this particular task, that turns out to be unimportant.

Our main loop needs only minor changes:

```Tcl
proc task {{find 6}} {
    coroutine gen apply {{} {yield; 2pals}}
    while {$find} {
        set b [gen]
        set i [scan $b %b]
        set t [format_%t $i]
        if {[pal? $t]} {
            puts "Palindrome: $i ($b) ($t)"
            incr find -1
        }
    }
    rename gen {}
}

puts [time task]
```

This version finds the first 6 in under 4 seconds, which is good enough for the task at hand:
```txt
Palindrome: 0 (0) (0)
Palindrome: 1 (1) (1)
Palindrome: 6643 (1100111110011) (100010001)
Palindrome: 1422773 (101011011010110110101) (2200021200022)
Palindrome: 5415589 (10100101010001010100101) (101012010210101)
Palindrome: 90396755477 (1010100001100000100010000011000010101) (22122022220102222022122)
3643152 microseconds per iteration
```


Plenty more optimisations are possible!  Exploiting the observations in Ruby's implementation should make the 7th element reachable in reasonable time ...


## VBA


```vb
Public Declare Function GetTickCount Lib "kernel32.dll" () As Long
'palindromes both in base3 and base2
'using Decimal data type to find number 6 and 7, although slowly
Private Function DecimalToBinary(DecimalNum As Long) As String
    Dim tmp As String
    Dim n As Long

    n = DecimalNum

    tmp = Trim(CStr(n Mod 2))
    n = n \ 2

    Do While n <> 0
    tmp = Trim(CStr(n Mod 2)) & tmp
    n = n \ 2
    Loop

    DecimalToBinary = tmp
End Function
Function Dec2Bin(ByVal DecimalIn As Variant, _
              Optional NumberOfBits As Variant) As String
    Dec2Bin = ""
    DecimalIn = Int(CDec(DecimalIn))
    Do While DecimalIn <> 0
        Dec2Bin = Format$(DecimalIn - 2 * Int(DecimalIn / 2)) & Dec2Bin
        DecimalIn = Int(DecimalIn / 2)
    Loop
    If Not IsMissing(NumberOfBits) Then
       If Len(Dec2Bin) > NumberOfBits Then
          Dec2Bin = "Error - Number exceeds specified bit size"
       Else
          Dec2Bin = Right$(String$(NumberOfBits, _
                    "0") & Dec2Bin, NumberOfBits)
       End If
    End If
End Function
Public Sub base()
    'count integer n from 0 upwards
    'display representation in base 3

    Time1 = GetTickCount
    Dim n As Long
    Dim three(19) As Integer
    Dim pow3(19) As Variant
    Dim full3 As Variant
    Dim trail As Variant
    Dim check As Long
    Dim len3 As Integer
    Dim carry As Boolean
    Dim i As Integer, j As Integer
    Dim s As String
    Dim t As String
    pow3(0) = CDec(1)
    For i = 1 To 19
        pow3(i) = 3 * pow3(i - 1)
    Next i
    Debug.Print String$(5, " "); "iter"; String$(7, " "); "decimal"; String$(18, " "); "binary";
    Debug.Print String$(30, " "); "ternary"
    n = 0: full3 = 0: t = "0": s = "0"
    Debug.Print String$(8 - Len(CStr(n)), " "); n; String$(12 - Len(CStr(full3)), " ");
    Debug.Print full3; String$((41 - Len(t)) / 2, " "); t; String$((41 - Len(t)) / 2, " ");
    Debug.Print String$((31 - Len(s)) / 2, " "); s
    n = 0: full3 = 1: t = "1": s = "1"
    Debug.Print String$(8 - Len(CStr(n)), " "); n; String$(12 - Len(CStr(full3)), " ");
    Debug.Print full3; String$((41 - Len(t)) / 2, " "); t; String$((41 - Len(t)) / 2, " ");
    Debug.Print String$((31 - Len(s)) / 2, " "); s
    number = 0
    n = 1
    len3 = 0
    full3 = 3
    Do 'For n = 1 To 200000 '20000000 takes 1000 seconds and number 7 not found yet
        three(0) = three(0) + 1
        carry = False
        If three(0) = 3 Then
            three(0) = 0
            carry = True
            j = 1
            Do While carry
                three(j) = three(j) + 1
                If three(j) = 3 Then
                    three(j) = 0
                    j = j + 1
                Else
                    carry = False
                End If
            Loop
            If len3 < j Then
                trail = full3 - (n - 1) * pow3(len3 + 2) - pow3(len3 + 1)
                len3 = j
                full3 = n * pow3(len3 + 2) + pow3(len3 + 1) + 3 * trail
                For i = 0 To j - 1
                    full3 = full3 - 2 * pow3(len3 - i)
                Next i
                full3 = full3 + 1 'as j=len3 now and 1=pow3(len3 - j)
            Else
                full3 = full3 + pow3(len3 + 2)
                For i = 0 To j - 1
                    full3 = full3 - 2 * pow3(len3 - i)
                Next i
                full3 = full3 + pow3(len3 - j)
            End If
        Else
            full3 = full3 + pow3(len3 + 2) + pow3(len3)
        End If
        s = ""
        For i = 0 To len3
            s = s & CStr(three(i))
        Next i
        'do we have a hit?
        t = Dec2Bin(full3) 'CStr(DecimalToBinary(full3))
        If t = StrReverse(t) Then
            'we have a hit
            number = number + 1
            s = StrReverse(s) & "1" & s
            If n < 200000 Then
                Debug.Print String$(8 - Len(CStr(n)), " "); n; String$(12 - Len(CStr(full3)), " ");
                Debug.Print full3; String$((41 - Len(t)) / 2, " "); t; String$((41 - Len(t)) / 2, " ");
                Debug.Print String$((31 - Len(s)) / 2, " "); s
                If number = 4 Then
                    Debug.Print "Completed in"; (GetTickCount - Time1) / 1000; "seconds"
                    Time2 = GetTickCount
                    Application.ScreenUpdating = False
                End If
            Else
                Debug.Print n, full3, Len(t), t, Len(s), s
                Debug.Print "Completed in"; (Time2 - Time1) / 1000; "seconds";
                Time3 = GetTickCount
            End If
        End If
        n = n + 1
    Loop Until number = 5 'Next n
    Debug.Print "Completed in"; (Time3 - Time1) / 1000; "seconds"
    Application.ScreenUpdating = True
End Sub
```
```txt
'     iter       decimal                  binary                              ternary
'        0             0                     0                                   0
'        0             1                     1                                   1
'       27          6643               1100111110011                         100010001
'      650       1422773           101011011010110110101                   2200021200022
'      825       5415589          10100101010001010100101                 101012010210101
'   170097   90396755477   1010100001100000100010000011000010101      22122022220102222022122
'Completed in 5,14 seconds
' 328601606     381920985378904469          59           10101001100110110110001110011011001110001101101100110010101            37           2112200222001222121212221002220022112
Completed in 5,14 secondsCompleted in 16394,64 seconds

```



## zkl

VERY slow after six but does find it.

```zkl
fcn pal23W{  //--> iterator returning (index,palindromic number)
   Walker.tweak(fcn(ri,r){  // references to loop start and count of palindromes
      foreach i in ([ri.value..*]){
	 n3:=i.toString(3);
	 n:=String(n3,"1",n3.reverse()).toInt(3);  // create base 3 palindrome
	 n2:= n.toString(2);
	 if(n2.len().isOdd and n2==n2.reverse()){  // stop here, return answer
	    ri.set(i+1);    // continue loop from this value at next iteration
	    return(r.inc(),n);
	 }
      }
   }.fp(Ref(3),Ref(3))).push(T(1,0),T(2,1))  // seed with first two results
}
```


```zkl
foreach idx,n in (pal23W().walk(6)){
   println("%2d: %,d == %.3B(3) == %.2B(2)".fmt(idx,n,n,n))
}
```

```txt

 1: 0 == 0(3) == 0(2)
 2: 1 == 1(3) == 1(2)
 3: 6,643 == 100010001(3) == 1100111110011(2)
 4: 1,422,773 == 2200021200022(3) == 101011011010110110101(2)
 5: 5,415,589 == 101012010210101(3) == 10100101010001010100101(2)
 6: 90,396,755,477 == 22122022220102222022122(3) == 1010100001100000100010000011000010101(2)

```

