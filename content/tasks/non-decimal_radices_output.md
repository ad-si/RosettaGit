+++
title = "Non-decimal radices/Output"
description = ""
date = 2018-08-29T21:30:32Z
aliases = []
[extra]
id = 3326
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "auto_hotkey",
  "awk",
  "bbc_basic",
  "bc",
  "c",
  "c_sharp",
  "c_plus_plus",
  "clojure",
  "common_lisp",
  "d",
  "e",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gema",
  "go",
  "haskell",
  "hic_est",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "locomotive_basic",
  "lua",
  "mathematica",
  "net_rexx",
  "nim",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "pico_lisp",
  "pl_i",
  "power_shell",
  "pure_basic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "standard_ml",
  "tcl",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

Programming languages often have built-in routines to convert a non-negative integer for printing in different number bases. Such common number bases might include binary, [[Octal]] and [[Hexadecimal]].


## Task

Print a small range of integers in some different bases, as supported by standard routines of your programming language.


;Note:
This is distinct from [[Number base conversion]] as a user-defined conversion function is '''not''' asked for.)

The reverse operation is [[Common number base parsing]].





## Ada


```ada
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Text_IO;          use Ada.Text_IO;

procedure Test_Integer_Text_IO is
begin
  for I in 1..33 loop
    Put (I, Width =>3, Base=> 10);
    Put (I, Width =>7, Base=> 16);
    Put (I, Width =>6, Base=>  8);
    New_Line;
  end loop;
end Test_Integer_Text_IO;
```

Sample output:
<pre style="height:30ex;overflow:scroll">
  1  16#1#  8#1#
  2  16#2#  8#2#
  3  16#3#  8#3#
  4  16#4#  8#4#
  5  16#5#  8#5#
  6  16#6#  8#6#
  7  16#7#  8#7#
  8  16#8# 8#10#
  9  16#9# 8#11#
 10  16#A# 8#12#
 11  16#B# 8#13#
 12  16#C# 8#14#
 13  16#D# 8#15#
 14  16#E# 8#16#
 15  16#F# 8#17#
 16 16#10# 8#20#
 17 16#11# 8#21#
 18 16#12# 8#22#
 19 16#13# 8#23#
 20 16#14# 8#24#
 21 16#15# 8#25#
 22 16#16# 8#26#
 23 16#17# 8#27#
 24 16#18# 8#30#
 25 16#19# 8#31#
 26 16#1A# 8#32#
 27 16#1B# 8#33#
 28 16#1C# 8#34#
 29 16#1D# 8#35#
 30 16#1E# 8#36#
 31 16#1F# 8#37#
 32 16#20# 8#40#
 33 16#21# 8#41#

```



## Aime


```aime
o_xinteger(16, 1000000);
o_byte('\n');
o_xinteger(5, 1000000);
o_byte('\n');
o_xinteger(2, 1000000);
o_byte('\n');
```



## ALGOL 68

```algol68
main:(
  FOR i TO 33 DO
    printf(($10r6d," "16r6d," "8r6dl$, BIN i, BIN i, BIN i))
  OD
)
```

Sample output:

```txt

000001 000001 000001
000002 000002 000002
000003 000003 000003
000004 000004 000004
000005 000005 000005
000006 000006 000006
000007 000007 000007
000008 000008 000010
000009 000009 000011
000010 00000a 000012
000011 00000b 000013
000012 00000c 000014
000013 00000d 000015
000014 00000e 000016
000015 00000f 000017
000016 000010 000020
000017 000011 000021
000018 000012 000022
000019 000013 000023
000020 000014 000024
000021 000015 000025
000022 000016 000026
000023 000017 000027
000024 000018 000030
000025 000019 000031
000026 00001a 000032
000027 00001b 000033
000028 00001c 000034
000029 00001d 000035
000030 00001e 000036
000031 00001f 000037
000032 000020 000040
000033 000021 000041

```



## ALGOL W

Algol W has a standard procedure intbase16 that returns its parameter converted to a string in hexadecimal.

```algolw
begin
    % print some numbers in hex %
    for i := 0 until 20 do write( intbase16( i ) )
end.
```

```txt

    00000000
    00000001
    00000002
    00000003
    00000004
    00000005
    00000006
    00000007
    00000008
    00000009
    0000000A
    0000000B
    0000000C
    0000000D
    0000000E
    0000000F
    00000010
    00000011
    00000012
    00000013
    00000014

```



## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276235.html#276235 forum]

```AutoHotkey
MsgBox % BC("FF",16,3) ; -> 100110 in base 3 = FF in hex = 256 in base 10

BC(NumStr,InputBase=8,OutputBase=10) {
  Static S = 12345678901234567890123456789012345678901234567890123456789012345
  DllCall("msvcrt\_i64toa","Int64",DllCall("msvcrt\_strtoui64","Str",NumStr,"Uint",0,"UInt",InputBase,"CDECLInt64"),"Str",S,"UInt",OutputBase,"CDECL")
  Return S
}
```



## AWK

C's printf() is just exposed:

```awk
$ awk '{printf("%d 0%o 0x%x\n",$1,$1,$1)}'
10
10 012 0xa
16
16 020 0x10
255
255 0377 0xff
```



## BBC BASIC


```bbcbasic
      REM STR$ converts to a decimal string:
      PRINT STR$(0)
      PRINT STR$(123456789)
      PRINT STR$(-987654321)

      REM STR$~ converts to a hexadecimal string:
      PRINT STR$~(43981)
      PRINT STR$~(-1)
```

'''Output:'''

```txt

0
123456789
-987654321
ABCD
FFFFFFFF

```



## Bc

Variable <code>obase</code> is the base for all output.  It can be 2 (binary) up to some implementation-dependent limit.  In [[GNU bc]] the limit may be large, for example 2^31, with "digits" of bases bigger than 36 printed as individual decimal numbers.
```Bc

for(i=1;i<10;i++) {
  obase=10; print i," "
  obase=8; print i," "
  obase=3; print i," "
  obase=2; print i
  print "\n"
}
```



## C



```c
#include <stdio.h>

int main()
{
  int i;

  for(i=1; i <= 33; i++)
    printf("%6d %6x %6o\n", i, i, i);

  return 0;
}
```


Binary conversion using <tt>%b</tt> is not standard.


## C#


```c#

using System;

namespace NonDecimalRadicesOutput
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i = 0; i < 42; i++)
            {
                string binary = Convert.ToString(i, 2);
                string octal = Convert.ToString(i, 8);
                string hexadecimal = Convert.ToString(i, 16);
                Console.WriteLine(string.Format("Decimal: {0}, Binary: {1}, Octal: {2}, Hexadecimal: {3}", i, binary, octal, hexadecimal));
            }

            Console.ReadKey();
        }
    }
}

```

```txt

Decimal: 0, Binary: 0, Octal: 0, Hexadecimal: 0
Decimal: 1, Binary: 1, Octal: 1, Hexadecimal: 1
Decimal: 2, Binary: 10, Octal: 2, Hexadecimal: 2
Decimal: 3, Binary: 11, Octal: 3, Hexadecimal: 3
Decimal: 4, Binary: 100, Octal: 4, Hexadecimal: 4
Decimal: 5, Binary: 101, Octal: 5, Hexadecimal: 5
Decimal: 6, Binary: 110, Octal: 6, Hexadecimal: 6
Decimal: 7, Binary: 111, Octal: 7, Hexadecimal: 7
Decimal: 8, Binary: 1000, Octal: 10, Hexadecimal: 8
Decimal: 9, Binary: 1001, Octal: 11, Hexadecimal: 9
Decimal: 10, Binary: 1010, Octal: 12, Hexadecimal: a
Decimal: 11, Binary: 1011, Octal: 13, Hexadecimal: b
Decimal: 12, Binary: 1100, Octal: 14, Hexadecimal: c
Decimal: 13, Binary: 1101, Octal: 15, Hexadecimal: d
Decimal: 14, Binary: 1110, Octal: 16, Hexadecimal: e
Decimal: 15, Binary: 1111, Octal: 17, Hexadecimal: f
Decimal: 16, Binary: 10000, Octal: 20, Hexadecimal: 10
Decimal: 17, Binary: 10001, Octal: 21, Hexadecimal: 11
Decimal: 18, Binary: 10010, Octal: 22, Hexadecimal: 12
Decimal: 19, Binary: 10011, Octal: 23, Hexadecimal: 13
Decimal: 20, Binary: 10100, Octal: 24, Hexadecimal: 14
Decimal: 21, Binary: 10101, Octal: 25, Hexadecimal: 15
Decimal: 22, Binary: 10110, Octal: 26, Hexadecimal: 16
Decimal: 23, Binary: 10111, Octal: 27, Hexadecimal: 17
Decimal: 24, Binary: 11000, Octal: 30, Hexadecimal: 18
Decimal: 25, Binary: 11001, Octal: 31, Hexadecimal: 19
Decimal: 26, Binary: 11010, Octal: 32, Hexadecimal: 1a
Decimal: 27, Binary: 11011, Octal: 33, Hexadecimal: 1b
Decimal: 28, Binary: 11100, Octal: 34, Hexadecimal: 1c
Decimal: 29, Binary: 11101, Octal: 35, Hexadecimal: 1d
Decimal: 30, Binary: 11110, Octal: 36, Hexadecimal: 1e
Decimal: 31, Binary: 11111, Octal: 37, Hexadecimal: 1f
Decimal: 32, Binary: 100000, Octal: 40, Hexadecimal: 20
Decimal: 33, Binary: 100001, Octal: 41, Hexadecimal: 21
Decimal: 34, Binary: 100010, Octal: 42, Hexadecimal: 22
Decimal: 35, Binary: 100011, Octal: 43, Hexadecimal: 23
Decimal: 36, Binary: 100100, Octal: 44, Hexadecimal: 24
Decimal: 37, Binary: 100101, Octal: 45, Hexadecimal: 25
Decimal: 38, Binary: 100110, Octal: 46, Hexadecimal: 26
Decimal: 39, Binary: 100111, Octal: 47, Hexadecimal: 27
Decimal: 40, Binary: 101000, Octal: 50, Hexadecimal: 28
Decimal: 41, Binary: 101001, Octal: 51, Hexadecimal: 29

```


Binary conversion is not standard.


## C++



```cpp
#include <iostream>
#include <iomanip>

int main()
{
  for (int i = 0; i <= 33; i++)
    std::cout << std::setw(6) << std::dec << i << " "
              << std::setw(6) << std::hex << i << " "
              << std::setw(6) << std::oct << i << std::endl;

  return 0;
}
```



## Clojure

Clojure eschews duplicating functionality already present in Java when interop is sufficiently idiomatic:

```lisp
(Integer/toBinaryString 25) ; returns "11001"
(Integer/toOctalString 25)  ; returns "31"
(Integer/toHexString 25)    ; returns "19"

(dotimes [i 20]
  (println (Integer/toHexString i)))
```



## Common Lisp


```lisp
(loop for n from 0 to 33 do
  (format t " ~6B ~3O ~2D ~2X~%" n n n n))
```



## D


```d
import std.stdio;

void main() {
    writeln("Base: 2      8     10     16");
    writeln("----------------------------");
    foreach (i; 0 .. 34)
        writefln(" %6b %6o %6d %6x", i, i, i, i);
}
```

```txt
Base: 2      8     10     16
----------------------------
      0      0      0      0
      1      1      1      1
     10      2      2      2
     11      3      3      3
    100      4      4      4
    101      5      5      5
    110      6      6      6
    111      7      7      7
   1000     10      8      8
   1001     11      9      9
   1010     12     10      a
   1011     13     11      b
   1100     14     12      c
   1101     15     13      d
   1110     16     14      e
   1111     17     15      f
  10000     20     16     10
  10001     21     17     11
  10010     22     18     12
  10011     23     19     13
  10100     24     20     14
  10101     25     21     15
  10110     26     22     16
  10111     27     23     17
  11000     30     24     18
  11001     31     25     19
  11010     32     26     1a
  11011     33     27     1b
  11100     34     28     1c
  11101     35     29     1d
  11110     36     30     1e
  11111     37     31     1f
 100000     40     32     20
 100001     41     33     21
```


### Tango Version

Number following formatting character is width. When no formatting
character is specified it is inferred from variable's type.

```d
for (int i = 0; i < 35; i++)
    Stdout.formatln ("{:b8} {:o3} {} {:x2}", i, i, i, i);
```



## E


```e
for value in 0..33 {
  for base in [2, 8, 10, 12, 16, 36] {
    def s := value.toString(base)
    print(" " * (8 - s.size()), s)
  }
  println()
}
```



## Elixir


```elixir
Enum.each(0..32, fn i -> :io.format "~2w :~6.2B, ~2.8B, ~2.16B~n", [i,i,i,i] end)
```


<pre style="height: 32ex; overflow: scroll">
 0 :     0,  0,  0
 1 :     1,  1,  1
 2 :    10,  2,  2
 3 :    11,  3,  3
 4 :   100,  4,  4
 5 :   101,  5,  5
 6 :   110,  6,  6
 7 :   111,  7,  7
 8 :  1000, 10,  8
 9 :  1001, 11,  9
10 :  1010, 12,  A
11 :  1011, 13,  B
12 :  1100, 14,  C
13 :  1101, 15,  D
14 :  1110, 16,  E
15 :  1111, 17,  F
16 : 10000, 20, 10
17 : 10001, 21, 11
18 : 10010, 22, 12
19 : 10011, 23, 13
20 : 10100, 24, 14
21 : 10101, 25, 15
22 : 10110, 26, 16
23 : 10111, 27, 17
24 : 11000, 30, 18
25 : 11001, 31, 19
26 : 11010, 32, 1A
27 : 11011, 33, 1B
28 : 11100, 34, 1C
29 : 11101, 35, 1D
30 : 11110, 36, 1E
31 : 11111, 37, 1F
32 :100000, 40, 20

```



## Erlang

Printing 63 (decimal) in some different bases (here: 3,8,16,26). The base can be 2..36.
```txt

4> [io:fwrite("~s ", [erlang:integer_to_list(63, X)]) || X <- [3,8,16,26]].
2100 77 3F 2B

```



## Euphoria


```euphoria
for i = 1 to 33 do
    printf(1,"%6d %6x %6o\n",{i,i,i})
end for
```


=={{header|F_Sharp|F#}}==
<p>Base 8, 10 and 16 can be output by <code>printf</code></p>

```fsharp
let ns = [30..33]
ns |> Seq.iter (fun n -> printfn " %3o %2d %2X" n n n)
```

```txt
 36 30 1E
 37 31 1F
 40 32 20
 41 33 21
```

<p>The .NET library <code>System.Convert</code> is able to also convert from and to base 2</p>

```fsharp
let bases = [2; 8; 10; 16]

ns |> Seq.map (fun n -> Seq.initInfinite (fun i -> n))
|> Seq.map (fun s -> Seq.zip s bases)
|> Seq.map (Seq.map System.Convert.ToString >> Seq.toList)
|> Seq.iter (fun s -> (printfn "%6s %2s %2s %2s" s.[0] s.[1] s.[2] s.[3]))
```

```txt
 11110 36 30 1e
 11111 37 31 1f
100000 40 32 20
100001 41 33 21
```



## Factor


```factor
1234567 2 36 [a,b] [ >base print ] with each
```

<pre style="height:30ex;overflow:scroll">
100101101011010000111
2022201111201
10231122013
304001232
42243331
13331215
4553207
2281451
1234567
773604
4b6547
342c19
241cb5
195be7
12d687
ed4ea
bdc71
98ig4
7e687
6769j
55kgf
49ahj
3h787
3407h
2i679
28jdj
206jj
1lhs8
1flm7
1adkn
15lk7
11bm4
vdwr
srsc
qglj

```



## Forth

GNU Forth has convenience functions for printing an integer in decimal or hex, regardless of the current BASE.

```forth
: main 34 1 do cr i dec. i hex. loop ;
main
...
11 $B
...
```

This is not standardized because such functions are very easy to define as needed:

```forth
: base. ( n base -- ) base @ >r  base !  .  r> base ! ;
: oct. ( n -- ) 8 base. ;
: bin. ( n -- ) 2 base. ;
```



## Fortran

```fortran
do n = 1, 33
  write(*, "(b6, o4, i4, z4)") n, n, n, n
end do
```



## FreeBASIC

FreeBASIC has built in functions called Hex, Str, Oct and Bin which convert decimal numbers into hexadecimal, decimal,
octal and binary strings respectively. Here's an example:

```freebasic
' FB 1.05.0 Win64

Dim ui(1 To 4) As UInteger = {10, 26, 52, 100}
Print "Decimal    Hex       Octal    Binary"
Print "
### ====  ========   =======   ===
"
For i As Integer = 1 To 4
  Print Str(ui(i)); Tab(12); Hex(ui(i)); Tab(23); Oct(ui(i)); Tab(31); Bin(ui(i))
Next

Sleep
```


```txt

Decimal    Hex       Octal    Binary

### ====  ========   =======   ===

10         A          12      1010
26         1A         32      11010
52         34         64      110100
100        64         144     1100100

```



## Gema

After decimal numbers in the input stream, add hexadecimal and octal of the same number in the output stream. Also after hexadecimal add decimal and octal, and after octal add decimal and hexadecimal.

```gema
0x<A>
=$0 (@radix{16;10;$1}, 0@radix{16;8;$1})
0<D>=$0 (@radix{8;10;$1}, 0x@radix{8;16;$1})
<D>=$0 (0x@radix{10;16;$1}, 0@radix{10;8;$1})
```

Invocation and sample input and output

```txt
$ gema -p radix.gema
The 99 beers and 0x2D Scotches.
The 99 (0x63, 0143) beers and 0x2D (45, 055) Scotches.
```


## Go


```go
package main

import (
    "fmt"
    "math/big"
    "strconv"
)

func main() {
    // fmt.Print formats integer types directly as bases 2, 8, 10, and 16.
    fmt.Printf("%b\n", 13)
    fmt.Printf("%o\n", 13)
    fmt.Printf("%d\n", 13)
    fmt.Printf("%x\n", 13)
    // big ints work with fmt as well.
    d := big.NewInt(13)
    fmt.Printf("%b\n", d)
    fmt.Printf("%o\n", d)
    fmt.Printf("%d\n", d)
    fmt.Printf("%x\n", d)
    // strconv.FormatInt handles arbitrary bases from 2 to 36 for the
    // int64 type.  There is also strconv.FormatUInt for the uint64 type.
    // There no equivalent for big ints.
    fmt.Println(strconv.FormatInt(1313, 19))
}
```

```txt

1101
15
13
d
1101
15
13
d
3c2

```



## Haskell


```haskell
import Text.Printf

main :: IO ()
main = mapM_ f [0..33] where
  f :: Int -> IO ()
  f n = printf " %3o %2d %2X\n" n n n -- binary not supported
```


alternately, without <code>Text.Printf</code>:

```haskell
import Numeric

main :: IO ()
main = mapM_ f [0..33] where
  f :: Int -> IO ()
  f n = putStrLn $ " " ++ showOct n "" ++ " " ++ show n ++ " " ++ showHex n ""
```


Or, generalising and tabulating a little:

```haskell
import Data.List (unfoldr, transpose, intercalate)
import Data.Array (Array, listArray, (!))
import Data.Monoid ((<>))


-- ARBITRARY RADICES ---------------------------------------
bases :: [Int]
bases = abs <$> [2, 7, 8, 10, 12, 16, 32]

tableRows :: [[String]]
tableRows = ((([baseDigits] <*> bases) <*>) . return) <$> [1 .. 33]

digits :: Array Int Char
digits = listArray (0, 35) (['0' .. '9'] <> ['A' .. 'Z'])

baseDigits :: Int -> Int -> String
baseDigits base
  | base > 36 = const "Needs glyphs beyond Z"
  | otherwise = reverse . unfoldr remQuot
  where
    remQuot 0 = Nothing
    remQuot n =
      let (q, r) = quotRem n base
      in Just (digits ! r, q)

-- TEST AND TABULATION-------------------------------------
table :: String -> [[String]] -> [String]
table delim rows =
  intercalate delim <$>
  transpose
    ((fmap =<< flip justifyRight ' ' . maximum . fmap length) <$> transpose rows)

justifyRight :: Int -> Char -> String -> String
justifyRight n c s = drop (length s) (replicate n c <> s)

main :: IO ()
main =
  mapM_
    putStrLn
    (table " " (([fmap show, fmap $ const "----"] <*> [bases]) <> tableRows))
```

```txt
     2    7    8   10   12   16   32
  ---- ---- ---- ---- ---- ---- ----
     1    1    1    1    1    1    1
    10    2    2    2    2    2    2
    11    3    3    3    3    3    3
   100    4    4    4    4    4    4
   101    5    5    5    5    5    5
   110    6    6    6    6    6    6
   111   10    7    7    7    7    7
  1000   11   10    8    8    8    8
  1001   12   11    9    9    9    9
  1010   13   12   10    A    A    A
  1011   14   13   11    B    B    B
  1100   15   14   12   10    C    C
  1101   16   15   13   11    D    D
  1110   20   16   14   12    E    E
  1111   21   17   15   13    F    F
 10000   22   20   16   14   10    G
 10001   23   21   17   15   11    H
 10010   24   22   18   16   12    I
 10011   25   23   19   17   13    J
 10100   26   24   20   18   14    K
 10101   30   25   21   19   15    L
 10110   31   26   22   1A   16    M
 10111   32   27   23   1B   17    N
 11000   33   30   24   20   18    O
 11001   34   31   25   21   19    P
 11010   35   32   26   22   1A    Q
 11011   36   33   27   23   1B    R
 11100   40   34   28   24   1C    S
 11101   41   35   29   25   1D    T
 11110   42   36   30   26   1E    U
 11111   43   37   31   27   1F    V
100000   44   40   32   28   20   10
100001   45   41   33   29   21   11
```



## HicEst


```HicEst
DO n = 1, 33
  WRITE(Format="b6.0, o4.0, i4.0, z4.0") n, n, n, n
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==
Strictly speaking output conversion to different representations isn't built-in to Icon and Unicon; however, printf is included as part of the standard library.

```Icon
procedure main()
write("Non-decimal radices/Output")
every i := 255 | 2 | 5 | 16 do {
   printf("%%d = %d\n",i) # integer format
   printf("%%x = %x\n",i) # hex format
   printf("%%o = %o\n",i) # octal format
   printf("%%s = %s\n",i) # string format
   printf("%%i = %i\n",i) # image format
   }
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf, fprintf, and sprintf]
Output:
```txt
%d = 255
%x = ff
%o = 377
%s = 255
%i = 255
...
```



## J


J can natively break out numbers using a specific base


```j
   2 #.inv 12
1 1 0 0
   3 #.inv 100
1 0 2 0 1
   16 #.inv 180097588
10 11 12 1 2 3 4
```

However, this numeric representation would not satisfy most people's idea of "formatting", for most bases.  It might be useful, however, for bases less than 10:


```j
   8 #.inv 4009
7 6 5 1
   -.&' '": 8 #.inv 4009
7651
```

J also includes some explicit support for hexadecimal numbers


```j
  require 'convert'
   hfd 180097588
ABC1234
```

(and a few other hexadecimal related mechanisms which are not relevant here.)


## Java


```java5
public static void main(String args[]){
   for(int a= 0;a < 33;a++){
      System.out.println(Integer.toBinaryString(a));
      System.out.println(Integer.toOctalString(a));
      System.out.println(Integer.toHexString(a));
      //the above methods treat the integer as unsigned
      //there are also corresponding Long.to***String() methods for long's.

      System.out.printf("%3o %2d %2x\n",a ,a ,a); //printf like the other languages; binary not supported
   }
}
```



## JavaScript

The <code><i>number</i>.toString(<i>radix</i>)</code> method produces a string representation of a number in any radix between 2 and 36.


```javascript
var bases = [2, 8, 10, 16, 24];
for (var n = 0; n <= 33; n++) {
    var row = [];
    for (var i = 0; i < bases.length; i++)
        row.push( n.toString(bases[i]) );
    print(row.join(', '));
}
```


outputs
<pre style='height: 30ex; overflow: scroll'>0, 0, 0, 0, 0
1, 1, 1, 1, 1
10, 2, 2, 2, 2
11, 3, 3, 3, 3
100, 4, 4, 4, 4
101, 5, 5, 5, 5
110, 6, 6, 6, 6
111, 7, 7, 7, 7
1000, 10, 8, 8, 8
1001, 11, 9, 9, 9
1010, 12, 10, a, a
1011, 13, 11, b, b
1100, 14, 12, c, c
1101, 15, 13, d, d
1110, 16, 14, e, e
1111, 17, 15, f, f
10000, 20, 16, 10, g
10001, 21, 17, 11, h
10010, 22, 18, 12, i
10011, 23, 19, 13, j
10100, 24, 20, 14, k
10101, 25, 21, 15, l
10110, 26, 22, 16, m
10111, 27, 23, 17, n
11000, 30, 24, 18, 10
11001, 31, 25, 19, 11
11010, 32, 26, 1a, 12
11011, 33, 27, 1b, 13
11100, 34, 28, 1c, 14
11101, 35, 29, 1d, 15
11110, 36, 30, 1e, 16
11111, 37, 31, 1f, 17
100000, 40, 32, 20, 18
100001, 41, 33, 21, 19
```



## Julia

```julia
using Primes
println("Primes ≤ $hi written in common bases.")
@printf("%8s%8s%8s%8s", "bin", "oct", "dec", "hex")
for i in primes(50)
    @printf("%8s%8s%8s%8s\n", bin(i), oct(i), dec(i), hex(i))
end
```


```txt
Primes ≤ 50 written in common bases.
     bin     oct     dec     hex
      10       2       2       2
      11       3       3       3
     101       5       5       5
     111       7       7       7
    1011      13      11       b
    1101      15      13       d
   10001      21      17      11
   10011      23      19      13
   10111      27      23      17
   11101      35      29      1d
   11111      37      31      1f
  100101      45      37      25
  101001      51      41      29
  101011      53      43      2b
  101111      57      47      2f
```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val bases = intArrayOf(2, 8, 10, 16, 19, 36)
    for (base in bases) print("%6s".format(base))
    println()
    println("-".repeat(6 * bases.size))
    for (i in 0..35) {
        for (base in bases) print("%6s".format(i.toString(base)))
        println()
    }
}
```


```txt

     2     8    10    16    19    36
------------------------------------
     0     0     0     0     0     0
     1     1     1     1     1     1
    10     2     2     2     2     2
    11     3     3     3     3     3
   100     4     4     4     4     4
   101     5     5     5     5     5
   110     6     6     6     6     6
   111     7     7     7     7     7
  1000    10     8     8     8     8
  1001    11     9     9     9     9
  1010    12    10     a     a     a
  1011    13    11     b     b     b
  1100    14    12     c     c     c
  1101    15    13     d     d     d
  1110    16    14     e     e     e
  1111    17    15     f     f     f
 10000    20    16    10     g     g
 10001    21    17    11     h     h
 10010    22    18    12     i     i
 10011    23    19    13    10     j
 10100    24    20    14    11     k
 10101    25    21    15    12     l
 10110    26    22    16    13     m
 10111    27    23    17    14     n
 11000    30    24    18    15     o
 11001    31    25    19    16     p
 11010    32    26    1a    17     q
 11011    33    27    1b    18     r
 11100    34    28    1c    19     s
 11101    35    29    1d    1a     t
 11110    36    30    1e    1b     u
 11111    37    31    1f    1c     v
100000    40    32    20    1d     w
100001    41    33    21    1e     x
100010    42    34    22    1f     y
100011    43    35    23    1g     z

```



## Locomotive Basic



```locobasic
10 FOR i=1 TO 20
20 PRINT i,BIN$(i),HEX$(i)
30 NEXT
```


Output:

<pre style='height: 30ex; overflow: scroll'> 1           1            1
 2           10           2
 3           11           3
 4           100          4
 5           101          5
 6           110          6
 7           111          7
 8           1000         8
 9           1001         9
 10          1010         A
 11          1011         B
 12          1100         C
 13          1101         D
 14          1110         E
 15          1111         F
 16          10000        10
 17          10001        11
 18          10010        12
 19          10011        13
 20          10100        14
```



## Lua


```lua
for i = 1, 33 do
    print( string.format( "%o \t %d \t %x", i, i, i ) )
end
```



## Mathematica


```Mathematica
Scan[Print[IntegerString[#, 2], ",", IntegerString[#, 8],
",",#, ",",IntegerString[#, 16],",", IntegerString[#, 36]]&, Range[38]]
```


Output:


```txt

1,1,1,1,1
10,2,2,2,2
11,3,3,3,3
...
...
100010,42,34,22,y
100011,43,35,23,z
100100,44,36,24,10
100101,45,37,25,11
100110,46,38,26,12
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
fprintf('%3d  %3o  %3x\n',repmat(1:20,3,1))
```

Output:

```txt
  1    1    1
  2    2    2
  3    3    3
  4    4    4
  5    5    5
  6    6    6
  7    7    7
  8   10    8
  9   11    9
 10   12    a
 11   13    b
 12   14    c
 13   15    d
 14   16    e
 15   17    f
 16   20   10
 17   21   11
 18   22   12
 19   23   13
 20   24   14
```


=={{header|Modula-3}}==

```modula3
MODULE Conv EXPORTS Main;

IMPORT IO, Fmt;

BEGIN
  FOR i := 1 TO 33 DO
    IO.Put(Fmt.Int(i, base := 10) & " ");
    IO.Put(Fmt.Int(i, base := 16) & " ");
    IO.Put(Fmt.Int(i, base := 8) & " ");
    IO.Put("\n");
  END;
END Conv.
```

Output:
<pre style="height:30ex;overflow:scroll">
1 1 1
2 2 2
3 3 3
4 4 4
5 5 5
6 6 6
7 7 7
8 8 10
9 9 11
10 a 12
11 b 13
12 c 14
13 d 15
14 e 16
15 f 17
16 10 20
17 11 21
18 12 22
19 13 23
20 14 24
21 15 25
22 16 26
23 17 27
24 18 30
25 19 31
26 1a 32
27 1b 33
28 1c 34
29 1d 35
30 1e 36
31 1f 37
32 20 40
33 21 41

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.Formatter

loop i_ = 1 to 3
  loop n_ = 20 to 20000 by 2131
    select case i_
      when 1 then say useBif(n_)
      when 2 then say useJavaFormat(n_)
      when 3 then say useJavaNumber(n_)
      otherwise nop
      end
    end n_
  say
end i_

return

-- NetRexx doesn't have a decimal to octal conversion
method useBif(n_) public static
  d_ = '_'
  return '[Base 16='n_.d2x().right(8)',Base 10='n_.right(8)',Base 8='d_.right(8)',Base 2='n_.d2x().x2b().right(20)']'

-- Some of Java's java.lang.Number classes have conversion methods
method useJavaNumber(n_) public static
  nx = Long.toHexString(n_)
  nd = Long.toString(n_)
  no = Long.toOctalString(n_)
  nb = Long.toBinaryString(n_)
  return '[Base 16='Rexx(nx).right(8)',Base 10='Rexx(nd).right(8)',Base 8='Rexx(no).right(8)',Base 2='Rexx(nb).right(20)']'

-- Java Formatter doesn't have a decimal to binary conversion
method useJavaFormat(n_) public static
  fb = StringBuilder()
  fm = Formatter(fb)
  fm.format("[Base 16=%1$8x,Base 10=%1$8d,Base 8=%1$8o,Base 2=%2$20s]", [Object Long(n_), String('_')])
  return fb.toString()

```

'''Output:'''
<pre style="height:30ex; overflow:scroll;">
[Base 16=      14,Base 10=      20,Base 8=       _,Base 2=            00010100]
[Base 16=     867,Base 10=    2151,Base 8=       _,Base 2=        100001100111]
[Base 16=    10BA,Base 10=    4282,Base 8=       _,Base 2=    0001000010111010]
[Base 16=    190D,Base 10=    6413,Base 8=       _,Base 2=    0001100100001101]
[Base 16=    2160,Base 10=    8544,Base 8=       _,Base 2=    0010000101100000]
[Base 16=    29B3,Base 10=   10675,Base 8=       _,Base 2=    0010100110110011]
[Base 16=    3206,Base 10=   12806,Base 8=       _,Base 2=    0011001000000110]
[Base 16=    3A59,Base 10=   14937,Base 8=       _,Base 2=    0011101001011001]
[Base 16=    42AC,Base 10=   17068,Base 8=       _,Base 2=    0100001010101100]
[Base 16=    4AFF,Base 10=   19199,Base 8=       _,Base 2=    0100101011111111]

[Base 16=      14,Base 10=      20,Base 8=      24,Base 2=                   _]
[Base 16=     867,Base 10=    2151,Base 8=    4147,Base 2=                   _]
[Base 16=    10ba,Base 10=    4282,Base 8=   10272,Base 2=                   _]
[Base 16=    190d,Base 10=    6413,Base 8=   14415,Base 2=                   _]
[Base 16=    2160,Base 10=    8544,Base 8=   20540,Base 2=                   _]
[Base 16=    29b3,Base 10=   10675,Base 8=   24663,Base 2=                   _]
[Base 16=    3206,Base 10=   12806,Base 8=   31006,Base 2=                   _]
[Base 16=    3a59,Base 10=   14937,Base 8=   35131,Base 2=                   _]
[Base 16=    42ac,Base 10=   17068,Base 8=   41254,Base 2=                   _]
[Base 16=    4aff,Base 10=   19199,Base 8=   45377,Base 2=                   _]

[Base 16=      14,Base 10=      20,Base 8=      24,Base 2=               10100]
[Base 16=     867,Base 10=    2151,Base 8=    4147,Base 2=        100001100111]
[Base 16=    10ba,Base 10=    4282,Base 8=   10272,Base 2=       1000010111010]
[Base 16=    190d,Base 10=    6413,Base 8=   14415,Base 2=       1100100001101]
[Base 16=    2160,Base 10=    8544,Base 8=   20540,Base 2=      10000101100000]
[Base 16=    29b3,Base 10=   10675,Base 8=   24663,Base 2=      10100110110011]
[Base 16=    3206,Base 10=   12806,Base 8=   31006,Base 2=      11001000000110]
[Base 16=    3a59,Base 10=   14937,Base 8=   35131,Base 2=      11101001011001]
[Base 16=    42ac,Base 10=   17068,Base 8=   41254,Base 2=     100001010101100]
[Base 16=    4aff,Base 10=   19199,Base 8=   45377,Base 2=     100101011111111]

```



## Nim


```nim
import strutils

for i in 0..33:
  echo toBin(i, 6)," ",toOct(i, 3)," ",align($i,2)," ",toHex(i,2)
```

Output:

```txt
000000 000  0 00
000001 001  1 01
000010 002  2 02
000011 003  3 03
000100 004  4 04
000101 005  5 05
000110 006  6 06
000111 007  7 07
001000 010  8 08
001001 011  9 09
001010 012 10 0A
001011 013 11 0B
001100 014 12 0C
001101 015 13 0D
001110 016 14 0E
001111 017 15 0F
010000 020 16 10
010001 021 17 11
010010 022 18 12
010011 023 19 13
010100 024 20 14
010101 025 21 15
010110 026 22 16
010111 027 23 17
011000 030 24 18
011001 031 25 19
011010 032 26 1A
011011 033 27 1B
011100 034 28 1C
011101 035 29 1D
011110 036 30 1E
011111 037 31 1F
100000 040 32 20
100001 041 33 21
```



## OCaml


```ocaml
for n = 0 to 33 do
  Printf.printf " %3o %2d %2X\n" n n n (* binary not supported *)
done
```



## PARI/GP

The only bases supported by the language itself (as opposed to custom functions) are binary and decimal.

```parigp
printbinary(n)={
  n=binary(n);
  for(i=1,#n,print1(n[i]))
};
printdecimal(n)={
  print1(n)
};
```



## Perl


```perl
foreach my $n (0..33) {
  printf " %6b %3o %2d %2X\n", $n, $n, $n, $n;
}
```



## Perl 6


Calling the <code>.base</code> method on a number returns a string. It can handle all bases between 2 and 36:


```perl6
say 30.base(2);   # "11110"
say 30.base(8);   # "36"
say 30.base(10);  # "30"
say 30.base(16);  # "1E"
say 30.base(30);  # "10"
```


Alternatively, <code>printf</code> can be used for some common number bases:

```perl6
for 0..33 -> $n {
  printf " %6b %3o %2d %2X\n", $n xx 4;
}
```



## Phix


```phix
for i=1 to 33 do
    printf(1,"decimal:%6d hex:%6x HEX:%6X octal:%6o binary:%6b\n",{i,i,i})
end for
```



## PHP


```php
<?php
foreach (range(0, 33) as $n) {
  echo decbin($n), "\t", decoct($n), "\t", $n, "\t", dechex($n), "\n";
}
?>
```



```php
<?php
foreach (range(0, 33) as $n) {
  printf(" %6b %3o %2d %2X\n", $n, $n, $n, $n);
}
?>
```



## PicoLisp


```PicoLisp
(de printNumber (N Base)
   (when (>= N Base)
      (printNumber (/ N Base) Base) )
   (let C (% N Base)
      (and (> C 9) (inc 'C 39))
      (prin (char (+ C `(char "0")))) ) )

(printNumber 26 16))
(prinl)
(printNumber 123456789012345678901234567890 36))
(prinl)
```

Output:

```txt
1a
byw97um9s91dlz68tsi
```



## PL/I


```PL/I

get list (n);
put skip list (n);      /* Prints N in decimal */
put skip edit (n) (B);  /* prints N as a bit string, N > 0 */

```



## PowerShell

The .NET class <code>Convert</code> handles conversions in binary, octal, decimal and hexadecimal. Furthermore, format strings may be used for hexadecimal conversion.

```powershell
foreach ($n in 0..33) {
    "Base 2:  " + [Convert]::ToString($n, 2)
    "Base 8:  " + [Convert]::ToString($n, 8)
    "Base 10: " + $n
    "Base 10: " + [Convert]::ToString($n, 10)
    "Base 10: " + ("{0:D}" -f $n)
    "Base 16: " + [Convert]::ToString($n, 16)
    "Base 16: " + ("{0:X}" -f $n)
}
```



## PureBasic


```PureBasic
For i=105 To 115
  Bin$=RSet(Bin(i),8,"0") ;- Convert to wanted type & pad with '0'
  Hex$=RSet(Hex(i),4,"0")
  Dec$=RSet(Str(i),3)
  PrintN(Dec$+" decimal = %"+Bin$+" = $"+Hex$+".")
Next
```


 105 decimal = %01101001 = $0069.
 106 decimal = %01101010 = $006A.
 107 decimal = %01101011 = $006B.
 108 decimal = %01101100 = $006C.
 109 decimal = %01101101 = $006D.
 110 decimal = %01101110 = $006E.
 111 decimal = %01101111 = $006F.
 112 decimal = %01110000 = $0070.
 113 decimal = %01110001 = $0071.
 114 decimal = %01110010 = $0072.
 115 decimal = %01110011 = $0073.


## Python

Binary (b), Octal (o), Decimal (d), and Hexadecimal (X and x) are supported by the [http://www.python.org/dev/peps/pep-3101/ format]method of a string
<div style="height:30ex;overflow:scroll">
```python
>>>
 for n in range(34):
	print " {0:6b} {1:3o} {2:2d} {3:2X}".format(n, n, n, n)
	#The following would give the same output, and,
	#due to the outer brackets, works with Python 3.0 too
	#print ( " {n:6b} {n:3o} {n:2d} {n:2X}".format(n=n) )


      0   0  0  0
      1   1  1  1
     10   2  2  2
     11   3  3  3
    100   4  4  4
    101   5  5  5
    110   6  6  6
    111   7  7  7
   1000  10  8  8
   1001  11  9  9
   1010  12 10  A
   1011  13 11  B
   1100  14 12  C
   1101  15 13  D
   1110  16 14  E
   1111  17 15  F
  10000  20 16 10
  10001  21 17 11
  10010  22 18 12
  10011  23 19 13
  10100  24 20 14
  10101  25 21 15
  10110  26 22 16
  10111  27 23 17
  11000  30 24 18
  11001  31 25 19
  11010  32 26 1A
  11011  33 27 1B
  11100  34 28 1C
  11101  35 29 1D
  11110  36 30 1E
  11111  37 31 1F
 100000  40 32 20
 100001  41 33 21
>>>
```
</div>

Octal (o), Decimal (d), and Hexadecimal (X and x), but not binary are supported by the string modulo operator, %:

```python
for n in range(34):
	print " %3o %2d %2X" % (n, n, n)
```


----
For each of these bases there is also a built-in function that will convert it to a string with the proper prefix appended, so that it is a valid Python expression:

```python
n = 33
#Python 3.x:
print(bin(n), oct(n), n, hex(n)) # bin() only available in Python 3.x and 2.6
# output: 0b100001 0o41 33 0x21

#Python 2.x:
#print oct(n), n, hex(n)
# output: 041 33 0x21
```



## R

Conversion to and from binary does not have built-in support.

```R
# dec to oct
as.octmode(x)
# dec to hex
as.hexmode(x)
# oct or hex to dec
as.integer(x)
# or
as.numeric(x)
```



## Racket



```racket

#lang racket

;; Explicit conversion of numbers can use the standard radices
(map (λ(r) (number->string 123 r)) '(2 8 10 16))
;; -> '("1111011" "173" "123" "7b")

;; There is also the `~r' formatting function that works with any radix
;; up to 36
(for/list ([r (in-range 2 37)]) (~r 123 #:base r))
;; -> '("1111011" "02111" "3231" "344" "323" "432" "173" "641" "123" "201"
;;      "3a" "69" "b8" "38" "7b" "47" "f6" "96" "36" "i5" "d5" "85" "35"
;;      "n4" "j4" "f4" "b4" "74" "34" "u3" "r3" "o3" "l3" "i3" "f3")

```



## REXX

===dec ◄──► bin, hex===
Note that some REXX interpreters have the '''D2B''' (decimal-->binary) built-in function.

So, the '''D2B''' function was coded here for those REXX interpreters that don't have that function.


The reason for the apparent complexity of the '''D2B''' function is to handle the special case of

zero   (with regards to striping leading zeroes from the converted number)..

```rexx
/*REXX pgm shows REXX's ability to show decimal numbers in binary & hex.*/

      do j=0  to 50                /*show some low-value num conversions*/
      say right(j,3)         ' in decimal is',
          right(d2b(j),12)   " in binary",
          right(d2x(j),12)   ' in hexadecimal.'
      end   /*j*/
exit                                   /*stick a fork in it, we're done.*/
/*────────────────────────────D2B subroutine────────────────────────────*/
d2b: return word(strip(x2b(d2x(arg(1))),'L',0) 0,1)  /*convert dec──►bin*/
```

'''output'''
<pre style="height:20ex">
  0  in decimal is            0  in binary            0  in hexadecimal.
  1  in decimal is            1  in binary            1  in hexadecimal.
  2  in decimal is           10  in binary            2  in hexadecimal.
  3  in decimal is           11  in binary            3  in hexadecimal.
  4  in decimal is          100  in binary            4  in hexadecimal.
  5  in decimal is          101  in binary            5  in hexadecimal.
  6  in decimal is          110  in binary            6  in hexadecimal.
  7  in decimal is          111  in binary            7  in hexadecimal.
  8  in decimal is         1000  in binary            8  in hexadecimal.
  9  in decimal is         1001  in binary            9  in hexadecimal.
 10  in decimal is         1010  in binary            A  in hexadecimal.
 11  in decimal is         1011  in binary            B  in hexadecimal.
 12  in decimal is         1100  in binary            C  in hexadecimal.
 13  in decimal is         1101  in binary            D  in hexadecimal.
 14  in decimal is         1110  in binary            E  in hexadecimal.
 15  in decimal is         1111  in binary            F  in hexadecimal.
 16  in decimal is        10000  in binary           10  in hexadecimal.
 17  in decimal is        10001  in binary           11  in hexadecimal.
 18  in decimal is        10010  in binary           12  in hexadecimal.
 19  in decimal is        10011  in binary           13  in hexadecimal.
 20  in decimal is        10100  in binary           14  in hexadecimal.
 21  in decimal is        10101  in binary           15  in hexadecimal.
 22  in decimal is        10110  in binary           16  in hexadecimal.
 23  in decimal is        10111  in binary           17  in hexadecimal.
 24  in decimal is        11000  in binary           18  in hexadecimal.
 25  in decimal is        11001  in binary           19  in hexadecimal.
 26  in decimal is        11010  in binary           1A  in hexadecimal.
 27  in decimal is        11011  in binary           1B  in hexadecimal.
 28  in decimal is        11100  in binary           1C  in hexadecimal.
 29  in decimal is        11101  in binary           1D  in hexadecimal.
 30  in decimal is        11110  in binary           1E  in hexadecimal.
 31  in decimal is        11111  in binary           1F  in hexadecimal.
 32  in decimal is       100000  in binary           20  in hexadecimal.
 33  in decimal is       100001  in binary           21  in hexadecimal.
 34  in decimal is       100010  in binary           22  in hexadecimal.
 35  in decimal is       100011  in binary           23  in hexadecimal.
 36  in decimal is       100100  in binary           24  in hexadecimal.
 37  in decimal is       100101  in binary           25  in hexadecimal.
 38  in decimal is       100110  in binary           26  in hexadecimal.
 39  in decimal is       100111  in binary           27  in hexadecimal.
 40  in decimal is       101000  in binary           28  in hexadecimal.
 41  in decimal is       101001  in binary           29  in hexadecimal.
 42  in decimal is       101010  in binary           2A  in hexadecimal.
 43  in decimal is       101011  in binary           2B  in hexadecimal.
 44  in decimal is       101100  in binary           2C  in hexadecimal.
 45  in decimal is       101101  in binary           2D  in hexadecimal.
 46  in decimal is       101110  in binary           2E  in hexadecimal.
 47  in decimal is       101111  in binary           2F  in hexadecimal.
 48  in decimal is       110000  in binary           30  in hexadecimal.
 49  in decimal is       110001  in binary           31  in hexadecimal.
 50  in decimal is       110010  in binary           32  in hexadecimal.

```


===dec ◄──► bin, hex, char===
Rexx also has the ability to use base 256 and uses the D2C and C2D function for this purpose.


Of course, using base 256 is hampered in ASCII machines in that some lower values are

interpreted by the operating system as control characters and therefore aren't displayed as their (true) glyph.

```rexx
/*REXX program shows REXX's ability to show dec nums in bin/hex/base256.*/

      do j=14  to 67               /*display some lower-value numbers.  */
      say right(j,3)        ' in decimal is',
          right(d2b(j),12)  " in binary",
          right(d2x(j),12)  ' in hexadecimal',
          right(d2c(j),12)  ' in base256.'
      end
exit                                   /*stick a fork in it, we're done.*/
/*────────────────────────────D2B subroutine────────────────────────────*/
d2b: return word(strip(x2b(d2x(arg(1))),'L',0) 0,1)  /*convert dec──►bin*/
```

'''output'''
<pre style="height:20ex">
 14  in decimal is         1110  in binary            E  in hexadecimal            ♫  in base256.
 15  in decimal is         1111  in binary            F  in hexadecimal            ☼  in base256.
 16  in decimal is        10000  in binary           10  in hexadecimal            ►  in base256.
 17  in decimal is        10001  in binary           11  in hexadecimal            ◄  in base256.
 18  in decimal is        10010  in binary           12  in hexadecimal            ↕  in base256.
 19  in decimal is        10011  in binary           13  in hexadecimal            ‼  in base256.
 20  in decimal is        10100  in binary           14  in hexadecimal            ¶  in base256.
 21  in decimal is        10101  in binary           15  in hexadecimal            §  in base256.
 22  in decimal is        10110  in binary           16  in hexadecimal            ▬  in base256.
 23  in decimal is        10111  in binary           17  in hexadecimal            ↨  in base256.
 24  in decimal is        11000  in binary           18  in hexadecimal            ↑  in base256.
 25  in decimal is        11001  in binary           19  in hexadecimal            ↓  in base256.
 26  in decimal is        11010  in binary           1A  in hexadecimal            →  in base256.
 27  in decimal is        11011  in binary           1B  in hexadecimal            ←  in base256.
 28  in decimal is        11100  in binary           1C  in hexadecimal            ∟  in base256.
 29  in decimal is        11101  in binary           1D  in hexadecimal            ↔  in base256.
 30  in decimal is        11110  in binary           1E  in hexadecimal            ▲  in base256.
 31  in decimal is        11111  in binary           1F  in hexadecimal            ▼  in base256.
 32  in decimal is       100000  in binary           20  in hexadecimal               in base256.
 33  in decimal is       100001  in binary           21  in hexadecimal            !  in base256.
 34  in decimal is       100010  in binary           22  in hexadecimal            "  in base256.
 35  in decimal is       100011  in binary           23  in hexadecimal            #  in base256.
 36  in decimal is       100100  in binary           24  in hexadecimal            $  in base256.
 37  in decimal is       100101  in binary           25  in hexadecimal            %  in base256.
 38  in decimal is       100110  in binary           26  in hexadecimal            &  in base256.
 39  in decimal is       100111  in binary           27  in hexadecimal            '  in base256.
 40  in decimal is       101000  in binary           28  in hexadecimal            (  in base256.
 41  in decimal is       101001  in binary           29  in hexadecimal            )  in base256.
 42  in decimal is       101010  in binary           2A  in hexadecimal            *  in base256.
 43  in decimal is       101011  in binary           2B  in hexadecimal            +  in base256.
 44  in decimal is       101100  in binary           2C  in hexadecimal            ,  in base256.
 45  in decimal is       101101  in binary           2D  in hexadecimal            -  in base256.
 46  in decimal is       101110  in binary           2E  in hexadecimal            .  in base256.
 47  in decimal is       101111  in binary           2F  in hexadecimal            /  in base256.
 48  in decimal is       110000  in binary           30  in hexadecimal            0  in base256.
 49  in decimal is       110001  in binary           31  in hexadecimal            1  in base256.
 50  in decimal is       110010  in binary           32  in hexadecimal            2  in base256.
 51  in decimal is       110011  in binary           33  in hexadecimal            3  in base256.
 52  in decimal is       110100  in binary           34  in hexadecimal            4  in base256.
 53  in decimal is       110101  in binary           35  in hexadecimal            5  in base256.
 54  in decimal is       110110  in binary           36  in hexadecimal            6  in base256.
 55  in decimal is       110111  in binary           37  in hexadecimal            7  in base256.
 56  in decimal is       111000  in binary           38  in hexadecimal            8  in base256.
 57  in decimal is       111001  in binary           39  in hexadecimal            9  in base256.
 58  in decimal is       111010  in binary           3A  in hexadecimal            :  in base256.
 59  in decimal is       111011  in binary           3B  in hexadecimal            ;  in base256.
 60  in decimal is       111100  in binary           3C  in hexadecimal            <  in base256.
 61  in decimal is       111101  in binary           3D  in hexadecimal            =  in base256.
 62  in decimal is       111110  in binary           3E  in hexadecimal            >  in base256.
 63  in decimal is       111111  in binary           3F  in hexadecimal            ?  in base256.
 64  in decimal is      1000000  in binary           40  in hexadecimal            @  in base256.
 65  in decimal is      1000001  in binary           41  in hexadecimal            A  in base256.
 66  in decimal is      1000010  in binary           42  in hexadecimal            B  in base256.
 67  in decimal is      1000011  in binary           43  in hexadecimal            C  in base256.

```



## Ring


```ring

# Project : Non Decimal radices/Output

see string(0) + nl
see string(123456789) + nl
see string(-987654321) + nl

see upper(hex(43981)) + nl
see upper(hex(-1)) + nl

```

Output:

```txt

0
123456789
-987654321
ABCD
FFFFFFFF

```



## Ruby


```ruby
for n in 0..33
  puts " %6b %3o %2d %2X" % [n, n, n, n]
end
puts
[2,8,10,16,36].each {|i| puts " 100.to_s(#{i}) => #{100.to_s(i)}"}
```

<div style="height:30ex;overflow:scroll">
      0   0  0  0
      1   1  1  1
     10   2  2  2
     11   3  3  3
    100   4  4  4
    101   5  5  5
    110   6  6  6
    111   7  7  7
   1000  10  8  8
   1001  11  9  9
   1010  12 10  A
   1011  13 11  B
   1100  14 12  C
   1101  15 13  D
   1110  16 14  E
   1111  17 15  F
  10000  20 16 10
  10001  21 17 11
  10010  22 18 12
  10011  23 19 13
  10100  24 20 14
  10101  25 21 15
  10110  26 22 16
  10111  27 23 17
  11000  30 24 18
  11001  31 25 19
  11010  32 26 1A
  11011  33 27 1B
  11100  34 28 1C
  11101  35 29 1D
  11110  36 30 1E
  11111  37 31 1F
 100000  40 32 20
 100001  41 33 21

 100.to_s(2) => 1100100
 100.to_s(8) => 144
 100.to_s(10) => 100
 100.to_s(16) => 64
 100.to_s(36) => 2s
</div>


## Run BASIC


```runbasic

print asc("X")			' convert to ascii
print chr$(169)			' ascii to character
print dechex$(255)		' decimal to hex
print hexdec("FF")		' hex to decimal
print str$(467)			' decimal to string
print val("27")			' string to decimal

```



## Scala


```Scala
object Main extends App {
  val radices = List(2, 8, 10, 16, 19, 36)
  for (base <- radices) print(f"$base%6d")
  println(s"""\n${"-" * (6 * radices.length)}""")
  for (i <- BigInt(0) to 35; // BigInt has a toString(radix) method
       radix <- radices;
       eol = if (radix == radices.last) '\n' else '\0'
  ) print(f"${i.toString(radix)}%6s$eol")
}
```


## Scheme


```scheme
(do ((i 0 (+ i 1)))
    ((>= i 33))
    (display (number->string i 2)) ; binary
    (display "  ")
    (display (number->string i 8)) ; octal
    (display "  ")
    (display (number->string i 10)) ; decimal, the "10" is optional
    (display "  ")
    (display (number->string i 16)) ; hex
    (newline))
```



## Seed7

The [http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29radix%28in_integer%29 radix]
operator converts an integer number to a string. The conversion uses the numeral system with
the given base. The base can be any integer value between 2 and 36. Digits greater than 9
are represented with lower case characers (10 is represented with a, etc.). The operator
[http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29RADIX%28in_integer%29 RADIX] works
just like ''radix'', but uses upper case characters for digits greater than 9 (10 is represented with A, etc.).
The [http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29lpad%28in_integer%29 lpad] operator
is used to pad the result of the ''radix'' operator at the left side. The padding is done with spaces.

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 0;
  begin
    for i range 1 to 33 do
      writeln(i lpad 6 <&
              i radix 8 lpad 6 <&
              i radix 16 lpad 6);
    end for;
  end func;
```



## Sidef


```ruby
range(0, 33).each { |n|
    printf(" %6b %3o %2d %2X\n", ([n]*4)...);
}
```



## Smalltalk

The radix can be from 2 to 49 and its value is prepended to the string followed by "r".

```smalltalk
1 to: 33 do: [ :i |
  ('%1 %2 %3' % { i printStringRadix: 8. i printStringRadix: 16. i printStringRadix: 2 })
  printNl.
].
```



## Standard ML


```sml
let
  fun loop i =
    if i < 34 then (
      print (Int.fmt StringCvt.BIN i ^ "\t"
           ^ Int.fmt StringCvt.OCT i ^ "\t"
           ^ Int.fmt StringCvt.DEC i ^ "\t"
           ^ Int.fmt StringCvt.HEX i ^ "\n");
      loop (i+1)
    ) else ()
in
  loop 0
end
```



## Tcl


The <code>format</code> command supports conversions to octal, decimal, and hex:

```tcl
for {set n 0} {$n <= 33} {incr n} {
    puts [format " %3o %2d %2X" $n $n $n]
}
```

<!--The following should be moved to: [[Number base conversion]]

Conversion to binary requires a procedure.  Here's two ways to do it:


```tcl
# process the value as if it's a string
proc int2bits {i} {
    string map {0 000 1 001 2 010 3 011 4 100 5 101 6 110 7 111} [format %o $i]
}

# format the number string as an integer, then scan into a binary string
proc int2bits {i} {
    binary scan [binary format I1 $i] B* x
    return $x
}
```


-->

=={{header|TI-89 BASIC}}==

Bases 2, 10, and 16 are supported. The base is controlled by a global ''mode''.


```ti89b
Local old
getMode("Base")→old
setMode("Base", "BIN")
Disp string(16)
setMode("Base", "HEX")
Disp string(16)
setMode("Base", "DEC")
Disp string(16)
setMode("Base", old)
```


Output:


```ti89b
0b10000
0h10
16
```



## XPL0


```XPL0
include c:\cxpl\codes;
int N;
[N:= 2;
repeat  HexOut(0, N);  Text(0, "  ");
        IntOut(0, N);  CrLf(0);
        N:= N*N;
until   N=0;
]
```


Output:

```txt

00000002  2
00000004  4
00000010  16
00000100  256
00010000  65536

```



## Yabasic


```Yabasic
for i = 1 to 33
	print "decimal: ", i, " hex: ", hex$(i), " bin: ", bin$(i)
next

```



## zkl

"%.nB" formats a [unsigned] number in base n (2-36). So

```zkl
const N=16;
var fmt=[2..N].pump(String,"%%5.%dB".fmt);  // %5.2B%5.3B%5.4B%5.5B ...
foreach n in (17){fmt.fmt(n.pump(N,List,n.fp(n)).xplode()).println()}
```

```txt

    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
   10    2    2    2    2    2    2    2    2    2    2    2    2    2    2
   11   10    3    3    3    3    3    3    3    3    3    3    3    3    3
  100   11   10    4    4    4    4    4    4    4    4    4    4    4    4
  101   12   11   10    5    5    5    5    5    5    5    5    5    5    5
  110   20   12   11   10    6    6    6    6    6    6    6    6    6    6
  111   21   13   12   11   10    7    7    7    7    7    7    7    7    7
 1000   22   20   13   12   11   10    8    8    8    8    8    8    8    8
 1001  100   21   14   13   12   11   10    9    9    9    9    9    9    9
 1010  101   22   20   14   13   12   11   10    a    a    a    a    a    a
 1011  102   23   21   15   14   13   12   11   10    b    b    b    b    b
 1100  110   30   22   20   15   14   13   12   11   10    c    c    c    c
 1101  111   31   23   21   16   15   14   13   12   11   10    d    d    d
 1110  112   32   24   22   20   16   15   14   13   12   11   10    e    e
 1111  120   33   30   23   21   17   16   15   14   13   12   11   10    f
10000  121  100   31   24   22   20   17   16   15   14   13   12   11   10

```


```zkl
(100).toString(36) //-->"2s"
```

For binary, decimal and hex, you can also have [fixed, sorry Europe] separators:

```zkl
"%,.2B".fmt(1234567) //-->"1|0010|1101|0110|1000|0111"
"%,d".fmt(1234567)   //-->"1,234,567"
"%,x".fmt(1234567)   //-->"12|d6|87"
```


