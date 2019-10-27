+++
title = "Decimal floating point number to binary"
description = ""
date = 2019-10-04T23:06:44Z
aliases = []
[extra]
id = 17162
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Create a program that takes a decimal floating point number and displays its binary representation and vice versa: takes a floating point binary number and outputs its decimal representation.


The output might be something like this:
        23.34375  => 10111.01011
      1011.11101  =>    11.90625





## bc



```bc

obase = 2
scale=100
3.14159265358979323846264338327950288419716939937510
11.00100100001111110110101010001000100001011010001100001000110100110001001100011001100010100010111000000011011100000111001101000100101001000000100100111000001000100010011



```



## dc

{{works with|dc|1.3.95 (GNU bc 1.06.95)}}
Interactively:

```bash
$ dc
2o
23.34375 p
10111.01011000000000000
q
$ dc
2i
1011.11101
p
11.90625
q
$
```


Directly on the command line:

```bash
$ dc -e '2o 23.34375 p'
10111.01011000000000000
$ dc -e '2i 1011.11101 p'
11.90625
$ echo '2o 23.34375 p' | dc
10111.01011000000000000
$ echo '2i 1011.11101 p' | dc
11.90625
$
```


From the manpage: "To enter a negative number, begin the number with '_'.  '-' cannot be used for this, as it is a binary operator for subtraction instead."

```bash
$ dc -e '2o _23.34375 p'
-10111.01011000000000000
$ dc -e '2i _1011.11101 p'
-11.90625
$
```



## D

{{trans|Python}}

```d
import std.stdio, std.conv, std.array, std.string, std.range, std.algorithm, std.typecons;

immutable string[string] hex2bin, bin2hex;

static this() pure @safe {
    hex2bin = 16.iota.map!(x => tuple("%x".format(x), "%04b".format(x))).assocArray;
    bin2hex = 16.iota.map!(x => tuple("%b".format(x), "%x".format(x))).assocArray;
}

string dec2bin(real d) pure @safe {
    immutable neg = d < 0;
    if (neg)
        d = -d;
    immutable hx = "%a".format(d);
    immutable p = hx.countUntil('p');
    immutable bn = hx[2 .. p].split("").map!(ch => hex2bin.get(ch, ch)).join;
    // Currently Phobos lacks a strip(string, string) function.
    // return (neg ? "-" : "") ~ bn.strip("0")
    return (neg ? "-" : "") ~ bn.tr("0", " ").strip.tr(" ", "0")
           ~ hx[p .. p + 2] ~ "%b".format(hx[p + 2 .. $].to!int);
}

real bin2dec(string bn) pure /*@safe*/
in {
    assert(!bn.empty);
} body {
    immutable neg = bn[0] == '-';
    if (neg)
        bn = bn[1 .. $];
    immutable dp1 = bn.countUntil('.');
    immutable extra0a = "0".replicate(4 - dp1 % 4);
    immutable bn2 = extra0a ~ bn;
    immutable dp2 = bn2.countUntil('.');
    immutable p = bn2.countUntil('p');
    auto hx = iota(0, dp2 + 1, 4)
              .map!(i => bin2hex.get(bn2[i..min(i + 4, p)]
                                     .tr("0", " ")
                                     .stripLeft
                                     .tr(" ", "0"),
                                     bn2[i .. i + 1]))
              .join;
    immutable bn3 = bn2[dp2 + 1 .. p];
    immutable extra0b = "0".replicate(4 - bn3.length % 4);
    immutable bn4 = bn3 ~ extra0b;
    hx ~= iota(0, bn4.length, 4)
          .map!(i => bin2hex[bn4[i .. i + 4].tr("0", " ").stripLeft.tr(" ", "0")])
          .join;
    hx = (neg ? "-" : "") ~ "0x" ~ hx ~ bn2[p .. p+2] ~ bn2[p + 2 .. $].to!int(2).text;
    return hx.to!real;
}

void main() /*@safe*/ {
    immutable x = 23.34375;
    immutable y1 = x.dec2bin;
    y1.writeln;
    writefln("%.6f", y1.bin2dec);
    immutable y2 = dec2bin(-x);
    y2.writeln;
    y2.bin2dec.writeln;
    writefln("%.6f", "1011.11101p+0".bin2dec);
}
```

{{out}}

```txt
1.011101011p+100
23.343750
-1.011101011p+100
-23.3438
11.906250
```



## Elixir


```elixir
defmodule RC do
  def dec2bin(dec, precision\\16) do
    [int, df] = case String.trim(dec) |> String.split(".") do
      [int] -> [int, nil]
      [int, df] -> [int, df]
    end
    {sign, int} = if String.first(int)=="-", do: String.split_at(int, 1), else: {"", int}
    bin = sign <> (String.to_integer(int) |> Integer.to_string(2)) <> "."
    if df && String.to_integer(df)>0 do
      String.to_float("0."<>df) |> dec2bin(precision, bin)
    else
      bin <> "0"
    end
  end
  
  defp dec2bin(fp, digit, bin) when fp==0.0 or digit<=0, do: bin
  defp dec2bin(fp, digit, bin) do
    fp = fp * 2
    n = trunc(fp)
    dec2bin(fp-n, digit-1, bin<>Integer.to_string(n))
  end
  
  def bin2dec(bin) do
    [int, df] = case String.trim(bin) |> String.split(".") do
      [int] -> [int, nil]
      [int, df] -> [int, df]
    end
    {sign, int} = if String.first(int)=="-", do: String.split_at(int, 1), else: {"", int}
    dec = sign <> (String.to_integer(int, 2) |> Integer.to_string)
    dec <> if df && String.to_integer(df,2)>0 do
             1..String.length(df)
             |> Enum.reduce(String.to_integer(df, 2), fn _,acc -> acc / 2 end)
             |> to_string
             |> String.slice(1..-1)
           else
             ".0"
           end
  end
end

data = ~w[23.34375 11.90625 -23.34375 -11.90625]
Enum.each(data, fn dec ->
  bin  = RC.dec2bin(dec)
  dec2 = RC.bin2dec(bin)
  :io.format "~10s => ~12s =>~10s~n", [dec, bin, dec2]
end)

data = ~w[13 0.1 -5 -0.25]
Enum.each(data, fn dec ->
  bin  = RC.dec2bin(dec)
  dec2 = RC.bin2dec(bin)
  :io.format "~10s => ~18s =>~12s~n", [dec, bin, dec2]
end)
```


{{out}}

```txt

  23.34375 =>  10111.01011 =>  23.34375
  11.90625 =>   1011.11101 =>  11.90625
 -23.34375 => -10111.01011 => -23.34375
 -11.90625 =>  -1011.11101 => -11.90625
        13 =>             1101.0 =>        13.0
       0.1 => 0.0001100110011001 =>0.0999908447
        -5 =>             -101.0 =>        -5.0
     -0.25 =>              -0.01 =>       -0.25

```



## Factor


```factor
USING: interpolate io kernel math.parser sequences ;

: bin>dec ( x -- y )
    number>string "0b${}p0" interpolate>string string>number ;

23.34375 dup >bin
1011.11101 dup bin>dec [ [I ${} => ${}I] nl ] 2bi@
```

{{out}}

```txt

23.34375 => 1.011101011p4
1011.11101 => 11.90625

```



## Fortran

This is a cut-back version of a free-format routine EATREAL that worked in a more general context. The text was to be found in an external variable ACARD(1:LC) with external fingers L1 and L2; L1 marked the start point and L2 advanced through the number. If a problem arose then an error message could denounce the offending text ACARD(L1:L2) as well as just say "Invalid input" or similar. The routine worked in base ten only, but it is a trivial matter to replace *10 by *BASE. Here however a possible base may extend beyond just the decimal digits, so it is no longer possible to rely on zero to nine only and their associated character codes, thus the "digit" is now identified by indexing into an array of digits, thereby enabling "A" to follow "9" without character code testing. As the INDEX function works with CHARACTER variables that are indexed from one, to get zero for the first character in DIGIT, one must be subtracted. For handling the rescaling needed for fractional digits, a table of powers of ten up to sixteen was defined, but now the base may not be ten so BASE**DD is computed on the fly. The original routine was intended for usages in the hundreds of millions of calls, so this version would be unsuitable! Further, the exponent addendum (as in 35E+16) can no longer be recognised because "E" is now a possible digit. In handling this, the exponent part was added to DD and who knows, the result may produce a zero DD as in "123.456E-3" but otherwise a MOD(DD,16) would select from the table of powers of ten, and beyond that would be handled by successive squaring. Few numbers are presented with more than sixteen fractional digits, but I have been supplied data supposedly on electric power consumption via the national grid with values such as 1.21282E-31 kilowatt-hours, and other values with twenty-eight digits of ... precision?

Because on a binary computer most decimal fractions are recurring sequences of binary digits, it is better to divide by ten than to multiply by 0·1. Thus, although the successive fractional digits could be incorporated by something like <code>P = P*BASE; X = X + D/P</code> if the computer's arithmetic was conducted in a base that is not compatible with BASE (for example, two and ten) each step would introduce another calculation error. It is better to risk one only, at the end.

An alternative method is to present the text to the I/O system as with <code>READ (ACARD,*) X</code>, except that there is no facility for specifying any base other than ten. In a more general situation the text would first have to be scanned to span the number part, thus incurring double handling. The codes for hexadecimal, octal and binary formats do ''not'' read or write numbers in those bases, they show the bit pattern of the numerical storage format instead, and for floating-point numbers this is very different. Thus, Pi comes out as 100000000001001001000011111101101010100010001000010110100011000 in B64 format, not 11·0010010000111111011010101... Note the omitted high-order bit in the normalised binary floating-point format - a further complication.

The source is F77 style, except for the MODULE usage simply for some slight convenience in sharing DIGIT and not having to re-declare the type of EATNUM. 
```Fortran
      MODULE REBASE	!Play with some conversions between bases.
       CHARACTER*36 DIGIT	!A set of acceptable digit characters.
       PARAMETER (DIGIT = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")	!Not only including hexadecimal.
       CONTAINS		!No great complication.
        LOGICAL FUNCTION EATNUM(ACARD,BASE,V)	!Reads a text number using the specified base.
Chews into the likes of 666, -666.666, .666 with their variations.
Completes with the value in V, success as the result.
Could check that no digit exceeds the specified BASE usage, but that would mean an error message...
Concocted by R.N.McLean (whom God preserve) May XXMM.
Clunky usage of ICHAR encourages the compaq compiler to employ single-character-at-a-time usage.
         CHARACTER*(*) ACARD	!The text.
         INTEGER BASE		!The base may not be ten.
         DOUBLE PRECISION V	!The object of the exercise.
         DOUBLE PRECISION X	!Maximum precision for all this fun. No sign of REAL*10.
         INTEGER D,DD		!A single digit. and a digit count.
         INTEGER L2,LC		!Finger and limit.
         INTEGER*1 C		!Since ICHAR is in use.
         LOGICAL ADIGIT,XNEG	!Things noticed along the way.
          ADIGIT = .FALSE.	!No digits seen.
          XNEG = .FALSE.	!No negative number.
          DD = 0		!No decimal digits.
          X = 0			!No value.
          L2 = 1		!The starting point.
          LC = LEN(ACARD)	!The ending point
          IF (L2.GT.LC) GO TO 20	!Urk! Off the end even before I start.
Chew into the number. Admit a possible leading sign, then digits.
          C = ICHAR(ACARD(L2:L2))	!Grab, since there are two comparisons.
          IF (C.EQ.ICHAR("+")) GO TO 1	!First consider
          IF (C.NE.ICHAR("-")) GO TO 2	!Possible signs.
          XNEG = .TRUE.		!To be acted upon later.
    1     L2 = L2 + 1		!Advance one.
          IF (L2.GT.LC) GO TO 20	!Off the end?
    2     D = INDEX(DIGIT,ACARD(L2:L2)) - 1	!No. Taste the candidate digit.
          IF (D .LT. 0) GO TO 10	!Is it to my taste?
          X = D				!Yes. Yum.
          ADIGIT = .TRUE.		!A digit has been seen. One is enough to be seen.
    3     L2 = L2 + 1			!More may follow.
          IF (L2.GT.LC) GO TO 20	!Perhaps not.
          D = ICHAR(ACARD(L2:L2)) - ICHAR("0")	!Taste the candidate digit.
          IF (D .LT. 0) GO TO 10	!Digitish?
          X = X*BASE + D		!Yes. Assimilate.
          GO TO 3			!Even more might well follow.
Consider any decimal digits, introduced by a decimal point.
   10     IF (ICHAR(ACARD(L2:L2)).EQ.ICHAR(".")) GO TO 11	!A full stop as a decimal point?
          IF (ICHAR(ACARD(L2:L2)).NE.ICHAR("·")) GO TO 20	!So, is there a decimal point?
   11     L2 = L2 + 1			!Advance one.
          IF (L2.GT.LC) GO TO 20	!Sudden end?
          D = INDEX(DIGIT,ACARD(L2:L2)) - 1	!No. Taste the digit candidate.
          IF (D .LT. 0) GO TO 20	!Suitable?
          X = X*BASE + D		!Yes. Continue augmenting the number.
          DD = 1			!This is the first decimal digit.
          ADIGIT = .TRUE.		!There may have been none before the decimal point.
   12     L2 = L2 + 1			!If once one digit is seen, is not the jungle full of digits?
          IF (L2.GT.LC) GO TO 20	!Perhaps not.
          D = ICHAR(ACARD(L2:L2)) - ICHAR("0")	!Taste the digit candidate.
          IF (D < 0 .OR. 9 < D) GO TO 20	!Suitable?
          X = X*BASE + D			!Yes. Accept as before.
          DD = DD + 1			!Now, no need to set ADIGIT to true again.
          GO TO 12			!Carry on.
Can't consider any exponent part, started by an "E" or "D", as these may be possible digit symbols.
   20     IF (DD .GT. 0) X = X/BASE**DD	!Rescale for the fractional digits.
          IF (XNEG) X = -X		!Fix the sign.
          V = X				!Place the result.
          EATNUM = ADIGIT		!Report success.
        END FUNCTION EATNUM	!And awayt.

        SUBROUTINE FP8DIGITS(X,BASE,TEXT,L)	!Full expansion of the value of X in BASE.
Converts a number X to a specified BASE. For integers, successive division by BASE, for fractions, successive multiplication.
         REAL*8 X,T		!The value, and an associate.
         INTEGER BASE		!As desired.
         CHARACTER*(*) TEXT	!Scratchpad for results.
         INTEGER L		!The length of the result.
         INTEGER N,ND		!Counters.
         INTEGER D		!The digit of the moment.
         LOGICAL NEG		!Annoyance with signs.
          IF (BASE.LE.1 .OR. BASE.GT.LEN(DIGIT)) BASE = 10	!Preclude oddities.
          WRITE (TEXT,1) BASE	!Scrub the TEXT with an announcement.
    1     FORMAT ("Base",I3)	!A limited range is expected..
          T = X			!Grab the value.
          N = T			!Its integer part, with truncation.
          T = ABS(T - N)	!Thus obtain the fractional part.
          NEG = N .LT. 0	!Negative numbers are a nuisance.
          IF (NEG) N = -N	!So simplify for what follows.
          L = LEN(TEXT)		!Limit of the scratchpad.
          ND = 0		!No digits have been rolled.
Crunch the integer part. Use the tail end of TEXT as a scratchpad, as the size of N is unassessed.
   10     D = MOD(N,BASE)		!Extract the low-order digit in BASE.
          TEXT(L:L) = DIGIT(D+1:D+1)	!Place it as text.
          ND = ND + 1			!Count another digit rolled.
          N = N/BASE			!Drop down a power.
          L = L - 1			!Move back correspondingly.
          IF (L.LE.0) THEN		!Run out of space?
            TEXT = "Overflow!"		!Then, this will have to do!
            L = MIN(9,LEN(TEXT))	!TEXT might be far too short.
           RETURN			!Give up.
          END IF			!But, space is expected.
          IF (N.GT.0) GO TO 10		!Are we there yet?
          IF (NEG) THEN			!Yes! Is a negative sign needed?
            TEXT(L:L) = "-"		!Yes. Place it.
            L = L - 1			!And retreat another place.
          END IF			!No + sign for positive numbers.
          N = LEN(TEXT) - L		!So, how much scratchpad was used?
          TEXT(9:9 + N - 1) = TEXT(L + 1:)	!Append to the initial TEXT(1:8) from the start.
          L = 9 + N			!Finger what follows the units position.
          TEXT(L:L) = "."		!Laziness leads to a full stop for a decimal point.
Crunch through the fractional part until nothing remains.
          DO WHILE(T.GT.0)	!Eventually, this will be zero.
            IF (L.GE.LEN(TEXT)) THEN	!Provided I have enough space!
              L = LEN(TEXT)		!If not, use the whole supply.
              TEXT(L:L) = "~"		!Place a marker suggesting that more should follow.
             RETURN		!And give up.
            END IF		!Otherwise, a digit is to be found.
            T = T*BASE		!Shift up a power.
            N = T		!The integer part is the digit.
            T = T - N		!Remove that integer part from T.
            L = L + 1		!Advance the finger.
            TEXT(L:L) = DIGIT(N+1:N+1)	!Place the digit.
            ND = ND + 1		!Count it also.
          END DO		!And see if anything remains.
Cast forth an addendum, to save the reader from mumbling while counting long strings of digits.
          IF (LEN(TEXT) - L .GT. 11) THEN	!Err, is there space for an addendum?
            WRITE (TEXT(L + 2:),11) ND		!Yes! Reveal the number of digits.
   11       FORMAT ("Digits:",I3)		!I expect no ore than two-digit digit counts.
            L = L + 1 + 10			!So this should do.
          END IF				!So much for the addendum.
        END SUBROUTINE FP8DIGITS	!Bases play best with related bases, such as 4 and 8. Less so with (say) 3 and 7...
      END MODULE REBASE	!Enough for inspection.

      PROGRAM TESTSOME
Check some conversions from one base to another.
      USE REBASE
      INTEGER N		!Some number of tests.
      PARAMETER (N = 5)		!This number.
      CHARACTER*12 TEXT(N)	!Sufficient size texts.
      DATA TEXT/"23.34375","10111.01011","1011.1101","11.90625","-666"/	!Also demonstrate a negative.
      DOUBLE PRECISION V	!The value in the computer's own representation.
      INTEGER I,L,BASE		!Assistants.
      CHARACTER*88 BACK		!A scratchpad.

      WRITE (6,1)	!A heading would be nice.
    1 FORMAT ("Test text in base",3X,"Value in base 10")
Chug through the tests.
      DO BASE = 10,2,-8	!Odd loop generates BASE = 10 then BASE = 2.
        DO I = 1,N		!Step through the test texts.
          WRITE (6,11) TEXT(I),BASE	!Start the line with the input.
   11     FORMAT (A,I5,$)		!The $, obviously, means no new line.
          IF (.NOT.EATNUM(TEXT(I),BASE,V)) THEN	!There shouldn't be any trouble.
            WRITE (6,*) "Not a good number!"		!But...
           ELSE				!So then,
            WRITE (BACK,*) V			!Reveal the resulting value.
            WRITE (6,12) BACK(1:20)		!Sufficient space, I hope.
   12       FORMAT (A,$)			!All to produce tabular output.
            CALL FP8DIGITS(V,2,BACK,L)		!Convert back to a text string.
            WRITE (6,13) BACK(1:L)		!And reveal.
   13       FORMAT (A)				!Thus end the line.
            CALL FP8DIGITS(V,10,BACK,L)		!And in another base,
            WRITE (6,14) BACK(1:L)		!The same value.
   14       FORMAT (37X,A)			!Nicely aligned.
          END IF			!So much for that test.
        END DO			!On to the next test.
      END DO		!And another base.
      END	!Enough of that. 
```


Rather than mess about with invocations, the test interprets the texts firstly as base ten sequences, then base two. It makes no complaint over encountering the likes of "666" when commanded to absorb according to base two. The placewise notation is straightforward: 666 = 6x2<sup>2</sup> + 6x2<sup>1</sup> + 6x2<sup>0</sup>

```txt

Test text in base   Value in base 10
23.34375       10   23.3437500000000 Base  2 10111.01011 Digits: 10
                                     Base 10 23.34375 Digits:  7
10111.01011    10   10111.0101100000 Base  2 10011101111111.00000010100101101001000110100111010111 Digits: 52
                                     Base 10 10111.01010999999925843439996242523193359375 Digits: 43
1011.1101      10   1011.11010000000 Base  2 1111110011.0001110000101111100000110111101101001010001 Digits: 53
                                     Base 10 1011.1100999999999885403667576611042022705078125 Digits: 47
11.90625       10   11.9062500000000 Base  2 1011.11101 Digits:  9
                                     Base 10 11.90625 Digits:  7
-666           10  -666.000000000000 Base  2 -1010011010. Digits: 10
                                     Base 10 -666. Digits:  3
23.34375        2   10.4687500000000 Base  2 1010.01111 Digits:  9
                                     Base 10 10.46875 Digits:  7
10111.01011     2   23.3437500000000 Base  2 10111.01011 Digits: 10
                                     Base 10 23.34375 Digits:  7
1011.1101       2   11.8125000000000 Base  2 1011.1101 Digits:  8
                                     Base 10 11.8125 Digits:  6
11.90625        2   8.53125000000000 Base  2 1000.10001 Digits:  9
                                     Base 10 8.53125 Digits:  6
-666            2  -42.0000000000000 Base  2 -101010. Digits:  6
                                     Base 10 -42. Digits:  2
```

Note again that a decimal value in binary is almost always a recurring sequence and that the ''exact'' decimal value of the actual binary sequence in the computer (of finite length) is not the same as the original decimal value. 23·34375 happens to be an exact decimal representation of a binary value whose digit count is less than that available to a double-precision floating-point variable. But although 1011·1101 has few digits, in decimal it converts to a recurring sequence in binary just as does 0·1.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Expresses (or rounds) the fractional part to the same number of places in binary as the decimal to be converted.
' This is accurate for the numbers used in this exercise but in general would not be.
Function decToBin(d As Double) As String
  Dim neg As String = ""
  If d < 0.0 Then
    d = -d
    neg = "-"
  End If
  Dim i  As Integer = Fix(d)
  Dim f  As Double  = Frac(d)
  If f = 0 Then
    Return neg + Bin(i) 
  End If
  Dim le As Integer = Len(Str(d)) - Len(Str(i)) - 1
  Return neg + Bin(i) + "." + Bin(Fix((2.0 ^ le) * f + 0.5), le)    
End Function

Function binToDec(s As String) As Double
  If s = "" Then Return 0.0
  Dim neg As Integer = 1
  If Left(s, 1) = "-" Then
     s = Mid(s, 2)
     neg = -1
  End If
  Dim index As Integer = Instr(s, ".")   
  If index = 0 Then 
    Return ValLng("&B" + s) * neg
  Else
    Dim a  As Integer  = ValLng("&B" + Left(s, index - 1))
    Dim b  As Integer  = ValLng("&B" + Mid(s, index + 1))
    Dim le As Integer  = Len(Mid(s, index + 1))
    Return (a + b / (2.0 ^ le)) * neg
  End If
End Function

Print "23.34375     =>  "; decToBin(23.34375)
Print "1011.11101   => " ; binToDec("1011.11101")
Print "-23.34375    => " ; decToBin(-23.34375)
Print "-1011.11101  => " ; binToDec("-1011.11101")
Print "64           =>  "; decToBin(64)
Print "-100001      => " ; binToDec("-100001")
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

23.34375     =>  10111.01011
1011.11101   =>  11.90625
-23.34375    => -10111.01011
-1011.11101  => -11.90625
64           =>  1000000
-100001      => -33

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

func decToBin(d float64) string {
    whole := int64(math.Floor(d))
    binary := strconv.FormatInt(whole, 2) + "."
    dd := d - float64(whole)
    for dd > 0.0 {
        r := dd * 2.0
        if r >= 1.0 {
            binary += "1"
            dd = r - 1.0
        } else {
            binary += "0"
            dd = r
        }
    }
    return binary
}

func binToDec(s string) float64 {
    ss := strings.Replace(s, ".", "", 1)
    num, _ := strconv.ParseInt(ss, 2, 64)
    ss = strings.Split(s, ".")[1]
    ss = strings.Replace(ss, "1", "0", -1)
    den, _ := strconv.ParseInt("1" + ss, 2, 64)
    return float64(num) / float64(den)
}

func main() {
    f := 23.34375
    fmt.Printf("%v\t => %s\n", f, decToBin(f))
    s := "1011.11101"
    fmt.Printf("%s\t => %v\n", s, binToDec(s))
}
```


{{out}}

```txt

23.34375	 => 10111.01011
1011.11101	 => 11.90625

```



## Haskell

float to binary part only:

```haskell
import Data.Char (intToDigit)
import Numeric (floatToDigits, showIntAtBase)

dec2bin :: RealFloat a => a -> String
dec2bin f = "0." ++ map intToDigit digits ++ "p+" ++ showIntAtBase 2 intToDigit ex ""
  where (digits, ex) = floatToDigits 2 f

main :: IO ()
main = putStrLn $ dec2bin 23.34375
```

{{out}}

```txt

0.1011101011p+101

```



## J


In this draft, the task does not give any guidelines for handling precision. So we will use 99 places after the decimal point and trim any trailing zeros (and the decimal point, for integer case).

Also, since J does not have a "Decimal floating point number" data type, we will use a list of characters to represent a decimal or binary number (this corresponds roughly with the relevant feature set of REXX which seems to have had a strong influence on this draft of this task), and use internal (mantissa,exponent) representations during the conversion.

Implementation:


```J
b2b=:2 :0
  NB. string to rational number
  exp=. (1x+y i.'.')-#y
  mant=. n#.x:0"."0 y-.'.'
  number=. mant*n^exp*'.' e. y
  NB. rational number to string
  exp=. _99
  mant=. <.1r2+number*m^x:-exp
  s=. exp&(}.,'.',{.) (":m#.inv mant)-.' '
  ((exp-1)>.-+/*/\|.s e.'.0') }. s
)
```


Example use:


```J
   2 b2b 10 '23.34375'
10111.01011
   10 b2b 2 '1011.11101'
11.90625
```




## Julia


```julia
""" Convert numeric string in base 10 to base 2 floating point string """
function dec2bin(x::String)
    bx = parse(BigFloat, x)
    xneg = bx >= 0 ? false : true
    bx = BigFloat(xneg ? -bx : bx)
    pre, post, n = "", "", div(nextpow(2, bx), 2)
    while bx > 0
        a, bx = divrem(bx, BigFloat(n))
        if n >= 1
            pre *= a > 0 ? '1' : '0'
        else
            post *= a > 0 ? '1' : '0'
        end
        n /= 2.0
    end
    (xneg ? "-" : "") * pre * "." * post
end

""" Convert binary floating point format string to Float64 numeric type """
function bin2dec(binfloat::String)
    binfloat = strip(binfloat)
    if binfloat[1] == '-'
        binfloat = binfloat[2:end]
        xneg = -1
    else
        xneg = 1
    end
    if occursin(".", binfloat)
        (pre, post) = split(binfloat, ".")
        mult = BigInt(2)^length(post)
        return xneg * (BigFloat(parse(BigInt, pre, base=2)) + 
            BigFloat(parse(BigInt, post, base=2) / mult))
    else
        return xneg * BigFloat(parse(BigInt, binfloat, base=2))
    end
end
 
function testconversions(tdata)
    println("String (base 10)    Base 2     Base 10")
    for dstring in testdata
        b2 = dec2bin(dstring)
        b10 = bin2dec(b2)
        println(lpad(dstring, 10), lpad(b2, 18), lpad(b10, 10))
    end
end

testdata = ["23.34375", "11.90625", "-23.34375", "-11.90625"]
testconversions(testdata)

```
{{out}}

```txt

String (base 10)    Base 2     Base 10
  23.34375       10111.01011  23.34375
  11.90625        1011.11101  11.90625
 -23.34375      -10111.01011 -23.34375
 -11.90625       -1011.11101 -11.90625

```



## Kotlin


```scala
// version 1.1.0

fun decToBin(d: Double): String {
    val whole  = Math.floor(d).toLong()
    var binary = whole.toString(2) + "." 
    var dd = d - whole
    while (dd > 0.0) { 
        val r = dd * 2.0
        if (r >= 1.0) {  
            binary += "1"
            dd = r - 1
        }
        else {    
            binary += "0"
            dd = r
        }
    } 
    return binary
}

fun binToDec(s: String): Double {
    val num = s.replace(".", "").toLong(2)
    val den = ("1" + s.split('.')[1].replace("1", "0")).toLong(2)
    return num.toDouble() / den
}

fun main(args: Array<String>) {
    val d = 23.34375
    println("$d\t => ${decToBin(d)}")
    val s = "1011.11101"
    println("$s\t => ${binToDec(s)}")
}
```


{{out}}

```txt

23.34375         => 10111.01011
1011.11101       => 11.90625

```



## LiveCode

LiveCode's baseConvert only works on integers.
Only the first part of this task is complete.

```LiveCode
function float2bin n
    put 15 into limit
    put 0 into i
    split n using "."
    put  baseConvert(n[1],10,2) into significand
    put "0." before n[2]
    repeat until (n[2] = 0 or i > limit)
        put n[2] * 2 into tmp
        split tmp with "."
        put tmp[1] after insignificand
        put "0." & tmp[2] into n[2]
        add 1 to i
    end repeat
    return significand & "." & insignificand
end float2bin

put float2bin(23.34375) // 10111.01011
put float2bin(11.90625) //1011.11101
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Conv2dec=lambda (n$, frombase=10, dp$=".") -> {
           neg=left$(n$,1)="-": if neg then n$=mid$(n$,2)
           if instr(n$, dp$)>0 then {
                 n2$=Piece$(n$,dp$,2)
                 n$=Piece$(n$, dp$,1)
           } else n2$=""
           n0=0
           l1=len(n$)+1
           For i=len(n$) to 1
                 dig$=Mid$(n$,l1-i,1)
                 dig=asc(dig$)-48
                 if dig$>"9" then dig-=7
                 if dig>=frombase then error "not in base:"+frombase
                 n0+=dig*frombase^(i-1)
           next i
           if n2$<>"" then {
              For i=1 to len(n2$)
                 dig$=Mid$(n2$,i,1)
                 dig=asc(dig$)-48
                 if dig$>"9" then dig-=7
                 if dig>=frombase then error "not in base:"+frombase
                 n0+=dig/frombase^i
              Next i 
           }
           if neg then n0-!
           =n0
      }
      Conv2Any$=Lambda$ (dec, tobase=10, dp$=".", prec=16) -> {
           a$=""
           neg=false
           if dec<0 then neg=true
           dec=abs(dec)
           n2=frac(dec)
           if dec=0 then {
                 a$="0" 
           } else {
                 do {
                        n=dec mod tobase
                        if n>=10 then n+=7
                        a$=chr$(n+48)+a$
                        dec=dec div tobase
                  } until dec==0
            }
            if n2<>0 then {
                 a$+=dp$
                 prec--
                 do {
                      prec--
                      dec=n2*tobase           
                      n2=frac(dec)
                      dec-=n2
                      n2=round(n2)
                      if dec>=10 then dec+=7
                      a$+=chr$(dec+48)
                 } until n2=0  or prec<0      
            }
            if neg then {="-"+a$} else =a$
      }
      Rem : Locale 1033 ' use . for all print out for decimal point
      Print Conv2dec("10111.01011",2); " => ";Conv2Any$(23.34375,2)
      Print Conv2Any$(11.90625, 2); " => "; Conv2dec("1011.11101",2)
      \\ using , for decimal point
      Print Conv2Any$(Conv2dec("1011,11101",2, ","), 10, ",") 
      Print 12312321.1212
      Print Conv2Any$(12312321.1212, 2)
      \\ using . for 1033 locale
      Print Str$(Conv2Dec(Conv2Any$(12312321.1212, 2), 2), 1033)="12312321.1211853"
      Print Str$(Conv2Dec(Conv2Any$(12312321.1212, 2,,52), 2), 1033) ="12312321.1212"
}
Checkit

```

{{out}}

```txt

23.34375 => 10111.01011
1011.11101 => 11.90625
11,90625
12312321.1212
101110111101111100000001.0001111100000110
     True
     True

```



## Maple


```Maple

convert(23.34375,binary,decimal);

convert(1011.11101,decimal,binary);

```

Output:

```txt

                          10111.01011

                          11.90625000

```



## Mathematica


```Mathematica
dec2bin[x_] := Block[{digits, pos},
  {digits,  pos} = (RealDigits[ToExpression@x,  2] /. {a_, b_} :> {ToString /@ a, b});
  StringJoin @@ 
   Join@{Take[digits, pos], {"."}, Quiet@NestWhile[Most, Drop[digits, pos], Last@# === "0" &]}]

bin2dec[x_] := FromDigits[RealDigits@ToExpression@x, 2] // N

Print[NumberForm[#, {9, 5}], " => ", dec2bin@#] &@23.34375;
Print[NumberForm[#, {9, 5}], " => ", NumberForm[bin2dec@#, {9, 5}]] &@1011.11101;
```

{{out}}

```txt

23.34375 => 10111.01011
1011.11101 => 11.90625
```




## OCaml

Well, I am no expert in OCaml, and my code may seem a bit messy, but I actually
took a rather naive aproach... Anyway, the program seems to work, but the
algorithm(s) can probably be improved.
After reading the discussion, I took into account the suggestion that the
program should perform conversions from any base to any other base.
{{works with|OCaml|4.03+}}

```ocaml

#load "str.cma"
(* Using the interpteter or the compiler:
 *
 * Interpreter:
 *     $ ocaml convert.ml
 *
 * Compiler:
 *     First of all, delete the line '#load "str.cma"'.
 *     Then, using the native compiler:
 *         $ ocmalopt -o convert str.cmxa convert.ml
 *         $ ./convert
 *     Or using ocamlbuild:
 *         $ ocamlbuild -pkg str convert.native
 *         $ ./convert.native
 *)

(* This program converts from any numerical base to any numerical base. *)
(* The numbers are wrapped within a type named 'value'.
 * Ex: Float 23.7
 *     String "1011.110101"
 *
 * Conversion is performed by the function 'convert'.
 * Ex: Convert.convert ~to_base:2 (Convert.Float 23.7)
 *     Convert.convert ~from_base:2 to_base:10 (Convert.String "1011.110101")
 *
 * (Parameter 'from_base' is optional and defaults to 10)
 *)

(* This signature should be located in a separate file 'convert.mli'... *)
module Convert : sig
  type value = | Float of float
               | String of string
  val convert : ?from_base:int -> to_base:int -> value -> value
end =
struct

type value = | Float of float
             | String of string


(* 
### =====================
 *)
(* 
###  Auxiliary functions 
 *)
(* 
### =====================
 *)
(* Digits available: 0 to 9 plus A to Z.
 * A base less than 2 does not make sense... Does it?
 * A base greater than 36 would need more letters... *)
let min_base = 2
let max_base = 10 + int_of_char 'Z' - int_of_char 'A' + 1

(* A maximum number of decimal positions just to avoid infinite loops while
 * computing them... I think 30 is enough... *)
let max_decs = 30


(* Convert an integer into the corresponding digit:
 * 0->'0'; ...; 9->'9'; 10->'A'; 11->'B'; etc. *)
let dig_of_int n =
  if n >= 0 && n <= 9
  then char_of_int (int_of_char '0' + n)
  else if n >= 10 && n <= max_base
       then char_of_int (n + int_of_char 'A' - 10)
       else failwith (Printf.sprintf "Incorrect digit: %c" (char_of_int n))


(* Convert a digit into the corresponding integer:
 * '0'->0; ...; '9'->9; 'A'->10; 'B'->11; etc. *)
let int_of_dig c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | 'A' .. 'Z' -> int_of_char c - int_of_char 'A' + 10
  | _ -> failwith (Printf.sprintf "Incorrect character: %c" c)


(* A numerical base must be within some limits. *)
let check_base b =
  if b >= min_base && b <= max_base
  then true
  else invalid_arg ("Invalid base " ^ string_of_int b)


(* A number must have its digits within the range [0..b-1] for any base b. *) 
let check_number b n =
  let max_digit = dig_of_int (b - 1)
  and str = match n with
            | Float f -> string_of_float f
            | String s -> s
         |> Str.replace_first (Str.regexp "-") ""  (* strip off one sign *)
         |> Str.replace_first (Str.regexp "\\.") "" (* strip off one dot *)
  in
  let rec scan i =
    if i >= String.length str then true
    else
      if str.[i] <= max_digit
      then scan (i + 1)
      else invalid_arg (Printf.sprintf "Invalid digit %c for base %d" str.[i] b)
  in
  scan 0
(* 
### =========================
 *)
(* 
###  End Auxiliary functions 
 *)
(* 
### =========================
 *)


(* 
### ======================
 *)
(* 
###  Conversion functions 
 *)
(* 
### ======================
 *)
(* Convert a floating point number, which is always base 10, to any base. *)
let conv_float to_base fl =
  let d = abs_float fl in
  let int_part = truncate d in
  let dec_part = d -. float int_part
  in
  let rec ft_int n ls = 
    (* Conversion of the integer part. *)
    let quot = n / to_base
    and rest = n mod to_base in
    if quot = 0 then rest :: ls
                else ft_int quot (rest :: ls)
  in
  let rec ft_dec nd x ls =
    (* Conversion of the decimal part. nd is the maximum number of decinals to
     * be computed. *)
    if x = 0. || nd = 0 then List.rev ls
    else let prod = x *. float to_base in
         let intpart = truncate prod in
         ft_dec (nd - 1) (prod -. float intpart) (intpart :: ls)
  in
  let join_digs ls =
    (* Convert integers into digits and join them into a string. *)
    List.map (fun n -> dig_of_int n |> Char.escaped) ls |> String.concat ""
  in
  let sign = if fl < 0. then "-" else "" in 
  let left = sign ^ join_digs (ft_int int_part [])
  and right = join_digs (ft_dec max_decs dec_part []) in
  (* Decimal point only if a decimal part exists. *)
  let str = left ^ (if String.length right = 0 then "" else  "." ^ right) in
  if to_base = 10 then Float (float_of_string str)
                  else String str


(* Convert a value from one base to another.
 * Using base 10 as an intermediate conversion. *)
let conv_string from_base to_base st =
  let digs = Str.replace_first (Str.regexp "-") "" st in
  let splitted = Str.split (Str.regexp "\\.") digs in
  let max_weight = String.length (List.hd splitted) - 1
  in
  let intdigs = List.map (Str.split (Str.regexp "")) splitted
             |> List.flatten
             |> List.map (fun s -> int_of_dig s.[0])
  in
  let conv10 w ns =
    List.mapi (fun i n -> float n *. float from_base ** float (w - i)) ns
    |> List.fold_left (+.) 0.
  in
  let sign = if st.[0] = '-' then -1. else 1. in
  let num_b10 = sign *. conv10 max_weight intdigs in
  if to_base = 10 then Float num_b10
                  else conv_float to_base num_b10
(* 
### ==========================
 *)
(* 
###  End Conversion functions 
 *)
(* 
### ==========================
 *)


(* 
### ========================
 *)
(* 
###  Conversion starts here 
 *)
(* 
### ========================
 *)
let convert ?(from_base = 10) ~to_base number =
  let _ = check_base from_base && check_base to_base
  and _ = check_number from_base number
  in
  match number, from_base, to_base with
  | (Float fl, fb, tb) when fb = 10 && tb = 10 ->
      number
  | (Float fl, fb, _) when fb = 10 ->
      conv_float to_base fl
  | (Float _, _, _) ->
      invalid_arg "With a float, base of origin is always 10"
  | (String st, _, _) ->
      conv_string from_base to_base st

end  (* of module Convert *)


(* 
### =================
 *)
(* 
###  Some testing... 
 *)
(* 
### =================
 *)
open Convert
let () =
  let values_both = [
    (10, 2, Float 23.34375, String "10111.01011");
    (10, 2, Float (-23.34375), String "-10111.01011");
    (10, 2, Float 11.90625, String "1011.11101");
    (10, 2, Float (-11.90625), String "-1011.11101");
    (10, 2, Float 0., String "0");
    (2, 16, String "1111", String "F");
    (2, 16, String "-1111", String "-F")
  ]
  and values = [
    (10, 10, String "23.7", Float 23.7);
    (* conversion of Float to base 10 results in the same Float, not a String *)
    (10, 10, Float 23.7, Float 23.7)
  ] in
  let get_float = function
    | Float f -> f
    | _ -> failwith "Incorrect Float..."
  in
  let get_string = function
    | String s -> s
    | _ -> failwith "Incorrect String..."
  in
  let result pred =
    if pred then "PASS" else "FAIL"
  in
  let pretty_print v1 v2 calc =
    match v1, v2 with
    | Float f, String s ->
       Printf.sprintf "%f => %s; expected %s [%s]\n"
                      (get_float v1) (get_string calc) (get_string v2)
                      (result (calc = v2))
    | String s, Float f -> 
       Printf.sprintf "%s => %f; expected %f [%s]\n"
                      (get_string v1) (get_float calc) (get_float v2)
                      (result (calc = v2))
    | String s1, String s2 -> 
       Printf.sprintf "%s => %s; expected %s [%s]\n"
                      (get_string v1) (get_string calc) (get_string v2)
                      (result (calc = v2))
    | Float f1, Float f2 -> 
       Printf.sprintf "%f => %f; expected %f [%s]\n"
                      (get_float v1) (get_float calc) (get_float v2)
                      (result (calc = v2))
  in
  let testit (base1, base2, num1, num2) =
    let calc1 = convert ~from_base:base1 ~to_base:base2 num1 in
    pretty_print num1 num2 calc1
  in
  let testit_both (base1, base2, num1, num2) =
    testit (base1, base2, num1, num2) ^ testit (base2, base1, num2, num1)
  in
  let _ = List.iter (fun tpl -> print_endline (testit_both tpl)) values_both
  and _ = List.iter (fun tpl -> print_endline (testit tpl)) values
  in ()

```

{{out}}
 23.343750 => 10111.01011; expected 10111.01011 [PASS]
 10111.01011 => 23.343750; expected 23.343750 [PASS]
 
 -23.343750 => -10111.01011; expected -10111.01011 [PASS]
 -10111.01011 => -23.343750; expected -23.343750 [PASS]
 
 11.906250 => 1011.11101; expected 1011.11101 [PASS]
 1011.11101 => 11.906250; expected 11.906250 [PASS]
 
 -11.906250 => -1011.11101; expected -1011.11101 [PASS]
 -1011.11101 => -11.906250; expected -11.906250 [PASS]
 
 0.000000 => 0; expected 0 [PASS]
 0 => 0.000000; expected 0.000000 [PASS]
 
 1111 => F; expected F [PASS]
 F => 1111; expected 1111 [PASS]
 
 -1111 => -F; expected -F [PASS]
 -F => -1111; expected -1111 [PASS]
 
 23.7 => 23.700000; expected 23.700000 [PASS]
 
 23.700000 => 23.700000; expected 23.700000 [PASS]


## Perl


```perl
use strict;
use warnings;
use feature 'say';

sub dec2bin {
    my($l,$r) = split /\./, shift;
    my $int  = unpack('B*',  pack('N',               $l ));
    my $frac = unpack('B32', pack('N',4294967296 * ".$r"));
    "$int.$frac" =~ s/^0*(.*?)0*$/$1/r;
}

sub bin2dec {
    my($l,$r) = split /\./, shift;
    my $frac = my $i = 0;
    --$i, $frac += $_ * 2**$i for split '', $r;
    eval('0b'.$l) + $frac;
}

say dec2bin(23.34375);
say bin2dec('1011.11101');
```

{{out}}

```txt
10111.01011
11.90625
```



## Perl 6


```perl6
given "23.34375"   { say "$_ => ", :10($_).base(2) }
given "1011.11101" { say "$_ => ", :2($_).base(10) }
```

{{out}}

```txt
23.34375 => 10111.01011
1011.11101 => 11.90625
```


## Phix

Handles bases 2..36. Does not handle any form of scientific notation.

```Phix
function dec_to(atom d, integer base)
-- convert d to a string in the specified base
-- eg dec_to(65535,16) => "FFFF"
    bool neg = d<0
    if neg then d = -d end if
    string res = ""
    integer whole = floor(d)
    d -= floor(d)
    while true do
        integer ch = mod(whole,base)
        ch += iff(ch>9?'A'-10:'0')
        res = ch&res
        whole = floor(whole/base)
        if whole=0 then exit end if
    end while
    if d>0 then
        res &= '.'
        while d>0 
          and (find(base,{2,4,8,16,32}) or length(res)<15) do
            d *= base
            integer digit = floor(d)
            d -= digit
            digit += iff(digit>9?'A'-10:'0')
            res &= digit
        end while
    end if
    if neg then res = '-'&res end if
    return res
end function

function to_dec(string s, integer base)
-- convert the string s (in the specified base) 
-- back into a normal decimal/floating point.
-- eg to_dec("FFFF",16) => 65535
    bool neg = (s[1]='-')
    if neg then s = s[2..$] end if
    integer dot = find('.',s)
    if dot then s[dot..dot] = "" end if
    atom res = 0
    for i=1 to length(s) do
        integer ch = upper(s[i])
        ch -= iff(ch>='A'?'A'-10:'0')
        res = res*base + ch
    end for
    if dot then res /= power(base,length(s)-dot+1) end if
    if neg then res = -res end if
    return res
end function

procedure test(atom f, integer base=2)
    string s = dec_to(f,base)
    atom g = to_dec(s,base),
         h = f-g
    string e = iff(h=0?"":sprintf(" (error: %g)",h))
    printf(1,"%.8g => 0(%d):%s => %.8g%s\n", {f,base,s,g,e})
end procedure
test(23.34375)
test(-23.34375)
test(11.90625)
test(-11.90625)
test(13)
test(0.1)
test(-5)
test(-0.25)
test(0)
test(65535,16)
test(23.7,35)
?to_dec("23.7",10)
?dec_to(23.7,10)
```

{{out}}

```txt

23.34375 => 0(2):10111.01011 => 23.34375
-23.34375 => 0(2):-10111.01011 => -23.34375
11.90625 => 0(2):1011.11101 => 11.90625
-11.90625 => 0(2):-1011.11101 => -11.90625
13 => 0(2):1101 => 13
0.1 => 0(2):0.0001100110011001100110011001100110011001100110011001101 => 0.1
-5 => 0(2):-101 => -5
-0.25 => 0(2):-0.01 => -0.25
0 => 0(2):0 => 0
65535 => 0(16):FFFF => 65535
23.7 => 0(35):N.OHHHHHHHHFIVE => 23.7 (error: -3.55271e-15)
23.7
"23.699999999999"

```

Aside: I was quite surprised to get 100% accuracy on these tests, but actually it is more of
a lucky coincidence in the way it is written, as the last few tests show.

The truth of the matter is simply that you ''can'' extract a float to a binary text representation exactly, 
in a way that you just cannot do for most other (ie non-power-2) bases.

Update: Added a limiter for non-base-2 fractions, as per 1/3 -> 0.333 forever in decimal. 
Base 2/4/8/16/32 are guaranteed to terminate anyway, but for other bases we need some limit - 
the 15 that I opted for is completely arbitrary.


## Python

Python has float.hex() and float.fromhex() that can be used to form our own binary format.

```python
hex2bin = dict('{:x} {:04b}'.format(x,x).split() for x in range(16))
bin2hex = dict('{:b} {:x}'.format(x,x).split() for x in range(16))

def float_dec2bin(d):
    neg = False
    if d < 0:
        d = -d
        neg = True
    hx = float(d).hex()
    p = hx.index('p')
    bn = ''.join(hex2bin.get(char, char) for char in hx[2:p])
    return (('-' if neg else '') + bn.strip('0') + hx[p:p+2]
            + bin(int(hx[p+2:]))[2:])

def float_bin2dec(bn):
    neg = False
    if bn[0] == '-':
        bn = bn[1:]
        neg = True
    dp = bn.index('.')
    extra0 = '0' * (4 - (dp % 4))
    bn2 = extra0 + bn
    dp = bn2.index('.')
    p = bn2.index('p')
    hx = ''.join(bin2hex.get(bn2[i:min(i+4, p)].lstrip('0'), bn2[i])
                 for i in range(0, dp+1, 4))
    bn3 = bn2[dp+1:p]
    extra0 = '0' * (4 - (len(bn3) % 4))
    bn4 = bn3 + extra0
    hx += ''.join(bin2hex.get(bn4[i:i+4].lstrip('0'))
                  for i in range(0, len(bn4), 4))
    hx = (('-' if neg else '') + '0x' + hx + bn2[p:p+2]
          + str(int('0b' + bn2[p+2:], 2)))
    return float.fromhex(hx)
```


{{out}}
Run the above in idle then you can do the following interactively:

```txt
>>> x = 23.34375
>>> y = float_dec2bin(x)
>>> y
'1.011101011p+100'
>>> float_bin2dec(y)
23.34375
>>> y = float_dec2bin(-x)
>>> y
'-1.011101011p+100'
>>> float_bin2dec(y)
-23.34375
>>> float_bin2dec('1011.11101p+0')
11.90625
>>> 
```



## Racket

The binary to number conversion is easy because it's supported by Racket. We can use  <code>string-&gt;number</code>, wrap it in a dedicated function or use the read extension.

```Racket
#lang racket

(define (string->number/binary x)
  (string->number x 2))

(string->number/binary "10.0101")
(newline)
(string->number/binary "0.01")
#b0.01
(string->number "0.01" 2)
(newline)
```

{{out}}

```txt
2.3125

0.25
0.25
0.25
```

Racket only supports the number to binary conversion for integer numbers, so we multiply the original number by a power of two, to get all the binary digits, and then we manipulate the string to place the point in the correct place.

```Racket
(define (number->string/binary x)
  (define decimals-places 10)
  (define digits-all (~r (inexact->exact (round (* x (expt 2 decimals-places))))
                         #:base 2
                         #:pad-string "0"
                         #:min-width (add1 decimals-places)))
  (define digits-length (string-length digits-all))
  (define integer-part (substring digits-all 0 (- digits-length decimals-places)))
  (define decimal-part* (string-trim (substring digits-all (- digits-length decimals-places))
                                    "0" 
                                    #:left? #f
                                    #:repeat? #t))
  (define decimal-part (if (string=? decimal-part* "") "0"  decimal-part*))
  (string-append integer-part "." decimal-part))
  

(number->string/binary 9.01)
(number->string/binary 9)
(number->string/binary 0.01)
(newline)
```

{{out}}

```txt
"1001.000000101"
"1001.000000000"
"0.000000101"
```

Some additional interesting examples

```Racket
(number->string/binary (string->number/binary "010110.0011010"))
(string-&gt;number/binary (number->string/binary .1))
(newline)

(number->string/binary (string->number/binary "0.11111111111"))
(string->number/binary "0.11111111111")
```

{{out}}

```txt
"10110.001101000"
0.099609375

"1.000000000"
0.99951171875
```



## REXX


### version 1

This REXX version will handle any number of digits,   and with   ''any''   base up to '''242'''
(using extended ASCII characters).

Bases up to '''62''' will just use decimal digits along with upper and lowercase
(Latin) letters.

This REXX program is a modified version of the original program which can
handle   ''any''   base (no limit),   and the original 

program did more extensive error checking.

This program handles numbers with a leading sign ('''-''', '''+''') and preserves the sign.

''Bases''    that are   ''negative''   are also supported   (which won't
be explained here).


This REXX version used a single function   ('''base''')   to
perform the base conversions.

Quite a bit of additional code was added to:
::*   verify that the number   (and other arguments)   are valid 
::*   check if the number has too many signs
::*   check if the number has too many decimal points
::*   check if the number is just a bare decimal point
::*   check if the number is null
::*   check for invalid digits/numerals for the specified base
::*   check for invalid   '''TO'''   base
::*   check for invalid   '''IN'''   base
::*   etc.

The number of digits (numerals) in the result may be specified   (for
continued fractions).   The default is '''60'''.

This REXX program can handle any sized number   (as per the number of digits/numerals)   that can be entered at the

command line.

```rexx
/*REXX pgm converts any number in a base to another base including fractions; bases≤242.*/
parse arg number toBase inBase digits .          /*obtain optional arguments from the CL*/
if toBase=='' | toBase==","  then toBase= 10     /*Not specified?  Then use the default.*/
if inBase=='' | inBase==","  then inBase= 10     /* "      "         "   "   "     "    */
if digits=='' | digits==","  then digits= 60     /* "      "         "   "   "     "    */
if number=='' | number==","  then call err  "no number specified."
if \datatype(toBase, 'W')    then call err  "invalid toBase: "    toBase
if \datatype(inBase, 'W')    then call err  "invalid inBase: "    inBase
if \datatype(digits, 'W')    then call err  "invalid digits: "    digits
numeric digits max(digits, length(number)) + 5   /*use a bigger numeric decimal digits. */
$= base(number, toBase, inBase)                  /*convert the # from a base to a base. */
numeric digits digits                            /*use a smaller numeric digs*/
if toBase==10  then if pos(., $)\==0  then $= format($)      /*maybe use the FORMAT BIF.*/
say number  ' (in base'    inBase")    =   "         $         ' (in base'      toBase")."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base: procedure;  parse arg x 1 s 2 1 ox,tt,ii,,oS
      @#= 0123456789;   @abc= 'abcdefghijklmnopqrstuvwxyz';    @abcu= @abc;    upper @abcu
      dontUse= @#'.+-'@abc || @abcu"0708090a0b0c0d"x;          OK= @# || @abcu || @abc
      $= OK || space( translate( xrange('1'x, "fe"x), , dontUse), 0)   /*max base string*/
      m= length($) - 1                                     /*M:  is the maximum base.   */
      if tt==''           then tt= 10                      /*assume base 10  if omitted.*/
      if ii==''           then ii= 10                      /*assume base 10  if omitted.*/
      i= abs(ii);              t= abs(tt)                  /*use absolute values for I&T*/
      if t==999 | t=="*"  then t= m                        /*T=999 or *?   Then use max.*/
      if t>m  then call err  'invalid range for ToBase:'     t";  the range is: "   2   m
      if i>m  then call err  'invalid range for InBase:'     i";  the range is: "   2   m
      != substr($, 1   +   10 * (tt<0), t)                 /*character string for base. */
      if tt<0   then != 0 || !                             /*Net base?   Prefix a zero. */
      if x==''  then return left(!, t)                     /*Is X null?  Show base chars*/
      @=substr($, 1   +   10 * (ii<0), i)                  /*@:  legal chars for base X.*/
      if s='-' | s="+"  then do;  x= substr(x, 2);   oS= s /*elide the sign character,  */
                             end                           /* ··· and save the original·*/
      if (ii>10 & ii<37) | (ii<0 & ii>-27)  then upper x   /*should  X  be uppercased ? */
      parse var x  w  '.'  g                               /*sep whole from fraction.   */
      if pos('-', x)>0 | pos("+", x)>0 | x==. | x=='' | ,  /*too many signs,  bare .  or*/
         pos(., g)>0  then call err 'illegal number: '  ox /*too many decimal points ?  */
      __=w || g;                     _= verify(__, @'.')   /*# have any unusual digits? */
      if _\==0  then call err 'illegal char in number:'   ox   'char='    substr(__, _, 1)
      if i\==10 then do    /*────────────────────────────────convert # base I─►base ten.*/
                     _=0;  p=0;      do j=length(w)  to 1  by -1  while  w\==''
                                     _= _ + (( pos( substr(w, j, 1), @)-1) * i**p);  p=p+1
                                     end   /*j*/           /*increase power of base  [↑]*/
                     w=_;  p=1;  _=0                       /*[↓] convert fractional part*/
                                     do j=1  for length(g)
                                     _= _ + (( pos( substr(g, j, 1), @)-1)/i**p);    p=p+1
                                     end   /*j*/           /*increase power of base  [↑]*/
                     g= _
                     end
                else if g\==''  then g= "."g               /*reinsert period if needed. */
      if t\==10 then do    /*────────────────────────────────convert # base ten─►base T.*/
                     if w\==''  then do                    /*convert whole number part. */
                                         do j=1  until t**j>w;  end   /*j*/
                                     n=
                                         do k=j-1  to 1  by -1;         _= t**k
                                         n=n || substr(!, 1 + w%_, 1);          w= w // _
                                         end     /*k*/                /*remainder ≡  // */
                                     w= n || substr(!, 1+w, 1)
                                     end
                     if g\==''  then do;  n=                /*convert fractional part.  */
                                             do digits()+1  while g\==0;  p= g*t;  g= p//1
                                             n= n || substr(!, 1 + p%1, 1)
                                             end   /*digits ···*/
                                     if n==0    then n=
                                     if n\==''  then n= '.'n          /*only a fraction?*/
                                     g= n
                                     end
                     end
      return oS || word( strip( space(w), 'L', 0)strip( strip(g, , 0),  "T", .)  0,  1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:  say;         say '***error***: '    arg(1);                 say;             exit 13
```

{{out|output|text=  when using the input of:     <tt> 23.34375   2 </tt>}}

```txt

23.34375  (in base 10)    =    10111.01011  (in base 2).

```

{{out|output|text=  when using the input of:     <tt> 1011.11101   10   2 </tt>}}

```txt

1011.11101  (in base 2)    =    11.90625  (in base 10).

```

{{out|output|text=  when using the input of:     <tt> 3.14159265358979323846264338327950288419716939937510582097494   62 </tt>}}

```txt

3.14159265358979323846264338327950288419716939937510582097494  (in base 10)    =    3.8mHUcirZ3g3aaX5Bn156eBkfOx43HPGx7xT3yBX1Aoh3TAAEolLiHWo8Z4XVLWesfA6  (in base 62).

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 08.02.2014 Walter Pachl
*--------------------------------------------------------------------*/
Call df2bf 23.34375,10111.01011
Call bf2df 1011.11101,11.90625
Call df2bf -23.34375,-10111.01011
Call bf2df -1011.11101,-11.90625
Exit

bf2df: Procedure
  Parse Arg x,soll
  If left(x,1)='-' Then Do
    sign='-'
    x=substr(x,2)
    End
  Else sign=''
  Parse Var x int '.' fract
  int=reverse(int)
  vb=1
  res=0
  Do while int<>''
    Parse Var int d +1 int
    res=res+d*vb
    vb=vb*2
    End
  vb=1
  Do while fract<>''
    vb=vb/2
    Parse Var fract d +1 fract
    res=res+d*vb
    End
  res=sign||res
  Say sign||x '->' res
  If res<>soll Then
    Say 'soll='soll
  Return

df2bf: Procedure
  Parse Arg x,soll
  If left(x,1)='-' Then Do
    sign='-'
    x=substr(x,2)
    End
  Else sign=''
  res=''
  Parse Var x int '.' +0 fract
  Do While int>0
    dig=int//2
    int=int%2
    res=dig||res
    End
  If res='' Then res='0'
  vb=1
  bf=''
  Do i=1 To 30 while fract>0
    vb=vb/2
    If fract>=vb Then Do
      bf=bf'1'
      fract=fract-vb
      End
    Else
      bf=bf'0'
    End
  res=sign||res'.'bf
  Say sign||x '->' res
  If res<>soll Then
    Say 'soll='soll
  Return
```

'''Output:'''

```txt
23.34375 -> 10111.01011
1011.11101 -> 11.90625
-23.34375 -> -10111.01011
-1011.11101 -> -11.90625
```



## Ruby


```ruby
def dec2bin(dec, precision=16)    # String => String
  int, df = dec.split(".")
  minus = int.delete!("-")
  bin = (minus ? "-" : "") + int.to_i.to_s(2) + "."
  if df and df.to_i>0
    fp = ("."+df).to_f
    digit = 1
    until fp.zero? or digit>precision
      fp *= 2
      n = fp.to_i
      bin << n.to_s
      fp -= n
      digit += 1
    end
  else
    bin << "0"
  end
  bin
end

def bin2dec(bin)              # String => String
  int, df = bin.split(".")
  minus = int.delete!("-")
  dec = (minus ? "-" : "") + int.to_i(2).to_s
  if df
    dec << (df.to_i(2) / 2.0**(df.size)).to_s[1..-1]
  else
    dec << ".0"
  end
end

data = %w[23.34375 11.90625 -23.34375 -11.90625]
data.each do |dec|
  bin  = dec2bin(dec)
  dec2 = bin2dec(bin)
  puts "%10s => %12s =>%10s" % [dec, bin, dec2]
end
```


{{out}}

```txt

  23.34375 =>  10111.01011 =>  23.34375
  11.90625 =>   1011.11101 =>  11.90625
 -23.34375 => -10111.01011 => -23.34375
 -11.90625 =>  -1011.11101 => -11.90625

```



## Scala

===Idiomatic (FP with tailrec)===

```Scala
import java.lang.Long

import scala.annotation.tailrec

object DecimalFloatPoint2Binary extends App {

  def doubleToBin(d: Double): String = {
    require(d >= 0.0, "Only positive values are allowed.")

    def fraction2BinaryFractionString(s: String, frac: Double) = {
      @tailrec
      def loop(acc: String, mid: Double): String = {
        if (mid > 0.0) {
          val r = mid * 2.0
          if (r >= 1.0) loop(acc + "1", r - 1) else loop(acc + "0", r)
        } else acc
      }

      loop(s + ".", frac)
    }

    val whole = math.floor(d).toLong

    fraction2BinaryFractionString(Long.toString(whole, 2), d - whole)
  }

  def binToDec(s: String) = {
    def num = Long.parseLong(s.replace(".", ""), 2)

    def den = Long.parseLong("1" + s.split('.')(1).replace("1", "0"), 2)

    num.toDouble / den
  }

  { // main
    println( { def d = 23.34375;     s"$d\t => ${doubleToBin(d)}" } )
    println( { def s = "1011.11101"; s"$s\t => ${binToDec(s)}"    } )
  }
}
```

{{Out}}Experience running it in your browser by [https://scastie.scala-lang.org/auzWgFqCRBaYoOaJV92tgw Scastie (remote JVM)].

## Sidef


```ruby
func dec2bin(String n) {
    Num(Num(n, 10).base(2), 10)
}

func bin2dec(String n) {
    Num(Num(n, 10).base(10), 2)
}

with("23.34375")   { |s| say ("  #{s} => ", dec2bin(s)) }
with("1011.11101") { |s| say (  "#{s} => ", bin2dec(s)) }
```

{{out}}

```txt

  23.34375 => 10111.01011
1011.11101 => 11.90625

```



## Tcl

By far the easiest way to do this is to use Tcl's built-in handling of IEEE arithmetic, converting the IEEE representation into the string representation we want (and ''vice versa'') by simple string manipulations.
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc dec2bin x {
    binary scan [binary format R $x] B* x
    regexp {(.)(.{8})(.{23})} $x -> s e m
    binary scan [binary format B* $e] cu e
    if {$e == 0 && ![string match *1* $m]} {
	# Special case for zero
	set m 0.0
    } else {
	incr e -127

	set m 1$m
	if {$e < 0} {
	    set m [string repeat "0" [expr {-$e}]]$m
	    set m [string trimright [regsub {^.} $m "&."] "0"]
	} else {
	    set m [string trimright [regsub ^.[string repeat . $e] $m "&."] "0"]
	}
	if {[string match *. $m]} {
	    append m 0
	}
    }
    if {$s} {
	return -$m
    } else {
	return $m
    }
}
proc bin2dec x {
    if {[regexp {^-} $x]} {
	set s 1
	set x [string trimleft $x -0]
    } else {
	set s 0
	set x [string trimleft $x +0]
    }
    lassign [split [string trimright $x 0] .] fore aft
    if {[string length $fore]} {
	set e [expr {[string length $fore] - 1}]
	set digits [string range $fore$aft 1 end]
    } elseif {[string length $aft]} {
	set digits [string range [string trimleft $aft 0] 1 end]
	set e [expr {[string length $digits] - [string length $aft]}]
    } else {
	set e -127
	set digits {}
    }
    incr e 127
    binary scan [binary format B* [format %b%08b%-023s $s $e $digits]] R x
    return $x
}

foreach case {77 0.25 0.15625 0.1 -33.8 0 1 2 3 23.34375 11.90625} {
    set b [dec2bin $case]
    set d [bin2dec $b]
    puts "$case => $b => $d"
}
```

{{out}}

```txt

77 => 1001101.0 => 77.0
0.25 => 0.01 => 0.25
0.15625 => 0.00101 => 0.15625
0.1 => 0.000110011001100110011001101 => 0.10000000149011612
-33.8 => -100001.110011001100110011 => -33.79999923706055
0 => 0.0 => 0.0
1 => 1.0 => 1.0
2 => 10.0 => 2.0
3 => 11.0 => 3.0
23.34375 => 10111.01011 => 23.34375
11.90625 => 1011.11101 => 11.90625

```

Adapting the code to work with IEEE double-precision floats is left as an exercise for the reader, as is dealing with the trickier special cases of the infinities and NaN.


## zkl

Rather limited.

```zkl
fcn bin2float(bstr){  // no exponents
   bstr=bstr.strip();
   m:=bstr[0,1]=="-"; if(m)bstr=bstr[1,*]; m=m and -1 or 1;
   a,b:=bstr.split(".").apply(fcn(s){ s and s or 0 }).append(0,0);
   (a.toInt(2).toFloat() + b.toInt(2).toFloat()/(2).pow(b.len()))*m
}
```


```zkl
foreach bstr in (T("1011.11101","0.01","0.11111111111","-.1","","1")){
   println(bstr," --> ",bin2float(bstr).toString(20))
}
```

{{out}}

```txt

1011.11101 --> 11.90625
0.01 --> 0.25
0.11111111111 --> 0.99951171875
-.1 --> -0.5
 --> 0
1 --> 1

```


```zkl
fcn float2bin(x,digitsOfPrecision=20){
   m,zeros:="","0"*digitsOfPrecision;
   if(x<0){ m="-"; x=-x }
   a,b:=x.modf();  // int and fractional parts
   b=(b*(2).pow(digitsOfPrecision)).round().toInt().toString(2);
   b=zeros[0,digitsOfPrecision-b.len()] + b;   // don't drop leading zeros
   if(z:=b.reverse().prefix(zeros)) b=b[0,-z]; // remove trailing zeros
   String(m,a.toString(2),".",b);   
}
```


```zkl
foreach x in (T(23.34375,(0.0).pi,-33.8,0.1,0.15625)){
   println(x," --> ",s:=float2bin(x)," --> ",bin2float(s).toString(20));
}
```

{{out}}

```txt

23.3438 --> 10111.01011 --> 23.34375
3.14159 --> 11.00100100001111110111 --> 3.1415929794311523438
-33.8 --> -100001.11001100110011001101 --> -33.800000190734863281
0.1 --> 0.0001100110011001101 --> 0.1000003814697265625
0.15625 --> 0.00101 --> 0.15625

```

