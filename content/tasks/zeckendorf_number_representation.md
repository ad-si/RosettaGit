+++
title = "Zeckendorf number representation"
description = ""
date = 2019-03-27T15:33:22Z
aliases = []
[extra]
id = 12371
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "autoit",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elena",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "logo",
  "lua",
  "mathematica",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "plaintex",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "tcl",
  "ubasic_4th",
  "vba",
  "vbscript",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

Just as numbers can be represented in a positional notation as sums of multiples of the powers of ten (decimal) or two (binary); all the positive integers can be represented as the sum of one or zero times the distinct members of the Fibonacci series.

Recall that the first six distinct Fibonacci numbers are:  1, 2, 3, 5, 8, 13.

The decimal number eleven can be written as 0*13 + 1*8 + 0*5 + 1*3 + 0*2 + 0*1 or 010100 in positional notation where the columns represent multiplication by a particular member of the sequence. Leading zeroes are dropped so that 11 decimal becomes 10100.

10100 is not the only way to make 11 from the Fibonacci numbers however;  0*13 + 1*8 + 0*5 + 0*3 + 1*2 + 1*1 or 010011 would also represent decimal 11. For a true Zeckendorf number there is the added restriction that ''no two consecutive Fibonacci numbers can be used'' which leads to the former unique solution.


## Task

Generate and show here a table of the Zeckendorf number representations of the decimal numbers zero to twenty, in order.

The intention in this task to find the Zeckendorf form of an arbitrary integer.  The Zeckendorf form can be iterated by some bit twiddling rather than calculating each value separately but leave that to another separate task.


;Also see:
*   [http://oeis.org/A014417 OEIS A014417]   for the the sequence of required results.
*   [http://www.youtube.com/watch?v=kQZmZRE0cQY&list=UUoxcjq-8xIDTYp3uz647V5A&index=3&feature=plcp Brown's Criterion - Numberphile]


## Related tasks

*   [[Fibonacci sequence]]





## 360 Assembly

```360asm
*        Zeckendorf number representation  04/04/2017
ZECKEN   CSECT
         USING  ZECKEN,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,0               i=0
       DO WHILE=(C,R6,LE,=A(20))   do i=0 to 20
         MVC    PG,=CL80'xx : '      init buffer
         LA     R10,PG               pgi=0
         XDECO  R6,XDEC              i
         MVC    0(2,R10),XDEC+10     output i
         LA     R10,5(R10)           pgi+=5
         MVC    FIB,=A(1)            fib(1)=1
         MVC    FIB+4,=A(2)          fib(2)=2
         LA     R7,2                 j=2
         LR     R1,R7                j
         SLA    R1,2                 @fib(j)
       DO WHILE=(C,R6,GT,FIB-4(R1)   do while fib(j)<i
         LA     R7,1(R7)               j++
         LR     R1,R7                  j
         SLA    R1,2                   ~
         L      R2,FIB-8(R1)           fib(j-1)
         A      R2,FIB-12(R1)          fib(j-2)
         ST     R2,FIB-4(R1)           fib(j)=fib(j-1)+fib(j-2)
         LR     R1,R7                  j
         SLA    R1,2                   @fib(j)
       ENDDO    ,                    enddo j
         LR     R8,R6                k=i
         MVI    BB,X'00'             bb=false
       DO WHILE=(C,R7,GE,=A(1))      do j=j to 1 by -1
         LR     R1,R7                  j
         SLA    R1,2                   ~
       IF C,R8,GE,FIB-4(R1) THEN       if fib(j)<=k then
         MVI    BB,X'01'                 bb=true
         MVC    0(1,R10),=C'1'           output '1'
         LA     R10,1(R10)               pgi+=1
         LR     R1,R7                    j
         SLA    R1,2                     ~
         S      R8,FIB-4(R1)             k=k-fib(j)
       ELSE     ,                      else
       IF CLI,BB,EQ,X'01' THEN           if bb then
         MVC    0(1,R10),=C'0'             output '0'
         LA     R10,1(R10)                 pgi+=1
       ENDIF    ,                        endif
       ENDIF    ,                      endif
         BCTR   R7,0                   j--
       ENDDO    ,                    enddo j
       IF CLI,BB,NE,X'01' THEN       if not bb then
         MVC    0(1,R10),=C'0'         output '0'
       ENDIF    ,                    endif
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
FIB      DS     32F                Fibonnacci table
BB       DS     X                  flag
PG       DS     CL80               buffer
XDEC     DS     CL12               temp
         YREGS
         END    ZECKEN
```

```txt

 0 : 0
 1 : 1
 2 : 10
 3 : 100
 4 : 101
 5 : 1000
 6 : 1001
 7 : 1010
 8 : 10000
 9 : 10001
10 : 10010
11 : 10100
12 : 10101
13 : 100000
14 : 100001
15 : 100010
16 : 100100
17 : 100101
18 : 101000
19 : 101001
20 : 101010

```



## Ada



```Ada
with Ada.Text_IO, Ada.Strings.Unbounded;

procedure Print_Zeck is

   function Zeck_Increment(Z: String) return String is
   begin
      if Z="" then
	 return "1";
      elsif Z(Z'Last) = '1' then
	 return Zeck_Increment(Z(Z'First .. Z'Last-1)) & '0';
      elsif Z(Z'Last-1) = '0' then
	 return Z(Z'First .. Z'Last-1) & '1';
      else -- Z has at least two digits and ends with "10"
	 return Zeck_Increment(Z(Z'First .. Z'Last-2)) & "00";
      end if;
   end Zeck_Increment;

   use Ada.Strings.Unbounded;
   Current: Unbounded_String := Null_Unbounded_String;

begin
   for I in 1 .. 20 loop
      Current := To_Unbounded_String(Zeck_Increment(To_String(Current)));
      Ada.Text_IO.Put(To_String(Current) & " ");
   end loop;
end Print_Zeck;
```


```txt
1 10 100 101 1000 1001 1010 10000 10001 10010 10100 10101 100000 100001 100010 100100 100101 101000 101001 101010
```



## ALGOL 68


```algol68
# print some Zeckendorf number representations                             #

# We handle 32-bit numbers, the maximum fibonacci number that can fit in a #
# 32 bit number is F(45)                                                   #

# build a table of 32-bit fibonacci numbers                                #
[ 45 ]INT fibonacci;
fibonacci[ 1 ] := 1;
fibonacci[ 2 ] := 2;
FOR i FROM 3 TO UPB fibonacci DO fibonacci[ i ] := fibonacci[ i - 1 ] + fibonacci[ i - 2 ] OD;

# returns the Zeckendorf representation of n or "?" if one cannot be found #
PROC to zeckendorf = ( INT n )STRING:
     IF n = 0 THEN
        "0"
     ELSE
        STRING result := "";
        INT    f pos  := UPB fibonacci;
        INT    rest   := ABS n;
        # find the first non-zero Zeckendorf digit                        #
        WHILE f pos > LWB fibonacci AND rest < fibonacci[ f pos ] DO
            f pos -:= 1
        OD;
        # if we found a digit, build the representation                   #
        IF f pos >= LWB fibonacci THEN
            # have a digit                                                #
            BOOL skip digit := FALSE;
            WHILE f pos >= LWB fibonacci DO
                IF   rest <= 0 THEN
                    result    +:= "0"
                ELIF skip digit THEN
                    # we used the previous digit                          #
                    skip digit := FALSE;
                    result    +:= "0"
                ELIF rest < fibonacci[ f pos ] THEN
                    # can't use the digit at f pos                        #
                    skip digit := FALSE;
                    result    +:= "0"
                ELSE
                    # can use this digit                                  #
                    skip digit := TRUE;
                    result    +:= "1";
                    rest      -:= fibonacci[ f pos ]
                FI;
                f pos -:= 1
            OD
        FI;
        IF rest = 0 THEN
            # found a representation                                      #
            result
        ELSE
            # can't find a representation                                 #
            "?"
        FI
     FI; # to zeckendorf #

FOR i FROM 0 TO 20 DO
    print( ( whole( i, -3 ), " ", to zeckendorf( i ), newline ) )
OD

```

```txt

  0 0
  1 1
  2 10
  3 100
  4 101
  5 1000
  6 1001
  7 1010
  8 10000
  9 10001
 10 10010
 11 10100
 12 10101
 13 100000
 14 100001
 15 100010
 16 100100
 17 100101
 18 101000
 19 101001
 20 101010
```




## AppleScript

{{Trans|Haskell}} ('''mapAccumuL''' example)


```AppleScript
-- ZECKENDORF NUMBERS --------------------------------------------------------

-- zeckendorf :: Int -> String
on zeckendorf(n)
    script f
        on |λ|(n, x)
            if n < x then
                [n, 0]
            else
                [n - x, 1]
            end if
        end |λ|
    end script

    if n = 0 then
        {0} as string
    else
        item 2 of mapAccumL(f, n, |reverse|(just of tailMay(fibUntil(n)))) as string
    end if
end zeckendorf

-- fibUntil :: Int -> [Int]
on fibUntil(n)
    set xs to {}
    set limit to n

    script atLimit
        property ceiling : limit
        on |λ|(x)
            (item 2 of x) > (atLimit's ceiling)
        end |λ|
    end script

    script nextPair
        property series : xs
        on |λ|([a, b])
            set nextPair's series to nextPair's series & b
            [b, a + b]
        end |λ|
    end script

    |until|(atLimit, nextPair, {0, 1})
    return nextPair's series
end fibUntil

-- TEST ----------------------------------------------------------------------
on run

    intercalate(linefeed, ¬
        map(zeckendorf, enumFromTo(0, 20)))

end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
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

-- 'The mapAccumL function behaves like a combination of map and foldl;
-- it applies a function to each element of a list, passing an
-- accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.' (see Hoogle)

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
on mapAccumL(f, acc, xs)
    script
        on |λ|(a, x)
            tell mReturn(f) to set pair to |λ|(item 1 of a, x)
            [item 1 of pair, (item 2 of a) & item 2 of pair]
        end |λ|
    end script

    foldl(result, [acc, []], xs)
end mapAccumL

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- reverse :: [a] -> [a]
on |reverse|(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|

-- tailMay :: [a] -> Maybe [a]
on tailMay(xs)
    if length of xs > 1 then
        {nothing:false, just:items 2 thru -1 of xs}
    else
        {nothing:true}
    end if
end tailMay

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x

    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|
```

```txt
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010
```



## AutoHotkey

```AutoHotkey
Fib := NStepSequence(1, 2, 2, 20)
Loop, 21 {
	i := A_Index - 1
	, Out .= i ":`t", n := ""
	Loop, % Fib.MaxIndex() {
		x := Fib.MaxIndex() + 1 - A_Index
		if (Fib[x] <= i)
			n .= 1, i -= Fib[x]
		else
			n .= 0
	}
	Out .= (n ? LTrim(n, "0") : 0) "`n"
}
MsgBox, % Out

NStepSequence(v1, v2, n, k) {
    a := [v1, v2]
	Loop, % k - 2 {
		a[j := A_Index + 2] := 0
		Loop, % j < n + 2 ? j - 1 : n
			a[j] += a[j - A_Index]
	}
	return, a
}
```
[http://rosettacode.org/wiki/Fibonacci_n-step_number_sequences#AutoHotkey NStepSequence()]
'''Output:'''

```txt
0:	0
1:	1
2:	10
3:	100
4:	101
5:	1000
6:	1001
7:	1010
8:	10000
9:	10001
10:	10010
11:	10100
12:	10101
13:	100000
14:	100001
15:	100010
16:	100100
17:	100101
18:	101000
19:	101001
20:	101010
```



## AutoIt


```autoit

For $i = 0 To 20
	ConsoleWrite($i &": "& Zeckendorf($i)&@CRLF)
Next

Func Zeckendorf($int, $Fibarray = "")
	If Not IsArray($Fibarray) Then $Fibarray = Fibonacci($int)
	Local $ret = ""
	For $i = UBound($Fibarray) - 1 To 1 Step -1
		If $Fibarray[$i] > $int And $ret = "" Then ContinueLoop ; dont use Leading  Zeros
		If $Fibarray[$i] > $int Then
			$ret &= "0"
		Else
			If StringRight($ret, 1) <>  "1" Then
				$ret &= "1"
				$int -= $Fibarray[$i]
			Else
				$ret &= "0"
			EndIf
		EndIf
	Next
	If $ret = "" Then $ret = "0"
	Return $ret
EndFunc   ;==>Zeckendorf

Func Fibonacci($max)
	$AList = ObjCreate("System.Collections.ArrayList")
	$AList.add("0")
	$current = 0
	While True
		If $current > 1 Then
			$count = $AList.Count
			$current = $AList.Item($count - 1)
			$current = $current + $AList.Item($count - 2)
		Else
			$current += 1
		EndIf
		$AList.add($current)
		If $current > $max Then ExitLoop
	WEnd
	$Array = $AList.ToArray
	Return $Array
EndFunc   ;==>Fibonacci

```

'''Output:'''

```txt

0: 0
1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## BBC BASIC


```bbcbasic
      FOR n% = 0 TO 20
        PRINT n% RIGHT$("       " + FNzeckendorf(n%), 8)
      NEXT
      PRINT '"Checking numbers up to 10000..."
      FOR n% = 21 TO 10000
        IF INSTR(FNzeckendorf(n%), "11") STOP
      NEXT
      PRINT "No Zeckendorf numbers contain consecutive 1's"
      END

      DEF FNzeckendorf(n%)
      LOCAL i%, o$, fib%() : DIM fib%(45)
      fib%(0) = 1 : fib%(1) = 1 : i% = 1
      REPEAT
        i% += 1
        fib%(i%) = fib%(i%-1) + fib%(i%-2)
      UNTIL fib%(i%) > n%
      REPEAT
        i% -= 1
        IF n% >= fib%(i%) THEN
          o$ += "1"
          n% -= fib%(i%)
        ELSE
          o$ += "0"
        ENDIF
      UNTIL i% = 1
      = o$
```

'''Output:'''

```txt

         0       0
         1       1
         2      10
         3     100
         4     101
         5    1000
         6    1001
         7    1010
         8   10000
         9   10001
        10   10010
        11   10100
        12   10101
        13  100000
        14  100001
        15  100010
        16  100100
        17  100101
        18  101000
        19  101001
        20  101010

Checking numbers up to 10000...
No Zeckendorf numbers contain consecutive 1's

```



## Befunge

The first number on the stack, <tt>45*</tt>, specifies the range of values to display. However, the algorithm depends on a hardcoded list of Fibonacci values (currently just 10) so the theoretical maximum is 143. It's also constrained by the range of a Befunge data cell, which on many implementations will be as low as 127.


```befunge
45*83p0>:::.0`"0"v
v53210p 39+!:,,9+<
>858+37 *66g"7Y":v
>3g`#@_^   v\g39$<
^8:+1,+5_5<>-:0\`|
v:-\g39_^#:<*:p39<
>0\`:!"0"+#^ ,#$_^
```


```txt
0       0
1       1
2       10
3       100
4       101
5       1000
6       1001
7       1010
8       10000
9       10001
10      10010
11      10100
12      10101
13      100000
14      100001
15      100010
16      100100
17      100101
18      101000
19      101001
20      101010
```



## C


```c
#include <stdio.h>

typedef unsigned long long u64;

#define FIB_INVALID (~(u64)0)

u64 fib[] = {
	1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,
	2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418,
	317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465,
	14930352, 24157817, 39088169, 63245986, 102334155, 165580141,
	267914296, 433494437, 701408733, 1134903170, 1836311903,
	2971215073ULL, 4807526976ULL, 7778742049ULL, 12586269025ULL,
	20365011074ULL, 32951280099ULL, 53316291173ULL, 86267571272ULL,
	139583862445ULL, 225851433717ULL, 365435296162ULL, 591286729879ULL,
	956722026041ULL, 1548008755920ULL, 2504730781961ULL, 4052739537881ULL,
	6557470319842ULL, 10610209857723ULL, 17167680177565ULL,

	27777890035288ULL // this 65-th one is for range check
};

u64 fibbinary(u64 n)
{
	if (n >= fib[64]) return FIB_INVALID;

	u64 ret = 0;
	int i;
	for (i = 64; i--; )
		if (n >= fib[i]) {
			ret |= 1ULL << i;
			n -= fib[i];
		}

	return ret;
}

void bprint(u64 n, int width)
{
	if (width > 64) width = 64;

	u64 b;
	for (b = 1ULL << (width - 1); b; b >>= 1)
		putchar(b == 1 && !n
			? '0'
			: b > n	? ' '
				: b & n ? '1' : '0');
	putchar('\n');
}

int main(void)
{
	int i;

	for (i = 0; i <= 20; i++)
		printf("%2d:", i), bprint(fibbinary(i), 8);

	return 0;
}
```

```txt

 0:       0
 1:       1
 2:      10
 3:     100
 4:     101
 5:    1000
 6:    1001
 7:    1010
 8:   10000
 9:   10001
10:   10010
11:   10100
12:   10101
13:  100000
14:  100001
15:  100010
16:  100100
17:  100101
18:  101000
19:  101001
20:  101010

```



## C++


### Using a C++11 User Defined Literal

see [[Fibonacci sequence#Using Zeckendorf Numbers|Here]] for a further example using this class.

```cpp

// For a class N which implements Zeckendorf numbers:
// I define an increment operation ++()
// I define a comparison operation <=(other N)
// Nigel Galloway October 22nd., 2012
#include <iostream>
class N {
private:
  int dVal = 0, dLen;
public:
  N(char const* x = "0"){
    int i = 0, q = 1;
    for (; x[i] > 0; i++);
    for (dLen = --i/2; i >= 0; i--) {
      dVal+=(x[i]-48)*q;
      q*=2;
  }}
  const N& operator++() {
    for (int i = 0;;i++) {
      if (dLen < i) dLen = i;
      switch ((dVal >> (i*2)) & 3) {
        case 0: dVal += (1 << (i*2)); return *this;
        case 1: dVal += (1 << (i*2)); if (((dVal >> ((i+1)*2)) & 1) != 1) return *this;
        case 2: dVal &= ~(3 << (i*2));
  }}}
  const bool operator<=(const N& other) const {return dVal <= other.dVal;}
  friend std::ostream& operator<<(std::ostream&, const N&);
};
N operator "" N(char const* x) {return N(x);}
std::ostream &operator<<(std::ostream &os, const N &G) {
  const static std::string dig[] {"00","01","10"}, dig1[] {"","1","10"};
  if (G.dVal == 0) return os << "0";
  os << dig1[(G.dVal >> (G.dLen*2)) & 3];
  for (int i = G.dLen-1; i >= 0; i--) os << dig[(G.dVal >> (i*2)) & 3];
  return os;
}

```

I may now write:

```cpp

int main(void) {
//for (N G; G <= 101010N; ++G) std::cout << G << std::endl;       // from zero to 101010M
  for (N G(101N); G <= 101010N; ++G) std::cout << G << std::endl; // from 101N to 101010N
  return 0;
}

```

Which produces:
```txt

101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010

```



## C#


```c#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Zeckendorf
{
    class Program
    {
        private static uint Fibonacci(uint n)
        {
            if (n < 2)
            {
                return n;
            }
            else
            {
                return Fibonacci(n - 1) + Fibonacci(n - 2);
            }
        }

        private static string Zeckendorf(uint num)
        {
            IList<uint> fibonacciNumbers = new List<uint>();
            uint fibPosition = 2;

            uint currentFibonaciNum = Fibonacci(fibPosition);

            do
            {
                fibonacciNumbers.Add(currentFibonaciNum);
                currentFibonaciNum = Fibonacci(++fibPosition);
            } while (currentFibonaciNum <= num);

            uint temp = num;
            StringBuilder output = new StringBuilder();

            foreach (uint item in fibonacciNumbers.Reverse())
            {
                if (item <= temp)
                {
                    output.Append("1");
                    temp -= item;
                }
                else
                {
                    output.Append("0");
                }
            }

            return output.ToString();
        }

        static void Main(string[] args)
        {
            for (uint i = 1; i <= 20; i++)
            {
                string zeckendorfRepresentation = Zeckendorf(i);
                Console.WriteLine(string.Format("{0} : {1}", i, zeckendorfRepresentation));
            }

            Console.ReadKey();
        }
    }
}

```

```txt

1 : 1
2 : 10
3 : 100
4 : 101
5 : 1000
6 : 1001
7 : 1010
8 : 10000
9 : 10001
10 : 10010
11 : 10100
12 : 10101
13 : 100000
14 : 100001
15 : 100010
16 : 100100
17 : 100101
18 : 101000
19 : 101001
20 : 101010

```



## Clojure


```clojure
(def fibs (lazy-cat [1 1] (map + fibs (rest fibs))))

(defn z [n]
  (if (zero? n)
    "0"
    (let [ps (->> fibs (take-while #(<= % n)) rest reverse)
          fz (fn [[s n] p]
                (if (>= n p)
                  [(conj s 1) (- n p)]
                  [(conj s 0) n]))]
      (->> ps (reduce fz [[] n]) first (apply str)))))

(doseq [n (range 0 21)] (println n (z n)))
```



## Common Lisp

Common Lisp's arbitrary precision integers should handle any positive input:

```lisp
(defun zeckendorf (n)
   "returns zeckendorf integer of n (see OEIS A003714)"
   (let ((fib '(2 1)))
	;; extend Fibonacci sequence long enough
	(loop while (<= (car fib) n) do
	      (push (+ (car fib) (cadr fib)) fib))
	(loop with r = 0 for f in fib do
	      (setf r (* 2 r))
	      (when (>= n f) (setf n (- n f))
			     (incf r))
	      finally (return r))))

;;; task requirement
(loop for i from 0 to 20 do
      (format t "~2D: ~2R~%" i (zeckendorf i)))
```



```lisp

;; Print Zeckendorf numbers upto 20.
;; I have implemented this as a state machine.
;; Nigel Galloway - October 13th., 2012
;;
(let ((fibz '(13 8 5 3 2 1))) (dotimes (G 21) (progn (format t "~S is " G)
   (let ((z 0) (ng G)) (dolist (N fibz)
     (if (> z 1) (progn (setq z 1) (format t "~S" 0))
       (if (>= ng N) (progn (setq z 2) (setq ng (- ng N)) (format t "~S" 1))
         (if (= z 1) (format t "~S" 0)))))
   (if (= z 0) (format t "~S~%" 0) (format t "~%"))))))

```

```txt

0 is 0
1 is 1
2 is 10
3 is 100
4 is 101
5 is 1000
6 is 1001
7 is 1010
8 is 10000
9 is 10001
10 is 10010
11 is 10100
12 is 10101
13 is 100000
14 is 100001
15 is 100010
16 is 100100
17 is 100101
18 is 101000
19 is 101001
20 is 101010

```



## D

```d
import std.stdio, std.range, std.algorithm, std.functional;

void main() {
    size_t
    .max
    .iota
    .filter!q{ !(a & (a >> 1)) }
    .take(21)
    .binaryReverseArgs!writefln("%(%b\n%)");
}
```

```txt
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010
```


```d
import std.stdio, std.typecons;

int zeckendorf(in int n) pure nothrow {
     Tuple!(int,"remaining", int,"set")
     zr(in int fib0, in int fib1, in int n, in uint bit) pure nothrow {
        if (fib1 > n)
            return typeof(return)(n, 0);
        auto rs = zr(fib1, fib0 + fib1, n, bit + 1);
        if (fib1 <= rs.remaining) {
            rs.set |= 1 << bit;
            rs.remaining -= fib1;
        }
        return rs;
    }

    return zr(1, 1, n, 0)[1];
}

void main() {
    foreach (i; 0 .. 21)
        writefln("%2d: %6b", i, zeckendorf(i));
}
```

```txt
 0:      0
 1:      1
 2:     10
 3:    100
 4:    101
 5:   1000
 6:   1001
 7:   1010
 8:  10000
 9:  10001
10:  10010
11:  10100
12:  10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010
```


(Same output.)

```d
import std.stdio, std.algorithm, std.range;

string zeckendorf(size_t n) {
    if (n == 0)
        return "0";
    auto fibs = recurrence!q{a[n - 1] + a[n - 2]}(1, 2);

    string result;
    foreach_reverse (immutable f; fibs.until!(x => x > n).array) {
        result ~= (f <= n) ? '1' : '0';
        if (f <= n)
            n -= f;
    }

    return result;
}

void main() {
    foreach (immutable i; 0 .. 21)
        writefln("%2d: %6s", i, i.zeckendorf);
}
```



## EchoLisp

We analytically find the first fibonacci(i) >= n, using the formula i = log((n* Φ) + 0.5) / log(Φ) .

```scheme

;; special fib's starting with 1 2 3 5 ...
(define (fibonacci n)
    (+ (fibonacci (1- n)) (fibonacci (- n 2))))
(remember 'fibonacci #(1 2))

(define-constant Φ (// (1+ (sqrt 5)) 2))
(define-constant logΦ (log Φ))
;; find i : fib(i) >= n
(define (iFib n)
   (floor (// (log (+ (* n Φ) 0.5)) logΦ)))

;; left trim zeroes
(string-delimiter "")
(define (zeck->string digits)
        (if (!= 0 (first digits))
            (string-join digits "")
            (zeck->string (rest digits))))

(define (Zeck n)
        (cond
        (( < n 0) "no negative zeck")
        ((inexact? n) "no floating zeck")
        ((zero? n) "0")
        (else (zeck->string
                (for/list ((s (reverse (take fibonacci (iFib n)))))
                (if ( > s n) 0
                    (begin (-= n s) 1 )))))))

```


```txt

(take Zeck 21)
    → (0 1 10 100 101 1000 1001 1010 10000 10001
     10010 10100 10101 100000 100001 100010 100100 100101 101000 101001 101010)

(Zeck 1000000000)
    → 1010000100100001010101000001000101000101001

```


## Elena

ELENA 4.x :

```elena
import system'routines;
import system'collections;
import system'text;
import extensions;
extension op
{
    fibonacci()
    {
        if (self < 2)
        {
            ^ self
        }
        else
        {
            ^ (self - 1).fibonacci() + (self - 2).fibonacci()
        };
    }

    zeckendorf()
    {
        var fibonacciNumbers := new List<int>();

        int num := self;
        int fibPosition := 2;
        int currentFibonaciNum := fibPosition.fibonacci();

        while (currentFibonaciNum <= num)
        {
            fibonacciNumbers.append:currentFibonaciNum;

            fibPosition := fibPosition + 1;
            currentFibonaciNum := fibPosition.fibonacci()
        };

        auto output := new TextBuilder();
        int temp := num;

        fibonacciNumbers.sequenceReverse().forEach:(item)
        {
            if (item <= temp)
            {
                output.write("1");
                temp := temp - item
            }
            else
            {
                output.write("0")
            }
        };

        ^ output.Value
    }
}

public program()
{
    for(int i := 1, i <= 20, i += 1)
    {
        console.printFormatted("{0} : {1}",i,i.zeckendorf()).writeLine()
    };

    console.readChar()
}
```

```txt
1 : 1
2 : 10
3 : 100
4 : 101
5 : 1000
6 : 1001
7 : 1010
8 : 10000
9 : 10001
10 : 10010
11 : 10100
12 : 10101
13 : 100000
14 : 100001
15 : 100010
16 : 100100
17 : 100101
18 : 101000
19 : 101001
20 : 101010
```



## Elixir

Stream generator:

```elixir
defmodule Zeckendorf do
  def number do
    Stream.unfold(0, fn n -> zn_loop(n) end)
  end

  defp zn_loop(n) do
    bin = Integer.to_string(n, 2)
    if String.match?(bin, ~r/11/), do: zn_loop(n+1), else: {bin, n+1}
  end
end

Zeckendorf.number |> Enum.take(21) |> Enum.with_index
|> Enum.each(fn {zn, i} -> IO.puts "#{i}: #{zn}" end)
```


```txt

0: 0
1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```


Fibonacci numbers:

```elixir
defmodule Zeckendorf do
  def number(n) do
    fib_loop(n, [2,1])
    |> Enum.reduce({"",n}, fn f,{dig,i} ->
         if f <= i, do: {dig<>"1", i-f}, else: {dig<>"0", i}
       end)
    |> elem(0) |> String.to_integer
  end

  defp fib_loop(n, fib) when n < hd(fib), do: fib
  defp fib_loop(n, [a,b|_]=fib), do: fib_loop(n, [a+b | fib])
end

for i <- 0..20, do: IO.puts "#{i}: #{Zeckendorf.number(i)}"
```

same output

=={{header|F_Sharp|F#}}==

```fsharp
let fib = Seq.unfold (fun (x, y) -> Some(x, (y, x + y))) (1,2)

let zeckendorf n =
    if n = 0 then ["0"]
    else
        let folder k state =
            let (n, z) = (fst state), (snd state)
            if n >= k then (n - k, "1" :: z)
            else (n, "0" :: z)
        let fb = fib |> Seq.takeWhile (fun i -> i<=n) |> Seq.toList
        snd (List.foldBack folder fb (n, []))
        |> List.rev

for i in 0 .. 20 do printfn "%2d: %8s" i (String.concat "" (zeckendorf i))
```

<pre style="height:5em"> 0:        0
 1:        1
 2:       10
 3:      100
 4:      101
 5:     1000
 6:     1001
 7:     1010
 8:    10000
 9:    10001
10:    10010
11:    10100
12:    10101
13:   100000
14:   100001
15:   100010
16:   100100
17:   100101
18:   101000
19:   101001
20:   101010
```



## Factor


```factor
USING: formatting kernel locals make math sequences ;

:: fib<= ( n -- seq )
    1 2 [ [ dup n <= ] [ 2dup + [ , ] 2dip ] while drop , ]
    { } make ;

:: zeck ( n -- str )
    0 :> s! n fib<= <reversed>
    [ dup s + n <= [ s + s! 49 ] [ drop 48 ] if ] "" map-as ;

21 <iota> [ dup zeck "%2d: %6s\n" printf ] each
```

```txt

 0:      0
 1:      1
 2:     10
 3:    100
 4:    101
 5:   1000
 6:   1001
 7:   1010
 8:  10000
 9:  10001
10:  10010
11:  10100
12:  10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## Forth


```forth
: fib<= ( n -- n )
    >r 0 1 BEGIN dup r@ <= WHILE  tuck +  REPEAT  drop rdrop ;

: z. ( n -- )
   dup fib<= dup . -
   BEGIN ?dup WHILE
      dup fib<= dup [char] + emit space . -
   REPEAT ;

: tab  9 emit ;

: zeckendorf ( -- )
    21 0 DO
        cr i 2 .r tab i z.
    LOOP ;
```

```txt

zeckendorf
 0	0
 1	1
 2	2
 3	3
 4	3 + 1
 5	5
 6	5 + 1
 7	5 + 2
 8	8
 9	8 + 1
10	8 + 2
11	8 + 3
12	8 + 3 + 1
13	13
14	13 + 1
15	13 + 2
16	13 + 3
17	13 + 3 + 1
18	13 + 5
19	13 + 5 + 1
20	13 + 5 + 2  ok

```



## Fortran

The simplest representation of a number in the Zeckendorf manner is as a sequence of digits, such as are used in multi-precision arithmetic, and for this an array of integers will do. Rather excessively, as only two states are required and the default integer style is usually thirty-two bits these days. Some compilers allow the specification of one-byte integers, as in <code>INTEGER*1 D(0:ZLAST)</code> so that would be only an eight-fold excess. One could escalate to fiddling with individual bits within a number (as is done in [[Extensible_prime_generator#Fortran]]) and a 16-bit integer would be adequate for the specified tests, but Fortran syntax has not been extended to offer simple methods for manipulating bits such as <code>D(7:7)</code> to obtain the seventh bit of D. Instead one might use special library routines as supplied by F90 such as <code>IBITS(D,7,1)</code> for the same effect, though possibly at a cost in code size and execution time. Less storage may be saved through cramming bits than is consumed by the code needed to extract individual bits. Such values could then be displayed using the<code>B</code> format code. However, the source code would now be littered with the details of bit access rather than the form of the Zeckendorf procedure.

An alternative lies in noting that only the sequences 00, 01, and 10 can appear (because 11 is unnecessary; see below), so a base three scheme could be used to represent the three such pairs of bits. But this still contains redundancies. Suppose a 01 value is somewhere in the sequence: then it may be followed only by 00 or 01, and likewise 10 be preceded only by 10 or 00; just two values, not three. Perhaps a still more cunning compaction scheme could be devised to take advantage of these details, or some other scheme concocted. For simplicity, no compaction will be attempted so the states of 0 and 1 will be represented by a simple integer devoted to that bit.

The conversion scheme requires the values of the Fibonacci sequence, except not quite: the Fibonacci sequence starts F<sub>0</sub> = 0, F<sub>1</sub> = 1, F<sub>2</sub> = 1, F<sub>3</sub> = 2, F<sub>4</sub> = 3, ''etc.'' but what is wanted is to start with the second 1, so F<sub>1</sub> = 1, F<sub>2</sub> = 2, F<sub>3</sub> = 3, F<sub>4</sub> = 5, ''etc.'' so this sequence has been named the Fib1nacci sequence to replace conceptual dissonance with lexical dissonance, and similarly with array <code>F1B</code> instead of <code>FIB</code>. Initial investigations show that F<sub>45</sub> is the last before a thirty-two bit two's complement integer is overflowed, though systems offering INTEGER*8 could be pushed further.

Initialising this table in array <code>F1B</code> could be done via specifying the relevant values (computed separately, even by hand) or by some banal initialisation loop that would be executed on the first invocation of any of the routines requiring those values, a tedious and annoying rigmarole to organise. More interesting are the facilities offered by the PARAMETER statement (introduced with F77), with the further possibility that constants, so defined, would be held safe from accidental change, nor would there be initialising code to execute at run time. Alas, the obvious approach (commented out in the source) using array <code>F1B</code> is rejected by the F90 compiler even though it ''does'' allow a value to be defined in terms of other defined values, as is demonstrated by the horde of simple variables following. Despite being a multi-pass compiler, the dependencies will ''not'' be unravelled if the statements appear out of order. Fortran does not include a standard pre-processor stage, unlike say pl/i where it is built-in to the language and uses much the same syntax as normal pl/i statements, so loops, IF-tests and so forth are available. By such means, the upper limit of 45 could be determined, the initial values calculated, and the array be defined with initial values, all at compile time.

By declaring the horde of simple names to have the PRIVATE attribute, they will not litter the name space of routines invoking the module, but alas, they will still occupy their own storage space. Another possibility would be to use the EQUIVALENCE statement to have them placed within array <code>F1B</code>, but alas, as noted in [[15_Puzzle_Game#Fortran]], the compiler will not countenance PARAMETER statements for names engaged in such misbehviour. A pity.

Still another possibility would be to take advantage of the formula for calculating the values of the Fibonacci series directly (with careful attention to the offsets needed for the Fib1nacci sequence), but this formula is rather intimidating:
```Fortran
F(N) = ((1 + SQRT(5))**N - (1 - SQRT(5))**N)/(SQRT(5)*2**N)
```
 It can easily be coded as a Fortran function (and would have to be double precision because 32-bit floating-point arithmetic is not accurate enough for integer constants approaching 32 bits), but alas, the compiler does not allow itself to take the risk of invoking a user-written function in a PARAMETER statement, even if the compiler had itself compiled it. For, [https://en.wikipedia.org/wiki/Entscheidungsproblem who knows] what it might do?


Given the array <code>F1B</code> the conversion from an integer to Zeckendorf digit sequence starts from the high-order end to find the highest <code>F1B</code> value not exceeding the number. There is a formula for this mentioned in the [[Zeckendorf_number_representation#EchoLisp|EchoLisp]] section, but it too is intimidating and the rounding of its result would also require checking. Rather than a linear search, a binary chop could be used, though at the cost of additional code. The location of the high-order digit is recorded on general principles, it being useful in formatting output for example. This requires a "first-time" test within the loop, that could be avoided if the conversion were to be done in two stages.

The special feature of the conversion lies in noting that F1B(n + 1) = F1B(n) + F1B(n - 1), the defining feature of the Fibonacci sequence. Thus, when a 1-bit is found (say it is bit ''n''), the next bit down must be a 0 and so the test for it may be skipped, by incrementing <code>L</code> This is because if it were not 0, then the bit above (bit ''n + 1'') would have been turned on in the previous stage instead. Because of this adjustment, the controlling loop cannot be <code>DO L = ZLAST,1,-1</code> to step down the entries in array <code>F1B</code> because modifications to the index variable of a DO-loop, if not rejected out-of-hand by the compiler, may have no effect on the execution of the loop. This is because the execution of a DO-loop is often controlled by an [https://en.wikipedia.org/wiki/For_loop#Loop_variable_scope_and_semantics "iteration count"], calculated on entry to the DO-loop, which is thereby unaffected by changes to the index variable, or indeed to the bounds and step size of the loop. Other implementations of a DO-loop will offer other behaviour. There being no equivalent in Fortran of ''Repeat ... until ...  ;'' (whereby the test is at the end, and there is no initial test), a <code>GO TO</code> appears...

The source uses F90 for its MODULE facility, in particular having array <code>F1B</code> available without having to mess about with additional parameters or COMMON statements. This also enables the specification of arrays with a lower bound other than one, which makes it easy to define the digit arrays to have a current length, stored in element zero. This sort of "string" facility is often restricted only to strings of characters, but the notion "string of <type>" is often useful. If in routines declared within a MODULE the size of an array parameter is declared via <code>:</code> there are secret additional parameters defining its size, accessible via special functions such as <code>UBOUND</code> so there is no need for an explicit parameter doing so as would be the case prior to F90. With F90 it is also possible to define a compound data type for the digit sequence, but a simple array seems more flexible.

The pleasing name, "MODULE ZECKENDORF ARITHMETIC" causes some odd behaviour, even though Fortran source normally involves spaces having no significance outside text literals.
```Fortran
      MODULE ZECKENDORF ARITHMETIC	!Using the Fibonacci series, rather than powers of some base.
       INTEGER ZLAST		!The standard 32-bit two's complement integers
       PARAMETER (ZLAST = 45)	!only get so far, just as there's a limit to the highest power.
       INTEGER F1B(ZLAST)	!I want the Fibonacci series, and, starting with its second one.
c       PARAMETER (F1B = (/1,2,	!But alas, the compiler doesn't allow
c     3  F1B(1) + F1B(2),		!for this sort of carpet-unrolling
c     4  F1B(2) + F1B(3), 		!initialisation sequence.
       INTEGER,PRIVATE:: F01,F02,F03,F04,F05,F06,F07,F08,F09,F10,	!So, not bothering with F00,
     1  F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,	!Prepare a horde of simple names,
     2  F21,F22,F23,F24,F25,F26,F27,F28,F29,F30,	!which can be initialised
     3  F31,F32,F33,F34,F35,F36,F37,F38,F39,F40,	!in a certain way,
     4  F41,F42,F43,F44,F45				!without scaring the compiler.
       PARAMETER (F01 = 1, F02 = 2, F03 = F02 + F01, F04 = F03 + F02,	!Thusly.
     1  F05=F04+F03,F06=F05+F04,F07=F06+F05,F08=F07+F06,F09=F08+F07,	!Typing all this
     2  F10=F09+F08,F11=F10+F09,F12=F11+F10,F13=F12+F11,F14=F13+F12,	!is an invitation
     3  F15=F14+F13,F16=F15+F14,F17=F16+F15,F18=F17+F16,F19=F18+F17,	!for mistypes.
     4  F20=F19+F18,F21=F20+F19,F22=F21+F20,F23=F22+F21,F24=F23+F22,	!So a regular layout
     5  F25=F24+F23,F26=F25+F24,F27=F26+F25,F28=F27+F26,F29=F28+F27,	!helps a little.
     6  F30=F29+F28,F31=F30+F29,F32=F31+F30,F33=F32+F31,F34=F33+F32,	!Otherwise,
     7  F35=F34+F33,F36=F35+F34,F37=F36+F35,F38=F37+F36,F39=F38+F37,	!devise a prog.
     8  F40=F39+F38,F41=F40+F39,F42=F41+F40,F43=F42+F41,F44=F43+F42,	!to generate these texts...
     9  F45=F44+F43)	!The next is 2971215073. Too big for 32-bit two's complement integers.
       PARAMETER (F1B = (/F01,F02,F03,F04,F05,F06,F07,F08,F09,F10,	!And now,
     1  F11, F12, F13, F14, F15, F16, F17, F18, F19, F20,		!Here is the desired
     2  F21, F22, F23, F24, F25, F26, F27, F28, F29, F30,		!array of constants.
     3  F31, F32, F33, F34, F35, F36, F37, F38, F39, F40,		!And as such, possibly
     4  F41, F42, F43, F44, F45/))					!protected from alteration.
       CONTAINS	!After all that, here we go.
        SUBROUTINE ZECK(N,D)	!Convert N to a "Zeckendorf" digit sequence.
Counts upwards from digit one. D(i) ~ F1B(i). D(0) fingers the high-order digit.
         INTEGER N	!The normal number, in the computer's base.
         INTEGER D(0:)	!The digits, to be determined.
         INTEGER R	!The remnant.
         INTEGER L	!A finger, similar to the power of the base.
          IF (N.LT.0) STOP "ZECK! No negative numbers!"	!I'm not thinking about them.
          R = N		!Grab a copy that I can mess with.
          D = 0		!Scrub the lot in one go.
          L = ZLAST	!As if starting with BASE**MAX, rather than BASE**0.
   10     IF (R.GE.F1B(L)) THEN	!Has the remnant sufficient for this digit?
            R = R - F1B(L)		!Yes! Remove that amount.
            IF (D(0).EQ.0) THEN		!Is this the first non-zero digit?
              IF (L.GT.UBOUND(D,DIM=1)) STOP "ZECK! Not enough digits!"	!Yes.
              D(0) = L			!Remember the location of the high-order digit.
            END IF			!Two loops instead, to avoid repeated testing?
            D(L) = 1			!Place the digit, knowing a place awaits.
            L = L - 1			!Never need a ...11... sequence because F1B(i) + F1B(i+1) = F1B(i+2).
          END IF		!So much for that digit "power".
          L = L - 1		!Down a digit.
          IF (L.GT.0 .AND. R.GT.0) GO TO 10	!Are we there yet?
          IF (N.EQ.0) D(0) = 1	!Zero has one digit.
        END SUBROUTINE ZECK	!That was fun.

        INTEGER FUNCTION ZECKN(D)	!Converts a "Zeckendorf" digit sequence to a number.
         INTEGER D(0:)	!The digits. D(0) fingers the high-order digit.
          IF (D(0).LE.0) STOP "ZECKN! Empty number!"	!General paranoia.
          IF (D(0).GT.ZLAST) STOP "ZECKN! Oversize number!"	!I hate array bound hiccoughs.
          ZECKN = SUM(D(1:D(0))*F1B(1:D(0)))	!This is what positional notation means.
          IF (ZECKN.LT.0) STOP "ZECKN! Integer overflow!"	!Oh for IF OVERFLOW as in First Fortran.
        END FUNCTION ZECKN	!Overflows by a small amount will produce a negative number.
      END MODULE ZECKENDORF ARITHMETIC	!Odd stuff.

      PROGRAM POKE
      USE ZECKENDORF ARITHMETIC	!Please.
      INTEGER ZD(0:ZLAST)	!A scratchpad.
      INTEGER I,J,W
      CHARACTER*1 DIGIT(0:1)	!Assistance for the output.
      PARAMETER (DIGIT = (/"0","1"/), W = 6)	!This field width suffices.
c      WRITE (6,*) F1B
c      WRITE (6,*) INT8(F1B(44)) + INT8(F1B(45))
      WRITE (6,1) F1B(1:4),ZLAST,ZLAST,F1B(ZLAST),HUGE(I)	!Show some provenance.
    1 FORMAT ("Converts integers to their Zeckendorf digit string "
     1 "using the Fib1nacci sequence (",4(I0,","),
     2 " ...) as the equivalent of powers."/
     3 "At most, ",I0," digits because Fib1nacci(",I0,") = ",I0,
     4 " and the integer limit is ",I0,".",//,"  N     ZN")	!Ends with a heading.

      DO I = 0,20	!Step through the specified range.
        CALL ZECK(I,ZD)		!Convert I to ZD.
c       WRITE (6,2) I,ZD(ZD(0):1:-1)	!Show digits from high-order to low.
c   2   FORMAT (I3,1X,66I1)		!Or, WRITE (6,2) I,(ZD(J), J = ZD(0),1,-1)
        WRITE (6,3) I,(" ",J = ZD(0) + 1,W),DIGIT(ZD(ZD(0):1:-1))	!Right-aligned in field width W.
    3   FORMAT (I3,1X,66A1)		!The digits appear as characters.
        IF (I.NE.ZECKN(ZD)) STOP "Huh?"	!Should never happen...
      END DO		!On to the next.

      END
```


Output: shown aligned right for a more regular table. Producing leading spaces or digits required a conversion from a numerical digit to a character digit, so that all the output could use the <code>A</code> format code.

```txt

Converts integers to their Zeckendorf digit string using the Fib1nacci sequence (1,2,3,5, ...) as the equivalent of powers.
At most, 45 digits because Fib1nacci(45) = 1836311903 and the integer limit is 2147483647.

  N     ZN
  0      0
  1      1
  2     10
  3    100
  4    101
  5   1000
  6   1001
  7   1010
  8  10000
  9  10001
 10  10010
 11  10100
 12  10101
 13 100000
 14 100001
 15 100010
 16 100100
 17 100101
 18 101000
 19 101001
 20 101010

```



## FreeBASIC


```freebasic
' version 17-10-2016
' compile with: fbc -s console

#Define max 92 ' max for Fibonacci number

Dim Shared As ULongInt fib(max)

fib(0) = 1
fib(1) = 1

For x As Integer = 2 To max
  fib(x) = fib(x-1) + fib(x-2)
Next

Function num2zeck(n As Integer) As String

If n < 0 Then
  Print "Error: no negative numbers allowed"
  Beep : Sleep 5000,1 : End
End If

If n < 2 Then Return Str(n)

  Dim As String zeckendorf

  For x As Integer = max To 1 Step -1
    If fib(x) <= n Then
      zeckendorf = zeckendorf + "1"
      n = n - fib(x)
    Else
      zeckendorf = zeckendorf + "0"
    End If
  Next

  return LTrim(zeckendorf, "0") ' get rid of leading zeroes
End Function

' ------=< MAIN >=------

Dim As Integer x, e
Dim As String zeckendorf
Print "number       zeckendorf"

For x = 0 To 200000

  zeckendorf = num2zeck(x)
  If x <= 20 Then Print x, zeckendorf

  ' check for two consecutive Fibonacci numbers
  If InStr(zeckendorf, "11") <> 0 Then
    Print " Error: two consecutive Fibonacci numbers "; x, zeckendorf
    e = e +1
  End If
Next

Print
If e = 0 Then
  Print " No Zeckendorf numbers with two consecutive Fibonacci numbers found"
Else
  Print e; " error(s) found"
End If


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
number       zeckendorf
 0            0
 1            1
 2            10
 3            100
 4            101
 5            1000
 6            1001
 7            1010
 8            10000
 9            10001
 10           10010
 11           10100
 12           10101
 13           100000
 14           100001
 15           100010
 16           100100
 17           100101
 18           101000
 19           101001
 20           101010

 No Zeckendorf numbers with two consecutive Fibonacci numbers found
```



## Go


```go
package main

import "fmt"

func main() {
    for i := 0; i <= 20; i++ {
        fmt.Printf("%2d %7b\n", i, zeckendorf(i))
    }
}

func zeckendorf(n int) int {
    // initial arguments of fib0 = 1 and fib1 = 1 will produce
    // the Fibonacci sequence {1, 2, 3,..} on the stack as successive
    // values of fib1.
    _, set := zr(1, 1, n, 0)
    return set
}

func zr(fib0, fib1, n int, bit uint) (remaining, set int) {
    if fib1 > n {
        return n, 0
    }
    // recurse.
    // construct sequence on the way in, construct ZR on the way out.
    remaining, set = zr(fib1, fib0+fib1, n, bit+1)
    if fib1 <= remaining {
        set |= 1 << bit
        remaining -= fib1
    }
    return
}
```

```txt
 0       0
 1       1
 2      10
 3     100
 4     101
 5    1000
 6    1001
 7    1010
 8   10000
 9   10001
10   10010
11   10100
12   10101
13  100000
14  100001
15  100010
16  100100
17  100101
18  101000
19  101001
20  101010
```



## Haskell

Using "no consecutive 1s" rule:

```haskell
import Data.Bits
import Numeric

zeckendorf = map b $ filter ones [0..] where
	ones :: Int -> Bool
	ones x = 0 == x .&. (x `shiftR` 1)
	b x = showIntAtBase 2 ("01"!!) x ""

main = mapM_ putStrLn $ take 21 zeckendorf
```

which is the same as

```haskell
zeckendorf = "0":"1":[s++[d] |	s <- tail zeckendorf, d <- "01",
				last s /= '1' || d /= '1']

main = mapM putStrLn $ take 21 zeckendorf
```

or a different way to generate the sequence:

```haskell
import Numeric

fib = 1 : 1 : zipWith (+) fib (tail fib)
pow2 = iterate (2*) 1

zeckendorf = map b z where
	z = 0:concat (zipWith f fib pow2)
	f x y = map (y+) (take x z)
	b x = showIntAtBase 2 ("01"!!) x ""

main = mapM_ putStrLn $ take 21 zeckendorf
```


Creating a string for an individual number:

```haskell
import Data.List (mapAccumL)

fib :: [Int]
fib = 1 : 2 : zipWith (+) fib (tail fib)

zeckendorf :: Int -> String
zeckendorf 0 = "0"
zeckendorf n = snd $ mapAccumL f n $ reverse $ takeWhile (<= n) fib
  where
    f n x
      | n < x = (n, '0')
      | otherwise = (n - x, '1')

main :: IO ()
main = (putStrLn . unlines) $ zeckendorf <$> [0 .. 20]
```

```txt

0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010

```



## J

Please enjoy our [http://www.jsoftware.com/jwiki/Essays/Fibonacci%20Sums Zeckendorf essay].

```J

fib=: 3 : 0 " 0
 mp=. +/ .*
 {.{: mp/ mp~^:(I.|.#:y) 2 2$0 1 1 1x
)

phi=: -:1+%:5

fi =: 3 : 'n - y<fib n=. 0>.(1=y)-~>.(phi^.%:5)+phi^.y'

fsum=: 3 : 0
 z=. 0$r=. y
 while. 3<r do.
  m=. fib fi r
  z=. z,m
  r=. r-m
 end.
 z,r$~(*r)+.0=y
)

Filter=: (#~`)(`:6)

' '&~:Filter@:":@:#:@:#.@:((|. fib 2+i.8) e. fsum)&.>i.3 7
┌──────┬──────┬──────┬──────┬──────┬──────┬──────┐
│0     │1     │10    │100   │101   │1000  │1001  │
├──────┼──────┼──────┼──────┼──────┼──────┼──────┤
│1010  │10000 │10001 │10010 │10100 │10101 │100000│
├──────┼──────┼──────┼──────┼──────┼──────┼──────┤
│100001│100010│100100│100101│101000│101001│101010│
└──────┴──────┴──────┴──────┴──────┴──────┴──────┘

```


Explanation:

<code>fsum</code> finds the canonical list of fibonacci terms which sum to its argument.

<code>fib</code> finds the nth fibonacci term of the fibonacci sequence. This would be 0 1 1 2 3 5 8 13 21 34 55 89 ... but we ignore the first two values of that sequence for the purpose of this exercise.

<code>(|. fib 2+i.8)</code> is <code>34 21 13 8 5 3 2 1</code>. You can think of an eight bit Zeckendorf number such as <code>101010</code> as representing the inner product of its digits with <code>(|. fib 2+i.8)</code>. Thus, we can find the relevant Zeckendorf bits by finding which which members of that sequence are in the result of <code>fsum</code>

The rest is just formatting. (We convert from binary list to integer and then back to binary list, to eliminate leading zeros from the list. Then we convert to text and remove all the spaces. Since we arranged for each result to be in a box, the boxes will align giving us a somewhat readable presentation.


## Java


'''Code:'''


```java
import java.util.*;

class Zeckendorf
{
  public static String getZeckendorf(int n)
  {
    if (n == 0)
      return "0";
    List<Integer> fibNumbers = new ArrayList<Integer>();
    fibNumbers.add(1);
    int nextFib = 2;
    while (nextFib <= n)
    {
      fibNumbers.add(nextFib);
      nextFib += fibNumbers.get(fibNumbers.size() - 2);
    }
    StringBuilder sb = new StringBuilder();
    for (int i = fibNumbers.size() - 1; i >= 0; i--)
    {
      int fibNumber = fibNumbers.get(i);
      sb.append((fibNumber <= n) ? "1" : "0");
      if (fibNumber <= n)
        n -= fibNumber;
    }
    return sb.toString();
  }

  public static void main(String[] args)
  {
    for (int i = 0; i <= 20; i++)
      System.out.println("Z(" + i + ")=" + getZeckendorf(i));
  }
}
```


'''Output:'''


```txt
Z(0)=0
Z(1)=1
Z(2)=10
Z(3)=100
Z(4)=101
Z(5)=1000
Z(6)=1001
Z(7)=1010
Z(8)=10000
Z(9)=10001
Z(10)=10010
Z(11)=10100
Z(12)=10101
Z(13)=100000
Z(14)=100001
Z(15)=100010
Z(16)=100100
Z(17)=100101
Z(18)=101000
Z(19)=101001
Z(20)=101010
```



### Recursive Implementation


'''Code:'''

```java
import java.util.ArrayList;
import java.util.List;

public class Zeckendorf {

    private List<Integer> getFibList(final int maxNum, final int n1, final int n2, final List<Integer> fibs){
        if(n2 > maxNum) return fibs;

        fibs.add(n2);

        return getFibList(maxNum, n2, n1 + n2, fibs);
    }

    public String getZeckendorf(final int num) {
        if (num <= 0) return "0";

        final List<Integer> fibs = getFibList(num, 1, 2, new ArrayList<Integer>(){{ add(1); }});

        return getZeckString("", num, fibs.size() - 1, fibs);
    }

    private String getZeckString(final String zeck, final int num, final int index, final List<Integer> fibs){
        final int curFib = fibs.get(index);
        final boolean placeZeck = num >= curFib;

        final String outString = placeZeck ? zeck + "1" : zeck + "0";
        final int outNum = placeZeck ? num - curFib : num;

        if(index == 0) return outString;

        return  getZeckString(outString, outNum, index - 1, fibs);
    }

    public static void main(final String[] args) {
        final Zeckendorf zeckendorf = new Zeckendorf();

        for(int i =0; i <= 20; i++){
            System.out.println("Z("+ i +"):\t" + zeckendorf.getZeckendorf(i));
        }
    }
}
```


'''Output:'''


```txt
Z(0):	0
Z(1):	1
Z(2):	10
Z(3):	100
Z(4):	101
Z(5):	1000
Z(6):	1001
Z(7):	1010
Z(8):	10000
Z(9):	10001
Z(10):	10010
Z(11):	10100
Z(12):	10101
Z(13):	100000
Z(14):	100001
Z(15):	100010
Z(16):	100100
Z(17):	100101
Z(18):	101000
Z(19):	101001
Z(20):	101010
```




## JavaScript


### ES6

{{Trans|Haskell}} ('''mapAccumuL''' example)

```JavaScript
(() => {
    'use strict';

    const main = () =>
        unlines(
            map(n => concat(zeckendorf(n)),
                enumFromTo(0, 20)
            )
        );

    // zeckendorf :: Int -> String
    const zeckendorf = n => {
        const go = (n, x) =>
            n < x ? (
                Tuple(n, '0')
            ) : Tuple(n - x, '1')
        return 0 < n ? (
            snd(mapAccumL(
                go, n,
                reverse(fibUntil(n))
            ))
        ) : ['0'];
    };

    // fibUntil :: Int -> [Int]
    const fibUntil = n =>
        cons(1, takeWhile(x => n >= x,
            map(snd, iterateUntil(
                tpl => n <= fst(tpl),
                tpl => {
                    const x = snd(tpl);
                    return Tuple(x, x + fst(tpl));
                },
                Tuple(1, 2)
            ))));

    // GENERIC FUNCTIONS ----------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : (x + xs);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // 'The mapAccumL function behaves like a combination of map and foldl;
    // it applies a function to each element of a list, passing an accumulating
    // parameter from left to right, and returning a final value of this
    // accumulator together with the new list.' (See Hoogle)

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x, i) => {
            const pair = f(a[0], x, i);
            return Tuple(pair[0], a[1].concat(pair[1]));
        }, Tuple(acc, []));

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // takeWhile :: (a -> Bool) -> [a] -> [a]
    // takeWhile :: (Char -> Bool) -> String -> String
    const takeWhile = (p, xs) => {
        const lng = xs.length;
        return 0 < lng ? xs.slice(
            0,
            until(
                i => i === lng || !p(xs[i]),
                i => 1 + i,
                0
            )
        ) : [];
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

```txt
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010
```



## jq

```jq
def zeckendorf:
  # rfibs(n) returns an array of fibonnaci numbers up to n,
  # beginning with 1, 2, ..., in reverse order
  def rfibs(n):
    # input: [f(i-2), f(i-1)]
    [1,1] | [recurse( if .[1] >= n then empty
                      else [.[1], add]
                      end ) | .[1]] | reverse;

  . as $n
  # [n, rfibs, digit ]
  | [$n, rfibs($n), "" ]
  | [ recurse( .[0] as $n | .[1] as $f
               | if ($f|length) == 0 then empty
                 else
                   $f[0] as $next
                 | if $n >= $next then [ ( $n - $next), $f[1:], "1"]
		   else [ $n, $f[1:], "0"]
                   end
                 end )
      | .[2] ]
  | if .[1] == "0" then .[2:] else . end  # remove leading 0 if any
  | join("") ;
```


'''Example:'''

```jq
range(0;21) | "\(.): \(zeckendorf)"
```

```sh
$ jq -n -r -f zeckendorf.jq
0:
1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010
```



## Julia

```julia
function zeck(n)
    n <= 0 && return 0
    fib = [2,1]; while fib[1] < n unshift!(fib,sum(fib[1:2])) end
    dig = Int[]; for f in fib f <= n ? (push!(dig,1); n = n-f;) : push!(dig,0) end
    return dig[1] == 0 ? dig[2:end] : dig
end
```

```txt

julia> for x = 0:20
           println(join(zeck(x)))
       end
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010

```



## Kotlin


```scala
// version 1.0.6

const val LIMIT = 46  // to stay within range of signed 32 bit integer
val fibs = fibonacci(LIMIT)

fun fibonacci(n: Int): IntArray {
    if (n !in 2..LIMIT) throw IllegalArgumentException("n must be between 2 and $LIMIT")
    val fibs = IntArray(n)
    fibs[0] = 1
    fibs[1] = 1
    for (i in 2 until n) fibs[i] = fibs[i - 1] + fibs[i - 2]
    return fibs
}

fun zeckendorf(n: Int): String {
    if (n < 0) throw IllegalArgumentException("n must be non-negative")
    if (n < 2) return n.toString()
    var lastFibIndex = 1
    for (i in 2..LIMIT)
        if (fibs[i] > n) {
            lastFibIndex = i - 1
            break
        }
    var nn = n - fibs[lastFibIndex--]
    val zr = StringBuilder("1")
    for (i in lastFibIndex downTo 1)
        if (fibs[i] <= nn) {
            zr.append('1')
            nn -= fibs[i]
        } else {
            zr.append('0')
        }
    return zr.toString()
}

fun main(args: Array<String>) {
    println(" n   z")
    for (i in 0..20) println("${"%2d".format(i)} : ${zeckendorf(i)}")
}
```


```txt

 n   z
 0 : 0
 1 : 1
 2 : 10
 3 : 100
 4 : 101
 5 : 1000
 6 : 1001
 7 : 1010
 8 : 10000
 9 : 10001
10 : 10010
11 : 10100
12 : 10101
13 : 100000
14 : 100001
15 : 100010
16 : 100100
17 : 100101
18 : 101000
19 : 101001
20 : 101010

```



## Lingo

```Lingo
-- Return the distinct Fibonacci numbers not greater than 'n'
on fibsUpTo (n)
    fibList = []
    last = 1
    current = 1
    repeat while current <= n
        fibList.add(current)
        nxt = last + current
        last = current
        current = nxt
    end repeat
    return fibList
end

-- Return the Zeckendorf representation of 'n'
on zeckendorf (n)
    fib = fibsUpTo(n)
    zeck = ""
    repeat with pos = fib.count down to 1
        if n >= fib[pos] then
            zeck = zeck & "1"
            n = n - fib[pos]
        else
            zeck = zeck & "0"
        end if
    end repeat
    if zeck = "" then return "0"
    return zeck
end
```



```Lingo
repeat with n = 0 to 20
    put n & ": " & zeckendorf(n)
end repeat
```


```txt

0: 0
1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## Logo


```logo
; return the (N+1)th Fibonacci number (1,2,3,5,8,13,...)
to fib m
  local "n
  make "n sum :m 1
  if [lessequal? :n 0] [output difference fib sum :n 2 fib sum :n 1]
  global "_fib
  if [not name? "_fib] [
    make "_fib [1 1]
  ]
  local "length
  make "length count :_fib
  while [greater? :n :length] [
    make "_fib (lput (sum (last :_fib) (last (butlast :_fib))) :_fib)
    make "length sum :length 1
  ]
  output item :n :_fib
end

; return the binary Zeckendorf representation of a nonnegative number
to zeckendorf n
  if [less? :n 0] [(throw "error [Number must be nonnegative.])]
  (local "i "f "result)
  make "i :n
  make "f fib :i
  while [less? :f :n] [make "i sum :i 1 make "f fib :i]

  make "result "||
  while [greater? :i 0] [
    ifelse [greaterequal? :n :f] [
      make "result lput 1 :result
      make "n difference :n :f
    ] [
      if [not empty? :result] [
        make "result lput 0 :result
      ]
    ]
    make "i difference :i 1
    make "f fib :i
  ]
  if [equal? :result "||] [
    make "result 0
  ]
  output :result
end

type zeckendorf 0
repeat 20 [
  type word "| | zeckendorf repcount
]
print []
bye
```

```txt
0 1 10 100 101 1000 1001 1010 10000 10001 10010 10100 10101 100000 100001 100010 100100 100101 101000 101001 101010
```



## Lua


```Lua
-- Return the distinct Fibonacci numbers not greater than 'n'
function fibsUpTo (n)
    local fibList, last, current, nxt = {}, 1, 1
    while current <= n do
        table.insert(fibList, current)
        nxt = last + current
        last = current
        current = nxt
    end
    return fibList
end

-- Return the Zeckendorf representation of 'n'
function zeckendorf (n)
    local fib, zeck = fibsUpTo(n), ""
    for pos = #fib, 1, -1 do
        if n >= fib[pos] then
            zeck = zeck .. "1"
            n = n - fib[pos]
        else
            zeck = zeck .. "0"
        end
    end
    if zeck == "" then return "0" end
    return zeck
end

-- Main procedure
print(" n\t| Zeckendorf(n)")
print(string.rep("-", 23))
for n = 0, 20 do
    print(" " .. n, "| " .. zeckendorf(n))
end
```

```txt
 n      | Zeckendorf(n)
-----------------------
 0      | 0
 1      | 1
 2      | 10
 3      | 100
 4      | 101
 5      | 1000
 6      | 1001
 7      | 1010
 8      | 10000
 9      | 10001
 10     | 10010
 11     | 10100
 12     | 10101
 13     | 100000
 14     | 100001
 15     | 100010
 16     | 100100
 17     | 100101
 18     | 101000
 19     | 101001
 20     | 101010
```



## Mathematica


```Mathematica
zeckendorf[0] = 0;
zeckendorf[n_Integer] :=
  10^(# - 1) + zeckendorf[n - Fibonacci[# + 1]] &@
   LengthWhile[
    Fibonacci /@
     Range[2, Ceiling@Log[GoldenRatio, n Sqrt@5]], # <= n &];
zeckendorf /@ Range[0, 20]
```


```txt
{0, 1, 10, 100, 101, 1000, 1001, 1010, 10000, 10001, 10010, 10100,
10101, 100000, 100001, 100010, 100100, 100101, 101000, 101001, 101010}
```



## Nim

```nim
import strutils

proc z(n): string =
  if n == 0: return "0"
  var fib = @[2,1]
  var n = n
  while fib[0] < n: fib.insert(fib[0] + fib[1])
  result = ""
  for f in fib:
    if f <= n:
      result.add '1'
      n -= f
    else:
      result.add '0'
  if result[0] == '0':
    result = result[1..result.high]

for i in 0 .. 20:
  echo align($i, 3)," ",align(z(i), 8)
```

Output:

```txt
  0        0
  1        1
  2       10
  3      100
  4      101
  5     1000
  6     1001
  7     1010
  8    10000
  9    10001
 10    10010
 11    10100
 12    10101
 13   100000
 14   100001
 15   100010
 16   100100
 17   100101
 18   101000
 19   101001
 20   101010
```



## PARI/GP


```parigp
Z(n)=if(!n,print1(0));my(k=2);while(fibonacci(k)<=n,k++); forstep(i=k-1,2,-1,print1(if(fibonacci(i)<=n,n-=fibonacci(i);1,0)));print
for(n=0,20,Z(n))
```


```txt
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010
```



## Perl


```perl
my @fib;

sub fib {
  my $n = shift;
  return 1 if $n < 2;
  return $fib[$n] //= fib($n-1)+fib($n-2);
}

sub zeckendorf {
  my $n = shift;
  return "0" unless $n;
  my $i = 1;
  $i++ while fib($i) <= $n;
  my $z = '';
  while( --$i ) {
    $z .= "0", next if fib( $i ) > $n;
    $z .= "1";
    $n -= fib( $i );
  }
  return $z;
}

printf "%4d: %8s\n", $_, zeckendorf($_) for 0..20;

```

```txt
   0:        0
   1:        1
   2:       10
   3:      100
   4:      101
   5:     1000
   6:     1001
   7:     1010
   8:    10000
   9:    10001
  10:    10010
  11:    10100
  12:    10101
  13:   100000
  14:   100001
  15:   100010
  16:   100100
  17:   100101
  18:   101000
  19:   101001
  20:   101010

```



## Perl 6

```perl6
printf "%2d: %8s\n", $_, zeckendorf($_) for 0 .. 20;

multi zeckendorf(0) { '0' }
multi zeckendorf($n is copy) {
    constant FIBS = (1,2, *+* ... *).cache;
    [~] map {
        $n -= $_ if my $digit = $n >= $_;
        +$digit;
    }, reverse FIBS ...^ * > $n;
}
```

```txt
 0:        0
 1:        1
 2:       10
 3:      100
 4:      101
 5:     1000
 6:     1001
 7:     1010
 8:    10000
 9:    10001
10:    10010
11:    10100
12:    10101
13:   100000
14:   100001
15:   100010
16:   100100
17:   100101
18:   101000
19:   101001
20:   101010
```



## Phix


```Phix
function zeckendorf(integer n)
integer r = 0, c
sequence fib = {1,1}
    while fib[$]<n do
        fib &= fib[$] + fib[$-1]
    end while
    for i=length(fib) to 2 by -1 do
        c = n>=fib[i]
        r += r+c
        n -= c*fib[i]
    end for
    return r
end function

for i=0 to 20 do
    printf(1,"%2d: %7b\n",{i,zeckendorf(i)})
end for
```

```txt

 0:       0
 1:       1
 2:      10
 3:     100
 4:     101
 5:    1000
 6:    1001
 7:    1010
 8:   10000
 9:   10001
10:   10010
11:   10100
12:   10101
13:  100000
14:  100001
15:  100010
16:  100100
17:  100101
18:  101000
19:  101001
20:  101010

```



## PHP


```PHP

<?php
$m = 20;

$F = array(1,1);
while ($F[count($F)-1] <= $m)
   $F[] = $F[count($F)-1] + $F[count($F)-2];

while ($n = $m--) {
   while ($F[count($F)-1] > $n) array_pop($F);
   $l = count($F)-1;
   print "$n: ";
   while ($n) {
      if ($n >= $F[$l]) {
         $n = $n - $F[$l];
         print '1';
      } else print '0';
      --$l;
   }
   print str_repeat('0',$l);
   print "\n";
}
?>

```

```txt

20: 101010
19: 101001
18: 101000
17: 100101
16: 100100
15: 100010
14: 100001
13: 100000
12: 10101
11: 10100
10: 10010
9: 10001
8: 10000
7: 1010
6: 1001
5: 1000
4: 101
3: 100
2: 10
1: 1

```



## PicoLisp


```PicoLisp
(de fib (N)
   (let Fibs (1 1)
      (while (>= N (+ (car Fibs) (cadr Fibs)))
         (push 'Fibs (+ (car Fibs) (cadr Fibs))) )
      (uniq Fibs) ) )

(de zecken1 (N)
   (make
      (for I (fib N)
         (if (> I N)
            (link 0)
            (link 1)
            (dec 'N I) ) ) ) )

(de zecken2 (N)
   (make
      (when (=0 N) (link 0))
      (for I (fib N)
         (when (<= I N)
            (link I)
            (dec 'N I) ) ) ) )

(for (N 0 (> 21 N) (inc N))
   (tab (2 4 6 2 -10)
      N
      " -> "
      (zecken1 N)
      "  "
      (glue " + " (zecken2 N)) ) )

(bye)
```

```txt

 0 ->      0  0
 1 ->      1  1
 2 ->     10  2
 3 ->    100  3
 4 ->    101  3 + 1
 5 ->   1000  5
 6 ->   1001  5 + 1
 7 ->   1010  5 + 2
 8 ->  10000  8
 9 ->  10001  8 + 1
10 ->  10010  8 + 2
11 ->  10100  8 + 3
12 ->  10101  8 + 3 + 1
13 -> 100000  13
14 -> 100001  13 + 1
15 -> 100010  13 + 2
16 -> 100100  13 + 3
17 -> 100101  13 + 3 + 1
18 -> 101000  13 + 5
19 -> 101001  13 + 5 + 1
20 -> 101010  13 + 5 + 2
```



## plainTeX

This code needs an etex engine.


```tex
\def\genfibolist#1{% #creates the fibo list which sum>=#1
	\let\fibolist\empty\def\targetsum{#1}\def\fibosum{0}%
	\genfibolistaux1,1\relax
}
\def\genfibolistaux#1,#2\relax{%
	\ifnum\fibosum<\targetsum\relax
		\edef\fibosum{\number\numexpr\fibosum+#2}%
		\edef\fibolist{#2,\fibolist}%
		\edef\tempfibo{\noexpand\genfibolistaux#2,\number\numexpr#1+#2\relax\relax}%
		\expandafter\tempfibo
	\fi
}
\def\zeckendorf#1{\expandafter\zeckendorfaux\fibolist,\relax#1\relax\relax0}
\def\zeckendorfaux#1,#2\relax#3\relax#4\relax#5{%
	\ifx\relax#2\relax
		#4%
	\else
		\ifnum#3<#1
			\edef\temp{#2\relax#3\relax#4\ifnum#5=1 0\fi\relax#5}%
		\else
			\edef\temp{#2\relax\number\numexpr#3-#1\relax\relax#41\relax1}%
		\fi
		\expandafter\expandafter\expandafter\zeckendorfaux\expandafter\temp
	\fi
}
\newcount\ii
\def\listzeckendorf#1{%
	\genfibolist{#1}%
	\ii=0
	\loop
		\ifnum\ii<#1
		\advance\ii1
		\number\ii: \zeckendorf\ii\endgraf
	\repeat
}
\listzeckendorf{20}% any integer accepted
\bye
```


pdf output looks like:

```txt

1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## PowerShell

```PowerShell

function Get-ZeckendorfNumber ( $N )
    {
    #  Calculate relevant portation of Fibonacci series
    $Fib = @( 1, 1 )
    While ( $Fib[-1] -lt $N ) { $Fib += $Fib[-1] + $Fib[-2] }

    #  Start with 0
    $ZeckendorfNumber = 0

    #  For each number in the relevant portion of Fibonacci series
    For ( $i = $Fib.Count - 1; $i -gt 0; $i-- )
        {
        #  If Fibonacci number is less than or equal to remainder of N
        If ( $Fib[$i] -le $N )
            {
            #  Double Z number and add 1 (equivalent to adding a '1' to the end of a binary number)
            $ZeckendorfNumber = $ZeckendorfNumber * 2 + 1
            #  Reduce N by Fibonacci number, skip next Fibonacci number
            $N -= $Fib[$i--]
            }
        #  If were aren't finished yet, double Z number
        #  (equivalent to adding a '0' to the end of a binary number)
        If ( $i ) { $ZeckendorfNumber *= 2 }
        }
    return $ZeckendorfNumber
    }

```


```PowerShell

#  Get Zeckendorf numbers through 20, convert to binary for display
0..20 | ForEach { [convert]::ToString( ( Get-ZeckendorfNumber $_ ), 2 ) }

```

```txt

0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010

```



## PureBasic


```PureBasic
Procedure.s zeck(n.i)
  Dim f.i(1) : Define i.i=1, o$
  f(0)=1 : f(1)=1
  While f(i)<n
    i+1 : ReDim f(ArraySize(f())+1) : f(i)=f(i-1)+f(i-2)
  Wend
  For i=i To 1 Step -1
    If n>=f(i) : o$+"1" : n-f(i) : Else : o$+"0" : EndIf
  Next
  If Len(o$)>1 : o$=LTrim(o$,"0") : EndIf
  ProcedureReturn o$
EndProcedure

Define n.i, t$
OpenConsole("Zeckendorf number representation")
PrintN(~"\tNr.\tZeckendorf")
For n=0 To 20
  t$=zeck(n)
  If FindString(t$,"11")
    PrintN("Error: n= "+Str(n)+~"\tZeckendorf= "+t$)
    Break
  Else
    PrintN(~"\t"+RSet(Str(n),3," ")+~"\t"+RSet(t$,7," "))
  EndIf
Next
Input()
```

```txt
        Nr.     Zeckendorf
          0           0
          1           1
          2          10
          3         100
          4         101
          5        1000
          6        1001
          7        1010
          8       10000
          9       10001
         10       10010
         11       10100
         12       10101
         13      100000
         14      100001
         15      100010
         16      100100
         17      100101
         18      101000
         19      101001
         20      101010
```



## Python


```python
def fib():
    memo = [1, 2]
    while True:
        memo.append(sum(memo))
        yield memo.pop(0)

def sequence_down_from_n(n, seq_generator):
    seq = []
    for s in seq_generator():
        seq.append(s)
        if s >= n: break
    return seq[::-1]

def zeckendorf(n):
    if n == 0: return [0]
    seq = sequence_down_from_n(n, fib)
    digits, nleft = [], n
    for s in seq:
        if s <= nleft:
            digits.append(1)
            nleft -= s
        else:
            digits.append(0)
    assert nleft == 0, 'Check all of n is accounted for'
    assert sum(x*y for x,y in zip(digits, seq)) == n, 'Assert digits are correct'
    while digits[0] == 0:
        # Remove any zeroes padding L.H.S.
        digits.pop(0)
    return digits

n = 20
print('Fibonacci digit multipliers: %r' % sequence_down_from_n(n, fib))
for i in range(n + 1):
    print('%3i: %8s' % (i, ''.join(str(d) for d in zeckendorf(i))))
```


```txt
Fibonacci digit multipliers: [21, 13, 8, 5, 3, 2, 1]
  0:        0
  1:        1
  2:       10
  3:      100
  4:      101
  5:     1000
  6:     1001
  7:     1010
  8:    10000
  9:    10001
 10:    10010
 11:    10100
 12:    10101
 13:   100000
 14:   100001
 15:   100010
 16:   100100
 17:   100101
 18:   101000
 19:   101001
 20:   101010
```



### Shorter version


```python
n = 20
def z(n):
    if n == 0 : return [0]
    fib = [2,1]
    while fib[0] < n: fib[0:0] = [sum(fib[:2])]
    dig = []
    for f in fib:
        if f <= n:
            dig, n = dig + [1], n - f
        else:
            dig += [0]
    return dig if dig[0] else dig[1:]

for i in range(n + 1):
    print('%3i: %8s' % (i, ''.join(str(d) for d in z(i))))
```


```txt
  0:        0
  1:        1
  2:       10
  3:      100
  4:      101
  5:     1000
  6:     1001
  7:     1010
  8:    10000
  9:    10001
 10:    10010
 11:    10100
 12:    10101
 13:   100000
 14:   100001
 15:   100010
 16:   100100
 17:   100101
 18:   101000
 19:   101001
 20:   101010
```



## R


```R
zeckendorf <- function(number) {

  # Get an upper limit on Fibonacci numbers needed to cover number
  indexOfFibonacciNumber <- function(n) {
    if (n < 1) {
      2
    } else {
      Phi <- (1 + sqrt(5)) / 2
      invertClosedFormula <- log(n * sqrt(5)) / log(Phi)
      ceiling(invertClosedFormula)
    }
  }

  upperLimit <- indexOfFibonacciNumber(number)

  # Return the sequence as digits, sorted descending
  fibonacciSequenceDigits <- function(n) {
    fibGenerator <- function(f, ...) { c(f[2], sum(f)) }
    fibSeq <- Reduce(fibGenerator, 1:n, c(0,1), accumulate=TRUE)

    fibNums <- unlist(lapply(fibSeq, head, n=1))

    # drop last F0 and F1 and reverse sequence
    rev(fibNums[-2:-1])
  }

  digits <- fibonacciSequenceDigits(upperLimit)

  isInNumber <- function(digit) {
    if (number >= digit) {
      number <<- number - digit
      1
    } else {
      0
    }
  }

  zeckSeq <- Map(isInNumber, digits)

  # drop leading 0 and convert to String
  gsub("^0+1", "1", paste(zeckSeq, collapse=""))
}

print(unlist(lapply(0:20, zeckendorf)))
```


This is definitely not the shortest way to implement the Zeckendorf numbers but focus was on the functional aspect of R, so no loops and (almost) no assignments.

```txt
 [1] "0"      "1"      "10"     "100"    "101"    "1000"   "1001"   "1010"
 [9] "10000"  "10001"  "10010"  "10100"  "10101"  "100000" "100001" "100010"
[17] "100100" "100101" "101000" "101001" "101010"
```



## Racket


```racket

#lang racket (require math)

(define (fibs n)
  (reverse
   (for/list ([i (in-naturals 2)] #:break (> (fibonacci i) n))
     (fibonacci i))))

(define (zechendorf n)
  (match/values
   (for/fold ([n n] [xs '()]) ([f (fibs n)])
     (if (> f n)
         (values n       (cons 0 xs))
         (values (- n f) (cons 1 xs))))
   [(_ xs) (reverse xs)]))

(for/list ([n 21])
  (list n (zechendorf n)))

```

Output:

```racket

'((0 ())
  (1 (1))
  (2 (1 0))
  (3 (1 0 0))
  (4 (1 0 1))
  (5 (1 0 0 0))
  (6 (1 0 0 1))
  (7 (1 0 1 0))
  (8 (1 0 0 0 0))
  (9 (1 0 0 0 1))
  (10 (1 0 0 1 0))
  (11 (1 0 1 0 0))
  (12 (1 0 1 0 1))
  (13 (1 0 0 0 0 0))
  (14 (1 0 0 0 0 1))
  (15 (1 0 0 0 1 0))
  (16 (1 0 0 1 0 0))
  (17 (1 0 0 1 0 1))
  (18 (1 0 1 0 0 0))
  (19 (1 0 1 0 0 1))
  (20 (1 0 1 0 1 0)))

```



## REXX


### specific to 20


```rexx

/* REXX ***************************************************************
* 11.10.2012 Walter Pachl
**********************************************************************/
fib='13 8 5 3 2 1'
Do i=6 To 1 By -1                   /* Prepare Fibonacci Numbers     */
  Parse Var fib f.i fib             /* f.1 ... f.7                   */
  End
Do n=0 To 20                        /* for all numbers in the task   */
  m=n                               /* copy of number                */
  r=''                              /* result for n                  */
  Do i=6 To 1 By -1                 /* loop through numbers          */
    If m>=f.i Then Do               /* f.i must be used              */
      r=r||1                        /* 1 into result                 */
      m=m-f.i                       /* subtract                      */
      End
    Else                            /* f.i is larger than the rest   */
      r=r||0                        /* 0 into result                 */
    End
  r=strip(r,'L','0')                /* strip leading zeros           */
  If r='' Then r='0'                /* take care of 0                */
  Say right(n,2)':  'right(r,6)     /* show result                   */
  End
```

Output:

```txt

 0:       0
 1:       1
 2:      10
 3:     100
 4:     101
 5:    1000
 6:    1001
 7:    1010
 8:   10000
 9:   10001
10:   10010
11:   10100
12:   10101
13:  100000
14:  100001
15:  100010
16:  100100
17:  100101
18:  101000
19:  101001
20:  101010
```



### generalized

This generalized REXX version will work for any Zeckendorf number (up to 100,000 decimal digits).


A list of Fibonacci numbers (in ascending order) is generated large enough to handle the   '''N<sup>th</sup>'''   Zeckendorf number.

```rexx
/*REXX program  calculates and displays the  first   N   Zeckendorf numbers.            */
numeric digits 100000                            /*just in case user gets real ka─razy. */
parse arg N .                                    /*let the user specify the upper limit.*/
if N=='' | N==","  then n=20;   w= length(N)     /*Not specified?  Then use the default.*/
@.1= 1                                           /*start the array with  1   and   2.   */
@.2= 2;   do  #=3  until #>=N;  p= #-1;  pp= #-2 /*build a list of Fibonacci numbers.   */
          @.#= @.p + @.pp                        /*sum the last two Fibonacci numbers.  */
          end   /*#*/                            /* [↑]   #:  contains a Fibonacci list.*/

  do j=0  to N;             parse var j x z      /*task:  process zero  ──►  N  numbers.*/
     do k=#  by -1  for #;  _= @.k               /*process all the Fibonacci numbers.   */
     if x>=_  then do;      z= z'1'              /*is X>the next Fibonacci #?  Append 1.*/
                            x= x - _             /*subtract this Fibonacci # from index.*/
                   end
              else z= z'0'                       /*append zero (0)  to the Fibonacci #. */
     end   /*k*/
  say '    Zeckendorf'     right(j, w)    "="     right(z+0, 30)     /*display a number.*/
  end     /*j*/                                  /*stick a fork in it,  we're all done. */
```

```txt

    Zeckendorf  0 =                              0
    Zeckendorf  1 =                              1
    Zeckendorf  2 =                             10
    Zeckendorf  3 =                            100
    Zeckendorf  4 =                            101
    Zeckendorf  5 =                           1000
    Zeckendorf  6 =                           1001
    Zeckendorf  7 =                           1010
    Zeckendorf  8 =                          10000
    Zeckendorf  9 =                          10001
    Zeckendorf 10 =                          10010
    Zeckendorf 11 =                          10100
    Zeckendorf 12 =                          10101
    Zeckendorf 13 =                         100000
    Zeckendorf 14 =                         100001
    Zeckendorf 15 =                         100010
    Zeckendorf 16 =                         100100
    Zeckendorf 17 =                         100101
    Zeckendorf 18 =                         101000
    Zeckendorf 19 =                         101001
    Zeckendorf 20 =                         101010

```



### generic

This generic REXX version will generate up to the   '''N<sup>th</sup>'''   Zeckendorf numbers (up to 100,000 decimal digits) by

using binary numbers that   ''don't''   have two consecutive   '''<big>11</big>'''s   within their binary version.

There isn't any need to generate a Fibonacci series with this method.   This method is extremely fast.

```REXX
/*REXX program  calculates and displays the  first   N   Zeckendorf numbers.            */
numeric digits 100000                            /*just in case user gets real ka─razy. */
parse arg N .                                    /*let the user specify the upper limit.*/
if N=='' | N==","  then n=20;    w= length(N)    /*Not specified?  Then use the default.*/
z=0                                              /*the index of a  Zeckendorf number.   */
    do j=0  until z>N;          _=x2b( d2x(j) )  /*task:   process zero  ──►   N.       */
    if pos(11, _) \== 0  then iterate            /*are there two consecutive ones (1s) ?*/
    say '    Zeckendorf'   right(z, w)    "="     right(_+0, 30)     /*display a number.*/
    z= z + 1                                     /*bump the  Zeckendorf  number counter.*/
    end   /*j*/                                  /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the previous (generalized) version.




## Ring


```ring

# Project : Zeckendorf number representation

see "0 0" + nl
for n = 1 to 20
     see "" + n + " " + zeckendorf(n) + nl
next

func zeckendorf(n)
       fib = list(45)
       fib[1] = 1
       fib[2] = 1
       i = 2
       o = ""
       while fib[i] <= n
               i = i + 1
               fib[i] = fib[i-1] + fib[i-2]
       end
       while i != 2
               i = i - 1
               if n >= fib[i]
                   o = o + "1"
                   n = n - fib[i]
               else
                   o = o + "0"
               ok
        end
        return o

```

Output:

```txt

0 0
1 1
2 10
3 100
4 101
5 1000
6 1001
7 1010
8 10000
9 10001
10 10010
11 10100
12 10101
13 100000
14 100001
15 100010
16 100100
17 100101
18 101000
19 101001
20 101010

```



## Ruby

Featuring a method doubling as an enumerator.

```ruby
def zeckendorf
  return to_enum(__method__) unless block_given?
  x = 0
  loop do
    bin = x.to_s(2)
    yield bin unless bin.include?("11")
    x += 1
  end
end

zeckendorf.take(21).each_with_index{|x,i| puts "%3d: %8s"% [i, x]}

```

```txt

  0:        0
  1:        1
  2:       10
  3:      100
  4:      101
  5:     1000
  6:     1001
  7:     1010
  8:    10000
  9:    10001
 10:    10010
 11:    10100
 12:    10101
 13:   100000
 14:   100001
 15:   100010
 16:   100100
 17:   100101
 18:   101000
 19:   101001
 20:   101010

```

```ruby
def zeckendorf(n)
  return 0 if n.zero?
  fib = [1,2]
  fib << fib[-2] + fib[-1] while fib[-1] < n
  dig = ""
  fib.reverse_each do |f|
    if f <= n
      dig, n = dig + "1", n - f
    else
      dig += "0"
    end
  end
  dig.to_i
end

for i in 0..20
  puts '%3d: %8d' % [i, zeckendorf(i)]
end
```

(Same output.)


## Scala


```scala
def zNum( n:BigInt ) : String = {

  if( n == 0 ) return "0"	// Short-circuit this and return zero if we were given zero


  val v = n.abs

  val fibs : Stream[BigInt] = { def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j, i+j); series(1,0).tail.tail.tail }


  def z( v:BigInt ) : List[BigInt] = if(v == 0) List() else {val m = fibs(fibs.indexWhere(_>v) - 1); m :: z(v-m)}

  val zv = z(v)

  // Walk the list of fibonacci numbers from the number that matches the most significant down to 1,
  // if the zeckendorf matchs then yield '1' otherwise '0'
  val s = (for( i <- (fibs.indexWhere(_==zv(0)) to 0 by -1) ) yield {

    if( zv.contains(fibs(i))) "1" else "0"

  }).mkString

  if( n < 0 ) "-" + s		// Using a negative-sign instead of twos-complement
  else s
}


// A little test...
(0 to 20) foreach( i => print( zNum(i) + "\n" ) )

```

```txt
0
1
10
100
101
1000
1001
1010
10000
10001
10010
10100
10101
100000
100001
100010
100100
100101
101000
101001
101010

```


## Scheme

```scheme
(import (rnrs))

(define (getFibList maxNum n1 n2 fibs)
  (if (> n2 maxNum)
      fibs
      (getFibList maxNum n2 (+ n1 n2) (cons n2 fibs))))

(define (getZeckendorf num)
  (if (<= num 0)
      "0"
      (let ((fibs (getFibList num 1 2 (list 1))))
        (getZeckString "" num fibs))))

(define (getZeckString zeck num fibs)
  (let* ((curFib (car fibs))
         (placeZeck (>= num curFib))
         (outString (string-append zeck (if placeZeck "1" "0")))
         (outNum (if placeZeck (- num curFib) num)))
    (if (null? (cdr fibs))
        outString
        (getZeckString outString outNum (cdr fibs)))))

(let loop ((i 0))
  (when (<= i 20)
    (for-each
      (lambda (item)
        (display item))
      (list "Z(" i "):\t" (getZeckendorf i)))
    (newline)
    (loop (+ i 1))))

```

```txt
Z(0):   0
Z(1):   1
Z(2):   10
Z(3):   100
Z(4):   101
Z(5):   1000
Z(6):   1001
Z(7):   1010
Z(8):   10000
Z(9):   10001
Z(10):  10010
Z(11):  10100
Z(12):  10101
Z(13):  100000
Z(14):  100001
Z(15):  100010
Z(16):  100100
Z(17):  100101
Z(18):  101000
Z(19):  101001
Z(20):  101010
```



## Sidef

```ruby
func fib(n) is cached {
    n < 2 ? 1
          : (fib(n-1) + fib(n-2))
}

func zeckendorf(n) {
    n == 0 && return '0'
    var i = 1
    ++i while (fib(i) <= n)
    gather {
        while (--i > 0) {
            var f = fib(i)
            f > n ? (take '0')
                  : (take '1'; n -= f)
        }
    }.join
}

for n (0..20) {
    printf("%4d: %8s\n", n, zeckendorf(n))
}
```

```txt

   0:        0
   1:        1
   2:       10
   3:      100
   4:      101
   5:     1000
   6:     1001
   7:     1010
   8:    10000
   9:    10001
  10:    10010
  11:    10100
  12:    10101
  13:   100000
  14:   100001
  15:   100010
  16:   100100
  17:   100101
  18:   101000
  19:   101001
  20:   101010

```


## Simula

```simula
BEGIN
   INTEGER N, F0, F1, F2, D;
   N := 20;
   COMMENT CALCULATE D FROM ANY GIVEN N ;
   F1 := 1; F2 := 2; F0 := F1 + F2; D := 2;
   WHILE F0 < N DO BEGIN
      F1 := F2; F2 := F0; F0 := F1 + F2; D := D + 1;
   END;
   BEGIN
      COMMENT Sinclair ZX81 BASIC Solution ;
      TEXT Z1, S1;
      INTEGER I, J, Z;
      INTEGER ARRAY F(1:D);                  !  10 dim f(6) ;
      F(1) := 1;                             !  20 let f(1)=1 ;
      F(2) := 2;                             !  30 let f(2)=2 ;
      FOR I := 3 STEP 1 UNTIL D DO BEGIN     !  40 for i=3 to 6 ;
         F(I) := F(I-2) + F(I-1);            !  50 let f(i)=f(i-2)+f(i-1) ;
      END;                                   !  60 next i ;
      FOR I := 0 STEP 1 UNTIL N DO BEGIN     !  70 for i=0 to 20 ;
         Z1 :- "";                           !  80 let z$="" ;
         S1 :- " ";                          !  90 let s$=" " ;
         Z := I;                             ! 100 let z=i ;
         FOR J := D STEP -1 UNTIL 1 DO BEGIN ! 110 for j=6 to 1 step -1 ;
            IF J=1 THEN S1 :- "0";           ! 120 if j=1 then let s$="0" ;
            IF NOT (Z<F(J)) THEN BEGIN       ! 130 if z<f(j) then goto 180 ;
               Z1 :- Z1 & "1";               ! 140 let z$=z$+"1" ;
               Z := Z-F(J);                  ! 150 let z=z-f(j) ;
               S1 :- "0";                    ! 160 let s$="0" ;
            END ELSE                         ! 170 goto 190 ;
               Z1 :- Z1 & S1;                ! 180 let z$=z$+s$ ;
         END;                                ! 190 next j ;
         OUTINT(I, 0); OUTCHAR(' ');         ! 200 print i ; !" "; !;
         IF I<10 THEN OUTCHAR(' ');          ! 210 if i<10 then print " "; !;
         OUTTEXT(Z1); OUTIMAGE;              ! 220 print z$ ;
      END;                                   ! 230 next i ;
   END;
END
```

```txt
0       0
1       1
2      10
3     100
4     101
5    1000
6    1001
7    1010
8   10000
9   10001
10  10010
11  10100
12  10101
13 100000
14 100001
15 100010
16 100100
17 100101
18 101000
19 101001
20 101010
```



## Sinclair ZX81 BASIC

Works on the 1k RAM model, albeit without much room for manoeuvre. (You'd like the Zeckendorf numbers further over towards the right-hand side of the screen? Sorry, can't spare the video RAM.) If you have 2k or more, you can replace the constant 6 with some higher value wherever it occurs in the program and enable yourself to represent bigger numbers in Zeckendorf form.

```basic
 10 DIM F(6)
 20 LET F(1)=1
 30 LET F(2)=2
 40 FOR I=3 TO 6
 50 LET F(I)=F(I-2)+F(I-1)
 60 NEXT I
 70 FOR I=0 TO 20
 80 LET Z$=""
 90 LET S$=" "
100 LET Z=I
110 FOR J=6 TO 1 STEP -1
120 IF J=1 THEN LET S$="0"
130 IF Z<F(J) THEN GOTO 180
140 LET Z$=Z$+"1"
150 LET Z=Z-F(J)
160 LET S$="0"
170 GOTO 190
180 LET Z$=Z$+S$
190 NEXT J
200 PRINT I;" ";
210 IF I<10 THEN PRINT " ";
220 PRINT Z$
230 NEXT I
```

```txt
0       0
1       1
2      10
3     100
4     101
5    1000
6    1001
7    1010
8   10000
9   10001
10  10010
11  10100
12  10101
13 100000
14 100001
15 100010
16 100100
17 100101
18 101000
19 101001
20 101010
```



## Tcl


```tcl
package require Tcl 8.5

# Generates the Fibonacci sequence (starting at 1) up to the largest item that
# is no larger than the target value. Could use tricks to precompute, but this
# is actually a pretty cheap linear operation.
proc fibseq target {
    set seq {}; set prev 1; set fib 1
    for {set n 1;set i 1} {$fib <= $target} {incr n} {
	for {} {$i < $n} {incr i} {
	    lassign [list $fib [incr fib $prev]] prev fib
	}
	if {$fib <= $target} {
	    lappend seq $fib
	}
    }
    return $seq
}

# Produce the given Zeckendorf number.
proc zeckendorf n {
    # Special case: only value that begins with 0
    if {$n == 0} {return 0}
    set zs {}
    foreach f [lreverse [fibseq $n]] {
	lappend zs [set z [expr {$f <= $n}]]
	if {$z} {incr n [expr {-$f}]}
    }
    return [join $zs ""]
}
```

Demonstration

```tcl
for {set i 0} {$i <= 20} {incr i} {
    puts [format "%2d:%9s" $i [zeckendorf $i]]
}
```

```txt
 0:        0
 1:        1
 2:       10
 3:      100
 4:      101
 5:     1000
 6:     1001
 7:     1010
 8:    10000
 9:    10001
10:    10010
11:    10100
12:    10101
13:   100000
14:   100001
15:   100010
16:   100100
17:   100101
18:   101000
19:   101001
20:   101010
```



## uBasic/4tH

<lang>For x = 0 to 20                        ' Print Zeckendorf numbers 0 - 20
  Print x,
  Push x : Gosub _Zeckendorf           ' get Zeckendorf number repres.
  Print                                ' terminate line
Next

End

_Fibonacci
  Push Tos()                           ' duplicate TOS()
  @(0) = 0                             ' This function returns the
  @(1) = 1                             ' Fibonacci number which is smaller
                                       ' or equal to TOS()
  Do While @(1) < Tos() + 1
     Push (@(1))
     @(1) = @(0) + @(1)                ' get next Fibonacci number
     @(0) = Pop()
  Loop                                 ' loop if not exceeded TOS()

  Gosub _Drop                          ' clear TOS()
  Push @(0)                            ' return Fibonacci number
Return

_Zeckendorf
  GoSub _Fibonacci                     ' This function breaks TOS() up
  Print Tos();                         ' into its Zeckendorf components
  Push -(Pop() - Pop())                ' first digit is always there
                                       ' the remainder to resolve
  Do While Tos()                       ' now go for the next digits
    GoSub _Fibonacci
    Print " + ";Tos();                 ' print the next digit
    Push -(Pop() - Pop())
  Loop

  Gosub _Drop                          ' clear TOS()
Return                                 ' and return

_Drop
  If Pop()%1 = 0 Then Return           ' This function clears TOS()
```

Output:

```txt
0       0
1       1
2       2
3       3
4       3 + 1
5       5
6       5 + 1
7       5 + 2
8       8
9       8 + 1
10      8 + 2
11      8 + 3
12      8 + 3 + 1
13      13
14      13 + 1
15      13 + 2
16      13 + 3
17      13 + 3 + 1
18      13 + 5
19      13 + 5 + 1
20      13 + 5 + 2

0 OK, 0:901
```



## VBA

```vb
Private Function zeckendorf(ByVal n As Integer) As Integer
    Dim r As Integer: r = 0
    Dim c As Integer
    Dim fib As New Collection
    fib.Add 1
    fib.Add 1
    Do While fib(fib.Count) < n
        fib.Add fib(fib.Count - 1) + fib(fib.Count)
    Loop
    For i = fib.Count To 2 Step -1
        c = n >= fib(i)
        r = r + r - c
        n = n + c * fib(i)
    Next i
    zeckendorf = r
End Function

Public Sub main()
    Dim i As Integer
    For i = 0 To 20
        Debug.Print Format(i, "@@"); ":"; Format(WorksheetFunction.Dec2Bin(zeckendorf(i)), "@@@@@@@")
    Next i
End Sub
```
```txt
 0:      0
 1:      1
 2:     10
 3:    100
 4:    101
 5:   1000
 6:   1001
 7:   1010
 8:  10000
 9:  10001
10:  10010
11:  10100
12:  10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010
```


## VBScript


```vb

Function Zeckendorf(n)
	num = n
	Set fibonacci = CreateObject("System.Collections.Arraylist")
	fibonacci.Add 1 : fibonacci.Add 2
	i = 1
	Do While fibonacci(i) < num
		fibonacci.Add fibonacci(i) + fibonacci(i-1)
		i = i + 1
	Loop
	tmp = ""
	For j = fibonacci.Count-1 To 0 Step -1
		If fibonacci(j) <= num And (tmp = "" Or Left(tmp,1) <> "1") Then
			tmp = tmp & "1"
			num = num - fibonacci(j)
		Else
			tmp = tmp & "0"
		End If
	Next
	Zeckendorf = CLng(tmp)
End Function

'testing the function
For k = 0 To 20
	WScript.StdOut.WriteLine k & ": " & Zeckendorf(k)
Next

```


```txt

0: 0
1: 1
2: 10
3: 100
4: 101
5: 1000
6: 1001
7: 1010
8: 10000
9: 10001
10: 10010
11: 10100
12: 10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Zeckendorf(N);             \Display Zeckendorf number (N <= 20)
int N;
int Fib, LZ, I;
[Fib:= [1, 2, 3, 5, 8, 13];     \Fibonacci sequence
LZ:= true;                      \suppress leading zeros
for I:= 5 downto 1 do
        [if N >= Fib(I) then [N:= N-Fib(I);  ChOut(0, ^1);  LZ:= false]
        else ChOut(0, if LZ then ^  else ^0);
        ];
ChOut(0, N+^0);                 \output final digit, which can be 0
];

int N;
[for N:= 0 to 20 do
        [if N<10 then ChOut(0,^ );  IntOut(0, N);  Text(0, ": ");
        Zeckendorf(N);  CrLf(0);
        ];
]
```


Output:

```txt

 0:      0
 1:      1
 2:     10
 3:    100
 4:    101
 5:   1000
 6:   1001
 7:   1010
 8:  10000
 9:  10001
10:  10010
11:  10100
12:  10101
13: 100000
14: 100001
15: 100010
16: 100100
17: 100101
18: 101000
19: 101001
20: 101010

```



## Yabasic


```Yabasic
sub Zeckendorf(n)
	local i, n$, c

	do
		n$ = bin$(i)
		if not instr(n$,"11") then
			print c,":\t",n$
			if c = n break
			c = c + 1
		end if
		i = i + 1
	loop
end sub

Zeckendorf(20)

```



## zkl


```zkl
    // return powers (0|1) of fib sequence (1,2,3,5,8...) that sum to n
fcn zeckendorf(n){ //-->String of 1s & 0s, no consecutive 1's
   if(n<=0) return("0");
   fibs:=fcn(ab){ ab.append(ab.sum()).pop(0) }.fp(L(1,2));
   (0).pump(*,List,fibs,'wrap(fib){ if(fib>n)Void.Stop else fib })
   .reverse()
   .pump(String,fcn(fib,rn){
      if(fib>rn.value)"0" else { rn.set(rn.value-fib); "1" } }.fp1(Ref(n)))
}
```


```zkl
[0..20].pump(Console.println,fcn(n){ "%2d: %8s".fmt(n,zeckendorf(n)) });
```

<pre style="height:7em">
 0:        0
 1:        1
 2:       10
 3:      100
 4:      101
 5:     1000
 6:     1001
 7:     1010
 8:    10000
 9:    10001
10:    10010
11:    10100
12:    10101
13:   100000
14:   100001
15:   100010
16:   100100
17:   100101
18:   101000
19:   101001
20:   101010

```


