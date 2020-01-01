+++
title = "Narcissistic decimal number"
description = ""
date = 2019-08-25T12:52:35Z
aliases = []
[extra]
id = 17335
[taxonomies]
categories = []
tags = []
+++

{{task}}

A   [http://mathworld.wolfram.com/NarcissisticNumber.html Narcissistic decimal number]   is a non-negative integer,   <math>n</math>,   that is equal to the sum of the   <math>m</math>-th   powers of each of the digits in the decimal representation of   <math>n</math>,   where   <math>m</math>   is the number of digits in the decimal representation of   <math>n</math>.


Narcissistic (decimal) numbers are sometimes called   '''Armstrong'''   numbers, named after Michael F. Armstrong.

They are also known as   '''Plus Perfect'''   numbers.


;An example:
::::*   if   <math>n</math>   is   '''153'''
::::*   then   <math>m</math>,   (the number of decimal digits)   is   '''3'''
::::*   we have   <big> 1<sup>3</sup> + 5<sup>3</sup> + 3<sup>3</sup>   =   1 + 125 + 27   =   '''153''' </big>
::::*   and so   '''153'''   is a narcissistic decimal number


;Task:
Generate and show here the first   '''25'''   narcissistic decimal numbers.



Note:   <math>0^1 = 0</math>,   the first in the series.


;See also:
*   the  OEIS entry:     [http://oeis.org/A005188 Armstrong (or Plus Perfect, or narcissistic) numbers].
*   MathWorld entry:   [http://mathworld.wolfram.com/NarcissisticNumber.html Narcissistic Number].
*   Wikipedia entry:     [https://en.wikipedia.org/wiki/Narcissistic_number Narcissistic number].





## Ada



```Ada
with Ada.Text_IO;

procedure Narcissistic is

   function Is_Narcissistic(N: Natural) return Boolean is
      Decimals: Natural := 1;
      M: Natural := N;
      Sum: Natural := 0;
   begin
      while M >= 10 loop
	 M := M / 10;
	 Decimals := Decimals + 1;
      end loop;
      M := N;
      while M >= 1 loop
	 Sum := Sum + (M mod 10) ** Decimals;
	 M := M/10;
      end loop;
      return Sum=N;
   end Is_Narcissistic;

   Count, Current: Natural := 0;

begin
   while Count < 25 loop
      if Is_Narcissistic(Current) then
	 Ada.Text_IO.Put(Integer'Image(Current));
	 Count := Count + 1;
      end if;
      Current := Current + 1;
   end loop;
end Narcissistic;
```


{{out}}

```txt
 0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
scope
    # print the first 25 narcissistic numbers

    local power := reg( 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 );
    local count      := 0;
    local maxCount   := 25;
    local candidate  := 0;
    local prevDigits := 0;
    local digits     := 1;

    for d9 from 0 to 2 while count < maxCount do
        if d9 > 0 and digits < 9 then digits := 9 fi;
        for d8 from 0 to 9 while count < maxCount do
            if d8 > 0 and digits < 8 then digits := 8 fi;
            for d7 from 0 to 9 while count < maxCount do
                if d7 > 0 and digits < 7 then digits := 7 fi;
                for d6 from 0 to 9 while count < maxCount do
                    if d6 > 0 and digits < 6 then digits := 6 fi;
                    for d5 from 0 to 9 while count < maxCount do
                        if d5 > 0 and digits < 5 then digits := 5 fi;
                        for d4 from 0 to 9 while count < maxCount do
                            if d4 > 0 and digits < 4 then digits := 4 fi;
                            for d3 from 0 to 9 while count < maxCount do
                                if d3 > 0 and digits < 3 then digits := 3 fi;
                                for d2 from 0 to 9 while count < maxCount do
                                    if d2 > 0 and digits < 2 then digits := 2 fi;
                                    for d1 from 0 to 9 do
                                        if prevDigits <> digits then
                                            # number of digits has increased - increase the powers
                                            prevDigits := digits;
                                            for i from 2 to 9 do mul power[ i + 1 ], i od;
                                        fi;
                                        # sum the digits'th powers of the digits of candidate
                                        local sum := power[ d1 + 1 ] + power[ d2 + 1 ] + power[ d3 + 1 ]
                                                   + power[ d4 + 1 ] + power[ d5 + 1 ] + power[ d6 + 1 ]
                                                   + power[ d7 + 1 ] + power[ d8 + 1 ] + power[ d9 + 1 ]
                                                   ;
                                        if candidate = sum
                                        then
                                            # found another narcissistic decimal number
                                            io.write( " ", candidate );
                                            inc count, 1
                                        fi;
                                        inc candidate, 1
                                    od; # d1
                                od; # d2
                            od; # d3
                        od; # d4
                    od; # d5
                od; # d6
            od; # d7
        od; # d8
    od; # d9
    io.writeline()

epocs
```

{{out}}

```txt

 0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## ALGOL 68


```algol68
# find some narcissistic decimal numbers                                      #

# returns TRUE if n is narcissitic, FALSE otherwise; n should be >= 0         #
PROC is narcissistic = ( INT n )BOOL:
     BEGIN
        # count the number of digits in n                                     #
        INT digits := 0;
        INT number := n;
        WHILE digits +:= 1;
              number OVERAB 10;
              number > 0
        DO SKIP OD;
        # sum the digits'th powers of the digits of n                         #
        INT sum := 0;
        number  := n;
        TO digits DO
            sum +:= ( number MOD 10 ) ^ digits;
            number OVERAB 10
        OD;
        # n is narcissistic if n = sum                                        #
        n = sum
     END # is narcissistic # ;

# print the first 25 narcissistic numbers                                     #
INT count := 0;
FOR n FROM 0 WHILE count < 25 DO
    IF is narcissistic( n ) THEN
        # found another narcissistic number                                   #
        print( ( " ", whole( n, 0 ) ) );
        count +:= 1
    FI
OD;
print( ( newline ) )
```

{{out}}

```txt

 0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## ALGOL W

{{Trans|Agena}}

```algolw
begin
    % print the first 25 narcissistic numbers                                 %

    integer array  power( 0 :: 9 );
    integer count, candidate, prevDigits, digits;

    power( 0 ) := 0;
    for i := 1 until 9 do power( i ) := 1;

    count      := 0;
    candidate  := 0;
    prevDigits := 0;
    digits     := 1;

    for d9 := 0 until 2 do begin
        if d9 > 0 and digits < 9 then digits := 9;
        for d8 := 0 until 9 do begin
            if d8 > 0 and digits < 8 then digits := 8;
            for d7 := 0 until 9 do begin
                if d7 > 0 and digits < 7 then digits := 7;
                for d6 := 0 until 9 do begin
                    if d6 > 0 and digits < 6 then digits := 6;
                    for d5 := 0 until 9 do begin
                        if d5 > 0 and digits < 5 then digits := 5;
                        for d4 := 0 until 9 do begin
                            if d4 > 0 and digits < 4 then digits := 4;
                            for d3 := 0 until 9 do begin
                                if d3 > 0 and digits < 3 then digits := 3;
                                for d2 := 0 until 9 do begin
                                    if d2 > 0 and digits < 2 then digits := 2;
                                    for d1 := 0 until 9 do begin
                                        integer number, sum;
                                        if prevDigits <> digits then begin
                                            % number of digits has increased %
                                            % - increase the powers          %
                                            prevDigits := digits;
                                            for i := 2 until 9 do power( i ) := power( i ) * i;
                                        end;

                                        % sum the digits'th powers of the    %
                                        % digits of candidate                %
                                        sum := power( d1 ) + power( d2 ) + power( d3 )
                                             + power( d4 ) + power( d5 ) + power( d6 )
                                             + power( d7 ) + power( d8 ) + power( d9 )
                                             ;
                                        if candidate = sum then begin
                                            % found another narcissistic    %
                                            % decimal number                %
                                            writeon( i_w := 1, s_w := 1, candidate );
                                            count := count + 1;
                                            if count >= 25 then goto done
                                        end;
                                        candidate := candidate + 1
                                    end d1;
                                end d2;
                            end d3;
                        end d4;
                    end d5;
                end d6;
            end d7;
        end d8;
    end d9;
done:
    write()

end.
```

{{out}}

```txt

0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}
AppleScript is a little out of its depth here, even with an algorithm which restricts the search space (see the JavaScript and Haskell discussions).

The full 7 digit search that finds the 25th number takes nearly 14 minutes on the system here (four seconds to scan the 5 digit combinations, and find the first 20, and 103 seconds to scan the six digit combinations for the first 21 narcissi).

(For comparison, equivalent code in JavaScript returns all 25 numbers in about 120 milliseconds)

```AppleScript
-- NARCISSI -------------------------------------------------------------------

-- isDaffodil :: Int -> Int -> Bool
on isDaffodil(e, n)
    set ds to digitList(n)
    (e = length of ds) and (n = powerSum(e, ds))
end isDaffodil

-- digitList :: Int -> [Int]
on digitList(n)
    if n > 0 then
        {n mod 10} & digitList(n div 10)
    else
        {}
    end if
end digitList

-- powerSum :: Int -> [Int] -> Int
on powerSum(e, ns)
    script
        on |λ|(a, x)
            a + x ^ e
        end |λ|
    end script

    foldl(result, 0, ns) as integer
end powerSum

-- narcissiOfLength :: Int -> [Int]
on narcissiOfLength(nDigits)
    script nthPower
        on |λ|(x)
            {x, x ^ nDigits as integer}
        end |λ|
    end script
    set powers to map(nthPower, enumFromTo(0, 9))

    script combn
        on digitTree(n, parents)
            if n > 0 then
                if parents ≠ {} then
                    script nextLayer
                        on |λ|(pair)
                            set {digit, intSum} to pair
                            script addPower
                                on |λ|(dp)
                                    set {d, p} to dp
                                    {d, p + intSum}
                                end |λ|
                            end script

                            map(addPower, items 1 thru (digit + 1) of powers)
                        end |λ|
                    end script

                    set nodes to concatMap(nextLayer, parents)
                else
                    set nodes to powers
                end if
                digitTree(n - 1, nodes)
            else
                script
                    on |λ|(pair)
                        isDaffodil(nDigits, item 2 of pair)
                    end |λ|
                end script

                filter(result, parents)
            end if
        end digitTree
    end script

    script snd
        on |λ|(ab)
            item 2 of ab
        end |λ|
    end script
    map(snd, combn's digitTree(nDigits, {}))
end narcissiOfLength


-- TEST -----------------------------------------------------------------------
on run

    {0} & concatMap(narcissiOfLength, enumFromTo(1, 5))
    -- 4 seconds, 20 narcissi

    -- {0} & concatMap(narcissiOfLength, enumFromTo(1, 6))
    -- 103 seconds, 21 narcissi

    -- {0} & concatMap(narcissiOfLength, enumFromTo(1, 7))
    -- 13.75 minutes, 25 narcissi

end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

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

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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
```

{{Out}}

```AppleScript
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315}
```



## AutoHotkey


```AutoHotkey

#NoEnv ; Do not try to use environment variables
SetBatchLines, -1 ; Execute as quickly as you can

StartCount := A_TickCount
Narc := Narc(25)
Elapsed := A_TickCount - StartCount

MsgBox, Finished in %Elapsed%ms`n%Narc%
return

Narc(m)
{
	Found := 0, Lower := 0
	Progress, B2
	Loop
	{
		Max := 10 ** Digits:=A_Index
		Loop, 10
			Index := A_Index-1, Powers%Index% := Index**Digits
		While Lower < Max
		{
			Sum := 0
			Loop, Parse, Lower
				Sum += Powers%A_LoopField%
			Loop, 10
			{

				if (Lower + (Index := A_Index-1) == Sum + Powers%Index%)
				{
					Out .= Lower+Index . (Mod(++Found,5) ? ", " : "`n")
					Progress, % Found/M*100
					if (Found >= m)
					{
						Progress, Off
						return Out
					}
				}
			}
			Lower += 10
		}
	}
}

```


{{out}}

```txt

Finished in 17690ms
0, 1, 2, 3, 4
5, 6, 7, 8, 9
153, 370, 371, 407, 1634
8208, 9474, 54748, 92727, 93084
548834, 1741725, 4210818, 9800817, 9926315

```


This is a derivative of the python example, but modified for speed reasons.

Instead of summing all the powers of all the numbers at once, we sum the powers for this multiple of 10, then check each number 0 through 9 at once before summing the next multiple of 10. This way, we don't have to calculate the sum of 174172_ for every number 1741720 through 1741729.


## AWK


```AWK

# syntax: GAWK -f NARCISSISTIC_DECIMAL_NUMBER.AWK
BEGIN {
    for (n=0;;n++) {
      leng = length(n)
      sum = 0
      for (i=1; i<=leng; i++) {
        c = substr(n,i,1)
        sum += c ^ leng
      }
      if (n == sum) {
        printf("%d ",n)
        if (++count == 25) { break }
      }
    }
    exit(0)
}

```

<p>output:</p>

```txt

0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## Befunge


This can take several minutes to complete in most interpreters, so it's probably best to use a compiler if you want to see the full sequence.


```befunge
p55*\>:>:>:55+%\55+/00gvv_@
>1>+>^v\_^#!:<p01p00:+1<>\>
>#-_>\>20p110g>\20g*\v>1-v|
^!p00:-1g00+$_^#!:<-1<^\.:<
```


{{out}}


```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```



## C

For a much longer but faster solution, see [[Narcissistic decimal number/C]].

The following prints the first 25 numbers, though not in order...

```c
#include <stdio.h>
#include <gmp.h>

#define MAX_LEN 81

mpz_t power[10];
mpz_t dsum[MAX_LEN + 1];
int cnt[10], len;

void check_perm(void)
{
	char s[MAX_LEN + 1];
	int i, c, out[10] = { 0 };

	mpz_get_str(s, 10, dsum[0]);
	for (i = 0; s[i]; i++) {
		c = s[i]-'0';
		if (++out[c] > cnt[c]) return;
	}

	if (i == len)
		gmp_printf(" %Zd", dsum[0]);
}

void narc_(int pos, int d)
{
	if (!pos) {
		check_perm();
		return;
	}

	do {
		mpz_add(dsum[pos-1], dsum[pos], power[d]);
		++cnt[d];
		narc_(pos - 1, d);
		--cnt[d];
	} while (d--);
}

void narc(int n)
{
	int i;
	len = n;
	for (i = 0; i < 10; i++)
		mpz_ui_pow_ui(power[i], i, n);

	mpz_init_set_ui(dsum[n], 0);

	printf("length %d:", n);
	narc_(n, 9);
	putchar('\n');
}

int main(void)
{
	int i;

	for (i = 0; i <= 10; i++)
		mpz_init(power[i]);
	for (i = 1; i <= MAX_LEN; i++) narc(i);

	return 0;
}
```

{{out}}

```txt

length 1: 9 8 7 6 5 4 3 2 1 0
length 2:
length 3: 407 371 370 153
length 4: 9474 8208 1634
length 5: 93084 92727 54748
length 6: 548834
length 7: 9926315 9800817 4210818 1741725
length 8: 88593477 24678051 24678050
length 9: 912985153 534494836 472335975 146511208
length 10: 4679307774
length 11: 94204591914 82693916578 49388550606 44708635679 42678290603 40028394225 32164049651 32164049650
length 12:
length 13:
length 14: 28116440335967
length 15:
length 16: 4338281769391371 4338281769391370
length 17: 35875699062250035 35641594208964132 21897142587612075
length 18:
^C

```



## C++


```cpp

#include <iostream>
#include <vector>
using namespace std;
typedef unsigned int uint;

class NarcissisticDecs
{
public:
    void makeList( int mx )
    {
	uint st = 0, tl; int pwr = 0, len;
        while( narc.size() < mx )
	{
	    len = getDigs( st );
	    if( pwr != len )
	    {
		pwr = len;
		fillPower( pwr );
	    }
            tl = 0;
	    for( int i = 1; i < 10; i++ )
		tl += static_cast<uint>( powr[i] * digs[i] );

	    if( tl == st ) narc.push_back( st );
	    st++;
	}
    }

    void display()
    {
	for( vector<uint>::iterator i = narc.begin(); i != narc.end(); i++ )
	    cout << *i << " ";
	cout << "\n\n";
    }

private:
    int getDigs( uint st )
    {
	memset( digs, 0, 10 * sizeof( int ) );
	int r = 0;
	while( st )
	{
	    digs[st % 10]++;
	    st /= 10;
	    r++;
	}
        return r;
    }

    void fillPower( int z )
    {
	for( int i = 1; i < 10; i++ )
	    powr[i] = pow( static_cast<float>( i ), z );
    }

    vector<uint> narc;
    uint powr[10];
    int digs[10];
};

int main( int argc, char* argv[] )
{
    NarcissisticDecs n;
    n.makeList( 25 );
    n.display();
    return system( "pause" );
}

```

{{out}}

```txt

0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```


=={{header|C sharp|C#}}==

```csharp

using System;

namespace Narcissistic
{
    class Narcissistic
    {
        public bool isNarcissistic(int z)
        {
            if (z < 0) return false;
            string n = z.ToString();
            int t = 0, l = n.Length;
            foreach (char c in n)
                t += Convert.ToInt32(Math.Pow(Convert.ToDouble(c - 48), l));

            return t == z;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Narcissistic n = new Narcissistic();
            int c = 0, x = 0;
            while (c < 25)
            {
                if (n.isNarcissistic(x))
                {
                    if (c % 5 == 0) Console.WriteLine();
                    Console.Write("{0,7} ", x);
                    c++;
                }
                x++;
            }
            Console.WriteLine("\n\nPress any key to continue...");
            Console.ReadKey();
        }
    }
}

```

{{out}}

```txt

      0       1       2       3       4
      5       6       7       8       9
    153     370     371     407    1634
   8208    9474   54748   92727   93084
 548834 1741725 4210818 9800817 9926315

```


### or


```csharp

//Narcissistic numbers: Nigel Galloway: February 17th., 2015
using System;
using System.Collections.Generic;
using System.Linq;

namespace RC {
    public static class NumberEx {
        public static IEnumerable<int> Digits(this int n) {
            List<int> digits = new List<int>();
            while (n > 0) {
                digits.Add(n % 10);
                n /= 10;
            }
            return digits.AsEnumerable();
        }
    }

    class Program {
        static void Main(string[] args) {
            foreach (int N in Enumerable.Range(0, Int32.MaxValue).Where(k => {
                var digits = k.Digits();
                return digits.Sum(x => Math.Pow(x, digits.Count())) == k;
            }).Take(25)) {
                System.Console.WriteLine(N);
            }
        }
    }
}

```

{{out}}

```txt

0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315

```



### All 89 terms

{{libheader|System.Numerics}}
{{trans|FreeBASIC}} (FreeBASIC, GMP version)<br/>Why stop at 25?  Even using '''ulong''' instead of '''int''' only gets one to the 44th item.  The 89th (last) item has 39 digits, which '''BigInteger''' easily handles.  Of course, the BigInteger implementation is slower than native data types.  But one can compensate a bit by calculating in parallel.  Not bad, it can get all 89 items in under 7 1/2 minutes on a core i7.  The calculation to the 25th item takes a fraction of a second.  The calculation for all items up to 25 digits long (67th item) takes about half a minute with sequential processing and less than a quarter of a minute using parallel processing.  Note that parallel execution involves some overhead, and isn't a time improvement unless computing around 15 digits or more.  This program can test all numbers up to 61 digits in under half an hour, of course the highest item found has only 39 digits.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Numerics;

static class Program
{
    public static void nar(int max, bool only1 = false)
    {
        int n, n1, n2, n3, n4, n5, n6, n7, n8, n9;
        int[] d;                                       // digits tally
        char [] bs;                                    // BigInteger String
        List<BigInteger> res = new List<BigInteger>(); // per n digits results
        BigInteger[,] p = new BigInteger[10, max + 1]; // powers array

        // BigIntegers for intermediate results
        BigInteger x2, x3, x4, x5, x6, x7, x8, x9;

        for (n = only1 ? max : 1; n <= max; n++) // main loop
        {
            for (int i = 1; i <= 9; i++) // init powers array for this n
            {
                p[i, 1] = BigInteger.Pow(i, n);
                for (int j = 2; j <= n; j++) p[i, j] = p[i, 1] * j;
            }
            for (n9 = n; n9 >= 0; n9--) // nested loops...
            {
                x9 = p[9, n9];
                for (n8 = n - n9; n8 >= 0; n8--)
                {
                    x8 = x9 + p[8, n8];
                    for (n7 = n - n9 - n8; n7 >= 0; n7--)
                    {
                        x7 = x8 + p[7, n7];
                        for (n6 = n - n9 - n8 - n7; n6 >= 0; n6--)
                        {
                            x6 = x7 + p[6, n6];
                            for (n5 = n - n9 - n8 - n7 - n6; n5 >= 0; n5--)
                            {
                                x5 = x6 + p[5, n5];
                                for (n4 = n - n9 - n8 - n7 - n6 - n5; n4 >= 0; n4--)
                                {
                                    x4 = x5 + p[4, n4];
                                    for (n3 = n - n9 - n8 - n7 - n6 - n5 - n4; n3 >= 0; n3--)
                                    {
                                        x3 = x4 + p[3, n3];
                                        for (n2 = n - n9 - n8 - n7 - n6 - n5 - n4 - n3; n2 >= 0; n2--)
                                        {
                                            x2 = x3 + p[2, n2];
                                            for (n1 = n - n9 - n8 - n7 - n6 - n5 - n4 - n3 - n2; n1 >= 0; n1--)
                                            {
                                                bs = (x2 + n1).ToString().ToCharArray();
                                                switch (bs.Length.CompareTo(n))
                                                { // Since all the for/next loops step down, when the digit count
                                                  // becomes smaller than n, it's time to try the next n value.
                                                    case -1: { goto Next_n; }
                                                    case 0:
                                                        {
                                                            d = new int[10]; foreach (char c in bs) d[c - 48] += 1;
                                                            if (n9 == d[9] && n8 == d[8] && n7 == d[7] &&
                                                                n6 == d[6] && n5 == d[5] && n4 == d[4] &&
                                                                n3 == d[3] && n2 == d[2] && n1 == d[1] &&
                                                                n - n9 - n8 - n7 - n6 - n5 - n4 - n3 - n2 - n1 == d[0])
                                                                res.Add(BigInteger.Parse(new string(bs)));
                                                            break;
                                                        }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
Next_n:     if (only1) {
                Console.Write("{0} ", n); lock (resu) resu.AddRange(res); return;
            } else {
                res.Sort(); Console.WriteLine("{2,3} {0,3}: {1}",
                Math.Ceiling((DateTime.Now - st).TotalSeconds), string.Join(" ", res), n); res.Clear();
            }
        }
    }

    private static DateTime st = default(DateTime);
    private static List<BigInteger> resu = new List<BigInteger>();
    private static bool para = true; // parallel (default) or sequential calcualtion
    private static int lim = 7;  // this is the number of digits to calcualate, not the nth entry.
                                 // for up to the 25th item, use lim = 7 digits.
                                 // for all 89 items, use lim = 39 digits.
    public static void Main(string[] args)
    {
        if (args.Count() > 0)
        {
            int t = lim; int.TryParse(args[0], out t);
            if (t < 1) t = 1;   // number of digits must be > 0
            if (t > 61) t = 61; // no point when lim * math.pow(9, lim) < math.pow(10, lim - 1)
            lim = t;
            // default is parallel, will do sequential when any 2nd command line parameter is present.
            para = !(args.Count() > 1);
        }
        st = DateTime.Now;
        if (para)
        {
            Console.Write("Calculations in parallel... "); // starts the bigger ones first
            Parallel.ForEach(Enumerable.Range(1, lim).Reverse().ToArray(), n => { nar(n, true); } );
            resu.Sort(); int[] g = Enumerable.Range(1, resu.Count).ToArray();
            var both = g.Zip(resu, (a, b) => a.ToString() + " " + b.ToString());
            Console.WriteLine("\n{0}", string.Join("\n", both));
        }
        else { Console.WriteLine("Sequential calculations:"); nar(lim); }
        Console.WriteLine("Total elasped: {0} seconds", (DateTime.Now - st).TotalSeconds);
        if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
    }
}

```

{{out}}(with command line parameter = "39")
<pre style="height:30ex;overflow:scroll">Calculations in parallel... 7 6 5 4 3 2 1 11 10 9 8 15 14 13 12 19 18 17 16 23 22 20 21 26 27 25 24 30 31 29 34 28 35 38 33 39 32 37 36
1 0
2 1
3 2
4 3
5 4
6 5
7 6
8 7
9 8
10 9
11 153
12 370
13 371
14 407
15 1634
16 8208
17 9474
18 54748
19 92727
20 93084
21 548834
22 1741725
23 4210818
24 9800817
25 9926315
26 24678050
27 24678051
28 88593477
29 146511208
30 472335975
31 534494836
32 912985153
33 4679307774
34 32164049650
35 32164049651
36 40028394225
37 42678290603
38 44708635679
39 49388550606
40 82693916578
41 94204591914
42 28116440335967
43 4338281769391370
44 4338281769391371
45 21897142587612075
46 35641594208964132
47 35875699062250035
48 1517841543307505039
49 3289582984443187032
50 4498128791164624869
51 4929273885928088826
52 63105425988599693916
53 128468643043731391252
54 449177399146038697307
55 21887696841122916288858
56 27879694893054074471405
57 27907865009977052567814
58 28361281321319229463398
59 35452590104031691935943
60 174088005938065293023722
61 188451485447897896036875
62 239313664430041569350093
63 1550475334214501539088894
64 1553242162893771850669378
65 3706907995955475988644380
66 3706907995955475988644381
67 4422095118095899619457938
68 121204998563613372405438066
69 121270696006801314328439376
70 128851796696487777842012787
71 174650464499531377631639254
72 177265453171792792366489765
73 14607640612971980372614873089
74 19008174136254279995012734740
75 19008174136254279995012734741
76 23866716435523975980390369295
77 1145037275765491025924292050346
78 1927890457142960697580636236639
79 2309092682616190307509695338915
80 17333509997782249308725103962772
81 186709961001538790100634132976990
82 186709961001538790100634132976991
83 1122763285329372541592822900204593
84 12639369517103790328947807201478392
85 12679937780272278566303885594196922
86 1219167219625434121569735803609966019
87 12815792078366059955099770545296129367
88 115132219018763992565095597973971522400
89 115132219018763992565095597973971522401
Total elasped: 443.8791684 seconds
```


(without any command line parameters)
<pre style="height:30ex;overflow:scroll">Calculations in parallel... 1 3 2 4 5 7 6
1 0
2 1
3 2
4 3
5 4
6 5
7 6
8 7
9 8
10 9
11 153
12 370
13 371
14 407
15 1634
16 8208
17 9474
18 54748
19 92727
20 93084
21 548834
22 1741725
23 4210818
24 9800817
25 9926315
Total elasped: 0.0279259 seconds
```


(with command line parameters= "7 x")
<pre style="height:30ex;overflow:scroll">Sequential calculations:
  1   1: 0 1 2 3 4 5 6 7 8 9
  2   1:
  3   1: 153 370 371 407
  4   1: 1634 8208 9474
  5   1: 54748 92727 93084
  6   1: 548834
  7   1: 1741725 4210818 9800817 9926315
Total elasped: 0.0175957 seconds
```



(with command line parameters= "25 x")
<pre style="height:30ex;overflow:scroll">Sequential calculations:
  1   1: 0 1 2 3 4 5 6 7 8 9
  2   1:
  3   1: 153 370 371 407
  4   1: 1634 8208 9474
  5   1: 54748 92727 93084
  6   1: 548834
  7   1: 1741725 4210818 9800817 9926315
  8   1: 24678050 24678051 88593477
  9   1: 146511208 472335975 534494836 912985153
 10   1: 4679307774
 11   1: 32164049650 32164049651 40028394225 42678290603 44708635679 49388550606 82693916578 94204591914
 12   1:
 13   1:
 14   1: 28116440335967
 15   1:
 16   1: 4338281769391370 4338281769391371
 17   2: 21897142587612075 35641594208964132 35875699062250035
 18   3:
 19   4: 1517841543307505039 3289582984443187032 4498128791164624869 4929273885928088826
 20   6: 63105425988599693916
 21   9: 128468643043731391252 449177399146038697307
 22  12:
 23  17: 21887696841122916288858 27879694893054074471405 27907865009977052567814 28361281321319229463398 35452590104031691935943
 24  23: 174088005938065293023722 188451485447897896036875 239313664430041569350093
 25  31: 1550475334214501539088894 1553242162893771850669378 3706907995955475988644380 3706907995955475988644381 4422095118095899619457938
Total elasped: 30.5658944 seconds
```



## Clojure

Find N first Narcissistic numbers.

```Clojure

(ns narcissistic.core
  (:require [clojure.math.numeric-tower :as math]))

(defn digits [n] ;; digits of a number.
  (->> n str (map (comp read-string str))))

(defn narcissistic? [n] ;; True if the number is a Narcissistic one.
  (let [d (digits n)
        s (count d)]
    (= n (reduce + (map #(math/expt % s) d)))))

(defn firstNnarc [n] ;;list of the first "n" Narcissistic numbers.
  (take n (filter narcissistic? (range))))

```

{{out}}
by Average-user

```txt

(time (doall (firstNnarc 25)))
"Elapsed time: 186430.429966 msecs"
(0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315)

```



## COBOL


```COBOL

       PROGRAM-ID. NARCISSIST-NUMS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01 num-length PIC 9(2) value 0.
           01 in-sum PIC  9(9) value 0.
           01 counter PIC  9(9) value 0.
           01 current-number PIC  9(9) value 0.
           01 narcissist PIC Z(9).
           01 temp PIC  9(9) value 0.
           01 modulo PIC  9(9) value 0.
           01 answer PIC  9 .

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "the first 20 narcissist numbers:" .

           MOVE 20 TO counter.
           PERFORM UNTIL counter=0

               PERFORM 000-NARCISSIST-PARA

                   IF answer = 1
                       SUBTRACT 1 from counter
                       GIVING counter
                       MOVE current-number TO narcissist
                       DISPLAY narcissist
                   END-IF

                   ADD 1 TO current-number

               END-PERFORM

            STOP RUN.

       000-NARCISSIST-PARA.

             MOVE ZERO TO in-sum.
             MOVE current-number TO temp.
             COMPUTE num-length =1+  FUNCTION Log10(temp)

             PERFORM  UNTIL temp=0

                  DIVIDE temp BY 10 GIVING temp
                            REMAINDER  modulo

                  COMPUTE modulo=modulo**num-length
                  ADD modulo to in-sum GIVING in-sum

            END-PERFORM.

               IF current-number=in-sum
                   MOVE 1 TO answer
                   ELSE MOVE 0 TO answer
               END-IF.

       END PROGRAM NARCISSIST-NUMS.


```


{{out}}

```txt

the first 20 narcissist numbers:
        0
        1
        2
        3
        4
        5
        6
        7
        8
        9
      153
      370
      371
      407
     1634
     8208
     9474
    54748
    92727
    93084


```



## Common Lisp


```lisp

(defun integer-to-list (n)
  (map 'list #'digit-char-p (prin1-to-string n)))

(defun narcissisticp (n)
  (let* ((lst (integer-to-list n))
         (e (length lst)))
        (= n
	   (reduce #'+ (mapcar (lambda (x) (expt x e)) lst)))))

(defun start ()
  (loop for c from 0
        while (< narcissistic 25)
        counting (narcissisticp c) into narcissistic
        do (if (narcissisticp c) (print c))))

```

{{out}}

```txt

CL-USER> (start)

0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315
NIL

```



## D


### Simple Version


```d
void main() {
    import std.stdio, std.algorithm, std.conv, std.range;

    immutable isNarcissistic = (in uint n) pure @safe =>
        n.text.map!(d => (d - '0') ^^ n.text.length).sum == n;
    writefln("%(%(%d %)\n%)",
             uint.max.iota.filter!isNarcissistic.take(25).chunks(5));
}
```

{{out}}

```txt
0 1 2 3 4
5 6 7 8 9
153 370 371 407 1634
8208 9474 54748 92727 93084
548834 1741725 4210818 9800817 9926315
```



### Fast Version

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.range, std.array;

uint[] narcissists(in uint m) pure nothrow @safe {
    typeof(return) result;

    foreach (immutable uint digits; 0 .. 10) {
        const digitPowers = 10.iota.map!(i => i ^^ digits).array;

        foreach (immutable uint n; 10 ^^ (digits - 1) .. 10 ^^ digits) {
            uint digitPSum, div = n;
            while (div) {
                digitPSum += digitPowers[div % 10];
                div /= 10;
            }

            if (n == digitPSum) {
                result ~= n;
                if (result.length >= m)
                    return result;
            }
        }
    }

    assert(0);
}

void main() {
    writefln("%(%(%d %)\n%)", 25.narcissists.chunks(5));
}
```

With LDC2 compiler prints the same output in less than 0.3 seconds.


### Faster Version

{{trans|C}}

```d
import std.stdio, std.bigint, std.conv;

struct Narcissistics(TNum, uint maxLen) {
    TNum[10] power;
    TNum[maxLen + 1] dsum;
    uint[10] count;
    uint len;

    void checkPerm() const {
        uint[10] mout;

        immutable s = dsum[0].text;
        foreach (immutable d; s) {
            immutable c = d - '0';
            if (++mout[c] > count[c])
                return;
        }

        if (s.length == len)
            writef(" %d", dsum[0]);
    }

    void narc2(in uint pos, uint d) {
        if (!pos) {
            checkPerm;
            return;
        }

        do {
            dsum[pos - 1] = dsum[pos] + power[d];
            count[d]++;
            narc2(pos - 1, d);
            count[d]--;
        } while (d--);
    }

    void show(in uint n) {
        len = n;
        foreach (immutable i, ref p; power)
            p = TNum(i) ^^ n;
        dsum[n] = 0;
        writef("length %d:", n);
        narc2(n, 9);
        writeln;
    }
}

void main() {
    enum maxLength = 16;
    Narcissistics!(ulong, maxLength) narc;
    //Narcissistics!(BigInt, maxLength) narc; // For larger numbers.
    foreach (immutable i; 1 .. maxLength + 1)
        narc.show(i);
}
```

{{out}}

```txt
length 1: 9 8 7 6 5 4 3 2 1 0
length 2:
length 3: 407 371 370 153
length 4: 9474 8208 1634
length 5: 93084 92727 54748
length 6: 548834
length 7: 9926315 9800817 4210818 1741725
length 8: 88593477 24678051 24678050
length 9: 912985153 534494836 472335975 146511208
length 10: 4679307774
length 11: 94204591914 82693916578 49388550606 44708635679 42678290603 40028394225 32164049651 32164049650
length 12:
length 13:
length 14: 28116440335967
length 15:
length 16: 4338281769391371 4338281769391370
```

With LDC2 compiler and maxLength=16 the run-time is about 0.64 seconds.


## Elixir

{{trans|D}}

```elixir
defmodule RC do
  def narcissistic(m) do
    Enum.reduce(1..10, [0], fn digits,acc ->
      digitPowers = List.to_tuple(for i <- 0..9, do: power(i, digits))
      Enum.reduce(power(10, digits-1) .. power(10, digits)-1, acc, fn n,result ->
        sum = divsum(n, digitPowers, 0)
        if n == sum do
          if length(result) == m-1, do: throw Enum.reverse(result, [n])
          [n | result]
        else
          result
        end
      end)
    end)
  end

  defp divsum(0, _, sum), do: sum
  defp divsum(n, digitPowers, sum) do
    divsum(div(n,10), digitPowers, sum+elem(digitPowers,rem(n,10)))
  end

  defp power(n, m), do: power(n, m, 1)

  defp power(_, 0, pow), do: pow
  defp power(n, m, pow), do: power(n, m-1, pow*n)
end

try do
  RC.narcissistic(25)
catch
  x -> IO.inspect x
end
```


{{out}}

```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748,
 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315]

```



## ERRE


```ERRE
PROGRAM NARCISISTIC

!$DOUBLE

BEGIN
    N=0
    LOOP
      C$=MID$(STR$(N),2)
      LENG=LEN(C$)
      SUM=0
      FOR I=1 TO LENG DO
        C=VAL(MID$(C$,I,1))
        SUM+=C^LENG
      END FOR
      IF N=SUM THEN
        PRINT(N;)
        COUNT=COUNT+1
        EXIT IF COUNT=25
      END IF
      N=N+1
    END LOOP
END PROGRAM
```

Output

```txt

 0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834
 1741725 4210818  9800817 9926315

```


=={{header|F Sharp|F#}}==

```fsharp

//Naïve solution of Narcissitic number: Nigel Galloway - Febryary 18th., 2015
open System
let rec _Digits (n,g) = if n < 10 then n::g else _Digits(n/10,n%10::g)

seq{0 .. Int32.MaxValue} |> Seq.filter (fun n ->
  let d = _Digits (n, [])
  d |> List.fold (fun a l -> a + int ((float l) ** (float (List.length d)))) 0 = n) |> Seq.take(25) |> Seq.iter (printfn "%A")

```

{{out}}

```txt

0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315

```



## Factor

<lang>USING: io kernel lists lists.lazy math math.functions
math.text.utils prettyprint sequences ;
IN: rosetta-code.narcissistic-decimal-number

: digit-count ( n -- count ) log10 floor >integer 1 + ;

: narcissist? ( n -- ? ) dup [ 1 digit-groups ]
    [ digit-count [ ^ ] curry ] bi map-sum = ;

: first25 ( -- seq ) 25 0 lfrom [ narcissist? ] lfilter
    ltake list>array ;

: main ( -- ) first25 [ pprint bl ] each ;

MAIN: main
```

{{out}}

```txt

0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## Forth

{{works with|GNU Forth|0.7.0}}

```forth

 : dig.num                           \ returns input number and the number of its digits ( n -- n n1 )
	 dup
	 0 swap
     begin
	 swap 1 + swap
	 dup 10 >= while
	 10 /
     repeat
	 drop ;

 : zero.divmod                         \ /mod that returns zero if number is zero
	   dup
	   0 = if drop 0 else
           /mod
	   then ;

 : zero.div                            \ division that returns zero if divisor is zero
	 dup
	 0 = if drop else
         /
	 then ;

 : next.last
	 depth 2 - roll ;                 \ gets next-to-last number from the stack

 : ten.to			          \ ( n -- 10^n ) returns 1 for zero and negative
	  dup 0 <= if drop 1 else
	  dup 1 = if drop 10 else
	  10 swap
	  1 do
	  10 *
	  loop then then ;

 : split.div                                        \ returns input number and its digits ( n -- n n1 n2 n3....)
	   dup 10 < if dup  else		    \ duplicates single digit numbers
	   dig.num				    \ provides number of digits
	   swap dup rot dup 1 - ten.to swap         \ stack juggling, ten raised to number of digits - 1...
	   1 do                                     \ ... is the needed divisor, counter on top and ...
	   dup rot swap zero.divmod swap rot 10 /   \ ...division loop
	   loop drop then ;

 : to.pow                           \ nth power of positive numbers ( n m -- n^m )
	 swap dup rot
	 dup 0 <= if
	 2drop drop 1
	 else
	 0 do
	 swap dup rot *
	 loop
	 swap zero.div
	 then ;

 : num.pow                        \ raises each digit to the power of (number of digits)
	 depth 1 - 0 do
	 next.last depth 1 - to.pow
	 loop ;

 : add.num
	 depth 2 > if
	 begin
	 +
	 depth 2 = until then ;

 : narc.check
	split.div
        num.pow
        add.num ;

 : narc.num 0 { a b }              \  ( m -- n1 n2 n3 ... nm )
	 page                      \ displays m narcissistic decimal numbers...
	 999999999 0 do            \ ...beginning with 0
	 a b = if leave then
	 i narc.check = if
	 i . cr b 1 + to b
	 then
	 loop
	 ;

 25 narc.num

```

{{out}}

```txt

0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315
 ok

```



## FreeBASIC


### Simple Version


```FreeBASIC
' normal version: 14-03-2017
' compile with: fbc -s console
' can go up to 18 digits (ulongint is 64bit), above 18 overflow will occur

Dim As Integer n, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, a, b
Dim As Integer d()
Dim As ULongInt d2pow(0 To 9) = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
Dim As ULongInt x
Dim As String str_x

For n = 1 To 7
  For n9 = n To 0 Step -1
    For n8 = n-n9 To 0 Step -1
      For n7 = n-n9-n8 To 0 Step -1
        For n6 = n-n9-n8-n7 To 0 Step -1
          For n5 = n-n9-n8-n7-n6 To 0 Step -1
            For n4 = n-n9-n8-n7-n6-n5 To 0 Step -1
              For n3 = n-n9-n8-n7-n6-n5-n4 To 0 Step -1
                For n2 = n-n9-n8-n7-n6-n5-n4-n3 To 0 Step -1
                  For n1 = n-n9-n8-n7-n6-n5-n4-n3-n2 To 0 Step -1
                    n0 = n-n9-n8-n7-n6-n5-n4-n3-n2-n1

                    x = n1 + n2*d2pow(2) + n3*d2pow(3) + n4*d2pow(4) + n5*d2pow(5)_
                           + n6*d2pow(6) + n7*d2pow(7) + n8*d2pow(8) + n9*d2pow(9)

                    str_x = Str(x)
                    If Len(str_x) = n Then

                      ReDim d(10)
                      For a = 0 To n-1
                        d(Str_x[a]- Asc("0")) += 1
                      Next a

                      If n0 = d(0) AndAlso n1 = d(1) AndAlso n2 = d(2) AndAlso n3 = d(3)_
                                   AndAlso n4 = d(4) AndAlso n5 = d(5) AndAlso n6 = d(6)_
                                   AndAlso n7 = d(7) AndAlso n8 = d(8) AndAlso n9 = d(9) Then
                        Print x
                      End If
                    End If

                  Next n1
                Next n2
              Next n3
            Next n4
          Next n5
        Next n6
      Next n7
    Next n8
  Next n9

  For a As Integer = 2 To 9
    d2pow(a) = d2pow(a) * a
  Next a

Next n

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
9
8
7
6
5
4
3
2
1
0
407
371
370
153
9474
8208
1634
93084
92727
54748
548834
9926315
9800817
4210818
1741725
```



### GMP Version


```txt
It takes about 35 min. to find all 88 numbers (39 digits).
To go all the way it takes about 2 hours.
```


```FreeBASIC
' gmp version: 17-06-2015
' uses gmp
' compile with: fbc -s console

#Include Once "gmp.bi"
' change the number after max for the maximum n-digits you want (2 to 61)
#Define max 61

Dim As Integer n, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9
Dim As Integer i, j
Dim As UInteger d()
Dim As ZString Ptr gmp_str
gmp_str = Allocate(100)

' create gmp integer array,
Dim d2pow(9, max) As Mpz_ptr
' initialize array and set start value,
For i = 0 To 9
  For j = 0 To max
    d2pow(i, j) = Allocate(Len(__mpz_struct)) : Mpz_init(d2pow(i, j))
  Next j
Next i

' gmp integers for to hold intermediate result
Dim As Mpz_ptr x1 = Allocate(Len(__mpz_struct)) : Mpz_init(x1)
Dim As Mpz_ptr x2 = Allocate(Len(__mpz_struct)) : Mpz_init(x2)
Dim As Mpz_ptr x3 = Allocate(Len(__mpz_struct)) : Mpz_init(x3)
Dim As Mpz_ptr x4 = Allocate(Len(__mpz_struct)) : Mpz_init(x4)
Dim As Mpz_ptr x5 = Allocate(Len(__mpz_struct)) : Mpz_init(x5)
Dim As Mpz_ptr x6 = Allocate(Len(__mpz_struct)) : Mpz_init(x6)
Dim As Mpz_ptr x7 = Allocate(Len(__mpz_struct)) : Mpz_init(x7)
Dim As Mpz_ptr x8 = Allocate(Len(__mpz_struct)) : Mpz_init(x8)

For n = 1 To max

  For i = 1 To 9
    'Mpz_set_ui(d2pow(i,0), 0)
    Mpz_ui_pow_ui(d2pow(i,1), i, n)
    For j = 2 To n
      Mpz_mul_ui(d2pow(i, j), d2pow(i, 1), j)
    Next j
  Next i

  For n9 = n To 0 Step -1
    For n8 = n-n9 To 0 Step -1
      Mpz_add(x8, d2pow(9, n9), d2pow(8, n8))
      For n7 = n-n9-n8 To 0 Step -1
        Mpz_add(x7, x8, d2pow(7, n7))
        For n6 = n-n9-n8-n7 To 0 Step -1
          Mpz_add(x6, x7, d2pow(6, n6))
          For n5 = n-n9-n8-n7-n6 To 0 Step -1
            Mpz_add(x5, x6, d2pow(5, n5))
            For n4 = n-n9-n8-n7-n6-n5 To 0 Step -1
              Mpz_add(x4, x5, d2pow(4, n4))
              For n3 = n-n9-n8-n7-n6-n5-n4 To 0 Step -1
                Mpz_add(x3, x4, d2pow(3, n3))
                For n2 = n-n9-n8-n7-n6-n5-n4-n3 To 0 Step -1
                  Mpz_add(x2, x3, d2pow(2, n2))
                  For n1 = n-n9-n8-n7-n6-n5-n4-n3-n2 To 0 Step -1
                    Mpz_add_ui(x1, x2, n1)
                    n0 = n-n9-n8-n7-n6-n5-n4-n3-n2-n1

                    Mpz_get_str(gmp_str, 10, x1)

                    If Len(*gmp_str) = n Then
                      ReDim d(10)

                      For i = 0 To n-1
                        d(gmp_str[i] - Asc("0")) += 1
                      Next i

                      If n9 = d(9) AndAlso n8 = d(8) AndAlso n7 = d(7) AndAlso n6 = d(6)_
                                   AndAlso n5 = d(5) AndAlso n4 = d(4) AndAlso n3 = d(3)_
                                   AndAlso n2 = d(2) AndAlso n1 = d(1) AndAlso n0 = d(0) Then
                        Print *gmp_str
                      End If
                    ElseIf Len(*gmp_str) < n Then
                      ' all for next loops have a negative step value
                      ' if len(str_x) becomes smaller then n it's time to try the next n value
                      ' GoTo label1   ' old school BASIC
                      ' prefered FreeBASIC style
                      Exit   For, For, For, For, For, For, For, For, For
                      ' leave n1,  n2,  n3,  n4,  n5,  n6,  n7,  n8,  n9 loop
                      ' and continue's after next n9
                    End If

                  Next n1
                Next n2
              Next n3
            Next n4
          Next n5
        Next n6
      Next n7
    Next n8
  Next n9
  ' label1:
Next n

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}
Left side: program output, right side: sorted on length, value
<pre style="height:35ex;overflow:scroll">9                                                                                            0
8                                                                                            1
7                                                                                            2
6                                                                                            3
5                                                                                            4
4                                                                                            5
3                                                                                            6
2                                                                                            7
1                                                                                            8
0                                                                                            9
407                                                                                        153
371                                                                                        370
370                                                                                        371
153                                                                                        407
9474                                                                                      1634
8208                                                                                      8208
1634                                                                                      9474
93084                                                                                    54748
92727                                                                                    92727
54748                                                                                    93084
548834                                                                                  548834
9926315                                                                                1741725
9800817                                                                                4210818
4210818                                                                                9800817
1741725                                                                                9926315
88593477                                                                              24678050
24678051                                                                              24678051
24678050                                                                              88593477
912985153                                                                            146511208
534494836                                                                            472335975
472335975                                                                            534494836
146511208                                                                            912985153
4679307774                                                                          4679307774
94204591914                                                                        32164049650
82693916578                                                                        32164049651
49388550606                                                                        40028394225
44708635679                                                                        42678290603
42678290603                                                                        44708635679
40028394225                                                                        49388550606
32164049651                                                                        82693916578
32164049650                                                                        94204591914
28116440335967                                                                  28116440335967
4338281769391371                                                              4338281769391370
4338281769391370                                                              4338281769391371
35875699062250035                                                            21897142587612075
35641594208964132                                                            35641594208964132
21897142587612075                                                            35875699062250035
4929273885928088826                                                        1517841543307505039
4498128791164624869                                                        3289582984443187032
3289582984443187032                                                        4498128791164624869
1517841543307505039                                                        4929273885928088826
63105425988599693916                                                      63105425988599693916
449177399146038697307                                                    128468643043731391252
128468643043731391252                                                    449177399146038697307
35452590104031691935943                                                21887696841122916288858
28361281321319229463398                                                27879694893054074471405
27907865009977052567814                                                27907865009977052567814
27879694893054074471405                                                28361281321319229463398
21887696841122916288858                                                35452590104031691935943
239313664430041569350093                                              174088005938065293023722
188451485447897896036875                                              188451485447897896036875
174088005938065293023722                                              239313664430041569350093
4422095118095899619457938                                            1550475334214501539088894
3706907995955475988644381                                            1553242162893771850669378
3706907995955475988644380                                            3706907995955475988644380
1553242162893771850669378                                            3706907995955475988644381
1550475334214501539088894                                            4422095118095899619457938
177265453171792792366489765                                        121204998563613372405438066
174650464499531377631639254                                        121270696006801314328439376
128851796696487777842012787                                        128851796696487777842012787
121270696006801314328439376                                        174650464499531377631639254
121204998563613372405438066                                        177265453171792792366489765
23866716435523975980390369295                                    14607640612971980372614873089
19008174136254279995012734741                                    19008174136254279995012734740
19008174136254279995012734740                                    19008174136254279995012734741
14607640612971980372614873089                                    23866716435523975980390369295
2309092682616190307509695338915                                1145037275765491025924292050346
1927890457142960697580636236639                                1927890457142960697580636236639
1145037275765491025924292050346                                2309092682616190307509695338915
17333509997782249308725103962772                              17333509997782249308725103962772
186709961001538790100634132976991                            186709961001538790100634132976990
186709961001538790100634132976990                            186709961001538790100634132976991
1122763285329372541592822900204593                          1122763285329372541592822900204593
12679937780272278566303885594196922                        12639369517103790328947807201478392
12639369517103790328947807201478392                        12679937780272278566303885594196922
1219167219625434121569735803609966019                    1219167219625434121569735803609966019
12815792078366059955099770545296129367                  12815792078366059955099770545296129367
115132219018763992565095597973971522401                115132219018763992565095597973971522400
115132219018763992565095597973971522400                115132219018763992565095597973971522401
```



## FunL


```funl
def narcissistic( start ) =
  power = 1
  powers = array( 0..9 )

  def narc( n ) =
    num = n.toString()
    m = num.length()

    if power != m
      power = m
      powers( 0..9 ) = [i^m | i <- 0..9]

    if n == sum( powers(int(d)) | d <- num )
      n # narc( n + 1 )
    else
      narc( n + 1 )

  narc( start )

println( narcissistic(0).take(25) )
```


{{out}}


```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315]

```



## Go

Nothing fancy as it runs in a fraction of a second as-is.

```go
package main

import "fmt"

func narc(n int) []int {
	power := [...]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	limit := 10
	result := make([]int, 0, n)
	for x := 0; len(result) < n; x++ {
		if x >= limit {
			for i := range power {
				power[i] *= i // i^m
			}
			limit *= 10
		}
		sum := 0
		for xx := x; xx > 0; xx /= 10 {
			sum += power[xx%10]
		}
		if sum == x {
			result = append(result, x)
		}
	}
	return result
}

func main() {
	fmt.Println(narc(25))
}
```

{{out}}

```txt

[0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315]

```

=={{header|GW-BASIC}}==
{{trans|FreeBASIC}}
Maximum for N (double) is14 digits, there are no 15 digits numbers

```qbasic
1 DEFINT A-W : DEFDBL X-Z : DIM D(9) : DIM X2(9) : KEY OFF : CLS
2 FOR A = 0 TO 9 : X2(A) = A : NEXT A
3 FOR N = 1 TO 7
4 FOR N9 = N TO 0 STEP -1
5 FOR N8 = N-N9 TO 0 STEP -1
6 FOR N7 = N-N9-N8 TO 0 STEP -1
7 FOR N6 = N-N9-N8-N7 TO 0 STEP -1
8 FOR N5 = N-N9-N8-N7-N6 TO 0 STEP -1
9 FOR N4 = N-N9-N8-N7-N6-N5 TO 0 STEP -1
10 FOR N3 = N-N9-N8-N7-N6-N5-N4 TO 0 STEP -1
11 FOR N2 = N-N9-N8-N7-N6-N5-N4-N3 TO 0 STEP -1
12 FOR N1 = N-N9-N8-N7-N6-N5-N4-N3-N2 TO 0 STEP -1
13 N0 = N-N9-N8-N7-N6-N5-N4-N3-N2-N1
14 X = N1 + N2*X2(2) + N3*X2(3) + N4*X2(4) + N5*X2(5) + N6*X2(6) + N7*X2(7) + N8*X2(8) + N9*X2(9)
15 S$ = MID$(STR$(X),2)
16 IF LEN(S$) < N THEN GOTO 25
17 IF LEN(S$) <> N THEN GOTO 24
18 FOR A = 0 TO 9 : D(A) = 0 : NEXT A
19 FOR A = 0 TO N-1
20 B = ASC(MID$(S$,A+1,1))-48
21 D(B) = D(B) + 1
22 NEXT A
23 IF N0 = D(0) AND N1 = D(1) AND N2 = D(2) AND N3 = D(3) AND N4 = D(4) AND N5 = D(5) AND N6 = D(6) AND N7 = D(7) AND N8 = D(8) AND N9 = D(9) THEN PRINT X,
24 NEXT N1 : NEXT N2 : NEXT N3 : NEXT N4 : NEXT N5 : NEXT N6 : NEXT N7 : NEXT N8 : NEXT N9
25 FOR A = 2 TO 9
26 X2(A) = X2(A) * A
27 NEXT A
28 NEXT N
29 PRINT
30 PRINT "done"
31 END
```

{{out}}

```txt
 9             8             7             6             5
 4             3             2             1             0
 407           371           370           153           9474
 8208          1634          93084         92727         54748
 548834        9926315       9800817       4210818       1741725
```



## Haskell

===Exhaustive search (integer series)===

```Haskell
import Data.Char (digitToInt)

isNarcissistic :: Int -> Bool
isNarcissistic n = (sum ((^ digitCount) <$> digits) ==) n
  where
    digits = digitToInt <$> show n
    digitCount = length digits

main :: IO ()
main = mapM_ print $ take 25 (filter isNarcissistic [0 ..])
```


===Reduced search (unordered digit combinations)===
As summing the nth power of the digits is unaffected by digit order, we can reduce the search space by filtering digit combinations of given length and arbitrary order, rather than filtering a full integer sequence.

In this way we can find the 25th narcissistic number after '''length $ concatMap digitPowerSums [1 .. 7] == 19447''' tests – an improvement on the exhaustive trawl through '''9926315''' integers.


```haskell
import Control.Arrow (second)

isDaffodil :: Int -> Int -> Bool
isDaffodil e n =
  let ds = digitList n
  in e == length ds && n == powerSum e ds

powerSum :: Int -> [Int] -> Int
powerSum n = foldr ((+) . (^ n)) 0

digitList :: Int -> [Int]
digitList 0 = []
digitList n = rem n 10 : digitList (quot n 10)

narcissiOfLength :: Int -> [Int]
narcissiOfLength nDigits = snd <$> digitTree nDigits []
  where
    powers = ((,) <*> (^ nDigits)) <$> [0 .. 9]
    digitTree n parents =
      if n > 0
        then digitTree -- Power sums for all unordered digit combinations.
               (n - 1) -- (Digit order is irrelevant when summing powers)
               (if null parents
                  then powers
                  else concatMap
                         (\(d, pwrSum) ->
                             (second (pwrSum +) <$> take (d + 1) powers))
                         parents)
        else filter (isDaffodil nDigits . snd) parents

main :: IO ()
main = print $ 0 : concatMap narcissiOfLength [1 .. 7]
```

{{Out}}

```txt
[0,1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725,4210818,9800817,9926315]
```


=={{header|Icon}} and {{header|Unicon}}==

The following is a quick, dirty, and slow solution that works in both languages:

```unicon
procedure main(A)
    limit := integer(A[1]) | 25
    every write(isNarcissitic(seq(0))\limit)
end

procedure isNarcissitic(n)
    sn := string(n)
    m := *sn
    every (sum := 0) +:= (!sn)^m
    return sum = n
end
```


Sample run:

```txt

->ndn
0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315
->

```



## J


```j
getDigits=: "."0@":                  NB. get digits from number
isNarc=: (= +/@(] ^ #)@getDigits)"0  NB. test numbers for Narcissism
```

'''Example Usage'''

```j
   (#~ isNarc) i.1e7   NB. display Narcissistic numbers
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```


## Java

{{works with|Java|1.5+}}

```java5
public class Narc{
	public static boolean isNarc(long x){
		if(x < 0) return false;

		String xStr = Long.toString(x);
		int m = xStr.length();
		long sum = 0;

		for(char c : xStr.toCharArray()){
			sum += Math.pow(Character.digit(c, 10), m);
		}
		return sum == x;
	}

	public static void main(String[] args){
		for(long x = 0, count = 0; count < 25; x++){
			if(isNarc(x)){
				System.out.print(x + " ");
				count++;
			}
		}
	}
}
```

{{out}}

```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```


{{works with|Java|1.8}}
The statics and the System.exit(0) stem from having first developed a version that is not limited by the amount of narcisstic numbers that are to be calculated. I then read that this is a criterion and thus the implementation is an afterthought and looks awkwardish... but still... works!

```java5

import java.util.stream.IntStream;
public class NarcissisticNumbers {
    static int numbersToCalculate = 25;
    static int numbersCalculated = 0;

    public static void main(String[] args) {
        IntStream.iterate(0, n -> n + 1).limit(Integer.MAX_VALUE).boxed().forEach(i -> {
            int length = i.toString().length();
            int addedDigits = 0;

            for (int count = 0; count < length; count++) {
                int value = Integer.parseInt(String.valueOf(i.toString().charAt(count)));
                addedDigits += Math.pow(value, length);
            }

            if (i == addedDigits) {
                numbersCalculated++;
                System.out.print(addedDigits + " ");
            }

            if (numbersCalculated == numbersToCalculate) {
                System.exit(0);
            }
        });
    }
}
```

{{out}}

```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```



## JavaScript


### ES5

{{trans|Java}}

```javascript
function isNarc(x) {
    var str = x.toString(),
        i,
        sum = 0,
        l = str.length;
    if (x < 0) {
        return false;
    } else {
        for (i = 0; i < l; i++) {
            sum += Math.pow(str.charAt(i), l);
        }
    }
    return sum == x;
}
function main(){
    var n = [];
    for (var x = 0, count = 0; count < 25; x++){
        if (isNarc(x)){
            n.push(x);
            count++;
        }
    }
    return n.join(' ');
}
```

{{out}}

```txt
"0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315"
```



### ES6

====Exhaustive search (integer series)====

```JavaScript
(() => {
    'use strict';

    // digits :: Int -> [Int]
    const digits = n => n.toString()
        .split('')
        .map(x => parseInt(x, 10));

    // pow :: Int -> Int -> Int
    const pow = Math.pow;

    // isNarc :: Int -> Bool
    const isNarc = n => {
        const
            ds = digits(n),
            len = ds.length;

        return ds.reduce((a, x) =>
            a + pow(x, len), 0) === n;
    };

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    return until(
            x => x.narc.length > 24,
            x => ({
                n: x.n + 1,
                narc: (isNarc(x.n) ? x.narc.concat(x.n) : x.narc)
            }), {
                n: 0,
                narc: []
            }
        )
        .narc
})();
```

{{Out}}

```JavaScript
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315]
```



====Reduced search (unordered digit combinations)====
{{Trans|Haskell}}
As summing the nth power of the digits is unaffected by digit order, we can reduce the search space by filtering digit combinations of given length and arbitrary order, rather than filtering a full integer sequence.

In this way we can find the 25th narcissistic number after '''length(concatMap(digitPowerSums, enumFromTo(0, 7))) === 19447''' tests – an improvement on the exhaustive trawl through '''9926315''' integers.

(Generating the unordered digit combinations directly as power sums allows faster testing later, and needs less space)

```JavaScript
(() => {
    'use strict';

    // DAFFODILS --------------------------------------------------------------

    // narcissiOfLength :: Int -> [Int]
    const narcissiOfLength = n =>
        n > 0 ? filter(curry(isDaffodil)(n), digitPowerSums(n)) : [0];

    // Do the decimal digits of N, each raised to the power E, sum to N itself ?

// isDaffodil :: Int -> Int -> Bool
const isDaffodil = (e, n) => {
    const
        powerSum = (n, xs) => xs.reduce((a, x) => a + Math.pow(x, n), 0),
        digitList = n => (n > 0) ? (
            cons((n % 10), digitList(Math.floor(n / 10)))
        ) : [],
        ds = digitList(n);
    return e === ds.length && n === powerSum(e, ds);
};

    // The subset of integers of n digits that actually need daffodil checking:

    // (Flattened leaves of a tree of unique digit combinations, in which
    // order is not significant. Digit sequence doesn't affect power summing)

    // digitPowerSums :: Int -> [Int]
    const digitPowerSums = nDigits => {
        const
            digitPowers = map(x => [x, pow(x, nDigits)], enumFromTo(0, 9)),
            treeGrowth = (n, parentPairs) => (n > 0) ? (
                treeGrowth(n - 1,
                    isNull(parentPairs) ? (
                        digitPowers
                    ) : concatMap(([parentDigit, parentSum]) =>
                        map(([leafDigit, leafSum]) => //
                            [leafDigit, parentSum + leafSum],
                            take(parentDigit + 1, digitPowers)
                        ),
                        parentPairs
                    ))
            ) : parentPairs;
        return map(snd, treeGrowth(nDigits, []));
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // enumFromTo :: Int -> Int -> Maybe Int -> [Int]
    const enumFromTo = (m, n, step) => {
        const d = (step || 1) * (n >= m ? 1 : -1);
        return Array.from({
            length: Math.floor((n - m) / d) + 1
        }, (_, i) => m + (i * d));
    };
    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // isNull :: [a] -> Bool
    const isNull = xs => (xs instanceof Array) ? xs.length < 1 : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // pow :: Int -> Int -> Int
    const pow = Math.pow

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // show ::
    // (a -> String) f,  Num n =>
    // a -> maybe f -> maybe n -> String
    const show = JSON.stringify;

    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;


    // TEST -------------------------------------------------------------------

    // return length(concatMap(digitPowerSums, enumFromTo(0, 7)));

    return show(
        //digitPowerSums(3)
        concatMap(narcissiOfLength, enumFromTo(0, 7))
    );
})();
```

{{Out}}
(Tested in Atom editor, using Script package)

```txt
[0,1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725,4210818,9800817,9926315]
[Finished in 0.118s]
```



## jq


{{works with|jq|1.4}}

A function for checking whether a given non-negative integer is narcissistic could be implemented in jq as follows:

```jq
def is_narcissistic:
  def digits: tostring | explode[] | [.] | implode | tonumber;
  def pow(n): . as $x | reduce range(0;n) as $i (1; . * $x);

  (tostring | length) as $len
  | . == reduce digits as $d (0;  . + ($d | pow($len)) )
  end;
```


In the following, this definition is modified to avoid recomputing (d ^ i). This is accomplished introducing the array [i, [0^i, 1^i, ..., 9^i]].
To update this array for increasing values of i, the function powers(j) is defined as follows:

```jq
# Input:  [i, [0^i, 1^i, 2^i, ..., 9^i]]
# Output: [j, [0^j, 1^j, 2^j, ..., 9^j]]
# provided j is i or (i+1)
def powers(j):
  if .[0] == j then .
  else .[0] += 1
  | reduce range(0;10) as $k (.; .[1][$k] *= $k)
  end;
```


The function is_narcisstic can now be modified to use powers(j) as follows:

```jq
# Input: [n, [i, [0^i, 1^i, 2^i,...]]] where i is the number of digits in n.
def is_narcissistic:
  def digits: tostring | explode[] | [.] | implode | tonumber;
  .[1][1] as $powers
  | .[0]
  | if . < 0 then false
    else . == reduce digits as $d (0;  . + $powers[$d] )
    end;
```

'''The task'''

```jq
# If your jq has "while", then feel free to omit the following definition:
def while(cond; update):
  def _while:  if cond then ., (update | _while) else empty end;
  _while;

# The first k narcissistic numbers, beginning with 0:
def narcissistic(k):
  # State: [n, is_narcissistic, count, [len, [0^len, 1^len, ...]]]
  # where len is the number of digits in n.
  [0, true, 1, [1, [range(0;10)]]]
  | while( .[2] <= k;
           .[3] as $powers
           | (.[0]+1) as $n
           | ($n | tostring | length) as $len
	   | ($powers | powers($len)) as $powersprime
	   | if [$n, $powersprime] | is_narcissistic
	     then [$n, true, .[2] + 1, $powersprime]
	     else [$n, false, .[2], $powersprime ]
	     end )
  | select(.[1])
  | "\(.[2]): \(.[0])" ;

narcissistic(25)
```

{{out}}

```sh
jq -r -n -f Narcissitic_decimal_number.jq
1: 0
2: 1
3: 2
4: 3
5: 4
6: 5
7: 6
8: 7
9: 8
10: 9
11: 153
12: 370
13: 371
14: 407
15: 1634
16: 8208
17: 9474
18: 54748
19: 92727
20: 93084
21: 548834
22: 1741725
23: 4210818
24: 9800817
25: 9926315
```



## Julia

This easy to implement brute force technique is plenty fast enough to find the first few Narcissistic decimal numbers.

```Julia
using Printf  # for Julia version 1.0+

function isnarcissist(n, b=10)
    -1 < n || return false
    d = digits(n, base=b)
    m = length(d)
    n == mapreduce((x)->x^m, +, d)
end

function findnarcissist(verbose=false)
    goal = 25
    ncnt = 0
    verbose && println("Finding the first ", goal, " Narcissistic numbers:")
    for i in 0:typemax(1)
        isnarcissist(i) || continue
        ncnt += 1
        verbose && println(@sprintf "    %2d %7d" ncnt i)
        ncnt < goal || break
    end
end

findnarcissist()
@time findnarcissist(true)

```
{{out}}

```txt

Finding the first 25 Narcissistic numbers:
     1       0
     2       1
     3       2
     4       3
     5       4
     6       5
     7       6
     8       7
     9       8
    10       9
    11     153
    12     370
    13     371
    14     407
    15    1634
    16    8208
    17    9474
    18   54748
    19   92727
    20   93084
    21  548834
    22 1741725
    23 4210818
    24 9800817
    25 9926315
  3.054463 seconds (19.90 M allocations: 1.466 GiB, 14.27% gc time)

```



## Kotlin


```scala
// version 1.1.0

fun isNarcissistic(n: Int): Boolean {
    if (n < 0) throw IllegalArgumentException("Argument must be non-negative")
    var nn = n
    val digits = mutableListOf<Int>()
    val powers = IntArray(10) { 1 }
    while (nn > 0) {
       digits.add(nn % 10)
       for (i in 1..9) powers[i] *= i // no need to calculate powers[0]
       nn /= 10
    }
    val sum = digits.filter { it > 0 }.map { powers[it] }.sum()
    return n == sum
}

fun main(args: Array<String>) {
    println("The first 25 narcissistic (or Armstrong) numbers are:")
    var i = 0
    var count = 0
    do {
        if (isNarcissistic(i)) {
            print("$i ")
            count++
        }
        i++
    }
    while (count < 25)
}
```


{{out}}

```txt

The first 25 narcissistic (or Armstrong) numbers are:
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## Lua

This is a simple/naive/slow method but it still spits out the requisite 25 in less than a minute using LuaJIT on a 2.5 GHz machine.

```Lua
function isNarc (n)
    local m, sum, digit = string.len(n), 0
    for pos = 1, m do
        digit = tonumber(string.sub(n, pos, pos))
        sum = sum + digit^m
    end
    return sum == n
end

local n, count = 0, 0
repeat
    if isNarc(n) then
        io.write(n .. " ")
        count = count + 1
    end
    n = n + 1
until count == 25
```

{{out}}

```txt

0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315

```



## Maple


```Maple


Narc:=proc(i)
	local num,len,j,sums:
	sums:=0:
	num := parse~(StringTools:-Explode((convert(i,string)))):
	len:=numelems(num):
	for j from 1 to len do
		sums:=sums+(num[j]^(len)):
	end do;
	if sums = i then
		return i;
	else
		return NULL;
	end if;
end proc:

i:=0:
NDN:=[]:
while numelems(NDN)<25 do
	NDN:=[op(NDN),(Narc(i))]:
	i:=i+1:
end do:
NDN;

```



## Mathematica


```Mathematica
narc[1] = 0;
narc[n_] :=
  narc[n] =
   NestWhile[# + 1 &, narc[n - 1] + 1,
    Plus @@ (IntegerDigits[#]^IntegerLength[#]) != # &];
narc /@ Range[25]
```

{{out}}

```txt
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315}
```



## MATLAB


```MATLAB
function testNarcissism
    x = 0;
    c = 0;
    while c < 25
        if isNarcissistic(x)
            fprintf('%d ', x)
            c = c+1;
        end
        x = x+1;
    end
    fprintf('\n')
end

function tf = isNarcissistic(n)
    dig = sprintf('%d', n) - '0';
    tf = n == sum(dig.^length(dig));
end
```

{{out}}

```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```



## Oforth



```Oforth
: isNarcissistic(n)
| i m |
   n 0 while( n ) [ n 10 /mod ->n swap 1 + ] ->m
   0 m loop: i [ swap m pow + ] == ;

: genNarcissistic(n)
| l |
   ListBuffer new dup ->l
   0 while(l size n <>) [ dup isNarcissistic ifTrue: [ dup l add ] 1 + ] drop ;

```


{{out}}

```txt

>genNarcissistic(25) .
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084,
548834, 1741725, 4210818, 9800817, 9926315] ok

```



## PARI/GP

Naive code, could be improved by splitting the digits in half and meeting in the middle.

```parigp
isNarcissistic(n)=my(v=digits(n)); sum(i=1, #v, v[i]^#v)==n
v=List();for(n=1,1e9,if(isNarcissistic(n),listput(v,n);if(#v>24, return(Vec(v)))))
```

{{out}}

```txt
%1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315, 24678050]
```



## Pascal

{{works with|Free Pascal}}
A recursive version starting at the highest digit and recurses to digit 0. Bad runtime. One more digit-> 10x runtime
runtime ~ 10^(count of Digits).

```pascal

program NdN;
//Narcissistic decimal number
const
  Base = 10;
  MaxDigits = 16;
type
  tDigit = 0..Base-1;
  tcntDgt= 0..MaxDigits-1;
var
  powDgt   : array[tDigit]  of NativeUint;
  PotdgtPos: array[tcntDgt] of NativeUint;
  UpperSum : array[tcntDgt] of NativeUint;

  tmpSum,
  tmpN,
  actPot  : NativeUint;

procedure InitPowDig;
var
  i,j : NativeUint;
Begin
  j := 1;
  For i := 0 to High(tDigit) do
  Begin
    powDgt[i] := i;
    PotdgtPos[i] := j;
    j := j*Base;
  end;
  actPot := 0;
end;

procedure NextPowDig;
var
  i,j : NativeUint;
Begin
  // Next power of digit =  i ^ actPot,always 0 = 0 , 1 = 1
  For i := 2 to High(tDigit) do
    powDgt[i] := powDgt[i]*i;
  // number of digits times 9 ^(max number of digits)
  j := powDgt[High(tDigit)];
  For i := 0 to High(UpperSum) do
    UpperSum[i] := (i+1)*j;
  inc(actPot);
end;
procedure OutPutNdN(n:NativeUint);
Begin
  write(n,' ');
end;

procedure NextDgtSum(dgtPos,i,sumPowDgt,n:NativeUint);
begin
  //unable to reach sum
  IF (sumPowDgt+UpperSum[dgtPos]) < n then
    EXIT;
  repeat
    tmpN   := n+PotdgtPos[dgtPos]*i;
    tmpSum := sumPowDgt+powDgt[i];
    //unable to get smaller
    if tmpSum > tmpN then
      EXIT;
    IF tmpSum = tmpN then
      OutPutNdN(tmpSum);
    IF dgtPos>0 then
      NextDgtSum(dgtPos-1,0,tmpSum,tmpN);
    inc(i);
  until i >= Base;
end;

var
  i : NativeUint;
Begin
  InitPowDig;
  For i := 1 to 9 do
  Begin
    write(' length ',actPot+1:2,': ');
    //start with 1 in front, else you got i-times 0 in front
    NextDgtSum(actPot,1,0,0);
    writeln;
    NextPowDig;
  end;
end.
```

;output:

```txt

 time ./NdN
 length  1: 1 2 3 4 5 6 7 8 9
 length  2:
 length  3: 153 370 370 371 407
 length  4: 1634 8208 9474
 length  5: 54748 92727 93084
 length  6: 548834
 length  7: 1741725 4210818 9800817 9926315
 length  8: 24678050 24678050 24678051 88593477
 length  9: 146511208 472335975 534494836 912985153

real	0m1.000s
```



## Perl

Simple version using a naive predicate.  About 15 seconds.

```perl
sub is_narcissistic {
  my $n = shift;
  my($k,$sum) = (length($n),0);
  $sum += $_**$k for split(//,$n);
  $n == $sum;
}
my $i = 0;
for (1..25) {
  $i++ while !is_narcissistic($i);
  say $i++;
}
```



## Perl 6

Here is a straightforward, naive implementation.  It works but takes ages.

```perl6
sub is-narcissistic(Int $n) { $n == [+] $n.comb »**» $n.chars }

for 0 .. * {
    if .&is-narcissistic {
	.say;
	last if ++state$ >= 25;
    }
}
```

{{out}}

```txt
0
1
2
3
4
5
6
7
8
9
153
370
371
407
Ctrl-C
```


Here the program was interrupted but if you're patient enough you'll see all the 25 numbers.

Here's a faster version that precalculates the values for base 1000 digits:

```perl6
sub kigits($n) {
    my int $i = $n;
    my int $b = 1000;
    gather while $i {
        take $i % $b;
        $i = $i div $b;
    }
}

for (1..*) -> $d {
    my @t = 0..9 X** $d;
    my @table = @t X+ @t X+ @t;
    sub is-narcissistic(\n) { n == [+] @table[kigits(n)] };
    state $l = 2;
    FIRST say "1\t0";
    say $l++, "\t", $_ if .&is-narcissistic for 10**($d-1) ..^ 10**$d;
    last if $l > 25
};
```

{{out}}

```txt
1	0
2	1
3	2
4	3
5	4
6	5
7	6
8	7
9	8
10	9
11	153
12	370
13	371
14	407
15	1634
16	8208
17	9474
18	54748
19	92727
20	93084
21	548834
22	1741725
23	4210818
24	9800817
25	9926315
```



## Phix


```Phix
function narcissistic(integer n)
    string d = sprintf("%d",n)
    integer l = length(d)
    integer sumn = 0
    for i=1 to l do
        sumn += power(d[i]-'0',l)
    end for
    return sumn=n
end function

sequence s = {}
integer n = 0
while length(s)<25 do
    if narcissistic(n) then s &= n end if
    n += 1
end while
?s
```

{{out}}

```txt

{0,1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725,4210818,9800817,9926315}

```



## PicoLisp


```PicoLisp
(let (C 25 N 0 L 1)
   (loop
      (when
         (=
            N
            (sum ** (mapcar format (chop N)) (need L L)) )
         (println N)
         (dec 'C) )
      (inc 'N)
      (setq L (length N))
      (T (=0 C) 'done) ) )

(bye)
```



## PL/I


### version 1

{{trans|REXX}}

```pli
 narn: Proc Options(main);
 Dcl (j,k,l,nn,n,sum) Dec Fixed(15)init(0);
 Dcl s Char(15) Var;
 Dcl p(15) Pic'9' Based(addr(s));
 Dcl (ms,msa,ela) Dec Fixed(15);
 Dcl tim Char(12);
 n=30;
 ms=milliseconds();
 Do j=0 By 1 Until(nn=n);
   s=dec2str(j);
   l=length(s);
   sum=left(s,1)**l;
   Do k=2 To l;
     sum=sum+substr(s,k,1)**l;
     If sum>j Then Leave;
     End;
   If sum=j Then Do
     nn=nn+1;
     msa=milliseconds();
     ela=msa-ms;
     /*Put Skip Data(ms,msa,ela);*/
     ms=msa;                            /*yyyymmddhhmissmis*/
     tim=translate('ij:kl:mn.opq',datetime(),'abcdefghijklmnopq');
     Put Edit(nn,' narcissistic:',j,ela,tim)
             (Skip,f(9),a,f(12),f(15),x(2),a(12));
     End;
   End;
 dec2str: Proc(x) Returns(char(16) var);
 Dcl x Dec Fixed(15);
 Dcl ds Pic'(14)z9';
 ds=x;
 Return(trim(ds));
 End;
 milliseconds: Proc Returns(Dec Fixed(15));
 Dcl c17 Char(17);
 dcl 1 * Def C17,
      2 * char(8),
      2 hh Pic'99',
      2 mm Pic'99',
      2 ss Pic'99',
      2 ms Pic'999';
 Dcl result Dec Fixed(15);
 c17=datetime();
 result=(((hh*60+mm)*60)+ss)*1000+ms;
 /*
 Put Edit(translate('ij:kl:mn.opq',datetime(),'abcdefghijklmnopq'),
          result)
         (Skip,a(12),F(15));
 */
 Return(result);
 End
 End;
```

{{out}}

```txt
       1 narcissistic:           0              0  16:10:17.586
        2 narcissistic:           1              0  16:10:17.586
        3 narcissistic:           2              0  16:10:17.586
        4 narcissistic:           3              0  16:10:17.586
        5 narcissistic:           4              0  16:10:17.586
        6 narcissistic:           5              0  16:10:17.586
        7 narcissistic:           6              0  16:10:17.586
        8 narcissistic:           7              0  16:10:17.586
        9 narcissistic:           8              0  16:10:17.586
       10 narcissistic:           9              0  16:10:17.586
       11 narcissistic:         153              0  16:10:17.586
       12 narcissistic:         370              0  16:10:17.586
       13 narcissistic:         371              0  16:10:17.586
       14 narcissistic:         407              0  16:10:17.586
       15 narcissistic:        1634             10  16:10:17.596
       16 narcissistic:        8208             30  16:10:17.626
       17 narcissistic:        9474             10  16:10:17.636
       18 narcissistic:       54748            210  16:10:17.846
       19 narcissistic:       92727            170  16:10:18.016
       20 narcissistic:       93084              0  16:10:18.016
       21 narcissistic:      548834           1630  16:10:19.646
       22 narcissistic:     1741725           4633  16:10:24.279
       23 narcissistic:     4210818          10515  16:10:34.794
       24 narcissistic:     9800817          28578  16:11:03.372
       25 narcissistic:     9926315            510  16:11:03.882
       26 narcissistic:    24678050          73077  16:12:16.959
       27 narcissistic:    24678051              0  16:12:16.959
       28 narcissistic:    88593477         365838  16:18:22.797
       29 narcissistic:   146511208         276228  16:22:59.025
       30 narcissistic:   472335975        1682125  16:51:01.150
```



### version 2

Precompiled powers
<lang>*process source xref attributes or(!);
 narn3: Proc Options(main);
 Dcl (i,j,k,l,nn,n,sum) Dec Fixed(15)init(0);
 Dcl s  Char(15) Var;
 dcl t  Char(15);
 Dcl p9(15) Pic'9' Based(addr(t));
 Dcl (ms,msa,ela) Dec Fixed(15);
 Dcl tim Char(12);
 n=30;
 Dcl power(0:9,1:9) Dec Fixed(15);
 Do i=0 To 9;
   Do j=1 To 9;
     Power(i,j)=i**j;
     End;
   End;
 ms=milliseconds();
 Do j=0 By 1 Until(nn=n);
   s=dec2str(j);
   t=s;
   l=length(s);
   sum=power(p9(1),l);
   Do k=2 To l;
     sum=sum+power(p9(k),l);
     If sum>j Then Leave;
     End;
   If sum=j Then Do;
     nn=nn+1;
     msa=milliseconds();
     ela=msa-ms;
     ms=msa;                                /*yyyymmddhhmissmis*/
     tim=translate('ij:kl:mn.opq',datetime(),'abcdefghijklmnopq');
     Put Edit(nn,' narcissistic:',j,ela,tim)
             (Skip,f(9),a,f(12),f(15),x(2),a(12));
     End;
   End;

 dec2str: Proc(x) Returns(char(15) var);
 Dcl x Dec Fixed(15);
 Dcl ds Pic'(14)z9';
 ds=x;
 Return(trim(ds));
 End;

 milliseconds: Proc Returns(Dec Fixed(15));
 Dcl c17 Char(17);
 dcl 1 * Def C17,
      2 * char(8),
      2 hh Pic'99',
      2 mm Pic'99',
      2 ss Pic'99',
      2 ms Pic'999';
 Dcl result Dec Fixed(15);
 c17=datetime();
 result=(((hh*60+mm)*60)+ss)*1000+ms;
 Return(result);
 End;
 End;
```

{{out}}

```txt
        1 narcissistic:           0              0  00:41:43.632
        2 narcissistic:           1              0  00:41:43.632
        3 narcissistic:           2              0  00:41:43.632
        4 narcissistic:           3              0  00:41:43.632
        5 narcissistic:           4              0  00:41:43.632
        6 narcissistic:           5              0  00:41:43.632
        7 narcissistic:           6              0  00:41:43.632
        8 narcissistic:           7              0  00:41:43.632
        9 narcissistic:           8              0  00:41:43.632
       10 narcissistic:           9              0  00:41:43.632
       11 narcissistic:         153              0  00:41:43.632
       12 narcissistic:         370              0  00:41:43.632
       13 narcissistic:         371              0  00:41:43.632
       14 narcissistic:         407              0  00:41:43.632
       15 narcissistic:        1634              0  00:41:43.632
       16 narcissistic:        8208             20  00:41:43.652
       17 narcissistic:        9474             10  00:41:43.662
       18 narcissistic:       54748            130  00:41:43.792
       19 narcissistic:       92727            120  00:41:43.912
       20 narcissistic:       93084              0  00:41:43.912
       21 narcissistic:      548834           1310  00:41:45.222
       22 narcissistic:     1741725           3642  00:41:48.864
       23 narcissistic:     4210818           7488  00:41:56.352
       24 narcissistic:     9800817          22789  00:42:19.141
       25 narcissistic:     9926315            550  00:42:19.691
       26 narcissistic:    24678050          45358  00:43:05.049
       27 narcissistic:    24678051              0  00:43:05.049
       28 narcissistic:    88593477         237960  00:47:03.009
       29 narcissistic:   146511208         199768  00:50:22.777
       30 narcissistic:   472335975        1221384  01:10:44.161
```



## PowerShell


```PowerShell

function Test-Narcissistic ([int]$Number)
{
    if ($Number -lt 0) {return $false}

    $total  = 0
    $digits = $Number.ToString().ToCharArray()

    foreach ($digit in $digits)
    {
        $total += [Math]::Pow([Char]::GetNumericValue($digit), $digits.Count)
    }

    $total -eq $Number
}


[int[]]$narcissisticNumbers = @()
[int]$i = 0

while ($narcissisticNumbers.Count -lt 25)
{
    if (Test-Narcissistic -Number $i)
    {
        $narcissisticNumbers += $i
    }

    $i++
}

$narcissisticNumbers | Format-Wide {"{0,7}" -f $_} -Column 5 -Force

```

{{Out}}

```txt

      0                     1                     2                    3                    4
      5                     6                     7                    8                    9
    153                   370                   371                  407                 1634
   8208                  9474                 54748                92727                93084
 548834               1741725               4210818              9800817              9926315

```



## Python

This solution pre-computes the powers once.


```python
from __future__ import print_function
from itertools import count, islice

def narcissists():
    for digits in count(0):
        digitpowers = [i**digits for i in range(10)]
        for n in range(int(10**(digits-1)), 10**digits):
            div, digitpsum = n, 0
            while div:
                div, mod = divmod(div, 10)
                digitpsum += digitpowers[mod]
            if n == digitpsum:
                yield n

for i, n in enumerate(islice(narcissists(), 25), 1):
    print(n, end=' ')
    if i % 5 == 0: print()
print()
```


{{out}}

```txt
0 1 2 3 4
5 6 7 8 9
153 370 371 407 1634
8208 9474 54748 92727 93084
548834 1741725 4210818 9800817 9926315
```



### Faster Version

{{trans|D}}

```python
try:
    import psyco
    psyco.full()
except:
    pass

class Narcissistics:
    def __init__(self, max_len):
        self.max_len = max_len
        self.power = [0] * 10
        self.dsum = [0] * (max_len + 1)
        self.count = [0] * 10
        self.len = 0
        self.ord0 = ord('0')

    def check_perm(self, out = [0] * 10):
        for i in xrange(10):
            out[i] = 0

        s = str(self.dsum[0])
        for d in s:
            c = ord(d) - self.ord0
            out[c] += 1
            if out[c] > self.count[c]:
                return

        if len(s) == self.len:
            print self.dsum[0],

    def narc2(self, pos, d):
        if not pos:
            self.check_perm()
            return

        while True:
            self.dsum[pos - 1] = self.dsum[pos] + self.power[d]
            self.count[d] += 1
            self.narc2(pos - 1, d)
            self.count[d] -= 1
            if d == 0:
                break
            d -= 1

    def show(self, n):
        self.len = n
        for i in xrange(len(self.power)):
            self.power[i] = i ** n
        self.dsum[n] = 0
        print "length %d:" % n,
        self.narc2(n, 9)
        print

def main():
    narc = Narcissistics(14)
    for i in xrange(1, narc.max_len + 1):
        narc.show(i)

main()
```

{{out}}

```txt
length 1: 9 8 7 6 5 4 3 2 1 0
length 2:
length 3: 407 371 370 153
length 4: 9474 8208 1634
length 5: 93084 92727 54748
length 6: 548834
length 7: 9926315 9800817 4210818 1741725
length 8: 88593477 24678051 24678050
length 9: 912985153 534494836 472335975 146511208
length 10: 4679307774
length 11: 94204591914 82693916578 49388550606 44708635679 42678290603 40028394225 32164049651 32164049650
length 12:
length 13:
length 14: 28116440335967
```



## Racket


```racket
;; OEIS: A005188 defines these as positive numbers, so I will follow that definition in the function
;; definitions.
;;
;; 0: assuming it is represented as the single digit 0 (and not an empty string, which is not the
;;    usual convention for 0 in decimal), is not: sum(0^0), which is 1.  0^0 is a strange one,
;;    wolfram alpha calls returns 0^0 as indeterminate -- so I will defer to the brains behind OEIS
;;    on the definition here, rather than copy what I'm seeing in some of the results here
#lang racket

;; Included for the serious efficientcy gains we get from fxvectors vs. general vectors.
;;
;; We also use fx+/fx- etc. As it stands, they do a check for fixnumness, for safety.
;; We can link them in as "unsafe" operations (see the documentation on racket/fixnum);
;; but we get a result from this program quickly enough for my tastes.
(require racket/fixnum)

; uses a precalculated (fx)vector of powers -- caller provided, please.
(define (sub-narcissitic? N powered-digits)
  (let loop ((n N) (target N))
    (cond
      [(fx> 0 target) #f]
      [(fx= 0 target) (fx= 0 n)]
      [(fx= 0 n) #f]
      [else (loop (fxquotient n 10)
                  (fx- target (fxvector-ref powered-digits (fxremainder n 10))))])))

; Can be used as standalone, since it doesn't require caller to care about things like order of
; magnitude etc. However, it *is* slow, since it regenerates the powered-digits vector every time.
(define (narcissitic? n) ; n is +ve
  (define oom+1 (fx+ 1 (order-of-magnitude n)))
  (define powered-digits (for/fxvector ((i 10)) (expt i oom+1)))
  (sub-narcissitic? n powered-digits))

;; next m primes > z
(define (next-narcissitics z m) ; naming convention following math/number-theory's next-primes
  (let-values
      ([(i l)
        (for*/fold ((i (fx+ 1 z)) (l empty))
          ((oom (in-naturals))
           (dgts^oom (in-value (for/fxvector ((i 10)) (expt i (add1 oom)))))
           (n (in-range (expt 10 oom) (expt 10 (add1 oom))))
           #:when (sub-narcissitic? n dgts^oom)
           ; everyone else uses ^C to break...
           ; that's a bit of a manual process, don't you think?
           #:final (= (fx+ 1 (length l)) m))
          (values (+ i 1) (append l (list n))))])
    l)) ; we only want the list

(module+ main
  (next-narcissitics 0 25)
  ; here's another list... depending on whether you believe sloane or wolfram :-)
  (cons 0 (next-narcissitics 0 25)))

(module+ test
  (require rackunit)
  ; example given at head of task
  (check-true (narcissitic? 153))
  ; rip off the first 12 (and 0, since Armstrong numbers seem to be postivie) from
  ; http://oeis.org/A005188 for testing
  (check-equal?
   (for/list ((i (in-range 12))
              (n (sequence-filter narcissitic? (in-naturals 1)))) n)
   '(1 2 3 4 5 6 7 8 9 153 370 371))
  (check-equal? (next-narcissitics 0 12) '(1 2 3 4 5 6 7 8 9 153 370 371)))
```


{{out}}

```txt
(1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315 24678050)
(0 1 2 ... 9926315)
```



### Faster Version

This version uses lists of digits, rather than numbers themselves.

```racket
#lang racket
(define (non-decrementing-digital-sequences L)
  (define (inr d l)
    (cond
      [(<= l 0) '(())]
      [(= d 9) (list (make-list l d))]
      [else (append (map (curry cons d) (inr d (- l 1))) (inr (+ d 1) l))]))
  (inr 0 L))

(define (integer->digits-list n)
  (let inr ((n n) (l null)) (if (zero? n) l (inr (quotient n 10) (cons (modulo n 10) l)))))

(define (narcissitic-numbers-of-length L)
  (define tail-digits (non-decrementing-digital-sequences (sub1 L)))
  (define powers-v (for/fxvector #:length 10 ((i 10)) (expt i L)))
  (define (powers-sum dgts) (for/sum ((d (in-list dgts))) (fxvector-ref powers-v d)))
  (for*/list
      ((dgt1 (in-range 1 10))
       (dgt... (in-list tail-digits))
       (sum-dgt^l (in-value (powers-sum (cons dgt1 dgt...))))
       (dgts-sum (in-value (integer->digits-list sum-dgt^l)))
       #:when (= (car dgts-sum) dgt1)
       ; only now is it worth sorting the digits
       #:when (equal? (sort (cdr dgts-sum) <) dgt...))
    sum-dgt^l))

(define (narcissitic-numbers-of-length<= L)
  (cons 0 ; special!
        (apply append (for/list ((l (in-range 1 (+ L 1)))) (narcissitic-numbers-of-length l)))))

(module+ main
  (define all-narcissitics<10000000
    (narcissitic-numbers-of-length<= 7))
  ; conveniently, this *is* the list of 25... but I'll be a bit pedantic anyway
  (take all-narcissitics<10000000 25))

(module+ test
  (require rackunit)
  (check-equal? (non-decrementing-digital-sequences 1) '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9)))
  (check-equal?
   (non-decrementing-digital-sequences 2)
   '((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9)
           (1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9)
           (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9)
           (3 3) (3 4) (3 5) (3 6) (3 7) (3 8) (3 9)
           (4 4) (4 5) (4 6) (4 7) (4 8) (4 9)
           (5 5) (5 6) (5 7) (5 8) (5 9) (6 6) (6 7) (6 8) (6 9)
           (7 7) (7 8) (7 9) (8 8) (8 9) (9 9)))

  (check-equal? (integer->digits-list 0) null)
  (check-equal? (integer->digits-list 7) '(7))
  (check-equal? (integer->digits-list 10) '(1 0))

  (check-equal? (narcissitic-numbers-of-length 1) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (narcissitic-numbers-of-length 2) '())
  (check-equal? (narcissitic-numbers-of-length 3) '(153 370 371 407))

  (check-equal? (narcissitic-numbers-of-length<= 1) '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (narcissitic-numbers-of-length<= 3) '(0 1 2 3 4 5 6 7 8 9 153 370 371 407)))
```


{{out}}

```txt
'(0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 93084 92727 548834 1741725 4210818 9800817 9926315)
```



## REXX


### idiomatic


```rexx
/*REXX program  generates and displays  a number of  narcissistic (Armstrong)  numbers. */
numeric digits 39                                /*be able to handle largest Armstrong #*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=25                     /*Not specified?  Then use the default.*/
N=min(N, 89)                                     /*there are only  89  narcissistic #s. */
#=0                                              /*number of narcissistic numbers so far*/
     do j=0  until #==N;     L=length(j)         /*get length of the  J  decimal number.*/
     $=left(j, 1) **L                            /*1st digit in  J  raised to the L pow.*/

               do k=2  for L-1  until $>j        /*perform for each decimal digit in  J.*/
               $=$ + substr(j, k, 1) ** L        /*add digit raised to power to the sum.*/
               end   /*k*/                       /* [↑]  calculate the rest of the sum. */

     if $\==j  then iterate                      /*does the sum equal to J?  No, skip it*/
     #=# + 1                                     /*bump count of narcissistic numbers.  */
     say right(#, 9)     ' narcissistic:'     j  /*display index and narcissistic number*/
     end   /*j*/                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

        1  narcissistic: 0
        2  narcissistic: 1
        3  narcissistic: 2
        4  narcissistic: 3
        5  narcissistic: 4
        6  narcissistic: 5
        7  narcissistic: 6
        8  narcissistic: 7
        9  narcissistic: 8
       10  narcissistic: 9
       11  narcissistic: 153
       12  narcissistic: 370
       13  narcissistic: 371
       14  narcissistic: 407
       15  narcissistic: 1634
       16  narcissistic: 8208
       17  narcissistic: 9474
       18  narcissistic: 54748
       19  narcissistic: 92727
       20  narcissistic: 93084
       21  narcissistic: 548834
       22  narcissistic: 1741725
       23  narcissistic: 4210818
       24  narcissistic: 9800817
       25  narcissistic: 9926315

```



### optimized

This REXX version is optimized to pre-compute all the ten (single) digits raised to all possible powers (there are
only 39 possible widths/powers of narcissistic numbers).

It is about   '''77%'''   faster then 1<sup>st</sup> REXX version.

```rexx
/*REXX program  generates and displays  a number of  narcissistic (Armstrong)  numbers. */
numeric digits 39                                /*be able to handle largest Armstrong #*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=25                     /*Not specified?  Then use the default.*/
N=min(N, 89)                                     /*there are only  89  narcissistic #s. */

     do     p=1  for 39                          /*generate tables:   digits ^ P power. */
         do i=0  for 10;      @.p.i= i**p        /*build table of ten digits ^ P power. */
         end   /*i*/
     end       /*w*/                             /* [↑]  table is a fixed (limited) size*/
#=0                                              /*number of narcissistic numbers so far*/
     do j=0  until #==N;      L=length(j)        /*get length of the  J  decimal number.*/
     _=left(j, 1)                                /*select the first decimal digit to sum*/
     $=@.L._                                     /*sum of the J dec. digits ^ L (so far)*/
               do k=2  for L-1  until $>j        /*perform for each decimal digit in  J.*/
               _=substr(j, k, 1)                 /*select the next decimal digit to sum.*/
               $=$ + @.L._                       /*add dec. digit raised to power to sum*/
               end   /*k*/                       /* [↑]  calculate the rest of the sum. */

     if $\==j  then iterate                      /*does the sum equal to J?  No, skip it*/
     #=# + 1                                     /*bump count of narcissistic numbers.  */
     say right(#, 9)     ' narcissistic:'     j  /*display index and narcissistic number*/
     end   /*j*/                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}

===optimized, unrolled===
This REXX version is further optimized by unrolling part of the   '''do'''   loop that sums the decimal digits.

The unrolling also necessitated the special handling of one─ and two─digit narcissistic numbers.

It     is about      '''44%'''   faster then 2<sup>nd</sup> REXX version,   and

it is about          '''154%'''   faster then 1<sup>st</sup> REXX version.

```rexx
/*REXX program  generates and displays  a number of  narcissistic (Armstrong)  numbers. */
numeric digits 39                                /*be able to handle largest Armstrong #*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=25                     /*Not specified?  Then use the default.*/
N=min(N, 89)                                     /*there are only  89  narcissistic #s. */
@.=0                                             /*set default for the @ stemmed array. */
#=0                                              /*number of narcissistic numbers so far*/
     do p=0  for 39+1; if p<10  then call tell p /*display the 1st 1─digit dec. numbers.*/
         do i=1  for 9;     @.p.i= i**p          /*build table of ten digits ^ P power. */
         end   /*i*/
     end       /*p*/                             /* [↑]  table is a fixed (limited) size*/
                                                 /* [↓]  skip the 2─digit dec. numbers. */
     do j=100;              L=length(j)          /*get length of the  J  decimal number.*/
     parse var  j    _1  2  _2  3  m  ''  -1  _R /*get 1st, 2nd, middle, last dec. digit*/
     $=@.L._1  +  @.L._2  +  @.L._R              /*sum of the J decimal digs^L (so far).*/

              do k=3  for L-3  until $>j         /*perform for other decimal digits in J*/
              parse var  m    _  +1  m           /*get next dec. dig in J, start at 3rd.*/
              $=$ + @.L._                        /*add dec. digit raised to pow to sum. */
              end   /*k*/                        /* [↑]  calculate the rest of the sum. */

     if $==j  then do;  call tell j              /*does the sum equal to  J?  Show the #*/
                        if #==n  then leave      /*does the sum equal to  J?  Show the #*/
                   end
     end   /*j*/                                 /* [↑]  the  J loop  list starts at 100*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: #=# + 1                                    /*bump the counter for narcissistic #s.*/
      say right(#,9)   ' narcissistic:'   arg(1) /*display index and narcissistic number*/
      if #==n  &  n<11  then exit                /*finished showing of narcissistic #'s?*/
      return                                     /*return to invoker & keep on truckin'.*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}

===optimized, 3-digit chunks===
This REXX version is further optimized by pre-computing the narcissistic sums of all two-digit and three-digit numbers   (and also including those with leading zeros).

It     is about     '''65%'''   faster then 3<sup>rd</sup> REXX version,   and

it is about          '''136%'''   faster then 2<sup>nd</sup> REXX version,   and

it is about          '''317%'''   faster then 1<sup>st</sup> REXX version.

```rexx
/*REXX program  generates and displays  a number of  narcissistic (Armstrong)  numbers. */
numeric digits 39                                /*be able to handle largest Armstrong #*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=25                     /*Not specified?  Then use the default.*/
N=min(N, 89)                                     /*there are only  89  narcissistic #s. */
@.=0                                             /*set default for the @ stemmed array. */
#=0                                              /*number of narcissistic numbers so far*/
     do p=0  for 39+1; if p<10  then call tell p /*display the 1st 1─digit dec. numbers.*/
         do i=1  for 9;      @.p.i= i**p         /*build table of ten digits ^ P power. */
         zzj= '00'j;       @.p.zzj= @.p.j        /*assign value for a 3-dig number (LZ),*/
         end   /*i*/

         do j=10  to 99;   parse var j  t 2 u    /*obtain 2 decimal digits of J:    T U */
         @.p.j = @.p.t + @.p.u                   /*assign value for a 2─dig number.     */
         zj=  '0'j;        @.p.zj = @.p.j        /*   "     "    "  " 3─dig    "   (LZ),*/
         end   /*j*/                             /* [↑]  T≡ tens digit;  U≡ units digit.*/

         do k=100  to 999; parse var k h 2 t 3 u /*obtain 3 decimal digits of J:  H T U */
         @.p.k= @.p.h + @.p.t + @.p.u            /*assign value for a three-digit number*/
         end   /*k*/                             /* [↑]  H≡ hundreds digit;  T≡ tens ···*/
     end       /*p*/                             /* [↑]  table is a fixed (limited) size*/
                                                 /* [↓]  skip the 2─digit dec. numbers. */
     do j=100;               L=length(j)         /*get length of the  J  decimal number.*/
     parse var  j  _  +3  m                      /*get 1st three decimal digits of  J.  */
     $=@.L._                                     /*sum of the J decimal digs^L (so far).*/
                do  while m\==''                 /*do the rest of the dec. digs in  J.  */
                parse var  m    _  +3  m         /*get the next 3 decimal digits in  M. */
                $=$ + @.L._                      /*add dec. digit raised to pow to sum. */
                end   /*while*/                  /* [↑]  calculate the rest of the sum. */

     if $==j  then do;  call tell j              /*does the sum equal to  J?  Show the #*/
                        if #==n  then leave      /*does the sum equal to  J?  Show the #*/
                   end
     end   /*j*/                                 /* [↑]  the  J loop  list starts at 100*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: #=# + 1                                    /*bump the counter for narcissistic #s.*/
      say right(#,9)   ' narcissistic:'   arg(1) /*display index and narcissistic number*/
      if #==n  &  n<11  then exit                /*finished showing of narcissistic #'s?*/
      return                                     /*return to invoker & keep on truckin'.*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}

Further optimization could be utilized by increasing the chunk size to four or five decimal digits, but with an accompanying increase in the size of the pre-computed values.




## Ring


```ring

n = 0
count = 0
size = 15
while count != size
      m = isNarc(n)
      if m=1 see "" + n + " is narcisstic" + nl
         count = count + 1 ok
      n = n + 1
end

func isNarc n
     m = len(string(n))
     sum = 0
     digit = 0
     for pos = 1 to m
         digit = number(substr(string(n), pos, 1))
         sum = sum + pow(digit,m)
     next
     nr = (sum = n)
     return nr

```



## Ruby


```ruby
class Integer
  def narcissistic?
    return false if negative?
    digs = self.digits
    m    = digs.size
    digs.map{|d| d**m}.sum == self
  end
end

puts 0.step.lazy.select(&:narcissistic?).first(25)
```

{{out}}

```txt

0
1
2
3
4
5
6
7
8
9
153
370
371
407
1634
8208
9474
54748
92727
93084
548834
1741725
4210818
9800817
9926315

```



## Scala

{{works with|Scala|2.9.x}}


```Scala
object NDN extends App {

  val narc: Int => Int = n => (n.toString map (_.asDigit) map (math.pow(_, n.toString.size)) sum) toInt
  val isNarc: Int => Boolean = i => i == narc(i)

  println((Iterator from 0 filter isNarc take 25 toList) mkString(" "))

}
```


Output:

```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
```



## Sidef


```ruby
func is_narcissistic(n) {
    n.digits »**» n.len -> sum == n
}
 
var count = 0
for i in ^Inf {
    if (is_narcissistic(i)) {
        say "#{++count}\t#{i}"
        break if (count == 25)
    }
}
```

{{out}}

```txt

1	0
2	1
3	2
4	3
5	4
6	5
7	6
8	7
9	8
10	9
11	153
12	370
13	371
14	407
15	1634
16	8208
17	9474
18	54748
19	92727
20	93084
21	548834
22	1741725
23	4210818
24	9800817
25	9926315

```



## Tcl


```tcl
proc isNarcissistic {n} {
    set m [string length $n]
    for {set t 0; set N $n} {$N} {set N [expr {$N / 10}]} {
	incr t [expr {($N%10) ** $m}]
    }
    return [expr {$n == $t}]
}

proc firstNarcissists {target} {
    for {set n 0; set count 0} {$count < $target} {incr n} {
	if {[isNarcissistic $n]} {
	    incr count
	    lappend narcissists $n
	}
    }
    return $narcissists
}

puts [join [firstNarcissists 25] ","]
```

{{out}}

```txt

0,1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725,4210818,9800817,9926315

```



## UNIX Shell

{{works with|ksh93}}

```bash
function narcissistic {
    integer n=$1 len=${#n} sum=0 i
    for ((i=0; i<len; i++)); do
        (( sum += pow(${n:i:1}, len) ))
    done
    (( sum == n ))
}

nums=()
for ((n=0; ${#nums[@]} < 25; n++)); do
    narcissistic $n && nums+=($n)
done
echo "${nums[*]}"
echo "elapsed: $SECONDS"
```


{{output}}

```txt
0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474 54748 92727 93084 548834 1741725 4210818 9800817 9926315
elapsed: 436.639
```



## VBA

{{trans|Phix}}
```vb
Private Function narcissistic(n As Long) As Boolean
    Dim d As String: d = CStr(n)
    Dim l As Integer: l = Len(d)
    Dim sumn As Long: sumn = 0
    For i = 1 To l
        sumn = sumn + (Mid(d, i, 1) - "0") ^ l
    Next i
    narcissistic = sumn = n
End Function

Public Sub main()
    Dim s(24) As String
    Dim n As Long: n = 0
    Dim found As Integer: found = 0
    Do While found < 25
        If narcissistic(n) Then
            s(found) = CStr(n)
            found = found + 1
        End If
        n = n + 1
    Loop
    Debug.Print Join(s, ", ")
End Sub
```
{{out}}

```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315
```


## VBScript


```vb
Function Narcissist(n)
	i = 0
	j = 0
	Do Until j = n
		sum = 0
		For k = 1 To Len(i)
			sum = sum + CInt(Mid(i,k,1)) ^ Len(i)
		Next
		If i = sum Then
			Narcissist = Narcissist & i & ", "
			j = j + 1
		End If
		i = i + 1
	Loop
End Function

WScript.StdOut.Write Narcissist(25)
```

{{out}}

```txt
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474, 54748, 92727, 93084, 548834, 1741725, 4210818, 9800817, 9926315,
```



## zkl


```zkl
fcn isNarcissistic(n){
   ns,m := n.split(), ns.len() - 1;
   ns.reduce('wrap(s,d){ z:=d; do(m){z*=d} s+z },0) == n
}
```

Pre computing the first 15 powers of 0..9 for use as a look up table speeds things up quite a bit but performance is pretty underwhelming.

```zkl
var [const] powers=(10).pump(List,'wrap(n){
      (1).pump(15,List,'wrap(p){ n.toFloat().pow(p).toInt() }) });
fcn isNarcissistic2(n){
   m:=(n.numDigits - 1);
   n.split().reduce('wrap(s,d){ s + powers[d][m] },0) == n
}
```

Now stick a filter on a infinite lazy sequence (ie iterator) to create an infinite sequence of narcissistic numbers (iterator.filter(n,f) --> n results of f(i).toBool()==True).

```zkl
ns:=[0..].filter.fp1(isNarcissistic);
ns(15).println();
ns(5).println();
ns(5).println();
```

{{out}}

```txt

L(0,1,2,3,4,5,6,7,8,9,153,370,371,407,1634)
L(8208,9474,54748,92727,93084)
L(548834,1741725,4210818,9800817,9926315)

```


## ZX Spectrum Basic

Array index starts at 1. Only 1 character long variable names are allowed for For-Next loops. 8 Digits or higher numbers are displayed as floating point numbers. Needs about 2 hours (3.5Mhz)

```zxbasic
 1 DIM K(10): DIM M(10)
 2 FOR Y=0 TO 9: LET M(Y+1)=Y: NEXT Y
 3 FOR N=1 TO 7
 4 FOR J=N TO 0 STEP -1
 5 FOR I=N-J TO 0 STEP -1
 6 FOR H=N-J-I TO 0 STEP -1
 7 FOR G=N-J-I-H TO 0 STEP -1
 8 FOR F=N-J-I-H-G TO 0 STEP -1
 9 FOR E=N-J-I-H-G-F TO 0 STEP -1
10 FOR D=N-J-I-H-G-F-E TO 0 STEP -1
11 FOR C=N-J-I-H-G-F-E-D TO 0 STEP -1
12 FOR B=N-J-I-H-G-F-E-D-C TO 0 STEP -1
13 LET A=N-J-I-H-G-F-E-D-C-B
14 LET X=B+C*M(3)+D*M(4)+E*M(5)+F*M(6)+G*M(7)+H*M(8)+I*M(9)+J*M(10)
15 LET S$=STR$ (X)
16 IF LEN (S$)<N THEN GO TO 34
17 IF LEN (S$)<>N THEN GO TO 33
18 FOR Y=1 TO 10: LET K(Y)=0: NEXT Y
19 FOR Y=1 TO N
20 LET Z= CODE (S$(Y))-47
21 LET K(Z)=K(Z)+1
22 NEXT Y
23 IF A<>K(1) THEN GO TO 33
24 IF B<>K(2) THEN GO TO 33
25 IF C<>K(3) THEN GO TO 33
26 IF D<>K(4) THEN GO TO 33
27 IF E<>K(5) THEN GO TO 33
28 IF F<>K(6) THEN GO TO 33
29 IF G<>K(7) THEN GO TO 33
30 IF H<>K(8) THEN GO TO 33
31 IF I<>K(9) THEN GO TO 33
32 IF J=K(10) THEN PRINT X,
33 NEXT B: NEXT C: NEXT D: NEXT E: NEXT F: NEXT G: NEXT H: NEXT I: NEXT J
34 FOR Y=2 TO 9
35 LET M(Y+1)=M(Y+1)*Y
36 NEXT Y
37 NEXT N
38 PRINT
39 PRINT "DONE"
```

{{out}}

```txt
9               8
7               6
5               4
3               2
1               0
9               8
7               6
5               4
3               2
1               0
407             371
370             153
9474            8208
1634            93084
92727           54748
548834          9926315
9800817         4210818
1741725
```

