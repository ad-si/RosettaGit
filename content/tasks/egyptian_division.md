+++
title = "Egyptian division"
description = ""
date = 2019-04-24T00:52:17Z
aliases = []
[extra]
id = 21552
[taxonomies]
categories = ["Mathematics", "Arithmetic", "Ancient mathematics", "task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "bacon",
  "c",
  "cpp",
  "csharp",
  "d",
  "erlang",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "vba",
  "visual_basic_.net",
  "zkl",
]
+++

## Task

Egyptian division is a method of dividing integers using addition and
doubling that is similar to the algorithm of [[Ethiopian multiplication]]

'''Algorithm:'''

Given two numbers where the '''dividend''' is to be divided by the '''divisor''':

# Start the construction of a table of two columns: '''<code>powers_of_2</code>''', and '''<code>doublings</code>'''; by a first row of a 1 (i.e. 2^0) in the first column and 1 times the divisor in the first row second column.
# Create the second row with columns of 2 (i.e 2^1), and 2 * divisor in order.
# Continue with successive i’th rows of 2^i and 2^i * divisor.
# Stop adding rows, and keep only those rows, where 2^i * divisor is less than or equal to the dividend.
# We now assemble two separate sums that both start as zero, called here '''answer''' and '''accumulator'''
# Consider each row of the table, in the ''reverse'' order of its construction.
# If the current value of the accumulator added to the doublings cell would be less than or equal to the dividend then add it to the accumulator, as well as adding the powers_of_2 cell value to the answer.
# When the first row has been considered as above, then the integer division of dividend by divisor is given by answer.
 (And the remainder is given by the absolute value of accumulator - dividend).



'''Example: 580 / 34'''

''' Table creation: '''

::: {| class="wikitable"
! powers_of_2
! doublings
|-
| 1
| 34
|-
| 2
| 68
|-
| 4
| 136
|-
| 8
| 272
|-
| 16
| 544
|}

''' Initialization of sums: '''

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| 1
| 34
|

|

|-
| 2
| 68
|

|

|-
| 4
| 136
|

|

|-
| 8
| 272
|

|

|-
| 16
| 544
|

|

|-
|
|
| 0
| 0
|}

''' Considering table rows, bottom-up: '''

When a row is considered it is shown <s>crossed out</s> if it is not accumulated, or '''bold''' if the row causes summations.

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| 1
| 34
|

|

|-
| 2
| 68
|

|

|-
| 4
| 136
|

|

|-
| 8
| 272
|

|

|-
| '''16'''
| '''544'''
| 16
| 544
|}

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| 1
| 34
|

|

|-
| 2
| 68
|

|

|-
| 4
| 136
|

|

|-
| <s>8</s>
| <s>272</s>
| 16
| 544
|-
| '''16'''
| '''544'''
|

|

|}

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| 1
| 34
|

|

|-
| 2
| 68
|

|

|-
| <s>4</s>
| <s>136</s>
| 16
| 544
|-
| <s>8</s>
| <s>272</s>
|

|

|-
| '''16'''
| '''544'''
|

|

|}

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| 1
| 34
|

|

|-
| <s>2</s>
| <s>68</s>
| 16
| 544
|-
| <s>4</s>
| <s>136</s>
|

|

|-
| <s>8</s>
| <s>272</s>
|

|

|-
| '''16'''
| '''544'''
|

|

|}

::: {| class="wikitable"
! powers_of_2
! doublings
! answer
! accumulator
|-
| '''1'''
| '''34'''
| 17
| 578
|-
| <s>2</s>
| <s>68</s>
|

|

|-
| <s>4</s>
| <s>136</s>
|

|

|-
| <s>8</s>
| <s>272</s>
|

|

|-
| '''16'''
| '''544'''
|

|

|}

'''Answer'''

So 580 divided by 34 using the Egyptian method is '''<code>17</code>''' remainder (578 - 580) or '''<code>2</code>'''.



'''Task'''

The task is to create a function that does [https://en.wikipedia.org/wiki/Ancient_Egyptian_mathematics#Multiplication_and_division Egyptian division]. The function should<br />
closely follow the description above in using a list/array of powers of two, and<br />
another of doublings.

* Functions should be clear interpretations of the algorithm.
* Use the function to divide 580 by 34 and show the answer '''here, on this page'''.


;References:
:*   [https://discoveringegypt.com/egyptian-hieroglyphic-writing/egyptian-mathematics-numbers-hieroglyphs/ Egyptian Number System]


;Related tasks:
:*   [[Egyptian_fractions|Egyptian fractions]]





## Ada



```Ada

with Ada.Text_IO;

procedure Egyptian_Division is

  procedure Divide  (a : Natural; b : Positive; q, r : out Natural) is
    doublings : array (0..31) of Natural;  -- The natural type holds values < 2^32 so no need going beyond
    m, sum, last_index_touched : Natural := 0;
  begin
    for i in doublings'Range loop
      m := b * 2**i;
      exit when m > a ;
      doublings (i) := m;
      last_index_touched := i;
    end loop;
    q := 0;
    for i in reverse doublings'First .. last_index_touched loop
        m := sum + doublings (i);
        if m <= a then
          sum := m;
          q := q + 2**i;
        end if;
    end loop;
    r := a -sum;
  end Divide;

  q, r : Natural;
begin
  Divide (580,34, q, r);
  Ada.Text_IO.put_line ("Quotient="&q'Img & " Remainder="&r'img);
end Egyptian_Division;

```

{{Out}}

```txt
Quotient= 17 Remainder= 2
```



## ALGOL 68


```algol68
BEGIN
    # performs Egyptian division of dividend by divisor, setting quotient and remainder #
    # this uses 32 bit numbers, so a table of 32 powers of 2 should be sufficient       #
    # ( divisors > 2^30 will probably overflow - this is not checked here )             #
    PROC egyptian division = ( INT dividend, divisor, REF INT quotient, remainder )VOID:
         BEGIN
            [ 1 : 32 ]INT powers of 2, doublings;
            # initialise the powers of 2 and doublings tables #
            powers of 2[ 1 ] := 1;
            doublings  [ 1 ] := divisor;
            INT   table pos  := 1;
            WHILE table pos +:= 1;
                  powers of 2[ table pos ] := powers of 2[ table pos - 1 ] * 2;
                  doublings  [ table pos ] := doublings  [ table pos - 1 ] * 2;
                  doublings[ table pos ] <= dividend
            DO
                SKIP
            OD;
            # construct the accumulator and answer #
            INT accumulator := 0, answer := 0;
            WHILE table pos >=1
            DO
                IF ( accumulator + doublings[ table pos ] ) <= dividend
                THEN
                    accumulator +:= doublings  [ table pos ];
                    answer      +:= powers of 2[ table pos ]
                FI;
                table pos -:= 1
            OD;
            quotient  := answer;
            remainder := ABS ( accumulator - dividend )
        END # egyptian division # ;

    # task test case #
    INT quotient, remainder;
    egyptian division( 580, 34, quotient, remainder );
    print( ( "580 divided by 34 is: ", whole( quotient, 0 ), " remainder: ", whole( remainder, 0 ), newline ) )
END
```

{{out}}

```txt

580 divided by 34 is: 17 remainder: 2

```



## AppleScript


Unfold to derive successively doubled rows, fold to sum quotient and derive remainder

```AppleScript
-- EGYPTIAN DIVISION ------------------------------------

-- eqyptianQuotRem :: Int -> Int -> (Int, Int)
on egyptianQuotRem(m, n)
    script expansion
        on |λ|(ix)
            set {i, x} to ix
            if x > m then
                Nothing()
            else
                Just({ix, {i + i, x + x}})
            end if
        end |λ|
    end script

    script collapse
        on |λ|(ix, qr)
            set {i, x} to ix
            set {q, r} to qr
            if x < r then
                {q + i, r - x}
            else
                qr
            end if
        end |λ|
    end script

    return foldr(collapse, {0, m}, ¬
        unfoldr(expansion, {1, n}))
end egyptianQuotRem


-- TEST -------------------------------------------------
on run
    egyptianQuotRem(580, 34)
end run

-- GENERIC FUNCTIONS ------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- > [10,9,8,7,6,5,4,3,2,1]
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
on unfoldr(f, v)
    set xr to {v, v} -- (value, remainder)
    set xs to {}
    tell mReturn(f)
        repeat -- Function applied to remainder.
            set mb to |λ|(item 2 of xr)
            if Nothing of mb then
                exit repeat
            else -- New (value, remainder) tuple,
                set xr to Just of mb
                -- and value appended to output list.
                set end of xs to item 1 of xr
            end if
        end repeat
    end tell
    return xs
end unfoldr
```

{{Out}}

```txt
{17, 2}
```



## AutoHotkey


```AutoHotkey
divident := 580
divisor := 34

answer := accumulator := 0
obj := []	, div := divisor

while (div < divident)
{
	obj[2**(A_Index-1)] := div				; obj[powers_of_2] := doublings
	div *= 2						; double up
}

while obj.MaxIndex()						; iterate rows "in the reverse order"
{
	if (accumulator + obj[obj.MaxIndex()] <= divident)	; If (accumulator + current doubling) <= dividend
	{
		accumulator += obj[obj.MaxIndex()]		; add current doubling to the accumulator
		answer += obj.MaxIndex()			; add the powers_of_2 value to the answer.
	}
	obj.pop()						; remove current row
}
MsgBox % divident "/" divisor " = " answer ( divident-accumulator > 0 ? " r" divident-accumulator : "")
```

Outputs:
```txt
580/34 = 17 r2
```




## BaCon


```c


'---Ported from the c code example to BaCon by bigbass

'
### ============================================================================

FUNCTION EGYPTIAN_DIVISION(long dividend, long divisor, long remainder) TYPE long
'
### ============================================================================

'--- remainder is the third  parameter, pass 0 if you do not need the remainder

DECLARE powers[64] TYPE long
DECLARE doublings[64] TYPE long

	LOCAL i TYPE long

	FOR i = 0 TO  63 STEP 1
		powers[i] = 1 << i
		doublings[i] = divisor << i
		IF (doublings[i] > dividend) THEN
			BREAK
		ENDIF
	NEXT

	LOCAL answer TYPE long
	LOCAL accumulator TYPE long
	answer = 0
	accumulator = 0

	WHILE i >= 0
		'--- If the current value of the accumulator added to the
		'--- doublings cell would be less than or equal to the
		'--- dividend then add it to the accumulator
		IF (accumulator + doublings[i] <= dividend) THEN
			accumulator = accumulator + doublings[i]
			answer = answer + powers[i]
		ENDIF
		DECR i
	WEND

	IF remainder THEN
		remainder = dividend - accumulator
		PRINT dividend ," / ", divisor, " = " , answer ," remainder " , remainder

        PRINT "Decoded the answer to a standard fraction"
        PRINT  (remainder + 0.0 )/ (divisor + 0.0) + answer
        PRINT

	ELSE
		PRINT dividend ," / ", divisor , " = " , answer
	ENDIF

	RETURN answer

ENDFUNCTION


	'--- the large number divided by the smaller number
	'--- the third argument is 1 if you want to have a remainder
	'--- and 0 if you dont want to have a remainder

	EGYPTIAN_DIVISION(580,34,1)
	EGYPTIAN_DIVISION(580,34,0)

EGYPTIAN_DIVISION(580,34,1)




```




## C


```c

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

uint64_t egyptian_division(uint64_t dividend, uint64_t divisor, uint64_t *remainder) {
	// remainder is an out parameter, pass NULL if you do not need the remainder

	static uint64_t powers[64];
	static uint64_t doublings[64];

	int i;

	for(i = 0; i < 64; i++) {
		powers[i] = 1 << i;
		doublings[i] = divisor << i;
		if(doublings[i] > dividend)
			break;
	}

	uint64_t answer = 0;
	uint64_t accumulator = 0;

	for(i = i - 1; i >= 0; i--) {
		// If the current value of the accumulator added to the
		// doublings cell would be less than or equal to the
		// dividend then add it to the accumulator
		if(accumulator + doublings[i] <= dividend) {
			accumulator += doublings[i];
			answer += powers[i];
		}
	}

	if(remainder)
		*remainder = dividend - accumulator;
	return answer;
}

void go(uint64_t a, uint64_t b) {
	uint64_t x, y;
	x = egyptian_division(a, b, &y);
	printf("%llu / %llu = %llu remainder %llu\n", a, b, x, y);
	assert(a == b * x + y);
}

int main(void) {
	go(580, 32);
}

```



## C++

{{trans|C}}

```cpp
#include <cassert>
#include <iostream>

typedef unsigned long ulong;

/*
 * Remainder is an out paramerter. Use nullptr if the remainder is not needed.
 */
ulong egyptian_division(ulong dividend, ulong divisor, ulong* remainder) {
    constexpr int SIZE = 64;
    ulong powers[SIZE];
    ulong doublings[SIZE];
    int i = 0;

    for (; i < SIZE; ++i) {
        powers[i] = 1 << i;
        doublings[i] = divisor << i;
        if (doublings[i] > dividend) {
            break;
        }
    }

    ulong answer = 0;
    ulong accumulator = 0;

    for (i = i - 1; i >= 0; --i) {
        /*
         * If the current value of the accumulator added to the
         * doublings cell would be less than or equal to the
         * dividend then add it to the accumulator
         */
        if (accumulator + doublings[i] <= dividend) {
            accumulator += doublings[i];
            answer += powers[i];
        }
    }

    if (remainder) {
        *remainder = dividend - accumulator;
    }
    return answer;
}

void print(ulong a, ulong b) {
    using namespace std;

    ulong x, y;
    x = egyptian_division(a, b, &y);

    cout << a << " / " << b << " = " << x << " remainder " << y << endl;
    assert(a == b * x + y);
}

int main() {
    print(580, 34);

    return 0;
}
```

{{out}}

```txt
580 / 34 = 17 remainder 2
```



## C#


```c#

using System;
using System.Collections;

namespace Egyptian_division
{
	class Program
	{
		public static void Main(string[] args)
		{
			Console.Clear();
			Console.WriteLine();
			Console.WriteLine(" Egyptian division ");
			Console.WriteLine();
			Console.Write(" Enter value of dividend : ");
			int dividend = int.Parse(Console.ReadLine());

			Console.Write(" Enter value of divisor : ");
			int divisor = int.Parse(Console.ReadLine());

			Divide(dividend, divisor);

			Console.WriteLine();
			Console.Write("Press any key to continue . . . ");
			Console.ReadKey(true);



		}

		static void Divide(int dividend, int divisor)
		{
			//
			// Local variable declaration and initialization
			//
			int result   = 0;
			int reminder = 0;

			int powers_of_two = 0;
			int doublings 	  = 0;

			int answer 	= 0;
			int accumulator = 0;

			int two = 2;
			int pow = 0;
			int row = 0;

			//
			// Tables declaration
			//
			ArrayList table_powers_of_two = new ArrayList();
			ArrayList table_doublings     = new ArrayList();

			//
			// Fill and Show table values
			//
			Console.WriteLine("                           ");
			Console.WriteLine(" powers_of_2     doublings ");
			Console.WriteLine("                           ");

			// Set initial values
			powers_of_two = 1;
			doublings = divisor;
			while( doublings <= dividend )
			{
				// Set table value
				table_powers_of_two.Add( powers_of_two );
				table_doublings.Add( doublings );

				// Show new table row
				Console.WriteLine("{0,8}{1,16}",powers_of_two, doublings);


				pow++;

				powers_of_two = (int)Math.Pow( two, pow );
				doublings = powers_of_two * divisor;
			}
			Console.WriteLine("                           ");

			//
			// Calculate division and Show table values
			//
			row = pow - 1;
			Console.WriteLine("                                                 ");
			Console.WriteLine(" powers_of_2     doublings   answer   accumulator");
			Console.WriteLine("                                                 ");
			Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop + row);

			pow--;
			while( pow >= 0 && accumulator < dividend )
			{
				// Get values from tables
				doublings = int.Parse(table_doublings[pow].ToString());
				powers_of_two = int.Parse(table_powers_of_two[pow].ToString());

				if(accumulator + int.Parse(table_doublings[pow].ToString()) <= dividend )
				{
					// Set new values
					accumulator += doublings;
					answer += powers_of_two;

					// Show accumulated row values in different collor
					Console.ForegroundColor = ConsoleColor.Green;
					Console.Write("{0,8}{1,16}",powers_of_two, doublings);
					Console.ForegroundColor = ConsoleColor.Green;
					Console.WriteLine("{0,10}{1,12}", answer, accumulator);
					Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop - 2);
				}
				else
				{
					// Show not accumulated row walues
					Console.ForegroundColor = ConsoleColor.DarkGray;
					Console.Write("{0,8}{1,16}",powers_of_two, doublings);
					Console.ForegroundColor = ConsoleColor.Gray;
					Console.WriteLine("{0,10}{1,12}", answer, accumulator);
					Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop - 2);
				}


				pow--;
			}

			Console.WriteLine();
			Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop + row + 2);
			Console.ResetColor();

			// Set result and reminder
			result = answer;
			if( accumulator < dividend )
			{
				reminder = dividend - accumulator;

				Console.WriteLine(" So " + dividend +
				                  " divided by " + divisor +
				                  " using the Egyptian method is \n " + result +
				                  " remainder (" + dividend + " - " + accumulator +
				                  ") or " + reminder);
				Console.WriteLine();
			}
			else
			{
				reminder = 0;

				Console.WriteLine(" So " + dividend +
				                  " divided by " + divisor +
				                  " using the Egyptian method is \n " + result +
				                  " remainder " + reminder);
				Console.WriteLine();
			}
		}
	}
}

```

{{out| Program Input and Output : Instead of bold and strikeout text format, numbers are represented in different color}}

```txt


 Egyptian division

 Enter value of dividend : 580
 Enter value of divisor  : 34

 powers_of_2     doublings

       1              34
       2              68
       4             136
       8             272
      16             544


 powers_of_2     doublings   answer   accumulator

       1              34        17         578
       2              68        16         544
       4             136        16         544
       8             272        16         544
      16             544        16         544

 So 580 divided by 34 using the Egyptian method is
 17 remainder (580 - 578) or 2


Press any key to continue . . .


```



## D


```D

import std.stdio;

version(unittest) {
    // empty
} else {
    int main(string[] args) {
        import std.conv;

        if (args.length < 3) {
            stderr.writeln("Usage: ", args[0], " dividend divisor");
            return 1;
        }

        ulong dividend = to!ulong(args[1]);
        ulong divisor = to!ulong(args[2]);
        ulong remainder;

        auto ans = egyptian_division(dividend, divisor, remainder);
        writeln(dividend, " / ", divisor, " = ", ans, " rem ", remainder);

        return 0;
    }
}

ulong egyptian_division(ulong dividend, ulong divisor, out ulong remainder) {
    enum SIZE = 64;
    ulong[SIZE] powers;
    ulong[SIZE] doublings;
    int i;

    for (; i<SIZE; ++i) {
        powers[i] = 1 << i;
        doublings[i] = divisor << i;
        if (doublings[i] > dividend) {
            break;
        }
    }

    ulong answer;
    ulong accumulator;

    for (i=i-1; i>=0; --i) {
        if (accumulator + doublings[i] <= dividend) {
            accumulator += doublings[i];
            answer += powers[i];
        }
    }

    remainder = dividend - accumulator;
    return answer;
}

unittest {
    ulong remainder;

    assert(egyptian_division(580UL, 34UL, remainder) == 17UL);
    assert(remainder == 2);
}

```



## Erlang


```erlang
-module(egypt).

-export([ediv/2]).

ediv(A, B) ->
    {Twos, Ds} = genpowers(A, [1], [B]),
    {Quot, C} = accumulate(A, Twos, Ds),
    {Quot, abs(C - A)}.

genpowers(A, [_|Ts], [D|Ds]) when D > A -> {Ts, Ds};
genpowers(A, [T|_] = Twos, [D|_] = Ds) -> genpowers(A, [2*T|Twos], [D*2|Ds]).

accumulate(N, Twos, Ds) -> accumulate(N, Twos, Ds, 0, 0).
accumulate(_, [], [], Q, C) -> {Q, C};
accumulate(N, [T|Ts], [D|Ds], Q, C) when (C + D) =< N -> accumulate(N, Ts, Ds, Q+T, C+D);
accumulate(N, [_|Ts], [_|Ds], Q, C) -> accumulate(N, Ts, Ds, Q, C).

```

{{out}}

```txt

1> egypt:ediv(580,34).
{17,2}

```


=={{header|F_Sharp|F#}}==

```fsharp
// A function to perform Egyptian Division: Nigel Galloway August 11th., 2017
let egyptianDivision N G =
  let rec fn n g = seq{yield (n,g); yield! fn (n+n) (g+g)}
  Seq.foldBack (fun (n,i) (g,e)->if (i<=g) then ((g-i),(e+n)) else (g,e)) (fn 1 G |> Seq.takeWhile(fun (_,g)->g<=N)) (N,0)

```

Which may be used:

```fsharp

let (n,g) = egyptianDivision 580 34
printfn "580 divided by 34 is %d remainder %d" g n

```

{{out}}

```txt

580 divided by 34 is 17 remainder 2

```



## Factor

{{works with|Factor|0.98}}

```factor
USING: assocs combinators formatting kernel make math sequences ;
IN: rosetta-code.egyptian-division

: table ( dividend divisor -- table )
    [ [ 2dup >= ] [ dup , 2 * ] while ] { } make 2nip
    dup length <iota> [ 2^ ] map zip <reversed> ;

: accum ( a b dividend -- c )
    [ 2dup [ first ] bi@ + ] dip < [ [ + ] 2map ] [ drop ] if ;

: ediv ( dividend divisor -- quotient remainder )
    {
        [ table ]
        [ 2drop { 0 0 } ]
        [ drop [ accum ] curry reduce first2 swap ]
        [ drop - abs ]
    } 2cleave ;

580 34 ediv "580 divided by 34 is %d remainder %d\n" printf
```

{{out}}

```txt

580 divided by 34 is 17 remainder 2

```



## FreeBASIC


```freebasic
' version 09-08-2017
' compile with: fbc -s console

Data 580, 34

Dim As UInteger dividend, divisor, answer, accumulator, i
ReDim As UInteger table(1 To 32, 1 To 2)

Read dividend, divisor

i = 1
table(i, 1) = 1 : table(i, 2) = divisor

While table(i, 2) < dividend
    i += 1
    table(i, 1) = table(i -1, 1) * 2
    table(i, 2) = table(i -1, 2) * 2
Wend

i -= 1
answer = table(i, 1)
accumulator = table(i, 2)

While i > 1
    i -= 1
    If table(i,2)+ accumulator <= dividend Then
        answer += table(i, 1)
        accumulator += table(i, 2)
    End If
Wend

Print Str(dividend); " divided by "; Str(divisor); " using Egytian division";
Print " returns "; Str(answer); " mod(ulus) "; Str(dividend-accumulator)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
580 divided by 34 using Egytian division returns 17 mod(ulus) 2
```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

func egyptianDivide(dividend, divisor int) (quotient, remainder int) {
    if dividend < 0 || divisor <= 0 {
        panic("Invalid argument(s)")
    }
    if dividend < divisor {
        return 0, dividend
    }
    powersOfTwo := []int{1}
    doublings := []int{divisor}
    doubling := divisor
    for {
        doubling *= 2
        if doubling > dividend {
            break
        }
        l := len(powersOfTwo)
        powersOfTwo = append(powersOfTwo, powersOfTwo[l-1]*2)
        doublings = append(doublings, doubling)
    }
    answer := 0
    accumulator := 0
    for i := len(doublings) - 1; i >= 0; i-- {
        if accumulator+doublings[i] <= dividend {
            accumulator += doublings[i]
            answer += powersOfTwo[i]
            if accumulator == dividend {
                break
            }
        }
    }
    return answer, dividend - accumulator
}

func main() {
    dividend := 580
    divisor := 34
    quotient, remainder := egyptianDivide(dividend, divisor)
    fmt.Println(dividend, "divided by", divisor, "is", quotient, "with remainder", remainder)
}
```


{{out}}

```txt

580 divided by 34 is 17 with remainder 2

```



## Haskell

Deriving division from (+) and (-) by unfolding from a seed pair (1, divisor) up to a series of successively doubling pairs, and then refolding that series of 'two column rows' back down to a (quotient, remainder) pair, using (0, dividend) as the initial accumulator value. In other words, taking the divisor as a unit, and deriving the binary composition of the dividend in terms of that unit.

```Haskell
import Data.List (unfoldr)

egyptianQuotRem :: Integer -> Integer -> (Integer, Integer)
egyptianQuotRem m n =
  let expansion (i, x)
        | x > m = Nothing
        | otherwise = Just ((i, x), (i + i, x + x))
      collapse (i, x) (q, r)
        | x < r = (q + i, r - x)
        | otherwise = (q, r)
  in foldr collapse (0, m) $ unfoldr expansion (1, n)

main :: IO ()
main = print $ egyptianQuotRem 580 34
```

{{Out}}

```txt
(17,2)
```


We can make the process of calculation more visible by adding a trace layer:


```Haskell
import Data.List (unfoldr)
import Debug.Trace (trace)

egyptianQuotRem :: Int -> Int -> (Int, Int)
egyptianQuotRem m n =
  let rows =
        unfoldr
          (\(i, x) ->
              if x > m
                then Nothing
                else Just ((i, x), (i + i, x + x)))
          (1, n)
  in trace
       (unlines
          [ "Number pair unfolded to series of doubling rows:"
          , show rows
          , "\nRows refolded down to (quot, rem):"
          , show (0, m)
          ])
       foldr
       (\(i, x) (q, r) ->
           if x < r
             then trace
                    (concat
                       ["(+", show i, ", -", show x, ") -> rem ", show (r - x)])
                    (q + i, r - x)
             else (q, r))
       (0, m)
       rows

main :: IO ()
main = print $ egyptianQuotRem 580 34
```

{{Out}}

```txt
Number pair unfolded to series of doubling rows:
[(1,34),(2,68),(4,136),(8,272),(16,544)]

Rows refolded down to (quot, rem):
(0,580)

(+16, -544) -> rem 36
(+1, -34) -> rem 2
(17,2)
```


Another approach, using lazy lists and foldr:


```haskell
doublings = iterate (* 2)

powers = doublings 1

k n (u, v) (ans, acc) =
  if v + ans <= n
    then (v + ans, u + acc)
    else (ans, acc)

egy n = snd . foldr (k n) (0, 0) . zip powers . takeWhile (<= n) . doublings

main :: IO ()
main = print $ egy 580 34
```

{{Out}}

```txt
17
```



## J


Implementation:


```J
doublings=:_1 }. (+:@]^:(> {:)^:a: (,~ 1:))
ansacc=: 1 }. (] + [ * {.@[ >: {:@:+)/@([,.doublings)
egydiv=: (0,[)+1 _1*ansacc
```


Task example:


```J
   580 doublings 34
 1  34
 2  68
 4 136
 8 272
16 544
   580 ansacc 34
17 578
   580 egydiv 34
17 2
```


Notes:
pre
When building the doublings table, we don't actually know we've exceeded our numerator until we are done. This would result in an excess row, so we have to explicitly not include that excess row in our <code>doublings</code> result.

Our "fold" is actually not directly on the result of doublings - for our fold, we add another column where every value is the numerator. This conveniently makes it available for comparison at every stage of the fold and seems a more concise approach than creating a closure. (We do not include this extra value in our <code>ansacc</code> result, of course.)


## Java


```Java

import java.util.ArrayList;
import java.util.List;

public class EgyptianDivision {

    /**
     * Runs the method and divides 580 by 34
     *
     * @param args not used
     */
    public static void main(String[] args) {

        divide(580, 34);

    }

    /**
     * Divides <code>dividend</code> by <code>divisor</code> using the Egyptian Division-Algorithm and prints the
     * result to the console
     *
     * @param dividend
     * @param divisor
     */
    public static void divide(int dividend, int divisor) {

        List<Integer> powersOf2 = new ArrayList<>();
        List<Integer> doublings = new ArrayList<>();

        //populate the powersof2- and doublings-columns
        int line = 0;
        while ((Math.pow(2, line) * divisor) <= dividend) { //<- could also be done with a for-loop
            int powerOf2 = (int) Math.pow(2, line);
            powersOf2.add(powerOf2);
            doublings.add(powerOf2 * divisor);
            line++;
        }

        int answer = 0;
        int accumulator = 0;

        //Consider the rows in reverse order of their construction (from back to front of the List<>s)
        for (int i = powersOf2.size() - 1; i >= 0; i--) {
            if (accumulator + doublings.get(i) <= dividend) {
                accumulator += doublings.get(i);
                answer += powersOf2.get(i);
            }
        }

        System.out.println(String.format("%d, remainder %d", answer, dividend - accumulator));
    }
}


```

{{Out}}

```txt
17, remainder 2
```



## JavaScript


### ES6


```JavaScript
(() => {
    'use strict';

    // EGYPTIAN DIVISION --------------------------------

    // eqyptianQuotRem :: Int -> Int -> (Int, Int)
    const eqyptianQuotRem = (m, n) => {
        const expansion = ([i, x]) =>
            x > m ? (
                Nothing()
            ) : Just([
                [i, x],
                [i + i, x + x]
            ]);
        const collapse = ([i, x], [q, r]) =>
            x < r ? (
                [q + i, r - x]
            ) : [q, r];
        return foldr(
            collapse,
            [0, m],
            unfoldr(expansion, [1, n])
        );
    };

    // TEST ---------------------------------------------

    // main :: IO ()
    const main = () =>
        showLog(
            eqyptianQuotRem(580, 34)
        );
        // -> [17, 2]



    // GENERIC FUNCTIONS --------------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));


    // foldr :: (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(flip(f), a);


    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    const unfoldr = (f, v) => {
        let
            xr = [v, v],
            xs = [];
        while (true) {
            const mb = f(xr[1]);
            if (mb.Nothing) {
                return xs
            } else {
                xr = mb.Just;
                xs.push(xr[0])
            }
        }
    };

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[17,2]
```



## Julia

{{works with|Julia|0.6}}

```julia
function egyptiandivision(dividend::Int, divisor::Int)
    N         = 64
    powers    = Vector{Int}(N)
    doublings = Vector{Int}(N)

    ind = 0
    for i in 0:N-1
        powers[i+1] = 1 << i
        doublings[i+1] = divisor << i
        if doublings[i+1] > dividend ind = i-1; break end
    end

    ans = acc = 0
    for i in ind:-1:0
        if acc + doublings[i+1] ≤ dividend
            acc += doublings[i+1]
            ans += powers[i+1]
        end
    end

    return ans, dividend - acc
end

q, r = egyptiandivision(580, 34)
println("580 ÷ 34 = $q (remains $r)")

using Base.Test

@testset "Equivalence to divrem builtin function" begin
    for x in rand(1:100, 100), y in rand(1:100, 10)
    @test egyptiandivision(x, y) == divrem(x, y)
    end
end
```


{{out}}

```txt
580 ÷ 34 = 17 (remains 2)
Test Summary:                          | Pass  Total
Equivalence to divrem builtin function | 1000   1000
```



## Kotlin


```scala
// version 1.1.4

data class DivMod(val quotient: Int, val remainder: Int)

fun egyptianDivide(dividend: Int, divisor: Int): DivMod {
    require (dividend >= 0 && divisor > 0)
    if (dividend < divisor) return DivMod(0, dividend)
    val powersOfTwo = mutableListOf(1)
    val doublings = mutableListOf(divisor)
    var doubling = divisor
    while (true) {
       doubling *= 2
       if (doubling > dividend) break
       powersOfTwo.add(powersOfTwo[powersOfTwo.lastIndex] * 2)
       doublings.add(doubling)
    }
    var answer = 0
    var accumulator = 0
    for (i in doublings.size - 1 downTo 0) {
        if (accumulator + doublings[i] <= dividend) {
            accumulator += doublings[i]
            answer += powersOfTwo[i]
            if (accumulator == dividend) break
        }
    }
    return DivMod(answer, dividend - accumulator)
}

fun main(args: Array<String>) {
    val dividend = 580
    val divisor = 34
    val (quotient, remainder) = egyptianDivide(dividend, divisor)
    println("$dividend divided by $divisor is $quotient with remainder $remainder")
}
```


{{out}}

```txt

580 divided by 34 is 17 with remainder 2

```



## Lua

{{trans|Python}}

```lua
function egyptian_divmod(dividend,divisor)
    local pwrs, dbls = {1}, {divisor}
    while dbls[#dbls] <= dividend do
        table.insert(pwrs, pwrs[#pwrs] * 2)
        table.insert(dbls, pwrs[#pwrs] * divisor)
    end
    local ans, accum = 0, 0

    for i=#pwrs-1,1,-1 do
        if accum + dbls[i] <= dividend then
            accum = accum + dbls[i]
            ans = ans + pwrs[i]
        end
    end

    return ans, math.abs(accum - dividend)
end

local i, j = 580, 34
local d, m = egyptian_divmod(i, j)
print(i.." divided by "..j.." using the Egyptian method is "..d.." remainder "..m)
```

{{out}}

```txt
580 divided by 34 using the Egyptian method is 17 remainder 2
```


=={{header|Modula-2}}==

```modula2
MODULE EgyptianDivision;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

PROCEDURE EgyptianDivision(dividend,divisor : LONGCARD; VAR remainder : LONGCARD) : LONGCARD;
CONST
    SZ = 64;
VAR
    powers,doublings : ARRAY[0..SZ] OF LONGCARD;
    answer,accumulator : LONGCARD;
    i : INTEGER;
BEGIN
    FOR i:=0 TO SZ-1 DO
        powers[i] := 1 SHL i;
        doublings[i] := divisor SHL i;
        IF doublings[i] > dividend THEN
            BREAK
        END
    END;

    answer := 0;
    accumulator := 0;
    FOR i:=i-1 TO 0 BY -1 DO
        IF accumulator + doublings[i] <= dividend THEN
            accumulator := accumulator + doublings[i];
            answer := answer + powers[i]
        END
    END;

    remainder := dividend - accumulator;
    RETURN answer
END EgyptianDivision;

VAR
    buf : ARRAY[0..63] OF CHAR;
    div,rem : LONGCARD;
BEGIN
    div := EgyptianDivision(580, 34, rem);
    FormatString("580 divided by 34 is %l remainder %l\n", buf, div, rem);
    WriteString(buf);

    ReadChar
END EgyptianDivision.
```



## Perl

{{trans|Perl 6}}

```perl
sub egyptian_divmod {
    my($dividend, $divisor) = @_;
    die "Invalid divisor" if $divisor <= 0;

    my @table = ($divisor);
    push @table, 2*$table[-1] while $table[-1] <= $dividend;

    my $accumulator = 0;
    for my $k (reverse 0 .. $#table) {
        next unless $dividend >= $table[$k];
        $accumulator += 1 << $k;
        $dividend    -= $table[$k];
    }
    $accumulator, $dividend;
}

for ([580,34], [578,34], [7532795332300578,235117]) {
    my($n,$d) = @$_;
    printf "Egyption divmod %s %% %s = %s remainder %s\n", $n, $d, egyptian_divmod( $n, $d )
}
```

{{out}}

```txt
Egyption divmod 580 % 34 = 17 remainder 2
Egyption divmod 578 % 34 = 17 remainder 0
Egyption divmod 7532795332300578 % 235117 = 32038497141 remainder 81
```



## Perl 6

{{works with|Rakudo|2017.07}}

### Normal version

Only works with positive real numbers, not negative or complex.

```perl6
sub egyptian-divmod (Real $dividend is copy where * >= 0, Real $divisor where * > 0) {
    my $accumulator = 0;
    ([1, $divisor], { [.[0] + .[0], .[1] + .[1]] } … ^ *.[1] > $dividend)
      .reverse.map: { $dividend -= .[1], $accumulator += .[0] if $dividend >= .[1] }
    $accumulator, $dividend;
}

#TESTING
for 580,34, 578,34, 7532795332300578,235117 -> $n, $d {
    printf "%s divmod %s = %s remainder %s\n",
        $n, $d, |egyptian-divmod( $n, $d )
}
```

{{out}}

```txt
580 divmod 34 = 17 remainder 2
578 divmod 34 = 17 remainder 0
7532795332300578 divmod 235117 = 32038497141 remainder 81
```


===More "Egyptian" version===
As a preceding version was determined to be "let's just say ... not Egyptian" we submit an alternate which is hopefully more "Egyptian". Now only handles positive Integers up to 10 million, mostly due to limitations on Egyptian notation for numbers.

Note: if the below is just a mass of "unknown glyph" boxes, try [https://www.google.com/get/noto/help/install/ installing] Googles free [https://www.google.com/get/noto/#sans-egyp Noto Sans Egyptian Hieroglyphs font].

This is intended to be humorous and should not be regarded as good (or even sane) programming practice. That being said, 𓂽 & 𓂻 really are the ancient Egyptian symbols for addition and subtraction, and the Egyptian number notation is as accurate as possible. Everything else owes more to whimsy than rigor.

```perl6
my (\𓄤, \𓄊, \𓎆, \𓄰) = (0, 1, 10, 10e7);
sub infix:<𓂽> { $^𓃠 + $^𓃟 }
sub infix:<𓂻> { $^𓃲 - $^𓆊 }
sub infix:<𓈝> { $^𓃕 < $^𓃢 }
sub 𓁶 (Int \𓆉) {
    my \𓁢 = [«'' 𓏺 𓏻 𓏼 𓏽 𓏾 𓏿 𓐀 𓐁 𓐂»], [«'' 𓎆 𓎏 𓎐 𓎑 𓎊 𓎋 𓎌 𓎍 𓎎»],
      [«'' 𓍢 𓍣 𓍤 𓍥 𓍦 𓍧 𓍨 𓍩 𓍪»], [«'' 𓆼 𓆽 𓆾 𓆿 𓇀 𓇁 𓇂 𓇃 𓇄»],
      [«'' 𓂭 𓂮 𓂯 𓂰 𓂱 𓂲 𓂳 𓂴 𓂵»], ['𓆐' Xx ^𓎆], ['𓁨' Xx ^𓎆];
    ([~] 𓆉.polymod( 𓎆 xx * ).map( { 𓁢[$++;$_] } ).reverse) || '𓄤'
}

sub infix:<𓅓> (Int $𓂀 is copy where 𓄤 𓂻 𓄊 𓈝 * 𓈝 𓄰, Int \𓌳 where 𓄤 𓈝 * 𓈝 𓄰) {
    my $𓎦 = 𓄤;
    ([𓄊,𓌳], { [.[𓄤] 𓂽 .[𓄤], .[𓄊] 𓂽 .[𓄊]] } … ^$𓂀 𓈝 *.[𓄊])
      .reverse.map: { $𓂀 𓂻= .[𓄊], $𓎦 𓂽= .[𓄤] if .[𓄊] 𓈝 ($𓂀 𓂽 𓄊) }
    $𓎦, $𓂀;
}

#TESTING
for 580,34, 578,34, 2300578,23517 -> \𓃾, \𓆙 {
    printf "%s divmod %s = %s remainder %s =OR= %s 𓅓 %s = %s remainder %s\n",
        𓃾, 𓆙, |(𓃾 𓅓 𓆙), (𓃾, 𓆙, |(𓃾 𓅓 𓆙))».&𓁶;
}
```


{{out}}

```txt
580 divmod 34 = 17 remainder 2 =OR= 𓍦𓎍 𓅓 𓎐𓏽 = 𓎆𓐀 remainder 𓏻
578 divmod 34 = 17 remainder 0 =OR= 𓍦𓎌𓐁 𓅓 𓎐𓏽 = 𓎆𓐀 remainder 𓄤
2300578 divmod 23517 = 97 remainder 19429 =OR= 𓁨𓁨𓆐𓆐𓆐𓍦𓎌𓐁 𓅓 𓂮𓆾𓍦𓎆𓐀 = 𓎎𓐀 remainder 𓂭𓇄𓍥𓎏𓐂
```



## Phix


```Phix
procedure egyptian_division(integer dividend, divisor)
integer p2 = 1, dbl = divisor, ans = 0, accum = 0
sequence p2s = {}, dbls = {}, args
    while dbl<=dividend do
        p2s = append(p2s,p2)
        dbls = append(dbls,dbl)
        dbl += dbl
        p2 += p2
    end while
    for i=length(p2s) to 1 by -1 do
        if accum+dbls[i]<=dividend then
            accum += dbls[i]
            ans += p2s[i]
        end if
    end for
    args = {dividend,divisor,ans,abs(accum-dividend)}
    printf(1,"%d divided by %d is: %d remainder %d\n",args)
end procedure

egyptian_division(580,34)
```

{{out}}

```txt

580 divided by 34 is: 17 remainder 2

```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(de divmod (Dend Disor)
   (cons (/ Dend Disor) (% Dend Disor)) )
(de egyptian (Dend Disor)
   (let
      (P 0
         D Disor
         S
         (make
            (while (>= Dend (setq @@ (+ D D)))
               (yoke
                  (cons
                     (** 2 (swap 'P (inc P)))
                     (swap 'D @@) ) ) ) )
         P (** 2 P) )
      (mapc
         '((L)
            (and
               (>= Dend (+ D (cdr L)))
               (inc 'P (car L))
               (inc 'D (cdr L)) ) )
         S )
      (cons P (abs (- Dend D))) ) )
(for N 1000
   (let (A (rand 1 1000)  B (rand 1 A))
      (test (divmod A B) (egyptian A B)) ) )
(println (egyptian 580 34))
```


{{out}}

```txt
(17 . 2)
```



## Python


### More idiomatic
 <!-- When compared to a Haskel translation -->

```python
from itertools import product

def egyptian_divmod(dividend, divisor):
    assert divisor != 0
    pwrs, dbls = [1], [divisor]
    while dbls[-1] <= dividend:
        pwrs.append(pwrs[-1] * 2)
        dbls.append(pwrs[-1] * divisor)
    ans, accum = 0, 0
    for pwr, dbl in zip(pwrs[-2::-1], dbls[-2::-1]):
        if accum + dbl <= dividend:
            accum += dbl
            ans += pwr
    return ans, abs(accum - dividend)

if __name__ == "__main__":
    # Test it gives the same results as the divmod built-in
    for i, j in product(range(13), range(1, 13)):
            assert egyptian_divmod(i, j) == divmod(i, j)
    # Mandated result
    i, j = 580, 34
    print(f'{i} divided by {j} using the Egyption method is %i remainder %i'
          % egyptian_divmod(i, j))
```


'''Sample output'''

```txt
580 divided by 34 using the Egyption method is 17 remainder 2

```



### Functional

Expressing the summing catamorphism in terms of '''functools.reduce''', and the preliminary expansion (by repeated addition to self) in terms of an '''unfoldl''' function, which is dual to reduce, and constructs a list from a seed value.

Multiplication and division operators are both avoided, in the spirit of the Rhind Papyrus derivations of both (*) and (/) from plain addition and subtraction.

Also in deference to the character of the Rhind methods, the ('''unfoldl''') unfolding of the seed values to a list of progressively doubling rows is recursively defined, and mutation operations are avoided. The efficiency of the Egyptian method's exponential expansion means that there is no need here, even with larger numbers, to compress space by using an imperative translation of the higher-order unfold function.

{{Trans|Haskell}}

```python
'''Quotient and remainder of division by the Rhind papyrus method.'''

from functools import reduce


# eqyptianQuotRem :: Int -> Int -> (Int, Int)
def eqyptianQuotRem(m):
    '''Quotient and remainder derived by the Eqyptian method.'''

    def expansion(xi):
        '''Doubled value, and next power of two - both by self addition.'''
        x, i = xi
        return Nothing() if x > m else Just(
            ((x + x, i + i), xi)
        )

    def collapse(qr, ix):
        '''Addition of a power of two to the quotient,
           and subtraction of a paired value from the remainder.'''
        i, x = ix
        q, r = qr
        return (q + i, r - x) if x < r else qr

    return lambda n: reduce(
        collapse,
        unfoldl(expansion)(
            (1, n)
        ),
        (0, m)
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    print(
        eqyptianQuotRem(580)(34)
    )


# GENERIC FUNCTIONS ---------------------------------------


# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldl(f):
    '''Dual to reduce or foldl.
       Where a fold reduces a list to a summary value,
       unfoldl builds a list from a seed value.
       When f returns Just(a, b), a is appended to the list,
       and the residual b becomes the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(v):
        xr = v, v
        xs = []
        while True:
            mb = f(xr[0])
            if mb.get('Nothing'):
                return xs
            else:
                xr = mb.get('Just')
                xs.insert(0, xr[1])
        return xs
    return lambda x: go(x)


# MAIN ----------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
(17, 2)
```



## Racket


```racket
#lang racket

(define (quotient/remainder-egyptian dividend divisor (trace? #f))
  (define table
    (for*/list ((power_of_2 (sequence-map (curry expt 2) (in-naturals)))
                (doubling (in-value (* divisor power_of_2)))
                #:break (> doubling dividend))
      (list power_of_2 doubling)))

  (when trace?
    (displayln "Table\npow_2\tdoubling")
    (for ((row table)) (printf "~a\t~a~%" (first row) (second row))))

  (define-values (answer accumulator)
    (for*/fold ((answer 0) (accumulator 0))
               ((row (reverse table))
                (acc′ (in-value (+ accumulator (second row)))))
      (when trace? (printf "row:~a\tans/acc:~a ~a\t" row answer accumulator))
      (cond
        [(<= acc′ dividend)
         (define ans′ (+ answer (first row)))
         (when trace? (printf "~a <= ~a -> ans′/acc′:~a ~a~%" acc′ dividend ans′ acc′))
         (values ans′ acc′)]
        [else
         (when trace? (printf "~a > ~a [----]~%" acc′ dividend))
         (values answer accumulator)])))

  (values answer (- dividend accumulator)))

(module+ test
  (require rackunit)
  (let-values (([q r] (quotient/remainder-egyptian 580 34)))
    (check-equal? q 17)
    (check-equal? r 2))

  (let-values (([q r] (quotient/remainder-egyptian 192 3)))
    (check-equal? q 64)
    (check-equal? r 0)))

(module+ main
  (quotient/remainder-egyptian 580 34 #t))
```


{{out}}


```txt
Table
pow_2	doubling
1	34
2	68
4	136
8	272
16	544
row:(16 544)	ans/acc:0 0	544 <= 580 -> ans′/acc′:16 544
row:(8 272)	ans/acc:16 544	816 > 580 [----]
row:(4 136)	ans/acc:16 544	680 > 580 [----]
row:(2 68)	ans/acc:16 544	612 > 580 [----]
row:(1 34)	ans/acc:16 544	578 <= 580 -> ans′/acc′:17 578
17
2
```



## REXX

Only addition and subtraction is used in this version of the Egyptian division method.

```rexx
/*REXX program performs division on positive integers using the Egyptian division method*/
numeric digits 1000                              /*support gihugic numbers & be gung-ho.*/
parse arg n d .                                  /*obtain optional arguments from the CL*/
if d=='' | d==","  then do;  n= 580;    d= 34    /*Not specified?  Then use the defaults*/
                        end
call EgyptDiv n, d                               /*invoke the Egyptian Division function*/
parse var result q r                             /*extract the quotient & the remainder.*/
say n   ' divided by '       d       " is "       q       ' with a remainder of '      r
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
EgyptDiv: procedure;  parse arg num,dem          /*obtain the numerator and denominator.*/
          p= 1;                       t= dem     /*initialize the double & power values.*/
                        do #=1  until t>num      /*construct the power & doubling lists.*/
                        pow.#= p;     p= p + p   /*build power  entry; bump power value.*/
                        dbl.#= t;     t= t + t   /*  "  doubling  "  ;   " doubling val.*/
                        end   /*#*/
          acc=0;  ans=0                          /*initialize accumulator & answer to 0 */
                        do s=#   by -1   for #   /* [↓]  process the table "backwards". */
                        sum= acc + dbl.s         /*compute the sum (to be used for test)*/
                        if sum>num  then iterate /*Is sum to big?  Then ignore this step*/
                        acc= sum                 /*use the "new" sum for the accumulator*/
                        ans= ans + pow.s         /*calculate the (newer) running answer.*/
                        end   /*s*/
          return ans  num-acc                    /*return the answer and the remainder. */
```

{{out|output|text=  when using the default inputs:}}

```txt

580  divided by  34  is  17  with a remainder of  2

```

{{out|output|text=  when using the input of:     <tt> 9876543210111222333444555666777888999   13579 </tt>}}

```txt

9876543210111222333444555666777888999  divided by  13579  is  727339510281406755537562093436769  with a remainder of  2748

```



## Ring


```ring

load "stdlib.ring"

table = newlist(32, 2)
dividend = 580
divisor = 34

i = 1
table[i][1] = 1
table[i][2] = divisor

while table[i] [2] < dividend
      i = i + 1
      table[i][1] = table[i -1] [1] * 2
      table[i][2] = table[i -1] [2] * 2
end
i = i - 1
answer = table[i][1]
accumulator = table[i][2]

while i > 1
      i = i - 1
      if table[i][2]+ accumulator <= dividend
         answer = answer + table[i][1]
         accumulator = accumulator + table[i][2]
      ok
end

see string(dividend)  + " divided by " + string(divisor) + " using egytian division" + nl
see " returns " + string(answer) + " mod(ulus) " + string(dividend-accumulator)

```

Output:

```txt

580 divided by 34 using egytian division
returns 17 mod(ulus) 2

```



## Ruby


```ruby
def egyptian_divmod(dividend, divisor)
  table = [[1, divisor]]
  table << table.last.map{|e| e*2} while table.last.first * 2 <= dividend
  answer, accumulator = 0, 0
  table.reverse_each do |pow, double|
    if accumulator + double <= dividend
      accumulator += double
      answer += pow
    end
  end
  [answer, dividend - accumulator]
end

puts "Quotient = %s Remainder = %s" % egyptian_divmod(580, 34)

```

{{out}}

```txt
Quotient = 17 Remainder = 2

```



## Rust


```rust
fn egyptian_divide(dividend: u32, divisor: u32) -> (u32, u32) {
    let dividend = dividend as u64;
    let divisor = divisor as u64;

    let pows = (0..32).map(|p| 1 << p);
    let doublings = (0..32).map(|p| divisor << p);

    let (answer, sum) = doublings
        .zip(pows)
        .rev()
        .skip_while(|(i, _)| i > &dividend )
        .fold((0, 0), |(answer, sum), (double, power)| {
            if sum + double < dividend {
                (answer + power, sum + double)
            } else {
                (answer, sum)
            }
        });

    (answer as u32, (dividend - sum) as u32)
}

fn main() {
    let (div, rem) = egyptian_divide(580, 34);
    println!("580 divided by 34 is {} remainder {}", div, rem);
}
```

{{out}}

```txt
580 divided by 34 is 17 remainder 2
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/sYSdo9u/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/3yry7OurSQS72xNMK0GMEg Scastie (remote JVM)].

```Scala
object EgyptianDivision extends App {

  private def divide(dividend: Int, divisor: Int): Unit = {
    val powersOf2, doublings = new collection.mutable.ListBuffer[Integer]

    //populate the powersof2- and doublings-columns
    var line = 0
    while ((math.pow(2, line) * divisor) <= dividend) {
      val powerOf2 = math.pow(2, line).toInt
      powersOf2 += powerOf2
      doublings += (powerOf2 * divisor)
      line += 1
    }

    var answer, accumulator = 0
    //Consider the rows in reverse order of their construction (from back to front of the List)
    var i = powersOf2.size - 1
    for (i <- powersOf2.size - 1 to 0 by -1)
      if (accumulator + doublings(i) <= dividend) {
        accumulator += doublings(i)
        answer += powersOf2(i)
      }

    println(f"$answer%d, remainder ${dividend - accumulator}%d")
  }

  divide(580, 34)

}
```


## Sidef

{{trans|Ruby}}

```ruby
func egyptian_divmod(dividend, divisor) {
  var table = [[1, divisor]]
  table << table[-1].map{|e| 2*e } while (2*table[-1][0] <= dividend)
  var (answer, accumulator) = (0, 0)
  table.reverse.each { |pair|
    var (pow, double) = pair...
    if (accumulator + double <= dividend) {
      accumulator += double
      answer += pow
    }
  }
  return (answer, dividend - accumulator)
}

say ("Quotient = %s Remainder = %s" % egyptian_divmod(580, 34))
```

{{out}}

```txt

Quotient = 17 Remainder = 2

```



## VBA


```vb
Option Explicit

Private Type MyTable
    powers_of_2 As Long
    doublings As Long
End Type

Private Type Assemble
    answer As Long
    accumulator As Long
End Type

Private Type Division
    Quotient As Long
    Remainder As Long
End Type

Private Type DivEgyp
    Dividend As Long
    Divisor As Long
End Type

Private Deg As DivEgyp

Sub Main()
Dim d As Division
    Deg.Dividend = 580
    Deg.Divisor = 34
    d = Divise(CreateTable)
    Debug.Print "Quotient = " & d.Quotient & " Remainder = " & d.Remainder
End Sub

Private Function CreateTable() As MyTable()
Dim t() As MyTable, i As Long
    Do
        i = i + 1
        ReDim Preserve t(i)
        t(i).powers_of_2 = 2 ^ (i - 1)
        t(i).doublings = Deg.Divisor * t(i).powers_of_2
    Loop While 2 * t(i).doublings <= Deg.Dividend
    CreateTable = t
End Function

Private Function Divise(t() As MyTable) As Division
Dim a As Assemble, i As Long
    a.accumulator = 0
    a.answer = 0
    For i = UBound(t) To LBound(t) Step -1
        If a.accumulator + t(i).doublings <= Deg.Dividend Then
            a.accumulator = a.accumulator + t(i).doublings
            a.answer = a.answer + t(i).powers_of_2
        End If
    Next
    Divise.Quotient = a.answer
    Divise.Remainder = Deg.Dividend - a.accumulator
End Function
```

{{out}}

```txt
Quotient = 17 Remainder = 2
```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1

    Function EgyptianDivision(dividend As ULong, divisor As ULong, ByRef remainder As ULong) As ULong
        Const SIZE = 64
        Dim powers(SIZE) As ULong
        Dim doublings(SIZE) As ULong
        Dim i = 0

        While i < SIZE
            powers(i) = 1 << i
            doublings(i) = divisor << i
            If doublings(i) > dividend Then
                Exit While
            End If
            i = i + 1
        End While

        Dim answer As ULong = 0
        Dim accumulator As ULong = 0
        i = i - 1
        While i >= 0
            If accumulator + doublings(i) <= dividend Then
                accumulator += doublings(i)
                answer += powers(i)
            End If
            i = i - 1
        End While

        remainder = dividend - accumulator
        Return answer
    End Function

    Sub Main(args As String())
        If args.Length < 2 Then
            Dim name = Reflection.Assembly.GetEntryAssembly().Location
            Console.Error.WriteLine("Usage: {0} dividend divisor", IO.Path.GetFileNameWithoutExtension(name))
            Return
        End If

        Dim dividend = CULng(args(0))
        Dim divisor = CULng(args(1))
        Dim remainder As ULong

        Dim ans = EgyptianDivision(dividend, divisor, remainder)
        Console.WriteLine("{0} / {1} = {2} rem {3}", dividend, divisor, ans, remainder)
    End Sub

End Module
```

{{out}}

```txt
580 / 34 = 17 rem 2
```



## zkl


```zkl
fcn egyptianDivmod(dividend,divisor){
   table:=[0..].pump(List, 'wrap(n){	// (2^n,divisor*2^n)
      r:=T( p:=(2).pow(n), s:=divisor*p); (s<=dividend) and r or Void.Stop });
   accumulator:=0;
   foreach p2,d in (table.reverse()){
      if(dividend>=d){ accumulator+=p2; dividend-=d; }
   }
   return(accumulator,dividend);
}
```


```zkl
foreach dividend,divisor in (T(T(580,34), T(580,17), T(578,34), T(7532795332300578,235117))){
  println("%d %% %d = %s".fmt(dividend,divisor,egyptianDivmod(dividend,divisor)));
}
```

{{out}}

```txt

580 % 34 = L(17,2)
580 % 17 = L(34,2)
578 % 34 = L(17,0)
7532795332300578 % 235117 = L(32038497141,81)

```

