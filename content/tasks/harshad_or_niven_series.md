+++
title = "Harshad or Niven series"
description = ""
date = 2019-08-25T21:45:18Z
aliases = []
[extra]
id = 13183
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cobol",
  "coldfusion",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "factor",
  "fbsl",
  "fortran",
  "freebasic",
  "frink",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lolcode",
  "lua",
  "min",
  "mlite",
  "netrexx",
  "nim",
  "objeck",
  "oforth",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "swift",
  "tcl",
  "ubasic_4th",
  "vba",
  "vbscript",
  "visual_foxpro",
  "whitespace",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

The [http://mathworld.wolfram.com/HarshadNumber.html Harshad] or Niven numbers are positive integers ≥ 1 that are divisible by the sum of their digits.

For example,   '''42'''   is a [[oeis:A005349|Harshad number]] as   '''42'''   is divisible by   ('''4''' + '''2''')   without remainder.

Assume that the series is defined as the numbers in increasing order.


## Task

The task is to create a function/method/procedure to generate successive members of the Harshad sequence.

Use it to list the first twenty members of the sequence and list the first Harshad number greater than 1000.

Show your output here.





## 360 Assembly


```360asm
*        Harshad or Niven series - 01/05/2019
NIVEN    CSECT
         USING  NIVEN,R13          base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R7,2               j=2
LOOP     MVC    PG,=CL80' '        clear buffer
         LA     R10,PG             @pg
         LA     R8,0               n=0
       IF C,R7,EQ,=A(2) THEN       if j=2
         LA     R9,20                nn=20
         LA     R6,1                 i=1
       ELSE     ,                  else
         LA     R9,1                 nn=1
         LA     R6,1001              i=1001
       ENDIF    ,                  end if
       DO WHILE=(CR,R8,LT,R9)      do i=1 by 1 while(n<nn)
         BAL    R14,HARSHAD          call harshad(i)
       IF   LTR,R1,Z,R1 THEN         if rc=0 then
         LA     R8,1(R8)               n++
         XDECO  R6,XDEC                edit i
         MVC    0(4,R10),XDEC+8        output i
         LA     R10,4(R10)             @pg+=4
       ENDIF    ,                    end if
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XPRNT  PG,L'PG            print buffer
         BCT    R7,LOOP            j=j-1; loop if j<>0
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
HARSHAD  EQU    *                  harshad(i)
         CVD    R6,PACKED          convert to packed   PL8
         UNPK   ZONED,PACKED       packed PL8 to zoned ZL16
         LA     R1,ZONED           @c
         XR     R4,R4              sum=0; m=1
       DO WHILE=(C,R1,LE,=A(ZONED+15)) do m=1 to 16
         NI     0(R1),X'0F'          c(m) : character to integer
         XR     R2,R2                ~
         IC     R2,0(R1)             c(m)
         AR     R4,R2                sum=sum+c(m)
         LA     R1,1(R1)             @c++
       ENDDO    ,                  enddo m
         XR     R2,R2              ~
         LR     R3,R6              i
         DR     R2,R4              i/sum
         LR     R1,R2              rc=mod(i,sum)
         BR     R14                return to caller
PACKED   DS     PL8                packed decimal (15num)
ZONED    DS     ZL16               zoned  decimal (16num)
PG       DS     CL80               buffer
XDEC     DS     CL12               temp xdeco
         REGEQU                    symbolic registers
         END    NIVEN
```

```txt

   1   2   3   4   5   6   7   8   9  10  12  18  20  21  24  27  30  36  40  42
1002

```




## Ada


```Ada
with Ada.Text_IO;

procedure Harshad is

   function Next(N: in out Positive) return Positive is

      function Sum_Of_Digits(N: Natural) return Natural is
	 ( if N = 0 then 0 else ((N mod 10) + Sum_Of_Digits(N / 10)) );

   begin
      while not (N mod Sum_Of_Digits(N) = 0) loop
	 N := N + 1;
      end loop;
      return N;
   end Next;

   Current: Positive := 1;

begin
   for I in 1 .. 20 loop
      Ada.Text_IO.Put(Integer'Image(Next(Current)));
      Current := Current + 1;
   end loop;
   Current := 1000 + 1;
   Ada.Text_IO.Put_Line(" ..." & Integer'Image(Next(Current)));
end Harshad;
```

```txt
 1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 ... 1002
```



## ALGOL 68


```algol68
BEGIN
   PROC digit sum = (INT i) INT :
   BEGIN
      INT res := i %* 10, h := i;
      WHILE (h %:= 10) > 0 DO res +:= h %* 10 OD;
      res
   END;
   INT  found := 0;
   FOR i WHILE found < 20 DO
      (i %* digit sum (i) = 0 | found +:= 1; printf (($g(0)", "$, i)) ) OD;
   FOR i FROM 1001 DO
      (i %* digit sum (i) = 0 | printf (($g(0)l$, i)); stop) OD
END
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42, 1002

```



## AutoHotkey


```AutoHotkey
H := []
n := 1

Loop
	n := (H[A_Index] := NextHarshad(n)) + 1
until  H[H.MaxIndex()] > 1000

Loop, 20
	Out .= H[A_Index] ", "

MsgBox, % Out ". . . " H[H.MaxIndex()]

NextHarshad(n) {
	Loop, {
		Loop, Parse, n
			sum += A_LoopField
		if (!Mod(n, sum))
			return n
		n++, sum := ""
	}
}
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42, . . . 1002
```



## AWK


```AWK
#!/usr/bin/awk -f
BEGIN {
	k=0; n=0;
	printf("First twenty Harshad numbers are:\n   ");
	while (k<20) {
		if (isharshad(++n)) {
			printf("%i ",n);
			++k;
		}
	}
	n = 1000;
	while (!isharshad(++n));
	printf("\nFirst Harshad number larger than 1000 is \n   %i\n",n);
}

function isharshad(n) {
	s = 0;
	for (i=0; i<length(n); ) {
		s+=substr(n,++i,1);
	}
	return !(n%s);
}
```

```txt
First twenty Harshad numbers are:
   1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
First Harshad number larger than 1000 is
   1002
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

for /l %%i in (1,1,20) do (
  call:harshad
  echo Harshad number %%i - !errorlevel!
)

:loop
call:harshad
if %errorlevel% leq 1000 goto loop
echo First Harshad number greater than 1000: %errorlevel%
pause>nul
exit /b

:harshad
if "%harshadnum%"=="" set harshadnum=0
set /a harshadnum+=1
call:strlength %harshadnum%

set harshadsum=0
for /l %%i in (0,1,%errorlevel%) do set /a harshadsum+=!harshadnum:~%%i,1!

set /a isharshad=%harshadnum% %% %harshadsum%
if %isharshad%==0 exit /b %harshadnum%
goto harshad

:strlength
setlocal enabledelayedexpansion
set tempcount=1
set str=%1
:strlengthloop
set /a length=%tempcount%-1
if "!str:~%tempcount%,1!"=="" endlocal && exit /b %length%
set /a tempcount+=1
goto strlengthloop

```

```txt

Harshad number 1 - 1
Harshad number 2 - 2
Harshad number 3 - 3
Harshad number 4 - 4
Harshad number 5 - 5
Harshad number 6 - 6
Harshad number 7 - 7
Harshad number 8 - 8
Harshad number 9 - 9
Harshad number 10 - 10
Harshad number 11 - 12
Harshad number 12 - 18
Harshad number 13 - 20
Harshad number 14 - 21
Harshad number 15 - 24
Harshad number 16 - 27
Harshad number 17 - 30
Harshad number 18 - 36
Harshad number 19 - 40
Harshad number 20 - 42
First Harshad number greater than 1000: 1002

```




## BBC BASIC


```bbcbasic
      I%=1:CNT%=0
      WHILE TRUE
        IF FNHarshad(I%) THEN
          IF CNT%<20 PRINT ;I%;" ";:CNT%+=1
          IF I%>1000 PRINT ;I%:EXIT WHILE
        ENDIF
        I%+=1
      ENDWHILE
      END

      DEF FNHarshad(num%)
      LOCAL sum%,tmp%
      tmp%=num%
      sum%=0
      WHILE (tmp%>0)
        sum%+=tmp% MOD 10
        tmp%/=10
      ENDWHILE
      =(num% MOD sum%)=0
```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 1002
```



## Befunge


```befunge
45*1>::01-\>:55+%\vv\0<
>\1+^  +  <|:/<+55<`  :
^_>1-\:.v@1>\:0\`#v_+\^
>^1\,+55<.^_:#%$:#<"}"v
^!:\_    ^###<    !`*8<
```


```txt
1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
1002
```



## C


```c
#include <stdio.h>

static int digsum(int n)
{
    int sum = 0;
    do { sum += n % 10; } while (n /= 10);
    return sum;
}

int main(void)
{
    int n, done, found;

    for (n = 1, done = found = 0; !done; ++n) {
        if (n % digsum(n) == 0) {
            if (found++ < 20) printf("%d ", n);
            if (n > 1000) done = printf("\n%d\n", n);
        }
    }

    return 0;
}
```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```


## C#

```c#

using System;
using System.Collections.Generic;

namespace Harshad
{
    class Program
    {
        public static bool IsHarshad(int n)
        {
            char[] inputChars = n.ToString().ToCharArray();
            IList<byte> digits = new List<byte>();

            foreach (char digit in inputChars)
            {
                digits.Add((byte)Char.GetNumericValue(digit));
            }

            if (n < 1)
            {
                return false;
            }

            int sum = 0;

            foreach (byte digit in digits)
            {
                sum += digit;
            }

            return n % sum == 0;
        }

        static void Main(string[] args)
        {
            int i = 1;
            int count = 0;

            while (true)
            {
                if (IsHarshad(i))
                {
                    count++;

                    if (count <= 20)
                    {
                        Console.Write(string.Format("{0} ", i));
                    }
                    else if (i > 1000)
                    {
                        Console.Write(string.Format("{0} ", i));
                        break;
                    }
                }

                i++;
            }

            Console.ReadKey();
        }
    }
}

```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 1002
```



### Shorter solution


```c#
using System.Collections.Generic;
using static System.Linq.Enumerable;
using static System.Console;

public static class Program
{
    public static void Main()
    {
        WriteLine(string.Join(" ", From(1).Where(IsHarshad).Take(20)));
        WriteLine(From(1001).First(IsHarshad));
    }

    static bool IsHarshad(this int i) => i % i.Digits().Sum() == 0;

    static IEnumerable<int> From(int start) {
        for (int i = start; ; i++) yield return i;
    }

    static IEnumerable<int> Digits(this int n) {
        for (; n > 0; n /= 10) yield return n % 10;
    }
}
```



## C++


```cpp
#include <vector>
#include <iostream>

int sumDigits ( int number ) {
   int sum = 0 ;
   while ( number != 0 ) {
      sum += number % 10 ;
      number /= 10 ;
   }
   return sum ;
}

bool isHarshad ( int number ) {
   return number % ( sumDigits ( number ) ) == 0 ;
}

int main( ) {
   std::vector<int> harshads ;
   int i = 0 ;
   while ( harshads.size( ) != 20 ) {
      i++ ;
      if ( isHarshad ( i ) )
	 harshads.push_back( i ) ;
   }
   std::cout << "The first 20 Harshad numbers:\n" ;
   for ( int number : harshads )
      std::cout << number << " " ;
   std::cout << std::endl ;
   int start = 1001 ;
   while ( ! ( isHarshad ( start ) ) )
      start++ ;
   std::cout << "The smallest Harshad number greater than 1000 : " << start << '\n' ;
   return 0 ;
}
```

```txt

The first 20 Harshad numbers:
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
The smallest Harshad number greater than 1000 : 1002

```



## Clojure


```Clojure
(defn digsum [n acc]
  (if (zero? n) acc
      (digsum (quot n 10) (+ acc (mod n 10)))))

(let [harshads (filter
                 #(zero? (mod % (digsum % 0)))
                 (iterate inc 1))]
  (prn (take 20 harshads))
  (prn (first (filter #(> % 1000) harshads))))
```

```txt
(1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)
1002
```






## COBOL

```cobol
identification division.
program-id. harshad.
environment division.
data division.
working-storage section.
*> for storing first 20 harshad-niven numbers
01  harshads.
    03  harshad pic 9(5)    occurs 20 times indexed by niven.

*> numbers tested for harshad-niven-ness.
01  first-num   pic 9(5).
01  second-num  pic 9(5).

*> loop counter
01  i   pic 9(5).

*> for calculating sum of digits
01  div pic 9(5).
01  mod pic 9(5).
01  tot pic 9(5).

*> for harshad-niven calculation and display
01  harshad-div pic 9(5).
01  harshad-mod pic 9(5).
    88  evenly-divisible    value 0.
01  harshad-disp    pic zzzz9.
01  harshad-result  pic 9(5).

*> for selecting what to do with results of harshad calculation
01  pass        pic 9.
    88  first-pass  value 1.
    88  second-pass value 2.

procedure division.
10-main section.
    move 1 to pass.
    set niven to 1.
    perform 20-calculate-harshad with test before varying first-num from 1 by 1 until niven = 21.

    move 2 to pass.
    move first-num to second-num.
    perform 20-calculate-harshad with test after varying first-num from second-num by 1 until harshad-result > 1000.

    perform with test after varying i from 1 by 1 until i = 20
        move harshad(i) to harshad-disp
        display function trim(harshad-disp) space with no advancing
    end-perform.

    move harshad-result to harshad-disp.
    display "... " function trim(harshad-disp).
    stop run.

20-calculate-harshad.
    move first-num to div.
    move zero to harshad-result.
    perform 100-calculate-sum-of-digits.
    divide first-num by tot giving harshad-div remainder harshad-mod.
    if evenly-divisible
        if first-pass
            move first-num to harshad(niven)
            set niven up by 1
        else
            move first-num to harshad-result
        end-if
    end-if.
    exit paragraph.

100-calculate-sum-of-digits.
    move zero to tot.
    perform with test after until div = 0
        divide div by 10 giving div remainder mod
        add mod to tot
    end-perform.
    *> if tot >= 10
    *>  move tot to div
    *>  go to 100-calculate-sum-of-digits
    *> end-if.
    exit paragraph.

```



## ColdFusion


```cfm

<Cfset harshadNum = 0>
<Cfset counter = 0>

<Cfloop condition="harshadNum lte 1000">

  <Cfset startnum = harshadNum + 1>
  <Cfset digits = 0>
  <Cfset harshad = 0>

  <Cfloop condition="Harshad eq 0">

    <Cfset current_i = startnum>
    <Cfset digits = 0>

    <cfloop condition="len(current_i) gt 1">
      <Cfset digit = left(current_i, 1)>
      <Cfset current_i = right(current_i, len(current_i)-1)>
      <Cfset digits = digits + digit>
    </cfloop>
    <Cfset digits = digits + current_i>

    <Cfif Startnum MOD digits eq 0>
      <Cfset harshad = 1>
    <Cfelse>
      <cfset startnum = startnum + 1>
    </Cfif>

  </Cfloop>

  <cfset harshadNum = startnum>
  <Cfset counter = counter + 1>

  <Cfif counter lte 20>
    <Cfoutput>#harshadNum# </Cfoutput>
  </Cfif>

</Cfloop>

<Cfoutput>... #harshadNum# </Cfoutput>

```



## Common Lisp


```lisp
(defun harshadp (n)
  (zerop (rem n (digit-sum n))))

(defun digit-sum (n &optional (a 0))
  (cond ((zerop n) a)
	(t (digit-sum (floor n 10) (+ a (rem n 10))))))

(defun list-harshad (n &optional (i 1) (lst nil))
  "list the first n Harshad numbers starting from i (default 1)"
  (cond ((= (length lst) n) (reverse lst))
	((harshadp i) (list-harshad n (+ i 1) (cons i lst)))
	(t (list-harshad n (+ i 1) lst))))
```
```txt
CL-USER> (list-harshad 20)
(1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)
CL-USER> (list-harshad 1 1001)
(1002)

```



## Crystal

```ruby
harshad = 1.step.select { |n| n % n.to_s.chars.sum(&.to_i) == 0 }

puts "The first 20 harshard numbers are: \n#{ harshad.first(20).to_a }"
puts "The first harshard number > 1000 is #{ harshad.find { |n| n > 1000 } }"

```

```txt

The first 20 harshard numbers are:
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
The first harshard number > 1000 is 1002

```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.conv;

    enum digSum = (int n) => n.text.map!(d => d - '0').sum;
    enum harshads = iota(1, int.max).filter!(n => n % digSum(n) == 0);

    harshads.take(20).writeln;
    harshads.filter!(h => h > 1000).front.writeln;
}
```

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002
```



## EchoLisp


```scheme

(define (harsh? n)
    (zero? (modulo n
        (apply + (map string->number (string->list (number->string n)))))))

(harsh? 42)
    → #t

(define H (stream-filter harsh? (in-naturals 1)))

(take H 20)
    → (1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)

(for ((n H)) #:break (> n 1000) => n)
    → 1002

```



## Eiffel


```eiffel

note
	description : "project application root class"
	date        : "$October 10, 2014$"
	revision    : "$Revision$"

class
	NIVEN_SERIES

create
	make

feature
	make
		local
			number : INTEGER
			count : INTEGER
			last : BOOLEAN
		do
			number := 1

			from
				number := 1
				last := false

			until
				last = true

			loop

				if
					(number \\ sum_of_digits(number) = 0)
				then
					count := count + 1

					if
						(count <= 20 )
					then
						print("%N")
						print(number)
					end

					if
						(number > 1000)
					then
						print("%N")
						print(number)
						last := true
					end
				end

				 number := number + 1
			end
		end

	sum_of_digits(no:INTEGER):INTEGER

		local
			sum : INTEGER
			num : INTEGER
		do
			sum := 0

			from
				num := no

			until
				num = 0

			loop
				sum := sum + num \\ 10
				num := num // 10
			end

			Result := sum
		end
end


```


```txt

1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
1002
```



## Elixir


```elixir
defmodule Harshad do
  def series, do: Stream.iterate(1, &(&1+1)) |> Stream.filter(&(number?(&1)))

  def number?(n), do: rem(n, digit_sum(n, 0)) == 0

  defp digit_sum(0, sum), do: sum
  defp digit_sum(n, sum), do: digit_sum(div(n, 10), sum + rem(n, 10))
end

IO.inspect Harshad.series |> Enum.take(20)

IO.inspect Harshad.series |> Stream.drop_while(&(&1 <= 1000)) |> Enum.take(1) |> hd
```


```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002

```



## Erlang


```Erlang

-module( harshad ).

-export( [greater_than/1, sequence/1, task/0] ).

greater_than( N ) when N >= 1  ->
        greater_than( 2, N, acc(1, {0, []}) ).

sequence( Find_this_many ) when Find_this_many >= 1 ->
        sequence( 2, Find_this_many, acc(1, {0, []}) ).

task() ->
        io:fwrite( "~p~n", [sequence(20)] ),
        io:fwrite( "~p~n", [greater_than(1000)] ).



acc( N, Acc ) -> acc( N rem lists:sum([X - $0|| X <- erlang:integer_to_list(N)]), N, Acc ).

acc( 0, N, {Found, Acc} ) -> {Found + 1, [N | Acc]};
acc( _Reminder, _N, Acc ) -> Acc.

greater_than( _N, Find, {_, [Found | _T]} ) when Found > Find -> Found;
greater_than( N, Find, Acc ) ->	greater_than( N + 1, Find, acc(N, Acc) ).

sequence( _N, Found, {Found, Acc} ) -> lists:reverse( Acc );
sequence( N, Find, Acc ) -> sequence( N + 1, Find, acc(N, Acc) ).

```


```txt

39> harshad:task().
[1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42]
1002

```


'''Erlang 2'''

A somewhat more simple approach. Somewhat more efficient since it produces the partial list 23 times for the 20 element case whereas the above does so 36 or 37 times.


```Erlang

-module(harshad).
-export([main/0,harshad/1,seq/1]).

% We return the number R if harshad, else 0
harshad(R) ->
        case R
        rem lists:sum([X - $0|| X <- erlang:integer_to_list(R)]) of 0
        -> R; _ -> 0 end.

% build a list of harshads retrieving input from harshad(R)
% filter out the nulls and return
hlist(A,B) ->
      RL =  [ harshad(X) || X <- lists:seq(A,B) ],
      lists:filter( fun(X) -> X > 0 end,  RL).

seq(Total) -> seq(Total, [], 0).

seq(Total,L,_) when length(L) == Total-> L;
seq(Total,L,Acc) when length(L) < Total ->
      NL = hlist(1,Total + Acc),
      seq(Total,NL,Acc+1).

gt(_,L) when length(L) == 1 ->  hd(L);
gt(X,_) ->
      NL = hlist(X+1,X+2),
      gt(X+2,NL).

main() ->
      io:format("seq(20): ~w~n", [ seq(20) ]),
      io:format("gt(1000): ~w~n", [ gt(1000,[]) ]).

```



```txt

2> harshad:main().
seq(20): [1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42]
gt(1000): 1002
ok


```


=={{header|F_Sharp|F#}}==

```fsharp
let divides d n =
    match bigint.DivRem(n, d) with
    | (_, rest) -> rest = 0I

let splitToInt (str:string) = List.init str.Length (fun i -> ((int str.[i]) - (int "0".[0])))

let harshads =
    let rec loop n = seq {
        let sum = List.fold (+) 0 (splitToInt (n.ToString()))
        if divides (bigint sum) n then yield n
        yield! loop (n + 1I)
    }
    loop 1I

[<EntryPoint>]
let main argv =
    for h in (Seq.take 20 harshads) do printf "%A " h
    printfn ""
    printfn "%A" (Seq.find (fun n -> n > 1000I) harshads)
    0
```


```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```



## Factor


```factor
USING: math.text.utils lists lists.lazy ;

: niven? ( n -- ? ) dup 1 digit-groups sum mod 0 = ;

: first-n-niven ( n -- seq )
    1 lfrom [ niven? ] lfilter ltake list>array ;

: next-niven ( n -- m ) 1 + [ dup niven? ] [ 1 + ] until ;

20 first-n-niven .
1000 next-niven .
```

```txt

{ 1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 }
1002

```



## FBSL

The INITIALIZE routine fills a dynamic array with all we need, even the ellipsis.

```qbasic
#APPTYPE CONSOLE

CLASS harshad
    PRIVATE:
    memo[]

    SUB INITIALIZE()
        DIM i = 1, c
        DO
            IF isNiven(i) THEN
                c = c + 1
                memo[c] = i
            END IF
            i = i + 1
            IF c = 20 THEN EXIT DO
        LOOP
        memo[] = "..."
        i = 1000
        WHILE NOT isNiven(INCR(i)): WEND
        memo[] = i
    END SUB

    FUNCTION isNiven(n)
        RETURN NOT (n MOD sumdigits(n))
    END FUNCTION

    FUNCTION sumdigits(n)
        DIM num = n, m, sum
        WHILE num
            sum = sum + num MOD 10
            num = num \ 10
        WEND
        RETURN sum
    END FUNCTION

    PUBLIC:
    METHOD Yield()
        FOREACH DIM e IN memo
            PRINT e, " ";
        NEXT
    END METHOD
END CLASS

DIM niven AS NEW harshad
niven.Yield()

PAUSE

```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 ... 1002
Press any key to continue...
```



## Fortran


Please observe compilation on GNU/linux system and output from run
are in the comments at the START of the FORTRAN 2003 source.
The 1--20 loop idea was stolen from the ada solution.  Thank you.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Tue May 21 13:15:59
!
!a=./f && make $a && $a < unixdict.txt
!gfortran -std=f2003 -Wall -ffree-form f.f03 -o f
!    1    2    3    4    5    6    7    8    9   10   12   18   20   21   24   27   30   36   40   42 1002
!
!Compilation finished at Tue May 21 13:15:59

program Harshad
  integer :: i, h = 0
  do i=1, 20
    call nextHarshad(h)
    write(6, '(i5)', advance='no') h
  enddo
  h = 1000
  call nextHarshad(h)
  write(6, '(i5)') h

contains

  subroutine nextHarshad(h) ! alter input integer h to be the next greater Harshad number.
    integer, intent(inout) :: h
    h = h+1 ! bigger
    do while (.not. isHarshad(h))
      h = h+1
    end do
  end subroutine nextHarshad

  logical function isHarshad(a)
    integer, intent(in) :: a
    integer :: mutable, digit_sum
    isHarshad = .false.
    if (a .lt. 1) return ! false if a < 1
    mutable = a
    digit_sum = 0
    do while (mutable /= 0)
      digit_sum = digit_sum + mod(mutable, 10)
      mutable = mutable / 10
    end do
    isHarshad = 0 .eq. mod(a, digit_sum)
  end function isHarshad

end program Harshad

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function sumDigits(n As Integer) As Integer
  If n < 0 Then Return 0
  Dim sum As Integer
  While n > 0
    sum += n Mod 10
    n \= 10
  Wend
  Return sum
End Function

Function isHarshad(n As Integer) As Boolean
  Return n Mod sumDigits(n) = 0
End Function

Print "The first 20 Harshad or Niven numbers are :"
Dim count As Integer = 0
Dim i As Integer = 1

Do
  If isHarshad(i) Then
    Print i; " ";
    Count += 1
    If count = 20 Then Exit Do
  End If
  i += 1
Loop

Print : Print
Print "The first such number above 1000 is :"
i = 1001

Do
  If isHarshad(i) Then
    Print i; " "
    Exit Do
  End If
  i += 1
Loop

Print
Print "Press any key to quit"
Sleep
```


```txt

The first 20 Harshad or Niven numbers are :
 1  2  3  4  5  6  7  8  9  10  12  18  20  21  24  27  30  36  40  42

The first such number above 1000 is :
 1002

```



## Frink


```frink

isHarshad[n] := n mod sum[integerDigits[n]] == 0

c = 0
i = 1
while c<20
{
   if isHarshad[i]
   {
      c = c + 1
      println[i]
   }
   i = i + 1
}

println[]
i = 1000

do
   i = i + 1
while ! isHarshad[i]

println[i]

```


```txt

1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42

1002

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=9d814ce9936ed7fdce2a084004c437f4 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount, siLoop, siTotal, siCounter As Short
Dim sNo, sTemp As String
Dim sHold, sNiven As New String[]

For siCount = 1 To 1500
  sNo = Str(siCount)
  For siLoop = 1 To Len(sNo)
    sHold.Add(Mid(sNo, siLoop, 1))
  Next
  For Each sTemp In sHold
    siTotal += Val(sTemp)
  Next
  If siCount Mod siTotal = 0 Then
    Inc siCounter
    If siCounter < 21 Or siCount > 1000 Then
      sNiven.Add(Str(siCount))
      If siCount > 1000 Then Break
    Endif
  Endif
  siTotal = 0
  sHold.Clear
Next

Print "First twenty Harshad numbers and the first Harshad number greater than 1000"
Print sNiven.Join(", ")

End
```

Output:

```txt

First twenty Harshad numbers and the first Harshad number greater than 1000
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42, 1002

```



## Go


```go
package main

import "fmt"

type is func() int

func newSum() is {
    var ms is
    ms = func() int {
        ms = newSum()
        return ms()
    }
    var msd, d int
    return func() int {
        if d < 9 {
            d++
        } else {
            d = 0
            msd = ms()
        }
        return msd + d
    }
}

func newHarshard() is {
    i := 0
    sum := newSum()
    return func() int {
        for i++; i%sum() != 0; i++ {
        }
        return i
    }
}

func main() {
    h := newHarshard()
    fmt.Print(h())
    for i := 1; i < 20; i++ {
        fmt.Print(" ", h())
    }
    fmt.Println()
    h = newHarshard()
    n := h()
    for ; n <= 1000; n = h() {
    }
    fmt.Println(n)
}
```

```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002

```




## Groovy


```Groovy

​class HarshadNiven{ public static boolean find(int x)
   {
     int sum = 0,temp,var;
      var = x;
     while(x>0)
       {
         temp = x%10;
         sum = sum + temp;
         x = x/10;
       }
     if(var%sum==0) temp = 1;
     else temp = 0;
    return temp;
   }
 public static void main(String[] args)
  {
    int t,i;
     t = 0;
     for(i=1;t<20;i++)
      {
        if(find(i))
           {
             print(i + " ");
             t++;
           }
      }
     int x = 0;
     int y = 1000;
     while(x!=1)
      {
        if(find(y)) x = 1;
         y++;
      }
    println();
    println(y+1);
  }
}

```

```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002

```

​


## Haskell


```haskell
import Data.Char (ord)

harshads :: [Int]
harshads =
  let digsum = sum . map ((48 -) . ord) . show
  in filter ((0 ==) . (mod <*> digsum)) [1 ..]

main :: IO ()
main = mapM_ print [take 20 harshads, [(head . filter (> 1000)) harshads]]
```

```txt
[1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42]
1002
```


Or, as an alternative to strings and imports:


```haskell
harshadSeries :: [Int]
harshadSeries = filter ((0 ==) . (rem <*> (sum . digitList))) [1 ..]

digitList :: Int -> [Int]
digitList 0 = []
digitList n = rem n 10 : digitList (quot n 10)

main :: IO ()
main = mapM_ print $ [take 20, take 1 . dropWhile (<= 1000)] <*> [harshadSeries]
```

```txt

[1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42]
[1002]
```


=={{header|Icon}} and {{header|Unicon}}==


```unicon
procedure main(A)
    limit := integer(A[1]) | 20
    every writes(niven(seq())\limit," ")
    writes("... ")
    write(niven(seq(1001))\1)
end

procedure niven(n)
    n ? {s := 0; while s +:= move(1)}
    if (n%s) = 0 then return n
end
```


```txt

->ns
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 ... 1002
->

```



## J


```J
Until =: 2 : 'u^:(-.@:v)^:_'
isHarshad =: 0 = ] |~ [: +/ #.inv  NB. BASE isHarshad N
assert 1 0 -: 10 isHarshad&> 42 38
nextHarshad =: (>: Until (10&isHarshad))@:>:
assert 45 -: nextHarshad 42
assert 3 4 5 -: nextHarshad&> 2 3 4
assert 1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 -: (, nextHarshad@:{:)Until (20 = #) 1
assert 1002 -: nextHarshad 1000


   NB. next Harshad number in base 6.  Input and output are in base 6.
   NB. Verification left to you, gentle programmer.
   nextHarshad_base_6 =: (>: Until (6&isHarshad))@:>:
   ' '-.~":6#.inv nextHarshad_base_6 6b23235
23253

```



## Java

```java5
public class Harshad{
    private static long sumDigits(long n){
        long sum = 0;
        for(char digit:Long.toString(n).toCharArray()){
            sum += Character.digit(digit, 10);
        }
        return sum;
    }
    public static void main(String[] args){
        for(int count = 0, i = 1; count < 20;i++){
            if(i % sumDigits(i) == 0){
                System.out.println(i);
                count++;
            }
        }
        System.out.println();
        for(int i = 1001; ; i++){
            if(i % sumDigits(i) == 0){
                System.out.println(i);
                break;
            }
        }
    }
}
```

```txt
1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42

1002
```



## JavaScript


### ES5


```javascript
function isHarshad(n) {
    var s = 0;
    var n_str = new String(n);
    for (var i = 0; i < n_str.length; ++i) {
        s += parseInt(n_str.charAt(i));
    }
    return n % s === 0;
}

var count = 0;
var harshads = [];

for (var n = 1; count < 20; ++n) {
    if (isHarshad(n)) {
        count++;
        harshads.push(n);
    }
}

console.log(harshads.join(" "));

var h = 1000;
while (!isHarshad(++h));
console.log(h);

```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```



### ES6

One possible approach to functional composition:

```JavaScript
(() => {
    'use strict';

    // HARSHADS ---------------------------------------------------------------

    // nHarshads :: Int -> [Int]
    const nHarshads = n => {

        // isHarshad :: Int -> Bool
        const isHarshad = n => 0 === n % sum(digitList(n));

        return until(
                dct => dct.nth === n,
                dct => {
                    const
                        next = succ(dct.i),
                        blnHarshad = isHarshad(next);
                    return {
                        i: next,
                        hs: blnHarshad ? dct.hs.concat(next) : dct.hs,
                        nth: dct.nth + (blnHarshad ? 1 : 0)
                    };
                }, {
                    i: 0,
                    hs: [],
                    nth: 0
                }
            )
            .hs;
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // digitList :: Int -> [Int]
    const digitList = n =>
        n > 0 ? [n % 10].concat(digitList(Math.floor(n / 10))) : [];

    // dropWhile :: (a -> Bool) -> [a] -> [a]
    const dropWhile = (p, xs) => {
        let i = 0;
        for (let lng = xs.length;
            (i < lng) && p(xs[i]); i++) {}
        return xs.slice(i);
    };

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // a -> String
    const show = x => JSON.stringify(x, null, 2);

    // succ :: Int -> Int
    const succ = x => x + 1

    // sum :: (Num a) => [a] -> a
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        const go = x => p(x) ? x : go(f(x));
        return go(x);
    };

    // TEST -------------------------------------------------------------------
    return show({
        firstTwenty: nHarshads(20),
        firstOver1000: head(dropWhile(x => x <= 1000, nHarshads(1000)))
    });
})();
```

```txt
{
  "firstTwenty": [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    12,
    18,
    20,
    21,
    24,
    27,
    30,
    36,
    40,
    42
  ],
  "firstOver1000": 1002
}
```



## jq


```jq
def is_harshad:
 def digits: tostring | [explode[] | ([.]| implode) | tonumber];
 if . >= 1 then (. % (digits|add)) == 0
 else false
 end ;

# produce a stream of n Harshad numbers
def harshads(n):
  # [candidate, count]
  def _harshads:
    if .[0]|is_harshad then .[0], ([.[0]+1, .[1]-1]| _harshads)
    elif .[1] > 0 then [.[0]+1, .[1]] | _harshads
    else empty
    end;
  [1, n] | _harshads ;

# First Harshad greater than n where n >= 0
def harshad_greater_than(n):
  # input: next candidate
  def _harshad:
    if is_harshad then .
    else .+1 | _harshad
    end;
  (n+1) | _harshad ;

# Task:
[ harshads(20), "...", harshad_greater_than(1000)]
```

 $ jq -n -c -f harshad.jq
 [1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42,"...",1002]


## Julia

```julia
isharshad(x)   = x % sum(digits(x)) == 0
nextharshad(x) = begin while !isharshad(x+1) x += 1 end; return x + 1 end

function harshads(n::Integer)
	h = Vector{typeof(n)}(n)
	h[1] = 1
	for j in 2:n
		h[j] = nextharshad(h[j-1])
	end
	return h
end

println("First 20 harshad numbers: ", join(harshads(20), ", "))
println("First harshad number after 1001: ", nextharshad(1000))
```


```txt
First 20 harshad numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42
First harshad number after 1001: 1002
```



## K


```K

/ sum of digits of an integer
sumdig: {d::(); (0<){d::d,x!10; x%:10}/x; +/d}
/ Test if an integer is a Harshad number
isHarshad: {:[x!(sumdig x); 0; 1]} / Returns 1 if Harshad
/ Generate x Harshad numbers starting from y and display the list
hSeries: {harshad::();i:y;while[(x-#harshad)>0;:[isHarshad i; harshad::(harshad,i)]; i+:1];harshad}

```


```txt

    hSeries[20;1]
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
    hSeries[1; 1001]
,1002

```



## Kotlin


```scala
// version 1.1

fun sumDigits(n: Int): Int = when {
        n <= 0 -> 0
        else   -> {
            var sum = 0
            var nn = n
            while (nn > 0) {
                sum += nn % 10
                nn /= 10
            }
            sum
        }
    }

fun isHarshad(n: Int): Boolean = (n % sumDigits(n) == 0)

fun main(args: Array<String>) {
    println("The first 20 Harshad numbers are:")
    var count = 0
    var i = 0

    while (true) {
        if (isHarshad(++i)) {
            print("$i ")
            if (++count == 20) break
        }
    }

    println("\n\nThe first Harshad number above 1000 is:")
    i = 1000

    while (true) {
        if (isHarshad(++i)) {
            println(i)
            return
        }
    }
}
```


```txt

The first 20 Harshad numbers are:
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42

The first Harshad number above 1000 is:
1002

```



## LOLCODE


```LOLCODE
HAI 1.3

HOW IZ I digsummin YR num
    I HAS A digsum ITZ 0
    IM IN YR loop
        num, O RLY?
            YA RLY
                digsum R SUM OF digsum AN MOD OF num AN 10
                num R QUOSHUNT OF num AN 10
            NO WAI, FOUND YR digsum
        OIC
    IM OUTTA YR loop
IF U SAY SO

I HAS A found ITZ 0

IM IN YR finder UPPIN YR n
    I HAS A n ITZ SUM OF n AN 1
    I HAS A digsum ITZ I IZ digsummin YR n MKAY

    NOT MOD OF n AN digsum, O RLY?
        YA RLY
            DIFFRINT found AN BIGGR OF found AN 20, O RLY?
                YA RLY
                    VISIBLE n " "!
                    found R SUM OF found AN 1
            OIC

            DIFFRINT n AN SMALLR OF n AN 1000, O RLY?
                YA RLY, VISIBLE ":)" n, GTFO
            OIC
    OIC
IM OUTTA YR finder

KTHXBYE
```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```



## Lua


```lua
function isHarshad(n)
    local s=0
    local n_str=tostring(n)
    for i=1,#n_str do
        s=s+tonumber(n_str:sub(i,i))
    end
    return n%s==0
end

local count=0
local harshads={}
local n=1

while count<20 do
    if isHarshad(n) then
        count=count+1
        table.insert(harshads, n)
    end
    n=n+1
end

print(table.concat(harshads, " "))

local h=1001
while not isHarshad(h) do
    h=h+1
end
print(h)

```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
nextHarshad =
  NestWhile[# + 1 &, # + 1, ! Divisible[#, Total@IntegerDigits@#] &] &;
Print@Rest@NestList[nextHarshad, 0, 20];
Print@nextHarshad@1000;
```

```txt
{1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42}
1002
```


=={{header|MATLAB}} / {{header|Octave}}==
Define a testing function whether n is harshad or not

```MATLAB
function v = isharshad(n)
	v = isinteger(n) && ~mod(n,sum(num2str(n)-'0'));
end;
```

Check numbers

```MATLAB
k=1; n=1;
while (k<=20)
	if isharshad(n)
		printf('%i ',n);
		k=k+1;
	end;
	n=n+1;
end
n = 1001;
while ~isharshad(n)
	n=n+1;
end;
printf('\nFirst harshad number larger than 1000 is %i\n',n);
```


```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
First harshad number larger than 1000 is 1002
```



## min

```min
(
  :n () =list
  (n 0 >) (
    n 10 mod list prepend #list
    n 10 div @n
  ) while
  list
) :digits

(dup digits sum mod 0 ==) :harshad?

(
  succ :n
  (n harshad? not) (
    n succ @n
  ) while
  n
) :next-harshad

0 (next-harshad print " " print!) 20 times newline
1000 next-harshad print!
```

```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002

```



## MLite


```ocaml
fun sumdigits
                 (0, n) = n
        |        (m, n) = sumdigits (m div 10, m rem 10) + n
        |        n      = sumdigits (n div 10, n rem 10)

fun is_harshad n = (n rem (sumdigits n) = 0)

fun next_harshad_after
		(n, ~1) = if is_harshad n then
			n
		else
			next_harshad_after (n + 1, ~1)
	| 	n = next_harshad_after (n + 1, ~1)

fun harshad
                (max, _, count > max, accum) = rev accum
        |       (max, here, count, accum) =
			if is_harshad here then
                                harshad (max, here + 1, count + 1, here :: accum)
                        else
                                harshad (max, here + 1, count, accum)
        |       max = harshad (max, 1, 1, [])

;

print "first twenty harshad numbers = "; println ` harshad 20;
print "first harshad number after 1000 = "; println ` next_harshad_after 1000;
```



## NetRexx


```netrexx
/* NetRexx ------------------------------------------------------------
* 21.01.2014 Walter Pachl  translated from ooRexx (from REXX version 1)
*--------------------------------------------------------------------*/

options replace format comments java crossref symbols nobinary

  Parse Arg x y .                   /* get optional arguments:  X  Y */
  If x='' Then x=20                 /* Not specified?  Use default   */
  If y='' Then y=1000               /* "      "        "     "       */
  n=0                               /* Niven count                   */
  nl=''                             /* Niven list.                   */

  Loop j=1 By 1 Until n=x           /* let's go Niven number hunting.*/
    If j//sumdigs(j)=0 Then Do      /* j is a Niven number           */
      n=n+1                         /* bump Niven count              */
      nl=nl j                       /* add to list.                  */
      End
    End

  Say 'first' n 'Niven numbers:'nl

  Loop j=y+1 By 1                   /* start with first candidate    */
    If j//sumdigs(j)=0 Then         /* j is a Niven number           */
      Leave
    End

  Say 'first Niven number >' y 'is:' j
  Exit

method sumdigs(n) public static returns Rexx
  sum=n.left(1)
  Loop k=2 To n.length()
    sum=sum+n.substr(k,1)
    End
  Return sum
```

'''output''' same as ooRexx's


## Nim


```nim
import strutils

proc slice[T](iter: iterator(): T {.closure.}, sl): seq[T] =
  var result {.gensym.}: seq[int64] = @[]
  var i = 0
  for n in iter():
    if i > sl.b:
      break
    if i >= sl.a:
      result.add(n)
    inc i
  result

iterator harshad(): int64 {.closure.} =
  for n in 1 .. < int64.high:
    var sum = 0
    for ch in string($n):
      sum += parseInt("" & ch)
    if n mod sum == 0:
      yield n

echo harshad.slice 0 .. <20

for n in harshad():
  if n > 1000:
    echo n
    break
```

```txt
@[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002
```



## Objeck


```objeck

class Harshad {
  function : Main(args : String[]) ~ Nil {
    count := 0;
    for(i := 1; count < 20; i += 1;) {
      if(i % SumDigits(i) = 0){
        "{$i} "->Print();
        count += 1;
      };
    };

    for(i := 1001; true; i += 1;) {
      if(i % SumDigits(i) = 0){
        "... {$i}"->PrintLine();
        break;
      };
    };
  }

  function : SumDigits(n : Int) ~ Int {
    sum := 0;
    do {
      sum += n % 10;
      n /= 10;
    } while(n <> 0);

    return sum;
  }
}

```


```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 ... 1002
```



## Oforth



```Oforth
: sumDigits(n)  0 while(n) [ n 10 /mod ->n + ] ;
: isHarshad     dup sumDigits mod 0 == ;

1100 seq filter(#isHarshad) dup left(20) println dup filter(#[ 1000 > ]) first println
```


```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002

```



## ooRexx


```oorexx
/* REXX ---------------------------------------------------------------
* 21.01.2014 Walter Pachl modi-(simpli-)fied from REXX version 1
*--------------------------------------------------------------------*/
  Parse Arg x y .                   /* get optional arguments:  X  Y */
  If x='' Then x=20                 /* Not specified?  Use default   */
  If y='' Then y=1000               /* "      "        "     "       */
  n=0                               /* Niven count                   */
  nl=''                             /* Niven list.                   */

  Do j=1 Until n=x                  /* let's go Niven number hunting.*/
    If j//sumdigs(j)=0 Then Do      /* j is a Niven number           */
      n=n+1                         /* bump Niven count              */
      nl=nl j                       /* add to list.                  */
      End
    End

  Say 'first' n 'Niven numbers:'nl

  Do j=y+1                          /* start with first candidate    */
    If j//sumdigs(j)=0 Then         /* j is a Niven number           */
      Leave
    End

  Say 'first Niven number >' y 'is:' j
  Exit

sumdigs: Procedure                  /* compute sum of n's digits     */
  Parse Arg n
  sum=left(n,1)
  Do k=2 To length(n)
    sum=sum+substr(n,k,1)
    End
  Return sum
```

```txt
first 20 Niven numbers: 1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
first Niven number > 1000 is: 1002
```



## PARI/GP

```parigp
isHarshad(n)=n%sumdigits(n)==0
n=0;k=20;while(k,if(isHarshad(n++),k--;print1(n", ")));
n=1000;while(!isHarshad(n++),);print("\n"n)
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42,
1002
```


## Pascal

Optimized for speed, by using the state before in IncSumDigit.

```pascal
program Niven;
const
  base = 10;
type
  tNum      = longword;{Uint64}
const
   cntbasedigits = trunc(ln(High(tNum))/ln(base))+1;
type
  tSumDigit = record
                sdNumber  : tNum;
                sdDigits  : array[0..cntbasedigits-1] of byte;
                sdSumDig  : byte;
                sdIsNiven : boolean;
              end;

function InitSumDigit( n : tNum):tSumDigit;
var
  sd : tSumDigit;
  qt : tNum;
  i  : integer;
begin
  with sd do
  begin
    sdNumber:= n;
    fillchar(sdDigits,SizeOf(sdDigits),#0);
    sdSumDig :=0;
    sdIsNiven := false;
    i := 0;
    // calculate Digits und sum them up
    while n > 0 do
    begin
      qt := n div base;
      {n mod base}
      sdDigits[i] := n-qt*base;
      inc(sdSumDig,sdDigits[i]);
      n:= qt;
      inc(i);
    end;
    IF sdSumDig  >0 then
      sdIsNiven := (sdNumber MOD sdSumDig = 0);
  end;
  InitSumDigit:=sd;
end;

procedure IncSumDigit(var sd:tSumDigit);
var
   i,d: integer;
begin
  i := 0;
  with sd do
  begin
    inc(sdNumber);
    repeat
      d := sdDigits[i];
      inc(d);
      inc(sdSumDig);
      //base-1 times the repeat is left here
      if d < base then
      begin
        sdDigits[i] := d;
        BREAK;
      end
      else
      begin
        sdDigits[i] := 0;
        dec(sdSumDig,base);
        inc(i);
      end;
    until i > high( sdDigits);
    sdIsNiven := (sdNumber MOD sdSumDig) = 0;
  end;
end;

var
  MySumDig : tSumDigit;
  ln : tNum;
  cnt: integer;
begin
  MySumDig:=InitSumDigit(0);
  cnt := 0;
  repeat
    IncSumDigit(MySumDig);
    IF MySumDig.sdIsNiven then
    begin
      write(MySumDig.sdNumber,'.');
      inc(cnt);
    end;
  until cnt >= 20;
  write('....');
  MySumDig:=InitSumDigit(1000);
  repeat
    IncSumDigit(MySumDig);
  until MySumDig.sdIsNiven;
  writeln(MySumDig.sdNumber,'.');
// searching for big gaps between two niven-numbers
//  MySumDig:=InitSumDigit(18879989100-276);
  MySumDig:=InitSumDigit(1);
  cnt := 0;
  ln:= MySumDig.sdNumber;
  repeat
    IncSumDigit(MySumDig);
    if MySumDig.sdIsNiven then
    begin
      IF cnt < (MySumDig.sdNumber-ln) then
      begin
        cnt :=(MySumDig.sdNumber-ln);
        writeln(ln,' --> ',MySumDig.sdNumber,'  d=',cnt);
      end;
      ln:= MySumDig.sdNumber;
    end;
  until MySumDig.sdNumber= High(tNum);
{
689988915 --> 689989050  d=135
879987906 --> 879988050  d=144
989888823 --> 989888973  d=150
2998895823 --> 2998895976  d=153
~ 24 Cpu-cycles per test i3- 4330 1..2^32-1}
end.
```

output:

```txt
1.2.3.4.5.6.7.8.9.10.12.18.20.21.24.27.30.36.40.42.....1002.
```



## Perl


```perl
#!/usr/bin/perl
use strict ;
use warnings ;
use List::Util qw ( sum ) ;

sub createHarshads {
   my @harshads ;
   my $number = 1 ;
   do {
      if ( $number % sum ( split ( // , $number ) ) == 0 ) {
	 push @harshads , $number ;
      }
      $number++ ;
   } until (  $harshads[ -1 ] > 1000 ) ;
   return @harshads ;
}
my @harshadnumbers = createHarshads ;
for my $i ( 0..19 ) {
   print "$harshadnumbers[ $i ]\n" ;
}
print "The first Harshad number greater than 1000 is $harshadnumbers[ -1 ]!\n" ;
```

```txt
1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
The first Harshad number greater than 1000 is 1002!
```



## Perl 6


```perl6
constant @harshad = grep { $_ %% .comb.sum }, 1 .. *;

say @harshad[^20];
say @harshad.first: * > 1000;
```

```txt
(1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)
1002
```



## Phix


```Phix
integer n = 0
sequence digits={0}

procedure nNiven()
    while 1 do
        n += 1
        for i=length(digits) to 0 by -1 do
            if i=0 then
                digits = prepend(digits,1)
                exit
            end if
            if digits[i]<9 then
                digits[i] += 1
                exit
            end if
            digits[i] = 0
        end for
        if remainder(n,sum(digits))=0 then exit end if
    end while
end procedure

sequence s = {}
for i=1 to 20 do
    nNiven()
    s &= n
end for
?s
while n<=1000 do
    nNiven()
end while
?n
```

```txt

{1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42}
1002

```

Alternative version

```Phix
function isHarshad(integer n)
    return remainder(n,sum(sq_sub(sprint(n),'0')))=0
end function

sequence s = {}
integer n = 0
while length(s)<20 do
    n += 1
    if isHarshad(n) then
        s &= n
    end if
end while
n = 1001
while not isHarshad(n) do n += 1 end while
?s&n
```

```txt

{1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42,1002}

```



## PicoLisp


```PicoLisp

#if niven number, return it.
(de niven (N)
   (if (=0 (% N (apply + (getN N)))) N) )

#function which creates a list of numbers from input
(de getN (N)
   (mapcar format (chop N)) )

#This function generates niven number list
(de nivGen (R N)
   (extract niven (range R N)) )

#print 1st 20 niven numbers and 1st niven number greater than 1000
(printsp ~(list ~(head 20
                    (nivGen 1 1000) ) (max ~(nivGen 1001 1010)) ) )

```

```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 1002

```



## PL/I


```pli
*process source or(!) xref attributes;
 niven: Proc Options(main);
 /*********************************************************************
 * 08-06.2013 Walter Pachl translated from Rexx
 *                         with a slight improvement:  Do j=y+1 By 1;
 *********************************************************************/
 Dcl (ADDR,HBOUND,MOD,SUBSTR,VERIFY) Builtin;
 Dcl SYSPRINT Print;

 Dcl (x,y) dec fixed(8);
 x=20;
 y=1000;
 Begin;
   Dcl (n(x),j) Dec Fixed(8);
   Dcl ni Bin Fixed(31) Init(0);
   Dcl result Char(100) Var Init('');
 loop:
   Do j=1 By 1;
     If mod(j,sumdigs(j))=0 Then Do;
       ni+=1;
       n(ni)=j;
       result=result!!' '!!d2c(j);
       If ni=x Then Leave loop;
       End;
     End;
   Put Edit('first 20 Niven numbers: ',result)(Skip,a,a);
   Do j=y+1 By 1;
     If mod(j,sumdigs(j))=0 Then
       Leave;
     End;
   Put Edit('first Niven number > ',d2c(y),' is: ',d2c(j))(Skip,4(a));
   End;

 sumDigs: proc(z) Returns(Dec Fixed(3));
 Dcl z Pic'(8)9';
 Dcl d(8) Pic'9' Based(addr(z));
 Dcl i Bin Fixed(31);
 Dcl sd Dec Fixed(3) Init(0);
 Do i=1 To hbound(d);
   sd+=d(i);
   End;
 Return(sd);
 End;

 d2c: Proc(z) Returns(char(8) Var);
 Dcl z Pic'(8)z';
 Dcl p Bin Fixed(31);
 p=verify(z,' ');
 Return(substr(z,p));
 End;

 End;
```


```txt
first 20 Niven numbers:  1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
first Niven number > 1000 is: 1002
```



## PowerShell

In PowerShell, we generally don't wrap every little thing in a function. If you have something simple to do, you just do it.

```PowerShell

   1..1000 | Where { $_ % ( [int[]][string[]][char[]][string]$_ | Measure -Sum ).Sum -eq 0 } | Select -First 20
1001..2000 | Where { $_ % ( [int[]][string[]][char[]][string]$_ | Measure -Sum ).Sum -eq 0 } | Select -First 1

```

```txt

1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
1002

```

But if we do have a need for the code to be reusable, we can do that.

```PowerShell

function Get-HarshadNumbers
    {
    <#
    .SYNOPSIS
    Returns numbers in the Harshad or Niven series.

    .DESCRIPTION
    Returns all integers in the given range that are evenly divisible by the sum of their digits
    in ascending order.

    .PARAMETER Minimum
    Lower bound of the range to search for Harshad numbers. Defaults to 1.

    .PARAMETER Maximum
    Upper bound of the range to search for Harshad numbers. Defaults to 2,147,483,647

    .PARAMETER Count
    Maximum number of Harshad numbers to return.
    #>

    [cmdletbinding()]
    Param (
        [int]$Minimum = 1,
        [int]$Maximum = [int]::MaxValue,
        [int]$Count )

    #  Skip any non-positive numbers in the specified range
    $Minimum = [math]::Max( 1, $Minimum )

    #  If the adjusted range has any numbers in it...
    If ( $Maximum -ge $Minimum )
        {
        #  If a count was specified, build a parameter for the Select statement to kill the pipeline when the count is achieved.
        If ( $Count ) { $SelectParam = @{ First = $Count } }
        Else          { $SelectParam = @{} }

        #  For each number in the range, test the remainder of it divided it by iteself (converted to a string,
        #  then a character array, then a string array, then an integer array, then summed).
        $Minimum..$Maximum | Where { $_ % ( [int[]][string[]][char[]][string]$_ | Measure -Sum ).Sum -eq 0 } | Select @SelectParam
        }
    }

```


```PowerShell

Get-HarshadNumbers -Count 20
Get-HarshadNumbers -Minimum 1001 -Count 1

```

```txt

1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
1002

```



## Prolog

Works with SWI-Prolog and module lambda.pl written by '''Ulrich Neumerkel''', it can be found there :  http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl.

```Prolog
:- use_module(library(lambda)).

niven :-
	nb_setval(go, 1),

	L = [1 | _],
	print_niven(L, 1),
	gen_niven(1, L).


print_niven([X|T], N) :-
	when(ground(X),
	     (	 (   nb_getval(go, 1)
		 ->  (   N < 20
		     ->  writeln(X),
			 N1 is N+1,
			 print_niven(T, N1)
		     ;	 (   X > 1000
			 ->  writeln(X),
			     nb_setval(go, 0)
			 ;   N1 is N+1,
			     print_niven(T, N1)))
		 ;   true))).



gen_niven(X, [N | T]) :-
	(   nb_getval(go, 1)
	->  X1 is X+1,
	    sum_of_digit(X, S),
	    (   X mod S =:= 0
	    ->  N = X,
		gen_niven(X1, T)
	    ;	gen_niven(X1, [N | T]))
	;   true).


sum_of_digit(N, S) :-
	number_chars(N, LC),
	maplist(\X^Y^number_chars(Y, [X]), LC, LN),
	sum_list(LN, S).


```

```txt
 ?- niven.
1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
1002
true.


```


## Python


### Python: Procedural


```python>>>
 import itertools
>>> def harshad():
	for n in itertools.count(1):
		if n % sum(int(ch) for ch in str(n)) == 0:
			yield n


>>> list(itertools.islice(harshad(), 0, 20))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
>>> for n in harshad():
	if n > 1000:
		print(n)
		break


1002
>>>
```



### Python: Functional

The for loop above [http://paddy3118.blogspot.co.uk/2013/03/itertoolsfirst.html could be changed] to the following to find the number > 1000; in fact the harshad generator function could become a generator expression creating this more functional version:

```python>>>
 from itertools import count, islice
>>> harshad = (n for n in count(1) if n % sum(int(ch) for ch in str(n)) == 0)
>>> list(islice(harshad, 0, 20))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
>>> next(x for x in harshad if x > 1000)
1002
>>>
```


And we can sum digits more directly (without string coercion) while still preserving functional composition:

```python
'''Harshad or Niven series'''

from itertools import dropwhile, islice


# harshads :: () -> Gen [Int]
def harshads():
    '''Harshad series.'''
    x = 1
    while True:
        if 0 == (x % digitSum(x)):
            yield x
        x = 1 + x


# digitSum :: Int -> Int
def digitSum(n):
    '''The Sum of the decimal digits of n.'''
    def plusDigit(ra):
        r = ra[0]
        return (r // 10, ra[1] + (r % 10))

    def remZero(ra):
        return 0 == ra[0]

    return until(remZero)(plusDigit)(
        (n, 0)
    )[1]


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''First 20, and first above 1000.'''

    def firstTwenty(xs):
        return take(20)(xs)

    def firstAbove1000(xs):
        return take(1)(
            dropwhile(lambda x: 1000 >= x, xs)
        )

    print(
        fTable(__doc__ + ':\n')(
            lambda x: x.__name__
        )(showList)(lambda f: f(harshads()))([
            firstTwenty,
            firstAbove1000
        ])
    )


# GENERIC -------------------------------------------------

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


# DISPLAY -------------------------------------------------

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


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(repr(x) for x in xs) + ']'


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Harshad or Niven series:

   firstTwenty -> [1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42]
firstAbove1000 -> [1002]
```



## Racket


```scheme
#lang racket

(define (digsum n)
  (for/sum ([c (number->string n)]) (string->number [string c])))

(define harshads
  (stream-filter (λ (n) (= (modulo n (digsum n)) 0)) (in-naturals 1)))

; First 20 harshad numbers
(displayln (for/list ([i 20]) (stream-ref harshads i)))

; First harshad greater than 1000
(displayln (for/first ([h harshads] #:when(> h 1000)) h))
```


```txt
(1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)
1002
```


Different to the Scheme implementation in that it illustrates Racket's native iterators,
and ''let-values'' with ''quotient/remainder'':

```racket
#lang racket
(require math/number-theory)
(define (digital-sum n)
  (let inner
    ((n n) (s 0))
    (if (zero? n) s
        (let-values ([(q r) (quotient/remainder n 10)])
          (inner q (+ s r))))))

(define (harshad-number? n)
  (and (>= n 1)
       (divides? (digital-sum n) n)))

;; find 1st 20 Harshad numbers
(for ((i (in-range 1 (add1 20)))
      (h (sequence-filter harshad-number? (in-naturals 1))))
  (printf "#~a ~a~%" i h))

;; find 1st Harshad number > 1000
(displayln (for/first ((h (sequence-filter harshad-number? (in-naturals 1001)))) h))
```

```txt
#1 1
#2 2
#3 3
#4 4
#5 5
#6 6
#7 7
#8 8
#9 9
#10 10
#11 12
#12 18
#13 20
#14 21
#15 24
#16 27
#17 30
#18 36
#19 40
#20 42
1002
```



## REXX

These REXX examples allow the user to specify how many Niven numbers to list,

as well as find the first Niven number greater than a specified positive integer.

Also, gihugeic integers are supported   (essentially no limit).

### generic


```rexx
/*REXX program finds the first  A  Niven numbers;  it also finds first Niven number > B.*/
parse arg A B .                                  /*obtain optional arguments from the CL*/
if A=='' | A==','  then A=  20                   /*Not specified?  Then use the default.*/
if B=='' | B==','  then B=1000                   /* "      "         "   "    "     "   */
numeric digits 1+max(8, length(A), length(B))    /*enable the use of any sized numbers. */
#=0;    $=                                       /*set Niven numbers count;  Niven list.*/
                     do j=1  until  #==A         /*◄───── let's go Niven number hunting.*/
                     if j//sumDigs(j)==0  then do;  #=#+1;  $=$ j;  end
                     end   /*j*/                 /* [↑]   bump count; append J ──► list.*/

say 'first'   A   'Niven numbers:'   $

  do t=B+1  until  t//sumDigs(t)==0;    end      /*hunt for a Niven (or Harshad) number.*/

say  'first Niven number >'     B      " is: "      t
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumDigs: procedure; parse arg x; s=0;   do k=1 for length(x); s=s+substr(x,k,1); end /*k*/
```

'''output'''   when using the default inputs:

```txt

first 20 Niven numbers:  1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
first Niven number > 1000  is:  1002

```



### idomatic

This REXX version idiomatically uses a   '''isNiven'''   function.

```rexx
/*REXX program finds the first  A  Niven numbers;  it also finds first Niven number > B.*/
parse arg A B .                                  /*obtain optional arguments from the CL*/
if A=='' | A==','  then A=  20                   /*Not specified?  Then use the default.*/
if B=='' | B==','  then B=1000                   /* "      "         "   "    "     "   */
numeric digits 1+max(8, length(A), length(B))    /*enable the use of any sized numbers. */
#=0;    $=                                       /*set Niven numbers count;  Niven list.*/
                           do j=1  until  #==A   /*◄───── let's go Niven number hunting.*/
                           if isNiven(j)  then do;  #=#+1;  $=$ j;  end
                           end   /*j*/           /* [↑]   bump count; append J ──► list.*/

say 'first'   A   'Niven numbers:'   $

   do t=B+1  until  isNiven(t);      end         /*hunt for a Niven (or Harshad) number.*/

say  'first Niven number >'     B      " is: "      t
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isNiven: procedure; parse arg x; s=0;   do k=1 for length(x); s=s+substr(x,k,1); end /*k*/
         return x//s==0
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.




### esoteric

This REXX version optimizes the   '''isNiven'''   function by using   '''parse'''   statements instead of the   '''substr'''   BIF,

yielding a faster algorithm.

```rexx
/*REXX program finds the first  A  Niven numbers;  it also finds first Niven number > B.*/
parse arg A B .                                  /*obtain optional arguments from the CL*/
if A=='' | A==','  then A=  20                   /*Not specified?  Then use the default.*/
if B=='' | B==','  then B=1000                   /* "      "         "   "    "     "   */
numeric digits 1+max(8, length(A), length(B))    /*enable the use of any sized numbers. */
#=0;    $=                                       /*set Niven numbers count;  Niven list.*/
                           do j=1  until  #==A   /*◄───── let's go Niven number hunting.*/
                           if isNiven(j)  then do;  #=#+1;  $=$ j;  end
                           end   /*j*/           /* [↑]   bump count; append J ──► list.*/

say 'first'   A   'Niven numbers:'   $

   do t=B+1  until  isNiven(t);      end         /*hunt for a Niven (or Harshad) number.*/

say  'first Niven number >'     B      " is: "      t
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isNiven: procedure;  parse arg x 1 sum 2 q       /*use the first decimal digit for  SUM.*/
                           do  while  q\=='';  parse var q _ 2 q;  sum=sum+_;   end  /*k*/
                                                 /*      ↑                              */
         return x//sum==0                        /*      └───◄ is destructively parsed. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.


### array of numbers

This REXX version builds an   ''array''   of numbers instead of a   ''list''   (building an array is much faster than building a list, especially if the list is very long).

In addition, if the   '''A'''   number is negative, the numbers in the array aren't displayed, but the   ''last''   number in the array is displayed.

```rexx
/*REXX program finds the first  A  Niven numbers;  it also finds first Niven number > B.*/
parse arg A B .                                  /*obtain optional arguments from the CL*/
if A=='' | A==','  then A=  20                   /*Not specified?  Then use the default.*/
if B=='' | B==','  then B=1000                   /* "      "         "   "    "     "   */
tell= A>0;              A=abs(A)                 /*flag for showing a Niven numbers list*/
A=abs(a)
numeric digits 1+max(8, length(A), length(B))    /*enable the use of any sized numbers. */
#=0;    $=                                       /*set Niven numbers count;  Niven list.*/
                           do j=1  until  #==A   /*◄───── let's go Niven number hunting.*/
                           if isNiven(j)  then do;  #=#+1;  !.#=j;  end
                           end   /*j*/           /* [↑]   bump count; append J ──► list.*/
w=length(!.w)                                    /*W:   is the width of largest Niven #.*/
if tell  then do
              say 'first' A 'Niven numbers:';  do k=1  for #; say right(!.k, w); end /*k*/
              end
         else say 'last of the'      A      'Niven numbers: '           !.#
say
      do t=B+1  until  isNiven(t);   end         /*hunt for a Niven (or Harshad) number.*/

say  'first Niven number >'     B      " is: "      t
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isNiven: procedure;  parse arg x 1 sum 2 q       /*use the first decimal digit for  SUM.*/
                           do  while  q\=='';  parse var q _ 2 q;  sum=sum+_;   end  /*k*/
                                                 /*      ↑                              */
         return x//sum==0                        /*      └───◄ is destructively parsed. */
```

'''output'''   when the input used is:   <tt> -1000000   66777888 </tt>

```txt

last of the 1000000 Niven numbers:  12150510

first Niven number > 66777888  is:  66777900

```



## Ring


```ring

i = 1
count = 0
while true
      sum = 0
      if niven(i) = 1
         if count < 20 see "" + i + " is a Niven number" + nl count +=1 ok
         if i > 1000 see "" + i + " is a Niven number" exit ok ok
      i + =1
end

func niven nr
     nrString = string(nr)
     for j = 1 to len(nrString)
         sum = sum + number(nrString[j])
     next
     niv = ((nr % sum) = 0)
     return niv

```

Output:

```txt

1 is a Niven number
2 is a Niven number
3 is a Niven number
4 is a Niven number
5 is a Niven number
6 is a Niven number
7 is a Niven number
8 is a Niven number
9 is a Niven number
10 is a Niven number
12 is a Niven number
18 is a Niven number
20 is a Niven number
21 is a Niven number
24 is a Niven number
27 is a Niven number
30 is a Niven number
36 is a Niven number
40 is a Niven number
42 is a Niven number
1002 is a Niven number

```



## Ruby

Ruby 2.4 gave Integers a '''digits''' method, and Arrays a '''sum''' method.

```Ruby
harshad = 1.step.lazy.select { |n| n % n.digits.sum == 0 }

puts "The first 20 harshard numbers are: \n#{ harshad.first(20) }"
puts "The first harshard number > 1000 is #{ harshad.find { |n| n > 1000 } }"

```

```txt

The first 20 harshard numbers are:
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
The first harshard number > 1000 is 1002

```
>


## Run BASIC


```runbasic
while count < 20
  h = h + 1
  if neven(h) = 0 then
    count = count + 1
    print count;": ";h
  end if
wend

h = 1000
while 1 = 1
  h = h + 1
  if neven(h) = 0 then
    print h
    exit while
  end if
wend

function neven(h)
h$ = str$(h)
for i = 1 to len(h$)
 d = d + val(mid$(h$,i,1))
next i
neven = h mod d
end function
```

```txt

1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9
10: 10
11: 12
12: 18
13: 20
14: 21
15: 24
16: 27
17: 30
18: 36
19: 40
20: 42
1002

```


## Rust


```Rust

fn is_hashard (n : u32) -> bool {
    let sum_digits = n.to_string()
                      .chars()
                      .map(|c| c.to_digit(10).unwrap())
                      .fold(0, |a, b| a+b);
    n % sum_digits == 0
}

fn main() {
    for i in (1u32..).filter(|num| is_hashard(*num)).take(20) {
        println!("Hashard : {}", i);
    }
    for i in (1_001u32..).filter(|num| is_hashard(*num)).take(1) {
        println!("First Hashard bigger than 1_000 : {}", i);
    }
}

```

```txt

Hashard : 1
Hashard : 2
Hashard : 3
Hashard : 4
Hashard : 5
Hashard : 6
Hashard : 7
Hashard : 8
Hashard : 9
Hashard : 10
Hashard : 12
Hashard : 18
Hashard : 20
Hashard : 21
Hashard : 24
Hashard : 27
Hashard : 30
Hashard : 36
Hashard : 40
Hashard : 42
First Hashard bigger than 1_000 : 1002

```



## Scala


```Scala
object Harshad extends App {

  val harshads = Stream from 1 filter (i => i % i.toString.map(_.asDigit).sum == 0)

  println(harshads.take(20).toList)
  println(harshads.filter(_ > 1000).head)

}
```

```txt
List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42)
1002
```



## Scheme



```scheme
#!/usr/local/bin/gosh

;; Show the first 20 niven numbers and the
;; first one greater than 1000.
(define (main args)
    (display (iota-filtered 20 1 niven?))(newline)
    (display (iota-filtered 1 1001 niven?))(newline))

;; Return a list of length n
;; for numbers starting at start
;; that satisfy the predicate fn.
(define (iota-filtered n start fn)
    (let loop ((num start)(lst (list)))
        (if (= (length lst) n)
            lst
            (loop (+ 1 num) (if (fn num) (append lst (list num)) lst)))))

;; Is a number a niven number?
(define (niven? n)
    (and (> n 0) (= 0 (remainder n (sum-of-digits n)))))

;; Get the sum of the digits of a number.
(define (sum-of-digits n)
    (apply + (map string->number (map string (string->list (number->string n))))))


```


```txt

(1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42)
(1002)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: sumOfDigits (in var integer: num) is func
  result
    var integer: sum is 0;
  begin
    repeat
      sum +:= num rem 10;
      num := num div 10;
    until num = 0;
  end func;

const func integer: nextHarshadNum (inout integer: num) is func
  result
    var integer: harshadNumber is 0;
  begin
    while num mod sumOfDigits(num) <> 0 do
      incr(num);
    end while;
    harshadNumber := num;
  end func;

const proc: main is func
  local
    var integer: current is 1;
    var integer: count is 0;
  begin
    for count range 1 to 20 do
      write(nextHarshadNum(current) <& " ");
      incr(current);
    end for;
    current := 1001;
    writeln(" ... " <& nextHarshadNum(current));
  end func;
```


```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42  ... 1002

```



## Sidef


```ruby
func harshad() {
    var n = 0;
    {
        ++n while !(n %% n.digits.sum);
        n;
    }
}

var iter = harshad();
say 20.of { iter.run };

var n;
do {
    n = iter.run
} while (n <= 1000);

say n;
```

```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002

```



## Sinclair ZX81 BASIC

Works with 1k of RAM. <code>FAST</code> isn't all that fast.

```basic
 10 FAST
 20 LET N=0
 30 LET H=0
 40 LET N=N+1
 50 LET N$=STR$ N
 60 LET SD=0
 70 FOR I=1 TO LEN N$
 80 LET SD=SD+VAL N$(I)
 90 NEXT I
100 IF N/SD<>INT (N/SD) THEN GOTO 40
110 LET H=H+1
120 IF H<=20 OR N>1000 THEN PRINT N
130 IF N>1000 THEN GOTO 150
140 GOTO 40
150 SLOW
```

```txt
1
2
3
4
5
6
7
8
9
10
12
18
20
21
24
27
30
36
40
42
1002
```



## Swift



```swift
struct Harshad: Sequence, IteratorProtocol {
  private var i = 0

  mutating func next() -> Int? {
    while true {
      i += 1

      if i % Array(String(i)).map(String.init).compactMap(Int.init).reduce(0, +) == 0 {
        return i
      }
    }
  }
}

print("First 20: \(Array(Harshad().prefix(20)))")
print("First over a 1000: \(Harshad().first(where: { $0 > 1000 })!)")
```


```txt
First 20: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
First over a 1000: 1002
```



## Tcl


```tcl
# Determine if the given number is a member of the class of Harshad numbers
proc isHarshad {n} {
    if {$n < 1} {return false}
    set sum [tcl::mathop::+ {*}[split $n ""]]
    return [expr {$n%$sum == 0}]
}

# Get the first 20 numbers that satisfy the condition
for {set n 1; set harshads {}} {[llength $harshads] < 20} {incr n} {
    if {[isHarshad $n]} {
	lappend harshads $n
    }
}
puts [format "First twenty Harshads: %s" [join $harshads ", "]]

# Get the first value greater than 1000 that satisfies the condition
for {set n 1000} {![isHarshad [incr n]]} {} {}
puts "First Harshad > 1000 = $n"
```

```txt

First twenty Harshads: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42
First Harshad > 1000 = 1002

```



## uBasic/4tH

<lang>C=0

For I = 1 Step 1 Until C = 20          ' First 20 Harshad numbers
  If FUNC(_FNHarshad(I)) Then Print I;" "; : C = C + 1
Next

For I = 1001 Step 1                    ' First Harshad greater than 1000
  If FUNC(_FNHarshad(I)) Then Print I;" " : Break
Next

End

_FNHarshad Param(1)
  Local(2)

  c@ = a@
  b@ = 0
  Do While (c@ > 0)
     b@ = b@ + (c@ % 10)
     c@ = c@ / 10
  Loop

Return ((a@ % b@) = 0)
```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 1002

0 OK, 0:185
```



## VBA


```vb
Option Explicit

Sub Main()
Dim i As Long, out As String, Count As Integer
   Do
      i = i + 1
      If IsHarshad(i) Then out = out & i & ", ": Count = Count + 1
   Loop While Count < 20
   Debug.Print "First twenty Harshad numbers are : " & vbCrLf & out & "..."

   i = 1000
   Do
      i = i + 1
   Loop While Not IsHarshad(i)
   Debug.Print "The first harshad number after 1000 is : " & i
End Sub

Function IsHarshad(sNumber As Long) As Boolean
Dim Summ As Long, i As Long, temp
   temp = Split(StrConv(sNumber, vbUnicode), Chr(0))
   For i = LBound(temp) To UBound(temp) - 1
      Summ = Summ + temp(i)
   Next i
   IsHarshad = sNumber Mod Summ = 0
End Function
```


```txt
First twenty Harshad numbers are :
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42, ...
The first harshad number after 1000 is : 1002
```



## VBScript


```vb
n = 0
m = 1
first20 = ""
after1k = ""

Do
	If IsHarshad(m) And n <= 20 Then
		first20 = first20 & m & ", "
		n = n + 1
		m = m + 1
	ElseIf IsHarshad(m) And m > 1000 Then
		after1k = m
		Exit Do
	Else
		m = m + 1
	End If
Loop

WScript.StdOut.Write "First twenty Harshad numbers are: "
WScript.StdOut.WriteLine
WScript.StdOut.Write first20
WScript.StdOut.WriteLine
WScript.StdOut.Write "The first Harshad number after 1000 is: "
WScript.StdOut.WriteLine
WScript.StdOut.Write after1k

Function IsHarshad(s)
	IsHarshad = False
	sum = 0
	For i = 1 To Len(s)
		sum = sum + CInt(Mid(s,i,1))
	Next
	If s Mod sum = 0 Then
		IsHarshad = True
	End If
End Function
```


```txt
First twenty Harshad numbers are:
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42, 45,
The first Harshad number after 1000 is:
1002
```



## Visual FoxPro


```vfp

LOCAL lnCount As Integer, k As Integer
CLEAR
lnCount = 0
k = 0
*!* First 20 numbers
? "First 20 numbers:"
DO WHILE lnCount < 20
    k = k + 1
    IF Harshad(k)
	lnCount = lnCount + 1
	? lnCount, k
    ENDIF
ENDDO
*!* First such number > 1000
k = 1001
DO WHILE NOT Harshad(k)
    k = k + 1
ENDDO
? "First such number > 1000", k

FUNCTION Harshad(n As Integer) As Boolean
LOCAL cn As String, d As Integer, i As Integer
cn = TRANSFORM(n)
d = 0
FOR i = 1 TO LEN(cn)
    d = d + VAL(SUBSTR(cn, i, 1))
ENDFOR
RETURN n % d = 0
ENDFUNC

```

```txt

First 20 numbers:
         1          1
         2          2
         3          3
         4          4
         5          5
         6          6
         7          7
         8          8
         9          9
        10         10
        11         12
        12         18
        13         20
        14         21
        15         24
        16         27
        17         30
        18         36
        19         40
        20         42
First such number > 1000: 1002

```



## Whitespace


```Whitespace































































```

This solution was generated from the pseudo-Assembly below.
A [http://ideone.com/AKxEMY live run] is available for the inquiring skeptic.

```asm
push 0 ; Harshad numbers found
push 0 ; counter

0:  ; Increment the counter, call "digsum", branch on the modulus.
    push 1 add dup dup
    push 0 call 1 mod
        jz 2
        jump 0

1:  ; [n 0] => [digsum(n)]
    copy 1
    push 10 mod add swap
    push 10 div swap
    push 0 copy 2 sub
        jn 1
        slide 1 ret

2:  ; Should we print this Harshad number?
    push 1000 copy 1 sub jn 3 ; We're done if it's greater than 1000.
    swap push 1 add swap      ; Increment how many we've found so far.
    push 20 copy 2 sub jn 0   ; If we've already got 20, go back to the top.
    dup onum push 32 ochr     ; Otherwise, print it and a space.
    jump 0                    ; And /then/ go back to the top.

3:  ; Print the > 1000 Harshad number on its own line and exit clean.
    push 10 ochr onum pop push 10 ochr exit
```

```txt
1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42
1002
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int H, C, N, S;                 \Harshad number, Counter, Number, Sum
[H:= 1;  C:= 0;
loop    [N:= H;  S:= 0;         \sum digits
        repeat  N:= N/10;
                S:= S + rem(0);
        until   N = 0;
        if rem(H/S) = 0 then    \Harshad no.is evenly divisible by sum of digits
                [if C < 20 then [IntOut(0, H);  ChOut(0, ^ );  C:= C+1];
                if H > 1000 then [IntOut(0, H);  CrLf(0);  quit];
                ];
        H:= H+1;
        ];
]
```


```txt

1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 1002

```



## zkl


```zkl
fcn harshad(n){ 0==n%(n.split().sum(0)) }
[1..].tweak(fcn(n){ if(not harshad(n)) return(Void.Skip); n })
   .walk(20).println();
[1..].filter(20,harshad).println();
[1001..].filter1(harshad).println();
```

Walkers are zkl iterators. [a..b] is a Walker from a to b. Walkers can be tweaked to transform the sequence they are walking. In this case, ignore non Harshad numbers. Then tell the walker to get 20 items from that [modified] sequence.

In this case, filters are the better solution.
```txt

L(1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42)
L(1,2,3,4,5,6,7,8,9,10,12,18,20,21,24,27,30,36,40,42)
L(1002)

```



## ZX Spectrum Basic

```zxbasic
10 LET k=0: LET n=0
20 IF k=20 THEN GO TO 60
30 LET n=n+1: GO SUB 1000
40 IF isHarshad THEN PRINT n;" ";: LET k=k+1
50 GO TO 20
60 LET n=1001
70 GO SUB 1000: IF NOT isHarshad THEN LET n=n+1: GO TO 70
80 PRINT '"First Harshad number larger than 1000 is ";n
90 STOP
1000 REM is Harshad?
1010 LET s=0: LET n$=STR$ n
1020 FOR i=1 TO LEN n$
1030 LET s=s+VAL n$(i)
1040 NEXT i
1050 LET isHarshad=NOT FN m(n,s)
1060 RETURN
1100 DEF FN m(a,b)=a-INT (a/b)*b
```

