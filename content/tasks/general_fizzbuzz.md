+++
title = "General FizzBuzz"
description = ""
date = 2019-09-13T22:18:36Z
aliases = []
[extra]
id = 18843
[taxonomies]
categories = ["Classic CS problems and programs", "Iteration", "Recursion", "task"]
tags = []
languages = [
  "applescript",
  "awk",
  "batch_file",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "livecode",
  "lua",
  "maple",
  "mathematica",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tailspin",
  "tcl",
  "ursa",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task
Write a generalized version of [[FizzBuzz]] that works for any list of factors, along with their words.

This is basically a "fizzbuzz" implementation where the user supplies the parameters.

The user will enter the max number, then they will enter the factors to be calculated along with the corresponding word to be printed.

For simplicity's sake, assume the user will input an integer as the max number and 3 factors, each with a word associated with them.


For example, given:

```txt

>20      #This is the maximum number, supplied by the user
>3 Fizz  #The user now enters the starting factor (3) and the word they want associated with it (Fizz)
>5 Buzz  #The user now enters the next factor (5) and the word they want associated with it (Buzz)
>7 Baxx  #The user now enters the next factor (7) and the word they want associated with it (Baxx)

```


In other words: For this example, print the numbers '''1''' through '''20''', replacing every multiple of '''3''' with "Fizz", every multiple of '''5''' with "Buzz", and every multiple of '''7''' with "Baxx".

In the case where a number is a multiple of at least two factors, print each of the words associated with those factors in the order of least to greatest factor.

For instance, the number '''15''' is a multiple of both '''3''' and '''5'''; print "FizzBuzz".

If the max number was '''105''' instead of '''20''', you would print "FizzBuzzBaxx" because it's a multiple of '''3''', '''5''', and '''7'''.

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```






## AppleScript


{{Trans|JavaScript}}

```AppleScript
-- GENERAL FIZZBUZZ ----------------------------------------------------------

-- fizz :: [[Int, String]] -> Int -> String
on fizz(lstRules, intMax)

    -- fizzLine :: String -> Int -> String
    script fizzline
        on |λ|(strSeries, n)

            -- Multiple rule matches ->  single or concatenated words
            -- wordIfRuleMatch :: String -> (Int, String) -> String
            script wordIfRuleMatch
                on |λ|(str, rulePair)
                    set {factor, noiseWord} to rulePair

                    cond(n mod factor > 0, str, str & noiseWord)
                end |λ|
            end script

            set strPhrase to foldl(wordIfRuleMatch, "", lstRules)

            strSeries & cond(strPhrase ≠ "", strPhrase, n as string) & linefeed
        end |λ|
    end script

    foldl(fizzline, "", enumFromTo(1, intMax))
end fizz

-- TEST ----------------------------------------------------------------------
on run

    fizz([[3, "Fizz"], [5, "Buzz"], [7, "Baxx"]], 20)

end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- cond :: Bool -> a -> a -> a
on cond(bool, x, y)
    if bool then
        x
    else
        y
    end if
end cond

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

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## AWK

This is a two-step solution:
* First, we get the parameters, and
** generate a file with the list of numbers (writing directly to that file)
** generate a custom awk-program for that special case (redirecting standard-output)
* the custom program is run, and does the actual work to output the desired result

'''Input:'''

```txt

105
3 Fizz
5 Buzz
7 Baxx
```


;Usage:

```bash
awk  -f fizzbuzzGenerate.awk  input.txt > fizzbuzzCustom.awk
awk  -f fizzbuzzCustom.awk  numbers.txt
```


;Program:
<!-- http://ideone.com/fACMfK  -->
<!-- (!!) Note: the sandbox at ideone.com does not allow output to files -->

```awk
# usage:  awk -f fizzbuzzGen.awk > fizzbuzzCustom.awk
#
function Print(s) {
    print s > "/dev/stderr"
}

BEGIN { Print( "# FizzBuzz-Generate:" )
        q2 = "\""
        fN = "numbers.txt"
       #fP = "fizzbuzzCustom.awk"
}

NF==1 { Print( "# " $1 " Numbers:" )
        for( i=1; i <= $1; i++ )
            print( i ) > fN   # (!!) write to file not allowed in sandbox at ideone.com

        Print( "# Custom program:" )
        print "BEGIN {print " q2 "# CustomFizzBuzz:" q2 "} \n"
        next
}

NF==2 { Print( "# " $1 "-->" $2 )   ##
        print "$1 %  "$1" == 0 {x = x "q2 $2 q2 "}"
        next
}

END {  print ""
       print "!x  {print $1; next}"
       print "    {print " q2 " " q2 ", x; x=" q2 q2 "} \n"

       print "END {print " q2 "# Done." q2 "}"
       Print( "# Done." )
}
```


Example output see [[FizzBuzz/AWK#Custom_FizzBuzz]]


## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
	::Range variable
	set range=20

	::The input data [will not be validated]
	::This is the strictly the data format...
	set "data=3:Fizz 5:Buzz 7:Baxx"

	::Parsing the data into 1-based pseudo-arrays...
set "data_cnt=0"
for %%A in (!data!) do (
	set /a "data_cnt+=1"
	for /f "tokens=1-2 delims=:" %%D in ("%%A") do (
		set "fact!data_cnt!=%%D"
		set "prnt!data_cnt!=%%E"
	)
)

	::Do the count...
for /l %%C in (1,1,!range!) do (
	set "out="
	for /l %%. in (1,1,!data_cnt!) do (
		set /a "mod=%%C %% fact%%."
		if !mod! equ 0 (
			set "out=!out!!prnt%%.!"
		)
	)
	if not defined out (echo.%%C) else (echo.!out!)
)
pause
exit /b 0
```

{{Out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
Press any key to continue . . .
```




## BBC BASIC

This implementation (unlike some of the ones given on this page...) fully obeys the specification, in that it prompts the user for the parameters at run time. It also allows users to specify as many factors as they want, rather than limiting them to three.

```bbcbasic
REM>
genfizzb
INPUT "Maximum number: " max%
INPUT "Number of factors: " n%
DIM factors%(n% - 1)
DIM words$(n% - 1)
FOR i% = 0 TO n% - 1
    INPUT "> " factor$
    factors%(i%) = VAL(LEFT$(factor$, INSTR(factor$, " ") - 1))
    words$(i%) = MID$(factor$, INSTR(factor$, " ") + 1)
NEXT
FOR i% = 1 TO max%
    matched% = FALSE
    FOR j% = 0 TO n% - 1
        IF i% MOD factors%(j%) = 0 THEN
            PRINT words$(j%);
            matched% = TRUE
        ENDIF
    NEXT
    IF matched% THEN PRINT ELSE PRINT;i%
NEXT
```

Output:

```txt
Maximum number: 20
Number of factors: 3
> 3 Fizz
> 5 Buzz
> 7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## C


```C

#include <stdio.h>
#include <stdlib.h>

struct replace_info {
    int n;
    char *text;
};

int compare(const void *a, const void *b)
{
    struct replace_info *x = (struct replace_info *) a;
    struct replace_info *y = (struct replace_info *) b;
    return x->n - y->n;
}

void generic_fizz_buzz(int max, struct replace_info *info, int info_length)
{
    int i, it;
    int found_word;

    for (i = 1; i < max; ++i) {
        found_word = 0;

        /* Assume sorted order of values in the info array */
        for (it = 0; it < info_length; ++it) {
            if (0 == i % info[it].n) {
                printf("%s", info[it].text);
                found_word = 1;
            }
        }

        if (0 == found_word)
            printf("%d", i);

        printf("\n");
    }
}

int main(void)
{
    struct replace_info info[3] = {
        {5, "Buzz"},
        {7, "Baxx"},
        {3, "Fizz"}
    };

    /* Sort information array */
    qsort(info, 3, sizeof(struct replace_info), compare);

    /* Print output for generic FizzBuzz */
    generic_fizz_buzz(20, info, 3);
    return 0;
}

```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## C++


```cpp

#include <algorithm>
#include <iostream>
#include <vector>
#include <string>

class pair  {
public:
    pair( int s, std::string z )            { p = std::make_pair( s, z ); }
    bool operator < ( const pair& o ) const { return i() < o.i(); }
    int i() const                           { return p.first; }
    std::string s() const                   { return p.second; }
private:
    std::pair<int, std::string> p;
};
void gFizzBuzz( int c, std::vector<pair>& v ) {
    bool output;
    for( int x = 1; x <= c; x++ ) {
        output = false;
        for( std::vector<pair>::iterator i = v.begin(); i != v.end(); i++ ) {
            if( !( x % ( *i ).i() ) ) {
                std::cout << ( *i ).s();
                output = true;
            }
        }
        if( !output ) std::cout << x;
        std::cout << "\n";
    }
}
int main( int argc, char* argv[] ) {
    std::vector<pair> v;
    v.push_back( pair( 7, "Baxx" ) );
    v.push_back( pair( 3, "Fizz" ) );
    v.push_back( pair( 5, "Buzz" ) );
    std::sort( v.begin(), v.end() );
    gFizzBuzz( 20, v );
    return 0;
}

```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## C#

Not extremely clever and doesn't use anything too fancy.

```c#

using System;

public class GeneralFizzBuzz
{
    public static void Main()
    {
        int i;
        int j;
        int k;

        int limit;

        string iString;
        string jString;
        string kString;

        Console.WriteLine("First integer:");
        i = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine("First string:");
        iString = Console.ReadLine();

        Console.WriteLine("Second integer:");
        j = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine("Second string:");
        jString = Console.ReadLine();

        Console.WriteLine("Third integer:");
        k = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine("Third string:");
        kString = Console.ReadLine();

        Console.WriteLine("Limit (inclusive):");
        limit = Convert.ToInt32(Console.ReadLine());

        for(int n = 1; n<= limit; n++)
        {
            bool flag = true;
            if(n%i == 0)
            {
                Console.Write(iString);
                flag = false;
            }

            if(n%j == 0)
            {
                Console.Write(jString);
                flag = false;
            }

            if(n%k == 0)
            {
                Console.Write(kString);
                flag = false;
            }
            if(flag)
                Console.Write(n);
            Console.WriteLine();
        }
    }
}

```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>GENFIZBUZZ(MAX,WORDS,NUMBERS)
    ; loop until max, casting numeric to avoid errors
    for i=1:1:+MAX {
        set j = 1
        set descr = ""

        ; assumes NUMBERS parameter is comma-delimited
        while (j <= $length(NUMBERS,",")) {
            if ((i # $piece(NUMBERS,",",j,j)) = 0) {

                ; build descriptor string, again assuming comma-delimited WORDS parameter
                set descr = descr_$piece(WORDS,",",j,j)
            }

            set j = j + 1
        }    ; check list of numbers

        ; output values to screen
        write " "_$case(descr,"":i,:descr)
    }    ; next value until MAX

    quit
```



{{out}}
```txt
SAMPLES>do ^GENFIZBUZZ(12,"FIN,FANG,FOOM","2,3,4")
 1 FIN FANG FINFOOM 5 FINFANG 7 FINFOOM FANG FIN 11 FINFANGFOOM
```



## Ceylon


```ceylon
shared void run() {

	print("enter the max value");
	assert(exists maxLine = process.readLine(),
		exists max = parseInteger(maxLine));

	print("enter your number/word pairs
	       enter a blank line to stop");

	variable value divisorsToWords = map<Integer, String> {};

	while(true) {
		value line = process.readLine();
		assert(exists line);
		if(line.trimmed.empty) {
			break;
		}
		value pair = line.trimmed.split().sequence();
		if(exists first = pair.first,
			exists integer = parseInteger(first),
			exists word = pair[1]) {
			divisorsToWords = divisorsToWords.patch(map {integer -> word});
		}
	}

	value divisors = divisorsToWords.keys.sort(byIncreasing(Integer.magnitude));
	for(i in 1..max) {
		value builder = StringBuilder();
		for(divisor in divisors) {
			if(divisor.divides(i), exists word = divisorsToWords[divisor]) {
				builder.append(word);
			}
		}
		if(builder.empty) {
			print(i);
		} else {
			print(builder.string);
		}
	}
}
```



## Clojure


```Clojure
(defn fix [pairs]
  (map second pairs))

(defn getvalid [pairs n]
  (filter (fn [p] (zero? (mod n (first p))))
          (sort-by first pairs)))

(defn gfizzbuzz [pairs numbers]
  (interpose "\n"
             (map (fn [n] (let [f (getvalid pairs n)]
                            (if (empty? f)
                              n
                              (apply str
                                     (fix f)))))
                  numbers)))
```


'''Usage:'''

```txt

user#=> (def pairs [[5 "Buzz"] [3 "Fizz"] [7 "Baxx"]])
#'user/pairs
user#=> (println (apply str (gfizzbuzz pairs (range 1 21))))
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
nil

```



## Common Lisp


```Lisp

(defun fizzbuzz (limit factor-words)
  (loop for i from 1 to limit
     if (assoc-if #'(lambda (factor) (zerop (mod i factor))) factor-words)
     do (loop for (factor . word) in factor-words
           when (zerop (mod i factor)) do (princ word)
           finally (fresh-line))
     else do (format t "~a~%" i)))

(defun read-factors (&optional factor-words)
  (princ "> ")
  (let ((input (read-line t nil)))
    (cond ((zerop (length input))
           (sort factor-words #'< :key #'car))
          ((digit-char-p (char input 0))
           (multiple-value-bind (n i) (parse-integer input :junk-allowed t)
             (read-factors (acons n (string-trim " " (subseq input i))
                                  factor-words))))
          (t (write-line "Invalid input.")
             (read-factors factor-words)))))

(defun main ()
  (loop initially (princ "> ")
     for input = (read-line t nil)
     until (and (> (length input) 0)
                (digit-char-p (char input 0))
                (not (zerop (parse-integer input :junk-allowed t))))
     finally (fizzbuzz (parse-integer input :junk-allowed t) (read-factors))))

```



## Crystal


```Crystal

counter = 0

puts "Enter a maximum number:"
limit = gets

puts "Enter the first integer for factoring:"
first_int = gets
puts "Enter the name of the first integer:"
first_int_name = gets

puts "Enter the second integer for factoring:"
second_int = gets
puts "Enter the name of the second integer:"
second_int_name = gets

puts "Enter the third integer for factoring:"
third_int = gets
puts "Enter the name of the third integer:"
third_int_name = gets

if (limit &&
   first_int &&
   second_int &&
   third_int &&
   first_int_name &&
   second_int_name &&
   third_int_name)
  limit = limit.chomp.to_i
  first_int = first_int.chomp.to_i
  second_int = second_int.chomp.to_i
  third_int = third_int.chomp.to_i
  while limit > counter
    counter += 1
    if (counter % first_int) == 0 && (counter % second_int) == 0 && (counter % third_int) == 0
      puts first_int_name + second_int_name + third_int_name
    elsif (counter % first_int) == 0 && (counter % second_int) == 0
      puts first_int_name + second_int_name
    elsif (counter % first_int) == 0
      puts first_int_name
    elsif (counter % second_int) == 0
      puts second_int_name
    elsif (counter % third_int) == 0
      puts third_int_name
    else
      puts counter
    end
  end
else
  exit
end

```



## D


```D
import core.stdc.stdlib;
import std.stdio;

void main() {
    int limit;
    write("Max number (>0): ");
    readf!"%d\n"(limit);
    if (limit <= 0) {
        writeln("The max number to consider must be greater than zero.");
        exit(1);
    }

    int terms;
    write("Terms (>0): ");
    readf!"%d\n"(terms);
    if (terms <= 0) {
        writeln("The number of terms to consider must be greater than zero.");
        exit(1);
    }

    int[] factors = new int[terms];
    string[] words = new string[terms];

    for (int i=0; i<terms; ++i) {
        write("Factor ", i+1, " and word: ");
        readf!"%d %s\n"(factors[i], words[i]);
        if (factors[i] <= 0) {
            writeln("The factor to consider must be greater than zero.");
            exit(1);
        }
    }

    foreach(n; 1..limit+1) {
        bool print = true;

        for (int i=0; i<terms; ++i) {
            if (n % factors[i] == 0) {
                write(words[i]);
                print = false;
            }
        }

        if (print) {
            writeln(n);
        } else {
            writeln();
        }
    }
}
```


{{out}}

```txt
Max number (>0): 20
Terms (>0): 3
Factor 1 and word: 3 Fizz
Factor 2 and word: 5 Buzz
Factor 3 and word: 7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule General do
  def fizzbuzz(input) do
    [num | nwords] = String.split(input)
    max = String.to_integer(num)
    dict = Enum.chunk(nwords, 2) |> Enum.map(fn[n,word] -> {String.to_integer(n),word} end)
    Enum.each(1..max, fn i ->
      str = Enum.map_join(dict, fn {n,word} -> if rem(i,n)==0, do: word end)
      IO.puts if str=="", do: i, else: str
    end)
  end
end

input = """
105
3 Fizz
5 Buzz
7 Baxx
"""
General.fizzbuzz(input)
```


{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
FizzBaxx
...
Buzz
101
Fizz
103
104
FizzBuzzBaxx

```



## Erlang


```erlang

%%% @doc Implementation of General FizzBuzz
%%% @see https://rosettacode.org/wiki/General_FizzBuzz
-module(general_fizzbuzz).
-export([run/2]).
-spec run(N :: pos_integer(), Factors :: list(tuple())) -> ok.

fizzbuzz(N, [], []) ->
    integer_to_list(N);
fizzbuzz(_, [], Result) ->
    lists:flatten(lists:reverse(Result));
fizzbuzz(N, Factors, Result) ->
    [{Factor, Output}|FactorsRest] = Factors,

    NextResult = case N rem Factor of
        0 -> [Output|Result];
        _ -> Result
    end,

    fizzbuzz(N, FactorsRest, NextResult).

run(N, Factors) ->
    lists:foreach(
        fun(S) -> io:format("~s~n", [S]) end,
        [fizzbuzz(X, Factors, []) || X <- lists:seq(1, N)]
    ).

```

'''Usage:'''

```txt

    $ erl
    1> c(general_fizzbuzz).
    2> general_fizzbuzz:run(105, [{3, "Fizz"}, {5, "Buzz"}, {7, "Baxx"}]).

```


{{out}}
```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
...
Fizz
97
Baxx
Fizz
Buzz
101
Fizz
103
104
FizzBuzzBaxx
ok

```



## Factor


```factor
USING: assocs combinators.extras io kernel math math.parser
math.ranges prettyprint sequences splitting ;
IN: rosetta-code.general-fizzbuzz

: prompt ( -- str ) ">" write readln ;

: get-factor ( -- seq )
    prompt " " split first2 [ string>number ] dip
    { } 2sequence ;

: get-input ( -- assoc n )
    prompt string>number [1,b] [ get-factor ] thrice
    { } 3sequence swap ;

: fizzbuzz ( assoc n -- )
    swap dupd [ drop swap mod 0 = ] with assoc-filter
    dup empty? [ drop . ] [ nip values concat print ] if ;

: main ( -- ) get-input [ fizzbuzz ] with each ;

MAIN: main
```

{{out}}

```txt

>20
>3 Fizz
>5 Buzz
>7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Forth

Uses GForth specific words ']]' and '[['.
If your forth does not have them: Sequence ']] A B C .. [['  is equivalent with 'postpone A postpone B postpone C ..'

```Forth
\ gfb.fs - generalized fizz buzz
: times		( xt n -- )
	BEGIN dup WHILE
		1- over swap 2>r execute 2r>
	REPEAT
	2drop
;
\ 'Domain Specific Language' compiling words
\ -- First  comment: stack-effect at compile-time
\ -- Second comment: stack efect of compiled sequence
: ]+[		( u ca u -- ) ( u f -- u f' )
	2>r >r	]
			]] over [[
	r>  		]] literal mod 0= IF [[
	2r> 		]] sliteral type 1+ THEN [ [[
;
: ]fb		( -- xt ) ( u f -- u+1 )
	]] IF space ELSE dup u. THEN 1+ ; [[
;
: fb[		( -- ) ( u -- u 0  ;'u' is START-NUMBER )
	:noname  0 ]] literal [ [[
;
\ Usage: START-NUMBER COMPILING-SEQUENCE U times drop ( INCREASED-NUBER )
\ Example:
\ 1 fb[ 3 s" fizz" ]+[ 5 s" buzz" ]+[ 7 s" dizz" ]+[ ]fb 40 times drop

```

{{out}}

```txt

gforth gfb.fs --evaluate '1 fb[ ]fb 40 times drop cr bye'
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
gforth gfb.fs --evaluate '1 fb[ 3 s" F" ]+[ 5 s" B" ]+[ 7 s" G" ]+[ ]fb 40 times drop cr bye'
1 2 F 4 B F G 8 F B 11 F 13 G FB 16 17 F 19 B FG 22 23 F B 26 F G 29 FB 31 32 F 34 BG F 37 38 F B

```


## FreeBASIC

{{trans|BBC BASIC}}

```freebasic
' version 01-03-2018
' compile with: fbc -s console

Dim As UInteger f(), factor, c, i, n
Dim As Integer max
Dim As String w(), word
Dim As boolean flag

Do
    Input "Enter maximum number, if number < 1 then the program wil end ", max
    If max < 1 Then Exit Do

    Print
    While c = 0 Or c > max
        Input "Total number of factors ", c
    Wend
    c -= 1
    ReDim f(c), w(c)

    Print
    For i = 0 To c
        Input "Enter factor and word, separated by a comma ", factor, word
        f(i) = factor
        w(i) = word
    Next


    While flag = FALSE
        flag = TRUE
        For n = 0 To c-1
            For i = 1 To c
                If f(n) > f(i) Then
                    flag = FALSE
                    Swap f(n), f(i)
                    Swap w(n), w(i)
                End If
            Next
        Next
    Wend

    For n = 1 To max
        flag = FALSE
        For i = 0 To c
            If n Mod f(i) = 0 Then
                flag = TRUE
                Print w(i);
            End If
        Next
        Print IIf(flag , "", Str(n))
    Next

    Exit Do
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Enter maximum number, if number < 1 then the program wil end 110

Total number of factors 3

Enter factor and word, separated by a comma 3, Fizz
Enter factor and word, separated by a comma 5, Buzz
Enter factor and word, separated by a comma 7, Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
...
101
Fizz
103
104
FizzBuzzBaxx
106
107
Fizz
109
Buzz
```



## Go


```Go

package main

import (
	"fmt"
)

const numbers = 3

func main() {

	//using the provided data
	max := 20
	words := map[int]string{
		3: "Fizz",
		5: "Buzz",
		7: "Baxx",
	}
	keys := []int{3, 5, 7}
	divisible := false
	for i := 1; i <= max; i++ {
		for _, n := range keys {
			if i % n == 0 {
				fmt.Print(words[n])
				divisible = true
			}
		}
		if !divisible {
			fmt.Print(i)
		}
		fmt.Println()
		divisible = false
	}

}
```



## Groovy


```Groovy
def log = ''
(1..40).each {Integer value -> log +=(value %3 == 0) ? (value %5 == 0)? 'FIZZBUZZ\n':(value %7 == 0)? 'FIZZBAXX\n':'FIZZ\n'
                                    :(value %5 == 0) ? (value %7 == 0)? 'BUZBAXX\n':'BUZZ\n'
                                    :(value %7 == 0) ?'BAXX\n'
                                    :(value+'\n')}
println log

```


```txt

1
2
FIZZ
4
BUZZ
FIZZ
BAXX
8
FIZZ
BUZZ
11
FIZZ
13
BAXX
FIZZBUZZ
16
17
FIZZ
19
BUZZ
FIZZBAXX
22
23
FIZZ
BUZZ
26
FIZZ
BAXX
29
FIZZBUZZ
31
32
FIZZ
34
BUZBAXX
FIZZ
37
38
FIZZ
BUZZ

```



## Haskell



```haskell
fizz :: (Integral a, Show a) => a -> [(a, String)] -> String
fizz a xs
    | null result = show a
    | otherwise   = result
    where result = concatMap (fizz' a) xs
          fizz' a (factor, str)
              | a `mod` factor == 0 = str
              | otherwise           = ""

main = do
    line <- getLine
    let n = read line
    contents <- getContents
    let multiples = map (convert . words) $ lines contents
    mapM_ (\ x -> putStrLn $ fizz x multiples) [1..n]
    where convert [x, y] = (read x, y)

```


Or, as a function which takes a list of rules as an argument:


```haskell
type Rule = (Int, String)

testFizz :: Int -> [String]
testFizz = fizz [(3, "Fizz"), (5, "Buzz"), (7, "Baxx")]

fizz :: [Rule] -> Int -> [String]
fizz rules n = foldr nextLine [] [1 .. n]
  where
    nextLine x a =
      (if null noise
         then show x
         else noise) :
      a
      where
        noise = foldl reWrite [] rules
        reWrite s (m, k) =
          s ++
          (if rem x m == 0
             then k
             else [])

main :: IO ()
main = mapM_ putStrLn (testFizz 20)
```

{{Out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## J


The trick here involves looking for where the factors evenly divide the counting numbers. Where no factor is relevant we use the counting number, an in the remaining cases we use the string which corresponds to the factor:


```J
genfb=:1 :0
:
  b=. * x|/1+i.y
  >,&":&.>/(m#inv"_1~-.b),(*/b)#&.>1+i.y
)
```


Example use:


```J
   3 5 7 ('Fizz';'Buzz';'Baxx')genfb 20
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```


For our example, b looks like this:


```J
1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1
1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0
1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1
```


<code>*/b</code> gives us 1s where we want numbers and 0s where we want to plug in the strings:


```J
   */*3 5 7|/1+i.20
1 1 0 1 0 0 0 1 0 0 1 0 1 0 0 1 1 0 1 0
```


<code>m</code> is our strings, and #inv expands values out to match a selection. So, in our example, <code>m#inv"_1~-.b</code> looks like this:


```J
┌┬┬────┬┬────┬────┬────┬┬────┬────┬┬────┬┬────┬────┬┬┬────┬┬────┐
│││Fizz││    │Fizz│    ││Fizz│    ││Fizz││    │Fizz│││Fizz││    │
├┼┼────┼┼────┼────┼────┼┼────┼────┼┼────┼┼────┼────┼┼┼────┼┼────┤
│││    ││Buzz│    │    ││    │Buzz││    ││    │Buzz│││    ││Buzz│
├┼┼────┼┼────┼────┼────┼┼────┼────┼┼────┼┼────┼────┼┼┼────┼┼────┤
│││    ││    │    │Baxx││    │    ││    ││Baxx│    │││    ││    │
└┴┴────┴┴────┴────┴────┴┴────┴────┴┴────┴┴────┴────┴┴┴────┴┴────┘
```


All that remains is to assemble these pieces into the final result...


## Java


```java
public class FizzBuzz {

    public static void main(String[] args) {
        Sound[] sounds = {new Sound(3, "Fizz"), new Sound(5, "Buzz"),  new Sound(7, "Baxx")};
        for (int i = 1; i <= 20; i++) {
            StringBuilder sb = new StringBuilder();
            for (Sound sound : sounds) {
                sb.append(sound.generate(i));
            }
            System.out.println(sb.length() == 0 ? i : sb.toString());
        }
    }

    private static class Sound {
        private final int trigger;
        private final String onomatopoeia;

        public Sound(int trigger, String onomatopoeia) {
            this.trigger = trigger;
            this.onomatopoeia = onomatopoeia;
        }

        public String generate(int i) {
            return i % trigger == 0 ? onomatopoeia : "";
        }

    }

}
```



'''For a more complete example see [[/jFizzBuzz|jFizzBuzz]]'''


## JavaScript



### ES5


In a functional style of JavaScript, with two nested ''reduce'' folds – one through the integer series,
and one through the series of rules.

First as compacted by Google's Closure compiler:

```JavaScript
function fizz(d, e) {
  return function b(a) {
    return a ? b(a - 1).concat(a) : [];
  }(e).reduce(function (b, a) {
    return b + (d.reduce(function (b, c) {
      return b + (a % c[0] ? "" : c[1]);
    }, "") || a.toString()) + "\n";
  }, "");
}
```


and then in the original expanded form,  for better legibility:


```JavaScript
function fizz(lstRules, lngMax) {

    return (
        function rng(i) {
            return i ? rng(i - 1).concat(i) : []
        }
    )(lngMax).reduce(
        function (strSeries, n) {

            // The next member of the series of lines:
            // a word string or a number string
            return strSeries + (
                lstRules.reduce(
                    function (str, tplNumWord) {
                        return str + (
                            n % tplNumWord[0] ? '' : tplNumWord[1]
                        )
                    }, ''
                ) || n.toString()
            ) + '\n';

        }, ''
    );
}

fizz([[3, 'Fizz'], [5, 'Buzz'], [7, 'Baxx']], 20);
```


{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```




### ES6



```JavaScript
// range :: Int -> Int -> [Int]
const range = (min, max) =>
  Array.from({ length: max - min }, (_, i) => min + i)

const defaultRules = Object.freeze([
  [3, 'Fizz'],
  [5, 'Buzz'],
  [7, 'Baxx'],
])

// fizzBuzz :: Int -> [[Int, String]] -> String
const fizzBuzz = (max, rules = defaultRules) =>
  range(1, max + 1).map(n =>
    rules.reduce((words, [factor, word]) =>
      words + (n % factor ? '' : word), ''
    ) || n
  ).join('\n')

console.log(fizzBuzz(20))
```


{{Out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Julia

For simplicity, assume that the user will enter valid input.

```Julia
function fizzbuzz(triggers :: Vector{Tuple{Int, ASCIIString}}, upper :: Int)
    for i = 1 : upper
        triggered = false

        for trigger in triggers
            if i % trigger[1] == 0
                triggered = true
                print(trigger[2])
            end
        end

        !triggered && print(i)
        println()
    end
end

print("Enter upper limit:\n> ")
upper = parse(Int, readline())

triggers = Tuple{Int, ASCIIString}[]
print("Enter factor/string pairs (space delimited; ^D when done):\n> ")
while (r = readline()) != ""
    input = split(r)
    push!(triggers, (parse(Int, input[1]), input[2]))
    print("> ")
end

println("EOF\n")
fizzbuzz(triggers, upper)
```


{{out}}

```txt
Enter upper limit:
> 20
Enter factor/string pairs (space delimited; ^D when done):
> 3 Fizz
> 5 Buzz
> 7 Baxx
> EOF

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## Kotlin



```Kotlin
fun main(args: Array<String>) {

    //Read the maximum number, set to 0 if it couldn't be read
    val max = readLine()?.toInt() ?: 0
    val words = mutableMapOf<Int, String>()

    //Read input three times for a factor and a word
    (1..3).forEach {
        readLine()?.let {
            val tokens = it.split(' ')
            words.put(tokens[0].toInt(), tokens[1])
        }
    }

    //Sort the words so they will be output in arithmetic order
    val sortedWords = words.toSortedMap()

    //Find the words with matching factors and print them, print the number if no factors match
    for (i in 1..max) {
        val wordsToPrint = sortedWords.filter { i % it.key == 0 }.map { it.value }
        if (wordsToPrint.isNotEmpty()) {
            wordsToPrint.forEach { print(it) }
            println()
        }
        else
            println(i)
    }
}
```



## LiveCode


```LiveCode
function generalisedFizzBuzz m, f1, f2, f3
    put f1 & cr & f2 & cr & f3 into factors
    sort factors ascending numeric
    repeat with i = 1 to m
        put false into flag
        if i mod (word 1 of line 1 of factors) = 0 then
            put word 2 of line 1 of factors after fizzbuzz
            put true into flag
        end if
        if i mod (word 1 of line 2 of factors) = 0 then
            put word 2 of line 2 of factors after fizzbuzz
            put true into flag
        end if
        if i mod (word 1 of line 3 of factors) = 0 then
            put word 2 of line 3 of factors after fizzbuzz
            put true into flag
        end if
        if flag is false then put i after fizzbuzz
        put cr after fizzbuzz
    end repeat
    return fizzbuzz
end generalisedFizzBuzz
```
Example
```LiveCode
put generalisedFizzBuzz(20,"7 baxx","3 fizz","5 buzz")
```




## Lua


```Lua
function genFizz (param)
  local response
  print("\n")
  for n = 1, param.limit do
    response = ""
    for i = 1, 3 do
      if n % param.factor[i] == 0 then
        response = response .. param.word[i]
      end
    end
    if response == "" then print(n) else print(response) end
  end
end

local param = {factor = {}, word = {}}
param.limit = io.read()
for i = 1, 3 do
  param.factor[i], param.word[i] = io.read("*number", "*line")
end
genFizz(param)
```



###  Without modulo

{{trans|Python}}

```Lua
local function fizzbuzz(n, mods)
  local res = {}

  for i = 1, #mods, 2 do
    local mod, name = mods[i], mods[i+1]
    for i = mod, n, mod do
      res[i] = (res[i] or '') .. name
    end
  end

  for i = 1, n do
    res[i] = res[i] or i
  end

  return table.concat(res, '\n')
end

do
  local n = tonumber(io.read())     -- number of lines, eg. 100
  local mods = {}

  local n_mods = 0
  while n_mods ~= 3 do              -- for reading until EOF, change 3 to -1
    local line = io.read()
    if not line then break end
    local s, e = line:find(' ')
    local num  = tonumber(line:sub(1, s-1))
    local name = line:sub(e+1)
    mods[#mods+1] = num
    mods[#mods+1] = name
    n_mods = n_mods + 1
  end

  print(fizzbuzz(n, mods))
end

```


{{out}}

```txt

> mods = {
>>   3, 'cheese ',
>>   2, 'broccoli ',
>>   3, 'sauce ',
>> }
> fizzbuzz(8, mods)
1
broccoli
cheese sauce
broccoli
5
cheese broccoli sauce
7
broccoli
```



## Maple


```Maple
findNum := proc(str) #help parse input
	local i;
	i := 1:
	while (true) do
		if (StringTools:-IsAlpha(str[i])) then
			return i-2:
		end if:
		i := i+1:
	end do:
end proc:
path := "input.txt";
input := readline(path):
T := table():
maxnum := parse(input):
while (true) do
	input := readline(path):
	if input = 0 then break; end if:
	pos := findNum(input):
	num := parse(input[..pos]):
	T[num] := input[pos+2..]:
end do:
for i from 1 to maxnum do
	factored := false:
	for j in [indices(T)] do
		if i mod j[1] = 0 then
			factored := true:
			printf(T[j[1]]);
		end if:
	end do:
	if (not factored) then printf("%d", i): end if:
	printf("\n");
end do:
```

{{Out|Output}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## Mathematica


```Mathematica
list={{5,"Buzz"},{3,"Fizz"},{7,"Baxx"}};
runTo=(*LCM@@list[[All,1]]+1*)20;
Column@Table[
 Select[list,Mod[x,#[[1]]]==0&][[All,2]]/.{}->{x}
 ,{x,1,runTo}
]
```



```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```


=={{header|Modula-2}}==

```modula2
MODULE GeneralFizzBuzz;
FROM Conversions IMPORT StrToInt;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;

TYPE
    Word = ARRAY[0..63] OF CHAR;

PROCEDURE WriteInt(i : INTEGER);
VAR buf : Word;
BEGIN
    FormatString("%i", buf, i);
    WriteString(buf);
END WriteInt;

PROCEDURE ReadInt() : INTEGER;
VAR
    buf : ARRAY[0..9] OF CHAR;
    c : CHAR;
    i : INTEGER;
BEGIN
    i := 0;
    LOOP
        c := ReadChar();
        IF (c=0C) OR (i>9) THEN
            BREAK
        ELSIF (c=012C) OR (c=015C) THEN
            WriteLn;
            buf[i] := 0C;
            BREAK
        ELSIF (c<'0') OR (c>'9') THEN
            Write(c);
            buf[i] := 0C;
            BREAK
        ELSE
            Write(c);
            buf[i] := c;
            INC(i)
        END
    END;
    StrToInt(buf, i);
    RETURN i
END ReadInt;

PROCEDURE ReadLine() : Word;
VAR
    buf : Word;
    i : INTEGER;
    c : CHAR;
BEGIN
    i := 0;
    WHILE i<HIGH(buf) DO
        c := ReadChar();
        IF (c=0C) OR (c=012C) OR (c=015C) THEN
            WriteLn;
            buf[i] := 0C;
            BREAK
        ELSE
            Write(c);
            buf[i] := c;
            INC(i)
        END
    END;
    RETURN buf;
END ReadLine;

VAR
    i,max : INTEGER;
    fa,fb,fc : INTEGER;
    wa,wb,wc : Word;
    done : BOOLEAN;
BEGIN
    max := ReadInt();

    fa := ReadInt();
    wa := ReadLine();
    fb := ReadInt();
    wb := ReadLine();
    fc := ReadInt();
    wc := ReadLine();

    FOR i:=1 TO max DO
        done := FALSE;
        IF i MOD fa = 0 THEN
            done := TRUE;
            WriteString(wa);
        END;
        IF i MOD fb = 0 THEN
            done := TRUE;
            WriteString(wb);
        END;
        IF i MOD fc = 0 THEN
            done := TRUE;
            WriteString(wc);
        END;
        IF NOT done THEN
            WriteInt(i)
        END;
        WriteLn;
    END;

    ReadChar
END GeneralFizzBuzz.
```



## Nim

This solution has no input validation

```nim

import parseutils, strutils, algorithm

type FactorAndWord = tuple[factor:int, word: string]

var number: int
var factorAndWords: array[3, FactorAndWord]

#custom comparison proc for the FactorAndWord type
proc customCmp(x,y: FactorAndWord): int =
  if x.factor < y.factor:
    -1
  elif x.factor > y.factor:
    1
  else:
    0

echo "Enter max number:"
var input = readLine(stdin)
discard parseInt(input, number)

for i in 0..2:

  echo "Enter a number and word separated by space:"
  var input = readLine(stdin)

  var tokens = input.split
  discard parseInt(tokens[0], factorAndWords[i].factor)
  factorAndWords[i].word = tokens[1]

#sort factors in ascending order
sort(factorAndWords, customCmp)

#implement fiz buz
for i in 1..number:
  var written = false;
  for item in items(factorAndWords):
    if i mod item.factor == 0 :
      write(stdout, item.word)
      written = true
  if written :
    write(stdout, "\n")
  else :
    writeLine(stdout, i)



```



## PARI/GP

{{works with|PARI/GP|2.8.0+}}
This version uses a variadic argument to allow more or less than 3 factors. It could be easily modified for earlier versions, either by taking a vector rather than bare arguments (making the call <code>fizz(20,[[3,"Fizz"],[5,"Buzz"],[7,"Baxx"]])</code>) or to support exactly factors (keeping the call the same).

```parigp
fizz(n,v[..])=
{
	v=vecsort(v,1);
	for(k=1,n,
		my(t);
		for(i=1,#v,
			if(k%v[i][1]==0,
				print1(v[i][2]);
				t=1
			)
		);
		print(if(t,"",k))
	);
}
fizz(20,[3,"Fizz"],[5,"Buzz"],[7,"Baxx"])
```

{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## Perl


```perl

#!bin/usr/perl
use 5.020;
use strict;
use warnings;

#Get a max number from the user
say("Please enter the maximum possible multiple. ");
my $max = <STDIN>;

#Get the factors from the user
my @factors = ();
my $buffer;
say("Now enter the first factor and its associated word. Ex: 3 Fizz ");
chomp($buffer = <STDIN>);
push @factors, $buffer;
say("Now enter the second factor and its associated word. Ex: 5 Buzz ");
chomp($buffer = <STDIN>);
push @factors, $buffer;
say("Now enter the third factor and its associated word. Ex: 7 Baxx ");
chomp($buffer = <STDIN>);
push @factors, $buffer;

#Counting from 1 to max
for(my $i = 1; $i <= $max; $i++)
{
    #Create a secondary buffer as well as set the original buffer to the current index
    my $oBuffer;
    $buffer = $i;
    #Run through each element in our array
    foreach my $element (@factors)
    {
        #Look for white space
        $element =~ /\s/;
        #If the int is a factor of max, append it to oBuffer as a string to be printed
        if($i % substr($element, 0, @-) == 0)
        {
            $oBuffer = $oBuffer . substr($element, @+ + 1, length($element));
            #This is essentially setting a flag saying that at least one element is a factor
            $buffer = "";
        }
    }
    #If there are any factors for that number, print their words. If not, print the number.
    if(length($buffer) > 0)
    {
        print($buffer . "\n");
    }
    else
    {
        print($oBuffer . "\n");
    }
}

```


{{out}}

```txt

Please enter the maximum possible multiple.
20
Now enter the first factor and its associated word. Ex: 3 Fizz
3 Fizz
Now enter the second factor and its associated word. Ex: 5 Buzz
5 Buzz
Now enter the third factor and its associated word. Ex: 7 Baxx
7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Perl 6

{{works with|rakudo|2015-09-20}}

```perl6
# General case implementation of a "FizzBuzz" class.
# Defaults to standard FizzBuzz unless a new schema is passed in.
class FizzBuzz {
    has $.schema is rw = < 3 Fizz 5 Buzz >.hash;
    method filter (Int $this) {
        my $fb;
        for $.schema.sort: { +.key } -> $p { $fb ~= $this %% +$p.key ?? $p.value !! ''};
        return $fb || $this;
    }
}


# Sub implementing the specific requirements of the task.
sub GeneralFizzBuzz (Int $upto, @schema?) {
    my $ping = FizzBuzz.new;
    $ping.schema = @schema.hash if @schema;
    map { $ping.filter: $_ }, 1 .. $upto;
}

# The task
say 'Using: 20 ' ~ <3 Fizz 5 Buzz 7 Baxx>;
.say for GeneralFizzBuzz(20, <3 Fizz 5 Buzz 7 Baxx>);

say '';

# And for fun
say 'Using: 21 ' ~ <2 Pip 4 Squack 5 Pocketa 7 Queep>;
say join ', ', GeneralFizzBuzz(21, <2 Pip 4 Squack 5 Pocketa 7 Queep>);
```

{{Out}}

```txt
Using: 20 3 Fizz 5 Buzz 7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

Using: 21 2 Pip 4 Squack 5 Pocketa 7 Queep
1, Pip, 3, PipSquack, Pocketa, Pip, Queep, PipSquack, 9, PipPocketa, 11, PipSquack, 13, PipQueep, Pocketa, PipSquack, 17, Pip, 19, PipSquackPocketa, Queep
```


Here's the same program in a more functional idiom:

```perl6
sub genfizzbuzz($n, +@fb) {
    [Z~](
        do for @fb || <3 fizz 5 buzz> -> $i, $s {
            flat ('' xx $i-1, $s) xx *;
        }
    ) Z|| 1..$n
}

.say for genfizzbuzz(20, <3 Fizz 5 Buzz 7 Baxx>);
```



## Phix


```Phix
procedure general_fizz_buzz(integer lim, sequence words, sequence facts)
    for i=1 to lim do
        string word = ""
        for j=1 to length(facts) do
            if remainder(i,facts[j])=0 then
                word &= words[j]
            end if
        end for
        if length(word)=0 then
            word = sprintf("%d",i)
        end if
        printf(1,"%s\n",{word})
    end for
end procedure
general_fizz_buzz(20, {"Fizz","Buzz","Baxx"}, {3,5,7})
```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## PicoLisp


```PicoLisp
(de general (N Lst)
   (for A N
      (prinl
         (or
            (extract
               '((L)
                  (and (=0 (% A (car L))) (cdr L)) )
               Lst )
            A ) ) ) )

(general 20 '((3 . Fizz) (5 . Buzz) (7 . Baxx)))
```


{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## PowerShell


```powershell
$limit = 20
$data  = @("3 Fizz","5 Buzz","7 Baxx")
	#An array with whitespace as the delimiter
	#Between the factor and the word

for ($i = 1;$i -le $limit;$i++){
	$outP = ""
	foreach ($x in $data){
		$data_split = $x -split " "	#Split the "<factor> <word>"
		if (($i % $data_split[0]) -eq 0){
			$outP += $data_split[1]	#Append the <word> to outP
		}
	}
	if(!$outP){	#Is outP equal to NUL?
		Write-HoSt $i
	} else {
		Write-HoSt $outP
	}
}
```

{{Out}}

```txt
PS> ./GENFB
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
PS>
```



## Python


```python
def genfizzbuzz(factorwords, numbers):
    factorwords.sort(key=lambda p: p[0])
    lines = []
    for num in numbers:
        words = ''.join(wrd for fact, wrd in factorwords if (num % fact) == 0)
        lines.append(words if words else str(num))
    return '\n'.join(lines)

if __name__ == '__main__':
    print(genfizzbuzz([(5, 'Buzz'), (3, 'Fizz'), (7, 'Baxx')], range(1, 21)))
```


{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



'''Alternative version - generator using counters instead of modulo'''
{{works with|Python|3.x}}

```Python
from collections import defaultdict

n = 100
mods = {
    3: "Fizz",
    5: "Buzz",
}

def fizzbuzz(n=n, mods=mods):
    factors = defaultdict(list)
    for mod in mods:
        factors[mod].append(mod)

    for i in range(1,n+1):
        res = ''
        for mod in sorted(factors[i]):
            factors[i+mod].append(mod)
            res += mods[mod]
        del factors[i]
        yield res or str(i)

if __name__ == '__main__':
    n = int(input())
    mods = { int(k): v for k,v in (input().split(maxsplit=1) for _ in range(3)) }
    for line in fizzbuzz(n, mods):
        print(line)

```



'''Another version, using ranges with step 3, 5, etc. Preserves order and duplicate moduli.'''

```Python
from collections import defaultdict

n = 100
mods = [
    (3, 'Fizz'),
    (5, 'Buzz'),
]

def fizzbuzz(n=n, mods=mods):
    res = defaultdict(str)

    for num, name in mods:
        for i in range(num, n+1, num):
            res[i] += name

    return '\n'.join(res[i] or str(i) for i in range(1, n+1))


if __name__ == '__main__':
    n = int(input())

    mods = []
    while len(mods) != 3:   # for reading until EOF change 3 to -1
        try:
            line = input()
        except EOFError:
            break
        idx = line.find(' ')                        # preserves whitespace
        num, name = int(line[:idx]), line[idx+1:]   #   after the first space
        mods.append((num, name))    # preserves order and duplicate moduli

    print(fizzbuzz(n, mods))

```


{{Out}}

```txt

>>> mods = [
...   (4, 'Four '),
...   (6, 'six '),
...   (2, 'Two '),
...   (8, 'eight... '),
...   (6, 'HA! SIX!'),
... ]
>>> print(fizzbuzz(16, mods))
1
Two
3
Four Two
5
six Two HA! SIX!
7
Four Two eight...
9
Two
11
Four six Two HA! SIX!
13
Two
15
Four Two eight...

```


'''First contribution to rosettacode, uses list comprehension to join the lists and args to allow for arbitrary range'''

```python
def genfizzbuzz(numberlist, wordlist, *args):
    nml = [[numberlist[i], wordlist[i]] for i in range(len(numberlist))]
    for z in range(*args):
        res = ""
        for j in nml:
            if z % j[0] == 0:
                res += j[1]
        print(res or z)


genfizzbuzz([3, 5, 7], ['Fizz', 'Buzz', 'Baxx'], 1, 21)

```


{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```


'''One liner using generator expressions'''

```python
n = 20
mappings = {3: "Fizz", 5: "Buzz", 7: "Baxx"}
for i in range(1, n+1): print(''.join(word*(i%key==0) for  key, word in mappings.items()) or i)
```



## Racket

{{trans|Python}}

```Racket
#lang racket/base

(define (get-matches num factors/words)
  (for*/list ([factor/word (in-list factors/words)]
              [factor (in-value (car factor/word))]
              [word (in-value (cadr factor/word))]
              #:when (zero? (remainder num factor)))
    word))

(define (gen-fizzbuzz from to factors/words)
  (for ([num (in-range from to)])
    (define matches (get-matches num factors/words))
    (displayln (if (null? matches)
                  (number->string num)
                  (apply string-append matches)))))

(gen-fizzbuzz 1 21 '((3 "Fizz")
                     (5 "Buzz")
                     (7 "Baxx")))
```

{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## REXX


### idiomatic version


```rexx
/*REXX program shows a generalized  FizzBuzz  program:  #1 name1    #2 name2   ···      */
parse arg h $                                    /*obtain optional arguments from the CL*/
if h='' | h=","  then h=20                       /*Not specified?  Then use the default.*/
if $='' | $=","  then $= "3 Fizz 5 Buzz 7 Baxx"  /* "      "         "   "   "     "    */

  do j=1  for h;             _=                  /*traipse through the numbers to   H.  */
    do k=1  by 2  for words($) % 2               /*   "       "     " factors  in   J.  */
    if j//word($,k)==0  then _=_ || word($,k+1)  /*Is it a factor?  Then append it to _ */
    end   /*k*/                                  /* [↑]  Note:  the factors may be zero.*/
  say word(_ j,1)                                /*display the number  or  its factors. */
  end     /*j*/                                  /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default inputs:

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



### optimized version


```rexx
/*REXX program shows a generalized  FizzBuzz  program:  #1 name1    #2 name2   ···      */
parse arg h $                                    /*obtain optional arguments from the CL*/
if h='' | h=","  then h=20                       /*Not specified?  Then use the default.*/
if $='' | $=","  then $= "3 Fizz 5 Buzz 7 Baxx"  /* "      "         "   "   "     "    */
factors=words($) % 2                             /*determine number of factors to use.  */

    do i=1  by 2  for factors                    /*parse the number factors to be used. */
    #.i=word($, i);   @.i=word($, i+1)           /*obtain the factor and its  "name".   */
    end   /*i*/

    do j=1  for h;                    _=         /*traipse through the numbers to   H.  */
                   do k=1  by 2  for factors     /*   "       "     " factors  in   J.  */
                   if j//#.k==0  then _=_ || @.k /*Is it a factor?  Then append it to _ */
                   end   /*k*/                   /* [↑]  Note:  the factors may be zero.*/
    say word(_ j,1)                              /*display the number  or  its factors. */
    end                  /*j*/                   /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.




## Ring


```ring

limit = 20
for n = 1 to limit
    if n % 3 = 0 see "" + n + " = " + "Fizz"+ nl
    but n % 5 = 0 see "" + n + " = " + "Buzz" + nl
    but n % 7 = 0 see "" + n + " = " + "Baxx" + nl
    else see "" + n + " = " + n + nl ok
next


```



## Ruby


```ruby
def general_fizzbuzz(text)
  num, *nword = text.split
  num = num.to_i
  dict = nword.each_slice(2).map{|n,word| [n.to_i,word]}
  (1..num).each do |i|
    str = dict.map{|n,word| word if i%n==0}.join
    puts str.empty? ? i : str
  end
end

text = <<EOS
20
3 Fizz
5 Buzz
7 Baxx
EOS

general_fizzbuzz(text)
```


{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Rust



```rust
use std::io;
use std::io::BufRead;

fn parse_entry(l: &str) -> (i32, String) {
    let params: Vec<&str> = l.split(' ').collect();

    let divisor = params[0].parse::<i32>().unwrap();
    let word = params[1].to_string();
    (divisor, word)
}

fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(|l| l.unwrap());

    let l = lines.next().unwrap();
    let high = l.parse::<i32>().unwrap();

    let mut entries = Vec::new();
    for l in lines {
        if &l == "" { break }
        let entry = parse_entry(&l);
        entries.push(entry);
    }

    for i in 1..(high + 1) {
        let mut line = String::new();
        for &(divisor, ref word) in &entries {
            if i % divisor == 0 {
                line = line + &word;
            }
        }
        if line == "" {
            println!("{}", i);
        } else {
            println!("{}", line);
        }
    }
}
```


This solution stores the Fizz Buzz state in a struct, leveraging types and the standard library for a more general solution:


```rust
use std::collections::BTreeMap;
use std::fmt::{self, Write};
use std::io::{self, Stdin};

#[derive(Debug, PartialEq)]
pub struct FizzBuzz {
    end: usize,
    factors: Factors,
}

impl FizzBuzz {
    fn from_reader(rdr: &Stdin) -> Result<FizzBuzz, Box<dyn std::error::Error>> {
        let mut line = String::new();
        rdr.read_line(&mut line)?;

        let end = line.trim().parse::<usize>()?;

        let mut factors = Factors::new();

        loop {
            let mut line = String::new();
            rdr.read_line(&mut line)?;

            if line.trim().is_empty() { break; }

            let mut split = line.trim().splitn(2, ' ');

            let factor = match split.next() {
                Some(f) => f.parse::<usize>()?,
                None => break,
            };

            let phrase = match split.next() {
                Some(p) => p,
                None => break,
            };

            factors.insert(factor, phrase.to_string());
        }

        Ok(FizzBuzz { end, factors })
    }
}

impl fmt::Display for FizzBuzz {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for n in 1..=self.end {
            let mut had_factor = false;

            // check for factors
            for (factor, phrase) in self.factors.iter() {
                if n % factor == 0 {
                    f.write_str(&phrase)?;
                    had_factor = true;
                }
            }

            if !had_factor {
                f.write_str(n.to_string().as_str())?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

type Factors = BTreeMap<usize, String>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = io::stdin();

    let fizz_buzz = FizzBuzz::from_reader(&input)?;

    println!("{}", fizz_buzz);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fizz_buzz_prints_expected_format() {
        let expected_factors = {
            let mut map = Factors::new();
            map.insert(3, "Fizz".to_string());
            map.insert(5, "Buzz".to_string());
            map.insert(7, "Baxx".to_string());
            map
        };

        let expected_end = 20;

        let fizz_buzz = FizzBuzz {
            end: expected_end,
            factors: expected_factors,
        };

        let expected = r#"1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
"#;
        let printed = format!("{}", fizz_buzz);

        assert_eq!(expected, &printed);
    }
}
```



## Sidef


```ruby
class FizzBuzz(schema=Hash(<3 Fizz 5 Buzz>...)) {
    method filter(this) {
        var fb = ''
        schema.sort_by {|k,_| k.to_i }.each { |pair|
            fb += (pair[0].to_i `divides` this ? pair[1] : '')
        }
        fb.len > 0 ? fb : this
    }
}
 
func GeneralFizzBuzz(upto, schema) {
    var ping = FizzBuzz()
    if (nil != schema) {
        ping.schema = schema.to_hash
    }
    (1..upto).map {|i| ping.filter(i) }
}
 
GeneralFizzBuzz(20, <3 Fizz 5 Buzz 7 Baxx>).each { .say }
```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Scala



```Scala
object GenericFizzBuzz extends App {

  val max = io.StdIn.readInt()
  val factors = io.Source.stdin.getLines().toSeq.sorted.map(_.split(" ", 2)).map(f => f(0).toInt -> f(1))

  1 to max foreach { i =>
    val words = factors collect { case (k, v) if i % k == 0 => v }
    println(if (words.nonEmpty) words.mkString else i)
  }

}
```


{{out}}

```txt

$ scala GenericFizzBuzz.scala
20
3 Fizz
5 Buzz
7 Baxx
^D

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## Tailspin


```tailspin

def input: {N: 110, words: [ { mod: 3, word: 'Fizz' }, { mod: 5, word: 'Buzz'}, {mod:7, word: 'Baxx'}]};

templates sayWords
  def i: $;
  templates maybeSay
    def word: $.word;
    $i mod $.mod -> (<0> $word !) !
  end maybeSay
  $input.words... -> maybeSay !
end sayWords

[ 1..$input.N -> '$->sayWords;' ] -> [i](<''> $i ! <> $ !) -> '$;
' -> !OUT::write

```

{{out}}

```txt

[1, 2, Fizz, 4, Buzz, Fizz, Baxx, 8, Fizz, Buzz, 11, Fizz, 13, Baxx, FizzBuzz, 16, 17, Fizz, 19, Buzz, FizzBaxx, 22, 23, Fizz, Buzz, 26, Fizz, Baxx, 29, FizzBuzz, 31, 32, Fizz, 34, BuzzBaxx, Fizz, 37, 38, Fizz, Buzz, 41, FizzBaxx, 43, 44, FizzBuzz, 46, 47, Fizz, Baxx, Buzz, Fizz, 52, 53, Fizz, Buzz, Baxx, Fizz, 58, 59, FizzBuzz, 61, 62, FizzBaxx, 64, Buzz, Fizz, 67, 68, Fizz, BuzzBaxx, 71, Fizz, 73, 74, FizzBuzz, 76, Baxx, Fizz, 79, Buzz, Fizz, 82, 83, FizzBaxx, Buzz, 86, Fizz, 88, 89, FizzBuzz, Baxx, 92, Fizz, 94, Buzz, Fizz, 97, Baxx, Fizz, Buzz, 101, Fizz, 103, 104, FizzBuzzBaxx, 106, 107, Fizz, 109, Buzz]

```



## Tcl


Tcl excels at metaprogramming, so this task is trivial.
For fun, the below implementation is a compatible extension of [[FizzBuzz#Tcl]]:


```Tcl
proc fizzbuzz {n args} {
    if {$args eq ""} {
        set args {{3 Fizz} {5 Buzz}}
    }
    while {[incr i] <= $n} {
        set out ""
        foreach rule $args {
            lassign $rule m echo
            if {$i % $m == 0} {append out $echo}
        }
        if {$out eq ""} {set out $i}
        puts $out
    }
}
fizzbuzz 20 {3 Fizz} {5 Buzz} {7 Baxx}
```



## Ursa

This program reads a max number, then reads factors until the user enters a blank line.

```ursa
#
# general fizzbuzz
#
decl int<> factors
decl string<> words
decl int max

# get the max number
out ">" console
set max (in int console)

# get the factors
decl string input
set input " "
while (not (= input ""))
        out ">" console
        set input (in string console)
        if (not (= input ""))
                append (int (split input " ")<0>) factors
                append (split input " ")<1> words
        end if
end while

# output all the numbers
decl int i
for (set i 1) (< i (+ max 1)) (inc i)
        decl boolean foundfactor
        set foundfactor false
        for (decl int j) (< j (size factors)) (inc j)
                if (= (mod i factors<j>) 0)
                        set foundfactor true
                        out words<j> console
                end if
        end for
        set j 0

        if (not foundfactor)
                out i console
        end if
        out endl console
end for
```

Output:

```txt
>20
>3 Fizz
>5 Buzz
>7 Baxx
>
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```


## VBA


```vb

Option Explicit

Private Type Choice
    Number As Integer
    Name As String
End Type

Private MaxNumber As Integer

Sub Main()
Dim U(1 To 3) As Choice, i As Integer, j As Integer, t$

    MaxNumber = Application.InputBox("Enter the max number : ", "Integer please", Type:=1)
    For i = 1 To 3
        U(i) = UserChoice
    Next
    For i = 1 To MaxNumber
        t = vbNullString
        For j = 1 To 3
            If i Mod U(j).Number = 0 Then t = t & U(j).Name
        Next
        Debug.Print IIf(t = vbNullString, i, t)
    Next i
End Sub

Private Function UserChoice() As Choice
Dim ok As Boolean

    Do While Not ok
        UserChoice.Number = Application.InputBox("Enter the factors to be calculated : ", "Integer please", Type:=1)
        UserChoice.Name = InputBox("Enter the corresponding word : ")
        If StrPtr(UserChoice.Name) <> 0 And UserChoice.Number < MaxNumber Then ok = True
    Loop
End Function
```

{{out}}
With the entry :
Max : 120;
3 : Fizz;
5 : Buzz;
7 : Baxx

```txt
 1
 2
Fizz
 4
Buzz
Fizz
Baxx
 8
Fizz
Buzz
 11
Fizz
 13
Baxx
FizzBuzz
 16
 17
Fizz
 19
Buzz
FizzBaxx
 22
 23
......
Buzz
 101
Fizz
 103
 104
FizzBuzzBaxx
 106
 107
Fizz
 109
Buzz
Fizz
Baxx
 113
Fizz
Buzz
 116
Fizz
 118
Baxx
FizzBuzz
```



## VBScript


```vb
'The Function
Function FizzBuzz(range, mapping)
    data = Array()

    'Parse the mapping and put to "data" array
    temp = Split(mapping, ",")
    ReDim data(UBound(temp),1)
    For i = 0 To UBound(temp)
        map = Split(temp(i), " ")
        data(i, 0) = map(0)
        data(i, 1) = map(1)
    Next

    'Do the loop
    For i = 1 to range
        noMatch = True
        For j = 0 to UBound(data, 1)
            If (i Mod data(j, 0)) = 0 Then
                WScript.StdOut.Write data(j, 1)
                noMatch = False
            End If
        Next
        If noMatch Then WScript.StdOut.Write i
        WScript.StdOut.Write vbCrLf
    Next
End Function

'The Main Thing
WScript.StdOut.Write "Range? "
x = WScript.StdIn.ReadLine
WScript.StdOut.Write "Mapping? "
y = WScript.StdIn.ReadLine
WScript.StdOut.WriteLine ""
FizzBuzz x, y
```

{{Out|Sample Run}}

```txt
\Desktop>cscript /nologo fizzbuzz.vbs
Range? 20
Mapping? 3 Fizz,5 Buzz,7 Baxx

1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

\Desktop>
```



## Visual Basic .NET


```vbnet
Imports System.Globalization

Module Program
    Sub Main()
        Console.Write("Max: ")
        Dim max = Integer.Parse(Console.ReadLine(), CultureInfo.InvariantCulture)

        Dim factors As New SortedDictionary(Of Integer, String)

        Const NUM_FACTORS = 3
        For i = 1 To NUM_FACTORS
            Console.Write("Factor {0}: ", i)
            Dim input = Console.ReadLine().Split()
            factors.Add(Integer.Parse(input(0), CultureInfo.InvariantCulture), input(1))
        Next

        For i = 1 To max
            Dim anyMatches = False
            For Each factor In factors
                If i Mod factor.Key = 0 Then
                    Console.Write(factor.Value)
                    anyMatches = True
                End If
            Next
            If Not anyMatches Then Console.Write(i)
            Console.WriteLine()
        Next
    End Sub
End Module
```


{{out}}

```txt
Max: 20
Factor 1: 7 Baxx
Factor 2: 3 Fizz
Factor 3: 5 Buzz
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz
```



## zkl


```zkl
stop:=ask("Count: ").toInt();
fizzBuzzers:=List();
do(3){ n,txt:=ask(">").split(); fizzBuzzers.append(T(n.toInt(),txt)) }
foreach n in ([1..stop]){
   s:=fizzBuzzers.filter('wrap([(fb,txt)]){ n%fb==0 }).apply("get",1).concat();
   println(s or n);
}
```

{{out}}

```txt

$ zkl bbb
Count: 20
>3 Fizz
>5 Buzz
>7 Baxx
1
2
Fizz
4
Buzz
Fizz
Baxx
8
Fizz
Buzz
11
Fizz
13
Baxx
FizzBuzz
16
17
Fizz
19
Buzz

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 INPUT "Maximum number: ";max
20 INPUT "Number of factors: ";n
30 DIM f(n): DIM w$(n,4)
40 FOR i=1 TO n
50 INPUT "Input value-ENTER-word: ";f(i);w$(i)
60 NEXT i
70 FOR i=1 TO max
80 LET matched=0
90 FOR j=1 TO n
100 IF FN m(i,f(j))=0 THEN LET matched=1: PRINT w$(j);
110 NEXT j
120 IF NOT matched THEN PRINT ;i: GO TO 140
130 PRINT
140 NEXT i
150 DEF FN m(a,b)=a-INT (a/b)*b
```

