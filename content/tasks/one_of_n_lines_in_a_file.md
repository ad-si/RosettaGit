+++
title = "One of n lines in a file"
description = ""
date = 2019-08-26T20:16:32Z
aliases = []
[extra]
id = 10467
[taxonomies]
categories = ["task"]
tags = []
+++

A method of choosing a line randomly from a file:
::* Without reading the file more than once
::* When substantial parts of the file cannot be held in memory
::* Without knowing how many lines are in the file
Is to:
::* keep the first line of the file as a possible choice, then
::* Read the second line of the file if possible and make it the possible choice if a uniform random value between zero and one is less than 1/2.
::* Read the third line of the file if possible and make it the possible choice if a uniform random value between zero and one is less than 1/3.
::* ...
::* Read the Nth line of the file if possible and make it the possible choice if a uniform random value between zero and one is less than 1/N

::* Return the computed possible choice when no further lines exist in the file.


## Task

# Create a function/method/routine called <code>one_of_n</code> that given <code>n</code>, the number of actual lines in a file, follows the algorithm above to return an integer - the line number of the line chosen from the file.
The number returned can vary, randomly, in each run.
# Use <code>one_of_n</code> in a ''simulation'' to find what woud be the chosen line of a 10 line file simulated 1,000,000 times.
# Print and show how many times each of the 10 lines is chosen as a rough measure of how well the algorithm works.


Note: You may choose a smaller number of repetitions if necessary, but mention this up-front.

Note: This is a specific version of a Reservoir Sampling algorithm: https://en.wikipedia.org/wiki/Reservoir_sampling





## Ada


```Ada
with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure One_Of_N is

   Num_Of_Lines: constant Positive := 10;

   package Rnd renames Ada.Numerics.Float_Random;
   Gen: Rnd.Generator; -- used globally

   function Choose_One_Of_N(Last_Line_Number: Positive) return Natural is
      Current_Choice: Natural := 0;
   begin
      for Line_Number in 1 .. Last_Line_Number loop
        if (Rnd.Random(Gen) * Float(Line_Number) <= 1.0) then
           Current_Choice := Line_Number;
        end if;
      end loop;
      return Current_Choice;
   end Choose_One_Of_N;

   Results: array(1 .. Num_Of_Lines) of Natural := (others => 0);
   Index: Integer range 1 .. Num_Of_Lines;

begin
   Rnd.Reset(Gen);
   for I in 1 .. 1_000_000 loop    -- compute results
      Index := Choose_One_Of_N(Num_Of_Lines);
      Results(Index) := Results(Index) + 1;
   end loop;

   for R in Results'Range loop    -- output results
      Ada.Text_IO.Put(Integer'Image(Results(R)));
   end loop;
end One_Of_N;
```


Example output:
```txt
 100104 100075 99761 99851 100457 100315 100101 99557 99678 100101
```



## Aime


```aime
one_of_n(integer n)
{
    integer i, r;

    i = r = 0;
    while ((r += 1) < n) {
        i = drand(r) ? i : r;
    }

    i;
}

main(void)
{
    integer i;
    index x;

    i = 1000000;
    do {
        x[one_of_n(10)] += 1;
    } while (i -= 1);

    x.ucall(o_winteger, 1, 7);
    o_newline();

    0;
}
```

```txt
  99804 100236  99846 100484  99888  99639  99886  99810  99923 100484
```



## ALGOL 68


```algol68
BEGIN
   INT max lines = 10;		CO Should be read from a file. CO
   [max lines]INT stats;
   FOR i TO max lines DO stats[i] := 0 OD;
   first random (42);		CO Should have rather more entropy! CO
   PROC one of n = (INT n) INT :
   BEGIN
      INT result := 1;
      FOR i TO n DO (random < 1/i | result := i) OD;
      result
   END;
   TO 1000000 DO stats[one of n (max lines)] +:= 1 OD;
   print (("Line  Number times chosen", newline));
   FOR i TO max lines DO printf (($g(0)7xg(0)l$, i, stats[i])) OD
END
```

```txt

Line  Number times chosen
1       99808
2       99715
3       100018
4       100064
5       99373
6       100687
7       99363
8       100349
9       100029
10       100594

```



## Applesoft BASIC

A million iterations will take a very very long time to run.  I suggest cranking the end value of J down to 1000 or less in line 20, and run this at full speed in a modern Apple II emulator.  Initializing with RND(0) in line 10, seeds the RND function to be random, otherwise you may see the same results every time.

```ApplesoftBASIC
10 I = RND(0) : REMRANDOM SEED

20 FOR J = 1 TO 1000000 : REMMAYBE TRY 100 ON A 1MHZ APPLE II
30     N = 10 : GOSUB 100"ONE_OF_N
40     C(C) = C(C) + 1
50 NEXT

60 FOR J = 1 TO 10
70     PRINT J, C(J)
80 NEXT
90 END

100 REMONE_OF_N
110 FOR I = 1 TO N
120     IF INT(RND(1) * I) = 0 THEN C = I
130 NEXT I
140 RETURN
```



## AutoHotkey

This simulation is for 100,000 repetitions.

```AutoHotkey
one_of_n(n){
    ; One based line numbers
    choice = 1
    Loop % n-1
    {
        Random, rnd, 1, % A_Index+1
        If rnd = 1
            choice := A_Index+1
    }
    return choice
}
one_of_n_test(n=10, trials=100000){
    bins := [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Loop % trials
        bins[one_of_n(n)] += 1
    return bins
}

b := one_of_n_test()
Loop 10
   out .= A_Index ": " b[A_Index] "`n"
MsgBox % out
```

Output:

```txt
---------------------------
One of n.ahk
---------------------------
1: 10046
2: 9958
3: 9953
4: 9973
5: 9915
6: 10212
7: 9941
8: 9993
9: 10063
10: 9946

---------------------------
OK
---------------------------
```



## AWK


```AWK
#!/usr/bin/gawk -f
#
# Usage:
#   gawk -v Seed=$RANDOM -f one_of_n_lines_in_a_file.awk
#
BEGIN {
   srand(Seed ? Seed : 1);
}
{
   if (NR*rand() < 1 ) {
      line = $0
   }
}
END {
   print line;
}
```


Test randomness

```bash

for i in `seq 1 10000`; do seq 1 10 | awk -v Seed=$RANDOM -f one_of_n_lines_in_a_file.awk; done |sort|uniq -c

```



```txt

     91 1
    102 10
    120 2
     82 3
    103 4
     93 5
    112 6
    108 7
     94 8
     95 9

```



## BASIC

```qbasic
DECLARE FUNCTION oneofN& (n AS LONG)

DIM L0 AS LONG, c AS LONG
DIM chosen(1 TO 10) AS LONG

RANDOMIZE TIMER

FOR L0 = 1 TO 1000000
    c = oneofN&(10)
    chosen(c) = chosen(c) + 1
NEXT

FOR L0 = 1 TO 10
    PRINT L0, chosen(L0)
NEXT

FUNCTION oneofN& (n AS LONG)
    'assumes first line is 1
    DIM L1 AS LONG, choice AS LONG
    FOR L1 = 1 TO n
        IF INT(RND * L1) = 0 THEN choice = L1
    NEXT
    oneofN& = choice
END FUNCTION
```


Sample output:
 1             100106
 2             99533
 3             100318
 4             100203
 5             99750
 6             100412
 7             99625
 8             100019
 9             100154
 10            99880


## Batch File

I chose to only run this 100,000 times as it is slightly time consuming (running at 105 simulations / second).
I attempted to optimise a few ways but none were especially helpful.

There is also no need to parse the actual number of lines in the file to <code>one_of_n</code>

'''Batch file to call one_of_n'''

```dos

@echo off

for /l %%i in (1,1,10000) do call one_of_n
:: To show progress add to the FOR loop code block -
:: title %%i

for /l %%i in (1,1,10) do (
	for /f "usebackq tokens=1,2 delims=:" %%j in (`find /c "%%i" output.txt`) do echo Line %%i =%%k
)
del output.txt
pause>nul

```



'''one_of_n'''

```dos

setlocal enabledelayedexpansion

set /a count=1

for /f "tokens=*" %%i in (file.txt) do (
	set /a rand=!random! %% !count!
	if !rand!==0 set line=!count!
	set /a count+=1
)
echo %line% >> output.txt

endlocal
exit /b

```



'''Output'''

```txt

Line 1 = 9,868‬
Line 2 = 10032
Line 3 = 9988
Line 4 = 10160
Line 5 = 10189
Line 6 = 9953
Line 7 = 9851
Line 8 = 10052
Line 9 = 9937
Line 10 = 9970

```



## BBC BASIC


```bbcbasic
      @% = 7 : REM Column width
      DIM cnt%(10)
      FOR test% = 1 TO 1000000
        cnt%(FNone_of_n(10)) += 1
      NEXT
      FOR i% = 1 TO 10
        PRINT cnt%(i%);
      NEXT
      PRINT
      END

      DEF FNone_of_n(n%)
      LOCAL i%, l%
      FOR i% = 1 TO n%
        IF RND(1) <= 1/i% l% = i%
      NEXT
      = l%
```

'''Output:'''

```txt

  99846 100156  99812 100275  99995 100019  99217  99778 100409 100493

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

inline int irand(int n)
{
	int r, randmax = RAND_MAX/n * n;
	while ((r = rand()) >= randmax);
	return r / (randmax / n);
}

inline int one_of_n(int n)
{
	int i, r = 0;
	for (i = 1; i < n; i++) if (!irand(i + 1)) r = i;
	return r;
}

int main(void)
{
	int i, r[10] = {0};

	for (i = 0; i < 1000000; i++, r[one_of_n(10)]++);
	for (i = 0; i < 10; i++)
		printf("%d%c", r[i], i == 9 ? '\n':' ');

	return 0;
}
```
output

```txt
100561 99814 99816 99721 99244 99772 100790 100072 99997 100213
```



## C++

```cpp
#include <random>
#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

mt19937 engine; //mersenne twister

unsigned int one_of_n(unsigned int n) {
	unsigned int choice;
	for(unsigned int i = 0; i < n; ++i) {
		uniform_int_distribution<unsigned int> distribution(0, i);
		if(!distribution(engine))
			choice = i;
	}
	return choice;
}

int main() {
	engine = mt19937(random_device()()); //seed random generator from system
	unsigned int results[10] = {0};
	for(unsigned int i = 0; i < 1000000; ++i)
		results[one_of_n(10)]++;
	ostream_iterator<unsigned int> out_it(cout, " ");
	copy(results, results+10, out_it);
	cout << '\n';
}

```
output

```txt
99981 99806 100190 99831 99833 100291 99356 100165 100279 100268
```


## C#


```c#

    class Program
    {
        private static Random rnd = new Random();
        public static int one_of_n(int n)
        {
            int currentChoice = 1;
            for (int i = 2; i <= n; i++)
            {
                double outerLimit = 1D / (double)i;
                if (rnd.NextDouble() < outerLimit)
                    currentChoice = i;
            }
            return currentChoice;
        }

        static void Main(string[] args)
        {
            Dictionary<int, int> results = new Dictionary<int, int>();
            for (int i = 1; i < 11; i++)
                results.Add(i, 0);

            for (int i = 0; i < 1000000; i++)
            {
                int result = one_of_n(10);
                results[result] = results[result] + 1;
            }

            for (int i = 1; i < 11; i++)
                Console.WriteLine("{0}\t{1}", i, results[i]);
            Console.ReadLine();
        }
    }

```


```txt

1       99777
2       100289
3       100028
4       100294
5       99777
6       100330
7       100480
8       99617
9       99682
10      99726

```



## Chapel



```chapel
use Random;

proc one_of_n(n) {
    var rand = new RandomStream();
    var keep = 1;

    for i in 2..n do
        if rand.getNext() < 1.0 / i then
            keep = i;

    delete rand;

    return keep;
}
```



## Clojure

The function ''rand-seq-elem'' actually implements the algorithm; ''one-of-n'' uses it to select a random element from the sequence (1 ... n). (Though they weren't automatically highlighted when I wrote this, ''rand-int'', ''second'', and ''frequencies'' and ''println'' are also standard functions)

```clojure
(defn rand-seq-elem [sequence]
  (let [f (fn [[k old] new]
            [(inc k) (if (zero? (rand-int k)) new old)])]
    (->> sequence (reduce f [1 nil]) second)))

(defn one-of-n [n]
  (rand-seq-elem (range 1 (inc n))))

(let [countmap (frequencies (repeatedly 1000000 #(one-of-n 10)))]
  (doseq [[n cnt] (sort countmap)]
    (println n cnt)))
```


Sample output
<lang>1 99350
2 99933
3 99820
4 100266
5 100675
6 100370
7 99842
8 100020
9 100342
10 99382
```


To actually get a randomly selected line from a file:

```clojure
(require '[clojure.java.io :as io])

(defn rand-line [filename]
  (with-open [reader (io/reader filename)]
    (rand-seq-elem (line-seq reader)))
```



## Common Lisp

Great place to use closures.

```lisp
(defun one-of-n-fn ()
  (let ((cur 0) (sel nil))
    #'(lambda (v)
        (setq cur (+ cur 1))
        (if (eql 0 (random cur)) (setq sel v))
        sel)))

(defun test-one-of-n ()
  (let ((counts (make-array 10 :initial-contents '(0 0 0 0 0 0 0 0 0 0)))
        (fnt))
    (do ((test 0 (+ 1 test)))
        ((eql test 1000000) counts)
      (setq fnt (one-of-n-fn))
      (do ((probe 0 (+ 1 probe)))
          ((eql probe 9) t)
        (funcall fnt probe))
      (let* ((sel (funcall fnt 9)))
        (setf (aref counts sel) (+ 1 (aref counts sel)))))))

```

Output:
<lang>#(100104 100144 100004 99593 100049 99706 100612 99481 100029 100278)
```



## D


```d
import std.stdio, std.random, std.algorithm;

// Zero-based line numbers.
int oneOfN(in int n) {
    int choice = 0;
    foreach (immutable i; 1 .. n)
        if (!uniform(0, i + 1))
            choice = i;
    return choice;
}

void main() {
    int[10] bins;
    foreach (immutable i; 0 .. 1_000_000)
        bins[10.oneOfN]++;

    bins.writeln;
    writeln("Total of bins: ", bins[].sum);
}
```

```txt
[100091, 99940, 100696, 99799, 100234, 99419, 100225, 99716, 99942, 99938]
Total of bins: 1000000
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Simulates one_of_n_lines a 1000000 times.
		local
			t: INTEGER
			simulator: ARRAY [INTEGER]
		do
			create simulator.make_filled (0, 1, 10)
			create one.make
			across
				1 |..| 1000000 as c
			loop
				t := one.one_of_n_lines (10)
				simulator [t] := simulator [t] + 1
			end
			across
				simulator as s
			loop
				io.put_integer (s.item)
				io.new_line
			end
		end

	one: ONE_OF_N_LINES

end

```


```Eiffel

class
	ONE_OF_N_LINES

create
	make

feature {NONE}

	r: RANDOM

feature

	make
		do
			create r.make
		end

	one_of_n_lines (n: INTEGER): INTEGER
                        -- A integer between 1 and 'n', denoting a line.
		require
			n_is_positive: n > 0
		local
			p: REAL_64
		do
			across
				1 |..| n as c
			loop
				p := r.double_item
				if p < (1 / c.item) then
					Result := c.item
				end
				r.forth
			end
		ensure
			Result_in_file: Result <= n
			Result_is_positive: Result > 0
		end

end

```

```txt

99698
100643
100017
100251
99742
99877
100244
99897
99738
99893

```



## Elixir

```elixir
defmodule One_of_n_lines_in_file do
  def task do
    dict = Enum.reduce(1..1000000, %{}, fn _,acc ->
      Map.update( acc, one_of_n(10), 1, &(&1+1) )
    end)
    Enum.each(Enum.sort(Map.keys(dict)), fn x ->
      :io.format "Line ~2w selected: ~6w~n", [x, dict[x]]
    end)
  end

  def one_of_n( n ), do: loop( n, 2, :rand.uniform, 1 )

  def loop( max, n, _random, acc ) when n == max + 1, do: acc
  def loop( max, n, random, _acc ) when random < (1/n), do: loop( max, n + 1, :rand.uniform, n )
  def loop( max, n, _random, acc ), do: loop( max, n + 1, :rand.uniform, acc )
end

One_of_n_lines_in_file.task
```


```txt

Line  1 selected: 100327
Line  2 selected:  99858
Line  3 selected: 100292
Line  4 selected:  99992
Line  5 selected: 100138
Line  6 selected:  99807
Line  7 selected:  99982
Line  8 selected: 100065
Line  9 selected:  99765
Line 10 selected:  99774

```



## Erlang


```Erlang

-module( one_of_n_lines_in_file  ).

-export( [one_of_n/1, task/0] ).

one_of_n( N ) -> loop( N, 2, random:uniform(), 1 ).

task() ->
	Dict = lists:foldl( fun update_counter/2,  dict:new(), lists:seq(1, 1000000) ),
	[io:fwrite("Line ~p selected: ~p~n", [X, dict:fetch(X, Dict)]) || X <- lists:sort(dict:fetch_keys(Dict))].


loop( Max, N, _Random, Acc ) when N =:= Max + 1 -> Acc;
loop( Max, N, Random, _Acc ) when Random < (1/N) -> loop( Max, N + 1, random:uniform(), N );
loop( Max, N, _Random, Acc ) -> loop( Max, N + 1, random:uniform(), Acc ).

update_counter( _N, Dict ) -> dict:update_counter( one_of_n(10), 1, Dict ).

```

```txt

74> one_of_n_lines_in_file:task().
Line 1 selected: 100038
Line 2 selected: 99849
Line 3 selected: 99851
Line 4 selected: 99661
Line 5 selected: 100326
Line 6 selected: 100485
Line 7 selected: 99760
Line 8 selected: 99920
Line 9 selected: 100129
Line 10 selected: 99981

```



## ERRE


```ERRE

PROGRAM ONE_OF_N

DIM CNT[10]

PROCEDURE ONE_OF_N(N->L)
  LOCAL I
  FOR I=1 TO N DO
     IF RND(1)<=1.0/I THEN L=I END IF
  END FOR
END PROCEDURE

BEGIN
  N=10
  RANDOMIZE(TIMER)       ! init
  FOR TEST=1 TO 1000000 DO
     ONE_OF_N(N->L)
     CNT[L]+=1
  END FOR
  FOR I=1 TO N DO
     PRINT(CNT[I];)
  END FOR
  PRINT
END PROGRAM

```

```txt

 99864  99893  99973  100016  100097  100169  100561  99804  100006  99617

```



## Euphoria


```Euphoria
--  One of n lines in a file
include std/rand.e
include std/math.e

function one_of_n(integer n)
	integer line_num = 1
	for i = 2 to n do
		if rnd() < 1 / i then
			line_num = i
		end if
	end for
	return line_num
end function

procedure main()
	integer num_reps = 1000000, num_lines_in_file = 10
	sequence lines = repeat(0,num_lines_in_file)
	for i = 1 to num_reps do
		lines[one_of_n(num_lines_in_file)] += 1
	end for
	for i = 1 to num_lines_in_file do
		printf(1,"Number of times line %d was selected: %g\n", {i,lines[i]})
	end for
	printf(1,"Total number selected: %d\n", sum(lines) )
end procedure

main()

```

Sample Output:

```txt

Number of times line 1 was selected: 100154
Number of times line 2 was selected: 99778
Number of times line 3 was selected: 99906
Number of times line 4 was selected: 99727
Number of times line 5 was selected: 100025
Number of times line 6 was selected: 100465
Number of times line 7 was selected: 99738
Number of times line 8 was selected: 100135
Number of times line 9 was selected: 99871
Number of times line 10 was selected: 100201
Total number selected: 1000000

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main args =
    let rnd = new Random()

    let one_of_n n =
        let rec loop i r =
            if i >= n then r else
                if rnd.Next(i + 1) = 0
                then loop (i + 1) i
                else loop (i + 1) r
        loop 1 0

    let test n trials =
        let ar = Array.zeroCreate n
        for i = 1 to trials do
            let d = one_of_n n
            ar.[d] <- 1 + ar.[d]
        Console.WriteLine (String.Join(" ", ar))

    test 10 1000000
    0
```

Output

```txt
99721 100325 99939 99579 100174 100296 99858 99910 100192 100006
```



## Factor

''random-line'' uses an input stream. <code>"/etc/passwd" ascii [ random-line . ] with-file-reader</code> would print a random line from /etc/passwd.


```factor
! rosettacode/random-line/random-line.factor
USING: io kernel locals math random ;
IN: rosettacode.random-line

:: random-line ( -- line )
    readln :> choice! 1 :> count!
    [ readln dup ]
    [ count 1 + dup count! random zero?
        [ choice! ] [ drop ] if
    ] while drop
    choice ;
```


''one-of-n'' wants to use the same algorithm. Factor has duck typing, so ''one-of-n'' creates a mock object that quacks like an input stream. This mock object only responds to ''stream-readln'', not the other methods of stream protocol. This works because ''random-line'' only needs ''stream-readln''. The mock response is a line number instead of a real line.


```factor
! rosettacode/one-of-n/one-of-n.factor
USING: accessors io kernel math rosettacode.random-line ;
IN: rosettacode.one-of-n

<PRIVATE
TUPLE: mock-stream count last ;
: <mock-stream> ( n -- stream )
    mock-stream new 0 >>count swap >>last ;
M: mock-stream stream-readln ! stream -- line
    dup [ count>> ] [ last>> ] bi <
    [ [ 1 + ] change-count count>> ]
    [ drop f ] if ;
PRIVATE>

: one-of-n ( n -- line )
    <mock-stream> [ random-line ] with-input-stream* ;

USING: assocs formatting locals sequences sorting ;
<PRIVATE
: f>0 ( object/f -- object/0 )
    dup [ drop 0 ] unless ;
:: test-one-of-n ( -- )
    H{ } clone :> chosen
    1000000 [
        10 one-of-n chosen [ f>0 1 + ] change-at
    ] times
    chosen keys natural-sort [
        dup chosen at "%d chosen %d times\n" printf
    ] each ;
PRIVATE>
MAIN: test-one-of-n
```



```txt
$ ./factor -run=rosettacode.one-of-n
Loading resource:work/rosettacode/one-of-n/one-of-n.factor
Loading resource:work/rosettacode/random-line/random-line.factor
Loading resource:basis/formatting/formatting.factor
Loading resource:basis/formatting/formatting-docs.factor
1 chosen 100497 times
2 chosen 100157 times
3 chosen 100207 times
4 chosen 99448 times
5 chosen 100533 times
6 chosen 99774 times
7 chosen 99535 times
8 chosen 99826 times
9 chosen 100058 times
10 chosen 99965 times
```



## Forth

{{works with|GNU Forth}}for random.fs and 1/f

```forth
require random.fs

: frnd
   rnd 0 d>f [ s" MAX-U" environment? drop 0 d>f 1/f ] fliteral f* ;
: u>f 0 d>f ;
: one_of_n ( u1 -- u2 )
   1 swap  1+ 2 ?do  frnd  i u>f 1/f  f<  if drop i then  loop ;

create hist 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  does> swap cells + ;
: simulate  1000000 0 do  1  10 one_of_n 1- hist  +!  loop ;
: .hist  cr 10 0 do  i 1+ 2 .r ." : "  i hist @ .  cr loop ;

simulate .hist bye
```

```txt
&gt; gforthamd64 rosetta_one_of_n.fs

 1: 99381
 2: 99970
 3: 99793
 4: 100035
 5: 100195
 6: 100147
 7: 99583
 8: 100135
 9: 100309
10: 100452
```



## Go


```go
package main

import (
    "bufio"
    "fmt"
    "io"
    "math/rand"
    "time"
)

// choseLineRandomly implements the method described in the task.
// input is a an io.Reader, which could be an os.File, for example.
// Or, to implement a simulation, it could be anything else that implements
// io.Reader.  The method as described suggests saving and returning
// lines, but the rest of the task requires line numbers.  This function
// thus returns both.
func choseLineRandomly(r io.Reader) (s string, ln int, err error) {
    br := bufio.NewReader(r)
    s, err = br.ReadString('\n')
    if err != nil {
        return
    }
    ln = 1
    lnLast := 1.
    var sLast string
    for {
        // note bufio.ReadString used here.  This effectively defines a
        // line of the file as zero or more bytes followed by a newline.
        sLast, err = br.ReadString('\n')
        if err == io.EOF {
            return s, ln, nil // normal return
        }
        if err != nil {
            break
        }
        lnLast++
        if rand.Float64() < 1/lnLast {
            s = sLast
            ln = int(lnLast)
        }
    }
    return // error return
}

// oneOfN function required for task item 1.  Specified to take a number
// n, the number of lines in a file, but the method (above) specified to
// to be used does not need n, but rather the file itself.  This function
// thus takes both, ignoring n and passing the file to choseLineRandomly.
func oneOfN(n int, file io.Reader) int {
    _, ln, err := choseLineRandomly(file)
    if err != nil {
        panic(err)
    }
    return ln
}

// simulated file reader for task item 2
type simReader int

func (r *simReader) Read(b []byte) (int, error) {
    if *r <= 0 {
        return 0, io.EOF
    }
    b[0] = '\n'
    *r--
    return 1, nil
}

func main() {
    // task item 2 simulation consists of accumulating frequency statistic
    // on 1,000,000 calls of oneOfN on simulated file.
    n := 10
    freq := make([]int, n)
    rand.Seed(time.Now().UnixNano())
    for times := 0; times < 1e6; times++ {
        sr := simReader(n)
        freq[oneOfN(n, &sr)-1]++
    }

    // task item 3.  show frequencies.
    fmt.Println(freq)
}
```

Output:

```txt

[99945 99770 99594 100532 99941 100223 99716 100217 99855 100207]

```



## Haskell

The function selItem will operate on any list, whether a lazy list of strings from a file or a list of numbers, as in the test.


```Haskell
import qualified Data.Map as M
import System.Random
import Data.List
import Control.Monad
import System.Environment

testFile = [1..10]

selItem g xs = foldl' f (head xs, 1, 2, g) $ tail xs
    where f :: RandomGen a => (b, Int, Int, a) -> b -> (b, Int, Int, a)
          f (c, cn, n, gen) l | v == 1    = (l, n, n+1, ngen)
                              | otherwise = (c, cn, n+1, ngen)
            where (v, ngen) = randomR (1, n) gen

oneOfN a = do
            g <- newStdGen
            let (r, _, _, _) = selItem g a
            return r

test = do
        x <- replicateM 1000000 (oneOfN testFile)
        let f m l = M.insertWith (+) l 1 m
        let results = foldl' f M.empty x
        forM_ (M.toList results) $ \(x, y) -> putStrLn $ "Line number " ++ show x ++
								 " had count :" ++ show y

main = do
        a <- getArgs
        g <- newStdGen
        if null a then test
                  else putStrLn.(\(l, n, _, _) -> "Line " ++
                               show n ++ ": " ++ l)
                       .selItem g.lines =<< (readFile $ head a)
```


Running without any args runs the test:


```txt
$ ./oneofn
Line number 1 had count :99887
Line number 2 had count :99739
Line number 3 had count :99749
Line number 4 had count :100689
Line number 5 had count :100126
Line number 6 had count :99986
Line number 7 had count :100699
Line number 8 had count :99569
Line number 9 had count :99121
Line number 10 had count :100435
```


Running with a filename argument prints the line number and the line to stdin:


```txt
$ ./oneofn test.txt
Line 6.0: This is line 6
```


=={{header|Icon}} and {{header|Unicon}}==
```Icon
procedure main() # one of n
   one_of_n_test(10,1000000)
end

procedure one_of_n(n)
   every i := 1 to n do
      choice := (?0  < 1. / i, i)
   return \choice | fail
end

procedure one_of_n_test(n,trials)
   bins := table(0)
   every i := 1 to trials do
         bins[one_of_n(n)] +:= 1
   every writes(bins[i := 1 to n]," ")
   return bins
end
```


Sample output:
```txt
99470 99806 99757 99921 100213 100001 99778 100385 100081 100588
```



## J


This implementation also implements line buffering, since the built-in line handling does not work quite how I want it to work.  That said, if a line is too large (many gigabytes, for example), the system will crawl to a halt when the line is read.


```j
randLineBig=:3 :0
  file=. boxopen y
  r=. ''
  n=. 1
  size=. fsize file
  blocksize=. 1e7
  buffer=. ''
  for_block. |: blocksize -~/\@(] <. [ * 0 1 +/i.@>.@%~) size do.
    buffer=. buffer, fread file,<block
    linends=. LF = buffer
    lines=. linends <;.2 buffer
    buffer=. buffer }.~ {: 1+I.linends
    pick=. (0 ?@$~ #lines) < % n+i.#lines
    if. 1 e. pick do.
      r=. ({:I.pick) {:: lines
    end.
    n=. n+#lines
  end.
  r
)
```


Usage:  randLineBig 'filename'

Testing:


```j
   (,LF,.~":,.i.10) fwrite <'seq.txt'
20
   (#;~.)/.~ /:~ <@randLineBig"0]1e6#<'seq.txt'
┌──────┬───┐
│99916 │0  │
├──────┼───┤
│99944 │1  │
├──────┼───┤
│100250│2  │
├──────┼───┤
│100621│3  │
├──────┼───┤
│99594 │4  │
├──────┼───┤
│100106│5  │
├──────┼───┤
│99957 │6  │
├──────┼───┤
│99975 │7  │
├──────┼───┤
│100054│8  │
├──────┼───┤
│99583 │9  │
└──────┴───┘
```



## Java

```Java
import java.util.Arrays;
import java.util.Random;

public class OneOfNLines {

	static Random rand;

	public static int oneOfN(int n) {
		int choice = 0;

		for(int i = 1; i < n; i++) {
			if(rand.nextInt(i+1) == 0)
				choice = i;
		}

		return choice;
	}

	public static void main(String[] args) {
		int n = 10;
		int trials = 1000000;
		int[] bins = new int[n];
		rand = new Random();

		for(int i = 0; i < trials; i++)
			bins[oneOfN(n)]++;


		System.out.println(Arrays.toString(bins));
	}
}

```


Sample output:

```txt
[99832, 99958, 100281, 99601, 99568, 99689, 100118, 99753, 100659, 100541]
```



## Julia


```Julia

const N = 10
const GOAL = 10^6

function oneofn{T<:Integer}(n::T)
    0 < n || error("n = ", n, ", but it should be positive.")
    oon = 1
    for i in 2:n
        rand(1:i) == 1 || continue
        oon = i
    end
    return oon
end

nhist = zeros(Int, N)
for i in 1:GOAL
    nhist[oneofn(N)] += 1
end

println("Simulating oneofn(", N, ") ", GOAL, " times:")
for i in 1:N
    println(@sprintf "   %2d => %6d" i nhist[i])
end

```


```txt

Simulating oneofn(10) 1000000 times:
    1 =>  99759
    2 =>  99933
    3 => 100052
    4 =>  99893
    5 => 100489
    6 =>  99727
    7 => 100114
    8 => 100116
    9 => 100139
   10 =>  99778

```



## Kotlin


```scala
// version 1.1.51

import java.util.Random

val r = Random()

fun oneOfN(n: Int): Int {
    var choice = 1
    for (i in 2..n) {
        if (r.nextDouble() < 1.0 / i) choice = i
    }
    return choice
}

fun main(args: Array<String>) {
    val n = 10
    val freqs = IntArray(n)
    val reps = 1_000_000
    repeat(reps) {
        val num = oneOfN(n)
        freqs[num - 1]++
    }
    for (i in 1..n) println("Line ${"%-2d".format(i)} = ${freqs[i - 1]}")
}
```


Sample output:

```txt

Line 1  = 100363
Line 2  = 99669
Line 3  = 100247
Line 4  = 100248
Line 5  = 100401
Line 6  = 99457
Line 7  = 100015
Line 8  = 100215
Line 9  = 99920
Line 10 = 99465

```



## Liberty BASIC


```lb

DIM chosen(10)

FOR i = 1 TO 10000'00
    c = oneofN(10)
    chosen(c) = chosen(c) + 1
NEXT

FOR i = 1 TO 10
    PRINT i, chosen(i)
NEXT

end

FUNCTION oneofN(n)
    FOR i = 1 TO n
        IF RND(1) < 1/i THEN oneofN = i
    NEXT
END FUNCTION

```

```txt

1             1008
2             934
3             1009
4             1012
5             981
6             1013
7             1015
8             986
9             1002
10            1040

```



## Lua


```Lua

math.randomseed(os.time())

local n = 10
local trials = 1000000

function one(n)
    local chosen = 1
    for i = 1, n do
        if math.random() < 1/i then
            chosen = i
        end
    end

    return chosen
end

-- 0 filled table for storing results
local results = {}
for i = 1, n do results[i] = 0 end

-- run simulation
for i = 1, trials do
    local result = one(n)
    results[result] = results[result] + 1
end

print("Value","Occurrences")
print("-------------------")
for k, v in ipairs(results) do
    print(k,v)
end

```

```txt

Value   Occurrences
-------------------
1       99393
2       100092
3       100412
4       100139
5       99773
6       99802
7       100020
8       99941
9       100063
10      100365

```



## Maple


```Maple

with(RandomTools[MersenneTwister]);
one_of_n_lines_in_a_file := proc(fn)
	local fid, N, n, L, l, line;
	fid := fopen(fn,'READ');
	if fid<0 then
		return;
	end if;
	N := 0;
	n := 1;
	while not feof(fid) do
		N := N+1;
		L := FileTools[Text][ReadLine](fid);
		if (N*GenerateFloat() < 1) then
			n := N;
			line := L;
		end if;
	end do;
	fclose(fid);
	return(n);
end proc;

```





## Mathematica


```Mathematica
chooseLine[file_] := Block[{strm = OpenRead[file], n = 1, rec, selected},
  					     rec = selected = Read[strm];
  					     While[rec =!= EndOfFile,
                                                    rec=Read[strm];
                                                    n++;
                                                    If[RandomReal[] < 1/n, selected = rec]];
                                             Close[strm];
                                             selected]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
function [n,line] = one_of_n_lines_in_a_file(fn)
fid = fopen(fn,'r');
if fid<0, return; end;
N = 0;
n = 1;
while ~feof(fid)
	N = N+1;
	L = fgetl(fid);
	if (N*rand() < 1)
		n = N;
		line = L;
	end;
end
fclose(fid);
```

test

```Matlab
x=zeros(1,10);
for k = 1:1e6;
  n = one_of_n_lines_in_a_file('f1');
  x(n) = x(n) + 1;
end;
[1:10;x]
```


```txt
        1        2        3        4        5        6        7        8        9       10
   105973   105715   106182   106213   105443   105255   106048   105999   105366   106070
```



## Nim


```nim
import math
randomize()

proc oneOfN(n: int): int =
  result = 0
  for x in 0 .. <n:
    if random(x+1) == 0:
      result = x

proc oneOfNTest(n = 10, trials = 1_000_000): seq[int] =
  result = newSeq[int](n)
  if n > 0:
    for i in 1..trials:
      inc result[oneOfN(n)]

echo oneOfNTest()
```

Output:

```txt
@[99912, 100048, 99894, 99697, 99806, 100316, 100209, 99898, 100027, 100193]
```



## OCaml


```ocaml
let one_of_n n =
  let rec aux i r =
    if i >= n then r else
      if Random.int (i + 1) = 0
      then aux (succ i) i
      else aux (succ i) r
  in
  aux 1 0

let test ~n ~trials =
  let ar = Array.make n 0 in
  for i = 1 to trials do
    let d = one_of_n n in
    ar.(d) <- succ ar.(d)
  done;
  Array.iter (Printf.printf " %d") ar;
  print_newline ()

let () =
  Random.self_init ();
  test ~n:10 ~trials:1_000_000
```


Executing:


```txt

$ ocamlopt -o one.opt one.ml
$ ./one.opt
 100620 99719 99928 99864 99760 100151 99553 100529 99800 100076

```



## PARI/GP

gp can't read individual lines from a file (PARI would be needed for that) but it can do the simulation easily. The <code>random()</code> function produces high-quality pseudorandom numbers (via Brent's [http://maths-people.anu.edu.au/~brent/pub/pub224.html XORGEN]) so the output passes a chi-square test easily (p = 0.848).

```parigp
one_of_n(n)={
  my(chosen=1);
  for(k=2,n,
    if(random(k)==0, chosen=k)
  );
  chosen;
}
v=vector(10); for(i=1,1e6, v[one_of_n(10)]++); v
```

```txt
%1 = [99933, 100021, 100125, 100071, 99876, 99485, 100108, 100183, 99861, 100337]
```



## Pascal

```pascal
Program OneOfNLines (Output);

function one_of_n(n: longint): longint;
  var
    i: longint;
  begin
    one_of_n := 1;
    for i := 2 to n do
      if random < 1.0 / i then
	one_of_n := i;
  end;

function sum(a: array of longint): longint;
  var
    i: integer;
  begin
    sum := 0;
    for i := low(a) to high(a) do
      sum := sum + a[i];
  end;

const
  num_reps = 1000000;
  num_lines_in_file = 10;

var
  lines: array[1..num_reps] of longint;
  i: longint;

begin
  randomize;
  for i := 1 to num_reps do
    lines[i] := 0;
  for i := 1 to num_reps do
    inc(lines[one_of_n(num_lines_in_file)]);
  for i := 1 to num_lines_in_file do
    writeln('Number of times line ', i, ' was selected: ', lines[i]);
  writeln('Total number selected: ', sum(lines));
end.
```

Output:

```txt
% ./OneOfNLines
Number of times line 1 was selected: 100388
Number of times line 2 was selected: 100206
Number of times line 3 was selected: 100427
Number of times line 4 was selected: 100092
Number of times line 5 was selected: 99951
Number of times line 6 was selected: 100114
Number of times line 7 was selected: 99518
Number of times line 8 was selected: 99900
Number of times line 9 was selected: 99967
Number of times line 10 was selected: 99437
Total number selected: 1000000

```

===using int-random===
int-random needn't the calculation of (1.0 /i).That is 3-times faster.I implemented the use of reading a random line of a textfile as discribed.In that case, there is no need to use the functoin one_of_n .

```pascal

Program OneOfNLines(InPut,Output);
{$h+} //use Ansistring
{type
  NativeInt = LongInt;}

function one_of_n(n: NativeInt): NativeInt;
  var
    ch,i: LongInt;// doubles speed of random using 64-Bit :-(
  begin
    ch := 1;
    for i := 2 to n do
      if random(i) = 0 then
        ch := i;
    one_of_n := ch;
  end;

function ChooseRNDLine(          fileNm : string;
                       var LnChsn,LnCnt :NativeInt):string;
var
  choosen,
  n : NativeInt;
  f : textFile;
  actRow,
  chsnRow: string;
  buf : array[0..4095] of char;// speed things up
begin
  n:= 0;
  choosen := n;
  chsnRow := '';

  Assign(f,fileNm);
  {$I-}
  Reset(f);
  {$I+}
  IF IoResult <> 0 then
     close(f)
  else
  Begin
    SetTextBuf(f,Buf[0]);

    while Not(EOF(f)) do
    Begin
      readln(f,actRow);
      inc(n);
      IF Random(n)= 0 then
      begin
        chsnRow:= actRow;
        choosen := n;
      end;
    end;
    close(f);
  end;

  LnChsn := choosen;
  LnCnt := n;
  ChooseRNDLine := chsnRow;
end;

const
  cFn = 'OneOfNLines.s';// compiled with -al assembler output
  num_reps = 1000000;
  num_lines_in_file = 10;

var
  Ln    : String;
  LnChsn,
  LnCnt,
  i     : NativeInt;
  cntLns: array[1..num_lines_in_file] of NativeUint;
begin
  randomize;
  Ln := ChooseRNDLine(cFn,LnChsn,LnCnt);
  writeln('choosen ', LnChsn,' out of ',LnCnt );
  writeln(Ln);writeln;

  FillChar(cntLns,SizeOf(cntLns),#0);
  for i := 1 to num_reps do
    inc(cntLns[one_of_n(num_lines_in_file)]);
  for i := 1 to num_lines_in_file do
    writeln('Number of times line ', i, ' was selected: ', cntLns[i]);

  LnCnt := 0;
  For i := Low(cntLns) to High(cntLns) do
    inc(LnCnt,cntLns[i]);
  writeln('Total number selected: ', LnCnt);
end.
```

;Output:

```txt

time ./OneOfNLines
choosen 463 out of 667
.section .data.n_INITFINAL

rest like above:
Number of times line 1 was selected: 99891
....
Number of times line 10 was selected: 99523
Total number selected: 1000000

real  0m0.086s
```



## Perl



```perl
#!/usr/bin/perl
use warnings;
use strict;

sub one_of_n {
    my $n = shift;
    my $return = 1;
    for my $line (2 .. $n) {
        $return = $line if 1 > rand $line;
    }
    return $return;
}

my $repeat = 1_000_000;
my $size   = 10;

my @freq;
++$freq[ one_of_n($size) - 1 ] for 1 .. $repeat;
print "@freq\n";
```



## Perl 6

```perl6
sub one_of_n($n) {
    my $choice;
    $choice = $_ if .rand < 1 for 1 .. $n;
    $choice - 1;
}

sub one_of_n_test($n = 10, $trials = 1_000_000) {
    my @bins;
    @bins[one_of_n($n)]++ for ^$trials;
    @bins;
}

say one_of_n_test();
```

Output:

```txt
100288 100047 99660 99773 100256 99633 100161 100483 99789 99910
```



## Phix


```Phix
function one_of_n(integer n)
integer line_num = 1
    for i=2 to n do
        if rnd()<1/i then
            line_num = i
        end if
    end for
    return line_num
end function

sequence counts = repeat(0,10)
    for i=1 to 1000000 do
        counts[one_of_n(10)] += 1
    end for
    ?counts
```

```txt

{99998,100223,99972,100323,100174,99663,99593,100141,99866,100047}

```



## PicoLisp


```PicoLisp
(de one-of-n (N)
   (let R 1
      (for I N
         (when (= 1 (rand 1 I))
            (setq R I) ) )
      R ) )

(let L (need 10 0)
   (do 1000000
      (inc (nth L (one-of-n 10))) )
   L )
```

Output:

```txt
-> (99893 100145 99532 100400 100263 100229 99732 100116 99709 99981)
```



## PowerShell

'''Translation''' of: '''C#'''

```PowerShell

function Get-OneOfN ([int]$Number)
{
    $current = 1

    for ($i = 2; $i -le $Number; $i++)
    {
        $limit = 1 / $i

        if ((Get-Random -Minimum 0.0 -Maximum 1.0) -lt $limit)
        {
            $current = $i
        }
    }

    $current
}


$table = [ordered]@{}

for ($i = 1; $i -lt 11; $i++)
{
    $table.Add(("Line {0,2}" -f $i), 0)
}

for ($i = 0; $i -lt 1000000; $i++)
{
    $index = (Get-OneOfN -Number 10) - 1
    $table[$index] = $table[$index] + 1
}

[PSCustomObject]$table

```

```txt

Line  1 : 99928
Line  2 : 100067
Line  3 : 100415
Line  4 : 100133
Line  5 : 100555
Line  6 : 99845
Line  7 : 99625
Line  8 : 99968
Line  9 : 99864
Line 10 : 99600

```

The above version runs in ~650 seconds, because of the large overhead of calling PowerShell functions and binding their parameters. With a small change to move the function into a class method, the parameter binding becomes faster, and swapping Get-Random for System.Random, the overall code runtime drops to ~20 seconds. Changing the ordered hashtable to a Generic Dictionary reduces it again to ~15 seconds:

```powershell
class Holder {
    [System.Random]$rng

    Holder()
    {
        $this.rng = [System.Random]::new()
    }

    [int] GetOneOfN([int]$Number)
    {
        $current = 1

        for ($i = 2; $i -le $Number; $i++)
        {
            $limit = 1 / $i

            if ($this.rng.NextDouble() -lt $limit)
            {
                $current = $i
            }
        }

        return $current
    }
}


$table = [Collections.Generic.Dictionary[int, int]]::new()
$X = [Holder]::new()

1..10 | ForEach-Object {
    $table.Add($_, 0)
}

for ($i = 0; $i -lt 1e6; $i++)
{
    $index = $X.GetOneOfN(10) - 1
    $table[$index] += 1
}

[PSCustomObject]$table
```



## PureBasic


```purebasic
Procedure.f randomFloat()
   ProcedureReturn Random(1000000) / 1000000
EndProcedure

Procedure one_of_n(n)
  Protected linesRead, lineChosen
  While linesRead < n
    linesRead + 1
    If randomFloat() <= (1.0 / (linesRead))
       lineChosen = linesRead
    EndIf
  Wend
  ProcedureReturn lineChosen
EndProcedure

If OpenConsole()
  #testFileLineCount = 10
  #simulationCount = 1000000
  Define i
  Dim a(#testFileLineCount) ;index 0 is not used
  For i = 1 To #simulationCount
    x = one_of_n(#testFileLineCount)
    a(x) + 1
  Next

  For i = 1 To #testFileLineCount
    Print(Str(a(i)) + "  ")
  Next
  PrintN("")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
99959  100011  100682  100060  99834  99632  100083  99817  99824  100098
```



## Python

To be more in line with the spirit of the problem, <code>one_of_n</code> will take the "lines" as an iterator, guaranteeing that it only traverses the lines one time, and does not know the length until the end.

```python
from random import randrange
try:
    range = xrange
except: pass

def one_of_n(lines): # lines is any iterable
    choice = None
    for i, line in enumerate(lines):
        if randrange(i+1) == 0:
            choice = line
    return choice

def one_of_n_test(n=10, trials=1000000):
    bins = [0] * n
    if n:
        for i in range(trials):
            bins[one_of_n(range(n))] += 1
    return bins

print(one_of_n_test())
```


;Sample output:

```txt
[99833, 100303, 99902, 100132, 99608, 100117, 99531, 100017, 99795, 100762]
```



## R


```rsplus
one_of_n <- function(n)
{
  choice <- 1L

  for (i in 2:n)
  {
    if (i*runif(1) < 1)
      choice <- i
  }

  return(choice)
}

table(sapply(1:1000000, function(i) one_of_n(10)))
```


Sample output:

```txt
     1      2      3      4      5      6      7      8      9     10
 99783  99776 100214 100342 100342  99771 100394 100176  99486  99716
```



## Racket


```Racket

#lang racket

(define (one-of-n n)
  (for/fold ([n 0]) ([i (in-range 1 n)])
    (if (zero? (random (add1 i))) i n)))

(define (try n times)
  (define rs (make-vector n 0))
  (for ([i (in-range times)])
    (define r (one-of-n n))
    (vector-set! rs r (add1 (vector-ref rs r))))
  (vector->list rs))

(define TIMES 1000000)
(for ([n (in-range 1 21)])
  (define rs (try n TIMES))
  (printf "~a: ~a\n    ~a\n" (~a #:width 2 n) rs
          (map (lambda (r) (~a (round (/ r TIMES 1/100)) "%")) rs)))

#| Sample Run:

1 : (1000000)
    (100%)
2 : (499702 500298)
    (50% 50%)
3 : (332426 333314 334260)
    (33% 33% 33%)
4 : (249925 250083 249695 250297)
    (25% 25% 25% 25%)
5 : (200304 199798 199920 199983 199995)
    (20% 20% 20% 20% 20%)
6 : (166276 167085 165955 166792 167143 166749)
    (17% 17% 17% 17% 17% 17%)
7 : (142067 143242 142749 142997 143248 142746 142951)
    (14% 14% 14% 14% 14% 14% 14%)
8 : (125026 125187 125214 124770 124785 125141 125039 124838)
    (13% 13% 13% 12% 12% 13% 13% 12%)
9 : (111551 111013 110741 111292 111105 110627 110570 111685 111416)
    (11% 11% 11% 11% 11% 11% 11% 11% 11%)
10: (100322 100031 100176 100590 99799 99892 100305 99955 99493 99437)
    (10% 10% 10% 10% 10% 10% 10% 10% 10% 10%)
11: (91237 90706 90962 90901 90872 91002 91164 90967 90092 90706 91391)
    (9% 9% 9% 9% 9% 9% 9% 9% 9% 9% 9%)
12: (83046 83556 83003 84128 83264 83305 83093 83202 83430 83605 83276 83092)
    (8% 8% 8% 8% 8% 8% 8% 8% 8% 8% 8% 8%)
13: (77282 76936 76667 76659 76771 76736 77165 77190 77341 76469 76985 76942 76857)
    (8% 8% 8% 8% 8% 8% 8% 8% 8% 8% 8% 8% 8%)
14: (71389 71496 71141 71314 71670 72062 71979 71361 71198 71457 70854 71686 71300 71093)
    (7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7%)
15: (66534 66571 66072 66977 66803 66894 67076 66409 66306 67222 66590 66780 66341 66680 66745)
    (7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7% 7%)
16: (62155 62496 62846 62136 62447 62714 62228 62454 62527 62577 62775 62692 62491 62231 62460 62771)
    (6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6%)
17: (58852 59046 58726 58782 58979 58725 59051 58935 58910 59082 58567 58863 58625 58922 58648 58456 58831)
    (6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6%)
18: (55204 55683 55547 55492 55671 55467 55801 55704 55235 55411 55482 55387 55679 55557 55398 55649 55815 55818)
    (6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6% 6%)
19: (52564 52283 52918 52363 52316 52511 52500 53042 52594 52720 52577 52623 52762 53047 52798 52832 52267 52550 52733)
    (5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5%)
20: (50107 50008 49786 50128 50431 49905 50109 49781 50099 50117 49772 50128 49721 49937 49735 50067 49865 50155 50231 49918)
    (5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5% 5%)

|#

```



## REXX


```rexx
/*REXX program simulates reading a  ten─line  file,  count the selection randomness.    */
N= 10                                            /*the number of lines in pseudo-file.  */
@.= 0                                            /*zero  all  the (ten)  "buckets".     */
      do 1000000                                 /*perform main loop  one million times.*/
      ?= 1
                    do k=1  for N                /*N  is the number of lines in the file*/
                    if random(0, 99999) / 100000  <  1/k  then ?= k      /*the criteria.*/
                    end   /*k*/
      @.?= @.? + 1                               /*bump the count in a particular bucket*/
      end                 /*1000000*/

   do j=1  for N                                 /*display randomness counts (buckets). */
   say "number of times line"       right(j, 2)       "was selected:"       right(@.j, 9)
   end   /*j*/                                   /*stick a fork in it,  we're all done. */
```

```txt

number of times line  1 was selected:     99752
number of times line  2 was selected:    100060
number of times line  3 was selected:     99996
number of times line  4 was selected:    100023
number of times line  5 was selected:    100028
number of times line  6 was selected:    100262
number of times line  7 was selected:    100755
number of times line  8 was selected:     99794
number of times line  9 was selected:     99539
number of times line 10 was selected:     99791

```



## Ring


```ring

cnt = list(10)
for nr = 1 to 10000
    cnt[oneofn(10)] += 1
next
for m = 1 to 10
    see "" + m + " : " + cnt[m] + nl
next
see nl

func oneofn n
for i = 1 to n
    if random(1) <= 1/i d = i ok
next
return d

```


```txt

1 : 15
2 : 12
3 : 37
4 : 74
5 : 158
6 : 323
7 : 646
8 : 1233
9 : 2506
10 : 4996

```



## Ruby


```ruby
# Returns a random line from _io_, or nil if _io_ has no lines.
#  # Get a random line from /etc/passwd
#  line = open("/etc/passwd") {|f| random_line(f) }
def random_line(io)
  choice = io.gets; count = 1
  while line = io.gets
    rand(count += 1).zero? and choice = line
  end
  choice
end

def one_of_n(n)
  # Create a mock IO that provides line numbers instead of lines.
  # Assumes that #random_line calls #gets.
  (mock_io = Object.new).instance_eval do
    @count = 0
    @last = n
    def self.gets
      (@count < @last) ? (@count += 1) : nil
    end
  end
  random_line(mock_io)
end

chosen = Hash.new(0)
1_000_000.times { chosen[one_of_n(10)] += 1 }
chosen.keys.sort.each do |key|
  puts "#{key} chosen #{chosen[key]} times"
end
```



```txt
$ ruby one-of-n.rb
1 chosen 100470 times
2 chosen 100172 times
3 chosen 100473 times
4 chosen 99725 times
5 chosen 100600 times
6 chosen 99126 times
7 chosen 100297 times
8 chosen 99606 times
9 chosen 100039 times
10 chosen 99492 times
```



## Run BASIC


```runbasic
for i1 = 1 to 1000000
    c = oneOfN(10)
    chosen(c) = chosen(c) + 1
next

for i1 = 1 to 10
    print i1;" ";chosen(i1)
next

FUNCTION oneOfN(n)
    for i2 = 1 to n
        IF int(rnd(1) * i2) = 0 then choice = i2
    next
    oneOfN = choice
END FUNCTION
```
Output:

```txt
1 99034
2 98462
3 98741
4 100256
5 100449
6 100758
7 100206
8 100982
9 100520
10 100592
```



## Rust

You could also use `rand::seq::sample_iter` which uses a more general version of this problem, Reservoir Sampling: https://en.wikipedia.org/wiki/Reservoir_sampling.

```rust
extern crate rand;

use rand::{Rng, thread_rng};

fn one_of_n<R: Rng>(rng: &mut R, n: usize) -> usize {
    (1..n).fold(0, |keep, cand| {
        // Note that this will break if n is larger than u32::MAX
        if rng.gen_weighted_bool(cand as u32 + 1) {
            cand
        } else {
            keep
        }
    })
}

fn main() {
    const LINES: usize = 10;

    let mut dist = [0; LINES];
    let mut rng = thread_rng();

    for _ in 0..1_000_000 {
        let num = one_of_n(&mut rng, LINES);
        dist[num] += 1;
    }

    println!("{:?}", dist);
}

```

```txt

[100203, 100012, 99854, 99686, 99888, 99899, 99559, 100584, 100208, 100107]

```



## Scala


```Scala
def one_of_n(n: Int, i: Int = 1, j: Int = 1): Int =
  if (n < 1) i else one_of_n(n - 1, if (scala.util.Random.nextInt(j) == 0) n else i, j + 1)

def simulate(lines: Int, iterations: Int) = {
  val counts = new Array[Int](lines)
  for (_ <- 1 to iterations; i = one_of_n(lines) - 1) counts(i) = counts(i) + 1
  counts
}

println(simulate(10, 1000000) mkString "\n")
```

```txt
100014
100233
100146
100121
99796
99677
99948
99260
100299
100506
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: one_of_n (in integer: n) is func
  result
    var integer: r is 1;
  local
    var integer: i is 0;
  begin
    for i range 2 to n do
      if rand(1, i) = 1 then
        r := i;
      end if;
    end for;
  end func;

const proc: main is func
  local
    var array integer: r is 10 times 0;
    var integer: i is 0;
  begin
    for i range 1 to 1000000 do
      incr(r[one_of_n(10)]);
    end for;
    for i range 1 to 10 do
      write(r[i] <& " ");
    end for;
    writeln;
  end func;
```


Output:

```txt

100372 99661 100264 99644 100180 99748 99718 100205 99714 100494

```



## Sidef

```ruby
func one_of_n(n) {
    var choice
    n.times { |i|
        choice = i if (1 > i.rand)
    }
    choice - 1
}

func one_of_n_test(n = 10, trials = 1_000_000) {
    var bins = []
    trials.times {
        bins[one_of_n(n)] := 0 ++
    }
    bins
}

say one_of_n_test()
```


```txt
99838 100843 99696 100078 99973 100350 100054 99495 99540 100133
```



## Swift


```swift
func one_of_n(n: Int) -> Int {
  var result = 1
  for i in 2...n {
    if arc4random_uniform(UInt32(i)) < 1 {
      result = i
    }
  }
  return result
}

var counts = [0,0,0,0,0,0,0,0,0,0]
for _ in 1..1_000_000 {
  counts[one_of_n(10)-1]++
}

println(counts)
```


```txt
[100475, 99986, 99725, 100069, 99702, 100065, 99840, 100501, 100186, 99450]
```



## Tcl


```tcl
package require Tcl 8.5
proc 1ofN {n} {
    for {set line 1} {$line <= $n} {incr line} {
	if {rand() < 1.0/[incr fraction]} {
	    set result $line
	}
    }
    return $result
}

for {set i 0} {$i < 1000000} {incr i} {
    incr count([1ofN 10])
}
parray count;   # Alphabetic order, but convenient
```

Sample output:

```txt

count(1)  = 99862
count(10) = 100517
count(2)  = 100545
count(3)  = 100339
count(4)  = 99636
count(5)  = 99920
count(6)  = 99263
count(7)  = 100283
count(8)  = 99871
count(9)  = 99764

```



## VBScript


```vb

Dim chosen(10)

For j = 1 To 1000000
	c = one_of_n(10)
	chosen(c) = chosen(c) + 1
Next

For k = 1 To 10
	WScript.StdOut.WriteLine k & ". " & chosen(k)
Next

Function one_of_n(n)
	Randomize
	For i = 1 To n
		If Rnd(1) < 1/i Then
			one_of_n = i
		End If
	Next
End Function

```


```txt

1. 100082
2. 100358
3. 100184
4. 99573
5. 100404
6. 99544
7. 99884
8. 99995
9. 100081
10. 99895

```



## zkl

```zkl
fcn one_of_n(lines){ # lines is any iterable
#if 0  // iterative
   choice:=Void;
   foreach i,line in ([0..].zip(lines)){
      if((0).random(i+1)==0) choice=line;
   }
   return(choice);
#else  // functional
   [0..].zip(lines).pump(Ref(Void).set,fcn([(n,line)])
      { if((0).random(n+1)==0) line else Void.Skip }).value
#endif
}

fcn one_of_n_test(n=10, trials=0d1_000_000){
   bins:=n.pump(List(),0); // List(0,0,0...)
   if(n){ foreach i in (trials){ bins[one_of_n((n).walker())]+=1 } }
   return(bins);
}

println(one_of_n_test());
```

A Ref is a strong reference to a value, Ref.set(value) sets the Ref, Ref.value gets the value. A pump pumps data through a list of functions into a sink, Void.Skip skips this value (ie same as continue in a loop).
```txt
L(99402,99786,99751,100299,100356,99579,100280,100073,100308,100166)
```

