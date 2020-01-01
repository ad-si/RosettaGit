+++
title = "Input/Output for Pairs of Numbers"
description = ""
date = 2019-08-01T05:00:05Z
aliases = []
[extra]
id = 16816
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
From lines of input starting with a line containing the numbers of pairs to follows, followed by that number of pairs of integers separated by a space on separate lines from STDIN, output the sum of each pair to STDOUT.


;Sample input with corresponding output:

'''Input'''

```txt
5
1 2
10 20
-3 5
100 2
5 5
```


'''Output'''

```txt
3
30
2
102
10
```



## ALGOL 68

Simple version - there can be newlines before or between the numbers

```algol68
# read a number from stand in then read and add that many pairs of numbers from stand in      #
# and write the sum to stand out. If non integer data is supplied, a runtime error will occur #
TO ( INT n; read( ( n, newline ) ); n ) DO
    INT a, b;
    read( ( a, b, newline ) );
    print( ( a + b, newline ) )
OD

```

Strict version - the pairs of numbers must appear on the same line.

```algol68

# read a number from stand in then read and add that many pairs of numbers from stand in      #
# and write the sum to stand out. If non integer data is supplied, a runtime error will occur #
# This version does not allow the pairs of numbers to be spread over several lines            #
STRING line;
FILE   numbers;
associate( numbers, line );
TO ( INT n
   ; read( ( line, newline ) )
   ; reset( numbers )
   ; get( numbers, ( n ) )
   ; n
   )
DO
    INT a, b;
    read( ( line, newline ) );
    reset( numbers );
    get( numbers, ( a, b ) );
    print( ( a + b, newline ) )
OD
```

{{out}}

```txt

         +3
        +30
         +2
       +102
        +10

```



## AWK


```awk
NR == 1 {n=$1; next}
NR > n+1 {exit}
{print $1+$2}
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

set /p pairs=

for /l %%i in (1,1,%pairs%) do set /p pair%%i=
for /l %%i in (1,1,%pairs%) do (
  for %%j in (!pair%%i!) do (
    set /a sum%%i+=%%j
  )
)

for /l %%i in (1,1,%pairs%) do echo !sum%%i!
pause>nul

```

{{in}}

```txt

5
10 10
5 6
-3 2
-6 -8
111 2

```

{{out}}

```txt

20
11
-1
-14
113

```



## BBC BASIC

The specification is a bit ambiguous, but I understood it as wanting us to read all the numbers in <i>first</i> and then print all the sums. This program does that. It could be a couple of lines shorter if we were allowed to use a comma instead of a space as separator.

```bbcbasic
INPUT n%
DIM pairs%(n% - 1, 1)
FOR i% = 0 TO n% - 1
  INPUT s$
  pairs%(i%, 0) = VAL(LEFT$(s$, INSTR(s$, " ")))
  pairs%(i%, 1) = VAL(MID$(s$, INSTR(s$, " ")))
NEXT
FOR i% = 0 TO n% - 1
  PRINT pairs%(i%, 0) + pairs%(i%, 1)
NEXT
```

With the sample inputs:

```txt
?5
?1 2
?10 20
?-3 5
?100 2
?5 5
         3
        30
         2
       102
        10
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	int i, n, a, b, *f;
	scanf("%d", &n);
	f = malloc(sizeof(*f) * n);

	for (i = 0; i < n; i++) {
		if (2 != scanf("%d %d", &a, &b))
			abort();
		f[i] = a + b;
	}

	for (i = 0; i < n; i++)
		printf("%d\n", f[i]);

	return 0;
}
```


Output for example input


```txt

3
30
2
102
10

```



## C++

Modified in order to take in all inputs and then give the output, the original gave the output for each pair immediately.

```cpp

#include <iostream>
using namespace std;

int doStuff(int a, int b) {
    return a + b;
}

int main() {

	int t, **list;

	cin >> t;

	list = new int*[t];

	for(int j=0; j<t; j++){

		list[j] = new int[2];
		cin >> list[j][0]>> list[j][1];

	}

	cout << endl;

	for(int j=0;j<t;j++){
		cout << doStuff(list[j][0], list[j][1]) << endl;;
	}
	return 0;
}

```


Run as per given input


```txt

5
1 2
10 20
-3 5
100 2
5 5

3
30
2
102
10

```



## C#


```c#
using System;
using static System.Linq.Enumerable;

public class Program
{
    static void Main(string[] args)
    {
	int count = Convert.ToInt32(Console.ReadLine());
	for (int line = 0; line < count; line++) {
            Console.WriteLine(Console.ReadLine().Split(' ').Sum(i => Convert.ToInt32(i)));
	}
    }
}
```

{{out}}

```txt

3
30
2
102
10

```



## D

This works with any number of integers on lines.

```d
void main() {
    import std.stdio, std.string, std.conv, std.algorithm;

    foreach (immutable _; 0 .. readln.strip.to!uint)
        readln.split.to!(int[]).sum.writeln;
}
```



## Factor


```factor

USING: io math.parser prettyprint sequences splitting ;
IN: rosetta-code.pair-output

: process-line ( str -- n )
    " " split [ string>number ] map-sum ;
: main ( -- ) lines 1 tail [ process-line ] map [ . ] each ;

MAIN: main

```

{{out}}

```txt

3
30
2
102
10

```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program i_o_pairs
  implicit none

  integer :: npairs
  integer :: i
  integer, allocatable :: pairs(:,:)

  read(*,*) npairs
  allocate(pairs(npairs,2))

  do i = 1, npairs
    read(*,*) pairs(i,:)
  end do
  write(*, "(i0)") sum(pairs, 2)

end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As UInteger n
Dim As Integer x, y
Input "", n
Dim sums(1 To n) As Integer
For i As Integer = 1 To  n
  Input "", x, y
  sums(i) =  x + y
Next
Print
For i As Integer = 1 To n
  Print Str(sums(i))
Next
Sleep
```


{{out}}

```txt

5
1 2
10 20
-3 5
100 2
5 5

3
30
2
102
10

```



## Haskell

This solution will actually add any number of integers placed on each line. Additionally, after removing the bits of code that cut out the specified number of lines, the solution will sum any number of lines of integers.


```Haskell
main = do
    contents <- getContents
    let numberOfLines  =  read.head.lines$ contents
        nums  =  map (map read.words).take numberOfLines.tail.lines$ contents
        sums  =  map sum nums
    mapM_ print sums
```



## Go


```go
package main

import (
	"fmt"
	"log"
)

func main() {
	var lines int
	n, err := fmt.Scanln(&lines)
	if n != 1 || err != nil {
		log.Fatal(err)
	}

	var a, b int
	for ; lines > 0; lines-- {
		n, err = fmt.Scanln(&a, &b)
		if n != 2 || err != nil {
			log.Fatal(err)
		}
		fmt.Println(a + b)
	}
}
```



## J


```J

$ cat <<EOF | jconsole -js '([: exit 0: [: smoutput [: ,. [: ({. {. }.) [: (+/"1) [: (0&".;._2) (1!:1)) 3'
> 5
> 1 2
> 10 20
> -3 5
> 100 2
> 5 5
> EOF
  3
 30
  2
102
 10

```

Considerably simpler than [[http://rosettacode.org/wiki/Input/Output_for_Lines_of_Text#J|see explanation]] output for lines of text, this sentence is a single fork.  J pads the numeric arrays of 0&".;._2 (numbers cut) with 0 .  We form the +/"1 (row sums), then take the sum of the first row of the beheaded sums ({. {. }.) for display.  ,. (raveled items) reshapes the vector into a column-vector shaped matrix.  And the [: (cap) causes the monadic form of the verb to cap's right.


## Java


```java
import java.util.Scanner;

public class Main {

	public static int doStuff(int a, int b){
	    int sum = a+b;
	    return sum;
	}

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);

		int n = in.nextInt();
		for(int i=0; i<n; i++){
			int a = in.nextInt();
			int b= in.nextInt();
			int result = doStuff(a, b);
			System.out.println(result);
		}
	}
}
```




## Julia


```julia
parseints() = (a = split(strip(readline()), r"\s+"); map(x -> parse(Int, x), a))

const lines = parseints()[1]

for _ in 1:lines
    println(sum(parseints()))
end

```
{{out}}

```txt

3
5 6
11
8 2
10
9 23
32

```



## Kotlin


```scala
// version 1.0.6

import java.util.Scanner

fun main(args: Array<String>) {
    val sc = Scanner(System.`in`)  // note: backticks required as 'in' is a Kotlin keyword
    val n = sc.nextInt()
    val x = IntArray(n)
    val y = IntArray(n)
    for (i in 0 until n) {
        x[i] = sc.nextInt()
        y[i] = sc.nextInt()
    }
    println()
    for (i in 0 until n) println(x[i] + y[i])
}
```

Sample input/output:
{{out}}

```txt

5
1 2
10 20
-3 5
100 2
5 5

3
30
2
102
10

```



## Lua

This solution will sum any number of space-separated numbers per input line, assuming the user won't input too many to store in the available RAM.

```Lua
local intTab, numLines, sum = {}, io.read()
for i = 1, numLines do
    sum = 0
    for number in io.read():gmatch("%S+") do sum = sum + number end
    table.insert(intTab, sum)
end
for _, result in pairs(intTab) do print(result) end
```




## OCaml



```ocaml
let () =
  let n = int_of_string (input_line stdin) in
  for i = 1 to n do
    let line = input_line stdin in
    match String.split_on_char ' ' line with
    | a::b::[] ->
        let x = int_of_string a + int_of_string b in
        print_int x;
        print_newline ()
    | _ ->
        raise (Invalid_argument "wrong input")
  done
```


{{out}}

```txt

$ cat input.txt
5
1 2
10 20
-3 5
100 2
5 5
$ cat input.txt | ocaml pairs.ml
3
30
2
102
10

```




## PARI/GP


Interestingly, this task is not possible to implement directly in GP, since <code>input()</code>, like the gp REPL itself, ignores spaces. One must use PARI:

```c
#include <stdio.h>
#include <stdlib.h>
#include <pari/pari.h>

int main(void);

int
main()
{
  int i, n, a, b;
  GEN f, sum;
  pari_sp ltop;

  // 1 MB stack, not using prime table
  pari_init(1000000, 0);

  scanf("%d", &n);
  GEN f = cgetg(n+1, t_VEC);

  for (i = 1; i <= n; i++) {
    if (2 != scanf("%d %d", &a, &b)) abort();

    ltop = avma;

	// Add a and b in PARI
	sum = addii(stoi(a), stoi(b));

	// Store the sum in a vector, collecting garbage as you go.
	gel(f, i) = gerepileupto(ltop, sum);
  }

  pari_printf("%Ps", f);
  return 0;
}
```

Of course for such a simple task this has very little advantage over C, but it does demonstrate the general principle.


## Perl

Reads from STDIN, added any pair of numbers.

```perl
$n = scalar <>;

for (1..$n) {
    ($a,$b) = split ' ', <>;
    print $a + $b . "\n";
}
```



## Perl 6


```perl6
for ^get() { say [+] get.words }
```

This does more than the task asks.  It will sum as many numbers as you care to put on each line, and the numbers need not be integers, but may also be a mix of rational, floating-point, or complex numbers.  More subtly, <tt>get</tt> can read from a file specified as a command-line argument, but defaults to taking STDIN if no filename is specified.


## Phix


```Phix
string line = gets(0)
sequence r = scanf(trim(line),"%d"), s = {}
if length(r)!=1 then
    puts(1,"input not a number\n")
    abort(0)
end if
puts(1,"\n")
for i=1 to r[1][1] do
    line = gets(0)
    r = scanf(trim(line),"%d %d")
    if length(r)!=1 then
        puts(1,"input not a pair of numbers\n")
        abort(0)
    end if
    s &= sum(r[1])
    puts(1,"\n")
end for
puts(1,"===\n")
?s
```

{{out}}
(or more accurately the final state of the console)

```txt

5
1 2
10 20
-3 5
100 2
5 5
===
{3,30,2,102,10}

```



## PowerShell


```PowerShell

# script.ps1

$in, $line = (Get-Content $args[0]), 0
$nb = $in[$line++]
1..$nb | foreach {
    $sum = 0
    $in[$line++].Split() | foreach{ $sum += $_}
    $sum
}

# ./script file.txt

```



## Python


```python
def do_stuff(a, b):
	return a + b

t = input()
for x in range(0, t):
	a, b = raw_input().strip().split()
	print do_stuff(int(a), int(b))
```



### Python: Alternative

Or without the function do_stuff() and that works for Python 3 and Python 2:

```python>>>
 try: raw_input
except NameError: raw_input = input

>>> for i in range(int(raw_input())):
	print(sum(int(numberstring)
		  for numberstring
		  in raw_input().strip().split()))


5
1 2
3
10 20
30
-3 5
2
100 2
102
5 5
10
>>>
```

(All but the first line of single numbers, (the 5), is output from the program).


### Python: With prompts

More than is asked for by the task, but if working interactively then the following version adds prompts.

```python>>>
 for i in range(int(raw_input('lines: '))):
	print(sum(int(numberstring)
                  for numberstring in raw_input('two numbers: ').strip().split()))


lines: 5
two numbers: 1 2
3
two numbers: 10 20
30
two numbers: -3 5
2
two numbers: 100 2
102
two numbers: 5 5
10
>>>
```



## Racket


```Racket
#lang racket
;(define line-number (read)) ;reads all kind of things
;(for ([i (in-range line-number)])
;  (displayln (+ (read) (read))))

(define line-count (string->number ;only reads numbers
                    (string-trim (read-line))))

(for ([i (in-range line-count)])
  (displayln (apply +
                    (map string->number
                         (string-split (read-line))))))
```



## REXX

This version isn't limited to summing integers, any form of number that REXX supports can be used.

```rexx
/*REXX pgm reads a number (from the CL), reads that number of pairs, & writes their sum.*/
                                                 /*all input is from the  Command Line. */
     do  linein()                                /*read the number of pairs to be add*ed*/
     $=linein()                                  /*read a line (a record) from the C.L. */
     say word($, 1)   +   word($, 2)             /*display the sum of a pair of numbers.*/
     end   /*linein() */
                                                 /*stick a fork in it,  we're all done. */
```



## Ring


```ring

# Project : Input/Output for Pairs of Numbers

pairs = ["5", "1 2", "10 20", "-3 5", "100 2", "5 5"]
for n = 1 to len(pairs)
    nr = 0
    for p = 1 to len(pairs[n])
        if substr(pairs[n], p, 1) = " "
           nr = p
        ok
    next
    if nr > 0
       n1 = number(left(pairs[n], nr - 1))
       n2 = number(right(pairs[n], len(pairs[n]) - nr + 1))
       n3 = n1 + n2
       see n3 + nl
    ok
next

```

Output:

```txt

3
30
2
102
10

```



### Ring: Alternative


```ring

# Project : Input/Output for Pairs of Numbers (Alternative)

pairs = ["5", "1 2", "10 20", "5 -3", "100 2", "5 5"]
for n = 1 to len(pairs)
    nr = 0
    for p = 1 to len(pairs[n])
        if substr(pairs[n], p, 1) = " "
           pairs[n] = substr(pairs[n], " ", "+")
           nr = p
        ok
    next
    if nr > 0
       eval("ev = " + pairs[n])
       see ev + nl
    ok
next
>>>
```


```txt

3
30
2
102
10

```



## Ruby


```ruby
n = gets.to_i
n.times do
  a, b = gets.split.map(&:to_i)
  puts a + b
end
```



## Scala


```Scala
object IOPairs extends App {
  private val in = scala.io.StdIn
  private val n = in.readInt()

  for (_ <- 0 until n) {
    val Array(a, b) = in.readLine().split(" ").map(_.toInt)

    def doStuff(a: Long, b: Long): Long = a + b

    println(doStuff(a, b))
  }

}

```


## Tcl


```tcl
gets stdin n
while {$n > 0} {
    if {[scan [gets stdin] "%d %d" a b] == 2} {
        puts [expr {$a + $b}]
    }
    incr n -1
}
```



## UNIX Shell


{{works with|Bourne Again SHell}}


```bash
read n
while (( n > 0 )); do
    read a b
    echo $((a+b))
    ((n--))
done
```



## Ursa


```ursa
decl int amount
set amount (in int console)

decl int<> ints
for (decl int i) (< i amount) (inc i)
        decl string input
        set input (in string console)
        append (int (split input " ")<0>) (int (split input " ")<1>) ints
end for

out endl console

for (set i 0) (< i (size ints)) (set i (int (+ 2 i)))
        out (int (+ ints<i> ints<(int (+ i 1))>)) endl console
end for
```

Networked version. Runs on port 20000.

```ursa
decl serverport sp
decl port p
sp.attach 20000
set p (sp.getconn)

decl int amount
set amount (in int p)

decl int<> ints
for (decl int i) (< i amount) (inc i)
        decl string input
        set input (in string p)
        append (int (split input " ")<0>) (int (split input " ")<1>) ints
end for

out endl p

for (set i 0) (< i (size ints)) (set i (int (+ 2 i)))
        out (int (+ ints<i> ints<(int (+ i 1))>)) endl p
end for
```



## zkl

Using the console as the input stream:

```zkl
fcn pairs{
   n:=ask("num pairs: ").toInt();
   do(n){ask("1 pair: ").split(" ").sum().println()}
}
```

{{out}}

```txt

pairs()
num pairs: 5
1 pair: 1 2
3
1 pair: 10 20
30
1 pair: -3 5
2
1 pair: 100 2
102
1 pair: 5 5
10

```

