+++
title = "Input/Output for Lines of Text"
description = ""
date = 2019-09-03T15:04:02Z
aliases = []
[extra]
id = 17065
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

## Task
The first line contains the number of lines to follow, followed by that number of lines of text on   STDIN.

Write to   STDOUT   each line of input by passing it to a method as an intermediate step. The code should demonstrate these 3 things.


## Sample input with corresponding output

Input

```txt

3
hello
hello world
Pack my Box with 5 dozen liquor jugs

```


Output

```txt

hello
hello world
Pack my Box with 5 dozen liquor jugs

```



## Related tasks
*   [[Input/Output for Pairs of Numbers]]
*   [[File/Input and Output]]





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# outputs line plus a newline                                #
PROC show line = ( STRING line )VOID:
    print( ( line, newline ) );

# copy the lines with an loop with an anonymous loop counter #
# as the loop limit is evaluated only once, we can read the  #
# number of lines in the "TO" part                           #
TO ( INT n; read( ( n, newline ) ); n )
DO
    show line( ( STRING line; read( ( line, newline ) ); line ) )
OD

```



## ALGOL W


```algolw
begin
    % outputs line on a newline %
    procedure showLine ( string(80) value line ); write( line );

    string(80) line;
    integer lineCount;
    read( lineCount );
    for lineNumber := 1 until lineCount do begin
        read( line );
        showLine( line )
    end for_lineNumber
end.
```



## AWK


```AWK

# syntax: GAWK -f INPUT_OUTPUT_FOR_LINES_OF_TEXT.AWK
BEGIN {
    getline n
    while (i++ < n) {
      getline
      str = sprintf("%s%s\n",str,$0)
    }
    printf("%s",str)
    exit(0)
}

```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

set /p lines=

for /l %%i in (1,1,%lines%) do set /p line%%i=
cls
for /l %%i in (1,1,%lines%) do echo !line%%i!
pause>nul

```

{{in}}

```txt

3
line 1
this is line 2
line 3 is the longest

```

{{out}}

```txt

line 1
this is line 2
line 3 is the longest

```



## C


```C

#include<stdlib.h>
#include<stdio.h>

#define LEN 100 /* Max string length */

int main()
{
	char **list;
	int num, i;

	scanf("%d",&num);

	list = (char**)malloc(num*sizeof(char*));

	for(i=0;i<num;i++)
	{
	   list[i] = (char*)malloc(LEN*sizeof(char));
	   fflush(stdin);
	   fgets(list[i],LEN,stdin);
	}

	printf("\n");

	for(i=0;i<num;i++)
	{
		printf("%s",list[i]);
	}

	return 0;
}

```



## D


```d
void main() {
    import std.stdio, std.conv, std.string;

    enum doStuff = (in string line) => line.write;

    foreach (_; 0 .. readln.strip.to!uint)
        doStuff(readln.idup);
}
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub printLines(lines() As String)
  For i As Integer = LBound(lines) To UBound(lines)
    Print lines(i)
  Next
End Sub

Dim As UInteger n
Input "", n
Dim lines(1 To n) As String
For i As Integer = 1 To  n
  Line Input lines(i)
Next
Print
printLines lines()
Sleep
```


{{out}}

```txt

3
hello
hello world
Pack my Box with 5 dozen liquor jugs

hello
hello world
Pack my Box with 5 dozen liquor jugs

```



## Free Pascal

This requires FPC – the FreePascal compiler – to be in a configuration enabling the use ob <tt>object</tt>s.

```pascal
program head(input, output, stdErr);

type
	obj = object
			public
				procedure method(const s: string); static;
		end;

procedure obj.method(const s: string);
begin
	writeLn(s);
end;

var
	numberOfLines: integer;
	line: string;
begin
	readLn(numberOfLines);

	for numberOfLines := numberOfLines downto 1 do
	begin
		readLn(line);
		obj.method(line);
	end;
end.
```



## Go


```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	// Often we'd already have wrapped os.Stdin (or some other
	// io.Reader, like an *os.File) in a bufio.Reader by this point
	// and we'd use fmt.Fscanln() on that reader instead.
	var lines int
	n, err := fmt.Scanln(&lines)
	if n != 1 || err != nil {
		log.Fatal(err)
	}

	// Use a bufio.Scanner. This uses a SplitFunc which we can choose
	// or provide our own that splits or otherwise pre-processes the
	// input into tokens however we like.
	//
	// Could also just use bufio.ReadString('\n') but a Scanner
	// with ScanLines matches (and removes) `\r?\n$` and is more
	// general purpose.
	//
	// Normally the loop would be just:
	//	for scanner.Scan() {
	//		// use scanner.Text() or scanner.Bytes()
	//	}
	// and we'd loop until the scan indicated EOF. But for this task
	// we've got an explictly specified number of lines to read.

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanLines) // not needed, this is the default
	for ; scanner.Scan() && lines > 0; lines-- {
		doStuff(scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	// Check for too few lines, normally not needed
	if lines > 0 {
		log.Fatalln("early", io.EOF)
	}
}

func doStuff(line string) {
	fmt.Println(line)
}
```



## Haskell


```Haskell

doStuff = putStrLn
main = getContents >>= mapM_ doStuff.tail.lines

```



## J

[[http://rosettacode.org/wiki/Input/Output_for_Pairs_of_Numbers#J|io for number pairs links to this page.]]

Example in bash.  jconsole is on the PATH.

```J

$ cat <<EOF | jconsole -js '2!:55@:0:@:(; (1!:2) 4:)@:(}. {.~ _ ". [: }: 0&{::)@:(<;.2)@:(1!:1) 3'
> 3
> hello
> hello world
> Pack my Box with 5 dozen liquor jugs
> EOF
hello
hello world
Pack my Box with 5 dozen liquor jugs

```

From the dictionary of j (DOJ) the data flow for the fork (f g h) is

''5. Forks

As illustrated above, an isolated sequence of three verbs is called a fork; its monadic and dyadic cases are defined by:''

```txt

  g
 / \
f   h
|   |
y   y

    g
   / \
  f   h
 / \ / \
x  y x  y

```


Reading from left to right

2!:55 is exit.
0: is a verb that returns 0 for any input.
So now we know the script will terminate the j session with successful status.

What does it do before this?
1!:2 is "write to file", with left argument x as the data to write, and the right argument y specifies the file.
4: is a verb returning 4 for any input.  File 4 is stdout.

```txt
;
```
 is "raze".
The fork
```txt
(; (1!:2) 4:)
```
 writes data to stdout.
Good!

What is the data?
(}. {.~ _ ". [: }: 0&{::)
Because it has an odd number of verbs, this expresses a fork.  And because {:: (fetch) is in the fork the right argument y is a vector of boxes.  We know that the data has a number followed by some lines of text.  Let's read the fork from left to right.  The second verb, {. is "take" modified by the ~ "passive" adverb to swap arguments.  Take uses a shape argument on left (x), and the data to take as y.  Remembering the passive effect, the data to which take applies will be the beheaded vector of boxes---beheading removes the first line which is the number, and the fork to the right of {.~ computes the shape.  Now looking at the fourth verb, ". (numbers) the default in case of error is _ (infinity meaning "all" when used along a shape dimension to take) and the data for numbers is the curtailed }: content of the first box (index origin 0).  0 is & (bonded also known as curried) to fetch.  Curtailing removes the line feed.  Since this gives a list of boxes, but we need to display literal data, raze "unboxes" one level of boxing.
Good, if we have a list of boxed lines of input.

(<;.2)@:(1!:1) 3
(<;.2) is "< (box) ;. (cut) 2 .  The 2 specifies the last item of the data as the fret, and to preserve the frets.
(1!:1) 3  is "read stdin".

I chose to connect the parts into a single verb using @: (at).


With predefined verbs from standard profile we can write the simpler, more readable for native English speakers, and robust sentence which ensures a final linefeed fret and discards the frets with <;._2

exit@:0:@:(smoutput&>)@:(}. {.~ _ ". 0&{::)@:cutLF@:(1!:1) 3

Cheers!  That's tacit j.


## Java


```java
import java.util.Scanner;

public class Main {
	public static void doStuff(String word){
	   System.out.println(word);
	}

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int n = Integer.parseInt(in.nextLine());  //doesn't use nextInt() so nextLine doesn't just read newline character
		for(int i=0; i<n; i++){
			String word = in.nextLine();
			doStuff(word);
		}
	}
}
```



## Julia

{{works with|Julia|0.6}}


```julia
function dosomething(words)
    print(words)
end

nlines = parse.(Int, readline())
for _ in 1:nlines
    words = readline()
    dosomething(words)
end
```



## Kotlin


```scala
// version 1.1

fun output(lines: Array<String>) = println(lines.joinToString("\n"))

fun main(args: Array<String>) {
    println("Enter the number of lines to be input followed by those lines:\n")
    val n = readLine()!!.toInt()
    val lines = Array(n) { readLine()!! }
    println("\nThe lines you entered are:\n")
    output(lines)
}
```


{{out}}

```txt

Enter the number of lines to be input followed by those lines:

3
hello
hello world
Pack my Box with 5 dozen liquor jugs

The lines you entered are:

hello
hello world
Pack my Box with 5 dozen liquor jugs

```



## Lua


```lua
function show (t)
    for _, line in pairs(t) do print(line) end
end

local lineTable, numLines = {}, io.read()
for i = 1, numLines do table.insert(lineTable, io.read()) end
show(lineTable)
```



## Objeck


```objeck
use System.IO.File;

class Rosetta {
  function : Main(args : String[]) ~ Nil {
    in : FileReader;
    leaving {
      if(in <> Nil) {
        in->Close();
      };
    };

    if(args->Size() = 1) {
      in := FileReader->New(args[0]);
      i := in->ReadString()->ToInt();
      while(i-- <> 0) {
        in->ReadString()->PrintLine();
      };
    };
  }
}
```



## PARI/GP


This task is not possible to implement directly in GP: for <code>input()</code> to take a string the user would have to wrap it in quotes (and escape quotes and newlines). One must use PARI:

```c
#include <stdio.h>
#include <stdlib.h>
#include <pari/pari.h>

int main(void);

int
main()
{
  int i, n, s;
  GEN vec;

  // 1 MB stack, not using prime table
  pari_init(1000000, 0);

  scanf("%d", &n);
  GEN vec = cgetg(n+1, t_VEC);

  for (i = 1; i <= n; i++) {
    if (1 != scanf("%s", &s)) abort();
    gel(vec, i) = strtoGENstr(s);
  }

  pari_printf("%Ps", vec);
  return 0;
}
```



## Perl


```perl
$n = scalar <>;

do_stuff(scalar <>) for 1..$n;

sub do_stuff { print $_[0] }
```



## Perl 6


Short version:


```perl6
say get for ^get;
```


Verbose version:


```perl6
sub do-stuff ($line) {
    say $line;
}

my $n = +get;
for ^$n {
    my $line = get;
    do-stuff $line;
}
```



## Phix


```Phix
sequence stack = {}
procedure push(string line)
    stack = append(stack,line)
end procedure

procedure pop_all()
    while length(stack) do
        puts(1,stack[1])
        stack = stack[2..$]
    end while
end procedure

string line = gets(0)
sequence r = scanf(trim(line),"%d")
if length(r)!=1 then
    puts(1,"input not a number\n")
    abort(0)
end if
puts(1,"\n")
for i=1 to r[1][1] do
    line = gets(0)
    push(line)
    puts(1,"\n")
end for
puts(1,"===\n")
pop_all()
```

{{out}}
(or more accurately the final state of the console)

```txt

3
one
two
three
===
one
two
three

```



## PowerShell


```PowerShell

# script.ps1

$in = Get-Content $args[0]
$in[1..($in.Count-1)]

# ./script file.txt

```



## Python


```python
try: input = raw_input
except: pass

def do_stuff(words):
	print(words)

linecount = int(input())
for x in range(linecount):
	line = input()
	do_stuff(line)
```



## Prolog


```Prolog

number_of_lines(Num) :-
	current_input(In),
	read_line_to_codes(In, Line),
	number_codes(Num, Line).

input_lines_for_num(0, ListOfLines)	:-
	format('~nThe lines you entered were: ~n~n'),
	maplist(format('~w~n'), ListOfLines).
input_lines_for_num(Num, CurrentLines) :-
	Num > 0,
	Num1 is Num - 1,
	current_input(In),
	read_line_to_codes(In, Line),
	atom_codes(LineAsAtom, Line),
	append(CurrentLines, [LineAsAtom], MoreLines),
	input_lines_for_num(Num1, MoreLines).

lines :-
	number_of_lines(Num),
	input_lines_for_num(Num, []).

```

{{out}}

```txt

2 ?- lines.
|: 3
line 1
line 2
line 3

The lines you entered were:

line 1
line 2
line 3
true ;
false.

3 ?-

```



## Racket

{{trans|Python}}

```Racket
#lang racket
(define (do-stuff str)
  (displayln str))

;(define line-count (read)) ;reads all kind of things

(define line-count (string->number ;only reads numbers
                    (string-trim
                     (read-line))))

(for ([i (in-range line-count)])
  (do-stuff (read-line)))
```



## REXX

Programming note:   this method was chosen because the standard input may be identical to the standard output.

```rexx
/*REXX program writes a number of lines from the default input file (C.L.).   */
#=linein()                             /*number of lines to be read from C.L. */

  do j=1  for #;   x.j=linein();  end  /*obtain input lines from stdin (C.L.).*/

call stuff                             /*call the STUFF subroutine for writes.*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
stuff:    do k=1  for #;   call lineout ,x.k;   end;          return
```

{{out}}

```txt
3
aaa
bbb
ccc
aaa
bbb
ccc
```



## Ring


```ring

# Project : Input/Output for Lines of Text

see "n = "
give n
lines = list(number(n))
for i = 1 to  n
    see "lines[" + i + "] = " + nl
    give lines[i]
next
see nl
printlines(lines)

func printlines(lines)
     for i = 1 to len(lines)
         see lines[i] + nl
     next

```

Input:

```txt

3
hello
hello world
Pack my Box with 5 dozen liquor jugs

```

Output:

```txt

hello
hello world
Pack my Box with 5 dozen liquor jugs

```



## Ruby


```ruby
def do_stuff(line)
  puts line
end

n = gets.to_i
n.times do
  line = gets
  do_stuff(line)
end
```



## Scala


```Scala
// Input/Output for Lines of Text
object IOLines extends App {
  private val in = scala.io.StdIn
  private val n = in.readInt()

  private def doStuff(word: String): Unit = println(word)

  for (_ <- 0 until n) {
    val word = in.readLine()
    doStuff(word)
  }
}
```


## Tcl


```tcl
proc do_stuff {line} {
    puts $line
}

foreach - [lrepeat [gets stdin] dummy] {
    do_stuff [gets stdin]
}
```



## Ursa


```ursa
#
# input/output for lines of text
#

# get how many lines the user wants
decl int amount
set amount (in int console)

# loop through and get lines
decl string<> lines
decl int i
for (set i 0) (< i amount) (inc i)
        append (in string console) lines
end for

# output the lines that the user entered
out endl console
for (set i 0) (< i amount) (inc i)
        out lines<i> endl console
end for

```



## zkl

File ff.zkl:

```zkl
numLines:=File.stdin.readln().strip().toInt();
text:=File.stdin.readln(numLines);

text.apply(File.stdout.write);
```

{{out}}

```txt

cat foo.txt | zkl ff
hello
hello world
Pack my Box with 5 dozen liquor jugs

```
