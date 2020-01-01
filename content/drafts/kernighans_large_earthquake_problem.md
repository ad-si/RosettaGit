+++
title = "Kernighans large earthquake problem"
description = ""
date = 2019-08-17T17:24:17Z
aliases = []
[extra]
id = 21787
[taxonomies]
categories = []
tags = []
+++

{{task}}

[https://en.wikipedia.org/wiki/Brian_Kernighan Brian Kernighan], in a [https://www.youtube.com/watch?v=Sg4U4r_AgJU lecture] at the University of Nottingham, described a [https://youtu.be/Sg4U4r_AgJU?t=50s problem] on which this task is based.

;Problem:
You are given a a data file of thousands of lines; each of three `whitespace` separated fields: a date, a one word name and the magnitude of the event.

Example lines from the file would be lines like:

```txt
8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
3/13/2009    CostaRica           5.1
```


;Task:
* Create a program or script invocation to find all the events with magnitude greater than 6
* Assuming an appropriate name e.g. "data.txt" for the file:
:# Either: Show how your program is invoked to process a data file of that name.
:# Or: Incorporate the file name into the program, (as it is assumed that the program is single use).





## ALGOL 68


```algol68
IF  FILE input file;
    STRING file name = "data.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( ( "Unable to open """ + file name + """", newline ) )
ELSE
    # file opened OK #
    BOOL at eof := FALSE;
    # set the EOF handler for the file #
    on logical file end( input file, ( REF FILE f )BOOL:
                                     BEGIN
                                         # note that we reached EOF on the latest read #
                                         at eof := TRUE;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                       );
    # return the real value of the specified field on the line #
    PROC real field = ( STRING line, INT field )REAL:
         BEGIN
            REAL result  := 0;
            INT  c pos   := LWB line;
            INT  max pos := UPB line;
            STRING f     := "";
            FOR f ield number TO field WHILE c pos <= max pos DO
                # skip leading spaces #
                WHILE IF c pos > max pos THEN FALSE ELSE line[ c pos ] = " " FI DO
                    c pos +:= 1
                OD;
                IF c pos <= max pos THEN
                    # have a field #
                    INT start pos = c pos;
                    WHILE IF c pos > max pos THEN FALSE ELSE line[ c pos ] /= " " FI DO
                        c pos +:= 1
                    OD;
                    IF field number = field THEN
                        # have the required field #
                        f := line[ start pos : c pos - 1 ]
                    FI
                FI
            OD;
            IF f /= "" THEN
                # have the field - assume it a real value and convert it #
                FILE real value;
                associate( real value, f );
                on value error( real value
                              , ( REF FILE f )BOOL:
                                     BEGIN
                                         # "handle" invalid data #
                                         result := 0;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                              );
                get( real value, ( result ) )
            FI;
            result
         END # real field # ;
    # show the lines where the third field is > 6 #
    WHILE NOT at eof
    DO
        STRING line;
        get( input file, ( line, newline ) );
        IF real field( line, 3 ) > 6 THEN
            print( ( line, newline ) )
        FI
    OD;
    # close the file #
    close( input file )
FI
```



## AWK


```awk
 awk '$3 > 6' data.txt
```



## C++


```cpp
// Randizo was here!
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main()
{
    ifstream file("../include/earthquake.txt");

    int count_quake = 0;
    int column = 1;
    string value;
    double size_quake;
    string row = "";


    while(file >> value)
    {
        if(column == 3)
        {
            size_quake = stod(value);

            if(size_quake>6.0)
            {
                count_quake++;
                row += value + "\t";
                cout << row << endl;
            }

            column = 1;
            row = "";
        }
        else
        {
            column++;
            row+=value + "\t";
        }
    }

    cout << "\nNumber of quakes greater than 6 is " << count_quake << endl;

    return 0;
}
```


New version:

```cpp
// Jolkdarr was also here!
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>

int main() {
    using namespace std;
    ifstream file("data.txt");
    int count_quake = 0;
    string s1, s2;
    double rate;
    while (!file.eof()) {
        file >> s1 >> s2 >> rate;
        if (rate > 6.0) {
        	cout << s1 << setw(20) << s2 << " " << rate << endl;
        	count_quake++;
        }
    }

    cout << endl << "Number of quakes greater than 6 is " << count_quake << endl;
    return 0;
}
```



## C


```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    char *lw, *lt;
    fp = fopen("data.txt", "r");
    if (fp == NULL) {
        printf("Unable to open file\n");
        exit(1);
    }
    printf("Those earthquakes with a magnitude > 6.0 are:\n\n");
    while ((read = getline(&line, &len, fp)) != EOF) {
        if (read < 2) continue;   /* ignore blank lines */
        lw = strrchr(line, ' ');  /* look for last space */
        lt = strrchr(line, '\t'); /* look for last tab */
        if (!lw && !lt) continue; /* ignore lines with no whitespace */
        if (lt > lw) lw = lt;     /* lw points to last space or tab */
        if (atof(lw + 1) > 6.0) printf("%s", line);
    }
    fclose(fp);
    if (line) free(line);
    return 0;
}
```


{{output}}
Using the given file:

```txt

Those earthquakes with a magnitude > 6.0 are:

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```




## C#


```c#
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    static void Main() {
        foreach (var earthquake in LargeEarthquakes("data.txt", 6))
            Console.WriteLine(string.Join(" ", earthquake));
    }

    static IEnumerable<string[]> LargeEarthquakes(string filename, double limit) =>
        from line in File.ReadLines(filename)
        let parts = line.Split(default(char[]), StringSplitOptions.RemoveEmptyEntries)
        where double.Parse(parts[2]) > limit
        select parts;

}
```



## Cixl


```cixl

use: cx;

'data.txt' `r fopen lines {
  let: (time place mag) @@s split ..;
  let: (m1 m2) $mag @. split &int map ..;
  $m1 6 >= $m2 0 > and {[$time @@s $place @@s $mag] say} if
} for

```


{{output}}

```txt

8/27/1883 Krakatoa 8.8
5/18/1980 MountStHelens 7.6

```



## D

{{trans|Kotlin}}

```d
import std.conv : to;
import std.regex : ctRegex, split;
import std.stdio : File, writeln;

void main() {
    auto ctr = ctRegex!"\\s+";

    writeln("Those earthquakes with a magnitude > 6.0 are:");
    foreach (line; File("data.txt").byLineCopy) {
        auto parts = split(line, ctr);
        if (parts[2].to!double > 6.0) {
            writeln(line);
        }
    }
}
```

{{out}}

```txt
Those earthquakes with a magnitude > 6.0 are:
8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
```



## Emacs Lisp


```lisp
#!/usr/bin/env emacs --script

(dolist (arg command-line-args-left)
  (find-file arg)
  (while (not (eobp))
    (let* ((line (buffer-substring (line-beginning-position)
                                   (line-end-position)))
           (magn (nth 2 (split-string line "\\s-+"))))
      (when (> (string-to-number magn) 6.0)
        (message line)))
    (forward-line 1))))
```



## Factor

<code>lines</code> is a convenience word that reads lines from standard input. If you don't want to type them all in yourself, it is suggested that you give the program a file to read. For example, on the Windows command line: <code>factor kernighan.factor < earthquakes.txt</code>

```factor
USING: io math math.parser prettyprint sequences splitting ;
IN: rosetta-code.kernighan

lines [ "\s" split last string>number 6 > ] filter .
```



## Go


```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    f, err := os.Open("data.txt")
    if err != nil {
        fmt.Println("Unable to open the file")
        return
    }
    defer f.Close()
    fmt.Println("Those earthquakes with a magnitude > 6.0 are:\n")
    input := bufio.NewScanner(f)
    for input.Scan() {
        line := input.Text()
        fields := strings.Fields(line)
        mag, err := strconv.ParseFloat(fields[2], 64)
        if err != nil {
            fmt.Println("Unable to parse magnitude of an earthquake")
            return
        }
        if mag > 6.0 {
            fmt.Println(line)
        }
    }
}
```


{{out}}

```txt

Those earthquakes with a magnitude > 6.0 are:

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```



## Groovy

{{trans|Kotlin}}

```groovy
import java.util.regex.Pattern

class LargeEarthquake {
    static void main(String[] args) {
        def r = Pattern.compile("\\s+")
        println("Those earthquakes with a magnitude > 6.0 are:\n")
        def f = new File("data.txt")
        f.eachLine { it ->
            if (r.split(it)[2].toDouble() > 6.0) {
                println(it)
            }
        }
    }
}
```

{{out}}

```txt
Those earthquakes with a magnitude > 6.0 are:

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
```



## Kotlin


```scala
// Version 1.2.40

import java.io.File

fun main(args: Array<String>) {
    val r = Regex("""\s+""")
    println("Those earthquakes with a magnitude > 6.0 are:\n")
    File("data.txt").forEachLine {
        if (it.split(r)[2].toDouble() > 6.0) println(it)
    }
}
```


{{output}}
Using the given file:

```txt

Those earthquakes with a magnitude > 6.0 are:

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```



## Haskell


```haskell
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  cs <- C.readFile "data.txt"
  mapM_ print $
    C.lines cs >>=
    (\x ->
        [ x
        | 6 < (read (last (C.unpack <$> C.words x)) :: Float) ])
```

{{Out}}

```txt
"8/27/1883    Krakatoa            8.8"
"5/18/1980    MountStHelens       7.6"
```




## J


```J

NB. this program is designed for systems where the line ending is either LF or CRLF


NB. filename select_magnitude minimum
NB. default file is /tmp/famous.quakers

select_magnitude=: '/tmp/famous.quakers'&$: : (4 :0)
 data =. 1!:1 boxopen x       NB. read the file
 data =. data -. CR           NB. remove nasty carriage returns
 data =. ,&LF^:(LF~:{:) data  NB. append new line if none found
 lines =. [;._2 data          NB. split the literal based on the final character
 magnitudes =. ". _1&{::@(<;._2)@(,&' ')@deb"1 lines
 (y <: magnitudes) # lines
)

```



```txt

   select_magnitude 6
8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```



## Julia

Using the example data as a small text file.

```julia
using DataFrames, CSV

df = CSV.File("kernighansproblem.txt", delim=" ", ignorerepeated=true,
    header=["Date", "Location", "Magnitude"], types=[DateTime, String, Float64],
    dateformat="mm/dd/yyyy") |> DataFrame

println(filter(row -> row[:Magnitude] > 6, df))

```
 {{output}}
```txt

2×3 DataFrame
│ Row │ Date                │ Location      │ Magnitude │
│     │ DateTime            │ String        │ Float64   │
├─────┼─────────────────────┼───────────────┼───────────┤
│ 1   │ 1883-08-27T00:00:00 │ Krakatoa      │ 8.8       │
│ 2   │ 1980-05-18T00:00:00 │ MountStHelens │ 7.6       │

```



## Lua

For each line, the Lua pattern "%S+$" is used to capture between the final space character and the end of the line.

```lua
-- arg[1] is the first argument provided at the command line
for line in io.lines(arg[1] or "data.txt") do  -- use data.txt if arg[1] is nil
  magnitude = line:match("%S+$")
  if tonumber(magnitude) > 6 then print(line) end
end
```



## Perl


```perl
perl -n -e '/(\S+)\s*$/ and $1 > 6 and print' data.txt
```



## Perl 6

{{works with|Rakudo|2018.03}}
Pass in a file name, or use default for demonstration purposes.

```perl6
$_ = @*ARGS[0] ?? @*ARGS[0].IO !! q:to/END/;
    8/27/1883    Krakatoa            8.8
    5/18/1980    MountStHelens       7.6
    3/13/2009    CostaRica           5.1
    END

map { .say if .words[2] > 6 }, .lines;
```



## PHP

Parse using PHP's fscanf().

```php
<?php

// make sure filename was specified on command line
if ( ! isset( $argv[1] ) )
	die( 'Data file name required' );

// open file and check for success
if ( ! $fh = fopen( $argv[1], 'r' ) )
	die ( 'Cannot open file: ' . $argv[1] );

while ( list( $date, $loc, $mag ) = fscanf( $fh, "%s %s %f" ) ) {
	if ( $mag > 6 ) {
		printf( "% -12s % -19s %.1f\n", $date, $loc, $mag );
	}
}

fclose( $fh );

```


Usage: Specify file name on command line. Ex:
<code>php eq.php data.txt</code>

{{Out}}
<pre style="font-size:84%">
8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```



## Phix


```Phix
sequence cl = command_line()
string filename = iff(length(cl)>=3?cl[3]:"e02.txt")
integer fn = open(filename,"r")
if fn=-1 then crash("cannot open filename") end if
while 1 do
    object line = gets(fn)
    if line=-1 then exit end if
    line = substitute(trim(line),"\t"," ")
    sequence r = scanf(line,"%s %f")
    if length(r)=1 and r[1][2]>6 then ?line end if
end while
close(fn)
```

{{out}}

```txt

"8/27/1883    Krakatoa            8.8"
"5/18/1980    MountStHelens       7.6"

```



## Python

Typed into a bash shell or similar:

```python
python -c '
with open("data.txt") as f:
    for ln in f:
        if float(ln.strip().split()[2]) > 6:
            print(ln.strip())'
```



Or, if scale permits a file slurp and a parse retained for further processing, we can combine the parse and filter with a concatMap abstraction:


```python
from os.path import expanduser
from functools import (reduce)
from itertools import (chain)


# largeQuakes :: Int -> [String] -> [(String, String, String)]
def largeQuakes(n):
    def quake(threshold):
        def go(x):
            ws = x.split()
            return [tuple(ws)] if threshold < float(ws[2]) else []
        return lambda x: go(x)
    return concatMap(quake(n))


# main :: IO ()
def main():
    print (
        largeQuakes(6)(
            open(expanduser('~/data.txt')).read().splitlines()
        )
    )


# GENERIC ABSTRACTION -------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[('8/27/1883', 'Krakatoa', '8.8'), ('5/18/1980', 'MountStHelens', '7.6')]
```



## Racket


The file specified contains the three lines from the task description.

This is just a file filter, matching lines are printed out.


```racket
#lang racket

(with-input-from-file "data/large-earthquake.txt"
  (λ ()
    (for ((s (in-port read-line))
          #:when (> (string->number (third (string-split s))) 6))
    (displayln s))))
```



Or, defining a list -> list function in terms of '''filter''':

```scheme
#lang racket

; largeQuakes :: Int -> [String] -> [String]
(define (largeQuakes n xs)
  (filter
   (λ (x)
     (< n (string->number (last (string-split x)))))
   xs))

; main :: IO ()
(module* main #f
  (display
   (unlines
    (largeQuakes
     6
     (lines (readFile "~/quakes.txt"))))))


; GENERIC ---------------------------------------------

; lines :: String -> [String]
(define (lines s)
  (string-split s "\n"))

; readFile :: FilePath -> IO String
(define (readFile fp)
  (file->string
   (expand-user-path fp)))

; unlines :: [String] -> String
(define (unlines xs)
  (string-join xs "\n"))
```


{{out}}

```txt
8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
```


To combine filtering with more pre-processing, we can use '''concatMap''' in place of '''filter''':

```scheme
#lang racket

(require gregor) ; Date parsing

; test :: IO ()
(module* main #f
  (for
      ([q ((quakesAbove 6)
           (lines (readFile "~/quakes.txt")))])
    (writeln q)))


; quakesAbove :: Int -> [String] -> [(Date, String, Float)]
(define (quakesAbove n)
  (λ (xs)
    ((concatMap
      (λ (x)
        (local [(define-values (dte k mgn)
                  (apply values (string-split x)))
                (define m (string->number mgn))]
          (if (< n m)
              (list (list (parse-date dte "M/d/y") k m))
              '()))))
     xs)))

; GENERIC ---------------------------------------------

; concatMap :: (a -> [b]) -> [a] -> [b]
(define (concatMap f)
  (λ (xs)
    (foldr (λ (x a) (append (f x) a)) '() xs)))

; lines :: String -> [String]
(define (lines s)
  (string-split s "\n"))

; readFile :: FilePath -> IO String
(define (readFile fp)
  (file->string
   (expand-user-path fp)))
```

{{Out}}

```txt
(#<date 1883-08-27> "Krakatoa" 8.8)
(#<date 1980-05-18> "MountStHelens" 7.6)
```



## REXX

A little extra coding was added to provide:
:::*   an output title   (with centering and better alignment)
:::*   an error message for when the input file wasn't found   (or is empty)
:::*   the number of records read
:::*   the number of records that met the qualifying magnitude
:::*   the qualifying magnitude

```rexx
/*REXX program to read a file containing a list of earthquakes:   date, site, magnitude.*/
parse arg iFID mMag .                            /*obtain optional arguments from the CL*/
if iFID=='' | iFID==","  then iFID= 'earthquakes.dat' /*Not specified?  Then use default*/
if mMag=='' | mMag==","  then mMag= 6                 /* "      "         "   "     "   */
#=0                                              /*# of earthquakes that meet criteria. */
   do j=0  while lines(iFID)\==0                 /*read all lines in the input file.    */
   if j==0  then say 'Reading from file: ' iFID  /*show the name of the file being read.*/
   parse value linein(iFID) with date site mag . /*parse three words from an input line.*/
   if mag<=mMag  then iterate                    /*Is the quake too small?  Then skip it*/
   #= # + 1;     if j==0  then say               /*bump the number of qualifying quakes.*/
   if #==1  then say center('date', 20, "═")     '=magnitude='     center("site", 20, '═')
   say               center(date, 20)      center(mag/1, 11)   '  '        site
   end   /*j*/                                   /*stick a fork in it,  we're all done. */
say
say
if j\==0  then say j  'records read from file: ' iFID
say
if j==0  then say er 'file    '          iFID           "   is empty or not found."
         else say #  ' earthquakes listed whose magnitude is  ≥ ' mMag
```

{{out|output|text=  when using the default inputs:}}

```txt

Reading from file:  earthquakes.dat

════════date════════ =magnitude= ════════site════════
     08/27/1883          8.8        Krakatoa
     05/18/1980          7.6        MountStHelens


3 records read from file:  earthquakes.dat

2  earthquakes listed whose magnitude is  ≥  6

```



## Ring


```ring

# Project  : Kernighans large earthquake problem

load "stdlib.ring"
nr = 0
equake = list(3)
fn = "equake.txt"
fp = fopen(fn,"r")

while not feof(fp)
         nr = nr + 1
         equake[nr] = readline(fp)
end
fclose(fp)
for n = 1 to len(equake)
     for m = 1 to len(equake[n])
          if equake[n][m] = " "
             sp = m
          ok
     next
     sptemp = right(equake[n],len(equake[n])-sp)
     sptemo = number(sptemp)
     if sptemp > 6
        see equake[n] + nl
     ok
next

```

Output:

```txt

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens   7.6

```



## Ruby


```txt

ruby -nae "$F[2].to_f > 6 && print" data.txt

```

A more interesting problem. Print only the events whose magnitude is above average.

Contents of the file:

```txt

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
3/13/2009    CostaRica           5.1
2000-02-02  Foo               7.7
1959-08-08   Bar             6.2
1849-09-09  Pym                9.0

```

The command:

```txt

ruby -e"m=$<.to_a;f=->s{s.split[2].to_f};a=m.reduce(0){|t,s|t+f[s]}/m.size;puts m.select{|s|f[s]>a}" e.txt

```

Output:

```txt

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6
2000-02-02  Foo               7.7
1849-09-09  Pym                9.0

```



## Scala


```Scala
scala.io.Source.fromFile("data.txt").getLines
  .map("\\s+".r.split(_))
  .filter(_(2).toDouble > 6.0)
  .map(_.mkString("\t"))
  .foreach(println)
```



## Swift


Expects the program to be started with the path to the data file.


```Swift
import Foundation

guard let path = Array(CommandLine.arguments.dropFirst()).first else {
  fatalError()
}

let fileData = FileManager.default.contents(atPath: path)!
let eventData = String(data: fileData, encoding: .utf8)!

for line in eventData.components(separatedBy: "\n") {
  guard let lastSpace = line.lastIndex(of: " "), // Get index of last space
        line.index(after: lastSpace) != line.endIndex, // make sure the last space isn't the end of the line
        let magnitude = Double(String(line[line.index(after: lastSpace)])),
        magnitude > 6 else { // Finally check the magnitude
    continue
  }

  print(line)
}
```



## Tcl

Inspired by awk.

```tcl
catch {console show} 			;## show console when running from tclwish
catch {wm withdraw .}

set filename "data.txt"
set fh [open $filename]
set NR 0 				;# number-of-record, means linenumber

while {[gets $fh line]>=0} { 		;# gets returns length of line, -1 means eof
    incr NR
    set  line2 [regexp -all -inline {\S+} $line]  ;# reduce multiple whitespace
    set  fld   [split $line2]  	;# split line into fields, at whitespace
    set  f3    [lindex $fld 2] 	;# zero-based
   #set  NF    [llength $fld]   	;# number-of-fields

    if {$f3 > 6} { puts "$line" }
}
close $fh
```



## Yabasic


```Yabasic
if peek("argument") then
    filename$ = peek$("argument")
else
    filename$ = "data.txt"
end if

dim tok$(1)
a = open(filename$)
if not a error "Could not open '" + filename$ + "' for reading"
while(not eof(a))
  line input #a a$
  void = token(a$, tok$())
  if val(tok$(3)) > 6 print a$
wend
close a
```



## zkl

While lexical comparsions [of numeric data] are fine for this problem, it
is bad practice so I don't do it (written so text is automatically
converted to float).

```zkl
fcn equake(data,out=Console){
   data.pump(out,fcn(line){ 6.0line.split()[-1] },Void.Filter)
}
```


```zkl
equake(Data(Void,
#<<<
"8/27/1883    Krakatoa            8.8\n"
"5/18/1980    MountStHelens       7.6\n"
"3/13/2009    CostaRica           5.1\n"
#<<<
));
```

or

```zkl
equake(File("equake.txt"));
```

or

```zkl
$ zkl --eval 'File.stdin.pump(Console,fcn(line){ 6.0<line.split()[-1] },Void.Filter)' < equake.txt
```

{{out}}

```txt

8/27/1883    Krakatoa            8.8
5/18/1980    MountStHelens       7.6

```

