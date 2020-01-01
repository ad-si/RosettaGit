+++
title = "Copy stdin to stdout"
description = ""
date = 2019-08-22T04:10:21Z
aliases = []
[extra]
id = 22066
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Create an executable file that copies stdin to stdout, or else a script that does so through the invocation of an interpreter at the command line.


## Aime


```aime
file f;
data b;
f.stdin;
while (f.b_line(b) ^ -1) {
    o_(b, "\n");
}
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
BEGIN
    BOOL at eof := FALSE;
    # set the EOF handler for stand in to a procedure that sets "at eof" to true #
    # and returns true so processing can continue                                #
    on logical file end( stand in, ( REF FILE f )BOOL: at eof := TRUE );
    # copy stand in to stand out                                                 #
    WHILE STRING line; read( ( line, newline ) ); NOT at eof DO write( ( line, newline ) ) OD
END
```


=={{Header|AWK}}==
Using the awk interpreter, the following command uses the pattern // (which matches anything) with the default action (which is to print the current line) and so copy lines from stdin to stdut.

```AWK
awk "//"
```


=={{Header|C}}==

```C

#include <stdio.h>

int main(){
  char c;
  while ( (c=getchar()) != EOF ){
    putchar(c);
  }
  return 0;
}

```



## C++


```cpp
#include <iostream>
#include <iterator>

int main() {
    using namespace std;
    noskipws(cin);
    copy(
        istream_iterator<char>(cin),
        istream_iterator<char>(),
        ostream_iterator<char>(cout)
    );
    return 0;
}
```



## D


```d
import std.stdio;

void main() {
    foreach (line; stdin.byLine) {
        writeln(line);
    }
}
```



## Go


```go
package main

import (
    "bufio"
    "io"
    "os"
)

func main() {
    r := bufio.NewReader(os.Stdin)
    w := bufio.NewWriter(os.Stdout)
    for {
        b, err := r.ReadByte()
        if err == io.EOF {
            return
        }
        w.WriteByte(b)
        w.Flush()
    }
}
```


=={{Header|Julia}}==

```Julia
while !eof(stdin)
    write(stdout, read(stdin, UInt8))
end
```



## Kotlin


```scala
fun main() {
    var c: Int
    do {
        c = System.`in`.read()
        System.out.write(c)
    } while (c >= 0)
}
```


=={{Header|Mercury}}==

```Mercury


:- module stdin_to_stdout.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%



main(!IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(Line),
        io.write_string(Line, !IO),
        main(!IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, Message),
        io.input_stream_name(StreamName, !IO),
        io.progname("stdin_to_stdout", ProgName, !IO),
        io.write_strings([
            ProgName, ": ",
            "error reading from `", StreamName, "': \n\t",
            Message, "\n"
        ], !IO)
    ).

%-----------------------------------------------------------------------------%

```


=={{Header|OCaml}}==


```ocaml
try
  while true do
    output_char stdout (input_char stdin)
  done
with End_of_file -> ()
```



=={{Header|Perl}}==

```perl

perl -pe ''

```



## Perl 6

When invoked at a command line: Slightly less magical than Perl / sed. The p flag means automatically print each line of output to STDOUT. The e flag means execute what follows inside quotes. ".lines" reads lines from the assigned pipe (file handle), STDIN by default.


```perl6
perl6 -pe'.lines'
```


When invoked from a file: Lines are auto-chomped, so need to re-add newlines (hence .say rather than .print)

```perl6>.say for lines</lang



## Phix


```Phix
while true do
    integer ch = wait_key()
    if ch=#1B then exit end if
    puts(1,ch)
end while
```



## PicoLisp


```PicoLisp
(in NIL (echo))
```


=={{Header|Prolog}}==

```Prolog

%File: stdin_to_stdout.pl
:- initialization(main).

main :- repeat,
	get_char(X),
	put_char(X),
	X == end_of_file,
	fail.

```


Invocation at the command line (with Swi-prolog):

```sh

swipl stdin_to_stdout.pl

```



## Python


```txt
python -c 'import sys; sys.stdout.write(sys.stdin.read())'
```



## R


```txt
Rscript -e 'cat(readLines(file("stdin")))'
```


=={{Header|Racket}}==

```racket
#lang racket

(let loop ()
  (match (read-char)
    [(? eof-object?) (void)]
    [c (display c)
       (loop)]))
```


=={{Header|REXX}}==
In the REXX language,   the   '''STDIN'''   (default input
stream)   is normally the console,   and
the   '''STDOUT'''   (default output stream)   is
normally the console.    So for REXX, this task equates to copying data
from the console to itself.

```rexx
/*REXX pgm copies data from STDIN──►STDOUT (default input stream──►default output stream*/

  do while chars()\==0                           /*repeat loop until no more characters.*/
  call charin  , x                               /*read  a char from the  input stream. */
  call charout , x                               /*write "   "    "   "   output   "    */
  end   /*while*/                                /*stick a fork in it,  we're all done. */
```


=={{Header|Rust}}==

```Rust
use std::io;

fn main() {
    io::copy(&mut io::stdin().lock(), &mut io::stdout().lock());
}
```


=={{Header|Scheme}}==


```scheme

(do ((c (read-char) (read-char)))
    ((eof-object? c) 'done)
  (display c))

```


=={{Header|sed}}==


```sh

sed -e ''

```



## zkl


```zkl
zkl --eval "File.stdout.write(File.stdin.read())"
```

