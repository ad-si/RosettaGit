+++
title = "Check input device is a terminal"
description = ""
date = 2019-05-18T19:44:30Z
aliases = []
[extra]
id = 12341
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
[[Category:Terminal control]]
[[Category:Hardware]]
[[Category:Initialization]]
{{omit from|Clojure}}
{{omit from|Java|See bug JDK-4099017}}
{{omit from|GUISS}}
{{omit from|TI-83 BASIC|Input device is always either a terminal or created by the program}}
{{omit from|ZX Spectrum Basic}}

;Task:
Demonstrate how to check whether the input device is a terminal or not.


;Related task:
*   [[Check output device is a terminal]]





## Ada

{{works with|GNAT}}
We use the interface to C library functions <code>isatty()</code> and <code>fileno()</code>.


```ada
with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces.C_Streams; use Interfaces.C_Streams;

procedure Test_tty is
begin
   if Isatty(Fileno(Stdin)) = 0 then
      Put_Line(Standard_Error, "stdin is not a tty.");
   else
      Put_Line(Standard_Error, "stdin is a tty.");
   end if;
end Test_tty;
```


{{out}}


```txt

$ ./test_tty
stdin is a tty.
$ ./test_tty < /dev/null
stdin is not a tty.

```



## C

Use <code>isatty()</code> on file descriptor to determine if it's a TTY.  To get the file descriptor from a <code>FILE*</code> pointer, use <code>fileno</code>:

```c
#include <unistd.h>
// for isatty()
#include <stdio.h>	//for fileno()

int main(void)
{
	puts(isatty(fileno(stdin))
		? "stdin is tty"
		: "stdin is not tty");
	return 0;
}
```

{{out}}

```txt

$ ./a.out
stdin is tty
$ ./a.out < /dev/zero
stdin is not tty
$ echo "" | ./a.out
stdin is not tty

```



## Common Lisp

{{Works with|SBCL}}

```lisp
(with-open-stream (s *standard-input*)
  (format T "stdin is~:[ not~;~] a terminal~%"
          (interactive-stream-p s)))
```


{{Out}}

```txt
$ sbcl --script rc.lisp
stdin is a terminal
$ sbcl --script rc.lisp < /dev/zero
stdin is not a terminal
$ echo "" | sbcl --script rc.lisp
stdin is not a terminal
```



## Crystal


```ruby
File.new("testfile").tty?   #=> false
File.new("/dev/tty").tty?   #=> true
STDIN.tty?  #=> true
```



## D


```d
import std.stdio;

extern(C) int isatty(int);

void main() {
    if (isatty(0))
        writeln("Input comes from tty.");
    else
        writeln("Input doesn't come from tty.");
}
```

{{out}}

```txt
C:\test
Input comes from tty.
C:\test < in.txt
Input doesn't come from tty.
```



## Go

{{libheader|Go sub-repositories}}

```go
package main

import (
    "golang.org/x/crypto/ssh/terminal"
    "fmt"
    "os"
)

func main() {
    if terminal.IsTerminal(int(os.Stdin.Fd())) {
        fmt.Println("Hello terminal")
    } else {
        fmt.Println("Who are you?  You're not a terminal.")
    }
}
```

{{out}}

```txt

> hello
Hello terminal
> hello </dev/null
Who are you?  You're not a terminal.

```



## Haskell


Example uses [https://hackage.haskell.org/package/unix <tt>unix</tt>] package:


```haskell
module Main (main) where

import           System.Posix.IO (stdInput)
import           System.Posix.Terminal (queryTerminal)

main :: IO ()
main = do
    isTTY <- queryTerminal stdInput
    putStrLn $ if isTTY
                then "stdin is TTY"
                else "stdin is not TTY"
```



## Jsish


```javascript
/* Check input device is a terminal, in Jsish */
;Interp.conf().subOpts.istty;

/*
=!EXPECTSTART!=
Interp.conf().subOpts.istty ==> false
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]' or 'history'.  \ cancels > input.  ctrl-c aborts running script.
jsi> Interp.conf().subOpts.istty;
true
jsi>
prompt$ jsish --U checkInputDevice.jsi
Interp.conf().subOpts.istty ==> false
```



## Julia


```Julia

if isa(STDIN, Base.TTY)
    println("This program sees STDIN as a TTY.")
else
    println("This program does not see STDIN as a TTY.")
end

```


{{out}}

```txt

This program sees STDIN as a TTY.

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// Kotlin Native version 0.5

import platform.posix.*

fun main(args: Array<String>) {
    if (isatty(STDIN_FILENO) != 0)
        println("stdin is a terminal")
    else
        println("stdin is not a terminal")
}

```


{{out}}

```txt

stdin is a terminal

```



## Nemerle

There is no explicit way (ie <tt>isatty()</tt>)to do this; however, if we ''assume'' that standard input ''is'' a terminal, we can check if the input stream has been redirected (presumably to something other than a terminal).

```Nemerle>def isTerm = System.Console.IsInputRedirected;</lang



## OCaml



```ocaml
let () =
  print_endline (
    if Unix.isatty Unix.stdin
    then "Input comes from tty."
    else "Input doesn't come from tty."
  )
```


Testing in interpreted mode:


```txt
$ ocaml unix.cma istty.ml
Input comes from tty.
$ echo "foo" | ocaml unix.cma istty.ml
Input doesn't come from tty.

```



## Perl


```perl
use strict;
use warnings;
use 5.010;
if (-t) {
    say "Input comes from tty.";
}
else {
    say "Input doesn't come from tty.";
}
```


 $ perl istty.pl
 Input comes from tty.
 $ true | perl istty.pl
 Input doesn't come from tty.


## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
say $*IN.t ?? "Input comes from tty." !! "Input doesn't come from tty.";
```


 $ perl6 istty.p6
 Input comes from tty.
 $ true | perl6 istty.p6
 Input doesn't come from tty.


## Pike


```pike
void main()
{
    if(Stdio.Terminfo.is_tty())
	write("Input comes from tty.\n");
    else
        write("Input doesn't come from tty.\n");
}
```


{{out}}

```txt
$ ./istty.pike
Input comes from tty.
$ echo | ./istty.pike
Input doesn't come from tty.
```



## Python


```python
from sys import stdin
if stdin.isatty():
    print("Input comes from tty.")
else:
    print("Input doesn't come from tty.")
```


 $ python istty.py
 Input comes from tty.
 $ true | python istty.py
 Input doesn't come from tty.


## Racket


```racket

(terminal-port? (current-input-port))

```



## REXX


```rexx
/*REXX program determines if input comes from terminal or standard input*/

if queued()  then say 'input comes from the terminal.'
             else say 'input comes from the (stacked) terminal queue.'

                                       /*stick a fork in it, we're done.*/

```



## Ring


```ring

# Project  : Check input device is a terminal

load "stdlib.ring"

if isWindows()
   write("mycmd.bat","
   @echo off
    timeout 1 2>nul >nul
    if errorlevel 1 (
       echo input redirected
        ) else (
       echo input is console
       )
       ")
    see SystemCmd("mycmd.bat")
ok

```

Output:

```txt

input redirected

```



## Ruby

Example from the docs.

```ruby
File.new("testfile").isatty   #=> false
File.new("/dev/tty").isatty   #=> true
```



## Rust


```rust
/* Uses C library interface */

extern crate libc;

fn main() {
    let istty = unsafe { libc::isatty(libc::STDIN_FILENO as i32) } != 0;
    if istty {
        println!("stdout is tty");
    } else {
        println!("stdout is not tty");
    }
}
```



## Scala

{{Works with|Ubuntu|14.04}}

```scala
import org.fusesource.jansi.internal.CLibrary._

object IsATty  extends App {

  var enabled = true

  def apply(enabled: Boolean): Boolean = {
    // We must be on some unix variant..
    try {
      enabled && isatty(STDIN_FILENO) == 1
    }
    catch {
      case ignore: Throwable =>
        ignore.printStackTrace()
        false

    }
  }

    println("tty " + apply(true))
}
```



## Tcl

Tcl automatically detects whether <tt>stdin</tt> is coming from a terminal (or a socket) and sets up the channel to have the correct type. One of the configuration options of a terminal channel is <tt>-mode</tt> (used to configure baud rates on a real serial terminal) so we simply detect whether the option is present.

```tcl
if {[catch {fconfigure stdin -mode}]} {
    puts "Input doesn't come from tty."
} else {
    puts "Input comes from tty."
}
```

Demonstrating:

```txt

$ tclsh8.5 istty.tcl
Input comes from tty.
$ tclsh8.5 istty.tcl </dev/null
Input doesn't come from tty.

```



## UNIX Shell


```sh
#!/bin/sh

if [ -t 0 ]
then
   echo "Input is a terminal"
else
   echo "Input is NOT a terminal"
fi
```



## zkl

On Unix, check to see if stdin's st_mode is a character device.

```zkl
const S_IFCHR=0x2000;
fcn S_ISCHR(f){ f.info()[4].bitAnd(S_IFCHR).toBool() }
S_ISCHR(File.stdin).println();
```

{{out}}

```txt

$ zkl bbb  # from the command line
True
$ zkl bbb < bbb.zkl
False
$ cat bbb.zkl | zkl bbb
False

```

