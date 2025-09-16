+++
title = "Check output device is a terminal"
description = ""
date = 2019-08-26T23:37:56Z
aliases = []
[extra]
id = 13194
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "c",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "factor",
  "go",
  "haskell",
  "j",
  "javascript_nodejs",
  "julia",
  "kotlin",
  "nemerle",
  "ocaml",
  "perl",
  "perl_6",
  "php",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "tcl",
  "unix_shell",
  "visual_basic_.net",
  "zkl",
]
+++

## Task

Demonstrate how to check whether the output device is a terminal or not.


## Related tasks

*   [[Check input device is a terminal]]





## Ada

We use the interface to C library functions <code>isatty()</code> and <code>fileno()</code>.


```ada
with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces.C_Streams; use Interfaces.C_Streams;

procedure Test_tty is
begin
   if Isatty(Fileno(Stdout)) = 0 then
      Put_Line(Standard_Error, "stdout is not a tty.");
   else
      Put_Line(Standard_Error, "stdout is a tty.");
   end if;
end Test_tty;
```


```txt

$ ./test_tty
stdout is a tty.
$ ./test_tty > /dev/null
stdout is not a tty.

```


## C


Use <code>isatty()</code> on file descriptor to determine if it's a TTY.  To get the file descriptor from a <code>FILE*</code> pointer, use <code>fileno</code>:


```c
#include <unistd.h>   // for isatty()
#include <stdio.h>    // for fileno()

int main()
{
    puts(isatty(fileno(stdout))
          ? "stdout is tty"
          : "stdout is not tty");
    return 0;
}
```


```txt

$ ./a.out
stdout is tty

$ ./a.out > tmp
$ cat tmp
stdout is not tty

$ ./a.out | cat
stdout is not tty

```



## C++

```cpp
#if _WIN32
#include <io.h>
#define ISATTY _isatty
#define FILENO _fileno
#else
#include <unistd.h>
#define ISATTY isatty
#define FILENO fileno
#endif

#include <iostream>

int main() {
    if (ISATTY(FILENO(stdout))) {
        std::cout << "stdout is a tty\n";
    } else {
        std::cout << "stdout is not a tty\n";
    }

    return 0;
}
```


## C#

```c#
using System;

namespace CheckTerminal {
    class Program {
        static void Main(string[] args) {
            Console.WriteLine("Stdout is tty: {0}", Console.IsOutputRedirected);
        }
    }
}
```



## Common Lisp

```lisp
(with-open-stream (s *standard-output*)
  (format T "stdout is~:[ not~;~] a terminal~%"
          (interactive-stream-p s)))
```


```txt
$ sbcl --script rc.lisp
stdout is a terminal
$ sbcl --script rc.lisp | cat
stdout is not a terminal
$ sbcl --script rc.lisp > foo.txt
$ cat foo.txt
stdout is not a terminal
```


We use the interface to C library functions <code>isatty()</code> and <code>fileno()</code>. It needs to be compiled to be executed.


```lisp
(ffi:clines "
    #include <sys/ioctl.h>
    #include <unistd.h>
    int ttyPredicate() {
      return isatty(fileno(stdout));
     }")

(ffi:def-function
    ("ttyPredicate" c-ttyp)
    () :returning :int)

(defun tty-p()
  (if (= 1 (c-ttyp))
      t
      nil))

(format T "stdout is~:[ not~;~] a terminal~%" (tty-p))
(quit)
```


Compilation can be done with the following commands :

<code>ecl --eval '(compile-file "file.lisp" :system-p t)' --eval '(quit)'</code>

<code>ecl --eval '(c:build-program "is-tty" :lisp-files (list "file.o"))' --eval '(quit)'</code>

```txt
$ ./is-tty
stdout is a terminal
$ ./is-tty  | cat -
stdout is not a terminal
```



## Crystal


```ruby
File.new("testfile").tty?   #=> false
File.new("/dev/tty").tty?   #=> true
STDOUT.tty?  #=> true
```



## D


```D
import std.stdio;

extern(C) int isatty(int);

void main() {
    writeln("Stdout is tty: ", stdout.fileno.isatty == 1);
}
```


```txt

prompt>a.out
Stdout is tty: true
prompt>a.out > out.txt
Stdout is tty: false

```



## Factor

You have to know 1 is the correct file descriptor number:

```factor

IN: scratchpad USE: unix.ffi
IN: scratchpad 1 isatty

--- Data stack:
1

```




## Go

```go
package main

import (
    "os"
    "fmt"
    "golang.org/x/crypto/ssh/terminal"
)

func main() {
    if terminal.IsTerminal(int(os.Stdout.Fd())) {
        fmt.Println("Hello terminal")
    } else {
        fmt.Println("Who are you?  You're not a terminal.")
    }
}
```

```txt

> hello
Hello terminal
> hello | cat
Who are you?  You're not a terminal.

```



## Haskell



```haskell
module Main where

-- requires the unix package
-- https://hackage.haskell.org/package/unix
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

main :: IO ()
main = do
  istty <- queryTerminal stdOutput
  putStrLn
    (if istty
       then "stdout is tty"
       else "stdout is not tty")
```

```txt
$ runhaskell istty.hs
stdout is tty
$ runhaskell istty.hs | cat
stdout is not tty

```



## Javascript/NodeJS


```js
node -p -e "Boolean(process.stdout.isTTY)"
true
```



## J



```J
3=nc<'wd'
```


Explanation:

J does not have a concept of an "output device", so we approximate that by seeing whether we have bothered to define a the code which typically does graphical output.

The use of the phrase "output device" suggests that we are thinking about something like the unix `isatty` command. Here, stdout might be a file or might be a terminal. But in J we are often hosting our own user interaction environment. It's not uncommon for a J user to be on a web page where hitting enter sends a form request to a J interpreter which in turn composes an updated html presentation of current state which it sends to the browser. Or, the J user might be talking to a Java program which similarly wraps the J session (though this is older technology at this point). Or, the J user might be interacting with Qt. Or, sure, we might be talking to a tty and J might be sending its output straight to the tty. (But we can't know if that tty is hosted in emacs, running under control of a script on a remote machine via ssh, talking directly to a human user who happens to be in direct control of the session, or whatever else...)

The point being that in the general case the J programmer cannot know whether the concept of "terminal" has any relevance to the user.

But, like everyone else, we can certainly use heuristics.

But, correctness requires us to keep in mind that these will only be heuristics, and will sometimes be incorrect (hopefully not often enough to matter a lot...).


## Julia


```Julia

if isa(STDOUT, Base.TTY)
    println("This program sees STDOUT as a TTY.")
else
    println("This program does not see STDOUT as a TTY.")
end

```


```txt

This program sees STDOUT as a TTY.

```



## Kotlin

```scala
// Kotlin Native version 0.5

import platform.posix.*

fun main(args: Array<String>) {
    if (isatty(STDOUT_FILENO) != 0)
        println("stdout is a terminal")
    else
        println("stdout is not a terminal")
}
```


```txt

stdout is a terminal

```



## Nemerle

There is no explicit way (ie <tt>isatty()</tt>)to do this; however, if we ''assume'' that standard out ''is'' a terminal, we can check if the output stream has been redirected (presumably to something other than a terminal).

```nemerle
def isTerm = System.Console.IsOutputRedirected;
```



## OCaml



```ocaml
let () =
  print_endline (
    if Unix.isatty Unix.stdout
    then "Output goes to tty."
    else "Output doesn't go to tty."
  )
```


Testing in interpreted mode:


```txt
$ ocaml unix.cma istty.ml
Output goes to tty.

$ ocaml unix.cma istty.ml > tmp
$ cat tmp
Output doesn't go to tty.

$ ocaml unix.cma istty.ml | cat
Output doesn't go to tty.

```



## Perl

The -t function on a filehandle tells you whether it's a terminal.


```bash
$ perl -e "warn -t STDOUT ? 'Terminal' : 'Other'"
Terminal
$ perl -e "warn -t STDOUT ? 'Terminal' : 'Other'" > x.tmp
Other

```



## Perl 6

The .t method on a filehandle tells you whether it's going to the terminal.  Here we use the note function to emit our result to standard error rather than standard out.

```txt
$ perl6 -e 'note $*OUT.t'
True
$ perl6 -e 'note $*OUT.t' >/dev/null
False
```



## PHP


```php

if(posix_isatty(STDOUT)) {
    echo "The output device is a terminal".PHP_EOL;
} else {
    echo "The output device is NOT a terminal".PHP_EOL;
}

```



## Python

Pretty much the same as [[Check input device is a terminal#Python]].

```python
from sys import stdout
if stdout.isatty():
    print 'The output device is a teletype. Or something like a teletype.'
else:
    print 'The output device isn\'t like a teletype.'
```



## Racket


```racket

(terminal-port? (current-output-port))

```


## REXX

Programming note:   The comment about the REXX statements have to be on one line isn't quite true,

but because the REXX special variable '''SIGL''' is defined where it's executed, it makes coding simpler.


'''SIGL'''   is set to the REXX statment number where:
:::*   a '''CALL''' statement is used
:::*   a ''function'' is invoked
:::*   a '''SIGNAL''' statement is used
Method used:   since REXX has no direct way of determining if the STDIN is a terminal or not, the REXX code (below)

actually ''raises'' (which is no way to run a railroad) a syntax error when attempting to read the 2<sup>nd</sup> line from   STDIN,

which causes a routine (named '''syntax:''') to get control, determines where the syntax error occurred, and returns an

appropriate string indicating if STDIN is a '''terminal''' (or '''other''').


Note that under VM/CMS, this can be accomplished with a (host) command within REXX and then examining the results.

On IBM mainframes, a user can have STDIN defined, but the terminal can be ''disconnected''.

```rexx
/*REXX program determines if the   STDIN   is a   terminal   or  other. */
signal on syntax                       /*if syntax error, jump──► SYNTAX*/
say  'output device:'  testSTDIN()     /*displays terminal ──or── other */
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────TESTSTDIN subroutine────────────────*/
testSTDIN: syntax.=1; signal .; .: z.=sigl; call linein ,2;  ..: syntax.=0
return z..                             /* [↑]   must all be on one line.*/
/*──────────────────────────────────SYNTAX subroutine───────────────────*/
syntax:  z..='other'                   /*when SYNTAX occur,  come here. */
if syntax.  then do                    /*handling  STDIN  thingy error? */
                 if sigl==z.  then z..='terminal'; signal ..   /*stdin ?*/
                 end                   /* [↑]   can't use a RETURN here.*/

     /* ··· handle other REXX syntax errors here ···   */
```

'''output'''

```txt

output device: terminal

```



## Ruby


```rust
f = File.open("test.txt")
p f.isatty          # => false
p STDOUT.isatty     # => true

```


## Rust


```rust
/* Uses C library interface */

extern crate libc;

fn main() {
    let istty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    if istty {
        println!("stdout is tty");
    } else {
        println!("stdout is not tty");
    }
}
```



## Scala

```scala
import org.fusesource.jansi.internal.CLibrary._

object IsATty  extends App {

  var enabled = true

  def apply(enabled: Boolean): Boolean = {
    // We must be on some unix variant..
    try {
      enabled && isatty(STDOUT_FILENO) == 1
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

To detect whether output is going to a terminal in Tcl, you check whether the <code>stdout</code> channel looks like a serial line (as those are indistinguishable from terminals). The simplest way of doing that is to see whether you can read the <tt>-mode</tt> or <code>-xchar</code> channel options, which are only present on serial channels:

```tcl
set toTTY [dict exists [fconfigure stdout] -mode]
puts [expr {$toTTY ? "Output goes to tty" : "Output doesn't go to tty"}]
```

At the system call level, when Tcl is setting up the channels that correspond to the underlying <tt>stdout</tt> (and <tt>stdin</tt> and <tt>stderr</tt>) file descriptors, it checks whether the channels are network sockets (with <code>getsockname()</code>) or serial lines (with <code>isatty()</code>). This allows Tcl scripts to find out information about their calling environment (e.g., when they are run from <tt>inetd</tt>) with minimal code.
Assuming that the above script is stored in the file <tt>istty.tcl</tt>:

```txt
$ tclsh8.5 istty.tcl
Output goes to tty
$ tclsh8.5 istty.tcl | cat
Output doesn't go to tty
```


### Channel type discovery with older Tcl versions

Before Tcl 8.4, this discovery process is impossible; <code>stdout</code> always looks like it is going to a file. With 8.4, you can discover the channel type but you need slightly different (and less efficient, due to the thrown error in the non-tty case) code to do it.

```tcl
set toTTY [expr {![catch {fconfigure stdout -mode}]}]
```



## UNIX Shell


```sh
#!/bin/sh

if [ -t 1 ]
then
   echo "Output is a terminal"
else
   echo "Output is NOT a terminal" >/dev/tty
fi
```



## Visual Basic .NET

```vbnet
Module Module1

    Sub Main()
        Console.WriteLine("Stdout is tty: {0}", Console.IsOutputRedirected)
    End Sub

End Module
```



## zkl

On Unix, check to see if stdout's st_mode is a character device.

```zkl
const S_IFCHR=0x2000;
fcn S_ISCHR(f){ f.info()[4].bitAnd(S_IFCHR).toBool() }
S_ISCHR(File.stdout).println();
```

```txt

$ zkl bbb  # from the command line
True
$ zkl bbb | more
False
$ zkl bbb > foo.txt
$ cat foo.txt
False

```

