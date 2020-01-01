+++
title = "Hello world/Standard error"
description = ""
date = 2019-10-01T05:45:26Z
aliases = []
[extra]
id = 3291
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
{{selection|Short Circuit|Console Program Basics}}
[[Category:Streams]]
{{omit from|Applesoft BASIC}}
{{omit from|bc|Always prints to standard output.}}
{{omit from|Brainfuck}}
{{omit from|dc|Always prints to standard output.}}
{{omit from|GUISS|Cannot customize error messages}}
{{omit from|Integer BASIC}}
{{omit from|Jack|No other output stream available.}}
{{omit from|SQL PL|It only prints in standard output. There is no way to print in standard error. Not even with "BEGIN SIGNAL SQLSTATE 'ABCDE' SET MESSAGE_TEXT = 'Hello World!'; END"}} <!-- The only way is with an external function in Java or C that prints in Standard Error -->
{{omit from|TI-83 BASIC|Same reason as TI-89.}}
{{omit from|TI-89 BASIC|no analogue to stderr, unless you count graph display vs. program IO}}
{{omit from|Unlambda|No concept of standard error (or alternate output streams in general).}}

A common practice in computing is to send error messages
to a different output stream than [[User Output - text|normal text console messages]].

The normal messages print to what is called "standard output" or "standard out".

The error messages print to "standard error".

This separation can be used to redirect error messages to a different place than normal messages.


;Task:
Show how to print a message to standard error by printing     '''Goodbye, World!'''     on that stream.





## 4DOS Batch


```4dos
echoerr Goodbye, World!
```



## AArch64 Assembly

<lang ARM_Assembly>.equ STDERR, 2
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	mov x0, #STDERR
	ldr x1, =msg
	mov x2, 15
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0 // write(stderr, msg, 15);
	ldp x29, x30, [sp], 16
	mov x0, #0
	mov x8, #SVC_EXIT
	svc #0 // exit(0);

msg:	.ascii "Goodbye World!\n"
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Goodbye_World is
begin
   Put_Line (Standard_Error, "Goodbye, World!");
end Goodbye_World;
```



## Agena


```agena
io.write( io.stderr, "Goodbye, World!\n" )
```



## Aime


```aime
v_text("Goodbye, World!\n");
```



## ALGOL 68

The procedures <tt>print</tt> and <tt>printf</tt> output to ''stand out'',
whereas <tt>put</tt> and <tt>putf</tt> can output to any open '''file''', including ''stand error''.

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - note that printf and putf were not ported into ELLA's libraries.}}

```algol68
main:(
  put(stand error, ("Goodbye, World!", new line))
)
```

{{out}}

```txt

Goodbye, World!

```



## Argile


```Argile
use std
eprint "Goodbye, World!"
```

or

```Argile
use std
eprintf "Goodbye, World!\n"
```

or

```Argile
use std
fprintf stderr "Goodbye, World!\n"
```


## ARM Assembly

{{works with|as|Raspberry Pi}}


```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program hellowordLP.s   */
.data
szMessage: .asciz "Goodbye world. \n "       @ error message
.equ LGMESSAGE, . -  szMessage  @ compute length of message

.text
.global main
main:
    mov r0, #2                  @ output error linux
    ldr r1, iAdrMessage         @ adresse of message
    mov r2, #LGMESSAGE          @ sizeof(message)
    mov r7, #4                  @ select system call 'write'
    swi #0                      @ perform the system call

    mov r0, #0                  @ return code
    mov r7, #1                  @ request to exit program
    swi #0                       @ perform the system call
iAdrMessage: .int szMessage


```



## Arturo


```arturo
panic "Goodbye, World!"
```



## ATS


```ATS
implement main0 () = fprint (stderr_ref, "Goodbye, World!\n")
```



## AutoHotkey

requires [http://github.com/tinku99/ahkdll/tree/master AutoHotkey_N]
implementation.

```autohotkey>; c:\>  autohotkey.exe stderr.ahk 2
 error.txt
FileAppend, Goodbye`, World!, stderr   ; requires AutoHotkey_N
```


Or with the current AutoHotkey_L:
{{works with|AutoHotkey_L}}
(documentation on this behavior: http://www.autohotkey.net/~Lexikos/AutoHotkey_L/docs/commands/FileAppend.htm)

```AutoHotkey
FileAppend, Goodbye`, World!, *
```



## AutoIt


```AutoIt
ConsoleWriteError("Goodbye, World!" & @CRLF)
```



## AWK

To print a message to standard error,
pipe it through a shell command:


```awk
BEGIN {
  print "Goodbye, World!"| "cat 1>&2"
}
```


Or write to /dev/stderr:

{{works with|gawk}}
{{works with|mawk}}
{{works with|nawk}}

```awk
BEGIN {
  print "Goodbye, World!" > "/dev/stderr"
}
```


With ''gawk'', ''mawk'' and ''nawk'': a special feature
associates "/dev/stderr" with standard error.
The manuals of ''gawk'' and ''mawk'' describe this feature;
''nawk'' also has this feature.

Other implementations might try to open /dev/stderr as a file.
Some Unix clones, like [[BSD]], have a /dev/stderr device node
that duplicates standard error, so this code would still work.
Some systems have no such device node, so this code would fail.
We recommend "cat 1>&2", which is more portable, and works with any Unix clone.


## BASIC


=
## BaCon
=

```freebasic
EPRINT "Goodbye, World!"
```


=
## ZX Spectrum Basic
=

On the ZX Spectrum, standard error is on stream 1:


```zxbasic

10 PRINT #1;"Goodbye, World!"
20 PAUSE 50: REM allow time for the user to see the error message

```



## Batch File


```dos>1
&2 echo Goodbye, World!
```

The redirection operator <code>1>&2</code> causes all output on stream 1 (standard out) to be redirected to stream 2 (standard error).
The redirection can be moved to the end of the line, too.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
The program must be compiled as a console application for this to work.

```bbcbasic
      STD_ERROR_HANDLE = -12
      SYS "GetStdHandle", STD_ERROR_HANDLE TO @hfile%(1)
      PRINT #13, "Goodbye, World!"
      QUIT
```



## C

Unlike puts(), fputs() does not append a terminal newline.

```c
#include <stdio.h>

int main()
{
	fprintf(stderr, "Goodbye, ");
	fputs("World!\n", stderr);

	return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
static class StdErr
{
    static void Main(string[] args)
    {
        Console.Error.WriteLine("Goodbye, World!");
    }
}
```



## C++


```cpp
#include <iostream>

int main() {
  std::cerr << "Goodbye, World!\n";
}
```



## Clojure


```lisp
(binding [*out* *err*]
  (println "Goodbye, world!"))
```



## CMake

Most messages go to standard error.


```cmake
message("Goodbye, World!")
```


The message cannot be a keyword; <code>message("STATUS")</code> never prints "STATUS", but <code>message("" "STATUS")</code> does work.


## COBOL

Using fixed format.
{{works with|OpenCOBOL}}


```cobol
	program-id. ehello.
	procedure division.
		display "Goodbye, world!"  upon syserr.
		stop run.
```



## CoffeeScript

{{trans|JavaScript}}
{{works with|Node.js}}

```coffeescript
console.warn "Goodbye, World!"
```



## Common Lisp


```lisp
(format *error-output* "Goodbye, world!~%")
```



## D


```d
import std.stdio;

void main () {
    stderr.writeln("Goodbye, World!");
}
```


### Alternative Version

{{libheader|tango}}


```d
import tango.io.Stdout;

void main () {
    Stderr("Goodbye, World!").newline;
}
```


=={{header|Déjà Vu}}==

```dejavu
!write-fragment!stderr !encode!utf-8 "Goodbye, World!\n"
```



## Delphi


```Delphi
program Project1;

{$APPTYPE CONSOLE}

begin
  WriteLn(ErrOutput, 'Goodbye, World!');
end.
```



## Dylan.NET


{{works with|Mono|2.6.7}}
{{works with|Mono|2.10.x}}
{{works with|Mono|3.x.y}}
{{works with|.NET|3.5}}
{{works with|.NET|4.0}}
{{works with|.NET|4.5}}
One Line version:

```Dylan.NET
Console::get_Error()::WriteLine("Goodbye World!")
```

Goodbye World Program:

```Dylan.NET

//compile using the new dylan.NET v, 11.5.1.2 or later
//use mono to run the compiler
#refstdasm mscorlib.dll

import System

assembly stderrex exe
ver 1.1.0.0

class public Program

   method public static void main()
      Console::get_Error()::WriteLine("Goodbye World!")
   end method

end class

```



## E



```e
stderr.println("Goodbye, World!")
```



## Elixir


```elixir
IO.puts :stderr, "Goodbye, World!"
```



## Emacs Lisp


```Emacs Lisp

(error "Goodbye, World!")

```

<b>Output:</b>

```txt

Goodbye, World!

```



## Erlang


```erlang
io:put_chars(standard_error, "Goodbye, World!\n").
```



## Euphoria


```Euphoria
puts(2,"Goodbye, world!\n") -- 2 means output to 'standard error'
```


=={{header|F_Sharp|F#}}==

```fsharp
eprintfn "%s" "Goodbye, World!"
```

or you can use the .Net classes

```fsharp
System.Console.Error.WriteLine("Goodbye, World!");
```



## Factor

Start Factor in a terminal for this:

```factor
error-stream get [ "Goodbye, World! bbl, crashing" print flush ] with-output-stream*
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Env.cur.err.printLine ("Goodbye, World!")
  }
}

```



## Forth

{{works with|GNU Forth}}

```forth
outfile-id
  stderr to outfile-id
  ." Goodbye, World!" cr
to outfile-id
```



## Fortran


Normally standard error is associated with the unit 0
but this could be different for different vendors.
Therefore since Fortran 2003 there's an intrinsic module
which defines the parameter ERROR_UNIT.


```fortran
program StdErr
  ! Fortran 2003
  use iso_fortran_env

  ! in case there's no module iso_fortran_env ...
  !integer, parameter :: ERROR_UNIT = 0

  write (ERROR_UNIT, *) "Goodbye, World!"
end program StdErr
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open Err As #1
Print #1, "Goodbye World!"
Close #1
Sleep
```



## Frink


```frink

staticJava["java.lang.System","err"].println["Goodbye, World!"]

```



## Genie


```genie
[indent=4]
/*
  Hello, to Standard error, in Genie
  valac helloStderr.gs
*/

init
    stderr.printf("%s\n", "Goodbye, World!")
```


{{out}}

```txt
prompt$ ./helloStderr | wc
Goodbye, World!
      0       0       0
```



## Go

Built in println now goes to stderr.

```go
package main
func main() { println("Goodbye, World!") }
```

but the builtin print() and println() functions are not guaranteed to stay in the language. So you should probably use

```go
package main
import ("fmt"; "os")
func main() { fmt.Fprintln(os.Stderr, "Goodbye, World!") }
```



## Groovy


```groovy
System.err.println("Goodbye, World!")
```



## Haskell


```haskell
import System.IO
main = hPutStrLn stderr "Goodbye, World!"
```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}" "${@}"
#! huginn

import OperatingSystem as os;

main() {
	os.stderr().write( "Goodbye, World!\n" );
	return ( 0 );
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
  write(&errout, "Goodbye World" )
end
```



## J


```j
stderr =: 1!:2&4
stderr 'Goodbye, World!'
```



## Java


```java
public class Err{
   public static void main(String[] args){
      System.err.println("Goodbye, World!");
   }
}
```



## JavaScript

{{works with|JScript}} and only with <code>cscript.exe</code>

```javascript
WScript.StdErr.WriteLine("Goodbye, World!");
```


{{works with|Node.js}}

```javascript
console.warn("Goodbye, World!")
```


{{works with|Firefox}}

```javascript
console.error("Goodbye, World!")//only works if console object exists
```

OR

```javascript
throw new Error("Goodbye, World!")//Should work in any browser
```



## jq



```jq
jq -n —-arg s 'Goodbye, World!' '$s | stderr | empty'
```


`stderr` copies its input to STDERR before passing it along the pipeline, and hence the occurrence of `empty` above.


## Julia


```julia
println(STDERR, "Goodbye, World!")
```



## Kotlin


```scala
fun main(args: Array<String>) {
    System.err.println("Goodbye, World!")
}
```



## Lasso


```Lasso
define stderr(s::string) => {
    file_stderr->writeBytes(#s->asBytes)
}

stderr('Goodbye, World!')
```



## Lingo

*Windows:
{{libheader|CommandLine Xtra}}

```lingo
-- print to standard error
stdErr("Goodbye, World!", TRUE)

-- print to the Windows debug console (shown in realtime e.g. in Systernal's DebugView)
dbgPrint("Goodbye, World!")
```


*Mac OS X:
{{libheader|Shell Xtra}}

```lingo
sx = xtra("Shell").new()

-- print to standard error
sx.shell_cmd("echo Goodbye, World!>&2")

-- print to system.log (shown in realtime e.g. in Konsole.app)
sx.shell_cmd("logger Goodbye, World!")
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

%struct._iobuf = type { i8* }

$"message" = comdat any
@"message" = linkonce_odr unnamed_addr constant [17 x i8] c"Goodbye, world!\0A\00", comdat, align 1

;-- For discovering stderr (io pipe 2)
declare %struct._iobuf* @__acrt_iob_func(i32)

;--- The declaration for the external C fprintf function.
declare i32 @fprintf(%struct._iobuf*, i8*, ...)

define i32 @main() {
;-- load stderr
  %1 = call %struct._iobuf* @__acrt_iob_func(i32 2)
;-- print the message to stderr with fprintf
  %2 = call i32 (%struct._iobuf*, i8*, ...) @fprintf(%struct._iobuf* %1, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @"message", i32 0, i32 0))
;-- exit
  ret i32 0
}
```

{{out}}

```txt
Goodbye, world!
```



## Logtalk

The stream alias "user_error" can be used to print to the "standard error" stream.

```logtalk

:- object(error_message).

    % the initialization/1 directive argument is automatically executed
    % when the object is compiled and loaded into memory:
    :- initialization(write(user_error, 'Goodbye, World!\n')).

:- end_object.

```



## Lua


```lua
io.stderr:write("Goodbye, World!\n")
```



## m4


```m4
errprint(`Goodbye, World!
')dnl
```



## MANOOL


```MANOOL
{{extern "manool.org.18/std/0.3/all"} in Err.WriteLine["Goodbye, World!"]}
```



## Maple


```Maple

error "Goodbye World"

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Write[Streams["stderr"], "Goodbye, World!"]
```


=={{header|MATLAB}} / {{header|Octave}}==
This prints to standard error, and continues execution

```MATLAB
fprintf(2,'Goodbye, World!')
```

This will not stop further execution, if called from within a script or function.

```MATLAB
error 'Goodbye, World!'
```



## Mercury

<lang>
:- module hello_error.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.stderr_stream(Stderr, !IO),
    io.write_string(Stderr, "Goodbye, World!\n", !IO).

```



## Metafont

Metafont has no a real way to send a text to the standard output/error nor to a file. Anyway it exists the <code>errmessage</code> command which will output an error message and prompt the user for action (suspending the interpretation of the source).


```metafont
errmessage "Error";
message "...going on..."; % if the user decides to go on and not to stop
                          % the program because of the error.
```



## ML/I


```ML/I
MCSET S4=1
MCNOTE Goodbye, World!
```


=={{header|Modula-2}}==

```modula2
MODULE HelloErr;
IMPORT StdError;

BEGIN
  StdError.WriteString('Goodbye, World!');
  StdError.WriteLn
END HelloErr.
```


=={{header|Modula-3}}==

```modula3
MODULE Stderr EXPORTS Main;

IMPORT Wr, Stdio;

BEGIN
  Wr.PutText(Stdio.stderr, "Goodbye, World!\n");
END Stderr.
```



## N/t/roff


The request <code>.tm</code> prints whatever after it, until and including the newline character, to the standard error.  The string parsed to it need not be quoted and will never appear on standard output.


```N/t/roff

.tm Goodbye, World!

```



## Neko


```ActionScript
/**
  Hello world, to standard error, in Neko
  Tectonics:
    nekoc hello-stderr.neko
    neko hello-stderr
*/

/* Assume stderr is already open, just need write */
var file_write = $loader.loadprim("std@file_write", 4);

/* Load (and execute) the file_stderr primitive */
var stderr = $loader.loadprim("std@file_stderr", 0)();

file_write(stderr, "Goodbye, World!\n", 0, 16);
```


{{out}}

```txt
prompt$ nekoc hello-stderr.neko
prompt$ neko hello-stderr
Goodbye, World!
prompt$ neko hello-stderr 2>/dev/null
prompt$
```



## Nemerle


```Nemerle
System.Console.Error.WriteLine("Goodbye, World!");
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

System.err.println("Goodbye, World!")

```



## Nim


```nim
stderr.writeln "Hello World"
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE HelloErr;
IMPORT Err;
BEGIN
	Err.String("Goodbye, World!");Err.Ln
END HelloErr.

```

{{out}}

```txt

Goodbye, World!

```


=={{header|Objective-C}}==

{{Works with|GNUstep}}
{{Works with|Cocoa}}

In Objective-C one can use the standard C library and the stderr as in the C language; nonetheless a common way to output to stderr for logging purpose and/or error notification is the <tt>NSLog</tt> function, that works almost like <tt>fprintf(stderr, "...")</tt>, save for the fact that the format string
is an NSString object, and it also prepends a timestamp.


```objc>#import <Foundation/Foundation.h


int main()
{
   fprintf(stderr, "Goodbye, World!\n");
   fputs("Goodbye, World!\n", stderr);
   NSLog(@"Goodbye, World!");
   return 0;
}
```



## OCaml


```ocaml
prerr_endline "Goodbye, World!";    (* this is how you print a string with newline to stderr *)
Printf.eprintf "Goodbye, World!\n"; (* this is how you would use printf with stderr *)
```


we can also use the ''out_channel'' '''stderr''':


```ocaml
output_string stderr "Goodbye, World!\n";
Printf.fprintf stderr "Goodbye, World!\n";
```


finally the [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html Unix] module also provides unbuffered write functions:


```ocaml
let msg = "Goodbye, World!\n" in
ignore(Unix.write Unix.stderr msg 0 (String.length msg)) ;;
```



## Octave


```octave
fprintf(stderr, "Goodbye, World!\n");
```



## Oforth


```Oforth
System.Err "Goodbye, World!" << cr
```



## Ol


```scheme

(print-to stderr "Goodbye, World!")

```



## ooRexx

ooRexx provides a .error object that writes output to the standard error stream.

```ooRexx
.error~lineout("Goodbye, World!")
```

The .error object is a proxy that delegates to a backing stream, so this might be redirected.
By default, this delegates to the .stderr object, which can also be used directly.

```ooRexx
.stderr~lineout("Goodbye, World!")
```

or in 'Classic REXX style'

```ooRexx
/* REXX ---------------------------------------------------------------
* 07.07.2014 Walter Pachl
* 12.07.2014 WP see Discussion where redirection from within the program is shown
*--------------------------------------------------------------------*/
Say 'rexx serr 2>err.txt directs the stderr output to the file err.txt'
Call lineout 'stderr','Good bye, world!'
Call lineout ,'Hello, world!'
Say 'and this is the error output:'
'type err.txt'
```



## Oz


```oz
functor
import Application System
define
   {System.showError "Goodbye, World!"}
   {Application.exit 0}
end
```



## PARI/GP


```parigp
error("Goodbye, World!")
```



## Pascal

{{Works with|Free Pascal}}

```pascal
program byeworld;

begin
  writeln(StdErr, 'Goodbye, World!');
end.
```



## Perl


```perl
warn "Goodbye, World!\n";
```


Or:


```perl
print STDERR "Goodbye, World!\n";
```



## Perl 6


```perl6
note "Goodbye, World!";
```



## Phix


```Phix
puts(2,"Goodbye, World!\n")
```



## PHP


```php
fprintf(STDERR, "Goodbye, World!\n");
```

or

```php
file_put_contents("php://stderr","Hello World!\n");
```



## PicoLisp


```PicoLisp
(out 2 (prinl "Goodbye, World!"))
```



## PL/I


```PL/I
display ('Goodbye, World');
```



## PostScript


```postscript
(%stderr) (w) file dup
(Goodbye, World!
) writestring
closefile
```



## PowerBASIC

{{works with|PowerBASIC Console Compiler}}


```powerbasic
STDERR "Goodbye, World!"
```



## PowerShell

Since PowerShell has a slightly different system of pipes and streams
(to facilitate easy usage from a host application) the
standard Write-Error cmdlet is mainly for sending annotated error messages
to the host:

```powershell
Write-Error "Goodbye, World!"
```

Note that this outputs more than just the message,
because behind the scenes it is an uncaught exception:

```txt
Write-Error "Goodbye, World!" : Goodbye, World!
    + CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
    + FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException
```

To accurately reproduce the behavior of other languages one has to resort to .NET in this case:

```powershell
[Console]::Error.WriteLine("Goodbye, World!")
```



## PureBasic


[http://www.purebasic.com/documentation/console/consoleerror.html ConsoleError()] writes the message string (plus a newline) to the standard error output of current program.

Standard error output can be used in conjunction with [http://www.purebasic.com/documentation/process/readprogramerror.html ReadProgramError()] to reads a line from an other programs error output (stderr).


```PureBasic
ConsoleError("Goodbye, World!")
```



## Python

{{works with|Python|2.x}}

```python
import sys

print >> sys.stderr, "Goodbye, World!"
```


{{works with|Python|3.x}}

```python
import sys

print("Goodbye, World!", file=sys.stderr)
```


Works with either:

```python
import sys

sys.stderr.write("Goodbye, World!\n")
```



## R


```R
cat("Goodbye, World!", file=stderr())
```



## Ra


```Ra

class HelloWorld
	**Prints "Goodbye, World!" to standard error**

	on start

		print to Console.error made !, "Goodbye, World!"

```



## Racket


```Racket

(eprintf "Goodbye, World!\n")

```



## Retro


```Retro
'Goodbye,_World! '/dev/stderr file:spew
```



## REXX


### version 1

This version will work with those operating systems (hosts)
that support stream output and a STDERR output

stream (by name).

If the   '''stderr'''   name is supported and enabled, the output is written to the terminal.

If not supported or disabled, the output is written to a (disk) file named   '''STDERR'''.

```rexx
call lineout 'STDERR', "Goodbye, World!"
```



### version 2

Same as above, but uses a different style and also invokes   '''charout'''   instead of   '''lineout'''.

```rexx
msgText = 'Goodbye, World!'
call charout 'STDERR', msgText
```



### version 3

this works on Windows 7 and ooRexx and REGINA

```rexx
/* REXX ---------------------------------------------------------------
* 07.07.2014 Walter Pachl
* enter the appropriate command shown in a command prompt.
*    "rexx serr.rex 2>err.txt"
* or "regina serr.rex 2>err.txt"
* 2>file will redirect the stderr stream to the specified file.
* I don't know any other way to catch this stream
*--------------------------------------------------------------------*/
Parse Version v
Say v
Call lineout 'stderr','Good bye, world!'
Call lineout ,'Hello, world!'
Say 'and this is the error output:'
'type err.txt'
```



## Ring


```ring
fputs(stderr,"Goodbye, World!")
```



## Ruby


```ruby
$stderr.puts "Goodbye, World!"
```

The following also works, unless you have disabled warnings (ruby command line option "-W0" or set <code>$VERBOSE=nil</code>)

```ruby
warn "Goodbye, World!"
```



## Run BASIC


```runbasic
html "<script>
window.open('','error_msg','');
document.write('Goodbye, World!');
</script>""
```

Run Basic runs in a browser.
This opens a new browser window,
or a tab in the case of Chrome and some others.


## Rust


```rust
fn main() {
    eprintln!("Hello, {}!", "world");
}
```

or

```rust
fn main() {
    use ::std::io::Write;
    let (stderr, errmsg) = (&mut ::std::io::stderr(), "Error writing to stderr");
    writeln!(stderr, "Bye, world!").expect(errmsg);

    let (goodbye, world) = ("Goodbye", "world");
    writeln!(stderr, "{}, {}!", goodbye, world).expect(errmsg);
}
```

or

```rust
fn main() {
    use std::io::{self, Write};

    io::stderr().write(b"Goodbye, world!").expect("Could not write to stderr");
    // With some finagling, you can do a formatted string here as well
    let goodbye = "Goodbye";
    let world = "world";
    io::stderr().write(&*format!("{}, {}!", goodbye, world).as_bytes()).expect("Could not write to stderr");
    // Clearly, if you want formatted strings there's no reason not to just use writeln!
}
```


=={{header|S-lang}}==
<lang S-lang>() = fputs("Goodbye, World!\n", stderr);
```



## Salmon



```Salmon

standard_error.print("Goodbye, World!\n");

```


or


```Salmon

include "short.salm";
stderr.print("Goodbye, World!\n");

```


or


```Salmon

include "shorter.salm";
err.print("Goodbye, World!\n");

```

or


```Salmon

include "shorter.salm";
se.print("Goodbye, World!\n");

```



## Sather


```sather
class MAIN is
  main is
    #ERR + "Hello World!\n";
  end;
end;
```



## Scala

{{libheader|Console}}


### Ad hoc REPL solution

Ad hoc solution as [http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL] script:

```Scala
Console.err.println("Goodbye, World!")
```



### Via Java runtime

This is a call to the Java run-time library. '''Not recommendated'''.

```Scala
System.err.println("Goodbye, World!")
```



### Via Scala Console API

This is a call to the Scala API. '''Recommendated'''.

```Scala
Console.err.println("Goodbye, World!")
```



### Short term deviation to err


```Scala
Console.withOut(Console.err) { println("This goes to default _err_") }
```



### Long term deviation to err


```Scala
  println ("Out not deviated")
  Console.setOut(Console.err)
  println ("Out deviated")
  Console.setOut(Console.out) // Reset to normal
```



## Scilab


```scilab
error("Goodbye, World!")
```



## Scheme


```scheme
(error "Goodbye, World!")
```



## sed

Requires <tt>/dev/stderr</tt>


```sed
#n
1 {
	s/.*/Goodbye, World!/w /dev/stderr
}
```


This program requires at least 1 line of input.
It changes the first line to "Goodbye, World!"
and then prints the first line to standard error.
It reads and ignores the remaining lines.

{{out|Test output}}

```bash
$ echo a | sed -f error.sed >/dev/null
Goodbye, World!
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(STD_ERR, "Goodbye, World!");
  end func;
```



## Sidef


```ruby
STDERR.println("Goodbye, World!");
```



## Slate


```slate
inform: 'Goodbye, World!' &target: DebugConsole.
```



## Smalltalk

The details on to which name stderr is bound may vary between Smalltalk dialects. If different, a "Smalltalk at:#Stderr put:<name your stream here>" should provide compatibility.


```smalltalk
Stderr nextPutAll: 'Goodbye, World!'
```


However, all smalltalks provide a console named "Transcript", where diagnostics is usually sent to. Thus:


```smalltalk
Transcript show: 'Goodbye, World!'
```


will work on all, and is the preferred way to do this.
And yes, when operating UI-less, the global "Transcript" is usually bound to the stderr stream.


## SNOBOL4


```snobol4
        terminal = "Error"
        output = "Normal text"
end
```



## Standard ML


```sml
TextIO.output (TextIO.stdErr, "Goodbye, World!\n")
```



## Swift


```Swift
import Foundation

let out = NSOutputStream(toFileAtPath: "/dev/stderr", append: true)
let err = "Goodbye, World!".dataUsingEncoding(NSUTF8StringEncoding, allowLossyConversion: false)
out?.open()
let success = out?.write(UnsafePointer<UInt8>(err!.bytes), maxLength: err!.length)
out?.close()

if let bytes = success {
    println("\nWrote \(bytes) bytes")
}
```

{{out}}

```txt

Goodbye, World!
Wrote 15 bytes

```



## Tcl


```tcl
puts stderr "Goodbye, World!"
```


=={{header|Transact-SQL}}==
<lang Transact-SQL> RAISERROR 'Goodbye, World!', 16, 1
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT/ERROR "hello world"
text="goodbye world"
PRINT/ERROR text

```

{{out}}

```txt

@@@@@@@@  hello world                                                  @@@@@@@@
@@@@@@@@  goodbye world                                                @@@@@@@@

```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
echo "Goodbye, World!" >&2
```


=
## C Shell
=

```csh
echo "Goodbye, World!" >/dev/stderr
```


This requires <code>/dev/stderr</code>, a device node from [[BSD]]
and some other Unix clones.
This command works with both Bourne Shell and C Shell.


## Ursa


```ursa
out "goodbye, world!" endl console.err
```



## VBA


```VB

Sub StandardError()
    Debug.Print "Goodbye World!"
End Sub

```



## VBScript

Must work in cscript.exe

```vb
WScript.StdErr.WriteLine "Goodbye, World!"
```



## Verbexx


```verbexx
@STDERR "Goodbye, World!\n";
```



## Visual Basic .NET


```vbnet
Module Module1

    Sub Main()
        Console.Error.WriteLine("Goodbye, World!")
    End Sub

End Module
```



## WDTE


```WDTE
io.writeln io.stderr 'Goodbye, World!';
```



## X86 Assembly

{{works with|nasm|2.05.01}}

This is known to work on Linux, it may or may not work on other Unix-like systems

Note that it is only 2 characters different from the Assembly example on [[User Output - text]]

Prints "Goodbye, World!" to stderr (and there is probably an even simpler version):

```asm
section .data
msg     db      'Goodbye, World!', 0AH
len     equ     $-msg

section .text
global  _start
_start: mov     edx, len
        mov     ecx, msg
        mov     ebx, 2
        mov     eax, 4
        int     80h

        mov     ebx, 1
        mov     eax, 1
        int     80h
```



## XLISP


```xlisp
(DISPLAY "Goodbye, World!" *ERROR-OUTPUT*)
```



## XPL0

The terms "standard output" and "standard error" are not used,
but it's trivial to send messages to a variety of devices
by specifying their numbers.
Normally messages are displayed on the text console, which is device 0.
Instead, this example sends the message to the (first) printer,
which is device 2.


```XPL0
code Text=12;
Text(2, "Goodbye, World!")
```



## zkl


```zkl
File.stderr.writeln("Goodbye, World!")
```

