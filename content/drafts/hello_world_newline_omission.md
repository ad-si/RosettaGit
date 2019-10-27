+++
title = "Hello world/Newline omission"
description = ""
date = 2019-09-30T17:06:31Z
aliases = []
[extra]
id = 10150
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}

Some languages automatically insert a newline after outputting a string, unless measures are taken to prevent its output. 


;Task:
Display the string   <big><big><code>Goodbye, World!</code></big></big>   without a trailing newline.


;Related tasks:
*   [[Hello world/Graphical]]
*   [[Hello world/Line Printer]]
*   [[Hello world/Standard error]]
*   [[Hello world/Text]]





## ACL2


```lisp
(cw "Goodbye, World!")
```



## Ada

This example will implicitly include a final, implementation defined, terminator (usually a linefeed) if the output is a file (RM A.10.7-8) such as <code>stdout</code> on UNIX systems.

```ada

with Ada.Text_IO;

procedure Goodbye_World is
begin
   Ada.Text_IO.Put("Goodbye, World!");
end Goodbye_World;

```

Using <code>Ada.Text_IO.Text_Streams</code> instead allows us to control the termination.

```ada

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

procedure Goodbye_World is
    stdout: Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
begin
    String'Write(Ada.Text_IO.Text_Streams.Stream(stdout), "Goodbye World");
end Goodbye_World;

```



## Agena


```agena
io.write( "Goodbye, World!" )
```



## ALGOL 68

This works with Algol68 Genie 2.8.2 and above.  Earlier versions appended a gratuitous newline on unflushed output when the program terminated.

```algol68
BEGIN
   print ("Goodbye, World!")
END
```



## Arturo


```arturo
print "Goodbye, World!" true
```



## ATS


```ATS
implement main0 () = print "Goodbye, World!"
```



## AutoHotkey


```AHK
DllCall("AllocConsole")
FileAppend, Goodbye`, World!, CONOUT$ ; No newline outputted
MsgBox
```



## AutoIt


```AutoIt

ConsoleWrite("Goodbye, World!")

```



## AWK


```AWK

BEGIN { printf("Goodbye, World!") }

```



## Axe


```axe
Disp "Goodbye, World!"
```



## B

{{works with|The Amsterdam Compiler Kit - B|V6.1pre1}}                                                

```B
main()
{
    putstr("Goodbye, World!");
    return(0);
}
```



## BASIC


```basic
10 REM The trailing semicolon prevents a newline
20 PRINT "Goodbye, World!";
```


=
## BaCon
=
BaCon supports BASIC PRINT ending with trailing semicolon to prevent a newline and also supports a FORMAT clause that uses ''printf'' specifications and special character escapes (with no \n, there is no newline).

```freebasic
PRINT "Goodbye, World!";
PRINT "Goodbye, World!" FORMAT "%s"
```


=
## Applesoft BASIC
=

```ApplesoftBasic
PRINT "GOODBYE, WORLD!";
```


=
## Commodore BASIC
=

```basic
10 print chr$(14) : rem Switch to lower+uppercase character set
20 print "Goodbye, World!";
30 rem * If we end this program here, we will not see the effect because 
40 rem   BASIC will print 'READY' at a new line anyway.
50 rem * So, we just print additional message...
60 print "(End of the world)"
70 end
```

'''Output:'''

```txt

Goodbye, World!(End of the world)

ready.

```


=
## BASIC256
=
Output all on a single line.

```BASIC256
print "Goodbye,";
print " ";
print "World!";
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>10 PRINT "Goodbye, World! ";
```



## Batch File

'''Under normal circumstances, when delayed expansion is disabled'''<br/>
The quoted form guarantees there are no hidden trailing spaces after World!

```dos
<nul set/p"=Goodbye, World!"
<nul set/p=Goodbye, World!
```


'''If delayed expansion is enabled, then the ! must be escaped'''<br/>
Escape once if quoted form, twice if unquoted.

```dos
setlocal enableDelayedExpansion
<nul set/p"=Goodbye, World^!"
<nul set/p=Goodbye, World^^^!
```



## BBC BASIC


```bbcbasic
      REM BBC BASIC accepts the standard trailing semicolon:
      PRINT "Goodbye World!";
      
      REM One could also output the characters individually:
      GW$ = "Goodbye World!"
      FOR i% = 1 TO LEN(GW$)
        VDU ASCMID$(GW$, i%)
      NEXT
```



## Bc


```Bc
print "Goodbye, World!"
```



## beeswax


```beeswax
_`Goodbye, World!
```


beeswax prints everything without appending a newline character. beeswax has an instruction to explicitely print a newline character: <code>N</code>.


## Befunge

In Befunge, a newline has to be explicitly output when required, so you can just not include one if it's not wanted.


```befunge
"!dlroW ,eybdooG">:#,_@
```



## Bracmat



```bracmat
put$"Goodbye, World!"
```


=={{header|Brainf***}}==
One option was to copy the code from the regular Hello World version and omit the last period, but one of the nicer things about the language is that no matter how simple your program is, if it's more than a few characters long, it's probably unique. So here's yet another version of Goodbye, World in Brainf***.

```bf>>+++++[>++++>+>+>++++>>+++<<<+<+<++[>++>+++>+++>++++>+
+[<]>>-]<-]>>
+.>>+..<.--.++>>+.<<+.>>>-.>++.[<]++++[>++++<-]>.>>.+++.------.<-.[>]<+.[-]
[G   oo d  b     y   e    ,                     W  o   r      l  d     !]
```



## C

In C, we do not get a newline unless we embed one:

```c>#include <stdio.h

#include <stdlib.h>

int main(int argc, char *argv[]) {
  (void) printf("Goodbye, World!");    /* No automatic newline */
  return EXIT_SUCCESS;
}
```


However ISO C leaves it up to implementations to define whether or not the last line of a text stream requires a new-line. This means that the C can be targetted to environments where this task is impossible to implement, at least with a direct text stream manipulation like this.

=={{header|C sharp|C#}}==

```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        //Using Console.WriteLine() will append a newline
        Console.WriteLine("Goodbye, World!");

        //Using Console.Write() will not append a newline
        Console.Write("Goodbye, World!");
    }
}
```



## C++


```cpp>#include <iostream


int main() {
  std::cout << "Goodbye, World!";
  return 0;
}
```



## Clipper


```Clipper
?? "Goodbye, World!"
```



## Clojure


```clojure
(print "Goodbye, World!")
```



## COBOL


```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. GOODBYE-WORLD.

PROCEDURE DIVISION.
DISPLAY 'Goodbye, World!'
    WITH NO ADVANCING
END-DISPLAY
.
STOP RUN.
```



## CoffeeScript

Node JS:

```coffeescript
process.stdout.write "Goodbye, World!"
```



## Common Lisp


```lisp
(princ "Goodbye, World!")
```


## Creative Basic


```Creative Basic

'In a window

DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY:INT
 
GETSCREENSIZE(ScreenSizeX,ScreenSizeY)
 
WINDOW Win,0,0,ScreenSizeX,ScreenSizeY,0,0,"Goodbye program",MainHandler
 
PRINT Win,"Goodbye, World!"
'Prints in the upper left corner of the window (position 0,0).
PRINT"Win," I ride off into the sunset."

'There does not appear to be a means of starting a new line when printing in a window, other than by using the MOVE command.
'Therefore, both sentences here will print on the same line, i.e., in the same vertical position.
 
WAITUNTIL Close=1
 
CLOSEWINDOW Win
 
END
 
SUB MainHandler
 
    IF @CLASS=@IDCLOSEWINDOW THEN Close=1   
 
RETURN

'In the console

OPENCONSOLE

'Insert a trailing comma.
PRINT"Goodbye, World!",
PRINT" I ride off into the sunset."

PRINT:PRINT"Press any key to end."

DO:UNTIL INKEY$<>""

CLOSECONSOLE

'Since this a Cbasic console program.
END 

```



## D

{{works with|D|2.0}}

```D
import std.stdio;

void main() {
    write("Goodbye, World!");
}
```



## Dc


```Dc
[Goodbye, World!]P
```


```Dc>370913249815566165486152944077005857 P</lang


=={{header|Déjà Vu}}==

```dejavu
!print\ "Goodbye, World!"
```



## Delphi


```Delphi
program Project1;

{$APPTYPE CONSOLE}

begin
  Write('Goodbye, World!');
end.
```



## DWScript


```Delphi
Print('Goodbye, World!');
```



## Dyalect


```Dyalect
print("Goodbye, World!", terminator: "")
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
Console::Write("Goodbye, World!")
```

Goodbye World Program:

```Dylan.NET

//compile using the new dylan.NET v, 11.5.1.2 or later
//use mono to run the compiler
#refstdasm mscorlib.dll

import System

assembly gdbyeex exe
ver 1.2.0.0

class public Program

   method public static void main()
      Console::Write("Goodbye, World!")
   end method

end class

```



## EchoLisp


```lisp

(begin 
    (write "GoodBye, World")
    (write "Next on same line"))

```


## Elena

ELENA 4.x:

```elena
public program()
{
    //print will not append a newline
    console.write("Goodbye, World!")
}
```



## Elixir


```elixir

IO.write "Goodbye, World!"

```




## Emacs Lisp


```Emacs Lisp

(insert "Goodbye, World!")
 
```

<b>Output:</b>

```txt

Goodbye, World!                                                                 

```



## Erlang

In erlang a newline must be specified in the format string.

```erlang
io:format("Goodbye, world!").
```



## ERRE


```ERRE

.......
PRINT("Goodbye, World!";)
.......

```



## Euphoria


```euphoria
-- In Euphoria puts() does not insert a newline character after outputting a string
puts(1,"Goodbye, world!")
```


=={{header|F Sharp|F#}}==

```fsharp

// A program that will run in the interpreter (fsi.exe)
printf "Goodbye, World!";;

// A compiled program
[<EntryPoint>]
let main args =
    printf "Goodbye, World!"
    0 

```



## Factor


```factor
USE: io
"Goodbye, World!" write
```



## Falcon

With the print() function:

```falcon
print("Goodbye, World!")
```

Or via "fast print":

```falcon>>
 "Goodbye, World!"
```



## Fantom



```fantom

class Main {
  Void main() {
    echo("Goodbye, World!")
  }
}

```



## FOCAL

FOCAL does not insert a newline unless we specifically request one.

```focal
TYPE "Goodbye, World!"
```



## Forth


```Forth
\ The Forth word ." does not insert a newline character after outputting a string
." Goodbye, World!"
```



## Fortran


```Fortran
program bye
  write (*,'(a)',advance='no') 'Goodbye, World!'
end program bye
```


The "advance" facility was introduced with F90, as was the ability to specify format instructions (the <code>'(A)'</code> part) without a separate FORMAT statement. Earlier, there was a common extension:

```Fortran
      WRITE (6,1) "Goodbye, World!"
    1 FORMAT (A,$)
      END
```

In this, the FORMAT instruction is to accept alphabetic text (the A) from the WRITE statement, followed by the special $ item (of no mnemonic form) which signified that there was not to be any new line action at the end of the output. This sort of thing is useful when writing a prompt to the screen so that the input of the response appears on the same screen line. The text could also have been incorporated into the FORMAT statement, which would be useful if there were many WRITE statements scattered about that were to send forth the same text. 

These facilities only became of interest when, instead of card decks and lineprinters, I/O involved a keyboard and screen with both input and output appearing on the same screen. Thus, in earlier Fortran usage, the issue would not arise for output to a lineprinter, because it was already the case: a line written to the lineprinter was ''not'' followed by a end-of-line/start-new-line sort of action by the lineprinter. It stayed put on the line just written. It was the ''following'' output to the lineprinter that would state "advance one" (or two, or, no) lines at the ''start'' of its output. This was the "carriage control character", and a 1 signified "skip to top-of-form" which is to say, start a new page.

In other words, the Fortran approach for output was <carriage control><output text> rather than the <output text><carriage control> sequence, that now has to be suppressed by the "advance = 'no'" facility.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Print "Goodbye, World!";  '' the trailing semi-colon suppresses the new line
Sleep
```



## Frink


```Frink
print["Goodbye, World!"]
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=09c8c3464c556325f089f9e4c326eaca Click this link to run this code]'''

```gambas
Public Sub Main()

Print "Goodbye, "; 'The semicolon stops the newline being added
Print "World!"

End
```

Output:

```txt

Goodbye, World!

```


## gecho

{{incorrect|gecho|output isn't consistent with the task's requirements: wording, capitalization of the 2nd word.}}

```gecho
'Hello, <> 'world! print
```



## Genie


```genie
[indent=4]
/*
  Hello, with no newline, in Genie
  valac helloNoNewline.gs
*/

init
    stdout.printf("%s", "Goodbye, World!")
```


{{out}}

```txt
prompt$ valac helloNoNewline.gs
prompt$ ./helloNoNewline
Goodbye, World!prompt$
```



## GML


```lisp
show_message("Goodbye, World!")
```



## Go


```go
package main

import "fmt"

func main() { fmt.Print("Goodbye, World!") }
```



## Groovy



```groovy
print "Goodbye, world"
```



## GUISS

In Graphical User Interface Support Script, we specify a newline, if we want one. The following will not produce a newline:

```GUISS
Start,Programs,Accessories,Notepad,Type:Goodbye World[pling]
```



## Harbour


```visualfoxpro
?? "Goodbye, world"
or
QQout( "Goodbye, world" )

```



## Haskell



```haskell
main = putStr "Goodbye, world"
```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}" "${@}"
#! huginn

main() {
	print( "Goodbye, World!" );
	return ( 0 );
}
```


=={{header|Icon}} and {{header|Unicon}}==
Native output in Icon and Unicon is performed via the ''write'' and ''writes'' procedures. The ''write'' procedure terminates each line with both a return and newline (for consistency across platforms). The ''writes'' procedure omits this.  Additionally, the programming library has a series of ''printf'' procedures as well.

```Icon
procedure main()
   writes("Goodbye, World!")    
end
```



## HolyC


```holyc
"Goodbye, World!";
```


== {{header|Io}}==

```io

write("Goodbye, World!")

```



## IWBASIC


```IWBASIC

'In a window

DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY:UINT
 
GETSCREENSIZE(ScreenSizeX,ScreenSizeY)
 
OPENWINDOW Win,0,0,ScreenSizeX,ScreenSizeY,NULL,NULL,"Goodbye program",&MainHandler
 
PRINT Win,"Goodbye, World!"
'Prints in upper left corner of the window (position 0,0).
PRINT Win," You won't have this program to kick around anymore."

'There does not appear to be a means of starting a new line when printing in a window, other than by using the MOVE command.
'Therefore, both sentences here will print on the same line, i.e., in the same vertical position.
 
WAITUNTIL Close=1
 
CLOSEWINDOW Win
 
END
 
SUB MainHandler
 
    IF @MESSAGE=@IDCLOSEWINDOW THEN Close=1
 
RETURN
ENDSUB

'In the console

OPENCONSOLE

'by inserting a trailing comma.
PRINT"Goodbye, World!",
PRINT" You won't have this program to kick around anymore."

PRINT:PRINT

'A press any key to continue message is automatic in a program compiled as console only.
'I presume the compiler adds the code.  
CLOSECONSOLE

'Since this an IWBASIC console program.
END 

```



## J

On a linux system, you can use 1!:3 because stdout is a file:

```j
   'Goodbye, World!' 1!:3 <'/proc/self/fd/1'
Goodbye, World!   
```

However, J works in environments other than Linux, so...
'''Solution''':<code>prompt</code> from the misc package.
'''Example''':
```j
   load 'general/misc/prompt'
   prompt 'Goodbye, World!'
Goodbye, World!
```

'''Notes''':  J programs are normally run from a REPL, or session manager, which comes in several flavors.  The traditional commandline-based terminal (jconsole), one of several desktop applications (jqt for the current version of J, jgtk and jwd for older but still supported versions), a web-based frontend (jhs), and various mobile apps (J for iOS, Android).  

The specific session manager being used changes the context and therefore answer to this task.  For example, when using J from a browser (including mobile browsers) newlines are omitted by default.  Further, J provides strong tools for coalescing results and manipulating them prior to output, so newline elimination would typically happen before output rather than after.

With that said, <code>prompt</code> handles the most common cases (using binary output for jconsole, so no newline is appended; adjusting the REPL prompt in the desktop apps to to elide the newline which is normally included by default, etc).  

For truly automated processes, you'd almost always want this kind of functionality (omitting the newline when printing) in a file- or stream-oriented application.  For those cases, the simple <code>text 1!:3 file</code> will append the text to the referenced file verbatim, without inserting any extra newlines.

So, if a J programmer were asked to solve this task, the right approach would be to ask why that is needed, and then craft a solution appropriate to that situation.


## Jack


```jack
class Main {
  function void main () {
    do Output.printString("Goodbye, World!");und
    return;
  }
}
```



## Java


```java
public class HelloWorld
{
 public static void main(String[] args)
 {
  System.out.print("Goodbye, World!");
 }
}
```



## JavaScript

Node JS:

```javascript
process.stdout.write("Goodbye, World!");
```



## jq

The "-j" command-line option suppresses the newline that would otherwise be printed, e.g. if "$" is the command-line prompt:

```sh
$ jq -n -j '"Goodbye, World!"'
Goodbye, World!$ 
```

The trailing "$" is the command-line prompt.

Similarly:

```sh
$ echo '"Goodbye, World!"' | jq -j 
Goodbye, World!$ 
```



## Jsish


```javascript
printf("Goodbye, World!")
```


Evaluated from the command line as:
{{out}}

```txt
prompt$ jsish -e 'printf("Goodbye, World!")'
Goodbye, World!prompt$
```



## Julia

Julia provides a <code>println</code> function which appends a newline, and a <code>print</code> function which doesn't:

```julia
print("Goodbye, World!")
```



## Kotlin

{{trans|Java}}

```scala
fun main(args: Array<String>) = print("Goodbye, World!")
```



## Lasso

Lasso provides a <code>stdoutnl</code> method that prints a trailing newline, and a <code>stdout</code> method that does not:

```lasso
stdout("Goodbye, World!")
```



## LFE


```lisp

(io:format "Goodbye, World")

```



## Liberty BASIC

A trailing semicolon prevents a newline

```lb
print "Goodbye, World!";

```



## LIL


```tcl
write Goodbye, World!
```



## Limbo


```limbo
implement HelloWorld;

include "sys.m"; sys: Sys;
include "draw.m";

HelloWorld: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	sys->print("Goodbye, World!"); # No automatic newline.
}
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

$"OUTPUT_STR" = comdat any
@"OUTPUT_STR" = linkonce_odr unnamed_addr constant [16 x i8] c"Goodbye, World!\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

define i32 @main() {
    %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @"OUTPUT_STR", i32 0, i32 0))
    ret i32 0
}
```



## Logtalk

No action is necessary to avoid an unwanted newline.

```logtalk

:- object(error_message).

    % the initialization/1 directive argument is automatically executed
    % when the object is compiled loaded into memory:
    :- initialization(write('Goodbye, World!')).

:- end_object.

```



## Lua


```lua
io.write("Goodbye, World!")
```



## m4


(Quoted) text is issued verbatim, "dnl" suppresses all input until and including the next newline. Simply creating an input without a trailing newline would of course accomplish the same task.


```m4
`Goodbye, World!'dnl
```



## MANOOL


```MANOOL
{{extern "manool.org.18/std/0.3/all"} in Out.Write["Goodbye, World!"]}
```



## Maple


```Maple

printf( "Goodbye, World!" );

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
NotebookWrite[EvaluationNotebook[], "Goodbye, World!"]
```

Another one that works in scripts:

```Mathematica
WriteString[$Output, "Goodbye, World!"]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
 fprintf('Goodbye, World!');
```



## min


```min
"Goodbye, World!" print
```



## mIRC Scripting Language


```mirc
echo -ag Goodbye, World!
```



## ML/I


### Simple solution

In ML/I, if there isn't a newline in the input, there won't be one in the output; so a simple solution is this (although it's hard to see that there isn't a newline).

```ML/I
Goodbye, World!
```


### More sophisticated solution

To make it clearer, we can define an ML/I ''skip'' to delete itself and an immediately following newline.

```ML/I
MCSKIP " WITH " NL
Goodbye, World!""
```


=={{header|Modula-2}}==

```modula2
MODULE HelloWorld;
FROM Terminal IMPORT WriteString,ReadChar;

BEGIN
    WriteString("Goodbye, World!");
    ReadChar
END HelloWorld.
```



## N/t/roff


By default, /.ROFF/ replaces single non-consecutive newline characters with spaces, but considers two consecutive newline characters as a paragraph separator and omits 2-newline's worth of spaces.  The former behaviour is the same as in HTML and Rosettacode's Wiki syntax: text on non-consecutive single newlines get wrapped on the same line above it.  In /.ROFF/, this is the default behaviour if and only if the typesetter is processing the input in fill mode (<code>.fi</code>); though, by default, the typesetter processes in this mode anyway!

Because /.ROFF/ is a document formatting language, most text input is expected to be text input which will get output on paper, so there is usually no need to run a special procedure or routine to output text.


```N/t/roff

Goodbye, World!

```



## Nanoquery


```nanoquery
print "Goodbye, world!"
```



## Neko

The Neko builtin $print does not add a newline.


```ActionScript
/**
 hellonnl.neko
 Tectonics:
   nekoc hellonnl.neko
   neko hellonnl

   -or-

   nekoc hellonnl.neko
   nekotools boot hellonnl.n
   ./hellonnl
*/

$print("Goodbye, World!");
```


{{out}}

```txt
prompt$ nekoc hellonnl.neko
prompt$ neko hellonnl
Goodbye, World!prompt$
```



## Nemerle



```Nemerle
using System.Console;

module Hello
{
    // as with C#, Write() does not append a newline
    Write("Goodbye, world!");

    // equivalently
    Write("Goodbye, ");
    Write("world!");
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

say 'Goodbye, World!\-'

```



## NewLISP


```NewLISP
(print "Goodbye, World!")
```



## Nim


```nim
stdout.write "Goodbye, World!"
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 PRINT "GOODBYE, WORLD!";
```


=={{header|Oberon-2}}==

```oberon2

MODULE HelloWorld;
IMPORT Out;
BEGIN
  Out.String("Goodbye, world!")
END HelloWorld.

```



## Objeck



```objeck

bundle Default {
  class SayGoodbye {
    function : Main(args : String[]) ~ Nil {
      "Goodbye, World!"->Print();
    }
  }
}

```



## OCaml


In OCaml, the function <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALprint_endline print_endline]</code> prints a string followed by a newline character on the standard output and flush the standard output. And the function <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALprint_string print_string]</code> just prints a string with nothing additional.


```ocaml
print_string "Goodbye, World!"
```



## Oforth


```Oforth
"Goodbye, World!" print
```



## OOC

To omit the trailing newline use print instead of println:

```ooc
main: func {
  "Goodbye, World!" print()
}
```



## Oxygene

{{incorrect|Oxygene|output isn't consistent with the task's requirements: wording, capitalization.}}


```oxygene

namespace HelloWorld;
 
interface
 
type
  HelloWorld = class
  public
    class method Main; 
  end;
 
implementation
 
class method HelloWorld.Main;
begin
  Console.Write('Farewell, ');
  Console.Write('cruel ');
  Console.WriteLine('world!');
end;
 
end.

```


```txt

>HelloWorld.exe
Farewell, cruel world!

```



## Panoramic


```Panoramic

rem insert a trailing semicolon.
print "Goodbye, World!";
print " Nice having known you."
```



## PARI/GP


```parigp
print1("Goodbye, World!")
```



## Pascal


```pascal
program NewLineOmission(output);
 
begin
  write('Goodbye, World!');
end.
```

Output:

```txt
% ./NewLineOmission 
Goodbye, World!% 
```



## PASM



```pasm
print "Goodbye World!"    # Newlines do not occur unless we embed them
end
```



## Perl


```perl
print "Goodbye, World!";    # A newline does not occur automatically
```



## Perl 6

A newline is not added automatically to print or printf

```perl6
print "Goodbye, World!";
printf "%s", "Goodbye, World!";
```



## Phix

Phix does not add '\n' automatically, except for the '?' (debugging) shorthand; if you want one you must remember to add it explicitly.

```Phix
puts(1,"Goodbye, World!")
```



## PHL

Printf doesn't add newline automatically.


```phl
module helloworld_noln;
extern printf;

@Integer main [
    printf("Goodbye, World!");
    return 0;
]
```



## PicoLisp


```PicoLisp
(prin "Goodbye, World!")
```



## Pict


```pict
(pr "Hello World!");
```



## PL/I


```PL/I

put ('Goodbye, World!');

```


## PowerShell


```PowerShell
Write-Host -NoNewLine "Goodbye, "
Write-Host -NoNewLine "World!"
```

{{Out}}

```txt
Goodbye, World!PS C:\>
```



## PureBasic


```PureBasic
OpenConsole()
Print("Goodbye, World!")
Input() ;wait for enter key to be pressed
```



## Python


```python
import sys
sys.stdout.write("Goodbye, World!")
```


{{works with|Python|3.x}}

```python
print("Goodbye, World!", end="")
```



## R


```R
cat("Goodbye, world!")
```



## Ra


```Ra

class HelloWorld
	**Prints "Goodbye, World!" without a new line**

	on start

		print "Goodbye, World!" without new line

```



## Racket


```Racket
#lang racket
(display "Goodbye, World!")
```




## REBOL


```REBOL
prin "Goodbye, World!"
```



## Red


```Red
prin "Goodbye, World!"
```



## Retro


```Retro
'Goodbye,_World! s:put
```



## REXX

It should be noted that upon a REXX program completion, any text left pending without a C/R (or newline) is followed by a

blank line so as to not leave the state of the terminal with malformed "text lines" (which can be followed by other text

(lines) from a calling program(s), or the operating system (shell) which is usually some sort of a "prompt" text string. 

```rexx
/*REXX pgm displays a   "Goodbye, World!"   without a trailing newline. */

call charout ,'Goodbye, World!'
```



## Ring


```ring
see "Goodbye, World!"
```



## Ruby


```ruby
print "Goodbye, World!"
```



## Run BASIC


```RunBasic
print "Goodbye, World!";
```



## Rust


```rust
fn main () {
    print!("Goodbye, World!");
}
```



## Salmon


```Salmon
print("Goodbye, World!");
```



## Scala

{{libheader|scala}}

### Ad hoc REPL solution

Ad hoc solution as [http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL] script. Type this in a REPL session:

```Scala
print("Goodbye, World!")
```



## Scilab

Scilab can emulate C <code>printf</code> which, by default, does not return the carriage.

```scilab
print("Goodbye, World!")
```



## Scheme


```scheme
(display "Goodbye, World!")
```



## Seed7


```seed7
$ include "seed7_05.s7i";
 
const proc: main is func
  begin
    write("Goodbye, World!");
  end func;
```



## SETL


```setl
nprint( 'Goodbye, World!' );
```



## Sidef


```ruby
print "Goodbye, World!";
```

or:

```ruby
"%s".printf("Goodbye, World!");
```



## Smalltalk


```smalltalk

Transcript show: 'Goodbye, World!'.

```



## Standard ML


```sml
print "Goodbye, World!"
```



## Swift

{{works with|Swift|2.x+}}

```swift
print("Goodbye, World!", terminator: "")
```

{{works with|Swift|1.x}}

```swift
print("Goodbye, World!")
```



## Tcl


```tcl
puts -nonewline "Goodbye, World!"
```


=={{header|Transact-SQL}}==
{{incorrect|Transact-SQL|output isn't consistent with the task's requirements: wrong word.}}
As an output statement, PRINT always adds a new line
<lang Transact-SQL> PRINT 'Goodbye, World!'
```

or:
As a result set
<lang Transact-SQL> select 'Goodbye, World!'
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT "Goodbye, World!"

```

Output:

```txt

Goodbye, World!

```



## TXR

Possible using access to standard output stream via TXR Lisp:

```bash
$ txr -e '(put-string "Goodbye, world!")'
Goodbye, world!$
```



## UNIX Shell

The ''echo'' command is not portable, and <code>echo -n</code> is not guaranteed to prevent a newline from occuring. With the original [[Bourne Shell]], <code>echo -n "Goodbye, World!"</code> prints <code>-n Goodbye, World!</code> with a newline. So use a ''printf'' instead.

{{works with|Bourne Shell}}


```bash
printf "Goodbye, World!"          # This works. There is no newline.
printf %s "-hyphens and % signs"  # Use %s with arbitrary strings.
```


Unfortunately, older systems where you have to rely on vanilla Bourne shell may not have a ''printf'' command, either.  It's possible that there is no command available to complete the task, but only on very old systems.  For the rest, one of these two should work:


```bash
echo -n 'Goodbye, World!'
```

or

```bash
echo 'Goodbye, World!\c'
```


The ''print'' command, from the [[Korn Shell]], would work well, but most shells have no ''print'' command. (With [[pdksh]], ''print'' is slightly faster than ''printf'' because ''print'' runs a built-in command, but ''printf'' forks an external command. With [[ksh93]] and [[zsh]], ''print'' and ''printf'' are both built-in commands.)

{{works with|ksh93}}
{{works with|pdksh}}
{{works with|zsh}}


```bash
print -n "Goodbye, World!"
print -nr -- "-hyphens and \backslashes"
```


=
## C Shell
=
C Shell does support <code>echo -n</code> and omits the newline.


```csh
echo -n "Goodbye, World!"
echo -n "-hyphens and \backslashes"
```



## Ursa

Ursa doesn't output a newline to an I/O device by default, so simply omitting an endl object at the end of the output stream is all that's needed.

```ursa
out "goodbye world!" console
```



## Verbexx


```verbexx
@STDOUT "Goodbye, World!";
```



## Vim Script


```vim
echon "Goodbye, World!"
```



## Visual Basic .NET


```vbnet
Module Module1

    Sub Main()
        Console.Write("Goodbye, World!")
    End Sub

End Module
```



## Web 68

{{incorrect|Web 68|output isn't consistent with the task's requirements: wording, punctuation.}}
Use the command 'tang -V hello.w68', then 'chmod +x hello.a68', then './hello.a68'


```web68
@ @a@=#!/usr/bin/a68g -nowarn@>@\BEGIN print("Hello World") END
```



## Wren


```wren
System.write("Goodbye, World!")
```



## XLISP

Either

```scheme
(display "Goodbye, World!")
```

or

```lisp
(princ "Goodbye, World!")
```



## XPL0


```XPL0
code Text=12;
Text(0, "Goodbye, World!")
```



## zkl


```zkl
print("Goodbye, World!");
Console.write("Goodbye, World!");
```



## ZX Spectrum Basic


```basic
10 REM The trailing semicolon prevents a newline
20 PRINT "Goodbye, World!";
```


{{omit from|PHP|lack of special newline command}}
{{omit from|SQL PL|It does not provide a command to not add a new line. There is not CALL DBMS_OUTPUT.CURRENT_LINE, only CALL DBMS_OUTPUT.NEW_LINE}}
