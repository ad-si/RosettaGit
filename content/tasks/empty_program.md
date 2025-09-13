+++
title = "Empty program"
description = ""
date = 2019-10-17T04:18:08Z
aliases = []
[extra]
id = 1596
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Create the simplest possible program that is still considered "correct."





## 11l

An empty text file is a correct 11l program that does nothing.


## 360 Assembly

Return to caller

```360 Assembly
         BR    14
         END
```



## 8051 Assembly

Continuously loops.

```asm
ORG RESET
jmp $
```



## 8086 Assembly


```asm>end</lang


However, if the program needs to exit with an exit code of zero:


```asm
    segment .text
    global _start

_start:
    mov eax, 60
    xor edi, edi
    syscall
    end
```



## AArch64 Assembly

Simulates system call exit(0). In AArch64, the system call number is passed via x8, and the syscall number for exit is 93.
<lang ARM_Assembly>.text
.global _start

_start:
        mov x0, #0
        mov x8, #93
        svc #0
```



## ABAP

Note that the statement "start-of-selection." is implicitly added. This event block needs to be present in executable programs, it's comparable to the main function in other programming languages.


```ABAP

report z_empty_program.

```



## Ada

```ada
procedure Empty is
begin
   null;
end;
```



## Agena

Actually nothing is valid code, too.

```agena></lang



## Aime

The nil input is a valid program.

```aime></lang



## ALGOL 60


```algol60
'BEGIN' 'END'
```



## ALGOL 68


###  Brief form


```algol68
~
```


###  BOLD form


```algol68>SKIP</lang



## ALGOL W


```algolw
.
comment using the <empty statement>;
```



## AmigaE


```amigae
PROC main()
ENDPROC
```



## AppleScript

An empty .scpt file is considered the smallest runnable code, but the following would also be acceptable.

```applescript>return</lang



## Argile

The empty string or file are valid and do nothing.

```Argile></lang



## ARM Assembly

GNU/Linux RaspberryPi example.
<lang ARM_Assembly>.text
    .global _start
_start:
    mov r0, #0
    mov r7, #1
    svc #0
```



## ArnoldC


```ArnoldC
IT'S SHOWTIME
YOU HAVE BEEN TERMINATED
```



## AutoHotkey

An empty script would be enough. Adding "#Persistent" makes it persistent.

```AutoHotkey>#Persistent</lang



## AutoIt

A single comment can be considered a valid program that does nothing.

```AutoIt>;nothing</lang



## AWK

The empty string (or file) is recognised as valid program that does nothing.

The program

```awk>    1</lang

is the simplest useful program, equivalent to

```awk
// {print}
```

I.e. match every input-line, and print it.

Like the UNIX command 'cat', it prints every line of the files given as arguments,
or (if no arguments are given) the standard input.


## Axe

Most Axe examples omit the executable name, but it is shown in this example for completeness.

```axe
:.PRGMNAME
:
```



## BASIC

An empty file is a correct program. It won't be near empty as an executable file, though.

```qbasic></lang

On the ZX Spectrum, we can have a completely empty program with no lines. Here we attempt to run the empty program:

```basic>RUN</lang

 0 OK, 0:1

=
## Applesoft BASIC
=

```ApplesoftBasic></lang



## Batch File

On Windows XP and older, an empty batch file is syntactically correct and does nothing.

```dos></lang

But on Windows 7, an empty .bat file is not recognized and thus a character must exist in it. Some valid characters are <code>: @ %</code>

```dos>:</lang



## BBC BASIC

In BBC BASIC an empty program is syntactically correct.

```bbcbasic></lang



## bc

An empty file is a valid program.


## Beeswax



```beeswax
*
```

(create 6 bees moving in all 6 cardinal directions)
or

```beeswax>\</lang

(create 2 bees moving in “northwest” and “southeast” directions)
or

```beeswax
_
```

(create 2 bees moving left and right)
or

```beeswax>/</lang

(create 2 bees moving in “northeast” and “southwest” directions)

A valid beeswax program needs at least one of these bee spawning symbols, as a program without bees is not executable. All bees that step off the honeycomb (program area) are automatically deleted, and the program ends if no bees are left.


## Befunge

 @

The halt command '''@''' is required because code wraps around. An empty file would be an infinite loop.


## Bracmat

An empty file is a valid program. However you need to load it, which requires a statement. In a Linux terminal, you could do

```bracmat
touch empty
bracmat 'get$empty'
```


In DOS, you can do

```dos
touch empty
bracmat get$empty
```


If we drop the requirement that the shortest program is stored in a file, we can do

```bash
bracmat ''
```

(Linux)
or

```dos
bracmat ""
```

(Windows)

If two quotes to demarcate an empty string are counted as bigger than a single undemarcated non-empty expression, we can do
  bracmat .

The dot is a binary operator. So the input consists of three nodes: the operator and its lhs and rhs, both empty strings in this case. If three nodes is too much, consider a slightly bigger glyph, such as the hyphen, which is a prefix, not a binary operator:
  bracmat -

You also can start Bracmat without arguments, in which case it will run in interactive mode. Now press the Enter key. You have just run the shortest valid Bracmat program.

=={{header|Brainfuck}}==

```txt
Empty program
```

''Note:'' this works as all non-instruction characters are considered comments. Alternatively, a zero-byte file also works.


## Brlcad

Pressing enter from the mged prompt, just returns another prompt, so I suppose that is the smallest possible program. However, before we can draw anything we at least need to open a database:

```mged>opendb empty.g y</lang



## C

```c
main()
{
  return 0;
}
```


As of C99 the return type is required, but the return statement is not.
```c
int main() { }
```


This is technically undefined behavior but on 8086 compatible processors <code>195</code> corresponds to the <code>ret</code> assembly instruction.
```c
const main = 195;
```


## C#

```c#
class P{static void Main(){}}
```



## C++

```cpp
int main(){}
```



## Clean


```clean
module Empty

Start world = world
```

Compile the project with ''No Console'' or ''No Return Type'' to suppress printing of the value of the world.


## Clojure

An empty file is the simplest valid Clojure program.


## COBOL

```cobol></lang



## CoffeeScript


```coffeescript></lang



## Common Lisp


```lisp
()
```



## Component Pascal

BlackBox Component Builder;

```oberon2

MODULE Main;
END Main.

```



## Computer/zero Assembly

The smallest legal program is a single Stop instruction.

```czasm>        STP</lang



## D


```d
void main() {}
```



## Dart


```dart
main() {
}
```



## dc

An empty file is a valid program.


## DCL

An empty file is a valid program.


## Delphi

:''See [[#Pascal|Pascal]]''

=={{header|Déjà Vu}}==

```dejavu></lang

Shortest module that works with <code>!import</code>:

```dejavu
{}
```



## Dyalect


Dyalect is not very happy with a completely empty source code file, however a pair of curly brackets would do:


```dyalect
{}
```


This program would evaluate and return "nil".


## E

The shortest possible program:

```txt

```

This is equivalent to:

```e>null</lang



## eC


```txt

```

or

```ec
class EmptyApp : Application
{
    void Main()
    {

    }
}
```



## EchoLisp


```scheme


```



## EDSAC order code

The smallest program that will load and run without error. Apart from <tt>ZF</tt>, the 'stop' order, it consists solely of directives to the loader.

```edsac
T64K  [ set load point ]
GK    [ set base address ]
ZF    [ stop ]
EZPF  [ begin at load point ]
```



## Egel

The smallest program contains nothing.

```Egel


```



## EGL

General program

```EGL

package programs;

program Empty_program type BasicProgram {}
	function main()
	end
end

```

Rich UI handler (but also without 'initialUI = [ ui ], onConstructionFunction = start' it would have been valid.)

```txt

package ruihandlers;

import com.ibm.egl.rui.widgets.Div;

handler Empty_program type RUIhandler {initialUI = [ ui ], onConstructionFunction = start}
	ui Div{};

	function start()
	end
end

```



## Eiffel

A file called root.e:

```eiffel
class
    ROOT

create
    make

feature
    make
        do

        end
end
```



## Elena

ELENA 4.x

```elena
public program()
{
}
```



## Elixir


```elixir></lang



## Elm


```elm

--Language prints the text in " "
import Html
main =
 Html.text"empty"

```



## Erlang

An empty module:

```erlang
-module(empty).
```

An empty Erlang script file (escript):

```erlang
main(_) -> 1.
```



## ERRE

<lang>
PROGRAM EMPTY
BEGIN
END PROGRAM

```



## eSQL


```sql
CREATE COMPUTE MODULE ESQL_Compute
  CREATE FUNCTION Main() RETURNS BOOLEAN
  BEGIN
    RETURN TRUE;
  END;
END MODULE;
```



## Euphoria


```Euphoria></lang


=={{header|F_Sharp|F#}}==
F# has an interactive mode and a compiled mode. The interactive interpreter will accept an empty file so the shortest valid program is an empty zero-length file with the .fsx extension.

```fsharp></lang

An empty compiled program is:

```fsharp>[<EntryPoint
]
let main args = 0
```



## Factor


```factor></lang

If you want to deploy a stand-alone application, that doesn't suffice though. Here's another version.

```factor
IN: rosetta.empty
: main ( -- ) ;
MAIN: main
```



## Falcon


```txt
>
```

Prints an empty line.


```txt
>>
```

Prints nothing.


## FALSE


```false></lang



## Fantom


```fantom
class Main
{
  public static Void main () {}
}
```



## FBSL

An empty string is a valid FBSL script in both uncompiled and compiled form. It won't however produce any visible output on the screen. The minimum implementations for the user to see the result are presented below:

'''Console mode:'''
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:gray">#APPTYPE CONSOLE</span>

<span style="color:blue">PAUSE</span>
</code></b></div>
'''Output:'''
<div style="overflow:auto;white-space:nowrap;background-color:black;border:1px dashed rgb(167, 215, 249); padding:12px"><b><code>
<span style="color:white">Press any key to continue...</span>
</code></b></div>
'''Graphics mode:'''
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:blue">SHOW</span>(<span style="color:darkblue">ME</span>) <span style="color:green">' all FBSL scripts are #APPTYPE GUI on default</span>
<span style="color:red">BEGIN EVENTS
END EVENTS</span>
</code></b></div>
'''Output:''' [http://i1240.photobucket.com/albums/gg490/FbslGeek/FBSL_ME.png GUI Form]

'''Minimum empty Dynamic Assembler block:'''
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:red">DYNASM</span> Foo()
:<span style="color:blue">RET</span> <span style="color:green">; mandatory</span>

<span style="color:red">END DYNASM</span>
</code></b></div>
'''Minimum empty Dynamic C block:'''
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:red">DYNC</span> Foo()
:<span style="color:blue">void</span> <span style="color:red">main</span>(<span style="color:blue">void</span>)

:{
::<span style="color:blue">return</span>; <span style="color:green">// optional</span>

:}
<span style="color:red">END DYNC</span>
</code></b></div>


## Fish

Actually the shortest valid program is a space (not empty file!), which is an infinite loop, though. (It keeps looping around)

```Fish> </lang

An empty program is invalid; the interpreter will give an error.<br/>
The shortest program that will actually finish is a <tt>;</tt>, which will end the program immediately:

```Fish>;</lang


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Empty_program this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth></lang

For a Forth script to be used from a shell, you usually want the last command to be BYE in order to exit the interpreter when finished.

```forth>bye</lang



## Fortran


```fortran>       end</lang


## FreeBASIC

A completely empty program compiles and runs fine:

```freebasic></lang



## friendly interactive shell

Empty programs are valid, but are useless.

```fishshell></lang



## Frink

Empty programs are valid.

```frink></lang



## FunL

An empty text file is a valid FunL program that does nothing.

```funl></lang



## Futhark


Any Futhark program must have a <code>main</code> function.  Alternatively, a Futhark library can be an empty file.


```Futhark

let main = 0

```



## FutureBasic

Why?

```futurebasic

end

```



## Gecho

Empty programs are valid.

```gecho></lang



## Gema

An empty program will copy input stream to output stream unchanged.

```gema></lang



## Genyris


```txt

```



## Global Script

This program is intended for use with the [[HS Global Script]] and uses its syntax for imperative programs.

```Global Script
λ _. impunit 〈〉
```



## Go


```go
package main
func main() { }
```



## Groovy


```groovy></lang



## Haskell

'''Standard:''' [[Haskell 98]]

The simplest possible program is a single module using the implicit module header "<tt>module Main(main) where</tt>", and defining the action <tt>main</tt> to do nothing:

```haskell
main = return ()
```

The simplest possible module other than Main is one which contains no definitions:

```haskell
module X where {}
```



## Haxe


```haxe
class Program {
    static function main() {
    }
}
```

Unlike most languages Haxe doesn't have arguments in the main function because it targets different platforms (some which don't support program arguments, eg: Flash or Javascript). You need to use the specific libraries of the platform you are targeting to get those.


## HicEst


```hicest
END ! looks better, but is not really needed
```



## HQ9+

An empty file is a valid HQ9+ program that does nothing.


## HolyC

An empty file is the simplest valid HolyC program and returns 0.


## HTML

HTML 5, [http://www.whatwg.org/specs/web-apps/current-work/multipage/syntax.html#optional-tags section 12.1.2.4 Optional tags], allows to omit ''html'', ''head'' and ''body'' tags. The implicit ''body'' element can be empty, but the implicit ''head'' element must contain a ''title'' element, says [http://www.whatwg.org/specs/web-apps/current-work/multipage/semantics.html#the-head-element section 4.2.1 The head element]. There seems no rule against an empty title. Therefore, the shortest correct HTML document is:

```html5
<!DOCTYPE html><title></title>
```


The shortest correct XHTML document is:

```html5
<html xmlns="http://www.w3.org/1999/xhtml"><head><title /></head><body /></html>
```



## Huginn


```huginn
main(){}
```



## i


```i
software{}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()   # a null file will compile but generate a run-time error for missing main
end
```



## IDL


```idl>end</lang



## Inform 7


```inform7>X is a room</lang

Inform 7 is a language built for making interactive fiction, so a room needs to be defined for the player to start in.


## Intercal


```intercal>PLEASE GIVE UP</lang



## Io


```txt

```



## J


```j
''
```

It returns itself:

```j
   '' -: ". ''
1
```



## Java

```java
public class EmptyApplet extends java.applet.Applet {
    @Override public void init() {
    }
}
```



```java
public class EmptyMainClass {
    public static void main(String... args) {
    }
}
```


The "..." basically means "as many of these as the programmer wants." Java will put multiple arguments into an array with the given name. This will work for any method where an array is an argument, but with a twist. A call can be made like this:


```java
method(arg0, arg1, arg2, arg3)
```


All of the args will be put into an array in the order they were in the call.

```java
public class EmptyMainClass {
    public static void main(String[] args) {
    }
}
```



```java
public class EmptyApplet extends java.applet.Applet {
    public void init() {
    }
}
```


@Override - Indicates that a method declaration is intended to override a method declaration in a superclass. If a method is annotated with this annotation type but does not override a superclass method, compilers are required to generate an error message. It's present from JDK 5.0 (1.5.0) and up.

Actually this is not strictly correct. The smallest possible correct program in Java is an empty source file.


## JavaScript

The empty file is a valid program.

```txt

```



## Joy


```joy>.</lang



## Jq

The “empty” filter ignores its input and outputs nothing.


```jq>empty</lang



## Julia

Julia accepts an empty file as a program.

```Julia></lang


```txt

$ wc empty_program.jl
0 0 0 empty_program.jl
$ julia empty_program.jl
$

```



## K

<lang>
```



## KonsolScript


```KonsolScript
function main() {

}
```



## Kotlin


```scala
fun main(a: Array<String>) {}
```



## Lang5


```Lang5>exit</lang



## Lasso

Lasso will parse any file thrown at it. It will ignore everything except what's inside specific Lasso delimiters. Thus a valid program that did nothing, could be an empty file. Perhaps more correct would be a file that had the specific delimiters and then nothing inside them.

```Lasso
[]
```


```Lasso><?lasso  ?></lang


```Lasso><?=  ?></lang



## LaTeX


```latex
\documentclass{article}
\begin{document}
\end{document}
```



## LC3 Assembly

The only thing you absolutely need is a directive telling the assembler to stop assembling code (which in this case it has not actually started doing).

```lc3asm>        .END</lang



## Liberty BASIC


```lb>end</lang



## Lilypond

According to the manual, all lilypond programs should contain a version statement expressing the minimum version number. If this is missing then a warning will be emitted.

```lilypond
\version "2.6.12"
```


An input file should really have a basic structure as follows. The compiler automatically adds some of the structure components if they are not present in the source code. However, explicit definition should be used to prevent the compiler from creating unwanted contexts (which can cause side effects):


```lilypond
\version "2.16.2"

\header {

}

\book {
  \score {
    \new Staff {
      \new Voice {

      }
    }
    \layout {

    }
  }
}
```



## Lingo

"Program" doesn't really apply to Lingo. A Director projector (exe/app) doesn't have to contain any scripts/code. For scripts, the shortest possible code is:

```lingo></lang



## Lisp

Most Lisp dialects, including Common Lisp, will accept no text (no forms) as a valid program.

```lisp></lang



## Logo


```logo></lang

or end a standalone script with "bye"

```logo
#! /usr/local/bin/logo

bye
```



## LSE64

As with [[Forth]], an empty file is the shortest program. To exit the interpreter at the end of a loaded file:
 bye


## Lua


```Lua></lang



## M2000 Interpreter

Open M2000 Environment, type Edit A and place one space or insert a new line, then exit pressing Esc and write Save Empty (press enter). Now write New (press enter). Now write Load Empty (press enter) and execute it: write A (press enter). Now you can close environment: write End (press enter).

To open file Empty.gsb, as text file (without loading to list of modules) use Edit "Empty.Gsb". To open it in notepad, we can use this command from M2000 console: win "notepad", dir$+"empty.gsb"

We can open explorer for dir$ (the m2000 user directory) using Win Dir$  (we can use paths without quotes if no space includes, so win c:\ open explorer to c:)

File saved as:

```M2000 Interpreter

MODULE GLOBAL A {
}

```

Because we make it in "level 0" (from console) this is a global module. A global module which loaded from console, or was a module loaded from a file at command line, when opening the environment, erased with an End, or a New, or a Start statement (reset of environment by software), or a Break by keyboard (although a dialog ask for proceed the breaking, the reset of environment) , or in some situation by using End Process from Task Manager.

If we wish to run it from command line (by clicking the file in explorer, and let m2000.exe open gsb files), we have to consider the first that this file not contain an execute statement, and that if we didn't use an input statement/function which need console, then console stay hide. To be sure that console open we have to use Show statement. To run A we have to include A at the last line (or append a line and write A). So we write in first line Show (press Esc to return to prompt) and save the file as Save Empty, A so we get this:


```M2000 Interpreter

MODULE GLOBAL A {Show
}
A

```

We can open it with Edit "empty.gsb" add some statements between A and block of module, to make some globals, say a DIM a(10) which stay there until the end of current interpreter run (interpreter may run multiple times simultaneously). All globals are globals for current interpreter only.

We can run empty.gsb now (supposed we save it as Save Empty, A), from explorer or using a command in M2000 console which opens another interpreter (and can feed it with some numbers or and strings, but this is another story):

```txt

Use empty

```

So now we see environment again, with open console and at prompt (execution done, and stay open because no End statement executed after the A, or inside module as Set End). Set used to send commands to prompt by code.

Finally this is the code in a file (say Empty.gsb) to open, display something, waiting for a key (now a Show automatic happen) and then finish. We have to write it, in M2000 editor, and save it using Save Empty, A or in any editor and save it as empty.gsb in your desired folder.
sav

```M2000 Interpreter

MODULE GLOBAL A {
Print "Hello World"
a$=Key$
Set End
}
A

```


We can save it scrabbled text using Save "empty" @, A (not readable, but environment can revert the process using a unique key)

(m2000 is open source so key is not a mystery, but you can make your own clone, and use own key)


## M4


```M4></lang



## Maple


```Maple></lang


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica></lang



## MATLAB


```Matlab
 function [varargout] = emptyprogram(varargin)
```



## Maxima



```maxima
block()$
```



## MAXScript

An empty MAXScript file returns "OK" on execution


## Metafont


```metafont>end</lang



## Microsoft Small Basic


```smallbasic></lang

```txt

```



## min

```min></lang



## MiniScript


```MiniScript></lang


=={{header|MK-61/52}}==
<lang>С/П
```



## MIPS Assembly

This just exits the program with exit code 0 (exit_success)

```mips

	.text
main: 	li 	$v0, 10
	syscall

```



## ML/I


```ML/I></lang



## MMIX


```mmix
	LOC	#100
Main	TRAP	0,Halt,0	// main (argc, argv) {}
```


=={{header|Modula-2}}==

```modula2
MODULE Main;

BEGIN
END Main.
```


=={{header|Modula-3}}==

```modula3
MODULE Main;

BEGIN
END Main.
```



## MUMPS

The empty file is a valid program.

```txt

```



## N/t/roff


```N/t/roff></lang


An empty input file is valid, but if the output is Postscript or PDF, most PDF viewers will suffer.  However, that's the PDF viewer's fault; the typesetter is still okay with an empty file.  If one wants grace for the PDF viewers, import a macro that, at the very least, defines some proper margins and pagination as in the following code:

```N/t/roff>.mso me.tmac</lang



## Nemerle

Compiles with warnings:

```Nemerle>null</lang

Compiles without warnings (so, more correct):

```Nemerle
module Program
{
    Main() : void
    {
    }
}
```



## NetRexx

The following two samples both generate valid programs.

This minimal example requires that the file be named to match the class:

```NetRexx>class empty</lang


This example will generate its class based on the file name:

```NetRexx
method main(args = String[]) static
```



## NewLISP


```NewLISP>; </lang



## Nim


```txt

```



## Nit

Although a Nit module (file) usually include a <tt>module</tt> declaration, an empty module is a valid Nit program.

```txt

```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>
```



## Objeck


```objeck
bundle Default {
  class Empty {
    function : Main(args : String[]) ~ Nil {
    }
}
```


=={{header|Objective-C}}==
```objc
int main(int argc, const char **argv) {
    return 0;
}
```


The minimal ''empty'' Cocoa/OpenStep application, useful as life-support for many examples given at RosettaCode, is

```objc>#import <Cocoa/Cocoa.h


int main( int argc, const char *argv[] )
{
  @autoreleasepool {
    [NSApplication sharedApplication];
  }
  return 0;
}
```



## OCaml

```ocaml>;;</lang


Actually, the smallest possible correct program in OCaml is an empty source file.


## Octave

An empty text file can be a valid empty program, but since Octave has the concept of "function file" (a file containing a single function; the file is automatically loaded when a function with the same name of the file, save for the extension, is called, and the first function present in the file is used), the name of the empty file matters. E.g. calling an empty file as <tt>isempty.m</tt> makes unusable the builtin <tt>isempty</tt> function.


## Oforth


An empty file is a valid oforth file


```Oforth>oforth empty.of</lang


Without file, interpreter can just evaluate bye :

```Oforth
oforth --P"bye"
```



## OOC

The Compiler will accept an empty file:

```ooc></lang



## OpenLisp


We can run OpenLisp in shell mode with an empty program as follows.
This is for the Linux version of OpenLisp.


```openlisp

#!/openlisp/uxlisp -shell
()

```



## OxygenBasic

The smallest possible program is a single space character:

```oxygenbasic



```



## Oz


###  Accepted by compiler

The simplest 'program' that can be compiled is a file which contains a single expression.

```oz>unit</lang

Such a 'program' cannot be executed, though.

###  Standalone

The simplest standalone program is a root functor that does not define anything. ("Functors" are first-class modules.)

```oz
functor
define
   skip
end
```



## PARI/GP


```parigp></lang



## Pascal


```pascal
program ProgramName;

begin
end.
```

The first line is not necessary in modern Pascal dialects. With today's most compilers, the empty program is just:

```pascal>begin end.</lang



## Perl


The empty program is valid and does nothing but return a successful exit code:


```perl></lang


Of course, this then requires you to specify the interpreter on the command line (i.e. <code>perl empty.pl</code>). So slightly more correct as a stand-alone program, is:


```perl
#!/usr/bin/perl
```


The smallest possible Perl one-liner is <code>perl -e0</code>.


## Perl 6


The empty program is valid and does nothing but return a successful exit code:

```perl6></lang


It is also possible to just specify that the program is written in Perl6:

```perl6>use v6;</lang


or even:

```perl6>v6;</lang



## Phix

An empty file is a valid program. When compiled however, it is far from empty as it contains most of the VM and a full run-time diagnostics kit (together about 202K).


## PHP

An empty text file is a correct PHP program that does nothing.


## PicoLisp


```PicoLisp
(de foo ())
```



## Pike


```pike
int main(){}
```



## PIR

The ''':main''' pragma indicates that a subroutine is the program's entry point. However, if a subroutine is the first (or only, which would also have the effect of making it the first) routine in the program, Parrot will use that. So we may comfortably omit it in this case.

```pir
.sub empty_program
.end
```



## PL/I


```PL/I
s: proc options (main);
end;
```



## PL/SQL


```sql
BEGIN
    NULL;
END;
```



## plainTeX


```tex>\bye</lang



## Pop11

Pop11 has two compilers, incremental and batch compiler.  For the incremental compiler one can use just empty program text (empty file), or a file containing nothing but a comment, e.g.

```pop11>;;; This is a valid Pop11 program that does absolutely nothing.</lang

The batch compiler generates an executable which starts at a given entry point, so one should provide an empty function. If one wants program that works the same both with incremental compiler and batch compiler the following may be useful:

```pop11
compile_mode :pop11 +strict;
define entry_point();
enddefine;

#_TERMIN_IF DEF POPC_COMPILING
entry_point();
```

Here the batch compiler will stop reading source before call to entry_point while incremental compiler will execute the call, ensuring that in both cases execution will start from the function entry_point.


## PostScript

An empty file is a valid PostScript program that does nothing.

Following good programming practice, however, and to ensure that a PostScript printer will interpret a file correctly, one should make the first 4 characters of the file be

```postscript
%!PS
```


If a particular version of the PS interpreter is needed, this would be included right there:

```postscript
%!PS-2.0
% ...or...
%!PS-3.0
% etc
```



## PowerShell

An empty script block.  A script block is a nameless (lamda) function.

```PowerShell

&{}

```


```txt



```



## ProDOS

This is an acceptable program:

```ProDOS>IGNORELINE</lang

But also you could include a delimiter character recognized by the compiler/interpreter:

```ProDOS>;</lang



## PSQL


```sql
EXECUTE BLOCK
AS
BEGIN
END
```



## PureBasic

An empty file is a correct PureBasic program that does nothing.

```PureBasic></lang



## Python

An empty text file is a correct Python program that does nothing.

An empty file named <code>__init__.py</code> even has a structural purpose in Python of declaring that a directory is a [https://docs.python.org/3/tutorial/modules.html#packages Package].


## QUACKASM


```quackasm
1
QUIT
```



## R

An empty text file is a valid empty program


## Racket

The following shows an empty program in Racket's default language. Other Racket languages may impose different conditions on the empty program.

```racket

#lang racket

```



## Raven

An empty text file is an empty program.


## REBOL

The header section is mandatory if you want it to be recognized as a REBOL program. It doesn't have to be filled in though:

```REBOL
REBOL []
```



## Retro

An empty file is the smallest valid program.


```Retro></lang



## REXX

An empty (or blank) file is a valid REXX program.

Some REXX implementations require a special comment   [(1<sup>st</sup> word in the comment

must be   REXX   (in upper/lower/mixed) case]   to distinguish from other types of

scripting languages, but a null program (or a program with only blanks in it)   in those

other scripting languages are also considered an empty program.

### version 1

This program can be empty (no characters),   or a program with (only) one or more blanks.

```rexx></lang



### version 2

REXX on MVS/TSO requires REXX to be within a REXX comment that begins on the first line:

```rexx
/*REXX*/
```



## Rhope

 Main(0,0)
 |: :|


## Ring


```ring></lang



## Robotic


```robotic></lang



## Ruby

An empty file is a valid Ruby program. However, in order to make it runnable on *nix systems, a shebang line is necessary:

```ruby
#!/usr/bin/env ruby
```



## Run BASIC


```runbasic
end  ' actually a blank is ok
```



## Rust


```rust
fn main(){}
```



## Scala


```scala
object emptyProgram extends App {}
```



## Scheme


```scheme></lang



## Scilab


```scilab></lang



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is noop;
```



## Set lang

<lang Set_lang>
```



## Sidef


```ruby></lang



## Simula

```simula
BEGIN
END
```



## Slate


```slate></lang



## Smalltalk


```smalltalk
[]
```



## SNOBOL4

A valid program requires an '''end''' label. The shortest (virtually empty) program is then:

```snobol>end</lang



## SNUSP

 $#
'''$''' sets the instruction pointer (going right), and '''#''' halts the program (empty stack).


## Sparkling


```Sparkling></lang



## SQL PL

With SQL only:

```sql pl

SELECT 1 FROM sysibm.sysdummy1;

```

Output:

```txt

db2 -t
db2 => SELECT 1 FROM sysibm.sysdummy1;

1
------------
           1

  1 record(s) selected.

```

With SQL PL:

```sql pl

--#SET TERMINATOR @

CREATE PROCEDURE myProc ()
 BEGIN
 END @

```

Output:

```txt

db2 -td@
db2 => CREATE PROCEDURE myProc ()
...
db2 (cont.) =>  END @
DB20000I  The SQL command completed successfully.

```

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

BEGIN
END;

```

Output:

```txt

db2 -t
db2 => BEGIN
db2 (cont.) => END;
DB20000I  The SQL command completed successfully.

```



## SSEM

A completely empty program—all store bits clear, just power the machine up and hit Run—is meaningful in SSEM code and even does something, although not something desirable:

```ssem
00000000000000000000000000000000   0. 0 to CI    jump to store(0) + 1
00000000000000000000000000000000   1. 0 to CI    jump to store(0) + 1
```

Since the number in address 0 is 0, this is equivalent to

```txt
    goto 1;
1:  goto 1;
```

and has the effect of putting the machine into an infinite loop.

The smallest program that will terminate is:

```ssem>00000000000001110000000000000000   0. Stop</lang



## Standard ML


```sml>;</lang


Actually, the smallest possible correct program in Standard ML is an empty source file.


## Stata

Stata does not accept an empty program, so we have to do something. Here we only declare the minimum [http://www.stata.com/help.cgi?version version] of the interpreter for the program.


```stata
program define nop
        version 15
end
```


It's also possible to define an empty function in Mata.


```stata
function nop() {}
```



## Suneido


```Suneido
function () { }
```



## Swift


```Swift></lang



## Symsyn


```Symsyn></lang



## Tcl

Nothing is mandatory in Tcl, so an empty file named <tt>nothing.tcl</tt> would be a valid "empty program".

```tcl></lang


=={{header|TI-83 BASIC}}==
<lang TI-BASIC>
```

Displays "Done". If an empty program isn't valid, there are numerous other one-byte solutions:
 :

 Disp

 Return

 Stop

=={{header|TI-83 Hex Assembly}}==
<lang TI-BASIC>PROGRAM:EMPTY
:AsmPrgmC9
```


=={{header|TI-89 BASIC}}==
 Prgm
 EndPrgm


## Toka

For interpreted code, nothing is required, although '''bye''' is necessary for an empty script to exit (rather than wait for the user to exit the listener). Hence:

 bye

Or, for a directly runnable script:

 #! /usr/bin/toka
 bye

For compiled code, the simplest program is an empty quote:

  [ ]

Again, to exit the listener, you will still need user input if this is not followed with '''bye'''.


## Trith


```trith></lang



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
```



## UNIX Shell

```bash
#!/bin/sh
```

```bash
#!/bin/bash
```

```ksh
#!/bin/ksh
```



## Unlambda

 i
(See how i plays so many roles in unlambda?)


## Ursa

The Cygnus/X Ursa interpreter has no problems with empty files, so the shortest program is an empty file.

```ursa></lang



## VAX Assembly


```VAX Assembly
0000  0000     1 .entry	main,0	;register save mask
  04  0002     2 	ret	;return from main procedure
      0003     3 .end	main	;start address for linker
```


## VBScript

An empty .vbs file is considered the smallest runnable code, but the following (a single apostrophe as comment marker) would also be acceptable (along with other non-executing instructions like <code>option explicit</code>.)

```vb
'
```



## Verbexx

An empty file is the smallest valid script, but running it does nothing.

```verbexx></lang



## VHDL

Compiled and simulated by Modelsim:

```VHDL
entity dummy is
end;

architecture empty of dummy is
begin
end;
```



## Vim Script

An empty file is a valid program.


## VBA

Same as Visual Basic, VB6, etc.

```vb
Sub Demo()
End Sub
```



## Visual Basic

'''Works with:''' VB6

```vb
Sub Main()
End Sub
```



## Visual Basic .NET

```vbnet
Module General
    Sub Main()
    End Sub
End Module
```



## Wart


```txt

```



## WDTE


An empty 'file' is a valid WDTE script. That being said, WDTE has no inherent concept of scripts being in files, so a zero-length input may be a better description.


## Wee Basic


```Wee Basic></lang



## X86 Assembly

```asm
section .text
	global _start

	_start:
		mov eax, 1
		int 0x80
		ret
```

```asm
.386
.model flat, stdcall
option casemap:none

.code
start:
ret
end start
```



## XPL0

An empty file compiles and builds an EXE file with a single RET instruction, but of course does nothing when executed.

```txt

```



## XQuery


```xquery>.</lang

The dot selects the current context node and returns it unchanged.


## XSLT


```xslt
<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
   <!-- code goes here -->
</xsl:stylesheet>
```


Add other namespaces to the stylesheet attributes (like xmlns:fo="http://www.w3.org/1999/XSL/Format") if you use them.

Since XSLT is XML, and <code>transform</code> is a synonym for <code>stylesheet</code>, the example above can be minified to:

```xslt
<transform xmlns="http://www.w3.org/1999/XSL/Transform" version="1.0"/>
```


This stylesheet echoes the text content of an XML file. The shortest stylesheet without any output would be

```xslt
<transform xmlns="http://www.w3.org/1999/XSL/Transform" version="1.0">
   <template match="/" />
</transform>
```



## xTalk

 on startup

 end startup


## XUL


```xul

<?xml version="1.0"?>

```



## Yorick

An empty file is valid and does nothing.

```txt

```



## Z80 Assembly

 ret


## zkl

An empty file/string is valid.

```txt

```


```zkl
c:=Compiler.Compiler.compileText("");
c() //--> Class(RootClass#)
```



## ZX Spectrum Basic

:''See [[#BASIC|BASIC]]''
