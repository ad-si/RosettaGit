+++
title = "Interactive programming"
description = ""
date = 2019-09-08T13:14:13Z
aliases = []
[extra]
id = 3837
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "8th",
  "acl2",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "bracmat",
  "brat",
  "burlesque",
  "c_shell",
  "clojure",
  "coffeescript",
  "common_lisp",
  "csharp",
  "e",
  "echolisp",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "es",
  "factor",
  "fantom",
  "forth",
  "freebasic",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "huginn",
  "io",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "lingo",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "maxima",
  "ml_i",
  "nim",
  "ocaml",
  "octave",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "picolisp",
  "pike",
  "powershell",
  "prolog",
  "python",
  "r",
  "racket",
  "rebol",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "tcl",
  "unix_shell",
  "ursa",
  "vedit_macro_language",
  "xlisp",
  "zkl",
]
+++

Many language implementations come with an ''interactive mode''.

This is a [[wp:command-line interpreter|command-line interpreter]] that reads lines from the user and evaluates these lines as statements or expressions.

An interactive mode may also be known as a ''command mode'',   a ''[[wp:read-eval-print loop|read-eval-print loop]]'' (REPL),   or a ''shell''.


## Task

Show how to start this mode.

Then, as a small example of its use, interactively create a function of two strings and a separator that returns the strings separated by two concatenated instances of the separator   (the 3<sup>rd</sup> argument).


;Example:
            <big>  f('Rosetta',  'Code',  ':')  </big>
should return
            <big>  'Rosetta::Code'              </big>


;Note:
This task is   ''not''   about creating your own interactive mode.





## 8th

8th (like all Forth derivatives) has a built-in interpreter loop.  All you need to do to access it is to start 8th:

```txt
 $ 8th
8th 1.0.2 Linux 64 (f1b7a8c2)
ok>

```

At the "ok> " prompt type in what you want:

```txt

ok> : f 2 s:* swap s:+ s:+ ;
ok> "Rosetta" "Code" ":" f . cr
Rosetta::Code
ok>

```


## ACL2



```txt
$ acl2
Welcome to Clozure Common Lisp Version 1.7-r14925M  (DarwinX8664)!

 ACL2 Version 4.3 built September 8, 2011  09:08:23.
 Copyright (C) 2011  University of Texas at Austin
 ACL2 comes with ABSOLUTELY NO WARRANTY.  This is free software and you
 are welcome to redistribute it under certain conditions.  For details,
 see the GNU General Public License.

 Initialized with (INITIALIZE-ACL2 'INCLUDE-BOOK *ACL2-PASS-2-FILES*).
 See the documentation topic note-4-3 for recent changes.
 Note: We have modified the prompt in some underlying Lisps to further
 distinguish it from the ACL2 prompt.

ACL2 Version 4.3.  Level 1.  Cbd "/Users/username/".
Distributed books directory "/Users/username/Code/acl2/books/".
Type :help for help.
Type (good-bye) to quit completely out of ACL2.

ACL2 !>(defun f (s1 s2 sep)
(concatenate 'string s1 sep sep s2))

Since F is non-recursive, its admission is trivial.  We observe that
the type of F is described by the theorem (STRINGP (F S1 S2 SEP)).
We used the :type-prescription rule STRING-APPEND-LST.

Summary
Form:  ( DEFUN F ...)
Rules: ((:TYPE-PRESCRIPTION STRING-APPEND-LST))
Time:  0.01 seconds (prove: 0.00, print: 0.00, other: 0.00)
 F
ACL2 !>(f "Rosetta" "Code" ":")
"Rosetta::Code"
ACL2 !>(good-bye)

```


## AWK


```AWK

# syntax: GAWK -f CON   GNU
# syntax: TAWK          Thompson Automation

```

<p>Command, program, output:</p>

```txt

GAWK -f CON
BEGIN {
x = f("Rosetta","Code",":")
print(x)
}
function f(s1,s2,s3) { return(s1 s3 s3 s2) }
^Z   <-- CTRL-Z
Rosetta::Code

```

<p>Command (followed by welcome banner), program, output:</p>

```txt

TAWK
TAWK Version 5.0c (for Windows/NT)
Copyright 1996 Thompson Automation, Inc.  All Rights Reserved.
Enter AWK Program followed by a blank line:
BEGIN {
x = f("Rosetta","Code",":")
print(x)
}
function f(s1,s2,s3) { return(s1 s3 s3 s2) }
   <-- blank line
Enter program arguments, if any:
   <-- blank line
Rosetta::Code

```



## BASIC

This was tested with SAM BASIC, but it should work with most Basic interpreters.

A Basic interpreter is in command mode by default. Enter the following in command mode:

```qbasic
10 DEF FN f$(a$, b$, s$) = a$+s$+s$+b$
PRINT FN f$("Rosetta", "Code", ":")
```



## Batch File

CMD.EXE is the CLI for Batch Files. Batch Files do not have traditional functions and doing the [http://rosettacode.org/wiki/Call_a_function#Batch_File alternative] will not work in interactive mode. So this example uses blocks of code.
```dos>
set r=Rosetta

>set c=Code

>set s=:

>echo %r%%s%%s%%c%
Rosetta::Code

>
```



## BBC BASIC

Interactive mode is entered by clicking on the 'Immediate' toolbar button or selecting Immediate from the Run menu.
Functions cannot be defined in immediate mode so the example below uses inline code:

```txt
>r$ = "Rosetta"
>c$ = "Code"
>s$ = ":"
>PRINT r$+s$+s$+c$
Rosetta::Code
>

```



## bc

bc has no string operations, so I will instead show how to use hexadecimal numbers such that <tt>f(205E77A, C0DE, 1)</tt> returns <tt>205E77A11C0DE</tt>.

Step 1. I start the interactive interpreter, switch from base 10 to base 16, and check that <tt>1000 - 2</tt> is <tt>FFE</tt>, not <tt>998</tt>. My bc has no prompt, so I will use <tt>'''&raquo;bold text'''</tt> to show where I typed my input.

 $ '''&raquo;bc'''
 '''&raquo;obase = ibase = 16'''
 '''&raquo;1000 - 2'''
 FFE

Step 2. I define a helper function <tt>d()</tt> to count the number of digits in a number. I check that <tt>d()</tt> works for positive numbers.

 '''&raquo;define d(a) {'''
 '''&raquo;  auto r'''
 '''&raquo;  r = 0'''
 '''&raquo;  while (a != 0) {'''
 '''&raquo;    r += 1'''
 '''&raquo;    a /= 10'''
 '''&raquo;  }'''
 '''&raquo;  return (r)'''
 '''&raquo;}'''
 '''&raquo;d(4)'''
 1
 '''&raquo;d(5A)'''
 2
 '''&raquo;d(67B)'''
 3

Step 3. I define <tt>f()</tt> and call it.

 '''&raquo;define f(a, b, c) {'''
 '''&raquo;  auto d, e, f '''
 '''&raquo;  d = d(b)'''
 '''&raquo;  e = d + d(c)'''
 '''&raquo;  f = e + d(c)'''
 '''&raquo;  return (a * 10 ^ f + c * 10 ^ e + c * 10 ^ d + b)'''
 '''&raquo;}'''
 '''&raquo;f(205E77A, C0DE, 1)'''
 205E77A11C0DE

Step 4. I quit. (Control-D also works.)

 '''&raquo;quit'''
 $


## Bracmat

Running Bracmat without arguments starts the program in interactive mode. The prompt is <code>{?}</code>. After evaluation, Bracmat prints <code>{!}</code> followed by the result. Input can extend over multiple lines, but is terminated by a new line if all parentheses are balanced (<code>()</code> or <code>{}</code>) and any string introduced with <code>"</code> also is closed with <code>"</code>. The number of needed closing parentheses is indicated at the start of a new input line. Provided the file <code>help</code> is in the current working directory, the user can get interactive help by entering <code>get$help</code>. The program is closed by entering an extra closing parenthesis, followed by a confirming <code>y</code>.


```txt
D:\projects\Bracmat>bracmat
Bracmat version 5, build 105 (1 December 2011)
Copyright (C) 2002 Bart Jongejan
Bracmat comes with ABSOLUTELY NO WARRANTY; for details type `!w'.
This is free software, and you are welcome to redistribute it
under certain conditions; type `!c' for details.



{?} get$help { tutorial }
{?} )        { stop }
{?} (f=a b c.
{1}   !arg:(?a,?b,?c)
{1}   & str$(!a !c !b)
{1}   )
{!} f
    S   0,00 sec  (2156.2173.0)
{?} f$(Rosetta,Code,":")
{!} Rosetta:Code
    S   0,00 sec  (2156.2173.0)
{?}
```



## Brat


```brat
$ brat
# Interactive Brat
brat:1> f = { a, b, s | a + s + s + b }
#=> function: 0xb737ac08
brat:2> f "Rosetta" "Code" ":"
#=> Rosetta::Code
brat:3> quit
Exiting
```



## Burlesque



```burlesque

C:\Burlesque>Burlesque.exe --shell
blsq ) {+]?+}hd"Rosetta""Code"':!a
"Rosetta:Code"
blsq )

```



## C#

Visual Studio 2015 and above come with a C# REPL (called the C# Interactive Compiler), which can be launched as '''csi''' from the Visual Studio developer command prompt.


```txt
**********************************************************************
** Visual Studio 2017 Developer Command Prompt v15.9.14
** Copyright (c) 2017 Microsoft Corporation
**********************************************************************

C:\Program Files (x86)\Microsoft Visual Studio\2017\Community>csi /?
Microsoft (R) Visual C# Interactive Compiler version 2.10.0.0
Copyright (C) Microsoft Corporation. All rights reserved.

Usage: csi [option] ... [script-file.csx] [script-argument] ...

Executes script-file.csx if specified, otherwise launches an interactive REPL (Read Eval Print Loop).

Options:
  /help                          Display this usage message (alternative form: /?)
  /version                       Display the version and exit
  /i                             Drop to REPL after executing the specified script.
  /r:<file>                      Reference metadata from the specified assembly file (alternative form: /reference)
  /r:<file list>                 Reference metadata from the specified assembly files (alternative form: /reference)
  /lib:<path list>               List of directories where to look for libraries specified by #r directive.
                                 (alternative forms: /libPath /libPaths)
  /u:<namespace>                 Define global namespace using (alternative forms: /using, /usings, /import, /imports)
  @<file>                        Read response file for more options
  --                             Indicates that the remaining arguments should not be treated as options.
```



```txt
C:\Program Files (x86)\Microsoft Visual Studio\2017\Community>csi
Microsoft (R) Visual C# Interactive Compiler version 2.10.0.0
Copyright (C) Microsoft Corporation. All rights reserved.

Type "#help" for more information.
> string f(string s1, string s2, char sep) => s1+sep+sep+s2;
> f("Rosetta", "Code", ':')
"Rosetta::Code"
>
```



## Clojure

With ''clojure.jar'' on the Java classpath, the Clojure REPL is invoked with ''java clojure.main''.

```lisp

Clojure 1.1.0
user=> (defn f [s1 s2 sep] (str s1 sep sep s2))
#'user/f
user=> (f "Rosetta" "Code" ":")
"Rosetta::Code"
user=>

```



## CoffeeScript

With the [https://www.npmjs.com/package/coffee-script coffee-script] package for [https://www.npmjs.org/ NPM] on [http://nodejs.org/ Node.js]. The ''-n'' flag is '--node'.

```CoffeeScript

$ coffee -n
coffee> f = (a, b, c) -> a + c + c + b
[Function]
coffee> f "Rosetta", "Code", ":"
"Rosetta::Code"

```



## Common Lisp


The details of interactive use vary widely between implementations; this example is from [[SBCL]]. <code>*</code> is the prompt. By default, SBCL compiles (not interprets) all code, unless <code>[http://www.sbcl.org/manual/Interpreter.html sb-ext:*evaluator-mode*]</code> is changed.

```lisp
$ rlwrap sbcl
This is SBCL 1.0.25, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.
...
* (defun f (string-1 string-2 separator)
    (concatenate 'string string-1 separator separator string-2))

F
* (f "Rosetta" "Code" ":")

"Rosetta::Code"
*
```


=={{header|Déjà Vu}}==

The interactive interpreter provides <code>.</code> as a shortcut for <code>!.</code> and <code>;</code> as a shortcut for <code>!(print-stack)</code>.


```dejavu
$ vu
<1:1> f str1 str2 sep:
<1:2>     join sep [ str2 "" str1 ]
<1:3>
<2:1> . f "Rosetta" "Code" ":"
"Rosetta::Code"
```



## E



```e
$ rune   # from an OS shell. On Windows there is also a desktop shortcut.
```


"<code>?</code>" and "<code>&gt;</code>" are prompts for input; "<code>#</code>" marks output.


```e
? def f(string1 :String, string2 :String, separator :String) {
>     return separator.rjoin(string1, "", string2)
> }
# value: <f>

? f("Rosetta", "Code", ":")
# value: "Rosetta::Code"
```


If you type a definitely incomplete expression, such as "<code>def f() {</code>", then it gives an "<code>&gt;</code>" prompt and takes additional lines. If the expression is not necessarily incomplete, you can continue anyway by ending a line with "<code>\</code>".


## EchoLisp

EchoLisp runs in a browser window, and is -par essence - interactive. To start the REPL mode, type http://www.echolalie.org/echolisp in the browser URL address field.

```scheme

;; screen copy of the REPL
;; note that the &i variables remember expression evaluation, and may be used in other expressions

EchoLisp - 2.16.2
📗 local-db: db.version: 3
(define ( f s1 s2 s3) (string-append s1 s3 s3 s2))
[0]→ f
(f "Rosetta" "Code" ":")
[1]→ "Rosetta::Code"
(+ 4 8)
[2]→ 12
(* 4 8)
[3]→ 32
(* &2 &3)
[4]→ 384
(f &1 &1 ":")
[5]→ "Rosetta::Code::Rosetta::Code"

;; etc.

```



## Elixir

Elixir's Interactive REPL is IEx, Interactive Elixir. To open IEx, open a shell and type: iex

The 'def' keyword isn't allowed outside of modules in Elixir, so the easiest way to write a function in iex is usually by writing an anonymous function.

```Elixir
iex(1)> f = fn str1,str2,sep -> [str1,"",str2] |> Enum.join(sep) end # Join list on separator
iex(2)> g = fn str1,str2,sep -> str1 <> sep <> sep <> str2 end       # Or concatenate strings

iex(3)> defmodule JoinStrings do
...(3)>   def f(str1,str2,sep), do: [str1,"",str2] |> Enum.join(sep)
...(3)>   def g(str1,str2,sep), do: str1 <> sep <> sep <> str2
...(3)> end
```


'''Example:'''

```Elixir
iex(4)> f.("Rosetta","Code",":")
"Rosetta::Code"
iex(5)> JoinStrings.f("Rosetta","Code",":")
"Rosetta::Code"
```



## Emacs Lisp

[[Emacs]] opens a *scratch* buffer by default. This buffer is in ''Lisp Interaction'' mode; <code>C-j</code> evaluates the Lisp expression before point, and prints the result.

Switch to the buffer (<code>C-x b *scratch*</code>, or use the Buffers menu), type some Lisp expressions, and press <code>C-j</code> after each expression.


```lisp
(defun my-join (str1 str2 sep)
  (concat str1 sep sep str2))
my-join
(my-join "Rosetta" "Code" ":")
"Rosetta::Code"
```


Emacs also provides ''ielm'', the interactive Emacs Lisp mode. Start it with <code>M-x ielm</code>, type some expressions and press <code>RET</code>.


```lisp
*** Welcome to IELM ***  Type (describe-mode) for help.
ELISP> (defun my-join (str1 str2 sep)
	 (concat str1 sep sep str2))
my-join
ELISP> (my-join "Rosetta" "Code" ":")
"Rosetta::Code"
ELISP>
```



## Erlang


```erlang
$erl
1> F = fun(X,Y,Z) -> string:concat(string:concat(X,Z),string:concat(Z,Y)) end.
#Fun<erl_eval.18.105910772>
2> F("Rosetta", "Code", ":").
"Rosetta::Code"

```



## ERRE

ERRE hasn't no ''interactive mode''. You can only execute the R-Code Interpreter in this way:
from main menu do Utilità --> Dos Shell and then from command prompt RCODE <Enter> and then type this

```txt
 r$="Rosetta"
 Ok   <--- from interpreter
 c$="Code"
 Ok   <--- from interpreter
 s$="-"
 Ok   <--- from interpreter
 PRINT r$+s$+c$
 Rosetta-Code
 Ok   <--- from interpreter

```

Type
 system
to return to command prompt.


## Factor

Factor comes with a graphical interpreter called the listener. The listener can also be run in a console with the following command:
 ./factor -run=listener

```factor
( scratchpad ) : cool-func ( w1 w2 sep -- res ) dup append glue ;
( scratchpad ) "Rosetta" "Code" ":" cool-func .
"Rosetta::Code"
```



## Fantom


Fantom comes with a command-line interpreter called 'fansh'


```txt

$ fansh
Fantom Shell v1.0.57 ('?' for help)
fansh> f := |Str a, Str b, Str c -> Str| {"$a$c$c$b"}
|sys::Str,sys::Str,sys::Str->sys::Str|
fansh> f("Rosetta", "Code", ":")
Rosetta::Code

```



## Forth

All Forth systems come with an interpreter. On embedded systems, the interpreter functions as a monitor or lightweight operating system.

```forth
$ gforth
Gforth 0.7.0, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
  ok
: f ( separator suffix prefix -- )  compiled
   pad place  2swap 2dup   compiled
   pad +place    compiled
   pad +place    compiled
   pad +place  compiled
   pad count ;  ok
  ok
 s" :" s" Code" s" Rosetta" f cr type
Rosetta::Code ok
```



## FreeBASIC


FreeBASIC is a compiler rather than an interpreter and doesn't have an interactive mode as such.

However, as simple programs can be written in a few lines and compile and execute very quickly, I think it's fair to say that the lack of an interpreter is not considered much of a drawback by most FB developers.

For example, the current task can be coded as follows and compiles/runs instantly :


```freebasic
' FB 1.05.0 Win64

Dim As String s1, s2, sep
Input "First string  "; s1
Input "Second string "; s2
Input "Separator     "; sep
Print : Print  s1 + sep + sep + s2
Sleep
```


```txt

First string  ? Rosetta
Second string ? Code
Separator     ? :

Rosetta::Code

```


=={{header|F_Sharp|F#}}==
The F# interpreter is called '''fsi'''. As F# accepts multi-line input it will not evaluate the input until you enter two semi-colons together.

```fsharp
Microsoft F# Interactive, (c) Microsoft Corporation, All Rights Reserved
F# Version 1.9.6.2, compiling for .NET Framework Version v2.0.50727

Please send bug reports to fsbugs@microsoft.com
For help type #help;;

> let f a b sep = String.concat sep [a; ""; b] ;;

val f : string -> string -> string -> string

> f "Rosetta" "Code" ":" ;;
val it : string = "Rosetta::Code"
```


## GAP


```gap
~% gap

            #########           ######         ###########           ###
         #############          ######         ############         ####
        ##############         ########        #############       #####
       ###############         ########        #####   ######      #####
      ######         #         #########       #####    #####     ######
     ######                   ##########       #####    #####    #######
     #####                    ##### ####       #####   ######   ########
     ####                    #####  #####      #############   ###  ####
     #####     #######       ####    ####      ###########    ####  ####
     #####     #######      #####    #####     ######        ####   ####
     #####     #######      #####    #####     #####         #############
      #####      #####     ################    #####         #############
      ######     #####     ################    #####         #############
      ################    ##################   #####                ####
       ###############    #####        #####   #####                ####
         #############    #####        #####   #####                ####
          #########      #####          #####  #####                ####

     Information at:  http://www.gap-system.org
     Try '?help' for help. See also  '?copyright' and  '?authors'

   Loading the library. Please be patient, this may take a while.
GAP4, Version: 4.4.12 of 17-Dec-2008, x86_64-unknown-linux-gnu-gcc
Components:  small 2.1, small2 2.0, small3 2.0, small4 1.0, small5 1.0, small6 1.0, small7 1.0, small8 1.0,
             small9 1.0, small10 0.2, id2 3.0, id3 2.1, id4 1.0, id5 1.0, id6 1.0, id9 1.0, id10 0.1, trans 1.0,
             prim 2.1  loaded.
Packages:    AClib 1.1, Polycyclic 2.6, Alnuth 2.2.5, AutPGrp 1.4, CrystCat 1.1.3, Cryst 4.1.6, CRISP 1.3.2,
             CTblLib 1.1.3, TomLib 1.1.4, FactInt 1.5.2, GAPDoc 1.2, FGA 1.1.0.1, IRREDSOL 1.1.2, LAGUNA 3.5.0,
             Sophus 1.23, Polenta 1.2.7, ResClasses 2.5.3  loaded.
gap> join := function(a, b, sep)
>   return Concatenation(a, sep, sep, b);
> end;
function( a, b, sep ) ... end
gap>
gap> join("Rosetta", "Code", ":");
"Rosetta::Code"
gap>
```


## Go

To satisfy some of the desire for a REPL, Go includes a browser-based "playground" that compiles and executes directly from a browser edit box.  You still have to type complete programs with all the usual boilerplate, but at least you don't have to create a source file, run the compiler, run the linker, and run the program.  It has a convenient checkbox for "compile and run after each keystroke" that works quite well.  You just type, and as soon as your program is valid, you see output.

Running goplay takes two steps.  First, start the program with "goplay" on the command line, then visit <nowiki>http://localhost:3999/</nowiki> with a browser.

The complete program satisfying the task is,

```go
package main

import "fmt"

func f(s1, s2, sep string) string {
	return s1 + sep + sep + s2
}

func main() {
	fmt.Println(f("Rosetta", "Code", ":"))
}
```


It works well to enter the program, check "every keystroke" to see syntax errors from whatever silly oversights you made, then fix them one by one until your desired output appears.

```txt

Rosetta::Code

```



## Groovy

The '''groovysh''' interpreter requires a command-line interpreter (terminal) environment in which to run. This example was run under the CMD command-line interpreter on Microsoft Windows XP.

```groovy>C:\Apps\groovy
groovysh
Groovy Shell (1.6.2, JVM: 1.6.0_13)
Type 'help' or '\h' for help.
---------------------------------------------------------------------------------------------------
groovy:000> f = { a, b, sep -> a + sep + sep + b }
===> groovysh_evaluate$_run_closure1@5e8d7d
groovy:000> println f('Rosetta','Code',':')
Rosetta::Code
===> null
groovy:000> exit

C:\Apps\groovy>
```



## Haskell


The details of interactive use vary widely between implementations. This example is from [[GHC|GHCi]].


```haskell
$ ghci
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4.2, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Prelude> let f as bs sep = as ++ sep ++ sep ++ bs
Prelude> f "Rosetta" "Code" ":"
"Rosetta::Code"
```



## HicEst

Start HicEst e.g. with:

"c:\Program Files\HicEst\HicEst.exe E:\Rosetta\Interactive_programming.hic f('Rosetta', 'Code', ':')"

Type the following script. Each line is executed (and incrementally compiled) when it is typed:

```HicEst
CHARACTER A*40, B*40, C*40

READ(Text=$CMD_LINE, Format="'','','',") A, B, C
WRITE(ClipBoard, Name) A, B, C               ! A=Rosetta; B=Code; C=:;

WRITE(ClipBoard) TRIM(A) // ':' // TRIM(C) // TRIM(B)  ! Rosetta::Code
```



## Huginn

[[File:huginn_interactive.gif]]


## Io


```txt
$ io
Io 20110905
Io> f := method(str1,str2,sep,
... str1 .. sep .. sep .. str2)
==> method(str1, str2, sep,
    str1 .. sep .. sep .. str2
)
Io> f("Rosetta","Code",":")
==> Rosetta::Code
Io> writeln("I am going to exit now")
I am going to exit now
==> nil
Io> exit
$
```

The function could have been written on one line but I wanted to show multi-line input with the continuation prompt <code>...</code>.  The result of the expression that is entered appears after <code>==></code> and any output or error message appears before.


## J


J runs in command mode by default. Starting J depends on your operating system and other factors, but typically would involve double clicking on an icon, or starting one of several programs from a command line (j, jwd, jconsole, jee, jhs -- though note that jhs would also require a web browser and visiting a localhost URL).

This is a session log once the os specific stuff has been handled:

```j
   f=: [: ; 0 2 2 1&{
   f 'Rosetta';'Code';':'
Rosetta::Code
```



## Java



```java

public static void main(String[] args) {
    System.out.println(concat("Rosetta", "Code", ":"));
}

public static String concat(String a, String b, String c) {
   return a + c + c + b;
}

Rosetta::Code

```



## JavaScript

```javascript
$ java -cp js.jar org.mozilla.javascript.tools.shell.Main
Rhino 1.7 release 2 2009 03 22
js> function f(a,b,s) {return a + s + s + b;}
js> f('Rosetta', 'Code', ':')
Rosetta::Code
js> quit()
$
```



## Jsish


```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]'
# function f(a:string, b:string, s:string):string { return a+s+s+b; }
# f('Rosetta', 'Code', 1)
warn: type mismatch for argument arg 3 's': expected "string" but got "number", in call to 'f' <1>.    (at or near "Code")

"Rosetta11Code"
# f('Rosetta', 'Code', ':')
"Rosetta::Code"
```



## Julia

<code>Julia</code> has a fine <code>REPL</code> which is invoked by the command <code>julia</code> when no arguments are supplied.

```txt

usr@host:~/rosetta/julia$ julia
               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type "help()" for help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 0.3.7 (2015-03-23 21:36 UTC)
 _/ |\__'_|_|_|\__'_|  |  Official http://julialang.org release
|__/                   |  x86_64-linux-gnu

julia> function strcat(a::String, b::String, sep::String)
                  a*(sep^2)*b
       end
strcat (generic function with 1 method)

julia> strcat("rosetta", "code", "_")
"rosetta__code"

julia>

```



## K

```k
$ rlwrap k
K Console - Enter \ for help
  f:{x,z,z,y}
  f["Rosetta";"Code";":"]
"Rosetta::Code"
```



## Kotlin

Kotlin has an interactive shell for the JVM which can be started by simply typing 'kotlinc' at the command line. Here's a sample interactive session to accomplish the specified task on Windows 10:

```scala
c:\kotlin-compiler-1.0.6>kotlinc
Welcome to Kotlin version 1.0.6-release-127 (JRE 1.8.0_31-b13)
Type :help for help, :quit for quit
>>> fun f(s1: String, s2: String, sep: String) = s1 + sep + sep + s2
>>> f("Rosetta", "Code", ":")
Rosetta::Code
>>> :quit
```



## Lasso


```Lasso
#!/usr/bin/lasso9

// filename: interactive_demo

define concatenate_with_delimiter(
	string1::string,
	string2::string,
	delimiter::string

) => #string1 + (#delimiter*2) + #string2

define read_input(prompt::string) => {

	local(string)

	// display prompt
	stdout(#prompt)
	// the following bits wait until the terminal gives you back a line of input
	while(not #string or #string -> size == 0) => {
		#string = file_stdin -> readsomebytes(1024, 1000)
	}
	#string -> replace(bytes('\n'), bytes(''))

	return #string -> asstring
}

local(
	string1,
	string2,
	delimiter
)

// get first string
#string1 = read_input('Enter the first string: ')

// get second string
#string2 = read_input('Enter the second string: ')

// get delimiter
#delimiter = read_input('Enter the delimiter: ')

// deliver the result
stdoutnl(concatenate_with_delimiter(#string1, #string2, #delimiter))
```


Called from the command line: ./interactive_demo

Result -> Rosetta::Code


## Lingo

Lingo/Director is usually started as GUI program and has no access to the standard system console (unless using a binary plugin). But it has its own interactive Lingo console called "Message Window", that can be activated/displayed by executing "_player.debugPlaybackEnabled=1". Using this Message Window, the task can be solved like this:

```lingo>
 m=new(#script)
> m.scripttext="on conc(a,b,c)"&RETURN&"return a&c&c&b"&RETURN&"end"
> put conc("Rosetta", "Code", ":")
-- "Rosetta::Code"
```



## Logo

```logo
$ <i>logo</i>
Welcome to Berkeley Logo version 5.6
? <i>to f :prefix :suffix :separator</i>
> <i>output (word :prefix :separator :separator :suffix)</i>
> <i>end</i>
f defined
? <i>show f "Rosetta "Code ":</i>
Rosetta::Code
?
```



## Lua


```lua
$ lua
Lua 5.1.2  Copyright (C) 1994-2007 Lua.org, PUC-Rio
> function conc(a, b, c)
>> return a..c..c..b
>> end
> print(conc("Rosetta", "Code", ":"))
Rosetta::Code
>
```



## M2000 Interpreter

M2000 run through an environment (the M2000 Environment), and this can run a script by using command line arguments, or run in interactive mode, just opening the environment, in full screen.

To exit interactive mode just type END and press enter after prompt >, Old commands are back again using arrows up or down. We can open editor for these commands (internal editor has syntax color).

Command line has a some special commands (we need to use Set to send command to this line, from a program), to change many things. Type Help All to see help for all topics, or type Help Print to see help for Print.

We an use ? as a Print command. Any variable, array, module, function, group we make at command line is global.

We can make  functions using edit, so edit f$() open editor and we write these
<pre >
>edit f$()
</pre >
(we see Function F$() at the header of editor)

```M2000 Interpreter

Read name1$, name2$, sep$
=name1$+sep$+sep$+name2$

```

So now we press Esc and return to command line (editor open as a layer, so console has the last state). Using Clipboard we sen unicode string to clipboard
<pre >
>? f$("Rosseta","Code",":")
Rosseta::Code
>clipboard f$("Rosseta","Code",":")
>New  ' now we clear any stored function/module
>function f$(a$,b$,c$) {=a$+c$+c$+b$}    'in one line function
>? f$("Rosseta","Code",":")
Rosseta::Code
>Save ros1
>New
>Load ros1
>? f$("Rosseta","Code",":")
Rosseta::Code
>New
>f$=Lambda$ (a$,b$,c$)->a$+c$+c$+b$
>? f$("Rosseta","Code",":")
Rosseta::Code
>B$=F$  ' lambda functions as variables.
>? B$("Rosseta","Code",":")
Rosseta::Code
</pre >



## M4

Here is a terminal session with output lines marked by "==>":

```M4
$ m4
define(`f',`$1`'$3`'$3`'$2')
==>
f(`Rosetta',`Code',`:')
==>Rosetta::Code
m4exit
```




## Maple

Start Maple's commandline interface by issuing the command <code>maple</code> in a terminal shell. Ie,

```txt
$ maple
```

Then enter the Maple commands.

```Maple
f := (a,b,c)->cat(a,c,c,b):

f("Rosetta","Code",":");
```

Output:

```txt
                        "Rosetta::Code"
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
$ math
```


```Mathematica
f[x_,y_,z_]:=Print[x,z,z,y]
->""
f["Rosetta","Code",":"]
->Rosetta::Code
```


=={{header|MATLAB}} / {{header|Octave}}==
(Has been tested only with MATLAB)

Define an anonymous function in the Command Window

```Matlab

>> f = @(str1, str2, delim) [str1, delim, delim, str2];

```


Call of function and {{out}}

```txt

>> f('Rosetta', 'Code', ':')

ans =

Rosetta::Code

```



## Maxima


Simply run wxMaxima, xMaxima (or simply maxima in a Linux or Windows command line) to start a REPL.


```maxima
(%i1) f(a, b, c) := sconcat(a, c, c, b)$
(%i2) f("Rosetta", "Code", ":");
(%o2) "Rosetta::Code"
```



## ML/I

The following is exactly what should be fed to ML/I to start a suitable interactive session.
Start ML/I with (e.g.:  $ ml1), then type:


```ML/I
MCSKIP MT,<>
MCINS %.
MCDEF F WITHS (,,)
AS <%WA1.%WA3.%WA2.%WA2.>
```



## Nim

Use <code>nimble</code> (Nim's package manager) to install a REPL ('''nrpl''' or '''tnim''')

Run the REPL and paste the following code


```txt

proc f(x, y, z: string) = echo x & z & z & y
f("Rosetta", "Code", ":")

```


which outputs


```txt
Rosetta::Code
```


To get working arrow keys, compile nim with readline, like this: <code>./koch boot -d:release -d:useGnuReadline</code> or just run <code>rlwrap nim i</code> instead.


## OCaml


The default interactive interpreter is called the toplevel. In the toplevel because we can enter expressions that span multiple lines, we have to type the double semicolon (";;") at the end of an expression so that it knows we are done.


```ocaml
$ ocaml
        Objective Caml version 3.12.1

# let f s1 s2 sep = String.concat sep [s1; ""; s2];;
val f : string -> string -> string -> string = <fun>
# f "Rosetta" "Code" ":";;
- : string = "Rosetta::Code"
#
```


Also a lot of OCaml users invoke the toplevel with rlwrap or ledit to gain readline capabilities:


```ocaml
$ rlwrap ocaml
```


There is also [http://benediktmeurer.de/ocamlnat/ ocamlnat] that is a native toplevel. It permits interactive use of the OCaml system through a read-eval-print loop, similar to the standard OCaml toplevel, but up to 100 times faster.<BR>
Using a module in the default toplevel is made with <code>.cma</code> files, with '''ocamlnat''' just use <code>.cmxs</code> files instead.


## Octave


```octave
$ octave
GNU Octave, version 3.0.2
Copyright (C) 2008 John W. Eaton and others.
This is free software; see the source code for copying conditions.
There is ABSOLUTELY NO WARRANTY; not even for MERCHANTIBILITY or
FITNESS FOR A PARTICULAR PURPOSE.  For details, type `warranty'.

Octave was configured for "i586-mandriva-linux-gnu".

Additional information about Octave is available at http://www.octave.org.

Please contribute if you find this software useful.
For more information, visit http://www.octave.org/help-wanted.html

Report bugs to <bug@octave.org> (but first, please read
http://www.octave.org/bugs.html to learn how to write a helpful report).

For information about changes from previous versions, type `news'.

octave:1> function concat(a,b,c)
> disp(strcat(a,c,c,b));
> endfunction
octave:2> concat("Rosetta","Code",":");
Rosetta::Code
octave:3>
```



## Oforth


Oforth interpreter is started using --i command line parameter :


```Oforth
oforth --i
```


Into the interpreter, you can create functions :


```Oforth
: x(a, b, sep)   a sep + sep + b + ;
ok
>x("Rosetta", "Code", ":")
ok
>.s
[1] (String) Rosetta::Code
ok
>
```


Alternatively you don't need the local variables


```Oforth>
 : x   dup rot + + + ;
ok
> "Rosetta" "Code" ";" x .s
[1] (String) Rosetta::Code
ok
>
```



## ooRexx

ooRexx ships a program rexxtry.rex which does exactly what this task asks for.
```txt
D:\>rexx rexxtry ?
 This procedure lets you interactively try REXX statements.
 If you run it with no parameter, or with a question mark
 as a parameter, it will briefly describe itself.
 You may also enter a REXX statement directly on the command line
 for immediate execution and exit.  Example:  rexxtry call show

 Enter 'call show' to see user variables provided by REXXTRY.
 Enter '=' to repeat your previous statement.
 Enter '?' to invoke system-provided online help for REXX.
 The subroutine named 'sub' can be CALLed or invoked as 'sub()'.
 REXXTRY can be run recursively with CALL.

 Except for the signal instructions after a syntax error, this
 procedure is an example of structured programming.

D:\>rexx rexxtry
REXX-ooRexx_4.2.0(MT)_64-bit 6.04 22 Feb 2014
  rexxtry.rex lets you interactively try REXX statements.
    Each string is executed when you hit Enter.
    Enter 'call tell' for a description of the features.
  Go on - try a few...            Enter 'exit' to end.
x=3
  ........................................... rexxtry.rex on WindowsNT
y=x**2
  ........................................... rexxtry.rex on WindowsNT
say x y
3 9
  ........................................... rexxtry.rex on WindowsNT

  rexxtry.rex:  Enter 'exit' to end.       Or '?' for online REXX help.
say sigl
160
  ........................................... rexxtry.rex on WindowsNT
say sourceline(12)
/* Redistribution and use in source and binary forms, with or                 */
  ........................................... rexxtry.rex on WindowsNT
exit

D:\>
```

For the specific test I need to create the function on my disk

```txt
H:\>rexx rexxtry
REXX-ooRexx_4.2.0(MT)_64-bit 6.04 22 Feb 2014
  rexxtry.rex lets you interactively try REXX statements.
    Each string is executed when you hit Enter.
    Enter 'call tell' for a description of the features.
  Go on - try a few...            Enter 'exit' to end.
pgm='ff.rex'; Call lineout pgm,'return arg(1)||arg(3)||arg(3)||arg(2)';Call line
out pgm
  ........................................... rexxtry.rex on WindowsNT
say ff('AAA','BBB',':')
AAA::BBB
  ........................................... rexxtry.rex on WindowsNT
exit
```



## Oz

Mozart supports this style of programming with its Emacs interface.
Go to the "Oz" buffer and enter

```oz
declare fun {F As Bs Sep} {Append As Sep|Sep|Bs} end
```

Press C-. C-l to evaluate the line.

Now enter

```oz
{System.showInfo {F "Rosetta" "Code" &:}}
```

and again press C-. C-l to execute the code.
You will see the result in the "*Oz Emulator*" buffer.


## PARI/GP

gp *is* a REPL built on the PARI library. You can start it from the command line with <code>gp</code>, though you may wish to change to your Pari directory first so it can read your <code>.gprc</code> file. Alternatively, if you are using a GUI, double-click the shortcut.


```parigp
f(s1,s2,sep)=Str(s1, sep, sep, s2);
```



## Perl

Perl doesn't have an interpreter, but there is an interactive debugger:

```perl
$ perl -de1

Loading DB routines from perl5db.pl version 1.3
Editor support available.

Enter h or `h h' for help, or `man perldebug' for more help.

main::(-e:1):   1
  DB<1> sub f {my ($s1, $s2, $sep) = @_; $s1 . $sep . $sep . $s2}

  DB<2> p f('Rosetta', 'Code', ':')
Rosetta::Code
  DB<3> q
```


Alternative way:


```perl
$ perl
# Write the script here and press Ctrl+D plus ENTER when finished (^D means Ctrl+D):
sub f {my ($s1, $s2, $sep) = @_; $s1 . $sep . $sep . $s2};
print f('Rosetta', 'Code', ':');
^D
Rosetta::Code
$
```


Another:


```perl
$ perl -lpe '$_=eval||$@'
sub f { join '' => @_[0, 2, 2, 1] }

f qw/Rosetta Code :/
Rosetta::Code
```



## Perl 6

Using [[Rakudo]].

```perl6
$ rakudo/perl6
> sub f($str1,$str2,$sep) { $str1~$sep x 2~$str2 };
f
> f("Rosetta","Code",":");
Rosetta::Code
>

```



## PicoLisp


```bash
$ pil +
```


```PicoLisp
: (de f (Str1 Str2 Sep)
   (pack Str1 Sep Sep Str2) )
-> f

: (f "Rosetta" "Code" ":")
-> "Rosetta::Code"
```



## Pike


```pike
$ pike
Pike v7.8 release 352 running Hilfe v3.5 (Incremental Pike Frontend)
> string f(string first, string second, string sep){
>>   return(first + sep + sep + second);
>> }
> f("Rosetta","Code",":");
(1) Result: "Rosetta::Code"
>
```



## PowerShell

PowerShell itself is already a shell and therefore an interactive environment is the default.

```powershell
Windows PowerShell
Copyright (C) 2009 Microsoft Corporation. All rights reserved.

PS Home:\> function f ([string] $string1, [string] $string2, [string] $separator) {
>>     $string1 + $separator * 2 + $string2
>> }
>>
PS Home:\> f 'Rosetta' 'Code' ':'
Rosetta::Code
PS Home:\>
```



## Prolog

Works with SWI-Prolog.<BR>
Prolog works in its own environnment.<BR>
Start the interpreter by typing pl at the command line (or by clicking on the exe).

```Prolog
% library(win_menu) compiled into win_menu 0.00 sec, 12,872 bytes
% library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 2,404 bytes
% The graphical front-end will be used for subsequent tracing
% c:/users/joel-seven/appdata/roaming/swi-prolog/pl.ini compiled 0.13 sec, 876,172 bytes
XPCE 6.6.66, July 2009 for Win32: NT,2000,XP
Copyright (C) 1993-2009 University of Amsterdam.
XPCE comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
The host-language is SWI-Prolog version 5.10.0

For HELP on prolog, please type help. or apropos(topic).
         on xpce, please type manpce.

1 ?- assert((f(A, B,C) :- format('~w~w~w~w~n', [A, C, C, B]))).
true.

2 ?- f('Rosetta', 'Code', ':').
Rosetta::Code
true.

3 ?-

```



## Python

Start the interpreter by typing python at the command line (or select it from a menu). You get a response showing the version of the interpreter being run before giving an input prompt of three greater-than characters and a space:


```python
python
Python 2.6.1 (r261:67517, Dec  4 2008, 16:51:00) [MSC v.1500 32 bit (Intel)] on
win32
Type "help", "copyright", "credits" or "license" for more information.
>>> def f(string1, string2, separator):
	return separator.join([string1, '', string2])

>>> f('Rosetta', 'Code', ':')
'Rosetta::Code'
>>>
```



## R


```r
$ R

R version 2.7.2 (2008-08-25)
Copyright (C) 2008 The R Foundation for Statistical Computing
ISBN 3-900051-07-0

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> f <- function(a, b, s) paste(a, "", b, sep=s)
> f("Rosetta", "Code", ":")
[1] "Rosetta::Code"
> q()
Save workspace image? [y/n/c]: n
```



## Racket


Much like Scheme, Racket features a full-featured REPL:


```Racket>oiseau:/tmp
 racket
Welcome to Racket v5.3.3.5.
> (define (f string-1 string-2 separator)
    (string-append string-1 separator separator string-2))
> (f "Rosetta" "Code" ":")
"Rosetta::Code"
> ^D
oiseau:/tmp>
```



## REBOL

Start the REBOL/Core interpreter in quiet mode with -q. q to quit.


```rebol
$ rebol -q
>> f: func [a b s] [print rejoin [a s s b]]
>> f "Rosetta" "Code" ":"
Rosetta::Code
>> q
```



## Retro

Retro's interpreter is started automatically. Once you see the "ok" prompt, you can proceed to enter code:


```Retro
:f (sss-s) [ s:prepend ] sip s:prepend s:append ;
'Rosetta 'Code ': f
```



## REXX


### version 1


```rexx
/*REXX*/  parse arg a b c
say f(a,b,c)
exit
f:return arg(1)arg(3)arg(3)arg(2)
```



### version 2

This REXX version allows blanks to be included in the three strings.

```rexx
/*REXX program demonstrates  interactive programming  by using a  function [F].         */
say f('Rosetta',  "Code",  ':')
say f('The definition of a trivial program is ',  " one that has no bugs.",  '───')
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:  return arg(1) || copies(arg(3),2) || arg(2)  /*return the required string to invoker*/
```

'''output'''   when using the (internal) default strings:

```txt

Rosetta::Code
The definition of a trivial program is ────── one that has no bugs.

```



### version 3

This REXX version is more to the keeping of the spirit of the task requirements.

```rexx
/*REXX program demonstrates  interactive programming  by using a  function [F].         */
say '══════════════════ enter the function  F  with three comma-separated arguments:'
parse pull funky
interpret  'SAY'  funky
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:  return arg(1) || copies(arg(3),2) || arg(2)  /*return the required string to invoker*/
```

'''output'''  (a screen scraping is shown)


'''int_act3''' is the name of the REXX program  [INT_ACT3.REX)

```txt

─────────────────────────────────────────────────────────Thu 10/18/2012 15:00:01
C:\►int_act3
══════════════════ enter the function  F  with three comma-separated arguments:
f("what the",  "hey",  "--")             ◄■■■■■■■■■■■■ this is what the user entered.
what the----hey

─────────────────────────────────────────────────────────Thu 10/18/2012 15:00:38
C:\►

```

Note that the timestamp and the '''  C:\►  ''' text is part of my (wrapped) two-line prompt message.





## Ring


```ring

r = "Rosetta"
c = "Code"
s = ":"
see r+s+s+c

```

Output:

```txt

Rosetta::Code

```



## Ruby

The read-eval-print loop (REPL) for Ruby is ''irb'', the '''''i'''nteractive '''r'''u'''b'''y shell''.

Start the interpreter by typing <code>irb</code> at the command line. You will see an input prompt, which by default is <code>name of this program(name of main object):line number:indent level> </code>:


```ruby
$ irb
irb(main):001:0> def f(string1, string2, separator)
irb(main):002:1>     [string1, '', string2].join(separator)
irb(main):003:1> end
=> :f
irb(main):004:0> f('Rosetta', 'Code', ':')
=> "Rosetta::Code"
irb(main):005:0> exit
$
```


=={{header|S-lang}}==
S-Lang includes a general-purpose shell called <code>slsh</code>.
Command-line arguments are available with:

    slsh --help

Without any arguments it starts from the command-line in [REPL] command-mode:
<lang S-lang>> slsh<Enter>
slsh version 0.9.1-2; S-Lang version: pre2.3.1-23
Copyright (C) 2005-2014 John E. Davis <jed@jedsoft.org>
This is free software with ABSOLUTELY NO WARRANTY.

slsh> define f(s1, s2, sep) {<Enter>
       return(strcat(s1, sep, sep, s2));<Enter>
       }<Enter>
slsh> f("Rosetta", "Code", ":");<Enter>
Rosetta::Code
slsh> quit<Enter>
>
```



## Scala

Scala has a REPL -- Read, Evaluate & Print Loop. Being a compiled language,
everything you type is encapsulated into a compilable template, compiled,
executed, and the return value assigned to a special variable and displayed.

One invokes the REPL by just typing "scala", which is either a shell script
or a batch file depending on one's operating system:


```scala>C:\
scala
Welcome to Scala version 2.8.0.r21356-b20100407020120 (Java HotSpot(TM) Client V
M, Java 1.6.0_05).
Type in expressions to have them evaluated.
Type :help for more information.

scala> "rosetta"
res0: java.lang.String = rosetta
```

Scala's REPL, starting with version 2.8, offers both auto-completion and alternatives when typing TAB. For instance, to check what methods are available on a String, one
may do the following:

```scala>scala
 "rosetta".

!=                    ##                    $asInstanceOf
$isInstanceOf         +                     ==
charAt                clone                 codePointAt
codePointBefore       codePointCount        compareTo
compareToIgnoreCase   concat                contains
contentEquals         endsWith              eq
equals                equalsIgnoreCase      finalize
getBytes              getChars              getClass
hashCode              indexOf               intern
isEmpty               lastIndexOf           length
matches               ne                    notify
notifyAll             offsetByCodePoints    regionMatches
replace               replaceAll            replaceFirst
split                 startsWith            subSequence
substring             synchronized          this
toCharArray           toLowerCase           toString
toUpperCase           trim                  wait

scala> "rosetta".+(":")
res1: java.lang.String = rosetta:
```

One can use `object`, `class`, `trait`, `case object`,
`case class`, `def`, `val` and `var` definitions at any point. However,
`package` and `package object` definitions are not allowed.

```scala>scala
 val str1 = "rosetta"
str1: java.lang.String = rosetta
```

Using these features, one can build the code for a method by testing it
part of it individually:

```scala>scala
 val str2 = "code"
str2: java.lang.String = code

scala> val separator = ":"
separator: java.lang.String = :

scala> str1 + separator + str2
res2: java.lang.String = rosetta:code
```

If one makes a mistake, the REPL will print an error message, and display the
point at which the mistake was made.

```scala>scala
 def (str1: String, str2: String, separator: String) =
<console>:1: error: identifier expected but '(' found.
       def (str1: String, str2: String, separator: String) =
           ^
```

If a definition takes more than a line, the REPL will print an indented "|" sign, and
wait for more input. If one wishes to abort a definition, just enter two consecutive
empty lines.

```scala>scala
 def f(str1: String, str2: String, separator: String) =
     | str1 + separator + str2
f: (str1: String,str2: String,separator: String)java.lang.String

scala> f("rosetta", "code", ":")
res3: java.lang.String = rosetta:code

scala> f("code", "rosetta", ", ")
res4: java.lang.String = code, rosetta
```

Also starting with version 2.8, a line starting with a dot will be interpreted
as a method call on the last result produced.

```scala>scala
 .length
res5: Int = 13

scala>
```

The results are actually displayed with a special function, which pretty prints
some results, and avoid eagerly evaluating others, where that could cause problems
(such as infinite collections).

```scala>scala
 Array(1, 2, 3, 4)
res8: Array[Int] = Array(1, 2, 3, 4)

scala> println(res8)
[I@383244
```

There are many other features, such as the ability to add new jars to
the class path, executing commands on the shell, retrieving the last
exception thrown, etc.


## Scheme

Several interpreters exist for Scheme. These are just some examples.
<lang>> scheme
Scheme Microcode Version 14.9
MIT Scheme running under FreeBSD
Type `^C' (control-C) followed by `H' to obtain information about interrupts.
Scheme saved on Monday June 17, 2002 at 10:03:44 PM
  Release 7.7.1
  Microcode 14.9
  Runtime 15.1

1 ]=> (define (f string-1 string-2 separator)
        (string-append string-1 separator separator string-2))

;Value: f

1 ]=> (f "Rosetta" "Code" ":")

;Value 1: "Rosetta::Code"

1 ]=> ^D
End of input stream reached
Happy Happy Joy Joy.
>
```

<lang>> scheme48
Welcome to Scheme 48 1.8 (made by root on Wed Sep 24 22:37:08 UTC 2008)
Copyright (c) 1993-2008 by Richard Kelsey and Jonathan Rees.
Please report bugs to scheme-48-bugs@s48.org.
Get more information at http://www.s48.org/.
Type ,? (comma question-mark) for help.
> (define (f string-1 string-2 separator)
    (string-append string-1 separator separator string-2))
; no values returned
> (f "Rosetta" "Code" ":")
"Rosetta::Code"
> ^D
Exit Scheme 48 (y/n)? ^D
I'll only ask another 100 times.
Exit Scheme 48 (y/n)? ^D
I'll only ask another 99 times.
Exit Scheme 48 (y/n)? y
>
```


## Sidef


```ruby
$ sidef -i
>>> func f(s1, s2, sep) { s1 + sep*2 + s2 };
f
>>> f('Rosetta', 'Code', ':')
"Rosetta::Code"
>>>
```



## Slate



```slate
slate[1]> s@(String traits) rosettaWith: s2@(String traits) and: s3@(String traits) [s ; s3 ; s3 ; s2].
[rosettaWith:and:]
slate[2]> 'Rosetta' rosettaWith: 'Code' and: ':'.
'Rosetta::Code'
```



## Smalltalk

```smalltalk
$ gst
GNU Smalltalk ready

st> |concat|
st> concat := [ :a :b :c | (a,c,c,b) displayNl ].
a BlockClosure
st> concat value: 'Rosetta' value: 'Code' value: ':'.
Rosetta::Code
'Rosetta::Code'
st>
```

```smalltalk
$ stx --repl
Welcome...

STX> |concat|\
concat := [ :a :b :c | (a,c,c,b) displayNl ].\
concat value: 'Rosetta' value: 'Code' value: ':'.
Rosetta::Code
-> (Answer): 'Rosetta::Code'
STX>
```



## SNOBOL4


```txt

>spitbol code
SPITBOL-386   Release 3.7(ver 1.30.20)   Serial xxxxx
...
Enter SPITBOL statements:
? define('f(a,b,s)'):(z);f f = a s s b:(return);z
Success
?= f('Rosetta','Code',':')
Rosetta::Code
Success
?end
>

```



## Standard ML

Because you can enter expressions that span multiple lines, you have to type the semicolon (";") at the end so that it knows you are done.


```sml
$ sml
Standard ML of New Jersey v110.67 [built: Fri Jul  4 09:00:58 2008]
- fun f (s1, s2, sep) = String.concatWith sep [s1, "", s2];
[autoloading]
[library $SMLNJ-BASIS/basis.cm is stable]
[autoloading done]
val f = fn : string * string * string -> string
- f ("Rosetta", "Code", ":");
val it = "Rosetta::Code" : string
-
```



## Tcl


```tcl
$ tclsh
% proc f {s1 s2 sep} {
    append result $s1 $sep $sep $s2
}
% f Rosetta Code :
Rosetta::Code
% exit
```

A simple alternative (one-liners are most convenient in an interactive shell):

```tcl
$ tclsh
% proc f {a b s} {join [list $a "" $b] $s}
% f Rosetta Code :
Rosetta::Code
%
```


=={{header|TI-89 BASIC}}==

To switch to the interpreter ("home screen"), press the HOME key.


```txt
■ x & s & s & y → f(x,y,s)
                                  Done
■ f("Rosetta", "Code", ":")
                       "Rosetta::Code"
```


Input is left-aligned, output is right-aligned. “→” is typed by pressing STO▸, and “&amp;” by pressing ◆ ×. All whitespace is optional.


## UNIX Shell

```bash
$ sh
sh-3.2$ concat() { echo "$1$3$3$2"; }
sh-3.2$ concat Rosetta Code :
Rosetta::Code
sh-3.2$
```


=
## C Shell
=

```csh
$ csh -f
% alias concat 'echo "\!:1\!:3\!:3\!:2"'
% concat Rosetta Code :
Rosetta::Code
%
```


=
## es
=

```es
$ es
; fn concat a b s { result $a$s$s$b }
; echo <={concat Rosetta Code :}
Rosetta::Code
;
```



## Ursa


```ursa
$ java -jar ursa.jar
cygnus/x ursa v0.76 (default, release 1)
[Oracle Corporation JVM 1.8.0_91 on Linux 3.16.0-4-686-pae i386]
> def f (string s1, string s2, string sep)
..	return (+ s1 sep sep s2)
..end
> out (f "Rosetta" "Code" ":") endl console
Rosetta::Code
> _
```



## Vedit macro language

To enter command mode, type <Esc>c, or to open command mode window, type <Esc>w.
Or if the command mode window is already open, just click on the window.

To define a macro in text register 100:

```vedit
RS(100, "RS(10, @1) RS(10, @3, APPEND) RS(10, @3, APPEND) RS(10, @2, APPEND)")
```


To call the macro:

```vedit
RS(1,"Rosetta") RS(2,"Code") RS(3,":") Call(100)
Message(@10)
```



## XLISP

How to start a REPL depends on the operating system.

```lisp
XLISP 3.3, September 6, 2002 Copyright (c) 1984-2002, by David Betz
[1] (defun f (a b sep)
        (string-append a sep sep b))

F
[2] (f "Rosetta" "Code" ":")

"Rosetta::Code"
[3]
```



## zkl


```txt

$ zkl
zkl 1.12.8, released 2014-04-01
zkl: fcn f(a,b,c){String(a,c,c,b)}
Void
zkl: f("Rosetta", "Code", ":")
Rosetta::Code
zkl:

```


