+++
title = "Command-line arguments"
description = ""
date = 2019-10-08T17:39:51Z
aliases = []
[extra]
id = 1937
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "ada",
  "aikido",
  "aime",
  "algol_68",
  "applescript",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "babel",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "clean",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "delphi",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euphoria",
  "factor",
  "fancy",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "gambas",
  "genie",
  "global_script",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "hicest",
  "io",
  "ioke",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "klong",
  "kotlin",
  "lasso",
  "lfe",
  "liberty_basic",
  "lingo",
  "logo",
  "lse64",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "mercury",
  "min",
  "mmix",
  "neko",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "powerbasic",
  "powershell",
  "pure",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "raven",
  "realbasic",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "standard_ml",
  "swift",
  "tailspin",
  "tcl",
  "toka",
  "txr",
  "unix_shell",
  "ursa",
  "ursala",
  "v",
  "vbscript",
  "visual_basic",
  "visual_basic_.net",
  "zkl",
]
+++

## Task

{{task}}{{selection|Short Circuit|Console Program Basics}} [[Category:Basic language learning]][[Category:Programming environment operations]][[Category:Initialization]]Retrieve the list of command-line arguments given to the program. For programs that only print the arguments when run directly, see [[Scripted main]].

See also [[Program name]].

For parsing command line arguments intelligently, see [[Parsing command-line arguments]].

Example command line:

 myprogram -c "alpha beta" -h "gamma"


## 11l

<code>:argv</code> is a list containing all command line arguments, including the program name.

```11l
:start:
print(‘Program name: ’:argv[0])
print("Arguments:\n":argv[1..].join("\n"))
```



## Ada

In Ada95 and later versions, command line arguments are available through the predefined package Ada.Command_Line. In Ada83, this would be implementation dependent.


```ada
with Ada.Command_line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Commands is
begin
   -- The number of command line arguments is retrieved from the function Argument_Count
   -- The actual arguments are retrieved from the function Argument
   -- The program name is retrieved from the function Command_Name
   Put(Command_Name & " ");
   for Arg in 1..Argument_Count loop
      Put(Argument(Arg) & " ");
   end loop;
   New_Line;
end Print_Commands;
```



###  Alternative version using Matreshka


Uses [http://forge.ada-ru.org/matreshka Matreshka]


```ada
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

procedure Main is
begin
   for J in 1 .. League.Application.Arguments.Length loop
      Ada.Wide_Wide_Text_IO.Put_Line
       (League.Application.Arguments.Element (J).To_Wide_Wide_String);
   end loop;
end Main;
```



## Aikido

The arguments are passed to the program as a vector of strings called <em>args</em>

```aikido


foreach arg in args {
    println ("arg: " + arg)
}


```



## Aime


```aime
integer i;

i = 0;
while (i < argc()) {
    o_text(argv(i));
    o_byte('\n');
    i += 1;
}
```



## ALGOL 68

```algol68
main:(
  FOR i TO argc DO
    printf(($"the argument #"g(-0)" is "gl$, i, argv(i)))
  OD
)
```

Linux command:
 /usr/bin/a68g Command-line_arguments.a68 - 1 2 3 ...
Output:

```txt

the argument #1 is /usr/bin/a68g
the argument #2 is ./Command-line_arguments.a68
the argument #3 is -
the argument #4 is 1
the argument #5 is 2
the argument #6 is 3
the argument #7 is ...

```



## AppleScript



```applescript

#!/usr/bin/env osascript
-- Print first argument
on run argv
  return (item 1 of argv)
end run

```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program commandLine.s   */

/* Constantes    */
.equ STDOUT, 1                         @ Linux output console
.equ EXIT,   1                         @ Linux syscall
.equ WRITE,  4                         @ Linux syscall
/* Initialized data */
.data
szCarriageReturn:  .asciz "\n"

/* UnInitialized data */
.bss
.align 4

/*  code section */
.text
.global main
main:                                   @ entry of program
    push {fp,lr}                        @ saves registers
    add fp,sp,#8                        @  fp <- start address
    ldr r4,[fp]                         @ number of Command line arguments
    add r5,fp,#4                        @ first parameter address
    mov r2,#0                           @ init loop counter
loop:
    ldr r0,[r5,r2,lsl #2]               @ string address parameter
    bl affichageMess                    @ display string
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                    @ display carriage return
    add r2,#1                           @ increment counter
    cmp r2,r4                           @ number parameters ?
    blt loop                            @ loop

100:                                    @ standard end of the program
    mov r0, #0                          @ return code
    pop {fp,lr}                         @restaur  registers
    mov r7, #EXIT                       @ request to exit program
    swi 0                               @ perform the system call

iAdrszCarriageReturn:    .int szCarriageReturn


/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur 2 registres
    bx lr                                          @ return


```



## Arturo


```arturo
loop & {
	print &
}
```

Or:

```arturo
args &

loop args [arg]{
	print arg
}
```



## AutoHotkey

From the AutoHotkey [http://www.autohotkey.com/docs/Scripts.htm documentation]:
"The script sees incoming parameters as the variables %1%, %2%, and so on. In addition, %0% contains the number of parameters passed (0 if none). "

```autohotkey
Loop %0% ; number of parameters
  params .= %A_Index% . A_Space
If params !=
  MsgBox, %0% parameters were passed:`n`n %params%
Else
  Run, %A_AhkPath% "%A_ScriptFullPath%" -c "\"alpha beta\"" -h "\"gamma\""
```



## AWK



```awk
#!/usr/bin/awk -f

BEGIN {
  print "There are " ARGC "command line parameters"
  for(l=1; l<ARGC; l++) {
    print "Argument " l " is " ARGV[l]
  }
}
```




## Babel

Invoke Babel in interactive mode with arguments using the -i switch:


```babel
babel -i Larry Mo Curly
```


Print the argv list with newlines:


```babel
argv prn !
```


```txt
Larry
Mo
Curly

```


Print the argv list with spaces:


```babel
argv prs !
```


```txt
Larry Mo Curly
```


To access an individual argument, use the ith operator to select an element from the argv list; print with newline using say:


```babel
argv 1 ith say !
```


```txt
Mo

```



## BASIC

For most older BASICs that supply the keyword <code>COMMAND$</code>, all arguments are returned in a single string that must then be parsed inside the program. (Unlike modern BASICs, there is often no easy way to retrieve the program's name.)


```qbasic
PRINT "args: '"; COMMAND$; "'"
```


Sample output:
 args: 'This is a test.'

FreeBASIC supplies three ways to retrieve the arguments: <code>COMMAND$</code> (which works identically to QuickBASIC's <code>COMMAND$</code>), <code>COMMAND$()</code> (a string array which works like [[#C|C]]'s <code>argv[]</code>), and <code>__FB_ARGV__</code> (an array of pointers which works even more like C's <code>argv[]</code>) and __FB_ARGC__ (which works like C's <code>argc</code>).


```freebasic
DIM i AS INTEGER

PRINT COMMAND$

PRINT "This program is named "; COMMAND$(0)
i = 1
DO WHILE(LEN(COMMAND$(i)))
    PRINT "The argument "; i; " is "; COMMAND$(i)
    i = i + 1
LOOP

FOR i = 0 TO __FB_ARGC__ - 1
        PRINT "arg "; i; " = '"; *__FB_ARGV__[i]; "'"
NEXT i
```


Sample output:
 C:\>cla 1 2 3
 1 2 3
 This program is named cla
 The argument  1 is 1
 The argument  2 is 2
 The argument  3 is 3
 arg  0 = 'cla'
 arg  1 = '1'
 arg  2 = '2'
 arg  3 = '3'

=
## BaCon
=

```freebasic
' Command line arguments including program name
PRINT "Entire command line: ", ARGUMENT$

SPLIT ARGUMENT$ BY " " TO cli$ SIZE args
PRINT "Skip program name:";
FOR i = 1 TO args - 1
    PRINT " " & cli$[i];
NEXT
PRINT
```


```txt
prompt$ bacon command-line.bac
Converting 'command-line.bac'... done, 9 lines were processed in 0.002 seconds.
Compiling 'command-line.bac'... cc  -c command-line.bac.c
cc -o command-line command-line.bac.o -lbacon -lm
Done, program 'command-line' ready.

prompt$ ./command-line -c "alpha beta" -h "gamma"
Entire command line: ./command-line -c "alpha beta" -h gamma
Skip program name: -c "alpha beta" -h gamma
```



## Batch File

```dos
@echo off
setlocal enabledelayedexpansion

set Count=0
:loop
if not "%1"=="" (
   set /a count+=1
   set parameter[!count!]=%1
   shift
   goto loop
)

for /l %%a in (1,1,%count%) do (
   echo !parameter[%%a]!
)
```


Another way of doing it


```dos
::args2.cmd
@echo off
setlocal enabledelayedexpansion
set fn=%~f0
set p0=%~0
set p*=%*
set /a c=1
:loop
if @%1==@ goto done
	set p%c%=%~1
	set /a c=c+1
	shift
	goto loop
:done
set /a c=c-1
set p#=%c%
echo fn=%fn%
echo p0=%p0%
echo p*=%p*%
echo p#=%p#%
for /l %%i in (1,1,%p#%) do (
	echo p%%i=!p%%i!
)
```


Invocation:


```dos>
args2 foo "bar baz" quux
fn=d:\bin\args2.cmd
p0=args2
p*=foo "bar baz" quux
p#=3
p1=foo
p2=bar baz
p3=quux

```



## BBC BASIC

```bbcbasic
PRINT @cmd$
```



## Bracmat

When Bracmat is started with one or more arguments, each argument is evaluated as if it were a Bracmat expression <i>unless</i> an argument (for example the first one) consumes the next argument(s) by calling <code>arg$</code>. Each invocation of <code>arg$</code> pops one argument from the remaining list of arguments. Calling <code>arg$</code> when no more arguments are available results in failure. The following program iterates over all arguments following the currently evaluated argument and outputs the argument to standard output.
<lang>whl'(arg$:?a&out$(str$("next arg=" !a)))
```

Now run Bracmat with this program as the first argument in a DOS environment:

```txt
bracmat "whl'(arg$:?a&out$(str$(\"next arg=\" !a)))" "a" /b -c 2+3 'd;' "out$(\"13+7=\" 13+7)"
```

Instead of starting in interactive mode, Bracmat interprets the first argument, which consumes all following arguments. This is output to standard output:

```txt
next arg=a
next arg=/b
next arg=-c
next arg=2+3
next arg='d;'
next arg=out$("13+7=" 13+7)
```

If given an argument index, <code>arg$&lt;<i>arg index</i>&gt;</code> returns the indexed argument without consuming any argument.

```txt
bracmat "0:?n&whl'(arg$!n:?a&out$str$(arg[ !n \"]=\" !a)&1+!n:?n)" "a" /b -c 2+3 'd;' "out$(\"13+7=\" 13+7)"
```

Output:

```txt
arg[0]=bracmat
arg[1]=0:?n&whl'(arg$!n:?a&out$str$(arg[ !n "]=" !a)&1+!n:?n)
arg[2]=a
arg[3]=/b
arg[4]=-c
arg[5]=2+3
arg[6]='d;'
arg[7]=out$("13+7=" 13+7)
13+7= 20
```

The last line demonstrates that not only the first argument is evaluated, but also the following arguments.

If Bracmat is run without arguments, Bracmat starts in interactive mode. In that situation calling <code>arg$</code> fails. The same is true if Bracmat is compiled as a shared library (DLL or so).


## C


Command line arguments are passed to main. Since the program name is also passed as "argument", the provided count is actually one more than the number of program arguments. Traditionally the argument count is named argc and the array of argument strings is called argv, but that's not mandatory; any (non-reserved) name will work just as well. It is, however, a good idea to stick to the conventional names.

Be careful on systems that use Unicode or other multibyte character sets. You may need to use a type of _wchar* and multi-byte-character-set-aware versions of printf.


```cpp
#include <iostream>
#include <stdio.h>

int main(int argc, char* argv[])
{
  int i;
  (void) printf("This program is named %s.\n", argv[0]);
  for (i = 1; i < argc; ++i)
    (void) printf("the argument #%d is %s\n", i, argv[i]);
  return EXIT_SUCCESS;
}
```



## C++

Command line arguments are passed the same way as in C.

This example uses iostream. Traditional C I/O also works.


```cpp
#include <iostream>

int main(int argc, char* argv[])
{
  std::cout << "This program is named " << argv[0] << std::endl;
  std::cout << "There are " << argc-1 << " arguments given." << std::endl;
  for (int i = 1; i < argc; ++i)
    std::cout << "the argument #" << i << " is " << argv[i] << std::endl;

  return 0;
}
```


## C#
There are at least two methods to access the command-line arguments. The first method is to access the string array passed to Main. This method only accesses the arguments and not the path to the executable.

```c#
using System;

namespace RosettaCode {
    class Program {
        static void Main(string[] args) {
            for (int i = 0; i < args.Length; i++)
                Console.WriteLine(String.Format("Argument {0} is '{1}'", i, args[i]));
        }
    }
}
```


The second method is to call the Environment.GetCommandLineArgs function. This method also returns the path to the executable as args[0] followed by the actual command line arguments.

```c#
using System;

namespace RosettaCode {
    class Program {
        static void Main() {
            string[] args = Environment.GetCommandLineArgs();
            for (int i = 0; i < args.Length; i++)
                Console.WriteLine(String.Format("Argument {0} is '{1}'", i, args[i]));
        }
    }
}
```



## Clean

<tt>getCommandLine</tt> from the module <tt>ArgEnv</tt> returns an array of command-line arguments (the first element is the name of the program).


```clean
import ArgEnv

Start = getCommandLine
```



## Clojure


The value of ''*command-line-args*'' is a sequence of the supplied command line arguments, or ''nil'' if none were supplied.


```Clojure
(dorun (map println *command-line-args*))
```



## COBOL

The COBOL standard appears to say nothing regarding the retrieval of command-line arguments, although methods of retrieving them are provided by most vendors.

Getting the arguments in one go, exactly as they were passed in:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. accept-all-args.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  args                   PIC X(50).

       PROCEDURE DIVISION.
       main-line.
           ACCEPT args FROM COMMAND-LINE
           DISPLAY args

           GOBACK
           .
```


Getting the arguments one at a time, with arguments being split by whitespace if not in quotes:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. accept-args-one-at-a-time.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  arg                 PIC X(50) VALUE SPACES.

       PROCEDURE DIVISION.
           ACCEPT arg FROM ARGUMENT-VALUE
           PERFORM UNTIL arg = SPACES
               DISPLAY arg
               MOVE SPACES TO arg
               ACCEPT arg FROM ARGUMENT-VALUE
           END-PERFORM

           GOBACK
           .
```


Passing arguments from UNIX/Linux Systems to COBOL.
```cobol

       *>Created By Zwiegnet 8/19/2004

        IDENTIFICATION DIVISION.
        PROGRAM-ID. arguments.

        ENVIRONMENT DIVISION.

        DATA DIVISION.


        WORKING-STORAGE SECTION.

        01 command1 PIC X(50).
        01 command2 PIC X(50).
        01 command3 PIC X(50).


        PROCEDURE DIVISION.

        PERFORM GET-ARGS.

        *> Display Usage for Failed Checks
        ARGUSAGE.
        display "Usage: <command1> <command2> <command3>"
        STOP RUN.

        *> Evaluate Arguments
        GET-ARGS.
        ACCEPT command1 FROM ARGUMENT-VALUE
        IF command1 = SPACE OR LOW-VALUES THEN
        PERFORM ARGUSAGE
        ELSE
        INSPECT command1 REPLACING ALL SPACES BY LOW-VALUES


        ACCEPT command2 from ARGUMENT-VALUE
        IF command2 = SPACE OR LOW-VALUES THEN
        PERFORM ARGUSAGE
        ELSE
        INSPECT command2 REPLACING ALL SPACES BY LOW-VALUES


        ACCEPT command3 from ARGUMENT-VALUE
        IF command3 = SPACE OR LOW-VALUES THEN
        PERFORM ARGUSAGE
        ELSE
                INSPECT command3 REPLACING ALL SPACES BY LOW-VALUES



        *> Display Final Output
        display command1 " " command2 " " command3


        STOP RUN.

.
```



## CoffeeScript

```coffeescript

console.log arg for arg in process.argv

```



## Common Lisp


The Common Lisp standard does not specify anything relating to external invocation of a Common Lisp system. The method for getting command-line arguments varies by implementation.

The following function could be used to create a uniform way to access the arguments:


```lisp
(defun argv ()
  (or
   #+clisp (ext:argv)
   #+sbcl sb-ext:*posix-argv*
   #+abcl ext:*command-line-argument-list*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   nil))
```



## D


```d
void main(in string[] args) {
    import std.stdio;

    foreach (immutable i, arg; args[1 .. $])
        writefln("#%2d : %s", i + 1, arg);
}
```


## DCL

case is not preserved unless the parameter is in quotes

```DCL
$ i = 1
$ loop:
$  write sys$output "the value of P''i' is ", p'i
$  i = i + 1
$  if i .le. 8 then $ goto loop
```

```txt
$ @command_line_arguments -c "alpha beta" -h "gamma"
the value of P1 is -C
the value of P2 is alpha beta
the value of P3 is -H
the value of P4 is gamma
the value of P5 is
the value of P6 is
the value of P7 is
the value of P8 is
```


=={{header|Déjà Vu}}==
Command line arguments are found in <code>!args</code> and <code>!opts</code>.


```dejavu
for i range 0 -- len !args:
	print\( "Argument #" i " is " )
	. get-from !args i

if has !opts :c:
	!print "Ah, the -c option."

if has !opts :four:
	!. get-from !opts :four
```

```txt
$ vu args-3.deja one two -c three --four=five
Argument #0 is "args-3.deja"
Argument #1 is "one"
Argument #2 is "two"
Argument #3 is "three"
Ah, the -c option.
"five"
```


The order of command line ''options'' is lost.


## Delphi



```delphi
// The program name and the directory it was called from are in
// param[0] , so given the axample of myprogram -c "alpha beta" -h "gamma"

  for x := 0 to paramcount do
      writeln('param[',x,'] = ',param[x]);

// will yield ( assuming windows and the c drive as the only drive) :

//  param[0] = c:\myprogram
//  param[1] = -c
//  param[2] = alpha beta
//  param[3] = -h
//  param[4] = gamma

```



## E



```e
interp.getArgs()
```



## Eiffel


This class inherits functionality for dealing with command line arguments from class <code lang="eiffel">ARGUMENTS</code>. It uses the feature <code lang="eiffel">separate_character_option_value</code> to return the values by option name for each of the two arguments.


```eiffel
class
    APPLICATION
inherit
    ARGUMENTS
create
    make
feature {NONE} -- Initialization
    make
            -- Print values for arguments with options 'c' and 'h'.
        do
            print ("Command line argument value for option 'c' is: ")
            print (separate_character_option_value ('c') + "%N")
            print ("Command line argument value for option 'h' is: ")
            print (separate_character_option_value ('h') + "%N")
            io.read_line    -- Keep console window open
        end
end
```


Output (for command line arguments: -c "alpha beta" -h "gamma"):

```txt

Command line argument value for option 'c' is: alpha beta
Command line argument value for option 'h' is: gamma

```



## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;

public program()
{
    program_arguments.forEvery:(int i)
        { console.printLine("Argument ",i," is ",program_arguments[i]) }
}
```

```txt

Argument 0 is myprogram.exe
Argument 1 is -c
Argument 2 is alpha beta
Argument 3 is -h
Argument 4 is gamma

```



## Elixir

Elixir provides command line arguments via the <tt>System.argv()</tt> function.

```elixir
#!/usr/bin/env elixir
IO.puts 'Arguments:'
Enum.map(System.argv(),&IO.puts(&1))
```

Example run:

```bash
$ ./show-args.exs a b=2 --3 -4
Arguments:
a
b=2
--3
-4
```



## Emacs Lisp


```emacslisp

#!/usr/bin/env emacs --script
(dolist (arg command-line-args-left) (message arg))

```



## Erlang

When used as a script language the arguments is a list to the main/1 function. When compiled use init:get_arguments/0

```erlang
3>
 init:get_arguments().
```

result

```erlang
[{root,["/usr/erlang/erl5.5"]},
{progname,["erl"]},
{home,["/home/me"]},
{c,["alpha beta"]},
{h,["gamma"]}]
```


init:get_argument(name) can be used to fetch value of a particular flag


```erlang
4>
 init:get_argument(h).
{ok,[["gamma"]]}
5> init:get_argument(c).
{ok,[["alpha beta"]]}
```



## Euphoria


```Euphoria
constant cmd = command_line()
printf(1,"Interpreter/executable name: %s\n",{cmd[1]})
printf(1,"Program file name: %s\n",{cmd[2]})
if length(cmd)>2 then
  puts(1,"Command line arguments:\n")
  for i = 3 to length(cmd) do
    printf(1,"#%d : %s\n",{i,cmd[i]})
  end for
end if
```


=={{header|F_Sharp|F#}}==
The entry-point function accepts the comment line arguments as an array of strings. The following program will print each argument on a separate line.

```fsharp
#light
[<EntryPoint>]
let main args =
    Array.iter (fun x -> printfn "%s" x) args
    0
```


## Factor

 USING: io sequences command-line ;
 (command-line) [ print ] each


## Fancy


```fancy
ARGV each: |a| {
  a println # print each given command line argument
}
```



## Fantom



```fantom

class Main
{
  public static Void main (Str[] args)
  {
    echo ("command-line args are: " + args)
  }
}

```



## Forth

Access to command line arguments is not a standard feature of Forth, since it is designed to be used without an operating system. The popular GNU implementation gforth runs from a shell and can access command line arguments similar to C: variable '''argc''' contains the count (including the command itself) and '''arg''' is a function that returns the ''nth'' argument as a string.

```forth
\ args.f: print each command line argument on a separate line
: main
  argc @ 0 do i arg type cr loop ;

main bye
```


Here is output from a sample run.

```forth
$ gforth args.f alpha "beta gamma" delta
gforth
args.f
alpha
beta gamma
delta
$
```


## Fortran

```fortran
program command_line_arguments

  implicit none
  integer, parameter :: len_max = 256
  integer :: i , nargs
  character (len_max) :: arg

  nargs = command_argument_count()
  !nargs = iargc()
  do i = 0, nargs
    call get_command_argument (i, arg)
    !call getarg (i, arg)
    write (*, '(a)') trim (arg)
  end do

end program command_line_arguments

```

Note: This sample uses the Fortran 2003 intrinsic routines <code>command_argument_count</code> and <code>get_command_argument</code> instead of the nonstandard extensions <code>iargc</code> and <code>getarg</code>. Most Fortran compilers support both.

Sample usage:
<lang>> ./a.out -c "alpha beta" -h "gamma"
./a.out
-c
alpha beta
-h
gamma
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Program (myprogram.exe) invoke as follows:
' myprogram -c "alpha beta" -h "gamma"

Print "The program was invoked like this => "; Command(0) + " " + Command(-1)
Print
Print "Press any key to quit"
Sleep
```


```txt

The program was invoked like this => myprogram -c alpha beta -h gamma

```



## Frink

Arguments to a program are available in the <CODE>ARGS</CODE> array variable.

```frink

println[ARGS]

```



## FunL


```funl
println( args )
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=a1374aa441520314ad0c7decb1e91c97 Click this link to run this code]'''

```gambas
PUBLIC SUB main()
  DIM l AS Integer
  DIM numparms AS Integer
  DIM parm AS String
  numparms = Application.Args.Count
  FOR l = 0 TO numparms - 1
    parm = Application.Args[l]
    PRINT l; " : "; parm
  NEXT
END
```



## Genie


```genie
[indent=4]
/*
   Command line arguments, in Genie

   valac commandLine.gs
   ./commandLine sample arguments 'four in total here, including args 0'
*/

init

    // Output the number of arguments
    print "%d command line argument(s):", args.length

    // Enumerate all command line arguments
    for s in args
        print s

    // to reiterate, args[0] is the command
    if args[0] is not null
        print "\nWith Genie, args[0] is the command: %s", args[0]
```


```txt
prompt$ valac commandLine.gs
prompt$ ./commandLine -c "alpha beta" -h "gamma"
5 command line argument(s):
./commandLine
-c
alpha beta
-h
gamma

With Genie, args[0] is the command: ./commandLine
```



## Global Script


Command-line arguments are passed to the main program as a linked list of strings (which are also linked lists).

This uses the <code>gsio</code> I/O operations, which are designed to be simple to implement on top of Haskell and simple to use. It also uses impmapM, which is a specific specialization of mapM for the HSGS implementation.

```Global Script
λ 'as. impmapM (λ 'a. print qq{Argument: §(a)\n}) as
```



## Go


```go

package main
import (
	"fmt"
	"os"
)

func main() {
	for i, x := range os.Args[1:] {
		fmt.Printf("the argument #%d is %s\n", i, x)
	}
}

```



## Groovy

Command-line arguments are accessible via the '''args''' list variable. The following is saved as the file "Echo.groovy":

```groovy
println args>
```


The existence of command-line arguments presupposes the existence of a command line interpreter. The following test runs were entered in a cygwin bash shell in a Microsoft Windows XP system:

```txt
$ groovy Echo this is an argument list
[this, is, an, argument, list]
$ groovy Echo -x alkfrew4oij -cdkjei +22
[-x, alkfrew4oij, -cdkjei, +22]
$
```


For more sophisticated command-line option and option-argument parsing use the '''CliBuilder''' (command-line interface builder) library, which extends the functionality of the Java-based '''Apache Commons CLI''' library to Groovy.


## Harbour

Uses the Harbour-specific hb_PValue() function

```visualfoxpro
PROCEDURE Main()

   LOCAL i

   FOR i := 1 TO PCount()
      ? "argument", hb_ntos( i ), "=", hb_PValue( i )
   NEXT

   RETURN
```



## Haskell


Defined by the System module, getArgs :: IO [String] provides the command-line arguments in a list.

myprog.hs:

```haskell
import System
main = getArgs >>= print
```


```txt

myprog a -h b c
=> ["a","-h","b","c"]

```



## HicEst


```hicest
DO i = 2, 100 ! 1 is HicEst.exe
  EDIT(Text=$CMD_LINE, SePaRators='-"', ITeM=i, IF ' ', EXit, ENDIF, Parse=cmd, GetPosition=position)
  IF(position > 0) WRITE(Messagebox) cmd
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==
Command line parameters are passed to Icon/Unicon programs as a list of strings.

```Icon
procedure main(arglist)
every write(!arglist)
end
```


{{libheader|Icon Programming Library}} includes [http://www.cs.arizona.edu/icon/library/procs/options.htm options] that parses the command line as switches and arguments and returns the results in a table.


## Io


```io
System args foreach(a, a println)
```



## Ioke


```ioke
System programArguments each(println)
```



## J


The global <code>ARGV</code> holds the command line arguments.  Thus, a program to display them:


```j
   ARGV
```



## Java



```java
public class Arguments {
  public static void main(String[] args) {
     System.out.println("There are " + args.length + " arguments given.");
     for(int i = 0; i < args.length; i++)
        System.out.println("The argument #" + (i+1) + " is " + args[i] + " and is at index " + i);
  }
}
```


For more sophisticated command-line option and option-argument parsing use the [http://commons.apache.org/cli '''Apache Commons CLI'''] (command-line interface) library.


## JavaScript

```javascript
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

```javascript
var objArgs = WScript.Arguments;
for (var i = 0; i < objArgs.length; i++)
   WScript.Echo(objArgs.Item(i));
```

```javascript
import System;
var argv:String[] = Environment.GetCommandLineArgs();
for (var i in argv)
  print(argv[i]);
```

```javascript
for (var i = 0; i < arguments.length; i++)
    print(arguments[i]);
```



## jq

<br/>
jq distinguishes between command-line arguments and command-line options.
Only the former are available programmatically.

Specifically, both named and positional command-line arguments are available in the
global constant '''$ARGS''', a JSON object, as follows:

    $ARGS.positional contains an array of the positional arguments as JSON strings

    $ARGS.names is a JSON object of giving the mapping of name to value.

For example, the invocation:

    $ jq -n '$ARGS' --args a b

yields:
```json
{
  "positional": [
    "a",
    "b"
  ],
  "named": {}
}
```


Arguments specified with ''--args'' are always read as JSON strings; arguments specified with ''--jsonargs''
are interpreted as JSON, as illustrated here:

```txt

$ jq -n '$ARGS' --argjson x 0 --jsonargs 0 '{"a":1}'
{
  "positional": [
    0,
    {
      "a": 1
    }
  ],
  "named": {
    "x": 0
  }
}
```



## Jsish


```javascript
#!/usr/local/bin/jsish
puts(Info.argv0());
puts(console.args);
```


```txt
$ jsish command-line-arguments.jsi -c "alpha beta" -h "gamma"
/home/btiffin/lang/jsish/command-line-arguments.jsi
[ "-c", "alpha beta", "-h", "gamma" ]
```



## Julia

Works when the Julia program is run as a file argument to julia.exe.

```Julia
using Printf

prog = Base.basename(Base.source_path())

println(prog, "'s command-line arguments are:")
for s in ARGS
    println("    ", s)
end

```


```txt

$ julia command_line_arguments.jl -c "alpha beta" -h "gamma"
command_line_arguments.jl's command-line arguments are:
    -c
    alpha beta
    -h
    gamma

```



## Klong

Command line arguments (but not the program name itself) are bound
to the variable ".a". The following program prints them, one argument
per line:


```K

.p'.a

```



## Kotlin

```scala
fun main(args: Array<String>) {
     println("There are " + args.size + " arguments given.")
     args.forEachIndexed { i, a -> println("The argument #${i+1} is $a and is at index $i") }
}
```

See Java output.


## Lasso


```lasso
#!/usr/bin/lasso9

iterate($argv) => {
  stdoutnl("Argument " + loop_count + ": " + loop_value)
}
```

Output:

```shell
$ lasso9 arguments.lasso -c "alpha beta" -h "gamma"
Argument 1: arguments.lasso
Argument 2: -c
Argument 3: alpha beta
Argument 4: -h
Argument 5: gamma
```



## LFE


To demonstrate this, we can start the LFE REPL up with the parameters for this example:

```shell

$ ./bin/lfe -pa ebin/ -c "alpha beta" -h "gamma"

```


Once we're in the shell, we can get all the initializing arguments with this call:

```lisp

> (: init get_arguments)
(#(root ("/opt/erlang/r15b03"))
 #(progname ("erl"))
 #(home ("/Users/oubiwann"))
 #(user ("lfe_boot"))
 #(pa ("ebin/"))
 #(c ("alpha beta"))
 #(h ("gamma")))

```


We can also get specific arguments if we know their keys:

```lisp

> (: init get_argument 'c)
#(ok (("alpha beta")))
> (: init get_argument 'h)
#(ok (("gamma")))

```



## Liberty BASIC


```lb
print CommandLine$
```



## Lingo


```lingo
put the commandline
-- "-c alpha beta -h gamma"
```


In latest versions of Mac OS X, the above approach doesn't work anymore. But there is a free "Xtra" (binary plugin/shared library) called "CommandLine Xtra" that works both in Windows and Mac OS X and returns the command-line parsed into a lingo list (array):

```lingo
put getCommandLineArgs()
-- ["-c", "alpha beta", "-h", "gamma"]
```



## Logo

If the command line to a logo script is written
 logo file.logo - arg1 arg2 arg3
Then the arguments after the "-" are found in a list in variable :COMMAND.LINE

```logo
show :COMMAND.LINE
[arg1 arg2 arg3]
```

Alternatively, make the first line of an executable logo script:
 #! /usr/bin/logo -
to be able to invoke the script with arguments.
 file.logo arg1 arg2 arg3


## LSE64


```lse64
argc , nl  # number of arguments (including command itself)
0         # argument
dup arg dup 0 = || ,t  1 + repeat
drop
```


## Lua


The lua scripting language does not use argc and argv conventions for the command line parameters. Instead, the command line parameters to the main script are provided through the global table arg. The script name is placed into element zero of arg, and the script parameters go into the subsequent elements:


```lua
print( "Program name:", arg[0] )

print "Arguments:"
for i = 1, #arg do
    print( i," ", arg[i] )
end
```


## M2000 Interpreter

function quote$("a") return "a" a string in ""

Arguments in command line maybe two kinds, in first part those  three letters identifiers with + or - for switches for interpreter and the last part to executed before loading the actual script.

For this example we make a script, save to temporary directory, and call it passing arguments. We can use Win as shell substitute in M2000 environment, or the Use statement. Reading the shell statement Win we can see how the command line composed. We call the m2000.exe in the appdir$ (application directory, is the path to M2000.exe), and pass a string as a file with a path. That path will be the current path for the new start of m2000.exe the host for M2000 Interpreter (an activeX dll).


```M2000 Interpreter

Module Checkit {
      Document a$ = {
      Module Global A {
            Show
            Read a$="nothing", x=0
            Print a$, x
            A$=Key$
      }
      A: End
      }
      Dir temporary$
      Save.doc a$, "program.gsb"
      \\ open if gsb extension is register to m2000.exe
      Win quote$(dir$+"program.gsb")
      \\ +txt is a switch for interpreter to use string comparison as text (not binary)
      \\ so we can send switches and commands before the program loading
      Win appdir$+"m2000.exe", quote$(dir$+"program.gsb +txt : Data {Hello}, 100")
      \\ no coma after name (we can use "program.gsb" for  names with spaces)
      Use program.gsb  "From Use statement", 200
      \\ delete file
      Wait 5000
      Dos "del "+quote$(dir$+"program.gsb");
      \\ open directory
      Rem : Win temporary$
}
Checkit

```




## Mathematica

myprogram:

```Mathematica
#!/usr/local/bin/MathematicaScript -script
$CommandLine
```

Output:

```txt
{myprogram,-c,alpha beta,-h,gamma}
```



## Mercury

<lang>
:- module cmd_line_args.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.progname("", ProgName, !IO),
    io.format("This program is named %s.\n", [s(ProgName)], !IO),
    io.command_line_arguments(Args, !IO),
    list.foldl2(print_arg, Args, 1, _, !IO).

:- pred print_arg(string::in, int::in, int::out, io::di, io::uo) is det.

print_arg(Arg, ArgNum, ArgNum + 1, !IO) :-
    io.format("the argument #%d is %s\n", [i(ArgNum), s(Arg)], !IO).

```



## min

```min
args>
```



## MMIX


```mmix
argv   IS    $1
argc   IS    $0
i      IS    $2

       LOC   #100
Main   LOC   @
       SETL  i,1               % i = 1
Loop   CMP   $3,argc,2         % argc < 2 ?
       BN    $3,1F             % then jump to end
       XOR   $255,$255,$255    % clear $255
       8ADDU $255,i,argv       % i*8 + argv
       LDOU  $255,$255,0       % argv[i]
       TRAP  0,Fputs,StdOut    % write the argument
       GETA  $255,NewLine      % add a newline
       TRAP  0,Fputs,StdOut
       INCL  i,1               % increment index
       SUB   argc,argc,1       % argc--
       BP    argc,Loop         % argc > 0? then Loop
1H     LOC   @
       XOR   $255,$255,$255    % exit(0)
       TRAP  0,Halt,0

NewLine BYTE #a,0
```


=={{header|Modula-2}}==

```modula2
MODULE try;

FROM  Arguments  IMPORT  GetArgs, ArgTable, GetEnv;
FROM  InOut      IMPORT  WriteCard, WriteLn, WriteString;

VAR   count, item        : SHORTCARD;
      storage            : ArgTable;

BEGIN
   GetArgs (count, storage);
   WriteString ('Count =');     WriteCard (count, 4);   WriteLn;
   item := 0;
   REPEAT
      WriteCard (item, 4);
      WriteString (' :  ');
      WriteString (storage^ [item]^);
      WriteLn;
      INC (item)
   UNTIL item = count
END try.
```

Example:
<lang Modula-2>
jan@Beryllium:~/modula/test$ try jantje zag eens pruimen hangen
Count =   6
   0 :  try
   1 :  jantje
   2 :  zag
   3 :  eens
   4 :  pruimen
   5 :  hangen

```


=={{header|Modula-3}}==
Command line parameters are accessed using the <tt>Params</tt> module.

```modula3
MODULE Args EXPORTS Main;

IMPORT IO, Params;

BEGIN
  IO.Put(Params.Get(0) & "\n");
  IF Params.Count > 1 THEN
    FOR i := 1 TO Params.Count - 1 DO
      IO.Put(Params.Get(i) & "\n");
    END;
  END;
END Args.
```


Output:

```txt

martin@thinkpad:~$ ./prog
./prog
martin@thinkpad:~$ ./prog 10
./prog
10
martin@thinkpad:~$ ./prog 10 20
./prog
10
20

```



## Neko


```actionscript
/* command line arguments, neko */
var argc = $asize($loader.args)

/* Display count and arguments, indexed from 0, no script name included */
$print("There are ", argc, " arguments\n")

var arg = 0
while arg < argc $print($loader.args[arg ++= 1], "\n")
```


```txt
prompt$ nekoc command-line-arguments.neko
prompt$ neko ./command-line-arguments.n -c "alpha beta" -h "gamma"
There are 4 arguments
-c
alpha beta
-h
gamma
```



## Nemerle


```Nemerle
using System;
using System.Console;

module CLArgs
{
    Main(args : array[string]) : void
    {
        foreach (arg in args) Write($"$arg "); // using the array passed to Main(), everything after the program name
        Write("\n");

        def cl_args = Environment.GetCommandLineArgs(); // also gets program name
        foreach (cl_arg in cl_args) Write($"$cl_arg ");
    }
}
```



## NetRexx

In a stand-alone application NetRexx places the command string passed to it in a variable called <tt>arg</tt>.

```NetRexx
/* NetRexx */
-- sample arguments: -c "alpha beta" -h "gamma"
say "program arguments:" arg

```

'''Output:'''

```txt

program arguments: -c "alpha beta" -h "gamma"

```



## Nim


```nim
import os
let programName = getAppFilename()
let arguments = commandLineParams()
```


=={{header|Oberon-2}}==
```oberon2

MODULE CommandLineArguments;
IMPORT
  NPCT:Args,
  Out := NPCT:Console;

BEGIN
  Out.String("Args number: ");Out.Int(Args.Number(),0);Out.Ln;
  Out.String("0.- : ");Out.String(Args.AsString(0));Out.Ln;
  Out.String("1.- : ");Out.String(Args.AsString(1));Out.Ln;
  Out.String("2.- : ");Out.String(Args.AsString(2));Out.Ln;
  Out.String("3.- : ");Out.String(Args.AsString(3));Out.Ln;
  Out.String("4.-: ");Out.String(Args.AsString(4));Out.Ln
END CommandLineArguments.

```

```txt

Args number: 5
0.- : bin/CommandLineArguments
1.- : -c
2.- : alpha beta
3.- : -h
4.-: gamma

```



## Objeck


```objeck

bundle Default {
  class Line {
    function : Main(args : String[]) ~ Nil {
      each(i : args) {
        args[i]->PrintLine();
       };
    }
  }
}

```


=={{header|Objective-C}}==

In addition to the regular C mechanism of arguments to main(), Objective-C also has another way to get the arguments as string objects inside an array object:

```objc
NSArray *args = [[NSProcessInfo processInfo] arguments];
NSLog(@"This program is named %@.", [args objectAtIndex:0]);
NSLog(@"There are %d arguments.", [args count] - 1);
for (i = 1; i < [args count]; ++i){
    NSLog(@"the argument #%d is %@", i, [args objectAtIndex:i]);
}
```



## OCaml


The program name is also passed as "argument", so the array length is actually one more than the number of program arguments.


```ocaml
let () =
  Printf.printf "This program is named %s.\n" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.printf "the argument #%d is %s\n" i Sys.argv.(i)
  done
```


=== Using the [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html Arg] module ===


```ocaml
(* default values *)
let somebool = ref false
let somestr = ref ""
let someint = ref 0

let usage = "usage: " ^ Sys.argv.(0) ^ " [-b] [-s string] [-d int]"

let speclist = [
    ("-b", Arg.Set somebool, ": set somebool to true");
    ("-s", Arg.Set_string somestr, ": what follows -s sets some string");
    ("-d", Arg.Set_int someint, ": some int parameter");
  ]

let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;

  Printf.printf " %b %d '%s'\n" !somebool !someint !somestr;
;;
```



 % ocaml arg.ml --help
 usage: tmp.ml [-b] [-s string] [-d int]
   -b : set somebool to true
   -s : what follows -s sets some string
   -d : some int parameter
   --help  Display this list of options

 % ocaml arg.ml -d 4 -b -s blabla
  true 4 'blabla'

 % ocaml arg.ml
  false 0 <nowiki>''</nowiki>



## Oforth


System.Args returns command line arguments.

All arguments that begin with "--" are not included into this list.
The first argument is the program name, so this list is never empty.


```oforth
System.Args println
```



## Oz


### Raw arguments

Like in C, but without the program name:

```oz
functor
import Application System
define
   ArgList = {Application.getArgs plain}
   {ForAll ArgList System.showInfo}
   {Application.exit 0}
end
```



### Preprocessed arguments


```oz
functor
import Application System
define
   ArgSpec =
   record(
      c(type:string single      %% option "--c" expects a string, may only occur once,
	optional:false char:&c) %% is not optional and has a shortcut "-c"

      h(type:string single      %% option "--h" expects a string, may only occur once,
	default:"default h"     %% is optional and has a default value if not given
	char:&h)                %% and has a shortcut "-h"
      )
   Args = {Application.getArgs ArgSpec}
   {System.showInfo Args.c}
   {System.showInfo Args.h}
   {Application.exit 0}
end
```



## Pascal

Depends on implementation.


## Perl

@ARGV is the array containing all command line parameters


```perl
my @params = @ARGV;
my $params_size = @ARGV;
my $second = $ARGV[1];
my $fifth = $ARGV[4];
```


If you don't mind importing a module:


```perl
use Getopt::Long;
GetOptions (
    'help|h'     => \my $help,
    'verbose|v'  => \my $verbose,
);
```



## Perl 6

Perl 5's <code>@ARGV</code> is available as <code>@*ARGS</code>. Alternatively, if you define a subroutine named <code>MAIN</code>, Perl will automatically process <code>@*ARGS</code> according to Unix conventions and <code>MAIN</code>'s signature (or signatures, if your <code>MAIN</code> is a multi sub) and then call <code>MAIN</code> with appropriate arguments; see [http://perlcabal.org/syn/S06.html#Declaring_a_MAIN_subroutine Synopsis 6] or [http://perlgeek.de/en/article/5-to-6#post_14|5-to-6].


```perl6
# with arguments supplied
$ perl6 -e 'sub MAIN($x, $y) { say $x + $y }' 3 5
8

# missing argument:
$ perl6 -e 'sub MAIN($x, $y) { say $x + $y }' 3
Usage:
-e '...' x y
```


If the program is stored in a file, the file name is printed instead of <code>-e '...'</code>.


## Phix


```Phix
constant cmd = command_line()
?cmd
if cmd[1]=cmd[2] then
    printf(1,"Compiled executable name: %s\n",{cmd[1]})
else
    printf(1,"Interpreted (using %s) source name: %s\n",cmd[1..2])
end if
if length(cmd)>2 then
  puts(1,"Command line arguments:\n")
  for i = 3 to length(cmd) do
    printf(1,"#%d : %s\n",{i,cmd[i]})
  end for
end if
```

When interpreting, the first two elements returned by command_line() are {interpreter,source}.

When compiled, the first two elements are instead {executable,executable}, so the parameters (if any) are consistently the 3rd element onwards.
```txt

C:\Program Files (x86)\Phix>p testcl -c "alpha beta" -h "gamma"
{"C:\\Program Files (x86)\\Phix\\p.exe","C:\\Program Files (x86)\\Phix\\testcl.exw","-c","alpha beta","-h","gamma"}
Interpreted (using C:\Program Files (x86)\Phix\p.exe) source name: C:\Program Files (x86)\Phix\testcl.exw
Command line arguments:
#3 : -c
#4 : alpha beta
#5 : -h
#6 : gamma
C:\Program Files (x86)\Phix>p -c testcl -c "alpha beta" -h "gamma"
{"C:\\Program Files (x86)\\Phix\\testcl.exe","C:\\Program Files (x86)\\Phix\\testcl.exe","-c","alpha beta","-h","gamma"}
Compiled executable name: C:\Program Files (x86)\Phix\testcl.exe
Command line arguments:
#3 : -c
#4 : alpha beta
#5 : -h
#6 : gamma
C:\Program Files (x86)\Phix>testcl -c "alpha beta" -h "gamma"
{"C:\\Program Files (x86)\\Phix\\testcl.exe","C:\\Program Files (x86)\\Phix\\testcl.exe","-c","alpha beta","-h","gamma"}
Compiled executable name: C:\Program Files (x86)\Phix\testcl.exe
Command line arguments:
#3 : -c
#4 : alpha beta
#5 : -h
#6 : gamma
C:\Program Files (x86)\Phix>

```



## PHP

When PHP is run from the command line, the special variables <tt>$argv</tt> and <tt>$argc</tt> contain the array of arguments, and the number of arguments, respectively. The program name is passed as the first argument.


```php
<?php
$program_name = $argv[0];
$second_arg = $argv[2];
$all_args_without_program_name = array_shift($argv);

```



## PicoLisp

There are three ways to handle command-line arguments in PicoLisp:

1. Obtain all arguments as a list of strings via '[http://software-lab.de/doc/refA.html#argv argv]'

2. Fetch each argument individually with '[http://software-lab.de/doc/refO.html#opt opt]'

3. Use the built-in [http://software-lab.de/doc/ref.html#invoc command-line interpretation], where arguments starting with a hyphen are executed as functions.

Here we use the third option, as it is not so obvious, sometimes more flexible,
and in fact the most commonly used one for application development.

We define 'c' and 'h' as functions, which retrieve their argument with 'opt',
and then '[http://software-lab.de/doc/refL.html#load load]' all remaining
command line arguments.

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(de c ()
   (prinl "Got 'c': " (opt)) )

(de h ()
   (prinl "Got 'h': " (opt)) )

(load T)
(bye)
```

Output:

```txt
$ ./myprogram -c "alpha beta" -h "gamma"
Got 'c': alpha beta
Got 'h': gamma
```



## PL/I


```pli

/* The entire command line except the command word itself is passed */
/* to the parameter variable in PL/I.                               */
program: procedure (command_line) options (main);
   declare command_line character (100) varying;

...

end program;

```



## Pop11


variable poparglist contains list of command line arguments (as strings). One can use iteration over list to process then (for example print).


```pop11
lvars arg;
for arg in poparglist do
   printf(arg, '->%s<-\n');
endfor;
```



## PowerBASIC

For versions of PowerBASIC prior to [[PB/Win]] 9 and [[PB/CC]] 5, the only option available is identical to the one used by [[#BASIC|QuickBASIC]] above:

```powerbasic
? "args: '"; COMMAND$; "'"
```


Current versions of PowerBASIC (with the likely exception of [[PB/DOS]]) include <code>COMMAND$()</code> that works similarly to [[FreeBASIC]]'s <code>COMMAND$()</code>, except that you can't retrieve the application's name:

```powerbasic
'these two both return ALL args
? COMMAND$
? COMMAND$(0)

DO WHILE(LEN(COMMAND$(i)))
    PRINT "The argument "; i; " is "; COMMAND$(i)
    i = i + 1
LOOP
```



## PowerShell

In PowerShell the arguments to a script can be accessed with the <code>$args</code> array:

```powershell
$i = 0
foreach ($s in $args) {
    Write-Host Argument (++$i) is $s
}
```



## Pure

Arguments are in global variables, argc and argv.


```pure

using system;

printf "There are %d command line argumants\n" argc;
puts "They are " $$ map (puts) argv;

```



## PureBasic



### Reading all parameters

You can easily read all parameters by using ProgramParameter() without argument.

```PureBasic
If OpenConsole()
  Define n=CountProgramParameters()
  PrintN("Reading all parameters")
  While n
    PrintN(ProgramParameter())
    n-1
  Wend
  Print(#CRLF$+"Press Enter")
  Input()
  CloseConsole()
EndIf
```


### Reading specific parameters

You can specify which parameter 'n' to read.

```PureBasic
If OpenConsole()
  Define n
  PrintN("Reading specific pameters")
  For n=0 To CountProgramParameters()
    PrintN(ProgramParameter(n))
  Next
  Print(#CRLF$+"Press Enter")
  Input()
  CloseConsole()
EndIf
```



## Python

''sys.argv'' is a list containing all command line arguments, including the program name. Typically you slice the list to access the actual command line argument:


```python
import sys
program_name = sys.argv[0]
arguments = sys.argv[1:]
count = len(arguments)
```


When running a module by invoking Python, the Python interpreter processes and removes some of the arguments, and the module cannot access them. To process command line arguments, run the module directly. sys.argv is a copy of the command line arguments; modifying sys.argv will not change the arguments seen by other processes, e.g. ps.  (In other words sys.argv is an object which contains a copy of the process' command line arguments ... modifying that copy is only visible from within the Python program and not externally).

For powerful option parsing capabilities check out the [http://docs.python.org/library/optparse.html optparse] module.


## R


Following adapted from [http://quantitative-ecology.blogspot.com/2007/08/including-arguments-in-r-cmd-batch-mode.html this post by Forester]:

Suppose you want to call your script <tt>test.r</tt> with the arguments <tt>a=1 b=c(2,5,6)</tt>, where <tt>b</tt> is a numeric vector. Suppose you also want to redirect your output to <tt>test.out</tt> (not that you have a choice--I still don't know how to make R send shell-script output to stdout). You would then run


```R
R CMD BATCH --vanilla --slave '--args a=1 b=c(2,5,6)' test.r test.out
```


from the commandline, with the following text in <tt>test.r</tt>:


```R
# Read the commandline arguments
args <- (commandArgs(TRUE))

# args is now a list of character vectors
# First check to see if any arguments were passed,
# then evaluate each argument.
if (length(args)==0) {
    print("No arguments supplied.")
    # Supply default values
    a <- 1
    b <- c(1,1,1)
} else {
    for (i in 1:length(args)) {
         eval(parse(text=args[[i]]))
    }
}
print(a*2)
print(b*3)
```


(possibly preceding code that actually does something :-) Your output <tt>test.out</tt> would then contain (e.g., if you <tt>cat</tt> it)


```txt
[1] 2
[1]  6 15 18
> proc.time()
   user  system elapsed
  0.168   0.026   0.178

```


If you know how to get the output

* sent to stdout (i.e., as is normal with shell scripts)
* done without the profiling

please update this example!


## Racket


The following is the simplest program that prints the command-line arguments:


```scheme
#lang racket
(current-command-line-arguments)
```


You can also explicitly print each argument to standard output:


```scheme
#lang racket

(for ([arg (current-command-line-arguments)]) (displayln arg))
```



## RapidQ


```rapidq
PRINT "This program is named "; Command$(0)
FOR i=1 TO CommandCount
    PRINT "The argument "; i; " is "; Command$(i)
NEXT i
```



## Raven



```raven
ARGS print

stack (6 items)
 0 => "raven"
 1 => "myprogram"
 2 => "-c"
 3 => "alpha beta"
 4 => "-h"
 5 => "gamma"
```



## REALbasic


```vb
Function Run(args() as String) As Integer
  For each arg As String In args
    Stdout.WriteLine(arg)
  Next
End Function
```

Output (given arguments: ''--foo !bar "bat bang"''):
 appName.exe
 --foo
 !bar
 bat bang


## REXX

The entire command line arguments (as a single string) are passed by REXX to the program.

```rexx
say 'command arguments:'
say arg(1)
```

Input:

 myprogram -c "alpha beta" -b "gamma"

However, in the above example (as shown), it's suggested that (maybe)
only options that start with a minus (-) are to be examined
and assumed to be options.

```rexx
parse arg aaa                          /*get the arguments.        */
                                       /*another version:          */
                                       /*  aaa=arg(1)              */
say 'command arguments:'
say aaa

opts=''                                /*placeholder for options.  */
data=''                                /*placeholder for data.     */

  do j=1 to words(aaa)
  x=word(aaa,j)
  if left(x,1)=='-' then opts=opts x   /*Option?  Then add to opts.*/
                    else data=data x   /*Must be data. Add to data.*/
  end

        /*the above process adds a leading blank to  OPTS and  DATA*/

opts=strip(opts,'L')                   /*strip leading blanks.     */
data=strip(data,'l')                   /*strip leading blanks.     */
say
say 'options='opts
say '   data='data
```


;Note to users of Microsoft Windows and Regina REXX:
Note that some REXX pass the command line as is, but Regina REXX lets
the operating system parse it first (for instance Windows), and
Windows will pass anything   (along to the program being invoked)   inside double quotes (<big>"</big>) to the program as is.   Any other data not in double quotes is passed as is.

Output from Regina REXX under Windows with the invocation:

 myprogram -c "alpha beta" -h "gamma"


```txt

command arguments:
-c alpha beta -h gamma

options=-c -h
   data=alpha beta gamma

```

Output from others REXXes under Windows with the invocation:

 myprogram -c "alpha beta" -h "gamma"


```txt

command arguments:
-c "alpha beta" -h "gamma"

options=-c -h
   data="alpha beta" "gamma"

```



;Notes to UNIX users:
The rexx programming language does not preserve command line parameters containing spaces. This renders it unsuitable for use for wrapperscript applications, where filenames containing spaces need to be preserved, because there is no way to differentiate between a space within a parameter, and a space used to separate parameters.

;Scenario:
If a script is called as follows:

 ccwrapper "foo bar.c" "bar bar.c"

From the shell:

 argv[0] = ccwrapper
 argv[1] = foo bar.c
 argv[2] = bar bar.c

It is a requirement of a wrapper that !argv[1] and !argv[2] are preserved when passed to the target application (a C compiler, in this example). Current implementations of rexx treat the command line arguments as one long argument:

 arg() = 1
 arg(1) = "foo bar.c bar bar.c"

The [parser] would separates the command line arguments by spaces. this results in !argv[1] and !argv[2] becoming split, so the target application would be called with different arguments:

 argv[1] = foo
 argv[2] = bar.c
 argv[3] = bar
 argv[4] = bar.c

This has a different meaning to the compiler, so the arguments forwarded from rexx are rendered useless.

;Workaround:
A workaround would be to create a wrapper around the rexx interpreter that encodes the command-line before calling rexx. The rexx application then decodes it. Some rexx interpreters, such as [[regina]] also provide a !-a switch as a workaround.


## Ring


```ring

see copy("=",30) + nl
see "Command Line Parameters" + nl
see "Size : " + len(sysargv) + nl
see sysargv
see copy("=",30) + nl
for x = 1 to len(sysargv)
    see x + nl
next

```



## Ruby

Command line arguments are available in the constant Object::ARGV.

myprog:

```ruby
#! /usr/bin/env ruby
p ARGV
```


  myprog a -h b c
  => ["a","-h","b","c"]


## Rust


```rust
use std::env;

fn main(){
    let args: Vec<_> = env::args().collect();
    println!("{:?}", args);
}
```

Run:
<lang>./program -c "alpha beta" -h "gamma"
["./program", "-c", "alpha beta", "-h", "gamma"]
```


=={{header|S-lang}}==
The command-line arguments exist in the array __argv:
<lang S-lang>variable a;
foreach a (__argv)
  print(a);

```
Note 1: This array can be changed by calling

    __set_argc_argv(new_argv);

Note 2: When a script is run as a parameter to slsh, __argv  does not
include slsh.  __argv[0] is simply the name of the script.

The same is true if running a script via the editor jed, as in:

    jed -script FILE [arg ...]

However, when [x/w]jed is entered normally, __argv consists of the
command-line for the editor itself, with __argv[0] == jed or /path/to/jed
or the equivalent.


## Sather


```sather
class MAIN is
  main(args:ARRAY{STR}) is
    loop
      #OUT + args.elt! + "\n";
    end;
  end;
end;
```


As in C (and others), the first element is the command itself (exactly as it is written in the command line and after shell variable expansion); e.g.


```txt
$ /home/home/rosetta/sather/a.out arg1 arg2 arg3
```


prints


```txt
/home/home/rosetta/sather/a.out
arg1
arg2
arg3
```



## Scala

Calling Scala from command line means invoking a method called <code>main</code>, defined on an
<code>object</code>, whose type is <code>(Array[String]):Unit</code>, meaning it receives an
array of strings, and returns unit. That array contains the command line arguments.


```scala
object CommandLineArguments extends App {
    println(s"Received the following arguments: + ${args.mkString("", ", ", ".")}")
}
```


When running a Scala script, where the whole body is executed, the arguments get stored in an array of strings called <code>argv</code>:


```scala
println(s"Received the following arguments: + ${argv.mkString("", ", ", ".")}")
```



## Scheme



```scheme
 (define (main args)
  (for-each (lambda (arg) (display arg) (newline)) args))
```



## Seed7



```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 0;
  begin
    writeln("This program is named " <& name(PROGRAM) <& ".");
    for i range 1 to length(argv(PROGRAM)) do
      writeln("The argument #" <& i <& " is " <& argv(PROGRAM)[i]);
    end for;
  end func;
```



## Sidef

Command line arguments are available in the ARGV array.

```ruby
say ARGV;
```

```txt
% myprog -c "alpha beta" -h "gamma"
['-c', 'alpha beta', '-h', 'gamma']

```



## Slate


```slate
StartupArguments do: [| :arg | inform: arg]
```



## Smalltalk

```smalltalk
(1 to: Smalltalk getArgc) do: [ :i |
  (Smalltalk getArgv: i) displayNl
]
```


```smalltalk
Smalltalk commandLineArguments printCR
```



## Standard ML



```sml
print ("This program is named " ^ CommandLine.name () ^ ".\n");
val args = CommandLine.arguments ();
Array.appi
  (fn (i, x) => print ("the argument #" ^ Int.toString (i+1) ^ " is " ^ x ^ "\n"))
  (Array.fromList args);
```



## Swift



```swift
let args = Process.arguments
println("This program is named \(args[0]).")
println("There are \(args.count-1) arguments.")
for i in 1..<args.count {
  println("the argument #\(i) is \(args[i])")
}
```


Alternately:

```swift
println("This program is named \(String.fromCString(Process.unsafeArgv[0])!).")
println("There are \(Process.argc-1) arguments.")
for i in 1 ..< Int(Process.argc) {
  println("the argument #\(i) is \(String.fromCString(Process.unsafeArgv[i])!)")
}
```

```swift
println("This program is named \(String.fromCString(C_ARGV[0])!).")
println("There are \(C_ARGC-1) arguments.")
for i in 1 ..< Int(C_ARGC) {
  println("the argument #\(i) is \(String.fromCString(C_ARGV[i])!)")
}
```



## Tailspin


```tailspin

$ARGS -> !OUT::write

```

```txt

[-c, alpha beta, -h, gamma]

```



## Tcl


The predefined global variable <tt>argc</tt> contains the number of arguments passed to the program after the script being executed, <tt>argv</tt> contains those arguments as a list. (The name of the script is in the <tt>argv0</tt> global variable, and the name of the executable interpreter itself is returned by the command <code>info nameofexecutable</code>.) Retrieving the second argument might look something like this:


```tcl
if { $argc > 1 } {
    puts [lindex $argv 1]
}
```


(Tcl counts from zero, thus <tt>[lindex $list 1]</tt> retrieves the second item in the list)


## Toka


Arguments are stored into an array. The first element in the array
is the name of the program, the rest are the arguments in order. The
number of arguments is provided by #args.


```toka
[ arglist array.get type cr ] is show-arg
[ dup . char: = emit space ] is #=
1 #args [ i #= show-arg ] countedLoop
```



## TXR


Command line arguments in TXR's pattern-based extraction language can be treated as the lines of a text stream, which is arranged using the directive <code>@(next :args)</code>. Thus TXR's text parsing capabilities work over the argument list.

This <code>@(next :args)</code> should be written as the first line of the TXR program, because TXR otherwise interprets the first argument as the name of an input file to open.


```txr
@(next :args)
@(collect)
@arg
@(end)
@(output)
My args are: {@(rep)@arg, @(last)@arg@(end)}
@(end)
```



```txt
$ ./txr args.txr
My args are: {}
$ ./txr args.txr 1
My args are: {1}
$ ./txr args.txr 1 2 3
My args are: {1, 2, 3}
```


Arguments are also available via two predefined variables: <code>*full-args*</code> and <code>*args*</code>, which are lists of strings, such that <code>*args*</code> is a suffic of <code>*full-args*</code>. <code>*full-args*</code> includes the arguments that were processed by TXR itself; <code>*args*</code> omits them.

Here is an example program which requires exactly three arguments. Note how <code>ldiff</code> is used to compute the arguments that are processed by TXR (the interpreter name, any special arguments and script name), to print an accurate usage message.


```txrlisp
(tree-case *args*
  ((a b c) (put-line "got three args, thanks!"))
  (else (put-line `usage: @(ldiff *full-args* *args*) <arg1> <arg2> <arg3>`)))
```

```txt
$ txr command-line-args.txr 1 2
usage: txr command-line-args.txr <arg1> <arg2> <arg3>
$ txr command-line-args.txr 1 2 3 4
usage: txr command-line-args.txr <arg1> <arg2> <arg3>
$ txr command-line-args.txr 1 2 3
got three args, thanks!
```



## UNIX Shell

===[[Bourne Shell]]===
To retrieve the entire list of arguments:

```bash
WHOLELIST="$@"
```

To retrieve the second and fifth arguments:

```bash
SECOND=$2
FIFTH=$5
```



## Ursa

In Ursa, all command line arguments (including the program name as invoked) are contained in the string stream args.

```ursa
#
# command-line arguments
#

# output all arguments
for (decl int i) (< i (size args)) (inc i)
        out args<i> endl console
end for
```


Sample shell session in the Bourne shell:

```txt
$ ursa cmdlineargs.u "alpha beta" -h "gamma"
cmdlineargs.u
alpha beta
-h
gamma
$
```



## Ursala

Command line arguments are accessible to an application
through a data structure initialized by the run-time system.
This example application does nothing but display the data
structure on standard output.

```Ursala
#import std

#executable ('parameterized','')

clarg = <.file$[contents: --<''>+ _option%LP]>+ ~command.options
```

Here is a bash terminal session.

```txt
$ clarg -c alpha,beta -h gamma --foo=bar,baz
<
   option[
      keyword: 'c',
      parameters: <'alpha','beta'>],
   option[
      position: 1,
      keyword: 'h',
      parameters: <'gamma'>],
   option[
      position: 2,
      longform: true,
      keyword: 'foo',
      parameters: <'bar','baz'>]>
```



## V

The arguments to the program are stored in the stack,

args.v

```v
$stack puts

./args.v a b c
=[args.v a b c]
```



## Visual Basic


Like [[#BASIC|Qbasic]], Visual Basic returns all of the args in the built-in variable <code>Command$</code>:

```vb
Sub Main
    MsgBox Command$
End Sub
```



## Visual Basic .NET


This syntax will tokenize the command line arguments. Tokens are normally delimited by spaces, but spaces can be part of a token if surrounded by quotes.


```vbnet
Sub Main(ByVal args As String())
    For Each token In args
        Console.WriteLine(token)
    Next
End Sub
```




## vbScript



```vbscript

'Command line arguments can be accessed all together by

For Each arg In Wscript.Arguments
    Wscript.Echo "arg=", arg
Next

'You can access only the named arguments such as /arg:value

For Each arg In Wscript.Arguments.Named
    Wscript.Echo "name=", arg, "value=", Wscript.Arguments.Named(arg)
Next

'Or just the unnamed arguments

For Each arg In Wscript.Arguments.Unnamed
    Wscript.Echo "arg=", arg
Next

```



## zkl

File myprogram.zkl:

```zkl
System.argv.println();
vm.arglist.println();
```

zkl myprogram -c "alpha beta" -h "gamma"
```txt

L("/home/craigd/Projects/ZKL/Bin/zkl","myprogram","-c","alpha beta","-h","gamma")
L("-c","alpha beta","-h","gamma")

```


