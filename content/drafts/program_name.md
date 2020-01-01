+++
title = "Program name"
description = ""
date = 2019-10-04T04:45:37Z
aliases = []
[extra]
id = 10242
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}} [[Category:Initialization]]
The task is to programmatically obtain the name used to invoke the program. (For example determine whether the user ran "python hello.py", or "python hellocaller.py", a program importing the code from "hello.py".)

Sometimes a [[multiline shebang]] is necessary in order to provide the script name to a language's internal ARGV.

See also [[Command-line arguments]]

Examples from [https://github.com/mcandre/scriptname GitHub].


## AArch64 Assembly


{{works with|aarch64-linux-gnu-as/qemu-aarch64}}

Without built-in CRT, <code>argc</code> and <code>argv</code> are stored in the stack. The format looks like:

<lang>sp+0  = argc
sp+8  = argv[0]
sp+16 = argv[1] ...
```


Each item of <code>argv</code> is a pointer to a null-terminated 8-bit string.

<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	mov x29, sp
	ldr x0, [sp, 24] // argv[0]
	bl _strlen // strlen(argv[0])
	mov x2, x0
	mov x0, #STDOUT
	ldr x1, [sp, 24]
	bl _write // write(stdout, argv[0], strlen(argv[0]))
	ldp x29, x30, [sp], 16
	mov x0, #0
	b _exit // exit(0);

// ssize_t _strlen(const char *str)
_strlen:
	mov x1, x0
	mov x0, #-1
1:	add x0, x0, #1
	ldrb w2, [x1, x0]
	cbnz x2, 1b
	ret

.text
//////////////// system call wrappers
// ssize_t _write(int fd, void *buf, size_t count)
_write:
	stp x29, x30, [sp, -16]!
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0
	ldp x29, x30, [sp], 16
	ret

// void _exit(int retval)
_exit:
	mov x8, #SVC_EXIT
	svc #0
```



## Ada

Being a compiled language, Ada has difficulties accessing source code filenames. But it is easy to access the executable's filename, using the function Command_Name defined in Ada.Command_Line:

```Ada
with Ada.Command_Line, Ada.Text_IO;

procedure Command_Name is
begin
   Ada.Text_IO.Put_Line(Ada.Command_Line.Command_Name);
end Command_Name;
```



## Aime

The program command line arguments are accessible via the argc()/argv() functions.  The program name is the first in the list of arguments.

```aime
o_text(argv(0));
o_byte('\n');
```


## ALGOL 68

<lang>
BEGIN
   print ((program idf, newline))
END

```

{{out}}

```txt

$ a68g Program_name.a68
Program_name.a68
$

```


## Applesoft BASIC


```ApplesoftBASIC

10 GOSUB 40"GET PROGRAM NAME
20 PRINT N$
30 END

40 REMGET PROGRAM NAME
50 GOSUB 100"GET INPUT BUFFER
60 GOSUB 200"REMOVE RUN PREFIX
70 GOSUB 300"REMOVE , SUFFIXES
80 GOSUB 400"TRIM SPACES
90 RETURN

100 REMGET INPUT BUFFER
110 N$ = ""
120 FOR I = 512 TO 767
130     B =  PEEK (I) - 128
140     IF B < 32 THEN  RETURN
150     N$ = N$ +  CHR$ (B)
160 NEXT I
170 RETURN

200 REMREMOVE RUN PREFIX
210 P = 1
220 FOR I = 1 TO 3
230     FOR J = P TO LEN(N$)
240         C$ =  MID$ (N$,J,1)
250         P = P + 1
260         IF C$ = " " THEN  NEXT J
270         IF C$ = MID$("RUN",I,1) THEN NEXT I:N$ =  MID$(N$,P,LEN(N$)-P+1):RETURN
280 PRINT "YOU NEED TO RUN THIS PROGRAM USING THE  RUN COMMAND FROM DOS."
290 END

300 REMREMOVE , SUFFIXES
310 L =  LEN (N$)
320 FOR I = 1 TO L
330     C$ =  MID$ (N$,I,1)
340     IF C$ = "," THEN N$ =  LEFT$(N$,I - 1): RETURN
350 NEXT I
360 RETURN

400 REMTRIM SPACES
410 GOSUB 600

500 REMLEFT TRIM SPACES
510 L = LEN(N$) - 1
520 FOR I = L TO 0 STEP -1
530     IF I < 0 THEN RETURN
540     IF LEFT$ (N$,1) <> " " THEN RETURN
550     IF I THEN N$ = RIGHT$ (N$, I)
560 NEXT I
570 N$ = "
580 RETURN

600 REMRIGHT TRIM SPACES
610 L = LEN(N$) - 1
620 FOR I = L TO 0 STEP -1
630     IF I < 0 THEN RETURN
640     IF RIGHT$ (N$,1) <> " " THEN RETURN
650     IF I THEN N$ = LEFT$ (N$, I)
660 NEXT I
670 N$ = "
680 RETURN

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program namepgm.s   */
/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1
/* Initialized data */
.data
szMessage: .asciz "Program : "       @
szRetourLigne: .asciz "\n"


.text
.global main
main:
    push {fp,lr}    /* save des  2 registres */
    add fp,sp,#8    /* fp <- adresse début */
    ldr r0, iAdrszMessage         @ adresse of message
    bl affichageMess            @ call function
    ldr r0,[fp,#4]                 @ recup name of program in command line
    bl affichageMess            @ call function
    ldr r0, iAdrszRetourLigne         @ adresse of message
    bl affichageMess            @ call function

 /* fin standard du programme */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur des  2 registres
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszMessage: .int szMessage
iAdrszRetourLigne: .int szRetourLigne
/******************************************************************/
/*     affichage des messages   avec calcul longueur              */
/******************************************************************/
/* r0 contient l adresse du message */
affichageMess:
	push {fp,lr}    /* save des  2 registres */
	push {r0,r1,r2,r7}    /* save des autres registres */
	mov r2,#0   /* compteur longueur */
1:	      /*calcul de la longueur */
        ldrb r1,[r0,r2]  /* recup octet position debut + indice */
	cmp r1,#0       /* si 0 c est fini */
	beq 1f
	add r2,r2,#1   /* sinon on ajoute 1 */
	b 1b
1:	/* donc ici r2 contient la longueur du message */
	mov r1,r0        /* adresse du message en r1 */
	mov r0,#STDOUT      /* code pour écrire sur la sortie standard Linux */
        mov r7, #WRITE                  /* code de l appel systeme "write" */
        swi #0                      /* appel systeme */
	pop {r0,r1,r2,r7}     /* restaur des autres registres */
	pop {fp,lr}    /* restaur des  2 registres */
        bx lr	        /* retour procedure */


```


## AutoHotkey


```AutoHotkey

MsgBox, % A_ScriptName

```


## AWK


```AWK

# syntax: TAWK -f PROGRAM_NAME.AWK
#
# GAWK can provide the invoking program name from ARGV[0] but is unable to
# provide the AWK script name that follows -f.  Thompson Automation's TAWK
# version 5.0c, last released in 1998 and no longer commercially available, can
# provide the AWK script name that follows -f from the PROGFN built-in
# variable.  It should also provide the invoking program name, E.G. TAWK, from
# ARGV[0] but due to a bug it holds the fully qualified -f name instead.
#
# This example is posted here with hopes the TAWK built-in variables PROGFN
# (PROGram File Name) and PROGLN (PROGram Line Number) be added to GAWK by its
# developers.
#
BEGIN {
    printf("%s -f %s\n",ARGV[0],PROGFN)
    printf("line number %d\n",PROGLN)
    exit(0)
}

```

{{out}}

```txt

TAWK -f PROGRAM_NAME.AWK
C:\program_name.awk -f program_name.awk
line number 16

GAWK -f PROGRAM_NAME.AWK
gawk -f
line number 0

```



## BASIC


Many BASICs -- notably older DOS BASICs, and especially DOS MS BASICs -- do not provide any way to retrieve the program's name.

=
## FreeBASIC
=

Unlike most MS BASICs, [[FreeBASIC]] provides a parsed version of <code>[http://www.freebasic.net/wiki/wikka.php?wakka=KeyPgCommand COMMAND$]</code> (called as <code>COMMAND$(n)</code>). <code>COMMAND$(0)</code> is the program's name:

```qbasic
appname = COMMAND$(0)
```


Additionally, FreeBASIC also provides an analog of [[#C|C's]] <code>argc/argv[]</code>, called <code>[http://www.freebasic.net/wiki/wikka.php?wakka=KeyPgDdfbargc __FB_ARGC__]</code> and <code>[http://www.freebasic.net/wiki/wikka.php?wakka=KeyPgDdfbargv __FB_ARGV__]</code>. __FB_ARGV__ can be used to get the program's name like this:

```qbasic
appname = *__FB_ARGV__(0)
```


See also: [[#PowerBASIC|PowerBASIC]], [[#PureBasic|PureBasic]], [[#Visual Basic|Visual Basic]].

=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      SYS "GetCommandLine" TO cl%
      PRINT $$cl%
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Name1.bas"(A,B,C,S$)
110 CHAIN "Name2.BAS"(A,B,C,S$)

edit 1
100 PROGRAM "Name2.bas"(A,B,C,S$)
110 PRINT A,B,C,S$

edit 0
start(1,2,3,"Hello")
```



## C

It might not be very useful for a C program to access source filenames, because C code must be compiled into an executable, and anything could have happened to the source file after the compilation. However, C can access the executable's name in <code>argv[0]</code>.

* <code>argv[0]</code> might be the name of an executable in the PATH, or it might be an absolute or relative path to the executable. At least with [[Unix]], the parent process can set <code>argv[0]</code> to any string, so <code>argv[0]</code> might not be the real name. It is best to pretend that <code>argv[0]</code> has the correct value, but mind that <code>argv[0]</code> might not be an actual file.


```c
#include <stdio.h>

int main(int argc, char **argv) {
	printf("Executable: %s\n", argv[0]);

	return 0;
}
```


To get the source information about some part of code, use compiler defined macros.  Most compilers support them or some variation of.

```c
#include <stdio.h>

int main()
{
	printf("This code was in file %s in function %s, at line %d\n",\
		__FILE__, __FUNCTION__, __LINE__);
	return 0;
}
```



###  BSD

[[BSD]] provides two more ways to get the program's name.

# <code>__progname</code> is the filename from <code>argv[0]</code> (so if <code>argv[0]</code> is a path, then <code>__progname</code> is only the filename). No header file declares <code>__progname</code>, so programs must declare <code>extern char __progname;</code> to use it.
# <code>ucomm</code> always gives the real filename of the executable, even if <code>argv[0]</code> has a different name. <code>ucomm</code> is a field in the process information; <code style="background: black; color: white;">ps -O ucomm</code> prints it. Other than ps(1) and top(1), few programs access <code>ucomm</code>. There is a C interface to the process information, but it often changes between BSD versions.

Starting with OpenBSD 5.0, <code>ucomm</code> is field <code>p_comm</code> of <code>struct kinfo_proc</code>, and <code>kvm_getprocs()</code> in libkvm can fill this struct. (Rosetta Code will welcome contributions for other BSD variants.)

{{libheader|BSD libc}}
{{works with|OpenBSD|5.0}} '''To compile myname.c:''' <code>make myname LDLIBS=-lkvm</code>


```c
/* myname.c */

#include <sys/param.h>
#include <sys/sysctl.h>	/* struct kinfo_proc */
#include <err.h>
#include <fcntl.h>	/* O_RDONLY */
#include <kvm.h>
#include <limits.h>	/* _POSIX2_LINE_MAX */
#include <stdio.h>

int
main(int argc, char **argv) {
	extern char *__progname;	/* from crt0.o */

	struct kinfo_proc *procs;
	kvm_t *kd;
	int cnt;
	char errbuf[_POSIX2_LINE_MAX];

	printf("argv[0]: %s\n", argv[0]);
	printf("__progname: %s\n", __progname);

	kd = kvm_openfiles(NULL, NULL, NULL, KVM_NO_FILES, errbuf);
	if (kd == NULL)
		errx(1, "%s", errbuf);
	procs = kvm_getprocs(kd, KERN_PROC_PID, getpid(),
	    sizeof procs[0], &cnt);
	if (procs == NULL)
		errx(1, "%s", kvm_geterr(kd));
	if (cnt != 1)
		errx(1, "impossible");

	printf("p_comm: %s\n", procs[0].p_comm);

	kvm_close(kd);
	return 0;
}
```


The program can have three different names!


```txt
$ perl -e 'exec {"./myname"} "/nobin/fakename"'
argv[0]: /nobin/fakename
__progname: fakename
ucomm: myname
```



###  Windows

[http://msdn.microsoft.com/en-us/library/ms683197%28v=VS.85%29.aspx GetModuleFileName], from the Win32 API, provides the correct path to the current executable file.

{{libheader|Win32}}

```c
#include <windows.h>
#include <stdlib.h>
#include <wchar.h>

/*
 * Returns the path to the current executable file, in a newly
 * allocated buffer. Use free() to free it.
 */
wchar_t *
exepath(void)
{
	wchar_t *buf, *newbuf;
	long blen, flen;

	/*
	 * Most paths fit in MAX_PATH == 260 characters, but very
	 * long UNC paths might require a larger buffer.
	 */
	buf = NULL;
	for (blen = MAX_PATH; 1; blen += MAX_PATH) {
		/* Enlarge buffer. */
		newbuf = realloc(buf, blen * sizeof buf[0]);
		if (newbuf == NULL) {
			free(buf);
			return NULL;
		}
		buf = newbuf;

		flen = GetModuleFileNameW(NULL, buf, blen);
		if (flen == 0) {
			free(buf);
			return NULL;
		}
		if (flen < blen)
			return buf;
	}
}

/*
 * Print the path to this executable.
 */
int
main()
{
	wchar_t *path;

	path = exepath();
	if (path == NULL) {
		wprintf(L"Sorry, an error occured.\n");
		return 1;
	}

	wprintf(L"Path to executable: %ls\n", path);

	free(path);
	return 0;
}
```



```txt
Path to executable: C:\Users\kernigh\Documents\field\scratch.exe
```



## C++

C++ has difficulty accessing source code filenames, because C code must be compiled into an executable. However, C++ can access the executable's filename.


```cpp
#include <iostream>

using namespace std;

int main(int argc, char **argv) {
	char *program = argv[0];
	cout << "Program: " << program << endl;

	return 0;
}
```

## C#
This effectively outputs the executable name, file path, and any arguments for the current program.

```c#
using System;
namespace ProgramName
{
	class Program
	{
		static void Main(string[] args)
		{
			Console.Write(Environment.CommandLine);
		}
	}
}
```

In a C# application with a reference to System.Windows.Forms, the following can be used to retrieve the executable name and arguments without the full path.

```c#
using System;
namespace ProgramName
{
	class Program
	{
		static void Main(string[] args)
		{
			// Extracts the filename from the full path
			System.IO.FileInfo exeInfo = new System.IO.FileInfo(System.Windows.Forms.Application.ExecutablePath);
			Console.Write(exeInfo.Name);

			// Writes all arguments to the console
			foreach (string argument in args)
			{
				Console.Write(" " + argument);
			}
		}
	}
}
```



## Clojure


```clojure
":";exec lein exec $0 ${1+"$@"}
":";exit

(ns scriptname
  (:gen-class))

(defn -main [& args]
  (let [program (first *command-line-args*)]
    (println "Program:" program)))

(when (.contains (first *command-line-args*) *source-path*)
  (apply -main (rest *command-line-args*)))
```



## COBOL

{{works with|GnuCOBOL}}
COBOL has an internal PROGRAM-ID name, and then the external invocation name.


```COBOL
       identification division.
       program-id. sample.

       data division.
       working-storage section.
       01 progname pic x(16).

       procedure division.
       sample-main.

       display 0 upon argument-number
       accept progname from argument-value
       display "argument-value zero :" progname ":"

       display "function module-id  :" function module-id ":"

       goback.
       end program sample.
```


{{out}}

```txt
prompt$ cobc -xj progname.cob
argument-value zero :./progname      :
function module-id  :sample:
```



## CoffeeScript

scriptname.coffee:

```coffeescript
#!/usr/bin/env coffee

main = () ->
  program = __filename
  console.log "Program: " + program

if not module.parent then main()
```



## Common Lisp

Shebangs require a special tweak to ~/.clisprc.lisp.


```lisp
;;; Play nice with shebangs
(set-dispatch-macro-character #\# #\!
 (lambda (stream character n)
  (declare (ignore character n))
  (read-line stream nil nil t)
  nil))
```



```lisp
#!/bin/sh
#|
exec clisp -q -q $0 $0 ${1+"$@"}
exit
|#

;;; Usage: ./scriptname.lisp

(defun main (args)
 (let ((program (car args)))
  (format t "Program: ~a~%" program)
  (quit)))

;;; With help from Francois-Rene Rideau
;;; http://tinyurl.com/cli-args
(let ((args
       #+clisp (ext:argv)
       #+sbcl sb-ext:*posix-argv*
       #+clozure (ccl::command-line-arguments)
       #+gcl si:*command-args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
       #+cmu extensions:*command-line-strings*
       #+allegro (sys:command-line-arguments)
       #+lispworks sys:*line-arguments-list*
     ))

  (if (member (pathname-name *load-truename*)
              args
              :test #'(lambda (x y) (search x y :test #'equalp)))
    (main args)))
```



## D


```d
#!/usr/bin/env rdmd

import std.stdio;

void main(in string[] args) {
    writeln("Program: ", args[0]);
}
```

{{out}}

```txt
C:\rosetta>program_name.exe
Program: program_name.exe
```



```sh
$ dmd scriptname.d
$ ./scriptname
Program: ./scriptname
```


If the system is configured, the D programming language offers an 'interpreted-looking' mode, which exhibits slightly different behavior than the normal compilation:

```sh
$ ./scriptname.d
Program: /tmp/.rdmd/Users/andrew/Desktop/src/scriptname/scriptname.d.D3B32385A31B968A3CF8CAF1E1426E5F
```



Alternative method using built-in function [http://dlang.org/changelog.html#current_path thisExePath()]
{{works with|D|2.064+}}

```d
// thisExePath function was introduced in D 2.064 (November 5, 2013)

import std.file;
import std.stdio;

void main(string[] args)
{
    writeln("Program: ", thisExePath());
}
```

{{out}}
```txt
Z:\rosettacode>program_name.exe
Program: Z:\rosettacode\program_name.exe
```



## Dart


```dart
#!/usr/bin/env dart

main() {
	var program = new Options().script;
	print("Program: ${program}");
}
```



## Delphi


```Delphi
program ProgramName;

{$APPTYPE CONSOLE}

begin
  Writeln('Program name: ' + ParamStr(0));
  Writeln('Command line: ' + CmdLine);
end.
```


=={{header|Déjà Vu}}==


```dejavu
!print( "Name of this file: " get-from !args 0 )
```



## EchoLisp


```lisp

(js-eval "window.location.href")
    → "http://www.echolalie.org/echolisp/"

```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    console.printLine(program_arguments.asEnumerable());  // the whole command line

    console.printLine(program_arguments[0]); // the program name
}
```



## Emacs Lisp


```lisp
:;exec emacs -batch -l $0 -f main $*

;;; Shebang from John Swaby
;;; http://www.emacswiki.org/emacs/EmacsScripts

(defun main ()
 (let ((program (nth 2 command-line-args)))
  (message "Program: %s" program)))
```


<code>load-file-name</code> is the ".el" or ".elc" currently being loaded.  Within a batch <code>-l</code> it will be the script name, but within sub-loads like <code>require</code> etc it is that sub-load.


## Erlang

If Erlang is used as a script language the function escript:script_name/0 will return the program name.
When compiled Erlang's macros hold information about the running module.


```erlang
%% Compile
%%
%% erlc scriptname.erl
%%
%% Run
%%
%% erl -noshell -s scriptname

-module(scriptname).
-export([start/0]).

start() ->
  Program = ?FILE,
  io:format("Program: ~s~n", [Program]),
  init:stop().
```



## Euphoria


```euphoria
constant cmd = command_line()
puts(1,cmd[2])
```


=={{header|F Sharp|F#}}==

This code correctly prints the program name in three modes:

* Run as a compiled program (either scriptname.exe in Windows, or mono scriptname.exe in Unix)
* Run as an interpreted program (fsharpi scriptname.fsx)
* Run as a dotslashed program in Unix (./scriptname.fsx)


```fsharp
#light (*
	exec fsharpi --exec $0 --quiet
*)

let scriptname =
    let args = System.Environment.GetCommandLineArgs()

    let arg0 = args.[0]

    if arg0.Contains("fsi") then
        let arg1 = args.[1]
        if arg1 = "--exec" then
            args.[2]
        else
            arg1
    else
        arg0

let main =
    printfn "%s" scriptname
```



## Factor



```factor
#! /usr/bin/env factor

USING: namespaces io command-line ;
IN: scriptname

: main ( -- ) script get print ;

MAIN: main
```



## Forth

{{works with|GNU Forth}}

```forth
0 arg type cr    \ gforth or gforth-fast, for example
1 arg type cr     \ name of script
```



## Fortran

Please find example runs in the comments at the beginning of the FORTRAN2003 source.  Program name verification can deter system attackers.  Therefore, the code is provided as a separate easily reused module.

```FORTRAN

! program run with invalid name path/f
!
!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sun Jun  2 00:18:31
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a < unixdict.txt
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!
!Compilation finished at Sun Jun  2 00:18:31



! program run with valid name path/rcname
!
!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sun Jun  2 00:19:01
!
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o rcname && ./rcname
! ./rcname approved.
! program continues...
!
!Compilation finished at Sun Jun  2 00:19:02


module sundry

contains

  subroutine verify_name(required)
    ! name verification reduces the ways an attacker can rename rm as cp.
    character(len=*), intent(in) :: required
    character(len=1024) :: name
    integer :: length, status
    ! I believe get_command_argument is part of the 2003 FORTRAN standard intrinsics.
    call get_command_argument(0, name, length, status)
    if (0 /= status) stop
    if ((len_trim(name)+1) .ne. (index(name, required, back=.true.) + len(required))) stop
    write(6,*) trim(name)//' approved.'
  end subroutine verify_name

end module sundry

program name
  use sundry
  call verify_name('rcname')
  write(6,*)'program continues...'
end program name

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Print "The program was invoked like this => "; Command(0) + " " + Command(-1)
Print "Press any key to quit"
Sleep
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=fc8af45b13a9bb52b6955bab487fc7ac Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sTemp As String

Print "Command to start the program was ";;

For Each sTemp In Args.All
  Print sTemp;;
Next

End
```

Output:

```txt

Command to start the program was  ./CLIOnly.gambas Hello World!

```



## Genie


```genie
[indent=4]
init
    print args[0]
    print Path.get_basename(args[0])
    print Environment.get_application_name()
    print Environment.get_prgname()
```


{{out}}

```txt
prompt$ valac programname.gs
prompt$ ./programname
./programname
programname
(null)
(null)
```



## Go

scriptname.go:


```go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("Program:", os.Args[0])
}
```

{{out|Command line session}}

```txt

> go build scriptname.go
> ./scriptname
Program: ./scriptname
> mv scriptname newname
> ./newname
Program: ./newname

```



## Groovy

All the [[#Java|Java]] solutions work equally well under Groovy when the program is invoked through the "main" method of a class. However, if a Groovy program is invoked as a script, the script runs as an instance method of itself as the instantiating class. If the script is running as a compiled string, the Groovy environment under which it is running assigns it a name.

If you want the script filename, use:


```groovy
#!/usr/bin/env groovy

def program = getClass().protectionDomain.codeSource.location.path

println "Program: " + program
```


But if you just want the class name, there are easier ways.

So, just typing in and running the following in the GroovyConsole environment:

```groovy
println this.class.getName()
```

yields the following on the first run:

```txt
ConsoleScript0
```

and the following on the third run:

```txt
ConsoleScript2
```


But if we save this one line script under the filename "Autonomous.groovy" and then load the file into the console, we get this on every run:

```txt
Autonomous
```


Using the package statement and an appropriate directory hierarchy to provide namespace semantics works exactly as it does in Java.


## Haskell

Haskell has an impure function for this.


```haskell
import System (getProgName)

main :: IO ()
main = getProgName >>= putStrLn . ("Program: " ++)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
write(&progname)    # obtain and write out the program name from the keyword &progname
end
```



## Io


```io
#!/usr/bin/env io

main := method(
	program := System args at(0)

	("Program: " .. program) println
)

if (System args size > 0 and System args at(0) containsSeq("scriptname"), main)
```



## J



```j
#!/usr/bin/env jconsole

program =: monad : 0
	if. (#ARGV) > 1 do.
		> 1 { ARGV
	else.
		'Interpreted'
	end.
)

echo 'Program: ', program 0

exit ''
```



## Java


On one hand, this task is trivial for Java. Java code is (usually) compiled into bytecode as ''class files''. There is exactly one class file per class, named <code><class name>.class</code> (regardless of what the original source files were called or how classes were organized in the source). One executes Java code by executing some class which contains a main method, by running the command <code>java <class name> <possible arguments></code>. Hence, it is guaranteed that the "name" of the executable ''is'' the class name (possibly prepended by package names, using the usual Java dot notation); and this is known in the main method at the time the code is written because ''it is the very class that the main method is in''. Hence, the complicated solutions listed in this section do not gain anything that is not already known by the programmer at the time the code is written.

However, it is tedious to hard-code the class names if you need to do this in a lot of Java programs. Thus, a more interesting task is to '''write a snippet of Java code which, without modification, can be copy-and-pasted into the main method of any class and retrieve the class name'''. This is not trivial because in Java there is no way to use <code>this</code> in a static method to get the class it's in. Listed below are several notable, commonly-cited solutions for this.

You can get the listing of the arguments to the <code>java</code> command through a system property. The first one is the name of the main class that was run. This depends on a system property named "sun.java.command", which might not exist on all Java virtual machines.

```java
public class ScriptName {
	public static void main(String[] args) {
		String program = System.getProperty("sun.java.command").split(" ")[0];
		System.out.println("Program: " + program);
	}
}
```


An alternate solution is to create a dummy inner class, and then retrieve its enclosing class (which is the class the main method is in) through reflection (though this will not work if the code is placed in a method in another source file--it will give the name of the class it is in inside that source file):

```java
public class ScriptName {
	public static void main(String[] args) {
		Class c = new Object(){}.getClass().getEnclosingClass();
		System.out.println("Program: " + c.getName());
	}
}
```


A solution using the security manager:

```java
public class ScriptName {
	public static void main(String[] args) {
		Class c = System.getSecurityManager().getClassContext()[0];
		System.out.println("Program: " + c.getName());
	}
}
```


A solution using the stack trace (requires Java 1.5+):

```java
public class ScriptName {
	public static void main(String[] args) {
		String program = Thread.currentThread().getStackTrace()[1].getClassName();
		System.out.println("Program: " + program);
	}
}
```



## JavaScript


There is no capability within the ECMA-262 standard (the standard for ECMAScript, the language underlying JavaScript) for a function to determine its name. Since objects in JavaScript are first class objects, variables and properties are only references to objects. The name of an object might be said to be the name used to reference it, however a single object may have many variables and properties that reference it, or none.

In some implementations, the following (non–standard) code will work:


```javascript
function foo() {
  return arguments.callee.name;
}
```


But it will fail in in others. JavaScript also has anonymous functions that don't have a name, e.g.:

```javascript
(function(){alert(arguments.callee.name);}())
```

returns an empty string or <code>undefined</code> even where the first example works.

{{works with|Node.js}}
Node.js has a global variable for this.


```javascript
#!/usr/bin/env node
/*jslint nodejs:true */

function main() {
	var program = __filename;
	console.log("Program: " + program);
}

if (!module.parent) { main(); }
```



## Jsish


```javascript
#!/usr/bin/env jsish
/* Program name, in Jsish */
puts('Executable:', Info.executable());
puts('Argv0     :', Info.argv0());
```


{{out}}

```txt
prompt$ jsish programName.jsi
Executable: /usr/local/bin/jsish
Argv0     : /home/btiffin/forge/jsi/jsi-test/rosetta/programName.jsi
```



## Julia

Julia strips the program file name from <tt>ARGS</tt>, so this information is not available to the program from its command line arguments.  Instead it is accessible via <tt>Base.source_path()</tt>.

```Julia

prog = basename(Base.source_path())
println("This program file is \"", prog, "\".")

```


{{out}}

```txt

This program file is "program_name.jl".

```



## Kotlin


```scala
// version 1.0.6

// 'progname.kt' packaged as 'progname.jar'

fun main(args: Array<String>) {
    println(System.getProperty("sun.java.command"))  // may not exist on all JVMs
    println(System.getProperty("java.vm.name"))
}
```


{{out}}

```txt

progname.jar
Java HotSpot(TM) 64-Bit Server VM

```



## Lasso


```lasso
#!/usr/bin/lasso9

stdoutnl("Program: " + $argv->first)
```

Output:

```shell
$ lasso9 script_name.lasso
Program: script_name.lasso
```



## Liberty BASIC


```lb

nSize = _MAX_PATH + 2
lpFilename$ = space$(nSize); chr$(0)

    calldll #kernel32, "GetModuleFileNameA", _
        hModule     as ulong, _
        lpFilename$ as ptr, _
        nSize       as ulong, _
        result      as ulong
lpFilename$ = left$(lpFilename$,result)

print "Path to LB exe"
print lpFilename$
print "current program file (:last one on LRU list)"
print getLastLRU$(lbPath$)
end

Function getLastLRU$(lbPath$)
    open lbPath$+"lbasic404.ini" for input as #1
        while not(eof(#1))
            line input #1, a$
            if instr(a$, "recent files")<>0 then [readRecentFiles]
        wend
        getLastLRU$ = "* Failed: Recent files section not found *"
        close #1
        exit function

[readRecentFiles]
        nRecent = val(word$(a$,1))
        'print "nRecentFiles", nRecent
        for i = 1 to nRecent
            if eof(#1) then
                getLastLRU$ = "* Failed: File ended while in recent files section *"
                close #1
                exit function
            end if
            line input #1, a$
            'print i, a$
        next
    close #1
    getLastLRU$ = a$
end function


```

Output:

```text

Path to LB exe
C:\progs\Liberty BASIC v4.04\liberty.exe
current program file (:last one on LRU list)
C:\progs\Liberty BASIC v4.04\untitled.bas

```



## Lingo


```lingo
put _player.applicationName
-- "lsw.exe"
put _movie.name
-- "lsw_win_d11.dir"
```



## LLVM

Like C, LLVM can use argv to access the executable's filename.


```sh
$ make
llvm-as scriptname.ll
llc -disable-cfi scriptname.bc
gcc -o scriptname scriptname.s
./scriptname
Program: ./scriptname
```


Makefile



```make
all: scriptname.ll
	llvm-as scriptname.ll
	llc scriptname.bc
	gcc -o scriptname scriptname.s
	./scriptname

clean:
	-rm scriptname
	-rm scriptname.s
	-rm scriptname.bc
```



```llvm
@msg_main = internal constant [13 x i8] c"Program: %s\0A\00"

declare i32 @printf(i8* noalias nocapture, ...)

define i32 @main(i32 %argc, i8** %argv) {
	%program = load i8** %argv
	call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @msg_main, i32 0, i32 0), i8* %program)

	ret i32 0
}
```



## Lua

Lua's arg is like C's argv.


```lua
#!/usr/bin/env lua

function main(arg)
	local program = arg[0]
	print("Program: " .. program)
end

if type(package.loaded[(...)]) ~= "userdata" then
	main(arg)
else
	module(..., package.seeall)
end
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Declare GetModuleFileName Lib "kernel32.GetModuleFileNameW"  {Long hModule, &lpFileName$, Long nSize}
      a$=string$(chr$(0), 260)
      namelen=GetModuleFileName(0, &a$, 260)
      a$=left$(a$, namelen)
      \\ normally m2000.exe is the caller of m2000.dll, the activeX script language
      Print Mid$(a$, Rinstr(a$, "\")+1)="m2000.exe"
}
Checkit
\\ command$ return the file's path plus name of script
\\ we can use edit "callme.gsb" to paste these, and use USE callme to call it from M2000 console.
Module SayIt {
      Show
      Print command$
      a$=key$
}
SayIt

```



## Make



```make
NAME=$(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))

all:
	@echo $(NAME)

```



## Mathematica



```mathematica
#!/usr/bin/env MathKernel -script

ScriptName[] = Piecewise[
	{
		{"Interpreted", Position[$CommandLine, "-script", 1] == {}}
	},
	$CommandLine[[Position[$CommandLine, "-script", 1][[1,1]] + 1]]
]

Program = ScriptName[];

Print["Program: " <> Program]
```




## Mercury



```mercury
:- module program_name.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    % The first argument is used as the program name if it is not otherwise
    % available.  (We could also have used the predicate io.progname_base/4
    % if we did not want path preceding the program name.)
    io.progname("", ProgName, !IO),
    io.print_line(ProgName, !IO).

:- end_module program_name.
```



## Mozart/Oz


Makefile:

```make
all: test

test: scriptname
	./scriptname

scriptname: scriptname.oz
	ozc -x scriptname.oz

clean:
	-rm scriptname
	-rm *.exe
```


scriptname.oz:

```oz
functor
import
  System
  Application
  Property
define
  local ScriptName = {Property.get 'application.url'} in
    {System.printInfo "Script name: "#ScriptName#"\n"}
    {Application.exit 0}
  end
end

```



## Nemerle


```Nemerle
using System.Environment;
...
def program_name = GetCommandLineArgs()[0];
...
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

package org.rosettacode.samples

say 'Source: ' source
say 'Program:' System.getProperty('sun.java.command')
return

```

'''Output'''

Called directly:

```txt

$ java org.rosettacode.samples.RProgramName
Source:  Java method RProgramName.nrx
Program: org.rosettacode.samples.RProgramName

```


When bundled inside a JAR file and referenced as the application entry point via the manifest's <tt>Main-Class</tt> header:

```txt

$ java -jar pn.jar
Source:  Java method RProgramName.nrx
Program: pn.jar

```



## newLISP

newLISP has a function, (main-args int) for this.


```lisp
#!/usr/bin/env newlisp

(let ((program (main-args 1)))
  (println (format "Program: %s" program))
  (exit))
```



## Nim


```nim
import os
echo getAppFilename() # Prints the full path of the executed file
echo paramStr(0) # Prints argv[0]
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE ProgramName;
IMPORT
  NPCT:Args,
  Out;
BEGIN
  Out.Object("Program name: " + Args.Get(0));Out.Ln
END ProgramName.

```

Output:

```txt

Program name: ./ProgramName

```

=={{header|Objective-C}}==

scriptname.m:


```objc>#import <Foundation/Foundation.h


int main(int argc, char **argv) {
	@autoreleasepool {

		char *program = argv[0];
		printf("Program: %s\n", program);

		// Alternatively:
		NSString *program2 = [[NSProcessInfo processInfo] processName];
		NSLog(@"Program: %@\n", program2);

	}

	return 0;
}
```



```sh
$ gcc -o scriptname -framework foundation scriptname.m
$ ./scriptname
Program: ./scriptname
```



## OCaml



```ocaml
let () =
  print_endline Sys.executable_name;
  print_endline Sys.argv.(0)
```



```txt

$ ocamlc -o prog_name.bye prog_name.ml
$ ocamlopt -o prog_name.opt prog_name.ml

$ ocaml prog_name.ml
/usr/bin/ocaml
prog_name.ml

$ ./prog_name.bye
./prog_name.bye
./prog_name.bye

$ ./prog_name.opt
/tmp/prog_name.opt
./prog_name.opt

```



## Octave


```octave
function main()
	program = program_name();
	printf("Program: %s", program);
endfunction

main();
```



## Ol

First argument of *vm-args* is an executing program name.

```scheme

(print (car *vm-args*))

```



## Order

This is relatively trivial in Order, as the program being executed is a macro expression in the current C program file being read by the compiler:

```c
__FILE__
```

When including another file containing another <code>ORDER_PP</code> expression, within that file the <code>__FILE__</code> macro will expand to the name of that file; but arguably that expression constitutes a separate Order program within the greater C project.


## PARI/GP

GP does not have access to the name of the program running (especially since it is usually run from the REPL gp). PARI has the same access to <code>argv[0]</code> as [[#C|C]].


## Pascal



```pascal
program ScriptName;
var
	prog : String;
begin
	prog := ParamStr(0);
	write('Program: ');
	writeln(prog)
end.
```



## Perl


```perl
#!/usr/bin/env perl

use strict;
use warnings;

sub main {
	my $program = $0;
	print "Program: $program\n";
}

unless(caller) { main; }
```


<code>$0</code> includes the full path if a script is run as say <code>perl /some/dir/foo.pl</code>.  The <code>FindBin</code> module can give just the basename.  This can be good for printing in diagnostics etc.

```Perl
use FindBin;
print "Program name $FindBin::Script\n";
```



## Perl 6

{{works with|rakudo|2015-10-16}}
In Perl 6, the name of the program being executed is in the special global variable <tt>$*PROGRAM-NAME</tt>.

```perl6
say $*PROGRAM-NAME;
```



## Phix


```Phix
puts(1,command_line()[2])
```



## PHP

PHP has a global dictionary for this.


```php
<?php
$program = $_SERVER["SCRIPT_NAME"];
echo "Program: $program\n";
?>
```



## PicoLisp

The function '[http://software-lab.de/doc/refC.html#cmd cmd]' returns the command name.

```PicoLisp
: (cmd)
-> "/usr/bin/picolisp"
```



## PowerBASIC


Previous versions of PowerBASIC ([[PB/Win]] 8 or older; [[PB/CC]] 4 or older) have to make an [[API]] call:


```powerbasic
#INCLUDE "Win32API.inc"
'[...]
DIM fullpath AS ASCIIZ * 260, appname AS STRING
GetModuleFileNameA 0, fullpath, 260
IF INSTR(fullpath, "\") THEN
    appname = MID$(fullpath, INSTR(-1, fullpath, "\") + 1)
ELSE
    appname = fullpath
END IF
```


{{works with|PowerBASIC for Windows|9}}
{{works with|PowerBASIC Console Compiler|5}}

Recent versions of PowerBASIC provide the <code>EXE</code> object; <code>EXE.NAME$</code> returns the program's name, while <code>EXE.NAMEX$</code> returns the program's name and extension. (<code>EXE.EXTN$</code> returns the extension only.) So, to get the program's name, we do this:

```powerbasic
appname = EXE.NAMEX$
```



## PowerShell


```PowerShell

# write this in file <program.ps1>
$MyInvocation.MyCommand.Name
# launch with <.\program>

```

<b>Output:</b>

```txt

program.ps1

```



## Prolog


```Prolog
% SWI-Prolog version 8.0.0 for i686-linux.
% This will find itself, and return the knowledge base it is in.
file_name(F) :- true
   , M = user            % M is the module    .
   , P = file_name(_)    % P is the predicate .
   , source_file(M:P, F) % F is the file      .
   , \+ predicate_property(M:P, imported_from(_))
   .
```


Alternatively, you may prefer a list of all your knowledge bases; adding the following code to each of your knowledge bases will allow you to query <code>findall(F, source_file(this_is_one_of_my_files, F), L).</code>.

```Prolog
:- multifile(this_is_one_of_my_files). this_is_one_of_my_files.
```



## PureBasic

PureBasic provides a way to retrieve the filename of the program being executed.  It includes the file's path.

```PureBasic
If OpenConsole()
  PrintN(ProgramFilename())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output when executing the above program compiled to an executible file called 'program name.exe':

```txt
H:\Data\Rosetta Code\program name.exe
```



## Python

Python has at least two ways to get the script name: the traditional ARGV and the inspect module.


```python
#!/usr/bin/env python

import sys

def main():
    program = sys.argv[0]
    print("Program: %s" % program)

if __name__ == "__main__":
    main()
```




```python
#!/usr/bin/env python

import inspect

def main():
    program = inspect.getfile(inspect.currentframe())
    print("Program: %s" % program)

if __name__ == "__main__":
    main()
```



## R

R's syntax is complicated, but doable.


```R
#!/usr/bin/env Rscript

getProgram <- function(args) {
	sub("--file=", "", args[grep("--file=", args)])
}

args <- commandArgs(trailingOnly = FALSE)
program <- getProgram(args)

cat("Program: ", program, "\n")

q("no")
```



## Racket


```racket
#!/usr/bin/env racket
#lang racket

(define (program) (find-system-path 'run-file))

(module+ main (printf "Program: ~a\n" (program)))
```



## Raven


```Raven
ARGS list " " join "%s\n" print
```

{{out}}

```txt
raven arg_file.rv
```



## REXX

The [[REXX|Rexx]] <code>PARSE SOURCE</code> instruction parses data describing the source of the program running.
The language processor returns a string that does not change while the program is running.
The source string contains operating system name, followed by either <code>COMMAND</code>, <code>FUNCTION</code>, or
<code>SUBROUTINE</code>, depending on whether the program was called as a host command or from a
function call in an expression or using the <code>CALL</code> instruction.
These two tokens are followed by the complete path specification of the program file.


It should be noted that the format of the complete path varies depending upon the operating system.


```REXX
/* Rexx */

Parse source . . pgmPath
Say pgmPath

```

;Output

```txt

$ rexx RProgramName.rex
/tmp/RProgramName.rex

```


REXX does not support the use of arg(0) to access the program name. A workaround is to use a shell wrapper script to obtain and provide the invocation name of the wrapper:


```sh
#!/bin/sh
rexxbit.rexx $0 $*
```


Here is a rexx script that makes use of this:


```rexx
say "The program is called " arg(1)
```


On TSO, this program

```rexx
/*REXX program RANG1 in PDS N561985.PRIV.CLIST W. Pachl */
Parse Source a b c
Say 'a='a
Say 'b='!!b
Say 'c='c
```

yields

```txt

a=TSO
b=COMMAND
c=RANG1 SYS00056 N561985.PRIV.CLIST ? TSO ISPF ?

```



### version with various types of invokes

Used under Windows/XP and Windows 7 with the following REXXes:
:::* Brexx
:::* R4 REXX
:::* REGINA REXX
:::* Personal REXX
:::* ROO REXX
:::* ooRexx

```rexx
/*REXX pgm displays the name (& possible path) of the REXX program name.*/
parse version _version
parse source _system _howInvoked _path

say right(_version '◄──►' space(arg(1) arg(2)), 79, '─')   /*show title.*/
say "     REXX's name of system being used:"  _system
say '     how the REXX program was invoked:'  _howInvoked
say '    name of the REXX program and path:'  _path

if arg()>1  then return 0              /*don't let this program recurse.*/
                                       /*Mama said that cursing is a sin*/
                                       /*invoke ourself with a  2nd arg.*/
call prog_nam  , 'subroutine'          /*call ourself as a  subroutine. */
zz = prog_nam( , 'function')           /*  "     "     " "  function.   */
                                       /*stick a fork in it, we're done.*/
```

'''output'''   when using '''BREXX''' with the input of:   <tt> command </tt>

```txt

───────────────────────────────────────────brexx 2.1.0 Mar 11 2003 ◄──► command
     REXX's name of system being used: MSDOS
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: prog_nam.rex J:\-\BREXX\REXX16.EXE D:\WIN
DOWS\SYSTEM32\COMMAND.COM
────────────────────────────────────────brexx 2.1.0 Mar 11 2003 ◄──► subroutine
     REXX's name of system being used: MSDOS
     how the REXX program was invoked: PROCEDURE
    name of the REXX program and path: prog_nam.rex J:\-\BREXX\REXX16.EXE D:\WIN
DOWS\SYSTEM32\COMMAND.COM
──────────────────────────────────────────brexx 2.1.0 Mar 11 2003 ◄──► function
     REXX's name of system being used: MSDOS
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: prog_nam.rex J:\-\BREXX\REXX16.EXE D:\WIN
DOWS\SYSTEM32\COMMAND.COM

```

Output note (above):   note that the wrap-around of the output is an artifact of the BREXX interpreter, not the pasting of this output.




'''output'''   when using '''R4 REXX''' with the input of:   <tt> command </tt>

```txt

─────────────────────────────────────────────────REXX-r4 4.00 16 Aug 2015 ◄──► command
     REXX's name of system being used: Win32
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: C:\PROG_NAM.REX * * PROG_NAM
───────────────────────────────────────REXX-r4 4.00 16 Aug 2015 ◄──► subroutine
     REXX's name of system being used: Win32
     how the REXX program was invoked: SUBROUTINE
    name of the REXX program and path: C:\PROG_NAM.REX * * PROG_NAM
─────────────────────────────────────────REXX-r4 4.00 16 Aug 2015 ◄──► function
     REXX's name of system being used: Win32
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: C:\PROG_NAM.REX * * PROG_NAM

```

'''output'''   when using '''REGINA REXX''' with the input of:   <tt> command </tt>

```txt

────────────────────────────────────REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015 ◄──► command
     REXX's name of system being used: WIN32
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: c:\prog_nam.rex
──────────────────────────REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015 ◄──► subroutine
     REXX's name of system being used: WIN32
     how the REXX program was invoked: SUBROUTINE
    name of the REXX program and path: c:\PROG_NAM.rex
────────────────────────────REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015 ◄──► function
     REXX's name of system being used: WIN32
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: c:\PROG_NAM.rex

```

'''output'''   when using '''Personal REXX''' with the input of:   <tt> command </tt>

```txt

────────────────────────────────────REXX/Personal 4.00 21 Mar 1992 ◄──► command
     REXX's name of system being used: PCDOS
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: D:\PROG_NAM.REX
─────────────────────────────────REXX/Personal 4.00 21 Mar 1992 ◄──► subroutine
     REXX's name of system being used: PCDOS
     how the REXX program was invoked: SUBROUTINE
    name of the REXX program and path: D:\PROG_NAM.REX
───────────────────────────────────REXX/Personal 4.00 21 Mar 1992 ◄──► function
     REXX's name of system being used: PCDOS
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: D:\PROG_NAM.REX

```

'''output'''   when using '''ROO REXX''' with the input of:   <tt> command </tt>

```txt

─────────────────────────────────────────REXX-roo 4.00 28 Jan 2007 ◄──► command
     REXX's name of system being used: Win32
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: D:\PROG_NAM.REX * * PROG_NAM
──────────────────────────────────────REXX-roo 4.00 28 Jan 2007 ◄──► subroutine
     REXX's name of system being used: Win32
     how the REXX program was invoked: SUBROUTINE
    name of the REXX program and path: D:\PROG_NAM.REX * * PROG_NAM
────────────────────────────────────────REXX-roo 4.00 28 Jan 2007 ◄──► function
     REXX's name of system being used: Win32
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: D:\PROG_NAM.REX * * PROG_NAM

```

'''output'''   when using '''ooRexx''' with the input of:   <tt> command </tt>

```txt

-----------------------------------REXX-ooRexx_4.1.2(MT) 6.03 28 Aug 2012 ?--?
     REXX's name of system being used: WindowsNT
     how the REXX program was invoked: COMMAND
    name of the REXX program and path: E:\PROG_NAM.REX
-------------------------REXX-ooRexx_4.1.2(MT) 6.03 28 Aug 2012 ?--? subroutine
     REXX's name of system being used: WindowsNT
     how the REXX program was invoked: SUBROUTINE
    name of the REXX program and path: E:\PROG_NAM.REX
---------------------------REXX-ooRexx_4.1.2(MT) 6.03 28 Aug 2012 ?--? function
     REXX's name of system being used: WindowsNT
     how the REXX program was invoked: FUNCTION
    name of the REXX program and path: E:\PROG_NAM.REX

```

Output note of ooRexx (which should be shown in the ooRexx language section):   the output from ooRexx (in this Classic REXX section) needs to be corrected and re-done;   the title is different, it is not showing the argument supplied, and it's incorrectly translating the (solid) left and right arrow characters.


## Ring


```ring

see "Active Source File Name : " + filename() + nl

```

output

```ring

Active Source File Name : tests\filename.ring

```


Check the main file in the program

```ring

if sysargv[2] = filename()
        see "I'm the main program file!" + nl
        # we can run tests here!
else
        see "I'm a sub file in a program" + nl
ok

```



## Ruby


```ruby
#!/usr/bin/env ruby

puts "Path: #{$PROGRAM_NAME}"  # or puts "Path: #{$0}"
puts "Name: #{File.basename $0}"
```


For example,


```txt
$ ruby script.rb
Path: script.rb
Name: script.rb
$ ruby ../rc/script.rb
Path: ../rc/script.rb
Name: script.rb
$ ruby -e 'load "script.rb"'
Path: -e
Name: -e
```



## Rust


scriptname.rs:


```rust
fn main() {
    println!("Program: {}", std::env::args().next().unwrap());
}
```


Example:


```sh
$ rustc scriptname.rs
$ ./scriptname
Program: ./scriptname
```



## SAC


scriptname.sac:


```c
use StdIO: all;
use Array: all;
use String: { string };
use CommandLine: all;

int main() {
	program = argv(0);
	printf("Program: %s\n", program);
	return(0);
}
```


Makefile:


```make
all: scriptname

scriptname: scriptname.sac
	sac2c -o scriptname scriptname.sac

clean:
	-rm scriptname
	-rm scriptname.c
```


Example:


```sh
$ make
sac2c -o scriptname scriptname.sac
$ ./scriptname
Program: ./scriptname
```



## Scala


```scala
object ScriptName extends App {
  println(s"Program of instantiated object: ${this.getClass.getName}")
  // Not recommended, due various implementations
  println(s"Program via enviroment:         ${System.getProperty("sun.java.command")}")
}
```



## Scheme

{{works with|Chicken Scheme}}
Getting the program name is tricky. When interpreted, the script name will be printed. When compiled, the executable name will be printed.


```scheme
#!/bin/sh
#|
exec csi -ss $0 ${1+"$@"}
exit
|#

(use posix)
(require-extension srfi-1) ; lists
(require-extension srfi-13) ; strings

(define (main args)
	(let ((prog (cdr (program))))
		(display (format "Program: ~a\n" prog))
		(exit)))

(define (program)
	(if (string=? (car (argv)) "csi")
		(let ((s-index (list-index (lambda (x) (string-contains x "-s")) (argv))))
			(if (number? s-index)
				(cons 'interpreted (list-ref (argv) (+ 1 s-index)))
				(cons 'unknown "")))
		(cons 'compiled (car (argv)))))

(if (equal? (car (program)) 'compiled)
	(main (cdr (argv))))
```



## Seed7

The function ''path(PROGRAM)'' returns the path of the file executed.
When the program is interpreted this is the path of the source file.
When the program is compiled this is the path of the executable.
The functions ''dir(PROGRAM)'' and ''file(PROGRAM)'' deliver the directory respectivly file name of the program path.

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 0;
  begin
    writeln("Program path:      " <& path(PROGRAM));
    writeln("Program directory: " <& dir(PROGRAM));
    writeln("Program file:      " <& file(PROGRAM));
  end func;
```


Output when the program is interpreted:

```txt

Program path:      /home/anyuser/seed7_5/prg/programname.sd7
Program directory: /home/anyuser/seed7_5/prg
Program file:      programname.sd7

```


Output when the program is compiled:

```txt

Program path:      /home/anyuser/seed7_5/prg/programname
Program directory: /home/anyuser/seed7_5/prg
Program file:      programname

```



## Sidef


```ruby
say __MAIN__;
if (__MAIN__ != __FILE__) {
    say "This file has been included!";
}
```



## Smalltalk


{{works with|GNU Smalltalk}}
Note that this only works when run as "./scriptname.st", because the shebang must force the script name onto ARGV.


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$@"
"exit"

| program |

program := Smalltalk getArgv: 1.

Transcript show: 'Program: ', program; cr.
```


{{works with|Smalltalk/X}}
Works when run in script mode or in a workspace.


```smalltalk
| program |

program := Smalltalk commandLine first.
Transcript show: 'Program: ', program; cr.
```



## Standard ML


```sml
#!/usr/bin/env sml

let
	val program = CommandLine.name ()
in
	print ("Program: " ^ program ^ "\n")
end;
```



## Tcl


```tcl
#!/usr/bin/env tclsh

proc main {args} {
    set program $::argv0
    puts "Program: $program"
}

if {$::argv0 eq [info script]} {
    main {*}$::argv
}
```



## TXR


Given this code in <code>program-name.txr</code>, marked executable:


```txr
#!/usr/local/bin/txr -B
@(bind my-name @self-path)
```


If we run it as an executable:


```txt
$ ./program-name.txr
my-name="./program-name.txr"
```


If we pass it as an argument to <code>txr</code>:


```txt
$ txr program-name.txr
my-name="program-name.txr"
```


If we evaluate the same thing on the command line:


```txt
$ txr -c '@(bind my-name @self-path)'
my-name="cmdline"
```


If we pass in the code on standard input:


```txt
$ txr -
@(bind my-name @self-path)
my-name="stdin"
```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh

echo "Program: $0"
```



## Vala

Get name of program and print to console:

```vala

public static void main(string[] args){
	string command_name = args[0];

	stdout.printf("%s\n", command_name);
}

```


Output for program named "filename":

```txt

./filename

```



## VBA


VBA can retrieve the name of the program hosting the VBA code using the <code>Application</code> object:

```vb>Debug.Print Application.Name</lang


This is mostly useful for code that is shared between, say, [[wp:Microsoft Excel|Microsoft Excel]] and [[wp:Microsoft Word|Microsoft Word]], but has different requirements or actions depending on where it's running.

Wscript.Echo "FullName:",Wscript.FullName

## vbscript


vbscript provides the Wscript object. Among its properties are the following:


```vbscript

Wscript.Echo "FullName:",Wscript.FullName
Wscript.Echo "Name:",Wscript.Name
Wscript.Echo "Path:",Wscript.Path
Wscript.Echo "ScriptFullName:",Wscript.ScriptFullName
Wscript.Echo "ScriptName:",Wscript.ScriptName

```



```txt

FullName: C:\WINDOWS\System32\CScript.exe
Name: Windows Script Host
Path: C:\WINDOWS\System32
ScriptFullName: D:\Utils\test.vbs
ScriptName: test.vbs

```



## Visual Basic


Visual Basic provides the <code>App</code> object, which has a property called <code>EXEName</code> that contains the program's filename ''without the extension''. (For most uses, this doesn't matter, but for code shared between, for example, a program and a screensaver, it can be important.) So, if a program is called "MyVBapp.exe", retreiving the value of <code>App.EXEName</code> would look like this:

```vb
appname = App.EXEName 'appname = "MyVBapp"
```


Alternately, Visual Basic can make an [[API]] call:

```vb
Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Dim fullpath As String * 260, appname As String, namelen As Long
namelen = GetModuleFileName (0, fullpath, 260)
fullpath = Left$(fullpath, namelen)
If InStr(fullpath, "\") Then
    appname = Mid$(fullpath, InStrRev(fullpath, "\") + 1)
Else
    appname = fullpath
End If
```





## x86 Assembly

{{Works with|Nasm}}

### Linux

Makefile:

```make
FORMAT=-f elf
RUN=./
BIN=scriptname
OBJ=scriptname.o

all: test

test: $(BIN)
	$(RUN)$(BIN)

$(BIN): $(OBJ)
	ld -o $(BIN) $(OBJ)

$(OBJ): scriptname.asm
	nasm $(FORMAT) -o $(OBJ) scriptname.asm

clean:
	-rm $(BIN)
	-rm $(OBJ)
```


scriptname.asm:

```asm
bits 32

section .data

stdout equ 1

sys_write equ 4
sys_exit equ 1

kernel equ 0x80

program db "Program: ", 0
programlen equ $-program

nl db "", 10, 0
nllen equ $-nl

section .bss

scriptname resd 1
scriptnamelen resd 1

section .text

global _start

strlen:							; eax: a string ending in 0
	push eax						; cache eax

	.strloop:
		mov bl, byte [eax]
		cmp bl, 0
		je .strret						; return len if bl == 0
		inc eax							; else eax++
		jmp .strloop

	.strret:
		pop ebx							; ebx = cached eax
		sub eax, ebx					; eax -= ebx
		ret								; eax = len

_start:
	mov eax, esp
	add eax, 4
	mov eax, [eax]
	mov dword [scriptname], eax

	mov eax, sys_write
	mov ebx, stdout
	mov ecx, program
	mov edx, programlen
	int kernel

	mov dword eax, [scriptname]
	call strlen
	mov dword [scriptnamelen], eax

	mov eax, sys_write
	mov ebx, stdout
	mov dword ecx, [scriptname]
	mov dword edx, [scriptnamelen]
	int kernel

	mov eax, sys_write
	mov ebx, stdout
	mov ecx, nl
	mov edx, nllen
	int kernel

	mov eax, sys_exit
	mov ebx, 0
	int kernel
```



### FreeBSD/Mac OS X

Makefile:

```make
# FreeBSD defaults

FORMAT=-f elf
RUN=./
BIN=scriptname
OBJ=scriptname.o

# Mac OS X
ifeq ($(shell uname -s),Darwin)
	FORMAT=-f macho
	MINV=-macosx_version_min 10.6
endif

all: test

test: $(BIN)
	$(RUN)$(BIN)

$(BIN): $(OBJ)
	ld -o $(BIN) $(MINV) $(OBJ)

$(OBJ): scriptname.asm
	nasm $(FORMAT) -o $(OBJ) scriptname.asm

clean:
	-rm $(BIN)
	-rm $(OBJ)
```


scriptname.asm:

```asm
bits 32

section .data

stdout equ 1

sys_write equ 4
sys_exit equ 1

kernel equ 0x80

program db "Program: ", 0
programlen equ $-program

nl db "", 10, 0
nllen equ $-nl

section .bss

scriptname resd 1
scriptnamelen resd 1

section .text

global start

strlen:							; eax: a string ending in 0
	push eax						; cache eax

	.strloop:
		mov bl, byte [eax]
		cmp bl, 0
		je .strret						; return len if bl == 0
		inc eax							; else eax++
		jmp .strloop

	.strret:
		pop ebx							; ebx = cached eax
		sub eax, ebx					; eax -= ebx
		ret								; eax = len

start:
	mov eax, esp
	add eax, 4
	mov eax, [eax]
	mov dword [scriptname], eax

	push programlen
	push program
	push stdout
	mov eax, sys_write
	sub esp, 4
	int kernel
	add esp, 4 + 4 * 3

	mov dword eax, [scriptname]
	call strlen
	mov dword [scriptnamelen], eax

	push dword [scriptnamelen]
	push dword [scriptname]
	push stdout
	mov eax, sys_write
	sub esp, 4
	int kernel
	add esp, 4 + 4 * 3

	push nllen
	push nl
	push stdout
	mov eax, sys_write
	sub esp, 4
	int kernel
	add esp, 4 + 4 * 3

	push 0
	mov eax, sys_exit
	sub esp, 4
	int kernel
```



### Windows

Makefile:

```make
FORMAT=-f win32
BIN=scriptname.exe
OBJ=scriptname.obj
RUN=

all: test

test: $(BIN)
	$(RUN)$(BIN)

$(BIN): $(OBJ)
	golink /fo $(BIN) $(OBJ) /console kernel32.dll Msvcrt.dll

$(OBJ): scriptname.asm
	nasm $(FORMAT) -o $(OBJ) scriptname.asm

clean:
	-rm $(BIN)
	-rm $(OBJ)
```


scriptname.asm:

```asm
bits 32

section .data

program db "Program: ", 0
programlen equ $-program

nl db "", 13, 10, 0
nllen equ $-nl

stdouthandle equ -11

section .bss

stdout resd 1

charswritten resd 1

env resd 1
argc resd 1
argv resd 255

scriptname resd 1
scriptnamelen resd 1

section .text

global Start
extern GetStdHandle
extern __getmainargs
extern WriteConsoleA
extern ExitProcess

strlen:				; eax: a string ending in 0
	push eax			; cache eax

	.strloop:
		mov bl, byte [eax]
		cmp bl, 0
		je .strret			; return len if bl == 0
		inc eax				; else eax++
		jmp .strloop

	.strret:
		pop ebx				; ebx = cached eax
		sub eax, ebx		; eax -= ebx
		ret					; eax = len

Start:
	push 0
	push env
	push argv
	push argc
	call __getmainargs
	mov eax, [argv]
	mov eax, [eax]
	mov [scriptname], eax
	add esp, 4 * 4

	push stdouthandle
	call GetStdHandle
	mov [stdout], eax
	add esp, 4 * 1

	push 0
	push charswritten
	push programlen
	push program
	push dword [stdout]
	call WriteConsoleA
	add esp, 4 * 5

	mov eax, [scriptname]
	call strlen
	mov [scriptnamelen], eax

	push 0
	push charswritten
	push dword [scriptnamelen]
	push dword [scriptname]
	push dword [stdout]
	call WriteConsoleA
	add esp, 4 * 5

	push 0
	push charswritten
	push nllen
	push nl
	push dword [stdout]
	call WriteConsoleA
	add esp, 4 * 5

	push 0
	call ExitProcess
```



## Yabasic


```Yabasic
print peek$("program_name")

s$ = system$("cd")
n = len(s$)
print left$(s$, n - 2), "\\", peek$("program_name")
```



## zkl

C's argv is exposed to the zkl runtime so if file bbb.zkl contains:

```zkl
#!/Homer/craigd/Bin/zkl
println(System.argv);
```

Then (on Unix like OSes)

```zkl
./bbb.zkl
zkl bbb.zkl
```

both print

```txt

L("/home/craigd/Projects/ZKL/Bin/zkl","bbb.zkl")

```

On Unix, zkl is actually a shell script:

```bash
#!/bin/sh
# A bash script to run zkl if you haven't jumped
#   through all the Unix hoops to put the bits in the "right" places
# You can change zklRoot to your build directory,
#   change the script name to "zkl" and put it in your bin path.
# You may need to chmod a+x <this script>
if [ -z $zklRoot ]; then
   zklRoot=$HOME/ZKL
   if [ ! -d $zklRoot ]; then
      zklRoot=$HOME/Projects/ZKL
   fi
fi
export zklRoot
#set -o noglob
LD_LIBRARY_PATH=$zklRoot/Lib $zklRoot/Bin/zkl "$@"
```

On Windows, no launch script (windows knows where the DLLs are) but argv[0] can be messed up.


{{omit from|GUISS}}
{{omit from|Retro}}
{{omit from|ZX Spectrum Basic}}
{{omit from|Maxima}}
