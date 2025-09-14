+++
title = "Execute a system command"
description = ""
date = 2019-10-15T09:57:35Z
aliases = []
[extra]
id = 1883
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "11l",
  "abap",
  "ada",
  "aikido",
  "aime",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "arturo",
  "autohotkey",
  "autoit",
  "awk",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "brat",
  "brlcad",
  "c",
  "c_shell",
  "clojure",
  "cmake",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dc",
  "dcl",
  "delphi",
  "e",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "futurebasic",
  "gambas",
  "genie",
  "gnuplot",
  "go",
  "groovy",
  "guiss",
  "haskell",
  "hicest",
  "holyc",
  "idl",
  "io",
  "j",
  "java",
  "javascript",
  "joy",
  "julia",
  "k",
  "kotlin",
  "lang5",
  "lasso",
  "lfe",
  "liberty_basic",
  "limbo",
  "lingo",
  "locomotive_basic",
  "logo",
  "logtalk",
  "lua",
  "m4",
  "make",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "mercury",
  "min",
  "mumps",
  "netrexx",
  "newlisp",
  "nim",
  "ocaml",
  "octave",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pop11",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "red",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "slate",
  "smalltalk",
  "sql_pl",
  "standard_ml",
  "stata",
  "tcl",
  "toka",
  "tuscript",
  "unix_shell",
  "ursa",
  "ursala",
  "vbscript",
  "vedit_macro_language",
  "visual_basic",
  "visual_basic_dotnet",
  "wart",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Run either the   <tt>'''ls'''</tt>   system command   (<tt>'''dir'''</tt>   on Windows),   or the   <tt>'''pause'''</tt>    system command.



## Related tasks

* [[Get_system_command_output | Get system command output]]





## 11l


```11l
os:(‘pause’)
```



## ABAP

ABAP report which checks if there is an external command called 'ls' for the os of the current application server. When running on Windows, it calls dir, for all other platforms ls. A new command is created if not existing and run.


```abap
*&---------------------------------------------------------------------*
*& Report  ZEXEC_SYS_CMD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zexec_sys_cmd.

DATA: lv_opsys      TYPE          syst-opsys,
      lt_sxpgcotabe TYPE TABLE OF sxpgcotabe,
      ls_sxpgcotabe LIKE LINE OF  lt_sxpgcotabe,
      ls_sxpgcolist TYPE          sxpgcolist,
      lv_name       TYPE          sxpgcotabe-name,
      lv_opcommand  TYPE          sxpgcotabe-opcommand,
      lv_index      TYPE          c,
      lt_btcxpm     TYPE TABLE OF btcxpm,
      ls_btcxpm     LIKE LINE OF  lt_btcxpm
      .

* Initialize
lv_opsys = sy-opsys.
CLEAR lt_sxpgcotabe[].

IF lv_opsys EQ 'Windows NT'.
  lv_opcommand = 'dir'.
ELSE.
  lv_opcommand = 'ls'.
ENDIF.

* Check commands
SELECT * FROM sxpgcotabe INTO TABLE lt_sxpgcotabe
  WHERE opsystem  EQ lv_opsys
    AND opcommand EQ lv_opcommand.

IF lt_sxpgcotabe IS INITIAL.
  CLEAR ls_sxpgcolist.
  CLEAR lv_name.
  WHILE lv_name IS INITIAL.
* Don't mess with other users' commands
    lv_index = sy-index.
    CONCATENATE 'ZLS' lv_index INTO lv_name.
    SELECT * FROM sxpgcostab INTO ls_sxpgcotabe
      WHERE name EQ lv_name.
    ENDSELECT.
    IF sy-subrc = 0.
      CLEAR lv_name.
    ENDIF.
  ENDWHILE.
  ls_sxpgcolist-name      = lv_name.
  ls_sxpgcolist-opsystem  = lv_opsys.
  ls_sxpgcolist-opcommand = lv_opcommand.
* Create own ls command when nothing is declared
  CALL FUNCTION 'SXPG_COMMAND_INSERT'
    EXPORTING
      command                = ls_sxpgcolist
      public                 = 'X'
    EXCEPTIONS
      command_already_exists = 1
      no_permission          = 2
      parameters_wrong       = 3
      foreign_lock           = 4
      system_failure         = 5
      OTHERS                 = 6.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
* Hooray it worked! Let's try to call it
    CALL FUNCTION 'SXPG_COMMAND_EXECUTE_LONG'
      EXPORTING
        commandname                   = lv_name
      TABLES
        exec_protocol                 = lt_btcxpm
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      WRITE: 'Cant execute ls - '.
      CASE sy-subrc.
        WHEN 1.
          WRITE: / ' no permission!'.
        WHEN 2.
          WRITE: / ' command could not be created!'.
        WHEN 3.
          WRITE: / ' parameter list too long!'.
        WHEN 4.
          WRITE: / ' security risk!'.
        WHEN 5.
          WRITE: / ' wrong call of SXPG_COMMAND_EXECUTE_LONG!'.
        WHEN 6.
          WRITE: / ' command cant be started!'.
        WHEN 7.
          WRITE: / ' program terminated!'.
        WHEN 8.
          WRITE: / ' x_error!'.
        WHEN 9.
          WRITE: / ' parameter missing!'.
        WHEN 10.
          WRITE: / ' too many parameters!'.
        WHEN 11.
          WRITE: / ' illegal command!'.
        WHEN 12.
          WRITE: / ' wrong asynchronous parameters!'.
        WHEN 13.
          WRITE: / ' cant enqueue job!'.
        WHEN 14.
          WRITE: / ' cant create job!'.
        WHEN 15.
          WRITE: / ' unknown error!'.
        WHEN OTHERS.
          WRITE: / ' unknown error!'.
      ENDCASE.
    ELSE.
      LOOP AT lt_btcxpm INTO ls_btcxpm.
        WRITE: / ls_btcxpm.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDIF.
```



## Ada

Using the IEEE POSIX Ada standard, P1003.5c:

```ada
with POSIX.Unsafe_Process_Primitives;

procedure Execute_A_System_Command is
   Arguments : POSIX.POSIX_String_List;
begin
   POSIX.Append (Arguments, "ls");
   POSIX.Unsafe_Process_Primitives.Exec_Search ("ls", Arguments);
end Execute_A_System_Command;
```


Importing the C system() function:

```ada
with Interfaces.C; use Interfaces.C;

procedure Execute_System is
    function Sys (Arg : Char_Array) return Integer;
    pragma Import(C, Sys, "system");
    Ret_Val : Integer;
begin
    Ret_Val := Sys(To_C("ls"));
end Execute_System;
```


Using the GNAT run-time library:

```ada

with Ada.Text_IO;     use Ada.Text_IO;
with System.OS_Lib;   use System.OS_Lib;

procedure Execute_Synchronously is
   Result    : Integer;
   Arguments : Argument_List :=
                 (  1=> new String'("cmd.exe"),
                    2=> new String'("/C dir c:\temp\*.adb")
                 );
begin
   Spawn
   (  Program_Name           => "cmd.exe",
      Args                   => Arguments,
      Output_File_Descriptor => Standout,
      Return_Code            => Result
   );
   for Index in Arguments'Range loop
      Free (Arguments (Index)); -- Free the argument list
   end loop;
end Execute_Synchronously;

```



## Aikido

The simplest way to do this is using the <code>system()</code> function.  It returns a vector of strings (the output from the command).

```aikido

var lines = system ("ls")
foreach line lines {
    println (line)
}

```

If you don't want to process the output you can use the <code>exec</code> function.  It writes the output to the standard output stream by default;

```aikido

exec ("ls")

```

You also have the regular <code>fork</code> and <code>execv</code> calls available:

```aikido

var pid = fork()
if (pid == 0) {
    var args = ["/bin/ls"]
    execv ("/bin/ls", args)
    exit(1)
}
var status = 0
waitpid (pid, status)


```



## Aime


```aime
sshell ss;

ss.argv.insert("ls");

o_(ss.link);

```



## ALGOL 68

```algol68
system("ls")
```


Or the classic "!" shell escape can be implemented as an "!" operator:

```algol68
OP ! = (STRING cmd)BOOL: system(cmd) = 0;

IF ! "touch test.tmp" ANDF ( ! "ls test.tmp" ANDF ! "rm test.tmp" ) THEN
  print (("test.tmp now gone!", new line))
FI
```



## AppleScript


```applescript
do shell script "ls" without altering line endings
```


## Applesoft BASIC


```ApplesoftBASIC
? CHR$(4)"CATALOG"
```



## Arturo



```arturo
print $(shell "ls")
```


```txt

data.log
rosetta.art
sample.txt
```



## AutoHotkey


```autohotkey
Run, %comspec% /k dir & pause
```



## AutoIt


```AutoIt
Run(@ComSpec & " /c " & 'pause', "", @SW_HIDE)
```



## AWK


Using system() function:

```awk
BEGIN {
  system("ls")		# Unix
 #system("dir")		# DOS/MS-Windows
}
```


Using getline command:

```awk
BEGIN {
         ls = sys2var("ls")
         print ls
}
function sys2var(command        ,fish, scale, ship) {
         command = command " 2>/dev/null"
         while ( (command | getline fish) > 0 ) {
             if ( ++scale == 1 )
                 ship = fish
             else
                 ship = ship "\n" fish
         }
         close(command)
         return ship
}
```



## BASIC



```qbasic
SHELL "dir"
```


=
## BaCon
=

```freebasic
' Execute a system command
SYSTEM "ls"
```



## Batch File



```batch>dir</lang



## BBC BASIC

On Acorn computers the *CAT command catalogues the current directory, the equivalent of the Unix ls command or the DOS/Windows dir command. The BBC BASIC OSCLI command passes a string to the Command Line Interpreter to execute a system command, it is the equivalent of C's system() command.

```bbcbasic
OSCLI "CAT"
```


With BBC BASIC for Windows you can execute the Windows dir command:

```bbcbasic
OSCLI "*dir":REM *dir to bypass BB4W's built-in dir command
```


And if running BBC BASIC on a Unix host, you can execute the ls command:

```bbcbasic
OSCLI "ls"
```



## Bracmat


```bracmat
sys$dir
```



## Brat


```brat
include :subprocess

p subprocess.run :ls  #Lists files in directory
```



## Brlcad



```brlcad

exec ls

```



## C

ISO C & POSIX:


```cpp
#include <iostream>

int main()
{
    system("ls");
    return 0;
}
```



## C++

```cpp
system("pause");
```


## C#
Using Windows / .NET:

```c#
using System.Diagnostics;

namespace Execute
{
    class Program
    {
        static void Main(string[] args)
        {
            Process.Start("cmd.exe", "/c dir");
        }
    }
}
```


```c#
using System;

  class Execute {
     static void Main() {
         System.Diagnostics.Process proc = new System.Diagnostics.Process();
         proc.EnableRaisingEvents=false;
         proc.StartInfo.FileName="ls";
         proc.Start();
    }
 }
```


## Clojure



```lisp
(.. Runtime getRuntime (exec "cmd /C dir"))
```


```lisp


user=> (use '[clojure.java.shell :only [sh]])

user=> (sh "ls" "-aul")

{:exit 0,
 :out total 64
drwxr-xr-x  11 zkim  staff    374 Jul  5 13:21 .
drwxr-xr-x  25 zkim  staff    850 Jul  5 13:02 ..
drwxr-xr-x  12 zkim  staff    408 Jul  5 13:02 .git
-rw-r--r--   1 zkim  staff     13 Jul  5 13:02 .gitignore
-rw-r--r--   1 zkim  staff  12638 Jul  5 13:02 LICENSE.html
-rw-r--r--   1 zkim  staff   4092 Jul  5 13:02 README.md
drwxr-xr-x   2 zkim  staff     68 Jul  5 13:15 classes
drwxr-xr-x   5 zkim  staff    170 Jul  5 13:15 lib
-rw-r--r--@  1 zkim  staff   3396 Jul  5 13:03 pom.xml
-rw-r--r--@  1 zkim  staff    367 Jul  5 13:15 project.clj
drwxr-xr-x   4 zkim  staff    136 Jul  5 13:15 src
, :err }

```



```lisp

user=> (use '[clojure.java.shell :only [sh]])

user=> (println (:out (sh "cowsay" "Printing a command-line output")))

 _________________________________
< Printing a command-line output. >
 ---------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

nil

```



## CMake

```cmake
execute_process(COMMAND ls)
```


Because of a quirk in the implementation ([http://cmake.org/gitweb?p=cmake.git;a=blob;f=Source/cmExecuteProcessCommand.cxx;hb=HEAD cmExecuteProcessCommand.cxx] and [http://cmake.org/gitweb?p=cmake.git;a=blob;f=Source/kwsys/ProcessUNIX.c;hb=HEAD ProcessUNIX.c]), CMake diverts the standard output to a pipe. The effect is like running <code>ls | cat</code> in the shell. The ''ls'' process inherits the original standard input and standard error, but receives a new pipe for standard output. CMake then reads this pipe and copies all data to the original standard output.

''execute_process()'' can also chain commands in a pipeeline, and capture output.


```cmake
# Calculate pi to 40 digits after the decimal point.
execute_process(
  COMMAND printf "scale = 45; 4 * a(1) + 5 / 10 ^ 41\\n"
  COMMAND bc -l
  COMMAND sed -e "s/.\\{5\\}$//"
  OUTPUT_VARIABLE pi OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "pi is ${pi}")
```



```txt
-- pi is 3.1415926535897932384626433832795028841972
```



## COBOL

```cobol
CALL "SYSTEM" USING BY CONTENT "ls"
```



## CoffeeScript

```coffeescript

{ spawn } = require 'child_process'

ls = spawn 'ls'

ls.stdout.on 'data', ( data ) -> console.log "Output: #{ data }"

ls.stderr.on 'data', ( data ) -> console.error "Error: #{ data }"

ls.on 'close', -> console.log "'ls' has finished executing."

```



## Common Lisp

```lisp
(with-output-to-string (stream) (extensions:run-program "ls" nil :output stream))
```


```lisp
(system:call-system "ls")
```


```lisp
(trivial-shell:shell-command "ls")
```



## D


```d

import std.process, std.stdio;
//these two alternatives wait for the process to return, and capture the output
//each process function returns a Tuple of (int)"status" and (string)"output
auto ls_string = executeShell("ls -l"); //takes single string
writeln((ls_string.status == 0) ? ls_string.output : "command failed");

auto ls_array = execute(["ls", "-l"]); //takes array of strings
writeln((ls_array.status == 0) ? ls_array.output : "command failed");
//other alternatives exist to spawn processes in parallel and capture output via pipes

```

std.process.system() is deprecated.


## dc


```dc
! ls
```


## DCL


```DCL>Directory</lang

Or, shorter
```DCL>dir</lang



## Delphi


```Delphi
program ExecuteSystemCommand;

{$APPTYPE CONSOLE}

uses Windows, ShellApi;

begin
  ShellExecute(0, nil, 'cmd.exe', ' /c dir', nil, SW_HIDE);
end.
```



## E


```e
def ls := makeCommand("ls")
ls("-l")

def [results, _, _] := ls.exec(["-l"])
when (results) -> {
  def [exitCode, out, err] := results
  print(out)
} catch problem {
  print(`failed to execute ls: $problem`)
}
```



## Emacs Lisp


### Syncronous


```Lisp
(shell-command "ls")
```



### Asyncronous


```Lisp
(async-shell-command "ls")
```



## Erlang


```erlang
os:cmd("ls").
```



## ERRE

In ERRE language you have the SHELL command followed, eventually, by a string command.
SHELL itself opens a new DOS/Windows shell: you must use EXIT to end.
For example

<lang> SHELL("DIR/W")
```


lists the current directory and then returns to the program.


```ERRE
cmd$="DIR/W"
SHELL(cmd$)
```



## Euphoria

Euphoria has 2 systems command functions: '''system()''' and '''system_exec()'''.

```euphoria
 -- system --
-- the simplest way --
-- system spawns a new shell so I/O redirection is possible --

system( "dir /w c:\temp\ " ) -- Microsoft --

system( "/bin/ls -l /tmp" ) -- Linux BSD OSX --

----

 -- system_exec() --
 -- system_exec does not spawn a new shell --
 -- ( like bash or cmd.exe ) --

integer exit_code = 0
sequence ls_command = ""

ifdef UNIX or LINUX or OSX then
    ls_command = "/bin/ls -l "
elsifdef WINDOWS then
    ls_command = "dir /w "
end ifdef

exit_code = system_exec( ls_command )

if exit_code = -1 then
    puts( STDERR, " could not execute " & ls_command & "\n" )
elsif exit_code = 0 then
    puts( STDERR, ls_command & " succeeded\n")
else
    printf( STDERR, "command %s failed with code %d\n", ls_command, exit_code)
end if
```


=={{header|F_Sharp|F#}}==

```fsharp
System.Diagnostics.Process.Start("cmd", "/c dir")
```



## Factor


```factor
"ls" run-process wait-for-process
```



## Fantom


The Process class handles creating and running external processes.  in/out/err streams can be redirected, but default to the usual stdin/stdout/stderr.  So following program prints result of 'ls' to the command line:


```fantom

class Main
{
  public static Void main ()
  {
    p := Process (["ls"])
    p.run
  }
}

```



## Forth

```forth
s" ls" system
```



## Fortran

execute_command_line subroutine in Fortran 2008 and later  runs a system command

```fortran

program SystemTest
integer :: i
 call execute_command_line ("ls", exitstat=i)
end program SystemTest

```


The <tt>SYSTEM</tt> subroutine (and function) are a GNU extension.

```fortran
program SystemTest
  call system("ls")
end program SystemTest
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Shell "dir"
Sleep
```



## Frink


```frink
r = callJava["java.lang.Runtime", "getRuntime"]
println[read[r.exec["dir"].getInputStream[]]]
```



## FunL


```funl
import sys.execute

execute( if $os.startsWith('Windows') then 'dir' else 'ls' )
```



## FutureBasic

This simple example prints the output to a console window. With its open "Unix" command, FB has robust capability as a system interface to the Free BSD Unix core of Macintosh OS X 10.x.

```futurebasic

include "ConsoleWindow"

local fn DoUnixCommand( cmd as str255 )
  dim as str255 s

  open "Unix", 2, cmd
  while ( not eof(2) )
    line input #2, s
    print s
  wend
  close 2
end fn

fn DoUnixCommand( "ls -A" )

```


Output:

```txt

.DocumentRevisions-V100
.Spotlight-V100
.Trashes
.file
.fseventsd
.hotfiles.btree
.vol
Applications
Library
Network
System
Users
Volumes
bin
cores
dev
etc
home
mach_kernel
net
private
sbin
tmp
usr
var

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=9460b39a86794a7346a390aeb50fc5cf Click this link to run this code]'''

```gambas
Public Sub Main()

Shell "ls -aul"

End
```

Output:

```txt

total 36364
drwxr-xr-x  88 charlie charlie     4096 May 29 10:26 .
drwxr-xr-x   5 root    root        4096 May 26 15:44 ..
drwxr-xr-x   2 charlie charlie     4096 May 29 10:54 15PuzzleGame
drwx------   3 charlie charlie     4096 May 28 13:51 .adobe
drwxr-xr-x   4 charlie charlie     4096 May 28 13:52 .audacity-data
drwxr-xr-x   4 charlie charlie     4096 May 28 13:51 .barcode
etc....

```



## Genie


```genie
[indent=4]
/*
  Execute system command, in Genie

  valac executeSystemCommand.gs
  ./executeSystemCommand
*/

init
    try
        // Non Blocking
        Process.spawn_command_line_async("ls")
    except e : SpawnError
        stderr.printf("%s\n", e.message)
```


Output is asynchronous (could be made synchronous with ''spawn_command_line_sync''), and elided here for the sample capture.

```txt
prompt$ valac executeSystemCommand.gs
prompt$ ./executeSystemCommand
...
aplusb            executeSystemCommand       hello.gs          helloNoNewline.gs
memavail          progress-bar               readfile.vapi     stringsample.vala
...

```



## Go


```go
package main

import (
    "log"
    "os"
    "os/exec"
)

func main() {
    cmd := exec.Command("ls", "-l")
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        log.Fatal(err)
    }
}
```



## gnuplot



```gnuplot
!ls
```



## Groovy


```groovy
println "ls -la".execute().text
```



## GUISS



```guiss
Start,Programs,Accessories,MSDOS Prompt,Type:dir[enter]
```



## Haskell

```haskell
import System.Cmd

main = system "ls"

```


See also: the [http://www.haskell.org/ghc/docs/latest/html/libraries/process-1.2.0.0/System-Process.html System.Process] module


## HicEst


```hicest
SYSTEM(CoMmand='pause')
SYSTEM(CoMmand='dir & pause')
```



## HolyC

HolyC is the official programming language for The Temple Operating System (TempleOS). The Temple Operating System interpreter executes just-in-time compiled HolyC code. All HolyC code is effectively executed as system commands.

For example, to execute the <code>Dir</code> command:


```holyc>Dir;</lang


=={{header|Icon}} and {{header|Unicon}}==
The code below selects the 'ls' or 'dir' command at runtime based on the UNIX feature.


```Icon
procedure main()

write("Trying command ",cmd := if &features == "UNIX" then "ls" else "dir")
system(cmd)

end
```


Unicon extends system to allow specification of files and a wait/nowait parameter as in the examples below.

```Icon

  pid := system(command_string,&input,&output,&errout,"wait")
  pid := system(command_string,&input,&output,&errout,"nowait")

```


## IDL


```idl
$ls
```


Will execute "ls" with output to the screen.


```idl
spawn,"ls",result
```


will execute it and store the result in the string array "result".


```idl
spawn,"ls",unit=unit
```


will execute it asynchronously and direct any output from it into the LUN "unit" from whence it can be read at any (later) time.


## Io


```io
System runCommand("ls") stdout println
```



## J


The system command interface in J is provided by the standard "task" script:

```j
load'task'

NB.  Execute a command and wait for it to complete
shell 'dir'

NB.  Execute a command but don't wait for it to complete
fork 'notepad'

NB.  Execute a command and capture its stdout
stdout   =:  shell 'dir'

NB.  Execute a command, provide it with stdin,
NB.  and capture its stdout
stdin    =:  'blahblahblah'
stdout   =:  stdin spawn 'grep blah'
```


Note that on unix systems, you can also use the [http://www.jsoftware.com/help/dictionary/dx002.htm 2!:x family] of foreign verbs to execute system commands.


## Java

```java5
import java.util.Scanner;
import java.io.*;

public class Program {
    public static void main(String[] args) {
    	try {
    		Process p = Runtime.getRuntime().exec("cmd /C dir");//Windows command, use "ls -oa" for UNIX
    		Scanner sc = new Scanner(p.getInputStream());
    		while (sc.hasNext()) System.out.println(sc.nextLine());
    	}
    	catch (IOException e) {
    		System.out.println(e.getMessage());
    	}
    }
}
```


There are two ways to run system commands. The simple way, which will hang the JVM (I would be interested in some kind of reason). -- this happens because the the inputStream buffer fills up and blocks until it gets read. Moving your .waitFor after reading the InputStream would fix your issue (as long as your error stream doesn't fill up)

```java
import java.io.IOException;
import java.io.InputStream;

public class MainEntry {
    public static void main(String[] args) {
        executeCmd("ls -oa");
    }

    private static void executeCmd(String string) {
        InputStream pipedOut = null;
        try {
            Process aProcess = Runtime.getRuntime().exec(string);
            aProcess.waitFor();

            pipedOut = aProcess.getInputStream();
            byte buffer[] = new byte[2048];
            int read = pipedOut.read(buffer);
            // Replace following code with your intends processing tools
            while(read >= 0) {
                System.out.write(buffer, 0, read);

                read = pipedOut.read(buffer);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        } finally {
            if(pipedOut != null) {
                try {
                    pipedOut.close();
                } catch (IOException e) {
                }
            }
        }
    }


}
```


And the right way, which uses threading to read the InputStream given by the process.

```java
import java.io.IOException;
import java.io.InputStream;

public class MainEntry {
    public static void main(String[] args) {
        // the command to execute
        executeCmd("ls -oa");
    }

    private static void executeCmd(String string) {
        InputStream pipedOut = null;
        try {
            Process aProcess = Runtime.getRuntime().exec(string);

            // These two thread shall stop by themself when the process end
            Thread pipeThread = new Thread(new StreamGobber(aProcess.getInputStream()));
            Thread errorThread = new Thread(new StreamGobber(aProcess.getErrorStream()));

            pipeThread.start();
            errorThread.start();

            aProcess.waitFor();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }
    }
}

//Replace the following thread with your intends reader
class StreamGobber implements Runnable {

    private InputStream Pipe;

    public StreamGobber(InputStream pipe) {
        if(pipe == null) {
            throw new NullPointerException("bad pipe");
        }
        Pipe = pipe;
    }

    public void run() {
        try {
            byte buffer[] = new byte[2048];

            int read = Pipe.read(buffer);
            while(read >= 0) {
                System.out.write(buffer, 0, read);

                read = Pipe.read(buffer);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(Pipe != null) {
                try {
                    Pipe.close();
                } catch (IOException e) {
                }
            }
        }
    }
}
```



## JavaScript

JavaScript does not have any facilities to interact with the OS.  However, host environments can provide this ability.

```javascript
var shell = new ActiveXObject("WScript.Shell");
shell.run("cmd /c dir & pause");
```


```javascript
runCommand("cmd", "/c", "dir", "d:\\");
print("===");
var options = {
    // can specify arguments here in the options object
    args: ["/c", "dir", "d:\\"],
    // capture stdout to the options.output property
    output: ''
};
runCommand("cmd", options);
print(options.output);
```



## Joy


```joy
"ls" system.
```



## Julia

The Julia manual has an excellent [http://docs.julialang.org/en/release-0.3/manual/running-external-programs/ section] on this topic, which is worth a read.  The short answer on Linux is:

```Julia
run(`ls`)
```


```txt

$ ls
bitmap_bresenham_line.jl   completed                  single_link_list_collection.jl
color_quantization_in.png  execute_system_command.jl  single_link_list_insert.jl
color_quantization.jl      README.md                  support
$ julia execute_system_command.jl
bitmap_bresenham_line.jl   completed                  single_link_list_collection.jl
color_quantization_in.png  execute_system_command.jl  single_link_list_insert.jl
color_quantization.jl      README.md                  support

```



## K


Execute "ls"

```K>    \ls</lang


Execute "ls" and capture the output in the variable "r":

```K
   r: 4:"ls"
```



## Kotlin


```scala
// version 1.0.6

import java.util.Scanner

fun main(args: Array<String>) {
    val proc = Runtime.getRuntime().exec("cmd /C dir")  // testing on Windows 10
    Scanner(proc.inputStream).use {
        while (it.hasNextLine()) println(it.nextLine())
    }
}
```



## Lang5

For one-word commands:

```Lang5
'ls system
```

For multi-word commands:

```Lang5
"ls -a" system
```



## Lasso


```Lasso
local(
	path	= file_forceroot,
	ls	= sys_process('/bin/ls', (:'-l', #path)),
	lswait	= #ls -> wait
)
'
```txt
'
#ls -> read
'
```
'
```


```txt
total 16
drwxr-xr-x  8 _lasso  staff  272 Nov 10 08:13 mydir
-rw-r--r--  1 _lasso  staff   38 Oct 29 16:05 myfile.lasso
-rw-r--r--@ 1 _lasso  staff  175 Oct 29 18:18 rosetta.lasso
```



## LFE


In the LFE REPL:


```lisp

> (os:cmd "ls -alrt")

```


That will display output on a single line, with literal newlines.

For pretty output, compose with <code>io:format</code>:


```lisp

> (io:format (os:cmd "ls -alrt"))

```




## Liberty BASIC


```lb

 drive1$ = left$(Drives$,1)
run "cmd.exe /";drive1$;" dir & pause"

```



## Lingo

```lingo
sx = xtra("Shell").new()
if the platform contains "win" then
  put sx.shell_cmd("dir")
else
  put sx.shell_cmd("ls")
end if
```



## Limbo


There is no equivalent to Unix's exec() in Inferno per se; commands are just modules that have at least an init() function with the correct signature, and are loaded the same way as any other module.  (As a result, there's nothing in the language or OS that prevents a program from acting as both a command and a library except convention.)

This version passes its argument list through to ls:


```Limbo
implement Runls;

include "sys.m"; sys: Sys;
include "draw.m";
include "sh.m";

Runls: module {
	init: fn(ctxt: ref Draw->Context, args: list of string);
};

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	ls := load Command "/dis/ls.dis";
	if(ls == nil)
		die("Couldn't load /dis/ls.dis");
	ls->init(ctxt, "ls" :: tl args);
}

die(s: string)
{
	sys->fprint(sys->fildes(2), "runls: %s: %r", s);
	raise "fail:errors";
}
```


It's not strictly necessary to pass the graphics context to ls, but it is generally a good idea to do so when calling another program.


## Locomotive Basic


The Amstrad CPC464 uses a ROM based basic interpreter, so every statement within the program is a system command. If a command without a line number is typed, whilst the computer is in a ready state, the command gets executed immediately. There is no pause command, so in this example, we use the list command (which exhibits totally different behaviour to a pause command):


```basic>LIST</lang



## Logo

The lines of output of the SHELL command are returned as a list.

```logo
print first butfirst shell [ls -a]   ; ..
```



## Logtalk

Using the standard library:

```logtalk
os::shell('ls -a').
```



## Lua


```lua
-- just executing the command
os.execute("ls")

-- to execute and capture the output, use io.popen
local f = io.popen("ls") -- store the output in a "file"
print( f:read("*a") )    -- print out the "file"'s content
```



## M4


```M4
syscmd(ifdef(`__windows__',`dir',`ls'))
```



## Make

make can use system command in either definition of variables or in the targets

in definition


```make
contents=$(shell cat foo)
curdir=`pwd`
```


in target


```make
mytarget:
   cat foo | grep mytext
```



## Maple


```Maple
ssystem("dir");
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Run["ls"]
```



## MATLAB

To execute system commands in MATLAB, use the "system" keyword.

Sample Usage:

```MATLAB>>
 system('PAUSE')

Press any key to continue . . .


ans =

     0

```



## Maxima

<lang>system("dir > list.txt")$
```



## MAXScript


```maxscript
dosCommand "pause"
```



## Mercury

<lang>
:- module execute_sys_cmd.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
   io.call_system("ls", _Result, !IO).

```



## min

```min
!dir
```


=={{header|Modula-2}}==

```modula2
MODULE tri;

FROM   SYSTEM           IMPORT  ADR;
FROM   SysLib           IMPORT  system;

IMPORT TextIO, InOut, ASCII;

VAR   fd                : TextIO.File;
      ch                : CHAR;

PROCEDURE SystemCommand (VAR  command : ARRAY OF CHAR) : BOOLEAN;

BEGIN
   IF  system (ADR (command) ) = 0  THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END SystemCommand;

BEGIN
   IF  SystemCommand ("ls -1 tri.mod | ") = TRUE  THEN
      InOut.WriteString ("No error reported.")
   ELSE
      InOut.WriteString ("Error reported!")
   END;
   LOOP
      InOut.Read (ch);
      InOut.Write (ch);
      IF  ch < ' '  THEN  EXIT  END
   END;
   InOut.WriteLn;
   InOut.WriteBf
END tri.
```


=={{header|Modula-3}}==
This code requires the <code>UNSAFE</code> keyword because <code>M3toC</code> deals with C strings (which are pointers), and are implemented in Modula-3 as <code>UNTRACED</code>, meaning they are not garbage collected, which is why the code calls <code>FreeCopiedS()</code>.

Also note the <code>EVAL</code> keyword, which ignores the return value of a function.

```modula3
UNSAFE MODULE Exec EXPORTS Main;

IMPORT Unix, M3toC;

VAR command := M3toC.CopyTtoS("ls");

BEGIN
  EVAL Unix.system(command);
  M3toC.FreeCopiedS(command);
END Exec.
```


## MUMPS

<p>ANSI MUMPS doesn't allow access to the operating system except possibly through the View command and $View function, both of which are implementation specific. Intersystems' Caché does allow you to create processes with the $ZF function, and if the permissions for the Caché process allow it you can perform operating system commands.</p>
<p>In Caché on OpenVMS in an FILES-11 filesystem ODS-5 mode this could work:

```MUMPS
Set X=$ZF(-1,"DIR")
```
</p>

<p>In GT.M on OpenVMS, the following will work:

```MUMPS
ZSY "DIR"
```
</p>
<p>GT.M on UNIX is the same:

```MUMPS
ZSY "ls"
```
</p>
<p>Note: $ZF in GT.M is Unicode version of $F[ind].</p>


## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.util.Scanner

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg command
  if command = '' then command = 'ls -oa' -- for Windows change to: 'cmd /C dir'
  do
    say 'Executing command:' command
    jprocess = Runtime.getRunTime().exec(command)
    jscanner = Scanner(jprocess.getInputStream())
    loop label scanning while jscanner.hasNext()
      say jscanner.nextLine()
      end scanning
  catch ex = IOException
    ex.printStackTrace()
  end
  return

```



## NewLISP


```NewLISP
(exec "ls")
```



## Nim


```nim
import osproc

let exitCode = execCmd "ls"
let (output, exitCode2) = execCmdEx "ls"
```


=={{header|Objective-C}}==
NSTask runs an external process with explicit path and arguments.

```objc
void runls()
{
    [[NSTask launchedTaskWithLaunchPath:@"/bin/ls"
        arguments:@[]] waitUntilExit];
}
```

If you need to run a system command, invoke the shell:

```objc
void runSystemCommand(NSString *cmd)
{
    [[NSTask launchedTaskWithLaunchPath:@"/bin/sh"
        arguments:@[@"-c", cmd]]
        waitUntilExit];
}
```

Complete usage example:

<!-- {{libheader|Cocoa}} -->
```objc>#import <Foundation/Foundation.h


void runSystemCommand(NSString *cmd)
{
    [[NSTask launchedTaskWithLaunchPath:@"/bin/sh"
        arguments:@[@"-c", cmd]]
        waitUntilExit];
}

int main(int argc, const char **argv)
{
    @autoreleasepool {

      runSystemCommand(@"ls");
    }
    return 0;
}
```

Or use the C method above.


## OCaml

Just run the command:


```ocaml
Sys.command "ls"
```


To capture the output of the command:


```ocaml
#load "unix.cma"

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

let listing = syscall "ls" ;;
```



a more complete version which also returns the contents from stderr, and checks the exit-status, and where the environment can be specified:


```ocaml
let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED r -> Printf.eprintf "warning: the process terminated with exit code (%d)\n%!" r
  | Unix.WSIGNALED n -> Printf.eprintf "warning: the process was killed by a signal (number: %d)\n%!" n
  | Unix.WSTOPPED n -> Printf.eprintf "warning: the process was stopped by a signal (number: %d)\n%!" n
;;

let syscall ?(env=[| |]) cmd =
  let ic, oc, ec = Unix.open_process_full cmd env in
  let buf1 = Buffer.create 96
  and buf2 = Buffer.create 48 in
  (try
     while true do Buffer.add_channel buf1 ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel buf2 ec 1 done
   with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  check_exit_status exit_status;
  (Buffer.contents buf1,
   Buffer.contents buf2)
```


 val syscall : ?env:string array -> string -> string * string


## Octave


```octave
system("ls");
```



## Oforth



```Oforth
System cmd("pause")
```



## Oz


```oz
{OS.system "ls" _}
```


A more sophisticated example can be found [http://www.mozart-oz.org/home/doc/op/node17.html here].


## PARI/GP


```parigp
system("ls")
```



## Pascal

 {{works with|Free_Pascal}} {{libheader|SysUtils}}

```pascal
Program ExecuteSystemCommand;

uses
  SysUtils;
begin
  ExecuteProcess('/bin/ls', '-alh');
end.
```



## Perl


```perl
my @results = qx(ls);  # run command and return STDOUT as a string

my @results = `ls`;    # same, alternative syntax

system "ls";           # run command and return exit status; STDOUT of command goes program STDOUT

print `ls`;            # same, but with back quotes

exec "ls";             # replace current process with another
```


See also:
http://perldoc.perl.org/perlipc.html#Using-open()-for-IPC
http://perldoc.perl.org/IPC/Open3.html


## Perl 6


```perl6
run "ls" orelse .die; # output to stdout

my @ls = qx/ls/;    # output to variable

my $cmd = 'ls';
@ls = qqx/$cmd/;  # same thing with interpolation
```


=={{header|PDP-11 Assembly}}==
PDP-11 running Unix
<lang pdp-11>; Execute a file - the equivalent of system() in stdio
;
; On entry, r1=>nul-terminated command string
; On exit,  VS=Couldn't fork
;           VC=Forked successfully, r0=return value
;
.CLIsystem
trap 2			; fork()
br   CLIchild		; Child process returns here
bcc  CLIparent		; Parent process returns here
mov  (sp)+,r1
tst  (sp)+
sev			; Couldn't fork, set V
rts  pc
.CLIparent
mov  r0,-(sp)		; Save child's PID
.CLIwait
trap 7			; wait()
cmp  r0,(sp)
beq  CLIfinished
cmp  r0,#&FFFF
bne  CLIwait		; Loop until child finished
.CLIfinished
tst  (sp)+		; Drop child's PID
mov  r1,r0		; R0=return value
mov  (sp)+,r1		; Restore R1
tst  (sp)+		; Drop original R0
swab r0			; Move return value to bottom byte
rts  pc

; CLI child process
; -----------------
.CLIchild
clr  -(sp)			; end of string array
mov  r1,-(sp)			; => command string
mov  #UXsh3,-(sp)		; => "-c"
mov  #UXsh2,-(sp)		; => "sh"
mov  #&890B,TRAP_BUF		; exec
mov  #UXsh1,TRAP_BUF+2		; => "/bin/sh"
mov  sp,TRAP_BUF+4		; => pointers to command strings
;mov  SV_ENVPTR,TRAP_BUF+6	; => "PATH=etc"
trap 0				; indir()
EQUW TRAP_BUF			; exec(shell, parameters)
add  #8,sp			; If we get back, we didn't fork, we spawned
mov  (sp)+,r1			; So, restore registers
clr  (sp)+			; and return exit value in R0
rts  pc

.UXsh1	EQUS "/bin/sh",0
.UXsh2	EQUS "sh",0
.UXsh3	EQUS "-c",0
ALIGN

.TRAP_BUF
EQUW 0
EQUW 0
EQUW 0
EQUW 0
```

So, call with, for example:
<lang pdp-11>mov  #cmd_ls,r1		; => "ls" command string
jsr  pc,CLIsystem
...
.cmd_ls	EQUS "ls",0
```



## Phix


```Phix
string cmd = iff(platform()=WINDOWS?"dir":"ls")
system(cmd)
integer res = system_exec("pause",4)
```

system_exec allows you to specify whether you want a command shell or not, and whether to wait for a result. In the case of pause, the 4 signifies that we need a shell and we want to wait for it to complete.


## PHP

The first line execute the command and the second line display the output:

```php
@exec($command,$output);
echo nl2br($output);
```

'''Note:'''The '@' is here to prevent error messages to be displayed, 'nl2br' translate '\n' chars to 'br' in HTML.

Other:

```php
$results = `ls`;
# runs command and returns its STDOUT as a string

system("ls");
# runs command and returns its exit status; its STDOUT gets output to our STDOUT

echo `ls`;
# the same, but with back quotes

passthru("ls");
# like system() but binary-safe
```


See also: [http://us.php.net/manual/en/function.proc-open.php proc_open()]


## PicoLisp


```PicoLisp
(call "ls")
```



## Pike


```pike
int main(){
   // Process.run was added in Pike 7.8 as a wrapper to simplify the use of Process.create_process()
   mapping response = Process.run("ls -l");
   // response is now a map containing 3 fields
   // stderr, stdout, and exitcode. We want stdout.
   write(response["stdout"] + "\n");

   // with older versions of pike it's a bit more complicated:
   Stdio.File stdout = Stdio.File();
   Process.create_process(({"ls", "-l"}), ([ "stdout" : stdout->pipe() ]) );
   write(stdout->read() + "\n");
}
```



## Pop11

The sysobey function runs commands using a shell:


```pop11
sysobey('ls');
```



## PowerShell

Since PowerShell is a shell, running commands is the default operation.

```powershell
dir
ls
Get-ChildItem
```

are all equivalent (the first two are aliases for the third) but they are PowerShell-native commands. If one really needs to execute <code>dir</code> (which is no program but rather a built-in command in <code>cmd.exe</code>) this can be achieved by

```powershell>cmd /c dir</lang



## Prolog

```prolog
shell('ls').
```


## PureBasic


```PureBasic
ImportC "msvcrt.lib"
  system(str.p-ascii)
EndImport

If OpenConsole()
  system("dir & pause")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```python
import os
exit_code = os.system('ls')       # Just execute the command, return a success/fail code
output    = os.popen('ls').read() # If you want to get the output data. Deprecated.
```

or

```python
import subprocess
# if the exit code was non-zero these commands raise a CalledProcessError
exit_code = subprocess.check_call(['ls', '-l'])   # Python 2.5+
assert exit_code == 0
output    = subprocess.check_output(['ls', '-l']) # Python 2.7+
```


or

```python
from subprocess import PIPE, Popen, STDOUT
p = Popen('ls', stdout=PIPE, stderr=STDOUT)
print p.communicate()[0]
```


'''Note:''' The latter is the preferred method for calling external processes, although cumbersome, it gives you finer control over the process.

or

```python
import commands
stat, out = commands.getstatusoutput('ls')
if not stat:
    print out
```



## R


```R
system("ls")
output=system("ls",intern=TRUE)
```



## Racket


```Racket

#lang racket

;; simple execution of a shell command
(system "ls")

;; capture output
(string-split (with-output-to-string (λ() (system "ls"))) "\n")

;; Warning: passing random string to be run in a shell is a bad idea!
;; much safer: avoids shell parsing, arguments passed separately
(system* "/bin/ls" "-l")

;; avoid specifying the executable path
(system* (find-executable-path "/bin/ls") "-l")

```



## Raven

Back tick string is auto executed:


```raven
`ls -la` as listing
```


Or specifically on any string:


```raven
'ls -la' shell as listing
```



## REBOL


```REBOL
; Capture output to string variable:

x: ""  call/output "dir" x
print x

; The 'console' refinement displays the command output on the REBOL command line.

call/console "dir *.r"
call/console "ls *.r"

call/console "pause"

; The 'shell' refinement may be necessary to launch some programs.

call/shell "notepad.exe"
```



## Red


```red

call/show %pause        ;The /show refinement forces the display of system's shell window (Windows only).
call/show %dir
call/show %notepad.exe
```



## REXX

Since REXX is a shell scripting language, it's easy to execute commands:

```REXX
"dir /a:d"
```



## Ring


```ring

system("dir")

```



## Ruby


```ruby
string = `ls`
# runs command and returns its STDOUT as a string
string = %x{ls}
# ditto, alternative syntax

system "ls"
# runs command and returns its exit status; its STDOUT gets output to our STDOUT

print `ls`
#The same, but with back quotes

exec "ls"
# replace current process with another

# call system command and read output asynchronously
io = IO.popen('ls')
# ... later
io.each {|line| puts line}
```



## Run BASIC


```runbasic
print shell$("ls")  ' prints the returned data from the OS
a$ =  shell$("ls")  ' holds returned data in a$
```



## Rust


```rust
use std::process::Command;
fn main() {
    let output = Command::new("ls").output().unwrap_or_else(|e| {
        panic!("failed to execute process: {}", e)
    });
    println!("{}", String::from_utf8_lossy(&output.stdout));
}

```



## Scala


```scala
import scala.sys.process.Process
Process("ls", Seq("-oa"))!
```



## Scheme

```scheme
(system "ls")
```



## Seed7

System commands can make a program unportable.
Unix, Linux and BSD use the command ''ls'', while Windows respectively DOS use the command ''dir''.
The format written by ''ls'' respectively ''dir'' depends on operating system and locale.
The library [http://seed7.sourceforge.net/libraries/osfiles.htm osfiles.s7i] defines
the function [http://seed7.sourceforge.net/libraries/osfiles.htm#readDir%28in_string%29 readDir],
which reads the contents of a directory in a portable way. ''ReadDir'' works independend
from operating system and locale and supports also Unicode filenames.
Anyway, the task was to use a system command, so here is the example:


```seed7
$ include "seed7_05.s7i";
  include "shell.s7i";

const proc: main is func
  begin
    cmd_sh("ls");
  end func;
```



## SETL


```SETL
system("ls");
```



## Sidef


```ruby
# Pipe in read-only mode
%p(ls).open_r.each { |line|
    print line;
};

var str1 = `ls`;         # backtick: returns a string
var str2 = %x(ls);       # ditto, alternative syntax

Sys.system('ls');   # system: executes a command and prints the result
Sys.exec('ls');     # replaces current process with another
```



## Slate


Run a command normally through the shell:


```slate
Platform run: 'ls'.
```


Run a command (this way takes advantage of the 'does not understand' message for the shell object and calls the Platform run: command above with a specific command):


```slate
shell ls: '*.slate'.
```



## Smalltalk



```smalltalk
Smalltalk system: 'ls'.
```



## Standard ML

Just run the command:


```sml
OS.Process.system "ls"
```



## SQL PL

In Linux or UNIX:

```sql pl

!ls

```

Output:

```txt

db2 => !ls
adm	  ctrlhamirror	    fm.ip-10-0-0-85.reg  lib64	    profile.env  security64
adsm	  dasfcn	    function		 log	    python32	 spmlog
backup	  db2cshrc	    gskit		 map	    python64	 sqldbdir
bin	  db2dump	    hmonCache		 misc	    rdf		 tmp
bnd	  db2nodes.cfg	    include		 msg	    Readme	 tools
cfg	  db2profile	    infopop		 nodes	    ruby32	 uif
cfgcache  db2systm	    java		 nodes.reg  ruby64	 usercshrc
conv	  doc		    json		 pd	    samples	 userprofile
ctrl	  fm.db2-1.reg	    lib			 php32	    security
ctrlha	  fm.db2-model.reg  lib32		 php64	    security32

```

In Windows:

```sql pl

!dir

```



## Stata

Stata has a built-in '''[http://www.stata.com/help.cgi?dir dir]''' command. However, it's also possible to run arbitrary external programs using the '''[http://www.stata.com/help.cgi?shell shell]''' or '''winexec''' commands.

The command '''!''' (or equivalently '''shell'''), opens a Windows console to run the command, while '''winexec''' does not.


```stata
!dir

* print a message and wait
!echo Ars Longa Vita Brevis & pause

* load Excel from Stata
!start excel

* run a Python program (Python must be installed and accessible in the PATH environment variable)
!python preprocessing.py

* load Windows Notepad
winexec notepad
```



## Tcl



```tcl
puts [exec ls]
```


This page uses "ls" as the primary example. For what it's worth, Tcl has built-in primitives for retrieving lists of files so one would rarely ever directly exec an ls command.

It is also possible to execute a system command by "open"ing it through a pipe from whence any output of the command can be read at any (later) time. For example:


```tcl
set io [open "|ls" r]
```


would execute "ls" and pipe the result into the channel whose name is put in the "io" variable. From there one could receive it either line by line like this:


```tcl
set nextline [gets $io]
```


or read the whole shebang in a fell swoop:


```tcl
set lsoutput [read $io]
```


If the command is opened "rw", it is even possible to send it user input through the same handle, though care must be taken with buffering in that case.


## Toka


```toka
needs shell
" ls" system
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
system=SYSTEM ()
IF (system=="WIN") THEN
EXECUTE "dir"
ELSEIF (system.sw."LIN") THEN
EXECUTE "ls -l"
ENDIF

```



## UNIX Shell

UNIX shells are designed to run system commands as a default operation.

```bash>ls</lang


If one wishes to replace the shell process with some other command (chain into some command with no return) one can use the '''''exec''''' shell built-in command.


```bash>exec ls</lang



### Command substitution

One can also capture the command's standard output in a variable.

With [[Bourne Shell]]:

```bash
output=`ls`
```


With [[Korn Shell]] or any modern shell:

```bash
output=$(ls)
```


* '''Note 1:''' in <code>`ls`</code>, these are "backticks" rather than quotes or apostrophes.
* '''Note 2:''' the '''$(...)''' form works in all modern shells, including the [[Almquist Shell]], [[Bash]] and any POSIX shell.
* The old `backticks` can also be used in the newer shells, but their users prefer the '''$(...)''' form when discussing such things in e-mail, on USENET, or in other online forums (such as this wiki). The only reason to use `backticks` is in scripts for old Bourne Shell.

The '''`...`''' form is difficult to nest, but the '''$(...)''' form is very nestable.


```bash
output=`expr \`echo hi | wc -c\` - 1`
output=$(expr $(echo hi | wc -c) - 1)
```


Both forms, `backticks` and '''$(...)''', also work inside double-quoted strings. This prevents file name expansion and also prevents word splitting.


```bash
echo "Found: `grep 80/tcp /etc/services`"
echo "Found: $(grep 80/tcp /etc/services)"
```


=
## C Shell
=
C Shell also runs system commands, and has an '''exec''' built-in command, exactly like Bourne Shell.


```csh
ls         # run command, return to shell
exec ls    # replace shell with command
```


`Backticks` are slightly different. When inside double quotes, as '''"`...`"''', C Shell splits words at newlines, like '''"line 1" "line 2" ...''', but preserves spaces and tabs.


```csh
set output=( "`grep 80/ /etc/services`" )
echo "Line 1: $output[1]"
echo "Line 2: $output[2]"
```



## Ursa


```ursa>decl string<
 arg
decl string<> output
decl iodevice iod

append "ls" arg
set iod (ursa.util.process.start arg)
set output (iod.readlines)

for (decl int i) (< i (size output)) (inc i)
        out output<i> endl console
end for
```



## Ursala

The library function, ask, parameterized by a shell descriptor, such as bash,
spawns a process that interacts with that shell by feeding it a list of
commands, and returns a transcript of the interaction.

Note that the output from the spawned process is captured and returned only,
not sent to the standard output stream of the parent.

Here is a self-contained command line application providing a limited replacement
for the ls command.

```Ursala
#import std
#import cli

#executable ('parameterized','')

myls = <.file$[contents: --<''>]>@hm+ (ask bash)/0+ -[ls --color=no]-!
```

The color option is needed to suppress terminal escape sequences.


## VBScript


```vb

Set objShell = CreateObject("WScript.Shell")
objShell.Run "%comspec% /K dir",3,True

```



## Vedit macro language



```vedit
system("dir", DOS)
```


The above does not work on 64-bit Windows versions which do not have 16-bit DOS emulation.
In this case, you need to call cmd.exe explicitly:


```vedit
system('cmd /k "dir"')
```



## Visual Basic

Shelling out a sub task in Visual Basic is rather a pain if you need to wait for the task to complete, which
is probably the usual case.  But it is possible.

```vb
Attribute VB_Name = "mdlShellAndWait"
Option Explicit

Private Declare Function OpenProcess Lib "kernel32" _
    (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, _
    ByVal dwProcessId As Long) As Long

Private Declare Function GetExitCodeProcess Lib "kernel32" _
    (ByVal hProcess As Long, lpExitCode As Long) As Long

Private Const STATUS_PENDING = &H103&
Private Const PROCESS_QUERY_INFORMATION = &H400

'
' Little function go get exit code given processId
'
Function ProcessIsRunning( processId as Long ) as Boolean
    Dim exitCode as Long
    Call GetExitCodeProcess(lProcessId, exitCode)
    ProcessIsRunning = (exitCode = STATUS_PENDING)
End Function

' Spawn subprocess and wait for it to complete.
'   I believe that the command in the command line must be an exe or a bat file.
'   Maybe, however, it can reference any file the system knows how to "Open"
'
' commandLine is an executable.
' expectedDuration - is for poping up a dialog for whatever
' infoText - text for progressDialog dialog

Public Function ShellAndWait( commandLine As String, _
    expectedDuration As Integer ) As Boolean

    Dim inst As Long
    Dim startTime As Long
    Dim expirationTime As Long
    Dim pid As Long
    Dim expiresSameDay As Boolean

    On Error GoTo HandleError

    'Deal with timeout being reset at Midnight ($hitForBrains VB folks)
    startTime = CLng(Timer)
    expirationTime = startTime + expectedDuration
    expiresSameDay = expirationTime < 86400
    If Not expiresSameDay Then
        expirationTime = expirationTime - 86400
    End If

    inst = Shell(commandLine, vbMinimizedNoFocus)

    If inst <> 0 Then
        pid = OpenProcess(PROCESS_QUERY_INFORMATION, False, inst)

        Do While ProcessIsRunning( pid)
            DoEvents
            If Timer > expirationTime And (expiresSameDay Or Timer < startTime) Then
                Exit Do
            End If
        Loop
        ShellAndWait = True
    Else
        MsgBox ("Couldn't execute command: " & commandLine)
        ShellAndWait = False
    End If

    Exit Function

HandleError:
    MsgBox ("Couldn't execute command: " & commandLine)
    ShellAndWait = False
End Function

Sub SpawnDir()
   ShellAndWait("dir", 10)
End Sub
```



## Visual Basic .NET

```vbnet
Module System_Command

    Sub Main()
        Dim cmd As New Process
        cmd.StartInfo.FileName = "cmd.exe"
        cmd.StartInfo.RedirectStandardInput = True
        cmd.StartInfo.RedirectStandardOutput = True
        cmd.StartInfo.CreateNoWindow = True
        cmd.StartInfo.UseShellExecute = False

        cmd.Start()

        cmd.StandardInput.WriteLine("dir")
        cmd.StandardInput.Flush()
        cmd.StandardInput.Close()

        Console.WriteLine(cmd.StandardOutput.ReadToEnd)
    End Sub

End Module

```


## Wart


```wart
system "ls"
```



## zkl


```zkl
System.cmd(System.isWindows and "dir" or "ls")
```



## ZX Spectrum Basic


The ZX Spectrum uses a ROM based basic interpreter, so every statement within the program is a system command. If a command without a line number is typed, whilst the computer is in a ready state, the command gets executed immediately:


```zxbasic>PAUSE 100</lang


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have an external OS/command processor. -->
