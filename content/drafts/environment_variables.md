+++
title = "Environment variables"
description = ""
date = 2019-08-25T20:05:36Z
aliases = []
[extra]
id = 3271
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}} 
[[Category:Environment variables]] 
[[Category:Initialization]] 
[[Category:Simple]]
{{omit from|M4}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have an environment other than regular global variables. -->
{{omit from|Unlambda|Does not provide access to environment variables.}}

;Task:
Show how to get one of your process's [[wp:Environment variable|environment variables]]. 

The available variables vary by system;   some of the common ones available on Unix include:
:::*   PATH
:::*   HOME
:::*   USER





## 11l


```11l
print(os:getenv(‘HOME’))
```



## Ada

Print a single environment variable.

```ada
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_Io; use Ada.Text_Io;

procedure Print_Path is
begin
   Put_Line("Path : " & Value("PATH"));
end Print_Path;
```

Print all environment variable names and values.

```ada
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_Io; use Ada.Text_Io;

procedure Env_Vars is
   procedure Print_Vars(Name, Value : in String) is
   begin
      Put_Line(Name & " : " & Value);
   end Print_Vars;
begin
   Iterate(Print_Vars'access);
end Env_Vars;
```




###  Alternative version using Matreshka 


Uses [http://forge.ada-ru.org/matreshka Matreshka].


```Ada
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;

procedure Main is

   function "+"
    (Item : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

begin
   Ada.Wide_Wide_Text_IO.Put_Line
    (League.Application.Environment.Value (+"HOME").To_Wide_Wide_String);
end Main;
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386 - ''getenv'' is not part of the standard's prelude}}

```algol68
print((getenv("HOME"), new line))
```



## AutoHotkey


```autohotkey
EnvGet, OutputVar, Path
MsgBox, %OutputVar%
```


== {{header|AutoIt}} ==

```AutoIt
ConsoleWrite("# Environment:" & @CRLF)

Local $sEnvVar = EnvGet("LANG")
ConsoleWrite("LANG : " & $sEnvVar & @CRLF)

ShowEnv("SystemDrive")
ShowEnv("USERNAME")

Func ShowEnv($N)
    ConsoleWrite( StringFormat("%-12s : %s\n", $N, EnvGet($N)) )
EndFunc   ;==>ShowEnv
```


{{Out}}

```txt
# Environment:
LANG : DE
SystemDrive  : C:
USERNAME     : HaJo
```



## AWK

The ENVIRON array contains the values of the current environment:

```awk
$ awk 'BEGIN{print "HOME:"ENVIRON["HOME"],"USER:"ENVIRON["USER"]}' 
```

{{out}}

```txt

HOME:/home/suchrich USER:SuchRich
```


Environment variables can also be assigned to awk variables before execution, with (-v) options:

```awk
$ awk -v h=$HOME -v u=$USER 'BEGIN{print "HOME:"h,"USER:"u}' 
```

{{out}}

```txt
HOME:/home/suchrich USER:SuchRich
```


Listing all the environment variables:

```awk
# http://ideone.com/St5SHF
BEGIN { print "# Environment:"
        for (e in ENVIRON) { printf( "%10s = %s\n", e, ENVIRON[e] ) }
}
END   { print "# Done." } 
```

{{out}}

```txt

# Environment:
   AWKPATH = .:/usr/share/awk
AWKLIBPATH = /usr/lib/i386-linux-gnu/gawk
      LANG = en_US.UTF-8
      PATH = /usr/local/bin:/usr/bin:/bin
      HOME = /home/guest
       PWD = /home/guest
     SHLVL = 0
    TMPDIR = /home/guest
# Done.


```



## BASIC


```qbasic
x$ = ENVIRON$("path")
PRINT x$
```


=
## BaCon
=
''Case matters and needs to match''

```freebasic
PRINT GETENVIRON$("PATH")
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 ASK BORDER VAR01
110 ASK DEFAULT CHANNEL VAR02
120 ASK EDITOR BUFFER VAR03
130 ASK EDITOR KEY VAR04
140 ASK EDITOR VIDEO VAR05
150 ASK FAST SAVE VAR06
160 ASK INTERRUPT KEY VAR07
170 ASK INTERRUPT NET VAR08
180 ASK INTERRUPT STOP VAR09
190 ASK KEY CLICK VAR10
200 ASK KEY DELAY VAR11
210 ASK KEY RATE VAR12
220 ASK NET CHANNEL VAR13
230 ASK NET MACHINE VAR14
240 ASK REM1 VAR15
250 ASK REM2 VAR16
260 ASK SERIAL BAUD VAR17
270 ASK SERIAL FORMAT VAR18
280 ASK STATUS VAR19
290 ASK SOUND BUFFER VAR20
300 ASK SPEAKER VAR21
310 ASK TAPE LEVEL VAR22
320 ASK TAPE SOUND VAR23
330 ASK TIMER VAR24
340 ASK VIDEO COLOR VAR25
350 ASK VIDEO MODE VAR26
360 ASK VIDEO X VAR27
370 ASK VIDEO Y VAR28
```


or

<lang IS-BASIC>ASK machine-option-code var
```


=== {{header|ZX Spectrum Basic}} ===
The ZX Spectrum does not use environmental variables in a traditional sense. However, it does provide a set of system variables held at a fixed memory address:

```zxbasic
10 PRINT "The border colour is "; PEEK (23624): REM bordcr
20 PRINT "The ramtop address is "; PEEK (23730) + 256 * PEEK (23731): REM ramtop
30 POKE 23609,50: REM set keyboard pip to 50
```



## Batch File

Batch files don't have any other kind of variables except environment variables. They can be accessed by enclosing the variable name in percent signs:

```dos
echo %Foo%
```

For interactive use one can use <code>set</code> to view all environment variables or all variables starting with a certain string:

```dos
set
set Foo
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT FNenvironment("PATH")
      PRINT FNenvironment("USERNAME")
      END
      
      DEF FNenvironment(envar$)
      LOCAL buffer%, size%
      SYS "GetEnvironmentVariable", envar$, 0, 0 TO size%
      DIM buffer% LOCAL size%
      SYS "GetEnvironmentVariable", envar$, buffer%, size%+1
      = $$buffer%
```



## C


```c>#include <stdlib.h

#include <stdio.h>

int main() {
  puts(getenv("HOME"));
  return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;

namespace RosettaCode {
    class Program {
        static void Main() {
            string temp = Environment.GetEnvironmentVariable("TEMP");
            Console.WriteLine("TEMP is " + temp);
        }
    }
}
```



## C++


```cpp>#include <cstdlib

#include <cstdio>

int main()
{
   puts(getenv("HOME"));
   return 0;
}
```



## Clojure


```lisp
(System/getenv "HOME")
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Environment-Vars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  home PIC X(75).

       PROCEDURE DIVISION.
*          *> Method 1.      
           ACCEPT home FROM ENVIRONMENT "HOME"
           DISPLAY home

*          *> Method 2.
           DISPLAY "HOME" UPON ENVIRONMENT-NAME
           ACCEPT home FROM ENVIRONMENT-VALUE

           GOBACK
           .
```



## CoffeeScript

{{works with|node.js}}

```coffeescript
for var_name in ['PATH', 'HOME', 'LANG', 'USER']
  console.log var_name, process.env[var_name]
```



## Common Lisp

Access to environment variables isn't a part of the Common Lisp standard, but most implementations provide some way to do it.
{{works with|LispWorks}}

```lisp
(lispworks:environment-variable "USER")
```

{{works with|SBCL}}

```lisp
(sb-ext:posix-getenv "USER")
```

{{works with|Clozure CL}}

```lisp
(ccl:getenv "USER")
```

{{works with|CLISP}}

```lisp
(getenv "HOME")
```

Ways to do this in some other implementations are listed in the [http://cl-cookbook.sourceforge.net/os.html#env Common Lisp Cookbook].


## D

{{libheader|phobos}}

```d
import std.stdio, std.process;

void main() {
    auto home = getenv("HOME");
}
```

{{libheader|tango}}

```d
import tango.sys.Environment;

void main() {
    auto home = Environment("HOME");
}
```


=={{header|Delphi}}/{{header|Pascal}}==

```Delphi
program EnvironmentVariable;

{$APPTYPE CONSOLE}

uses SysUtils;

begin
  WriteLn('Temp = ' + GetEnvironmentVariable('TEMP'));
end.
```



## E

{{works with|E-on-Java}}

```e><unsafe:java.lang.System
.getenv("HOME")
```



## Eiffel

The feature <code lang="eiffel">get</code> returns the value of an environment variable. <code lang="eiffel">get</code> is defined in the library class EXECUTION_ENVIRONMENT. So the class APPLICATION inherits from EXECUTION_ENVIRONMENT in order to make <code lang="eiffel">get</code> available. 

```eiffel 
class
    APPLICATION
inherit
    EXECUTION_ENVIRONMENT
create
    make
feature {NONE} -- Initialization
    make
            -- Retrieve and print value for environment variable `USERNAME'.
        do
            print (get ("USERNAME"))
        end
end
```



## Elixir


```elixir
System.get_env("PATH")
```



## Emacs Lisp


```lisp
(getenv "HOME")
```



## Erlang


```Erlang

os:getenv( "HOME" ).

```



## Euphoria


```euphoria
puts(1,getenv("PATH"))
```


=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main args =
    printfn "%A" (Environment.GetEnvironmentVariable("PATH"))
    0
```



## Factor


```factor
"HOME" os-env print
```



## Forth

{{works with|GNU Forth}}

```forth
s" HOME" getenv type
```



## Fortran

{{works with|any Fortran compiler}}

```fortran
program show_home
implicit none
character(len=32) :: home_val  ! The string value of the variable HOME
integer           :: home_len  ! The actual length of the value
integer           :: stat      ! The status of the value:
                               !  0 = ok
                               !  1 = variable does not exist
                               ! -1 = variable is not long enought to hold the result
call get_environment_variable('HOME', home_val, home_len, stat)
if (stat == 0) then
    write(*,'(a)') 'HOME = '//trim(home_val)
else
    write(*,'(a)') 'No HOME to go to!'
end if
end program show_home
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Var v = Environ("SystemRoot")
Print v
Sleep
```


{{out}}

```txt

C:\WINDOWS

```



## FunL


```funl
println( System.getenv('PATH') )
println( $home )
println( $user )
```



## Go

;Simply:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println(os.Getenv("SHELL"))
}
```

{{out}}

```txt

/bin/bash

```

;Alternatively:
Library function os.Environ returns all environment variables.  
You're on your own then to parse out the one you want.  
Example:

```go
package main

import (
    "fmt"
    "os"
    "strings"
)

func main() {
    s := "SHELL"
    se := s + "="
    for _, v := range os.Environ() {
        if strings.HasPrefix(v, se) {
            fmt.Println(s, "has value", v[len(se):])
            return
        }
    }
    fmt.Println(s, "not found")
}
```

{{out}}

```txt

SHELL has value /bin/bash

```



## Gri

Command <code>get env</code> fetches an environment variable into a synonym (a string)

```Gri
get env \foo HOME
show "\foo"
```


Quotes can be used in the usual way if the environment variable name contains spaces (which is unusual, but possible).

```Gri
get env \foo "X Y Z"
```



## Groovy


```groovy
System.getenv().each { property, value -> println "$property = $value"}
```



## Haskell


```haskell
import System.Environment
main = do getEnv "HOME" >>= print  -- get env var
          getEnvironment >>= print -- get the entire environment as a list of (key, value) pairs
```



## hexiscript


```hexiscript
println env "HOME"
println env "PATH"
println env "USER"
```



## HicEst


```HicEst
CHARACTER string*255

string = "PATH="
SYSTEM(GEteNV = string)
```



## i


```i
software {
	print(load("$HOME"))
	print(load("$USER"))
	print(load("$PATH"))
}
```


=={{header|Icon}} and {{header|Unicon}}==
{{works with|Unicon}}

```Icon
procedure main(arglist)

if *envars = 0 then envars := ["HOME", "TRACE", "BLKSIZE","STRSIZE","COEXPSIZE","MSTKSIZE", "IPATH","LPATH","NOERRBUF"] 

every v := !sort(envars) do 
   write(v," = ",image(getenv(v))|"* not set *")
end
```



## J


```j
2!:5'HOME'
```



## Java


```java
System.getenv("HOME") // get env var
System.getenv()       // get the entire environment as a Map of keys to values
```



## JavaScript

The JavaScript language has no facilities to access the computer: it relies on the host environment to provide it.
{{works with|JScript}}

```javascript
var shell = new ActiveXObject("WScript.Shell");
var env = shell.Environment("PROCESS");
WScript.echo('SYSTEMROOT=' + env.item('SYSTEMROOT'));
```



## Joy


```joy
"HOME" getenv.
```



## jq


```jq>env.HOME</lang
If the environment variable name has spaces or special characters, the name must be given as a string, e.g. <tt>env."HOME"</tt>.


## jsish

The Jsi ''Util'' module provides access to set ''Util.setenv(name, value)'' and get ''Util.getenv(name)'' process environment variables.  ''Util.getenv()'', with no argument will return an object with all available name:value pairs.

```javascript
/* Environment variables, in Jsi */
puts(Util.getenv("HOME"));
var environment = Util.getenv();
puts(environment.PATH);
```



## Julia

{{works with|Julia|0.6}}


```julia
@show ENV["PATH"]
@show ENV["HOME"]
@show ENV["USER"]
```



## K


```K
_getenv "HOME"
```



## Kotlin


```scala
// version 1.0.6

// tested on Windows 10

fun main(args: Array<String>) {
   println(System.getenv("SystemRoot"))
}
```


{{out}}

```txt

C:\WINDOWS

```



## Lasso


```Lasso
#!/usr/bin/lasso9

define getenv(sysvar::string) => {
	local(regexp = regexp(
		-find = `(?m)^` + #sysvar + `=(.*?)$`,
		-input = sys_environ -> join('\n'),
		-ignorecase
	))
	return #regexp ->find ? #regexp -> matchString(1)
}

stdoutnl(getenv('HOME'))
stdoutnl(getenv('PATH'))
stdoutnl(getenv('USER'))
stdoutnl(getenv('WHAT'))
```

{{out}}

```txt
/Users/rosetta
/opt/local/bin:/opt/local/sbin:/usr/local/bin/:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin
rosetta

```



## Liberty BASIC

=== Built-in variables ===

```lb
print StartupDir$
print DefaultDir$
```


###  Other variables 


```lb
print GetEnvironmentVariable$("USERNAME")
    print GetEnvironmentVariable$("USERPROFILE") ' equivalent to UNIX HOME variable
    print GetEnvironmentVariable$("PATH")
    end

function GetEnvironmentVariable$(lpName$)
    'get the value of an environment variable
    nSize = 1024

[Retry]
    lpBuffer$ = space$(nSize)

    calldll #kernel32, "GetEnvironmentVariableA", _
        lpName$   as ptr, _
        lpBuffer$ as ptr, _
        nSize     as ulong, _
        result    as ulong

    select case
        ' buffer too small
        case result > nSize
        nSize = result
        goto [Retry]

        ' variable found
        case result > 0
        GetEnvironmentVariable$ = left$(lpBuffer$, result)
    end select
end function
```



## LIL

LIL does not ship with a command to retrieve process environment variables.  The '''system''' command could be used, but here is an extension in C for the lil shell.


```c

static LILCALLBACK lil_value_t fnc_env(lil_t lil, size_t argc, lil_value_t* argv)
{
    if (!argc) return NULL;
    return lil_alloc_string(getenv(lil_to_string(argv[0])));
}
```


Then inside the main functions for repl and nonint (Interactive, Noninteractive):

```c
lil_register(lil, "env", fnc_env);
```


Now lil can get at the environment.  That could fairly easily be extended further to return the entire environment array if no arguments are passed to '''env''', this just returns an empty result for that case.  Defaults values could also be supported if the named environment variable is empty.  Etcetera.  Setting variables would be similar, a few lines of lil C to wrap a call to libc setenv in a new command, and registering the command.

{{out}}

```txt
prompt$ make
cc -c -g3 -std=c99 -pedantic -Wall -Wextra -Wno-format -Wno-long-long -Wno-unused-parameter  main.c -o main.o
cc -g -L.  -o lil main.o -llil -lm

prompt$ lil
Little Interpreted Language Interactive Shell
# env TERM
xterm-256color
```



## Lingo

{{libheader|Shell Xtra}}

```lingo
sx = xtra("Shell").new()
if the platform contains "win" then
  path = sx.shell_cmd("echo %PATH%").line[1]
else
  path = sx.shell_cmd("echo $PATH").line[1]
end if
```



## LSL

Rez a box on the ground, and add the following as a New Script.

```LSL
default {
	state_entry() {
		llOwnerSay("llGetTimestamp()="+(string)llGetTimestamp());
		llOwnerSay("llGetEnergy()="+(string)llGetEnergy());
		llOwnerSay("llGetFreeMemory()="+(string)llGetFreeMemory());
		llOwnerSay("llGetMemoryLimit()="+(string)llGetMemoryLimit());
	}
}
```

{{out}}

```txt
llGetTimestamp()=2012-07-18T01:26:12.133137Z
llGetEnergy()=1.230000
llGetFreeMemory()=16000
llGetMemoryLimit()=65536
```



## Logtalk

Using the standard library:

```logtalk
os::environment_variable('PATH', Path).
```



## Lua


```lua
print( os.getenv( "PATH" ) )
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      \\ using read only variablles
      Print "Platform: ";Platform$
      Print "Computer Os: "; Os$
      Print "Type of OS: ";OsBit;" bit"
      Print "Computer Name:";  Computer$
      Print "User Name: "; User.Name$
      \\ using WScript.Shell
      Declare objShell "WScript.Shell"
      With  objShell, "Environment" set env ("Process")
      With env, "item" as Env$()
      Print Env$("PATH")
      Print Env$("HOMEPATH")
      Declare objShell Nothing
      \\ using internal Information object
      Declare OsInfo INFORMATION
      With OsInfo, "build" as build, "NtDllVersion" as NtDllVersion$
      Method OsInfo, "GetCurrentProcessSID" as PID$
      Method OsInfo, "IsProcessElevated" as isElevated
      Print "Os build number: ";build
      Print "Nr Dll version: ";NtDllVersion$
      Print "ProcessSID: ";pid$
      Print "Is Process Eleveted: ";isElevated
      Declare OsInfo Nothing
}
Checkit

```



## Make

Make variables are initialized from the environment, so simply

```Make
TARGET = $(HOME)/some/thing.txt
foo:
	echo $(TARGET)
```


The shell code in a rule can use the shell's environment in the usual way ([[Environment variables#Unix Shell|Unix Shell]]), but remember <code>$</code> must be doubled <code>$$</code> to get a literal <code>$</code> in that code.

```Make
bar:
        echo "$$HOME"
```


If you mistakenly write just <code>$HOME</code> then it means the makefile <code>$H</code> followed by characters <code>OME</code>.


```Make
H = oops ...
bar:
	echo $HOME

# prints oops ... OME
```



## Maple


```Maple
getenv("PATH");
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Environment["PATH"]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
    getenv('HOME')
    getenv('PATH')
    getenv('USER')
```



## Mercury


```mercury
:- module env_var.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module maybe, string.

main(!IO) :-
    io.get_environment_var("HOME", MaybeValue, !IO),
    (
        MaybeValue = yes(Value),
        io.write_string("HOME is " ++ Value ++ "\n", !IO)
    ;
        MaybeValue = no,
        io.write_string("environment variable HOME not set\n", !IO)
    ).
```



## min

{{works with|min|0.19.3}}

```min
$PATH
```


=={{header|Modula-3}}==

```modula3
MODULE EnvVars EXPORTS Main;

IMPORT IO, Env;

VAR
  k, v: TEXT;

BEGIN
  IO.Put(Env.Get("HOME") & "\n");

  FOR i := 0 TO Env.Count - 1 DO
    Env.GetNth(i, k, v);
    IO.Put(k & " = " & v & "\n")
  END
END EnvVars.
```



## MUMPS

ANSI MUMPS doesn't allow access to the operating system except possibly through the View command and $View function, both of which are implementation specific. Intersystems' Caché does allow you to create processes with the $ZF function, and if the permissions for the Caché process allow it you can perform operating system commands.

In Caché on OpenVMS in an FILES-11 filesystem ODS-5 mode these could work:

```MUMPS
 Set X=$ZF(-1,"show logical")
 Set X=$ZF(-1,"show symbol")
```



## NetRexx

When NetRexx runs under a JVM, system ENVIRONMENT variables are complimented by JVM system properties.  This sample shows how to get both.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sysEnvironment(vn = '') public static
  if vn.length > 0 then do
    envName = vn
    envValu = System.getenv(envName)
    if envValu = null then envValu = ''
    say envName '=' envValu
    end
  else do
    envVars = System.getenv()
    key = String
    loop key over envVars.keySet()
      envName = key
      envValu = String envVars.get(key)
      say envName '=' envValu
      end key
    end
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sysProperties(vn = '') public static
  if vn.length > 0 then do
    propName = vn
    propValu = System.getProperty(propName)
    if propValu = null then propValu = ''
    say propName '=' propValu
    end
  else do
    sysProps = System.getProperties()
    key = String
    loop key over sysProps.keySet()
      propName = key
      propValu = sysProps.getProperty(key)
      say propName '=' propValu
      end key
    end
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg ev pv .
  if ev = '' then ev = 'CLASSPATH'
  if pv = '' then pv = 'java.class.path'
  say '-'.left(80, '-').overlay(' Environment "'ev'" ', 5)
  sysEnvironment(ev)
  say '-'.left(80, '-').overlay(' Properties "'pv'" ', 5)
  sysProperties(pv)
  say
  say '-'.left(80, '-').overlay(' Environment ', 5)
  sysEnvironment()
  say '-'.left(80, '-').overlay(' Properties ', 5)
  sysProperties()
  say
  return

```

{{out}}

```txt

---- Environment "CLASSPATH" ---------------------------------------------------
CLASSPATH = /usr/local/NetRexx/runlib/NetRexxR.jar:.
---- Properties "java.class.path" ----------------------------------------------
java.class.path = /usr/local/NetRexx/runlib/NetRexxR.jar:.

---- Environment ---------------------------------------------------------------
HOME = /Users/nrxuser
HISTCONTROL = ignoredups
USER = nrxuser
ZBASHRC = 1
COMMAND_MODE = unix2003
CLASSPATH = /usr/local/NetRexx/runlib/NetRexxR.jar:.
SHELL = /bin/bash
. . .
---- Properties ----------------------------------------------------------------
java.vm.specification.name = Java Virtual Machine Specification
sun.cpu.endian = little
sun.io.unicode.encoding = UnicodeBig
sun.os.patch.level = unknown
file.separator = /
java.vendor = Oracle Corporation
sun.java.launcher = SUN_STANDARD
java.specification.vendor = Oracle Corporation
user.home = /Users/nrxuser
java.class.path = /usr/local/NetRexx/runlib/NetRexxR.jar:.
java.vm.vendor = Oracle Corporation
java.runtime.name = Java(TM) SE Runtime Environment
. . .

```



## NewLISP


```NewLISP>
 (env "SHELL")
"/bin/zsh"
> (env "TERM")
"xterm"
```



## Nim


```nim
import os
echo getEnv("HOME")
```



## NSIS

While common environment variables exist as constants within the NSIS script compilation environment [http://nsis.sourceforge.net/Docs/Chapter4.html#4.2.3 (see NSIS documentation)], arbitrarily-named environment variables' values may be retrieved using [http://nsis.sourceforge.net/Docs/Chapter4.html#4.9.2.7 '''ExpandEnvStrings'''].

```nsis
ExpandEnvStrings $0 "%PATH%" ; Retrieve PATH and place it in builtin register 0.
ExpandEnvStrings $1 "%USERPROFILE%" ; Retrieve the user's profile location and place it in builtin register 1.
ExpandEnvStrings $2 "%USERNAME%" ; Retrieve the user's account name and place it in builtin register 2.
```


=={{header|Objective-C}}==
<code>[[NSProcessInfo processInfo] environment]</code> returns an <tt>NSDictionary</tt> of the current environment.

```objc
[[[NSProcessInfo processInfo] environment] objectForKey:@"HOME"]
```



## OCaml


```ocaml
Sys.getenv "HOME"
```



## Oforth



```Oforth
System getEnv("PATH") println
```



## Oz


```oz
{System.showInfo "This is where Mozart is installed: "#{OS.getEnv 'OZHOME'}}
```



## PARI/GP

{{works with|PARI/GP|2.6.0 and above}}

```parigp
getenv("HOME")
```

{{works with|PARI/GP|2.4.3 and above}}

```parigp
externstr("echo $HOME")
```

{{works with|PARI/GP|2.0.3 and above}}
In older versions, the command must effectively be triple-quoted:

```parigp
extern("echo \"\\\"$HOME\\\"\"")
```

The shell sees

```bash
echo "\"$HOME\""
```

which causes it to return

```txt
"/home/username"
```

so that the result is interpreted by GP as a string.

Leaving out the quotation marks allows external commands to return expressions that are then evaluated by GP. For example,

```parigp
extern("echo Pi")
```

causes the shell to send Pi back to GP, which interprets the result and returns

```txt
%1 = 3.141592653589793238462643383
```



## Perl

The <code>%ENV</code> hash maps environment variables to their values:

```perl
print $ENV{HOME}, "\n";
```


The <code>POSIX</code>module also has <code>getenv()</code> which is the same thing as a function.

```perl
use POSIX 'getenv';
print getenv("HOME"),"\n";
```



## Perl 6

{{works with|Rakudo|#24 "Seoul"}}
The <code>%*ENV</code> hash maps environment variables to their values:

```perl6
say %*ENV<HOME>;
```



## Phix


```Phix
?getenv("PATH")
```



## PHP

The <tt>$_ENV</tt> associative array maps environmental variable names to their values:

```php
$_ENV['HOME']
```



## PicoLisp


```PicoLisp
: (sys "TERM")
-> "xterm"

: (sys "SHELL")
-> "/bin/bash"
```



## PowerShell

Environment variables can be found in the Env: drive and are accessed using a special variable syntax:

```powershell
$Env:Path
```

To get a complete listing of all environment variables one can simply query the appropriate drive for its contents:

```powershell
Get-ChildItem Env:
```



## Prolog

SWI-Prolog has the built in function '''getenv'''.

```txt
 ?- getenv('TEMP', Temp).

```



## PureBasic

PureBasic has the built in funtion

```PureBasic
GetEnvironmentVariable("Name")
```

'''Example'''

```PureBasic
If OpenConsole()
  PrintN("Path:"+#CRLF$ + GetEnvironmentVariable("PATH"))
  PrintN(#CRLF$+#CRLF$+"NUMBER_OF_PROCESSORS= "+ GetEnvironmentVariable("NUMBER_OF_PROCESSORS"))

  PrintN(#CRLF$+#CRLF$+"Press Enter to quit.")
  Input() 
  CloseConsole()
EndIf
```



## Python

The <tt>os.environ</tt> dictionary maps environmental variable names to their values:

```python
import os
os.environ['HOME']
```



## R


```R
Sys.getenv("PATH")
```



## Racket


```racket

#lang racket
(getenv "HOME")

```



## REBOL


```REBOL
print get-env "HOME"
```



## Retro


```Retro
here "HOME" getEnv
here puts
```



## REXX

Each REXX interpreter sets its own rules by what identifies the pool in which the environmental variables are named. In addition, each operation system (OS) has their own definition as well. This makes it problematic in the accessing/acquiring of environmental variables. Most programmers know what REXX interpreter they are using, and furthermore, they also know what operating system they are writing the REXX program for, so most programmers hard-wire (explicitly code) the "access-name" of the system environmental variables into the program.

The following will work for 
::* '''Regina'''
::* '''R4'''
::* '''ROO''' 
for the DOS shell under Microsoft Windows (any version).<br />
(Also successfully tested with Regina under the bash shell in UNIX.)
{{works with|Regina}}
{{works with|R4}}
{{works with|ROO}}

```rexx
/*REXX program shows how to get an environmental variable under Windows*/

x=value('TEMP',,'SYSTEM')
```

The following will work for 
::* '''PC/REXX'''
::* '''Personal REXX'''
::* '''Regina''' 
::* '''Open Object Rexx'''
for the DOS shell under Microsoft Windows (any version).<br />
(Also successfully tested with Regina and ooRexx under the bash shell in UNIX.)
{{works with|PC/REXX}}
{{works with|Personal REXX}}
{{works with|Regina}}
{{works with|ooRexx}}

```rexx
/*REXX program shows how to get an environmental variable under Windows*/

x=value('TEMP',,'ENVIRONMENT')
```


The brexx interpreter provides a getenv function for accessing environment variables:
{{works with|Brexx}}

```rexx
x=getenv("PATH") /* Get the contents of the path environment variable */
```


Other REXX interpreters have their own requirements to identify the SYSTEM environment. 

VM/CMS has something called GLOBALV (global variables) and are of three types:
::* temporary,   lasting only for execution of the REXX program
::* temporary,   lasting only for LOGON or CMS session)
::* permanent
As such, CMS has its own command interface for these variables.


## Ring


```ring

see get("path")

```



## Ruby

The <tt>ENV</tt> hash maps environment variable names to their values:

```ruby
ENV['HOME']
```



## Run BASIC


```runbasic
' ------- Major environment variables -------------------------------------------
'DefaultDir$    - The folder path where program files are read/written by default
'Platform$      - The operating system on which Run BASIC is being hosted
'UserInfo$      - This is information about the user's web browser
'UrlKeys$       - Contains informational parameters from the URL submitted when the user connected
'UserAddress$   - Contains the IP address of the user
'ProjectsRoot$  - The folder path where Run BASIC keeps programming projects
'ResourcesRoot$ - The folder path where Run BASIC keeps web-servable files
'Err$           - A description of the last runtime error
'Err            - A numeric code for the last runtime error (errors that have no code use zero)
'EventKey$      - The id of the object that generated the last user event
'RowIndex       - The numeric index of the table or database accessor link that generated the last user event


print "User Info is   : ";UserInfo$
print "Platform is    : ";Platform$
print "Url Keys is    : ";UrlKeys$
print "User Address is: ";UserAddress$
print "Event Key is   : ";EventKey$
print "Default Dir is : ";DefaultDir$
```


```txt

{{out}}
User Info is   : Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.79 Safari/535.11
Platform is    : win32
Url Keys is    : none
User Address is: 127.0.0.1
Event Key is   : none
Default Dir is : c:\rbp101

```



## Rust


```Rust
use std::env;

fn main() {
    println!("{:?}", env::var("HOME"));
    println!();
    for (k, v) in env::vars().filter(|(k, _)| k.starts_with('P')) {
        println!("{}: {}", k, v);
    }
}

```

{{out}}

```txt
Ok("/root")

PATH: /root/.cargo/bin:/root/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
PLAYGROUND_EDITION: 2018
PLAYGROUND_TIMEOUT: 10
PWD: /playground
```

Note that <code>var_os</code> and <code>vars_os</code> are also available, which produce <code>OsString</code> instead of <code>String</code>, offering compatibility with non-utf8 systems.


## Scala


```scala
sys.env.get("HOME")
```



## Seed7

Seed7 provides the function [http://seed7.sourceforge.net/manual/os.htm#getenv getenv],
to get the value of an environment variable. Environment variables are highly operating system
dependent. Some variables such as HOME are not always defined and others like PATH use
an operating system dependent format (different delimiters). Seed7 provides the functions
[http://seed7.sourceforge.net/libraries/osfiles.htm#homeDir homeDir] and [http://seed7.sourceforge.net/libraries/process.htm#getSearchPath getSearchPath]
to get the home directory and the search path in an operating system independent manner.


```seed7
$ include "seed7_05.s7i";
 
const proc: main is func
  begin
    writeln(getenv("HOME"));
  end func;
```



## Sidef

The ''ENV'' hash maps environment variables to their values:

```ruby
say ENV{'HOME'};
```



## Slate

 
```slate
Environment variables at: 'PATH'.
"==> '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games'"
```



## Smalltalk

Use the [http://pharo.gemtalksystems.com/book/PharoTools/OSProcess OSProcess] library to gain access to environment variables:


```smalltalk

OSProcess thisOSProcess environment at: #HOME.
OSProcess thisOSProcess environment at: #PATH.
OSProcess thisOSProcess environment at: #USER.

```



## SNOBOL4

{{works with|Macro Spitbol}}
{{works with|CSnobol}}
The host(4) function returns a known environment variable.

```SNOBOL4
         output = host(4,'PATH')
end
```



## Standard ML


```sml
OS.Process.getEnv "HOME"
```

returns an option type which is either SOME value or NONE if variable doesn't exist


## Stata

Use the '''env''' [http://www.stata.com/help.cgi?extended_fcn extended macro function].


```stata
display "`:env PATH'"
display "`:env USERNAME'"
display "`:env USERPROFILE'"
```



## Tcl

The <code>env</code> global array maps environmental variable names to their values:

```tcl
$env(HOME)
```



## TXR

TXR can treat the environment vector as text stream:

```txr
@(next :env)
@(collect)
@VAR=@VAL
@(end)
```

A recently added <code>gather</code> directive is useful for extracting multiple items of data from an unordered stream of this kind (not only the environment vector):

```txr
@(next :env)
@(gather)
HOME=@home
USER=@user
PATH=@path
@(end)
```

What if some of the variables might not exist? Gather has some discipline for that. The following means that three variables are required (the gather construct fails if they are not found), but <code>shell</code> is optional with a default value of <code>/bin/sh</code> if it is not extracted from the data:

```txr
@(next :env)
@(gather :vars (home user path (shell "/bin/sh")))
HOME=@home
USER=@user
PATH=@path
SHELL=@shell
@(end)
```

From TXR Lisp, the environment is available via the <code>(env)</code> function, which returns a raw list of <code>"name=value</code> strings. The <code>(env-hash)</code> function returns a hash from environment keys to their values.


```bash
$ ./txr -p "(mapcar (env-hash) '(\"HOME\" \"USER\" \"PATH\"))"
("/home/kaz" "kaz" "/home/kaz/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/home/kaz/bin"
```


Here, the hash is being used as a function to filter several environment keys to their values via <code>mapcar</code>.


Platform note: On POSIX, environment variables, which are extracted using <code>extern char **environ</code> are assumed to contain UTF-8. On Windows, the <code>GetEnvironmentStringsW</code> function is used to obtain the environment vector as wide character data.


## UNIX Shell

In the Unix Shell Language, environment variables are available as ordinary variables:

```bash
echo "$HOME"
```

An ordinary variable can be marked as an environment variable with the <code>export</code> command:

```bash>export VAR</lang

Now child processes launched by the shell will have an environment variable called <code>VAR</code>.

The Unix command "env" will print out all of the environment variables 
as key=value pairs on standard output.


## Ursa


```ursa
import "system"
out (system.getenv "HOME") endl console
```



## Ursala

The argument to the main program is a record initialized by the run-time system in which one of the fields (environs) contains the environment as a list of key:value pairs.

```Ursala
#import std

#executable ('parameterized','')

showenv = <.file$[contents: --<''>]>+ %smP+ ~&n-={'TERM','SHELL','X11BROWSER'}*~+ ~environs
```

The rest of this application searches for the three variables named 
and displays them on standard output.

{{out|Here is a bash session}}

```txt

$ showenv
<
   'TERM': 'Eterm',
   'SHELL': '/bin/bash',
   'X11BROWSER': '/usr/bin/firefox'>
```



## Vedit macro language


```vedit
Get_Environment(10,"PATH")
Message(@10)
```

Or with short keywords:

```vedit
GE(10,"PATH") M(@10)
```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb
Debug.Print Environ$("PATH")
```



## XPL0

This task was particularly worthwhile because it revealed a discrepancy
in the way 32-bit XPL0 accessed the environment block. A small mod to
Tran's PMODE.ASM DPMI was required to make the 32-bit protected-mode
version work the same as the 16-bit real-mode versions.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings
int  CpuReg, PspSeg, EnvSeg, I, J, C;
char EnvVar;
[CpuReg:= GetReg;               \access CPU registers
PspSeg:= CpuReg(9);             \get segment address of our PSP
EnvSeg:= Peek(PspSeg,$2C) + Peek(PspSeg,$2D)<<8;
EnvVar:= "PATH";                \environment variable
I:= 0;
loop    [J:= 0;
        loop    [C:= Peek(EnvSeg,I);  I:= I+1;
                if C = 0 then quit;
                if C = EnvVar(J) then
                        [J:= J+1;
                        if J = 4 then
                                [Text(0, EnvVar);               \show env. var.
                                loop    [C:= Peek(EnvSeg,I);    \ and rest of
                                        I:= I+1;                \ its string
                                        if C = 0 then exit;
                                        ChOut(0, C);
                                        ];
                                ];
                        ]
                else J:= 5;     \line must start with environment variable
                ];
        if Peek(EnvSeg,I) = 0 then quit;        \double 0 = env. var. not found
        ];
]
```


{{out}}

```txt

PATH=\masm6;C:\;C:\CXPL;C:\UTIL;C:\DOS;C:\BORLANDC\BIN

```



## zkl


```zkl
System.getenv("HOME")
/home/craigd
System.getenv() //--> Dictionary of all env vars
```

