+++
title = "Rename a file"
description = ""
date = 2019-08-25T19:18:56Z
aliases = []
[extra]
id = 2030
[taxonomies]
categories = ["task", "File System Operations"]
tags = []
+++

## Task

Rename:
:::*   a file called     '''input.txt'''     into     '''output.txt'''     and
:::*   a directory called     '''docs'''     into     '''mydocs'''.


This should be done twice:
once "here", i.e. in the current working directory and once in the filesystem root.

It can be assumed that the user has the rights to do so.

(In unix-type systems, only the user root would have
sufficient permissions in the filesystem root.)





## Ada


```ada
with Ada.Directories;  use Ada.Directories;
   ...
Rename ("input.txt", "output.txt");
Rename ("docs", "mydocs");
Rename ("/input.txt", "/output.txt");
Rename ("/docs", "/mydocs");
```

The behavior depends on the concrete [[OS | operating system]] regarding:
* file name encoding issues;
* file path notation (directory separator, directory syntax etc);
* file extension syntax;
* file system root (provided there is any).


## ALGOL 68

<!-- {{does not work with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386.}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - ''get directory'' and'' grep in string'' not available in any library ... yet}} -->
Note: <tt>reidf</tt> does not appear to be included in [[ALGOL 68G]].  Also note that file names would be Operating System dependent.

```algol68
main:(
  PROC rename = (STRING source name, dest name)INT:
  BEGIN
    FILE actual file;
    INT errno = open(actual file, source name, stand back channel);
    IF errno NE 0 THEN
      errno
    ELSE
      IF reidf possible(actual file) THEN
        reidf(actual file, dest name); # change the identification of the book #
        errno
      ELSE
        close(actual file);
        -1
      FI
    FI
  END;
  rename("input.txt", "output.txt");
  rename("/input.txt", "/output.txt");
  rename("docs", "mydocs");
  rename("/docs", "/mydocs")
)
```



## AWK

Awk allows to call operating system commands with the ''system()'' function. However, the awk script won't get its output, only the return code. But this task is simple enough for the trivial implementation to work:

```awk
$ awk 'BEGIN{system("mv input.txt output.txt")}'
$ awk 'BEGIN{system("mv docs mydocs")}'
$ awk 'BEGIN{system("mv /input.txt /output.txt")}'
$ awk 'BEGIN{system("mv docs mydocs")}'
```



## AutoHotkey


```AutoHotkey
FileMove, oldname, newname
```



## AutoIt

FileMove("input.txt", "output.txt")
FileMove("\input.txt", "\output.txt")
DirMove("docs", "mydocs")
DirMove("\docs", "\mydocs")


## BaCon


```freebasic
RENAME "input.txt" TO "output.txt"
RENAME "/input.txt" TO "/output.txt"
RENAME "docs" TO "mydocs"
RENAME "/docs" TO "/mydocs"

```



## BASIC


```qbasic
NAME "input.txt" AS "output.txt"
NAME "\input.txt" AS "\output.txt"
NAME "docs" AS "mydocs"
NAME "\docs" AS "\mydocs"
```


=
## Applesoft BASIC
=
Apple DOS 3.3 has a flat filesystem with no concept of directories. DOS commands such as RENAME are run directly from the Applesoft prompt but are not technically part of Applesoft BASIC. Thus, attempting to add this line to a program and run it results in a syntax error on that line.


```basic
RENAME INPUT.TXT,OUTPUT.TXT
```


To use a DOS command from a program, use the BASIC statement PRINT followed by CHR$(4) and the string containing the DOS command.


```basic
10  PRINT  CHR$ (4)"RENAME INPUT.TXT,OUTPUT.TXT"
```


Apple ProDOS does have directories.  To change the current working directory to the "root" directory, issue a PREFIX / command.


```basic
10  PRINT  CHR$ (4)"RENAME INPUT.TXT,OUTPUT.TXT"
20  PRINT  CHR$ (4)"RENAME DOCS,MYDOCS"
30  PRINT  CHR$ (4)"PREFIX /"
40  PRINT  CHR$ (4)"RENAME INPUT.TXT,OUTPUT.TXT"
50  PRINT  CHR$ (4)"RENAME DOCS,MYDOCS"
```


=
## ZX Spectrum Basic
=
The ZX Spectrum basic does not not have a facility to rename a file on the microdrive. To rename a file, it is necessary to delete the file and recreate it. Alternatively, a machine code program can be used to achieve this.


## Batch File


```dos
ren input.txt output.txt
ren \input.txt output.txt
ren docs mydocs
ren \docs mydocs
```



## BBC BASIC

When the file or directory names are known as constants at 'compile time':

```bbcbasic
      *RENAME input.txt output.txt
      *RENAME \input.txt \output.txt
      *RENAME docs. mydocs.
      *RENAME \docs. \mydocs.
```

When the file or directory names are known only at run time:

```bbcbasic
      OSCLI "RENAME input.txt output.txt"
      OSCLI "RENAME \input.txt \output.txt"
      OSCLI "RENAME docs. mydocs."
      OSCLI "RENAME \docs. \mydocs."
```



## Bracmat


```bracmat
ren$("input.txt"."output.txt")         { 'ren' is based on standard C function 'rename()' }
ren$(docs.mydocs)                      { No quotes needed: names don't contain dots or colons. }
ren$("d:\\input.txt"."d:\\output.txt") { Backslash is escape character, so we need to escape it. }
ren$(@"d:\docs".@"d:\mydocs")          { @ used as syntactic sugar as in C# for inhibiting escape. }

```



## C


```c
#include <stdio.h>

int main()
{
  rename("input.txt", "output.txt");
  rename("docs", "mydocs");
  rename("/input.txt", "/output.txt");
  rename("/docs", "/mydocs");
  return 0;
}
```



## C++

```cpp
#include <cstdio>

int main()
{
    std::rename("input.txt", "output.txt");
    std::rename("docs", "mydocs");
    std::rename("/input.txt", "/output.txt");
    std::rename("/docs", "/mydocs");
    return 0;
}
```


compiled with g++ -lboost_system -lboost_filesystem


```cpp
#include "boost/filesystem.hpp"

int main()
{
    boost::filesystem::rename(
        boost::filesystem::path("input.txt"),
        boost::filesystem::path("output.txt"));
    boost::filesystem::rename(
        boost::filesystem::path("docs"),
        boost::filesystem::path("mydocs"));
    boost::filesystem::rename(
        boost::filesystem::path("/input.txt"),
        boost::filesystem::path("/output.txt"));
    boost::filesystem::rename(
        boost::filesystem::path("/docs"),
        boost::filesystem::path("/mydocs"));*/
    return 0;
}
```



## C#



```c#
using System;
using System.IO;

class Program {
    static void Main(string[] args) {
        File.Move("input.txt","output.txt");
        File.Move(@"\input.txt",@"\output.txt");

        Directory.Move("docs","mydocs");
        Directory.Move(@"\docs",@"\mydocs");
    }
}
```



## Clipper



```Clipper
FRename( "input.txt","output.txt")
or
RENAME input.txt TO output.txt

FRename("\input.txt","\output.txt")
or
RENAME \input.txt TO \output.txt
```


Clipper has no it's own functions to rename a directory, it is possible, though, to write appropriate code in '''C''' and link it to Clipper application, using the Clipper's Extend system.


## Clojure


```clojure
(import '(java.io File))

(.renameTo (File. "input.txt") (File. "output.txt"))
(.renameTo (File. "docs") (File. "mydocs"))

(.renameTo
 (File. (str (File/separator) "input.txt"))
 (File. (str (File/separator) "output.txt")))
(.renameTo
 (File. (str (File/separator) "docs"))
 (File. (str (File/separator) "mydocs")))
```



## Common Lisp

<code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_fil.htm rename-file]</code>


```lisp
(rename-file "input.txt" "output.txt")
(rename-file "docs" "mydocs")
(rename-file "/input.txt" "/output.txt")
(rename-file "/docs" "/mydocs")
```



## D


```d
std.file.rename("input.txt","output.txt");
std.file.rename("/input.txt","/output.txt");
std.file.rename("docs","mydocs");
std.file.rename("/docs","/mydocs");
```



## DCL


```DCL
rename input.txt output.txt
rename docs.dir mydocs.dir
rename [000000]input.txt [000000]output.txt
rename [000000]docs.dir [000000]mydocs.dir
```



## Delphi


```Delphi
program RenameFile;

{$APPTYPE CONSOLE}

uses SysUtils;

begin
  SysUtils.RenameFile('input.txt', 'output.txt');
  SysUtils.RenameFile('\input.txt', '\output.txt');

  // RenameFile works for both files and folders
  SysUtils.RenameFile('docs', 'MyDocs');
  SysUtils.RenameFile('\docs', '\MyDocs');
end.
```



## E



```e>for where in [<file:.
, <file:///>] {
  where["input.txt"].renameTo(where["output.txt"], null)
  where["docs"].renameTo(where["mydocs"], null)
}
```



## Elixir


```elixir
File.rename "input.txt","output.txt"
File.rename "docs", "mydocs"
File.rename "/input.txt", "/output.txt"
File.rename "/docs", "/mydocs"
```



## Emacs Lisp


```lisp
(rename-file "input.txt" "output.txt")
(rename-file "/input.txt" "/output.txt")
(rename-file "docs" "mydocs")
(rename-file "/docs" "/mydocs")
```


This can also be done interactively with <code>M-x rename-file</code>.  Programmatically the default is to throw an error if the new name already exists but the "ok-if-already-exists" parameter can either forcibly overwrite or query the user.  Query is the default interactively.


## Erlang



```erlang

file:rename("input.txt","output.txt"),
file:rename( "docs", "mydocs" ),
file:rename( "/input.txt", "/output.txt" ),
file:rename( "/docs", "/mydocs" ).

```



## ERRE


Renaming a file using ERRE language is a operating system-dependant operation.
In PC version you must use the SHELL command like:

   CMD$="REN "+OLDFILENAME$+" "+NEWFILENAME$
   SHELL CMD$

with an obvious meaning of the string variables. You can specify a path using DOS conventions.
In C64 version you must use

   OPEN(15,8,15,"R0:"+NF$+"="+OF$)
   CLOSE(15)

with an obvious meaning of the string variables.

=={{header|F_Sharp|F#}}==
```fsharp
open System.IO

[<EntryPoint>]
let main args =
    File.Move("input.txt","output.txt")
    File.Move(@"\input.txt",@"\output.txt")
    Directory.Move("docs","mydocs")
    Directory.Move(@"\docs",@"\mydocs")
    0
```



## Factor


```factor
"" "/" [
    [ "input.txt" "output.txt" move-file "docs" "mydocs" move-file ] with-directory
] bi@
```



## Fantom


```fantom

class Rename
{
  public static Void main ()
  {
    // rename file/dir in current directory
    File.rename("input.txt".toUri).rename("output.txt")
    File.rename("docs/".toUri).rename("mydocs/")
    // rename file/dir in root directory
    File.rename("/input.txt".toUri).rename("/output.txt")
    File.rename("/docs/".toUri).rename("/mydocs/")
  }
}

```



## Forth


```forth
 s" input.txt"  s" output.txt" rename-file throw
s" /input.txt" s" /output.txt" rename-file throw
```



## Fortran

Using UNIX extensions to '''Fortran 77''' (see e.g. [http://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Rename-Intrinsic-_0028subroutine_0029.html#Rename-Intrinsic-_0028subroutine_0029 g77 manual]) :

```fortran
      PROGRAM EX_RENAME
      CALL RENAME('input.txt','output.txt')
      CALL RENAME('docs','mydocs')
      CALL RENAME('/input.txt','/output.txt')
      CALL RENAME('/docs','/mydocs')
      END
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim result As Long
result = Name("input.txt", "output.txt")
If result <> 0 Then
  Print "Renaming file failed"
End If

result = Name("docs", "mydocs")
If result <> 0 Then
  Print "Renaming directory failed"
End If

Sleep
```



## Go


```go
package main
import "os"

func main() {
  os.Rename("input.txt", "output.txt")
  os.Rename("docs", "mydocs")
  os.Rename("/input.txt", "/output.txt")
  os.Rename("/docs", "/mydocs")
}
```



## Groovy

Using File

```groovy
['input.txt':'output.txt', 'docs':'mydocs'].each { src, dst ->
  ['.', ''].each { dir ->
    new File("$dir/$src").renameTo(new File("$dir/$dst"))
  }
}
```


Using Ant

```groovy
['input.txt':'output.txt', 'docs':'mydocs'].each { src, dst ->
  ['.', ''].each { dir ->
    new AntBuilder().move(file:"$dir/$src", toFile:"$dir/$dst")
  }
}
```



## Harbour



```visualfoxpro
FRename( "input.txt","output.txt")
// or
RENAME input.txt TO output.txt

FRename( hb_ps() + "input.txt", hb_ps() + "output.txt")
```


Harbour has no it's own functions to rename a directory, it is possible, though, to write appropriate code in '''C''' and link it to Harbour application, using the Harbour's Extend system.


## Haskell


```haskell
import System.IO
import System.Directory

main = do
  renameFile "input.txt" "output.txt"
  renameDirectory "docs" "mydocs"
  renameFile "/input.txt" "/output.txt"
  renameDirectory "/docs" "/mydocs"
```



## HicEst


```hicest
WRITE(FIle='input.txt', REName='.\output.txt')
SYSTEM(DIR='E:\HicEst\Rosetta')
WRITE(FIle='.\docs', REName='.\mydocs')

WRITE(FIle='\input.txt', REName='\output.txt')
SYSTEM(DIR='\')
WRITE(FIle='\docs', REName='\mydocs')
```


=={{header|Icon}} and {{header|Unicon}}==
Icon supports 'rename' for files only.

```Unicon
every dir := !["./","/"] do {
   rename(f := dir || "input.txt", dir || "output.txt")  |stop("failure for file rename ",f)
   rename(f := dir || "docs", dir || "mydocs")           |stop("failure for directory rename ",f)
   }
```

Note: Icon and Unicon accept both / and \ for directory separators.


## Io



```Io

// rename file in current directory
f := File with("input.txt")
f moveTo("output.txt")

// rename file in root directory
f := File with("/input.txt")
f moveTo("/output.txt")

// rename directory in current directory
d := Directory with("docs")
d moveTo("mydocs")

// rename directory in root directory
d := Directory with("/docs")
d moveTo("/mydocs")

```



## J


J does not ship with a built-in utility for renaming files.
The following will work on Windows, Linux and Macs:


```j
frename=: 4 : 0
 if. x -: y do. return. end.
 if. IFUNIX do.
   hostcmd=. [: 2!:0 '('"_ , ] , ' || true)'"_
   hostcmd 'mv "',y,'" "',x,'"'
 else.
   'kernel32 MoveFileA i *c *c' 15!:0 y;x
 end.
)
```


Useage:

```j
'output.txt' frename 'input.txt'
'/output.txt' frename '/input.txt'
'mydocs' frename 'docs'
'/mydocs' frename '/docs'
```



## Java


```java
import java.io.File;
public class FileRenameTest {
   public static boolean renameFile(String oldname, String newname) {
       // File (or directory) with old name
       File file = new File(oldname);

       // File (or directory) with new name
       File file2 = new File(newname);

       // Rename file (or directory)
       boolean success = file.renameTo(file2);
       return success;
   }
   public static void test(String type, String oldname, String newname) {
       System.out.println("The following " + type + " called " + oldname +
           ( renameFile(oldname, newname) ? " was renamed as " : " could not be renamed into ")
           + newname + "."
       );
   }
   public static void main(String args[]) {
        test("file", "input.txt", "output.txt");
        test("file", File.separator + "input.txt", File.separator + "output.txt");
        test("directory", "docs", "mydocs");
        test("directory", File.separator + "docs" + File.separator, File.separator + "mydocs" + File.separator);
   }
}
```



## JavaScript

Throws an error if the destination file/folder exists.

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject")
fso.MoveFile("input.txt", "output.txt")
fso.MoveFile("c:/input.txt", "c:/output.txt")
fso.MoveFolder("docs", "mydocs")
fso.MoveFolder("c:/docs", "c:/mydocs")
```



## Joy


```Joy

"input.txt" "output.txt" frename
"/input.txt" "/output.txt" frename
"docs" "mydocs" frename
"/docs" "/mydocs" frename.

```



## Jsish


```javascript
/* File rename, in jsish */
try { File.rename('input.txt', 'output.txt', false); } catch (str) { puts(str); }

exec('touch input.txt');
puts("overwrite set true if output.txt exists");
File.rename('input.txt', 'output.txt', true);

try { File.rename('docs', 'mydocs', false); } catch (str) { puts(str); }
try { File.rename('/docs', '/mydocs', false); } catch (str) { puts(str); }
```



## Julia


```julia

mv("input.txt", "output.txt")
mv("docs", "mydocs")
mv("/input.txt", "/output.txt")
mv("/docs", "/mydocs")
```



## Kotlin


```scala
// version 1.0.6

/* testing on Windows 10 which needs administrative privileges
   to rename files in the root */

import java.io.File

fun main(args: Array<String>) {
    val oldPaths = arrayOf("input.txt", "docs", "c:\\input.txt", "c:\\docs")
    val newPaths = arrayOf("output.txt", "mydocs", "c:\\output.txt", "c:\\mydocs")
    var oldFile: File
    var newFile: File
    for (i in 0 until oldPaths.size) {
        oldFile = File(oldPaths[i])
        newFile = File(newPaths[i])
        if (oldFile.renameTo(newFile))
            println("${oldPaths[i]} successfully renamed to ${newPaths[i]}")
        else
            println("${oldPaths[i]} could not be renamed")
    }
}
```


```txt

input.txt successfully renamed to output.txt
docs successfully renamed to mydocs
c:\input.txt successfully renamed to c:\output.txt
c:\docs successfully renamed to c:\mydocs

```



## Lasso


```Lasso
// move file
local(f = file('input.txt'))
#f->moveTo('output.txt')
#f->close

// move directory, just like a file
local(d = dir('docs'))
#d->moveTo('mydocs')

// move file in root file system (requires permissions at user OS level)
local(f = file('//input.txt'))
#f->moveTo('//output.txt')
#f->close

// move directory in root file system (requires permissions at user OS level)
local(d = file('//docs'))
#d->moveTo('//mydocs')
```



## LFE



```lisp

(file:rename "input.txt" "output.txt")
(file:rename "docs" "mydocs")
(file:rename "/input.txt" "/output.txt")
(file:rename "/docs" "/mydocs")

```



## Liberty BASIC


```lb
'   LB has inbuilt 'name' command, but can also run batch files

nomainwin

name "input.txt" as "output.txt"
run "cmd.exe /c ren docs mydocs", HIDE
name "C:\input.txt" as "C:\output.txt"
run "cmd.exe /c ren C:\docs mydocs", HIDE

end
```



## LiveCode


```LiveCode
rename file "input.txt" to "output.txt"
rename folder "docs" to "mydocs"
rename file "/input.txt" to "/output.txt"
rename folder "/docs" to "/mydocs"
```



## Locomotive Basic



```locobasic
|ren,"input.txt","output.txt"
```

([[wp:AMSDOS|AMSDOS]] RSX command, therefore prefixed with a vertical bar. Also, there are no subdirectories in AMSDOS, so there is no way to do the other tests.)


## Lua


```lua
os.rename( "input.txt", "output.txt" )
os.rename( "/input.txt", "/output.txt" )
os.rename( "docs", "mydocs" )
os.rename( "/docs", "/mydocs" )
```



## M2000 Interpreter

To delete a file we have to use Dos shell,so this can be done using Dos command

```M2000 Interpreter

Module checkit {
      Document A$={Alfa, beta}
      Save.Doc A$, "this.aaa"
      Print Exist("this.aaa")=true
      dos "cd "+quote$(dir$)+" && del this.bbb", 100;  ' using; to close dos window, and 100ms for waiting
      Name this.aaa as this.bbb
      Rem : Name "this.aaa" as "this.bbb"  ' we can use strings or variables
      Print Exist("this.bbb")=true
}
checkit

```



## Maple


```Maple
use FileTools in
  Rename( "input.txt", "output.txt" );
  Rename( "docs", "mydocs" );
  Rename( "/input.txt", "/output.txt" ); # assuming permissions in /
  Rename( "/docs", "/mydocs" )           # assuming permissions in /
end use:
```



## Mathematica


```Mathematica
SetDirectory[NotebookDirectory[]]
RenameFile["input.txt", "output.txt"]
RenameDirectory["docs", "mydocs"]
SetDirectory[$RootDirectory]
RenameFile["input.txt", "output.txt"]
RenameDirectory["docs", "mydocs"]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
 [STATUS, MSG, MSGID] = movefile (F1, F2);
```



## MAXScript

MAXScript has no folder rename method

```maxscript
-- Here
renameFile "input.txt" "output.txt"
-- Root
renameFile "/input.txt" "/output.txt"
```



## Mercury


```mercury
:- module rename_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dir.

main(!IO) :-
    rename_file("input.txt", "output.txt", !IO),
    rename_file("docs", "mydocs", !IO),
    rename_file("/input.txt", "/output.txt", !IO),
    rename_file("/docs", "/mydocs", !IO).

:- pred rename_file(string::in, string::in, io::di, io::uo) is det.

rename_file(OldName, NewName, !IO) :-
    io.rename_file(OldName, NewName, Result, !IO),
    (
        Result = ok
    ;
        Result = error(Error),
        print_io_error(Error, !IO)
    ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.nl(Stderr, !IO),
   io.set_exit_status(1, !IO).
```



## MUMPS

<p>ANSI MUMPS doesn't allow access to the operating system except possibly through the View command and $View function, both of which are implementation specific. Intersystems' Caché does allow you to create processes with the $ZF function, and if the permissions for the Caché process allow it you can perform operating system commands.</p>
<p>In Cache on OpenVMS in an FILES-11 filesystem ODS-5 mode these could work:</p>

```MUMPS
 ;Local
 S X=$ZF(-1,"rename input.txt output.txt")
 S X=$ZF(-1,"rename docs.dir mydocs.dir")
  ;Root of current device
 S X=$ZF(-1,"rename [000000]input.txt [000000]output.txt")
 S X=$ZF(-1,"rename [000000]docs.dir [000000]mydocs.dir")
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method isFileRenamed(oldFile, newFile) public static returns boolean
  fo = File(oldFile)
  fn = File(newFile)
  fRenamed = fo.renameTo(fn)
  return fRenamed

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg files
  if files = '' then files = 'input.txt output.txt F docs mydocs D /input.txt /output.txt F /docs /mydocs D'
  loop while files.length > 0
    parse files of nf ft files
    select case(ft.upper())
      when 'F' then do
        ft = 'File'
        end
      when 'D' then do
        ft = 'Directory'
        end
      otherwise do
        ft = 'File'
        end
      end
    if isFileRenamed(of, nf) then dl = 'renamed'
    else                          dl = 'not renamed'
    say ft ''''of'''' dl 'to' nf
    end

  return

```



## NewLISP


```NewLISP
(rename-file "./input.txt" "./output.txt")
(rename-file "./docs" "./mydocs")
(rename-file "/input.txt" "/output.txt")
(rename-file "/docs" "/mydocs")
```



## Nim


```nim
import os

moveFile("input.txt", "output.txt")
moveFile("docs", "mydocs")

moveFile(DirSep & "input.txt", DirSep & "output.txt")
moveFile(DirSep & "docs", DirSep & "mydocs")
```



## Objeck


```objeck

use IO;
bundle Default {
  class FileExample {
    function : Main(args : String[]) ~ Nil {
      File->Rename("input.txt", "output.txt");
      File->Rename("docs", "mydocs");
      File->Rename("/input.txt", "/output.txt");
      File->Rename("/docs", "/mydocs");
    }
  }
}

```


=={{header|Objective-C}}==
```objc
NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
[fm movePath:@"input.txt" toPath:@"output.txt" handler:nil];
[fm movePath:@"docs" toPath:@"mydocs" handler:nil];

// OS X 10.5+
[fm moveItemAtPath:@"input.txt" toPath:@"output.txt" error:NULL];
[fm moveItemAtPath:@"docs" toPath:@"mydocs" error:NULL];
```



## OCaml


```ocaml

let () =
  Sys.rename "input.txt" "output.txt";
  Sys.rename "docs" "mydocs";
  Sys.rename "/input.txt" "/output.txt";
  Sys.rename "/docs" "/mydocs";
```



## Octave


```octave

rename('docs','mydocs');
rename('input.txt','/output.txt');
rename('/docs','/mydocs');

```



## OpenEdge/Progress


```progress
OS-RENAME "input.txt" "output.txt".
OS-RENAME "docs" "mydocs".

OS-RENAME "/input.txt" "/output.txt".
OS-RENAME "/docs" "/mydocs".
```



## PARI/GP

GP has no built-in facilities for renaming, but can use a system call:

```parigp
system("mv input.txt output.txt");
system("mv /input.txt /output.txt");
system("mv docs mydocs");
system("mv /docs /mydocs");
```

PARI, as usual, has access to all the standard [[#C|C]] methods.

Alternatively it's possible to bind rename() system call to a new function:

```parigp
install("rename","iss","rename");
rename("input.txt", "output.txt");
```



## Pascal


```pascal
  var
    f : file ; // Untyped file
 begin

  // as current directory
  AssignFile(f,'input.doc');
  Rename(f,'output.doc');

  // as root directory
  AssignFile(f,'\input.doc');
  Rename(f,'\output.doc');

  // rename a directory
  AssignFile(f,'docs');
  Rename(f,'mydocs');

  //rename a directory off the root

  AssignFile(f,'\docs');
  Rename(f,'\mydocs');

end;
```



## Perl


```perl
use File::Copy qw(move);
use File::Spec::Functions qw(catfile rootdir);
# here
move 'input.txt', 'output.txt';
move 'docs', 'mydocs';
# root dir
move (catfile rootdir, 'input.txt'), (catfile rootdir, 'output.txt');
move (catfile rootdir, 'docs'), (catfile rootdir, 'mydocs');
```


The core <code>rename($oldfile,$newfile)</code> can rename a file within a directory, but has the usual limitations of the <code>rename()</code> system call or C library function, which means generally not working across filesystems, and perhaps not working to rename directories.  <code>move()</code> does a copy and delete when necessary.


## Perl 6


```perl6
rename 'input.txt', 'output.txt';
rename 'docs', 'mydocs';
rename '/input.txt', '/output.txt';
rename '/docs', '/mydocs';
```



## Phix

Windows now makes it fairly difficult to rename files in the root directory, even by hand.
Output is 0 for success, 1 for failure.

```Phix
?rename_file("input.txt","output.txt")
?rename_file("docs","mydocs")
?rename_file("C:\\Copy.txt","Copy.xxx")
?rename_file("C:\\docs","C:\\mydocs")
```



## PHP


```php
<?php
rename('input.txt', 'output.txt');
rename('docs', 'mydocs');
rename('/input.txt', '/output.txt');
rename('/docs', '/mydocs');
?>
```



## PicoLisp


```PicoLisp
(call 'mv "input.txt" "output.txt")
(call 'mv "docs" "mydocs")
(call 'mv "/input.txt" "/output.txt")
(call 'mv "/docs" "/mydocs")
```



## Pike


```pike
int main(){
   mv("input.txt", "output.txt");
   mv("/input.txt", "/output.txt");
   mv("docs", "mydocs");
   mv("/docs", "/mydocs");
}
```



## Pop11


```pop11
sys_file_move('inputs.txt', 'output.txt');
sys_file_move('docs', 'mydocs');
sys_file_move('/inputs.txt', '/output.txt');
sys_file_move(/'docs', '/mydocs');
```


Note that notion of the root of filesystem is Unix specific, so above we
do not try to suport other systems.


## PowerShell


```powershell
Rename-Item input.txt output.txt

# The Rename-item has the alias ren
ren input.txt output.txt
```



## Processing


```processing
void setup(){
  boolean sketchfile = rename(sketchPath("input.txt"), sketchPath("output.txt"));
  boolean sketchfold = rename(sketchPath("docs"), sketchPath("mydocs"));
  // sketches will seldom have write permission to root files/folders
  boolean rootfile = rename("input.txt", "output.txt");
  boolean rootfold = rename("docs", "mydocs");
  // true if succeeded, false if failed
  println(sketchfile, sketchfold, rootfile, rootfold);
}

boolean rename(String oldname, String newname) {
  // File (or directory) with old name
  File file = new File(oldname);
  // File (or directory) with new name
  File file2 = new File(newname);
  // Rename file (or directory)
  boolean success = file.renameTo(file2);
  return success;
}
```



## ProDOS


```ProDOS
rename input.txt to output.txt
rename docs to mydocs
```



## PureBasic



```purebasic
RenameFile("input.txt", "output.txt")
RenameFile("docs\", "mydocs\")

RenameFile("/input.txt","/output.txt")
RenameFile("/docs\","/mydocs\")
```



## Python



```python
import os

os.rename("input.txt", "output.txt")
os.rename("docs", "mydocs")

os.rename(os.sep + "input.txt", os.sep + "output.txt")
os.rename(os.sep + "docs", os.sep + "mydocs")
```


Or the alternative:


```python
import shutil

shutil.move("input.txt", "output.txt")
shutil.move("docs", "mydocs")

shutil.move("/input.txt", "/output.txt")
shutil.move("/docs", "/mydocs")
```



## R


```R
file.rename("input.txt", "output.txt")
file.rename("/input.txt", "/output.txt")
file.rename("docs", "mydocs")
file.rename("/docs", "/mydocs")
```



## Racket



```racket

#lang racket

(rename-file-or-directory "input.txt" "output.txt")
(rename-file-or-directory "docs" "mydocs")

;; find the filesystem roots, and pick the first one
(define root (first (filesystem-root-list)))

(rename-file-or-directory (build-path root "input.txt")
                          (build-path root "output.txt"))
(rename-file-or-directory (build-path root "docs")
                          (build-path root "mydocs"))

```



## Raven


```Raven
`mv /path/to/file/oldfile /path/to/file/newfile` shell
```



## REALbasic


This ''should'' work regardless of what OS it's running under (but untested under [[Mac OS]]).


```vb
Sub Renamer()
    Dim f As FolderItem, r As FolderItem

    f = GetFolderItem("input.txt")
    'Changing a FolderItem's Name attribute renames the file or directory.
    If f.Exists Then f.Name = "output.txt"
    'Files and directories are handled almost identically in RB.
    f = GetFolderItem("docs")
    If f.Exists Then f.Name = "mydocs"

    'Jump through hoops to find the root directory.
    r = RootDir(GetFolderItem("."))

    f = r.Child("input.txt")
    'Renaming in a different directory identical to renaming in current directory.
    If f.Exists Then f.Name = "output.txt"
    f = r.Child("docs")
    If f.Exists Then f.Name = "mydocs"
End Sub

Function RootDir(what As FolderItem) As FolderItem
    'RB doesn't have an easy way to find the root of the current drive;
    'not an issue under *nix but troublesome under Windows.
    If what.Parent <> Nil Then  'Nil = no parent = root.
        Return RootDir(what.Parent) 'Recursive.
    Else
        Return what
    End If
End Function
```



## REBOL


```REBOL
rename %input.txt %output.txt
rename %docs/ %mydocs/

; Unix. Note that there's no path specification used for the
; new name. "Rename" is not "move".

rename %/input.txt %output.txt
rename %/docs/ %mydocs/

; DOS/Windows:

rename %/c/input.txt %output.txt
rename %/c/docs/ %mydocs/

; Because REBOL treats data access schemes as uniformly as possible,
; you can do tricks like this:

rename ftp://username:password@ftp.site.com/www/input.txt %output.txt
rename ftp://username:password@ftp.site.com/www/docs/ %mydocs/

```



## REXX


### error messages shown


```rexx
/*REXX program renames a file & a directory  (in current dir & in root).*/
trace off                              /*suppress error messages, bad RC*/

    do 2                               /* [↓]  perform this code twice. */
    'RENAME' "input.txt  output.txt"   /*rename a particular DOS file.  */
    'MOVE'   "\docs  \mydocs"          /*use (DOS) MOVE to rename a dir.*/
    'CD'     "\"                       /*for 2nd pass, change──►root dir*/
    end   /*2*/
                                       /*stick a fork in it, we're done.*/
```



### error messages suppressed


```rexx
/*REXX program renames a file & a directory  (in current dir & in root).*/
trace off                              /*suppress error messages, bad RC*/
$ = '2> NUL'                           /*used to suppress error messages*/

  do 2                                 /* [↓]  perform this code twice. */
  'RENAME' "input.txt  output.txt"  $  /*rename a particular DOS file.  */
  'MOVE'   "\docs  \mydocs"         $  /*use (DOS) MOVE to rename a dir.*/
  'CD'     "\"                      $  /*for 2nd pass, change──►root dir*/
  end   /*2*/
                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

  rename("input.txt", "output.txt")
  rename("docs", "mydocs")
  rename("/input.txt", "/output.txt")
  rename("/docs", "/mydocs")

```



## Ruby


```ruby
File.rename('input.txt', 'output.txt')
File.rename('/input.txt', '/output.txt')
File.rename('docs', 'mydocs')
File.rename('/docs', '/mydocs')
```


With 'fileutils' from the standard library: The <tt>FileUtils#move</tt> method has some more flexibility than the core <tt>File#rename</tt> method (not really demonstrated here).


```ruby
require 'fileutils'
moves = { "input.txt" => "output.txt", "/input.txt" => "/output.txt", "docs" => "mydocs","/docs" => "/mydocs"}
moves.each{ |src, dest| FileUtils.move( src, dest, :verbose => true ) }
```



## Run BASIC

RB has no direct command. You Shell out to windows or unix.

```runbasic
a$ = shell$("RENAME input.txt output.txt")
a$ = shell$("RENAME docs mydocs")
a$ = shell$("RENAME \input.txt \output.txt")
a$ = shell$("RENAME \docs \mydocs")
```



## Rust


```rust

use std::fs;

fn main() {
    let err = "File move error";
    fs::rename("input.txt", "output.txt").ok().expect(err);
    fs::rename("docs", "mydocs").ok().expect(err);
    fs::rename("/input.txt", "/output.txt").ok().expect(err);
    fs::rename("/docs", "/mydocs").ok().expect(err);
}

```



## Scala

### Straight forward


```Scala
import scala.language.implicitConversions
import java.io.File

object Rename0 {
  def main(args: Array[String]) {              // The implicit causes every String to File mapping,
    implicit def file(s: String) = new File(s) // will be converted to new File(String)
    "myfile.txt" renameTo "anotherfile.txt"
    "/tmp/myfile.txt" renameTo "/tmp/anotherfile.txt"
    "mydir" renameTo "anotherdir"
    "/tmp/mydir" renameTo "/tmp/anotherdir"
  }
}
```



### Alternative


```Scala

object Rename1 {
  def main(args: Array[String]) {
    List(("myfile.txt", "anotherfile.txt"),("/tmp/myfile.txt", "/tmp/anotherfile.txt"),
         ("mydir", "anotherdir"),("/tmp/mydir", "/tmp/anotherdir")).foreach{ case (oldf, newf) ⇒
      new java.io.File(oldf) renameTo new java.io.File(newf)
    }
  }
}

```



## Scheme

```scheme
(rename-file "input.txt" "output.txt")
(rename-file "docs" "mydocs")
(rename-file "/input.txt" "/output.txt")
(rename-file "/docs" "/mydocs")
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/osfiles.htm osfiles.s7i]
defines the function [http://seed7.sourceforge.net/libraries/osfiles.htm#moveFile%28in_string,in_string%29 moveFile],
which renames / moves a file.
Seed7 uses a [http://seed7.sourceforge.net/manual/os.htm#Standard_path_representation standard path representation]
to make paths operating system independent. In the standard path representation
a / is used as path delimiter and drive letters like C: must be written as /c instead.
Creating files and directories in a file system root may need privileges, so the program may fail,
when it is started by a normal user.


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  begin
    moveFile("input.txt", "output.txt");
    moveFile("docs", "mydocs");
    moveFile("/input.txt", "/output.txt");
    moveFile("/docs", "/mydocs");
  end func;
```


Under Windows each filesystem has its own root.
Therefore you need to replace "/input.txt", "/output.txt", "/docs" and "/mydocs" with
"/c/input.txt", "/c/output.txt", "/c/docs" and "/c/mydocs".


## Sidef


```ruby
# Here
File.rename('input.txt', 'output.txt');
File.rename('docs',      'mydocs');

# Root dir
File.rename(Dir.root + %f'input.txt', Dir.root + %f'output.txt');
File.rename(Dir.root + %f'docs',      Dir.root + %f'mydocs');
```



## Slate


```slate
(File newNamed: 'input.txt') renameTo: 'output.txt'.
(File newNamed: '/input.txt') renameTo: '/output.txt'.
(Directory newNamed: 'docs') renameTo: 'mydocs'.
(Directory newNamed: '/docs') renameTo: '/mydocs'.
```



## Smalltalk

```smalltalk
File rename: 'input.txt' to: 'output.txt'.
File rename: 'docs' to: 'mydocs'.
"as for other example, this works on systems
 where the root is / ..."
File rename: '/input.txt' to: '/output.txt'.
File rename: '/docs' to: '/mydocs'
```

```smalltalk
'input.txt' asFilename renameTo: 'output.txt'.
'docs' asFilename renameTo: 'mydocs'.
'/input.txt' asFilename renameTo: '/output.txt'.
'/docs' asFilename renameTo: '/mydocs'
```

```smalltalk
'input.txt' asFileReference renameTo: 'output.txt'.
'docs' asFileReference renameTo: 'mydocs'.
'/input.txt' asFileReference renameTo: '/output.txt'.
'/docs' asFileReference renameTo: '/mydocs'
```



## Standard ML


```sml
OS.FileSys.rename {old = "input.txt", new = "output.txt"};
OS.FileSys.rename {old = "docs", new = "mydocs"};
OS.FileSys.rename {old = "/input.txt", new = "/output.txt"};
OS.FileSys.rename {old = "/docs", new = "/mydocs"};
```



## Stata

Use a '''[http://www.stata.com/help.cgi?shell shell]''' command. The following works on Windows, there are similar commands on other operating systems.


```stata
!ren input.txt output.txt
!ren docs mydocs
```



## Tcl

<i>Assuming</i> that the Bash example shows what is actually meant with this task (one file and one directory here, one file and one directory in the root) and further assuming that this is supposed to be generic (i.e. OS agnostic):

```tcl
file rename inputs.txt output.txt
file rename docs mydocs

file rename [file nativename /inputs.txt] [file nativename /output.txt]
file rename [file nativename /docs] [file nativename /mydocs]
```

Without the need to work on unusual platforms like Mac OS 9, the code could be just:

```tcl
file rename inputs.txt output.txt
file rename docs mydocs

file rename /inputs.txt /output.txt
file rename /docs /mydocs
```



## Toka


```toka
needs shell
" input.txt"  " output.txt"  rename
" /input.txt"  " /output.txt"  rename

" docs"  " mydocs"  rename
" /docs"  " /mydocs"  rename
```



## TorqueScript


```Torque
fileCopy("Input.txt", "Output.txt");
fileDelete("Input.txt");

for(%File = FindFirstFile("Path/*.*"); %File !$= ""; %File = FindNextFile("Path/*.*"))
{
	fileCopy(%File, "OtherPath/" @ %File);
	fileDelete(%File);
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
- rename file
ERROR/STOP RENAME ("input.txt","output.txt")
- rename directory
ERROR/STOP RENAME ("docs","mydocs",-std-)

```



## TXR


TXR works with native paths.


```txrlisp
(rename-path "input.txt" "output.txt")
;; Windows (MinGW based port)
(rename-path "C:\\input.txt" "C:\\output.txt")
;; Unix; Windows (Cygwin port)
(rename-path "/input.txt" "/output.txt"))
```


Directories are renamed the same way; <code>input.txt</code> could be a directory.


## UNIX Shell


```bash
mv input.txt output.txt
mv /input.txt /output.txt
mv docs mydocs
mv /docs /mydocs
```



## Vedit macro language

Vedit allows using either '\' or '/' as directory separator character, it is automatically converted to the one used by the operating system.

```vedit
// In current directory
File_Rename("input.txt", "output.txt")
File_Rename("docs", "mydocs")

// In the root directory
File_Rename("/input.txt", "/output.txt")
File_Rename("/docs", "/mydocs")
```



## Visual Basic .NET

'''Platform:''' [[.NET]]

```vbnet
'Current Directory
IO.Directory.Move("docs", "mydocs")
IO.File.Move("input.txt", "output.txt")

'Root
IO.Directory.Move("\docs", "\mydocs")
IO.File.Move("\input.txt", "\output.txt")

'Root, platform independent
IO.Directory.Move(IO.Path.DirectorySeparatorChar & "docs", _
 IO.Path.DirectorySeparatorChar & "mydocs")
IO.File.Move(IO.Path.DirectorySeparatorChar & "input.txt", _
  IO.Path.DirectorySeparatorChar & "output.txt")
```



## Yorick

```yorick
rename, "input.txt", "output.txt";
rename, "/input.txt", "/output.txt";
rename, "docs", "mydocs";
rename, "/docs", "/mydocs";
```



## Yabasic


```Yabasic
if peek$("os") = "windows" then
    slash$ = "\\" : com$ = "ren "
else
    slash$ = "/" : com$ = "mv "
end if

system(com$ + "input.txt output.txt")
system(com$ + "docs mydocs")
system(com$ + slash$ + "input.txt " + slash$ + "output.txt")
system(com$ + slash$ + "docs " + slash$ + "mydocs")
```



## zkl

Tested on Linux and Windows 10.
```txt

ls -ld input.txt docs output.txt mydocs
ls: cannot access 'output.txt': No such file or directory
ls: cannot access 'mydocs': No such file or directory
drwxr-xr-x 2 craigd craigd 4096 Aug 10 16:24 docs
-rw-r--r-- 1 craigd craigd    0 Aug 10 16:24 input.txt
$ zkl
zkl: File.rename("input.txt","output.txt")
True
zkl: File.rename("docs","mydocs")
True
zkl: ^D
$ ls -ld input.txt docs output.txt mydocs
ls: cannot access 'input.txt': No such file or directory
ls: cannot access 'docs': No such file or directory
drwxr-xr-x 2 craigd craigd 4096 Aug 10 16:24 mydocs
-rw-r--r-- 1 craigd craigd    0 Aug 10 16:24 output.txt

```

If don't have permissions (amongst other things):

```txt

zkl: File.rename("/input.txt","/output.txt")
Exception thrown: IOError(rename(/input.txt,/output.txt): Permission denied)

```



{{omit from|Befunge}} <!-- No filesystem support -->
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
