+++
title = "Delete a file"
description = ""
date = 2019-09-09T16:46:36Z
aliases = []
[extra]
id = 2029
[taxonomies]
categories = []
tags = []
+++

{{task|File System Operations}}

;Task:
Delete a file called "input.txt" and delete a directory called "docs".

This should be done twice: once "here", i.e. in the current working directory and once in the filesystem root.





## 11l


```11l
fs:remove_file(‘output.txt’)
fs:remove_dir(‘docs’)
fs:remove_file(‘/output.txt’)
fs:remove_dir(‘/docs’)
```



## 8th


```Forth

"input.txt" f:rm drop
"/input.txt" f:rm drop
"docs" f:rmdir drop
"/docs" f:rmdir drop

```

The 'drop' removes the result (true or false, indicating success or failure).  It is not strictly necessary to do so, but it keeps the stack clean.


## Ada


```ada>with Ada.Directories;  use Ada.Directories;</lang

and then

```ada
Delete_File ("input.txt");
Delete_File ("/input.txt");
Delete_Tree ("docs");
Delete_Tree ("/docs");
```

Naming conventions for the file path are [[OS]]-specific. The language does not specify the encoding of the file paths, the directory separators or brackets, the file extension delimiter, the file version delimiter and syntax. The example provided works under [[Linux]] and [[Windows]].


## Aikido

The <code>remove</code> function removes either a file or a directory (the directory must be empty for this to work).  Exception is thrown if this fails.

```aikido

remove ("input.txt")
remove ("/input.txt")
remove ("docs")
remove ("/docs")

```



## Aime


```aime
remove("input.txt");
remove("/input.txt");
remove("docs");
remove("/docs");
```



## ALGOL 68

Note: <tt>scratch</tt> does not appear to do anything on [[ALGOL 68G]].  Also note that file names are Operating System dependent.

```algol68
main:(
  PROC remove = (STRING file name)INT:
  BEGIN
    FILE actual file;
    INT errno = open(actual file, file name, stand back channel);
    IF errno NE 0 THEN stop remove FI;
    scratch(actual file); # detach the book and burn it #
    errno
  EXIT
  stop remove:
      errno
  END;
  remove("input.txt");
  remove("/input.txt");
  remove("docs");
  remove("/docs")
)
```



## AutoHotkey


```AutoHotkey
FileDelete, input.txt
FileDelete, \input.txt
FileRemoveDir, docs, 1
FileRemoveDir, \docs, 1
```


### with DllCall

Source: [https://github.com/jNizM/AHK_DllCall_WinAPI/ DeleteFile @github] by jNizM

```AutoHotkey
DeleteFile(lpFileName)
{
    DllCall("Kernel32.dll\DeleteFile", "Str", lpFileName)
}

DeleteFile("C:\Temp\TestFile.txt")
```



## AWK

Assuming we are on a Unix/Linux or at least Cygwin system:

```awk
system("rm input.txt")
system("rm /input.txt")
system("rm -rf docs")
system("rm -rf /docs")
```



## Axe


```axe
DelVar "appvINPUT"
```



## Batch File


```dos
del input.txt
rd /s /q docs

del \input.txt
rd /s /q \docs
```



## BASIC


{{works with|QBasic}}
{{works with|DOS}}

Some versions of Qbasic may have had a builtin RMDIR command. However this is not documented in the manual, so we use the external MSDOS command in this example.


```qbasic

KILL "INPUT.TXT"
KILL "C:\INPUT.TXT"
SHELL "RMDIR /S /Q DIR"
SHELL "RMDIR /S /Q C:\DIR"

```


=
## BaCon
=
BaCon has a <tt>DELETE</tt> instruction, that accepts <tt>FILE|DIRECTORY|RECURSIVE</tt> options.


```freebasic
DELETE FILE "input.txt"
DELETE FILE "/input.txt"
```


Errors can be caught with the <tt>CATCH GOTO label</tt> instruction (which allows <tt>RESUME</tt> from the labelled code section).

=
## ZX Spectrum Basic
=

The ZX Spectrum microdrive had only a main directory, and filenames did not have
file extensions. Here we delete the file named INPUTTXT from the first microdrive:


```zxbasic

ERASE "m"; 1; "INPUTTXT"

```


And for disc drive of ZX Spectrum +3:


```zxbasic

ERASE "a:INPUTTXT"

```


=
## BBC BASIC
=
If the names are known as constants at compile time:

```bbcbasic

      *DELETE input.txt
      *DELETE \input.txt
      *RMDIR docs
      *RMDIR \docs

```

If the names are known only at run time:

```BBC BASIC
      OSCLI "DELETE " + file$
      OSCLI "RMDIR " + dir$
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 WHEN EXCEPTION USE IOERROR
110   EXT "del input.txt"
120   EXT "del \input.txt"
130   EXT "rmdir docs"
140   EXT "rmdir \docs"
150 END WHEN
160 HANDLER IOERROR
170   PRINT "Error in line";EXLINE
180   PRINT "*** ";EXSTRING$(EXTYPE)
190   CONTINUE
200 END HANDLER
```



## C

ISO C:

```c
#include <stdio.h>

int main() {
  remove("input.txt");
  remove("/input.txt");
  remove("docs");
  remove("/docs");
  return 0;
}
```


POSIX:

```c
#include <unistd.h>

int main() {
  unlink("input.txt");
  unlink("/input.txt");
  rmdir("docs");
  rmdir("/docs");
  return 0;
}
```



## C++


```cpp
#include <cstdio>
#include <direct.h>

int main() {
	remove( "input.txt" );
	remove( "/input.txt" );
	_rmdir( "docs" );
	_rmdir( "/docs" );

	return 0;
}
```


## C#

```c#
using System;
using System.IO;

namespace RosettaCode {
    class Program {
        static void Main() {
            try {
                File.Delete("input.txt");
                Directory.Delete("docs");
                File.Delete(@"\input.txt");
                Directory.Delete(@"\docs");
            } catch (Exception exception) {
                Console.WriteLine(exception.Message);
            }
        }
    }
}
```



## Clojure


```lisp
(import '(java.io File))
(.delete (File. "output.txt"))
(.delete (File. "docs"))

(.delete (new File (str (File/separator) "output.txt")))
(.delete (new File (str (File/separator) "docs")))
```



## COBOL

To delete files or directories in COBOL we need to use unofficial extensions. The following are built-in subroutines originally created as part of some of the COBOL products created by Micro Focus.
{{works with|Visual COBOL}}
{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Delete-Files.

       PROCEDURE DIVISION.
           CALL "CBL_DELETE_FILE" USING "input.txt"
           CALL "CBL_DELETE_DIR"  USING "docs"
           CALL "CBL_DELETE_FILE" USING "/input.txt"
           CALL "CBL_DELETE_DIR"  USING "/docs"

           GOBACK
           .
```


Alternate method of deleting files using the <code>DELETE FILE</code> statement.
{{works with|Visual COBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Delete-Files-2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Local-File ASSIGN TO "input.txt".
           SELECT Root-File  ASSIGN TO "/input.txt".

       DATA DIVISION.
       FILE SECTION.
       FD  Local-File.
       01  Local-Record PIC X.

       FD  Root-File.
       01  Root-Record  PIC X.

       PROCEDURE DIVISION.
           DELETE FILE Local-File
           DELETE FILE Root-File

           GOBACK
           .
```



## Common Lisp


```lisp
(delete-file (make-pathname :name "input.txt"))
(delete-file (make-pathname :directory '(:absolute "") :name "input.txt"))
```

To delete directories we need an implementation specific extension. In clisp this is ''ext:delete-dir''.
{{works with|CLISP}}

```lisp
(let ((path (make-pathname :directory '(:relative "docs"))))
  (ext:delete-dir path))

(let ((path (make-pathname :directory '(:absolute "docs"))))
  (ext:delete-dir path))
```


Or you can use the portability library CL-FAD:

{{libheader|CL-FAD}}

```lisp
(let ((path (make-pathname :directory '(:relative "docs"))))
  (cl-fad:delete-directory-and-files path))
```



## Component Pascal

{{Works with|BlackBox Component Builder}}

```oberon2

VAR
  l: Files.Locator;
BEGIN
  (* Locator is the directory *)
  l := Files.dir.This("proof");
  (* delete 'xx.txt' file, in directory 'proof'  *)
  Files.dir.Delete(l,"xx.txt");
END ...

```



## D

{{works with|D|2}}

```d
import std.file: remove;

void main() {
    remove("data.txt");
}
```

{{libheader|Tango}}

```d
import tango.io.Path;

void main() {
    remove("input.txt");
    remove("/input.txt");
    remove("docs");
    remove("/docs");
}
```


{{libheader|Tango}}
POSIX:

```d
import tango.stdc.posix.unistd;

void main() {
  unlink("input.txt");
  unlink("/input.txt");
  rmdir("docs");
  rmdir("/docs");
}
```



## Delphi


```e
procedure TMain.btnDeleteClick(Sender: TObject);
var
  CurrentDirectory : String;
begin
   CurrentDirectory := GetCurrentDir;

   DeleteFile(CurrentDirectory + '\input.txt');
   RmDir(PChar(CurrentDirectory + '\docs'));

   DeleteFile('c:\input.txt');
   RmDir(PChar('c:\docs'));
end;

```



## E


```e><file:input.txt
.delete(null)
<file:docs>.delete(null)
<file:///input.txt>.delete(null)
<file:///docs>.delete(null)
```



## Elena

ELENA 4.x :

```elena
import system'io;

public program()
{
    File.assign("output.txt").delete();

    File.assign("\output.txt").delete();

    Directory.assign("docs").delete();

    Directory.assign("\docs").delete();
}
```



## Elixir


```elixir
File.rm!("input.txt")
File.rmdir!("docs")
File.rm!("/input.txt")
File.rmdir!("/docs")
```



## Emacs Lisp


```Lisp

;; function to remove file & directory
(defun my-files-rm ()
  (delete-file "input.txt")
  (delete-directory "docs"))

(my-files-rm)
(cd "~/") ;; change to home dir
(my-files-rm)

```



## Erlang


```erlang

-module(delete).
-export([main/0]).

main() ->
	% current directory
        ok = file:del_dir( "docs" ),
	ok = file:delete( "input.txt" ),
	% root directory
	ok = file:del_dir( "/docs" ),
	ok = file:delete( "/input.txt" ).

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO

[<EntryPoint>]
let main argv =
    let fileName = "input.txt"
    let dirName = "docs"
    for path in ["."; "/"] do
        ignore (File.Delete(Path.Combine(path, fileName)))
        ignore (Directory.Delete(Path.Combine(path, dirName)))
    0
```



## Factor


```factor
"docs" "/docs" [ delete-tree ] bi@
"input.txt" "/input.txt" [ delete-file ] bi@
```



## Forth

There is no means to delete directories in ANS Forth.

```forth
 s" input.txt" delete-file throw
s" /input.txt" delete-file throw
```



## Fortran

{{works with|Fortran|90 and later}}
I don't know a way of deleting directories in Fortran

```fortran
 OPEN  (UNIT=5, FILE="input.txt", STATUS="OLD")   ! Current directory
 CLOSE (UNIT=5, STATUS="DELETE")
 OPEN  (UNIT=5, FILE="/input.txt", STATUS="OLD")  ! Root directory
 CLOSE (UNIT=5, STATUS="DELETE")
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' delete file and empty sub-directory in current directory

Kill "input.txt"
RmDir "docs"

' delete file and empty sub-directory in root directory c:\
' deleting file in root requires administrative privileges in Windows 10

'Kill "c:\input.txt"
'RmDir "c:\docs"

Print "Press any key to quit"
Sleep

```



## Free Pascal

All required functions already exist in the RTL’s (run-time library) <code>system</code> unit which is shipped with every FPC (Free Pascal compiler) distribution and automatically included by every program.

```pascal
program deletion(input, output, stdErr);
const
	rootDirectory = '/'; // might have to be altered for other platforms
	inputTextFilename = 'input.txt';
	docsFilename = 'docs';
var
	fd: file;
begin
	assign(fd, inputTextFilename);
	erase(fd);

	rmDir(docsFilename);

	assign(fd, rootDirectory + inputTextFilename);
	erase(fd);

	rmDir(rootDirectory + docsFilename);
end.
```

Note, depending on the <code>{$IOChecks}</code> compiler switch state, run-time error generation is inserted.
In particular deletion of non-existent files or lack of privileges may cause an abort.

More convenient routines are <code>sysUtils.deleteFile</code> and <code>sysUtils.removeDir</code>.
Both accept strings and just return, whether the operation was successful.


## Gambas


```gambas
Public Sub Main()

Kill User.home &/ "input.txt"
Rmdir User.home &/ "docs"

'Administrative privileges (sudo) would be required to mess about in Root - I'm not going there!

End
```



## GAP


```gap
# Apparently GAP can only remove a file, not a directory
RemoveFile("input.txt");
# true
RemoveFile("docs");
# fail
```



## Go


```go
package main
import "os"

func main() {
  os.Remove("input.txt")
  os.Remove("/input.txt")
  os.Remove("docs")
  os.Remove("/docs")
  // recursively removes contents:
  os.RemoveAll("docs")
  os.RemoveAll("/docs")
}
```



## Groovy

On most *nix systems, this must be run as sudo for the files in root to be deleted.  If you don't have permissions, it will silently fail to delete those files.  I would recommend against running anything you find on the internet as sudo.


```groovy
// Gets the first filesystem root.  On most systems this will be / or c:\
def fsRoot = File.listRoots().first()

// Create our list of files (including directories)
def files = [
        new File("input.txt"),
        new File(fsRoot, "input.txt"),
        new File("docs"),
        new File(fsRoot, "docs")
]

/*
We use it.directory to determine whether each file is a regular file or directory.  If it is a directory, we delete
it with deleteDir(), otherwise we just use delete().
 */
files.each{
    it.directory ? it.deleteDir() : it.delete()
}
```



## Haskell



```haskell
import System.IO
import System.Directory

main = do
  removeFile "output.txt"
  removeDirectory "docs"
  removeFile "/output.txt"
  removeDirectory "/docs"
```



## HicEst


```hicest
SYSTEM(DIR="docs")   ! create docs in current directory (if not existent), make it current
OPEN (FILE="input.txt", "NEW")    ! in current directory = docs
WRITE(FIle="input.txt", DELETE=1) ! no command to DELETE a DIRECTORY in HicEst

SYSTEM(DIR="C:\docs")             ! create C:\docs (if not existent), make it current
OPEN (FILE="input.txt", "NEW")    ! in current directory = C:\docs
WRITE(FIle="input.txt", DELETE=1)
```



## Io


```io
Directory fileNamed("input.txt") remove
Directory directoryNamed("docs") remove
RootDir := Directory clone setPath("/")
RootDir fileNamed("input.txt") remove
RootDir directoryNamed("docs") remove
```


or

```io
File with("input.txt") remove
Directory with("docs") remove
File with("/input.txt") remove
Directory with("/docs") remove
```


=={{header|Icon}} and {{header|Unicon}}==
Icon supports 'remove' for files.

```Unicon
every dir := !["./","/"] do {
   remove(f := dir || "input.txt")  |stop("failure for file remove ",f)
   rmdir(f := dir || "docs")        |stop("failure for directory remove ",f)
   }

```

Note Icon and Unicon accept both / and \ for directory separators.


## J

The J standard library comes with a set of file access utilities.

```j
   load 'files'
   ferase 'input.txt'
   ferase '\input.txt'
   ferase 'docs'
   ferase '\docs'

NB. Or all at once...
   ferase 'input.txt';'/input.txt';'docs';'/docs'
```


The function above actually uses a foreign conjunction and defined in the <tt>files</tt> library like so:

```j
NB.
### ===================================================

NB.*ferase v erases a file
NB. Returns 1 if successful, otherwise _1
ferase=: (1!:55 :: _1:) @ (fboxname &>) @ boxopen
```


This means that you can directly erase files and directories without loading the <tt>files</tt> library.

```j
1!:55 <'input.txt'
1!:55 <'\input.txt'
1!:55 <'docs'
1!:55 <'\docs'
```



## Java



```java
import java.io.File;
public class FileDeleteTest {
   public static boolean deleteFile(String filename) {
       boolean exists = new File(filename).delete();
       return exists;
   }
   public static void test(String type, String filename) {
       System.out.println("The following " + type + " called " + filename +
           (deleteFile(filename) ? " was deleted." : " could not be deleted.")
       );
   }
   public static void main(String args[]) {
        test("file", "input.txt");
        test("file", File.seperator + "input.txt");
        test("directory", "docs");
        test("directory", File.seperator + "docs" + File.seperator);
   }
}
```



## JavaScript

{{works with|JScript}}

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");

fso.DeleteFile('input.txt');
fso.DeleteFile('c:/input.txt');

fso.DeleteFolder('docs');
fso.DeleteFolder('c:/docs');
```


or


```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");
var f;
f = fso.GetFile('input.txt');
f.Delete();
f = fso.GetFile('c:/input.txt');
f.Delete();
f = fso.GetFolder('docs');
f.Delete();
f = fso.GetFolder('c:/docs');
f.Delete();
```


{{works with|Node.js}}
Synchronous

```javascript
const fs = require('fs');
fs.unlinkSync('myfile.txt');
```

Asynchronous

```javascript
const fs = require('fs');
fs.unlink('myfile.txt', ()=>{
  console.log("Done!");
})
```



## Julia


```julia

# Delete a file
rm("input.txt")

# Delete a directory
rm("docs", recursive = true)

```



## Kotlin


```scala
// version 1.0.6

/* testing on Windows 10 which needs administrative privileges
   to delete files from the root */

import java.io.File

fun main(args: Array<String>) {
    val paths = arrayOf("input.txt", "docs", "c:\\input.txt", "c:\\docs")
    var f: File
    for (path in paths) {
        f = File(path)
        if (f.delete())
            println("$path successfully deleted")
        else
            println("$path could not be deleted")
    }
}
```


{{out}}

```txt

input.txt successfully deleted
docs successfully deleted
c:\input.txt successfully deleted
c:\docs successfully deleted

```

Running program again after files have been deleted:
{{out}}

```txt

input.txt could not be deleted
docs could not be deleted
c:\input.txt could not be deleted
c:\docs could not be deleted

```



## LabVIEW

{{libheader|LabVIEW_CWD}}
{{VI solution|LabVIEW_Delete_a_file.png}}



## Lasso


```Lasso
// delete file
local(f = file('input.txt'))
#f->delete

// delete directory
// directory must be empty before it can be successfully deleted. A failure is generated if the operation fails.
local(d = dir('docs'))
#d->delete

// delete file in root file system (requires permissions at user OS level)
local(f = file('//input.txt'))
#f->delete

// delete directory in root file system (requires permissions at user OS level)
// directory must be empty before it can be successfully deleted. A failure is generated if the operation fails.
local(d = file('//docs'))
#d->delete
```



## Liberty BASIC


```lb
' show where we are
print DefaultDir$

' in here
kill "input.txt"
result=rmdir("Docs")

' from root
kill "\input.txt"
result=rmdir("\Docs")

```



## Lingo


Delete file "input.txt" in cwd:

```lingo
-- note: fileIO xtra is shipped with Director, i.e. an "internal"
fp = xtra("fileIO").new()
fp.openFile("input.txt", 0)
fp.delete()
```


Delete file "input.txt" in root of current volume:

```lingo
-- note: fileIO xtra is shipped with Director, i.e. an "internal"
pd = the last char of _movie.path -- "\" for win, ":" for mac
_player.itemDelimiter = pd
vol = _movie.path.item[1]
fp = xtra("fileIO").new()
fp.openFile(vol&pd&"input.txt", 0)
fp.delete()
```


Deleting a directory requires a 3rd party xtra, but there are various free xtras that allow this. Here as example usage of BinFile xtra:


```lingo
-- delete (empty) directory "docs" in cwd
bx_folder_delete("docs")

-- delete (empty) directory "docs" in root of current volume
pd = the last char of _movie.path -- "\" for win, ":" for mac
_player.itemDelimiter = pd
vol = _movie.path.item[1]
bx_folder_delete(vol&pd&"docs")
```



## Locomotive Basic



```locobasic
|era,"input.txt"
```

([[wp:AMSDOS|AMSDOS]] RSX command, therefore prefixed with a vertical bar. Also, there are no subdirectories in AMSDOS.)


## Logo

{{works with|UCB Logo}}
UCB Logo has no means to delete directories.

```logo
erasefile "input.txt
erasefile "/input.txt
```



## Lua


```lua
os.remove("input.txt")
os.remove("/input.txt")
os.remove("docs")
os.remove("/docs")
```



## Maple


```Maple
FileTools:-Remove("input.txt");
FileTools:-RemoveDirectory("docs");
FileTools:-Remove("/input.txt");
FileTools:-RemoveDirectory("/docs");

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
wd = NotebookDirectory[];
DeleteFile[wd <> "input.txt"]
DeleteFile["/" <> "input.txt"]
DeleteDirectory[wd <> "docs"]
DeleteDirectory["/" <> "docs"]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
   delete('input.txt');   % delete local file input.txt
   delete('/input.txt');   % delete file /input.txt
   rmdir('docs');    % remove local directory docs
   rmdir('/docs');   % remove directory /docs

```


On Unix-Systems:

```matlab
if system('rm input.txt') == 0
   disp('input.txt removed')
end
if system('rm /input.txt') == 0
   disp('/input.txt removed')
end
if system('rmdir docs') == 0
   disp('docs removed')
end
if system('rmdir /docs') == 0
   disp('/docs removed')
end
```



## MAXScript

There's no way to delete folders in MAXScript

```maxscript
-- Here
deleteFile "input.txt"
-- Root
deleteFile "\input.txt"
```



## Mercury


```mercury
:- module delete_file.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.remove_file("input.txt", _, !IO),
    io.remove_file("/input.txt", _, !IO),
    io.remove_file("docs", _, !IO),
    io.remove_file("/docs", _, !IO).
```



## Nemerle


```Nemerle
using System;
using System.IO;
using System.Console;

module DeleteFile
{
    Main() : void
    {
        when (File.Exists("input.txt")) File.Delete("input.txt");
        try {
            when (File.Exists(@"\input.txt")) File.Delete(@"\input.txt");
        }
        catch {
            |e is UnauthorizedAccessException => WriteLine(e.Message)
        }

        when (Directory.Exists("docs")) Directory.Delete("docs");
        when (Directory.Exists(@"\docs")) Directory.Delete(@"\docs");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method isFileDeleted(fn) public static returns boolean
  ff = File(fn)
  fDeleted = ff.delete()
  return fDeleted

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg files
  if files = '' then files = 'input.txt F docs D /input.txt F /docs D'
  loop while files.length > 0
    parse files fn ft files
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
    if isFileDeleted(fn) then dl = 'deleted'
    else                      dl = 'not deleted'
    say ft ''''fn'''' dl
    end

  return

```



## NewLISP


```NewLISP
(delete-file "input.txt")
(delete-file "/input.txt")
(remove-dir "docs")
(remove-dir "/docs")
```



## Nim


```Nim
import os
removeFile("input.txt")
removeFile("/input.txt")
removeDir("docs")
removeDir("/docs")
```


=={{header|Objective-C}}==


```objc
NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
[fm removeFileAtPath:@"input.txt" handler:nil];
[fm removeFileAtPath:@"/input.txt" handler:nil];
[fm removeFileAtPath:@"docs" handler:nil];
[fm removeFileAtPath:@"/docs" handler:nil];

// OS X 10.5+
[fm removeItemAtPath:@"input.txt" error:NULL];
[fm removeItemAtPath:@"/input.txt" error:NULL];
[fm removeItemAtPath:@"docs" error:NULL];
[fm removeItemAtPath:@"/docs" error:NULL];
```



## Objeck


```objeck

use IO;

bundle Default {
  class FileExample {
    function : Main(args : String[]) ~ Nil {
      File->Delete("output.txt");
      File->Delete("/output.txt");

      Directory->Delete("docs");
      Directory->Delete("/docs");
    }
  }
}

```



## OCaml



```ocaml
Sys.remove "input.txt";;
Sys.remove "/input.txt";;
```


with the Unix library:

```ocaml
#load "unix.cma";;
Unix.unlink "input.txt";;
Unix.unlink "/input.txt";;
Unix.rmdir "docs";;
Unix.rmdir "/docs";;
```



## ooRexx


```oorexx
/*REXX pgm deletes a file */
file= 'afile.txt'                     /*name of a file  to be deleted.*/
res=sysFileDelete(file); Say file 'res='res
File= 'bfile.txt'                     /*name of a file  to be deleted.*/
res=sysFileDelete(file); Say file 'res='res
```

{{out}}

```txt
afile.txt res=0
bfile.txt res=2
```



## Oz


```oz
for Dir in ["/" "./"] do
   try {OS.unlink Dir#"output.txt"}
   catch _ then {System.showInfo "File does not exist."} end
   try {OS.rmDir Dir#"docs"}
   catch _ then {System.showInfo "Directory does not exist."} end
end
```



## PARI/GP

GP has no built-in facilities for deleting files, but can use a system call:

```parigp
system("rm -rf docs");
system("rm input.txt");
system("rm -rf /docs");
system("rm /input.txt");
```

PARI, as usual, has access to all the standard [[#C|C]] methods.


## Pascal

''See [[#Delphi|Delphi]] or [[#Free Pascal|Free Pascal]]''


## Perl



```perl
use File::Spec::Functions qw(catfile rootdir);
# here
unlink 'input.txt';
rmdir 'docs';
# root dir
unlink catfile rootdir, 'input.txt';
rmdir catfile rootdir, 'docs';
```


'''Without Perl Modules'''

Current directory
 perl -e 'unlink input.txt'
 perl -e 'rmdir docs'

Root Directory
 perl -e 'unlink "/input.txt"'
 perl -e 'rmdir "/docs"'


## Perl 6


```perl6
unlink 'input.txt';
unlink '/input.txt';
rmdir 'docs';
rmdir '/docs';
```



## Phix


```Phix
constant root = iff(platform()=LINUX?"/":"C:\\")
?delete_file("input.txt")
?delete_file(root&"input.txt")
?remove_directory("docs")
?remove_directory(root&"docs")
```

output is 0 0 0 0 or 1 1 1 1 or some combination thereof


## PHP


```php
<?php
unlink('input.txt');
unlink('/input.txt');
rmdir('docs');
rmdir('/docs');
?>
```



## PicoLisp


```PicoLisp
(call 'rm "input.txt")
(call 'rmdir "docs")
(call 'rm "/input.txt")
(call 'rmdir "/docs")
```



## Pike


```pike
int main(){
   rm("input.txt");
   rm("/input.txt");
   rm("docs");
   rm("/docs");
}
```



## PowerShell


```powershell
# possible aliases for Remove-Item: rm, del, ri
Remove-Item input.txt
Remove-Item \input.txt  # file system root

Remove-Item -Recurse docs  # recurse for deleting folders including content
Remove-Item -Recurse \docs
```



## ProDOS

Because input.txt is located inside of "docs" this will delete it when it deletes "docs"

```ProDOS>deletedirectory docs</lang



## PureBasic


```PureBasic
DeleteFile("input.txt")
DeleteDirectory("docs","")  ; needs to delete all included files
DeleteFile("/input.txt")
DeleteDirectory("/docs","*.*")  ; deletes all files according to a pattern

DeleteDirectory("/docs","",#PB_FileSystem_Recursive)  ; deletes all files and directories recursive
```



## Python



```python
import os
# current directory
os.remove("output.txt")
os.rmdir("docs")
# root directory
os.remove("/output.txt")
os.rmdir("/docs")
```

If you wanted to remove a directory and all its contents, recursively, you would do:

```python
import shutil
shutil.rmtree("docs")
```



## R


```R
file.remove("input.txt")
file.remove("/input.txt")

# or
file.remove("input.txt", "/input.txt")

# or
unlink("input.txt"); unlink("/input.txt")

# directories needs the recursive flag
unlink("docs", recursive = TRUE)
unlink("/docs", recursive = TRUE)
```


The function <tt>unlink</tt> allows wildcards (* and ?)


## Racket



```Racket

#lang racket

;; here
(delete-file "input.txt")
(delete-directory "docs")
(delete-directory/files "docs") ; recursive deletion

;; in the root
(delete-file "/input.txt")
(delete-directory "/docs")
(delete-directory/files "/docs")

;; or in the root with relative paths
(parameterize ([current-directory "/"])
  (delete-file "input.txt")
  (delete-directory "docs")
  (delete-directory/files "docs"))

```



## Raven



```raven
'input.txt'  delete
'/input.txt' delete
'docs'  rmdir
'/docs' rmdir
```



## REBOL


```REBOL
; Local.
delete %input.txt
delete-dir %docs/

; Root.
delete %/input.txt
delete-dir %/docs/
```



## Retro


```Retro
with files'
"input.txt" delete
"/input.txt" delete
```



## REXX

Note that this REXX program will work on the Next family of Microsoft Windows systems as well as DOS   (both under Windows in a DOS-prompt window  or stand-alone DOS).

```rexx
/*REXX program deletes a file and a folder in the  current directory  and  the root.    */
trace off                                        /*suppress REXX error messages from DOS*/
aFile= 'input.txt'                               /*name of a  file  to be deleted.      */
aDir = 'docs'                                    /*name of a folder to be removed.      */
               do j=1  for 2                     /*perform this  DO  loop exactly twice.*/
               'ERASE'  aFile                    /*erase this  file in the current dir. */
               'RMDIR'  "/s /q"  aDir            /*remove the folder "  "     "     "   */
               if j==1  then 'CD \'              /*make the  current dir  the  root dir.*/
               end                               /* [↑]  just do   CD \    command once.*/
                                                 /*stick a fork in it,  we're all done. */
```



## Ring



```ring

remove("output.txt")
system("rmdir docs")

```



## Ruby



```ruby
File.delete("output.txt", "/output.txt")
Dir.delete("docs")
Dir.delete("/docs")
```



## Run BASIC


```runbasic
'------ delete input.txt ----------------
kill "input.txt"   ' this is where we are
kill "/input.txt"  ' this is the root

' ---- delete directory docs ----------
result = rmdir("Docs")  ' directory where we are
result = rmdir("/Docs") ' root directory
```



## Rust


```rust
use std::io::{self, Write};
use std::fs::{remove_file,remove_dir};
use std::path::Path;
use std::{process,display};

const FILE_NAME: &'static str = "output.txt";
const DIR_NAME : &'static str = "docs";

fn main() {
    delete(".").and(delete("/"))
               .unwrap_or_else(|e| error_handler(e,1));
}


fn delete<P>(root: P) -> io::Result<()>
    where P: AsRef<Path>
{
    remove_file(root.as_ref().join(FILE_NAME))
        .and(remove_dir(root.as_ref().join(DIR_NAME)))
}

fn error_handler<E: fmt::Display>(error: E, code: i32) -> ! {
    let _ = writeln!(&mut io::stderr(), "{:?}", error);
    process::exit(code)
}
```



## Scala

{{libheader|Scala}}

```scala
import java.util._
import java.io.File

object FileDeleteTest extends App {
  def deleteFile(filename: String) = { new File(filename).delete() }

  def test(typ: String, filename: String) = {
    System.out.println("The following " + typ + " called " + filename +
      (if (deleteFile(filename)) " was deleted." else " could not be deleted."))
  }
  test("file", "input.txt")
  test("file", File.separatorChar + "input.txt")
  test("directory", "docs")
  test("directory", File.separatorChar + "docs" + File.separatorChar)
}
```



## Scheme

{{works with|Scheme|R6RS}}[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-10.html]

```scheme
(delete-file filename)
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/osfiles.htm osfiles.s7i] provides the functions [http://seed7.sourceforge.net/libraries/osfiles.htm#removeFile%28in_string%29 removeFile] and [http://seed7.sourceforge.net/libraries/osfiles.htm#removeTree%28in_string%29 removeTree]. RemoveFile removes a file of any type unless it is a directory that is not empty. RemoveTree remove a file of any type inclusive a directory tree. Note that removeFile and removeTree fail with the exception [http://seed7.sourceforge.net/manual/errors.htm#FILE_ERROR FILE_ERROR] when the file does not exist.

```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  begin
    removeFile("input.txt");
    removeFile("/input.txt");
    removeTree("docs");
    removeTree("/docs");
  end func;
```



## Sidef


```ruby
# here
%f'input.txt' -> delete;
%d'docs'      -> delete;

# root dir
Dir.root + %f'input.txt' -> delete;
Dir.root + %d'docs'      -> delete;
```



## Slate


(It will succeed deleting the directory if it is empty)


```slate
(File newNamed: 'input.txt') delete.
(File newNamed: '/input.txt') delete.
(Directory newNamed: 'docs') delete.
(Directory newNamed: '/docs') delete.
```


Also:


```slate

(Directory current / 'input.txt') delete.
(Directory root / 'input.txt') delete.
```



## Smalltalk


(It will succeed deleting the directory if it is empty)


```smalltalk
File remove: 'input.txt'.
File remove: 'docs'.
File remove: '/input.txt'.
File remove: '/docs'
```



## Standard ML



```sml
OS.FileSys.remove "input.txt";
OS.FileSys.remove "/input.txt";
OS.FileSys.rmDir "docs";
OS.FileSys.rmDir "/docs";
```



## Stata


```stata
erase input.txt
rmdir docs
```



## Tcl



```tcl
file delete input.txt /input.txt

# preserve directory if non-empty
file delete docs /docs

# delete even if non-empty
file delete -force docs /docs
```



## Toka



```toka
needs shell
" docs" remove
" input.txt" remove
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
- delete file
SET status = DELETE ("input.txt")
- delete directory
SET status = DELETE ("docs",-std-)

```



## UNIX Shell


```bash
rm -rf docs
rm input.txt
rm -rf /docs
rm /input.txt
```



## Ursa


```ursa
decl file f
f.delete "input.txt"
f.delete "docs"
f.delete "/input.txt"
f.delete "/docs"
```



## VAX Assembly


```VAX Assembly
74 75 70 6E 69 20 65 74 65 6C 65 64  0000     1 dcl:	.ascii	"delete input.txt;,docs.dir;"
64 2E 73 63 6F 64 2C 3B 74 78 74 2E  000C
                           3B 72 69  0018
69 76 65 64 73 79 73 24 73 79 73 2C  001B     2 	.ascii	",sys$sysdevice:[000000]input.txt;"
69 5D 30 30 30 30 30 30 5B 3A 65 63  0027
         3B 74 78 74 2E 74 75 70 6E  0033
69 76 65 64 73 79 73 24 73 79 73 2C  003C     3 	.ascii	",sys$sysdevice:[000000]docs.dir;"
64 5D 30 30 30 30 30 30 5B 3A 65 63  0048
            3B 72 69 64 2E 73 63 6F  0054
                                     005C     4
                           0000005C  005C     5 desc:	.long	.-dcl					;character count
                           00000000' 0060     6 	.address dcl
                                     0064     7
                               0000  0064     8 .entry	main,0
                         F3 AF   7F  0066     9 	pushaq	desc
              00000000'GF   01   FB  0069    10 	calls	#1, g^lib$do_command			;execute shell command
                                 04  0070    11 	ret
                                     0071    12 .end	main


```



## Vedit macro language

Vedit allows using either '\' or '/' as directory separator character, it is automatically converted to the one used by the operating system.

```vedit
// In current directory
File_Delete("input.txt", OK)
File_Rmdir("docs")

// In the root directory
File_Delete("/input.txt", OK)
File_Rmdir("/docs")
```



## VBA


```vb
Option Explicit

Sub DeleteFileOrDirectory()
Dim myPath As String
    myPath = "C:\Users\surname.name\Desktop\Docs"
'delete file
    Kill myPath & "\input.txt"
'delete Directory
    RmDir myPath
End Sub
```



## VBScript


```vb
Set oFSO = CreateObject( "Scripting.FileSystemObject" )

oFSO.DeleteFile "input.txt"
oFSO.DeleteFolder "docs"

oFSO.DeleteFile "\input.txt"
oFSO.DeleteFolder "\docs"

'Using Delete on file and folder objects

dim fil, fld

set fil = oFSO.GetFile( "input.txt" )
fil.Delete
set fld = oFSO.GetFolder( "docs" )
fld.Delete

set fil = oFSO.GetFile( "\input.txt" )
fil.Delete
set fld = oFSO.GetFolder( "\docs" )
fld.Delete


```



## Visual Basic .NET

'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
'Current Directory
IO.Directory.Delete("docs")
IO.Directory.Delete("docs", True) 'also delete files and sub-directories
IO.File.Delete("output.txt")

'Root
IO.Directory.Delete("\docs")
IO.File.Delete("\output.txt")

'Root, platform independent
IO.Directory.Delete(IO.Path.DirectorySeparatorChar & "docs")
IO.File.Delete(IO.Path.DirectorySeparatorChar & "output.txt")
```



## X86 Assembly

{{works with|NASM|Linux}}


```asm

;syscall numbers for readability. :]
%define sys_rmdir 40
%define sys_unlink 10

section .text
	global _start

	_start:
		mov ebx, fn
		mov eax, sys_unlink
		int 0x80
		test eax, eax
		js _ragequit

		mov ebx, dn
		mov eax, sys_rmdir
		int 0x80

		mov ebx, rfn
		mov eax, sys_unlink
		int 0x80
		cmp eax, 0
		je _exit

		_ragequit:
			mov edx, err_len
			mov ecx, err_msg
			mov ebx, 4
			mov eax ,1
			int 0x80

		_exit:
			push 0x1
			mov eax, 1
			push eax
			int 0x80
			ret

section .data
fn		db 'input.txt',0
rfn		db '/input.txt',0
dn		db 'doc',0

err_msg		db "Something went wrong! :[",0xa
err_len		equ $-err_msg

```



## Yorick

Yorick does not have a built-in function to recursively delete a directory; the rmdir function only works on empty directories.

```yorick
remove, "input.txt";
remove, "/input.txt";
rmdir, "docs";
rmdir, "/docs";
```



## zkl

zkl doesn't have built ins to delete files or directories but you can let a shell do it:

```zkl
zkl: System.cmd((System.isWindows and "del" or "unlink") + " input.txt")
0
zkl: System.cmd((System.isWindows and "del" or "unlink") + " /input.txt")
unlink: cannot unlink ‘/input.txt’: No such file or directory
256
zkl: System.cmd("rmdir docs")
rmdir: failed to remove ‘docs’: Directory not empty
256
zkl: System.cmd("rm -r docs")
0
zkl: System.cmd("rm -r /docs")
rm: cannot remove ‘/docs’: No such file or directory
256
```


{{omit from|Befunge}} <!-- No filesystem support -->
{{omit from|HTML}}
{{omit from|Openscad}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
