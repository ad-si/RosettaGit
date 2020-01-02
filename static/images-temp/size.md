+++
title = "File size"
description = ""
date = 2019-10-15T10:07:08Z
aliases = []
[extra]
id = 2028
[taxonomies]
categories = []
tags = []
+++

{{task|File System Operations}}

Verify the size of a file called     '''input.txt'''     for a file in the current working directory, and another one in the file system root.





## Ada


```ada
with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

procedure Test_File_Size is
begin
   Put_Line (File_Size'Image (Size ("input.txt")) & " bytes");
   Put_Line (File_Size'Image (Size ("/input.txt")) & " bytes");
end Test_File_Size;
```

Note that reference to the root directory, if there is any, is [[OS]] specific.


## Aime


```aime
o_(stat("input.txt", ST_SIZE), "\n");
o_("/Cygwin.ico".stat(ST_SIZE), "\n");
```



## ALGOL 68

There is no build in way to find the size of an arbitrary file, especially of the file is a
special channel, e.g. a tape device.

Conceptually the procedure
```algol68
PROC set = (REF FILE file,  INT page, line, character)VOID: ~
```

could be used to do a binary search find the last page's page number.  And if it is known
that every page has the same number of lines, and every line has the same number of '''char'''[s],
and the character set is not ''compressible'', then the size could be quickly calculated.
Otherwise every page, and every line would have to be tallied.

It is probably much easier to use some an operating system library.  This library is not part of
the standard ALGOL 68 language definition.


## Arturo



```arturo
file "input.txt"

print file + " size: " + $(filesize file) + " bytes"
```


{{out}}


```txt
input.txt size: 37 bytes
```



## AutoHotkey


```AutoHotkey
FileGetSize, FileSize, input.txt  ; Retrieve the size in bytes.
MsgBox, Size of input.txt is %FileSize% bytes
FileGetSize, FileSize, \input.txt, K  ; Retrieve the size in Kbytes.
MsgBox, Size of \input.txt is %FileSize% Kbytes
```



## AWK

{{works with|gawk}}

```awk
@load "filefuncs"
function filesize(name         ,fd) {
    if ( stat(name, fd) == -1)
      return -1  # doesn't exist
    else
      return fd["size"]
}
BEGIN {
    print filesize("input.txt")
    print filesize("/input.txt")
}
```


Some awk's don't have direct access to the filesystem, but can execute system-commands like dir (DOS/Windows) and ls


```awk
BEGIN {

         # Windows
          printf("input.txt\t%s\n", system2var("for %I in (input.txt) do @echo %~zI"))
          printf("\input.txt\t%s\n", system2var("for %I in (\input.txt) do @echo %~zI"))

         # Non-Windows
          printf("input.txt\t%s\n", getline2var("stat --printf=\"%s\" input.txt"))
          printf("/input.txt\t%s\n", getline2var("stat --printf=\"%s\" /input.txt"))
}

# Windows system() method
function system2var(command    ,tempfile, cmd, out, rec, data, i) {
         tempfile = "C:\\TEMP\\TMP.TMP"
         cmd = command " > " tempfile
         system(cmd)
         close(cmd)
         while (getline rec < tempfile > 0) {
             if ( ++i == 1 )
                 data = rec
             else
                 data = data "\n" rec
         }
         return(data)
}

# Non-windows getline method
function getline2var(command        ,fish, scale, ship) {
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



See also [http://rosettacode.org/wiki/File_size#UNIX_Shell UNIX_Shell]]


## Axe


```axe
If GetCalc("appvINPUT")→I
 Disp {I-2}ʳ▶Dec,i
Else
 Disp "NOT FOUND",i
End
```



## BaCon


```freebasic
' file size
' Return the entire message, FILELEN returns a NUMBER
FUNCTION printlen$(STRING name$)
    IF FILEEXISTS(name$) THEN
        RETURN name$ & ": " & STR$(FILELEN(name$))
    ELSE
        RETURN "file " & name$ & " not found"
    END IF
END FUNCTION

PRINT printlen$("input.txt")
PRINT printlen$("/input.txt")
```


{{out}}

```txt

prompt$ bacon filelen.bac
Converting 'filelen.bac'... done, 12 lines were processed in 0.004 seconds.
Compiling 'filelen.bac'... cc  -c filelen.bac.c
cc -o filelen filelen.bac.o -lbacon -lm
Done, program 'filelen' ready.
prompt$ ./filelen
input.txt: 15
file /input.txt not found
```



## Batch File

Outputs file size of the first parameter (you can drag and drop a file in aswell).

```dos

@echo off
if not exist "%~1" exit /b 1 & rem If file doesn't exist exit with error code of 1.
for /f %%i in (%~1) do echo %~zi
pause>nul

```



## BBC BASIC


```bbcbasic
      file% = OPENIN(@dir$+"input.txt")
      IF file% THEN
        PRINT "File size = " ; EXT#file%
        CLOSE #file%
      ENDIF

      file% = OPENIN("\input.txt")
      IF file% THEN
        PRINT "File size = " ; EXT#file%
        CLOSE #file%
      ENDIF
```



## Bracmat

This solution assumes that the file can be opened for reading. The <code>fil</code> function is the Bracmat interface to the underlying C functions <code>fopen, fclose, fseek, ftell, fread, fgetc, fwrite, fputc</code> and <code>feof</code>. More than one file can be opened at the same time. Focus is shifted from one open file to another by mentioning the file name as the first argument.


```bracmat
(getFileSize=
  size
.     fil$(!arg,rb)     {read in binary mode}
    & fil$(,END)        {seek to end of file}
    & fil$(,TEL):?size  {tell where we are}
    & fil$(,SET,-1)     {seeking to an impossible position closes the file, and fails}
  | !size               {return the size}
);

getFileSize$"valid.bra"
113622

getFileSize$"c:\\boot.ini"
211

```



## C


```cpp
#include <iostream>
#include <stdio.h>

long getFileSize(const char *filename)
{
  long result;
  FILE *fh = fopen(filename, "rb");
  fseek(fh, 0, SEEK_END);
  result = ftell(fh);
  fclose(fh);
  return result;
}

int main(void)
{
  printf("%ld\n", getFileSize("input.txt"));
  printf("%ld\n", getFileSize("/input.txt"));
  return 0;
}
```


{{works with|POSIX}}

```cpp
#include <iostream>
#include <stdio.h>
#include <sys/stat.h>

int main(void)
{
  struct stat foo;
  stat("input.txt", &foo);
  printf("%ld\n", foo.st_size);
  stat("/input.txt", &foo);
  printf("%ld\n", foo.st_size);
  return 0;
}
```



## C++


```cpp
#include <iostream>
#include <fstream>

std::ios::off_type getFileSize(const char *filename) {
  std::ifstream f(filename);
  std::ios::pos_type begin = f.tellg();
  f.seekg(0, std::ios::end);
  std::ios::pos_type end = f.tellg();
  return end - begin;
}

int main() {
  std::cout << getFileSize("input.txt") << std::endl;
  std::cout << getFileSize("/input.txt") << std::endl;
  return 0;
}
```


'''optimized '''

```cpp
#include <iostream>
#include <fstream>

int main()
{
	std::cout << std::ifstream("input.txt", std::ios::binary | std::ios::ate).tellg() << "\n"
		  << std::ifstream("/input.txt", std::ios::binary | std::ios::ate).tellg() << "\n";
}
```


## C#


```c#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine(new FileInfo("/input.txt").Length);
        Console.WriteLine(new FileInfo("input.txt").Length);
    }
}

```



## Clean

There is not function to get the file size, therefore we seek to the end and query the file pointer position.


```clean
import StdEnv

fileSize fileName world
    # (ok, file, world) = fopen fileName FReadData world
    | not ok = abort "Cannot open file"
    # (ok, file) = fseek file 0 FSeekEnd
    | not ok = abort "Cannot seek file"
    # (size, file) = fposition file
      (_, world) = fclose file world
    = (size, world)

Start world = fileSize "input.txt" world
```



## Clojure


```clojure
(require '[clojure.java.io :as io])
(defn show-size [filename]
  (println filename "size:" (.length (io/file filename))))

(show-size "input.txt")
(show-size "/input.txt")
```



## COBOL


```Cobol

       identification division.
       program-id. FileInfo.

       data division.
       working-storage section.
       01  file-name              pic x(256).
       01  file-size-edited       pic zzz,zzz,zzz.
       01  file-details.
           05 file-size           pic x(8) comp-x.
           05 file-date.
              10 file-day         pic x comp-x.
              10 file-month       pic x comp-x.
              10 file-year        pic xx comp-x.
           05 file-time.
              10 file-hour        pic x comp-x.
              10 file-minute      pic x comp-x.
              10 file-second      pic x comp-x.
              10 file-hundredths  pic x comp-x.

       procedure division.
       main.
           move "input.txt" to file-name
           perform file-info

           move "\input.txt" to file-name
           perform file-info

           stop run
           .

       file-info.
           call "CBL_CHECK_FILE_EXIST"
              using file-name, file-details
              returning return-code
           if return-code = 0
              move file-size to file-size-edited
              display function trim(file-name) " "
                      function trim(file-size-edited) " Bytes"
           else
              display function trim(file-name) " not found!"
           end-if
           .

```



## ColdFusion


```ColdFusion><cfscript

  localFile = getFileInfo(expandpath("input.txt"));
  rootFile = getFileInfo("/input.txt");
</cfscript>

<cfoutput>
  Size of input.txt is #localFile.size# bytes.
  Size of /input.txt is #rootFile.size# bytes.
</cfoutput>
```



## Common Lisp


```lisp
(with-open-file (stream (make-pathname :name "input.txt")
                 :direction :input
                 :if-does-not-exist nil)
  (print (if stream (file-length stream) 0)))

(with-open-file (stream (make-pathname :directory '(:absolute "") :name "input.txt")
                 :direction :input
                 :if-does-not-exist nil)
  (print (if stream (file-length stream) 0)))
```


  (osicat-posix:stat-size (osicat-posix:stat #P"input.txt"))


## D


```d
import std.file, std.stdio, std.path, std.file, std.stream,
       std.mmfile;

void main() {
    immutable fileName = "file_size.exe";

    try {
        writefln("File '%s' has size:", fileName);

        writefln("%10d bytes by std.file.getSize (function)",
                 std.file.getSize(fileName));

        writefln("%10d bytes by std.stream (class)",
                 new std.stream.File(fileName).size);

        // mmfile can treat the file as an array in memory.
        writefln("%10d bytes by std.mmfile (class)",
                 new std.mmfile.MmFile(fileName).length);
    } catch (Exception e) {
        e.msg.writefln;
    }
}
```

{{out}}

```txt
File 'file_size.exe' has size:
   1066164 bytes by std.file.getSize (function)
   1066164 bytes by std.stream (class)
   1066164 bytes by std.mmfile (class)
```



## Delphi


```Delphi
program SizeOfFile;

{$APPTYPE CONSOLE}

uses SysUtils;

function CheckFileSize(const aFilename: string): Integer;
var
  lFile: file of Byte;
begin
  AssignFile(lFile, aFilename);
  FileMode := 0; {Access file in read only mode}
  Reset(lFile);
  Result := FileSize(lFile);
  CloseFile(lFile);
end;

begin
  Writeln('input.txt ', CheckFileSize('input.txt'));
  Writeln('\input.txt ', CheckFileSize('\input.txt'));
end.
```



## E



```e>for file in [<file:input.txt
, <file:///input.txt>] {
  println(`The size of $file is ${file.length()} bytes.`)
}
```



## Eiffel



```eiffel

class
    APPLICATION
create
    make
feature {NONE} -- Initialization
    make
            -- Run application.
        do
            create input_file.make_open_read ("input.txt")
            print(input_file.count)
            print("%N")
            input_file.close
            create environment
            input_file.make_open_read(environment.root_directory_name + "input.txt")
            print(input_file.count)
            input_file.close
        end
feature -- Access
    input_file: PLAIN_TEXT_FILE
    environment:EXECUTION_ENVIRONMENT
end

```



## Elena

ELENA 4.x :

```elena
import system'io;
import extensions;

public program()
{
    console.printLine(File.assign("input.txt").Length);

    console.printLine(File.assign("\input.txt").Length)
}
```



## Elixir


```elixir
IO.puts File.stat!("input.txt").size
IO.puts File.stat!("/input.txt").size
```



## Emacs Lisp

This shows <code>nil</code> if no such file since
<code>file-attributes</code> returns <code>nil</code> in that case.


```Lisp
(message "sizes are %s and %s"
         (nth 7 (file-attributes "input.txt"))
         (nth 7 (file-attributes "/input.txt")))
```



## Erlang



```erlang
-module(file_size).
-export([file_size/0]).

-include_lib("kernel/include/file.hrl").

file_size() ->
    print_file_size("input.txt"),
    print_file_size("/input.txt").

print_file_size(Filename) ->
    case file:read_file_info(Filename) of
	{ok, FileInfo} ->
	    io:format("~s ~p~n", [Filename, FileInfo#file_info.size]);
	{error, _} ->
	    io:format("~s could not be opened~n",[Filename])
    end.

```



## Euphoria


```euphoria
include file.e

function file_size(sequence file_name)
    object x
    x = dir(file_name)
    if sequence(x) and length(x) = 1 then
        return x[1][D_SIZE]
    else
        return -1 -- the file does not exist
    end if
end function

procedure test(sequence file_name)
    integer size
    size = file_size(file_name)
    if size < 0 then
        printf(1,"%s file does not exist.\n",{file_name})
    else
        printf(1,"%s size is %d.\n",{file_name,size})
    end if
end procedure

test("input.txt") -- in the current working directory
test("/input.txt") -- in the file system root
```



## Factor


```factor
"input.txt" file-info size>> .
    1321
"file-does-not-exist.txt" file-info size>>
 "Unix system call ``stat'' failed:"...
```



## FBSL

FileLen returns -1 if the file is not found. FileLen will also accept a file handle and give the file length of the open file.

```qbasic
#APPTYPE CONSOLE

print FileLen("sync.log")
print FileLen("\sync.log")
PAUSE

```



## Forth



```forth
: .filesize ( addr len -- ) 2dup type ."  is "
  r/o open-file throw
  dup file-size throw  <# #s #> type ."  bytes long." cr
  close-file throw ;

 s" input.txt" .filesize
s" /input.txt" .filesize
```



## Fortran


Since Fortran 95 the size of standard external files may be determined simply by using INQUIRE(SIZE=...).
The following previous example pertains to FORTRAN 77 and is now superceded.

```Fortran


    use :: iso_fortran_env, only : FILE_STORAGE_SIZE
    implicit none
    character(len=*),parameter :: filename(*)=[character(len=256) :: 'input.txt', '/input.txt']
    integer                    :: file_size, i
    do i=1,size(filename)
       INQUIRE(FILE=filename(i), SIZE=file_size)  ! return -1 if cannot determine file size
       write(*,*)'size of file '//trim(filename(i))//' is ',file_size * FILE_STORAGE_SIZE /8,' bytes'
    enddo
    end

```


The original example, now obsolete ...

Alas, although there is a statement <code>INQUIRE(FILE="Input.txt",EXIST = ISTHERE, RECL = RL, ''etc.'')</code> whereby a logical variable ISTHERE has a value (output: assigned left-to-right) according to whether a named file (input: assignment right-to-left) exists or not, and the parameter RECL returns the maximum allowed record length for the file, there is no parameter that reports how many records there are in the file so that the file size remains unknowable. Further, the value returned by RECL is not necessarily related to the file itself, but is likely to be a standard value such as 132, a default used when deciding on the line wrap length with free-format output as in <code>WRITE (6,*) stuff</code> but not necessarily being a limit on the length of a line written or read.

Further, in the ASCII world, text files are often implemented as variable-length records with special characters inline as record boundaries, usually one of CR, CRLF, LFCR, or LF. Without knowing which is in use, the storage taken up by them would be unknown. Other file systems may offer different types of disc files with fixed-size records or variable length records with a record length counter, but this is not standard across all computers.

In other words, Fortran does not specify a linkage to the filesystem whereby these details could be revealed, and not all filesystems maintain them anyway.

But if one wrote Fortran on a B6700 system, its F77 compiler offered additional attributes that could be returned via an INQUIRE statement: MAXRECSIZE really was the length of the longest record in the disc file (whether fixed record lengths or variable record lengths), BLOCKSIZE reported the number of records per block of disc space, AREASIZE the size of a disc space allocation area, and AREAS their number, while KIND reveals the code number of the type of file (not via a .txt suffix or whatever). Armed with these values, the file size could be determined in bits, bytes, words (six characters/word), records, blocks and areas.

These facilities were not carried forward into standardised Fortran 90, etc. So, one is stuck with devising a routine that reads all the records of a disc file, counting their length. This is straightforward, but tedious, as in the following fragment:
```Fortran
   20     READ (INF,21, END = 30) L	!R E A D  A  R E C O R D - but only its length.
   21     FORMAT(Q)			!This obviously indicates the record's length.
          NRECS = NRECS + 1	!CALL LONGCOUNT(NRECS,1)	!C O U N T  A  R E C O R D.
          NNBYTES = NNBYTES + L	!CALL LONGCOUNT(NNBYTES,L)	!Not counting any CRLF (or whatever) gibberish.
          IF (L.LT.RMIN) THEN		!Righto, now for the record lengths.
            RMIN = L			!This one is shorter.
            RMINR = NRECS		!Where it's at.
          ELSE IF (L.GT.RMAX) THEN	!Perhaps instead it is longer?
            RMAX = L			!Longer.
            RMAXR = NRECS		!Where it's at.
          END IF			!So much for the lengths.
          GO TO 20			!All I wanted to know...
```

The LONGCOUNT routine uses two 32-bit integers (the first parameter being a two-element array) to deliver a much larger capacity, given modern file size opportunities, but this is unnecessary if INTEGER*8 variables are available. The count will not include any contribution from record splitters such as CR, etc. A file more properly thought of as containing binary data (say, integer or floating-point values) will by chance have a CR or LF bit pattern here and there, and they will be taken as marking record splits when reading a file as being FORMATTED, which is the default setting.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#include "file.bi"

Print FileLen("input.txt"), FileLen(Environ("SystemRoot") + "\input.txt")
Sleep
```


=={{header|F_Sharp|F#}}==


```fsharp
open NUnit.Framework
open FsUnit

[<Test>]
let ``Validate that the size of the two files is the same`` () =
  let local = System.IO.FileInfo(__SOURCE_DIRECTORY__ + "\input.txt")
  let root = System.IO.FileInfo(System.IO.Directory.GetDirectoryRoot(__SOURCE_DIRECTORY__) + "input.txt")
  local.Length = root.Length |> should be True
```



## Gambas


```gambas
Public Sub Main()
Dim stInfo As Stat = Stat(User.home &/ "input.txt")
Dim stInfo1 As Stat = Stat("/input.txt")

Print User.Home &/ "input.txt = " & stInfo.Size & " bytes"
Print "/input.txt = " & stInfo1.Size & " bytes"

End
```

Output:

```txt

/home/charlie/input.txt = 121 bytes
/input.txt = 32 bytes

```



## Go


```go
package main

import "fmt"
import "os"

func printFileSize(f string) {
    if stat, err := os.Stat(f); err != nil {
        fmt.Println(err)
    } else {
        fmt.Println(stat.Size())
    }
}

func main() {
    printFileSize("input.txt")
    printFileSize("/input.txt")
}
```



## Groovy


```groovy
println new File('index.txt').length();
println new File('/index.txt').length();
```



## Haskell



```haskell
import System.IO

printFileSize filename = withFile filename ReadMode hFileSize >>= print

main = mapM_ printFileSize ["input.txt", "/input.txt"]
```

or

```haskell
import System.Posix.File

printFileSize filename = do stat <- getFileStatus filename
                            print (fileSize stat)

main = mapM_ printFileSize ["input.txt", "/input.txt"]
```



## HicEst


```hicest
READ(FILE="input.txt", LENgth=bytes) ! bytes = -1 if not existent
READ(FILE="C:\input.txt", LENgth=bytes) ! bytes = -1 if not existent
```


=={{header|Icon}} and {{header|Unicon}}==
Icon doesn't support 'stat'; however, information can be obtained by use of the system function to access command line.

```Unicon
every dir := !["./","/"] do {
   write("Size of ",f := dir || "input.txt"," = ",stat(f).size)  |stop("failure for to stat ",f)
   }
```

Note: Icon and Unicon accept both / and \ for directory separators.


## J


```J
require 'files'
fsize 'input.txt';'/input.txt'
```



## Java


```java
import java.io.File;

public class FileSize
{
    public static void main ( String[] args )
    {
        System.out.println("input.txt  : " + new File("input.txt").length() + " bytes");
        System.out.println("/input.txt : " + new File("/input.txt").length() + " bytes");
    }
}

```



## JavaScript

{{works with|JScript}}

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");
fso.GetFile('input.txt').Size;
fso.GetFile('c:/input.txt').Size;
```


The following works in all browsers, including IE10.

```javascript
var file = document.getElementById("fileInput").files.item(0); //a file input element
if (file) {
	var reader = new FileReader();
	reader.readAsText(file, "UTF-8");
	reader.onload = loadedFile;
	reader.onerror = errorHandler;
}
function loadedFile(event) {
	var fileString = event.target.result;
	alert(fileString.length);
}
function errorHandler(event) {
	alert(event);
}
```



## jq


```sh
jq -Rs length input.txt

jq -Rs length /input.txt
```


The -R option causes the file to be read as text, and the -s option causes it to be read as a single string.


## Julia


```julia
println(filesize("input.txt"))
println(filesize("/input.txt"))
```



## K


```K
_size "input.txt"
_size "/input.txt"
```



## Kotlin


```scala
// version 1.0.6

import java.io.File

fun main(args: Array<String>) {
    val paths = arrayOf("input.txt", "c:\\input.txt")
    for (path in paths)
        println("Length of $path is ${File(path).length()} bytes")
}
```



## Lasso


```Lasso
// local to current directory
local(f = file('input.txt'))
handle => { #f->close }
#f->size

// file at file system root
local(f = file('//input.txt'))
handle => { #f->close }
#f->size

```



## Liberty BASIC


```lb
'input.txt in current directory
OPEN DefaultDir$ + "/input.txt" FOR input AS #m
PRINT "File size: "; lof(#m)
CLOSE #m

'input.txt in root
OPEN "c:/input.txt" FOR input AS #m
PRINT "File size: "; lof(#m)
CLOSE #m
```



## Lingo


```lingo
----------------------------------------
-- Returns file size
-- @param {string} filename
-- @return {integer}
----------------------------------------
on getFileSize (filename)
  fp = xtra("fileIO").new()
  fp.openFile(filename, 1)
  if fp.status() then return 0
  len = fp.getLength()
  fp.closeFile()
  return len
end
```



## LiveCode


```LiveCode
// root folder
set the defaultfolder to "/"
repeat for each line fline in (the detailed files)
    if item 1 of fline is "input.txt" then
        put item 2 of fline --bytes
        exit repeat
    end if
end repeat

// current working dir of stack
put the effective filename of this stack into tPath
set the itemDelimiter to slash
delete last item of tPath
set the defaultfolder to tPath
repeat for each line fline in (the detailed files)
    if item 1 of fline is "input.txt" then
        put item 2 of fline
        exit repeat
    end if
end repeat
```



## Lua


```Lua
function GetFileSize( filename )
    local fp = io.open( filename )
    if fp == nil then
 	return nil
    end
    local filesize = fp:seek( "end" )
    fp:close()
    return filesize
end
```



## Maple


```Maple
FileTools:-Size( "input.txt" )
```


```Maple
FileTools:-Size( "/input.txt" )
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

FileByteCount["input.txt"]
FileByteCount[FileNameJoin[{$RootDirectory, "input.txt"}]]
```


=={{header|MATLAB}} / {{header|Octave}}==

```matlab
d1 = dir('input.txt');
d2 = dir('/input.txt');
fprintf('Size of input.txt is %d bytes\n', d1.bytes)
fprintf('Size of /input.txt is %d bytes\n', d2.bytes)
```



## MAXScript


```maxscript
-- Returns filesize in bytes or 0 if the file is missing
getFileSize "index.txt"
getFileSize "\index.txt"
```



## Mirah


```mirah
import java.io.File

puts File.new('file-size.mirah').length()
puts File.new("./#{File.separator}file-size.mirah").length()
```



## mIRC Scripting Language


```mirc
echo -ag $file(input.txt).size bytes
echo -ag $file(C:\input.txt).size bytes
```


=={{header|Modula-3}}==

```modula3
MODULE FSize EXPORTS Main;

IMPORT IO, Fmt, FS, File, OSError;

VAR fstat: File.Status;

BEGIN
  TRY
    fstat := FS.Status("input.txt");
    IO.Put("Size of input.txt: " & Fmt.LongInt(fstat.size) & "\n");
    fstat := FS.Status("/input.txt");
    IO.Put("Size of /input.txt: " & Fmt.LongInt(fstat.size) & "\n");
  EXCEPT
  | OSError.E => IO.Put("ERROR: Could not get file status.\n");
  END;
END FSize.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java symbols binary

runSample(arg)
return

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method fileSize(fn) public static returns double
  ff = File(fn)
  fSize = ff.length()
  return fSize

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
    sz = fileSize(fn)
    say ft ''''fn'''' sz 'bytes.'
    end

  return
```

{{out}}

```txt

J:\>nrc fsz
java -cp "c:\netRexx\lib\NetRexxR.jar;c:\netRexx\lib\NetRexxC.jar;.;C:\Program Files\BSF4ooRexx\bsf4ooRexx-v452-20150825-bin.jar;;c:\netrexx\lib\NetRexxF.jar;." -Dnrx.compiler=ecj org.netrexx.process.NetRexxC  fsz
NetRexx portable processor 3.04 GA build 4-20150630-1657
Copyright (c) RexxLA, 2011,2015.   All rights reserved.
Parts Copyright (c) IBM Corporation, 1995,2008.
Program fsz.nrx
    function fileSize(Rexx)
    function runSample(Rexx)
Compilation of 'fsz.nrx' successful

J:\>java fsz test.txt
File 'test.txt' 8 bytes.
```



## NewLISP


```NewLISP
(println (first (file-info "input.txt")))
(println (first (file-info "/input.txt")))
```



## Nim


```nim
import os
echo getFileSize "input.txt"
echo getFileSize "/input.txt"
```


=={{header|Objective-C}}==


```objc
NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
NSLog(@"%llu", [[fm fileAttributesAtPath:@"input.txt" traverseLink:YES] fileSize]);

// OS X 10.5+
NSLog(@"%llu", [[fm attributesOfItemAtPath:@"input.txt" error:NULL] fileSize]);
```



## Objeck


```objeck

use IO;
...
File("input.txt")->Size()->PrintLine();
File("c:\input.txt")->Size()->PrintLine();

```



## OCaml



```ocaml
let printFileSize filename =
  let ic = open_in filename in
  Printf.printf "%d\n" (in_channel_length ic);
  close_in ic ;;

printFileSize "input.txt" ;;
printFileSize "/input.txt" ;;
```


For files greater than Pervasives.max_int, one can use the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.LargeFile.html Pervasives.LargeFile]:

```ocaml
let printLargeFileSize filename =
  let ic = open_in filename in
  Printf.printf "%Ld\n" (LargeFile.in_channel_length ic);
  close_in ic ;;
```


Alternatively:

```ocaml
#load "unix.cma" ;;
open Unix ;;
Printf.printf "%d\n" (stat "input.txt").st_size ;;
Printf.printf "%d\n" (stat "/input.txt").st_size ;;
```


The module Unix has also a [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.LargeFile.html LargeFile sub-module].


## Oforth



```Oforth
File new("input.txt") size println
File new("/input.txt") size println
```



## ooRexx


```oorexx
Parse Version v
Say v
fid='test.txt'
x=sysfiletree(fid,a.)
Say a.0
Say a.1
Say left(copies('123456789.',10),length(a.1))
Parse Var a.1 20 size .
Say 'file size:' size
s=charin(fid,,1000)
Say length(s)
Say 'file' fid
'type' fid
```

{{out}}

```txt
J:\>rexx sft
REXX-ooRexx_4.2.0(MT)_64-bit 6.04 22 Feb 2014
1
 7/26/16   3:28p           8  A----  J:\test.txt
123456789.123456789.123456789.123456789.12345678
file size: 8
8
file test.txt
12
34
```



## Oz


```oz
declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
in
  {Show {Path.size "input.txt"}}
  {Show {Path.size "/input.txt"}}
```



## Pascal

See [[File_size#Delphi | Delphi]]


## Perl



```perl
my $size1 = -s 'input.txt';
my $size2 = -s '/input.txt';
```


Or, to be 100% cross-platform:

```perl
use File::Spec::Functions qw(catfile rootdir);
my $size1 = -s 'input.txt';
my $size2 = -s catfile rootdir, 'input.txt';
```


Alternative way to get the size:

```perl
my $size1 = (stat 'input.txt')[7];  # builtin stat() returns an array with file size at index 7
my $size2 = (stat '/input.txt')[7];
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
say 'input.txt'.IO.s;
say '/input.txt'.IO.s;
```


Cross-platform version of the second one:

```perl6
say $*SPEC.rootdir.IO.child("input.txt").s;
```



## Phix


```Phix
function file_size(sequence file_name)
object d = dir(file_name)
    if atom(d) or length(d)!=1 then return -1 end if
    return d[1][D_SIZE]
end function

procedure test(sequence file_name)
integer size = file_size(file_name)
    if size<0 then
        printf(1,"%s file does not exist.\n",{file_name})
    else
        printf(1,"%s size is %d.\n",{file_name,size})
    end if
end procedure

test("input.txt") -- in the current working directory
test("/input.txt") -- in the file system root
```



## PHP


```php
<?php
echo filesize('input.txt'), "\n";
echo filesize('/input.txt'), "\n";
?>
```



## PicoLisp


```PicoLisp
(println (car (info "input.txt")))
(println (car (info "/input.txt")))
```



## Pike


```pike
import Stdio;

int main(){
   write(file_size("input.txt") + "\n");
   write(file_size("/input.txt") + "\n");
}
```



## PL/I


```PL/I

/* To obtain file size of files in root as well as from current directory. */

test: proc options (main);
   declare ch character (1);
   declare i fixed binary (31);
   declare in1 file record;

   /* Open a file in the root directory. */
   open file (in1) title ('//asd.log,type(fixed),recsize(1)');
   on endfile (in1) go to next1;
   do i = 0 by 1;
      read file (in1) into (ch);
   end;
next1:
   put skip list ('file size in root directory =' || trim(i));
   close file (in1);

   /* Open a file in the current dorectory. */
   open file (in1) title ('/asd.txt,type(fixed),recsize(1)');
   on endfile (in1) go to next2;
   do i = 0 by 1;
      read file (in1) into (ch);
   end;
next2:
   put skip list ('local file size=' || trim(i));
end test;

```


```txt

I used differently-named files to prove that local and root directory
files were accessed.
This program ran with Windows PL/I.

```



## Pop11



```pop11
;;; prints file size in bytes
sysfilesize('input.txt') =>
sysfilesize('/input.txt') =>
```



## PostScript

<code>status</code> returns status information about a file if given a file name. This includes the size in pages (implementation-dependent), the size in bytes, creation and modification time and a final <code>true</code>. The values not needed here are simply <code>pop</code>ed off the stack.

```postscript
(input.txt  ) print
(input.txt) status pop pop pop = pop
(/input.txt ) print
(/input.txt) status pop pop pop = pop
```



## PowerShell


```powershell
Get-ChildItem input.txt | Select-Object Name,Length
Get-ChildItem \input.txt | Select-Object Name,Length
```



## PureBasic


```purebasic
Debug FileSize("input.txt")
Debug FileSize("/input.txt")
```



## Python



```python
import os

size = os.path.getsize('input.txt')
size = os.path.getsize('/input.txt')
```



## R

{{works with|R|2.8.1}}

R has a function file.info() in the base package that performs this function.  Note that regardless of the OS, R uses forward slashes for the directories.


```R
sizeinwd <- file.info('input.txt')[["size"]]
sizeinroot <- file.info('/input.txt')[["size"]]
```



## Racket


```Racket
#lang racket
(file-size "input.txt")
(file-size "/input.txt")
```



## RapidQ

File I/O is one of the things where RapidQ differs from standard Basic. RapidQ uses file streams.

Method 1: display file size using file streams


```rapidq
$INCLUDE "rapidq.inc"

DIM file AS QFileStream

FUNCTION fileSize(name$) AS Integer
    file.Open(name$, fmOpenRead)
    Result = file.Size
    file.Close
END FUNCTION

PRINT "Size of input.txt is "; fileSize("input.txt")
PRINT "Size of \input.txt is "; fileSize("\input.txt")
```


Method 2: using DIR$


```rapidq
FileName$ = DIR$("input.txt", 0)
PRINT "Size of input.txt is "; FileRec.Size
FileName$ = DIR$("\input.txt", 0)
PRINT "Size of \input.txt is "; FileRec.Size
```



## Raven


```raven
'input.txt'  status.size
'/input.txt' status.size
```



## REBOL


```REBOL
size? %info.txt
size? %/info.txt
size? ftp://username:password@ftp.site.com/info.txt
size? http://rosettacode.org
```


## Red


```Red>>
 size? %input.txt
== 39244
>> size? %/c/input.txt
== 39244
```



## Retro

The simple way is to open and read the size. This may crash if the file does not exist.


```Retro
with files'
"input.txt" :R open &size sip close drop putn
"/input.txt" :R open &size sip close drop putn
```


For added stability, check that the returned file handle is not zero:


```Retro
with files'
"input.txt" :R open over 0 <> [ &size sip close drop ] ifTrue
```


Or, if you need to do this more often, setup a function that'll also display an error message if the file does not exist:


```Retro
with files'
: getFileSize ( $-n )
  :R open 0 over =
  [ "File does Not Exist\n" puts ]
  [ &size sip close drop ] if ;

"input.txt" getFileSize putn
"/input.txt" getFileSize putn
```



## REXX


### MS DOS version 1

This REXX example was executed on a Windows/XP and also a Windows 7 system (in a DOS ''window''),   and

it reports the file's size (in bytes) for both of the required files.

Various REXXes were used for testing:   '''Regina,   PERSONAL REXX,   PC/REXX,'''   and   '''R4'''.

Note that some operating systems don't have a concept of a   ''current directory''   or a   ''file system root''.

```rexx
/*REXX program determines a file's size (by reading all the data) in current dir & root.*/
parse arg iFID .                                 /*allow the user specify the  file ID. */
if iFID=='' | iFID==","  then iFID='input.txt'   /*Not specified?  Then use the default.*/
say 'size of     'iFID   "="   fSize(iFID)         'bytes'      /*the current directory.*/
say 'size of \..\'iFID   "="   fSize('\..\'iFID)   'bytes'      /* "    root      "     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fSize: parse arg f;   $=0;   do while chars(f)\==0;    $ = $ + length( charin( f, , 1e4) )
                             end   /*while*/;          call lineout f      /*close file.*/
       return $
```

{{out|output|text=  when using the default input:}}

```txt

size of     input.txt = 40 bytes
size of \..\input.txt = 40 bytes

```



### MS DOS version 2


```rexx
/*REXX pgm to verify a file's size */
parse arg iFID .                       /*let user specify the file ID.  */
if iFID==''  then iFID="FILESIZ.DAT"   /*Not specified? Then use default*/
say 'size of' iFID':'
Say chars(ifid) '(CR LF included)'
Call lineout ifid /* close the file */
say filesize(ifid) '(net data)'
Call lineout ifid
exit

filesize:  parse arg f;
  sz=0;
  Do while lines(f)\==0
    sz=sz+length(linein(f))
    End
  return sz
```

{{out}}

```txt
size of FILESIZ.DAT:
4 (CR LF included)
2 (net data)
```



### CMS version

Note that CMS hasn't a concept of a ''root''.

Also note that the CMS system doesn't normally support the use of periods ('''.''');   it uses blanks instead.

```rexx
/*REXX program determines a file's size (by reading all the data)  on the default mDisk.*/
parse arg iFID                                   /*allow the user specify the  file ID. */
if iFID=='' | iFID==","  then iFID= 'INPUT TXT'  /*Not specified?  Then use the default.*/
say 'size of'     iFID     "="     fSize(iFID)     'bytes'       /*on the default mDisk.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fSize: parse arg f;    $= 0;      do while lines(f)\==0;        $= $ + length( linein(f) )
                                  end   /*while*/
       return $
```




## Ring


```ring
See len(read('input.txt')) + nl
see len(read('/input.txt')) + nl
```



## Ruby


```ruby
size = File.size('input.txt')
size = File.size('/input.txt')
```



## Run BASIC


```runbasic
print fileSize(DefaultDir$,"input.txt")  ' current default directory
print fileSize("","input.txt")       ' root directory

function fileSize(dir$,file$)
open dir$;"\";file$ FOR input as #f
fileSize = lof(#f)                   ' Length Of File
close #f
end function
```



## Rust


```rust
use std::{env, fs, process};
use std::io::{self, Write};
use std::fmt::Display;

fn main() {
    let file_name = env::args().nth(1).unwrap_or_else(|| exit_err("No file name supplied", 1));
    let metadata = fs::metadata(file_name).unwrap_or_else(|e| exit_err(e, 2));

    println!("Size of file.txt is {} bytes", metadata.len());
}

#[inline]
fn exit_err<T: Display>(msg: T, code: i32) -> ! {
    writeln!(&mut io::stderr(), "Error: {}", msg).expect("Could not write to stdout");
    process::exit(code)
}

}
```



## Scala

{{libheader|Scala}}

```Scala
import java.io.File

object FileSize extends App {
  val name = "pg1661.txt"

  println(s"$name  : ${new File(name).length()} bytes")
  println(s"/$name : ${new File(s"${File.separator}$name").length()} bytes")
}
```



## Scheme


```scheme

(define (file-size filename)
  (call-with-input-file filename (lambda (port)
    (let loop ((c (read-char port))
               (count 0))
      (if (eof-object? c)
          count
          (loop (read-char port) (+ 1 count)))))))

(file-size "input.txt")
(file-size "/input.txt")

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(fileSize("input.txt"));
    writeln(fileSize("/input.txt"));
  end func;
```



## Sidef


```ruby
say (Dir.cwd  + %f'input.txt' -> size);
say (Dir.root + %f'input.txt' -> size);
```



## Slate


```slate
(File newNamed: 'input.txt') fileInfo fileSize.
(File newNamed: '/input.txt') fileInfo fileSize.
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
(File name: 'input.txt') size printNl.
(File name: '/input.txt') size printNl.
```

{{works with|Smalltalk/X}}
{{works with|VisualWorks Smalltalk}}

```smalltalk
'input.txt' asFilename fileSize
'/input.txt' asFilename fileSize
```



## Standard ML



```sml
val size = OS.FileSys.fileSize "input.txt" ;;
val size = OS.FileSys.fileSize "/input.txt" ;
```



## Stata


To get the size in byte of an arbitrary file, use [http://www.stata.com/help.cgi?file file seek]. Just replace input.txt with \input.txt if the file resides in the root directory of the current disk.


```stata
file open f using input.txt, read binary
file seek f eof
file seek f query
display r(loc)
file close f
```


However, what is usually interesting is the size of a datatset. Use [http://www.stata.com/help.cgi?describe describe], either on the currently loaded dataset, or on a dataset on disk. The describe command will print the file size, but it's possible to use [http://www.stata.com/help.cgi?stored_results stored results] as well.


```stata
describe using test.dta
display r(N)*r(width)
```



## Tcl


```tcl
file size input.txt
file size /input.txt
```



## Toka

A trivial method follows:


```toka
" input.txt"  "R" file.open dup file.size . file.close
" /input.txt" "R" file.open dup file.size . file.close
```


A better method would be to define a new function that actually
checks whether the file exists:


```toka
[ "R" file.open
  dup 0 <> [ dup file.size . file.close ] ifTrue
  drop
] is display-size

" input.txt"  display-size
" /input.txt" display-size
```



## TorqueScript

--[[User:Ipquarx|Ipquarx]] June 19th, 10:00 AM<br /><br />
Since TorqueScript cannot set the current working directory, the second part of the task cannot be completed.<br /><br />

TGE Version (Works with all versions containing the basic file i/o):
{{works with|TGE}}

```Torque
%File = new FileObject();
%File.openForRead("input.txt");

while(!%File.isEOF())
{
	%Length += strLen(%File.readLine());
}

%File.close();
%File.delete();
```

<br />
T3D Version (Only works with T3D):
{{works with|T3D}}

```Torque
fileSize("input.txt");
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
-- size of file input.txt
file="input.txt"
ERROR/STOP OPEN (file,READ,-std-)
file_size=BYTES ("input.txt")
ERROR/STOP CLOSE (file)

-- size of file x:/input.txt
ERROR/STOP OPEN (file,READ,x)
file_size=BYTES (file)
ERROR/STOP CLOSE (file)

```



## UNIX Shell

An interactive user would run ''ls -l input.txt /input.txt'' to see the file sizes. This task is more difficult for a shell script, that must extract each size from command output.


### Using ls

''ls'' most likely gets the length from the file's inode.


```bash
size1=$(ls -l input.txt | tr -s ' ' | cut -d ' ' -f 5)
size2=$(ls -l /input.txt | tr -s ' ' | cut -d ' ' -f 5)
```


''ls -l'' reports the size in 5th field, with spaces between fields.
''tr'' squeezes spaces (because ''cut'' needs one single space between fields),
and ''cut'' extracts 5th field.


```bash

echo "# ls:"
ls  -la  input.txt

echo "# stat:"
stat input.txt

echo "# Size:"
size1=$(ls -l input.txt | tr -s ' ' | cut -d ' ' -f 5)
size2=$(wc -c < input.txt | tr -d ' ')
echo $size1, $size2

```

{{Out}} Test run at compileonline.com:

```txt

# ls:
-rw-r--r-- 1 apache apache 126 Nov  5 19:02 input.txt
# stat:
  File: `input.txt'
  Size: 126       	Blocks: 8          IO Block: 4096   regular file
Device: 700h/1792d	Inode: 2195776     Links: 1
Access: (0644/-rw-r--r--)  Uid: (   48/  apache)   Gid: (   48/  apache)
Access: 2014-11-05 19:02:25.000000000 -0600
Modify: 2014-11-05 19:02:25.000000000 -0600
Change: 2014-11-05 19:02:25.000000000 -0600
# Size:
126, 126

```



### Using wc

''wc'' may actually read the whole file and count the bytes. Some implementations, like [http://git.savannah.gnu.org/cgit/coreutils.git/tree/src/wc.c wc.c] from GNU coreutils, can optimize ''wc -c'' by getting the length from the file's inode.


```bash
size1=$(wc -c < input.txt | tr -d ' ')
size2=$(wc -c < /input.txt | tr -d ' ')
```


The peculiar use of ''wc -c < file'', not ''wc -c file'', is to prevent printing the file's name. Then ''wc'' only reports the size. Some versions of ''wc'' print spaces before the number; ''tr'' deletes all these spaces.


### Using BSD stat

[[BSD]] has [http://netbsd.gw.com/cgi-bin/man-cgi?stat++NetBSD-current stat(1)], a nonstandard command. With ''stat'', a shell script can easily get the file size.

{{works with|NetBSD|1.6}}
{{works with|FreeBSD|4.10}}
{{works with|OpenBSD|3.8}}


```bash
size1=$(stat -f %z input.txt)
size2=$(stat -f %z /input.txt)
```



### Z Shell

{{works with|zsh}}


```bash
# from module 'zsh/stat', load builtin 'zstat'
zmodload -F zsh/stat b:zstat

size1=$(zstat +size input.txt)
size2=$(zstat +size /input.txt)
```



## Ursa


```ursa
decl file f

f.open "input.txt"
out (size f) endl console
f.close

f.open "/input.txt"
out (size f) endl console
f.close
```



## VBScript

{{works with|Windows Script Host|*}}

```VBScript

With CreateObject("Scripting.FileSystemObject")
	WScript.Echo .GetFile("input.txt").Size
	WScript.Echo .GetFile("\input.txt").Size
End With

```



## Vedit macro language


```vedit
Num_Type(File_Size("input.txt"))
Num_Type(File_Size("/input.txt"))
```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Option Explicit

----

Sub DisplayFileSize(ByVal Path As String, ByVal Filename As String)
Dim i As Long
  If InStr(Len(Path), Path, "\") = 0 Then
    Path = Path & "\"
  End If
  On Error Resume Next 'otherwise runtime error if file does not exist
  i = FileLen(Path & Filename)
  If Err.Number = 0 Then
    Debug.Print "file size: " & CStr(i) & " Bytes"
  Else
    Debug.Print "error: " & Err.Description
  End If
End Sub

----

Sub Main()
  DisplayFileSize CurDir(), "input.txt"
  DisplayFileSize CurDir(), "innputt.txt"
  DisplayFileSize Environ$("SystemRoot"), "input.txt"
End Sub

```

{{out}}

```txt
file size: 37 Bytes
error: file not found
file size: 37 Bytes
```



## Visual Basic .NET


'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Dim local As New IO.FileInfo("input.txt")
Console.WriteLine(local.Length)

Dim root As New IO.FileInfo("\input.txt")
Console.WriteLine(root.Length)
```



## X86 Assembly


```x86asm

; x86_64 linux nasm

section .data
localFileName: db "input.txt", 0
rootFileName: db "/initrd.img", 0

section .text

global _start

_start:

  ; open file in current dir
  mov rax, 2
  mov rdi, localFileName
  xor rsi, rsi
  mov rdx, 0
  syscall
  push rax

  mov rdi, rax ; file descriptior
  mov rsi, 0 ; offset
  mov rdx, 2 ; whence
  mov rax, 8 ; sys_lseek
  syscall

  ; compare result to actual size
  cmp rax, 11
  jne fail

  ; close the file
  pop rdi
  mov rax, 3
  syscall

  ; open file in root dir
  mov rax, 2
  mov rdi, rootFileName
  xor rsi, rsi
  mov rdx, 0
  syscall
  push rax

  mov rdi, rax ; file descriptior
  mov rsi, 0 ; offset
  mov rdx, 2 ; whence
  mov rax, 8 ; sys_lseek
  syscall

  ; compare result to actual size
  cmp rax, 37722243
  jne fail

  ; close the file
  pop rdi
  mov rax, 3
  syscall

  ; test successful
  mov rax, 60
  mov rdi, 0
  syscall

  ; test failed
  fail:
    mov rax, 60
    mov rdi, 1
    syscall

```



## zkl


```zkl
File.info("input.txt").println();
File.info("/input.txt").println();
```

-->T(size,creation time,last mod time,isDir,mode), from stat(2)
{{out}}

```txt

L(391,1393658766,1393658766,False,33156)
Exception thrown: NameError(File.info(/input.txt): Could not open)

```



{{omit from|Befunge}} <!-- No filesystem support -->
{{omit from|HTML}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
