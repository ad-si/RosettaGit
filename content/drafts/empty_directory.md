+++
title = "Empty directory"
description = ""
date = 2019-06-21T05:46:54Z
aliases = []
[extra]
id = 11132
[taxonomies]
categories = []
tags = []
+++

{{task}}
Starting with a path to some directory, determine whether the directory is empty.

An empty directory contains no files nor subdirectories. 
With [[Unix]] or [[Windows]] systems, every directory contains an entry for “<code>.</code>” and almost every directory contains “<code>..</code>” (except for a root directory); an empty directory contains no other entries.


## 11l


```11l
I fs:list_dir(input()).empty
   print(‘empty’)
E
   print(‘not empty’)
```



## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
procedure EmptyDir is
   function Empty (path : String) return String is
      use Ada.Directories;
      result : String := "Is empty.";
      procedure check (ent : Directory_Entry_Type) is begin
         if Simple_Name (ent) /= "." and Simple_Name (ent) /= ".." then
            Empty.result := "Not empty";
         end if;
      end check;
   begin
      if not Exists (path) then return "Does not exist.";
      elsif Kind (path) /= Directory then return "Not a Directory.";
      end if;
      Search (path, "", Process => check'Access);
      return result;
   end Empty;
begin
   Put_Line (Empty ("."));
   Put_Line (Empty ("./empty"));
   Put_Line (Empty ("./emptydir.adb"));
   Put_Line (Empty ("./foobar"));
end EmptyDir;
```

{{out}}

```txt
Not empty
Is empty.
Not a Directory.
Does not exist.
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
This uses the "argc", "argv", "file is directory" and "get directory" procedures specific to Algol 68 G.

Note the Algol 68 G interpreter processes the command line parameters before "-" so this example expects the directory names to follow "-".

```algol68
# returns TRUE if the specified directory is empty, FALSE if it doesn't exist or is non-empty #
PROC is empty directory = ( STRING directory )BOOL:
     IF NOT file is directory( directory )
     THEN
         # directory doesn't exist #
         FALSE
     ELSE
         # directory is empty if it contains no files or just "." and possibly ".." #
         []STRING files   = get directory( directory );
         BOOL     result := FALSE;
         FOR f FROM LWB files TO UPB files
         WHILE result := files[ f ] = "." OR files[ f ] = ".."
         DO
             SKIP
         OD;
         result
     FI # is empty directory # ;

# test the is empty directory procedure #
# show whether the directories specified on the command line ( following "-" ) are empty or not #
BOOL directory name parameter := FALSE;
FOR i TO argc DO
    IF argv( i ) = "-"
    THEN
        # marker to indicate directory names follow #
        directory name parameter := TRUE
    ELIF directory name parameter
    THEN
        # have a directory name - report whether it is emty or not #
        print( ( argv( i ), " is ", IF is empty directory( argv( i ) ) THEN "empty" ELSE "not empty" FI, newline ) )
    FI
OD
```

{{out}}

```txt

H:\algol\rosetta>a68g emptyDirectory.a68 - . /temp \zz
. is not empty
/temp is not empty
\zz is empty

```



## AutoHotkey


```autohotkey
MsgBox % isDir_empty(A_ScriptDir)?"true":"false"

isDir_empty(p) {
	Loop, %p%\* , 1
		return 0
	return 1
}
```

{{out}}

```txt
false
```


## AWK


```AWK

# syntax: GAWK -f EMPTY_DIRECTORY.AWK
BEGIN {
    n = split("C:\\TEMP3,C:\\NOTHERE,C:\\AWK\\FILENAME,C:\\WINDOWS",arr,",")
    for (i=1; i<=n; i++) {
      printf("'%s' %s\n",arr[i],is_dir(arr[i]))
    }
    exit(0)
}
function is_dir(path,  cmd,dots,entries,msg,rec,valid_dir) {
    cmd = sprintf("DIR %s 2>NUL",path) # MS-Windows
    while ((cmd | getline rec) > 0) {
      if (rec ~ /[0-9]:[0-5][0-9]/) {
        if (rec ~ / (\.|\.\.)$/) { # . or ..
          dots++
          continue
        }
        entries++
      }
      if (rec ~ / Dir\(s\) .* bytes free$/) {
        valid_dir = 1
      }
    }
    close(cmd)
    if (valid_dir == 0) {
      msg = "does not exist"
    }
    else if (valid_dir == 1 && entries == 0) {
      msg = "is an empty directory"
    }
    else if (dots == 0 && entries == 1) {
      msg = "is a file"
    }
    else {
      msg = sprintf("is a directory with %d entries",entries)
    }
    return(msg)
}

```

{{out}}

```txt

'C:\TEMP3' is an empty directory
'C:\NOTHERE' does not exist
'C:\AWK\FILENAME' is a file
'C:\WINDOWS' is a directory with 98 entries

```



## BaCon


```freebasic
' empty directory test
somedir$ = "somedir"
files = testdir(somedir$)
IF files = 0 THEN PRINT somedir$, " directory empty"

files = testdir(".")
PRINT "Current directory holds ", files, " files"
END

FUNCTION testdir(somedir$)
    OPEN somedir$ FOR DIRECTORY AS adir
    total = 0
    REPEAT
        GETFILE afile$ FROM adir
        IF afile$ != "." AND afile$ != ".." AND afile$ != "" THEN INCR total
    UNTIL ISFALSE(LEN(afile$))
    IF (long)adir != 0 THEN CLOSE DIRECTORY adir
    RETURN total
END FUNCTION
```


{{out}}

```txt
prompt$ ./empty-dir
somedir directory empty
Current directory holds 183 files
```



## Batch File

This demo verifies first if the directory exists. This script returns errorlevel code:
*0 - input directory is empty.
*1 - input directory is NOT empty.
*2 - input directory does not exist.
*3 - input not found.

```dos
@echo off
if "%~1"=="" exit /b 3
set "samp_path=%~1"
set "tst_var="

	%== Store the current directory of the CMD ==%
for /f %%T in ('cd') do set curr_dir=%%T

	%== Go to the samp_path ==%
cd %samp_path% 2>nul ||goto :folder_not_found

	%== The current directory is now samp_path ==%
	%== Scan what is inside samp_path ==%	
for /f "usebackq delims=" %%D in (
	`dir /b 2^>nul ^& dir /b /ah 2^>nul`
) do set "tst_var=1"

if "%tst_var%"=="1" (
	echo "%samp_path%" is NOT empty.
	cd %curr_dir%
	exit /b 1
) else (
	echo "%samp_path%" is empty.
	cd %curr_dir%
	exit /b 0
)

:folder_not_found
echo Folder not found.
exit /b 2
```

{{Out|Sample Session}}
(Saved the Batch File as IsEmpty.Bat in C:\)

```txt
C:\>IsEmpty

C:\>echo %errorlevel%
3

C:\>IsEmpty "C:\Sample"
Folder not found.

C:\>md Sample

C:\>IsEmpty "C:\Sample"
"C:\Sample" is empty.

C:\>cd Sample

C:\Sample>echo 0123456789 >>digits.txt

C:\Sample>dir /b
digits.txt

C:\Sample>cd..

C:\>IsEmpty "C:\Sample"
"C:\Sample" is NOT empty.

C:\>
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      IF FNisdirectoryempty("C:\") PRINT "C:\ is empty" ELSE PRINT "C:\ is not empty"
      IF FNisdirectoryempty("C:\temp") PRINT "C:\temp is empty" ELSE PRINT "C:\temp is not empty"
      END
      
      DEF FNisdirectoryempty(dir$)
      LOCAL dir%, sh%, res%
      DIM dir% LOCAL 317
      IF RIGHT$(dir$)<>"\" dir$ += "\"
      SYS "FindFirstFile", dir$+"*", dir% TO sh%
      IF sh% = -1 ERROR 100, "Directory doesn't exist"
      res% = 1
      REPEAT
        IF $$(dir%+44)<>"." IF $$(dir%+44)<>".." EXIT REPEAT
        SYS "FindNextFile", sh%, dir% TO res%
      UNTIL res% == 0
      SYS "FindClose", sh%
      = (res% == 0)
```



## C


```c>#include <stdio.h

#include <dirent.h>
#include <string.h>

int dir_empty(const char *path)
{
	struct dirent *ent;
	int ret = 1;

	DIR *d = opendir(path);
	if (!d) {
		fprintf(stderr, "%s: ", path);
		perror("");
		return -1;
	}

	while ((ent = readdir(d))) {
		if (!strcmp(ent->d_name, ".") || !(strcmp(ent->d_name, "..")))
			continue;
		ret = 0;
		break;
	}

	closedir(d);
	return ret;
}

int main(int c, char **v)
{
	int ret = 0, i;
	if (c < 2) return -1;

	for (i = 1; i < c; i++) {
		ret = dir_empty(v[i]);
		if (ret >= 0)
			printf("%s: %sempty\n", v[i], ret ? "" : "not ");
	}

	return 0;
}
```
Running it:
```txt

% mkdir stuff; ./a.out /usr/ ./stuff /etc/passwd
/usr/: not empty
./stuff: empty
/etc/passwd: Not a directory

```



## C++

{{libheader|Boost}}

```cpp

#include <iostream>
#include <boost/filesystem.hpp>

using namespace boost::filesystem;

int main(int argc, char *argv[])
{
    for (int i = 1; i < argc; ++i) {
        path p(argv[i]);

        if (exists(p) && is_directory(p))
            std::cout << "'" << argv[i] << "' is" << (!is_empty(p) ? " not" : "") << " empty\n";
        else
            std::cout << "dir '" << argv[i] << "' could not be found\n";
    }
}

```



## C sharp


```csharp
using System;
using System.IO;

class Program
{
    static void Main( string[] args )
    {
        foreach ( string dir in args )
        {
            Console.WriteLine( "'{0}' {1} empty", dir, IsDirectoryEmpty( dir ) ? "is" : "is not" );
        }
    }

    private static bool IsDirectoryEmpty( string dir )
    {
        return ( Directory.GetFiles( dir ).Length == 0 &&
            Directory.GetDirectories( dir ).Length == 0 );
    }
}

```

Running it:
```txt

Assume c:\temp exists and is not empty, c:\temp\empty exists and is empty

c:\>IsEmptyDir c:\temp c:\temp\empty
'c:\temp' is not empty
'c:\temp\empty' is empty

```



## Clojure


```clojure
(require '[clojure.java.io :as io])
(defn empty-dir? [path]
  (let [file (io/file path)]
    (assert (.exists file))
    (assert (.isDirectory file))
    (-> file .list empty?))) ; .list ignores "." and ".."
```




## CoffeeScript


```coffeescript

fs = require 'fs'

is_empty_dir = (dir) ->
  throw Error "#{dir} is not a dir" unless fs.statSync(dir).isDirectory()
  # readdirSync does not return . or ..
  fns = fs.readdirSync dir
  fns.length == 0

```



## Common Lisp

Will also return <code>T</code> if <code>path</code> doesn't exist.

```lisp

(defun empty-directory-p (path)
  (and (null (directory (concatenate 'string path "/*")))
       (null (directory (concatenate 'string path "/*/")))))

```



## D



```d
import std.stdio, std.file;

void main() {
    auto dir = "somedir";
    writeln(dir ~ " is empty: ", dirEmpty(dir));
}

bool dirEmpty(string dirname) {
    if (!exists(dirname) || !isDir(dirname))
        throw new Exception("dir not found: " ~ dirname);
    return dirEntries(dirname, SpanMode.shallow).empty;
}
```


```txt
somedir is empty: false
```



## Elixir


```elixir
path = hd(System.argv)
IO.puts File.dir?(path) and Enum.empty?( File.ls!(path) )
```



## Erlang

From the Erlang shell, or in a program, match {ok, []}. It this works the directory is empty.
{{out}}

```txt

3> {ok, []} = file:list_dir_all("/usr").   
** exception error: no match of right hand side value 
                    {ok,["X11R6","X11","standalone","share","sbin","local",
                         "libexec","lib","bin"]}
4> {ok, []} = file:list_dir_all("/asd").
** exception error: no match of right hand side value {error,enoent}
5> {ok, []} = file:list_dir_all("./empty").
{ok,[]}

```



## Factor


```factor
USE: io.directories
: empty-directory? ( path -- ? ) directory-entries empty? ;
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "dir.bi"

Function IsDirEmpty(dirPath As String) As Boolean
  Err = 0
  ' check dirPath is a valid directory
  Dim As String fileName = Dir(dirPath, fbDirectory)
  If Len(fileName) = 0 Then
    Err = 1000  ' dirPath is not a valid path
    Return False
  End If
  ' now check if there are any files/subdirectories in it other than . and ..
  Dim fileSpec As String = dirPath + "\*.*"
  Const attribMask = fbNormal Or fbHidden Or fbSystem Or fbDirectory
  Dim outAttrib As UInteger
  fileName = Dir(fileSpec, attribMask, outAttrib)  ' get first file
  Do
    If fileName <> ".." AndAlso fileName <> "." Then
      If Len(fileName) = 0 Then Return True
      Exit Do
    End If
    fileName = Dir  ' get next file
  Loop
  Return False
End Function

Dim outAttrib As UInteger
Dim dirPath As String = "c:\freebasic\docs"  ' known to be empty
Dim empty As Boolean = IsDirEmpty(dirPath)
Dim e As Long = Err
If e = 1000 Then
  Print "'"; dirPath; "' is not a valid directory"
  End
End If
If empty Then
  Print "'"; dirPath; "' is empty"
Else
  Print "'"; dirPath; "' is not empty"
End If
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

'c:\freebasic\docs' is empty

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO
let isEmptyDirectory x = (Directory.GetFiles x).Length = 0 && (Directory.GetDirectories x).Length = 0
```



## Gambas


```gambas
Public Sub Main()
Dim sFolder As String = User.home &/ "Rosetta"
Dim sDir As String[] = Dir(sFolder)
Dim sTemp As String
Dim sOutput As String = sfolder & " is NOT empty"

Try sTemp = sDir[0]
If Error Then sOutput = sfolder & " is empty"

Print sOutput

End
```

Output:

```txt

/home/charlie/Rosetta is empty

```



## Go


```go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	empty, err := IsEmptyDir("/tmp")
	if err != nil {
		log.Fatalln(err)
	}
	if empty {
		fmt.Printf("/tmp is empty\n")
	} else {
		fmt.Printf("/tmp is not empty\n")
	}
}

func IsEmptyDir(name string) (bool, error) {
	entries, err := ioutil.ReadDir(name)
	if err != nil {
		return false, err
	}
	return len(entries) == 0, nil
}

```


## Groovy

Solution:

```groovy
def isDirEmpty = { dirName ->
    def dir = new File(dirName)
    dir.exists() && dir.directory && (dir.list() as List).empty
}
```


Test:

```groovy
def currentDir = new File('.')
def random = new Random()
def subDirName = "dir${random.nextInt(100000)}"
def subDir = new File(subDirName)
subDir.mkdir()
subDir.deleteOnExit()

assert ! isDirEmpty('.')
assert isDirEmpty(subDirName)
```



## Haskell


```haskell
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)


isEmpty x = getDirectoryContents x >>= return . f . (== [".", ".."])
    where f True = "Directory is empty"
          f False = "Directory is not empty"

main = getArgs >>= isEmpty . (!! 0) >>= putStrLn
```

Test:
<lang>$ mkdir 1
$ ./isempty 1
Directory is empty
$ ./isempty /usr/
Directory is not empty
```



==Icon and {{header|Unicon}}==
This example uses Unicon extensions.  The 'empty' sub-directory was manually setup for this test.

```Icon
procedure main()
   every dir := "." | "./empty" do 
      write(dir, if isdirempty(dir) then " is empty" else " is not empty")
end
 
procedure isdirempty(s)  #: succeeds if directory s is empty (and a directory)
local d,f
   if ( stat(s).mode ? ="d" ) & ( d := open(s) ) then {
         while f := read(d) do 
            if f == ("."|"..") then next else fail 
         close(d)
         return s
         }
   else stop(s," is not a directory or will not open")
end
```


{{out}}

```txt
. is not empty
./empty is empty
```



## J



```j
require 'dir'
empty_dir=: 0 = '/*' #@dir@,~ ]
```


In other words, list the contents of the directory, count how many items are in it, and test if that count was zero.

Note that <code>1!:0</code> could have been used here, instead of the <code>dir</code> cover.

Example, under windows, create some directories using cygwin:


```bash
$ mkdir /tmp/a
$ touch /tmp/a/...
$ mkdir /tmp/b
$ mkdir /tmp/c
$ mkdir /tmp/c/d
```


Then, testing these directories, in J:


```j
   empty_dir 'c:/cygwin/tmp/a'
0
   empty_dir 'c:/cygwin/tmp/b'
1
   empty_dir 'c:/cygwin/tmp/c'
0
```



## Java

{{works with|Java|7+}}
This method does not check that the path given is actually a directory. If a path to a normal file is given, it will throw a <code>NullPointerException</code>. <code>File.listFiles()</code> does not count the "." and ".." entries.

```java5
import java.nio.file.Paths;
//... other class code here
public static boolean isEmptyDir(String dirName){
    return Paths.get(dirName).toFile().listFiles().length == 0;
}
```



## Julia


```julia
# v0.6.0
isemptydir(dir::AbstractString) = isempty(readdir(dir))

@show isemptydir(".")
@show isemptydir("/home")

```


{{out}}

```txt

isemptydir(".") = false
isemptydir("/home") = false

```



## Kotlin


```scala
// version 1.1.4

import java.io.File

fun main(args: Array<String>) {
    val dirPath = "docs" // or whatever
    val isEmpty = (File(dirPath).list().isEmpty())
    println("$dirPath is ${if (isEmpty) "empty" else "not empty"}")
}
```



## Lasso


```Lasso
dir('has_content') -> isEmpty
'<br />'
dir('no_content') -> isEmpty
```

{{out}}

```txt
false
true
```



## Liberty BASIC


```lb

dim info$(10, 10)
files "c:\", info$()

qtyFiles=val(info$(0,0))
n = qtyFiles+1  'begin directory info

folder$ = info$(n,0)  'path to first directory in c:

files folder$, info$() 're-fill array with data from sub folder

if val(info$(0,0)) + val(info$(0, 1)) <> 0 then
    print "Folder ";folder$;" is not empty."
else
    print "Folder ";folder$;" is empty."
end if

```



## Lingo


```lingo
on isDirEmpty (dir)
  return getNthFileNameInFolder(dir, 1) = EMPTY
end
```



## Lua

Pure Lua function based on snipplet from Stack Overflow[http://stackoverflow.com/questions/5303174/how-to-get-list-of-directories-in-lua#11130774].

```lua

function scandir(directory)
	local i, t, popen = 0, {}, io.popen
	local pfile = popen('ls -a "'..directory..'"')
	for filename in pfile:lines() do
		if filename ~= '.' and filename ~= '..' then
			i = i + 1
			t[i] = filename
		end
	end
	pfile:close()
	return t
end

function isemptydir(directory)
	return #scandir(directory) == 0
end

```


Using lfs[https://keplerproject.github.io/luafilesystem/] library.

```lua

function isemptydir(directory,nospecial)
	for filename in require('lfs').dir(directory) do
		if filename ~= '.' and filename ~= '..' then
			return false
		end
	end
	return true
end

```



## Maple



```Maple

emptydirectory := proc (dir) 
is(listdir(dir) = [".", ".."]); 
end proc;

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
EmptyDirectoryQ[x_] := (SetDirectory[x]; If[FileNames[] == {}, True, False])

Example use:
EmptyDirectoryQ["C:\\Program Files\\Wolfram Research\\Mathematica\\9"]
->True
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

  function x = isEmptyDirectory(p)
    if isdir(p)		
      f = dir(p)
      x = length(f)>2;
    else 
      error('Error: %s is not a directory');     
    end; 
  end; 

```



## min

{{works with|min|0.19.3}}

```min
(ls bool not) :empty-dir?
```



## Nemerle


```Nemerle
using System.IO;
using System.Console;

module EmptyDirectory
{
    IsDirectoryEmpty(dir : string) : bool
    {
        Directory.GetFileSystemEntries(dir).Length == 0
    }
    
    Main(args : array[string]) : void
    {
        foreach (dir in args) {
            when (Directory.Exists(dir)) {
                WriteLine("{0} {1} empty.", dir, if (IsDirectoryEmpty(dir))  "is" else "is not");
            }
        }
    }
}
```




## NewLISP


```NewLISP

(define (empty-dir? path-to-check)
	(empty? (clean (lambda (x) (or (= "." x) (= ".." x))) (directory path-to-check)))
)

```



## Nim


```nim
import os, rdstdin

var empty = true
for f in walkDir(readLineFromStdin "directory: "):
  empty = false
  break
echo empty
```



## Objeck


```objeck
function : IsEmptyDirectory(dir : String) ~ Bool {
  return Directory->List(dir)->Size() = 0;
}
```



## OCaml



```ocaml
let is_dir_empty d =
  Sys.readdir d = [| |]
```



## ooRexx


```oorexx
Call test 'D:\nodir'        /* no such directory      */
Call test 'D:\edir'         /* an empty directory     */
Call test 'D:\somedir'      /* directory with 2 files */
Call test 'D:\somedir','S'  /* directory with 3 files */
Exit
test: Parse Arg fd,nest
If SysIsFileDirectory(fd)=0 Then
  Say 'Directory' fd 'not found'
Else Do
  ret=SysFileTree(fd'\*.*','X', 'F'nest)
  If x.0=0 Then
    say 'Directory' fd 'is empty'
  Else Do
    If nest='' Then
      say 'Directory' fd 'contains' x.0 'files'
    Else
      say 'Directory' fd 'contains' x.0 'files (some nested)'
    End
  End
Return
```

{{out}}

```txt
Directory D:\nodir not found
Directory D:\edir is empty
Directory D:\somedir contains 2 files
Directory D:\somedir contains 3 files (some nested)
```



## PARI/GP


Define a function ''chkdir(<path>)'' that returns count of entries in a directory (without . and .. ): 

```parigp
chkdir(d)=extern(concat(["[ -d '",d,"' ]&&ls -A '",d,"'|wc -l||echo -1"]))
```


On error ''chkdir(...)'' returns -1 else count of entries. If ''chkdir() == 0'' then directory is empty. So define an additional function:

```parigp
dir_is_empty(d)=!chkdir(d)
```


Output:
```txt
chkdir("/tmp"): 52

dir_is_empty("/tmp"): 0
```



## Perl


### Simple version


```perl
sub dir_is_empty {!<$_[0]/*>}
```

This version however doesn't catch 'hidden' files that start with a dot.

May be good for quick one-liners or in some special cases when you are sure that there are no 'hidden' files or they doesn't play any role.

Unfortunalety, BSD glob() doesn't handle inverted character class. If it did, this pattern could be used: <tt>{.[^.],}*</tt> (this works well in bash). But it doesn't, so there's a

### Thorough version


```perl
use IO::Dir;
sub dir_is_empty { !grep !/^\.{1,2}\z/, IO::Dir->new(@_)->read }
```



## Perl 6


```perl6
sub dir-is-empty ($d) { not dir $d }
```

The <tt>dir</tt> function returns a lazy list of filenames, excluding "<tt>.</tt>" and "<tt>..</tt>" automatically.  Any boolean context (in this case the <tt>not</tt> function) will do just enough work on the lazy list to determine whether there are any elements, so we don't have to count the directory entries, or even read them all into memory, if there are more than one buffer's worth.


## Phix


```Phix
procedure test(string filename)
string msg
    switch get_file_type(filename) do
        case FILETYPE_UNDEFINED: msg = "is UNDEFINED"
        case FILETYPE_NOT_FOUND: msg = "is NOT_FOUND"
        case FILETYPE_FILE:      msg = "is a FILE"
        case FILETYPE_DIRECTORY:
            sequence d = dir(filename)
            integer count = 0
            for i=1 to length(d) do
                if not find(d[i][D_NAME],{".",".."}) then
                    count += 1
                end if
            end for
            if count=0 then
                msg = "is an empty directory"
            else
                msg = sprintf("is a directory containing %d files",{count})
            end if
    end switch
    printf(1,"%s %s\n",{filename,msg})
end procedure

constant tests = {"C:\\xx","C:\\not_there","C:\\Program Files (x86)\\Phix\\p.exe","C:\\Windows"}
for i=1 to length(tests) do
    test(tests[i])
end for
```

{{out}}

```txt

C:\xx is an empty directory
C:\not_there is NOT_FOUND
C:\Program Files (x86)\Phix\p.exe is a FILE
C:\Windows is a directory containing 110 files

```



## PHP

Any improvements welcome but here is a starting point for PHP

```php


$dir = 'path_here';

if(is_dir($dir)){
  //scandir grabs the contents of a directory and array_diff is being used to filter out .. and .
  $list = array_diff(scandir($dir), array('..', '.'));
  //now we can just use empty to check if the variable has any contents regardless of it's type
  if(empty($list)){
    echo 'dir is empty';
  }
  else{
    echo 'dir is not empty';
  }
}
else{
  echo 'not a directory';
}


```



## PicoLisp


```PicoLisp
(prinl "myDir is" (and (dir "myDir") " not") " empty")
```

{{out}}

```txt
myDir is not empty
```




## PowerShell


{{works with|PowerShell|4.0}}

```PowerShell

$path = "C:\Users"
if((Dir $path).Count -eq 0) {
    "$path is empty"
} else {
    "$path is not empty"
} 

```

<b>Output:</b>

```txt

C:\Users is not empty

```



## PureBasic


```purebasic
Procedure isDirEmpty(path$)
  If Right(path$, 1) <> "\": path$ + "\": EndIf
  Protected dirID = ExamineDirectory(#PB_Any, path$, "*.*")
  Protected result
  
  If dirID
    result = 1
    While NextDirectoryEntry(dirID)
      If DirectoryEntryType(dirID) = #PB_DirectoryEntry_File Or (DirectoryEntryName(dirID) <> "." And DirectoryEntryName(dirID) <> "..")
        result = 0
        Break
      EndIf 
    Wend 
    FinishDirectory(dirID)
  EndIf 
  ProcedureReturn result
EndProcedure

Define path$, result$

path$ = PathRequester("Choose a path", "C:\")
If path$
  If isDirEmpty(path$)
    result$ = " is empty."
  Else
    result$ = " is not empty."
  EndIf 
  MessageRequester("Empty directory test", #DQUOTE$ + path$ + #DQUOTE$ + result$)
EndIf 
```

{{out}} when selecting directories "L:\vv\6\" and "L:\vv\" :

```txt
"L:\vv\6\" is empty.

"L:\vv\" is not empty.
```



## Python

{{works with|Python|2.x}}

```python
import os;
if os.listdir(raw_input("directory")):
    print "not empty"
else:
    print "empty"

```



## Racket



```racket

#lang racket
(empty? (directory-list "some-directory"))

```



## REXX

{{works with|Regina}}
The following program was tested in a DOS window under Windows/XP and should work for all Microsoft Windows.

```rexx
/*REXX pgm checks to see if a directory is empty; if not, lists entries.*/
parse arg xdir;  if xdir='' then xdir='\someDir' /*Any DIR? Use default.*/
@.=0                                   /*default in case ADDRESS fails. */
trace off                              /*suppress REXX err msg for fails*/
address system 'DIR' xdir '/b' with output stem @.  /*issue the DIR cmd.*/
if rc\==0  then do                                  /*an error happened?*/
                say '***error!*** from DIR' xDIR    /*indicate que pasa.*/
                say 'return code='  rc              /*show the ret Code.*/
                exit rc                             /*exit with the  RC.*/
                end                                 /* [↑]  bad address.*/
#=@.rc                                              /*number of entries.*/
if #==0  then #='   no   '                          /*use a word, ¬zero.*/
say center('directory ' xdir " has "     #     ' entries.',79,'─')
exit @.0+rc                            /*stick a fork in it, we're done.*/
```

{{out}} when the following input was used:   <tt> temp </tt>

```txt

──────────────────────directory  temp  has  10  entries.───────────────────────

```

{{out}} when the following input was used:   <tt> \someDir </tt>

```txt

───────────────────directory  \someDir  has    no   entries.───────────────────

```



## Ring


```ring

myList = dir("C:\Ring\bin")
if len(myList) > 0 see "C:\Ring\bin is not empty" + nl
else see "C:\Ring\bin is empty" + nl ok

```



## Ruby

Raises a SystemCallError if the named directory doesn’t exist.

```ruby
Dir.entries("testdir").empty? 
```



## Run BASIC


```runbasic
files #f, DefaultDir$ + "\*.*"         ' open some directory. 
 
print "hasanswer: ";#f HASANSWER()     ' if it has an answer it is not MT
print "rowcount:  ";#f ROWCOUNT()      ' if not MT, how many files?
```



## Rust


```rust
use std::fs::read_dir;
use std::error::Error;

fn main() {
    for path in std::env::args().skip(1) { // iterate over the arguments, skipping the first (which is the executable)
        match read_dir(path.as_str()) { // try to read the directory specified
            Ok(contents) => {
                let len = contents.collect::<Vec<_>>().len(); // calculate the amount of items in the directory
                if len == 0 {
                    println!("{} is empty", path);
                } else {
                    println!("{} is not empty", path);
                }
            },
            Err(e) => { // If the attempt failed, print the corresponding error msg
                println!("Failed to read directory \"{}\": {}", path, e.description());
            }
        }
    }
}
```



## Scala


```scala
import java.io.File

def isDirEmpty(file:File) : Boolean =
   return file.exists && file.isDirectory && file.list.isEmpty
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const func boolean: dirEmpty (in string: dirName) is
  return fileType(dirName) = FILE_DIR and length(readDir(dirName)) = 0;

const proc: main is func
  begin
    writeln(dirEmpty("somedir"));
  end func;
```



## Sidef

Built-in method:

```ruby
Dir.new('/my/dir').is_empty;    # true, false or nil
```


User-defined function:

```ruby
func is_empty(dir) {
    dir.open(\var dir_h) || return nil;
    dir_h.each { |file|
        file ~~ ['.', '..'] && next;
        return false;
    };
    return true;
};
```



## Standard ML


```sml
fun isDirEmpty(path: string) = 
  let
    val dir = OS.FileSys.openDir path
    val dirEntryOpt = OS.FileSys.readDir dir
  in
    (
      OS.FileSys.closeDir(dir);
      case dirEntryOpt of 
        NONE => true
      | _    => false
    )
  end;
```




## Tcl


```tcl
proc isEmptyDir {dir} {
    # Get list of _all_ files in directory
    set filenames [glob -nocomplain -tails -directory $dir * .*]
    # Check whether list is empty (after filtering specials)
    expr {![llength [lsearch -all -not -regexp $filenames {^\.\.?$}]]}
}
```



## UNIX Shell


```bash

#!/bin/sh
DIR=/tmp/foo
[ `ls -a $DIR|wc -l` -gt 2 ]  && echo $DIR is NOT empty || echo $DIR is empty

```



## VBA


```vb
Sub Main()
    Debug.Print IsEmptyDirectory("C:\Temp")
    Debug.Print IsEmptyDirectory("C:\Temp\")
End Sub

Private Function IsEmptyDirectory(D As String) As Boolean
Dim Sep As String
    Sep = Application.PathSeparator
    D = IIf(Right(D, 1) <> Sep, D & Sep, D)
    IsEmptyDirectory = (Dir(D & "*.*") = "")
End Function
```



## VBScript


```vb

Function IsDirEmpty(path)
	IsDirEmpty = False
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	Set objFolder = objFSO.GetFolder(path)
	If objFolder.Files.Count = 0 And objFolder.SubFolders.Count = 0 Then
		IsDirEmpty = True
	End If
End Function

'Test
WScript.StdOut.WriteLine IsDirEmpty("C:\Temp")
WScript.StdOut.WriteLine IsDirEmpty("C:\Temp\test")

```


{{Out}}

```txt

False
True

```



## zkl


```zkl
path:="Empty"; File.isDir(path).println();
File.mkdir(path); File.isDir(path).println();
File.glob(path+"/*").println(); // show contents of directory
```

{{out}}

```txt

False
True
L()

```


{{omit from|ACL2}}
{{omit from|TI-83 BASIC|Does not have a filesystem.}}
