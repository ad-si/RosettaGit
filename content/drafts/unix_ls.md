+++
title = "Unix/ls"
description = ""
date = 2019-10-14T09:48:07Z
aliases = []
[extra]
id = 17682
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a program that will list everything in the current folder,   similar to:
:::*   the Unix utility   “<tt>ls</tt>”   [http://man7.org/linux/man-pages/man1/ls.1.html]       or
:::*   the Windows terminal command   “<tt>DIR</tt>”



The output must be sorted, but printing extended details and producing multi-column output is not required.


;Example output
For the list of paths:

```txt

/foo/bar
/foo/bar/1
/foo/bar/2
/foo/bar/a
/foo/bar/b

```



When the program is executed in   `/foo`,   it should print:

```txt

bar

```

and when the program is executed in   `/foo/bar`,   it should print:

```txt

1
2
a
b

```






## 8th


```forth

"*" f:glob
' s:cmp a:sort
"\n" a:join .

```



## Ada


```Ada
with Ada.Text_IO, Ada.Directories, Ada.Containers.Indefinite_Vectors;

procedure Directory_List is

   use Ada.Directories, Ada.Text_IO;
   Search: Search_Type; Found: Directory_Entry_Type;
   package SV is new Ada.Containers.Indefinite_Vectors(Natural, String);
   Result: SV.Vector;
   package Sorting is new SV.Generic_Sorting; use Sorting;
   function SName return String is (Simple_Name(Found));

begin
   -- search directory and store it in Result, a vector of strings
   Start_Search(Search, Directory => ".", Pattern =>"");
   while More_Entries(Search) loop
      Get_Next_Entry(Search, Found);
      declare
         Name: String := Simple_Name(Found);
      begin
         if Name(Name'First) /= '.' then
            Result.Append(Name);
         end if; -- ingnore filenames beginning with "."
      end;
   end loop; -- Result holds the entire directory in arbitrary order

   Sort(Result); -- Result holds the directory in proper order

   -- print Result
   for I in Result.First_Index .. Result.Last_Index loop
      Put_Line(Result.Element(I));
   end loop;
end Directory_List;
```




## Aime


```aime
record r;
file f;
text s;

f.opendir(1.argv);

while (~f.case(s)) {
    if (s != "." && s != "..") {
        r[s] = 0;
    }
}

r.vcall(o_, 0, "\n");
```



## Arturo



```arturo
print $(dir)
```




## AWK

{{works with|gawk}} "BEGINFILE" is a gawk-extension

```AWK

# syntax: GAWK -f UNIX_LS.AWK * | SORT
BEGINFILE {
    printf("%s\n",FILENAME)
    nextfile
}
END {
    exit(0)
}

```


Sample commands and output under Windows 8:

```txt

REM create folders and files
MKDIR c:\foo\bar
CD /D c:\foo\bar
GAWK "BEGIN{x=\"12ab\";for(i=1;i<=length(x);i++){print(i)>substr(x,i,1)}}"
REM run test
CD /D c:\foo
GAWK -f UNIX_LS.AWK * | SORT
bar
CD /D c:\foo\bar
GAWK -f UNIX_LS.AWK * | SORT
1
2
a
b

```


{{works with|gawk}}
To replicate 'ls .'

```txt

gawk -lreaddir 'BEGIN { FS = "/" } {print $2}' .

```


{{works with|gawk}}
To replicate 'ls examplefile.txt'

```txt

gawk -lfilefuncs -lreaddir 'BEGIN { FS = "/"; stat(ARGV[1], fd); if(fd["type"] == "file") {print ARGV[1]; exit} } { print $2}' examplefile.txt

```



## BaCon


```freebasic
' Emulate ls
cnt% = 0
files$ = ""
OPEN CURDIR$ FOR DIRECTORY AS mydir
GETFILE myfile$ FROM mydir
WHILE ISTRUE(LEN(myfile$))
    IF LEFT$(myfile$, 1) != "." THEN
        INCR cnt%
        files$ = APPEND$(files$, cnt%, UNFLATTEN$(myfile$))
    ENDIF
    GETFILE myfile$ FROM mydir
WEND
CLOSE DIRECTORY mydir
IF cnt% > 0 THEN
    FOR f$ IN SORT$(files$)
        PRINT FLATTEN$(f$)
    NEXT
ENDIF
```



## C

C does not have any os-independent way of reading a directory. The following uses readdir and should work on any Unix system.

```C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

int cmpstr(const void *a, const void *b)
{
    return strcmp(*(const char**)a, *(const char**)b);
}

int main(void)
{
    DIR *basedir;
    char path[PATH_MAX];
    struct dirent *entry;
    char **dirnames;
    int diralloc = 128;
    int dirsize  = 0;

    if (!(dirnames = malloc(diralloc * sizeof(char*)))) {
        perror("malloc error:");
        return 1;
    }

    if (!getcwd(path, PATH_MAX)) {
        perror("getcwd error:");
        return 1;
    }

    if (!(basedir = opendir(path))) {
        perror("opendir error:");
        return 1;
    }

    while ((entry = readdir(basedir))) {
        if (dirsize >= diralloc) {
            diralloc *= 2;
            if (!(dirnames = realloc(dirnames, diralloc * sizeof(char*)))) {
                perror("realloc error:");
                return 1;
            }
        }
        dirnames[dirsize++] = strdup(entry->d_name);
    }

    qsort(dirnames, dirsize, sizeof(char*), cmpstr);

    int i;
    for (i = 0; i < dirsize; ++i) {
        if (dirnames[i][0] != '.') {
            printf("%s\n", dirnames[i]);
        }
    }

    for (i = 0; i < dirsize; ++i)
        free(dirnames[i]);
    free(dirnames);
    closedir(basedir);
    return 0;
}

```



## C++

{{libheader|Boost}}

```cpp

#include <iostream>
#include <set>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main(void)
{
    fs::path p(fs::current_path());
    std::set<std::string> tree;

    for (auto it = fs::directory_iterator(p); it != fs::directory_iterator(); ++it)
        tree.insert(it->path().filename().native());

    for (auto entry : tree)
        std::cout << entry << '\n';
}

```

## C#

```c#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Unix_ls
{
    public class UnixLS
    {
        public static void Main(string[] args)
        {
            UnixLS ls = new UnixLS();
            ls.list(args.Length.Equals(0) ? "." : args[0]);
        }

        private void list(string folder)
        {
            foreach (FileSystemInfo fileSystemInfo in new DirectoryInfo(folder).EnumerateFileSystemInfos("*", SearchOption.TopDirectoryOnly))
            {
                Console.WriteLine(fileSystemInfo.Name);
            }
        }
    }
}
```



## Clojure


```clojure
(def files (sort (filter #(= "." (.getParent %)) (file-seq (clojure.java.io/file ".")))))

(doseq [n files] (println (.getName n)))
```



## Common Lisp

In some implementations, `(directory)` results automatically include subdirectories (e.g. SBCL); some require them to be requested separately (e.g. CLISP).
The function below asks for both and then removes any duplicates from the resulting list.

The workhorse is `files-list`, which returns a list of filenames.  The `ls` function sorts the resulting list and formats it for output.


```lisp
(defun files-list (&optional (path "."))
  (let* ((dir (concatenate 'string path "/"))
         (abs-path (car (directory dir)))
         (file-pattern (concatenate 'string dir "*"))
         (subdir-pattern (concatenate 'string file-pattern "/")))
    (remove-duplicates
       (mapcar (lambda (p) (enough-namestring p abs-path))
               (mapcan #'directory (list file-pattern subdir-pattern)))
       :test #'string-equal)))

(defun ls (&optional (path "."))
  (format t "~{~a~%~}" (sort (files-list path) #'string-lessp)))
```



## D


```d
void main() {
    import std.stdio, std.file, std.path, std.array, std.algorithm;

    foreach (const string path; dirEntries(getcwd, SpanMode.shallow).array.sort)
        path.baseName.writeln;
}
```



## EchoLisp

No directory in EchoLisp, which is run in a browser window. Instead, "stores" (folders) and keys in stores (file names) are located in local storage.

```lisp

;; ls of stores (kind of folders)
(for-each writeln (list-sort < (local-stores))) →
    AGES
    NEMESIS
    info
    objects.dat
    reader
    system
    user
    words

;; ls of "NEMESIS" store
(for-each writeln (local-keys "NEMESIS")) →
    Alan
    Glory
    Jonah

```



## Elixir


```elixir
iex(1)> ls = fn dir -> File.ls!(dir) |> Enum.each(&IO.puts &1) end
#Function<6.54118792/1 in :erl_eval.expr/5>
iex(2)> ls.("foo")
bar
:ok
iex(3)> ls.("foo/bar")
1
2
a
b
:ok
```



## Erlang


```erlang

1> Ls = fun(Dir) ->
1> {ok, DirContents} = file:list_dir(Dir),
1> [io:format("~s~n", [X]) || X <- lists:sort(DirContents)]
1> end.
#Fun<erl_eval.6.36634728>
2> Ls("foo").
bar
[ok]
3> Ls("foo/bar").
1
2
a
b
[ok,ok,ok,ok]

```


=={{header|F_Sharp|F#}}==
<p>Works with .NET framework 4.</p>

```fsharp
let ls = DirectoryInfo(".").EnumerateFileSystemInfos() |> Seq.map (fun i -> i.Name) |> Seq.sort |> Seq.iter (printfn "%s")
```

<p>Prior to .NET4 you had to enumerate files and directories separately.</p>
<p>The call to <code>sort</code> is probably redundant, since "sorted by name" seems to be the default in Windows.</p>


## Forth

This is much easier without the 'sorted output' requirement:

```forth
256 buffer: filename-buf
: each-filename { xt -- }  \ xt-consuming variant
  s" ." open-dir throw { d }
  begin filename-buf 256 d read-dir throw while
    filename-buf swap xt execute
  repeat  d close-dir throw ;

\ immediate variant
: each-filename[  s" ." postpone sliteral ]] open-dir throw >r begin filename-buf 256 r@ read-dir throw while filename-buf swap [[ ; immediate compile-only
: ]each-filename  ]] repeat drop r> close-dir throw [[ ; immediate compile-only

: ls ( -- )  [: cr type ;] each-filename ;
```


Given that requirement, we must first generate a sorted array of filenames:
{{libheader|Forth Foundation Library}}

```forth
: save-string ( c-addr u -- a )
  dup 1+ allocate throw dup >r place r> ;

require ffl/car.fs
: sorted-filenames ( -- car )
  0 car-new { a }
  [: swap count rot count compare ;] a car-compare!
  each-filename[ save-string a car-insert-sorted ]each-filename
  a ;

: each-sorted-filename ( xt -- )
  sorted-filenames { a }  a car-execute  [: free throw ;] a car-execute  a car-free ;

: ls ( -- )
  [: count cr type ;] each-sorted-filename ;

```



## Fortran

This is possible only for those Fortran compilers that offer some sort of interface with the operating system's file handling routines. Not standard at all!
```Fortran
      PROGRAM LS		!Names the files in the current directory.
      USE DFLIB			!Mysterious library.
      TYPE(FILE$INFO) INFO	!With mysterious content.
      NAMELIST /HIC/INFO	!This enables annotated output.
      INTEGER MARK,L		!Assistants.

      MARK = FILE$FIRST		!Starting state.
Call for the next file.
   10 L = GETFILEINFOQQ("*",INFO,MARK)	!Mystery routine returns the length of the file name.
      IF (MARK.EQ.FILE$ERROR) THEN	!Or possibly, not.
        WRITE (6,*) "Error!",L		!Something went wrong.
        WRITE (6,HIC)			!Reveal INFO, annotated.
        STOP "That wasn't nice."	!Quite.
      ELSE IF (IAND(INFO.PERMIT,FILE$DIR) .EQ. 0) THEN	!Not a directory.
        IF (L.GT.0) WRITE (6,*) INFO.NAME(1:L)	!The object of the exercise!
      END IF				!So much for that entry.
      IF (MARK.NE.FILE$LAST) GO TO 10	!Lastness is discovered after the last file is fingered.
      END	!If FILE$LAST is not reached, "system resources may be lost."
```

This relies on the supplied routine GETFILEINFOQQ, which is not at all a standard routine, but it does behave in the same way as is found in many other systems, notably with a file name selection filter, here chosen to be "*" meaning "any file". It supplies successive file names and requires mysterious parameters to keep track of what it is doing. In the installation file C:/Compilers/Furrytran/Compaq Furrytran 6.6a CD/X86/DF/INCLUDE/DFLIB.F90, there is the following segment:
```Fortran
      INTERFACE
	INTEGER*4 FUNCTION GETFILEINFOQQ(FILES, BUFFER,dwHANDLE)
!DEC$ ATTRIBUTES DEFAULT :: GETFILEINFOQQ
	  CHARACTER*(*) FILES
	  STRUCTURE / FILE$INFO /
	    INTEGER*4       CREATION          ! Creation time (-1 on FAT)
	    INTEGER*4       LASTWRITE         ! Last write to file
	    INTEGER*4       LASTACCESS        ! Last access (-1 on FAT)
	    INTEGER*4       LENGTH            ! Length of file
	    INTEGER*2       PERMIT            ! File access mode
	    CHARACTER*255   NAME              ! File name
	  END STRUCTURE
	  RECORD / FILE$INFO / BUFFER
	  INTEGER*4 dwHANDLE
	END FUNCTION
      END INTERFACE
```

Getting this to work was quite annoying. It turned out that the irritating "files" . and .. are deemed a directory (via the bit in INFO.PERMIT matching that of FILE$DIR = 16) and so can be skipped along with proper subdirectories, but the "PERMIT" value of -1 returned for the FILE$LAST state also matches, though its (non-existent) file name length is given as zero. Thus, if one skips directories filter-style by IF ... GO TO 10, in such a case the end will never be seen. Further, although Fortran syntax allows <code>INFO.PERMIT .AND. FILE$DIR</code> the bit values of logical variables are strange. Instead, what is needed is <code>IAND(INFO.PERMIT,FILE$DIR)</code>

Further vexation was due to the compiler's "help" system giving PERMIT as a 32-bit integer in its example. Copying the declaration to give a type name not involving a dollar symbol foundered because the type checking done by the compiler for the parameters of the function is based not on the contents of the type matching, but on the name of the type matching. So, one is stuck with the $. In a mood for retaliation, some special tests were made, involving a pause for input after each name was revealed. Removing the file just named before responding to the request for input did not prevent the next file from being named, nor (in another run) did removing the file next to be named: although it was gone, it was still named in the next step as if it were still there, and this worked even if INFO.NAME were scrubbed each time as well. Evidently, there could arise transient problems when using this scheme in a file system undergoing turbulence.

As for the ordering of results, on this Windows XP system, the file names came out ordered so there is no need to mess about with a storage area to sort the names in.


## FunL


```funl
import io.File

for f <- sort( list(File( "." ).list()).filterNot(s -> s.startsWith(".")) )
    println( f )
```


{{out}}

The above script has been placed in a file called <code>ls.lf</code> which has been placed in the home directory.


```txt

$ sudo mkdir -p /foo/bar
$ cd /foo/bar
$ sudo touch 1 2 a b
$ cd ..
$ funl ~/ls
bar
$ cd bar
$ funl ~/ls
1
2
a
b
$

```



## Gambas


```gambas
Public Sub Main()
Dim sDir As String[] = Dir(User.Home &/ "test").Sort()

Print sDir.Join(gb.NewLine)

End
```

Output:

```txt

a.txt
b.txt
c.txt
d.txt
e.txt

```



## Go


```go
package main

import (
	"fmt"
	"log"
	"os"
	"sort"
)

func main() {
	f, err := os.Open(".")
	if err != nil {
		log.Fatal(err)
	}
	files, err := f.Readdirnames(0)
	f.Close()
	if err != nil {
		log.Fatal(err)
	}
	sort.Strings(files)
	for _, n := range files {
		fmt.Println(n)
	}
}
```



## Haskell


{{Works with|GHC|7.8.3}}


```haskell
import Control.Monad
import Data.List
import System.Directory

dontStartWith = flip $ (/=) . head

main = do
  files <- getDirectoryContents "."
  mapM_ putStrLn $ sort $ filter (dontStartWith '.') files
```



## J

See the [http://www.jsoftware.com/wsvn/base8/trunk/main/main/dir.ijs dir.ijs script] for a full description of the interface for <code>dir</code>:

```J
   dir ''       NB. includes properties
   >1 1 dir ''  NB. plain filename as per task
```



## Java

{{Works with|Java|8}}

```java

package rosetta;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class UnixLS {

	public static void main(String[] args) throws IOException {
		UnixLS ls = new UnixLS();
		ls.list(System.out);
	}

	private void list(PrintStream out) throws IOException {
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(Paths.get("."))) {
			stream.forEach((path) -> out.println(path.getFileName()));
		}
	}
}


```



## Javascript


{{Works with|Node.js|4.3.2+}}

```javascript
const fs = require('fs');
fs.readdir('.', (err, names) => names.sort().map( name => console.log(name) ));
```



## Jsish

Jsi provides a '''File''' module with a '''glob''' method.

```txt
# help File.glob
File.glob(pattern:regexp|string|null='*', options:function|object|null=void):array
Return list of files in dir with optional pattern match.
With no arguments (or null) returns all files/directories in current directory.
The first argument can be a pattern (either a glob or regexp) of the files to return.
When the second argument is a function, it is called with each path, and filter on false.
Otherwise second argument must be a set of options.
```


To emulate ''ls'', sorted, one entry per line:


```javascript
puts(File.glob().sort().join('\n'));
```



## Julia


```julia
# v0.6.0

for e in readdir() # Current directory
    println(e)
end

# Same for...
readdir("~") # Read home directory
readdir("~/documents")
```



## Kotlin


```scala
// Version 1.2.41

import java.io.File

fun ls(directory: String) {
    val d = File(directory)
    if (!d.isDirectory) {
        println("$directory is not a directory")
        return
    }
    d.listFiles().map { it.name }
                 .sortedBy { it.toLowerCase() }  // case insensitive
                 .forEach { println(it) }
}

fun main(args: Array<String>) {
    ls(".")  // list files in current directory, say
}
```



## LiveCode


```LiveCode
set the defaultFolder to "/foo"
put the folders & the files
set the defaultFolder to "/foo/bar"
put the folders & the files
```



## Lua

Using LuaFileSystem - available in LuaRocks, ULua, major Linux distro repos, etc, etc.

```Lua
require("lfs")
for file in lfs.dir(".") do print(file) end
```



## Mathematica



```Mathematica
Column[FileNames[]]
```



## Nim


```Nim

import os

for kind, path in walkDir(getCurrentDir(), true):
  echo path

```



## Objeck


```objeck

class Test {
  function : Main(args : String[]) ~ Nil {
    file_names := System.IO.File.Directory->List(".");
    each(i : file_names) {
      file_name := file_names[i];
      if(System.IO.File.Directory->Exists(file_name)) {
        file_name += '/';
      };
      file_name->PrintLine();
    };
  }
}

```



## OCaml



```ocaml
let () =
  Array.iter print_endline (
    Sys.readdir Sys.argv.(1) )
```


{{Output}}
 $ cd /foo/bar
 $ ocaml ls.ml
 1
 2
 a
 b


## PARI/GP

GP doesn't have this capability so we can either use the shell or PARI. For the latter see [[#C|C]]; for the former:

```parigp
system("dir/b/on")
```

in DOS/Windows or

```parigp
system("ls")
```

in *nix.


## Pascal

This is the example in the Turbo Pascal 4 manual. With Turbo Pascal and old-style DOS file names, all file names come out in capitals, further, names not fitting into the "8.3" style (of up to eight characters followed by an extension of up to three characters) are presented with ad-hoc names fitting that style, so for example, Tab2Comma.exe comes out as TAB2CO~1.EXE. The same source file compiles unchanged via the Free Pascal compiler, whereupon long file names appear with capitals and lower-case letters rather than all-capitals.

When tested via Windows XP, the names came out in sorted order (ignoring case) however in earlier systems the files would be presented in entry order. That is, if files a, c, b were saved, they would be named in that order. Then, if file c were deleted and then a file named x were added, they would be named in the order a, x, b. In this case, a scheme for saving an unknown number of names (of unknown length) would be needed so that they could be sorted. Perhaps some linked-list with an insertionsort for each added name...

```Pascal

Program ls;	{To list the names of all files/directories in the current directory.}
 Uses DOS;
 var DirInfo: SearchRec;	{Predefined. See page 403 of the Turbo Pascal 4 manual.}
 BEGIN
  FindFirst('*.*',AnyFile,DirInfo);	{AnyFile means any file name OR directory name.}
  While DOSerror = 0 do			{Result of FindFirst/Next not being a function, damnit.}
   begin
    WriteLn(DirInfo.Name);
    FindNext(DirInfo);
   end;
 END.

```


## Perl



```perl
opendir my $handle, '.' or die "Couldnt open current directory: $!";
while (readdir $handle) {
    print "$_\n";
}
closedir $handle;
```


Alternatively, using <tt>glob</tt>:

```perl
print "$_\n" for glob '*';
```



```perl
print "$_\n" for glob '* .*';  # If you want to include dot files
```



## Perl 6


There is a <tt>dir</tt> builtin command which returns a list of IO::Path objects.  We stringify them all with a hyperoperator before sorting the strings.


```perl6
.say for sort ~«dir
```



## Phix


```Phix
pp(dir("."),{pp_Nest,1})
```

{{Out}}

```txt

{{".", "d", 0,2017,4,3,21,0,7},
 {"..", "d", 0,2017,4,3,21,0,7},
 {".hg", "d", 0,2017,3,27,13,53'5',38'&'},
 {".hgignore", "a", 15330,2017,3,27,13,29,21},
 {"alice_oz.txt", "a", 336926,2017,3,15,19,12,24},
 {"asm", "d", 0,2017,4,2,20,33'!',39'''},
...etc

```



## PHP


This will output all the filenames in the current directory.


```php

<?php
foreach(scandir('.') as $fileName){
    echo $fileName."\n";
}

```



## PicoLisp


```PicoLisp
(for F (sort (dir))
   (prinl F) )
```



## PowerShell


```PowerShell
# Prints Name, Length, Mode, and LastWriteTime
Get-ChildItem | Sort-Object Name | Write-Output

# Prints only the name of each file in the directory
Get-ChildItem | Sort-Object Name | ForEach-Object Name | Write-Output
```



## Python


```python>>>
 import os
>>> print('\n'.join(sorted(os.listdir('.'))))
DLLs
Doc
LICENSE.txt
Lib
NEWS.txt
README.txt
Scripts
Tools
include
libs
python.exe
pythonw.exe
tcl
>>>
```



## R


```rsplus

cat(paste(list.files(), collapse  = "\n"), "\n")
cat(paste(list.files("bar"), collapse = "\n"), "\n")

```

{{out}}

```txt

bar
1
2
a
b

```



## Racket


Ooh... warning... if you run the <code>test</code> module (either with DrRacket with the test module automatically running, or with <code>raco test ls.rkt</code>, then the example directory tree is built but not torn down.


```racket
#lang racket/base

;; Racket's `directory-list' produces a sorted list of files
(define (ls) (for-each displayln (directory-list)))

;; Code to run when this file is running directly
(module+ main
  (ls))

(module+ test
  (require tests/eli-tester racket/port racket/file)
  (define (make-directory-tree)
    (make-directory* "foo/bar")
    (for ([f '("1" "2" "a" "b")])
      (with-output-to-file (format "foo/bar/~a"f) #:exists 'replace newline)))
  (make-directory-tree)
  (define (ls/str dir)
    (parameterize ([current-directory dir]) (with-output-to-string ls)))
  (test (ls/str "foo") => "bar\n"
        (ls/str "foo/bar") => "1\n2\na\nb\n"))
```


Both tests pass.


## REXX

The following program works under Windows and used the Windows DIR command to list a bare-bones sorted list.

```rexx
/*REXX program lists contents of current folder  (ala mode UNIX's  LS). */
'DIR /b /oN'                           /*use Windows DIR: sorts & lists.*/
                                       /*stick a fork in it, we're done.*/
```

Notes on the options used for the '''DIR''' command:
::::*   '''b'''   is for <u>'''b'''</u>are format (no heading information or summary).
::::*   '''o'''   is for <u>'''o'''</u>rder, and it orders (sorts) by file <u>'''N'''</u>ame.


## Ruby


```ruby

Dir.foreach("./"){|n| puts n}

```

This will output all files including hidden ones e.g. '.' and '..'.

## Run BASIC


```Runbasic
files #f, DefaultDir$ + "\*.*" 	' RunBasic Default directory.. Can be any directroy
print "rowcount: ";#f ROWCOUNT()		' how many rows in directory
#f DATEFORMAT("mm/dd/yy") 			'set format of file date or not
#f TIMEFORMAT("hh:mm:ss") 			'set format of file time or not
count = #f rowcount()
for i = 1 to count				' loop thru the row count
print "info: ";#f nextfile$()			' file info
print "name: ";#f NAME$()			' Name of file
print "size: ";#f SIZE()			' size
print "date: ";#f DATE$()			' date
print "time: ";#f TIME$()			' time
print "isdir: ";#f ISDIR()			' 1 = is a directory
next
```

This will output RunBasics Default Directory.. It can be any directory

```txt
rowcount: 30
info: antiGram1.bas,1743,08/02/16,08:34:50,
name: antiGram1.bas
size: 1743
date: 08/02/16
time: 08:34:50
isdir: 0
info: avionics.db,0,05/09/16,09:02:01,
name: avionics.db
size: 0
date: 05/09/16
time: 09:02:01
isdir: 0
...
```



## Rust


```rust
use std::{env, fmt, fs, process};
use std::io::{self, Write};
use std::path::Path;

fn main() {
    let cur = env::current_dir().unwrap_or_else(|e| exit_err(e, 1));
    let arg = env::args().nth(1);
    print_files(arg.as_ref().map_or(cur.as_path(), |p| Path::new(p)))
        .unwrap_or_else(|e| exit_err(e, 2));
}

#[inline]
fn print_files(path: &Path) -> io::Result<()> {
    for x in try!(fs::read_dir(path)) {
        println!("{}", try!(x).file_name().to_string_lossy());
    }
    Ok(())
}

#[inline]
fn exit_err<T>(msg: T, code: i32) -> ! where T: fmt::Display {
    writeln!(&mut io::stderr(), "{}", msg).expect("Could not write to stderr");
    process::exit(code)
}
```


{{out}}

```txt

$ mkdir -p foo/bar
$ ./unix_ls
foo
unix_ls
$ cd foo/bar
$ touch a b 1 2
$ cd ../..
$ ./unix_ls foo
bar
$ ./unix_ls foo/bar
1
2
a
b

```



=={{header|S-lang}}==
<lang S-lang>variable d = listdir(getcwd()), p;
foreach p (array_sort(d))
  () = printf("%s\n", d[p] );
```



## Scala

{{out}}

```txt

scala> new java.io.File("/").listFiles.sorted.foreach(println)
/bin
/boot
/core
/dev
/etc
/home
/lib
/lib64
/local
/lost+found
/media
/mnt
/opt
/proc
/root
/run
/sbin
/selinux
/srv
/sys
/tmp
/user
/usr
/var
```


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  local
    var string: name is "";
  begin
    for name range readDir(".") do
       writeln(name);
    end for;
  end func;
```



## Sidef

Explicit, by opening the current working directory:

```ruby
var content = [];
Dir.cwd.open.each { |file|
    file ~~ < . .. > && next;
    content.append(file);
}

content.sort.each { |file|
    say file;
}
```


Implicit, by using the <i>String.glob</i> method:

```ruby
'*'.glob.each { |file|
    say file;
}
```



## Standard ML


```sml
OS.Process.system "ls -a"
```

Doing it all by yourself:

```sml

local   (* make a sort function *)
  val rec insert = fn s :string =>fn [] => [s]
	| ll as h::t => if s<=h then s::ll else h::insert s t;
in
  val rec sort = fn [] => [] | h::t => insert h (sort t)
end;

open Posix.FileSys ;
val istream = opendir "." ;
val ll =  ref [readdir istream] ;
while ( isSome (hd (!ll)) ) do  ( ll:=readdir istream :: !ll );
val result = List.map valOf (tl (!ll));
closedir istream ;

sort result;

```



## Stata

Stata has a builtin '''[http://www.stata.com/help.cgi?dir dir]''' command (or equivalently '''ls''').


```stata
. dir *.dta
   6.3k   6/12/17 14:26  auto.dta
   2.3k   8/10/17  7:34  titanium.dta
   6.0k   8/12/17  9:28  trend.dta
```



## Tcl


```tcl
puts [join [lsort [glob -nocomplain *]] "\n"]
```



## Ursa


```ursa
decl file f
decl string<> fnames
set fnames (sort (f.listdir "."))

for (decl int i) (< i (size fnames)) (inc i)
        out fnames<i> endl console
end for
```



## zkl


```zkl
File.glob("*").sort()
```

Lists all files and directories in the current directory. If you only want a list of files:

```zkl
File.glob("*",0x8).sort()
```

{{out}}

```txt

L("README","superball","testThemAll.log","zkl.exe","zkl_tests.zip","zkl_vm_src.zip")

```

The glob method uses Unix shell wild cards.

The globular method recurses down through the directories. It can send results to objects, functions, methods, threads, etc, etc. To get a sorted list of all the directories under the "Src" directory:

```zkl
File.globular("Src",*,True,0x10,List).sort().concat("\n")
```

{{out}}

```txt

Src/Compiler/
Src/Misc/
Src/Test/
Src/Time/
Src/Utils/
Src/ZenKinetic/
Src/ZenKinetic/Frame_O_Matic/
Src/ZenKinetic/GBalls/
Src/ZenKinetic/Twist and Draw/
Src/ZenKinetic/ZEd

```


{{omit from|SQL PL|Natively, it does not have something to access directories.}} <!-- If necessary, external functions written in Java or C can be written, to be used along SQL -->
{{omit from|TI-83 BASIC|Does not have a filesystem, just namespaced variables, which can't be listed from a program.}}
