+++
title = "Walk a directory/Non-recursively"
description = ""
date = 2019-09-07T17:18:20Z
aliases = []
[extra]
id = 1766
[taxonomies]
categories = ["task", "File System Operations"]
tags = []
+++

## Task

Walk a given directory and print the ''names'' of files matching a given pattern.

(How is "pattern" defined?  substring match?  DOS pattern?  BASH pattern?  ZSH pattern?  Perl regular expression?)


'''Note:''' This task is for non-recursive methods.   These tasks should read a ''single directory'', not an entire directory tree.

'''Note:''' Please be careful when running any code presented here.


## Related tasks

*   [[Walk Directory Tree]]   (read entire directory tree).





## 68000 Assembly

Non-recursive directory walk in Motorola 68000 assembly language under AmigaOs 2.04+ by Thorham. Uses regular Amiga dos pattern matching.

```68000devpac
;
; Non-recursive directory walk for Motorola 68000 under AmigaOs 2.04+ by Thorham
;

execBase equ 4

;
; from exec includes
;
_LVOOpenLibrary equ -552
_LVOCloseLibrary equ -414
_LVOAllocVec equ -684
_LVOFreeVec equ -690

MEMF_ANY equ 0

;
; from dos includes
;
_LVOVPrintf equ -954
_LVOExamine equ -102
_LVOExNext equ -108
_LVOLock equ -84
_LVOUnLock equ -90
_LVOParsePatternNoCase equ -966
_LVOMatchPatternNoCase equ -972

ACCESS_READ equ -2
                    rsset   0
fib_DiskKey         rs.l    1
fib_DirEntryType    rs.l    1
fib_FileName        rs.b    108
fib_Protection      rs.l    1
fib_EntryType       rs.l    1
fib_Size            rs.l    1
fib_NumBlocks       rs.l    1
fib_DateStamp       rs.b    12
fib_Comment         rs.b    80
fib_OwnerUID        rs.w    1
fib_OwnerGID        rs.w    1
fib_Reserved        rs.b    32
fib_SIZEOF          rs.b    0

;
; main
;

start
    move.l  execBase,a6

; open dos.library

    lea     dosName,a1
    moveq   #37,d0
    jsr     _LVOOpenLibrary(a6)
    move.l  d0,dosBase
    beq     exit

; allocate memory for file info block

    move.l  #fib_SIZEOF,d0
    move.l  #MEMF_ANY,d1
    jsr     _LVOAllocVec(a6)
    move.l  d0,fib
    beq     exit

; get directory lock

    move.l  dosBase,a6

    move.l  #pathString,d1
    move.l  #ACCESS_READ,d2
    jsr     _LVOLock(a6)
    move.l  d0,lock
    beq     exit

; examine directory for ExNext

    move.l  lock,d1
    move.l  fib,d2
    jsr     _LVOExamine(a6)
    tst.w   d0
    beq     exit

; parse pattern string

    move.l  #patternString,d1
    move.l  #patternParsed,d2
    move.l  #sizeof_patternString*2+2,d3
    jsr     _LVOParsePatternNoCase(a6)
    tst.l   d0
    blt     exit

; get some pointers for use in the loop

    lea     printfArgs,a2
    move.l  fib,a3
    lea     fib_FileName(a3),a3

.loop

; get next directory entry

    move.l  lock,d1
    move.l  fib,d2
    jsr     _LVOExNext(a6)
    tst.w   d0
    beq     exit

; match pattern

    move.l  #patternParsed,d1
    move.l  a3,d2
    jsr     _LVOMatchPatternNoCase(a6)

; if match then print file name

    tst.l   d0
    beq     .nomatch

    move.l  a3,(a2)
    move.l  #formatString,d1
    move.l  #printfArgs,d2
    jsr     _LVOVPrintf(a6)

.nomatch
    bra     .loop

; cleanup and exit

exit
    move.l  dosBase,a6
    move.l  lock,d1
    jsr     _LVOUnLock(a6)

    move.l  execBase,a6
    move.l  fib,a1
    tst.l   a1
    beq     .l1
    jsr     _LVOFreeVec(a6)
.l1
    move.l  dosBase,a1
    jsr     _LVOCloseLibrary(a6)
    rts

    section data,data_p
;
; variables
;
dosBase
    dc.l    0

lock
    dc.l    0

fib
    dc.l    0

printfArgs
    dc.l    0
;
; strings
;
dosName
    dc.b    "dos.library",0

pathString
    dc.b    "ram:",0

formatString
    dc.b    "%s",10,0

patternString
    dc.b    "#?",0
patternString_end
sizeof_patternString=patternString_end-patternString

patternParsed
    dcb.b   sizeof_patternString*2+2
```



## 8th


```forth

"*.c" f:glob \ puts an array of strings with the file names on the top of the stack

```



## Ada

```ada
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Walk_Directory
            (Directory : in String := ".";
             Pattern   : in String := "") -- empty pattern = all file names/subdirectory names
is
   Search  : Search_Type;
   Dir_Ent : Directory_Entry_Type;
begin
   Start_Search (Search, Directory, Pattern);

   while More_Entries (Search) loop
      Get_Next_Entry (Search, Dir_Ent);
      Put_Line (Simple_Name (Dir_Ent));
   end loop;

   End_Search (Search);
end Walk_Directory;
```



## ALGOL 68

<!-- {{does not work with|ALGOL 68|Standard - extensions to language used}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - ''get directory'' and'' grep in string'' not available in any library ... yet}} -->

```algol68
INT match=0, no match=1, out of memory error=2, other error=3;

[]STRING directory = get directory(".");
FOR file index TO UPB directory DO
  STRING file = directory[file index];
  IF grep in string("[Ss]ort*.[.]a68$", file, NIL, NIL) = match THEN
    print((file, new line))
  FI
OD
```

```txt

Quick_sort.a68
Shell_sort.a68
Cocktail_Sort.a68
Selection_Sort.a68
Merge_sort.a68
Bobosort.a68
Insertion_Sort.a68
Permutation_Sort.a68

```



## AppleScript

AppleScript itself has limited built-in file system access. Typically, the Mac OS Finder is used to gather such information.
To list all file/folders in the root directory:

```AppleScript
tell application "Finder" to return name of every item in (startup disk)
--> EXAMPLE RESULT: {"Applications", "Developer", "Library", "System", "Users"}
```

To list all pdf files in user's home directory:

```AppleScript
tell application "Finder" to return name of every item in (path to documents folder from user domain) whose name ends with "pdf"
--> EXAMPLE RESULT: {"About Stacks.pdf", "Test.pdf"}
```

The key clause is the <code>whose</code> modifier keyword. The Finder can interpret many variations, including such terms as <code>whose name begins with</code>, <code>whose name contains</code>, etc. As well as boolean combinations:

```AppleScript
tell application "Finder" to return name of every item in (path to documents folder from user domain) whose name does not contain "about" and name ends with "pdf"
--> RETURNS: {"Test.pdf"}
```

The Finder also supports the <code>entire contents</code> modifier keyword, which effectively performs a recursive directory scan without recursion.

```AppleScript
tell application "Finder" to return name of every item in entire contents of (path to documents folder from user domain) whose name ends with "pdf"
```



## AutoHotkey

Display all INI files in Windows directory.

```autohotkey
Loop, %A_WinDir%\*.ini
 out .= A_LoopFileName "`n"
MsgBox,% out
```



## BaCon

This code will print all files in the current directory ".", separated by a newline symbol:

```qbasic
PRINT WALK$(".", 1, ".+", FALSE, NL$)
```



## BASIC

{{works with|QuickBASIC|7}} (older versions don't have <code>DIR$</code>)

DOS wildcards are rather underpowered when compared to... well... anything else.


```qbasic
DECLARE SUB show (pattern AS STRING)

show "*.*"

SUB show (pattern AS STRING)
    DIM f AS STRING
    f = DIR$(pattern)
    DO WHILE LEN(f)
        PRINT f
        f = DIR$
    LOOP
END SUB
```



## Batch File

A simple command that displays all EXE files in System32 directory non-recursively.

```dos
dir /b "%windir%\system32\*.exe"
```

The same command inside FOR loop:
*Inside a Batch File:

```dos
@for /F "tokens=*" %%F in ('dir /b "%windir%\system32\*.exe"') do echo %%F
```

*Command-line:

```dos
for /F "tokens=*" %F in ('dir /b "%windir%\system32\*.exe"') do echo %F
```



## BBC BASIC

```bbcbasic
      directory$ = "C:\Windows\"
      pattern$ = "*.ini"
      PROClistdir(directory$ + pattern$)
      END

      DEF PROClistdir(afsp$)
      LOCAL dir%, sh%, res%
      DIM dir% LOCAL 317
      SYS "FindFirstFile", afsp$, dir% TO sh%
      IF sh% <> -1 THEN
        REPEAT
          PRINT $$(dir%+44)
          SYS "FindNextFile", sh%, dir% TO res%
        UNTIL res% = 0
        SYS "FindClose", sh%
      ENDIF
      ENDPROC
```



## C

In this example, the pattern is a [[POSIX]] extended regular expression.

```c
#include <sys/types.h>
#include <dirent.h>
#include <regex.h>
#include <stdio.h>

enum {
    WALK_OK = 0,
    WALK_BADPATTERN,
    WALK_BADOPEN,
};

int walker(const char *dir, const char *pattern)
{
    struct dirent *entry;
    regex_t reg;
    DIR *d;

    if (regcomp(&reg, pattern, REG_EXTENDED | REG_NOSUB))
        return WALK_BADPATTERN;
    if (!(d = opendir(dir)))
        return WALK_BADOPEN;
    while (entry = readdir(d))
        if (!regexec(&reg, entry->d_name, 0, NULL, 0))
            puts(entry->d_name);
    closedir(d);
    regfree(&reg);
    return WALK_OK;
}

int main()
{
    walker(".", ".\\.c$");
    return 0;
}
```


## C#

```c#
using System;
using System.IO;

namespace DirectoryWalk
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] filePaths = Directory.GetFiles(@"c:\MyDir", "a*");
            foreach (string filename in filePaths)
                Console.WriteLine(filename);
        }
    }
}

```



## C++

```cpp
#include "boost/filesystem.hpp"
#include "boost/regex.hpp"
#include <iostream>

using namespace boost::filesystem;

int main()
{
  path current_dir(".");
  // list all files starting with a
  boost::regex pattern("a.*");
  for (directory_iterator iter(current_dir), end;
       iter != end;
       ++iter)
  {
    boost::smatch match;
    std::string fn = iter->path().filename().string(); // must make local variable
    if (boost::regex_match( fn, match, pattern))
    {
      std::cout << match[0] << "\n";
    }
  }
}
```


```cpp

#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;

int main() {
  fs::path current_dir(".");
  // list all files containing an mp3 extension
  for (auto &file : fs::directory_iterator(current_dir)) {
    if (file.path().extension() == ".mp3")
      std::cout << file.path().filename().string() << std::endl;
  }
}
```



## Clojure

Using Java 8's [https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String- PathMatcher] patterns.


```clojure
(import java.nio.file.FileSystems)

(defn match-files [f pattern]
  (.matches (.getPathMatcher (FileSystems/getDefault) (str "glob:*" pattern)) (.toPath f)))

(defn walk-directory [dir pattern]
  (let [directory (clojure.java.io/file dir)]
    (map #(.getPath %) (filter #(match-files % pattern) (.listFiles directory)))))

```



## ColdFusion

This example display all files and directories directly under '''C:\temp''' that end with ''.html''

```cfm
<cfdirectory action="list" directory="C:\temp" filter="*.html" name="dirListing">
<cfoutput query="dirListing">
  #dirListing.name# (#dirListing.type#)

</cfoutput>
```



## Common Lisp


```lisp
(defun walk-directory (directory pattern)
  (directory (merge-pathnames pattern directory)))
```

Uses the filename pattern syntax provided by the CL implementation.


## D


```d
void main() {
    import std.stdio, std.file;

    dirEntries(".", "*.*", SpanMode.shallow).writeln;
}
```



## DCL


```txt
* matches any number of characters
& matches exactly any one character
```

<lang>$ loop:
$  f = f$search( p1 )
$  if f .eqs. "" then $ exit
$  write sys$output f
$  goto loop
```

```txt
$ @walk_a_directory *.*
USERS:[DAVID]A.A;1
USERS:[DAVID]B.B;1
USERS:[DAVID]GG.GG;1
USERS:[DAVID]WALK_A_DIRECTORY.COM;1
$ @walk_a_directory *.%
USERS:[DAVID]A.A;1
USERS:[DAVID]B.B;1
$ @walk_a_directory *a*.*
USERS:[DAVID]A.A;1
USERS:[DAVID]WALK_A_DIRECTORY.COM;1
$
```



## E


```e
def walkDirectory(directory, pattern) {
  for name => file ? (name =~ rx`.*$pattern.*`) in directory {
    println(name)
  }
}
```

Example:

```e
? walkDirectory(<file:~>, "bash_")
.bash_history
.bash_profile
.bash_profile~
```


## Elena

ELENA 4.0:

```elena
import system'io;
import system'routines;
import extensions'routines;

public program()
{
    var dir := Directory.assign("c:\MyDir");

    dir.getFiles("a.*").forEach:printingLn;
}
```



## Elixir


```elixir
# current directory
IO.inspect File.ls!

dir = "/users/public"
IO.inspect File.ls!(dir)
```


```txt

["check.exs", "e.bat", "foo", "input.txt", "test.beam", "test.exs", "test.txt"]
["Desktop", "desktop.ini", "Documents", "Downloads", "Favorites", "Libraries",
 "Music", "Pictures", "Recorded TV", "Videos"]

```



## Emacs Lisp

<code>directory-files</code> gives filenames in a given
directory, optionally restricted to those matching a regexp.


```lisp
(directory-files "/some/dir/name"
                 nil        ;; just the filenames, not full paths
                 "\\.c\\'"  ;; regexp
                 t)         ;; don't sort the filenames
=>
("foo.c" "bar.c" ...)
```



## Erlang

Use builtin function filelib:fold_files/5

```txt

8> filelib:fold_files( "/tmp", ".*", false, fun(File, Acc) -> [File|Acc] end, []).
["/tmp/.X0-lock","/tmp/.cron-check-4000-was-here",
 "/tmp/kerneloops.XyN0SP","/tmp/npicagwD7tf"]
9> filelib:fold_files( "/tmp", "k.*P", false, fun(File, Acc) -> [File|Acc] end, []).
["/tmp/kerneloops.XyN0SP"]

```



## Euphoria


```euphoria
include file.e

procedure show(sequence pattern)
    sequence f
    f = dir(pattern)
    for i = 1 to length(f) do
        puts(1,f[i][D_NAME])
        puts(1,'\n')
    end for
end procedure

show("*.*")
```


=={{header|F_Sharp|F#}}==

```fsharp
System.IO.Directory.GetFiles("c:\\temp", "*.xml")
|> Array.iter (printfn "%s")
```



## Factor

Using unix globs. Also see the "directory." in basis/tools/files.factor.

```factor
USING: globs io io.directories kernel regexp sequences ;
IN: walk-directory-non-recursively

: print-files ( path pattern -- )
    [ directory-files ] [ <glob> ] bi* [ matches? ] curry filter
    [ print ] each ;
```

Ex:
    ( scratchpad ) "." "*.txt" print-files
    license.txt


## Forth

Gforth's directory walking functions are tied to the POSIX ''dirent'' functions, used by the C langauge entry above. Forth doesn't have regex support, so a simple filter function is used instead.

```forth
defer ls-filter ( name len -- ? )
: ls-all  2drop true ;
: ls-visible  drop c@ [char] . <> ;

: ls ( dir len -- )
  open-dir throw  ( dirid )
  begin
    dup pad 256 rot read-dir throw
  while
    pad over ls-filter if
      cr pad swap type
    else drop then
  repeat
  drop close-dir throw ;

\ only show C language source and header files (*.c *.h)
: c-file? ( str len -- ? )
  dup 3 < if 2drop false exit then
  + 1- dup c@
   dup [char] c <> swap [char] h <> and if drop false exit then
  1- dup c@ [char] . <> if drop false exit then
  drop true ;
' c-file? is ls-filter

s" ." ls
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=c5fde952fecd1d7052101b1e2287f2ff Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sTemp As String

For Each sTemp In Dir("/etc", "*.d")
  Print sTemp
Next

End
```

Output:

```txt

profile.d
rc1.d
rc4.d
rcS.d
binfmt.d
init.d
rc5.d
rc2.d

```



## Go


```go
package main

import (
    "fmt"
    "path/filepath"
)

func main() {
    fmt.Println(filepath.Glob("*.go"))
}
```



## Groovy


```groovy
// *** print *.txt files in current directory
 new File('.').eachFileMatch(~/.*\.txt/) {
   println it
 }

 // *** print *.txt files in /foo/bar
 new File('/foo/bar').eachFileMatch(~/.*\.txt/) {
   println it
 }
```



## Haskell

In this example, the pattern is a POSIX extended regular expression.

```haskell
import System.Directory
import Text.Regex
import Data.Maybe

walk :: FilePath -> String -> IO ()
walk dir pattern = do
    filenames <- getDirectoryContents dir
    mapM_ putStrLn $ filter (isJust.(matchRegex $ mkRegex pattern)) filenames

main = walk "." ".\\.hs$"
```



## HicEst

More on [http://www.HicEst.com/SYSTEM.htm SYSTEM], [http://www.HicEst.com/OPEN.htm OPEN], [http://www.HicEst.com/indexfnc.htm INDEX]

```hicest
CHARACTER dirtxt='dir.txt', filename*80

SYSTEM(DIR='*.*', FIle=dirtxt) ! "file names", length, attrib, Created, LastWrite, LastAccess
OPEN(FIle=dirtxt, Format='"",', LENgth=files) ! parses column 1 ("file names")

DO nr = 1, files
  filename = dirtxt(nr,1) ! reads dirtxt row = nr, column = 1 to filename
  ! write file names with extensions "txt", or "hic", or "jpg" (case insensitive) using RegEx option =128:
  IF( INDEX(filename, "\.txt|\.hic|\.jpg", 128) ) WRITE() filename
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==
This uses Unicon extensions for ''stat'' and to read directories.  Icon can uses ''system'' to accomplish the same objective.

```Icon
procedure main()
every write(getdirs(".","icn"))  # writes out all directories from the current directory down
end

procedure getdirs(s,pat)  #: return a list of directories beneath the directory 's'
local d,f

if ( stat(s).mode ? ="d" ) & ( d := open(s) ) then {
      while f := read(d) do
         if find(pat,f) then
            suspend f
      close(d)
      }
end
```



## IDL


```idl
f = file_search('*.txt', count=cc)
if cc gt 0 then print,f
```

(IDL is an array language - very few things are ever done in 'loops'.)


## J


```j
require 'dir'
0 dir '*.png'
0 dir '/mydir/*.txt'
```

The verb <tt>dir</tt> supports a number of reporting options determined by its left argument. A left argument of <tt>0</tt> reports just the file names.


## Java


```java
File dir = new File("/foo/bar");

String[] contents = dir.list();
for (String file : contents)
    if (file.endsWith(".mp3"))
        System.out.println(file);
```



## JavaScript

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");
var dir = fso.GetFolder('test_folder');

function walkDirectory(dir, re_pattern) {
    WScript.Echo("Files in " + dir.name + " matching '" + re_pattern +"':");
    walkDirectoryFilter(dir.Files, re_pattern);

    WScript.Echo("Folders in " + dir.name + " matching '" + re_pattern +"':");
    walkDirectoryFilter(dir.Subfolders, re_pattern);
}

function walkDirectoryFilter(items, re_pattern) {
    var e = new Enumerator(items);
    while (! e.atEnd()) {
        var item = e.item();
        if (item.name.match(re_pattern))
            WScript.Echo(item.name);
        e.moveNext();
    }
}

walkDirectory(dir, '\\.txt$');
```



## Julia

```julia
for filename in readdir("/foo/bar")
    if endswith(filename, ".mp3")
        print(filename)
    end
end
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun walkDirectory(dirPath: String, pattern: Regex): List<String> {
    val d = File(dirPath)
    require(d.exists() && d.isDirectory())
    return d.list().filter { it.matches(pattern) }
}

fun main(args: Array<String>) {
    val r = Regex("""^a.*\.h$""")  // get all C header files beginning with 'a'
    val files = walkDirectory("/usr/include", r)
    for (file in files) println(file)
}
```

Sample output (Ubuntu v14.04):
```txt

argp.h
alloca.h
ar.h
aliases.h
autosprintf.h
aio.h
assert.h
argz.h

```



## Lasso


```Lasso
local(matchingfilenames = array)

dir('.') -> foreach => {#1 >> 'string' ? #matchingfilenames -> insert(#1)}

#matchingfilenames
```

-> array(mystrings.html, a_string_file.txt)


## Lingo


```lingo
-- Usage: printFiles("C:\scripts", ".ls")
on printFiles (dir, fileType)
  i = 1
  sub = fileType.length -1
  repeat while TRUE
    fn = getNthFileNameInFolder(dir, i)
    if fn = EMPTY then exit repeat
    i = i+1
    if fn.length<fileType.length then next repeat
    if fn.char[fn.length-sub..fn.length]=fileType then put fn
  end repeat
end
```



## LiveCode


```LiveCode
set the defaultFolder to the documents folder  -- the documents folder is a "specialFolderPath"
put the files into docfiles
filter docfiles with "*.txt"
put docfiles
```



## Lua

Lua itself is extremely spartanic as it is meant for embedding. Reading out a directory is not something that a minimal standard C library can do, and so minimal Lua without native extension libraries can't do it either. But lfs (LuaFileSystem) is about as standard an extension as it gets, so we use that.

```Lua
require "lfs"
directorypath = "." -- current working directory
for filename in lfs.dir(directorypath) do
    if filename:match("%.lua$") then -- "%." is an escaped ".", "$" is end of string
        print(filename)
    end
end
```

Although Lua is spartanic, it still provides functions such as os.execute([command]) and io.popen(prog [, mode]). Below an example for Windows users having io.popen at their disposal. Mind you, it may pop-up a command window.

```Lua
-- Gets the output of given program as string
-- Note that io.popen is not available on all platforms
local function getOutput(prog)
    local file = assert(io.popen(prog, "r"))
    local output = assert(file:read("*a"))
    file:close()
    return output
end

-- Iterates files in given directory
local function files(directory, recursively)
    -- Use windows" dir command
    local directory = directory:gsub("/", "\\")
    local filenames = getOutput(string.format("dir %s %s/B/A:A", directory, recursively and '/S' or ''))

    -- Function to be called in "for filename in files(directory)"
    return coroutine.wrap(function()
        for filename in filenames:gmatch("([^\r\n]+)") do
            coroutine.yield(filename)
        end
    end)
end

-- Walk "C:/Windows" looking for executables
local directory = "C:/Windows"
local pattern = ".*%.exe$" -- for finding executables
for filename in files(directory) do
    if filename:match(pattern) then
        print(filename)
    end
end
```



## M2000 Interpreter

Console has a popup list called Menu, which we can fill using Files statements. Files statement get some symbols before first argument for sorting and to not export to console but to menu list. So we can use MenuItems to find how many items return, and we can walk menu array to get the names (from 1 to MenuItems).

Files statement get as first argument a pattern or a list of file extensions "txt|bmp" return these two kind of files.
There is a second optional parameter which examine all files founded from first filter for included letters. We can add using | as seperator, a list of strings included in same line. Files examine all files, opened one by one, using an automatic way to find what kind of text file is, an Ansi, a Utf8, a Utf-16LE, or a Utf-16BE. Also automatic find the line breaks. All files converted at open as utf-16LE and then searched. For Ansi files, Locale used to make the right conversion.


```M2000 Interpreter

Module Show_Files_Standard {
      \\ we get more (include hidden too)
      Module InnerWay (folder_path$, pattern$){
            olddir$=dir$
            dir folder_path$
            \\ clear menu list
            Menu
            \\ + place export to menu, without showing
            \\ ! sort to name
            files ! + pattern$
            If MenuItems>0 then {
                  For i=1 to MenuItems {
                        Print Menu$(i)+".exe"
                  }
            }
            dir olddir$
      }
      InnerWay "C:\Windows","*.exe"
}
Show_Files_Standard

```


Like VbScript using external help, from a COM object.

We use an enumerator to iterate all file names and checked using like operator "~",and then we push them to end of stack (Data push to end), so we get first the first entered (we use stack here as a FIFO, using a New stack for temporary use), and we remove at the end all items and place them in an array. This array return from get_file$() and we make a second iterator for array, to get each end display it. The second iterator is not a com enumerator, but another type of object included in this interpreter. This iterator can get start and end position, defining a range and a direction too.

EnumFile is an object in an object. In expression we get the inner object. In While {} we get the outer object, and iterate or not (depends of state), so the inner object change. Because we get the first object at creation time, the first time when While structure found this object skips iteration.

Stack New {} make a block of a fresh stack of values, and at the exit attach the old stack (which for this block detached from execute object at the begin of block).


```M2000 Interpreter

Module Show_Files {
      Function  get_files$ (folder_path$) {
            \\ we get second argument using letter$ which pop from stack
            pattern$=lcase$(Letter$)
            Declare  objfso "Scripting.FileSystemObject"
            Method objfso, "GetFolder", folder_path$ as fc
            With fc, "files" set files
            \\ from revision 13 - version 9.4
            With files, -4& as EnumFile
            With EnumFile, "Name" as name$
            Dim empty$()
            =empty$()
            Stack New {
                  While EnumFile {
                        If lcase$(name$) ~ pattern$ Then Data name$
                  }
                  \\ get stack values and fill an array
                  =Array$([])
            }
      }
      Dim Name$()
      Name$()=get_files$("C:\Windows","*.exe")
      m=each(Name$())
      While m {
            Print Array$(m)
      }
}
Show_Files

```



## Mathematica

The built-in function <code>FileNames</code> does exactly this:
:<code>FileNames[]</code> lists all files in the current working directory.
:<code>FileNames[form]</code> lists all files in the current working directory whose names match the string pattern form.
:<code>FileNames[{form1,form2,...}]</code> lists all files whose names match any of the form_i.
:<code>FileNames[forms,{dir1,dir2,...}]</code> lists files with names matching forms in any of the directories dir_i.
:<code>FileNames[forms,dirs,n]</code> includes files that are in subdirectories up to n levels down.
Examples (find all files in current directory, find all png files in root directory):

```Mathematica
FileNames["*"]
FileNames["*.png", $RootDirectory]
```

the result can be printed with Print /@ FileNames[....].


## MAXScript


```maxscript
getFiles "C:\\*.txt"
```



## Nemerle


```Nemerle
using System.Console;
using System.IO;

module DirWalk
{
    Main() : void
    {
        def files = Directory.GetFiles(@"C:\MyDir");                  // retrieves only files
        def files_subs = Directory.GetFileSystemEntries(@"C:\MyDir"); // also retrieves (but does not enter) sub-directories
                                                                      // (like ls command)
        foreach (file in files) WriteLine(file);
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.List

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getFileNames(dirname, pattern) public static returns List
  dir = File(dirname)
  contents = dir.list()
  fileNames = ArrayList()
  loop fname over contents
    if fname.matches(pattern) then do
      fileNames.add(fname)
      end
    end fname
  Collections.sort(fileNames)
  return fileNames

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg dirname pattern
  if dirname = '' then dirname = System.getProperty('user.dir')
  if pattern = '' then pattern = '^RW.*\\.nrx$'

  fileNames = getFileNames(dirname, pattern)
  say 'Search of' dirname 'for files matching pattern "'pattern'" found' fileNames.size() 'files.'
  loop fn = 0 while fn < fileNames.size()
    say (fn + 1).right(5)':' fileNames.get(fn)
    end fn

  return

```

```txt

Search of /Users/projects/RosettaCode/netrexx for files matching pattern "^RW.*\.nrx$" found 5 files.
    1: RWalkDir_Iter.nrx
    2: RWebScraping.nrx
    3: RWindowCreate.nrx
    4: RWriteFloatArray.nrx
    5: RWriteName3D01.nrx

```



## Nim


```nim
import os

for file in walkFiles "/foo/bar/*.mp3":
  echo file
```



## Objeck


```objeck
use IO;

bundle Default {
  class Test {
    function : Main(args : System.String[]) ~ Nil {
       dir := Directory->List("/src/code");
       for(i := 0; i < dir->Size(); i += 1;) {
         if(dir[i]->EndsWith(".obs")) {
           dir[i]->PrintLine();
        };
      };
    }
  }
}
```


=={{header|Objective-C}}==

```objc
NSString *dir = @"/foo/bar";

// Pre-OS X 10.5
NSArray *contents = [[NSFileManager defaultManager] directoryContentsAtPath:dir];
// OS X 10.5+
NSArray *contents = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:dir error:NULL];

for (NSString *file in contents)
  if ([[file pathExtension] isEqualToString:@"mp3"])
    NSLog(@"%@", file);
```



## OCaml


```ocaml
#load "str.cma"
let contents = Array.to_list (Sys.readdir ".") in
let select pat str = Str.string_match (Str.regexp pat) str 0 in
List.filter (select ".*\\.jpg") contents
```



## Oz


```oz
declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  Files = {Filter {Path.readdir "."} Path.isFile}
  Pattern = ".*\\.oz$"
  MatchingFiles = {Filter Files fun {$ File} {Regex.search Pattern File} \= false end}
in
  {ForAll MatchingFiles System.showInfo}
```



## Pascal

```pascal
{$H+}

program Walk;

uses SysUtils;

var Res: TSearchRec;
    Pattern, Path, Name: String;
    FileAttr: LongInt;
    Attr: Integer;

begin
   Write('File pattern: ');
   ReadLn(Pattern);            { For example .\*.pas }

   Attr := faAnyFile;
   if FindFirst(Pattern, Attr, Res) = 0 then
   begin
      Path := ExtractFileDir(Pattern);
      repeat
         Name := ConcatPaths([Path, Res.Name]);
         FileAttr := FileGetAttr(Name);
         if FileAttr and faDirectory = 0 then
         begin
            { Do something with file name }
            WriteLn(Name);
         end
      until FindNext(Res) <> 0;
   end;
   FindClose(Res);
end.
```



## Perl


```perl
use 5.010;
opendir my $dh, '/home/foo/bar';
say for grep { /php$/ } readdir $dh;
closedir $dh;
```

Or using globbing, with the <code>&lt;&gt;</code> operator,

```perl
use 5.010; say while </home/foo/bar/*.php>;
```

Or the same with the builtin <code>glob()</code> function,

```perl
my @filenames = glob('/home/foo/bar/*.php');
```

The <code>glob()</code> function takes any expression for its pattern, whereas <code>&lt;&gt;</code> is only for a literal.

```perl
my $pattern = '*.c';
my @filenames = glob($pattern);
```



## Perl 6

The <code>dir</code> function takes the directory to traverse, and optionally a named parameter <code>test</code>, which is [https://docs.perl6.org/routine/$TILDE$TILDE smart-matched] against the basename of each file (so for example we can use a regex):

```perl6
.say for dir ".", :test(/foo/);
```



## Phix

The dir function accepts a DOS pattern, with some minor variations (eg "*" gets all files with no extension).

```Phix
puts(1,join(columnize(dir("*.txt"))[D_NAME],"\n"))
```

```txt

copyright.txt
e-1millon.txt
ildump.txt
output.txt
readme.txt
_TODO.TXT

```



## PHP

```php
$pattern = 'php';
$dh = opendir('c:/foo/bar'); // Or '/home/foo/bar' for Linux
while (false !== ($file = readdir($dh)))
{
    if ($file != '.' and $file != '..')
    {
        if (preg_match("/$pattern/", $file))
        {
            echo "$file matches $pattern\n";
        }
    }
}
closedir($dh);
```

Or:

```php
$pattern = 'php';
foreach (scandir('/home/foo/bar') as $file)
{
    if ($file != '.' and $file != '..')
    {
        if (preg_match("/$pattern/", $file))
        {
            echo "$file matches $pattern\n";
        }
    }
}
```

```php
foreach (glob('/home/foo/bar/*.php') as $file){
    echo "$file\n";
}
```



## PicoLisp


```PicoLisp
(for F (dir "@src/")                         # Iterate directory
   (when (match '`(chop "s@.c") (chop F))    # Matches 's*.c'?
      (println F) ) )                        # Yes: Print it
```

```txt
"start.c"
"ssl.c"
"subr.c"
"sym.c"
...
```



## Pike


```pike
array(string) files = get_dir("/home/foo/bar");
foreach(files, string file)
    write(file + "\n");
```



## Pop11

Built-in procedure sys_file_match searches directories (or directory trees) using shell-like patterns:

```pop11
lvars repp, fil;
;;; create path repeater
sys_file_match('*.p', '', false, 0) -> repp;
;;; iterate over files
while (repp() ->> fil) /= termin do
     ;;; print the file
     printf(fil, '%s\n');
endwhile;
```



## PowerShell

Since PowerShell is also a shell it should come as no surprise that this task is very simple. Listing the names of all text files, or the names of all files, starting with "f":

```powershell
Get-ChildItem *.txt -Name
Get-ChildItem f* -Name
```

The <code>-Name</code> parameter tells the <code>Get-ChildItem</code> to return only the file names as string, otherwise a complete <code>FileInfo</code> or <code>DirectoryInfo</code> object would be returned, containing much more information than only the file name.

More complex matching can be accomplished by filtering the complete list of files using the <code>Where-Object</code> cmdlet. The following will output all file names that contain at least one vowel:

```powershell
Get-ChildItem -Name | Where-Object { $_ -match '[aeiou]' }
```



## PureBasic

The match is made using DOS wildcards.  It could easily be modified to match based on a regular expression if desired (i.e. using the PCRE library).

```PureBasic
Procedure walkDirectory(directory.s = "", pattern.s = "")
  Protected directoryID

  directoryID = ExamineDirectory(#PB_Any,directory,pattern)
  If directoryID
    While NextDirectoryEntry(directoryID)
      PrintN(DirectoryEntryName(directoryID))
    Wend
    FinishDirectory(directoryID)
  EndIf
EndProcedure

If OpenConsole()
  walkDirectory()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python

The [http://python.org/doc/lib/module-glob.html glob] library included with Python lists files matching shell-like patterns:

```python
import glob
for filename in glob.glob('/foo/bar/*.mp3'):
    print(filename)
```

Or manually:

```python
import os
for filename in os.listdir('/foo/bar'):
    if filename.endswith('.mp3'):
        print(filename)
```



## R


```R
dir("/foo/bar", "mp3")
```



## Racket



```Racket

-> (for ([f (directory-list "/tmp")] #:when (regexp-match? "\\.rkt$" f))
     (displayln f))
... *.rkt files ...

```



## Rascal


```rascal
import IO;
public void Walk(loc a, str pattern){
	for (entry <- listEntries(a))
		endsWith(entry, pattern) ? println(entry);
}
```



## Raven


```raven
'dir://.' open each as item
    item m/\.txt$/ if "%(item)s\n" print
```



## REXX

The following program was tested in a DOS window under Windows/XP and should work for all Microsoft Windows.

```rexx
/*REXX program shows files in directory tree that match a given criteria*/
parse arg xdir;  if xdir='' then xdir='\'        /*Any DIR? Use default.*/
@.=0                                   /*default in case ADDRESS fails. */
trace off                              /*suppress REXX err msg for fails*/
address system 'DIR' xdir '/b /s' with output stem @.   /*issue DIR cmd.*/
if rc\==0  then do                                  /*an error happened?*/
                say '***error!*** from DIR' xDIR    /*indicate que pasa.*/
                say 'return code='  rc              /*show the Ret Code.*/
                exit rc                             /*exit with the  RC.*/
                end                                 /* [↑]  bad address.*/
#=@.rc                                              /*number of entries.*/
if #==0  then #='   no   '                          /*use a word, ¬zero.*/
say center('directory ' xdir " has "    #     ' matching entries.',79,'─')

       do j=1  for #;  say @.j;  end   /*show files that met criteria.  */

exit @.0+rc                            /*stick a fork in it, we're done.*/
```




## Ring


```ring


###---------------------------------------
### Directory Tree Walk
### Look for FileType for Music and Video

fileType = [".avi", ".mp4", ".mpg", ".mkv", ".mp3", ".wmv" ]

dirList   = []
musicList = []

###---------------------------------------
### Main

    ###-----------------------------------
    ### Start at this directory

    searchVideoMusic("C:\Users\Umberto\")

    see nl +"Number of Music and Videos files: " +len(musicList) +nl +nl
    see musicList
    See nl +"Finished" +nl

###
### =================================

### Search for Video and Music files

Func searchVideoMusic(startDir)

    ChDir(startDir + "Music")     ### <<<== add Music subpath C:\Users\Umberto\Music
    listDir( CurrentDir() )

    ChDir(startDir + "Videos")    ### <<<== add Videos subpath C:\Users\Umberto\Videos
    listDir( CurrentDir() )

    for searchDir in dirList      ### Search Directory List for Music and Videos files
        listDir(searchDir)
    next


###
### ========================

### Find Files in Directory

Func listDir(dirName)

    ChDir(dirName)
    Try
        ###-------------------------------------
        ### Get SubDirectories

        myListSub = Dir( CurrentDir() )
    Catch
        ###-------------------------------------
        ### Error, Couldn't open the directory

        See "ListDir Catch! " + CurrentDir() +" --- "+ cCatchError +nl
        return
    Done

    for x in myListSub
        if x[2]
            thisDir = x[1]

            if thisDir[1] = "."
                ### Do Nothing. Ignore dot.name

            else
                see nl +"Dir: " + CurrentDir() +"\"+ thisDir + nl

                ###----------------------------------------
                ###  Directory Walk add to directory list

                Add( dirList, (CurrentDir() +"\"+  thisDir))
            ok
        else
            thisFile = x[1]

            ###-------------------------------
            ### Add Music or Video file type

            for thisType in fileType
                if ( substr(thisFile, thisType) )             ### <<<== Type of File from List
                     see "         File: " + thisFile + nl
                     Add(musicList, (CurrentDir() +"\"+  thisFile))
                ok
            next
        ok
    next
return

###
### =========================================



```


OUTPUT:

```txt


Dir: C:\Users\Umberto\Music\Free YouTube Downloader
         File: stock.mp3
         File: big_buck_bunny.mp4
         File: BowieNikolaTesla'ThePrestige'.mpg
         File: BowieTesla'The Prestige'.wmv
         File: Candyman.mp4

Dir: C:\Users\Umberto\Videos\Captures
         File: drop.avi

Dir: C:\Users\Umberto\Videos\Free YouTube Downloader
         File: GaryUSBondsQuarterToThree.avi

Dir: C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]
         File: Joe.Versus.The.Volcano[1990].avi
         File: SampleTheMythSanWa2005.mkv
         File: stock.mp3

Dir: C:\Users\Umberto\Videos\The Prestige (2006)
         File: BowieNikola'The Prestige'.mp4
         File: BowieTeslaThe PrestigeConverted.mpg
         File: 027_3xplanet_MDYD-895.avi
         File: 3.mpg
         File: HitomiTanaka[MIDE-219].mp4
         File: MDYD-868.wmv
         File: MIDE-253.mp4
         File: MIDE_280.mp4
         File: PPPD-432.avi
         File: The.Prestige.2006.mkv

Number of Music and Videos files: 20

C:\Users\Umberto\Music\stock.mp3
C:\Users\Umberto\Videos\big_buck_bunny.mp4
C:\Users\Umberto\Videos\BowieNikolaTesla'ThePrestige'.mpg
C:\Users\Umberto\Videos\BowieTesla'The Prestige'.wmv
C:\Users\Umberto\Videos\Candyman.mp4
C:\Users\Umberto\Videos\drop.avi
C:\Users\Umberto\Videos\GaryUSBondsQuarterToThree.avi
C:\Users\Umberto\Videos\Joe.Versus.The.Volcano[1990].avi
C:\Users\Umberto\Videos\SampleTheMythSanWa2005.mkv
C:\Users\Umberto\Videos\stock.mp3
C:\Users\Umberto\Videos\Free YouTube Downloader\BowieNikola'The Prestige'.mp4
C:\Users\Umberto\Videos\Free YouTube Downloader\BowieTeslaThe PrestigeConverted.mpg
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\027_3xplanet_MDYD-895.avi
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\3.mpg
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\HitomiTanaka[MIDE-219].mp4
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\MDYD-868.wmv
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\MIDE-253.mp4
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\MIDE_280.mp4
C:\Users\Umberto\Videos\HitomiTanaka[MIDE-219]\PPPD-432.avi
C:\Users\Umberto\Videos\The Prestige (2006)\The.Prestige.2006.mkv

Finished


```



## Ruby


```ruby
# Files under this directory:
Dir.glob('*') { |file| puts file }

# Files under path '/foo/bar':
Dir.glob( File.join('/foo/bar', '*') ) { |file| puts file }

# As a method
def file_match(pattern=/\.txt/, path='.')
  Dir[File.join(path,'*')].each do |file|
    puts file if file =~ pattern
  end
end
```


## Run BASIC


```runbasic
files #g, DefaultDir$ + "\*.jpg"   ' find all jpg files

if #g HASANSWER() then
	count = #g rowcount()	' get count of files
	for i = 1 to count
	if #g hasanswer() then	'retrieve info for next file
		#g nextfile$()	'print name of file
		print #g NAME$()
	end if
	next
end if
wait
```

FILE ACCESSOR methods

#handle HASANSWER() - Return non-zero if the file accessor has at least one resulting row.
#handle ROWCOUNT() - Return the number of rows returned.
#handle NEXTFILE$() - Advance to the next row and return a comma delimited string for the next file (name, size, date, time, directory flag).
#handle NEXTFILE$([delimExpr$]) - Like NEXTFILE$() but you get to specify the delimiter instead of a comma.
#handle NAME$() - Return the name of the current file row.
#handle SIZE() - Return the size of the current file row.
#handle DATE$() - Return a string containing a formatted date for the current file row.
#handle TIME$() - Return a string containing a formatted time for the current file row.
#handle ISDIR() - Return non-zero if the current file row represents a directory instead of a file.
#handle RESET() - Reset the file accessor back to the beginning so you can read through them again.
#handle DATEFORMAT(template$) - Set the date format using a "mmm dd, yyyy" style template$.
#handle TIMEFORMAT(template$) - Set the time format using a "hh:mm:ss" style template$.
#handle ISNULL() - Returns zero (or false)
#handle DEBUG$() - Returns the string "Files"

OUTPUT:

```txt

button.JPG
circuitbanner1.JPG
circuitbanner2.JPG
copy.jpg
homecomputerbanner1.JPG
mandelbrot.jpg
```



## Rust


```rust
extern crate docopt;
extern crate regex;
extern crate rustc_serialize;

use docopt::Docopt;
use regex::Regex;

const USAGE: &'static str = "
Usage: rosetta <pattern>

Walks the directory tree starting with the current working directory and
print filenames matching <pattern>.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_pattern: String,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let re = Regex::new(&args.arg_pattern).unwrap();
    let paths = std::fs::read_dir(".").unwrap();

    for path in paths {
        let path = path.unwrap().path();
        let path = path.to_str().unwrap();

        if re.is_match(path) {
            println!("{}", path);
        }
    }
}
```



## Scala


```Scala
import java.io.File

val dir = new File("/foo/bar").list()
dir.filter(file => file.endsWith(".mp3")).foreach(println)
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  local
    var string: fileName is "";
  begin
    for fileName range readDir(".") do
      if endsWith(fileName, ".sd7") then
        writeln(fileName);
      end if;
    end for;
  end func;
```



## Sidef


```ruby
'*.p[lm]'.glob.each { |file| say file }    # Perl files under this directory
```

```txt

x.pl
x.pm

```



```ruby
func file_match(Block callback, pattern=/\.txt\z/, path=Dir.cwd) {
    path.open(\var dir_h) || return nil
    dir_h.entries.each { |entry|
        if (entry.basename ~~ pattern) {
            callback(entry)
        }
    }
}
 
file_match(
    path: %d'/tmp',
    pattern: /\.p[lm]\z/i,
    callback: { |file|
        say file;
    }
)
```

```txt

/tmp/x.pl
/tmp/x.pm

```



## Smalltalk


```smalltalk
(Directory name: 'a_directory')
  allFilesMatching: '*.st' do: [ :f | (f name) displayNl ]
```



## Tcl

For the current directory:

```tcl
foreach filename [glob *.txt] {
    puts $filename
}
```

For an arbitrary directory:

```tcl
set dir /foo/bar
foreach filename [glob -directory $dir *.txt] {
    puts $filename
    ### Or, if you only want the local filename part...
    # puts [file tail $filename]
}
```



## Toka

As with the C example, this uses a a POSIX extended regular expression as the pattern. The '''dir.listByPattern''' function used here was introduced in library revision 1.3.

```toka
needs shell
" ."  " .\\.txt$" dir.listByPattern
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
files=FILE_NAMES (+,-std-)
fileswtxt= FILTER_INDEX (files,":*.txt:",-)
txtfiles= SELECT (files,#fileswtxt)
```

```txt

files     DEST'MAKROS'ROSETTA.TXT'SKRIPTE'STUDENTS.XML'TUSTEP.INI
fileswtxt 3
txtfiles  ROSETTA.TXT

```



## TXR



### = Using <code>glob</code> =



```txrlisp
(glob "/etc/*.conf")
```


```txrlisp
("/etc/adduser.conf" "/etc/apg.conf" "/etc/blkid.conf" "/etc/brltty.conf"
 "/etc/ca-certificates.conf" "/etc/colord.conf" "/etc/ddclient.conf"
 "/etc/debconf.conf" "/etc/deluser.conf" "/etc/dnsmasq.conf" "/etc/ffserver.conf"
 "/etc/fuse.conf" "/etc/gai.conf" "/etc/hdparm.conf" "/etc/host.conf"
 "/etc/insserv.conf" "/etc/irssi.conf" "/etc/kernel-img.conf"
 "/etc/kerneloops.conf" "/etc/knockd.conf" "/etc/ld.so.conf" "/etc/lftp.conf"
 "/etc/logrotate.conf" "/etc/ltrace.conf" "/etc/mke2fs.conf" "/etc/mtools.conf"
 "/etc/netscsid.conf" "/etc/nsswitch.conf" "/etc/ntp.conf" "/etc/pam.conf"
 "/etc/pnm2ppa.conf" "/etc/popularity-contest.conf" "/etc/resolv.conf"
 "/etc/rsyslog.conf" "/etc/sensors3.conf" "/etc/sysctl.conf" "/etc/ucf.conf"
 "/etc/updatedb.conf" "/etc/usb_modeswitch.conf" "/etc/wodim.conf")
```


==== Using <code>open-directory</code> and <code>get-lines</code> ====


```txrlisp
(mappend [iff (op ends-with ".conf") list] (get-lines (open-directory "/etc")))
```


```txrlisp
("ddclient.conf" "gai.conf" "ucf.conf" "kernel-img.conf" "ltrace.conf"
 "debconf.conf" "apg.conf" "adduser.conf" "mke2fs.conf" "colord.conf"
 "kerneloops.conf" "fuse.conf" "hdparm.conf" "irssi.conf" "host.conf"
 "ffserver.conf" "pam.conf" "sysctl.conf" "ld.so.conf" "dnsmasq.conf"
 "insserv.conf" "brltty.conf" "deluser.conf" "netscsid.conf" "nsswitch.conf"
 "mtools.conf" "wodim.conf" "updatedb.conf" "popularity-contest.conf"
 "knockd.conf" "ntp.conf" "sensors3.conf" "resolv.conf" "blkid.conf"
 "lftp.conf" "ca-certificates.conf" "usb_modeswitch.conf" "logrotate.conf"
 "rsyslog.conf" "pnm2ppa.conf")
```



## UNIX Shell


```bash
ls -d *.c                # *.c files in current directory
(cd mydir && ls -d *.c)  # *.c files in mydir
```

<code>*.c</code> is a ''file name pattern'', also known as a ''glob pattern''. The shell expands each pattern to a sorted list of matching files. Details are in your shell's manual.

If there are no *.c files, <code>ls</code> fails with an error message.


## UnixPipes

Here using grep for regexp.

```bash
ls | grep '\.c$'
```



## VBScript


```vb

Sub show_files(folder_path,pattern)
	Set objfso = CreateObject("Scripting.FileSystemObject")
	For Each file In objfso.GetFolder(folder_path).Files
		If InStr(file.Name,pattern) Then
			WScript.StdOut.WriteLine file.Name
		End If
	Next
End Sub

Call show_files("C:\Windows",".exe")

```



## Visual Basic .NET

```vbnet
'Using the OS pattern matching
For Each file In IO.Directory.GetFiles("\temp", "*.txt")
  Console.WriteLine(file)
Next

'Using VB's pattern matching and LINQ
For Each file In (From name In IO.Directory.GetFiles("\temp") Where name Like "*.txt")
  Console.WriteLine(file)
Next

'Using VB's pattern matching and dot-notation
For Each file In IO.Directory.GetFiles("\temp").Where(Function(f) f Like "*.txt")
  Console.WriteLine(file)
Next
```



## zkl

Unix glob, with wildcarding and options on file type, case folding and a few others.

```zkl
File.glob("*.zkl") //--> list of matches
```



## Zsh

Zsh has powerful filename generation features, which can filter by file names, permissions, size, type, etc.

```bash
print -l -- *.c
```


