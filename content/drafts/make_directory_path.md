+++
title = "Make directory path"
description = ""
date = 2019-07-23T02:33:41Z
aliases = []
[extra]
id = 17833
[taxonomies]
categories = []
tags = []
+++

{{task|File System Operations}}

;Task:
Create a directory and any missing parents.

This task is named after the posix <code>[http://www.unix.com/man-page/POSIX/0/mkdir/ mkdir -p]</code> command, and several libraries which implement the same behavior.

Please implement a function of a single path string (for example <code>./path/to/dir</code>) which has the above side-effect.
If the directory already exists, return successfully.
Ideally implementations will work equally well cross-platform (on windows, linux, and OS X).

It's likely that your language implements such a function as part of its standard library. If so, please also show how such a function would be implemented.





## Aime


```aime
void
mkdirp(text path)
{
    list l;
    text p, s;

    file().b_affix(path).news(l, 0, 0, "/");

    for (, s in l) {
        p = p + s + "/";
        trap_q(mkdir, p, 00755);
    }
}

integer
main(void)
{
    mkdirp("./path/to/dir");

    0;
}
```



## AppleScript

AppleScript is not a cross-platform language so this is a macOS-only solution.
In post-Yosemite AppleScript we can draw on the macOS Foundation classes,
which include the NSFileManager method:
`createDirectoryAtPath:withIntermediateDirectories:attributes:error:`


```AppleScript
use framework "Foundation"
use scripting additions


-- createOrFindDirectoryMay :: Bool -> FilePath -> Maybe IO ()
on createOrFindDirectoryMay(fp)
    createDirectoryIfMissingMay(true, fp)
end createOrFindDirectoryMay


-- createDirectoryIfMissingMay :: Bool -> FilePath -> Maybe IO ()
on createDirectoryIfMissingMay(blnParents, fp)
    if doesPathExist(fp) then
        nothing("Directory already exists: " & fp)
    else
        set e to reference
        set ca to current application
        set oPath to (ca's NSString's stringWithString:(fp))'s ¬
            stringByStandardizingPath
        set {bool, nse} to ca's NSFileManager's ¬
            defaultManager's createDirectoryAtPath:(oPath) ¬
            withIntermediateDirectories:(blnParents) ¬
            attributes:(missing value) |error|:(e)
        if bool then
            just(fp)
        else
            nothing((localizedDescription of nse) as string)
        end if
    end if
end createDirectoryIfMissingMay

-- TEST ----------------------------------------------------------------------
on run

    createOrFindDirectoryMay("~/Desktop/Notes/today")

end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- doesPathExist :: FilePath -> IO Bool
on doesPathExist(strPath)
    set ca to current application
    ca's NSFileManager's defaultManager's ¬
        fileExistsAtPath:((ca's NSString's ¬
            stringWithString:strPath)'s ¬
            stringByStandardizingPath)
end doesPathExist

-- just :: a -> Just a
on just(x)
    {nothing:false, just:x}
end just

-- nothing :: () -> Nothing
on nothing(msg)
    {nothing:true, msg:msg}
end nothing
```



## AWK


```AWK

# syntax: GAWK -f MAKE_DIRECTORY_PATH.AWK path ...
BEGIN {
    for (i=1; i<=ARGC-1; i++) {
      path = ARGV[i]
      msg = (make_dir_path(path) == 0) ? "created" : "exists"
      printf("'%s' %s\n",path,msg)
    }
    exit(0)
}
function make_dir_path(path,  cmd) {
#   cmd = sprintf("mkdir -p '%s'",path) # Unix
    cmd = sprintf("MKDIR \"%s\" 2>NUL",path) # MS-Windows
    return system(cmd)
}

```

<p>sample command and output under Windows 8:</p>

```txt

GAWK -f MAKE_DIRECTORY_PATH.AWK \TEMP\A \TEMP\A "\TEMP\A\B C"

'\TEMP\A' created
'\TEMP\A' exists
'\TEMP\A\B C' created

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

int main (int argc, char **argv) {
    char *str, *s;
    struct stat statBuf;

    if (argc != 2) {
        fprintf (stderr, "usage: %s <path>\n", basename (argv[0]));
        exit (1);
    }
    s = argv[1];
    while ((str = strtok (s, "/")) != NULL) {
        if (str != s) {
            str[-1] = '/';
        }
        if (stat (argv[1], &statBuf) == -1) {
            mkdir (argv[1], 0);
        } else {
            if (! S_ISDIR (statBuf.st_mode)) {
                fprintf (stderr, "couldn't create directory %s\n", argv[1]);
                exit (1);
            }
        }
        s = NULL;
    }
    return 0;
}
```


=={{header|C++|CPP}}==

```cpp

#include <filesystem>
#include <iostream>

namespace fs = std::experimental::filesystem;

int main(int argc, char* argv[])
{
	if(argc != 2)
	{
		std::cout << "usage: mkdir <path>\n";
		return -1;
	}

	fs::path pathToCreate(argv[1]);

	if (fs::exists(pathToCreate))
		return 0;

	if (fs::create_directories(pathToCreate))
		return 0;
	else
	{
		std::cout << "couldn't create directory: " << pathToCreate.string() << std::endl;
		return -1;
	}
}

```

=={{header|C sharp|C#}}==

```csharp
System.IO.Directory.CreateDirectory(path)
```



## Common Lisp


```lisp

(ensure-directories-exist "your/path/name")

```



## Clojure


```clojure
(defn mkdirp [path]
  (let [dir (java.io.File. path)]
    (if (.exists dir)
      true
      (.mkdirs dir))))
```



## D


```D
import std.stdio;

void main() {
    makeDir("parent/test");
}

/// Manual implementation of what mkdirRecurse in std.file does.
void makeDir(string path) out {
    import std.exception : enforce;
    import std.file : exists;
    enforce(path.exists, "Failed to create the requested directory.");
} body {
    import std.array : array;
    import std.file;
    import std.path : pathSplitter, chainPath;

    auto workdir = "";
    foreach (dir; path.pathSplitter) {
        workdir = chainPath(workdir, dir).array;
        if (workdir.exists) {
            if (!workdir.isDir) {
                import std.conv : text;
                throw new FileException(text("The file ", workdir, " in the path ", path, " is not a directory."));
            }
        } else {
            workdir.mkdir();
        }
    }
}
```



## Elixir

Tries to create the directory `path`. Missing parent directories are created.

```elixir
File.mkdir_p("./path/to/dir")
```



## ERRE

ERRE has the procedure "OS_MKDIR" in PC.LIB standard library, that creates a directory with
all missing parents. Existing directory are simply ignored.

```ERRE
OS_MKDIR("C:\EXAMPLES\03192015")
```


=={{header|F_Sharp|F#}}==
<p>The library function System.IO.Directory.CreateDirectory also returns a DirectoryInfo
object of the deepest directory in the path.</p>
<p>In the F# REPL:</p>
{{out}}

```txt

> System.IO.Directory.CreateDirectory (System.IO.Directory.GetCurrentDirectory())
;;
val it : System.IO.DirectoryInfo =
  Temp {Attributes = Directory;
        CreationTime = 2016-06-01 04:12:25;
        CreationTimeUtc = 2016-06-01 02:12:25;
        Exists = true;
        Extension = "";
        FullName = "C:\Users\Kai\AppData\Local\Temp";
        LastAccessTime = 2016-08-18 20:42:21;
        LastAccessTimeUtc = 2016-08-18 18:42:21;
        LastWriteTime = 2016-08-18 20:42:21;
        LastWriteTimeUtc = 2016-08-18 18:42:21;
        Name = "Temp";
        Parent = Local;
        Root = C:\;}
>
```



## Factor

The <code>make-directories</code> word performs this task. Note the value of <code>current-directory</code> is used as the base directory if a relative pathname is given.

```factor
USE: io.directories
"path/to/dir" make-directories
```

The implementation of <code>make-directories</code>:

```factor
USING: combinators.short-circuit io.backend io.files
io.pathnames kernel sequences ;
IN: io.directories
: make-directories ( path -- )
    normalize-path trim-tail-separators dup
    { [ "." = ] [ root-directory? ] [ empty? ] [ exists? ] } 1||
    [ make-parent-directories dup make-directory ] unless drop ;
```



## Gambas


```gambas
Public Sub Form_Open()

If Not Exist(User.home &/ "TestFolder") Then Mkdir User.Home &/ "TestFolder"

End
```



## Go

The standard packages include <tt>[http://golang.org/pkg/os/#MkdirAll os.MkdirAll]</tt> which does exactly this
(and its source is also available via that link).

```go
	os.MkdirAll("/tmp/some/path/to/dir", 0770)
```



## Haskell


```Haskell

import System.Directory (createDirectory, setCurrentDirectory)
import Data.List.Split  (splitOn)

main :: IO ()
main = do
  let path = splitOn "/" "path/to/dir"
  mapM_ (\x -> createDirectory x >> setCurrentDirectory x) path

```



## J


The verb <code>pathcreate</code> in the addon package [http://www.jsoftware.com/jwiki/Addons/general/dirutils general/dirutils] will create any non-existing directories in a path. It works on Windows, Linux and OS X.


```J
require 'general/dirutils'
pathcreate '/tmp/some/path/to/dir'
```


Code is similar to the following:

```J
pathcreate=: monad define
  todir=. termsep_j_ jpathsep y
  todirs=. }. ,each /\ <;.2 todir  NB. base dirs
  msk=. -.direxist todirs          NB. 1 for each non-existing dir
  msk=. 0 (i. msk i: 0)}msk
  dircreate msk#todirs             NB. create non-existing base dirs
)

dircreate=: monad define
  y=. boxxopen y
  msk=. -.direxist y
  if. ''-:$msk do. msk=. (#y)#msk end.
  res=. 1!:5 msk#y
  msk #inv ,res
)

direxist=: 2 = ftype&>@:boxopen
```



## Java

The Java method for this is ''mkdirs'' and can be found in java.io.File. The source is in the ''src.zip'' of the JDK root directory.

```java
import java.io.File;

public interface Test {

    public static void main(String[] args) {
        try {
            File f = new File("C:/parent/test");
            if (f.mkdirs())
                System.out.println("path successfully created");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```



## JavaScript

{{works with|Node.js}}

Simplified version of the popular [https://www.npmjs.org/package/mkdirp mkdirp library]:


```Javascript
var path = require('path');
var fs = require('fs');

function mkdirp (p, cb) {
    cb = cb || function () {};
    p = path.resolve(p);

    fs.mkdir(p, function (er) {
        if (!er) {
            return cb(null);
        }
        switch (er.code) {
            case 'ENOENT':
                // The directory doesn't exist. Make its parent and try again.
                mkdirp(path.dirname(p), function (er) {
                    if (er) cb(er);
                    else mkdirp(p, cb);
                });
                break;

                // In the case of any other error, something is borked.
            default:
                cb(er);
                break;
        }
    });
}
```



## Julia

{{works with|Julia|0.6}}


```julia
mkpath("/tmp/unusefuldir/helloworld.d/test123")
```



## Kotlin


```scala
// version 1.0.6

import java.io.File

fun main(args: Array<String>) {
    // using built-in mkdirs() method
    val success = File("./path/to/dir").mkdirs()
    if (success) println("Directory path was created successfully")
    else         println("Failed to create directory path")
}
```


{{out}}

```txt

Directory path was created successfully

```



## Lua

The ubiquitous luafilesystem module contains lfs.mkdir but this does not have an option equivalent to the posix mkdir -p.
Instead, the function shown here uses package.config to determine the correct directory separator for the OS and then iterates over the path string to create each individual folder in turn.

```Lua
require("lfs")

function mkdir (path)
  local sep, pStr = package.config:sub(1, 1), ""
  for dir in path:gmatch("[^" .. sep .. "]+") do
    pStr = pStr .. dir .. sep
    lfs.mkdir(pStr)
  end
end

mkdir("C:\\path\\to\\dir") -- Quoting backslashes requires escape sequence
```

Note that attempting to run lfs.mkdir for a path that already exists writes no changes to disk and returns nil.


## Mathematica

Creates directory specified by path, creating intermediate directories as necessary, and never fails if path already exists.

```Mathematica
mkdirp[path_] := Quiet[CreateDirectory[path,{CreateIntermediateDirectories->True}],{CreateDirectory::filex}]
```



## NewLISP


```newlisp
(define (mkdir-p mypath)
    (if (= "/" (mypath 0)) ;; Abs or relative path?
        (setf /? "/")
        (setf /? "")
    )
    (setf path-components (clean empty? (parse mypath "/"))) ;; Split path and remove empty elements
    (for (x 0 (length path-components))
        (setf walking-path (string /? (join (slice path-components 0 (+ 1 x)) "/")))
        (make-dir walking-path)
    )
)

;; Using user-made function...
(mkdir-p "/tmp/rosetta/test1")

;; ... or calling OS command directly.
(! "mkdir -p /tmp/rosetta/test2")
(exit)
```



## Objeck


```objeck
class Program {
  function : Main(args : String[]) ~ Nil {
    System.IO.File.Directory->CreatePath("your/path/name")->PrintLine();
  }
}
```



## Perl


Using the File::Path core module:


```perl
use File::Path qw(make_path);

make_path('path/to/dir')
```



## Perl 6

{{Works with|rakudo|2016.06}}

There is a built-in function for this:


```perl6
mkdir 'path/to/dir'
```


Alternatively, a custom solution (as per task description) that only uses the built-in <tt>mkdir</tt> non-recursively. The "triangle reduce" meta-operator <code>[\ ]</code> is used get the intermediate results of a left fold with the comma operator on the list of path elements.


```perl6
for [\,] $*SPEC.splitdir("../path/to/dir") -> @path {
    mkdir $_ unless .e given $*SPEC.catdir(@path).IO;
}
```



## Phix

There's a builtin for that

```Phix
if not create_directory("myapp/interface/letters") then
    crash("Filesystem problem - could not create the new folder")
end if
```

The implementation in builtins/pfile.e is as follows (see there for initf() etc):

```Phix
global function create_directory(string name, integer mode=0o700, bool make_parent=1)
bool ret
    if not finit then initf() end if

    if length(name)=0 then
        ?9/0
    end if

    name = get_proper_path(name)

    -- Remove any trailing slash.
    if name[$]=SLASH then
        name = name[1..$-1]
    end if

    if make_parent then
        integer pos = rfind(SLASH, name)
        if pos!=0 then
            string parent = name[1..pos-1]
            if file_exists(parent) then
                if file_type(parent)!=FILETYPE_DIRECTORY then ?9/0 end if
            else
                if not create_directory(parent, mode, make_parent) then
                    return 0
                end if
            end if
        end if
    end if

    if platform()=LINUX then
        ret = not c_func(xCreateDirectory, {name, mode})
    elsif platform()=WINDOWS then
        ret = c_func(xCreateDirectory, {name, 0})
    end if

    return ret
end function
```

Of course you could also use system("mkdir -p path/to/dir") or whatever.


## PicoLisp


```PicoLisp
(call "mkdir" "-p" "path/to/dir")
```



## PowerShell


```PowerShell

New-Item -Path ".\path\to\dir" -ItemType Directory -ErrorAction SilentlyContinue

```



## Python


```Python

from errno import EEXIST
from os import mkdir, curdir
from os.path import split, exists

def mkdirp(path, mode=0777):
    head, tail = split(path)
    if not tail:
        head, tail = split(head)
    if head and tail and not exists(head):
        try:
            mkdirp(head, mode)
        except OSError as e:
            # be happy if someone already created the path
            if e.errno != EEXIST:
                raise
        if tail == curdir:  # xxx/newdir/. exists if xxx/newdir exists
            return
    try:
        mkdir(path, mode)
    except OSError as e:
        # be happy if someone already created the path
        if e.errno != EEXIST:
            raise

```


Above is a modified version of the standard library's <code>os.makedirs</code>, for pedagogical purposes. In practice, you would be more likely to use the standard library call:


```Python

def mkdirp(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else: raise

```


In Python3 this becomes even simpler:


```Python

def mkdirp(path):
    os.makedirs(path, exist_ok=True)

```



## Racket


Uses <code>make-directory*</code> (NB the star &mdash; that causes the intermediate directories to be produced).

Canonical documentation at [http://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28lib._racket%2Ffile..rkt%29._make-directory*%29%29 Racket Documentation for <var>Filesystem</var>]

<blockquote>Creates directory specified by <var>path</var>, creating intermediate
directories as necessary, and never failing if <var>path</var> exists already.
</blockquote>


```racket
#lang racket
(define path-str "/tmp/woo/yay")
(define path/..-str "/tmp/woo")

;; clean up from a previous run
(when (directory-exists? path-str)
  (delete-directory path-str)
  (delete-directory path/..-str))
;; delete-directory/files could also be used -- but that requires goggles and rubber
;; gloves to handle safely!

(define (report-path-exists)
  (printf "~s exists (as a directory?):~a~%~s exists (as a directory?):~a~%~%"
          path/..-str (directory-exists? path/..-str)
          path-str (directory-exists? path-str)))

(report-path-exists)

;; Really ... this is the only bit that matters!
(make-directory* path-str)

(report-path-exists)
```


{{out}}

```txt
"/tmp/woo" exists (as a directory?):#f
"/tmp/woo/yay" exists (as a directory?):#f

"/tmp/woo" exists (as a directory?):#t
"/tmp/woo/yay" exists (as a directory?):#t


```



## REXX

The following works with any modern (Microsoft) Windows &reg; and/or DOS.

Operating system note:   all versions of Microsoft &reg; DOS require the use of a blackslash   [<big><big>'''\'''</big></big>]   instead of a forward slash   [<big><big>'''/'''</big></big>].

Usage note:   without the error messages being suppressed, the   '''MKDIR'''   command will issue an error message if the subdirectory (or its path) already exists.

```rexx
/*REXX program  creates a  directory (folder)  and all its  parent paths  as necessary. */
trace off                                              /*suppress possible warning msgs.*/

dPath = 'path\to\dir'                                  /*define directory (folder) path.*/

'MKDIR'  dPath  "2>nul"                                /*alias could be used:  MD dPath */
                                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

System("mkdir C:\Ring\docs")
isdir("C:\Ring\docs")

see isdir("C:\Ring\docs") + nl
func isdir cDir
     try
        dir(cDir)
        return true
     catch
        return false
     done

```



## Ruby


```ruby
require 'fileutils'
FileUtils.mkdir_p("path/to/dir")
```

mkdir_p also takes an array of pathnames instead of a single pathname as an argument.
mkdir_p is aliased as: mkpath, makedirs.


## Run BASIC


```runbasic

files #f, "c:\myDocs"		' check for directory
if #f hasanswer() then
   if #f isDir() then		' is it a file or a directory
    print "A directory exist"
    else
    print "A file exist"
   end if
 else
  shell$("mkdir c:\myDocs"    ' if not exist make a directory
end if
```

The following info about files / directory
FILE ACCESSOR methods
#handle HASANSWER() - Return non-zero if the file accessor has at least one
resulting row.
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


## Rust


```rust
use std::fs;

fn main() {
    fs::create_dir_all("./path/to/dir").expect("An Error Occured!")
}
```



## Scala


```Scala
new java.io.File("/path/to/dir").mkdirs
```

Alternative (platform-independent) for the library function:

```Scala
import java.io.File

def mkdirs(path: List[String]) = // return true if path was created
    path.tail.foldLeft(new File(path.head)){(a,b) => a.mkdir; new File(a,b)}.mkdir

mkdirs(List("/path", "to", "dir"))

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/cli_cmds.htm cli_cmds.s7i]
defines the function doMkdirCmd, which is used below.


```seed7
$ include "seed7_05.s7i";
  include "cli_cmds.s7i";

const proc: main is func
  begin
    doMkdirCmd(argv(PROGRAM), TRUE);
  end func;
```


The library cli_cmds.s7i defines also
[http://seed7.sourceforge.net/libraries/cli_cmds.htm#doMkdir%28inout_string%29 doMkdir] (Make directories like the Unix mkdir command)
and [http://seed7.sourceforge.net/libraries/cli_cmds.htm#doMd%28inout_string%29 doMd] (Make directories like the DOS md command).
This functions read the parameters and options from a string.
The reading is done according to Unix respectively DOS/Windows rules.
The function doMkdir is used in the alternate solution below:


```seed7
$ include "seed7_05.s7i";
  include "cli_cmds.s7i";

const proc: main is func
  local
    var string: parameters is "";
  begin
    parameters := join(argv(PROGRAM), " ");
    doMkdir(parameters);
  end func;
```



## Sidef


```ruby
Dir.new(Dir.cwd, "path", "to", "dir").make_path;   # works cross-platform
```



## Tcl

Tcl's built in <code>file mkdir</code> works exactly this way:

```tcl>file mkdir ./path/to/dir</lang

If a directory cannot be made (e.g., because it would involve making a directory with the same name as an existing file) the command will throw a trappable error, as normal for Tcl commands.


## UNIX Shell

{{works with|Bourne Again SHell}}


```bash
function mkdirp() { mkdir -p "$1"; }
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Sub Main()
        System.IO.Directory.CreateDirectory("some/where")
    End Sub

End Module
```



## zkl

This is for Unix as zkl doesn't have a built in mkdir method.

```zkl
System.cmd("mkdir -p ../foo/bar")
```

The system error code is returned (0 in this case).

```zkl
fcn mkdir(path) { System.cmd("mkdir -p "+path) }
```

{{out}}

```txt

zkl: mkdir("../foo/bar")
0
zkl: mkdir("/foo/bar")
mkdir: cannot create directory ‘/foo’: Permission denied
256

```


{{omit from|Axe}}
