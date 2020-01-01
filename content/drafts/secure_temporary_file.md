+++
title = "Secure temporary file"
description = ""
date = 2019-07-24T21:14:59Z
aliases = []
[extra]
id = 2178
[taxonomies]
categories = []
tags = []
+++

{{Task|Programming environment operations}}

;Task:
Create a temporary file, '''securely and exclusively''' (opening it such that there are no possible [[race condition|race conditions]]).

It's fine assuming local filesystem semantics (NFS or other networking filesystems can have signficantly more complicated semantics for satisfying the "no race conditions" criteria).

The function should automatically resolve name collisions and should only fail in cases where permission is denied, the filesystem is read-only or full, or similar conditions exist (returning an error or raising an exception as appropriate to the language/environment).





## Ada

Ada creates a temporary file whenever the create procedure is called without file name. That temporary file is automatically deleted at the end of the program creating the file.

This example creates a temporary file, writes to the file, then reads from the file.


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Temp_File is
   Temp : File_Type;
   Contents : String(1..80);
   Length : Natural;
begin
   -- Create a temporary file
   Create(File => Temp);
   Put_Line(File => Temp, Item => "Hello World");
   Reset(File => Temp, Mode => In_File);
   Get_Line(File => Temp, Item => Contents, Last => Length);
   Put_Line(Contents(1..Length));
end Temp_File;
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
The file is automatically deleted when closed.

```bbcbasic
      file% = FNopentempfile
      IF file% = 0 ERROR 100, "Failed to open temp file"
      PRINT #file%, "Hello world!"
      PTR#file% = 0
      INPUT #file%, message$
      CLOSE #file%
      PRINT message$
      END

      DEF FNopentempfile
      LOCAL pname%, hfile%, chan%
      OPEN_EXISTING = 3
      FILE_FLAG_DELETE_ON_CLOSE = &4000000
      GENERIC_READ = &80000000
      GENERIC_WRITE = &40000000
      INVALID_HANDLE_VALUE = -1
      DIM pname% LOCAL 260
      FOR chan% = 5 TO 12
        IF @hfile%(chan%) = 0 EXIT FOR
      NEXT
      IF chan% > 12 THEN = 0
      SYS "GetTempFileName", @tmp$, "BBC", 0, pname%
      SYS "CreateFile", $$pname%, GENERIC_READ OR GENERIC_WRITE, 0, 0, \
      \                 OPEN_EXISTING, FILE_FLAG_DELETE_ON_CLOSE, 0 TO hfile%
      IF hfile% = INVALID_HANDLE_VALUE THEN = 0
      @hfile%(chan%) = hfile%
      = chan%
```

'''Output:'''

```txt

Hello world!

```



## C


```cpp
#include <iostream>
#include <stdio.h>

int main(void)
{
  FILE *fh = tmpfile(); /* file is automatically deleted when program exits */
  /* do stuff with stream "fh" */
  fclose(fh);
  /* The C standard library also has a tmpnam() function to create a file
    for you to open later. But you should not use it because someone else might
    be able to open the file from the time it is created by this function to the
    time you open it. */
  return 0;
}
```


The following {{works with|POSIX}}

```cpp
#include <iostream>
#include <stdio.h>

int main(void)
{
  char filename[] = "/tmp/prefixXXXXXX";
  int fd = mkstemp(filename);
  puts(filename);
  /* do stuff with file descriptor "fd" */
  close(fd);
  return 0;
}
```


## C#


```c#
using System;
using System.IO;

Console.WriteLine(Path.GetTempFileName());
```



## Clojure

It is good practice to explicitly delete temp files immediately once they've been used.

```clojure
(let [temp-file (java.io.File/createTempFile "pre" ".suff")]
  ; insert logic here that would use temp-file
  (.delete temp-file))
```



## D

{{works with|Tango}}

```d
module tempfile ;
import tango.io.TempFile, tango.io.Stdout ;

void main(char[][] args) {

  // create a temporary file that will be deleted automatically when out of scope
  auto tempTransient = new TempFile(TempFile.Transient) ;
  Stdout(tempTransient.path()).newline ;

  // create a temporary file, still persist after the TempFile object has been destroyed
  auto tempPermanent = new TempFile(TempFile.Permanent) ;
  Stdout(tempPermanent.path()).newline ;

  // both can only be accessed by the current user (the program?).
}
```



## Emacs Lisp

<code>make-temp-file</code> creates a new empty temporary file, with perms "0700" so read-write to the current user only.


```Lisp
(make-temp-file "prefix")
=>
"/tmp/prefix25452LPe"
```


<code>make-temp-file</code> is available in GNU Emacs 21 up, but not XEmacs 21.4.  The Gnus included with XEmacs 21.4 has an <code>mm-make-temp-file</code> in its <code>mm-util.el</code>, or the [[APEL]] library can supply a <code>make-temp-file</code> (and for Emacs 20 too).


## Fortran

Supposing F is an integer variable, whose value might be 10. This is the I/O unit number, and would be used in READ(F,''etc.'' and WRITE(F,''etc.'' statements.
```Fortran
        OPEN (F,STATUS = 'SCRATCH')   !Temporary disc storage.
```

Other attributes might be specified depending on the intended usage, but note that no file name is given. When the file is closed, its storage vanishes back to the file system.

Following the OPEN statement with <code>INQUIRE (F,NAME = FNAME); WRITE (6,*) FNAME</code> yields <code> C:\DOCUME~1\Nicky\LOCALS~1\Temp\FOR57.tmp</code> which is the DOS style short (8.3) file name for <code>C:\Documents and Settings\Nicky\Local Settings\Temp\FOR57.tmp</code> and the example's numerical value of 57 will be different on another run. Thus, the file could be interfered with, except that a file opened with WRITE access is exclusive-use, and there is no point in opening a SCRATCH file with READONLY (to allow sharing) because it cannot be a pre-existing disc file. However, the actual behaviour of a particular file system and compiler may or may not support such refinements as shared access as implied by non-standard keywords as READONLY, etc.


## Go

Use <code>[https://golang.org/pkg/io/ioutil/#TempFile ioutil.TempFile]</code>

```go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

func main() {
	f, err := ioutil.TempFile("", "foo")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	// We need to make sure we remove the file
	// once it is no longer needed.
	defer os.Remove(f.Name())

	// … use the file via 'f' …
	fmt.Fprintln(f, "Using temporary file:", f.Name())
	f.Seek(0, 0)
	d, err := ioutil.ReadAll(f)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Wrote and read: %s\n", d)

        // The defer statements above will close and remove the
        // temporary file here (or on any return of this function).
}
```

{{out}}

```txt

Wrote and read: Using temporary file: /tmp/foo054003078

```



## Groovy


```groovy
def file = File.createTempFile( "xxx", ".txt" )

// There is no requirement in the instructions to delete the file.
//file.deleteOnExit()

println file
```


Output:

```txt
C:\DOCUME~1\ROGER~1.GLO\LOCALS~1\Temp\xxx8918700806071036530.txt
```



## Haskell


```haskell
import System.IO

main = do (pathOfTempFile, h) <- openTempFile "." "prefix.suffix" -- first argument is path to directory where you want to put it
          -- do stuff with it here; "h" is the Handle to the opened file
          return ()
```



## HicEst


```HicEst
! The "scratch" option opens a file exclusively for the current process.
! A scratch file is automatically deleted upon process termination.

OPEN( FIle='TemporaryAndExclusive', SCRatch, IOStat=ErrNr)
WRITE(FIle='TemporaryAndExclusive') "something"
WRITE(FIle='TemporaryAndExclusive', CLoSe=1) ! explicit "close" deletes file

! Without "scratch" access can be controlled by "denyread", "denywrite", "denyreadwrite" options.

OPEN( FIle='DenyForOthers', DenyREAdWRIte, IOStat=ErrNr)
WRITE(FIle='DenyForOthers') "something"
WRITE(FIle='DenyForOthers', DELETE=1)
```


=={{header|Icon}} and {{header|Unicon}}==

A posix-based solution that works in both languages:


```unicon
procedure main()
    write("Creating: ",fName := !open("mktemp","rp"))
    write(f := open(fName,"w"),"Hello, world")
    close(f)
end
```



## Java



```java
import java.io.File;
import java.io.IOException;

public class CreateTempFile {
    public static void main(String[] args)  {
        try {
            //create a temp file
            File temp = File.createTempFile("temp-file-name", ".tmp");
            System.out.println("Temp file : " + temp.getAbsolutePath());
        }
        catch(IOException e) {
            e.printStackTrace();
    	}
    }
}
```



## Julia

{{works with|Linux}} On Unix systems, Julia calls <code>mkstemp</code> to securely open a temporary file.  This is likely multi-thread safe, check your system documentation for verification.  This code should also work on Windows, but I've not verified that.

```Julia

msg = "Rosetta Code, Secure temporary file, implemented with Julia."

(fname, tio) = mktemp()
println(fname, " created as a temporary file.")
println(tio, msg)
close(tio)
println("\"", msg, "\" written to ", fname)

```

Files written to <code>\tmp</code> persist for the login session, and are thus truly temporary.  If the environment variable <code>TMPDIR</code> is set, the temporary file is created in this directory.  In this case, the file may not be properly temporary.

```Julia

ENV["TMPDIR"] = pwd()
(fname, tio) = mktemp()
println(fname, " created as a \"temporary\" file.")
println(tio, msg)
close(tio)
println("\"", msg, "\" written to ", fname)

```


{{out}}

```txt

/tmp/tmphe0qlu created as a temporary file.
"Rosetta Code, Secure temporary file, implemented with Julia." written to /tmp/tmphe0qlu
/home/mike/rosetta/julia/tmpVNGK8D created as a "temporary" file.
"Rosetta Code, Secure temporary file, implemented with Julia." written to /home/joeb/rosetta/julia/tmpVNGK8D
$ cat /tmp/tmphe0qlu
Rosetta Code, Secure temporary file, implemented with Julia.
$ ls -l /tmp/tmphe0qlu
-rw------- 1 joeb joeb 61 Apr 13 13:18 /tmp/tmphe0qlu
$ cat tmpVNGK8D
Rosetta Code, Secure temporary file, implemented with Julia.
$ ls -l tmpVNGK8D
-rw------- 1 joeb jeob 61 Apr 13 13:18 tmpVNGK8D

```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    try {
        val tf = File.createTempFile("temp", ".tmp")
        println(tf.absolutePath)
        tf.delete()
    }
    catch (ex: Exception) {
        println(ex.message)
    }
}
```

Sample output (Ubuntu v14.04):
{{out}}

```txt

/tmp/temp1551492276377305257.tmp

```



## Lua


```lua
fp = io.tmpfile()

-- do some file operations

fp:close()
```



## M2000 Interpreter

tmp files automatic deleted when Environment end (per running environment)


```M2000 Interpreter

Module Checkit {
      \\ we get a tempname$ choosed  from Windows
      a$=tempname$
      Try ok  {
            \\ we can use wide to export in utf-16le
            \\ without wide we export as Ansi (set Local to desired language)
            Rem Locale 1033 ' when no use of wide
            Open a$ for wide output exclusive as #f
                  wait 10
                  \\ Notepad can't open, because we open it for exclusive use
                  Win "Notepad", a$
                  Print  #f, "something"
                  Print "Press a key";Key$
            Close #f
      }
      If error or not ok then Print Error$
      Win "Notepad", a$
}
Checkit


```



## Mathematica


```Mathematica
tmp = OpenWrite[]
Close[tmp]
```



## NetRexx

{{incomplete|NetRexx|JVM Windows related bug workaround JDK-4715154}}
```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method makeTempFile(prefix = String, suffix = String null, startDir = String null) -
  public static signals IOException returns File
  if startDir \= null then fStartDir = File(startDir)
  else                     fStartDir = null
  ff = File.createTempFile(prefix, suffix, fStartDir)
  ff.deleteOnExit() -- make sure the file is deleted at termination
  return ff

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  do
    tempFiles = [ -
      makeTempFile('rexx'), -
      makeTempFile('rexx', '.rex'), -
      makeTempFile('rexx', null, './tmp') -
    ]
    loop fFile over tempFiles
      fName = fFile.getCanonicalPath()
      say 'Temporary file:' fName
      end fFile
  catch ex = IOException
    ex.printStackTrace()
  end
  return

```



## OCaml

From the module Filename, one can use the functions [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Filename.html#VALtemp_file temp_file] or [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Filename.html#VALopen_temp_file open_temp_file]

```ocaml
# Filename.temp_file "prefix." ".suffix" ;;
- : string = "/home/blue_prawn/tmp/prefix.301f82.suffix"
```



## Octave


Octave has several related functions

```Matlab
  [FID, MSG] = tmpfile();    % Return the file ID corresponding to a new temporary
  filename = tmpnam (...);   % generates temporary file name, but does not open file
  [FID, NAME, MSG] = mkstemp (TEMPLATE, DELETE);    % Return the file ID corresponding to a new temporary file with a unique name created from TEMPLATE.
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|SysUtils}}

```pascal
Program TempFileDemo;

uses
  SysUtils;

var
  tempFile: text;

begin
  assign (Tempfile, GetTempFileName);
  rewrite (tempFile);
  writeln (tempFile, 5);
  close (tempFile);
end.
```



## Perl

function interface:

```perl
use File::Temp qw(tempfile);
$fh = tempfile();
($fh2, $filename) = tempfile(); # this file stays around by default
print "$filename\n";
close $fh;
close $fh2;
```


object-oriented interface:

```perl
use File::Temp;
$fh = new File::Temp;
print $fh->filename, "\n";
close $fh;
```



## Perl 6

{{works with|Rakudo|2017.09}}
This is something best done with a module which is heavily tested, tries to account for all corner cases and automatically cleans up after itself.

Almost verbatim from the synopsis:

```perl6
use File::Temp;

# Generate a temp file in a temp dir
my ($filename0,$filehandle0) = tempfile;

# specify a template for the filename
#  * are replaced with random characters
my ($filename1,$filehandle1) = tempfile("******");

# Automatically unlink files at DESTROY (this is the default)
my ($filename2,$filehandle2) = tempfile("******", :unlink);

# Specify the directory where the tempfile will be created
my ($filename3,$filehandle3) = tempfile(:tempdir("/path/to/my/dir"));

# don't unlink this one
my ($filename4,$filehandle4) = tempfile(:tempdir('.'), :!unlink);

# specify a prefix, a suffix, or both for the filename
my ($filename5,$filehandle5) = tempfile(:prefix('foo'), :suffix(".txt"));
```



## Phix

The temp_file() function (see builtins/pfile.e) can be used for this:

```Phix
pp(temp_file())
{integer fn, string name} = temp_file("myapp/tmp","data","log","wb")
pp({fn,name})
close(fn)
{} = delete_file(name)
```

{{out}}

```txt

`C:\Users\Pete\AppData\Local\Temp\419750.tmp`
{3, `C:\Program Files (x86)\Phix\myapp\tmp\data408865.log`}

```

If you don't provide an open mode (one of "w", "wb", "a", or "ab") then there is a 1-in-a-million
chance someone else will beat you to the punch; if you do provide one, it will open/loop for you.


## PHP


```php
$fh = tmpfile();
// do stuff with $fh
fclose($fh);
// file removed when closed

// or:
$filename = tempnam('/tmp', 'prefix');
echo "$filename\n";
// open $filename and do stuff with it
```



## PicoLisp

The 'tmp' function returns temporary file names which are exclusively for the
current process (based on the process ID). These files are automatically deleted
upon process termination. Background tasks within a single PicoLisp process is
always non-preemptive, therefore dedicated locks are usually not necessary. If
they are (e.g. because such a file name is passed to a child process), explicit
locks with the 'ctl' functions are possible.

```PicoLisp
: (out (tmp "foo") (println 123))         # Write tempfile
-> 123

: (in (tmp "foo") (read))                 # Read tempfile
-> 123

: (let F (tmp "foo")
   (ctl F                                 # Get exclusive lock
      (in F
         (let N (read)                    # Atomic increment
            (out F (println (inc N))) ) ) ) )
-> 124
```



## PowerShell


```PowerShell

$tempFile = [System.IO.Path]::GetTempFileName()
Set-Content -Path $tempFile -Value "FileName = $tempFile"
Get-Content -Path $tempFile
Remove-Item -Path $tempFile

```

{{Out}}

```txt

FileName = C:\Users\Owner\AppData\Local\Temp\tmpB68.tmp

```



## PureBasic


```PureBasic
Procedure.s TempFile()
  Protected a, Result$

  For a = 0 To 9999
    Result$ = GetTemporaryDirectory() + StringField(GetFilePart(ProgramFilename()),1,".")
    Result$ + "_" + Str(ElapsedMilliseconds()) + "_(" + RSet(Str(a),4,"0") + ").tmp"
    If FileSize(Result$) = -1                                      ;  -1 = File not found
      ProcedureReturn Result$
    EndIf
  Next

  ProcedureReturn ""
EndProcedure


Define File, File$

File$ = TempFile()
If File$ <> ""
  File = CreateFile(#PB_Any, File$)
  If File <> 0
    WriteString(File, "Some temporary data here...")
    CloseFile(File)
  EndIf
EndIf
```



## Python

{{works with|Python|2.3+}}
In both cases, the temporary file will be deleted automatically when the file is closed. The invisible file will not be accessible on UNIX-like systems. You can use os.link to preserve the visible temporary file contents.



```python>>>
 import tempfile
>>> invisible = tempfile.TemporaryFile()
>>> invisible.name
'<fdopen>'
>>> visible = tempfile.NamedTemporaryFile()
>>> visible.name
'/tmp/tmpZNfc_s'
>>> visible.close()
>>> invisible.close()
```



More low level way, if you have special needs. This was the only option before Python 2.3:



```python
fd, path = tempfile.mkstemp()
try:
    # use the path or the file descriptor
finally:
    os.close(fd)
```



## Racket



```racket

#lang racket
(make-temporary-file)

```



## Ruby


```ruby
irb(main):001:0> require 'tempfile'
=> true
irb(main):002:0> f = Tempfile.new('foo')
=> #<File:/tmp/foo20081226-307-10p746n-0>
irb(main):003:0> f.path
=> "/tmp/foo20081226-307-10p746n-0"
irb(main):004:0> f.close
=> nil
irb(main):005:0> f.unlink
=> #<Tempfile: (closed)>
```



## Scala


```scala
import java.io.{File, FileWriter, IOException}

  def writeStringToFile(file: File, data: String, appending: Boolean = false) =
    using(new FileWriter(file, appending))(_.write(data))

  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  try {
    val file = File.createTempFile("_rosetta", ".passwd")
    // Just an example how you can fill a file
    using(new FileWriter(file))(writer => rawDataIter.foreach(line => writer.write(line)))
    scala.compat.Platform.collectGarbage() // JVM Windows related bug workaround JDK-4715154
    file.deleteOnExit()
    println(file)
  } catch {
    case e: IOException => println(s"Running Example failed: ${e.getMessage}")
  }
```



## Sidef


```ruby
var tmpfile = require('File::Temp');
var fh = tmpfile.new(UNLINK => 0);
say fh.filename;
fh.print("Hello, World!\n");
fh.close;
```



## Standard ML


```sml
val filename = OS.FileSys.tmpName ();
```



## Tcl

{{works with|Tcl|8.6}}
Will store the name of the file in the variable <code>filenameVar</code> and an open read-write channel on the file in the variable <code>chan</code>.

```Tcl
set chan [file tempfile filenameVar]
```

Note that because we're asking for the filename in the script, Tcl does not automatically clean the file. (There are cases where auto-cleaning would be really unwanted.) If we hadn't asked for it, the file would be automatically deleted (at a time that depends on platform constraints).

## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
tmpfile="test.txt"
ERROR/STOP CREATE (tmpfile,seq-E,-std-)
text="hello world"
FILE $tmpfile = text
- tmpfile "test.txt" can only be accessed by one user an will be deleted upon programm termination

```



## UNIX Shell


UNIX shell scripts cannot guarantee secure, race-free, exclusive access to an open file descriptor.
The best approach to working around this limitation is to create a directory (the ''mkdir'' command
is a wrapper around the atomic ''mkdir()'' system call) and then perform all temporary file operations
thereunder.


```bash
RESTOREUMASK=$(umask)
TRY=0
while :; do
   TRY=$(( TRY + 1 ))
   umask 0077
   MYTMP=${TMPDIR:-/tmp}/$(basename $0).$$.$(date +%s).$TRY
   trap "rm -fr $MYTMP" EXIT
   mkdir "$MYTMP" 2>/dev/null && break
done
umask "$RESTOREUMASK"
cd "$MYTMP" || {
   echo "Temporary directory failure on $MYTMP" >&2
   exit 1; }
```


Note that the shell special variable $$ (the PID of the currently ''exec()''-ed shell) is unique at any given
moment on a UNIX system, and $(date +%s) gives the time represented as a number of seconds since the UNIX epoch
(GNU date or any other with the %s extension).  So any name collision here is most likely "enemy action."
This code will loop, picking new names and resetting the ''trap'' (clean-up command) until it succeeds in making
a directory.  (Simple changes to the code could limit the number of attempts or implement a timeout).


## zkl

zkl uses the underlying OS to create these files, mkstemp (POSIX), _mktemp_s (Windows). Not at all sure Windows is race free however.
{{out}}

```txt

zkl: File.mktmp()
File(zklTmpFile082p1V)

```


{{omit from|HTML}}
{{omit from|Locomotive Basic}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|Openscad}}
{{omit from|PARI/GP}}
{{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables, which can't be listed from a program. Doesn't have background processes either. -->
{{omit from|ZX Spectrum Basic|No network filesystem or background processes}}

[[Category:File handling]]
