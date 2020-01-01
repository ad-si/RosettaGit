+++
title = "File modification time"
description = ""
date = 2019-10-10T02:42:04Z
aliases = []
[extra]
id = 2031
[taxonomies]
categories = []
tags = []
+++

{{task|File modification time|File System Operations}}
[[Category:Date and time]]
{{omit from|Befunge|No filesystem support}}
{{omit from|HTML}}
{{omit from|Locomotive Basic|Does not have a real time clock.}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|Retro}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
{{omit from|Axe}}
{{omit from|ZX Spectrum Basic|Does not have a real time clock.}}

;Task:
Get and set the modification time of a file.





## Ada

Ada does not allow you to change the date of a file but you can definitely read it:

```ada
with Ada.Directories;          use Ada.Directories;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;

procedure File_Time_Test is
begin
   Put_Line (Image (Modification_Time ("file_time_test.adb")));
end File_Time_Test;
```



## ALGOL 68

Definition of the PROC is lifted from the Algol68 Genie documentation.

```algol68
PROC get output = (STRING cmd) VOID:
IF STRING sh cmd = " " + cmd + " ; 2>&1";
   STRING output;
   execve output ("/bin/sh", ("sh", "-c", sh cmd), "", output) >= 0
THEN print (output) FI;
get output ("rm -rf WTC_1");				CO Ensure file doesn't exist CO
get output ("touch WTC_1");				CO Create file CO
get output ("ls -l --time-style=full-iso WTC_1");	CO Display its last modified time CO
get output ("touch -t 200109111246.40 WTC_1");		CO Change its last modified time CO
get output ("ls -l --time-style=full-iso WTC_1")	CO Verify it changed CO

```

{{Out}}

```txt
-rw-r--r-- 1 pcl pcl 0 2015-08-10 17:57:50.279854695 +0100 WTC_1
-rw-r--r-- 1 pcl pcl 0 2001-09-11 12:46:40.000000000 +0100 WTC_1
```



## AutoHotkey


```AutoHotkey
FileGetTime, OutputVar, output.txt
MsgBox % OutputVar
FileSetTime, 20080101, output.txt
FileGetTime, OutputVar, output.txt
MsgBox % OutputVar
```



## AWK

{{works with|gawk}}

```awk
@load "filefuncs"
BEGIN {

    name = "input.txt"

    # display time
    stat(name, fd)
    printf("%s\t%s\n", name, strftime("%a %b %e %H:%M:%S %Z %Y", fd["mtime"]) )

    # change time
    cmd = "touch -t 201409082359.59 " name
    system(cmd)
    close(cmd)

}
```


Non-GNU awk's don't have direct access to the filesystem but can execute system-commands.


```awk
#!/bin/awk -f
BEGIN {   	          # file modification time on Unix, using stat
   fn ="input.txt"

   cmd="stat " fn
   print "#", cmd
   system(cmd)            # just execute cmd

   cmd="stat -c %Y " fn   # seconds since the epoch
   print "#", cmd
   system(cmd)

   cmd="stat -c %y " fn   # human-readable format
   print "#", cmd
   system(cmd)

   print "##"
   cmd | getline x        # get output from cmd
  #print x
   close(cmd)

   n=split(x,stat," ")
  #for (i in stat) { print i, stat[i] }
   print "file:", fn
   print "date:", stat[1], "time:", stat[2]

### change filetime with touch:

   cmd="touch -t 201409082359.59 " fn
   print "#", cmd; system(cmd)

   cmd="stat " fn
   print "#", cmd; system(cmd)
}
```


{{out}}

```txt

# stat input.txt
  File: `input.txt'
  Size: 126       	Blocks: 8          IO Block: 4096   regular file
Device: 700h/1792d	Inode: 2195620     Links: 1
Access: (0644/-rw-r--r--)  Uid: (   48/  apache)   Gid: (   48/  apache)
Access: 2014-11-05 20:18:39.000000000 -0600
Modify: 2014-11-05 20:18:39.000000000 -0600
Change: 2014-11-05 20:18:39.000000000 -0600
# stat -c %Y input.txt
1415240319
# stat -c %y input.txt
2014-11-05 20:18:39.000000000 -0600
##
file: input.txt
date: 2014-11-05 time: 20:18:39.000000000

```


;See also
[[File modification time#Batch_File]],
[[File modification time#Run_BASIC|#Run_BASIC]],
[[File modification time#UNIX_Shell|#UNIX_Shell]]


## Batch File

{{works with|Windows NT|4}}

```dos
for %%f in (file.txt) do echo.%%~tf
```

The date/time format is dependent on the user's locale, like the contents of the <code>%DATE%</code> and <code>%TIME%</code> built-ins.
There is no built-in way of setting a file's modification time.


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM ft{dwLowDateTime%, dwHighDateTime%}
      DIM st{wYear{l&,h&}, wMonth{l&,h&}, wDayOfWeek{l&,h&}, \
      \      wDay{l&,h&}, wHour{l&,h&}, wMinute{l&,h&}, \
      \      wSecond{l&,h&}, wMilliseconds{l&,h&} }

      REM File is assumed to exist:
      file$ = @tmp$ + "rosetta.tmp"

      REM Get and display the modification time:
      file% = OPENIN(file$)
      SYS "GetFileTime", @hfile%(file%), 0, 0, ft{}
      CLOSE #file%
      SYS "FileTimeToSystemTime", ft{}, st{}
      date$ = STRING$(16, CHR$0)
      time$ = STRING$(16, CHR$0)
      SYS "GetDateFormat", 0, 0, st{}, 0, date$, LEN(date$) TO N%
      date$ = LEFT$(date$, N%-1)
      SYS "GetTimeFormat", 0, 0, st{}, 0, time$, LEN(time$) TO N%
      time$ = LEFT$(time$, N%-1)
      PRINT date$ " " time$

      REM Set the modification time to the current time:
      SYS "GetSystemTime", st{}
      SYS "SystemTimeToFileTime", st{}, ft{}
      file% = OPENUP(file$)
      SYS "SetFileTime", @hfile%(file%), 0, 0, ft{}
      CLOSE #file%

```



## C


===POSIX utime()===
utime() has a precision of one second.
This program would truncate the time to the last second, losing precision if the filesystem is more precise.

{{libheader|POSIX}}

```c
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
#include <utime.h>

const char *filename = "input.txt";

int main() {
  struct stat foo;
  time_t mtime;
  struct utimbuf new_times;

  if (stat(filename, &foo) < 0) {
    perror(filename);
    return 1;
  }
  mtime = foo.st_mtime; /* seconds since the epoch */

  new_times.actime = foo.st_atime; /* keep atime unchanged */
  new_times.modtime = time(NULL);    /* set mtime to current time */
  if (utime(filename, &new_times) < 0) {
    perror(filename);
    return 1;
  }

  return 0;
}
```


===BSD utimes()===
With [[BSD]], utime() is obsolete.
utimes() has a precision of 1 microsecond (where 1 second = 1000000 microseconds).

{{libheader|BSD libc}}

```c
#include <sys/stat.h>
#include <sys/time.h>
#include <err.h>

const char *filename = "input.txt";

int main() {
  struct stat foo;
  struct timeval new_times[2];

  if (stat(filename, &foo) < 0)
    err(1, "%s", filename);

  /* keep atime unchanged */
  TIMESPEC_TO_TIMEVAL(&new_times[0], &foo.st_atim);

  /* set mtime to current time */
  gettimeofday(&new_times[1], NULL);

  if (utimes(filename, new_times) < 0)
    err(1, "%s", filename);

  return 0;
}
```


=== POSIX utimensat() ===
[http://kernel.org/doc/man-pages/online/pages/man2/utimensat.2.html utimensat(2)] has a precision of 1 nanosecond (where 1 second = 10**9 nanoseconds).
Program needs to be linked with <code>-lrt</code>.

{{libheader|POSIX}}
{{works with|POSIX|-1.2008}}

```c
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <fcntl.h>
#include <stdio.h>

const char *filename = "input.txt";

int main() {
  struct stat foo;
  struct timespec new_times[2];

  if (stat(filename, &foo) < 0) {
    perror(filename);
    return 1;
  }

  /* keep atime unchanged */
  new_times[0] = foo.st_atim;

  /* set mtime to current time */
  clock_gettime(CLOCK_REALTIME, &new_times[1]);

  if (utimensat(AT_FDCWD, filename, new_times, 0) < 0) {
    perror(filename);
    return 1;
  }

  return 0;
}
```



###  Windows

Declare <code>FILETIME modtime;</code> and then use <code>GetFileTime(fh, NULL, NULL, &modtime);</code> to get the file modification time, or <code>SetFileTime(fh, NULL, NULL, &modtime);</code> to set it.

{{libheader|Win32}}

```c
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

/* Print "message: last Win32 error" to stderr. */
void
oops(const wchar_t *message)
{
	wchar_t *buf;
	DWORD error;

	buf = NULL;
	error = GetLastError();
	FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
	    FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
	    NULL, error, 0, (wchar_t *)&buf, 0, NULL);

	if (buf) {
		fwprintf(stderr, L"%ls: %ls", message, buf);
		LocalFree(buf);
	} else {
		/* FormatMessageW failed. */
		fwprintf(stderr, L"%ls: unknown error 0x%x\n",
		    message, error);
	}
}

int
setmodtime(const wchar_t *path)
{
	FILETIME modtime;
	SYSTEMTIME st;
	HANDLE fh;
	wchar_t date[80], time[80];

	fh = CreateFileW(path, GENERIC_READ | FILE_WRITE_ATTRIBUTES,
	    0, NULL, OPEN_EXISTING, 0, NULL);
	if (fh == INVALID_HANDLE_VALUE) {
		oops(path);
		return 1;
	}

	/*
	 * Use GetFileTime() to get the file modification time.
	 */
	if (GetFileTime(fh, NULL, NULL, &modtime) == 0)
		goto fail;
	FileTimeToSystemTime(&modtime, &st);
	if (GetDateFormatW(LOCALE_USER_DEFAULT, DATE_LONGDATE, &st, NULL,
	    date, sizeof date / sizeof date[0]) == 0 ||
	    GetTimeFormatW(LOCALE_USER_DEFAULT, 0, &st, NULL,
	    time, sizeof time / sizeof time[0]) == 0)
		goto fail;
	wprintf(L"%ls: Last modified at %s at %s (UTC).\n",
	    path, date, time);

	/*
	 * Use SetFileTime() to change the file modification time
	 * to the current time.
	 */
	GetSystemTime(&st);
	if (GetDateFormatW(LOCALE_USER_DEFAULT, DATE_LONGDATE, &st, NULL,
	    date, sizeof date / sizeof date[0]) == 0 ||
	    GetTimeFormatW(LOCALE_USER_DEFAULT, 0, &st, NULL,
	    time, sizeof time / sizeof time[0]) == 0)
		goto fail;
	SystemTimeToFileTime(&st, &modtime);
	if (SetFileTime(fh, NULL, NULL, &modtime) == 0)
		goto fail;
	wprintf(L"%ls: Changed to %s at %s (UTC).\n", path, date, time);

	CloseHandle(fh);
	return 0;

fail:
	oops(path);
	CloseHandle(fh);
	return 1;
}

/*
 * Show the file modification time, and change it to the current time.
 */
int
main()
{
	int argc, i, r;
	wchar_t **argv;

	/* MinGW never provides wmain(argc, argv). */
	argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argv == NULL) {
		oops(L"CommandLineToArgvW");
		exit(1);
	}

	if (argc < 2) {
		fwprintf(stderr, L"usage: %ls file...\n", argv[0]);
		exit(1);
	}

	r = 0;
	for (i = 1; argv[i] != NULL; i++)
		if (setmodtime(argv[i])) r = 1;
	return r;
}
```



## C++

compiled with g++ -lboost_filesystem <sourcefile> -o <destination file>

```cpp
#include <boost/filesystem/operations.hpp>
#include <ctime>
#include <iostream>

int main( int argc , char *argv[ ] ) {
   if ( argc != 2 ) {
      std::cerr << "Error! Syntax: moditime <filename>!\n" ;
      return 1 ;
   }
   boost::filesystem::path p( argv[ 1 ] ) ;
   if ( boost::filesystem::exists( p ) ) {
      std::time_t t = boost::filesystem::last_write_time( p ) ;
      std::cout << "On " << std::ctime( &t ) << " the file " << argv[ 1 ]
	 << " was modified the last time!\n" ;
      std::cout << "Setting the modification time to now:\n" ;
      std::time_t n = std::time( 0 ) ;
      boost::filesystem::last_write_time( p , n ) ;
      t = boost::filesystem::last_write_time( p ) ;
      std::cout << "Now the modification time is " << std::ctime( &t ) << std::endl ;
      return 0 ;
   } else {
      std::cout << "Could not find file " << argv[ 1 ] << '\n' ;
      return 2 ;
   }
}
```


## C#


```c#
using System;
using System.IO;

Console.WriteLine(File.GetLastWriteTime("file.txt"));
File.SetLastWriteTime("file.txt", DateTime.Now);
```



## Clojure


```lisp
(import '(java.io File)
        '(java.util Date))

(Date. (.lastModified (File. "output.txt")))
(Date. (.lastModified (File. "docs")))

(.setLastModified (File. "output.txt")
                  (.lastModified (File. "docs")))
```



## Common Lisp


Common Lisp doesn't provide a way to set the modification time.
The write date can be accessed, however, using [http://www.lispworks.com/documentation/HyperSpec/Body/f_file_w.htm file-write-date]:


```lisp
(file-write-date "input.txt")
```


Implementations may, in addition, provide other ways of accessing these file attributes, e.g., POSIX bindings:

{{works with|SBCL}} (1.0.30, OS X, Intel), {{trans|C}}

(<code>sb-posix:utime</code> takes an access time and a modification time rather than an array of two times.)


```lisp
(let* ((filename "input.txt")
       (stat (sb-posix:stat filename))
       (mtime (sb-posix:stat-mtime stat)))
  (sb-posix:utime filename
		  (sb-posix:stat-atime stat)
		  (sb-posix:time)))
```



## D


```d
import std.stdio;
import std.file: getTimes, setTimes, SysTime;

void main() {
    auto fname = "unixdict.txt";
    SysTime fileAccessTime, fileModificationTime;
    getTimes(fname, fileAccessTime, fileModificationTime);
    writeln(fileAccessTime, "\n", fileModificationTime);
    setTimes(fname, fileAccessTime, fileModificationTime);
}
```

For Windows systems there is also getTimesWin(),
that gives the file creation time too.


## Delphi


```Delphi
function GetModifiedDate(const aFilename: string): TDateTime;
var
  hFile: Integer;
  iDosTime: Integer;
begin
  hFile := FileOpen(aFilename, fmOpenRead);
  iDosTime := FileGetDate(hFile);
  FileClose(hFile);
  if (hFile = -1) or (iDosTime = -1) then raise Exception.Create('Cannot read file: ' + sFilename);
  Result := FileDateToDateTime(iDosTime);
end;

procedure ChangeModifiedDate(const aFilename: string; aDateTime: TDateTime);
begin
  FileSetDate(aFileName, DateTimeToFileDate(aDateTime));
end;
```



## E


{{works with|E-on-Java}}

{{trans|Java}}

E follows the Java File interface, except that it replaces boolean success/failure returns with an ejector parameter, so that the default behavior if the client does not handle the case is not to continue ignoring the failure.


```e
def strdate(date) {
  return E.toString(<unsafe:java.util.makeDate>(date))
}

def test(type, file) {
    def t := file.lastModified()
    println(`The following $type called ${file.getPath()} ${
            if (t == 0) { "does not exist." } else { `was modified at ${strdate(t)}` }}`)
    println(`The following $type called ${file.getPath()} ${
            escape ne { file.setLastModified(timer.now(), ne); "was modified to current time." } catch _ { "does not exist." }}`)
    println(`The following $type called ${file.getPath()} ${
            escape ne { file.setLastModified(t, ne); "was modified to previous time." } catch _ { "does not exist." }}`)
}

test("file", <file:output.txt>)
test("directory", <file:docs>)
```



## Emacs Lisp


```Lisp
(nth 5 (file-attributes "input.txt")) ;; mod date+time

(set-file-times "input.txt") ;; to current-time
(set-file-times "input.txt"
                (encode-time 0 0 0  1 1 2014)) ;; to given date+time
```


<code>set-file-times</code> sets both the access time and modification time of the file.  Time values are in the usual Emacs list form.
Emacs file name handler magic applies to both <code>file-attributes</code> and <code>set-file-times</code> so they can act on "remote" files too.

File visiting buffers record the modification time of the file so as to guard against changes by another program.  <code>set-file-times</code> from within Emacs doesn't update those buffer times and so looks like an external change.


## Elixir


```txt

iex(1)> info = File.stat!("input.txt")
%File.Stat{access: :read_write, atime: {{2015, 10, 29}, {20, 44, 28}},
 ctime: {{2015, 9, 20}, {9, 5, 58}}, gid: 0, inode: 0, links: 1,
 major_device: 3, minor_device: 0, mode: 33206,
 mtime: {{2015, 10, 29}, {20, 44, 28}}, size: 45, type: :regular, uid: 0}
iex(2)> info.mtime
{{2015, 10, 29}, {20, 44, 28}}
iex(3)> File.touch!("input.txt")
:ok
iex(4)> File.stat!("input.txt")
%File.Stat{access: :read_write, atime: {{2016, 3, 7}, {23, 12, 35}},
 ctime: {{2016, 3, 7}, {23, 12, 35}}, gid: 0, inode: 0, links: 1,
 major_device: 3, minor_device: 0, mode: 33206,
 mtime: {{2016, 3, 7}, {23, 12, 35}}, size: 45, type: :regular, uid: 0}
```


```



## Erlang



```Erlang

-module( file_modification_time ).

-include_lib("kernel/include/file.hrl").

-export( [task/0] ).

task() ->
       File = "input.txt",
       {ok, File_info} = file:read_file_info( File ),
       io:fwrite( "Modification time ~p~n", [File_info#file_info.mtime] ),
       ok = file:write_file_info( File, File_info#file_info{mtime=calendar:local_time()} ).

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.IO

[<EntryPoint>]
let main args =
    Console.WriteLine(File.GetLastWriteTime(args.[0]))
    File.SetLastWriteTime(args.[0], DateTime.Now)
    0
```



## Factor


```factor
"foo.txt" file-info modified>> .
```

Setting the modified time is not cross-platform, so here's a Unix version.

```factor
USE: io.files.info.unix

"foo.txt" now 2 hours time+ set-file-modified-time
```



## Fortran

No facility. The INQUIRE statement for files has no keywords to ascertain the disc file's creation time, last access time, last modification time, and much else also in a standardised way so there is no way to modify these items either. A particular installation may offer library routines (written in assembler perhaps), or a modified compiler with extra features, but in general, there is no facility - other than performing a file action at the desired time.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' This example is taken directly from the FB documentation (see [http://www.freebasic.net/wiki/wikka.php?wakka=KeyPgFiledatetime])

#include "vbcompat.bi"  '' to use Format function

Dim filename As String, d As Double

Print "Enter a filename: "
Line Input filename

If FileExists(filename) Then
  Print "File last modified: ";
  d = FileDateTime( filename )
  Print Format( d, "yyyy-mm-dd hh:mm AM/PM" )
Else
  Print "File not found"
End If

Sleep
```


Sample input/output:

{{out}}

```txt

Enter a filename:
c:\windows\system32\kernel32.dll
File last modified: 2016-09-07 05:39 AM

```



## Gambas


```gambas
' There is no built in command in Gambas to 'set' the modification time of a file
' A shell call to 'touch' would do it

Public Sub Main()
Dim stInfo As Stat = Stat(User.home &/ "Rosetta.txt")

Print "Rosetta.txt was last modified " & Format(stInfo.LastModified, "dd/mm/yyy hh:nn:ss")

End
```

Output:

```txt

Rosetta.txt was last modified 28/05/2017 16:39:39

```



## Go


```go
package main

import (
    "fmt"
    "os"
    "syscall"
    "time"
)

var filename = "input.txt"

func main() {
    foo, err := os.Stat(filename)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("mod time was:", foo.ModTime())
    mtime := time.Now()
    atime := mtime // a default, because os.Chtimes has an atime parameter.
    // but see if there's a real atime that we can preserve.
    if ss, ok := foo.Sys().(*syscall.Stat_t); ok {
        atime = time.Unix(ss.Atim.Sec, ss.Atim.Nsec)
    }
    os.Chtimes(filename, atime, mtime)
    fmt.Println("mod time now:", mtime)
}
```



## GUISS


In Graphical User Interface Support Script, we can can only use facilities that the underlying user interface provides, so we can display file timestamps, but cannot alter them.
In this example, we get the date and timestamp for the file Foobar.txt.


```guiss
Start,My Documents,Rightclick:Icon:Foobar.txt,Properties
```



## Haskell



```haskell
import System.Posix.Files
import System.Posix.Time

do status <- getFileStatus filename
   let atime = accessTime status
       mtime = modificationTime status -- seconds since the epoch
   curTime <- epochTime
   setFileTimes filename atime curTime -- keep atime unchanged
                                       -- set mtime to current time
```


Alternative (only gets modification time):

```haskell
import System.Directory
import System.Time

do ct <- getModificationTime filename
   cal <- toCalendarTime ct
   putStrLn (calendarTimeToString cal)
```



## HicEst


```hicest
CHARACTER timestamp*18

timestamp = ' ' ! blank timestamp will read:
SYSTEM(FIle="File_modification_time.hic", FileTime=timestamp) ! 20100320141940.525

timestamp = '19991231235950' ! set timestamp to Millenium - 10 seconds
SYSTEM(FIle="File_modification_time.hic", FileTime=timestamp)
```


=={{header|Icon}} and {{header|Unicon}}==
Icon doesn't support 'stat' or 'utime'; however,
information can be obtained by use of the system function to access command line.

```Unicon

every dir := !["./","/"] do {
   if i := stat(f := dir || "input.txt") then {
      write("info for ",f ," mtime= ",ctime(i.mtime),", atime=",ctime(i.ctime), ", atime=",ctime(i.atime))
      utime(f,i.atime,i.mtime-1024)
      i := stat(f)
      write("update for ",f ," mtime= ",ctime(i.mtime),", atime=",ctime(i.ctime), ", atime=",ctime(i.atime))
      }
   else stop("failure to stat ",f)
   }
```

Notes:
* Icon and Unicon accept both / and \ for directory separators.
* 'ctime' formats an integer representing an epoch time into human readable form
* 'utime' updates the atime and mtime values


## J

The standard file access library only supports reading the file modification time.

```j
   load 'files'
   fstamp 'input.txt'
2009 8 24 20 34 30
```

It is possible to set the time but it has to be done through OS specific external calls.


## Java


```java
import java.io.File;
import java.util.Date;
public class FileModificationTimeTest {
   public static void test(String type, File file) {
       long t = file.lastModified();
       System.out.println("The following " + type + " called " + file.getPath() +
            (t == 0 ? " does not exist." : " was modified at " + new Date(t).toString() )
       );
       System.out.println("The following " + type + " called " + file.getPath() +
            (!file.setLastModified(System.currentTimeMillis()) ? " does not exist." : " was modified to current time." )
       );
       System.out.println("The following " + type + " called " + file.getPath() +
            (!file.setLastModified(t) ? " does not exist." : " was modified to previous time." )
       );
   }
   public static void main(String args[]) {
       test("file", new File("output.txt"));
       test("directory", new File("docs"));
   }
}
```



## JavaScript

{{works with|JScript}}
Get only.

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");
var f = fso.GetFile('input.txt');
var mtime = f.DateLastModified;
```

The following works in all browsers, including IE10.

```javascript
var file = document.getElementById("fileInput").files.item(0);
var last_modified = file.lastModifiedDate;
```



## Jsish

Getting the mod time is a builtin.  Setting the time is currently not, so ''touch'' is used here.

A C extension could provide access to the ''utime'' system call if desired.


```javascript
/* File modification times, in Jsi */

var fileTime = File.mtime('fileModificationTime.jsi');
puts("Last mod time was: ", strftime(fileTime * 1000));

exec('touch fileModificationTime.jsi');

fileTime = File.mtime('fileModificationTime.jsi');
puts("Mod time now: ", strftime(fileTime * 1000));
```


{{out}}

```txt
prompt$ jsish fileModificationTime.jsi
Last mod time was:  2019-05-08 13:21:10
Mod time now:  2019-05-08 13:23:35
```



## Julia


```julia
using Dates

fname, _ = mktemp()

println("The modification time of $fname is ", Dates.unix2datetime(mtime(fname)))
println("\nTouch this file.")
touch(fname)
println("The modification time of $fname is now ", Dates.unix2datetime(mtime(fname)))
```



## Kotlin


```scala
// version 1.0.6

import java.io.File

fun main(args: Array<String>) {
    val filePath = "input.txt" // or whatever
    val file = File(filePath)
    with (file) {
        println("%tc".format(lastModified()))
        // update to current time, say
        setLastModified(System.currentTimeMillis())
        println("%tc".format(lastModified()))
    }
}
```



## Lasso

Note this is retrieve only.

File modification date is updated on save of file.

```Lasso
local(f) = file('input.txt')
handle => { #f->close }
#f->modificationDate->format('%-D %r')
// result: 12/2/2010 11:04:15 PM
```



## Lua


```lua
require "lfs"
local attributes = lfs.attributes("input.txt")
if attributes then
    print(path .. " was last modified " .. os.date("%c", attributes.modification) .. ".")

    -- set access and modification time to now ...
    lfs.touch("input.txt")

    -- ... or set modification time to now, keep original access time
    lfs.touch("input.txt", attributes.access, os.time())
else
    print(path .. " does not exist.")
end
```


## M2000 Interpreter

Most of the file statements/functions use current directory as path.


```M2000 Interpreter

Module CheckIt {
      \\ without *for wide output*  we open for ANSI (1 byte per character)
      \\ but here we need it only for the creation of a file
      Open "afile" for output as #f
      Close #f
      Print file.stamp("afile")   'it is a number in VB6 date format.
      \\ day format as for Greece
      Print Str$(File.Stamp("afile"),"hh:nn:ss dd/mm/yyyy") , "utc write time - by default"
      Print Str$(File.Stamp("afile" ,1),"hh:nn:ss dd/mm/yyyy") , "utc write time, 1"
      Print  Str$(File.Stamp("afile" ,-1),"hh:nn:ss dd/mm/yyyy"), "local write time, -1"
      Print Str$(File.Stamp("afile" ,2),"hh:nn:ss dd/mm/yyyy"), "utc creation time, 2"
      Print  Str$(File.Stamp("afile" ,-2),"hh:nn:ss dd/mm/yyyy"), "local creation time, -2"
}
Checkit

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Get file modification time:

```Mathematica
 FileDate["file","Modification"]
```

results is returned in format: {year,month,day,hour,minute,second}. Setting file modification time:

```Mathematica
 SetFileDate["file",date,"Modification"]
```

where date is specified as {year,month,day,hour,minute,second}.

=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
   f = dir('output.txt');  % struct f contains file information
   f.date     % is string containing modification time
   f.datenum  % numerical format (number of days)
   datestr(f.datenum)   % is the same as f.date
   % see also: stat, lstat
```


Modifying of file access time can be done through system calls:


```Matlab
  system('touch -t 201002032359.59 output.txt');
```



## MAXScript

MAXScript has no method to set the mod time

```maxscript
-- Returns a string containing the mod date for the file, e.g. "1/29/99 1:52:05 PM"
getFileModDate "C:\myFile.txt"
```


=={{header|Modula-3}}==

```modula3
MODULE ModTime EXPORTS Main;

IMPORT IO, Fmt, File, FS, Date, OSError;

TYPE dateArray = ARRAY [0..5] OF TEXT;

VAR
  file: File.Status;
  date: Date.T;

PROCEDURE DateArray(date: Date.T): dateArray =
  BEGIN
    RETURN
      dateArray{Fmt.Int(date.year), Fmt.Int(ORD(date.month) + 1), Fmt.Int(date.day),
                Fmt.Int(date.hour), Fmt.Int(date.minute), Fmt.Int(date.second)};
  END DateArray;

BEGIN
  TRY
    file := FS.Status("test.txt");
    date := Date.FromTime(file.modificationTime);
    IO.Put(Fmt.FN("%s-%02s-%02s %02s:%02s:%02s", DateArray(date)));
    IO.Put("\n");
  EXCEPT
  | OSError.E => IO.Put("Error: Failed to get file status.\n");
  END;
END ModTime.
```

{{Out}}

```txt

2011-07-14 14:12:46

```

This program sets the modification time to any value we wish:

```modula3
MODULE SetModTime EXPORTS Main;

IMPORT Date, FS;

<*FATAL ANY*>

VAR
  date: Date.T;

BEGIN
  (* Set the modification time to January 1st, 1999 *)
  date.year := 1999;
  date.month := Date.Month.Jan;
  date.day := 1;
  date.hour := 0;
  date.minute := 0;
  date.second := 0;
  date.offset := 21601;
  date.zone := "CST";

FS.SetModificationTime("test.txt", Date.ToTime(date));
END SetModTime.
```

We can see the output with the Unix command <tt>stat</tt>
{{Out}}

```txt

stat test.txt | grep Mod
Modify: 1999-01-01 00:00:01.000000000 -0600

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg fileName
  if fileName = '' then fileName = 'data/tempfile01'
  mfile = File(fileName)
  mtime = mfile.lastModified()
  dtime = Date(mtime).toString()
  say 'File' fileName 'last modified at' dtime
  say
  if mfile.setLastModified(0) then do
    dtime = Date(mfile.lastModified()).toString()
    say 'File modification time altered...'
    say 'File' fileName 'now last modified at' dtime
    say
    say 'Resetting...'
    mfile.setLastModified(mtime)
    dtime = Date(mfile.lastModified()).toString()
    say 'File' fileName 'reset to last modified at' dtime
    end
  else do
    say 'Unable to modify time for file' fileName
    end
  return

```

{{out}}

```txt

File data/tempfile01 last modified at Fri May 24 14:45:55 PDT 2013

File modification time altered...
File data/tempfile01 now last modified at Wed Dec 31 16:00:00 PST 1969

Resetting...
File data/tempfile01 reset to last modified at Fri May 24 14:45:55 PDT 2013

```



## NewLISP


```NewLISP
;; print modification time
(println (date (file-info "input.txt" 6)))

;; set modification time to now (Unix)
(! "touch -m input.txt")
```



## Nim


```nim
import os, times
let accTime = getLastAccessTime("filename")
let modTime = getLastModificationTime("filename")

import posix
var unixAccTime = Timeval(tv_sec: int(accTime))
var unixModTime = Timeval(tv_sec: int(modTime))

# Set the modification time
unixModTime.tv_sec = 0

var times = [unixAccTime, unixModTime]
discard utimes("filename", addr(times))

# Set the access and modification times to the current time
discard utimes("filename", nil)
```



## Objeck

Get only


```objeck
use System.IO.File;

class Program {
  function : Main(args : String[]) ~ Nil {
    File->ModifiedTime("file_mod.obs")->ToString()->PrintLine();
  }
}
```


=={{header|Objective-C}}==


```objc
NSFileManager *fm = [NSFileManager defaultManager];

// Pre-OS X 10.5
NSLog(@"%@", [[fm fileAttributesAtPath:@"input.txt" traverseLink:YES] fileModificationDate]);
[fm changeFileAttributes:[NSDictionary dictionaryWithObject:[NSDate date] forKey:NSFileModificationDate]
                  atPath:@"input.txt"];

// OS X 10.5+
NSLog(@"%@", [[fm attributesOfItemAtPath:@"input.txt" error:NULL] fileModificationDate]);
[fm setAttributes:[NSDictionary dictionaryWithObject:[NSDate date] forKey:NSFileModificationDate]
     ofItemAtPath:@"input.txt" error:NULL];
```



## OCaml


```ocaml
#load "unix.cma";;
open Unix;;
let mtime = (stat filename).st_mtime;; (* seconds since the epoch *)

utimes filename (stat filename).st_atime (time ());;
(* keep atime unchanged
   set mtime to current time *)
```



## Oforth


Get only :


```Oforth
File new("myfile.txt") modified
```



## OpenEdge/Progress

The file modified time can only be read.


```progress
FILE-INFO:FILE-NAME = 'c:/temp'.
MESSAGE
   STRING( FILE-INFO:FILE-MOD-TIME, 'HH:MM:SS' )
VIEW-AS ALERT-BOX
```



## Oz

Getting the modification time:

```oz
declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  Modified = {Path.mtime "input.txt"} %% posix time
in
  {Show {OsTime.localtime Modified}} %% human readable record
```


Setting the modification time is not possible,
unless implemented as an extension in C or C++.


## Pascal

See [[File_modification_time#Delphi | Delphi]]


## Perl

{{works with|Perl|5}}


```perl
my $mtime = (stat($file))[9]; # seconds since the epoch

# you should use the more legible version below:
use File::stat qw(stat);
my $mtime = stat($file)->mtime; # seconds since the epoch

utime(stat($file)->atime, time, $file);
# keep atime unchanged
# set mtime to current time
```



## Perl 6

{{Works with|rakudo|2018.03}}

```perl6
use NativeCall;

class utimbuf is repr('CStruct') {
    has int $.actime;
    has int $.modtime;

    submethod BUILD(:$atime, :$mtime) {
        $!actime = $atime;
        $!modtime = $mtime.to-posix[0].round;
    }
}

sub sysutime(Str, utimbuf --> int32) is native is symbol('utime') {*}

sub MAIN (Str $file) {
    my $mtime = $file.IO.modified orelse .die;

    my $ubuff = utimbuf.new(:atime(time),:mtime($mtime));

    sysutime($file, $ubuff);
}
```

Sets the last access time to now,
while restoring the modification time to what it was before.


## Phix


```Phix
constant filename = "test.txt"
?get_file_date(filename)

include timedate.e
?format_timedate(get_file_date(filename))
bool res = set_file_date(filename)
?format_timedate(get_file_date(filename))
```

{{out}}

```txt

{2019,5,2,14,46,40}
"2:46pm Thursday May 02nd, 2019"
"12:25pm Wednesday May 08nd, 2019"

```



## PHP


```php
<?php
$filename = 'input.txt';

$mtime = filemtime($filename); // seconds since the epoch

touch($filename,
      time(), // set mtime to current time
      fileatime($filename)); // keep atime unchanged
?>
```



## PicoLisp


```PicoLisp
(let File "test.file"
   (and
      (info File)
      (prinl (stamp (cadr @) (cddr @))) ) # Print date and time in UTC
   (call 'touch File) )                   # Set modification time to "now"
```

{{Out}}

```txt
2010-02-20 15:46:37
```



## Pop11


```pop11
;;; Print modification time (seconds since Epoch)
sysmodtime('file') =>
```



## PowerShell


```powershell

$modificationTime = (Get-ChildItem file.txt).LastWriteTime
Set-ItemProperty file.txt LastWriteTime (Get-Date)

$LastReadTime = (Get-ChildItem file.txt).LastAccessTime
Set-ItemProperty file.txt LastAccessTime(Get-Date)

$CreationTime = (Get-ChildItem file.txt).CreationTime
Set-ItemProperty file.txt CreationTime(Get-Date)

```


You can also use alternates to get UTC time:
LastWriteTimeUtc
LastAccessTimeUtc
CreationTimeUtc


## PureBasic


```PureBasic
Debug FormatDate("%yyyy/%mm/%dd", GetFileDate("file.txt",#PB_Date_Modified))
  SetFileDate("file.txt",#PB_Date_Modified,Date(1987, 10, 23, 06, 43, 15))
Debug FormatDate("%yyyy/%mm/%dd - %hh:%ii:%ss", GetFileDate("file.txt",#PB_Date_Modified))
```



## Python


```python
import os

#Get modification time:
modtime = os.path.getmtime('filename')

#Set the access and modification times:
os.utime('path', (actime, mtime))

#Set just the modification time:
os.utime('path', (os.path.getatime('path'), mtime))

#Set the access and modification times to the current time:
os.utime('path', None)
```



## R

See [http://tolstoy.newcastle.edu.au/R/e4/devel/08/02/0639.html this R-devel mailing list thread] for more information.

```r
# Get the value
file.info(filename)$mtime

#To set the value, we need to rely on shell commands.  The following works under windows.
shell("copy /b /v filename +,,>nul")
# and on Unix (untested)
shell("touch -m filename")
```



## Racket



```racket

#lang racket
(file-or-directory-modify-seconds "foo.rkt")

```



## RapidQ


```rapidq
name$ = DIR$("input.txt", 0)
PRINT "File date: "; FileRec.Date
PRINT "File time: "; FileRec.Time
```



## REALbasic

The ''Date'' object has properties which correspond to various date formats
such as SQLDateTime (YYYY-MM-DD HH:MM:SS), DayOfWeek, DayOfYear, and TotalSeconds since 12:00AM, January 1, 1904, among others.

```REALbasic

Function getModDate(f As FolderItem) As Date
  Return f.ModificationDate
End Function
```



## REXX

{{works with|Regina REXX}}
The REXX language has no facility to update a file's modification time.

```rexx
/*REXX program obtains and displays a  file's  time of modification.                    */
parse arg $ .                                    /*obtain required argument from the CL.*/
if $==''  then do;  say "***error*** no filename was specified.";   exit 13;   end
q=stream($, 'C', "QUERY TIMESTAMP")              /*get file's modification time info.   */
if q==''  then q="specified file doesn't exist." /*set an error indication message.     */
say 'For file: '  $                              /*display the file ID information.     */
say 'timestamp of last modification: ' q         /*display the modification time info.  */
                                                 /*stick a fork in it,  we're all done. */

```

'''output'''   when using the file ID for this particular REXX program (source file):   <tt> FILEMTIME.REX </tt>

```txt

For file:  FILEMTIM.REX
timestamp of last modification:  2015-05-30 14:22:26

```



## Ring


```ring

load "stdlib.ring"
see GetFileInfo( "test.ring" )

func GetFileInfo cFile
     cOutput = systemcmd("dir /T:W " + cFile )
     aList = str2list(cOutput)
     cLine = aList[6]
     aInfo = split(cLine," ")
     return aInfo

```

Output:

```txt

2017.
08.
27.
10:31
189
test.ring

```



## Ruby


```ruby
#Get modification time:
modtime = File.mtime('filename')

#Set the access and modification times:
File.utime(actime, mtime, 'path')

#Set just the modification time:
File.utime(File.atime('path'), mtime, 'path')

#Set the access and modification times to the current time:
File.utime(nil, nil, 'path')
```



## Run BASIC


```runbasic
files #f, DefaultDir$ + "\*.*"                  ' all files in the default directory

print "hasanswer: ";#f HASANSWER()              ' does it exist
print "rowcount: ";#f ROWCOUNT()                ' number of files in the directory
print                                           '
#f DATEFORMAT("mm/dd/yy")                       ' set format of file date to template given
#f TIMEFORMAT("hh:mm:ss")                       ' set format of file time to template given

for i = 1 to #f rowcount()                      ' loop through the files
if #f hasanswer() then                          '  or loop with while #f hasanswer()
 print "nextfile info: ";#f nextfile$("    ")   ' set the delimiter for nextfile info
 print "name: ";name$                           ' file name
 print "size: ";#f SIZE()                       ' file size
 print "date: ";#f DATE$()                      ' file date
 print "time: ";#f TIME$()                      ' file time
 print "isdir: ";#f ISDIR()                     ' is it a directory or file

 name$ = #f NAME$()
 shell$("touch -t 201002032359.59 ";name$;""")  ' shell to os to set date

 print
end if
next
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";
  include "time.s7i";

const proc: main is func
  local
    var time: modificationTime is time.value;
  begin
    modificationTime := getMTime("data.txt");
    setMTime("data.txt", modificationTime);
  end func;
```



## Sidef


```ruby
var file = File.new(__FILE__);
say file.stat.mtime;            # seconds since the epoch

# keep atime unchanged
# set mtime to current time
file.utime(file.stat.atime, Time.now);
```



## Scala

{{libheader|Scala}}

```Scala
import java.io.File
import java.util.Date

object FileModificationTime extends App {
  def test(file: File) {
    val (t, init) = (file.lastModified(),
      s"The following ${if (file.isDirectory()) "directory" else "file"} called ${file.getPath()}")

    println(init + (if (t == 0) " does not exist." else " was modified at " + new Date(t).toInstant()))
    println(init +
      (if (file.setLastModified(System.currentTimeMillis())) " was modified to current time." else " does not exist."))
    println(init +
      (if (file.setLastModified(t)) " was reset to previous time." else " does not exist."))
  }

  // main
  List(new File("output.txt"), new File("docs")).foreach(test)
}
```



## Slate

Modifying the timestamp value is not currently a built-in feature.
This code gets a raw value:


```slate
slate[1]> (File newNamed: 'LICENSE') fileInfo modificationTimestamp.
1240349799
```



## Smalltalk


```smalltalk
|a|
a := File name: 'input.txt'.
(a lastModifyTime) printNl.
```



## Standard ML


```sml
val mtime = OS.FileSys.modTime filename; (* returns a Time.time data structure *)

(* unfortunately it seems like you have to set modification & access times together *)
OS.FileSys.setTime (filename, NONE); (* sets modification & access time to now *)
(* equivalent to: *)
OS.FileSys.setTime (filename, SOME (Time.now ()))
```



## Tcl

Assuming that the variable <tt>filename</tt> holds the name of the file...

```tcl
# Get the modification time:
set timestamp [file mtime $filename]

# Set the modification time to ‘now’:
file mtime $filename [clock seconds]
```



## TUSCRIPT

TUSCRIPT does not allow to modify/set the timestamp of a file,
but it is able to read it:

```tuscript

$$ MODE TUSCRIPT
file="rosetta.txt"
ERROR/STOP OPEN (file,READ,-std-)
modified=MODIFIED (file)
PRINT "file ",file," last modified: ",modified

```

{{Out}}

```txt

file rosetta.txt last modified: 2011-12-14 23:50:48

```



## UNIX Shell


There are several ways to handle files' timestamps in most *nix systems.
For these examples, we'll assume you need to retrieve the timestamp to a variable.
These examples use [[wp:stat (Unix)|stat]] and [[wp:touch (Unix)|touch]] from the [[wp:GNU Core Utilities|GNU Core Utilities]], under [[bash]], but they ''should'' work with any [[POSIX]]-compliant implementation under any sh-compatible shell.

For all of these examples, <code>$F</code> is a variable holding the filename to examine, while <code>T</code> (and <code>$T</code>) is the timestamp.

To get the timestamp in seconds since the epoch:

```bash
T=`stat -c %Y $F`
```


To get the timestamp in human-readable format:

```bash
T=`stat -c %y $F`
```


Note that the only difference between the above examples is capital Y vs lower-case y.

In the following examples, the "-c" argument to touch tells it to ''not'' create any files that don't already exist.

To set file F to time T, where T is in a human-readable format:

```bash
# Note the quotation marks -- very important!
T="2000-01-01 01:02:03.040506070 -0800"
touch -c -d "$T" $F
```


To set file F to time T, where T is in the format [[CC]YY]MMDDhhmm[.ss] (the square brackets mark optional parts):

```bash
T=200102030405.06
touch -c -t $T $F
```


If the year is left out, the current year is used.
If the seconds are left out, 0 (zero) is used.
Leaving out the optional parts of the above results in this:

```bash
T=02030405
touch -c -t $T $F
```


If no time is specified, then the timestamp is set to the current time:

```bash
touch -c $F
```


There are, of course, other ways to do both tasks -- for example, one could use <code>ls -l --time-style=full-iso</code> and then process the resulting list with some other tool (e.g. [[awk]] or [[Perl]]).


## Ursa

{{works with|Cygnus/X Ursa}}

```ursa
decl java.util.Date d
decl file f

f.open "example.txt"
d.setTime (f.lastmodified)
out d endl console

f.setlastmodified 10
```



## VBScript

{{works with|Windows Script Host|*}}
VBScript provides no way to set the last modification time. It does allow you to retrieve it, however.

```VBScript

WScript.Echo CreateObject("Scripting.FileSystemObject").GetFile("input.txt").DateLastModified

```



## Vedit macro language

Display file's last modification time as number of seconds since midnight.

```vedit
Num_Type(File_Stamp_Time("input.txt"))
```


Displays file's last modification date and time as a string.

```vedit
File_Stamp_String(10, "input.txt")
Reg_Type(10)
```

Vedit Macro Language has <span class="plainlinks">[http://seoph1.cafe24.com/wordpress/ <span style="color:black;font-weight:normal; text-decoration:none!important; background:none!important; text-decoration:none;">Blog in SEO</span>] no method to set the modification time


## Visual Basic .NET

'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Dim file As New IO.FileInfo("test.txt")

'Creation Time
Dim createTime = file.CreationTime
file.CreationTime = createTime.AddHours(1)

'Write Time
Dim writeTime = file.LastWriteTime
file.LastWriteTime = writeTime.AddHours(1)

'Access Time
Dim accessTime = file.LastAccessTime
file.LastAccessTime = accessTime.AddHours(1)
```



## zkl


```zkl
info:=File.info("input.txt");
  // -->T(size,last status change time,last mod time,isDir,mode), from stat(2)
info.println();
Time.Date.ctime(info[2]).println();
File.setModTime("input.txt",Time.Clock.mktime(2014,2,1,0,0,0));
File.info("input.txt")[2] : Time.Date.ctime(_).println();
```

{{Out}}

```txt

L(391,1394910932,1394910932,False,33156)
Sat Mar 15 12:15:32 2014
Sat Feb 1 00:00:00 2014

```

