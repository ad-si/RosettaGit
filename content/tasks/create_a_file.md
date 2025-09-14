+++
title = "Create a file"
description = ""
date = 2019-06-29T22:06:21Z
aliases = []
[extra]
id = 2027
[taxonomies]
categories = ["task", "File System Operations"]
tags = []
languages = [
  "11l",
  "4dos_batch",
  "ada",
  "aikido",
  "aime",
  "algol_68",
  "apl",
  "applescript",
  "arm_assembly",
  "autohotkey",
  "awk",
  "axe",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "chuck",
  "clojure",
  "cobol",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "delphi",
  "e",
  "echolisp",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fancy",
  "forth",
  "fortran",
  "freebasic",
  "friendly_interactive_shell",
  "funl",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "i",
  "j",
  "java",
  "jcl",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lasso",
  "lfe",
  "liberty_basic",
  "lingo",
  "little",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "maxscript",
  "mercury",
  "mirah",
  "nanoquery",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "powershell",
  "prodos",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "sql_pl",
  "sqlite",
  "standard_ml",
  "stata",
  "tcl",
  "toka",
  "tuscript",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic_.net",
  "visual_objects",
  "x86_assembly",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

{{task|File System Operations}} [[Category:Simple]]

In this task, the job is to create a new empty file called "output.txt" of size 0 bytes
and an empty directory called "docs". This should be done twice: once "here", i.e. in the current working directory and once in the filesystem root.


## 11l

```11l
L(directory) [‘/’, ‘./’]
   File(directory‘output.txt’, ‘w’) // create /output.txt, then ./output.txt
   fs:create_dir(directory‘docs’)   // create directory /docs, then ./docs
```



## 4DOS Batch


```4dos>echos
 output.txt
mkdir docs

echos > \output.txt
mkdir \docs
```



## Ada

Notes:
* Use Streams_IO to write 0 bytes.  File creation with Ada.Text_IO does not create 0 byte files (it inserts EOL/EOF).

* The forward slash (/) notation works in Windows XP as well as Unix/Linux.

```ada
with Ada.Streams.Stream_IO, Ada.Directories;
use  Ada.Streams.Stream_IO, Ada.Directories;

procedure File_Creation is

   File_Handle : File_Type;

begin

   Create (File_Handle, Out_File, "output.txt");
   Close (File_Handle);
   Create_Directory("docs");
   Create (File_Handle, Out_File, "/output.txt");
   Close (File_Handle);
   Create_Directory("/docs");

end File_Creation;
```



## Aikido


```aikido

var sout = openout ("output.txt") // in current dir
sout.close()

var sout1 = openout ("/output.txt")   // in root dir
sout1.close()

mkdir ("docs")
mkdir ("/docs")


```



## Aime


```aime
# Make a directory using the -mkdir- program
void
mkdir(text p)
{
    sshell ss;

    b_cast(ss_path(ss), "mkdir");

    l_append(ss_argv(ss), "mkdir");
    l_append(ss_argv(ss), p);

    ss_link(ss);
}

void
create_file(text p)
{
    file f;

    f_open(f, p, OPEN_CREATE | OPEN_TRUNCATE | OPEN_WRITEONLY, 00644);
}

void
create_pair(text prefix)
{
    create_file(cat(prefix, "output.txt"));
    mkdir(cat(prefix, "docs"));
}

integer
main(void)
{
    create_pair("");
    create_pair("/");

    return 0;
}
```



## ALGOL 68

Note: file names are Operating System dependent.
* [[ALGOL 68G]] does not support pages, and ''"set"'' procedure only has 2 arguments.
* [[ELLA ALGOL 68]] also encounters problems with ''"set"'' page on [[linux]].

It may be best to to use an operating system provided library.

```algol68
main:(

  INT errno;

  PROC touch = (STRING file name)INT:
  BEGIN
    FILE actual file;
    INT errno := open(actual file, file name, stand out channel);
    IF errno NE 0 THEN GO TO stop touch FI;
    close(actual file); # detach the book and keep it #
    errno
  EXIT
  stop touch:
      errno
  END;

  errno := touch("input.txt");
  errno := touch("/input.txt");

  # ALGOL 68 has no concept of directories,
    however a file can have multiple pages,
    the pages are identified by page number only #

  PROC mkpage = (STRING file name, INT page x)INT:
  BEGIN
    FILE actual file;
    INT errno := open(actual file, file name, stand out channel);
    IF errno NE 0 THEN GO TO stop mkpage FI;
    set(actual file,page x,1,1); # skip to page x, line 1, character 1 #
    close(actual file); # detach the new page and keep it #
    errno
  EXIT
  stop mkpage:
      errno
  END;

  errno := mkpage("input.txt",2);
)
```



## APL


```APL
      'output.txt' ⎕ncreate ¯1+⌊/0,⎕nnums
      '\output.txt' ⎕ncreate ¯1+⌊/0,⎕nnums
      ⎕mkdir 'Docs'
      ⎕mkdir '\Docs'
```



## AppleScript

AppleScript itself has limited built-in File System access, but folders (directories) can be created by controlling the Mac OS Finder, and files can be created and accessed using the Standard Additions (osax) scripting addition included with AppleScript. Also, the Finder has no concept of the working directory (as it is a GUI). You can however target the frontmost Finder window that is open.

Create a zero-byte text file on the startup disk (root directory). Note: the <code>close</code> command is a memory allocation housekeeping command that should be performed once file access is complete.

```AppleScript
close (open for access "output.txt")
```

Create a new folder (directory) on the startup disk (root directory).

```AppleScript
tell application "Finder" to make new folder at startup disk with properties {name:"docs"}
```

Create a zero-byte text file in the frontmost (open) Finder window.

```AppleScript
tell application "Finder" to set wd to target of window 1 as string
close (open for access wd & "output.txt")
```

Create a new folder (directory) in the frontmost (open) Finder window.

```AppleScript
tell application "Finder" to make new folder at window 1 with properties {name:"docs"}
```

--[[User:Apl.way|Apl.way]] 21:20, 9 June 2010 (UTC)


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program createDirFic.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall   exit Program
.equ WRITE,  4     @ Linux syscall  write FILE
.equ MKDIR, 0x27  @ Linux Syscal  create directory
.equ CHGDIR, 0xC  @ Linux Syscal  change directory
.equ CREATE, 0x8 @ Linux Syscal  create file
.equ CLOSE,   0x6  @ Linux Syscal  close file
/* Initialized data */
.data
szMessCreateDirOk: .asciz "Create directory Ok.\n"
szMessErrCreateDir: .asciz "Unable create directory. \n"
szMessErrChangeDir: .asciz "Unable change directory. \n"
szMessCreateFileOk: .asciz "Create file Ok.\n"
szMessErrCreateFile: .asciz "Unable create file. \n"
szMessErrCloseFile: .asciz "Unable close file. \n"

szNameDir: .asciz "Dir1"
szNameFile:  .asciz "file1.txt"


/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                @ entry of program
    push {fp,lr}        @ saves registers
	@ create directory
    ldr r0,iAdrszNameDir   @ directory name
    mov r1,#0775                 @ mode (in octal zero is important !!)
    mov r7, #MKDIR             @ code call system create directory
    swi #0                      @ call systeme
    cmp r0,#0             @ error ?
    bne 99f

   @ display  message ok directory
    ldr r0,iAdrszMessCreateDirOk
    bl affichageMess
    @ change directory
    ldr r0,iAdrszNameDir   @ directory name
    mov r7, #CHGDIR             @ code call system change directory
    swi #0                      @ call systeme
    cmp r0,#0     @ error ?
    bne 98f
    @ create file
    ldr r0,iAdrszNameFile   @ directory name
    mov r1,#0755                 @ mode (in octal zero is important !!)
    mov r2,#0
    mov r7,#CREATE             @ code call system create file
    swi #0                      @ call systeme
    cmp r0,#0             @ error ?
    ble 97f
    mov r8,r0     @ save File Descriptor
    @ display  message ok file
    ldr r0,iAdrszMessCreateFileOk
    bl affichageMess

    @ close file
    mov r0,r8       @ Fd
    mov r7, #CLOSE @ close file
    swi 0
    cmp r0,#0
    bne 96f
    @ end Ok
    b 100f
96:
    @ display error message close file
    ldr r0,iAdrszMessErrCloseFile
    bl affichageMess
    b 100f
97:
    @ display error message create file
    ldr r0,iAdrszMessErrCreateFile
    bl affichageMess
    b 100f
98:
    @ display error message change directory
    ldr r0,iAdrszMessErrChangeDir
    bl affichageMess
    b 100f
99:
    @ display error message create directory
    ldr r0,iAdrszMessErrCreateDir
    bl affichageMess
    b 100f
100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszMessCreateDirOk:		.int szMessCreateDirOk
iAdrszMessErrCreateDir:	.int szMessErrCreateDir
iAdrszMessErrChangeDir:	.int szMessErrChangeDir
iAdrszMessCreateFileOk:	.int szMessCreateFileOk
iAdrszNameFile:				.int szNameFile
iAdrszMessErrCreateFile:	.int szMessErrCreateFile
iAdrszMessErrCloseFile:	.int szMessErrCloseFile

iAdrszNameDir:				.int szNameDir
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registers */
    mov r2,#0   				/* counter length */
1:      	/* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system "write" */
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */



```



## AutoHotkey


```AutoHotkey
FileAppend,,output.txt
FileCreateDir, docs
FileAppend,,c:\output.txt
FileCreateDir, c:\docs
```



## AWK


```awk
BEGIN {
  printf "" > "output.txt"
  close("output.txt")
  printf "" > "/output.txt"
  close("/output.txt")
  system("mkdir docs")
  system("mkdir /docs")
}
```



## Axe

Since the TI-OS does not have a true filesystem, this task is emulated using an application variable instead of a file.

```axe
GetCalc("appvOUTPUT",0)
```



## BASIC


```qbasic
OPEN "output.txt" FOR OUTPUT AS 1
CLOSE
OPEN "\output.txt" FOR OUTPUT AS 1
CLOSE
```


=
## BaCon
=

```qbasic
' Create file and dir
TRAP LOCAL

OPEN "output.txt" FOR WRITING AS afile
CLOSE FILE afile

CATCH GOTO report
OPEN "/output.txt" FOR WRITING AS afile
CLOSE FILE afile

LABEL trydir
MAKEDIR "docs"

CATCH GOTO report2
MAKEDIR "/docs"
END

LABEL report
    PRINT ERR$(ERROR)
    GOTO trydir

LABEL report2
    PRINT ERR$(ERROR)
```


```txt

prompt$ ./creates
Error opening file: Permission denied
Unable to create directory: Permission denied
prompt$ ls -gocart
...
-rw-rw-r-- 1     324 May  2 23:53 creates.bac
-rwxrwxr-x 1   27184 May  2 23:53 creates
-rw-rw-r-- 1       0 May  2 23:53 output.txt
drwxr-xr-x 2    4096 May  2 23:53 docs
drwxrwxr-x 7   12288 May  2 23:53 .
```



## Batch File


```dos
copy nul output.txt
copy nul \output.txt
```



```dos
md docs
md \docs
```



## BBC BASIC

```bbcbasic
      CLOSE #OPENOUT("output.txt")
      CLOSE #OPENOUT("\output.txt")
      *MKDIR docs
      *MKDIR \docs
```



## Bracmat


```bracmat
put$(,"output.txt",NEW)
```

Or

```bracmat
fil$("output.txt",w)
```

In the latter case the file is still open, so unless the file is implicitly flushed and closed by ending the Bracmat program, you would want to close it explicitly:

```bracmat
fil$(,SET,-1)
```

To create a directory we are dependent on the underlying OS. In DOS:

```bracmat
sys$"mkdir docs"
```

And in the file system root:

```bracmat
sys$"mkdir \\docs"
```



## C

ISO C (directory creation not supported):

```c
#include <stdio.h>

int main() {
  FILE *fh = fopen("output.txt", "w");
  fclose(fh);

  return 0;
}
```


POSIX:

```c
#include <sys/stat.h>

#include <unistd.h>
#include <fcntl.h>

int main() { /* permissions are before umask */
  int fd = open("output.txt", O_WRONLY|O_CREAT|O_TRUNC, 0640); /* rights 0640 for rw-r----- */
  /* or equivalently:
     int fd = creat("output.txt", 0640); */ /* rights 0640 for rw-r----- */
  close(fd);

  mkdir("docs", 0750); /* rights 0750 for rwxr-x--- */

  return 0;
}
```


(for creation in the filesystem root, replace the filenames by "/output.txt" and "/docs")


## ChucK

This creates a file in root:

```c

FileIO text;
text.open("output.txt", FileIO.WRITE);

```



## C++

Uses some Microsoft library:

```cpp
#include <fstream>
#include <direct.h>

int main() {
	std::fstream f( "output.txt", std::ios::out );
	f.close();
	f.open( "/output.txt", std::ios::out );
	f.close();

	_mkdir( "docs" );
	_mkdir( "/docs" );

	return 0;
}
```


## C#


```c#
using System;
using System.IO;

class Program {
    static void Main(string[] args) {
        File.Create("output.txt");
        File.Create(@"\output.txt");

        Directory.CreateDirectory("docs");
        Directory.CreateDirectory(@"\docs");
    }
}
```



## Clojure


```lisp
(import '(java.io File))
(.createNewFile (new File "output.txt"))
(.mkdir (new File "docs"))
(.createNewFile (File. (str (File/separator) "output.txt")))
(.mkdir (File. (str (File/separator) "docs")))
```



## COBOL

{{works with|GnuCOBOL}} and other compilers with the system call extensions

```COBOL
       identification division.
       program-id. create-a-file.

       data division.
       working-storage section.
       01 skip                 pic 9 value 2.
       01 file-name.
          05 value "/output.txt".
       01 dir-name.
          05 value "/docs".
       01 file-handle          usage binary-long.

       procedure division.
       files-main.

      *> create in current working directory
       perform create-file-and-dir

      *> create in root of file system, will fail without privilege
       move 1 to skip
       perform create-file-and-dir

       goback.

       create-file-and-dir.
      *> create file in current working dir, for read/write
       call "CBL_CREATE_FILE" using file-name(skip:) 3 0 0 file-handle
       if return-code not equal 0 then
           display "error: CBL_CREATE_FILE " file-name(skip:) ": "
                   file-handle ", " return-code upon syserr
       end-if

      *> create dir below current working dir, owner/group read/write
       call "CBL_CREATE_DIR" using dir-name(skip:)
       if return-code not equal 0 then
           display "error: CBL_CREATE_DIR " dir-name(skip:) ": "
                   return-code upon syserr
       end-if
       .

       end program create-a-file.
```

```txt

prompt$ cobc -xj create-a-file.cob
error: CBL_CREATE_FILE /output.txt: -0000000001, +000000035
error: CBL_CREATE_DIR /docs: +000000128
```

Errors due to running sample without root permissions.

```txt

prompt$ ls -larct
...
-rw-rw-r--  1 rosetta rosetta   1279 Jun  1 08:14 create-a-file.cob
-rwxrwxr-x  1 rosetta rosetta  13896 Jun  1 08:17 create-a-file
-rw-rw-r--  1 rosetta rosetta      0 Jun  1 08:17 output.txt
drwxrwx---  2 rosetta rosetta   4096 Jun  1 08:17 docs
drwxrwxr-x  5 rosetta rosetta  12288 Jun  1 08:17 .

```



## Common Lisp

Lisp provides open and close commands for I/O with files
```lisp
(let ((stream (open "output.txt" :direction :output)))
  (close stream))
```

but it is more common to use ''with-open-file'' which has better exception handling.

```lisp
(with-open-file (stream "output.txt" :direction :output)
    ;; use the stream here
 )
```

As lisp is capable of being run on many different platforms and no assumptions should be made about the filesystem there are functions to construct paths in a platform independent manner

```lisp
(let ((paths (list (make-pathname :directory '(:relative "docs"))
                     (make-pathname :directory '(:absolute "docs")))))
  (mapcar #'ensure-directories-exist paths))
```

So creating a file called ''output.txt'' with an absolute path in the root directory becomes:

```lisp
(with-open-file
    (stream
        (make-pathname :directory '(:absolute "") :name "output.txt")
        :direction :output))
```


On the other hand, if you may depend on the platform's pathname syntax then shorter notation may be used:

```lisp
(mapcar #'ensure-directories-exist '(#p"docs/" #p"/docs/")))
```



## Component Pascal

```oberon2

MODULE CreateFile;
IMPORT Files, StdLog;

PROCEDURE Do*;
VAR
	f: Files.File;
	res: INTEGER;
BEGIN
	f := Files.dir.New(Files.dir.This("docs"),Files.dontAsk);
	f.Register("output","txt",TRUE,res);
	f.Close();

	f := Files.dir.New(Files.dir.This("C:\AEAT\docs"),Files.dontAsk);
	f.Register("output","txt",TRUE,res);
	f.Close()
END Do;

END CreateFile.

```


## D

For file creation, std.file.write function & std.stream.file class are used.

For dir creation, std.file.mkdir is used.

```d
module fileio ;
import std.stdio ;
import std.path ;
import std.file ;
import std.stream ;

string[] genName(string name){
  string cwd  = curdir ~ sep ; // on current directory
  string root = sep ;          // on root
  name = std.path.getBaseName(name) ;
  return [cwd ~ name, root ~ name] ;
}
void Remove(string target){
  if(exists(target)){
    if (isfile(target))
      std.file.remove(target);
    else
      std.file.rmdir(target) ;
  }
}
void testCreate(string filename, string dirname){
  // files:
  foreach(fn ; genName(filename))
    try{
      writefln("file to be created : %s", fn) ;
      std.file.write(fn, cast(void[])null) ;
      writefln("\tsuccess by std.file.write") ; Remove(fn) ;
      (new std.stream.File(fn, FileMode.OutNew)).close() ;
      writefln("\tsuccess by std.stream") ; Remove(fn) ;
    } catch(Exception e) {
      writefln(e.msg) ;
    }
  // dirs:
  foreach(dn ; genName(dirname))
    try{
      writefln("dir to be created : %s", dn) ;
      std.file.mkdir(dn) ;
      writefln("\tsuccess by std.file.mkdir") ; Remove(dn) ;
    } catch(Exception e) {
      writefln(e.msg) ;
    }
}
void main(){
  writefln("== test: File & Dir Creation ==") ;
  testCreate("output.txt", "docs") ;
}
```



## Delphi


These functions illustrate two methods for creating text files in Delphi: standard text file I/O and filestreams.


```Delphi

program createFile;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils;

const
  filename = 'output.txt';

var
  cwdPath,
  fsPath: string;


// Create empty file in current working directory
function CreateEmptyFile1: Boolean;
var
  f: textfile;
begin
  // Make path to the file to be created
  cwdPath := ExtractFilePath(ParamStr(0)) + '1_'+filename;

  // Create file
  AssignFile(f,cwdPath);
  {$I-}
  Rewrite(f);
  {$I+}
  Result := IOResult = 0;
  CloseFile(f);
end;

// Create empty file in filesystem root
function CreateEmptyFile2: Boolean;
var
  f: textfile;
begin
  // Make path to the file to be created
  fsPath := ExtractFileDrive(ParamStr(0)) + '\' + '2_'+filename;

  // Create file
  AssignFile(f,fsPath);
  {$I-}
  Rewrite(f);
  {$I+}
  Result := IOResult = 0;
  CloseFile(f);
end;

function CreateEmptyFile3: Boolean;
var
  fs: TFileStream;
begin
  // Make path to the file to be created
  cwdPath := ExtractFilePath(ParamStr(0)) + '3_'+filename;

  // Create file
  fs := TFileStream.Create(cwdPath,fmCreate);
  fs.Free;
  Result := FileExists(cwdPath);
end;

function CreateEmptyFile4: Boolean;
var
  fs: TFileStream;
begin
  // Make path to the file to be created
  fsPath := ExtractFileDrive(ParamStr(0)) + '\' + '4_'+filename;

  // Create file
  fs := TFileStream.Create(fsPath,fmCreate);
  fs.Free;
  Result := FileExists(fsPath);
end;

begin
  if CreateEmptyFile1 then
    Writeln('File created at '+cwdPath)
    else
    Writeln('Error creating file at '+cwdPath);

  if CreateEmptyFile2 then
    Writeln('File created at '+fsPath)
    else
    Writeln('Error creating file at '+fsPath);

  if CreateEmptyFile3 then
    Writeln('File created at '+cwdPath)
    else
    Writeln('Error creating file at '+cwdPath);

  if CreateEmptyFile4 then
    Writeln('File created at '+fsPath)
    else
    Writeln('Error creating file at '+fsPath);

  // Keep console window open
  Readln;
end.



```



## DCL


```DCL
open/write output_file output.txt
open/write output_file [000000]output.txt
create/directory [.docs]
create/directory [000000.docs]
```



## E



```e><file:output.txt
.setBytes([])
<file:docs>.mkdir(null)
<file:///output.txt>.setBytes([])
<file:///docs>.mkdir(null)
```



## EchoLisp


```lisp

;; The file system is the browser local storage
;; It is divided into named stores (directories)
;; "user" is the default (home) store

; before : list of stores
(local-stores) → ("system" "user" "words" "reader" "info" "root")

(local-put-value "output.txt" "") → "output.txt" ; into "user"
(local-make-store "user/docs") → "user/docs"
(local-put-value "output.txt" "" "root") → "output.txt" ; into "root"
(local-make-store 'root/docs) → "root/docs"

; after : list of stores
(local-stores 'root) → ("root" "root/docs")
(local-stores 'user) → ("user" "user/docs")

```



## Elena

ELENA 4.x :

```elena
import system'io;

public program()
{
    File.assign("output.txt").textwriter().close();

    File.assign("\output.txt").textwriter().close();

    Directory.assign("docs").create();

    Directory.assign("\docs").create();
}
```



## Elixir


```elixir
File.open("output.txt", [:write])
File.open("/output.txt", [:write])

File.mkdir!("docs")
File.mkdir!("/docs")
```



## Emacs Lisp


```Lisp

(shell-command "touch output.txt & mkdir docs")
(cd "~/")
(shell-command "touch output.txt & mkdir docs")
```




## Erlang

"/" is documented as working on Windows.

```erlang

-module(new_file).
-export([main/0]).

main() ->
	ok = file:write_file( "output.txt", <<>> ),
	ok = file:make_dir( "docs" ),
	ok = file:write_file( filename:join(["/", "output.txt"]), <<>> ),
	ok = file:make_dir( filename:join(["/", "docs"]) ).

```



## ERRE

Filenames are in 8+3 DOS format: without drive and directory info, refer to the same directory as the ERRE program is running from; full pathnames can include drive name and directory.
You must use PC.LIB for managing directories.

```ERRE

PROGRAM FILE_TEST

!$INCLUDE="PC.LIB"

BEGIN

    OPEN("O",#1,"output.txt")
    CLOSE(1)

    OS_MKDIR("C:\RC")   ! with the appropriate access rights .......
    OPEN("O",#1,"C:\RC\output.txt")
    CLOSE(1)

END PROGRAM

```



## Euphoria


```euphroria
integer fn

-- In the current working directory
system("mkdir docs",2)
fn = open("output.txt","w")
close(fn)

-- In the filesystem root
system("mkdir \\docs",2)
fn = open("\\output.txt","w")
close(fn)
```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO

[<EntryPoint>]
let main argv =
    let fileName = "output.txt"
    let dirName = "docs"
    for path in ["."; "/"] do
        ignore (File.Create(Path.Combine(path, fileName)))
        ignore (Directory.CreateDirectory(Path.Combine(path, dirName)))
    0
```



## Factor


```factor
USE: io.directories

"output.txt" "/output.txt" [ touch-file ] bi@
"docs" "/docs" [ make-directory ] bi@
```



## Fancy


```fancy
["/", "./"] each: |dir| {
  # create '/docs', then './docs'
  Directory create: (dir ++ "docs")
  # create files /output.txt, then ./output.txt
  File open: (dir ++ "output.txt") modes: ['write] with: |f| {
    f writeln: "hello, world!"
  }
}
```



## Forth

There is no means to create directories in ANS Forth.

```forth
 s" output.txt" w/o create-file throw ( fileid) drop
s" /output.txt" w/o create-file throw ( fileid) drop
```



## Fortran

Don't know a way of creating directories in Fortran
#Edit: Use system commands to create directories

```fortran

PROGRAM CREATION
OPEN (UNIT=5, FILE="output.txt", STATUS="NEW")   ! Current directory
CLOSE (UNIT=5)
OPEN (UNIT=5, FILE="/output.txt", STATUS="NEW")  ! Root directory
CLOSE (UNIT=5)

!Directories (Use System from GNU Fortran Compiler)
! -- Added by Anant Dixit, November 2014
call system("mkdir docs/")
call system("mkdir ~/docs/")

END PROGRAM

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' create empty file and sub-directory in current directory
Open "output.txt" For Output As #1
Close #1
MkDir "docs"

' create empty file and sub-directory in root directory c:\
' creating file in root requires administrative privileges in Windows 10
Open "c:\output.txt" For Output As #1
Close #1
MkDir "c:\docs"

Print "Press any key to quit"
Sleep
```



## friendly interactive shell

```fishshell
touch {/,}output.txt    # create both /output.txt and output.txt
mkdir {/,}docs          # create both /docs and docs
```



## FunL

```funl
import io.File

File( 'output.txt' ).createNewFile()
File( File.separator + 'output.txt' ).createNewFile()
File( 'docs' ).mkdir()
File( File.separator + 'docs' ).mkdir()
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=abe59d5d62a4d01817638115e75e7e29 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byCount As Byte
Dim sToSave As String

For byCount = 0 To 50
  sToSave &= Format(Str(byCount), "00") & " - Charlie was here!" & gb.NewLine
Next

File.Save(User.Home &/ "TestFile", sToSave)
Print File.Load(User.Home &/ "TestFile")

End
```

Output:

```txt

00 - Charlie was here!
01 - Charlie was here!
02 - Charlie was here!
03 - Charlie was here!
04 - Charlie was here!
.........

```



## Go


```go
package main

import (
    "fmt"
    "os"
)

func createFile(fn string) {
    f, err := os.Create(fn)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("file", fn, "created!")
    f.Close()
}

func createDir(dn string) {
    err := os.Mkdir(dn, 0666)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("directory", dn, "created!")
}

func main() {
    createFile("input.txt")
    createFile("/input.txt")
    createDir("docs")
    createDir("/docs")
}
```



## Groovy


```groovy
new File("output.txt").createNewFile()
new File(File.separator + "output.txt").createNewFile()
new File("docs").mkdir()
new File(File.separator + "docs").mkdir()
```



## Haskell



```haskell
import System.Directory

createFile name = writeFile name ""

main = do
  createFile "output.txt"
  createDirectory "docs"
  createFile "/output.txt"
  createDirectory "/docs"
```



## HicEst


```hicest
SYSTEM(DIR="\docs")              ! create argument if not existent, make it current
OPEN(FILE="output.txt", "NEW")   ! in current directory

SYSTEM(DIR="C:\docs")            ! create C:\docs if not existent, make it current
OPEN(FILE="output.txt", "NEW")   ! in C:\docs
```



## i


```i
software {
	create("output.txt")
	create("docs/")
	create("/output.txt")
	create("/docs/")
}
```


=={{header|Icon}} and {{header|Unicon}}==
Icon does not support 'mkdir' - otherwise the Unicon code below will work.  A work around would be to use 'system' to invoke command line to create a directory.

```Unicon
every dir := !["./","/"] do {
   close(open(f := dir || "input.txt","w"))  |stop("failure for open ",f)
   mkdir(f := dir || "docs")                 |stop("failure for mkdir ",f)
   }
```

Note: Icon and Unicon accept both / and \ for directory separators.


## J


The conjunction <tt>!:</tt> with a scalar <tt>1</tt> to the left (<tt>1!:</tt>) provides the underlying cross-platform support for [http://www.jsoftware.com/help/dictionary/dx001.htm working with files].


```j
'' 1!:2 <'/output.txt'   NB. write an empty file
   1!:5 <'/docs'         NB. create a directory
```


However a number of libraries provide a more convenient/conventional interface to that underlying functionality.

```j
require 'files'
NB. create two empty files named /output.txt and output.txt
'' fwrite '/output.txt' ; 'output.txt'

require 'general/dirutils'   NB. addon package
NB. create two directories: /docs and docs:
dircreate '/docs' ; 'docs'
```


Finally note that writing a file in J creates that file.  In typical use, files are referred to by name, and the entire contents of the file are written.  (Appends and partial writes are also supported but they are more complicated than the typical case.)


### See Also


* http://www.jsoftware.com/help/learning/28.htm
* http://www.jsoftware.com/help/jforc/input_and_output.htm
* http://www.jsoftware.com/jwiki/Studio/Mapped%20Files


## Java


```java
import java.io.*;
public class CreateFileTest {
	public static void main(String args[]) {
		try {
			new File("output.txt").createNewFile();
			new File(File.separator + "output.txt").createNewFile();
			new File("docs").mkdir();
			new File(File.separator + "docs").mkdir();
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}
}
```



## JCL


```JCL

// EXEC PGM=IEFBR14
//* CREATE EMPTY FILE NAMED "OUTPUT.TXT" (file names upper case only)
//ANYNAME DD UNIT=SYSDA,SPACE=(0,0),DSN=OUTPUT.TXT,DISP=(,CATLG)
//* CREATE DIRECTORY (PARTITIONED DATA SET) NAMED "DOCS"
//ANYNAME DD UNIT=SYSDA,SPACE=(TRK,(1,1)),DSN=DOCS,DISP=(,CATLG)

```



## Julia


```Julia
# many I/O functions have UNIX names

touch("output.txt")
mkdir("docs")

# probably don't have permission
try
    touch("/output.txt")
    mkdir("/docs")
catch e
    warn(e)
end
```



## K

Directory creation is OS-dependent
```K
   "output.txt" 1: ""
   "/output.txt" 1: ""
   \ mkdir docs
   \ mkdir /docs
```



## Kotlin


```scala
/* testing on Windows 10 which needs administrative privileges
   to create files in the root */

import java.io.File

fun main(args: Array<String>) {
    val filePaths = arrayOf("output.txt", "c:\\output.txt")
    val dirPaths  = arrayOf("docs", "c:\\docs")
    var f: File
    for (path in filePaths) {
        f = File(path)
        if (f.createNewFile())
            println("$path successfully created")
        else
            println("$path already exists")
    }
    for (path in dirPaths) {
        f = File(path)
        if (f.mkdir())
            println("$path successfully created")
        else
            println("$path already exists")
    }
}
```


```txt

output.txt successfully created
c:\output.txt successfully created
docs successfully created
c:\docs successfully created

```

Running program again after files created :
```txt

output.txt already exists
c:\output.txt already exists
docs already exists
c:\docs already exists

```



## LabVIEW

## Lasso


```Lasso
// create file
local(f) = file
handle => { #f->close }
#f->openWriteOnly('output.txt')

// make directory, just like a file
local(d = dir('docs'))
#d->create

// create file in root file system (requires permissions at user OS level)
local(f) = file
handle => { #f->close }
#f->openWriteOnly('//output.txt')

// create directory in root file system (requires permissions at user OS level)
local(d = dir('//docs'))
#d->create
```



## Lingo


Create an empty file in cwd:


```lingo
-- note: fileIO xtra is shipped with Director, i.e. an "internal"
fp = xtra("fileIO").new()
fp.createFile("output.txt")
```


Create empty file in root of current volume:


```lingo
-- note: fileIO xtra is shipped with Director, i.e. an "internal"
pd = the last char of _movie.path -- "\" for win, ":" for mac
_player.itemDelimiter = pd
vol = _movie.path.item[1]
fp = xtra("fileIO").new()
fp.createFile(vol&pd&"output.txt")
```


Creating an empty directory requires a 3rd party xtra, but there are various free xtras that allow this. Here as example usage of Shell xtra:


```lingo
shell_cmd("mkdir Docs") -- in cwd, both win and mac
shell_cmd("mkdir \Docs") -- win
shell_cmd("mkdir /Docs") -- mac
```



## Liberty BASIC

Filenames without drive and directory info. refer to the same directory as the LB program is running from.


Full pathnames including drive name and directory can be used- back-slash separated.

```lb

nomainwin

open "output.txt" for output as #f
close #f

result = mkdir( "F:\RC")
if result <>0 then notice "Directory not created!": end

open "F:\RC\output.txt" for output as #f
close #f

end

```


## Little

We are going to use /tmp instead the root.


```C
void create_file(string path) {
    FILE f;
    unless (exists(path)) {
        unless (f = fopen(path, "w")){
            die(path);
        } else {
            puts("file ${path} created");
            fclose(f);
        }
    } else {
        puts("File ${path} already exists");
    }
}

void create_dir(string path) {
    unless (exists(path)) {
        unless(mkdir(path)) { //mkdir returns 0 on success, -1 on error
            puts("directory ${path} created");
        } else {
            puts(stderr, "Error: directory ${path} not created");
        }
    } else {
        puts("directory ${path} already exists");
    }
}

create_file("output.txt");
create_file("/tmp/output.txt");
create_dir("docs");
create_dir("/tmp/docs");
```



## LFE



```lisp

(: file write_file '"output.txt" '"Some data")
(: file make_dir '"docs")
(: file write_file '"/output.txt" '"Some data")
(: file make_dir '"/docs")

```



## Lua

### = Create File =



```lua
io.open("output.txt", "w"):close()
io.open("\\output.txt", "w"):close()
```



### = Create Directory =

This solution sends the command to the OS shell.


```lua
os.execute("mkdir docs")
os.execute("mkdir \\docs")
```


A more portable solution requires a library such as LuaFileSystem.


```lua
require "lfs"
lfs.mkdir("docs")
lfs.mkdir("/docs")
```



## M2000 Interpreter

Create directory and a file in two directories. First we look if places are in Drive Fixed.

If ok then we look if directory exist and if not then we make it

We check the time stamp of directory

Next we make an empty file, and get the time stamp of it

We use a Sub as Task(). Subs can view everything in Module. This not hold for modules, a module can't see anything outside in either way, for parent module or child module, with one exception, a module in a Group can see other members in that group, if they are in same level, or public members from children groups, lower levels (upper levels are not aware for modules).

In subs we have to use Local to make local variables, that can be shadow local variables with same name in module.

We can use Try {Print str$(File.Stamp("output.txt"), "YYYY|MM|DD|hh:nn:ss")} so if file not exist nothing printed and error dropped from Try block.

Work nice in Ubuntu using Wine.

Work nice in Windows 10 64bit. From explorer in C:\ we see directory docs but not file output.txt. File exist when we use M2000. Perhaps Explorer hide files;



```M2000 Interpreter

Module MakeDirAndFile {
      Def WorkingDir$, RootDir$
      If Drive$(Dir$)="Drive Fixed" Then WorkingDir$=Dir$
      If Drive$("C:\")="Drive Fixed" Then RootDir$="C:\"

      if WorkingDir$<>"" Then task(WorkingDir$)
      If RootDir$<>"" then task(RootDir$)
      Dir User ' return to user directory

      Sub task(WorkingDir$)
            Dir WorkingDir$
            If Not Exist.Dir("docs") then SubDir "docs" : Dir WorkingDir$
            If Exist.Dir("docs") Then Print str$(File.Stamp("docs"), "YYYY|MM|DD|hh:nn:ss")
            Open "output.txt" For Output as #F
            Close #f
            If Exist("output.txt") Then Print str$(File.Stamp("output.txt"), "YYYY|MM|DD|hh:nn:ss")
      End Sub
}
MakeDirAndFile

```



## Maple


```Maple

FileTools:-Text:-WriteFile("output.txt", "");  # make empty file in current dir
FileTools:-MakeDirectory("docs");               # make empty dir in current dir
FileTools:-Text:-WriteFile("/output.txt", ""); # make empty file in root dir
FileTools:-MakeDirectory("/docs");              # make empty dir in root dir

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

SetDirectory@NotebookDirectory[];
t = OpenWrite["output.txt"]
Close[t]
s = OpenWrite[First@FileNameSplit[$InstallationDirectory] <> "\\output.txt"]
Close[s]

(*In root directory*)
CreateDirectory["\\docs"]
(*In current operating directory*)
CreateDirectory[Directory[]<>"\\docs"]
(*"left<>right" is shorthand for "StringJoin[left,right]"*)


```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
	fid = fopen('output.txt','w'); fclose(fid);
	fid = fopen('/output.txt','w'); fclose(fid);
	mkdir('docs');
	mkdir('/docs');
```



## Maxima


```maxima
f: openw("/output.txt");
close(f);

f: openw("output.txt");
close(f);

/* Maxima has no function to create directories, but one can use the underlying Lisp system */

:lisp (mapcar #'ensure-directories-exist '("docs/" "/docs/"))
```




## MAXScript


```maxscript
-- Here
f = createFile "output.txt"
close f
makeDir (sysInfo.currentDir + "\docs")
-- System root
f = createFile "\output.txt"
close f
makeDir ("c:\docs")
```



## Mercury


```mercury
:- module create_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dir.

main(!IO) :-
    create_file("output.txt", !IO),
    create_file("/output.txt", !IO),
    create_dir("docs", !IO),
    create_dir("/docs", !IO).

:- pred create_file(string::in, io::di, io::uo) is det.

create_file(FileName, !IO) :-
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(File),
        io.close_output(File, !IO)
    ;
        Result = error(Error),
        print_io_error(Error, !IO)
    ).

:- pred create_dir(string::in, io::di, io::uo) is det.

create_dir(DirName, !IO) :-
    dir.make_single_directory(DirName, Result, !IO),
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



## Mirah


```mirah
import java.io.File

File.new('output.txt').createNewFile()
File.new('docs').mkdir()
File.new("docs#{File.separator}output.txt").createNewFile()

```


=={{header|Modula-3}}==

```modula3
MODULE FileCreation EXPORTS Main;

IMPORT FS, File, OSError, IO, Stdio;

VAR file: File.T;

BEGIN
  TRY
    file := FS.OpenFile("output.txt");
    file.close();
    FS.CreateDirectory("docs");
    file := FS.OpenFile("/output.txt");
    file.close();
    FS.CreateDirectory("/docs");
  EXCEPT
  | OSError.E => IO.Put("Error creating file or directory.\n", Stdio.stderr);
  END;
END FileCreation.
```



## Nanoquery


```nanoquery
import "Nanoquery.IO"

$f = new("File")
$f.create("output.txt")
$f.createDir("docs")

// in the root directory
$f.create("/output.txt")
$f.createDir("/docs")

```



## Nemerle


```Nemerle
using System;
using System.IO;

module CreateFile
{
    Main() : void
    {
        unless (File.Exists("output.txt")) File.Create("output.txt");         // here
        // returns a FileStream object which we're ignoring
        try {
            unless (File.Exists(@"\output.txt")) File.Create(@"\output.txt"); // root
        }
        catch {
            |e is UnauthorizedAccessException => Console.WriteLine(
            "Cannot create file in root directory without Administrator priveleges.")
        }

        unless (Directory.Exists("docs")) Directory.CreateDirectory("docs");
        // returns a DirectoryInfo object which we're ignoring
        unless (Directory.Exists(@"\docs")) Directory.CreateDirectory(@"\docs");
        // no Exception for directory creation
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

fName = ''; fName[0] = 2; fName[1] = '.' || File.separator || 'output.txt'; fName[2] = File.separator || 'output.txt'
dName = ''; dName[0] = 2; dName[1] = '.' || File.separator || 'docs';       dName[2] = File.separator || 'docs'

do
  loop i_ = 1 to fName[0]
    say fName[i_]
    fc = File(fName[i_]).createNewFile()
    if fc then  say 'File' fName[i_] 'created successfully.'
    else        say 'File' fName[i_] 'aleady exists.'
    end i_

  loop i_ = 1 to dName[0]
    say dName[i_]
    dc = File(dName[i_]).mkdir()
    if dc then  say 'Directory' dName[i_] 'created successfully.'
    else        say 'Directory' dName[i_] 'aleady exists.'
    end i_
catch iox = IOException
  iox.printStackTrace
end

return

```



## Nim


```nim
import os

open("output.txt", fmWrite).close()
createDir("docs")

open(DirSep & "output.txt", fmWrite).close()
createDir(DirSep & "docs")
```


```nim
import os
const directories = ["/", "./"]
for directory in directories:
  open(directory & "output.txt", fmWrite).close()
  createDir(directory & "docs")
```


=={{header|Objective-C}}==


```objc
NSFileManager *fm = [NSFileManager defaultManager];

[fm createFileAtPath:@"output.txt" contents:[NSData data] attributes:nil];
// Pre-OS X 10.5
[fm createDirectoryAtPath:@"docs" attributes:nil];
// OS X 10.5+
[fm createDirectoryAtPath:@"docs" withIntermediateDirectories:NO attributes:nil error:NULL];
```



## Objeck


``` objeck

use IO;

bundle Default {
  class FileExample {
    function : Main(args : String[]) ~ Nil {
      file := FileWriter->New("output.txt");
      file->Close();

      file := FileWriter->New("/output.txt");
      file->Close();

      Directory->Create("docs");
      Directory->Create("/docs");
    }
  }
}

```



## OCaml



```ocaml
# let oc = open_out "output.txt" in
  close_out oc;;
- : unit = ()

# Unix.mkdir "docs" 0o750 ;; (* rights 0o750 for rwxr-x--- *)
- : unit = ()
```


(for creation in the filesystem root, replace the filenames by "/output.txt" and "/docs")


## Oz


```oz
for Dir in ["/" "./"] do
   File = {New Open.file init(name:Dir#"output.txt" flags:[create])}
in
   {File close}
   {OS.mkDir Dir#"docs" ['S_IRUSR' 'S_IWUSR' 'S_IXUSR' 'S_IXGRP']}
end
```




## PARI/GP

Creating an empty file in GP requires <code>write1</code> rather than <code>write</code> to avoid the automatic newline.

```parigp
write1("0.txt","")
write1("/0.txt","")
```


GP cannot, itself, create directories; for that, you would need PARI (where the solution would follow those in [[#C|C]]) or <code>system</code>:

```parigp
system("mkdir newdir")
```



## Pascal


The Pascal & Delphi Standard Libraries support all of this functionality.

<lang pascal-delphi>
program in out;

var

   f : textfile;

begin

   assignFile(f,'/output.txt');
   rewrite(f);
   close(f);
   makedir('/docs');
   assignFile(f,'/docs/output.txt');
   rewrite(f);
   close(f);

end;

```



## Perl


```perl
use File::Spec::Functions qw(catfile rootdir);
{ # here
    open my $fh, '>', 'output.txt';
    mkdir 'docs';
};
{ # root dir
    open my $fh, '>', catfile rootdir, 'output.txt';
    mkdir catfile rootdir, 'docs';
};
```


'''Without Perl Modules'''

Current directory

```perl
perl -e 'qx(touch output.txt)'
perl -e 'mkdir docs'
```


Root directory

```perl
perl -e 'qx(touch /output.txt)'
perl -e 'mkdir "/docs"'
```


'''For comparison with Perl 6'''

```perl
for my $prefix (qw( ./ / )) {
   mkdir "${prefix}docs";
   open my $FH, '>', "${prefix}docs/output.txt";
}
```

Cleanup

```perl
unlink $_ for qw(/docs/output.txt ./docs/output.txt);
rmdir  $_ for qw(/docs ./docs);
```



## Perl 6


```perl6

for '.', '' -> $prefix {
    mkdir "$prefix/docs";
    open "$prefix/output.txt", :w;
}

```



## Phix

Copy of [[Create_a_file#Euphoria|Euphoria]], modified to display a warning when it cannot create a file in the system root (as such is typically banned on more recent operating systems)

```Phix
integer fn

-- In the current working directory
system("mkdir docs",2)
fn = open("output.txt","w")
close(fn)

-- In the filesystem root
system("mkdir \\docs",2)
fn = open("\\output.txt","w")
if fn=-1 then
    puts(1,"unable to create \\output.txt\n")
else
    close(fn)
end if
```



## PHP


```php
<?php
touch('output.txt');
mkdir('docs');
touch('/output.txt');
mkdir('/docs');
?>
```



## PicoLisp


```PicoLisp
(out "output.txt")                     # Empty output
(call 'mkdir "docs")                   # Call external
(out "/output.txt")
(call 'mkdir "/docs")
```



## Pike


```pike
import Stdio;

int main(){
   write_file("input.txt","",0100);
   write_file("/input.txt","",0100);
}
```



## PL/I


```PL/I

open file (output) title ('/OUTPUT.TXT,type(text),recsize(100)' );
close file (output);

```



## PowerShell


```powershell
New-Item output.txt -ItemType File
New-Item \output.txt -ItemType File
New-Item docs -ItemType Directory
New-Item \docs -ItemType Directory
```



## ProDOS


```ProDOS
makedirectory docs
changedirectory docs
makenewfile output.txt
```



## PureBasic



```PureBasic
CreateFile(0,"output.txt"):CloseFile(0)
CreateDirectory("docs")
CreateFile(0,"/output.txt"):CloseFile(0)
CreateDirectory("/docs")
```



## Python


```python
import os
for directory in ['/', './']:
  open(directory + 'output.txt', 'w').close()  # create /output.txt, then ./output.txt
  os.mkdir(directory + 'docs')                 # create directory /docs, then ./docs
```


Exception-safe way to create file:


```python
from __future__ import with_statement
import os
def create(directory):
    with open(os.path.join(directory, "output.txt"), "w"):
        pass
    os.mkdir(os.path.join(directory, "docs"))

create(".") # current directory
create("/") # root directory
```



## R


```R
f <- file("output.txt", "w")
close(f)

# it may fails and the exact syntax to achieve the root
# changes according to the operating system
f <- file("/output.txt", "w")
close(f)

success <- dir.create("docs")
success <- dir.create("/docs")
```



## Racket


```Racket
#lang racket

(display-to-file "" "output.txt")
(make-directory "docs")
(display-to-file "" "/output.txt")
(make-directory "/docs")
```



## Raven



```raven
"" as str
str 'output.txt'  write
str '/output.txt' write
'docs'  mkdir
'/docs' mkdir
```



## REBOL



```REBOL
; Creating in current directory:

write %output.txt ""
make-dir %docs/

; Creating in root directory:

write %/output.txt ""
make-dir %/docs/

```



## Retro

There are no facilities in Retro to create directories.


```Retro
with files'
"output.txt" :w open close drop
"/output.txt" :w open close drop
```



## REXX

This REXX version works under Microsoft Windows (any version).

```rexx
/*REXX pgm creates a new empty file and directory; in curr dir and root.*/
       do 2                            /*perform three statements twice.*/
       'COPY NUL output.txt'           /*copy a "null" (empty) file.    */
       'MKDIR DOCS'                    /*make a directory (aka: folder).*/
       'CD \'                          /*change currect dir to the root.*/
       end   /*2*/                     /*now, go and perform them again.*/
                                       /*stick a fork in it, we're done.*/
```




## Ring


```ring

system("mkdir C:\Ring\docs")
fopen("C:\Ring\docs\output.txt", "w+")
system("mkdir docs")
fopen("output.txt", "w+")

```



## Ruby


```ruby
['/', './'].each{|dir|
  Dir.mkdir(dir + 'docs')      # create '/docs', then './docs'
  File.open(dir + 'output.txt', 'w') {}  # create empty file /output.txt, then ./output.txt
}
```


## Run BASIC


```RunBasic
open "output.txt" for output as #f
close #f

dirOk = mkdir( "f:\doc")
if not(dirOk) then print "Directory not created!": end

open "f:\doc\output.txt" for output as #f
close #f
```


## Rust


```rust
use std::io::{self, Write};
use std::fs::{DirBuilder, File};
use std::path::Path;
use std::{process,fmt};

const FILE_NAME: &'static str = "output.txt";
const DIR_NAME : &'static str = "docs";

fn main() {
    create(".").and(create("/"))
               .unwrap_or_else(|e| error_handler(e,1));
}


fn create<P>(root: P) -> io::Result<File>
    where P: AsRef<Path>
{
    let f_path = root.as_ref().join(FILE_NAME);
    let d_path = root.as_ref().join(DIR_NAME);
    DirBuilder::new().create(d_path).and(File::create(f_path))
}

fn error_handler<E: fmt::Display>(error: E, code: i32) -> ! {
    let _ = writeln!(&mut io::stderr(), "Error: {}", error);
    process::exit(code)
}
```



## Scala

```scala
import java.io.File

object CreateFile extends App {
  try { new File("output.txt").createNewFile() }
  catch { case e: Exception => println(s"Exception caught: $e with creating output.txt") }
  try { new File(s"${File.separator}output.txt").createNewFile() }
  catch { case e: Exception => println(s"Exception caught: $e with creating ${File.separator}output.txt") }
  try { new File("docs").mkdir() }
  catch { case e: Exception => println(s"Exception caught: $e with creating directory docs") }
  try { new File(s"${File.separator}docs").mkdir() }
  catch { case e: Exception => println(s"Exception caught: $e with creating directory ${File.separator}docs") }
}
```



## Scheme


```scheme
(open-output-file "output.txt")
(open-output-file "/output.txt")
```

Results:
 > file output.txt
 output.txt: empty
 > file /output.txt
 /output.txt: empty
I am not aware of any '''standard''' way of creating directories in Scheme.


## Seed7

Seed7 uses a [http://seed7.sourceforge.net/manual/os.htm#Standard_path_representation standard path representation]
to make paths operating system independent. In the standard path representation
a / is used as path delimiter and drive letters like C: must be written as /c instead.
Creating files and directories in a file system root may need privileges, so the program may fail,
when it is started by a normal user.


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  local
    var file: aFile is STD_NULL;
  begin
    aFile := open("output.txt", "w");
    close(aFile);
    mkdir("docs");
    aFile := open("/output.txt", "w");
    close(aFile);
    mkdir("/docs");
  end func;
```


Under Windows each filesystem has its own root.
Therefore you need to replace "/output.txt" and "/docs" with "/c/output.txt" and "/c/docs".


## Sidef


```ruby
# Here
%f'output.txt' -> create;
%d'docs'       -> create;

# Root dir
Dir.root + %f'output.txt' -> create;
Dir.root + %d'docs'       -> create;
```



## Slate

File creation locally:

```slate
(File newNamed: 'output.txt') touch.
(Directory current / 'output.txt') touch.
```


File creation at root:

```slate
(File newNamed: '/output.txt') touch.
(Directory root / 'output.txt') touch.
```



## Smalltalk


[[Squeak]] has no notion of 'current directory' because it isn't tied to the shell that created it.


```smalltalk
(FileDirectory on: 'c:\') newFileNamed: 'output.txt'; createDirectory: 'docs'.
```


In [[GNU Smalltalk]] you can do instead:


```smalltalk
ws := (File name: 'output.txt') writeStream.
ws close.
Directory create: 'docs'.

ws := (File name: '/output.txt') writeStream.
ws close.
Directory create: '/docs'.
```



## SNOBOL4


```SNOBOL4
        output(.file,1,'output.txt');  endfile(1)  ;* Macro Spitbol
*       output(.file,1,,'output.txt'); endfile(1)  ;* CSnobol
        host(1,'mkdir docs')

        output(.file,1,'/output.txt');  endfile(1) ;* Macro Spitbol
*       output(.file,1,,'/output.txt'); endfile(1) ;* CSnobol
        host(1,'mkdir /docs')
end
```



## SQLite


```sqlite3

/*
*Use '/' for *nix. Use whatever your root directory is on Windows.
*Must be run as admin.
*/
.shell mkdir "docs";
.shell mkdir "/docs";
.output output.txt
.output /output.txt

```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

BEGIN
  DECLARE UTL_FILE_HANDLER UTL_FILE.FILE_TYPE;
  DECLARE DIR_ALIAS_CURRENT VARCHAR(128);
  DECLARE DIR_ALIAS_ROOT VARCHAR(128);
  DECLARE DIRECTORY VARCHAR(1024);
  DECLARE FILENAME VARCHAR(255);

  SET DIR_ALIAS_CURRENT = 'outputFileCurrent';
  SET DIRECTORY = '/home/db2inst1/doc';
  SET FILENAME = 'output.txt';

  CALL UTL_DIR.CREATE_OR_REPLACE_DIRECTORY(DIR_ALIAS_CURRENT, DIRECTORY);
  SET UTL_FILE_HANDLER = UTL_FILE.FOPEN(DIR_ALIAS_CURRENT, FILENAME, 'a');
  CALL UTL_FILE.FFLUSH(UTL_FILE_HANDLER);
  CALL UTL_FILE.FCLOSE(UTL_FILE_HANDLER);

  SET DIR_ALIAS_ROOT = 'outputFileRoot';
  SET DIRECTORY = '/doc';

  CALL UTL_DIR.CREATE_OR_REPLACE_DIRECTORY(DIR_ALIAS_ROOT, DIRECTORY);
  SET UTL_FILE_HANDLER = UTL_FILE.FOPEN(DIR_ALIAS_ROOT, FILENAME, 'a');
  CALL UTL_FILE.FFLUSH(UTL_FILE_HANDLER);
  CALL UTL_FILE.FCLOSE(UTL_FILE_HANDLER);
END @

```

The current directory notion does not exist in Db2. However, we can consider the home directory of the instance (in this case db2inst1) as current.
For the directory under root, Db2 needs extra permissions to create a subdirectory at that level. Normally, that operation of creating a subdirectory at that level will raise an exception: "UTL_FILE.INVALID_OPERATION" SQLSTATE=58024.

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.
db2 => !ls /doc @
output.txt
db2 => !ls /home/db2inst1/doc @
output.txt

```



## Standard ML



```sml
let val out = TextIO.openOut "output.txt" in
  TextIO.closeOut out
end;

OS.FileSys.mkDir "docs";
```


(for creation in the filesystem root, replace the filenames by "/output.txt" and "/docs")


## Stata


```stata
file open f using output.txt, write replace
file close f
mkdir docs

file open f using \output.txt, write replace
file close f
mkdir \docs
```



## Tcl


Assuming that we're supposed to create two files and two directories (one each here and one each in the file system root) and further assuming that the code is supposed to be portable, i.e. work on win, linux, MacOS (the task is really not clear):


```tcl
close [open output.txt w]
close [open [file nativename /output.txt] w]

file mkdir docs
file mkdir [file nativename /docs]
```



## Toka



```toka
needs shell
" output.txt" "W" file.open file.close
" /output.txt" "W" file.open file.close

( Create the directories with permissions set to 777)
" docs" &777 mkdir
" /docs" &777 mkdir
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
- create file
ERROR/STOP CREATE ("output.txt",FDF-o,-std-)
- create directory
ERROR/STOP CREATE ("docs",project,-std-)

```


## UNIX Shell


```bash
touch output.txt /output.txt   # create both output.txt and /output.txt
mkdir /docs
mkdir docs    # create both /docs and docs
```


```bash
touch {/,}output.txt    # create both /output.txt and output.txt
mkdir {/,}docs          # create both /docs and docs
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
{{omit from|Befunge}} <!-- No filesystem support -->


## Ursa


```ursa
decl file f
f.create "output.txt"
f.createdir "docs"

# in the root directory
f.create "/output.txt"
f.createdir "/docs"

```


## VBA


```vb
Public Sub create_file()
    Dim FileNumber As Integer
    FileNumber = FreeFile
    MkDir "docs"
    Open "docs\output.txt" For Output As #FreeFile
    Close #FreeFile
    MkDir "C:\docs"
    Open "C:\docs\output.txt" For Output As #FreeFile
    Close #FreeFile
End Sub
```


## VBScript


```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")

'current directory
objFSO.CreateFolder(".\docs")
objFSO.CreateTextFile(".\docs\output.txt")

'root directory
objFSO.CreateFolder("\docs")
objFSO.CreateTextFile("\docs\output.txt")

```



## Vedit macro language

When closing a file, Vedit saves it only if it has been modified. Therefore, in order to create an empty file, we first insert a character in the file and then delete it.

```vedit
// In current directory
File_Open("input.txt") Ins_Char(' ') Del_Char(-1) Buf_Close()
File_Mkdir("docs")

// In the root directory
File_Open("/input.txt") Ins_Char(' ') Del_Char(-1) Buf_Close()
File_Mkdir("/docs")
```



## Visual Basic .NET


'''Platform:''' [[.NET]]

```vbnet
 'Current Directory
IO.Directory.CreateDirectory("docs")
IO.File.Create("output.txt").Close()

 'Root
IO.Directory.CreateDirectory("\docs")
IO.File.Create("\output.txt").Close()

 'Root, platform independent
IO.Directory.CreateDirectory(IO.Path.DirectorySeparatorChar & "docs")
IO.File.Create(IO.Path.DirectorySeparatorChar & "output.txt").Close()
```




## X86 Assembly

```asm

; syscall numbers for readability. :]

%define sys_mkdir 39
%define sys_creat 8

section .data
	fName       db 'doc/output.txt',0
	rfName      db '/output.txt',0
	dName       db 'doc',0

	err_msg     db "Something went wrong! :[",0xa
	err_len     equ $-err_msg

section .text
global _start

_start:

nop
	mov ebx, dName              ; Directory name
	mov eax, sys_mkdir          ; Specify sys_mkdir call
	mov ecx, 0750o              ; permission (rwxr-x---)
	int 0x80                    ; Make kernel call

	mov ebx, fName              ; File name
	mov eax, sys_creat          ; Specify sys_creat call
	mov ecx, 0640o              ; permission (rw-r-----)
	int 0x80                    ; Make kernel call
	test eax, eax               ; eax AND eax
	js _ragequit                ; If EAX is less than zero
                                    ; THEN Display Message Error

	mov ebx, rfName             ; File name Root
	mov eax, sys_creat          ; Specify sys_creat call
	mov ecx, 0777o              ; permission (rwxrwxrwx)
	int 0x80                    ; Make kernel call
	cmp eax, 0
	jle _exit                   ; IF EAX is less or equal than zero
                                    ; THEN jump to EXIT
                                    ; ELSE Display Message Error

	_ragequit:
		mov edx, err_len        ; Pass offset of the message error
		mov ecx, err_msg        ; Pass the length of the message error
		mov eax, 4              ; Specify sys_write call
		mov ebx, 2              ; Specify File Descriptor 2: Error Output
		int 0x80                ; Make kernel call

	_exit:
		push 0x1
		mov eax, 1              ; Code for Exit Syscall
		push eax
		int 0x80                ; Make kernel call
		ret

```



## zkl

Works on Unix and Windows

```zkl
$ ls -l docs
ls: cannot access docs: No such file or directory
$ zkl
zkl: fcn createOutputTxt(dir){ dir=dir+"/docs"; File.mkdir(dir); File(dir+"/output.txt","w") }
Void
zkl: createOutputTxt(".")
File(./docs/output.txt)
zkl: createOutputTxt("/")
Stack trace for VM#1 ():
   Cmd.createOutputTxt@mkdir addr:16  args(1) reg(0)
   Cmd.__constructor@createOutputTxt addr:9  args(0) reg(0) R
   startup.__constructor addr:2271  args(0) reg(1) ER
   startup.__constructor addr:2207  args(0) reg(22)
Exception thrown: IOError(File.mkdir(//docs): Permission denied)
zkl: ^D
$ ls -l docs
total 0
-rw-r--r-- 1 craigd craigd 0 Oct 27 22:08 output.txt

```



## ZX Spectrum Basic


The ZX Spectrum saves to a tape recorder and does not have subdirectories. Here
we create an empty file named OUTPUT of zero bytes. We can use any start address, because the file is empty. Here we write zero bytes from address 16384:


```zxbasic
SAVE "OUTPUT" CODE 16384,0
```



## Visual Objects

//Use Library System Library

```visualfoxpro

DirMake(String2Psz("c:\docs"))
FCreate("c:\docs\output.txt", FC_NORMAL)

```


