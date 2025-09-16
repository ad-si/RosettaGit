+++
title = "Check that file exists"
description = ""
date = 2019-10-15T08:09:45Z
aliases = []
[extra]
id = 2026
[taxonomies]
categories = ["File System Operations", "Simple", "task"]
tags = []
languages = [
  "11l",
  "ada",
  "aikido",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "delphi",
  "e",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "gap",
  "genie",
  "go",
  "groovy",
  "haskell",
  "hexiscript",
  "hicest",
  "holyc",
  "i",
  "idl",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "labview",
  "lasso",
  "lfe",
  "liberty_basic",
  "little",
  "livecode",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxscript",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
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
  "pop11",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rebol",
  "red",
  "rexx",
  "ring",
  "rlab",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "standard_ml",
  "stata",
  "tcl",
  "toka",
  "tuscript",
  "unix_shell",
  "ursa",
  "vala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic",
  "visual_basic_.net",
  "yabasic",
  "zkl",
]
+++

## Task

### Task:
Verify that a file called     '''input.txt'''     and   a directory called     '''docs'''     exist.


This should be done twice:
:::*   once for the current working directory,   and
:::*   once for a file and a directory in the filesystem root.



Optional criteria (May 2015):   verify it works with:
:::*   zero-length files
:::*   an unusual filename:   <big> ''' `Abdu'l-Bahá.txt ''' </big>





## 11l

{{Trans|Python}}

```11l
fs:is_file(‘input.txt’)
fs:is_file(‘/input.txt’)
fs:is_dir(‘docs’)
fs:is_dir(‘/docs’)
```



## Ada

This example should work with any Ada 95 compiler.

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure File_Exists is
   function Does_File_Exist (Name : String) return Boolean is
      The_File : Ada.Text_IO.File_Type;
   begin
      Open (The_File, In_File, Name);
      Close (The_File);
      return True;
   exception
      when Name_Error =>
         return False;
   end Does_File_Exist;
begin
   Put_Line (Boolean'Image (Does_File_Exist ("input.txt" )));
   Put_Line (Boolean'Image (Does_File_Exist ("\input.txt")));
end File_Exists;
```

This example should work with any Ada 2005 compiler.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

procedure File_Exists is
   procedure Print_File_Exist (Name : String) is
   begin
      Put_Line ("Does " & Name & " exist? " &
                  Boolean'Image (Exists (Name)));
   end Print_File_Exist;
   procedure Print_Dir_Exist (Name : String) is
   begin
      Put_Line ("Does directory " & Name & " exist? " &
                  Boolean'Image (Exists (Name) and then Kind (Name) = Directory));
   end Print_Dir_Exist;
begin
   Print_File_Exist ("input.txt" );
   Print_File_Exist ("/input.txt");
   Print_Dir_Exist ("docs");
   Print_Dir_Exist ("/docs");
end File_Exists;
```



## Aikido

The <code>stat</code> function returns a <code>System.Stat</code> object for an existing file or directory, or <code>null</code> if it can't be found.

```aikido

function exists (filename) {
    return stat (filename) != null
}

exists ("input.txt")
exists ("/input.txt")
exists ("docs")
exists ("/docs")


```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
Uses the Algol 68G specific "file is directory" procedure to test for the existence of directories.

```algol68
# Check files and directories exist                               #

# check a file exists by attempting to open it for input          #
# returns TRUE if the file exists, FALSE otherwise                #
PROC file exists = ( STRING file name )BOOL:
    IF  FILE f;
        open( f, file name, stand in channel ) = 0
    THEN
        # file opened OK so must exist                            #
        close( f );
        TRUE
    ELSE
        # file cannot be opened - assume it does not exist        #
        FALSE
    FI # file exists # ;

# print a suitable messages if the specified file exists          #
PROC test file exists = ( STRING name )VOID:
    print( ( "file: "
           , name
           , IF file exists( name ) THEN " does" ELSE " does not" FI
           , " exist"
           , newline
           )
         );
# print a suitable messages if the specified directory exists     #
PROC test directory exists = ( STRING name )VOID:
    print( ( "dir:  "
           , name
           , IF file is directory( name ) THEN " does" ELSE " does not" FI
           , " exist"
           , newline
           )
         );

# test the flies and directories mentioned in the task exist or not #
test file exists( "input.txt" );
test file exists( "\input.txt");
test directory exists( "docs" );
test directory exists( "\docs" )
```



## AppleScript

{{Trans|JavaScript}}
(macOS JavaScript for Automation)

```AppleScript
use framework "Foundation" -- YOSEMITE OS X onwards
use scripting additions

on run
    setCurrentDirectory("~/Desktop")

    ap({doesFileExist, doesDirectoryExist}, ¬
        {"input.txt", "/input.txt", "docs", "/docs"})

    --> {true, true, true, true, false, false, true, true}

    -- The first four booleans are returned by `doesFileExist`.

    -- The last four are returned by `doesDirectoryExist`,
    -- which yields false for simple files, and true for directories.
end run

-- GENERIC SYSTEM DIRECTORY FUNCTIONS -----------------------------------------

-- doesDirectoryExist :: String -> Bool
on doesDirectoryExist(strPath)
    set ca to current application
    set oPath to (ca's NSString's stringWithString:strPath)'s ¬
        stringByStandardizingPath
    set {bln, int} to (ca's NSFileManager's defaultManager()'s ¬
        fileExistsAtPath:oPath isDirectory:(reference))
    bln and (int = 1)
end doesDirectoryExist

-- doesFileExist :: String -> Bool
on doesFileExist(strPath)
    set ca to current application
    set oPath to (ca's NSString's stringWithString:strPath)'s ¬
        stringByStandardizingPath
    ca's NSFileManager's defaultManager()'s fileExistsAtPath:oPath
end doesFileExist

-- getCurrentDirectory :: String
on getCurrentDirectory()
    set ca to current application
    ca's NSFileManager's defaultManager()'s currentDirectoryPath as string
end getCurrentDirectory

-- getFinderDirectory :: String
on getFinderDirectory()
    tell application "Finder" to POSIX path of (insertion location as alias)
end getFinderDirectory

-- getHomeDirectory :: String
on getHomeDirectory()
    (current application's NSHomeDirectory() as string)
end getHomeDirectory

-- setCurrentDirectory :: String -> IO ()
on setCurrentDirectory(strPath)
    if doesDirectoryExist(strPath) then
        set ca to current application
        set oPath to (ca's NSString's stringWithString:strPath)'s ¬
            stringByStandardizingPath
        ca's NSFileManager's defaultManager()'s ¬
            changeCurrentDirectoryPath:oPath
    end if
end setCurrentDirectory


-- GENERIC HIGHER ORDER FUNCTIONS FOR THE TEST --------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set {intFs, intXs} to {length of fs, length of xs}
    set lst to {}
    repeat with i from 1 to intFs
        tell mReturn(item i of fs)
            repeat with j from 1 to intXs
                set end of lst to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return lst
end ap

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

{{Out}}
The first four booleans are returned by `doesFileExist`.

The last four are returned by `doesDirectoryExist`,
which yields false for simple files, and true for directories.

```AppleScript
{true, true, true, true, false, false, true, true}
```



## Applesoft BASIC

The error code for FILE NOT FOUND is 6.

```ApplesoftBasic
100 F$ = "THAT FILE"
110 T$(0) = "DOES NOT EXIST."
120 T$(1) = "EXISTS."
130 GOSUB 200"FILE EXISTS?
140 PRINT F$; " "; T$(E)
150 END

200 REM FILE EXISTS?
210 REM TRY
220    ON ERR GOTO 300"CATCH
230    PRINT CHR$(4); "VERIFY "; F$
240    POKE 216, 0 : REM ONERR OFF
250    E = 1
260    GOTO 350"END TRY
300 REM CATCH
310    E = PEEK(222) <> 6
320    POKE 216, 0 : REM ONERR OFF
330    IF E THEN RESUME : REM THROW
340    CALL  - 3288 : REM RECOVER
350 REM END TRY
360 RETURN

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program verifFic.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ OPEN,   5     @ Linux syscall
.equ CLOSE, 6     @ Linux syscall

.equ O_RDWR,	0x0002		/* open for reading and writing */

/*******************************************/
/* Fichier des macros                       */
/********************************************/
.include "../../ficmacros.s"

/* Initialized data */
.data
szMessFound1:     .asciz "File 1 found.\n"
szMessFound2:     .asciz "File 2 found.\n"
szMessNotFound1: .asciz "File 1 not found.\n"
szMessNotFound2: .asciz "File 2 not found.\n"
szMessNotAuth2:   .asciz "File 2 permission denied.\n"
szCarriageReturn: .asciz "\n"

/* areas strings  */
szFicName1:  .asciz "test1.txt"
szFicName2:  .asciz "/debian-binary"


/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    /*************************************
     open file 1
    ************************************/
    ldr r0,iAdrszFicName1    @ file name
    mov r1,#O_RDWR             @ flags
    mov r2,#0                    @ mode
    mov r7, #OPEN               @ call system OPEN
    swi 0
    cmp r0,#0    @ error ?
    ble 1f
    mov r1,r0    @ FD
    ldr r0,iAdrszMessFound1
    bl affichageMess
    @ close file
    mov r0,r1   @ Fd
    mov r7, #CLOSE
    swi 0
    b 2f
1:
    ldr r0,iAdrszMessNotFound1
    bl affichageMess
2:
    /*************************************
     open file 2
    ************************************/
    ldr r0,iAdrszFicName2    @ file name
    mov r1,#O_RDWR   @  flags
    mov r2,#0         @ mode
    mov r7, #OPEN    @ call system OPEN
    swi 0
    vidregtit verif
    cmp r0,#-13    @ permission denied
    beq 4f
    cmp r0,#0    @ error ?
    ble 3f
    mov r1,r0    @ FD
    ldr r0,iAdrszMessFound2
    bl affichageMess
    @ close file
    mov r0,r1   @ Fd
    mov r7, #CLOSE
    swi 0
    b 100f
3:
    ldr r0,iAdrszMessNotFound2
    bl affichageMess
    b 100f
4:
    ldr r0,iAdrszMessNotAuth2
    bl affichageMess
100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszFicName1:			.int szFicName1
iAdrszFicName2:			.int szFicName2
iAdrszMessFound1:		.int szMessFound1
iAdrszMessFound2:		.int szMessFound2
iAdrszMessNotFound1: 	.int szMessNotFound1
iAdrszMessNotFound2: 	.int szMessNotFound2
iAdrszMessNotAuth2:	.int szMessNotAuth2
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



## Arturo



```arturo
checkIfExists [file]{
    if $(exists file) { print file + " exists" } { print file + " doesn't exist" }
}

checkIfExists "input.txt"
checkIfExists "docs"

checkIfExists "/input.txt"
checkIfExists "/docs"
```



## AutoHotkey

AutoHotkey’s FileExist() function returns an attribute string (a subset of "RASHNDOCT") if a matching file or directory is found. The attribute string must be parsed for the letter D to determine whether the match is a directory or file.



Another option is AutoHotkey's IfExist/IfNotExist command


```autohotkey
; FileExist() function examples
ShowFileExist("input.txt")
ShowFileExist("\input.txt")
ShowFolderExist("docs")
ShowFolderExist("\docs")

; IfExist/IfNotExist command examples (from documentation)
IfExist, D:\
  MsgBox, The drive exists.
IfExist, D:\Docs\*.txt
  MsgBox, At least one .txt file exists.
IfNotExist, C:\Temp\FlagFile.txt
  MsgBox, The target file does not exist.

Return

ShowFileExist(file)
{
  If (FileExist(file) && !InStr(FileExist(file), "D"))
    MsgBox, file: %file% exists.
  Else
    MsgBox, file: %file% does NOT exist.
  Return
}

ShowFolderExist(folder)
{
  If InStr(FileExist(folder), "D")
    MsgBox, folder: %folder% exists.
  Else
    MsgBox, folder: %folder% does NOT exist.
  Return
}
```



## AWK

{{works with|gawk}}

```awk
@load "filefuncs"
BEGIN {
    exists("input.txt")
    exists("/input.txt")
    exists("docs")
    exists("/docs")
}

function exists(name    ,fd) {
    if ( stat(name, fd) == -1)
      print name " doesn't exist"
    else
      print name " exists"
}
```


Getline method. Also works in a Windows environment.

```AWK
BEGIN {
    exists("input.txt")
    exists("\\input.txt")
    exists("docs")
    exists("\\docs")
    exit(0)
}

#
# Check if file or directory exists, even 0-length file.
#   Return 0 if not exist, 1 if exist
#
function exists(file    ,line, msg)
{
        if ( (getline line < file) == -1 )
        {
                # "Permission denied" is for MS-Windows
                msg = (ERRNO ~ /Permission denied/ || ERRNO ~ /a directory/) ? 1 : 0
                close(file)
                return msg
        }
        else {
                close(file)
                return 1
        }
}
```


{{works with|gawk}}
Check file only (no directories)
<lang>gawk 'BEGINFILE{if (ERRNO) {print "Not exist."; exit} } {print "Exist."; exit}' input.txt
```



## Axe


```axe
If GetCalc("appvINPUT")
 Disp "EXISTS",i
Else
 Disp "DOES NOT EXIST",i
End
```



## BASIC

{{works with|QBasic}}

```qbasic

ON ERROR GOTO ohNo
f$ = "input.txt"
GOSUB opener
f$ = "\input.txt"
GOSUB opener

'can't directly check for directories,
'but can check for the NUL device in the desired dir
f$ = "docs\nul"
GOSUB opener
f$ = "\docs\nul"
GOSUB opener
END

opener:
    e$ = " found"
    OPEN f$ FOR INPUT AS 1
    PRINT f$; e$
    CLOSE
    RETURN

ohNo:
    IF (53 = ERR) OR (76 = ERR) THEN
        e$ = " not" + e$
    ELSE
        e$ = "Unknown error"
    END IF
    RESUME NEXT

```


You can also check for a directory by trying to enter it.


```qbasic

ON ERROR GOTO ohNo
d$ = "docs"
CHDIR d$
d$ = "\docs"
CHDIR d$
END

ohNo:
    IF 76 = ERR THEN
        PRINT d$; " not found"
    ELSE
        PRINT "Unknown error"
    END IF
    RESUME NEXT

```


{{works with|QuickBasic|7.1}} {{works with|PowerBASIC for DOS}}

Later versions of MS-compatible BASICs include the <CODE>DIR$</CODE> keyword, which makes this pretty trivial.


```qbasic

f$ = "input.txt"
GOSUB opener
f$ = "\input.txt"
GOSUB opener

'can't directly check for directories,
'but can check for the NUL device in the desired dir
f$ = "docs\nul"
GOSUB opener
f$ = "\docs\nul"
GOSUB opener
END

opener:
    d$ = DIR$(f$)
    IF LEN(d$) THEN
        PRINT f$; " found"
    ELSE
        PRINT f$; " not found"
    END IF
    RETURN

```


=
## BaCon
=

```freebasic
' File exists
f$ = "input.txt"
d$ = "docs"
IF FILEEXISTS(f$) THEN PRINT f$, " exists"
IF FILEEXISTS(d$) AND FILETYPE(d$) = 2 THEN PRINT d$, " directory exists"

f$ = "/" & f$
d$ = "/" & d$
PRINT f$, IIF$(FILEEXISTS(f$), " exists", " does not exist")
PRINT d$, IIF$(FILEEXISTS(d$) AND FILETYPE(d$) = 2, " is", " is not"), " a directory"

f$ = "empty.bac"
PRINT f$, IIF$(FILEEXISTS(f$), " exists", " does not exist")

f$ = "`Abdu'l-Bahá.txt"
PRINT f$, IIF$(FILEEXISTS(f$), " exists", " does not exist")
```


{{out}}

```txt
prompt$ ./fileexits
/input.txt does not exist
/docs is not a directory
empty.bac exists
`Abdu'l-Bahá.txt does not exist
```



=
## Commodore BASIC
=
Try a file, then check the error status of the disk drive. Error code 62 is the "File not found" error. The trick is to open the file without specifying the file type (PRG, SEQ, REL, etc.) and the Read/Write mode in the OPEN statement, otherwise you may end up with error code 64 "File Type Mismatch".

```gwbasic
10 REM CHECK FILE EXISTS
15 ER=0:EM$="":MSG$="FILE EXISTS."
20 PRINT CHR$(147);:REM CLEAR SCREEN
30 FI$="":INPUT "ENTER FILENAME TO CHECK";FI$:PRINT
35 IF FI$="" THEN PRINT "ABORTED.":END
40 OPEN 8,8,8,FI$
50 GOSUB 1000:REM FETCH ERROR STATUS FROM DRIVE: 0=OK, 62=FILE NOT FOUND
55 REM COMPARE ERROR NUMBER
60 IF ER<>0 THEN MSG$="I/O ERROR:"+STR$(ER)+" "+EM$
70 IF ER=62 THEN MSG$="'"+FI$+"' IS NOT HERE."
80 REM DO THINGS WITH FILE...
100 CLOSE 8
110 PRINT MSG$
120 PRINT:GOTO 30:REM REPEAT UNTIL EMPTY FILENAME IS ENTERED
1000 REM CHECK ERROR CHANNEL FOR STATUS OF LAST DISK OPERATION
1010 OPEN 15,8,15
1015 REM GET ERROR CODE, ERROR MESSAGE, TRACK, SECTOR
1020 INPUT#15,ER,EM$,T,S
1030 CLOSE 15
1040 RETURN
```


{{Out}}
```txt
ENTER FILENAME TO CHECK? INDEX.TXT

FILE EXISTS.

ENTER FILENAME TO CHECK? NOFILE.DOC
'NOFILE.DOC' IS NOT HERE.
```



## Batch File


```dos
if exist input.txt echo The following file called input.txt exists.
if exist \input.txt echo The following file called \input.txt exists.
if exist docs echo The following directory called docs exists.
if exist \docs\ echo The following directory called \docs\ exists.
```



## BBC BASIC


```bbcbasic
      test% = OPENIN("input.txt")
      IF test% THEN
        CLOSE #test%
        PRINT "File input.txt exists"
      ENDIF

      test% = OPENIN("\input.txt")
      IF test% THEN
        CLOSE #test%
        PRINT "File \input.txt exists"
      ENDIF

      test% = OPENIN("docs\NUL")
      IF test% THEN
        CLOSE #test%
        PRINT "Directory docs exists"
      ENDIF

      test% = OPENIN("\docs\NUL")
      IF test% THEN
        CLOSE #test%
        PRINT "Directory \docs exists"
      ENDIF
```



## C

{{libheader|POSIX}}

```c
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>

/* Check for regular file. */
int check_reg(const char *path) {
	struct stat sb;
	return stat(path, &sb) == 0 && S_ISREG(sb.st_mode);
}

/* Check for directory. */
int check_dir(const char *path) {
	struct stat sb;
	return stat(path, &sb) == 0 && S_ISDIR(sb.st_mode);
}

int main() {
	printf("input.txt is a regular file? %s\n",
	    check_reg("input.txt") ? "yes" : "no");
	printf("docs is a directory? %s\n",
	    check_dir("docs") ? "yes" : "no");
	printf("/input.txt is a regular file? %s\n",
	    check_reg("/input.txt") ? "yes" : "no");
	printf("/docs is a directory? %s\n",
	    check_dir("/docs") ? "yes" : "no");
	return 0;
}
```



## C++

{{libheader|boost}}

```cpp
#include "boost/filesystem.hpp"
#include <string>
#include <iostream>

void testfile(std::string name)
{
  boost::filesystem::path file(name);
  if (exists(file))
  {
    if (is_directory(file))
      std::cout << name << " is a directory.\n";
    else
      std::cout << name << " is a non-directory file.\n";
  }
  else
    std::cout << name << " does not exist.\n";
}

int main()
{
  testfile("input.txt");
  testfile("docs");
  testfile("/input.txt");
  testfile("/docs");
}
```


## C#


```c#
using System.IO;

Console.WriteLine(File.Exists("input.txt"));
Console.WriteLine(File.Exists("/input.txt"));
Console.WriteLine(Directory.Exists("docs"));
Console.WriteLine(Directory.Exists("/docs"));
```



## COBOL

{{works with|GnuCOBOL}} and other compilers with this system call extension

```COBOL
       identification division.
       program-id. check-file-exist.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 skip                 pic 9 value 2.
       01 file-name.
          05 value "/output.txt".
       01 dir-name.
          05 value "/docs/".
       01 unusual-name.
          05 value "Abdu'l-Bahá.txt".

       01 test-name            pic x(256).

       01 file-handle          usage binary-long.
       01 file-info.
          05 file-size         pic x(8) comp-x.
          05 file-date.
             10 file-day       pic x comp-x.
             10 file-month     pic x comp-x.
             10 file-year      pic xx comp-x.
          05 file-time.
             10 file-hours     pic x comp-x.
             10 file-minutes   pic x comp-x.
             10 file-seconds   pic x comp-x.
             10 file-hundredths  pic x comp-x.

       procedure division.
       files-main.

      *> check in current working dir
       move file-name(skip:) to test-name
       perform check-file

       move dir-name(skip:) to test-name
       perform check-file

       move unusual-name to test-name
       perform check-file

      *> check in root dir
       move 1 to skip
       move file-name(skip:) to test-name
       perform check-file

       move dir-name(skip:) to test-name
       perform check-file

       goback.

       check-file.
       call "CBL_CHECK_FILE_EXIST" using test-name file-info
       if return-code equal zero then
           display test-name(1:32) ": size " file-size ", "
                   file-year "-" file-month "-" file-day space
                   file-hours ":" file-minutes ":" file-seconds "."
                   file-hundredths
       else
           display "error: CBL_CHECK_FILE_EXIST " return-code space
                   trim(test-name)
       end-if
       .

       end program check-file-exist.

```

{{out}}

```txt

prompt$ cobc -xj check-file-exists.cob
output.txt                      : size 000000000000000000, 2016-06-01 09:27:14.00
docs/                           : size 000000000000004096, 2016-06-01 09:27:14.00
error: CBL_CHECK_FILE_EXIST +000000035 Abdu'l-Bahá.txt
error: CBL_CHECK_FILE_EXIST +000000035 /output.txt
error: CBL_CHECK_FILE_EXIST +000000035 /docs/
prompt$ echo -n >Abdu\'l-Bahá.txt
prompt$ cobc -xj check-file-exists.cob
output.txt                      : size 000000000000000000, 2016-06-01 09:27:14.00
docs/                           : size 000000000000004096, 2016-06-01 09:27:14.00
Abdu'l-Bahá.txt                 : size 000000000000000000, 2016-06-01 09:33:35.00
error: CBL_CHECK_FILE_EXIST +000000035 /output.txt
error: CBL_CHECK_FILE_EXIST +000000035 /docs/
```

Errors due to file and dir not existing in root directory for this test pass


## CoffeeScript

{{works with|Node.js}}

```coffeescript

fs = require 'fs'
path = require 'path'

root = path.resolve '/'
current_dir = __dirname
filename = 'input.txt'
dirname = 'docs'

local_file = path.join current_dir, filename
local_dir = path.join current_dir, dirname

root_file = path.join root, filename
root_dir = path.join root, dirname

for p in [ local_file, local_dir, root_file, root_dir ] then do ( p ) ->
    fs.exists p, ( p_exists ) ->
        unless p_exists
            console.log "#{ p } does not exist."
        else then fs.stat p, ( error, stat ) ->
            console.log "#{ p } exists and is a #{ if stat.isFile() then 'file' else then 'directory' }."



```



## Clojure


```clojure


(map #(.exists (clojure.java.io/as-file %)) '("/input.txt" "/docs" "./input.txt" "./docs"))


```



## Common Lisp


''probe-file'' returns ''nil'' if a file does not exist. ''directory'' returns ''nil'' if there are no files in a specified directory.

```lisp
(if (probe-file (make-pathname :name "input.txt"))
    (print "rel file exists"))
(if (probe-file (make-pathname :directory '(:absolute "") :name "input.txt"))
    (print "abs file exists"))

(if (directory (make-pathname :directory '(:relative "docs")))
    (print "rel directory exists")
    (print "rel directory is not known to exist"))
(if (directory (make-pathname :directory '(:absolute "docs")))
    (print "abs directory exists")
    (print "abs directory is not known to exist"))
```


There is no standardized way to determine if an empty directory exists, as Common Lisp dates from before the notion of directories as a type of file was near-universal. [http://www.weitz.de/cl-fad/ CL-FAD] provides many of the therefore-missing capabilities in a cross-implementation library.

{{libheader|CL-FAD}}

```lisp
(if (cl-fad:directory-exists-p (make-pathname :directory '(:relative "docs")))
    (print "rel directory exists")
    (print "rel directory does not exist"))
```



## D


```d
import std.stdio, std.file, std.path;

void verify(in string name) {
    if (name.exists())
        writeln("'", name, "' exists");
    else
        writeln("'", name, "' doesn't exist");
}

void main() {
    // check in current working dir
    verify("input.txt");
    verify("docs");

    // check in root
    verify(dirSeparator ~ "input.txt");
    verify(dirSeparator ~ "docs");
}
```

{{out}}

```txt
'input.txt' doesn't exist
'docs' doesn't exist
'\input.txt' doesn't exist
'\docs' doesn't exist
```


## DCL


```DCL
$ if f$search( "input.txt" ) .eqs. ""
$ then
$  write sys$output "input.txt not found"
$ else
$  write sys$output "input.txt found"
$ endif
$ if f$search( "docs.dir" ) .eqs. ""
$ then
$  write sys$output "directory docs not found"
$ else
$  write sys$output "directory docs found"
$ endif
$ if f$search( "[000000]input.txt" ) .eqs. ""
$ then
$  write sys$output "[000000]input.txt not found"
$ else
$  write sys$output "[000000]input.txt found"
$ endif
$ if f$search( "[000000]docs.dir" ) .eqs. ""
$ then
$  write sys$output "directory [000000]docs not found"
$ else
$  write sys$output "directory [000000]docs found"
$ endif
```

{{out}}

```txt

$ @check_that_file_exists
input.txt found
directory docs not found
[000000]input.txt not found
directory [000000]docs not found
```



## Delphi


```Delphi
program EnsureFileExists;

{$APPTYPE CONSOLE}

uses
  SysUtils;

begin
  if FileExists('input.txt') then
    Writeln('File "input.txt" exists.')
  else
    Writeln('File "input.txt" does not exist.');

  if FileExists('\input.txt') then
    Writeln('File "\input.txt" exists.')
  else
    Writeln('File "\input.txt" does not exist.');

  if DirectoryExists('docs') then
    Writeln('Directory "docs" exists.')
  else
    Writeln('Directory "docs" does not exists.');

  if DirectoryExists('\docs') then
    Writeln('Directory "\docs" exists.')
  else
    Writeln('Directory "\docs" does not exists.');
end.
```



## E


```e
for file in [<file:input.txt
,
             <file:///input.txt>] {
  require(file.exists(),       fn { `$file is missing!` })
  require(!file.isDirectory(), fn { `$file is a directory!` })
}

for file in [<file:docs>,
             <file:///docs>] {
  require(file.exists(),      fn { `$file is missing!` })
  require(file.isDirectory(), fn { `$file is not a directory!` })
}
```



## Elena

ELENA 4.x :

```elena
import system'io;
import extensions;

extension op
{
    validatePath()
        = self.Available.iif("exists","not found");
}

public program()
{
    console.printLine("input.txt file ",File.assign("input.txt").validatePath());

    console.printLine("\input.txt file ",File.assign("\input.txt").validatePath());

    console.printLine("docs directory ",Directory.assign("docs").validatePath());

    console.printLine("\docs directory ",Directory.assign("\docs").validatePath())
}
```



## Elixir


```Elixir
File.regular?("input.txt")
File.dir?("docs")
File.regular?("/input.txt")
File.dir?("/docs")
```



## Emacs Lisp


```Lisp
(file-exists-p "input.txt")
(file-directory-p "docs")
(file-exists-p "/input.txt")
(file-directory-p "/docs")
```


<code>file-exists-p</code> is true on both files and directories.  <code>file-directory-p</code> is true only on directories.  Both go through the <code>file-name-handler-alist</code> "magic filenames" mechanism so can act on remote files.  On MS-DOS generally both <code>/</code> and <code>\</code> work to specify the root directory.


## Erlang


```erlang
#!/usr/bin/escript
existence( true ) ->"exists";
existence( false ) ->"does not exist".

print_result(Type, Name, Flag) -> io:fwrite( "~s ~s ~s~n", [Type, Name, existence(Flag)] ).


main(_) ->
        print_result( "File", "input.txt", filelib:is_regular("input.txt") ),
        print_result( "Directory", "docs", filelib:is_dir("docs") ),
        print_result( "File", "/input.txt", filelib:is_regular("/input.txt") ),
        print_result( "Directory", "/docs", filelib:is_dir("/docs") ).

```



## Euphoria


```euphoria
include file.e

procedure ensure_exists(sequence name)
    object x
    sequence s
    x = dir(name)
    if sequence(x) then
        if find('d',x[1][D_ATTRIBUTES]) then
            s = "directory"
        else
            s = "file"
        end if
        printf(1,"%s %s exists.\n",{name,s})
    else
        printf(1,"%s does not exist.\n",{name})
    end if
end procedure

ensure_exists("input.txt")
ensure_exists("docs")
ensure_exists("/input.txt")
ensure_exists("/docs")
```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO
File.Exists("input.txt")
Directory.Exists("docs")
File.Exists("/input.txt")
Directory.Exists(@"\docs")
```



## Factor


```factor
: print-exists? ( path -- )
    [ write ": " write ] [ exists? "exists." "doesn't exist." ? print ] bi ;

{ "input.txt" "/input.txt" "docs" "/docs" } [ print-exists? ] each
```



## Forth



```forth
: .exists ( str len -- ) 2dup file-status nip 0= if type ."  exists" else type ."  does not exist" then ;
 s" input.txt" .exists
s" /input.txt" .exists
 s" docs" .exists
s" /docs" .exists
```



## Fortran

{{works with|Fortran|90 and later}}

Cannot check for directories in Fortran

```fortran
LOGICAL :: file_exists
INQUIRE(FILE="input.txt", EXIST=file_exists)   ! file_exists will be TRUE if the file
                                               ! exists and FALSE otherwise
INQUIRE(FILE="/input.txt", EXIST=file_exists)
```


Actually, f90,f95 are able to deal with directory staff:


```fortran
logical :: dir_e
! a trick to be sure docs is a dir
inquire( file="./docs/.", exist=dir_e )
if ( dir_e ) then
  write(*,*), "dir exists!"
else
  ! workaround: it calls an extern program...
  call system('mkdir docs')
end if
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Enable FileExists() function to be used
#include "file.bi"

' Use Win32 function to check if directory exists on Windows 10
Declare Function GetFileAttributes Lib "kernel32.dll" Alias "GetFileAttributesA" _
(ByVal lpFileName As ZString Ptr) As ULong

Const InvalidFileAttributes As ULong = -1UL
Const FileAttributeDirectory As ULong = &H10UL

Sub CheckFileExists(ByRef filePath As String)
  If FileExists(filePath) Then
    Print "'"; filePath; "' exists"
  Else
    Print "'"; filePath; "' does not exist"
  End If
End Sub

Sub CheckDirectoryExists(ByVal dirPath As ZString Ptr)
  Dim attrib As ULong = GetFileAttributes(dirPath)
  Dim dirExists As ULong = attrib <> InvalidFileAttributes AndAlso (attrib And FileAttributeDirectory) <> 0
  If dirExists Then
    Print "'"; *dirPath; "' exists"
  Else
    Print "'"; *dirPath; "' does not exist"
  End If
End Sub

CheckFileExists(CurDir + "\input.txt")
Dim dirPath As String = CurDir + "\docs"
CheckDirectoryExists(StrPtr(dirPath))
CheckFileExists("c:\input.txt")
CheckDirectoryExists(StrPtr("c:\docs"))
Print
Print "Press any key to quit the program"
Sleep
```


{{out}}
All files and directories were created first. The files are empty:

```txt

'c:\FreeBasic\input.txt' exists
'c:\FreeBasic\docs' exists
'c:\input.txt' exists
'c:\docs' exists

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=fa06b7cc43166fb0ab48e878d86e3d1b Click this link to run this code]'''

```gambas
Public Sub Main()

If Exist(User.Home &/ "input.txt") Then Print "'input.txt' does exist in the Home folder"
If Not Exist("/input.txt") Then Print "'input.txt' does NOT exist in Root"                'Not messing With my Root files

If Exist(User.home &/ "docs/") Then Print "The folder '~/docs' does exist"
If Not Exist("/docs/") Then Print "The folder '/docs' does NOT exist"                     'Not messing With my Root files

File.Save(User.Home &/ "`Abdu'l-Bahá.txt", "")
If Exist(User.Home &/ "`Abdu'l-Bahá.txt") Then Print "'`Abdu'l-Bahá.txt' does exist (zero length and unusual name)"

End
```

Output:

```txt

'input.txt' does exist in the Home folder
'input.txt' does NOT exist in Root
The folder '~/docs' does exist
The folder '/docs' does NOT exist
'`Abdu'l-Bahá.txt' does exist (zero length and unusual name)

```



## GAP


```gap
IsExistingFile("input.txt");
IsDirectoryPath("docs");
IsExistingFile("/input.txt");
IsDirectoryPath("/docs");
```



## Genie


```genie
[indent=4]
/*
   Check file exists, in Genie
   valac --pkg=gio-2.0 checkFile.gs
*/

init
    Intl.setlocale()

    files:array of string[] = {"input.txt", "docs",
    Path.DIR_SEPARATOR_S + "input.txt", Path.DIR_SEPARATOR_S + "docs", "`Abdu'l-Bahá.txt"}
    for f:string in files
        var file = File.new_for_path(f)
        var exists = file.query_exists()
        var dir = false
        if exists
            dir = file.query_file_type(0) == FileType.DIRECTORY
        print("%s %sexist%s%s", f, exists ? "" : "does not ", exists ? "s" : "", dir ? " and is a directory" : "")
```


{{out}}

```txt
prompt$ valac --pkg=gio-2.0 checkFile.gs
prompt$ ./checkFile
input.txt exists
docs exists and is a directory
/input.txt does not exist
/docs does not exist
`Abdu'l-Bahá.txt does not exist
```


For the run, ''input.txt'' was zero length.


## Go


```go
package main

import (
    "fmt"
    "os"
)

func printStat(p string) {
    switch i, err := os.Stat(p); {
    case err != nil:
        fmt.Println(err)
    case i.IsDir():
        fmt.Println(p, "is a directory")
    default:
        fmt.Println(p, "is a file")
    }
}

func main() {
    printStat("input.txt")
    printStat("/input.txt")
    printStat("docs")
    printStat("/docs")
}
```



## Groovy


```groovy
println new File('input.txt').exists()
println new File('/input.txt').exists()
println new File('docs').exists()
println new File('/docs').exists()
```



## Haskell



```haskell
import System.Directory (doesFileExist, doesDirectoryExist)

check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $
    s ++
    if result
      then " does exist"
      else " does not exist"

main :: IO ()
main = do
  check doesFileExist "input.txt"
  check doesDirectoryExist "docs"
  check doesFileExist "/input.txt"
  check doesDirectoryExist "/docs"
```



## hexiscript


```hexiscript
println "File \"input.txt\"? " + (exists "input.txt")
println "Dir \"docs\"?       " + (exists "docs/")
println "File \"/input.txt\"? " + (exists "/input.txt")
println "Dir \"/docs\"?       " + (exists "/docs/")
```



## HicEst


```hicest
   OPEN(FIle=   'input.txt', OLD, IOStat=ios, ERror=99)
   OPEN(FIle='C:\input.txt', OLD, IOStat=ios, ERror=99)
! ...
99 WRITE(Messagebox='!') 'File does not exist. Error message ', ios
```



## i


```i
concept exists(path) {
	open(path)
	errors {
		if error.DoesNotExist()
			print(path, " does not exist!")
		end
		return
	}
	print(path, " exists!")
}

software {
	exists("input.txt")
	exists("/input.txt")
	exists("docs")
	exists("/docs")
	exists("docs/Abdu'l-Bahá.txt")
}
```


=={{header|Icon}} and {{header|Unicon}}==
Icon doesn't support 'stat'; however, information can be obtained by use of the system function to access command line.

```Unicon
every dir := !["./","/"] do {
   write("file ", f := dir || "input.txt", if stat(f) then " exists." else " doesn't exist.")
   write("directory ", f := dir || "docs", if stat(f) then " exists." else " doesn't exist.")
   }
```

Note: Icon and Unicon accept both / and \ for directory separators.


## HolyC


```holyc
U0 FileExists(U8 *f) {
  if (FileFind(f) && !IsDir(f)) {
    Print("'%s' file exists.\n", f);
  } else {
    Print("'%s' file does not exist.\n", f);
  }
}

U0 DirExists(U8 *d) {
  if (IsDir(d)) {
    Print("'%s' directory exists.\n", d);
  } else {
    Print("'%s' directory does not exist.\n", d);
  }
}

FileExists("input.txt");
FileExists("::/input.txt");
DirExists("docs");
DirExists("::/docs");
```



## IDL


```idl

print, FILE_TEST('input.txt')
print, FILE_TEST(PATH_SEP()+'input.txt')
print, FILE_TEST('docs', /DIRECTORY)
print, FILE_TEST(PATH_SEP()+'docs', /DIRECTORY)


```



## J


```j
require 'files'
fexist 'input.txt'
fexist '/input.txt'
direxist=: 2 = ftype
direxist 'docs'
direxist '/docs'
```



## Java


```java
import java.io.File;
public class FileExistsTest {
   public static boolean isFileExists(String filename) {
       boolean exists = new File(filename).exists();
       return exists;
   }
   public static void test(String type, String filename) {
       System.out.println("The following " + type + " called " + filename +
           (isFileExists(filename) ? " exists." : " not exists.")
       );
   }
   public static void main(String args[]) {
        test("file", "input.txt");
        test("file", File.separator + "input.txt");
        test("directory", "docs");
        test("directory", File.separator + "docs" + File.separator);
   }
}
```

{{works with|Java|7+}}

```java5
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
public class FileExistsTest{
   private static FileSystem defaultFS = FileSystems.getDefault();
   public static boolean isFileExists(String filename){
       return Files.exists(defaultFS.getPath(filename));
   }
   public static void test(String type, String filename){
       System.out.println("The following " + type + " called " + filename +
           (isFileExists(filename) ? " exists." : " not exists.")
       );
   }
   public static void main(String args[]){
        test("file", "input.txt");
        test("file", defaultFS.getSeparator() + "input.txt");
        test("directory", "docs");
        test("directory", defaultFS.getSeparator() + "docs" + defaultFS.getSeparator());
   }
}
```

Non retarded version:

```java5

import java.nio.file.Files;
public class FileExistsTest{
   public static void main(String args[]){
        System.out.printf("input.txt - %s", new File("input.txt").exists());
   }

```



## JavaScript

Javascript interpreters are now widely embedded in contexts which do have access to file systems, but the early context of browser scripting has precluded the inclusion of file system libraries in the definition of the language itself.
Each non-browser JS context is likely to have its own home-grown and unstandardised file system library.

### JScript


```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");

fso.FileExists('input.txt');
fso.FileExists('c:/input.txt');
fso.FolderExists('docs');
fso.FolderExists('c:/docs');
```



### macOS JavaScript for Automation


### =ES6: Sierra onwards=

{{Trans|Haskell}}
(Adopting function names used in the Haskell System.Directory library)

```JavaScript
(() => {

    // SYSTEM DIRECTORY FUNCTIONS
    // FOR MAC OS 'JAVASCRIPT FOR AUTOMATION' SCRIPTING -----------------------

    // doesDirectoryExist :: String -> IO Bool
    const doesDirectoryExist = strPath => {
        const
            dm = $.NSFileManager.defaultManager,
            ref = Ref();
        return dm
            .fileExistsAtPathIsDirectory(
                $(strPath)
                .stringByStandardizingPath, ref
            ) && ref[0] === 1;
    };

    // doesFileExist :: String -> Bool
    const doesFileExist = strPath => {
        var error = $();
        return (
            $.NSFileManager.defaultManager
            .attributesOfItemAtPathError(
                $(strPath)
                .stringByStandardizingPath,
                error
            ),
            error.code === undefined
        );
    };

    // getCurrentDirectory :: String
    const getCurrentDirectory = () =>
        ObjC.unwrap($.NSFileManager.defaultManager.currentDirectoryPath);

    // getFinderDirectory :: String
    const getFinderDirectory = () =>
        Application('Finder')
        .insertionLocation()
        .url()
        .slice(7);

    // getHomeDirectory :: String
    const getHomeDirectory = () =>
        ObjC.unwrap($.NSHomeDirectory());

    // setCurrentDirectory :: String -> IO ()
    const setCurrentDirectory = strPath =>
        $.NSFileManager.defaultManager
        .changeCurrentDirectoryPath(
            $(strPath)
            .stringByStandardizingPath
        );

    // GENERIC FUNCTIONS FOR THE TEST -----------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // TEST -------------------------------------------------------------------
    return (
        setCurrentDirectory('~/Desktop'),
        show(ap(
            [doesFileExist, doesDirectoryExist],
            ['input.txt', '/input.txt', 'docs', '/docs']
        ))
    );
})();
```

{{Out}}
The first four booleans are returned by doesFileExist – the last four by
doesDirectoryExist, which returns false in the case of files which do
exist but are not directories.

```txt
[
  true,
  true,
  true,
  true,
  false,
  false,
  true,
  true
]
```



## Julia

{{works with|Julia|0.6}}


```julia
@show isfile("input.txt")
@show isfile("/input.txt")
@show isdir("docs")
@show isdir("/docs")
@show isfile("")
@show isfile("`Abdu'l-Bahá.txt")
```



## Kotlin


```scala
// version 1.0.6

import java.io.File

fun main(args: Array<String>) {
    val filePaths = arrayOf("input.txt", "c:\\input.txt", "zero_length.txt", "`Abdu'l-Bahá.txt")
    val dirPaths  = arrayOf("docs", "c:\\docs")
    for (filePath in filePaths) {
        val f = File(filePath)
        println("$filePath ${if (f.exists() && !f.isDirectory) "exists" else "does not exist"}")
    }
    for (dirPath in dirPaths) {
        val d = File(dirPath)
        println("$dirPath ${if (d.exists() && d.isDirectory) "exists" else "does not exist"}")
    }
}
```



## LabVIEW

{{libheader|LabVIEW CWD}}
{{VI snippet}}<br/>
[[File:Ensure_that_a_file_exists.png]]



## Lasso


```Lasso
// local file
file_exists('input.txt')

// local directory
file_exists('docs')

// file in root file system (requires permissions at user OS level)
file_exists('//input.txt')

// directory in root file system (requires permissions at user OS level)
file_exists('//docs')
```



## LFE

From the LFE REPL:

```lisp

> (: filelib is_regular '"input.txt")
false
> (: filelib is_dir '"docs")
false
> (: filelib is_regular '"/input.txt")
false
> (: filelib is_dir '"/docs"))
false

```



## Liberty BASIC


```lb
'fileExists.bas - Show how to determine if a file exists
dim info$(10,10)
input "Type a file path (ie. c:\windows\somefile.txt)?"; fpath$
if fileExists(fpath$) then
    print fpath$; " exists!"
else
    print fpath$; " doesn't exist!"
end if
end

'return a true if the file in fullPath$ exists, else return false
function fileExists(fullPath$)
    files pathOnly$(fullPath$), filenameOnly$(fullPath$), info$()
    fileExists = val(info$(0, 0)) > 0
end function

'return just the directory path from a full file path
function pathOnly$(fullPath$)
    pathOnly$ = fullPath$
    while right$(pathOnly$, 1) <> "\" and pathOnly$ <> ""
        pathOnly$ = left$(pathOnly$, len(pathOnly$)-1)
    wend
end function

'return just the filename from a full file path
function filenameOnly$(fullPath$)
    pathLength = len(pathOnly$(fullPath$))
    filenameOnly$ = right$(fullPath$, len(fullPath$)-pathLength)
end function
```



## Little


```C
if (exists("input.txt")) {
    puts("The file \"input.txt\" exist");
}
if (exists("/input.txt")) {
    puts("The file \"/input.txt\" exist");
}
if (exists("docs")) {
    puts("The file \"docs\" exist");
}
if (exists("/docs")) {
    puts("The file \"/docs\" exist");
}
```



## LiveCode


```LiveCode
there is a file "/input.txt"
there is a file "input.txt"
there is a folder "docs"
there is a file "/docs/input.txt"
```

LiveCode also allows setting a default folder for subsequent file commands. To check if a file exists in the doc folder

```LiveCode
set the defaultFolder to "docs"
there is a file "input.txt"
```



## Logo

{{works with|UCB Logo}}

```logo
show file? "input.txt
show file? "/input.txt
show file? "docs
show file? "/docs
```

Alternatively, one can set a file prefix used for subsequent file commands.

```logo
setprefix "/
show file? "input.txt
```



## Lua

For directories, the following only works on platforms on which directories can be opened for reading like files.

```lua
function output( s, b )
    if b then
        print ( s, " does not exist." )
    else
        print ( s, " does exist." )
    end
end

output( "input.txt",  io.open( "input.txt", "r" ) == nil )
output( "/input.txt", io.open( "/input.txt", "r" ) == nil )
output( "docs",  io.open( "docs", "r" ) == nil )
output( "/docs", io.open( "/docs", "r" ) == nil )
```


The following more portable solution uses LuaFileSystem.

```lua
require "lfs"
for i, path in ipairs({"input.txt", "/input.txt", "docs", "/docs"}) do
    local mode = lfs.attributes(path, "mode")
    if mode then
        print(path .. " exists and is a " .. mode .. ".")
    else
        print(path .. " does not exist.")
    end
end
```


## M2000 Interpreter

Report print proportional text using word wrap, and justification. Can be used to calculate lines, and to render form a line, a number of lines. We can specify the width of the text, and by moving the cursor horizontal we can specify the left margin. This statement can be used to any layer, including user forms and printer page.


```M2000 Interpreter

Module ExistDirAndFile {
      Let WorkingDir$=Dir$, RootDir$="C:\"

      task(WorkingDir$)
      task(RootDir$)
      Dir User ' return to user directroy

      Sub task(WorkingDir$)
            Local counter
            Dir WorkingDir$
            If Not Exist.Dir("docs") then Report  "docs not exist in "+WorkingDir$ : counter++
            If Not Exist("output.txt") Then {
                  Report "output.txt not exist in "+ WorkingDir$ : counter++
            } Else.if Filelen("output.txt")=0 Then Report "output.txt has zero length"
            If counter =0 then Report  WorkingDir$+ " has docs directory and file output.txt"
      End Sub
}
ExistDirAndFile

```



## Maple


```Maple
with(FileTools):
Exists("input.txt");
Exists("docs") and IsDirectory("docs");
Exists("/input.txt");
Exists("/docs") and IsDirectory("/docs");
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
wd = NotebookDirectory[];
FileExistsQ[wd <> "input.txt"]
DirectoryQ[wd <> "docs"]

FileExistsQ["/" <> "input.txt"]
DirectoryQ["/" <> "docs"]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
 exist('input.txt','file')
 exist('/input.txt','file')
 exist('docs','dir')
 exist('/docs','dir')
```



## MAXScript


```maxscript
-- Here
doesFileExist "input.txt"
(getDirectories "docs").count == 1
-- Root
doesFileExist "\input.txt"
(getDirectories "C:\docs").count == 1
```


=={{header|Modula-3}}==

```modula3
MODULE FileTest EXPORTS Main;

IMPORT IO, Fmt, FS, File, OSError, Pathname;

PROCEDURE FileExists(file: Pathname.T): BOOLEAN =
  VAR status: File.Status;
  BEGIN
    TRY
      status := FS.Status(file);
      RETURN TRUE;
    EXCEPT
    | OSError.E => RETURN FALSE;
    END;
  END FileExists;

BEGIN
  IO.Put(Fmt.Bool(FileExists("input.txt")) & "\n");
  IO.Put(Fmt.Bool(FileExists("/input.txt")) & "\n");
  IO.Put(Fmt.Bool(FileExists("docs/")) & "\n");
  IO.Put(Fmt.Bool(FileExists("/docs")) & "\n");
END FileTest.
```



## Neko


```ActionScript
/**
 Check that file/dir exists, in Neko
*/

var sys_exists = $loader.loadprim("std@sys_exists", 1)
var sys_file_type = $loader.loadprim("std@sys_file_type", 1)
var sys_command = $loader.loadprim("std@sys_command", 1)

var name = "input.txt"
$print(name, " exists as file: ", sys_exists(name), "\n")

$print(name = "docs", " exists as dir: ", sys_exists(name) && sys_file_type(name) == "dir", "\n")
$print(name = "neko", " exists as dir: ", sys_exists(name) && sys_file_type(name) == "dir", "\n")

$print(name = "/input.txt", " exists as file: ", sys_exists(name) && sys_file_type(name) == "file", "\n")
$print(name = "/docs", " exists as dir: ", sys_exists(name) && sys_file_type(name) == "dir", "\n")
$print(name = "/tmp", " exists as dir: ", sys_exists(name) && sys_file_type(name) == "dir", "\n")

/* bonus round */
name = "empty.txt"
var stat_size = $loader.loadprim("std@sys_stat", 1)(name).size
$print(name, " exists as empty file: ", sys_exists(name) && stat_size == 0, "\n")

name = "`Abdu'l-Bahá.txt"
$print(name, " exists as file: ", sys_exists(name) && sys_file_type(name) == "file", "\n")
```


{{out}}

```txt
prompt$ nekoc exists.neko
prompt$ neko exists.n
input.txt exists as file: true
docs exists as dir: false
neko exists as dir: true
/input.txt exists as file: false
/docs exists as dir: false
/tmp exists as dir: true
empty.txt exists as empty file: true
`Abdu'l-Bahá.txt exists as file: true
```



## Nemerle

{{trans|C#}}

```Nemerle
using System.Console;
using System.IO;

WriteLine(File.Exists("input.txt"));
WriteLine(File.Exists("/input.txt"));
WriteLine(Directory.Exists("docs"));
WriteLine(Directory.Exists("/docs"));
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method isExistingFile(fn) public static returns boolean
  ff = File(fn)
  fExists = ff.exists() & ff.isFile()
  return fExists

-- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
method isExistingDirectory(fn) public static returns boolean
  ff = File(fn)
  fExists = ff.exists() & ff.isDirectory()
  return fExists

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg files
  if files = '' then files = 'input.txt F docs D /input.txt F /docs D'
  loop while files.length > 0
    parse files fn ft files
    select case(ft.upper())
      when 'F' then do
        if isExistingFile(fn) then ex = 'exists'
        else                       ex = 'does not exist'
        say 'File '''fn'''' ex
        end
      when 'D' then do
        if isExistingDirectory(fn) then ex = 'exists'
        else                            ex = 'does not exist'
        say 'Directory '''fn'''' ex
        end
      otherwise do
        if isExistingFile(fn) then ex = 'exists'
        else                       ex = 'does not exist'
        say 'File '''fn'''' ex
        end
      end
    end

  return

```



## NewLISP


```NewLISP
(dolist (file '("input.txt" "/input.txt"))
  (if (file? file true)
      (println "file " file " exists")))

(dolist (dir '("docs" "/docs"))
  (if (directory? dir)
      (println "directory " dir " exists")))
```



## Nim


```nim
import os

echo existsFile "input.txt"
echo existsFile "/input.txt"
echo existsDir "docs"
echo existsDir "/docs"
```


=={{header|Objective-C}}==

```objc
NSFileManager *fm = [NSFileManager defaultManager];
NSLog(@"input.txt %s", [fm fileExistsAtPath:@"input.txt"] ? @"exists" : @"doesn't exist");
NSLog(@"docs %s", [fm fileExistsAtPath:@"docs"] ? @"exists" : @"doesn't exist");
```



## Objeck


```objeck

use IO;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      File->Exists("input.txt")->PrintLine();
      File->Exists("/input.txt")->PrintLine();
      Directory->Exists("docs")->PrintLine();
      Directory->Exists("/docs")->PrintLine();
    }
}

```



## OCaml


```ocaml
Sys.file_exists "input.txt";;
Sys.file_exists "docs";;
Sys.file_exists "/input.txt";;
Sys.file_exists "/docs";;
```



## ooRexx


```oorexx
/**********************************************************************
* exists(filespec)
* returns 1   if filespec identifies a file with size>0
*                (a file of size 0 is deemed not to exist.)
*             or a directory
*         0   otherwise
* 09.06.2013 Walter Pachl (retrieved from my toolset)
**********************************************************************/
exists:
  parse arg spec
  call sysfiletree spec, 'LIST', 'BL'
  if list.0\=1 then return 0        -- does not exist
  parse var list.1 . . size flags .
  if size>0 then return 1           -- real file
  If substr(flags,2,1)='D' Then Do
    Say spec 'is a directory'
    Return 1
    End
  If size=0 Then Say spec 'is a zero-size file'
  Return 0
```



## Oz


```oz
declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
in
  {Show {Path.exists "docs"}}
  {Show {Path.exists "input.txt"}}
  {Show {Path.exists "/docs"}}
  {Show {Path.exists "/input.txt"}}
```



## PARI/GP


```parigp
trap(,"does not exist",read("input.txt");"exists")
trap(,"does not exist",read("c:\\input.txt");"exists")
trap(,"does not exist",read("c:\\dirname\\nul");"exists")
```


A better version would use <code>externstr</code>.

Under PARI it would typically be more convenient to use [[#C|C]] methods.


## Pascal

See [[Ensure_that_a_file_exists#Delphi | Delphi]]


## Phix


```Phix
constant fd = {"file","directory"}

procedure check(string name)
object d = dir(name)
    if sequence(d) then
        d = (find('d',d[1][D_ATTRIBUTES])!=0)
        printf(1,"%s %s exists.\n",{fd[1+d],name})
    else
        printf(1,"%s does not exist.\n",{name})
    end if
end procedure

check("input.txt")
check("docs")
check("/input.txt")
check("/docs")
check("/pagefile.sys")
check("/Program Files (x86)")
```

{{out}}

```txt

input.txt does not exist.
directory docs exists.
/input.txt does not exist.
/docs does not exist.
file /pagefile.sys exists.
directory /Program Files (x86) exists.

```



## PHP


```php
if (file_exists('input.txt')) echo 'input.txt is here right by my side';
if (file_exists('docs'     )) echo 'docs is here with me';
if (file_exists('/input.txt')) echo 'input.txt is over there in the root dir';
if (file_exists('/docs'    )) echo 'docs is over there in the root dir';
```



## Perl


```perl
use File::Spec::Functions qw(catfile rootdir);
# here
print -e 'input.txt';
print -d 'docs';
# root dir
print -e catfile rootdir, 'input.txt';
print -d catfile rootdir, 'docs';
```


'''Without a Perl Module'''

A '''1''' is printed if the file or dir exists.
 perl -e 'print -e "./input.txt", "\n";'
 perl -e 'print -d "./docs", "\n";'
 perl -e 'print -e "/input.txt", "\n";'
 perl -e 'print -d "/docs", "\n";'

## Perl 6


```perl6

my $path = "/etc/passwd";
say $path.IO.e ?? "Exists" !! "Does not exist";

given $path.IO {
    when :d { say "$path is a directory"; }
    when :f { say "$path is a regular file"; }
    when :e { say "$path is neither a directory nor a file, but it does exist"; }
    default { say "$path does not exist" }
}


```


<code>when</code> internally uses the smart match operator <code>~~</code>, so <code>when :e</code> really does <code>$given ~~ :e</code> instead of the method call <code>$given.e</code>; both test whether the file exists.


```perl6

run ('touch', "♥ Unicode.txt");

say "♥ Unicode.txt".IO.e;      # "True"
say "♥ Unicode.txt".IO ~~ :e;  # same

```



## PicoLisp


```PicoLisp
(if (info "file.txt")
   (prinl "Size: " (car @) " bytes, last modified " (stamp (cadr @) (cddr @)))
   (prinl "File doesn't exist") )

# for directory existing
# Nehal-Singhal 2018-05-25

(if (info "./docs")
  (print 'exists)
  (print 'doesNotExist)))

# To verify if it's really a directory, (CAR of return value will be 'T').
# abu 2018-05-25

  (let I (info "./docs")
      (prinl
         (nond
            (I "Does not exist")
            ((=T (car I)) "Is not a directory")
            (NIL "Directory exists") ) ) )

```



## Pike


```pike
import Stdio;

int main(){
   if(exist("/var")){
      write("/var exists!\n");
   }

   if(exist("file-exists.pike")){
      write("I exist!\n");
   }
}
```



## PL/I


```pli
*Process source or(!);
 /*********************************************************************
 * 20.10.2013 Walter Pachl
 * 'set dd:f=d:\_l\xxx.txt,recsize(300)'
 * 'tex'
 *********************************************************************/
 tex: Proc Options(main);
 Dcl fid Char(30) Var Init('D:\_l\tst.txt');
 Dcl xxx Char(30) Var Init('D:\_l\nix.txt');
 Dcl r   Char(1000) Var;
 Dcl f Record input;
 On Undefinedfile(f) Goto label;
 Open File(f) Title('/'!!fid);
 Read file(f) Into(r);
 Put Skip List('First line of file '!!fid!!': '!!r);
 Close File(f);
 Open File(f) Title('/'!!xxx);
 Read file(f) Into(r);
 Put Skip List(r);
 Close File(f);
 Label: Put Skip List('File '!!xxx!!' not found');
 End;
```

{{out}}

```txt

First line of file D:\_l\tst.txt: Test line 1
File D:\_l\nix.txt not found

```



## Pop11



```pop11
sys_file_exists('input.txt') =>
sys_file_exists('/input.txt') =>
sys_file_exists('docs') =>
sys_file_exists('/docs') =>
```


Note that the above literally checks for existence. Namely sys_file_exists returns true if file exists but can not be read.

The only sure method to check if file can be read is to try to open it. If one just wants to check if file is readable the following may be useful:


```pop11
;;; Define an auxilary function, returns boolean
define file_readable(fname);
   lvars f = sysopen(fname, 0, true, `A`);
   if f then
       sysclose(f);
       return (true);
   else
       return (false);
   endif;
enddefine;
```


The above works but is not the only way or the best way to check status of a file in Pop11. There is a very general procedure sys_file_stat that allows interrogation of a file or directory. The full documentation can be seen in the online documentation (search for sys_file_stat):

http://wwwcgi.rdg.ac.uk:8081/cgi-bin/cgiwrap/wsi14/poplog/pop11/ref/sysio

http://www.poplog.org/docs/popdocs/pop11/ref/sysio

http://www.cs.bham.ac.uk/research/projects/poplog/doc/popref/sysio
(Not so well formatted).

Users can easily define special cases of the general procedure.


## PowerShell



```powershell
 Test-Path input.txt
```




## Prolog


{{works with|SWI-Prolog|6.6}}


```prolog


exists_file('input.txt'),
exists_directory('docs').

exits_file('/input.txt'),
exists_directory('/docs').


```



## PureBasic


```PureBasic
result = ReadFile(#PB_Any, "input.txt")
If result>0 : Debug "this local file exists"
  Else : Debug "result=" +Str(result) +" so this local file is missing"
EndIf

result = ReadFile(#PB_Any, "/input.txt")
If result>0 : Debug "this root file exists"
  Else : Debug "result=" +Str(result) +" so this root file is missing"
EndIf

result = ExamineDirectory(#PB_Any,"docs","")
If result>0 : Debug "this local directory exists"
  Else : Debug "result=" +Str(result) +" so this local directory is missing"
EndIf

result = ExamineDirectory(#PB_Any,"/docs","")
If result>0 : Debug "this root directory exists"
  Else : Debug "result=" +Str(result) +" so this root directory is missing"
EndIf
```



## Python



```python
import os

os.path.isfile("input.txt")
os.path.isfile("/input.txt")
os.path.isdir("docs")
os.path.isdir("/docs")
```


The more generic [https://docs.python.org/3/library/os.path.html#os.path.exists <code>os.path.exists(path)</code>] function will return True if the path exists, being it either a regular file or a directory.


## R


```R
file.exists("input.txt")
file.exists("/input.txt")
file.exists("docs")
file.exists("/docs")

# or
file.exists("input.txt", "/input.txt", "docs", "/docs")
```


The function <tt>file.exists</tt> returns a logical value (or a vector of logical values if more than one argument is passed)


## Racket



```Racket

#lang racket

;; here
(file-exists? "input.txt")
(file-exists? "docs")

;; in the root
(file-exists? "/input.txt")
(file-exists? "/docs")

;; or in the root with relative paths
(parameterize ([current-directory "/"])
  (and (file-exists? "input.txt")
       (file-exists? "docs")))

```



## Raven



```raven
'input.txt'  exists if 'input.txt exists'  print
'/input.txt' exists if '/input.txt exists' print
'docs'  isdir if 'docs exists and is a directory'  print
'/docs' isdir if '/docs exists and is a directory' print
```



## REBOL


```REBOL
exists? %input.txt
exists? %docs/

exists? %/input.txt
exists? %/docs/
```



## Red


```Red
exists? %input.txt
exists? %docs/
exists? %/c/input.txt
exists? %/c/docs/
exists? %//input.txt
exists? %//docs/

>> exists? %`Abdu'l-Bahá.txt
== true
```



## REXX


### version 1

{{works with|PC/REXX}}
{{works with|Personal REXX}}
{{works with|Regina}}

```rexx
/*REXX program creates a new empty file and directory in current directory and root dir.*/
fn= 'input.txt'                                  /*default name of a file.              */
dn= 'docs'                                       /*default name of a directory (folder).*/
@.1= 'current directory';  @.2= 'root directory' /*messages used to indicate which pass.*/
parse upper version v                            /*obtain name of the REXX being used.  */
regina= pos('REGINA'  , v)\==0                   /*is this the Regina REXX being used?  */
r4    = pos('REXX-R4' , v)\==0                   /*is this the R4     REXX being used?  */
@doesnt= "doesn't exist in the"
@does  = "does exist in the"

 do j=1  for 2;    say                           /* [↑]  perform these statements twice.*/
 if stream(fn, 'C', "QUERY EXISTS")==''  then say 'file '       fn   @doesnt   @.j
                                         else say 'file '       fn   @does     @.j

 if j==2  then iterate
 if stream(dn, 'C', "QUERY EXISTS")==''  then say 'directory'   dn   @doesnt   @.j
                                         else say 'directory'   dn   @does     @.j
 if j==1  then select
               when regina  then call chdir    '\'    /*use Regina's version of  CHDIR. */
               when r4      then call stream   '\', "C", 'CHDIR'        /*R4's version. */
               otherwise         call doschdir '\'    /*PC/REXX & Personal REXX version.*/
               end   /*select*/
 end   /*j*/                                     /*stick a fork in it,  we're all done. */
```



### version 2

{{works with|ARexx}}
{{works with|Regina 3.8 and later, with options:   AREXX_BIFS   AREXX_SEMANTICS}}

```rexx

/* Check if a file already exists */
filename='file.txt'
IF ~Openfile(filename) THEN CALL Openfile(':'filename)
EXIT 0
Openfile:
IF ~Exists(filename) THEN RETURN 0
CALL Open(filehandle,filename,'APPEND')
RETURN 1

```



## Ring


```ring

aFile = "C:\Ring\ReadMe.txt"
see aFile
if Fexists(aFile) see " exists" + nl
else see " doesn't exist" + nl ok

```



## RLaB


RLaB provides two user functions for the task, ''isfile'' and ''isdir''.

```RLaB

>> isdir("docs")
  0
>> isfile("input.txt")
  0

```



## Ruby

<code>File.exists?</code> only checks if a file exists; it can be a regular file, a directory, or something else. <code>File.file?</code> or <code>File.directory?</code> checks for a regular file or a directory. Ruby also allows <code>FileTest.file?</code> or <code>FileTest.directory?</code>.


```ruby
File.file?("input.txt")
File.file?("/input.txt")
File.directory?("docs")
File.directory?("/docs")
```


The next program runs all four checks and prints the results.


```ruby
["input.txt", "/input.txt"].each { |f|
  printf "%s is a regular file? %s\n", f, File.file?(f) }
["docs", "/docs"].each { |d|
  printf "%s is a directory? %s\n", d, File.directory?(d) }
```




## Run BASIC


```runbasic
files #f,"input.txt"
if #f hasanswer() = 1 then print "File does not exist"
files #f,"docs"
if #f hasanswer() = 1 then print "File does not exist"
if #f isDir()     = 0 then print "This is a directory"

```



## Rust


```rust
use std::fs;

fn main() {
    for file in ["input.txt", "docs", "/input.txt", "/docs"].iter() {
        match fs::metadata(file) {
            Ok(attr) => {
                if attr.is_dir() {
                    println!("{} is a directory", file);
                }else {
                    println!("{} is a file", file);
                }
            },
            Err(_) => {
                println!("{} does not exist", file);
            }
        };
    }
}

```



## Scala

{{libheader|Scala}}
```scala
import java.nio.file.{ Files, FileSystems }

object FileExistsTest extends App {

  val defaultFS = FileSystems.getDefault()
  val separator = defaultFS.getSeparator()

  def test(filename: String) {
    val path = defaultFS.getPath(filename)

    println(s"The following ${if (Files.isDirectory(path)) "directory" else "file"} called $filename" +
      (if (Files.exists(path)) " exists." else " not exists."))
  }

  // main
  List("output.txt", separator + "output.txt", "docs", separator + "docs" + separator).foreach(test)
}
```



## Scheme

{{works with|Scheme|R6RS}}[http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-10.html]

```scheme
(file-exists? filename)
```



## Seed7



```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(fileType("input.txt") = FILE_REGULAR);
    writeln(fileType("/input.txt") = FILE_REGULAR);
    writeln(fileType("docs") = FILE_DIR);
    writeln(fileType("/docs") = FILE_DIR);
  end func;
```



## Sidef


```ruby
# Here
say (Dir.cwd  + %f'input.txt' -> is_file);
say (Dir.cwd  + %d'docs'      -> is_dir);

# Root
say (Dir.root + %f'input.txt' -> is_file);
say (Dir.root + %d'docs'      -> is_dir);
```

NOTE: To check only for existence, use the method ''exists''


## Slate


```slate
(File newNamed: 'input.txt') exists
(File newNamed: '/input.txt') exists
(Directory root / 'input.txt') exists
(Directory newNamed: 'docs') exists
(Directory newNamed: '/docs') exists
```



## Smalltalk


[[Squeak]] has no notion of 'current directory' because it isn't tied to the shell that created it.


```smalltalk
FileDirectory new fileExists: 'c:\serial'.
(FileDirectory on: 'c:\') directoryExists: 'docs'.
```


In [[GNU Smalltalk]] instead you can do:


```smalltalk
(Directory name: 'docs') exists ifTrue: [ ... ]
(Directory name: 'c:\docs') exists ifTrue: [ ... ]
(File name: 'serial') isFile ifTrue: [ ... ]
(File name: 'c:\serial') isFile ifTrue: [ ... ]
```


Using ''exists'' in the third and fourth case will return true for directories too.


## Standard ML


```sml
OS.FileSys.access ("input.txt", []);
OS.FileSys.access ("docs", []);
OS.FileSys.access ("/input.txt", []);
OS.FileSys.access ("/docs", []);
```



## Stata

Mata has functions to check the existence of files and directories:

```stata
mata
fileexists("input.txt")
direxists("docs")
end
```


It's not as straightforward in Stata's macro language. For files, use [http://www.stata.com/help.cgi?confirm confirm]. Since it throws an error when the file does not exist, use [http://www.stata.com/help.cgi?capture capture] and check [http://www.stata.com/help.cgi?_variables _rc] afterwards.


```stata
capture confirm file input.txt
if !_rc {
	* do something if the file exists
}
```


It's not possible to check existence of a directory with confirm. One may use the [https://ideas.repec.org/c/boc/bocode/s435507.html confirmdir] package from SSC. The confirmdir command saves the current directory, then tries to chdir to the directory to test (with capture to prevent an error). Then the value of _rc is put in a [http://www.stata.com/help.cgi?return stored result]. Example of use:


```stata
confirmdir docs
if !`r(confirmdir)' {
	* do something if the directory exists
}
```



## Tcl

Taking the meaning of the task from the DOS example: <!-- multiline “if” because of formatting -->

```tcl
if { [file exists "input.txt"] } {
    puts "input.txt exists"
}

if { [file exists [file nativename "/input.txt"]] } {
    puts "/input.txt exists"
}

if { [file isdirectory "docs"] } {
    puts "docs exists and is a directory"
}

if { [file isdirectory [file nativename "/docs"]] } {
    puts "/docs exists and is a directory"
}
```

Note that these operations do not require the use of <tt>file nativename</tt> on either Windows or any version of Unix.


## Toka



```toka
[ "R" file.open dup 0 <> [ dup file.close ] ifTrue 0 <> ] is exists?
" input.txt" exists? .
" /input.txt" exists? .
" docs" exists? .
" /docs" exists? .
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
file="input.txt",directory="docs"
IF (file=='file') THEN
PRINT file, " exists"
ELSE
PRINT/ERROR file," not exists"
ENDIF
IF (directory=='project') THEN
PRINT directory," exists"
ELSE
PRINT/ERROR "directory ",directory," not exists"
ENDIF

```

{{out}}

```txt

input.txt exists
@@@@@@@@  directory docs not exists                                    @@@@@@@@

```



## UNIX Shell


```bash
test -f input.txt
test -f /input.txt
test -d docs
test -d /docs
```


The next program runs all four checks and prints the results.


```bash
for f in input.txt /input.txt; do
	test -f "$f" && r=true || r=false
	echo "$f is a regular file? $r"
done
for d in docs /docs; do
	test -d "$d" && r=true || r=false
	echo "$d is a directory? $r"
done
```



## Ursa

The easiest way to do this in Ursa is to attempt to open the file in question. If it doesn't exist, an ioerror will be thrown.

```ursa
def exists (string filename)
	decl file f
	try
		f.open filename
		return true
	catch ioerror
		return false
	end try
end exists
```



## Vala

This needs to be compiled with the gio-2.0 package: valac --pkg gio-2.0 check_that_file_exists.vala

```vala
int main (string[] args) {
    string[] files = {"input.txt", "docs", Path.DIR_SEPARATOR_S + "input.txt", Path.DIR_SEPARATOR_S + "docs"};
    foreach (string f in files) {
        var file = File.new_for_path (f);
        print ("%s exists: %s\n", f, file.query_exists ().to_string ());
    }
    return 0;
}
```

A more complete version which informs whether the existing file is a regular file or a directory

```vala
int main (string[] args) {
    string[] files = {"input.txt", "docs", Path.DIR_SEPARATOR_S + "input.txt", Path.DIR_SEPARATOR_S + "docs"};
    foreach (var f in files) {
        var file = File.new_for_path (f);
        var exists = file.query_exists ();
        var name = "";
        if (!exists) {
            print ("%s does not exist\n", f);
        } else {
            var type = file.query_file_type (FileQueryInfoFlags.NOFOLLOW_SYMLINKS);
            if (type == 1) {
                name = "file";
            } else if (type == 2) {
                name = "directory";
            }
            print ("%s %s exists\n", name, f);
        }
    }
    return 0;
}
```



## Vedit macro language

Vedit allows using either '\' or '/' as directory separator character, it is automatically converted to the one used by the operating system.

```vedit
// In current directory
if (File_Exist("input.txt")) { M("input.txt exists\n") } else { M("input.txt does not exist\n") }
if (File_Exist("docs/nul", NOERR)) { M("docs exists\n") } else { M("docs does not exist\n") }

// In the root directory
if (File_Exist("/input.txt")) { M("/input.txt exists\n") } else { M("/input.txt does not exist\n") }
if (File_Exist("/docs/nul", NOERR)) { M("/docs exists\n") } else { M("/docs does not exist\n") }
```



## Visual Basic .NET

'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
'Current Directory
Console.WriteLine(If(IO.Directory.Exists("docs"), "directory exists", "directory doesn't exists"))
Console.WriteLine(If(IO.Directory.Exists("output.txt"), "file exists", "file doesn't exists"))

'Root
Console.WriteLine(If(IO.Directory.Exists("\docs"), "directory exists", "directory doesn't exists"))
Console.WriteLine(If(IO.Directory.Exists("\output.txt"), "file exists", "file doesn't exists"))

'Root, platform independent
Console.WriteLine(If(IO.Directory.Exists(IO.Path.DirectorySeparatorChar & "docs"), _
   "directory exists", "directory doesn't exists"))
Console.WriteLine(If(IO.Directory.Exists(IO.Path.DirectorySeparatorChar & "output.txt"), _
  "file exists", "file doesn't exists"))
```



## VBA



```vb

Option Explicit

Sub Main_File_Exists()
Dim myFile As String, myDirectory As String

    myFile = "Abdu'l-Bahá.txt"
    myDirectory = "C:\"
    Debug.Print File_Exists(myFile, myDirectory)
End Sub

Function File_Exists(F As String, D As String) As Boolean
    If F = "" Then
        File_Exists = False
    Else
        D = IIf(Right(D, 1) = "\", D, D & "\")
        File_Exists = (Dir(D & F) <> "")
    End If
End Function

```




## VBScript



```vbscript
Set FSO = CreateObject("Scripting.FileSystemObject")

Function FileExists(strFile)
    If FSO.FileExists(strFile) Then
        FileExists = True
    Else
        FileExists = False
    End If
End Function

Function FolderExists(strFolder)
    If FSO.FolderExists(strFolder) Then
        FolderExists = True
    Else
        Folderexists = False
    End If
End Function

'''''Usage (apostrophes indicate comments-this section will not be run)'''''
'If FileExists("C:\test.txt") Then
'   MsgBox "It Exists!"
'Else
'   Msgbox "awww"
'End If
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'Shorter version

If CreateObject("Scripting.FileSystemObject").FileExists("d:\test.txt") Then
    Wscript.Echo "File Exists"
Else
    Wscript.Echo "File Does Not Exist")
End If



```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}
The proposed solutions for VBA and VBScript work in VB6 as well, however here's a Windows API based approach:

```vb

'declarations:
Public Declare Function GetFileAttributes Lib "kernel32" _
  Alias "GetFileAttributesA" (ByVal lpFileName As String) As Long
Public Const INVALID_FILE_ATTRIBUTES As Long = -1
Public Const ERROR_SHARING_VIOLATION As Long = 32&

'implementation:
Public Function FileExists(ByVal Filename As String) As Boolean
Dim l As Long
l = GetFileAttributes(Filename)
  If l <> INVALID_FILE_ATTRIBUTES Then
    FileExists = ((l And vbDirectory) = 0)
  ElseIf Err.LastDllError = ERROR_SHARING_VIOLATION Then
    FileExists = True
  End If
End Function

```



## Yabasic


```Yabasic
open "foo.bar" for writing as #1
print #1 "Hallo !"
close #1
if (not open(1,"foo.bar")) print "Could not open 'foo.bar' for reading"
close #1
if (not open(1,"buzz.bar")) print "Could not open 'buzz.bar' for reading"

```



## zkl


```zkl
File.exists("input.txt")  //--> True (in this case a sym link)
File.exists("/input.txt") //-->False
File.isDir("/")     //-->True
File.isDir("docs")  //-->False

```


{{omit from|HTML}}
{{omit from|TI-83 BASIC|Does not have a filesystem, just namespaced variables.}}
{{omit from|TI-89 BASIC|Does not have a filesystem, just namespaced variables.}}
{{omit from|Befunge|No filesystem support}}
