+++
title = "Truncate a file"
description = ""
date = 2019-03-05T09:51:16Z
aliases = []
[extra]
id = 10120
[taxonomies]
categories = []
tags = []
+++

{{task}} 
[[Category:File System Operations]]

;Task:
Truncate a file to a specific length.   This should be implemented as a routine that takes two parameters: the filename and the required file length (in bytes).


Truncation can be achieved using system or library calls intended for such a task, if such methods exist, or by creating a temporary file of a reduced size and renaming it, after first deleting the original file, if no other method is available.   The file may contain non human readable binary data in an unspecified format, so the routine should be "binary safe", leaving the contents of the untruncated part of the file unchanged.

If the specified filename does not exist, or the provided length is not less than the current file length, then the routine should raise an appropriate error condition. 

On some systems, the provided file truncation facilities might not change the file or may extend the file, if the specified length is greater than the current length of the file. 

This task permits the use of such facilities.   However, such behaviour should be noted, or optionally a warning message relating to an non change or increase in file size may be implemented.





## Ada


The following program is an implementation in Ada using system-independent tools from the standard library to read and write files, remove and rename them. It should work for on any system with byte-oriented file storage and uses Ada 2012 conditional expressions.


```Ada
with Ada.Command_Line, Ada.Sequential_IO, Ada.Directories;

procedure Truncate_File is

   type Byte is mod 256;
   for Byte'Size use 8;

   package Byte_IO is new Ada.Sequential_IO(Byte);

   function Arg(N: Positive) return String renames Ada.Command_Line.Argument;
   function Args return Natural renames Ada.Command_Line.Argument_Count;

begin
   -- give help output if neccessary
   if Args < 2 or else Args > 3 then
      raise Program_Error
        with "usage: truncate_file <filename> <length> [<temp_file>]";
   end if;

   -- now do the real work
   declare
      File_Name: String := Arg(1);
      Temp_Name: String := (if Args = 2 then Arg(1) & ".tmp" else Arg(3));
                             -- an Ada 2012 conditional expression
      File, Temp: Byte_IO.File_Type;
      Count: Natural := Natural'Value(Arg(2));
      Value: Byte;
   begin
      -- open files
      Byte_IO.Open  (File => File, Mode => Byte_IO.In_File,  Name => File_Name);
      Byte_IO.Create(File => Temp, Mode => Byte_IO.Out_File, Name => Temp_Name);

      -- copy the required bytes (but at most as much as File has) from File to Temp
      while (not Byte_IO.End_Of_File(File)) and Count > 0 loop
         Byte_IO.Read (File, Value);
         Byte_IO.Write(Temp, Value);
         Count := Count - 1;
      end loop;

      -- close files
      Byte_IO.Close(Temp);
      Byte_IO.Close(File);

      if Count = 0 then -- File was at least Count bytes long
         -- remove File and rename Temp to File
         Ada.Directories.Delete_File(Name => File_Name);
         Ada.Directories.Rename(Old_Name => Temp_Name, New_Name => File_Name);
      else -- File was too short
         -- remove Temp and leave File as it is, output error
         Ada.Directories.Delete_File(Name => Temp_Name);
         raise Program_Error
           with "Size of """ & File_Name & """ less than " & Arg(2);
      end if;
   end;
end Truncate_File;
```



## AutoHotkey


```autohotkey
truncFile("S:\Portables\AutoHotkey\My Scripts\Other_Codes\motion2.ahk", 1200)
return

truncFile(file, length_bytes){
	if !FileExist(file)
		msgbox, File doesn't exists.
	FileGetSize, fsize, % file, B
	if (length_bytes>fsize)
		msgbox, New truncated size more than current file size
	f := FileOpen(file, "rw")
	f.length := length_bytes
	f.close()
}
```


## AWK


```AWK

# syntax: GAWK -f TRUNCATE_A_FILE.AWK
BEGIN {
    main("NOTHERE",100)
    main("FILENAME.TMP",-1)
    main("FILENAME.TMP",500)
    exit(0)
}
function main(filename,size,  ret) {
    ret = truncate_file(filename,size)
    if (ret != "") {
      printf("error: FILENAME=%s, %s\n",filename,ret)
    }
}
function truncate_file(filename,size,  cmd,fnr,msg,old_BINMODE,old_RS,rec) {
    cmd = sprintf("ls --full-time -o %s",filename)
    if (size < 0) {
      return("size cannot be negative")
    }
    old_BINMODE = BINMODE
    old_RS = RS
    BINMODE = 3
    RS = "[^\x00-\xFF]"
    while (getline rec <filename > 0) {
      fnr++
    }
    close(filename)
    if (fnr == 0) {
      msg = "file not found"
    }
    if (fnr > 1) {
      msg = "choose a different RecordSeparator"
    }
    if (msg == "") { # no errors
      system(cmd) # optional: show filesize before truncation
      if (length(rec) > size) {
        rec = substr(rec,1,size)
      }
      printf("%s",rec) >filename
      close(filename)
      system(cmd) # optional: show filesize after truncation
    }
    BINMODE = old_BINMODE
    RS = old_RS
    return(msg)
}

```

{{out}}

```txt

error: FILENAME=NOTHERE, file not found
error: FILENAME=FILENAME.TMP, size cannot be negative
-rw-r--r--   1 1001       240128 Mon Aug 02 12:18:15 2004 FILENAME.TMP
-rw-r--r--   1 1001          500 Sat Jan 07 00:55:28 2017 FILENAME.TMP

```



## BASIC

{{works with|QuickBASIC|7}}

This is fairly generic MS BASIC. As such, it actually works in a wide variety of compilers and interpreters -- certainly too many to list here, but the list includes non-.Net [[Visual Basic]], [[FreeBASIC]], [[QB64]], etc. With a user-provided implementation of <code>DIR$</code>, it even works under [[QBasic]].


```qbasic
SUB truncateFile (file AS STRING, length AS LONG)
    IF LEN(DIR$(file)) THEN
        DIM f AS LONG, c AS STRING
        f = FREEFILE
        OPEN file FOR BINARY AS f
        IF length > LOF(f) THEN
            CLOSE f
            ERROR 62 'Input past end of file
        ELSE
            c = SPACE$(length)
            GET #f, 1, c
            CLOSE f
            OPEN file FOR OUTPUT AS f
            PRINT #f, c;
            CLOSE f
        END IF
    ELSE
        ERROR 53
    END IF
END SUB
```


See also: [[#Liberty BASIC|Liberty BASIC]], [[#PowerBASIC|PowerBASIC]], [[#PureBasic|PureBasic]], [[#ZX Spectrum Basic|ZX Spectrum Basic]].


## BBC BASIC

This will extend the file if the specified length is greater than the existing length.  A test to prevent that could easily be added.

```bbcbasic
      DEF PROCtruncate(file$, size%)
      LOCAL file%
      file% = OPENUP(file$)
      IF file%=0 ERROR 100, "Could not open file"
      EXT#file% = size%
      CLOSE #file%
      ENDPROC
```



## Bracmat

Handling binary data is not easy in Bracmat. This solution reads all bytes as numbers. They are prepended to a list. After reversing the list, all numbers are written to the file. Notice that to close a file you should try to seek to a non-existing file position.<code>fil$(,SET,-1)</code> seeks to the position before the start of the file currently in focus.  

```bracmat
( ( trunc
  =   name length elif file c
    .   !arg:(?name,?length)
      & fil$(!name,rb)
      & fil$(,DEC,1)
      & :?elif
      &   whl
        ' ( !length+-1:?length:~<0
          & fil$() !elif:?elif
          )
      & (fil$(,SET,-1)|)
      & whl'(!elif:%?c ?elif&!c !file:?file)
      & fil$(!name,wb)
      & fil$(,DEC,1)
      & whl'(!file:%?c ?file&fil$(,,1,!c))
      & (fil$(,SET,-1)|)
      & !length:<0
  )
& put$("I have a secret to tell you. Listen:","test.txt",NEW)
& ( trunc$("test.txt",20)&out$(get$("test.txt",STR))
  | out$"File too short"
  )
);
```

Output:

```txt
I have a secret to t
```



## C


### Windows

Windows uses <code>SetEndOfFile()</code> to change the length of a file. This program can truncate or extend a file. It can detect and print errors.

* If the file does not exist: "The system cannot find the file specified."
* If the length is negative: "An attempt was made to move the file pointer before the beginning of the file."
* If the length is too large: "There is not enough space on the disk."

{{works with|MinGW}}

```c>#include <windows.h

#include <stdio.h>
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
dotruncate(wchar_t *fn, LARGE_INTEGER fp)
{
	HANDLE fh;

	fh = CreateFileW(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
	if (fh == INVALID_HANDLE_VALUE) {
		oops(fn);
		return 1;
	}

	if (SetFilePointerEx(fh, fp, NULL, FILE_BEGIN) == 0 ||
	    SetEndOfFile(fh) == 0) {
		oops(fn);
		CloseHandle(fh);
		return 1;
	}

	CloseHandle(fh);
	return 0;
}

/*
 * Truncate or extend a file to the given length.
 */
int
main()
{
	LARGE_INTEGER fp;
	int argc;
	wchar_t **argv, *fn, junk[2];

	/* MinGW never provides wmain(argc, argv). */
	argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argv == NULL) {
		oops(L"CommandLineToArgvW");
		return 1;
	}

	if (argc != 3) {
		fwprintf(stderr, L"usage: %ls filename length\n", argv[0]);
		return 1;
	}

	fn = argv[1];

	/* fp = argv[2] converted to a LARGE_INTEGER. */
	if (swscanf(argv[2], L"%lld%1ls", &fp.QuadPart, &junk) != 1) {
		fwprintf(stderr, L"%ls: not a number\n", argv[2]);
		return 1;
	}

	return dotruncate(fn, fp);
}
```



### POSIX


```c>#include <unistd.h

#include <sys/types.h>

...
truncate(filename, length);
ftruncate(fd, length);
...

```

Both functions have <code>length</code> argument of <code>off_t</code> type.  There are about a million possible errors, of interest to this task are <code>EFBIG</code>: size too large; <code>EINVAL</code>: size is negative or too large; <code>EIO</code>: IO error; <code>EACCESS</code> (for <code>truncate</code>): either can't see or can't write to the file; <code>EBADF</code> or <code>EINVAL</code> (for <code>ftruncate</code>): file descriptor is not a file descriptor, or not opened for writing.

When specifying a new file size larger than current value, the file will be extended and padded with null bytes.


## C++


```C++>#include <string

#include <fstream>

using namespace std;

void truncateFile(string filename, int max_size) {
  std::ifstream input( filename, std::ios::binary );
  char buffer;
  string outfile = filename + ".trunc";
  ofstream appendFile(outfile, ios_base::out);
  for(int i=0; i<max_size; i++) {
    input.read( &buffer, sizeof(buffer) );
    appendFile.write(&buffer,1);
  }
  appendFile.close();                                                                                                                                                                }

int main () {
  truncateFile("test.txt", 5);
  return 0;
}

```



## C sharp



```c sharp
using System;
using System.IO;

namespace TruncateFile
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            TruncateFile(args[0], long.Parse(args[1]));
        }

        private static void TruncateFile(string path, long length)
        {
            if (!File.Exists(path))
                throw new ArgumentException("No file found at specified path.", "path");

            using (var fileStream = new FileStream(path, FileMode.Open, FileAccess.Write))
            {
                if (fileStream.Length < length)
                    throw new ArgumentOutOfRangeException("length",
                                                          "The specified length is greater than that of the file.");

                fileStream.SetLength(length);
            }
        }
    }
}
```



## Clojure

{{trans|Java}}

```clojure
(defn truncate [file size]
  (with-open [chan (.getChannel (java.io.FileOutputStream. file true))]
    (.truncate chan size)))

(truncate "truncate_test.txt" 2)
```



## D


```d
import std.file, std.exception;

void truncateFile(in string name, in size_t newSize) {
    if (!exists(name) || !isFile(name))
        throw new Exception("File not found.");

    auto size = getSize(name);
    if (size <= newSize)
        throw new Exception(
            "New size must be smaller than original size.");

    auto content = cast(ubyte[])read(name, newSize);
    if (content.length != newSize)
        throw new Exception("Reading file failed.");

    write(name, content);
    enforce(getSize(name) == newSize);
}

void main() {
    truncateFile("truncate_test.txt", 0);
}
```



## Delphi


Delphi has the same <code>truncate</code> method as Pascal, which could be used like this:


```Delphi
procedure TruncateFile(FileName : string; NewSize : integer);
var
  aFile:   file of byte;
begin
  Assign(aFile, FileName);
  Reset(aFile);
  try
    Seek(afile, NewSize);
    Truncate(aFile);
  finally
    Close(afile);
  end;
end;
```


Most people nowadays seem to use streams to access files, so an alternative is:


```Delphi
procedure TruncateFile(FileName : string; NewSize : integer);
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenReadWrite, fmShareExclusive);
  try
    Stream.Size := NewSize;
  finally
    Stream.Free;
  end;
end;
```


With both of these approaches, if the NewSize is greater than the existing file size the file will be expanded and filled with nulls.

If you try to truncate a non-existent file with the Pascal-style code you will get an EInOutError exception with the message:
  File not found.
The exception message does not specify the file name.

If you try to truncate a non-existent file using a stream you will get an EFOpenError exception with the message:
  Cannot open file "<File name with full path>. The system cannot find the file specified.

## Elena

ELENA 4.x:

```elena
import system'io;
import extensions;
 
extension fileOp : File
{
    set Length(int len)
    {
        auto stream := FileStream.openForEdit(self);
 
        stream.Length := len;
 
        stream.close()
    }
}
 
public program()
{
    if (program_arguments.Length != 3)
        { console.printLine:"Please provide the path to the file and a new length"; AbortException.raise() };
 
    auto file := File.assign(program_arguments[1]);
    var length := program_arguments[2].toInt();
 
    ifnot(file.Available)
        { console.printLine("File ",file," does not exist"); AbortException.raise() };
 
    file.Length := length
}
```



## Erlang


```Erlang

-module( truncate ).

-export( [file/2] ).

file( Name, Size ) ->
	{file_exists, true} = {file_exists, filelib:is_file( Name )},
	{ok, IO} = file:open( Name, [read, write] ),
	{ok, Max} = file:position( IO, eof ),
	{correct_size, true} = {correct_size, (Size < Max)},
	{ok, Size} = file:position( IO, {bof, Size} ),
	ok = file:truncate( IO ).

```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System
open System.IO

let truncateFile path length =
    if not (File.Exists(path)) then failwith ("File not found: " + path)
    use fileStream = new FileStream(path, FileMode.Open, FileAccess.Write)
    if (fileStream.Length < length) then failwith "The specified length is greater than the current file length."
    fileStream.SetLength(length)

[<EntryPoint>]
let main args =
    truncateFile args.[0] (Int64.Parse(args.[1]))
    0
```



## Go

Go has the required function in the standard library.  The implementation calls operating system specific functions and returns whatever errors the operating system reports.

```go
import (
    "fmt"
    "os"
)

if err := os.Truncate("filename", newSize); err != nil {
    fmt.Println(err)
}
```

Package os also has a separate function that operates on an open file.


## Fortran

Fortran offers no access to any facilities the file system may offer for truncating a disc file via standard language features, thus in the absence of special routines or deviant compilers that do, you're stuck.

So, plan B is to copy the desired part of the file to a temporary storage area then write it back out again. Standard Fortran has no access to facilities that might change a disc file's name, though it can delete one via <code>CLOSE(F, STATUS = "DELETE")</code> - unless it had been opened with READONLY. It can however overwrite a file, or more precisely, close the file then reopen it with STATUS="REPLACE". This is rather bad, because there will be a period during which the desired data in the file might vanish, due to a power failure or system crash (or running out of disc space, or...) before the rewriting has been completed which is why the source file should be renamed to something, the new version written, and only then (if at all), the original deleted. This imposes a greater requirement on storage space, but avoids a catastrophic loss of data. By going for the input file with ACTION="READWRITE" even though it will only be read from, there is a chance of an early rebuff if write authority is not available as will be needed when it is reopened with STATUS="REPLACE" - though there is a brief opportunity for trouble after the close and before the reopen.

The details turn out to be tricky. There is no equivalent of Turbo Pascal's BlockRead(F,stuff,size(stuff),N) whereby the next block of bytes from file F are read (or, similarly, written) up to the size of the recipient "stuff", with the number actually read appearing in N as when the amount remaining is smaller than "stuff". It is possible to use the Q-format feature (not standard, but common) that states how many characters are yet to be read from an input record, however, this is for FORM="FORMATTED", and in the ASCII world, the record dividers are indicated in-line by one of CR, CRLF, LFCR, or LF which are ''not'' counted in the character count, nor can they be imputed as being present because you don't know which length is being used and even if the code investigates to find out, I have seen a mixture in the one file. The task evidently means for the byte count to be absolute, not omitting the space occupied by such splitters if present. A file containing binary data rather than text (though both are bits, no more, no less), say integers and floating-point numbers, etc. will likely have bytes containing a CR or LF here and there just by chance, but if on output they are reconstituted as CRLF (or whichever is used) the resulting file will be uselessly damaged.

Accordingly, the input must be read in the UNFORMATTED style. This is done with a specified record length, but again unlike the BlockRead procedure there is no indication of a short record at the end - if the record length is say 60, there is no guarantee that the file's length will be a multiple of sixty and the remainder will cause trouble. The ERR=''label'' can catch this mishap, but still, there is no indication of by how much the remaining portion of the file fell short. The only record length that will divide all possible file lengths is ... one. Now arises a minor obstacle: some systems use the size of a default INTEGER to be the unit of a RECL=''n'', but fortunately, a compiler option allows the specification of the record length to be in units of bytes.

Initial tests threw up further obstacles, such as "end-of-file during read", the very first read. Floundering discovered that escalating to random access avoided this confusion, so ACCESS="DIRECT" and READ (F,REC=L) worked, except that the END action label is not permitted for direct access. ERR instead, to cover the case when a byte beyond end-of-file is requested, clearly an error. 
Despite writing output as a single byte at a time, the output file appeared to manifest as a file having records with a 32-bit record length of value one (not helped by endian issues) followed by the content, but changing to writing each byte of the output using the FORM="FORMATTED" option dodged that, provided that each output employed the "$" output format code that signifies that a record ender (the CR, etc. mess) is not to be appended. After these experiences, one suspects that each system will have its own quirks for some types of file, and testing will be required.

There is an option to specify BUFFERED="YES" and BUFFERCOUNT=n or similar in many Fortrans, but alas, these are not standard even in F90/95. One's guilt is not so easily diverted, but, ... this is really only a demonstration, perhaps with very occasional use.

And so...
```Fortran
      SUBROUTINE CROAK(GASP)	!Something bad has happened.
       CHARACTER*(*) GASP	!As noted.
        WRITE (6,*) "Oh dear. ",GASP	!So, gasp away.
        STOP "++ungood."	!Farewell, cruel world.
      END			!No return from this.

      SUBROUTINE FILEHACK(FNAME,NB)
       CHARACTER*(*) FNAME	!Name for the file.
       INTEGER NB		!Number of bytes to survive.
       INTEGER L		!A counter for te length of the file.
       INTEGER F,T		!Mnemonics for file unit numbers.
       PARAMETER (F=66,T=67)	!These should do.
       LOGICAL EXIST		!Same as the mnemonic so left/right can be forgotten.
       CHARACTER*1 B		!The worker!
        IF (FNAME.EQ."") CALL CROAK("Blank file name!")
        IF (NB.LE.0)     CALL CROAK("Chop must be positive!")
        INQUIRE(FILE = FNAME, EXIST = EXIST)	!This mishap is frequent, so attend to it.
        IF (.NOT.EXIST) CALL CROAK("Can't find a file called "//FNAME)	!Tough love.
        OPEN (F,FILE=FNAME,STATUS="OLD",ACTION="READWRITE",	!Grab the source file.
     1   FORM="UNFORMATTED",RECL=1,ACCESS="DIRECT")		!Oh dear.
        OPEN (T,STATUS="SCRATCH",FORM="UNFORMATTED",RECL=1)	!Request a temporary file.

Copy the desired "records" to the temporary file.
   10   DO L = 1,NB	!Only up to a point.
          READ  (F,REC = L,ERR = 20) B	!One whole byte!
          WRITE (T) B			!And, write it too!
        END DO		!Again.
   20   IF (L.LE.NB) CALL CROAK("Short file!")	!Should end the loop with L = NB + 1.
Convert from input to output...
        REWIND T		!Not CLOSE! That would discard the file!
        CLOSE(F)		!The source file still exists.
        OPEN (F,FILE=FNAME,FORM="FORMATTED",	!But,
     1   ACTION="WRITE",STATUS="REPLACE")	!This dooms it!
Copy from the temporary file.
        DO L = 1,NB	!A certain number only.
          READ  (T) B		!One at at timne.
          WRITE (F,"(A1,$)") B	!The $, obviously, means no end-of-record appendage.
        END DO		!And again.
Completed.
   30   CLOSE(T)	!Abandon the temporary file.
        CLOSE(F)	!Finished with the source file.
      END		!Done.

      PROGRAM CHOPPER
       CALL FILEHACK("foobar.txt",12)
      END
```



## Haskell

This can be achieved by using the <code>setFileSize</code> function in [http://hackage.haskell.org/packages/archive/unix-compat/0.1.2.1/doc/html/System-PosixCompat-Files.html#13 System.PosixCompat.Files]:


```Haskell
setFileSize :: FilePath -> FileOffset -> IO ()
-- Truncates the file down to the specified length. 
-- If the file was larger than the given length
-- before this operation was performed the extra is lost.
-- Note: calls truncate.

```


=={{header|Icon}} and {{header|Unicon}}==
Unicon provides the built-in function truncate which can be used to truncate a file.  The following line of code truncates ''filename'' to ''newsizeinbytes''.  The file is opened for both read and write in untranslated mode.

```Unicon
truncate(f := open(filename, "bu"), newsizeinbytes) & close(f)
```

Note: The Unicon book incorrectly indicates that truncate doesn't work on Windows.


## J

'''Solution:'''

```j
require 'files'                         NB. needed for versions prior to J7
ftruncate=: ] fwrite~ ] fread@; 0 , [
```

'''Usage:'''

```j
   (1000$ 'abcdefg') fwrite 'text.txt'  NB. create test file
   567 ftruncate 'test.txt'             NB. truncate test file
567
   fsize 'test.txt'                     NB. check new file size
567
```



## Java

The built-in function for truncating a file in Java will leave the file unchanged if the specified size is larger than the file. This version expects the source file name and the new size as command line arguments (in that order).

```java
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;

public class TruncFile {
	public static void main(String[] args) throws IOException{
		if(args.length < 2){
			System.out.println("Usage: java TruncFile fileName newSize");
			return;
		}
		//turn on "append" so it doesn't clear the file
		FileChannel outChan = new FileOutputStream(args[0], true).getChannel();
		long newSize = Long.parseLong(args[1]);
		outChan.truncate(newSize);
		outChan.close();
	}
}
```



## Julia

Uses the built-in <code>truncate</code> function (which truncates a stream):

```julia
function truncate_file(fname, size):
    open(fname, "r+") do f
        truncate(f, size)
    end
end
```



## Kotlin


```scala
// version 1.1.2

import java.io.FileOutputStream
import java.nio.channels.FileChannel

fun truncateFile(fileName: String, newSize: Long) {
    var fc: FileChannel? = null
    try {
        fc = FileOutputStream(fileName, true).channel
        if (newSize >= fc.size()) 
            println("Requested file size isn't less than existing size")
        else
            fc.truncate(newSize)
    }
    catch (ex: Exception) {
        println(ex.message)
    }
    finally {
        fc!!.close()
    }
}

fun main(args: Array<String>) {
    truncateFile("test.txt", 10)
}
```



## Lasso


```Lasso
define file_truncate(path::string, size::integer) => {

	local(file = file(#path))

	fail_if(not(#file -> exists), -1, 'There is no file at the given path')
	fail_if(#file -> size < #size, -1, 'No point in truncating a file to a larger size than it already is')

	#file -> setSize(#size)

}
local(filepath = '//Library/WebServer/Documents/Lasso9cli/trunk/testing/lorem_ipsum_long.txt')

stdoutnl(file(#filepath) -> readbytes)
stdoutnl('Original size: ' + file(#filepath) -> size)

file_truncate(#filepath, 300)

stdoutnl(file(#filepath) -> readbytes)
stdout(file('Truncated size: ' + #filepath) -> size)
```

Output:

```txt
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris consequat
ornare lectus, dignissim iaculis libero consequat sed. Proin quis magna in
arcu sagittis consequat sed ac risus. Ut a pharetra dui. Phasellus molestie,
mauris eget scelerisque laoreet, diam dolor vulputate nulla, in porta sem sem
sit amet lacus. Aenean sed volutpat magna. Vestibulum lobortis mollis lectus,
eu semper quam congue at. Donec ac ligula a neque tincidunt elementum. Nam
urna felis, interdum non ullamcorper eget, commodo viverra ligula. Fusce cursus
dolor in nisl tincidunt non sagittis libero elementum. Maecenas rhoncus ornare
gravida. Nullam luctus pulvinar lorem, laoreet aliquet massa malesuada eget.
Original size: 692
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris consequat
ornare lectus, dignissim iaculis libero consequat sed. Proin quis magna in
arcu sagittis consequat sed ac risus. Ut a pharetra dui. Phasellus molestie,
mauris eget scelerisque laoreet, diam dolor vulputate nulla, in porta sem 
Truncated size: 300

```



## Liberty BASIC

Note Liberty BASIC provides a way to exit a function.


It is also possible to call API functions to achieve this task.

```lb

dim info$( 50, 50)    '   NB pre-dimension before calling file-exists
                      '        needed for file-exists function
open "test.dat" for output as #1    'create file
    for i = 1 to 10000
        #1 chr$( int( 256 *rnd( 1)));
    next
close #1

call truncateFile, "test.dat", 5000

wait

sub truncateFile fn$, length
    if fileExists( DefaultDir$, fn$) =0 then notice "No such file": exit sub
    open fn$ for input as #i
        file$ =input$( #i, lof( #i))
        if len( file$) <length then notice "Too short": close #i: exit sub
        file$ =left$( file$, length)
    close #i
    open "test.dat" for output as #o
        #o file$
    close #o
end sub

function fileExists( path$, filename$)
  files path$, filename$, info$()
  fileExists =val( info$( 0, 0))  'non zero is true
end function

end

```



## Lingo

With native means (Fileio Xtra) file truncation can only be implemented indirectly, either using a temp file or loading truncated file contents into memory, deleting original and writing memory back to file:

```lingo
----------------------------------------
-- Truncates file
-- @param {string} filename
-- @param {integer} length
-- @return {bool} success
----------------------------------------
on truncate (filename, length)
  fp = xtra("fileIO").new()
  fp.openFile(filename, 0)
  if fp.status() then return false
  if fp.getLength()=length then
    -- nothing to do
    fp.closeFile()
    return true
  end if
  data = fp.readByteArray(length)
  if data.length<>length then
    fp.closeFile()
    return false
  end if
  fp.delete()
  fp.createFile(filename)
  fp.openFile(filename, 2)
  fp.writeByteArray(data)
  ok = fp.status()=0
  fp.closeFile()
  return ok
end
```

But there are also free plugins ("Xtras") like e.g. "BinFile Xtra" that support "in-file" truncation:
{{libheader|BinFile Xtra}}

```lingo
-- truncates file to 10 KB length
bx_file_truncate(_movie.path&"foo.dat", 10240)
```



## Lua

Lua treats strings as being invariably one byte per character (hence the awkwardness of trying to use it with unicode), so it's safe to use string methods to truncate binary data.

```Lua
function truncate (filename, length)
    local inFile = io.open(filename, 'r')
    if not inFile then
        error("Specified filename does not exist")
    end
    local wholeFile = inFile:read("*all")
    inFile:close()
    if length >= wholeFile:len() then
        error("Provided length is not less than current file length")
    end
    local outFile = io.open(filename, 'w')
    outFile:write(wholeFile:sub(1, length))
    outFile:close()
end
```



## Mathematica


```Mathematica
Truncate[file_, n_] := Module[{filename = file, nbbytes = n, temp},
  temp = $TemporaryPrefix <> filename;
  BinaryWrite[temp, BinaryReadList[filename, "Byte", nbbytes]];
  Close[temp]; DeleteFile[filename]; RenameFile[temp, filename];
  ]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function truncate_a_file(fn,count); 

fid=fopen(fn,'r');
s = fread(fid,count,'uint8');
fclose(fid); 

fid=fopen(fn,'w');
s = fwrite(fid,s,'uint8');
fclose(fid); 
```



## Nim


```nim
import posix

discard truncate("filename", 1024)
```



## OCaml


The <code>Unix</code> module provided with the standard distribution provides a function <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALtruncate truncate]</code>:


```ocaml
val truncate : string -> int -> unit
(** Truncates the named file to the given size. *)
```


There is also a function <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALftruncate ftruncate]</code> that does the equivalent operation but with a file descriptor instead of a file name:


```ocaml
val ftruncate : file_descr -> int -> unit
(** Truncates the file corresponding to the given descriptor to the given size. *)
```



## PARI/GP


Create a file /tmp/test.file and truncate to 20 bytes: (Linux only)

```parigp
install("truncate", "isL", "trunc")

trunc("/tmp/test.file", 20)
```



## Pascal


```pascal

Program FileTruncate;

uses
  SysUtils;

var
  myfile:   file of byte;
  filename: string;
  position: integer;

begin
  write('File for truncation: ');
  readln(filename);
  if not FileExists(filename) then
  begin
    writeln('Error: File does not exist.');
    exit;
  end;

  write('Truncate position: ');
  readln(position);

  Assign(myfile, filename);
  Reset(myfile);
  if FileSize(myfile) < position then
  begin
    writeln('Warning: The file "', filename, '" is too short. No need to truncate at position ', position);
    Close(myfile);
    exit;
  end;

  Seek(myfile, position);
  Truncate(myfile);
  Close(myfile);
  writeln('File "', filename, '" truncated at position ', position, '.');
end.

```

Output:

```txt

File for truncation: test
Truncate position: 3
File "test" truncated at position 3.

```



## Perl


```perl
# Open a file for writing, and truncate it to 1234 bytes.
open FOO, ">>file" or die;
truncate(FOO, 1234);
close FOO;

# Truncate a file to 567 bytes.
truncate("file", 567);
```


## Perl 6

{{Works with|rakudo|2016.07}}

```perl6
use NativeCall;

sub truncate(Str, int32 --> int32) is native {*}

sub MAIN (Str $file, Int $to) {
    given $file.IO { 
        .e        or die "$file doesn't exist";
        .w        or die "$file isn't writable";
        .s >= $to or die "$file is not big enough to truncate";
    }
    truncate($file, $to) == 0 or die "Truncation was unsuccessful";
}
```



## Phix

In honour of this very task, there is now (0.8.0+) a set_file_size() builtin, for the grubby/cross-platform details see builtins/pfile.e (an autoinclude)

It will pad or truncate as needed.

```Phix
?get_file_size("test.txt")
?set_file_size("test.txt",100)
?get_file_size("test.txt")
?set_file_size("test.txt",1024)
?get_file_size("test.txt")
```

{{out}} (annotated, repeatable, assumes test.txt already exists)

```txt

--1024
--1 (true)
--100
--1 (true)
--1024

```



## PicoLisp

On the 64-bit version, we can call the native runtime library:

```PicoLisp
(de truncate (File Len)
   (native "@" "truncate" 'I File Len) )
```

Otherwise (on all versions), we call the external truncate command:

```PicoLisp
(de truncate (File Len)
   (call "truncate" "-s" Len File) )
```



## PL/I

<lang>
/* Parameters to be read in by the program are:   */
/* 1. the name of the file to be truncated, and   */
/* 2. the size of file after truncation.          */

truncate_file: procedure options (main); /* 12 July 2012 */
   declare (i, n) fixed binary (31);
   declare filename character(50) varying;
   declare in file record input, out file record output;

   put ('What is the name of the file to be truncated?');
   get edit (filename) (L);
   put ('What is the length of file to be retained?');
   get (n);

   begin;
      declare c(n) character (1), ch character (1);

      open file (in) title ('/' || filename || ',type(fixed),recsize(1)' )
         input;
      do i = 1 to n; read file (in) into (ch); c(i) = ch; end;
      close file (in);

      open file (out) title ('/' || filename || ',append(n),type(fixed),
         recsize(' || trim(n) || ')' );
      write file (out) from (c);
      close file (out);
   end;
end truncate_file;

```



## PowerBASIC

{{trans|BASIC}}

While PowerBASIC can use the QuickBASIC version of <code>truncateFile</code>, PB provides an easier way to do this, via the <code>SETEOF</code> function -- but since <code>SETEOF</code> will extend a file if it is not as long as specified, we still need to wrap it in a <code>SUB</code> to meet this task's specifications.


```powerbasic
SUB truncateFile (file AS STRING, length AS DWORD)
    IF LEN(DIR$(file)) THEN
        DIM f AS LONG
        f = FREEFILE
        OPEN file FOR BINARY AS f
        IF length > LOF(f) THEN
            CLOSE f
            ERROR 62 'Input past end
        ELSE
            SEEK f, length + 1
            SETEOF f
            CLOSE f
        END IF
    ELSE
        ERROR 53 'File not found
    END IF
END SUB
```



## PureBasic

PureBasic has the internal function [http://www.purebasic.com/documentation/file/truncatefile.html TruncateFile] that cuts the file at the current file position and discards all data that follows.

```PureBasic
Procedure SetFileSize(File$, length.q)
  Protected fh, pos, i
  If FileSize(File$) < length
    Debug "File to small, is a directory or does not exist."
    ProcedureReturn #False
  Else 
    fh = OpenFile(#PB_Any, File$)
    FileSeek(fh, length)
    TruncateFile(fh)
    CloseFile(fh)
  EndIf
  ProcedureReturn #True
EndProcedure
```



## Powershell


```powershell
Function Truncate-File(fname) {
   $null | Set-Content -Path "$fname"
}

```



## Python


```python

def truncate_file(name, length):
    if not os.path.isfile(name):
        return False
    if length >= os.path.getsize(name):
        return False
    with open(name, 'ab') as f:
        f.truncate(length)
    return True

```



## Racket


Racket has a <tt>file-truncate</tt> function that expects an open port and truncate its associated file.  Note that it can also extend the file, and the code below prints a warning in that case.


```Racket

#lang racket

(define (truncate file size)
  (unless (file-exists? file) (error 'truncat "missing file: ~a" file))
  (when (> size (file-size file)) (printf "Warning: extending file size.\n"))
  (call-with-output-file* file #:exists 'update
    (λ(o) (file-truncate o size))))

```



## REXX


### without memory constraints

As long as the REXX program has access to enough virtual memory, this version won't have an issue, except for

some versions of REXX that are severely limited in virtual storage by design or by the operating system.

Extra boilerplate code was added to ensure that various flavors of REXX correctly handle a file that:
::::*   has been read (in input mode), 
::::*   then erased (deleted), 
::::*   then written to.

```rexx
/*REXX program  truncates  a file  to a  specified  (and smaller)  number of bytes.     */
parse arg siz FID                                /*obtain required arguments from the CL*/
FID=strip(FID)                                   /*elide  FID  leading/trailing blanks. */
if siz==''             then call ser "No truncation size was specified  (1st argument)."
if FID==''             then call ser "No fileID was specified  (2nd argument)."
if \datatype(siz,'W')  then call ser "trunc size isn't an integer: "          siz
if siz<1               then call ser "trunc size isn't a positive integer: "  siz
_=charin(FID,1,siz+1)                            /*position file and read a wee bit more*/
#=length(_)                                      /*get the length of the part just read.*/
if #==0                then call ser "the specified file doesn't exist: "     FID
if #<siz               then call ser "the file is smaller than trunc size: "   #
call lineout FID                                 /*close the file used, just to be safe.*/
'ERASE'      FID                                 /*invoke a command to delete the file  */
call lineout FID                                 /*close the file, maybe for REXX's use.*/
call charout FID, left(_,siz), 1                 /*write a truncated version of the file*/
call lineout FID                                 /*close the file used, just to be safe.*/
say 'file '  FID " truncated to "  siz  'bytes.' /*display some information to terminal.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:  say '***error***'  arg(1);      exit 13    /*display an error message  and  exit. */
```

For this example, Windows/XP and Windows 7 operating systems were used, the program was tested with:
:::*   Personal REXX
:::*   PC/REXX
:::*   all currently available Regina REXX interpreters   (versions 3.2 ──► 3.9.1)
:::*   R4 REXX
:::*   ROO
 
'''sample output'''   for the file named   '''AA.AA'''   being truncated to   '''333'''   bytes:

```txt

file  AA.AA  truncated to  333 bytes.

```



### with memory constraints


```rexx
/*REXX program  truncates  a file  to a  specified  (and smaller)  number of bytes.     */
parse arg siz FID                                /*obtain required arguments from the CL*/
FID=strip(FID)                                   /*elide  FID  leading/trailing blanks. */
if siz==''             then call ser "No truncation size was specified  (1st argument)."
if FID==''             then call ser "No fileID was specified  (2nd argument)."
if \datatype(siz,'W')  then call ser "trunc size isn't an integer: "          siz
if siz<1               then call ser "trunc size isn't a positive integer: "  siz
chunk=4000                                       /*read the file with this "buffer" size*/
call charin fid,1,0                              /*position the file pointer to byte # 1*/
_=                                               /* [↓]  read while the length data<siz.*/
             do  while length(_)<=siz            /* [↓]  have we reached  End-Of-File ? */
             buffer=charin(FID,,chunk)
             if length(buffer)==0  then leave    /*Nothing read? Then we're done reading*/
             _=_ || buffer                       /*append the chunk to the _  input data*/
             end   /*while*/
#=length(_)                                      /*get the length of the part just read.*/
if #==0                then call ser "the specified file doesn't exist: "     FID
if #<siz               then call ser "the file is smaller than trunc size: "   #
call lineout FID                                 /*close the file used, just to be safe.*/
'ERASE'      FID                                 /*invoke a command to delete the file  */
call lineout FID                                 /*close the file, maybe for REXX's use.*/
call charout FID, left(_,siz), 1                 /*write a truncated version of the file*/
call lineout FID                                 /*close the file used, just to be safe.*/
say 'file '  FID " truncated to "  siz  'bytes.' /*display some information to terminal.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:  say '***error***'  arg(1);      exit 13    /*display an error message  and  exit. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version. 




## Ring


```ring

file = "C:\Ring\ReadMe.txt"
fp = read(file)
fpstr = left(fp, 100)
see fpstr + nl
write(file, fpstr)

```

Output:

```txt

The Ring Programming Language
http://ring-lang.net/
Version 1.0
Release Date : January 25, 2016

```



## Ruby

This only works with some platforms. If truncation is not available, then Ruby raises NotImplementedError.


```ruby
# Open a file for writing, and truncate it to 1234 bytes.
File.open("file", "ab") do |f|
  f.truncate(1234)
  f << "Killroy was here" # write to file
end  # file is closed now.

# Just truncate a file to 567 bytes.
File.truncate("file", 567)
```



## Scala

{{libheader|Scala}}
```Scala
import java.io.FileOutputStream

object TruncFile extends App {
  if (args.length < 2) println("Usage: java TruncFile fileName newSize")
  else { //turn on "append" so it doesn't clear the file
    val outChan = new FileOutputStream(args(0), true).getChannel()
    val newSize = args(1).toLong
    outChan.truncate(newSize)
  }
}
```



## Sidef


```ruby
func truncate(filename, len) {
    var file = File(filename);
    len > file.size ->
        && die "The provided length is greater than the length of the file";
    file.truncate(len);
}

# truncate "file.ext" to 1234 bytes
truncate("file.ext", 1234);
```



## Tcl


```tcl
package require Tcl 8.5

set f [open "file" r+];	# Truncation is done on channels
chan truncate $f 1234;		# Truncate at a particular length (in bytes)
close $f
```



## UNIX Shell

The dd(1) command can truncate a file. Because dd(1) would create the file, this example runs ls(1). If the file does not exist, then ls(1) prints an error. If the file exists, then dd(1) truncates the file or prints an error. Unix can extend a file, so there is no error if the length increases.


```bash
# Truncate a file named "myfile" to 1440 kilobytes.
ls myfile >/dev/null &&
  dd if=/dev/null of=myfile bs=1 seek=1440k
```


----
Some systems have a truncate(1) command ([http://www.freebsd.org/cgi/man.cgi?query=truncate&apropos=0&sektion=0&manpath=FreeBSD+8.2-RELEASE&format=html FreeBSD truncate(1)], [http://www.gnu.org/software/coreutils/manual/html_node/truncate-invocation.html#truncate-invocation GNU truncate(1)]). 


```bash
# Truncate a file named "myfile" to 1440 kilobytes.
truncate -s 1440k myfile
```


----
Pure bourne-shell approach (cross-OS, no truncate(1) binary required)

```bash

# 1. simplest one-liner
-bash$ >file

# or the more clear/verbose:
echo -n>file

# 2. checks for file as input prior to truncate:
f="file";echo -n<$f>$f

# double-check file size
ls -1 file # it's trunc

# 3. multiple files (glob)
glob=$(ls -1 file[0-9]); for f in $glob; do echo -n<$f>$f; done


## OUTPUT:
# 1. one-liner
-bash-4.2$ echo -n>bad
+ echo -n
-bash-4.2$ ls -l bad; cat bad
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  0 Dec  4 20:07 bad
+ cat bad
-bash-4.2$ 

# 2. checks
-bash-4.2$ set -x; f="bad"; echo -n<$f>$f
+ f=bad
+ echo -n
-bash: bad: No such file or directory

# Try again with the shortest version: f="bad"<$f>$f
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  8192 Dec  5 01:04 bad
-bash-4.2$ f="bad" <$f>$f
+ f=bad
-bash-4.2$ ls -l
+ ls -l
-rw-r--r--  1 fieldsa  fieldsa       0 Dec  5 01:04 bad
-bash-4.2$ rm bad
+ rm bad
-bash-4.2$ f="bad" <$f>$f
+ f=bad
-bash: bad: No such file or directory

# Now a bad file exists
-bash-4.2$ echo 'abc'>bad
+ echo abc
-bash-4.2$ ls -l bad;cat<bad     
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  4 Dec  4 19:21 bad
+ cat
abc

-bash-4.2$ f="bad"; echo -n<$f>$f
+ f=bad
+ echo -n
-bash-4.2$ ls -l bad;cat<bad;echo "_EOF_"
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  0 Dec  4 19:23 bad
+ cat
+ echo _EOF_
_EOF_

-bash-4.2$ rm bad
+ rm bad
-bash-4.2$ f="bad" <$f&&echo -n>$f
+ f=bad
-bash: bad: No such file or directory

# 4. trunc(): trunc file size # truncate at length
-bash-4.2$ trunc() { IFS= read -N $2 b<"$1"; echo -n "$b">"$1"; }
-bash-4.2$ echo "abcdef">bad; ls -l bad; cat bad; trunc bad 3; ls -l bad; cat bad; echo "_EOF_"
+ echo abcdef
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  7 Dec  4 19:59 bad
+ cat bad
abcdef
+ trunc bad 3
+ read -N 3 b
+ echo -n abc
+ ls -l bad
-rw-r--r--  1 fieldsa  fieldsa  3 Dec  4 19:59 bad
+ cat bad
abc+ echo _EOF_
_EOF_

```


NOTE[4]: Designed for regular text files.  Does not work when files contain: chars 0x00 and 0xFF as of bash 4.2 - these chars are treated differently in read -N.  For this reason use of method#4 is not prefer over dd(1) or truncate(1) when using binary files, or large files. [4b] http://unix.stackexchange.com/questions/8618/script-that-keep-reading-a-stream,[4a] https://groups.google.com/forum/#!topic/gnu.bash.bug/a2rjQHpQYSU


## VBScript


```vb

Sub truncate(fpath,n)
	'Check if file exist
	Set objfso = CreateObject("Scripting.FileSystemObject")
	If objfso.FileExists(fpath) = False Then
		WScript.Echo fpath & " does not exist"
		Exit Sub
	End If
	content = ""
	'stream the input file
	Set objinstream = CreateObject("Adodb.Stream")
	With objinstream
		.Type = 1
		.Open
		.LoadFromFile(fpath)
		'check if the specified size is larger than the file content
		If n <= .Size Then
			content = .Read(n)
		Else
			WScript.Echo "The specified size is larger than the file content"
			Exit Sub
		End If
		.Close
	End With
	'write the truncated version
	Set objoutstream = CreateObject("Adodb.Stream")
	With objoutstream
		.Type = 1
		.Open
		.Write content
		.SaveToFile fpath,2
		.Close
	End With
	Set objinstream = Nothing
	Set objoutstream = Nothing
	Set objfso = Nothing
End Sub

'testing
Call truncate("C:\temp\test.txt",30)

```



## ZX Spectrum Basic


We can truncate files that were saved as binary. We don't know the length of the original file, so if the provided length is longer, then the file will be extended.


```zxbasic
10 CLEAR 29999
20 INPUT "Which file do you want to truncate?";f$
30 PRINT "Start tape to load file to truncate."
40 LOAD f$ CODE 30000
50 "Input how many bytes do you want to keep?";n
60 PRINT "Please rewind the tape and press record."
70 SAVE f$ CODE 30000,n
80 STOP
```


{{omit from|GUISS}}
{{omit from|HTML}}
