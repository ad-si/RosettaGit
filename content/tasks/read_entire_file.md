+++
title = "Read entire file"
description = ""
date = 2019-10-12T12:22:58Z
aliases = []
[extra]
id = 7628
[taxonomies]
categories = ["task", "File handling"]
tags = []
+++

## Task

Load the entire contents of some text file as a single string variable.

If applicable, discuss: encoding selection, the possibility of memory-mapping.

Of course, in practice one should avoid reading an entire file at once
if the file is large and the task can be accomplished incrementally instead
(in which case check [[File IO]]);
this is for those cases where having the entire file is actually what is wanted.





## 8th

The "slurp" word will read the entire contents of the file into memory, as-is, and give a "buffer".   The ">s" converts that to a string, again "as-is"

```forth

"somefile.txt" f:slurp >s

```


## Ada


=== Ada.Direct_IO ===

Using Ada.Directories to first ask for the file size
and then Ada.Direct_IO to read the whole file in one chunk:


```Ada
with Ada.Directories,
     Ada.Direct_IO,
     Ada.Text_IO;

procedure Whole_File is

   File_Name : String  := "whole_file.adb";
   File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

   subtype File_String    is String (1 .. File_Size);
   package File_String_IO is new Ada.Direct_IO (File_String);

   File     : File_String_IO.File_Type;
   Contents : File_String;

begin
   File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                               Name => File_Name);
   File_String_IO.Read  (File, Item => Contents);
   File_String_IO.Close (File);

   Ada.Text_IO.Put (Contents);
end Whole_File;
```


This kind of solution is limited a bit by the fact that the GNAT implementation of Ada.Direct_IO first allocates a copy of the read object on the stack inside Ada.Direct_IO.Read.  On Linux you can use the command "<code>limit stacksize 1024M</code>" to increase the available stack for your processes to 1Gb, which gives your program more freedom to use the stack for allocating objects.

=== POSIX.Memory_Mapping ===

Mapping the whole file into the address space of your process and then overlaying the file with a String object.


```Ada
with Ada.Text_IO,
     POSIX.IO,
     POSIX.Memory_Mapping,
     System.Storage_Elements;

procedure Read_Entire_File is

   use POSIX, POSIX.IO, POSIX.Memory_Mapping;
   use System.Storage_Elements;

   Text_File    : File_Descriptor;
   Text_Size    : System.Storage_Elements.Storage_Offset;
   Text_Address : System.Address;

begin
   Text_File := Open (Name => "read_entire_file.adb",
                      Mode => Read_Only);
   Text_Size := Storage_Offset (File_Size (Text_File));
   Text_Address := Map_Memory (Length     => Text_Size,
                               Protection => Allow_Read,
                               Mapping    => Map_Shared,
                               File       => Text_File,
                               Offset     => 0);

   declare
      Text : String (1 .. Natural (Text_Size));
      for Text'Address use Text_Address;
   begin
      Ada.Text_IO.Put (Text);
   end;

   Unmap_Memory (First  => Text_Address,
                 Length => Text_Size);
   Close (File => Text_File);
end Read_Entire_File;
```


Character encodings and their handling are not really specified in Ada.  What Ada does specify is three different character types (and corresponding string types):
* Character - containing the set of ISO-8859-1 characters.
* Wide_Character - containing the set of ISO-10646 BMP characters.
* Wide_Wide_Character - containing the full set of ISO-10646 characters.
The GNU Ada compiler (GNAT) seems to read in text files as bytes, completely ignoring any operating system information on character encoding. You can use -gnatW8 in Ada 2005 mode to use UTF-8 characters in identifier names.


## AutoHotkey


```AutoHotKey

fileread, varname, C:\filename.txt ; adding "MsgBox %varname%" (no quotes) to the next line will display the file contents.
```

This script works fine as-is provided C:\filename.txt exists.


## AutoIt


```AutoIt

$fileOpen = FileOpen("file.txt")
$fileRead = FileRead($fileOpen)
FileClose($fileOpen)

```



## ALGOL 68

In official ALGOL 68 a '''file''' is composed of pages, lines and characters, however
for [[ALGOL 68 Genie]] and [[ELLA ALGOL 68RS]] this concept is not supported as they
adopt the [[wp:Unix|Unix]] concept of files being "flat", and hence contain only characters.

The book can contain ''new page''s and ''new line''s, are not of any particular
character set, hence are system independent.  The character set is set by a call
to ''make conv'', eg ''make conv(tape, ebcdic conv);'' -
c.f. [[Character_codes#ALGOL_68|Character_codes]] for more details.

'''In official/standard ALGOL 68 only:'''

```algol68
MODE BOOK = FLEX[0]FLEX[0]FLEX[0]CHAR; ¢ pages of lines of characters ¢
BOOK book;

FILE book file;
INT errno = open(book file, "book.txt", stand in channel);

get(book file, book)
```


Once a "book" has been read into a '''book''' array it can still be associated
with a virtual '''file''' and again be accessed with standard '''file''' routines (such
as ''readf'', ''printf'', ''putf'', ''getf'', ''new line'' etc). This means data
can be directly manipulated from a array cached in "core" using ''transput''
(stdio) routines.

'''In official/standard ALGOL 68 only:'''

```algol68
FILE cached book file;
associate(cached book file, book)
```



## AppleScript


```AppleScript
set pathToTextFile to ((path to desktop folder as string) & "testfile.txt")

-- short way: open, read and close in one step
set fileContent to read file pathToTextFile

-- long way: open a file reference, read content and close access
set fileRef to open for access pathToTextFile
set fileContent to read fileRef
close access fileRef
```



## Arturo



```arturo
contents $(file:read "input.txt")
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
   ## empty record separate,
   RS="";
   ## read line (i.e. whole file) into $0
   getline;
   ## print line number and content of line
   print "=== line "NR,":",$0;
}
{
   ## no further line is read printed
   print "=== line "NR,":",$0;
}
```


```awk

#!/usr/bin/awk -f

@include "readfile"

BEGIN {

  str = readfile("file.txt")
  print str

}

```



## BaCon

For string data:


```qbasic
content$ = LOAD$(filename$)
```


For memory mapped binary data:


```freebasic
binary = BLOAD("somefile.bin")
PRINT "First two bytes are: ", PEEK(binary), " ", PEEK(binary+1)
FREE binary
```



## BASIC


Whether or not various encodings are supported is implementation-specific.

```qbasic
DIM f AS STRING
OPEN "file.txt" FOR BINARY AS 1
f = SPACE$(LOF(1))
GET #1, 1, f
CLOSE 1
```


=
## Commodore BASIC
=

```basic
10 rem load the entire contents of some text file as a single string variable.
20 rem should avoid reading an entire file at once if the file is large
30 rem
### ==========================

40 print chr$(14) : rem switch to upper+lowercase character set
50 open 4,8,4,"data.txt,seq,read"
60 n=0
70 for i=0 to 1
80 get#4,x$
90 i=st and 64 : rem check for 'end-of-file'
100 if i=0 then a$=a$+x$ : n=n+1
110 if n=255 then i=1 : rem max string length is 255 only
120 next
130 close 4
140 end
```



## BBC BASIC

In [[BBC BASIC for Windows]] and [[Brandy BASIC]] the maximum string length is 65535 characters.

```bbcbasic
      file% = OPENIN("input.txt")
      strvar$ = ""
      WHILE NOT EOF#file%
        strvar$ += CHR$(BGET#file%)
      ENDWHILE
      CLOSE #file%
```

API version:
```bbcbasic
      file% = OPENIN("input.txt")
      strvar$ = STRING$(EXT#file%, " ")
      SYS "ReadFile", @hfile%(file%), !^strvar$, EXT#file%, ^temp%, 0
      CLOSE #file%
```



## Bracmat

<code>get'(<i>filename</i>,STR):?myString</code>

=={{header|Brainfuck}}==
While the language certainly doesn't support strings in the traditional sense, relaxing the definition to mean any contiguous sequence of null-terminated bytes permits a reasonable facsimile. This <tt>cat</tt> program eschews the simpler byte-by-byte approach (<tt>,[.,]</tt>) to demonstrate the technique.

```bf>
     Keep cell 0 at 0 as a sentinel value
,[>,] Read into successive cells until EOF
<[<]  Go all the way back to the beginning
>[.>] Print successive cells while nonzero
```

```txt
$ curl -Ls rosettacode.org | bf ">,[>,]<[<]>[.>]"
<!DOCTYPE html>
...
</html>
Tape: [0, 60, 33, 68, 79, 67, 84, 89, 80, 69, 32, 104, 116, 109, 108, 62, 10 ... 60, 47, 104, 116, 109, 108, 62, 10, 0]
```



## Brat


```brat
include :file

file.read file_name
```



## C

It is not possible to specify encodings: the file is read as binary data (on some system, the <tt>b</tt> flag is ignored and there's no difference between <tt>"r"</tt> and <tt>"rb"</tt>; on others, it changes the way the "new lines" are treated, but this should not affect <tt>fread</tt>)

```c
#include <stdio.h>
#include <stdlib.h>

int main()
{
  char *buffer;
  FILE *fh = fopen("readentirefile.c", "rb");
  if ( fh != NULL )
  {
    fseek(fh, 0L, SEEK_END);
    long s = ftell(fh);
    rewind(fh);
    buffer = malloc(s);
    if ( buffer != NULL )
    {
      fread(buffer, s, 1, fh);
      // we can now close the file
      fclose(fh); fh = NULL;

      // do something, e.g.
      fwrite(buffer, s, 1, stdout);

      free(buffer);
    }
    if (fh != NULL) fclose(fh);
  }
  return EXIT_SUCCESS;
}
```



###  Memory map

We can memory-map the file.


```c
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

int main()
{
  char *buffer;
  struct stat s;

  int fd = open("readentirefile_mm.c", O_RDONLY);
  if (fd < 0 ) return EXIT_FAILURE;
  fstat(fd, &s);
  /* PROT_READ disallows writing to buffer: will segv */
  buffer = mmap(0, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  if ( buffer != (void*)-1 )
  {
    /* do something */
    fwrite(buffer, s.st_size, 1, stdout);
    munmap(buffer, s.st_size);
  }

  close(fd);
  return EXIT_SUCCESS;
}
```


Memory map on Windows. See MSDN, starting with '''[https://msdn.microsoft.com/en-us/library/windows/desktop/aa366556.aspx File Mapping]'''. In practice, it would be necessary to check for errors, and to take care of large files. Also, this example is using a view on the whole file, but it's possible to create a smaller view.


```c
#include <windows.h>
#include <stdio.h>

int main() {
    HANDLE hFile, hMap;
    DWORD filesize;
    char *p;

    hFile = CreateFile("mmap_win.c", GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    filesize = GetFileSize(hFile, NULL);
    hMap = CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL);
    p = MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0);

    fwrite(p, filesize, 1, stdout);

    CloseHandle(hMap);
    CloseHandle(hFile);
    return 0;
}
```



## C++


```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <iterator>

int main( )
{
    if (std::ifstream infile("sample.txt"))
    {
        // construct string from iterator range
        std::string fileData(std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>());

        cout << "File has " << fileData.size() << "chars\n";

        // don't need to manually close the ifstream; it will release the file when it goes out of scope
        return 0;
   }
   else
   {
      std::cout << "file not found!\n";
      return 1;
   }
}

```



## C#

```c#
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        var fileContents = File.ReadAllText("c:\\autoexec.bat");
        // Can optionally take a second parameter to specify the encoding, e.g. File.ReadAllText("c:\\autoexec.bat", Encoding.UTF8)
    }
}
```



## Clojure

The core function ''slurp'' does the trick; you can specify an encoding as an optional second argument:

```clojure
(slurp "myfile.txt")
(slurp "my-utf8-file.txt" "UTF-8")
```



## CMake

Sets a variable named ''string''.


```cmake
file(READ /etc/passwd string)
```


This works with text files, but fails with binary files that contain NUL characters. CMake truncates the string at the first NUL character, and there is no way to detect this truncation.

The only way to read binary files is to use the ''HEX'' keyword to convert the entire file to a hexadecimal string.


```cmake
file(READ /etc/pwd.db string HEX)
```



## Common Lisp

The following will read and store the file as a sequence of bytes.


```lisp
(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
```

The macro '''with-open-file''' could be passed ''':external-format :utf-8''' on some implementations (which it would pass on to '''open''') so that reading would occur by unicode character but '''(file-length stream)''' would continue to return the number of bytes, not characters, necessary for storing it.


## D


```d
import std.file: read, readText;

void main() {
    // To read a whole file into a dynamic array of unsigned bytes:
    auto data = cast(ubyte[])read("unixdict.txt");

    // To read a whole file into a validated UTF-8 string:
    string txt = readText("unixdict.txt");
}
```



## Delphi

Using TStringList


```Delphi
program ReadAll;

{$APPTYPE CONSOLE}

uses Classes;

var
  i: Integer;
  lList: TStringList;
begin
  lList := TStringList.Create;
  try
    lList.LoadFromFile('c:\input.txt');
    // Write everything at once
    Writeln(lList.Text);
    // Write one line at a time
    for i := 0 to lList.Count - 1 do
      Writeln(lList[i]);
  finally
    lList.Free;
  end;
end.
```



'''Works with''': Delphi 2010 and above


```Delphi
program ReadAll;

{$APPTYPE CONSOLE}

uses
  SysUtils, IOUtils;

begin
// with default encoding:
  Writeln(TFile.ReadAllText('C:\autoexec.bat'));
// with encoding specified:
  Writeln(TFile.ReadAllText('C:\autoexec.bat', TEncoding.ASCII));
  Readln;
end.
```


=={{header|Déjà Vu}}==
To get a string from a file, you need to explicitly decode the binary blob that is read. Currently only UTF-8 is supported by <code>vu</code>.

```dejavu
local :filecontents !decode!utf-8 !read "file.txt"
```



## E



```e><file:foo.txt
.getText()
```


The file is assumed to be in the default encoding.


## Elixir

Two solutions in the FileReader namespace.
File returns a tuple: {:ok, file} is successful or {:error, reason} if unsuccessful. Errors can be caught and turned into error strings via Erlang's :file.format_error function.


```Elixir

defmodule FileReader do
  # Read in the file
  def read(path) do
    case File.read(path) do
      {:ok, body} ->
        IO.inspect body
      {:error,reason} ->
        :file.format_error(reason)
      end
    end

  # Open the file path, then read in the file
  def bit_read(path) do
    case File.open(path) do
      {:ok, file} ->
        # :all can be replaced with :line, or with a positive integer to specify the number of characters to read.
	IO.read(file,:all)
	  |> IO.inspect
      {:error,reason} ->
	:file.format_error(reason)
    end
  end
end

```



## Emacs Lisp

<code>insert-file-contents</code> does all Emacs' usual character coding, magic file names, decompression, format decoding, etc.  (<code>insert-file-contents-literally</code> can avoid that if unwanted.)


```Lisp
(setq my-variable (with-temp-buffer
                    (insert-file-contents "foo.txt")
                    (buffer-string)))
```


(If an existing buffer is visiting the file, perhaps yet unsaved, it may be helpful to take its contents instead of re-reading the file.  <code>find-buffer-visiting</code> can locate such a buffer.)


## Erlang


```erlang
{ok, B} = file:read_file("myfile.txt").
```


This reads the entire file into a binary object.


## Euphoria

''Euphoria cannot natively handle multibyte character encodings.
The openEuphoria team is/was working on supporting it. It may have been implemented by now.''


```euphoria

function load_file(sequence filename)
  integer fn,c
  sequence data
    fn = open(filename,"r") -- "r" for text files, "rb" for binary files
    if (fn = -1) then return {} end if -- failed to open the file

    data = {} -- init to empty sequence
    c = getc(fn) -- prime the char buffer
    while (c != -1) do -- while not EOF
      data &= c -- append each character
      c = getc(fn) -- next char
    end while

    close(fn)
    return data
end function

```


=={{header|F Sharp|F#}}==

```fsharp
// read entire file into variable using default system encoding or with specified encoding
open System.IO
let data = File.ReadAllText(filename)
let utf8 = File.ReadAllText(filename, System.Text.Encoding.UTF8)
```



## Factor



```factor
USING: io.encodings.ascii io.encodings.binary io.files ;

! to read entire file as binary
"foo.txt" binary file-contents

! to read entire file as lines of text
"foo.txt" ascii file-lines
```



## Fantom

Provide the filename to read from as a command-line parameter.

```fantom

class ReadString
{
  public static Void main (Str[] args)
  {
    Str contents := File(args[0].toUri).readAllStr
    echo ("contents: $contents")
  }
}

```



## Forth

```forth
s" foo.txt" slurp-file   ( str len )
```



## Fortran


Reading the entire source file in memory, then printing it. It relies on the SIZE attribute of the INQUIRE statement returning the size of the file in bytes, whereupon the ALLOCATE statement prepares a variable of the right size to receive the whole content. This SIZE facility was introduced with F2003, and prior to F90 there was no ALLOCATE facility: the size of variables was fixed at compile time.


```fortran
program read_file
    implicit none
    integer :: n
    character(:), allocatable :: s

    open(unit=10, file="read_file.f90", action="read", &
         form="unformatted", access="stream")
    inquire(unit=10, size=n)
    allocate(character(n) :: s)
    read(10) s
    close(10)

    print "(A)", s
end program
```



###  Intel Fortran on Windows

Here is a solution using the Windows API to create a memory map of a file. It is used to print the source code of the program on the console.


```fortran
program file_win
    use kernel32
    use iso_c_binding
    implicit none

    integer(HANDLE) :: hFile, hMap, hOutput
    integer(DWORD) :: fileSize
    integer(LPVOID) :: ptr
    integer(LPDWORD) :: charsWritten
    integer(BOOL) :: s

    hFile = CreateFile("file_win.f90" // c_null_char, GENERIC_READ, &
                       0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL)
    filesize = GetFileSize(hFile, NULL)
    hMap = CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL)
    ptr = MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0)

    hOutput = GetStdHandle(STD_OUTPUT_HANDLE)
    s = WriteConsole(hOutput, ptr, fileSize, transfer(c_loc(charsWritten), 0_c_intptr_t), NULL)
    s = CloseHandle(hMap)
    s = CloseHandle(hFile)
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open "input.txt" For Input Encoding "ascii" As #1
Dim fileLen As LongInt = Lof(1) '' get file length in bytes
Dim buffer As String = Space(fileLen) '' allocate a string of size 'fileLen' bytes
Get #1, 1, buffer '' read all data from start of file into the buffer
Print buffer  '' print to console
buffer = ""  '' release memory used by setting buffer to empty
Close #1
Sleep
```



## Frink

The read[URL] function reads the entire contents of a URL.  The encoding can be specified if necessary.

```frink

a = read["file:yourfile.txt"]
b = read["file:yourfile.txt", "UTF-8"]

```



## FutureBasic


Note: This code goes beyond simply specifying the file to open. It includes a dialog window that allows the user to select a text file to read. Depending on system memory, as many as 4.2 billion characters can be read. The file contents are placed in a convenient console window with automatic save as, copy and paste, select all and undo commands. (Did I mention that FutureBasic -- or FB as developers prefer to call it -- is handy for Macintosh development!) Of course, the programmer is free to code his own window and menu options.

```futurebasic

include "ConsoleWindow"

local fn ReadTextFile
dim as CFURLRef      fileRef
dim as Handle        h
dim as CFStringRef   cfStr : cfStr = NULL
dim as long          fileLen

if ( files$( _CFURLRefOpen, "TEXT", "Select text file...", @fileRef ) )
   open "i", 2, fileRef
   fileLen = lof( 2, 1 )
   h = fn NewHandleClear( fileLen )
      if ( h )
         read file 2, [h], fileLen
         close #2
         cfStr = fn CFStringCreateWithBytes( _kCFAllocatorDefault, #[h], fn GetHandleSize(h), _kCFStringEncodingMacRoman, _false )
         fn DisposeH( h )
      end if
else
// User canceled
end if

fn HIViewSetText( sConsoleHITextView, cfStr )
CFRelease( cfStr )
end fn

fn ReadTextFile

```

This can be shortened considerably by wrapping Objective-C code:

```txt

include "ConsoleWindow"

local fn ReadTextFile
dim as CFURLRef    fileRef
dim as CFStringRef cfStr : cfStr = NULL

if ( files$( _CFURLRefOpen, "TEXT", "Select text file...", @fileRef ) )
BeginCCode
cfStr = (CFStringRef)[[NSString alloc] initWithContentsOfURL:(NSURL *)fileRef encoding:NSUTF8StringEncoding error:nil];
EndC
fn HIViewSetText( sConsoleHITextView, cfStr )
CFRelease( cfStr )
else
// User canceled
end if
end fn

fn ReadTextFile

```



## GAP


```gap
InputTextFile("input.txt");
s := ReadAll(f);;  # two semicolons to hide the result, which may be long
CloseStream(f);
```



## Gambas


```gambas
Public Sub Form_Open()
Dim sFile As String

sFile = File.Load(User.home &/ "file.txt")

End
```



## Genie


```genie
[indent=4]
/*
   Read entire file, in Genie

   valac readEntireFile.gs
   ./readEntireFile [filename]
*/

init

    fileName:string
    fileContents:string
    fileName = (args[1] is null) ? "readEntireFile.gs" : args[1]

    try
        FileUtils.get_contents(fileName, out fileContents)
    except exc:Error
        print "Error: %s", exc.message
        return

    stdout.printf("%d bytes read from %s\n", fileContents.length, fileName)
```


```txt
prompt$ valac readEntireFile.gs
prompt$ ./readEntireFile
443 bytes read from readEntireFile.gs
prompt$ ./readEntireFile nofile
Error: Failed to open file ?nofile?: No such file or directory
prompt$ ./readEntireFile leapYear.gs
291 bytes read from leapYear.gs
```



## Go

Go has good support for working with strings as UTF-8, but there is no requirement that strings be UTF-8 and in fact they can hold arbitrary data.
<tt>ioutil.ReadFile</tt> returns the contents of the file unaltered as a byte array.
The conversion in the next line from byte array to string also makes no changes to the data.
In the example below <tt>sv</tt> will have an exact copy of the data in the file, without regard to encoding.

```go
import "io/ioutil"

data, err := ioutil.ReadFile(filename)
sv := string(data)
```

Go also supports memory mapped files on OSes with a mmap syscall (e.g. Unix-like).
The following prints the contents of "file".
(The included "build constraint" prevents this from being compiled on architectures known to lack <tt>syscall.Mmap</tt>, another source file with the opposite build constraint could use <tt>ioutil.ReadFile</tt> as above).

```go
// +build !windows,!plan9,!nacl // These lack syscall.Mmap

package main

import (
    "fmt"
    "log"
    "os"
    "syscall"
)

func main() {
    f, err := os.Open("file")
    if err != nil {
        log.Fatal(err)
    }
    fi, err := f.Stat()
    if err != nil {
        log.Fatal(err)
    }
    data, err := syscall.Mmap(int(f.Fd()), 0, int(fi.Size()),
        syscall.PROT_READ, syscall.MAP_PRIVATE)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(data))
}
```



## Groovy


```groovy
def fileContent = new File("c:\\file.txt").text
```



## GUISS



```guiss
Start,Programs,Accessories,Notepad,Menu:File,Open,Doubleclick:Icon:Notes.TXT,Button:OK
```



## Haskell

In the IO monad:


```haskell
do text <- readFile filepath
   -- do stuff with text
```


Note that <code>readFile</code> is lazy. If you want to ensure the entire file is read in at once, before any other IO actions are run, try:


```haskell
eagerReadFile :: FilePath -> IO String
eagerReadFile filepath = do
    text <- readFile filepath
    last text `seq` return text
```


=={{header|Icon}} and {{header|Unicon}}==
The first code snippet below reads from stdin directly into the string fs,
preserving line separators (if any) and reading in large chunks.

```Icon
every (fs := "") ||:= |reads(1000000)
```

The second code snippet below performs the same operation using an intermediate list fL and applying a function (e.g. FUNC) to each line. Use this form when you need to perform additional string functions such as 'trim' or 'map' on each line.  This avoids unnecessary garbage collections which will occur with larger files.  The list can be discarded when done.  Line separators are mapped into newlines.

```Icon
every put(fL := [],|FUNC(read()))
every (fs := "") ||:= !fL || "\n"
fL := &null
```



## Inform 7

File access is sandboxed by the interpreter, so this solution essentially requires that the file have been previously written by an Inform program running from the same location under the same interpreter.

```inform7
Home is a room.

The File of Testing is called "test".

When play begins:
	say "[text of the File of Testing]";
	end the story.
```



## J



```j
   require 'files'         NB. not needed for J7 & later
   var=: freads 'foo.txt'
```


To memory map the file:


```j
   require 'jmf'
   JCHAR map_jmf_ 'var';'foo.txt'
```


Caution: updating the value of the memory mapped variable will update the file, and this characteristic remains when the variable's value is passed, unmodified, to a verb which modifies its own local variables.


## Java

There is no single method to do this in Java 6 and below (probably because reading an entire file at once could fill up your memory quickly), so to do this you could simply append the contents as you read them into a buffer.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadFile {
    public static void main(String[] args) throws IOException{
        String fileContents = readEntireFile("./foo.txt");
    }

    private static String readEntireFile(String filename) throws IOException {
        FileReader in = new FileReader(filename);
        StringBuilder contents = new StringBuilder();
        char[] buffer = new char[4096];
        int read = 0;
        do {
            contents.append(buffer, 0, read);
            read = in.read(buffer);
        } while (read >= 0);
        in.close();
        return contents.toString();
    }
}
```


One can memory-map the file in Java, but there's little to gain if one is to create a String out of the file:


```java

import java.nio.channels.FileChannel.MapMode;
import java.nio.MappedByteBuffer;
import java.io.RandomAccessFile;
import java.io.IOException;
import java.io.File;

public class MMapReadFile {
	public static void main(String[] args) throws IOException {
		MappedByteBuffer buff = getBufferFor(new File(args[0]));
                String results = new String(buff.asCharBuffer());
	}

	public static MappedByteBuffer getBufferFor(File f) throws IOException {
		RandomAccessFile file = new RandomAccessFile(f, "r");

		MappedByteBuffer buffer = file.getChannel().map(MapMode.READ_ONLY, 0, f.length());
		file.close();
		return buffer;
	}
}
```


or one can take a shortcut:

```java
String content = new Scanner(new File("foo"), "UTF-8").useDelimiter("\\A").next();
```

this works because Scanner will search the file for a delimiter and return everything before that. <code>\A</code> is the beginning of the file, which it will never find until the end of the file is reached.

Java 7 added <code>java.nio.file.Files</code> which has two methods for accomplishing this task: <code>Files.readAllLines</code> and <code>Files.readAllBytes</code>:

```java5
import java.util.List;
import java.nio.charset.Charset;
import java.nio.file.*;

public class ReadAll {
	public static List<String> readAllLines(String filesname){
		Path file = Paths.get(filename);
		return Files.readAllLines(file, Charset.defaultCharset());
	}

	public static byte[] readAllBytes(String filename){
		Path file = Paths.get(filename);
		return Files.readAllBytes(file);
	}
}
```



## JavaScript

This works in IExplorer or a standalone js file. Note the similarity to the VBScript code.

```javascript
var fso=new ActiveXObject("Scripting.FileSystemObject");
var f=fso.OpenTextFile("c:\\myfile.txt",1);
var s=f.ReadAll();
f.Close();
try{alert(s)}catch(e){WScript.Echo(s)}
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
	alert(fileString);
}
function errorHandler(event) {
	alert(event);
}

```



## jq

The . filter will read in a file of raw text, e.g. if the file is named input.txt and we wanted to emit it as a single JSON string:

```sh
jq -R -s . input.txt
```

In practice, this is probably not very useful.  It would be more typical to collect the raw lines into an array of JSON strings.

If it is known that the lines are delimited by a single "newline" character, then one could simply pipe from one jq command to another:
```sh
jq -R . input.txt | jq -s .
```

Equivalently:
```sh
jq -R -s 'split("\n")' input.txt
```


Other cases can be similarly handled.


## Jsish


```javascript
var contents = File.read("filename")
```


From the shell:
```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]'
# var contents = File.read("README")
variable
# contents
"Jsi is a small javascript interpreter for embedded-C development.

Internally Jsi emulates Tcl.  The parser and execution engine originate from quad_wheel,

It is being developed under an MIT license.

"
```



## Julia

The built-in function <code>readstring</code> reads a whole file into a string (assuming UTF8 encoding), or you can also use read to read the content into an array of bytes:

```Julia
readstring("/devel/myfile.txt") # read file into a string
read("/devel/myfile.txt", filesize("/devel/myfile.txt")) # read file into an array of bytes
```

Alternatively, there are a variety of ways to memory-map the file, here as an array of bytes:

```Julia
A = Mmap.mmap(open("/devel/myfile.txt"), Array{UInt8,1})
```



## Kotlin


```scala
import java.io.File

fun main(args: Array<String>) {
    println(File("unixdict.txt").readText(charset = Charsets.UTF_8))
}
```



## LabVIEW

{{VI snippet}}<br/>[[File:LabVIEW Read entire file.png]]


## Lang5


```lang5
'foo.txt slurp
```



## Lasso

By default, string objects, which are always Unicode, are created with the assumption that the file contains UTF-8 encoded data. This assumption can be changed by settings the file objects’s character encoding value. When reading the data as a bytes object, the unaltered file data is returned.


```Lasso
local(f) = file('foo.txt')
#f->readString</lang >


## LFE



```lisp

(set `#(ok ,data) (file:read_file "myfile.txt"))

```



## Liberty BASIC


```lb
filedialog "Open a Text File","*.txt",file$
if file$<>"" then
    open file$ for input as #1
    entire$ = input$(#1, lof(#1))
    close #1
    print entire$
end if
```



## Lingo


```lingo
----------------------------------------
-- Reads whole file, returns string
-- @param {string} tFile
-- @return {string|false}
----------------------------------------
on readFile (tFile)
  fp = xtra("fileIO").new()
  fp.openFile(tFile, 1)
  if fp.status() then return false
  res = fp.readFile()
  fp.closeFile()
  return res
end
```



## LiveCode


Livecode offers 2 ways:

Using URL

```LiveCode
put URL "file:///usr/share/dict/words" into tVar
put the number of lines of tVar
```


Using file open + read + close

```LiveCode
local tFile,tLinecount
put "/usr/share/dict/words" into tFile
open file tFile for text read
read from file tFile until EOF
put the number of lines of it  -- file contents held in "it" variable
close file tFile
```



## Lua


```Lua

--If the file opens with no problems, io.open will return a
--handle to the file with methods attached.
--If the file does not exist, io.open will return nil and
--an error message.
--assert will return the handle to the file if present, or
--it will throw an error with the message returned second
--by io.open.
local file = assert(io.open(filename))
--Without wrapping io.open in an assert, local file would be nil,
--which would cause an 'attempt to index a nil value' error when
--calling file:read.

--file:read takes the number of bytes to read, or a string for
--special cases, such as "*a" to read the entire file.
local contents = file:read'*a'

--If the file handle was local to the expression
--(ie. "assert(io.open(filename)):read'a'"),
--the file would remain open until its handle was
--garbage collected.
file:close()

```




## M2000 Interpreter


```M2000 Interpreter

Module checkit {
      \\ prepare a file
      \\ Save.Doc and Append.Doc  to file, Load.Doc and Merge.Doc from file
      document a$
      a$={First Line
            Second line
            Third Line
            Ελληνικά Greek Letters
            }
      Save.Doc a$, "checkthis.txt", 2  ' 2 for UTF-8

      Buffer1=Buffer("checkthis.txt")
      Print Len(Buffer1)=Filelen("checkthis.txt")
      b$=String$(Eval$(Buffer1, 0) as UTF8Dec)
      Report b$
      openfile$=FILE$("text file","txt")
      Merge.doc a$, openfile$
      Edit.Doc a$
}
checkit

```




## M4

An approximation to file reading can be had by <code>include()</code> which reads a file as M4 input.  If it's inside a <code>define()</code> then the input is captured as a definition.  But this is extremely limited since any macro names, parens, commas, quote characters etc in the file will expand and upset the capture.


```m4
define(`foo',include(`file.txt'))
defn(`foo')
defn(`foo')
```



## Make

```Make
contents := $(shell cat foo.txt)
```

This is [http://www.gnu.org/software/make/manual/html_node/Shell-Function.html from the GNU Make manual].  As noted there, newlines are converted to spaces in the <code>$(contents)</code> variable.  This might be acceptable for files which are a list of words anyway.


## Maple

First solution:

```Maple

s1 := readbytes( "file1.txt", infinity, TEXT ):

```

Second solution:

```Maple

s2 := FileTools:-Text:-ReadFile( "file2.txt" ):

```



## Mathematica


```Mathematica
Import["filename","String"]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
  fid = fopen('filename','r');
  [str,count] = fread(fid, [1,inf], 'uint8=>char');  % s will be a character array, count has the number of bytes
  fclose(fid);
```



## Mercury


```Mercury
:- module read_entire_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
   io.open_input("file.txt", OpenResult, !IO),
   (
      OpenResult = ok(File),
      io.read_file_as_string(File, ReadResult, !IO),
      (
           ReadResult = ok(FileContents),
           io.write_string(FileContents, !IO)
      ;
           ReadResult = error(_, IO_Error),
           io.stderr_stream(Stderr, !IO),
           io.write_string(Stderr, io.error_message(IO_Error) ++ "\n", !IO)
      )
   ;
      OpenResult = error(IO_Error),
      io.stderr_stream(Stderr, !IO),
      io.write_string(Stderr, io.error_message(IO_Error) ++ "\n", !IO)
   ).
```




## Neko


```ActionScript
/**
 Read entire file
 Tectonics:
   nekoc read-entire-file.neko
   neko read-entire-file
*/

var file_contents = $loader.loadprim("std@file_contents", 1);

try {
  var entire_file = file_contents("read-entire-file.neko");
  $print("Read: ", $ssize(entire_file), " bytes\n");
} catch e {
  $print("Exception: ", e, "\n");
}
```


```txt
prompt$ nekoc read-entire-file.neko
prompt$ neko read-entire-file.n
Read: 325 bytes
```




## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg inFileName .

if inFileName = '' | inFileName = '.' then inFileName = './data/dwarfs.json'
fileContents = slurp(inFileName)
say fileContents

return

-- Slurp a file and return contents as a Rexx string
method slurp(inFileName) public static returns Rexx

  slurped = Rexx null
  slurpStr = StringBuilder()
  ioBuffer = byte[1024]
  inBytes = int 0

  do
    inFile = File(inFileName)
    inFileIS = BufferedInputStream(FileInputStream(inFile))

    loop label ioLoop until inBytes = -1
      slurpStr.append(String(ioBuffer, 0, inBytes))
      inBytes = inFileIS.read(ioBuffer)
      end ioLoop

  catch exFNF = FileNotFoundException
    exFNF.printStackTrace
  catch exIO = IOException
    exIO.printStackTrace
  finally
    do
      inFileIS.close()
    catch ex = IOException
      ex.printStackTrace
    end
  end

  slurped = Rexx(slurpStr.toString)

  return slurped

```



## NewLISP


```NewLISP
(read-file "filename")
```



## Microsoft Small Basic


```smallbasic
  v=File.ReadContents(filename)
```



## Nim


```Nim
readFile(filename)
```



## Objeck


```objeck

string := FileReader->ReadFile("in.txt");

```


=={{header|Objective-C}}==

```objc

    /*** 0. PREPARATION    */
    // We need a text file to read; let's redirect a C string to a new file
    // using the shell by way of the stdlib system() function.
    system ("echo \"Hello, World!\" > ~/HelloRosetta");



    /*** 1. THE TASK      */
    // Instantiate an NSString which describes the filesystem location of
    // the file we will be reading.
    NSString *filePath = [NSHomeDirectory() stringByAppendingPathComponent:@"HelloRosetta"];

    // The selector we're going to use to complete this task,
    // stringWithContentsOfFile:encoding:error, has an optional `error'
    // parameter which can be used to return information about any
    // errors it might run into. It's optional, but we'll create an NSError
    // anyways to demonstrate best practice.
    NSError *anError;

    // And finally, the task: read and store the contents of a file as an
    // NSString.
    NSString *aString = [NSString stringWithContentsOfFile:filePath
                                                  encoding:NSUTF8StringEncoding
                                                     error:&anError];

    // If the file read was unsuccessful, display the error description.
    // Otherwise, display the NSString.
    if (!aString) {
        NSLog(@"%@", [anError localizedDescription]);
    } else {
        NSLog(@"%@", aString);
    }

```



## OCaml


For most uses we can use this function:


```ocaml
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
```


We can replace the last line with the one below if we want to return a type <code>string</code> instead of <code>bytes</code>:

```ocaml
  (Bytes.unsafe_to_string s)
```


There is no problem reading an entire file with the function <code>really_input</code> because this function is implemented appropriately with an internal loop, but it can only load files which size is equal or inferior to the maximum length of an ocaml string. This maximum size is available with the variable <code>Sys.max_string_length</code>. On 32 bit machines this size is about 16Mo.

To load bigger files several solutions exist, for example create a structure that contains several strings where the contents of the file can be split. Or another solution that is often used is to use a bigarray of chars instead of a string:


```ocaml
type big_string =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
```


The function below returns the contents of a file with this type <code>big_string</code>, and it does so with "memory-mapping":


```ocaml
let load_big_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let shared = false in  (* modifications are done in memory only *)
  let bstr = Bigarray.Array1.map_file fd
               Bigarray.char Bigarray.c_layout shared len in
  Unix.close fd;
  (bstr)
```


Then the length of the data can be get with <code>Bigarray.Array1.dim</code> instead of <code>String.length</code>, and we can access to a given char with the syntactic sugar <code>bstr.{i}</code> (instead of <code>str.[i]</code>) as shown in the small piece of code below (similar to the cat command):


```ocaml
let () =
  let bstr = load_big_file Sys.argv.(1) in
  let len = Bigarray.Array1.dim bstr in
  for i = 0 to pred len do
    let c = bstr.{i} in
    print_char c
  done
```



## Ol


```scheme

(define content (bytes->string
   (vec-iter
      (file->vector "file.txt"))))

(print content)

```



## ooRexx


### version 1


```ooRexx
file = 'c:\test.txt'
myStream = .stream~new(file)
myString = myStream~charIn(,myStream~chars)
```


Streams are opened on demand and closed when the script finishes.
It is possible if you wish to open and close the streams explicitly


```ooRexx
file = 'c:\test.txt'
myStream = .stream~new(file)
if mystream~open('read') = 'READY:'
then do
   myString = myStream~charIn(,myStream~chars)
   myStream~close
end
```



### version 2  EXECIO

One can also use EXECIO as it is known from VM/CMS and MVS/TSO:

```rexx
address hostemu 'execio * diskr "./st.in" (finis stem in.'
Say in.0 'lines in file st.in'
v=''
Do i=1 To in.0
  Say i '>'in.i'<'
  v=v||in.i
  End
say 'v='v
::requires "hostemu" LIBRARY
```

```txt
E:\>rexx ref
6 lines in file st.in
1 >address hostemu 'execio * diskr "./st.in" (finis stem in.'<
2 >Say in.0<
3 >Do i=1 To in.0<
4 >  Say i '>'in.i'<'<
5 >  End<
6 >::requires "hostemu" LIBRARY<
v=address hostemu 'execio * diskr "./st.in" (finis stem in.'Say in.0Do i=1 To in
.0  Say i '>'in.i'<'  End::requires "hostemu" LIBRARY

```



## OxygenBasic

Two Formats:

```txt


string s

'AS FUNCTION

s=GetFile "t.txt"

'AS PROCEDURE

Getfile "t.txt",s


```



## Oz

The interface for file operations is object-oriented.

```oz
declare
  FileHandle = {New Open.file init(name:"test.txt")}
  FileContents = {FileHandle read(size:all list:$)}
in
  {FileHandle close}
  {System.printInfo FileContents}
```

<code>FileContents</code> is a list of bytes. The operation does not assume any particular encoding.


## PARI/GP

The GP interpreter's ability to read files is extremely limited; reading an entire file is almost all that it can do.  The C code PARI library is not similarly limited.

<code>readstr()</code> returns a vector of strings which are the file lines, without newlines.  They can be concatenated to make a single string.


```parigp
str = concat(apply(s->concat(s,"\n"), readstr("file.txt")))
```


Since <code>readstr()</code> returns strings without newlines there's no way to tell whether the last line had a newline or not.  This is fine for its intended use on text files, but not good for reading binary files.


## Panda

It returns a unicode string of type 'text'.

```panda>file:readme.txt .text</lang



## Pascal

See TStrignList example of [[Read_entire_file#Delphi | Delphi]]


## Perl


The modern recommended way, is using one of these CPAN modules:

*
```perl
use File::Slurper 'read_text';
my $text = read_text($filename, $data);
```

*
```perl
use Path::Tiny;
my $text = path($filename)->slurp_utf8;
```

*
```perl
use IO::All;
$text = io($filename)->utf8->all;
```


Traditional ways, without CPAN modules:

*
```perl
open my $fh, '<:encoding(UTF-8)', $filename or die "Could not open '$filename':  $!";
my $text;
read $fh, $text, -s $filename;
close $fh;
```

*
```perl
my $text;
{
  local $/ = undef;
  open my $fh, '<:encoding(UTF-8)', $filename or die "Could not open '$filename':  $!";
  $text = <$fh>;
  close $fh;
}
```

*
```perl
my $text = do { local( @ARGV, $/ ) = ( $filename ); <> };
```


For a one-liner from shell, use <code>-0[code]</code>. It normally specifies the oct char code of record separator (<code>$/</code>), so for example <code>perl -n -040</code> would read chunks of text ending at each space (<code>$/ = ' '</code>).  However, <code>-0777</code> has special meaning: <code>$/ = undef</code>, so the whole file is read in at once (<code>chr 0777</code> happens to be "ǿ", but Larry doesn't think one should use that as record separator).

```perl
perl -n -0777 -e 'print "file len: ".length' stuff.txt
```


===Memory-mapping===

```Perl
use File::Map 'map_file';
map_file(my $str, "foo.txt");
print $str;
```



```Perl
use Sys::Mmap;
Sys::Mmap->new(my $str, 0, 'foo.txt')
  or die "Cannot Sys::Mmap->new: $!";
print $str;
```


<code>File::Map</code> has the advantage of not requiring an explicit <code>munmap()</code>.  Its tie is faster than the tie form of <code>Sys::Mmap</code> too.


## Perl 6


```perl6
my $string = slurp 'sample.txt';
```


The default encoding is UTF-8. The <tt>:enc</tt> adverb can be used to specify a different one:


```perl6
my $string = slurp 'sample.txt', :enc<UTF-16>;
```


<tt>IO::Path</tt> objects also provide <tt>slurp</tt> as a method:

```perl6
my $string = 'sample.txt'.IO.slurp;
```



## Phix


```Phix
constant fn = open(command_line()[2],"rb")
?get_text(fn)
close(fn)
{} = wait_key()
```

```txt

"constant fn = open(command_line()[2],\"rb\")\r\n?get_text(fn)\r\nclose(fn)\r\n{} = wait_key()\r\n"

```

The value returned by get_text is actually a string containing raw binary data (no \r\n -> \n substitution, even if the file is opened in text mode) and is not limited to text files.

There is no builtin method for handling different encodings, but demo\edita handles all such files with ease, including the nifty little encoding drop-down on the open/close dialog.


## PHP

Read as string

```php
file_get_contents($filename)
```

Read as array, one element per line

```php
file($filename)
```



## PicoLisp

Using '[http://software-lab.de/doc/refT.html#till till]' is the shortest way:

```PicoLisp
(in "file" (till NIL T))
```

To read the file into a list of characters:

```PicoLisp
(in "file" (till NIL))
```

or, more explicit:

```PicoLisp
(in "file" (make (while (char) (link @))))
```

Encoding is always assumed to be UTF-8.


## Pike


```pike
string content=Stdio.File("foo.txt")->read();
```



## PL/I


```PL/I

get file (in) edit ((substr(s, i, 1) do i = 1 to 32767)) (a);

```



## PowerShell


```powershell
Get-Content foo.txt
```

This will only detect Unicode correctly with a BOM in place (even for UTF-8). With explicit selection of encoding:

```powershell
Get-Content foo.txt -Encoding UTF8
```

However, both return an array of strings which is fine for pipeline use but if a single string is desired the array needs to be joined:

```powershell
(Get-Content foo.txt) -join "`n"
```



## PureBasic

A file can be read with any of the built in commands

```PureBasic
Number.b = ReadByte(#File)
Length.i = ReadData(#File, *MemoryBuffer, LengthToRead)
Number.c = ReadCharacter(#File)
Number.d = ReadDouble(#File)
Number.f = ReadFloat(#File)
Number.i = ReadInteger(#File)
Number.l = ReadLong(#File)
Number.q = ReadQuad(#File)
Text$    = ReadString(#File [, Flags])
Number.w = ReadWord(#File)
```

If the file is s pure text file (no CR/LF etc.), this will work and will read each line untill EOL is found.

```PureBasic
If ReadFile(0, "RC.txt")
  Variable$=ReadString(0)
  CloseFile(0)
EndIf
```

Since PureBasic terminates strings with a #NULL and also split the ReadString() is encountering new line chars, any file containing these must be treated as a data stream.

```PureBasic
Title$="Select a file"
Pattern$="Text (.txt)|*.txt|All files (*.*)|*.*"
fileName$ = OpenFileRequester(Title$,"",Pattern$,0)
If fileName$
  If ReadFile(0, fileName$)
    length = Lof(0)
    *MemoryID = AllocateMemory(length)
    If *MemoryID
      bytes = ReadData(0, *MemoryID, length)
      MessageRequester("Info",Str(bytes)+" was read")
    EndIf
    CloseFile(0)
  EndIf
EndIf
```



## Python



```python
open(filename).read()
```


This returns a byte string and does not assume any particular encoding.

In Python 3 strings are in unicode, you can specify encoding when reading:


```python
open(filename, encoding='utf-8').read()
```


Python docs recommend dealing with files using the with statement:


```python

with open(filename) as f:
    data = f.read()

```



## Q



```Q
q)file:read0`:file.txt
"First line of file"
"Second line of file"
""
```



## R



```r
fname <- "notes.txt"
contents <- readChar(fname, file.info(fname)$size)
```



## Racket



```racket
(file->string "foo.txt")
```



## Raven


```Raven
'myfile.txt' read as $content_as_string
```

or

```Raven
'file://r:/home/me/myfile.txt' open as $handle
$handle read as $content_as_string
$handle close
```



## REALbasic

This function accepts a file (FolderItem object) and an optional TextEncoding class. If the TextEncoding is not defined, then REALbasic defaults to UTF-8. Since it is intended for cross-platform development, REALbasic has a number of built-in tools for working with different text encodings, line terminators, etc. [http://docs.realsoftware.com/index.php/TextEncoding]

```realbasic

Function readFile(theFile As FolderItem, txtEncode As TextEncoding = Nil) As String
  Dim fileContents As String
  Dim tis As TextInputStream
  tis = tis.Open(theFile)
  fileContents = tis.ReadAll(txtEncode)
  tis.Close
  Return fileContents

Exception err As NilObjectException
  MsgBox("File Not Found.")
End Function

```



## REBOL


```rebol
read %my-file  ; read as text
read/binary %my-file       ; preserve contents exactly
```



## Retro


```Retro

here 'input.txt file:slurp
```



## REXX


### using LINEIN


```rexx
/*REXX program reads an entire file line-by-line  and  stores it as a continuous string.*/
parse arg iFID .                                 /*obtain optional argument from the CL.*/
if iFID==''  then iFID= 'a_file'                 /*Not specified?  Then use the default.*/
$=                                               /*a string of file's contents (so far).*/
             do  while lines(iFID)\==0           /*read the file's lines until finished.*/
             $=$ || linein(iFID)                 /*append a (file's) line to the string,*/
             end   /*while*/                     /*stick a fork in it,  we're all done. */
```



### using CHARIN

Note that CRLF are in the resulting string.
<lang>/*REXX program reads a file and stores it as a continuous character str.*/
Parse Version v
iFID = 'st.in'                         /*name of the input file.        */
If left(v,11)='REXX-Regina' |,
   left(v,11)='REXX-ooRexx' Then Do
  len=chars(iFid)                      /*size of the file               */
  v = charin(iFid,,len)                /*read entire file               */
  End
Else Do                                /* for other Rexx Interpreters   */
  v=''
  Do while chars(iFid)>0               /* read the file chunk by chunk  */
    v=v||charin(iFid,,500)
    End
  End
say 'v='v
say 'length(v)='length(v)

```

```txt
E:\>rexx refc
v=line 1 of 3
line 2 of 3
line 3 of 3

length(v)=39
```



## Ring


```ring

# Read the file
cStr = read("myfile.txt")
# print the file content
See cStr

```


Also in one line we can read and print the file content.


```ring

cStr = read("myfile.txt")   See cStr

```


We can avoid the string, but it's required in the task.


```ring

See read("myfile.txt")

```



## Ruby

IO.read is for text files. It uses the default text encodings, and on Microsoft Windows, it also converts "\r\n" to "\n".


```ruby
# Read entire text file.
str = IO.read "foobar.txt"

# It can also read a subprocess.
str = IO.read "| grep ftp /etc/services"
```


''Caution!'' IO.read and File.read take a portname. To open an arbitrary path (which might start with "|"), you must use File.open, then IO#read.


```ruby
path = "|strange-name.txt"
str = File.open(path) {|f| f.read}
```


To read a binary file, open it in binary mode.


```ruby
# Read entire binary file.
str = File.open(path, "rb") {|f| f.read}
```


Ruby 1.9 can read text files in different encodings.

```ruby
# Read EUC-JP text from file.
str = File.open(path, "r:euc-jp") {|f| f.read}

# Read EUC-JP text from file; transcode text from EUC-JP to UTF-8.
str = File.open(path, "r:euc-jp:utf-8") {|f| f.read}
```



## Run BASIC


```Runbasic
open DefaultDir$ + "/public/test.txt" for binary as #f
fileLen = LOF(#f)
a$ = input$(#f, fileLen)
print a$
close #f
```



## Rust



```rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut file = File::open("somefile.txt").unwrap();

    let mut contents: Vec<u8> = Vec::new();
    // Returns amount of bytes read and append the result to the buffer
    let result = file.read_to_end(&mut contents).unwrap();
    println!("Read {} bytes", result);

    // To print the contents of the file
    let filestr = String::from_utf8(contents).unwrap();
    println!("{}", filestr);
}
```



## Scala

```scala
object TextFileSlurper extends App {
  val fileLines =
    try scala.io.Source.fromFile("my_file.txt", "UTF-8").mkString catch {
      case e: java.io.FileNotFoundException => e.getLocalizedMessage()
    }
}
```



## Scheme

Uses SRFI-13:

```scheme
(with-input-from-file "foo.txt"
  (lambda ()
    (reverse-list->string
     (let loop ((char (read-char))
                (result '()))
       (if (eof-object? char)
           result
           (loop (read-char) (cons char result)))))))
```


Works with Chicken Scheme:

```scheme
(with-input-from-file "foo.txt" read-string)
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/getf.htm getf.s7i]
defines the function [http://seed7.sourceforge.net/libraries/getf.htm#getf%28in_string%29 getf],
which reads a whole file into a string:

```seed7
$ include "seed7_05.s7i";
  include "getf.s7i";

const proc: main is func
  local
    var string: fileContent is "";
  begin
    fileContent := getf("text.txt");
  end func;
```



## Sidef

Reading an entire file as a string, can be achieved with the '''FileHandle.slurp()''' method, as illustrated bellow:

```ruby
var file = File.new(__FILE__);
var content = file.open_r.slurp;
print content;
```


Starting with version 2.30, ''File.read()'' can do the same:

```ruby
var file = File(__FILE__)
var content = file.read(:utf8)
print content
```



## Smalltalk

```smalltalk
(StandardFileStream oldFileNamed: 'foo.txt') contents
```

```smalltalk
'foo.txt' asFilename contentsAsString
```



## SNOBOL4

In SNOBOL4, file I/O is done by associating a variable with the desired file, via the input() built-in function.  After the association, each reference to the named variable provides as the variable's value the next block or line of data from the corresponding file.  The exact format of the input() function parameters tends to vary based on the implementation in use.  In this example, the code reads the file in blocks of 512k bytes (or less) until the entire file has been read into one long string in memory.


```SNOBOL4
      input(.inbin,21,"filename.txt [-r524288]")     :f(end)
rdlp  buf = inbin                                    :s(rdlp)
*
*  now process the 'buf' containing the file
*
end
```



## Sparkling


```sparkling
let contents = readfile("foo.txt");
```


## SPL


```spl
text = #.readtext("filename.txt")
```



## Stata

It's possible to get the entire content as an array of lines with '''[http://www.stata.com/help.cgi?mf_cat cat]'''. However, here we want a single string. See '''[http://www.stata.com/help.cgi?mf_fopen fopen]''' and related functions.


```stata
mata
f = fopen("somedata.txt", "r")
fseek(f, 0, 1)
n = ftell(f)
fseek(f, 0, -1)
s = fread(f, n)
fclose(f)
end
```



## Swift


```Swift
import Foundation

let path = "~/input.txt".stringByExpandingTildeInPath
if let string = String(contentsOfFile: path, encoding: NSUTF8StringEncoding) {
  println(string) // print contents of file
}
```



## Tcl

This reads the data in as text, applying the default encoding translations.

```tcl
set f [open $filename]
set data [read $f]
close $f
```

To read the data in as uninterpreted bytes, either use <code>fconfigure</code> to put the handle into binary mode before reading, or (from Tcl 8.5 onwards) do this:

```tcl
set f [open $filename "rb"]
set data [read $f]
close $f
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ERROR/STOP OPEN ("rosetta.txt",READ,-std-)
var=FILE ("rosetta.txt")

```



## TXR



```txr
@(next "foo.txt")
@(freeform)
@LINE

```


The freeform directive in TXR causes the remaining lines of the text stream
to be treated as one big line, catenated together.
The default line terminator is the newline "\n".
This lets the entire input be captured into a single variable
as a whole-line match.


## UNIX Shell


We start a 'cat' process to read the entire file, and use '$(...)' to grab
the output of 'cat'. We use 'printf' which might be more portable than 'echo'.
Because '$(...)' can chop off a newline at the end of the file,
we tell 'printf' to add an extra newline.


```sh
f=`cat foo.txt`    # f will contain the entire contents of the file
printf '%s\n' "$f"
```



```bash
f=$(cat foo.txt)
printf '%s\n' "$f"
```


Some shells provide a shortcut to read a file without starting a 'cat' process.

```bash
f=$(<foo.txt)
echo -E "$f"
```


```bash
file=$(<foo.txt)
print $file
```

alternatively

```bash
zmodload zsh/mapfile
print $mapfile[foo.txt]
```



## Ursa


```ursa
decl string contents
decl file f
f.open "filename.txt"
set contents (f.readall)
```



## Vala


```vala

string file_contents;
FileUtils.get_contents("foo.txt", out file_contents);

```



## VBScript

Read text file with default encoding into variable and display

```vb
dim s
s = createobject("scripting.filesystemobject").opentextfile("slurp.vbs",1).readall
wscript.echo s
```


Read text file with UTF-16 encoding into memory and display

```vb
wscript.echo createobject("scripting.filesystemobject").opentextfile("utf16encoded.txt",1,-1).readall
```



## Vedit macro language

In Vedit Macro Language, a "string variable" can be either an edit buffer or a text register.

Text registers can hold only a limited amount of data (about 120 KB each in current version).

Edit buffers can handle files of unlimited size (even larger than the size of virtual memory).
For large files, only a part of the file is kept in memory, but from users point of view there is no practical difference to having the whole file in memory.

Read file into edit buffer. The buffer is allocated automatically:

```vedit
File_Open("example.txt")
```


Read file into text register 10:

```vedit
Reg_Load(10, "example.txt")
```



## Visual Basic

```vb
Declare Function MultiByteToWideChar Lib "kernel32.dll" ( _
     ByVal CodePage As Long, _
     ByVal dwFlags As Long, _
     ByVal lpMultiByteStr As Long, _
     ByVal cchMultiByte As Long, _
     ByVal lpWideCharStr As Long, _
     ByVal cchWideChar As Long) As Long
Const CP_UTF8 As Long = 65001

Sub Main()
Dim fn As Integer
Dim i As Long
Dim b() As Byte
Dim s As String

  fn = FreeFile()
  Open "c:\test.txt" For Binary Access Read As #fn
  ReDim b(0 To (LOF(fn) - 1))
  Get #fn, 1, b()

  If b(0) = &HFF And b(1) = &HFE Then
  'UTF-16, little-endian
    ReDim b(0 To (LOF(fn) - 3))
    Get #fn, 3, b()
    s = b()
  ElseIf b(0) = &HEF And b(1) = &HBB And b(2) = &HBF Then
  'UTF-8
    i = MultiByteToWideChar(CP_UTF8, 0&, VarPtr(b(3)), LOF(fn) - 3, StrPtr(s), 0)
    s = Space$(i)
    i = MultiByteToWideChar(CP_UTF8, 0&, VarPtr(b(3)), LOF(fn) - 3, StrPtr(s), Len(s))
  Else
  'assume ANSI
    s = StrConv(b(), vbUnicode)
  End If
  Close #fn
  Debug.Print s
End Sub
```



## Visual Basic .NET


```vbnet
Imports System.IO

Public Class Form1

  ' Read all of the lines of a file.
  ' Function assumes that the file exists.
  Private Sub ReadLines(ByVal FileName As String)

    Dim oReader As New StreamReader(FileName)
    Dim sLine As String = oReader.ReadToEnd()

    oReader.Close()

  End Sub

End Class
```



## Wart


```wart
with infile "x"
  with outstring
    whilet line (read_line)
      prn line
```



## XPL0

This example reads its own source code file and displays it as a string.
The command line is: readfile <readfile.xpl


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated string convention
int  I;
char Str;
[Str:= GetHp;           \starting address of block of local "heap" memory
I:= 0;                  \ [does the exact same thing as Reserve(0)]
loop    [Str(I):= ChIn(1);
        if Str(I) = $1A\EOF\ then [Str(I):= 0;  quit];
        I:= I+1;
        ];
SetHp(Str+I+1);         \set heap pointer beyond Str (not really needed here)
Text(0, Str);           \show file as a string
]
```


```txt

include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated string convention
int  I;
char Str;
[Str:= GetHp;           \starting address of block of local "heap" memory
I:= 0;                  \ [does the exact same thing as Reserve(0)]
loop    [Str(I):= ChIn(1);
        if Str(I) = $1A\EOF\ then [Str(I):= 0;  quit];
        I:= I+1;
        ];
SetHp(Str+I+1);         \set heap pointer beyond Str (not really needed here)
Text(0, Str);           \show file as a string
]

```



## Xtend


```java

package com.rosetta.example

import java.io.File
import java.io.PrintStream

class ReadFile {
    def static main( String ... args ) {
        val content = new String(Files.readAllBytes(Paths.get("file.txt")))
    }
}

```



## Yorick

This loads foo.txt into ''lines'' as an array of strings.
Each array element is one line.
Each line's trailing newline is removed.

```yorick
lines = rdfile("foo.txt");
```

This loads foo.txt into ''content'' as a single scalar string, without losing newlines.

```yorick
f = open("foo.txt", "rb");
raw = array(char, sizeof(f));
_read, f, 0, raw;
close, f;
content = strchar(raw);
```



## zkl



```zkl
data := File("foo.txt","r").read()
```

The file parameters are the same as C's
