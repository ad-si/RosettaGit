+++
title = "Write entire file"
description = ""
date = 2019-10-12T12:23:14Z
aliases = []
[extra]
id = 19684
[taxonomies]
categories = []
tags = []
+++

{{task|File handling}}
{{omit from|Batch File|No way of reading entire files or putting line breaks into variables.}}
{{omit from|Brainfuck}}
{{omit from|TI-83 BASIC|No filesystem.}}
{{omit from|TI-89 BASIC|No filesystem.}}
{{omit from|Unlambda|Does not know files.}}

;Task:
(Over)write a file so that it contains a string.


The reverse of [[Read entire file]]—for when you want to update or create a file which you would read in its entirety all at once.





## Ada



```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Write_Whole_File is
   File_Name : constant String := "the_file.txt";

   F : File_Type;
begin
   begin
      Open (F, Mode => Out_File, Name => File_Name);
   exception
      when Name_Error => Create (F, Mode => Out_File, Name => File_Name);
   end;

   Put (F, "(Over)write a file so that it contains a string. "               &
           "The reverse of Read entire file—for when you want to update or " &
           "create a file which you would read in its entirety all at once.");
   Close (F);
end Write_Whole_File;

```



## ALGOL 68


```algol68
IF  FILE   output;
    STRING output file name = "output.txt";
    open( output, output file name, stand out channel ) = 0
THEN
    # file opened OK #
    put( output, ( "line 1", newline, "line 2", newline ) );
    close( output )
ELSE
    # unable to open the output file #
    print( ( "Cannot open ", output file name, newline ) )
FI
```



## Arturo



```arturo
contents "Hello World!"
file:write "output.txt" contents
```



## AutoHotkey


```AutoHotkey
file := FileOpen("test.txt", "w")
file.Write("this is a test string")
file.Close()
```


## AWK


```AWK

# syntax: GAWK -f WRITE_ENTIRE_FILE.AWK
BEGIN {
    dev = "FILENAME.TXT"
    print("(Over)write a file so that it contains a string.") >dev
    close(dev)
    exit(0)
}

```



## BaCon

String:

```freebasic
SAVE s$ TO filename$
```


Binary:

```freebasic
BSAVE mem TO filename$ SIZE n
```



## BBC BASIC


```bbcbasic
file% = OPENOUT filename$
PRINT#file%, text$
CLOSE#file%
```



## Bracmat


```bracmat
put$("(Over)write a file so that it contains a string.",file,NEW)
```



## C



###  Dirty solution


```C
/*
 * Write Entire File -- RossetaCode -- dirty hackish solution
 */
#define _CRT_SECURE_NO_WARNINGS  // turn off MS Visual Studio restrictions
#include <stdio.h>

int main(void)
{
    return 0 >= fputs("ANY STRING TO WRITE TO A FILE AT ONCE.",
        freopen("sample.txt","wb",stdout));
}

```

=== Standard function fwrite() ===

```C
/*
 * Write Entire File -- RossetaCode -- ASCII version with BUFFERED files
 */

#define _CRT_SECURE_NO_WARNINGS

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/**
 *  Write entire file at once.
 *
 *  @param fileName file name
 *  @param data     buffer with data
 *  @param size     number of bytes to write
 *
 *  @return Number of bytes have been written.
 */
int writeEntireFile(char* fileName, const void* data, size_t size)
{
    size_t numberBytesWritten = 0; // will be updated

    // Notice: assertions can be turned off by #define NDEBUG
    //
    assert( fileName != NULL );
    assert( data != NULL );
    assert( size >  0 );

    // Check for a null pointer or an empty file name.
    //
    // BTW, should we write  if ( ptr != NULL)  or simply  if ( ptr )  ?
    // Both of these forms are correct. At issue is which is more readable.
    //
    if ( fileName != NULL && *fileName != '\0' )
    {
        // Try to open file in BINARY MODE
        //
        FILE* file = fopen(fileName,"wb");

        // There is a possibility to allocate a big buffer to speed up i/o ops:
        //
        // const size_t BIG_BUFFER_SIZE = 0x20000; // 128KiB
        // void* bigBuffer = malloc(BIG_BUFFER_SIZE);
        // if ( bigBuffer != NULL )
        // {
        //     setvbuf(file,bigBuffer,_IOFBF,BIG_BUFFER_SIZE);
        // }
        //
        // Of course, you should release the malloc allocated buffer somewhere.
        // Otherwise, bigBuffer will be released after the end of the program.


        if ( file != NULL )
        {
            // Return value from fwrite( data, 1, size, file ) is the number
            // of bytes written. Return value from fwrite( data, size, 1, file )
            // is the number of blocks (either 0 or 1) written.
            //
            // Notice, that write (see io.h) is less capable than fwrite.
            //

            if ( data != NULL )
            {
                numberBytesWritten = fwrite( data, 1, size, file );
            }
            fclose( file );
        }
    }
    return numberBytesWritten;
}

#define DATA_LENGTH 8192 /* 8KiB */

int main(void)
{
    // Large arrays can exhaust memory on the stack. This is why the static
    // keyword is used.Static variables are allocated outside the stack.
    //
    static char data[DATA_LENGTH];

    // Filling data array with 'A' character.
    // Of course, you can use any other data here.
    //
    int i;
    for ( i = 0; i < DATA_LENGTH; i++ )
    {
        data[i] = 'A';
    }

    // Write entire file at once.
    //
    if ( writeEntireFile("sample.txt", data, DATA_LENGTH ) == DATA_LENGTH )
        return EXIT_SUCCESS;
    else
        return EXIT_FAILURE;
}

```

=== POSIX function write() ===

```C
/*
 * Write Entire File -- RossetaCode -- plain POSIX write() from io.h
 */

#define _CRT_SECURE_NO_WARNINGS  // turn off MS Visual Studio restrictions
#define _CRT_NONSTDC_NO_WARNINGS // turn off MS Visual Studio restrictions

#include <assert.h>
#include <fcntl.h>
#include <io.h>

/**
 *  Write entire file at once.
 *
 *  @param fileName file name
 *  @param data     buffer with data
 *  @param size     number of bytes to write
 *
 *  @return Number of bytes have been written.
 */
int writeEntireFile(char* fileName, const void* data, size_t size)
{
    size_t numberBytesWritten = 0; // will be updated
    int file; // file handle is an integer (see Fortran ;)

    // Notice: we can not trust in assertions to work.
    // Assertions can be turned off by #define NDEBUG.
    //
    assert( fileName );
    assert( data );
    assert( size >  0 );

    if(fileName && fileName[0] && (file=open(fileName,O_CREAT|O_BINARY|O_WRONLY))!=(-1))
    {
        if ( data )
            numberBytesWritten = write( file, data, size );
        close( file );
    }
    return numberBytesWritten;
}

#define DATA_LENGTH 8192 /* 8KiB */

int main(void)
{
    // Large arrays can exhaust memory on the stack. This is why the static
    // keyword is used.Static variables are allocated outside the stack.
    //
    static char data[DATA_LENGTH];

    // Filling data array with 'Z' character.
    // Of course, you can use any other data here.
    //
    int i;
    for ( i = 0; i < DATA_LENGTH; i++ )
    {
        data[i] = 'Z';
    }

    // Write entire file at once.
    //
    if ( writeEntireFile("sample.txt", data, DATA_LENGTH ) == DATA_LENGTH )
        return 0;
    else
        return 1;
}

```



## C++


```cpp
#include <fstream>
using namespace std;

int main()
{
    ofstream file("new.txt");
    file << "this is a string";
    file.close();
    return 0;
}
```


## C#


```c#
System.IO.File.WriteAllText("filename.txt", "This file contains a string.");
```



## Clojure


```clojure
(spit "file.txt" "this is a string")
```



## Common Lisp


```lisp
(with-open-file (str "filename.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str "File content...~%"))
```



## D


```D
import std.stdio;

void main() {
    auto file = File("new.txt", "wb");
    file.writeln("Hello World!");
}
```



## Elena

ELENA 4.x :

```elena
import system'io;

public program()
{
    File.assign("filename.txt").saveContent("This file contains a string.")
}
```



## Elixir


```elixir
File.write("file.txt", string)
```


=={{header|F_Sharp|F#}}==

```fsharp
System.IO.File.WriteAllText("filename.txt", "This file contains a string.")
```



## Factor


```factor
USING: io.encodings.utf8 io.files ;
"this is a string" "file.txt" utf8 set-file-contents
```



## Fortran

Where F is an integer with a value such as 10 (these days not being 5, standard input, nor 6, standard output), the I/O unit number. "REPLACE" means create the file if it does not exist, otherwise delete the existing file and create a new one of that name. The WRITE statement may present a single (large?) item, a list of items, and possibly there would be a series of WRITE statements if there is no need to do the deed via one WRITE only.

```Fortran
      OPEN (F,FILE="SomeFileName.txt",STATUS="REPLACE")
      WRITE (F,*) "Whatever you like."
      WRITE (F) BIGARRAY
```

Earlier Fortran might have other predefined unit numbers, for a card punch, paper tape, lineprinter, and so on, according to the installation. For a disc file, there may be no facility to name the file within Fortran itself as in the example OPEN statement because there may be no OPEN statement. Fortran IV used a DEFINE FILE statement, for example, which specified the record length and number of records in the file. As constants. File naming would instead be done by commands to the operating system when the prog. is started, as with the DD statement of JCL (Job Control Language) of IBM mainframes, and this would have its own jargon for NEW, OLD, REPLACE and what to do when the step finishes: DISP=KEEP, perhaps.

The OPEN statement started becoming standard with F77, but a READ or WRITE statement for a file could mix FORMAT style I/O with unformatted I/O - or indeed do both at once. This could be achieved via the A''n'' format code, which essentially states "unformatted" because it takes the variable's bit pattern "as is". Thus, a typical 32-bit floating-point variable occupies four bytes so it could be read or written with format code <code>A4</code> and so on for other sizes. With declarations like <code>REAL*8 X</code>, Fortran programmers are usually well aware of storage sizes.

With F90, constraints were tightened - for our own good, supposedly. A file is opened with FORM="FORMATTED" by default, meaning that text is expected to be involved and all READ or WRITE statements must use the FORMAT facilities for that file. In the example, the * specifies free-format, which is not format free. Contrariwise, if the file is opened with FORM="UNFORMATTED" then the FORMAT facility must ''not'' be specified, as in the second WRITE of the example.

There is no particular limit on how much can be written in one go since no fixed record length has been specified, and indeed the text rolled forth via a variable may contain whatever character codes are desired, including CR, LF - but you will have to know whether the record separator is CR, CRLF, LFCR or CR, if you expect the file to be read later as a series of separate records. The output from each WRITE will be followed by the record separator (whichever it is) in the character interpretation in force, usually ASCII these days.

If instead FORM="UNFORMATTED" were specified in the OPEN statement you can still write whatever you wish, but now each output will not be followed by a record terminator. It is usual for such files to have a defined record length and reading or writing more than a record can hold is deemed an error. Previously (as with Fortran IV) the output (or input) would continue into however many successive records were needed. This was convenient when writing a large array of integers or floating-point, or indeed any list of such items. This would be useful when a complex data structure had been devised, and a simple statement with a short ''list'' would write the lot to a disc file, or read it back later. This would be far less trouble than working through its many details. One could go further and use EQUIVALENCE to overlay the pieces upon a large single array (called say THELOT) so that one need merely use <code>WRITE(F) THELOT</code> and likewise for reading. However keeping the equivalences correct when changes are made is troublesome. Instead, the data structure's collection of items could be placed in a COMMON work area and a STASH subroutine would for its COMMON statement merely declare an array THELOT of a suitable size without any mention of EQUIVALENCE. A somewhat similar arrangement is provided in pl/i via the BASED facility.

Option FORM="BINARY" ''does'' allow the reading or writing of however many records are needed to satisfy the I/O list, but, this is not a standard usage.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open "output.txt" For Output As #1
Print #1, "This is a string"
Close #1
```



## Free Pascal

''See also: [[#Pascal]]''

```Pascal
program overwriteFile(input, output, stdErr);
{$mode objFPC} // for exception treatment
uses
	sysUtils; // for applicationName, getTempDir, getTempFileName
	// also: importing sysUtils converts all run-time errors to exceptions
resourcestring
	hooray = 'Hello world!';
var
	FD: text;
begin
	// on a Debian GNU/Linux distribution,
	// this will write to /tmp/overwriteFile00000.tmp (or alike)
	assign(FD, getTempFileName(getTempDir(false), applicationName()));
	try
		rewrite(FD); // could fail, if user has no permission to write
		writeLn(FD, hooray);
	finally
		close(FD);
	end;
end.
```



## Gambas


```gambas
Public Sub Main()

File.Save(User.home &/ "test.txt", "(Over)write a file so that it contains a string.")

End
```



## Go


```Go
import "io/ioutil"

func main() {
    ioutil.WriteFile("path/to/your.file", []byte("data"), 0644)
}
```



## Groovy


```Groovy
 new File("myFile.txt").text = """a big string
that can be
splitted over lines
"""

```



## Haskell


```Haskell
main :: IO ( )
main = do
   putStrLn "Enter a string!"
   str <- getLine
   putStrLn "Where do you want to store this string ?"
   myFile <- getLine
   appendFile myFile str
```



## J


```J>   characters fwrite filename</lang


or,


```J
  characters 1!:2<filename
```



## Java


```java
import java.io.*;

public class Test {

    public static void main(String[] args) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter("test.txt"))) {
            bw.write("abc");
        }
    }
}
```



## Julia


```julia
function writeFile(filename, data)
	f = open(filename, "w")
	write(f, data)
	close(f)
end

writeFile("test.txt", "Hi there.")
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    val text = "empty vessels make most noise"
    File("output.txt").writeText(text)
}
```



## Lingo


```lingo
----------------------------------------
-- Saves string as file
-- @param {string} tFile
-- @param {string} tString
-- @return {bool} success
----------------------------------------
on writeFile (tFile, tString)
  fp = xtra("fileIO").new()
  fp.openFile(tFile, 2)
  err = fp.status()
  if not (err) then fp.delete()
  else if (err and not (err = -37)) then return false
  fp.createFile(tFile)
  if fp.status() then return false
  fp.openFile(tFile, 2)
  if fp.status() then return false
  fp.writeString(tString)
  fp.closeFile()
  return true
end
```



## LiveCode

This will create a file "TestFile.txt" or overwrite it if it already exists. This is the shortest method for file I/O in LiveCode, but not the only method.

```LiveCode

put "this is a string" into URL "file:~/Desktop/TestFile.txt"

```



## Lua


```Lua
function writeFile (filename, data)
	local f = io.open(filename, 'w')
	f:write(data)
	f:close()
end

writeFile("stringFile.txt", "Mmm... stringy.")
```



## Maple


```Maple
Export("directory/filename.txt", string);
```




## Mathematica


```Mathematica
Export["filename.txt","contents string"]
```



## Nim


```nim

writeFile("filename.txt", "An arbitrary string")

```



## Objeck


```objeck
use System.IO.File;

class WriteFile {
  function : Main(args : String[]) ~ Nil {
    writer ← FileWriter→New("test.txt");
    leaving {
      writer→Close();
    };
    writer→WriteString("this is a test string");
  }
}
```




## OCaml



```ocaml
let write_file filename s =
  let oc = open_out filename in
  output_string oc s;
  close_out oc;
;;

let () =
  let filename = "test.txt" in
  let s = String.init 26 (fun i -> char_of_int (i + int_of_char 'A')) in
  write_file filename s
```



## Pascal

''See also: [[#Free Pascal|Free Pascal]]''

In Standard Pascal, a list of identifiers in the program header sets up a list of file handlers.

```Pascal
{$ifDef FPC}{$mode ISO}{$endIf}
program overwriteFile(FD);
begin
	writeLn(FD, 'Whasup?');
	close(FD);
end.
```

In some environments, it is the caller’s sole responsibility to invoke the program properly.
I. e., the program above could be invoked on a Bourne-again shell in this fashion:

```bash>./overwriteFile
&- <&- 0>/tmp/foo # open file descriptor with index 0 for writing
```



## Perl


The modern recommended way, is to use one of these CPAN modules:


```perl
use File::Slurper 'write_text';
write_text($filename, $data);
```



```perl
use Path::Tiny;
path($filename)->spew_utf8($data);
```


Traditional way, in case using CPAN modules is not an option:


```perl
open my $fh, '>:encoding(UTF-8)', $filename or die "Could not open '$filename':  $!";
print $fh $data;
close $fh;
```



## Perl 6



```perl6
spurt 'path/to/file', $file-data;
```

or

```perl6
'path/to/file'.IO.spurt: $file-data;
```



## Phix

Deep in the heart of the compiler itself, after much huffing and puffing, the following code can be found:

```Phix
fn = open(outfile,"wb")
...
string img = repeat(' ',SizeOfImage)
...
SetCheckSum(img,SizeOfImage)
puts(fn,img)
close(fn)
```

Obviously as that can successfully write a binary executable, simple strings are no problem, except that when dealing with text you would normally want automatic line ending conversion enabled, so drop the 'b' (binary) mode option, ie the "wb" above should be just "w".

The distribution also includes the file builtins\writefile.e which declares

```Phix
global function write_file(object file, sequence data, integer as_text = BINARY_MODE, integer encoding = ANSI, integer with_bom = 1)
```

which is intended to be a one-line call with full unicode support, however as yet it is neither properly documented nor adequately tested.


## PHP


```php
file_put_contents($filename, $data)
```



## PicoLisp


```PicoLisp
(out "file.txt"
   (prinl "My string") )
```



## PowerShell

Writes all running process details to a file in the current directory.

```powershell

Get-Process | Out-File -FilePath .\ProcessList.txt

```


## PureBasic


```PureBasic

EnableExplicit

Define fOutput$ = "output.txt" ; enter path of file to create or overwrite
Define str$ = "This string is to be written to the file"

If OpenConsole()
  If CreateFile(0, fOutput$)
    WriteString(0, str$)
    CloseFile(0)
  Else
    PrintN("Error creating or opening output file")
  EndIf
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out|Output (contents of 'output.exe')}}

```txt

This string is to be written to the file

```



## Python


```python

with open(filename, 'w') as f:
    f.write(data)

```



## Racket

This only replaces the file if it exists, otherwise it writes a new file.  Use <tt>'truncate</tt> to overwrite the new contents into the existing file.

```racket
#lang racket/base
(with-output-to-file "/tmp/out-file.txt" #:exists 'replace
  (lambda () (display "characters")))
```



## REXX


### version 1


```rexx
of='file.txt'
'erase' of
s=copies('1234567890',10000)
Call charout of,s
Call lineout of
Say chars(of) length(s)
```

{{out}}

```txt
100000 100000
```

{{out}} (first time use) using Regina REXX (among others) on a Windows or DOS system:

```txt

Could Not Find c:\file.txt
100000 100000

```



### version 2

This REXX version doesn't depend on any (operating system) host commands.

```rexx
/*REXX program  writes  an  entire file  with a  single write  (a long text record).    */
oFID= 'OUTPUT.DAT'                               /*name of the output file to be used.  */
                                                 /* [↓]  50 bytes, including the fences.*/
$ = '<<<This is the text that is written to a file. >>>'
                                                 /* [↓]  COPIES  creates a 50k byte str.*/
call charout oFID, copies($,1000), 1             /*write the longish text to the file.  */
                                                 /* [↑]  the "1"  writes text ──► rec #1*/
                                                 /*stick a fork in it,  we're all done. */
```






## Ring


```ring

write("myfile.txt","Hello, World!")

```



## Ruby

The file is closed at the end of the block.

```ruby
open(fname, 'w'){|f| f.write(str) }
```



## Rust


```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let data = "Sample text.";
    let mut file = File::create("filename.txt")?;
    write!(file, "{}", data)?;
    Ok(())
}
```



## Scala


```Scala
import java.io.{File, PrintWriter}

object Main extends App {
  val pw = new PrintWriter(new File("Flumberboozle.txt"),"UTF8"){
  print("My zirconconductor short-circuited and I'm having trouble fixing this issue.\nI researched" +
    " online and they said that I need to connect my flumberboozle to the XKG virtual port, but I was" +
    " wondering if I also needed a galvanized tungsten retrothruster array? Maybe it'd help the" +
    " frictional radial anti-stabilizer vectronize from the flumberboozle to the XKG virtual port?")
  close()}
}
```



## Sidef

With error handling:

```ruby
var file = File(__FILE__)
file.open_w(\var fh, \var err) || die "Can't open #{file}: #{err}"
fh.print("Hello world!")       || die "Can't write to #{file}: #{$!}"
```

Without error handling:

```ruby
File(__FILE__).open_w.print("Hello world!")
```



## SPL


```spl
#.writetext("file.txt","This is the string")
```



## Tcl



```Tcl
proc writefile {filename data} {
    set fd [open $filename w]   ;# truncate if exists, else create
    try {
        puts -nonewline $fd $data
    } finally {
        close $fd
    }
}
```


A more elaborate version of this procedure might take optional arguments to pass to <tt>fconfigure</tt> (such as encoding) or <tt>open</tt> (such as permissions).


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
content="new text that will overwrite content of myfile"
LOOP
 path2file=FULLNAME (TUSTEP,"myfile",-std-)
 status=WRITE (path2file,content)
 IF (status=="OK") EXIT
 IF (status=="CREATE") ERROR/STOP CREATE ("myfile",seq-o,-std-)
ENDLOOP

```


## VBA



```vb

Option Explicit

Const strName As String = "MyFileText.txt"
Const Text As String = "(Over)write a file so that it contains a string. " & vbCrLf & _
           "The reverse of Read entire file—for when you want to update or " & vbCrLf & _
           "create a file which you would read in its entirety all at once."

Sub Main()
Dim Nb As Integer

Nb = FreeFile
Open "C:\Users\" & Environ("username") & "\Desktop\" & strName For Output As #Nb
    Print #1, Text
Close #Nb
End Sub
```



## VBScript

Text file created or overwritten in the same folder as the script.

```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")

SourceFile = objFSO.GetParentFolderName(WScript.ScriptFullName) & "\out.txt"
Content = "(Over)write a file so that it contains a string." & vbCrLf &_
		"The reverse of Read entire file—for when you want to update or create a file which you would read in its entirety all at once."

With objFSO.OpenTextFile(SourceFile,2,True,0)
	.Write Content
	.Close
End With

Set objFSO = Nothing

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Sub Main()
        My.Computer.FileSystem.WriteAllText("new.txt", "This is a string", False)
    End Sub

End Module

```



## XLISP


```scheme
(define n (open-output-file "example.txt"))
(write "(Over)write a file so that it contains a string." n)
(close-output-port n)
```



## Xtend


```java

package com.rosetta.example

import java.io.File
import java.io.PrintStream

class WriteFile {
    def static main( String ... args ) {
        val fout = new PrintStream(new File(args.get(0)))
        fout.println("Some text.")
        fout.close
    }
}

```



## Yabasic


```Yabasic
open "output.txt" for writing as #1
print #1 "This is a string"
close #1
```



## zkl


```zkl
      // write returns bytes written, GC will close the file (eventually)
File("foo","wb").write("this is a test",1,2,3); //-->17

f:=File("bar",wb");
data.pump(f,g); // use g to process data as it is written to file
f.close();  // don't wait for GC
```

