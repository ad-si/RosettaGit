+++
title = "Create a file on magnetic tape"
description = ""
date = 2019-10-21T03:03:42Z
aliases = []
[extra]
id = 12431
[taxonomies]
categories = []
tags = []
+++

{{task|File System Operations}} [[Category:Hardware]]
{{omit from|Axe}}
{{omit from|AWK|not OO}}
{{omit from|BASIC|not OO}}
{{omit from|C sharp}}
{{omit from|Fortran|not OO}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic}}
{{omit from|M4}}
{{omit from|Mathematica|Cannot create basic classes}}
{{omit from|Maxima}}
{{omit from|Metafont}}
{{omit from|Octave}}
{{omit from|PARI/GP}}
{{omit from|Retro}}
{{omit from|TI-89 BASIC}}
{{omit from|Visual Basic .NET}}
{{omit from|zkl}}

The task is to create a new file called "TAPE.FILE" of any size on Magnetic Tape.


## Applesoft BASIC


```ApplesoftBasic>SAVE</lang



## C

The program is self explanatory. :)

```C

#include<stdio.h>

int main()
{
	FILE* fp = fopen("TAPE.FILE","w");
	
	fprintf(fp,"This code should be able to write a file to magnetic tape.\n");
	fprintf(fp,"The Wikipedia page on Magnetic tape data storage shows that magnetic tapes are still in use.\n");
	fprintf(fp,"In fact, the latest format, at the time of writing this code is TS1155 released in 2017.\n");
	fprintf(fp,"And since C is already 44, maybe 45, years old in 2017, I am sure someone somewhere did use a C compiler on magnetic tapes.\n");
	fprintf(fp,"If you happen to have one, please try to compile and execute me on that system.\n");
	fprintf(fp,"My creator tested me on an i5 machine with SSD and RAM that couldn't have even been dreamt of by Denis Ritchie.\n");
	fprintf(fp,"Who knows ? Maybe he did foresee today, after all he created something which is still young after 44-45 years and counting...\n");
	fprintf(fp,"EOF");
	
	fclose(fp);
	
	return 0;
}


```



## C++

{{trans|D}}

```cpp>#include <iostream

#include <fstream>

#if defined(_WIN32) || defined(WIN32)
constexpr auto FILENAME = "tape.file";
#else
constexpr auto FILENAME = "/dev/tape";
#endif

int main() {
    std::filebuf fb;
    fb.open(FILENAME,std::ios::out);
    std::ostream os(&fb);
    os << "Hello World\n";
    fb.close();
    return 0;
}
```



## Clojure


```clojure
(spit "/dev/tape" "Hello, World!")
```



## COBOL


{{works with|OpenCOBOL}}


```COBOL>       >
SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAKE-TAPE-FILE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT TAPE-FILE
        ASSIGN "./TAPE.FILE"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD TAPE-FILE.
01 TAPE-FILE-RECORD PIC X(51).

PROCEDURE DIVISION.
    OPEN OUTPUT SHARING WITH ALL OTHER TAPE-FILE
    WRITE TAPE-FILE-RECORD 
        FROM "COBOL treats tape files and text files identically."
    END-WRITE
    STOP RUN.
```



## D


```D
import std.stdio;

void main() {
    version(Windows) {
        File f = File("TAPE.FILE", "w");
    } else {
        File f = File("/dev/tape", "w");
    }
    f.writeln("Hello World!");
}
```



## Fortran

Fortran performs reads and writes to some device, without caring much about the precise nature of the device: card reader/punch, paper tape reader/punch, keyboard, display console, and of course magnetic tape. Sequential input or output is normal, but random access by record number is also possible, though the interface between the Fortran program and the device may not support such attempts, as with a card reader or a display screen. There are also special statements, such as REWIND and BACKSPACE, and ENDFILE. The file OPEN statement will specify such matters as record length and block size and so all is in place for dealing with magnetic tapes...

Of the simplest sort: "Unlabelled". Such a tape consists of a sequence of data blocks, separated by inter-record-gaps, with two in a row at the end to specify the end-of-data on the tape. Although called inter-record, they are actually inter-block: the tape unit reads or writes blocks of say 5,000 characters, not caring that each block might be regarded as containing 50 records of length 100, or 100 records of length 50, etc. The inter-record gap is a sequence of three quarters of an inch of magnetic tape, containing special magnetisation that can be written or sensed at any speed, because these spaces are where the tape is brought to rest between reads or writes. That is, having written the data of a block at full speed, the tape write head writes an IRG as the tape is brought to a stop, and later, when it resumes motion to reach operating speed for the writing of the next block. With adequate data buffering, tape is moved past the head without pause through the IRG as blocks are read or written. If the tape is being read (and written!) with random access the width of the IRG allows for some slop in positioning. Some caution is in order: a "fast forward" action, followed by a few seconds pause to allow the attainment of maximum speed, then a REWIND resulted in a piece of tape being stretched to double length and half width...

Unlabelled tapes are quite inconvenient in a non-user operated computer installation, as mistakes are very easy to make. The wrong tape gets overwritten and the ensuing conversation sours. Computer-readable labels were introduced to supplement human-readable written labels affixed to the tape reel. Co-ordination and standardisation were weak in the 1970s, so there was a lot of variation and partial interoperability between manufacturers and ANSI standards. The first step was of course non-standard labels: some data would be written to the start of the tape that constituted a label, followed by a tape mark, then the actual data file, and so on. Users at an installation would attempt to conform to the local style, and there would be discussions, especially when tapes were exchanged with different systems. A large organisation such as IBM would have its method, and the basic idea was that at the start of the tape would be a volume label and additional information to allow for a data file (or multiple data files) that extended over many reels of tape, each tape identified in turn. This would start with an eighty-character record, beginning "VOL1" - but in EBCDIC, and only later in ASCII. None of this would be visible to the user's programme because it would be handled by the operating system with statements in Job Control Language for the run to identify the file on tape that is to be accessed when the user's programme executes an OPEN statement of a suitable form. And this access would revert to sequential reading or writing with quite ordinary READ and WRITE statements. 

This was developed when tape drives used half-inch tapes in lengths up to 2,400 feet and seven or nine-track recording, storing up to about 150MB per reel - much depended on the length lost to IRG usage between blocks, but block sizes beyond 20,000 were considered large. Subsequent tape designs have included 18, 36, and 128 tracks, helical recording and storage capacities in tens of gigabytes. Naturally, the tape labelling protocols have become even more complex, but, these matters are handled by more complex operating systems.



## Go

Taking a cue from the [[Talk:Create_a_file_on_magnetic_tape|discussion page]], this creates 
an optionally compressed [https://en.wikipedia.org/wiki/Tar_%28computing%29 tar (tape archive)] file
on stdout
(or written to a file or a device such as /dev/tape).
The tar archive will contain a single file, called <tt>TAPE.FILE</tt> by default,
with the contents of the command line <tt>-data</tt> option.

```go
package main

import (
        "archive/tar"
        "compress/gzip"
        "flag"
        "io"
        "log"
        "os"
        "time"
)

func main() {
        filename := flag.String("file", "TAPE.FILE", "filename within TAR")
        data := flag.String("data", "", "data for file")
        outfile := flag.String("out", "", "output file or device (e.g. /dev/tape)")
        gzipFlag := flag.Bool("gzip", false, "use gzip compression")
        flag.Parse()

        var w io.Writer = os.Stdout
        if *outfile != "" {
                f, err := os.Create(*outfile)
                if err != nil {
                        log.Fatalf("opening/creating %q: %v", *outfile, err)
                }
                defer f.Close()
                w = f
        }

        if *gzipFlag {
                zw := gzip.NewWriter(w)
                defer zw.Close()
                w = zw
        }

        tw := tar.NewWriter(w)
        defer tw.Close()
        w = tw
        tw.WriteHeader(&tar.Header{
                Name:     *filename,
                Mode:     0660,
                Size:     int64(len(*data)),
                ModTime:  time.Now(),
                Typeflag: tar.TypeReg,
                Uname:    "guest",
                Gname:    "guest",
        })

        _, err := w.Write([]byte(*data))
        if err != nil {
                log.Fatal("writing data:", err)
        }
}
```

{{out}}

```txt

% go run tapefile.go -data "Hello World" -gzip | tar -tvzf -
-rw-rw----  0 guest  guest      11 11 Aug 13:42 TAPE.FILE
```



## Haskell



```haskell
module Main (main) where

main :: IO ()
main = writeFile "/dev/tape" "Hello from Rosetta Code!"
```


=={{header|Icon}} and {{header|Unicon}}==

This solution mimics the solution used in many other languages here and works in both Icon and Unicon.

```unicon
procedure main()
    write(open("/dev/tape","w"),"Hi")
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 OPEN #1:"Tape1:README.TXT" ACCESS OUTPUT
110 PRINT #1:"I am a tape file now, or hope to be soon."
120 CLOSE #1
```



## Java


```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;

public class CreateFile {
    public static void main(String[] args) throws IOException {
        String os = System.getProperty("os.name");
        if (os.contains("Windows")) {
            Path path = Paths.get("tape.file");
            Files.write(path, Collections.singletonList("Hello World!"));
        } else {
            Path path = Paths.get("/dev/tape");
            Files.write(path, Collections.singletonList("Hello World!"));
        }
    }
}
```



## JCL


```JCL
// EXEC PGM=IEBGENER 
//* Create a file named "TAPE.FILE" on magnetic tape; "UNIT=TAPE" 
//*    may vary depending on site-specific esoteric name assignment 
//SYSPRINT DD SYSOUT=* 
//SYSIN    DD DUMMY 
//SYSUT2   DD UNIT=TAPE,DSN=TAPE.FILE,DISP=(,CATLG) 
//SYSUT1   DD * 
DATA TO BE WRITTEN TO TAPE 
/* 
```




## Julia


```julia

open("/dev/tape", "w") do f
    write(f, "Hello tape!")
end

```



## Kotlin

{{trans|Scala}}

```scala
// version 1.1.0 (Linux)

import java.io.FileWriter

fun main(args: Array<String>) {
    val lp0 = FileWriter("/dev/tape")
    lp0.write("Hello, world!")
    lp0.close()
}
```



## Lua


```lua
require "lfs"

local out
if lfs.attributes('/dev/tape') then
    out = '/dev/tape'
else
    out = 'tape.file'
end
file = io.open(out, 'w')
file:write('Hello world')
io.close(file)
```



## Nim


```nim
var t = open("/dev/tape", fmWrite)
t.writeln "Hi Tape!"
t.close
```



## Perl 6


```perl6
my $tape = open "/dev/tape", :w or die "Can't open tape: $!";
$tape.say: "I am a tape file now, or hope to be soon.";
$tape.close;
```



## Phix


```Phix
include builtins/write_file.e
constant filepath = iff(platform()=WINDOWS?"tape.file":"/dev/tape"),
write_file(file_path,"Hello world!")
```



## PicoLisp


```PicoLisp
(out "/dev/tape"
   (prin "Hello World!") )
```



## Python


```python>>>
 with open('/dev/tape', 'w') as t: t.write('Hi Tape!\n')
... 
>>> 
```



## Racket


```Racket

#lang racket
(with-output-to-file "/dev/tape" #:exists 'append
  (λ() (displayln "I am a cheap imitation of the Perl code for a boring problem")))

```



## REXX

The actual mounting and attaching would be performed by the appropriate operating system (and/or user) commands.


DSNAME   (below)   would be the REXX variable that is associated with a dataset that is assigned to a magnetic tape

device.   The association would be different, depending on upon the operating system being used. 


VM/CMS would use a   '''CP ATTACH'''   command, coupled with a   '''CMS FILEDEF'''   command which associates a

DSNAME   (dataset name)   that will be written to on the attached (and mounted) magnetic tape device.                       

```rexx
/*REXX pgm  demonstrates  writing records  to an attached magnetic tape.*/
dsName = 'TAPE.FILE'                   /*dsName of "file" being written.*/

          do j=1  for 100              /*write 100 records to mag tape. */
          call lineout  dsName,  'this is record'   j   ||   "."
          end   /*j*/
                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

# Project : Create a file on magnetic tape

fn = "Tape.file"
fp = fopen(fn,"w")
str = "I am a tape file now, or hope to be soon."
fwrite(fp, str)
fclose(fp)

```



## Rust


```Rust
use std::io::Write;
use std::fs::File;

fn main() -> std::io::Result<()> {
    File::open("/dev/tape")?.write_all(b"Hello from Rosetta Code!")
}
```



## Scala

===[[Unix]]===
Assuming device is attached to "tape"

```Scala
object LinePrinter extends App {
  import java.io.{ FileWriter, IOException }
  {
    val lp0 = new FileWriter("/dev/tape")
    lp0.write("Hello, world!")
    lp0.close()
  }
}
```


## Tcl

Tcl does not have built-in special support for tape devices, so it relies on the OS to handle most of the details for it. Assuming a relatively modern Unix:
{{trans|UNIX Shell}}

```tcl
cd /tmp

# Create the file
set f [open hello.jnk w]
puts $f "Hello World!"
close $f

# Archive to tape
set fin [open "|tar cf - hello.jnk" rb]
set fout [open /dev/tape wb]
fcopy $fin $fout
close $fin
close $fout
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
STATUS = CREATE ("tape.file",tape-o,-std-)
PRINT STATUS
```

{{Out}}

```txt

OK

```



## UNIX Shell


```bash
#!/bin/sh
cd    # Make our home directory current
echo "Hello World!" > hello.jnk  # Create a junk file
# tape rewind                    # Uncomment this to rewind the tape
tar c hello.jnk                  # Traditional archivers use magnetic tape by default
# tar c hello.jnk > /dev/tape    # With newer archivers redirection is needed
```



## ZX Spectrum Basic

The ZX Spectrum does not have file type extensions, so we save as TAPEFILE, 
rather than TAPE.FILE. 
We can use any start address, depending on where we want the data to come from. 
Here we dump the contents of the screen:

```zxbasic
SAVE "TAPEFILE" CODE 16384,6912
```

