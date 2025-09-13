+++
title = "Fixed length records"
description = ""
date = 2019-10-04T00:00:44Z
aliases = []
[extra]
id = 22028
[taxonomies]
categories = ["task"]
tags = []
+++

{{task}}Fixed length read/write

Before terminals, computers commonly used [https://en.wikipedia.org/wiki/Punched_card_reader punch card readers] or paper tape input.

A common format before these devices were superseded by terminal technology was based on the Hollerith code, [https://en.wikipedia.org/wiki/Hollerith_card Hollerith code].

These input devices handled 80 columns per card and had a limited character set, encoded by punching holes in one or more rows of the card for each column.

These devices assumed/demanded a fixed line width of 80 characters, newlines were not required (and could not even be encoded in some systems).

## Task

Write a program to read 80 column fixed length records (no newline terminators (but newline characters allowed in the data)) and then write out the reverse of each line as fixed length 80 column records. 

Samples here use printable characters, but that is not a given with fixed length data.  Filenames used are sample.txt, infile.dat, outfile.dat.

'''Note:''' There are no newlines, inputs and outputs are fixed at 80 columns, no more, no less, space padded.  Fixed length data is 8 bit complete.  NUL bytes of zero are allowed.

These fixed length formats are still in wide use on mainframes, with JCL and with COBOL (which commonly use [https://en.wikipedia.org/wiki/EBCDIC EBCDIC] encoding and not [https://en.wikipedia.org/wiki/ASCII ASCII]).  Most of the large players in day to day financial transactions know all about fixed length records and the expression ''logical record length''.

;Sample data:
To create the sample input file, use an editor that supports fixed length records or use a conversion utility.  For instance, most GNU/Linux versions of '''dd''' support blocking and unblocking records with a conversion byte size.


```txt
Line 1...1.........2.........3.........4.........5.........6.........7.........8
Line 2
Line 3
Line 4

Line 6
Line 7
     Indented line 8............................................................
Line 9                                                                 RT MARGIN
```




```txt
prompt$ dd if=sample.txt of=infile.dat cbs=80 conv=block
```
 will create a fixed length record file of 80 bytes given newline delimited text input.


```txt
prompt$ dd if=infile.dat cbs=80 conv=unblock
```
 will display a file with 80 byte logical record lengths to standard out as standard text with newlines.

<br />
;Bonus round:
Forth systems often include BLOCK words.  A block is 1024 bytes.  Source code is stored as 16 lines of 64 characters each (again, no newline character or sequence to mark the end of a line).

Write a program to convert a block file to text (using newlines). Trailing spaces should be excluded from the output.

Also demonstrate how to convert from a normal text file to block form. All lines either truncated or padded to 64 characters with no newline terminators. The last block filled to be exactly 1024 characters by adding blanks if needed. Assume a full range of 8 bit byte values for each character.

The COBOL example uses forth.txt and forth.blk filenames.
<br /><br />


## COBOL



```cobol
      *> Rosetta Code, fixed length records
      *> Tectonics:
      *>   cobc -xj lrecl80.cob
       identification division.
       program-id. lrecl80.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select infile
               assign to infile-name
               organization is sequential
               file status is infile-status
           .
           select outfile
               assign to outfile-name
               organization is sequential
               file status is outfile-status
           .

       data division.
       file section.
       fd infile.
           01 input-text pic x(80).

       fd outfile.
           01 output-text pic x(80).

       working-storage section.
       01 infile-name.
          05 value "infile.dat".
       01 infile-status pic xx.
          88 ok-input value '00'.
          88 eof-input value '10'.

       01 outfile-name.
          05 value "outfile.dat".
       01 outfile-status pic xx.
          88 ok-output value '00'.

       procedure division.

       open input infile
       if not ok-input then
           display "error opening input " infile-name upon syserr
           goback
       end-if

       open output outfile
       if not ok-output
           display "error opening output " outfile-name upon syserr
           goback
       end-if

      *> read lrecl 80 and write the reverse as lrecl 80
       read infile
       perform until not ok-input
           move function reverse(input-text) to output-text

           write output-text
           if not ok-output then
               display "error writing: " output-text upon syserr
           end-if
           read infile
       end-perform

       close infile outfile

      *> from fixed length to normal text, outfile is now the input file
       open input outfile
       if not ok-output then
           display "error opening input " outfile-name upon syserr
           goback
       end-if

       read outfile
       perform until not ok-output
           display function trim(output-text trailing)
           read outfile
       end-perform

       close outfile

       goback.
       end program lrecl80.
```


Given a starting file sample.txt of


```txt
Line 1...1.........2.........3.........4.........5.........6.........7.........8
Line 2
Line 3
Line 4

Line 6
Line 7
     Indented line 8............................................................
Line 9                                                                 RT MARGIN
```


And a setup pass of


```txt
prompt$ dd if=sample.txt of=infile.dat cbs=80 conv=block
```


```txt
prompt$ cobc -xj fixed-length.cob
8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL

                                                                          6 eniL
                                                                          7 eniL
..............................................................8 enil fo tuO
NIGRAM TR                                                                 9 eniL
```



```txt
prompt$ file outfile.dat
outfile.dat: ASCII text, with very long lines, with no line terminators
```



###  blocks 


Demonstrate text to Forth source block form.


```cobol
      *> Rosetta Code fixed length records, text to Forth block
       identification division.
       program-id. blocking.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select infile
               assign to infile-name
               organization is line sequential
               file status is infile-status
           .
           select outfile
               assign to outfile-name
               organization is sequential
               file status is outfile-status
           .

       data division.
       file section.
       fd infile.
           01 input-text pic x(64).

       fd outfile.
           01 output-text pic x(64).

       working-storage section.
       01 infile-name.
          05 value "forth.txt".
       01 infile-status pic xx.
          88 ok-input value '00'.
          88 eof-input value '10'.

       01 outfile-name.
          05 value "forth.blk".
       01 outfile-status pic xx.
          88 ok-output value '00'.

       procedure division.

      *> read a line, padded to or truncated at 64 as defined in FD
       open input infile
       if not ok-input then
           display "error opening input " infile-name upon syserr
           goback
       end-if

       open output outfile
       if not ok-output
           display "error opening output " outfile-name upon syserr
           goback
       end-if

       move 0 to tally
       read infile
       perform until not ok-input
           move input-text to output-text

           write output-text
           if not ok-output then
               display "error writing: " output-text upon syserr
           end-if

           add 1 to tally
           if tally > 15 then move 0 to tally end-if

           read infile
       end-perform

      *> Output up to next 1024 byte boundary
       if tally > 0 then
           compute tally = 16 - tally
           move spaces to output-text

           perform tally times
               write output-text
               if not ok-output then
                   display "error writing: " output-text upon syserr
               end-if
           end-perform
       end-if

       close infile outfile

       goback.
       end program blocking.
```


Demonstrate Forth source block to text form.


```cobol
      *> Rosetta Code fixed length records, Forth blocks to text.
       identification division.
       program-id. unblocking.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select infile
               assign to infile-name
               organization is sequential
               file status is infile-status
           .
           select outfile
               assign to outfile-name
               organization is line sequential
               file status is outfile-status
           .

       data division.
       file section.
       fd infile.
           01 input-text pic x(64).

       fd outfile.
           01 output-text pic x(64).

       working-storage section.
       01 infile-name.
          05 value "forth.blk".
       01 infile-status pic xx.
          88 ok-input value '00'.
          88 eof-input value '10'.

       01 outfile-name.
          05 value "forth.txt".
       01 outfile-status pic xx.
          88 ok-output value '00'.

       procedure division.

       open input infile
       if not ok-input then
           display "error opening input " trim(infile-name) upon syserr
           goback
       end-if

       open output outfile
       if not ok-output
           display "error opening write " trim(outfile-name) upon syserr
           goback
       end-if

      *> read a fixed length line, 64 characters
       read infile
       perform until not ok-input
           move trim(input-text) to output-text

           write output-text
           if not ok-output then
               display "error writing: " output-text upon syserr
           end-if
           read infile
       end-perform

       close infile outfile

       goback.
       end program unblocking.
```



## Delphi

''See [[#Pascal|Pascal]]''


## Free Pascal

''See [[#Pascal|Pascal]]''


## J

'''Solution:'''

Using the 720 byte input file linked to in the Perl6 entry. 

```j
   _80 ]\ fread 'flr-infile.dat'   NB. reads the file into a n by 80 array
   _80 |.\ fread 'flr-infile.dat'  NB. as above but reverses each 80 byte chunk
   'flr-outfile.dat' fwrite~ , _80 |.\ fread 'flr-infile.dat'  NB. as above but writes result to file (720 bytes)
   processFixLenFile=: fwrite~ [: , _80 |.\ fread              NB. represent operation as a verb/function
```


'''Example Usage:'''


```j
   'flr-outfile.dat' processFixLenFile 'flr-infile.dat'  NB. returns number of bytes written
720
```



## jq


To read the raw input and write out raw strings, the command-line options -R (raw input), -s ("slurp"), and -r (raw output) options are necessary:

    jq -Rrs -f program.jq infile.dat

program.jq: 

For the sake of generality, we define `cut` with an argument corresponding to the number of columns.  To illustrate that an efficient recursive definition is possible, we define an inner helper function, `c`, with arity 0, as currently jq's tail-call optimization is restricted to zero-arity filters:


```jq
def cut($n):
  def c: if length==0 then empty else .[:$n] , (.[$n:] | c) end;
  c;

cut(80) | explode | reverse | implode;
```



## Julia

The program reads from an "infile.dat" file created with dd, as described in the task instructions.

```julia

function processfixedlengthrecords(infname, blocksize, outfname)
    inf = open(infname)
    outf = open(outfname, "w")
    filedata = [ read(inf, blocksize) for _ in 1:10 ]
       
    for line in filedata
        s = join([Char(c) for c in line], "")
        @assert(length(s) == blocksize)
        write(outf, s)
    end
end
    
processfixedlengthrecords("infile.dat", 80, "outfile.dat")

```



## Go


```go
package main

import (
    "fmt"
    "log"
    "os"
    "os/exec"
)

func reverseBytes(bytes []byte) {
    for i, j := 0, len(bytes)-1; i < j; i, j = i+1, j-1 {
        bytes[i], bytes[j] = bytes[j], bytes[i]
    }
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    in, err := os.Open("infile.dat")
    check(err)
    defer in.Close()

    out, err := os.Create("outfile.dat")
    check(err)

    record := make([]byte, 80)
    empty := make([]byte, 80)
    for {
        n, err := in.Read(record)
        if err != nil {
            if n == 0 {
                break // EOF reached
            } else {
                out.Close()
                log.Fatal(err)
            }
        }
        reverseBytes(record)
        out.Write(record)
        copy(record, empty)
    }
    out.Close()

    // Run dd from within program to write output.dat
    // to standard output as normal text with newlines.
    cmd := exec.Command("dd", "if=outfile.dat", "cbs=80", "conv=unblock")
    bytes, err := cmd.Output()
    check(err)
    fmt.Println(string(bytes))
}
```


```txt

8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL

                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI
NIGRAM TR                                                                 9 eniL

```


'''Bonus round'''


```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strings"
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func block2text(inputFile, outputFile string) {
    in, err := os.Open(inputFile)
    check(err)
    defer in.Close()

    out, err := os.Create(outputFile)
    check(err)
    defer out.Close()

    line := make([]byte, 64)
    empty := make([]byte, 64)
    for {
        n, err := in.Read(line)
        if err != nil {
            if n == 0 {
                break // EOF reached
            } else {
                log.Fatal(err)
            }
        }
        str := string(line)
        str = strings.TrimRight(str, " \000")
        out.WriteString(str + "\n")
        copy(line, empty)
    }
}

func text2block(inputFile, outputFile string) {
    in, err := os.Open(inputFile)
    check(err)
    defer in.Close()

    out, err := os.Create(outputFile)
    check(err)
    defer out.Close()

    scanner := bufio.NewScanner(in)
    count := 0
    for scanner.Scan() {
        str := scanner.Text()
        count++
        le := len(str)
        if le > 64 {
            str = str[0:64]
        } else if le < 64 {
            str = fmt.Sprintf("%-64s", str)
        }
        out.WriteString(str)
    }
    if rem := count % 16; rem > 0 {
        str := strings.Repeat(" ", (16-rem)*64)
        out.WriteString(str)
    }
}

func main() {
    block2text("block.dat", "block.txt")
    text2block("block.txt", "block2.dat")
}
```



## M2000 Interpreter


### Fixed length Record

Form Buffer object (hold a memory block) we can read a byte at offset. So Print Eval(Line80,3) return the 4th byte, and Return Line80, 2:=255,5:=0 set two bytes (unsigned, we place any number and interpreter convert it to byte).


```M2000 Interpreter

Module FixedFile {
      Read fixed$
      OldLocale=Locale
      \\ chr$(string_argument$)
      \\ use Locale to convert from Ansi to Utf-16LE
      \\ Read Ansi form files also use Locale
      Locale 1032
      Try ok {
            \\ Make the file first
            Const Center=2
            Font "Courier New"
            Bold 0
            Italic 0
            Def long m, z=1, f
            Def text2read$,test3write$
            Form 100, 50  ' 100 by 60 characters
            Document txt$={Line 1...1.........2.........3.........4.........5.........6.........7.........8
                  Line 2
                  Line 3
                  Line 4
                  
                  Line 6
                  Line 7
                       Indented line 8............................................................
                  Line 9                                                                 RT MARGIN
                  }
            \\ use Help Open in M2000 console for details
            \\ Method one
            Report Center,  "Make file"
            \\ for WIDE Random \\  for Utf-16
            Open fixed$ for Random Exclusive as #f len=80
            m=Paragraph(txt$, 0)
            z=1
            If forward(txt$, m) then
                  while m, z<10
                        text2write$=Paragraph$(txt$,(m))
                        Print format$("Len:{0}, Data: {1}",Len(text2write$),text2write$)
                        Put #f, text2write$ , z
                        \\ record number from 1
                        \\ if number is total records plus one
                        \\ we append a record
                        z++     
                  End while
            End If
            Print "Press any key"
            Push Key$ : Drop
            Form 80, 40
            Report Center,  "Method1"
            For z=1 to 9
                  Get #f, text2read$, z
                  text2read$=StrRev$(text2read$)
                  Put #f, text2read$, z
                  Print text2read$
            Next z
            Close #f
            Report Center,  "Method2"
            \\ Method2
            \\ Buffer Clear Line80 ... \\ to clear memory
            \\ here we write all bytes so not needed
            Buffer Line80 as byte*80
            m=filelen(fixed$)
            If m mod 80=0 Then
                  m=1
                  \\ now Get/Put read write at byte position
                  \\ we have to use seek to move to byte position
                  \\ This way used for Binary files
                  Open fixed$ for Input as #f1
                  Open fixed$ for Append as #f2
                  while not eof(#f1)
                        seek #f1, m
                        Rem Print seek(#f)
                        Get #f1, Line80
                        Return line80,0:=Str$(StrRev$(Chr$(Eval$(line80,0,80))))
                        seek #f2, m
                        Put #f2, Line80
                        seek #f1, m
                        Get #f1, Line80
                        Print Chr$(Eval$(line80,0,80))
                        m+=80
                  End While
                  Close #f1
                  Close #f2
            End if
      }
      \\ use Close with no parameters for close all files if something happen
      If error then Close: Print Error$
      Locale OldLocale    
}
FixedFile "fixed.random"


```


### Forth Blocks


```M2000 Interpreter

Form 80,50
Print "Forth's Blocks"
\\ Forth Blocks
Structure Line16 {
      a`Line as byte*64
}
NewBlock=lambda Line16 -> {
      Buffer a`Block as Line16*16
      \\ fill spaces
      Return a`Block, 0:=str$(string$(" ",1024))
      =a`Block
}
\\ Events are value types, but for closures and groups are reference types
Event Doit {
      Read something$
}
Header=Doit
DisplayBlock= Lambda NewBlock, Line16, Doit, Header (Blocks`File$,Block`Number, UseLocale=1033)->{
      Page1=NewBlock()
      Open Blocks`File$ for input as #f
            Seek #f, 1024*(Block`Number-1)+1
            Get #f,Page1
      Close #f
      Document NewDoc$
      \\ need to convert from Ansi
      Call Event Header, "Block:"+Str$(Block`Number)
      oldlocale=locale
      locale UseLocale
      For i=0 to 15
            lineAny$=chr$(Eval$(Page1,i, Len(Line16)))
            Call Event Doit, format$("{0::-2} {1}",i,chr$(Eval$(Page1,i, Len(Line16))))
      Next i
      locale oldlocale
}
Document ForthCode$={( Large letter F) 
      : STAR    [CHAR] * EMIT ;
      : STARS   0 DO  STAR  LOOP ;
      : MARGIN  CR 30 SPACES ;
      : BLIP    MARGIN STAR ;
      : BAR     MARGIN 5 STARS ;
      : F       BAR BLIP BAR BLIP BLIP CR ;
      }
\\ Make Document bigger than 16 lines
\\ doc.par(ForthCode$)  return paragraphs (here we have no wrap)
\\ actuall lines per layer can be found from Report (the renderer)
\\ using Reportlines. layer can be the printer page.
ForthCode$=string$(ForthCode$,5)
Print "Make Block"
Page1=NewBlock()
Locale 1033
Blocks`File$="Forth Blocks"
Block`Number=1
\\ Apppend three times same blocks
For Pass=1 to 3
      If Doc.Len(ForthCode$)>0 then
            For i=1 to Doc.par(ForthCode$)-1
            \\ we give order number but Paragraph$ use unique number for paragraphs
            \\ if we didn't delete or insert lines, then these two are the same
                  Print Paragraph$(ForthCode$, Paragraph(ForthCode$,i))
                  \\ convert to Ansi using Locale
                  \\ offset from 0, so minus 1
                  \\ offset 1 is at len(Line16)
                  \\ Page1(0) is the real address, but here doesn't matter
                  Return Page1, (i-1) mod 16:=Str$(Paragraph$(ForthCode$, Paragraph(ForthCode$,i)))
                  if i mod 16=0 then Gosub SaveBlock
            Next i
            i--
            if Not i mod 16=0 then  Gosub SaveBlock
      End if
Next Pass
\\ now we read from disk
Class DocumentKeeper {
      Document Text$
      Function AppendLine(aline$) {
            \\ right trim$
            .Text$<=Mid$(Trim$("*"+aline$),2)+{
            }
      }
}
Function Disp(aline$) {
      Print aline$
}
DocumentKeeper=DocumentKeeper()
Event Doit New Disp()
Event Header New Disp()
For i=1 to Block`Number-1
      Call DisplayBlock(Blocks`File$, i)
      Print "Press any key"
      Push key$ : Drop
Next i
Event Doit Drop Disp()
Event Doit New DocumentKeeper.AppendLine()
Event Header Hold
For i=1 to Block`Number-1
      Call DisplayBlock(Blocks`File$, i)
Next i
Report DocumentKeeper.Text$

End 
SaveBlock:
      Print "Save as Number ";Block`Number
      If Exist(Blocks`File$) then 
            \\ check if there is space for this block
            If Not filelen(Blocks`File$) div 1024>=Block`Number-1 Then
                  Error "Wrong Block Number"
            End if
      Else
            Print "not exist"
            Open Blocks`File$ for output as #f
            Close #f
            Wait 100
            \\ or Error "Empty File" if we wish only read
            If Block`Number<>1 then Error "Wrong Block Number"
      End if
      Open Blocks`File$ for append as #f
      Seek #f, Block`Number*1024-1023 ' so we seek to first byte
      Put #f, Page1
      Close #f
      Block`Number++
      Page1=NewBlock()
Return

```



## Neko


```ActionScript
/**
 fixed length records, in Neko
*/

var LRECL = 80

var reverse = function(s) {
  var len = $ssize(s)
  if len < 2 return s

  var reverse = $smake(len)
  var pos = 0
  while len > 0 $sset(reverse, pos ++= 1, $sget(s, len -= 1))
  return reverse
}

var file_open = $loader.loadprim("std@file_open", 2)
var file_read = $loader.loadprim("std@file_read", 4)
var file_write = $loader.loadprim("std@file_write", 4)
var file_close = $loader.loadprim("std@file_close", 1)

var input = file_open("infile.dat", "r")
var output = file_open("outfile.dat", "w")

var len
var pos = 0
var record = $smake(LRECL)

while true {
  try {
    len = file_read(input, record, pos, LRECL)
    if len != LRECL $throw("Invalid read")

    len = file_write(output, reverse(record), pos, LRECL)
    if len != LRECL $throw("Invalid write")
  } catch a break;
}
file_close(input)
file_close(output)
```


```txt

prompt$ dd if=infile.txt cbs=80 conv=block status=none of=infile.dat
prompt$ nekoc fixed-length.neko
prompt$ neko fixed-length
prompt$ dd if=outfile.dat cbs=80 conv=unblock status=none
8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL

                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI
NIGRAM TR                                                                 9 eniL

```



## Pascal


```pascal
program reverseFixedLines(input, output, stdErr);
const
	lineLength = 80;
var
	// in Pascal, `string[n]` is virtually an alias for `array[1..n] of char`
	line: string[lineLength];
	i: integer;
begin
	while not eof() do
	begin
		for i := 1 to lineLength do
		begin
			read(line[i]);
		end;
		
		for i := lineLength downto 1 do
		begin
			write(line[i]);
		end;
		writeLn();
	end;
end.
```



## Perl


```perl
open $in,  '<', 'flr-infile.dat';
open $out, '>', 'flr-outfile.dat';

while ($n=sysread($in, $record, 80)) {   # read in fixed sized binary chunks
     syswrite $out, reverse $record;     # write reversed records to file
     print reverse($record)."\n"         # display reversed records, line-by-line
}
close $out;
```

```txt
8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL
                                                                                
                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI     
NIGRAM TR                                                                 9 eniL
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/flr-infile.dat input file]
and
[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/flr-outfile.dat output file]


## Perl 6

Link to a copy of the input file used: [https://github.com/thundergnat/rc/blob/master/resouces/flr-infile.dat flr-infile.dat]

Essentially the same as task [[Selective_File_Copy]] except more boring.


```perl6
$*OUT = './flr-outfile.dat'.IO.open(:w, :bin) orelse .die; # open a file in binary mode for writing
while my $record = $*IN.read(80) {                       # read in fixed sized binary chunks
     $*OUT.write: $record.=reverse;                      # write reversed records out to $outfile
     $*ERR.say: $record.decode('ASCII');                 # display decoded records on STDERR
}
close $*OUT;
```

```txt
8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL
                                                                                
                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI     
NIGRAM TR                                                                 9 eniL
```



## Phix

You might want to investigate using builtins such as get_text() and read_lines(), but they were
never really designed for fixed length record handling and for me, for this task, their limits 
outweight any advantages. To simplify matters, this creates any files needed on the fly.

```Phix
constant sample_text = """
Line 1...1.........2.........3.........4.........5.........6.........7.........8
Line 2
Line 3
Line 4

Line 6
Line 7
     Indented line 8............................................................
Line 9                                                                 RT MARGIN"""

constant indat = "infile.dat",
         outdat = "outfile.dat"

if not file_exists(indat) then
    object text = get_text("sample.txt")
    if text=-1 then text = sample_text end if
    sequence s = split(text,'\n')
    integer fn = open(indat,"wb")
    for i=1 to length(s) do
        puts(fn,s[i]&repeat(' ',80-length(s[i])))
    end for
    close(fn)
    printf(1,"%s created (%d bytes)\n",{indat,get_file_size(indat)})
end if

function get_block(integer fn, size)
    string res = ""
    for i=1 to size do
        res &= getc(fn)
    end for
    return res
end function

integer fn = open(indat,"rb"),
        isize = get_file_size(indat)
puts(1,"reversed:\n")
for i=1 to isize by 80 do
    ?reverse(get_block(fn,80))
end for
close(fn)

-- Bonus part, 16*64 (=1024) block handling:

procedure put_block(integer fn, sequence lines)
    lines &= repeat("",16-length(lines))
    for i=1 to length(lines) do
        string li = lines[i]
        if length(li)>64 then
            li = li[1..64]
        else
            li &= repeat(' ',64-length(li))
        end if
        puts(fn,li)
    end for
end procedure

fn = open(outdat,"wb")
put_block(fn,split(sample_text,'\n'))
close(fn)
integer osize = get_file_size(outdat)
printf(1,"\n%s created (%d bytes):\n",{outdat,osize})
fn = open(outdat,"rb")
sequence lines = {}
for i=1 to osize by 64 do
    string line = get_block(fn,64)
    ?line
    lines = append(lines,trim_tail(line,' '))
end for
lines = trim_tail(lines,{""})
puts(1,"\ntrimmed:\n")
pp(lines,{pp_Nest,1})
{} = delete_file(indat) -- (for consistent 2nd run)
```

```txt

infile.dat created (720 bytes)
reversed:
"8.........7.........6.........5.........4.........3.........2.........1...1 eniL"
"                                                                          2 eniL"
"                                                                          3 eniL"
"                                                                          4 eniL"
"                                                                                "
"                                                                          6 eniL"
"                                                                          7 eniL"
"............................................................8 enil detnednI     "
"NIGRAM TR                                                                 9 eniL"

outfile.dat created (1024 bytes):
"Line 1...1.........2.........3.........4.........5.........6...."
"Line 2                                                          "
"Line 3                                                          "
"Line 4                                                          "
"                                                                "
"Line 6                                                          "
"Line 7                                                          "
"     Indented line 8............................................"
"Line 9                                                          "
"                                                                "
"                                                                "
"                                                                "
"                                                                "
"                                                                "
"                                                                "
"                                                                "

trimmed:
{"Line 1...1.........2.........3.........4.........5.........6....",
 "Line 2",
 "Line 3",
 "Line 4",
 {},
 "Line 6",
 "Line 7",
 "     Indented line 8............................................",
 "Line 9"}

```


## Python


```Python

infile = open('infile.dat', 'rb')
outfile = open('outfile.dat', 'wb')

while True:
    onerecord = infile.read(80)
    if len(onerecord) < 80:
        break
    onerecordreversed = bytes(reversed(onerecord))
    outfile.write(onerecordreversed)

infile.close()
outfile.close()

```


Output:

```txt

 dd if=outfile.dat cbs=80 conv=unblock
 8.........7.........6.........5.........4.........3.........2.........1...1 eniL
 2 eniL
 3 eniL
 4 eniL
 
 6 eniL
 7 eniL
 ............................................................8 enil detnednI
 NIGRAM TR                                                                 9 eniL
 1+1 records in
 1+1 records out

```




## REXX


```rexx
/*REXX pgm reads fixed-length 80 byte records;  reverses each record, displays to term. */
iFID= 'FIXEDLEN.TXT'                             /*the file's filename (used for input).*/
call charin iFID, 1, 0                           /*open the file, point rec pointer to 1*/
                                                 /* [+]  just to be safe, position file.*/
                 do j=1  while chars(iFID)>=80   /*read data records of LRECL ≡ eighty. */
                 @.j= charin(iFID, , 80)         /*read a data record of eighty bytes.  */
                 end   /*j*/
#= j - 1                                         /*adjust # of records (J is 1 too high)*/
                 do k=1  for #                   /* [+]  process all the records read.  */
                 say reverse(@.k)                /* reverse a record and write to term. */
                 end   /*k*/                     /*stick a fork in it,  we're all done. */
```

```txt

8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL

                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI
NIGRAM TR                                                                 9 eniL

```



## zkl

Reading from the dd formatted file, which is 720 characters:

```txt

Line 1...1.........2.........3.........4.........5.........6.........7.........8Line 2                                                                          Line 3                                                                          Line 4                                                                                                                                                          Line 6                                                                          Line 7                                                                               Indented line 8............................................................Line 9                                                                 RT MARGIN

```


```zkl
File("infile.dat","rb")	// could contain nulls and newlines
   // Since we are writing to a ASCII terminal, ignore nulls
.walker(3).chunk(80,String).pump(Console.println,"reverse"); // 3-->read chars
```

```txt

8.........7.........6.........5.........4.........3.........2.........1...1 eniL
                                                                          2 eniL
                                                                          3 eniL
                                                                          4 eniL
                                                                                
                                                                          6 eniL
                                                                          7 eniL
............................................................8 enil detnednI     
NIGRAM TR                                                                 9 eniL

```

To write to a file (as one big line), preserving nulls:

```zkl
in,out := File("infile.dat","rb"), File("outfile.dat","wb");
in.walker(0).chunk(80).pump(out,"reverse"); // may contain nulls and newlines
// 0-->read bytes, chunk to list of bytes, reverse and write the bytes
in.close(); out.close();
```

outfile.dat:
```txt

8.........7.........6.........5.........4.........3.........2.........1...1 eniL                                                                          2 eniL                                                                          3 eniL                                                                          4 eniL                                                                                                                                                          6 eniL                                                                          7 eniL............................................................8 enil detnednI     NIGRAM TR                                                                 9 eniL

```

<b>Bonus Round</b>

```zkl
// read block file (as in read a file of blocks) to text
fcn readFourthBlock(inFileNm){
   out,f,buf := Sink(String), File(inFileNm,"rb"), Data(1024);
   while(f.read(1024,buf,False)){
      // read 64 chars from buf, strip ws from right side, repeat
      buf.walker(3).chunk(64,String).pump(out,T("strip",1),'+("\n"));
   }
   f.close(); out.close();
}
```


```zkl
// read text file and write as block to file
fcn formatToFourthBlock(inFileNm,outFileNm){
   n,blk,in,out := 0, Data(), File(inFileNm,"r"), File(outFileNm,"wb");
   foreach line in (in.walker(11)){	// right side stripped
      if(not line) blk.write(" "*64);
      else blk.write(line.walker().chunk(64,String).pump(String,"%-64s".fmt));
      if(blk.len()==1024){ out.write(blk); blk.clear(); }
   }
   if(blk) out.write(blk, " "*(1024 - blk.len()));
   f.close(); out.close();
}
```

