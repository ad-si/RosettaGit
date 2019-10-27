+++
title = "Hello world/Line printer"
description = ""
date = 2019-09-09T17:30:38Z
aliases = []
[extra]
id = 8380
[taxonomies]
categories = []
tags = []
+++

{{task}} 
[[Category:Hardware]] 
[[Category:Printer]]
{{omit from|PARI/GP}}
{{omit from|ML/I|Does not have printer-related functions}}
{{omit from|SQL PL|It does not handle attached devices}} <!-- The only way is with an external procedure implemented in Java or C that directly writes in the printer. -->

;Task:
Cause a line printer attached to the computer to print a line containing the message:   <big><code> Hello World! </code></big>


;Note:
A line printer is not the same as standard output. 

A   [[wp:line printer|line printer]]   was an older-style printer which prints one line at a time to a continuous ream of paper. 

With some systems, a line printer can be any device attached to an appropriate port (such as a parallel port).





## 360 Assembly


```360asm
HELLO    CSECT
         PRINT NOGEN
         BALR  12,0
         USING *,12
         OPEN  LNPRNTR
         LA    6,HW
         PUT   LNPRNTR
         CLOSE LNPRNTR
         EOJ
LNPRNTR  DTFPR DEVADDR=SYSLST,IOAREA1=L1
L1       DS    0CL133
HW       DC    C'Hello World!'
         END HELLO
```



## Ada

===[[Unix]]===
Assuming that the line printer is attached to /dev/lp0

```Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Line is
   Printer : File_Type;
begin
   begin
      Open (Printer, Mode => Out_File, Name => "/dev/lp0");
   exception
      when others =>
         Put_Line ("Unable to open printer.");
         return;
   end;

   Set_Output (Printer);
   Put_Line ("Hello World!");
   Close (Printer);
end Print_Line;

```



## ALGOL 68


This task is VERY system and hardware dependent.  The code below works
with Algol 68 Genie and a Linux system without /dev/lp0 but with a
remote printer interfaced via CUPS.  Extending it to other
environments is left as an exercise for the reader.

```algol68

BEGIN
   STRING printer name = "/dev/lp0";
   FILE line printer;
   IF open (line printer, printer name, stand out channel) = 0 THEN
      put (line printer, ("Hello world", newline));
      close (line printer)
   ELSE
      put (stand error, ("Can't contact line printer on ", printer name, newline));
      put (stand error, ("Trying to use lpr(1)", newline));
      PIPE printer pipe = execve child pipe ("lpr", "", "");
      IF pid OF printer pipe < 0 THEN
	 put (stand error, ("Oh dear, that didn't seem to work either.  Giving up.", newline));
	 stop
      FI;
      put (write OF printer pipe, ("Hello world", newline));
      close (read OF printer pipe);
      close (write OF printer pipe)
   FI
END

```
 {{out}}

```txt

Can't contact line printer on /dev/lp0
Trying to use lpr(1)

```



## Applesoft BASIC


Assumes a printer card is installed in the Apple II's number 1 expansion slot.


```basic

PR#1
PRINT "HELLO WORLD!"

```



## AutoHotkey


```AutoHotkey

Fileappend, Hello World!, print.txt
Run, print "print.txt" 

```



## AWK

Unix / Linux:

```AWK

BEGIN { print("Hello World!") >"/dev/lp0" }

```



## BASIC


{{works with|QBasic}}

{{works with|ZX Spectrum Basic}}

{{works with|Liberty BASIC}}


```qbasic
LPRINT "Hello World!"
```


=
## BaCon
=
Piping data to ''lp'' would also work.  This example demonstrates writing to a device.

```freebasic
' Hello, printer
READ msg$
DATA "Hello World!\n"

' Assume printer is on /dev/lp0
OPEN "/dev/lp0" FOR DEVICE AS printer
PUTBYTE msg$ TO printer SIZE LEN(msg$)
CLOSE DEVICE printer
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>LPRINT "Hello World!"
```



## Batch File



```dos
ECHO Hello world!>PRN
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      prn% = OPENOUT("PRN:")
      PRINT #prn%, "Hello World!"
      CLOSE #prn%
```



## C

===[[Unix]]===
Assuming that the line printer is attached to /dev/lp0

```C>#include <stdio.h


int main()
{
   FILE *lp;
   lp = fopen("/dev/lp0","w");
   fprintf(lp,"Hello world!\n");
   fclose(lp);
   return 0;
}
```



## C++


```cpp>#include <iostream

#include <fstream>

int main(){
  std::ofstream lprFile;
  lprFile.open( "/dev/lp0" );
  lprFile << "Hello World!\n";
  lprFile.close();
  return 0;
}
```


## C sharp

"My Printer" should be replaced with the friendly name of the printer. 
This is to avoid the extra step of locating the default printer, 
which is out of scope of this example.


```C sharp

[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
public class DOCINFOA
{
    [MarshalAs(UnmanagedType.LPStr)]
    public string pDocName;
    [MarshalAs(UnmanagedType.LPStr)]
    public string pOutputFile;
    [MarshalAs(UnmanagedType.LPStr)]
    public string pDataType;
}

[DllImport("winspool.Drv", EntryPoint = "OpenPrinterA", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool OpenPrinter([MarshalAs(UnmanagedType.LPStr)] string szPrinter, out IntPtr hPrinter, IntPtr pd);

[DllImport("winspool.Drv", EntryPoint = "StartDocPrinterA", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool StartDocPrinter(IntPtr hPrinter, int level, [In, MarshalAs(UnmanagedType.LPStruct)] DOCINFOA di);

[DllImport("winspool.Drv", EntryPoint = "StartPagePrinter", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool StartPagePrinter(IntPtr hPrinter);

[DllImport("winspool.Drv", EntryPoint = "EndPagePrinter", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool EndPagePrinter(IntPtr hPrinter);

[DllImport("winspool.Drv", EntryPoint = "EndDocPrinter", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool EndDocPrinter(IntPtr hPrinter);

[DllImport("winspool.Drv", EntryPoint = "ClosePrinter", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool ClosePrinter(IntPtr hPrinter);

[DllImport("winspool.Drv", EntryPoint = "WritePrinter", CharSet = CharSet.Ansi, ExactSpelling = true)]
public static extern bool WritePrinter(IntPtr hPrinter, IntPtr pBytes, Int32 dwCount, out Int32 dwWritten);

public void HelloWorld()
{
    IntPtr hPrinter;
    bool openSuccessful = OpenPrinter("My Printer", out hPrinter, IntPtr.Zero);
    if (openSuccessful)
    {
        DOCINFOA docInfo = new DOCINFOA();
        docInfo.pDocName = "Hello World Example";
        docInfo.pOutputFile = null;
        docInfo.pDataType = "RAW";

        if (StartDocPrinter(hPrinter, 1, docInfo))
        {
            StartPagePrinter(hPrinter);

            const string helloWorld = "Hello World!";
            IntPtr buf = Marshal.StringToCoTaskMemAnsi(helloWorld);

            int bytesWritten;
            WritePrinter(hPrinter, buf, helloWorld.Length, out bytesWritten);

            Marshal.FreeCoTaskMem(buf);
        }
        if (EndPagePrinter(hPrinter))
            if (EndDocPrinter(hPrinter))
                ClosePrinter(hPrinter);
    }
}
```



## Clipper


```Clipper
SET PRINT ON
SET CONSOLE OFF
? "Hello World!"
SET PRINT OFF
SET CONSOLE ON

```



## Clojure

Translated from Java (mechanically, as I don't understand how to test a line printer):

```clojure
(ns rosetta-code.line-printer
  (:import java.io.FileWriter))

(defn -main [& args]
  (with-open [wr (new FileWriter "/dev/lp0")]
    (.write wr "Hello, World!")))
```



## COBOL


```COBOL
IDENTIFICATION DIVISION.
PROGRAM-ID. GOODBYE-WORLD-PRINTER.

PROCEDURE DIVISION.
DISPLAY 'Hello World!'
	UPON PRINTER
END-DISPLAY.
STOP RUN.
```



## Common Lisp

Assuming that the line printer is attached to /dev/lp0

```Lisp
(defun main ()
  (with-open-file (stream "/dev/lp0"
    :direction :output
    :if-exists :append)
    (format stream "Hello World~%")))

(main)

```



## D


```d
import std.stdio;

void main()
{
    auto lp = File("/dev/lp0", "w");
    lp.writeln("Hello World!");
}

```



## Delphi


```Delphi
program Project1;

{$APPTYPE CONSOLE}

uses Printers;

var
  lPrinterAsTextFile: TextFile;
begin
  AssignPrn(lPrinterAsTextFile);
  Rewrite(lPrinterAsTextFile);
  Writeln(lPrinterAsTextFile, 'Hello World!');
  CloseFile(lPrinterAsTextFile);
end.
```



## Dragon


```dragon

select "files"

f2 = fopen("E:\my.txt", "w")
f = "my data"
writeText(f2,f)
flush(f2)
fclose(f2)

```



## EchoLisp

EchoLisp supports a virtual printer which is not stdout. It is actually an extensible division of the HTML document, with printer pages as subdivisions. Printer and pages may be hidden/shown at convenience.

```lisp

(printer-font "Courier")       ;; change printer font
(printer-page "ROSETTA CODE")  ;; starts a new page with nice header
(printer-writeln "Hello World!") ;; prints new line (not seen on stdout)

```



## EDSAC order code

This program uses self-modifying code to loop through an array of characters. Since the EDSAC character set does not include lower-case letters or exclamation marks, we actually print <tt>HELLO WORLD</tt> followed by a carriage return and a line feed. Strings cannot be null-terminated, because 0 happens to be the character code for <tt>P</tt>; instead, we mark the final character by including a 1 (which has no printable effect) in the least significant bit.

```edsac
[ Hello world
  
### =====


  A program for the EDSAC

  Can be used to print any character string:
  the string (including necessary *F and #F
  characters) should be stored in sequential
  memory addresses beginning at @+17.

  The last character of the string should be
  marked with a 1 in the least significant
  bit. This can be coded by using D in place
  of F, e.g. AD would be an 'A' as the last
  character

  Works with Initial Orders 2 ]

        T56K
        GK

[  0 ]  O17@    [ Print character        ]

[  1 ]  H17@    [ AND character with 1:  ]
        C15@    [ if the result is 1, we ]
        S15@    [ have reached the end   ]
        E13@    [ of the string          ]

        T14@    [ Modify the orders in   ]
        A@      [ addresses @+0 and @+1  ]
        A16@    [ to point to the next   ]
        T@      [ character              ]
        A1@
        A16@
        T1@
        E@

[ 13 ]  ZF

[ 14 ]  PF
[ 15 ]  PD
[ 16 ]  P1F     [ NB Least significant bit
                  is not part of address,
                  so add 2 not 1         ]

[ 17 ]  *F      [ Letter shift           ]
        HF
        EF
        LF
        LF
        OF
        !F      [ Blank                  ]
        WF
        OF
        RF
        LF
        DF
        @F      [ Carriage return        ]
        &D      [ Line feed + 1          ]

        EZPF
```



## ERRE


```ERRE

! Hello World in ERRE language
PROGRAM HELLO
BEGIN
  !$REDIR
  PRINT("Hello World !")
  !$NOREDIR
END PROGRAM

```


Prints on LPT1: (if exists) without opening a file.
'''Note''': !$... is a directive pragma not a part of
the language.


## Factor

Prints through Unix "lpr" command.


```factor
( scratchpad ) USE: io.encodings.utf8
( scratchpad ) USE: io.launcher
( scratchpad ) "lpr" utf8 [ "Hello World!" print ] with-process-writer
```




## Forth

Forth systems currently run on everything from bare metal to modern multi-user operating systems and printers are handled differently on each. This demonstration shows a common way that text output is re-directed to printers and/or other devices by vectoring the action of the Forth word EMIT.  Emit takes one character off the stack and outputs it to a device. By defining all I/O with the primitive operation EMIT, we can vector the output anywhere we choose, even on hardware with no O/S. Here we show a very basic printer device driver for an embedded system that adds I/O re-direction to the system's Forth language.

```Forth
\ No operating system, embedded device, printer output example

defer emit                                   \ deferred words in Forth are a place holder for an 
                                             \ execution token (XT) that is assigned later.
                                             \ When executed the deferred word simply runs that assigned routine 

: type ( addr count -- )                     \ type a string uses emit
       bounds ?do  i c@ emit   loop ;        \ type is used by all other text output words in the system

HEX 
: CR   ( -- )  0A emit 0D emit ;             \ send a carriage return, linefeed pair with emit

\ memory mapped I/O addresses for the printer port
B02E   constant scsr                         \ serial control status register 
B02F   constant scdr                         \ serial control data register

: printer-emit ( char -- )                   \ output 'char' to the printer serial port  
         begin   scsr C@ 80 and  until       \ loop until the port shows a ready bit
         scdr C!                             \ C! (char store) writes a byte to an address
         20 ms ;                             \ 32 mS delay to prevent over-runs

: console-emit ( char -- )   ...             \ defined in the Forth system, usually assembler

\ vector control words
: >console     ['] console-emit is EMIT ;     \ assign the execution token of console-emit to EMIT
: >printer     ['] printer-emit is EMIT ;     \ assign the execution token of printer-emit to EMIT
```


Usage Examples:

```txt

S" Hello Console World!" TYPE CR              \ default output goes to console
S" Hello Printer World!" >PRINTER TYPE CR     \ re-direct to printer
>CONSOLE                                      \ return output to console  

```



## Fortran

Fortran I/O statements refer to logical unit numbers to select the file. The device associated with a unit number depends on the computer installation, and can also be arranged via instructions to the operating system. A value such as 6 is often the default for the lineprinter on mainframe systems but on PCs it might be for the computer screen. Thus the "6". The "1" is the label number of the FORMAT statement.

Output to the lineprinter has a "carriage control character" as the first output position, thus a lineprinter capable of 120 characters to a line would be fed up to 12'''1''' characters of output, the first printing position (column one on the lineprinter output) would receive the ''second'' character of the output, and so on. This could cause surprises. A <code>FORMAT (I6,etc)</code> rather than <code>FORMAT(1X,I5,etc)</code> used to print a five-digit integer at the start of a line (with the leading space being supplied by the first of the six supplied by I6) works well and saves a little on the complexity of the format statement, but if the integer's value were to exceed 99999, say be 100000, the first character is no longer a space but a one, and so the output will suddenly be one line to a page...

The protocol was to act on the carriage control, then print the line. The character code interpretations were
 + No movement - thus overprint.
   (a blank) Advance one line.
 0 Advance two lines - thus leave a blank line.
 1 Page throw.
The page-throw was actually a "skip to control column 1"; that is, the lineprinter has an associated paper tape in a loop with holes punched in certain columns of the tape and the tape would be advanced one position for each line advance. The length of the loop matches the number of lines to a page of printout, or was twice that number, etc. A hole in column one of the loop would be aligned (by the human operator during setup) with the top-of-form paper position and when a carriage control of "1" was acted on, the lineprinter would skip forwards until the "1" hole was detected. A carriage control character of "2" would thus skip onwards until a hole in column two was detected - and if there was no such hole, the skip wouldn't stop until the human operator noticed. Thus, many control tapes had all columns punched across, not just one. However, this ability was more properly used in producing vast outputs with subsections to a page suitably marked by suitable holes. The benefit was firstly that the printer skipped to a hole mark more rapidly than via a sequence of "advance one" or "advance two" commands, and secondly, the program did not need to generate such sequences nor have then saved via output spooling. But all relied on the right output being matched to the right tape. This was more typical at COBOL installations.

It is because of the first character disappearing as carriage control that the "list" style output (as in <code>WRITE (6,*) "Hello World!"</code>) always starts a line of output with a space. This form does not require a FORMAT statement. 

Since for a new job, output would commence with the lineprinter already at the top of a new page, an overprint (no carriage advance) thus means writing to the very first line. If however, top-of-page placement was not assured at your installation, then "1HELLO WORLD!" would do.

```Fortran
      WRITE (6,1)
    1 FORMAT ("+HELLO WORLD!")
      END 
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open Lpt "Lpt:" As #1 '' prints to default printer
Print #1, "Hello World!"
Close #1
```



## Go


```go
package main

import (
	"fmt"
	"os"
)

func main() {
	lp0, err := os.Create("/dev/lp0")

	if err != nil {
		panic(err)
	}

	defer lp0.Close()

	fmt.Fprintln(lp0, "Hello World!")
}
```



## Groovy


```groovy
new File('/dev/lp0').write('Hello World!\n')

```



## GUISS



```guiss
Start,Programs,Accessories,Notepad,Type:Goodbye World[pling],
Menu:File,Print,Button:OK
```



## Harbour


```visualfoxpro
SET PRINT ON
SET CONSOLE OFF
? "Hello World!"
SET PRINT OFF
SET CONSOLE ON
```



## Haskell


```haskell
import System.Process (ProcessHandle, runCommand)

main :: IO ProcessHandle
main = runCommand "echo \"Hello World!\" | lpr"
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages, provided printer is attached to <tt>/dev/lp0</tt>.


```unicon
procedure main()
    write(open("/dev/lp0","w"),"Hello, world!")
end
```



## Integer BASIC


See [[#Applesoft BASIC|Applesoft BASIC]].


## J



```j
require'print'
print'Hello world!'
```



## Java



```java
import java.io.FileWriter;
import java.io.IOException;
 
public class LinePrinter {
  public static void main(String[] args) {
    try {
      FileWriter lp0 = new FileWriter("/dev/lp0");
      lp0.write("Hello World!");
      lp0.close();
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }
}
```



## JavaScript

{{works with|Node.js}}

```javascript
// This example runs on Node.js
var fs = require('fs');
// Assuming lp is at /dev/lp0
var lp = fs.openSync('/dev/lp0', 'w');
fs.writeSync(lp, 'Hello, world!\n');
fs.close(lp);
```

{{works with|Firefox}}
{{works with|Chromium}}

```javascript

document.write("Hello World!");
print(); //Opens a dialog.

```




## Julia


```julia

lineprinter = Sys.iswindows() ? "LPT3" : "/dev/lp0"
lp = open(lineprinter, "w")
write(lp, "Hello world")

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
import java.io.File

fun main(args: Array<String>) {
    val text = "Hello World!\n"
    File("/dev/lp0").writeText(text)
}
```



## Lasso



```lasso
File_Write: '/dev/lp0', 'Hello world', -FileOverWrite;
```



## Locomotive Basic



```locobasic
10 PRINT #8, "Hello World!"
```



## M2000 Interpreter

We can use printer like a page printer

```M2000 Interpreter

Printer {
      \\ just change the current layer to Print Page
      \\ Using Layer { } we can change to basic console layer inside any layer
      Print "Hello World!"
}

```


Or we can use ANSI output using a file for export in Lpt1


```M2000 Interpreter

Try ok {
      Open "Lpt1" For OutPut As N '' prints to Lpt1 if exist a printer 
      Print #N, "Hello World!"
      Close #N
}
If Not Ok Then Print "Can't Print"

```


If we have a file in current dir we can use a Dos command:

```M2000 Interpreter

Dos "Print /d:lpt1 file " +quote$(dir$+"this.txt");

```

Using ; at the end of DOS command we have no open terminal


```M2000 Interpreter

Dos "command" [, sleep time after call] [;]

```



## Maple
 

```Maple
lprint("Hello World!")
```


=={{header|Mathematica}} / {{header|Wolfram Language}}== 

```Mathematica
commandstring = "echo Hello World!  | lpr -P Printer01"
Run[commandstring]
```


=={{header|MATLAB}} / {{header|Octave}}== 
===[[Unix]]===
Assuming that the line printer is attached to /dev/lp0

```Matlab
  fid = fopen('/dev/lp0'); 
  fprintf(fid,'Hello World!\n');
  fclose(fid);
```



## N/t/roff


/.ROFF/, being a document formatting language, is especially suited for formatting documents and sending them to printers of nearly all types.  In fact, /.ROFF/ has been used to print documents on line printers.
To send the output to the line printer, you must compile the source file with the following command on the shell, assuming the source file is <code>file.roff</code> and that the line printer is already setup properly.

<code>
nroff -Tlpr file.roff
</code>

In this case, you must use NROFF, not TROFF, to compile the source file, as NROFF is better-suited for monospaced, typewriter-style line formatting.

Because /.ROFF/ is a document formatting language, the majority of lines in a typical /.ROFF/ source file is to be textual input.  This input is typeset directly onto the output medium.  Therefore, the user need not call a procedure to print text to any terminal.


```N/t/roff

Hello World!

```



## Nim

Assuming that the line printer is attached to /dev/lp0:

```nim
var lp = open("/dev/lp0", fmWrite)
lp.writeln "Hello World"
lp.close()
```



## MIXAL


```MIXAL

LPR	EQU	18
STRING	EQU	2000
	ORIG	3000
START	IOC	0(LPR)
	OUT	STRING(LPR)
	HLT
	ORIG	STRING
	ALF	HELLO
	ALF	 WORL
	ALF	D!
	END	START

```



## OCaml

Assuming that the line printer is attached to /dev/lp0

```ocaml
let () =
  let oc = open_out "/dev/lp0" in
  output_string oc "Hello world!\n";
  close_out oc ;;
```



## Oforth


```Oforth
File new("/dev/lp0") dup open(File.WRITE) "Hello world\n" << close
```



## OpenEdge/Progress


```progress
OUTPUT TO PRINTER.
PUT UNFORMATTED "Hello world!" SKIP.
OUTPUT CLOSE.
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|Printer}}
Example from the FreePascal documentation:

```pascal
program testprn;
uses printer;
var i: integer;
    f: text;
begin
  writeln ( 'Test of printer unit' );
  writeln ( 'Writing to lst ...' );
  for i := 1 to 80 do
    writeln ( lst, 'This is line', i, '.' #13 );
  close ( lst );
  writeln ( 'Done.' );
  {$ifdef Unix }
  writeln ( 'Writing to pipe ...' );
  assignlst ( f, '|/usr/bin/lpr −m' );
  rewrite ( f );
  for i:= 1 to 80 do
    writeln ( f, 'This is line ', i, '.'#13 );
  close ( f );
  writeln ( 'Done.' )
  {$endif}
end.
```



## Perl

Assuming that the line printer is attached to /dev/lp0

```perl
open O, ">", "/dev/lp0";
print O "Hello World!\n";
close O;
```



## Perl 6



```perl6
my $lp = open '/dev/lp0', :w;
$lp.say: 'Hello World!';
$lp.close;
```


Or using <code>given</code> to avoid having to write the variable name repeatedly:


```perl6
given open '/dev/lp0', :w {
    .say: 'Hello World!';
    .close;
}
```



## Phix

If you have not got something appropriate attached, this will just hang. Other values you can try, on windows: "AUX", "COM1", "COM2", "LPT1"

```Phix
integer fn = open(iff(platform()=WIN32?"PRN":"/dev/lp0"),"w")
if fn=-1 then
    puts(1,"some error")
else
    puts(fn,"Hello World!")
    close(fn)
    puts(1,"success!")
end if
{} = wait_key()
```



## PHP


```PHP
<?php
file_put_contents('/dev/lp0', 'Hello world!');
?>
```



```PHP
<?php
fclose(STDOUT);
$STDOUT = fopen('/dev/lp0', 'a');
echo 'Hello world!';
?>
```



## PicoLisp


```PicoLisp
(out '(lpr "-P" "Printer01")
   (prinl "Hello world") )
```



## PL/I


```pli

hello: procedure options(main);
   put skip list('Hello world.');
end hello;
```



## PostScript

Technically not really correct, as this has to be sent to the printer directly. 
It will output Hello world, then, though.

```postscript
<</PageSize [595 842]>> setpagedevice  % set page size to DIN A4
/Courier findfont                      % use Courier
12 scalefont setfont                   % 12 pt
28 802 moveto                          % 1 cm from the top and left edges
(Hello world) show                     % draw the string
```



## PureBasic

{{libheader|PureLPRINT}}

```PureBasic
MyPrinter$ = LPRINT_GetDefaultPrinter()
If LPRINT_OpenPrinter(MyPrinter$)
  If LPRINT_StartDoc("Printing a RC-Task")
    LPRINT_Print(Chr(27) + "E") ; PCL reset for HP Printers
    LPRINT_PrintN("Hello World!")
    LPRINT_NewPage()
    LPRINT_EndDoc()
  EndIf
  LPRINT_ClosePrinter()  
EndIf
```



## Python

Assuming that the line printer is attached to /dev/lp0:

```python
lp = open("/dev/lp0")
lp.write("Hello World!\n")
lp.close()
```


If the above code gives you the error "IOError: File not open for writing", try:

```python
lp = open("/dev/lp0","w")
lp.write("Hello World!\n")
lp.close()
```



## Racket



```racket

#lang racket
(define (print text)
  ;; try lpr first
  (define lpr-exe (find-executable-path "lpr"))
  ;; otherwise use a special file
  (if lpr-exe
    (with-input-from-string (~a text "\n") (λ() (void (system* lpr-exe))))
    (with-output-to-file #:exists 'append
      (case (system-type) [(windows) "PRN"] [else "/dev/lp0"])
      (λ() (displayln text)))))
(print "Hello World!")

```



## REXX

There is no direct way for REXX programs to write to the printer, 
but a shell command could be used.


In DOS (or under Windows):

```rexx
/*REXX program prints a string to the  (DOS) line printer  via redirection to a printer.*/
$= 'Hello World!'                                /*define a string to be used for output*/
'@ECHO'   $    ">PRN"                            /*stick a fork in it,  we're all done. */
```



## RPG

{{works with|ILE RPG}}

```RPG

      Fqsysprt   O    F   80        printer                                 
      C                   except                                            
      C                   seton                                        LR   
      Oqsysprt   E                                                          
      O                                           11 'Hello world'          

```



## Ring


```ring

   lp = fopen("/dev/lp0","w")  fputs(lp,"Hello world!")  fclose(lp)

```



## Ruby

Assumes that <code>lpr</code> command reaches printer.


```ruby
open("| lpr", "w") { |f| f.puts "Hello World!" }
```



## Run BASIC


```runbasic
 shell$("echo \"Hello World!\" | lpr")
```



## Salmon

Assuming /dev/lp0 accesses the printer:


```Salmon
open_output_text_file("/dev/lp0").print("Hello World!");
```


Assuming lpr is a command that prints to a printer:

```Salmon
`echo "Hello World!" | lpr`;
```



## Rust


### Unix


```rust
use std::fs::OpenOptions;
use std::io::Write;

fn main() {
    let file = OpenOptions::new().write(true).open("/dev/lp0").unwrap();
    file.write(b"Hello, World!").unwrap();
}
```



## Scala

{{libheader|Scala}}

### All platforms


```scala
import java.awt.print.PrinterException
import scala.swing.TextArea

object LinePrinter extends App {
  val (show, context) = (false, "Hello, World!")
  try // Default Helvetica, 12p
    new TextArea(context) {
      append(" in printing.")
      peer.print(null, null, show, null, null, show)
    }
  catch {
    case ex: PrinterException => ex.getMessage()
  }
  println("Document printed.")
}
```


===[[Unix]]===
Assuming device is attached to lp0

```Scala
object LinePrinter extends App {
  import java.io.{ FileWriter, IOException }
  {
    val lp0 = new FileWriter("/dev/lp0")
    lp0.write("Hello, world!")
    lp0.close()
  }
}
```



## Scheme

===[[Unix]]===
Assuming device is attached to lp0

```scheme
(call-with-output-file "/dev/lp0"
  (lambda (printer)
    (write "Hello World!" printer)))
```



## Seed7

Assuming that the line printer is attached to /dev/lp0:

```seed7
$ include "seed7_05.s7i";
 
const proc: main is func
  local
    var file: lp is STD_NULL;
  begin
    lp := open("/dev/lp0", "w");
    writeln(lp, "Hello world!");
    close(lp);
  end func;
```



## Sidef


```ruby
Sys.open(\var fh, '>', '/dev/lp0') \
    && fh.say("Hello World!")      \
    && fh.close
```



## Simula

{{works with|SIMULA-67}}

```simula
BEGIN
   OUTTEXT("Hello World!");
   OUTIMAGE
END
```



## SNOBOL4


In SNOBOL4, variables can be associated with input and output files.  
Assigning a value to an output-associated variable also writes it to the associated output file.  (Likewise, accessing a variable associated with an input file returns as its value the next record from the associated input file.)  
By default, the variable "input" is associated with standard input, and the variable "output" is associated with standard output.


```SNOBOL4
     output = "Hello, world."
```


You can associate the variable "print" with lpt1 (the default local printer port) using the output() function:


```SNOBOL4
     output(.print,25,"lpt1")
     print = "Hello, world."
```


## Swift


```Swift
import Foundation

let out = NSOutputStream(toFileAtPath: "/dev/lp0", append: true)
let data = "Hello, World!".dataUsingEncoding(NSUTF8StringEncoding, allowLossyConversion: false)
out?.open()
out?.write(UnsafePointer<UInt8>(data!.bytes), maxLength: data!.length)
out?.close()
```



## Tcl


===[[Unix]]===

```tcl
exec lp << "Hello World!"
```


```tcl
set f [open |lp w]
puts $f "Hello World!"
close $f
```


===[[Windows]]===


```tcl
set f [open prn w]
puts $f "Hello World!"
close $f
```



## UNIX Shell

Use ''one'' of the following lines.


```bash
# Use the default printer queue, with lp(1) or lpr(1).
#  1. The system must have a printer queue.
#  2. The printer queue must understand plain text.
#  3. System V has lp(1). BSD has lpr(1).
#     CUPS has both lp(1) and lpr(1).
#
echo 'Hello World!' | lp
echo 'Hello World!' | lpr

# Use a character device.
#  1. The device must understand plain text.
#  2. You must have write permission for the device.
#  3. Some systems have /dev/lp0, /dev/lp1, ...
#  4. BSD has /dev/lpt0, /dev/lpt1, ... for the parallel ports;
#     and /dev/ulpt0, /dev/ulpt1, ... for the USB printers.
# Note that intermingling can occur if two processes write to the device at the
# same time. Using the print spooler method above avoids this problem,
#
echo 'Hello World!' >/dev/lp0
echo 'Hello World!' >/dev/lpt0
echo 'Hello World!' >/dev/ulpt0
```



## XPL0


```XPL0
code Text=12;
Text(2, "Hello World!
");
```


The 2 directs the output to the printer (LPT1). 
Output is usually directed to the console using device code 0 instead.

A carriage return and line feed are normally required to make a line printer actually print. 
(A laser or inkjet printer may require a form feed.) 
However, some printers, or printer drivers, have a timeout feature
that print even without the CR+LF (or FF). 
The CR+LF can simply be included in the string as shown. 
Another method is to include the CR+LF control characters as ^M^J.
