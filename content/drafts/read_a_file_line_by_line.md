+++
title = "Read a file line by line"
description = ""
date = 2019-05-19T18:41:42Z
aliases = []
[extra]
id = 9412
[taxonomies]
categories = []
tags = []
+++

{{task|File handling}} [[Category:Simple]]
{{omit from|GUISS}}
{{omit from|Lotus 123 Macro Scripting|Reads the whole file at once}}
{{omit from|Openscad}}
{{omit from|TI-83 BASIC|No filesystem.}}
{{omit from|TI-89 BASIC|No filesystem.}}
{{omit from|Unlambda|Does not know files.}}

Read a file one line at a time,
as opposed to [[Read entire file|reading the entire file at once]].


;Related tasks:
* [[Read a file character by character]]
* [[Input loop]].





## 360 Assembly

This program uses OS QSAM I/O macros (OPEN,CLOSE,GET,PUT,DCB).

```360asm
*        Read a file line by line  12/06/2016
READFILE CSECT
         SAVE  (14,12)             save registers on entry
         PRINT NOGEN
         BALR  R12,0               establish addressability
         USING *,R12               set base register
         ST    R13,SAVEA+4         link mySA->prevSA
         LA    R11,SAVEA           mySA
         ST    R11,8(R13)          link prevSA->mySA
         LR    R13,R11             set mySA pointer
         OPEN  (INDCB,INPUT)       open the input file
         OPEN  (OUTDCB,OUTPUT)     open the output file
LOOP     GET   INDCB,PG            read record
         CLI   EOFFLAG,C'Y'        eof reached?
         BE    EOF
         PUT   OUTDCB,PG           write record
         B     LOOP
EOF      CLOSE (INDCB)             close input
         CLOSE (OUTDCB)            close output
         L     R13,SAVEA+4         previous save area addrs
         RETURN (14,12),RC=0       return to caller with rc=0
INEOF    CNOP  0,4                 end-of-data routine
         MVI   EOFFLAG,C'Y'        set the end-of-file flag
         BR    R14                 return to caller
SAVEA    DS    18F                 save area for chaining
INDCB    DCB   DSORG=PS,MACRF=PM,DDNAME=INDD,LRECL=80,                 *
               RECFM=FB,EODAD=INEOF
OUTDCB   DCB   DSORG=PS,MACRF=PM,DDNAME=OUTDD,LRECL=80,                *
               RECFM=FB
EOFFLAG  DC    C'N'                end-of-file flag
PG       DS    CL80                buffer
         YREGS
         END   READFILE
```



## 8th


```forth

"path/to/file" f:open ( . cr ) f:eachline f:close

```



## Ada

{{works with|Ada 2005}}
line_by_line.adb:

```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Line_By_Line is
   File : File_Type;
begin
   Open (File => File,
         Mode => In_File,
         Name => "line_by_line.adb");
   While not  End_Of_File (File) Loop
      Put_Line (Get_Line (File));
   end loop;

   Close (File);
end Line_By_Line;

```


{{Out}}

```txt
with Ada.Text_IO;  use Ada.Text_IO;

procedure Line_By_Line is
   File : File_Type;
begin
   Open (File => File,
         Mode => In_File,
         Name => "line_by_line.adb");
   While not  End_Of_File (File) Loop
      Put_Line (Get_Line (File));
   end loop;

   Close (File);
end Line_By_Line;

```



## Aime


```aime
file f;
text s;

f.affix("src/aime.c");

while (f.line(s) != -1) {
    o_text(s);
    o_byte('\n');
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extension to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.5 algol68g-2.3.5].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: ./Read_a_file_line_by_line.a68'''
```algol68
#!/usr/local/bin/a68g --script #

FILE foobar;
INT errno = open(foobar, "Read_a_file_line_by_line.a68", stand in channel);

STRING line;
FORMAT line fmt = $gl$;

PROC mount next tape = (REF FILE file)BOOL: (
  print("Please mount next tape or q to quit");
  IF read char = "q" THEN done ELSE TRUE FI
);

on physical file end(foobar, mount next tape);
on logical file end(foobar, (REF FILE skip)BOOL: done);

FOR count DO
  getf(foobar, (line fmt, line));
  printf(($g(0)": "$, count, line fmt, line))
OD;
done: SKIP
```

{{out}}
<pre style="height:15ex;overflow:scroll">
1: #!/usr/local/bin/a68g --script #
2:
3: FILE foobar;
4: INT errno = open(foobar, "Read_a_file_line_by_line.a68", stand in channel);
5:
6: STRING line;
7: FORMAT line fmt = $gl$;
8:
9: PROC mount next tape = (REF FILE file)BOOL: (
10:   print("Please mount next tape or q to quit");
11:   IF read char = "q" THEN done ELSE TRUE FI
12: );
13:
14: on physical file end(foobar, mount next tape);
15: on logical file end(foobar, (REF FILE skip)BOOL: done);
16:
17: FOR count DO
18:   getf(foobar, (line fmt, line));
19:   printf(($g(0)": "$, count, line fmt, line))
20: OD;
21: done: SKIP

```



## Astro


```python
for line in lines open('input.txt'):
    print line

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program readfile.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4
.equ OPEN,   5
.equ CLOSE,  6

.equ O_RDWR,  0x0002                    @ open for reading and writing

.equ BUFFERSIZE,          100
.equ LINESIZE,            100

/*******************************************/
/* Structures                               */
/********************************************/
/* structure read file*/
    .struct  0
readfile_Fd:                           @ File descriptor
    .struct  readfile_Fd + 4
readfile_buffer:                       @ read buffer
    .struct  readfile_buffer + 4
readfile_buffersize:                   @ buffer size
    .struct  readfile_buffersize + 4
readfile_line:                         @ line buffer
    .struct  readfile_line + 4
readfile_linesize:                     @ line buffer size
    .struct  readfile_linesize + 4
readfile_pointer:
    .struct  readfile_pointer + 4      @ read pointer  (init to buffer size + 1)
readfile_end:
/* Initialized data */
.data
szFileName:              .asciz "fictest.txt"
szCarriageReturn:        .asciz "\n"
/* datas error display */
szMessErreur:        .asciz "Error detected.\n"
szMessErr:           .ascii "Error code hexa : "
sHexa:               .space 9,' '
                     .ascii "  decimal :  "
sDeci:               .space 15,' '
                     .asciz "\n"

/* UnInitialized data */
.bss
sBuffer:             .skip BUFFERSIZE             @ buffer result
szLineBuffer:        .skip LINESIZE
.align 4
stReadFile:          .skip readfile_end

/*  code section */
.text
.global main
main:
    ldr r0,iAdrszFileName               @ File name
    mov r1,#O_RDWR                      @  flags
    mov r2,#0                           @ mode
    mov r7,#OPEN                        @ open file
    svc #0
    cmp r0,#0                           @ error ?
    ble error
    ldr r1,iAdrstReadFile               @ init struture readfile
    str r0,[r1,#readfile_Fd]            @ save FD in structure
    ldr r0,iAdrsBuffer                  @ buffer address
    str r0,[r1,#readfile_buffer]
    mov r0,#BUFFERSIZE                  @ buffer size
    str r0,[r1,#readfile_buffersize]
    ldr r0,iAdrszLineBuffer             @ line buffer address
    str r0,[r1,#readfile_line]
    mov r0,#LINESIZE                    @ line buffer size
    str r0,[r1,#readfile_linesize]
    mov r0,#BUFFERSIZE + 1              @ init read pointer
    str r0,[r1,#readfile_pointer]
1:                                      @ begin read loop
    mov r0,r1
    bl readLineFile
    cmp r0,#0
    beq end                             @ end loop
    blt error

    ldr r0,iAdrszLineBuffer             @  display line
    bl affichageMess
    ldr r0,iAdrszCarriageReturn         @ display line return
    bl affichageMess
    b 1b                                @ and loop

end:
    ldr r1,iAdrstReadFile
    ldr r0,[r1,#readfile_Fd]            @ load FD to structure
    mov r7, #CLOSE                      @ call system close file
    svc #0
    cmp r0,#0
    blt error
    mov r0,#0                           @ return code
    b 100f
error:
    ldr r1,iAdrszMessErreur             @ error message
    bl   displayError
    mov r0,#1                           @ return error code
100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrsBuffer:               .int sBuffer
iAdrszFileName:            .int szFileName
iAdrszMessErreur:          .int szMessErreur
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrstReadFile:            .int stReadFile
iAdrszLineBuffer:          .int szLineBuffer
/******************************************************************/
/*     sub strings  index start  number of characters             */
/******************************************************************/
/* r0 contains the address of the structure */
/* r0 returns number of characters or -1 if error */
readLineFile:
    push {r1-r8,lr}                             @ save  registers
    mov r4,r0                                   @ save structure
    ldr r1,[r4,#readfile_buffer]
    ldr r2,[r4,#readfile_buffersize]
    ldr r5,[r4,#readfile_pointer]
    ldr r6,[r4,#readfile_linesize]
    ldr r7,[r4,#readfile_buffersize]
    ldr r8,[r4,#readfile_line]
    mov r3,#0                                   @ line pointer
    strb r3,[r8,r3]                             @ store zéro in line buffer
    cmp r5,r2                                   @ pointer buffer < buffer size ?
    ble 2f                                      @ no file read
1:                                              @ loop read file
    ldr r0,[r4,#readfile_Fd]
    mov r7,#READ                                @ call system read file
    svc 0
    cmp r0,#0                                   @ error read or end ?
    ble 100f
    mov r7,r0                                   @ number of read characters
    mov r5,#0                                   @ init buffer pointer

2:                                              @ begin loop copy characters
    ldrb r0,[r1,r5]                             @ load 1 character read buffer
    cmp r0,#0x0A                                @ end line ?
    beq 4f
    strb r0,[r8,r3]                             @ store character in line buffer
    add r3,#1                                   @ increment pointer line
    cmp r3,r6
    movgt r0,#-2                                @ line buffer too small -> error
    bgt 100f
    add r5,#1                                   @ increment buffer pointer
    cmp r5,r2                                   @ end buffer ?
    bge 1b                                      @ yes new read
    cmp r5,r7                                   @ read characters ?
    blt 2b                                      @ no loop
                                                @ final
    cmp r3,#0                                   @ no characters in line buffer ?
    beq 100f
4:
    mov r0,#0
    strb r0,[r8,r3]                             @ store zéro final
    add r5,#1
    str r5,[r4,#readfile_pointer]               @ store pointer in structure
    str r7,[r4,#readfile_buffersize]            @ store number of last characters
    mov r0,r3                                   @ return length of line
100:
    pop {r1-r8,lr}                              @ restaur registers
    bx lr                                       @ return

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return
/***************************************************/
/*   display error message                        */
/***************************************************/
/* r0 contains error code  r1 : message address */
displayError:
    push {r0-r2,lr}                         @ save registers
    mov r2,r0                               @ save error code
    mov r0,r1
    bl affichageMess
    mov r0,r2                               @ error code
    ldr r1,iAdrsHexa
    bl conversion16                         @ conversion hexa
    mov r0,r2                               @ error code
    ldr r1,iAdrsDeci                        @ result address
    bl conversion10S                        @ conversion decimale
    ldr r0,iAdrszMessErr                    @ display error message
    bl affichageMess
100:
    pop {r0-r2,lr}                          @ restaur registers
    bx lr                                   @ return
iAdrszMessErr:                 .int szMessErr
iAdrsHexa:                     .int sHexa
iAdrsDeci:                     .int sDeci
/******************************************************************/
/*     Converting a register to hexadecimal                      */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion16:
    push {r1-r4,lr}                          @ save registers
    mov r2,#28                               @ start bit position
    mov r4,#0xF0000000                       @ mask
    mov r3,r0                                @ save entry value
1:                                           @ start loop
    and r0,r3,r4                             @ value register and mask
    lsr r0,r2                                @ move right
    cmp r0,#10                               @ compare value
    addlt r0,#48                             @ <10  ->digit
    addge r0,#55                             @ >10  ->letter A-F
    strb r0,[r1],#1                          @ store digit on area and + 1 in area address
    lsr r4,#4                                @ shift mask 4 positions
    subs r2,#4                               @ counter bits - 4 <= zero  ?
    bge 1b                                   @ no -> loop

100:
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}       @ save registers
    mov r2,r1             @ debut zone stockage
    mov r3,#'+'           @ par defaut le signe est +
    cmp r0,#0             @ negative number ?
    movlt r3,#'-'         @ yes
    mvnlt r0,r0           @ number inversion
    addlt r0,#1
    mov r4,#10            @ length area
1:                        @ start loop
    bl divisionpar10U
    add r1,#48            @ digit
    strb r1,[r2,r4]       @ store digit on area
    sub r4,r4,#1          @ previous position
    cmp r0,#0             @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]       @ store signe
    subs r4,r4,#1         @ previous position
    blt  100f             @ if r4 < 0 -> end

    mov r1,#' '           @ space
2:
    strb r1,[r2,r4]       @store byte space
    subs r4,r4,#1         @ previous position
    bge 2b                @ loop if r4 > 0
100:
    pop {r0-r4,lr}        @ restaur registers
    bx lr
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD


```



## AutoHotkey



```AutoHotkey
; --> Prompt the user to select the file being read

FileSelectFile, File, 1, %A_ScriptDir%, Select the (text) file to read, Documents (*.txt) ; Could of course be set to support other filetypes
If Errorlevel ; If no file selected
	ExitApp

; --> Main loop: Input (File), Output (Text)

Loop
{
FileReadLine, Line, %File%, %A_Index% ; Reads line N (where N is loop iteration)
if Errorlevel ; If line does not exist, break loop
	break
Text .= A_Index ". " Line . "`n" ; Appends the line to the variable "Text", adding line number before & new line after
}

; --> Delivers the output as a text file

FileDelete, Output.txt ; Makes sure output is clear before writing
FileAppend, %Text%, Output.txt ; Writes the result to Output.txt
Run Output.txt ; Shows the created file
```



## AWK

Reading files line-by-line is the standard operation of awk.

'''One-liner:'''

```AWK
awk '{ print $0 }' filename
```


'''Shorter:'''

Printing the input is the default-action for matching lines,
and "1" evaluates to "True",

so this is the shortest possible awk-program
(not counting the [[Empty program]]):

```AWK
awk '1' filename
```


'''Longer:'''

Reading several files, with some processing:

```AWK
# usage: awk  -f readlines.awk  *.txt
BEGIN  { print "# Reading..." }
FNR==1 { f++; print "# File #" f, ":", FILENAME }
/^#/   { c++; next }               # skip lines starting with "#", but count them
/you/  { gsub("to", "TO") }        # change text in lines with "you" somewhere
/TO/   { print FNR,":",$0; next }  # print with line-number
       { print }                   # same as "print $0"
END    { print "# Done with", f, "file(s), with a total of", NR, "lines." }
END    { print "# Comment-lines:", c }
```

Note:
* The variables c and f are initialized automatically to 0
*  NR is the number of records read so far, for all files read
* FNR is the number of records read from the current file
* There can be multiple BEGIN and END-blocks
{{in}}

```txt

# This is the file input.txt
you can use it
to provide input
to your program to do
some processing.

```

{{out}}

```txt

# Reading...
# File #1 : input.txt
you can use it
to provide input
4 : TO your program TO do
some processing.
# Done with 1 file(s), with a total of 5 lines.
# Comment-lines: 1

```



## BASIC


=
## BaCon
=

```freebasic
' Read a file line by line
filename$ = "readlines.bac"
OPEN filename$ FOR READING AS fh
READLN fl$ FROM fh
WHILE ISFALSE(ENDFILE(fh))
    INCR lines
    READLN fl$ FROM fh
WEND
PRINT lines, " lines in ", filename$
CLOSE FILE fh
```


{{out}}

```txt
prompt$ ./readlines
10 lines in readlines.bac
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Filename: ":NAME$
110 OPEN #1:NAME$ ACCESS INPUT
120 COPY FROM #1 TO #0
130 CLOSE #1
```


=
## Locomotive Basic
=


```locobasic
10 OPENIN"foo.txt"
20 WHILE NOT EOF
30 LINE INPUT#9,i$
40 PRINT i$
50 WEND
```


=
## ZX Spectrum Basic
=
The tape recorder interface does not support fragmented reads, because tape recorder start and stop is not atomic, (and a leadin is required for tape input).
However, the microdrive does support fragmented reads.
In the following example, we read a file line by line from a file on microdrive 1.


```basic
10 REM open my file for input
20 OPEN #4;"m";1;"MYFILE": REM stream 4 is the first available for general purpose
30 INPUT #4; LINE a$: REM a$ will hold our line from the file
40 REM because we do not know how many lines are in the file, we need an error trap
50 REM to gracefully exit when the file is read. (omitted from this example)
60 REM to prevent an error at end of file, place a handler here
100 GOTO 30
```



## Batch File

This takes account on the blank lines, because FOR ignores blank lines when reading a file.

```dos
@echo off
rem delayed expansion must be disabled before the FOR command.
setlocal disabledelayedexpansion
for /f "tokens=1* delims=]" %%A in ('type "File.txt"^|find /v /n ""') do (
	set var=%%B
	setlocal enabledelayedexpansion
		echo(!var!
	endlocal
)
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This method is appropriate if the lines are terminated by a single CR or LF:

```bbcbasic
      file% = OPENIN("*.txt")
      IF file%=0 ERROR 100, "File could not be opened"
      WHILE NOT EOF#file%
        a$ = GET$#file%
      ENDWHILE
      CLOSE #file%
```

This method is appropriate if the lines are terminated by a CRLF pair:

```bbcbasic
      file% = OPENIN("*.txt")
      IF file%=0 ERROR 100, "File could not be opened"
      WHILE NOT EOF#file%
        INPUT #file%, a$
        IF ASCa$=10 a$ = MID$(a$,2)
      ENDWHILE
      CLOSE #file%
```



## Bracmat

<code>fil</code> is a relatively low level Bracmat function for manipulating files. Depending on the parameters it opens, closes, reads, writes a file or reads or sets the file position.

```bracmat
  fil$("test.txt",r)    { r opens a text file, rb opens a binary file for reading }
& fil$(,STR,\n)         { first argument empty: same as before (i.e. "test.txt") }
                        { if \n were replaced by e.g. "\n\t " we would read word-wise instead }
& 0:?lineno
&   whl
  ' ( fil$:(?line.?sep) { "sep" contains found stop character, i.e. \n }
    & put$(line (1+!lineno:?lineno) ":" !line \n)
    )
& (fil$(,SET,-1)|);     { Setting file position before start closes file, and fails.
                          Therefore the | }
```



## Brat


```brat
include :file

file.each_line "foobar.txt" { line |
  p line
}
```



## C


### with fgets


```c
/*
 * Read (and write) the standard input file
 * linie-by-line. This version is for ASCII
 * encoded text files.
 */
#include <stdio.h>

/*
 * BUFSIZE is a max size of line plus 1.
 *
 * It would be nice to dynamically allocate  bigger buffer for longer lines etc.
 * - but this example is as simple as possible. Dynamic buffer allocation from
 * the heap may not be a good idea as it seems, because it can cause memory
 * segmentation in embeded systems.
 */
#define BUFSIZE 1024

int main(void)
{
    static char buffer[BUFSIZE];

    /*
     * Never use gets() instead fgets(), because gets()
     * is a really unsafe function.
     */
    while (fgets(buffer, BUFSIZE, stdin))
        puts(buffer);

    return 0;
}
```



### with getline


```C
// From manpage for "getline"

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	FILE *stream;
	char *line = NULL;
	size_t len = 0;
	ssize_t read;

	stream = fopen("file.txt", "r");
	if (stream == NULL)
		exit(EXIT_FAILURE);

	while ((read = getline(&line, &len, stream)) != -1) {
		printf("Retrieved line of length %u :\n", read);
		printf("%s", line);
	}

	free(line);
	fclose(stream);
	exit(EXIT_SUCCESS);
}
```


=== Using mmap() ===
Implementation using mmap syscall.  Works on Linux 2.6.* and on *BSDs.  Line reading routine takes a callback function, each line is passed into callback as begin and end pointer.  Let OS handle your memory pages, we don't need no stinking mallocs.

```c
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <err.h>

int read_lines(const char * fname, int (*call_back)(const char*, const char*))
{
        int fd = open(fname, O_RDONLY);
        struct stat fs;
        char *buf, *buf_end;
        char *begin, *end, c;

        if (fd == -1) {
                err(1, "open: %s", fname);
                return 0;
        }

        if (fstat(fd, &fs) == -1) {
                err(1, "stat: %s", fname);
                return 0;
        }

        /* fs.st_size could have been 0 actually */
        buf = mmap(0, fs.st_size, PROT_READ, MAP_SHARED, fd, 0);
        if (buf == (void*) -1) {
                err(1, "mmap: %s", fname);
                close(fd);
                return 0;
        }

        buf_end = buf + fs.st_size;

        begin = end = buf;
        while (1) {
                if (! (*end == '\r' || *end == '\n')) {
                        if (++end < buf_end) continue;
                } else if (1 + end < buf_end) {
                        /* see if we got "\r\n" or "\n\r" here */
                        c = *(1 + end);
                        if ( (c == '\r' || c == '\n') && c != *end)
                                ++end;
                }

                /* call the call back and check error indication. Announce
                   error here, because we didn't tell call_back the file name */
                if (! call_back(begin, end)) {
                        err(1, "[callback] %s", fname);
                        break;
                }

                if ((begin = ++end) >= buf_end)
                        break;
        }

        munmap(buf, fs.st_size);
        close(fd);
        return 1;
}

int print_line(const char* begin, const char* end)
{
        if (write(fileno(stdout), begin, end - begin + 1) == -1) {
                return 0;
        }
        return 1;
}

int main()
{
        return read_lines("test.ps", print_line) ? 0 : 1;
}

```



## C++

{{works with|C++03 to C++17 }}

```cpp
#include <fstream>
#include <string>
#include <iostream>

int main( int argc , char** argv ) {
   int linecount = 0 ;
   std::string line  ;
   std::ifstream infile( argv[ 1 ] ) ; // input file stream
   if ( infile ) {
      while ( getline( infile , line ) ) {
	 std::cout << linecount << ": "
                   << line      << '\n' ;  //supposing '\n' to be line end
	 linecount++ ;
      }
   }
   infile.close( ) ;
   return 0 ;
}
```



### =using std::getline=


{{works with|C++| 11+ }}


```txt

"thefile.txt"
888 4432
100  -25
doggie

```



```cpp

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

int main()
{
  std::ifstream infile("thefile.txt");
  std::string line;
  while (std::getline(infile, line) )
     {
        std::istringstream iss(line);
        int a, b;
        if (!(iss >> a >> b)) { break; } // if no error a and b get values from file

        std::cout << "a:\t" << a <<"\n";
        std::cout << "b:\t" << b <<"\n";
     }
      std::cout << "finished" << std::endl;
}
```


{{out}}

```txt

a: 888
b: 4432
a: 100
b: -25
finished


```



{{libheader|U++}}


```cpp
#include <Core/Core.h>

using namespace Upp;

CONSOLE_APP_MAIN
{
	FileIn in(CommandLine()[0]);
	while(in && !in.IsEof())
		Cout().PutLine(in.GetLine());
}
```



## C#

'File.ReadLines' reads the lines of a file which could easily be stepped through.

```c#
foreach (string readLine in File.ReadLines("FileName"))
  DoSomething(readLine);
```

A full code may look like;

```c#
using System;
using System.IO;
using System.Text;

namespace RosettaCode
{
  internal class Program
  {
    private static void Main()
    {
      var sb = new StringBuilder();
      string F = "File.txt";

      // Read a file, line by line.
      try
      {
        foreach (string readLine in File.ReadLines(F))
        {
          // Use the data in some way...
          sb.Append(readLine);
          sb.Append("\n");
        }
      }
      catch (Exception exception)
      {
        Console.WriteLine(exception.Message);
        Environment.Exit(1);
      }

      // Preset the results
      Console.WriteLine(sb.ToString());
    }
  }
}
```



## Clojure



```Clojure

(with-open [r (clojure.java.io/reader "some-file.txt")]
   (doseq [l (line-seq r)]
     (println l)))

```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. read-file-line-by-line.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT input-file ASSIGN TO "input.txt"
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS input-file-status.

       DATA DIVISION.
       FILE SECTION.
       FD  input-file.
       01  input-record PIC X(256).

       WORKING-STORAGE SECTION.
       01  input-file-status PIC 99.
           88  file-is-ok    VALUE 0.
           88  end-of-file   VALUE 10.

       01  line-count        PIC 9(6).

       PROCEDURE DIVISION.
           OPEN INPUT input-file
           IF NOT file-is-ok
               DISPLAY "The file could not be opened."
               GOBACK
           END-IF

           PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
               READ input-file
               DISPLAY line-count ": " FUNCTION TRIM(input-record)
           END-PERFORM

           CLOSE input-file

           GOBACK
           .
```



## CoffeeScript

{{works_with|node.js}}

```coffeescript

# This module shows two ways to read a file line-by-line in node.js.
fs = require 'fs'

# First, let's keep things simple, and do things synchronously.  This
# approach is well-suited for simple scripts.
do ->
  fn = "read_file.coffee"
  for line in fs.readFileSync(fn).toString().split '\n'
    console.log line
  console.log "DONE SYNC!"

# Now let's complicate things.
#
# Use the following code when files are large, and memory is
# constrained and/or where you want a large amount of concurrency.
#
# Protocol:
#   Call LineByLineReader, which calls back to you with a reader.
#   The reader has two methods.
#      next_line: call to this when you want a new line
#      close: call this when you are done using the file before
#         it has been read completely
#
#   When you call next_line, you must supply two callbacks:
#     line_cb: called back when there is a line of text
#     done_cb: called back when there is no more text in the file
LineByLineReader = (fn, cb) ->
  fs.open fn, 'r', (err, fd) ->
    bufsize = 256
    pos = 0
    text = ''
    eof = false
    closed = false
    reader =
      next_line: (line_cb, done_cb) ->
        if eof
          if text
            last_line = text
            text = ''
            line_cb last_line
          else
            done_cb()
          return

        new_line_index = text.indexOf '\n'
        if new_line_index >= 0
          line = text.substr 0, new_line_index
          text = text.substr new_line_index + 1, text.length - new_line_index - 1
          line_cb line
        else
          frag = new Buffer(bufsize)
          fs.read fd, frag, 0, bufsize, pos, (err, bytesRead) ->
            s = frag.toString('utf8', 0, bytesRead)
            text += s
            pos += bytesRead
            if (bytesRead)
              reader.next_line line_cb, done_cb
            else
              eof = true
              fs.closeSync(fd)
              closed = true
              reader.next_line line_cb, done_cb
        close: ->
          # The reader should call this if they abandon mid-file.
          fs.closeSync(fd) unless closed

    cb reader

# Test our interface here.
do ->
  console.log '---'
  fn = 'read_file.coffee'
  LineByLineReader fn, (reader) ->
    callbacks =
      process_line: (line) ->
         console.log line
         reader.next_line callbacks.process_line, callbacks.all_done
      all_done: ->
        console.log "DONE ASYNC!"
    reader.next_line callbacks.process_line, callbacks.all_done

```



## Common Lisp


```lisp
(with-open-file (input "file.txt")
   (loop for line = (read-line input nil)
      while line do (format t "~a~%" line)))
```



## D


```d
void main() {
    import std.stdio;

    foreach (line; "read_a_file_line_by_line.d".File.byLine)
        line.writeln;
}
```

The File is managed by reference count, and it gets closed when it gets out of scope or it changes. The 'line' is a char[] (with newline), so if you need a string you have to idup it.

## DCL


```DCL
$ open input input.txt
$ loop:
$  read /end_of_file = done input line
$  goto loop
$ done:
$ close input
```


## Delphi


```Delphi

   procedure ReadFileByLine;
   var
      TextFile: text;
      TextLine: String;
   begin
      Assign(TextFile, 'c:\test.txt');
      Reset(TextFile);
      while not Eof(TextFile) do
         Readln(TextFile, TextLine);
      CloseFile(TextFile);
   end;

```

The example file (above) '''"c:\test.txt"''' is assigned to the text file variable '''"TextFile"''' is opened and any line is read in a loop into the string variable '''"TextLine"'''.


```Delphi

procedure ReadFileByLine;
   var
      TextLines :  TStringList;
      i         :  Integer;
   begin
      TextLines := TStringList.Create;
      TextLines.LoadFromFile('c:\text.txt');
      for i := 0 to TextLines.count -1 do
      ShowMessage(TextLines[i]);
   end;

```


Above uses the powerful utility classs type [http://www.delphibasics.co.uk/RTL.asp?Name=TStringList TStringList]  from Classes Unit

See also GNU LGPL (Delphi replacement) [http://www.lazarus.freepascal.org/ Lazarus IDE FreePascal] and specifically [http://wiki.lazarus.freepascal.org/TString_List-TString_Tutorial Lazarus FreePascal Equivalent for TStringList]

## Elena

ELENA 4.x :

```elena
import system'io;
import extensions;
import extensions'routines;

public program()
{
    File.assign:"file.txt".forEachLine(printingLn)
}
```



## Elixir

Two Slightly different solutions in the FileReader namespace

```Elixir

  defmodule FileReader do
    # Create a File.Stream and inspect each line
    def by_line(path) do
      File.stream!(path)
        |> Stream.map(&(IO.inspect(&1)))
	|> Stream.run
      end

    def bin_line(path) do
    # Build the stream in binary instead for performance increase
      case File.open(path) do
        # File returns a tuple, {:ok,file}, if successful
        {:ok, file} ->
	  IO.binstream(file, :line)
	    |> Stream.map(&(IO.inspect(&1)))
	    |> Stream.run
	# And returns {:error,reason} if unsuccessful
	{:error,reason} ->
	# Use Erlang's format_error to return an error string
	  :file.format_error(reason)
      end
    end
  end

```



## Erlang

read_a_file_line_by_line:into_list/1 is used by [[Read_a_specific_line_from_a_file]]. If this task is updated keep backwards compatibility, or change [[Read_a_specific_line_from_a_file]], too.

```erlang

-module( read_a_file_line_by_line ).

-export( [into_list/1] ).

into_list( File ) ->
        {ok, IO} = file:open( File, [read] ),
        into_list( io:get_line(IO, ''), IO, [] ).


into_list( eof, _IO, Acc ) -> lists:reverse( Acc );
into_list( {error, _Error}, _IO, Acc ) -> lists:reverse( Acc );
into_list( Line, IO, Acc ) -> into_list( io:get_line(IO, ''), IO, [Line | Acc] ).

```


{{out}}

```txt

6> read_a_file_line_by_line:into_list("read_a_file_line_by_line.erl").
["-module( read_a_file_line_by_line ).\n","\n",
 "-export( [into_list/1] ).\n","\n","into_list( File ) ->\n",
 "\t{ok, IO} = file:open( File, [read] ),\n",
 "\tinto_list( io:get_line(IO, ''), IO, [] ).\n","\n","\n",
 "into_list( eof, _IO, Acc ) -> lists:reverse( Acc );\n",
 "into_list( {error, _Error}, _IO, Acc ) -> lists:reverse( Acc );\n",
 "into_list( Line, IO, Acc ) -> into_list( io:get_line(IO, ''), IO, [Line | Acc] ).\n"]

```



## ERRE


```ERRE

PROGRAM LETTURA

EXCEPTION
    FERROR%=TRUE        ! si e' verificata l'eccezione !
    PRINT("Il file richiesto non esiste .....")
END EXCEPTION

BEGIN
    FERROR%=FALSE
    PRINT("Nome del file";)
    INPUT(FILE$)      ! chiede il nome del file
    OPEN("I",1,FILE$) ! apre un file sequenziale in lettura
      IF NOT FERROR% THEN
         REPEAT
           INPUT(LINE,#1,CH$)   ! legge una riga ....
           PRINT(CH$)           ! ... la stampa ...
         UNTIL EOF(1)           ! ... fine a fine file
      END IF
      PRINT
    CLOSE(1)          ! chiude il file
END PROGRAM

```

From ERRE manual: use an EXCEPTION to trap a "file not found" error. If you change INPUT(LINE statement with a GET you can read the file one character at time.


## Euphoria


```euphoria
constant cmd = command_line()
constant filename = cmd[2]
constant fn = open(filename,"r")
integer i
i = 1
object x
while 1 do
    x = gets(fn)
    if atom(x) then
        exit
    end if
    printf(1,"%2d: %s",{i,x})
    i += 1
end while
close(fn)
```


{{out}}

```txt
 1: constant cmd = command_line()
 2: constant filename = cmd[2]
 3: constant fn = open(filename,"r")
 4: integer i
 5: i = 1
 6: object x
 7: while 1 do
 8:     x = gets(fn)
 9:     if atom(x) then
10:         exit
11:     end if
12:     printf(1,"%2d: %s",{i,x})
13:     i += 1
14: end while
15: close(fn)

```



=={{header|F_Sharp|F#}}==
Using DotNet's [http://msdn.microsoft.com/en-us/library/dd383503.aspx System.IO.File.ReadLines] iterator:

```fsharp
open System.IO

[<EntryPoint>]
let main argv =
    File.ReadLines(argv.[0]) |> Seq.iter (printfn "%s")
    0
```



## Factor



```factor
 "path/to/file" utf8 [ [ readln dup [ print ] when* ] loop ] with-file-reader
```



## Fantom


Reads each line from the file "data.txt".


```fantom

class Main
{
  Void main ()
  {
    File (`data.txt`).eachLine |Str line|
    {
      echo ("Line: $line")
    }
  }
}

```



## Forth



```forth
4096 constant max-line

: third ( A b c -- A b c A )
  >r over r> swap ;

: read-lines ( fileid -- )
  begin  pad max-line third read-line throw
  while  pad swap  ( fileid c-addr u )  \ string excludes the newline
         2drop
  repeat 2drop ;
```



## Fortran


### Old Fortran

Usually, one reads a file with some idea what is in the file and some purpose behind reading it. For this task, suppose that the file just contains plain text, and the text is to be listed, line by line, to the end of the file. The remaining question is how long is the longest record? Some systems enable the reading of a record into a variable that is made big enough to hold whatever the record contains, though perhaps only up to some integer limit such as 65535. Fortran 2000 has formalised the provision of character variables whose size is determined when assigned to (as in <code>TEXT = "This"//"That"</code> where character variable TEXT is reallocated memory so as to hold eight characters, as needed for the assignment) but without a F2000 compiler to test, it is not clear that this arrangement will work for READ statements as well.

So one is faced again with the question "How long is a piece of string?" when choosing a predefined size. I have confronted a singularly witless format for supplying electricity data that would write up to an entire year's worth of half-hourly values to one line though it might be used to write just a few day's worth of data also. The header line specified the date and time slot for each column as <code>Country,Island,Node,MEAN Energy,01AUG2010 Daily ENERGY,01AUG2010 01,01AUG2010 02,01AUG2010 03, ''etc.''</code> so all-in-all it was less trouble to specify CHARACTER*246810 for the input record scratchpad so as not to have to struggle with piecemeal input. In this example, change the value of ENUFF.

A common extension after F77 was the "Q" format, which returns the number of characters yet to be read in the input record. In its absence, one would have to just read the input with A format, and if the input record was shorter than ENUFF, then trailing spaces would be appended to ALINE and if ALINE was capacious then this would waste time. Similarly, for output, trailing spaces should be trimmed off, which means that if the input record contained trailing spaces, they would be lost. The scheme here, available via F90 is to use the Q format feature to determine how long the record is, then, request only that many characters to be placed in ALINE, and, write that many characters to the output which will thereby include any supplied trailing spaces. However, there must of course be no attempt to cram any more than ENUFF characters into ALINE, thus the MIN(L,ENUFF) in the READ statement, where the calculation is done on-the-fly. As well, should L be greater than ENUFF this is worth some remark, and in a way that cannot be confused with a listing line, each of which is prefixed by the record number. The default integer size is 32 bit so the numbers could be large but to avoid annoying blank space in the message, I0 format is used. Earlier Fortrans do not allow this, so one might specify I9.

On the other hand, the output device might be less than accommodating when presented with a line longer than it can print: lineprinters typically printed 120, 132 or maybe 144 characters to a line with anything beyond that ignored if it were not a cause for immediate termination. Thus, the WRITE statement could be augmented with <code>ERR = ''label'', END = ''label''</code> in hope of recovery attempts. If output were to a disc file, the END might be activated on running out of disc space but with windows the system would probably have crashed already. Given a long line to print a teletype printer would just hammer away at the last printing position, but more refined printers would start new lines as needed. I have used a dot-matrix printer that with lineprinter paper could be set to print some 360 cramped characters to a line, and have seen photographs of a special accountant's typewriter with a platen about four feet long. Then for spreadsheet users, there arrived a special printing prog, SIDEWAYS.

Peripheral to the task of reading a file line-by-line is the blather about specifying the file name and opening it. The OPEN statement allows for jumping to an ERR label (just as the READ statement has a jump for end-of-file), and carrying an IOSTAT value to specify the nature of the problem (invalid file name form, file access denied, etc.) but this is all very messy and the error codes are not the same across different systems either. I wish these statements were more like functions and returned TRUE/FALSE or a result code that could be tested in an IF-statement directly, as for example in Burroughs Algol where one could write something like <code>While Read(in) Stuff Do ... ;</code> - though a READ statement returned ''true'' for an I/O error, and ''false'' for success, so one defined ''Ican'' to be ''not'' and wrote <code>While Ican Read(in) Stuff Do ... ;</code>

In the absence of such error reception, ugly messages are presented as the prog. is cancelled, and the most common such error is to name a missing file. So, an INQUIRE statement to check first. This too should have an ERR and IOSTAT blather (the file name might be malformed) but enough is enough. The assignment direction for such codes as EXIST and IOSTAT is ''left'' to right rather than the usual right to left (as in FILE = FNAME), but rather than remember this, it is easiest to take advantage of Fortran's (complete) absence of reserved words and define a logical variable EXIST so that the statement is EXIST = EXIST, and the compiler and the programmer can go their own ways.

```Fortran

      INTEGER ENUFF		!A value has to be specified beforehand,.
      PARAMETER (ENUFF = 2468)	!Provide some provenance.
      CHARACTER*(ENUFF) ALINE	!A perfect size?
      CHARACTER*66 FNAME	!What about file name sizes?
      INTEGER LINPR,IN		!I/O unit numbers.
      INTEGER L,N		!A length, and a record counter.
      LOGICAL EXIST		!This can't be asked for in an "OPEN" statement.
      LINPR = 6			!Standard output via this unit number.
      IN = 10			!Some unit number for the input file.
      FNAME = "Read.for"	!Choose a file name.
      INQUIRE (FILE = FNAME, EXIST = EXIST)	!A basic question.
      IF (.NOT.EXIST) THEN		!Absent?
        WRITE (LINPR,1) FNAME		!Alas, name the absentee.
    1   FORMAT ("No sign of file ",A)	!The name might be mistyped.
        STOP "No file, no go."		!Give up.
      END IF				!So much for the most obvious mishap.
      OPEN (IN,FILE = FNAME, STATUS = "OLD", ACTION = "READ")	!For formatted input.

      N = 0		!No records read so far.
   10 READ (IN,11,END = 20) L,ALINE(1:MIN(L,ENUFF))	!Read only the L characters in the record, up to ENUFF.
   11 FORMAT (Q,A)		!Q = "how many characters yet to be read", A = characters with no limit.
      N = N + 1			!A record has been read.
      IF (L.GT.ENUFF) WRITE (LINPR,12) N,L,ENUFF	!Was it longer than ALINE could accommodate?
   12 FORMAT ("Record ",I0," has length ",I0,": my limit is ",I0)	!Yes. Explain.
      WRITE (LINPR,13) N,ALINE(1:MIN(L,ENUFF))	!Print the record, prefixed by the count.
   13 FORMAT (I9,":",A)		!Fixed number size for alignment.
      GO TO 10			!Do it again.

   20 CLOSE (IN)	!All done.
      END	!That's all.

```

With F90 and later it is possible to use an ALLOCATE statement to prepare a variable of a size determined at run time, so that one could for each record use the <code>Q</code> format code (or a new feature of the READ statement) to ascertain the size of the record about to be read, free the storage for the old ALINE and allocate a new sized ALINE, then read that record into ALINE. This avoids worrying about the record overflowing (or underflowing) ALINE, at the cost of hammering at the memory allocation process.

An alternative approach would be to read the file as UNFORMATTED, just reading binary into some convenient scratchpad and then write the content to the output device, which would make what it could of the ASCII world's dithering between CR, CRLF, LFCR and CR as end-of-record markers. However, this would not be reading the file line-by-line.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open "input.txt" For Input As #1
Dim line_ As String
While Not Eof(1)
  Line Input #1, line_  '' read each line
  Print line_           '' echo it to the console
Wend
Close #1
Print
Print "Press any key to quit"
Sleep
```



## Frink

The lines function can also take an optional second string argument indicating the encoding of the file, and can read from any supported URL type (HTTP, FTP, etc.)

```frink

for line = lines["file:yourfile.txt"]
   println[line]

```



## Gambas


```gambas
Public Sub Main()
Dim hFile As File
Dim sLine As String

hFile = Open "../InputText.txt" For Input

While Not Eof(hFile)
  Line Input #hFile, sLine
  Print sLine
Wend

End
```



## GAP


```gap
ReadByLines := function(name)
	local file, line, count;
	file := InputTextFile(name);
	count := 0;
	while true do
		line := ReadLine(file);
		if line = fail then
			break;
		fi;
		count := count + 1;
	od;
	CloseStream(file);
	return count;
end;

# With [http://www.ibiblio.org/pub/docs/misc/amnesty.txt amnesty.txt]
ReadByLines("amnesty.txt");
# 384
```



## Genie


```genie
[indent=4]
/*
   Read file line by line, in Genie

   valac readFileLines.gs
   ./readFileLines [filename]
*/

init

    fileName:string
    fileName = (args[1] is null) ? "readFileLines.gs" : args[1]
    var file = FileStream.open(fileName, "r")
    if file is null
        stdout.printf("Error: %s did not open\n", fileName)
        return

    lines:int = 0
    line:string? = file.read_line()
    while line is not null
        lines++
        stdout.printf("%04d %s\n", lines, line)
        line = file.read_line()
```


{{out}}

```txt
prompt$ valac readFileLines.gs
prompt$ ./readFileLines nofile
Error: nofile did not open
prompt$ ./readFileLines hello.gs
0001 [indent=4]
0002
0003 init
0004     print "Hello, Genie"
```



## Go

;bufio.Scanner
The bufio package provides Scanner, a convenient interface for reading data such as a file of newline-delimited lines of text. Successive calls to the Scan method will step through the 'tokens' of a file, skipping the bytes between the tokens. The specification of a token is defined by a split function of type SplitFunc; the default split function breaks the input into lines with line termination stripped. Split functions are defined in this package for scanning a file into lines, bytes, UTF-8-encoded runes, and space-delimited words. The client may instead provide a custom split function.

Scanning stops unrecoverably at EOF, the first I/O error, or a token too large to fit in the buffer. When a scan stops, the reader may have advanced arbitrarily far past the last token. Programs that need more control over error handling or large tokens, or must run sequential scans on a reader, should use bufio.Reader instead.


```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func init() {
	log.SetFlags(log.Lshortfile)
}

func main() {
	// Open an input file, exit on error.
	inputFile, err := os.Open("byline.go")
	if err != nil {
		log.Fatal("Error opening input file:", err)
	}

	// Closes the file when we leave the scope of the current function,
	// this makes sure we never forget to close the file if the
	// function can exit in multiple places.
	defer inputFile.Close()

	scanner := bufio.NewScanner(inputFile)

	// scanner.Scan() advances to the next token returning false if an error was encountered
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	// When finished scanning if any error other than io.EOF occured
	// it will be returned by scanner.Err().
	if err := scanner.Err(); err != nil {
		log.Fatal(scanner.Err())
	}
}

```


;ReadLine
This function allows files to be rapidly scanned for desired data while minimizing memory allocations.  It also handles /r/n line endings and allows unreasonably long lines to be handled as error conditions.

```go
package main

import (
    "bufio"
    "fmt"
    "io"
    "log"
    "os"
)

func main() {
    f, err := os.Open("file") // os.OpenFile has more options if you need them
    if err != nil {           // error checking is good practice
        // error *handling* is good practice.  log.Fatal sends the error
        // message to stderr and exits with a non-zero code.
        log.Fatal(err)
    }

    // os.File has no special buffering, it makes straight operating system
    // requests.  bufio.Reader does buffering and has several useful methods.
    bf := bufio.NewReader(f)

    // there are a few possible loop termination
    // conditions, so just start with an infinite loop.
    for {
        // reader.ReadLine does a buffered read up to a line terminator,
        // handles either /n or /r/n, and returns just the line without
        // the /r or /r/n.
        line, isPrefix, err := bf.ReadLine()

        // loop termination condition 1:  EOF.
        // this is the normal loop termination condition.
        if err == io.EOF {
            break
        }

        // loop termination condition 2: some other error.
        // Errors happen, so check for them and do something with them.
        if err != nil {
            log.Fatal(err)
        }

        // loop termination condition 3: line too long to fit in buffer
        // without multiple reads.  Bufio's default buffer size is 4K.
        // Chances are if you haven't seen a line terminator after 4k
        // you're either reading the wrong file or the file is corrupt.
        if isPrefix {
            log.Fatal("Error: Unexpected long line reading", f.Name())
        }

        // success.  The variable line is now a byte slice based on on
        // bufio's underlying buffer.  This is the minimal churn necessary
        // to let you look at it, but note! the data may be overwritten or
        // otherwise invalidated on the next read.  Look at it and decide
        // if you want to keep it.  If so, copy it or copy the portions
        // you want before iterating in this loop.  Also note, it is a byte
        // slice.  Often you will want to work on the data as a string,
        // and the string type conversion (shown here) allocates a copy of
        // the data.  It would be safe to send, store, reference, or otherwise
        // hold on to this string, then continue iterating in this loop.
        fmt.Println(string(line))
    }
}
```

;ReadString
In comparison, ReadString is a little quick and dirty, but is often good enough.

```go
package main

import (
    "bufio"
    "fmt"
    "io"
    "log"
    "os"
)

func main() {
    f, err := os.Open("file")
    if err != nil {
        log.Fatal(err)
    }
    bf := bufio.NewReader(f)
    for {
        switch line, err := bf.ReadString('\n'); err {
        case nil:
            // valid line, echo it.  note that line contains trailing \n.
            fmt.Print(line)
        case io.EOF:
            if line > "" {
                // last line of file missing \n, but still valid
                fmt.Println(line)
            }
            return
        default:
            log.Fatal(err)
        }
    }
}
```



## Groovy


```groovy
new File("Test.txt").eachLine { line, lineNumber ->
    println "processing line $lineNumber: $line"
}
```



== {{header|Haskell}}==
Thanks to laziness, there's no difference between reading the file all at once and reading it line by line.


```Haskell
main = do
  file <- readFile "linebyline.hs"
  mapM_ putStrLn (lines file)

```


== {{header|Icon}} and {{header|Unicon}} ==
Line oriented I/O is basic.  This program reads lines from "input.txt" into the variable line, but does nothing with it.


```Icon
procedure main()
f := open("input.txt","r") | stop("cannot open file ",fn)
while line := read(f)
close(f)
end
```



## J


J currently discourages this "read just one line" approach.  In addition to the arbitrary character of lines, there are issues of problem size and scope (what happens when you have a billion characters between your newline delimiters?).  Usually, it's easier to just read the entire file, or memory map the file, and when files are so large that that is not practical it's probably better to put the programmer in explicit control of issues like block sizes and exception handling.

This implementation looks for lines separated by ascii character 10.  Lines returned here do not include the line separator character.  Files with no line-separating character at the end are treated as well formed -- if the last character of the file is the line separator that means that you have an empty line at the end of the file.

This implementation does nothing special when dealing with multi-gigabyte lines.  If you encounter an excessively large line and if do not have enough physical memory, your system will experience heavy memory pressure.  If you also do not have enough virtual memory to hold a line you will get an out of memory exception.


```j
cocurrent 'linereader'

  NB. configuration parameter
  blocksize=: 400000

  NB. implementation
  offset=: 0
  position=: 0
  buffer=: ''
  lines=: ''

  create=: monad define
    name=: boxxopen y
    size=: 1!:4 name
    blocks=: 2 <@(-~/\)\ ~. size <. blocksize * i. 1 + >. size % blocksize
  )

  readblocks=: monad define
     if. 0=#blocks do. return. end.
     if. 1<#lines do. return. end.
     whilst. -.LF e.chars do.
       buffer=: buffer,chars=. 1!:11 name,{.blocks
       blocks=: }.blocks
       lines=: <;._2 buffer,LF
     end.
     buffer=: _1{::lines
  )

  next=: monad define
    if. (#blocks)*.2>#lines do. readblocks'' end.
    r=. 0{::lines
    lines=: }.lines
    r
  )
```



```j
   example=: '/tmp/example.txt' conew 'linereader'
   next__example''
this is line 1
   next__example''
and this is line 2
```



## Java


```java
import java.io.BufferedReader;
import java.io.FileReader;

/**
 * Reads a file line by line, processing each line.
 *
 * @author  $Author$
 * @version $Revision$
 */
public class ReadFileByLines {
    private static void processLine(int lineNo, String line) {
        // ...
    }

    public static void main(String[] args) {
        for (String filename : args) {
            BufferedReader br = null;
            FileReader fr = null;
            try {
                fr = new FileReader(filename);
                br = new BufferedReader(fr);
                String line;
                int lineNo = 0;
                while ((line = br.readLine()) != null) {
                    processLine(++lineNo, line);
                }
            }
            catch (Exception x) {
                x.printStackTrace();
            }
            finally {
                if (fr != null) {
                    try {br.close();} catch (Exception ignoreMe) {}
                    try {fr.close();} catch (Exception ignoreMe) {}
                }
            }
        }
    }
}
```

{{works with|Java|7+}}
In Java 7, the try with resources block handles multiple readers and writers without nested try blocks. The loop in the main method would look like this:

```java5
for (String filename : args) {
    try (FileReader fr = new FileReader(filename);BufferedReader br = new BufferedReader(fr)){
        String line;
        int lineNo = 0;
        while ((line = br.readLine()) != null) {
            processLine(++lineNo, line);
        }
    }
    catch (Exception x) {
        x.printStackTrace();
    }
}
```

<code>fr</code> and <code>br</code> are automatically closed when the program exits the try block (it also checks for nulls before closing and throws closing exceptions out of the block).

A more under-the-hood method in Java 7 would be to use the <code>Files</code> class (line numbers can be inferred from indices in the returned <code>List</code>):

```java5
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.Charset;
import java.io.IOException;
//...other class code
List<String> lines = null;
try{
    lines = Files.readAllLines(Paths.get(filename), Charset.defaultCharset());
}catch(IOException | SecurityException e){
    //problem with the file
}
```



## JavaScript


```javascript
var fs = require("fs");

var readFile = function(path) {
    return fs.readFileSync(path).toString();
};

console.log(readFile('file.txt'));
```



## jq

When invoked with the -R option, jq will read each line as a JSON string.  For example:

```sh
$ seq 0 5 | jq -R 'tonumber|sin'
0
0.8414709848078965
0.9092974268256817
0.1411200080598672
-0.7568024953079282
-0.9589242746631385
```


To perform any kind of reduction operation while reading the lines one-by-one, one would normally use
`input` or `inputs`.  For example, to compute the maximum of the above sin values:


```sh
$ seq 0 5 | jq -Rn '[inputs | tonumber |sin] | max'
0.9092974268256817
```



## Jsish


```javascript
/* Read by line, in Jsish */
var f = new Channel('read-by-line.jsi');
var line;

while (line = f.gets()) puts(line);
f.close();
```

{{out}}

```txt
prompt$ jsish read-by-line.jsi
/* Read by line, in Jsish */
var f = new Channel('read-by-line.jsi');
var line;

while (line = f.gets()) puts(line);
f.close();
```



## Julia


```julia
open("input_file","r") do f
    for line in eachline(f)
      println("read line: ", line)
    end
end
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    File("input.txt").forEachLine { println(it) }
}
```



## Lasso


```Lasso
local(f) = file('foo.txt')
handle => {#f->close}
#f->forEachLine => {^
    #1
    '
' // note this simply inserts an HTML line break between each line.
^}
```



## Liberty BASIC


```lb
filedialog "Open","*.txt",file$
if file$="" then end
open file$ for input as #f
while not(eof(#f))
    line input #f, t$
    print t$
wend
close #f
```

Mac

```lb
filedialog "Open","*.txt",file$
if file$="" then end
open file$ for input as #f
while not(eof(#f))
    t$ = inputto$(#f, chr$(13))
    print t$
wend
close #f
```

Unix

```lb
filedialog "Open","*.txt",file$
if file$="" then end
open file$ for input as #f
while not(eof(#f))
    t$ = inputto$(#f, chr$(10))
    print t$
wend
close #f
```



## Lingo

The following code works fine for text files using 'CRLF' or 'CR only' as line end characters, but unfortunately not for the *nix default 'LF only' (note: Lingo's implementation Director does not run on Linux. It was originally created for ancient Mac OS systems, and later also ported to Windows):

```lingo
fp = xtra("fileIO").new()
fp.openFile(_movie.path & "input.txt", 1)
fileSize = fp.getLength()
repeat while TRUE
  str = fp.readLine()
  if str.char[1] = numtochar(10) then delete char 1 of str
  if the last char of str = numtochar(13) then delete the last char of str
  put str
  if fp.getPosition()>=fileSize then exit repeat
end repeat
fp.closeFile()
```



## LiveCode


```LiveCode
command readFileLineByLine
    local tFile, tLines, startRead
    put "/usr/share/dict/words" into tFile
    open file tFile for text read
    put true into startRead
    repeat until it is empty and startRead is false
        put false into startRead
        read from file tFile for 1 line
        add 1 to tLines
    end repeat
    close file tFile
    put tLines
end readFileLineByLine
```



## Logo

There are several words which will return a line of input.
* readline - returns a line as a list of words
* readword - returns a line as a single word, or an empty list if it reached the end of file
* readrawline - returns a line as a single word, with no characters escaped

```logo
while [not eof?] [print readline]
```



## Lua


```lua
filename = "input.txt"
fp = io.open( filename, "r" )

for line in fp:lines() do
    print( line )
end

fp:close()

```


### Simpler version

The following achieves the same result as the example above, including implicitly closing the file at the end of the loop.

```Lua

for line in io.lines("input.txt") do
  print(line)
end

```



## M2000 Interpreter

Utf-16LE (wide) and Ansi (locale selective) for Open statement.
Documents have Load.Doc statement to load text file. Here we see how we make indexes, and then reopen for input, and move to index, and then load a line.


```M2000 Interpreter

Module checkit {
      \\ prepare a file
      document a$
      a$={First Line
            Second line
            Third Line
            }
      Save.Doc a$, "checkthis.txt", 0  ' 0 for UTF-16LE
      Flush
      Open "checkthis.txt" For Wide Input as #F
      While not Eof(#f) {
            Data Seek(#f)
            Line Input #F, b$
            Print  b$
      }
      Close #f
      Dim  Index()
      \\ copy stack to index(), flush stack
      Index()=Array([])
      \\ change base to base 1
      Dim  Base 1, Index(len(index()))
      Open "checkthis.txt" For Wide Input as #F
            Seek#F, Index(2)
            Line Input #F, b$
            Print b$  ' print second line
      Close #f
      \\ prepare Ansi file
      Print "Ansi File"
      Save.Doc a$, "checkthis.txt",  1033  ' we use specific locale
      Flush \\ flush the stack to get indexes
      oldlocale=locale
      locale 1033
      \\ no Wide clause
      Open "checkthis.txt" For  Input as #F
      While not Eof(#f) {
            Data Seek(#f)
            Line Input #F, b$
            Print  b$
      }
      Close #f
      Dim  Index()
      \\ copy stack to index(), flush stack
      Index()=Array([])
      \\ change base to base 1
      Dim  Base 1, Index(len(index()))
      Open "checkthis.txt" For Input as #F
            Seek#F, Index(2)
            Line Input #F, b$
            Print b$  ' print second line
      Close #f
      locale oldlocale
}
checkit

```




## Maple


```Maple
path := "file.txt":
while (true) do
	input := readline(path):
	if input = 0 then break; end if:
	#The line is stored in input
end do:
```



## Mathematica


```Mathematica

strm=OpenRead["input.txt"];
If[strm=!=$Failed,
  While[line=!=EndOfFile,
    line=Read[strm];
    (*Do something*)
  ]];
Close[strm];

```


=={{header|MATLAB}} / {{header|Octave}}==

The function fgetl() read lines from file:


```Matlab

  fid = fopen('foobar.txt','r');
  if (fid < 0)
	printf('Error:could not open file\n')
  else
	while ~feof(fid),
		line = fgetl(fid);
                %% process line %%
	end;
        fclose(fid)
  end;
```



## Maxima


```maxima
/* Read a file and return a list of all lines */

readfile(name) := block(
   [v: [ ], f: openr(name), line],
   while stringp(line: readline(f)) do v: endcons(line, v),
   close(f),
   v
)$
```



## Mercury

Basic version.

```mercury
:- module read_a_file_line_by_line.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, require, string.

main(!IO) :-
    io.open_input("test.txt", OpenResult, !IO),
    (
        OpenResult = ok(File),
        read_file_line_by_line(File, 0, !IO)
    ;
        OpenResult = error(Error),
        error(io.error_message(Error))
    ).

:- pred read_file_line_by_line(io.text_input_stream::in, int::in,
    io::di, io::uo) is det.

read_file_line_by_line(File, !.LineCount, !IO) :-
    % We could also use io.read_line/3 which returns a list of characters
    % instead of a string.
    io.read_line_as_string(File, ReadLineResult, !IO),
    (
        ReadLineResult = ok(Line),
        !:LineCount = !.LineCount + 1,
        io.format("%d: %s", [i(!.LineCount), s(Line)], !IO),
        read_file_line_by_line(File, !.LineCount, !IO)
    ;
        ReadLineResult = eof
    ;
        ReadLineResult = error(Error),
        error(io.error_message(Error))
    ).
```


Version using a stream fold.


```mercury
:- module read_a_file_line_by_line.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, require, string, stream.

main(!IO) :-
    io.open_input("test.txt", OpenResult, !IO),
    (
        OpenResult = ok(File),
        stream.input_stream_fold2_state(File, process_line, 0, Result, !IO),
        (
            Result = ok(_)
        ;
            Result = error(_, Error),
            error(io.error_message(Error))
        )
    ;
        OpenResult = error(Error),
        error(io.error_message(Error))
    ).

:- pred process_line(line::in, int::in, int::out, io::di, io::uo) is det.

process_line(line(Line), !LineCount, !IO) :-
    !:LineCount = !.LineCount + 1,
    io.format("%d: %s", [i(!.LineCount), s(Line)], !IO).
```



## Neko

Need to define a growing buffer to handle streaming unknown sizes, 2 to the 29 max, for this one.


```ActionScript
/**
 Read a file line by line, in Neko
 <doc>
```txt
Tectonics:
   nekoc readfile.neko
   neko readfile [filename]
```
</doc>
*/


var stdin = $loader.loadprim("std@file_stdin", 0)()
var file_open = $loader.loadprim("std@file_open", 2)
var file_read_char = $loader.loadprim("std@file_read_char", 1)

/* Read a line from file f into string s returning length without any newline */
var NEKO_MAX = 1 << 29
var strsize = 256
var NEWLINE = 10
var readline = function(f) {
    var s = $smake(strsize)
    var len = 0
    var ch
    var file_exception = false
    while true {
        try ch = file_read_char(f) catch problem { file_exception = problem; break; }
        if ch == NEWLINE break;
        if $sset(s, len, ch) == null break; else len += 1

        if len == strsize - 1 {
            strsize *= 2
            if strsize > NEKO_MAX $throw("Out of string space for readline")
            var t = s
            s = $smake(strsize)
            $sblit(s, 0, t, 0, $ssize(t))
        }
    }
    if $istrue(file_exception) $rethrow(file_exception)
    return $ssub(s, 0, len)
}

var infile
var cli = $loader.args[0]
if cli == null infile = stdin
else {
    cli = $string(cli)
    try infile = file_open(cli, "r")
    catch problem $print(problem, " Can't open ", cli, "\n")
}
if infile == null $throw("Can't open " + cli)

var str
while true {
    try {
        str = readline(infile)
        $print(":", str, ":\n")
    } catch a break;
}

```


{{out}}

```txt
prompt$ nekoc readfile.neko
prompt$ seq 1 6 | neko readfile.n
:1:
:2:
:3:
:4:
:5:
:6:
prompt$ seq -s',' 1 1000000 | neko readfile | tail -c23
999998,999999,1000000:
prompt$ neko readfile.n readfile.neko | tail -4
:        str = readline(infile):
:        $print(":", str, ":\n"):
:    } catch a break;:
:}:
```



## NetRexx


###  Using Java <tt>Scanner</tt>


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg inFileName .

if inFileName = '' | inFileName = '.' then inFileName = './data/dwarfs.json'
lines = scanFile(inFileName)
loop l_ = 1 to lines[0]
  say l_.right(4)':' lines[l_]
  end l_

return

-- Read a file and return contents as a Rexx indexed string
method scanFile(inFileName) public static returns Rexx

  fileLines = ''
  do
    inFile = File(inFileName)
    inFileScanner = Scanner(inFile)
    loop l_ = 1 while inFileScanner.hasNext()
      fileLines[0] = l_
      fileLines[l_] = inFileScanner.nextLine()
      end l_
    inFileScanner.close()

  catch ex = FileNotFoundException
    ex.printStackTrace
  end

  return fileLines

```



###  Using Java <tt>Reader</tt>


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg inFileName .

if inFileName = '' | inFileName = '.' then inFileName = './data/dwarfs.json'
lines = readFile(inFileName)
loop l_ = 1 to lines[0]
  say l_.right(4)':' lines[l_]
  end l_

return

-- Read a file and return contents as a Rexx indexed string
method readFile(inFileName) public static returns Rexx

  fileLines = ''
  inLine = String null
  inFileBR = BufferedReader null

  do
    inFile = File(inFileName)
    inFileBR = BufferedReader(FileReader(inFile))
    loop l_ = 1 until inline = null
      inLine = inFileBR.readLine()
      if inline \= null then do
        fileLines[0] = l_
        fileLines[l_] = inLine
        end
      end l_

  catch exFNF = FileNotFoundException
    exFNF.printStackTrace
  catch exIO = IOException
    exIO.printStackTrace
  finally
    if inFileBR \= null then do
      do
        inFileBR.close()
      catch ex = IOException
        ex.printStackTrace
      end
      end
  end

  return fileLines

```



## NewLISP


```NewLISP

(set 'in-file (open "filename" "read"))
(while (read-line in-file)
       (write-line))
(close in-file)
```



## Nim


```nim
for line in lines "input.txt":
  echo line
```



## Objeck


```objeck

bundle Default {
  class ReadFile {
    function : Main(args : String[]) ~ Nil {
      f := IO.FileReader->New("in.txt");
      if(f->IsOpen()) {
        string := f->ReadString();
        while(f->IsEOF() = false) {
          string->PrintLine();
          string := f->ReadString();
        };
        f->Close();
      };
    }
  }
}

```


=={{header|Objective-C}}==

To read an entire file into a string, you can:

```objc
NSString *path = [NSString stringWithString:@"/usr/share/dict/words"];
NSError *error = nil;
NSString *words = [[NSString alloc] initWithContentsOfFile:path
                                               encoding:NSUTF8StringEncoding error:&error];

```


Use the UTF-8 encoder on ASCII.

Now to get the individual lines, break down the string:


```objc
NSArray* lines = [words componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
```



## OCaml



```ocaml
let () =
  let ic = open_in "input.txt" in
  try
    while true do
      let line = input_line ic in
      print_endline line
    done
  with End_of_file ->
    close_in ic
```


But if we want to write a functional loading function we should remember that the <code>try/with</code> couple breaks the [[tail recursion]]. So we should externalise it outside of the loop in another function:


```ocaml
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux []

let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines)
```


we use it like this:


```ocaml
let () =
  let lines = lines_of_file "unixdict.txt" in
  List.iter print_endline lines
```




## Oforth



```Oforth
: readFile(fileName)
  | line | File new(fileName) forEach: line [ line println ] ;
```



## OxygenBasic

The core function '''GetFile''' reads the whole file:

```oxygenbasic

function getline(string s, sys *i, *el) as string
  sys e
  e=instr i,s,chr(el)
  if e=0 then
    el=10
    e=instr i,s,chr(el) 'files not using chr 13
  end if
  if e=0 then e=len s
  e++
  if el=13 then
    if asc(s,e)=10 then e++ 'crlf
  end if
  function = mid s,i,e-i
  i=e
end function

'=====
'TEST:
'=====

s=getfile "t.txt"
i=1
wr=""
c=0
el=13
do
  wr = getline s,i,el
  if wr="" then exit do
  'print wr
  c++
end do
print "Line count " c

```



## PARI/GP

GP has an unfortunate limitations that prevents reading files line-by-line, but it's just as well since its file-handling capabilities are poor.  The [http://pari.math.u-bordeaux.fr/cgi-bin/viewcvs.cgi/trunk/TODO?view=markup&revision=12632&root=pari TODO] file lists one desiderata as adding a <code>t_FILE</code>, which if added would presumably have support for this sort of operation.

Thus the usual way of interacting with files in more than the simple way allowed by <code>read</code> is done by PARI with the usual [[#C|C]] commands:

```C
FILE *f = fopen(name, "r");
if (!f) {
	pari_err(openfiler, "input", name);
}
while(fgets(line, MAX_LINELEN, f) != NULL) {
	// ...
}
```



## Pascal


```pascal
(* Read a file line by line *)
   program ReadFileByLine;
   var
      InputFile,OutputFile: File;
      TextLine: String;
   begin
      Assign(InputFile, 'c:\testin.txt');
      Reset(InputFile);
      Assign(InputFile, 'c:\testout.txt');
      Rewrite(InputFile);
      while not Eof(InputFile) do
	  begin
         ReadLn(InputFile, TextLine);
		 (* do someting with TextLine *)
         WriteLn(OutputFile, TextLine)
      end;
      Close(InputFile);
      Close(OutputFile)
   end.
```



## Perl

For the simple case of iterating over the lines of a file you can do:

```perl
open(my $fh, '<', 'foobar.txt')
    || die "Could not open file: $!";
while (<$fh>)
{ # each line is stored in $_, with terminating newline
  # chomp, short for chomp($_), removes the terminating newline
    chomp;
    process($_);
}
close $fh;
```

File encoding can be specified like:

```perl
open(my $fh, '< :encoding(UTF-8)', 'foobar.txt')
    || die "Could not open file: $!";
```

The angle bracket operator <code>< ></code> reads a filehandle line by line. (The angle bracket operator can also be used to open and read from files that match a specific pattern, by putting the pattern in the brackets.)

Without specifying the variable that each line should be put into, it automatically puts it into <code>$_</code>, which is also conveniently the default argument for many Perl functions. If you wanted to use your own variable, you can do something like this:

```perl
open(my $fh, '<', 'foobar.txt')
    || die "Could not open file: $!";
while (my $line = <$fh>)
{
    chomp $line;
    process($line);
}
close $fh;
```


The special use of the angle bracket operator with nothing inside, will read from all files whose names were specified on the command line:

```perl
while (<>) {
    chomp;
    process($_);
}
```


As noted in <code>perlop.pod</code> under "I/O Operators", <code>&lt;&gt;</code> opens with the 2-arg <code>open()</code> and so can read from a piped command.  This can be convenient but is also very much insecure--a user could supply a file with the name like


```Shell
perl myscript.pl 'rm -rf / |'
```


or any other arbitrary command, which will be executed when perl attempts to open a pipe for it. As such, this feature is best reserved for one-liners and is bad practice to use in production code. The same is true for the open(FILEHANDLE, EXPR) form of open as opposed to open(FILEHANDLE, MODE, EXPR). (See <code>perlfunc.pod</code> on the <code>open()</code> function.)

The <code>ARGV::readonly</code> module can defang <code>@ARGV</code> by modifying the names to ensure they are treated only as files by the <code>open()</code>.

The readline function can be used instead of < >:

```perl
open(my $fh, '<', 'foobar.txt') or die "$!";
while (readline $fh)
{ ... }

while (my $line = readline $fh)
{ ... }
close $fh;
```

The readline function is the internal function used to implement < >, but can be used directly and is useful for conveying programmer intent in certain situations.


## Perl 6

The lines method is lazy so the following code does indeed read the file line by line, and not all at once.

```perl6
for open('test.txt').lines
{
  .say
}
```


In order to be more explicit about the file being read on line at a time, one can write:

```perl6
my $f = open 'test.txt';
while my $line = $f.get {
    say $line;
}
```



## Phix


```Phix
constant fn = open(command_line()[2],"r")
integer lno = 1
object line
while 1 do
    line = gets(fn)
    if atom(line) then exit end if
    printf(1,"%2d: %s",{lno,line})
    lno += 1
end while
close(fn)
{} = wait_key()
```

{{out}}

```txt

 1: constant fn = open(command_line()[2],"r")
 2: integer lno = 1
 3: object line
 4: while 1 do
 5:     line = gets(fn)
 6:     if atom(line) then exit end if
 7:     printf(1,"%2d: %s",{lno,line})
 8:     lno += 1
 9: end while
10: close(fn)
11: {} = wait_key()

```



## PHP


```php
<?php
$file = fopen(__FILE__, 'r'); // read current file
while ($line = fgets($file)) {
    $line = rtrim($line);      // removes linebreaks and spaces at end
    echo strrev($line) . "\n"; // reverse line and upload it
}
```



```php
<?php // HOW TO ECHO FILE LINE BY LINE FROM THE COMMAND LINE: php5-cli
$file = fopen('test.txt', 'r'); // OPEN FILE WITH READ ACCESS
while (!feof($file)) {
    $line = rtrim(fgets($file)); // REMOVE TRAILING WHITESPACE AND GET LINE
    if($line != NULL) echo("$line\n"); // IF THE LINE ISN'T NULL, ECHO THE LINE
}
```



## PicoLisp


```PicoLisp
(in "foobar.txt"
   (while (line)
      (process @) ) )
```



## PL/I


```pli

read: procedure options (main);
   declare line character (500) varying;

   on endfile (sysin) stop;

   do forever;
      get edit (line)(L);
   end;
end read;

```



## PowerShell


```PowerShell
$reader = [System.IO.File]::OpenText($mancorfile)
try {
	do {
		$line = $reader.ReadLine()
		if ($line -eq $null) { break }
		DoSomethingWithLine($line)
	} while ($TRUE)
} finally {
	$reader.Close()
}

```



## PureBasic


```PureBasic
FileName$ = OpenFileRequester("","foo.txt","*.txt",0)

If ReadFile(0, FileName$)          ; use ReadFile instead of OpenFile to include read-only files
  BOMformat = ReadStringFormat(0)  ; reads the BOMformat (Unicode, UTF-8, ASCII, ...)
  While Not Eof(0)
    line$ = ReadString(0, BOMformat)
    DoSomethingWithTheLine(Line)
  Wend
  CloseFile(0)
EndIf
```



## Python

For the simple case of iterating over the lines of a file you can do:

```python
with open("foobar.txt") as f:
    for line in f:
        process(line)
```

The with statement ensures the correct closing of the file after it is processed, and iterating over the file object <code>f</code>, adjusts what is considered line separator character(s) so the code will work on multiple operating systems such as Windows, Mac, and Solaris without change.

Any exceptional conditions seen when processing the file will raise an exception. Leaving the while loop because of an exception will also cause the file to be correctly closed on the way.

Python also has the [http://docs.python.org/library/fileinput.html fileinput module]. This can process multiple files parsed from the command line and can be set to modify files 'in-place'.

```python
import fileinput
for line in fileinput.input():
    process(line)

```



## R



```R
conn <- file("notes.txt", "r")
while(length(line <- readLines(conn, 1)) > 0) {
    cat(line, "\n")
}
```



## Racket


```racket
(define (read-next-line-iter file)
	   (let ((line (read-line file 'any)))
	     (unless (eof-object? line)
	       (display line)
	       (newline)
	       (read-next-line-iter file))))
(call-with-input-file "foobar.txt" read-next-line-iter)
```



```racket
(define in (open-input-file file-name))
(for ([line (in-lines in)])
     (displayln line))
(close-input-port in)
```



## RapidQ


```vb

$Include "Rapidq.inc"
dim file as qfilestream

if file.open("c:\A Test.txt", fmOpenRead) then
    while not File.eof
        print File.readline
    wend
else
    print "Cannot read file"
end if

input "Press enter to exit: ";a$

```



## REXX


### basic version


```REXX
/*REXX program to read and display (with count) a file, one line at a time.*/
parse arg fileID .
say 'Displaying file:' fileID

  do linenumber=1  while lines(fileID)\==0      /* loop construct */
  line=linein(fileID)                           /* read line */
  say 'Line' linenumber':' line                 /* show line number and line */
  end linenumber                                /* end loop and confirm which loop */

say
say 'File' fileID 'has' linenumber-1 'lines.'   /*summary.*/
```



### known name version

Or: the 'known name' short version:

```rexx
file='foobar.txt'
do while lines(file)\==0; say linein(file); end
```



### belt and suspenders

The first   '''linein'''   invocation is used to position the record pointer (current position in the file for reading)

in case the parent (REXX) program has already read (for instance) the first couple of records, and the

beginning of the file needs to be re-established so the reading can start from the beginning of the file.


The   '''lineout'''   BIF closes the file (in most REXX interpreters);   this is done for general housekeeping.

```rexx
/*REXX program   reads  and  displays  (with a count)  a file,  one line at a time.     */
parse arg fID .;      if fID==''  then exit      /*Was no fileID specified?  Then quit. */
say center(' displaying file: ' fID" ", 79, '═') /*show the name of the file being read.*/
call linein fID, 1, 0                            /*see the comment in the section header*/
say                                              /* [↓]  show a file's contents (lines).*/
     do #=1  while lines(fID)\==0; y=linein(fID) /*loop whilst there are lines in file. */
     say                                         /*show a blank line for peruseability. */
     say 'record#='   #   "  length="  length(y) /*show the record number and the length*/
     say y                                       /*show the content of the line (record)*/
     end   /*#*/
say
say 'file '   fID   " has "   #-1   ' records.'  /*display the record count summary.    */
call lineout  fID                                /*close the input file  (most REXXes). */
                                                 /*stick a fork in it,  we're all done. */
```






### ARexx version


```rexx
/* Also works with Regina if you state OPTIONS AREXX_BIFS ; OPTIONS AREXX_SEMANTICS */
filename='file.txt'
contents=''
IF Open(filehandle,filename,'READ')
THEN DO UNTIL EOF(filehandle)
   line=ReadLn(filehandle)
   SAY line
   contents=contents || line || '0a'x
   END
ELSE EXIT 20
CALL Close filehandle
EXIT 0

```



## Ring


```ring

fp = fopen("C:\Ring\ReadMe.txt","r")
r = ""
while isstring(r)
      r = fgetc(fp)
      if r = char(10) see nl
      else see r ok
end
fclose(fp)

```



## Ruby


```ruby
IO.foreach "foobar.txt" do |line|
  # Do something with line.
  puts line
end
```



```ruby
# File inherits from IO, so File.foreach also works.
File.foreach("foobar.txt") {|line| puts line}
```



```ruby
# IO.foreach and File.foreach can also read a subprocess.
IO.foreach "| grep afs3 /etc/services" do |line|
  puts line
end
```


''Caution!'' IO.foreach and File.foreach take a portname.
To open an arbitrary filename (which might start with "|"),
you must use File.open, then IO#each (or IO#each_line).
The block form of File.open automatically closes the file after running the block.


```ruby
filename = "|strange-name.txt"
File.open(filename) do |file|
  file.each {|line| puts line}
end
```



## Run BASIC


```runbasic
open DefaultDir$ + "\public\filetest.txt" for input as #f
while not(eof(#f))
  line input #f, a$
  print a$
wend
close #f

```



## Rust


```rust
use std::io::{BufReader,BufRead};
use std::fs::File;

fn main() {
    let file = File::open("file.txt").unwrap();
    for line in BufReader::new(file).lines() {
        println!("{}", line.unwrap());
    }
}
```


{{out}}


```txt
First line of the file!
Second line of the file!
```



## Scala


```scala
import scala.io._
Source.fromFile("foobar.txt").getLines.foreach(println)
```



## Scheme


```scheme
; Commented line below should be uncommented to use read-line with Guile
;(use-modules (ice-9 rdelim))

(define file (open-input-file "input.txt"))
(do ((line (read-line file) (read-line file))) ((eof-object? line))
        (display line)
        (newline))
```



## Sed

Through a .sed file:

```sed
#!/bin/sed -f
p

```


or through a one-liner in bash:

```bash

sed p filename

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var file: aFile is STD_NULL;
    var string: line is "";
  begin
    aFile := open("input.txt", "r");
    while hasNext(aFile) do
      readln(aFile, line);
      writeln("LINE: " <& line);
    end while;
  end func;
```


The function [http://seed7.sourceforge.net/libraries/file.htm#hasNext%28in_file%29 hasNext]
returns TRUE when at least one character can be read successfully.


## Sidef

''FileHandle.each{}'' is lazy, allowing us to do this:

```ruby
File(__FILE__).open_r.each { |line|
    print line
}
```


Same thing explicitly:

```ruby
var fh = File(__FILE__).open_r
while (fh.readline(\var line)) {
    print line
}
```



## Smalltalk

{{works with|Pharo}}

```smalltalk

(StandardFileStream oldFileNamed: 'test.txt') contents lines do: [  :each | Transcript show: each. ]

```


{{works with|Smalltalk/X}}

```smalltalk
'foobar.txt' asFilename readingLinesDo:[:eachLine | eachLine printCR]
```

alternatively:

```smalltalk
|s|
s := 'foobar.txt' asFilename readStream.
[ s atEnd ] whileFalse:[
    s nextLine printCR.
].
s close
```

alternatively:

```smalltalk
'foobar.txt' asFilename contents do:[:eachLine | eachLine printCR].
```



## SNOBOL4


In SNOBOL4, file I/O is done by associating a file with a variable.
Every subsequent access to the variable provides the next record of the file.
Options to the input() function allow the file to be opened in line mode, fixed-blocksize (raw binary) mode, and with various sharing options.
The input() operation generally fails (in most modern implementations) if the file requested is not found (in earlier implementations, that failure is reported the same way as end-of-file when the first actual read from the file is attempted).
You can specify the file unit number to use (a vestigial remnant of the Fortran I/O package used by original Bell Labs SNOBOL4 implementations... in this case, I'll use file unit 20).
Accessing the variable fails (does not succeed) when the end of file is reached.


```snobol4
        input(.infile,20,"readfrom.txt")      :f(end)
rdloop  output = infile                       :s(rdloop)
end
```



## Sparkling


```sparkling
let f = fopen("foo.txt", "r");
if f != nil {
    var line;
    while (line = fgetline(f)) != nil {
        print(line);
    }

    fclose(f);
}
```



## SPL


```spl
f = "file.txt"
> !#.eof(f)
  #.output(#.readline(f))
<
```



## Tcl


```tcl
set f [open "foobar.txt"]
while {[gets $f line] >= 0} {
    # This loops over every line
    puts ">>$line<<"
}
close $f
```



## TorqueScript


Read a file line by line:


```TorqueScript

//Create a file object

%f = new fileObject();

//Open and read a file

%f.openForRead("PATH/PATH.txt");

while(!%f.isEOF())
{
	//Read each line from our file

	%line = %f.readLine();
}

//Close the file object

%f.close();

//Delete the file object

%f.delete();

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

datei="rosetta.txt"
ERROR/STOP OPEN (datei,READ,-std-)

ACCESS q: READ/RECORDS/UTF8 $datei s,line
 LOOP
 READ/NEXT/EXIT q
 PRINT line
 ENDLOOP
ENDACCESS q

```

or:

```tuscript

LOOP line=datei
 PRINT line
ENDLOOP

```



## UNIX Shell

Redirect standard input from a file, and then use '''<code>IFS= read -r line</code>''' to read each line.

: [https://www.mirbsd.org/htman/i386/man1/mksh.htm mksh(1) manual] says, "If '''<code>read</code>''' is run in a loop such as '''<code>while read foo; do ...; done</code>''' then leading whitespace will be removed (IFS) and backslashes processed. You might want to use '''<code>while IFS= read -r foo; do ...; done</code>''' for pristine I/O."

{{works with|Almquist Shell}}


```bash
# This while loop repeats for each line of the file.
# This loop is inside a pipeline; many shells will
# run this loop inside a subshell.
cat input.txt |
while IFS= read -r line ; do
  printf '%s\n' "$line"
done
```


{{works with|Almquist Shell}}


```bash
# This loop runs in the current shell, and can read both
# the old standard input (fd 1) and input.txt (fd 3).
exec 3<input.txt
while IFS= read -r line <&3 ; do
  printf '%s\n' "$line"
done
exec 3>&-
```


{{works with|Bourne Shell}}


```bash
# The old Bourne Shell interprets 'IFS= read' as 'IFS= ; read'.
# It requires extra code to restore the original value of IFS.
exec 3<input.txt
oldifs=$IFS
while IFS= ; read -r line <&3 ; do
  IFS=$oldifs
  printf '%s\n' "$line"
done
IFS=$oldifs
exec 3>&-
```



## Ursa

Reads the file "filename.txt" and outputs it to the console line by line.

```ursa
decl file f
f.open "filename.txt"
while (f.hasline)
        out (in string f) endl console
end while
```



## Vala

Reads and prints out file line by line:

```vala

public static void main(){
	var file = FileStream.open("foo.txt", "r");

	string line = file.read_line();
	while (line != null){
		stdout.printf("%s\n", line);
		line = file.read_line();
	}
}

```



## VBA


```vb
' Read a file line by line
Sub Main()
    Dim fInput As String, fOutput As String 'File names
    Dim sInput As String, sOutput As String 'Lines
    fInput = "input.txt"
    fOutput = "output.txt"
    Open fInput For Input As #1
    Open fOutput For Output As #2
    While Not EOF(1)
        Line Input #1, sInput
        sOutput = Process(sInput) 'do something
        Print #2, sOutput
    Wend
    Close #1
    Close #2
End Sub 'Main
```



## VBScript


```vb

FilePath = "<SPECIFY FILE PATH HERE>"
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.OpenTextFile(FilePath,1)
Do Until objFile.AtEndOfStream
	WScript.Echo objFile.ReadLine
Loop
objFile.Close
Set objFSO = Nothing

```



## Vedit macro language

On Vedit, you do not actually read file line by line.
File reading and writing is handled by automatic file buffering
while you process the file.

This example reads the source code of this macro,
copies it line by line into a new buffer and adds line numbers.

```vedit
File_Open("line_by_line.vdm")
#10 = Buf_Num                   // edit buffer for input file
#11 = Buf_Free                  // edit buffer for output
#1 = 1                          // line number
while (!At_EOF) {
    Reg_Copy(20,1)              // read one line into text register 20
    Buf_Switch(#11)             // switch to output file
    Num_Ins(#1++, NOCR)         // write line number
    Ins_Text("  ")
    Reg_Ins(20)                 // write the line
    Buf_Switch(#10)             // switch to input file
    Line(1)                     // next line
}
Buf_Close(NOMSG)                // close the input file
Buf_Switch(#11)                 // show the output
```


{{out}}

```txt

    1  File_Open("line_by_line.vdm")
    2  #10 = Buf_Num                   // edit buffer for input file
    3  #11 = Buf_Free                  // edit buffer for output
    4  #1 = 1                          // line number
    5  while (!At_EOF) {
    6      Reg_Copy(20,1)              // read one line into text register 20
    7      Buf_Switch(#11)             // switch to output file
    8      Num_Ins(#1++, NOCR)         // write line number
    9      Ins_Text("  ")
   10      Reg_Ins(20)                 // write the line
   11      Buf_Switch(#10)             // switch to input file
   12      Line(1)                     // next line
   13  }
   14  Buf_Close(NOMSG)                // close the input file
   15  Buf_Switch(#11)                 // show the output

```




## Visual Basic



### Simple version

{{works with|Visual Basic|VB6 Standard}}

```vb
' Read a file line by line
Sub Main()
    Dim fInput As String, fOutput As String 'File names
    Dim sInput As String, sOutput As String 'Lines
    Dim nRecord As Long
    fInput = "input.txt"
    fOutput = "output.txt"
    On Error GoTo InputError
    Open fInput For Input As #1
    On Error GoTo 0 'reset error handling
    Open fOutput For Output As #2
    nRecord = 0
    While Not EOF(1)
        Line Input #1, sInput
        sOutput = Process(sInput) 'do something
        nRecord = nRecord + 1
        Print #2, sOutput
    Wend
    Close #1
    Close #2
    Exit Sub
InputError:
    MsgBox "File: " & fInput & " not found"
End Sub 'Main
```



### Complex version


```vb
' Read lines from a file
'
' (c) Copyright 1993 - 2011 Mark Hobley
'
' This code was ported from an application program written in Microsoft Quickbasic
'
' This code can be redistributed or modified under the terms of version 1.2 of
' the GNU Free Documentation Licence as published by the Free Software Foundation.

Sub readlinesfromafile()
  var.filename = "foobar.txt"
  var.filebuffersize = ini.inimaxlinelength
  Call openfileread
  If flg.error = "Y" Then
    flg.abort = "Y"
    Exit Sub
  End If
  If flg.exists <> "Y" Then
    flg.abort = "Y"
    Exit Sub
  End If
readfilelabela:
  Call readlinefromfile
  If flg.error = "Y" Then
    flg.abort = "Y"
    Call closestream
    flg.error = "Y"
    Exit Sub
  End If
  If flg.endoffile <> "Y" Then
    ' We have a line from the file
    Print message$
    GoTo readfilelabela
  End If
  ' End of file reached
  ' Close the file and exit
  Call closestream
  Exit Sub
End Sub

Sub openfileread()
  flg.streamopen = "N"
  Call checkfileexists
  If flg.error = "Y" Then Exit Sub
  If flg.exists <> "Y" Then Exit Sub
  Call getfreestream
  If flg.error = "Y" Then Exit Sub
  var.errorsection = "Opening File"
  var.errordevice = var.filename
  If ini.errortrap = "Y" Then
    On Local Error GoTo openfilereaderror
  End If
  flg.endoffile = "N"
  Open var.filename For Input As #var.stream Len = var.filebuffersize
  flg.streamopen = "Y"
  Exit Sub
openfilereaderror:
  var.errorcode = Err
  Call errorhandler
  resume '!!
End Sub

Public Sub checkfileexists()
  var.errorsection = "Checking File Exists"
  var.errordevice = var.filename
  If ini.errortrap = "Y" Then
    On Local Error GoTo checkfileexistserror
  End If
  flg.exists = "N"
  If Dir$(var.filename, 0) <> "" Then
    flg.exists = "Y"
  End If
  Exit Sub
checkfileexistserror:
  var.errorcode = Err
  Call errorhandler
End Sub

Public Sub getfreestream()
  var.errorsection = "Opening Free Data Stream"
  var.errordevice = ""
  If ini.errortrap = "Y" Then
    On Local Error GoTo getfreestreamerror
  End If
  var.stream = FreeFile
  Exit Sub
getfreestreamerror:
  var.errorcode = Err
  Call errorhandler
  resume '!!
End Sub

Sub closestream()
  If ini.errortrap = "Y" Then
    On Local Error GoTo closestreamerror
  End If
  var.errorsection = "Closing Stream"
  var.errordevice = ""
  flg.resumenext = "Y"
  Close #var.stream
  If flg.error = "Y" Then
    flg.error = "N"
    '!! Call unexpectederror
  End If
  flg.streamopen = "N"
  Exit Sub
closestreamerror:
  var.errorcode = Err
  Call errorhandler
  resume next
End Sub

Public Sub errorhandler()
  tmp$ = btrim$(var.errorsection)
  tmp2$ = btrim$(var.errordevice)
  If tmp2$ <> "" Then
    tmp$ = tmp$ + " (" + tmp2$ + ")"
  End If
  tmp$ = tmp$ + " : " + Str$(var.errorcode)
  tmp1% = MsgBox(tmp$, 0, "Error!")
  flg.error = "Y"
  If flg.resumenext = "Y" Then
    flg.resumenext = "N"
'    Resume Next
  Else
    flg.error = "N"
'    Resume
  End If
End Sub

Public Function btrim$(arg$)
  btrim$ = LTrim$(RTrim$(arg$))
End Function
```



## Visual Basic .NET


```vbnet
Imports System.IO

  ' Loop through the lines of a file.
  ' Function assumes that the file exists.
  Private Sub ReadLines(ByVal FileName As String)

    Dim oReader As New StreamReader(FileName)
    Dim sLine As String = Nothing

    While Not oReader.EndOfStream
      sLine = oReader.ReadLine()
      ' Do something with the line.
    End While

    oReader.Close()

  End Sub
```



## Wart


```wart
with infile "x"
  drain (read_line)
```



## zkl

So many ways, here are a few

```zkl
foreach line in (File("foo.zkl")){print(line)}
```


```zkl
File("foo.zkl").pump(Console.print)
```


```zkl
Utils.zipWith(False,fcn(a,b){"%d: %s".fmt(a,b).print()},
      [0..],File("foo.zkl","r"))
-->
0: var R; n:=GarbageMan.gcCount;
1: ref := GarbageMan.WeakRef(String("the","quick brown fox"));
...
```

