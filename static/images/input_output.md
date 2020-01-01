+++
title = "File input/output"
description = ""
date = 2019-10-15T10:07:11Z
aliases = []
[extra]
id = 1511
[taxonomies]
categories = []
tags = []
+++

{{task|File handling}} {{selection|Short Circuit|Console Program Basics}} [[Category:Simple]]

;Task:
Create a file called   "output.txt",   and place in it the contents of the file   "input.txt",   ''via an intermediate variable''.

In other words, your program will demonstrate:
::#   how to read from a file into a variable
::#   how to write a variable's contents into a file



Oneliners that skip the intermediate variable are of secondary interest — operating systems have copy commands for that.





## ACL2


```lisp
:set-state-ok t

(defun read-channel (channel limit state)
   (mv-let (ch state)
           (read-char$ channel state)
      (if (or (null ch)
              (zp limit))
          (let ((state (close-input-channel channel state)))
             (mv nil state))
          (mv-let (so-far state)
                  (read-channel channel (1- limit) state)
             (mv (cons ch so-far) state)))))

(defun read-from-file (filename limit state)
   (mv-let (channel state)
           (open-input-channel filename :character state)
      (mv-let (contents state)
              (read-channel channel limit state)
         (mv (coerce contents 'string) state))))

(defun write-channel (channel cs state)
   (if (endp cs)
       (close-output-channel channel state)
       (let ((state (write-byte$ (char-code (first cs))
                                 channel state)))
          (let ((state (write-channel channel
                                      (rest cs)
                                      state)))
              state))))

(defun write-to-file (filename str state)
   (mv-let (channel state)
           (open-output-channel filename :byte state)
      (write-channel channel (coerce str 'list) state)))

(defun copy-file (in out state)
   (mv-let (contents state)
           (read-from-file in (expt 2 40) state)
      (write-to-file out contents state)))
```



## Ada



### Line by line


Assuming everything is fine and no error handling is required, this solution is sufficient:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Read_And_Write_File_Line_By_Line is
   Input, Output : File_Type;
begin
   Open (File => Input,
         Mode => In_File,
         Name => "input.txt");
   Create (File => Output,
           Mode => Out_File,
           Name => "output.txt");
   loop
      declare
         Line : String := Get_Line (Input);
      begin
         -- You can process the contents of Line here.
         Put_Line (Output, Line);
      end;
   end loop;
   Close (Input);
   Close (Output);
exception
   when End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;
      if Is_Open(Output) then
         Close (Output);
      end if;
end Read_And_Write_File_Line_By_Line;
```


Expanded with proper error handling and reporting it reads:


```ada
with Ada.Command_Line, Ada.Text_IO; use Ada.Command_Line, Ada.Text_IO;

procedure Read_And_Write_File_Line_By_Line is
   Read_From : constant String := "input.txt";
   Write_To  : constant String := "output.txt";

   Input, Output : File_Type;
begin
   begin
      Open (File => Input,
            Mode => In_File,
            Name => Read_From);
   exception
      when others =>
         Put_Line (Standard_Error,
                   "Can not open the file '" & Read_From & "'. Does it exist?");
         Set_Exit_Status (Failure);
         return;
   end;

   begin
      Create (File => Output,
              Mode => Out_File,
              Name => Write_To);
   exception
      when others =>
         Put_Line (Standard_Error,
                   "Can not create a file named '" & Write_To & "'.");
         Set_Exit_Status (Failure);
         return;
   end;

   loop
      declare
         Line : String := Get_Line (Input);
      begin
         -- You can process the contents of Line here.
         Put_Line (Output, Line);
      end;
   end loop;
   Close (Input);
   Close (Output);
exception
   when End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;
      if Is_Open(Output) then
         Close (Output);
      end if;
end Read_And_Write_File_Line_By_Line;
```



### Character by character


The following example reads and writes each file one character at a time.  (You should of course add error reporting as in the example above.)

```ada
with Ada.Sequential_IO;

procedure Read_And_Write_File_Character_By_Character is
   package Char_IO is new Ada.Sequential_IO (Character);
   use Char_IO;

   Input, Output : File_Type;
   Buffer        : Character;
begin
   Open   (File => Input,  Mode => In_File,  Name => "input.txt");
   Create (File => Output, Mode => Out_File, Name => "output.txt");
   loop
      Read  (File => Input,  Item => Buffer);
      Write (File => Output, Item => Buffer);
   end loop;
   Close (Input);
   Close (Output);
exception
   when End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;
      if Is_Open(Output) then
         Close (Output);
      end if;
end Read_And_Write_File_Character_By_Character;
```


===Using Ada.Text_IO.Text_Streams===

The following solution uses stream I/O. Any file of Ada.Text_IO can be used to obtain a corresponding stream. Reading and writing streams is more efficient than reading text files directly, because it skips formatting.


```ada
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;  use Ada.Text_IO.Text_Streams;

procedure Using_Text_Streams is
   Input, Output : File_Type;
   Buffer        : Character;
begin
   Open   (File => Input,  Mode => In_File,  Name => "input.txt");
   Create (File => Output, Mode => Out_File, Name => "output.txt");
   loop
      Buffer := Character'Input (Stream (Input));
      Character'Write (Stream (Output), Buffer);
   end loop;
   Close (Input);
   Close (Output);
exception
   when End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;
      if Is_Open(Output) then
         Close (Output);
      end if;
end Using_Text_Streams;
```



## Aime


```aime
file i, o;
text s;

i.open("input.txt", OPEN_READONLY, 0);
o.open("output.txt", OPEN_CREATE | OPEN_TRUNCATE | OPEN_WRITEONLY,
       0644);

while (i.line(s) ^ -1) {
    o.text(s);
    o.byte('\n');
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
PROC copy file v1 = (STRING in name, out name)VOID: (
  # note: algol68toc-1.18 - can compile, but not run v1 #
  INT errno;
  FILE in file, out file;
  errno := open(in  file, in  name, stand in  channel);
  errno := open(out file, out name, stand out channel);

  BOOL in ended := FALSE;
  PROC call back ended = (REF FILE f) BOOL: in ended := TRUE;
  on logical file end(in file, call back ended);

  STRING line;
  WHILE
    get(in  file, (line, new line));
# WHILE # NOT in ended DO # break to avoid excess new line #
    put(out file, (line, new line))
  OD;
ended:
  close(in  file);
  close(out file)
);

PROC copy file v2 = (STRING in name, out name)VOID: (
  INT errno;
  FILE in file, out file;
  errno := open(in file, in name, stand in channel);
  errno := open(out file, out name, stand out channel);

  PROC call back ended = (REF FILE f) BOOL: GO TO done;
  on logical file end(in file, call back ended);

  STRING line;
  DO
    get(in  file, line);
    put(out file, line);
    get(in  file, new line);
    put(out file, new line)
  OD;
done:
  close(in file);
  close(out file)
);

test:(
  copy file v2("input.txt","output.txt")
)
```



## AppleScript


```applescript
on copyFile from src into dst
       set filedata to read file src
       set outfile to open for access dst with write permission
       write filedata to outfile
       close access outfile
end copyFile

copyFile from ":input.txt" into ":output.txt"
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program readwrtfile.s   */

/*********************************************/
/*constantes                                */
/********************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ READ,   3
.equ WRITE,  4
.equ OPEN,   5
.equ CLOSE,  6
.equ CREATE,  8
/*  file */
.equ O_RDWR,	0x0002		@ open for reading and writing

.equ TAILLEBUF,  1000
/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessErreur: .asciz "Erreur ouverture fichier input.\n"
szMessErreur4: .asciz "Erreur création fichier output.\n"
szMessErreur1: .asciz "Erreur fermeture fichier.\n"
szMessErreur2: .asciz "Erreur lecture fichier.\n"
szMessErreur3: .asciz "Erreur d'écriture dans fichier de sortie.\n"
szRetourligne: .asciz  "\n"
szMessErr: .ascii	"Error code : "
sDeci: .space 15,' '
         .asciz "\n"

szNameFileInput:	.asciz "input.txt"
szNameFileOutput:	.asciz "output.txt"

/*******************************************/
/* DONNEES NON INITIALISEES                */
/*******************************************/
.bss
sBuffer:  .skip TAILLEBUF

/**********************************************/
/* -- Code section                            */
/**********************************************/
.text
.global main
main:
    push {fp,lr}    /* save registers */

    ldr r0,iAdrszNameFileInput   @ file name
    mov r1,#O_RDWR                   @  flags
    mov r2,#0                         @ mode
    mov r7,#OPEN                     @ call system OPEN
    swi #0
    cmp r0,#0        @ open error ?
    ble erreur
    mov r8,r0               @ save File Descriptor
    ldr r1,iAdrsBuffer   @ buffer address
    mov r2,#TAILLEBUF     @ buffer size
    mov r7, #READ          @ call system  READ
    swi 0
    cmp r0,#0            @ read error ?
    ble erreur2
    mov r2,r0            @ length read characters

    /* close imput file */
    mov r0,r8     @ Fd
    mov r7, #CLOSE      @ call system CLOSE
    swi 0
    cmp r0,#0            @ close error ?
    blt erreur1

    @ create output file
    ldr r0,iAdrszNameFileOutput   @ file name
    ldr r1,iFicMask1                 @ flags
    mov r7, #CREATE                  @ call system create file
    swi 0
    cmp r0,#0                         @ create error ?
    ble erreur4
    mov r0,r8                       @ file descriptor
    ldr r1,iAdrsBuffer
    @ et r2 contains the length to write
    mov r7, #WRITE                 @ select system call 'write'
    swi #0                        @ perform the system call
    cmp r0,#0                      @ error write ?
    blt erreur3

    @ close output file
    mov r0,r8    @ Fd  fichier
    mov r7, #CLOSE    @  call system CLOSE
    swi #0
    cmp r0,#0      @ error close ?
    blt erreur1
    mov r0,#0     @ return code OK
    b 100f
erreur:
    ldr r1,iAdrszMessErreur
    bl   afficheerreur
    mov r0,#1       @ error return code
    b 100f
erreur1:
    ldr r1,iAdrszMessErreur1
    bl   afficheerreur
    mov r0,#1       @ error return code
    b 100f
erreur2:
    ldr r1,iAdrszMessErreur2
    bl   afficheerreur
    mov r0,#1       @ error return code
    b 100f
erreur3:
    ldr r1,iAdrszMessErreur3
    bl   afficheerreur
    mov r0,#1       @ error return code
    b 100f
erreur4:
    ldr r1,iAdrszMessErreur4
    bl   afficheerreur
    mov r0,#1       @ error return code
    b 100f

100:		@ end program
    pop {fp,lr}   /* restaur des  2 registres */
    mov r7, #EXIT /* appel fonction systeme pour terminer */
    swi 0
iAdrszNameFileInput:	.int szNameFileInput
iAdrszNameFileOutput:	.int szNameFileOutput
iAdrszMessErreur:		.int szMessErreur
iAdrszMessErreur1:		.int szMessErreur1
iAdrszMessErreur2:		.int szMessErreur2
iAdrszMessErreur3:		.int szMessErreur3
iAdrszMessErreur4:		.int szMessErreur4
iAdrsBuffer:				.int sBuffer
iFicMask1: 				.octa 0644
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}    			/* save  registres */
    mov r2,#0   				/* counter length */
1:      /* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system "write" */
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7,lr}    	/* restaur des  2 registres */
    bx lr	        			/* return  */
/***************************************************/
/*   display error message                         */
/***************************************************/
/* r0 contains error code  r1  address error message */
afficheerreur:
   push {r1-r2,lr}    @ save registers
    mov r2,r0         @ save error code
    mov r0,r1         @ address error message
    bl affichageMess   @ display error message
    mov r0,r2         @ error code
    ldr r1,iAdrsDeci    @ result address
    bl conversion10S
    ldr r0,iAdrszMessErr @ display error code
    bl affichageMess
    pop {r1-r2,lr}    @ restaur registers
    bx lr              @ return function
iAdrszMessErr:   .int szMessErr
iAdrsDeci:		.int sDeci

/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}    @ save registers
    mov r2,r1       /* debut zone stockage */
    mov r3,#'+'     /* par defaut le signe est + */
    cmp r0,#0       @ negative number ?
    movlt r3,#'-'   @ yes
    mvnlt r0,r0     @ number inversion
    addlt r0,#1
    mov r4,#10       @ length area
1:  @ start loop
    bl divisionPar10R
    add r1,#48   @ digit
    strb r1,[r2,r4]  @ store digit on area
    sub r4,r4,#1      @ previous position
    cmp r0,#0          @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]  @ store signe
    subs r4,r4,#1    @ previous position
    blt  100f        @ if r4 < 0 -> end

    mov r1,#' '   @ space
2:
    strb r1,[r2,r4]  @store byte space
    subs r4,r4,#1    @ previous position
    bge 2b           @ loop if r4 > 0
100:
    pop {r0-r4,lr}   @ restaur registers
    bx lr

/***************************************************/
/*   division for 10 fast unsigned                 */
/***************************************************/
@ r0 contient le dividende
@ r0 retourne le quotient
@ r1 retourne le reste
divisionPar10R:
    push {r2,lr}         @ save  registers
    sub r1, r0, #10        @ calcul de r0 - 10
    sub r0, r0, r0, lsr #2  @ calcul de r0 - (r0 /4)
    add r0, r0, r0, lsr #4  @ calcul de (r0-(r0/4))+ ((r0-(r0/4))/16
    add r0, r0, r0, lsr #8  @ etc ...
    add r0, r0, r0, lsr #16
    mov r0, r0, lsr #3
    add r2, r0, r0, asl #2
    subs r1, r1, r2, asl #1    @ calcul (N-10) - (N/10)*10
    addpl r0, r0, #1          @ regul quotient
    addmi r1, r1, #10         @ regul reste
    pop {r2,lr}
    bx lr

```



## Arturo



```arturo
source $(read "input.txt")
write "output.txt" source

print $(source)
```


{{out}}


```txt
This is some
sample text in input.txt
```



## AutoHotkey

Method 1: the input file can be processed line by line.

```AutoHotkey
Loop, Read, input.txt, output.txt
 FileAppend, %A_LoopReadLine%`n
```

Method 2: the input file can be read at once if it is less than 1 GB.

```autohotkey
FileRead, var, input.txt
FileAppend, %var%, output.txt
```

Method 3: the file can be copied without I/O.

```autohotkey
FileCopy, input.txt, output.txt
```


Binary I/O is possible with [http://www.autohotkey.com/forum/topic4604.html&highlight=binread this] library from Laszlo.


## AWK


(This does not handle properly binary files)


```awk
BEGIN {
  while ( (getline <"input.txt") > 0 ) {
    print >"output.txt"
  }
}
```



## Babel


```babel
(main
    { "input.txt" >>>   -- File is now on stack
    foo set             -- File is now in 'foo'
    foo "output.txt" <<< })
```


The spirit of Babel is to manipulate things on the stack whenever feasible. In this example,
I showed how to save it into a symbolic variable (foo) but this step would not be necessary
for many simple file-processing tasks, such as splitting on newlines or spaces.

Also note that the >>> (slurp) and <<< (spit) operators only handle "small" files - the limit is
configurable but the default limit is 100MB. If you want to open very large files or if you need
to perform a lot of interactive file I/O, Babel provides operators that wrap the C standard library
fopen()/fclose() functions.


## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
 OPEN "INPUT.TXT" FOR INPUT AS #1
 OPEN "OUTPUT.TXT" FOR OUTPUT AS #2
 DO UNTIL EOF(1)
   LINE INPUT #1, Data$
   PRINT #2, Data$
 LOOP
 CLOSE #1
 CLOSE #2
 SYSTEM
```


=
## Applesoft BASIC
=
This is only meant to copy a sequential text file. It is very unlikely that this works copying a random access text file.

```ApplesoftBasic
100 I$ = "INPUT.TXT"
110 O$ = "OUTPUT.TXT"
120 M$ = CHR$(13)
130 D$ = CHR$(4)
140 PRINT D$"VERIFY"I$
150 PRINT D$"OPEN"O$
160 PRINT D$"DELETE"O$
170 PRINT D$"OPEN"O$
180 PRINT D$"OPEN"I$

190 PRINT D$"READ"I$
200 ONERR GOTO 280
210 GET C$
220 POKE 216,0
230 PRINT M$D$"WRITE"O$",B"B
240 B = B + 1
250 P = 2 - (C$ <> M$)
260 PRINT MID$(C$, P)
270 GOTO 190

280 POKE 216,0
290 EOF = PEEK(222) = 5
300 IF NOT EOF THEN RESUME
310 PRINT M$D$"CLOSE"

```


=
## Commodore BASIC
=

```commodorebasic
10 print chr$(14) : rem switch to upper+lower case set
20 print "read seq file input.txt and write to seq file output.txt"
30 open 4,8,4,"input.txt,seq,read"
40 open 8,8,8,"@:output.txt,seq,write" : rem '@'== new file
50 for i=0 to 1 : rem while i==0
60 input#4,a$
70 i=64 and st : rem check bit 6=='end of file'
80 print a$
90 print#8,a$
100 next : rem end while
110 close 4
120 close 8
130 end
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 STRING TX$*254
110 OPEN #1:"output.txt"
120 OPEN #2:"input.txt" ACCESS OUTPUT
130 WHEN EXCEPTION USE IOERROR
140   DO
150     LINE INPUT #1:TX$
160     PRINT #2:TX$
170   LOOP
180 END WHEN
190 HANDLER IOERROR
200   IF EXTYPE<>8001 THEN PRINT EXSTRING$(EXTYPE)
210   CLOSE #1
220   CLOSE #2
230 END HANDLER
```



## Batch File


```dos>copy input.txt output.txt</lang

or

```dos>type input.txt > output.txt</lang

or

```dos
for /f "" %L in ('more^<input.txt') do echo %L>>output.txt
```


There may be other techniques too.


## BBC BASIC

[[BBC BASIC for Windows]] has a file copy command:

```bbcbasic
      *COPY input.txt output.txt
```

Alternatively the copy can be done explicitly:

```bbcbasic
      infile% = OPENIN("input.txt")
      outfile% = OPENOUT("output.txt")
      WHILE NOT EOF#infile%
        BPUT #outfile%, BGET#infile%
      ENDWHILE
      CLOSE #infile%
      CLOSE #outfile%
```



## Befunge

{{works with|CCBI|2.1}}

```befunge
0110"txt.tupni"#@i10"txt.tuptuo"#@o@
```


This linear program tries to open "input.txt" as text file (or aborts).
It then writes the content in text mode (i.e. minus trailing spaces) to "output.txt" (or aborts).


## Bracmat


```bracmat
put$(get$"input.txt","output.txt",NEW)
```



## C


```c
#include <stdio.h>

int main(int argc, char **argv) {
  FILE *in, *out;
  int c;

  in = fopen("input.txt", "r");
  if (!in) {
    fprintf(stderr, "Error opening input.txt for reading.\n");
    return 1;
  }

  out = fopen("output.txt", "w");
  if (!out) {
    fprintf(stderr, "Error opening output.txt for writing.\n");
    fclose(in);
    return 1;
  }

  while ((c = fgetc(in)) != EOF) {
    fputc(c, out);
  }

  fclose(out);
  fclose(in);
  return 0;
}
```


A couple of remarks on the preceding example:

It uses <code>fgetc</code> to read one character at a time. Each character is visited, even though there's nothing to do with it. Copying bigger blocks of data is much more efficient.

The following example addresses those issues. To avoid buffered I/O, it uses ''open()'', ''read()'', ''write()'' and ''close()'', which are part of [[POSIX]].

{{works with|POSIX}}

```c
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

/* we just return a yes/no status; caller can check errno */
int copy_file(const char *in, const char *out)
{
	int ret = 0;
	int fin, fout;
	ssize_t len;
	char *buf[4096]; /* buffer size, some multiple of block size preferred */
	struct stat st;

	if ((fin  = open(in,  O_RDONLY)) == -1) return 0;
	if (fstat(fin, &st)) goto bail;

	/* open output with same permission */
	fout = open(out, O_WRONLY|O_CREAT|O_TRUNC, st.st_mode & 0777);
	if (fout == -1) goto bail;

	while ((len = read(fin, buf, 4096)) > 0)
		write(fout, buf, len);

	ret = len ? 0 : 1; /* last read should be 0 */

bail:	if (fin != -1)  close(fin);
	if (fout != -1) close(fout);
	return ret;
}

int main()
{
	copy_file("infile", "outfile");
	return 0;
}
```


If it's certain that mapping the whole input file into memory poses no problem (there can be all kinds of problems), this may be the most efficient:
```c
int copy_file(const char *in, const char *out)
{
	int ret = 0;
	int fin, fout;
	char *bi;
	struct stat st;

	if ((fin  = open(in,  O_RDONLY)) == -1) return 0;
	if (fstat(fin, &st)) goto bail;

	fout = open(out, O_WRONLY|O_CREAT|O_TRUNC, st.st_mode & 0777);
	if (fout == -1) goto bail;

	bi = mmap(0, st.st_size, PROT_READ, MAP_PRIVATE, fin,  0);

	ret = (bi == (void*)-1)
		? 0 : (write(fout, bi, st.st_size) == st.st_size);

bail:	if (fin != -1)  close(fin);
	if (fout != -1) close(fout);
	if (bi != (void*)-1) munmap(bi, st.st_size);
	return ret;
}
```


=={{header|C sharp|C#}}==

The long way:


```csharp
using System.IO;

using (var reader = new StreamReader("input.txt"))
using (var writer = new StreamWriter("output.txt"))
{
    var text = reader.ReadToEnd();
    writer.Write(text);
}
```


The short way:


```csharp
using System.IO;

var text = File.ReadAllText("input.txt");
File.WriteAllText("output.txt", text);
```



## C++

{{works with|g++|3.4.2}}

```cpp
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
    string line;
    ifstream input ( "input.txt" );
    ofstream output ("output.txt");

    if (output.is_open()) {
        if (input.is_open()){
            while (getline (input,line)) {
                output << line << endl;
            }
            input.close(); // Not necessary - will be closed when variable goes out of scope.
        }
        else {
            cout << "input.txt cannot be opened!\n";
        }
        output.close(); // Not necessary - will be closed when variable goes out of scope.
    }
    else {
        cout << "output.txt cannot be written to!\n";
    }
    return 0;
}
```


Simpler version:


```cpp
#include <iostream>
#include <fstream>
#include <cstdlib>

int main()
{
  std::ifstream input("input.txt");
  if (!input.is_open())
  {
    std::cerr << "could not open input.txt for reading.\n";
    return EXIT_FAILURE;
  }

  std::ofstream output("output.txt");
  if (!output.is_open())
  {
    std::cerr << "could not open output.txt for writing.\n";
    return EXIT_FAILURE;
  }

  output << input.rdbuf();
  if (!output)
  {
    std::cerr << "error copying the data.\n";
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
```


Using istream- and ostream- iterators:


```cpp
# include <algorithm>
# include <fstream>

int main() {
  std::ifstream ifile("input.txt");
  std::ofstream ofile("output.txt");
  std::copy(std::istreambuf_iterator<char>(ifile),
            std::istreambuf_iterator<char>(),
            std::ostreambuf_iterator<char>(ofile));
}
```


Even simpler way:


```cpp
#include <fstream>

int main()
{
  std::ifstream input("input.txt");
  std::ofstream output("output.txt");
  output << input.rdbuf();
}
```



## Clean

Define a function that copies the content from one file to another.


```clean
import StdEnv

copyFile fromPath toPath world
    # (ok, fromFile, world) = fopen fromPath FReadData world
    | not ok = abort ("Cannot open " +++ fromPath +++ " for reading")
    # (ok, toFile, world) = fopen toPath FWriteData world
    | not ok = abort ("Cannot open " +++ toPath +++ " for writing")
    # (fromFile, toFile) = copyData 1024 fromFile toFile
    # (ok, world) = fclose fromFile world
    | not ok = abort ("Cannot close " +++ fromPath +++ " after reading")
    # (ok, world) = fclose toFile world
    | not ok = abort ("Cannot close " +++ toPath +++ " after writing")
    = world
where
    copyData bufferSize fromFile toFile
        # (buffer, fromFile) = freads fromFile bufferSize
        # toFile = fwrites buffer toFile
        | size buffer < bufferSize = (fromFile, toFile) // we're done
        = copyData bufferSize fromFile toFile // continue recursively
```


Apply this function to the world to copy a file.


```clean
Start world = copyFile "input.txt" "output.txt" world
```



## Clojure


```lisp

(use 'clojure.java.io)

(copy (file "input.txt") (file "output.txt"))

```



```lisp

;; simple file writing
(spit "filename.txt" "your content here")

;; simple file reading
(slurp "filename.txt")

```



## COBOL


### COBOL 85

{{works with|COBOL 85 standard}}
Flags used for Micro Focus COBOL:
      $set ans85 flag"ans85" flagas"s" sequential"line"

```COBOL
       identification division.
       program-id. copyfile.
       environment division.
       input-output section.
       file-control.
           select input-file assign to "input.txt"
               organization sequential
           .
           select output-file assign to "output.txt"
               organization sequential
           .
       data division.
       file section.
       fd input-file.
       1 input-record pic x(80).
       fd output-file.
       1 output-record pic x(80).
       working-storage section.
       1 end-of-file-flag pic 9 value 0.
         88 eof value 1.
       1 text-line pic x(80).
       procedure division.
       begin.
           open input input-file
               output output-file
           perform read-input
           perform until eof
               write output-record from text-line
               perform read-input
           end-perform
           close input-file output-file
           stop run
           .
       read-input.
           read input-file into text-line
           at end
               set eof to true
           end-read
           .
       end program copyfile.
```


### Implementation

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. file-io.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT in-file ASSIGN "input.txt"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT OPTIONAL out-file ASSIGN "output.txt"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  in-file.
       01  in-line                 PIC X(256).

       FD  out-file.
       01  out-line                PIC X(256).

       PROCEDURE DIVISION.
       DECLARATIVES.
       in-file-error SECTION.
           USE AFTER ERROR ON in-file.
           DISPLAY "An error occurred while using input.txt."
           GOBACK
           .
       out-file-error SECTION.
           USE AFTER ERROR ON out-file.
           DISPLAY "An error occurred while using output.txt."
           GOBACK
           .
       END DECLARATIVES.

       mainline.
           OPEN INPUT in-file
           OPEN OUTPUT out-file

           PERFORM FOREVER
               READ in-file
                   AT END
                       EXIT PERFORM
               END-READ
               WRITE out-line FROM in-line
           END-PERFORM

           CLOSE in-file, out-file
           .
```


===Built-in Subroutines===
{{works with|OpenCOBOL}}
{{works with|Visual COBOL}}

```cobol
*> Originally from ACUCOBOL-GT
CALL "C$COPY" USING "input.txt", "output.txt", 0
```


```cobol
*> Originally from Micro Focus COBOL
CALL "CBL_COPY_FILE" USING "input.txt", "output.txt"
```



## ColdFusion



```cfm
<cfif fileExists(expandPath("input.txt"))>
  <cffile action="read" file="#expandPath('input.txt')#" variable="inputContents">
  <cffile action="write" file="#expandPath('output.txt')#" output="#inputContents#">
</cfif>
```



## Common Lisp


By lines:


```lisp
(with-open-file (in #p"input.txt" :direction :input)
  (with-open-file (out #p"output.txt" :direction :output)
    (loop for line = (read-line in nil 'foo)
          until (eq line 'foo)
          do (write-line line out))))
```


By arbitrary blocks and for possibly-binary files:


```lisp
(defconstant +buffer-size+ (expt 2 16))

(with-open-file (in #p"input.txt" :direction :input
                                :element-type '(unsigned-byte 8))
  (with-open-file (out #p"output.txt"
                   :direction :output
                   :element-type (stream-element-type in))
    (loop with buffer = (make-array +buffer-size+
                                    :element-type (stream-element-type in))
          for size = (read-sequence buffer in)
          while (plusp size)
          do (write-sequence buffer out :end size))))
```


If you're on an odd platform which actually stores text/binary/... type information for files and your CL implementation will use this information, then <tt>in</tt> should be opened with <tt>:element-type :default</tt>.


## D

{{libheader|Phobos}}
{{works with|D|2}}

```d
import std.file: copy;

void main() {
    copy("input.txt", "output.txt");
}
```


very plainly, with an intermediate variable:

```d

void main() {
import std.file;
auto data = std.file.read("input.txt");
std.file.write("output.txt", data);
}

```


via an intermediate buffer variable:

```d
import std.stdio;

int main() {
    auto from = File("input.txt", "rb");
    scope(exit) from.close();

    auto to = File("output.txt", "wb");
    scope(exit) to.close();

    foreach(buffer; from.byChunk(new ubyte[4096*1024])) {
        to.rawWrite(buffer);
    }

    return 0;
}
```


{{libheader|Tango}}
{{works with|D|1}}

Copy the content from one file to another (exceptions are handled by Tango):

```d
import tango.io.device.File;

void main()
{
    auto from = new File("input.txt");
    auto to = new File("output.txt", File.WriteCreate);
    to.copy(from).close;
    from.close;
}
```

Or a shorter example without explicitly closing the output file:

```d
import tango.io.device.File;

void main()
{
    auto to = new File("output.txt", File.WriteCreate);
    to.copy(new File("input.txt")).close;
}
```



## DCL


```DCL
$ open input input.txt
$ open /write output output.txt
$ loop:
$  read /end_of_file = done input line
$  write output line
$  goto loop
$ done:
$ close input
$ close output
```



## Delphi


Delphi supports both typed and untyped as well as a textfile type for files. Delphi provides a default 128 byte buffer for text files. This may be enlarged via a call to SetTextBuff(Var F: Text; Var Buf [Size : integer]) procedure. All other files have no buffer at all and it is the programmers option to do buffering.

The following file I/O procedures have existed since Turbo Pascal V-3.

 - Read(F,V1..Vn)
 - ReadLn(F,V1..Vn)
 - Write(F,V1[,V2..Vn])
 - WriteLn(f,V1[,V2..Vn])
 - BlockRead(F,Buff,BytesToRead[,BytesRead])
 - BlockWrite(F,Buff,BytesToRead[,BytesWritten])

Files are opened using:

AssignFile(f,{fully qualified path and file name})

-------

Assigns the file name to the file structure in preparation for opening.

Reset(f)

Opens and existing file. If it does not exist EIOError is raised.

-------

Rewrite(f)

Creates a new file and opens it for I/O. If the files exists is is overwritten.

Delphi implemented Streams of which a variant is TFileStream and are very closely related to the Windows API for file handling.

'''- Text File I/O -'''


```delphi
var
  f : TextFile ;
  s : string ;
begin
  AssignFile(f,[fully qualified file name);
  Reset(f);
  writeln(f,s);
  Reset(f);
  ReadLn(F,S);
  CloseFile(
end;
```



'''- Untyped File I/O -'''

This is perhaps one of the most powerful I/O functions built into Pascal. This will allow you to open and read a file of ANY type, regardless of structure, size or content. Note the usage of Reset(). This is using the optional size parameter that instructs the record size of file I/O. This could have been called with SizeOf(Buff) as the optional parameter but that would have limited flexibility. Calling it with a size of ONE byte allows you to adjust the buffer size on the fly, as conditions warrant. Also note the use of the BytesRead parameter. When included in the BlockRead() function it will return the number of bytes actually read. If this is not included, then if your directive to read n bytes is greater then the size of the file, the EOF will be encountered unexpectedly and EIOError will be raised.


```delphi
var
  f         : File ;
  buff      : array[1.1024] of byte ;
  BytesRead : Integer ;
begin
  AssignFile(f,fully qualified file name);
  Reset(f,1);
  Blockread(f,Buff,SizeOf(Buff),BytesRead);
  CloseFile(f);
end;
```


'''- Typed File I/O -'''

Typed file I/O is very useful when reading and writing structures. An Address List is quiet easy to write when using this type of I/O.  The same file procedures are used with some subtle differences. Bite below in the blockread and blockwrite procedures that the bytes to read or write are 1. Also note that the reset procedure is not called with a buffer size. When performing '''Typed File I/O''' the size of the type definition is the buffer size. In the BlockRead() and BlockWrite() procedures I elected to read '''one record'''. Had I declared a very large buffer of type tAddressBook of say 500 records, I could have set bytes to read as SizeOf(Buffer) thereby reading a minimum of 500 records.


```delphi
type

  tAddressBook = Record
                  FName   : string[20];
                  LName   : string[30];
                  Address : string[30];
                  City    : string[30];
                  State   : string[2];
                  Zip5    : string[5];
                  Zip4    : string[4];
                  Phone   : string[14];
                  Deleted : boolean ;
                end;

var
  f     : file of tAddressBook ;
  v     : tAddressBook ;
  bytes : integer ;
begin
  AssignFile(f,fully qualified file name);
  Reset(f);
  Blockread(f,V,1,Bytes);
  Edit(v);
  Seek(F,FilePos(f)-1);
  BlockWrite(f,v,1,bytes);
  CloseFile(f);
end;
```



## E

{{works with|E-on-Java}}

```e><file:output.txt
.setText(<file:input.txt>.getText())
```


(This version holds the entire contents in memory.)


## Eiffel



```eiffel
class
    APPLICATION

create
    make

feature {NONE} -- Initialization

    make
            -- Run application.
        do
            create input_file.make_open_read ("input.txt")
            create output_file.make_open_write ("output.txt")

            from
                input_file.read_character
            until
                input_file.exhausted
            loop
                output_file.put (input_file.last_character)
                input_file.read_character
            end

            input_file.close
            output_file.close
        end

feature -- Access

    input_file: PLAIN_TEXT_FILE
    output_file: PLAIN_TEXT_FILE

end
```


## Elena

ELENA 4.x :

```elena
import system'io;

public program()
{
    var text := File.assign("input.txt").readContent();

    File.assign("output.txt").saveContent(text);
}
```



## Elixir

Read in the whole file and write the contents to a new file.

```Elixir
defmodule FileReadWrite do
  def copy(path,new_path) do
    case File.read(path) do
      # In case of success, write to the new file
      {:ok, body} ->
        # Can replace with :write! to generate an error upon failure
        File.write(new_path,body)
      # If not successful, raise an error
      {:error,reason} ->
        # Using Erlang's format_error to generate error string
        :file.format_error(reason)
    end
  end
end

FileReadWrite.copy("input.txt","output.txt")
```


'''Built in function:'''

```Elixir
File.cp!("input.txt", "output.txt")
```



## Euphoria


### Read the entire file and then write it

{{works with|Euphoria|4.0.0}}

```euphoria
include std/io.e
write_lines("output.txt", read_lines("input.txt"))
```


===Line-by-line reading and writing===
{{works with|Euphoria|any}}

```euphoria
integer in,out
object line

in = open("input.txt","r")
out = open("output.txt","w")

while 1 do
    line = gets(in)
    if atom(line) then -- EOF reached
        exit
    end if
    puts(out,line)
end while

close(out)
close(in)
```



## Erlang



```erlang

-module( file_io ).

-export( [task/0] ).

task() ->
       {ok, Contents} = file:read_file( "input.txt" ),
       ok = file:write_file( "output.txt", Contents ).

```


=={{header|F Sharp|F#}}==
Using an intermediate variable for the input file content is not ideomatic in functional programming. Nevertheless...


```fsharp
open System.IO

let copyFile fromTextFileName toTextFileName =
    let inputContent = File.ReadAllText fromTextFileName
    inputContent |> fun text -> File.WriteAllText(toTextFileName, text)

[<EntryPoint>]
let main argv =
    copyFile "input.txt" "output.txt"
    0

```



## Factor

Holds entire file content in memory:

```factor
"input.txt" binary file-contents
"output.txt" binary set-file-contents
```

A bit longer, but only holds a small amount of data in memory.  If opening the file for writing fails, we want to clean up the file that's open for reading:

```factor
[
    "input.txt" binary <file-reader> &dispose
    "output.txt" binary <file-writer> stream-copy
] with-destructors

```

Possibly cheating:

```factor
"input.txt" "output.txt" copy-file
```



## Forth


Forth traditionally has not had any file handling capabilities, preferring instead to operate on a disk image block by block.  Most modern Forth systems however run under an existing operating system and provide methods for disk access.


```forth>\ <to> <from
 copy-file
: copy-file ( a1 n1 a2 n2 -- )
    r/o open-file throw >r
    w/o create-file throw r>
    begin
        pad maxstring  2 pick  read-file throw
    ?dup while
        pad swap  3 pick  write-file throw
    repeat
    close-file throw
    close-file throw ;

\ Invoke it like this:
s" output.txt" s" input.txt" copy-file
```


Note the use of "2 pick" to get the input file handle and "3 pick" to get the output file handle. Local or global variables could have been used, but in this implementation simple stack manipulation was chosen. Also, only maxstring bytes are copied at a time, and the global "pad" memory area is used to hold the data. For faster copies, allocating a larger buffer could be advantageous.

Also, abort" can be used instead of throw if desired.

A good practice is to ask the user the file name he wants to create like in this short example
<lang>: INPUT$ ( text -- n n )
  pad swap accept pad swap ;
cr ." Enter file name : " 20 INPUT$ w/o create-file throw Value fd-out
: get-content cr ." Enter your nickname : " 20 INPUT$ fd-out write-file cr ;
: close-output ( -- )  fd-out close-file throw ;
get-content
\ Inject a carriage return at end of file
s\" \n" fd-out write-file
close-output
bye
```



## Fortran

{{works with|Fortran|2003}}
It uses the <tt>access="stream"</tt> which is defined in Fortran 2003 standard and should allow to "copy" also binary data easily.


```fortran
program FileIO

  integer, parameter :: out = 123, in = 124
  integer :: err
  character :: c

  open(out, file="output.txt", status="new", action="write", access="stream", iostat=err)
  if (err == 0) then
     open(in, file="input.txt", status="old", action="read", access="stream", iostat=err)
     if (err == 0) then
        err = 0
        do while (err == 0)
           read(unit=in, iostat=err) c
           if (err == 0) write(out) c
        end do
        close(in)
     end if
     close(out)
  end if

end program FileIO
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

/'
input.txt contains:

The quick brown fox jumps over the lazy dog.
Empty vessels make most noise.
Too many chefs spoil the broth.
A rolling stone gathers no moss.
'/

Open "output.txt" For Output As #1
Open "input.txt" For Input As #2
Dim line_ As String ' note that line is a keyword

While Not Eof(2)
  Line Input #2, line_
  Print #1, line_
Wend

Close #2
Close #1
```


{{out}}

```txt

output.txt contains:

The quick brown fox jumps over the lazy dog.
Empty vessels make most noise.
Too many chefs spoil the broth.
A rolling stone gathers no moss.

```



## Frink


```frink

contents = read["file:input.txt"]
w = new Writer["output.txt"]
w.print[contents]
w.close[]

```



## Gambas


```gambas
Public Sub Main()
Dim sOutput As String = "Hello "
Dim sInput As String = File.Load(User.Home &/ "input.txt") 'Has the word 'World!' stored

File.Save(User.Home &/ "output.txt", sOutput)
File.Save(User.Home &/ "input.txt", sOutput & sInput)

Print "'input.txt' contains - " & sOutput & sInput
Print "'output.txt' contains - " & sOutput

End
```

Output:

```txt

'input.txt' contains - Hello World!
'output.txt' contains - Hello

```



## GAP


```gap
CopyFile := function(src, dst)
  local f, g, line;
  f := InputTextFile(src);
  g := OutputTextFile(dst, false);
  while true do
    line := ReadLine(f);
    if line = fail then
      break
    else
      WriteLine(g, Chomp(line));
    fi;
  od;
  CloseStream(f);
  CloseStream(g);
end;
```



## GML



```GML
var file, str;
file = file_text_open_read("input.txt");
str = "";
while (!file_text_eof(file))
    {
    str += file_text_read_string(file);
    if (!file_text_eof(file))
        {
        str += "
"; //It is important to note that a linebreak is actually inserted here rather than a character code of some kind
        file_text_readln(file);
        }
    }
file_text_close(file);

file = file_text_open_write("output.txt");
file_text_write_string(file,str);
file_text_close(file);
```



## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    b, err := ioutil.ReadFile("input.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = ioutil.WriteFile("output.txt", b, 0666); err != nil {
        fmt.Println(err)
    }
}
```

Alternative solution is not a one-liner, but is one of "secondary interest" that copies data from one file to another without an intermediate variable.

```go
package main

import (
    "io"
    "log"
    "os"
)

func CopyFile(out, in string) (err error) {
    var inf, outf *os.File
    inf, err = os.Open(in)
    if err != nil {
        return
    }
    defer func() {
        cErr := inf.Close()
        if err == nil {
            err = cErr
        }
    }()
    outf, err = os.Create(out)
    if err != nil {
        return
    }
    _, err = io.Copy(outf, inf)
    cErr := outf.Close()
    if err == nil {
        err = cErr
    }
    return
}

func main() {
    if err := CopyFile("output.txt", "input.txt"); err != nil {
        log.Fatal(err)
    }
}
```



## Groovy


Using File

```groovy
new File('output.txt').write(new File('input.txt').text)
```


Using Ant

```groovy
new AntBuilder().copy(file:'input.txt', toFile:'output.txt', overwrite:true)
```


Buffered

```groovy
new File('output.txt').withWriter( w ->
  new File('input.txt').withReader( r -> w << r }
}
```



## GUISS



```guiss
Start,My Documents,Rightclick:input.txt,Copy,Menu,Edit,Paste,
Rightclick:Copy of input.txt,Rename,Type:output.txt[enter]
```



## Haskell

Note: this doesn't keep the file in memory. Buffering is provided by lazy evaluation.

```haskell
main = readFile "input.txt" >>= writeFile "output.txt"
```



## hexiscript


```hexiscript
let in  openin "input.txt"
let out openout "output.txt"
while !(catch (let c read char in))
  write c out
endwhile
close in; close out
```



## HicEst

Copy via system call:

```hicest
CHARACTER input='input.txt ', output='output.txt ', c, buffer*4096
SYSTEM(COPY=input//output, ERror=11) ! on error branch to label 11 (not shown)
```

Read and write line by line

```hicest
OPEN(FIle=input, OLD, ERror=21) ! on error branch to label 21 (not shown)
OPEN(FIle=output)
    DO i = 1, 1E300 ! "infinite" loop, exited on end-of-file error
      READ( FIle=input,  ERror=22) buffer ! on error (end of file) branch to label 22
      WRITE(FIle=output, ERror=23) buffer ! on error branch to label 23 (not shown)
    ENDDO
22  WRITE(FIle=output, CLoSe=1)
```

Read and write in 1 block

```hicest
OPEN(FIle=input, SEQuential, UNFormatted, OLD, LENgth=len, ERror=31) ! on error branch to label 31 (not shown)
OPEN(FIle=output, SEQuential, UNFormatted, ERror=32) ! on error branch to label 32 (not shown)
ALLOCATE(c, len)
READ(FIle=input, CLoSe=1) c
WRITE(FIle=output, CLoSe=1) c END
```



## i


```i
software {
	file = load("input.txt")
	open("output.txt").write(file)
}
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon I/O by default is line driven.  This can be changed with options in open and by the use of reads() and writes().

```Icon
procedure main()
in  := open(f := "input.txt","r") | stop("Unable to open ",f)
out := open(f := "output.txt","w") | stop("Unable to open ",f)
while write(out,read(in))
end
```



## IDL



```idl
; open two LUNs
openw,unit1,'output.txt,/get
openr,unit2,'input.txt',/get
; how many bytes to read
fs = fstat(unit2)
; make buffer
buff = bytarr(fs.size)
; transfer content
readu,unit2,buff
writeu,unit1,buff
; that's all
close,/all
```




## Io



```io
inf := File with("input.txt") openForReading
outf := File with("output.txt") openForUpdating

while(l := inf readLine,
  outf write(l, "\n")
)

inf close
outf close

```



## J



```j
 'output.txt' (1!:2~ 1!:1)&< 'input.txt'
```


Or using the system library <tt>files</tt>:


```j
require 'files'
'output.txt' (fwrite~ fread) 'input.txt'
```


Note that J will read as many characters from the file as the system reports, for the size of the file. So if the system reports that the file is empty when it is not, J will return an empty result when using this file reading mechanism. (This can happen for "files" which really represent a connection to something else. When this happens, it's usually better to dedicate a [[Execute_a_system_command#J|separate process]] to reading the file.)


## Java

{{works with|GCJ|4.1.2}}

Simple version; Files ''may'' be closed automatically by OS, on some systems.


```java
import java.io.*;

public class FileIODemo {
  public static void main(String[] args) {
    try {
      FileInputStream in = new FileInputStream("input.txt");
      FileOutputStream out = new FileOutputStream("ouput.txt");
      int c;
      while ((c = in.read()) != -1) {
        out.write(c);
      }
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e){
      e.printStackTrace();
    }
  }
}
```


This version closes both files after without OS intervention.


```java
import java.io.*;

public class FileIODemo2 {
  public static void main(String args[]) {
    try {
      // Probably should wrap with a BufferedInputStream
      final InputStream in = new FileInputStream("input.txt");
      try {
        // Probably should wrap with a BufferedOutputStream
        final OutputStream out = new FileOutputStream("output.txt");
        try {
          int c;
          while ((c = in.read()) != -1) {
            out.write(c);
          }
        }
        finally {
          out.close();
        }
      }
      finally {
        in.close();
      }
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e){
      e.printStackTrace();
    }
  }
}
```


{{works with|Java|1.4}}
'''Package''' [[nio]]


```java
import java.io.*;
import java.nio.channels.*;

public class FileIODemo3 {
  public static void main(String args[]) {
    try {
      final FileChannel in = new FileInputStream("input.txt").getChannel();
      try {
        final FileChannel out = new FileOutputStream("output.txt").getChannel();
        try {
          out.transferFrom(in, 0, in.size());
        }
        finally {
          out.close();
        }
      }
      finally {
        in.close();
      }
    }
    catch (Exception e) {
      System.err.println("Exception while trying to copy: "+e);
      e.printStackTrace(); // stack trace of place where it happened
    }
  }
}
```


This version is more in line with the other languages' implementations: it assumes simple text files, and doesn't worry too much about errors (just throws them out to the caller, the console in this case). It's shorter and simpler and shows that simple programs can be simple to write, in Java as well.


```java
import java.io.*;
public class Test {
  public static void main (String[] args) throws IOException {
    BufferedReader br = new BufferedReader(new FileReader("input.txt"));
    BufferedWriter bw = new BufferedWriter(new FileWriter("output.txt"));
    String line;
    while ((line = br.readLine()) != null) {
      bw.write(line);
      bw.newLine();
    }
    br.close();
    bw.close();
  }
}
```

{{works with|Java|7+}}

```java5
import java.nio.file.*;
public class Copy{
   public static void main(String[] args) throws Exception{
      FileSystem fs = FileSystems.getDefault();
      Path in = fs.getPath("input.txt");
      Path out = fs.getPath("output.txt");
      Files.copy(in, out, StandardCopyOption.REPLACE_EXISTING);
   }
}
```



## JavaScript

{{works with|JScript}}

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading = 1, ForWriting = 2;
var f_in = fso.OpenTextFile('input.txt', ForReading);
var f_out = fso.OpenTextFile('output.txt', ForWriting, true);

// for small files:
// f_out.Write( f_in.ReadAll() );

while ( ! f_in.AtEndOfStream) {
    // ReadLine() does not include the newline char
    f_out.WriteLine( f_in.ReadLine() );
}

f_in.Close();
f_out.Close();
```



{{works with|Node.js}}

```javascript

var fs = require('fs');
require('util').pump(fs.createReadStream('input.txt', {flags:'r'}), fs.createWriteStream('output.txt', {flags:'w+'}));

```



## jq

If the input file consists of ordinary lines of text, then the lines can be copied verbatim, one by one, as follows:

```jq
jq -M --raw-input --raw-output '. as $line | $line' input.txt > output.txt

```


If the input file consists of JSON entities, and if we wish to "pretty print" each, then the following will suffice:
```jq

jq -M '. as $line | $line' input.txt > output.txt

```



## Julia

Here we read the content of file1 into the variable mystring. Then we write the content of string to file2.

```Julia
mystring = read("file1", String)
open(io->write(io, mystring), "file2", "w")
```

Note however that Julia has a `cp` function to copy the content of a file to another file.

```julia
cp("file1","file2")
```

We can also open and close the file handles manually.

```Julia
infile = open("file1", "r")
outfile = open("file2", "w")
write(outfile, read(infile, String))
close(outfile)
close(infile)
```

Here is a one-liner that guarantees that the file handle is closed
even if something goes wrong during the read/write phase.

```Julia
open(IO ->write(IO, read("file1", String)), "file2", "w")
```



## K


```K
`output.txt 0:0:`input.txt
```



## Kotlin



```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    val text = File("input.txt").readText()
    File("output.txt").writeText(text)
}
```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW File IO.png]]


## Lang5


```lang5
: puts(*)  . "\n" . ;
: set-file  '> swap open ;
: >>contents  slurp puts ;
: copy-file
    swap set-file 'fdst set fdst fout >>contents fdst close ;

'output.txt 'input.txt copy-file
```



## Liberty BASIC


```lb
nomainwin

    open "input.txt" for input as #f1
        qtyBytes = lof( #f1)
        source$  = input$( #f1, qtyBytes)
    close #f1

    open "output.txt" for output as #f2
        #f2 source$;
    close #f2

    end
```



## Lingo


```lingo
----------------------------------------
-- Returns file as ByteArray
-- @param {string} tFile
-- @return {byteArray|false}
----------------------------------------
on getBytes (tFile)
  fp = xtra("fileIO").new()
  fp.openFile(tFile, 1)
  if fp.status() then return false
  data = fp.readByteArray(fp.getLength())
  fp.closeFile()
  return data
end

----------------------------------------
-- Saves ByteArray to file
-- @param {string} tFile
-- @param {byteArray} tString
-- @return {bool} success
----------------------------------------
on putBytes (tFile, tByteArray)
  fp = xtra("fileIO").new()
  fp.openFile(tFile, 2)
  err = fp.status()
  if not (err) then fp.delete()
  else if (err and not (err = -37)) then return false
  fp.createFile(tFile)
  if fp.status() then return false
  fp.openFile(tFile, 2)
  if fp.status() then return false
  fp.writeByteArray(tByteArray)
  fp.closeFile()
  return true
end
```



```lingo
data = getBytes("input.txt")
putBytes("output.txt", data)
```



## Lisaac


```Lisaac
Section Header

+ name := FILE_IO;

Section Public

- main <- (
  + e : ENTRY;
  + f : STD_FILE;
  + s : STRING;

  e := FILE_SYSTEM.get "input.txt";
  (e != NULL).if {
    f ?= e.open_read_only;
    (f != NULL).if {
      s := STRING.create(f.size);
      f.read s size (f.size);
      f.close;
    };
  };

  (s != NULL).if {
    e := FILE_SYSTEM.make_file "output.txt";
    (e != NULL).if {
      f ?= e.open;
      (f != NULL).if {
        f.write s from (s.lower) size (s.count);
        f.close;
      };
    };
  };
);
```



## Logo

{{works with|UCB Logo}}

```logo
to copy :from :to
  openread :from
  openwrite :to
  setread :from
  setwrite :to
  until [eof?] [print readrawline]
  closeall
end

copy "input.txt "output.txt
```


## Lua


```lua

inFile  = io.open("input.txt", "r")
data = inFile:read("*all") -- may be abbreviated to "*a";
                           -- other options are "*line",
                           -- or the number of characters to read.
inFile:close()

outFile = io.open("output.txt", "w")
outfile:write(data)
outfile:close()

-- Oneliner version:
io.open("output.txt", "w"):write(io.open("input.txt", "r"):read("*a"))

```



## M2000 Interpreter


### Using Document Object

We can use Open for input/ Input$()/close, Open for Output/Print/close, but using a Document object is better, because Load.Doc can find the type of line breack and the coding, if it is UTF-8, UTG-16LE/BE and Ansi (we can provide another argument with specific way to open, or a Locale id for Ansi). When we save the document, we use the same format, so we preserve the coding and the line break type.

Open statement works for ANSI and using Wide for UTF-16LE only.

Here we use Edit to make Input.txt and edit some lines of text. Pressing Esc text saved to disk. Using Load.Doc we get the Input.txt, and we can display it using Report (which render text with word wrap, in M2000 console, using proportional text rendering), also if we have many lines there is a stop in each 3/4 of scrolling lines, to wait for a space bar or mouse click.
After a wait for a keypress Doc$ saved to Output.txt, and we open it in editor.

We can use Edit.Doc to edit Doc$, without save and then open for edit.

We can edit thousands of lines. Document is a double linked list.


```M2000 Interpreter

Module FileInputOutput {
      Edit "Input.txt"
      Document Doc$
      Load.Doc Doc$, "Input.txt"
      Report Doc$
      Print "Press a key:";Key$
      Save.Doc Doc$, "Output.txt"
      Edit "Output.txt"
}
FileInputOutput

```


### Using Buffer Object

A buffer expose real pointer (address), so here M(0) is the address of first byte, and Len(m) is the size of buffer in bytes. This buffer is not for code, but for data (no execution allowed).


```M2000 Interpreter

Module Using_Buffer {
      M=buffer("Input.txt")
      Print Len(m)
      Open "Output1.txt" For Wide Output as #F
      Print #F, Eval$(M);
      Close #F
      Edit "Output1.txt"
      z=Filelen("Output1.txt")
      Print z
      Open "OutputAscii.txt" For Output as #F
      Print #F, Eval$(M);
      Close #F
      Print Filelen("OutputAscii.txt")=z/2
      Edit "OutputAscii.txt"
}
Using_Buffer

```



## MAXScript


```maxscript
inFile = openFile "input.txt"
outFile = createFile "output.txt"
while not EOF inFile do
(
    format "%" (readLine inFile) to:outFile
)
close inFile
close outFile
```




## Maple


```Maple

inout:=proc(filename)
local f;
f:=FileTools[Text][ReadFile](filename);
FileTools[Text][WriteFile]("output.txt",f);
end proc;

```



## Mathematica


```Mathematica
SetDirectory@NotebookDirectory[];
If[FileExistsQ["output.txt"], DeleteFile["output.txt"], Print["No output yet"] ];
CopyFile["input.txt", "output.txt"]
```



## Mercury


```mercury
:- module file_io.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
   io.open_input("input.txt", InputRes, !IO),
   (
       InputRes = ok(Input),
       io.read_file_as_string(Input, ReadRes, !IO),
       (
           ReadRes = ok(Contents),
           io.close_input(Input, !IO),
           io.open_output("output.txt", OutputRes, !IO),
           (
                OutputRes = ok(Output),
                io.write_string(Output, Contents, !IO),
                io.close_output(Output, !IO)
           ;
                OutputRes = error(OutputError),
                print_io_error(OutputError, !IO)
           )
       ;
           ReadRes = error(_, ReadError),
           print_io_error(ReadError, !IO)
       )
    ;
       InputRes = error(InputError),
       print_io_error(InputError, !IO)
    ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.set_exit_status(1, !IO).
```



## mIRC Scripting Language


{{works with|mIRC}}

```mirc
alias Write2FileAndReadIt {
.write myfilename.txt Goodbye Mike!
.echo -a Myfilename.txt contains: $read(myfilename.txt,1)
}
```


=={{header|Modula-3}}==

```modula3
MODULE FileIO EXPORTS Main;

IMPORT IO, Rd, Wr;

<*FATAL ANY*>

VAR
  infile: Rd.T;
  outfile: Wr.T;
  txt: TEXT;

BEGIN
  infile := IO.OpenRead("input.txt");
  outfile := IO.OpenWrite("output.txt");
  txt := Rd.GetText(infile, LAST(CARDINAL));
  Wr.PutText(outfile, txt);
  Rd.Close(infile);
  Wr.Close(outfile);
END FileIO.
```


The code <code><*FATAL ANY*></code> is a pragma that tells the program to die if any exceptions (such as read/write errors) occur.


## Nanoquery

{{trans|Ursa}}

```nanoquery
import Nanoquery.IO

$input = new("File", "input.txt")
$output = new("File")

$output.create("output.txt")
$output.open("output.txt")

$contents = $input.readAll()
$output.write($contents)
```



## NetRexx

{{works with|Java|7}}
Takes advantage of some of the new path and file handling features of [[Java|Java's]] <tt>java.nio</tt> library.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.nio.

parse arg infileName outfileName .

if infileName  = '' | infileName.length  = 0 then infileName  = 'data/input.txt'
if outfileName = '' | outfileName.length = 0 then outfileName = 'data/output.txt'

binaryCopy(infileName, outfileName)

return

method binaryCopy(infileName, outfileName) public static

  do
    infile  = Paths.get('.', [String infileName])
    outfile = Paths.get('.', [String outfileName])
    fileOctets = Files.readAllBytes(infile)
    Files.write(outfile, fileOctets, [StandardOpenOption.WRITE, StandardOpenOption.CREATE])

  catch ioex = IOException
    ioex.printStackTrace()
  end

  return

```



## Nim

Copying the file directly (without buffer):

```nim
import os
copyfile("input.txt", "output.txt")
```


Buffer for the entire file:

```nim
let x = readFile("input.txt")
writeFile("output.txt", x)
```


Line by line:

```nim
var
  i = open("input.txt")
  o = open("output.txt", fmWrite)

for line in i.lines:
  o.writeln(line)

i.close()
o.close()
```


With a fixed sized buffer:

```nim
const size = 4096

var
  i = open("input.txt")
  o = open("output.txt", fmWrite)
  buf: array[size, char]

while i.readBuffer(buf.addr, size) > 0:
  discard o.writeBuffer(buf.addr, size)

i.close()
o.close()
```


Using memory mapping:

```nim
import memfiles

var
  i = memfiles.open("input.txt")
  o = system.open("output.txt", fmWrite)

var written = o.writeBuffer(i.mem, i.size)
assert(written == i.size)

i.close()
o.close()
```



## Objeck


```objeck
use IO;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      len := File->Size("input.txt");
      buffer := Byte->New[len];
      in := FileReader->New("input.txt");
      if(in->IsOpen() <> Nil) {
        in->ReadBuffer(0, len, buffer);
        out := FileWriter->New("output.txt");
        if(out->IsOpen() <> Nil) {
          out->WriteBuffer(0, len, buffer);
          out->Close();
        };
        in->Close();
      };
    }
  }
}
```


=={{header|Objective-C}}==

For copying files, using <code>NSFileManager</code> is preferred:


```objc
[[NSFileManager defaultManager] copyItemAtPath:@"input.txt" toPath:@"output.txt" error:NULL];
```


If you want to do it manually:


```objc
NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];

[data writeToFile:@"output.txt" atomically:YES];
```


Displayed without error checking to make it more clear. In real code you will need to add lot of error checking code, and maybe use <tt>dataWithContentsOfFile:error:</tt> if you want to get error information on failure. However, this code will mostly work correctly even if input does not exist or is not accessible. <tt>dataWithContentsOfFile:</tt> will return nil, and sending nil the message <tt>writeTofile:atomically:</tt> does nothing :-)

The second argument (<tt>atomically:YES</tt>) write the content to a temporary file, and rename the temporary file to the destination file, replacing existing file.


## Object Pascal

For procedural code see the [[File_IO#Delphi | Delphi]] code, which is perfectly fine in ObjectPascal.

For a more object oriented style one can use a TFilestream:


```pascal
uses
  classes;
begin
  with TFileStream.Create('input.txt', fmOpenRead) do
  try
    SaveToFile('output.txt');
  finally
    Free;
  end;
end;
```



## OCaml

By line:

```ocaml
let () =
  let ic = open_in "input.txt" in
  let oc = open_out "output.txt" in
  try
    while true do
      let s = input_line ic in
      output_string oc s;
      output_char oc '\n';
    done
  with End_of_file ->
    close_in ic;
    close_out oc;
;;
```


By character:

```ocaml
let () =
  let ic = open_in "input.txt" in
  let oc = open_out "output.txt" in
  try
    while true do
      let c = input_char ic in
      output_char oc c
    done
  with End_of_file ->
    close_in ic;
    close_out oc;
;;
```


(Notice that ic and oc, of type ''in_channel'' and ''out_channel'', are buffered)


## Octave


```octave

in = fopen("input.txt", "r", "native");
out = fopen("output.txt", "w","native");
if (in == -1)
  disp("Error opening input.txt for reading.");
else
if (out == -1)
  disp("Error opening output.txt for writing.");
else
while (1)
  [val,count]=fread(in,1,"uchar",0,"native");
  if (count > 0)
    count=fwrite(out,val,"uchar",0,"native");
    if (count == 0)
      disp("Error writing to output.txt.");
    end
  else
    break;
  end
endwhile
end
end
if (in != -1)
  fclose(in);
end
if (out != -1)
  fclose(out);
end

```



## Oforth



```Oforth
: fcopy(in, out)
| f g |
   File newMode(in,  File.BINARY) dup open(File.READ) ->f
   File newMode(out, File.BINARY) dup open(File.WRITE) ->g

   while(f >> dup notNull) [ g addChar ] drop
   f close g close ;
```


Usage :

```Oforth
fcopy("input.txt", "output.txt")
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>COPY-LOB FROM FILE "input.txt" TO FILE "output.txt".
```



## Oz


```oz
declare
  class TextFile from Open.file Open.text end

  In = {New TextFile init(name:"input.txt")}
  Out = {New TextFile init(name:"output.txt" flags:[write text create truncate])}

  proc {CopyAll In Out}
     case {In getS($)} of false then skip
     [] Line then
        {Out putS(Line)}
        {CopyAll In Out}
     end
  end
in
  {CopyAll In Out}
  {Out close}
  {In close}
```



## PARI/GP


```parigp
f=read("filename.in");
write("filename.out", f);
```



## Pascal

The [http://wiki.freepascal.org/File_Handling_In_Pascal | FreePascal wiki] gives a detailed description.
For procedureal code see the [[File_IO#Delphi | Delphi]] examples. The [[File_IO#Object_Pascal | ObjectPascal]] example is more OO coding style.


## Perl


{{works with|Perl|5.8.8}}

```perl
#!/usr/bin/perl

open my $fh_in, '<', 'input.txt' or die "could not open <input.txt> for reading: $!";
open my $fh_out, '>', 'output.txt' or die "could not open <output.txt> for writing: $!";
# '>' overwrites file, '>>' appends to file, just like in the shell

binmode $fh_out; # marks filehandle for binary content on systems where that matters

print $fh_out $_ while <$fh_in>;
# prints current line to file associated with $fh_out filehandle

# the same, less concise
#while (<$fh_in>) {
#  print $fh_out $_;
#};

close $fh_in;
close $fh_out;
```


Perl has also a powerful mechanism in conjunction with opening files called IO disciplines. It allows you to automatically apply chainable transformations on the input and output. Mangling newlines, gzip (de)compression and character encoding are the most used examples.


## Perl 6


If it is okay to have a temporary copy of the entire file in memory:

{{works with|Rakudo|2016.07}}

```perl6
spurt "output.txt", slurp "input.txt";
```


Otherwise, copying line-by line:

{{works with|Rakudo|2015.12}}

```perl6
my $in = open "input.txt";
my $out = open "output.txt", :w;
for $in.lines -> $line {
    $out.say: $line;
}
$in.close;
$out.close;
```



## Phix

whole file as a single string (safe on small binary files)

```Phix
integer fn = open("input.txt","rb")
string txt = get_text(fn)
    close(fn)
    fn = open("output.txt","wb")
    puts(fn,txt)
    close(fn)
```

line-by-line (text files only)

```Phix
integer infn = open("input.txt","r"),
        outfn = open("output.txt","w")
object line
    while 1 do
        line = gets(infn)
        if atom(line) then exit end if
        puts(outfn,line)
    end while
    close(infn)
    close(outfn)
```

byte-by-byte (safe on binary files)

```Phix
integer byte,
        infd = open("input.txt","rb"),
        outfd = open("output.txt","wb")
    while 1 do
        byte = getc(infd)
        if byte=-1 then exit end if
        puts(outfd,byte)
    end while
    close(infd)
    close(outfd)
```



## PHP


{{works with|PHP|4}}

```php
<?php

if (!$in = fopen('input.txt', 'r')) {
    die('Could not open input file.');
}

if (!$out = fopen('output.txt', 'w')) {
    die('Could not open output file.');
}

while (!feof($in)) {
    $data = fread($in, 512);
    fwrite($out, $data);
}

fclose($out);
fclose($in);
?>
```


{{works with|PHP|5}}

```php
<?php
if ($contents = file_get_contents('input.txt')) {
    if (!file_put_contents('output.txt', $contents)) {
        echo('Could not write output file.');
    }
} else {
    echo('Could not open input file.');
}
?>
```



## PicoLisp


### Using a variable


```PicoLisp
(let V (in "input.txt" (till))
   (out "output.txt" (prin V)) )
```


### Skipping intermediate variable


```PicoLisp
(in "input.txt"
   (out "output.txt"
      (echo) ) )
```



## PL/I


```pli

declare in file, out file;

open file (in) title ('/INPUT.TXT,type(text),recsize(100)') input;
open file (out) title ('/OUTPUT.TXT,type(text),recsize(100') output;
do forever;
   get file (in)  edit (line) (L);
   put file (out) edit (line) (A);
end;

```



## Pop11


Char by char copy:


```pop11
lvars i_stream = discin('input.txt');
lvars o_stream = discout('output.txt');
lvars c;
while (i_stream() ->> c) /= termin do
    o_stream(c);
endwhile;
```


Low level block copy:


```pop11
lvars i_file = sysopen('input.txt', 0, true);
lvars o_file = syscreate('output.txt', 1, true);
lvars buff = inits(4096);
lvars i;
while (sysread(i_file, buff, length(buff)) ->> i) > 0 do
    syswrite(o_file, buff, i);
endwhile;
```



## PowerShell


Read the input file then pipe it's contents to output file.
Assumes that the files are in the same folder that the script is executing in.

```PowerShell
Get-Content $PWD\input.txt | Out-File $PWD\output.txt
```


Using an alternate cmdlet to write the file

```PowerShell
Get-Content $PWD\input.txt | Set-Content $PWD\output.txt
```



## PureBasic



Basic file copy

```PureBasic
CopyFile("input.txt","output.txt")
```



Line by line

```PureBasic
in = ReadFile(#PB_Any,"input.txt")
If in
  out = CreateFile(#PB_Any,"output.txt")
  If out
    Define MyLine$
    While Not Eof(in)
      MyLine$ = ReadString(in)
      WriteString(out,MyLine$)
    Wend
    CloseFile(out)
  EndIf
  CloseFile(in)
EndIf
```



Reading & writing the complete file in one pass

```PureBasic
If ReadFile(0,"input.txt")
  Define MyLine$, *Buffer, length
  length=FileSize("input.txt")
  *Buffer = AllocateMemory(length)
  If *Buffer
    If OpenFile(1,"output.txt")
      ReadData(0, *Buffer, length)
      WriteData(1, *Buffer, length)
      CloseFile(1)
    EndIf
    FreeMemory(*Buffer)
  EndIf
  CloseFile(0)
EndIf
```



## Python



The following use of the standard libraries shutil.copyfile is to be preferred. (Current source code ensures that failure to open files raises appropriate exceptions, a restricted buffer is used to copy the files using binary mode, and any used file descriptors are always closed).


```python
import shutil
shutil.copyfile('input.txt', 'output.txt')
```


However the following example shows how one would do file I/O of other sorts:


```python
infile = open('input.txt', 'r')
outfile = open('output.txt', 'w')
for line in infile:
   outfile.write(line)
outfile.close()
infile.close()
```


This does no error checking.  A more robust program would wrap each open with exception handling blocks:


```python
import sys
try:
    infile = open('input.txt', 'r')
except IOError:
    print >> sys.stderr, "Unable to open input.txt for input"
    sys.exit(1)
try:
    outfile = open('output.txt', 'w')
except IOError:
    print >> sys.stderr, "Unable to open output.txt for output"
    sys.exit(1)
try:  # for finally
    try: # for I/O
        for line in infile:
            outfile.write(line)
    except IOError, e:
        print >> sys.stderr, "Some I/O Error occurred (reading from input.txt or writing to output.txt)"
finally:
    infile.close()
    outfile.close()
```


In Python 2.6 (or 2.5 if we use ''from __future__ import with_statement'') we can more simply write:


```python
import sys
try:
    with open('input.txt') as infile:
        with open('output.txt', 'w') as outfile:
            for line in infile:
                outfile.write(line)
except IOError:
    print >> sys.stderr, "Some I/O Error occurred"
    sys.exit(1)
```


The files will automatically be closed on exit of their ''with:'' blocks.  (Thus even if an I/O error occurred while reading the middle of the input file we are assured that the ''.close()'' method will have been called on each of the two files.


## R

If files are textual we can use <tt>readLines</tt> ("-1" means "read until the end")


```rsplus
src <- file("input.txt", "r")
dest <- file("output.txt", "w")

fc <- readLines(src, -1)
writeLines(fc, dest)
close(src); close(dest)
```


If the files are not textual but "generic":


```rsplus
src <- file("input.txt", "rb")
dest <- file("output.txt", "wb")

while( length(v <- readBin(src, "raw")) > 0 ) {
  writeBin(v, dest)
}
close(src); close(dest)
```


Another simpler way is to use <tt>file.copy</tt>


```rsplus
file.copy("input.txt", "output.txt", overwrite = FALSE)
```



## Racket


```Racket
#lang racket
(define file-content
  (with-input-from-file "input.txt"
    (lambda ()
      (let loop ((lst null))
        (define new (read-char))
        (if (eof-object? new)
            (apply string lst)
            (loop (append lst (list new))))))))

(with-output-to-file "output.txt"
  (lambda ()
    (write file-content)))
```



## RapidQ


File I/O is one of the things where RapidQ differs from standard Basic. RapidQ uses file streams.

The first version copies text line by line, as in the ''BASIC'' example.


```rapidq
$INCLUDE "rapidq.inc"

DIM File1 AS QFileStream
DIM File2 AS QFileStream

File1.Open("input.txt", fmOpenRead)
File2.Open("output.txt", fmCreate)

WHILE NOT File1.EOF
    data$ = File1.ReadLine
    File2.WriteLine(data$)
WEND

File1.Close
File2.Close
```


When just copying data, the code can be simplified by using the CopyFrom method.<br />
(The second parameter for CopyFrom is number of bytes to copy, 0 = copy the whole file.)


```rapidq
$INCLUDE "rapidq.inc"

DIM File1 AS QFileStream
DIM File2 AS QFileStream

File1.Open("input.txt", fmOpenRead)
File2.Open("output.txt", fmCreate)

File2.CopyFrom(File1, 0)

File1.Close
File2.Close
```



## Raven


```raven
'input.txt' read 'output.txt' write
```


## REALbasic


```vb

Sub WriteToFile(input As FolderItem, output As FolderItem)
  Dim tis As TextInputStream
  Dim tos As TextOutputStream
  tis = tis.Open(input)
  tos = tos.Create(output)
  While Not tis.EOF
    tos.WriteLine(tis.ReadLine)
  Wend
  tis.Close
  tos.Close
End Sub

```



## REBOL


```REBOL
write %output.txt read %input.txt

; No line translations:
write/binary %output.txt read/binary %input.txt

; Save a web page:
write/binary %output.html read http://rosettacode.org

```



## Red


```Red

file: read %input.txt
write %output.txt file
```



## Retro


```Retro
with files'
here dup "input.txt" slurp "output.txt" spew
```



## REXX

In REXX, filename association is used rather than numeric stream numbers and explicit file opening is not required.


### version 1

The two   ''optional''   REXX statements are only needed if there is another REXX program in the invocation chain

(which may have invoked this program)   that already has one of the input and/or output files open.

The two   ''best programming practice''   REXX statements are only needed if there is another calling program in the invocation chain

(which may want to (re-)use the two files just used.

```rexx
/*REXX program reads a file and copies the contents into an output file  (on a line by line basis).*/
iFID =  'input.txt'                              /*the name of the   input  file.       */
oFID = 'output.txt'                              /* "    "   "  "   output    "         */
call lineout iFID,,1                             /*insure the  input starts at line one.*/      /* ◄■■■■■■ optional.*/
call lineout oFID,,1                             /*   "    "  output    "    "   "   "  */      /* ◄■■■■■■ optional.*/

  do  while lines(iFID)\==0;    $=linein(iFID)   /*read records from input 'til finished*/
             call lineout oFID, $                /*write the record just read ──► output*/
  end   /*while*/                                /*stick a fork in it,  we're all done. */

call lineout iFID                                /*close   input  file, just to be safe.*/      /* ◄■■■■■■ best programming practice.*/
call lineout oFID                                /*  "    output    "     "   "  "   "  */      /* ◄■■■■■■ best programming practice.*/
```



### version 2

Note that this version is limited to files less than one million bytes (and/or possibly virtual memory).

```rexx
/*REXX program to read a file and write contents to an output file*****
* 03.09.2012 Walter Pachl (without erase string would be appended)
**********************************************************************/
ifid='input.txt'                        /*name of the  input file.   */
ofid='output.txt'                       /*name of the output file.   */
'erase' ofid                            /* avoid appending           */
s=charin(ifid,,1000000)                 /* read the input file       */
Call charout ofid,s                     /* write to output file      */
```



## Ring


```ring

fn1 = "ReadMe.txt"
fn2 = "ReadMe2.txt"

fp = fopen(fn1,"r")
str = fread(fp, getFileSize(fp))
fclose(fp)

fp = fopen(fn2,"w")
fwrite(fp, str)
fclose(fp)
see "OK" + nl

func getFileSize fp
     c_filestart = 0
     c_fileend = 2
     fseek(fp,0,c_fileend)
     nfilesize = ftell(fp)
     fseek(fp,0,c_filestart)
     return nfilesize

```



## Ruby

In general, open both files in binary mode.


```ruby
str = File.open('input.txt', 'rb') {|f| f.read}
File.open('output.txt', 'wb') {|f| f.write str}
```


If 'input.txt' is a text file, we may forget binary mode.  If no pathname begins with a pipe '|', then we may use ''IO::read'' and ''Kernel#open''.  (The pipe is a problem, because <code>IO.read('| uname')</code> or <code>open('| sh', 'w')</code> would open a subprocess and not a file.)


```ruby
# Only if 'input.txt' is a text file!
# Only if pipe '|' is not first character of path!
str = IO.read('input.txt')
open('output.txt', 'w') {|f| f.write str}
```


To copy a file block by block, use FileUtils from the standard library.


```ruby
require 'fileutils'
FileUtils.copy_file 'input.txt', 'output.txt'
```



## Run BASIC


```runbasic
open "input.txt" for input as #in
fileLen   = LOF(#in)		    'Length Of File
fileData$ = input$(#in, fileLen)    'read entire file
close #in

open "output.txt" for output as #out
print #out, fileData$               'write entire fie
close #out
end

' or directly with no intermediate fileData$

open "input.txt"  for input  as #in
open "output.txt" for output as #out
fileLen   = LOF(#in)		    'Length Of File
print #out, input$(#in, fileLen)    'entire file
close #in
close #out

```



## Rust


```rust
use std::fs::File;
use std::io::{Read, Write};

fn main() {
    let mut file = File::open("input.txt").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let mut file = File::create("output.txt").unwrap();
    file.write_all(&data).unwrap();
}

```

The above program will panic with any sort of error. The following shows proper error handling:

```rust
use std::fs::File;
use std::io::{self, Read,  Write};
use std::path::Path;
use std::{env, fmt, process};

fn main() {
    let files: Vec<_> = env::args_os().skip(1).take(2).collect();

    if files.len() != 2 {
        exit_err("Both an input file and output file are required", 1);
    }

    copy(&files[0], &files[1]).unwrap_or_else(|e| exit_err(&e, e.raw_os_error().unwrap_or(-1)));
}

fn copy<P: AsRef<Path>>(infile: P, outfile: P) -> io::Result<()> {
    let mut vec = Vec::new();

    Ok(try!(File::open(infile)
         .and_then(|mut i| i.read_to_end(&mut vec))
         .and_then(|_| File::create(outfile))
         .and_then(|mut o| o.write_all(&vec))))
}

fn exit_err<T: fmt::Display>(msg: T, code: i32) -> ! {
    writeln!(&mut io::stderr(), "ERROR: {}", msg).expect("Could not write to stdout");
    process::exit(code);
}
```



## Scala

{{libheader|Scala}}

```scala
import java.io.{ FileNotFoundException, PrintWriter }

object FileIO extends App {
  try {
    val MyFileTxtTarget = new PrintWriter("output.txt")

    scala.io.Source.fromFile("input.txt").getLines().foreach(MyFileTxtTarget.println)
    MyFileTxtTarget.close()
  } catch {
    case e: FileNotFoundException => println(e.getLocalizedMessage())
    case e: Throwable => {
      println("Some other exception type:")
      e.printStackTrace()
    }
  }
}
```



## Scheme

Character by character copy
```scheme
; Open ports for the input and output files
(define in-file (open-input-file "input.txt"))
(define out-file (open-output-file "output.txt"))

; Read and write characters from the input file
; to the output file one by one until end of file
(do ((c (read-char in-file) (read-char in-file)))
        ((eof-object? c))
        (write-char c out-file))

; Close the ports
(close-input-port in-file)
(close-output-port out-file)

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/osfiles.htm osfiles.s7i] contains the
function [http://seed7.sourceforge.net/libraries/osfiles.htm#copyFile%28in_string,in_string%29 copyFile] which
can be used to copy a source file to a destination.


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: main is func
  begin
    copyFile("input.txt", "output.txt");
  end func;
```



## Sidef


```ruby
var in = %f'input.txt'.open_r;
var out = %f'output.txt'.open_w;

in.each { |line|
    out.print(line);
};
```



## Slate


```slate
(File newNamed: 'input.txt' &mode: File Read) sessionDo: [| :in |
  (File newNamed: 'output.txt' &mode: File CreateWrite) sessionDo: [| :out |
    in >> out]]
```



## Snabel

Reads the entire file into into a list of buffers before writing and returns number of bytes written.

```snabel

let: q Bin list;
'input.txt' rfile read {{@q $1 push} when} for
@q 'output.txt' rwfile write
0 $1 &+ for

```


Alternative solution for large files with comparable performance to shell cp; also returns number of bytes written.

```snabel

let: q Bin list;
let: wq @q fifo;
let: w 'output.txt' rwfile @wq $1 write;

'input.txt' rfile read 0 $1 {{
  $ @q $1 push
  len +
  @w &break _for
} when} for

@q +? {@w &_ for} when

```



## Smalltalk


```smalltalk
| in out |
in := FileStream open: 'input.txt' mode: FileStream read.
out := FileStream open: 'output.txt' mode: FileStream write.
[ in atEnd ]
  whileFalse: [
     out nextPut: (in next)
  ]
```



## SNOBOL4


```snobol4

        input(.input,5,,'input.txt')
        output(.output,6,,'output.txt')
while   output = input                   :s(while)
end
```



## Standard ML

{{works with|SML/NJ|110.59}}

```sml
fun copyFile (from, to) =
let
  val instream = TextIO.openIn from
  val outstream = TextIO.openOut to
  val () = TextIO.output (outstream, TextIO.inputAll instream)
  val () = TextIO.closeIn instream
  val () = TextIO.closeOut outstream
in
  true
end handle _ => false;
```



## Stata

Stata has a [http://www.stata.com/help.cgi?copy copy] command. Here is a way to implement this by reading and writing line by line.

```stata
program copyfile
	file open fin using `1', read text
	file open fout using `2', write text replace

	file read fin line
	while !r(eof) {
		file write fout `"`line'"' _newline
		file read fin line
	}
	file close fin
	file close fout
end

copyfile input.txt output.txt
```



## Tcl

{{works with|tclsh}}
{{works with|eTcl}}
{{works with|wish}}
{{works with|tixwish}}
{{works with|tclkit}}

```tcl
set in [open "input.txt" r]
set out [open "output.txt" w]
# Obviously, arbitrary transformations could be added to the data at this point
puts -nonewline $out [read $in]
close $in
close $out
```

For larger files, it is better to use the <tt>fcopy</tt> command, though in general this restricts what operations can be performed rather more (only encoding and end-of-line translations are possible — or more general byte-level transformations with the generic filter mechanism provided in Tcl 8.6 — none of which are shown here):

```tcl
set in [open "input.txt" r]
set out [open "output.txt" w]
fcopy $in $out
close $in
close $out
```

Or the minimal version if we don't need any processing of the data at all:

```tcl>file copy input.txt output.txt</lang


### Other key file I/O operations

;Writing a line to a file<nowiki>:</nowiki>

```tcl
#open file for writing
set myfile [open "README.TXT" w]
#write something to the file
puts $myfile "This is line 1, so hello world...."
#close the file
close $myfile
```

;Reading a line from a file<nowiki>:</nowiki>

```tcl
#open file for reading
set myfile [open "README.TXT" r]
#read something from the file
gets $myfile mydata
#show what was read from the file
#should print "This is line1, so hello world...."
puts $mydata
#close the file
close $myfile
```



## Toka

This is one method, which works with any type of file:


```toka
( source dest -- )
{
  value| source dest size buffer |
  {
    {
      [ "W" file.open to dest ] is open-dest
      [ "R" file.open to source ] is open-source
      [ open-dest open-source ]
    } is open-files
    {
      [ source file.size to size ] is obtain-size
      [ size malloc to buffer ] is allocate-buffer
      [ obtain-size allocate-buffer ]
    } is create-buffer
    [ source dest and 0 <> ] is check
    [ open-files create-buffer check ]
  } is prepare
  [ source buffer size file.read drop ] is read-source
  [ dest buffer size file.write drop ] is write-dest
  [ source file.close dest file.close ] is close-files
  [ prepare [ read-source write-dest close-files ] ifTrue ]
} is copy-file
```


And a much simpler way for plain text files, making use of file.slurp:


```toka
[ ( source dest -- )
  swap file.slurp dup 0 <>
  [ >r "W" file.open dup r> string.getLength file.write drop file.close ] ifTrue
] is copy-file
```


And a test:


```toka
" input.txt" " output.txt" copy-file
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ERROR/STOP CREATE ("input.txt", seq-o,-std-)
ERROR/STOP CREATE ("output.txt",seq-o,-std-)

FILE/ERASE "input.txt" = "Some irrelevant content"
path2input =FULLNAME(TUSTEP,"input.txt", -std-)
status=READ (path2input,contentinput)

path2output=FULLNAME(TUSTEP,"output.txt",-std-)
status=WRITE(path2output,contentinput)

```


## TXR


As a character string:


```txrlisp
(let ((var (file-get-string "input.txt")))
  (file-put-string "output.txt" var))
```


As a list of lines:


```txrlisp
(let ((var (file-get-lines "input.txt")))
  (file-put-lines "output.txt" var))
```



## UNIX Shell

Using the 'read' built-in

* Hint: [https://www.mirbsd.org/htman/i386/man1/mksh.htm mksh(1) manual] says, "If '''read''' is run in a loop such as '''while read foo; do ...; done''' then leading whitespace will be removed (IFS) and backslashes processed. You might want to use '''while IFS= read -r foo; do ...; done''' for pristine I/O."
* Caveat: output.txt will end with a newline, whether or not input.txt ended with one.


```bash
#!/bin/sh
while IFS= read -r a; do
    printf '%s\n' "$a"
done <input.txt >output.txt
```


Another way, using the 'cat' program


```bash
#!/bin/sh
cat input.txt >output.txt
```


Yet another way, using the 'cp' utility

```bash
#!/bin/sh
cp input.txt output.txt
```



## Ursa


```ursa
decl file input output
decl string contents
input.open "input.txt"
output.create "output.txt"
output.open "output.txt"
set contents (input.readall)
out contents output
```



## Ursala


I/O in Ursala is meant to be handled transparently by the run time
system.  The application is passed the input files as an argument and
expected to return the output files as a result.

Returning a copy of the input file with a new name causes it to be
written as a new file.

```Ursala
#import std

#executable ('parameterized','')

fileio = ~command.files; &h.path.&h:= 'output.txt'!
```



## VBA


```vb
Option Explicit

Sub Main()
Dim s As String, FF As Integer

'read a file line by line
FF = FreeFile
Open "C:\Users\" & Environ("username") & "\Desktop\input.txt" For Input As #FF
While Not EOF(FF)
    Line Input #FF, s
    Debug.Print s
Wend
Close #FF

'read a file
FF = FreeFile
Open "C:\Users\" & Environ("username") & "\Desktop\input.txt" For Input As #FF
    s = Input(LOF(1), #FF)
Close #FF
Debug.Print s

'write a file
FF = FreeFile
Open "C:\Users\" & Environ("username") & "\Desktop\output.txt" For Output As #FF
    Print #FF, s
Close #FF
End Sub
```



## VBScript

one liner (-2 for system default encoding)

```vb
CreateObject("Scripting.FileSystemObject").OpenTextFile("output.txt",2,-2).Write CreateObject("Scripting.FileSystemObject").OpenTextFile("input.txt", 1, -2).ReadAll
```



## Vedit macro language

An edit buffer is normally used as "string variable" in Vedit.
To read a file into edit buffer, simply open the file. The file contents can then be modified if required before saving into a new file.

```vedit
File_Open("input.txt")
File_Save_As("output.txt", NOMSG)
Buf_Close(NOMSG)
```



## Visual Basic .NET

{{works with|Visual Basic .NET|9.0+}}


```vbnet
'byte copy
My.Computer.FileSystem.WriteAllBytes("output.txt", _
  My.Computer.FileSystem.ReadAllBytes("input.txt"), False)

'text copy
Using input = IO.File.OpenText("input.txt"), _
      output As New IO.StreamWriter(IO.File.OpenWrite("output.txt"))
  output.Write(input.ReadToEnd)
End Using

'Line by line text copy
Using input = IO.File.OpenText("input.txt"), _
      output As New IO.StreamWriter(IO.File.OpenWrite("output.txt"))
  Do Until input.EndOfStream
    output.WriteLine(input.ReadLine)
  Loop
End Using
```



## Wart


```wart
with infile "input.txt"
  with outfile "output.txt"
    whilet line (read_line)
      prn line
```



## XPL0

Usage: fileio <input.txt >output.txt


```XPL0
include c:\cxpl\codes;
int  I, C;
char IntermediateVariable;
[IntermediateVariable:= GetHp;
I:= 0;
repeat  C:= ChIn(1);
        IntermediateVariable(I):= C;
        I:= I+1;
until C = $1A; \EOF
I:= 0;
repeat  C:= IntermediateVariable(I);
        I:= I+1;
        ChOut(0, C);
until C = $1A; \EOF
]
```



## zkl


```zkl
var d=File("input.txt").read();
(f:=File("output.txt","w")).write(d); f.close(); // one read, one write copy
File("output.txt").pump(Console); // verify by printing
```

It might be considered "good form" to immediately close files (rather than wait for the garbage collector to do it) as these are system resources.

```zkl
var in=File("input.txt"), out=File("output.txt","w");
foreach line in (in) { out.write(line) } // copy line by line
out.close(); // or out=Void and let GC close the file
```


```zkl
fin,fout:=File("input.txt","rb"), File("output.txt","wb"); // copy in chunks, implicit buffer
fin.pump(Data(0d524_287),fout); fin.close(); fout.close();
```


```zkl
// copy in chunks, let GC close file handles
File("input.txt","rb").pump(Data(0d524_287),File("output.txt","wb"));
```

In these last two, the implicit buffer (whose size if given by the numbers) is visible inside the pump. Consider this example, which converts input to upper case text on its way to output (the Data() is optional; using it chunks, without, lines):

```zkl
File("input.txt").pump(Data(),File("output.txt","w"),"text","toUpper");
```


{{omit from|HTML}}
{{omit from|Order}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables. -->
{{omit from|Unlambda|Does not handle files.}}
