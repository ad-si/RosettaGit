+++
title = "Loops/For"
description = ""
date = 2019-10-13T03:27:28Z
aliases = []
[extra]
id = 2819
[taxonomies]
categories = ["Iteration", "task"]
tags = []
languages = [
  "360_assembly",
  "8th",
  "actionscript",
  "ada",
  "agena",
  "algol_60",
  "algol_68",
  "algol_w",
  "algol_m",
  "alore",
  "amigae",
  "apex",
  "applescript",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "babel",
  "bash",
  "basic",
  "applesoft_basic",
  "bbc_basic",
  "commodore_basic",
  "creative_basic",
  "gw_basic",
  "fbsl",
  "fuze_basic",
  "iwbasic",
  "liberty_basic",
  "microsoft_small_basic",
  "purebasic",
  "run_basic",
  "smart_basic",
  "visual_basic",
  "visual_basic_net",
  "zx_spectrum_basic",
  "batch_file",
  "bc",
  "befunge",
  "blz",
  "bracmat",
  "brat",
  "c",
  "cpp",
  "c_sharp",
  "ceylon",
  "chapel",
  "chef",
  "clojure",
  "cobol",
  "coldfusion",
  "common_lisp",
  "coq",
  "crystal",
  "d",
  "dao",
  "dart",
  "dc",
  "delphi",
  "dms",
  "dodo0",
  "dragon",
  "dwscript",
  "dyalect",
  "e",
  "easylang",
  "edsac_order_code",
  "egl",
  "ela",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "false",
  "fantom",
  "focal",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "futhark",
  "gambas",
  "gap",
  "gml",
  "go",
  "groovy",
  "hack",
  "haskell",
  "haxe",
  "hexiscript",
  "hicest",
  "holyc",
  "icon",
  "unicon",
  "inform_7",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "klong",
  "kotlin",
  "labview",
  "lang5",
  "langur",
  "lasso",
  "lc3_assembly",
  "lil",
  "lingo",
  "lisaac",
  "livecode",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "make",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "mercury",
  "miniscript",
  "moo",
  "morfa",
  "mumps",
  "nanoquery",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "onyx",
  "order",
  "oz",
  "panoramic",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pilot",
  "pl_i",
  "pop11",
  "powershell",
  "processing",
  "prolog",
  "python",
  "r",
  "racket",
  "rebol",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "salmon",
  "sas",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "setl",
  "sidef",
  "simula",
  "slate",
  "smalltalk",
  "snobol4",
  "snusp",
  "sparkling",
  "spin",
  "spl",
  "stata",
  "suneido",
  "swift",
  "tcl",
  "torquescript",
  "transforth",
  "tuscript",
  "typescript",
  "unix_shell",
  "c_shell",
  "unixpipes",
  "ursa",
  "vala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "wart",
  "wee_basic",
  "x86_assembly",
  "xlisp",
  "xpl0",
  "z80_assembly",
  "zkl",
  "brainfuck",
  "f_sharp",
  "matlab",
  "modula_2",
  "modula_3",
  "ns_hubasic",
  "oberon_2",
  "ti_83_basic",
  "ti_89_basic",
]
+++

## Task

“'''For'''”   loops are used to make some block of code be iterated a number of times, setting a variable or parameter to a monotonically increasing integer value for each execution of the block of code.

Common extensions of this allow other counting patterns or iterating over abstract structures other than the integers.


## Task
Show how two loops may be nested within each other, with the number of iterations performed by the inner for loop being controlled by the outer for loop.

Specifically print out the following pattern by using one for loop nested in another:

```txt
*
**
***
****
*****
```



## Related tasks
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## Reference
* [[wp:For loop|For loop]] Wikipedia.





## 360 Assembly

;Basic - Algol style
The opcode BXH uses 3 registers, one for index, one for step and one for limit.

```360asm

*        Loops/For - BXH Algol     27/07/2015
LOOPFOR  CSECT
         USING  LOOPFORC,R12
         LR     R12,R15            set base register
BEGIN    LA     R2,0               from 1 (from-step=0)
         LA     R4,1               step 1
         LA     R5,5               to 5
LOOPI    BXH    R2,R4,ELOOPI       for i=1 to 5  (R2=i)
         LA     R8,BUFFER-1          ipx=-1
         LA     R3,0                 from 1 (from-step=0)
         LA     R6,1                 step 1
         LR     R7,R2                to i
LOOPJ    BXH    R3,R6,ELOOPJ         for j:=1 to i  (R3=j)
         LA     R8,1(R8)               ipx=ipx+1
         MVI    0(R8),C'*'             buffer(ipx)='*'
         B      LOOPJ                next j
ELOOPJ   XPRNT  BUFFER,L'BUFFER      print buffer
         B      LOOPI              next i
ELOOPI   BR     R14                return to caller
BUFFER   DC     CL80' '            buffer
         YREGS
         END    LOOPFOR

```

{{out}}

```txt

*
**
***
****
*****

```

;Structured Macros
Structured and without BXH, only one register used by loop.

```360asm

*        Loops/For - struct        29/06/2016
LOOPFOR  CSECT
         USING  LOOPFORC,R12
         LR     R12,R15            set base register
         LA     R2,1               from 1
         DO WHILE=(CH,R2,LE,=H'5') for i=1 to 5  (R2=i)
           LA     R8,BUFFER-1        ipx=-1
           LA     R3,1               from 1
           DO WHILE=(CR,R3,LE,R2)    for j:=1 to i  (R3=j)
             LA     R8,1(R8)           ipx=ipx+1
             MVI    0(R8),C'*'         buffer(ipx)='*'
             LA     R3,1(R3)           j=j+1  (step)
           ENDDO  ,                  next j
           XPRNT  BUFFER,L'BUFFER    print buffer
           LA     R2,1(R2)           i=i+1  (step)
         ENDDO  ,                  next i
         BR     R14                return to caller
BUFFER   DC     CL80' '            buffer
         YREGS
         END    LOOPFOR

```

{{out}}
Same as above


## 8th

This illustrates two kinds of 'for' loop.  The first kind is "loop", which iterates from the low to the high value, and passes the current loop index as a parameter to the inner word.  The second is 'times', which takes a count and repeats the word that many times.


```forth

( ( '* putc ) swap times cr ) 1 5 loop

```



## ActionScript


```actionscript
var str:String = "";
for (var i:int = 1; i <= 5; i++) {
	for (var j:int = 1; j <= i; j++)
		str += "*";
	trace(str);
	str = "";
}
```



## Ada


```ada
for I in 1..5 loop
   for J in 1..I loop
      Put("*");
   end loop;
   New_Line;
end loop;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
for i to 5 do
    for j to i do
        write( "*" )
    od;
    print()
od
```



## ALGOL 60


```algol60
INTEGER I,J;
FOR I:=1 STEP 1 UNTIL 5 DO
BEGIN
   FOR J:=1 STEP 1 UNTIL I DO
      OUTTEXT("*");
   OUTLINE
END

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
FOR i TO 5 DO
   TO i DO
      print("*")
   OD;
  print(new line)
OD
```

{{out}}

```txt

*
**
***
****
*****

```


=={{header|ALGOL-M}}==

```algol
BEGIN
    INTEGER I, J;
    FOR I := 1 STEP 1 UNTIL 5 DO
    BEGIN
        WRITE( "" );
        FOR J := 1 STEP 1 UNTIL I DO
            WRITEON( "*" );
    END;
END
```

{{out}}

```txt
*
**
***
****
*****
```



## ALGOL W

In Algol W, write starts a new line, writeon continues it.

```algolw
begin
    for i := 1 until 5 do
    begin
        write( "*" );
        for j := 2 until i do
        begin
            writeon( "*" )
        end j
    end i
end.
```



## Alore



```Alore
for i in 0 to 6
  for j in 0 to i
      Write('*')
  end
  WriteLn()
end

```



## AmigaE


```amigae
PROC main()
  DEF i, j
  FOR i := 1 TO 5
    FOR j := 1 TO i DO WriteF('*')
    WriteF('\n')
  ENDFOR
ENDPROC
```



## Apex


```java
for (Integer i = 0; i < 5; i++) {
    String line = '';

    for (Integer j = 0; j < i; j++) {
        line += '*';
    }

    System.debug(line);
}

List<String> lines = new List<String> {
    '*',
    '**',
    '***',
    '****',
    '*****'
};

for (String line : lines) {
    System.debug(line);
}
```



## AppleScript


```AppleScript
set x to return
repeat with i from 1 to 5
	repeat with j from 1 to i
		set x to x & "*"
	end repeat
	set x to x & return
end repeat
return x
```

{{out}}
```txt
"
*
**
***
****
*****
"
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program loop1.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessX: .asciz "X"
szCarriageReturn:  .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    mov r2,#0       @ counter loop 1
1:       @ loop start 1
    mov r1,#0        @ counter loop 2
2:       @ loop start 2
    ldr r0,iAdrszMessX
    bl affichageMess
    add r1,#1       @ r1 + 1
    cmp r1,r2       @ compare r1 r2
    ble 2b        @ loop label 2 before
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
    add r2,#1       @ r2 + 1
    cmp r2,#5       @ for five loop
    blt 1b         @ loop label 1 before


100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call

iAdrszMessX:  .int szMessX
iAdrszCarriageReturn:  .int  szCarriageReturn
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
loop $(range 0 5) {
	loop $(range 0 &) {
		print "*" true
	}
	""
}
```


## AutoHotkey


```AutoHotkey
Gui, Add, Edit, vOutput r5 w100 -VScroll ; Create an Edit-Control
Gui, Show ; Show the window
Loop, 5 ; loop 5 times
{
  Loop, %A_Index% ; A_Index contains the Index of the current loop
  {
    output .= "*" ; append an "*" to the output var
    GuiControl, , Output, %Output% ; update the Edit-Control with the new content
    Sleep, 500 ; wait some(500ms) time, [just to show off]
  }
  Output .= (A_Index = 5) ? "" : "`n" ; append a new line to the output if A_Index is not "5"
}
Return ; End of auto-execution section
```



## AWK



```awk
BEGIN {
  for(i=1; i < 6; i++) {
    for(j=1; j <= i; j++ ) {
      printf "*"
    }
    print
  }
}
```



## Axe

In this example, the Axe code is nearly identical to the [[#TI-83_BASIC|TI-83 BASIC]] version. However, note the swapped order of the I and J in the Output() statement. Also, unlike TI-83 BASIC, Axe does not support an increment value other than 1.

```axe
ClrHome
For(I,1,5)
For(J,1,I)
Output(J,I,"*")
End
End
```



## Babel



```babel
((main { 10 star_triangle ! })

(star_triangle {
    dup
    <-
    { dup { "*" << } <->
            iter - 1 +
        times
        "\n" << }
    ->
    times }))
```


{{out}}

```txt
*
**
***
****
*****
******
*******
********
*********
**********
```


The key operator here is 'iter' which gives the current iteration of the loop body it
resides in. When used with the 'times' operator, it generates a countdown.



## bash


```bash

for i in {1..5}
do
  for ((j=1; j<=i; j++));
  do
    echo -n "*"
  done
  echo
done

```




## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
for i = 1 to 5
   for j = 1 to i
      print "*";
   next j
   print
next i
```


=
## Applesoft BASIC
=

```ApplesoftBasic
FOR I = 1 TO 5 : FOR J = 1 TO I : PRINT "*"; : NEXT J : PRINT : NEXT
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}
<lang>
      FOR I% = 1 TO 5
        FOR J% = 1 TO I%
          PRINT"*";
        NEXT
        PRINT
      NEXT

```


=
## Commodore BASIC
=

```qbasic
10 FOR I = 1 TO 5
20 FOR J = 1 TO I
30 PRINT "*";
40 NEXT
50 PRINT
60 NEXT
```


=
## Creative Basic
=

```Creative Basic

OPENCONSOLE

FOR X=1 TO 5

	FOR Y=1 TO X

            PRINT"*",:'No line feed or carriage return after printing.

	NEXT Y

PRINT

NEXT X

PRINT:PRINT"Press any key to end."

DO:UNTIL INKEY$<>""

CLOSECONSOLE

END

```


==={{header|GW-BASIC}}===

```qbasic
10 FOR I = 1 TO 5
20 FOR J = 1 TO I
30 PRINT "*";
40 NEXT J
50 PRINT
60 NEXT I

```


=
## FBSL
=

```qbasic

#APPTYPE CONSOLE
FOR dim i = 1 TO 5
    FOR dim j = 1 TO i
        PRINT "*";
    NEXT j
    PRINT
NEXT i
Pause
```

{{out}}

```txt

*
**
***
****
*****
Press any key to continue...
```


=
## FUZE BASIC
=

```qbasic
FOR n = 1 to 5 CYCLE
    FOR k = 1 to n CYCLE
        print "*";
    REPEAT
    PRINT
REPEAT
END
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 FOR I=1 TO 5
110   FOR J=1 TO I
120     PRINT "*";
130   NEXT
140   PRINT
150 NEXT
```


=
## IWBASIC
=

```IWBASIC

OPENCONSOLE

FOR X=1 TO 5

    FOR Y=1 TO X

    LOCATE X,Y:PRINT"*"

    NEXT Y

NEXT X

PRINT

CLOSECONSOLE

END

'Could also have been written the same way as the Creative Basic example, with no LOCATE command.

```


=
## Liberty BASIC
=
Unlike some BASICs, Liberty BASIC does not require that the counter variable be specified with 'next'.

```lb
for i = 1 to 5
    for j = 1 to i
        print "*";
    next
    print
next

```



=
## Microsoft Small Basic
=

```microsoftsmallbasic

For i = 1 TO 5
  For j = 1 To i
    TextWindow.Write("*")
  EndFor
  TextWindow.WriteLine("")
EndFor

```


=
## PureBasic
=

```PureBasic
If OpenConsole()
  Define i, j
  For i=1 To 5
    For j=1 To i
      Print("*")
    Next j
    PrintN("")
  Next i
  Print(#LFCR$+"Press ENTER to quit"): Input()
  CloseConsole()
EndIf
```


=
## Run BASIC
=

```runbasic

FOR i = 1 TO 5
   FOR j = 1 TO i
      PRINT "*";
   NEXT j
   PRINT
NEXT i

```


=
## smart BASIC
=

While some versions of BASIC allow for NEXT without a variable, smart BASIC requires the variable designation.


```qbasic
for n = 1 to 5
    for m = 1 to n
        print "*";
    next m
    print
next n
```


=
## Visual Basic
=
'''Works with:''' VB6

```vb
Public OutConsole As Scripting.TextStream
For i = 0 To 4
    For j = 0 To i
        OutConsole.Write "*"
    Next j
    OutConsole.WriteLine
Next i
```


=
## Visual Basic .NET
=
{{works with|Visual Basic .Net 2002}}

```vbnet
For x As Integer = 0 To 4
    For y As Integer = 0 To x
        Console.Write("*")
    Next
    Console.WriteLine()
Next
```


=
## ZX Spectrum Basic
=

On the ZX Spectrum, we need line numbers:


```basic

10 FOR i = 1 TO 5
20 FOR j = 1 TO i
30 PRINT "*";
40 NEXT j
50 PRINT
60 NEXT i

```



## Batch File


<lang>@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

for /l %%i in (1,1,5) do (
    SET line=
    for /l %%j in (1,1,%%i) do (
        SET line=!line!*
    )
    ECHO !line!
)

ENDLOCAL
```



## bc


```bc
for (i = 1; i <= 5; i++) {
	for (j = 1; j <= i; j++) "*"
	"
"
}
quit
```



## Befunge


```befunge>1
:5`#@_:>"*",v
         | :-1<
 ^+1,+5+5<
```



## blz


```blz
for i = 1; i <= 5; i++
    line = ""
    for (j = 1; j <= i; j++)
        line = line + "*"
    end
    print(line)
end
```



## Bracmat


```bracmat
  0:?i
&   whl
  ' ( !i+1:~>5:?i
    & 0:?k
    & whl'(!k+1:~>!i:?k&put$"*")
    & put$\n
    )
&
);
```


=={{header|Brainfuck}}==

```bf>>>+++++++[>++++++[
+<-]<-]       place * in cell 3
+++++[>++[>>+<<-]<-]<<           place \n in cell 4
+++++[                           set outer loop count
[>+                              increment inner counter
>[-]>[-]<<[->+>+<<]>>[-<<+>>]<<  copy inner counter
>[>>.<<-]>>>.<<<                 print line
<<-]                             end inner loop
]                                end outer loop
```



## Brat


```brat
1.to 5, { i |
  1.to i, { j |
    print "*"
  }
  print "\n"
}
```



## C


```c
int i, j;
for (i = 1; i <= 5; i++) {
  for (j = 1; j <= i; j++)
    putchar('*');
  puts("");
}
```



## C++


```cpp

for(int i = 0; i < 5; ++i) {
  for(int j = 0; j < i; ++j)
    std::cout.put('*');

  std::cout.put('\n');
}
```


## C#

```c#
using System;

class Program {
    static void Main(string[] args)
    {
        for (int i = 0; i < 5; i++)
        {
            for (int j = 0; j <= i; j++)
            {
                Console.Write("*");
            }
            Console.WriteLine();
        }
    }
}
```



## Ceylon


```ceylon
shared void run() {

	for(i in 1..5) {
		for(j in 1..i) {
			process.write("*");
		}
		print("");
	}
}
```



## Chapel


```chapel
for i in 1..5 {
        for 1..i do write('*');
        writeln();
}
```



## Chef



```chef
Asterisks Omelette.

This recipe prints a triangle of asterisks.

Ingredients.
5 eggs
1 onion
1 potato
42 ml water
10 ml olive oil
1 garlic

Method.
Put eggs into the mixing bowl.
Fold onion into the mixing bowl.
Put eggs into the mixing bowl.
Add garlic into the mixing bowl.
Fold eggs into the mixing bowl.
Chop onion.
Put onion into the mixing bowl.
Fold potato into the mixing bowl.
Put olive oil into the mixing bowl.
Mash potato.
Put water into the mixing bowl.
Mash potato until mashed.
Chop onion until choped.
Pour contents of the mixing bowl into the baking dish.

Serves 1.
```



## Clojure



```clojure
(doseq [i (range 5), j (range (inc i))]
  (print "*")
  (if (= i j) (println)))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Display-Triangle.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Outer-Counter PIC 9.
       01  Inner-Counter PIC 9.

       PROCEDURE DIVISION.
       PERFORM VARYING Outer-Counter FROM 1 BY 1 UNTIL 5 < Outer-Counter

           PERFORM VARYING Inner-Counter FROM 1 BY 1
                   UNTIL Outer-Counter < Inner-Counter
               DISPLAY "*" NO ADVANCING
           END-PERFORM

           DISPLAY "" *> Output a newline
       END-PERFORM

       GOBACK
       .

```



## ColdFusion

Remove the leading space from the line break tag.

With tags:

```cfm
<cfloop index = "i" from = "1" to = "5">
  <cfloop index = "j" from = "1" to = "#i#">
    *
  </cfloop>
  < br />
</cfloop>
```

With script:

```cfm><cfscript

  for( i = 1; i <= 5; i++ )
  {
    for( j = 1; j <= i; j++ )
    {
      writeOutput( "*" );
    }
    writeOutput( "< br />" );
  }
</cfscript>
```



## Common Lisp


```lisp
(loop for i from 1 upto 5 do
  (loop for j from 1 upto i do
    (write-char #\*))
  (write-line ""))
```



```lisp
(dotimes (i 5)
  (dotimes (j (+ i 1))
    (write-char #\*))
  (terpri))
```



```lisp
(do ((i 1 (+ i 1)))
    ((> i 5))
  (do ((j 1 (+ j 1)))
      ((> j i))
    (write-char #\*))
  (terpri))
```



## Coq



```coq
Section FOR.
  Variable T : Type.
  Variable body : nat -> T -> T.
  Variable start : nat.

  Fixpoint for_loop n : T -> T :=
    match n with
    | O => fun s => s
    | S n' => fun s => for_loop n' (body (start + n') s)
    end.

End FOR.

Eval vm_compute in
  for_loop _
    (fun i =>
      cons
        (for_loop _
          (fun j => cons tt)
          0 (S i) nil
        )
    )
    0 5 nil.

```



## Crystal



```crystal

(1..5).each do |i|
  (1..i).each do |j|
    print "*"
  end
  puts
end

```


Or another way, more succinctly put:


```crystal

puts (1..5).map { |i| "*" * i }.join("\n")

```




## D


```d
import std.stdio: write, writeln;

void main() {
    for (int i; i < 5; i++) {
        for (int j; j <= i; j++)
            write("*");
        writeln();
    }
    writeln();

    foreach (i; 0 .. 5) {
        foreach (j; 0 .. i + 1)
            write("*");
        writeln();
    }
}
```

{{out}}

```txt
*
**
***
****
*****

*
**
***
****
*****
```



## Dao


```dao
for( i = 1 : 5 ){
    for( j = 1 : i ) io.write( '*' )
    io.writeln()
}
```



## Dart


```dart
main() {
    for (var i = 0; i < 5; i++)
        for (var j = 0; j < i + 1; j++)
            print("*");
        print("\n");
}
```



## dc

<tt>[...]sA</tt> defines the inner loop A and <tt>[...]sB</tt> defines the outer loop B. This program nests the entrance to loop A inside loop B.

{{trans|bc}}

```dc
[
 [*]P		[print asterisk]sz
 lj 1 + d sj	[increment j, leave it on stack]sz
 li !<A		[continue loop if i >= j]sz
]sA
[
 1 d sj		[j = 1, leave it on stack]sz
 li !<A		[enter loop A if i >= j]sz
 [
]P		[print newline]sz
 li 1 + d si	[increment i, leave it on stack]sz
 5 !<B		[continue loop if 5 >= i]sz
]sB
1 d si		[i = 1, leave it on stack]sz
5 !<B		[enter loop B if 5 >= i]sz
```



## Delphi


```Delphi
program LoopFor;

{$APPTYPE CONSOLE}

var
  i, j: Integer;
begin
  for i := 1 to 5 do
  begin
    for j := 1 to i do
      Write('*');
    Writeln;
  end;
end.
```



## DMS


```DMS
number i, j
for (i = 1; i <= 5; i++)
{
    for (j = 1; j <= i; j++)
    {
        Result( "*" )
    }
    Result( "\n" )
}
```



## dodo0


```dodo0
fun for -> var, test, body, return    # define a for loop using recursion
(
   test(var) -> continue
   if (continue) ->
   (
      body(var) -> var
      for (var, test, body, return)
   )
   |
      return(var)
)
| for

fun upToFive (-> index, return) '<='(index, 5, return) | upToFive

for (1, upToFive) -> index, return
(
   fun countTheStars -> stars, return
   (
      'count'(stars) -> n
      '<'(n, index, return)   # continue until n = index
   )
   | countTheStars

   for ("*", countTheStars) -> prefix, return
      'str'(prefix, "*", return)
   | stars

   println(stars) ->

   'inc'(index, return)
)
| result
exit()
```



## Dragon


```dragon
for (i = 0, i < 5, i++) {
   for (j = 0, j <= i, j++) {
      show "*"
   }
   showln ""
}
```



## DWScript


```Delphi
var i, j : Integer;

for i := 1 to 5 do begin
   for j := 1 to i do
      Print('*');
   PrintLn('');
end;
```



## Dyalect

{{trans|Swift}}


```Dyalect
for i in 1..5 {
    for _ in 1..i {
        print("*", terminator: "")
    }
    print()
}
```


Output:


```txt
*
**
***
****
*****
```



## E



```e
for width in 1..5 {
    for _ in 1..width {
        print("*")
    }
    println()
}
```


This loop is a combination of <code>for ... in ...</code> which iterates over something and <code>a..b</code> which is a range object that is iteratable. (Also, writing <code>a..!b</code> excludes the value b.)


## EasyLang


<lang>for i range 5
  a$ = "*"
  for j range i
    a$ &= "*"
  .
  print a$
.
```



## EDSAC order code

As with many other machine-level languages, there is no inbuilt looping construct; but the equivalent of a <tt>FOR</tt> or <tt>DO</tt> loop can easily be synthesized using conditional branching orders and a control variable.

The EDSAC character set does not include a <tt>*</tt> character, so <tt>+</tt> has been used instead.

Characters are encoded in five-bit form, with each code point producing a different character depending on whether the machine is in 'letter' or 'figure' mode: this is why it is necessary to output a 'figure shift' control character at the beginning of the job.

```edsac
[ Loops
  =====

  A program for the EDSAC

  Demonstrates nested loops
  and printer output

  Works with Initial Orders 2 ]



        T56K  [ set load point  ]
        GK    [ set theta       ]

        O21@  [ figure shift    ]

[  1 ]  T24@  [ a = 0           ]
        A19@  [ a = i           ]

[  3 ]  T20@  [ j = a; a = 0    ]
        O22@  [ write character ]
        A20@  [ a = j           ]
        S17@  [ a -= 1          ]
        U20@  [ j = a           ]
        E3@   [ if a>=0 go to 3 ]

        O23@  [ write line feed ]
        T24@  [ a = 0           ]
        A19@  [ a = i           ]
        A17@  [ a += 1          ]
        U19@  [ i = a           ]
        S18@  [ a -= 5          ]
        G1@   [ if a<0 go to 1  ]

        ZF    [ halt            ]

[ 17 ]  P0D   [ const: 1        ]
[ 18 ]  P2D   [ const: 5        ]

[ 19 ]  P0F   [ var: i          ]
[ 20 ]  P0F   [ var: j          ]

[ 21 ]  #F    [ figure shift    ]
[ 22 ]  ZF    [ '+' character   ]
[ 23 ]  &F    [ line feed       ]

[ 24 ]  P0F   [ used to clear a ]

        EZPF  [ begin execution ]
```

{{out}}

```txt
+
++
+++
++++
+++++
```



## EGL


```EGL
str string;
for ( i int to 5 )
   str = "";
   for ( j int to i )
      str += "*";
   end
   SysLib.writeStdout(str);
end
```



## Ela



```ela
open monad io

loop m n | n < m = do
            loop' n 0
            putStrLn ""
            loop m (n + 1)
         | else = do return ()
          where loop' m n | n <= m = do
                              putStr "*"
                              loop' m (n + 1)
                          | else = do return ()

_ = loop 10 1 ::: IO
```



Output:


```ela
**
***
****
*****
******
*******
********
*********
**********
```



## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    for(int i := 0, i < 5, i += 1)
    {
        for(int j := 0, j <= i, j += 1)
            { console.write:"*" };

        console.writeLine()
    }
}
```

{{out}}

```txt

*
**
***
****
*****

```



## Elixir


```elixir
defmodule Loops do
  def loops_for(n) do
    Enum.each(1..n, fn i ->
      Enum.each(1..i, fn _ -> IO.write "*" end)
      IO.puts ""
    end)
  end
end

Loops.loops_for(5)
```


one line (Comprehensions)

```elixir
for i <- 1..5, do: IO.puts (for j <- 1..i, do: "*")
```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(nested_loops).
-export([main/0, inner_loop/0]).

main() ->
	outer_loop(1).

inner_loop()->
	inner_loop(1).

inner_loop(N) when N rem 5 =:= 0 ->
	io:format("* ");

inner_loop(N) ->
	io:fwrite("* "),
	inner_loop(N+1).

outer_loop(N) when N rem 5 =:= 0 ->
	io:format("*");

outer_loop(N) ->
	outer_loop(N+1),
	io:format("~n"),
	inner_loop(N).

```




## ERRE


```ERRE

FOR I=1 TO 5 DO
    FOR J=1 TO I DO
        PRINT("*";)
    END FOR
    PRINT
END FOR

```



## Euphoria



```Euphoria

for i = 1 to 5 do
    for j = 1 to i do
        puts(1, "*") -- Same as "puts(1, {'*'})"
    end for
    puts(1, "\n") -- Same as "puts(1, {'\n'})"
end for

```


<code>puts()</code> is a function that takes two arguments; an <code>integer</code> and a <code>sequence</code>.  Strings are simply <code>sequence</code>s; there is no string type.
The <code>integer</code> specifies where to put the "string".  0 = STDIN, 1 = STDOUT, 2 = STDERR, 3+ = files that are opened with the <code>open()</code> function.
<code>puts()</code> prints the <code>sequence</code> out, as a "string".  Each element in the <code>sequence</code> provided is printed out as the character with that value in the ASCII character chart.

=={{header|F_Sharp|F#}}==

```fsharp
#light
[<EntryPoint>]
let main args =
    for i = 1 to 5 do
        for j = 1 to i do
            printf "*"
        printfn ""
    0
```



## Factor


```factor
5 [1,b] [ [ "*" write ] times nl ] each
```



## FALSE


```false
1[$6-][$[$]["*"1-]#%"
"1+]#%
```



## Fantom


Using for loops:


```fantom

class ForLoops
{
  public static Void main ()
  {
    for (Int i := 1; i <= 5; ++i)
    {
      for (Int j := 1; j <= i; ++j)
      {
         Env.cur.out.print ("*")
      }
      Env.cur.out.printLine ("")
    }
  }
}

```


Using range objects:


```fantom

class ForLoops
{
  public static Void main ()
  {
    (1..5).each |i|
    {
      (1..i).each |j|
      {
         Env.cur.out.print ("*")
      }
      Env.cur.out.printLine ("")
    }
  }
}

```



## FOCAL

When the program exits the outer loop, the control variable <tt>I</tt> is set to 4 + 1 = 5; we can therefore permit execution to fall through into the inner loop for one more iteration.

```focal
01.10 FOR I=1,4; DO 2.0

02.10 FOR J=1,I; TYPE "*"
02.20 TYPE !
```

{{out}}

```txt

*
**
***
****
*****
```



## Forth


```forth
: triangle ( n -- )
  1+ 1 do
    cr i 0 do [char] * emit loop
  loop ;
5 triangle
```

One more:

```forth

: limit_example
        15 1 do r> r@ dup rot >r drop \ Bring limit on stack
                . \ And print it
        loop ;
\ Gforth and JSForth all work, SP-Forth brakes (different 'for' implementation?)

```



## Fortran

{{works with|Fortran|77 and later}}

```fortran
C     WARNING: This program is not valid ANSI FORTRAN 77 code. It uses
C     one nonstandard character on the line labelled 5001. Many F77
C     compilers should be okay with it, but it is *not* standard.
      PROGRAM FORLOOP
        INTEGER I, J

        DO 20 I = 1, 5
          DO 10 J = 1, I
C           Print the asterisk.
            WRITE (*,5001) '*'
   10     CONTINUE
C         Print a newline.
          WRITE (*,5000) ''
   20   CONTINUE

        STOP

 5000   FORMAT (A)
C       Standard FORTRAN 77 is completely incapable of completing a
C       WRITE statement without printing a newline. If you wanted to
C       write this program in valid F77, you would have to come up with
C       a creative way of printing varying numbers of asterisks in a
C       single write statement.
C
C       The dollar sign at the end of the format is a nonstandard
C       character. It tells the compiler not to print a newline. If you
C       are actually using FORTRAN 77, you should figure out what your
C       particular compiler accepts. If you are actually using Fortran
C       90 or later, you should replace this line with the commented
C       line that follows it.
 5001   FORMAT (A, $)
C5001   FORMAT (A, ADVANCE='NO')
      END
```


{{works with|Fortran|90 and later}}

```fortran
DO i = 1, 5
  DO j = 1, i
    WRITE(*, "(A)", ADVANCE="NO") "*"
  END DO
  WRITE(*,*)
END DO
```


Fortran 95 (and later) has also a loop structure that can be used only when the result is independent from real order of execution of the loop.

{{works with|Fortran|95 and later}}

```fortran
integer :: i
integer, dimension(10) :: v

forall (i=1:size(v)) v(i) = i
```


But if one accepts that a do-loop can be expressed without the actual word "do" (or "for"), then

```Fortran

      DO 1 I = 1,5
    1 WRITE (6,*) ("*", J = 1,I)
      END

```

That is a complete programme, though a more polite source file would have INTEGER I,J. It uses the old-style DO ''label'' etc. style of DO-loop to save on having to specify an END DO. The WRITE statement's output list is generated by an "implied" DO-loop having much of the form of DO J = 1,I and is indeed a proper loop. The output item is a text literal, which in earlier Fortran was unknown, however the result can still be achieved:

```Fortran

      DO 1 I = 1,5
    1 WRITE (6,2) (666, J = 1,I)
    2 FORMAT(5I1)
      END

```

This works because if a value cannot be fitted into its output field, the field is filled with asterisks. Which, is what is wanted! Just allow one digit for output (I1), and present a large integer.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

For i As Integer = 1 To 5
  For j As Integer = 1 To i
    Print "*";
  Next
  Print
Next

Sleep
```


{{out}}

```txt

*
**
***
****
*****

```



## Frink


```frink

for n = 1 to 5
{
   for a = 1 to n
      print["*"]

   println[]
}

```



## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so this example will not compile}}

Futhark does not have I/O, so this program simply counts in the
inner loop.


```Futhark

fun main(n: int): [n]int =
  loop (a = replicate n 0) = for i < n do
    (loop (s = 0) = for j < i+1 do
     s + j
     let a[i] = s
     in a)
  in a

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=9be4a2a6e69b2bf2921279a057df18c5 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim i, j As Integer

For i = 1 To 5
   For j = 1 To i
      Print "*";
   Next
   Print
Next

End
```


```txt

*
**
***
****
*****

```



## GAP


```gap
for i in [1 .. 5] do
    for j in [1 .. i] do
        Print("*");
    od;
    Print("\n");
od;

# *
# **
# ***
# ****
# *****
```



## GML


```GML
pattern = ""
for(i = 1; i <= 5; i += 1)
    {
    for(j = 1; j <= i; j += 1)
        {
        pattern += "*"
        }
    pattern += "#"
    }
show_message(pattern)
```



## Go


```go
package main

import "fmt"

func main() {
    for i := 1; i <= 5; i++ {
        for j := 1; j <= i; j++ {
            fmt.Printf("*")
        }
        fmt.Printf("\n")
    }
}
```

{{out}}

```txt

*
**
***
****
*****

```



## Groovy

Solution:

```groovy
for(i in (1..6)) {
    for(j in (1..i)) {
        print '*'
    }
    println ()
}
```


=={{header|GW-BASIC}}==

```qbasic
100 FOR I=1 TO 5
110 FOR J=1 TO I
120 PRINT "*";
130 NEXT J
140 PRINT
150 NEXT I
160 END
```


{{out}}

```txt

*
**
***
****
*****

```



## Hack


```hack
for($i = 0; $i < 5; $i++) {
    for($j = 0; $j <= $i; $j++) {
        echo '*';
    }

    echo '\n';
}
```



## Haskell


```haskell
import Control.Monad

main = do
  forM_ [1..5] $ \i -> do
    forM_ [1..i] $ \j -> do
      putChar '*'
    putChar '\n'
```


But it's more Haskellish to do this without loops:


```haskell
import Data.List (inits)

main = mapM_ putStrLn $ tail $ inits $ replicate 5 '*'
```



## Haxe



```Haxe
for (i in 1...6) {
	for(j in 0...i) {
		Sys.print('*');
	}
	Sys.println('');
}
```



## hexiscript


```hexiscript
for let i 1; i <= 5; i++
  for let j 1; j <= i; j++
    print "*"
  endfor
  println ""
endfor
```



## HicEst


```hicest
DO i = 1, 5
  DO j = 1, i
    WRITE(APPend) "*"
  ENDDO
  WRITE() ' '
ENDDO
```



## HolyC


```holyc
U8 i, j;
for (i = 1; i <= 5; i++) {
  for (j = 1; j <= i; j++)
    Print("*");
  Print("\n");
}
```


== Icon and Unicon ==
=
## Icon
=

```Icon
procedure main()
every i := 1 to 5 do {
   every 1 to i do
      writes("*")
   write()
   }
end
```

=
## Unicon
=
The Icon solution works in Unicon.


## Inform 7


```inform7
repeat with length running from 1 to 5:
	repeat with N running from 1 to length:
		say "*";
	say line break;
```



## J

J is array-oriented, so there is very little need for loops.  For example, except for the requirement for loops, one could satisfy this task this way:

   ]\ '*****'

J does support loops for those times they can't be avoided (just like many languages support gotos for those time they can't be avoided).

```j
3 : 0
        for_i. 1 + i. y do.
             z =. ''

             for. 1 + i. i do.
                  z=. z,'*'
             end.

             z 1!:2 ] 2
         end.

        i.0 0
   )
```


But you would almost never see J code like this.


## Java


```java
for (int i = 0; i < 5; i++) {
   for (int j = 0; j <= i; j++) {
      System.out.print("*");
   }
   System.out.println();
}
```



## JavaScript


```javascript
var i, j;
for (i = 1; i <= 5; i += 1) {
  s = '';
  for (j = 0; j < i; j += 1)
    s += '*';
  document.write(s + '
');
}
```



Alternatively, using JavaScript's '''Array.forEach()''', and given an array of indices,
or a simple range function which generates a range:


```JavaScript
function range(i) {
  return i ? range(i - 1).concat(i) : [];
}

range(5) --> [1, 2, 3, 4, 5]
```


We could write something like:


```JavaScript
var s = '';

range(5).forEach(
  function (line) {
    range(line).forEach(
      function () { s += '*'; }
    );
    s += '\n';
  }
);

console.log(s);
```


but it might be more natural in JavaScript, if we are going to use built-in Array functions, to simplify a little with '''Array.reduce()''', writing:


```JavaScript
console.log(
  range(5).reduce(
    function (a, n) {
      return a + Array(n + 1).join('*') + '\n';
    }, ''
  )
);
```


in which the inner ''n'' refers to the Array value visited at the next level out, and the triangle is returned as a single expression, rather than as a series of variable mutations.

Finally, in contexts where an expression composes better than a statement, the effect of a loop can often be expressed as a map.


```JavaScript
console.log(
  range(5).map(function(a) {
    return Array(a + 1).join('*');
  }).join('\n')
);
```



## jq


```jq
# Single-string version using explicit nested loops:
def demo(m):
  reduce range(0;m) as $i
    (""; reduce range(0;$i) as $j
           (.; . + "*" )  + "\n" ) ;

# Stream of strings:
def demo2(m):
  range(1;m)
  | reduce range(0;.) as $j (""; . + "*");

# Variation of demo2 using an implicit inner loop:
def demo3(m): range(1;m) | "*" * . ;
```

'''Example using demo(6)'''
{{Out}}
 $ jq -r -n -f loops_for.jq
 *
 **
 ***
 ****
 *****


## Jsish

Code from Javascript entry.

```javascript
var i, j, s;
for (i = 1; i <= 5; i += 1) {
    s = '';
    for (j = 0; j < i; j += 1) s += '*';
    puts(s);
}
```


{{out}}

```txt
prompt$ jsish forloop.jsi
*
**
***
****
*****
```



## Julia


```Julia

for i in 1:5
    for j in 1:i
        print("*")
    end
    println()
end

```


{{out}}

```txt
*
**
***
****
*****
```



## Klong


```K

:" x{p}:*y means repeat {p} x times starting at y "

5{x{.d(0c*);x}:*0;.p("");x+1}:*1

:" But you would not do it like this! "
:" You would reshape 0c* to the desired length "
:" in a function and then iterate that function "
:" over a vector of numbers: "

{.p(x:^0c*)}'1+!5

```



## Kotlin


```Kotlin
fun main(args: Array<String>) {
    (1..5).forEach {
        (1..it).forEach { print('*') }
        println()
    }
}
```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW_Loops_For.png]]


## Lang5


```lang5
: cr  "\n" . ;      : dip  swap '_ set execute _ ;
: nip  swap drop ;  : last  -1 extract nip ;
: times
    swap iota '_ set
    do   dup 'execute dip _ last 0 == if break then
    loop drop ;

: concat  "" join ;
'* 1 5 "2dup reshape concat . cr 1 +" times
```



## Langur


```Langur
for .i = 0; .i < 5; .i += 1 {
    for .j = 0; .j <= .i; .j += 1 {
        write "*"
    }
    writeln()
}
```



```Langur
for .i of 5 {
    for of .i {
        write "*"
    }
    writeln()
}
```


Or, with one for loop...

```Langur
for .i of 5 {
    writeln "*" x .i
}
```



## Lasso


```Lasso
loop(5) => {^
    loop(loop_count) => {^ '*' ^}
    '\r'
^}
```



## LC3 Assembly


```lc3asm
      .ORIG      0x3000

      AND        R1,R1,0
      ADD        R1,R1,1
      AND        R5,R5,0
      ADD        R5,R5,5
      NOT        R5,R5

LOOPI LD         R0,STAR
      AND        R2,R2,0
      ADD        R3,R1,0

LOOPJ OUT
      ADD        R2,R2,1
      NOT        R4,R2
      ADD        R4,R3,R4
      BRZP       LOOPJ

      LD         R0,LF
      OUT

      ADD        R1,R1,1
      ADD        R4,R1,R5
      BRN        LOOPI

      HALT

STAR  .FILL      0x2A
LF    .FILL      0x0A

      .END
```

Output:

```txt
*
**
***
****
*****
```



## LIL

In LIL '''for''' takes a before loop code block for init, a conditional expression (true to enter loop step, false to exit loop), an after each loop step code block for value reassignment, followed by the code for the loop.


```tcl
for {set i 1} {$i <= 5} {inc i} {
    for {set j 1} {$j <= $i} {inc j} {
        write "*"
    }
    print
}
```


{{out}}

```txt
prompt$ lil loopsFor.lil
*
**
***
****
*****
```


The '''for''' statement in LIL, like Tcl, is pretty flexible and is not limited to simple ''incremented variable'' style loops.


## Lingo


```lingo
repeat with i = 1 to 5
  str = ""
  repeat with j = 1 to i
    put "*" after str
  end repeat
  put str
end repeat
```



## Lisaac


```Lisaac
1.to 5 do { i : INTEGER;
  1.to i do { dummy : INTEGER;
    '*'.print;
  };
  '\n'.print;
};
```



## LiveCode


```LiveCode
put 0 into n
repeat for 5 times
  add 1 to n
  repeat for n times
    put "*"
  end repeat
  put return
end repeat
```



## Logo


```logo
for [i 1 5] [repeat :i [type "*] (print)]
repeat 5 [repeat repcount [type "*] (print)]
```



## Lua


```lua

for i=1,5 do
  for j=1,i do
    io.write("*")
  end
  io.write("\n")
end

```



## M2000 Interpreter

By default there For loops always perform on execution of block. If end value is smaller than fist value, then step adjust to that direction. When first value is equal to second value then if we declare step negative end value after execution of block became start value minus absolute step, or if step is positive, became start value plus step. We can use a switch for interpreter to change IF's STEP to act as BASIC's, and sign of step always used, and there is situations where block can't executed.


```M2000 Interpreter

For i=1 to 5
      For j=1 to i
            Print "*";
      Next j
      Print
Next i
Print "End1"
For i=1 to 5 {
      For j=1 to i {
            Print "*";
      }
      Print
}
Print "End2"

```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl

for(`x',`1',`5',`1',
   `for(`y',`1',x,`1',
      `*')
')
```



## make

{{works with|BSD make}}
{{libheader|jot}}

```make
all: line-5

ILIST != jot 5
.for I in $(ILIST)

line-$(I): asterisk-$(I)-$(I)
	@echo

JLIST != jot $(I)
. for J in $(JLIST)

.  if "$(J)" == "1"
.   if "$(I)" == "1"
asterisk-1-1:
.   else
IM != expr $(I) - 1
asterisk-$(I)-1: line-$(IM)
.   endif
.  else
JM != expr $(J) - 1
asterisk-$(I)-$(J): asterisk-$(I)-$(JM)
.  endif
	@printf \*

. endfor
.endfor
```



## Maple


```Maple>
 for i to 5 do to i do printf( "*" ) end; printf( "\n" ) end;
*
**
***
****
*****
```




## Mathematica


```Mathematica
n=5;
For[i=1,i<=5,i++,
 string="";
 For[j=1,j<=i,j++,string=string<>"*"];
 Print[string]
]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
for i = (1:5)
    output = [];
    for j = (1:i)
        output = [output '*'];
    end
    disp(output);
end
```


Vectorized version:


```MATLAB
for i = (1:5)
    disp(repmat('*',1,i));
end
```



## Maxima



```maxima
for i thru 5 do (
   s: "",
   thru i do s: sconcat(s, "*"),
   print(s)
);
```



## MAXScript


```maxscript
for i in 1 to 5 do
(
    line = ""
    for j in 1 to i do
    (
        line += "*"
    )
    format "%\n" line
)
```



## Mercury

<lang>:- module loops_for.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
   int.fold_up(outer_loop_body, 1, 5, !IO).

:- pred outer_loop_body(int::in, io::di, io::uo) is det.

outer_loop_body(I, !IO) :-
   int.fold_up(inner_loop_body, 1, I, !IO),
   io.nl(!IO).

:- pred inner_loop_body(int::in, io::di, io::uo) is det.

inner_loop_body(_, !IO) :-
   io.write_char('*', !IO).
```



## MiniScript

Literal interpretation of the task is somewhat complicated by the fact that the standard implementation of <code>print</code> in MiniScript adds a line break:

```MiniScript
for i in range(1,5)
    s = ""
    for j in range(1, i)
        s = s + "*"
    end for
    print s
end for
```


{{out}}

```txt
*
**
***
****
*****
```


However, it is worth noting that MiniScript's string replication operator (*) makes a more natural solution possible:


```MiniScript
for i in range(1,5)
    print "*" * i
end for
```


(Output same as above.)

=={{header|Modula-2}}==

```modula2
MODULE For;
  IMPORT InOut;

  VAR
    i, j: INTEGER;

BEGIN
  FOR i := 1 TO 5 DO
    FOR j := 1 TO i DO
      InOut.Write('*');
    END;
    InOut.WriteLn
  END
END For.
```


=={{header|Modula-3}}==

```modula3
MODULE Stars EXPORTS Main;

IMPORT IO;

BEGIN
  FOR i := 1 TO 5 DO
    FOR j := 1 TO i DO
      IO.Put("*");
    END;
    IO.Put("\n");
  END;
END Stars.
```



## MOO


```moo
for i in [1..5]
  s = "";
  for j in [1..i]
    s += "*";
  endfor
  player:tell(s);
endfor
```



## Morfa


```morfa

import morfa.base;

for (i in 0..5)
{
    for (j in 0..i+1)
    {
        print("*");
    }
    println("");
}

```



## MUMPS


###  Routine


```MUMPS
FORLOOP
 NEW I,J
 FOR I=1:1:5 DO
 .FOR J=1:1:I DO
 ..WRITE "*"
 .WRITE !
 QUIT
```

{{out}}

```txt

USER>D FORLOOP^ROSETTA
*
**
***
****
*****

```



###  One line

The if statement has to follow the write, or else the if statement would control the write (5 lines with one asterisk each).

```MUMPS
FOR I=1:1:5 FOR J=1:1:I WRITE "*" IF J=I W !
```



## Nanoquery


```nanoquery
for ($i = 1) ($i <= 5) ($i = $i+1)
    for ($j = 0) ($j < $i) ($j = $j+1)
        print "*"
    end for
    println
end for
```



## Nemerle


```Nemerle
for (int i = 0; i < 5; i++)
{
    for (int j = 0; j <= i; j++)
    {
        Write("*");
    }
    WriteLine();
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/For'

  loop i_ = 1 to 5
    loop for i_
      say '*\-'
      end
    say
    end i_

```



## NewLISP


```NewLISP

(for (i 1 5)
  (for(j 1 i)
    (print "*"))
  (print "\n"))

```



## Nim


```Python
for i in 1..5:
  for j in 1..i:
    stdout.write("*")
  echo("")
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASUC>10 FOR I=1 TO 5
20 FOR J=1 TO I
30 PRINT "*";
40 NEXT
50 PRINT
60 NEXT
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE LoopFor;
IMPORT
  Out;
VAR
  i, j: INTEGER;

BEGIN
  FOR i := 1 TO 5 DO
    FOR j := 1 TO i DO
      Out.Char('*');
    END;
    Out.Ln
  END
END LoopFor.

```



## Objeck


```objeck

bundle Default {
  class For {
    function : Main(args : String[]) ~ Nil {
      DoFor();
    }

    function : native : DoFor() ~ Nil {
    	for (i := 0; i < 5; i += 1;) {
          for (j := 0; j <= i; j += 1;) {
            "*"->Print();
          };
          ""->PrintLine();
       };
    }
  }
}

```



## OCaml


```ocaml
for i = 1 to 5 do
  for j = 1 to i do
    print_string "*"
  done;
  print_newline ()
done
```



## Octave


```octave
for i = 0:1:4
  for j = 0:1:i
    printf("*");
  endfor
  printf("\n");
endfor
```



## Oforth


```Oforth
: loopFor(n)
| i j |
   n loop: i [
      i loop: j [ "*" print ]
      printcr ;
```



## Onyx


```onyx
1 1 5 {dup {`*'} repeat bdup bpop ncat `\n' cat print} for flush
```

Using repeat inside the for loop instead of nesting another for loop is shorter and more efficient.


## Order


```c
#include <order/interpreter.h>

ORDER_PP(
  8for_each_in_range(8fn(8I,
                         8print(
                           8for_each_in_range(8fn(8J, 8print((*))),
                                              1, 8plus(8I, 1))
                           8space)),
                         1, 6)
)
```

(Order cannot print newlines, so this example just uses a space.)


## Oz


```oz
for I in 1..5 do
   for _ in 1..I do
      {System.printInfo "*"}
   end
   {System.showInfo ""}
end
```

Note: we don't use the inner loop variable, so we prefer not to give it a name.

## Panoramic


```Panoramic

dim x,y

for x=1 to 5

    for y=1 to x

    print "*";

    next y

print

next x

```



## PARI/GP


```parigp
for(a=1,5,for(b=1,a,print1("*"));print())
```



## Pascal


```pascal
program stars(output);

var
  i, j: integer;

begin
  for i := 1 to 5 do
    begin
      for j := 1 to i do
        write('*');
      writeln
    end
end.
```



## Perl


```perl
for(my $x = 1; $x <= 5; $x++) {
  for(my $y = 1; $y <= $x; $y++) {
    print "*";
  }
  print "\n";
}
```


```perl
foreach (1..5) {
  foreach (1..$_) {
    print '*';
  }
  print "\n";
}
```


However, if we lift the constraint of two loops the code will be simpler:


```perl
print ('*' x $_ . "\n") for 1..5;
```


or equivalently


```perl
map {print '*' x $_ . "\n"} 1..5;
```



## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}


```perl6
for ^5 {

	for 0..$_ {
		print "*";
	}

	print "\n";

}
```


or using only one for loop:


```perl6
say '*' x $_ for 1..5;
```


or without using any loops at all:


```perl6
([\~] "*" xx 5).join("\n").say;
```



## Phix


```Phix
for i=1 to 5 do
    for j=1 to i do
        puts(1,"*")
    end for
    puts(1,"\n")
end for
```



## PHP


```php
for ($i = 1; $i <= 5; $i++) {
  for ($j = 1; $j <= $i; $j++) {
    echo '*';
  }
  echo "\n";
}
```

or

```php
foreach (range(1, 5) as $i) {
  foreach (range(1, $i) as $j) {
    echo '*';
  }
  echo "\n";
}
```

or

```php
foreach (range(1, 5) as $i)
  echo str_repeat('*', $i) , PHP_EOL;
```



## PicoLisp


```PicoLisp
(for N 5
   (do N (prin "*"))
   (prinl) )
```



## Pike


```pike
int main(){
   for(int i = 1; i <= 5; i++){
      for(int j=1; j <= i; j++){
         write("*");
      }
      write("\n");
   }
}
```



## PILOT

Core PILOT does not offer any way of printing without a newline, so in the inner loop we concatenate another star onto the string variable <code>$stars</code> each time round and then print it in the outer loop.

```pilot
C  :i = 1
*OuterLoop
C  :j = 0
C  :$stars =
*InnerLoop
C  :j = j + 1
C  :$stars =*$stars
J ( j < i )          :*InnerLoop
T  :$stars
C  :i = i + 1
J ( i < 6 )          :*OuterLoop
END:
```



## PL/I

Basic version:

```PL/I
do i = 1 to 5;
   do j = 1 to i;
      put edit ('*') (a);
   end;
   put skip;
end;
```

Advanced version:

```PL/I
do i = 1 to 5;
   put skip edit (('*' do j = 1 to i)) (a);
end;
```

Due to the new line requirement a mono line version is not possible

```PL/I
put edit ((('*' do j = 1 to i)do i=1 to 5))(a);  /* no new line */
```



## Pop11


```pop11
lvars i, j;
for i from 1 to 5 do
    for j from 1 to i do
        printf('*','%p');
    endfor;
    printf('\n')
endfor;
```


## PowerShell


```powershell
for ($i = 1; $i -le 5; $i++) {
    for ($j = 1; $j -le $i; $j++) {
        Write-Host -NoNewline *
    }
    Write-Host
}
```

Alternatively the same can be achieved with a slightly different way by using the range operator along with the <code>ForEach-Object</code> cmdlet:

```powershell
1..5 | ForEach-Object {
    1..$_ | ForEach-Object {
        Write-Host -NoNewline *
    }
    Write-Host
}
```

while the inner loop wouldn't strictly be necessary and can be replaced with simply <code>"*" * $_</code>.


## Processing


```java
size( 105,120 );

for ( int i=20; i<=100; i+=20 )
   for ( int j=10; j<=i; j+=20 )
      text( "*", j,i );
```



## Prolog

Prolog has a built in iterator, between(Lo,Hi,I) which binds the value of I to successive values from Lo to Hi.  This is the closest thing Prolog has to a 'for' loop.

```prolog
example :-
    between(1,5,I), nl, between(1,I,_J),
    write('*'), fail.
example.
```


```txt
?- example.

*
**
***
****
*****
true.
```



## Python


```python
import sys
for i in xrange(5):
    for j in xrange(i+1):
        sys.stdout.write("*")
    print
```

Note that we have a constraint to use two for loops, which leads to non-idiomatic Python. If that constraint is dropped we can use the following, more idiomatic Python solution:

```python
for i in range(1,6):
    print '*' * i
```

or

```python
print('\n'.join('*' * i for i in range(1, 6)))
```



## R


```R
for(i in 0:4) {
  s <- ""
  for(j in 0:i) {
    s <- paste(s, "*", sep="")
  }
  print(s)
}
```



## Racket


```racket
(for ([i (in-range 1 6)]) (for ([j i]) (display "*")) (newline))
```



## REBOL


```REBOL
; Use 'repeat' when an index required, 'loop' when repetition suffices:

repeat i 5 [
	loop i [prin "*"]
	print ""
]

; or a more traditional for loop:

for i 1 5 1 [
	loop i [prin "*"]
	print ""
]
```



## Retro


```Retro
6 [ 0; cr [ '* emit ] times ] iter
```



## REXX


### using concatenation


```rexx
/*REXX program demonstrates an outer DO loop controlling the inner DO loop with a "FOR".*/

       do j=1  for 5                             /*this is the same as:   do j=1  to 5  */
       $=                                        /*initialize the value to a null string*/
              do k=1  for j                      /*only loop for a   J   number of times*/
              $= $ || '*'                        /*using concatenation  (||)  for build.*/
              end   /*k*/
       say $                                     /*display character string being built.*/
       end          /*j*/                        /*stick a fork in it,  we're all done. */
```

{{out|output|:}}

```txt

*
**
***
****
*****

```



### using abutment


```rexx
/*REXX program demonstrates an outer DO loop controlling the inner DO loop with a "FOR".*/

       do j=1  for 5                             /*this is the same as:   do j=1  to 5  */
       $=                                        /*initialize the value to a null string*/
              do k=1  for j                      /*only loop for a   J   number of times*/
              $= $'*'                            /*using abutment for the construction. */
              end   /*k*/
       say $                                     /*display character string being built.*/
       end          /*j*/                        /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ring

can be done in just one line:

```ring

for i = 1 to 5 for x = 1 to i see "*" next see nl next

```

or multiple line

```ring

for i = 1 to 5
     for x = 1 to i
         see "*"
     next
     see nl
next

```



## Ruby

One can write a <tt>for</tt> loop as <tt>for i in 1..5; ...end</tt> or as <tt>for i in 1..5 do ... end</tt> or as <tt>(1..5).each do |i| ... end</tt>. All three forms call <tt>Range#each</tt> to iterate <tt>1..5</tt>.

{| class="wikitable"
! for
! Range#each
|-
| style="vertical-align: top;" |

```ruby
for i in 1..5
  for j in 1..i
    print "*"
  end
  puts
end
```

| style="vertical-align: top;" |

```ruby
(1..5).each do |i|
  (1..i).each do |j|
    print "*"
  end
  puts
end
```

|}

Ruby has other ways to code these loops; <tt>Integer#upto</tt> is most convenient.

{| class="wikitable"
! Integer#upto
! Integer#times
! Kernel#loop
|-
| style="vertical-align: top;" |

```ruby
1.upto(5) do |i|
  1.upto(i) do |j|
    print "*"
  end
  puts
end
```

| style="vertical-align: top;" |

```ruby
5.times do |i|
  # i goes from 0 to 4
  (i+1).times do
    print "*"
  end
  puts
end
```

| style="vertical-align: top;" |

```ruby
i = 1
loop do
  j = 1
  loop do
    print "*"
    break if (j += 1) > i
  end
  puts
  break if (i += 1) > 5
end
```

|}

Or we can use String#* as the inner loop, and Enumerable#map as the outer loop. This shrinks the program to one line.


```ruby
puts (1..5).map { |i| "*" * i }
```



## Rust

The compiler warns when you create an unused variable; here we use _ to avoid this effect.

```rust
fn main() {
    for i in 0..5 {
        for _ in 0..=i {
            print!("*");
        }

        println!();
    }
}
```



## Salmon


```Salmon
iterate (x; [0...4])
  {
    iterate (y; [0...x])
        print("*");;
    print("\n");
  };
```


or


```Salmon
for (x; 0; x < 5)
  {
    for (y; 0; y <= x)
        print("*");;
    print("\n");
  };
```




## SAS


```sas
data _null_;
length a $5;
do n=1 to 5;
  a="*";
  do i=2 to n;
    a=trim(a) !! "*";
  end;
  put a;
end;
run;

/* Possible without the inner loop. Notice TRIM is replaced with STRIP,
otherwise there is a blank space on the left */

data _null_;
length a $5;
do n=1 to 5;
  a=strip(a) !! "*";
  put a;
end;
run;
```



## Sather


Sather allows the definition of new iterators. Here's we define <code>for!</code> so that it resembles the known <code>for</code> in other languages, even though the <code>upto!</code> built-in can be used.


```sather
class MAIN is
  -- from, to, step
  for!(once init:INT, once to:INT, once inc:INT):INT is
    i ::= init;
    loop while!( i <= to );
      yield i;
      i := i + inc;
    end;
  end;

  main is
    i, j :INT;
    loop i := for!(1, 5, 1);   -- 1.upto!(5)
      loop j := for!(1, i, 1); -- 1.upto!(i)
        #OUT + "*";
      end;
      #OUT + "\n";
    end;
  end;
end;

```



## Scala


```scala
for (i <- 1 to 5) {
    for (j <- 1 to i)
        print("*")
    println()
}
```



## Scheme


```scheme
(do ((i 1 (+ i 1)))
    ((> i 5))
    (do ((j 1 (+ j 1)))
        ((> j i))
        (display "*"))
    (newline))
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>for i=1:5
    s=""
    for j=1:i
        s=s+"*"
    end
    printf("%s\n",s)
end
```

{{out}}

```txt
*
**
***
****
*****
```



## Seed7


```seed7
for I range 1 to 5 do
  for J range 1 to I do
    write("*");
  end for;
  writeln;
end for;
```



## SETL


```ada
for i in {1..5} loop
    for j in {1..i} loop
        nprint( '*' );
    end loop;
    print;    -- new line
end loop;
```



## Sidef

'''for(;;)''' loop:

```ruby
for (var i = 1; i <= 5; i++) {
    for (var j = 1; j <= i; j++) {
        print '*'
    }
    print "\n"
}
```


'''for([])''' loop:

```ruby
for (1..5) { |i|
    for (1..i) { print '*' }
    print "\n"
}
```


'''for-in''' loop:

```ruby
for i in (1..5) {
    for j in (1..i) { print '*' }
    print "\n"
}
```


Idiomatic:

```ruby
5.times { |i|
    i.times { print '*' }
    print "\n"
}
```



## Simula

{{works with|SIMULA-67}}

```simula
begin
   integer i,j;
   for i:=1 step 1 until 5 do
   begin
      for j:=1 step 1 until i do
         outtext("*");
      outimage
   end
end

```



## Slate


```slate
1 to: 5 do: [| :n | inform: ($* repeatedTimes: n)].
```



## Smalltalk


```smalltalk
1 to: 5 do: [ :aNumber |
  aNumber timesRepeat: [ '*' display ].
  Character nl display.
]
```

or:

```smalltalk
1 to: 5 do: [ :row |
  1 to: row do: [:col | '*' display ].
]
```

(only for demonstration of nested for-loops; as the column is not needed, the first solution is probably clearer).

However, streams already have some builtin repetition mechanism, so a programmer might write:
{{works with|Smalltalk/X}}

```smalltalk
1 to: 5 do: [ :n |
  Stdout next: n put: $*; cr
]
```



## SNOBOL4

A slightly longer, "mundane" version


```snobol
ol	outer = ?lt(outer,5) outer + 1	:f(end)
	inner = outer; stars = ""
il	stars = ?gt(inner,0) stars "*"	:f(disp)
	inner = inner - 1	:(il)
disp	output = stars;	:(ol)
end
```


The "real SNOBOL4" starts here:

```snobol
outer	b = a = ?lt(a,5) a + 1	:f(end)
inner	t = t ?(b = (gt(b,0) b - 1)) "*"	:s(inner)
	t span("*") . terminal = 	:(outer)
end
```


one "loop" only:

```snobol
	a = "*****";
a	a len(x = x + 1) . output	:s(a)
end
```


... or just (courtesy of GEP2):
{{works with|SNOBOL4|which defaults to anchored mode}}

```snobol
        "*****" arb $ output fail
end
```


## SNUSP



        / \
          <
        < <
        < /<<<<<.\
        . ?
        > \->>>>>/
        > !
        > >
        ! >
     />-\ />+>+\
        ? ?
     \+</ \ -<</
        < !
        < +
        - >
   /   !\?/#
   \+++++>+++++++++++++>\
 /++++++++++++++++++++++/
 \++++++++++++++++++++\
                     $/


```txt

*
**
***
****
*****

```


## Sparkling


```sparkling
for (var row = 1; row <= 5; row++) {
    for (var col = 1; col <= row; col++) {
        printf("*");
    }

    print();
}
```



## Spin

{{works with|BST/BSTC}}
{{works with|FastSpin/FlexSpin}}
{{works with|HomeSpun}}
{{works with|OpenSpin}}

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | m, n
  ser.start(31, 30, 0, 115200)

  repeat n from 1 to 5
    repeat m from 1 to n
      ser.tx("*")
    ser.str(string(13,10))

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

{{out}}

```txt

*
**
***
****
*****

```



## SPL


```spl>
 i, 1..5
  > j, 1..i
    #.output("*",#.rs)
  <
  #.output()
<
```



## Stata



```stata
forvalues n=1/5 {
	local s ""
	forvalues i=1/`n' {
		local s `s'*
	}
	display "`s'"
}
```



###  Mata


```stata
for (i=1; i<=5; i++) {
	for (j=1; j<=i; j++) printf("*")
	printf("\n")
}
```



## Suneido


```Suneido
for(i = 0; i < 5; ++i)
    {
    str = ''
    for (j = 0; j <= i; ++j)
        str $= '*'
    Print(str)
    }
```



## Swift


```swift
for i in 1...5 {
    for _ in 1...i {
        print("*", terminator: "")
    }
    print()
}
```

{{out}}

```txt

*
**
***
****
*****

```



## Tcl


```tcl
for {set lines 1} {$lines <= 5} {incr lines} {
    for {set i 1} {$i <= $lines} {incr i} {
        puts -nonewline "*"
    }
    puts ""
}
```

Note that it would be more normal to produce this output with:

```tcl
for {set i 1} {$i <= 5} {incr i} {
    puts [string repeat "*" $i]
}
```


It bears noting that the three parts of the for loop do not have to consist of "initialize variable", "test value of variable" and "increment variable". This is a common way to think of it as it resembles the "for" loop in other languages, but many other things make sense. For example this for-loop will read a file line-by-line:


```tcl
set line ""
for { set io [open test.txt r] } { ![eof $io] } { gets $io line } {
    if { $line != "" } { ...do something here... }
}
```


(This is a somewhat awkward example; just to show what is possible)

=={{header|TI-83 BASIC}}==
For loops in TI-83 BASIC take at least 3 arguments, with an optional fourth: For(variable,start,end[,step]. Parentheses don't need to be closed in TI-BASIC.

 ClrHome
 For(I,1,5
 For(J,1,I
 Output(I,J,"*
 End
 End

=={{header|TI-89 BASIC}}==

```ti89b
Local i,j
ClrIO
For i, 1, 5
  For j, 1, i
    Output i*8, j*6, "*"
  EndFor
EndFor
```



## TorqueScript



```Torque
for(%i = 0; %i < 5; %i++)
{
    for(%x = %i; %x < 5; %x++)
    {
        %string = %string @ "*";
        echo(%string);
    }
}
```



## TransFORTH


```forth
: PRINTSTARS ( ROWS -- )
1 + 1 DO
I 0 DO
PRINT " * " LOOP
CR LOOP ;
5 PRINTSTARS
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
m=""
LOOP n=1,5
 m=APPEND (m,"","*")
 PRINT m
ENDLOOP

```

{{out}}

```txt

*
**
***
****
*****

```




## TypeScript


```JavaScript
for (let i: number = 0; i < 5; ++i) {
    let line: string = ""
    for(let j: number = 0; j <= i; ++j) {
        line += "*"
    }
    console.log(line)
}

```



## UNIX Shell

A conditional loop, using a while control construct, can have the same effect as a for loop. (The original Bourne Shell has no <code>echo -n "*"</code>, so this uses <code>printf "*"</code>.)

{{works with|Bourne Shell}}

```bash
#!/bin/sh
# Using a while control construct to emulate a for loop

l="1"                   # Set the counters to one
while [ "$l" -le 5 ]    # Loop while the counter is less than five
  do
  m="1"
  while [ "$m" -le "$l" ]  # Loop while the counter is less than five
    do
    printf "*"
    m=`expr "$m" + 1`   # Increment the inner counter
  done
  echo
  l=`expr "$l" + 1`   # Increment the outer counter
done
```


The [[Bourne Shell]] has a for loop, but it requires a list of words to iterate.
The [http://www.openbsd.org/cgi-bin/man.cgi?query=jot&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html <tt>jot(1)</tt>] command from [[BSD]] can output an appropriate list of numbers.

{{works with|Bourne Shell}}
{{libheader|jot}}

```bash
for i in `jot 5`; do
	for j in `jot $i`; do
		printf \*
	done
	echo
done
```


Bash has <tt>for</tt> loops that act like C. These loops are very good for this task.

{{works with|Bourne Again SHell|3}}

```bash
for (( x=1; $x<=5; x=$x+1 )); do
  for (( y=1; y<=$x; y=$y+1 )); do
    echo -n '*'
  done
  echo ""
done
```


=
## C Shell
=
{{libheader|jot}}

```csh
foreach i (`jot 5`)
	foreach j (`jot $i`)
		echo -n \*
	end
	echo ""
end
```



## UnixPipes


```bash
yes \ | cat -n | (while read n ; do
  [ $n -gt 5 ] && exit 0;
  yes \* | head -n $n | xargs -n $n echo
done)
```



## Ursa


```ursa
#
# for loop
#

decl int i j
for (set i 0) (< i 5) (inc i)
        for (set j 0) (< j (int (+ i 1))) (inc j)
                out "*" console
        end for
        out endl console
end for
```



## Vala


```vala
int main (string[] args) {
    for (var i = 1; i <= 5; i++) {
        for (var j = 1; j <= i; j++) {
	    stdout.putc ('*');
        }
        stdout.putc ('\n');
    }
    return 0;
}
```



## VBA


```vb
Option Explicit
Sub LoopEx()
    Dim i As Long, j As Long, s As String
    For i = 1 To 5
        s = ""
        For j = 1 To i
            s = s + "*"
        Next
        Debug.Print s
    Next
End Sub
```


## VBScript


```vb
Option Explicit
Dim i, j, s
For i = 1 To 5
    s = ""
    For j = 1 To i
        s = s + "*"
    Next
    WScript.Echo s
Next
```



## Vedit macro language


```vedit
for (#1 = 1; #1 <= 5; #1++) {
    for (#2 = 1; #2 <= #1; #2++) {
        Type_Char('*')
    }
    Type_Newline
}
```



## Wart


```wart
for i 1 (i <= 5) ++i
  for j 0 (j < i) ++j
    pr "*"
  (prn)
```



## Wee Basic

print 1 "" ensures the end of program text is separate from the asterisk characters.

```Wee Basic
for y=0 to 4
print 1 ""
for x=0 to y
print 1 at x,y "*"
next
next
end
```



## x86 Assembly

This subroutine uses only the original 16-bit 8086 instruction set; it is written for DOS, but could be adapted to run under other operating systems.

```x86asm
loops:      mov       bx,      1    ; outer loop counter

outerloop:  mov       cx,      bx   ; inner loop counter
            mov       dl,      42   ; '*' character

innerloop:  mov       ah,      6
            int       21h           ; print

            dec       cx
            jcxz      innerdone
            jmp       innerloop

innerdone:  mov       dl,      10   ; newline
            mov       ah,      6
            int       21h

            inc       bx
            cmp       bx,      6
            jne       outerloop

            ret
```



## XLISP

The equivalent of other languages' <tt>FOR</tt> or <tt>DO</tt> loops can be written using <tt>DO</tt>:

```xlisp
(DO ((I 1 (+ I 1))) ((> I 5))
    (DO ((J 0 (+ J 1))) ((= J I))
        (DISPLAY "*"))
    (NEWLINE))
```

{{out}}

```txt
*
**
***
****
*****
```

This construct is not, however, very idiomatic: loops in XLISP are mostly written using recursion.


## XPL0


```XPL0
code ChOut=8, CrLf=9;
int I, J;
for I:= 1 to 5 do
    [for J:= 1 to I do
        ChOut(0, ^*);
    CrLf(0);
    ]
```



## Z80 Assembly

For the Amstrad CPC (should work with e.g. the built-in assembler in JavaCPC; use <tt>call &4000</tt> to start from BASIC):

```z80
org &4000		; put code at memory address 0x4000
wr_char equ &bb5a ; write ASCII character in register A to screen
				; (jumps into CPC ROM)

; put registers on stack so we can return to BASIC later
push bc
push de
push hl

ld b,5		; loop from 5 to 1

row:

push bc	; save outer loop variable

; calculate inner loop limit (6 - outer loop variable)
ld a,6
sub b
ld b,a

column:

ld a,42	; asterisk in ASCII
call wr_char
djnz column ; decrement B, jump to label if non-zero

pop bc	; restore outer loop

; print carriage return/line feed
ld a,13
call wr_char
ld a,10
call wr_char

djnz row

; restore registers
pop hl
pop de
pop bc
ret	; return to BASIC interpreter
```



## zkl


```zkl
foreach i in ([1..5]){
   foreach j in (i){print("*")}
   println();
}
```

{{out}}

```txt

*
**
***
****
*****

```
