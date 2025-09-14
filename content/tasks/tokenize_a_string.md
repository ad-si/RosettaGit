+++
title = "Tokenize a string"
description = ""
date = 2019-10-14T14:09:38Z
aliases = []
[extra]
id = 1904
[taxonomies]
categories = ["String manipulation", "Simple", "task"]
tags = []
languages = [
  "360_assembly",
  "acl2",
  "actionscript",
  "ada",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "arturo",
  "astro",
  "autohotkey",
  "awk",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "e",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "falcon",
  "fantom",
  "forth",
  "fortran",
  "frink",
  "gambas",
  "gap",
  "genie",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lang5",
  "ldpl",
  "lfe",
  "liberty_basic",
  "lingo",
  "logo",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "mercury",
  "min",
  "miniscript",
  "mmix",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "openedge_progress",
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
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "q",
  "qbasic",
  "r",
  "racket",
  "raven",
  "rebol",
  "red",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "self",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "swift",
  "tcl",
  "tr",
  "tuscript",
  "txr",
  "unix_shell",
  "unixpipes",
  "ursa",
  "ursala",
  "vala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic",
  "winbatch",
  "wortel",
  "xpath_2_0",
  "xpl0",
  "yabasic",
  "zkl",
  "zsh",
]
+++

## Task

Separate the string "Hello,How,Are,You,Today" by commas into an array (or list) so that each element of it stores a different word.
Display the words to the 'user', in the simplest manner possible,
separated by a period.
To simplify, you may display a trailing period.

'''''Related tasks:'''''

* [[Tokenize a string with escaping]]


## 360 Assembly


```360asm
*        Tokenize a string -       08/06/2018
TOKSTR   CSECT
         USING  TOKSTR,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    N,=A(1)            n=1
         LA     R7,1               i1=1
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,LENS)     do i=1 to length(s);
         LA     R4,S-1             @s-1
         AR     R4,R6              +i
         MVC    C,0(R4)            c=substr(s,i,1)
       IF CLI,C,EQ,C',' THEN       if c=',' then do
         BAL    R14,TOK              call tok
         LR     R2,R8                i2
         SR     R2,R7                i2-i1
         LA     R2,1(R2)             i2-i1+1
         L      R1,N                 n
         SLA    R1,1                 *2
         STH    R2,TALEN-2(R1)       talen(n)=i2-i1+1
         L      R2,N                 n
         LA     R2,1(R2)             n+1
         ST     R2,N                 n=n+1
         LA     R7,1(R6)             i1=i+1
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         BAL    R14,TOK            call tok
         LR     R2,R8              i2
         SR     R2,R7              i2-i1
         LA     R2,1(R2)           i2-i1+1
         L      R1,N               n
         SLA    R1,1               *2
         STH    R2,TALEN-2(R1)     talen(n)=i2-i1+1
         LA     R11,PG             pgi=@pg
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LR     R1,R6                i
         SLA    R1,1                 *2
         LH     R10,TALEN-2(R1)      l=talen(i)
         LR     R1,R6                i
         SLA    R1,3                 *8
         LA     R4,TABLE-8(R1)       @table(i)
         LR     R2,R10               l
         BCTR   R2,0                 ~
         EX     R2,MVCX              output table(i) length(l)
         AR     R11,R10              pgi=pgi+l
       IF C,R6,NE,N THEN             if i^=n then
         MVC    0(1,R11),=C'.'         output '.'
         LA     R11,1(R11)             pgi=pgi+1
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XPRNT  PG,L'PG            print
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
TOK      LR     R5,R6              i                              <--
         BCTR   R5,0               i-1                              |
         LR     R8,R5              i2=i-1
         SR     R5,R7              i2-i1
         LA     R5,1(R5)           l=i2-i1+1  source length
         L      R1,N               n
         SLA    R1,3               *8
         LA     R2,TABLE-8(R1)     @table(n)
         LA     R4,S-1             @s-1
         AR     R4,R7              @s+i1-1
         LA     R3,8               target length
         MVCL   R2,R4              table(n)=substr(s,i1,i2-i1+1)    |
         BR     R14                End TOK subroutine             <--
MVCX     MVC    0(0,R11),0(R4)     output table(i)
S        DC     CL80'Hello,How,Are,You,Today'  <== input string ==
LENS     DC     F'23'              length(s)   <==
TABLE    DC     8CL8' '            table(8)
TALEN    DC     8H'0'              talen(8)
C        DS     CL1                char
N        DS     F                  number of tokens
PG       DC     CL80' '            buffer
         YREGS
         END    TOKSTR
```

{{out}}

```txt

Hello.How.Are.You.Today

```



## ACL2


```lisp
(defun split-at (xs delim)
   (if (or (endp xs) (eql (first xs) delim))
       (mv nil (rest xs))
       (mv-let (before after)
               (split-at (rest xs) delim)
          (mv (cons (first xs) before) after))))

(defun split (xs delim)
   (if (endp xs)
       nil
       (mv-let (before after)
               (split-at xs delim)
          (cons before (split after delim)))))

(defun css->strs (css)
   (if (endp css)
       nil
       (cons (coerce (first css) 'string)
             (css->strs (rest css)))))

(defun split-str (str delim)
   (css->strs (split (coerce str 'list) delim)))

(defun print-with (strs delim)
   (if (endp strs)
       (cw "~%")
       (progn$ (cw (first strs))
               (cw (coerce (list delim) 'string))
               (print-with (rest strs) delim))))
```


{{out}}

```txt
&gt; (print-with (split-str "Hello,How,Are,You,Today" #\,) #\.)
Hello.How.Are.You.Today.
```



## ActionScript


```actionscript
var hello:String = "Hello,How,Are,You,Today";
var tokens:Array = hello.split(",");
trace(tokens.join("."));

// Or as a one-liner
trace("Hello,How,Are,You,Today".split(",").join("."));
```



## Ada


```ada

with Ada.Text_IO, Ada.Containers.Indefinite_Vectors;
use  Ada.Text_IO, Ada.Containers;

procedure tokenize is
  package String_Vector is new Indefinite_Vectors (Natural,String); use String_Vector;
  s       : String   := "Hello,How,Are,You,Today" & ",";
  current : Positive := s'First;
  v       : Vector;
begin
  for i in s'range loop
    if s (i) = ',' or i = s'last then
      v.append (s (current .. i-1));
      current := i + 1;
    end if;
   end loop;
  for s of v loop put(s & "."); end loop;
end tokenize;

```



## ALGOL 68


```algol68
main:(

  OP +:=  = (REF FLEX[]STRING in out, STRING item)VOID:(
    [LWB in out: UPB in out+1]STRING new;
    new[LWB in out: UPB in out]:=in out;
    new[UPB new]:=item;
    in out := new
  );

  PROC string split = (REF STRING beetles, STRING substr)[]STRING:(
    """ Split beetles where substr is found """;
    FLEX[1:0]STRING out;
    INT start := 1, pos;
    WHILE string in string(substr, pos, beetles[start:]) DO
      out +:= STRING(beetles[start:start+pos-2]);
      start +:= pos + UPB substr - 1
    OD;
    IF start > LWB beetles THEN
      out +:= STRING(beetles[start:])
    FI;
    out
  );

  PROC char split = (REF STRING beetles, STRING chars)[]STRING: (
    """ Split beetles where character is found in chars """;
    FLEX[1:0]STRING out;
    FILE beetlef;
    associate(beetlef, beetles); # associate a FILE handle with a STRING   #
    make term(beetlef, chars);   # make term: assign CSV string terminator #

    PROC raise logical file end = (REF FILE f)BOOL: except logical file end;
    on logical file end(beetlef, raise logical file end);

    STRING solo;
    DO
      getf(beetlef, ($g$, solo));
      out+:=solo;
      getf(beetlef, ($x$)) # skip CHAR separator #
    OD;
    except logical file end:
      SKIP;
    out
  );

  STRING beetles := "John Lennon, Paul McCartney, George Harrison, Ringo Starr";

  printf(($g"."$, string split(beetles, ", "),$l$));
  printf(($g"."$, char   split(beetles, ", "),$l$))
)
```

{{out}}

```txt

 John Lennon.Paul McCartney.George Harrison.Ringo Starr.
 John.Lennon..Paul.McCartney..George.Harrison..Ringo.Starr.

```




## AppleScript


```AppleScript
on run {}
  intercalate(".", splitOn(",", "Hello,How,Are,You,Today"))
end run


-- splitOn :: String -> String -> [String]
on splitOn(strDelim, strMain)
  set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
  set lstParts to text items of strMain
  set my text item delimiters to dlm
  return lstParts
end splitOn

-- intercalate :: String -> [String] -> String
on intercalate(strText, lstText)
  set {dlm, my text item delimiters} to {my text item delimiters, strText}
  set strJoined to lstText as text
  set my text item delimiters to dlm
  return strJoined
end intercalate
```


{{Out}}


```txt
"Hello.How.Are.You.Today"
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program strTokenize.s   */

/* Constantes    */
.equ STDOUT, 1                          @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ NBPOSTESECLAT,          20

/* Initialized data */
.data
szMessFinal:   .asciz "Words are : \n"

szString:            .asciz "Hello,How,Are,You,Today"
szMessError:         .asciz "Error tokenize !!\n"
szCarriageReturn:   .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:
    ldr r0,iAdrszString                           @ string address
    mov r1,#','                                   @ separator
    bl stTokenize
    cmp r0,#-1                                    @ error ?
    beq 99f
    mov r2,r0                                     @ table address
    ldr r0,iAdrszMessFinal                        @ display message
    bl affichageMess
    ldr r4,[r2]                                   @ number of areas
    add r2,#4                                     @ first area
    mov r3,#0                                     @ loop counter
1:                                                @ display loop
    ldr r0,[r2,r3, lsl #2]                        @ address area
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                   @ display carriage return
    bl affichageMess
    add r3,#1                                     @ counter + 1
    cmp r3,r4                                     @ end ?
    blt 1b                                        @ no -> loop

    b 100f
99:                                               @ display error message
    ldr r0,iAdrszMessError
    bl affichageMess

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc 0                                         @ perform the system call
iAdrszString:             .int szString
iAdrszFinalString:       .int szFinalString
iAdrszMessFinal:          .int szMessFinal
iAdrszMessError:          .int szMessError
iAdrszCarriageReturn:    .int szCarriageReturn
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
    svc #0                                      @ call systeme
    pop {r0,r1,r2,r7,lr}                        @ restaur des  2 registres
    bx lr                                       @ return
/*******************************************************************/
/* Separate string by separator into an array                     */
/* areas are store on the heap Linux                               */
/*******************************************************************/
/* r0 contains string address */
/* r1 contains separator character (, or . or : )    */
/* r0 returns table address with first item = number areas */
/* and other items contains pointer of each string     */
stTokenize:
    push {r1-r8,lr}                                 @ save des registres
    mov r6,r0
    mov r8,r1                                       @ save separator
    bl strLength                                    @ length string for place reservation on the heap
    mov r4,r0
    ldr r5,iTailleTable
    add r5,r0
    and r5,#0xFFFFFFFC
    add r5,#4                                       @ align word on the heap
                                                    @ place reservation on the heap
    mov r0,#0                                       @ heap address
    mov r7, #0x2D                                   @ call system linux 'brk'
    svc #0                                          @ call system
    cmp r0,#-1                                      @ error call system
    beq 100f
    mov r3,r0                                       @ save address  heap begin
    add r0,r5                                       @ reserve r5 byte on the heap
    mov r7, #0x2D                                   @ call system linux 'brk'
    svc #0
    cmp r0,#-1
    beq 100f
                                                    @ string copy on the heap
    mov r0,r6
    mov r1,r3
1:                                                  @ loop copy string
    ldrb r2,[r0],#1                                 @ read one byte and increment pointer one byte
    strb r2,[r1],#1                                 @ store one byte and increment pointer one byte
    cmp r2,#0                                       @ end of string ?
    bne 1b                                          @ no -> loop

    add r4,r3                                        @ r4 contains address table begin
    mov r0,#0
    str r0,[r4]
    str r3,[r4,#4]
    mov r2,#1                                       @ areas counter
2:                                                  @ loop load string character
    ldrb r0,[r3]
    cmp r0,#0
    beq 3f                                          @ end string
    cmp r0,r8                                       @ separator ?
    addne r3,#1                                     @ no -> next location
    bne 2b                                          @ and loop
    mov r0,#0                                       @ store zero final of string
    strb r0,[r3]
    add r3,#1                                       @ next character
    add r2,#1                                       @ areas counter + 1
    str r3,[r4,r2, lsl #2]                          @ store address area in the table at index r2
    b 2b                                            @ and loop

3:
    str r2,[r4]                                     @ returns number areas
    mov r0,r4
100:
    pop {r1-r8,lr}
    bx lr
iTailleTable: .int 4 * NBPOSTESECLAT
/***************************************************/
/*   calcul size string                            */
/***************************************************/
/* r0 string address                 */
/* r0 returns size string            */
strLength:
    push {r1,r2,lr}
    mov r1,#0                                           @ init counter
1:
   ldrb r2,[r0,r1]                                      @ load byte of string index r1
   cmp r2,#0                                            @ end string ?
   addne r1,#1                                          @ no -> +1 counter
   bne 1b                                               @ and loop

100:
    mov r0,r1
    pop {r1,r2,lr}
    bx lr


```



## Arturo


{{trans|D}}


```arturo
str "Hello,How,Are,You,Today"

print $(join $(split str ",") ".")
```


{{out}}


```txt
Hello.How.Are.You.Today
```



## Astro


```python
let text = 'Hello,How,Are,You,Today'
let tokens = text.split(||,||)
print tokens.join(with: '.')
```



## AutoHotkey


```AutoHotkey
string := "Hello,How,Are,You,Today"
stringsplit, string, string, `,
loop, % string0
{
msgbox % string%A_Index%
}
```



## AWK



```awk
BEGIN {
  s = "Hello,How,Are,You,Today"
  split(s, arr, ",")
  for(i=1; i < length(arr); i++) {
    printf arr[i] "."
  }
  print
}
```


A more ''idiomatic'' way for AWK is


```awk
BEGIN { FS = "," }
{
  for(i=1; i <= NF; i++) printf $i ".";
  print ""
}
```


which "tokenize" each line of input and this is achieved by using "," as field separator


## BASIC

=
## Applesoft BASIC
=

```ApplesoftBasic
100 T$ = "HELLO,HOW,ARE,YOU,TODAY"
110 GOSUB 200"TOKENIZE
120 FOR I = 1 TO N
130     PRINT A$(I) "." ;
140 NEXT
150 PRINT
160 END

200 IF N = 0 THEN DIM A$(256)
210 N = 1
220 A$(N) = "
230 FOR TI = 1 TO LEN(T$)
240     C$ = MID$(T$, TI, 1)
250     T = C$ = ","
260     IF T THEN C$ = "
270     N = N + T
280     IF T THEN A$(N) = C$
290     A$(N) = A$(N) + C$
300 NEXT TI
310 RETURN
```


=
## BaCon
=
BaCon includes extensive support for ''delimited strings''.

```freebasic
' Tokenize a string
OPTION BASE 1

READ csv$
DATA "Hello,How,Are,You,Today"

SPLIT csv$ BY "," TO elements$ SIZE n

FOR i = 1 TO n
    PRINT elements$[i] FORMAT "%s"
    IF i < n THEN PRINT "." FORMAT "%s"
NEXT
PRINT
```


{{out}}

```txt
prompt$ ./tokenize
Hello.How.Are.You.Today
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"STRINGLIB"

      text$ = "Hello,How,Are,You,Today"
      n% = FN_split(text$, ",", array$())
      FOR i% = 0 TO n%-1
        PRINT array$(i%) "." ;
      NEXT
      PRINT
```


=
## Commodore BASIC
=
Based on the AppleSoft BASIC version.

```commodorebasic

10 REM TOKENIZE A STRING ... ROSETTACODE.ORG
20 T$ = "HELLO,HOW,ARE,YOU,TODAY"
30 GOSUB 200, TOKENIZE
40 FOR I = 1 TO N
50     PRINT A$(I) "." ;
60 NEXT
70 PRINT
80 END
200 IF N = 0 THEN DIM A$(256)
210 N = 1
220 A$(N) = ""
230 FOR L = 1 TO LEN(T$)
240     C$ = MID$(T$, L, 1)
250     IF C$<>"," THEN A$(N) = A$(N) + C$: GOTO 270
260     N = N + 1
270 NEXT L
280 RETURN

```

=
## Liberty BASIC
=

```lb
'Note that Liberty Basic's array usage can reach element #10 before having to DIM the array
For i = 0 To 4
    array$(i) = Word$("Hello,How,Are,You,Today", (i + 1), ",")
    array$ = array$ + array$(i) + "."
Next i

Print Left$(array$, (Len(array$) - 1))
```


=
## PowerBASIC
=

PowerBASIC has a few keywords that make parsing strings trivial: <code>PARSE</code>, <code>PARSE$</code>, and <code>PARSECOUNT</code>. (<code>PARSE$</code>, not shown here, is for extracting tokens one at a time, while <code>PARSE</code> extracts all tokens at once into an array. <code>PARSECOUNT</code> returns the number of tokens found.)


```powerbasic
FUNCTION PBMAIN () AS LONG
    DIM parseMe AS STRING
    parseMe = "Hello,How,Are,You,Today"

    REDIM parsed(PARSECOUNT(parseMe) - 1) AS STRING
    PARSE parseMe, parsed()  'comma is default delimiter

    DIM L0 AS LONG, outP AS STRING
    outP = parsed(0)
    FOR L0 = 1 TO UBOUND(parsed)  'could reuse parsecount instead of ubound
        outP = outP & "." & parsed(L0)
    NEXT

    MSGBOX outP
END FUNCTION
```


=
## PureBasic
=

'''As described

```PureBasic
NewList MyStrings.s()

For i=1 To 5
  AddElement(MyStrings())
  MyStrings()=StringField("Hello,How,Are,You,Today",i,",")
Next i

ForEach MyStrings()
  Print(MyStrings()+".")
Next
```


'''Still, easier would be

```PureBasic
Print(ReplaceString("Hello,How,Are,You,Today",",","."))
```


=
## QBasic
=

```qbasic
DIM parseMe AS STRING
parseMe = "Hello,How,Are,You,Today"

DIM tmpLng1 AS INTEGER, tmpLng2 AS INTEGER, parsedCount AS INTEGER
tmpLng2 = 1
parsedCount = -1

'count number of tokens
DO
    tmpLng1 = INSTR(tmpLng2, parseMe, ",")
    IF tmpLng1 THEN
        parsedCount = parsedCount + 1
        tmpLng2 = tmpLng1 + 1
    ELSE
        IF tmpLng2 < (LEN(parseMe) + 1) THEN parsedCount = parsedCount + 1
        EXIT DO
    END IF
LOOP

IF parsedCount > -1 THEN
    REDIM parsed(parsedCount) AS STRING
    tmpLng2 = 1
    parsedCount = -1

    'parse
    DO
        tmpLng1 = INSTR(tmpLng2, parseMe, ",")
        IF tmpLng1 THEN
            parsedCount = parsedCount + 1
            parsed(parsedCount) = MID$(parseMe, tmpLng2, tmpLng1 - tmpLng2)
            tmpLng2 = tmpLng1 + 1
        ELSE
            IF tmpLng2 < (LEN(parseMe) + 1) THEN
                parsedCount = parsedCount + 1
                parsed(parsedCount) = MID$(parseMe, tmpLng2)
            END IF
            EXIT DO
        END IF
    LOOP

    PRINT parsed(0);
    FOR L0 = 1 TO parsedCount
        PRINT "."; parsed(L0);
    NEXT
END IF
```


=
## Run BASIC
=

```runbasic
text$ = "Hello,How,Are,You,Today"
FOR i = 1 to 5
 textArray$(i) = word$(text$,i,",")
 print textArray$(i);" ";
NEXT
```


=
## VBScript
=

### =One liner=


```vb
WScript.Echo Join(Split("Hello,How,Are,You,Today", ","), ".")
```


In fact, the Visual Basic solution (below) could have done the same, as Join() is available.

=
## Visual Basic
=
{{trans|PowerBASIC}}

Unlike PowerBASIC, there is no need to know beforehand how many tokens are in the string -- <code>Split</code> automagically builds the array for you.


```vb
Sub Main()
    Dim parseMe As String, parsed As Variant
    parseMe = "Hello,How,Are,You,Today"

    parsed = Split(parseMe, ",")

    Dim L0 As Long, outP As String
    outP = parsed(0)
    For L0 = 1 To UBound(parsed)
        outP = outP & "." & parsed(L0)
    Next

    MsgBox outP
End Sub
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
call :tokenize %1 res
echo %res%
goto :eof

:tokenize
set str=%~1
:loop
for %%i in (%str%) do set %2=!%2!.%%i
set %2=!%2:~1!
goto :eof
```


''Demo''
 >tokenize.cmd "Hello,How,Are,You,Today"
 Hello.How.Are.You.Today


## Bracmat

Solution that employs string pattern matching to spot the commas

```bracmat
( "Hello,How,Are,You,Today":?String
& :?ReverseList
&   whl
  ' ( @(!String:?element "," ?String)
    & !element !ReverseList:?ReverseList
    )
& !String:?List
&   whl
  ' ( !ReverseList:%?element ?ReverseList
    & (!element.!List):?List
    )
& out$!List
)
```

Solution that starts by evaluating the input and employs the circumstance that the comma is a list constructing binary operator and that the string does not contain any other characters that are interpreted as operators on evaluation.

```bracmat
(  get$("Hello,How,Are,You,Today",MEM):?CommaseparatedList
& :?ReverseList
&   whl
  ' ( !CommaseparatedList:(?element,?CommaseparatedList)
    & !element !ReverseList:?ReverseList
    )
& !CommaseparatedList:?List
&   whl
  ' ( !ReverseList:%?element ?ReverseList
    & (!element.!List):?List
    )
& out$!List
)
```



## C

{{works with|ANSI C}}

{{libheader|POSIX}}

This example uses the ''strtok()'' function to separate the tokens. This function is destructive (replacing token separators with '\0'), so we have to make a copy of the string (using ''strdup()'') before tokenizing. ''strdup()'' is not part of [[ANSI C]], but is available on most platforms. It can easily be implemented with a combination of ''strlen()'', ''malloc()'', and ''strcpy()''.


```c
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	char *a[5];
	const char *s="Hello,How,Are,You,Today";
	int n=0, nn;

	char *ds=strdup(s);

	a[n]=strtok(ds, ",");
	while(a[n] && n<4) a[++n]=strtok(NULL, ",");

	for(nn=0; nn<=n; ++nn) printf("%s.", a[nn]);
	putchar('\n');

	free(ds);

	return 0;
}
```


Another way to accomplish the task without the built-in string functions is to temporarily modify the separator character. This method does not need any additional memory, but requires the input string to be writeable.

```c
#include <stdio.h>

typedef void (*callbackfunc)(const char *);

void doprint(const char *s) {
	printf("%s.", s);
}

void tokenize(char *s, char delim, callbackfunc cb) {
	char *olds = s;
	char olddelim = delim;
	while(olddelim && *s) {
		while(*s && (delim != *s)) s++;
		*s ^= olddelim = *s; // olddelim = *s; *s = 0;
		cb(olds);
		*s++ ^= olddelim; // *s = olddelim; s++;
		olds = s;
	}
}

int main(void)
{
        char array[] = "Hello,How,Are,You,Today";
	tokenize(array, ',', doprint);
	return 0;
}
```


## C#

```c#
string str = "Hello,How,Are,You,Today";
// or Regex.Split ( "Hello,How,Are,You,Today", "," );
// (Regex is in System.Text.RegularExpressions namespace)
string[] strings = str.Split(',');
Console.WriteLine(String.Join(".", strings));

```



## C++


{{works with|C++98}}
std::getline() is typically used to tokenize strings on a single-character delimiter


```cpp
#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <iostream>
#include <algorithm>
int main()
{
    std::string s = "Hello,How,Are,You,Today";
    std::vector<std::string> v;
    std::istringstream buf(s);
    for(std::string token; getline(buf, token, ','); )
        v.push_back(token);
    copy(v.begin(), v.end(), std::ostream_iterator<std::string>(std::cout, "."));
    std::cout << '\n';
}
```


{{works with|C++98}}
C++ allows the user to redefine what is considered whitespace. If the delimiter is whitespace, tokenization becomes effortless.


```cpp
#include <string>
#include <locale>
#include <sstream>
#include <vector>
#include <iterator>
#include <iostream>
#include <algorithm>
struct comma_ws : std::ctype<char> {
    static const mask* make_table() {
    static std::vector<mask> v(classic_table(), classic_table() + table_size);
        v[','] |= space;  // comma will be classified as whitespace
        return &v[0];
    }
    comma_ws(std::size_t refs = 0) : ctype<char>(make_table(), false, refs) {}
};
int main()
{
    std::string s = "Hello,How,Are,You,Today";
    std::istringstream buf(s);
    buf.imbue(std::locale(buf.getloc(), new comma_ws));
    std::istream_iterator<std::string> beg(buf), end;
    std::vector<std::string> v(beg, end);
    copy(v.begin(), v.end(), std::ostream_iterator<std::string>(std::cout, "."));
    std::cout << '\n';
}
```


{{works with|C++98}}
{{libheader|boost}}
The boost library has multiple options for easy tokenization.


```cpp
#include <string>
#include <vector>
#include <iterator>
#include <algorithm>
#include <iostream>
#include <boost/tokenizer.hpp>
int main()
{
    std::string s = "Hello,How,Are,You,Today";
    boost::tokenizer<> tok(s);
    std::vector<std::string> v(tok.begin(), tok.end());
    copy(v.begin(), v.end(), std::ostream_iterator<std::string>(std::cout, "."))
    std::cout << '\n';
}
```



## Ceylon

{{works with|Ceylon 1.2}}

```ceylon
shared void tokenizeAString() {
	value input = "Hello,How,Are,You,Today";
	value tokens = input.split(','.equals);
	print(".".join(tokens));
}
```



## COBOL

This can be made to handle more complex cases; UNSTRING allows multiple delimiters, capture of which delimiter was used for each field, a POINTER for starting position (set on ending), along with match TALLYING.


```COBOL

      identification division.
       program-id. tokenize.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 period constant as ".".
       01 cmma   constant as ",".

       01 start-with.
          05 value "Hello,How,Are,You,Today".

       01 items.
          05 item pic x(6) occurs 5 times.

       procedure division.
       tokenize-main.
       unstring start-with delimited by cmma
           into item(1) item(2) item(3) item(4) item(5)

       display trim(item(1)) period trim(item(2)) period
               trim(item(3)) period trim(item(4)) period
               trim(item(5))

       goback.
       end program tokenize.

```


{{out}}

```txt

prompt$ cobc -xj tokenize.cob
Hello.How.Are.You.Today

```



## CoffeeScript



```coffeescript

arr = "Hello,How,Are,You,Today".split ","
console.log arr.join "."

```



## ColdFusion


###  Classic tag based CFML


```cfm

<cfoutput>
  <cfset wordListTag = "Hello,How,Are,You,Today">
  #Replace( wordListTag, ",", ".", "all" )#
</cfoutput>

```

{{Output}}

```txt

"Hello.How.Are.You.Today"

```



###  Script Based CFML


```cfm><cfscript

  wordList = "Hello,How,Are,You,Today";
  splitList = replace( wordList, ",", ".", "all" );
  writeOutput( splitList );
</cfscript>
```

{{Output}}

```txt

"Hello.How.Are.You.Today"

```



## Common Lisp


There are libraries out there that handle splitting (e.g., [http://www.cliki.net/SPLIT-SEQUENCE SPLIT-SEQUENCE], and the more-general [http://weitz.de/cl-ppcre/ CL-PPCRE]), but this is a simple one-off, too.  When the words are written with write-with-periods, there is no final period after the last word.


```lisp
(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun write-with-periods (strings)
  (format t "~{~A~^.~}" strings))
```



## Clojure

Using native Clojure functions and Java Interop:

```clojure
(apply str (interpose "." (.split #"," "Hello,How,Are,You,Today")))
```


Using the clojure.string library:

```clojure
(clojure.string/join "." (clojure.string/split "Hello,How,Are,You,Today" #","))
```



## D


```d
void main() {
    import std.stdio, std.string;

    "Hello,How,Are,You,Today".split(',').join('.').writeln;
}
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## Delphi



```Delphi

program TokenizeString;

{$APPTYPE CONSOLE}

uses
  Classes;

var
  tmp: TStringList;
  i: Integer;

begin

  // Instantiate TStringList class
  tmp := TStringList.Create;
  try
    { Use the TStringList's CommaText property to get/set
      all the strings in a single comma-delimited string }
    tmp.CommaText := 'Hello,How,Are,You,Today';

    { Now loop through the TStringList and display each
      token on the console }
    for i := 0 to Pred(tmp.Count) do
      Writeln(tmp[i]);

  finally
    tmp.Free;
  end;

  Readln;

end.

```


The result is:


```Delphi

Hello
How
Are
You
Today

```


=={{header|Déjà Vu}}==

```dejavu
!print join "." split "Hello,How,Are,You,Today" ","
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## Dyalect



```dyalect
var str = "Hello,How,Are,You,Today"
var strings = str.split(',')
print(values: strings, separator: ".")
```


{{out}}


```txt
Hello.How.Are.You.Today
```



## E


```e
".".rjoin("Hello,How,Are,You,Today".split(","))
```



## Elena

ELENA 4.x:

```elena
import system'routines;
import extensions;

public program()
{
    var string := "Hello,How,Are,You,Today";

    string.splitBy:",".forEach:(s)
    {
        console.print(s,".")
    }
}
```



## Elixir


```elixir

tokens = String.split("Hello,How,Are,You,Today", ",")
IO.puts Enum.join(tokens, ".")

```



## Erlang


```erlang
-module(tok).
-export([start/0]).

start() ->
   Lst = string:tokens("Hello,How,Are,You,Today",","),
   io:fwrite("~s~n", [string:join(Lst,".")]),
   ok.
```



## Euphoria


```euphoria
function split(sequence s, integer c)
    sequence out
    integer first, delim
    out = {}
    first = 1
    while first<=length(s) do
        delim = find_from(c,s,first)
        if delim = 0 then
            delim = length(s)+1
        end if
        out = append(out,s[first..delim-1])
        first = delim + 1
    end while
    return out
end function

sequence s
s = split("Hello,How,Are,You,Today", ',')

for i = 1 to length(s) do
    puts(1, s[i] & ',')
end for
```



=={{header|F_Sharp|F#}}==

```fsharp
System.String.Join(".", "Hello,How,Are,You,Today".Split(','))
```



## Factor


```factor
"Hello,How,Are,You,Today" "," split "." join print
```




## Falcon

'''VBA/Python programmer's approach to this solution, not sure if it's the most falconic way'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

a = []
a = strSplit("Hello,How,Are,You,Today", ",")
index = 0
start = 0
b = ""
for index in [ start : len(a)-1 : 1 ]
	b = b + a[index] + "."
end

> b

```

{{out}}

```txt

Hello.How.Are.You.
[Finished in 0.2s]

```



## Fantom


A string can be split on a given character, returning a list of the intervening strings.


```fantom

class Main
{
  public static Void main ()
  {
    str := "Hello,How,Are,You,Today"
    words := str.split(',')
    words.each |Str word|
    {
      echo ("${word}. ")
    }
  }
}

```



## Forth

There is no standard string split routine, but it is easily written. The results are saved temporarily to the dictionary.


```forth
: split ( str len separator len -- tokens count )
  here >r 2swap
  begin
    2dup 2,             \ save this token ( addr len )
    2over search        \ find next separator
  while
    dup negate  here 2 cells -  +!  \ adjust last token length
    2over nip /string               \ start next search past separator
  repeat
  2drop 2drop
  r>  here over -   ( tokens length )
  dup negate allot           \ reclaim dictionary
  2 cells / ;                \ turn byte length into token count

: .tokens ( tokens count -- )
  1 ?do dup 2@ type ." ." cell+ cell+ loop 2@ type ;

s" Hello,How,Are,You,Today" s" ," split .tokens  \ Hello.How.Are.You.Today
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM Example

  CHARACTER(23) :: str = "Hello,How,Are,You,Today"
  CHARACTER(5) :: word(5)
  INTEGER :: pos1 = 1, pos2, n = 0, i

  DO
    pos2 = INDEX(str(pos1:), ",")
    IF (pos2 == 0) THEN
       n = n + 1
       word(n) = str(pos1:)
       EXIT
    END IF
    n = n + 1
    word(n) = str(pos1:pos1+pos2-2)
    pos1 = pos2+pos1
 END DO

 DO i = 1, n
   WRITE(*,"(2A)", ADVANCE="NO") TRIM(word(i)), "."
 END DO

END PROGRAM Example
```



## Frink


```frink

println[join[".", split[",", "Hello,How,Are,You,Today"]]]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=218e240236cdf1419a405abfed906ed3 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String[] = Split("Hello,How,Are,You,Today")

Print sString.Join(".")

End
```

Output:

```txt

Hello.How.Are.You.Today

```



## GAP


```gap
SplitString("Hello,How,Are,You,Today", ",");
# [ "Hello", "How", "Are", "You", "Today" ]

JoinStringsWithSeparator(last, ".");
# "Hello.How.Are.You.Today"
```



## Genie


```genie
[indent=4]

init
    str:string = "Hello,How,Are,You,Today"
    words:array of string[] = str.split(",")
    joined:string = string.joinv(".", words)
    print joined
```


{{out}}

```txt
prompt$ valac tokenize.gs
prompt$ ./tokenize
Hello.How.Are.You.Today
```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Hello,How,Are,You,Today"
    fmt.Println(strings.Join(strings.Split(s, ","), "."))
}
```



## Groovy


```groovy
println 'Hello,How,Are,You,Today'.split(',').join('.')
```



## Haskell

'''Using Data.Text'''


```haskell
{-# OPTIONS_GHC -XOverloadedStrings #-}
import Data.Text (splitOn,intercalate)
import qualified Data.Text.IO as T (putStrLn)

main = T.putStrLn . intercalate "." $ splitOn "," "Hello,How,Are,You,Today"
```


Output: Hello.How.Are.You.Today

'''Alternate Solution'''

The necessary operations are unfortunately not in the standard library (yet), but simple to write:


```haskell
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

splitRegex :: Regex -> String -> [String]

joinWith :: [a] -> [[a]] -> [a]
joinWith d xs = concat $ List.intersperse d xs
-- "concat $ intersperse" can be replaced with "intercalate" from the Data.List in GHC 6.8 and later

putStrLn $ joinWith "." $ splitBy (== ',') $ "Hello,How,Are,You,Today"

-- using regular expression to split:
import Text.Regex
putStrLn $ joinWith "." $ splitRegex (mkRegex ",") $ "Hello,How,Are,You,Today"
```


Tokenizing can also be realized by using unfoldr and break:

```Haskell
*Main> mapM_ putStrLn $ takeWhile (not.null) $ unfoldr (Just . second(drop 1). break (==',')) "Hello,How,Are,You,Today"
Hello
How
Are
You
Today
```

* You need to import the modules Data.List and Control.Arrow

As special cases, splitting / joining by white space and by newlines are provided by the Prelude functions <code>words</code> / <code>unwords</code> and <code>lines</code> / <code>unlines</code>, respectively.


## HicEst


```hicest
CHARACTER string="Hello,How,Are,You,Today", list

nWords = INDEX(string, ',', 256) + 1
maxWordLength = LEN(string) - 2*nWords
ALLOCATE(list, nWords*maxWordLength)

DO i = 1, nWords
  EDIT(Text=string, SePaRators=',', item=i, WordEnd, CoPyto=CHAR(i, maxWordLength, list))
ENDDO

DO i = 1, nWords
  WRITE(APPend) TRIM(CHAR(i, maxWordLength, list)), '.'
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   A := []
   "Hello,How,Are,You,Today" ? while put(A, 1(tab(upto(',')|0),=","))
   every writes(!A,".")
   write()
end
```


{{out}}

```txt

 ->ss
 Hello.How.Are.You.
 ->

```



## Io


```io
"Hello,How,Are,You,Today" split(",") join(".") println
```



## J


```j
   s=: 'Hello,How,Are,You,Today'
   ] t=: <;._1 ',',s
+-----+---+---+---+-----+
|Hello|How|Are|You|Today|
+-----+---+---+---+-----+
   ; t,&.>'.'
Hello.How.Are.You.Today.

  '.' (I.','=s)}s  NB. two steps combined
Hello.How.Are.You.Today
```


Alternatively using the system library/script <tt>strings</tt>

```j
   require 'strings'
   ',' splitstring s
+-----+---+---+---+-----+
|Hello|How|Are|You|Today|
+-----+---+---+---+-----+

   '.' joinstring ',' splitstring s
Hello.How.Are.You.Today
```


<tt>splitstring</tt> and <tt>joinstring</tt> also work with longer "delimiters":

```j
   '"'([ ,~ ,) '","' joinstring ',' splitstring s
"Hello","How","Are","You","Today"
```


But, of course, this could be solved with simple string replacement:


```J
   rplc&',.' s
Hello.How.Are.You.Today
```


The task asks us to ''Separate the string "Hello,How,Are,You,Today" by commas into an array (or list) so that each element of it stores a different word.'' but for many purposes the original string is an adequate data structure.  Note also that given a string, a list of "word start" indices and "word length" integers can be logically equivalent to having an "array of words" -- and, depending on implementation details may be a superior or inferior choice to some other representation.  But, in current definition of this task, the concept of "word length" plays no useful role.

Note also that J provides several built-in concepts of parsing:  split on leading delimiter, split on trailing delimiter, split J language words.  Also, it's sometimes more efficient to append to a string than to prepend to it.  So a common practice for parsing on an embedded delimiter is to append a copy of the delimiter to the string and then use the appended result:


```J
   fn;._2 string,','
```


Here '''fn''' is applied to each ',' delimited substring and the results are assembled into an array.

Or, factoring out the names:

```J
   fn ((;._2)(@(,&','))) string
```



## Java

{{works with|Java|1.0+}}

There are multiple ways to tokenize a String in Java.

The first is by splitting the String into an array of Strings. The separator is actually a regular expression so you could do very powerful things with this, but make sure to escape any characters with special meaning in regex.

{{works with|Java|1.8+}}


```java5
String toTokenize = "Hello,How,Are,You,Today";
System.out.println(String.join(".", toTokenize.split(",")));
```


{{works with|Java|1.4+}}

```java5
String toTokenize = "Hello,How,Are,You,Today";

String words[] = toTokenize.split(",");//splits on one comma, multiple commas yield multiple splits
               //toTokenize.split(",+") if you want to ignore empty fields
for(int i=0; i<words.length; i++) {
    System.out.print(words[i] + ".");
}
```


The other way is to use StringTokenizer. It will skip any empty tokens. So if two commas are given in line, there will be an empty string in the array given by the split function, but no empty string with the StringTokenizer object. This method takes more code to use, but allows you to get tokens incrementally instead of all at once.

{{works with|Java|1.0+}}

```java5
String toTokenize = "Hello,How,Are,You,Today";

StringTokenizer tokenizer = new StringTokenizer(toTokenize, ",");
while(tokenizer.hasMoreTokens()) {
    System.out.print(tokenizer.nextToken() + ".");
}
```



## JavaScript

{{works with|Firefox|2.0}}


```javascript
alert( "Hello,How,Are,You,Today".split(",").join(".") );
```



## jq


```jq
split(",") | join(".")
```
Example:
```sh
$ jq -r 'split(",") | join(".")'
"Hello,How,Are,You,Today"
Hello.How.Are.You.Today
```



## Jsish

Being in the ECMAScript family, Jsi is blessed with many easy to use character, string and array manipulation routines.


```javascript
puts('Hello,How,Are,You,Today'.split(',').join('.'))
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## Julia


```Julia

s = "Hello,How,Are,You,Today"
a = split(s, ",")
t = join(a, ".")

println("The string \"", s, "\"")
println("Splits into ", a)
println("Reconstitutes to \"", t, "\"")

```


{{out}}

```txt

The string "Hello,How,Are,You,Today"
Splits into SubString{ASCIIString}["Hello","How","Are","You","Today"]
Reconstitutes to "Hello.How.Are.You.Today"

```



## K


```K
words: "," \: "Hello,How,Are,You,Today"
"." /: words
```


{{out}}

```txt

"Hello.How.Are.You.Today"

```



## Kotlin

{{works with|Kotlin|1.0b4}}

```scala
fun main(args: Array<String>) {
    val input = "Hello,How,Are,You,Today"
    println(input.split(',').joinToString("."))
}
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## LabVIEW

To tokenize the string, we use the Search/Split String function to split the string by its first comma. Add the beginning (up to, but not including the comma) to the end of the array, remove the first comma from the rest of the string, and pass it back through the shift register to the loop's next iteration. This is repeated until the string is empty. Printing is a simple matter of concatenation.<br/>
{{VI solution|LabVIEW_Tokenize_a_string.png}}


## LDPL


```ldpl

DATA:
explode/words is text vector
explode/index is number
explode/string is text
explode/length is number
explode/stringlength is number
explode/current-token is text
explode/char is text
explode/separator is text
i is number

PROCEDURE:
# Ask for a sentence
display "Enter a sentence: "
accept explode/string

# Declare explode Subprocedure
# Splits a text into a text vector by a certain delimiter
# Input parameters:
# - explode/string: the string to explode (destroyed)
# - explode/separator: the character used to separate the string (preserved)
# Output parameters:
# - explode/words: vector of splitted words
# - explode/length: length of explode/words
sub-procedure explode
    join explode/string and explode/separator in explode/string
    store length of explode/string in explode/stringlength
    store 0 in explode/index
    store 0 in explode/length
    store "" in explode/current-token
    while explode/index is less than explode/stringlength do
        get character at explode/index from explode/string in explode/char
        if explode/char is equal to explode/separator then
            store explode/current-token in explode/words:explode/length
            add explode/length and 1 in explode/length
            store "" in explode/current-token
        else
            join explode/current-token and explode/char in explode/current-token
        end if
        add explode/index and 1 in explode/index
    repeat
    subtract 1 from explode/length in explode/length
end sub-procedure

# Separate the entered string
store " " in explode/separator
call sub-procedure explode
while i is less than or equal to explode/length do
    display explode/words:i crlf
    add 1 and i in i
repeat

```



## LFE



```lisp

> (set split (string:tokens "Hello,How,Are,You,Today" ","))
("Hello" "How" "Are" "You" "Today")
> (string:join split ".")
"Hello.How.Are.You.Today"

```



## Lang5


```lang5
'Hello,How,Are,You,Today ', split '. join .
```



## Lingo


```lingo
input = "Hello,How,Are,You,Today"
_player.itemDelimiter = ","
output = ""
repeat with i = 1 to input.item.count
  put input.item[i]&"." after output
end repeat
delete the last char of output
put output
-- "Hello.How.Are.You.Today"
```



## Logo

{{works with|UCB Logo}}

```logo
to split :str :sep
  output parse map [ifelse ? = :sep ["| |] [?]] :str
end
```


This form is more robust, doing the right thing if there are embedded spaces.

```logo
to split :str :by [:acc []] [:w "||]
  if empty? :str [output lput :w :acc]
  ifelse equal? first :str :by ~
    [output (split butfirst :str :by lput :w :acc)] ~
    [output (split butfirst :str :by         :acc  lput first :str :w)]
end
```



```logo
? show split "Hello,How,Are,You,Today ",
[Hello How Are You Today]
```



## Logtalk

Using Logtalk built-in support for Definite Clause Grammars (DCGs) and representing the strings as atoms for readbility:

```logtalk

:- object(spliting).

    :- public(convert/2).
    :- mode(convert(+atom, -atom), one).

    convert(StringIn, StringOut) :-
        atom_chars(StringIn, CharactersIn),
        phrase(split(',', Tokens), CharactersIn),
        phrase(split('.', Tokens), CharactersOut),
        atom_chars(StringOut, CharactersOut).

    split(Separator, [t([Character| Characters])| Tokens]) -->
        [Character], {Character \== Separator}, split(Separator, [t(Characters)| Tokens]).
    split(Separator, [t([])| Tokens]) -->
        [Separator], split(Separator, Tokens).
    split(_, [t([])]) -->
        [].
    % the look-ahead in the next rule prevents adding a spurious separator at the end
    split(_, []), [Character] -->
        [Character].

:- end_object.

```

{{out}}

```txt

| ?- spliting::convert('Hello,How,Are,You,Today', Converted).
Converted = 'Hello.How.Are.You.Today'
yes

```



## Lua

Split function callously stolen from the lua-users wiki

```Lua
function string:split (sep)
    local sep, fields = sep or ":", {}
    local pattern = string.format("([^%s]+)", sep)
    self:gsub(pattern, function(c) fields[#fields+1] = c end)
    return fields
end

local str = "Hello,How,Are,You,Today"
print(table.concat(str:split(","), "."))
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
	Function Tokenize$(s){
		\\ letter$ pop a string from stack of values
		\\ shift 2 swap top two values on stack of values
		fold1=lambda m=1 ->{
			shift 2 :if m=1 then m=0:drop: push letter$ else push letter$+"."+letter$
		}
		=s#fold$(fold1)
	}
	Print Tokenize$(piece$("Hello,How,Are,You,Today",",")) ="Hello.How.Are.You.Today"   ' true
}
Checkit

```



## M4


```M4
define(`s',`Hello,How,Are,You,Today')
define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn($1[$2])')
define(`n',0)
define(`fill',
   `set(a,n,$1)`'define(`n',incr(n))`'ifelse(eval($#>1),1,`fill(shift($@))')')
fill(s)
define(`j',0)
define(`show',
   `ifelse(eval(j<n),1,`get(a,j).`'define(`j',incr(j))`'show')')
show
```


{{out}}

```txt

 Hello.How.Are.You.Today.

```



## Maple


```Maple
StringTools:-Join(StringTools:-Split("Hello,How,Are,You,Today", ","),".");
```

{{Out|Output}}

```txt
"Hello.How.Are.You.Today"
```




## Mathematica


```Mathematica
StringRiffle[StringSplit["Hello,How,Are,You,Today", ","], "."]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function tokenizeString(string,delimeter)

    tokens = {};

    while not(isempty(string))
        [tokens{end+1},string] = strtok(string,delimeter);
    end

    for i = (1:numel(tokens)-1)
        fprintf([tokens{i} '.'])
    end

    fprintf([tokens{end} '\n'])
end
```


{{out}}

```txt
>> tokenizeString('Hello,How,Are,You,Today',',')
Hello.How.Are.You.Today
```



## Maxima


```Maxima
l: split("Hello,How,Are,You,Today", ",")$
printf(true, "~{~a~^.~}~%", l)$
```



## MAXScript


```maxscript
output = ""
for word in (filterString "Hello,How,Are,You,Today" ",") do
(
    output += (word + ".")
)
format "%\n" output
```



## Mercury

<lang>
:- module string_tokenize.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
    Tokens = string.split_at_char((','), "Hello,How,Are,You,Today"),
    io.write_list(Tokens, ".", io.write_string, !IO),
    io.nl(!IO).
```



## min

{{works with|min|0.19.3}}

```min
"Hello,How,Are,You,Today" "," split "." join print
```



## MiniScript


```MiniScript
tokens = "Hello,How,Are,You,Today".split(",")
print tokens.join(".")
```



## MMIX


```mmix
sep	IS	','
EOS	IS	0
NL	IS	10

// main registers
p	IS	$255
tp	GREG
c	GREG
t	GREG

	LOC	Data_Segment
	GREG	@
Text	BYTE	"Hello,How,Are,You,Today",EOS
token	BYTE	0
eot	IS	@+255

	LOC	#100	% main () {
Main	LDA	p,Text		%
	LDA	tp,token	% initialize pointers
2H	LDBU	c,p		% DO  get char
	BZ	c,5F		%  break if char == EOS
	CMP	t,c,sep		%  if char != sep then
	PBNZ	t,3F		%     store char
	SET	t,NL		%  terminate token with NL,EOS
	STBU	t,tp
	SET	t,EOS
	INCL	tp,1
	STBU	t,tp
	JMP	4F		%  continue

3H	STBU	c,tp		%  store char
4H	INCL	tp,1		%  update pointers
	INCL	p,1
	JMP	2B		% LOOP

5H	SET	t,NL		% terminate last token and buffer
	STBU	t,tp
	SET	t,EOS
	INCL	tp,1
	STBU	t,tp
%  next part is not really necessary
%  program runs only once
%	INCL	tp,1		% terminate buffer
%	STBU	t,tp

	LDA	tp,token	% reset token pointer
				% REPEAT
2H	ADD	p,tp,0		%    start of token
	TRAP	0,Fputs,StdOut	%    output token
	ADD	tp,tp,p
	INCL	tp,1		%    step to next token
	LDBU	t,tp
	PBNZ	t,2B		% UNTIL EOB(uffer)
	TRAP	0,Halt,0
```

{{out}}

```txt

 ~/MIX/MMIX/Progs> mmix tokenizing
 Hello
 How
 Are
 You
 Today

```


=={{header|Modula-3}}==

```modula3
MODULE Tokenize EXPORTS Main;

IMPORT IO, TextConv;

TYPE Texts = REF ARRAY OF TEXT;

VAR tokens: Texts;
    string := "Hello,How,Are,You,Today";
    sep := SET OF CHAR {','};

BEGIN
  tokens := NEW(Texts, TextConv.ExplodedSize(string, sep));
  TextConv.Explode(string, tokens^, sep);
  FOR i := FIRST(tokens^) TO LAST(tokens^) DO
    IO.Put(tokens[i] & ".");
  END;
  IO.Put("\n");
END Tokenize.
```


## MUMPS


```MUMPS
TOKENS
 NEW I,J,INP
 SET INP="Hello,how,are,you,today"
 NEW I FOR I=1:1:$LENGTH(INP,",") SET INP(I)=$PIECE(INP,",",I)
 NEW J FOR J=1:1:I WRITE INP(J) WRITE:J'=I "."
 KILL I,J,INP  // Kill is optional. "New" variables automatically are killed on "Quit"
 QUIT
```


In use:
 USER>D TOKENS^ROSETTA
 Hello.how.are.you.today


## Nemerle


```Nemerle
using System;
using System.Console;
using Nemerle.Utility.NString;

module Tokenize
{
    Main() : void
    {
        def cswords = "Hello,How,Are,You,Today";
        WriteLine(Concat(".", $[s | s in cswords.Split(',')]));
        // Split() produces an array while Concat() consumes a list
        // a quick in place list comprehension takes care of that
    }
}
```



## NetRexx


```NetRexx
/*NetRexx program *****************************************************
* 20.08.2012 Walter Pachl derived from REXX Version 3
**********************************************************************/
  sss='Hello,How,Are,You,Today'
  Say 'input string='sss
  Say ''
  Say 'Words in the string:'
  ss =sss.translate(' ',',')
  Loop i=1 To ss.words()
    Say ss.word(i)'.'
    End
  Say 'End-of-list.'
```

Output as in REXX version


## NewLISP


```NewLISP
(print (join (parse "Hello,How,Are,You,Today" ",") "."))
```



## Nial


Example for <b>Q'Nial7</b>, using <code>set "nodecor</code> and <code>set "diagram</code> switches for better display of the array structure:

Define Array with input string:


```Nial
     s := 'Hello,How,Are,You,Today'
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|H|e|l|l|o|,|H|o|w|,|A|r|e|,|Y|o|u|,|T|o|d|a|y|
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```


Split string at the commas:


```Nial
     t := s eachall = `, cut s
+-----------+-------+-------+-------+-----------+
|+-+-+-+-+-+|+-+-+-+|+-+-+-+|+-+-+-+|+-+-+-+-+-+|
||H|e|l|l|o|||H|o|w|||A|r|e|||Y|o|u|||T|o|d|a|y||
|+-+-+-+-+-+|+-+-+-+|+-+-+-+|+-+-+-+|+-+-+-+-+-+|
+-----------+-------+-------+-------+-----------+
```


Join string with <code>.</code> and remove last <code>.</code>


```Nial
     u := front content (cart t `.)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|H|e|l|l|o|.|H|o|w|.|A|r|e|.|Y|o|u|.|T|o|d|a|y|
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```


Less cluttered display, using <code>set "sketch;set "nodecor</code> display switches.


```Nial
     s:='Hello,How,Are,You,Today'
Hello,How,Are,You,Today
     t:= s eachall = `, cut s
+-----+---+---+---+-----+
|Hello|How|Are|You|Today|
+-----+---+---+---+-----+
     u:=front content (cart t `.)
Hello.How.Are.You.Today
```





## Nim


```nim
import strutils

let text = "Hello,How,Are,You,Today"
let tokens = text.split(',')
echo tokens.join(" ")
```



## Objeck


```objeck

class Parse {
  function : Main(args : String[]) ~ Nil {
    tokens := "Hello,How,Are,You,Today"->Split(",");
    each(i : tokens) {
      tokens[i]->PrintLine();
    };
  }
}
```


=={{header|Objective-C}}==
{{works with|GNUstep}}

{{works with|Cocoa}}


```objc
NSString *text = @"Hello,How,Are,You,Today";
NSArray *tokens = [text componentsSeparatedByString:@","];
NSString *result = [tokens componentsJoinedByString:@"."];
NSLog(result);
```



## OCaml

To split on a single-character separator:

```ocaml
let words = String.split_on_char ',' "Hello,How,Are,You,Today" in
String.concat "." words

```


The function split_on_char has been introduced in OCaml 4.04.  In previous versions, it could be implemented by:


```ocaml
let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r
```



## Oforth



```Oforth
"Hello,How,Are,You,Today" wordsWith(',') println
```


{{out}}

```txt

[Hello, How, Are, You, Today]

```



## ooRexx


```ooRexx
text='Hello,How,Are,You,Today'
do while text \= ''
   parse var text word1 ',' text
   call charout 'STDOUT:',word1'.'
end
```

{{out}}

```txt
Hello.How.Are.You.Today.
```



## OpenEdge/Progress


```progress
FUNCTION tokenizeString RETURNS CHAR (
   i_c AS CHAR
):

   DEF VAR ii        AS INT.
   DEF VAR carray    AS CHAR EXTENT.
   DEF VAR cresult   AS CHAR.

   EXTENT( carray ) = NUM-ENTRIES( i_c ).

   DO ii = 1 TO NUM-ENTRIES( i_c ):
      carray[ ii ] = ENTRY( ii, i_c ).
   END.

   DO ii = 1 TO EXTENT( carray ).
      cresult = cresult + "." + carray[ ii ].
   END.
   RETURN SUBSTRING( cresult, 2 ).

END FUNCTION. /* tokenizeString */

MESSAGE
   tokenizeString( "Hello,How,Are,You,Today" )
VIEW-AS ALERT-BOX.
```

{{out}}

```txt

 ---------------------------
 Message
 ---------------------------
 Hello.How.Are.You.Today
 ---------------------------
 OK
 ---------------------------

```



## Oz


```oz
for T in {String.tokens "Hello,How,Are,You,Today" &,} do
   {System.printInfo T#"."}
end
```



## PARI/GP


### Version #1.

Simple version, like the most custom ones here (for this task). This version has 1 character delimiter,
which is not allowed in the beginning and at the end of string, in addition, double, triple, etc., delimiters
are not allowed too.

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Tokenize a string str according to 1 character delimiter d. Return a list of tokens.
\\ Using ssubstr() from http://rosettacode.org/wiki/Substring#PARI.2FGP
\\ tokenize() 3/5/16 aev
tokenize(str,d)={
my(str=Str(str,d),vt=Vecsmall(str),d1=sasc(d),Lr=List(),sn=#str,v1,p1=1);
for(i=p1,sn, v1=vt[i]; if(v1==d1, listput(Lr,ssubstr(str,p1,i-p1)); p1=i+1));
return(Lr);
}

{
\\ TEST
print(" *** Testing tokenize from Version #1:");
print("1.", tokenize("Hello,How,Are,You,Today",","));
\\ BOTH 2 & 3 are NOT OK!!
print("2.",tokenize("Hello,How,Are,You,Today,",","));
print("3.",tokenize(",Hello,,How,Are,You,Today",","));
}

```


{{Output}}

```txt

 *** Testing tokenize from Version #1:
1.List(["Hello", "How", "Are", "You", "Today"])
2.List(["Hello", "How", "Are", "You", "Today", ","])
3.List([",Hello,,How,Are,You,Today,", "Hello", ",How,Are,You,Today,", "How", "Ar
e", "You", "Today"])

```



### Version #2.

Advanced version. Delimiter is allowed in any place. In addition, multiple delimiters are allowed too.
This is really useful for considering omitted data.
This version can be used for positional parameters processing, or for processing data from tables with string rows.

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Tokenize a string str according to 1 character delimiter d. Return a list of tokens.
\\ Using ssubstr() from http://rosettacode.org/wiki/Substring#PARI.2FGP
\\ stok() 3/5/16 aev
stok(str,d)={
my(d1c=ssubstr(d,1,1),str=Str(str,d1c),vt=Vecsmall(str),d1=sasc(d1c),
   Lr=List(),sn=#str,v1,p1=1,vo=32);
if(sn==1, return(List(""))); if(vt[sn-1]==d1,sn--);
for(i=1,sn, v1=vt[i];
    if(v1!=d1, vo=v1; next);
    if(vo==d1||i==1, listput(Lr,""); p1=i+1; vo=v1; next);
    if(i-p1>0, listput(Lr,ssubstr(str,p1,i-p1)); p1=i+1);
    vo=v1;
   );
return(Lr);
}

{
\\ TEST
print(" *** Testing stok from Version #2:");
\\ pp - positional parameter(s)
print("1. 5 pp: ", stok("Hello,How,Are,You,Today",","));
print("2. 5 pp: ", stok("Hello,How,Are,You,Today,",","));
print("3. 9 pp: ", stok(",,Hello,,,How,Are,You,Today",","));
print("4. 6 pp: ", stok(",,,,,,",","));
print("5. 1 pp: ", stok(",",","));
print("6. 1 pp: ", stok("Hello-o-o??",","));
print("7. 0 pp: ", stok("",","));
}

```


{{Output}}

```txt

 *** Testing stok from Version #2:
1. 5 pp: List(["Hello", "How", "Are", "You", "Today"])
2. 5 pp: List(["Hello", "How", "Are", "You", "Today"])
3. 9 pp: List(["", "", "Hello", "", "", "How", "Are", "You", "Today"])
4. 6 pp: List(["", "", "", "", "", ""])
5. 1 pp: List([""])
6. 1 pp: List(["Hello-o-o??"])
7. 0 pp: List([""])

```



## Pascal

{{works with|Free_Pascal}}

```pascal
program TokenizeString;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;
const
  TestString = 'Hello,How,Are,You,Today';
var
  Tokens: TStringList;
  I: Integer;
begin
  // Uses FCL facilities, "harder" algorithm not implemented
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ',';
    Tokens.DelimitedText := TestString;
    Tokens.Delimiter := '.'; // For example
    // To standard Output
    WriteLn(Format('Tokenize from: "%s"', [TestString]));
    WriteLn(Format('to:            "%s"',[Tokens.DelimitedText]));
  finally
    Tokens.Free;
  end;
end.
```


The result is:
 Tokenize from: "Hello,How,Are,You,Today"
 to:            "Hello.How.Are.You.Today"


## Perl


```perl
print join('.', split /,/, 'Hello,How,Are,You,Today'), "\n";
```

CLI one-liner form:

```perl
echo "Hello,How,Are,You,Today" | perl -aplF/,/ -e '$" = "."; $_ = "@F";'
```

which is a compact way of telling Perl to do

```perl
BEGIN { $/ = "\n"; $\ = "\n"; }
LINE: while (defined($_ = <ARGV>)) {
    chomp $_;
    our(@F) = split(/,/, $_, 0);
    $" = '.';
    $_ = "@F";
}
continue {
    die "-p destination: $!\n" unless print $_;
}
```



## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}

```perl6
'Hello,How,Are,You,Today'.split(',').join('.').say;
```


Or with function calls:


```perl6
say join '.', split ',', 'Hello,How,Are,You,Today';
```



## Phix


```Phix
?join(split("Hello,How,Are,You,Today",","),".")
```

{{Out}}

```txt

"Hello.How.Are.You.Today"

```



## PHP

{{works with|PHP|5.x}}


```php
<?php
$str = 'Hello,How,Are,You,Today';
echo implode('.', explode(',', $str));
?>
```



## PicoLisp


```PicoLisp
(mapcar pack
   (split (chop "Hello,How,Are,You,Today") ",") )
```



## Pike


```pike
("Hello,How,Are,You,Today" / ",") * ".";
```



## PL/I


```pli
tok: Proc Options(main);
declare s character (100) initial ('Hello,How,Are,You,Today');
declare n fixed binary (31);

n = tally(s, ',')+1;

begin;
   declare table(n) character (50) varying;
   declare c character (1);
   declare (i, k) fixed binary (31);

   table = ''; k = 1;
   do i = 1 to length(s);
      c = substr(s, i, 1);
      if c = ',' then k = k + 1;
      else table(k) = table(k) || c;
   end;

   /* display the table */
   table = table || '.';
   put skip list (string(table));
end;
end;
```

{{out}}

```txt
Hello.How.Are.You.Today
```



## Pop11

The natural solution in Pop11 uses lists.

There are built in libraries for tokenising strings, illustrated below, along with code that the user could create for the task.

First show the use of sysparse_string to break up a string and make a list of strings.


```pop11
;;; Make a list of strings from a string using space as separator
lvars list;
sysparse_string('the cat sat on the mat') -> list;
;;; print the list of strings
list =>
** [the cat sat on the mat]
```


By giving it an extra parameter 'true' we can make it recognize numbers and produce a list of strings and numbers


```pop11
lvars list;
sysparse_string('one 1 two 2 three 3 four 4', true) -> list;
;;; print the list of strings and numbers
list =>
** [one 1 two 2 three 3 four 4]
;;; check that first item is a string and second an integer
isstring(list(1))=>
** <true>
isinteger(list(2))=>
** <true>
```


Now show some uses of the built in procedure sys_parse_string, which allows more options:


```pop11
;;; Make pop-11 print strings with quotes
true -> pop_pr_quotes;
;;;
;;; Create a string of tokens using comma as token separator
lvars str='Hello,How,Are,You,Today';
;;;
;;; Make a list of strings by applying sys_parse_string
;;; to str, using the character `,` as separator (the default
;;; separator, if none is provided, is the space character).
lvars strings;
[% sys_parse_string(str, `,`) %] -> strings;
;;;
;;; print the list of strings
strings =>
** ['Hello' 'How' 'Are' 'You' 'Today']
```


If {% ... %} were used instead of [% ... %] the result would be
a vector (i.e. array) of strings rather than a list of strings.


```pop11
{% sys_parse_string(str, `,`) %} -> strings;
;;; print the vector
strings =>
** {'Hello' 'How' 'Are' 'You' 'Today'}
```


It is also possible to give sys_parse_string a 'conversion' procedure, which is applied to each of the tokens.
E.g. it could be used to produce a vector of numbers, using the conversion procedure 'strnumber', which converts a string to a number:


```pop11
lvars numbers;
{% sys_parse_string('100 101 102 103 99.9 99.999', strnumber) %} -> numbers;
;;; the result is a vector containing integers and floats,
;;; which can be printed thus:
numbers =>
** {100 101 102 103 99.9 99.999}
```


Using lower level pop-11 facilities to tokenise the string:


```pop11
;;; Declare and initialize variables
lvars str='Hello,How,Are,You,Today';
;;; Iterate over string
lvars ls = [], i, j = 1;
for i from 1 to length(str) do
    ;;; If comma
    if str(i) = `,` then
       ;;; Prepend word (substring) to list
       cons(substring(j, i - j, str), ls) -> ls;
       i + 1 -> j;
    endif;
endfor;
;;; Prepend final word (if needed)
if j <= length(str) then
    cons(substring(j, length(str) - j + 1, str), ls) -> ls;
endif;
;;; Reverse the list
rev(ls) -> ls;
```


Since the task requires to use array we convert list to array


```pop11
;;; Put list elements and lenght on the stack
destlist(ls);
;;; Build a vector from them
lvars ar = consvector();
;;; Display in a loop, putting trailing period
for i from 1 to length(ar) do
   printf(ar(i), '%s.');
endfor;
printf('\n');
```


We could use list directly for printing:


```pop11
for i in ls do
    printf(i, '%s.');
endfor;
```


so the conversion to vector is purely to satisfy task formulation.


## PowerShell

{{works with|PowerShell|1}}

```powershell
$words = "Hello,How,Are,You,Today".Split(',')
[string]::Join('.', $words)
```


{{works with|PowerShell|2}}

```powershell
$words = "Hello,How,Are,You,Today" -split ','
$words -join '.'
```


{{works with|PowerShell|2}}
The StringSplitOptions enumeration weeds out the return of empty elements.

```PowerShell

"Hello,How,Are,You,Today", ",,Hello,,Goodbye,," | ForEach-Object {($_.Split(',',[StringSplitOptions]::RemoveEmptyEntries)) -join "."}

```

{{Out}}

```txt

Hello.How.Are.You.Today
Hello.Goodbye

```



## Prolog

{{works with|SWI Prolog}}

```prolog
splitup(Sep,[token(B)|BL]) --> splitup(Sep,B,BL).
splitup(Sep,[A|AL],B)      --> [A], {\+ [A] = Sep }, splitup(Sep,AL,B).
splitup(Sep,[],[B|BL])     --> Sep, splitup(Sep,B,BL).
splitup(_Sep,[],[])        --> [].
start :-
    phrase(splitup(",",Tokens),"Hello,How,Are,You,Today"),
    phrase(splitup(".",Tokens),Backtogether),
    string_to_list(ABack,Backtogether),
    writeln(ABack).
```

{{out}}

```txt

 ?- start.
 Hello.How.Are.You.Today

```


{{works with|SWI Prolog 7}}

Using the SWI Prolog string data type and accompanying predicates,
this can be accomplished in a few lines in the top level:


```prolog

?- split_string("Hello,How,Are,You,Today", ",", "", Split),
|    atomics_to_string(Split, ".", PeriodSeparated),
|    writeln(PeriodSeparated).
Hello.How.Are.You.Today

```



## Python

{{works with|Python|2.5}}{{works with|Python|3.0}}


```python
text = "Hello,How,Are,You,Today"
tokens = text.split(',')
print ('.'.join(tokens))
```


Or if interpretation of the task description means you don't need to keep an intermediate array:

```python
print ('.'.join('Hello,How,Are,You,Today'.split(',')))
```



## Q


```Q
words: "," vs "Hello,How,Are,You,Today"
"." sv words
```


{{out}}

```txt
"Hello.How.Are.You.Today"
```



## R


```R
text <- "Hello,How,Are,You,Today"
junk <- strsplit(text, split=",")
print(paste(unlist(junk), collapse="."))
```


or the one liner


```R
paste(unlist(strsplit(text, split=",")), collapse=".")
```



## Racket



```racket

#lang racket
(string-join (string-split "Hello,How,Are,You,Today" ",") ".")
;; -> "Hello.How.Are.You.Today"

```



## Raven


```raven
'Hello,How,Are,You,Today' ',' split '.' join print
```



## REBOL


```REBOL
print ["Original:"  original: "Hello,How,Are,You,Today"]
tokens: parse original ","
dotted: ""  repeat i tokens [append dotted rejoin [i "."]]
print ["Dotted:  "  dotted]
```


{{out}}

```txt

 Original: Hello,How,Are,You,Today
 Dotted:   Hello.How.Are.You.Today.

```



## Red


```Red
str: "Hello,How,Are,You,Today"
>> tokens: split str ","
>> probe tokens
["Hello" "How" "Are" "You" "Today"]

>> periods: replace/all form tokens " " "."        ;The word FORM converts the list series to a string removing quotes.
>> print periods                                            ;then REPLACE/ALL spaces with period
Hello.How.Are.You.Today
```



## Retro


```Retro
{{
  : char     (  -$  )   " " ;
  : tokenize ( $-$$ )
    @char ^strings'splitAtChar withLength 1- over + 0 swap ! tempString ;
  : action   ( $-   )
    keepString ^buffer'add ;
---reveal---
  : split    ( $cb- )
    ^buffer'set !char
    char ^strings'append
    [ tokenize action dup 1 <> ] while drop
    ^buffer'get drop ;
}}
```


This will suffice to split a string into an array of substrings. It is used like this:


```Retro
create strings 100 allot
"Hello,How,Are,You,Today" ', strings split
```


Since the buffer' vocabulary creates a zero-terminated buffer, we can display it using the each@ combinator and a simple quote:


```Retro
strings [ @ "%s." puts ] ^types'STRING each@
```



## REXX


### version 1

This REXX version doesn't append a period to the last word in the list.

```rexx
/*REXX program separates a string of comma-delimited words, and echoes. */
sss = 'Hello,How,Are,You,Today'        /*words seperated by commas (,). */
say 'input string =' sss               /*display the original string.   */
new=sss                                /*make a copy of the string.     */
                                       /* [↓]  string NEW is destroyed. */
  do items=1  until  new==''           /*keep going until  NEW is empty.*/
  parse  var  new a.items  ','  new    /*parse words delinated by comma.*/
  end   /*items*/                      /* [↑]  the array is named   A.  */

say;   say 'Words in the string:'      /*display a header for the list. */

     do j=1  for items                 /*now, display all the words.    */
     say a.j || left('.', j\==items)   /*append period to word,  maybe. */
     end   /*j*/                       /* [↑]  don't append "." if last.*/

say 'End-of-list.'                     /*display a trailer for the list.*/
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

input string = Hello,How,Are,You,Today

Words in the string:
Hello.
How.
Are.
You.
Today
End-of-list.

```



### version 2


```rexx
/*REXX program to separate a string of comma-delimited words and echo */
sss='Hello,How,Are,You,Today'
say 'input string='sss
say ''
say 'Words in the string:'
ss =translate(sss,' ',',')
dot='.'
Do i=1 To words(ss)
  If i=words(ss) Then dot=''
  say word(ss,i)dot
  End
say 'End-of-list.'
```

'''output''' is identical to REXX version 1.


## Ring


```ring

see substr("Hello,How,Are,You,Today", ",", ".")

```



## Ruby


```ruby
puts "Hello,How,Are,You,Today".split(',').join('.')
```



## Rust


```rust
fn main() {
    let s = "Hello,How,Are,You,Today";
    let tokens: Vec<&str> = s.split(",").collect();
    println!("{}", tokens.join("."));
}
```



=={{header|S-lang}}==
<lang S-lang>variable a = strchop("Hello,How,Are,You,Today", ',', 0);
print(strjoin(a, "."));
```


{{out}}

```txt
"Hello.How.Are.You.Today"
```



## Scala


```scala
println("Hello,How,Are,You,Today" split "," mkString ".")
```



## Scheme

{{works with|Guile}}

```scheme
(use-modules (ice-9 regex))
(define s "Hello,How,Are,You,Today")
(define words (map match:substring (list-matches "[^,]+" s)))

(do ((n 0 (+ n 1))) ((= n (length words)))
        (display (list-ref words n))
        (if (< n (- (length words) 1))
                (display ".")))
```


(with SRFI 13)

```scheme
(define s "Hello,How,Are,You,Today")
(define words (string-tokenize s (char-set-complement (char-set #\,))))
(define t (string-join words "."))
```


{{works with|Gauche Scheme}}

```Scheme
(print
  (string-join
    (string-split "Hello,How,Are,You,Today" #\,)
    "."))
```

{{output}}

```txt

Hello.How.Are.You.Today

```



## Seed7


```seed7
var array string: tokens is 0 times "";

tokens := split("Hello,How,Are,You,Today", ",");
```



## Self


```self
| s = 'Hello,How,Are,You,Today' |
((s splitOn: ',') joinUsing: '.') printLine.

```



## Sidef


```ruby
'Hello,How,Are,You,Today'.split(',').join('.').say;
```



## Slate


```slate
('Hello,How,Are,You,Today' splitWith: $,) join &separator: '.'.
```



## Smalltalk


```smalltalk
|array |
array := 'Hello,How,Are,You,Today' subStrings: $,.
array fold: [:concatenation :string | concatenation, '.', string ]
```


Some implementations also have a ''join:'' convenience method that allows the following shorter solution:


```smalltalk
('Hello,How,Are,You,Today' subStrings: $,) join: '.'
```


The solution displaying a trailing period would be:


```smalltalk
|array |
array := 'Hello,How,Are,You,Today' subStrings: $,.
array inject: '' into: [:concatenation :string | concatenation, string, '.' ]
```



## SNOBOL4


For this task, it's convenient to define Perl-style split( ) and join( ) functions.


```SNOBOL4
        define('split(chs,str)i,j,t,w2') :(split_end)
split   t = table()
sp1     str pos(0) (break(chs) | rem) $ t<i = i + 1>
+           span(chs) (break(chs) | '') . w2  = w2 :s(sp1)
*       t<i> = differ(str,'') str ;* Uncomment for CSnobol
        split = array(i)
sp2     split<j = j + 1> = t<j> :s(sp2)f(return)
split_end

        define('join(ch,a)i,') :(join_end)
join    join = join a<i = i + 1>
        join = join ?a<i + 1> ch :s(join)f(return)
join_end

*       # Test and display
        output = join('.',split(',','Hello,How,Are,You,Today'))
end
```


{{out}}

```txt

 Hello.How.Are.You.Today

```



## Standard ML


```sml
val splitter = String.tokens (fn c => c = #",");
val main = (String.concatWith ".") o splitter;
```


Test:


```sml
- main "Hello,How,Are,You,Today"
val it = "Hello.How.Are.You.Today" : string
```



## Swift


{{works with|Swift|3.x}}

```swift
let text = "Hello,How,Are,You,Today"
let tokens = text.components(separatedBy: ",") // for single or multi-character separator
print(tokens)
let result = tokens.joined(separator: ".")
print(result)
```


{{works with|Swift|2.x}}

```swift
let text = "Hello,How,Are,You,Today"
let tokens = text.characters.split(",").map{String($0)} // for single-character separator
print(tokens)
let result = tokens.joinWithSeparator(".")
print(result)
```


{{works with|Swift|1.x}}

```swift
let text = "Hello,How,Are,You,Today"
let tokens = split(text, { $0 == "," }) // for single-character separator
println(tokens)
let result = ".".join(tokens)
println(result)
```


For multi-character separators:
```swift
import Foundation

let text = "Hello,How,Are,You,Today"
let tokens = text.componentsSeparatedByString(",")
print(tokens)
```



## Tcl

Generating a list form a string by splitting on a comma:

```tcl
split $string ","
```


Joining the elements of a list by a period:

```tcl
join $list "."
```


Thus the whole thing would look like this:

```tcl
puts [join [split "Hello,How,Are,You,Today" ","] "."]
```


If you'd like to retain the list in a variable with the name "words", it would only be marginally more complex:

```tcl
puts [join [set words [split "Hello,How,Are,You,Today" ","]] "."]
```


(In general, the <tt>regexp</tt> command is also used in Tcl for tokenization of strings, but this example does not need that level of complexity.)


## tr

<code>tr</code> knows nothing about arrays, so this solution only changes each comma to a period.


```bash
echo 'Hello,How,Are,You,Today' | tr ',' '.'
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SET string="Hello,How,Are,You,Today"
SET string=SPLIT (string,":,:")
SET string=JOIN  (string,".")

```



## TXR


Collecting tokens which consist of non-empty
sequences of non-commas.


```txr
@(next :list "Hello,How,Are,You,Today")
@(coll)@{token /[^,]+/}@(end)
@(output)
@(rep)@token.@(last)@token@(end)
@(end)
```


Different approach. Collect tokens, each of
which is a piece of text which either terminates
before a comma, or else extends to the end of the line.


```txr
@(next :list "Hello,How,Are,You,Today")
@(coll)@(maybe)@token,@(or)@token@(end)@(end)
@(output)
@(rep)@token.@(last)@token@(end)
@(end)
```


Using TXR Lisp:


```bash
txr -p '(cat-str (split-str "Hello,How,Are,You,Today" ",") ".")'
Hello.How.Are.You.Today
```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
string='Hello,How,Are,You,Today'

(IFS=,
 printf '%s.' $string
 echo)
```


----
{{works with|Bourne Again SHell}}
{{works with|Public Domain Korn SHell|5.2.14}}

```bash
#! /bin/bash
stripchar-l ()
#removes the specified character from the left side of the string
#USAGE: stripchar "stuff" "s" --> tuff
{
    string="$1";
    string=${string#"$2"};

  echo "$string"
}

join ()
#join a string of characters on a specified delimiter
#USAGE: join "1;2;3;4" ";" "," --> 1,2,3,4
{
    local result="";
    local list="$1";
    OLDIFS="$IFS";
    local IFS=${2-" "};
    local output_field_seperator=${3-" "};

    for element in $list;
    do
        result="$result$output_field_seperator$element";
    done;

    result="`stripchar-l "$result" "$output_field_seperator"`";
    echo "$result";
    IFS="$OLDIFS"
}

split ()
{
#split a string of characters on a specified delimiter
#USAGE: split "1;2;3;4" ";" --> 1 2 3 4
    local list="$1";
    local input_field_seperator=${2-" "};
    local output_field_seperator=" ";

  #defined in terms of join
  join "$list" "$input_field_seperator" "$output_field_seperator"
}

strtokenize ()
{
#splits up a string of characters into tokens,
#based on a user supplied delimiter
#USAGE:strtokenize "1;2;3;4" ";" ":" --> 1:2:3:4
    local list="$1";
	local input_delimiter=${2-" "};
	local output_delimiter=${3-" "};
	local contains_a_space=" "; #added to highlight the use
                                    #of " " as an argument to join

  #splits it input then joins it with a user supplied delimiter
  join "$( split "$list" "$input_delimiter" )" \
    "$contains_a_space" "$output_delimiter";
}
```


''Example''


```bash
 strtokenize "Hello,How,Are,You,Today" "," "."
            Hello.How.Are.You.Today
```


----
{{works with|Almquist Shell}}
{{works with|bash}}
{{works with|pdksh}}
{{works with|ksh93}}
{{works with|zsh}}

```sh

string1="Hello,How,Are,You,Today"
elements_quantity=$(echo $string1|tr "," "\n"|wc -l)

present_element=1
while [ $present_element -le $elements_quantity ];do
echo $string1|cut -d "," -f $present_element|tr -d "\n"
if [ $present_element -lt $elements_quantity ];then echo -n ".";fi
present_element=$(expr $present_element + 1)
done
echo

# or to cheat
echo "Hello,How,Are,You,Today"|tr "," "."
```



## UnixPipes

{{works with|Bourne Shell}}

```bash
token() {
   (IFS=, read -r A B; echo "$A".; test -n "$B" && (echo "$B" | token))
}

echo "Hello,How,Are,You" | token
```



## Ursa


```ursa
decl string text
set text "Hello,How,Are,You,Today"
decl string<> tokens
set tokens (split text ",")
for (decl int i) (< i (size tokens)) (inc i)
        out tokens<i> "." console
end for
out endl console
```



## Ursala

A list of strings is made by separating at the commas using the library
function, sep.  A single string is then made by joining the list of strings
with periods using the library function, mat. Each of these is a
second order function parameterized by the delimiter. Character
literals are preceded by a backquote.

```Ursala
#import std

token_list = sep`, 'Hello,How,Are,You,Today'

#cast %s

main = mat`. token_list
```

{{out}}

```txt

 'Hello.How.Are.You.Today'

```



## Vala


```vala
// declare test string
string s = "Hello,How,Are,You,Today";
// create array of strings, could use var words instead if desired
string[] words = s.split(",");
// create string by joining array of strings with .
string joined = string.joinv(".", words);
```



## VBA


```vb
Sub Main()
Dim temp() As String
   temp = Tokenize("Hello,How,Are,You,Today", ",")
   Display temp, Space(5)
End Sub

Private Function Tokenize(strS As String, sep As String) As String()
   Tokenize = Split(strS, sep)
End Function

Private Sub Display(arr() As String, sep As String)
   Debug.Print Join(arr, sep)
End Sub
```

{{Out}}

```txt
Hello     How     Are     You     Today
```



## VBScript


```vb

s = "Hello,How,Are,You,Today"
WScript.StdOut.Write Join(Split(s,","),".")

```

{{Out}}

```txt
Hello.How.Are.You.Today
```



## Vedit macro language

Vedit does not use the concepts of array or list. Normally, the text is processed as text in an edit buffer.

However, this example shows how to split the text into multiple text registers (10, 11, 12 etc.).
The contents of each text register is then displayed to user, separated by a period.


```vedit
Buf_Switch(Buf_Free)
Ins_Text("Hello,How,Are,You,Today")

// Split the text into text registers 10, 11, ...
BOF
#1 = 9
Repeat(ALL) {
    #1++
    #2 = Cur_Pos
    Search(",", ADVANCE+ERRBREAK)
    Reg_Copy_Block(#1, #2, Cur_Pos-1)
}
Reg_Copy_Block(#1, #2, EOB_Pos)

// Display the list
for (#3 = 10; #3 <= #1; #3++) {
    Reg_Type(#3) Message(".")
}

Buf_Quit(OK)
```



## WinBatch



```WinBatch
text  = 'Hello,How,Are,You,Today'
result = ''
BoxOpen('WinBatch Tokenizing Example', '')
for ix = 1 to itemcount(text,',')
    result = result : itemextract(ix, text, ',') : '.'
    BoxText(result)
next
display(10, 'End of Program', 'Dialog and program will close momentarily.')
BoxShut()
```


{{out}}
 Hello.How.Are.You.Today.


## Wortel


```wortel
@join "." @split "," "Hello,How,Are,You,Today"
```

Returns

```txt
"Hello.How.Are.You.Today"
```



## XPath 2.0


```XPath
string-join(tokenize("Hello,How,Are,You,Today", ","), ".")
```


{{out}}
 Hello.How.Are.You.Today


## XPL0


```XPL0
string 0;
include c:\cxpl\codes;
int  I, J, K, Char;
char String, Array(5,6);        \5 words and 5 maximum chars + terminating 0

[String:= "Hello,How,Are,You,Today";
I:= 0;  K:= 0;
repeat  J:= 0;
        loop    [Char:= String(I);
                I:= I+1;
                if Char=^, or Char=0 then quit;
                Array(K,J):= Char;
                J:= J+1;
                ];
        Array(K,J):= 0;         \terminate word
        K:= K+1;                \next word in array
until   K>=5;
for K:= 4 downto 0 do [Text(0, addr Array(K,0));  ChOut(0, ^.)];
CrLf(0);
]
```


The 'addr' operator is used to fetch the 32-bit address of Array rather
than a byte from the character array.

Output (done in reverse order to emphasize the tokens are indeed separate):

```txt

Today.You.Are.How.Hello.

```



## Yabasic


```Yabasic
dim s$(1)

n = token("Hello. How are you today?", s$(), ".? ")

for i = 1 to n
	print s$(i);
	if i < n print ".";
next
print
```



## zkl


```zkl
"Hello,How,Are,You,Today".split(",").concat(".").println();
Hello.How.Are.You.Today
```



## Zsh


```zsh
str='Hello,How,Are,You,Today'
tokens=(${(s:,:)str})
print ${(j:.:)tokens}
```


Or, using SH_SPLIT_WORD:


```zsh
str='Hello,How,Are,You,Today'
IFS=, echo ${(j:.:)${=str}}
```


{{omit from|PARI/GP|No real capacity for string manipulation}}

[[Category: String manipulation]]
