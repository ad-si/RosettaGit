+++
title = "100 doors"
description = ""
date = 2019-10-11T20:27:38Z
aliases = []
[extra]
id = 2151
[taxonomies]
categories = []
tags = []
languages = [
  "11l",
  "360_assembly",
  "4dos_batch",
  "68000_assembly",
  "8086_assembly",
  "8th",
  "abap",
  "acl2",
  "actionscript",
  "acurity_architect",
  "ada",
  "agena",
  "aikido",
  "algol_68",
  "algol_w",
  "amigae",
  "apl",
  "applescript",
  "applesoft_basic",
  "arbre",
  "argile",
  "arm_assembly",
  "astro",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "axiom",
  "b",
  "bacon",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "blitzmax",
  "bloop",
  "bracmat",
  "burlesque",
  "c",
  "c1r",
  "ceylon",
  "clarion",
  "clio",
  "clips",
  "clojure",
  "cobol",
  "coco",
  "coffeescript",
  "coldfusion",
  "commodore_basic",
  "commodore_basic",
  "common_lisp",
  "component_pascal",
  "coq",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dafny",
  "dart",
  "qbasic",
  "sinclair_zx81_basic",
]
+++

{{task}}

There are 100 doors in a row that are all initially closed.

You make 100 [[task feature::Rosetta Code:multiple passes|passes]] by the doors.

The first time through, visit every door and  ''toggle''  the door  (if the door is closed,  open it;   if it is open,  close it).

The second time, only visit every 2<sup>nd</sup> door   (door #2, #4, #6, ...),   and toggle it.

The third time, visit every 3<sup>rd</sup> door   (door #3, #6, #9, ...), etc,   until you only visit the 100<sup>th</sup> door.


;Task:
Answer the question:   what state are the doors in after the last pass?   Which are open, which are closed?


'''[[task feature::Rosetta Code:extra credit|Alternate]]:'''
As noted in this page's   [[Talk:100 doors|discussion page]],   the only doors that remain open are those whose numbers are perfect squares.

Opening only those doors is an   [[task feature::Rosetta Code:optimization|optimization]]   that may also be expressed;
however, as should be obvious, this defeats the intent of comparing implementations across programming languages.





## 11l

{{trans|Python}}

```11l
V doors = [0B] * 100
L(i) 100
   L(j) (i .< 100).step(i + 1)
      doors[j] = !doors[j]
   print(‘Door ’(i + 1)‘: ’(I doors[i] {‘open’} E ‘close’))
```



## 360 Assembly


```360asm
*        100 doors                 13/08/2015
HUNDOOR  CSECT
         USING  HUNDOOR,R12
         LR     R12,R15
         LA     R6,0
         LA     R8,1               step 1
         LA     R9,100
LOOPI    BXH    R6,R8,ELOOPI       do ipass=1 to 100 (R6)
         LR     R7,R6
         SR     R7,R6
         LR     R10,R6             step ipass
         LA     R11,100
LOOPJ    BXH    R7,R10,ELOOPJ      do idoor=ipass to 100 by ipass (R7)
         LA     R5,DOORS-1
         AR     R5,R7
         XI     0(R5),X'01'        doors(idoor)=not(doors(idoor))
NEXTJ    B      LOOPJ
ELOOPJ   B      LOOPI
ELOOPI   LA     R10,BUFFER         R10 address of the buffer
         LA     R5,DOORS           R5 address of doors item
         LA     R6,1               idoor=1 (R6)
         LA     R9,100             loop counter
LOOPN    CLI    0(R5),X'01'        if doors(idoor)=1
         BNE    NEXTN
         XDECO  R6,XDEC            idoor to decimal
         MVC    0(4,R10),XDEC+8    move decimal to buffer
         LA     R10,4(R10)
NEXTN	 LA     R6,1(R6)           idoor=idoor+1
         LA     R5,1(R5)
         BCT    R9,LOOPN           loop
ELOOPN   XPRNT  BUFFER,80
RETURN   XR     R15,R15
         BR     R14
DOORS    DC     100X'00'
BUFFER   DC     CL80' '
XDEC     DS     CL12
         YREGS
         END    HUNDOOR
```

{{out}}

```txt
   1   4   9  16  25  36  49  64  81 100
```



## 4DOS Batch



```4DOS Batch

@echo off
set doors=%@repeat[C,100]
do step = 1 to 100
  do door = %step to 100 by %step
    set doors=%@left[%@eval[%door-1],%doors]%@if[%@instr[%@eval[%door-1],1,%doors]==C,O,C]%@right[%@eval[100-%door],%doors]
  enddo
enddo

```


The SET line consists of three functions:
<lang>
%@left[n,string]                      ^: Return n leftmost chars in string
%@right[n,string]                     ^: Return n rightmost chars in string
%@if[condition,true-val,false-val]    ^: Evaluate condition; return true-val if true, false-val if false

```


Here @IF is used to toggle between C and O.


## 6502 Assembly

{{works with|http://www.6502asm.com/beta/index.html www.6502asm.com|beta}}
'''unoptimized'''
Based on BASIC QB64 unoptimized version

```6502asm
; 100 DOORS in  6502 assembly language for: http://www.6502asm.com/beta/index.html
; Written for the original MOS Technology, Inc. NMOS version of the 6502, but should work with any version.
; Based on BASIC QB64 unoptimized version: http://rosettacode.org/wiki/100_doors#BASIC
;
; Notes:
;    Doors array[1..100] is at $0201..$0264. On the specified emulator, this is in video memory, so tbe results will
; be directly shown as pixels in the display.
;    $0200 (door 0) is cleared for display purposes but is not involved in the open/close loops.
;    Y register holds Stride
;    X register holds Index
;    Zero Page address $01 used to add Stride to Index (via A) because there's no add-to-X or add-Y-to-A instruction.

  ; First, zero door array
    LDA #00
    LDX #100
Z_LOOP:
    STA 200,X
    DEX
    BNE Z_LOOP
    STA 200,X

  ; Now do doors repeated open/close
    LDY #01        ; Initial value of Stride
S_LOOP:
    CPY #101
    BCS S_DONE
    TYA            ; Initial value of Index
I_LOOP:
    CMP #101
    BCS I_DONE
    TAX            ; Use as Door array index
    INC $200,X     ; Toggle bit 0 to reverse state of door
    STY 01         ; Add stride (Y) to index (X, via A)
    ADC 01
    BCC I_LOOP
I_DONE:
    INY
    BNE S_LOOP
S_DONE:

  ; Finally, format array values for output: 0 for closed, 1 for open
    LDX #100
C_LOOP:
    LDA $200,X
    AND #$01
    STA $200,X
    DEX
    BNE C_LOOP
```

48. bytes of code; the specified emulator does not report cycles.


{{works with|http://www.6502asm.com/ 6502asm.com|1.2}}
'''optimized'''
Largely inspired by the optimized C implementation -  makes use of the fact that finally only the doors whose numbers are squares of integers are open, as well as the fact that
 <math>n^2 = 1 + 3 + 5 + \ldots + (2n-1)</math>.

```6502asm
  ;assumes memory at $02xx is initially set to 0 and stack pointer is initialized
  ;the 1 to 100 door byte array will be at $0200-$0263 (decimal 512 to 611)
  ;Zero-page location $01 will hold delta
  ;At end, closed doors = $00, open doors = $01

start:    ldx #0        ;initialize index - first door will be at $200 + $0
          stx $1
          inc $1        ;start out with a delta of 1 (0+1=1)
openloop: inc $200,X    ;open X'th door
          inc $1        ;add 2 to delta
          inc $1
          txa           ;add delta to X by transferring X to A, adding delta to A, then transferring back to X
          clc           ;  clear carry before adding (6502 has no add-without-carry instruction)
          adc $1
          tax
          cpx #$64      ;check to see if we're at or past the 100th door (at $200 + $63)
          bmi openloop  ;jump back to openloop if less than 100
```

22. bytes of code; the specified emulator does not report cycles.


## 68000 Assembly

{{works with|http://www.easy68k.com/ EASy68K v5.13.00}}
Some of the macro code is derived from the examples included with EASy68K.

```68000devpac
*-----------------------------------------------------------
* Title      : 100Doors.X68
* Written by : G. A. Tippery
* Date       : 2014-01-17
* Description: Solves "100 Doors" problem, see: http://rosettacode.org/wiki/100_doors
* Notes      : Translated from C "Unoptimized" version, http://rosettacode.org/wiki/100_doors#unoptimized
*            : No optimizations done relative to C version; "for("-equivalent loops could be optimized.
*-----------------------------------------------------------

*
*   System-specific general console I/O macros (Sim68K, in this case)
*
PUTS    MACRO
    ** Print a null-terminated string w/o CRLF **
    ** Usage: PUTS stringaddress
    ** Returns with D0, A1 modified
        MOVEQ   #14,D0      ; task number 14 (display null string)
        LEA     \1,A1       ; address of string
        TRAP    #15         ; display it
        ENDM
*
PRINTN  MACRO
    ** Print decimal integer from number in register
    ** Usage: PRINTN register
    ** Returns with D0,D1 modified
        IFNC '\1','D1'      ;if some register other than D1
          MOVE.L \1,D1      ;put number to display in D1
        ENDC
        MOVE.B  #3,D0
        TRAP    #15         ;display number in D1
*
*   Generic constants
*
CR      EQU     13      ;ASCII Carriage Return
LF      EQU     10      ;ASCII Line Feed

*
*   Definitions specific to this program
*
*   Register usage:
*   D3 == pass (index)
*   D4 == door (index)
*   A2 == Doors array pointer
*
SIZE    EQU     100             ;Define a symbolic constant for # of doors

        ORG     $1000           ;Specify load address for program -- actual address system-specific
START:                          ; Execution starts here
        LEA     Doors,A2        ; make A2 point to Doors byte array
        MOVEQ   #0,D3
PassLoop:
        CMP     #SIZE,D3
        BCC     ExitPassLoop    ; Branch on Carry Clear - being used as Branch on Higher or Equal
        MOVE    D3,D4
DoorLoop:
        CMP     #SIZE,D4
        BCC     ExitDoorLoop
        NOT.B   0(A2,D4)
        ADD     D3,D4
        ADDQ    #1,D4
        BRA     DoorLoop
ExitDoorLoop:
        ADDQ    #1,D3
        BRA     PassLoop
ExitPassLoop:

* $28 = 40. bytes of code to this point. 32626 cycles so far.
*   At this point, the result exists as the 100 bytes starting at address Doors.
* To get output, we must use methods specific to the particular hardware, OS, or
* emulator system that the code is running on.  I use macros to "hide" some of the
* system-specific details; equivalent macros would be written for another system.

        MOVEQ   #0,D4
PrintLoop:
        CMP     #SIZE,D4
        BCC     ExitPrintLoop
        PUTS    DoorMsg1
        MOVE    D4,D1
        ADDQ    #1,D1           ; Convert index to 1-based instead of 0-based
        PRINTN  D1
        PUTS    DoorMsg2
        TST.B   0(A2,D4)        ; Is this door open (!= 0)?
        BNE     ItsOpen
        PUTS    DoorMsgC
        BRA     Next
ItsOpen:
        PUTS    DoorMsgO
Next:
        ADDQ    #1,D4
        BRA     PrintLoop
ExitPrintLoop:

*  What to do at end of program is also system-specific
        SIMHALT             ;Halt simulator
*
* $78 = 120. bytes of code to this point, but this will depend on how the I/O macros are actually written.
* Cycle count is nearly meaningless, as the I/O hardware and routines will dominate the timing.

*
*   Data memory usage
*
        ORG     $2000
Doors   DCB.B   SIZE,0      ;Reserve 100 bytes, prefilled with zeros

DoorMsg1 DC.B   'Door ',0
DoorMsg2 DC.B   ' is ',0
DoorMsgC DC.B   'closed',CR,LF,0
DoorMsgO DC.B   'open',CR,LF,0

        END     START       ;last line of source


```



## 8086 Assembly

See [[100 doors/8086 Assembly]]

## 8th


```forth

\ Array of doors; init to empty; accessing a non-extant member will return
\ 'null', which is treated as 'false', so we don't need to initialize it:
[] var, doors

\ given a door number, get the value and toggle it:
: toggle-door \ n --
	doors @ over a:@
	not rot swap a:! drop ;

\ print which doors are open:
: .doors
	(
		doors @ over a:@ nip
		if . space else drop then
	) 1 100 loop ;

\ iterate over the doors, skipping 'n':
: main-pass \ n --
	0
	true
	repeat
		drop
		dup toggle-door
		over n:+
		dup 101 <
	while 2drop drop ;

\ calculate the first 100 doors:
' main-pass 1 100 loop
\ print the results:
.doors cr bye

```

{{out}}
1 4 9 16 25 36 49 64 81 100


## ABAP

'''unoptimized'''

```ABAP
form open_doors_unopt.
  data: lv_door  type i,
        lv_count type i value 1.
  data: lt_doors type standard table of c initial size 100.
  field-symbols: <wa_door> type c.
  do 100 times.
    append initial line to lt_doors assigning <wa_door>.
    <wa_door> = 'X'.
  enddo.

  while lv_count < 100.
    lv_count = lv_count + 1.
    lv_door = lv_count.
    while lv_door < 100.
      read table lt_doors index lv_door assigning <wa_door>.
      if <wa_door> = ' '.
        <wa_door> = 'X'.
      else.
        <wa_door> = ' '.
      endif.
      add lv_count to lv_door.
    endwhile.
  endwhile.

  loop at lt_doors assigning <wa_door>.
    if <wa_door> = 'X'.
      write : / 'Door', (4) sy-tabix right-justified, 'is open' no-gap.
    endif.
  endloop.
endform.
```


'''unoptimized / functional'''

```ABAP

cl_demo_output=>display( REDUCE stringtab( INIT list TYPE stringtab
                                              aux TYPE i
                                          FOR door = 1 WHILE door <= 100
                                          FOR pass = 1 WHILE pass <= 100
                                         NEXT aux   = COND #( WHEN pass = 1 THEN 1
                                                              WHEN door MOD pass = 0 THEN aux + 1 ELSE aux  )
                                              list  = COND #( WHEN pass = 100
                                                                THEN COND #( WHEN aux MOD 2 <> 0 THEN VALUE #( BASE list ( CONV #( door ) ) )
                                                                              ELSE list ) ELSE list ) ) ).


```


'''optimized'''

Using <math>\sum_{i=1}^n (2i-1) = n^2</math>

```ABAP
form open_doors_opt.
  data: lv_square type i value 1,
        lv_inc    type i value 3.
  data: lt_doors  type standard table of c initial size 100.
  field-symbols: <wa_door> type c.
  do 100 times.
    append initial line to lt_doors assigning <wa_door>.
    if sy-index = lv_square.
      <wa_door> = 'X'.
      add: lv_inc to lv_square, 2 to lv_inc.
      write : / 'Door', (4) sy-index right-justified, 'is open' no-gap.
    endif.
  enddo.
endform.
```



'''ultra-optimized / imperative'''

```ABAP

DO 10 TIMES.
  DATA(val) = sy-index * sy-index.
  WRITE: / val.
ENDDO.

```


'''ultra-optimized / functional'''

```ABAP

cl_demo_output=>display( REDUCE stringtab( INIT list TYPE stringtab
                                          FOR i = 1 WHILE i <= 10
                                         NEXT list = VALUE #( BASE list ( i * i ) ) ) ).

```



## ACL2



```lisp
(defun rep (n x)
   (if (zp n)
       nil
       (cons x
             (rep (- n 1) x))))

(defun toggle-every-r (n i bs)
   (if (endp bs)
       nil
       (cons (if (zp i)
                 (not (first bs))
                 (first bs))
             (toggle-every-r n (mod (1- i) n) (rest bs)))))

(defun toggle-every (n bs)
   (toggle-every-r n (1- n) bs))

(defun 100-doors (i doors)
   (if (zp i)
       doors
       (100-doors (1- i) (toggle-every i doors))))
```



## ActionScript

{{works with|ActionScript|3.0}}
'''unoptimized'''

```actionscript
package {
    import flash.display.Sprite;

    public class Doors extends Sprite {
        public function Doors() {

            // Initialize the array
            var doors:Array = new Array(100);
            for (var i:Number = 0; i < 100; i++) {
                doors[i] = false;

            // Do the work
            for (var pass:Number = 0; pass < 100; pass++) {
                for (var j:Number = pass; j < 100; j += (pass+1)) {
                    doors[j] = !doors[j];
                }
            }
            trace(doors);
        }
    }
}
```



## Acurity Architect


```txt

Using #HASH-OFF, OPTION OICC ="^" , CICC ="^"

```


```acurity architect

VAR sStatus: SHORT
VAR sArray: SHORT
VAR sCount: SHORT
VAR sDoor: SHORT
VAR sPass: SHORT
VAR zIndex: STRING
VAR zState: STRING
//
SET sStatus = GET_UNUSED_ARRAY_HANDLE(sArray)
SET sStatus = INIT_SORTED_ARRAY(sArray, 0, 0, 1)
//
DO sCount = 1 TO 100
  DO sPass = 1 TO 100
    SET sDoor = sCount * sPass
    IF sDoor <= 100
      SET zIndex = REPEAT("0", 3 - LENGTH(STR(sDoor))) + STR(sDoor)
      SET sStatus = READ_ARRAY_REC("=", sArray, zIndex)
      SET zState = "OPEN"
      IF GET_STRING_SAY(sArray, 1) = "OPEN"
        SET zState = "CLOSE"
      ENDIF
      //
      SET sStatus = ADD_ARRAY_REC(sArray, zIndex)
      SET sStatus = PUT_STRING_SAY(sArray, 1, zState)
    ELSE
      BREAK
    ENDIF
  ENDDO
ENDDO
//
SET zIndex = ""
SET sStatus = READ_ARRAY_REC(">=", sArray, zIndex)
DO WHILE sStatus = 0
  >>Door:  ^zIndex^  State: ^GET_STRING_SAY(sArray, 1)^
  SET sStatus = READ_ARRAY_REC("+", sArray, zIndex)
ENDDO

```

{{out}}

```txt

Door:  001  State: OPEN
Door:  002  State: CLOSE
Door:  003  State: CLOSE
Door:  004  State: OPEN
Door:  005  State: CLOSE
Door:  006  State: CLOSE
Door:  007  State: CLOSE
Door:  008  State: CLOSE
Door:  009  State: OPEN
Door:  010  State: CLOSE
Door:  011  State: CLOSE
Door:  012  State: CLOSE
Door:  013  State: CLOSE
Door:  014  State: CLOSE
Door:  015  State: CLOSE
Door:  016  State: OPEN
Door:  017  State: CLOSE
Door:  018  State: CLOSE
Door:  019  State: CLOSE
Door:  020  State: CLOSE
Door:  021  State: CLOSE
Door:  022  State: CLOSE
Door:  023  State: CLOSE
Door:  024  State: CLOSE
Door:  025  State: OPEN
Door:  026  State: CLOSE
Door:  027  State: CLOSE
Door:  028  State: CLOSE
Door:  029  State: CLOSE
Door:  030  State: CLOSE
Door:  031  State: CLOSE
Door:  032  State: CLOSE
Door:  033  State: CLOSE
Door:  034  State: CLOSE
Door:  035  State: CLOSE
Door:  036  State: OPEN
Door:  037  State: CLOSE
Door:  038  State: CLOSE
Door:  039  State: CLOSE
Door:  040  State: CLOSE
Door:  041  State: CLOSE
Door:  042  State: CLOSE
Door:  043  State: CLOSE
Door:  044  State: CLOSE
Door:  045  State: CLOSE
Door:  046  State: CLOSE
Door:  047  State: CLOSE
Door:  048  State: CLOSE
Door:  049  State: OPEN
Door:  050  State: CLOSE
Door:  051  State: CLOSE
Door:  052  State: CLOSE
Door:  053  State: CLOSE
Door:  054  State: CLOSE
Door:  055  State: CLOSE
Door:  056  State: CLOSE
Door:  057  State: CLOSE
Door:  058  State: CLOSE
Door:  059  State: CLOSE
Door:  060  State: CLOSE
Door:  061  State: CLOSE
Door:  062  State: CLOSE
Door:  063  State: CLOSE
Door:  064  State: OPEN
Door:  065  State: CLOSE
Door:  066  State: CLOSE
Door:  067  State: CLOSE
Door:  068  State: CLOSE
Door:  069  State: CLOSE
Door:  070  State: CLOSE
Door:  071  State: CLOSE
Door:  072  State: CLOSE
Door:  073  State: CLOSE
Door:  074  State: CLOSE
Door:  075  State: CLOSE
Door:  076  State: CLOSE
Door:  077  State: CLOSE
Door:  078  State: CLOSE
Door:  079  State: CLOSE
Door:  080  State: CLOSE
Door:  081  State: OPEN
Door:  082  State: CLOSE
Door:  083  State: CLOSE
Door:  084  State: CLOSE
Door:  085  State: CLOSE
Door:  086  State: CLOSE
Door:  087  State: CLOSE
Door:  088  State: CLOSE
Door:  089  State: CLOSE
Door:  090  State: CLOSE
Door:  091  State: CLOSE
Door:  092  State: CLOSE
Door:  093  State: CLOSE
Door:  094  State: CLOSE
Door:  095  State: CLOSE
Door:  096  State: CLOSE
Door:  097  State: CLOSE
Door:  098  State: CLOSE
Door:  099  State: CLOSE
Door:  100  State: OPEN

```



## Ada

'''unoptimized'''

```ada
with Ada.Text_Io; use Ada.Text_Io;

 procedure Doors is
    type Door_State is (Closed, Open);
    type Door_List is array(Positive range 1..100) of Door_State;
    The_Doors : Door_List := (others => Closed);
 begin
    for I in 1..100 loop
       for J in The_Doors'range loop
          if J mod I = 0 then
             if The_Doors(J) = Closed then
                 The_Doors(J) := Open;
             else
                The_Doors(J) := Closed;
             end if;
          end if;
       end loop;
    end loop;
    for I in The_Doors'range loop
       Put_Line(Integer'Image(I) & " is " & Door_State'Image(The_Doors(I)));
    end loop;
 end Doors;
```


'''optimized'''

```ada
with Ada.Text_Io; use Ada.Text_Io;
 with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

 procedure Doors_Optimized is
    Num : Float;
 begin
    for I in 1..100 loop
       Num := Sqrt(Float(I));
       Put(Integer'Image(I) & " is ");
       if Float'Floor(Num) = Num then
          Put_Line("Opened");
       else
          Put_Line("Closed");
       end if;
    end loop;
 end Doors_Optimized;
```



## Agena

Translation of Algol W. Tested with Agena 2.9.5 Win32

```agena
# find the first few squares via the unoptimised door flipping method
scope

    local doorMax := 100;
    local door;
    create register door( doorMax );

    # set all doors to closed
    for i to doorMax do door[ i ] := false od;

    # repeatedly flip the doors
    for i to doorMax do
        for j from i to doorMax by i do door[ j ] := not door[ j ] od
    od;

    # display the results
    for i to doorMax do if door[ i ] then write( " ", i ) fi od; print()

epocs
```



## Aikido


```aikido

var doors = new int [100]

foreach pass 100 {
    for (var door = pass ; door < 100 ; door += pass+1) {
        doors[door] = !doors[door]
    }
}

var d = 1
foreach door doors {
    println ("door " + d++ + " is " + (door ? "open" : "closed"))

}


```



## ALGOL 68

'''unoptimized'''

```algol68
# declare some constants #
INT limit = 100;

PROC doors = VOID:
(
  MODE DOORSTATE = BOOL;
  BOOL closed = FALSE;
  BOOL open = NOT closed;
  MODE DOORLIST = [limit]DOORSTATE;

  DOORLIST the doors;
  FOR i FROM LWB the doors TO UPB the doors DO the doors[i]:=closed OD;

  FOR i FROM LWB the doors TO UPB the doors DO
    FOR j FROM LWB the doors TO UPB the doors DO
      IF j MOD i = 0 THEN
        the doors[j] :=  NOT the doors[j]
      FI
    OD
  OD;
  FOR i FROM LWB the doors TO UPB the doors DO
    printf(($g" is "gl$,i,(the doors[i]|"opened"|"closed")))
  OD
);
doors;
```

'''optimized'''

```algol68
PROC doors optimised = ( INT limit )VOID:
  FOR i TO limit DO
    REAL num := sqrt(i);
    printf(($g" is "gl$,i,(ENTIER num = num |"opened"|"closed") ))
  OD
;
doors optimised(limit)
```



## ALGOL W


```algolw
begin
    % find the first few squares via the unoptimised door flipping method   %

    integer doorMax;
    doorMax := 100;

    begin
        % need to start a new block so the array can have variable bounds   %

        % array of doors - door( i ) is true if open, false if closed       %
        logical array door( 1 :: doorMax );

        % set all doors to closed                                           %
        for i := 1 until doorMax do door( i ) := false;

        % repeatedly flip the doors                                         %
        for i := 1 until doorMax
        do begin
            for j := i step j until doorMax
            do begin
                door( j ) := not door( j )
            end
        end;

        % display the results                                               %
        i_w := 1; % set integer field width                                 %
        s_w := 1; % and separator width                                     %
        for i := 1 until doorMax do if door( i ) then writeon( i )

    end

end.
```

{{out}}

```txt

 1 4 9 16 25 36 49 64 81 100

```



## AmigaE


```amigae
PROC main()
  DEF t[100]: ARRAY,
      pass, door
  FOR door := 0 TO 99 DO t[door] := FALSE
  FOR pass := 0 TO 99
    door := pass
    WHILE door <= 99
      t[door] := Not(t[door])
      door := door + pass + 1
    ENDWHILE
  ENDFOR
  FOR door := 0 TO 99 DO WriteF('\d is \s\n', door+1,
                                IF t[door] THEN 'open' ELSE 'closed')
ENDPROC
```



## APL

{{works with|GNU APL}}


```APL
doors←{100⍴((⍵-1)⍴0),1}
≠⌿⊃doors¨ ⍳100
```

{{out}}

```txt

1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
      0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1

```


'''optimized'''
Note that &#9109;IO = 1

```txt

2|+/[1]0=D&#8728;.|D&#8592;&#9075;100

```

The idea is that the <i>n</i>:th door will be flipped the same number of times as there are divisors for <i>n</i>. So first we make D all ints 1..100 (D&#8592;&#9075;100).
The next step is to find the remainders of every such int when divided by every other (D&#8728;.|D).
 This results in a 100&#215;100 matrix which we turn into a binary one by testing if the values are equal to zero i.e. divisors.
Next: sum along axis 1, i.e. the columns. This tells us the number of divisors. Finally calculate the remainder of these when divided by 2, i.e. find which <i>n</i> have an odd number of divisors, i.e. will be flipped an odd number of times and thus end up open.


## AppleScript



### Iteration



```AppleScript
set is_open to {}
repeat 100 times
   set end of is_open to false
end
repeat with pass from 1 to 100
  repeat with door from pass to 100 by pass
    set item door of is_open to not item door of is_open
  end
end
set open_doors to {}
repeat with door from 1 to 100
   if item door of is_open then
     set end of open_doors to door
   end
end
set text item delimiters to ", "
display dialog "Open doors: " & open_doors
```



### Functional composition



```AppleScript
-- FINAL DOOR STATES ---------------------------------------------------------

-- finalDoors :: Int -> [(Int, Bool)]
on finalDoors(n)

    -- toggledCorridor :: [(Int, Bool)] -> (Int, Bool) -> Int -> [(Int, Bool)]
    script toggledCorridor
        on |λ|(a, _, k)

            -- perhapsToggled :: Bool -> Int -> Bool
            script perhapsToggled
                on |λ|(x, i)
                    if i mod k = 0 then
                        {i, not item 2 of x}
                    else
                        {i, item 2 of x}
                    end if
                end |λ|
            end script

            map(perhapsToggled, a)
        end |λ|
    end script

    set xs to enumFromTo(1, n)

    foldl(toggledCorridor, ¬
        zip(xs, replicate(n, {false})), xs)
end finalDoors

-- TEST ----------------------------------------------------------------------
on run
    -- isOpenAtEnd :: (Int, Bool) -> Bool
    script isOpenAtEnd
        on |λ|(door)
            (item 2 of door)
        end |λ|
    end script

    -- doorNumber :: (Int, Bool) -> Int
    script doorNumber
        on |λ|(door)
            (item 1 of door)
        end |λ|
    end script

    map(doorNumber, filter(isOpenAtEnd, finalDoors(100)))

    --> {1, 4, 9, 16, 25, 36, 49, 64, 81, 100}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    repeat with i from 1 to lng
        set end of lst to {item i of xs, item i of ys}
    end repeat
    return lst
end zip
```

{{Out}}

```AppleScript
{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}
```



### Odd numbers of integer factors


The question of which doors are flipped an odd number of times reduces to the question of which numbers in the range have an odd number of integer factors (for an AppleScript implementation of integerFactors(n) see Factors of An Integer). Using '''map''' from the functional composition example above:


```AppleScript
map(factorCountMod2, enumFromTo(1, 100))

on factorCountMod2(n)
    {n, (length of integerFactors(n)) mod 2 = 1}
end factorCountMod2
```


This, on inspection, and further reflection, then collapses to the even simpler question of which numbers are perfect squares, since all other numbers have an even number of integer factors (n factors below the square root, plus n paired cofactors above the square root). Using '''map''' and '''enumFromTo''' from the functional composition example above:


```AppleScript
-- perfectSquaresUpTo :: Int -> [Int]
on perfectSquaresUpTo(n)
    script squared
        -- (Int -> Int)
        on |λ|(x)
            x * x
        end |λ|
    end script

    set realRoot to n ^ (1 / 2)
    set intRoot to realRoot as integer
    set blnNotPerfectSquare to not (intRoot = realRoot)

    map(squared, enumFromTo(1, intRoot - (blnNotPerfectSquare as integer)))
end perfectSquaresUpTo

on run

    perfectSquaresUpTo(100)

end run
```

{{Out}}

```AppleScript
{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}
```



## Arbre


```Arbre

openshut(n):
  for x in [1..n]
    x%n==0

pass(n):
  if n==100
    openshut(n)
  else
    openshut(n) xor pass(n+1)

100doors():
  pass(1) -> io

```



## Argile


```Argile
use std, array

close all doors
for each pass from 1 to 100
  for (door = pass) (door <= 100) (door += pass)
    toggle door

let int pass, door.

.: close all doors :. {memset doors 0 size of doors}
.:toggle <int door>:. {    !!(doors[door - 1])     }

let doors be an array of 100 bool

for each door from 1 to 100
  printf "#%.3d %s\n" door (doors[door - 1]) ? "[ ]", "[X]"
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

'''unoptimized'''

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program 100doors.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1                                 @ Linux output console
.equ EXIT,   1                                 @ Linux syscall
.equ WRITE,  4                                 @ Linux syscall
.equ NBDOORS,   100
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:       .ascii "The door "
sMessValeur:       .fill 11, 1, ' '            @ size => 11
                      .asciz "is open.\n"

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
stTableDoors:	.skip   4 * NBDOORS
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                         @ entry of program
    push {fp,lr}                              @ saves 2 registers
    @ display first line
    ldr r3,iAdrstTableDoors                   @ table address
    mov r5,#1
1:
    mov r4,r5
2:                                            @ begin loop
    ldr r2,[r3,r4,lsl #2]                     @ read doors index r4
    cmp r2,#0
    moveq r2,#1                               @ if r2 = 0   1 -> r2
    movne r2,#0                               @ if r2 = 1   0 -> r2
    str r2,[r3,r4,lsl #2]                     @ store value of doors
    add r4,r5                                 @ increment r4 with  r5 value
    cmp r4,#NBDOORS                           @ number of doors ?
    ble 2b                                    @ no -> loop
    add r5,#1                                 @ increment the increment !!
    cmp r5,#NBDOORS                           @ number of doors ?
    ble 1b                                    @ no -> loop

                                              @ loop display state doors
    mov r4,#0
3:
    ldr r2,[r3,r4,lsl #2]                     @ read state doors r4 index
    cmp r2,#0
    beq 4f
    mov r0,r4                                 @ open -> display message
    ldr r1,iAdrsMessValeur                    @ display value index
    bl conversion10                           @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                          @ display message
4:
    add r4,#1
    cmp r4,#NBDOORS
    ble 3b                                    @ loop


100:                                          @ standard end of the program
    mov r0, #0                                @ return code
    pop {fp,lr}                               @restaur 2 registers
    mov r7, #EXIT                             @ request to exit program
    svc #0                                    @ perform the system call

iAdrsMessValeur:                .int sMessValeur
iAdrstTableDoors:		.int stTableDoors
iAdrsMessResult:		.int sMessResult

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                     @ save  registres
    mov r2,#0                                 @ counter length
1:                                            @ loop length calculation
    ldrb r1,[r0,r2]                           @ read octet start position + index
    cmp r1,#0                                 @ if 0 its over
    addne r2,r2,#1                            @ else add 1 in the length
    bne 1b                                    @ and loop
                                              @ so here r2 contains the length of the message
    mov r1,r0        			      @ address message in r1
    mov r0,#STDOUT      		      @ code to write to the standard output Linux
    mov r7, #WRITE                            @ code call system "write"
    svc #0                                    @ call systeme
    pop {r0,r1,r2,r7,lr}                      @ restaur des  2 registres */
    bx lr                                     @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                            @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                       @ start loop
    bl divisionpar10U                          @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                 @ digit
    strb r1,[r3,r2]                            @ store digit on area
    cmp r0,#0                                  @ stop if quotient = 0
    subne r2,#1                                @ else previous position
    bne 1b	                               @ and loop
    // and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
    // and move spaces in end on area
    mov r0,r4                                 @ result length
    mov r1,#' '                               @ space
3:
    strb r1,[r3,r4]                           @ store space in area
    add r4,#1                                 @ next position
    cmp r4,#LGZONECAL
    ble 3b                                    @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                            @ restaur registres
    bx lr                                     @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                        @ save value
    //mov r3,#0xCCCD                                 @ r3 <- magic_number  lower   @ for Raspberry pi 3
    //movt r3,#0xCCCC                                @ r3 <- magic_number  upper   @ for Raspberry pi 3
    ldr r3,iMagicNumber                              @ for Raspberry pi 1 2
    umull r1, r2, r3, r0                             @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                               @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                             @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                             @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                            @ leave function
iMagicNumber:            .int 0xCCCCCCCD

```

'''optimized'''

```ARM Assembly

/*********************************************/
/* optimized version                         */
/*********************************************/
/* ARM assembly Raspberry PI  */
/*  program 100doors.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ NBDOORS,   100
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:       .ascii "The door "
sMessValeur:       .fill 11, 1, ' '                 @ size => 11
                   .asciz "is open.\n"

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                               @ entry of program
    push {fp,lr}                                    @ saves 2 registers
                                                    @ display first line
    mov r5,#3                                       @ start value of increment
    mov r4,#1                                       @ start doors
                                                    @ loop display state doors
1:
    mov r0,r4                                       @ open -> display message
    ldr r1,iAdrsMessValeur                          @ display value index
    bl conversion10                                 @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                                @ display message
    add r4,r5                                       @ add increment
    add r5,#2                                       @ new increment
    cmp r4,#NBDOORS
    ble 1b                                          @ loop


100:   @ standard end of the program
    mov r0, #0                                      @ return code
    pop {fp,lr}                                     @ restaur 2 registers
    mov r7, #EXIT                                   @ request to exit program
    svc #0                                          @ perform the system call

iAdrsMessValeur:                .int sMessValeur
iAdrsMessResult:		.int sMessResult

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                           @ save  registres
    mov r2,#0                                       @ counter length
1:                                                  @ loop length calculation
    ldrb r1,[r0,r2]                                 @ read octet start position + index
    cmp r1,#0                                       @ if 0 its over
    addne r2,r2,#1                                  @ else add 1 in the length
    bne 1b                                          @ and loop
                                                    @ so here r2 contains the length of the message
    mov r1,r0        			            @ address message in r1
    mov r0,#STDOUT      		            @ code to write to the standard output Linux
    mov r7, #WRITE                                  @ code call system "write"
    svc #0                                          @ call systeme
    pop {r0,r1,r2,r7,lr}                            @ restaur des  2 registres */
    bx lr                                           @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                            @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b	                                    @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                    @ and move spaces in end on area
    mov r0,r4                                       @ result length
    mov r1,#' '                                     @ space
3:
    strb r1,[r3,r4]                                 @ store space in area
    add r4,#1                                       @ next position
    cmp r4,#LGZONECAL
    ble 3b                                          @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                  @ restaur registres
    bx lr                                           @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                       @ save value
    //mov r3,#0xCCCD                                @ r3 <- magic_number  lower   @ for raspberry 3
    //movt r3,#0xCCCC                               @ r3 <- magic_number  upper   @ for raspberry 3
    ldr r3,iMagicNumber                             @ for raspberry 1 2
    umull r1, r2, r3, r0                            @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                              @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                            @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                            @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                           @ leave function
iMagicNumber:            .int 0xCCCCCCCD

```



## Astro


```python
var doors = falses(100)

for a in 1..100: for b in a..a..100:
    doors[b] = not doors[b]

for a in 1..100:
    print "Door $a is ${(doors[a]) ? 'open.': 'closed.'}"

```



## ATS


```ATS

#include "share/atspre_staload.hats"

implement
main0((*void*)) = let
//
var A = @[bool][100](false)
val A = $UNSAFE.cast{arrayref(bool,100)}(addr@A)
//
fnx
loop
(
  pass: intGte(0)
) : void =
  if pass < 100
    then loop2 (pass, pass)
  // end of [if]
and
loop2
(
  pass: natLt(100), door: intGte(0)
) : void =
  if door < 100
    then (A[door] := ~A[door]; loop2(pass, door+pass+1))
    else loop(pass+1)
  // end of [if]
//
fun
loop3
(
  door: intGte(0)
) : void =
  if door < 100
    then (
      println!("door #", door+1, " is ", (if A[door] then "open" else "closed"): string, ".");
      loop3(door+1)
    ) (* end of [then] *)
  // end of [if]
//
in
  loop(0); loop3 (0)
end // end of [main0]

```



## AutoHotkey


###  Standard Approach


```autohotkey
Loop, 100
  Door%A_Index% := "closed"

Loop, 100 {
  x := A_Index, y := A_Index
  While (x <= 100)
  {
    CurrentDoor := Door%x%
    If CurrentDoor contains closed
    {
      Door%x% := "open"
      x += y
    }
    else if CurrentDoor contains open
    {
      Door%x% := "closed"
      x += y
    }
  }
}

Loop, 100 {
   CurrentDoor := Door%A_Index%
   If CurrentDoor contains open
      Res .= "Door " A_Index " is open`n"
}
MsgBox % Res
```



###  Alternative Approach

Making use of the identity:

<math>\sum_{i=1}^n (2i-1) = n^2</math>

```autohotkey
increment := 3, square := 1
Loop, 100
    If (A_Index = square)
        outstring .= "`nDoor " A_Index " is open"
        ,square += increment, increment += 2
MsgBox,, Succesfull, % SubStr(outstring, 2)
```


###  Optimized


```autohotkey
While (Door := A_Index ** 2) <= 100
   Result .= "Door " Door " is open`n"
MsgBox, %Result%
```



## AutoIt


```AutoIt

#include <array.au3>
$doors = 100

;door array, 0 = closed, 1 = open
Local $door[$doors +1]

For $ii = 1 To $doors
	For $i = $ii To $doors Step $ii
		$door[$i] = Not $door[$i]
	next
Next

;display to screen
For $i = 1 To $doors
	ConsoleWrite (Number($door[$i])& " ")
	If Mod($i,10) = 0 Then ConsoleWrite(@CRLF)
Next

```



## Axiom

Unoptimized:
```Axiom
(open,closed,change,open?) := (true,false,not,test);
doors := bits(100,closed);
for i in 1..#doors repeat
  for j in i..#doors by i repeat
    doors.j := change doors.j
[i for i in 1..#doors | open? doors.i]

```
Optimized:
```Axiom
[i for i in 1..100 | perfectSquare? i] -- or
[i^2 for i in 1..sqrt(100)::Integer]
```


## AWK

'''unoptimized'''

```awk
BEGIN {
  for(i=1; i <= 100; i++)
  {
    doors[i] = 0 # close the doors
  }
  for(i=1; i <= 100; i++)
  {
    for(j=i; j <= 100; j += i)
    {
      doors[j] = (doors[j]+1) % 2
    }
  }
  for(i=1; i <= 100; i++)
  {
    print i, doors[i] ? "open" : "close"
  }
}
```

'''optimized'''

```awk
BEGIN {
  for(i=1; i <= 100; i++) {
    doors[i] = 0 # close the doors
  }
  for(i=1; i <= 100; i++) {
    if ( int(sqrt(i)) == sqrt(i) ) {
      doors[i] = 1
    }
  }
  for(i=1; i <= 100; i++)
  {
    print i, doors[i] ? "open" : "close"
  }
}
```



## B

{{works with|The Amsterdam Compiler Kit - B|V6.1pre1}}

```B
main()
{
  auto doors[100]; /* != 0 means open */
  auto pass, door;

  door = 0;
  while( door<100 ) doors[door++] = 0;

  pass = 0;
  while( pass<100 )
  {
    door = pass;
    while( door<100 )
    {
      doors[door] = !doors[door];
      door =+ pass+1;
    }
    ++pass;
  }

  door = 0;
  while( door<100 )
  {
    printf("door #%d is %s.*n", door+1, doors[door] ? "open" : "closed");
    ++door;
  }

  return(0);
}
```



## BASIC

=
## Applesoft BASIC
=
Based on the Sinclair ZX81 BASIC implementation.

```basic

 100 :
 110  REM  100 DOORS PROBLEM
 120 :
 130  DIM D(100)
 140  FOR P = 1 TO 100
 150  FOR T = P TO 100 STEP P
 160  D(T) =  NOT D(T): NEXT T
 170  NEXT P
 180  FOR I = 1 TO 100
 190  IF D(I) THEN  PRINT I;" ";
 200  NEXT I

```

{{out}}

```txt

]RUN
1 4 9 16 25 36 49 64 81 100
```


=
## BASIC256
=

```BASIC256
# 100 doors problem
dim d(100)

# simple solution
print "simple solution"
gosub initialize
for t = 1 to 100
   for j = t to 100 step t
      d[j-1] = not d[j-1]
   next j
next t
gosub showopen

# more optimized solution
print "more optimized solution"
gosub initialize
for t = 1 to 10
      d[t^2-1] = true
next t
gosub showopen
end

initialize:
for t = 1 to d[?]
   d[t-1] = false	 # closed
next t
return

showopen:
for t = 1 to d[?]
   print d[t-1]+ " ";
   if t%10 = 0 then print
next t
return
```


=
## Commodore BASIC
=
Based on the Sinclair ZX81 BASIC implementation.

```gwbasic
10 DIM D(100)
20 FOR I=1 TO 100
30 FOR J=I TO 100 STEP I
40 D(J) = NOT D(J)
50 NEXT J
60 NEXT I
70 FOR I=1 TO 100
80 IF D(I) THEN PRINT I,
90 NEXT I
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "100doors.bas"
110 NUMERIC D(1 TO 100)
120 FOR I=1 TO 100
130   LET D(I)=0
140 NEXT
150 FOR I=1 TO 100
160   FOR J=I TO 100 STEP I
170     LET D(J)=NOT D(J)
180   NEXT
190 NEXT
200 FOR I=1 TO 100
210   IF D(I) THEN PRINT I
220 NEXT
```

Optimized:
<lang IS-BASIC>100 PROGRAM "100doors.bas"
110 LET NR=1:LET D=3
120 DO
130   PRINT NR
140   LET NR=NR+D:LET D=D+2
150 LOOP WHILE NR<=100
```



### QBASIC

{{works with|QBASIC, QB64}}

'''unoptimized'''

```qbasic
REM "100 Doors" program for QB64 BASIC (http://www.qb64.net/), a QuickBASIC-like compiler.
REM Author: G. A. Tippery
REM Date: 12-Feb-2014
REM
REM   Unoptimized (naive) version, per specifications at http://rosettacode.org/wiki/100_doors

DEFINT A-Z
CONST N = 100
DIM door(N)

FOR stride = 1 TO N
    FOR index = stride TO N STEP stride
        LET door(index) = NOT (door(index))
    NEXT index
NEXT stride

PRINT "Open doors:"
FOR index = 1 TO N
    IF door(index) THEN PRINT index
NEXT index

END
```


{{works with|QuickBasic|4.5}}

'''unoptimized'''

```qbasic
DIM doors(0 TO 99)
FOR pass = 0 TO 99
	FOR door = pass TO 99 STEP pass + 1
		PRINT doors(door)
		PRINT NOT doors(door)
		doors(door) = NOT doors(door)
	NEXT door
NEXT pass
FOR i = 0 TO 99
	PRINT "Door #"; i + 1; " is ";
	IF NOT doors(i) THEN
		PRINT "closed"
	ELSE
		PRINT "open"
	END IF
NEXT i
```

'''optimized'''

```qbasic
DIM doors(0 TO 99)
FOR door = 0 TO 99
	IF INT(SQR(door)) = SQR(door) THEN doors(door) = -1
NEXT door
FOR i = 0 TO 99
	PRINT "Door #"; i + 1; " is ";
	IF NOT doors(i) THEN
		PRINT "closed"
	ELSE
		PRINT "open"
	END IF
NEXT i
```

'''optimized'''

```qbasic

'lrcvs 04.11.12
cls
x = 1 : y = 3 : z = 0
print x + " Open"
do
z = x + y
print z + " Open"
x = z : y = y + 2
until z >= 100
end

```


=
## Sinclair ZX81 BASIC
=
Works with only 1k of RAM, although it doesn't leave too much to play with.

```basic
10 DIM D(100)
20 FOR I=1 TO 100
30 FOR J=I TO 100 STEP I
40 LET D(J)=NOT D(J)
50 NEXT J
60 NEXT I
70 FOR I=1 TO 100
80 IF D(I) THEN PRINT I,
90 NEXT I
```



## BaCon


```qbasic

OPTION BASE 1

DECLARE doors[100]

FOR size = 1 TO 100
    FOR pass = 0 TO 100 STEP size
	doors[pass] = NOT(doors[pass])
    NEXT
NEXT

FOR which = 1 TO 100
    IF doors[which] THEN PRINT which
NEXT

```

{{out}}

```txt

1
4
9
16
25
36
49
64
81
100

```



## Batch File


'''unoptimized'''

```dos

@echo off
setlocal enableDelayedExpansion
:: 0 = closed
:: 1 = open
:: SET /A treats undefined variable as 0
:: Negation operator ! must be escaped because delayed expansion is enabled
for /l %%p in (1 1 100) do for /l %%d in (%%p %%p 100) do set /a "door%%d=^!door%%d"
for /l %%d in (1 1 100) do if !door%%d!==1 (
  echo door %%d is open
) else echo door %%d is closed

```


'''optimized'''

```dos

@echo off
setlocal enableDelayedExpansion
set /a square=1, incr=3
for /l %%d in (1 1 100) do (
  if %%d neq !square! (echo door %%d is closed) else (
    echo door %%d is open
    set /a square+=incr, incr+=2
  )
)

```



## BBC BASIC


```bbcbasic
      DIM doors%(100)

      FOR pass% = 1 TO 100
        FOR door% = pass% TO 100 STEP pass%
          doors%(door%) = NOT doors%(door%)
        NEXT door%
      NEXT pass%

      FOR door% = 1 TO 100
        IF doors%(door%) PRINT "Door " ; door% " is open"
      NEXT door%
```



## bc


```bc
/* 0 means door is closed, 1 means door is open */
for (i = 0; i < 100; i++) {
    for (j = i; j < 100; j += (i + 1)) {
        d[j] = 1 - d[j]     /* Toggle door */
    }
}

"Open doors:
"
for (i = 0; i < 100; i++) {
    if (d[i] == 1) (i + 1)
}
```



## Befunge


===Befunge-93===

### =Unoptimized=

Requires an interpreter with working read-write memory support. Padding the code page with extra blank lines can sometimes help.

```befunge>
"d">:00p1-:>:::9%\9/9+g2%!\:9v
$.v_^#!$::$_^#`"c":+g00p+9/9\%<
::<_@#`$:\*:+55:+1p27g1g+9/9\%9

```


### =Optimized=

Just calculates the first 10 perfect squares.

```befunge
1+:::*.9`#@_

```


===Befunge-98===
{{works with|CCBI|2.1}}

```befunge>108p0>:18p;;
:9g!18g9p08g]
*`!0\|+relet|-1`*aap81::+]
;::+1<r]!g9;>$08g1+:08paa[
*`#@_^._aa
```



## BlitzMax

{{works with|BlitzMax|1.37}}

'''optimized'''

```BlitzMax
Graphics 640,480
i=1
While ((i*i)<=100)
	a$=i*i
	DrawText a$,10,20*i
	Print i*i
	i=i+1
Wend
Flip
WaitKey
```



## BlooP

The currently available BlooP interpreters don't really allow iterating over cells with any level of ease, so instead I loop over each door in turn, running it through all 100 cycles and toggling it when it is a multiple of the step number.

```BlooP

DEFINE PROCEDURE ''DIVIDE'' [A,B]:
BLOCK 0: BEGIN
  IF A < B, THEN:
    QUIT BLOCK 0;
  CELL(0) <= 1;
  OUTPUT <= 1;
  LOOP AT MOST A TIMES:
  BLOCK 2: BEGIN
    IF OUTPUT * B = A, THEN:
    QUIT BLOCK 0;
    OUTPUT <= OUTPUT + 1;
    IF OUTPUT * B > A, THEN:
    BLOCK 3: BEGIN
      OUTPUT <= CELL(0);
      QUIT BLOCK 0;
    BLOCK 3: END;
    CELL(0) <= OUTPUT;
  BLOCK 2: END;
BLOCK 0: END.

DEFINE PROCEDURE ''MINUS'' [A,B]:
BLOCK 0: BEGIN
  IF A < B, THEN:
    QUIT BLOCK 0;
  LOOP AT MOST A TIMES:
  BLOCK 1: BEGIN
    IF OUTPUT + B = A, THEN:
      QUIT BLOCK 0;
    OUTPUT <= OUTPUT + 1;
  BLOCK 1: END;
BLOCK 0: END.

DEFINE PROCEDURE ''MODULUS'' [A,B]:
BLOCK 0: BEGIN
  CELL(0) <= DIVIDE[A,B];
  OUTPUT <= MINUS[A,CELL(0) * B];
BLOCK 0: END.



DEFINE PROCEDURE ''TOGGLE'' [DOOR]:
BLOCK 0: BEGIN
  IF DOOR = 1, THEN:
    QUIT BLOCK 0;
  OUTPUT <= 1;
BLOCK 0: END.

DEFINE PROCEDURE ''NUMBERS'' [DOOR, COUNT]:
BLOCK 0: BEGIN
  CELL(0) <= 1; /*each number*/
  OUTPUT <= 0; /*current door state*/

  LOOP COUNT TIMES:
  BLOCK 1: BEGIN

    IF MODULUS[DOOR, CELL(0)] = 0, THEN:
      OUTPUT <= TOGGLE[OUTPUT];

    CELL(0) <= CELL(0) + 1;

  BLOCK 1: END;

BLOCK 0: END.

DEFINE PROCEDURE ''DOORS'' [COUNT]:
BLOCK 0: BEGIN

  CELL(0) <= 1; /*each door*/
  LOOP COUNT TIMES:
  BLOCK 1: BEGIN

    CELL(1) <= NUMBERS[CELL(0), COUNT];  /*iterate over the states of this door to get its final state*/
    IF CELL(1) = 1, THEN: /*door state = open*/
      PRINT[CELL(0), '   '];

    CELL(0) <= CELL(0) + 1;

  BLOCK 1: END;
BLOCK 0: END.

DOORS[100];

```


{{out}}

```txt

 > 1
 > 4
 > 9
 > 16
 > 25
 > 36
 > 49
 > 64
 > 81
 > 100

```



## Bracmat

Bracmat is not really at home in tasks that involve addressing things by index number. Here are four solutions that each do the task, but none should win a price for cleanliness.

Solution 1. Use an indexable array. Local variables are stored in stacks. Each stack corresponds to one variable name and vice versa. Stacks can also be used as arrays, but because of how local variables are implemented, arrays cannot be declared as local variables.

```bracmat
( 100doors-tbl
=   door step
  .   tbl$(doors.101) { Create an array. Indexing is 0-based. Add one extra for addressing element nr. 100 }
    & 0:?step
    &   whl
      ' ( 1+!step:~>100:?step   { ~> means 'not greater than', i.e. 'less than or equal' }
        & 0:?door
        &   whl
          ' ( !step+!door:~>100:?door
            & 1+-1*!(!door$doors):?doors  { <number>$<variable> sets the current index, which stays the same until explicitly changed. }
            )
        )
    & 0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        &   out
          $ ( door
              !door
              is
              ( !(!door$doors):1&open
              | closed
              )
            )
        )
    & tbl$(doors.0)  { clean up the array }
)
```


Solution 2. Use one variable for each door. In Bracmat, a variable name can be any non-empty string, even a number, so we use the numbers 1 .. 100 as variable names, but also as door numbers. When used as variable an extra level of indirection is needed. See the occurrences of <code>?!</code> and <code>!!</code> in the following code.

```bracmat
( 100doors-var
=   step door
  .   0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        & closed:?!door { this creates a variable and assigns a value 'closed' to it }
        )
    & 0:?step
    &   whl
      ' ( 1+!step:~>100:?step
        & 0:?door
        &   whl
          ' ( !step+!door:~>100:?door
            &   ( !!door:closed&open
                | closed
                )
              : ?!door
            )
        )
    & 0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        & out$(door !door is !!door)
        )
    & 0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        & tbl$(!door.0)         { cleanup the variable }
        )
)
```


Solution 3. Use a list and a dedicated positioning pattern to address the right door in the list. Create a new list by concatenating the skipped elements with the toggled elements. This solution is computationally unfavourable because of the many concatenations.

```bracmat
( 100doors-list
=   doors door doorIndex step
  .   :?doors
    & 0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        & closed !doors:?doors
        )
    & 0:?skip
    &   whl
      ' ( :?ndoors
        &   whl
          ' ( !doors:?skipped [!skip %?door ?doors  { the [<number> pattern only succeeds when the scanning cursor is at position <number> }
            &     !ndoors
                  !skipped
                  ( !door:open&closed
                  | open
                  )
              : ?ndoors
            )
        & !ndoors !doors:?doors
        & 1+!skip:<100:?skip
        )
    & out$!doors
)
```


Solution 4. Use a list of objects. Each object can be changed without the need to re-create the whole list.

```bracmat
( 100doors-obj
=   doors door doorIndex step
  .   :?doors
    & 0:?door
    &   whl
      ' ( 1+!door:~>100:?door
        & new$(=closed) !doors:?doors
        )
    & 0:?skip
    &   whl
      ' ( !doors:?tododoors
        &   whl
          ' ( !tododoors:? [!skip %?door ?tododoors
            &   ( !(door.):open&closed
                | open
                )
              : ?(door.)
            )
        & 1+!skip:<100:?skip
        )
    & out$!doors
)
```


These four functions are called in the following way:

```bracmat
100doors-tbl$
& 100doors-var$
& 100doors-list$
& 100doors-obj$;
```



## Burlesque


Version using square numbers:


```burlesque
blsq ) 10ro2?^
{1 4 9 16 25 36 49 64 81 100}
```


## C

### Unoptimized

```c
#include <stdio.h>

int main()
{
  char is_open[100] = { 0 };
  int pass, door;

  /* do the 100 passes */
  for (pass = 0; pass < 100; ++pass)
    for (door = pass; door < 100; door += pass+1)
      is_open[door] = !is_open[door];

  /* output the result */
  for (door = 0; door < 100; ++door)
    printf("door #%d is %s.\n", door+1, (is_open[door]? "open" : "closed"));

  return 0;
}
```


Using defensive programming, pointers, sentinel values and some other standard programming practices,
{{uses from|Library|C Runtime|component1=printf}}

```c
#include <stdio.h>

#define NUM_DOORS 100

int main(int argc, char *argv[])
{
  int is_open[NUM_DOORS] = { 0 } ;
  int * doorptr, * doorlimit = is_open + NUM_DOORS ;
  int pass ;

  /* do the N passes, go backwards because the order is not important */
  for ( pass= NUM_DOORS ; ( pass ) ; -- pass ) {
    for ( doorptr= is_open + ( pass-1 ); ( doorptr < doorlimit ) ; doorptr += pass ) {
      ( * doorptr ) ^= 1 ;
    }
  }

  /* output results */
  for ( doorptr= is_open ; ( doorptr != doorlimit ) ; ++ doorptr ) {
    printf("door #%ld is %s\n", ( doorptr - is_open ) + 1, ( * doorptr ) ? "open" : "closed" ) ;
  }
}
```



### optimized

This optimized version makes use of the fact that finally only the doors with square index are open, as well as the fact that <math>n^2 = 1 + 3 + 5 + \ldots + (2n-1)</math>.

```c
#include <stdio.h>

int main()
{
  int square = 1, increment = 3, door;
  for (door = 1; door <= 100; ++door)
  {
    printf("door #%d", door);
    if (door == square)
    {
      printf(" is open.\n");
      square += increment;
      increment += 2;
    }
    else
      printf(" is closed.\n");
  }
  return 0;
}
```


The following ultra-short optimized version demonstrates the flexibility of C loops, but isn't really considered good C style:


```c
#include <stdio.h>


int main()
{
  int door, square, increment;
  for (door = 1, square = 1, increment = 1; door <= 100; door++ == square && (square += increment += 2))
    printf("door #%d is %s.\n", door, (door == square? "open" : "closed"));
  return 0;
}
```


Or really optimize it -- square of an integer is, well, computable:
```C
#include <stdio.h>


int main()
{
	int i;
	for (i = 1; i * i <= 100; i++)
		printf("door %d open\n", i * i);

	return 0;
}
```



## C++

{{works with|GCC|4.1.2 20061115 (prerelease) (SUSE Linux)}}

'''unoptimized '''

```cpp
#include <iostream>

int main()
{
  bool is_open[100] = { false };

  // do the 100 passes
  for (int pass = 0; pass < 100; ++pass)
    for (int door = pass; door < 100; door += pass+1)
      is_open[door] = !is_open[door];

  // output the result
  for (int door = 0; door < 100; ++door)
    std::cout << "door #" << door+1 << (is_open[door]? " is open." : " is closed.") << std::endl;
  return 0;
}
```


'''optimized '''
This optimized version makes use of the fact that finally only the doors with square index are open, as well as the fact that <math>(n+1)^2 = 1 + 3 + 5 + \ldots + (2n+1)</math>.


```cpp
#include <iostream>

int main()
{
  int square = 1, increment = 3;
  for (int door = 1; door <= 100; ++door)
  {
    std::cout << "door #" << door;
    if (door == square)
    {
      std::cout << " is open." << std::endl;
      square += increment;
      increment += 2;
    }
    else
      std::cout << " is closed." << std::endl;
  }
  return 0;
}
```


The only calculation that's really needed:

```cpp
#include <iostream>
//compiled with "Dev-C++" , from RaptorOne

int main()
{
    for(int i=1; i*i<=100; i++)
            std::cout<<"Door "<<i*i<<" is open!"<<std::endl;
}
```


Compile time computation using C++17 to produce fastest runtime.

```cpp
#include <iostream>
// compiled with clang (tags/RELEASE_600/final)
#include <type_traits> // or g++ (GCC) 7.3.1 20180406 -- from hare1039
namespace functional_list // basic building block for template meta programming
{
struct NIL
{
	using head = NIL;
	using tail = NIL;
	friend std::ostream& operator << (std::ostream& os, NIL const) { return os; }
};

template <typename H, typename T = NIL>
struct list
{
	using head = H;
	using tail = T;
};

template <int i>
struct integer
{
	static constexpr int value = i;
	friend std::ostream& operator << (std::ostream& os, integer<i> const) { os << integer<i>::value; return os;}
};

template <typename L, int nTH> constexpr
auto at()
{
	if constexpr (nTH == 0)
		return (typename L::head){};
	else if constexpr (not std::is_same_v<typename L::tail, NIL>)
		return at<typename L::tail, nTH - 1>();
	else
		return NIL{};
}
template <typename L, int nTH>
using at_t = decltype(at<L, nTH>());

template <typename L, typename elem> constexpr
auto prepend() { return list<elem, L>{}; }

template <typename L, typename elem>
using prepend_t = decltype(prepend<L, elem>());

template <int Size, typename Dat = integer<0>> constexpr
auto gen_list()
{
	if constexpr (Size == 0)
		return NIL{};
	else
	{
		using next = decltype(gen_list<Size - 1, Dat>());
		return prepend<next, Dat>();
	}
}
template <int Size, typename Dat = integer<0>>
using gen_list_t = decltype(gen_list<Size, Dat>());

} namespace fl = functional_list;

constexpr int door_amount = 101; // index from 1 to 100

template <typename L, int current, int moder> constexpr
auto construct_loop()
{
	using val_t = fl::at_t<L, current>;
	if constexpr (std::is_same_v<val_t, fl::NIL>)
		return fl::NIL{};
	else
	{
		constexpr int val = val_t::value;
		using val_add_t = fl::integer<val + 1>;
		using val_old_t = fl::integer<val>;

		if constexpr (current == door_amount)
		{
			if constexpr(current % moder == 0)
				return fl::list<val_add_t>{};
			else
				return fl::list<val_old_t>{};
		}
		else
		{
			using sub_list = decltype(construct_loop<L, current + 1, moder>());
			if constexpr(current % moder == 0)
				return fl::prepend<sub_list, val_add_t>();
			else
				return fl::prepend<sub_list, val_old_t>();
		}
	}
}

template <int iteration> constexpr
auto construct()
{
	if constexpr (iteration == 1) // door index = 1
	{
		using l = fl::gen_list_t<door_amount>;
		return construct_loop<l, 0, iteration>();
	}
	else
	{
		using prev_iter_list = decltype(construct<iteration - 1>());
		return construct_loop<prev_iter_list, 0, iteration>();
	}
}

template <typename L, int pos> constexpr
void show_ans()
{
	if constexpr (std::is_same_v<typename L::head, fl::NIL>)
		return;
	else
	{
		if constexpr (L::head::value % 2 == 1)
			std::cout << "Door " << pos << " is opened.\n";
		show_ans<typename L::tail, pos + 1>();
	}
}

int main()
{
	using result = decltype(construct<100>());
	show_ans<result, 0>();
}
```


## C#

### Unoptimized with Modulus % Operator

```c#
namespace ConsoleApplication1
{
    using System;
    class Program
    {
        static void Main(string[] args)
        {
            bool[] doors = new bool[100];

            //Close all doors to start.
            for (int d = 0; d < 100; d++) doors[d] = false;

            //For each pass...
            for (int p = 0; p < 100; p++)//number of passes
            {
                //For each door to toggle...
                for (int d = 0; d < 100; d++)//door number
                {
                    if ((d + 1) % (p + 1) == 0)
                    {
                        doors[d] = !doors[d];
                    }
                }
            }

            //Output the results.
            Console.WriteLine("Passes Completed!!!  Here are the results: \r\n");
            for (int d = 0; d < 100; d++)
            {
                if (doors[d])
                {
                    Console.WriteLine(String.Format("Door #{0}: Open", d + 1));
                }
                else
                {
                    Console.WriteLine(String.Format("Door #{0}: Closed", d + 1));
                }
            }
            Console.ReadKey(true);
        }
    }
}
```



### Optimized for Increments


```c#
namespace ConsoleApplication1
{
    using System;
    class Program
    {
        static void Main()
        {
            //The o variable stores the number of the next OPEN door.
            int o = 1;
            int f = 1;
            int l = 5;
            Random r = new Random();
            o = r.Next(f, l);

            //The d variable determines the door to be output next.
            for (int d = 1; d <= 100; d++)
            {
                Console.Write("Door #{0}: ", d);
                if (d == o)
                {
                    Console.WriteLine("Open");
                    f = f + 5;
                    l = l + 5;
                    o = r.Next(f, l);
                }
                else
                    Console.WriteLine("Closed");
            }
            Console.ReadKey(true);
        }
    }
}
```



### Optimized for Orthogonality

(This version demonstrates a different thought pattern during development, where operation and presentation are separated. It could easily be refactored so that the operations to determine which doors are opened and to display the list of doors would be in separate methods, at which point it would become simple to extract them to separate classes and employ a DI pattern to switch the algorithm or display mechanism being used. It also keeps the calculation clear and concise.)

```c#
namespace ConsoleApplication1
{
    using System;
    class Program
    {
        static void Main(string[] args)
        {
            //Perform the operation.
            bool[] doors = new bool[100];
            int n = 0;
            int d;
            while ((d = (++n * n)) <= 100)
                doors[d - 1] = true;

            //Perform the presentation.
            for (d = 0; d < doors.Length; d++)
                Console.WriteLine("Door #{0}: {1}", d + 1, doors[d] ? "Open" : "Closed");
            Console.ReadKey(true);
        }
    }
}
```



### Unoptimized but Concise


```c#
namespace ConsoleApplication1
{
    using System;
    class Program
    {
        static void Main()
        {
            bool[] doors = new bool[100];

            //The number of passes can be 1-based, but the number of doors must be 0-based.
            for (int p = 1; p <= 100; p++)
                for (int d = p - 1; d < 100; d += p)
                    doors[d] = !doors[d];
            for (int d = 0; d < 100; d++)
                Console.WriteLine("Door #{0}: {1}", d + 1, doors[d] ? "Open" : "Closed");
            Console.ReadKey(true);
        }
    }
}
```



### Optimized for brevity


```c#
namespace ConsoleApplication1
{
    using System;
    class Program
    {
        static void Main()
        {
            double n;

            //If the current door number is the perfect square of an integer, say it is open, else say it is closed.
            for (int d = 1; d <= 100; d++)
                Console.WriteLine("Door #{0}: {1}", d, (n = Math.Sqrt(d)) == (int)n ? "Open" : "Closed");
            Console.ReadKey(true);
        }
    }
}
```



## Ceylon


```ceylon
shared void run() {
    print("Open doors (naive):     ``naive()``
           Open doors (optimized): ``optimized()``");

}

shared {Integer*} naive(Integer count = 100) {
    variable value doors = [ for (_ in 1..count) closed ];
    for (step in 1..count) {
        doors = [for (i->door in doors.indexed) let (index = i+1) if (step == 1 || step.divides(index)) then door.toggle() else door ];
    }
    return doors.indexesWhere((door) => door == opened).map(1.plusInteger);
}

shared {Integer*} optimized(Integer count = 100) =>
        { for (i in 1..count) i*i }.takeWhile(count.notSmallerThan);


shared abstract class Door(shared actual String string) of opened | closed {
    shared formal Door toggle();
}
object opened extends Door("opened") { toggle() => closed; }
object closed extends Door("closed") { toggle() => opened; }
```


'''Output:'''

```txt
Open doors (naive):     { 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 }
Open doors (optimized): { 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 }
```



## C1R


```c
100_doors
```


=={{header|Caché ObjectScript}}==
<lang>
 for i=1:1:100 {
	 set doors(i) = 0
 }
 for i=1:1:100 {
	 for door=i:i:100 {
		  Set doors(door)='doors(door)
	 }
 }
 for i = 1:1:100
 {
	if doors(i)=1 write i_": open",!
 }

```

Output:
<lang>
1: open
4: open
9: open
16: open
25: open
36: open
49: open
64: open
81: open
100: open

```



## Clarion


```clarion

    program

    map
    end

MAX_DOOR_NUMBER         equate(100)
CRLF                    equate('<13,10>')

Doors                   byte,dim(MAX_DOOR_NUMBER)
Pass                    byte
DoorNumber              byte
DisplayString           cstring(2000)

ResultWindow            window('Result'),at(,,133,291),center,double,auto
                            prompt('Door states:'),at(8,4),use(?PromptTitle)
                            text,at(8,16,116,266),use(DisplayString),boxed,vscroll,font('Courier New',,,,CHARSET:ANSI),readonly
                        end

    code

    Doors :=: false
    loop Pass = 1 to MAX_DOOR_NUMBER
        loop DoorNumber = Pass to MAX_DOOR_NUMBER by Pass
            Doors[DoorNumber] = choose(Doors[DoorNumber], false, true)
        end
    end

    clear(DisplayString)
    loop DoorNumber = 1 to MAX_DOOR_NUMBER
        DisplayString = DisplayString & format(DoorNumber, @n3) & ' is ' & choose(Doors[DoorNumber], 'opened', 'closed') & CRLF
    end
    open(ResultWindow)
    accept
    end
    close(ResultWindow)

    return

```



## Clio


'''Unoptimized'''


```clio
fn visit-doors doors step:
  if step > 100: doors
  else:
    [1:100]
      -> * fn index:
            if index % step: doors[(index - 1)]
            else: not doors[(index - 1)]
      -> visit-doors (step + 1)

[1:100] -> * n: false -> visit-doors 1 => doors
[1:100] -> * (@eager) fn i:
  doors[(i - 1)]
    -> if = true: #open
            else: #closed
    -> print #Door i #is @
```


'''Optimized'''


```clio
[1:100] -> * (@eager) fn i:
  i ^ 0.5
    -> eq @ (transform i: floor)
    -> if = true: #open
            else: #closed
    -> print #Door i #is @
```



## CLIPS


'''Unoptimized'''


```clips
(deffacts initial-state
  (door-count 100)
)

(deffunction toggle
  (?state)
  (switch ?state
    (case "open" then "closed")
    (case "closed" then "open")
  )
)

(defrule create-doors-and-visits
  (door-count ?count)
  =>
  (loop-for-count (?num 1 ?count) do
    (assert (door ?num "closed"))
    (assert (visit-from ?num ?num))
  )
  (assert (doors initialized))
)

(defrule visit
  (door-count ?max)
  ?visit <- (visit-from ?num ?step)
  ?door <- (door ?num ?state)
  =>
  (retract ?visit)
  (retract ?door)
  (assert (door ?num (toggle ?state)))
  (if
    (<= (+ ?num ?step) ?max)
    then
    (assert (visit-from (+ ?num ?step) ?step))
  )
)

(defrule start-printing
  (doors initialized)
  (not (visit-from ? ?))
  =>
  (printout t "These doors are open:" crlf)
  (assert (print-from 1))
)

(defrule print-door
  (door-count ?max)
  ?pf <- (print-from ?num)
  (door ?num ?state)
  =>
  (retract ?pf)
  (if
    (= 0 (str-compare "open" ?state))
    then
    (printout t ?num " ")
  )
  (if
    (< ?num ?max)
    then
    (assert (print-from (+ ?num 1)))
    else
    (printout t crlf "All other doors are closed." crlf)
  )
)
```


'''Optimized'''


```clips
(deffacts initial-state
  (door-count 100)
)

(deffunction is-square
  (?num)
  (= (sqrt ?num) (integer (sqrt ?num)))
)

(defrule check-doors
  (door-count ?count)
  =>
  (printout t "These doors are open:" crlf)
  (loop-for-count (?num 1 ?count) do
    (if (is-square ?num) then
      (printout t ?num " ")
    )
  )
  (printout t crlf "All other doors are closed." crlf)
)
```



## Clojure

'''Unoptimized / mutable array'''

```clojure
(defn doors []
  (let [doors (into-array (repeat 100 false))]
    (doseq [pass   (range 1 101)
            i      (range (dec pass) 100 pass) ]
      (aset doors i (not (aget doors i))))
    doors))

(defn open-doors [] (for [[d n] (map vector (doors) (iterate inc 1)) :when d] n))

(defn print-open-doors []
  (println
    "Open doors after 100 passes:"
    (apply str (interpose ", " (open-doors)))))
```


'''Unoptimized / functional '''

```clojure
(defn doors []
  (reduce (fn [doors toggle-idx] (update-in doors [toggle-idx] not))
          (into [] (repeat 100 false))
          (for [pass   (range 1 101)
                i      (range (dec pass) 100 pass) ]
            i)))

(defn open-doors [] (for [[d n] (map vector (doors) (iterate inc 1)) :when d] n))

(defn print-open-doors []
  (println
    "Open doors after 100 passes:"
    (apply str (interpose ", " (open-doors)))))
```


'''Alternative Unoptimized / functional '''

```clojure
(defn open-doors []
  (->> (for [step (range 1 101), occ (range step 101 step)] occ)
       frequencies
       (filter (comp odd? val))
       keys
       sort))

(defn print-open-doors []
  (println
    "Open doors after 100 passes:"
    (apply str (interpose ", " (open-doors)))))
```


'''Optimized / functional'''

```clojure
(defn doors []
	(reduce (fn [doors idx] (assoc doors idx true))
	        (into [] (repeat 100 false))
	        (map #(dec (* % %)) (range 1 11))))

(defn open-doors [] (for [[d n] (map vector (doors) (iterate inc 1)) :when d] n))

(defn print-open-doors []
  (println
    "Open doors after 100 passes:"
    (apply str (interpose ", " (open-doors)))))
```



'''Alternative Optimized / functional'''

```clojure
(defn open-doors [] (->> (iterate inc 1) (map #(* % %)) (take-while #(<= % 100))))

(defn print-open-doors []
  (println
    "Open doors after 100 passes:"
    (apply str (interpose ", " (open-doors)))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 100Doors.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Current-n      PIC 9(3).
       01 StepSize       PIC 9(3).
       01 DoorTable.
          02 Doors       PIC 9(1)   OCCURS 100 TIMES.
             88 ClosedDoor          VALUE ZERO.
       01 Idx            PIC 9(3).

       PROCEDURE DIVISION.
       Begin.
           INITIALIZE DoorTable
           PERFORM VARYING StepSize FROM 1 BY 1 UNTIL StepSize > 100
             PERFORM VARYING Current-n FROM StepSize BY StepSize
                     UNTIL Current-n > 100
               SUBTRACT Doors (Current-n) FROM 1 GIVING Doors (Current-n)
             END-PERFORM
           END-PERFORM

           PERFORM VARYING Idx FROM 1 BY 1
                   UNTIL Idx > 100
             IF ClosedDoor (Idx)
               DISPLAY Idx " is closed."
             ELSE
               DISPLAY Idx " is open."
             END-IF
           END-PERFORM

           STOP RUN
           .
```



## Coco


We use the naive algorithm.


```coco
doors = [false] * 100

for pass til doors.length
    for i from pass til doors.length by pass + 1
        ! = doors[i]

for i til doors.length
    console.log 'Door %d is %s.', i + 1, if doors[i] then 'open' else 'closed'
```



## CoffeeScript

'''unoptimized''':

```coffeescript
doors = []

for pass in [1..100]
  for i in [pass..100] by pass
    doors[i] = !doors[i]

console.log "Doors #{index for index, open of doors when open} are open"

# matrix output
console.log doors.map (open) -> +open

```


'''optimized''':


```coffeescript
isInteger = (i) -> Math.floor(i) == i

console.log door for door in [1..100] when isInteger Math.sqrt door
```


'''ultra-optimized''':

```coffeescript
console.log Math.pow(i,2) for i in [1..10]
```



## ColdFusion

'''Basic Solution: Returns List of 100 values: 1=open 0=closed'''

```coldfusion

	doorCount = 1;
	doorList = "";
	// create all doors and set all doors to open
	while (doorCount LTE 100) {
		doorList = ListAppend(doorList,"1");
		doorCount = doorCount + 1;
	}
	loopCount = 2;
	doorListLen = ListLen(doorList);
	while (loopCount LTE 100) {
		loopDoorListCount = 1;
		while (loopDoorListCount LTE 100) {
			testDoor = loopDoorListCount / loopCount;
			if (testDoor EQ Int(testDoor)) {
				checkOpen = ListGetAt(doorList,loopDoorListCount);
				if (checkOpen EQ 1) {
					doorList = ListSetAt(doorList,loopDoorListCount,"0");
				} else {
					doorList = ListSetAt(doorList,loopDoorListCount,"1");
				}
			}
			loopDoorListCount = loopDoorListCount + 1;
		}
		loopCount = loopCount + 1;
	}

```


'''Squares of Integers Solution: Returns List of 100 values: 1=open 0=closed'''

```coldfusion

	doorCount = 1;
	doorList = "";
	loopCount = 1;
	while (loopCount LTE 100) {
		if (Sqr(loopCount) NEQ Int(Sqr(loopCount))) {
			doorList = ListAppend(doorList,0);
		} else {
			doorList = ListAppend(doorList,1);
		}
		loopCount = loopCount + 1;
	}

```


'''Display only'''

```cfm
// Display all doors
<cfloop from="1" to="100" index="x">
    Door #x# Open: #YesNoFormat(ListGetAt(doorList,x))#<br />
</cfloop>

// Output only open doors
<cfloop from="1" to="100" index="x">
    <cfif ListGetAt(doorList,x) EQ 1>
        #x#<br />
    </cfif>
</cfloop>
```


'''Another Route'''

```cfm
<Cfparam name="doorlist" default="">
<cfloop from="1" to="100" index="i">
    <Cfset doorlist = doorlist & 'c,'>
</cfloop>
<cfloop from="1" to="100" index="i">
    <Cfloop from="1" to="100" index="door" step="#i#">
    <Cfif listgetat(doorlist, door) eq 'c'>
        <Cfset doorlist = listsetat(doorlist, door, 'O')>
    <Cfelse>
        <Cfset doorlist = listsetat(doorlist, door, 'c')>
    </Cfif>
    </Cfloop>
</cfloop>
<Cfoutput>#doorlist#</Cfoutput>
```



## Commodore BASIC


```basic

10 D=100: DIMD(D): P=1
20 PRINT CHR$(147);"PASS: ";P
22 FOR I=P TO D STEP P: D(I)=NOTD(I): NEXT
30 IF P=100 THEN 40
32 P=P+1: GOTO20
40 PRINT: PRINT"THE FOLLOWING DOORS ARE OPEN: "
42 FOR I=1 TO D: IF D(I)=-1 THEN PRINTI;
44 NEXT

```



## Common Lisp


'''Unoptimized / functional '''
This is a very unoptimized version of the problem,
using recursion and quite considerable list-copying.
It emphasizes the functional way of solving this problem.


```lisp
(defun visit-door (doors doornum value1 value2)
  "visits a door, swapping the value1 to value2 or vice-versa"
  (let ((d (copy-list doors))
        (n (- doornum 1)))
    (if (eql  (nth n d) value1)
        (setf (nth n d) value2)
      (setf (nth n d) value1))
    d))

(defun visit-every (doors num iter value1 value2)
  "visits every 'num' door in the list"
  (if (> (* iter num) (length doors))
      doors
    (visit-every (visit-door doors (* num iter) value1 value2)
                 num
                 (+ 1 iter)
                 value1
                 value2)))

(defun do-all-visits (doors cnt value1 value2)
  "Visits all doors changing the values accordingly"
  (if (< cnt 1)
      doors
    (do-all-visits (visit-every doors cnt 1 value1 value2)
                   (- cnt 1)
                   value1
                   value2)))

(defun print-doors (doors)
  "Pretty prints the doors list"
  (format T "~{~A ~A ~A ~A ~A ~A ~A ~A ~A ~A~%~}~%" doors))

(defun start (&optional (size 100))
  "Start the program"
  (let* ((open "_")
         (shut "#")
         (doors (make-list size :initial-element shut)))
    (print-doors (do-all-visits doors size open shut))))
```


'''Unoptimized, imperative '''
This is a version that closely follows the problem description and is still quite short.  Of all the presented solutions it might be closest to "idiomatic Common Lisp".


```lisp
(define-modify-macro toggle () not)

(defun 100-doors ()
  (let ((doors (make-array 100)))
    (dotimes (i 100)
      (loop for j from i below 100 by (1+ i)
	 do (toggle (svref doors j))))
    (dotimes (i 100)
      (format t "door ~a: ~:[closed~;open~]~%" (1+ i) (svref doors i)))))
```


'''Unoptimized, ''' n-doors.

```lisp
(defun doors (z &optional (w (make-list z)) (n 1))
  (if (> n z) w (doors z (toggle w n z) (1+ n))))

(defun toggle (w m z)
  (loop for a in w for n from 1 to z
        collect (if (zerop (mod n m)) (not a) a)))

> (doors 100)
(T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
 NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
 NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
 NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T
 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)
```


'''Optimized, ''' n-doors.

```lisp
(defun doors (n)
  (loop for a from 1 to n collect
        (zerop (mod (sqrt a) 1))))

> (doors 100)
(T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
 NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
 NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
 NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T
 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)
```


'''Optimized '''
This is an optimized version, using the perfect square algorithm.


```lisp
(defun 100-doors ()
  (let ((doors (make-array 100)))
    (dotimes (i 10)
      (setf (svref doors (* i i)) t))
    (dotimes (i 100)
      (format t "door ~a: ~:[closed~;open~]~%" (1+ i) (svref doors i)))))
```


'''Optimized 2'''
Another optimized version, with finer granular separation of functionality (might be a bit excessive).


```lisp
(defun perfect-square-list (n)
  "Generates a list of perfect squares from 0 up to n"
  (loop for i from 1 to (isqrt n) collect (expt i 2)))

(defun print-doors (doors)
  "Pretty prints the doors list"
  (format T "~{~A ~A ~A ~A ~A ~A ~A ~A ~A ~A~%~}~%" doors))

(defun open-door (doors num open)
  "Sets door at num to open"
  (setf (nth (- num 1) doors) open))

(defun visit-all (doors vlist open)
  "Visits and opens all the doors indicated in vlist"
  (dolist (dn vlist doors)
    (open-door doors dn open)))

(defun start2 (&optional (size 100))
  "Start the program"
  (print-doors
   (visit-all (make-list size :initial-element '\#)
              (perfect-square-list size)
              '_)))
```


'''Optimized (2) '''
This version displays a much more functional solution through the use of MAPCAR.


```lisp
(let  ((i 0))
    (mapcar (lambda (x)
                (if (zerop (mod (sqrt (incf i)) 1))
                    "_" "#"))
            (make-list 100)))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Doors100;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	i,j: INTEGER;
	closed: ARRAY 101 OF BOOLEAN;
BEGIN
	(* initialization of closed to true *)
	FOR i := 0 TO LEN(closed) - 1 DO closed[i] := TRUE END;
	(* process *)
	FOR i := 1 TO LEN(closed)  DO;
		j := 1;
		WHILE j < LEN(closed) DO
			IF j MOD i = 0 THEN closed[j] := ~closed[j] END;INC(j)
		END
	END;
	(* print results *)
	i := 1;
	WHILE  i < LEN(closed)  DO
		IF (i - 1) MOD 10 = 0 THEN StdLog.Ln END;
		IF closed[i] THEN StdLog.String("C ") ELSE StdLog.String("O ") END;
		INC(i)
	END;
END Do;
END Doors100.


```

Execute: ^Q Doors100.Do<br/>
{{out}}

```txt

O C C O C C C C O C
C C C C C O C C C C
C C C C O C C C C C
C C C C C O C C C C
C C C C C C C C O C
C C C C C C C C C C
C C C O C C C C C C
C C C C C C C C C C
O C C C C C C C C C
C C C C C C C C C O

```



## Coq

Basic solution:

```coq
Require Import List.

Fixpoint rep {A} (a : A) n :=
  match n with
    | O => nil
    | S n' => a::(rep a n')
  end.

Fixpoint flip (l : list bool) (n k : nat) : list bool :=
  match l with
    | nil => nil
    | cons h t => match k with
                | O => (negb h) :: (flip t n n)
                | S k' => h :: (flip t n k')
              end
  end.

Definition flipeach l n := flip l n n.

Fixpoint flipwhile l n :=
  match n with
    | O => flipeach l 0
    | S n' => flipwhile (flipeach l (S n')) n'
  end.

Definition prison cells := flipwhile (rep false cells) cells.
```


Optimized version ((n+1)^2 = n^2 + 2n + 1):

```coq
Require Import List.

Fixpoint prisoo' nd n k accu :=
  match nd with
    | O => rev accu
    | S nd' => let ra := match k with
                 | O => (true, S n, (n + n))
                 | S k' => (false, n, k')
               end in
               prisoo' nd' (snd (fst ra)) (snd ra) ((fst (fst ra))::accu)
  end.

Definition prisoo cells := prisoo' cells 1 0 nil.
```


Unit test:

```coq>Goal prison 100 = prisoo 100. compute. reflexivity. Qed.</lang


Full proof at [https://github.com/spanjel/rosetta github]:

```coq
Goal forall n, prison n = prisoo n. Abort.
```



## Crystal


```ruby
doors = Array.new(100, false)

1.upto(100) do |i|
  i.step(by: i, limit: 100) do |j|
    doors[j - 1] = !doors[j - 1]
  end
end

doors.each_with_index do |open, i|
  puts "Door #{i + 1} is #{open ? "open" : "closed"}"
end
```



## D



```d
import std.stdio, std.algorithm, std.range;

enum DoorState : bool { closed, open }
alias Doors = DoorState[];

Doors flipUnoptimized(Doors doors) pure nothrow {
    doors[] = DoorState.closed;

    foreach (immutable i; 0 .. doors.length)
        for (ulong j = i; j < doors.length; j += i + 1)
            if (doors[j] == DoorState.open)
                doors[j] = DoorState.closed;
            else
                doors[j] = DoorState.open;
    return doors;
}

Doors flipOptimized(Doors doors) pure nothrow {
    doors[] = DoorState.closed;
    for (int i = 1; i ^^ 2 <= doors.length; i++)
        doors[i ^^ 2 - 1] = DoorState.open;
    return doors;
}

void main() {
    auto doors = new Doors(100);

    foreach (const open; [doors.dup.flipUnoptimized,
                          doors.dup.flipOptimized])
        iota(1, open.length + 1).filter!(i => open[i - 1]).writeln;
}
```

{{out}}

```txt
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```


'''Unoptimized. Demonstrates very basic language syntax/features.
Program output allows to see what the code is doing:'''

```d

import std.stdio;

void printAllDoors(bool[] doors)
{
   // Prints the state of all the doors
   foreach(i, door; doors)
   {
      writeln("#: ", i + 1, (door) ? " open" : " closed");
      }
}
void main()
{
   bool[100] doors = false;   //Create 100 closed doors
   for(int a = 0; a < 100; ++a) {
      writefln("Pass #%s; visiting every %s door.", a + 1, a + 1);  // Optional
	 for(int i = a; i < 100; i += (a + 1)) {
	 writefln("Visited door %s", i + 1);  //Optional
	 doors[i] = !doors[i];
	 }
      writeln();  // Optional
      }
   printAllDoors(doors);   // Prints the state of each door
}

```



## Dafny


The InitializeDoors function demonstrates some of Dafny's advanced features.


```dafny

datatype Door = Closed | Open

method InitializeDoors(n:int) returns (doors:array<Door>)
  // Precondition: n must be a valid array size.
  requires n >= 0
  // Postcondition: doors is an array, which is not an alias for any other
  // object, with a length of n, all of whose elements are Closed. The "fresh"
  // (non-alias) condition is needed to allow doors to be modified by the
  // remaining code.
  ensures doors != null && fresh(doors) && doors.Length == n
  ensures forall j :: 0 <= j < doors.Length ==> doors[j] == Closed;
{
  doors := new Door[n];
  var i := 0;
  // Invariant: i is always a valid index inside the loop, and all doors less
  // than i are Closed. These invariants are needed to ensure the second
  // postcondition.
  while i < doors.Length
    invariant i <= doors.Length
    invariant forall j :: 0 <= j < i ==> doors[j] == Closed;
  {
    doors[i] := Closed;
    i := i + 1;
  }
}

method Main ()
{
  var doors := InitializeDoors(100);

  var pass := 1;
  while pass <= doors.Length
  {
    var door := pass;
    while door < doors.Length
    {
      doors[door] := if doors[door] == Closed then Open else Closed;
      door := door + pass;
    }
    pass := pass + 1;
  }
  var i := 0;
  while i < doors.Length
  {
    print i, " is ", if doors[i] == Closed then "closed\n" else "open\n";
    i := i + 1;
  }
}

```



## Dart

'''unoptimized'''

```dart
main() {
    for (var k = 1, x = new List(101); k <= 100; k++) {
        for (int i = k; i <= 100; i += k)
            x[i] = !x[i];
        if (x[k]) print("$k open");
    }
}
```


'''optimized version''' (including generating squares without multiplication)

```dart
main() {
  for(int i=1,s=3;i<=100;i+=s,s+=2)
    print("door $i is open");
}
```


'''comprehensible (not "code golf") version for a pedestrian language'''

```dart
import 'dart:io';

final numDoors = 100;
final List<bool> doorClosed = List(numDoors);

String stateToString(String message) {
  var res = '';
  for (var i = 0; i < numDoors; i++) {
    res += (doorClosed[i] ? 'X' : '\u2610');
  }
  return res + " " + message;
}

main() {
  for (var i = 0; i < numDoors; i++) {
    doorClosed[i] = true;
  }
  stdout.writeln(stateToString("after initialization"));
  for (var step = 1; step <= numDoors; step++) {
    final start = step - 1;
    for (var i = start; i < numDoors; i += step) {
      doorClosed[i] = !doorClosed[i];
    }
    stdout.writeln(stateToString("after toggling with step = $step"));
  }
}
```


<pre style="font-size:80%">
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX after initialization
☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐ after toggling with step = 1
☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X☐X after toggling with step = 2
☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XXX after toggling with step = 3
☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐XX☐X☐XX☐ after toggling with step = 4
☐XX☐X☐☐☐X☐☐X☐X☐☐☐☐☐XXX☐XXXX☐☐X☐☐XXXX☐XXX☐☐☐☐☐X☐X☐☐X☐☐☐X☐XX☐☐☐XX☐X☐☐☐X☐☐X☐X☐☐☐☐☐XXX☐XXXX☐☐X☐☐XXXX☐XXX after toggling with step = 5
☐XX☐XX☐☐X☐☐☐☐X☐☐☐X☐XXX☐☐XXX☐☐☐☐☐XXX☐☐XXX☐X☐☐☐X☐☐☐☐X☐☐XX☐XX☐X☐XX☐XX☐☐X☐☐☐☐X☐☐☐X☐XXX☐☐XXX☐☐☐☐☐XXX☐☐XXX after toggling with step = 6
☐XX☐XXX☐X☐☐☐☐☐☐☐☐X☐X☐X☐☐XXXX☐☐☐☐XX☐☐☐XXX☐☐☐☐☐X☐☐X☐X☐☐XXXXX☐X☐X☐☐XX☐☐XX☐☐☐X☐☐XX☐XXX☐XXXX☐☐☐X☐XXX☐☐☐XX after toggling with step = 7
☐XX☐XXXXX☐☐☐☐☐☐X☐X☐X☐X☐XXXXX☐☐☐XXX☐☐☐XX☐☐☐☐☐☐X☐XX☐X☐☐XX☐XX☐X☐X☐XXX☐☐XX☐X☐X☐☐XX☐☐XX☐XXXXX☐☐X☐XXXX☐☐XX after toggling with step = 8
☐XX☐XXXX☐☐☐☐☐☐☐X☐☐☐X☐X☐XXX☐X☐☐☐XXX☐X☐XX☐☐☐☐☐XX☐XX☐X☐☐☐X☐XX☐X☐XXXXX☐☐XX☐☐☐X☐☐XX☐☐☐X☐XXXXX☐XX☐XXXX☐☐☐X after toggling with step = 9
☐XX☐XXXX☐X☐☐☐☐☐X☐☐☐☐☐X☐XXX☐X☐X☐XXX☐X☐XXX☐☐☐☐XX☐XXXX☐☐☐X☐XX☐☐☐XXXXX☐☐X☐☐☐☐X☐☐XX☐X☐X☐XXXXX☐☐X☐XXXX☐☐☐☐ after toggling with step = 10
☐XX☐XXXX☐XX☐☐☐☐X☐☐☐☐☐☐☐XXX☐X☐X☐X☐X☐X☐XXX☐☐☐XXX☐XXXX☐☐☐☐☐XX☐☐☐XXXX☐☐☐X☐☐☐☐X☐☐☐X☐X☐X☐XXXX☐☐☐X☐XXXX☐☐X☐ after toggling with step = 11
☐XX☐XXXX☐XXX☐☐☐X☐☐☐☐☐☐☐☐XX☐X☐X☐X☐X☐☐☐XXX☐☐☐XXX☐☐XXX☐☐☐☐☐XX☐X☐XXXX☐☐☐X☐☐X☐X☐☐☐X☐X☐X☐☐XXX☐☐☐X☐XXX☐☐☐X☐ after toggling with step = 12
☐XX☐XXXX☐XXXX☐☐X☐☐☐☐☐☐☐☐X☐☐X☐X☐X☐X☐☐☐X☐X☐☐☐XXX☐☐XXXX☐☐☐☐XX☐X☐XXX☐☐☐☐X☐☐X☐X☐☐☐☐☐X☐X☐☐XXX☐☐☐☐☐XXX☐☐☐X☐ after toggling with step = 13
☐XX☐XXXX☐XXXXX☐X☐☐☐☐☐☐☐☐X☐☐☐☐X☐X☐X☐☐☐X☐X☐X☐XXX☐☐XXXX☐☐☐XXX☐X☐XXX☐☐☐☐XX☐X☐X☐☐☐☐☐X☐X☐XXXX☐☐☐☐☐XXX☐☐XX☐ after toggling with step = 14
☐XX☐XXXX☐XXXXXXX☐☐☐☐☐☐☐☐X☐☐☐☐☐☐X☐X☐☐☐X☐X☐X☐X☐X☐☐XXXX☐☐☐XXX☐☐☐XXX☐☐☐☐XX☐X☐XX☐☐☐☐X☐X☐XXXX☐☐X☐☐XXX☐☐XX☐ after toggling with step = 15
☐XX☐XXXX☐XXXXXX☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐X☐☐☐X☐X☐X☐X☐X☐XXXXX☐☐☐XXX☐☐☐XX☐☐☐☐☐XX☐X☐XX☐☐☐☐☐☐X☐XXXX☐☐X☐☐XXXX☐XX☐ after toggling with step = 16
☐XX☐XXXX☐XXXXXX☐X☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐X☐X☐X☐X☐XXX☐X☐☐☐XXX☐☐☐XX☐☐☐☐XXX☐X☐XX☐☐☐☐☐☐X☐X☐XX☐☐X☐☐XXXX☐XX☐ after toggling with step = 17
☐XX☐XXXX☐XXXXXX☐XX☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐XXX☐☐☐XX☐☐☐☐XXX☐☐☐XX☐☐☐☐☐☐X☐X☐XX☐☐☐☐☐XXXX☐XX☐ after toggling with step = 18
☐XX☐XXXX☐XXXXXX☐XXX☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐☐☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐☐☐XX☐☐☐☐XXX☐☐☐XXX☐☐☐☐☐X☐X☐XX☐☐☐☐☐XX☐X☐XX☐ after toggling with step = 19
☐XX☐XXXX☐XXXXXX☐XXXX☐☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐XX☐☐☐☐XXX☐☐☐XXX☐☐☐X☐X☐X☐XX☐☐☐☐☐XX☐X☐XXX after toggling with step = 20
☐XX☐XXXX☐XXXXXX☐XXXXX☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐☐☐☐☐XXX☐☐☐XXX☐☐☐X☐X☐☐☐XX☐☐☐☐☐XX☐X☐XXX after toggling with step = 21
☐XX☐XXXX☐XXXXXX☐XXXXXX☐☐X☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐X☐XXX☐X☐X☐X☐X☐X☐X☐☐☐X☐XXX☐☐☐XXX☐☐☐X☐X☐☐☐XXX☐☐☐☐XX☐X☐XXX after toggling with step = 22
☐XX☐XXXX☐XXXXXX☐XXXXXXX☐X☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐XXX☐X☐X☐X☐X☐X☐X☐☐☐X☐X☐X☐☐☐XXX☐☐☐X☐X☐☐☐XXX☐☐☐XXX☐X☐XXX after toggling with step = 23
☐XX☐XXXX☐XXXXXX☐XXXXXXXXX☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐XX☐X☐X☐X☐X☐X☐X☐☐☐X☐X☐X☐X☐XXX☐☐☐X☐X☐☐☐XXX☐☐☐XXX☐☐☐XXX after toggling with step = 24
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐X☐X☐X☐X☐X☐X☐☐☐X☐X☐X☐X☐X☐X☐☐☐X☐X☐☐☐XXX☐☐☐XXX☐☐☐XX☐ after toggling with step = 25
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐X☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐X☐X☐X☐X☐X☐☐☐X☐X☐X☐X☐X☐X☐X☐X☐X☐☐☐XXX☐☐☐XXX☐☐☐XX☐ after toggling with step = 26
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XX☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐X☐X☐X☐X☐☐☐X☐X☐X☐X☐X☐X☐X☐XXX☐☐☐XXX☐☐☐XXX☐☐☐XX☐ after toggling with step = 27
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXX☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐X☐X☐X☐☐☐X☐X☐X☐X☐X☐X☐X☐XXX☐X☐XXX☐☐☐XXX☐☐☐XX☐ after toggling with step = 28
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXX☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐X☐☐☐X☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐☐☐XXX☐☐☐XX☐ after toggling with step = 29
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXX☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐X☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐XXX☐☐☐XX☐ after toggling with step = 30
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXX☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐☐☐XX☐ after toggling with step = 31
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXX☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐X☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐XX☐ after toggling with step = 32
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXX☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐X☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 33
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐X☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 34
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐X☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 35
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐X☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 36
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐X☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 37
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XX☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐X☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 38
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXX☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐XXX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 39
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXX☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐XX☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 40
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXX☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐X☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 41
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXX☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐X☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 42
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXX☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐X☐X☐X☐X☐X☐X☐☐ after toggling with step = 43
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXX☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐X☐X☐X☐X☐X☐☐ after toggling with step = 44
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXX☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐X☐X☐X☐X☐☐ after toggling with step = 45
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXX☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐X☐X☐X☐☐ after toggling with step = 46
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐X☐☐ after toggling with step = 47
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐ after toggling with step = 48
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐ after toggling with step = 49
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 50
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XX☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 51
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXX☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 52
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXX☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 53
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXX☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 54
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXX☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 55
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXX☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 56
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXX☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 57
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXX☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 58
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXX☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 59
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXX☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 60
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXX☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 61
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 62
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 63
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 64
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 65
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XX☐☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 66
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXX☐☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 67
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXX☐☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 68
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXX☐☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 69
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXX☐☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 70
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXX☐☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 71
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXX☐☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 72
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXX☐☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 73
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXX☐☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 74
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXX☐☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 75
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXX☐☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 76
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXX☐☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 77
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXX☐☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 78
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 79
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 80
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 81
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐X☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 82
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 83
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 84
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXX☐☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 85
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXX☐☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 86
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXX☐☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 87
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXX☐☐☐☐☐☐☐☐☐☐☐X after toggling with step = 88
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXX☐☐☐☐☐☐☐☐☐☐X after toggling with step = 89
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXX☐☐☐☐☐☐☐☐☐X after toggling with step = 90
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXX☐☐☐☐☐☐☐☐X after toggling with step = 91
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXX☐☐☐☐☐☐☐X after toggling with step = 92
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXX☐☐☐☐☐☐X after toggling with step = 93
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXX☐☐☐☐☐X after toggling with step = 94
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXX☐☐☐☐X after toggling with step = 95
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXXX☐☐☐X after toggling with step = 96
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐☐X after toggling with step = 97
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXXX☐X after toggling with step = 98
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXXXXX after toggling with step = 99
☐XX☐XXXX☐XXXXXX☐XXXXXXXX☐XXXXXXXXXX☐XXXXXXXXXXXX☐XXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXX☐XXXXXXXXXXXXXXXXXX☐ after toggling with step = 100
```



## DCL

'''Adapted from optimized Batch example'''

```DCL

$! doors.com
$! Excecute by running @doors at prompt.
$ square = 1
$ incr = 3
$ count2 = 0
$ d = 1
$ LOOP2:
$       count2 = count2 + 1
$       IF (d .NE. square)
$               THEN WRITE SYS$OUTPUT "door ''d' is closed"
$       ELSE WRITE SYS$OUTPUT "door ''d' is open"
$               square = incr + square
$               incr = incr + 2
$       ENDIF
$       d = d + 1
$       IF (count2 .LT. 100) THEN GOTO LOOP2

```



## Delphi

:''See [[#Pascal|Pascal]]''

=={{header|Déjà Vu}}==

```dejavu
local :open-doors [ rep 101 false ]

for i range 1 100:
	local :j i
	while <= j 100:
		set-to open-doors j not open-doors! j
		set :j + j i

!print\ "Open doors: "
for i range 1 100:
	if open-doors! i:
		!print\( to-str i " " )
```

{{out}}

```txt
Open doors: 1 4 9 16 25 36 49 64 81 100
```



## DUP


```dup
100[$][0^:1-]#                                                  {initialize doors}
%
[s;[$101<][$$;~\:s;+]#%]d:                                     {function d, switch door state function}
1s:[s;101<][d;!s;1+s:]#                                        {increment step width from 1 to 100, execute function d each time}
1[$101<][$$.' ,;['o,'p,'e,'n,10,]['c,'l,'o,'s,'e,'d,10,]?1+]#  {loop through doors, print door number and state}
```


Result:


```dup
1 open
2 closed
3 closed
4 open
5 closed
6 closed
7 closed
8 closed
9 open
10 closed
11 closed
12 closed
...
94 closed
95 closed
96 closed
97 closed
98 closed
99 closed
100 open
```


Compare this solution to the [http://rosettacode.org/wiki/100_doors#FALSE FALSE] solution of this problem.


## DWScript

'''Unoptimized'''

```delphi
var doors : array [1..100] of Boolean;
var i, j : Integer;

for i := 1 to 100 do
   for j := i to 100 do
      if (j mod i) = 0 then
         doors[j] := not doors[j];F

for i := 1 to 100 do
   if doors[i] then
      PrintLn('Door '+IntToStr(i)+' is open');
```



## Dyalect

Outputs only open doors to save up space:


```Dyalect
var doors = Array.empty(100, false)

for p in 0..99 {
    for d in 0..99 {
        if (d + 1) % (p + 1) == 0 {
            doors[d] = !doors[d];
        }
    }
}

for d in doors.indices() when doors[d] {
    print("Door \(d+1): Open")
}
```


{{out}}


```txt
Door 1: Open
Door 4: Open
Door 9: Open
Door 16: Open
Door 25: Open
Door 36: Open
Door 49: Open
Door 64: Open
Door 81: Open
Door 100: Open
```



## Dylan

'''Unoptimized'''

```dylan
define method doors()
  let doors = make(<array>, fill: #f, size: 100);
  for (x from 0 below 100)
    for (y from x below 100 by x + 1)
      doors[y] := ~doors[y]
    end
  end;
  for (x from 1 to 100)
    if (doors[x - 1])
      format-out("door %d open\n", x)
    end
  end
end
```



## E

'''Graphical'''
{{works with|E-on-Java}}

This version animates the changes of the doors (as checkboxes).


```e
#!/usr/bin/env rune

var toggles := []
var gets := []

# Set up GUI (and data model)
def frame := <swing:makeJFrame>("100 doors")
frame.getContentPane().setLayout(<awt:makeGridLayout>(10, 10))
for i in 1..100 {
  def component := <import:javax.swing.makeJCheckBox>(E.toString(i))
  toggles with= fn { component.setSelected(!component.isSelected()) }
  gets with= fn { component.isSelected() }
  frame.getContentPane().add(component)
}

# Set up termination condition
def done
frame.addWindowListener(def _ {
  to windowClosing(event) {
    bind done := true
  }
  match _ {}
})

# Open and close doors
def loop(step, i) {
  toggles[i] <- ()
  def next := i + step
  timer.whenPast(timer.now() + 10, fn {
    if (next >= 100) {
      if (step >= 100) {
        # Done.
      } else {
        loop <- (step + 1, step)
      }
    } else {
      loop <- (step, i + step)
    }
  })
}
loop(1, 0)

frame.pack()
frame.show()
interp.waitAtTop(done)
```



## EasyLang

<lang>len d[] 101
for p = 1 to 100
  i = p
  while i <= 100
    d[i] = 1 - d[i]
    i += p
  .
.
for i = 1 to 100
  if d[i] = 1
    print i
  .
.
```



## EchoLisp

The result is obviously the same in we run the process backwards. So, we check the state of each door during the 100-th step (opening/closing every door)

```lisp

; initial state = closed = #f
(define doors (make-vector 101 #f))
; run pass 100 to 1
(for*
   ((pass (in-range 100 0 -1))
   (door (in-range 0 101 pass)))
    (when (and
        (vector-set! doors door (not (vector-ref doors door)))
        (= pass 1))
        (writeln door "is open")))

1     "is open"
4     "is open"
9     "is open"
16     "is open"
25     "is open"
36     "is open"
49     "is open"
64     "is open"
81     "is open"
100     "is open"

```



## ECL

'''optimized version'''


```ECL

Doors := RECORD
 UNSIGNED1 DoorNumber;
 STRING6   State;
END;

AllDoors := DATASET([{0,0}],Doors);

Doors  OpenThem(AllDoors L,INTEGER Cnt) := TRANSFORM
 SELF.DoorNumber := Cnt;
 SELF.State      := IF((CNT * 10) % (SQRT(CNT)*10)<>0,'Closed','Opened');
END;

OpenDoors := NORMALIZE(AllDoors,100,OpenThem(LEFT,COUNTER));

OpenDoors;

```


'''unoptimized version - demonstrating LOOP'''


```ECL

Doors := RECORD
  UNSIGNED1 DoorNumber;
  STRING6   State;
END;

AllDoors := DATASET([{0,'0'}],Doors);

//first build the 100 doors

Doors  OpenThem(AllDoors L,INTEGER Cnt) := TRANSFORM
  SELF.DoorNumber := Cnt;
  SELF.State      := 'Closed';
END;

ClosedDoors := NORMALIZE(AllDoors,100,OpenThem(LEFT,COUNTER));

//now iterate through them and use door logic

loopBody(DATASET(Doors) ds, UNSIGNED4 c) :=
            PROJECT(ds,    //ds=original input
              TRANSFORM(Doors,
                      	SELF.State := CASE((COUNTER % c) * 100,
		                            0 => IF(LEFT.STATE = 'Opened','Closed','Opened')
					    ,LEFT.STATE);
			SELF.DoorNumber := COUNTER;     //PROJECT COUNTER
                    ));

g1 := LOOP(ClosedDoors,100,loopBody(ROWS(LEFT),COUNTER));

OUTPUT(g1);


```


'''unoptimized version - using ITERATE'''
This is a bit more efficient than the LOOP version


```ECL

DoorSet := DATASET(100,TRANSFORM({UNSIGNED1 DoorState},SELF.DoorState := 1));
SetDoors := SET(DoorSet,DoorState);

Doors := RECORD
  UNSIGNED1 Pass;
  SET OF UNSIGNED1 DoorSet;
END;

StartDoors := DATASET(100,TRANSFORM(Doors,SELF.Pass := COUNTER,SELF.DoorSet := SetDoors));

Doors XF(Doors L, Doors R) := TRANSFORM
  ds := DATASET(L.DoorSet,{UNSIGNED1 DoorState});
  NextDoorSet := PROJECT(ds,
                         TRANSFORM({UNSIGNED1 DoorState},
                      	           SELF.DoorState := CASE((COUNTER % R.Pass) * 100,
                                                          0 => IF(LEFT.DoorState = 1,0,1),
                                                          LEFT.DoorState)));
  SELF.DoorSet := IF(L.Pass=0,R.DoorSet,SET(NextDoorSet,DoorState));
  SELF.Pass := R.Pass
END;

Res := DATASET(ITERATE(StartDoors,XF(LEFT,RIGHT))[100].DoorSet,{UNSIGNED1 DoorState});
PROJECT(Res,TRANSFORM({STRING20 txt},SELF.Txt := 'Door ' + COUNTER + ' is ' + IF(LEFT.DoorState=1,'Open','Closed')));

```



## EDSAC order code

Since there are only 100 doors, we'll keep things simple and use
a whole EDSAC location for each door.
A single bit would be enough, but that would make the code much longer.

The program works through the array of doors by modifying its own
orders (instructions). This would be considered bad practice today,
but was quite usual on the EDSAC.

```edsac

[Hundred doors problem from Rosetta Code website]
[EDSAC program, Initial Orders 2]

[Library subroutine M3. Prints header and is then overwritten.
Here, the last character sets the teleprinter to figures.]
       PFGKIFAFRDLFUFOFE@A6FG@E8FEZPF
       @&*THE!OPEN!DOORS!ARE@&#
       ..PZ   [blank tape, needed to mark end of header text]

[Library subroutine P6. Prints strictly positive integer.
32 locations; working locations 1, 4, 5]
        T56K  [define load address for subroutine]
        GKA3FT25@H29@VFT4DA3@TFH30@S6@T1F
        V4DU4DAFG26@TFTFO5FA4DF4FS4F
        L4FT4DA1FS3@G9@EFSFO31@E20@J995FJF!F

        T88K   [define load address for main program]
        GK     [set @ (theta) for relative addresses]

[The 100 doors are at locations 200..299.
Doors are numbered 0..99 internally, and 1..100 for output.
The base address and the number of doors can be varied.
The value of a door is 0 if open, negative if closed.]

                   [Constants. Program also uses order 'P 1 F'
                    which is permanently at absolute address 2.]
    [0] P200F  [address of door #0]
    [1] P100F  [number of doors, as an address]
    [2] UF     [makes S order from T, since 'S' = 'T' + 'U']
    [3] MF     [makes A order from T, since 'A' = 'T' + 'M']
    [4] V2047D [all 1's for "closed" (any negative value will do)]
    [5] &F     [line feed]
    [6] @F     [carriage return]
    [7] K4096F [teleprinter null[

                   [Variables]
    [8] PF   [pass number; step when toggling doors]
    [9] PF   [door number, as address, 0-based]
   [10] PF   [order referring to door 0]

                   [Enter with acc = 0]
                   [Part 1 : close all the doors]
   [11] T8@  [pass := 0 (used in part 2)]
        T9@  [door number := 0]
        A16@ [load 'T F' order]
        A@   [add base address]
        T10@ [store T order for door #0]
   [16] TF   [clear acc; also serves as constant]
        A9@  [load door number]
        A10@ [make T order]
        T21@ [plant in code]
        A4@  [load value for "closed"]
   [21] TF   [store in current door]
        A9@  [load door number]
        A2F  [add 1]
        U9@  [update door number]
        S1@  [done all doors yet?]
        G16@ [if not, loop back]

                   [Part 2 : 100 passes, toggling the doors]
   [27] TF   [clear acc]
        A8@  [load pass number]
        A2F  [add 1]
        T8@  [save updated pass number]
        S2F  [make -1]
        U9@  [door number := -1]
        A8@  [add pass number to get first door toggled on this pass]
        S1@  [gone beyond end?]
        E50@ [if so, move on to part 3]
   [36] A1@  [restore acc after test]
        U9@  [store current door number]
        A10@ [make T order to load status]
        U44@ [plant T order for first door in pass]
        A2@  [convert to S order]
        T43@ [plant S order]
        A4@  [load value for "closed"]
   [43] SF   [subtract status; toggles status]
   [44] TF   [update status]
        A9@  [load door number just toggled]
        A8@  [add pass number to get next door in pass]
        S1@  [gone beyond end?]
        G36@ [no, loop to do next door]
        E27@ [yes, loop to do next pass]

                   [Part 3 : Print list of open doors.
                    Header has set teleprinter to figures.]
   [50] TF   [clear acc]
        T9@  [door nr := 0]
        A10@ [T order for door 0]
        A3@  [convert to A order]
        T10@
   [55] TF
        A9@  [load door number]
        A10@ [make A order to load value]
        T59@ [plant in next order]
   [59] AF   [acc := 0 if open, < 0 if closed]
        G69@ [skip if closed]
        A9@  [door number as address]
        A2F  [add 1 for 1-based output]
        RD   [shift 1 right, address --> integer]
        TF   [store integer at 0 for printing]
   [65] A65@ [for return from subroutine]
        G56F [call subroutine to print door number]
        O6@  [followed by CRLF]
        O5@
   [69] TF   [clear acc]
        A9@  [load door number]
        A2F  [add 1]
        U9@  [update door number]
        S1@  [done all doors yet?]
        G55@  [if not, loop back]
   [75] O7@  [output null to flush teleprinter buffer]
        ZF   [stop]
        E11Z [define relative start address]
        PF

```

{{out}}

```txt

THE OPEN DOORS ARE
    1
    4
    9
   16
   25
   36
   49
   64
   81
  100

```




## Eero


```objc

#import <Foundation/Foundation.h>

int main()
  square := 1, increment = 3

  for int door in 1 .. 100
    printf("door #%d", door)

    if door == square
      puts(" is open.")
      square += increment
      increment += 2
    else
      puts(" is closed.")

  return 0

```



## Egel



```Egel

import "prelude.eg"

using System
using List

data open, closed

def toggle =
    [ open N -> closed N | closed N -> open N ]

def doors =
    [ N -> map [ N -> closed N ] (fromto 1 N) ]

def toggleK =
    [ K nil              -> nil
    | K (cons (D N) DD)  ->
         let DOOR = if (N%K) == 0 then toggle (D N) else D N in
             cons DOOR (toggleK K DD) ]

def toggleEvery =
    [ nil DOORS -> DOORS
    | (cons K KK) DOORS -> toggleEvery KK (toggleK K DOORS) ]

def run =
    [ N -> toggleEvery (fromto 1 N) (doors N) ]

def main = run 100

```



## EGL



```EGL

program OneHundredDoors

   function main()

      doors boolean[] = new boolean[100];
      n int = 100;

      for (i int from 1 to n)
         for (j int from i to n by i)
            doors[j] = !doors[j];
         end
      end

      for (i int from 1 to n)
         if (doors[i])
            SysLib.writeStdout( "Door " + i + " is open" );
         end
      end

   end

end

```



## Eiffel

This is my first RosettaCode submission, as well as a foray into Eiffel for myself. I've tried to adhere to the description of the problem statement, as well as showcase a few Eiffelisms shown in the documentation.

The replacement code below took the original code and has made improvements in some ways, such as:

# Removal of "magic" many magic numbers and strings.
# Refactor of various code blocks to routines (commands and queries with good CQS).
# Utilization/Demonstration of full, secret, and selective feature exporting.
# Utilization/Demonstration of constants as expanded type constants and once-functions.
# Utilization/Demonstration of static-references (e.g. {APPLICATION}.min_door_count).
# Utilization/Demonstration of "like" keyword type anchoring (e.g. a_index_address: like {DOOR}.address).
# Utilization/Demonstration of semi-strict logical implication (e.g. consistency: is_open implies not Is_closed).
# Utilization/Demonstration of contracts, including require, ensure, and class invariant.
# Utilization/Demonstration of agent and `do_all' call on ITERABLE type.
# Utilization/Demonstration of various forms of across including "loop" and "all".

... as well as other Eiffel-ism's and some coding standards/best-practices.

'''file: application.e'''

```eiffel
note
	description: "100 Doors problem"
	date: "08-JUL-2015"
	revision: "1.1"

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Main application routine.
		do
			initialize_closed_doors
			toggle_doors
			output_door_states
		end

feature -- Access

	doors: ARRAYED_LIST [DOOR]
			-- A set of doors (self-initialized to capacity of `max_door_count').
		attribute
			create Result.make (max_door_count)
		end

feature -- Basic Operations

	initialize_closed_doors
			-- Initialize all `doors'.
		do
			across min_door_count |..| max_door_count as ic_address_list loop
				doors.extend (create {DOOR}.make_closed (ic_address_list.item))
			end
		ensure
			has_all_closed_doors: across doors as ic_doors_list all not ic_doors_list.item.is_open end
		end

	toggle_doors
			-- Toggle all `doors'.
		do
			across min_door_count |..| max_door_count as ic_addresses_list loop
				across doors as ic_doors_list loop
					if is_door_to_toggle (ic_doors_list.item.address, ic_addresses_list.item) then
						ic_doors_list.item.toggle_door
					end
				end
			end
		end

	output_door_states
			-- Output the state of all `doors'.
		do
			doors.do_all (agent door_state_out)
		end

feature -- Status Report

	is_door_to_toggle (a_door_address, a_index_address: like {DOOR}.address): BOOLEAN
			-- Is the door at `a_door_address' needing to be toggled, when compared to `a_index_address'?
		do
			Result := a_door_address \\ a_index_address = 0
		ensure
			only_modulus_zero: Result = (a_door_address \\ a_index_address = 0)
		end

feature -- Outputs

	door_state_out (a_door: DOOR)
			-- Output the state of `a_door'.
		do
			print ("Door " + a_door.address.out + " is ")
			if a_door.is_open then
				print ("open.")
			else
				print ("closed.")
			end
			io.new_line
		end

feature {DOOR} -- Constants

	min_door_count: INTEGER = 1
			-- Minimum number of doors.

	max_door_count: INTEGER = 100
			-- Maximum number of doors.

end
```


'''file: door.e'''

```eiffel
note
	description: "A door with an address and an open or closed state."
	date: "08-JUL-2015"
	revision: "1.1"

class
	DOOR

create
	make_closed,
	make

feature {NONE} -- initialization

	make_closed (a_address: INTEGER)
			-- Initialize Current {DOOR} at `a_address' and state of `Is_closed'.
		require
			positive: a_address >= {APPLICATION}.min_door_count and a_address >= Min_door_count
		do
			make (a_address, Is_closed)
		ensure
			closed: is_open = Is_closed
		end

	make (a_address: INTEGER; a_status: BOOLEAN)
			-- Initialize Current {DOOR} with `a_address' and `a_status', denoting position and `is_open' or `Is_closed'.
		require
			positive: a_address >= {APPLICATION}.min_door_count and a_address >= Min_door_count
		do
			address := a_address
			is_open := a_status
		ensure
			address_set: address = a_address
			status_set: is_open = a_status
		end

feature -- access

	address: INTEGER
			-- `address' of Current {DOOR}.

	is_open: BOOLEAN assign set_open
			-- `is_open' (or not) status of Current {DOOR}.

feature -- Setters

	set_open (a_status: BOOLEAN)
			-- Set `status' with `a_status'
		do
			is_open := a_status
		ensure
			open_updated: is_open = a_status
		end

feature {APPLICATION} -- Basic Operations

	toggle_door
			-- Toggle Current {DOOR} from `is_open' to not `is_open'.
		do
			is_open := not is_open
		ensure
			toggled: is_open /= old is_open
		end

feature {NONE} -- Implementation: Constants

	Is_closed: BOOLEAN = False
			-- State of being not `is_open'.

	Min_door_count: INTEGER = 1
			-- Minimum door count.

invariant
	one_or_more: address >= 1
	consistency: is_open implies not Is_closed

end
```



## Ela

'''Standard Approach'''


```ela
open generic

type Door = Open | Closed
  deriving Show

gate [] _ = []
gate (x::xs) (y::ys)
  | x == y = Open :: gate xs ys
  | else = Closed :: gate xs ys

run n = gate [1..n] [& k*k \\ k <- [1..]]
```


'''Alternate Approach'''

```ela
open list
run n = takeWhile (<n) [& k*k \\ k <- [1..]]
```



## Elena

ELENA 4.0 :

```elena
import system'routines;
import extensions;

public program()
{
    var Doors := Array.allocate(100).populate:(n=>false);
    for(int i := 0, i < 100, i := i + 1)
    {
        for(int j := i, j < 100, j := j + i + 1)
        {
            Doors[j] := Doors[j].Inverted
        }
    };

    for(int i := 0, i < 100, i := i + 1)
    {
        console.printLine("Door #",i + 1," :",Doors[i].iif("Open","Closed"))
    };

    console.readChar()
}
```



## Elixir


```Elixir
defmodule HundredDoors do
  def doors(n \\ 100) do
    List.duplicate(false, n)
  end

  def toggle(doors, n) do
    List.update_at(doors, n, &(!&1))
  end

  def toggle_every(doors, n) do
    Enum.reduce( Enum.take_every((n-1)..99, n), doors, fn(n, acc) -> toggle(acc, n) end )
  end
end

# unoptimized
final_state = Enum.reduce(1..100, HundredDoors.doors, fn(n, acc) -> HundredDoors.toggle_every(acc, n) end)

open_doors = Enum.with_index(final_state)
             |> Enum.filter_map(fn {door,_} -> door end, fn {_,index} -> index+1 end)

IO.puts "All doors are closed except these: #{inspect open_doors}"


# optimized
final_state = Enum.reduce(1..10, HundredDoors.doors, fn(n, acc) -> HundredDoors.toggle(acc, n*n-1) end)

open_doors = Enum.with_index(final_state)
             |> Enum.filter_map(fn {door,_} -> door end, fn {_,index} -> index+1 end)

IO.puts "All doors are closed except these: #{inspect open_doors}"
```


{{out}}


```txt
All doors are closed except these: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



## Elm


```Elm
-- Unoptimized
import List exposing (indexedMap, foldl, repeat, range)
import Html exposing (text)
import Debug exposing (toString)

type Door = Open | Closed

toggle d = if d == Open then Closed else Open

toggleEvery : Int -> List Door -> List Door
toggleEvery k doors = indexedMap
  (\i door -> if modBy k (i+1) == 0 then toggle door else door)
  doors

n = 100

main =
  text (toString (foldl toggleEvery (repeat n Closed) (range 1 n)))

```



## Emacs Lisp

'''Unoptimized'''


```lisp
(defun create-doors ()
  "Returns a list of closed doors

Each door only has two status: open or closed.
If a door is closed it has the value 0, if it's open it has the value 1."
  (let ((return_value '(0))
         ;; There is already a door in the return_value, so k starts at 1
         ;; otherwise we would need to compare k against 99 and not 100 in
         ;; the while loop
         (k 1))
    (while (< k 100)
      (setq return_value (cons 0 return_value))
      (setq k (+ 1 k)))
    return_value))

(defun toggle-single-door (doors)
  "Toggle the stat of the door at the `car' position of the DOORS list

DOORS is a list of integers with either the value 0 or 1 and it represents
a row of doors.

Returns a list where the `car' of the list has it's value toggled (if open
it becomes closed, if closed it becomes open)."
  (if (= (car doors) 1)
    (cons 0 (cdr doors))
    (cons 1 (cdr doors))))

(defun toggle-doors (doors step original-step)
  "Step through all elements of the doors' list and toggle a door when step is 1

DOORS is a list of integers with either the value 0 or 1 and it represents
a row of doors.
STEP is the number of doors we still need to transverse before we arrive
at a door that has to be toggled.
ORIGINAL-STEP is the value of the argument step when this function is
called for the first time.

Returns a list of doors"
  (cond ((null doors)
          '())
    ((= step 1)
      (cons (car (toggle-single-door doors))
        (toggle-doors (cdr doors) original-step original-step)))
    (t
      (cons (car doors)
        (toggle-doors (cdr doors) (- step 1) original-step)))))

(defun main-program ()
  "The main loop for the program"
  (let ((doors_list (create-doors))
         (k 1)
         ;; We need to define max-specpdl-size and max-specpdl-size to big
         ;; numbers otherwise the loop reaches the max recursion depth and
         ;; throws an error.
         ;; If you want more information about these variables, press Ctrl
         ;; and h at the same time and then press v and then type the name
         ;; of the variable that you want to read the documentation.
         (max-specpdl-size 5000)
         (max-lisp-eval-depth 2000))
    (while (< k 101)
      (setq doors_list (toggle-doors doors_list k k))
      (setq k (+ 1 k)))
    doors_list))

(defun print-doors (doors)
  "This function prints the values of the doors into the current buffer.

DOORS is a list of integers with either the value 0 or 1 and it represents
a row of doors.
"
  ;; As in the main-program function, we need to set the variable
  ;; max-lisp-eval-depth to a big number so it doesn't reach max recursion
  ;; depth.
  (let ((max-lisp-eval-depth 5000))
    (unless (null doors)
      (insert (int-to-string (car doors)))
      (print-doors (cdr doors)))))

;; Returns a list with the final solution
(main-program)

;; Print the final solution on the buffer
(print-doors (main-program))
```



## Erlang

'''non-optimized'''

```erlang

-module(hundoors).

-export([go/0]).

toggle(closed) -> open;
toggle(open) -> closed.

go() -> go([closed || _ <- lists:seq(1, 100)],[], 1, 1).
go([], L, N, _I) when N =:= 101 -> lists:reverse(L);
go([], L, N, _I) -> go(lists:reverse(L), [], N + 1, 1);
go([H|T], L, N, I) ->
  H2 = case I rem N of
    0 -> toggle(H);
    _ -> H
  end,
  go(T, [H2|L], N, I + 1).

```



'''optimized'''

```erlang
doors() ->
     F = fun(X) -> Root = math:pow(X,0.5), Root == trunc(Root) end,
     Out = fun(X, true) -> io:format("Door ~p: open~n",[X]);
              (X, false)-> io:format("Door ~p: close~n",[X]) end,
     [Out(X,F(X)) || X <- lists:seq(1,100)].
```



## ERRE



```ERRE

! "100 Doors" program for ERRE LANGUAGE
! Author: Claudio Larini
! Date: 21-Nov-2014
!
! PC Unoptimized version translated from a QB version

PROGRAM 100DOORS

!$INTEGER

CONST N=100

DIM DOOR[N]

BEGIN

FOR STRIDE=1 TO N DO
    FOR INDEX=STRIDE TO N STEP STRIDE DO
        DOOR[INDEX]=NOT(DOOR[INDEX])
    END FOR
END FOR

PRINT("Open doors:";)
FOR INDEX=1 TO N DO
    IF DOOR[INDEX] THEN PRINT(INDEX;) END IF
END FOR
PRINT

END PROGRAM

```



## Euler Math Toolbox


```Euler Math Toolbox

>function Doors () ...
$  doors:=zeros(1,100);
$  for i=1 to 100
$    for j=i to 100 step i
$      doors[j]=!doors[j];
$    end;
$  end;
$  return doors
$endfunction
>nonzeros(Doors())
 [ 1  4  9  16  25  36  49  64  81  100 ]

```



## Euphoria

unoptimised

```Euphoria
-- doors.ex
include std/console.e
sequence doors
doors = repeat( 0, 100 ) -- 1 to 100, initialised to false

for pass = 1 to 100 do
	for door = pass to 100 by pass do
		--printf( 1, "%d", doors[door] )
		--printf( 1, "%d", not doors[door] )
		doors[door] = not doors[door]
	end for
end for

sequence oc

for i = 1 to 100 do
	if doors[i] then
		oc = "open"
	else
		oc = "closed"
	end if
 	printf( 1, "door %d is %s\n", { i, oc } )
end for

```



## Excel


Note: The use of Auto Fill saves a lot of time when entering this code. One can refer to Excel help pages to learn about Auto Fill features.

Create a labelling column (A) and row (1) labelling the number of the door (column A, labelling starts in row 2 with a "1" and continues counting up to "100" in row 101) and the number of the pass (row 1, labelling starts in column B with a "0" and continues counting up to "100" in column CX). Additonally, you can label cell A1 as "Door/Pass" or so.

Closed doors are represented by zeroes ("0"), open doors are represented by ones ("1"). To represent the initial condition fill rows 2 to 101 in column B (pass "0") with zeroes.

Starting in column C, row 2, you enter code as shown in the examples below. The examples show the code to be entered in cells C2, C3, and D2. Continue to write code for the rest of the 4245 data cells, accordingly. Excel Auto Fill feature is best used for this.


Cell C2:

```Excel

=IF($A2/C$1=INT($A2/C$1),IF(B2=0,1,IF(B2=1,0)),B2)

```

Cell C3:

```Excel

=IF($A3/C$1=INT($A3/C$1),IF(B3=0,1,IF(B3=1,0)),B3)

```

Cell D2:

```Excel

=IF($A2/D$1=INT($A2/D$1),IF(C2=0,1,IF(C2=1,0)),C2)

```


The last column (column CX, labelled "100") shows a "1" for each door (labelled by the rows in column A) that is open after the 100th pass. It shows a "1" for the following doors: 1, 4, 9, 16, 25, 36, 49, 64, 81, 100.

=={{header|F_Sharp|F#}}==
Requires #light in versions of F# prior to 2010 beta.

```fsharp
let answerDoors =
    let ToggleNth n (lst:bool array) =                  // Toggle every n'th door
        [(n-1) .. n .. 99]                              // For each appropriate door
        |> Seq.iter (fun i -> lst.[i] <- not lst.[i])   // toggle it
    let doors = Array.create 100 false                  // Initialize all doors to closed
    Seq.iter (fun n -> ToggleNth n doors) [1..100]      // toggle the appropriate doors for each pass
    doors                                               // Initialize all doors to closed

```

'''Unoptimized / functional'''

```fsharp

let modifier doors skip =
    let rec modifierInner doors skip counter =
        match doors with
        | [] -> []                                                  //base case: end of hall
        | first::rest when counter >= skip ->                       //case: reached door marked for change
            not first::(modifierInner rest skip 0)                  //  open or close that door
        | first::rest ->                                            //case: reached door to skip
            first::(modifierInner rest skip (counter+1))            //  skip it
    modifierInner doors skip 0                                      //Initial state for walkthrough

let answerDoors doors =
    let rec modifyDoors skipRange doors modifier =                  //fold each door result to the next with
        List.fold modifier doors skipRange                          //with an increasing skip
    modifyDoors [0..99] doors modifier                              //Initial starting state

let initDoors = Array.create 100 false |> Array.toList              //Initialize all doors to closed (false)

answerDoors initDoors |> printfn "%A"                               //print answer (false is closed door)

```

'''Tail-Recursive Optimized/Functional'''

```fsharp

let modifier doors skip =
    let rec modifier' doors skip counter result =
        match doors with
        | [] -> result |> List.rev                                  //base case: end of hall
        | first::rest when counter >= skip ->                       //case: reached door marked for change
            modifier' rest skip 0 ((not first)::result)             //  open or close that door
        | first::rest ->                                            //case: reached door to skip
            modifier' rest skip (counter+1) (first::result)         //  skip it
    modifier' doors skip 0 []                                       //Initial state for walkthrough

```

Following is the solution using perfect squares.  The coercions in PerfectSquare are, I believe, slightly different in versions prior to 2010 beta and, again, #light is required in those versions.

```fsharp
open System
let answer2 =
    let PerfectSquare n =
        let sqrt = int(Math.Sqrt(float n))
        n = sqrt * sqrt
    [| for i in 1..100 do yield PerfectSquare i |]
```


Simple single line solution using nothing but List

```fsharp

[1..100] |> List.fold (fun doors pass->List.mapi (fun i x->if ((i + 1) % pass)=0 then not x else x) doors) (List.init 100 (fun _->false))

```



## Factor

'''Unoptimized'''

```Factor
USING: bit-arrays formatting fry kernel math math.ranges
sequences ;
IN: rosetta.doors

CONSTANT: number-of-doors 100

: multiples ( n -- range )
    0 number-of-doors rot <range> ;

: toggle-multiples ( n doors -- )
    [ multiples ] dip '[ _ [ not ] change-nth ] each ;

: toggle-all-multiples ( doors -- )
    [ number-of-doors [1,b] ] dip '[ _ toggle-multiples ] each ;

: print-doors ( doors -- )
    [
        swap "open" "closed" ? "Door %d is %s\n" printf
    ] each-index ;

: main ( -- )
    number-of-doors 1 + <bit-array>
    [ toggle-all-multiples ] [ print-doors ] bi ;

main
```


'''Optimized'''

```Factor

USING:
    formatting
    math math.primes.factors math.ranges
    sequences ;
IN: rosetta-doors2

: main ( -- )
    100 [1,b] [ divisors length odd? ] filter "Open %[%d, %]\n" printf ;

```



## Falcon

'''Unoptimized code'''

```falcon
doors = arrayBuffer( 101, false )

for pass in [ 0 : doors.len() ]
  for door in [ 0 : doors.len() : pass+1 ]
    doors[ door ] = not doors[ door ]
  end
end

for door in [ 1 : doors.len() ]  // Show Output
  >  "Door ", $door, " is: ", ( doors[ door ] ) ? "open" : "closed"
end

```

'''Optimized code'''

```falcon

for door in [ 1 : 101 ]: > "Door ", $door, " is: ", fract( door ** 0.5 ) ? "closed" : "open"
```



## FALSE


```false
100[$][0 1ø:1-]#              {initialize doors}
%
[s;[$101\>][$$;~\:s;+]#%]d:   {function d, switch door state function}
1s:[s;101\>][d;!s;1+s:]#      {increment step width from 1 to 100, execute function d each time}
1[$101\>][$$." ";$["open
"]?~["closed
"]?1+]#                       {loop through doors, print door number and state}
```


Result:


```dup
1 open
2 closed
3 closed
4 open
5 closed
6 closed
7 closed
8 closed
9 open
10 closed
...
98 closed
99 closed
100 open
```


Compare this solution to the [http://rosettacode.org/wiki/100_doors#DUP DUP] solution of this problem.


## Fantom

'''Unoptimized'''

```fantom

    states := (1..100).toList
    100.times |i| {
      states = states.map |state| { state % (i+1) == 0 ? -state : +state }
    }
    echo("Open doors are " + states.findAll { it < 0 }.map { -it })

```

'''Optimized'''

```fantom

    echo("Open doors are " + (1..100).toList.findAll { it.toFloat.pow(0.5f).toInt.pow(2) == it})

```



## FBSL

'''Unoptimised'''

```qbasic
#AppType Console

Dim doors[], n As Integer = 100

For Dim i = 1 To n
	For Dim j = i To n Step i
		doors[j] = Not doors[j]
	Next
Next

For i = 1 To n
	If doors[i] Then Print "Door ", i, " is open"
Next

Pause
```

'''Optimised''' (by ML)

```qbasic
#APPTYPE CONSOLE

DIM i = 0, j = 0, door = 1

WHILE INCR(i) < 101
 IF i = door THEN
   PRINT "Door ", door, " open"
   INCR(door, INCR((INCR(j) << 1)))
 END IF
WEND

PAUSE
```



## Fish


'''Unoptimized'''

```fish
1001-p01.
>0101-p02.
>101-g001-g+:::aa*)?v101-p03.
>02-g?v1}02-p02.    >05.
      >0}02-p02.
>~~~0101-p001-g:1+001-paa*)?v02.
                            >07.
>0101-p08.
>101-g::02-g?v     >1+:101-paa*=?;
             >n" "o^
```



## friendly interactive shell


'''Unoptimized'''

```fishshell
# Set doors to empty list
set doors

# Initialize doors arrays
for i in (seq 100)
    set doors[$i] 0
end

for i in (seq 100)
    set j $i
    while test $j -le 100
        # Logical not on doors
        set doors[$j] (math !$doors[$j])
        set j (math $j + $i)
    end
end

# Print every door
for i in (seq (count $doors))
    echo -n "$i "
    if test $doors[$i] -eq 0
        echo closed
    else
        echo open
    end
end

```


'''Optimized'''

```fishshell
# Set doors to empty list
set doors

for i in (seq 100)
    set doors[(math "$i * $i")] 1
    echo -n "$i "
    if test $doors[$i] -eq 1
        echo open
    else
        echo closed
    end
end
```



## Forth


'''Unoptimized'''

```forth
: toggle ( c-addr -- )  \ toggle the byte at c-addr
    dup c@ 1 xor swap c! ;

100  1+ ( 1-based indexing ) constant ndoors
create doors  ndoors allot

: init ( -- )  doors ndoors erase ;  \ close all doors

: pass ( n -- )  \ toggle every nth door
    ndoors over do
        doors i + toggle
    dup ( n ) +loop drop ;

: run ( -- )  ndoors 1 do  i pass  loop ;
: display ( -- )  \ display open doors
    ndoors 1 do  doors i + c@ if  i .  then loop cr ;

init run display
```


'''Optimized'''

```forth
: squared ( n -- n' )  dup * ;
: doors ( n -- )
    1 begin 2dup squared >= while
        dup squared .
    1+ repeat 2drop ;
100 doors
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/100_doors this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

{{works with|Fortran 90}}

'''unoptimized'''


```fortran
program doors
    implicit none
    integer, allocatable :: door(:)
    character(6), parameter :: s(0:1) = ["closed", "open  "]
    integer :: i, n

    print "(A)", "Number of doors?"
    read *, n
    allocate (door(n))
    door = 1
    do i = 1, n
        door(i:n:i) = 1 - door(i:n:i)
        print "(A,G0,2A)", "door ", i, " is ", s(door(i))
    end do
end program
```


'''optimized'''


```fortran
PROGRAM DOORS

  INTEGER, PARAMETER :: n = 100    ! Number of doors
  INTEGER :: i
  LOGICAL :: door(n) = .TRUE.      ! Initially closed

  DO i = 1, SQRT(REAL(n))
    door(i*i) = .FALSE.
  END DO

  DO i = 1, n
    WRITE(*,"(A,I3,A)", ADVANCE="NO") "Door ", i, " is "
    IF (door(i)) THEN
      WRITE(*,"(A)") "closed"
    ELSE
      WRITE(*,"(A)") "open"
    END IF
  END DO

END PROGRAM DOORS
```



## FreeBASIC


### Toggle


```freebasic
' version 27-10-2016
' compile with: fbc -s console

#Define max_doors 100

Dim As ULong c, n, n1, door(1 To max_doors)

' toggle, at start all doors are closed (0)
' 0 = door closed, 1 = door open
For n = 1 To max_doors
    For n1 = n To max_doors Step n
        door(n1) = 1 - door(n1)
    Next
Next

' count the doors that are open (1)
Print "doors that are open nr: ";
For n = 1 To max_doors
    If door(n) = 1 Then
        Print n; " ";
        c = c + 1
    End If
Next

Print : Print
Print "There are " + Str(c) + " doors open"

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
doors that are open nr: 1 4 9 16 25 36 49 64 81 100

There are 10 doors open
```


### Count


```freebasic
' version 27-10-2016
' compile with: fbc -s console

#Define max_doors 100

Dim As ULong c, n, n1, door(1 To max_doors)

' at start all doors are closed
' simple add 1 each time we open or close a door
' doors with odd numbers are open
' doors with even numbers are closed
For n = 1 To max_doors
    For n1 = n To max_doors Step n
        door(n1) += 1
    Next
Next

Print "doors that are open nr: ";
For n = 1 To max_doors
    If door(n) And 1 Then
        Print n; " ";
        c = c + 1
    End If
Next

Print : Print
Print "There are " + Str(c) + " doors open"

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

Output is the same as the first version.

###  Optimized


```freebasic
' version 27-10-2016
' compile with: fbc -s console

#Define max_doors 100

Dim As ULong c, n

Print "doors that are open nr: ";
For n = 1 To 10
    Print n * n; " ";
    c = c + 1
Next

Print : Print
Print "There are " + Str(c) + " doors open"

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

Output is the same as the first version.


## Free Pascal


```pascal

program OneHundredIsOpen;

const
  DoorCount = 100;

var
  IsOpen: array[1..DoorCount] of boolean;
  Door, Jump: integer;

begin
  // Close all doors
  for Door := 1 to DoorCount do
    IsOpen[Door] := False;
  // Iterations
  for Jump := 1 to DoorCount do
  begin
    Door := Jump;
    repeat
      IsOpen[Door] := not IsOpen[Door];
      Door := Door + Jump;
    until Door > DoorCount;
  end;
  // Show final status
  for Door := 1 to DoorCount do
  begin
    Write(Door, ' ');
    if IsOpen[Door] then
      WriteLn('open')
    else
      WriteLn('closed');
  end;
  // Wait for <enter>
  Readln;
end.

```



## Frink


```frink

doors = new array[[101], false]
for pass=1 to 100
   for door=pass to 100 step pass
      doors@door = ! doors@door

print["Open doors:  "]
for door=1 to 100
   if doors@door
      print["$door "]

```



## FunL


###  Unoptimized


```funl
for i <- 1..100
  r = foldl1( \a, b -> a xor b, [(a|i) | a <- 1..100] )
  println( i + ' ' + (if r then 'open' else 'closed') )
```



###  Optimized


```funl
import math.sqrt

for i <- 1..100
  println( i + ' ' + (if sqrt(i) is Integer then 'open' else 'closed') )
```



## Futhark




```Futhark

let main(n: i32): [n]bool =
  loop is_open = replicate n false for i < n do
    let js = map (*i+1) (iota n)
    let flips = map (\j ->
                       if j < n
                       then unsafe !is_open[j]
                       else true -- Doesn't matter.
                    ) js
    in scatter is_open js flips

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as short door, square : square = 1
dim as short increment : increment = 3

for door = 1 to 100
   if (door == square)
      print "Door"; door; " is open."
      square += increment
      increment += 2
   else
      print "Door"; door; " is closed."
   end if
next

```


Output:

```txt

Door 1 is open.
Door 2 is closed.
Door 3 is closed.
Door 4 is open.
Door 5 is closed.
Door 6 is closed.
Door 7 is closed.
Door 8 is closed.
Door 9 is open.
Door 10 is closed.
Door 11 is closed.
Door 12 is closed.
Door 13 is closed.
Door 14 is closed.
Door 15 is closed.
Door 16 is open.
Door 17 is closed.
Door 18 is closed.
Door 19 is closed.
Door 20 is closed.
Door 21 is closed.
Door 22 is closed.
Door 23 is closed.
Door 24 is closed.
Door 25 is open.
Door 26 is closed.
Door 27 is closed.
Door 28 is closed.
Door 29 is closed.
Door 30 is closed.
Door 31 is closed.
Door 32 is closed.
Door 33 is closed.
Door 34 is closed.
Door 35 is closed.
Door 36 is open.
Door 37 is closed.
Door 38 is closed.
Door 39 is closed.
Door 40 is closed.
Door 41 is closed.
Door 42 is closed.
Door 43 is closed.
Door 44 is closed.
Door 45 is closed.
Door 46 is closed.
Door 47 is closed.
Door 48 is closed.
Door 49 is open.
Door 50 is closed.
Door 51 is closed.
Door 52 is closed.
Door 53 is closed.
Door 54 is closed.
Door 55 is closed.
Door 56 is closed.
Door 57 is closed.
Door 58 is closed.
Door 59 is closed.
Door 60 is closed.
Door 61 is closed.
Door 62 is closed.
Door 63 is closed.
Door 64 is open.
Door 65 is closed.
Door 66 is closed.
Door 67 is closed.
Door 68 is closed.
Door 69 is closed.
Door 70 is closed.
Door 71 is closed.
Door 72 is closed.
Door 73 is closed.
Door 74 is closed.
Door 75 is closed.
Door 76 is closed.
Door 77 is closed.
Door 78 is closed.
Door 79 is closed.
Door 80 is closed.
Door 81 is open.
Door 82 is closed.
Door 83 is closed.
Door 84 is closed.
Door 85 is closed.
Door 86 is closed.
Door 87 is closed.
Door 88 is closed.
Door 89 is closed.
Door 90 is closed.
Door 91 is closed.
Door 92 is closed.
Door 93 is closed.
Door 94 is closed.
Door 95 is closed.
Door 96 is closed.
Door 97 is closed.
Door 98 is closed.
Door 99 is closed.
Door 100 is open.

```



## FUZE BASIC


```qbasic
READ x,y,z
PRINT "Open doors: ";x;" ";
CYCLE
    z=x+y
    PRINT z;" ";
    x=z
    y=y+2
REPEAT UNTIL z>=100
DATA 1,3,0
END
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=10f109e90581b04b9cbc30fdaf49892b Click this link to run this code]'''

```gambas
Public Sub Main()
Dim bDoor As New Boolean[101]
Dim siCount1, siCount2, siStart As Short

For siCount1 = 1 To 100
  Inc siStart
  For siCount2 = siStart To 100 Step siCount1
    bDoor[siCount2] = Not bDoor[siCount2]
  Next
Next

For siCount1 = 1 To 100
  If bDoor[siCount1] Then Print siCount1;;
Next

End
```

Output:

```txt

1 4 9 16 25 36 49 64 81 100

```



## GAP


```gap
doors := function(n)
  local a,j,s;
  a := [ ];
  for j in [1 .. n] do
    a[j] := 0;
  od;
  for s in [1 .. n] do
    j := s;
    while j <= n do
      a[j] := 1 - a[j];
      j := j + s;
    od;
  od;
  return Filtered([1 .. n], j -> a[j] = 1);
end;

doors(100);
# [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 ]
```



## Genie



```genie

// 100 doors problem
// Author: Sinuhe masan (2019)
init

	// 100 elements array of boolean type
	doors:bool[100]

	for var i = 1 to 100
		doors[i] = false  // set all doors closed


	for var i = 1 to 100
		j:int = i
		while j <= 100 do
			doors[j] = not doors[j]
			j = j + i

	print("Doors open: ")
	for var i = 1 to 100
		if doors[i]
			stdout.printf ("%d ", i)


```



## Glee


```glee
100` *=0=>d                      $$ create vector 1..100, create bit pattern d, marking all equal to 0
:for (1..100[.s]){               $$ loop s from 1 to 100
  d^(100` %s *=0 )=>d;}          $$ d = d xor (bit pattern of vector 1..100 % s)
d                                $$ output d

```


The resulting output is the bit pattern showing the state of the 100 doors:


```glee
Result:
10010000 10000001 00000000 10000000 00010000 00000000 10000000 00000001 00000000 00000000 10000000 00000000 0001
```



## GML


```gml
var doors,a,i;
//Sets up the array for all of the doors.
for (i = 1; i<=100; i += 1)
    {
    doors[i]=0;
    }

//This first for loop goes through and passes the interval down to the next for loop.
for (i = 1; i <= 100; i += 1;)
    {
    //This for loop opens or closes the doors and uses the interval(if interval is 2 it only uses every other etc..)
    for (a = 0; a <= 100; a += i;)
        {
        //Opens or closes a door.
        doors[a] = !doors[a];
        }
    }
open_doors = '';

//This for loop goes through the array and checks for open doors.
//If the door is open it adds it to the string then displays the string.
for (i = 1; i <= 100; i += 1;)
    {
    if (doors[i] == 1)
        {
        open_doors += "Door Number "+string(i)+" is open#";
        }
    }
show_message(open_doors);
game_end();
```



## Go

'''unoptimized'''

```go
package main

import "fmt"

func main() {
    doors := [100]bool{}

    // the 100 passes called for in the task description
    for pass := 1; pass <= 100; pass++ {
        for door := pass-1; door < 100; door += pass {
            doors[door] = !doors[door]
        }
    }

    // one more pass to answer the question
    for i, v := range doors {
        if v {
            fmt.Print("1")
        } else {
            fmt.Print("0")
        }

        if i%10 == 9 {
            fmt.Print("\n")
        } else {
            fmt.Print(" ")
        }

    }
}
```

Output:

```txt

1 0 0 1 0 0 0 0 1 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 1 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 0 0 0
0 0 0 1 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 1

```

'''optimized'''

```go
package main

import "fmt"

func main() {
    var door int = 1
    var incrementer = 0

    for current := 1; current <= 100; current++ {
        fmt.Printf("Door %d ", current)

        if current == door {
            fmt.Printf("Open\n")
            incrementer++
            door += 2*incrementer + 1
        } else {
            fmt.Printf("Closed\n")
        }
    }
}
```



## Golfscript


```golfscript
100:c;[{0}c*]:d;
c,{.c,>\)%{.d<\.d=1^\)d>++:d;}/}/
[c,{)"door "\+" is"+}%d{{"open"}{"closed"}if}%]zip
{" "*puts}/
```


'''optimized with sqrt'''
(Original version of GolfScript has no sqrt operator, but it can be
added easily; the code was tested using a work-in-progress C interpreter
for a language compatible enough with Golfscript)

```golfscript
100,{)}%
{:d.sqrt 2?=
{"open"}{"close"}if"door "d+" is "+\+puts}/
```


'''optimized without sqrt'''

```golfscript
[{"close"}100*]:d;
10,{)2?(.d<\["open"]\)d>++:d;}/
[100,{)"door "\+" is"+}%d]zip
{" "*puts}/
```



## Gosu

'''unoptimized'''

```scala

uses java.util.Arrays

var doors = new boolean[100]
Arrays.fill( doors, false )

for( pass in 1..100 ) {
    var counter = pass-1
    while( counter < 100 ) {
        doors[counter] = !doors[counter]
        counter += pass
  }
}

for( door in doors index i ) {
    print( "door ${i+1} is ${door ? 'open' : 'closed'}" )
}


```


'''optimized'''

```scala

var door = 1
var delta = 0

for( i in 1..100 ) {
    if( i == door ) {
        print( "door ${i} is open" )
        delta++
        door += 2*delta + 1
    } else {
        print( "door ${i} is closed" )
    }
}

```



## Groovy

'''unoptimized'''

```groovy
doors = [false] * 100
(0..99).each {
   it.step(100, it + 1) {
      doors[it] ^= true
   }
}
(0..99).each {
   println("Door #${it + 1} is ${doors[it] ? 'open' : 'closed'}.")
}
```


'''optimized a'''
Using square roots


```groovy
(1..100).each {
   println("Door #${it} is ${Math.sqrt(it).with{it==(int)it} ? 'open' : 'closed'}.")
}
```


'''optimized b'''
Without using square roots

```groovy
doors = ['closed'] * 100
(1..10).each { doors[it**2 - 1] = 'open' }
(0..99).each {
   println("Door #${it + 1} is ${doors[it]}.")
}
```


=={{header|GW-BASIC}}==

```qbasic
10 DIM A(100)
20 FOR OFFSET = 1 TO 100
30      FOR I = 0 TO 100 STEP OFFSET
40              A(I) = A(I) + 1
50      NEXT I
60 NEXT OFFSET
70 ' Print "opened" doors
80 FOR I = 1 TO 100
90      IF A(I) MOD 2 = 1 THEN PRINT I
100 NEXT I
```


'''Output''':
 1
 4
 9
 16
 25
 36
 49
 64
 81
 100


## Harbour


'''Unoptimized code:'''

```visualfoxpro
#define ARRAY_ELEMENTS 100
PROCEDURE Main()
   LOCAL aDoors := Array( ARRAY_ELEMENTS )
   LOCAL i, j

   AFill( aDoors, .F. )
   FOR i := 1 TO ARRAY_ELEMENTS
      FOR j := i TO ARRAY_ELEMENTS STEP i
         aDoors[ j ] = ! aDoors[ j ]
      NEXT
   NEXT
   AEval( aDoors, {|e, n| QQout( Padl(n,3) + " is " + Iif(aDoors[n], "*open*", "closed" ) + "|" ), Iif( n%5 == 0, Qout(), e:=NIL) } )
   RETURN
```


'''Optimized code'''

```visualfoxpro
#define ARRAY_ELEMENTS 100
PROCEDURE Main()
   LOCAL aDoors := Array( ARRAY_ELEMENTS )

   AFill( aDoors, .F. )
   AEval( aDoors, {|e, n| aDoors[n] := e := Iif( Int(Sqrt(n))==Sqrt(n), .T., .F. ) } )
   AEval( aDoors, {|e, n| QQout( Padl(n,3) + " is " + Iif(aDoors[n], "*open*", "closed" ) + "|" ), Iif( n%5 == 0, Qout(), e:=NIL )} )
   RETURN
```

'''Output:'''
  1 is *open*|  2 is closed|  3 is closed|  4 is *open*|  5 is closed|
  6 is closed|  7 is closed|  8 is closed|  9 is *open*| 10 is closed|
 11 is closed| 12 is closed| 13 is closed| 14 is closed| 15 is closed|
 16 is *open*| 17 is closed| 18 is closed| 19 is closed| 20 is closed|
 21 is closed| 22 is closed| 23 is closed| 24 is closed| 25 is *open*|
 26 is closed| 27 is closed| 28 is closed| 29 is closed| 30 is closed|
 31 is closed| 32 is closed| 33 is closed| 34 is closed| 35 is closed|
 36 is *open*| 37 is closed| 38 is closed| 39 is closed| 40 is closed|
 41 is closed| 42 is closed| 43 is closed| 44 is closed| 45 is closed|
 46 is closed| 47 is closed| 48 is closed| 49 is *open*| 50 is closed|
 51 is closed| 52 is closed| 53 is closed| 54 is closed| 55 is closed|
 56 is closed| 57 is closed| 58 is closed| 59 is closed| 60 is closed|
 61 is closed| 62 is closed| 63 is closed| 64 is *open*| 65 is closed|
 66 is closed| 67 is closed| 68 is closed| 69 is closed| 70 is closed|
 71 is closed| 72 is closed| 73 is closed| 74 is closed| 75 is closed|
 76 is closed| 77 is closed| 78 is closed| 79 is closed| 80 is closed|
 81 is *open*| 82 is closed| 83 is closed| 84 is closed| 85 is closed|
 86 is closed| 87 is closed| 88 is closed| 89 is closed| 90 is closed|
 91 is closed| 92 is closed| 93 is closed| 94 is closed| 95 is closed|
 96 is closed| 97 is closed| 98 is closed| 99 is closed|100 is *open*|


## Haskell

'''unoptimized'''

```haskell
data Door
  = Open
  | Closed
  deriving (Eq, Show)

toggle :: Door -> Door
toggle Open = Closed
toggle Closed = Open

toggleEvery :: Int -> [Door] -> [Door]
toggleEvery k = zipWith toggleK [1 ..]
  where
    toggleK n door
      | n `mod` k == 0 = toggle door
      | otherwise = door

run :: Int -> [Door]
run n = foldr toggleEvery (replicate n Closed) [1 .. n]

main :: IO ()
main = print $ filter ((== Open) . snd) $ zip [1 ..] (run 100)
```

{{Out}}

```txt
[(1,Open),(4,Open),(9,Open),(16,Open),(25,Open),(36,Open),(49,Open),(64,Open),(81,Open),(100,Open)]
```



'''optimized'''
(without using square roots)

```haskell>gate :: Eq a =
 [a] -> [a] -> [Door]
gate (x:xs) (y:ys) | x == y  =  Open   : gate xs ys
gate (x:xs) ys               =  Closed : gate xs ys
gate []     _                =  []

run n = gate [1..n] [k*k | k <- [1..]]
```


alternatively, returning a list of all open gates, it's a one-liner:


```haskell
run n = takeWhile (< n) [k*k | k <- [1..]]
```



## Haxe


```haxe
class RosettaDemo
{
    static public function main()
    {
        findOpenLockers(100);
    }

    static function findOpenLockers(n : Int)
    {
        var i = 1;

        while((i*i) <= n)
        {
            Sys.print(i*i + "\n");
            i++;
        }
    }
}
```



## HicEst

Unoptimized

```hicest
REAL :: n=100, open=1, door(n)

door = 1 - open ! = closed
DO i = 1, n
  DO j = i, n, i
    door(j) = open - door(j)
  ENDDO
ENDDO
DLG(Text=door, TItle=SUM(door)//" doors open")
```

Optimized

```hicest
door = 1 - open ! = closed
DO i = 1, n^0.5
  door(i*i) = open
ENDDO
DLG(Text=door, TItle=SUM(door)//" doors open")
```



## HolyC

{{trans|C}}

```holyc
U8 is_open[100];
U8 pass = 0, door = 0;

/* do the 100 passes */
for (pass = 0; pass < 100; ++pass)
  for (door = pass; door < 100; door += pass + 1)
    is_open[door] = !is_open[door];

/* output the result */
for (door = 0; door < 100; ++door)
  if (is_open[door])
    Print("Door #%d is open.\n", door + 1);
  else
    Print("Door #%d is closed.\n", door + 1);

```



## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}"
#! huginn

import Algorithms as algo;

main() {
        doorCount = 100;
        doors = [].resize( doorCount, false );

        for ( pass : algo.range( doorCount ) ) {
                i = 0;
                step = pass + 1;
                while ( i < doorCount ) {
                        doors[i] = ! doors[i];
                        i += step;
                }
        }

        for ( i : algo.range( doorCount ) ) {
                if ( doors[i] ) {
                        print( "door {} is open\n".format( i ) );
                }
        }
        return ( 0 );
}
```



## Hy


{{trans|Coco}}


```lisp
(def doors (* [False] 100))

(for [pass (range (len doors))]
  (for [i (range pass (len doors) (inc pass))]
    (assoc doors i (not (get doors i)))))

(for [i (range (len doors))]
  (print (.format "Door {} is {}."
    (inc i)
    (if (get doors i) "open" "closed"))))
```



## I


```i
software {
	var doors = len(100)

	for pass over [1, 100]
		var door = pass - 1
		loop door < len(doors) {
			doors[door] = doors[door]/0
			door += pass
		}
	end

	for door,isopen in doors
		if isopen
			print("Door ",door+1,": open")
		end
	end
	print("All other doors are closed")
}
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon don't have a boolean type because most often, logic is expressed in terms of success or failure, which affects flow at run time.

'''Unoptimized solution.'''

```icon

procedure main()
    door := table(0)    # default value of entries is 0
    every pass := 1 to 100 do
        every door[i := pass to 100 by pass] := 1 - door[i]

    every write("Door ", i := 1 to 100, " is ", if door[i] = 1 then "open" else "closed")
end

```


'''Optimized solution.'''

```icon

procedure main()
    every write("Door ", i := 1 to 100, " is ", if integer(sqrt(i)) = sqrt(i) then "open" else "closed")
end

```


or


```icon
procedure main(args)
    dMap := table("closed")
    every dMap[(1 to sqrt(100))^2] := "open"
    every write("Door ",i := 1 to 100," is ",dMap[i])
end
```



## Idris


```idris
import Data.Vect

-- Creates list from 0 to n (not including n)
upTo : (m : Nat) -> Vect m (Fin m)
upTo Z = []
upTo (S n) = 0 :: (map FS (upTo n))

data DoorState = DoorOpen | DoorClosed

toggleDoor : DoorState -> DoorState
toggleDoor DoorOpen = DoorClosed
toggleDoor DoorClosed = DoorOpen

isOpen : DoorState -> Bool
isOpen DoorOpen = True
isOpen DoorClosed = False

initialDoors : Vect 100 DoorState
initialDoors = fromList $ map (\_ => DoorClosed) [1..100]

iterate : (n : Fin m) -> Vect m DoorState -> Vect m DoorState
iterate n doors {m} =
  map (\(idx, doorState) =>
          if ((S (finToNat idx)) `mod` (S (finToNat n))) == Z
              then toggleDoor doorState
              else doorState)
      (zip (upTo m) doors)

-- Returns all doors left open at the end
solveDoors : List (Fin 100)
solveDoors =
  findIndices isOpen $ foldl (\doors,val => iterate val doors) initialDoors (upTo 100)

main : IO ()
main = print $ map (\n => S (finToNat n)) solveDoors
```



## Inform 7

{{works with|Z-machine|8}}
{{works with|Glulx virtual machine}}

```inform7
Hallway is a room.

A toggle door is a kind of thing.
A toggle door can be open or closed. It is usually closed.
A toggle door has a number called the door number.
Understand the door number property as referring to a toggle door.
Rule for printing the name of a toggle door: say "door #[door number]".

There are 100 toggle doors.

When play begins (this is the initialize doors rule):
	let the next door number be 1;
	repeat with D running through toggle doors:
		now the door number of D is the next door number;
		increment the next door number.

To toggle (D - open toggle door): now D is closed.
To toggle (D - closed toggle door): now D is open.

When play begins (this is the solve puzzle rule):
	let the door list be the list of toggle doors;
	let the door count be the number of entries in the door list;
	repeat with iteration running from 1 to 100:
		let N be the iteration;
		while N is less than the door count:
			toggle entry N in the door list;
			increase N by the iteration;
	say "Doors left open: [list of open toggle doors].";
	end the story.
```



## Informix 4GL


```Informix 4GL

MAIN
    DEFINE
        i, pass SMALLINT,
        doors ARRAY[100] OF SMALLINT

    FOR i = 1 TO 100
        LET doors[i] = FALSE
    END FOR

    FOR pass = 1 TO 100
        FOR i = pass TO 100 STEP pass
            LET doors[i] = NOT doors[i]
        END FOR
    END FOR

    FOR i = 1 TO 100
        IF doors[i]
          THEN DISPLAY i USING "Door <<& is open"
          ELSE DISPLAY i USING "Door <<& is closed"
        END IF
    END FOR
END MAIN

```



## Io

simple boolean list solution:

```io
doors := List clone
100 repeat(doors append(false))
for(i,1,100,
    for(x,i,100, i, doors atPut(x - 1, doors at(x - 1) not))
)
doors foreach(i, x, if(x, "Door #{i + 1} is open" interpolate println))
```

Optimized solution:

```io
(Range 1 to(10) asList) foreach(v, "Door #{v ** 2} is open." interpolate println)
```

Sample output:
```txt
Door 1 is open.
Door 4 is open.
Door 9 is open.
Door 16 is open.
Door 25 is open.
Door 36 is open.
Door 49 is open.
Door 64 is open.
Door 81 is open.
Door 100 is open.
```



## Ioke

'''Unoptimized Object Oriented solution.'''

```ioke
NDoors = Origin mimic

NDoors Toggle = Origin mimic do(
  initialize = method(toggled?, @toggled? = toggled?)
  toggle! = method(@toggled? = !toggled?. self)
)

NDoors Doors = Origin mimic do(
  initialize = method(n,
    @n = n
    @doors = {} addKeysAndValues(1..n, (1..n) map(_, NDoors Toggle mimic(false)))
  )
  numsToToggle = method(n, for(x <- (1..@n), (x % n) zero?, x))
  toggleThese = method(nums, nums each(x, @doors[x] = @doors at(x) toggle))
  show = method(@doors filter:dict(value toggled?) keys sort println)
)

; Test code
x = NDoors Doors mimic(100)
(1..100) each(n, x toggleThese(x numsToToggle(n)))
x show
```



## J

'''unoptimized'''

```j
   ~:/ (100 $ - {. 1:)"0 >:i.100
1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ...
   ~:/ 0=|/~ >:i.100  NB. alternative
1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ...
```

'''optimized'''

```j
   (e. *:) 1+i.100
1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ...
   1 (<:*:i.10)} 100$0  NB. alternative
1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ...
```


'''with formatting'''

```j
   'these doors are open' ; >: I. (>:i.100) e. *: i.11
+------------------------------------------------+
¦these doors are open¦1 4 9 16 25 36 49 64 81 100¦
+------------------------------------------------+


```



## Java



```java
class HundredDoors {
    public static void main(String[] args) {
        boolean[] doors = new boolean[101];

        for (int i = 1; i < doors.length; i++) {
            for (int j = i; j < doors.length; j += i) {
                doors[j] = !doors[j];
            }
        }

        for (int i = 1; i < doors.length; i++) {
            if (doors[i]) {
                System.out.printf("Door %d is open.%n", i);
            }
        }
    }
}
```


'''If only printing the result is required.'''

```java
class HundredDoors {
    public static void main(String[] args) {
        for (int i = 1; i <= 10; i++)
            System.out.printf("Door %d is open.%n", i * i);
    }
}
```


Output:

```txt
Door 1 is open.
Door 4 is open.
Door 9 is open.
Door 16 is open.
Door 25 is open.
Door 36 is open.
Door 49 is open.
Door 64 is open.
Door 81 is open.
Door 100 is open.
```


'''If only printing the result is required, using streams.'''

```java
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class HundredDoors {
    public static void main(String args[]) {
        String openDoors = IntStream.rangeClosed(1, 100)
                .filter(i -> Math.pow((int) Math.sqrt(i), 2) == i)
                .mapToObj(Integer::toString)
                .collect(Collectors.joining(", "));
        System.out.printf("Open doors: %s%n", openDoors);
    }
}

```


Output:

```txt

Open doors: 1, 4, 9, 16, 25, 36, 49, 64, 81, 100

```



## JavaScript


###  ES5


### = Iterative =


```javascript

var doors=[];
for(var i=0;i<100;i++)
 doors[i]=false;             //create doors
for(var i=1;i<=100;i++)
 for(var i2=i-1,g;i2<100;i2+=i)
  doors[i2]=!doors[i2];      //toggle doors
for(var i=1;i<=100;i++)      //read doors
 console.log("Door %d is %s",i,doors[i-1]?"open":"closed")

```



### =Functional Composition=


Naive search

```JavaScript
(function (n) {
    'use strict';


    // finalDoors :: Int -> [(Int, Bool)]
    function finalDoors(n) {
        var lstRange = range(1, n);

        return lstRange
            .reduce(function (a, _, k) {
                var m = k + 1;

                return a.map(function (x, i) {
                    var j = i + 1;

                    return [j, j % m ? x[1] : !x[1]];
                });
            }, zip(
                lstRange,
                replicate(n, false)
            ));
    };



    // GENERIC FUNCTIONS

    // zip :: [a] -> [b] -> [(a,b)]
    function zip(xs, ys) {
        return xs.length === ys.length ? (
            xs.map(function (x, i) {
                return [x, ys[i]];
            })
        ) : undefined;
    }

    // replicate :: Int -> a -> [a]
    function replicate(n, a) {
        var v = [a],
            o = [];

        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    }

    // range(intFrom, intTo, optional intStep)
    // Int -> Int -> Maybe Int -> [Int]
    function range(m, n, delta) {
        var d = delta || 1,
            blnUp = n > m,
            lng = Math.floor((blnUp ? n - m : m - n) / d) + 1,
            a = Array(lng),
            i = lng;

        if (blnUp)
            while (i--) a[i] = (d * i) + m;
        else
            while (i--) a[i] = m - (d * i);

        return a;
    }


    return finalDoors(n)
        .filter(function (tuple) {
            return tuple[1];
        })
        .map(function (tuple) {
            return {
                door: tuple[0],
                open: tuple[1]
            };
        });

})(100);
```


{{out}}

```JavaScript
[{"door":1, "open":true}, {"door":4, "open":true}, {"door":9, "open":true}, {"door":16, "open":true}, {"door":25, "open":true}, {"door":36, "open":true}, {"door":49, "open":true}, {"door":64, "open":true}, {"door":81, "open":true}, {"door":100, "open":true}]
```


==== Optimized ( iterative )====

```javascript
for (var door = 1; door <= 100; door++) {
  var sqrt = Math.sqrt(door);
  if (sqrt === (sqrt | 0)) {
    console.log("Door %d is open", door);
  }
}
```



Simple for loop. Optimizing the optimized?

```javascript
for(var door=1;i<10/*Math.sqrt(100)*/;i++){
 console.log("Door %d is open",i*i);
}
```


==== Optimized ( functional ) ====

The question of which doors are flipped an odd number of times reduces to the question of which numbers have an odd number of integer factors.

We can simply search for these:


```JavaScript
(function (n) {
    'use strict';

    return range(1, 100)
        .filter(function (x) {
            return integerFactors(x)
                .length % 2;
        });

    function integerFactors(n) {
        var rRoot = Math.sqrt(n),
            intRoot = Math.floor(rRoot),

            lows = range(1, intRoot)
            .filter(function (x) {
                return (n % x) === 0;
            });

        // for perfect squares, we can drop the head of the 'highs' list
        return lows.concat(lows.map(function (x) {
                return n / x;
            })
            .reverse()
            .slice((rRoot === intRoot) | 0));
    }

    // range(intFrom, intTo, optional intStep)
    // Int -> Int -> Maybe Int -> [Int]
    function range(m, n, delta) {
        var d = delta || 1,
            blnUp = n > m,
            lng = Math.floor((blnUp ? n - m : m - n) / d) + 1,
            a = Array(lng),
            i = lng;

        if (blnUp)
            while (i--) a[i] = (d * i) + m;
        else
            while (i--) a[i] = m - (d * i);

        return a;
    }

})(100);
```


Or we can note, on inspection and further reflection, that only perfect squares have odd numbers of integer factors - all other numbers have only matched pairs of factors - low factors below the non-integer square root, and the corresponding quotients above the square root. In the case of perfect squares, the additional integer square root (not paired with any other factor than itself) makes the total number of distinct factors odd.


```JavaScript
(function (n) {
    'use strict';

    return perfectSquaresUpTo(100);

    function perfectSquaresUpTo(n) {
        return range(1, Math.floor(Math.sqrt(n)))
            .map(function (x) {
                return x * x;
            });
    }

    // GENERIC

    // range(intFrom, intTo, optional intStep)
    // Int -> Int -> Maybe Int -> [Int]
    function range(m, n, delta) {
        var d = delta || 1,
            blnUp = n > m,
            lng = Math.floor((blnUp ? n - m : m - n) / d) + 1,
            a = Array(lng),
            i = lng;

        if (blnUp)
            while (i--) a[i] = (d * i) + m;
        else
            while (i--) a[i] = m - (d * i);
        return a;
    }

})(100);
```


{{Out}}

```JavaScript
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



###  ES6



```javascript

Array.apply(null, { length: 100 })
  .map((v, i) => i + 1)
    .forEach(door => {
      var sqrt = Math.sqrt(door);

      if (sqrt === (sqrt | 0)) {
        console.log("Door %d is open", door);
      }
    });
```




```javascript
// Array comprehension style
[ for (i of Array.apply(null, { length: 100 })) i ].forEach((_, i) => {
  var door = i + 1
  var sqrt = Math.sqrt(door);

  if (sqrt === (sqrt | 0)) {
    console.log("Door %d is open", door);
  }
});
```


The result is always:

```txt
Door 1 is open
Door 4 is open
Door 9 is open
Door 16 is open
Door 25 is open
Door 36 is open
Door 49 is open
Door 64 is open
Door 81 is open
Door 100 is open
```


Or using a more general function for listing perfect squares:


```JavaScript
(function (n) {


    // ONLY PERFECT SQUARES HAVE AN ODD NUMBER OF INTEGER FACTORS
    // (Leaving the door open at the end of the process)

    return perfectSquaresUpTo(n);


    // perfectSquaresUpTo :: Int -> [Int]
    function perfectSquaresUpTo(n) {
        return range(1, Math.floor(Math.sqrt(n)))
            .map(x => x * x);
    }


    // GENERIC

    // range(intFrom, intTo, optional intStep)
    // Int -> Int -> Maybe Int -> [Int]
    function range(m, n, step) {
        let d = (step || 1) * (n >= m ? 1 : -1);

        return Array.from({
            length: Math.floor((n - m) / d) + 1
        }, (_, i) => m + (i * d));
    }

})(100);
```


{{Out}}


```JavaScript
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



## jq

jq arrays have 0 as their index origin, but in the following, the 100 doors are numbered from 1 to 100.

'''Solution by simulation'''
```jq
# Solution for n doors:
def doors(n):

  def print:
    . as $doors
    | range(1; length+1)
    | if $doors[.] then "Door \(.) is open" else empty end;

    [range(n+1)|null] as $doors
  | reduce range(1; n+1) as $run
      ( $doors; reduce range($run; n+1; $run ) as $door
                  ( .; .[$door] = (.[$door] | not) ) )
  | print ;


```

'''Analytical solution'''
```jq
# Solution for 100 doors:
def solution:
  range(1;11) | "Door \(. * .) is open";

```



## Julia

'''Simple''':
* falses(100) creates a 100-element Bool array filled with false values,
* 'b in a:a:100' translates to 'start:step:end',
* string concatenation by '*'.



```julia
doors = falses(100)
for a in 1:100, b in a:a:100
    doors[b] = !doors[b]
end
for a = 1:100
    println("Door $a is " * (doors[a] ? "open." : "closed."))
end
```


'''Gimmicky-optimized''':

```julia
for i in 1:10 println("Door $(i^2) is open.") end
```



## K

'''unoptimized''' / converted from Q .

```k
 `closed `open ![ ; 2 ] @ #:' 1 _ = ,/ &:' 0 = t !\:/: t : ! 101
```


'''optimized'''  / 1 origin indices

```k
 ( 1 + ! 10 ) ^ 2
```


/ As parameterized function :

```k
 { ( 1 + ! _ x ^ % 2 ) ^ 2 } 100
```



## Klong


### unoptimized


```K

flip::{,/{(1-*x),1_x}'x:#y}
i::0;(100{i::i+1;flip(i;x)}:*100:^0)?1

```



### optimized


```K

(1+!9)^2

```



## Kotlin


```scala
fun oneHundredDoors(): List<Int> {
    val doors = BooleanArray(100, { false })
    for (i in 0..99) {
        for (j in i..99 step (i + 1)) {
            doors[j] = !doors[j]
        }
    }
    return doors
        .mapIndexed { i, b -> i to b }
        .filter { it.second }
        .map { it.first + 1 }
}
```



## KQL


```KQL
range InitialDoor from 1 to 100 step 1
| extend DoorsVisited=range(InitialDoor, 100, InitialDoor)
| mvexpand DoorVisited=DoorsVisited to typeof(int)
| summarize VisitCount=count() by DoorVisited
| project Door=DoorVisited, IsOpen=(VisitCount % 2) == 1
```



## LabVIEW

{{VI solution|100doors.png}}
;Optimized
{{VI solution|LabVIEW_100_doors.png}}


## Lasso


###  Loop


```Lasso
loop(100) => {^
	local(root = math_sqrt(loop_count))
	local(state = (#root == math_ceil(#root) ? '<strong>open</strong>' | 'closed'))
	#state != 'closed' ? 'Door ' + loop_count + ': ' + #state + '
'
^}
```

{{out}}

```txt
Door 1: open
Door 4: open
Door 9: open
Door 16: open
Door 25: open
Door 36: open
Door 49: open
Door 64: open
Door 81: open
Door 100: open
```



## Latitude


```latitude
use 'format importAllSigils.

doors := Object clone.
doors missing := { False. }.
doors check := {
  self slot ($1 ordinal).
}.
doors toggle := {
  self slot ($1 ordinal) = self slot ($1 ordinal) not.
}.
1 upto 101 do {
  takes '[i].
  local 'j = i.
  while { j <= 100. } do {
    doors toggle (j).
    j = j + i.
  }.
}.
$stdout printf: ~fmt "The open doors are: ~A", 1 upto 101 filter { doors check. } to (Array).
```



## Lhogho

This implementation defines 100 variables, named "1 through "100, rather than using a list. Thanks to Pavel Boytchev, the author of Lhogho, for help with the code.


```Logo
to doors
	;Problem 100 Doors
	;Lhogho

	for "p [1 100]
	[
		make :p "false
	]

	for "a [1 100 1]
	[
		for "b [:a 100 :a]
		[
			if :b < 101
			[
				make :b not thing :b
			]
		]
	]

	for "c [1 100]
	[
		if thing :c
		[
			(print "door :c "is "open)
		]
	]
end

doors
```



## Liberty BASIC


```lb
dim doors(100)
for pass = 1 to 100
    for door = pass to 100 step pass
        doors(door) = not(doors(door))
    next door
next pass
print "open doors ";
for door = 1 to 100
    if doors(door) then print door;"  ";
next door
```



## Lily


```Lily
var doors = List.fill(100, false)

for i in 0...99:
    for j in i...99 by i + 1:
        doors[j] = !doors[j]

# The type must be specified since the list starts off empty.
var open_doors: List[Integer] = []

doors.each_index{|i|
    if doors[i]:
        open_doors.push(i + 1)
}

print($"Open doors: ^(open_doors)")
```

{{out}}

```txt
Open doors: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



## LiveCode


```lb

on mouseUp
   repeat with tStep = 1 to 100
      repeat with tDoor = tStep to 100 step tStep
         put not tDoors[tDoor] into tDoors[tDoor]
      end repeat
      if tDoors[tStep] then put "Door " & tStep & " is open" & cr after tList
   end repeat
   set the text of field "Doors" to tList
end mouseUp

```



## Logo




```Logo
to doors
;Problem 100 Doors
;FMSLogo
;lrcvs 2010

make "door (vector 100 1)
for [p 1 100][setitem :p :door 0]

for [a 1 100 1][for [b :a 100 :a][make "x item :b :door
	                          ifelse :x  = 0 [setitem :b :door 1][setitem :b :door 0] ] ]

for [c 1 100][make "y item :c :door
	      ifelse :y = 0 [pr (list :c "Close)] [pr (list :c "Open)] ]
end
```



## LOLCODE


```LOLCODE
HAI 1.3

I HAS A doors ITZ A BUKKIT
IM IN YR hallway UPPIN YR door TIL BOTH SAEM door AN 100
    doors HAS A SRS door ITZ FAIL BTW, INISHULIZE ALL TEH DOORZ AS CLOZD
IM OUTTA YR hallway

IM IN YR hallway UPPIN YR pass TIL BOTH SAEM pass AN 100
    I HAS A door ITZ pass
    IM IN YR passer
        doors'Z SRS door R NOT doors'Z SRS door
        door R SUM OF door AN SUM OF pass AN 1
        DIFFRINT door AN SMALLR OF door AN 99, O RLY?
            YA RLY, GTFO
        OIC
    IM OUTTA YR passer
IM OUTTA YR hallway

IM IN YR printer UPPIN YR door TIL BOTH SAEM door AN 100
    VISIBLE "Door #" SUM OF door AN 1 " is "!
    doors'Z SRS door, O RLY?
        YA RLY, VISIBLE "open."
        NO WAI, VISIBLE "closed."
    OIC
IM OUTTA YR printer

KTHXBYE
```



## Lua


```lua
local is_open = {}

for pass = 1,100 do
    for door = pass,100,pass do
        is_open[door] = not is_open[door]
    end
end

for i,v in next,is_open do
    print ('Door '..i..':',v and 'open' or 'close')
end
```



## M2000 Interpreter

Second dim preserve values except explicit assign a value for each item using = or a different value using << and a lambda function as generator.

Here we use =false to make all items false (which is a double value of 0).

M2000 use True and False as -1 and 0 (type of double), but from comparisons return Boolean True and False, which used as -1 and 0 also. Using =1=1 we get Boolean True and =1=0 we get Boolean False. We can check type from a variable using Type$(), so x=1=1 : Print Type$(x)="Boolean". We can chack type of an expression using a function: Def ExpressionType$(x)=Type$(x)




```M2000 Interpreter

Module Doors100 {
      Dim Doors(1 to 100)
      For i=1 to 100
            For j=i to 100 step i
                  Doors(j)~
            Next j
      Next i
      DispAll()
      ' optimization
      Dim Doors(1 to 100)=False
      For i=1 to 10
            Doors(i**2)=True
      Next i
      Print
      DispAll()
      Sub DispAll()
            Local i
            For i=1 to 100
                  if Doors(i) then print i,
            Next i
            Print
      End Sub
}
Doors100

```




## M4


```m4
define(`_set', `define(`$1[$2]', `$3')')dnl
define(`_get', `defn(`$1[$2]')')dnl
define(`for',`ifelse($#,0,``$0'',`ifelse(eval($2<=$3),1,
`pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl
define(`opposite',`_set(`door',$1,ifelse(_get(`door',$1),`closed',`open',`closed'))')dnl
define(`upper',`100')dnl
for(`x',`1',upper,`1',`_set(`door',x,`closed')')dnl
for(`x',`1',upper,`1',`for(`y',x,upper,x,`opposite(y)')')dnl
for(`x',`1',upper,`1',`door x is _get(`door',x)
')dnl
```



## Maple



```Maple

NDoors := proc( N :: posint )
        # Initialise, using 0 to represent "closed"
        local pass, door, doors := Array( 1 .. N, 'datatype' = 'integer'[ 1 ] );
        # Now do N passes
        for pass from 1 to N do
                for door from pass by pass while door <= N do
                        doors[ door ] := 1 - doors[ door ]
                end do
        end do;
        # Output
        for door from 1 to N do
                printf( "Door %d is %s.\n", door, `if`( doors[ door ] = 0, "closed", "open" ) )
        end do;
        # Since this is a printing routine, return nothing.
        NULL
end proc:

```

To solve the problem, call it with 100 as argument (output not shown here).

```Maple

> NDoors( 100 );

```

Here is the optimised version, which outputs only the open doors.

```Maple

> seq( i^2, i = 1 .. isqrt( 100 ) );
                  1, 4, 9, 16, 25, 36, 49, 64, 81, 100

```

Alternatively,

```Maple

> [seq]( 1 .. 10 )^~2;
                 [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

```



## Mathematica

'''unoptimized 1'''

```mathematica
n=100;
tmp=ConstantArray[-1,n];
Do[tmp[[i;;;;i]]*=-1;,{i,n}];
Do[Print["door ",i," is ",If[tmp[[i]]==-1,"closed","open"]],{i,1,Length[tmp]}]
```


'''unoptimized 2'''

```mathematica
f[n_] = "Closed";
Do[Do[If[f[n] == "Closed", f[n] = "Open", f[n] = "Closed"], {n, k, 100, k}], {k, 1, 100}];
Table[f[n], {n, 1, 100}]
```


'''unoptimized 3'''

Mathematica also supports immutable data paradigms, like so:

```Mathematica

Fold[
 ReplacePart[#1, (i_ /; Mod[i, #2] == 0) :> (-#1[[i]])] &,
 ConstantArray[-1, {100}],
 Range[100]
] /. {1 -> "Open", -1 -> "Closed"}

```



'''optimized 1'''

```mathematica
Do[Print["door ",i," is ",If[IntegerQ[Sqrt[i]],"open","closed"]],{i,100}]
```


'''optimized 2'''

```mathematica
n=100;
a=Range[1,Sqrt[n]]^2
Do[Print["door ",i," is ",If[MemberQ[a,i],"open","closed"]],{i,100}]
```


'''optimized 3'''

```mathematica
n=100
nn=1
a=0
For[i=1,i<=n,i++,
 If[i==nn,
  Print["door ",i," is open"];
  a++;
  nn+=2a+1;
 ,
  Print["door ",i," is closed"];
 ];
]
```


These will only give the indices for the open doors:
'''unoptimized 2'''

```mathematica
Pick[Range[100], Xor@@@Array[Divisible[#1,#2]&, {100,100}]]
```


'''optimized 4'''

```mathematica
Range[Sqrt[100]]^2
```


=={{header|MATLAB}} / {{header|Octave}}==


### Iterative Method

'''Unoptimized'''

```MATLAB
a = false(1,100);
for b=1:100
  for i = b:b:100
    a(i) = ~a(i);
  end
end
a

```

'''Optimized'''

```MATLAB

for x=1:100;
  if sqrt(x) == floor(sqrt(x))
    a(i)=1;
  end
end
a

```

'''More Optimized'''

```MATLAB

a = zeros(100,1);
for counter = 1:sqrt(100);
  a(counter^2) = 1;
end
a

```



### Vectorized Method


```MATLAB
function [doors,opened,closed] = hundredDoors()

    %Initialize the doors, make them booleans for easy vectorization
    doors = logical( (1:1:100) );

    %Go through the flipping process, ignore the 1 case because the doors
    %array is already initialized to all open
    for initialPosition = (2:100)
        doors(initialPosition:initialPosition:100) = not( doors(initialPosition:initialPosition:100) );
    end

    opened = find(doors); %Stores the numbers of the open doors
    closed = find( not(doors) ); %Stores the numbers of the closed doors

end
```


===Known-Result Method===

```MATLAB

doors((1:10).^2) = 1;

doors

```



## Maxima


```maxima
doors(n) := block([v], local(v),
  v: makelist(true, n),
  for i: 2 thru n do
  for j: i step i thru n do v[j]: not v[j],
  sublist_indices(v, 'identity));
```

Usage:

```maxima
doors(100);
/* [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] */
```



## MAXScript

'''unoptimized'''

```maxscript
doorsOpen = for i in 1 to 100 collect false

for pass in 1 to 100 do
(
    for door in pass to 100 by pass do
    (
        doorsOpen[door] = not doorsOpen[door]
    )
)

for i in 1 to doorsOpen.count do
(
    format ("Door % is open?: %\n") i doorsOpen[i]
)
```

'''optimized'''

```maxscript
for i in 1 to 100 do
(
    root = pow i 0.5
    format ("Door % is open?: %\n") i (root == (root as integer))
)
```



## Mercury


```Mercury
:- module doors.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module bitmap, bool, list, string, int.

:- func doors = bitmap.
doors = bitmap.init(100, no).

:- pred walk(int, bitmap, bitmap).
:- mode walk(in, bitmap_di, bitmap_uo) is det.
walk(Pass, !Doors) :-
    walk(Pass, Pass, !Doors).

:- pred walk(int, int, bitmap, bitmap).
:- mode walk(in, in, bitmap_di, bitmap_uo) is det.
walk(At, By, !Doors) :-
    ( if bitmap.in_range(!.Doors, At - 1) then
        bitmap.unsafe_flip(At - 1, !Doors),
        walk(At + By, By, !Doors)
    else
        true
    ).

:- pred report(bitmap, int, io, io).
:- mode report(bitmap_di, in, di, uo) is det.
report(Doors, N, !IO) :-
    ( if is_set(Doors, N - 1) then
        State = "open"
    else
        State = "closed"
    ),
    io.format("door #%d is %s\n",
        [i(N), s(State)], !IO).

main(!IO) :-
    list.foldl(walk, 1 .. 100, doors, Doors),
    list.foldl(report(Doors), 1 .. 100, !IO).
```



## Metafont


```metafont
boolean doors[];
for i = 1 upto 100: doors[i] := false; endfor
for i = 1 upto 100:
  for j = 1 step i until 100:
    doors[j] := not doors[j];
  endfor
endfor
for i = 1 upto 100:
  message decimal(i) & " " & if doors[i]: "open" else: "close" fi;
endfor
end
```



## Microsoft Small Basic

{{trans|GW-BASIC}}

```microsoftsmallbasic

For offset = 1 To 100
  For i = 0 To 100 Step offset
    a[i] = a[i] + 1
  EndFor
EndFor
' Print "opened" doors
For i = 1 To 100
  If math.Remainder(a[i], 2) = 1 Then
    TextWindow.WriteLine(i)
  EndIf
EndFor

```


'''Output''':
 1
 4
 9
 16
 25
 36
 49
 64
 81
 100


## MIPS Assembly


```mips
.data
  doors:     .space 100
  num_str:   .asciiz "Number "
  comma_gap: .asciiz " is "
  newline:   .asciiz "\n"

.text
main:
# Clear all the cells to zero
  li $t1, 100
  la $t2, doors
clear_loop:
  sb $0, ($t2)
  add $t2, $t2, 1
  sub $t1, $t1, 1
  bnez $t1, clear_loop

# Now start the loops
  li $t0, 1         # This will the the step size
  li $t4, 1         # just an arbitrary 1
loop1:
  move $t1, $t0      # Counter
  la $t2, doors      # Current pointer
  add $t2, $t2, $t0
  addi $t2, $t2, -1
loop2:
  lb $t3, ($t2)
  sub $t3, $t4, $t3
  sb $t3, ($t2)
  add $t1, $t1, $t0
  add $t2, $t2, $t0
  ble $t1, 100, loop2

  addi $t0, $t0, 1
  ble $t0, 100, loop1

  # Now display everything
  la $t0, doors
  li $t1, 1
loop3:
  li $v0, 4
  la $a0, num_str
  syscall

  li $v0, 1
  move $a0, $t1
  syscall

  li $v0, 4
  la $a0, comma_gap
  syscall

  li $v0, 1
  lb $a0, ($t0)
  syscall

  li $v0, 4,
  la $a0, newline
  syscall

  addi $t0, $t0, 1
  addi $t1, $t1, 1
  bne $t1, 101 loop3

```



## MiniScript


Using a map to hold the set of open doors:

```MiniScript
d = {}
for p in range(1, 100)
    for t in range(p, 100, p)
        if d.hasIndex(t) then d.remove t else d.push t
    end for
end for

print d.indexes.sort
```


{{out}}

```txt
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```


Using an array of boolean values to keep track of door state, and a separate list of indexes of the open doors:

```MiniScript
d = [false] * 101
open = []
for p in range(1, 100)
    for t in range(p, 100, p)
        d[t] = not d[t]
    end for
    if d[p] then open.push p
end for

print open
```


(Output same as above.)


## Mirah



```Mirah
import java.util.ArrayList

class Door
	:state

	def initialize
		@state=false
	end

	def closed?; !@state; end
	def open?; @state; end

	def close; @state=false; end
	def open; @state=true; end

	def toggle
		if closed?
			open
		else
			close
		end
	end

	def toString; Boolean.toString(@state); end
end

doors=ArrayList.new
1.upto(100) do
    doors.add(Door.new)
end

1.upto(100) do |multiplier|
    index = 0
    doors.each do |door|
        Door(door).toggle if (index+1)%multiplier == 0
        index += 1
    end
end

i = 0
doors.each do |door|
    puts "Door #{i+1} is #{door}."
    i+=1
end

```



## mIRC Scripting Language


```mirc
var %d = $str(0 $+ $chr(32),100), %m = 1
while (%m <= 100) {
  var %n = 1
  while ($calc(%n * %m) <= 100) {
    var %d = $puttok(%d,$iif($gettok(%d,$calc(%n * %m),32),0,1),$calc(%n * %m),32)
    inc %n
  }
  inc %m
}
echo -ag All Doors (Boolean): %d
var %n = 1
while (%n <= $findtok(%d,1,0,32)) {
  var %t = %t $findtok(%d,1,%n,32)
  inc %n
}
echo -ag Open Door Numbers: %t
```



## ML/I


```ML/I
MCSKIP "WITH" NL
"" 100 doors
MCINS %.
MCSKIP MT,<>
"" Doors represented by P1-P100, 0 is closed
MCPVAR 100
"" Set P variables to 0
MCDEF ZEROPS WITHS NL AS <MCSET T1=1
%L1.MCSET PT1=0
MCSET T1=T1+1
MCGO L1 UNLESS T1 EN 101
>
ZEROPS
"" Generate door state
MCDEF STATE WITHS () AS <MCSET T1=%A1.
MCGO L1 UNLESS T1 EN 0
closed<>MCGO L0
%L1.open>
"" Main macro - no arguments
"" T1 is pass number
"" T2 is door number
MCDEF DOORS WITHS NL
AS <MCSET T1=1
"" pass loop
%L1.MCGO L4 IF T1 GR 100
"" door loop
MCSET T2=T1
%L2.MCGO L3 IF T2 GR 100
MCSET PT2=1-PT2
MCSET T2=T2+T1
MCGO L2
%L3.MCSET T1=T1+1
MCGO L1
%L4."" now output the result
MCSET T1=1
%L5.door %T1. is STATE(%PT1.)
MCSET T1=T1+1
MCGO L5 UNLESS T1 GR 100
>
"" Do it
DOORS
```



## MMIX

See [[100 doors/MMIX]]

=={{header|Modula-2}}==
'''unoptimized'''

```modula2
MODULE Doors;
IMPORT InOut;

TYPE State = (Closed, Open);
TYPE List = ARRAY [1 .. 100] OF State;

VAR
  Doors: List;
  I, J:  CARDINAL;

BEGIN
  FOR I := 1 TO 100 DO
    FOR J := 1 TO 100 DO
      IF J MOD I = 0 THEN
        IF Doors[J] = Closed THEN
          Doors[J] := Open
        ELSE
          Doors[J] := Closed
        END
      END
    END
  END;

  FOR I := 1 TO 100 DO
    InOut.WriteCard(I, 3);
    InOut.WriteString(' is ');

    IF Doors[I] = Closed THEN
      InOut.WriteString('Closed.')
    ELSE
      InOut.WriteString('Open.')
    END;

    InOut.WriteLn
  END
END Doors.
```


'''optimized'''

```modula2
MODULE DoorsOpt;
IMPORT InOut;

TYPE State = (Closed, Open);
TYPE List = ARRAY [1 .. 100] OF State;

VAR
  Doors: List;
  I:  CARDINAL;

BEGIN
  FOR I := 1 TO 10 DO
    Doors[I*I] := Open
  END;

  FOR I := 1 TO 100 DO
    InOut.WriteCard(I, 3);
    InOut.WriteString(' is ');
    IF Doors[I] = Closed THEN
      InOut.WriteString('Closed.')
    ELSE
      InOut.WriteString('Open.')
    END;
    InOut.WriteLn
  END
END DoorsOpt.
```


=={{header|Modula-3}}==
'''unoptimized'''

```modula3
MODULE Doors EXPORTS Main;

IMPORT IO, Fmt;

TYPE State = {Closed, Open};
TYPE List = ARRAY [1..100] OF State;

VAR doors := List{State.Closed, ..};

BEGIN
  FOR i := 1 TO 100 DO
    FOR j := FIRST(doors) TO LAST(doors) DO
      IF j MOD i = 0 THEN
        IF doors[j] = State.Closed THEN
          doors[j] := State.Open;
        ELSE
          doors[j] := State.Closed;
        END;
      END;
    END;
  END;

  FOR i := FIRST(doors) TO LAST(doors) DO
    IO.Put(Fmt.Int(i) & " is ");
    IF doors[i] = State.Closed THEN
      IO.Put("Closed.\n");
    ELSE
      IO.Put("Open.\n");
    END;
  END;
END Doors.
```


'''optimized'''


```modula3
MODULE DoorsOpt EXPORTS Main;

IMPORT IO, Fmt;

TYPE State = {Closed, Open};
TYPE List = ARRAY [1..100] OF State;

VAR doors := List{State.Closed, ..};

BEGIN
  FOR i := 1 TO 10 DO
    doors[i * i] := State.Open;
  END;

  FOR i := FIRST(doors) TO LAST(doors) DO
    IO.Put(Fmt.Int(i) & " is ");
    IF doors[i] = State.Closed THEN
      IO.Put("Closed.\n");
    ELSE
      IO.Put("Open.\n");
    END;
  END;
END DoorsOpt.
```



## MontiLang


```MontiLang
101 var l .

for l 0 endfor
arr

0 var i .
for l
    i 1 + var i var j .
    j l < var pass .
    while pass
        get j not insert j .
        j i + var j
        l < var pass .
    endwhile
endfor
print /# show all doors #/

/# show only open doors #/
|| print .
0 var i .
for l
    get i
    if : i out | | out . . endif .
    i 1 + var i .
endfor

input . /# pause until ENTER key pressed #/
```



## MOO


```moo
is_open = make(100);
for pass in [1..100]
  for door in [pass..100]
    if (door % pass)
      continue;
    endif
    is_open[door] = !is_open[door];
  endfor
endfor

"output the result";
for door in [1..100]
  player:tell("door #", door, " is ", (is_open[door] ? "open" : "closed"), ".");
endfor
```



## MoonScript


```MoonScript
is_open = [false for door = 1,100]

for pass = 1,100
    for door = pass,100,pass
        is_open[door] = not is_open[door]

for i,v in ipairs is_open
    print "Door #{i}: " .. if v then 'open' else 'closed'
```



## MUMPS


```MUMPS
doors	new door,pass
	For door=1:1:100 Set door(door)=0
	For pass=1:1:100 For door=pass:pass:100 Set door(door)='door(door)
	For door=1:1:100 If door(door) Write !,"Door",$j(door,4)," is open"
	Write !,"All other doors are closed."
	Quit
Do doors
Door   1 is open
Door   4 is open
Door   9 is open
Door  16 is open
Door  25 is open
Door  36 is open
Door  49 is open
Door  64 is open
Door  81 is open
Door 100 is open
All other doors are closed.
```



## Myrddin


```myrddin

use std

const main = {
	var isopen	: bool[100]

	std.slfill(isopen[:], false)
	for var i = 0; i < isopen.len; i++
		for var j = i; j < isopen.len; j += i + 1
			isopen[j] = !isopen[j]
		;;
	;;

	for var i = 0; i < isopen.len; i++
		if isopen[i]
			std.put("door {} is open\n", i + 1)
		;;
	;;
}

```

{{out}}


```txt

door 1 is open
door 4 is open
door 9 is open
door 16 is open
door 25 is open
door 36 is open
door 49 is open
door 64 is open
door 81 is open
door 100 is open

```



## MySQL



```mysql

DROP PROCEDURE IF EXISTS one_hundred_doors;

DELIMITER |

CREATE PROCEDURE one_hundred_doors (n INT)
BEGIN
  DROP TEMPORARY TABLE IF EXISTS doors;
  CREATE TEMPORARY TABLE doors (
    id INTEGER NOT NULL,
    open BOOLEAN DEFAULT FALSE,
    PRIMARY KEY (id)
  );

  SET @i = 1;
  create_doors: LOOP
    INSERT INTO doors (id, open) values (@i, FALSE);
    SET @i = @i + 1;
    IF @i > n THEN
      LEAVE create_doors;
    END IF;
  END LOOP create_doors;

  SET @i = 1;
  toggle_doors: LOOP
    UPDATE doors SET open = NOT open WHERE MOD(id, @i) = 0;
    SET @i = @i + 1;
    IF @i > n THEN
      LEAVE toggle_doors;
    END IF;
  END LOOP toggle_doors;

  SELECT id FROM doors WHERE open;
END|

DELIMITER ;

CALL one_hundred_doors(100);

```


{{out}}


```txt

+-----+
| id  |
+-----+
|   1 |
|   4 |
|   9 |
|  16 |
|  25 |
|  36 |
|  49 |
|  64 |
|  81 |
| 100 |
+-----+
10 rows in set (0.02 sec)

```



## NetRexx

'''unoptimized'''

```netrexx
/* NetRexx */
options replace format comments java crossref symbols binary

True  = Rexx(1 == 1)
False = Rexx(\True)

doors = False

loop i_ = 1 to 100
  loop j_ = 1 to 100
    if 0 = (j_ // i_) then doors[j_] = \doors[j_]
    end j_
  end i_

loop d_ = 1 to 100
  if doors[d_] then  state = 'open'
  else  state = 'closed'

  say 'Door Nr.' Rexx(d_).right(4) 'is' state
  end d_
```


'''optimized''' (Based on the Java 'optimized' version)
{{trans|Java}}

```netrexx
/* NetRexx */
options replace format comments java crossref symbols binary

True  = (1 == 1)
False = \True

doors = boolean[100]

loop i_ = 0 to 9
  doors[(i_ + 1) * (i_ + 1) - 1] = True;
  end i_

loop i_ = 0 to 99
  if doors[i_] then  state = 'open'
  else  state = 'closed'

  say 'Door Nr.' Rexx(i_ + 1).right(4) 'is' state
  end i_
```


'''optimized 2''' (Based on the Java 'optimized 2' version)
{{trans|Java}}

```netrexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

resultstring = ''

loop i_ = 1 to 10
  resultstring = resultstring || 'Door Nr.' Rexx(i_ * i_).right(4) 'is open\n'
  end i_

say resultstring
```


'''optimized 3'''

```netrexx
/* NetRexx */

loop i = 1 to 10
   say 'Door Nr.' i * i 'is open.'
  end i
```



## NewLISP


```NewLisp
(define (status door-num)
    (let ((x (int (sqrt door-num))))
     (if
       (= (* x x) door-num) (string "Door " door-num " Open")
       (string "Door " door-num " Closed"))))

(dolist (n (map status (sequence 1 100)))
  (println n))

```


Not optimized:

```NewLISP

(set 'Doors (array 100))  ;; Default value: nil (Closed)

(for (x 0 99)
    (for (y x 99 (+ 1 x))
        (setf (Doors y) (not (Doors y)))))

(for (x 0 99)  ;; Display open doors
    (if (Doors x)
        (println (+ x 1) " : Open")))

```

Output:

```txt

1 : Open
4 : Open
9 : Open
16 : Open
25 : Open
36 : Open
49 : Open
64 : Open
81 : Open
100 : Open

```



## Nial


unoptimized solution (works with Q'Nial7):

Output of the boolean array showing the status of the doors. Truth values in Nial arrays are shown as <code>l</code>(true) and <code>o</code>(false):


```nial
     n:=100;reduce xor (count n eachright mod count n eachall<1)
looloooolooooooloooooooolooooooooooloooooooooooolooooooooooooooloooooooooooooooo

looooooooooooooooool
```


Indices of the open doors:


```nial
     true findall (n:=100;reduce xor (count n eachright mod count n eachall<1))+1
1 4 9 16 25 36 49 64 81 100
```


optimized solution:


```nial
     count 10 power 2
1 4 9 16 25 36 49 64 81 100
```



## Nim

unoptimized:

```Nim
from strutils import format

proc check_doors() =
  const n = 100
  var is_open : array[1..n, bool] # auto-initialized to false
  # pass over the doors n times
  for pass in 1..n:
    var i = pass
    while i <= n:
      is_open[i] = not is_open[i]
      i += pass
  # print the result
  for door in 1..n:
    echo format("door $1 is $2.", door, (if is_open[door]: "open" else: "closed"))

check_doors()
```


another:

```Nim
var isOpen: array[1..100, bool]

for pass in countup(1, 100):
  for door in countup(pass,100,pass):
    isOpen[door] = not isOpen[door]

for i in countup(1, 100):
  if isOpen[i]:
    echo("Door ",i," is open.")
```



## Oberon

[http://oberon07.com/ Oberon-07], by [http://people.inf.ethz.ch/wirth/index.html Niklaus Wirth].

```oberon
MODULE Doors;
  IMPORT Out;

  PROCEDURE Do*;  (* In Oberon an asterisk after an identifier is an export mark *)
    CONST N = 100; len = N + 1;
    VAR i, j: INTEGER;
      closed: ARRAY len OF BOOLEAN;  (* Arrays in Oberon always start with index 0; closed[0] is not used *)
  BEGIN
    FOR i := 1 TO N DO closed[i] := TRUE END;
    FOR i := 1 TO N DO
      j := 1;
      WHILE j < len DO
        IF j MOD i = 0 THEN closed[j] := ~closed[j] END; INC(j)  (* ~ = NOT *)
      END
    END;
    (* Print a state diagram of all doors *)
    FOR i := 1 TO N DO
      IF (i - 1) MOD 10 = 0 THEN Out.Ln END;
      IF closed[i] THEN Out.String("- ") ELSE Out.String("+ ") END
    END;  Out.Ln;
    (* Print the numbers of the open doors *)
    FOR i := 1 TO N DO
      IF ~closed[i] THEN Out.Int(i, 0); Out.Char(" ") END
    END;  Out.Ln
  END Do;

END Doors.
```

Execute: Doors.Do<br/>
{{out}}

```txt

+ – – + – – – – + –
– – – – – + – – – –
– – – – + – – – – –
– – – – – + – – – –
– – – – – – – – + –
– – – – – – – – – –
– – – + – – – – – –
– – – – – – – – – –
+ – – – – – – – – –
– – – – – – – – – +
1 4 9 16 25 36 49 64 81 100

```



## Objeck

'''optimized'''

```objeck

bundle Default {
  class Doors {
    function : Main(args : String[]) ~ Nil {
      doors := Bool->New[100];

      for(pass := 0; pass < 10; pass += 1;) {
        doors[(pass + 1) * (pass + 1) - 1] := true;
      };

      for(i := 0; i < 100; i += 1;) {
        IO.Console->GetInstance()->Print("Door #")->Print(i + 1)->Print(" is ");
        if(doors[i]) {
          "open."->PrintLine();
        }
        else {
          "closed."->PrintLine();
        };
      };
    }
  }
}

```



=={{header|Objective-C}}==
'''A basic implementation in Objective-C:'''

This is a very basic Objective-C sample that shows the usage of standard types and classes such as NSInteger and NSMutableArray.

It uses modern Objective-C syntax such as literals, blocks, and a compiler module import statement.
<lang Objective-C>
@import Foundation;

int main(int argc, const char * argv[]) {
    @autoreleasepool {

        // Create a mutable array
        NSMutableArray *doorArray = [@[] mutableCopy];

        // Fill the doorArray with 100 closed doors
        for (NSInteger i = 0; i < 100; ++i) {
            doorArray[i] = @NO;
        }

        // Do the 100 passes
        for (NSInteger pass = 0; pass < 100; ++pass) {
            for (NSInteger door = pass; door < 100; door += pass+1) {
                doorArray[door] = [doorArray[door]  isEqual: @YES] ? @NO : @YES;
            }
        }

        // Print the results
        [doorArray enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            if ([obj isEqual: @YES]) {
                NSLog(@"Door number %lu is open", idx + 1);
            }
        }];
    }
}

```

'''A more typical implementation in Objective-C:'''

This example is more along the lines of what typical Objective-C program would look like.

Language features used include:
* MVC design pattern with separate classes for the data model, user interface, and controller (Here, main steps in to represent the controller class.)
* Class category to extend the standard NSMutableArray class to add doors without a subclass
* Class inheritance in the DoorViewClass when subclassing NSObject
* Pragma mark statements for IDE navigation in Xcode

In a real world program classes are normally separated into different files.

<lang Objective-C>
@import Foundation;

#pragma mark - Classes
////////////////////////////////////////////////////
// Model class header - A we are using a category to add a method to MSMutableArray
@interface NSMutableArray (DoorModelExtension)

- (void)setNumberOfDoors:(NSUInteger)doors;

@end

// Model class implementation
@implementation NSMutableArray (DoorModelExtension)

- (void)setNumberOfDoors:(NSUInteger)doors {
    // Fill the doorArray with 100 closed doors
    for (NSInteger i = 0; i < doors; ++i) {
        self[i] = @NO;
    }
}
@end
////////////////////////////////////////////////////

// View class header - A simple class to handle printing our values
@interface DoorViewClass : NSObject

- (void)printResultsOfDoorTask:(NSMutableArray *)doors;

@end

// View class implementation
@implementation DoorViewClass

- (void)printResultsOfDoorTask:(NSMutableArray *)doors {

    // Print the results, using an enumeration block for easy index tracking
    [doors enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        if ([obj isEqual: @YES]) {
            NSLog(@"Door number %lu is open", idx + 1);
        }
    }];
}

@end
////////////////////////////////////////////////////

#pragma mark - main
// With our classes set we can use them from our controller, in this case main
int main(int argc, const char * argv[]) {

    // Init our classes
    NSMutableArray *doorArray = [NSMutableArray array];
    DoorViewClass *doorView = [DoorViewClass new];

    // Use our class category to add the doors
    [doorArray setNumberOfDoors:100];

    // Do the 100 passes
    for (NSUInteger pass = 0; pass < 100; ++pass) {
        for (NSUInteger door = pass; door < 100; door += pass+1) {
            doorArray[door] = [doorArray[door]  isEqual: @YES] ? @NO : @YES;
        }
    }

    // Print the results
    [doorView printResultsOfDoorTask:doorArray];

}

```



## OCaml

'''unoptimized'''

```ocaml
let max_doors = 100

let show_doors =
  Array.iteri (fun i x -> Printf.printf "Door %d is %s\n" (i+1)
                                        (if x then "open" else "closed"))

let flip_doors doors =
  for i = 1 to max_doors do
    let rec flip idx =
      if idx < max_doors then begin
        doors.(idx) <- not doors.(idx);
        flip (idx + i)
      end
    in flip (i - 1)
  done;
  doors

let () =
  show_doors (flip_doors (Array.make max_doors false))
```


'''optimized'''

```ocaml
let optimised_flip_doors doors =
  for i = 1 to int_of_float (sqrt (float_of_int max_doors)) do
    doors.(i*i - 1) <- true
  done;
  doors

let () =
  show_doors (optimised_flip_doors (Array.make max_doors false))
```


This variant is more '''functional style''' (loops are recursions), unoptimized, and we do rather 100 passes on first element, then 100 * second, to avoid mutable data structures and many intermediate lists.

```ocaml
type door = Open | Closed    (* human readable code *)

let flipdoor = function Open -> Closed | Closed -> Open

let string_of_door =
  function Open -> "is open." | Closed -> "is closed."

let printdoors ls =
  let f i d = Printf.printf "Door %i %s\n" (i + 1) (string_of_door d)
  in List.iteri f ls

let outerlim = 100
let innerlim = 100

let rec outer cnt accu =
  let rec inner i door = match i > innerlim with (* define inner loop *)
    | true  -> door
    | false -> inner (i + 1) (if (cnt mod i) = 0 then flipdoor door else door)
  in (* define and do outer loop *)
  match cnt > outerlim with
  | true  -> List.rev accu
  | false -> outer  (cnt + 1)  (inner 1 Closed :: accu) (* generate new entries with inner *)

let () = printdoors (outer 1 [])
```



## Octave


```octave
doors = false(100,1);
for i = 1:100
  for j = i:i:100
    doors(j) = !doors(j);
  endfor
endfor
for i = 1:100
  if ( doors(i) )
    s = "open";
  else
    s = "closed";
  endif
  printf("%d %s\n", i, s);
endfor
```


See also the solutions in Matlab. They will work in Octave, too.


## Oforth



```Oforth
: doors
| i j l |
   100 false Array newWith dup ->l
   100 loop: i [
      i 100 i step: j [ l put ( j , j l at not ) ]
      ]
;

```



## Ol


```scheme

(define (flip doors every)
   (map (lambda (door num)
            (mod (+ door (if (eq? (mod num every) 0) 1 0)) 2))
      doors
      (iota (length doors) 1)))

(define doors
   (let loop ((doors (repeat 0 100)) (n 1))
      (if (eq? n 100)
         doors
         (loop (flip doors n) (+ n 1)))))

(print "100th doors: " doors)

```


Output:

```txt

100th doors: (1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

```



## Onyx



```Onyx
$Door dict def
1 1 100 {Door exch false put} for
$Toggle {dup Door exch get not Door up put} def
$EveryNthDoor {dup 100 {Toggle} for} def
$Run {1 1 100 {EveryNthDoor} for} def
$ShowDoor {dup `Door no. ' exch cvs cat ` is ' cat
  exch Door exch get {`open.\n'}{`shut.\n'} ifelse cat
  print flush} def
Run 1 1 100 {ShowDoor} for
```


{{out}}

```txt
Door no. 1 is open.
Door no. 2 is shut.
Door no. 3 is shut.
Door no. 4 is open.
Door no. 5 is shut.
Door no. 6 is shut.
Door no. 7 is shut.
Door no. 8 is shut.
Door no. 9 is open.
Door no. 10 is shut.
Door no. 11 is shut.
Door no. 12 is shut.
Door no. 13 is shut.
Door no. 14 is shut.
Door no. 15 is shut.
Door no. 16 is open.
Door no. 17 is shut.
Door no. 18 is shut.
Door no. 19 is shut.
Door no. 20 is shut.
Door no. 21 is shut.
Door no. 22 is shut.
Door no. 23 is shut.
Door no. 24 is shut.
Door no. 25 is open.
Door no. 26 is shut.
Door no. 27 is shut.
Door no. 28 is shut.
Door no. 29 is shut.
Door no. 30 is shut.
Door no. 31 is shut.
Door no. 32 is shut.
Door no. 33 is shut.
Door no. 34 is shut.
Door no. 35 is shut.
Door no. 36 is open.
Door no. 37 is shut.
Door no. 38 is shut.
Door no. 39 is shut.
Door no. 40 is shut.
Door no. 41 is shut.
Door no. 42 is shut.
Door no. 43 is shut.
Door no. 44 is shut.
Door no. 45 is shut.
Door no. 46 is shut.
Door no. 47 is shut.
Door no. 48 is shut.
Door no. 49 is open.
Door no. 50 is shut.
Door no. 51 is shut.
Door no. 52 is shut.
Door no. 53 is shut.
Door no. 54 is shut.
Door no. 55 is shut.
Door no. 56 is shut.
Door no. 57 is shut.
Door no. 58 is shut.
Door no. 59 is shut.
Door no. 60 is shut.
Door no. 61 is shut.
Door no. 62 is shut.
Door no. 63 is shut.
Door no. 64 is open.
Door no. 65 is shut.
Door no. 66 is shut.
Door no. 67 is shut.
Door no. 68 is shut.
Door no. 69 is shut.
Door no. 70 is shut.
Door no. 71 is shut.
Door no. 72 is shut.
Door no. 73 is shut.
Door no. 74 is shut.
Door no. 75 is shut.
Door no. 76 is shut.
Door no. 77 is shut.
Door no. 78 is shut.
Door no. 79 is shut.
Door no. 80 is shut.
Door no. 81 is open.
Door no. 82 is shut.
Door no. 83 is shut.
Door no. 84 is shut.
Door no. 85 is shut.
Door no. 86 is shut.
Door no. 87 is shut.
Door no. 88 is shut.
Door no. 89 is shut.
Door no. 90 is shut.
Door no. 91 is shut.
Door no. 92 is shut.
Door no. 93 is shut.
Door no. 94 is shut.
Door no. 95 is shut.
Door no. 96 is shut.
Door no. 97 is shut.
Door no. 98 is shut.
Door no. 99 is shut.
Door no. 100 is open.
```



## ooRexx


```ooRexx
doors = .array~new(100)    -- array containing all of the doors
do i = 1 to doors~size     -- initialize with a collection of closed doors
   doors[i] = .door~new(i)
end

do inc = 1 to doors~size
  do d = inc to doors~size by inc
    doors[d]~toggle
  end
end
say "The open doors after 100 passes:"
do door over doors
  if door~isopen then say door
end

::class door           -- simple class to represent a door
::method init          -- initialize an instance of a door
  expose id state      -- instance variables of a door
  use strict arg id    -- set the id
  state = .false       -- initial state is closed

::method toggle        -- toggle the state of the door
  expose state
  state = \state

::method isopen        -- test if the door is open
  expose state
  return state

::method string        -- return a string value for a door
  expose state id
  if state then return "Door" id "is open"
  else return "Door" id "is closed"

::method state         -- return door state as a descriptive string
  expose state
  if state then return "open"
  else return "closed"
```


The two programs in the Rexx section run under ooRexx when '#' is replaced by, e.g., 'dd'.


'#' is not supported by ooRexx as part of or as a symbol.
Neither are @ and $.


## OpenEdge/Progress


```OpenEdge/Progress
DEFINE VARIABLE lopen   AS LOGICAL     NO-UNDO EXTENT 100.
DEFINE VARIABLE idoor   AS INTEGER     NO-UNDO.
DEFINE VARIABLE ipass   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cresult AS CHARACTER   NO-UNDO.

DO ipass = 1 TO 100:
   idoor = 0.
   DO WHILE idoor <= 100:
      idoor = idoor + ipass.
      IF idoor <= 100 THEN
         lopen[ idoor ] = NOT lopen[ idoor ].
   END.
END.

DO idoor = 1 TO 100:
   cresult = cresult + STRING( lopen[ idoor ], "1  /0  " ).
   IF idoor MODULO 10 = 0 THEN
      cresult = cresult + "~r":U.
END.

MESSAGE cresult VIEW-AS ALERT-BOX.

```



## OxygenBasic


```txt

def    doors 100
int    door[doors],i ,j, c
string cr,tab,pr
'
for i=1 to doors
  for j=i to doors step i
    door[j]=1-door[j]
    if door[j] then c++ else c--
  next
next
'
cr=chr(13) chr(10)
pr="Doors Open: " c cr cr
'
for i=1 to doors
   if door[i] then pr+=i cr
next
print pr

```



## Oz


```oz
declare
  NumDoors = 100
  NumPasses = 100

  fun {NewDoor} closed end

  fun {Toggle Door}
     case Door of closed then open
     [] open then closed
     end
  end

  fun {Pass Doors I}
     {List.mapInd Doors
      fun {$ Index Door}
         if Index mod I == 0 then {Toggle Door}
         else Door
         end
      end}
  end

  Doors0 = {MakeList NumDoors}
  {ForAll Doors0 NewDoor}

  DoorsN = {FoldL {List.number 1 NumPasses 1} Pass Doors0}
in
  %% print open doors
  {List.forAllInd DoorsN
   proc {$ Index Door}
      if Door == open then
	 {System.showInfo "Door "#Index#" is open."}
      end
   end
  }
```


Output:

```txt

Door 1 is open.
Door 4 is open.
Door 9 is open.
Door 16 is open.
Door 25 is open.
Door 36 is open.
Door 49 is open.
Door 64 is open.
Door 81 is open.
Door 100 is open.

```



## PARI/GP

'''Unoptimized version.'''

```parigp

v=vector(d=100);/*set 100 closed doors*/
for(i=1,d,forstep(j=i,d,i,v[j]=1-v[j]));
for(i=1,d,if(v[i],print("Door ",i," is open.")))

```

'''Optimized version.'''

```parigp
for(n=1,sqrt(100),print("Door ",n^2," is open."))
```


'''Unoptimized version.'''

```pari/gp

doors =vector(100);
print("open doors are : ");
for(i=1,100,for(j=i,100,doors[j]=!doors[j];j +=i-1))
for(k=1,100,if(doors[k]==1,print1(" ",k)))

```

Output:

```txt
Open doors are:
 1 4 9 16 25 36 49 64 81 100
```



## Pascal


```pascal
Program OneHundredDoors;

var
   doors : Array[1..100] of Boolean;
   i, j	 : Integer;

begin
   for i := 1 to 100 do
      doors[i] := False;
   for i := 1 to 100 do begin
      j := i;
      while j <= 100 do begin
	 doors[j] := not doors[j];
	 j := j + i
      end
   end;
   for i := 1 to 100 do begin
      Write(i, ' ');
      if doors[i] then
	 WriteLn('open')
      else
	 WriteLn('closed');
   end
end.
```


'''Optimized version.'''


```pascal
program OneHundredDoors;

{$APPTYPE CONSOLE}

uses
  math, sysutils;

var
   AOpendoors  : String;
   ACloseDoors : String;
   i	       : Integer;

begin
   for i := 1 to 100 do
   begin
      if (sqrt(i) = floor(sqrt(i))) then
        AOpenDoors := AOpenDoors + IntToStr(i) + ';'
      else
        ACloseDoors := ACloseDoors + IntToStr(i) +';';
   end;

   WriteLn('Open doors: ' + AOpenDoors);
   WriteLn('Close doors: ' + ACloseDoors);
end.
```



## Perl

'''unoptimized'''
{{works with|Perl|5.x}}

```perl
my @doors;
for my $pass (1 .. 100) {
    for (1 .. 100) {
        if (0 == $_ % $pass) {
            $doors[$_] = not $doors[$_];
        };
    };
};

print "Door $_ is ", $doors[$_] ? "open" : "closed", "\n" for 1 .. 100;
```


'''semi-optimized'''
{{works with|Perl|5.x}}
This version flips doors, but doesn't visit (iterate over) doors that aren't toggled.  Note: I represent open doors as 0 and closed as 1 just for preference.  (When I print it as a bit vector, 0 looks more like an open door to me.)

```perl

#!/usr/bin/perl
use strict;
use warnings;

my @doors = (1) x 100;
for my $N (1 .. 100) {
   $doors[$_]=1-$doors[$_] for map { $_*$N - 1 } 1 .. int(100/$N);
}
print join("\n", map { "Door $_ is Open" } grep { ! $doors[$_-1] } 1 .. 100), "\n";
print "The rest are closed\n";

```


'''optimized'''
{{works with|Perl|5.x}}

```perl
print "Door $_ is open\n" for map $_**2, 1 .. 10;
```


```perl
print "Door $_ is ", qw"closed open"[int sqrt == sqrt], "\n" for 1..100;
```


```perl
while( ++$i <= 100 )
{
    $root = sqrt($i);
    if ( int( $root ) == $root )
    {
        print "Door $i is open\n";
    }
    else
    {
        print "Door $i is closed\n";
    }
}
```



## Perl5i


```Perl5i

use perl5i::2;

package doors {

  use perl5i::2;
  use Const::Fast;

  const my $OPEN   => 1;
  const my $CLOSED => 0;

  # ----------------------------------------
  # Constructor: door->new( @args );
  # input: N - how many doors?
  # returns: door object
  #
  method new($class: @args ) {
    my $self = bless {}, $class;
    $self->_init( @args );
    return $self;
  }

  # ----------------------------------------
  # class initializer.
  # input: how many doors?
  # sets N, creates N+1 doors ( door zero is not used ).
  #
  method _init( $N ) {
    $self->{N} = $N;
    $self->{doors} = [ ($CLOSED) x ($N+1) ];
  }

  # ----------------------------------------
  # $self->toggle( $door_number );
  # input: number of door to toggle.
  # OPEN a CLOSED door; CLOSE an OPEN  door.
  #
  method toggle( $which ) {
    $self->{doors}[$which] = ( $self->{doors}[$which] == $OPEN
                               ? $CLOSED
                               : $OPEN
        		     );
  }

  # ----------------------------------------
  # $self->toggle_n( $cycle );
  # input: number.
  # Toggle doors 0, $cycle, 2 * $cycle, 3 * $cycle, .. $self->{N}
  #
  method toggle_n( $n ) {
    $self->toggle($_)
      for map { $n * $_ }
          ( 1 .. int( $self->{N} / $n) );

  }

  # ----------------------------------------
  # $self->toggle_all();
  # Toggle every door, then every other door, every third door, ...
  #
  method toggle_all() {
    $self->toggle_n( $_ ) for ( 1 .. $self->{N} );
  }


  # ----------------------------------------
  # $self->print_open();
  # Print list of which doors are open.
  #
  method print_open() {
    say join ', ', grep { $self->{doors}[$_] == $OPEN } ( 1 ... $self->{N} );
  }
}

# ----------------------------------------------------------------------
# Main Thread
#
my $doors = doors->new(100);
$doors->toggle_all();
$doors->print_open();

```



## Perl 6


### unoptimized

{{works with|Rakudo|2015.09"}}

```perl6
my @doors = False xx 101;

(.=not for @doors[0, $_ ... 100]) for 1..100;

say "Door $_ is ", <closed open>[ @doors[$_] ] for 1..100;
```



### optimized



```perl6
say "Door $_ is open" for map {$^n ** 2}, 1..10;
```


===Here's a version using the cross meta-operator instead of a map:===


```perl6
 say "Door $_ is open" for 1..10 X** 2;
```


This one prints both opened and closed doors:


```perl6
say "Door $_ is ", <closed open>[.sqrt == .sqrt.floor] for 1..100;
```


===verbose version, but uses ordinary components===
{{works with|Rakudo|2016.07   Tom Legrady}}


```perl6

sub  output( @arr, $max ) {
    my $output = 1;
    for 1..^$max -> $index {
	if @arr[$index] {
	    printf "%4d", $index;
	    say '' if $output++ %%  10;
	}
    }
    say '';
}

sub MAIN ( Int :$doors = 100 ) {
    my $doorcount = $doors + 1;
    my @door[$doorcount] = 0 xx ($doorcount);

    INDEX:
    for 1...^$doorcount -> $index {
        # flip door $index & its multiples, up to last door.
        #
	for ($index, * + $index ... *)[^$doors] -> $multiple {
	    next INDEX if $multiple > $doors;
	    @door[$multiple] =  @door[$multiple] ?? 0 !! 1;
	}
    }
    output @door, $doors+1;
}

```

{{out}}

```txt

$ ./100_doors.pl6 -doors=100
   1   4   9  16  25  36  49  64  81

```



## Phix


### unoptimised


```Phix
sequence doors = repeat(false,100)

for i=1 to 100 do
    for j=i to 100 by i do
        doors[j] = not doors[j]
    end for
end for

for i=1 to 100 do
    if doors[i] == true then
        printf(1,"Door #%d is open.\n", i)
    end if
end for
```

{{out}}

```txt

Door #1 is open.
Door #4 is open.
Door #9 is open.
Door #16 is open.
Door #25 is open.
Door #36 is open.
Door #49 is open.
Door #64 is open.
Door #81 is open.
Door #100 is open.

```



### optimised


```Phix
function doors(integer n)
-- returns the perfect squares<=n
integer door = 1, step = 1
sequence res = {}
    while door<=n do
        res &= door
        step += 2
        door += step
    end while
    return res
end function

?doors(100)
```

{{out}}

```txt

{1,4,9,16,25,36,49,64,81,100}

```



## PHL



### unoptimized



```phl
module doors;

extern printf;

@Integer main [
	@Array<@Boolean> doors = new @Array<@Boolean>.init(100);
	var i = 1;
	while (i <= 100) {
		var j = i-1;
		while (j < 100) {
			doors.set(j, doors.get(j)::not);
			j = j + i;
		}
		i = i::inc;
	}
	i = 0;
	while (i < 100) {
		printf("%i %s\n", i+1, iif(doors.get(i), "open", "closed"));
		i = i::inc;
	}
	return 0;
]
```



### optimized


{{trans|C#}}


```phl
module var;

extern printf;

@Integer main [
	var door = 1;
	var incrementer = 0;
	var current = 1;
        while (current <= 100)
        {
		printf("Door %i ", current);
		if (current == door)
		{
			printf("open\n");
			incrementer = incrementer::inc;
			door = door + 2 * incrementer + 1;
		}
		else
			printf("closed\n");

		current = current + 1;

        }

	return 0;
]
```



## PHP

See: [http://www.thomporter.com/100doors.php Demo]
'''optimized'''

```php
<?php
for ($i = 1; $i <= 100; $i++) {
	$root = sqrt($i);
	$state = ($root == ceil($root)) ? 'open' : 'closed';
	echo "Door {$i}: {$state}\n";
}
?>
```


'''unoptimized'''

```php
<?php
$doors = array_fill(1, 100, false);
for ($pass = 1; $pass <= 100; ++$pass) {
	for ($nr = 1; $nr <= 100; ++$nr) {
		if ($nr % $pass == 0) {
			$doors[$nr] = !$doors[$nr];
		}
	}
}
for ($nr = 1; $nr <= 100; ++$nr)
	printf("Door %d: %s\n", $nr, ($doors[$nr])?'open':'closed');
?>
```



## Picat

Non-optimized:

```Picat
doors(N) =>
   Doors = new_array(N),
   foreach(I in 1..N) Doors[I] := 0 end,
   foreach(I in 1..N)
     foreach(J in I..I..N)
        Doors[J] := 1^Doors[J]
     end,
     if N <= 10 then
        print_open(Doors)
     end
   end,
   println(Doors),
   print_open(Doors),
   nl.

print_open(Doors) => println([I : I in 1..Doors.length, Doors[I] == 1]).

```


optimized version 1:

```Picat
doors_opt(N) =>
  foreach(I in 1..N)
     Root = sqrt(I),
     println([I, cond(Root == 1.0*round(Root), open, closed)])
  end,
  nl.

```


optimized version 2:

```Picat
doors_opt2(N) =>
  println([I**2 : I in 1..N, I**2 <= N]).

```



## PicoLisp

unoptimized

```PicoLisp
(let Doors (need 100)
   (for I 100
      (for (D (nth Doors I)  D  (cdr (nth D I)))
         (set D (not (car D))) ) )
   (println Doors) )
```

optimized

```PicoLisp
(let Doors (need 100)
   (for I (sqrt 100)
      (set (nth Doors (* I I)) T) )
   (println Doors) )
```

Output in both cases:

```txt
(T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL
 NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL
 NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T
 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL N
IL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)
```


With formatting:

```PicoLisp
(let Doors (need 100)
   (for I (sqrt 100)
      (set (nth Doors (* I I)) T) )
   (make
      (for (N . D) Doors
         (when D (link N)) ) ) )
```

Output:

```txt
(1 4 9 16 25 36 49 64 81 100)
```



## Piet

[http://www.toothycat.net/~sham/piet/100doors.png image]


## Pike


```pike
array onehundreddoors()
{
    array doors = allocate(100);
    foreach(doors; int i;)
        for(int j=i; j<100; j+=i+1)
            doors[j] = !doors[j];
    return doors;
}
```

optimized version:

```pike
array doors = map(enumerate(100,1,1), lambda(int x)
                                      {
                                          return sqrt((float)x)%1 == 0.0;
                                      });
```



```pike
write("%{%d %d %d %d %d %d %d %d %d %d\n%}\n", doors/10)
```

output:
 1 0 0 1 0 0 0 0 1 0
 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 1 0 0 0 0 0
 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 1 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 1 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 1


## PL/I


```pli

declare door(100) bit (1) aligned;
declare closed bit (1) static initial ('0'b),
        open   bit (1) static initial ('1'b);
declare (i, inc) fixed binary;

door = closed;
inc = 1;
do until (inc >= 100);
   do i = inc to 100 by inc;
      door(i) = ^door(i); /* close door if open; open it if closed. */
   end;
   inc = inc+1;
end;

do i = 1 to 100;
   put skip edit ('Door ', trim(i), ' is ') (a);
   if door(i) then put edit (' open.') (a);
   else put edit (' closed.') (a);
end;

```



## PL/SQL

'''Unoptimized'''


```plsql

DECLARE
  TYPE doorsarray IS VARRAY(100) OF BOOLEAN;
  doors doorsarray := doorsarray();
BEGIN

doors.EXTEND(100);  --ACCOMMODATE 100 DOORS

FOR i IN 1 .. doors.COUNT  --MAKE ALL 100 DOORS FALSE TO INITIALISE
  LOOP
     doors(i) := FALSE;
  END LOOP;

FOR j IN 1 .. 100 --ITERATE THRU USING MOD LOGIC AND FLIP THE DOOR RIGHT OPEN OR CLOSE
 LOOP
      FOR k IN 1 .. 100
        LOOP
                  IF MOD(k,j)=0 THEN
                     doors(k) := NOT doors(k);
                  END IF;
        END LOOP;
 END LOOP;

FOR l IN 1 .. doors.COUNT  --PRINT THE STATUS IF ALL 100 DOORS AFTER ALL ITERATION
  LOOP
       DBMS_OUTPUT.PUT_LINE('DOOR '||l||' IS -->> '||CASE WHEN SYS.DBMS_SQLTCB_INTERNAL.I_CONVERT_FROM_BOOLEAN(doors(l)) = 'TRUE'
                                                                THEN 'OPEN'
                                                              ELSE 'CLOSED'
                                                        END);
  END LOOP;

END;

```


## Pony


'''Combined Optimized and Unoptimized'''

Probably also rather pointless in its use of actors, but, after all, they're cheap.

```pony

actor Toggler
    let doors:Array[Bool]
    let env: Env
    new create(count:USize,_env:Env) =>
        var i:USize=0
        doors=Array[Bool](count)
        env=_env
        while doors.size() < count do
            doors.push(false)
        end
    be togglin(interval : USize)=>
        var i:USize=0
        try
            while i < doors.size() do
                doors.update(i,not doors(i)?)?
                i=i+interval
            end
        else
            env.out.print("Errored while togglin'!")
        end
    be printn(onlyOpen:Bool)=>
        try
            for i in doors.keys() do
                if onlyOpen and not doors(i)? then
                    continue
                end
                env.out.print("Door " + i.string() + " is " +
                    if doors(i)? then
                        "Open"
                    else
                        "closed"
                    end)
            end
        else
            env.out.print("Error!")
        end
        true

actor OptimizedToggler
    let doors:Array[Bool]
    let env:Env
    new create(count:USize,_env:Env)=>
        env=_env
        doors=Array[Bool](count)
        while doors.size()<count do
            doors.push(false)
        end
    be togglin()=>
        var i:USize=0
        if alreadydone then
            return
        end
        try
            doors.update(0,true)?
            doors.update(1,true)?
            while i < doors.size() do
                i=i+1
                let z=i*i
                let x=z*z
                if z > doors.size() then
                    break
                else
                    doors.update(z,true)?
                end
                if x < doors.size() then
                    doors.update(x,true)?
                end
            end
        end
    be printn(onlyOpen:Bool)=>
        try
            for i in doors.keys() do
                if onlyOpen and not doors(i)? then
                    continue
                end
                env.out.print("Door " + i.string() + " is " +
                    if doors(i)? then
                        "Open"
                    else
                        "closed"
                    end)
            end
        else
            env.out.print("Error!")
        end
        true
actor Main
    new create(env:Env)=>
        var count: USize =100
        try
            let index=env.args.find("-n",0,0,{(l,r)=>l==r})?
            try
            match env.args(index+1)?.read_int[USize]()?
                | (let x:USize, _)=>count=x
                end
            else
                env.out.print("You either neglected to provide an argument after -n or that argument was not an integer greater than zero.")
                return
            end
        end
        if env.args.contains("optimized",{(l,r)=>r==l}) then
            let toggler=OptimizedToggler(count,env)
            var i:USize = 1
            toggler.togglin()
            toggler.printn(env.args.contains("onlyopen", {(l,r)=>l==r}))
        else
            let toggler=Toggler(count,env)
            var i:USize = 1
            while i < count do
                toggler.togglin(i)
                i=i+1
            end
            toggler.printn(env.args.contains("onlyopen", {(l,r)=>l==r}))
        end

```



## Pop11

'''unoptimized'''

```pop11
lvars i;
lvars doors = {% for i from 1 to 100 do false endfor %};
for i from 1 to 100 do
   for j from i by i to 100 do
      not(doors(j)) -> doors(j);
   endfor;
endfor;
;;; Print state
for i from 1 to 100 do
   printf('Door ' >< i >< ' is ' ><
            if doors(i) then 'open' else 'closed' endif, '%s\n');
endfor;
```


'''optimized'''

```pop11
for i to 100 do
    lvars root = sqrt(i);
    i; if root = round(root) then ' open' ><; else ' closed' ><; endif; =>
endfor;
```



## PostScript

Bruteforce:
```PostScript
/doors [ 100 { false } repeat ] def

1 1 100 { dup 1 sub exch 99 {
        dup doors exch get not doors 3 1 roll put
} for } for
doors pstack
```
Shows: <lang>[true false false true false false false false true false ...<90 doors later>... true]
```



## Potion


```Potion
square=1, i=3
1 to 100(door):
  if (door == square):
    ("door", door, "is open") say
    square += i
    i += 2.
.
```



## PowerShell


### unoptimized


```powershell
$doors = @(0..99)
for($i=0; $i -lt 100; $i++) {
  $doors[$i] = 0  # start with all doors closed
}
for($i=0; $i -lt 100; $i++) {
  $step = $i + 1
  for($j=$i; $j -lt 100; $j = $j + $step) {
    $doors[$j] = $doors[$j] -bxor 1
  }
}
foreach($doornum in 1..100) {
  if($doors[($doornum-1)] -eq $true) {"$doornum open"}
  else {"$doornum closed"}
}
```


### Alternative Method


```powershell
function Get-DoorState($NumberOfDoors)
{
   begin
   {
      $Doors = @()
      $Multiple = 1
   }

   process
   {
      for ($i = 1; $i -le $NumberOfDoors; $i++)
      {
         $Door = [pscustomobject]@{
                    Name = $i
                    Open = $false
                 }

         $Doors += $Door
      }

      While ($Multiple -le $NumberOfDoors)
      {
	 Foreach ($Door in $Doors)
	 {
	    if ($Door.name % $Multiple -eq 0)
               {
	          If ($Door.open -eq $False){$Door.open = $True}
	          Else {$Door.open = $False}
	       }
	 }

         $Multiple++
      }
    }

    end {$Doors}
}
```


### unoptimized Pipeline


```powershell
$doors = 1..100 | ForEach-Object {0}
1..100 | ForEach-Object { $a=$_;1..100 | Where-Object { -not ( $_ % $a )  } | ForEach-Object { $doors[$_-1] = $doors[$_-1] -bxor 1 }; if ( $doors[$a-1] ) { "door opened" } else { "door closed" } }

```


### unoptimized Pipeline 2


```powershell
$doors = 1..100 | ForEach-Object {0}
$visited = 1..100
1..100 | ForEach-Object { $a=$_;$visited[0..([math]::floor(100/$a)-1)] | Where-Object { -not ( $_ % $a )  } | ForEach-Object { $doors[$_-1] = $doors[$_-1] -bxor 1;$visited[$_/$a-1]+=($_/$a) }; if ( $doors[$a-1] ) { "door opened" } else { "door closed" } }

```

===unoptimized Pipeline 3 (dynamically build pipeline)===

```powershell
1..100|foreach-object {$pipe += "toggle $_ |"} -begin {$pipe=""}
filter toggle($pass) {$_.door = $_.door -xor !($_.index % $pass);$_}
invoke-expression "1..100| foreach-object {@{index=`$_;door=`$false}} | $pipe  out-host"

```



### Using Powershell Workflow for Parallelism


```powershell


Workflow Calc-Doors {
    Foreach –parallel ($number in 1..100) {
        "Door " + $number.ToString("0000") + ": " + @{$true="Closed";$false="Open"}[([Math]::pow($number, 0.5)%1) -ne 0]
    }
}
Calc-Doors | sort


```



###  optimized


```powershell

1..10|%{"Door "+ $_*$_ + " is open"}

```



## ProDOS

Uses math module.

```ProDOS
enableextensions
enabledelayedexpansion
editvar /newvar /value=0 /title=closed
editvar /newvar /value=1 /title=open
editvar /newvar /range=1-100 /increment=1 /from=2
editvar /newvar /value=2 /title=next
:doors
for /alloccurrences (!next!-!102!) do editvar /modify /value=-open-
editvar /modify /value=-next-=+1
if -next- /hasvalue=100 goto :cont else goto :doors
:cont
printline !1!-!102!
stoptask
```



## Prolog


### unoptimized


Declarative:


```Prolog
main :-
    forall(between(1,100,Door), ignore(display(Door))).

% show output if door is open after the 100th pass
display(Door) :-
    status(Door, 100, open),
    format("Door ~d is open~n", [Door]).

% true if Door has Status after Pass is done
status(Door, Pass, Status) :-
    Pass > 0,
    Remainder is Door mod Pass,
    toggle(Remainder, OldStatus, Status),
    OldPass is Pass - 1,
    status(Door, OldPass, OldStatus).
status(_Door, 0, closed).

toggle(Remainder, Status, Status) :-
    Remainder > 0.
toggle(0, open, closed).
toggle(0, closed, open).

```


Doors as a list:


```Prolog
doors_unoptimized(N) :-
	length(L, N),
	maplist(init, L),
	doors(N, N, L, L1),
	affiche(N, L1).

init(close).

doors(Max, 1, L, L1) :-
	!,
       inverse(1, 1, Max, L, L1).

doors(Max, N, L, L1) :-
	N1 is N - 1,
	doors(Max, N1, L, L2),
	inverse(N, 1, Max, L2, L1).


inverse(N, Max, Max, [V], [V1]) :-
	!,
	0 =:= Max mod N -> inverse(V, V1); V1 = V.

inverse(N, M, Max, [V|T], [V1|T1]) :-
	M1 is M+1,
	inverse(N, M1, Max, T, T1),
	(   0 =:= M mod N -> inverse(V, V1); V1 = V).


inverse(open, close).
inverse(close, open).

affiche(N, L) :-
	forall(between(1, N, I),
	       (   nth1(I, L, open) -> format('Door ~w is open.~n', [I]); true)).

```


Using dynamic-rules. Tried to be ISO:


```prolog
doors(Num, Passes) :-
    forall(( everyNth(1,Passes,1,Pass)
           , forall((everyNth(Pass,Num,Pass,Door), toggle(Door)))
           ))
  , show(Num)
  .


toggle(Door) :-
    Opened = opened(Door)
  , ( clause(Opened,_) -> retract(Opened)
                        ; asserta(Opened)
    ).


show(Num) :-
    forall(( between(1,Num,Door)
           , (opened(Door) -> State = opened ; State = closed)
           , write(Door), write(' '), write(State), nl
           )).


% utils
forall(X) :- findall(_, X, _).

everyNth(From,To,Step,X) :-
    From =< To
  , ( X = From ; From1 is From + Step, everyNth(From1,To,Step,X) )
  .

main :- doors(100,100), halt.
```



### optimized


```Prolog
doors_optimized(N) :-
	Max is floor(sqrt(N)),
	forall(between(1, Max, I),
	       (   J is I*I,format('Door ~w is open.~n',[J]))).


```



## Processing

===Unoptimized, Brute Force===

```processing
boolean[] doors = new boolean[100];

void setup() {
  for (int i = 0; i < 100; i++) {
    doors[i] = false;
  }
  for (int i = 1; i < 100; i++) {
    for (int j = 0; j < 100; j += i) {
      doors[j] = !doors[j];
    }
  }
  println("Open:");
  for (int i = 1; i < 100; i++) {
    if (doors[i]) {
      println(i);
    }
  }
  exit();
}
```


{{out}}

```txt
Open:
1
4
9
16
25
36
49
64
81
```



## Pure


```Pure
using system;

// initialize doors as pairs: number, status where 0 means open
let doors = zip (1..100) (repeat 1);

toogle (x,y) = x,~y;

toogleEvery n d = map (tooglep n) d with
                    tooglep n d@((x,_)) = toogle d if ~(x mod n);
                                        = d otherwise; end;

// show description of given doors
status (n,x) = (str n) + (case x of
                            1 = " close";
                            0 = " open"; end);

let result = foldl (\a n -> toogleEvery n a) doors (1..100);

// pretty print the result (only open doors)
showResult = do (puts.status) final when
               final = filter open result with
                         open (_,x) = ~x;
                       end; end;

```


{{out}}

```txt

> showResult;
1 open
4 open
9 open
16 open
25 open
...

```



## PureBasic

'''unoptimized'''

```purebasic
Dim doors.i(100)

For x = 1 To 100
  y = x
  While y <= 100
    doors(y) = 1 - doors(y)
    y + x
  Wend
Next

OpenConsole()
PrintN("Following Doors are open:")
For x = 1 To 100
  If doors(x)
    Print(Str(x) + ", ")
  EndIf
Next
Input()
```


'''optimized'''

```PureBasic
OpenConsole()
PrintN("Following Doors are open:")
For i = 1 To 100
    root.f = Sqr(i)
    If root = Int(root)
    	Print (Str(i) + ", ")
    EndIf
Next
Input()
```



Output:

```txt
Following Doors are open:
1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
```





## Pure Data


```Pure Data
100Doors.pd

#N canvas 241 375 414 447 10;
#X obj 63 256 expr doors[$f1] = doors[$f1] ^ 1;
#X msg 83 118 \; doors const 0;
#X msg 44 66 bang;
#X obj 44 92 t b b b;
#X obj 43 28 table doors 101;
#X obj 44 360 sel 0;
#X obj 44 336 expr if (doors[$f1] == 1 \, $f1 \, 0);
#X obj 63 204 t b f f;
#X text 81 66 run;
#X obj 71 384 print -n;
#X text 132 310 print results (open doors);
#X obj 63 179 loop 1 100 1;
#X obj 63 231 loop 1 100 1;
#X obj 44 310 loop 1 100 1;
#X text 148 28 create array;
#X text 151 180 100 passes;
#X text 179 123 set values to 0;
#X connect 2 0 3 0;
#X connect 3 0 13 0;
#X connect 3 1 11 0;
#X connect 3 2 1 0;
#X connect 5 1 9 0;
#X connect 6 0 5 0;
#X connect 7 0 12 0;
#X connect 7 1 12 1;
#X connect 7 2 12 3;
#X connect 11 0 7 0;
#X connect 12 0 0 0;
#X connect 13 0 6 0;

loop.pd

#N canvas 656 375 427 447 10;
#X obj 62 179 until;
#X obj 102 200 f;
#X obj 62 89 inlet;
#X obj 303 158 f \$3;
#X obj 270 339 outlet;
#X obj 223 89 inlet;
#X obj 138 89 inlet;
#X obj 324 89 inlet;
#X obj 117 158 f \$1;
#X text 323 68 step;
#X obj 202 158 f \$2;
#X obj 62 118 t b b b b;
#X obj 270 315 spigot;
#X obj 89 314 sel 0;
#X obj 137 206 +;
#X obj 102 237 expr $f1 \; if ($f3 > 0 \, if ($f1 > $f2 \, 0 \, 1)
\, if ($f3 < 0 \, if ($f1 < $f2 \, 0 \, 1) \, 0)), f 34;
#X text 63 68 run;
#X text 136 68 start;
#X text 227 68 end;
#X text 58 31 loop (abstraction);
#X connect 0 0 1 0;
#X connect 1 0 14 0;
#X connect 1 0 15 0;
#X connect 2 0 11 0;
#X connect 3 0 14 1;
#X connect 3 0 15 2;
#X connect 5 0 10 1;
#X connect 6 0 8 1;
#X connect 7 0 3 1;
#X connect 8 0 1 1;
#X connect 10 0 15 1;
#X connect 11 0 0 0;
#X connect 11 1 8 0;
#X connect 11 2 10 0;
#X connect 11 3 3 0;
#X connect 12 0 4 0;
#X connect 13 0 0 1;
#X connect 14 0 1 1;
#X connect 15 0 12 0;
#X connect 15 1 12 1;
#X connect 15 1 13 0;

```



## Pyret


```pyret

data Door:
  | open
  | closed
end

fun flip-door(d :: Door) -> Door:
  cases(Door) d:
    | open => closed
    | closed => open
  end
end


fun flip-doors(doors :: List<Door>) -> List<Door>:
  doc:```Given a list of door positions, repeatedly switch the positions of
      every nth door for every nth pass, and return the final list of door
      positions```
  for fold(flipped-doors from doors, n from range(1, doors.length() + 1)):
    for map_n(m from 1, d from flipped-doors):
      if num-modulo(m, n) == 0:
        flip-door(d)
      else:
        d
      end
    end
  end
where:
    flip-doors([list: closed, closed, closed]) is
  [list: open, closed, closed]

  flip-doors([list: closed, closed, closed, closed]) is
  [list: open, closed, closed, open]

  flip-doors([list: closed, closed, closed, closed, closed, closed]) is
  [list: open, closed, closed, open, closed, closed]

  closed-100 = for map(_ from range(1, 101)): closed end
  answer-100 = for map(n from range(1, 101)):
    if num-is-integer(num-sqrt(n)): open
    else: closed
    end
  end

  flip-doors(closed-100) is answer-100
end

fun find-indices<A>(pred :: (A -> Boolean), xs :: List<A>) -> List<Number>:
    doc:```Given a list and a predicate function, produce a list of index
      positions where there's a match on the predicate```
  ps = map_n(lam(n,e): if pred(e): n else: -1 end end, 1, xs)
  ps.filter(lam(x): x >= 0 end)
where:
  find-indices((lam(i): i == true end), [list: true,false,true]) is [list:1,3]
end


fun run(n):
  doc:```Given a list of doors that are closed, make repeated passes
      over the list, switching the positions of every nth door for
      each nth pass. Return a list of positions in the list where the
      door is Open.```
  doors = repeat(n, closed)
  ys = flip-doors(doors)
  find-indices((lam(y): y == open end), ys)
where:
  run(4) is [list: 1,4]
end

run(100)


```



## Python

{{works with|Python|2.5+}}
'''unoptimized'''

```python

doors = [False] * 100
for i in range(100):
   for j in range(i, 100, i+1):
       doors[j] = not doors[j]
   print("Door %d:" % (i+1), 'open' if doors[i] else 'close')

```


'''optimized'''

A version that only visits each door once:


```python
for i in xrange(1, 101):
    root = i ** 0.5
    print "Door %d:" % i, 'open' if root == int(root) else 'close'
```


One liner using a list comprehension, item lookup, and is_integer


```python
print '\n'.join(['Door %s is %s' % (i, ('closed', 'open')[(i**0.5).is_integer()]) for i in xrange(1, 101)])
```


One liner using a generator expression, ternary operator, and modulo


```python
print '\n'.join('Door %s is %s' % (i, 'closed' if i**0.5 % 1 else 'open') for i in range(1, 101))
```


{{works with|Python|3.x}}

```python

for i in range(1, 101):
    if i**0.5 % 1:
        state='open'
    else:
        state='close'
    print("Door {}:{}".format(i, state))

```


'''ultra-optimized''': ported from Julia version


```python
for i in range(1,11): print("Door %s is open" % i**2)
```



## Q

'''unoptimized'''

```q
`closed`open mod[;2]count each 1 _ group raze where each 0=t mod\:/:t:til 101
```


'''optimized'''

```q
`closed`open (1+til 100) in `int$xexp[;2] 1+til 10
```



## R

'''Using a loop'''

```r
doors_puzzle <- function(ndoors=100,passes=100) {
    doors <- rep(FALSE,ndoors)
    for (ii in seq(1,passes)) {
        mask <- seq(0,ndoors,ii)
        doors[mask] <- !doors[mask]
    }
    return (which(doors == TRUE))
}

doors_puzzle()
```



'''optimized'''

```r
x <- rep(1, 100)
for (i in 1:100-1) {
    x <- xor(x, rep(c(rep(0,i),1), length.out=100))
}
which(!x)
```


'''Using a **ply function'''

```r
doors_puzzle <- function(ndoors=100,passes=100) {
names(which(table(unlist(sapply(1:passes, function(X) seq(0, ndoors, by=X)))) %% 2 == 1))
}

doors_puzzle()
```




### Using Reduce



```R
H=100
f=rep(F,H)
which(Reduce(function(d,n) xor(replace(f,seq(n,H,n),T),d), 1:H, f))
```


{{out}}

```txt

1   4   9  16  25  36  49  64  81 100

```



## Racket


```racket

#lang racket

;; Applies fun to every step-th element of seq, leaving the others unchanged.
(define (map-step fun step seq)
  (for/list ([elt seq] [i (in-naturals)])
    ((if (zero? (modulo i step)) fun values) elt)))

(define (toggle-nth n seq)
  (map-step not n seq))

(define (solve seq)
  (for/fold ([result seq]) ([_ seq] [pass (in-naturals 1)])
    (toggle-nth pass result)))

(for ([door (solve (make-vector 101 #f))] [index (in-naturals)]
      #:when (and door (> index 0)))
  (printf "~a is open~%" index))

```


Optimized:

```racket

#lang racket
(for ([x (in-range 1 101)] #:when (exact-integer? (sqrt x)))
  (printf "~a is open\n" x))

```


Unoptimized imperative, with graphic rendering:

```racket

#lang slideshow
(define-syntax-rule (vector-neg! vec pos)
  (vector-set! vec pos (not (vector-ref vec pos))))

(define (make-doors)
  (define doors (make-vector 100 #f))
  (for* ([i 100] [j (in-range i 100 (add1 i))]) (vector-neg! doors j))
  doors)

(displayln (list->string (for/list ([d (make-doors)]) (if d #\o #\-))))

(define closed-door (inset (filled-rectangle 4 20) 2))
(define open-door (inset (rectangle 4 20) 2))

(for/fold ([doors (rectangle 0 0)]) ([open? (make-doors)])
  (hc-append doors (if open? open-door closed-door)))

```


Output:

[[File:100doors_rkt.png]]


## RapidQ


```vb

dim x as integer, y as integer
dim door(1 to 100) as byte

'initialize array
for x = 1 to 100 : door(x) = 0 : next

'set door values
for y = 1 to 100
    for x = y to 100 step y
        door(x) = not door(x)
    next x
next y

'print result
for x = 1 to 100
    if door(x) then print "Door " + str$(x) + " = open"
next

while inkey$="":wend
end

```

'''Output'''

```txt

Door 1 = open
Door 4 = open
Door 9 = open
Door 16 = open
Door 25 = open
Door 36 = open
Door 49 = open
Door 64 = open
Door 81 = open
Door 100 = open
```



## REBOL



### Unoptimized


```rebol
doors: array/initial 100 'closed
repeat i 100 [
    door: at doors i
    forskip door i [change door either 'open = first door ['closed] ['open]]
]
```



### Optimized


```rebol
doors: array/initial 100 'closed
repeat i 10 [doors/(i * i): 'open]

```



## Red



### Unoptimized


```Red
Red [
  Purpose: "100 Doors Problem (Perfect Squares)"
  Author: "Barry Arthur"
  Date: "07-Oct-2016"
]
doors: make vector! [char! 8 100]
repeat i 100 [change at doors i #"."]

repeat i 100 [
    j: i
    while [j <= 100] [
      door: at doors j
      change door either #"O" = first door [#"."] [#"O"]
      j: j + i
    ]
]

repeat i 10 [
  print copy/part at doors (i - 1 * 10 + 1) 10
]

```



## Retro


```Retro
: squared ( n-n  ) dup * ;
: doors   ( n-   ) [ 1 repeat 2over squared > 0; drop dup squared putn space 1+ again ] do 2drop ;
100 doors
```



## REXX


### the idiomatic way


```rexx
/*REXX pgm solves the  100 doors puzzle, doing it the hard way by opening/closing doors.*/
parse arg doors .                                /*obtain the optional argument from CL.*/
if doors=='' | doors==","  then doors=100        /*not specified?  Then assume 100 doors*/
                                                 /*        0 =  the door is  closed.    */
                                                 /*        1 =   "    "   "  open.      */
door.=0                                          /*assume all doors are closed at start.*/
                do #=1  for doors                /*process a pass─through for all doors.*/
                    do j=#  by #  to doors       /*  ··· every Jth door from this point.*/
                    door.j= \door.j              /*toggle the  "openness"  of the door. */
                    end   /*j*/
                end       /*#*/

say 'After '                doors          " passes, the following doors are open:"
say
                do k=1  for doors
                if door.k  then say right(k, 20) /*add some indentation for the output. */
                end    /*k*/                     /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

After  100  passes, the following doors are open:

                   1
                   4
                   9
                  16
                  25
                  36
                  49
                  64
                  81
                 100

```



### the shortcut way


```rexx
/*REXX pgm solves the  100 doors  puzzle,  doing it the easy way by calculating squares.*/
parse arg doors .                                /*obtain the optional argument from CL.*/
if doors=='' | doors==","  then doors=100        /*not specified?  Then assume 100 doors*/
say 'After '          doors          " passes, the following doors are open:"
say
          do #=1  while  #**2 <= doors           /*process easy pass─through  (squares).*/
          say right(#**2, 20)                    /*add some indentation for the output. */
          end   /*#*/                            /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.





## Ring


'''Unoptimized'''

```ring
doors = list(100)
for i = 1 to 100
doors[i] = false
next

For pass = 1 To 100
         For door = pass To 100
             if doors[door] doors[door] = false else doors[door] = true ok
         door += pass-1
         Next
Next

For door = 1 To 100
     see "Door (" + door + ") is "
     If doors[door] see "Open" else see "Closed" ok
     see nl
Next
```


'''Optimized'''

```ring
doors = list(100)
for i = 1 to 100
doors[i] = false
next

For p = 1 To 10
        doors[pow(p,2)] = True
Next

For door = 1 To 100
     see "Door (" + door + ") is "
     If doors[door] see "Open" else see "Closed" ok
     see nl
Next
```



## Ruby


```ruby
doors = Array.new(101,0)
print "Open doors "
(1..100).step(){ |i|
(i..100).step(i) { |d|
    doors[d] = doors[d]^= 1
    if i == d and doors[d] == 1 then
      print "#{i} "
    end
  }
}
```
Output:

```txt
Open doors 1 4 9 16 25 36 49 64 81 100
```

'''unoptimized; Ruby-way'''


```ruby
class Door
  attr_reader :state

  def initialize
    @state = :closed
  end

  def close
    @state = :closed
  end

  def open
    @state = :open
  end

  def closed?
    @state == :closed
  end

  def open?
    @state == :open
  end

  def toggle
    if closed? then open else close end
  end

  def to_s
    @state.to_s
  end
end

doors = Array.new(100) { Door.new }
1.upto(100) do |multiplier|
  doors.each_with_index do |door, i|
    door.toggle if (i + 1) % multiplier == 0
  end
end

doors.each_with_index { |door, i| puts "Door #{i+1} is #{door}." }
```


'''unoptimized'''


```ruby
n = 100
Open = "open"
Closed = "closed"
def Open.toggle
  Closed
end
def Closed.toggle
  Open
end
doors = [Closed] * (n + 1)
for mul in 1..n
  for x in (mul..n).step(mul)
    doors[x] = doors[x].toggle
  end
end
doors.each_with_index do |b, i|
  puts "Door #{i} is #{b}" if i > 0
end
```


'''optimized'''


```ruby
n = 100
(1..n).each do |i|
  puts "Door #{i} is #{i**0.5 == (i**0.5).round ? "open" : "closed"}"
end
```


'''generic true/false, with another way of handling the inner loop demonstrating Range#step'''


```ruby
doors = [false] * 100
100.times do |i|
  (i ... doors.length).step(i + 1) do |j|
    doors[j] = !doors[j]
  end
end
puts doors.map.with_index(1){|d,i| "Door #{i} is #{d ? 'open' : 'closed'}."}
```


{{out}}
<pre style="height:30ex;overflow:scroll">
Door 1 is open
Door 2 is closed
Door 3 is closed
Door 4 is open
Door 5 is closed
Door 6 is closed
Door 7 is closed
Door 8 is closed
Door 9 is open
Door 10 is closed
Door 11 is closed
Door 12 is closed
Door 13 is closed
Door 14 is closed
Door 15 is closed
Door 16 is open
Door 17 is closed
Door 18 is closed
Door 19 is closed
Door 20 is closed
Door 21 is closed
Door 22 is closed
Door 23 is closed
Door 24 is closed
Door 25 is open
Door 26 is closed
Door 27 is closed
Door 28 is closed
Door 29 is closed
Door 30 is closed
Door 31 is closed
Door 32 is closed
Door 33 is closed
Door 34 is closed
Door 35 is closed
Door 36 is open
Door 37 is closed
Door 38 is closed
Door 39 is closed
Door 40 is closed
Door 41 is closed
Door 42 is closed
Door 43 is closed
Door 44 is closed
Door 45 is closed
Door 46 is closed
Door 47 is closed
Door 48 is closed
Door 49 is open
Door 50 is closed
Door 51 is closed
Door 52 is closed
Door 53 is closed
Door 54 is closed
Door 55 is closed
Door 56 is closed
Door 57 is closed
Door 58 is closed
Door 59 is closed
Door 60 is closed
Door 61 is closed
Door 62 is closed
Door 63 is closed
Door 64 is open
Door 65 is closed
Door 66 is closed
Door 67 is closed
Door 68 is closed
Door 69 is closed
Door 70 is closed
Door 71 is closed
Door 72 is closed
Door 73 is closed
Door 74 is closed
Door 75 is closed
Door 76 is closed
Door 77 is closed
Door 78 is closed
Door 79 is closed
Door 80 is closed
Door 81 is open
Door 82 is closed
Door 83 is closed
Door 84 is closed
Door 85 is closed
Door 86 is closed
Door 87 is closed
Door 88 is closed
Door 89 is closed
Door 90 is closed
Door 91 is closed
Door 92 is closed
Door 93 is closed
Door 94 is closed
Door 95 is closed
Door 96 is closed
Door 97 is closed
Door 98 is closed
Door 99 is closed
Door 100 is open

```



## Run BASIC


```Runbasic
dim doors(100)
print "Open doors ";
for i = 1 to 100
    for door = i to 100 step i
        doors(door) = (doors(door) <> 1)
        if i = door and doors(door) = 1 then   print i;" ";
    next door
next i
```
Output:

```txt
Open doors 1 4 9 16 25 36 49 64 81 100
```



## Rust


```rust
fn main() {
    let mut door_open = [false; 100];
    for pass in 1..100 {
        let mut door = pass;
        while door <= 100 {
            door_open[door - 1] = !door_open[door - 1];
            door += pass;
        }
    }
    for (i, &is_open) in door_open.iter().enumerate() {
        println!("Door {} is {}.", i + 1, if is_open {"open"} else {"closed"});
    }
}
}
```


Declarative version of above:


```rust
fn main() {
    let doors = vec![false; 100].iter_mut().enumerate()
                                .map(|(door, door_state)| (1..100).into_iter()
                                                                   .filter(|pass| door % pass == 0)
                                                                   .map(|_| { *door_state = !*door_state; *door_state })
                                                                   .last().unwrap()).collect::<Vec<_>>();

    println!("{:?}", doors);
}

```


Optimized version:

(In this case the printing is the bottleneck so this version is not faster than the above one.)

```rust
fn main() {
    let squares: Vec<_> = (1..10).map(|n| n*n).collect();
    let is_square = |num| squares.binary_search(&num).is_ok();

    for i in 1..100 {
        let state = if is_square(i) {"open"} else {"closed"};
        println!("Door {} is {}", i, state);
    }
}
```


ultra-optimized: ported from Julia version

```rust
fn main() {
    for i in 1u32..10u32{
        println!("Door {} is open", i.pow(2));
    }
}
```


=={{header|S-lang}}==
<lang s-lang>variable door,
    isOpen = Char_Type [101],
    pass;

for (door = 1; door <= 100; door++) {
    isOpen[door] = 0;
}

for (pass = 1; pass <= 100; pass++) {
    for (door = pass; door <= 100; door += pass) {
        isOpen[door] = not isOpen[door];
    }
}

for (door = 1; door <= 100; door++) {
    if (isOpen[door]) {
        print("Door " + string(door) + ":open");
    } else {
        print("Door " + string(door) + ":close");
    }
}
```



## Salmon

Here's an unoptimized version:

```Salmon
variable open := <<(* --> false)>>;
for (pass; 1; pass <= 100)
    for (door_num; pass; door_num <= 100; pass)
        open[door_num] := !(open[door_num]);;;
iterate (door_num; [1...100])
    print("Door ", door_num, " is ",
          (open[door_num] ? "open.\n" : "closed.\n"));;
```


And here's an optimized one-line version:


```Salmon
iterate (x; [1...10]) { iterate (y; [(x-1)*(x-1)+1...x*x-1]) { print("Door ", y, " is closed.\n"); }; print("Door ", x*x, " is open.\n"); };
```


And a shorter optimized one-line version:


```Salmon
variable y:=1;for(x;1;x<101)"Door "~sprint(x)~" is "~(x==y*y?{++y;return"open";}:"closed")!;
```



## SAS


```sas
data _null_;
   open=1;
   close=0;
   array Door{100};
   do Pass = 1 to 100;
      do Current = Pass to 100 by Pass;
         if Door{Current} ne open
            then Door{Current} = open;
            else Door{Current} = close;
      end;
   end;
   NumberOfOpenDoors = sum(of Door{*});
   put "Number of Open Doors:  " NumberOfOpenDoors;
run;
```



## Scala


```scala
for { i <- 1 to 100
      r = 1 to 100 map (i % _ == 0) reduceLeft (_^_)
    } println (i +" "+ (if (r) "open" else "closed"))
```

The map operation maps each door (i) to a boolean sequence of toggles, one for each pass: true toggles, false leaves the same.

The reduceLeft method combines all the toggles sequentially, using the XOR operator.

And then we just need to output the result.



I made a version that optional accepts an argument for the number of doors. It is also a little more a ‘classical’ solution:

```scala

def openDoors(length : Int = 100) = {
    var isDoorOpen = new Array[Boolean](length)

    for (i <- 0 until length) {
        for (j <- i until length by i + 1) {
            isDoorOpen(j) ^= true
        }
    }
    isDoorOpen
}

val doorState  = scala.collection.immutable.Map(false -> "closed", true -> "open")
val isDoorOpen = openDoors()

for (doorNo <- 0 until isDoorOpen.length) {
    println("Door %d is %s".format(doorNo + 1, doorState(isDoorOpen(doorNo))))
}

```


I created the function openDoors which gives back an array signifying if a door is open and optional accepts an argument for the number of doors. (I like to make things general.)
I call the function and use the result to display the status of the doors.



"Optimized" version:

```scala
val o = 1 to 10 map (i => i * i)
println("open: " + o)
println("closed: " + (1 to 100 filterNot o.contains))
```



## Sather


```sather
class MAIN is
  main is
    pass, door :INT;
    doors :ARRAY{BOOL} := #(100);
    loop
      doors[0.upto!(99)] := false;
    end;
    pass := 0;
    loop while!(pass < 100);
      door := pass;
      loop while! (door < 100);
        doors[door] := ~doors[door];
	door := door + pass + 1
      end;
      pass := pass + 1;
    end;
    loop
      door := 0.upto!(99);
      #OUT + (door+1) + " " + doors[door] + "\n";
    end;
  end;
end;
```



## Scheme

'''unoptimized'''

```scheme
(define *max-doors* 100)

(define (show-doors doors)
  (let door ((i 0)
             (l (vector-length doors)))
    (cond ((= i l)
           (newline))
          (else
           (printf "~nDoor ~a is ~a"
                   (+ i 1)
                   (if (vector-ref doors i) "open" "closed"))
           (door (+ i 1) l)))))

(define (flip-doors doors)
  (define (flip-all i)
    (cond ((> i *max-doors*) doors)
          (else
           (let flip ((idx (- i 1)))
             (cond ((>= idx *max-doors*)
                    (flip-all (+ i 1)))
                   (else
                    (vector-set! doors idx (not (vector-ref doors idx)))
                    (flip (+ idx i))))))))
  (flip-all 1))

(show-doors (flip-doors (make-vector *max-doors* #f)))
```


'''optimized'''

```scheme
(define (optimised-flip-doors doors)
  (define (flip-all i)
    (cond ((> i (floor (sqrt *max-doors*))) doors)
          (else
           (vector-set! doors (- (* i i) 1) #t)
           (flip-all (+ i 1)))))
  (flip-all 1))

(show-doors (optimised-flip-doors (make-vector *max-doors* #f)))
```


'''the 3rd version'''

```scheme
(define (N_doors N)
  (define (init)
    (define (str n)
      (if (> n N) '() (cons 0 (str (+ 1 n)))))
    (str 1))
  (define (toggle x str)
    (define (s n lis)
      (define (revert x)
        (if (eq? x 0) 1 0))
      (cond ((null? lis) '())
          ((zero? (remainder n x)) (cons (revert (car lis)) (s (+ n 1) (cdr lis))))
          (else (cons (car lis) (s (+ n 1) (cdr lis))))))
    (s 1 str))
  (define (iterate x lis)
    (if (> x N) lis (iterate (+ x 1) (toggle x lis))))
  (iterate 1 (init)))
(N_doors 100)
```


Output of the 3rd version:
1 represents open, 0 represents closed.

```txt

(1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)

```



## Scilab

{{trans|Octave}}
<lang>doors=zeros(1,100);
for i = 1:100
  for j = i:i:100
    doors(j) = ~doors(j);
  end
end
for i = 1:100
  if ( doors(i) )
    s = "open";
  else
    s = "closed";
  end
  printf("%d %s\n", i, s);
end
```

{{out}}

```txt
1 open
2 closed
3 closed
4 open
5 closed
6 closed
7 closed
8 closed
9 open
10 closed
11 closed
12 closed
13 closed
14 closed
15 closed
16 open
17 closed
18 closed
19 closed
20 closed
21 closed
22 closed
23 closed
24 closed
25 open
26 closed
27 closed
28 closed
29 closed
30 closed
31 closed
32 closed
33 closed
34 closed
35 closed
36 open
37 closed
38 closed
39 closed
40 closed
41 closed
42 closed
43 closed
44 closed
45 closed
46 closed
47 closed
48 closed
49 open
50 closed
51 closed
52 closed
53 closed
54 closed
55 closed
56 closed
57 closed
58 closed
59 closed
60 closed
61 closed
62 closed
63 closed
64 open
65 closed
66 closed
67 closed
68 closed
69 closed
70 closed
71 closed
72 closed
73 closed
74 closed
75 closed
76 closed
77 closed
78 closed
79 closed
80 closed
81 open
82 closed
83 closed
84 closed
85 closed
86 closed
87 closed
88 closed
89 closed
90 closed
91 closed
92 closed
93 closed
94 closed
95 closed
96 closed
97 closed
98 closed
99 closed
100 open

```




## Scratch

Scratch is a visual programming language. Click the link, then "see inside" to see the code.

https://scratch.mit.edu/projects/168687954/

Output: 100 indications that "Door ___ is _____," where doors with perfect square indices are open and the rest are closed.




## Seed7

'''unoptimized'''

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var array boolean: doorOpen is 100 times FALSE;
    var integer: pass is 0;
    var integer: index is 0;
    var array[boolean] string: closedOrOpen is [boolean] ("closed", "open");
  begin
    for pass range 1 to 100 do
      for key index range doorOpen do
        if index rem pass = 0 then
          doorOpen[index] := not doorOpen[index];
        end if;
      end for;
    end for;
    for key index range doorOpen do
      write(index lpad 3 <& " is " <& closedOrOpen[doorOpen[index]] rpad 7);
      if index rem 5 = 0 then
        writeln;
      end if;
    end for;
  end func;
```


'''optimized'''

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: index is 0;
    var integer: number is 0;
    var array[boolean] string: closedOrOpen is [boolean] ("closed", "open");
  begin
    for index range 1 to 100 do
      number := sqrt(index);
      write(index lpad 3 <& " is " <& closedOrOpen[number**2 = index] rpad 7);
      if index rem 5 = 0 then
        writeln;
      end if;
    end for;
  end func;
```


Output of both programs:

```txt

  1 is open     2 is closed   3 is closed   4 is open     5 is closed
  6 is closed   7 is closed   8 is closed   9 is open    10 is closed
 11 is closed  12 is closed  13 is closed  14 is closed  15 is closed
 16 is open    17 is closed  18 is closed  19 is closed  20 is closed
 21 is closed  22 is closed  23 is closed  24 is closed  25 is open
 26 is closed  27 is closed  28 is closed  29 is closed  30 is closed
 31 is closed  32 is closed  33 is closed  34 is closed  35 is closed
 36 is open    37 is closed  38 is closed  39 is closed  40 is closed
 41 is closed  42 is closed  43 is closed  44 is closed  45 is closed
 46 is closed  47 is closed  48 is closed  49 is open    50 is closed
 51 is closed  52 is closed  53 is closed  54 is closed  55 is closed
 56 is closed  57 is closed  58 is closed  59 is closed  60 is closed
 61 is closed  62 is closed  63 is closed  64 is open    65 is closed
 66 is closed  67 is closed  68 is closed  69 is closed  70 is closed
 71 is closed  72 is closed  73 is closed  74 is closed  75 is closed
 76 is closed  77 is closed  78 is closed  79 is closed  80 is closed
 81 is open    82 is closed  83 is closed  84 is closed  85 is closed
 86 is closed  87 is closed  88 is closed  89 is closed  90 is closed
 91 is closed  92 is closed  93 is closed  94 is closed  95 is closed
 96 is closed  97 is closed  98 is closed  99 is closed 100 is open

```



## SequenceL

'''Unoptimized'''

```sequencel

import <Utilities/Sequence.sl>;

main:=
	let
		doors := flipDoors(duplicate(false, 100), 1);
		open[i] := i when doors[i];
	in
		open;

flipDoors(doors(1), count) :=
	let
		newDoors[i] := not doors[i] when i mod count = 0 else doors[i];
	in
		doors when count >= 100 else flipDoors(newDoors, count + 1);

```


'''Optimized'''

```sequencel

main := flipDoors([1], 2);

flipDoors(openDoors(1), i) :=
	openDoors when i * i >= 100 else flipDoors(openDoors ++ [i * i], i + 1);

```



## SETL

'''Unoptimized'''

```setl
program hundred_doors;

const toggle := {['open', 'closed'], ['closed', 'open']};

doorStates := ['closed'] * 100;

(for interval in [1..100])
  doorStates := [if i mod interval = 0 then
                    toggle(prevState) else
                    prevState end:
                 prevState = doorStates(i)];
end;

(for finalState = doorStates(i))
  print('door', i, 'is', finalState);
end;

end program;
```

If 'open' weren't a reserved word, we could omit the single quotes around it.

'''Optimized'''
Exploits the fact that squares are separated by successive odd numbers.  Use array replication to insert the correct number of closed doors in between the open ones.

```setl
program hundred_doors;

doorStates := (+/ [['closed'] * oddNum with 'open': oddNum in [1,3..17]]);

(for finalState = doorStates(i))
  print('door', i, 'is', finalState);
end;

end program;
```



## Sidef


'''Unoptimized'''

```ruby
var doors = []

{ |pass|
    { |i|
        if (pass `divides` i) {
            doors[i] := false -> not!
        }
    } << 1..100
} << 1..100

{ |i|
    say ("Door %3d is %s" % (i, doors[i] ? 'open' : 'closed'))
} << 1..100
```


'''Optimized'''

```ruby
{ |i|
    "Door %3d is %s\n".printf(i, <closed open>[i.is_sqr])
} << 1..100
```



## Simula


```simula
BEGIN
    INTEGER LIMIT = 100, door, stride;
    BOOLEAN ARRAY DOORS(1:LIMIT);
    TEXT intro;

    FOR stride := 1 STEP 1 UNTIL LIMIT DO
        FOR door := stride STEP stride UNTIL LIMIT DO
            DOORS(door) := NOT DOORS(door);

    intro :- "All doors closed but ";
    FOR door := 1 STEP 1 UNTIL LIMIT DO
        IF DOORS(door) THEN BEGIN
            OUTTEXT(intro); OUTINT(door, 0); intro :- ", "
        END;
    OUTIMAGE
END.
```

{{out}}

```txt
All doors closed but 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
```



## Slate

'''Unoptimized'''

```slate
define: #a -> (Array newSize: 100).
a infect: [| :_ | False].

a keysDo: [| :pass |
  pass to: a indexLast by: pass do: [| :door |
    a at: door infect: #not `er]].

a keysAndValuesDo: [| :door :isOpen |
  inform: 'door #' ; door ; ' is ' ; (isOpen ifTrue: ['open'] ifFalse: ['closed'])].
```


'''Optimized'''

```slate
define: #a -> (Array newSize: 100).
a infect: [| :_ | False].

0 below: 10 do: [| :door | a at: door squared put: True].
a keysAndValuesDo: [| :door :isOpen |
  inform: 'door #' ; door ; ' is ' ; (isOpen ifTrue: ['open'] ifFalse: ['closed'])].
```



## Smalltalk

{{works with|GNU Smalltalk}}
'''Unoptimized'''

```smalltalk
|a|
a := Array new: 100 .
1 to: 100 do: [ :i | a at: i put: false ].

1 to: 100 do: [ :pass |
  pass to: 100 by: pass do: [ :door |
    a at: door put: (a at: door) not .
  ]
].

"output"
1 to: 100 do: [ :door |
   ( 'door #%1 is %2' %
     { door . (a at: door) ifTrue: [ 'open' ] ifFalse: [ 'closed' ] } ) displayNl
]
```

'''Optimized'''


```smalltalk
|a|
a := (1 to: 100) collect: [ :x | false ].
1 to: 10 do: [ :i | a at: (i squared) put: true ].
1 to: 100 do: [ :i |
   ( 'door #%1 is %2' % { i .
           (a at: i) ifTrue: [ 'open' ]
                     ifFalse: [ 'closed' ] }
   ) displayNl
]
```


{{works with|Squeak Smalltalk}}
'''Unoptimized, using Morphs'''

```smalltalk

| m w h smh smw delay closedDoor border subMorphList |

closedDoor := Color black.
border := Color veryLightGray.
delay := Delay forMilliseconds: 50.
w := World bounds corner x.
h := (World bounds corner y) / 2.
smw := w/100.
smh := h/2.

m := BorderedMorph new position: 0@h.
m height: smh; width: w; borderColor: border.
m color: Color veryLightGray.

1 to: 100 do: [ :pos || sm |
	sm := BorderedMorph new height: smh ; width: smw ;
		borderColor: border; color: closedDoor;
		position: (smw*pos)@h.
	m addMorph: sm asElementNumber: pos].

m openInWorld.
delay wait.
subMorphList := m submorphs.
"display every step"
[1 to: 100 do: [ :step |
	step to: 100 by: step do: [ :pos | | subMorph |
		subMorph := subMorphList at: pos.
		subMorph color: subMorph color negated.
		delay wait]]] fork.

```



## smart BASIC


```qbasic
x=1!y=3!z=0
PRINT "Open doors: ";x;" ";
DO
    z=x+y
    PRINT z;" ";
    x=z
    y=y+2
UNTIL z>=100
END
```



## SNOBOL4


'''unoptimized'''

```snobol4

		DEFINE('PASS(A,I),O')		:(PASS.END)
PASS		O = 0
PASS.LOOP	O = O + I
		EQ(A<O>,1)			:S(PASS.1)F(PASS.0)
PASS.0		A<O> = 1			:S(PASS.LOOP)F(RETURN)
PASS.1		A<O> = 0			:S(PASS.LOOP)F(RETURN)
PASS.END

MAIN		D = ARRAY(100,0)
		I = 0

MAIN.LOOP	I = LE(I,100) I + 1		:F(OUTPUT)
		PASS(D,I)			:(MAIN.LOOP)

OUTPUT		I = 1 ; OPEN = 'Opened doors are: '
OUTPUT.LOOP	OPEN = OPEN EQ(D<I>,1) " " I
		I = LE(I,100) I + 1		:S(OUTPUT.LOOP)F(OUTPUT.WRITE)
OUTPUT.WRITE	OUTPUT = OPEN

END

```


A run of this using CSNOBOL4 looks like this:
<code>
```txt

$ snobol4 100doors.sno
The Macro Implementation of SNOBOL4 in C (CSNOBOL4) Version 1.3+
    by Philip L. Budne, January 23, 2011
SNOBOL4 (Version 3.11, May 19, 1975)
    Bell Telephone Laboratories, Incorporated

No errors detected in source program

Opened doors are:  1 4 9 16 25 36 49 64 81 100
Normal termination at level 0
100doors.sno:18: Last statement executed was 19

```
</code>

(There are command flags to remove the header and the summary, but these have been left in to keep the original SNOBOL4 experience intact.)

'''optimized'''

```snobol4

MAIN		D = ARRAY(100,0)
		I = 1

MAIN.LOOP	LE(I, 10)			:F(OUTPUT)
		D<I ** 2> = 1
		I = I + 1			:(MAIN.LOOP)

OUTPUT		I = 1 ; O = 'Opened doors are: '
OUTPUT.LOOP	O = O EQ(D<I>,1) " " I
		I = LE(I,100) I + 1		:S(OUTPUT.LOOP)F(OUTPUT.WRITE)
OUTPUT.WRITE	OUTPUT = O
END

```


The output of this version is almost identical to the above.


## Sparkling


'''unoptimized'''


```Sparkling
/* declare the variables */
var isOpen = {};
var pass, door;

/* initialize the doors */
for door = 0; door < 100; door++ {
	isOpen[door] = true;
}

/* do the 99 remaining passes */
for pass = 1; pass < 100; ++pass {
	for door = pass; door < 100; door += pass+1 {
  		isOpen[door] = !isOpen[door];
	}
}

/* print the results */
var states = { true: "open", false: "closed" };
for door = 0; door < 100; door++ {
	printf("Door #%d is %s.\n", door+1, states[isOpen[door]]);
}
```


'''optimized'''


```Sparkling
/* declare the variables */
var door_sqrt = 1;
var door;

/* print the perfect square doors as open */
for door = 0; door < 100; door++ {
	if (door_sqrt*door_sqrt == door+1) {
		printf("Door #%d is open.\n", door+1);
		door_sqrt ++;
	} else {
		printf("Door #%d is closed.\n", door+1);
	}
}
```



## Spin

{{works with|BST/BSTC}}
{{works with|FastSpin/FlexSpin}}
{{works with|HomeSpun}}
{{works with|OpenSpin}}

```Spin
con
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub init
  ser.start(31, 30, 0, 115200)

  doors

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)

var

  byte door[101] ' waste one byte by using only door[1..100]

pri doors | i,j

  repeat i from 1 to 100
    repeat j from i to 100 step i
      not door[j]

  ser.str(string("Open doors: "))

  repeat i from 1 to 100
    if door[i]
      ser.dec(i)
      ser.tx(32)

  ser.str(string(13,10))
```

{{out}}

```txt

Open doors: 1 4 9 16 25 36 49 64 81 100

```



## SQL

'''optimized'''

```SQL

DECLARE	@sqr int,
		@i int,
		@door int;

SELECT @sqr =1,
	@i = 3,
	@door = 1;

WHILE(@door <=100)
BEGIN
	IF(@door = @sqr)
	BEGIN
		PRINT 'Door ' + RTRIM(CAST(@door as char)) + ' is open.';
		SET @sqr= @sqr+@i;
		SET @i=@i+2;
	END
	ELSE
	BEGIN
		PRINT 'Door ' + RTRIM(CONVERT(char,@door)) + ' is closed.';
	END
SET @door = @door + 1
END


```



## SQL PL

{{works with|Db2 LUW}}
With SQL only:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

BEGIN
 DECLARE TYPE DOORS_ARRAY AS BOOLEAN ARRAY [100];
 DECLARE DOORS DOORS_ARRAY;
 DECLARE I SMALLINT;
 DECLARE J SMALLINT;
 DECLARE STATUS CHAR(10);
 DECLARE SIZE SMALLINT DEFAULT 100;

 -- Initializes the array, with all spaces (doors) as false (closed).
 SET I = 1;
 WHILE (I <= SIZE) DO
  SET DOORS[I] = FALSE;
  SET I = I + 1;
 END WHILE;

 -- Processes the doors.
 SET I = 1;
 WHILE (I <= SIZE) DO
  SET J = 1;
  WHILE (J <= SIZE) DO
   IF (MOD(J, I) = 0) THEN
    IF (DOORS[J] = TRUE) THEN
     SET DOORS[J] = FALSE;
    ELSE
     SET DOORS[J] = TRUE;
    END IF;
   END IF;
   SET J = J + 1;
  END WHILE;
  SET I = I + 1;
 END WHILE;

 -- Prints the final status o the doors.
 SET I = 1;
 WHILE (I <= SIZE) DO
  SET STATUS = (CASE WHEN (DOORS[I] = TRUE) THEN 'OPEN' ELSE 'CLOSED' END);
  CALL DBMS_OUTPUT.PUT_LINE('Door ' || I || ' is '|| STATUS);
  SET I = I + 1;
 END WHILE;
END @

```

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

Door 1 is OPEN
Door 2 is CLOSED
Door 3 is CLOSED
Door 4 is OPEN
Door 5 is CLOSED
Door 6 is CLOSED
Door 7 is CLOSED
Door 8 is CLOSED
Door 9 is OPEN
Door 10 is CLOSED
Door 11 is CLOSED
Door 12 is CLOSED
Door 13 is CLOSED
Door 14 is CLOSED
Door 15 is CLOSED
Door 16 is OPEN
Door 17 is CLOSED
Door 18 is CLOSED
Door 19 is CLOSED
Door 20 is CLOSED
Door 21 is CLOSED
Door 22 is CLOSED
Door 23 is CLOSED
Door 24 is CLOSED
Door 25 is OPEN
Door 26 is CLOSED
Door 27 is CLOSED
Door 28 is CLOSED
Door 29 is CLOSED
Door 30 is CLOSED
Door 31 is CLOSED
Door 32 is CLOSED
Door 33 is CLOSED
Door 34 is CLOSED
Door 35 is CLOSED
Door 36 is OPEN
Door 37 is CLOSED
Door 38 is CLOSED
Door 39 is CLOSED
Door 40 is CLOSED
Door 41 is CLOSED
Door 42 is CLOSED
Door 43 is CLOSED
Door 44 is CLOSED
Door 45 is CLOSED
Door 46 is CLOSED
Door 47 is CLOSED
Door 48 is CLOSED
Door 49 is OPEN
Door 50 is CLOSED
Door 51 is CLOSED
Door 52 is CLOSED
Door 53 is CLOSED
Door 54 is CLOSED
Door 55 is CLOSED
Door 56 is CLOSED
Door 57 is CLOSED
Door 58 is CLOSED
Door 59 is CLOSED
Door 60 is CLOSED
Door 61 is CLOSED
Door 62 is CLOSED
Door 63 is CLOSED
Door 64 is OPEN
Door 65 is CLOSED
Door 66 is CLOSED
Door 67 is CLOSED
Door 68 is CLOSED
Door 69 is CLOSED
Door 70 is CLOSED
Door 71 is CLOSED
Door 72 is CLOSED
Door 73 is CLOSED
Door 74 is CLOSED
Door 75 is CLOSED
Door 76 is CLOSED
Door 77 is CLOSED
Door 78 is CLOSED
Door 79 is CLOSED
Door 80 is CLOSED
Door 81 is OPEN
Door 82 is CLOSED
Door 83 is CLOSED
Door 84 is CLOSED
Door 85 is CLOSED
Door 86 is CLOSED
Door 87 is CLOSED
Door 88 is CLOSED
Door 89 is CLOSED
Door 90 is CLOSED
Door 91 is CLOSED
Door 92 is CLOSED
Door 93 is CLOSED
Door 94 is CLOSED
Door 95 is CLOSED
Door 96 is CLOSED
Door 97 is CLOSED
Door 98 is CLOSED
Door 99 is CLOSED
Door 100 is OPEN

```



## Standard ML


```SML

datatype Door = Closed | Opened

fun toggle Closed = Opened
  | toggle Opened = Closed

fun pass (steps, doors) = List.mapi (fn (k, door) => if (k+1) mod steps = 0 then toggle door else door) doors

(* [1..n] *)
fun runs n = List.tabulate (n, fn k => k+1)

fun run n =
	let
		val initialdoors = List.tabulate (n, fn _ => Closed)
		val runs = runs n
	in
		foldl pass initialdoors runs
	end

fun opened_doors n = List.mapPartiali (fn (k, Closed) => NONE | (k, Opened) => SOME (k+1)) (run n)

```


{{out}}

```txt

- opened_doors 100;
val it = [1,4,9,16,25,36,49,64,81,100] : int list

```



## Stata


```stata
clear
set obs 100
gen doors=0
gen index=_n
forvalues i=1/100 {
	quietly replace doors=!doors if mod(_n,`i')==0
}
list index if doors, noobs noheader

  +-------+
  |     1 |
  |     4 |
  |     9 |
  |    16 |
  |    25 |
  |-------|
  |    36 |
  |    49 |
  |    64 |
  |    81 |
  |   100 |
  +-------+
```



## SuperCollider


```SuperCollider
(
var n = 100, doors = false ! n;
var pass = { |j| (0, j .. n-1).do { |i| doors[i] = doors[i].not } };
(1..n-1).do(pass);
doors.selectIndices { |open| open }; // all are closed except [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]
)
```



## Swift


'''unoptimized'''


```Swift
/* declare enum to identify the state of a door */
enum DoorState : String {
    case Opened = "Opened"
    case Closed = "Closed"
}

/* declare list of doors state and initialize them */
var doorsStateList = [DoorState](count: 100, repeatedValue: DoorState.Closed)

/* do the 100 passes */
for i in 1...100 {
    /* map on a strideTo instance to only visit the needed doors on each iteration */
    map(stride(from: i - 1, to: 100, by: i)) {
        doorsStateList[$0] = doorsStateList[$0] == .Opened ? .Closed : .Opened
    }
}

/* print the results */
for (index, item) in enumerate(doorsStateList) {
    println("Door \(index+1) is \(item.rawValue)")
}
```


'''optimized'''


```Swift
/* declare enum to identify the state of a door */
enum DoorState : String {
    case Opened = "Opened"
    case Closed = "Closed"
}

/* declare list of doors state and initialize them */
var doorsStateList = [DoorState](count: 100, repeatedValue: DoorState.Closed)

/* set i^2 doors to opened */
var i = 1
do {
    doorsStateList[(i*i)-1] = DoorState.Opened
    ++i
} while (i*i) <= doorsStateList.count

/* print the results */
for (index, item) in enumerate(doorsStateList) {
    println("Door \(index+1) is \(item.rawValue)")
}
```



## Tailspin


```tailspin

templates hundredDoors
  @: [ 1..100 -> 0 ];
  templates toggle
    def jump: $;
    $jump..100:$jump -> (<?($@hundredDoors($) <0>)> @hundredDoors($): 1;  <> @hundredDoors($): 0;) -> !VOID
  end toggle
  1..100 -> toggle -> !VOID
  $@ -> [i](<1> ' $i;' !) !
end hundredDoors
// hundredDoors should be called a source and we shouldn't need to do "0 ->" here
0 -> hundredDoors -> 'Open doors:$...;' -> !OUT::write

```

{{out}}

```txt

Open doors: 1 4 9 16 25 36 49 64 81 100

```



## Tcl


'''unoptimized'''


```tcl
package require Tcl 8.5
set n 100
set doors [concat - [lrepeat $n 0]]
for {set step 1} {$step <= $n} {incr step} {
    for {set i $step} {$i <= $n} {incr i $step} {
        lset doors $i [expr { ! [lindex $doors $i]}]
    }
}
for {set i 1} {$i <= $n} {incr i} {
    puts [format "door %d is %s" $i [expr {[lindex $doors $i] ? "open" : "closed"}]]
}
```


'''optimized'''


```tcl
package require Tcl 8.5
set doors [lrepeat [expr {$n + 1}] closed]
for {set i 1} {$i <= sqrt($n)} {incr i} {
    lset doors [expr {$i ** 2}] open
}
for {set i 1} {$i <= $n} {incr i} {
    puts [format "door %d is %s" $i [lindex $doors $i]]
}
```


'''graphical'''

{{libheader|Tk}}
Inspired by the E solution, here's a visual representation

```tcl
package require Tcl 8.5
package require Tk

array set door_status {}

# create the gui
set doors [list x]
for {set i 0} {$i < 10} {incr i} {
    for {set j 0} {$j < 10} {incr j} {
        set k [expr {1 + $j + 10*$i}]
        lappend doors [radiobutton .d_$k -text $k -variable door_status($k) \
                         -indicatoron no -offrelief flat -width 3 -value open]
        grid [lindex $doors $k] -column $j -row $i
    }
}

# create the controls
button .start -command go -text Start
label .i_label -text " door:"
entry .i -textvariable i -width 4
label .step_label -text " step:"
entry .step -textvariable step -width 4
grid .start - .i_label - .i - .step_label - .step - -row $i
grid configure .start -sticky ew
grid configure .i_label .step_label -sticky e
grid configure .i .step -sticky w

proc go {} {
    global doors door_status i step

    # initialize the door_status (all closed)
    for {set d 1} {$d <= 100} {incr d} {
        set door_status($d) closed
    }

    # now, begin opening and closing
    for {set step 1} {$step <= 100} {incr step} {
        for {set i 1} {$i <= 100} {incr i} {
            if {$i % $step == 0} {
                [lindex $doors $i] [expr {$door_status($i) eq "open" ? "deselect" : "select"}]
                update
                after 50
            }
        }
    }
}
```


=={{header|TI-83 BASIC}}==

### Naive

 seq(0,X,1,100
 For(X,1,100
 0 or Ans-not(fPart(cumSum(1 or Ans)/A
 End
 Pause Ans
<tt>A<sup>-1</sup>cumsum(1 or Ans</tt> should be able to replace <tt>cumsum(1 or Ans)/A</tt> (saving a byte because of the unnecessary closing parenthesis) but it falls victim to a rounding error that causes <tt>X^(-1)*X</tt> to be stored as <tt>0.99999999999999...</tt> (although it's still displayed as the original X). When the fPart( [fractional part] command evaluates this, it returns .999999999, which not( turns to 0 (meaning a closed door). Regular division, as shown, isn't prone to this.

### Optimized

 Pause not(fPart(√(seq(X,X,1,100

=={{header|TI-89 BASIC}}==

```ti89b
Define doors(fast) = Func
  Local doors,i,j
  seq(false,x,1,100) ? doors
  If fast Then
    For i,1,10,1
      true ? doors[i^2]
    EndFor
  Else
    For i,1,100,1
      For j,i,100,i
        not doors[j] ? doors[j]
      EndFor
    EndFor
  EndIf
  Return doors
EndFunc
```



## TorqueScript


```Torque
for(%steps = 1; %a <= 100; %a++)
	for(%current = %steps; %current <= 100; %current += %steps)
		%door[%current] = !%door[%current];
for(%a = 1; %a <= 100; %a++)
	echo("Door #" @ %a @ " is" SPC %door[%current] ? "Open" : "Closed" @ ".");
```



## TSE SAL


```TSE SAL


// library: math: get: task: door: open: close100 <description></description> <version control></version control> <version>1.0.0.0.11</version> <version control></version control> (filenamemacro=getmaocl.s) [<Program>] [<Research>] [kn, ri, mo, 31-12-2012 22:03:16]
PROC PROCMathGetTaskDoorOpenClose( INTEGER doorMaxI, INTEGER passMaxI )
 // e.g. PROC Main()
 // e.g.  PROCMathGetTaskDoorOpenClose( 100, 100 )
 // e.g. END
 // e.g.
 // e.g. <F12> Main()
 //
 // ===
 //
 // The output will be:
 //
 // door 1 is open
 // door 4 is open
 // door 9 is open
 // door 16 is open
 // door 25 is open
 // door 36 is open
 // door 49 is open
 // door 64 is open
 // door 81 is open
 // door 100 is open
 // all other doors are closed
 //
 // ===
 //
 INTEGER passMinI = 1
 INTEGER passI = 0
 //
 INTEGER doorminI = 1
 INTEGER doorI = 0
 //
 STRING s[255] = ""
 //
 INTEGER bufferI = 0
 //
 PushPosition()
 bufferI = CreateTempBuffer()
 PopPosition()
 //
 FOR doorI = doorMinI TO doorMaxI
  //
  SetGlobalInt( Format( "doorsI", doorI ), 0 )
  //
 ENDFOR
 //
 FOR passI = passMinI TO passMaxI
  //
  doorI = passI - passI
  //
  REPEAT
   //
   doorI = doorI + passI
   //
   SetGlobalInt( Format( "doorsI", doorI ), NOT( GetGlobalInt( Format( "doorsI", doorI ) ) ) )
   //
  UNTIL ( doorI >= doorMaxI )
  //
 ENDFOR
 //
 FOR doorI = doorMinI TO doorMaxI
  //
  IF ( GetGlobalInt( Format( "doorsI", doorI ) ) > 0 )
   //
   s = "open"
   //
   AddLine( Format( "door", " ", doorI, " ", "is", " ", s ), bufferI )
   //
  ELSE
   //
   s = "closed"
   //
  ENDIF
  //
 ENDFOR
 //
 AddLine( "all other doors are closed", bufferI )
 //
 GotoBufferId( bufferI )
 //
END

PROC Main()
 PROCMathGetTaskDoorOpenClose( 100, 100 )
END


```



## True BASIC


```basic

! Optimized solution with True BASIC

OPTION NOLET
x = 1
y = 3
z = 0
PRINT STR$(x) & " Open"
DO UNTIL z >= 100
z = x + y
PRINT STR$(z) & " Open"
x = z
y = y + 2
LOOP

END

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
DICT doors create
COMPILE
LOOP door=1,100
 LOOP pass=1,100
 SET go=MOD (door,pass)
 DICT doors lookup door,num,cnt,status
   IF (num==0) THEN
     SET status="open"
     DICT doors add  door,num,cnt,status
   ELSE
    IF (go==0) THEN
       IF (status=="closed") THEN
         SET status="open"
       ELSE
         SET status="closed"
       ENDIF
     DICT doors update door,num,cnt,status
     ENDIF
   ENDIF
 ENDLOOP
ENDLOOP
ENDCOMPILE
DICT doors unload door,num,cnt,status

```

Output (variable status):
<pre style="height:30ex;overflow:scroll">
 status       = *
           1 = open
           2 = closed
           3 = closed
           4 = open
           5 = closed
           6 = closed
           7 = closed
           8 = closed
           9 = open
          10 = closed
          11 = closed
          12 = closed
          13 = closed
          14 = closed
          15 = closed
          16 = open
          17 = closed
          18 = closed
          19 = closed
          20 = closed
          21 = closed
          22 = closed
          23 = closed
          24 = closed
          25 = open
          26 = closed
          27 = closed
          28 = closed
          29 = closed
          30 = closed
          31 = closed
          32 = closed
          33 = closed
          34 = closed
          35 = closed
          36 = open
          37 = closed
          38 = closed
          39 = closed
          40 = closed
          41 = closed
          42 = closed
          43 = closed
          44 = closed
          45 = closed
          46 = closed
          47 = closed
          48 = closed
          49 = open
          50 = closed
          51 = closed
          52 = closed
          53 = closed
          54 = closed
          55 = closed
          56 = closed
          57 = closed
          58 = closed
          59 = closed
          60 = closed
          61 = closed
          62 = closed
          63 = closed
          64 = open
          65 = closed
          66 = closed
          67 = closed
          68 = closed
          69 = closed
          70 = closed
          71 = closed
          72 = closed
          73 = closed
          74 = closed
          75 = closed
          76 = closed
          77 = closed
          78 = closed
          79 = closed
          80 = closed
          81 = open
          82 = closed
          83 = closed
          84 = closed
          85 = closed
          86 = closed
          87 = closed
          88 = closed
          89 = closed
          90 = closed
          91 = closed
          92 = closed
          93 = closed
          94 = closed
          95 = closed
          96 = closed
          97 = closed
          98 = closed
          99 = closed
         100 = open

```



## TXR


```txrlisp
(defun hyaku-mai-tobira ()
  (let ((doors (vector 100)))
    (each ((i (range 0 99)))
      (each ((j (range i 99 (+ i 1))))
        (flip [doors j])))
    doors))

(each ((counter (range 1))
       (door (hyaku-mai-tobira)))
  (put-line `door @counter is @(if door "open" "closed")`))
```



## uBasic/4tH

{{trans|BBC BASIC}}
Deliberately unoptimized.
<lang>FOR p = 1 TO 100
  FOR d = p TO 100 STEP p
    @(d) = @(d) = 0
  NEXT d
NEXT p

FOR d= 1 TO 100
  IF @(d) PRINT "Door ";d;" is open"
NEXT d
```


## Uniface

'''unoptimized'''
{{works with|Uniface 9.6}}


```Uniface

entry LP_DO_IT

    variables
        string  V_DOORS
        boolean V_DOOR_STATE
        string  V_DOOR_STATE_S
        numeric V_IDX
        numeric V_TOTAL_DOORS
        string  V_DOOR_STATE_LIST
        numeric V_LOOP_COUNT
    endvariables

    V_TOTAL_DOORS = 100
    putitem V_DOORS, V_TOTAL_DOORS, 0

    V_DOORS = $replace (V_DOORS, 1, "·;", "·;0", -1)

    putitem/id V_DOOR_STATE_LIST, "1", "Open"
    putitem/id V_DOOR_STATE_LIST, "0", "Close"

    V_LOOP_COUNT = 1
    while (V_LOOP_COUNT <= V_TOTAL_DOORS)
        V_IDX = 0
        V_IDX = V_IDX + V_LOOP_COUNT

        getitem V_DOOR_STATE, V_DOORS, V_IDX
        while (V_IDX <= V_TOTAL_DOORS)

            V_DOOR_STATE = !V_DOOR_STATE
            getitem/id V_DOOR_STATE_S, V_DOOR_STATE_LIST, $number(V_DOOR_STATE)
            putitem V_DOORS, V_IDX, V_DOOR_STATE

            V_IDX = V_IDX + V_LOOP_COUNT
            getitem V_DOOR_STATE, V_DOORS, V_IDX
        endwhile

        V_LOOP_COUNT = V_LOOP_COUNT + 1

    endwhile

    V_IDX = 1
    getitem V_DOOR_STATE, V_DOORS, V_IDX
    while (V_IDX <= V_TOTAL_DOORS)
        getitem/id V_DOOR_STATE_S, V_DOOR_STATE_LIST, $number(V_DOOR_STATE)
        if (V_DOOR_STATE)
            putmess "Door %%V_IDX%%% is finally %%V_DOOR_STATE_S%%%"
        endif

        V_IDX = V_IDX + 1
        getitem V_DOOR_STATE, V_DOORS, V_IDX
    endwhile

end ; LP_DO_IT


```

{{out}}

```txt


Door 1 is finally Open
Door 4 is finally Open
Door 9 is finally Open
Door 16 is finally Open
Door 25 is finally Open
Door 36 is finally Open
Door 49 is finally Open
Door 64 is finally Open
Door 81 is finally Open
Door 100 is finally Open


```



## UNIX Shell

{{works with|Bourne Again SHell}}

```bash
#! /bin/bash

declare -a doors
for((i=1; i <= 100; i++)); do
    doors[$i]=0
done

for((i=1; i <= 100; i++)); do
    for((j=i; j <= 100; j += i)); do
	echo $i $j
	doors[$j]=$(( doors[j] ^ 1 ))
    done
done

for((i=1; i <= 100; i++)); do
    if [[ ${doors[$i]} -eq 0 ]]; then
	op="closed"
    else
	op="open"
    fi
    echo $i $op
done
```


Optimised version

```bash
#!/bin/bash

for i in {1..100}; do
  door[$i*$i]=1
  [ -z ${door[$i]} ] && echo "$i closed" || echo "$i open"
done
```




## Ursa

<lang>
#
# 100 doors
#

decl int i j
decl boolean<> doors

# append 101 boolean values to doors stream
for (set i 0) (or (< i 100) (= i 100)) (inc i)
        append false doors
end for

# loop through, opening and closing doors
for (set i 1) (or (< i 100) (= i 100)) (inc i)
        for (set j i) (or (< j 100) (= j 100)) (inc j)
                if (= (mod j i) 0)
                        set doors<j> (not doors<j>)
                end if
        end for
end for

# loop through and output which doors are open
for (set i 1) (or (< i 100) (= i 100)) (inc i)
        out "Door " i ": " console
        if doors<i>
                out "open" endl console
        else
                out "closed" endl console
        end if
end if

```




## Ursala


The doors are represented as a list of 100 booleans initialized to false. The pass function takes a number and a door list to a door list with doors toggled at indices that are multiples of the number. The main program folds the pass function (to the right) over the list of pass numbers from 100 down to 1, numbers the result, and filters out the numbers of the open doors.

```Ursala
#import std
#import nat

doors = 0!* iota 100

pass("n","d") = remainder\"n"?l(~&r,not ~&r)* num "d"

#cast %nL

main = ~&rFlS num pass=>doors nrange(100,1)
```

optimized version:

```Ursala
#import nat

#cast %nL

main = product*tiiXS iota10
```

output:

```txt

<1,4,9,16,25,36,49,64,81>

```



## UTFool


```UTFool

···
http://rosettacode.org/wiki/100_doors
···
■ HundredDoors
  § static
    ▶ main
    • args⦂ String[]
      open⦂   boolean: true
      closed⦂ boolean: false
      doors⦂  boolean[1+100] · all initially closed
      🔁 pass from 1 to 100
         ∀ visited ∈ pass‥100 by pass
         · toggle the visited doors
           if the doors[visited] are closed
              let the doors[visited] be open
           else
              let the doors[visited] be closed
      for each door #n in doors⦂ boolean
        if the door is open
           System.out.println "Door #⸨n⸩ is open."

```



## Vala

'''Unoptimized'''

```vala
int main() {
	bool doors_open[101];
	for(int i = 1; i < doors_open.length; i++) {
		for(int j = 1; i*j < doors_open.length; j++) {
			doors_open[i*j] = !doors_open[i*j];
		}
		stdout.printf("%d: %s\n", i, (doors_open[i] ? "open" : "closed"));
	}
	return 0;
}
```

Output:

```txt
1: open
2: closed
3: closed
4: open
5: closed
6: closed
7: closed
8: closed
9: open
10: closed
11: closed
...
```

'''Optimized'''

```vala
int main() {
	int i = 1;
	while(i*i <= 100) {
		stdout.printf("${i*i} open\n");
		i++;
	}
	return 0;
}
```

Output:

```txt
1 open
4 open
9 open
16 open
25 open
36 open
49 open
64 open
81 open
100 open
```


## VAX Assembly



```VAX Assembly

                           00000064  0000     1 n = 100
                               0000  0000     2 .entry	doors, ^m<>
                         26'AF   9F  0002     3 	pushab	b^arr				; offset signed byte
                    50   64 8F   9A  0005     4 	movzbl	#n, r0
                            50   DD  0009     5 	pushl	r0				; (sp) -> .ascid arr
                                     000B     6 10$:
                       51   50   D0  000B     7 	movl	r0, r1				; step = start index
                                     000E     8 20$:
                  25'AF41   01   8C  000E     9 	xorb2	#^a"0" \^a"1", b^arr-1[r1]	; \ xor toggle "1"<->"0"
             FFF5 51   50   6E   F1  0013    10 	acbl	(sp), r0, r1, 20$		; limit, step, index
                         EF 50   F5  0019    11 	sobgtr	r0, 10$				; n..1
                                     001C    12
                            5E   DD  001C    13 	pushl	sp				; descriptor by reference
              00000000'GF   01   FB  001E    14 	calls	#1, g^lib$put_output		; show result
                                 04  0025    15 	ret
                                     0026    16
30'30'30'30'30'30'30'30'30'30'30'30' 0026    17 arr:	.byte	^a"0"[n]
30'30'30'30'30'30'30'30'30'30'30'30' 0032
30'30'30'30'30'30'30'30'30'30'30'30' 003E
30'30'30'30'30'30'30'30'30'30'30'30' 004A
30'30'30'30'30'30'30'30'30'30'30'30' 0056
30'30'30'30'30'30'30'30'30'30'30'30' 0062
30'30'30'30'30'30'30'30'30'30'30'30' 006E
30'30'30'30'30'30'30'30'30'30'30'30' 007A
                        30'30'30'30' 0086
                                     008A    18 .end	doors
$ run doors
1001000010000001000000001000000000010000000000001000000000000001000000000000000010000000000000000001

```




## VBA


```vb

Sub Rosetta_100Doors()
Dim Door(100) As Boolean, i As Integer, j As Integer
For i = 1 To 100 Step 1
    For j = i To 100 Step i
        Door(j) = Not Door(j)
    Next j
    If Door(i) = True Then
        Debug.Print "Door " & i & " is Open"
    Else
        Debug.Print "Door " & i & " is Closed"
    End If
Next i
End Sub
<!-- /lang -->

*** USE THIS ONE, SEE COMMENTED LINES, DONT KNOW WHY EVERYBODY FOLLOWED OTHERS ANSWERS AND CODED THE PROBLEM DIFFERENTLY ***
*** ALWAYS USE AND TEST A READABLE, EASY TO COMPREHEND CODING BEFORE 'OPTIMIZING' YOUR CODE AND TEST THE 'OPTIMIZED' CODE AGAINST THE 'READABLE' ONE.
Panikkos Savvides.


Sub Rosetta_100Doors2()
Dim Door(100) As Boolean, i As Integer, j As Integer
Dim strAns As String
' There are 100 doors in a row that are all initially closed.
' You make 100 passes by the doors.
For j = 1 To 100
    ' The first time through, visit every door and toggle the door
    ' (if the door is closed, open it; if it is open, close it).
    For i = 1 To 100 Step 1
      Door(i) = Not Door(i)
    Next i
    ' The second time, only visit every 2nd door (door #2, #4, #6, ...), and toggle it.
    For i = 2 To 100 Step 2
      Door(i) = Not Door(i)
    Next i
    ' The third time, visit every 3rd door (door #3, #6, #9, ...), etc, until you only visit the 100th door.
    For i = 3 To 100 Step 3
      Door(i) = Not Door(i)
    Next i
Next j

For j = 1 To 100
    If Door(j) = True Then
        strAns = j & strAns & ", "
    End If
Next j

If Right(strAns, 2) = ", " Then strAns = Left(strAns, Len(strAns) - 2)
If Len(strAns) = 0 Then strAns = "0"
Debug.Print "Doors [" & strAns & "] are open, the rest are closed."
' Doors [0] are open, the rest are closed., AKA ZERO DOORS OPEN
End Sub

```



## VBScript


{{works with|Windows Script Host|5.7}}
'''Unoptimized'''

```VBScript
Dim doorIsOpen(100), pass, currentDoor, text

For currentDoor = 0 To 99
	doorIsOpen(currentDoor) = False
Next

For pass = 0 To 99
	For currentDoor = pass To 99 Step pass + 1
		doorIsOpen(currentDoor) = Not doorIsOpen(currentDoor)
	Next
Next

For currentDoor = 0 To 99
	text = "Door #" & currentDoor + 1 & " is "
	If doorIsOpen(currentDoor) Then
		text = text & "open."
	Else
		text = text & "closed."
	End If
	WScript.Echo(text)
Next
```



## Vedit macro language

'''Unoptimized'''
This implementation uses a free edit buffer as data array and for displaying the results.

A closed door is represented by a character <tt>'-'</tt> and an open door by character <tt>'O'</tt>.

```vedit
Buf_Switch(Buf_Free)
Ins_Char('-', COUNT, 100)                      // All doors closed
for (#1 = 1; #1 <= 100; #1++) {
    for (#2 = #1; #2 <= 100; #2 += #1) {
        Goto_Col(#2)
        Ins_Char((Cur_Char^0x62), OVERWRITE)   // Toggle between '-' and 'O'
    }
}
```


'''Optimized'''

```vedit
Buf_Switch(Buf_Free)
Ins_Char('-', COUNT, 100)
for (#1=1; #1 <= 10; #1++) {
    Goto_Col(#1*#1)
    Ins_Char('O', OVERWRITE)
}
```


Output:

```txt

O--O----O------O--------O----------O------------O--------------O----------------O------------------O

```



## VHDL

'''unoptimized'''

```vhdl
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity DOORS is
	port (CLK: in std_logic; OUTPUT: out std_logic_vector(1 to 100));
end DOORS;

architecture Behavioral of DOORS is
begin
	process (CLK)
	variable TEMP: std_logic_vector(1 to 100);
	begin
		--setup closed doors
		TEMP := (others => '0');

		--looping through
		for i in 1 to TEMP'length loop
			for j in i to TEMP'length loop
				if (j mod i) = 0 then
					TEMP(j) := not TEMP(j);
				end if;
			end loop;
		end loop;

		--assign output
		OUTPUT <= TEMP;
	end process;
end Behavioral;

```


'''unoptimized and synthesizable'''

```VHDL
LIBRARY ieee;
USE ieee.std_logic_1164.all;


entity doors is
  port (
        clk   : in std_logic;
        reset : in std_logic;
        door  : buffer std_logic_vector(1 to 100)
        );
end entity doors;


architecture rtl of doors is
  signal step : integer range 1 to 101;
  signal addr : integer range 1 to 201;
begin
  proc_step: process(clk, reset)
  begin
    if reset = '1' then
      step  <= 1;
      addr  <= 1;
      door <= (others => '0');
    elsif rising_edge(clk) then
      if addr <= 100 then
        door(addr) <= not door(addr);
        addr <= addr + step;
      elsif step <= 100 then
        addr <= step + 1;
        step <= step + 1;
      end if;
    end if;
  end process;
end;
```
The synthesis requires 116 FFs plus combinatorial logic.
The result is stable after 581 clock cycles.


## Visual Basic


```vb

Public Sub Doors100()
  ' the state of a door is represented by the data type boolean (false = door closed, true = door opened)
  Dim doorstate(1 To 100) As Boolean ' the doorstate()-array is initialized by VB with value 'false'
  Dim i As Long, j As Long

  For i = 1 To 100
      For j = i To 100 Step i
          doorstate(j) = Not doorstate(j)
      Next j
  Next i

  Debug.Print "The following doors are open:"
  For i = 1 To 100
      ' print number if door is openend
      If doorstate(i) Then Debug.Print CStr(i)
  Next i
End Sub

```

Output:

```txt
The following doors are open:
1
4
9
16
25
36
49
64
81
100

```




## Visual Basic .NET

{{works with|Visual Basic .NET|9.0+}}
'''unoptimized'''

```vbnet
Module Module1

   Sub Main()
       Dim doors(100) As Boolean 'Door 1 is at index 0

       For pass = 1 To 100
           For door = pass - 1 To 99 Step pass
               doors(door) = Not doors(door)
           Next
       Next

       For door = 0 To 99
           Console.WriteLine("Door # " & (door + 1) & " is " & If(doors(door), "Open", "Closed"))
       Next

       Console.ReadLine()
   End Sub

End Module
```

'''optimized'''

```vbnet
Module Module1

   Sub Main()
       Dim doors(100) As Boolean 'Door 1 is at index 0

       For i = 1 To 10
           doors(i ^ 2 - 1) = True
       Next

       For door = 0 To 99
           Console.WriteLine("Door # " & (door + 1) & " is " & If(doors(door), "Open", "Closed"))
       Next

       Console.ReadLine()
   End Sub

End Module
```



## Wart


```python
def (doors n)
  let door (table)
    for step 1 (step <= n) ++step
      for j 0 (j < n) (j <- j+step)
        zap! not door.j

    for j 0 (j < n) ++j
      when door.j
        pr j
        pr " "
```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';
let io => import 'io';

let toggle doors m =>
	a.stream doors
	-> s.enumerate
	-> s.map (@ s n => [+ (a.at n 0) 1; a.at n 1])
	-> s.map (@ s n => switch n {
			(@ s n => == (% (a.at n 0) m) 0) => ! (a.at n 1);
			true => a.at n 1;
		})
	-> s.collect
	;

s.range 100
-> s.map false
-> s.collect : doors
-> s.range 1 100
-> s.reduce doors toggle
-> a.stream
-> s.map (@ s n => switch 0 {
		n => 'Open';
		true => 'Closed';
	} -- io.writeln io.stdout)
-> s.drain
;
```


Not the most efficient code, to say the least. This has a few more allocations than should sanely be used for a problem like this.


## Wortel

{{trans|JavaScript}}

```wortel
; unoptimized
+^[
  @var doors []

  @for i rangei [1 100]
    @for j rangei [i 100 i]
      :!@not `j doors

  @for i rangei [1 100]
    @if `i doors
      !console.log "door {i} is open"
]
; optimized, map square over 1 to 10
!*^@sq @to 10
```



## Wrapl

'''Unoptimized'''

```wrapl
MOD Doors;

IMP Agg.Table;
IMP Std.String;
IMP IO.Terminal USE Out;

VAR door <- {}; EVERY door[1:to(100), "closed"];

DEF toggle(num) door[num] <- door[num] = "open" => "closed" // "open";

EVERY WITH pass <- 1:to(100), num <- pass:to(100, pass) DO toggle(num);

Out:write('Doors {door @ String.T}.');

END Doors.
```

'''Optimized'''

```wrapl
MOD Doors;

IMP IO.Terminal USE Out;

DEF open <- ALL 1:to(100) ^ 2 \ $ <= 100;
DEF closed <- ALL 1:to(100) \ NOT $ IN open;

Out:write('Doors {open} are open.\n');
Out:write('Doors {closed} are closed.\n');

END Doors.
```



## Wren

'''Unoptimized'''

```wren

var doors = [true] * 100
for (i in 1..100) {
    var j = i
    while(j < 100) {
        doors[j] = !doors[j]
        j = j + i + 1
    }
}

for (i in 0...100) {
    if (doors[i]) System.print(i + 1)
}

```


'''Optimized'''

```wren

var door = 1
var increment = 3
while(door <= 100) {
    System.print(door)
    door = door + increment
    increment = increment + 2
}

```


=={{header|X86_64 Assembly}}==
<lang X86_64 Assembly>
; linux x86_64

section .data
open: db "open", 10
closed: db "closed", 10

section .bss
doors resb 101

section .text

global _start

_start:
mov rax, 1
  mov bl, 0
  zeroset_door:
    mov [doors + rax], bl
    inc rax
    cmp rax, 101
    jl zeroset_door

  mov rax, 0
  set_doors:
    inc rax
    cmp rax, 101
    je display_result
    mov rbx, 0

    make_pass:
      add rbx, rax
      cmp rbx, 101
      jge set_doors
      not byte [doors + rbx]
      jmp make_pass

  display_result:
    mov rbx, 0
    display_door:
      inc rbx
      cmp rbx, 101
      je exit
      cmp byte [doors + rbx], 0
      je print_closed
      jmp print_open

  print_open:
    mov rax, 1
    mov rdi, 1
    mov rsi, open
    mov rdx, 5
    syscall
    jmp display_door

  print_closed:
    mov rax, 1
    mov rdi, 1
    mov rsi, closed
    mov rdx, 7
    syscall
    jmp display_door

  exit:
    mov rax, 60
    mov rdi, 0
    syscall


```



## Xojo


```vb

// True=Open; False=Closed
Dim doors(100) As Boolean // Booleans default to false
For j As Integer = 1 To 100
  For i As Integer = 1 to 100
    If i Mod j = 0 Then doors(i) = Not doors(i)
  Next
Next

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int     Door(100);              \You have 100 doors in a row
define  Open, Closed;
int     D, Pass, Step;

[for D:= 0 to 100-1 do          \that are all initially closed
        Door(D):= Closed;

Step:= 1;                       \The first time through, you visit every door
for Pass:= 1 to 100 do          \You make 100 passes by the doors
        [D:= Step-1;
        repeat  \if the door is closed, you open it; if it is open, you close it
                if Door(D)=Closed then Door(D):= Open else Door(D):= Closed;
                D:= D+Step;
        until   D>=100;
        Step:= Step+1;          \The second time you only visit every 2nd door
        ];                      \The third time, every 3rd door
                                \until you only visit the 100th door
\What state are the doors in after the last pass?
Text(0, "Open: ");              \Which are open?
for D:= 0 to 100-1 do
        if Door(D)=Open then [IntOut(0, D+1); ChOut(0,^ )];
CrLf(0);

Text(0, "Closed: ");            \Which are closed?
for D:= 0 to 100-1 do
        if Door(D)=Closed then [IntOut(0, D+1); ChOut(0,^ )];
CrLf(0);

\Optimized: The only doors that remain open are those that are perfect squares
Text(0, "Open: ");
D:= 1;
repeat  IntOut(0, D*D); ChOut(0,^ );
        D:= D+1;
until   D*D>100;
CrLf(0);
]
```



## XSLT 1.0

With input document ...


```xml><hallway

  <door number="1">closed</door>
  <door number="2">closed</door>
  <door number="3">closed</door>
  <door number="4">closed</door>
  ... etc ...
  <door number="100">closed</door>
<hallway>
```


... visually representing the initial state of the hallway, apply the following XSLT 1.0 style-sheet...


```xml
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  exclude-result-prefixes="xsl exsl">
<xsl:output method="xml" indent="yes" omit-xml-declaration="yes"/>

<xsl:template match="/*">
  <xsl:copy>
    <xsl:apply-templates select="door" />
  </xsl:copy>
</xsl:template>

<xsl:template match="door">
  <xsl:variable name="door-num" select="@number" />
  <xsl:variable name="knocks">
    <xsl:for-each select="/*/door">
      <xsl:if test="$door-num mod position() = 0">
        <xsl:text>!</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <door number="{$door-num}">
   <xsl:choose>
     <xsl:when test="string-length($knocks) mod 2 = 1">
        <xsl:text>open</xsl:text>
     </xsl:when>
     <xsl:otherwise>
        <xsl:text>closed</xsl:text>
     </xsl:otherwise>
   </xsl:choose>
  </door>
</xsl:template>

</xsl:stylesheet>
```


Also see: [[100 doors/XSLT]]


## XSLT 2.0

This XSLT 2.0 style-sheet does not use the input document.


```xml
<xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="yes" omit-xml-declaration="yes"/>

<xsl:template match="/">
  <hallway>
    <xsl:for-each select="1 to 100">
      <xsl:variable name="door-num" select="position()" />
      <door number="{$door-num}">
        <xsl:value-of select="('closed','open')[
	    number( sum( for $pass in 1 to 100 return
	    number(($door-num mod $pass) = 0)) mod 2 = 1) + 1]" />
      </door>
    </xsl:for-each>
  </hallway>
</xsl:template>

</xsl:stylesheet>
```



## Yabasic


```Yabasic
n = 100	// doors
ppa = 1	// next open door
p2 = 1

for i = 1 to n
	print "Door ", i, " is ";
	if i < p2 then
		print "closed."
	else
		ppa = ppa + 1
		p2 = ppa^2
		print "OPEN."
	end if
next
```



## Yorick


'''Unoptimized, iterative'''

```yorick
doors = array(0, 100);
for(i = 1; i <= 100; i++)
    for(j = i; j <= 100; j += i)
        doors(j) ~= 1;
print, where(doors);
```


'''Unoptimized, vectorized'''

```yorick
doors = array(0, 100);
for(i = 1; i <= 100; i++)
    doors(i::i) ~= 1;
print, where(doors);
```


'''Optimized'''

```yorick
print, indgen(1:long(sqrt(100)))^2
```


All of the above output:

```txt

[1,4,9,16,25,36,49,64,81,100]

```



## zkl

Pure brute force.

```zkl
doors:=List.createLong(100,False);	// list of 100 Falses
foreach n,m in (100,[n..99,n+1]){ doors[m]=(not doors[m]); } //foreach{ foreach{} }
doors.filterNs().apply('+(1)).println();
```

The filterNs method returns the index of each item that passes the filter.
{{out}}
```txt
L(1,4,9,16,25,36,49,64,81,100)
```



{{omit from|GUISS}}


## ZX Spectrum Basic


simple calculation

  10 REM 100 doors open/closed?
  20 DIM d(100)
  25 LET o=0
  30 FOR a=1 TO 100
  40 FOR b=a TO 100 STEP a
  50 LET d(b)=NOT d(b)
  55 LET o=o+(d(b)=1)-(d(b)=0)
  60 NEXT b
  70 NEXT a
  80 PRINT o;" open doors"


changing viewable grid

  10 REM 100 doors open/closed?
  20 DIM d(100)
  25 GO SUB 170
  30 FOR a=1 TO 100
  35 PRINT AT 0,0;"step ";a
  40 FOR b=a TO 100 STEP a
  45 PRINT AT 0,10;"door:";b;"  "
  50 LET d(b)=NOT d(b)
  55 GO SUB 150
  60 NEXT b
  70 NEXT a
  80 GO SUB 170
  90 STOP
 150 REM print door status
 151 LET p=(b-1)/10
 152 LET q=1+10*(p-INT p)
 153 LET p=INT p
 154 LET op=op+(d(b)=1)-(d(b)=0)
 156 PRINT AT 2*p+2,2*q;d(b);AT 0,27;op;"  "
 160 RETURN
 165 REM print step status
 170 LET op=0
 175 FOR p=0 TO 9
 180 FOR q=1 TO 10
 185 PRINT AT 2*p+2,2*q;d(p*10+q)
 188 LET op=op+d(p*10+q)
 190 NEXT q
 200 NEXT p
 205 PRINT AT 0,22;"open:";op;"  "
 210 RETURN


{{omit from|GUISS}}



## SheerPower 4GL


```sp4gl

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         I n i t i a l i z a t i o n
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
doors% = 100

dim doorArray?(doors%)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         M a i n   L o g i c   A r e a
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// Initialize Array
for index% = 1 to doors%
  doorArray?(index%) = false
next index%

// Execute routine
toggle_doors

// Print results
for index% = 1 to doors%
  if doorArray?(index%) = true then print index%, ' is open'
next index%


stop


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         R o u t i n e s
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
routine toggle_doors
  for index_outer% = 1 to doors%
    for index_inner% = 1 to doors%
      if mod(index_inner%, index_outer%) = 0 then
        doorArray?(index_inner%) = not doorArray?(index_inner%)
      end if
    next index_inner%
  next index_outer%
end routine


end

```

