+++
title = "Execute Brainfuck"
description = ""
date = 2019-10-11T15:48:10Z
aliases = []
[extra]
id = 2327
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "68000_assembly",
  "ada",
  "agena",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "arturo",
  "autohotkey",
  "autoit",
  "awk",
  "axe",
  "bacon",
  "basic",
  "bbc_basic",
  "brat",
  "burlesque",
  "c",
  "clojure",
  "cobol",
  "comefrom0x10",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dodo0",
  "e",
  "elena",
  "erlang",
  "forth",
  "fortran",
  "freebasic",
  "gap",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "limbo",
  "lua",
  "m2000_interpreter",
  "nim",
  "objeck",
  "ocaml",
  "ol",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "potion",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "retro",
  "rexx",
  "ruby",
  "rust",
  "scheme",
  "seed7",
  "sidef",
  "standard_ml",
  "swift",
  "tcl",
  "vbscript",
  "x86_assembly",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

{{task}}[[Category:Compilers and Interpreters]]
RCBF is a set of [[Brainfuck]] compilers and interpreters written for Rosetta Code in a variety of languages.

Below are links to each of the versions of RCBF.

An implementation need only properly implement the following instructions:
{| class="wikitable"
!Command
!Description
|-
| style="text-align:center"| <code>&gt;</code> || Move the pointer to the right
|-
| style="text-align:center"| <code>&lt;</code> || Move the pointer to the left
|-
| style="text-align:center"| <code>+</code> || Increment the memory cell under the pointer
|-
| style="text-align:center"| <code>-</code> || Decrement the memory cell under the pointer
|-
| style="text-align:center"| <code>.</code> || Output the character signified by the cell at the pointer
|-
| style="text-align:center"| <code>,</code> || Input a character and store it in the cell at the pointer
|-
| style="text-align:center"| <code>[</code> || Jump past the matching <code>]</code> if the cell under the pointer is 0
|-
| style="text-align:center"| <code>]</code> || Jump back to the matching <code>[</code> if the cell under the pointer is nonzero
|}
Any cell size is allowed,   EOF    (<u>E</u>nd-<u>O</u>-<u>F</u>ile)   support is optional, as is whether you have bounded or unbounded memory.





## 11l


```11l
F bf(source)
   V tape = DefaultDict[Int, Int]()
   V cell = 0
   V ptr = 0
   L ptr < source.len
      S source[ptr]
         ‘>’
            cell++
         ‘<’
            cell--
         ‘+’
            tape[cell]++
         ‘-’
            tape[cell]--
         ‘.’
            :stdout.write(Char(code' tape[cell]))
         ‘,’
            tape[cell] = :stdin.read(1).code
         ‘[’
            I tape[cell] == 0
               V nesting_level = 0
               L
                  S source[ptr]
                     ‘[’
                        nesting_level++
                     ‘]’
                        I --nesting_level == 0
                           L.break
                  ptr++
         ‘]’
            I tape[cell] != 0
               V nesting_level = 0
               L
                  S source[ptr]
                     ‘[’
                        I --nesting_level == 0
                           L.break
                     ‘]’
                        nesting_level++
                  ptr--
      ptr++

bf(‘++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.’)
```



## 68000 Assembly

Takes a Brainfuck program as a command line parameter. Escape character (escape key!) is handled as a 0 when inputting characters. Does NOT support break, so programs that don't end by themselves will run forever.

```68000devpac
;
; Brainfuck interpreter by Thorham
;
; 68000+ AmigaOs2+
;
; Cell size is a byte
;
    incdir  "asminc:"

    include "dos/dosextens.i"
    include "lvo/lvos.i"

execBase equ 4

start

; parse command line parameter

    move.l  a0,fileName

    move.b  (a0)+,d0
    beq     exit ; no parameter

    cmp.b   #'"',d0 ; filter out double quotes
    bne     .loop

    addq.l  #1,fileName

.loop
    move.b  (a0)+,d0

    cmp.b   #'"',d0 ; filter out double quotes
    beq     .done

    cmp.b   #32,d0
    bge     .loop

.done
    clr.b   -(a0) ; end of string

; open dos library

    move.l  execBase,a6

    lea     dosName,a1
    moveq   #36,d0
    jsr     _LVOOpenLibrary(a6)
    move.l  d0,dosBase
    beq     exit

; get stdin and stdout handles

    move.l  dosBase,a6

    jsr     _LVOInput(a6)
    move.l  d0,stdIn
    beq     exit

    jsr     _LVOOutput(a6)
    move.l  d0,stdOut
    beq     exit

    move.l  stdIn,d1
    jsr     _LVOFlush(a6)

; open file

    move.l  fileName,d1
    move.l  #MODE_OLDFILE,d2
    jsr     _LVOOpen(a6)
    move.l  d0,fileHandle
    beq     exit

; examine file

    lea     fileInfoBlock,a4

    move.l  fileHandle,d1
    move.l  a4,d2
    jsr     _LVOExamineFH(a6)
    tst.w   d0
    beq     exit

; exit if the file is a folder

    tst.l   fib_DirEntryType(a4)
    bge     exit

; allocate file memory

    move.l  execBase,a6

    move.l  fib_Size(a4),d0
    beq     exit ; exit if file is empty
    clr.l   d1
    jsr     _LVOAllocVec(a6)
    move.l  d0,program
    beq     exit

; read file

    move.l  dosBase,a6

    move.l  fileHandle,d1
    move.l  program,d2
    move.l  fib_Size(a4),d3
    jsr     _LVORead(a6)
    tst     d0
    ble     exit ; exit if read didn't succeed

; close file

    move.l  fileHandle,d1
    jsr     _LVOClose(a6)
    clr.l   fileHandle

; clear tape (bss section is allocated by os but not cleared)

    lea     tape,a0
    lea     tapeEnd,a1

.loopClear
    clr.b   (a0)+
    cmp.l   a0,a1
    bne     .loopClear

; interpreter

    move.l  program,a2
    lea     tape,a3

    clr.l   d2

    move.l  a2,d6 ; start of program
    move.l  a2,d7 ; end of program
    add.l   fib_Size(a4),d7

loop
    move.b  (a2)+,d2

    cmp.b   #">",d2
    beq     .incPtr

    cmp.b   #"<",d2
    beq     .decPtr

    cmp.b   #"+",d2
    beq     .incMem

    cmp.b   #"-",d2
    beq     .decMem

    cmp.b   #".",d2
    beq     .outMem

    cmp.b   #",",d2
    beq     .inMem

    cmp.b   #"[",d2
    beq     .jmpForward

    cmp.b   #"]",d2
    beq     .jmpBack

; next command

.next
    cmp.l   d7,a2 ; test end of program
    blt     loop

; end of program reached

    bra     exit

; command implementations

.incPtr
    addq.l  #1,a3
    cmp.l   #tapeEnd,a3 ; test end of tape
    bge     exit
    bra     .next

.decPtr
    subq.l  #1,a3
    cmp.l   #tape,a3 ; test start of tape
    blt     exit
    bra     .next

.incMem
    addq.b  #1,(a3)
    bra     .next

.decMem
    subq.b  #1,(a3)
    bra     .next

.outMem
    move.l  stdOut,d1
    move.b  (a3),d2
    jsr     _LVOFPutC(a6)
    bra     .next

.inMem
    move.l  stdIn,d1
    jsr     _LVOFGetC(a6)

    cmp.b   #27,d0 ; convert escape to 0
    bne     .notEscape
    moveq   #0,d0
.notEscape
    move.b  d0,(a3)

    bra     .next

.jmpForward
    tst.b   (a3)
    bne     .next

    move.l  a2,a4
    clr.l   d3

.loopf
    cmp.l   d7,a4 ; test end of program
    bge     exit

    move.b  (a4)+,d2

    cmp.b   #"[",d2
    bne     .lf

    addq.l  #1,d3
    bra     .loopf
.lf
    cmp.b   #"]",d2
    bne     .loopf

    subq.l  #1,d3
    bge     .loopf

    move.l  a4,a2
    bra     .next

.jmpBack
    tst.b   (a3)
    beq     .next

    move.l  a2,a4
    clr.l   d3

.loopb
    move.b  -(a4),d2

    cmp.l   d6,a4 ; test start of program
    blt     exit

    cmp.b   #"]",d2
    bne     .lb

    addq.l  #1,d3
    bra     .loopb
.lb
    cmp.b   #"[",d2
    bne     .loopb

    subq.l  #1,d3
    bgt     .loopb

    move.l  a4,a2
    bra     .next

; cleanup and exit

exit
    move.l  dosBase,a6

    move.l  fileHandle,d1
    beq     .noFile
    jsr     _LVOClose(a6)
.noFile

    move.l  execBase,a6

    move.l  program,a1
    tst.l   a1
    beq     .noMem
    jsr     _LVOFreeVec(a6)
.noMem

    move.l  dosBase,a1
    tst.l   a1
    beq     .noLib
    jsr     _LVOCloseLibrary(a6)
.noLib

    rts

; data

    section data,data_p

dosBase
    dc.l    0

fileName
    dc.l    0

fileHandle
    dc.l    0

fileInfoBlock
    dcb.b   fib_SIZEOF

stdIn
    dc.l    0

stdOut
    dc.l    0

program
    dc.l    0

dosName
    dc.b    "dos.library",0

; tape memory

    section mem,bss_p

tape
    ds.b    1024*64
tapeEnd

```



## Agena

Tested with Agena 2.9.5 Win32

```agena
# Brainfuck interpreter

# execute the Brainfuck program in the code string
bf := proc( code :: string ) is
    local address       := 1;  # current data address
    local pc            := 1;  # current position in code
    local data          := []; # data - initially empty
    local input         := ""; # user input - initially empty
    local bfOperations  :=     # table of operations and their implemntations
          [ ">" ~ proc() is inc address, 1 end
          , "<" ~ proc() is dec address, 1 end
          , "+" ~ proc() is inc data[ address ], 1 end
          , "-" ~ proc() is dec data[ address ], 1 end
          , "." ~ proc() is io.write( char( data[ address ] ) ) end
          , "," ~ proc() is
                      # get next input character, converted to an integer
                      while input = ""
                      do
                          # no input left - get the next line
                          input := io.read()
                      od;
                      data[ address ] := abs( input[ 1 ] );
                      # remove the latest character from the input
                      if size input < 2
                      then
                          input := ""
                      else
                          input := input[ 2 to -1 ]
                      fi
                  end
          , "[" ~ proc() is
                      if data[ address ] = 0
                      then
                          # skip to the end of the loop
                          local depth := 0;
                          do
                              inc pc, 1;
                              if code[ pc ] = "["
                              then
                                  inc depth, 1
                              elif code[ pc ] = "]"
                              then
                                  dec depth, 1
                              fi
                          until depth < 0
                      fi
                  end
          , "]" ~ proc() is
                      if data[ address ] <> 0
                      then
                          # skip to the start of the loop
                          local depth := 0;
                          do
                              dec pc, 1;
                              if code[ pc ] = "["
                              then
                                  dec depth, 1
                              elif code[ pc ] = "]"
                              then
                                  inc depth, 1
                              fi
                          until depth < 0
                      fi
                  end
          ];
    # execute the operations - ignore anything invalid
    while pc <= size code
    do
        if  data[ address ] = null
        then
            data[ address ] := 0
        fi;
        if  bfOperations[ code[ pc ] ] <> null
        then
            bfOperations[ code[ pc ] ]()
        fi;
        inc pc, 1
    od
end;

# prompt for Brainfuck code and execute it, repeating until an empty code string is entered
scope
    local code;
    do
        io.write( "BF> " );
        code := io.read();
        bf( code )
    until code = ""
epocs;
```



## ALGOL 68


[[/ALGOL 68|Implementation in Algol 68]].


## Ada


[[/Ada|Implementation in Ada]].


## AppleScript

Outputs debug in a .txt file similar to that of [http://brainfuck.tk brainfuck.tk]

```AppleScript

set codeString to text returned of (display dialog "Enter BF code:" buttons "OK" default answer "")
set inputString to text returned of (display dialog "Enter input string" buttons "OK" default answer "")
set codePointer to 1
set loopPosns to {}
set tape to {}
set tapePointer to 1
set output to {}
set inputPointer to 1
set step to 0

set thePath to (path to desktop as Unicode text) & "log.txt"
set debug to (open for access file thePath with write permission)

write (step as string) & " (" & ((codePointer - 1) as string) & "): (The program contains " & ((length of codeString) as string) & " instructions.)
" to debug

set step to 1

on betterMod(x, y) -- so -2 mod 256 is 254 instead of -2
	local x
	local y
	try
		return -y * (round (x / y) rounding down) + x
	on error eMsg number eNum
		error "Can't call betterMod() on " & eMsg number eNum
	end try
end betterMod

repeat while codePointer ≤ length of codeString
	set theChar to (get character codePointer of codeString)

	if (theChar = "+") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		set item tapePointer of tape to betterMod(((get item tapePointer of tape) + 1), 256)
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | a[" & ((tapePointer - 1) as string) & "]= " & ((item tapePointer of tape) as string) & "
" to debug
	else if (theChar = "-") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		set item tapePointer of tape to betterMod(((get item tapePointer of tape) - 1), 256)
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | a[" & ((tapePointer - 1) as string) & "]= " & ((item tapePointer of tape) as string) & "
" to debug
	else if (theChar = "<") then
		set tapePointer to tapePointer - 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | array pos. now " & ((tapePointer - 1) as string) & "
" to debug

	else if (theChar = ">") then
		set tapePointer to tapePointer + 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | array pos. now " & ((tapePointer - 1) as string) & "
" to debug

	else if (theChar = "[") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Array[" & ((tapePointer - 1) as string) & "] is '" & ((item tapePointer of tape) as string) & "'" to debug
		if (item tapePointer of tape ≠ 0) then
			set loopPosns to loopPosns & codePointer
			write " ** Loop nesting level: " & (((length of loopPosns) - 1) as string) & ".
" to debug
		else
			write "
" & (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Not entering a loop but skipping to instruction number " to debug
			set matchLoops to 1
			repeat while matchLoops ≠ 0
				set codePointer to codePointer + 1
				if (item codePointer of codeString = "[") then
					set matchLoops to matchLoops + 1
				else if (item codePointer of codeString = "]") then
					set matchLoops to matchLoops - 1
				end if
			end repeat
			write ((codePointer - 1) as string) & "
" to debug
		end if

	else if (theChar = "]") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Array[" & ((tapePointer - 1) as string) & "] is '" & ((item tapePointer of tape) as string) & "'
" to debug
		if (item tapePointer of tape ≠ 0) then
			write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | looping back to " & (((item (length of loopPosns) of loopPosns) - 1) as string) & "
" to debug
			set codePointer to (item (length of loopPosns) of loopPosns) - 1
		end if
		if (length of loopPosns > 1) then
			set loopPosns to items 1 thru ((length of loopPosns) - 1) of loopPosns
		else
			set loopPosns to {}
		end if

	else if (theChar = ".") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | output '" & ((item tapePointer of tape) as string) & "' " & string id (item tapePointer of tape) & "
" to debug
		set output to output & item tapePointer of tape

	else if (theChar = ",") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		if (inputPointer > length of inputString) then
			set inputPointer to 1
		end if
		set item tapePointer of tape to id of item inputPointer of inputString
		set inputPointer to inputPointer + 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | read in " & string id (item tapePointer of tape) & " (" & ((item tapePointer of tape) as string) & ")
" to debug
	end if

	set codePointer to codePointer + 1
	set step to step + 1
end repeat

set strout to string id output
display dialog strout
close access debug

```



## Arturo



```arturo
/*********************************
 * Brainf*ck compiler
 * In Art:uro
 *********************************/

Tape                    #(0)
DataPointer             0
InstructionPointer      0

// Look for jumps in code an register them
// in the Jumps table

precomputeJumps {
    stack                   #()
    jumphash                #{}
    instrPointer            0

    loop instrPointer<CodeLength {
        command $(get $(characters Code) instrPointer)
        if command="[" { stack stack+instrPointer } {
            if command="]" {
                target $(last stack)
                deleteBy stack $(size stack)-1
                set jumphash $(toString target) instrPointer
                set jumphash $(toString instrPointer) target
            }
        }
        instrPointer instrPointer+1
    }
    jumphash
}

// Check if current state is valid

StateIsValid {
    all #(0<=DataPointer DataPointer<$(size Tape) 0<=InstructionPointer InstructionPointer<CodeLength)
}

// Compile the program

interpret {
    loop $(StateIsValid) {
        command $(get $(characters Code) InstructionPointer)

        | command="+" { set Tape DataPointer Tape.[DataPointer]+1 }
        | command="-" { set Tape DataPointer Tape.[DataPointer]-1 }
        | command=">" { DataPointer DataPointer+1, if DataPointer=$(size Tape) { Tape Tape+0 } }
        | command="<" { DataPointer DataPointer-1 }
        | command="." { print $(char Tape.[DataPointer]) true }
        | command="," {
            inp $(toNumber input)
            | inp=13 { inp 10 }
            | inp=3  { panic "something went wrong!" }
            set Tape DataPointer inp
        }
        | command="[" {
            if Tape.[DataPointer]=0 { InstructionPointer Jumps.[$(toString InstructionPointer)] }
        }
        | command="]" {
            if Tape.[DataPointer]!=0 { InstructionPointer Jumps.[$(toString InstructionPointer)] }
        }

        InstructionPointer InstructionPointer+1
    }
}

if $(size &)<1 { panic "Not enough arguments - Usage: bfc <script>" }

Code                    $(read &0)
CodeLength              $(size Code)
Jumps                   $(precomputeJumps)

interpret

```


```bf
++++++++++[>+++++++>++++++++++>+++>
+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
```


```txt
Hello World!
```



## AutoHotkey


[[/AutoHotkey|Implementation in AutoHotkey]].


## AutoIt



```AutoIt
; AutoFucck
; A AutoIt Brainfuck Interpreter
; by minx
; AutoIt Version: 3.3.8.x

; Commands:
; - 	DEC
; +		INC
; [		LOOP START
; ]		LOOP END
; .		Output cell value as ASCII Chr
; ,		Input a ASCII char (cell value = ASCII code)
; :		Ouput cell value as integer
; ;		Input a Integer
; _		Output a single whitespace
; / 	Output an Carriage Return and Line Feed

; You can load & save .atf Files.

#include <WindowsConstants.au3>
#include <EditConstants.au3>
#include <Array.au3>
#include <GUIConstants.au3>
#include <StaticCOnstants.au3>

HotKeySet("{F5}", "_Runn")

$hMain = GUICreate("Autofuck - Real Brainfuck Interpreter", 600, 525)
$mMain = GUICtrlCreateMenu("File")
Global $mCode = GUICtrlCreateMenu("Code")
$mInfo = GUICtrlCreateMenu("Info")
$mCredits = GUICtrlCreateMenuItem("Credits", $mInfo)
$mFile_New = GUICtrlCreateMenuItem("New", $mMain)
$mFile_Open = GUICtrlCreateMenuItem("Open", $mMain)
$mFile_Save = GUICtrlCreateMenuItem("Save", $mMain)
Global $mCode_Run = GUICtrlCreateMenuItem("Run [F5]", $mCode)
Global $lStatus = GUICtrlCreateLabel("++ Autofuck started...", 5, 480, 590, 20, $SS_SUNKEN)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
$eCode = GUICtrlCreateEdit("", 5, 5, 590, 350)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
$eConsole = GUICtrlCreateEdit("", 5, 360, 590, 115, $ES_WANTRETURN)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
GUISetState()

While 1
	$nMsg = GUIGetMsg()
	Switch $nMsg
		Case $mFile_New
			GUICtrlSetData($eCode, "")
		Case $mFile_Open
			GUICtrlSetData($eCode, FileRead(FileOpenDialog("Open Autofuck script", @DesktopDir, "Autofuck (*.atf)")))
		Case $mFile_Save
			FileWrite(FileOpen(StringReplace(FileSaveDialog("Save Autofuck script", @DesktopDir, "Autofuck (*.atf)"), ".atf", "") &".atf", 2), GUICtrlRead($eCode))
		Case $GUI_EVENT_CLOSE
			Exit
		Case $mCredits
			MsgBox(0, "Autofuck", "Copyright by: "&@CRLF&"minx (autoit.de)"&@CRLF&"crashdemons (autoitscript.com)")
	EndSwitch
WEnd

Func _Runn()
	$Timer = TimerInit()
	GUICtrlSetData($lStatus, "++ Program started")
	Global $tData=DllStructCreate('BYTE[65536]')
	Global $pData=0
	GUICtrlSetData($eConsole, "")
	Local $aError[6]=['','Unmatched closing bracket during search','Unmatched opening bracket during search','Unexpected closing bracket','Data pointer passed left boundary','Data pointer passed right boundary']
    Local $sError=''
    Local $i=_Run(GUICtrlRead($eCode))
    If @error>=0 And @error<6 Then $sError=$aError[@error]
    If StringLen($sError) Then GUICtrlSetData($eConsole, 'ERROR: '&$sError&'.'&@CRLF&'Ending Instruction Pointer: '&($i-1)&@CRLF&'Current Data Pointer: '&$pData)
	GUICtrlSetData($lStatus, "++ Program terminated. Runtime: "& Round( TimerDiff($Timer) / 1000, 4) &"s")
EndFunc

Func _Run($Code,$iStart=1,$iEnd=0)
    If $iEnd<1 Then $iEnd=StringLen($Code)
    For $i = $iStart to $iEnd
        Switch StringMid($Code, $i, 1)
            Case ">"
                $pData+=1
                If $pData=65536 Then Return SetError(5,0,$i)
            Case "<"
                $pData-=1
                If $pData<0 Then Return SetError(4,0,$i)
            Case "+"
                DllStructSetData($tData,1,DllStructGetData($tData,1,$pData+1)+1,$pData+1)
            Case "-"
                DllStructSetData($tData,1,DllStructGetData($tData,1,$pData+1)-1,$pData+1)
            Case ":"
                GUICtrlSetData($eConsole, GUICtrlRead($eConsole) & (DllStructGetData($tData,1,$pData+1)))
			Case "."
                GUICtrlSetData($eConsole, GUICtrlRead($eConsole) & Chr(DllStructGetData($tData,1,$pData+1)))
            Case ";"
                Local $cIn=StringMid(InputBox('Autofuck','Enter Number'),1)
                DllStructSetData($tData,1,Number($cIn),$pData+1)
			Case ","
                Local $cIn=StringMid(InputBox('Autofuck','Enter one ASCII character'),1,1)
                DllStructSetData($tData,1,Asc($cIn),$pData+1)
            Case "["
                Local $iStartSub=$i
                Local $iEndSub=_MatchBracket($Code,$i,$iEnd)
                If @error<>0 Then Return SetError(@error,0,$iEndSub)
                While DllStructGetData($tData,1,$pData+1)<>0
                    Local $iRet=_Run($Code,$iStartSub+1,$iEndSub-1)
                    If @error<>0 Then Return SetError(@error,0,$iRet)
                WEnd
                $i=$iEndSub
            Case ']'
                Return SetError(3,0,$i)
			Case "_"
				GUICtrlSetData($eConsole, GUICtrlRead($eConsole)&" ")
			Case "/"
				GUICtrlSetData($eConsole, GUICtrlRead($eConsole)&@CRLF)
        EndSwitch
    Next
    Return 0
EndFunc

Func _MatchBracket($Code,$iStart=1,$iEnd=0)
    If $iEnd<1 Then $iEnd=StringLen($Code)
    Local $Open=0
    For $i=$iStart To $iEnd
        Switch StringMid($Code,$i,1)
            Case '['
                $Open+=1
            Case ']'
                $Open-=1
                If $Open=0 Then Return $i
                If $Open<0 Then Return SetError(1,0,$i)
        EndSwitch
    Next
    If $Open>0 Then Return SetError(2,0,$i)
    Return 0
EndFunc
```



## AWK


Expects the program (not the program file) to be the first argument to the script.  Cells don't wrap (trivial if desired) and the code and arena are unbounded.


```AWK
BEGIN {
	bf=ARGV[1]; ARGV[1] = ""
	compile(bf)
	execute()
}

# Strips non-instructions, builds the jump table.
function compile(s,   i,j,k,f) {
	c = split(s, src, "")
	j = 0
	for(i = 1; i <= c; i++) {
		if(src[i] ~ /[\-\+\[\]\<\>,\.]/)
			code[j++] = src[i]

		if(src[i] == "[") {
			marks[j] = 1
		} else if(src[i] == "]") {
			f = 0
			for(k = j; k > 0; k--) {
				if(k in marks) {
					jump[k-1] = j - 1
					jump[j-1] = k - 1
					f = 1
					delete marks[k]
					break
				}
			}
			if(!f) {
				print "Unmatched ]"
				exit 1
			}
		}
	}
}

function execute(   pc,p,i) {
	pc = p = 0
	while(pc in code) {
		i = code[pc]

		if(i == "+")
			arena[p]++
		else if(i == "-")
			arena[p]--
		else if(i == "<")
			p--
		else if(i == ">")
			p++
		else if(i == ".")
			printf("%c", arena[p])
		else if(i == ",") {
			while(1) {
				if (goteof) break
				if (!gotline) {
					gotline = getline
					if(!gotline) goteof = 1
					if (goteof) break
					line = $0
				}
				if (line == "") {
					gotline=0
					m[p]=10
					break
				}
				if (!genord) {
					for(i=1; i<256; i++)
						ord[sprintf("%c",i)] = i
					genord=1
				}
				c = substr(line, 1, 1)
				line=substr(line, 2)
				arena[p] = ord[c]
				break
			}

		} else if((i == "[" && arena[p] == 0) ||
		          (i == "]" && arena[p] != 0))
			pc = jump[pc]
		pc++
	}
}

```


```txt
$ awk -f /tmp/bf.awk '++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.<+++++++.--------.<<<<<+.<+++.---.'
Goodbye, World!

```



## Axe

In this implementation, the array is limited to 768 bytes due to OS constraints. Call BF with pointers to the (null-terminated) program and input.

Note that this implementation has no error checking.


```axe
Lbl BF
r₁→P
r₂→I
L₁→D
Fill(D,768,0)

While {P}
 {P}→C
 If C='+'
  {D}++
 ElseIf C='-'
  {D}--
 ElseIf C='>'
  D++
 ElseIf C='<'
  D--
 ElseIf C='.'
  Disp {D}▶Char
 ElseIf C=','
  {I}→{D}
  I++
 ElseIf C='['?{D}=0
  NEXT(P)→P
 ElseIf C=']'
  PREV(P)→P
 End
 P++
End
Return

Lbl NEXT
r₁++
1→S
While S
 If {r₁}='['
  S++
 ElseIf {r₁}=']'
  S--
 End
 r₁++
End
r₁
Return

Lbl PREV
r₁--
1→S
While S
 If {r₁}=']'
  S++
 ElseIf {r₁}='['
  S--
 End
 r₁--
End
r₁
Return
```


'''Example'''

```axe
"++++++++++++++++++++++++++++++++[>+>+<<-]>>+++++++++++++++++++++++++<<++++++++++[>>.-<.<-]"→Str1
BF(Str1,0)
```


'''Output'''

```txt
9▪8▪7▪6▪5▪4▪3▪2▪1▪0▪
```



## BASIC


[[/BASIC/QuickBasic|Implementation in BASIC]] (QuickBasic dialect).

=
## Applesoft BASIC
=

```ApplesoftBasic
0 ON NOT T GOTO 20 : FOR A = T TO L : B = PEEK(S + P) : ON C%(ASC(MID$(C$, A, T))) GOSUB 1, 2, 3, 4, 5, 8, 6, 7 : NEXT A : END
1 P = P + T : ON P < E GOTO 11 : O = 1E99
2 P = P - T : ON P > M GOTO 11 : O = 1E99
3 B = B + T : B = B - (B > U) * B : GOTO 9
4 B = B - T : B = B - (B < 0) * (B - U) : GOTO 9
5 PRINT CHR$(B); : RETURN
6 D = T : ON NOT B GOTO 10 : RETURN
7 D = M : ON NOT NOT B GOTO 10 : RETURN
8 GET B$ : B = LEN(B$) : IF B THEN B = ASC(B$)
9 POKE S + P, B : RETURN
10 FOR K = D TO 0 STEP 0 : A = A + D : K = K + D%(ASC(MID$(C$, A, T))) : NEXT K : RETURN
11 RETURN
20 HIMEM: 38401
21 LOMEM: 8185
22 DIM C%(14999) : CLEAR
23 POKE 105, PEEK(175)
24 POKE 106, PEEK(176)
25 POKE 107, PEEK(175)
26 POKE 108, PEEK(176)
27 POKE 109, PEEK(175)
28 POKE 110, PEEK(176)
29 HIMEM: 8192
30 T = 1
31 M = -1
32 S = 8192
33 E = 30000
34 U = 255
35 DIM C%(255), D%(255)
43 C%(ASC("+")) = 3
44 C%(ASC(",")) = 6
45 C%(ASC("-")) = 4
46 C%(ASC(".")) = 5
60 C%(ASC("<")) = 2
62 C%(ASC(">")) = 1
91 C%(ASC("[")) = 7
92 D%(ASC("[")) = 1
93 C%(ASC("]")) = 8
94 D%(ASC("]")) = -1
95 C$ = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++.+++."
98 L = LEN(C$)
99 GOTO
```


=
## BaCon
=
By the author of BaCon, Peter van Eerten.

```freebasic
REM
REM Brainfuck interpreter

REM Get the separate arguments
SPLIT ARGUMENT$ BY " " TO arg$ SIZE dim

IF dim < 2 THEN
    PRINT "Usage: bf <file>"
    END
ENDIF

REM Determine size
filesize = FILELEN(arg$[1])

REM Get the contents
OPEN arg$[1] FOR READING AS bf

REM claim memory
txt = MEMORY(filesize)

REM Read file into memory
GETBYTE txt FROM bf SIZE filesize

CLOSE FILE bf

REM Initialize work memory
mem = MEMORY(30000)

REM This is The Pointer pointing to memory
thepointer = 0

REM This is the cursor pointing in the current program
cursor = 0

REM Start interpreting program
WHILE cursor < filesize DO

    command = PEEK(txt + cursor)

    SELECT command
        CASE 62
            INCR thepointer

        CASE 60
            DECR thepointer

        CASE 43
            POKE mem + thepointer, PEEK(mem + thepointer) + 1

        CASE 45
            POKE mem + thepointer, PEEK(mem + thepointer) - 1

        CASE 46
            PRINT CHR$(PEEK(mem + thepointer));

        CASE 44
            key = GETKEY
            POKE mem + thepointer, key

        CASE 91
            jmp = 1
            IF ISFALSE(PEEK(mem + thepointer)) THEN
                REPEAT
                    INCR cursor
                    IF PEEK(txt + cursor) = 91 THEN
                        INCR jmp
                    ELIF PEEK(txt + cursor) = 93 THEN
                        DECR jmp
                    END IF
                UNTIL PEEK(txt + cursor) = 93 AND NOT(jmp)
            END IF

        CASE 93
            jmp = 1
            IF ISTRUE(PEEK(mem + thepointer)) THEN
                REPEAT
                    DECR cursor
                    IF PEEK(txt + cursor) = 93 THEN
                        INCR jmp
                    ELIF PEEK(txt + cursor) = 91 THEN
                        DECR jmp
                    END IF
                UNTIL PEEK(txt + cursor) = 91 AND NOT(jmp)
            END IF
    END SELECT

    INCR cursor
WEND
```



## BBC BASIC


```bbcbasic
      bf$ = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>" + \
      \     ">---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++.+++."
      PROCbrainfuck(bf$)
      END

      DEF PROCbrainfuck(b$)
      LOCAL B%, K%, M%, P%
      DIM M% LOCAL 65535
      B% = 1 : REM pointer to string
      K% = 0 : REM bracket counter
      P% = 0 : REM pointer to memory
      FOR B% = 1 TO LEN(b$)
        CASE MID$(b$,B%,1) OF
          WHEN "+": M%?P% += 1
          WHEN "-": M%?P% -= 1
          WHEN ">": P% += 1
          WHEN "<": P% -= 1
          WHEN ".": VDU M%?P%
          WHEN ",": M%?P% = GET
          WHEN "[":
            IF M%?P% = 0 THEN
              K% = 1
              B% += 1
              WHILE K%
                IF MID$(b$,B%,1) = "[" THEN K% += 1
                IF MID$(b$,B%,1) = "]" THEN K% -= 1
                B% += 1
              ENDWHILE
            ENDIF
          WHEN "]":
            IF M%?P% <> 0 THEN
              K% = -1
              B% -= 1
              WHILE K%
                IF MID$(b$,B%,1) = "[" THEN K% += 1
                IF MID$(b$,B%,1) = "]" THEN K% -= 1
                B% -= 1
              ENDWHILE
            ENDIF
        ENDCASE
      NEXT
      ENDPROC

```

```txt
Hello World!
```


=={{header|Brainfuck}}==

Brainfuck in Brainfuck Yey!
Credits to Frans, NYYRIKKI, Daniel B Cristofani for the code.

Frans:

"I started to think about a BF interpreter written in BF, and because I did not want to write BF code directly, I started with writing a C program that could generate BF code for often used constructs. After some experimentation, I decided to implement a direct execution mode (making use of a define), so that I didn't have to go through the generate-interpret cycle. This resulted in the BF interpreter in BF generation program. If the macro symbol EXECUTE is not defined, this program when executed generates a BF interpreter in BF. This BF interpreter expects as input a BF program terminated with an exclamation mark, followed by the input for the program to be interpreted. I by no means claim that this BF interpreter in BF is the shortest possible. (Actually, NYYRIKKI wrote a much short one and Daniel B. Cristofani an even shorter one.)
The BF interpreter in BF (when filtered through a comment remover) looks like:"

<lang Brainfuck>
>>>,[->+>+<<]>>[-<<+>>]>++++[<++++++++>-]<+<[->>+>>+<<<<]>>>>[-<<<<+>>
>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>[[-]<<<<->-<[->>+>>+
<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>]<
<<<[->>+<<]>[->+<]>[[-]<<<[->+>+<<]>>[-<<+>>]>++++++[<+++++++>-]<+<[->
>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<
[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>
>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+>>>
>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++[<+++++++++>-]<<[->>>+>+<<<<]>>>>[-<<
<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[
->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]
]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++>>>>[-]]<<<[->+>+<<]
>>[-<<+>>]>++++++[<++++++++++>-]<<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+
>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>
>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->
>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++
+++[<++++++++++>-]<++<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-
<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>
]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>
+<<]>+>[<->[-]]<[<<<<++++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++[<+++++++
++>-]<+<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->
>+<<]>[-]]<[-]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<
]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]
]<[<<<<+++++>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>++++[<+++++++++++>-]<<[->>>
+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-
]>>[[-]<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>
]<[<[->>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++++++
>>>>[-]]<<<[->+>+<<]>>[-<<+>>]>+++++++[<+++++++++++++>-]<<[->>>+>+<<<<
]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]
<<<<->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->
>+<<]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<+++++++>>>>[-
]]<<<[->+>+<<]>>[-<<+>>]>+++++++[<+++++++++++++>-]<++<[->>>+>+<<<<]>>>
>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<]>[-]]<[-]>>[[-]<<<<
->-<[->>>+>+<<<<]>>>>[-<<<<+>>>>]<<<[->+>>+<<<]>>>[-<<<+>>>]<[<[->>+<<
]>[-]]<[-]>>]<<<<[->>>+<<<]>[->>+<<]>+>[<->[-]]<[<<<<++++++++>>>>[-]]<
<<<[->>+>+<<<]>>>[-<<<+>>>]<[<<<[->>>>>>>>>+<+<<<<<<<<]>>>>>>>>[-<<<<<
<<<+>>>>>>>>]<<<<<<<[->>>>>>>>>+<<+<<<<<<<]>>>>>>>[-<<<<<<<+>>>>>>>]>[
<[->>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>>[-]<[->+<]<<[[-<
<<<<+>>>>>]<<<<<-]<<<<<<<<+>[-]>>[-]]<,[->+>+<<]>>[-<<+>>]>++++[<+++++
+++>-]<+<[->>+>>+<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]>>>[-<<<+>>>]<<[>[
->+<]<[-]]>[-]>[[-]<<<<->-<[->>+>>+<<<<]>>>>[-<<<<+>>>>]<<<[->>+>+<<<]
>>>[-<<<+>>>]<<[>[->+<]<[-]]>[-]>]<<<<[->>+<<]>[->+<]>]<<<<<[-][->>>>>
>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>
+>-]>>[[-<+<+>>]<<[->>+<<]>[-<+>[<->[-]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[
-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>
>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>
>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>+<<<<[[-<<<<<+>>>>>]<<<
<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<
]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-<+>[<->[-]]]]<[[
-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<
[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>
>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]
>>>-<<<<[[-<<<<<+>>>>>]<<<<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<
+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->
>+<<]>[-[-[-<+>[<->[-]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[
-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<
<<<->+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>
>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-[-[-<+>[<->[-]]]]]]<[[-
]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[
-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<+>+[->>>>>>>>>+<<<<<<+<<<]>
>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>
>]<<[->>+<<]>[-[-[-[-[-<+>[<->[-]]]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<
<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>
>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>
>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>.<<<<[[-<<<<<+>>>>>]<<<<<
-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>
[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+<+>>]<<[->>+<<]>[-[-[-[-[-[-<+>[<->[-]
]]]]]]]<[[-]<[->+>+<<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<
-]<<<<<<<<[-]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<
<<<<<<<<]>>>>>>>>>[-<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<
<]>>>>+>-]>>>,<<<<[[-<<<<<+>>>>>]<<<<<-]<<<<<<<<+[->>>>>>>>>+<<<<<<+<<
<]>>>[-<<<+>>>]>>>>>>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-][-]]>>[-<+
<+>>]<<[->>+<<]>[-[-[-[-[-[-[-<+>[<->[-]]]]]]]]]<[[-]<[->+>+<<]>>[-<<+
>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>>>[-<<<<<
<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-<<<<<<<<<
+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>[-<<<+>+>>]<<[->
>+<<]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]>[-<<<<<<+>>>>>>]>+<<<<<<
<[>>>>>>>-<<<<<<<[-]]<<<[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>[<[-
>>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>[[-]<+[<[->>>>>+<<<<
<]>[->>>>>+<<<<<]>>>>+>>>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-<<<+>>>[<<
<->>>[-]]]]]]]]]<<<[<+>[-]]>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-[-<<<+>
>>[<<<->>>[-]]]]]]]]]]<<<[<->[-]]<]>[-]]<<[->>>>>+<<<<<]>>>>>+>[-]]>>[
-<+<+>>]<<[->>+<<]>[-[-[-[-[-[-[-[-<+>[<->[-]]]]]]]]]]<[[-][-]<[->+>+<
<]>>[-<<+>>]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]<<<<<<<<[-]>>>>>>>
>>[-<<<<<<<<<+>>>>>>>>>]<<<<<<<<<<[->>>>>>>>>>+<+<<<<<<<<<]>>>>>>>>>[-
<<<<<<<<<+>>>>>>>>>]>[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>>+>-]>>>[-<<<+>
+>>]<<[->>+<<]<<[[-<<<<<+>>>>>]>[-<<<<<+>>>>>]<<<<<<-]>[-<<<<<<+>>>>>>
]<<<<<<[->>>>>>>+<<<<<<<]<<<[->>>>>>>>>+<<<<<<+<<<]>>>[-<<<+>>>]>>>>>>
[<[->>>>>+<<<<<]>[->>>>>+<<<<<]>[->>>>>+<<<<<]>>>+>-]>[[-]<+[<[-<<<<<+
>>>>>]>[-<<<<<+>>>>>]<<<<<<->>>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-<<<+
>>>[<<<->>>[-]]]]]]]]]<<<[<->[-]]>[->>+<<<+>]<[->+<]>>>[-[-[-[-[-[-[-[
-<<<+>>>[<<<->>>[-]]]]]]]]]]<<<[<+>[-]]<]>[-]]<<[->>>>>+<<<<<]>>>>>+>[
-]]>>]

```


NYYRIKKI:

"Hi,

I saw your Brainfuck interpreter for Brainfuck and it encouraged me to write my own version of it. I now write to you as I thought, you might be interested about it.

I wanted to write as fast version as possible so I wrote it directly with Brainfuck. Here is a list of key tricks, that I used to get maximum performance:

* I used loader routine, that removes comments before executing and converts BF code to internal format. In the internal format       numbers 1-8 are used for commands and zero is used to terminate execution (code before line split) This is because handling small numbers is much more effective than handling large numbers in BF.

*I used special IF (x)=0 stucture like this: >+<[>-]>[THEN >] to avoid slow byte copying loops. (Command backup etc.)

*I used 3 bytes for each program element to get maximum speed. Using less would have caused program it self get more slow and complex and using more would have slowed down memory seek. For program memory I used 2 bytes for each element.
Data is not transferred between memory and program. In "[" command I only move Z flag. (Value in memory is more likely to be NZ and "0" is faster to move than "1")

This program works same way as yours. Only difference is, that program termination character is ":" instead of "!"
No more explaining... here is the code:"

<lang Brainfuck>
>>>+[,>+++++++[<------>-]<[->+>+<<]>>[-<<+>>]<->+<[>-<[-]]>[-<<[-]++++
+>>>>>]<<[->+>+<<]>>[-<<+>>]<-->+<[>-<[-]]>[-<<[-]++++++++>>>>>]<<[->+
>+<<]>>[-<<+>>]<--->+<[>-<[-]]>[-<<[-]++++++>>>>>]<<[->+>+<<]>>[-<<+>>
]<---->+<[>-<[-]]>[-<<[-]+++++++>>>>>]<<[->+>+<<]>>[-<<+>>]++++++[<---
>-]+<[>-<[-]]>[-<<[-]++++>>>>>]<<[->+>+<<]>>[-<<+>>]+++++[<---->-]+<[>
-<[-]]>[-<<[-]+++>>>>>]<<[->+>+<<]>>[-<<+>>]+++++++[<------->-]+<[>-<[
-]]>[-<<[-]+>>>>>]<<[->+>+<<]>>[-<<+>>]+++++++[<------->-]<-->+<[>-<[-
]]>[-<<[-]++>>>>>]<++++[<---->-]<]<<<[<<<]>>>
[-->+<[>-]>[>]<<++>[-<<<<
<+[>-->+<[>-]>[-<<+>>>]<<+>+<[>-]>[-<<->>>]<<+<[-<<<+>>>]<<<]>>>>>]<->
+<[>-]>[>]<<+>[-<->>>[>>>]>[->+>>+<<<]>[-<+>]>>[-[->>+<<]+>>]+>[->+<]>
[-<+>>>[-]+<<]+>>[-<<->>]<<<<[->>[-<<+>>]<<<<]>>[-<<<<<+>>>>>]<<<<<<<[
>>[-<<<+>>>]<<<<<]+>>[>-->+<[>-]>[-<<->>>]<<+>+<[>-]>[-<<+>>>]<<+<[->>
>+<<<]>>>]<]<--->+<[>-]>[->>[>>>]>+<<<<[<<<]>>]<<->+<[>-]>[->>[>>>]>-<
<<<[<<<]>>]<<->+<[>-]>[->>[>>>]>[->+>>+<<<]>[-<+>]>>[-[->>+<<]+>>]+>+<
[-<<]<<<<<[<<<]>>]<<->+<[>-]>[->>[>>>]>[->+>>+<<<]>[-<+>]>>[-[->>+<<]+
>>]+>-<[-<<]<<<<<[<<<]>>]<<->+<[>-]>[->>[>>>]>[->+>>+<<<]>[-<+>]>>[-[-
>>+<<]+>>]+>.<[-<<]<<<<<[<<<]>>]<<->+<[>-]>[->>[>>>]>[->+>>+<<<]>[-<+>
]>>[-[->>+<<]+>>]+>,<[-<<]<<<<<[<<<]>>]<<++++++++>>>]

```


Daniel B. Cristofani

<lang Brainfuck>

>>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]
[input a Brainfuck program and its input, separated by an exclamation point.
Daniel B Cristofani (cristofdathevanetdotcom)
http://www.hevanet.com/cristofd/brainfuck/]


```


Links:
[[http://www.iwriteiam.nl/Ha_bf_inter.html]]

[[http://www.iwriteiam.nl/Ha_vs_bf_inter.html]]

[[http://www.hevanet.com/cristofd/dbfi.b]]

Franco C. Bartolabac,
a 12 y/o boi.


## Brat


[[/Brat|Implementation in Brat]]


## Burlesque



```burlesque

".""X"r~"-""\/^^{vvvv}c!!!-.256.%{vvvv}c!sa\/"r~"+""\/^^{vvvv}c!!!+.
256.%{vvvv}c!sa\/"r~"[""{"r~"]""}{\/^^{vvvv}c!!!}w!"r~">""+."r~"<""
-."r~"X""\/^^{vvvv}c!!!L[+]\/+]\/+]^^3\/.+1RAp^\/+]\/[-1RA^^-]\/[-\/
"r~"\'\'1 128r@{vv0}m[0"\/.+pse!vvvv<-sh

```


However, this implementation does not support input.
Also, output is visible only after the brainfuck program terminated.
This is due to the limitation that Burlesque does not have actual I/O.


## C


[[/C|Implementation in C]].

## C#

[[/Csharp|Implementation in C#]].


## C++


[[/C++|Implementation in C++]].


## Clojure


```clojure
(ns brainfuck)

(def ^:dynamic *input*)

(def ^:dynamic *output*)

(defrecord Data [ptr cells])

(defn inc-ptr [next-cmd]
  (fn [data]
    (next-cmd (update-in data [:ptr] inc))))

(defn dec-ptr [next-cmd]
  (fn [data]
    (next-cmd (update-in data [:ptr] dec))))

(defn inc-cell [next-cmd]
  (fn [data]
    (next-cmd (update-in data [:cells (:ptr data)] (fnil inc 0)))))

(defn dec-cell [next-cmd]
  (fn [data]
    (next-cmd (update-in data [:cells (:ptr data)] (fnil dec 0)))))

(defn output-cell [next-cmd]
  (fn [data]
    (set! *output* (conj *output* (get (:cells data) (:ptr data) 0)))
    (next-cmd data)))

(defn input-cell [next-cmd]
  (fn [data]
    (let [[input & rest-input] *input*]
      (set! *input* rest-input)
      (next-cmd (update-in data [:cells (:ptr data)] input)))))

(defn if-loop [next-cmd loop-cmd]
  (fn [data]
    (next-cmd (loop [d data]
                (if (zero? (get (:cells d) (:ptr d) 0))
                  d
                  (recur (loop-cmd d)))))))

(defn terminate [data] data)

(defn split-cmds [cmds]
  (letfn [(split [[cmd & rest-cmds] loop-cmds]
                 (when (nil? cmd) (throw (Exception. "invalid commands: missing ]")))
                 (case cmd
                       \[ (let [[c l] (split-cmds rest-cmds)]
                            (recur c (str loop-cmds "[" l "]")))
                       \] [(apply str rest-cmds) loop-cmds]
                       (recur rest-cmds (str loop-cmds cmd))))]
    (split cmds "")))

(defn compile-cmds [[cmd & rest-cmds]]
  (if (nil? cmd)
    terminate
    (case cmd
          \> (inc-ptr (compile-cmds rest-cmds))
          \< (dec-ptr (compile-cmds rest-cmds))
          \+ (inc-cell (compile-cmds rest-cmds))
          \- (dec-cell (compile-cmds rest-cmds))
          \. (output-cell (compile-cmds rest-cmds))
          \, (input-cell (compile-cmds rest-cmds))
          \[ (let [[cmds loop-cmds] (split-cmds rest-cmds)]
               (if-loop (compile-cmds cmds) (compile-cmds loop-cmds)))
          \] (throw (Exception. "invalid commands: missing ["))
          (compile-cmds rest-cmds))))

(defn compile-and-run [cmds input]
  (binding [*input* input *output* []]
    (let [compiled-cmds (compile-cmds cmds)]
     (println (compiled-cmds (Data. 0 {}))))
    (println *output*)
    (println (apply str (map char *output*)))))

```


```clojure
brainfuck
 (compile-and-run "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." [])
{:ptr 4, :cells {4 10, 3 33, 2 100, 1 87, 0 0}}
[72 101 108 108 111 32 87 111 114 108 100 33 10]
Hello World!

nil

```


The alternate implementation at [[Execute Brainfuck/Clojure]] showcases a rather different approach.


## COBOL


[[/COBOL|Implementation in COBOL]].


## Comefrom0x10


This interpreter takes a command line argument with the path to a Brainfuck program. It uses strings as storage, so storage is unbounded on both sides of the pointer, but behavior is undefined for cell values lower than zero or higher than 0x10ffff.


```cf0x10
pointer_alpha = 1/0
pointer_numeric = 1/0
tape_behind = ''
tape_ahead = 1/0
tape_pos = 0 # only for debugging
array_behind = 1/0
array_ahead = ''
set_tape_ahead = array_ahead
array_ahead = 1/0
#
shift
  comefrom if array_ahead is array_ahead
  cdr = 1/0
  cdr = array_ahead
  shift_tail = cdr
  new_cell
    comefrom shift if shift_tail is ''
    itoa = 0
    shift_tail = itoa
  car = 1/0
  car = array_ahead
  array_behind = car array_behind
  done = shift_tail
  array_ahead = shift_tail
  comefrom shift if array_ahead is done

set_pointer_alpha = 1/0
set_pointer_alpha
  comefrom if set_pointer_alpha
  atoi = set_pointer_alpha
  cdr = tape_ahead
  set_tape_ahead = set_pointer_alpha cdr
  set_pointer_alpha = 1/0

set_tape_ahead = 1/0
set_pointer_vals
  comefrom if set_tape_ahead
  tape_ahead = set_tape_ahead
  car = tape_ahead
  pointer_alpha = car
  atoi = pointer_alpha
  pointer_numeric = atoi
  set_tape_ahead = 1/0

pointer_change = 1/0
change_pointer_val
  comefrom if pointer_change
  car = tape_ahead
  cdr = tape_ahead
  itoa = pointer_numeric + pointer_change
  set_tape_ahead = itoa cdr
  pointer_change = 1/0

file = 0 # initialize to something other than undefined so jump from file works when read fails
read_path = argv
error_reading_program
  comefrom file if file + 0 is 0
  'Error: cannot read Brainfuck program at "' read_path '"'
  ''

program_loaded
  comefrom file if file is file
  program_behind = ''
  program_ahead = file

  run
    comefrom program_loaded
    opcode = 1/0
    opcode_numeric = 1/0
    in_buffer = '' # cf0x10 stdin is line-buffered
    jumping = 0
    moving = 1
    comefrom run

    comefrom execute if opcode_numeric is 0
    ''
    execute
      comefrom run if moving
      # can be useful for debugging:
      #program_ahead moving ':' jumping '@' tape_pos ':' pointer_numeric
      car = program_ahead
      atoi = car
      opcode_numeric = atoi
      opcode = car
      opcode = 1/0

      #

    program_forward
      comefrom execute if moving > 0
      array_behind = program_behind
      array_ahead = 1/0
      array_ahead = program_ahead
      program_behind = array_behind
      program_ahead = array_ahead

      forward_jump
        comefrom execute if opcode is '['

        jump
          comefrom forward_jump if pointer_numeric is 0
          jumping = jumping + 1
          moving = 1
        match_brace
          comefrom forward_jump if jumping < 0
          jumping = jumping + 1
          stop_jump
            comefrom match_brace if jumping is 0
            moving = 1

    program_backward
      comefrom execute if moving < 0
      array_behind = program_ahead
      array_ahead = 1/0
      array_ahead = program_behind
      program_behind = array_ahead
      program_ahead = array_behind

      backward_jump
        comefrom execute if opcode is ']'

        jump
          comefrom backward_jump if pointer_numeric > 0
          jumping = jumping - 1
          moving = -1
        match_brace
          comefrom backward_jump if jumping > 0
          jumping = jumping - 1
          stop_jump
            comefrom match_brace if jumping is 0
            moving = 1

    op
      comefrom execute if opcode

      moving = 1
      do_op = opcode
      comefrom op if jumping
      #
      forward
        comefrom op if do_op is '>'
        tape_pos = tape_pos + 1
        array_ahead = 1/0
        array_behind = tape_behind
        array_ahead = tape_ahead
        tape_behind = array_behind
        set_tape_ahead = array_ahead
      backward
        comefrom op if do_op is '<'
        tape_pos = tape_pos - 1
        array_ahead = 1/0
        array_behind = tape_ahead
        array_ahead = tape_behind
        tape_behind = array_ahead
        set_tape_ahead = array_behind

      increment
        comefrom op if do_op is '+'
        pointer_change = 1
      decrement
        comefrom op if do_op is '-'
        pointer_change = -1

      print
        comefrom op if do_op is '.'
        pointer_alpha...
      read
        comefrom op if do_op is ','
        #
        cdr = 1/0
        cdr = in_buffer
        car = in_buffer
        set_pointer_alpha = car
        cdr = in_buffer
        in_buffer = cdr
        comefrom stdin if stdin + 0 is 0
        #
        block_for_input
          comefrom read if cdr is ''
          stdin = ''
          in_buffer = stdin
          cdr = in_buffer
          comefrom stdin if stdin + 0 is 0
```



## Common Lisp


[[/Common Lisp|Implementation in Common Lisp]].


## D


[[/D|Implementation in D]].


## dodo0



```dodo0
#Import some functions
clojure('count', 1) -> size
clojure('nth', 2) -> charAt
clojure('inc', 1) -> inc
clojure('dec', 1) -> dec
clojure('char', 1) -> char
clojure('int', 1) -> int
clojure('read-line', 0) -> readLine

#The characters we will need
charAt("\n", 0) -> newLine
charAt("@", 0) -> exitCommand
charAt("+", 0) -> incrCommand
charAt("-", 0) -> decrCommand
charAt("<", 0) -> shlCommand
charAt(">", 0) -> shrCommand
charAt(".", 0) -> printCommand
charAt(",", 0) -> inputCommand
charAt("[", 0) -> repeatCommand
charAt("]", 0) -> endCommand

#Read a character from a line of input.
fun readChar -> return
(
	readLine() -> line
	size(line) -> length

	#Return the ith character and a continuation
	fun nextFromLine -> i, return
	(
		'='(i, length) -> eol
		if (eol) ->
		(
			return(newLine, readChar)	#end of line
		)
		|
			charAt(line, i) -> value
			inc(i) -> i
			fun next (-> return) nextFromLine(i, return) | next
			return(value, next)
	)
	| nextFromLine

	nextFromLine(0, return)	#first character (position 0)
)
| readChar

#Define a buffer as a value and a left and right stack
fun empty (-> return, throw) throw("Error: out of bounds") | empty
fun fill (-> return, throw) return(0, fill) | fill

fun makeBuffer -> value, left, right, return
(
	fun buffer (-> return) return(value, left, right) | buffer
	return(buffer)
)
| makeBuffer

fun push -> value, stack, return
(
	fun newStack (-> return, throw) return(value, stack) | newStack
	return(newStack)
)
| push

#Brainfuck operations
fun noop -> buffer, input, return
(
	return(buffer, input)
)
| noop

fun selectOp -> command, return
(
	'='(command, incrCommand) -> eq
	if (eq) ->
	(
		fun increment -> buffer, input, return
		(
			buffer() -> value, left, right
			inc(value) -> value
			makeBuffer(value, left, right) -> buffer
			return(buffer, input)
		)
		| increment
		return(increment)
	)
	|
	'='(command, decrCommand) -> eq
	if (eq) ->
	(
		fun decrement -> buffer, input, return
		(
			buffer() -> value, left, right
			dec(value) -> value
			makeBuffer(value, left, right) -> buffer
			return(buffer, input)
		)
		| decrement
		return(decrement)
	)
	|
	'='(command, shlCommand) -> eq
	if (eq) ->
	(
		fun shiftLeft -> buffer, input, return
		(
			buffer() -> value, left, right
			push(value, right) -> right
			left() -> value, left
			(
				makeBuffer(value, left, right) -> buffer
				return(buffer, input)
			)
			| message
				println(message) ->
				exit()
		)
		| shiftLeft
		return(shiftLeft)
	)
	|
	'='(command, shrCommand) -> eq
	if (eq) ->
	(
		fun shiftRight -> buffer, input, return
		(
			buffer() -> value, left, right
			push(value, left) -> left
			right() -> value, right
			(
				makeBuffer(value, left, right) -> buffer
				return(buffer, input)
			)
			| message
				println(message) ->
				exit()
		)
		| shiftRight
		return(shiftRight)
	)
	|
	'='(command, printCommand) -> eq
	if (eq) ->
	(
		fun putChar -> buffer, input, return
		(
			buffer() -> value, left, right
			char(value) -> value
			'print'(value) -> dummy
			'flush'() -> dummy
			return(buffer, input)
		)
		| putChar
		return(putChar)
	)
	|
	'='(command, inputCommand) -> eq
	if (eq) ->
	(
		fun getChar -> buffer, input, return
		(
			input() -> letter, input
			int(letter) -> letter
			buffer() -> value, left, right
			makeBuffer(letter, left, right) -> buffer
			return(buffer, input)
		)
		| getChar
		return(getChar)
	)
	|
	return(noop)
)
| selectOp

#Repeat until zero operation
fun whileLoop -> buffer, input, continue, break
(
	buffer() -> value, left, right
	'='(value, 0) -> zero
	if (zero) ->
	(
		break(buffer, input)
	)
	|
		continue(buffer, input) -> buffer, input
		whileLoop(buffer, input, continue, break)
)
| whileLoop

#Convert the Brainfuck program into dodo0 instructions
fun compile -> input, endmark, return
(
	input() -> command, input

	'='(command, endmark) -> eq
	if (eq) ->
	(
		return(noop, input)	#the end, stop compiling
	)
	|
		#Put in sequence the current operation and the rest of the program
		fun chainOp -> op, input, return
		(
			compile(input, endmark) -> program, input
			fun exec -> buffer, input, return
			(
				op(buffer, input) -> buffer, input
				program(buffer, input, return)
			)
			| exec
			return(exec, input)
		)
		| chainOp

		'='(command, repeatCommand) -> eq
		if (eq) ->
		(
			compile(input, endCommand) -> body, input	#compile until "]"

			#Repeat the loop body until zero
			fun repeat -> buffer, input, return
			(
				whileLoop(buffer, input, body, return)
			)
			| repeat
			chainOp(repeat, input, return)
		)
		|
			selectOp(command) -> op
			chainOp(op, input, return)
)
| compile

#Main program
compile(readChar, exitCommand) -> program, input
makeBuffer(0, empty, fill) -> buffer
input() -> nl, input	#consume newline from input

#Execute the program instructions
program(buffer, input) -> buffer, input
exit()
```

Execution:

```txt

$ java -classpath antlr-3.2.jar:clojure-1.2.0/clojure.jar:. clojure.main dodo/runner.clj bfc2.do0
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.@
Hello World!

```



## E

[[/E|Implementation in E]].


## Elena

[[/Elena|Implementation in Elena]]


## Erlang

[[/Erlang|Implementation in Erlang]].


## Forth

[[/Forth|Implementation in Forth]].


## Fortran

Initial puzzlement as to the nature of the scratchpad was resolved: the source code being interpreted is in one storage area and the data scratchpad is another. Thus, self-modifying code is ''not'' in fact possible, so higher level of Brainfuck is precluded - as are still further opportunities offered by having the instruction and data pointers being in the data scratchpad rather than as separate items. Later experimentation showed that the initial value of all the STORE cells must be zero. Having a specified example code to try would help too.


### Interpreter

The source employs F90 so as to gain the convenience of a service routine SEEK contained within RUN that thereby has access to the PROG and the instruction pointer - though these could have been passed as additional parameters. The main idea is that the expression can fit on one line and special code is not used for the two cases. The STORE array of cells is represented as an array of  CHARACTER*1 variables rather than a CHARACTER*n single variable. This means that an element is addressed as STORE(i), rather than STORE(i:i), and that STORE = CHAR(0) initialises the whole array to zero. If it were CHARACTER*n, then only the first character would be zero, all subsequent would be blanks. It is not clear what size a cell represents, but a single character suffices for the trial run. For usage that involves arithmetic, the ICHAR and CHAR functions are needed which work on values of 0:255. The cell array could be declared INTEGER*1 instead, which would allow arithmetic without sacrifices on the altar of type checking. Such a variable in two's complement has values of -128:127 however with only addition and subtraction supported this doesn't matter - the bit patterns are the same as for unsigned integers. Larger integer sizes are possible if required, but would require changes to the READ and WRITE statements because A1 format works at the high-order end of a multi-byte variable.

The PROG variable could also be regarded as an array of single characters, but such an array is not a suitable candidate for a text literal such as initialises HELLOWORLD.
```Fortran
      MODULE BRAIN	!It will suffer.
       INTEGER MSG,KBD
       CONTAINS		!A twisted interpreter.
        SUBROUTINE RUN(PROG,STORE)	!Code and data are separate!
         CHARACTER*(*) PROG	!So, this is the code.
         CHARACTER*(1) STORE(:)	!And this a work area.
         CHARACTER*1 C		!The code of the moment.
         INTEGER I,D		!Fingers to an instruction, and to data.
          D = 1		!First element of the store.
          I = 1		!First element of the prog.

          DO WHILE(I.LE.LEN(PROG))	!Off the end yet?
            C = PROG(I:I)			!Load the opcode fingered by I.
            I = I + 1				!Advance one. The classic.
            SELECT CASE(C)			!Now decode the instruction.
             CASE(">"); D = D + 1				!Move the data finger one place right.
             CASE("<"); D = D - 1				!Move the data finger one place left.
             CASE("+"); STORE(D) = CHAR(ICHAR(STORE(D)) + 1)	!Add one to the fingered datum.
             CASE("-"); STORE(D) = CHAR(ICHAR(STORE(D)) - 1)	!Subtract one.
             CASE("."); WRITE (MSG,1) STORE(D)			!Write a character.
             CASE(","); READ (KBD,1) STORE(D)			!Read a character.
             CASE("["); IF (ICHAR(STORE(D)).EQ.0) CALL SEEK(+1)	!Conditionally, surge forward.
             CASE("]"); IF (ICHAR(STORE(D)).NE.0) CALL SEEK(-1)	!Conditionally, retreat.
             CASE DEFAULT				!For all others,
		  						!Do nothing.
            END SELECT				!That was simple.
          END DO			!See what comes next.

    1     FORMAT (A1,$)	!One character, no advance to the next line.
         CONTAINS	!Now for an assistant.
          SUBROUTINE SEEK(WAY)	!Look for the BA that matches the AB.
           INTEGER WAY		!Which direction: ±1.
           CHARACTER*1 AB,BA	!The dancers.
           INTEGER INDEEP	!Nested brackets are allowed.
            INDEEP = 0		!None have been counted.
            I = I - 1		!Back to where C came from PROG.
            AB = PROG(I:I)	!The starter.
            BA = "[ ]"(WAY + 2:WAY + 2)	!The stopper.
    1       IF (I.GT.LEN(PROG)) STOP "Out of code!"	!Perhaps not!
            IF (PROG(I:I).EQ.AB) THEN		!A starter? (Even if backwards)
              INDEEP = INDEEP + 1			!Yep.
            ELSE IF (PROG(I:I).EQ.BA) THEN	!A stopper?
              INDEEP = INDEEP - 1			!Yep.
            END IF				!A case statement requires constants.
            IF (INDEEP.GT.0) THEN	!Are we out of it yet?
              I = I + WAY			!No. Move.
              IF (I.GT.0) GO TO 1		!And try again.
              STOP "Back to 0!"			!Perhaps not.
            END IF			!But if we are out of the nest,
            I = I + 1			!Advance to the following instruction, either WAY.
          END SUBROUTINE SEEK	!Seek, and one shall surely find.
        END SUBROUTINE RUN	!So much for that.
      END MODULE BRAIN	!Simple in itself.

      PROGRAM POKE	!A tester.
      USE BRAIN		!In a rather bad way.
      CHARACTER*1 STORE(30000)	!Probably rather more than is needed.
      CHARACTER*(*) HELLOWORLD	!Believe it or not...
      PARAMETER (HELLOWORLD = "++++++++[>++++[>++>+++>+++>+<<<<-]"
     1 //" >+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------"
     2 //".--------.>>+.>++.")
      KBD = 5		!Standard input.
      MSG = 6		!Standard output.
      STORE = CHAR(0)	!Scrub.

      CALL RUN(HELLOWORLD,STORE)	!Have a go.

      END	!Enough.
```

Output:

```txt

Hello World!

```



### Compiler

Well, really a translator, as it translates the Brain*uck code into Fortran statements. This is relatively straightforward because the source code does not change and a simple translation scheme is possible. The standard problem with compilation is provided by forward references, specifically that the destination of a forwards jump is at an address that cannot be known until the code up to it has been produced. This can be handled in many ways, for instance with two passes where the first locates all the destinations so that the second can refer to them when generating code. Another method involves a "fixup table", whereby a record is kept of the locations of all leaps to as-yet unknown destinations, and when later those destinations are determined, the compiler goes back and fixes the destination fields. This all requires additional storage, in unknown amounts depending on the source being compiled.

The problem can be dodged with systems that generate say assembler source (or in this case, Fortran source) by developing some scheme for generating and using labels, merely placing them at the appropriate locations. The subsequent assembly (or Fortran compilation) will deal with these forwards references in its own way. The plan here is to recognise that a [...] sequence generates two labels, one at the location of the [ and the other at the ]. That's two labels per pair, so, count the labels and use an odd number for the [ <code>LABEL = 2*NLABEL - 1</code> and the corresponding even number for the ], then keep in mind which is used at which end. Because a [...] sequence can contain nested [...] sequences, a stack is needed to keep track, and so, why not indent the source accordingly? On the other hand, there is no attempt at checking that the [...] bracketing is correct, and run-time checking that the data pointer remains within bounds is left to the Fortran compiler.

Since the increment and decrement instructions are often repeated, it is simple enough to scan ahead and count up the repetitions via a function (that also manipulates its environment), and convert a sequence of operations into a single operation. Thus, this is an ''optimising'' Brain*uck compiler!

The source involves adding a subroutine to the module and an extended main line for testing:
```Fortran
        SUBROUTINE BRAINFORT(PROG,N,INF,OUF,F)	!Stand strong!
Converts the Brain*uck in PROG into the equivalent furrytran source...
         CHARACTER*(*) PROG	!The Brain*uck source.
         INTEGER N		!A size for the STORE.
         INTEGER INF,OUF,F	!I/O unit numbers.
         INTEGER L		!A stepper.
         INTEGER LABEL,NLABEL,INDEEP,STACK(66)	!Labels cause difficulty.
         CHARACTER*1 C		!The operation of the moment.
         CHARACTER*36 SOURCE	!A scratchpad.
          WRITE (F,1) PROG,N	!The programme heading.
    1     FORMAT (6X,"PROGRAM BRAINFORT",/,	!Name it.
     1     "Code: ",A,/				!Show the provenance.
     2     6X,"CHARACTER*1 STORE(",I0,")",/	!Declare the working memory.
     3     6X,"INTEGER D",/			!The finger to the cell of the moment.
     4     6X,"STORE = CHAR(0)",/		!Clear to nulls, not spaces.
     5     6X,"D = 1",/)			!Start the data finger at the first cell.
          NLABEL = 0		!No labels seen.
          INDEEP = 0		!So, the stack is empty.
          LABEL = 0		!And the current label is absent.
          L = 1			!Start at the start.
Chug through the PROG.
          DO WHILE(L.LE.LEN(PROG))	!And step through to the end.
            C = PROG(L:L)		!The code of the moment.
            SELECT CASE(C)		!What to do?
             CASE(">")			!Move the data finger forwards one.
              WRITE (SOURCE,2) "D = D + ",RATTLE(">")	!But, catch multiple steps.
             CASE("<")			!Move the data finger back one.
              WRITE (SOURCE,2) "D = D - ",RATTLE("<")	!Rather than a sequence of one steps.
             CASE("+")			!Increment the fingered datum by one.
              WRITE (SOURCE,2) "STORE(D) = CHAR(ICHAR(STORE(D)) + ",	!Catching multiple increments.
     1         RATTLE("+"),")"						!And being careful over the placement of brackets.
             CASE("-")			!Decrement the fingered datum by one.
              WRITE (SOURCE,2) "STORE(D) = CHAR(ICHAR(STORE(D)) - ",	!Catching multiple decrements.
     1         RATTLE("-"),")"						!And closing brackets.
             CASE(".")			!Write a character.
              WRITE (SOURCE,2) "WRITE (",OUF,",'(A1,$)') STORE(D)"	!Using the given output unit.
             CASE(",")			!Read a charactger.
              WRITE (SOURCE,2) "READ (",INF,",'(A1)') STORE(D)"		!And the input unit.
             CASE("[")			!A label!
              NLABEL = NLABEL + 1		!Labels come in pairs due to [...]
              LABEL = 2*NLABEL - 1		!So this belongs to the [.
              INDEEP = INDEEP + 1		!I need to remember when later the ] is encountered.
              STACK(INDEEP) = LABEL + 1		!This will be the other label.
              WRITE (SOURCE,2) "IF (ICHAR(STORE(D)).EQ.0) GO TO ",	!So, go thee, therefore.
     1         STACK(INDEEP)			!Its placement will come, all going well.
             CASE("]")			!The end of a [...] pair.
              LABEL = STACK(INDEEP)		!This was the value of the label to be, now to be placed.
              WRITE (SOURCE,2) "IF (ICHAR(STORE(D)).NE.0) GO TO ",	!The conditional part
     1         LABEL - 1			!The branch back destination is known by construction.
              INDEEP = INDEEP - 1		!And we're out of the [...] sequence's consequences.
             CASE DEFAULT		!All others are ignored.
              SOURCE = "CONTINUE"		!So, just carry on.
            END SELECT			!Enough of all that.
    2       FORMAT (A,I0,A)	!Text, an integer, text.
Cast forth the statement.
            IF (LABEL.LE.0) THEN	!Is a label waiting?
              WRITE (F,3) SOURCE		!No. Just roll the source.
    3         FORMAT (<6 + 2*MIN(12,INDEEP)>X,A)!With indentation.
             ELSE			!But if there is a label,
              WRITE (F,4) LABEL,SOURCE		!Slightly more complicated.
    4         FORMAT (I5,<1 + 2*MIN(12,INDEEP)>X,A)	!I align my labels rightwards...
              LABEL = 0				!It is used.
            END IF			!So much for that statement.
            L = L + 1		!Advance to the next command.
          END DO		!And perhaps we're finished.

Closedown.
          WRITE (F,100)		!No more source.
  100     FORMAT (6X,"END")	!So, this is the end.
         CONTAINS	!A function with odd effects.
          INTEGER FUNCTION RATTLE(C)	!Advances thrugh multiple C, counting them.
           CHARACTER*1 C	!The symbol.
            RATTLE = 1		!We have one to start with.
    1       IF (L.LT.LEN(PROG)) THEN	!Further text to look at?
              IF (PROG(L + 1:L + 1).EQ.C) THEN	!Yes. The same again?
              	L = L + 1		!Yes. Advance the finger to it.
                RATTLE = RATTLE + 1	!Count another.
                GO TO 1			!And try again.
              END IF			!Rather than just one at a time.
            END IF			!Curse the double evaluation of WHILE(L < LEN(PROG) & ...)
          END FUNCTION RATTLE	!Computers excel at counting.
        END SUBROUTINE BRAINFORT!Their only need be direction as to what to count...
      END MODULE BRAIN	!Simple in itself.

      PROGRAM POKE	!A tester.
      USE BRAIN		!In a rather bad way.
      CHARACTER*1 STORE(30000)	!Probably rather more than is needed.
      CHARACTER*(*) HELLOWORLD	!Believe it or not...
      PARAMETER (HELLOWORLD = "++++++++[>++++[>++>+++>+++>+<<<<-]"
     1 //" >+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------"
     2 //".--------.>>+.>++.")
      INTEGER F
      KBD = 5		!Standard input.
      MSG = 6		!Standard output.
      F = 10

      STORE = CHAR(0)	!Scrub.

c      CALL RUN(HELLOWORLD,STORE)	!Have a go.

      OPEN (F,FILE="BrainFort.for",STATUS="REPLACE",ACTION="WRITE")
      CALL BRAINFORT(HELLOWORLD,30000,KBD,MSG,F)
      END	!Enough.
```

And the output is...
```Fortran
      PROGRAM BRAINFORT
Code: ++++++++[>++++[>++>+++>+++>+<<<<-] >+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
      CHARACTER*1 STORE(30000)
      INTEGER D
      STORE = CHAR(0)
      D = 1

      STORE(D) = CHAR(ICHAR(STORE(D)) + 8)
    1   IF (ICHAR(STORE(D)).EQ.0) GO TO 2
        D = D + 1
        STORE(D) = CHAR(ICHAR(STORE(D)) + 4)
    3     IF (ICHAR(STORE(D)).EQ.0) GO TO 4
          D = D + 1
          STORE(D) = CHAR(ICHAR(STORE(D)) + 2)
          D = D + 1
          STORE(D) = CHAR(ICHAR(STORE(D)) + 3)
          D = D + 1
          STORE(D) = CHAR(ICHAR(STORE(D)) + 3)
          D = D + 1
          STORE(D) = CHAR(ICHAR(STORE(D)) + 1)
          D = D - 4
          STORE(D) = CHAR(ICHAR(STORE(D)) - 1)
    4   IF (ICHAR(STORE(D)).NE.0) GO TO 3
        CONTINUE
        D = D + 1
        STORE(D) = CHAR(ICHAR(STORE(D)) + 1)
        D = D + 1
        STORE(D) = CHAR(ICHAR(STORE(D)) + 1)
        D = D + 1
        STORE(D) = CHAR(ICHAR(STORE(D)) - 1)
        D = D + 2
        STORE(D) = CHAR(ICHAR(STORE(D)) + 1)
    5     IF (ICHAR(STORE(D)).EQ.0) GO TO 6
          D = D - 1
    6   IF (ICHAR(STORE(D)).NE.0) GO TO 5
        D = D - 1
        STORE(D) = CHAR(ICHAR(STORE(D)) - 1)
    2 IF (ICHAR(STORE(D)).NE.0) GO TO 1
      D = D + 2
      WRITE (6,'(A1,$)') STORE(D)
      D = D + 1
      STORE(D) = CHAR(ICHAR(STORE(D)) - 3)
      WRITE (6,'(A1,$)') STORE(D)
      STORE(D) = CHAR(ICHAR(STORE(D)) + 7)
      WRITE (6,'(A1,$)') STORE(D)
      WRITE (6,'(A1,$)') STORE(D)
      STORE(D) = CHAR(ICHAR(STORE(D)) + 3)
      WRITE (6,'(A1,$)') STORE(D)
      D = D + 2
      WRITE (6,'(A1,$)') STORE(D)
      D = D - 1
      STORE(D) = CHAR(ICHAR(STORE(D)) - 1)
      WRITE (6,'(A1,$)') STORE(D)
      D = D - 1
      WRITE (6,'(A1,$)') STORE(D)
      STORE(D) = CHAR(ICHAR(STORE(D)) + 3)
      WRITE (6,'(A1,$)') STORE(D)
      STORE(D) = CHAR(ICHAR(STORE(D)) - 6)
      WRITE (6,'(A1,$)') STORE(D)
      STORE(D) = CHAR(ICHAR(STORE(D)) - 8)
      WRITE (6,'(A1,$)') STORE(D)
      D = D + 2
      STORE(D) = CHAR(ICHAR(STORE(D)) + 1)
      WRITE (6,'(A1,$)') STORE(D)
      D = D + 1
      STORE(D) = CHAR(ICHAR(STORE(D)) + 2)
      WRITE (6,'(A1,$)') STORE(D)
      END
```

Which, when compiled and run, produces...

```txt

Hello World!

```

In a transcription error, I included a space in the Brain*uck code, which was of course ignored by the interpreter. The compiler initially spat out
```Fortran
  4   IF (ICHAR(STORE(D)).NE.0) GO TO 3
      IF (ICHAR(STORE(D)).NE.0) GO TO 3
```
 because the CASE statement was followed by writing SOURCE out and the no-op had not changed it; the Fortran compiler made no complaint about the obviously pointless replication. So much for ''its'' analysis. For such "no-op" codes, fortran's CONTINUE statement is an obvious "no action" match.



## FreeBASIC


```freebasic

' Intérprete de brainfuck
' FB 1.05.0 Win64
'

Const BF_error_memoria_saturada As Integer = 2
Const BF_error_memoria_insuficiente As Integer = 4
Const BF_error_codigo_saturado As Integer = 8
Const BF_error_desbordamiento_codigo As Integer = 16

Dim BFcodigo As String = ">++++++++++[>+++>+++++++>++++++++++>+++++++++++>++++++++++++>++++++++++++++++[<]>-]>>>>>>+.<<<<++.>>+.---.<---.<<++.>>>+.>---.<+.<+++.>+.<<<+."
Dim codigo_error As Integer

Function EjecutarBF (BFcodigo As String, tammem As Uinteger) As Integer
    Dim As String memoria = String(tammem, 0)
    Dim As Uinteger puntero_instrucciones, puntero_datos
    Dim As Integer nivel_de_alcance

    For puntero_instrucciones = 0 To Len(BFcodigo)
        Select Case Chr(BFcodigo[puntero_instrucciones])
        Case ">"
            puntero_datos += 1
            If (puntero_datos > tammem - 1) Then Return BF_error_memoria_saturada
        Case "<"
            puntero_datos -= 1
            If (puntero_datos > tammem - 1) Then Return BF_error_memoria_insuficiente
        Case "+"
            memoria[puntero_datos] += 1
        Case "-"
            memoria[puntero_datos] -= 1
        Case "."
            Print Chr(memoria[puntero_datos]);
        Case ","
            memoria[puntero_datos] = Asc(Input(1))
        Case "["
            If (memoria[puntero_datos] = 0) Then
                Dim nivel_antiguo As Uinteger = nivel_de_alcance
                nivel_de_alcance += 1
                Do Until (nivel_de_alcance = nivel_antiguo)
                    puntero_instrucciones += 1
                    If (puntero_instrucciones > Len(BFcodigo) - 1) Then Return BF_error_codigo_saturado
                    Select Case Chr(BFcodigo[puntero_instrucciones])
                    Case "["
                        nivel_de_alcance += 1
                    Case "]"
                        nivel_de_alcance -= 1
                    End Select
                Loop
            Else
                nivel_de_alcance += 1
            End If
            Continue For
        Case "]"
            If (memoria[puntero_datos] = 0) Then
                nivel_de_alcance -= 1
                Continue For
            Else
                Dim nivel_antiguo As Integer = nivel_de_alcance
                nivel_de_alcance -= 1
                Do Until (nivel_de_alcance = nivel_antiguo)
                    puntero_instrucciones -= 1
                    If (puntero_instrucciones > Len(BFcodigo) - 1) Then Return BF_error_desbordamiento_codigo
                    Select Case Chr(BFcodigo[puntero_instrucciones])
                    Case "["
                        nivel_de_alcance += 1
                    Case "]"
                        nivel_de_alcance -= 1
                    End Select
                Loop
            End If
            Continue For
        Case Else
            Continue For
        End Select
    Next puntero_instrucciones
    Return -1
End Function


Cls
codigo_error = EjecutarBF(BFcodigo, 1024)
If codigo_error Then
    Sleep
Else
    Print "codigo de error: " & codigo_error
End If
End

```

```txt

íHola mundo!

```



=={{header|F_Sharp|F#}}==
[[/F Sharp|Implementation in F#]].



## GAP


```gap
# Here . and , print and read an integer, not a character
Brainfuck := function(prog)
  local pointer, stack, leftcells, rightcells, instr, stackptr, len,
    output, input, jump, i, j, set, get;
  input := InputTextUser();
  output := OutputTextUser();
  instr := 1;
  pointer := 0;
  leftcells := [ ];
  rightcells := [ ];
  stack := [ ];
  stackptr := 0;
  len := Length(prog);
  jump := [ ];

  get := function()
    local p;
    if pointer >= 0 then
      p := pointer + 1;
      if IsBound(rightcells[p]) then
        return rightcells[p];
      else
        return 0;
      fi;
    else
      p := -pointer;
      if IsBound(leftcells[p]) then
        return leftcells[p];
      else
        return 0;
      fi;
    fi;
  end;

  set := function(value)
    local p;
    if pointer >= 0 then
      p := pointer + 1;
      if value = 0 then
        Unbind(rightcells[p]);
      else
        rightcells[p] := value;
      fi;
    else
      p := -pointer;
      if value = 0 then
        Unbind(leftcells[p]);
      else
        leftcells[p] := value;
      fi;
    fi;
  end;

  # find jumps for faster execution
  for i in [1 .. len] do
    if prog[i] = '[' then
      stackptr := stackptr + 1;
      stack[stackptr] := i;
    elif prog[i] = ']' then
      j := stack[stackptr];
      stackptr := stackptr - 1;
      jump[i] := j;
      jump[j] := i;
    fi;
  od;

  while instr <= len do
    c := prog[instr];
    if c = '<' then
      pointer := pointer - 1;
    elif c = '>' then
      pointer := pointer + 1;
    elif c = '+' then
      set(get() + 1);
    elif c = '-' then
      set(get() - 1);
    elif c = '.' then
      WriteLine(output, String(get()));
    elif c = ',' then
      set(Int(Chomp(ReadLine(input))));
    elif c = '[' then
      if get() = 0 then
        instr := jump[instr];
      fi;
    elif c = ']' then
      if get() <> 0 then
        instr := jump[instr];
      fi;
    fi;
    instr := instr + 1;
  od;
  CloseStream(input);
  CloseStream(output);
  # for debugging purposes, return last state
  return [leftcells, rightcells, pointer];
end;

# An addition
Brainfuck("+++.<+++++.[->+<]>.");
# 3
# 5
# 8
```


## Go

Fixed size data store, no bounds checking.

```go
package main

import "fmt"

func main() {
    // example program is current Brainfuck solution to
    // Hello world/Text task.  only requires 10 bytes of data store!
    bf(10, `++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++
++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
<+++++++.--------.<<<<<+.<+++.---.`)
}

func bf(dLen int, is string) {
    ds := make([]byte, dLen) // data store
    var dp int               // data pointer
    for ip := 0; ip < len(is); ip++ {
        switch is[ip] {
        case '>':
            dp++
        case '<':
            dp--
        case '+':
            ds[dp]++
        case '-':
            ds[dp]--
        case '.':
            fmt.Printf("%c", ds[dp])
        case ',':
            fmt.Scanf("%c", &ds[dp])
        case '[':
            if ds[dp] == 0 {
                for nc := 1; nc > 0; {
                    ip++
                    if is[ip] == '[' {
                        nc++
                    } else if is[ip] == ']' {
                        nc--
                    }
                }
            }
        case ']':
            if ds[dp] != 0 {
                for nc := 1; nc > 0; {
                    ip--
                    if is[ip] == ']' {
                        nc++
                    } else if is[ip] == '[' {
                        nc--
                    }
                }
            }
        }
    }
}
```

```txt

Goodbye, World!

```



## Groovy



```groovy
class BrainfuckProgram {

    def program = '', memory = [:]
    def instructionPointer = 0, dataPointer = 0

    def execute() {
        while (instructionPointer < program.size())
            switch(program[instructionPointer++]) {
            case '>': dataPointer++; break;
            case '<': dataPointer--; break;
            case '+': memory[dataPointer] = memoryValue + 1; break
            case '-': memory[dataPointer] = memoryValue - 1; break
            case ',': memory[dataPointer] = System.in.read(); break
            case '.': print String.valueOf(Character.toChars(memoryValue)); break
            case '[': handleLoopStart(); break
            case ']': handleLoopEnd(); break
            }
    }

    private getMemoryValue() { memory[dataPointer] ?: 0 }

    private handleLoopStart() {
        if (memoryValue) return

        int depth = 1
        while (instructionPointer < program.size())
            switch(program[instructionPointer++]) {
            case '[': depth++; break
            case ']': if (!(--depth)) return
            }
        throw new IllegalStateException('Could not find matching end bracket')
    }

    private handleLoopEnd() {
        int depth = 0
        while (instructionPointer >= 0) {
            switch(program[--instructionPointer]) {
            case ']': depth++; break
            case '[': if (!(--depth)) return; break
            }
        }
        throw new IllegalStateException('Could not find matching start bracket')
    }
}
```

Testing:

```groovy
new BrainfuckProgram(program: '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.').execute()
```

```txt
Hello World!
```



## Haskell


[[/Haskell|Implementation in Haskell]].

=={{header|Icon}} and {{header|Unicon}}==
[[/Icon|Implementation in Icon/Unicon]].


## J


[[/J|Implementation in J]].


## Java


[[/Java|Implementation in Java]].


## JavaScript


[[/JavaScript|Implementation in JavaScript]].


## Jsish

Part of the Jsi source kit unit tests. bf code from Hello World/text task entry.


```javascript
/*
 * javascript bf interpreter
 * by wenxichang@163.com
 */

function execute(code)
{
    var mem = new Array(30000);
    var sp = 10000;
    var opcode = new String(code);
    var oplen = opcode.length;
    var ip = 0;
    var loopstack = new Array();
    var output = "";

    for (var i = 0; i < 30000; ++i) mem[i] = 0;

    while (ip < oplen) {
        switch(opcode[ip]) {
            case '+':
                mem[sp]++;
                break;
            case '-':
                mem[sp]--;
                break;
            case '>':
                sp++;
                break;
            case '<':
                sp--;
                break;
            case '.':
                if (mem[sp] != 10 && mem[sp] != 13) {
                    output = output + Util.fromCharCode(mem[sp]);
                } else {
                    puts(output);
                    output = "";
                }
                break;
            case ',':
                var s = console.input();
                if (!s) exit(0);

                mem[sp] = s.charCodeAt(0);
                break;
            case '[':
                if (mem[sp]) {
                    loopstack.push(ip);
                } else {
                    for (var k = ip, j = 0; k < oplen; k++) {
                        opcode[k] == '[' && j++;
                        opcode[k] == ']' && j--;
                        if (j == 0) break;
                    }
                    if (j == 0) ip = k;
                    else {
                        puts("Unmatched loop");
                        return false;
                    }
                }
                break;
            case ']':
                ip = loopstack.pop() - 1;
                break;
            default:
                break;
        }
        ip++;
    }
    return true;
};

if (Interp.conf('unitTest') > 0)  execute('
    ++++++++++[>+>+++>++++>+++++++ >++++++++>+++++++++>++++++++++>+++++++++
    ++>++++++++++++<<<<<<<<<-]>>>>+.>>>>+..<.<++++++++.>>>+.<<+.<<<<++++.<+
    +.>>>+++++++.>>>.+++.<+++++++.--------.<<<<<+.<+++.---.
');
```


```txt
prompt$ jsish --U bf.jsi
Goodbye, World!

```



## Julia

```julia
using DataStructures

function execute(src)
    pointers = Dict{Int,Int}()
    stack    = Int[]
    for (ptr, opcode) in enumerate(src)
        if opcode == '[' push!(stack, ptr) end
        if opcode == ']'
            if isempty(stack)
                src = src[1:ptr]
                break
            end
            sptr = pop!(stack)
            pointers[ptr], pointers[sptr] = sptr, ptr
        end
    end
    if ! isempty(stack) error("unclosed loops at $stack") end
    tape = DefaultDict{Int,Int}(0)
    cell, ptr = 0, 1
    while ptr ≤ length(src)
        opcode = src[ptr]
        if     opcode == '>' cell += 1
        elseif opcode == '<' cell -= 1
        elseif opcode == '+' tape[cell] += 1
        elseif opcode == '-' tape[cell] -= 1
        elseif opcode == ',' tape[cell] = Int(read(STDIN, 1))
        elseif opcode == '.' print(STDOUT, Char(tape[cell]))
        elseif (opcode == '[' && tape[cell] == 0) ||
               (opcode == ']' && tape[cell] != 0) ptr = pointers[ptr]
        end
        ptr += 1
    end
end

const src = """\
    >++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>
    >+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++."""
execute(src)
```


```txt
Hello World!
```



## Kotlin

```scala
// version 1.1.2

class Brainf__k(val prog: String, memSize: Int) {
    private val mem = IntArray(memSize)
    private var ip = 0
    private var dp = 0
    private val memVal get() = mem.getOrElse(dp) { 0 }

    fun execute() {
        while (ip < prog.length) {
            when (prog[ip++]) {
                '>' -> dp++
                '<' -> dp--
                '+' -> mem[dp] = memVal + 1
                '-' -> mem[dp] = memVal - 1
                ',' -> mem[dp] = System.`in`.read()
                '.' -> print(memVal.toChar())
                '[' -> handleLoopStart()
                ']' -> handleLoopEnd()
            }
        }
    }

    private fun handleLoopStart() {
        if (memVal != 0) return
        var depth = 1
        while (ip < prog.length) {
            when (prog[ip++]) {
                '[' -> depth++
                ']' -> if (--depth == 0) return
            }
        }
        throw IllegalStateException("Could not find matching end bracket")
    }

    private fun handleLoopEnd() {
        var depth = 0
        while (ip >= 0) {
            when (prog[--ip]) {
                ']' -> depth++
                '[' -> if (--depth == 0) return
            }
        }
        throw IllegalStateException("Could not find matching start bracket")
    }
}

fun main(args: Array<String>) {
    val prog = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
    Brainf__k(prog, 10).execute()
}
```


```txt

Hello World!

```



## Limbo


Expects the program to be the first argument, compiles to bytecode (without optimization), uses a 1MB array of cells (and wraps), includes some rudimentary compiler diagnostics.


```Limbo
implement Bf;

include "sys.m"; sys: Sys;
include "draw.m";

Bf: module {
	init: fn(nil: ref Draw->Context, args: list of string);
	ARENASZ: con 1024 * 1024;
	EXIT, INC, DEC, JZ, JNZ, INCP, DECP, READ, WRITE: con iota;
};

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	args = tl args;
	if(args == nil || len args != 1) {
		sys->fprint(sys->fildes(2), "usage: bf program");
		raise "fail:usage";
	}
	code := compile(hd args);
	execute(code, array[ARENASZ] of { * => byte 0 });
}

compile(p: string): array of int
{
	marks: list of int = nil;
	code := array[len p * 2 + 1] of { * => EXIT };
	pc := 0;
	for(i := 0; i < len p; i++) {
		case p[i] {
		'-' => code[pc++] = DEC;
		'+' => code[pc++] = INC;
		'<' => code[pc++] = DECP;
		'>' => code[pc++] = INCP;
		',' => code[pc++] = READ;
		'.' => code[pc++] = WRITE;
		'[' =>
			code[pc++] = JZ;
			marks = pc++ :: marks;
		']' =>
			if(marks == nil) {
				sys->fprint(sys->fildes(2), "bf: unmatched ']' at character %d.", pc);
				raise "fail:errors";
			}
			c := hd marks;
			marks = tl marks;
			code[pc++] = JNZ;
			code[c] = pc;
			code[pc++] = c;
		}
	}
	if(marks != nil) {
		sys->fprint(sys->fildes(2), "bf: unmatched '['.");
		raise "fail:errors";
	}
	return code;
}

execute(code: array of int, arena: array of byte)
{
	pc := 0;
	p := 0;
	buf := array[1] of byte;
	stopreading := 0;
	for(;;) {
		case code[pc] {
		DEC => arena[p]--;
		INC => arena[p]++;
		DECP =>
			p--;
			if(p < 0)
				p = len arena - 1;
		INCP =>
			p = (p + 1) % len arena;
		READ =>
			if(!stopreading) {
				n := sys->read(sys->fildes(0), buf, 1);
				if(n < 1) {
					arena[p] = byte 0;
					stopreading = 1;
				} else {
					arena[p] = buf[0];
				}
			}
		WRITE =>
			buf[0] = arena[p];
			sys->write(sys->fildes(1), buf, 1);
		JNZ =>
			if(arena[p] != byte 0)
				pc = code[pc + 1];
			else
				pc++;
		JZ =>
			if(arena[p] == byte 0)
				pc = code[pc + 1];
			else
				pc++;
		EXIT => return;
		}
		pc++;
	}
}

```


Using the example code from [[Hello world/Text]]:

```txt
% bf '++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++
++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
<+++++++.--------.<<<<<+.<+++.---.'
Goodbye, World!


```



## Lua


[[/Lua|Implementation in Lua]].

===Simple meta-implementation using <code>load</code>===


```Lua
local funs = {
['>'] = 'ptr = ptr + 1; ',
['<'] = 'ptr = ptr - 1; ',
['+'] = 'mem[ptr] = mem[ptr] + 1; ',
['-'] = 'mem[ptr] = mem[ptr] - 1; ',
['['] = 'while mem[ptr] ~= 0 do ',
[']'] = 'end; ',
['.'] = 'io.write(string.char(mem[ptr])); ',
[','] = 'mem[ptr] = (io.read(1) or "\\0"):byte(); ',
}

local prog = [[
  local mem = setmetatable({}, { __index = function() return 0 end})
  local ptr = 1
]]

local source = io.read('*all')

for p = 1, #source do
  local snippet = funs[source:sub(p,p)]
  if snippet then prog = prog .. snippet end
end

load(prog)()
```


BTW very fast, considering how simple it is.


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      \\ Brainfuck Compiler

      Escape Off
      \\ no Esc function so we can use Ctrl+Z when input characters to terminate BF
      \\ ctrl+c open dialog for exit - by default in console mode

      Const skipmonitor as boolean=true, output as boolean=True
      Const ob$="{",cb$="}"
      Gosub CallOne
      \\ We use a group object with events.

      Group WithEvents BF=BrainF()

      Function BF_monitor {
            \\ Event functions have same scope as the module where belong
            If skipmonitor Then exit
            Read New pc, mem
            Print pc, mem
            Print "Press space bar": While Key$<>" " {}
      }
      Function BF_newline {
            If not skipmonitor then Print "newline" : exit
            if output then Print
      }
      Function BF_print {
            Read New c$
            If not skipmonitor then Print "character:";c$  : exit
            if output then Print c$;
      }

      Program$ = {++++++[>++++++++++++<-]>.
                  >++++++++++[>++++++++++<-]>+.
                  +++++++..+++.>++++[>+++++++++++<-]>.
                  <+++[>----<-]>.<<<<<+++[>+++++<-]>.
                  >>.+++.------.--------.>>+.
                  }
      Report Program$
      ExecBF(Program$)
      End

      Sub ExecBF(Code$)
            ClearMem()
            code$=filter$(code$, " "+chr$(10)+chr$(13))
            code$<=replace$(".","@", code$)
            code$<=replace$("-","-.D()", code$)
            code$<=replace$("+","-.A()", code$)
            code$<=replace$("<","-.L()", code$)
            code$<=replace$(">","-.R()", code$)
            code$<=replace$("@","-.P()", code$)
            code$<=replace$("[","-.S("+ob$,code$)
            code$<=replace$("]",cb$+")",code$)
            code$<=replace$(",","-.K()", code$)
            Rem : Print code$
            BF.Eval code$
            Print
      End Sub
      Sub ClearMem()
            Dim cMem(1 to 30000)=0
            For BF {
                  .Pc=1
                  .Zero=True
                  .Mem()=cMem()
            }
      End Sub
      CallOne:
      Class BrainF {
            events "monitor", "newline", "print"
            Dim Mem()
            Pc=1, Zero as Boolean=True
            Module UpdateZero {
                  .Zero<=.Mem(.Pc)=0
                  call event "monitor", .pc, .Mem(.pc)
            }
            Function A {   \\ +
                  .Mem(.Pc)++
                  .UpdateZero
            }
            Function D {  \\ -
                  .Mem(.Pc)--
                  .UpdateZero
            }
            Function R { \\ >
                  If .Pc=30000 Then Error "Upper Bound Error"
                  .Pc++
                  .UpdateZero
            }
            Function L { \\ <
                  If .Pc=1 Then Error "Lower Bound Error"
                  .Pc--
                  .UpdateZero
            }
            Function P { \\ .
                  Select Case .Mem(.Pc)
                  Case >31
                        Call Event "print", Chr$(.Mem(.Pc))
                  Case 10
                        Call Event "newline"
                  End Select
            }
            Function K {  \\ ,
                  .Mem(.Pc)=Asc(Key$)
                  \\ ctrl+z for exit
                  If .Mem(.Pc)=26 Then  Error "Finished"
                   .UpdateZero
            }
            Function S  { \\ [
                  If .Zero then =0: exit
                  Read newEval$
                  Do {ret=Eval(newEval$)} until .Zero
            }
            Module Eval {
                  ret=eval(Letter$)
            }
      }
      Return
}
Checkit

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

<lang>bf[program_, input_] :=
  Module[{p = Characters[program], pp = 0, m, mp = 0, bc = 0,
      instr = StringToStream[input]},
    m[_] = 0;
    While[pp < Length@p,
      pp++;
      Switch[p[[pp]],
        ">", mp++,
        "<", mp--,
        "+", m[mp]++,
        "-", m[mp]--,
        ".", BinaryWrite["stdout", m[mp]],
        ",", m[mp] = BinaryRead[instr],
        "[", If[m[mp] == 0,
          bc = 1;
          While[bc > 0, pp++; Switch[p[[pp]], "[", bc++, "]", bc--]]],
        "]", If[m[mp] != 0,
          bc = -1;
          While[bc < 0, pp--; Switch[p[[pp]], "[", bc++, "]", bc--]]]]];
    Close[instr];];
bf[program_] := bf[program, ""]
```


Example:

<lang>bf["++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.
<<+++++++++++++++.>.+++.------.--------.>+.>."]
```


```txt
Hello World!
```


=={{header|Modula-3}}==

[[/Modula-3|Implementation in Modula-3]].


## Nim


```nim
import os

var
  code = if paramCount() > 0: readFile paramStr 1
         else: readAll stdin
  tape = newSeq[char]()
  d    = 0
  i    = 0

proc run(skip = false): bool =
  while d >= 0 and i < code.len:
    if d >= tape.len: tape.add '\0'

    if code[i] == '[':
      inc i
      let p = i
      while run(tape[d] == '\0'): i = p
    elif code[i] == ']':
      return tape[d] != '\0'
    elif not skip:
      case code[i]
      of '+': inc tape[d]
      of '-': dec tape[d]
      of '>': inc d
      of '<': dec d
      of '.': stdout.write tape[d]
      of ',': tape[d] = stdin.readChar
      else: discard

    inc i

discard run()
```



## Objeck


```objeck
class Brainfu_k {
  @program : String; @mem : Int[];
  @ip : Int;  @dp : Int;

  New(program : String, size : Int) {
    @program := program;
    @mem := Int → New[size];
  }

  function : Main(args : String[]) ~ Nil {
    if(args → Size() = 2) {
      Brainfu_k → New(args[0], args[1] → ToInt()) → Execute();
    };
  }

  method : Execute() ~ Nil {
    while(@ip < @program → Size()) {
      instr := @program → Get(@ip);
      select(instr) {
        label '>': { @dp += 1; }
        label '<': { @dp -= 1; }
        label '+': { @mem[@dp] := @mem[@dp] + 1; }
        label '-': { @mem[@dp] := @mem[@dp] - 1; }
        label '.': { value := @mem[@dp] → As(Char); value → Print(); }
        label ',': { @mem[@dp] := Read(); }
        label '[': { JumpForward(); }
        label ']': { JumpBack(); }
      };
      @ip += 1;
    };
  }

  method : JumpForward() ~ Nil {
    depth := 1;
    if(@mem[@dp] = 0) {
      while(@ip < @program → Size()) {
        instr := @program → Get(@ip);
        if(instr = ']') {
          depth -= 1;  if(depth = 0) { return;  };
        }
        else if(instr = '[') { depth += 1; };
        @ip += 1;
      };
      "*** Unbalanced jump ***" → ErrorLine();
      Runtime → Exit(1);
    };
  }

  method : JumpBack() ~ Nil {
    depth := 1;
    if(@mem[@dp] <> 0) {
      while(@ip > 0) {
        @ip -= 1;
        instr := @program → Get(@ip);
        if(instr = '[') {
          depth -= 1;  if(depth = 0) { return; };
        }
        else if(instr = ']') { depth += 1; };
      };
      "*** Unbalanced jump ***" → ErrorLine();
      Runtime → Exit(1);
    };
  }

  method : Read() ~ Int {
    in := IO.Console → ReadString();
    if(in → Size() > 0) { return in → ToInt(); };
    return 0;
  }
}
```



## OCaml


[[/OCaml|Implementation in OCaml]].


## Ol

(without input operator ",")

```ol

(define (bf program stack-length)
   (let ((program (string-append program "]"))
         (program-counter 0)
         (stack (make-vector stack-length 0))
         (stack-pointer 0))
      (letrec ((skip (lambda (PC sp)
                        (let loop ((pc PC) (sp sp))
                           (let ((ch (string-ref program pc))
                                 (pc (+ pc 1)))
                              (case ch
                                 (#\]  (list pc sp))
                                 (#\[  (apply loop (skip pc sp)))
                                 (else
                                    (loop pc sp)))))))
               (step (lambda (PC SP)
                        (let loop ((pc PC) (sp SP))
                           (let ((ch (string-ref program pc))
                                 (pc (+ pc 1)))
                              (case ch
                                 (#\]  (list (- PC 1) sp))
                                 (#\[  (if (eq? (vector-ref stack sp) 0)
                                          (apply loop (skip pc sp))
                                          (apply loop (step pc sp))))
                                 (#\+  (set-ref! stack sp (+ (vector-ref stack sp) 1))
                                       (loop pc sp))
                                 (#\-  (set-ref! stack sp (- (vector-ref stack sp) 1))
                                       (loop pc sp))
                                 (#\>  (loop pc (+ sp 1)))
                                 (#\<  (loop pc (- sp 1)))
                                 (#\.  (display (make-string 1 (vector-ref stack sp)))
                                       (loop pc sp))
                                 (else
                                    (loop pc sp))))))))
         (step 0 0))))

; testing:
; (bf ",++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." 30000)
; ==> Hello World!
; (bf ">>++++[<++++[<++++>-]>-]<<.[-]++++++++++." 30000)
; ==> @

```



## PARI/GP

A case statement would have been really useful here...

```parigp
BF(prog)={
	prog=Vec(Str(prog));
	my(codeptr,ptr=1,v=vector(1000),t);
	while(codeptr++ <= #prog,
		t=prog[codeptr];
		if(t=="+",
			v[ptr]++
		,
			if(t=="-",
				v[ptr]--
			,
				if(t==">",
					ptr++
				,
					if(t=="<",
						ptr--
					,
						if(t=="[" && !v[ptr],
							t=1;
							while(t,
								if(prog[codeptr++]=="[",t++);
								if(prog[codeptr]=="]",t--)
							);
						);
						if(t=="]"&&v[ptr],
							t=1;
							while(t,
								if(prog[codeptr--]=="[",t--);
								if(prog[codeptr]=="]",t++)
							)
						);
						if(t==".",
							print1(Strchr(v[ptr]))
						);
						if(t==",",
							v[ptr]=Vecsmall(input)[1]
						)
					)
				)
			)
		)
	)
};
```


## Pascal


```Pascal

program rcExceuteBrainF;

uses
     Crt;

Const
  DataSize= 1024;                           // Size of Data segment
  MaxNest=  1000;                           // Maximum nesting depth of []

procedure ExecuteBF(Source: string);
var
  Dp:       pByte;                          // Used as the Data Pointer
  DataSeg:  Pointer;                        // Start of the DataSegment (Cell 0)
  Ip:       pChar;                          // Used as instruction Pointer
  LastIp:   Pointer;                        // Last adr of code.
  JmpStack: array[0..MaxNest-1] of pChar;   // Stack to Keep track of active "[" locations
  JmpPnt:   Integer;                        // Stack pointer ^^
  JmpCnt:   Word;                           // Used to count brackets when skipping forward.


begin

  // Set up then data segment
  getmem(DataSeg,dataSize);
  dp:=DataSeg;
  fillbyte(dp^,dataSize,0);

  // Set up the JmpStack
  JmpPnt:=-1;

  // Set up Instruction Pointer
  Ip:=@Source[1];
  LastIp:=@Source[length(source)];
  if Ip=nil then exit;

  // Main Execution loop
  repeat { until Ip > LastIp }
    Case Ip^ of
      '<': dec(dp);
      '>': inc(dp);
      '+': inc(dp^);
      '-': dec(dp^);
      '.': write(stdout,chr(dp^));
      ',': dp^:=ord(readkey);
      '[': if dp^=0 then
           begin
             // skip forward until matching bracket;
             JmpCnt:=1;
             while (JmpCnt>0) and (ip<=lastip) do
             begin
               inc(ip);
               Case ip^ of
                 '[': inc(JmpCnt);
                 ']': dec(JmpCnt);
                 #0:  begin
                        Writeln(StdErr,'Error brackets don''t match');
                        halt;
                      end;
                end;
             end;
           end else begin
             // Add location to Jump stack
             inc(JmpPnt);
             JmpStack[jmpPnt]:=ip;
           end;
      ']': if dp^>0 then
             // Jump Back to matching [
             ip:=JmpStack[jmpPnt]
           else
             // Remove Jump from stack
             dec(jmpPnt);
    end;
    inc(ip);
  until Ip>lastIp;
  freemem(DataSeg,dataSize);
end;

Const
  HelloWorldWiki = '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>'+
                   '---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.';

  pressESCtoCont = '>[-]+++++++[<++++++++++>-]<->>[-]+++++++[<+++++++++++'+
                   '+>-]<->>[-]++++[<++++++++>-]+>[-]++++++++++[<++++++++'+
                   '++>-]>[-]++++++++[<++++++++++++++>-]<.++.+<.>..<<.<<.'+
                   '-->.<.>>.>>+.-----.<<.[<<+>>-]<<.>>>>.-.++++++.<++++.'+
                   '+++++.>+.<<<<++.>+[>+<--]>++++...';
  waitForEsc     = '[-]>[-]++++[<+++++++>-]<->[-]>+[[-]<<[>+>+<<-]'+'>>[<'+
                   '<+>>-],<[->-<]>]';

begin
  // Execute "Hello World" example from Wikipedia
  ExecuteBF(HelloWorldWiki);

  // Print text "press ESC to continue....." and wait for ESC to be pressed
  ExecuteBF(pressESCtoCont+waitForEsc);
end.


```



## Perl


[[/Perl|Implementation in Perl]].


## Perl 6


See [[Execute_Brainfuck/Perl_6]].


## Phix


```Phix
procedure bfi(string pgm)
sequence jumptable = repeat(0,length(pgm)),
         loopstack = {},
         data = repeat(0,10)    -- size??
integer skip = 0, ch, loopstart, pc, dp
    --
    -- compile (pack/strip comments and link jumps)
    --
    for i=1 to length(pgm) do
        ch = pgm[i]
        switch ch do
            case '[': loopstack = append(loopstack,i-skip);
                      pgm[i-skip] = ch;
            case ']': loopstart = loopstack[$];
                      loopstack = loopstack[1..-2];
                      jumptable[i-skip] = loopstart;
                      jumptable[loopstart] = i-skip;
                      fallthrough
            case '+','-','<','>',',','.': pgm[i-skip] = ch;
            default: skip += 1
        end switch
    end for
    if length(loopstack) then ?9/0 end if
    pgm = pgm[1..-1-skip]

    --
    -- main execution loop
    --
    pc = 1
    dp = 1
    while pc<=length(pgm) do
        ch = pgm[pc]
        switch ch do
            case '>': dp += 1 if dp>length(data) then dp = 1 end if
            case '<': dp -= 1 if dp<1 then dp = length(data) end if
            case '+': data[dp] += 1
            case '-': data[dp] -= 1
            case ',': data[dp] = getc(0)
            case '.': puts(1,data[dp])
            case '[': if data[dp]=0 then pc = jumptable[pc] end if
            case ']': if data[dp]!=0 then pc = jumptable[pc] end if
            default: ?9/0
        end switch
        pc += 1
    end while
end procedure

constant bf="++++++++[>++++[>++>++++>+++>+<<<<-]>++>->+>>+[<]<-]>>.>>.+.<.>>.<<<++.>---------.>------.<----.++++++++.>>+.>++.+++."
constant fb="++++++++[>++++[>++>++++>+++>+<<<<-]>++>->+>>+[<]<-]>>.>>.+.<.>>.<<<+++.>---.>------.++++++++.<--.>>+.>++.+++.,"

bfi(bf)
bfi(fb)
```

```txt

Phix Rocks!
Phix Sucks!

```



## PHP


```php
<?php
function brainfuck_interpret(&$s, &$_s, &$d, &$_d, &$i, &$_i, &$o) {
   do {
     switch($s[$_s]) {
       case '+': $d[$_d] = chr(ord($d[$_d]) + 1); break;
       case '-': $d[$_d] = chr(ord($d[$_d]) - 1); break;
       case '>': $_d++; if(!isset($d[$_d])) $d[$_d] = chr(0); break;
       case '<': $_d--; break;
       case '.': $o .= $d[$_d]; break;
       case ',': $d[$_d] = $_i==strlen($i) ? chr(0) : $i[$_i++]; break;
       case '[':
         if((int)ord($d[$_d]) == 0) {
           $brackets = 1;
           while($brackets && $_s++ < strlen($s)) {
             if($s[$_s] == '[')
               $brackets++;
             else if($s[$_s] == ']')
               $brackets--;
           }
         }
         else {
             $pos = $_s++-1;
           if(brainfuck_interpret($s, $_s, $d, $_d, $i, $_i, $o))
             $_s = $pos;
         }
         break;
       case ']': return ((int)ord($d[$_d]) != 0);
    }
  } while(++$_s < strlen($s));
}

function brainfuck($source, $input='') {
  $data         = array();
  $data[0]      = chr(0);
  $data_index   = 0;
  $source_index = 0;
  $input_index  = 0;
  $output       = '';

  brainfuck_interpret($source, $source_index,
                      $data,   $data_index,
                      $input,  $input_index,
                      $output);
  return $output;
}

$code = "
    >++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>
    >+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++.
";
$inp = '123';
print brainfuck( $code, $inp );

```



## PicoLisp

This solution uses a doubly-linked list for the cell space. That list consists
of a single cell initially, and grows automatically in both directions. The
value in each cell is unlimited.

```PicoLisp
(off "Program")

(de compile (File)
   (let Stack NIL
      (setq "Program"
         (make
            (in File
               (while (char)
                  (case @
                     (">"
                        (link
                           '(setq Data
                              (or
                                 (cddr Data)
                                 (con (cdr Data) (cons 0 (cons Data))) ) ) ) )
                     ("<"
                        (link
                           '(setq Data
                              (or
                                 (cadr Data)
                                 (set (cdr Data) (cons 0 (cons NIL Data))) ) ) ) )
                     ("+" (link '(inc Data)))
                     ("-" (link '(dec Data)))
                     ("." (link '(prin (char (car Data)))))
                     ("," (link '(set Data (char (read)))))
                     ("["
                        (link
                           '(setq Code
                              ((if (=0 (car Data)) cdar cdr) Code) ) )
                        (push 'Stack (chain (cons))) )
                     ("]"
                        (unless Stack
                           (quit "Unbalanced ']'") )
                        (link
                           '(setq Code
                              ((if (n0 (car Data)) cdar cdr) Code) ) )
                        (let (There (pop 'Stack)  Here (cons There))
                           (chain (set There Here)) ) ) ) ) ) ) )
      (when Stack
         (quit "Unbalanced '['") ) ) )

(de execute ()
   (let Data (cons 0 (cons))              # Create initial cell
      (for (Code "Program"  Code)         # Run program
         (eval (pop 'Code)) )
      (while (cadr Data)                  # Find beginning of data
         (setq Data @) )
      (filter prog Data '(T NIL .)) ) )   # Return data space
```

```txt
: (compile "hello.bf")
-> NIL

: (execute)
Goodbye, World!
-> (0 10 33 44 71 87 98 100 114 121)
```



### Alternative solution


```txt

# This implements a BrainFuck *interpreter* similar to the "official" one.
# It has 30000 unsigned 8-bit cells with wrapping, going off the bounds
# of the memory results in an error.
(de bf (Prg)
   (let (P Prg S NIL D (need 30000 0) Dp D F T )
      (while P
         (case (car P)
            ("+" (if F (set Dp (% (inc (car Dp) 256)))))
            ("-" (if F (set Dp (% (dec (car Dp) 256)))))
            (">" (if F (setq Dp (cdr Dp))))
            ("<" (if F (setq Dp (prior Dp D))))
            ("." (if F (prin (char (car Dp)))))
            ("," (if F (set Dp (char (read)))))
            ("["
             (push 'S (if F (prior P Prg)))
             (setq F (n0 (car Dp))) )
            ("]"
             (and (setq F (pop 'S))
                (n0 (car Dp))
                (setq P F) ) ) )
         (pop 'P) ) ) )

# A little "Hello world! test of the interpreter."
(bf (chop ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]
>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++.------.---
-----.[-]>++++++++[<++++>- ]<+.[-]++++++++++." ) )
(bye)

```



### Dynamic solution

Dynamic and unlimited. Unwraping cells. Checking syntax.

```PicoLisp
(de brackets (Lst)
   (let S NIL
      (make
         (for (I . X) Lst
            (case X
               ("[" (push 'S I))
               ("]"
                  (unless S (quit "Unbalanced '['"))
                  (link (list (pop 'S) I)) ) ) )
         (when S (quit "Unbalanced ']'")) ) ) )

(de lupbra (Lst N)
   (find
      '((I)
         (or
            (= (car I) N)
            (= (cadr I) N) ) )
      Lst ) )

(de brain (L)
   (let
      (D (0)
         DH 1
         DL 1
         CH 1
         CL (length L)
         B (brackets L) )
      (loop
         (case (get L CH)
            (>
               (inc 'DH)
               (when (> DH DL)
                  (setq D (insert DH D 0))
                  (inc 'DL) ) )
            (<
               (dec 'DH)
               (when (< DH 1)
                  (setq D (insert DH D 0))
                  (inc 'DL)
                  (one DH) ) )
            (+ (inc (nth D DH)))
            (- (dec (nth D DH)))
            (. (prin (char (get D DH))))
            ("," (set (nth D DH) (char (key))))
            ("["
               (when (=0 (get D DH))
                  (setq CH (cadr (lupbra B CH))) ) )
            ("]"
               (when (n0 (get D DH))
                  (setq CH (car (lupbra B CH))) ) ) )
         (inc 'CH)
         (T (> CH CL)) ) ) )

(brain (chop ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]
>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++.------.---
-----.[-]>++++++++[<++++>- ]<+.[-]++++++++++." ) )

(bye)
```



## Potion

{{incorrect|Potion|
Example fails this test due to incorrect loop implementation:
```Brainfuck>
++++++++[-<+++++++++>]<.>[][<-]>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.
>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+.
```
}}
Tape is infinite length to the right. Cells use default Potion integer type.
<lang># Where `code` is a string.
bf = (code) :
   tape = (0)
   tape_pos = 0
   brackets = ()
   i = -1
   while (++i < code length) :
      if (code(i) == ">"): if (++tape_pos == tape length): tape append(0)..
      elsif (code(i) == "<"): tape_pos--.
      elsif (code(i) == "+"): tape(tape_pos) = tape(tape_pos) + 1.
      elsif (code(i) == "-"): tape(tape_pos) = tape(tape_pos) - 1.
      elsif (code(i) == "."): tape(tape_pos) chr print.
      elsif (code(i) == ","): tape(tape_pos) = read at(0) ord.
      elsif (code(i) == "["): brackets push(i).
      elsif (code(i) == "]") :
         if (tape(tape_pos) == 0): brackets pop.
         else: i = brackets(-1).
      .
   .
.
```



## Prolog


Features: Ignores comments (non Brainfuck characters), Can run as command, or from file, no limit on memory.


```prolog
/******************************************
 Starting point, call with program in atom.
*******************************************/
brain(Program) :-
	atom_chars(Program, Instructions),
	process_bf_chars(Instructions).

brain_from_file(File) :- % or from file...
	read_file_to_codes(File, Codes, []),
	maplist(char_code, Instructions, Codes),
	process_bf_chars(Instructions).

process_bf_chars(Instructions)	:-
	phrase(bf_to_pl(Code), Instructions, []),
	Code = [C|_],
	instruction(C, Code, mem([], [0])), !.


/********************************************
 DCG to parse the bf program into prolog form
*********************************************/
bf_to_pl([]) --> [].
bf_to_pl([loop(Ins)|Next]) --> loop_start, bf_to_pl(Ins), loop_end, bf_to_pl(Next).
bf_to_pl([Ins|Next]) --> bf_code(Ins), bf_to_pl(Next).
bf_to_pl(Ins) --> [X], { \+ member(X, ['[',']',>,<,+,-,'.',',']) }, bf_to_pl(Ins). % skip non bf characters

loop_start --> ['['].
loop_end --> [']'].

bf_code(next_addr) --> ['>'].
bf_code(prev_addr) --> ['<'].
bf_code(inc_caddr) --> ['+'].
bf_code(dec_caddr) --> ['-'].
bf_code(out_caddr) --> ['.'].
bf_code(in_caddr)  --> [','].

/**********************
  Instruction Processor
***********************/
instruction([], _, _).
instruction(I, Code, Mem) :-
	mem_instruction(I, Mem, UpdatedMem),
	next_instruction(Code, NextI, NextCode),
	!, % cuts are to force tail recursion, so big programs will run
	instruction(NextI, NextCode, UpdatedMem).

% to loop, add the loop code to the start of the program then execute
% when the loop has finished it will reach itself again then can retest for zero
instruction(loop(LoopCode), Code, Mem) :-
	caddr(Mem, X),
	dif(X, 0),
	append(LoopCode, Code, [NextI|NextLoopCode]),
	!,
	instruction(NextI, [NextI|NextLoopCode], Mem).
instruction(loop(_), Code, Mem) :-
	caddr(Mem, 0),
	next_instruction(Code, NextI, NextCode),
	!,
	instruction(NextI, NextCode, Mem).

% memory is stored in two parts:
%   1. a list with the current address and everything after it
%   2. a list with the previous memory in reverse order
mem_instruction(next_addr, mem(Mb, [Caddr]), mem([Caddr|Mb], [0])).
mem_instruction(next_addr, mem(Mb, [Caddr,NextAddr|Rest]), mem([Caddr|Mb], [NextAddr|Rest])).
mem_instruction(prev_addr, mem([PrevAddr|RestOfPrev], Caddrs), mem(RestOfPrev, [PrevAddr|Caddrs])).

% wrap instructions at the byte boundaries as this is what most programmers expect to happen
mem_instruction(inc_caddr, MemIn, MemOut) :- caddr(MemIn, 255), update_caddr(MemIn, 0, MemOut).
mem_instruction(inc_caddr, MemIn, MemOut) :- caddr(MemIn, Val), succ(Val, IncVal), update_caddr(MemIn, IncVal, MemOut).
mem_instruction(dec_caddr, MemIn, MemOut) :- caddr(MemIn, 0), update_caddr(MemIn, 255, MemOut).
mem_instruction(dec_caddr, MemIn, MemOut) :- caddr(MemIn, Val), succ(DecVal, Val), update_caddr(MemIn, DecVal, MemOut).

% input and output
mem_instruction(out_caddr, Mem, Mem) :- caddr(Mem, Val), char_code(Char, Val), write(Char).
mem_instruction(in_caddr, MemIn, MemOut) :-
	get_single_char(Code),
	char_code(Char, Code),
	write(Char),
	map_input_code(Code,MappedCode),
	update_caddr(MemIn, MappedCode, MemOut).

% need to map the newline if it is not a proper newline character (system dependent).
map_input_code(13,10) :- nl.
map_input_code(C,C).

% The value at the current address
caddr(mem(_, [Caddr]), Caddr).
caddr(mem(_, [Caddr,_|_]), Caddr).

% The updated value at the current address
update_caddr(mem(BackMem, [_]), Caddr, mem(BackMem, [Caddr])).
update_caddr(mem(BackMem, [_,M|Mem]), Caddr, mem(BackMem, [Caddr,M|Mem])).

% The next instruction, and remaining code
next_instruction([_], [], []).
next_instruction([_,NextI|Rest], NextI, [NextI|Rest]).
```

```txt

?- brain('++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.').
Hello World!
true

```



## PureBasic


[[/PureBasic|Implementation in PureBasic]]


## Python


[[/Python|Implementation in Python]].


## Racket

[http://hashcollision.org/brainfudge/ Brainfudge] is an implementation of Brainfuck in Racket.
Read the tutorial to see you can integrate a new language into the Racket system. The tutorial
also shows how to get IDE support from DrRacket.

As an appetizer this runs in Racket as is:


```racket

#lang planet dyoo/bf
++++++[>++++++++++++<-]>.
>++++++++++[>++++++++++<-]>+.
+++++++..+++.>++++[>+++++++++++<-]>.
<+++[>----<-]>.<<<<<+++[>+++++<-]>.
>>.+++.------.--------.>>+.

```



## Retro


[[/Retro|Implementation in Retro]].


## REXX

The REXX code is original, but the BRAINF░CK program was modified from the example given in Wikipedia:   [http://en.wikipedia.org/wiki/Brainfuck]

```rexx
/*REXX program implements the   Brainf*ck   (self─censored)  language.                  */
@.=0                                             /*initialize the infinite  "tape".     */
p =0                                             /*the  "tape"  cell  pointer.          */
! =0                                             /* !   is the instruction pointer (IP).*/
parse arg $                                      /*allow user to specify a BrainF*ck pgm*/
                                                 /* ┌──◄── No program? Then use default;*/
if $=''  then $=,                                /* ↓      it displays:  Hello, World!  */
  "++++++++++             initialize cell #0  to 10;   then loop:         ",
  "[   > +++++++              add  7 to cell #1;  final result:  70       ",
  "    > ++++++++++           add 10 to cell #2;  final result: 100       ",
  "    > +++                  add  3 to cell #3;  final result   30       ",
  "    > +                    add  1 to cell #4;  final result   10       ",
  "    <<<< -      ]      decrement  cell #0                              ",
  "> ++ .                 display 'H'    which is  ASCII  72 (decimal)    ",
  "> + .                  display 'e'    which is  ASCII 101 (decimal)    ",
  "+++++++ ..             display 'll'   which is  ASCII 108 (decimal) {2}",
  "+++ .                  display 'o'    which is  ASCII 111 (decimal)    ",
  "> ++ .                 display ' '    which is  ASCII  32 (decimal)    ",
  "<< +++++++++++++++ .   display 'W'    which is  ASCII  87 (decimal)    ",
  "> .                    display 'o'    which is  ASCII 111 (decimal)    ",
  "+++ .                  display 'r'    which is  ASCII 114 (decimal)    ",
  "------ .               display 'l'    which is  ASCII 108 (decimal)    ",
  "-------- .             display 'd'    which is  ASCII 100 (decimal)    ",
  "> + .                  display '!'    which is  ASCII  33 (decimal)    "
                                                 /* [↑]   note the  Brainf*ck  comments.*/
     do !=1  while  !\==0  &  !<=length($)       /*keep executing  BF  as long as IP ¬ 0*/
     parse var  $  =(!)  x  +1                   /*obtain a  Brainf*ck instruction  (x),*/
                                                 /*···it's the same as  x=substr($,!,1) */
       select                                    /*examine the current instruction.     */
       when x=='+'  then @.p=@.p + 1             /*increment the   "tape" cell    by  1 */
       when x=='-'  then @.p=@.p - 1             /*decrement  "       "     "      "  " */
       when x=='>'  then   p=  p + 1             /*increment  "  instruction ptr   "  " */
       when x=='<'  then   p=  p - 1             /*decrement  "       "       "    "  " */
       when x=='['  then != forward()            /*go  forward to   ]+1   if  @.P = 0.  */
       when x==']'  then !=backward()            /* " backward  "   [+1    "   "  ¬ "   */
       when x== .   then call charout , d2c(@.p) /*display a  "tape"  cell to terminal. */
       when x==','  then do;  say 'input a value:';  parse pull @.p;  end
       otherwise    iterate
       end   /*select*/
     end     /*forever*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
forward:  if @.p\==0  then return !;  c=1        /*C: ◄───  is the   [   nested counter.*/
                         do k=!+1  to length($);        ?=substr($, k, 1)
                         if ?=='['  then do; c=c+1;     iterate;                   end
                         if ?==']'  then do; c=c-1;     if c==0  then leave;       end
                         end   /*k*/
          return k
/*──────────────────────────────────────────────────────────────────────────────────────*/
backward: if @.p==0   then return !;  c=1        /*C: ◄───  is the   ]   nested counter.*/
                         do k=!-1  to 1  by -1;         ?=substr($, k, 1)
                         if ?==']'  then do; c=c+1;     iterate;                   end
                         if ?=='['  then do; c=c-1;     if c==0  then return k+1;  end
                         end   /*k*/
          return k
```

'''output'''   when using the default program as input:

```txt

Hello World!

```



## Ruby


[[/Ruby|Implementation in Ruby]].


## Rust



```rust
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::stdin;
use std::num::Wrapping;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} [path] (--debug)", args[0]);
        return;
    }

    let src: Vec<char> = {
        let mut buf = String::new();
        match File::open(&args[1])
        {
            Ok(mut f) => { f.read_to_string(&mut buf).unwrap(); }
            Err(e)    => {
                println!("Error opening '{}': {}", args[1], e);
                return;
            }
        }

        buf.chars().collect()
    };

    // Launch options
    let debug = args.contains(&"--debug".to_owned());

    // One pass to find bracket pairs.
    let brackets: HashMap<usize, usize> = {
        let mut m = HashMap::new();
        let mut scope_stack = Vec::new();
        for (idx, ch) in src.iter().enumerate() {
            match ch {
                &'[' => { scope_stack.push(idx); }
                &']' => { m.insert(scope_stack.pop().unwrap(), idx); }
                _    => { /* ignore */ }
            }
        }

        m
    };

    let mut pc: usize = 0;                                  // Program counter
    let mut mem: [Wrapping<u8>;5000] = [Wrapping(0);5000];  // Program cemory
    let mut ptr: usize = 0;                                 // Pointer
    let mut stack: Vec<usize> = Vec::new();                 // Bracket stack

    let stdin_ = stdin();
    let mut reader = stdin_.lock().bytes();
    while pc < src.len() {
        let Wrapping(val) = mem[ptr];

        if debug {
            println!("(BFDB) PC: {:04} \tPTR: {:04} \t$PTR: {:03} \tSTACK_DEPTH: {} \tSYMBOL: {}", pc, ptr, val, stack.len(), src[pc]);
        }

        const ONE: Wrapping<u8> = Wrapping(1);
        match src[pc] {
            '>' => { ptr += 1; }
            '<' => { ptr -= 1; }

            '+' => { mem[ptr] = mem[ptr] + ONE; }
            '-' => { mem[ptr] = mem[ptr] - ONE; }

            '[' => {
                if val == 0 {
                    pc = brackets[&pc];
                } else {
                    stack.push(pc);
                }
            }
            ']' => {
                let matching_bracket = stack.pop().unwrap();
                if val != 0 {
                    pc = matching_bracket - 1;
                }
            }

            '.' => {
                if debug {
                    println!("(BFDB) STDOUT: '{}'", val as char);  // Intercept output
                } else {
                    print!("{}", val as char);
                }
            }
            ',' => {
                mem[ptr] = Wrapping(reader.next().unwrap().unwrap());
            }

            _   => { /* ignore */ }
        }

        pc += 1;
    }
}
```




## Scheme


See [[Execute_Brainfuck/Scheme]].


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: brainF (in string: source, inout file: input, inout file: output) is func
  local
    var array char: memory is 100000 times '\0;';
    var integer: dataPointer is 50000;
    var integer: instructionPointer is 1;
    var integer: nestingLevel is 0;
  begin
    while instructionPointer <= length(source) do
      case source[instructionPointer] of
        when {'>'}: incr(dataPointer);
        when {'<'}: decr(dataPointer);
        when {'+'}: incr(memory[dataPointer]);
        when {'-'}: decr(memory[dataPointer]);
        when {'.'}: write(output, memory[dataPointer]);
        when {','}: memory[dataPointer] := getc(input);
        when {'['}: # Forward if zero at dataPointer
          if memory[dataPointer] = '\0;' then
            nestingLevel := 1;
            repeat
              incr(instructionPointer);
              case source[instructionPointer] of
                when {'['}: incr(nestingLevel);
                when {']'}: decr(nestingLevel);
              end case;
            until nestingLevel = 0;
          end if;
        when {']'}: # Backward if non-zero at dataPointer
          if memory[dataPointer] <> '\0;' then
            nestingLevel := 1;
            repeat
              decr(instructionPointer);
              case source[instructionPointer] of
                when {'['}: decr(nestingLevel);
                when {']'}: incr(nestingLevel);
              end case;
            until nestingLevel = 0;
          end if;
      end case;
      incr(instructionPointer);
    end while;
  end func;

const proc: brainF (in string: source) is func
  begin
    brainF(source, IN, OUT);
  end func;

const proc: main is func
  begin
    brainF("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.");
  end func;
```


```txt

Hello World!

```


Original source [http://seed7.sourceforge.net/algorith/puzzles.htm#brainf7].


## Sidef

```ruby
define tape_length = 50_000;
define eof_val = -1;
define unbalanced_exit_code = 1;

var cmd = 0;
var cell = 0;
var code = [];
var loops = [];
var tape = tape_length.of(0);

func get_input {
    static input_buffer = [];
    input_buffer.len || (input_buffer = ((STDIN.readline \\ return eof_val).chomp.chars.map{.ord}));
    input_buffer.shift \\ eof_val;
}

func jump {
    var depth = 0;
    while (depth >= 0) {
        ++cmd < code.len || Sys.exit(unbalanced_exit_code);
        if (code[cmd] == '[') {
            ++depth;
        }
        elsif (code[cmd] == ']') {
            --depth;
        }
    }
}

var commands = Hash.new(
    '>' => { ++cell },
    '<' => { --cell },
    '+' => { ++tape[cell] },
    '-' => { --tape[cell] },
    '.' => { tape[cell].chr.print },
    ',' => { tape[cell] = get_input() },
    '[' => { tape[cell] ? loops.append(cmd) : jump() },
    ']' => { cmd = (loops.pop - 1) },
);

STDOUT.autoflush(1);
code = ARGF.slurp.chars.grep {|c| commands.exists(c)};
var code_len = code.len;

while (cmd < code_len) {
    commands{code[cmd]}.run;
    cmd++;
}
```



## Standard ML


[[/Standard ML|Implementation in Standard ML]].


## Swift



```Swift
import Foundation

let valids = [">", "<", "+", "-", ".", ",", "[", "]"] as Set<Character>
var ip = 0
var dp = 0
var data = [UInt8](count: 30_000, repeatedValue: 0)

let input = Process.arguments

if input.count != 2 {
    fatalError("Need one input file")
}

let infile: String!

do {
    infile = try String(contentsOfFile: input[1], encoding: NSUTF8StringEncoding) ?? ""
} catch let err {
    infile = ""
}

var program = ""

// remove invalid chars
for c in infile.characters {
    if valids.contains(c) {
        program += String(c)
    }
}

let numChars = program.characters.count

if numChars == 0 {
    fatalError("Error reading file")
}

func increaseInstructionPointer() {
    ip += 1
}

func executeInstruction(ins: Character) {
    switch ins {
    case ">":
        dp += 1
        increaseInstructionPointer()
    case "<":
        dp -= 1
        increaseInstructionPointer()
    case "+":
        data[dp] = data[dp] &+ 1
        increaseInstructionPointer()
    case "-":
        data[dp] = data[dp] &- 1
        increaseInstructionPointer()
    case ".":
        print(Character(UnicodeScalar(data[dp])), terminator: "")
        increaseInstructionPointer()
    case ",":
        handleIn()
        increaseInstructionPointer()
    case "[":
        handleOpenBracket()
    case "]":
        handleClosedBracket()
    default:
        fatalError("What")
    }
}

func handleIn() {
    let input = NSFileHandle.fileHandleWithStandardInput()
    let bytes = input.availableData.bytes
    let buf = unsafeBitCast(UnsafeBufferPointer(start: bytes, count: 1),
        UnsafeBufferPointer<UInt8>.self)

    data[dp] = buf[0]
}

func handleOpenBracket() {
    if data[dp] == 0 {
        var i = 1

        while i > 0 {
            ip += 1
            let ins = program[program.startIndex.advancedBy(ip)]

            if ins == "[" {
                i += 1
            } else if ins == "]" {
                i -= 1
            }
        }
    } else {
        increaseInstructionPointer()
    }
}

func handleClosedBracket() {
    if data[dp] != 0 {
        var i = 1

        while i > 0 {
            ip -= 1
            let ins = program[program.startIndex.advancedBy(ip)]

            if ins == "[" {
                i -= 1
            } else if ins == "]" {
                i += 1
            }
        }
    } else {
        increaseInstructionPointer()
    }
}

func tick() {
    let ins = program[program.startIndex.advancedBy(ip)]

    if valids.contains(ins) {
        executeInstruction(ins)
    } else {
        increaseInstructionPointer()
    }
}

while ip != numChars {
    tick()
}
```


=={{header|TI-83 BASIC}}==

[[/TI-83 BASIC|Implementation in TI-83 BASIC]].

=={{header|TI-89 BASIC}}==

[[/TI-89 BASIC|Implementation in TI-89 Basic]].


## Tcl


[[/Tcl|Implementation in Tcl]].


## VBScript

```vb
'Execute BrainFuck
'VBScript Implementation

'The Main Interpreter
Function BFInpt(s, sp, d, dp, i, ip, o)
    While sp < Len(s)
        Select Case Mid(s, sp + 1, 1)
            Case "+"
                newd = Asc(d(dp)) + 1
                If newd > 255 Then newd = newd Mod 256    'To take account of values over 255
                d(dp) = Chr(newd)
            Case "-"
                newd = Asc(d(dp)) - 1
                If newd < 0 Then newd = (newd Mod 256) + 256    'To take account of negative values
                d(dp) = Chr(newd)
            Case ">"
                dp = dp + 1
                If dp > UBound(d) Then
                    ReDim Preserve d(UBound(d) + 1)
                    d(dp) = Chr(0)
                End If
            Case "<"
                dp = dp - 1
            Case "."
                o = o & d(dp)
            Case ","
                If ip = Len(i) Then d(dp) = Chr(0) Else ip = ip + 1 : d(dp) = Mid(i, ip, 1)
            Case "["
                If Asc(d(dp)) = 0 Then
                    bracket = 1
                    While bracket And sp < Len(s)
                        sp = sp + 1
                        If Mid(s, sp + 1, 1) = "[" Then
                            bracket = bracket + 1
                        ElseIf Mid(s, sp + 1, 1) = "]" Then
                            bracket = bracket - 1
                        End If
                    WEnd
                Else
                    pos = sp - 1
                    sp = sp + 1
                    If BFInpt(s, sp, d, dp, i, ip, o) Then sp = pos
                End If
            Case "]"
                BFInpt = Asc(d(dp)) <> 0
                Exit Function
        End Select
        sp = sp + 1
    WEnd
End Function

'This Prepares the Intepreter
Function BFuck(source, input)
    Dim data() : ReDim data(0)
    data(0)  = Chr(0)
    DataPtr  = 0
    SrcPtr   = 0
    InputPtr = 0
    output   = ""

    BFInpt source , SrcPtr   , _
           data   , DataPtr  , _
           input  , InputPtr , _
           output
    BFuck = output
End Function


'Sample Run
'The input is a string. The first character will be scanned by the first comma
'in the code, the next character will be scanned by the next comma, and so on.

code   = ">++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>" & _
         ">+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++."
inpstr = ""
WScript.StdOut.Write BFuck(code, inpstr)
```

```txt
C:\>cscript /nologo brainf.vbs
Hello world!

C:\>
```



## x86 Assembly


[[/x86_Assembly|Implementation in x86]]


## zkl


```zkl
fcn bf(pgm,input=""){ pgm=pgm.text;  // handle both String and Data
   const CELLS=0d30_000;
   if(Void==pgm.span("[","]")){ println("Mismatched brackets"); return(); }
   fcn(code,z,jmpTable){ // build jump table (for [ & ])
      if(span:=code.span("[","]")){
	 a,b:=span; b+=a-1; jmpTable[a+z]=b+z; jmpTable[b+z]=a+z;
	 self.fcn(code[a+1,b-a-1],z+a+1,jmpTable);
	 self.fcn(code[b+1,*],z+b+1,jmpTable);
      }
   }(pgm,0,jmpTable:=Dictionary());

   tape:=CELLS.pump(Data(CELLS,Int),0);
   ip:=dp:=0; input=input.walker();
   try{
      while(1){
	 switch(pgm[ip]){
	    case(">"){ dp+=1 }
	    case("<"){ dp-=1 }
	    case("+"){ tape[dp]=tape[dp]+1 }
	    case("-"){ tape[dp]=tape[dp]-1 }
	    case("."){ tape[dp].toChar().print() }
	    case(","){ c:=input._next(); tape[dp]=(c and input.value or 0); }
	    case("["){ if(0==tape[dp]){ ip=jmpTable[ip] }}
	    case("]"){ if(tape[dp])   { ip=jmpTable[ip] }}
	 }
	 ip+=1;
      } // while
   }catch(IndexError){}  // read past end of tape == end of program
}
```


```zkl
    // print Hello World!
bf("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++.."
   "+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.");

    // print @
bf(">>++++[<++++[<++++>-]>-]<<.[-]++++++++++.");

   // read 3 characters, inc by 1 and print: "abc"-->"bcd"
bf(",>,>,><<<[+.>]","abc"); println();

bf(",>++++++[<-------->-],[<+>-]<.","23"); println();  // add two digits

    // "Enter your name:", prints name backwards
bf(">+++++++++++++++++++++++++++++++++++++++++"
   "++++++++++++++++++++++++++++.++++++++++++++"
   "+++++++++++++++++++++++++++.++++++.-------------"
   "--.+++++++++++++.>++++++++++++++++++++++++++"
   "++++++.<+++++++.----------.++++++.---.>.<----.----------"
   "---.++++++++++++.--------.-----------------------------------"
   "--------.>.<>>>+[>,----------]++++++++++.<[+++++++++"
   "+.<][<]","Sam Iam\n");

    // word count
bf(File("wc.b").read(),"This\n is a test");

    // rot13
bf(File("rot13.b").read(),"This is a test 123");
```

```txt

Hello World!
@
bcd
5
Enter your name:
maI maS
       	1	4	15
Guvf vf n grfg 123^CCntl C noted

```

The rot13 program is from the Wikipedia and has an infinite loop as it expects a different EoF than I use.

The word count program is:

```txt

>>>+>>>>>+>>+>>+[<<],[
    -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<
        [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]
    <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[
        <+[
            >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]
            <[>+<-]>[>>>>>++>[-]]+<
        ]>[-<<<<<<]>>>>
    ],
]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]
[Counts lines, words, bytes. Assumes no-change-on-EOF or EOF->0.
Daniel B Cristofani (cristofdathevanetdotcom)
http://www.hevanet.com/cristofd/brainfuck/]

```



## ZX Spectrum Basic

The bracket loop could be accelerated to prevent searching the string every time, but it runs.

```zxbasic
10 GO SUB 1000
20 LET e=LEN p$
30 LET a$=p$(ip)
40 IF a$=">" THEN LET dp=dp+1
50 IF a$="<" THEN LET dp=dp-1
60 IF a$="+" THEN LET d(dp)=d(dp)+1
70 IF a$="-" THEN LET d(dp)=d(dp)-1
80 IF a$="." THEN PRINT CHR$ d(dp);
90 IF a$="," THEN INPUT d(dp)
100 IF a$="[" THEN GO SUB 500
110 IF a$="]" THEN LET bp=bp-1: IF d(dp)<>0 THEN LET ip=b(bp)-1
120 LET ip=ip+1
130 IF ip>e THEN PRINT "eof": STOP
140 GO TO 30

499 REM match close
500 LET bc=1: REM bracket counter
510 FOR x=ip+1 TO e
520 IF p$(x)="[" THEN LET bc=bc+1
530 IF p$(x)="]" THEN LET bc=bc-1
540 IF bc=0 THEN LET b(bp)=ip: LET be=x: LET x=e: REM bc will be 0 once all the subnests have been counted over
550 IF bc=0 AND d(dp)=0 THEN LET ip=be: LET bp=bp-1
560 NEXT x
570 LET bp=bp+1
580 RETURN

999 REM initialisation
1000 DIM d(100): REM data stack
1010 LET dp=1: REM data pointer
1020 LET ip=1: REM instruction pointer
1030 DIM b(30): REM bracket stack
1040 LET bp=1: REM bracket pointer
1050 LET p$="++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>+++++.": REM program, marginally modified from Wikipedia; outputs CHR$ 13 at the end instead of CHR$ 10 as ZX Spectrum Basic handles the carriage return better than the line feed
1060 RETURN
```


```txt
Hello World!
eof

9 STOP statement, 130:3
```


