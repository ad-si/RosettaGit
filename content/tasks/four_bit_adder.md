+++
title = "Four bit adder"
description = ""
date = 2019-10-10T16:07:32Z
aliases = []
[extra]
id = 7518
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "applesoft_basic",
  "autohotkey",
  "autoit",
  "basic",
  "batch_file",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "forth",
  "fortran",
  "fsharp",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "labview",
  "lua",
  "m2000_interpreter",
  "mumps",
  "myhdl",
  "nim",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sather",
  "scala",
  "scheme",
  "sed",
  "sidef",
  "systemverilog",
  "tcl",
  "torquescript",
  "verilog",
  "vhdl",
  "xpl0",
  "zkl",
]
+++

{{task}} [[Category:Electronics]]

## Task

"''Simulate''" a four-bit adder.

This design can be realized using four [[wp:Adder_(electronics)#Full_adder|1-bit full adder]]s.
Each of these 1-bit full adders can be built with two [[wp:Adder_(electronics)#Half_adder|half adder]]s and an ''or'' [[wp:Logic gate|gate]]. Finally a half adder can be made using a ''xor'' gate and an ''and'' gate.
The ''xor'' gate can be made using two ''not''s, two ''and''s and one ''or''.

'''Not''', '''or''' and '''and''', the only allowed "gates" for the task, can be "imitated" by using the [[Bitwise operations|bitwise operators]] of your language.
If there is not a ''bit type'' in your language, to be sure that the ''not'' does not "invert" all the other bits of the basic type (e.g. a byte) we are not interested in, you can use an extra ''nand'' (''and'' then ''not'') with the constant 1 on one input.

Instead of optimizing and reducing the number of gates used for the final 4-bit adder, build it in the most straightforward way, ''connecting'' the other "constructive blocks", in turn made of "simpler" and "smaller" ones.

{|
|+Schematics of the "constructive blocks"
!Xor gate done with ands, ors and nots
!A half adder
!A full adder
!A 4-bit adder
|-
|[[File:xor.png|frameless|Xor gate done with ands, ors and nots]]
|[[File:halfadder.png|frameless|A half adder]]
|[[File:fulladder.png|frameless|A full adder]]
|[[File:4bitsadder.png|frameless|A 4-bit adder]]
|}

Solutions should try to be as descriptive as possible, making it as easy as possible to identify "connections" between higher-order "blocks".
It is not mandatory to replicate the syntax of higher-order blocks in the atomic "gate" blocks, i.e. basic "gate" operations can be performed as usual bitwise operations, or they can be "wrapped" in a ''block'' in order to expose the same syntax of higher-order blocks, at implementers' choice.

To test the implementation, show the sum of two four-bit numbers (in binary).
<div style="clear:both"></div>





## Ada


```Ada

type Four_Bits is array (1..4) of Boolean;

procedure Half_Adder (Input_1, Input_2 : Boolean; Output, Carry : out Boolean) is
begin
   Output := Input_1 xor Input_2;
   Carry  := Input_1 and Input_2;
end Half_Adder;

procedure Full_Adder (Input_1, Input_2 : Boolean; Output : out Boolean; Carry : in out Boolean) is
   T_1, T_2, T_3 : Boolean;
begin
   Half_Adder (Input_1, Input_2, T_1, T_2);
   Half_Adder (Carry, T_1, Output, T_3);
   Carry := T_2 or T_3;
end Full_Adder;

procedure Four_Bits_Adder (A, B : Four_Bits; C : out Four_Bits; Carry : in out Boolean) is
begin
   Full_Adder (A (4), B (4), C (4), Carry);
   Full_Adder (A (3), B (3), C (3), Carry);
   Full_Adder (A (2), B (2), C (2), Carry);
   Full_Adder (A (1), B (1), C (1), Carry);
end Four_Bits_Adder;

```

A test program with the above definitions

```Ada

with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_4_Bit_Adder is

   -- The definitions from above

   function Image (Bit : Boolean) return Character is
   begin
      if Bit then
         return '1';
      else
         return '0';
      end if;
   end Image;

   function Image (X : Four_Bits) return String is
   begin
      return Image (X (1)) & Image (X (2)) & Image (X (3)) & Image (X (4));
   end Image;

   A, B, C : Four_Bits; Carry : Boolean;
begin
   for I_1 in Boolean'Range loop
      for I_2 in Boolean'Range loop
         for I_3 in Boolean'Range loop
            for I_4 in Boolean'Range loop
               for J_1 in Boolean'Range loop
                  for J_2 in Boolean'Range loop
                     for J_3 in Boolean'Range loop
                        for J_4 in Boolean'Range loop
                           A := (I_1, I_2, I_3, I_4);
                           B := (J_1, J_2, J_3, J_4);
                           Carry := False;
                           Four_Bits_Adder (A, B, C, Carry);
                           Put_Line
                           (  Image (A)
                           &  " + "
                           &  Image (B)
                           &  " = "
                           &  Image (C)
                           &  " "
                           &  Image (Carry)
                           );
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
end Test_4_Bit_Adder;

```

<div style="height: 320px;overflow:scroll">

```txt

0000 + 0000 = 0000 0
0000 + 0001 = 0001 0
0000 + 0010 = 0010 0
0000 + 0011 = 0011 0
0000 + 0100 = 0100 0
0000 + 0101 = 0101 0
0000 + 0110 = 0110 0
0000 + 0111 = 0111 0
0000 + 1000 = 1000 0
0000 + 1001 = 1001 0
0000 + 1010 = 1010 0
0000 + 1011 = 1011 0
0000 + 1100 = 1100 0
0000 + 1101 = 1101 0
0000 + 1110 = 1110 0
0000 + 1111 = 1111 0
0001 + 0000 = 0001 0
0001 + 0001 = 0010 0
0001 + 0010 = 0011 0
0001 + 0011 = 0100 0
0001 + 0100 = 0101 0
0001 + 0101 = 0110 0
0001 + 0110 = 0111 0
0001 + 0111 = 1000 0
0001 + 1000 = 1001 0
0001 + 1001 = 1010 0
0001 + 1010 = 1011 0
0001 + 1011 = 1100 0
0001 + 1100 = 1101 0
0001 + 1101 = 1110 0
0001 + 1110 = 1111 0
0001 + 1111 = 0000 1
0010 + 0000 = 0010 0
0010 + 0001 = 0011 0
0010 + 0010 = 0100 0
0010 + 0011 = 0101 0
0010 + 0100 = 0110 0
0010 + 0101 = 0111 0
0010 + 0110 = 1000 0
0010 + 0111 = 1001 0
0010 + 1000 = 1010 0
0010 + 1001 = 1011 0
0010 + 1010 = 1100 0
0010 + 1011 = 1101 0
0010 + 1100 = 1110 0
0010 + 1101 = 1111 0
0010 + 1110 = 0000 1
0010 + 1111 = 0001 1
0011 + 0000 = 0011 0
0011 + 0001 = 0100 0
0011 + 0010 = 0101 0
0011 + 0011 = 0110 0
0011 + 0100 = 0111 0
0011 + 0101 = 1000 0
0011 + 0110 = 1001 0
0011 + 0111 = 1010 0
0011 + 1000 = 1011 0
0011 + 1001 = 1100 0
0011 + 1010 = 1101 0
0011 + 1011 = 1110 0
0011 + 1100 = 1111 0
0011 + 1101 = 0000 1
0011 + 1110 = 0001 1
0011 + 1111 = 0010 1
0100 + 0000 = 0100 0
0100 + 0001 = 0101 0
0100 + 0010 = 0110 0
0100 + 0011 = 0111 0
0100 + 0100 = 1000 0
0100 + 0101 = 1001 0
0100 + 0110 = 1010 0
0100 + 0111 = 1011 0
0100 + 1000 = 1100 0
0100 + 1001 = 1101 0
0100 + 1010 = 1110 0
0100 + 1011 = 1111 0
0100 + 1100 = 0000 1
0100 + 1101 = 0001 1
0100 + 1110 = 0010 1
0100 + 1111 = 0011 1
0101 + 0000 = 0101 0
0101 + 0001 = 0110 0
0101 + 0010 = 0111 0
0101 + 0011 = 1000 0
0101 + 0100 = 1001 0
0101 + 0101 = 1010 0
0101 + 0110 = 1011 0
0101 + 0111 = 1100 0
0101 + 1000 = 1101 0
0101 + 1001 = 1110 0
0101 + 1010 = 1111 0
0101 + 1011 = 0000 1
0101 + 1100 = 0001 1
0101 + 1101 = 0010 1
0101 + 1110 = 0011 1
0101 + 1111 = 0100 1
0110 + 0000 = 0110 0
0110 + 0001 = 0111 0
0110 + 0010 = 1000 0
0110 + 0011 = 1001 0
0110 + 0100 = 1010 0
0110 + 0101 = 1011 0
0110 + 0110 = 1100 0
0110 + 0111 = 1101 0
0110 + 1000 = 1110 0
0110 + 1001 = 1111 0
0110 + 1010 = 0000 1
0110 + 1011 = 0001 1
0110 + 1100 = 0010 1
0110 + 1101 = 0011 1
0110 + 1110 = 0100 1
0110 + 1111 = 0101 1
0111 + 0000 = 0111 0
0111 + 0001 = 1000 0
0111 + 0010 = 1001 0
0111 + 0011 = 1010 0
0111 + 0100 = 1011 0
0111 + 0101 = 1100 0
0111 + 0110 = 1101 0
0111 + 0111 = 1110 0
0111 + 1000 = 1111 0
0111 + 1001 = 0000 1
0111 + 1010 = 0001 1
0111 + 1011 = 0010 1
0111 + 1100 = 0011 1
0111 + 1101 = 0100 1
0111 + 1110 = 0101 1
0111 + 1111 = 0110 1
1000 + 0000 = 1000 0
1000 + 0001 = 1001 0
1000 + 0010 = 1010 0
1000 + 0011 = 1011 0
1000 + 0100 = 1100 0
1000 + 0101 = 1101 0
1000 + 0110 = 1110 0
1000 + 0111 = 1111 0
1000 + 1000 = 0000 1
1000 + 1001 = 0001 1
1000 + 1010 = 0010 1
1000 + 1011 = 0011 1
1000 + 1100 = 0100 1
1000 + 1101 = 0101 1
1000 + 1110 = 0110 1
1000 + 1111 = 0111 1
1001 + 0000 = 1001 0
1001 + 0001 = 1010 0
1001 + 0010 = 1011 0
1001 + 0011 = 1100 0
1001 + 0100 = 1101 0
1001 + 0101 = 1110 0
1001 + 0110 = 1111 0
1001 + 0111 = 0000 1
1001 + 1000 = 0001 1
1001 + 1001 = 0010 1
1001 + 1010 = 0011 1
1001 + 1011 = 0100 1
1001 + 1100 = 0101 1
1001 + 1101 = 0110 1
1001 + 1110 = 0111 1
1001 + 1111 = 1000 1
1010 + 0000 = 1010 0
1010 + 0001 = 1011 0
1010 + 0010 = 1100 0
1010 + 0011 = 1101 0
1010 + 0100 = 1110 0
1010 + 0101 = 1111 0
1010 + 0110 = 0000 1
1010 + 0111 = 0001 1
1010 + 1000 = 0010 1
1010 + 1001 = 0011 1
1010 + 1010 = 0100 1
1010 + 1011 = 0101 1
1010 + 1100 = 0110 1
1010 + 1101 = 0111 1
1010 + 1110 = 1000 1
1010 + 1111 = 1001 1
1011 + 0000 = 1011 0
1011 + 0001 = 1100 0
1011 + 0010 = 1101 0
1011 + 0011 = 1110 0
1011 + 0100 = 1111 0
1011 + 0101 = 0000 1
1011 + 0110 = 0001 1
1011 + 0111 = 0010 1
1011 + 1000 = 0011 1
1011 + 1001 = 0100 1
1011 + 1010 = 0101 1
1011 + 1011 = 0110 1
1011 + 1100 = 0111 1
1011 + 1101 = 1000 1
1011 + 1110 = 1001 1
1011 + 1111 = 1010 1
1100 + 0000 = 1100 0
1100 + 0001 = 1101 0
1100 + 0010 = 1110 0
1100 + 0011 = 1111 0
1100 + 0100 = 0000 1
1100 + 0101 = 0001 1
1100 + 0110 = 0010 1
1100 + 0111 = 0011 1
1100 + 1000 = 0100 1
1100 + 1001 = 0101 1
1100 + 1010 = 0110 1
1100 + 1011 = 0111 1
1100 + 1100 = 1000 1
1100 + 1101 = 1001 1
1100 + 1110 = 1010 1
1100 + 1111 = 1011 1
1101 + 0000 = 1101 0
1101 + 0001 = 1110 0
1101 + 0010 = 1111 0
1101 + 0011 = 0000 1
1101 + 0100 = 0001 1
1101 + 0101 = 0010 1
1101 + 0110 = 0011 1
1101 + 0111 = 0100 1
1101 + 1000 = 0101 1
1101 + 1001 = 0110 1
1101 + 1010 = 0111 1
1101 + 1011 = 1000 1
1101 + 1100 = 1001 1
1101 + 1101 = 1010 1
1101 + 1110 = 1011 1
1101 + 1111 = 1100 1
1110 + 0000 = 1110 0
1110 + 0001 = 1111 0
1110 + 0010 = 0000 1
1110 + 0011 = 0001 1
1110 + 0100 = 0010 1
1110 + 0101 = 0011 1
1110 + 0110 = 0100 1
1110 + 0111 = 0101 1
1110 + 1000 = 0110 1
1110 + 1001 = 0111 1
1110 + 1010 = 1000 1
1110 + 1011 = 1001 1
1110 + 1100 = 1010 1
1110 + 1101 = 1011 1
1110 + 1110 = 1100 1
1110 + 1111 = 1101 1
1111 + 0000 = 1111 0
1111 + 0001 = 0000 1
1111 + 0010 = 0001 1
1111 + 0011 = 0010 1
1111 + 0100 = 0011 1
1111 + 0101 = 0100 1
1111 + 0110 = 0101 1
1111 + 0111 = 0110 1
1111 + 1000 = 0111 1
1111 + 1001 = 1000 1
1111 + 1010 = 1001 1
1111 + 1011 = 1010 1
1111 + 1100 = 1011 1
1111 + 1101 = 1100 1
1111 + 1110 = 1101 1
1111 + 1111 = 1110 1

```

</div>


## AutoHotkey

```AutoHotkey
A := 13
B := 9
N := FourBitAdd(A, B)
MsgBox, % A " + " B ":`n"
	. GetBin4(A) " + " GetBin4(B) " = " N.S " (Carry = " N.C ")"
return

Xor(A, B) {
	return (~A & B) | (A & ~B)
}

HalfAdd(A, B) {
	return {"S": Xor(A, B), "C": A & B}
}

FullAdd(A, B, C=0) {
	X := HalfAdd(A, C)
	Y := HalfAdd(B, X.S)
	return {"S": Y.S, "C": X.C | Y.C}
}

FourBitAdd(A, B, C=0) {
	A := GetFourBits(A)
	B := GetFourBits(B)
	X := FullAdd(A[4], B[4], C)
	Y := FullAdd(A[3], B[3], X.C)
	W := FullAdd(A[2], B[2], Y.C)
	Z := FullAdd(A[1], B[1], W.C)
	return {"S": Z.S W.S Y.S X.S, "C": Z.C}
}

GetFourBits(N) {
	if (N < 0 || N > 15)
		return -1
	return StrSplit(GetBin4(N))
}

GetBin4(N) {
	Loop 4
		Res := Mod(N, 2) Res, N := N >> 1
	return, Res
}
```

```txt
13 + 9:
1101 + 1001 = 0110 (Carry = 1)
```



## AutoIt


### Functions


```AutoIt

Func _NOT($_A)
	Return (Not $_A) *1
EndFunc  ;==>_NOT

Func _AND($_A, $_B)
	Return BitAND($_A, $_B)
EndFunc  ;==>_AND

Func _OR($_A, $_B)
	Return BitOR($_A, $_B)
EndFunc  ;==>_OR

Func _XOR($_A, $_B)
	Return _OR( _
		_AND( $_A, _NOT($_B) ), _
		_AND( _NOT($_A), $_B) )
EndFunc  ;==>_XOR

Func _HalfAdder($_A, $_B, ByRef $_CO)
	$_CO = _AND($_A, $_B)
	Return _XOR($_A, $_B)
EndFunc  ;==>_HalfAdder

Func _FullAdder($_A, $_B, $_CI, ByRef $_CO)
	Local $CO1, $CO2, $Q1, $Q2
	$Q1 = _HalfAdder($_A, $_B, $CO1)
	$Q2 = _HalfAdder($Q1, $_CI, $CO2)
	$_CO = _OR($CO2, $CO1)
	Return $Q2
EndFunc  ;==>_FullAdder

Func _4BitAdder($_A1, $_A2, $_A3, $_A4, $_B1, $_B2, $_B3, $_B4, $_CI, ByRef $_CO)
	Local $CO1, $CO2, $CO3, $CO4, $Q1, $Q2, $Q3, $Q4
	$Q1 = _FullAdder($_A4, $_B4, $_CI, $CO1)
	$Q2 = _FullAdder($_A3, $_B3, $CO1, $CO2)
	$Q3 = _FullAdder($_A2, $_B2, $CO2, $CO3)
	$Q4 = _FullAdder($_A1, $_B1, $CO3, $CO4)
	$_CO = $CO4
	Return $Q4 & $Q3 & $Q2 & $Q1
EndFunc  ;==>_4BitAdder

```


### Example


```AutoIt

Local $CarryOut, $sResult
$sResult = _4BitAdder(0, 0, 1, 1, 0, 1, 1, 1, 0, $CarryOut)  ; adds 3 + 7
ConsoleWrite('result: ' & $sResult & '  ==> carry out: ' & $CarryOut & @LF)

$sResult = _4BitAdder(1, 0, 1, 1, 1, 0, 0, 0, 0, $CarryOut)  ; adds 11 + 8
ConsoleWrite('result: ' & $sResult & '  ==> carry out: ' & $CarryOut & @LF)

```

```txt

result: 1010  ==> carry out: 0
result: 0011  ==> carry out: 1

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 17:10, 14 November 2013 (UTC)


## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

:: ":main" is where all the non-logic-gate stuff happens
:main
:: User input two 4-digit binary numbers
:: There is no error checking for these numbers, however if the first 4 digits of both inputs are in binary...
:: The program will use them. All non-binary numbers are treated as 0s, but having less than 4 digits will crash it
set /p "input1=First 4-Bit Binary Number: "
set /p "input2=Second 4-Bit Binary Number: "

:: Put the first 4 digits of the binary numbers and separate them into "A[]" for input A and "B[]" for input B
for /l %%i in (0,1,3) do (
  set A%%i=!input1:~%%i,1!
  set B%%i=!input2:~%%i,1!
)

:: Run the 4-bit Adder with "A[]" and "B[]" as parameters. The program supports a 9th parameter for a Carry input
call:_4bitAdder %A3% %A2% %A1% %A0% %B3% %B2% %B1% %B0% 0

:: Display the answer and exit
echo %input1% + %input2% = %outputC%%outputS4%%outputS3%%outputS2%%outputS1%
pause>nul
exit /b

:: Function for the 4-bit Adder following the logic given
:_4bitAdder
set inputA1=%1
set inputA2=%2
set inputA3=%3
set inputA4=%4

set inputB1=%5
set inputB2=%6
set inputB3=%7
set inputB4=%8

set inputC=%9

call:_FullAdder %inputA1% %inputB1% %inputC%
set outputS1=%outputS%
set inputC=%outputC%

call:_FullAdder %inputA2% %inputB2% %inputC%
set outputS2=%outputS%
set inputC=%outputC%

call:_FullAdder %inputA3% %inputB3% %inputC%
set outputS3=%outputS%
set inputC=%outputC%

call:_FullAdder %inputA4% %inputB4% %inputC%
set outputS4=%outputS%
set inputC=%outputC%

:: In order return more than one number (of which is usually done via 'exit /b') we declare them while ending the local environment
endlocal && set "outputS1=%outputS1%" && set "outputS2=%outputS2%" && set "outputS3=%outputS3%" && set "outputS4=%outputS4%" && set "outputC=%inputC%"
exit /b

:: Function for the 1-bit Adder following the logic given
:_FullAdder
setlocal
set inputA=%1
set inputB=%2
set inputC1=%3

call:_halfAdder %inputA% %inputB%
set inputA1=%outputS%
set inputA2=%inputA1%
set inputC2=%outputC%

call:_HalfAdder %inputA1% %inputC1%
set outputS=%outputS%
set inputC1=%outputC%

call:_Or %inputC1% %inputC2%
set outputC=%errorlevel%

endlocal && set "outputS=%outputS%" && set "outputC=%outputC%"
exit /b

:: Function for the half-bit adder following the logic given
:_halfAdder
setlocal
set inputA1=%1
set inputA2=%inputA1%
set inputB1=%2
set inputB2=%inputB1%

call:_XOr %inputA1% %inputB2%
set outputS=%errorlevel%

call:_And %inputA2% %inputB2%
set outputC=%errorlevel%

endlocal && set "outputS=%outputS%" && set "outputC=%outputC%"
exit /b

:: Function for the XOR-gate following the logic given
:_XOr
setlocal
set inputA1=%1
set inputB1=%2

call:_Not %inputA1%
set inputA2=%errorlevel%

call:_Not %inputB1%
set inputB2=%errorlevel%

call:_And %inputA1% %inputB2%
set inputA=%errorlevel%

call:_And %inputA2% %inputB1%
set inputB=%errorlevel%

call:_Or %inputA% %inputB%
set outputA=%errorlevel%

:: As there is only one output, we can use 'exit /b {errorlevel}' to return a specified errorlevel
exit /b %outputA%

:: The basic 3 logic gates that every other funtion is composed of
:_Not
setlocal
if %1==0 exit /b 1
exit /b 0
:_Or
setlocal
if %1==1 exit /b 1
if %2==1 exit /b 1
exit /b 0
:_And
setlocal
if %1==1 if %2==1 exit /b 1
exit /b 0

```

```txt

First 4-Bit Binary Number: 1011
Second 4-Bit Binary Number: 0111
1011 + 0111 = 10010

```



## BASIC


=
## Applesoft BASIC
=


```basic
100 S$ = "1100 + 1100 = " : GOSUB 400
110 S$ = "1100 + 1101 = " : GOSUB 400
120 S$ = "1100 + 1110 = " : GOSUB 400
130 S$ = "1100 + 1111 = " : GOSUB 400
140 S$ = "1101 + 0000 = " : GOSUB 400
150 S$ = "1101 + 0001 = " : GOSUB 400
160 S$ = "1101 + 0010 = " : GOSUB 400
170 S$ = "1101 + 0011 = " : GOSUB 400
180 END

400 A0 = VAL(MID$(S$, 4, 1))
410 A1 = VAL(MID$(S$, 3, 1))
420 A2 = VAL(MID$(S$, 2, 1))
430 A3 = VAL(MID$(S$, 1, 1))
440 B0 = VAL(MID$(S$, 11, 1))
450 B1 = VAL(MID$(S$, 10, 1))
460 B2 = VAL(MID$(S$, 9, 1))
470 B3 = VAL(MID$(S$, 8, 1))
480 GOSUB 600
490 PRINT S$;

REM 4 BIT PRINT
500 PRINT C;S3;S2;S1;S0
510 RETURN

REM 4 BIT ADD
REM  ADD A3 A2 A1 A0 TO B3 B2 B1 B0
REM  RESULT IN S3 S2 S1 S0
REM  CARRY IN C
600 C = 0
610 A = A0 : B = B0 : GOSUB 700 : S0 = S
620 A = A1 : B = B1 : GOSUB 700 : S1 = S
630 A = A2 : B = B2 : GOSUB 700 : S2 = S
640 A = A3 : B = B3 : GOSUB 700 : S3 = S
650 RETURN

REM FULL ADDER
REM  ADD A + B + C
REM  RESULT IN S
REM  CARRY IN C
700 BH = B : B = C : GOSUB 800 : C1 = C
710 A = S : B = BH : GOSUB 800 : C2 = C
720 C = C1 OR C2
730 RETURN

REM HALF ADDER
REM  ADD A + B
REM  RESULT IN S
REM  CARRY IN C
800 GOSUB 900 : S = C
810 C = A AND B
820 RETURN

REM XOR GATE
REM  A XOR B
REM  RESULT IN C
900 C = A AND NOT B
910 D = B AND NOT A
920 C = C OR D
930 RETURN
```


=
## BBC BASIC
=
```bbcbasic
      @% = 2
      PRINT "1100 + 1100 = ";
      PROC4bitadd(1,1,0,0, 1,1,0,0, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1100 + 1101 = ";
      PROC4bitadd(1,1,0,0, 1,1,0,1, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1100 + 1110 = ";
      PROC4bitadd(1,1,0,0, 1,1,1,0, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1100 + 1111 = ";
      PROC4bitadd(1,1,0,0, 1,1,1,1, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1101 + 0000 = ";
      PROC4bitadd(1,1,0,1, 0,0,0,0, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1101 + 0001 = ";
      PROC4bitadd(1,1,0,1, 0,0,0,1, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1101 + 0010 = ";
      PROC4bitadd(1,1,0,1, 0,0,1,0, e,d,c,b,a) : PRINT e,d,c,b,a
      PRINT "1101 + 0011 = ";
      PROC4bitadd(1,1,0,1, 0,0,1,1, e,d,c,b,a) : PRINT e,d,c,b,a
      END

      DEF PROC4bitadd(a3&, a2&, a1&, a0&, b3&, b2&, b1&, b0&, \
      \   RETURN c3&, RETURN s3&, RETURN s2&, RETURN s1&, RETURN s0&)
      LOCAL c0&, c1&, c2&
      PROCfulladder(a0&, b0&,   0, s0&, c0&)
      PROCfulladder(a1&, b1&, c0&, s1&, c1&)
      PROCfulladder(a2&, b2&, c1&, s2&, c2&)
      PROCfulladder(a3&, b3&, c2&, s3&, c3&)
      ENDPROC

      DEF PROCfulladder(a&, b&, c&, RETURN s&, RETURN c1&)
      LOCAL x&, y&, z&
      PROChalfadder(a&, c&, x&, y&)
      PROChalfadder(x&, b&, s&, z&)
      c1& = y& OR z&
      ENDPROC

      DEF PROChalfadder(a&, b&, RETURN s&, RETURN c&)
      s& = FNxorgate(a&, b&)
      c& = a& AND b&
      ENDPROC

      DEF FNxorgate(a&, b&)
      LOCAL c&, d&
      c& = a& AND NOT b&
      d& = b& AND NOT a&
      = c& OR d&
```

```txt

1100 + 1100 =  1 1 0 0 0
1100 + 1101 =  1 1 0 0 1
1100 + 1110 =  1 1 0 1 0
1100 + 1111 =  1 1 0 1 1
1101 + 0000 =  0 1 1 0 1
1101 + 0001 =  0 1 1 1 0
1101 + 0010 =  0 1 1 1 1
1101 + 0011 =  1 0 0 0 0

```



## C


```c
#include <stdio.h>

typedef char pin_t;
#define IN const pin_t *
#define OUT pin_t *
#define PIN(X) pin_t _##X; pin_t *X = & _##X;
#define V(X) (*(X))

/* a NOT that does not soil the rest of the host of the single bit */
#define NOT(X) (~(X)&1)

/* a shortcut to "implement" a XOR using only NOT, AND and OR gates, as
   task requirements constrain */
#define XOR(X,Y) ((NOT(X)&(Y)) | ((X)&NOT(Y)))

void halfadder(IN a, IN b, OUT s, OUT c)
{
  V(s) = XOR(V(a), V(b));
  V(c) = V(a) & V(b);
}

void fulladder(IN a, IN b, IN ic, OUT s, OUT oc)
{
  PIN(ps); PIN(pc); PIN(tc);

  halfadder(/*INPUT*/a, b, /*OUTPUT*/ps, pc);
  halfadder(/*INPUT*/ps, ic, /*OUTPUT*/s, tc);
  V(oc) = V(tc) | V(pc);
}

void fourbitsadder(IN a0, IN a1, IN a2, IN a3,
		   IN b0, IN b1, IN b2, IN b3,
		   OUT o0, OUT o1, OUT o2, OUT o3,
		   OUT overflow)
{
  PIN(zero); V(zero) = 0;
  PIN(tc0); PIN(tc1); PIN(tc2);

  fulladder(/*INPUT*/a0, b0, zero, /*OUTPUT*/o0, tc0);
  fulladder(/*INPUT*/a1, b1, tc0,  /*OUTPUT*/o1, tc1);
  fulladder(/*INPUT*/a2, b2, tc1,  /*OUTPUT*/o2, tc2);
  fulladder(/*INPUT*/a3, b3, tc2,  /*OUTPUT*/o3, overflow);
}


int main()
{
  PIN(a0); PIN(a1); PIN(a2); PIN(a3);
  PIN(b0); PIN(b1); PIN(b2); PIN(b3);
  PIN(s0); PIN(s1); PIN(s2); PIN(s3);
  PIN(overflow);

  V(a3) = 0; V(b3) = 1;
  V(a2) = 0; V(b2) = 1;
  V(a1) = 1; V(b1) = 1;
  V(a0) = 0; V(b0) = 0;

  fourbitsadder(a0, a1, a2, a3, /* INPUT */
		b0, b1, b2, b3,
		s0, s1, s2, s3, /* OUTPUT */
		overflow);

  printf("%d%d%d%d + %d%d%d%d = %d%d%d%d, overflow = %d\n",
	 V(a3), V(a2), V(a1), V(a0),
	 V(b3), V(b2), V(b1), V(b0),
	 V(s3), V(s2), V(s1), V(s0),
	 V(overflow));

  return 0;
}
```



## C++

See [[Four bit adder/C++]]

## C#

```c#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaCodeTasks.FourBitAdder
{
	public struct BitAdderOutput
	{
		public bool S { get; set; }
		public bool C { get; set; }
		public override string ToString ( )
		{
			return "S" + ( S ? "1" : "0" ) + "C" + ( C ? "1" : "0" );
		}
	}
	public struct Nibble
	{
		public bool _1 { get; set; }
		public bool _2 { get; set; }
		public bool _3 { get; set; }
		public bool _4 { get; set; }
		public override string ToString ( )
		{
			return ( _4 ? "1" : "0" )
				+ ( _3 ? "1" : "0" )
				+ ( _2 ? "1" : "0" )
				+ ( _1 ? "1" : "0" );
		}
	}
	public struct FourBitAdderOutput
	{
		public Nibble N { get; set; }
		public bool C { get; set; }
		public override string ToString ( )
		{
			return N.ToString ( ) + "c" + ( C ? "1" : "0" );
		}
	}

	public static class LogicGates
	{
		// Basic Gates
		public static bool Not ( bool A ) { return !A; }
		public static bool And ( bool A, bool B ) { return A && B; }
		public static bool Or ( bool A, bool B ) { return A || B; }

		// Composite Gates
		public static bool Xor ( bool A, bool B ) {	return Or ( And ( A, Not ( B ) ), ( And ( Not ( A ), B ) ) ); }
	}

	public static class ConstructiveBlocks
	{
		public static BitAdderOutput HalfAdder ( bool A, bool B )
		{
			return new BitAdderOutput ( ) { S = LogicGates.Xor ( A, B ), C = LogicGates.And ( A, B ) };
		}

		public static BitAdderOutput FullAdder ( bool A, bool B, bool CI )
		{
			BitAdderOutput HA1 = HalfAdder ( CI, A );
			BitAdderOutput HA2 = HalfAdder ( HA1.S, B );

			return new BitAdderOutput ( ) { S = HA2.S, C = LogicGates.Or ( HA1.C, HA2.C ) };
		}

		public static FourBitAdderOutput FourBitAdder ( Nibble A, Nibble B, bool CI )
		{

			BitAdderOutput FA1 = FullAdder ( A._1, B._1, CI );
			BitAdderOutput FA2 = FullAdder ( A._2, B._2, FA1.C );
			BitAdderOutput FA3 = FullAdder ( A._3, B._3, FA2.C );
			BitAdderOutput FA4 = FullAdder ( A._4, B._4, FA3.C );

			return new FourBitAdderOutput ( ) { N = new Nibble ( ) { _1 = FA1.S, _2 = FA2.S, _3 = FA3.S, _4 = FA4.S }, C = FA4.C };
		}

		public static void Test ( )
		{
			Console.WriteLine ( "Four Bit Adder" );

			for ( int i = 0; i < 256; i++ )
			{
				Nibble A = new Nibble ( ) { _1 = false, _2 = false, _3 = false, _4 = false };
				Nibble B = new Nibble ( ) { _1 = false, _2 = false, _3 = false, _4 = false };
				if ( (i & 1) == 1)
				{
					A._1 = true;
				}
				if ( ( i & 2 ) == 2 )
				{
					A._2 = true;
				}
				if ( ( i & 4 ) == 4 )
				{
					A._3 = true;
				}
				if ( ( i & 8 ) == 8 )
				{
					A._4 = true;
				}
				if ( ( i & 16 ) == 16 )
				{
					B._1 = true;
				}
				if ( ( i & 32 ) == 32)
				{
					B._2 = true;
				}
				if ( ( i & 64 ) == 64 )
				{
					B._3 = true;
				}
				if ( ( i & 128 ) == 128 )
				{
					B._4 = true;
				}

				Console.WriteLine ( "{0} + {1} = {2}", A.ToString ( ), B.ToString ( ), FourBitAdder( A, B, false ).ToString ( ) );

			}

			Console.WriteLine ( );
		}

	}
}


```



## Clojure


```clojure

(ns rosettacode.adder
  (:use clojure.test))

(defn xor-gate [a b]
  (or (and a (not b)) (and b (not a))))

(defn half-adder [a b]
  "output: (S C)"
  (cons (xor-gate a b) (list (and a b))))

(defn full-adder [a b c]
  "output: (C S)"
  (let [HA-ca (half-adder c a)
        HA-ca->sb (half-adder (first HA-ca) b)]
    (cons (or (second HA-ca) (second HA-ca->sb))
          (list (first HA-ca->sb)))))

(defn n-bit-adder
  "first bits on the list are low order bits
1 = true
2 = false true
3 = true true
4 = false false true..."
  can add numbers of different bit-length
  ([a-bits b-bits] (n-bit-adder a-bits b-bits false))
  ([a-bits b-bits carry]
  (let [added (full-adder (first a-bits) (first b-bits) carry)]
    (if(and (nil? a-bits) (nil? b-bits))
      (if carry (list carry) '())
      (cons (second added) (n-bit-adder (next a-bits) (next b-bits) (first added)))))))

;use:
(n-bit-adder [true true true true true true] [true true true true true true])
=> (false true true true true true true)

```



### Second Clojure solution


```clojure
(ns rosetta.fourbit)

;; a bit is represented as a boolean (true/false)
;; a word is a big-endian vector of bits [true false true true] = 11
;; multiple values are returned as vectors

(defn or-gate [a b]
  (or a b))

(defn and-gate [a b]
  (and a b))

(defn not-gate [a]
  (not a))

(defn xor-gate [a b]
  (or-gate (and-gate (not-gate a) b) (and-gate a (not-gate b))))

(defn half-adder [a b]
  "result is [carry sum]"
  (let [carry (and-gate a b)
        sum (xor-gate a b)]
    [carry sum]))

(defn full-adder [a b c0]
  "result is [carry sum]"
  (let [[ca sa] (half-adder c0 a)
        [cb sb] (half-adder sa b)]
    [(or-gate ca cb) sb]))

(defn nbit-adder [va vb]
  "va and vb should be big endian bit vectors of the same size. The result
is a bit vector having one more bit (carry) than args."
  {:pre [(= (count va) (count vb))]}
  (let [[c sums] (reduce (fn [[carry sums] [a b]]
                              (let [[c s] (full-adder a b carry)]
                                [c (conj sums s)]))
                         ;; initial value: false carry and an empty list of sums
                         [false ()]
                         ;; rseq is constant-time reverse for vectors
                         (map vector (rseq va) (rseq vb)))]
    (vec (conj sums c))))

(defn four-bit-adder [a4 a3 a2 a1 b4 b3 b2 b1]
  "Returns [carry s4 s3 s2 s1]"
  (nbit-adder [a4 a3 a2 a1] [b4 b3 b2 b1]))

(comment
(four-bit-adder false true true false  true false true true)
;; [true false false false true]
)


```



### Using Bitwise Operators


```clojure

(defn to-binary-seq [^long x]
  (map #(- (int %) (int \0))
       (Long/toBinaryString x)))

(defn half-adder [a b]
  [(bit-xor a b)
   (bit-and a b)])

(defn full-adder [a b carry]
  (let [added (half-adder b carry)
        half-sum (first added)]
    [(first (half-adder a half-sum))
     (bit-or (second (half-adder a half-sum)) (second added))]))

(defn ripple-carry-adder [a b]
  (loop [a (reverse a)
         b (reverse b)
         sum '()
         carry 0]
    (let [added (full-adder (first a) (first b) carry)]
      (if (and (empty? (next a)) (empty? (next b)))
        (conj sum (first added) (bit-or carry 1))
        (recur (next a) (next b) (conj sum (first added)) (second added))))))

(deftest adder
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 10) (to-binary-seq 10))) 2)
         (+ 10 10)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 50) (to-binary-seq 50))) 2)
         (+ 50 50)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 32) (to-binary-seq 38))) 2)
         (+ 32 38)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 130) (to-binary-seq 250))) 2)
         (+ 130 250))))

```



## COBOL


```COBOL

       program-id. test-add.
       environment division.
       configuration section.
       special-names.
           class bin is "0" "1".
       data division.
       working-storage section.
       1 parms.
        2 a-in pic 9999.
        2 b-in pic 9999.
        2 r-out pic 9999.
        2 c-out pic 9.
       procedure division.
           display "Enter 'A' value (4-bits binary): "
               with no advancing
           accept a-in
           if a-in (1:) not bin
               display "A is not binary"
               stop run
           end-if
           display "Enter 'B' value (4-bits binary): "
               with no advancing
           accept b-in
           if b-in (1:) not bin
               display "B is not binary"
               stop run
           end-if
           call "add-4b" using parms
           display "Carry " c-out " result " r-out
           stop run
           .
       end program test-add.

       program-id. add-4b.
       data division.
       working-storage section.
       1 wk binary.
        2 i pic 9(4).
        2 occurs 5.
         3 a-reg pic 9.
         3 b-reg pic 9.
         3 c-reg pic 9.
         3 r-reg pic 9.
        2 a pic 9.
        2 b pic 9.
        2 c pic 9.
        2 a-not pic 9.
        2 b-not pic 9.
        2 c-not pic 9.
        2 ha-1s pic 9.
        2 ha-1c pic 9.
        2 ha-1s-not pic 9.
        2 ha-1c-not pic 9.
        2 ha-2s pic 9.
        2 ha-2c pic 9.
        2 fa-s pic 9.
        2 fa-c pic 9.
       linkage section.
       1 parms.
        2 a-in pic 9999.
        2 b-in pic 9999.
        2 r-out pic 9999.
        2 c-out pic 9.
       procedure division using parms.
           initialize wk
           perform varying i from 1 by 1
           until i > 4
               move a-in (5 - i:1) to a-reg (i)
               move b-in (5 - i:1) to b-reg (i)
           end-perform
           perform simulate-adder varying i from 1 by 1
               until i > 4
           move c-reg (5) to c-out
           perform varying i from 1 by 1
           until i > 4
               move r-reg (i) to r-out (5 - i:1)
           end-perform
           exit program
           .

       simulate-adder section.
           move a-reg (i) to a
           move b-reg (i) to b
           move c-reg (i) to c
           add a -1 giving a-not
           add b -1 giving b-not
           add c -1 giving c-not

           compute ha-1s = function max (
               function min ( a b-not )
               function min ( b a-not ) )
           compute ha-1c = function min ( a b )
           add ha-1s -1 giving ha-1s-not
           add ha-1c -1 giving ha-1c-not

           compute ha-2s = function max (
               function min ( c ha-1s-not )
               function min ( ha-1s c-not ) )
           compute ha-2c = function min ( c ha-1c )

           compute fa-s = ha-2s
           compute fa-c = function max ( ha-1c ha-2c )

           move fa-s to r-reg (i)
           move fa-c to c-reg (i + 1)
           .
       end program add-4b.


```

```txt

Enter 'A' value (4-bits binary): 0011
Enter 'B' value (4-bits binary): 1010
Carry 0 result 1101

Enter 'A' value (4-bits binary): 1100
Enter 'B' value (4-bits binary): 1010
Carry 1 result 0110

```



## CoffeeScript

This code models gates as functions.  The connection of gates is done via custom logic, which doesn't involve any cheating, but a really good solution would be more constructive, i.e. it would show more of a notion of "connecting" up gates, using some kind of graph data structure.


```coffeescript

# ATOMIC GATES
not_gate = (bit) ->
  [1, 0][bit]

and_gate = (bit1, bit2) ->
  bit1 and bit2

or_gate = (bit1, bit2) ->
  bit1 or bit2

# COMPOSED GATES
xor_gate = (A, B) ->
  X = and_gate A, not_gate(B)
  Y = and_gate not_gate(A), B
  or_gate X, Y

half_adder = (A, B) ->
  S = xor_gate A, B
  C = and_gate A, B
  [S, C]

full_adder = (C0, A, B) ->
  [SA, CA] = half_adder C0, A
  [SB, CB] = half_adder SA, B
  S = SB
  C = or_gate CA, CB
  [S, C]

n_bit_adder = (n) ->
  (A_bits, B_bits) ->
    s = []
    C = 0
    for i in [0...n]
      [S, C] = full_adder C, A_bits[i], B_bits[i]
      s.push S
    [s, C]

adder = n_bit_adder(4)
console.log adder [1, 0, 1, 0], [0, 1, 1, 0]

```



## Common Lisp


```lisp
;; returns a list of bits: '(sum carry)
(defun half-adder (a b)
  (list (logxor a b) (logand a b)))

;; returns a list of bits: '(sum, carry)
(defun full-adder (a b c-in)
  (let*
    ((h1 (half-adder c-in a))
    (h2 (half-adder (first h1) b)))
    (list (first h2) (logior (second h1) (second h2)))))

;; a and b are lists of 4 bits each
(defun 4-bit-adder (a b)
  (let*
    ((add-1 (full-adder (fourth a) (fourth b) 0))
      (add-2 (full-adder (third a) (third b) (second add-1)))
      (add-3 (full-adder (second a) (second b) (second add-2)))
      (add-4 (full-adder (first a) (first b) (second add-3))))
    (list
      (list (first add-4) (first add-3) (first add-2) (first add-1))
      (second add-4))))

(defun main ()
  (print (4-bit-adder (list 0 0 0 0) (list 0 0 0 0)))   ;; '(0 0 0 0) and 0
  (print (4-bit-adder (list 0 0 0 0) (list 1 1 1 1)))   ;; '(1 1 1 1) and 0
  (print (4-bit-adder (list 1 1 1 1) (list 0 0 0 0)))   ;; '(1 1 1 1) and 0
  (print (4-bit-adder (list 0 1 0 1) (list 1 1 0 0)))   ;; '(0 0 0 1) and 1
  (print (4-bit-adder (list 1 1 1 1) (list 1 1 1 1)))   ;; '(1 1 1 0) and 1
  (print (4-bit-adder (list 1 0 1 0) (list 0 1 0 1)))   ;; '(1 1 1 1) and 0
 )

(main)

```

output:

```txt

((0 0 0 0) 0)
((1 1 1 1) 0)
((1 1 1 1) 0)
((0 0 0 1) 1)
((1 1 1 0) 1)
((1 1 1 1) 0)

```



## D

From the C version. An example of SWAR (SIMD Within A Register) code, that performs 32 (or 64) 4-bit adds in parallel.

```d
import std.stdio, std.traits;

void fourBitsAdder(T)(in T a0, in T a1, in T a2, in T a3,
                      in T b0, in T b1, in T b2, in T b3,
                      out T o0, out T o1,
                      out T o2, out T o3,
                      out T overflow) pure nothrow @nogc {

    // A XOR using only NOT, AND and OR, as task requires.
    static T xor(in T x, in T y) pure nothrow @nogc {
        return (~x & y) | (x & ~y);
    }

    static void halfAdder(in T a, in T b,
                          out T s, out T c) pure nothrow @nogc {
        s = xor(a, b);
        // s = a ^ b; // The built-in D xor.
        c = a & b;
    }

    static void fullAdder(in T a, in T b, in T ic,
                          out T s, out T oc) pure nothrow @nogc {
        T ps, pc, tc;

        halfAdder(/*in*/a, b,   /*out*/ps, pc);
        halfAdder(/*in*/ps, ic, /*out*/s, tc);
        oc = tc | pc;
    }

    T zero, tc0, tc1, tc2;

    fullAdder(/*in*/a0, b0, zero, /*out*/o0, tc0);
    fullAdder(/*in*/a1, b1, tc0,  /*out*/o1, tc1);
    fullAdder(/*in*/a2, b2, tc1,  /*out*/o2, tc2);
    fullAdder(/*in*/a3, b3, tc2,  /*out*/o3, overflow);
}

void main() {
    alias T = size_t;
    static assert(isUnsigned!T);

    enum T one = T.max,
           zero = T.min,
           a0 = zero, a1 = one, a2 = zero, a3 = zero,
           b0 = zero, b1 = one, b2 = one,  b3 = one;
    T s0, s1, s2, s3, overflow;

    fourBitsAdder(/*in*/ a0, a1, a2, a3,
                  /*in*/ b0, b1, b2, b3,
                  /*out*/s0, s1, s2, s3, overflow);

    writefln("      a3 %032b", a3);
    writefln("      a2 %032b", a2);
    writefln("      a1 %032b", a1);
    writefln("      a0 %032b", a0);
    writefln("      +");
    writefln("      b3 %032b", b3);
    writefln("      b2 %032b", b2);
    writefln("      b1 %032b", b1);
    writefln("      b0 %032b", b0);
    writefln("      =");
    writefln("      s3 %032b", s3);
    writefln("      s2 %032b", s2);
    writefln("      s1 %032b", s1);
    writefln("      s0 %032b", s0);
    writefln("overflow %032b", overflow);
}
```

```txt
      a3 00000000000000000000000000000000
      a2 00000000000000000000000000000000
      a1 11111111111111111111111111111111
      a0 00000000000000000000000000000000
      +
      b3 11111111111111111111111111111111
      b2 11111111111111111111111111111111
      b1 11111111111111111111111111111111
      b0 00000000000000000000000000000000
      =
      s3 00000000000000000000000000000000
      s2 00000000000000000000000000000000
      s1 00000000000000000000000000000000
      s0 00000000000000000000000000000000
overflow 11111111111111111111111111111111
```

128 4-bit adds in parallel:

```d
import std.stdio, std.traits, core.simd;

void fourBitsAdder(T)(in T a0, in T a1, in T a2, in T a3,
                      in T b0, in T b1, in T b2, in T b3,
                      out T o0, out T o1,
                      out T o2, out T o3,
                      out T overflow) pure nothrow {

    // A XOR using only NOT, AND and OR, as task requires.
    static T xor(in T x, in T y) pure nothrow {
        return (~x & y) | (x & ~y);
    }

    static void halfAdder(in T a, in T b,
                          out T s, out T c) pure nothrow {
        s = xor(a, b);
        // s = a ^ b; // The built-in D xor.
        c = a & b;
    }

    static void fullAdder(in T a, in T b, in T ic,
                          out T s, out T oc) pure nothrow {
        T ps, pc, tc;

        halfAdder(/*in*/a, b,   /*out*/ps, pc);
        halfAdder(/*in*/ps, ic, /*out*/s, tc);
        oc = tc | pc;
    }

    T zero, tc0, tc1, tc2;

    fullAdder(/*in*/a0, b0, zero, /*out*/o0, tc0);
    fullAdder(/*in*/a1, b1, tc0,  /*out*/o1, tc1);
    fullAdder(/*in*/a2, b2, tc1,  /*out*/o2, tc2);
    fullAdder(/*in*/a3, b3, tc2,  /*out*/o3, overflow);
}

int main() {
    alias T = ubyte16; // ubyte32 with AVX.
    immutable T zero;
    immutable T one = ubyte.max;
    immutable T a0 = zero, a1 = one, a2 = zero, a3 = zero,
                b0 = zero, b1 = one, b2 = one,  b3 = one;
    T s0, s1, s2, s3, overflow;

    fourBitsAdder(/*in*/ a0, a1, a2, a3,
                  /*in*/ b0, b1, b2, b3,
                  /*out*/s0, s1, s2, s3, overflow);

    //writefln("      a3 %(%08b%)", a3);
    writefln("      a3 %(%08b%)", a3.array);
    writefln("      a2 %(%08b%)", a2.array);
    writefln("      a1 %(%08b%)", a1.array);
    writefln("      a0 %(%08b%)", a0.array);
    writefln("      +");
    writefln("      b3 %(%08b%)", b3.array);
    writefln("      b2 %(%08b%)", b2.array);
    writefln("      b1 %(%08b%)", b1.array);
    writefln("      b0 %(%08b%)", b0.array);
    writefln("      =");
    writefln("      s3 %(%08b%)", s3.array);
    writefln("      s2 %(%08b%)", s2.array);
    writefln("      s1 %(%08b%)", s1.array);
    writefln("      s0 %(%08b%)", s0.array);
    writefln("overflow %(%08b%)", overflow.array);
}
```

```txt
      a3 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      a2 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      a1 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
      a0 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      +
      b3 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
      b2 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
      b1 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
      b0 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      =
      s3 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      s2 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      s1 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      s0 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
overflow 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
```

Compiled by the ldc2 compiler to (where T = ubyte32, 256 adds using AVX2):

```asm
fourBitsAdder:
    pushl       %ebp
    movl        %esp,   %ebp
    andl        $-32,   %esp
    subl        $32,    %esp
    vmovaps 136(%ebp),  %ymm4
    vxorps      %ymm3,  %ymm4, %ymm5
    movl     20(%ebp),  %ecx
    vmovaps     %ymm5, (%ecx)
    vandps      %ymm3,  %ymm4, %ymm3
    vmovaps 104(%ebp),  %ymm4
    vxorps      %ymm2,  %ymm4, %ymm5
    vxorps      %ymm3,  %ymm5, %ymm6
    movl     16(%ebp),  %ecx
    vmovaps     %ymm6, (%ecx)
    vandps      %ymm3,  %ymm5, %ymm3
    vandps      %ymm2,  %ymm4, %ymm2
    vorps       %ymm2,  %ymm3, %ymm2
    vmovaps  72(%ebp),  %ymm3
    vxorps      %ymm1,  %ymm3, %ymm4
    vxorps      %ymm2,  %ymm4, %ymm5
    movl     12(%ebp),  %ecx
    vmovaps    %ymm5,  (%ecx)
    vandps      %ymm2,  %ymm4, %ymm2
    vandps      %ymm1,  %ymm3, %ymm1
    vorps       %ymm1,  %ymm2, %ymm1
    vmovaps  40(%ebp),  %ymm2
    vxorps      %ymm0,  %ymm2, %ymm3
    vxorps      %ymm1,  %ymm3, %ymm4
    movl      8(%ebp),  %ecx
    vmovaps     %ymm4, (%ecx)
    vandps      %ymm1,  %ymm3, %ymm1
    vandps      %ymm0,  %ymm2, %ymm0
    vorps       %ymm0,  %ymm1, %ymm0
    vmovaps     %ymm0, (%eax)
    movl        %ebp,   %esp
    popl        %ebp
    vzeroupper
    ret         $160
```



## Elixir

```elixir
defmodule RC do
  use Bitwise
  @bit_size 4

  def four_bit_adder(a, b) do           # returns pair {sum, carry}
    a_bits = binary_string_to_bits(a)
    b_bits = binary_string_to_bits(b)
    Enum.zip(a_bits, b_bits)
    |> List.foldr({[], 0}, fn {a_bit, b_bit}, {acc, carry} ->
         {s, c} = full_adder(a_bit, b_bit, carry)
         {[s | acc], c}
       end)
  end

  defp full_adder(a, b, c0) do
    {s, c} = half_adder(c0, a)
    {s, c1} = half_adder(s, b)
    {s, bor(c, c1)}                     # returns pair {sum, carry}
  end

  defp half_adder(a, b) do
    {bxor(a, b), band(a, b)}            # returns pair {sum, carry}
  end

  def int_to_binary_string(n) do
    Integer.to_string(n,2) |> String.rjust(@bit_size, ?0)
  end

  defp binary_string_to_bits(s) do
    String.codepoints(s) |> Enum.map(fn bit -> String.to_integer(bit) end)
  end

  def task do
    IO.puts " A    B      A      B   C    S  sum"
    Enum.each(0..15, fn a ->
      bin_a = int_to_binary_string(a)
      Enum.each(0..15, fn b ->
        bin_b = int_to_binary_string(b)
        {sum, carry} = four_bit_adder(bin_a, bin_b)
        :io.format "~2w + ~2w = ~s + ~s = ~w ~s = ~2w~n",
            [a, b, bin_a, bin_b, carry, Enum.join(sum), Integer.undigits([carry | sum], 2)]
      end)
    end)
  end
end

RC.task
```


```txt

 A    B      A      B   C    S  sum
 0 +  0 = 0000 + 0000 = 0 0000 =  0
 0 +  1 = 0000 + 0001 = 0 0001 =  1
 0 +  2 = 0000 + 0010 = 0 0010 =  2
 0 +  3 = 0000 + 0011 = 0 0011 =  3
 0 +  4 = 0000 + 0100 = 0 0100 =  4
...
 7 + 13 = 0111 + 1101 = 1 0100 = 20
 7 + 14 = 0111 + 1110 = 1 0101 = 21
 7 + 15 = 0111 + 1111 = 1 0110 = 22
 8 +  0 = 1000 + 0000 = 0 1000 =  8
 8 +  1 = 1000 + 0001 = 0 1001 =  9
 8 +  2 = 1000 + 0010 = 0 1010 = 10
...
15 + 12 = 1111 + 1100 = 1 1011 = 27
15 + 13 = 1111 + 1101 = 1 1100 = 28
15 + 14 = 1111 + 1110 = 1 1101 = 29
15 + 15 = 1111 + 1111 = 1 1110 = 30

```



## Erlang

Yes, it is misleading to have a "choose your own number of bits" adder in the four_bit_adder module. But it does make it easier to find the module from the Rosettacode task name.

```Erlang

-module( four_bit_adder ).

-export( [add_bits/3, create/1, task/0] ).

add_bits( Adder, A_bits, B_bits ) ->
	Adder ! {erlang:self(), lists:reverse(A_bits), lists:reverse(B_bits)},
	receive
	{Adder, Sum, Carry} -> {Sum, Carry}
	end.

create( How_many_bits ) ->
	Full_adders = connect_full_adders( [full_adder_create() || _X <- lists:seq(1, How_many_bits)] ),
	erlang:spawn_link( fun() -> bit_adder_loop( Full_adders ) end ).

task() ->
	Adder = create( 4 ),
	add_bits( Adder, [0,0,1,0], [0,0,1,1] ).



bit_adder_loop( Full_adders ) ->
	receive
	{Pid, As, Bs} ->
		Sum = [full_adder_sum(Adder, A, B) || {Adder, A, B} <- lists:zip3(Full_adders, As, Bs)],
		Carry = receive
			{carry, C} -> C
			end,
		Pid ! {erlang:self(), lists:reverse(Sum), Carry},
		bit_adder_loop( Full_adders )
	end.

connect_full_adders( [Full_adder | T]=Full_adders ) ->
	lists:foldl( fun connect_full_adders/2, Full_adder, T ),
	Full_adders.

connect_full_adders( Full_adder, Previous_full_adder ) ->
	Previous_full_adder ! {carry_to, Full_adder},
	Full_adder.

half_adder( A, B ) -> {z_xor(A, B), A band B}.

full_adder( A, B, Carry ) ->
	{Sum1, Carry1} = half_adder( A, Carry),
	{Sum, Carry2} = half_adder( B, Sum1),
	{Sum, Carry1 bor Carry2}.

full_adder_create( ) -> erlang:spawn( fun() -> full_adder_loop({0, no_carry_pid}) end ).

full_adder_loop( {Carry, Carry_to} ) ->
	receive
	{carry, New_carry} -> full_adder_loop( {New_carry, Carry_to} );
	{carry_to, Pid} -> full_adder_loop( {Carry, Pid} );
	{add, Pid, A, B} ->
		{Sum, New_carry} = full_adder( A, B, Carry ),
		Pid ! {sum, erlang:self(), Sum},
		full_adder_loop_carry_pid( Carry_to, Pid ) ! {carry, New_carry},
		full_adder_loop( {New_carry, Carry_to} )
	end.

full_adder_loop_carry_pid( no_carry_pid, Pid ) -> Pid;
full_adder_loop_carry_pid( Pid, _Pid ) -> Pid.

full_adder_sum( Pid, A, B ) ->
	Pid ! {add, erlang:self(), A, B},
	receive
	{sum, Pid, S} -> S
	end.

%% xor exists, this is another implementation.
z_xor( A, B ) -> (A band (2+bnot B)) bor ((2+bnot A) band B).

```

```txt

28> four_bit_adder:task().
{[0,1,0,1],0}

```



## Forth


```forth
: "NOT" invert 1 and ;
: "XOR" over over "NOT" and >r swap "NOT" and r> or ;
: halfadder over over and >r "XOR" r> ;
: fulladder halfadder >r swap halfadder r> or ;

: 4bitadder                            ( a3 a2 a1 a0 b3 b2 b1 b0 -- r3 r2 r1 r0 c)
  4 roll 0  fulladder swap >r >r
  3 roll r> fulladder swap >r >r
  2 roll r> fulladder swap >r fulladder r> r> r> 3 roll
;

: .add4 4bitadder 0 .r 4 0 do i 3 - abs roll 0 .r loop cr ;
```

```txt
1 1 0 0     0 0 1 1  .add4 01111
 ok
```



## Fortran

```fortran
module logic
  implicit none

contains

function xor(a, b)
  logical :: xor
  logical, intent(in) :: a, b

  xor = (a .and. .not. b) .or. (b .and. .not. a)
end function xor

function halfadder(a, b, c)
  logical :: halfadder
  logical, intent(in)  :: a, b
  logical, intent(out) :: c

  halfadder = xor(a, b)
  c = a .and. b
end function halfadder

function fulladder(a, b, c0, c1)
  logical :: fulladder
  logical, intent(in)  :: a, b, c0
  logical, intent(out) :: c1
  logical :: c2, c3

  fulladder = halfadder(halfadder(c0, a, c2), b, c3)
  c1 = c2 .or. c3
end function fulladder

subroutine fourbitadder(a, b, s)
  logical, intent(in)  :: a(0:3), b(0:3)
  logical, intent(out) :: s(0:4)
  logical :: c0, c1, c2

  s(0) = fulladder(a(0), b(0), .false., c0)
  s(1) = fulladder(a(1), b(1), c0, c1)
  s(2) = fulladder(a(2), b(2), c1, c2)
  s(3) = fulladder(a(3), b(3), c2, s(4))
end subroutine fourbitadder
end module

program Four_bit_adder
  use logic
  implicit none

  logical, dimension(0:3) :: a, b
  logical, dimension(0:4) :: s
  integer, dimension(0:3) :: ai, bi
  integer, dimension(0:4) :: si
  integer :: i, j

  do i = 0, 15
    a(0) = btest(i, 0); a(1) = btest(i, 1); a(2) = btest(i, 2); a(3) = btest(i, 3)
    where(a)
      ai = 1
    else where
      ai = 0
    end where
    do j = 0, 15
      b(0) = btest(j, 0); b(1) = btest(j, 1); b(2) = btest(j, 2); b(3) = btest(j, 3)
      where(b)
        bi = 1
      else where
        bi = 0
      end where
      call fourbitadder(a, b, s)
      where (s)
        si = 1
      elsewhere
        si = 0
      end where
      write(*, "(4i1,a,4i1,a,5i1)") ai(3:0:-1), " + ", bi(3:0:-1), " = ", si(4:0:-1)
    end do
  end do
end program
```

{{out}} (selected)

```txt
1100 + 1100 = 11000
1100 + 1101 = 11001
1100 + 1110 = 11010
1100 + 1111 = 11011
1101 + 0000 = 01101
1101 + 0001 = 01110
1101 + 0010 = 01111
1101 + 0011 = 10000
```



## F#


```fsharp

type Bit =
    | Zero
    | One

let bNot = function
    | Zero -> One
    | One -> Zero

let bAnd a b =
    match (a, b) with
    | (One, One) -> One
    | _ -> Zero

let bOr a b =
    match (a, b) with
    | (Zero, Zero) -> Zero
    | _ -> One

let bXor a b = bAnd (bOr a b) (bNot (bAnd a b))

let bHA a b = bAnd a b, bXor a b

let bFA a b cin =
    let (c0, s0) = bHA a b
    let (c1, s1) = bHA s0 cin
    (bOr c0 c1, s1)

let b4A (a3, a2, a1, a0) (b3, b2, b1, b0) =
    let (c1, s0) = bFA a0 b0 Zero
    let (c2, s1) = bFA a1 b1 c1
    let (c3, s2) = bFA a2 b2 c2
    let (c4, s3) = bFA a3 b3 c3
    (c4, s3, s2, s1, s0)

printfn "0001 + 0111 ="
b4A (Zero, Zero, Zero, One) (Zero, One, One, One) |> printfn "%A"

```

```txt

0001 + 0111 =
(Zero, One, Zero, Zero,

```



## Go


### Bytes

Go does not have a bit type, so byte is used.
This is the straightforward solution using bytes and functions.

```go
package main

import "fmt"

func xor(a, b byte) byte {
    return a&(^b) | b&(^a)
}

func ha(a, b byte) (s, c byte) {
    return xor(a, b), a & b
}

func fa(a, b, c0 byte) (s, c1 byte) {
    sa, ca := ha(a, c0)
    s, cb := ha(sa, b)
    c1 = ca | cb
    return
}

func add4(a3, a2, a1, a0, b3, b2, b1, b0 byte) (v, s3, s2, s1, s0 byte) {
    s0, c0 := fa(a0, b0, 0)
    s1, c1 := fa(a1, b1, c0)
    s2, c2 := fa(a2, b2, c1)
    s3, v = fa(a3, b3, c2)
    return
}

func main() {
    // add 10+9  result should be 1 0 0 1 1
    fmt.Println(add4(1, 0, 1, 0, 1, 0, 0, 1))
}
```

```txt

1 0 0 1 1

```



### Channels

Alternative solution is a little more like a simulation.

```go
package main

import "fmt"

// A wire is modeled as a channel of booleans.
// You can feed it a single value without blocking.
// Reading a value blocks until a value is available.
type Wire chan bool

func MkWire() Wire {
	return make(Wire, 1)
}

// A source for zero values.
func Zero() (r Wire) {
	r = MkWire()
	go func() {
		for {
			r <- false
		}
	}()
	return
}

// And gate.
func And(a, b Wire) (r Wire) {
	r = MkWire()
	go func() {
		for {
			r <- (<-a && <-b)
		}
	}()
	return
}

// Or gate.
func Or(a, b Wire) (r Wire) {
	r = MkWire()
	go func() {
		for {
			r <- (<-a || <-b)
		}
	}()
	return
}

// Not gate.
func Not(a Wire) (r Wire) {
	r = MkWire()
	go func() {
		for {
			r <- !(<-a)
		}
	}()
	return
}

// Split a wire in two.
func Split(a Wire) (Wire, Wire) {
	r1 := MkWire()
	r2 := MkWire()
	go func() {
		for {
			x := <-a
			r1 <- x
			r2 <- x
		}
	}()
	return r1, r2
}

// Xor gate, composed of Or, And and Not gates.
func Xor(a, b Wire) Wire {
	a1, a2 := Split(a)
	b1, b2 := Split(b)
	return Or(And(Not(a1), b1), And(a2, Not(b2)))
}

// A half adder, composed of two splits and an And and Xor gate.
func HalfAdder(a, b Wire) (sum, carry Wire) {
	a1, a2 := Split(a)
	b1, b2 := Split(b)
	carry = And(a1, b1)
	sum = Xor(a2, b2)
	return
}

// A full adder, composed of two half adders, and an Or gate.
func FullAdder(a, b, carryIn Wire) (result, carryOut Wire) {
	s1, c1 := HalfAdder(carryIn, a)
	result, c2 := HalfAdder(b, s1)
	carryOut = Or(c1, c2)
	return
}

// A four bit adder, composed of a zero source, and four full adders.
func FourBitAdder(a1, a2, a3, a4 Wire, b1, b2, b3, b4 Wire) (r1, r2, r3, r4 Wire, carry Wire) {
	carry = Zero()
	r1, carry = FullAdder(a1, b1, carry)
	r2, carry = FullAdder(a2, b2, carry)
	r3, carry = FullAdder(a3, b3, carry)
	r4, carry = FullAdder(a4, b4, carry)
	return
}

func main() {
	// Create wires
	a1, a2, a3, a4 := MakeWire(), MakeWire(), MakeWire(), MakeWire()
	b1, b2, b3, b4 := MakeWire(), MakeWire(), MakeWire(), MakeWire()
	// Construct circuit
	r1, r2, r3, r4, carry := FourBitAdder(a1, a2, a3, a4, b1, b2, b3, b4)
	// Feed it some values
	a4 <- false
	a3 <- false
	a2 <- true
	a1 <- false // 0010
	b4 <- true
	b3 <- true
	b2 <- true
	b1 <- false // 1110
	B := map[bool]int{false: 0, true: 1}
	// Read the result
	fmt.Printf("0010 + 1110 = %d%d%d%d (carry = %d)\n",
		B[<-r4], B[<-r3], B[<-r2], B[<-r1], B[<-carry])
}

```


Mini reference:

* "<tt>channel <- value</tt>" sends a value to a channel. Blocks if its buffer is full.
* "<tt><-channel</tt>" reads a value from a channel. Blocks if its buffer is empty.
* "go function()" creates and runs a go-rountine. It will continue executing concurrently.


## Haskell

Basic gates:

```haskell
import Control.Arrow
import Data.List (mapAccumR)

bor, band :: Int -> Int -> Int
bor = max
band = min
bnot :: Int -> Int
bnot = (1-)
```

Gates built with basic ones:

```haskell
nand, xor :: Int -> Int -> Int
nand = (bnot.).band
xor a b = uncurry nand. (nand a &&& nand b) $ nand a b
```

Adder circuits:

```haskell
halfAdder = uncurry band &&& uncurry xor
fullAdder (c, a, b) =  (\(cy,s) ->  first (bor cy) $ halfAdder (b, s)) $ halfAdder (c, a)

adder4 as = mapAccumR (\cy (f,a,b) -> f (cy,a,b)) 0 . zip3 (replicate 4 fullAdder) as
```


Example using adder4

```haskell
*Main> adder4 [1,0,1,0] [1,1,1,1]
(1,[1,0,0,1])
```


=={{header|Icon}} and {{header|Unicon}}==

Based on the algorithms shown in the Fortran entry, but Unicon does not allow pass by reference for immutable types, so a small <code>carry</code> record is used instead.


```unicon
#
# 4bitadder.icn, emulate a 4 bit adder. Using only and, or, not
#
record carry(c)

#
# excercise the adder, either "test" or 2 numbers
#
procedure main(argv)
    c := carry(0)

    # cli test
    if map(\argv[1]) == "test" then {
        # Unicon allows explicit radix literals
        every i := (2r0000  | 2r1001 | 2r1111) do {
            write(i, "+0,3,9,15")
            every j := (0 | 3 | 9 | 15) do {
                ans := fourbitadder(t1 := fourbits(i), t2 := fourbits(j), c)
                write(t1, " + ", t2, " = ", c.c, ":", ans)
            }
        }
        return
    }
    # command line, two values, if given, first try four bit binaries
    cli := fourbitadder(t1 := (*\argv[1] = 4 & fourbits("2r" || argv[1])),
                        t2 := (*\argv[2] = 4 & fourbits("2r" || argv[2])), c)
    write(t1, " + ", t2, " = ", c.c, ":", \cli) & return

    # if no result for that, try decimal values
    cli := fourbitadder(t1 := fourbits(\argv[1]),
                        t2 := fourbits(\argv[2]), c)
    write(t1, " + ", t2, " = ", c.c, ":", \cli) & return

    # or display the help
    write("Usage: 4bitadder [\"test\"] | [bbbb bbbb] | [n n], range 0-15")
end

#
# integer to fourbits as string
#
procedure fourbits(i)
    local s, t
    if not numeric(i) then fail
    if not (0 <= integer(i) < 16) then {
        write("out of range: ", i)
        fail
    }
    s := ""
    every t := (8 | 4 | 2 | 1) do {
        s ||:= if iand(i, t) ~= 0 then "1" else "0"
    }
    return s
end

#
# low level xor emulation with or, and, not
#
procedure xor(a, b)
    return ior(iand(a, icom(b)), iand(b, icom(a)))
end

#
# half adder, and into carry, xor for result bit
#
procedure halfadder(a, b, carry)
    carry.c := iand(a,b)
    return xor(a,b)
end

#
# full adder, two half adders, or for carry
#
procedure fulladder(a, b, c0, c1)
    local c2, c3, r
    c2 := carry(0)
    c3 := carry(0)

    # connect two half adders with carry
    r := halfadder(halfadder(c0.c, a, c2), b, c3)
    c1.c := ior(c2.c, c3.c)
    return r
end

#
# fourbit adder, as bit string
#
procedure fourbitadder(a, b, cr)
    local cs, c0, c1, c2, s
    cs := carry(0)
    c0 := carry(0)
    c1 := carry(0)
    c2 := carry(0)

    # create a string for subscripting. strings are immutable, new strings created
    s := "0000"
    # bit 0 is string position 4
    s[4+:1] := fulladder(a[4+:1], b[4+:1], cs, c0)
    s[3+:1] := fulladder(a[3+:1], b[3+:1], c0, c1)
    s[2+:1] := fulladder(a[2+:1], b[2+:1], c1, c2)
    s[1+:1] := fulladder(a[1+:1], b[1+:1], c2, cr)
    # cr.c is the overflow carry
    return s
end
```


```txt
prompt$ unicon -s 4bitadder.icn -x 0111 0011
0111 + 0011 = 0:1010
prompt$ ./4bitadder 13 13
1101 + 1101 = 1:1010
prompt$ ./4bitadder test
0+0,3,9,15
0000 + 0000 = 0:0000
0000 + 0011 = 0:0011
0000 + 1001 = 0:1001
0000 + 1111 = 0:1111
9+0,3,9,15
1001 + 0000 = 0:1001
1001 + 0011 = 0:1100
1001 + 1001 = 1:0010
1001 + 1111 = 1:1000
15+0,3,9,15
1111 + 0000 = 0:1111
1111 + 0011 = 1:0010
1111 + 1001 = 1:1000
1111 + 1111 = 1:1110
```




## J



### Implementation


```j
and=: *.
or=: +.
not=: -.
xor=: (and not) or (and not)~
hadd=: and ,"0 xor
add=: ((({.,0:)@[ or {:@[ hadd {.@]), }.@])/@hadd
```



### Example use


```j
   1 1 1 1 add 0 1 1 1
1 0 1 1 0
```


To produce all results:

```j
   add"1/~#:i.16
```


This will produce a 16 by 16 by 5 array, the first axis being the left argument (representing values 0..15), the second axis the right argument and the final axis being the bit indices (carry, 8, 4, 2, 1).  In other words, the result is something like:


```j
   ,"2 ' ',"1 -.&' '@":"1 add"1/~#:i.16
 00000 00001 00010 00011 00100 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111
 00001 00010 00011 00100 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000
 00010 00011 00100 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001
 00011 00100 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010
 00100 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011
 00101 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100
 00110 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101
 00111 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110
 01000 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111
 01001 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000
 01010 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001
 01011 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001 11010
 01100 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001 11010 11011
 01101 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001 11010 11011 11100
 01110 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001 11010 11011 11100 11101
 01111 10000 10001 10010 10011 10100 10101 10110 10111 11000 11001 11010 11011 11100 11101 11110
```


Alternatively, the fact that add was designed to operate on lists of bits could have been incorporated into its definition:


```j
add=: ((({.,0:)@[ or {:@[ hadd {.@]), }.@])/@hadd"1
```


Then to get all results you could use:

```j
   add/~#:i.16
```


Compare this to a regular addition table:

```j
   +/~i.10
```

(this produces a 10 by 10 array -- the results have no further internal array structure, though of course in the machine implementation integers can be thought of as being represented as fixed width lists of bits.)


### Glossary

   ~: xor
   {. first
   }. rest
   {: last
   [  left (result is left argument)
   ]  right (result is right argument)
   0: verb which always has the result 0
   ,  combine sequences


### Grammar

   u v w   these letters represent verbs such as '''and''' '''or''' or '''not'''
   x y     these letters represent nouns such as 1 or 0
   u@v     function composition
   x u~ y  reverse arguments for u (y u x)
   u/ y    reduction (u is verb between each item in y)
   u"0     u applies to the smallest elements of its argument

Also:
   x (u v) y
produces the same result as
   x u v y

while
   x (u v w) y
produces the same result as
   (x u y) v (x w y)

and
   x (u1 u2 u3 u4 u5) y
produces the the same result as
   x (u1 u2 (u3 u4 u5)) y

See also: "A Formal Description of System/360” by Adin Falkoff



## Java



```java
public class GateLogic
{
  // Basic gate interfaces
  public interface OneInputGate
  {  boolean eval(boolean input);  }

  public interface TwoInputGate
  {  boolean eval(boolean input1, boolean input2);  }

  public interface MultiGate
  {  boolean[] eval(boolean... inputs);  }

  // Create NOT
  public static OneInputGate NOT = new OneInputGate() {
    public boolean eval(boolean input)
    {  return !input;  }
  };

  // Create AND
  public static TwoInputGate AND = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {  return input1 && input2;  }
  };

  // Create OR
  public static TwoInputGate OR = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {  return input1 || input2;  }
  };

  // Create XOR
  public static TwoInputGate XOR = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {
      return OR.eval(
               AND.eval(input1, NOT.eval(input2)),
               AND.eval(NOT.eval(input1), input2)
             );
    }
  };

  // Create HALF_ADDER
  public static MultiGate HALF_ADDER = new MultiGate() {
    public boolean[] eval(boolean... inputs)
    {
      if (inputs.length != 2)
        throw new IllegalArgumentException();
      return new boolean[] {
        XOR.eval(inputs[0], inputs[1]),  // Output bit
        AND.eval(inputs[0], inputs[1])   // Carry bit
      };
    }
  };

  // Create FULL_ADDER
  public static MultiGate FULL_ADDER = new MultiGate() {
    public boolean[] eval(boolean... inputs)
    {
      if (inputs.length != 3)
        throw new IllegalArgumentException();
      // Inputs: CarryIn, A, B
      // Outputs: S, CarryOut
      boolean[] haOutputs1 = HALF_ADDER.eval(inputs[0], inputs[1]);
      boolean[] haOutputs2 = HALF_ADDER.eval(haOutputs1[0], inputs[2]);
      return new boolean[] {
        haOutputs2[0],                         // Output bit
        OR.eval(haOutputs1[1], haOutputs2[1])  // Carry bit
      };
    }
  };

  public static MultiGate buildAdder(final int numBits)
  {
    return new MultiGate() {
      public boolean[] eval(boolean... inputs)
      {
        // Inputs: A0, A1, A2..., B0, B1, B2...
        if (inputs.length != (numBits << 1))
          throw new IllegalArgumentException();
        boolean[] outputs = new boolean[numBits + 1];
        boolean[] faInputs = new boolean[3];
        boolean[] faOutputs = null;
        for (int i = 0; i < numBits; i++)
        {
          faInputs[0] = (faOutputs == null) ? false : faOutputs[1];  // CarryIn
          faInputs[1] = inputs[i];                                   // Ai
          faInputs[2] = inputs[numBits + i];                         // Bi
          faOutputs = FULL_ADDER.eval(faInputs);
          outputs[i] = faOutputs[0];                                 // Si
        }
        if (faOutputs != null)
          outputs[numBits] = faOutputs[1];                           // CarryOut
        return outputs;
      }
    };
  }

  public static void main(String[] args)
  {
    int numBits = Integer.parseInt(args[0]);
    int firstNum = Integer.parseInt(args[1]);
    int secondNum = Integer.parseInt(args[2]);
    int maxNum = 1 << numBits;
    if ((firstNum < 0) || (firstNum >= maxNum))
    {
      System.out.println("First number is out of range");
      return;
    }
    if ((secondNum < 0) || (secondNum >= maxNum))
    {
      System.out.println("Second number is out of range");
      return;
    }

    MultiGate multiBitAdder = buildAdder(numBits);
    // Convert input numbers into array of bits
    boolean[] inputs = new boolean[numBits << 1];
    String firstNumDisplay = "";
    String secondNumDisplay = "";
    for (int i = 0; i < numBits; i++)
    {
      boolean firstBit = ((firstNum >>> i) & 1) == 1;
      boolean secondBit = ((secondNum >>> i) & 1) == 1;
      inputs[i] = firstBit;
      inputs[numBits + i] = secondBit;
      firstNumDisplay = (firstBit ? "1" : "0") + firstNumDisplay;
      secondNumDisplay = (secondBit ? "1" : "0") + secondNumDisplay;
    }

    boolean[] outputs = multiBitAdder.eval(inputs);
    int outputNum = 0;
    String outputNumDisplay = "";
    String outputCarryDisplay = null;
    for (int i = numBits; i >= 0; i--)
    {
      outputNum = (outputNum << 1) | (outputs[i] ? 1 : 0);
      if (i == numBits)
        outputCarryDisplay = outputs[i] ? "1" : "0";
      else
        outputNumDisplay += (outputs[i] ? "1" : "0");
    }
    System.out.println("numBits=" + numBits);
    System.out.println("A=" + firstNumDisplay + " (" + firstNum + "), B=" + secondNumDisplay + " (" + secondNum + "), S=" + outputCarryDisplay + " " + outputNumDisplay + " (" + outputNum + ")");
    return;
  }

}
```


```txt
java GateLogic 4 9 5
numBits=4
A=1001 (9), B=0101 (5), S=0 1110 (14)

java GateLogic 16 51239 15210
numBits=16
A=1100100000100111 (51239), B=0011101101101010 (15210), S=1 0000001110010001 (66449)
```



## JavaScript



### Error Handling

In order to keep the binary-ness obvious, all operations will occur on
0s and 1s. To enforce this, we'll first create a couple of helper functions.


```JavaScript

function acceptedBinFormat(bin) {
    if (bin == 1 || bin === 0 || bin === '0')
        return true;
    else
        return bin;
}

function arePseudoBin() {
    var args = [].slice.call(arguments), len = args.length;
    while(len--)
        if (acceptedBinFormat(args[len]) !== true)
            throw new Error('argument must be 0, \'0\', 1, or \'1\', argument ' + len + ' was ' + args[len]);
    return true;
}

```



### Implementation

Now we build up the gates, starting with 'not' and 'and' as building blocks.
Those allow us to construct 'nand', 'or', and 'xor' then a half and full adders
and, finally, the four bit adder.


```JavaScript

// basic building blocks allowed by the rules are ~, &, and |, we'll fake these
// in a way that makes what they do (at least when you use them) more obvious
// than the other available options do.

function not(a) {
    if (arePseudoBin(a))
        return a == 1 ? 0 : 1;
}

function and(a, b) {
    if (arePseudoBin(a, b))
        return a + b < 2 ? 0 : 1;
}

function nand(a, b) {
    if (arePseudoBin(a, b))
        return not(and(a, b));
}

function or(a, b) {
    if (arePseudoBin(a, b))
        return nand(nand(a,a), nand(b,b));
}

function xor(a, b) {
    if (arePseudoBin(a, b))
        return nand(nand(nand(a,b), a), nand(nand(a,b), b));
}

function halfAdder(a, b) {
    if (arePseudoBin(a, b))
        return { carry: and(a, b), sum: xor(a, b) };
}

function fullAdder(a, b, c) {
    if (arePseudoBin(a, b, c)) {
        var h0 = halfAdder(a, b),
            h1 = halfAdder(h0.sum, c);
        return {carry: or(h0.carry, h1.carry), sum: h1.sum };
    }
}

function fourBitAdder(a, b) {
    if (typeof a.length == 'undefined' || typeof b.length == 'undefined')
        throw new Error('bad values');
    // not sure if the rules allow this, but we need to pad the values
    // if they're too short and trim them if they're too long
    var inA = Array(4),
        inB = Array(4),
        out = Array(4),
        i = 4,
        pass;

    while (i--) {
        inA[i] = a[i] != 1 ? 0 : 1;
        inB[i] = b[i] != 1 ? 0 : 1;
    }

    // now we can start adding... I'd prefer to do this in a loop,
    // but that wouldn't be "connecting the other 'constructive blocks',
    // in turn made of 'simpler' and 'smaller' ones"

    pass = halfAdder(inA[3], inB[3]);
    out[3] = pass.sum;
    pass = fullAdder(inA[2], inB[2], pass.carry);
    out[2] = pass.sum;
    pass = fullAdder(inA[1], inB[1], pass.carry);
    out[1] = pass.sum;
    pass = fullAdder(inA[0], inB[0], pass.carry);
    out[0] = pass.sum;
    return out.join('');
}

```


### Example Use


```JavaScript
fourBitAdder('1010', '0101'); // 1111 (15)
```


all results:


```JavaScript

// run this in your browsers console
var outer = inner = 16, a, b;

while(outer--) {
    a = (8|outer).toString(2);
    while(inner--) {
        b = (8|inner).toString(2);
        console.log(a + ' + ' + b + ' = ' + fourBitAdder(a, b));
    }
    inner = outer;
}

```




## jq

Adaptation of the JavaScript entry, but without most of the honesty checks.

All the operations except fourBitAdder(a,b) assume the inputs are presented as 0 or 1 (i.e. integers).


```jq
# Start with the 'not' and 'and' building blocks.
# These allow us to construct 'nand', 'or', and 'xor',
# and so on.

def bit_not: if . == 1 then 0 else 1 end;

def bit_and(a; b): if a == 1 and b == 1 then 1 else 0 end;

def bit_nand(a; b): bit_and(a; b) | bit_not;

def bit_or(a; b): bit_nand(bit_nand(a;a); bit_nand(b;b));

def bit_xor(a; b):
  bit_nand(bit_nand(bit_nand(a;b); a);
           bit_nand(bit_nand(a;b); b));

def halfAdder(a; b):
  { "carry": bit_and(a; b), "sum": bit_xor(a; b) };

def fullAdder(a; b; c):
  halfAdder(a; b) as $h0
  | halfAdder($h0.sum; c) as $h1
  | {"carry": bit_or($h0.carry; $h1.carry), "sum": $h1.sum };

# a and b should be strings of 0s and 1s, of length no greater than 4
def fourBitAdder(a; b):

  # pad on the left with 0s, and convert the string
  # representation ("101") to an array of integers ([1,0,1]).
  def pad: (4-length) * "0" + . | explode | map(. - 48);

  (a|pad) as $inA | (b|pad) as $inB
  | [][3] = null                                # an array for storing the four results
  | halfAdder($inA[3]; $inB[3]) as $pass
  | .[3] = $pass.sum                            # store the lsb
  | fullAdder($inA[2]; $inB[2]; $pass.carry) as $pass
  | .[2] = $pass.sum
  | fullAdder($inA[1]; $inB[1]; $pass.carry) as $pass
  | .[1] = $pass.sum
  | fullAdder($inA[0]; $inB[0]; $pass.carry) as $pass
  | .[0] = $pass.sum
  | map(tostring) | join("") ;
```

'''Example:'''

```jq
fourBitAdder("0111"; "0001")
```

 $ jq -n -f Four_bit_adder.jq
 "1000"


## Jsish

Based on Javascript entry.

```javascript
#!/usr/bin/env jsish
/* 4 bit adder simulation, in Jsish */
function not(a) { return a == 1 ? 0 : 1; }
function and(a, b) { return a + b < 2 ? 0 : 1; }
function nand(a, b) { return not(and(a, b)); }
function or(a, b) { return nand(nand(a,a), nand(b,b)); }
function xor(a, b) { return nand(nand(nand(a,b), a), nand(nand(a,b), b)); }

function halfAdder(a, b) { return { carry: and(a, b), sum: xor(a, b) }; }
function fullAdder(a, b, c) {
    var h0 = halfAdder(a, b),
        h1 = halfAdder(h0.sum, c);
    return {carry: or(h0.carry, h1.carry), sum: h1.sum };
}

function fourBitAdder(a, b) {
    // set to width 4, pad with 0 if too short and truncate right if too long
    var inA = Array(4),
        inB = Array(4),
        out = Array(4),
        i = 4,
        pass;

    if (a.length < 4) a = '0'.repeat(4 - a.length) + a;
    a = a.slice(-4);
    if (b.length < 4) b = '0'.repeat(4 - b.length) + b;
    b = b.slice(-4);
    while (i--) {
        var re = /0|1/;
        if (a[i] && !re.test(a[i])) throw('bad bit at a[' + i + '] of ' + quote(a[i]));
        if (b[i] && !re.test(b[i])) throw('bad bit at b[' + i + '] of ' + quote(b[i]));
        inA[i] = a[i] != 1 ? 0 : 1;
        inB[i] = b[i] != 1 ? 0 : 1;
    }

    printf('%s + %s = ', a, b);

    // now we can start adding... connecting the constructive blocks
    pass = halfAdder(inA[3], inB[3]);
    out[3] = pass.sum;
    pass = fullAdder(inA[2], inB[2], pass.carry);
    out[2] = pass.sum;
    pass = fullAdder(inA[1], inB[1], pass.carry);
    out[1] = pass.sum;
    pass = fullAdder(inA[0], inB[0], pass.carry);
    out[0] = pass.sum;

    var result = parseInt(pass.carry + out.join(''), 2);
    printf('%s  %d\n', out.join('') + ' carry ' + pass.carry, result);
    return result;
}

if (Interp.conf('unitTest')) {
    var bits = [['0000', '0000'], ['0000', '0001'], ['1000', '0001'],
                ['1010', '0101'], ['1000', '1000'], ['1100', '1100'],
                ['1111', '1111']];
    for (var pair of bits) {
        fourBitAdder(pair[0], pair[1]);
    }
;   fourBitAdder('1', '11');
;   fourBitAdder('10001', '01110');
;// fourBitAdder('0002', 'b');
}

/*
=!EXPECTSTART!=
0000 + 0000 = 0000 carry 0  0
0000 + 0001 = 0001 carry 0  1
1000 + 0001 = 1001 carry 0  9
1010 + 0101 = 1111 carry 0  15
1000 + 1000 = 0000 carry 1  16
1100 + 1100 = 1000 carry 1  24
1111 + 1111 = 1110 carry 1  30
fourBitAdder('1', '11') ==> 0001 + 0011 = 0100 carry 0  4
4
fourBitAdder('10001', '01110') ==> 0001 + 1110 = 1111 carry 0  15
15
fourBitAdder('0002', 'b') ==>
PASS!: err = bad bit at a[3] of "2"
=!EXPECTEND!=
*/
```

```txt
prompt$ jsish --U fourBitAdder.jsi
0000 + 0000 = 0000 carry 0  0
0000 + 0001 = 0001 carry 0  1
1000 + 0001 = 1001 carry 0  9
1010 + 0101 = 1111 carry 0  15
1000 + 1000 = 0000 carry 1  16
1100 + 1100 = 1000 carry 1  24
1111 + 1111 = 1110 carry 1  30
fourBitAdder('1', '11') ==> 0001 + 0011 = 0100 carry 0  4
4
fourBitAdder('10001', '01110') ==> 0001 + 1110 = 1111 carry 0  15
15
fourBitAdder('0002', 'b') ==>
PASS!: err = bad bit at a[3] of "2"

prompt$ jsish -u fourBitAdder.jsi
[PASS] fourBitAdder.jsi
```



## Julia

This solution implements <tt>xor</tt>, <tt>halfadder</tt> and <tt>fulladder</tt> with type <tt>Bool</tt>.  <tt>adder</tt> is implemented for addends of type <tt>BitArray</tt>, which can be or arbitrary length (though if ''a'' and ''b'' have unequal lengths it throws an error).  A helper version of <tt>adder</tt> converts integer inputs to <tt>BitArray</tt> prior to calling the base version of this function.  The length of the <tt>BitArray</tt>s used in this conversion is adjustable, but in the spirit of this task, it has a default of 4.

'''Functions'''

```Julia
using Printf

xor{T<:Bool}(a::T, b::T) = (a&~b)|(~a&b)

halfadder{T<:Bool}(a::T, b::T) = (xor(a,b), a&b)

function fulladder{T<:Bool}(a::T, b::T, c::T=false)
    (s, ca) = halfadder(c, a)
    (s, cb) = halfadder(s, b)
    (s, ca|cb)
end

function adder(a::BitArray{1}, b::BitArray{1}, c0::Bool=false)
    len = length(a)
    length(b) == len || error("Addend width mismatch.")
    c = c0
    s = falses(len)
    for i in 1:len
        (s[i], c) = fulladder(a[i], b[i], c)
    end
    (s, c)
end

function adder{T<:Integer}(m::T, n::T, wid::T=4, c0::Bool=false)
    a = bitpack(digits(m, 2, wid))[1:wid]
    b = bitpack(digits(n, 2, wid))[1:wid]
    adder(a, b, c0)
end

Base.bits(n::BitArray{1}) = join(reverse(int(n)), "")

```


'''Main'''

```Julia

xavail = trues(15,15)
xcnt = 0
xgoal = 10
println("Testing adder with 4-bit words:")
while xcnt < xgoal
    m = rand(1:15)
    n = rand(1:15)
    xavail[m,n] || continue
    xavail[m,n] = xavail[n,m] = false
    xcnt += 1
    (s, c) = adder(m, n)
    oflow = c ? "*" : ""
    print(@sprintf "    %2d + %2d = %2d => " m n m+n)
    println(@sprintf("%s + %s = %s%s",
                     bits(m)[end-3:end],
                     bits(n)[end-3:end],
                     bits(s), oflow))
end

```


```txt

Testing adder with 4-bit words:
     6 + 14 = 20 => 0110 + 1110 = 0100*
     5 +  6 = 11 => 0101 + 0110 = 1011
     5 +  3 =  8 => 0101 + 0011 = 1000
     1 +  7 =  8 => 0001 + 0111 = 1000
    15 +  6 = 21 => 1111 + 0110 = 0101*
     1 + 14 = 15 => 0001 + 1110 = 1111
     8 +  9 = 17 => 1000 + 1001 = 0001*
    14 + 10 = 24 => 1110 + 1010 = 1000*
     3 +  1 =  4 => 0011 + 0001 = 0100
     6 + 11 = 17 => 0110 + 1011 = 0001*

```



## Kotlin


```scala
// version 1.1.51

val Boolean.I get() = if (this) 1 else 0

val Int.B get() = this != 0

class Nybble(val n3: Boolean, val n2: Boolean, val n1: Boolean, val n0: Boolean) {
    fun toInt() = n0.I + n1.I * 2 + n2.I * 4 + n3.I * 8

    override fun toString() = "${n3.I}${n2.I}${n1.I}${n0.I}"
}

fun Int.toNybble(): Nybble {
    val n = BooleanArray(4)
    for (k in 0..3) n[k] = ((this shr k) and 1).B
    return Nybble(n[3], n[2], n[1], n[0])
}

fun xorGate(a: Boolean, b: Boolean) = (a && !b) || (!a && b)

fun halfAdder(a: Boolean, b: Boolean) = Pair(xorGate(a, b), a && b)

fun fullAdder(a: Boolean, b: Boolean, c: Boolean): Pair<Boolean, Boolean> {
    val (s1, c1) = halfAdder(c, a)
    val (s2, c2) = halfAdder(s1, b)
    return s2 to (c1 || c2)
}

fun fourBitAdder(a: Nybble, b: Nybble): Pair<Nybble, Int> {
    val (s0, c0) = fullAdder(a.n0, b.n0, false)
    val (s1, c1) = fullAdder(a.n1, b.n1, c0)
    val (s2, c2) = fullAdder(a.n2, b.n2, c1)
    val (s3, c3) = fullAdder(a.n3, b.n3, c2)
    return Nybble(s3, s2, s1, s0) to c3.I
}

const val f = "%s + %s = %d %s (%2d + %2d = %2d)"

fun test(i: Int, j: Int) {
    val a = i.toNybble()
    val b = j.toNybble()
    val (r, c) = fourBitAdder(a, b)
    val s = c * 16 + r.toInt()
    println(f.format(a, b, c, r, i, j, s))
}

fun main(args: Array<String>) {
    println(" A      B     C  R     I    J    S")
    for (i in 0..15) {
        for (j in i..minOf(i + 1, 15)) test(i, j)
    }
}
```


```txt

 A      B     C  R     I    J    S
0000 + 0000 = 0 0000 ( 0 +  0 =  0)
0000 + 0001 = 0 0001 ( 0 +  1 =  1)
0001 + 0001 = 0 0010 ( 1 +  1 =  2)
0001 + 0010 = 0 0011 ( 1 +  2 =  3)
0010 + 0010 = 0 0100 ( 2 +  2 =  4)
0010 + 0011 = 0 0101 ( 2 +  3 =  5)
0011 + 0011 = 0 0110 ( 3 +  3 =  6)
0011 + 0100 = 0 0111 ( 3 +  4 =  7)
0100 + 0100 = 0 1000 ( 4 +  4 =  8)
0100 + 0101 = 0 1001 ( 4 +  5 =  9)
0101 + 0101 = 0 1010 ( 5 +  5 = 10)
0101 + 0110 = 0 1011 ( 5 +  6 = 11)
0110 + 0110 = 0 1100 ( 6 +  6 = 12)
0110 + 0111 = 0 1101 ( 6 +  7 = 13)
0111 + 0111 = 0 1110 ( 7 +  7 = 14)
0111 + 1000 = 0 1111 ( 7 +  8 = 15)
1000 + 1000 = 1 0000 ( 8 +  8 = 16)
1000 + 1001 = 1 0001 ( 8 +  9 = 17)
1001 + 1001 = 1 0010 ( 9 +  9 = 18)
1001 + 1010 = 1 0011 ( 9 + 10 = 19)
1010 + 1010 = 1 0100 (10 + 10 = 20)
1010 + 1011 = 1 0101 (10 + 11 = 21)
1011 + 1011 = 1 0110 (11 + 11 = 22)
1011 + 1100 = 1 0111 (11 + 12 = 23)
1100 + 1100 = 1 1000 (12 + 12 = 24)
1100 + 1101 = 1 1001 (12 + 13 = 25)
1101 + 1101 = 1 1010 (13 + 13 = 26)
1101 + 1110 = 1 1011 (13 + 14 = 27)
1110 + 1110 = 1 1100 (14 + 14 = 28)
1110 + 1111 = 1 1101 (14 + 15 = 29)
1111 + 1111 = 1 1110 (15 + 15 = 30)

```



## LabVIEW

LabVIEW's G language is a kind of circuit diagram based programming. Thus, a circuit diagram is pseudo-code for a G block diagram, which makes coding a four bit adder trivial.

{{works with|LabVIEW|8.0 Full Development Suite}}<br/><br/>

'''Half Adder'''

[[File:Half_adder_connector.png]]  [[File:Half_adder_panel.png]]  [[File:Half_adder_diagram.png]]

'''Full Adder'''

[[File:Full_adder_connector.png]]  [[File:Full_adder_panel.png]]  [[File:Full_adder_diagram.png]]

'''4bit Adder'''

[[File:4bit_adder_connector.png]]  [[File:4bit_adder_panel.png]]  [[File:4bit_adder_diagram.png]]


## Lua


```Lua
-- Build XOR from AND, OR and NOT
function xor (a, b) return (a and not b) or (b and not a) end

-- Can make half adder now XOR exists
function halfAdder (a, b) return xor(a, b), a and b end

-- Full adder is two half adders with carry outputs OR'd
function fullAdder (a, b, cIn)
    local ha0s, ha0c = halfAdder(cIn, a)
    local ha1s, ha1c = halfAdder(ha0s, b)
    local cOut, s = ha0c or ha1c, ha1s
    return cOut, s
end

-- Carry bits 'ripple' through adders, first returned value is overflow
function fourBitAdder (a3, a2, a1, a0, b3, b2, b1, b0) -- LSB-first
    local fa0c, fa0s = fullAdder(a0, b0, false)
    local fa1c, fa1s = fullAdder(a1, b1, fa0c)
    local fa2c, fa2s = fullAdder(a2, b2, fa1c)
    local fa3c, fa3s = fullAdder(a3, b3, fa2c)
    return fa3c, fa3s, fa2s, fa1s, fa0s -- Return as MSB-first
end

-- Take string of noughts and ones, convert to native boolean type
function toBool (bitString)
    local boolList, bit = {}
    for digit = 1, 4 do
        bit = string.sub(string.format("%04d", bitString), digit, digit)
        if bit == "0" then table.insert(boolList, false) end
        if bit == "1" then table.insert(boolList, true) end
    end
    return boolList
end

-- Take list of booleans, convert to string of binary digits (variadic)
function toBits (...)
    local bStr = ""
    for i, bool in pairs{...} do
        if bool then bStr = bStr .. "1" else bStr = bStr .. "0" end
    end
    return bStr
end

-- Little driver function to neaten use of the adder
function add (n1, n2)
    local A, B = toBool(n1), toBool(n2)
    local v, s0, s1, s2, s3 = fourBitAdder( A[1], A[2], A[3], A[4],
                                            B[1], B[2], B[3], B[4] )
    return toBits(s0, s1, s2, s3), v
end

-- Main procedure (usage examples)
print("SUM", "OVERFLOW\n")
print(add(0001, 0001)) -- 1 + 1 = 2
print(add(0101, 1010)) -- 5 + 10 = 15
print(add(0000, 1111)) -- 0 + 15 = 15
print(add(0001, 1111)) -- 1 + 15 = 16 (causes overflow)
```

Output:

```txt
SUM     OVERFLOW

0010    false
1111    false
1111    false
0000    true
```



## M2000 Interpreter


```M2000 Interpreter

Module  FourBitAdder {
	Flush
	dim not(0 to 1),and(0 to 1, 0 to 1),or(0 to 1, 0 to 1)
	not(0)=1,0
	and(0,0)=0,0,0,1
	or(0,0)=0,1,1,1
	xor=lambda not(),and(),or() (a,b)-> or(and(a, not(b)), and(b, not(a)))
	ha=lambda xor, and() (a,b, &s, &c)->{
		s=xor(a,b)
		c=and(a,b)
	}
	fa=lambda ha, or() (a, b, c0, &s, &c1)->{
		def sa,ca,cb
		call ha(a, c0, &sa, &ca)
		call ha(sa, b, &s,&cb)
		c1=or(ca,cb)
	}
	add4=lambda fa (inpA(), inpB(), &v, &out()) ->{
		dim carry(0 to 4)=0
		carry(0)=v    \\ 0 or 1  borrow
		for i=0 to 3
			\\ mm=fa(InpA(i), inpB(i), carry(i), &out(i), &carry(i+1)) ' same as this
			Call fa(InpA(i), inpB(i), carry(i), &out(i), &carry(i+1))
		next
		v=carry(4)
	}
	dim res(0 to 3)=-1,  low()
	source=lambda->{
		shift 1, -stack.size  ' reverse stack items
		=array([])  ' convert current stack to array, empty current stack
	}
	def v, k, k_low
	Print "First Example 4-bit"
	Print "A", "", 1, 0, 1, 0
	Print "B", "", 1, 0, 0, 1
	call add4(source(1,0,1,0), source(1,0,0,1), &v, &res())
	k=each(res() end to start)  ' k is an iterator, now configure to read items in reverse
	Print "A+B",v, k    ' print 1 0 0 1 1
	Print "Second Example 4-bit"
	v-=v
	Print "A", "", 0, 1, 1, 0
	Print "B", "", 0, 1, 1, 1
	call add4(source(0,1,1,0), source(0,1,1,1), &v, &res())
	k=each(res() end to start)  ' k is an iterator, now configure to read items in reverse
	Print "A+B",v, k    ' print  0 1 1 0 1
	Print "Third Example 8-bit"
	v-=v
	Print "A ", "", 1, 0, 0, 0, 0, 1, 1, 0
	Print "B ", "", 1, 1, 1, 1, 1, 1, 1, 1
	call add4(source(0,1,1,0), source(1,1,1,1), &v, &res())
	low()=res()  ' a copy of res()
	' v passed to second adder
	dim res(0 to 3)=-1
	call add4(source(1,0,0,0), source(1,1,1,1), &v, &res())
	k_low=each(low() end to start)  ' k_low is an iterator, now configure to read items in reverse
	k=each(res() end to start)  ' k is an iterator, now configure to read items in reverse
	Print "A+B",v, k, k_low   ' print  1 1 0 0 0 0 1 0 1
}
FourBitAdder

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
<lang>and[a_, b_] := Max[a, b];
or[a_, b_] := Min[a, b];
not[a_] := 1 - a;
xor[a_, b_] := or[and[a, not[b]], and[b, not[a]]];
halfadder[a_, b_] := {xor[a, b], and[a, b]};
fulladder[a_, b_, c0_] := Module[{s, c, c1},
   {s, c} = halfadder[c0, a];
   {s, c1} = halfadder[s, b];
   {s, or[c, c1]}];
fourbitadder[{a3_, a2_, a1_, a0_}, {b3_, b2_, b1_, b0_}] :=
  Module[{s0, s1, s2, s3, c0, c1, c2, c3},
   {s0, c0} = fulladder[a0, b0, 0];
   {s1, c1} = fulladder[a1, b1, c0];
   {s2, c2} = fulladder[a2, b2, c1];
   {s3, c3} = fulladder[a3, b3, c2];
   {{s3, s2, s1, s0}, c3}];
```

Example:
<lang>fourbitadder[{1, 0, 1, 0}, {1, 1, 1, 1}]
```

Output:

```txt
{{1, 0, 0, 1}, 1}
```


=={{header|MATLAB}} / {{header|Octave}}==
The four bit adder presented can work on matricies of 1's and 0's, which are stored as characters, doubles, or booleans. MATLAB has functions to convert decimal numbers to binary, but these functions convert a decimal number not to binary but a string data type of 1's and 0's. So, this four bit adder is written to be compatible with MATLAB's representation of binary. Also, because this is MATLAB, and you might want to add arrays of 4-bit binary numbers together, this implementation will add two column vectors of 4-bit binary numbers together.


```MATLAB
function [S,v] = fourBitAdder(input1,input2)

    %Make sure that only 4-Bit numbers are being added. This assumes that
    %if input1 and input2 are a vector of multiple decimal numbers, then
    %the binary form of these vectors are an n by 4 matrix.
    assert((size(input1,2) == 4) && (size(input2,2) == 4),'This will only work on 4-Bit Numbers');

    %Converts MATLAB binary strings to matricies of 1 and 0
    function mat = binStr2Mat(binStr)
        mat = zeros(size(binStr));
        for i = (1:numel(binStr))
            mat(i) = str2double(binStr(i));
        end
    end

    %XOR decleration
    function AxorB = xor(A,B)
        AxorB = or(and(A,not(B)),and(B,not(A)));
    end

    %Half-Adder decleration
    function [S,C] = halfAdder(A,B)
        S = xor(A,B);
        C = and(A,B);
    end

    %Full-Adder decleration
    function [S,Co] = fullAdder(A,B,Ci)
       [SAdder1,CAdder1] = halfAdder(Ci,A);
       [S,CAdder2] = halfAdder(SAdder1,B);
       Co = or(CAdder1,CAdder2);
    end

    %The rest of this code is the 4-bit adder

    binStrFlag = false; %A flag to determine if the original input was a binary string

    %If either of the inputs was a binary string, convert it to a matrix of
    %1's and 0's.
    if ischar(input1)
       input1 = binStr2Mat(input1);
       binStrFlag = true;
    end
    if ischar(input2)
       input2 = binStr2Mat(input2);
       binStrFlag = true;
    end

    %This does the addition
    S = zeros(size(input1));

    [S(:,4),Co] = fullAdder(input1(:,4),input2(:,4),0);
    [S(:,3),Co] = fullAdder(input1(:,3),input2(:,3),Co);
    [S(:,2),Co] = fullAdder(input1(:,2),input2(:,2),Co);
    [S(:,1),v] = fullAdder(input1(:,1),input2(:,1),Co);

    %If the original inputs were binary strings, convert the output of the
    %4-bit adder to a binary string with the same formatting as the
    %original binary strings.
    if binStrFlag
        S = num2str(S);
        v = num2str(v);
    end
end %fourBitAdder
```


Sample Usage:

```MATLAB>>
 [S,V] = fourBitAdder([0 0 0 1],[1 1 1 1])

S =

     0     0     0     0


V =

     1

>> [S,V] = fourBitAdder([0 0 0 1;0 0 1 0],[0 0 0 1;0 0 0 1])

S =

     0     0     1     0
     0     0     1     1


V =

     0
     0

>> [S,V] = fourBitAdder(dec2bin(10,4),dec2bin(1,4))

S =

1  0  1  1


V =

0

>> [S,V] = fourBitAdder(dec2bin([10 11],4),dec2bin([1 1],4))

S =

1  0  1  1
1  1  0  0


V =

0
0

>> bin2dec(S)

ans =

    11
    12
```



## MUMPS


```MUMPS
XOR(Y,Z) ;Uses logicals - i.e., 0 is false, anything else is true (1 is used if setting a value)
 QUIT (Y&'Z)!('Y&Z)
HALF(W,X)
 QUIT $$XOR(W,X)_"^"_(W&X)
FULL(U,V,CF)
 NEW F1,F2
 S F1=$$HALF(U,V)
 S F2=$$HALF($P(F1,"^",1),CF)
 QUIT $P(F2,"^",1)_"^"_($P(F1,"^",2)!($P(F2,"^",2)))
FOUR(Y,Z,C4)
 NEW S,I,T
 FOR I=4:-1:1 SET T=$$FULL($E(Y,I),$E(Z,I),C4),$E(S,I)=$P(T,"^",1),C4=$P(T,"^",2)
 K I,T
 QUIT S_"^"_C4
```

Usage:
```txt
USER>S N1="0110",N2="0010",C=0,T=$$FOUR^ADDER(N1,N2,C)

USER>W N1_" + "_N2_" + "_C_" = "_$P(T,"^")_" Carry "_$P(T,"^",2)
0110 + 0010 + 0 = 1000 Carry 0
USER>S N1="0110",N2="1110",C=0,T=$$FOUR^ADDER(N1,N2,C)

USER>W T
0100^1
USER>W N1_" + "_N2_" + "_C_" = "_$P(T,"^")_" Carry "_$P(T,"^",2)
0110 + 1110 + 0 = 0100 Carry 1
```



## MyHDL

To interpret and run this code you will need a recent copy of Python, and the MyHDL library from myhdl.org.  Both examples integrate test code, and export Verilog and VHDL for hardware synthesis.

Verbose Code - With integrated Test & Demo


```python
#!/usr/bin/env python

"""     http://rosettacode.org/wiki/Four_bit_adder
        Demonstrate theoretical four bit adder simulation
        using And, Or & Invert primitives

        2011-05-10  jc
"""

from myhdl import always_comb, ConcatSignal, delay, intbv, Signal, \
                  Simulation, toVerilog, toVHDL

from random import randrange


"""   define set of primitives
      ------------------------   """
def inverter(z, a):   # define component name & interface
   """ z <- not(a) """
   @always_comb   # define asynchronous logic
   def logic():
      z.next = not a
   return logic   # return defined logic, named 'inverter'

def and2(z, a, b):
   """ z <- a and b """
   @always_comb
   def logic():
      z.next = a and b
   return logic

def or2(z, a, b):
   """ z <- a or b """
   @always_comb
   def logic():
      z.next = a or b
   return logic


"""   build components using defined primitive set
      --------------------------------------------   """
def xor2 (z, a, b):
   """ z <- a xor b """
   # define interconnect signals
   nota, notb, annotb, bnnota = [Signal(bool(0)) for i in range(4)]
   # name sub-components, and their interconnect
   inv0 = inverter(nota,  a)
   inv1 = inverter(notb,  b)
   and2a = and2(annotb,  a, notb)
   and2b = and2(bnnota,  b, nota)
   or2a = or2(z,  annotb, bnnota)
   return inv0, inv1, and2a, and2b, or2a

def halfAdder(carry, summ,  in_a, in_b):
   """ carry,sum is the sum of in_a, in_b """
   and2a = and2(carry,  in_a, in_b)
   xor2a  =  xor2(summ,   in_a, in_b)
   return and2a, xor2a

def fullAdder(fa_c1, fa_s,  fa_c0, fa_a, fa_b):
   """ fa_c0,fa_s is the sum of fa_c0, fa_a, fa_b """
   ha1_s, ha1_c1, ha2_c1 = [Signal(bool(0)) for i in range(3)]
   halfAdder01 = halfAdder(ha1_c1, ha1_s, fa_c0, fa_a)
   halfAdder02 = halfAdder(ha2_c1, fa_s, ha1_s, fa_b)
   or2a = or2(fa_c1, ha1_c1, ha2_c1)
   return halfAdder01, halfAdder02, or2a

def Adder4b_ST(co,sum4, ina,inb):
   ''' assemble 4 full adders '''
   c = [Signal(bool()) for i in range(0,4)]
   sl = [Signal(bool()) for i in range(4)]  # sum list
   halfAdder_0 = halfAdder(c[1],sl[0],      ina(0),inb(0))
   fullAdder_1 = fullAdder(c[2],sl[1], c[1],ina(1),inb(1))
   fullAdder_2 = fullAdder(c[3],sl[2], c[2],ina(2),inb(2))
   fullAdder_3 = fullAdder(co,  sl[3], c[3],ina(3),inb(3))
   # create an internal bus for the output list
   sc = ConcatSignal(*reversed(sl))  # create internal bus for output list
   @always_comb
   def list2intbv():
      sum4.next = sc  # assign internal bus to actual output

   return halfAdder_0, fullAdder_1, fullAdder_2, fullAdder_3, list2intbv


"""   define signals and code for testing
      -----------------------------------   """
t_co, t_s, t_a, t_b, dbug = [Signal(bool(0)) for i in range(5)]
ina4, inb4, sum4 = [Signal(intbv(0)[4:])  for i in range(3)]

def test():
   print "\n      b   a   |  c1    s   \n     -------------------"
   for i in range(15):
      ina4.next, inb4.next = randrange(2**4), randrange(2**4)
      yield delay(5)
      print "     %2d  %2d   |  %2d   %2d     " \
             % (ina4,inb4, t_co,sum4)
      assert t_co * 16 + sum4 == ina4 + inb4
   print


"""   instantiate components and run test
      -----------------------------------   """
Adder4b_01 = Adder4b_ST(t_co,sum4, ina4,inb4)
test_1 = test()

def main():
   sim = Simulation(Adder4b_01, test_1)
   sim.run()
   toVHDL(Adder4b_ST, t_co,sum4, ina4,inb4)
   toVerilog(Adder4b_ST, t_co,sum4, ina4,inb4)

if __name__ == '__main__':
   main()

```



Professional Code - with test bench


```python
#!/usr/bin/env python

from myhdl import *

def Half_adder(a, b, s, c):

    @always_comb
    def logic():
        s.next = a ^ b
        c.next = a & b

    return logic


def Full_Adder(a, b, cin, s, c_out):
    s_ha1, c_ha1, c_ha2 = [Signal(bool()) for i in range(3)]
    ha1 = Half_adder(a=cin, b=a, s=s_ha1, c=c_ha1)
    ha2 = Half_adder(a=s_ha1, b=b, s=s, c=c_ha2)

    @always_comb
    def logic():
        c_out.next = c_ha1 | c_ha2

    return ha1, ha2, logic


def Multibit_Adder(a, b, s):
    N = len(s)-1
    # convert input busses to lists
    al = [a(i) for i in range(N)]
    bl = [b(i) for i in range(N)]
    # set up lists for carry and output
    cl = [Signal(bool()) for i in range(N+1)]
    sl = [Signal(bool()) for i in range(N+1)]
    # boundaries for carry and output
    cl[0] = 0
    sl[N] = cl[N]
    # create an internal bus for the output list
    sc = ConcatSignal(*reversed(sl))

    # assign internal bus to actual output
    @always_comb
    def assign():
        s.next = sc

    # create a list of adders
    add = [None] * N
    for i in range(N):
        add[i] = Full_Adder(a=al[i], b=bl[i], s=sl[i], cin=cl[i], c_out=cl[i+1])

    return add, assign


# declare I/O for a four-bit adder
N=4
a = Signal(intbv(0)[N:])
b = Signal(intbv(0)[N:])
s = Signal(intbv(0)[N+1:])

# convert to Verilog and VHDL
toVerilog(Multibit_Adder, a, b, s)
toVHDL(Multibit_Adder, a, b, s)

# set up a test bench
from random import randrange
def tb():
    dut = Multibit_Adder(a, b, s)

    @instance
    def check():
        yield delay(10)
        for i in range(100):
            p, q = randrange(2**N), randrange(2**N)
            a.next = p
            b.next = q
            yield delay(10)
            assert s == p + q

    return dut, check

# the entry point for the py.test unit test framework
def test_Adder():
    sim = Simulation(tb())
    sim.run()

```



## Nim

```nim
proc ha(a, b): auto = [a xor b, a and b] # sum, carry

proc fa(a, b, ci): auto =
  let a = ha(ci, a)
  let b = ha(a[0], b)
  [b[0], a[1] or b[1]] # sum, carry

proc fa4(a,b): array[5, bool] =
  var co,s: array[4, bool]
  for i in 0..3:
    let r = fa(a[i], b[i], if i > 0: co[i-1] else: false)
    s[i] = r[0]
    co[i] = r[1]
  result[0..3] = s
  result[4] = co[3]

proc int2bus(n): array[4, bool] =
  var n = n
  for i in 0..result.high:
    result[i] = (n and 1) == 1
    n = n shr 1

proc bus2int(b): int =
  for i,x in b:
    result += (if x: 1 else: 0) shl i

for a in 0..7:
  for b in 0..7:
    assert a + b == bus2int fa4(int2bus(a), int2bus(b))
```



## OCaml


```ocaml

(* File blocks.ml

A block is just a black box with nin input lines and nout output lines,
numbered from 0 to nin-1 and 0 to nout-1 respectively. It will be stored
in a caml record, with the operation stored as a function. A value on
a line is represented by a boolean value. *)

type block = { nin:int; nout:int; apply:bool array -> bool array };;

(* First we need function for boolean conversion to and from integer values,
mainly for pretty printing of results *)

let int_of_bits nbits v =
	if (Array.length v) <> nbits then failwith "bad args"
	else
		(let r = ref 0L in
		for i=nbits-1 downto 0 do
			r := Int64.add (Int64.shift_left !r 1) (if v.(i) then 1L else 0L)
		done;
		!r);;

let bits_of_int nbits n =
	let v = Array.make nbits false
	and r = ref n in
	for i=0 to nbits-1 do
		v.(i) <- (Int64.logand !r 1L) <> Int64.zero;
		r := Int64.shift_right_logical !r 1
	done;
	v;;

let input nbits v =
	let n = Array.length v in
	let w = Array.make (n*nbits) false in
	Array.iteri (fun i x ->
		Array.blit (bits_of_int nbits x) 0 w (i*nbits) nbits
	) v;
	w;;

let output nbits v =
	let nv = Array.length v in
	let r = nv mod nbits and n = nv/nbits in
	if r <> 0 then failwith "bad output size" else
	Array.init n (fun i ->
		int_of_bits nbits (Array.sub v (i*nbits) nbits)
	);;

(* We have a type for blocks, so we need operations on blocks.

assoc:        make one block from two blocks, side by side (they are not connected)
serial:       connect input from one block to output of another block
parallel:     make two outputs from one input passing through two blocks
block_array:  an array of blocks linked by the same connector (assoc, serial, parallel) *)

let assoc a b =
	{ nin=a.nin+b.nin; nout=a.nout+b.nout; apply=function
		bits -> Array.append
			(a.apply (Array.sub bits 0 a.nin))
			(b.apply (Array.sub bits a.nin b.nin)) };;

let serial a b =
	if a.nout <> b.nin then
		failwith "[serial] bad block"
	else
	{ nin=a.nin; nout=b.nout; apply=function
		bits -> b.apply (a.apply bits) };;

let parallel a b =
	if a.nin <> b.nin then
		failwith "[parallel] bad blocks"
	else { nin=a.nin; nout=a.nout+b.nout; apply=function
		bits -> Array.append (a.apply bits) (b.apply bits) };;

let block_array comb v =
	let n = Array.length v
	and r = ref v.(0) in
	for i=1 to n-1 do
		r := comb !r v.(i)
	done;
	!r;;

(* wires

map:     map n input lines on length(v) output lines, using the links out(k)=v(in(k))
pass:    n wires not connected (out(k) = in(k))
fork:    a wire is developed into n wires having the same value
perm:    permutation of wires
forget:  n wires going nowhere
sub:     subset of wires, other ones going nowhere *)

let map n v = { nin=n; nout=Array.length v; apply=function
	bits -> Array.map (function k -> bits.(k)) v };;

let pass n = { nin=n; nout=n; apply=function
	bits -> bits };;

let fork n = { nin=1; nout=n; apply=function
	bits -> Array.make n bits.(0) };;

let perm v =
	let n = Array.length v in
	{ nin=n; nout=n; apply=function
		bits -> Array.init n (function k -> bits.(v.(k))) };;

let forget n = { nin=n; nout=0; apply=function
	bits -> [| |] };;

let sub nin nout where = { nin=nin; nout=nout; apply=function
	bits -> Array.sub bits where nout };;

let transpose n p v =
	if n*p <> Array.length v
		then failwith "bad dim"
	else
		let w = Array.copy v in
		for i=0 to n-1 do
			for j=0 to p-1 do
				let r = i*p+j and s = j*n+i in
				w.(r) <- v.(s)
			done
		done;
		w;;

(* line mixing (a special permutation)
mix 4 2 : 0,1,2,3, 4,5,6,7 -> 0,4, 1,5, 2,6, 3,7
unmix: inverse operation *)

let mix n p = perm (transpose n p (Array.init (n*p) (function x -> x)));;

let unmix n p = perm (transpose p n (Array.init (n*p) (function x -> x)));;

(* basic blocks

dummy:   no input, no output, usually not useful
const:   n wires with constant value (true or false)
encode:  translates an Int64 into boolean values, keeping only n lower bits
bnand:   NAND gate, the basic building block for all the other basic gates (or, and, not...) *)

let dummy = { nin=0; nout=0; apply=function
	bits -> bits };;

let const b n = { nin=0; nout=n; apply=function
	bits -> Array.make n b };;

let encode nbits x = { nin=0; nout=nbits; apply=function
	bits -> bits_of_int nbits x };;

let bnand = { nin=2; nout=1; apply=function
	[| a; b |] -> [| not (a && b) |] | _ -> failwith "bad args" };;

(* block evaluation : returns the value of the output, given an input and a block. *)

let eval block nbits_in nbits_out v =
	output nbits_out (block.apply (input nbits_in v));;

(* building a 4-bit adder *)

(* first we build the usual gates *)

let bnot = serial (fork 2) bnand;;

let band = serial bnand bnot;;

(* a or b = !a nand !b *)
let bor = serial (assoc bnot bnot) bnand;;

(* line "a" -> two lines, "a" and "not a" *)
let a_not_a = parallel (pass 1) bnot;;

let bxor = block_array serial [|
	assoc a_not_a a_not_a;
	perm [| 0; 3; 1; 2 |];
	assoc band band;
	bor |];;

let half_adder = parallel bxor band;;

(* bits C0,A,B -> S,C1 *)
let full_adder = block_array serial [|
	assoc half_adder (pass 1);
	perm [| 1; 0; 2 |];
	assoc (pass 1) half_adder;
	perm [| 1; 0; 2 |];
	assoc (pass 1) bor |];;

(* 4-bit adder *)
let add4 = block_array serial [|
	mix 4 2;
	assoc half_adder (pass 6);
	assoc (assoc (pass 1) full_adder) (pass 4);
	assoc (assoc (pass 2) full_adder) (pass 2);
	assoc (pass 3) full_adder |];;

(* 4-bit adder and three supplementary lines to make a multiple of 4 (to translate back to 4-bit integers) *)
let add4_io = assoc add4 (const false 3);;

(* wrapping the 4-bit to input and output integers instead of booleans
plus a b -> (sum,carry)
*)
let plus a b =
	let v = Array.map Int64.to_int
		(eval add4_io 4 4 (Array.map Int64.of_int [| a; b |])) in
	v.(0), v.(1);;

```


Testing


```ocaml

# open Blocks;;

# plus 4 5;;
- : int * int = (9, 0)

# plus 15 1;;
- : int * int = (0, 1)

# plus 15 15;;
- : int * int = (14, 1)

# plus 0 0;;
- : int * int = (0, 0)

```


An extension : n-bit adder, for n <= 64 (n could be greater, but we use Int64 for I/O)


```ocaml

(* general adder (n bits with n <= 64) *)
let gen_adder n = block_array serial [|
	mix n 2;
	assoc half_adder (pass (2*n-2));
	block_array serial (Array.init (n-2) (function k ->
		assoc (assoc (pass (k+1)) full_adder) (pass (2*(n-k-2)))));
	assoc (pass (n-1)) full_adder |];;

let gadd_io n = assoc (gen_adder n) (const false (n-1));;

let gen_plus n a b =
	let v = Array.map Int64.to_int
		(eval (gadd_io n) n n (Array.map Int64.of_int [| a; b |])) in
	v.(0), v.(1);;

```


And a test


```ocaml

# gen_plus 7 100 100;;
- : int * int = (72, 1)
# gen_plus 8 100 100;;
- : int * int = (200, 0)

```



## PARI/GP


```parigp
xor(a,b)=(!a&b)||(a&!b);
halfadd(a,b)=[a&&b,xor(a,b)];
fulladd(a,b,c)=my(t=halfadd(a,c),s=halfadd(t[2],b));[t[1]||s[1],s[2]];
add4(a3,a2,a1,a0,b3,b2,b1,b0)={
	my(s0,s1,s2,s3);
	s0=fulladd(a0,b0,0);
	s1=fulladd(a1,b1,s0[1]);
	s2=fulladd(a2,b2,s1[1]);
	s3=fulladd(a3,b3,s2[1]);
	[s3[1],s3[2],s2[2],s1[2],s0[2]]
};
add4(0,0,0,0,0,0,0,0)
```



## Perl


```perl
sub dec2bin { sprintf "%04b", shift }
sub bin2dec { oct "0b".shift }
sub bin2bits { reverse split(//, substr(shift,0,shift)); }
sub bits2bin { join "", map { 0+$_ } reverse @_ }

sub bxor {
  my($a, $b) = @_;
  (!$a & $b) | ($a & !$b);
}

sub half_adder {
  my($a, $b) = @_;
  ( bxor($a,$b), $a & $b );
}

sub full_adder {
  my($a, $b, $c) = @_;
  my($s1, $c1) = half_adder($a, $c);
  my($s2, $c2) = half_adder($s1, $b);
  ($s2, $c1 | $c2);
}

sub four_bit_adder {
  my($a, $b) = @_;
  my @abits = bin2bits($a,4);
  my @bbits = bin2bits($b,4);

  my($s0,$c0) = full_adder($abits[0], $bbits[0], 0);
  my($s1,$c1) = full_adder($abits[1], $bbits[1], $c0);
  my($s2,$c2) = full_adder($abits[2], $bbits[2], $c1);
  my($s3,$c3) = full_adder($abits[3], $bbits[3], $c2);
  (bits2bin($s0, $s1, $s2, $s3), $c3);
}

print " A    B      A      B   C    S  sum\n";
for my $a (0 .. 15) {
  for my $b (0 .. 15) {
    my($abin, $bbin) = map { dec2bin($_) } $a,$b;
    my($s,$c) = four_bit_adder( $abin, $bbin );
    printf "%2d + %2d = %s + %s = %s %s = %2d\n",
           $a, $b, $abin, $bbin, $c, $s, bin2dec($c.$s);
  }
}
```


Output matches the [[Four bit adder#Ruby|Ruby]] output.


## Perl 6


```perl6
sub xor ($a, $b) { (($a and not $b) or (not $a and $b)) ?? 1 !! 0 }

sub half-adder ($a, $b) {
    return xor($a, $b), ($a and $b);
}

sub full-adder ($a, $b, $c0) {
    my ($ha0_s, $ha0_c) = half-adder($c0, $a);
    my ($ha1_s, $ha1_c) = half-adder($ha0_s, $b);
    return $ha1_s, ($ha0_c or $ha1_c);
}

sub four-bit-adder ($a0, $a1, $a2, $a3, $b0, $b1, $b2, $b3) {
    my ($fa0_s, $fa0_c) = full-adder($a0, $b0, 0);
    my ($fa1_s, $fa1_c) = full-adder($a1, $b1, $fa0_c);
    my ($fa2_s, $fa2_c) = full-adder($a2, $b2, $fa1_c);
    my ($fa3_s, $fa3_c) = full-adder($a3, $b3, $fa2_c);

    return $fa0_s, $fa1_s, $fa2_s, $fa3_s, $fa3_c;
}

{
    use Test;

    is four-bit-adder(1, 0, 0, 0, 1, 0, 0, 0), (0, 1, 0, 0, 0), '1 + 1 == 2';
    is four-bit-adder(1, 0, 1, 0, 1, 0, 1, 0), (0, 1, 0, 1, 0), '5 + 5 == 10';
    is four-bit-adder(1, 0, 0, 1, 1, 1, 1, 0)[4], 1, '7 + 9 == overflow';
}
```


```txt
ok 1 - 1 + 1 == 2
ok 2 - 5 + 5 == 10
ok 3 - 7 + 9 == overflow
```



## Phix


```Phix
function xor_gate(bool a, bool b)
    return (a and not b) or (not a and b)
end function

function half_adder(bool a, bool b)
    bool s = xor_gate(a,b)
    bool c = a and b
    return {s,c}
end function

function full_adder(bool a, bool b, bool c)
    bool {s1,c1} = half_adder(c,a)
    bool {s2,c2} = half_adder(s1,b)
    c = c1 or c2
    return {s2,c}
end function

function four_bit_adder(bool a0, a1, a2, a3, b0, b1, b2, b3)
bool s0,s1,s2,s3,c
    {s0,c} = full_adder(a0,b0,0)
    {s1,c} = full_adder(a1,b1,c)
    {s2,c} = full_adder(a2,b2,c)
    {s3,c} = full_adder(a3,b3,c)
    return {s3,s2,s1,s0,c}
end function

procedure test(integer a, integer b)
    bool {a0,a1,a2,a3} = int_to_bits(a,4)
    bool {b0,b1,b2,b3} = int_to_bits(b,4)
    bool {r3,r2,r1,r0,c} = four_bit_adder(a0,a1,a2,a3,b0,b1,b2,b3)
    integer r = bits_to_int({r0,r1,r2,r3})
    printf(1,"%04b + %04b = %04b %b (%d+%d=%d)\n",{a,b,r,c,a,b,c*16+r})
end procedure

test(0,0)
test(0,1)
test(0b1111,0b1111)
test(3,7)
test(11,8)
test(0b1100,0b1100)
test(0b1100,0b1101)
test(0b1100,0b1110)
test(0b1100,0b1111)
test(0b1101,0b0000)
test(0b1101,0b0001)
test(0b1101,0b0010)
test(0b1101,0b0011)
```

```txt

0000 + 0000 = 0000 0 (0+0=0)
0000 + 0001 = 0001 0 (0+1=1)
1111 + 1111 = 1110 1 (15+15=30)
0011 + 0111 = 1010 0 (3+7=10)
1011 + 1000 = 0011 1 (11+8=19)
1100 + 1100 = 1000 1 (12+12=24)
1100 + 1101 = 1001 1 (12+13=25)
1100 + 1110 = 1010 1 (12+14=26)
1100 + 1111 = 1011 1 (12+15=27)
1101 + 0000 = 1101 0 (13+0=13)
1101 + 0001 = 1110 0 (13+1=14)
1101 + 0010 = 1111 0 (13+2=15)
1101 + 0011 = 0000 1 (13+3=16)

```



## PicoLisp


```PicoLisp
(de halfAdder (A B)  #> (Carry . Sum)
   (cons
      (and A B)
      (xor A B) ) )

(de fullAdder (A B C)  #> (Carry . Sum)
   (let (Ha1 (halfAdder C A)  Ha2 (halfAdder (cdr Ha1) B))
      (cons
         (or (car Ha1) (car Ha2))
         (cdr Ha2) ) ) )

(de 4bitsAdder (A4 A3 A2 A1  B4 B3 B2 B1)  #> (V S4 S3 S2 S1)
   (let
      (Fa1 (fullAdder A1 B1)
         Fa2 (fullAdder A2 B2 (car Fa1))
         Fa3 (fullAdder A3 B3 (car Fa2))
         Fa4 (fullAdder A4 B4 (car Fa3)) )
      (list
         (car Fa4)
         (cdr Fa4)
         (cdr Fa3)
         (cdr Fa2)
         (cdr Fa1) ) ) )
```

```txt
: (4bitsAdder NIL NIL NIL T  NIL NIL NIL T)
-> (NIL NIL NIL T NIL)

: (4bitsAdder NIL T NIL NIL  NIL NIL T T)
-> (NIL NIL T T T)

: (4bitsAdder NIL T T T  NIL T T T)
-> (NIL T T T NIL)

: (4bitsAdder T T T T  NIL NIL NIL T)
-> (T NIL NIL NIL NIL)
```



## PL/I


```PL/I

/* 4-BIT ADDER */

TEST: PROCEDURE OPTIONS (MAIN);
   DECLARE CARRY_IN BIT (1) STATIC INITIAL ('0'B) ALIGNED;
   declare (m, n, sum)(4) bit(1) aligned;
   declare i fixed binary;

   get edit (m, n) (b(1));
   put edit (m, ' + ', n, ' = ') (4 b, a);

   do i = 4 to 1 by -1;
      call full_adder ((carry_in), m(i), n(i), sum(i), carry_in);
   end;
   put edit (sum) (b);

HALF_ADDER: PROCEDURE (IN1, IN2, SUM, CARRY);
   DECLARE (IN1, IN2, SUM, CARRY) BIT (1) ALIGNED;

   SUM = ( ^IN1 & IN2) | ( IN1 & ^IN2);
         /* Exclusive OR using only AND, NOT, OR. */
   CARRY = IN1 & IN2;

END HALF_ADDER;

FULL_ADDER: PROCEDURE (CARRY_IN, IN1, IN2, SUM, CARRY);
   DECLARE (CARRY_IN, IN1, IN2, SUM, CARRY) BIT (1) ALIGNED;
   DECLARE (SUM2, CARRY2) BIT (1) ALIGNED;

   CALL HALF_ADDER (CARRY_IN, IN1, SUM, CARRY);
   CALL HALF_ADDER (SUM, IN2, SUM2, CARRY2);
   SUM = SUM2;
   CARRY = CARRY | CARRY2;
END FULL_ADDER;

END TEST;

```


## PowerShell


### Using Bytes as Inputs


```Powershell
function bxor2 ( [byte] $a, [byte] $b )
{
    $out1 = $a -band ( -bnot $b )
    $out2 = ( -bnot $a ) -band $b
    $out1 -bor $out2
}

function hadder ( [byte] $a, [byte] $b )
{
    @{
        "S"=bxor2 $a $b
        "C"=$a -band $b
    }
}

function fadder ( [byte] $a, [byte] $b, [byte] $cd )
{
    $out1 = hadder $cd $a
    $out2 = hadder $out1["S"] $b
    @{
        "S"=$out2["S"]
        "C"=$out1["C"] -bor $out2["C"]
    }
}

function FourBitAdder ( [byte] $a, [byte] $b )
{
    $a0 = $a -band 1
    $a1 = ($a -band 2)/2
    $a2 = ($a -band 4)/4
    $a3 = ($a -band 8)/8
    $b0 = $b -band 1
    $b1 = ($b -band 2)/2
    $b2 = ($b -band 4)/4
    $b3 = ($b -band 8)/8
    $out1 = fadder $a0 $b0 0
    $out2 = fadder $a1 $b1 $out1["C"]
    $out3 = fadder $a2 $b2 $out2["C"]
    $out4 = fadder $a3 $b3 $out3["C"]
    @{
        "S"="{3}{2}{1}{0}" -f $out1["S"], $out2["S"], $out3["S"], $out4["S"]
        "V"=$out4["C"]
    }
}

FourBitAdder 3 5

FourBitAdder 0xA 5

FourBitAdder 0xC 0xB

[Convert]::ToByte((FourBitAdder 0xC 0xB)["S"],2)
```



### Translation of C# code

The well-written C# code on this page can be translated without any modification into a .NET type by PowerShell.

```PowerShell

$source = @'
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaCodeTasks.FourBitAdder
{
	public struct BitAdderOutput
	{
		public bool S { get; set; }
		public bool C { get; set; }
		public override string ToString ( )
		{
			return "S" + ( S ? "1" : "0" ) + "C" + ( C ? "1" : "0" );
		}
	}
	public struct Nibble
	{
		public bool _1 { get; set; }
		public bool _2 { get; set; }
		public bool _3 { get; set; }
		public bool _4 { get; set; }
		public override string ToString ( )
		{
			return ( _4 ? "1" : "0" )
				+ ( _3 ? "1" : "0" )
				+ ( _2 ? "1" : "0" )
				+ ( _1 ? "1" : "0" );
		}
	}
	public struct FourBitAdderOutput
	{
		public Nibble N { get; set; }
		public bool C { get; set; }
		public override string ToString ( )
		{
			return N.ToString ( ) + "c" + ( C ? "1" : "0" );
		}
	}

	public static class LogicGates
	{
		// Basic Gates
		public static bool Not ( bool A ) { return !A; }
		public static bool And ( bool A, bool B ) { return A && B; }
		public static bool Or ( bool A, bool B ) { return A || B; }

		// Composite Gates
		public static bool Xor ( bool A, bool B ) {	return Or ( And ( A, Not ( B ) ), ( And ( Not ( A ), B ) ) ); }
	}

	public static class ConstructiveBlocks
	{
		public static BitAdderOutput HalfAdder ( bool A, bool B )
		{
			return new BitAdderOutput ( ) { S = LogicGates.Xor ( A, B ), C = LogicGates.And ( A, B ) };
		}

		public static BitAdderOutput FullAdder ( bool A, bool B, bool CI )
		{
			BitAdderOutput HA1 = HalfAdder ( CI, A );
			BitAdderOutput HA2 = HalfAdder ( HA1.S, B );

			return new BitAdderOutput ( ) { S = HA2.S, C = LogicGates.Or ( HA1.C, HA2.C ) };
		}

		public static FourBitAdderOutput FourBitAdder ( Nibble A, Nibble B, bool CI )
		{

			BitAdderOutput FA1 = FullAdder ( A._1, B._1, CI );
			BitAdderOutput FA2 = FullAdder ( A._2, B._2, FA1.C );
			BitAdderOutput FA3 = FullAdder ( A._3, B._3, FA2.C );
			BitAdderOutput FA4 = FullAdder ( A._4, B._4, FA3.C );

			return new FourBitAdderOutput ( ) { N = new Nibble ( ) { _1 = FA1.S, _2 = FA2.S, _3 = FA3.S, _4 = FA4.S }, C = FA4.C };
		}

		public static void Test ( )
		{
			Console.WriteLine ( "Four Bit Adder" );

			for ( int i = 0; i < 256; i++ )
			{
				Nibble A = new Nibble ( ) { _1 = false, _2 = false, _3 = false, _4 = false };
				Nibble B = new Nibble ( ) { _1 = false, _2 = false, _3 = false, _4 = false };
				if ( (i & 1) == 1)
				{
					A._1 = true;
				}
				if ( ( i & 2 ) == 2 )
				{
					A._2 = true;
				}
				if ( ( i & 4 ) == 4 )
				{
					A._3 = true;
				}
				if ( ( i & 8 ) == 8 )
				{
					A._4 = true;
				}
				if ( ( i & 16 ) == 16 )
				{
					B._1 = true;
				}
				if ( ( i & 32 ) == 32)
				{
					B._2 = true;
				}
				if ( ( i & 64 ) == 64 )
				{
					B._3 = true;
				}
				if ( ( i & 128 ) == 128 )
				{
					B._4 = true;
				}

				Console.WriteLine ( "{0} + {1} = {2}", A.ToString ( ), B.ToString ( ), FourBitAdder( A, B, false ).ToString ( ) );

			}

			Console.WriteLine ( );
		}

	}
}
'@

Add-Type -TypeDefinition $source -Language CSharpVersion3

```


```PowerShell

[RosettaCodeTasks.FourBitAdder.ConstructiveBlocks]::Test()

```

```txt

Four Bit Adder
0000 + 0000 = 0000c0
0001 + 0000 = 0001c0
0010 + 0000 = 0010c0
.
.
.
1101 + 1111 = 1100c1
1110 + 1111 = 1101c1
1111 + 1111 = 1110c1

```


## Prolog

Using hi/lo symbols to represent binary.  As this is a simulation, there is no real "arithmetic" happening.

```prolog
% binary 4 bit adder chip simulation

b_not(in(hi), out(lo)) :- !.      % not(1) = 0
b_not(in(lo), out(hi)).           % not(0) = 1

b_and(in(hi,hi), out(hi)) :- !.   % and(1,1) = 1
b_and(in(_,_), out(lo)).          % and(anything else) = 0

b_or(in(hi,_), out(hi)) :- !.     % or(1,any) = 1
b_or(in(_,hi), out(hi)) :- !.     % or(any,1) = 1
b_or(in(_,_), out(lo)).           % or(anything else) = 0

b_xor(in(A,B), out(O)) :-
    b_not(in(A), out(NotA)), b_not(in(B), out(NotB)),
    b_and(in(A,NotB), out(P)), b_and(in(NotA,B), out(Q)),
    b_or(in(P,Q), out(O)).

b_half_adder(in(A,B), s(S), c(C)) :-
    b_xor(in(A,B),out(S)), b_and(in(A,B),out(C)).

b_full_adder(in(A,B,Ci), s(S), c(C1)) :-
  b_half_adder(in(Ci, A), s(S0), c(C0)),
  b_half_adder(in(S0, B), s(S), c(C)),
  b_or(in(C0,C), out(C1)).

b_4_bit_adder(in(A0,A1,A2,A3), in(B0,B1,B2,B3), out(S0,S1,S2,S3), c(V)) :-
  b_full_adder(in(A0,B0,lo), s(S0), c(C0)),
  b_full_adder(in(A1,B1,C0), s(S1), c(C1)),
  b_full_adder(in(A2,B2,C1), s(S2), c(C2)),
  b_full_adder(in(A3,B3,C2), s(S3), c(V)).

test_add(A,B,T) :-
  b_4_bit_adder(A, B, R, C),
  writef('%w + %w is %w %w  \t(%w)\n', [A,B,R,C,T]).

go :-
  test_add(in(hi,lo,lo,lo), in(hi,lo,lo,lo), '1 + 1 = 2'),
  test_add(in(lo,hi,lo,lo), in(lo,hi,lo,lo), '2 + 2 = 4'),
  test_add(in(hi,lo,hi,lo), in(hi,lo,lo,hi), '5 + 9 = 14'),
  test_add(in(hi,hi,lo,hi), in(hi,lo,lo,hi), '11 + 9 = 20'),
  test_add(in(lo,lo,lo,hi), in(lo,lo,lo,hi), '8 + 8 = 16'),
  test_add(in(hi,hi,hi,hi), in(hi,lo,lo,lo), '15 + 1 = 16').
```


```txt
?- go.
in(hi,lo,lo,lo) + in(hi,lo,lo,lo) is out(lo,hi,lo,lo) c(lo)  	(1 + 1 = 2)
in(lo,hi,lo,lo) + in(lo,hi,lo,lo) is out(lo,lo,hi,lo) c(lo)  	(2 + 2 = 4)
in(hi,lo,hi,lo) + in(hi,lo,lo,hi) is out(lo,hi,hi,hi) c(lo)  	(5 + 9 = 14)
in(hi,hi,lo,hi) + in(hi,lo,lo,hi) is out(lo,lo,hi,lo) c(hi)  	(11 + 9 = 20)
in(lo,lo,lo,hi) + in(lo,lo,lo,hi) is out(lo,lo,lo,lo) c(hi)  	(8 + 8 = 16)
in(hi,hi,hi,hi) + in(hi,lo,lo,lo) is out(lo,lo,lo,lo) c(hi)  	(15 + 1 = 16)
true.
```



## PureBasic


```PureBasic
;Because no representation for a solitary bit is present, bits are stored as bytes.
;Output values from the constructive building blocks is done using pointers (i.e. '*').

Procedure.b notGate(x)
  ProcedureReturn ~x
EndProcedure

Procedure.b xorGate(x,y)
  ProcedureReturn  (x & notGate(y)) | (notGate(x) & y)
EndProcedure

Procedure halfadder(a, b, *sum.Byte, *carry.Byte)
  *sum\b = xorGate(a, b)
  *carry\b = a & b
EndProcedure

Procedure fulladder(a, b, c0, *sum.Byte, *c1.Byte)
  Protected sum_ac.b, carry_ac.b, carry_sb.b

  halfadder(c0, a, @sum_ac, @carry_ac)
  halfadder(sum_ac, b, *sum, @carry_sb)
  *c1\b = carry_ac | carry_sb
EndProcedure

Procedure fourbitsadder(a0, a1, a2, a3, b0, b1, b2, b3 , *s0.Byte, *s1.Byte, *s2.Byte, *s3.Byte, *v.Byte)
  Protected.b c1, c2, c3

  fulladder(a0, b0, 0,   *s0, @c1)
  fulladder(a1, b1, c1,  *s1, @c2)
  fulladder(a2, b2, c2,  *s2, @c3)
  fulladder(a3, b3, c3,  *s3, *v)
EndProcedure

;// Test implementation, map two 4-character strings to the inputs of the fourbitsadder() and display results
Procedure.s test_4_bit_adder(a.s,b.s)
  Protected.b s0, s1, s2, s3,  v, i
  Dim a.b(3)
  Dim b.b(3)
  For i = 0 To 3
    a(i) = Val(Mid(a, 4 - i, 1))
    b(i) = Val(Mid(b, 4 - i, 1))
  Next

  fourbitsadder(a(0), a(1), a(2), a(3), b(0), b(1), b(2), b(3), @s0, @s1, @s2, @s3, @v)
  ProcedureReturn a + " + " + b + " = " + Str(s3) + Str(s2) + Str(s1) + Str(s0) + " overflow " + Str(v)
EndProcedure

If OpenConsole()
  PrintN(test_4_bit_adder("0110","1110"))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
0110 + 1110 = 0100 overflow 1
```



## Python

Individual boolean bits are represented by either 1, 0, True (interchangeable with 1),
and False (same as zero). a bit of value None is sometimes used as a place-holder.

Python functions represent the building blocks of the circuit: a function parameter for each block input and individual block outputs are either a return of a single value
or a return of a tuple of values - one for each block output.
A single element tuple is ''not'' returned for a block with one output.

Python lists are used to represent bus's of multiple bits, and in this circuit,
bit zero - the least significant bit of bus's, is at index zero of the list,
(which will be printed as the ''left-most'' member of a list).

The repetitive connections of the full adder block, fa4, are achieved by using a for loop which could easily be modified to generate adders of any width. fa4's arguments are interpreted as indexable, ordered collections of values - usually lists but tuples would work too. fa4's outputs are the sum, s, as a list and the single bit carry.

Functions are provided to convert between integers and bus's and back;
and the test routine shows how they can be used to translate
between the normal Python values and those of the simulation.


```python
def xor(a, b): return (a and not b) or (b and not a)

def ha(a, b): return xor(a, b), a and b     # sum, carry

def fa(a, b, ci):
    s0, c0 = ha(ci, a)
    s1, c1 = ha(s0, b)
    return s1, c0 or c1     # sum, carry

def fa4(a, b):
    width = 4
    ci = [None] * width
    co = [None] * width
    s  = [None] * width
    for i in range(width):
        s[i], co[i] = fa(a[i], b[i], co[i-1] if i else 0)
    return s, co[-1]

def int2bus(n, width=4):
    return [int(c) for c in "{0:0{1}b}".format(n, width)[::-1]]

def bus2int(b):
    return sum(1 << i for i, bit in enumerate(b) if bit)

def test_fa4():
    width = 4
    tot = [None] * (width + 1)
    for a in range(2**width):
        for b in range(2**width):
            tot[:width], tot[width] = fa4(int2bus(a), int2bus(b))
            assert a + b == bus2int(tot), "totals don't match: %i + %i != %s" % (a, b, tot)


if __name__ == '__main__':
   test_fa4()
```



## Racket


```Racket
#lang racket

(define (adder-and a b)
  (if (= 2 (+ a b)) 1 0))    ; Defining the basic and function

(define (adder-not a)
  (if (zero? a) 1 0))        ; Defining the basic not function

(define (adder-or a b)
  (if (> (+ a b) 0) 1 0))    ; Defining the basic or function

(define (adder-xor a b)
  (adder-or
   (adder-and
    (adder-not a)
    b)
   (adder-and
    a
    (adder-not b))))         ; Defines the xor function based on the basic functions

(define (half-adder a b)
  (list (adder-xor a b) (adder-and a b))) ; Creates the half adder, returning '(sum carry)

(define (adder a b c0)
  (define half-a (half-adder c0 a))
  (define half-b (half-adder (car half-a) b))
  (list
   (car half-b)
   (adder-or (cadr half-a) (cadr half-b))))  ; Creates the full adder, returns '(sum carry)

(define (n-bit-adder 4a 4b)   ; Creates the n-bit adder, it receives 2 lists of same length
  (let-values                 ; Lists of the form '([01]+)
      (((4s v)                ; for/fold form will return 2 values, receiving this here
        (for/fold ((S null) (c 0)) ;initializes the full sum and carry
          ((a (in-list (reverse 4a))) (b (in-list (reverse 4b))))
          ;here it prepares variables for summing each element, starting from the least important bits
          (define added
            (adder a b c))
          (values
           (cons (car added) S) ; changes S and c to it's new values in the next iteration
           (cadr added)))))
    (if (zero? v)
        4s
        (cons v 4s))))

(n-bit-adder '(1 0 1 0) '(0 1 1 1)) ;-> '(1 0 0 0 1)
```



## REXX

Programming note:   REXX subroutines/functions are call by ''value'', not call by ''name'', so REXX has to '''expose''' a variable to make it global.

REXX programming syntax:
::::* the   ''' &amp;&amp;'''   symbol is an e'''X'''clusive '''OR''' function ('''XOR''').
::::* the     '''|'''       symbol is a logical '''OR'''.
::::* the     '''&amp;'''     symbol is a logical '''AND'''.

```rexx
/*REXX program displays (all) the  sums  of a  full  4─bit adder  (with carry).         */
call hdr1;  call hdr2                            /*note the order of headers & trailers.*/
                                                 /* [↓]  traipse thru all possibilities.*/
   do    j=0  for 16
                            do m=0  for 4;   a.m=bit(j, m);   end  /*m*/
      do k=0  for 16
                            do m=0  for 4;   b.m=bit(k, m);   end  /*m*/
      sc=4bitAdder(a., b.)
      z=a.3 a.2 a.1 a.0   '_+_'   b.3 b.2 b.1 b.0    "_=_"    sc    ','    s.3 s.2 s.1 s.0
      say translate(space(z, 0), , '_')          /*remove all the underbars (_) from Z. */
      end   /*k*/
   end      /*j*/

call hdr2;  call hdr1                            /*display two trailers (note the order)*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bit:       procedure;  parse arg x,y;    return  substr( reverse( x2b( d2x(x) ) ), y+1, 1)
halfAdder: procedure expose c;   parse arg x,y;      c=x & y;     return x && y
hdr1:      say 'aaaa + bbbb = c, sum     [c=carry]';              return
hdr2:      say '════   ════   ══════'              ;              return
/*──────────────────────────────────────────────────────────────────────────────────────*/
fullAdder: procedure expose c;   parse arg x,y,fc
           _1=halfAdder(fc, x);        c1=c
           _2=halfAdder(_1, y);        c=c | c1;                  return _2
/*──────────────────────────────────────────────────────────────────────────────────────*/
4bitAdder: procedure expose s. a. b.;  carry.=0
                                do j=0  for 4;                n=j-1
                                s.j=fullAdder(a.j, b.j, carry.n);      carry.j=c
                                end   /*j*/
           return c
```

'''output'''   (most lines have been elided):
<pre style="height:63ex">
aaaa + bbbb = c, sum     [c=carry]
════   ════   ══════
0000 + 0000 = 0,0000
0000 + 0001 = 0,0001
0000 + 0010 = 0,0010
0000 + 0011 = 0,0011
0000 + 0100 = 0,0100
0000 + 0101 = 0,0101
0000 + 0110 = 0,0110
0000 + 0111 = 0,0111
0000 + 1000 = 0,1000
0000 + 1001 = 0,1001
 ∙
 ∙
 ∙
0101 + 0100 = 0,1001
0101 + 0101 = 0,1010
0101 + 0110 = 0,1011
0101 + 0111 = 0,1100
0101 + 1000 = 0,1101
0101 + 1001 = 0,1110
0101 + 1010 = 0,1111
0101 + 1011 = 1,0000
0101 + 1100 = 1,0001
0101 + 1101 = 1,0010
0101 + 1110 = 1,0011
0101 + 1111 = 1,0100
0110 + 0000 = 0,0110
0110 + 0001 = 0,0111
0110 + 0010 = 0,1000
0110 + 0011 = 0,1001
0110 + 0100 = 0,1010
0110 + 0101 = 0,1011
0110 + 0110 = 0,1100
0110 + 0111 = 0,1101
0110 + 1000 = 0,1110
0110 + 1001 = 0,1111
0110 + 1010 = 1,0000
0110 + 1011 = 1,0001
0110 + 1100 = 1,0010
0110 + 1101 = 1,0011
 ∙
 ∙
 ∙
1110 + 1110 = 1,1100
1110 + 1111 = 1,1101
1111 + 0000 = 0,1111
1111 + 0001 = 1,0000
1111 + 0010 = 1,0001
1111 + 0011 = 1,0010
1111 + 0100 = 1,0011
1111 + 0101 = 1,0100
1111 + 0110 = 1,0101
1111 + 0111 = 1,0110
1111 + 1000 = 1,0111
1111 + 1001 = 1,1000
1111 + 1010 = 1,1001
1111 + 1011 = 1,1010
1111 + 1100 = 1,1011
1111 + 1101 = 1,1100
1111 + 1110 = 1,1101
1111 + 1111 = 1,1110
════   ════   ══════
aaaa + bbbb = c, sum     [c=carry]

```



## Ring


```ring


###---------------------------
# Program: 4 Bit Adder - Ring
# Author:  Bert Mariani
# Date:    2018-02-28
#
# Bit Adder: Input  A B Cin
#            Output S   Cout
#
# A ^ B => axb                XOR gate
#          axb ^ C => Sout    XOR gate
#          axb & C => d       AND gate
#
# A & B => anb                AND gate
#          anb | d => Cout     OR gate
#
# Call Adder for number of bit in input fields
###-------------------------------------------
### 4 Bits

Cout     = "0"
OutputS  = "0000"
InputA   = "0101"
InputB   = "1101"

   See "InputA:.. "+ InputA +nl
   See "InputB:.. "+ InputB +nl
BitsAdd(InputA, InputB)
   See "Sum...: "+ Cout +" "+ OutputS +nl+nl

###-------------------------------------------
### 32 Bits

Cout     = "0"
OutputS  = "00000000000000000000000000000000"
InputA   = "01010101010101010101010101010101"
InputB   = "11011101110111011101110111011101"

   See "InputA:.. "+ InputA +nl
   See "InputB:.. "+ InputB +nl
BitsAdd(InputA, InputB)
   See "Sum...: "+ Cout +" "+ OutputS +nl+nl

###-------------------------------

Func BitsAdd(InputA, InputB)
	nbrBits = len(InputA)

	for i = nbrBits to 1 step -1
	      A = InputA[i]
	      B = InputB[i]
	      C = Cout

              S = Adder(A,B,C)
	      OutputS[i] = "" + S
	next
return

###------------------------
Func Adder(A,B,C)

    axb  =   A ^ B
    Sout = axb ^ C
    d    = axb & C

    anb  =   A & B
    Cout = anb | d    ### Cout is global

return(Sout)
###------------------------


```

Output:

```txt


InputA:.. 0101
InputB:.. 1101
Sum...: 1 0010

InputA:.. 01010101010101010101010101010101
InputB:.. 11011101110111011101110111011101
Sum...: 1 00110011001100110011001100110010


```



## Ruby


```ruby
# returns pair [sum, carry]
def four_bit_adder(a, b)
  a_bits = binary_string_to_bits(a,4)
  b_bits = binary_string_to_bits(b,4)

  s0, c0 = full_adder(a_bits[0], b_bits[0],  0)
  s1, c1 = full_adder(a_bits[1], b_bits[1], c0)
  s2, c2 = full_adder(a_bits[2], b_bits[2], c1)
  s3, c3 = full_adder(a_bits[3], b_bits[3], c2)

  [bits_to_binary_string([s0, s1, s2, s3]), c3.to_s]
end

# returns pair [sum, carry]
def full_adder(a, b, c0)
  s, c = half_adder(c0, a)
  s, c1 = half_adder(s, b)
  [s, _or(c,c1)]
end

# returns pair [sum, carry]
def half_adder(a, b)
  [xor(a, b), _and(a,b)]
end

def xor(a, b)
  _or(_and(a, _not(b)), _and(_not(a), b))
end

# "and", "or" and "not" are Ruby keywords
def _and(a, b)  a & b  end
def _or(a, b)   a | b  end
def _not(a)    ~a & 1  end

def int_to_binary_string(n, length)
  "%0#{length}b" % n
end

def binary_string_to_bits(s, length)
  ("%#{length}s" % s).reverse.chars.map(&:to_i)
end

def bits_to_binary_string(bits)
  bits.map(&:to_s).reverse.join
end

puts " A    B      A      B   C    S  sum"
0.upto(15) do |a|
  0.upto(15) do |b|
    bin_a = int_to_binary_string(a, 4)
    bin_b = int_to_binary_string(b, 4)
    sum, carry = four_bit_adder(bin_a, bin_b)
    puts "%2d + %2d = %s + %s = %s %s = %2d" %
         [a, b, bin_a, bin_b, carry, sum, (carry + sum).to_i(2)]
  end
end
```


```txt

 A    B      A      B   C    S  sum
 0 +  0 = 0000 + 0000 = 0 0000 =  0
 0 +  1 = 0000 + 0001 = 0 0001 =  1
 0 +  2 = 0000 + 0010 = 0 0010 =  2
 0 +  3 = 0000 + 0011 = 0 0011 =  3
 0 +  4 = 0000 + 0100 = 0 0100 =  4
...
 7 + 13 = 0111 + 1101 = 1 0100 = 20
 7 + 14 = 0111 + 1110 = 1 0101 = 21
 7 + 15 = 0111 + 1111 = 1 0110 = 22
 8 +  0 = 1000 + 0000 = 0 1000 =  8
 8 +  1 = 1000 + 0001 = 0 1001 =  9
 8 +  2 = 1000 + 0010 = 0 1010 = 10
...
15 + 12 = 1111 + 1100 = 1 1011 = 27
15 + 13 = 1111 + 1101 = 1 1100 = 28
15 + 14 = 1111 + 1110 = 1 1101 = 29
15 + 15 = 1111 + 1111 = 1 1110 = 30
```



## Rust


```rust

// half adder with XOR and AND
// SUM = A XOR B
// CARRY = A.B
fn half_adder(a: usize, b: usize) -> (usize, usize) {
    return (a ^ b, a & b);
}

// full adder as a combination of half adders
// SUM = A XOR B XOR C
// CARRY = A.B + B.C + C.A
fn full_adder(a: usize, b: usize, c_in: usize) -> (usize, usize) {
    let (s0, c0) = half_adder(a, b);
    let (s1, c1) = half_adder(s0, c_in);
    return (s1, c0 | c1);
}

// A = (A3, A2, A1, A0)
// B = (B3, B2, B1, B0)
// S = (S3, S2, S1, S0)
fn four_bit_adder (
    a: (usize, usize, usize, usize),
    b: (usize, usize, usize, usize)
)
    ->
    // 4 bit output, carry is ignored
    (usize, usize, usize, usize)
{
    // lets have a.0 refer to the rightmost element
    let a = a.reverse();
    let b = b.reverse();

    // i would prefer a loop but that would abstract
    // the "connections of the constructive blocks"
    let (sum, carry) = half_adder(a.0, b.0);
    let out0 = sum;
    let (sum, carry) = full_adder(a.1, b.1, carry);
    let out1 = sum;
    let (sum, carry) = full_adder(a.2, b.2, carry);
    let out2 = sum;
    let (sum, _) = full_adder(a.3, b.3, carry);
    let out3 = sum;
    return (out3, out2, out1, out0);
}

fn main() {
    let a: (usize, usize, usize, usize) = (0, 1, 1, 0);
    let b: (usize, usize, usize, usize) = (0, 1, 1, 0);
    assert_eq!(four_bit_adder(a, b), (1, 1, 0, 0));
    // 0110 + 0110 = 1100
    // 6 + 6 = 12
}

// misc. traits to make our life easier
trait Reverse<A, B, C, D> {
    fn reverse(self) -> (D, C, B, A);
}

// reverse a generic tuple of arity 4
impl<A, B, C, D> Reverse<A, B, C, D> for (A, B, C, D) {
    fn reverse(self) -> (D, C, B, A){
        return (self.3, self.2, self.1, self.0)
    }
}


```


## Sather


```sather
-- a "pin" can be connected only to one component
-- that "sets" it to 0 or 1, while it can be "read"
-- ad libitum. (Tristate logic is not taken into account)
-- This class does the proper checking, assuring the "circuit"
-- and the connections are described correctly. Currently can make
-- hard the implementation of a latch
class PIN is
  private attr v:INT;
  readonly attr name:STR;
  private attr connected:BOOL;

  create(n:STR):SAME is -- n = conventional name for this "pin"
    res ::= new;
    res.name := n;
    res.connected := false;
    return res;
  end;

  val:INT is
    if self.connected.not then
       #ERR + "pin " + self.name + " is undefined\n";
       return 0; -- could return a random bit to "simulate" undefined
                 -- behaviour
    else
       return self.v;
    end;
  end;

  -- connect ...
  val(v:INT) is
    if self.connected then
       #ERR + "pin " + self.name + " is already 'assigned'\n";
    else
       self.connected := true;
       self.v := v.band(1);
    end;
  end;

  -- connect to existing pin
  val(v:PIN) is
     self.val(v.val);
  end;
end;

-- XOR "block"
class XOR is
  readonly attr xor :PIN;

  create(a, b:PIN):SAME is
    res ::= new;
    res.xor := #PIN("xor output");
    l   ::= a.val.bnot.band(1).band(b.val);
    r   ::= a.val.band(b.val.bnot.band(1));
    res.xor.val := r.bor(l);
    return res;
  end;
end;

-- HALF ADDER "block"
class HALFADDER is
  readonly attr s, c:PIN;

  create(a, b:PIN):SAME is
    res ::= new;
    res.s := #PIN("halfadder sum output");
    res.c := #PIN("halfadder carry output");
    res.s.val := #XOR(a, b).xor.val;
    res.c.val := a.val.band(b.val);
    return res;
  end;
end;

-- FULL ADDER "block"
class FULLADDER is
  readonly attr s, c:PIN;

  create(a, b, ic:PIN):SAME is
    res ::= new;
    res.s := #PIN("fulladder sum output");
    res.c := #PIN("fulladder carry output");
    halfadder1 ::= #HALFADDER(a, b);
    halfadder2 ::= #HALFADDER(halfadder1.s, ic);
    res.s.val := halfadder2.s;
    res.c.val := halfadder2.c.val.bor(halfadder1.c.val);
    return res;
  end;
end;

-- FOUR BITS ADDER "block"
class FOURBITSADDER is
  readonly attr s0, s1, s2, s3, v :PIN;

  create(a0, a1, a2, a3, b0, b1, b2, b3:PIN):SAME is
    res ::= new;
    res.s0  := #PIN("4-bits-adder sum outbut line 0");
    res.s1  := #PIN("4-bits-adder sum outbut line 1");
    res.s2  := #PIN("4-bits-adder sum outbut line 2");
    res.s3  := #PIN("4-bits-adder sum outbut line 3");
    res.v   := #PIN("4-bits-adder overflow output");
    zero ::= #PIN("zero/mass pin");
    zero.val := 0;
    fa0 ::= #FULLADDER(a0, b0, zero);
    fa1 ::= #FULLADDER(a1, b1, fa0.c);
    fa2 ::= #FULLADDER(a2, b2, fa1.c);
    fa3 ::= #FULLADDER(a3, b3, fa2.c);
    res.v.val  := fa3.c;
    res.s0.val := fa0.s;
    res.s1.val := fa1.s;
    res.s2.val := fa2.s;
    res.s3.val := fa3.s;
    return res;
  end;
end;

-- testing --

class MAIN is
  main is
    a0 ::= #PIN("a0 in"); b0 ::= #PIN("b0 in");
    a1 ::= #PIN("a1 in"); b1 ::= #PIN("b1 in");
    a2 ::= #PIN("a2 in"); b2 ::= #PIN("b2 in");
    a3 ::= #PIN("a3 in"); b3 ::= #PIN("b3 in");
    ov ::= #PIN("overflow");

    a0.val := 1; b0.val := 1;
    a1.val := 1; b1.val := 1;
    a2.val := 0; b2.val := 0;
    a3.val := 0; b3.val := 1;

    fba ::= #FOURBITSADDER(a0,a1,a2,a3,b0,b1,b2,b3);
    #OUT + #FMT("%d%d%d%d", a3.val, a2.val, a1.val, a0.val) +
    	   " + " +
           #FMT("%d%d%d%d", b3.val, b2.val, b1.val, b0.val) +
           " = " +
           #FMT("%d%d%d%d", fba.s3.val, fba.s2.val, fba.s1.val, fba.s0.val) +
           ", overflow = " + fba.v.val + "\n";
  end;
end;
```



## Sed

This is full adder that means it takes arbitrary number of bits (think of it as infinite stack of 2 bit adders, which is btw how it's internally made).
I took it from https://github.com/emsi/SedScripts

```sed

#!/bin/sed -f
# (C) 2005,2014 by Mariusz Woloszyn :)
# https://en.wikipedia.org/wiki/Adder_(electronics)

##############################
# PURE SED BINARY FULL ADDER #
##############################


# Input two lines, sanitize input
N
s/ //g
/^[01	 ]\+\n[01	 ]\+$/! {
	i\
	ERROR: WRONG INPUT DATA
	d
	q
}
s/[ 	]//g

# Add place for Sum and Cary bit
s/$/\n\n0/

:LOOP
# Pick A,B and C bits and put that to hold
s/^\(.*\)\(.\)\n\(.*\)\(.\)\n\(.*\)\n\(.\)$/0\1\n0\3\n\5\n\6\2\4/
h

# Grab just A,B,C
s/^.*\n.*\n.*\n\(...\)$/\1/

# binary full adder module
# INPUT:  3bits (A,B,Carry in), for example 101
# OUTPUT: 2bits (Carry, Sum), for wxample   10
s/$/;000=00001=01010=01011=10100=01101=10110=10111=11/
s/^\(...\)[^;]*;[^;]*\1=\(..\).*/\2/

# Append the sum to hold
H

# Rewrite the output, append the sum bit to final sum
g
s/^\(.*\)\n\(.*\)\n\(.*\)\n...\n\(.\)\(.\)$/\1\n\2\n\5\3\n\4/

# Output result and exit if no more bits to process..
/^\([0]*\)\n\([0]*\)\n/ {
	s/^.*\n.*\n\(.*\)\n\(.\)/\2\1/
	s/^0\(.*\)/\1/
	q
}

b LOOP
```


Example usage:


```sed

./binAdder.sed
1111110111
1
1111111000

./binAdder.sed
10
10001
10011

./binAdder.sed
0 1 1 0
0 0 0 1
111

```



## Scala


```scala
object FourBitAdder {
   type Nibble=(Boolean, Boolean, Boolean, Boolean)

   def xor(a:Boolean, b:Boolean)=(!a)&&b || a&&(!b)

   def halfAdder(a:Boolean, b:Boolean)={
      val s=xor(a,b)
      val c=a && b
      (s, c)
   }

   def fullAdder(a:Boolean, b:Boolean, cIn:Boolean)={
      val (s1, c1)=halfAdder(a, cIn)
      val (s, c2)=halfAdder(s1, b)
      val cOut=c1 || c2
      (s, cOut)
   }

   def fourBitAdder(a:Nibble, b:Nibble)={
      val (s0, c0)=fullAdder(a._4, b._4, false)
      val (s1, c1)=fullAdder(a._3, b._3, c0)
      val (s2, c2)=fullAdder(a._2, b._2, c1)
      val (s3, cOut)=fullAdder(a._1, b._1, c2)
      ((s3, s2, s1, s0), cOut)
   }
}
```


A test program using the object above.

```scala
object FourBitAdderTest {
   import FourBitAdder._
   def main(args: Array[String]): Unit = {
      println("%4s   %4s   %4s %2s".format("A","B","S","C"))
      for(a <- 0 to 15; b <- 0 to 15){
         val (s, cOut)=fourBitAdder(a,b)
         println("%4s + %4s = %4s %2d".format(nibbleToString(a),nibbleToString(b),nibbleToString(s),cOut.toInt))
      }
   }

   implicit def toInt(b:Boolean):Int=if (b) 1 else 0
   implicit def intToBool(i:Int):Boolean=if (i==0) false else true
   implicit def intToNibble(i:Int):Nibble=((i>>>3)&1, (i>>>2)&1, (i>>>1)&1, i&1)
   def nibbleToString(n:Nibble):String="%d%d%d%d".format(n._1.toInt, n._2.toInt, n._3.toInt, n._4.toInt)
}
```

```txt
   A      B      S  C
0000 + 0000 = 0000  0
0000 + 0001 = 0001  0
0000 + 0010 = 0010  0
0000 + 0011 = 0011  0
0000 + 0100 = 0100  0
...
1111 + 1011 = 1010  1
1111 + 1100 = 1011  1
1111 + 1101 = 1100  1
1111 + 1110 = 1101  1
1111 + 1111 = 1110  1
```



## Scheme

```scheme

(import (scheme base)
        (scheme write)
        (srfi 60))      ;; for logical bits

;; Returns a list of bits: '(sum carry)
(define (half-adder a b)
  (list (bitwise-xor a b) (bitwise-and a b)))

;; Returns a list of bits: '(sum carry)
(define (full-adder a b c-in)
  (let* ((h1 (half-adder c-in a))
         (h2 (half-adder (car h1) b)))
    (list (car h2) (bitwise-ior (cadr h1) (cadr h2)))))

;; a and b are lists of 4 bits each
(define (four-bit-adder a b)
  (let* ((add-1 (full-adder (list-ref a 3) (list-ref b 3) 0))
         (add-2 (full-adder (list-ref a 2) (list-ref b 2) (list-ref add-1 1)))
         (add-3 (full-adder (list-ref a 1) (list-ref b 1) (list-ref add-2 1)))
         (add-4 (full-adder (list-ref a 0) (list-ref b 0) (list-ref add-3 1))))
    (list (list (car add-4) (car add-3) (car add-2) (car add-1))
          (cadr add-4))))

(define (show-eg a b)
  (display a) (display " + ") (display b) (display " = ")
  (display (four-bit-adder a b)) (newline))

(show-eg (list 0 0 0 0) (list 0 0 0 0))
(show-eg (list 0 0 0 0) (list 1 1 1 1))
(show-eg (list 1 1 1 1) (list 0 0 0 0))
(show-eg (list 0 1 0 1) (list 1 1 0 0))
(show-eg (list 1 1 1 1) (list 1 1 1 1))
(show-eg (list 1 0 1 0) (list 0 1 0 1))

```


```txt

(0 0 0 0) + (0 0 0 0) = ((0 0 0 0) 0)
(0 0 0 0) + (1 1 1 1) = ((1 1 1 1) 0)
(1 1 1 1) + (0 0 0 0) = ((1 1 1 1) 0)
(0 1 0 1) + (1 1 0 0) = ((0 0 0 1) 1)
(1 1 1 1) + (1 1 1 1) = ((1 1 1 0) 1)
(1 0 1 0) + (0 1 0 1) = ((1 1 1 1) 0)

```



## Sidef

```ruby
func bxor(a, b) {
  (~a & b) | (a & ~b)
}

func half_adder(a, b) {
  return (bxor(a, b), a & b)
}

func full_adder(a, b, c) {
  var (s1, c1) = half_adder(a, c)
  var (s2, c2) = half_adder(s1, b)
  return (s2, c1 | c2)
}

func four_bit_adder(a, b) {
  var (s0, c0) = full_adder(a[0], b[0], 0)
  var (s1, c1) = full_adder(a[1], b[1], c0)
  var (s2, c2) = full_adder(a[2], b[2], c1)
  var (s3, c3) = full_adder(a[3], b[3], c2)
  return ([s3,s2,s1,s0].join, c3.to_s)
}

say " A    B      A      B   C    S  sum"
for a in ^16 {
  for b in ^16 {
    var(abin, bbin) = [a,b].map{|n| "%04b"%n->chars.reverse.map{.to_i} }...
    var(s, c) = four_bit_adder(abin, bbin)
    printf("%2d + %2d = %s + %s = %s %s = %2d\n",
        a, b, abin.join, bbin.join, c, s, "#{c}#{s}".bin)
    }
}
```


```txt

 A    B      A      B   C    S  sum
 0 +  0 = 0000 + 0000 = 0 0000 =  0
 0 +  1 = 0000 + 0001 = 0 0001 =  1
 0 +  2 = 0000 + 0010 = 0 0010 =  2
 0 +  3 = 0000 + 0011 = 0 0011 =  3
 0 +  4 = 0000 + 0100 = 0 0100 =  4
...
 7 + 13 = 0111 + 1101 = 1 0100 = 20
 7 + 14 = 0111 + 1110 = 1 0101 = 21
 7 + 15 = 0111 + 1111 = 1 0110 = 22
 8 +  0 = 1000 + 0000 = 0 1000 =  8
 8 +  1 = 1000 + 0001 = 0 1001 =  9
 8 +  2 = 1000 + 0010 = 0 1010 = 10
...
15 + 12 = 1111 + 1100 = 1 1011 = 27
15 + 13 = 1111 + 1101 = 1 1100 = 28
15 + 14 = 1111 + 1110 = 1 1101 = 29
15 + 15 = 1111 + 1111 = 1 1110 = 30

```



## SystemVerilog

In SystemVerilog we can define a multibit adder as a parameterized module, that instantiates the components:

```SystemVerilog

module Half_Adder( input a, b, output s, c );
  assign s = a ^ b;
  assign c = a & b;
endmodule

module Full_Adder( input a, b, c_in, output s, c_out );

  wire s_ha1, c_ha1, c_ha2;

  Half_Adder ha1( .a(c_in), .b(a), .s(s_ha1), .c(c_ha1) );
  Half_Adder ha2( .a(s_ha1), .b(b), .s(s), .c(c_ha2) );
  assign c_out = c_ha1 | c_ha2;

endmodule


module Multibit_Adder(a,b,s);
  parameter N = 8;
  input [N-1:0] a;
  input [N-1:0] b;
  output [N:0] s;

  wire [N:0] c;

  assign c[0] = 0;
  assign s[N] = c[N];

  generate
    genvar I;
    for (I=0; I<N; ++I) Full_Adder add( .a(a[I]), .b(b[I]), .s(s[I]), .c_in(c[I]), .c_out(c[I+1]) );
  endgenerate

endmodule

```


And then a testbench to test it -- here I use random stimulus with an assertion (it's aften good to separate the stimulus generation from the results-checking):


```SystemVerilog

module simTop();

  bit [3:0] a;
  bit [3:0] b;
  bit [4:0] s;

  Multibit_Adder#(4) adder(.*);

  always_comb begin
    $display( "%d + %d = %d", a, b, s );
    assert( s == a+b );
  end

endmodule

program Main();

  class Test;
    rand bit [3:0] a;
    rand bit [3:0] b;
  endclass

  Test t = new;
  initial repeat (20) begin
    #10 t.randomize;
    simTop.a = t.a;
    simTop.b = t.b;
  end

endprogram


```


```txt

 0 +  0 =  0
 7 +  0 =  7
11 +  3 = 14
 9 + 15 = 24
 7 +  3 = 10
 1 +  4 =  5
 9 +  7 = 16
10 +  6 = 16
15 +  9 = 24
 9 +  3 = 12
 2 +  3 =  5
14 +  5 = 19
 1 +  8 =  9
 0 +  4 =  4
13 +  9 = 22
15 +  7 = 22
 3 + 15 = 18
 7 +  4 = 11
13 +  4 = 17
10 +  7 = 17
 1 +  2 =  3
$finish at simulation time                  200

```


A quick note on the use of random stimulus. You might think that, with an input space of only 2**8 (256) distinct inputs, that exhaustive testing (i.e. just loop through all the possible inputs) would be appropriate. In this case you might be right. But as a HW verification engineer I'm used to dealing with coverage spaces closer to 10**80 (every state element -- bit of memory) increases the space). It's not practical to verify such hardware exhaustively -- indeed, it's hard to know where the interesting cases are -- so we use constrained random verification. If you want to, to can work thought the statistics to figure out the probability that we missed a bug when sampling 20 cases from a space of 2**8 -- it's quite scary when you realize that every complex digital chip that you ever bought (cpu, gpu, networking, etc.) was 0% verified (zero to at least 50 decimal places).

For a problem this small, however, we'd probably just whip out a "formal" tool and statically prove that the assertion can never fire for all possible sets of inputs.


## Tcl

This example shows how you can make little languages in Tcl that describe the problem space.

```tcl
package require Tcl 8.5

# Create our little language
proc pins args {
    # Just declaration...
    foreach p $args {upvar 1 $p v}
}
proc gate {name pins body} {
    foreach p $pins {
	lappend args _$p
	append v " \$_$p $p"
    }
    proc $name $args "upvar 1 $v;$body"
}

# Fundamental gates; these are the only ones that use Tcl math ops
gate not {x out}   {
    set out [expr {1 & ~$x}]
}
gate and {x y out} {
    set out [expr {$x & $y}]
}
gate or  {x y out} {
    set out [expr {$x | $y}]
}
gate GND pin {
    set pin 0
}

# Composite gates: XOR
gate xor {x y out} {
    pins nx ny x_ny nx_y

    not x          nx
    not y          ny
    and x ny       x_ny
    and nx y       nx_y
    or  x_ny nx_y  out
}

# Composite gates: half adder
gate halfadd {a b sum carry} {
    xor a b  sum
    and a b  carry
}

# Composite gates: full adder
gate fulladd {a b c0 sum c1} {
    pins sum_ac carry_ac carry_sb

    halfadd c0 a          sum_ac carry_ac
    halfadd sum_ac b      sum carry_sb
    or carry_ac carry_sb  c1
}

# Composite gates: 4-bit adder
gate 4add {a0 a1 a2 a3  b0 b1 b2 b3  s0 s1 s2 s3  v} {
    pins c0 c1 c2 c3

    GND c0
    fulladd a0 b0 c0  s0 c1
    fulladd a1 b1 c1  s1 c2
    fulladd a2 b2 c2  s2 c3
    fulladd a3 b3 c3  s3 v
}
```



```tcl
# Simple driver for the circuit
proc 4add_driver {a b} {
    lassign [split $a {}] a3 a2 a1 a0
    lassign [split $b {}] b3 b2 b1 b0
    lassign [split 00000 {}] s3 s2 s1 s0 v

    4add a0 a1 a2 a3  b0 b1 b2 b3  s0 s1 s2 s3  v

    return "$s3$s2$s1$s0 overflow=$v"
}
set a 1011
set b 0110
puts $a+$b=[4add_driver $a $b]
```

```txt

1011+0110=0001 overflow=1

```

One feature of the Tcl code is that if you change the definitions of <code>and</code>, <code>or</code>, <code>not</code> and <code>GND</code> (as well as of <code>gate</code> and <code>pins</code>, of course) you could have this Tcl code generate hardware for the adder. The bulk of the code would be identical.

## TorqueScript


```Torque
function XOR(%a, %b)
{
	return (!%a && %b) || (%a && !%b);
}

//Seperated by space
function HalfAdd(%a, %b)
{
	return XOR(%a, %b) SPC %a && %b;
}

//First word is the carry bit
function FullAdd(%a, %b, %c0)
{
	%r1 = HalfAdd(%a, %c0);
	%r2 = HalfAdd(getWord(%r1, 0), %b);
	%r3 = getWord(%r1, 1) || getWord(%r2, 1);
	return %r3 SPC getWord(%r2, 0);
}

//Outputs each bit seperated by a space.
function FourBitFullAdd(%a0, %a1, %a2, %a3, %b0, %b1, %b2, %b3)
{
	%r0 = FullAdd(%a0, %b0, 0);
	%r1 = FullAdd(%a1, %b1, getWord(%r0, 0));
	%r2 = FullAdd(%a2, %b2, getWord(%r1, 0));
	%r3 = FullAdd(%a3, %b3, getWord(%r2, 0));
	return getWord(%r0,1) SPC getWord(%r1,1) SPC getWord(%r2,1) SPC getWord(%r3,1) SPC getWord(%r3,0);
}
```



## Verilog

In Verilog we can also define a multibit adder as a component with multiple instances:

```Verilog

module Half_Adder( output c, s, input a, b );
  xor xor01 (s, a, b);
  and and01 (c, a, b);
endmodule // Half_Adder

module Full_Adder( output c_out, s, input a, b, c_in );

  wire s_ha1, c_ha1, c_ha2;

  Half_Adder ha01( c_ha1, s_ha1, a, b );
  Half_Adder ha02( c_ha2, s, s_ha1, c_in );
  or or01 ( c_out, c_ha1, c_ha2 );

endmodule // Full_Adder

module Full_Adder4( output [4:0] s, input [3:0] a, b, input c_in );

  wire [4:0] c;

  Full_Adder adder00 ( c[1], s[0], a[0], b[0], c_in );
  Full_Adder adder01 ( c[2], s[1], a[1], b[1], c[1] );
  Full_Adder adder02 ( c[3], s[2], a[2], b[2], c[2] );
  Full_Adder adder03 ( c[4], s[3], a[3], b[3], c[3] );

  assign s[4] = c[4];

endmodule // Full_Adder4

module test_Full_Adder();

  reg  [3:0] a;
  reg  [3:0] b;
  wire [4:0] s;

  Full_Adder4 FA4 ( s, a, b, 0 );

  initial begin
    $display( "   a +    b =     s" );
    $monitor( "%4d + %4d = %5d", a, b, s );
     a=4'b0000; b=4'b0000;
  #1 a=4'b0000; b=4'b0001;
  #1 a=4'b0001; b=4'b0001;
  #1 a=4'b0011; b=4'b0001;
  #1 a=4'b0111; b=4'b0001;
  #1 a=4'b1111; b=4'b0001;
  end

endmodule // test_Full_Adder

```


```txt

 a +  b =  s
 0 +  0 =  0
 0 +  1 =  1
 1 +  1 =  2
 3 +  1 =  4
 7 +  1 =  8
15 +  1 = 16

```



## VHDL

The following is a direct implementation of the proposed schematic:

```VHDL
LIBRARY ieee;
USE ieee.std_logic_1164.all;

entity four_bit_adder is
   port(
      a : in     std_logic_vector (3 downto 0);
      b : in     std_logic_vector (3 downto 0);
      s : out    std_logic_vector (3 downto 0);
      v : out    std_logic
   );
end four_bit_adder ;

LIBRARY ieee;
USE ieee.std_logic_1164.all;

entity fa is
   port(
      a  : in     std_logic;
      b  : in     std_logic;
      ci : in     std_logic;
      co : out    std_logic;
      s  : out    std_logic
   );
end fa ;

LIBRARY ieee;
USE ieee.std_logic_1164.all;

entity ha is
   port(
      a : in     std_logic;
      b : in     std_logic;
      c : out    std_logic;
      s : out    std_logic
   );
end ha ;

LIBRARY ieee;
USE ieee.std_logic_1164.all;

entity xor_gate is
   port(
      a : in     std_logic;
      b : in     std_logic;
      x : out    std_logic
   );
end xor_gate ;



architecture struct of four_bit_adder is
   signal ci0 : std_logic;
   signal co0 : std_logic;
   signal co1 : std_logic;
   signal co2 : std_logic;

   component fa
   port (
      a  : in     std_logic ;
      b  : in     std_logic ;
      ci : in     std_logic ;
      co : out    std_logic ;
      s  : out    std_logic
   );
   end component;
begin
   ci0 <= '0';

   i_fa0 : fa
      port map (
         a  => a(0),
         b  => b(0),
         ci => ci0,
         co => co0,
         s  => s(0)
      );
   i_fa1 : fa
      port map (
         a  => a(1),
         b  => b(1),
         ci => co0,
         co => co1,
         s  => s(1)
      );
   i_fa2 : fa
      port map (
         a  => a(2),
         b  => b(2),
         ci => co1,
         co => co2,
         s  => s(2)
      );
   i_fa3 : fa
      port map (
         a  => a(3),
         b  => b(3),
         ci => co2,
         co => v,
         s  => s(3)
      );

end struct;


architecture struct of fa is
   signal c1 : std_logic;
   signal c2 : std_logic;
   signal s1 : std_logic;

   component ha
   port (
      a : in     std_logic ;
      b : in     std_logic ;
      c : out    std_logic ;
      s : out    std_logic
   );
   end component;
begin
   co <= c1 or c2;

   i_ha0 : ha
      port map (
         a => ci,
         b => a,
         c => c1,
         s => s1
      );
   i_ha1 : ha
      port map (
         a => s1,
         b => b,
         c => c2,
         s => s
      );
end struct;


architecture struct of ha is
   component xor_gate
   port (
      a : in     std_logic;
      b : in     std_logic;
      x : out    std_logic
   );
   end component;
begin
   c <= a and b;

   i_xor_gate : xor_gate
      port map (
         a => a,
         b => b,
         x => s
      );
end struct;


architecture rtl of xor_gate is
begin
  x <= (a and not b) or (b and not a);
end architecture rtl;
```



An exhaustive testbench:

```VHDL
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.NUMERIC_STD.all;

entity tb is
end tb ;


architecture struct of tb is
   signal a : std_logic_vector(3 downto 0);
   signal b : std_logic_vector(3 downto 0);
   signal s : std_logic_vector(3 downto 0);
   signal v : std_logic;

   component four_bit_adder
   port (
      a : in     std_logic_vector (3 downto 0);
      b : in     std_logic_vector (3 downto 0);
      s : out    std_logic_vector (3 downto 0);
      v : out    std_logic
   );
   end component;
begin

   proc_test: process
   begin
     for x in 0 to 15 loop
       for y in 0 to 15 loop
         a <= std_logic_vector(to_unsigned(x, 4));
         b <= std_logic_vector(to_unsigned(y, 4));
         wait for 100 ns;
       end loop;
     end loop;
     wait;
   end process;

   i_four_bit_adder : four_bit_adder
      port map (
         a => a,
         b => b,
         s => s,
         v => v
      );

end struct;
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;

func Not(A);
int A;
return not A;

func And(A, B);
int A, B;
return A and B;

func Or(A, B);
int A, B;
return A or B;

func Xor(A, B);
int A, B;
return Or(And(A, Not(B)), And(Not(A), B));

proc HalfAdd(A, B, S, C);
int A, B, S, C;
[S(0):= Xor(A, B);
 C(0):= And(A, B);
];

proc FullAdd(A, B, Ci, S, Co);
int A, B, Ci, S, Co;            \(Ci and Co are reversed from drawing)
int S0, S1, C0, C1;
[HalfAdd(Ci, A, @S0, @C0);
 HalfAdd(S0, B, @S1, @C1);
 S(0):= S1;
 Co(0):= Or(C0, C1);
];

proc Add4Bits(A0, A1, A2, A3, B0, B1, B2, B3, S0, S1, S2, S3, Co);
int A0, A1, A2, A3, B0, B1, B2, B3, S0, S1, S2, S3, Co;
int Co0, Co1, Co2;
[FullAdd(A0, B0, 0,   S0, @Co0);
 FullAdd(A1, B1, Co0, S1, @Co1);
 FullAdd(A2, B2, Co1, S2, @Co2);
 FullAdd(A3, B3, Co2, S3, Co);
];

proc BinOut(D, A0, A1, A2, A3, C);
int D, A0, A1, A2, A3, C;
[IntOut(D, C&1);
IntOut(D, A3&1);
IntOut(D, A2&1);
IntOut(D, A1&1);
IntOut(D, A0&1);
];

int S0, S1, S2, S3, C;
[Add4Bits(1, 0, 0, 0, 0, 0, 1, 0, @S0, @S1, @S2, @S3, @C);  \0001 + 0100 = 00101
 BinOut(0, S0, S1, S2, S3, C);  CrLf(0);
 Add4Bits(1, 0, 1, 0, 0, 1, 1, 1, @S0, @S1, @S2, @S3, @C);  \0101 + 1110 = 10011
 BinOut(0, S0, S1, S2, S3, C);  CrLf(0);
 Add4Bits(1, 1, 1, 1, 1, 1, 1, 1, @S0, @S1, @S2, @S3, @C);  \1111 + 1111 = 11110
 BinOut(0, S0, S1, S2, S3, C);  CrLf(0);
]
```


```txt

00101
10011
11110

```



## zkl


```zkl
fcn xor(a,b) // a,b are 1|0 -->a^b(1|0)
   { a.bitAnd(b.bitNot()).bitOr(b.bitAnd(a.bitNot())) }

fcn halfAdder(a,b) // -->(carry, a+b) (1|0)
   { return(a.bitAnd(b), xor(a,b)) }

fcn fullBitAdder(c, a,b){ //-->(carry, a+b+c), a,b,c are 1|0
   c1,s := halfAdder(a,c);
   c2,s := halfAdder(s,b);
   c3   := c1.bitOr(c2);
   return(c3,s);
}

   // big endian
fcn fourBitAdder(a3,a2,a1,a0, b3,b2,b1,b0){ //-->(carry, s3,s2,s1,s0)
   c,s0 := fullBitAdder(0, a0,b0);
   c,s1 := fullBitAdder(c, a1,b1);
   c,s2 := fullBitAdder(c, a2,b2);
   c,s3 := fullBitAdder(c, a3,b3);
   return(c, s3,s2,s1,s0);
}

// add(10,9)  result should be 1 0 0 1 1 (0x13, 3 carry 1)
println(fourBitAdder(1,0,1,0, 1,0,0,1));
```


```zkl
fcn nBitAddr(as,bs){ //-->(carry, sn..s3,s2,s1,s0)
   (ss:=List()).append(
      [as.len()-1 .. 0,-1].reduce('wrap(c,n){
         c2,s:=fullBitAdder(c,as[n],bs[n]); ss + s; c2
      },0))
   .reverse();
}
println(nBitAddr(T(1,0,1,0), T(1,0,0,1)));
```

```txt

L(1,0,0,1,1)
L(1,0,0,1,1)

```


