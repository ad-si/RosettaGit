+++
title = "Luhn test of credit card numbers"
description = ""
date = 2019-09-12T14:14:18Z
aliases = []
[extra]
id = 6216
[taxonomies]
categories = ["task", "Checksums"]
tags = []
+++

The [[wp:Luhn algorithm|Luhn test]] is used by some credit card companies to distinguish valid credit card numbers from what could be a random selection of digits.

Those companies using credit card numbers that can be validated by the Luhn test have numbers that pass the following test:
#  Reverse the order of the digits in the number.
#  Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
#  Taking the second, fourth ... and every other even digit in the reversed digits:
:#  Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
:#  Sum the partial sums of the even digits to form s2
# If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.



For example, if the trial number is 49927398716:

```txt
Reverse the digits:
  61789372994
Sum the odd digits:
  6 + 7 + 9 + 7 + 9 + 4 = 42 = s1
The even digits:
    1,  8,  3,  2,  9
  Two times each even digit:
    2, 16,  6,  4, 18
  Sum the digits of each multiplication:
    2,  7,  6,  4,  9
  Sum the last:
    2 + 7 + 6 + 4 + 9 = 28 = s2

s1 + s2 = 70 which ends in zero which means that 49927398716 passes the Luhn test
```



## Task

Write a function/method/procedure/subroutine that will validate a number with the Luhn test, and

use it to validate the following numbers:
    49927398716
    49927398717
    1234567812345678
    1234567812345670



## Related tasks

*   [[SEDOLs|SEDOL]]
*   [[Calculate International Securities Identification Number|ISIN]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
and an ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Luhn test of credit card numbers        22/05/2016
LUHNTEST CSECT
         USING  LUHNTEST,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R9,T               @t(k)
         LA     R8,N               for n
LOOPK    EQU    *                  for k=1 to n
         LR     R4,R9              @t(k),@s[1]
         LA     R6,1               from i=1
         LA     R7,M               to m
LOOPI1   CR     R6,R7              for i=1 to m
         BH     ELOOPI1            leave i
         CLI    0(R4),C' '           if mid(s,i,1)=" "
         BNE    ITERI1               then
         BCTR   R6,0                   i-1
         ST     R6,L                   l=i-1
         B      ELOOPI1                exit for
*                                    end if
ITERI1   LA     R4,1(R4)             next @s[i]
         LA     R6,1(R6)             i=i+1
         B      LOOPI1             next i
ELOOPI1  EQU    *                  out of loop i
         MVC    W,BLANK            w=" "
         LA     R4,W               iw=@w
         LR     R5,R9              is=@s
         A      R5,L               is=@s+l
         BCTR   R5,0               is=s+l-1
         L      R6,L               i=l
         LA     R7,1               to 1
LOOPI2   CR     R6,R7              for i=l to 1 by -1
         BL     ELOOPI2            leave i
         MVC    0(1,R4),0(R5)        mid(w,iw,1)=mid(s,is,1)
         LA     R4,1(R4)             iw=iw+1
         BCTR   R5,0                 is=is-1
         BCTR   R6,0                 i=i-1
         B      LOOPI2             next i
ELOOPI2  EQU    *                  out of loop i
         LA     R11,0              s1=0
         LA     R12,0              s2=0
         LA     R6,1               i=1
         L      R7,L               to l
LOOPI3   CR     R6,R7              for i=1 to l
         BH     ELOOPI3            leave i
         LA     R2,W-1             @w-1
         AR     R2,R6              w[i]
         MVC    CI,0(R2)           ci=mid(w,i,1)
         NI     CI,X'0F'           zap upper half byte
         LR     R4,R6              i
         SRDA   R4,32              >>32
         D      R4,=F'2'           i/2
         LTR    R4,R4              if mod(i,2)>0
         BNH    NOTMOD             then
         XR     R2,R2                clear
         IC     R2,CI                z=cint(mid(w,i,1))
         AR     R11,R2               s1=s1+cint(mid(w,i,1))
         B      EIFMOD             else
NOTMOD   XR     R2,R2                clear
         IC     R2,CI                cint(mid(w,i,1))
         SLA    R2,1                 *2
         ST     R2,Z                 z=cint(mid(w,i,1))*2
         C      R2,=F'10'            if z<10
         BNL    GE10                 then
         A      R12,Z                  s2=s2+z
         B      EIF10                else
GE10     L      R2,Z                   z
         CVD    R2,PL8                 binary to packed
         UNPK   CL16,PL8               packed to zoned
         OI     CL16+15,X'F0'          zoned to char (zap sign)
         MVC    X(1),CL16+15           x=right(cstr(z),1)
         NI     X,X'0F'                zap upper half byte
         XR     R2,R2                  r2=0
         IC     R2,X                   r2=cint(right(cstr(z),1))
         AR     R12,R2                 s2=s2+r2
         LA     R12,1(R12)             s2=s2+cint(right(cstr(z),1))+1
EIF10    EQU    *                    end if
EIFMOD   EQU    *                  end if
         LA     R6,1(R6)           i=i+1
         B      LOOPI3             next i
ELOOPI3  EQU    *                  out of loop i
         LR     R1,R11             s1
         AR     R1,R12             s1+s2
         CVD    R1,PL8             binary to packed
         UNPK   CL16,PL8           packed to zoned
         CLI    CL16+15,X'C0'      if right(cstr(s1+s2),1)="0"
         BNE    NOTZERO            then
         MVC    R,=CL8'Valid'        r="Valid"
         B      ECLI               else
NOTZERO  MVC    R,=CL8'Invalid'      r="Invalid"
ECLI     EQU    *                  end if
         MVC    PG(M),0(R9)        t(k)
         MVC    PG+M+1(L'R),R      r
         XPRNT  PG,L'PG            print buffer
         LA     R9,M(R9)           at=at+m
         BCT    R8,LOOPK           next k
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
N        EQU    (TEND-T)/L'T
M        EQU    20
T        DC     CL(M)'49927398716         '
         DC     CL(M)'49927398717         '
         DC     CL(M)'1234567812345678    '
         DC     CL(M)'1234567812345670    '
TEND     DS     0C
W        DS     CL(M)
BLANK    DC     CL(M)' '
L        DS     F
Z        DS     F
PL8      DS     PL8
CL16     DS     CL16
CI       DS     C
X        DS     C
R        DS     CL8
PG       DC     CL80' '            buffer
         YREGS
         END    LUHNTEST
```

```txt

49927398716          Valid
49927398717          Invalid
1234567812345678     Invalid
1234567812345670     Valid

```



## 8th


```Forth

\ Adapted from the C version:
: remap \ n1 -- n2
  [0,2,4,6,8,1,3,5,7,9]
  swap caseof ;

: luhn \ s -- f
  0 swap
  s:rev
  (
    '0 n:-
    swap 2 n:mod if remap then
    n:+
  ) s:each
  10 n:mod not ;

: test-luhn \ s --
  dup . space
  luhn if "OK" else "FAIL" then . cr ;

"49927398716" test-luhn
"49927398717" test-luhn
"1234567812345678" test-luhn
"1234567812345670" test-luhn

bye
```


```txt

49927398716 OK
49927398717 FAIL
1234567812345678 FAIL
1234567812345670 OK

```



## ABAP


```Abap
METHOD luhn_check.

  DATA: sum(1) TYPE n VALUE 0. " Sum of checksum.
  DATA: current TYPE i. " Current digit.
  DATA: odd TYPE i VALUE 1. " Multiplier.
  DATA: len TYPE i. " String crowler.


  " Luhn algorithm.
  len = NUMOFCHAR( pi_string ) - 1.
  WHILE ( len >= 0 ).
    current = pi_string+len(1) * odd.
    IF ( current > 9 ).
      current = current - 9. " Digits sum.
    ENDIF.
    sum = sum + current.
    odd = 3 - odd. " 1 <--> 2 Swich
    len = len - 1. " Move to next charcter.
  ENDWHILE.

  " Validation check.
  IF ( sum = 0 ).
    pr_valid = abap_true.
  ELSE.
    pr_valid = abap_false.
  ENDIF.

ENDMETHOD.
```



## ACL2


```Lisp
(include-book "arithmetic-3/top" :dir :system)

(defun digits (n)
   (if (zp n)
       nil
       (cons (mod n 10)
             (digits (floor n 10)))))

(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs)
          (sum (rest xs)))))

(defun double-and-sum-digits (xs)
   (if (endp xs)
       nil
       (cons (sum (digits (* 2 (first xs))))
             (double-and-sum-digits (rest xs)))))

(defun dmx (xs)
   (if (endp (rest xs))
       (mv xs nil)
       (mv-let (odds evens)
               (dmx (rest (rest xs)))
          (mv (cons (first xs) odds)
              (cons (second xs) evens)))))

(defun luhn (n)
   (mv-let (odds evens)
           (dmx (digits n))
      (= (mod (+ (sum odds)
                 (sum (double-and-sum-digits evens)))
              10)
         0)))
```


```txt
&gt; (luhn 49927398716)
T
&gt; (luhn 49927398717)
NIL
&gt; (luhn 1234567812345678)
NIL
&gt; (luhn 1234567812345670)
T
```



## ActionScript


```ActionScript
function isValid(numString:String):Boolean
{
	var isOdd:Boolean = true;
	var oddSum:uint = 0;
	var evenSum:uint = 0;
	for(var i:int = numString.length - 1; i >= 0; i--)
	{
		var digit:uint = uint(numString.charAt(i))
		if(isOdd) oddSum += digit;
		else evenSum += digit/5 + (2*digit) % 10;
		isOdd = !isOdd;
	}
	if((oddSum + evenSum) % 10 == 0) return true;
	return false;
}

trace(isValid("49927398716"));
trace(isValid("49927398717"));
trace(isValid("1234567812345678"));
trace(isValid("1234567812345670"));
```



## Ada

```Ada
with Ada.Text_IO;
use  Ada.Text_IO;

procedure Luhn is

  function Luhn_Test (Number: String) return Boolean is
    Sum  : Natural := 0;
    Odd  : Boolean := True;
    Digit: Natural range 0 .. 9;
  begin
    for p in reverse Number'Range loop
      Digit := Integer'Value (Number (p..p));
      if Odd then
        Sum := Sum + Digit;
      else
        Sum := Sum + (Digit*2 mod 10) + (Digit / 5);
      end if;
      Odd := not Odd;
    end loop;
    return (Sum mod 10) = 0;
  end Luhn_Test;

begin

  Put_Line (Boolean'Image (Luhn_Test ("49927398716")));
  Put_Line (Boolean'Image (Luhn_Test ("49927398717")));
  Put_Line (Boolean'Image (Luhn_Test ("1234567812345678")));
  Put_Line (Boolean'Image (Luhn_Test ("1234567812345670")));

end Luhn;
```

```txt

TRUE
FALSE
FALSE
TRUE

```



## ALGOL 68

```algol68
PROC to int = (CHAR c)INT:
    ABS c - ABS "0";

PROC confirm = (STRING id)BOOL:
(
    BOOL is odd digit := TRUE;
    INT s := 0;
    STRING cp;

    FOR cp key FROM UPB id BY -1 TO LWB id DO
        INT k := to int(id[cp key]);
        s +:=
            IF is odd digit THEN k
            ELIF k /= 9 THEN 2*k MOD 9
            ELSE 9
            FI;
        is odd digit := NOT is odd digit
    OD;
    0 = s MOD 10
);

main:
(
    []STRING t cases = (
        "49927398716",
        "49927398717",
        "1234567812345678",
        "1234567812345670"
    );
    FOR cp key TO UPB t cases DO
        STRING cp = t cases[cp key];
        print((cp, ": ", confirm(cp), new line))
    OD
)
```

```txt

49927398716: T
49927398717: F
1234567812345678: F
1234567812345670: T

```



## ALGOL W

Separate source so the LuhnTest procedure can be used in other tasks, e.g.: [[Validate International Securities Identification Number]]

```algolw
% returns true if ccNumber passes the Luhn test, false otherwise %
% as Algol W has fixed length strings, the length of the number  %
% must be specified in ccLength                                  %
logical procedure LuhnTest ( string(32) value ccNumber
                           ; integer    value ccLength
                           ) ;
begin
    integer checkSum;
    logical oddDigit, isValid;
    checkSum := 0;
    isValid := oddDigit := true;
    for cPos := ccLength step -1 until 1 do begin
        integer digit;
        digit := decode( ccNumber( cPos - 1 // 1 ) ) - decode( "0" );
        if digit < 0 or digit > 9 then isValid := false
        else if oddDigit
        then checkSum := checkSum + digit
        else checkSum := checkSum + ( case digit + 1 of ( 0, 2, 4, 6, 8
                                                        , 1, 3, 5, 7, 9
                                                        )
                                    );
        oddDigit := not oddDigit
    end for_cPos ;
    isValid and ( ( checkSum rem 10 ) = 0 )
end LuhnTest
```


Use the above to test the LuhnTest procedure:


```algolw
begin
    % external procedure that returns true if ccNumber passes the Luhn test, false otherwise %
    logical procedure LuhnTest ( string(32) value ccNumber
                               ; integer    value ccLength
                               ) ; algol "LUHN" ;


    % task test cases %

    procedure testLuhnTest ( string(32) value ccNumber
                           ; integer    value ccLength
                           ) ;
        write( s_w := 0, ccNumber, if LuhnTest( ccNumber, ccLength ) then " is valid" else " is invalid" );

    testLuhnTest( "49927398716",      11 );
    testLuhnTest( "49927398717",      11 );
    testLuhnTest( "1234567812345678", 16 );
    testLuhnTest( "1234567812345670", 16 )

end.
```

```txt

49927398716                      is valid
49927398717                      is invalid
1234567812345678                 is invalid
1234567812345670                 is valid

```



## APL

```APL
r←LuhnTest digits;even;odd;SumEven
(odd even)←↓⍉⍎¨{(n,2)⍴(⍵,'0')↑⍨2×n←⌈(⍴⍵)÷2}⌽digits
SumEven←{+/+/¨⍎¨¨⍕¨2×⍵}
r←'0'=⊃¯1↑⍕(+/odd)+(SumEven even)
```

```txt

 1 0 0 1

```


```APL
    ∇ ret←LuhnTest num;s1;s2
[1]   num←⌽((⌈10⍟num)/10)⊤num
[2]   s1←+/((⍴num)⍴1 0)/num
[3]   s2←+/∊(⊂10 10)⊤¨2×((⍴num)⍴0 1)/num
[4]   ret←0=10⊤s1+s2
    ∇
```

```txt
      LuhnTest¨ 49927398716 49927398717 1234567812345678 1234567812345670
1 0 0 1

```



## ARM Assembly

<lang ARM_Assembly>.text
.global _start
_start:
    ldr r0, =example_numbers
    bl test_number

    add r1, r0, #1
    bl length
    add r0, r1, r0
    bl test_number

    add r1, r0, #1
    bl length
    add r0, r1, r0
    bl test_number

    add r1, r0, #1
    bl length
    add r0, r1, r0
    bl test_number

    mov r0, #0
    mov r7, #1
    swi 0

test_number:
    push {r0, lr}
    bl print_string

    bl luhn_test
    cmp r0, #1
    ldreq r0, =valid_message
    ldrne r0, =invalid_message
    bl print_string
    pop {r0, lr}
    mov pc, lr



print_string:
    push {r0-r7, lr}
    mov r1, r0   @ string to print
    bl length
    mov r2, r0   @ length of string
    mov r0, #1   @ write to stdout
    mov r7, #4   @ SYS_WRITE
    swi 0        @ call system interupt
    pop {r0-r7, lr}
    mov pc, lr

@ r0 address of credit card number string
@ returns result in r0
luhn_test:
    push {r1-r7, lr}
    mov r1, r0
    bl isNumerical            @ check if string is a number
    cmp r0, #1
    bne .luhn_test_end        @ exit if not number
    mov r0, r1
    ldr r1, =reversed_string  @ address to store reversed string
    bl reverse                @ reverse string
    push {r0}
    bl length   @ get length of string
    mov r4, r0  @ store string length in r4
    pop {r0}
    mov r2, #0  @ string index
    mov r6, #0  @ sum of odd digits
    mov r7, #0  @ sum of even digits
    .loadNext:
        ldrb r3, [r1, r2]         @ load byte into r3
        sub r3, #'0'              @ convert letter to digit
        and r5, r2, #1            @ test if index is even or odd
        cmp r5, #0
        beq .odd_digit
        bne .even_digit
        .odd_digit:
            add r6, r3              @ add digit to sum if odd
            b .continue             @ skip next step
        .even_digit:
            lsl r3, #1              @ multiply digit by 2
            cmp r3, #10             @ sum digits
            subge r3, #10           @ get digit in 1s place
            addge r3, #1            @ add 1 for the 10s place
            add r7, r3              @ add digit sum to the total

        .continue:
        add r2, #1                @ increment digit index
        cmp r2, r4                @ check if at end of string
        blt .loadNext

    add r0, r6, r7                @ add even and odd sum
    mov r3, r0                    @ copy sum to r3
    ldr r1, =429496730            @ (2^32-1)/10
    sub r0, r0, r0, lsr #30       @ divide by 10
    umull r2, r0, r1, r0
    mov r1, #10
    mul r0, r1                    @ multiply the r0 by 10 to see if divisible
    cmp r0, r3                    @ compare with the original value in r3
    .luhn_test_end:
    movne r0, #0                  @ return false if invalid card number
    moveq r0, #1                  @ return true if valid card number
    pop {r1-r7, lr}
    mov pc, lr

length:
    push {r1-r2, lr}
    mov r2, r0              @ start of string address
    .loop:
        ldrb r1, [r2], #1   @ load byte from address r2 and increment
        cmp r1, #0          @ check for end of string
        bne .loop           @ load next byte if not 0
    sub r0, r2, r0          @ subtract end of string address from start
    sub r0, #1              @ end of line from count
    pop {r1-r2, lr}
    mov pc, lr

@ reverses a string
@ r0 address of string to reverse
@ r1 address to store reversed string
reverse:
    push {r0-r5, lr}
    push {r0, lr}
    bl length                @ get length of string to reverse
    mov r3, r0               @ backword index
    pop {r0, lr}
    mov r4, #0               @ fowrard index
    .reverse_next:
        sub r3, #1           @ decrement backword index
        ldrb r5, [r0, r3]    @ load byte from original string at index
        strb r5, [r1, r4]    @ copy byte to reversed string
        add r4, #1           @ increment fowrard index
        cmp r3, #0           @ check if any characters are left
        bge .reverse_next

    mov r5, #0
    strb r5, [r1, r4]  @ write null byte to terminate reversed string
    pop {r0-r5, lr}
    mov pc, lr

isNumerical:
    push {r1, lr}
    .isNumerical_checkNext:
        ldrb r1, [r0], #1
        cmp r1, #0
        beq .isNumerical_true
        cmp r1, #'0'
        blt .isNumerical_false
        cmp r1, #'9'
        bgt .isNumerical_false
        b .isNumerical_checkNext
    .isNumerical_false:
        mov r0, #0
        b .isNumerical_end
    .isNumerical_true:
        mov r0, #1
    .isNumerical_end:
    pop {r1, lr}
    mov pc, lr


.data
valid_message:
    .asciz " valid card number\n"
invalid_message:
    .asciz " invalid card number\n"

reversed_string:
    .space 32

example_numbers:
    .asciz "49927398716"
    .asciz "49927398717"
    .asciz "1234567812345678"
    .asciz "1234567812345670"
```



## AutoHotkey


```AutoHotkey
; Originally submitted by Laszlo:
; http://www.autohotkey.com/forum/post-229412.html#229412

MsgBox % LuhnTest(49927398716)
MsgBox % LuhnTest(49927398717)
MsgBox % LuhnTest(1234567812345678)
MsgBox % LuhnTest(1234567812345670)

Return

;-----------------------------

LuhnTest(Number)
{
  MultFactor := 2 - ( StrLen(Number) & 1 )  ,  Sum := 0
  Loop, Parse, Number
    Sum += ( ( 9 < ( Temp := MultFactor * A_LoopField ) ) ? Temp - 9 : Temp )  ,  MultFactor := 3 - MultFactor
  Return !Mod(Sum,10)
}
```




## AutoIt


```AutoIt

Global $avarray[4] = [49927398716, 49927398717, 1234567812345678, 1234567812345670]
For $i = 0 To 3
	checkLuhn($avarray[$i])
Next

Func checkLuhn($number)
	$sum = 0
	$numDigits = StringSplit($number, "")
	For $i = $numDigits[0] - 1 To 1 Step -2
		$numDigits[$i] = $numDigits[$i] * 2
		If $numDigits[$i] >= 10 Then $numDigits[$i] -= 9
	Next
	For $i = 1 To $numDigits[0]
		$sum += $numDigits[$i]
	Next
	If StringRight($sum, 1) = "0" Then
		ConsoleWrite("Luhn-Check (" & $number & ") : True" & @CRLF)
		Return True
	Else
		ConsoleWrite("Luhn-Check (" & $number & ") : False" & @CRLF)
		Return False
	EndIf
EndFunc   ;==>checkLuhn

```


```txt
Luhn-Check (49927398716) : True
Luhn-Check (49927398717) : False
Luhn-Check (1234567812345678) : False
Luhn-Check (1234567812345670) : True
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
    A[1] = 49927398716;
    A[2] = 49927398717;
    A[3] = 1234567812345678;
    A[4] = 1234567812345670;
    A[5] = "01234567897";
    A[6] = "01234567890";
    A[7] = "00000000000";
    for (k in A) print "isLuhn("A[k]"): ",isLuhn(A[k]);
}

function isLuhn(cardno) {
    s = 0;
    m = "0246813579";
    n = length(cardno);
    for (k = n; 0 < k; k -= 2) {
	s += substr(cardno, k, 1);
    }
    for (k = n-1; 0 < k; k -= 2) {
	s += substr(m, substr(cardno, k, 1)+1, 1);
    }
    return ((s%10)==0);
}
```

```txt
isLuhn(1234567812345670):  1
isLuhn(01234567897):  1
isLuhn(01234567890):  0
isLuhn(00000000000):  1
isLuhn(49927398716):  1
isLuhn(49927398717):  0
isLuhn(1234567812345678):  0
```



## Bash


```bash
#!/bin/bash

function luhn_validate  # <numeric-string>
{
    num=$1
    shift 1

    len=${#num}
    is_odd=1
    sum=0
    for((t = len - 1; t >= 0; --t)) {
        digit=${num:$t:1}

        if [[ $is_odd -eq 1 ]]; then
            sum=$(( sum + $digit ))
        else
            sum=$(( $sum + ( $digit != 9 ? ( ( 2 * $digit ) % 9 ) : 9 ) ))
        fi

        is_odd=$(( ! $is_odd ))
    }

    # NOTE: returning exit status of 0 on success
    return $(( 0 != ( $sum % 10 ) ))
}


function print_result  # <numeric-string>
{
    if luhn_validate "$1"; then
        echo "$1 is valid"
    else
        echo "$1 is not valid"
    fi
}

print_result "49927398716"
print_result "49927398717"
print_result "1234567812345678"
print_result "1234567812345670"
```


```txt
49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid

```



## Batch File

This simple implementation does not reverse the numbers. Instead, it counts from right to left.

```dos
@echo off
setlocal enabledelayedexpansion

call :luhn 49927398716
call :luhn 49927398717
call :luhn 1234567812345678
call :luhn 1234567812345670
exit /b 0

:luhn
set input=%1
set cnt=0
set s1=0&set s2=0
:digit_loop
	set /a cnt-=1
	set /a isOdd=^(-%cnt%^)%%2

	if !isodd! equ 1 (
		set /a s1+=!input:~%cnt%,1!
	) else (
		set /a twice=!input:~%cnt%,1!*2
		if !twice! geq 10 (
			set /a s2+=!twice:~0,1!+!twice:~1,1!
		) else (
			set /a s2+=!twice!
		)
	)
	if "!input:~%cnt%!"=="!input!" (
		set /a sum=^(!s1!+!s2!^)%%10
		if !sum! equ 0 (echo !input! is valid.) else (echo !input! is not valid.)
		goto :EOF
	)
	goto digit_loop
```

```txt
>luhn.bat
49927398716 is valid.
49927398717 is not valid.
1234567812345678 is not valid.
1234567812345670 is valid.

>
```



## BBC BASIC


```bbcbasic
      FOR card% = 1 TO 4
        READ cardnumber$
        IF FNluhn(cardnumber$) THEN
          PRINT "Card number " cardnumber$ " is valid"
        ELSE
          PRINT "Card number " cardnumber$ " is invalid"
        ENDIF
      NEXT card%
      END

      DATA 49927398716, 49927398717, 1234567812345678, 1234567812345670

      DEF FNluhn(card$)
      LOCAL I%, L%, N%, S%
      L% = LEN(card$)
      FOR I% = 1 TO L%
        N% = VAL(MID$(card$, L%-I%+1, 1))
        IF I% AND 1 THEN
          S% += N%
        ELSE
          N% *= 2
          S% += N% MOD 10 + N% DIV 10
        ENDIF
      NEXT
      = (S% MOD 10) = 0
```



## bc


```bc
/* Return 1 if number passes Luhn test, else 0 */
define l(n) {
    auto m, o, s, x

    o = scale
    scale = 0

    m = 1
    while (n > 0) {
        x = (n % 10) * m
        if (x > 9) x -= 9
        s += x
        m = 3 - m
        n /= 10
    }

    s %= 10
    scale = o
    if (s) return(0)
    return(1)
}

l(49927398716)
l(49927398717)
l(1234567812345678)
l(1234567812345670)
```


```txt
1
0
0
1
```



## Befunge



```befunge>v  1
$0 v   v                                    <
>&:19+`|v  <            >v      5      6   7      8
^  \   <>09p19p>09g+09p:|>2*:19+%19g+19p19+/19g+19p:|
   2          3      4  >                           v
                              v"invalid"<10  9
                                        |%+91+g91g90<
                              v  "valid"<
                              >:#,_@
                                 11

```

The labelled points (1 to 11) are:
1. Read in input until number greater than 10,
2. Reverse the order,
3. Set accumulators to 0,
4. Add odd number to accumulator,
5. Mod even number with 10,
6. Add this digit to accumulator,
7. Integer divide number by 10,
8. Add this digit to accumulator,
9. Add odd and even accumulators,
10. Mod this accumulator with 10,
11. Print result.

The code requires input be separated by spaces and ended with a number greater than 10 to exit the reading loop. This could be done by reading characters and ending at a new line, but this way is much simpler.

'''Inputs''':

```txt

4 9 9 2 7 3 9 8 7 1 6 99
4 9 9 2 7 3 9 8 7 1 7 99
1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 99
1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 0 99

```


```txt

valid
invalid
invalid
valid

```



## Bracmat


```bracmat
  ( luhn
  =   sum odd even
    .   0:?sum
      & rev$!arg:?arg
      &   whl
        ' ( @( !arg
             :   %?odd
                 ( %?even ?arg
                 | :?arg&0:?even
                 )
             )
          & !odd+mod$(2*!even.10)+div$(!even.5)+!sum:?sum
          )
      & mod$(!sum.10):0
  )
& ( test
  =
    .   out
      $ (!arg ":" (luhn$!arg&true|false))
  )
& test$49927398716
& test$49927398717
& test$1234567812345678
& test$1234567812345670
& ;
```

```txt
49927398716 : true
49927398717 : false
1234567812345678 : false
1234567812345670 : true
```



## C


```c
#include <string.h>
#include <stdio.h>

int luhn(const char* cc)
{
	const int m[] = {0,2,4,6,8,1,3,5,7,9}; // mapping for rule 3
	int i, odd = 1, sum = 0;

	for (i = strlen(cc); i--; odd = !odd) {
		int digit = cc[i] - '0';
		sum += odd ? digit : m[digit];
	}

	return sum % 10 == 0;
}

int main()
{
	const char* cc[] = {
		"49927398716",
		"49927398717",
		"1234567812345678",
		"1234567812345670",
		0
	};
	int i;

	for (i = 0; cc[i]; i++)
		printf("%16s\t%s\n", cc[i], luhn(cc[i]) ? "ok" : "not ok");

	return 0;
}
```
      49927398716        ok
      49927398717        not ok
 1234567812345678        not ok
 1234567812345670        ok


## C++


```cpp
#include <iostream>
using namespace std;

int toInt(const char c)
{
    return c-'0';
}

int confirm( const char *id)
{
    bool is_odd_dgt = true;
    int s = 0;
    const char *cp;

    for(cp=id; *cp; cp++);
    while(cp > id) {
        --cp;
        int k = toInt(*cp);
        if (is_odd_dgt) {
            s += k;
        }
        else {
            s += (k!=9)? (2*k)%9 : 9;
        }
	is_odd_dgt = !is_odd_dgt;
    }
    return 0 == s%10;
}

int main( )
{
    const char * t_cases[] = {
        "49927398716",
        "49927398717",
        "1234567812345678",
        "1234567812345670",
        NULL,
    };
    for ( const char **cp = t_cases; *cp; cp++) {
        cout << *cp << ": " << confirm(*cp) << endl;
    }
    return 0;
}
```



### C++11


```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

bool luhn( const string& id)
{
  static const int m[10]  = {0,2,4,6,8,1,3,5,7,9}; // mapping for rule 3
  bool is_odd_dgt = false;
  auto lambda = [&](int a, char c) {return a + ((is_odd_dgt = !is_odd_dgt) ? c-'0' : m[c-'0']);};
  int s = std::accumulate(id.rbegin(), id.rend(), 0, lambda);
  return 0 == s%10;
}

int main( )
{
  auto t_cases = {"49927398716", "49927398717", "1234567812345678", "1234567812345670"};
  auto print = [](const string & s) {cout << s << ": " << luhn(s) << endl;};
  for_each(t_cases.begin(), t_cases.end(), print);
  return 0;
}

```


It is also possible to achieve a compile-time version using metaprogramming.


```cpp
#include <iostream>
#include <type_traits>

template<size_t I, int... Args>
struct find_impl;

template<int A, int... Args>
struct find_impl<0, A, Args...> {
    using type = std::integral_constant<int, A>;
};

template<int A, int B, int... Args>
struct find_impl<0, A, B, Args...> {
    using type = std::integral_constant<int, A>;
};

template<size_t I, int A, int B, int... Args>
struct find_impl<I, A, B, Args...> {
    using type = typename find_impl<I-1, B, Args...>::type;
};

namespace detail {
template<typename, typename>
struct append_sequence
{};

template<typename T, typename... Ts>
struct append_sequence<T, std::tuple<Ts...>> {
    using type = std::tuple<Ts..., T>;
};

template<typename... Ts>
struct reverse_sequence {
    using type = std::tuple<>;
};

template<typename T, typename... Ts>
struct reverse_sequence<T, Ts...> {
    using type = typename append_sequence<
                            T,
                            typename reverse_sequence<Ts...>::type
                        >::type;
};
}

template<size_t I>
using rule3 = typename find_impl<I, 0, 2, 4, 6, 8, 1, 3, 5, 7, 9>::type;

template<int A, char C, bool dgt>
struct calc
    : std::integral_constant<int, A + C - '0'>
{};

template<int A, char C>
struct calc<A, C, false>
    : std::integral_constant<int, A + rule3<C - '0'>::type::value>
{};

template<typename Acc, bool Dgt, char...>
struct luhn_impl;

template<typename Acc, bool Dgt, char A, char... Args>
struct luhn_impl<Acc, Dgt, A, Args...> {
    using type = typename calc<Acc::value, A, Dgt>::type;
};

template<typename Acc, bool Dgt, char A, char B, char... Args>
struct luhn_impl<Acc, Dgt, A, B, Args...> {
    using type =
        typename luhn_impl<typename calc<Acc::value, A, Dgt>::type, !Dgt, B, Args...>::type;
};

template<typename>
struct luhn;

template<typename... Args>
struct luhn<std::tuple<Args...>> {
    using type = typename luhn_impl<std::integral_constant<int, 0>, true, Args::value...>::type;
    constexpr static bool result = (type::value % 10) == 0;
};

template<char... Args>
bool operator "" _luhn() {
    return luhn<typename detail::reverse_sequence<std::integral_constant<char, Args>...>::type>::result;
}

int main() {
    std::cout << std::boolalpha;
    std::cout << 49927398716_luhn << std::endl;
    std::cout << 49927398717_luhn << std::endl;
    std::cout << 1234567812345678_luhn << std::endl;
    std::cout << 1234567812345670_luhn << std::endl;
    return 0;
}

```


```txt

true
false
false
true

```


## C#

The LuhnCheck method takes an array of integers because values in memory will be integer-aligned.


```c#

    public static class Luhn
    {
        public static bool LuhnCheck(this string cardNumber)
        {
            return LuhnCheck(cardNumber.Select(c => c - '0').ToArray());
        }

        private static bool LuhnCheck(this int[] digits)
        {
            return GetCheckValue(digits) == 0;
        }

        private static int GetCheckValue(int[] digits)
        {
            return digits.Select((d, i) => i % 2 == digits.Length % 2 ? ((2 * d) % 10) + d / 5 : d).Sum() % 10;
        }
    }

    public static class TestProgram
    {
        public static void Main()
        {
            long[] testNumbers = {49927398716, 49927398717, 1234567812345678, 1234567812345670};
            foreach (var testNumber in testNumbers)
                Console.WriteLine("{0} is {1}valid", testNumber, testNumber.ToString().LuhnCheck() ? "" : "not ");
        }
    }

```


```txt

49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid

```


Note that the original implementation, which follows, is flawed because it assumes that n is a number which, when represented as a string, has an even number of characters. Granted, the brief is for Credit Card Numbers which are all, at the time of writing, an even number of digits.


```c#
using System;
using System.Linq;

namespace Luhn
{
    class Program
    {
        public static bool luhn(long n)
        {
            long nextdigit, sum = 0;
            bool alt = false;
            while (n != 0)
            {
                nextdigit = n % 10;
                if (alt)
                {
                    nextdigit *= 2;
                    nextdigit -= (nextdigit > 9) ? 9 : 0;
                }
                sum += nextdigit;
                alt = !alt;
                n /= 10;
            }
            return (sum % 10 == 0);
        }

        public static bool luhnLinq(long n)
        {
            string s = n.ToString();
            return s.Select((c, i) => (c - '0') << ((s.Length - i - 1) & 1)).Sum(n => n > 9 ? n - 9 : n) % 10 == 0;
        }

        static void Main(string[] args)
        {
            long[] given = new long[] {49927398716, 49927398717, 1234567812345678, 1234567812345670};
            foreach (long num in given)
            {
                string valid = (luhn(num)) ? " is valid" : " is not valid";
                Console.WriteLine(num + valid);
            }

        }
    }
}
```


```txt

49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid

```


A solution without using LINQ, works for all versions of .NET.

```c#

using System;
namespace Luhn_Test
{
	public static class Extensions
	{
		public static string Reverse(this string s )
		{
		    char[] charArray = s.ToCharArray();
		    Array.Reverse( charArray );
		    return new string( charArray );
		}
	}
	class Program
	{
		public static bool Luhn(long x)
		{
			long s1=0;
			long s2=0;
			bool STATE=x%10!=0; // If it ends with zero, we want the order to be the other way around
			x=long.Parse(x.ToString().Reverse());
			while (x!=0)
			{
				s1+=STATE?x%10:0;
				s2+=STATE?0:((x%10)*2>9)?(((x%10)*2/10)+((x%10)*2)%10):((x%10)*2);
				STATE=!STATE; //Switch state
				x/=10; //Cut the last digit and continue
			}
			return ((s1+s2)%10==0); //Check if it ends with zero, if so, return true, otherwise,false.
		}
		public static void Main(string[] args)
		{
			long[] ks = {1234567812345670, 49927398717, 1234567812345678 ,1234567812345670 };
			foreach (long k in ks)
			{
			Console.WriteLine("{0} is {1} Valid.",k,Luhn(k)?"":"Not");
			}
		Start:
			try {
			Console.WriteLine("Enter your credit:");
			long x=long.Parse(Console.ReadLine());
			Console.WriteLine("{0} Valid.",Luhn(x)?"":"Not");
			goto Start;
			}
			catch (FormatException)
			{
				goto Start;
			}
		}
	}
}

```


```txt

1234567812345670 is Valid.
49927398717 is Not Valid.
1234567812345678 is Not Valid.
49927398716 is Valid.

```


A solution optimized for readability:

```c#

using System;
using System.Linq;

public class CreditCardLogic
{
    static Func<char, int> charToInt = c => c - '0';

    static Func<int, int> doubleDigit = n => (n * 2).ToString().Select(charToInt).Sum();

    static Func<int, bool> isOddIndex = index => index % 2 == 0;

    public static bool LuhnCheck(string creditCardNumber)
    {
        var checkSum = creditCardNumber
            .Select(charToInt)
            .Reverse()
            .Select((digit, index) => isOddIndex(index) ? digit : doubleDigit(digit))
            .Sum();

        return checkSum % 10 == 0;
    }
}

```


Extremely compact version uses Europa rtl library https://github.com/CodeAlkemist/Europa-rtl

```c#

using System;
using EuropaRTL.Utilities;

public static partial class Algoritmhs
{
    public static bool CheckLuhn(long n)
    {
        int s1 = n.Shatter(true).Subset(2).Arithmetic('+');
        int s2 = n.Shatter(true).Subset(1, -1, 2).ArithmeticRA('*', 2).ShatterAndSum().Arithmetic('+');
        return (s1 + s2) % 10 == 0 ? true : false;
    }
}
class Program
{
    static void Main(string[] args)
    {
        long[] ll = {
                49927398716,
                49927398717,
                1234567812345678,
                1234567812345670
            };
        foreach (var item in ll)
        {
            item.ToString().WriteLine();
            Algoritmhs.CheckLuhn(item).ToString().WriteLine();
        }
        Console.ReadKey();
    }
}

```


```txt

49927398716
True
49927398717
False
1234567812345678
False
1234567812345670
False

```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Check [ Abstract ]
{

ClassMethod Luhn(x As %String) As %Boolean
{
	// https://www.simple-talk.com/sql/t-sql-programming/calculating-and-verifying-check-digits-in-t-sql/
	SET x=$TRANSLATE(x," "), cd=$EXTRACT(x,*)
	SET x=$REVERSE($EXTRACT(x,1,*-1)), t=0
	FOR i=1:1:$LENGTH(x) {
		SET n=$EXTRACT(x,i)
		IF i#2 SET n=n*2 IF $LENGTH(n)>1 SET n=$EXTRACT(n,1)+$EXTRACT(n,2)
		SET t=t+n
	}
	QUIT cd=((t*9)#10)
}

}
```

```txt
USER>For  { Read ccn Quit:ccn=""  Write ": "_##class(Utils.Check).Luhn(ccn), ! }
49927398716: 1
49927398717: 0
1234567812345678: 0
1234567812345670: 1

USER>
```



## Ceylon


```ceylon
shared void run() {
	value numbers = "49927398716
	                 49927398717
	                 1234567812345678
	                 1234567812345670";
	for(number in numbers.lines) {
		print("``number`` passes? ``luhn(number)``");
	}
}

shared Boolean luhn(String number) {
	value digits = number
			.reversed
			.map(Character.string)
			.map(Integer.parse)
			.narrow<Integer>();
	value s1 = sum { 0, *digits.by(2) };
	value s2 = sum {
		0,
		*digits
			.skip(1)
			.by(2)
			.map(curry(times<Integer>)(2))
			.map((Integer element) => element / 10 + element % 10)
	};
	return (s1 + s2) % 10 == 0;
}
```



## Clojure


```clojure
(defn luhn? [cc]
  (let [factors (cycle [1 2])
        numbers (map #(Character/digit % 10) cc)
        sum (reduce + (map #(+ (quot % 10) (mod % 10))
                           (map * (reverse numbers) factors)))]
    (zero? (mod sum 10))))

(doseq [n ["49927398716" "49927398717" "1234567812345678" "1234567812345670"]]
  (println (luhn? n)))
```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LUHNTEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       data division.
       WORKING-STORAGE SECTION.
       01  inp-card.
         03  inp-card-ch      pic x(01) occurs 20 times.
       01  ws-result          pic 9(01).
         88  pass-luhn-test             value 0.

       PROCEDURE DIVISION.
           move "49927398716"       to inp-card
           perform test-card
           move "49927398717"       to inp-card
           perform test-card
           move "1234567812345678"  to inp-card
           perform test-card
           move "1234567812345670"  to inp-card
           perform test-card
           stop run
           .
       test-card.
           call "LUHN" using inp-card, ws-result
           if pass-luhn-test
             display "input=" inp-card "pass"
           else
             display "input=" inp-card "fail"
           .

       END PROGRAM LUHNTEST.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LUHN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  maxlen           pic 9(02) comp value 16.
       01  inplen           pic 9(02) comp value 0.
       01  i                pic 9(02) comp value 0.
       01  j                pic 9(02) comp value 0.
       01  l                pic 9(02) comp value 0.
       01  dw               pic 9(02) comp value 0.
       01  ws-total         pic 9(03) comp value 0.
       01  ws-prod          pic 99.
       01  filler redefines ws-prod.
         03  ws-prod-tens   pic 9.
         03  ws-prod-units  pic 9.
       01  ws-card.
         03  filler           occurs 16 times depending on maxlen.
           05  ws-card-ch     pic x(01).
           05  ws-card-digit redefines ws-card-ch  pic 9(01).
       LINKAGE SECTION.
       01  inp-card.
         03  inp-card-ch      pic x(01) occurs 20 times.
       01  ws-result          pic 9(01).
         88  pass-luhn-test             value 0.

       PROCEDURE DIVISION using inp-card, ws-result.
           perform varying i from 1 by +1
           until i > maxlen
           or    inp-card-ch (i) = space
           end-perform
           compute l = i - 1
           compute inplen = l
           perform varying j from 1 by +1
           until j > inplen
             if l < 1
               move "0"             to ws-card-ch (j)
             else
               move inp-card-ch (l) to ws-card-ch (j)
               compute l = l - 1
             end-if
           end-perform
           move 0 to ws-total
           perform varying i from 1 by +1
           until i > inplen
             compute dw = 2 - (i - 2 * function integer (i / 2))
             compute ws-prod = ws-card-digit (i) * dw
             compute ws-total = ws-total
                              + ws-prod-tens
                              + ws-prod-units
           end-perform
           compute ws-result = ws-total - 10 * function integer (ws-total / 10)
           goback
           .
       END PROGRAM LUHN.
```


```txt

input=49927398716         pass
input=49927398717         fail
input=1234567812345678    fail
input=1234567812345670    pass

```



## Common Lisp


```lisp
(defun luhn (n)
  (labels ((sum-digits (n) (if (> n 9) (- n 9) n)))
    (let ((n* (reverse n)) (l (length n)))
      (let ((s1 (loop for i from 0 below l by 2
		   summing (digit-char-p (aref n* i))))
	    (s2 (loop for i from 1 below l by 2
		   summing (sum-digits (* 2 (digit-char-p (aref n* i)))))))
	(zerop (mod (+ s1 s2) 10))))))
```

Another version, using Maciej Pasternacki's reader macros for
function composition and [[currying]] in the [http://www.cl-user.net/asp/Fr4F/sdataQ1nAQqjvnQ3MDQ3ESH8X8yBX8yBXnMq=/sdataQu3F$sSHnB== curly] package:

```lisp
(require :curly)
(use-package :curly)
(enable-curly-syntax)

(defun luhn (seq)
  (labels ((sum-digits (n) (if (> n 9) (- n 9) n)))
    (funcall {zerop (mod * 10) (apply #'+) (mapcar #'sum-digits)
	     (mapcar #'* '#1=(1 2 . #1#)) (map 'list #'digit-char-p) reverse} seq)))
```



## D


### Functional Version

```d
import std.algorithm, std.range, std.string;

enum luhnTest = (in string n) pure /*nothrow*/ @safe /*@nogc*/ =>
    retro(n)
    .zip(only(1, 2).cycle)
    .map!(p => (p[0] - '0') * p[1])
    .map!(d => d / 10 + d % 10)
    .sum % 10 == 0;

void main() {
    assert("49927398716 49927398717 1234567812345678 1234567812345670"
           .split.map!luhnTest.equal([true, false, false, true]));
}
```



### More Imperative Version

```d
import std.algorithm;

bool luhnTest(in string num) @safe pure nothrow @nogc {
    uint sum;
    foreach_reverse (immutable i, immutable n; num) {
        immutable uint ord = n - '\u0030';
        sum += ((num.length - i) & 1) ? ord : ord / 5 + (2 * ord) % 10;
    }
    return sum % 10 == 0;
}

void main() {
    immutable data = ["49927398716",
                      "49927398717",
                      "1234567812345678",
                      "1234567812345670"];
    assert(data.map!luhnTest.equal([true, false, false, true]));
}
```



### Stronger Statically Typed Version

This version uses more precise types.
```d
import std.stdio;

struct Interval(T) {
    immutable T a, b;

    this(in T a_, in T b_) pure nothrow @nogc {
        this.a = a_;
        this.b = b_;
    }

    bool opBinaryRight(string op="in")(in T x)
    const pure nothrow @nogc {
        return x >= a && x <= b;
    }

    pure nothrow @safe @nogc const invariant {
        assert(a <= b);
    }
}

Interval!T interval(T)(in T a, in T b) pure nothrow @nogc {
    return Interval!T(a, b);
}


bool luhnTest(in string num) pure nothrow @nogc
in {
    assert(num.length <= 20);
} body {
    int sum = 0;
    bool od = true;
    bool ok = true;
    immutable int numLen = num.length;

    foreach_reverse (immutable p; 0 .. numLen) {
        immutable int i = num[p] - '0';
        if (i !in interval(0, 9)) {
            ok = false;
            break;
        }

        immutable int x = ((i * 2) % 10) + (i / 5);
        assert((numLen - p) in interval(0, 19));
        assert(sum in interval(0, (numLen - p) * 10));
        assert(i in interval(0, 9));
        assert(x in interval(0, 9));
        sum += od ? i : x;
        od = !od;
    }

    return ok && (sum % 10) == 0;
}


void main() {
    foreach (immutable n; ["49927398716", "49927398717",
                           "1234567812345678", "1234567812345670",
                           "123456781234567D"])
        writefln("%s is %svalid", n, luhnTest(n) ? "" : "not ");
}
```



## EchoLisp


```lisp

;; value for 'even' numbers
(define (even-val n) (if (> n 4) (+ n n -9) (+ n n)))

;;Luhn test
;; input : a string of decimal digits
;; output #t or #f
(define (valid nums (odd #f ))
    (let ((nums (map string->number (reverse (string->list nums)))))
    (= 0 (modulo
       (for/sum ((n nums)) (set! odd (not odd)) (if odd n (even-val n)))
       10))))

(valid "49927398716") → #t
(valid "1234567812345670") → #t
(valid "1234567812345678") → #f
(valid "49927398717") → #f

```



## Elixir



```elixir
defmodule Luhn do
  def valid?(cc) when is_binary(cc), do: String.to_integer(cc) |> valid?
  def valid?(cc) when is_integer(cc) do
    0 == Integer.digits(cc)
         |> Enum.reverse
         |> Enum.chunk(2, 2, [0])
         |> Enum.reduce(0, fn([odd, even], sum) -> Enum.sum([sum, odd | Integer.digits(even*2)]) end)
         |> rem(10)
  end
end

numbers = ~w(49927398716 49927398717 1234567812345678 1234567812345670)
for n <- numbers, do: IO.puts "#{n}: #{Luhn.valid?(n)}"
```


```txt

49927398716: true
49927398717: false
1234567812345678: false
1234567812345670: true

```



## Emacs Lisp


```lisp

(require 'seq)
(defun luhn (str)
  "Check if an input string STR is valid using lhun algorithm."
  (if (string-match-p "[^0-9]" str)
      (error "String contains invalid character")
    (progn
      (let ((digit-list (reverse (mapcar #'(lambda (x) (- x 48))
                                         (string-to-list str)))))
        (zerop
         (mod (apply #'+ (seq-map-indexed
                          (lambda (elt idx)
                            (if (oddp idx)
                                (if (> (* 2 elt) 9)
                                    (- (* 2 elt) 9)
                                  (* 2 elt))
                              elt))
                          digit-list))
              10))))))



 (mapcar #'luhn '("49927398716" "49927398717" "1234567812345678" "1234567812345670"))

```


```txt
 (t nil nil t)
```



## Erlang


```Erlang

-module(luhn_test).

-export( [credit_card/1, task/0] ).

luhn_sum([Odd, Even |Rest]) when Even >= 5 ->
    Odd + 2 * Even - 10 + 1 + luhn_sum(Rest);
luhn_sum([Odd, Even |Rest]) ->
    Odd + 2 * Even + luhn_sum(Rest);
luhn_sum([Odd]) ->
    Odd;
luhn_sum([]) ->
    0.

check( Sum ) when (Sum rem 10) =:= 0 -> valid;
check( _Sum ) -> invalid.

credit_card(Digits) ->
    check(luhn_sum(lists:map(fun(D) -> D-$0 end, lists:reverse(Digits)))).

task() ->
    Numbers = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"],
    [io:fwrite("~s: ~p~n", [X, credit_card(X)]) || X <- Numbers].

```


```txt

16> luhn_test:task().
49927398716: valid
49927398717: invalid
1234567812345678: invalid
1234567812345670: valid

```



## Euphoria

```euphoria
function luhn(sequence cc)
    integer isOdd, oddSum, evenSum, digit
    isOdd = 1
    oddSum = 0
    evenSum = 0
    for i = length(cc) to 1 by -1 do
        digit = cc[i] - '0'
        if isOdd then
            oddSum += digit
        else
            evenSum += floor(digit / 5) + remainder(2 * digit, 10)
        end if
        isOdd = not isOdd
    end for
    return not remainder(oddSum + evenSum, 10)
end function

constant cc_numbers = {
    "49927398716",
    "49927398717",
    "1234567812345678",
    "1234567812345670"
}

for i = 1 to length(cc_numbers) do
    printf(1,"%s = %d\n", {cc_numbers[i], luhn(cc_numbers[i])})
end for
```


```txt
49927398716 = 1
49927398717 = 0
1234567812345678 = 0
1234567812345670 = 1
```


=={{header|F Sharp|F#}}==

```fsharp
let luhn (s:string) =
  let rec g r c = function
  | 0 -> r
  | i ->
      let d = ((int s.[i - 1]) - 48) <<< c
      g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)
  (g 0 0 s.Length) % 10 = 0
```



## Factor

```factor
USING: kernel math math.parser math.order math.ranges sequences ;
IN: luhn

: reversed-digits ( n -- list )
    { } swap
    [ dup 0 > ]
        [ 10 /mod  swapd suffix  swap ]
    while drop ;

: luhn-digit  ( n -- n )
    reversed-digits dup length <iota> [
        2dup swap nth
        swap odd? [ 2 *  10 /mod + ] when
    ] map sum 10 mod
    nip ;

: luhn? ( n -- ? )
    luhn-digit 0 = ;

```


```txt

( scratchpad ) 49927398716 luhn? .
t
( scratchpad ) 49927398717 luhn? .
f
( scratchpad ) 1234567812345678 luhn? .
f
( scratchpad ) 1234567812345670 luhn? .
t

```



## Forth


```forth
: luhn ( addr len -- ? )
  0 >r over +             ( R: sum )
  begin  1- 2dup <=
  while                   \ odd
         dup c@ [char] 0 -
         r> + >r
         1- 2dup <=
  while                   \ even
         dup c@ [char] 0 -
         2* 10 /mod +     \ even digits doubled, split, and summed
         r> + >r
  repeat then
  2drop  r> 10 mod 0= ;

s" 49927398716"      luhn .   \ -1
s" 49927398717"      luhn .   \ 0
s" 1234567812345678" luhn .   \ 0
s" 1234567812345670" luhn .   \ -1
```



## Fortran


```fortran
program luhn
  implicit none
  integer              :: nargs
  character(len=20)    :: arg
  integer              :: alen, i, dr
  integer, allocatable :: number(:)
  integer, parameter   :: drmap(0:9) = [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]

  ! Get number
  nargs = command_argument_count()
  if (nargs /= 1) then
     stop
  end if
  call get_command_argument(1, arg, alen)
  allocate(number(alen))
  do i=1, alen
     number(alen-i+1) = iachar(arg(i:i)) - iachar('0')
  end do

  ! Calculate number
  dr = 0
  do i=1, alen
     dr = dr + merge(drmap(number(i)), number(i), mod(i,2) == 0)
  end do

  if (mod(dr,10) == 0) then
     write(*,'(a,i0)') arg(1:alen)//' is valid'
  else
     write(*,'(a,i0)') arg(1:alen)//' is not valid'
  end if
end program luhn

! Results:
! 49927398716 is valid
! 49927398717 is not valid
! 1234567812345678 is not valid
! 1234567812345670 is valid
```



## FreeBASIC


```FreeBASIC
' version 05-07-2015
' compile with: fbc -s console

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function luhntest(cardnr As String) As Integer

    cardnr = Trim(cardnr) ' we don't want spaces
    Dim As String reverse_nr = cardnr
    Dim As Integer i, j, s1, s2, l = Len(cardnr) - 1

    ' reverse string
    For i = 0 To l
        reverse_nr[i] = cardnr[l - i]
    Next
    ' sum odd numbers
    For i = 0 To l Step 2
        s1 = s1 + (reverse_nr[i] - Asc("0"))
    Next
    ' sum even numbers
    For i = 1 To l Step 2
        j = reverse_nr[i] - Asc("0")
        j = j * 2
        If j > 9 Then j = j Mod 10 + 1
        s2 = s2 + j
    Next

    If (s1 + s2) Mod 10 = 0 Then
        Return TRUE
    Else
        Return FALSE
    End If

End Function

' ------=< MAIN >=------

Dim As String input_nr(1 To ...) = {"49927398716", "49927398717",_
                          "1234567812345678", "1234567812345670"}
Dim As Integer a

Print  "Task test number 49927398716 should be TRUE, report back as ";
Print IIf(luhntest("49927398716" ) = TRUE, "TRUE", "FALSE")
Print : Print

Print "test card nr:"
For a = 1 To UBound(input_nr)
    Print input_nr(a); " = "; IIf(luhntest(input_nr(a)) = TRUE, "TRUE", "FALSE")
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Task test number 49927398716 should be TRUE, report back as TRUE

test card nr:
49927398716 = TRUE
49927398717 = FALSE
1234567812345678 = FALSE
1234567812345670 = TRUE
```



## FunL


```funl
def luhn_checksum( card_number ) =
  def digits_of( n ) = [int(d) | d <- n.toString()]

  digits = digits_of( card_number ).reverse()
  odd_digits = digits(0:digits.length():2)
  even_digits = digits(1:digits.length():2)
  (sum( odd_digits ) + sum( sum(digits_of(d*2)) | d <- even_digits )) mod 10

def is_luhn_valid( card_number ) = luhn_checksum( card_number ) == 0

for n <- [49927398716, 49927398717, 1234567812345678, 1234567812345670]
  println( n + ' is ' + (if is_luhn_valid(n) then 'valid' else 'invalid') )
```


```txt

49927398716 is valid
49927398717 is invalid
1234567812345678 is invalid
1234567812345670 is valid

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=966f04aa2a1e9af43ba9553f5cb9160d Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sTrial As String[] = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
Dim sRev As String
Dim siCount, siOdd, siEven, siHold, siQty As Short

For siQty = 0 To sTrial.Max

  siOdd = 0
  siEven = 0
  sRev = ""

  For siCount = Len(sTrial[siQty]) DownTo 1
    sRev &= Mid(sTrial[siQty], siCount, 1)
  Next

  For siCount = 1 To Len(sRev)
    If Odd(siCount) Then siOdd += Val(Mid(sRev, siCount, 1))
    If Even(siCount) Then
      siHold = Val(Mid(sRev, siCount, 1)) * 2
      If siHold > 9 Then
        siEven += Val(Mid(Str(siHold), 1, 1)) + Val(Mid(Str(siHold), 2, 1))
      Else
        siEven += Val(Mid(sRev, siCount, 1)) * 2
      End If
    End If
  Next

  sRev = Str(siOdd + siEven)
  If sRev Ends "0" Then
    Print sTrial[siQty] & " is a valid number"
  Else
    Print sTrial[siQty] & " is NOT a valid number"
  End If
Next

End
```

Output:

```txt

49927398716 is a valid number
49927398717 is NOT a valid number
1234567812345678 is NOT a valid number
1234567812345670 is a valid number

```



## GAP


```gap
IsLuhn := function(n)
	local c, d, i, j, r;
	d := "0123456789";
	j := 1;
	r := 0;
	for c in Reversed(String(n)) do
		i := Position(d, c);
		if i = fail then
			continue;
		fi;
		i := j*(i - 1);
		r := r + QuoInt(i, 10) + RemInt(i, 10);
		j := 3 - j;
	od;
	return RemInt(r, 10) = 0;
end;

List([49927398716, 49927398717, 1234567812345678, 1234567812345670], IsLuhn);
# [ true, false, false, true ]

# Will also work on strings, and will skip non-digits
IsLuhn("4-992-739-871-6");
# true
```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

const input = `49927398716
49927398717
1234567812345678
1234567812345670`

var t = [...]int{0, 2, 4, 6, 8, 1, 3, 5, 7, 9}

func luhn(s string) bool {
    odd := len(s) & 1
    var sum int
    for i, c := range s {
        if c < '0' || c > '9' {
            return false
        }
        if i&1 == odd {
            sum += t[c-'0']
        } else {
            sum += int(c - '0')
        }
    }
    return sum%10 == 0
}

func main() {
    for _, s := range strings.Split(input, "\n") {
        fmt.Println(s, luhn(s))
    }
}
```

```txt

49927398716 true
49927398717 false
1234567812345678 false
1234567812345670 true

```



## Groovy


```groovy
def checkLuhn(number) {
    int total
    (number as String).reverse().eachWithIndex { ch, index ->
        def digit = Integer.parseInt(ch)
        total += (index % 2 ==0) ? digit : [0, 2, 4, 6, 8, 1, 3, 5, 7, 9][digit]
    }
    total % 10 == 0
}
```

Testing the function:

```groovy
def verifyLuhn(number, expected) {
    println "Checking: $number (${checkLuhn(number)})"
    assert expected == checkLuhn(number)
}

[49927398716: true, 49927398717: false, 1234567812345678: false, 1234567812345670: true].each { number, expected ->
    verifyLuhn number, expected
}
```

```txt
Checking: 49927398716 (true)
Checking: 49927398717 (false)
Checking: 1234567812345678 (false)
Checking: 1234567812345670 (true)
```




## Haskell


```haskell
import Data.Char (digitToInt)
luhn = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) .
       zipWith (*) (cycle [1,2]) . map digitToInt . reverse
```


```haskell
map luhn ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
[True,False,False,True]
```


Or, aiming for a legible relationship with the stages shown in the task description:

```haskell
import Data.Char (digitToInt)

luhn :: String -> Bool
luhn x = 0 == rem (s1 + s2) 10
  where
    stringInts = fmap digitToInt
    (odds, evens) = oddsEvens (stringInts $ reverse x)
    s1 = sum odds
    s2 = sum $ sum . stringInts . show . (2 *) <$> evens

oddsEvens :: [a] -> ([a], [a])
oddsEvens xs = foldr go ([], []) (zip xs [1 ..])
  where
    go (x, i) (os, es)
      | 0 /= rem i 2 = (x : os, es)
      | otherwise = (os, x : es)

main :: IO ()
main =
  mapM_
    (print . ((,) <*> luhn))
    ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
```

```txt
("49927398716",True)
("49927398717",False)
("1234567812345678",False)
("1234567812345670",True)
```



## HicEst


```HicEst
CHARACTER numbers="49927398716 49927398717 1234567812345678 1234567812345670 "

DO nr = 1, 4
   EDIT(Text=numbers, ITeM=nr, Parse=number)
   sum_odds = 0
   sum_even = 0
   DO i = LEN(number), 1, -2
      sum_odds = sum_odds + ICHAR(number(i)) - 48
      IF(i > 1) THEN
         even2 = 2 * (ICHAR(number(i-1)) - 48)
         sum_even = sum_even + MOD(even2, 10) + INT(even2/10)
      ENDIF
   ENDDO
   valid = (0 == MOD(sum_odds + sum_even, 10))
   WRITE() number, " is ", "invalid"(1 + 2*valid:)
ENDDO
```


```txt
49927398716 is valid
49927398717 is invalid
1234567812345678 is invalid
1234567812345670 is valid
```


=={{header|Icon}} and {{header|Unicon}}==
We use map to pre-compute the sum of doubled digits.

```icon
procedure main(aL)
every write(i := !aL ," - ", ((\isluhn10(i),"valid")|"invalid") \ 1)
end

procedure isluhn10(i)  #: isluhn10(i) returns i (if i passes luhn10) or fails
local sum

sum :=0
reverse(integer(i)) ? while not pos(0) do {
      sum +:= move(1)
      sum +:= map(move(1),"0123456789","0246813579")
   }

return (sum % 10 = 0,i)
end
```


```txt

# luhn10 49927398716 49927398717 1234567812345678 1234567812345670

49927398716 - valid
49927398717 - invalid
1234567812345678 - invalid
1234567812345670 - valid

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "CredCard.bas"
110 DO
120   PRINT :PRINT "Credit card number:":INPUT PROMPT ">":CCN$
130   IF CCN$="" THEN EXIT DO
140   IF LUHN(TRIM$(CCN$)) THEN
150     PRINT "Card number is valid."
160   ELSE
170     SET #102:INK 3:PRINT "Card number is invalid.":SET #102:INK 1
180   END IF
190 LOOP
200 DEF LUHN(CCN$)
210   LET L=LEN(CCN$):LET S=0
220   FOR I=1 TO L
230     LET N=VAL(CCN$(L-I+1))
240     IF I BAND 1 THEN
250       LET S=S+N
260     ELSE
270       LET N=N*2:LET S=S+MOD(N,10)+INT(N/10)
280     END IF
290   NEXT
300   LET LUHN=MOD(S,10)=0
310 END DEF
320 DEF TRIM$(S$)
330   LET T$=""
340   FOR I=1 TO LEN(S$)
350     IF S$(I)>CHR$(47) AND S$(I)<CHR$(58) THEN LET T$=T$&S$(I)
360   NEXT
370   LET TRIM$=T$
380 END DEF
```


Output:

```txt
Credit card number:
>49927398716
Card number is valid.

Credit card number:
>49927398717
Card number is invalid.

Credit card number:
>1234 5678 1234 5678
Card number is invalid.

Credit card number:
>1234 5678 1234 5670
Card number is valid.
```



## J


We can treat the odd digits the same as even digits,
except that they are not doubled.
Also, we do not need the intermediate sums.


```J
luhn=: 0 = 10 (| +/@,) 10 #.inv 1 2 *&|: _2 "."0\ |.
```


Example use:

```J
   luhn&> '49927398716';'49927398717';'1234567812345678';'1234567812345670'
1 0 0 1
```


Interpreting that example:  In J, 1 is true, 0 is false, so the first and last provided digit sequences were valid and the middle two were not.


## Java


```java
public class Luhn {
    public static void main(String[] args) {
        System.out.println(luhnTest("49927398716"));
        System.out.println(luhnTest("49927398717"));
        System.out.println(luhnTest("1234567812345678"));
        System.out.println(luhnTest("1234567812345670"));
    }

    public static boolean luhnTest(String number){
        int s1 = 0, s2 = 0;
        String reverse = new StringBuffer(number).reverse().toString();
        for(int i = 0 ;i < reverse.length();i++){
            int digit = Character.digit(reverse.charAt(i), 10);
            if(i % 2 == 0){//this is for odd digits, they are 1-indexed in the algorithm
                s1 += digit;
            }else{//add 2 * digit for 0-4, add 2 * digit - 9 for 5-9
                s2 += 2 * digit;
                if(digit >= 5){
                    s2 -= 9;
                }
            }
        }
        return (s1 + s2) % 10 == 0;
    }
}
```

```txt
true
false
false
true
```



## JavaScript

Using prototype.

```javascript
mod10check = function(cc) {
  return $A(cc).reverse().map(Number).inject(0, function(s, d, i) {
    return s + (i % 2 == 1 ? (d == 9 ? 9 : (d * 2) % 9) : d);
  }) % 10 == 0;
};
['49927398716','49927398717','1234567812345678','1234567812345670'].each(function(i){alert(mod10check(i))});
```


Without any library.

```javascript
var LuhnCheck = (function()
{
	var luhnArr = [0, 2, 4, 6, 8, 1, 3, 5, 7, 9];
	return function(str)
	{
		var counter = 0;
		var incNum;
		var odd = false;
		var temp = String(str).replace(/[^\d]/g, "");
		if ( temp.length == 0)
			return false;
		for (var i = temp.length-1; i >= 0; --i)
		{
			incNum = parseInt(temp.charAt(i), 10);
			counter += (odd = !odd)? incNum : luhnArr[incNum];
		}
		return (counter%10 == 0);
	}
})();
```


Highly compressed version.

```javascript
var luhn10 = function(a,b,c,d,e) {
  for(d = +a[b = a.length-1], e=0; b--;)
    c = +a[b], d += ++e % 2 ? 2 * c % 10 + (c > 4) : c;
  return !(d%10)
};

// returns true
luhn10('4111111111111111')

// returns false
luhn10('4111111111111112')

```



## jq


```jq
def luhn:
  def odds: . as $in | reduce range(0; length) as $i
    ([]; if ($i % 2) == 0 then . + [$in[$i]] else . end);
  def evens: . as $in | reduce range(1; length) as $i
    ([]; if ($i % 2) == 1 then . + [$in[$i]] else . end);
  def digits: map([.]|implode|tonumber);
  def sumdigits: tostring | explode | digits | add;

  (tostring | explode | reverse ) as $reverse
  | ($reverse | odds  | digits | add) as $s1
  | ($reverse | evens | digits | map(. * 2 | sumdigits) | add) as $s2
  | 0 == ($s1 + $s2) % 10 ;
```

'''Example'''

```jq
  ( 49927398716,
    49927398717,
    1234567812345678,
    1234567812345670
  ) | "\(.) => \(luhn)";
```

 $ jq -r -M -n -f luhn.jq
 49927398716 => true
 49927398717 => false
 1234567812345678 => false
 1234567812345670 => true


## Julia

The test function itself is only a single line of code:


```Julia
luhntest(x::Integer) = (sum(digits(x)[1:2:end]) + sum(map(x -> sum(digits(x)), 2 * digits(x)[2:2:end]))) % 10 == 0
```


More readable version:


```julia
function luhntest(x::Integer)
    d = reverse(digits(x))
    s = sum(d[1:2:end])
    s += sum(sum.(digits.(2d[2:2:end])))
    return s % 10 == 0
end

for card in [49927398716, 49927398717, 1234567812345678, 1234567812345670]
    println(luhntest(card) ? "PASS " : "FAIL ", card)
end
```


```txt
PASS 49927398716
FAIL 49927398717
FAIL 1234567812345678
PASS 1234567812345670
```



## K


```k
so: {+/x@2*!_ceil(#x)%2}
se: {+/{+/0$'$x}'2*x@1+2*!(#x)%2}
luhn: {n:|0$'$x; 0=((se n)+so n)!10}
```


```k
luhn2: {~(+/,//10_vs'1 2*+-1 2#((#n)!2){x,0}/n:0$'|$x)
```


'''Example:'''


```k
  luhn'49927398716 49927398717 1234567812345678 1234567812345670
1 0 0 1
  luhn2'49927398716 49927398717 1234567812345678 1234567812345670
1 0 0 1
```


```k
luhn:{[cc]
    digits:0$/:|cc / convert chars to digit ints
    s:digits*(#cc)#1 2 / evens doubled, odds not
    nines:+/s>9 / number of sums greater than 9
    :~((+/s)-(9*nines))!10 / sum minus the nines is mod ten?
    }
```

'''Example:'''


```k
  test:("49927398716";"49927398717";"1234567812345678";"1234567812345670")
  luhn'test
1 0 0 1
```



## Kotlin


```scala
// version 1.1

fun luhn(s: String): Boolean {
    fun sumDigits(n : Int) =  n / 10 + n % 10
    val  t = s.reversed()
    val s1 = t.filterIndexed { i, _ -> i % 2 == 0 }.sumBy { it - '0' }
    val s2 = t.filterIndexed { i, _ -> i % 2 == 1 }.map { sumDigits((it - '0') * 2) }.sum()
    return (s1 + s2) % 10 == 0
}

fun main(args: Array<String>) {
    val numbers = arrayOf("49927398716", "49927398717", "1234567812345678", "1234567812345670")
    for (number in numbers) println("${number.padEnd(16)} is ${if(luhn(number)) "valid" else "invalid"}")
}
```


```txt

49927398716      is valid
49927398717      is invalid
1234567812345678 is invalid
1234567812345670 is valid

```



## Langur

```Langur
val .t = [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]

val .luhntest = f(.s) {
    val .numbers = map f .c-'0', s2cp .s
    val .oddeven = len(.numbers) rem 2

    foldfrom(
        f(.sum, .i, .c) if .i rem 2 == .oddeven { .sum + .c } else { .sum + .t[.c+1] },
        0,
        pseries len .numbers,
        .numbers,
    ) rem 10 == 0
}

val .tests = h{
    "49927398716": true,
    "49927398717": false,
    "1234567812345678": false,
    "1234567812345670": true,
}

for .key in sort(len, keys .tests) {
    val .pass = .luhntest(.key)
    write .key, ": ", .pass
    writeln if(.pass == .tests[.key]: ""; " (LUHN TEST FAILED)")
}
```


```txt
49927398717: false
49927398716: true
1234567812345678: false
1234567812345670: true
```



## Lasso

Part of the Lasso's implementation of "valid_creditcard".

```lasso
#!/usr/bin/lasso9

define luhn_check(number) => {
  local(
    rev = #number->asString,
    checksum = 0
  )
  #rev->reverse
  iterate(#rev, local(digit)) => {
    if((loop_count % 2) == 0) => {
      #checksum += (2 * integer(#digit))
      integer(#digit) >= 5 ? #checksum -= 9
    else
      #checksum += integer(#digit)
    }
  }
  (#checksum % 10) != 0 ? return false
  return true
}

stdoutnl(luhn_check(49927398716))       // true
stdoutnl(luhn_check(49927398717))       // false
stdoutnl(luhn_check(1234567812345678))  // false
stdoutnl(luhn_check(1234567812345670))  // true
```



## Liberty BASIC


```lb
' [RC] Luhn test

card$(1)="49927398716"
card$(2)="49927398717"
card$(3)="1234567812345678"
card$(4)="1234567812345670"

for test = 1 to 4
    odd=1
    sum = 0
    for n = len(card$(test)) to 1 step -1
        num=val(mid$(card$(test),n,1))
        if odd then
            sum = sum + num
            odd=0
        else
            num=num*2
            if num<=9 then
                sum = sum + num
            else
                sum = sum + val(left$(str$(num),1)) + val(right$(str$(num),1))
            end if
            odd=1
        end if
    next
    if sum mod 10 = 0 then
        print card$(test),"True"
    else
        print card$(test),"False"
    end if
next
```



## LiveCode


```LiveCode
function LuhnTest cc
    local s1,evens, s2
    repeat with n = 1 to len(cc)
        if n mod 2 is not 0 then
            add (char -n of cc) to s1
        else
            put (char -n of cc) * 2 into evens
            if evens > 9 then subtract 9 from evens
            add evens to s2
        end if
    end repeat
    return the last char of (s1 + s2) is 0
end LuhnTest

-- test
repeat for each item ccno in "49927398716,49927398717,1234567812345678,1234567812345670"
    put ccno && LuhnTest(ccno) & cr after luhncheck
end repeat
put luhncheck

49927398716 true
49927398717 false
1234567812345678 false
1234567812345670 true

```



## Logo


```logo
to small? :list
  output or [empty? :list] [empty? bf :list]
end
to every.other :list
  if small? :list [output :list]
  output fput first :list every.other bf bf :list
end
to wordtolist :word
  output map.se [?] :word
end

to double.digit :digit
  output item :digit {0 2 4 6 8 1 3 5 7 9}@0
  ; output ifelse :digit < 5 [2*:digit] [1 + modulo 2*:digit 10]
end

to luhn :credit
  localmake "digits reverse filter [number? ?] wordtolist :credit
  localmake "s1 apply "sum every.other :digits
  localmake "s2 apply "sum map "double.digit every.other bf :digits
  output equal? 0 last sum :s1 :s2
end

show luhn "49927398716          ; true
show luhn "49927398717          ; false
show luhn "1234-5678-1234-5678  ; false
show luhn "1234-5678-1234-5670  ; true
```



## Lua


```Lua
function luhn(n)
  n=string.reverse(n)
  print(n)
  local s1=0
  --sum odd digits
  for i=1,n:len(),2 do
    s1=s1+n:sub(i,i)
  end
  --evens
  local s2=0
  for i=2,n:len(),2 do
    local doubled=n:sub(i,i)*2
    doubled=string.gsub(doubled,'(%d)(%d)',function(a,b)return a+b end)
    s2=s2+doubled
  end
  print(s1)
  print(s2)
  local total=s1+s2
  if total%10==0 then
    return true
  end
  return false
end

-- Note that this function takes strings, not numbers.
-- 16-digit numbers tend to be problematic
-- when looking at individual digits.
print(luhn'49927398716')
print(luhn'49927398717')
print(luhn'1234567812345678')
print(luhn'1234567812345670')
```



## M2000 Interpreter

```M2000 Intertrpeter

Module Checkit {
      Function luhntest(cardnr$) {

            cardnr$ = Trim$(cardnr$) ' we don't want spaces
            if len(cardnr$)=0 then exit
            Dim Base 0,  reverse_nr$(Len(cardnr$))
            Def integer  i, j, s1, s2, l , l2
            Let l=Len(cardnr$)-1, l2=l+1

            ' reverse string
            For i = 0 To l
                reverse_nr$(i) = mid$(cardnr$,l2-i,1)
            Next i
            ' sum odd numbers
            For i = 0 To l Step 2
                s1 = s1 + (Asc(reverse_nr$(i)) - Asc("0"))
            Next i
            ' sum even numbers
            For i = 1 To l Step 2
                j = Asc(reverse_nr$(i)) - Asc("0")
                j = j * 2
                If j > 9 Then j = j Mod 10 + 1
                s2 = s2 + j
            Next i

            If (s1 + s2) Mod 10 = 0 Then
                = 1=1
            Else
                = 1=0
            End If
      }
      Flush
      Data "49927398716",  "49927398717", "1234567812345678", "1234567812345670"
      while not empty
            over
            print letter$;" = ";luhntest(letter$)
      end while
}
Checkit

```

```txt
49927398716 = True
49927398717 = False
1234567812345678 = False
1234567812345670 = True
```



## Mathematica


```Mathematica
LuhnQ[nb_] := (Mod[Total[(2*ToExpression[#[[2;;All;;2]]]) /. {z_?(Function[v, v>9]) -> z-9}]
       + Total[ToExpression[#[[1;;All;;2]]]], 10] == 0)& [Characters[StringReverse[ToString[nb]]] ]

LuhnQ /@ {49927398716, 49927398717, 1234567812345678, 1234567812345670}
->{True, False, False, True}
```


### Alternate Code

Eliminates conversion of numbers to strings and back

```Mathematica
LuhnQ[n_Integer] :=
 Block[{digits = Reverse@IntegerDigits@n},
  Mod[Total[{digits[[;; ;; 2]],
      IntegerDigits[2 #] & /@ digits[[2 ;; ;; 2]]}, -1], 10] == 0]

LuhnQ /@ {49927398716, 49927398717, 1234567812345678, 1234567812345670}
```

```txt
{True,False,False,True}
```



## MATLAB

The solution is basically the same as for [[#Octave|Octave]].

```MATLAB
function passed = luhn(num)
if nargin == 0 % evaluate test cases
  testnum = [49927398716 49927398717 1234567812345678 1234567812345670];
  for num = testnum
    disp([int2str(num) ': ' int2str(luhn(num))])
  end
  return
end
% luhn function starts here
d = int2str(num) - '0';	% convert number into vector of digits
m = [2:2:8,1:2:9];	% rule 3: maps 1:9 to [2 4 6 8 1 3 5 7 9]
passed = ~mod(sum(d(end:-2:1)) + sum(m(d(end-1:-2:1))), 10);
end
```

```txt
49927398716: 1
49927398717: 0
1234567812345678: 0
1234567812345670: 1
```



## min

```min
((dup 10 <) 'quote (((10 mod) (10 div)) cleave) 'cons linrec) :digits
((0 0) dip (pop 'succ dip over even?) partition ((pop pop) dip) dip) :evens/odds
((2 * digits sum) map sum) :evens-sum
(digits evens/odds ('evens-sum 'sum) => spread + 10 mod 0 ==) :luhn?

(49927398716 49927398717 1234567812345678 1234567812345670)
(dup print! " " print! luhn? puts!) foreach
```

```txt

49927398716 true
49927398717 false
1234567812345678 false
1234567812345670 true

```



## MiniScript


```MiniScript
isValid = function(s)
    sum = 0
    odd = true
    for i in range(s.len-1)
        d = val(s[i])
        sum = sum + d * (2 - odd)
        if not odd and d > 4 then sum = sum - 9
        odd = not odd
    end for
    return sum % 10 == 0
end function

test = function(s)
    if isValid(s) then print s + ": valid" else print s + ": invalid"
end function

test "49927398716"
test "49927398717"
test "1234567812345678"
test "1234567812345670"
```


```txt
49927398716: valid
49927398717: invalid
1234567812345678: invalid
1234567812345670: valid
```




## MUMPS


```mumps
LUHN(C)
 NEW ODD,EVEN,S
 SET S=$REVERSE(C)
 SET ODD=0 FOR I=1:2:$LENGTH(S) SET ODD=ODD+$EXTRACT(S,I)
 SET EVEN=0 FOR I=2:2:$LENGTH(S) SET T=$EXTRACT(S,I)*2 SET EVEN=EVEN+$SELECT(T<=9:T,T>9:$EXTRACT(T,1)+$EXTRACT(T,2))
 QUIT '((ODD+EVEN)#10)
```


```txt
USER>W !,$S($$LUHN^ROSETTA("49927398716")=0:"INVALID",1:"VALID")

VALID
USER>W !,$S($$LUHN^ROSETTA("49927398717")=0:"INVALID",1:"VALID")

INVALID
USER>W !,$S($$LUHN^ROSETTA("1234567812345678")=0:"INVALID",1:"VALID")

INVALID
USER>W !,$S($$LUHN^ROSETTA("1234567812345670")=0:"INVALID",1:"VALID")

VALID
```



## NetRexx

```netrexx

class LuhnTest

  method main(args=String[]) static
    cc	  = 0
    cc[1] = '49927398716'
    cc[2] = '49927398717'
    cc[3] = '1234567812345678'
    cc[4] = '1234567812345670'

    loop k=1 while cc[k] <> 0
      r = checksum(cc[k])
      if r==0 then say cc[k].right(20) 'passed'
      else say cc[k].right(20) 'failed'
    end

    -- Luhn algorithm checksum for credit card numbers
  method checksum(t) static
    if t.length()//2 then t = '0't  --pad # on left with 0
    t = t.reverse()
    s = 0
    loop j = 1 to t.length()-1 by 2
      q = 2*t.substr(j+1,1)
      if q>9 then q = q.left(1) + q.right(1)
      s= s+t.substr(j,1)+q
    end
    return s//10\==0

```



## Nim


```nim
proc luhn(cc): bool =
  const m = [0,2,4,6,8,1,3,5,7,9]
  var sum = 0
  var odd = true
  for i in countdown(cc.high, 0):
    let digit = ord(cc[i]) - ord('0')
    sum += (if odd: digit else: m[digit])
    odd = not odd
  sum mod 10 == 0

for cc in ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]:
  echo cc," ",luhn(cc)
```

```txt
49927398716 true
49927398717 false
1234567812345678 false
1234567812345670 true
```



## Objeck


```objeck
bundle Default {
  class Luhn {
    function : IsValid(cc : String) ~ Bool {
      isOdd := true;
      oddSum := 0;
      evenSum := 0;

      for(i := cc->Size() - 1; i >= 0; i -= 1;) {
        digit : Int := cc->Get(i) - '0';
        if(isOdd) {
          oddSum += digit;
        }
        else {
          evenSum += digit / 5 + (2 * digit) % 10;
        };
        isOdd := isOdd <> true;
      };

      return (oddSum + evenSum) % 10 = 0;
    }

    function : Main(args : String[]) ~ Nil {
      IsValid("49927398716")->PrintLine();
      IsValid("49927398717")->PrintLine();
      IsValid("1234567812345678")->PrintLine();
      IsValid("1234567812345670")->PrintLine();
    }
  }
}
```


=={{header|Objective-C}}==

```objc
- (NSArray *) toCharArray {

	NSMutableArray *characters = [[NSMutableArray alloc] initWithCapacity:[self length]];
	for (int i=0; i < [self length]; i++) {
		NSString *ichar  = [NSString stringWithFormat:@"%C", [self characterAtIndex:i]];
		[characters addObject:ichar];
	}

	return characters;
}

+ (BOOL) luhnCheck:(NSString *)stringToTest {

	NSArray *stringAsChars = [stringToTest toCharArray];

	BOOL isOdd = YES;
	int oddSum = 0;
	int evenSum = 0;

	for (int i = [stringToTest length] - 1; i >= 0; i--) {

		int digit = [(NSString *)stringAsChars[i] intValue];

		if (isOdd)
			oddSum += digit;
		else
			evenSum += digit/5 + (2*digit) % 10;

		isOdd = !isOdd;
	}

	return ((oddSum + evenSum) % 10 == 0);
}

BOOL test0 = [self luhnCheck:@"49927398716"]; //Result = YES
BOOL test1 = [self luhnCheck:@"49927398717"]; //Result = NO
BOOL test2 = [self luhnCheck:@"1234567812345678"]; //Result = NO
BOOL test3 = [self luhnCheck:@"1234567812345670"]; //Result = YES
```



## OCaml


```ocaml
let luhn s =
  let rec g r c = function
  | 0 -> r
  | i ->
      let d = c * ((int_of_char s.[i-1]) - 48) in
      g (r + (d/10) + (d mod 10)) (3-c) (i-1)
  in
  (g 0 1 (String.length s)) mod 10 = 0
;;
```


```ocaml
# List.map luhn [ "49927398716"; "49927398717"; "1234567812345678"; "1234567812345670" ];;
- : bool list = [true; false; false; true]
```




## Octave


```Octave
   function y = isluhn(s);
	if isnumeric(s) s = mat2str(s); end; 	% make sure s is a string
	d = s-'0';	% convert string into vector of digits
	m = [2:2:8,1:2:9];	% rule 3: maps [1:9] -> i
	y = ~mod(sum(d(end:-2:1)) + sum(m(d(end-1:-2:1))),10);
   end;
```


```octave
  isluhn('49927398716')
	ans =  1
  isluhn('49927398717')
	ans = 0
  isluhn('1234567812345678')
	ans = 0
  isluhn('1234567812345670')
	ans =  1

```



## Oforth



```Oforth
: luhnTest(n)
| s i |
   n asString reverse ->s
   0 s size loop: i [
      i s at asDigit
      i isEven ifTrue: [ 2 * dup 10 >= ifTrue: [ 9 - ] ] +
      ]
   10 mod ==0 ;
```


```txt

[ 49927398716, 49927398717, 1234567812345678, 1234567812345670 ] map(#luhnTest) println
[1, 0, 0, 1]

```



## OpenEdge/Progress


```progress
FUNCTION fnLuhnAlgorithm RETURNS LOGICAL
  (INPUT pcNumber AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:  Applies Luhn Algorithm to check a Number
    Notes:  Returns True/False Validation based on check digit
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cNum        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCheck      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoopCnt    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNum        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNum1       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNum2       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTestLength AS INTEGER   NO-UNDO.

    ASSIGN
      iLength     = LENGTH(pcNumber)
      iTestLength = iLength - 1
        iCheck = 1. /* 1 for the check digit we skip */

    DO iLoopCnt = iTestLength TO 1 BY -1:
      ASSIGN
          iNum = INTEGER(SUBSTR(pcNumber,iLoopCnt,1))
          iCheck = iCheck + 1.

      IF iCheck MODULO 2 = 1 THEN
          ASSIGN iNum1 = iNum1 + iNum.
      ELSE
      DO:
          ASSIGN iNum2 = iNum * 2.
          IF iNum2 < 10 THEN
              ASSIGN iNum1 = iNum1 + iNum2.
          ELSE
              ASSIGN
                cNum  = STRING(iNum2)
                iNum1 = iNum1 + INTEGER(SUBSTR(cNum,1,1)) + INTEGER(SUBSTR(cNum,2,1)).
      END.
    END.

    ASSIGN
        iNum2 = iNum1 * 9
        iNum = iNum2 MODULO 10.

    IF iNum = INTEGER(SUBSTR(pcNumber,iLength,1)) THEN
      RETURN TRUE.
    ELSE
      RETURN FALSE.

END FUNCTION. /* fnLuhnAlgorithm  */
```


```txt
49927398716      - yes
49927398717      - no
1234567812345678 - no
1234567812345670 - yes
```



## Order

This example highlights Order's unusual treatment of numbers.
Numbers larger than 100 are not recognized by the interpreter in literal form and must instead be entered as "native" numbers (i.e. listing the separate digits as arguments to <code>8nat</code>).
Since internally, a native number is just a sequence of the digits in reverse order with an end digit marker, converting the number into a reversed list of digits mainly involves removing this terminator, so that we can immediately treat the digits as number elements.

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8luhn ORDER_PP_FN( \
8fn(8N, 8if(8is_seq(8N), 8luhn_wk(8num_to_seq(8N)), 8false)) )

#define ORDER_PP_DEF_8num_to_seq ORDER_PP_FN( \
8fn(8N, 8seq_push_back(8seq(8seq_last(8N)), 8seq_pop_back(8N))) )

#define ORDER_PP_DEF_8luhn_wk ORDER_PP_FN(                      \
8fn(8N,                                                         \
    8lets((8P, 8unzip(8N, 8nil, 8nil, 8true))                   \
          (8O, 8seq_fold(8plus, 0, 8tuple_at_0(8P)))            \
          (8E, 8seq_fold(8plus, 0,                              \
                         8seq_map(8dig_map, 8tuple_at_1(8P)))), \
          8is_0(8remainder(8plus(8O, 8E), 10)))) )

#define ORDER_PP_DEF_8dig_map ORDER_PP_FN( \
8fn(8X, 8tuple_at(8X, 8tuple(0,2,4,6,8,1,3,5,7,9))) )

#define ORDER_PP_DEF_8unzip ORDER_PP_FN(                             \
8fn(8S, 8L, 8R, 8O,                                                  \
    8if(8is_nil(8S),                                                 \
        8pair(8L, 8R),                                               \
        8if(8O,                                                      \
            8unzip(8seq_tail(8S), 8seq_push_back(8seq_head(8S), 8L), \
                   8R, 8false),                                      \
            8unzip(8seq_tail(8S), 8L,                                \
                   8seq_push_back(8seq_head(8S), 8R), 8true)))) )

ORDER_PP(8seq_to_tuple(
  8seq_map(8luhn, 8seq(8nat(4,9,9,2,7,3,9,8,7,1,6),
                       8nat(4,9,9,2,7,3,9,8,7,1,7),
                       8nat(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8),
                       8nat(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,0)))
))
```

<lang>(8true,8false,8false,8true)
```



## Oz


```oz
declare
  fun {Luhn N}
     {Sum {List.mapInd {Reverse {Digits N}}
           fun {$ Idx Dig}
              if {IsEven Idx} then {Sum {Digits 2*Dig}}
              else Dig
              end
           end}}
     mod 10 == 0
  end

  fun {Digits N}
     {Map {Int.toString N} fun {$ D} D - &0 end}
  end

  fun {Sum Xs}
     {FoldL Xs Number.'+' 0}
  end
in
  {Show
   {Map
    [49927398716 49927398717 1234567812345678 1234567812345670]
    Luhn}}
```



## Pascal


```Pascal
program luhn;

  function lunh(arg: string): boolean;
  var
    i, sum: integer;
    temp: byte;
  begin
    sum := 0;
    for i:= length(arg) downto 1 do begin  // Run the characters backwards
      temp := byte(arg[i])-48;             // Convert from ASCII to byte
      if (length(arg)-i) mod 2 = 0
        then sum := sum + temp             // Odd characters just add
        else if temp < 5
           then sum := sum + 2*temp        // Even characters add double
           else sum := sum + (2*temp)-9;   // or sum the digits of the doubling
    end;
    result := sum mod 10 = 0;              // Return true if sum ends in a 0
  end;

begin
  writeln('     49927398716: ', lunh('49927398716'));
  writeln('     49927398717: ', lunh('49927398717'));
  writeln('1234567812345678: ', lunh('1234567812345678'));
  writeln('1234567812345670: ', lunh('1234567812345670'));
end.
```


```txt
     49927398716: TRUE
     49927398717: FALSE
1234567812345678: FALSE
1234567812345670: TRUE
```



## Perl


```perl
sub luhn_test
{
        my @rev = reverse split //,$_[0];
        my ($sum1,$sum2,$i) = (0,0,0);

        for(my $i=0;$i<@rev;$i+=2)
        {
                $sum1 += $rev[$i];
                last if $i == $#rev;
                $sum2 += 2*$rev[$i+1]%10 + int(2*$rev[$i+1]/10);
        }
        return ($sum1+$sum2) % 10 == 0;
}
print luhn_test('49927398716');
print luhn_test('49927398717');
print luhn_test('1234567812345678');
print luhn_test('1234567812345670');
```


Or using map( ) and a precomputed array:


```Perl
sub luhn {
    my (@n,$i,$sum) = split //, reverse $_[0];
    my @a = map {int(2*$_ / 10) + (2*$_ % 10)} (0..9);
    map {$sum += $i++ % 2 ? $a[$_] : $_} @n;
    return ($sum % 10) ? 0 : 1;
}

# Test and display
map {print luhn($_), ": $_\n"}
    qw(49927398716 49927398717 1234567812345678 1234567812345670);
```


```txt
1: 49927398716
0: 49927398717
0: 1234567812345678
1: 1234567812345670
```



## Perl 6

Here we make use of <tt>comb</tt>, which splits into individual characters,
and the sequence operator <tt>...</tt>, which can intuit an even or odd sequence from the first two values.
<tt>%%</tt> is the divisible-by operator.

<!--
  NOTE FOR EDITORS:
  This function is also referenced by [[Validate_International_Securities_Identification_Number#Perl_6]].
  If you rename it or change its behavior, make sure to update that task as well.
-->

```perl6
sub luhn-test ($number --> Bool) {
    my @digits = $number.comb.reverse;
    my $sum = @digits[0,2...*].sum
            + @digits[1,3...*].map({ |($_ * 2).comb }).sum;
    return $sum %% 10;
}

# And we can test it like this:

use Test;

my @cc-numbers =
    '49927398716'       => True,
    '49927398717'       => False,
    '1234567812345678'  => False,
    '1234567812345670'  => True;

plan @cc-numbers.elems;

for @cc-numbers».kv -> ($cc, $expected-result) {
    is luhn-test(+$cc), $expected-result,
        "$cc {$expected-result ?? 'passes' !! 'does not pass'} the Luhn test.";
}
```


```txt
1..4
ok 1 - 49927398716 passes the Luhn test.
ok 2 - 49927398717 does not pass the Luhn test.
ok 3 - 1234567812345678 does not pass the Luhn test.
ok 4 - 1234567812345670 passes the Luhn test.
```



## Phix


```Phix
function Luhn(string st)
integer s=0, d
    for i=1 to length(st) do
        d = st[i]-'0'
        s += iff(mod(i,2)?d,d*2-(d>4)*9)
    end for
    return remainder(s,10)=0
end function

procedure test(string s)
    printf(1,"%20s : %s\n",{s,iff(Luhn(reverse(s))?"OK":"FAIL")})
end procedure

test("49927398716")
test("49927398717")
test("1234567812345678")
test("1234567812345670")
```

```txt

         49927398716 : OK
         49927398717 : FAIL
    1234567812345678 : FAIL
    1234567812345670 : OK

```



## PHP

```php
$numbers = "49927398716 49927398717 1234567812345678 1234567812345670";
foreach (split(' ', $numbers) as $n)
    echo "$n is ", luhnTest($n) ? 'valid' : 'not valid', '</br>';

function luhnTest($num) {
    $len = strlen($num);
    for ($i = $len-1; $i >= 0; $i--) {
        $ord = ord($num[$i]);
        if (($len - 1) & $i) {
            $sum += $ord;
        } else {
            $sum += $ord / 5 + (2 * $ord) % 10;
        }
    }
    return $sum % 10 == 0;
}
```

```txt
49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid
```


And a more concise example using PHP core methods:

```php
function luhn_test($num) {
    $str = '';
    foreach( array_reverse( str_split( $num ) ) as $i => $c ) $str .= ($i % 2 ? $c * 2 : $c );
    return array_sum( str_split($str) ) % 10 == 0;
}

foreach (array('49927398716','49927398717','1234567812345678','1234567812345670') as $n)
    echo "$n is ", luhn_test($n) ? 'valid' : 'not valid', "</br>\n";
```

```txt
49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid

```



## PicoLisp


```PicoLisp
(de luhn (Num)  # 'Num' may be a number or a string
   (=0
      (%
         (sum
            '((C F)
               (setq C (- (char C) 48))
               (if F
                  C                               # Odd
                  (+ (/ C 5) (% (* 2 C) 10)) ) )  # Even
            (flip (chop Num))
            '(T NIL .) )
         10 ) ) )
```

```txt
: (mapcar luhn (49927398716 49927398717 1234567812345678 1234567812345670))
-> (0 NIL NIL 0)
```



## PL/I


```pli
test: procedure options (main);

   declare (cardnumber, rcn) character (20) varying;
   declare (i, k, s1, s2) fixed binary;

   get edit (cardnumber) (L);
   cardnumber = trim(cardnumber);
   rcn = reverse(cardnumber);
   s1, s2 = 0;
   /* Sum the odd-numbered digits */
   do i = 1 to length(rcn) by 2;
      s1 = s1 + substr(rcn, i, 1);
   end;
   /* Twice the even-numbered digits. */
   do i = 2 to length(rcn) by 2;
      k = 2 * substr(rcn, i, 1);
      s2 = s2 + mod(k,10) + trunc(k/10);
   end;
   if mod(s1 + s2, 10) = 0 then
      put skip edit (cardnumber, ' passes the Luhn test' )(a);
   else
      put skip edit (cardnumber, ' does not pass the Luhn test' )(a);
   put skip list (s1 + s2);
end test;
```


```txt

49927398716 passes the Luhn test
       70
49927398717 does not pass the Luhn test
       71
1234567812345678 does not pass the Luhn test
       68
1234567812345670 passes the Luhn test
       60

```

Comment: it isn't necessary to reverse the string
in order to perform the test.


## PL/SQL


```PLSQL
FUNCTION algoLuhn ( p_numeroVerif VARCHAR2 )
    RETURN NUMBER
  IS
    i         NUMBER;
    v_NBi     SMALLINT;
    v_retour  SMALLINT;
    v_somme   NUMBER := 0;
    v_nbCar   NUMBER;

  BEGIN
    v_nbCar := LENGTH(p_numeroVerif);

    FOR i IN 1..v_nbCar
    LOOP
        v_NBi := TO_NUMBER(SUBSTR(p_numeroVerif,v_nbCar+1-i,1));

        v_somme := v_somme
                  + MOD(i,2)   * v_NBi
                  + MOD(i+1,2) * SIGN(-SIGN(v_Nbi-4)+1) * (2*v_NBi)
                  + MOD(i+1,2) * SIGN( SIGN(v_Nbi-5)+1) * (2*v_NBi-9);

    END LOOP;

    v_retour := SIGN(MOD(v_somme,10));

    RETURN v_retour;

  EXCEPTION
    WHEN OTHERS
      THEN
        RETURN 1;

  END algoLuhn;
```



## PowerBASIC

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

FUNCTION LuhnCheckPassed(BYVAL dgts AS STRING) AS INTEGER
LOCAL i, s, s1 AS DWORD
  dgts = STRREVERSE$(dgts)
    FOR i = 1 TO LEN(dgts) STEP 2
        s += VAL(MID$(dgts, i, 1))
    NEXT i
    FOR i = 2 TO LEN(dgts) STEP 2
        s1 = 2 * VAL(MID$(dgts, i, 1))
        IF s1 >= 10 THEN
            s -= 9
        END IF
        s += s1
    NEXT i
  FUNCTION = NOT ISTRUE (s MOD 10)
END FUNCTION

FUNCTION PBMAIN () AS LONG
  ' this test is expected to pass:
  CON.PRINT IIF$(LuhnCheckPassed("49927398716"), "passed", "failed")
  ' this test is expected to fail:
  CON.PRINT IIF$(LuhnCheckPassed("49927398717"), "passed", "failed")
  ' this test is expected to fail:
  CON.PRINT IIF$(LuhnCheckPassed("1234567812345678"), "passed", "failed")
  ' this test is expected to pass:
  CON.PRINT IIF$(LuhnCheckPassed("1234567812345670"), "passed", "failed")
END FUNCTION
```

```txt
passed
failed
failed
passed
```



## PowerShell


```PowerShell

function Test-LuhnNumber
{
  <#
    .SYNOPSIS
        Tests validity of credit card numbers.
    .DESCRIPTION
        Tests validity of credit card numbers using the Luhn test.
    .PARAMETER Number
        The number must be 11 or 16 digits.
    .EXAMPLE
        Test-LuhnNumber 49927398716
    .EXAMPLE
        [int64[]]$numbers = 49927398716, 49927398717, 1234567812345678, 1234567812345670
        C:\PS>$numbers | ForEach-Object {
                  "{0,-17}: {1}" -f $_,"$(if(Test-LuhnNumber $_) {'Is valid.'} else {'Is not valid.'})"
              }
  #>
    [CmdletBinding()]
    [OutputType([bool])]
    Param
    (
        [Parameter(Mandatory=$true,
                   Position=0)]
        [ValidateScript({$_.Length -eq 11 -or $_.Length -eq 16})]
        [ValidatePattern("^\d+$")]
        [string]
        $Number
    )

    $digits = ([Regex]::Matches($Number,'.','RightToLeft')).Value

    $digits |
        ForEach-Object `
               -Begin   {$i = 1} `
               -Process {if ($i++ % 2) {$_}} |
        ForEach-Object `
               -Begin   {$sumOdds = 0} `
               -Process {$sumOdds += [Char]::GetNumericValue($_)}
    $digits |
        ForEach-Object `
               -Begin   {$i = 0} `
               -Process {if ($i++ % 2) {$_}} |
        ForEach-Object `
               -Process {[Char]::GetNumericValue($_) * 2} |
        ForEach-Object `
               -Begin   {$sumEvens = 0} `
               -Process {
                            $_number = $_.ToString()
                            if ($_number.Length -eq 1)
                            {
                                $sumEvens += [Char]::GetNumericValue($_number)
                            }
                            elseif ($_number.Length -eq 2)
                            {
                                $sumEvens += [Char]::GetNumericValue($_number[0]) + [Char]::GetNumericValue($_number[1])
                            }
                        }

    ($sumOdds + $sumEvens).ToString()[-1] -eq "0"
}

```


```PowerShell

Test-LuhnNumber 49927398716

```

```txt

True

```


```PowerShell

49927398716, 49927398717, 1234567812345678, 1234567812345670 | ForEach-Object {
    "{0,-17}: {1}" -f $_,"$(if(Test-LuhnNumber $_) {'Is valid.'} else {'Is not valid.'})"
}

```

```txt

49927398716      : Is valid.
49927398717      : Is not valid.
1234567812345678 : Is not valid.
1234567812345670 : Is valid.

```



## PureBasic


```PureBasic
DataSection
  Sample:
  Data.s "49927398716"
  Data.s "49927398717"
  Data.s "1234567812345678"
  Data.s "1234567812345670"
  Data.s ""
EndDataSection

Procedure isValid(cardNumber.s)
  Protected i, length, s1, s2, s2a

  cardNumber = ReverseString(cardNumber)
  length = Len(cardNumber)
  For i = 1 To length Step 2
    s1 + Val(Mid(cardNumber, i, 1))
  Next

  For i = 2 To length Step 2
    s2a = Val(Mid(cardNumber, i, 1)) * 2
    If s2a < 10
      s2 + s2a
    Else
      s2 + 1 + Val(Right(Str(s2a), 1))
    EndIf
  Next

  If Right(Str(s1 + s2), 1) = "0"
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure


If OpenConsole()
  Define cardNumber.s

  Restore Sample
  Repeat
    Read.s cardNumber
    If cardNumber <> ""
      Print(cardNumber + " is ")
      If isValid(cardNumber)
        PrintN("valid")
      Else
        PrintN("not valid")
      EndIf
    EndIf
  Until cardNumber = ""

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
49927398716 is valid
49927398717 is not valid
1234567812345678 is not valid
1234567812345670 is valid
```



## Python

The [http://docs.python.org/py3k/library/functions.html#divmod divmod] in the function below conveniently splits a number into its two digits ready for summing:

```python>>>
 def luhn(n):
	r = [int(ch) for ch in str(n)][::-1]
	return (sum(r[0::2]) + sum(sum(divmod(d*2,10)) for d in r[1::2])) % 10 == 0

>>> for n in (49927398716, 49927398717, 1234567812345678, 1234567812345670):
	print(n, luhn(n))
```


```txt
49927398716 True
49927398717 False
1234567812345678 False
1234567812345670 True

```
or without sum() and divmod() functions:


```python>>>
 def vérifLuhn(ch):
  sum = 0
  chParity = len(ch) % 2
  for i in range (len(ch)-1, -1, -1):
    j = int(ch[i])
      if ((i + 1) % 2 != chParity):
        j = j * 2
        if (j > 9):
          j = j - 9
      sum = sum + j
  return sum % 10 == 0, “somme calculée obtenue = “ + str(sum)

for n in (49927398716, 49927398717, 1234567812345678, 1234567812345670):
        print (str(n)+" =>", vérifLuhn(str(n)))

```



## Q


```q
sd:{s:0; while[x<>0; s+:x mod 10; x:floor x%10]; s}    / Sum digits of x
luhn:{
    r:reverse string x;                             / Reversed credit card number
    o:("I"$) each r[2*til ceiling (count r) % 2];   / Odd-indexed numbers
    e:("I"$) each r[1+2*til floor (count r) % 2];   / Even-indexed numbers
    0=(sum o,sd each e*2) mod 10                    / Return 1b if checksum ends in 0; 0b otherwise
}
```


```txt
q)luhn each  49927398716 49927398717 1234567812345678 1234567812345670
1001b
```



## R


```rsplus
is.luhn <- function(cc){
	numbers <- as.numeric(rev(unlist(strsplit(cc,""))))
	mod(sum({numbers[seq(2,length(numbers),by=2)]*2 ->.; mod(.,10)+.%/%10}) +
		sum(numbers[seq(1,length(numbers),by=2)]), 10)==0
}
```


```txt
sapply(c("49927398716","49927398717","1234567812345678","1234567812345670"),is.luhn)
     49927398716      49927398717 1234567812345678 1234567812345670
            TRUE            FALSE            FALSE             TRUE
```



## Racket



```Racket

#lang racket

(define (luhn-test n)
  (let loop ([n n] [odd? #t] [s 0])
    (if (zero? n)
      (zero? (modulo s 10))
      (let*-values ([(q r)   (quotient/remainder n 10)]
                    [(rq rr) (quotient/remainder (* (if odd? 1 2) r) 10)])
        (loop q (not odd?) (+ s rq rr))))))

(map luhn-test '(49927398716 49927398717 1234567812345678 1234567812345670))
;; -> '(#t #f #f #t)

```



## REXX


### version 1


```REXX
/*REXX program  validates  credit card numbers  using  the    Luhn    algorithm.        */
#.=;                #.1= 49927398716             /*the  1st  sample credit card number. */
                    #.2= 49927398717             /* "   2nd     "      "     "     "    */
                    #.3= 1234567812345678        /* "   3rd     "      "     "     "    */
                    #.4= 1234567812345670        /* "   4th     "      "     "     "    */
      do k=1  while #.k\==''                     /*validate all the credit card numbers.*/
      say right( Luhn(#.k), 9)          ' the Luhn test, credit card number: '         #.k
      end   /*k*/                                /* [↑] function returns passed│flunked.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Luhn: procedure;  parse arg x;              $=0  /*get credit card number;  zero $ sum. */
      y=reverse(left(0, length(x) // 2)x)        /*add leading zero if needed, reverse. */
               do j=1  to length(y)-1  by 2;            _=substr(y,j+1,1) * 2
               $=$ + substr(y,j,1) + left(_,1) + substr(_,2,1,0)      /* ◄────────────┐ */
               end   /*j*/                       /*sum odd and even decimal digits ►──┘ */
      return word('passed flunked',1+($//10==0)) /*$ ending in zero?  Then the # passed.*/
```

```txt

  flunked  the Luhn test, credit card number:  49927398716
   passed  the Luhn test, credit card number:  49927398717
   passed  the Luhn test, credit card number:  1234567812345678
  flunked  the Luhn test, credit card number:  1234567812345670

```



### Version 2


```REXX
/* Rexx ***************************************************
* 09.04.2013 Walter Pachl
* Implements the task's description in a rather concise way
* Instead of reverting the ccn work it backwards
**********************************************************/
numeric digits 20

push 49927398716
push 49927398717
push 1234567812345678
push 1234567812345670

do while queued() > 0
  parse pull ccnum
  if luhn(ccnum) then ln = 'passed'
  else                ln = 'failed'
  say right(ccnum, 20) ln
  end
return
exit

luhn:
Parse Arg ccn                /* credit card number       */
sum=0                        /* initialize test sum      */
even=0                       /* even indicator           */
Do i=length(ccn) To 1 By -1  /* process all digits       */
  c=substr(ccn,i,1)          /* pick one digit at a time */
  If even Then Do            /* even numbered digit      */
    c=c*2                    /* double it                */
    If c>=10 Then            /* 10, 12, 14, 16, 18       */
      c=c-9                  /* Sum of the two digits    */
    End                      /* end of even numbered     */
  even=\even                 /* flip even indicator      */
  sum=sum+c                  /* add into test sum        */
  End
Return right(sum,1)=0        /* ok if last digit is 0    */
```

```txt

    1234567812345670 passed
    1234567812345678 failed
         49927398717 failed
         49927398716 passed

```



## Ring


```ring
decimals(0)

test = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
for n = 1 to len(test)
      see test[n] + " -> " + cardtest(test[n]) + nl
next

func cardtest(numstr)
        revstring = revstr(numstr)
        s1 = revodd(revstring)
        s2 = reveven(revstring)
        s3 =right(string(s1+s2), 1)
        if s3 = "0"
           return "Valid"
        else
           return "Invalid"
        ok

func revstr(str)
      strnew = ""
      for nr = len(str) to 1 step -1
           strnew = strnew + str[nr]
      next
      return strnew

func revodd(str)
        strnew = ""
        for nr = 1 to len(str) step 2
             strnew = strnew + str[nr]
        next
        sumodd = 0
        for p = 1 to len(strnew)
              sumodd = sumodd + number(strnew[p])
        next
        return sumodd

func reveven(str)
        strnew = ""
        for nr = 2 to len(str) step 2
             strnew = strnew + str[nr]
        next
        lsteven = []
        for p = 1 to len(strnew)
             add(lsteven, string(2*number(strnew[p])))
        next
        arreven = list(len(lsteven))
        for q = 1 to len(lsteven)
              sum = 0
              for w = 1 to len(lsteven[q])
                    sum = sum + lsteven[q][w]
              next
              arreven[q] = sum
        next
        sumarr = 0
        for x = 1 to len(arreven)
             sumarr = sumarr + arreven[x]
        next
        return sumarr

```

Output:

```txt

49927398716 -> Valid
49927398717 -> Invalid
1234567812345678 -> Invalid
1234567812345670 -> Valid

```



## Ruby


```ruby
 def luhn_valid?(str)
   str.scan(/\d/).reverse            #using str.to_i.digits fails for cases with leading zeros
      .each_slice(2)
      .sum { |i, k = 0| i.to_i + ((k.to_i)*2).digits.sum }
      .modulo(10).zero?
 end

["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
  .map{|i| luhn_valid?(i)}
end
```


```txt

[ true, false, false, true ]
```



## Run BASIC


```runbasic
card$(1) = "49927398716"
card$(2) = "49927398717"
card$(3) = "1234567812345678"
card$(4) = "1234567812345670"

for i = 1 to 4
print card$(i);" ";luhn$(card$(i))
next i

FUNCTION luhn$(card$)
    lc      = len(card$)
    for i   = lc to 1 step -1
      digit = val(mid$(card$,i,1))
      if ((lc -i) mod 2) = 0 then chkSum = chkSum + digit else chkSum = chkSum + int(digit * 2.2)
    next i
    if chkSum mod 10 = 0 then luhn$ = "True" else luhn$ = "False"
end function
```

```txt
49927398716 True
49927398717 False
1234567812345678 False
1234567812345670 True
```



## Rust


```rust
extern crate luhn_test_of_credit_card_numbers;

use luhn_test_of_credit_card_numbers::luhn_test;

fn validate_isin(isin: &str) -> bool {
    if !isin.chars().all(|x| x.is_alphanumeric()) || isin.len() != 12 {
        return false;
    }
    if !isin[..2].chars().all(|x| x.is_alphabetic())
        || !isin[2..12].chars().all(|x| x.is_alphanumeric())
        || !isin.chars().last().unwrap().is_numeric()
    {
        return false;
    }

    let bytes = isin.as_bytes();

    let s2 = bytes
        .iter()
        .flat_map(|&c| {
            if c.is_ascii_digit() {
                vec![c]
            } else {
                (c + 10 - ('A' as u8)).to_string().into_bytes()
            }
        })
        .collect::<Vec<u8>>();

    let string = std::str::from_utf8(&s2).unwrap();
    let number = string.parse::<u64>().unwrap();

    return luhn_test(number as u64);
}

#[cfg(test)]
mod tests {
    use super::validate_isin;

    #[test]
    fn test_validate_isin() {
        assert_eq!(validate_isin("US0378331005"), true);
        assert_eq!(validate_isin("US0373831005"), false);
        assert_eq!(validate_isin("U50378331005"), false);
        assert_eq!(validate_isin("US03378331005"), false);
        assert_eq!(validate_isin("AU0000XVGZA3"), true);
        assert_eq!(validate_isin("AU0000VXGZA3"), true);
        assert_eq!(validate_isin("FR0000988040"), true);
    }
}
```

```txt
49927398716: true
49927398717: false
1234567812345678: false
1234567812345670: true
```



## Scala

### Functional style


```scala
object Luhn {
  private def parse(s: String): Seq[Int] = s.map{c =>
    assert(c.isDigit)
    c.asDigit
  }
  def checksum(digits: Seq[Int]): Int = {
    digits.reverse.zipWithIndex.foldLeft(0){case (sum,(digit,i))=>
      if (i%2 == 0) sum + digit
      else sum + (digit*2)/10 + (digit*2)%10
    } % 10
  }
  def validate(digits: Seq[Int]): Boolean = checksum(digits) == 0
  def checksum(number: String): Int = checksum(parse(number))
  def validate(number: String): Boolean = validate(parse(number))
}

object LuhnTest extends App {
  Seq(("49927398716", true),
    ("49927398717", false),
    ("1234567812345678", false),
    ("1234567812345670", true)
  ).foreach { case (n, expected) =>
    println(s"$n ${Luhn.validate(n)}")
    assert(Luhn.validate(n) == expected)
  }
}
```

```txt
49927398716 true
49927398717 false
1234567812345678 false
1234567812345670 true
```



### Imperative style


```scala
  def luhnTest1(number: String): Boolean = {
    var (odd, sum) = (true, 0)

    for (int <- number.reverse.map { _.toString.toShort }) {
      if (odd) sum += int
      else sum += (int * 2 % 10) + (int / 5)
      odd = !odd
    }
    sum % 10 == 0
  }
```



## Scheme


```scheme
(define luhn
  (lambda (n)
    (let loop ((number n)
               (index 0)
               (result 0))
      (if (= 0 number)
          (= 0 (remainder result 10))
          (loop (quotient number 10)
                (+ index 1)
                (+ result
                   (if (even? index)
                       (remainder number 10)
                       (let ((part (* 2 (remainder number 10))))
                         (+ (remainder part 10) (quotient part 10))))))))))
```

```txt

(map luhn '(49927398716 49927398717 1234567812345678 1234567812345670))
(#t #f #f #t)

```



## sed


```sed
# Split number into double evens and odds
s/.*/&: /
: split
s/\([0-4]\)\([0-9]\):\(.*\) /:\1\1\3 \2/
s/\([5-9]\)\([0-9]\):\(.*\) /:1\1\1\3 \2/
t split

# Set up addition lookup table
s/\([0-9]\)*:\(.*\) \(.*\)/\1\2\3:0123456789012345678/

: add
s/\([0-9]\)0:/\1:/
s/\([0-9]\)1:\(.*\1.\{0\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)2:\(.*\1.\{1\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)3:\(.*\1.\{2\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)4:\(.*\1.\{3\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)5:\(.*\1.\{4\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)6:\(.*\1.\{5\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)7:\(.*\1.\{6\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)8:\(.*\1.\{7\}\([0-9]\).*\)/\3:\2/
s/\([0-9]\)9:\(.*\1.\{8\}\([0-9]\).*\)/\3:\2/
t add

/0:/a\
Pass
/0:/!a\
Fail
d
```


```txt

$ sed -f luhn.sed <<!
49927398716
49927398717
1234567812345678
1234567812345670
!
Pass
Fail
Fail
Pass

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: luhnTest (in string: cardNumber) is func
  result
    var boolean: luhnTest is FALSE;
  local
    var integer: index is 0;
    var integer: digit is 0;
    var boolean: isOdd is TRUE;
    var integer: oddSum is 0;
    var integer: evenSum is 0;
  begin
    for index range length(cardNumber) downto 1 do
      digit := integer parse str(cardNumber[index]);
      if isOdd then
        oddSum +:= digit;
      else
        evenSum +:= digit div 5 + (2 * digit) rem 10;
      end if;
      isOdd := not isOdd;
    end for;
    luhnTest := (oddSum + evenSum) rem 10 = 0;
  end func;

const proc: main is func
  local
    var string: cardNumber is "";
  begin
    for cardNumber range [] ("49927398716", "49927398717", "1234567812345678", "1234567812345670") do
      writeln(cardNumber <& ": " <& luhnTest(cardNumber));
    end for;
  end func;
```


```txt

49927398716: TRUE
49927398717: FALSE
1234567812345678: FALSE
1234567812345670: TRUE

```



## SequenceL


```sequenceL

main(args(2)) :=
	sum(luhnTest(asciiToInt(args[1]) - asciiToInt('0'))) mod 10 = 0;

s2Mapping := [0,2,4,6,8,1,3,5,7,9];

luhnTest(x(1))[i] :=
	x[i] when i mod 2 = size(x) mod 2 else
	s2Mapping[x[i] + 1];

```



## Shen


```shen

(define mapi
  _ _ []       -> []
  F N [X | Xs] -> [(F N X) | (mapi F (+ N 1) Xs)])

(define double
  X -> (let Y (* 2 X) (if (> Y 9) (- Y 9) Y)))

(define luhn?
  Number ->
    (let Exploded (explode Number)
         Digits   (map (/. H (- (string->n H) 48)) Exploded)
         Reversed (reverse Digits)
         Doubled  (mapi (/. N X (if (= 1 (shen.mod N 2)) (double X) X)) 0 Reversed)
         Summed   (sum Doubled)
         Modded   (shen.mod Summed 10)
      (= 0 Modded)))

"Expected: [true false false true]"

(map (function luhn?) ["49927398716" "49927398717" "1234567812345678" "1234567812345670"])

```


```txt

mapi
transform-x
luhn?
"Expected: [true false false true]"
[true false false true]

run time: 0.014999985694885254 secs
loaded

```



## Sidef


```ruby
func luhn (n) {
    static a = {|j| (2*j // 10) + (2*j % 10) }.map(^10)

    var checksum = n.digits.map_kv {|i,j|
        i.is_odd ? a[j] : j
    }.sum

    checksum % 10 == 0
}

for n in [49927398716, 49927398717, 1234567812345678, 1234567812345670] {
    say [n, luhn(n)]
}
```


```txt
[49927398716, true]
[49927398717, false]
[1234567812345678, false]
[1234567812345670, true]
```



## SNOBOL4

Using a precomputed array.

```SNOBOL4
        define('luhn(n)a,d,i,j,sum') :(luhn_end)
luhn    n = reverse(n); a = array('0:9')
ln1     a<i> = (2 * i / 10) + remdr(2 * i,10)
        i = lt(i,9) i + 1 :s(ln1)
ln2     n len(1) . d = :f(ln3)
        d = ne(remdr(j,2),0) a<d>; j = j + 1
        sum = sum + d :(ln2)
ln3     luhn = 0; luhn = eq(remdr(sum,10),0) 1 :(return)
luhn_end

*       # Test and display
        test = " output = luhn(n) ': ' n"
        n = '49927398716'; eval(test)
        n = '49927398717'; eval(test)
        n = '1234567812345678'; eval(test)
        n = '1234567812345670'; eval(test)
end
```


```txt
1: 49927398716
0: 49927398717
0: 1234567812345678
1: 1234567812345670
```



## SPARK

Works with SPARK GPL 2010 and GPS GPL 2010.

Based on the Ada version. All code shown to be free of run-time type errors.

A final test has been added which passes as valid unless there is an explicit test for all digits.

```ada
with Spark_IO;
--# inherit Spark_IO;
--# main_program;
procedure Luhn
--# global in out Spark_IO.Outputs;
--# derives Spark_IO.Outputs from *;
is

   function Luhn_Test (Num : String) return Boolean
   --# pre  Num'Last <= 20;
   is
      Sum : Integer := 0;
      Od  : Boolean := True;
      Int : Integer;
      X   : Integer;
      OK  : Boolean := True;
   begin
      for P in reverse Integer range Num'Range loop
         Int := Character'Pos(Num(P)) - Character'Pos('0');
         if Int not in 0 .. 9 then
            OK := False;
            exit;
         end if;
         X := (((Int*2) mod 10) + (Int / 5));
         --# assert Num'Last - P in 0 .. 19
         --#   and  Sum in 0 .. (Num'Last - P) * 10
         --#   and  Int in 0 .. 9
         --#   and  X   in 0 .. 10;
         if Od then
            Sum := Sum + Int;
         else
            Sum := Sum + X;
         end if;
         Od := not Od;
      end loop;
      return OK and (Sum mod 10) = 0;
   end Luhn_Test;

   procedure Do_Test (Num : in     String)
   --# global in out Spark_IO.Outputs;
   --# derives Spark_IO.Outputs from *, Num;
   --# pre  Num'Last <= 20;
   is
   begin
      Spark_IO.Put_String(Spark_IO.Standard_Output, Num, 16);
      if Luhn_Test(Num) then
         Spark_IO.Put_String(Spark_IO.Standard_Output, " is valid.", 0);
      else
         Spark_IO.Put_String(Spark_IO.Standard_Output, " is not valid.", 0);
      end if;
      Spark_IO.New_Line(Spark_IO.Standard_Output, 1);
   end Do_Test;

begin
   Do_Test("49927398716");
   Do_Test("49927398717");
   Do_Test("1234567812345678");
   Do_Test("1234567812345670");
   Do_Test("123456781234567D");
end Luhn;
```

```txt
49927398716      is valid.
49927398717      is not valid.
1234567812345678 is not valid.
1234567812345670 is valid.
123456781234567D is not valid.
```



## SQL PL

With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

CREATE OR REPLACE FUNCTION LUHN_TEST (
  IN NUMBER VARCHAR(24)
 ) RETURNS SMALLINT
 --) RETURNS BOOLEAN
 BEGIN
  DECLARE TYPE CARD_NUMBER AS VARCHAR(1) ARRAY [24];
  DECLARE LENGTH SMALLINT;
  DECLARE REVERSE CARD_NUMBER;
  DECLARE I SMALLINT;
  DECLARE POS SMALLINT;
  DECLARE S1 SMALLINT;
  DECLARE S2 SMALLINT;
  DECLARE TEMP SMALLINT;
  DECLARE RET SMALLINT;
  --DECLARE RET BOOLEAN;
  DECLARE INVALID_CHAR CONDITION FOR SQLSTATE 'LUHN1';

  -- Reverse the order of the digits in the number.
  SET LENGTH = LENGTH(NUMBER);
  SET I = 1;
  WHILE (I <= LENGTH) DO
   SET POS = LENGTH - I + 1;
   SET REVERSE[POS] = SUBSTR(NUMBER, I, 1);
   IF (ASCII(REVERSE[POS]) < 48 OR 57 < ASCII(REVERSE[POS])) THEN
    SIGNAL INVALID_CHAR SET MESSAGE_TEXT = 'Invalid character, not a digit';
   END IF;
   SET I = I + 1;
  END WHILE;

  -- Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
  SET S1 = 0;
  SET I = 1;
  WHILE (I <= LENGTH) DO
   IF (MOD(I, 2) = 1) THEN
    SET S1 = S1 + REVERSE[I];
   END IF;
   -- CALL DBMS_OUTPUT.PUT_LINE('I ' || I || ', S1 ' || S1 || ', val ' || REVERSE[I]);
   SET I = I + 1;
  END WHILE;

  -- Taking the second, fourth ... and every other even digit in the reversed digits:
  SET S2 = 0;
  SET TEMP = 0;
  SET I = 1;
  WHILE (I <= LENGTH) DO
   IF (MOD(I, 2) = 0) THEN
    -- Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
    SET TEMP = REVERSE[I] * 2;
    IF (TEMP > 9) THEN
     SET TEMP = (TEMP / 10) + (MOD(TEMP, 10));
    END IF;
    -- Sum the partial sums of the even digits to form s2
    SET S2 = S2 + TEMP;
   END IF;
   -- CALL DBMS_OUTPUT.PUT_LINE('I ' || I || ', S2 ' || S2 || ', TEMP ' || TEMP || ' val ' || REVERSE[I]);
   SET I = I + 1;
  END WHILE;

  -- If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.
  SET RET = 1;
  --SET RET = FALSE;
  SET TEMP = S1 + S2;
  IF (MOD(TEMP, 10) = 0) THEN
   SET RET = 0;
   --SET RET = FALSE;
   CALL DBMS_OUTPUT.PUT_LINE('It is a valid number ' || S1 || '+' || S2 || '=' || TEMP);
  ELSE
   CALL DBMS_OUTPUT.PUT_LINE('It is NOT a valid number ' || S1 || '+' || S2 || '=' || TEMP);
  END IF;
  RETURN RET;
 END
@

```

Output:

```txt

db2 -td@
db2 => SET SERVEROUTPUT ON @
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => CREATE OR REPLACE FUNCTION VALIDATE_CREDIT_CARD_NUMBER (
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.
db2 => values VALIDATE_CREDIT_CARD_NUMBER(49927398716)@

1
------
     0

  1 record(s) selected.

It is a valid number 42+28=70
db2 => VALUES VALIDATE_CREDIT_CARD_NUMBER(49927398717)@

1
------
     1

  1 record(s) selected.

It is NOT a valid number 43+28=71
db2 => VALUES VALIDATE_CREDIT_CARD_NUMBER(1234567812345678)@

1
------
     1

  1 record(s) selected.

It is NOT a valid number 40+28=68
db2 => VALUES VALIDATE_CREDIT_CARD_NUMBER(1234567812345670)@

1
------
     0

  1 record(s) selected.

It is a valid number 32+28=60

```



## Standard ML


```sml
local
  fun revDigits 0 = []
    | revDigits n = (n mod 10) :: revDigits (n div 10)

  fun digitSum n = if n > 9 then digitSum (n div 10 + n mod 10)
                            else n

  fun luhn_sum []  = 0
    | luhn_sum [d] = d
    | luhn_sum (d::d'::ds) = d + digitSum (2*d') + luhn_sum ds
in
  fun luhn_test n = luhn_sum (revDigits n) mod 10 = 0

  val res = map luhn_test [49927398716, 49927398717, 1234567812345678, 1234567812345670];
end;

(*
[opening file "luhn.sml"]
> val luhn_test = fn : int -> bool
  val res = [true, false, false, true] : bool list
[closing file "luhn.sml"]
*)
```



## Swift


```swift
func luhn(_ number: String) -> Bool {
    return number.reversed().enumerated().map({
        let digit = Int(String($0.element))!
        let even = $0.offset % 2 == 0
        return even ? digit : digit == 9 ? 9 : digit * 2 % 9
    }).reduce(0, +) % 10 == 0
}

luhn("49927398716") // true
luhn("49927398717") // false
```



## Tcl

Based on an algorithmic encoding for the test on Wikipedia.

```tcl
package require Tcl 8.5
proc luhn digitString {
    if {[regexp {[^0-9]} $digitString]} {error "not a number"}
    set sum 0
    set flip 1
    foreach ch [lreverse [split $digitString {}]] {
	incr sum [lindex {
	    {0 1 2 3 4 5 6 7 8 9}
	    {0 2 4 6 8 1 3 5 7 9}
	} [expr {[incr flip] & 1}] $ch]
    }
    return [expr {($sum % 10) == 0}]
}
```

Driver:

```tcl
foreach testNumber {
    49927398716
    49927398717
    1234567812345678
    1234567812345670
} {
    puts [format "%s is %s" $testNumber \
	      [lindex {"NOT valid" "valid"} [luhn $testNumber]]]
}
```

```txt

49927398716 is valid
49927398717 is NOT valid
1234567812345678 is NOT valid
1234567812345670 is valid

```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:LUHN
:Disp "ENTER NUMBER"
:Input Str1
:0→S
:0→E
:For(I,length(Str1),1,-1)
  :inString("0123456789",sub(Str1,I,1))–1→X
  :If X<0
    :Goto BA
  :If E≠0
  :Then
    :2X→X
    :If X>9
      :X–9→X
  :End
  :X+S→S
  :not(E)→E
:End
:If fPart(S/10)=0
:Then
  :Disp "GOOD CARD"
:Else
  :Lbl BA
  :Disp "BAD CARD"
:End

```


=={{header|Transact-SQL}}==

<lang Transact-SQL>
CREATE FUNCTION dbo._CreditCardNumCheck( @strCCNum VarChar(40) )
RETURNS VarChar(7)
AS
BEGIN
	DECLARE @string			VarChar(40) = REVERSE(@strCCNum); -- usage: set once, never changed
	DECLARE @strS2Values		VarChar(10) = '0246813579';  -- constant: maps digits to their S2 summed values
	DECLARE @table			TABLE (ID INT, Value INT, S_Value INT); -- ID=digit position. S_Value is used for SUM().
	DECLARE @p			INT = 0; -- loop counter: position in string
	-- Convert the reversed string's digits into rows in a table variable, S_Values to be updated afterwards
	WHILE @p < LEN(@string)
		BEGIN
			SET @p = @p+1;
			INSERT INTO @table (ID,Value,S_Value) VALUES (@p, CONVERT(INT,SUBSTRING(@string,@p,1)), 0);
		END
	-- Update S_Value column : the digit's value to be summed (for even-positioned digits this is mapped via @strS2Values)
	UPDATE @table SET S_Value = CASE WHEN ID % 2 = 1 THEN Value ELSE CONVERT(INT,SUBSTRING(@strS2Values,Value+1,1)) END
	-- If the SUM of S_Values ends in 0 (modulo 10 = 0) then the CC Number is valid
	RETURN CASE WHEN (SELECT SUM(S_Value) FROM @table) % 10 = 0 THEN 'Valid' ELSE 'Invalid' END
END

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
MODE DATA
$$ SET cardnumbers=*
49927398716
49927398717
1234567812345678
1234567812345670
$$ MODE TUSCRIPT
-> collecting information for output-format
SET length=MAX_LENGTH(cardnumbers)
SET adjust=length+2

LOOP c=cardnumbers
-> ">/" = any digit
SET cstring=STRINGS (c,":>/:")
SET creverse=REVERSE (cstring)
SET s1=evenx2=esum=s2=""
 LOOP n,oe=creverse
  SET modrest=MOD(n,2)
  IF (modrest==0) THEN
   SET even=oe*2
    IF (even>9) THEN
     SET estring=STRINGS (even,":>/:")
     SET esum=SUM (estring)
     SET s2=APPEND (s2,esum)
    ELSE
     SET s2=APPEND (s2,even)
    ENDIF
  ELSE
   SET s1=APPEND(s1,oe)
  ENDIF
 ENDLOOP
SET s1=SUM(s1),s2=SUM(s2)
SET checksum=s1+s2
SET c=CENTER(c,-adjust)
IF (checksum.ew."0") THEN
 PRINT c,"true"
ELSE
 PRINT c,"false"
ENDIF
ENDLOOP
```

```txt

49927398716       true
49927398717       false
1234567812345678  false
1234567812345670  true

```



## TXR



```txr
@(do (defun luhn (num)
       (for ((i 1) (sum 0))
            ((not (zerop num)) (zerop (mod sum 10)))
            ((inc i) (set num (trunc num 10)))
          (let ((dig (mod num 10)))
            (if (oddp i)
              (inc sum dig)
              (let ((dig2 (* 2 dig)))
                (inc sum (+ (trunc dig2 10) (mod dig2 10)))))))))
@(collect :vars nil)
@{ccnumber /[0-9]+/}
@(output)
@ccnumber -> @(if (luhn (int-str ccnumber 10)) "good" "bad")
@(end)
@(end)
```



```txt
$ txr luhn.txr luhn.txt
49927398716 -> good
49927398717 -> bad
1234567812345678 -> bad
1234567812345670 -> good
```



## UNIX Shell

```bash
function luhn {
  typeset n p s t=('0123456789' '0516273849')
  while ((-n<${#1})); do
    p="${t[n--%2]%${1:n:1}*}"
    ((s+=${#p}))
  done
  ((s%10))
}

for c in 49927398716 49927398717 1234567812345678 1234567812345670; do
    if luhn $c; then
        echo $c is invalid
    else
        echo $c is valid
    fi
done
```


Notes:
* The parameter expansion hack (p=${t…%${1:n:1}};…${#p}…) is an interesting way of converting a set of characters to ordinals.  It's highly extensible to larger character sets (e.g. for ISBN and Code 39 checksums).
* Invalid characters are effectively treated as 0s.  This is actually useful sometimes for ignoring alphabetic prefixes.
* When attempting to understand the function, remember that n is negative, so it indexes from the end of the input string.

```txt
49927398716 is valid
49927398717 is invalid
1234567812345678 is invalid
1234567812345670 is valid
```



## Ursala


```Ursala
#import std
#import nat

luhn = %nP; %np*hxiNCNCS; not remainder\10+ //sum:-0@DrlrHK32 ~&iK27K28TK25 iota10
```


Some notes on this solution:
* <code>iota10</code> is the list of natural numbers <code><0,1,2,3,4,5,6,7,8,9></code>
* <code>~&K27</code> and <code>~&K28</code> of <code>iota10</code> extract the alternate items, respectively <code><0,2,4,6,8></code> and <code><1,3,5,7,9></code>
* <code>~&K27K28T iota10</code> is their concatenation, <code><0,2,4,6,8,1,3,5,7,9></code> which is also the list of values obtained by doubling each item of <code>iota10</code> and taking digit sums
* <code>~&iK27K28TX iota10</code> would be the pair <code>(<0,1,2,3,4,5,6,7,8,9>,<0,2,4,6,8,1,3,5,7,9>)</code>, but using the reification operator <code>K25</code> in place of <code>X</code> makes it an executable function taking any item of the left list as an argument and returning the corresponding item of the right.
* The part beginning with <code>//</code> is a function of the form <code>//f a</code>, which can be applied to any argument <code>b</code> to obtain <code>f(a,b)</code>. In this case, the <code>f</code> is <code>sum:-0@DrlrHK32</code>, which is equivalent to the composition of two functions <code>sum:-0</code> and <code>~&DrlrHK32</code>, and <code>a</code> is the function just obtained by reification.
* The function <code>~&D</code> by itself takes a pair <code>(a,<b0</code>...<code>bn>)</code> whose right side is a list, and returns the list of pairs <code><(a,b0)</code>...<code>(a,bn)></code> (i.e., a copy of <code>a</code> paired with each <code>b</code>). The <code>a</code> here will end up being the aforementioned function.
* <code>~&DrlrHK32</code> not only forms such a list of pairs, but operates on each pair thus obtained, alternately applying <code>~&r</code> and <code>~&lrH</code> to each pair in sequence, where <code>~&r</code> simply returns the right side of the pair, and <code>~&lrH</code> uses the left side as a function, which is applied to the right.
* <code>sum:-0</code> computes the cumulative sum of a list of natural numbers using the binary <code>sum</code> function, and the reduction operator (<code>:-</code>) with vacuous sum 0.
* The whole thing described up to this point is therefore a function that will take a list of numbers in the range 0 to 9, and compute the summation obtained when doubling and digit summing alternate items.
* The input list to this function is constructed from a single natural number first by <code>%nP</code>, which transforms it to text format in decimal, followed by <code>%np*hxiNCNCS</code>, which reverses the digits, makes a separate text of each, and parses them as individual numbers.
* The output from the function is tested for divisibility by 10 with <code>remainder\10</code>, with the result negated so that zero values map to true and non-zero to false.
usage:

```ursala
#cast %bL

test = luhn* <49927398716,49927398717,1234567812345678,1234567812345670>
```

```txt

<true,false,false,true>

```


## VBA


```VB

Option Explicit

Sub Main()
  Debug.Print "Number 49927398716 is "; Luhn("49927398716")
  Debug.Print "Number 49927398717 is "; Luhn("49927398717")
  Debug.Print "Number 1234567812345678 is "; Luhn("1234567812345678")
  Debug.Print "Number 1234567812345670 is "; Luhn("1234567812345670")
End Sub
Private Function Luhn(Nb As String) As String
Dim t$, i&, Summ&, s&
    t = StrReverse(Nb)
    For i = 1 To Len(t) Step 2
        Summ = Summ + CInt(Mid(t, i, 1))
    Next i
    For i = 2 To Len(t) Step 2
        s = 2 * (CInt(Mid(t, i, 1)))
        If s >= 10 Then
            Summ = Summ - 9
        End If
        Summ = Summ + s
    Next i
    If Summ Mod 10 = 0 Then
        Luhn = "valid"
    Else
        Luhn = "invalid"
    End If
End Function
```

```txt
Number 49927398716 is valid
Number 49927398717 is invalid
Number 1234567812345678 is invalid
Number 1234567812345670 is valid
```



## VBScript


```VBScript
Function Luhn_Test(cc)
	cc = RevString(cc)
	s1 = 0
	s2 = 0
	For i = 1 To Len(cc)
		If i Mod 2 > 0 Then
			s1 = s1 + CInt(Mid(cc,i,1))
		Else
			tmp = CInt(Mid(cc,i,1))*2
			If  tmp < 10 Then
				s2 = s2 + tmp
			Else
				s2 = s2 + CInt(Right(CStr(tmp),1)) + 1
			End If
		End If
	Next
	If Right(CStr(s1 + s2),1) = "0" Then
		Luhn_Test = "Valid"
	Else
		Luhn_Test = "Invalid"
	End If
End Function

Function RevString(s)
	For i = Len(s) To 1 Step -1
		RevString = RevString & Mid(s,i,1)
	Next
End Function

WScript.Echo "49927398716 is " & Luhn_Test("49927398716")
WScript.Echo "49927398717 is " & Luhn_Test("49927398717")
WScript.Echo "1234567812345678 is " & Luhn_Test("1234567812345678")
WScript.Echo "1234567812345670 is " & Luhn_Test("1234567812345670")
```


```txt
49927398716 is Valid
49927398717 is Invalid
1234567812345678 is Invalid
1234567812345670 is Valid
```




## Visual Basic

```vb
Public Function LuhnCheckPassed(ByVal dgts As String) As Boolean
Dim i As Long, s As Long, s1 As Long
  dgts = VBA.StrReverse(dgts)
    For i = 1 To Len(dgts) Step 2
        s = s + CInt(Mid$(dgts, i, 1))
    Next i
    For i = 2 To Len(dgts) Step 2
        s1 = 2 * (CInt(Mid$(dgts, i, 1)))
        If s1 >= 10 Then
            s = s - 9
        End If
        s = s + s1
    Next i
  LuhnCheckPassed = Not CBool(s Mod 10)
End Function
```


Test:

```vb
Sub Main()
  Debug.Assert LuhnCheckPassed("49927398716")
  Debug.Assert Not LuhnCheckPassed("49927398717")
  Debug.Assert Not LuhnCheckPassed("1234567812345678")
  Debug.Assert LuhnCheckPassed("1234567812345670")
End Sub
```



## Visual Basic .NET


```Visual Basic .NET

    Imports System.Linq
    Function ValidLuhn(value As String)
        Return value.Select(Function(c, i) (AscW(c) - 48) << ((value.Length - i - 1) And 1)).Sum(Function(n) If(n > 9, n - 9, n)) Mod 10 = 0
    End Function
    Sub Main()
        Console.WriteLine(ValidLuhn("49927398716"))
        Console.WriteLine(ValidLuhn("49927398717"))
        Console.WriteLine(ValidLuhn("1234567812345678"))
        Console.WriteLine(ValidLuhn("1234567812345670"))
    End Sub

```

```txt

True
False
False
True

```



## Xojo


```xojo
Public Function Modulus10(digits As String) as String
  //
  // Confirm the digits are really, well, digits
  //

  dim validator as new RegEx
  validator.SearchPattern = "\A\d+\z"
  if validator.Search( digits ) is nil then
    //
    // Raise an exception or something
    //
  end if

  static doublingTable() as string = array( "0", "2", "4", "6", "8", "1", "3", "5", "7", "9" )

  dim digitArr() as string = digits.Split( "" )
  for i as integer = digitArr.Ubound downto 0 step 2
    digitArr( i ) = doublingTable( digitArr( i ).Val )
  next

  dim sum as integer
  for each digit as string in digitArr
    sum = sum + digit.Val
  next

  dim check as integer = ( sum * 9 ) mod 10
  return str( check )
End Function

Public Function ValidateMod10(digits As String) as Boolean
  dim checkDigit as string = digits.Right( 1 )
  digits = digits.Left( digits.Len - 1 )

  return Modulus10( digits ) = checkDigit
End Function

```

```txt

ValididateMod10( "49927398716" ) = True
ValididateMod10( "49927398717" ) = False
ValididateMod10( "1234567812345678" ) = False
ValididateMod10( "1234567812345670" ) = True

```



## zkl


```zkl
fcn luhnTest(n){
   0 == (n.split().reverse().reduce(fcn(s,n,clk){
      s + if(clk.next()) n else 2*n%10 + n/5 },0,Walker.cycle(1,0)) %10)
}
```


```zkl
T(49927398716,49927398717,1234567812345678,1234567812345670)
.apply(luhnTest).println();
```

```txt

L(True,False,False,True)

```



## ZX Spectrum Basic


```ZXBasic
10 LET c$="49927398716": GO SUB 1000
20 LET c$="49927398717": GO SUB 1000
30 LET c$="1234567812345678": GO SUB 1000
40 LET c$="1234567812345670": GO SUB 1000
999 STOP
1000 REM *************
1001 REM * LUHN TEST *
1002 REM *************
1010 LET r$=""
1020 FOR i=LEN c$ TO 1 STEP -1
1030 LET r$=r$+c$(i)
1040 NEXT i
1050 LET s1=0: LET s2=0
1060 FOR i=1 TO LEN r$ STEP 2
1070 LET s1=s1+VAL r$(i)
1080 NEXT i
1090 FOR i=2 TO LEN r$ STEP 2
1100 LET s2sub=VAL r$(i)*2
1110 IF s2sub>=10 THEN LET s2sub=1+s2sub-10
1120 LET s2=s2+s2sub
1130 NEXT i
1140 LET s$=STR$ (s1+s2)
1150 IF s$(LEN s$)="0" THEN PRINT c$;" VALID!": LET retval=1: RETURN
1160 PRINT c$;" INVALID!": LET retval=0: RETURN

```

```txt

49927398716 VALID!
49927398717 INVALID!
1234567812345678 INVALID!
1234567812345670 VALID!

```

