+++
title = "Integer comparison"
description = ""
date = 2019-10-17T04:22:17Z
aliases = []
[extra]
id = 2019
[taxonomies]
categories = []
tags = []
+++

{{task|Basic Data Operations}}
[[Category:Arithmetic operations]]
{{basic data operation}}
[[Category:Simple]]

Get two integers from the user.

Then,   display a message if the first integer is:
::::*   less than,
::::*   equal to,   or
::::*   greater than
the second integer.


Test the condition   ''for each case separately'',   so that   ''all three comparison operators are used''   in the code.


;Related task:
*   [[String comparison]]





## 360 Assembly

Input is done by a standard register 1 pointed parameter list.

```360asm
INTCOMP  PROLOG
*                            Reg1=Addr(Addr(argA),Addr(argB))
         L     2,0(1)        Reg2=Addr(argA)
         L     3,4(1)        Reg3=Addr(argB)
         L     4,0(2)        Reg4=argA
         L     5,0(3)        Reg5=argA
         ST    4,A           Store R4 in A
         ST    5,B           Store R5 in B
*                            if (A < B)
         L     0,A           load R0
         C     0,B           compare
         BL    LOWER         branch if lower
*                            if (A = B)
         L     0,A           load R0
         C     0,B           compare
         BE    EQUAL         branch if equal
*                            if (A < B)
         L     0,A           load R0
         C     0,B           compare
         BH    GREATER       branch if higher
*                            other case ?
         B     RETURN
LOWER    WTO   'A<B'
         B     RETURN
EQUAL    WTO   'A=B'
         B     RETURN
GREATER  WTO   'A>B'
         B     RETURN
*
RETURN   EPILOG
A        DS    F             31-bit signed integer
B        DS    F             31-bit signed integer
         END
```



## 6502 Assembly

Code is called as a subroutine (i.e. JSR Compare).
Specific OS/hardware routines for user input and printing are left unimplemented.

```6502asm
Compare		PHA			;push Accumulator onto stack
		JSR GetUserInput	;routine not implemented
		;integers to compare now in memory locations A and B
		LDA A
		CMP B			;sets flags as if a subtraction (a - b) had been carried out
		BCC A_less_than_B	;branch if carry clear
		BEQ A_equals_B		;branch if equal
		;else A greater than B
		JSR DisplayAGreaterThanB;routine not implemented
		JMP Done
A_less_than_B:	JSR DisplayALessThanB	;routine not implemented
		JMP Done
A_equals_B:	JSR DisplayAEqualsB	;routine not implemented
Done:		PLA			;restore Accumulator from stack
		RTS			;return from subroutine
```



## 8051 Assembly

Input/output is specific to hardware; this code assumes the integers are in registers a and b.
There is only one comparison instruction to use: 'not equal'.

```asm
compare:
	push psw
	cjne a, b, clt
	; a == b
	; implement code here
	jmp compare_
clt:
	jc lt
	; a > b
	; implement code here
	jmp compare_
lt:
	; a < b
	; implement code here
compare_:
	pop psw
	ret
```

Testing for (a <= b) or (a >= b) can be performed by changing the jumps.


## 8th

The user would put the numbers on the stack and then invoke 'compare':

```forth

: compare \ n m --
  2dup n:= if "They are equal" . cr then
  2dup n:< if "First less than second" . cr then
  n:> if "First greater than second" . cr then ;

```

Alternatively one could use the "n:cmp" word


## AArch64 Assembly

{{works with|aarch64-linux-gnu-as/qemu-aarch64}}
Compare once (<code>cmp x0, x1</code>) and use conditional branching (<code>b.eq</code> and <code>b.gt</code>).
<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	mov x0, #123
	mov x1, #456
	mov x29, sp
	bl integer_compare // integer_compare(123, 456);
	mov x0, #-123
	mov x1, #-456
	bl integer_compare // integer_compare(-123, -456);
	mov x0, #123
	mov x1, #123
	bl integer_compare // integer_compare(123, 123);
	ldp x29, x30, [sp], 16
	mov x0, #0
	b _exit // exit(0);

// void integer_compare(long long x, long long y) - compare two signed integers and print a message
integer_compare:
	cmp x0, x1
	mov x0, #STDOUT
	b.eq 1f
	b.gt 2f
	// x < y
	ldr x1, =msg_lt
	mov x2, #17
	b _write
1:	// x == y
	ldr x1, =msg_eq
	mov x2, #16
	b _write
2:	// x > y
	ldr x1, =msg_gt
	mov x2, #20
	b _write

msg_lt:
	.ascii "x is less than y\n"
msg_eq:
	.ascii "x is equal to y\n"
msg_gt:
	.ascii "x is greater than y\n"
.align 4

//////////////// system call wrappers
// ssize_t _write(int fd, void *buf, size_t count)
_write:
	stp x29, x30, [sp, -16]!
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0
	ldp x29, x30, [sp], 16
	ret

// void _exit(int retval)
_exit:
	mov x8, #SVC_EXIT
	svc #0
```



## ABAP

This works in ABAP version 7.40 and above. Note that empty input is evaluated to 0.


```ABAP

report z_integer_comparison.

parameters: a type int4, b type int4.

data(comparison_result) = cond string(
  when a < b " can be replaced by a lt b
  then |{ a } is less than { b }|
  when a = b " can be replaced by a eq b
  then |{ a } is equal to { b }|
  when a > b " can be replaced by a gt b
  then |{ a } is greater than { b }| ).

write comparison_result.

```



## Ada


```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_Io;

procedure Compare_Ints is
   A, B : Integer;
begin
   Get(Item => A);
   Get(Item => B);

   -- Test for equality
   if A = B then
      Put_Line("A equals B");
   end if;

   -- Test For Less Than
   if A < B then
      Put_Line("A is less than B");
   end if;

   -- Test For Greater Than
   if A > B then
      Put_Line("A is greater than B");
   end if;
end Compare_Ints;
```



## Aime


```aime
void
output(integer a, integer b, text condition)
{
    o_integer(a);
    o_text(condition);
    o_integer(b);
    o_byte('\n');
}


integer
main(void)
{
    if (a < b) {
	output(a, b, " is less then ");
    }

    if (a == b) {
	output(a, b, " is equal to ");
    }

    if (a > b) {
	output(a, b, " is greater than ");
    }

    return 0;
}
```

Run as:

```txt
aime FILE integer a 33 integer b 133
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Note: the standard includes the characters "&le;", "&ge;" and "&ne;".
These appear in the character sets [[wp:GOST 10859|GOST 10859]], [http://www.w3.org/TR/REC-MathML/chap6/ISOTECH1.html ISOtech] and
IBM's [[:wp:EBCDIC|EBCDIC]] e.g. code page [http://www.tachyonsoft.com/cp00293.htm 293],
and in extended ASCII code pages [http://www.tachyonsoft.com/cp00907.htm 910] & [http://www.tachyonsoft.com/cp00907.htm 910]

The above distributions of both [[ALGOL 68G]] and [[ELLA ALGOL 68]] compilers only
allow [[wp:ASCII|ASCII]] characters (ASCII has neither "&le;", "&ge;" nor "&ne;" characters).

```algol68
main: (
  INT a, b;
  read((a, space, b, new line));

  IF a <= b OR a LE b # OR a ≤ b # THEN
    print((a," is less or equal to ", b, new line))
  FI;
  IF a < b OR a LT b THEN
    print((a," is less than ", b, new line))
  ELIF a = b OR a EQ b THEN
    print((a," is equal to ", b, new line))
  ELIF a > b OR a GT b THEN
    print((a," is greater than ", b, new line))
  FI;
  IF a /= b OR a NE b # OR a ≠ b # THEN
    print((a," is not equal to ", b, new line))
  FI;
  IF a >= b OR a GE b # OR a ≥ b # THEN
    print((a," is greater or equal to ", b, new line))
  FI
)
```

{{out}}

```txt

+3 is less or equal to          +4
         +3 is less than          +4
         +3 is not equal to          +4

```



## ALGOL W


```algolw
begin

    integer a, b;

    write( "first  number: " );
    read( a );
    write( "second number: " );
    read( b );

    if a < b then write( a, " is less    than ", b );
    if a = b then write( a, " is equal   to   ", b );
    if a > b then write( a, " is greater than ", b );

end.
```



## AppleScript


```AppleScript
set n1 to text returned of (display dialog "Enter the first number:" default answer "") as integer
set n2 to text returned of (display dialog "Enter the second number:" default answer "") as integer
set msg to {n1}
if n1 < n2 then
	set end of msg to " is less than "
else if n1 = n2 then
	set end of msg to " is equal to "
else if n1 > n2 then
	set end of msg to " is greater than "
end if
set end of msg to n2
return msg as string
```


Or...

```AppleScript
set n1 to text returned of (display dialog "Enter the first number:" default answer "") as integer
set n2 to text returned of (display dialog "Enter the second number:" default answer "") as integer
if n1 < n2 then return "" & n1 & " is less than " & n2
if n1 = n2 then return "" & n1 & " is equal to " & n2
if n1 > n2 then return "" & n1 & " is greater than " & n2
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program comparNumber.s   */

/* Constantes    */
.equ BUFFERSIZE,   100
.equ STDIN,  0     @ Linux input console
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ READ,   3     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessNum1: .asciz "Enter number 1 : \n"
szMessNum2: .asciz "Enter number 2: \n"
szMessEqual: .asciz "Number 1 and number 2 are equals.\n"
szMessSmall: .asciz "Number 1 smaller than number 2.\n"
szMessLarge: .asciz "Number 1 larger than number 2.\n"
szCarriageReturn:  .asciz "\n"

/* UnInitialized data */
.bss
sBuffer:    .skip    BUFFERSIZE

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */
    ldr r0,iAdrszMessNum1 @ message address
    ldr r1,iAdrsBuffer   @ buffer address
    mov r2,#BUFFERSIZE
    bl numberEntry
    mov r5,r0               @ save number 1 -> r5
    ldr r0,iAdrszMessNum2  @ message address
	ldr r1,iAdrsBuffer   @ buffer address
	mov r2,#BUFFERSIZE
    bl numberEntry
	cmp r5,r0         @ compar number 1 and number 2
    beq equal
	blt small
	bgt large
	@ never !!
	b 100f
equal:
    ldr r0,iAdrszMessEqual    @ message address
	b aff
small:
    ldr r0,iAdrszMessSmall    @ message address
	b aff
large:
    ldr r0,iAdrszMessLarge    @ message address
	b aff
aff:
    bl affichageMess      @ display message


100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call

iAdrszMessNum1:  .int szMessNum1
iAdrszMessNum2: .int  szMessNum2
iAdrszMessEqual: .int szMessEqual
iAdrszMessSmall: .int szMessSmall
iAdrszMessLarge: .int szMessLarge
iAdrsBuffer:   .int  sBuffer
iAdrszCarriageReturn:  .int  szCarriageReturn
/******************************************************************/
/*     Number entry with display message and conversion number    */
/******************************************************************/
/* r0 contains message address */
/* r1 contains buffer address
/* r2 contains buffersize     */
/* r0 return a number          */
numberEntry:
    push {fp,lr}         @ save  registres */
    push {r4,r6,r7}          @ save others registers
    mov r4,r1              @ save buffer address -> r4
    bl affichageMess
    mov r0,#STDIN         @ Linux input console
    //ldr r1,iAdrsBuffer   @ buffer address
    //mov r2,#BUFFERSIZE   @ buffer size
    mov r7, #READ         @ request to read datas
    swi 0                  @ call system
    mov r1,r4              @ buffer address
    mov r2,#0                @ end of string
    strb r2,[r1,r0]         @ store byte at the end of input string (r0
    @
    mov r0,r4              @ buffer address
    bl conversionAtoD    @ conversion string in number in r0

100:
    pop {r4,r6,r7}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
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

 /******************************************************************/
/*     Convert a string to a number stored in a registry          */
/******************************************************************/
/* r0 contains the address of the area terminated by 0 or 0A */
/* r0 returns a number                           */
conversionAtoD:
    push {fp,lr}         @ save 2 registers
    push {r1-r7}         @ save others registers
    mov r1,#0
    mov r2,#10           @ factor
    mov r3,#0            @ counter
    mov r4,r0            @ save address string -> r4
    mov r6,#0            @ positive sign by default
    mov r0,#0            @ initialization to 0
1:     /* early space elimination loop */
    ldrb r5,[r4,r3]     @ loading in r5 of the byte located at the beginning + the position
    cmp r5,#0            @ end of string -> end routine
    beq 100f
    cmp r5,#0x0A        @ end of string -> end routine
    beq 100f
    cmp r5,#' '          @ space ?
    addeq r3,r3,#1      @ yes we loop by moving one byte
    beq 1b
    cmp r5,#'-'          @ first character is -
    moveq r6,#1         @  1 -> r6
    beq 3f              @ then move on to the next position
2:   /* beginning of digit processing loop */
    cmp r5,#'0'          @ character is not a number
    blt 3f
    cmp r5,#'9'          @ character is not a number
    bgt 3f
    /* character is a number */
    sub r5,#48
    ldr r1,iMaxi       @ check the overflow of the register
    cmp r0,r1
    bgt 99f            @ overflow error
    mul r0,r2,r0         @ multiply par factor 10
    add r0,r5            @ add to  r0
3:
    add r3,r3,#1         @ advance to the next position
    ldrb r5,[r4,r3]     @ load byte
    cmp r5,#0            @ end of string -> end routine
    beq 4f
    cmp r5,#0x0A            @ end of string -> end routine
    beq 4f
    b 2b                 @ loop
4:
    cmp r6,#1            @ test r6 for sign
    moveq r1,#-1
    muleq r0,r1,r0       @ if negatif, multiply par -1
    b 100f
99:  /* overflow error */
    ldr r0,=szMessErrDep
    bl   affichageMess
    mov r0,#0      @ return  zero  if error
100:
    pop {r1-r7}          @ restaur other registers
    pop {fp,lr}          @ restaur   2 registers
    bx lr                 @return procedure
/* constante program */
iMaxi: .int 1073741824
szMessErrDep:  .asciz  "Too large: overflow 32 bits.\n"


```



## Arturo


```arturo
"enter a value for a:"
a $(toNumber $(strip $(input)))
"enter a value for b:"
b $(toNumber $(strip $(input)))

if a<b { print "`a` is less than `b`" }
if a>b { print "`a` is greater than `b`" }
```

{{out}}

```txt
enter a value for a:
10
enter a value for b:
20
10 is less than 20
```



## Astro


```python
let a = input('Enter value of a: ')
let b = input('Enter value of b: ')

if a < b:
    print 'a is less than b'
elif a > b:
    print 'a is greater than b'
elif a == b:
    print 'a is equal to b'
```



## AutoHotkey

Error checking is performed automatically by attaching UpDowns to each of the Edit controls. UpDown controls always yield an in-range number, even when the user has typed something non-numeric or out-of-range in the Edit control.
The default range is 0 to 100.

```autohotkey
Gui, Add, Edit
Gui, Add, UpDown, vVar1
Gui, Add, Edit
Gui, Add, UpDown, vVar2
Gui, Add, Button, Default, Submit
Gui, Show
Return

ButtonSubmit:
  Gui, Submit, NoHide
  If (Var1 = Var2)
    MsgBox, % Var1 "=" Var2
  Else If (Var1 < Var2)
    MsgBox, % Var1 "<" Var2
  Else If (Var1 > Var2)
    MsgBox, % Var1 ">" Var2
Return

GuiClose:
  ExitApp
```



## AWK


```awk
/[0-9]* [0-9]*/{
		if ($1 == $2) print $1, "is equal to", $2
		if ($1 < $2) print $1, "is less than", $2
		if ($1 > $2) print $1, "is greater than", $2
		}
```


In awk, a double equals symbol is required to test for equality.
A single equals sign is used for assignment, and will cause a bug if it is used within a boolean expression:


```awk
# This code contains a bug
IF (n=3) PRINT "n is equal to 3"    # The incorrectly used equals sign will set n to a value of 3
```



## Axe


```axe
Lbl FUNC
If r₁<r₂
 Disp "LESS THAN",i
End
If r₁=r₂
 Disp "EQUAL TO",i
End
If r₁>r₂
 Disp "GREATER THAN",i
End
Return
```



## BASIC


=
## BaCon
=

```freebasic

INPUT "Enter first number " ,a
INPUT "Enter second number " ,b
IF a < b THEN PRINT  a ," is less than ", b
IF a = b THEN PRINT  a, " is equal to ", b
IF a > b THEN PRINT  a, " is greater than ", b
```



{{works with|QuickBasic|4.5}}

```qbasic
CLS
INPUT "a, b"; a, b 'remember to type the comma when you give the numbers
PRINT "a is ";
IF a < b THEN PRINT "less than ";
IF a = b THEN PRINT "equal to ";
IF a > b THEN PRINT "greater than ";
PRINT "b"
```


=
## Applesoft BASIC
=

```ApplesoftBasic
10 INPUT "ENTER TWO INTEGERS: "; A%, B%
20 A$(0) = "NOT "
30 PRINT A% " IS " A$(A% < B%) "LESS THAN " B%
40 PRINT A% " IS " A$(A% = B%) "EQUAL TO " B%
50 PRINT A% " IS " A$(A% > B%) "GREATER THAN " B%
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Enter A: ":A
110 INPUT PROMPT "Enter B: ":B
120 IF A<B THEN
130   PRINT A;"is lesss than ";B
140 ELSE IF A=B THEN
150   PRINT A;"is equal to ";B
160 ELSE
170   PRINT A;"is greater than ";B
180 END IF
```


or

<lang IS-BASIC>100 INPUT PROMPT "Enter A: ":A
110 INPUT PROMPT "Enter B: ":B
120 SELECT CASE A
130 CASE IS<B
140   PRINT A;"is lesss than ";B
150 CASE IS=B
160   PRINT A;"is equal to ";B
170 CASE ELSE
180   PRINT A;"is greater than ";B
190 END SELECT
```



## Batch File


```dos
@echo off
setlocal EnableDelayedExpansion
set /p a="A: "
set /p b="B: "
if %a% LSS %b% (
  echo %a% is less than %b%
) else ( if %a% GTR %b% (
  echo %a% is greater than %b%
) else ( if %a% EQU %b% (
  echo %a% is equal to %b%
)))
```

{{Out}}

```txt
C:\Test>IntegerComparison.bat
A: 5
B: 3
5 is greater than 3
```



## BBC BASIC


```bbcbasic
      INPUT "Enter two numbers separated by a comma: " a, b
      CASE TRUE OF
        WHEN a < b: PRINT ;a " is less than "; b
        WHEN a = b: PRINT ;a " is equal to "; b
        WHEN a > b: PRINT ;a " is greater than "; b
      ENDCASE
```



## bc

{{Works with|GNU bc}}
(POSIX bc doesn't have I/O functions/statements (i.e. <code>read</code> and <code>print</code>) but the rest of the code would work.)

```bc
a = read()
b = read()
if (a < b) print "a is smaller than b\n"
if (a > b) print "a is greater than b\n"
if (a == b) print "a is equal to b\n"
quit
```




## Befunge

Befunge only has the greater-than operator (backtick `).
The branch commands (underline _ and pipe |) test for zero.


```befunge
v                     v  ">"   $<
>&&"=A",,\:."=B ",,,\: .55+,-:0`|
                      v  "<" _v#<
   @,+55,," B",,,"A " <  "="  <
```



## Bracmat


```bracmat
  get$:?A
& get$:?B
& (!A:!B&out$"A equals B"|)
& (!A:<!B&out$"A is less than B"|)
& (!A:>!B&out$"A is greater than B"|);
```



## Brat


```brat
first = ask("First integer: ").to_i
second = ask("Second integer: ").to_i

when { first > second } { p "#{first} is greater than #{second}" }
     { first < second } { p "#{first} is less than #{second}" }
     { first == second } { p "#{first} is equal to #{second}" }
```



## Burlesque



```burlesque

blsq ) "5 6"ps^pcm+.{"The first one is less than the second one""They are both equal""The second one is less than the first one"}\/!!sh
The first one is less than the second one
blsq ) "6 6"ps^pcm+.{"The first one is less than the second one""They are both equal""The second one is less than the first one"}\/!!sh
They are both equal
blsq ) "6 5"ps^pcm+.{"The first one is less than the second one""They are both equal""The second one is less than the first one"}\/!!sh
The second one is less than the first one

```



## C


```c
#include <stdio.h>

int main()
{
  int a, b;
  scanf("%d %d", &a, &b);

  if (a < b)
    printf("%d is less than %d\n", a, b);

  if (a == b)
    printf("%d is equal to %d\n", a, b);

  if (a > b)
    printf("%d is greater than %d\n", a, b);

  return 0;
}
```



## ChucK

<lang>
fun void intComparison (int one, int two)
{
    if(one < two) <<< one, " is less than ", two >>>;
    if(one == two) <<< one, " is equal than ", two >>>;
    if(one > two) <<< one, " is greater than ", two >>>;
}
//  uncomment next line and change values to test
// intComparison (2,4);

```



## C++


```cpp
#include <iostream>

int main()
{
  int a, b;

  if (!(std::cin >> a >> b)) {
    std::cerr << "could not read the numbers\n";
    return 1;
  }

  // test for less-than
  if (a < b)
    std::cout << a << " is less than " << b << "\n";

  // test for equality
  if (a == b)
    std::cout << a << " is equal to " << b << "\n";

  // test for greater-than
  if (a > b)
    std::cout << a << " is greater than " << b << "\n";
}
```


=={{header|C sharp|C#}}==

```csharp
using System;

class Program
{
    static void Main()
    {
        int a = int.Parse(Console.ReadLine());
        int b = int.Parse(Console.ReadLine());
        if (a < b)
            Console.WriteLine("{0} is less than {1}", a, b);
        if (a == b)
            Console.WriteLine("{0} equals {1}", a, b);
        if (a > b)
            Console.WriteLine("{0} is greater than {1}", a, b);
    }
}
```



## Clean


```clean
import StdEnv

compare a b
    | a < b = "A is less than B"
    | a > b = "A is more than B"
    | a == b = "A equals B"

Start world
    # (console, world) = stdio world
      (_, a, console) = freadi console
      (_, b, console) = freadi console
    = compare a b
```



## Clipper


```clipper
   Function Compare(a, b)
   IF a < b
      ? "A is less than B"
   ELSEIF a > b
      ? "A is more than B"
   ELSE
      ? "A equals B"
   ENDIF
   Return Nil

```



## Clojure

Creates an infinite sequence of calls to "read an object from the user", and assigns the first two elements to a and b, without evaluating the rest.
It evaluates the when/println body three times, each time with op and string bound to their corresponding entries in the list of three operator/string pairs.
Note that this does no validation on input: if the user inputs a string then an exception will be thrown.

```Clojure
(let [[a b] (repeatedly read)]
  (doseq [[op string] [[< "less than"]
                       [> "greater than"]
                       [= "equal to"]]]
    (when (op a b)
      (println (str a " is " string " " b)))))
```



## CMake


```cmake
# Define A and B as integers. For example:
#   cmake -DA=3 -DB=5 -P compare.cmake

# The comparisons can take variable names, or they can take numbers.
# So these act all the same:
#   A LESS B
#   ${A} LESS ${B}
#   A LESS ${B}
#   ${A} LESS B

if(A LESS B)
  message(STATUS "${A} is less than ${B}")
endif()
if(A EQUAL B)
  message(STATUS "${A} is equal to ${B}")
endif()
if(A GREATER B)
  message(STATUS "${A} is greater than ${B}")
endif()
```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Compare.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A PIC 9(10).
       01  B PIC 9(10).

       PROCEDURE DIVISION.
           DISPLAY "First number: " WITH NO ADVANCING
           ACCEPT A
           DISPLAY "Second number: " WITH NO ADVANCING
           ACCEPT B

*          *> Note: Longer verbal forms may be used instead of symbols
*          *> e.g. 'IS GREATER THAN' instead '<'
           IF A < B
               DISPLAY A " is less than " B
           ELSE IF A = B
               DISPLAY A " is equal to " B
           ELSE IF A > B
               DISPLAY A " is larger than " B
           END-IF.

           GOBACK
           .
```



## ColdFusion


* '''Less than:''' LT
* '''Less than or equal to:''' LTE
* '''Greater than:''' GT
* '''Greater than or equal to:''' GTE
* '''Equal to:''' EQ
* '''Not equal to:''' NEQ


### In CFML



```cfm
<cffunction name="CompareInteger">
    <cfargument name="Integer1" type="numeric">
    <cfargument name="Integer2" type="numeric">
    <cfset VARIABLES.Result = "" >
    <cfif ARGUMENTS.Integer1 LT ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is less than " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfif ARGUMENTS.Integer1 LTE ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is less than or equal to " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfif ARGUMENTS.Integer1 GT ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is greater than " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfif ARGUMENTS.Integer1 GTE ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is greater than or equal to " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfif ARGUMENTS.Integer1 EQ ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is equal to " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfif ARGUMENTS.Integer1 NEQ ARGUMENTS.Integer2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is not equal to " & ARGUMENTS.Integer2 & ")" >
    </cfif>
    <cfreturn VARIABLES.Result >
</cffunction>
```



### In CFScript



```cfm><cfscript

	function CompareInteger( Integer1, Integer2 ) {
		VARIABLES.Result = "";
		if ( ARGUMENTS.Integer1 LT ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is less than " & ARGUMENTS.Integer2 & ")";
		}
		if ( ARGUMENTS.Integer1 LTE ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is less than or equal to " & ARGUMENTS.Integer2 & ")";
		}
		if ( ARGUMENTS.Integer1 GT ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is greater than " & ARGUMENTS.Integer2 & ")";
		}
		if ( ARGUMENTS.Integer1 GTE ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is greater than or equal to " & ARGUMENTS.Integer2 & ")";
		}
		if ( ARGUMENTS.Integer1 EQ ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is equal to " & ARGUMENTS.Integer2 & ")";
		}
		if ( ARGUMENTS.Integer1 NEQ ARGUMENTS.Integer2 ) {
			VARIABLES.Result = VARIABLES.Result & "(" & ARGUMENTS.Integer1 & " is not equal to " & ARGUMENTS.Integer2 & ")";
		}
		return VARIABLES.Result;
	}
</cfscript>
```



## Common Lisp

You can type this directly into a REPL:


```lisp
(let ((a (read *standard-input*))
      (b (read *standard-input*)))
    (cond
      ((not (numberp a)) (format t "~A is not a number." a))
      ((not (numberp b)) (format t "~A is not a number." b))
      ((< a b) (format t "~A is less than ~A." a b))
      ((> a b) (format t "~A is greater than ~A." a b))
      ((= a b) (format t "~A is equal to ~A." a b))
      (t (format t "Cannot determine relevance between ~A and ~B!" a b)))))
```


After hitting enter, the REPL is expecting the two numbers right away.
You can enter the two numbers, and the result will print immediately.
Alternatively, you can wrap this code in a function definition:


```lisp
(defun compare-integers ()
  (let ((a (read *standard-input*))
        (b (read *standard-input*)))
    (cond
      ((not (numberp a)) (format t "~A is not a number." a))
      ((not (numberp b)) (format t "~A is not a number." b))
      ((< a b) (format t "~A is less than ~A." a b))
      ((> a b) (format t "~A is greater than ~A." a b))
      ((= a b) (format t "~A is equal to ~A." a b))
      (t (format t "Cannot determine relevance between ~A and ~B!" a b)))))
```


Then, execute the function for better control:
 (compare-integers)


## Computer/zero Assembly

The only conditional instruction we have is <tt>BRZ</tt> (branch on accumulator zero). We can therefore test for equality very quickly. To test for "greater than" or "less than", however, requires a loop.

If you run this program, it will halt awaiting user input. Toggle in the value of <math>x</math>, then click <tt>Enter</tt>, then toggle in <math>y</math>, then <tt>Enter</tt>, and then <tt>Run</tt>. <math>x</math> and <math>y</math> must both be unsigned eight-bit integers. The computer will halt with the accumulator storing 1 if <math>x</math>><math>y</math>, 0 if <math>x</math>=<math>y</math>, or -1 if <math>x</math><<math>y</math>; and it will be ready for a fresh pair of integers to be entered.

```czasm
start:  STP        ; get input

x:      NOP
y:      NOP

        LDA  x
        SUB  y
        BRZ  start ; x=y, A=0

loop:   LDA  x
        SUB  one
        BRZ  x<y
        STA  x

        LDA  y
        SUB  one
        BRZ  x>y
        STA  y

        JMP  loop

x>y:    LDA  one   ; A := 1
        JMP  start

x<y:    SUB  one   ; A := 0-1
        JMP  start

one:         1
```



## D


```d
void main() {
    import std.stdio, std.conv, std.string;

    int a = 10, b = 20;
    try {
        a = readln.strip.to!int;
        b = readln.strip.to!int;
    } catch (StdioException) {}

    if (a < b)
        writeln(a, " is less than ", b);

    if (a == b)
        writeln(a, " is equal to ", b);

    if (a > b)
        writeln(a, " is greater than ", b);
}
```

{{out}}

```txt
10 is less than 20
```


## DCL


```DCL
$ inquire a "Please provide an integer"
$ inquire b "Please provide another"
$ if a .lt. b then $ write sys$output "the first integer is less"
$ if a .eq. b then $ write sys$output "the integers have the same value"
$ if a .gt. b then $ write sys$output "the first integer is greater"
```

{{out}}

```txt
$ @integer_comparison
Please provide an integer: 0
Please provide another: -3
the first integer is greater
$ @integer_comparison
Please provide an integer: -2000
Please provide another: 12355
the first integer is less
$ @integer_comparison
Please provide an integer: 314
Please provide another: 314
the integers have the same value
```



## Delphi

:''Slightly different than the [[#Pascal|Pascal]] example''


```Delphi
program IntegerCompare;

{$APPTYPE CONSOLE}

var
  a, b: Integer;
begin
  Readln(a, b);
  if a < b then Writeln(a, ' is less than ', b);
  if a = b then Writeln(a, ' is equal to ', b);
  if a > b then Writeln(a, ' is greater than ', b);
end.
```



## Dyalect


{{trans|Clipper}}


```Dyalect
func compare(a, b) {
   if a < b {
      "A is less than B"
   } else if a > b {
      "A is more than B"
   } else {
      "A equals B"
   }
}
```



## E


```e
def compare(a :int, b :int) {
  println(if (a < b)        { `$a < $b` } \
          else if (a <=> b) { `$a = $b` } \
          else if (a > b)   { `$a > $b` } \
          else              { `You're calling that an integer?` })
}
```



## EasyLang

<lang>a = number input
b = number input
if a < b
  print "less"
.
if a = b
  print "equal"
.
if a > b
  print "greater"
.
```



## ECL


```ECL

CompareThem(INTEGER A,INTEGER B) := FUNCTION
  Result            := A <=> B;
  STRING ResultText := CASE (Result,1 => 'is greater than', 0 => 'is equal to','is less than');
  RETURN A + ' ' + TRIM(ResultText) + ' ' + B;
END;

CompareThem(1,2); //Shows "1 is less than 2"
CompareThem(2,2); //Shows "2 is equal to 2"
CompareThem(2,1); //Shows "2 is greater than 1"

```



## EDSAC order code

The EDSAC offers two conditional branching orders, <tt>E</tt> (branch if the accumulator >= 0) and <tt>G</tt> (branch if the accumulator < 0). Testing for equality thus requires two operations.

```edsac
[ Integer comparison

### ============


  A program for the EDSAC

  Illustrates the use of the E
  (branch on accumulator sign
  bit clear) and G (branch on
  accumulator sign bit set)
  orders

  The integers to be tested, x
  and y, should be stored in
  addresses 13@ and 14@

  Output: the program causes the
  machine to print
    '+' if x > y,
    '=' if x = y,
    '-' if x < y.

  Works with Initial Orders 2   ]

        T56K  [ load point      ]
        GK    [ base address    ]

        O15@  [ figure shift    ]

        A13@  [ a = x           ]
        S14@  [ a -= y          ]
        G10@  [ if a<0 go to 10 ]

        S12@  [ a -= 1          ]
        E8@   [ if a>=0 go to 8 ]

        O17@  [ write '='       ]
        ZF    [ halt            ]

[  8 ]  O16@  [ write '+'       ]
        ZF    [ halt            ]

[ 10 ]  O18@  [ write '-'       ]
        ZF    [ halt            ]

[ 12 ]  P0D   [ const: 1        ]

[ 13 ]  P16D  [ x = 37          ]
[ 14 ]  P14F  [ y = 28          ]

[ 15 ]  #F    [ figure shift    ]
[ 16 ]  ZF    [ + character     ]
[ 17 ]  VF    [ = character     ]
[ 18 ]  AF    [ - character     ]

        EZPF  [ begin execution ]
```



## Efene


since if does pattern matching the else is required to avoid the application from crashing


```efene
compare = fn (A, B) {
    if A == B {
        io.format("~p equals ~p~n", [A, B])
    }
    else {
        ok
    }

    if A < B {
        io.format("~p is less than ~p~n", [A, B])
    }
    else {
        ok
    }

    if A > B {
        io.format("~p is greater than ~p~n", [A, B])
    }
    else {
        ok
    }
}

@public
run = fn () {
    compare(5, 5)
    compare(6, 5)
    compare(4, 5)
}

```



## Eiffel


```Eiffel
class
	APPLICATION
inherit
	ARGUMENTS
create
	make

feature {NONE} -- Initialization

	make
		local
			i, j: INTEGER_32
		do
			io.read_integer_32
			i := io.last_integer_32

			io.read_integer_32
			j := io.last_integer_32

			if i < j then
				print("first is less than second%N")
			end
			if i = j then
				print("first is equal to the second%N")
			end
			if i > j then
				print("first is greater than second%N")
			end
		end
end
```



## Elena

ELENA 4.x:

```elena
import extensions;

public program()
{
   var a := console.readLine().toInt();
   var b := console.readLine().toInt();

   if (a < b)
       { console.printLine(a," is less than ",b) };

   if (a == b)
       { console.printLine(a," equals ",b) };

   if (a > b)
       { console.printLine(a," is greater than ",b) }
}
```



## Elixir


```ELixir
{a,_} = IO.gets("Enter your first integer: ") |> Integer.parse
{b,_} = IO.gets("Enter your second integer: ") |> Integer.parse

cond do
  a < b ->
    IO.puts "#{a} is less than #{b}"
  a > b ->
    IO.puts "#{a} is greater than #{b}"
  a == b ->
    IO.puts "#{a} is equal to #{b}"
end
```



## Emacs Lisp


```Emacs Lisp

(progn
  (if (< 1 2) (insert "True\n") (insert "False\n") )
  (if (= 1 2) (insert "True\n") (insert "False\n") )
  (if (= 1 1) (insert "True\n") (insert "False\n") )
  (if (> 1 2) (insert "True\n") (insert "False\n") )
  (if (<= 1 2) (insert "True\n") (insert "False\n") )
  (if (>= 1 2) (insert "True\n") (insert "False\n") ))

```

<b>Output:</b>

```txt

True
False
True
False
True
False

```


… or …


```Emacs Lisp

(defun integer-comparison (a b)
  "Compare A to B and print the outcome in the message buffer."
  (interactive "nFirst integer ⇒\nnSecond integer ⇒")
  (cond
    ((< a b) (message "%d is less than %d." a b))
    ((> a b) (message "%d is greater than %d." a b))
    ((= a b) (message "%d is equal to %d." a b))))

```


Invoke from within Emacs Lisp (or eg, with M-:) as
```Emacs Lisp
(integer-comparison 12 42)
```

Or, use M-x integer-comparison <RET> and you'll be prompted for the two numbers.


## Erlang


```erlang

main() ->
    {ok, [N]} = io:fread("First integer: ", "~d"),
    {ok, [M]} = io:fread("First integer: ", "~d"),
    if
        N < M ->
            io:format("~b is less than ~b~n",[N,M]);
        N > M ->
            io:format("~b is greater than ~b~n",[N,M]);
        N == M ->
            io:format("~b is equal to ~b~n",[N,M])
    end.
    if
       N =< M ->
           io:format("~b is less than or equal to ~b~n",[N,M]);
       N >= M ->
           io:format("~b is greater than or equal to ~b~n",[N,M])
    end.


```



## Euphoria


```Euphoria
include get.e

integer a,b
a = floor(prompt_number("a = ",{}))
b = floor(prompt_number("b = ",{}))

puts(1,"a is ")
if a < b then
    puts(1,"less then")
elsif a = b then
    puts(1,"equal to")
elsif a > b then
    puts(1,"grater then")
end if
puts(1," b")
```



## Excel


Let's say you type in the values in cells A1 and B1, in C1, type in the following in a MS Excel 2010 sheet:


```excel

=IF($A1>$B1;concatenate($A1;" is greater than ";$B1);IF($A1<$B1;concatenate($A1;" is smaller than ";$B1);concatenate($A1;" is equal to ";$B1)))

```


In a Google Docs spreadsheet, that becomes :


```excel

=IF($A1>$B1,concatenate($A1," is greater than ",$B1),IF($A1<$B1,concatenate($A1," is smaller than ",$B1),concatenate($A1," is equal to ",$B1)))

```



## Factor


```factor
: example ( -- )
readln readln [ string>number ] bi@
[ > [ "A > B" print ] when ]
[ < [ "A < B" print ] when ]
[ = [ "A = B" print ] when ] 2tri ;

```



## FALSE

Only equals and greater than are available.

```false
^^ \$@$@$@$@\
>[\$," is greater than "\$,]?
\>[\$," is less than "\$,]?
=["characters are equal"]?
```



## Fantom


Uses Env.cur to access stdin and stdout.


```fantom
class Main
{
  public static Void main ()
  {
    try
    {
      Env.cur.out.print ("Enter number 1: ").flush
      num1 := Env.cur.in.readLine.toInt
      Env.cur.out.print ("Enter number 2: ").flush
      num2 := Env.cur.in.readLine.toInt

      if (num1 < num2)
        echo ("$num1 is smaller than $num2")
      else if (num1 == num2)
        echo ("$num1 is equal to $num2")
      else if (num1 > num2)
        echo ("$num1 is greater than $num2")
    }
    catch (Err e)
      echo ("You must enter two integers")
  }
}

```



## Fish

This example assumes you [http://www.esolangs.org/wiki/Fish#Input.2Foutput pre-populate] the stack with the two integers.

```Fish
l2=?vv ~<                                                        v o<
v   <>l?^"Please pre-populate the stack with the two integers."ar>l?^;
\$:@@:@)?v   v               ;oanv!!!?<
         >$n" is greater than "{r>ol1=^
/            <
\$:@@:@=?v   v           ;oanv!!!?<
         >$n" is equal to "{r>ol1=^
/            <
\$:@@:@(?v   v               ;oanv!!!?<
         >$n" is smaller than "{r>ol1=^
             >        v
/oo". "nooooo" and "n$<                                                            v o<
>"They're not equal, not greater than and not smaller than eachother... strange."ar>l?^;
```


The last three lines aren't really needed, because it will never become true :P but I included them to show a way to do some error checking.


## Forth


To keep the example simple, the word takes the two numbers from the stack.

```forth
: compare-integers ( a b -- )
   2dup < if ." a is less than b" then
   2dup > if ." a is greater than b" then
        = if ." a is equal to b" then ;
```



## Fortran

In ALL Fortran versions (including original 1950's era) you could use an "arithmetic IF" statement to compare using subtraction:

```fortran
program arithif
integer a, b

c        fortran 77 I/O statements, for simplicity
read(*,*) a, b

if ( a - b ) 10, 20, 30
10 write(*,*) a, ' is less than ', b
   goto 40

20 write(*,*) a, ' is equal to ', b
   goto 40

30 write(*,*) a, ' is greater than ', b
40 continue

end
```


In ANSI FORTRAN 66 or later you could use relational operators (.lt., .gt., .eq., etc.) and unstructured IF statements:

```fortran
program compare
integer a, b
c        fortran 77 I/O statements, for simplicity
read(*,*) a, b

if (a .lt. b) write(*, *) a, ' is less than ', b
if (a .eq. b) write(*, *) a, ' is equal to ', b
if (a .gt. b) write(*, *) a, ' is greater than ', b
end
```


In ANSI FORTRAN 77 or later you can use relational operators and structured IF statements:

```fortran
program compare
integer a, b
read(*,*) a, b

if (a .lt. b) then
  write(*, *) a, ' is less than ', b
else if (a .eq. b) then
  write(*, *) a, ' is equal to ', b
else if (a .gt. b) then
  write(*, *) a, ' is greater than ', b
end if

end
```


In ISO Fortran 90 or later you can use symbolic relational operators (<, >, ==, etc.)

```fortran
program compare
integer :: a, b
read(*,*) a, b

if (a < b) then
  write(*, *) a, ' is less than ', b
else if (a == b) then
  write(*, *) a, ' is equal to ', b
else if (a > b) then
  write(*, *) a, ' is greater than ', b
end if

end program compare
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As Integer x, y
Input "Please enter two integers, separated by a comma : ", x , y

If x < y Then
  Print x; " is less than "; y
End If

If x = y Then
  Print x; " is equal to "; y
End If

If x > y Then
  Print x; " is greater than "; y
End If

Print
Print "Press any key to exit"
Sleep
```



## friendly interactive shell


```fishshell
read a
read b

if test $a -gt $b
    echo Greater
else if test $a -lt $b
    echo Less
else if test $a -eq $b
    echo Equal
end
```



## Frink

All integers in Frink can be arbitrarily large.


```frink

[a,b] = eval[input["Enter numbers",["a","b"]]]
if a<b
   println["$a < $b"]
if a==b
   println["$a == $b"]
if a>b
   println["$a > $b"]

```


=={{header|F_Sharp|F#}}==

```fsharp
let compare_ints a b =
    let r =
        match a with
        | x when x < b -> -1, printfn "%d is less than %d" x b
        | x when x = b -> 0,  printfn "%d is equal to %d" x b
        | x when x > b -> 1,  printfn "%d is greater than %d" x b
        | x -> 0, printf "default condition (not reached)"
    fst r
```



## FunL


```funl
import console.readInt

a = readInt()
b = readInt()

val (_, c) = [((<), 'less than'), ((==), 'equal to'), ((>), 'greater than')].find( (compare, _) -> compare(a, b) ).get()

println( "$a is $c $b." )
```



## Gambas


```gambas
Public Sub Form_Open()
Dim sIn As String = InputBox("Enter 2 integers seperated by a comma")
Dim iFirst, iSecond As Integer

iFirst = Val(Split(sIn)[0])
iSecond = Val(Split(sIn)[1])

If iFirst < iSecond Then Print iFirst & " is smaller than " & iSecond & gb.NewLine
If iFirst > iSecond Then Print iFirst & " is greater than " & iSecond & gb.NewLine
If iFirst = iSecond Then Print iFirst & " is equal to " & iSecond

End
```

Output:

```txt

21 is greater than 18

```



## Go


```go
package main

import (
	"fmt"
	"log"
)

func main() {
	var n1, n2 int
	if _, err := fmt.Scan(&n1); err != nil {
		log.Fatal(err)
	}
	if _, err := fmt.Scan(&n2); err != nil {
		log.Fatal(err)
	}
	switch {
	case n1 < n2:
		fmt.Println(n1, "less than", n2)
	case n1 == n2:
		fmt.Println(n1, "equal to", n2)
	case n1 > n2:
		fmt.Println(n1, "greater than", n2)
	}
}
```



## FutureBasic

Note: Strictly speaking, it's preferable to use "==" when comparing integers as seen in this example. While the "=" sign will work as a comparison in most cases, technically it should be used for assignment, i.e. a = 3 when a is assigned the value of 3, as contrasted with a == 3, where the value of a is being compared with 3. FB will flag a warning when "==" is used to compare two single, doubles or floats since comparing real numbers can be inaccurate.

```futurebasic

include "ConsoleWindow"

dim as long n1, n2

input "Enter two numbers (separated by a comma) to compare: "; n1, n2

if n1 < n2  then print : print n1; " is less than"; n2
if n1 > n2  then print : print n1; " is greater than"; n2
if n1 == n2 then print : print n1; " equals"; n2

```



## Groovy



### Relational Operators


```groovy
def comparison = { a, b ->
    println "a ? b    = ${a} ? ${b}    = a ${a < b ? '<' : a > b ? '>' : a == b ? '==' : '?'} b"
}
```


Program:

```groovy
comparison(2000,3)
comparison(2000,300000)
comparison(2000,2000)
```


{{out}}

```txt
a ? b    = 2000 ? 3    = a > b
a ? b    = 2000 ? 300000    = a < b
a ? b    = 2000 ? 2000    = a == b
```


==="Spaceship" (compareTo) Operator===
Using spaceship operator and a lookup table:

```groovy
final rels = [ (-1) : '<', 0 : '==', 1 : '>' ].asImmutable()
def comparisonSpaceship = { a, b ->
    println "a ? b    = ${a} ? ${b}    = a ${rels[a <=> b]} b"
}
```


Program:

```groovy
comparison(2000,3)
comparison(2000,300000)
comparison(2000,2000)
```


{{out}}

```txt
a ? b    = 2000 ? 3    = a > b
a ? b    = 2000 ? 300000    = a < b
a ? b    = 2000 ? 2000    = a == b
```



## Harbour


```visualfoxpro
PROCEDURE Compare( a, b )

   IF a < b
      ? "A is less than B"
   ELSEIF a > b
      ? "A is more than B"
   ELSE
      ? "A equals B"
   ENDIF

   RETURN
```



## Haskell


```haskell
myCompare :: Integer -> Integer -> String
myCompare a b
  | a < b  = "A is less than B"
  | a > b  = "A is greater than B"
  | a == b = "A equals B"

main = do
  a <- readLn
  b <- readLn
  putStrLn $ myCompare a b
```

However, the more idiomatic and less error-prone way to do it in Haskell would be to use a compare function that returns type Ordering, which is either LT, GT, or EQ:

```haskell
myCompare a b = case compare a b of
                  LT -> "A is less than B"
                  GT -> "A is greater than B"
                  EQ -> "A equals B"
```



## hexiscript


```hexiscript
let a scan int
let b scan int

if a < b
  println a + " is less than " + b
endif

if a > b
  println a + " is greater than " + b
endif

if a = b
  println a + " is equal to " + b
endif
```



## HicEst


```hicest
DLG(NameEdit=a, NameEdit=b, Button='OK')

IF (a < b) THEN
    WRITE(Messagebox) a, ' is less than ', b
  ELSEIF(a == b) THEN
    WRITE(Messagebox) a, ' is equal to ', b
  ELSEIF(a > b) THEN
    WRITE(Messagebox) a, ' is greater than ', b
ENDIF
```



## Hy


```clojure
(def a (int (input "Enter value of a: ")))
(def b (int (input "Enter value of b: ")))

(print (cond [(< a b) "a is less than b"]
             [(> a b) "a is greater than b"]
             [(= a b) "a is equal to b"]))
```



## HolyC


```holyc
I64 *a, *b;
a = Str2I64(GetStr("Enter your first number: "));
b = Str2I64(GetStr("Enter your second number: "));

if (a < b)
  Print("%d is less than %d\n", a, b);

if (a == b)
  Print("%d is equal to %d\n", a, b);

if (a > b)
  Print("%d is greater than %d\n", a, b);
```



## i


```i
software {
	a = number(read(' '))
	b = number(read())

	if a < b
		print(a, " is less than ", b)
	end

	if a = b
		print(a, " is equal to ", b)
	end

	if a > b
		print(a, " is greater than ", b)
	end
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

until integer(a) do {
   writes("Enter the first integer a := ")
   write(a := read())
   }

until integer(b) do {
   writes("Enter the second integer b := ")
   write(b := read())
   }
writes("Then ")
write(a," < ", a < b)
write(a," = ", a = b)
write(a," > ", a > b)
end
```


{{out}}

```txt
#int_compare.exe
Enter the first integer a := 7
Enter the second integer b := 7
Then 7 = 7
```



## J

Comparison is accomplished by the verb <code>compare</code>, which provides logical-numeric output.
Text elaborating the output of <code>compare</code> is provided by <code>cti</code>:

```j
compare=: < , = , >

cti=: dyad define
  select  =. ;@#
  English =. ' is less than ';' is equal to ';' is greater than '
  x (":@[, (compare select English"_), ":@]) y
)
```

Examples of use:

```j
   4 compare 4
0 1 0
   4 cti 3
4 is greater than 3
```



## Java


```java
import java.io.*;

public class compInt {
   public static void main(String[] args) {
       try {
           BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

           int nbr1 = Integer.parseInt(in.readLine());
           int nbr2 = Integer.parseInt(in.readLine());

           if(nbr1<nbr2)
               System.out.println(nbr1 + " is less than " + nbr2);

           if(nbr1>nbr2)
                System.out.println(nbr1 + " is greater than " + nbr2);

           if(nbr1==nbr2)
                System.out.println(nbr1 + " is equal to " + nbr2);
       } catch(IOException e) { }
   }
}
```


## JavaScript


```javascript

// Using type coercion
function compare(a, b) {
  if (a==b) print(a + " equals " + b);
  if (a < b) print(a + " is less than " + b);
  if (a > b) print(a + " is greater than " + b);
}

// Without using type coercion and using standards
// Written for browsers
// assumption of a and b are both integers if typeof test passes
function compare (a, b) {
  if (typeof a === typeof b) {
    if (a === b) {
      document.writeln(a + " equals " + b);
    }
    if (a < b) {
      document.writeln(a + " is less than " + b);
    }
    if (a > b) {
      document.writeln(a + " is greater than " + b);
    }
  } else {
    // "1" and 1 are an example of this as the first is type string and the second is type number
    print(a + "{" + (typeof a) + "} and " + b + "{" + (typeof b) + "} are not of the same type and cannot be compared.");
  }
}

```



## Joy


```joy
#!/usr/local/bin/joy.exe
DEFINE
prompt  == "Please enter a number and <Enter>: " putchars;
newline == '\n putch;
putln   == put newline.

stdin		# F
prompt fgets	# S F
10 strtol	# A F
swap		# F A
dupd		# F A A
prompt fgets	# S F A A
10 strtol	# B F A A
popd		# B A A
dup		# B B A A
rollup		# B A B A
[<] [swap put "is less than "    putchars putln] [] ifte
[=] [swap put "is equal to "     putchars putln] [] ifte
[>] [swap put "is greater than " putchars putln] [] ifte
		# B A
quit.
```



## jq


```jq
# compare/0 compares the first two items if they are numbers,
# otherwise an "uncomparable" message is emitted.

def compare:
  def english:
    if .[0] < .[1] then "less than"
    elif .[0] == .[1] then "equal to"
    else "greater than"
    end;
  if (.[0]|type) == "number" and (.[1]|type) == "number" then
        "\(.[0]) is \(english) \(.[1])"
  else
       "\(.[0]) is uncomparable to \(.[1])"
  end ;

compare
```


Examples;

```jq

$ jq -s -r -f Integer_comparison.jq
1 2
1 is less than 2

$ jq -s -r -f Integer_comparison.jq
1 "a"
1 is uncomparable to a

```



## Julia


```Julia
function compare()
int1 = readline(STDIN)
int2 = readline(STDIN)
print(int1, " is ",
       int1 <  int2 ? "less than "    :
       int1 == int2 ? "equal to "     :
       int1 >  int2 ? "greater than " :
       "uncomparable to",
      int2)
end
```

{{Out}}

```txt
julia> compare()
3
5
3 is less than 5
```



## Kotlin


```scala
fun main(args: Array<String>) {
    val n1 = readLine()!!.toLong()
    val n2 = readLine()!!.toLong()
    println(when {
        n1 < n2 -> "$n1 is less than $n2"
        n1 > n2 -> "$n1 is greater than $n2"
        n1 == n2 -> "$n1 is equal to $n2"
        else -> ""
    })
}
```



## Lasso


```Lasso
local(
	number1	= integer(web_request -> param('number1')),
	number2	= integer(web_request -> param('number2'))
)

#number1 < #number2 ? 'Number 1 is less than Number 2' | 'Number 1 is not less than Number 2'
'<br />'
#number1 == #number2 ? 'Number 1 is the same as Number 2' | 'Number 1 is not the same as Number 2'
'<br />'
#number1 > #number2 ? 'Number 1 is greater than Number 2' | 'Number 1 is not greater than Number 2'
```


{{out}}

```txt
// with input of 2 & 2
Number 1 is not less than Number 2
Number 1 is the same as Number 2
Number 1 is not greater than Number 2
```



## Liberty BASIC

Verbose version:

```lb
input "Enter an integer for a.  ";a
input "Enter an integer for b.  ";b

'a=int(a):b=int(b) ???
print "Conditional evaluation."
if a<b then print "a<b " ; a ; " < " ; b
if a=b then print "a=b " ; a ; " = " ; b
if a>b then print "a>b " ; a ; " > " ; b

print "Select case evaluation."
select case
    case (a<b)
        print "a<b " ; a ; " < " ; b
    case (a=b)
        print "a=b " ; a ; " = " ; b
    case (a>b)
        print "a>b " ; a ; " > " ; b
end select


```


Concise:

```lb
input "Enter an integer for a.  ";a
input "Enter an integer for b.  ";b

for i = 1 to 3
    op$=word$("< = >", i)
    if eval("a"+op$+"b") then print "a"+op$+"b " ; a;" ";op$;" ";b
next

```



## LIL


```tcl
print "Enter two numbers separated by space"
set rc [readline]
set a [index $rc 0]
set b [index $rc 1]

if {$a < $b} {print "$a is less than $b"}
if {$a == $b} {print "$a is equal to $b"}
if {$a > $b} {print "$a is greater than $b"}
```



## Lingo

Lingo programs are normally not run in the console, so interactive user input is handled via GUI. To not clutter this page with GUI creation code, here only the comparison part of the task:

```lingo
on compare (a, b)
  if a < b then put a&" is less than "&b
  if a = b then put a&" is equal to "&b
  if a > b then put a&" is greater than "&b
end
```



## LiveCode


```LiveCode
ask question "Enter 2 numbers (comma separated)" with empty titled "Enter 2 numbers"
if it is not empty then
    put item 1 of it into num1
    put item 2 of it into num2
    if isnumber(num1) and isnumber(num2) then
        if num1 < num2 then answer num1 && "is less than" && num2
        if num1 is num2 then answer num1 && "is equal to" && num2
        if num1 > num2 then answer num1 && "is greater than" && num2

        -- alternative is to use switch case construct
        switch
            case (num1 < num2)
                answer num1 && "is less! than" && num2; break
            case (num1 > num2)
                answer num1 && "is greater! than" && num2; break
            case (num1 = num2)
                answer num1 && "equal! to" && num2
        end switch
    end if
end if
```



## LLVM

Note, this targets the mingw-32 ABI.

{{libheader|cstdlib}}


```llvm
; ModuleID = 'test.o'
;e means little endian
;p: { pointer size : pointer abi : preferred alignment for pointers }
;i same for integers
;v is for vectors
;f for floats
;a for aggregate types
;s for stack objects
;n: {size:size:size...}, best integer sizes
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
;this was compiled with mingw32; thus it must be linked to a compatible c library
target triple = "i386-mingw32"

; Declare string constants
@.str = private constant [6 x i8] c"%d %d\00", align 1 ; <[6 x i8]*> [#uses=1]
@.str1 = private constant [20 x i8] c"%d is less than %d\0A\00", align 1 ; <[20 x i8]*> [#uses=1]
@.str2 = private constant [19 x i8] c"%d is equal to %d\0A\00", align 1 ; <[19 x i8]*> [#uses=1]
@.str3 = private constant [23 x i8] c"%d is greater than %d\0A\00", align 1 ; <[23 x i8]*> [#uses=1]

;Declare main function (entry point). It does not throw any exceptions, and returns an integer of size 32.
define i32 @main() nounwind {
;Entry block
entry:
  ;Allocate the first integer, register %a will point to that
  %a = alloca i32, align 4                        ; <i32*> [#uses=4]
  ;Allocate the second integer, register %b will point to that
  %b = alloca i32, align 4                        ; <i32*> [#uses=4]
  ;Use the C standard library function scanf() to obtain input from users.
  ;Scanf takes a pointer to the string constant @.str, "%d %d\00", which will take two integers from the user.
  ;getelementptr basically does pointer math, in this case, no ptr math is required (we point to the beginning of @.str).
  ;Pass %a and %b, which are pointers to integers allocated previously.
  ;Scanf will store the two integers into the memory locations represented by %a and %b
  %0 = call i32 (i8*, ...)* @scanf(i8* noalias getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i32* %a, i32* %b) nounwind ; <i32> [#uses=0]
  ;Load the integer pointed to by %a and %b into registers %1 and %2 respectively
  %1 = load i32* %a, align 4                      ; <i32> [#uses=3]
  %2 = load i32* %b, align 4                      ; <i32> [#uses=3]
  ;Boolean register which represents if %1 is less than to %2
  %3 = icmp slt i32 %1, %2                        ; <i1> [#uses=1]
  ;If %1 is less than to %2, goto branch %bb, otherwise, goto %bb1
  br i1 %3, label %bb, label %bb1

;If integer %1 is less than %2
bb:                                               ; preds = %entry
  ;Use the C standard library function printf to output information to users
  ;Print @.str1, "%d is less than %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %4 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([20 x i8]* @.str1, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Continue on to %bb1, to check for equality of the two integers
  br label %bb1

;Continue checking if the integers are equal
bb1:                                              ; preds = %bb, %entry
  ;Boolean register which represents if %1 is equal to %2
  %5 = icmp eq i32 %1, %2                         ; <i1> [#uses=1]
  ;If %1 is equal to %2, goto branch %bb2, otherwise, goto %bb3
  br i1 %5, label %bb2, label %bb3

;If integer %1 is equal to %2
bb2:                                              ; preds = %bb1
  ;Use the C standard library function printf to output information to users
  ;Print @.str2 "%d is equal to %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %6 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([19 x i8]* @.str2, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Continue on to %bb3, to check if %1 is greater than %2
  br label %bb3

;Continue checking if %1 is greater than %2
bb3:                                              ; preds = %bb2, %bb1
  ;Boolean register which represents if %1 is greater than %2
  %7 = icmp sgt i32 %1, %2                      ; <i1> [#uses=1]
  ;If %1 is greather than %2, goto branch %bb4, otherwise, goto %bb5
  br i1 %7, label %bb4, label %bb5

;If integer %1 is greater than %2
bb4:                                              ; preds = %bb3
  ;Use the C standard library function printf to output information to users
  ;Print @.str3 "%d is greater than %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %8 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.str3, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Return 0 for the main function, indicating program executed successfully
  ret i32 0

bb5:                                              ; preds = %bb3
  ;Return 0 for the main function, indicating program executed successfully
  ret i32 0
}

;Declare external fuctions
declare i32 @scanf(i8* nocapture, ...) nounwind

declare i32 @printf(i8* nocapture, ...) nounwind

```



## Logo


```logo
to compare :a :b
  if :a = :b [(print :a [equals] :b)]
  if :a < :b [(print :a [is less than] :b)]
  if :a > :b [(print :a [is greater than] :b)]
end
```

Each infix operator has prefix synonyms (equalp, equal?, lessp, less?, greaterp, greater?), where the 'p' stands for "predicate" as in [[Lisp]].


## Lua


```lua
print('Enter the first number: ')
a = tonumber(io.stdin:read())
print('Enter the second number: ')
b = tonumber(io.stdin:read())

if a < b then print(a .. " is less than " .. b) end
if a > b then print(a .. " is greater than " .. b) end
if a == b then print(a .. " is equal to " .. b) end
```


In lua, a double equals symbol is required to test for equality. A single equals sign is used for assignment, and will cause an error during jit precompilation, if it is used within a boolean expression:


```lua
-- if a = b then print("This will not work")
```



## LSE64


```lse64
over : 2 pick
2dup : over over

compare : 2dup = then " equals"
compare : 2dup < then " is less than"
compare : 2dup > then " is more than"

show : compare rot , sp ,t sp , nl
```



## Maple


```Maple
CompareNumbers := proc( )
    local a, b;
    printf( "Enter a number:> " );
    a := parse(readline());
    printf( "Enter another number:> " );
    b := parse(readline());
    if a < b then
        printf("The first number is less than the second");
    elif a = b then
        printf("The first number is equal to the second");
    elif a > b then
        printf("The first number is greater than the second");
    end if;
end proc:

CompareNumbers();
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a=Input["Give me the value for a please!"];
b=Input["Give me the value for b please!"];
If[a==b,Print["a equals b"]]
If[a>b,Print["a is bigger than b"]]
If[a<b,Print["b is bigger than a"]]
```



## Maxima


```maxima
/* all 6 comparison operators (last is "not equal") */
block(
   [a: read("a?"), b: read("b?")],
   if a < b  then print(a, "<",  b),
   if a <= b then print(a, "<=", b),
   if a > b  then print(a, ">",  b),
   if a >= b then print(a, ">=", b),
   if a = b  then print(a, "=",  b),
   if a # b  then print(a, "#",  b))$
```



## MAXScript


```maxscript
a = getKBValue prompt:"Enter value of a:"
b = getKBValue prompt:"Enter value of b:"
if a < b then print "a is less then b"
else if a > b then print "a is greater then b"
else if a == b then print "a is equal to b"
```



## Metafont


```metafont
message "integer 1: ";
a1 := scantokens readstring;
message "integer 2: ";
a2 := scantokens readstring;
if a1 < a2:
  message decimal a1 & " is less than " & decimal a2
elseif a1 > a2:
  message decimal a1 & " is greater than " & decimal a2
elseif a1 = a2:
  message decimal a1 & " is equal to " & decimal a2
fi;
end
```



## min

{{works with|min|0.19.3}}

```min
"$1 is $2 $3."
("Enter an integer" ask) 2 times over over
(
  ((>) ("greater than"))
  ((<) ("less than"))
  ((==) ("equal to"))
) case
' append prepend % print
```



## MiniScript


```MiniScript
integer1 = input("Please Input Integer 1:").val
integer2 = input("Please Input Integer 2:").val
if integer1 < integer2 then print integer1 + " is less than " + integer2
if integer1 == integer2 then print integer1 + " is equal to " + integer2
if integer1 > integer2 then print integer1 + " is greater than " + integer2
```



## ML/I

This reads the two numbers from 'standard input' or similar, and outputs the results to 'standard output' or equivalent.
Note that ML/I only has tests for ''equality'', ''greater-than'', and ''greater-than-or-equal''.


```ML/I
"" Integer comparison
"" assumes macros on input stream 1, terminal on stream 2
MCSKIP MT,<>
MCSKIP SL WITH ~
MCINS %.
MCDEF SL SPACES NL AS <MCSET T1=%A1.
MCSET T2=%A2.
MCGO L1 UNLESS T1 EN T2
%A1. is equal to %A2.
%L1.MCGO L2 UNLESS %A1. GR %A2.
%A1. is greater than %A2.
%L2.MCGO L3 IF %A1. GE %A2.
%A1. is less than %A2.
%L3.
MCSET S10=0
>
MCSET S1=1
~MCSET S10=2
```



## MMIX

Some simple error checking is included.

```mmix
// main registers
p	IS	$255	% pointer
pp	GREG		% backup for p
A	GREG		% first int
B	GREG		% second int

// arg registers
argc	IS	$0
argv	IS	$1

	LOC	Data_Segment
	GREG	@
ERR	BYTE	"Wrong number of arguments",#a,0
ILLH	BYTE	"Argument -> ",0
ILLT	BYTE	" <- contains an illegal character",#a,0
LT	BYTE	"A is less than B",#a,0
EQ	BYTE	"A equals B",#a,0
GT	BYTE	"A is greater than B",#a,0

	LOC	#1000
	GREG	@
// call: p points to the start of a 0-terminated numeric string
//       leading chars + and - are allowed
//       reg $72   0 if negative int
//	 reg $73   gen. purpose
// return: reg $70 contains integer value
readInt	XOR	$70,$70,$70	% reset result reg: N=0.
	LDA	pp,p		% remember &p
	LDBU	$72,p
	CMP	$73,$72,'+'	% ignore '+'
	BZ	$73,2F
	CMP	$72,$72,'-'
	BNZ	$72,1F
2H	INCL	p,1
	JMP	1F
				% repeat
3H	CMP	$73,$71,'0'	%  if c < '0' or c > '9'
	BN	$73,4F		%  then print err and halt program
	CMP	$73,$71,'9'
	BP	$73,4F
	SUB	$71,$71,'0'	%  'extract' number
	MUL	$70,$70,10
	ADD	$70,$70,$71	%  N = 10 * N + digit
	INCL	p,1
1H	LDBU	$71,p		%  get next digit
	PBNZ	$71,3B		% until end of string
	CMP	$72,$72,0
	BNZ	$72,2F		% if marked negative
	NEG	$70,$70		%  then make negative
2H	GO	$127,$127,0	% return (N)

4H	LDA	p,ILLH
	TRAP	0,Fputs,StdErr
	LDA	p,pp
	TRAP	0,Fputs,StdErr
	LDA	p,ILLT
	TRAP	0,Fputs,StdErr
	TRAP	0,Halt,0

// entrance of program
// e.g. ~> mmix compare2ints A B
//
Main	CMP	p,argc,3	% main (argc, argv) {
	BZ	p,1F		%  if argc == 3 then continue
	LDA	p,ERR		%  else print wrong number of args
	TRAP	0,Fputs,StdErr
	TRAP	0,Halt,0
// get ints A and B
1H	LDOU	p,argv,8	% fetch addres of first int
	GO	$127,readInt	% read int A
	ADD	A,$70,0

	LDOU	p,argv,16
	GO	$127,readInt	% read int B
	ADD	B,$70,0

// perform comparison
	CMP	A,A,B		% case compare A B
	LDA	p,LT
	BN	A,2F		% LT:   print 'LT'
	LDA	p,EQ
	BZ	A,2F		% EQ:   print 'EQ'
	LDA	p,GT		% _ :   print 'GT'
2H	TRAP	0,Fputs,StdOut	% print result
	TRAP	0,Halt,0
```

Example of use:

```txt
~/MIX/MMIX/Progs> mmix compare2ints 121 122
A is less than B

~/MIX/MMIX/Progs> mmix compare2ints 121 121
A equals B

~/MIX/MMIX/Progs> mmix compare2ints 121 120
A is greater than B

~/MIX/MMIX/Progs> mmix compare2ints -121 -122
A is greater than B

~/MIX/MMIX/Progs> mmix compare2ints -121 -121
A equals B

~/MIX/MMIX/Progs> mmix compare2ints -121 -120
A is less than B
```


=={{header|Modula-2}}==

```modula2
MODULE IntCompare;

IMPORT InOut;

VAR
  A, B: INTEGER;

BEGIN
  InOut.ReadInt(A);
  InOut.ReadInt(B);
  InOut.WriteInt(A, 1);

  IF A < B THEN
    InOut.WriteString(' is less than ')
  ELSIF A = B THEN
    InOut.WriteString(' is equal to ')
  ELSE
    InOut.WriteString(' is greater than ')
  END;
  InOut.WriteInt(B, 1);
  InOut.WriteLn
END IntCompare.
```


=={{header|Modula-3}}==

```modula3
MODULE Main;

FROM IO  IMPORT Put, GetInt;
FROM Fmt IMPORT Int;

VAR a,b: INTEGER;

BEGIN
  a := GetInt();
  b := GetInt();
  IF a < b THEN
    Put(Int(a) & " is less than " & Int(b) & "\n");
  ELSIF a = b THEN
    Put(Int(a) & " is equal to " & Int(b) & "\n");
  ELSIF a > b THEN
    Put(Int(a) & " is greater than " & Int(b) & "\n");
  END;
END Main.
```


=={{header|МК-61/52}}==
<lang>-	ЗН	С/П
```


''Input:'' a ^ b

''Output:'' 1 (a > b) | -1 (a < b) | 0 (a = b)


## MUMPS

<lang>INTCOMP
 NEW A,B
INTCOMPREAD
 READ !,"Enter an integer to test: ",A
 READ !,"Enter another integer: ",B
 IF (+A\1'=A)!(+B\1'=B) WRITE !!,"Please enter two integers.",! GOTO INTCOMPREAD
 IF A<B WRITE !,A," is less than ",B
 IF A=B WRITE !,A," is equal to ",B
 IF A>B WRITE !,A," is greater than ",B
 KILL A,B
 QUIT
```

{{out}}

```txt
USER>d INTCOMP^ROSETTA

Enter an integer to test: 43
Enter another integer: 44
43 is less than 44
USER>d INTCOMP^ROSETTA

Enter an integer to test: 44
Enter another integer: 43
44 is greater than 43
USER>d INTCOMP^ROSETTA

Enter an integer to test: 2
Enter another integer: 2
2 is equal to 2
```



## Nemerle

Showing both the use of comparison operators and the .Net Int32.CompareTo() method.

```Nemerle
using System;
using System.Console;

module IntComp
{
    Main() : void
    {
        def ReadInt() : int {Int32.Parse(ReadLine())}
        def WriteResult(x : int, y : int, res : string) : void
        {WriteLine($"$x is $res $y")}

        def a = ReadInt();
        def b = ReadInt();

        match(a)
        {
            |a when a > b  => WriteResult(a, b, "greater than")
            |a when a < b  => WriteResult(a, b, "less than")
            |a when a == b => WriteResult(a, b, "equal to")
        }

        def x = a.CompareTo(b);

        match(x)
        {
            |x when x > 0  => WriteResult(a, b, "greater than")
            |x when x < 0  => WriteResult(a, b, "less than")
            |x when x == 0 => WriteResult(a, b, "equal to")
        }
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numL = 0
numR = 0
loop label running forever
  say 'Provide two integers [or anything else to stop]:'
  parse ask numL numR .
  if \numL.datatype('w') | \numR.datatype('w') then leave running
  if numL < numR then say numL 'is less than' numR
  if numL = numR then say numL 'is equal to' numR
  if numL > numR then say numL 'is greater than' numR
  end running

return

```



## NewLISP


```NewLISP

(print "Please enter the first number: ")
(set 'A (int (read-line)))
(print "Please enter the second number: ")
(set 'B (int (read-line)))
(println
"The first one is "
    (cond
        ((> A B) "greater than")
        ((= A B) "equal to")
        (true "less than"))
" the second.")

```



## Nim


```nim
import rdstdin, strutils
var a = parseInt(readLineFromStdin "Enter value of a: ")
var b = parseInt(readLineFromStdin "Enter value of b: ")

if a < b:
  echo "a is less than b"
elif a > b:
  echo "a is greater than b"
elif a == b:
  echo "a is equal to b"
```



## NSIS

===Pure NSIS (Using [http://nsis.sourceforge.net/Docs/Chapter4.html#4.9.4.13 IntCmp] directly)===

```nsis

Function IntergerComparison
	Push $0
	Push $1
	StrCpy $0 8
	StrCpy $1 2

	IntCmp $0 $1 Equal Val1Less Val1More

Equal:
	DetailPrint "$0 = $1"
	Goto End
Val1Less:
	DetailPrint "$0 < $1"
	Goto End
Val1More:
	DetailPrint "$0 > $1"
	Goto End
End:

	Pop $1
	Pop $0
FunctionEnd

```

=== Using [http://nsis.sourceforge.net/LogicLib LogicLib] (bundled library) ===
{{libheader|LogicLib}}

```nsis

Function IntegerComparison
	Push $0
	Push $1

	StrCpy $0 8
	StrCpy $1 2

	${If} $0 == $1
		DetailPrint "$0 = $1"
	${ElseIf} $0 < $1
		DetailPrint "$0 < $1"
	${ElseIf} $0 > $1
		DetailPrint "$0 > $1"
	${EndIf}

	Pop $1
	Pop $0
FunctionEnd

```


=={{header|Oberon-2}}==

```oberon2
MODULE Compare;

   IMPORT In, Out;

   VAR a,b: INTEGER;

BEGIN
   In.Int(a);
   In.Int(b);
   IF a < b THEN
      Out.Int(a,0);
      Out.String(" is less than ");
      Out.Int(b,0);
      Out.Ln;
   ELSIF a = b THEN
      Out.Int(a,0);
      Out.String(" is equal to ");
      Out.Int(b,0);
      Out.Ln;
   ELSIF a > b THEN
      Out.Int(a,0);
      Out.String(" is greater than ");
      Out.Int(b,0);
      Out.Ln;
   END;
END Compare.
```



## Objeck


```objeck

bundle Default {
  class IntCompare {
    function : Main(args : String[]) ~ Nil {
      a := Console->GetInstance()->ReadString()->ToInt();
      b := Console->GetInstance()->ReadString()->ToInt();

      if (a < b) {
        Console->GetInstance()->Print(a)->Print(" is less than ")->PrintLine(b);
      };

      if (a = b) {
        Console->GetInstance()->Print(a)->Print(" is equal than ")->PrintLine(b);
      };

      if (a > b) {
        Console->GetInstance()->Print(a)->Print(" is greater than ")->PrintLine(b);
      };
    }
  }
}

```



## OCaml


```ocaml
let my_compare a b =
  if      a < b then "A is less than B"
  else if a > b then "A is greater than B"
  else if a = b then "A equals B"
  else "cannot compare NANs"

let () =
  let a = read_int ()
  and b = read_int () in
  print_endline (my_compare a b)
```



## Octave


```octave
printf("Enter a: ");
a = scanf("%d", "C");
printf("Enter b: ");
b = scanf("%d", "C");
if (a > b)
  disp("a greater than b");
elseif (a == b)
  disp("a equal to b");
elseif (a < b)
  disp("a less than b");
endif
```



## Oforth



```Oforth
import: console

: cmpInt
| a b |
   doWhile: [ System.Console askln asInteger dup ->a isNull ]
   doWhile: [ System.Console askln asInteger dup ->b isNull ]

   a b <  ifTrue: [ System.Out a << " is less than " << b << cr ]
   a b == ifTrue: [ System.Out a << " is equal to " << b << cr ]
   a b >  ifTrue: [ System.Out a << " is greater than " << b << cr ] ;
```



## Ol


```scheme

(define (compare a b)
  (cond ((< a b) "A is less than B")
        ((> a b) "A is greater than B")
        ((= a b) "A equals B")))

(print (compare 1 2))
; ==> A is less than B

(print (compare 2 2))
; ==> A equals B

(print (compare 3 2))
; ==> A is greater than B

; manual user input:
(print (compare (read) (read)))

```



## Oz


```oz
functor
import
  Application(exit)
  Open(text file)
define

Txt = class from Open.file Open.text end
Stdout = {New Open.file init(name:stdout)}
Stdin  = {New Txt init(name:stdin)}

proc{Print Msg}
  {Stdout write(vs:Msg)}
end

fun{GetInt Prompt}
  {Print Prompt}
  {StringToInt {Stdin getS($)}}
end

Int1 = {GetInt "Enter 1st Integer:"}
Int2 = {GetInt "Enter 2nd Integer:"}

if(Int1  < Int2) then {Print Int1#" less than "#Int2} end
if(Int1  > Int2) then {Print Int1#" greater than "#Int2} end
if(Int1 == Int2) then {Print Int1#" equal to "#Int2} end

{Application.exit 0}
end
```



## PARI/GP


```parigp
a=input();
b=input();
if(a<b, print(a" < "b));
if(a==b, print(a" = "b));
if(a>b, print(a" > "b));
```



## Pascal


```pascal
program compare(input, output);

var
 a, b: integer;

begin
  write('Input an integer number: ');
  readln(a);
  write('Input another integer number: ');
  readln(b);
  if (a < b) then writeln(a, ' is less than ', b);
  if (a = b) then writeln(a, ' is equal to ', b);
  if (a > b) then writeln(a, ' is greater than ', b);
end.
```



## Perl

{{works with|Perl|5.x}}

Separate tests for less than, greater than, and equals


```perl
sub test_num {
    my $f = shift;
    my $s = shift;
    if ($f < $s){
        return -1; # returns -1 if $f is less than $s
    } elsif ($f > $s) {
        return 1; # returns 1 if $f is greater than $s
    } elsif ($f == $s) {
# = operator is an assignment
# == operator is a numeric comparison
       return 0; # returns 0 $f is equal to $s
    };
};
```


All three tests in one. If $f is less than $s return -1, greater than return 1, equal to return 0


```perl
sub test_num {
    return $_[0] <=> $_[1];
};
```

Note: In Perl, $a and $b are (kind of) reserved identifiers for the built-in ''sort'' function. It's good style to use more meaningful names, anyway.


```perl
# Get input, test and display
print "Enter two integers: ";
($x, $y) = split ' ', <>;
print $x, (" is less than ", " is equal to ",
           " is greater than ")[test_num($x, $y) + 1], $y, "\n";
```



## Perl 6


```perl6
my $a = prompt("1st int: ").floor;
my $b = prompt("2nd int: ").floor;

if $a < $b {
    say 'Less';
}
elsif $a > $b {
    say 'Greater';
}
elsif $a == $b {
    say 'Equal';
}
```


With <code><=></code>:


```perl6>say <Less Equal Greater
[($a <=> $b) + 1];
```


A three-way comparison such as <tt><=></tt> actually returns an <code>Order</code> enum which stringifies into 'Decrease', 'Increase' or 'Same'.  So if it's ok to use this particular vocabulary, you could say that this task is actually a built in:


```perl6
say prompt("1st int: ") <=> prompt("2nd int: ");
```



## Phix


```Phix
atom a = prompt_number("first number:",{}),
     b = prompt_number("second number:",{})

printf(1,"%g is ",a)
if a < b then
    puts(1,"less than")
elsif a = b then
    puts(1,"equal to")
elsif a > b then
    puts(1,"greater than")
end if
printf(1," %g",b)
```



## PHL



```phl
module intergertest;

extern printf;
extern scanf;

@Integer main [
	var a = 0;
	var b = 0;
	scanf("%i %i", ref (a), ref (b));

	if (a < b)
		printf("%i is less than %i\n", a::get, b::get);

	if (a == b)
		printf("%i is equal to %i\n", a::get, b::get);

	if (a > b)
		printf("%i is greater than %i\n", a::get, b::get);

	return 0;
]
```



## PHP


```php
<?php

echo "Enter an integer [int1]: ";
fscanf(STDIN, "%d\n", $int1);
if(!is_numeric($int1)) {
  echo "Invalid input; terminating.\n";
  exit(1);      // return w/ general error
}

echo "Enter an integer [int2]: ";
fscanf(STDIN, "%d\n", $int2);
if(!is_numeric($int2)) {
  echo "Invalid input; terminating.\n";
  exit(1);      // return w/ general error
}

// now $int1 and $int2 are numbers.
// for simplicity, this does not explicitly examine types

if($int1 < $int2)
  echo "int1 < int2\n";
if($int1 == $int2)
  echo "int1 = int2\n";
if($int1 > $int2)
  echo "int1 > int2\n";

?>
```

Note that this works from the command-line interface only, whereas [http://www.php.net PHP] is usually executed as [[wp:Common_Gateway_Interface CGI]].


## PicoLisp


```PicoLisp
(prin "Please enter two values: ")

(in NIL  # Read from standard input
   (let (A (read)  B (read))
      (prinl
         "The first one is "
         (cond
            ((> A B) "greater than")
            ((= A B) "equal to")
            (T "less than") )
         " the second." ) ) )
```

{{out}}

```txt
Please enter two values: 4 3
The first one is greater than the second.
```



## Pike


```pike
int main(int argc, array(int) argv){
   if(argc != 3){
      write("usage: `pike compare-two-ints.pike <x> <y>` where x and y are integers.\n");
      return 0;
   }

   int a = argv[1];
   int b = argv[2];

   if(a > b) {
      write(a + " is greater than " + b + "\n");
   } else if (a < b) {
      write(a + " is less than " + b + "\n");
   } else {
      write(a + " is equal to " + b + "\n");
   }
}
```



## PL/I


```PL/I

declare (a, b) fixed binary;

get list (a, b);
if a = b then
   put skip list ('The numbers are equal');
if a > b then
   put skip list ('The first number is greater than the second');
if a < b then
   put skip list ('The second number is greater than the first');

```



## Pop11


```pop11
;;; Comparison procedure
define compare_integers(x, y);
if x > y then
   printf('x is greater than y\n');
elseif x < y then
   printf('x is less than y\n');
elseif x = y then
   printf('x equals y\n');
endif;
enddefine;

;;; Setup token reader
vars itemrep;
incharitem(charin) -> itemrep;

;;; Read numbers and call comparison procedure
compare_integers(itemrep(), itemrep());
```


## PowerShell


```powershell
$a = [int] (Read-Host a)
$b = [int] (Read-Host b)

if ($a -lt $b) {
    Write-Host $a is less than $b`.
} elseif ($a -eq $b) {
    Write-Host $a is equal to $b`.
} elseif ($a -gt $b) {
    Write-Host $a is greater than $b`.
}
```


## PureBasic


```PureBasic
If OpenConsole()

  Print("Enter an integer: ")
  x.i = Val(Input())
  Print("Enter another integer: ")
  y.i = Val(Input())

  If x < y
    Print( "The first integer is less than the second integer.")
  ElseIf x = y
    Print("The first integer is equal to the second integer.")
  ElseIf x > y
    Print("The first integer is greater than the second integer.")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```Python
#!/usr/bin/env python
a = input('Enter value of a: ')
b = input('Enter value of b: ')

if a < b:
    print 'a is less than b'
elif a > b:
    print 'a is greater than b'
elif a == b:
    print 'a is equal to b'
```


(Note: in Python3 ''input()'' will become ''int(input())'')

An alternative implementation could use a Python dictionary to house a small dispatch table to be indexed by the results of the built-in ''cmp()'' function.  ''cmp()'' returns a value suitable for use as a comparison function in a sorting algorithm: -1, 0 or 1 for <, = or > respectively.  Thus we could use:

{{works with|Python|2.x only, not 3.x}}

```Python
#!/usr/bin/env python
import sys
try:
   a = input('Enter value of a: ')
   b = input('Enter value of b: ')
except (ValueError, EnvironmentError), err:
   print sys.stderr, "Erroneous input:", err
   sys.exit(1)

dispatch = {
    -1: 'is less than',
     0: 'is equal to',
     1: 'is greater than'
     }
 print a, dispatch[cmp(a,b)], b
```


In this case the use of a dispatch table is silly.  However, more generally in Python the use of dispatch dictionaries or tables is often preferable to long chains of '''''elif'''' clauses in a condition statement.  Python's support of classes and functions (including [[currying]], partial function support, and lambda expressions) as first class objects obviates the need for a "case" or "switch" statement.


## R


```R
print("insert number a")
a <- scan(what=numeric(0), nmax=1)
print("insert number b")
b <- scan(what=numeric(0), nmax=1)
if ( a < b ) {
  print("a is less than b")
} else if ( a > b ) {
  print("a is greater than b")
} else if ( a == b ) { # could be simply else of course...
  print("a and b are the same")
}
```



## Racket


```Racket
#lang racket
(define (compare-two-ints a b)
  (define compared
    (cond ((> a b) "is greated than")
          ((= a b) "equals")
          ((< a b) "is lesser than")))
  (format "~a ~a ~a" a compared b))

(compare-two-ints (read) (read))
```



## Raven


```Raven
"Enter the first number: "  print
expect trim  1.1 prefer as $a
"Enter the second number: " print
expect trim  1.1 prefer as $b

$a $b < if  $b $a "%g is less than %g\n" print
$a $b > if  $b $a "%g is greater than %g\n" print
$a $b = if  $b $a "%g is equal to %g\n" print
```



## REBOL


```REBOL

REBOL [
	Title: "Comparing Two Integers"
	URL: http://rosettacode.org/wiki/Comparing_two_integers
]

a: ask "First integer? "  b: ask "Second integer? "

relation: [
	a < b "less than"
	a = b "equal to"
	a > b "greater than"
]
print [a "is"  case relation  b]

```



## Retro

Taking the numbers from the stack:


```Retro
:example (ab-)
  dup-pair gt? [ 'A>B s:put nl ] if
  dup-pair lt? [ 'A<B s:put nl ] if
           eq? [ 'A=B s:put nl ] if ;
```



## REXX


```REXX
/*REXX program  prompts  for  two integers,   compares them,  and  displays the results.*/
numeric digits 2000                              /*for the users that really go ka─razy.*/
@=copies('─', 20)                                /*eyeball catcher for the user's eyen. */
a=getInt(@  'Please enter your 1st integer:')    /*obtain the 1st integer from the user.*/
b=getInt(@  'Please enter your 2nd integer:')    /*   "    "  2nd    "      "   "    "  */
say
      if a<b  then say  @   a    ' is less than '        b
      if a=b  then say  @   a    ' is equal to '         b
      if a>b  then say  @   a    ' is greater than '     b
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
getInt:       do forever;     say                /*keep prompting the user until success*/
                              say arg(1)         /*display the prompt message to console*/
              parse pull x                       /*obtain  X,  and keep its case intact.*/
                 select
                 when x=''               then call serr "No argument was entered."
                 when words(x)>1         then call serr 'Too many arguments entered.'  x
                 when \datatype(x, 'N')  then call serr "Argument isn't numeric:"      x
                 when \datatype(x, 'W')  then call serr "Argument isn't an integer:"   x
                 otherwise    return x           /* [↑]  Eureka!   Return # to invoker. */
                 end   /*select*/
              end      /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
serr:  say @  '***error*** '    arg(1);        say @  "Please try again.";          return
```

{{out|output|text=  (shows user input and computer program output together):}}

```txt

──────────────────── Please enter your 1st integer:
bupkis                            ◄■■■■■■■■■■■■■■■ user input.
──────────────────── ***error***  Argument isn't numeric: bupkis
──────────────────── Please try again.

──────────────────── Please enter your 1st integer:
1 2                               ◄■■■■■■■■■■■■■■■ user input.
──────────────────── ***error***  Too many arguments entered.
──────────────────── Please try again.

──────────────────── Please enter your 1st integer:
5.77                              ◄■■■■■■■■■■■■■■■ user input.
──────────────────── ***error***  Argument isn't an integer: 5.77
──────────────────── Please try again.

──────────────────── Please enter your 1st integer:
-6                                ◄■■■■■■■■■■■■■■■ user input.

──────────────────── Please enter your 2nd integer:
19.00                             ◄■■■■■■■■■■■■■■■ user input.

──────────────────── -6  is less than  19.00

```



## RPG

Two integers are passed as parameters.  Because the command line casts numeric literals as packed(15,5), calling from the command line requires the user to specify the 8 nybbles in hexadecimal form:

CALL rc_intcmp (x'00000000' x'00000001')


```RPG

     h dftactgrp(*no)

     d                 pi
     d integer1                      10i 0
     d integer2                      10i 0

     d message         s             50a

       if integer1 < integer2;
         message = 'Integer 1 is less than integer 2';
       endif;

       if integer1 > integer2;
         message = 'Integer 1 is greater than integer 2';
       endif;

       if integer1 = integer2;
         message = 'Integer 1 is equal to integer 2';
       endif;

       dsply message;
       *inlr = *on;

```




## Ring


```ring

Func Compare a,b
   if a < b
      See "A is less than B"
   but a > b
      See "A is more than B"
   else
      See "A equals B"
   ok

```



## Rockstar


Minimized Rockstar:


```Rockstar

(Get two numbers from user)
Listen to Number One
Listen to Number Two
(Check if n1 > n2)
If Number One is greater than Number Two
Say "The first is greater than the second"

(Check if n1 = n2)
If Number One is Number Two
Say "The Numbers are equal"

(Check if n1 < n2)
If Number One is less than Number Two
Say "The first is less than the second"

```


Idiomatic version:

```Rockstar

Listen to your soul
Listen to my words

If your soul is my words,
Say "They're the same"

If your soul is stronger than my words,
Say "The first was bigger"

If your soul is smaller than my words,
Say "The second was bigger".

```



## Ruby

This uses Kernel#gets to get input from STDIN, and String#to_i to convert the string into an integer. (Without this conversion, Ruby would compare strings: 5 < 10 but "5" > "10".)


```ruby
a = (print "enter a value for a: "; gets).to_i
b = (print "enter a value for b: "; gets).to_i

puts "#{a} is less than #{b}" if a < b
puts "#{a} is greater than #{b}" if a > b
puts "#{a} is equal to #{b}" if a == b
```


Another way:


```ruby
a = (print "enter a value for a: "; gets).to_i
b = (print "enter a value for b: "; gets).to_i

case a <=> b
when -1; puts "#{a} is less than #{b}"
when  0; puts "#{a} is equal to #{b}"
when +1; puts "#{a} is greater than #{b}"
end
```


Example '''input''' and output:

```txt

 $ '''ruby compare.rb'''
 enter a value for a: '''5'''
 enter a value for b: '''10'''
 5 is less than 10
 $ '''ruby compare.rb'''
 enter a value for a: '''cat'''
 enter a value for b: '''dog'''
 0 is equal to 0

```


----
{{trans|Python}}

An alternative method, which is similar to the python version mentioned above (at the time of writing this) is:

```ruby
# Function to make prompts nice and simple to abuse
def prompt str
  print str, ": "
  gets.chomp
end

# Get value of a
a = prompt('Enter value of a').to_i
# Get value of b
b = prompt('Enter value of b').to_i

# The dispatch hash uses the <=> operator
# When doing x<=>y:
# -1 means x is less than y
# 0 means x is equal to y
# 1 means x is greater than y
dispatch = {
  -1 => "less than",
  0 => "equal to",
  1 => "greater than"
}

# I hope you can figure this out
puts "#{a} is #{dispatch[a<=>b]} #{b}"
```



## Run BASIC


```runbasic
input "1st number:"; n1
input "2nd number:"; n2

if n1 < n2 then print "1st number ";n1;" is less than 2nd number";n2
if n1 > n2 then print "1st number ";n1;" is greater than 2nd number";n2
if n1 = n2 then print "1st number ";n1;" is equal to 2nd number";n2
```



## Rust

Reading from stdin into a string is cumbersome at the moment, but convenience functions will be implemented in the future.

```rust
use std::io::{self, BufRead};

fn main() {
    let mut reader = io::stdin();
    let mut buffer = String::new();
    let mut lines = reader.lock().lines().take(2);
    let nums: Vec<i32>= lines.map(|string|
        string.unwrap().trim().parse().unwrap()
        ).collect();
    let a: i32 = nums[0];
    let b: i32 = nums[1];
    if a < b {
        println!("{} is less than {}" , a , b)
    } else if a == b {
        println!("{} equals {}" , a , b)
    } else if a > b {
        println!("{} is greater than {}" , a , b)
    };
}
```



## SAS


```sas
/* Showing operators and their fortran-like equivalents. Note that ~= and ^= both mean "different" */
data _null_;
input a b;
put a= b=;
if a = b then put "a = b";
if a ^= b then put "a ^= b";
if a ~= b then put "a ~= b";
if a < b then put "a < b";
if a > b then put "a > b";
if a <= b then put "a <= b";
if a >= b then put "a >= b";
if a eq b then put "a eq b";
if a ne b then put "a ne b";
if a lt b then put "a lt b";
if a gt b then put "a gt b";
if a le b then put "a le b";
if a ge b then put "a ge b";
cards;
1 2
2 1
1 1
;
run;
```



## Scala


```scala
object IntCompare {
  def main(args: Array[String]): Unit = {
    val a=Console.readInt
    val b=Console.readInt
    if (a < b)
      printf("%d is less than %d\n", a, b)
    if (a == b)
      printf("%d is equal to %d\n", a, b)
    if (a > b)
      printf("%d is greater than %d\n", a, b)
  }
}
```



## Scheme


```scheme
(define (my-compare a b)
  (cond ((< a b) "A is less than B")
        ((> a b) "A is greater than B")
        ((= a b) "A equals B")))

(my-compare (read) (read))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: a is 0;
    var integer: b is 0;
  begin
    readln(a);
    readln(b);

    if a < b then
      writeln(a <& " is less than " <& b);
    end if;

    if a = b then
      writeln(a <& " is equal to " <& b);
    end if;

    if a > b then
      writeln(a <& " is greater than " <& b);
    end if;
  end func;
```



## Sidef


```ruby
var a = read("a: ", Number);
var b = read("b: ", Number);

if (a < b) {
    say 'Lower';
}
elsif (a == b) {
    say 'Equal';
}
elsif (a > b) {
    say 'Greater';
}
```

{{out}}

```txt

% sidef numcmp.sf
a: 21
b: 42
Lower
```



## Slate



```slate
[ |:a :b |

 ( a > b ) ifTrue: [ inform: 'a greater than b\n' ].
 ( a < b ) ifTrue: [ inform: 'a less than b\n' ].
 ( a = b ) ifTrue: [ inform: 'a is equal to b\n' ].

] applyTo: {Integer readFrom: (query: 'Enter a: '). Integer readFrom: (query: 'Enter b: ')}.
```



## Smalltalk



```smalltalk
| a b |
'a = ' display. a := (stdin nextLine asInteger).
'b = ' display. b := (stdin nextLine asInteger).
( a > b ) ifTrue: [ 'a greater than b' displayNl ].
( a < b ) ifTrue: [ 'a less than b' displayNl ].
( a = b ) ifTrue: [ 'a is equal to b' displayNl ].
```



## SNOBOL4


Comparisons in Snobol are not operators, but predicate functions that return a null string and generate a success or failure value which allows or blocks statement execution, and which can be tested for branching. Other numeric comparisons are ge (>=), le (<=) and ne (!= ). There is also a parallel set of L-prefixed predicates in modern Snobols for lexical string comparison.


```SNOBOL4
*       # Get user input
        output = 'Enter X,Y:'
        trim(input) break(',') . x ',' rem . y

        output = lt(x,y) x ' is less than ' y :s(end)
        output = eq(x,y) x ' is equal to '  y :s(end)
        output = gt(x,y) x ' is greater than ' y
end
```



## SNUSP

There are no built-in comparison operators, but you can (destructively) check which of two adjacent cells is most positive.

```snusp>++++
++++ a b !/?\<?\#  a=b
               > -  \#  a>b
               - <
          a<b #\?/
```



## Sparkling


```sparkling
let a = 13, b = 37;
if a < b {
    print("a < b");
} else if a > b {
    print("a > b");
} else if a == b {
    print("a == b");
} else {
    print("either a or b or both are NaN");
}
```



## SQL

{{works with|Oracle}}

```sql

drop table test;

create table test(a integer, b integer);

insert into test values (1,2);

insert into test values (2,2);

insert into test values (2,1);

select to_char(a)||' is less than '||to_char(b) less_than
from test
where a < b;

select to_char(a)||' is equal to '||to_char(b) equal_to
from test
where a = b;

select to_char(a)||' is greater than '||to_char(b) greater_than
from test
where a > b;

```



```txt

SQL> SQL>   2    3
LESS_THAN
--------------------------------------------------------------------------------
1 is less than 2

SQL> SQL>   2    3
EQUAL_TO
--------------------------------------------------------------------------------
2 is equal to 2

SQL> SQL>   2    3
GREATER_THAN
--------------------------------------------------------------------------------
2 is greater than 1

```



## SQL PL

{{works with|Db2 LUW}}
With SQL only:

```sql pl

CREATE TABLE TEST (
  VAL1 INT,
  VAL2 INT
);
INSERT INTO TEST (VAL1, VAL2) VALUES
  (1, 2),
  (2, 2),
  (2, 1);
SELECT
  CASE
     WHEN VAL1 < VAL2 THEN VAL1 || ' less than ' || VAL2
     WHEN VAL1 = VAL2 THEN VAL1 || ' equal to ' || VAL2
     WHEN VAL1 > VAL2 THEN VAL1 || ' greater than ' || VAL2
  END COMPARISON
FROM TEST;

```

Output:

```txt

db2 -t
db2 => CREATE TABLE TEST (
db2 (cont.) =>   VAL1 INT,
db2 (cont.) =>   VAL2 INT
db2 (cont.) => );
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO TEST (VAL1, VAL2) VALUES
db2 (cont.) =>   (1, 2),
db2 (cont.) =>   (2, 2),
db2 (cont.) =>   (2, 1);
DB20000I  The SQL command completed successfully.
db2 => SELECT
db2 (cont.) =>   CASE
db2 (cont.) =>      WHEN VAL1 < VAL2 THEN VAL1 || ' less than ' || VAL2
db2 (cont.) =>      WHEN VAL1 = VAL2 THEN VAL1 || ' equal to ' || VAL2
db2 (cont.) =>      WHEN VAL1 > VAL2 THEN VAL1 || ' greater than ' || VAL2
db2 (cont.) =>   END COMPARISON
db2 (cont.) => FROM TEST;

COMPARISON
-----------------------------------
1 less than 2
2 equal to 2
2 greater than 1

  3 record(s) selected.

```

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET serveroutput ON @

CREATE PROCEDURE COMPARISON (IN VAL1 INT, IN VAL2 INT)
 BEGIN
  IF (VAL1 < VAL2) THEN
    CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' less than ' || VAL2);
  ELSEIF (VAL1 = VAL2) THEN
    CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' equal to ' || VAL2);
  ELSEIF (VAL1 > VAL2) THEN
    CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' greater than ' || VAL2);
  END IF;
 END @
CALL COMPARISON(1, 2) @
CALL COMPARISON(2, 2) @
CALL COMPARISON(2, 1) @

```

Output:

```txt

db2 -td@
db2 => SET serveroutput ON @
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => CREATE PROCEDURE COMPARISON (IN VAL1 INT, IN VAL2 INT)
db2 (cont.) =>  BEGIN
db2 (cont.) =>   IF (VAL1 < VAL2) THEN
db2 (cont.) =>     CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' less than ' || VAL2);
db2 (cont.) =>   ELSEIF (VAL1 = VAL2) THEN
db2 (cont.) =>     CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' equal to ' || VAL2);
db2 (cont.) =>   ELSEIF (VAL1 > VAL2) THEN
db2 (cont.) =>     CALL DBMS_OUTPUT.PUT_LINE(VAL1 || ' greater than ' || VAL2);
db2 (cont.) =>   END IF;
db2 (cont.) =>  END @
DB20000I  The SQL command completed successfully.
db2 => CALL COMPARISON(1, 2) @
  Return Status = 0

1 less than 2
db2 => CALL COMPARISON(2, 2) @
  Return Status = 0

2 equal to 2
db2 => CALL COMPARISON(2, 1) @

  Return Status = 0

2 greater than 1

```



## SSEM

The SSEM only provides one conditional operation: <tt>011 Test</tt>, which causes execution to skip one instruction if the value in the accumulator is negative. We can use this to implement conditional tests along the lines of the following pseudocode:

```txt
            accumulator := a - b;
            if accumulator >= 0 then
                (* a is not less than b, so *)
                goto next_test
            else
                goto less;
next_test:  accumulator := accumulator - 1;
            if accumulator >= 0 then
                goto greater
            else
                (* a and b are equal *)
                accumulator := 0;
                halt;
greater:    accumulator := 1;
            halt;
less:       accumulator := -1;
            halt
```

To run the SSEM program, load A into storage address 21 and B into storage address 22. No additional space is used. Like the pseudocode version, the program halts with the accumulator holding 1 if A>B, 0 if A=B, or -1 if A<B.

```ssem
10101000000000100000000000000000   0. -21 to c
10101000000001100000000000000000   1. c to 21
10101000000000100000000000000000   2. -21 to c
01101000000000010000000000000000   3. Sub. 22
00000000000000110000000000000000   4. Test
00001000000001000000000000000000   5. Add 16 to CI
00101000000000000000000000000000   6. 20 to CI
00001000000000010000000000000000   7. Sub. 16
00000000000000110000000000000000   8. Test
11001000000000000000000000000000   9. 19 to CI
10001000000000100000000000000000  10. -17 to c
00000000000001110000000000000000  11. Stop
01001000000000100000000000000000  12. -18 to c
00000000000001110000000000000000  13. Stop
00001000000000100000000000000000  14. -16 to c
00000000000001110000000000000000  15. Stop
10000000000000000000000000000000  16. 1
00000000000000000000000000000000  17. 0
11111111111111111111111111111111  18. -1
11010000000000000000000000000000  19. 11
10110000000000000000000000000000  20. 13
```



## Standard ML


```sml
fun compare_integers(a, b) =
  if a < b then print "A is less than B\n"
  if a > b then print "A is greater than B\n"
  if a = b then print "A equals B\n"

 fun test () =
  let
    open TextIO
    val SOME a = Int.fromString (input stdIn)
    val SOME b = Int.fromString (input stdIn)
  in
    compare_integers (a, b)
  end
    handle Bind => print "Invalid number entered!\n"
```

A more idiomatic and less error-prone way to do it in SML would be to use a compare function that returns type <tt>order</tt>, which is either LESS, GREATER, or EQUAL:

```sml
fun myCompare (a, b) = case Int.compare (a, b) of
                  LESS    => "A is less than B"
                | GREATER => "A is greater than B"
                | EQUAL   => "A equals B"
```


## Swift


```Swift
import Cocoa

var input = NSFileHandle.fileHandleWithStandardInput()

println("Enter two integers separated by a space: ")

let data = input.availableData
let stringArray = NSString(data: data, encoding: NSUTF8StringEncoding)?.componentsSeparatedByString(" ")
var a:Int!
var b:Int!
if (stringArray?.count == 2) {
    a = stringArray![0].integerValue
    b = stringArray![1].integerValue
}
if (a==b)  {println("\(a) equals \(b)")}
if (a < b) {println("\(a) is less than \(b)")}
if (a > b) {println("\(a) is greater than \(b)")}
```

{{out}}

```txt

Enter two integers separated by a space:
234 233
234 is greater than 233
```



## Tcl


This is not how one would write this in Tcl, but for the sake of clarity:


```tcl
puts "Please enter two numbers:"

gets stdin x
gets stdin y

if { $x > $y } { puts "$x is greater than $y" }
if { $x < $y } { puts "$x is less than $y" }
if { $x == $y } { puts "$x equals $y" }
```


Other comparison operators are "<=", ">=" and "!=".

Note that Tcl doesn't really have a notion of a variable "type" - all variables are just strings of bytes and notions like "integer" only ever enter at interpretation time. Thus the above code will work correctly for "5" and "6", but "5" and "5.5" will also be compared correctly. It will not be an error to enter "hello" for one of the numbers ("hello" is greater than any integer). If this is a problem, the type can be expressly cast


```tcl
if {int($x) > int($y)} { puts "$x is greater than $y" }
```


or otherwise [[IsNumeric | type can be checked]] with "<tt>if { string is integer $x }...</tt>"

Note that there is no substitution/evaluation here anywhere: entering "3*5" and "15" will parse "3*5" as a non-numerical string (like "hello") and thus the result will be "3*5 is greater than 15".

A variant that iterates over comparison operators, demonstrated in an interactive [[tclsh]]:

```Tcl
% set i 5;set j 6
% foreach {o s} {< "less than" > "greater than" == equal} {if [list $i $o $j] {puts "$i is $s $j"}}
5 is less than 6
% set j 5
% foreach {o s} {< "less than" > "greater than" == equal} {if [list $i $o $j] {puts "$i is $s $j"}}
5 is equal 5
% set j 4
% foreach {o s} {< "less than" > "greater than" == equal} {if [list $i $o $j] {puts "$i is $s $j"}}
5 is greater than 4
```


=={{header|TI-83 BASIC}}==

```ti83b
Prompt A,B
If A<B: Disp "A SMALLER B"
If A>B: Disp "A GREATER B"
If A=B: Disp "A EQUAL B"
```


=={{header|TI-89 BASIC}}==


```ti89b
Local a, b, result
Prompt a, b
If a < b Then
  "<" → result
ElseIf a = b Then
  "=" → result
ElseIf a > b Then
  ">" → result
Else
  "???" → result
EndIf
Disp string(a) & " " & result & " " & string(b)
```



## Toka


```toka
[ ( a b -- )
  2dup < [ ." a is less than b\n" ] ifTrue
  2dup > [ ." a is greater than b\n" ] ifTrue
       = [ ." a is equal to b\n" ] ifTrue
] is compare-integers

1 1 compare-integers
2 1 compare-integers
1 2 compare-integers
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

ASK "Please enter your first integer:": i1=""
ASK "Please enter your second integer:": i2=""

IF (i1!='digits'||i2!='digits') ERROR/STOP "Please insert digits"

IF (i1==i2) PRINT i1," is equal to     ",i2
IF (i1<i2)  PRINT i1," is less than    ",i2
IF (i1>i2)  PRINT i1," is greater than ",i2

```



## UNIX Shell

There are multiple examples here, because each shell has a different form of the 'read' command.

{{works with|ksh93}}


```bash
#!/bin/ksh
# tested with ksh93s+

builtin printf

integer a=0
integer b=0

read a?"Enter value of a: " || { print -u2 "Input of a aborted." ; exit 1 ; }
read b?"Enter value of b: " || { print -u2 "Input of b aborted." ; exit 1 ; }

if (( a < b )) ; then
    printf "%d is less than %d\n" a b
fi
if (( a == b )) ; then
    printf "%d is equal to %d\n" a b
fi
if (( a > b )) ; then
    printf "%d is greater than %d\n" a b
fi

exit 0
```


One can backport the previous code to pdksh, which has no builtin printf, but can call /usr/bin/printf as an external program.

{{works with|pdksh}}


```bash
#!/bin/ksh
# tested with pdksh

integer a=0
integer b=0

read a?"Enter value of a: " || { print -u2 "Input of a aborted." ; exit 1 ; }
read b?"Enter value of b: " || { print -u2 "Input of b aborted." ; exit 1 ; }

if (( a < b )) ; then
    printf "%d is less than %d\n" $a $b
fi
if (( a == b )) ; then
    printf "%d is equal to %d\n" $a $b
fi
if (( a > b )) ; then
    printf "%d is greater than %d\n" $a $b
fi

exit 0
```


----

{{works with|Bash}}


```bash
read -p "Enter two integers: " a b

if [ $a -gt $b ]; then comparison="greater than"
elif [ $a -lt $b ]; then comparison="less than"
elif [ $a -eq $b ]; then comparison="equal to"
else comparison="not comparable to"
fi

echo "${a} is ${comparison} ${b}"
```



## Ursa


```ursa
decl int first second
out "enter first integer:  " console
set first (in int console)
out "enter second integer: " console
set second (in int console)

if (= first second)
        out "the two integers are equal" endl console
end if
if (< first second)
        out first " is less than " second endl console
end if
if (> first second)
        out first " is greater than " second endl console
end if
```



## V


```v
[compare
  [ [>] ['less than' puts]
    [<] ['greater than' puts]
    [=] ['is equal' puts]
  ] when].

|2 3 compare
 greater than
|3 2 compare
 less than
|2 2 compare
 is equal
```


## Vala


```vala

void main(){
    int	a;
    int	b;

    stdout.printf("Please type in int 1\n");
    a = int.parse(stdin.read_line());

    stdout.printf("Please type in int 2\n");
    b =	int.parse(stdin.read_line());

    if (a < b)
        stdout.printf("%d is less than %d\n", a, b);
    if (a == b)
        stdout.printf("%d is equal to %d\n", a,	b);
    if (a > b)
        stdout.printf("%d is greater than %d\n", a, b);
}

```



## VBA


```vb
Public Sub integer_comparison()
    first_integer = CInt(InputBox("Give me an integer."))
    second_integer = CInt(InputBox("Give me another integer."))
    Debug.Print IIf(first_integer < second_integer, "first integer is smaller than second integer", "first integer is not smaller than second integer")
    Debug.Print IIf(first_integer = second_integer, "first integer is equal to second integer", "first integer is not equal to second integer")
    Debug.Print IIf(first_integer > second_integer, "first integer is bigger than second integer", "first integer is not bigger than second integer")
End Sub
```


## VBScript

Based on the BASIC

### ==Implementation==


```vb

option explicit

function eef( b, r1, r2 )
	if b then
		eef = r1
	else
		eef = r2
	end if
end function

dim a, b
wscript.stdout.write "First integer: "
a = cint(wscript.stdin.readline) 'force to integer

wscript.stdout.write "Second integer: "
b = cint(wscript.stdin.readline) 'force to integer

wscript.stdout.write "First integer is "
if a < b then wscript.stdout.write "less than "
if a = b then wscript.stdout.write "equal to "
if a > b then wscript.stdout.write "greater than "
wscript.echo "Second integer."

wscript.stdout.write "First integer is " & _
    eef( a < b, "less than ", _
    eef( a = b, "equal to ", _
    eef( a > b, "greater than ", vbnullstring ) ) ) & "Second integer."

```



## Visual Basic .NET

'''Platform:''' [[.NET]]

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Sub Main()

    Dim a = CInt(Console.ReadLine)
    Dim b = CInt(Console.ReadLine)

    'Using if statements
    If a < b Then Console.WriteLine("a is less than b")
    If a = b Then Console.WriteLine("a equals b")
    If a > b Then Console.WriteLine("a is greater than b")

    'Using Case
    Select Case a
        Case Is < b
            Console.WriteLine("a is less than b")
        Case b
            Console.WriteLine("a equals b")
        Case Is > b
            Console.WriteLine("a is greater than b")
    End Select

End Sub
```



## Wart


```wart
a <- (read)
a <- (read)
prn (if (a < b)
          : "a is less than b"
        (a > b)
          : "a is greater than b"
        :else
          : "a equals b")
```



## XLISP


```xlisp
(DEFUN COMPARE-INTEGERS ()
    (DISPLAY "Enter two integers separated by a space.")
    (NEWLINE)
    (DISPLAY "> ")
    (DEFINE A (READ))
    (DEFINE B (READ))
    (COND
        ((> A B) (DISPLAY "The first number is larger."))
        ((= A B) (DISPLAY "They are equal."))
        ((< A B) (DISPLAY "The first number is smaller."))))
```



## XPL0


```XPL0
code IntIn=10, Text=12;
int A, B;
[A:= IntIn(0);
 B:= IntIn(0);
if A<B then Text(0, "A<B");
if A=B then Text(0, "A=B");
if A>B then Text(0, "A>B");
CrLf(0);
]
```



## XSLT

Because XSLT uses XML syntax, the less than and greater than operators which would normally be written '<' and '>' must be escaped using character entities, even inside of XPath expressions.


```xml
<xsl:template name="compare">
  <xsl:param name="a" select="1"/>
  <xsl:param name="b" select="2"/>
  <fo:block>
  <xsl:choose>
    <xsl:when test="$a &lt; $b">a &lt; b</xsl:when>
    <xsl:when test="$a &gt; $b">a &gt; b</xsl:when>
    <xsl:when test="$a = $b">a = b</xsl:when>
  </xsl:choose>
  </fo:block>
 </xsl:template>
```



## zkl


```zkl
var x,y; x,y=ask("Two ints: ").split(" ").apply("toInt")
(if (x==y) "equal" else if (x<y) "less" else if(x>y) "greater").println()
```

{{out}}

```txt

Two ints: 3 2
greater

```



## ZX Spectrum Basic


```zxbasic
10 INPUT "Enter two integers: ";a;" ";b
20 PRINT a;" is ";("less than " AND (a<b));("equal to " AND (a=b));("greather than " AND (a>b));b
```

