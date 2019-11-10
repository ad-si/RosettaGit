+++
title = "Conditional structures"
description = ""
date = 2019-10-09T07:38:44Z
aliases = []
[extra]
id = 1787
[taxonomies]
categories = []
tags = []
+++

{{Task|Control Structures}}
{{Control Structures}}
[[Category:Simple]]

;Task:
List the   ''conditional structures''   offered by a programming language.

Common conditional structures are     '''if-then-else'''     and     '''switch'''.





## 11l

===if-else===

```11l
I x == 0
   foo()
E I x == 1
   bar()
E
   baz()
```



### switch


```11l
S x
   0
      foo()
   1
      bar()
   E
      baz()
```



## 360 Assembly

Here are the branch mnemonic opcodes:

```360asm
* Unconditional Branch or No Branch:
        B      label       Unconditional
        BR     Rx          "
        NOP    label       No Operation
        NOPR   Rx          "
* After Compare Instructions
        BH     label       Branch on High
        BHR    Rx          "
        BL     label       Branch on Low
        BLR    Rx          "
        BE     label       Branch on Equal
        BER    Rx          "
        BNH    label       Branch on Not High
        BNHR   Rx          "
        BNL    label       Branch on Not Low
        BNLR   Rx          "
        BNE    label       Branch on Not Equal
        BNER   Rx          "
* After Arithmetic Instructions:
        BP     label       Branch on Plus
        BPR    Rx          "
        BM     label       Branch on Minus
        BMR    Rx          "
        BZ     label       Branch on Zero
        BZR    Rx          "
        BO     label       Branch on Overflow
        BOR    Rx          "
        BNP    label       Branch on Not Plus
        BNPR   Rx          "
        BNM    label       Branch on Not Minus
        BNMR   Rx          "
        BNZ    label       Branch on Not Zero
        BNZR   Rx          "
        BNO    label       Branch on No Overflow
        BNOR   Rx          "
* After Test Under Mask Instructions:
        BO     label       Branch if Ones
        BOR    Rx          "
        BM     label       Branch if Mixed
        BMR    Rx          "
        BZ     label       Branch if Zero
        BZR    Rx          "
        BNO    label       Branch if Not Ones
        BNOR   Rx          "
        BNM    label       Branch if Not Mixed
        BNMR   Rx          "
        BNZ    label       Branch if Not Zero
        BNZR   Rx          "
```

The ASM (Assembler Structured Macros) toolkit brings structures to IBM assembler 360.

```360asm
      expression:
         opcode,op1,rel,op2
         opcode,op1,rel,op2,OR,opcode,op1,rel,op2
         opcode,op1,rel,op2,AND,opcode,op1,rel,op2
           opcode::=C,CH,CR,CLC,CLI,CLCL, LTR, CP,CE,CD,...
           rel::=EQ,NE,LT,LE,GT,GE,  (fortran style)
                 E,L,H,NE,NL,NH      (assembler style)
                 P (plus), M (minus) ,Z (zero) ,O (overflow)
           opcode::=CLM,TM
           rel::=O (ones),M (mixed) ,Z (zeros)

* IF
         IF     expression [THEN]
           ...
         ELSEIF expression [THEN]
           ...
         ELSE
           ...
         ENDIF

         IF     C,R4,EQ,=F'10' THEN     if     r4=10 then
           MVI  PG,C'A'                   pg='A'
         ELSEIF C,R4,EQ,=F'11' THEN     elseif r4=11 then
           MVI  PG,C'B'                   pg='B'
         ELSEIF C,R4,EQ,=F'12' THEN     elseif r4=12 then
           MVI  PG,C'C'                   pg='C'
         ELSE                           else
           MV   PG,C'?'                   pg='?'
         ENDIF                          end if

* SELECT
         SELECT expressionpart1
           WHEN expressionpart2a
             ...
           WHEN expressionpart2b
             ...
          OTHRWISE
             ...
         ENDSEL

* example SELECT type 1
         SELECT CLI,HEXAFLAG,EQ         select hexaflag=
           WHEN X'20'                     when x'20'
             MVI  PG,C'<'                   pg='<'
           WHEN X'21'                     when x'21'
             MVI  PG,C'!'                   pg='!'
           WHEN X'22'                     when x'21'
             MVI  PG,C'>'                   pg='>'
           OTHRWISE                       otherwise
             MVI  PG,C'?'                   pg='?'
         ENDSEL                         end select

* example SELECT type 2
         SELECT                         select
           WHEN C,DELTA,LT,0              when delta<0
             MVC  PG,=C'0 SOL'              pg='0 SOL'
           WHEN C,DELTA,EQ,0              when delta=0
             MVC  PG,=C'1 SOL''              pg='0 SOL'
           WHEN C,DELTA,GT,0              when delta>0
             MVC  PG,=C'2 SOL''              pg='0 SOL'
         ENDSEL                         end select

* CASE
         CASENTRY R4                    select case r4
           CASE 1                         case 1
             LA     R5,1                    r5=1
           CASE 3                         case 3
             LA     R5,2                    r5=2
           CASE 5                         case 5
             LA     R5,3                    r5=1
           CASE 7                         case 7
             LA     R5,4                    r5=4
         ENDCASE                        end select
```



## 6502 Assembly

6502 Assembly has 8 conditional branch instructions; each instruction will test the appropriate flag and condition and jump between -128 and 127 bytes.
To understand these conditional instructions, it is helpful to remember that the comparison instructions (CMP, CPX, CPY) set the flags as if a subtraction had occurred:

```6502asm
		LDA #10
		CMP #11
```

Following these instructions, the accumulator will still hold 10 but the flags are set as if you had instructed the processor to perform 10 - 11.
The result is -1, so the sign flag will be set, the zero flag will be cleared, the overflow flag will be cleared, and the carry flag will be set.

```6502asm
		BNE 		;Branch on Not Equal - branch when the zero flag is set
		BEQ 		;Branch on EQual - branch when the zero flag is set.
				;The zero flag is set when the result of an operation is zero

		BMI 		;Branch on MInus
		BPL 		;Branch on PLus - branch when the sign flag is cleared/set.
				;The sign flag is set when the result of an instruction is a negative number
				;and cleared when the result is a positive number

		BVS 		;Branch on oVerflow Set
		BVC 		;Branch on oVerflow Cleared - branch when the overflow flag is cleared/set.
				;The overflow flag is set when the result of an addition/subtraction would
				;result in a number larger than 127 or smaller than -128

		BCS		;Branch on Carry Set
		BCC		;Branch on Carry Clear - branch when the carry flag is cleared/set.
				;The carry flag is set when an addition produced a carry and when
				;a subtraction produced a borrow and cleared if an addition/subtraction
				;does not produce a carry/borrow.  The carry flag also holds bits
				;after shifts and rotates.
```

In the following example, the branch will be taken if memory location Variable holds 200:

```6502asm
		LDA #200
		CMP Variable
		BEQ #3			;if equal, skip ahead 3 bytes...
		CLC			;if unequal, continue executing instructions
		ADC #1
		STA OtherVariable	;				...to here.
```

Because you don't have to perform a comparison to set the flags, you can perform very fast checks in interative loops:

```6502asm
		LDX #100
Loop:		...do something
		DEX
		BNE Loop
```

This code will loop until X is zero.
Most assemblers will figure out the correct offset for you if you use a label in place of the offset after a branch instruction, as in the above example.


## ActionScript

:''See [[Conditional Structures#JavaScript|JavaScript]]''


## Ada

===if-then-else===

```ada
type Restricted is range 1..10;
My_Var : Restricted;

if My_Var = 5 then
  -- do something
elsif My_Var > 5 then
  -- do something
else
  -- do something
end if;
```


### case with a default alternative


```ada
type Days is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
Today : Days;

case Today is
  when Saturday | Sunday =>
     null;
  when Monday =>
     Compute_Starting_Balance;
  when Friday =>
     Compute_Ending_Balance;
  when others =>
     Accumulate_Sales;
end case;
```



### case without a default

When there is no '''when others''' clause, the compiler will complain about any uncovered alternative. This defends against a common reason for bugs in other languages.
I.e., the following code is syntactically incorrect:


```ada
case Today is
  when Monday =>
     Compute_Starting_Balance;
  when Friday =>
     Compute_Ending_Balance;
  when Tuesday .. Thursday =>
     Accumulate_Sales;
  -- ignore Saturday and Sunday
end case;
```


The syntactically correct version:


```ada
case Today is
  when Saturday | Sunday =>
     null; -- don't do anything, if Today is Saturday or Sunday
  when Monday =>
     Compute_Starting_Balance;
  when Friday =>
     Compute_Ending_Balance;
  when Tuesday .. Thursday =>
     Accumulate_Sales;
end case;
```



### select

Select provides conditional acceptance of entry calls.
Select can also be used to conditionally call an entry


### =Conditional Accept=


```ada
select
   accept first_entry;
   -- do something
   or accept second_entry;
   -- do something
   or terminate;
end select;
```



### =Conditional entry call=

A selective entry call provides a way to time-out an entry call.
Without the time-out the calling task will suspend until the entry call is accepted.

```ada
select
  My_Task.Start;
or
  delay Timeout_Period;
end select;
```

The entry Start on the task My_Task will be called.
If My_Task accepts the entry call before the timer expires the timer is canceled. If the timeout expires before the entry call is accepted the entry call is canceled.


## Aikido


### Conditional Expressions


```aikido

var x = loggedin ? sessionid : -1


```



### if..elif..else


```aikido

if (value > 40) {
    println ("OK")
} elif (value < 20) {
    println ("FAILED")
} else {
    println ("RETRY")
}

```



### switch


```aikido

switch (arg) {
case "-d":
case "--debug":
    debug = true
    break
case "-f":
    force = true
    break
default:
    throw "Unknown option " + arg
}

switch (value) {
case > 40:
    println ("OK")
    break
case < 20:
    println ("FAILED")
    break
case in 50..59:
   println ("WIERD")
   // fall through
default:
    println ("RETRY")
}

```



## Aime


===If-elif-else===


```aime
if (c1) {
  // first condition is true...
} elif (c2) {
  // second condition is true...
} elif (c3) {
  // third condition is true...
} else {
  // none was true...
}
```




## ALGOL 60

Algol 60 has invented the famous '''if then else block''' structure.
Algol 60 has conditional expressions of the form:
  expression::=  '''if''' conditional_expression '''then''' expression '''else''' expression
    K:='''if''' X=Y '''then''' I '''else''' J
Algol 60 has conditional statements of the form:
  statement::=  '''if''' conditional_expression '''then''' statement '''else''' statement
    '''if''' X=Y '''then''' K:=I '''else''' K:=J
  statement::=  '''if''' conditional_expression '''then''' statement
    '''if''' X=Y '''then''' K:=I
An example:

```algol60
    'IF' I=1 'THEN' OUTINTEGER(1,I);

    'IF' I<J 'THEN' OUTSTRING(1,'(' : I<J')')
             'ELSE' OUTSTRING(1,'(' : I>=J')');

    'IF' I>=J 'THEN' 'BEGIN'
        OUTSTRING(1,'(' I=')');
        OUTINTEGER(1,I)
    'END'
    'ELSE' 'BEGIN'
        OUTSTRING(1,'(' J=')');
        OUTINTEGER(1,J)
    'END'
```

Algol 60 has also a switch structure:
  declaration::=  '''switch''' switch:=list_of labels
    statement::=  '''goto''' switch[expression]
An example:

```algol60
  'SWITCH' TARGET:=L1,L2,L3;
  ...
  'GOTO' TARGET(/J/);
  L1: OUTSTRING(1,'('AA')');
  L2: OUTSTRING(1,'('BB')');
  L3: OUTSTRING(1,'('CC')');
```



## ALGOL 68

See [[Conditional Structures/ALGOL 68]]



## ALGOL W


```algolw
begin
    integer a, b, c;

    a := 1; b := 2; c := 3;

    % algol W has the traditional Algol if-the-else statement                %
    % there is no "elseif" contraction                                       %
    if a = b
    then write( "a = b" )
    else if a = c
         then write( "a = c" )
         else write( "a is ", a );

    % if-then-else can also be used in an expression                         %
    write( if a < 4 then "lt 4" else "ge 4" );

    % algol W also has a "case" statement, an integer expression is used to  %
    % select the statement to execute. If the expression evaluates to 1,    %
    % the first statement is executed, if 2, the second is executed etc.     %
    % If the expression is less than 1 or greater than the number of         %
    % statements, a run time error occurs                                    %
    case a + b of
    begin write( "a + b is one"   )
        ; write( "a + b is two"   )
        ; write( "a + b is three" )
        ; write( "a + b is four"  )
    end;

    % there is also an expression form of the case:                          %
    write( case c - a of ( "one", "two", "three", "four" ) )

end.
```

{{out}}

```txt

a is              1
lt 4
a + b is three
two

```



## AmbientTalk


===If-then-else===

In AmbientTalk, if:then:else: is a keyworded message (as in Smalltalk).
The first argument should be a boolean expression.
The second and third arguments should be blocks (aka anonymous functions or thunks).


```ambienttalk

if: condition then: {
  // condition is true...
} else: {
  // condition is false...
}

```



### IfTrue/IfFalse


One can also send a message to the boolean objects true and false:


```ambienttalk

condition.ifTrue: { /* condition is true... */ } ifFalse: { /* condition is false... */ }

```



## AmigaE

'''IF-THEN-ELSE'''

```amigae
IF condition
  -> if condition is true...
ELSEIF condition2
  -> else if condition2 is true...
ELSE
  -> if all other conditions are not true...
ENDIF
```


or on one single line:


```amigae>IF condition THEN statement</lang


'''Ternary IF THEN ELSE'''

The IF-THEN-ELSE can be used like ternary operator (?: in C)

```amigae
DEF c
c := IF condition THEN 78 ELSE 19
```


'''SELECT-CASE'''


```amigae
SELECT var
  CASE n1
    -> code
  CASE n2
    -> code
  DEFAULT
    -> no one of the previous case...
ENDSELECT
```


Another version allows for ranges:


```amigae
SELECT max_possible_value OF var
  CASE n1
    -> code
  CASE n2 TO n3, n4
    -> more
  CASE n5 TO n6, n7 TO n8
    -> more...
  DEFAULT
    -> none of previous ones
ENDSELECT
```


The biggest among n1, n2 and so on, must be not bigger than max_possible_value.


## Apex

===if-then-else===

```java
if (s == 'Hello World') {
    foo();
} else if (s == 'Bye World') {
    bar();
} else {
    deusEx();
}
```

Java also supports [[wp:Short-circuit_evaluation|short-circuit evaluation]]. So in a conditional like this:

```java
if(obj != null && obj.foo()){
   aMethod();
}
```

<tt>obj.foo()</tt> will not be executed if <tt>obj != null</tt> returns false. It is possible to have conditionals without short circuit evaluation using the <tt>&</tt> and <tt>|</tt> operators (from [[Bitwise operations]]). So in this conditional:

```java
if(obj != null & obj.foo()){
   aMethod();
}
```

You will get a null pointer exception if obj is null.

### ternary



```java
s == 'Hello World' ? foo() : bar();
```



### switch

Apex does not support switch / case statements.


## AppleScript

===if-then-else===

```applescript
if myVar is "ok" then return true

set i to 0
if i is 0 then
       return "zero"
else if i mod 2 is 0 then
       return "even"
else
       return "odd"
end if
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program condstr.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessTest1: .asciz "The test 1 is equal.\n"
szMessTest1N: .asciz "The test 1 is not equal.\n"
szMessTest2: .asciz "The test 2 is equal.\n"
szMessTest2N: .asciz "The test 2 is not equal.\n"
szMessTest3: .asciz "The test 3 is <.\n"
szMessTest3N: .asciz "The test 3 is >.\n"
szMessTest4: .asciz "The test 4 is <=.\n"
szMessTest4N: .asciz "The test 4 is >.\n"
szMessTest5: .asciz "The test 5 is negative.\n"
szMessTest5N: .asciz "The test 5 is positive ou equal 0.\n"
szMessTest6: .asciz "Test 6 : carry is off.\n"
szMessTest6N: .asciz "Test 6 : carry is set.\n"
szMessTest7: .asciz "Test 7 : no overflow.\n"
szMessTest7N: .asciz "Test 7 : overflow.\n"
szMessTest8: .asciz "Test 8 : then.\n"
szMessTest8N: .asciz "Test 8 : else.\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    @ test equal zero, not equal zero
    @movs r1,#0      @ comments
    movs r1,#1          @  @ s --> flags   and uncomments
    ldreq r0,iAdrszMessTest1
    ldrne r0,iAdrszMessTest1N
    bl affichageMess

    @ test equal 5, not equal 5
    @mov r1,#5
    mov r1,#10
    cmp r1,#5
    ldreq r0,iAdrszMessTest2
    ldrne r0,iAdrszMessTest2N
    bl affichageMess

    @ test < 5,  > 5  SIGNED
    mov r1,#-10
    @mov r1,#10
    cmp r1,#5
    ldrlt r0,iAdrszMessTest3
    ldrgt r0,iAdrszMessTest3N
    bl affichageMess

    @ test < 5,  > 5  UNSIGNED
    @mov r1,#-10
    mov r1,#2
    cmp r1,#5
    ldrls r0,iAdrszMessTest4
    ldrhi r0,iAdrszMessTest4N
    bl affichageMess

    @ test < 0,  > 0
    @movs r1,#-10
    movs r1,#2     @ s --> flags
    ldrmi r0,iAdrszMessTest5
    ldrpl r0,iAdrszMessTest5N
    bl affichageMess

    @ carry off carry on
    @mov r1,#-10     @ for carry set
    @mov r1,#10  @ for carry off
    mov r1,#(2<<30) - 1   @ for carry off
    adds r1,#20    @ s --> flags
    ldrcc r0,iAdrszMessTest6    @ carry clear
    ldrcs r0,iAdrszMessTest6N   @ carry set
    bl affichageMess

    @ overflow off overflow on
    @mov r1,#-10     @ for not overflow
    @mov r1,#10  @ for not overflow
    mov r1,#(2<<30) - 1  @ for overflow
    adds r1,#20    @ s --> flags
    ldrvc r0,iAdrszMessTest7    @ overflow off
    ldrvs r0,iAdrszMessTest7N   @ overflow on
    bl affichageMess

    @ other if then else
    mov r1,#5  @ for then
    @mov r1,#20  @ for else
    cmp r1,#10
    ble 1f         @ less or equal
    @bge 1f      @ greather or equal
    @else
    ldr r0,iAdrszMessTest8N    @ overflow off
    bl affichageMess
    b 2f
1:   @ then
   ldr r0,iAdrszMessTest8    @ overflow off
    bl affichageMess
2:

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszMessTest1:		.int szMessTest1
iAdrszMessTest1N:		.int szMessTest1N
iAdrszMessTest2:		.int szMessTest2
iAdrszMessTest2N:		.int szMessTest2N
iAdrszMessTest3:		.int szMessTest3
iAdrszMessTest3N:		.int szMessTest3N
iAdrszMessTest4:		.int szMessTest4
iAdrszMessTest4N:		.int szMessTest4N
iAdrszMessTest5:		.int szMessTest5
iAdrszMessTest5N:		.int szMessTest5N
iAdrszMessTest6:		.int szMessTest6
iAdrszMessTest6N:		.int szMessTest6N
iAdrszMessTest7:		.int szMessTest7
iAdrszMessTest7N:		.int szMessTest7N
iAdrszMessTest8:		.int szMessTest8
iAdrszMessTest8N:		.int szMessTest8N
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

===if-then-else===

```arturo
num 2

if num=2 {
	"yep, num is 2"
} {
	"something went wrong..."
}

```


{{out}}


```txt
yep, num is 2
```



### Guard statement


```arturo
num 2

| num!=2 { "something went wrong..." }
```



## Astro


```python
if x == 0:
    foo()
elif x == 1:
    bar()
elif x == 2:
    baz()
else:
    qux()

match x:
    0 => foo()
    1 => bar()
    2 => baz()
    _ => qux()

(a) ? b : c
```



## AutoHotkey


===if, else if, else===

```AutoHotkey
x = 1
If x
  MsgBox, x is %x%
Else If x > 1
  MsgBox, x is %x%
Else
  MsgBox, x is %x%
```



### ternary if


```AutoHotkey
x = 2
y = 1
var := x > y ? 2 : 3
MsgBox, % var
```


===while (looping if)===

```AutoHotkey
While (A_Index < 3) {
  MsgBox, %A_Index% is less than 3
}
```



## AutoIt

===If, ElseIf, Else===

```AutoIt>If <expression
 Then
    statements
    ...
[ElseIf expression-n Then
    [elseif statements ... ]]
    ...
[Else
    [else statements]
    ...
EndIf

```


### Select Case


```AutoIt
Select
    Case <expression>
        statement1
        ...
    [Case
        statement2
        ...]
    [Case Else
        statementN
        ...]
EndSelect

```


### Switch Case


```AutoIt>Switch <expression

    Case <value> [To <value>] [,<value> [To <value>] ...]
        statement1
        ...
    [Case <value> [To <value>] [,<value> [To <value>] ...]
        statement2
        ...]
    [Case Else
        statementN
        ...]
EndSwitch

```

--[[User:BugFix|BugFix]] ([[User talk:BugFix|talk]]) 15:39, 13 November 2013 (UTC)


## AWK

Conditionals in awk are modelled after C:

```awk
if(i<0) i=0; else i=42
```

For a branch with more than a single statement, this needs braces:

```awk

if(i<0) {
    i=0; j=1
  } else {
    i=42; j=2
  }
```

There is also the ternary conditional:

```awk
i=(i<0? 0: 42)
```



## Axe


Expressions that evaluate to zero are considered false. Expressions that evaluate to nonzero are considered true.


### Simple


```axe
If 1
 YEP()
End
```



### Inverse If


```axe
!If 1
 NOPE()
End
```


===If-Else===

```axe
If 1
 YEP()
Else
 NOPE()
End
```


Axe has no support for switch-like statements. If-ElseIf-Else structures are required to achieve the same goal.

===If-ElseIf-Else===

```axe
If 1=0
 NOPE()
ElseIf 1=1
 YEP()
Else
 NOPE()
End
```


===If-InverseElseIf-Else===

```axe
If 1=0
 NOPE()
Else!If 1=2
 YEP()
Else
 NOPE()
End
```



## Babel



### Simple select



```babel

"foo" "bar" 3 4 > sel <<

```


Prints "foo" since '3 4 >' evaluates to false, which causes sel to remove "bar" from the stack.

===If-Then-Else===


```babel

    {3 4 >}
        {"foo"}
        {"bar"}
    ifte
    <<

```


Prints "bar" because the first line is the "if", the second line is the "then" and the last line is the "else", and '3 4 >' evaluates to false.


### Conditional



```babel

    ({3 4 >} {"Three is greater than four" }
    {3 3 >}  {"Three is greater than three"}
    {3 2 >}  {"Three is greater than two"  }
    {3 1 >}  {"Three is greater than one"  })
    cond
    <<

```


Prints "Three is greater than two", as expected.


## BASIC


===if-then-else===

BASIC can use the if statement to perform conditional operations:


```basic
10 LET A%=1: REM A HAS A VALUE OF TRUE
20 IF A% THEN PRINT "A IS TRUE"
30 WE CAN OF COURSE USE EXPRESSIONS
40 IF A%<>0 THEN PRINT "A IS TRUE"
50 IF NOT(A%) THEN PRINT "A IS FALSE"
60 REM SOME VERSIONS OF BASIC PROVIDE AN ELSE KEYWORD
70 IF A% THEN PRINT "A IS TRUE" ELSE PRINT "A IS FALSE"
```


Here are code snippets from a more modern variant that does not need line numbers:

{{works with|QuickBasic|4.5}}
{{works with|FreeBASIC|0.20.0}}
Single line IF does not require END IF


```qbasic
IF x = 0 THEN doSomething
IF x < 0 THEN doSomething ELSE doOtherThing
```


Multi-line IF:


```qbasic>IF x
 0 AND x < 10 THEN
   'do stuff
ELSE IF x = 0 THEN
   'do other stuff
ELSE
   'do more stuff
END IF
```


Like in [[#C|C]], any non-zero value is interpreted as True:


```qbasic
IF aNumber THEN
   'the number is not 0
ELSE
   'the number is 0
END IF
```



### select case

{{works with|QuickBasic|4.5}}
{{works with|FreeBASIC|0.20.0}}
The condition in each case branch can be one or more constants or variables, a range or an expression.


```qbasic
SELECT CASE expression
CASE 1
   'do stuff
CASE 2, 3
   'do other stuff
CASE 3.1 TO 9.9
   'do this
CASE IS >= 10
   'do that
CASE ELSE
   'default case
END SELECT
```


===Computed ON-GOTO===
Older line-numbered BASICs had a mechanism for vectoring execution based on the contents of a numeric variable (a low-budget case statement).
 ON V GOTO 120,150,150,170

or:


```basic
10 INPUT "Enter 1,2 or 3: ";v
20 GOTO v * 100
99 STOP
100 PRINT "Apple"
110 STOP
200 PRINT "Banana"
210 STOP
300 PRINT "Cherry"
310 STOP
```



### Conditional loops


Some variants of basic support conditional loops:


```bbcbasic
10 REM while loop
20 L=0
30 WHILE L<5
40 PRINT L
50 L=L+1
60 WEND
70 REM repeat loop
80 L=1
90 REPEAT
100 PRINT L
110 L=L+1
120 UNTIL L>5
```



## BASIC256

Of these, we'll need to see how nested if statements parse.

```txt

# Begin Case / Case / End Case, Do / Until, If Then, While / End While


begin case
   case boolean_expr
      statement(s)
   case boolean_expr
      statement(s)
   else
      statement(s)
end case



do
   statement(s)
until boolean_expr



if booleanexpr then
   statement(s)
end if

if booleanexpr then
   statement(s)
else
   statement(s)
end if



while boolean_expr
   statement(s)
end while

```

I think this test shows that nested if statements parse as they do in c.

```BASIC256

for i = 0 to 1
  for j = 0 to 1
    print i
    print j
    if (i) then
      if (j) then
        print "i is true j is true"
      else
        print "i is true j is false"
      end if
    else
      if (j) then
        print "i is false j is true"
      else
        print "i is false j is false"
      end if
    end if
  next j
next i

```



## BBC BASIC


```bbcbasic
      REM Single-line IF ... THEN ... ELSE (ELSE clause is optional):
      IF condition% THEN statements ELSE statements

      REM Multi-line IF ... ENDIF (ELSE clause is optional):
      IF condition% THEN
        statements
      ELSE
        statements
      ENDIF

      REM CASE ... ENDCASE (OTHERWISE clause is optional):
      CASE expression OF
        WHEN value1: statements
        WHEN value2: statements
          ...
        OTHERWISE: statements
      ENDCASE

      REM ON ... GOTO (ELSE clause is optional):
      ON expression% GOTO dest1, dest2 ... ELSE statements

      REM ON ...GOSUB (ELSE clause is optional):
      ON expression% GOSUB dest1, dest2 ... ELSE statements

      REM ON ... PROC (ELSE clause is optional):
      ON expression% PROCone, PROCtwo ... ELSE statements
```



## Batch File

IF syntax:

```dos

IF [NOT] ERRORLEVEL number          command
IF [NOT] string1==string2           command
IF [NOT] EXIST filename             command
IF CMDEXTVERSION number             command
IF DEFINED variable                 command
IF [/I] string1 compare-op string2  command
   where compare-op is:
   EQU - equal
   NEQ - not equal
   LSS - less than
   LEQ - less than or equal
   GTR - greater than
   GEQ - greater than or equal
   /I    case insensitive string compares

```

The ELSE clause must be on the same line as the command after the IF.
For example:

```dos

IF EXIST %filename% (
  del %filename%
) ELSE (
  echo %filename% not found
)

```



## beeswax


beeswax has 4 conditional operators that act like the ternary '''?:''' operator in C or Julia. Due to the 2-dimensional nature of beeswax it is possible to realize complex branching structures this way.

The 4 conditional operators are:

```Beeswax

'      lstack top value == 0 ? skip next instruction : don’t skip next instruction.
"      lstack top value  > 0 ? skip next instruction : don’t skip next instruction.
K      lstack top value == 2nd value ? skip next instruction : don’t skip next instruction.
L      lstack top value  > 2nd value ? skip next instruction : don’t skip next instruction.
```


Example:

```Beeswax
_`Enter integer n:`T'p`n = 0`>N`Enter integer m:`T'p`m = 0`>` and `Kp`m = n`;
                     >`n > 0`d                     >`m > 0`d        >Lp`m > n`;
                                                                      >`m < n`;
```


Example output:

```Beeswax
Enter integer n:
i3
n > 0
Enter integer m:
i0
m = 0 and m < n
```



## Befunge

Befunge only has one conditional structure, which comes in two flavors: ''vertical IF'' ( | ) and ''horizontal IF'' ( _ ).
Befunge only has two boolean commands, ''greater-than'' ( ` ) and ''not'' ( ! ).
These snippets input a number and use the conditional operators to print a "0" if it is zero and an "X" otherwise.


```befunge>v
 "X",@ non-zero
> & |
    > "0",@ zero
```


'''#''' is the skip command.
It unconditionally skips one character, allowing a little flexibility in flow control.


```befunge
& #v_ "0",@ zero
   >  "X",@ non-zero
```



## blz

==if-else==

```blz

if i % 2 == 0
    print("even")
else
    print("odd")
end

```



## Bori

===if-elif-else===

```bori

if (i == 0)
    return "zero";
elif (i % 2)
    return "odd";
else
    return "even";

```



## Bracmat

=== "if .. then .. else .." type of branching ===
Bracmat uses & and | for branching.
These binary operators are like && and || in C-like languages.
Bracmat does not have the notion of Boolean variables, but marks all evaluated expressions as either succeeded or failed.
If the left hand side of the & operator has succeeded, Bracmat goes on evaluating the right hand side.
Only if both of left and right hand sides succeed, the expression tree headed by the & operator as a whole succeeds. Likewise, only if both of left and right hand sides of an expression tree headed by | fail, the expression tree as a whole fails.
Evaluated expressions are just that: expressions.
The following expression writes "That's what I thought." to your screen and evaluates to the expression "Right".


```bracmat
    2+2:5
  & put$"Strange, must check that Bracmat interpreter."
  & 0
|   put$"That's what I thought."
  & Right
```


=== switch-like branching ===
Use a patterns with alternations. Note that the match-expression (the tree headed by the : operator) evaluates to the left hand side of the : operator.
In the following example, the resulting expression is a single node containing "4".


```bracmat
  2+2
: (   (<3|>5)
    & put$"Not quite, must check that Bracmat interpreter."
  |   (3|5)
    & put$"Not far off, but must check that Bracmat interpreter some day."
  |   ?
    & put$"That's what I thought."
  )

```


=={{header|Brainfuck}}==

Brainfuck has two conditional jump instructions, [ and ]. the [ instruction jumps forward to the corresponding ] instruction if the value at the current memory cell is zero, while the ] instruction jumps back if the current memory cell is nonzero.
Thus in the following sequence:


```bf
[.]
```


The . instruction will be skipped, while the following sequence


```bf
+[.]
```


will result in an infinite loop.  Finally, in the following sequence


```bf
+[.-]
```


The . instruction will be executed once.


## Burlesque


Using the ''Choose'' command:


```burlesque

blsq ) 9 2.%{"Odd""Even"}ch
"Odd"

```


Using the ''If'' command (produce next even number if odd):


```burlesque

blsq ) 9^^2.%{+.}if
10
blsq ) 10^^2.%{+.}if
10

```


Using the ''IfThenElse'' command (produce next odd number if even or previous even number if odd):


```burlesque

blsq ) 10^^2.%{-.}\/{+.}\/ie
11
blsq ) 9^^2.%{-.}\/{+.}\/ie
8

```


Emulating Switch-Case behaviour:


```burlesque

blsq ) {"Hate tomatos" "Like Bananas" "Hate Apples"}{"Tomato" "Banana" "Apple"}"Banana"Fi!!
"Like Bananas"
blsq ) {"Hate tomatos" "Like Bananas" "Hate Apples"}{"Tomato" "Banana" "Apple"}"Apple"Fi!!
"Hate Apples"

```



## C

See [[Conditional Structures/C]]


## C++

=== Run-Time Control Structures ===
:''See [[Conditional Structures#C|C]]''

=== Compile-Time Control Structures ===

### = Preprocessor Techniques =


:''See [[Conditional Structures#C|C]]''


### = Template metaprogramming =


Selecting a type depending on a compile time condition


```cpp
template<bool Condition, typename ThenType, typename Elsetype> struct ifthenelse;

template<typename ThenType, typename ElseType> struct ifthenelse<true, ThenType, ElseType>
{
  typedef ThenType type;
};

template<typename ThenType, typename ElseType> struct ifthenelse<false, ThenType, ElseType>
{
  typedef ElseType type;
};

// example usage: select type based on size
ifthenelse<INT_MAX == 32767, // 16 bit int?
           long int,         // in that case, we'll need a long int
           int>              // otherwise an int will do
  ::type myvar;              // define variable myvar with that type
```



## Clean


### if

There are no ''then'' or ''else'' keyword in Clean.
The second argument of <tt>if</tt> is the then-part, the third argument is the else-part.

```clean>bool2int b = if b 1 0</lang


===case-of===

```clean
case 6 * 7 of
    42 -> "Correct"
    _  -> "Wrong" // default, matches anything
```



### function alternatives


```clean
answer 42 = True
answer _ = False
```



### guards


```clean
answer x
    | x == 42   = True
    | otherwise = False

case 6 * 7 of
    n | n < 0 -> "Not even close"
    42        -> "Correct"
    // no default, could result in a run-time error
```



## Clipper

'''if-elseif-else-endif'''

```clipper
IF x == 1
   SomeFunc1()
ELSEIF x == 2
   SomeFunc2()
ELSE
   SomeFunc()
ENDIF
```


'''do case'''

```clipper
DO CASE
CASE x == 1
   SomeFunc1()
CASE x == 2
   SomeFunc2()
OTHERWISE
   SomeFunc()
ENDCASE
```



## Clojure

===if-then-else===

```clojure
(if (= 1 1) :yes :no) ; returns :yes

(if (= 1 2) :yes :no) ; returns :no

(if (= 1 2) :yes) ; returns nil
```



### when

Similar to if, but body in an implicit do block allowing multiple statements.
No facility for providing an else. <code>when</code> is defined as a macro.

```clojure
(when x
  (print "hello")
  (println " world")
  5) ; when x is logical true, prints "hello world" and returns 5; otherwise does nothing, returns nil
```



### cond

The cond macro takes a series of test/result pairs, evaluating each test until one resolves to logical true, then evaluates its result.
Returns nil if none of the tests yield true.

```clojure
(cond
  (= 1 2) :no) ; returns nil

(cond
  (= 1 2) :no
  (= 1 1) :yes) ; returns :yes
```

Since non-nil objects are logical true, by convention the keyword :else is used to yield a default result.

```clojure
(cond
  (= 1 2) :no
  :else :yes) ; returns :yes
```



### condp

Similar to cond, but useful when each test differs by only one variant.

```clojure
(condp < 3
  4 :a  ; cond equivalent would be (< 4 3) :a
  3 :b
  2 :c
  1 :d) ; returns :c
```

Optionally takes a final arg to be used as the default result if none of the tests match.

```clojure
(condp < 3
  4 :a
  3 :b
  :no-match) ; returns :no-match
```



### case

{{Works with|Clojure|1.2}}

```clojure
(case 2
  0 (println "0")
  1 (println "1")
  2 (println "2")) ; prints 2.
```



## CMake


```cmake
set(num 5)

if(num GREATER 100)
  message("${num} is very large!")
elseif(num GREATER 10)
  message("${num} is large.")
else()
  message("${num} is small.")
  message("We might want a bigger number.")
endif()
```


The if() and elseif() commands evaluate boolean expressions like ''num GREATER 100''; refer to [http://www.cmake.org/cmake/help/cmake-2-8-docs.html#command:if cmake --help-command if].
The elseif() and else() sections are optional.


## COBOL

===if-then-else===

```cobol
if condition-1
    imperative-statement-1
else
    imperative-statement-2
end-if

if condition-1
    if condition-a
        imperative-statement-1a
    else
        imperative-statement-1
    end-if
else
    if condition-a
        imperative-statement-2a
    else
        imperative-statement-2
    end-if
end-if
```



### evaluate


```cobol
evaluate identifier-1
when 'good'
    good-imperative-statement
when 'bad'
    bad-imperative-statement
when 'ugly'
when 'awful'
    ugly-or-awful-imperative-statement
when other
    default-imperative-statement
end-evaluate

evaluate true
when condition-1
    condition-1-imperative-statement
when condition-2
    condition-2-imperative-statement
when condition-3
    condition-3-imperative-statement
when other
    default-condition-imperative-statement
end-evaluate

evaluate identifier-1 also identifier-2
when 10 also 20
   one-is-10-and-two-is-20-imperative-statement
when 11 also 30
   one-is-11-and-two-is-30-imperative-statement
when 20 also any
   one-is-20-and-two-is-anything-imperative-statement
when other
   default-imperative-statement
end-evaluate
```



## CoffeeScript


===if-then-else===


```coffeescript

if n == 1
  console.log "one"
else if n == 2
  console.log "two"
else
  console.log "other"

```



### switch



```coffeescript
n = 1

switch n
  when 1
    console.log "one"
  when 2, 3
    console.log "two or three"
  else
    console.log "other"

```



### ternary expressions


CoffeeScript is very expression-oriented, so you can assign the "result" of an if-then to a variable.


```coffeescript
s = if condition then "yup" else "nope"

# alternate form
s = \
  if condition
  then "yup"
  else "nope"
```



## ColdFusion

===if-elseif-else===
'''Compiler:''' [[ColdFusion]] any version

```cfm><cfif x eq 3

 do something
<cfelseif x eq 4>
 do something else
<cfelse>
 do something else
</cfif>
```



### switch

'''Compiler:''' [[ColdFusion]] any version

```cfm
<cfswitch expression="#x#">
 <cfcase value="1">
  do something
 </cfcase>
 <cfcase value="2">
  do something
 </cfcase>
 <cfdefaultcase>
  do something
 </cfdefaultcase>
</cfswitch>
```



## Comal


### IF/THEN


```Comal
IF condition THEN PRINT "True"
```



### IF/THEN/ELSE


```Comal
IF condition THEN
   PRINT "True"
ELSE
   PRINT "False"
ENDIF
```



### IF/THEN/ELIF/ELSE


```Comal
IF choice=1 THEN
   PRINT "One"
ELIF choice=2 THEN
   PRINT "Two"
ELSE
   Print "None of the above"
```



### CASE/WHEN


```Comal

CASE choice OF
WHEN 1
   PRINT "One"
WHEN 2
   PRINT "Two"
OTHERWISE
   PRINT "Some other choice"
ENDCASE

```



## Common Lisp


There are 2 main conditional operators in common lisp, (if ...) and (cond ...).

===(if cond then [else])===

The (if ...) construct takes a predicate as its first argument and evaluates it.
Should the result be non-nil, it goes on to evaluate and returnm the results of the 'then' part, otherwise, when present, it evaluates and returns the result of the 'else' part. Should there be no 'else' part, it returns nil.


```lisp
(if (= val 42)
    "That is the answer to life, the universe and everything"
    "Try again") ; the else clause here is optional
```



### = <code>when</code> and <code>unless</code> =

Common Lisp also includes <code>(when condition form*)</code> and <code>(unless condition form*)</code> which are equivalent, respectively, to <code>(if condition (progn form*))</code> and <code>(if (not condition) (progn form*))</code>.

It is unidiomatic to use <code>if</code> without an else branch for side effects; <code>when</code> should be used instead.

===(cond (pred1 form1) [... (predN formN)])===

The (cond ...) construct acts as both an if..elseif...elseif...else operator and a switch, returning the result of the form associated with the first non-nil predicate.


```lisp
(cond ((= val 1)                 (print "no"))
      ((and (> val 3) (< val 6)) (print "yes"))
      ((> val 99)                (print "too far"))
      (T                         (print "no way, man!")))
```



## Computer/zero Assembly

The only conditional operation provided is <tt>BRZ</tt> (branch on accumulator negative). For an example illustrating how this instruction can be used to code "equal to", "greater than", and "less than", see [[Integer comparison#Computer/zero Assembly]].


## Crack


===if-elseif-else===


```crack
if (condition)
{
   // Some Task
}

if (condition)
{
  // Some Task
}
else if (condition2)
{
  // Some Task
}
else
{
  // Some Task
}
```



### Ternary



```crack

// if condition is true var will be set to 1, else false.
int var = condition ? 1 : 2;

```


=={{header|C sharp|C#}}==

===if-elseif-else===


```csharp
if (condition)
{
   // Some Task
}

if (condition)
{
  // Some Task
}
else if (condition2)
{
  // Some Task
}
else
{
  // Some Task
}
```



### Ternary



```csharp
// if condition is true var will be set to 1, else 2.
int var = condition ? 1 : 2;
```



### switch



```csharp
switch (value)
{
   case 1:
          // Some task
          break;  // Breaks are required in C#.
   case 2:
   case 3:
         // Some task
         break;
   default: // If no other case is matched.
         // Some task
         break;
}
```


If fall through algorithms are required use the goto keyword.


```csharp
switch (value)
{
   case 1:
          // Some task
          goto case 2; // will cause the code indicated in case 2 to be executed.
   case 2:
          // Some task
          break;
   case 3:
         // Some task
         break;
   default: // If no other case is matched.
         // Some task
         break;
}
```



## D

:''See [[Conditional Structures#C|C]], sans the preprocessor.''


```d
void main() {
    enum int i = 5;

    // "static if" for various static checks:
    static if (i == 7) {
        // ...
    } else {
        //...
    }

    // is(T == U) checks if type T is U.
    static if (is(typeof(i) == int)) {
        // ...
    } else {
        // ...
    }

    // D switch is improved over C switch:
    switch (i) {
        case 0:
            break; // Silent fallthrough is forbidden.
        case 1:
            goto case; // Explicit fallthrough.
        case 2:
            // Empty cases don't require an explicit fallthrough.
        case 3:
            return;
        case 4, 5, 7: // Multiple cases.
            break;
        case 8: .. case 15: // Inclusive interval.
            goto case 3;
        default: // Default case is required.
            break;
    }

    enum Colors { yellow, blue, brown, green }
    immutable c = Colors.blue;

    // "final switch" is safer, for enums (and in future other values,
    // like Algebraic), because all cases must be present.
    // with() is handy to avoid repeating "Colors." for each case.
    final switch (c) with (Colors) {
        case yellow:        break;
        case blue:          break;
        case brown, green:  break;
        // case yellow: .. case brown: // Forbidden in final switches.
        // default: // Forbidden in final switches.
    }
}
```



## Dao


### If Elif Else


```java
a = 3
if( a == 1 ){
    io.writeln( 'a == 1' )
}else if( a== 3 ){
    io.writeln( 'a == 3' )
}else{
    io.writeln( 'a is neither 1 nor 3' )
}
```



### Switch Case


```java
a = 3
switch( a ){
case 0: io.writeln( 'case 0' )
case 1, 2: io.writeln( 'case 1,2' )
case 3, 4, 5: io.writeln( 'case 3,4,5' )
default: io.writeln( 'default' )
}
```


=={{header|Déjà Vu}}==

```dejavu
if a:
    pass
elseif b:
    pass
else: # c, maybe?
    pass
```



## Deluge



```deluge
if (input.Field == "Hello World") {
    sVar = "good";
} else if (input.Field == "Bye World") {
    sVar = "bad";
} else {
    sVar = "neutral";
}
```



## Delphi


:''See [[#Pascal|Pascal]]''


## DM

===if-elseif-else===

```DM
if (condition)
    // Do thing, DM uses indentation for control flow.

if (condition)
    // Do thing

else if (condition)
    // Do thing

else
    // Do thing

```



### Ternary


```DM
// x will be 1 if condition is a true value, 2 otherwise.
var/x = condition ? 1 : 2

```



### Switch


```DM
switch (value)
    if (0)
        // Do thing if zero
        // DM does not have fall through of switch cases, so explicit break is not required.
    if (1, 2, 3)
        // Multiple values can be allowed by using commas

    if (10 to 20)
        // Ranges are also allowed.
        // Ranges include the bounds (10 and 20 here),
        // and are checked in order if there is potential for overlap.

    else
        // Fallback if nothing was matched.

```



## Dragon

===if-then-else===

```dragon
if(a == b)
{
    add()
}
else if(a == c)
    less() //{}'s optional for one-liners
else
{
    both()
}
```



## DWScript


:''See [[Conditional Structures#Pascal|Pascal]]''


## E


===if-then-else===


```e
if (okay) {
    println("okay")
} else if (!okay) {
    println("not okay")
} else {
    println("not my day")
}
```


The pick/2 message of booleans provides a value-based conditional:


```e
println(okay.pick("okay", "not okay"))
```


It can therefore be used to construct a Smalltalk-style conditional:


```e
okay.pick(fn {
    println("okay")
}, fn {
    println("not okay")
})()
```


All of the above conditionals are expressions and have a usable return value.


### switch


E's "switch" allows pattern matching.


```e
def expression := ["+", [1, 2]]

def value := switch (expression) {
    match [`+`, [a, b]] { a + b }
    match [`*`, [a, b]] { a * b }
    match [op, _] { throw(`unknown operator: $op`) }
}
```



## EasyLang

<lang>i = random 10
if i mod 2 = 0
  print i & " is divisible by 2"
elif i mod 3 = 0
  print i & " is divisible by 3"
else
  print i & " is not divisble by 2 or 3"
.
```



## Efene


The expressions can contain parenthesis or not, here both options are shown.
Since if and case do pattern matching, if an if or case expression don't match some of the patterns, the program will crash


```efene

show_if_with_parenthesis = fn (Num) {
    if (Num == 1) {
        io.format("is one~n")
    }
    else if (Num === 2) {
        io.format("is two~n")
    }
    else {
        io.format("not one not two~n")
    }
}

show_if_without_parenthesis = fn (Num) {
    if Num == 1 {
        io.format("is one~n")
    }
    else if Num === 2 {
        io.format("is two~n")
    }
    else {
        io.format("not one not two~n")
    }
}

show_switch_with_parenthesis = fn (Num) {
    switch (Num) {
        case (1) {
            io.format("one!~n")
        }
        case (2) {
            io.format("two!~n")
        }
        else {
            io.format("else~n")
        }
    }
}

show_switch_without_parenthesis = fn (Num) {
    switch (Num) {
        case 1 {
            io.format("one!~n")
        }
        case 2 {
            io.format("two!~n")
        }
        else {
            io.format("else~n")
        }
    }
}

@public
run = fn () {
    show_if_with_parenthesis(random.uniform(3))
    show_if_without_parenthesis(random.uniform(3))

    show_switch_with_parenthesis(random.uniform(3))
    show_switch_without_parenthesis(random.uniform(3))
}
```



## Ela


===if-then-else===


```ela>if x < 0 then 0 else x</lang



### Guards



```ela
getX x | x < 0 = 0
       | else  = x
```



### Pattern matching



```ela
force (x::xs) = x :: force xs
force [] = []
```



### match expression



```ela
force lst = match lst with
                  x::xs = x :: force xs
                  [] = []
```



## Erlang


Erlang's conditionals are based on pattern matching and guards.
There are several mechanisms for this: case-of, if, function clauses.
Pattern matching allows destructuring a term and matches a clause based on the structure.
In the case example the term is X and the pattern is {N,M} or _. _ will match anything, while {N,M} will only match tuples of two terms.
Though N and M could be any other type (in this case an error will occur if they're non-numeric).
Guards allow more specification on the terms from the matched pattern. In the case example comparing N and M are guards.


### case


case expressions take an expression and match it to a pattern with optional guards.


```erlang
case X of
  {N,M} when N > M -> M;
  {N,M} when N < M -> N;
  _ -> equal
end.
```



### if


if expressions match against guards only, without pattern matching.
Guards must evaluate to true or false so true is the catch-all clause.


```erlang
{N,M} = X,
if
  N > M -> M;
  N < M -> N;
  true -> equal
end.
```



### Function Clauses


Functions can have multiple clauses tested in order.


```erlang
test({N,M}) when N > M -> M;
test({N,M}) when N < M -> N;
test(_) -> equal.
```



## Factor

There are many conditional structures in Factor.
Here I'll demonstrate the most common ones.
A few of these have other variations that abstract common stack shuffle patterns.
I will not be demonstrating them.


### ?

? is for when you don't need branching, but only need to select between two different values.

```factor

t 1 2 ? ! returns 1

```



### if


```factor
t [ 1 ] [ 2 ] if ! returns 1
```



### cond


```factor
{ { [ t ] [ 1 ] } { [ f ] [ 2 ] } } cond ! returns 1
```



### case


```factor
t { { t [ 1 ] } { f [ 2 ] } } case ! returns 1
```



### when


```factor
t [ "1" print ] when ! prints 1
```



### unless


```factor
f [ "1" print ] unless ! prints 1
```



## FALSE


```false
condition[body]?
```

Because there is no "else", you need to stash the condition if you want the same effect:

```false
$[\true\]?~[false]?
```

or

```false
$[%true0~]?~[false]?
```



## Fancy

Fancy has no built-in conditional structures. It uses a combination of polymorphism and blockliterals (closures) to achieve the same thing (like Smalltalk).


### if:then:


```fancy
if: (x < y) then: {
  "x < y!" println # will only execute this block if x < y
}

```



### if:then:else::


```fancy
if: (x < y) then: {
  "x < y!" println # will only execute this block if x < y
} else: {
  "x not < y!" println
}

```



===if_true:===

```fancy
x < y if_true: {
  "x < y!" println # will only execute this block if x < y
}

```


===if_false: / if_nil:===

```fancy
x < y if_false: {
  "x not < y!" println # will only execute this block if x >= y
}

```


===if_true:else:===

```fancy
x < y if_true: {
  "x < y!" println
} else: {
  "x >= y!" println
}

```


===if_false:else:===

```fancy
x < y if_false: {
  "x >= y!"
} else: {
  "x < y!" println
}

```



### if:


```fancy
{ "x < y!" println } if: (x < y)   # analog, but postfix
```




### unless:


```fancy
{ "x not < y!" } unless: (x < y)   # same here
```



## Forth

===IF-ELSE===

```forth
( condition ) IF ( true statements ) THEN
( condition ) IF ( true statements ) ELSE ( false statements ) THEN
```

example:

```forth
10 < IF ." Less than 10" ELSE ." Greater than or equal to 10" THEN
```


===CASE-OF===

```forth
( n -- ) CASE
( integer ) OF ( statements ) ENDOF
( integer ) OF ( statements ) ENDOF
( default instructions )
ENDCASE
```

example: a simple CASE selection

```forth
: test-case ( n -- )
   CASE
     0 OF ." Zero!" ENDOF
     1 OF ." One!"  ENDOF
     ." Some other number!"
   ENDCASE ;
```



### Execution vector

To obtain the efficiency of a C switch statement for enumerations, one needs to construct one's own execution vector.

```forth
: switch
  CREATE ( default-xt [count-xts] count -- ) DUP , 0 DO , LOOP ,
  DOES> ( u -- ) TUCK @ MIN 1+ CELLS + @ EXECUTE ;

  :NONAME ." Out of range!" ;
  :NONAME ." nine" ;
  :NONAME ." eight" ;
  :NONAME ." seven" ;
  :NONAME ." six" ;
  :NONAME ." five" ;
  :NONAME ." four" ;
  :NONAME ." three" ;
  :NONAME ." two" ;
  :NONAME ." one" ;
  :NONAME ." zero" ;
10 switch digit

 8 digit   \ eight
34 digit   \ Out of range!
```


### Execution Vector 2

This method was used by the late Jim Kalihan and Dr. Julian Nobel

```Forth
: CASE:  ( <name>)   CREATE   ;

\ lookup execution token and compile
: |      ( <name> )  '  compile,  ;

: ;CASE   ( n -- )  DOES>  OVER + + @ EXECUTE ;

 : FOO   ." FOO" ;
 : BAR   ." BAR" ;
 : FIZZ  ." FIZZ" ;
 : BUZZ  ." BUZZ" ;

CASE: SELECT ( n -- ) | FOO  | BAR | FIZZ | BUZZ  ;CASE

\ Usage:  3 SELECT
</LANG>


## Fortran

In ISO Fortran 90 and later, there are three conditional structures. There are also a number of other *unstructured* conditional statements, all of which are old and many of which are marked as "deprecated" in modern Fortran standards. These examples will, as requested, only cover conditional *structures*:
===IF-THEN-ELSE===
ANSI FORTRAN 77 or later has an IF-THEN-ELSE structure:

```fortran
if ( a .gt. 20.0 ) then
   q = q + a**2
else if ( a .ge. 0.0 ) then
   q = q + 2*a**3
else
   q = q - a
end if
```


===SELECT-CASE===
ISO Fortran 90 or later has a SELECT-CASE structure:

```fortran
select case (i)
   case (21:)      ! matches all integers greater than 20
      q = q + i**2
   case (0:20)     ! matches all integers between 0 and 20 (inclusive)
      q = q + 2*i**3
   case default    ! matches all other integers (negative in this particular case)
      q = q - I
end select
```


===WHERE-ELSEWHERE===
ISO Fortran 90 and later has a concurrent, array-expression-based WHERE-ELSEWHERE structure. The logical expressions in WHERE and ELSEWHERE clauses must be array-values. All statements inside the structure blocks must be array-valued. Furthermore, all array-valued expressions and statements must have the same "shape". That is, they must have the same number of dimensions, and each expression/statement must have the same sizes in corresponding dimensions as each other expression/statement. For each block, wherever the logical expression is true, the corresponding elements of the array expressions/statements are evaluated/executed.

```fortran
! diffusion grid time step
where     (edge_type(1:n,1:m) == center)
   anew(1:n,1:m) = (a(1:n,1:m) + a(0:n-1,1:m) + a(2:n+1,1:m) + a(1:n,0:m-1) + a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == left)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(2:n+1,1:m) + a(1:n,0:m-1) + a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == right)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(0:n-1,1:m) + a(1:n,0:m-1) + a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == top)
   anew(1:n,1:m) = (a(1:n,1:m) + a(0:n-1,1:m) + a(2:n+1,1:m) + 2*a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == bottom)
   anew(1:n,1:m) = (a(1:n,1:m) + a(0:n-1,1:m) + a(2:n+1,1:m) + 2*a(1:n,0:m-1)) / 5

elsewhere (edge_type(1:n,1:m) == left_top)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(2:n+1,1:m) + 2*a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == right_top)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(0:n-1,1:m) + 2*a(1:n,2:m+1)) / 5

elsewhere (edge_type(1:n,1:m) == left_bottom)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(2:n+1,1:m) + 2*a(1:n,0:m-1)) / 5

elsewhere (edge_type(1:n,1:m) == right_bottom)
   anew(1:n,1:m) = (a(1:n,1:m) + 2*a(0:n-1,1:m) + 2*a(1:n,0:m-1)) / 5

elsewhere      ! sink/source, does not change
   anew(1:n,1:m) = a(1:n,1:m)
end where
```



## FreeBASIC

===IF-ELSEIF-ELSE-END IF===

```freebasic
Dim a As Integer = 1
If a = 1 Then
  sub1
ElseIf a = 2 Then
  sub2
Else
  sub3
End If
```


===SELECT-CASE===

```freebasic
Dim a As Integer = 1
Select Case a
  Case 1
    sub1
  Case 2
    sub2
  Case Else
    sub3
End Select
```



### IFF


```freebasic
Dim b As Boolean = True
Dim i As Integer = IIf(b, 1, 2)
```


===ON-GOTO===

```freebasic
Dim a As Integer = 1
On a Goto label1, label2
```


===IF-GOTO (deprecated)===

```freebasic
Dim b As Boolean = True
If b Goto label
```


===ON-GOSUB (legacy dialects only)===

```freebasic
Dim a As Integer = 1
On a Gosub label1, label2
```


===#IF-#ELSEIF-#ELSE-#ENDIF (preprocessor)===

```freebasic
#DEFINE WORDSIZE 16
#IF (WORDSIZE = 16)
  ' Do some some 16 bit stuff
#ELSEIF (WORDSIZE = 32)
  ' Do some some 32 bit stuff
#ELSE
  #ERROR WORDSIZE must be set to 16 or 32
#ENDIF
```


===#IFDEF (preprocessor)===

```freebasic
#DEFINE _DEBUG
#IFDEF _DEBUG
  ' Special statements for debugging
#ENDIF
```


===#IFNDEF (preprocessor)===

```freebasic
#IFNDEF _DEBUG
  #DEFINE _DEBUG
#ENDIF
```



## friendly interactive shell

===if-then-else===

```fishshell
set var 'Hello World'
if test $var = 'Hello World'
    echo 'Welcome.'
else if test $var = 'Bye World'
    echo 'Bye.'
else
    echo 'Huh?'
end
```



### switch

case statements take wildcards as arguments, but because of syntax quirk, they have to be quoted (just like in Powershell), otherwise they would match files in current directory. Unlike switch statements in C, they don't fall through. To match something that would be matched if nothing was matches use wildcard that matches everything, the language doesn't have default statement.

```fishshell
switch actually
    case az
        echo The word is "az".
    case 'a*z'
        echo Begins with a and ends with z.
    case 'a*'
        echo Begins with a.
    case 'z*'
        echo Ends with z.
    case '*'
        echo Neither begins with a or ends with z.
end
```



## Futhark

=== if-then-else ===

Futhark supports branching with a syntax common to most functional languages.


```futhark

if <condition> then <truebranch> else <falsebranch>

```



## GAP

=== if-then-else ===

```gap>if <condition
 then
    <statements>
elif <condition> then
    <statements>
else
    <statements>
fi;
```



## Go

If and switch are the general purpose conditional structures in Go, although the language certainly contains other conditional elements.

### If

Simplest usage is,

```go
if booleanExpression {
    statements
}
```

The braces are required, even around a single statement.

```go
if booleanExpression {
    statements
} else {
    other
    statements
}
```

Braces are required around else clauses, as above, unless the statement of the else clause is another if statement.  In this case the statements are chained like this,

```go
if booleanExpression1 {
    statements
} else if booleanExpression2 {
    otherStatements
}

```

If allows a statement to be included ahead of the condition.  This is commonly a short variable declaration, as in,

```go
if x := fetchSomething(); x > 0 {
    DoPos(x)
} else {
    DoNeg(x)
}
```

In this case the scope of x is limited to if statement.


### Switch

Simple usage is,

```go
switch {
case booleanExpression1:
    statements
case booleanExpression2:
    other
    statements
default:
    last
    resort
    statements
}
```

Because switch can work with any number of arbitrary boolean expressions, it replaces if/elseif chains often found in other programming languages.

Switch can also switch on the value of an expression, as in,

```go
switch expressionOfAnyType {
case value1:
    statements
case value2, value3, value4:
    other
    statements
}
```

As shown, multiple values can be listed for a single case clause.
Since go is statically typed, the types of value1, 2, 3, and 4 must match the type of the expression.

As with if, a local statement such as a short variable declaration can precede the expression.  If there is no expression, the statement is still marked by a semicolon:

```go
switch x := fetch(); {
case x == "cheese":
    statements
case otherBooleanExpression:
    other
    statements
}
```

Also, as with if, the scope of x is limited to the switch statement.

Execution does not normally fall through from one case clause to the next, but this behavior can be forced with a fallthrough statement.

An interesting example:

```go
switch {
case booleanExpression1:
default:
    statements
    preliminaryToOtherStatements
    fallthrough
case booleanExpression2:
    other
    statements
}
```

Case expressions are evaluated in order, then if none are true, the default clause is executed.

Another statement that interacts with switch is break.  It breaks from the switch statement and so will not break from a surrounding for statement.  The following example prints "I want out!" endlessly.

```go
for {
    switch {
    case true:
        break
    }
    fmt.Println("I want out!")
}
```

Labels provide the desired capability.  The following prints "I'm off!"

```go
treadmill: for {
    switch {
    case true:
        break treadmill
    }
}
fmt.Println("I'm off!")
```



## Harbour

'''if-elseif-else-endif'''

```visualfoxpro
IF x == 1
   SomeFunc1()
ELSEIF x == 2
   SomeFunc2()
ELSE
   SomeFunc()
ENDIF
```


'''do case'''

```visualfoxpro
DO CASE
CASE x == 1
   SomeFunc1()
CASE x == 2
   SomeFunc2()
OTHERWISE
   SomeFunc()
ENDCASE
```


'''switch'''
While '''if-elseif-else-endif''' and '''do case''' constructions allows using of any expressions as conditions, the '''switch''' allows literals only in conditional '''case''' statements. The advantage of the '''switch''' command is that it is much faster.

```visualfoxpro
SWITCH x
CASE 1
   SomeFunc1()
   EXIT
CASE 2
   SomeFunc2()
   EXIT
OTHERWISE
   SomeFunc()
ENDSWITCH
```



## Haskell


===if-then-else===

```haskell
fac x = if x==0 then
           1
           else x * fac (x - 1)
```



### Guards


```haskell
fac x | x==0 = 1
      | x>0 = x * fac (x-1)
```


### Pattern matching


```haskell
fac 0 = 1
fac x = x * fac (x-1)
```


### case statement


```haskell
fac x = case x of 0 -> 1
                  _ -> x * fac (x-1)
```



## HicEst


```hicest
IF( a > 5 ) WRITE(Messagebox) a ! single line IF

IF( a >= b ) THEN
    WRITE(Text=some_string) a, b
  ELSEIF(some_string > "?") THEN
    WRITE(ClipBoard) some_string
  ELSEIF( nonzero ) THEN
    WRITE(WINdowhandle=nnn) some_string
  ELSE
    WRITE(StatusBar) a, b, some_string
ENDIF
```



## HPPPL


###  IF

Note that X has to be a number; else a runtime error occurs.

```HPPPL
IF X THEN
	// do if X is not 0
ELSE
	// do if X is 0
END;
```


###  CASE


```HPPPL
CASE
	IF X == 1 THEN
		// do stuff if X equals 1
	END
	IF X == 2 THEN
		// do stuff if X equals 1
	END
	IF X == 3 THEN
		// do stuff if X equals 3
	END
	DEFAULT
		// do other stuff
END;
```



## i


```i
//'i' supports if, else, and else if
software {
	a = 3

	if a = 3
		print("a = three")
	else if a = 2
		print("a = two")
	else
		print("a = ", a)
	end
}
```



## IDL


===if-else===

Basic if/then:


```idl
if a eq 5 then print, "a equals five" [else print, "a is something else"]
```


Any one statement (like these print statements) can always be expanded into a {begin ... end} pair with any amount of code in between. Thus the above will expand like this:


```idl
if a eq 5 then begin
  ... some code here ...
endif [else begin
  ... some other code here ...
endelse]
```



### case



```idl>case <expression
 of
  (choice-1): <command-1>
  [(choice-2): <command-2> [...]]
  [else: <command-else>]
endcase
```


(Or replace any of the commands with {begin..end} pairs)


### switch



```idl>switch <expression
 of
  (choice-1): <command-1>
  [(choice-2): <command-2> [...]]
  [else: <command-else>]
endswitch
```


The <tt>switch</tt> will execute all commands starting with the matching result, while the <tt>case</tt> will only execute the matching one.

===on_error===


```idl
on_error label
```


Will resume execution at label when an error is encountered. <tt>on_ioerror</tt> is similar but for IO errors.

=={{header|Icon}} and {{header|Unicon}}==
All Icon and Unicon expressions, including control structures, yield results or signal failure.
===if-then-else===
The control structure evaluates expr1 if expr0 succeeds and expr2 if it fails.

```Icon
if expr0 then
   expr1
else
   expr2
```

===case-of===
The first successful selection expression will select and evaluate the specific case.

```Icon
case expr0 of {
   expr1 : expr2
   expr3 : expr4
   default: expr5
   }
```

Note that expr1 and expr3 are expressions and not constants and it is possible to write expressions such as:

```Icon
case x of {
   f(x) | g(x) : expr2
   s(x) & t(x) : expr4
   default: expr5
   }
```

===Compound expressions (blocks)===
In the examples below, multiple expressions can be grouped as in:

```Icon
{
   expr1
   expr2
   expr3
}
```

Which is equivalent to this:

```Icon
{expr1; expr2; expr3}
```

For example the following, which will write 4, looks strange but is valid:

```Icon
write({1;2;3;4})
```

The value of a compound expression is the value of the last expression in the block.

### Alternation

Alternation of expressions yields a value for the first succeeding expression.

```Icon
   expr1 | expr2 | expr3
```


### Conjunction

Conjunctions yeild the value of the final expression provided all the previous expressions succeed.

```Icon
   expr1 & expr2 & expr3
```

Alternately, conjunction can be written thus:

```Icon
   (expr1, expr2, expr3)
```

===Conjunction, yielding a different result===
The alternate form of conjunction can be modified to produce a different result (other than the last)

```Icon
   expr0(expr1, expr2, expr3)
```

For example:

```Icon
   2(expr1, expr2, expr3)
```

Yields the value of expr2 if all of the expressions succeed.

A more complicated example showing non-constant expressions:

```Icon
   f(expr1)(g(expr2)(expr3,expr4,expr5))
```

Note: if expr0 yields a value of type 'procedure' or 'string' the appropriate procedure (or operator) is invoked.


## Inform 7

===if-then-else===

```inform7
[short form]
if N is 1, say "one.";
otherwise say "not one.";

[block form]
if N is 1:
	say "one.";
otherwise if N is 2:
	say "two.";
otherwise:
	say "not one or two.";

[short and long forms can be negated with "unless"]
unless N is 1, say "not one."
```



### switch


```inform7
if N is:
	-- 1: say "one.";
	-- 2: say "two.";
	-- otherwise: say "not one or two.";
```


===if-then-else in text===

```inform7
say "[if N is 1]one[otherwise if N is 2]two[otherwise]three[end if].";
say "[unless N is odd]even.[end if]";
```



### other branching text substitutions

Text that may be printed multiple times can also use sequential and random branching:

```inform7
[a different color every time]
say "[one of]red[or]blue[or]green[at random].";

["one" the first time it's printed, "two" the second time, then "three or more" subsequently]
say "[one of]one[or]two[or]three or more[stopping]";

[only appears once]
say "[first time]Hello world![only]";
```



### rulebook approach

Conditional logic may also be expressed in the form of a rulebook, with conditions on each rule:


```inform7
Number Factory is a room.

Number handling is a number based rulebook with default success.

Number handling for 1: say "one."
Number handling for 2: say "two."
Number handling for an even number (called N): say "[N in words] (which is even)."
Last number handling rule: say "other."

When play begins:
	follow the number handling rules for 2;
	follow the number handling rules for 4;
	follow the number handling rules for 5.
```



## J

See [[Conditional Structures/J]]


## Java

===if-then-else===

```java
if(s.equals("Hello World"))
{
    foo();
}
else if(s.equals("Bye World"))
    bar();//{}'s optional for one-liners
else
{
    deusEx();
}
```

Java also supports [[wp:Short-circuit_evaluation|short-circuit evaluation]]. So in a conditional like this:

```java
if(obj != null && obj.foo()){
   aMethod();
}
```

<tt>obj.foo()</tt> will not be executed if <tt>obj != null</tt> returns false. It is possible to have conditionals without short circuit evaluation using the <tt>&</tt> and <tt>|</tt> operators (from [[Bitwise operations]]). So in this conditional:

```java
if(obj != null & obj.foo()){
   aMethod();
}
```

You will get a null pointer exception if obj is null.

### ternary



```java
s.equals("Hello World") ? foo() : bar();
```



### switch

This structure will only work if the code being switched on evaluates to an integer or character. There is no switching on Objects or floating-point types in Java (except for <code>String</code>s in Java 7 and higher).

```java
switch(c) {
case 'a':
   foo();
   break;
case 'b':
   bar();
default:
   foobar();
}
```

This particular example can show the "fallthrough" behavior of a switch statement. If c is the character b, then bar() and foobar() will both be called. If c is the character a, only foo() will be called because of the break statement at the end of that case.

Also, the switch statement can be easily translated into an if-else if-else statement. The example above is equivalent to:

```java
if(c == 'a'){
   foo();
}else if(c == 'b'){
   bar();
   foobar();
}else{
   foobar();
}
```

Cases without breaks at the end require duplication of code for all cases underneath them until a break is found (like the else if block shown here).


## JavaScript


===if-then-else===


```javascript
if( s == "Hello World" ) {
    foo();
} else if( s == "Bye World" ) {
    bar();
} else {
    deusEx();
}
```



### switch



```javascript
switch(object) {
    case 1:
        one();
        break;
    case 2:
    case 3:
    case 4:
        twoThreeOrFour();
        break;
    case 5:
        five();
        break;
    default:
        everythingElse();
}
```


===conditional (ternary) operator (?:)===


```javascript
var num = window.obj ? obj.getNumber() : null;
```


The distinctive feature of the ternary operator (compared to JavaScript's other conditional structures) is that it evaluates as an expression rather than a statement, and can therefore be composed within larger expressions, making it a valuable resource of program structure in a functional idiom of JavaScript.


```JavaScript
function takeWhile(lst, fnTest) {
    'use strict';
    var varHead = lst.length ? lst[0] : null;

    return varHead ? (
        fnTest(varHead) ? [varHead].concat(
            takeWhile(lst.slice(1), fnTest)
        ) : []
    ) : [];
}
```



## JCL

At the origin, JCL has a tricky conditional statement: the COND parameter. It is an inverted condition of an execution bypass of a step.

Step return code values are traditionnaly ordered this way:

```txt

0  :  ok
4  :  warning
8  :  error
12 :  severe error
16 :  terminal error

```

The syntax of COND parameter of the EXEC statement is :

```jcl
  COND=(rcval,relop,step)
    relop is a relational opeator : EQ NE GT LT GE LE   (= ¬= < > <= >=)
```

It is a condition to bypass a job step, and it can be translateted into :

```jcl>  if rcval relop step.rc then not execute the current step </lang

Example:

```jcl
//STEP6  EXEC PGM=MYPROG,COND=(0,NE,STEP3)
```


```txt

if 0 ne STEP3.rc  then skip    step STEP6
if 0 ^= STEP3.rc  then skip    step STEP6
if STEP3.rc ^= 0  then skip    step STEP6
if STEP3.rc = 0   then execute step STEP6

```

The conditions can be multiple :

```jcl
  COND=((rcval1,relop1,step1),(rcval2,relop2,step2),...)
```

Means:

```jcl>  if rcval1 relop1 step1.rc or rcval2 relop2 step2.rc or ... then not execute the current step </lang

Example:

```jcl
//STEP6  EXEC PGM=MYPROG,COND=((4,LE,STEP1),(8,LE,STEP3))
```


```txt

if  4 le STEP1.rc   or  8 le STEP3.rc  then skip step STEP6
if  4 <= STEP1.rc   or  8 <= STEP3.rc  then skip step STEP6
if  STEP1.rc >= 4   or  STEP3.rc >= 8  then skip step STEP6
if  STEP1 is ok    and  STEP3 has only warnings then execute STEP6

```




## jq

jq's main conditional construct is:
```jq>if cond then f else g end</lang
where cond, f, and g, are filters, and where cond may evaluate to anything at all, it being understood that:
# all JSON values are truthy except for false and null;
# if cond evaluates to nothing (i.e., produces an empty stream), then the entire if-then-else-end expression also produces an empty stream.

The general pattern allows one or more "elif _ then _" clauses:

```jq

if cond then f elif cond1 then f1 .... else g end

```


For example:
```jq

if empty then 2 else 3 end          # produces no value
if 1 then 2 else 3 end              # produces 2
if [false, false] then 2 else 3 end # produces 2
if (true, true) then 2 else 3 end   # produces a stream: 2, 2

```
Notice that if cond produces a nonempty stream, then the entire expression will typically do the same.  Since f and g also can produce streams, this lends itself to interesting Cartesian-product possibilities.

There is no "case <exp>" construct, but the idiom illustrated by the following example can be used to avoid the need to create a temporary variable to hold the "case" expression:
```jq

exp
 | if   . == true then "true"
   elif . == false then "false"
   elif . == null  then "maybe"
   elif type == "string" then .
   else error("unexpected value: \(.)")
   end
```

Since jq's <tt>and</tt> and <tt>or</tt> are short-circuiting, they can also be used for branching, as can the binary disjunctive operator `//`.


## Julia

Note: this documentation is mostly copied from the Julia 0.6.0 documentation at: https://docs.julialang.org/en/stable/manual/control-flow/#man-conditional-evaluation-1
<h3>Conditional Evaluation</h3>

```Julia

function test(x, y)
    if x < y
        println("x is less than y")
    elseif x > y
        println("x is greater than y")
    else
        println("x is equal to y")
    end
end

julia> test(1, 2)
x is less than y

julia> test(2, 1)
x is greater than y

julia> test(1, 1)
x is equal to y

```

<p>
The elseif and else blocks are optional, and as many elseif blocks as desired can be used. The condition expressions in the if-elseif-else construct are evaluated until the first one evaluates to true, after which the associated block is evaluated, and no further condition expressions or blocks are evaluated.
</p><p>
The so-called "ternary operator", ?:, is closely related to the if-elseif-else syntax, but is used where a conditional choice between single expression values is required, as opposed to conditional execution of longer blocks of code. It gets its name from being the only operator in most languages taking three operands:
</p>
```Julia

a ? b : c

```

<p>
The expression a, before the ?, is a condition expression, and the ternary operation evaluates the expression b, before the :, if the condition a is true or the expression c, after the :, if it is false. To facilitate chaining, the operator associates from right to left.
</p><h3>
Short-Circuit Evaluation
</h3><p>
Short-circuit evaluation is quite similar to conditional evaluation. The behavior is found in most imperative programming languages having the && and || boolean operators: in a series of boolean expressions connected by these operators, only the minimum number of expressions are evaluated as are necessary to determine the final boolean value of the entire chain. Explicitly, this means that:
</p><p>
In the expression <code>a && b</code>, the subexpression b is only evaluated if a evaluates to true.
</p><p>
In the expression <code>a || b</code>, the subexpression b is only evaluated if a evaluates to false.
</p>


## Kabap

Kabap supports the '''if''' statement and a range of standard comparators.  Truthiness is considered anything not "0" or 0.

```Kabap

if 1;
  $result = "Execute";

if 0;
  $result = "Ignored";

if 1; {
  $result = "Block";
  $result = "Execute";
}

if 0; {
  $result = "Block";
  $result = "Ignored";
}

if 1 == 1;
  $result = "Execute";

if 1 < 2;
  $result = "Execute";

if 1 <= 1;
  $result = "Execute";

if 2 > 1;
  $result = "Execute";

if 1 >= 1;
  $result = "Execute";

if 1 != 2;
  $result = "Execute";

$a = 1;
if $a == 1;
  $result = "Execute";

if $a == kabap.version;
  $result = "Execute";

if 1 == "1";
  $result = "Execute";

if 1 + 1 == 2;
  $result = "Execute";

if 1;
  if 1; {
    if 1;
      if 1; {
        $result = "Execute";
      }
  }


```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    // conventional 'if/else if/else' statement
    if (args.isEmpty()) println("No arguments were supplied")
    else if (args.size == 1) println("One argument was supplied")
    else println("${args.size} arguments were supplied")

    print("Enter an integer : ")
    val i = readLine()!!.toInt()

    // 'when' statement (similar to 'switch' in C family languages)
    when (i) {
        0, 1      -> println("0 or 1")
        in 2 .. 9 -> println("Between 2 and 9")
        else      -> println("Out of range")
    }

    // both of these can be used as expressions as well as statements
    val s = if (i < 0) "negative" else "non-negative"
    println("$i is $s")
    val t = when {
        i > 0  -> "positive"
        i == 0 -> "zero"
        else   -> "negative"
    }
    println("$i is $t")
}
```

Sample input/output (program invoked without arguments):
{{out}}

```txt

No arguments were supplied
Enter an integer : 3
Between 2 and 9
3 is non-negative
3 is positive

```



## LabVIEW


### Case Structure

{{VI snippet}}<br/>
[[File:LabVIEW_Case.png]]

### Select

Select is similar to the Ternary operator in text-based languages.<br/>
[[File:LabVIEW_Select.png]]


## Langur

If and given expressions always produce a value, even if it's nothing (null).

In the shortened forms, you dispense with the keywords (except the first one).


### if/else if/else

If expressions are scoped per section.

```Langur
# using the fact that submatch() returns an empty array for no match and that a decoupling assignment returns a Boolean...

if val (.alias, .name) = submatch($re/^(\.idregex;)\\s*;\\s*(\.idregex;)/, .row) {
    # success (2 or more values in array returned from submatch function)
    # use .alias and .name here
    ...
} else if .x > 0 {
    val .y = 100
    ...
} else {
    val .y = 70
    ...
}
```



### shortened form if expression

The shortened form expects a single expression per test.

```Langur
if(.x > .y: ...; .x < .y: ...; /* else */ ...)
```



### given

Given expressions are highly flexible. See langurlang.org for details.

Given expressions are scoped per block. Also, test expressions (the .x, .y, .z list below) may contain declarations which are scoped to the entire expression. Conditions (case statements) may not contain declarations.

```Langur
given .x, .y, .z {
    case true: ...                      # all are true
    case false, _: ...                  # .x == false
    case _, null, true: ...             # .y == null and .z == true
}
```



```Langur
given .x, .y, != .z {
    case 7 <, 14, 21: ...                  # 7 < .x and .y == 14 and .z != 21
    case or > 100, _, re/abc/: ...         # .x > 100 or matching(re/abc/, .z)
    case ; .a > .b: ...                    # .a > .b
    case _, _, == 7; xor .a <= .b: ...     # .z == 7 xor .a <= .b
    default: ...
}
```



### implicit fallthrough

If a block of a given has any statements, there is no implicit fallthrough. A case with an empty block after it creates an implicit fallthrough.

```Langur
given .x {
    case true:     # implicit fallthrough
    case null: 0   # no fallthrough
    default: 1
}
```



### fallthrough from anywhere

A fallthrough statement is allowed anywhere within a given block, not just at the end.

```Langur
given .x {
    case true:
        if .y > 100 {
            fallthrough
        } else {
            120
        }
    case false: ...
}
```



### shortened form given

The shortened form expects a single expression per test and is also more limited (no explicit fallthrough, no alternate test expressions, no alternate logical operators).

```Langur
given(.x, .y, .z; true: /* all true */ ... ; _, >= .z: /* .y >= .z */ ...; /* default */ ... )
```



## LC3 Assembly

The LC3 sets condition codes (N[egative], Z[ero], and/or P[ositive]) based on the results of instructions that write values into the general purpose registers. The BR instruction utilizes these condition codes are to branch conditionally. If the BR instruction specifies one or more condition codes and at least one specified code is set, then the PC will be updated to the branch address. If none of the specified condition codes is set, then the next sequential instruction will execute. If the BR instruction does not specify any condition codes, then it is an unconditional branch, so the branch will be taken.

```lc3asm
BR or BRnzp    ; unconditional branch, i.e.
               ; branch if (result < 0 || result == 0 || result > 0)
               ; ^ this is always true

BRn            ; branch if (result < 0)
BRz            ; branch if (result == 0)
BRp            ; branch if (result > 0)

               ; or any combination of these condition codes, e.g.
BRnz           ; branch if (result <= 0)
```

The effect of <tt>if (x == y) { go to LABEL }</tt> is achieved by adding <tt>x</tt> to <tt>-y</tt> (the two's complements of <tt>y</tt>) and branching if the result is zero. The following example prints "Branch Taken!" because the values of <tt>x</tt> and <tt>y</tt> are both 1.

```lc3asm
.orig x3000
LD R1, x        ; get x
LD R2, y        ; get y
NOT R0, R2      ; R0 = ~y
ADD R0, R0, 1	; R0 = -y
ADD R0, R0, R1	; R0 = x - y
BRZ BRANCH      ; if (x == y) { go to BRANCH }
LEA R0, nottaken
PUTS            ; else print "Branch Not Taken!"
BR END
BRANCH
LEA R0, taken
PUTS            ; print "Branch Taken!"
END HALT
x .fill 1
y .fill 1
taken .stringz "Branch Taken!"
nottaken .stringz "Branch Not Taken!"
.end
```



## LIL

LIL sports '''if''' with an optional code block for else conditions.


```tcl
if {$a > 10} {print "code evaluated on true"}
if {$a > 10} {print "again"} {print "code evaluated on false"}
```


These can of course be nested in clear or nasty forms.  '''if''' blocks can contain '''if''' blocks with as many optional else clauses as a programmer sees fit to intermingle.

It might be argued that out of band '''catcher''' code blocks could be seen as conditional ''valid command'' branching?  It would be a feature and definition fairly unique to LIL and the Tcl likes.


## Lisaac

{{works with|Lisaac|0.13.1}}
===if-then-else===

```Lisaac
+ n : INTEGER;

n := 3;

(n = 2).if {
  IO.put_string "n is 2\n";
}.elseif {n = 3} then {
  IO.put_string "n is 3\n";
}.elseif {n = 4} then {
  IO.put_string "n is 4\n";
} else {
  IO.put_string "n is none of the above\n";
};
```


```Lisaac
(n = 2).if_true { "n is 2\n".print; };
(n = 2).if_false { "n is not 2\n".print; };
```


### when


```Lisaac
+ n : INTEGER;

n := 3;
n
.when 2 then {
  "n is 2\n".print;
}
.when 3 then {
  "n is 3\n".print;
}
.when 4 then {
  "n is 4\n".print;
};
```

There is no "else" or "otherwise" method.
If the values of the when-methods are overlapped, the related blocks will be evaluated ... they are not mutually exclusive.

## Little



```C
int a = 3;

// if-then-else
if (a == 2) {
    puts ("a is 2");
} else if (a == 3) {
    puts ("a is 3");
} else {
    puts("a is 4");
}

// unless
unless (a == 2) { // equivalent to if (a != 2)
    puts ("a is 2");  // It will print this line
} else if (a == 3) {
    puts ("a is 3");
} else {
    puts("a is 4");
}

// switch
switch (a) {
    case 2:
        puts ("a is 2");
        break;
    case 3:
        puts ("a is 3");
        break;
    case 4:
        puts ("a is 4");
        break;
    default:
        puts("is neither");
}
```



## Logo


```logo
if :x < 0 [make "x 0 - :x]

ifelse emptyp :list [print [empty]] [print :list]
```

[[UCB Logo]] and its descendants have also case:

```logo
to vowel? :letter
output case :letter [ [[a e i o u] "true] [else "false] ]
end
show vowel? "e
show vowel? "x
```

{{out}}

```logo
true
false
```

Logo also provides TEST which is local to a procedure:

```logo
to mytest :arg1 :arg2
test :arg1 = :arg2
iftrue [print [Arguments are equal]]
iffalse [print [Arguments are not equal]]
end
```



## LSE64

The simple conditionals take single words rather than blocks of statements, as in most other languages.

```lse64
t : " true"  ,t
f : " false" ,t
true  if t
false ifnot f
true  ifelse t f
```


Cascading conditionals are constructed using duplicate definitions and "then", yielding a syntax reminiscent of functional language [[Pattern Matching]].

```lse64
onetwo : drop " Neither one nor two" ,t    # default declared first
onetwo : dup 2 = then " Two" ,t
onetwo : dup 1 = then " One" ,t
```


Short-circuit operators "&&" and "||" are used for complex conditionals.

```lse64
dup 0 = || ,t    # avoid printing a null string
```



## Lua


```Lua

--if-then-elseif-then-else
if a then
  b()
elseif c then
  d()
else
  e()
end

for var = start, _end, step do --note: end is a reserved word
  something()
end

for var, var2, etc in iteratorfunction do
  something()
end

while somethingistrue() do
  something()
end

repeat
  something()
until somethingistrue()

cases = {
key1 = dothis,
key2 = dothat,
key3 = dotheother
}

cases[key]() --equivalent to dothis(), dothat(), or dotheother() respectively
```



## Luna


===if-then-else===

```luna
if char == "<" then Prepend "<" acc else acc
```

''(see: [https://github.com/luna/luna/issues/125#issuecomment-365683922 github/luna #125])''

===case-of===

```luna
class JSON:
    ...
    def asText: case self of:
        JSONString t: t
        other: throw "JSON.asText: not a text"
```


''(see: [https://github.com/luna/luna/blob/77b1d974cb07528e9f195dd47e177dd497560da1/stdlib/Std/src/Base.luna#L1047-L1050 Std.JSON])''


## M2000 Interpreter


### If Then Else.if Else


```M2000 Interpreter

Module CheckIt {
      Read a
      If a>1 then {
            Print "Top"
      } else.if a>=-4 then {
            Print "Middle"
      } else {
            Print "Bottom"
      }
}
CheckIt 100
CheckIt 0
CheckIt -100

Module CheckIt {
      Read a
      \\ using end if without blocks
      If a>1 then
            Print "Top"
      else.if a>=-4 then
            Print "Middle"
      else
            Print "Bottom"
      End If
}
CheckIt 100
CheckIt 0
CheckIt -100

Module CheckIt {
      Read a
      \\ without use of END IF in one line
      If a>1 then Print "Top" else.if a>=-4 then Print "Middle" else Print "Bottom"
}
CheckIt 100
CheckIt 0
CheckIt -100

```


===IF() and IF$() - Ternary===
One expression evaluated only. We can use If$() to use string expressions

```M2000 Interpreter

Module Checkit {
      Read x
      Print If(x>100-> 100, x)
}
Checkit 30
Checkit 300

\\ we can use more than two expressions, if i is not in range then 0 returned
Module Checkit {
      Read i,x
      Print If(i-> x*5, x*40, x*500)
}
Checkit 1, 20
Checkit 2, 20
Checkit 3, 20


```


Ternary can used as Elvis operator (a function here), when a false (or a Nothing, for some kind of objects) evaluated then return something after ->, else return true or the object so if A is object then If(A->,B) return A.



```M2000 Interpreter

Module Checkit {
      def a
      Print type$(a)="Double"
      b=(1,2,3,4)
      for i=1 to 3
            a=if(a->, b)  ' this happen only one time, where a is a double, second time a is an object
            Print a  ' print 3 values
            a++ ' add 1 to each value
            Print type$(a)="mArray"
      Next  i
}
Checkit

```



### Select Case

We can use string, and three types of cases (all of them in one case), >1000, 10 to 40, 400

```M2000 Interpreter

Module CheckIt {
      Read a
      Select Case a
      Case >1
            {
                  Print "Top"
                  \\ need block if we have more than one line of code
            }
      Case >=-4
            Print "Middle"
      Else
            Print "Bottom"
      End Select
}
CheckIt 100
CheckIt 0
CheckIt -100

Module CheckIt {
      Read a
      if a>-500 then
            Select Case a
            Case >1
                  {
                        Print "Top"
                        \\ need block if we have more than one line of code
                  }
            Case >=-4
                  Print "Middle"
            Else  Case    \\ need ELSE CASE if Select Case is inside a IF END IF (WITHOUT BLOCK)
                  Print "Bottom"
            End Select
      Else
            Print "Out of range"
    End if
}
CheckIt 100
CheckIt 0
CheckIt -100
CheckIt -500


```



### Conditional loops


```M2000 Interpreter

x=10
While x>0 {
      Print x
      x--
}
Do {  ' se can use Repeat in place of Do
      x++
      Print x
} Until x=10

\\ without curly braces:
x=10
While x>0
      Print x
      x--
End While
Do
      x++
      Print x
Until x=10


```


===On Goto, On Gosub===

```M2000 Interpreter

Module CheckIt {
      For i=1 to 10 {
            x=Random(1,2)
            {
                  On x Goto alfa, beta
                  alfa:
                        Print "ok1"
                        Exit
                  beta:
                        Print "ok2"
                        Exit
            }
            Print "End One"

            x=Random(1,2)
            {
                  On x Gosub alfa1, beta1
                  Exit
                  alfa1:
                        Print "ok1"
                        Return
                  beta1:
                        Print "ok2"
                        Return
            }
            Print "End"
      }
}
CheckIt


```


### If Then line No /label

Line numbers are optional and can be in any order, but from start in current block, and if not found then replaced with exit, until search can't continue (at modules/function bounds, we can't jump out of a module or function).


```M2000 Interpreter

a$="123"
if a$ ~ "12*" then 1000
alfa: ' only remarks here
Print "print here, spaghetti code, marvelous"
Exit
1000 Print "ok final"
Goto alfa

```


We can jump out of a sub, but we have to use Return to adjust the return stack.Wehn found Sub interpreter execute Exit statement so no need for End or Exit before sub beta()

We can call beta() using Gosub beta()  (not Call, call is for modules and functions). If we have an array beta() then we must use Gosub beta() because interpreter prefer arrays, and raise error "missing ="


```M2000 Interpreter

Module Checkit {
      Rem : Dim beta(10)  ' remove Rem to get error when call beta() without Gosub
      Rem : Gosub beta() ' remove again Rem  and erase next line to use beta() correct
      'beta()
      sub beta()
            local i
            for i=1 to 10
                  alfa(i)
            next i
      end sub
      sub alfa(x)
      goto 100
      Print "no print"
      End Sub

      100 Print "ok this printed", x
              Return
}
Checkit


```



## Make

An if condition using pure make (no gmake extensions)

```make
# make -f do.mk C=mycond if
C=0

if:
   -@expr $(C) >/dev/null && make -f do.mk true; exit 0
   -@expr $(C) >/dev/null || make -f do.mk false; exit 0

true:
   @echo "was true."

false:
   @echo "was false."
```


Using it

```make
make -f do.mk if C=0
> was false.

make -f do.mk if C=1
> was true.
```


With out using recursion but letting make continue with non-failed
targets even when some of the targets failed (-k)

```make
C=0

if: true false

true:
   @expr $(C) >/dev/null && exit 0 || exit 1
   @echo "was true."

false:
   @expr $(C) >/dev/null && exit 1 || exit 0
   @echo "was false."
```


Invoking it. Note the use of -k which allows make to evaluate subsequent
targets even when a previous non-related target failed.

```make
|make -f do.mk -s -k C=1
was true.
*** Error code 1
|make -f do.mk -s -k C=0
*** Error code 1
was false.
```


Using gmake


```make
A=
B=

ifeq "$(A)" "1"
       B=true
else
       B=false
endif

do:
       @echo $(A) ..  $(B)
```


Using it

```make
|gmake -f if.mk A=1
1 .. true
|gmake -f if.mk A=0
0 .. false
```



## Maple

Maple offers both conditional statements and conditional functions.


###  Conditional statements

Example syntax for conditional statements:

```Maple>if x
 0 then
   res := x;
else
   res := -x;
end if;
```


Example syntax for conditional statements with else-if:

```Maple
if x = 0 then
   res := y;
elif y = 0 then
   res := x;
else
   res := sqrt(x^2+y^2);
end if;
```



###  Conditional functions

The Maple function <code>`if`(cond,a,b)</code> (note the backtick <code>`</code> delimiters) returns ''a'' when ''cond'' is true and ''b'' otherwise.

<code>res := `if`(n::even, n/2, 3*n+1);</code>

The piecewise command can be used for functional evaluation in which there is more than one branch.  The following is equivalent to the if/then construct from the previous section.

<code>res := piecewise(x=0, y, y=0, x, sqrt(x^2+y^2));</code>


## Mathematica


Usual <code>If[condition,True,False]</code>

Make a definition with the condition that x should be positive:
<code>f[x_] := ppp[x] /; x > 0</code>

<code>f[5]</code> gives <code>ppp[5]</code>

<code>f[-6]</code> gives <code>f[-6]</code>


## MATLAB


### If statements

Example:

```MATLAB
if x == 1
    disp 'x==1';
elseif x > 1
    disp 'x>1';
else
    disp 'x<1';
end
```


### Switch statements

Example:

```MATLAB
switch x
    case 1
        disp 'Hello';
    case 2
        disp 'World';
    otherwise
        disp 'Skynet Active';
end
```



## Maxima


```maxima
if test1 then (...) elseif test2 then (...) else (...);
```



## MAXScript


### if


```maxscript
if x == 1 then
(
     print "one"
)
else if x == 2 then
(
    print "two"
)
else
(
    print "Neither one or two"
)
```



### case

'''Form one'''

```maxscript
case x of
(
    1:       (print "one")
    2:       (print "two")
    default: (print "Neither one or two")
)
```

'''Form two'''

```maxscript
case of
(
    (x == 1): (print "one")
    (x == 2): (print "two")
    default:  (print "Neither one or two")
)
```



## MBS



```MBS
INT x;
x:=0;
IF x = 1 THEN
  ! Do something
ELSE
  ! Do something else
ENDIF;
```



## MDL



### COND


This is the main conditional structure in MDL, equivalent to <code>cond</code> in other Lisp variants. <code>COND</code> may contain any number of clauses. The first element of each clause is evaluated, and if it's true, each following element in the clause is evaluated; otherwise control proceeds to the next clause, and so on. In any case, the return value is the result of the last evaluation performed.

An "else" part is traditionally implemented as a final clause whose first element is an atom, like <code>T</code>, since atoms always evaluate to themselves and are always true.


```MDL
<COND (<==? .X 1> <PRINC "one">)
      (<==? .X 2> <PRINC "two">)
      (T <PRINC "something else">)>
```



### AND and OR


These short-circuiting boolean functions can also be used as simple conditionals.


```MDL
;"Negate X if its value is less than 0"
<AND <L? .X 0> <SET X <- .X>>>

;"Print a message unless the quiet flag is set"
<OR .QUIET? <PRINC "Finished">>
```



## Metafont



```metafont
if conditionA:
  % do something
elseif conditionB:
  % do something
% more elseif, if needed...
else:
  % do this
fi;
```


The particularity of <tt>if</tt> construct in Metafont is that it can be part of an expression, and the "do something" <cite>does not need to fit into the syntactic structure</cite>. E.g. we can write something like


```metafont>b := if a
 5: 3 + else: 2 - fi c;
```


Alone, the code <tt>3 +</tt> does not mean anything; but once the condition is evaluated, the whole expression must become "correct"; e.g. if <tt>a > 5</tt>, the expression will be
<tt>b := 3 + c;</tt>.

There are no other kind of conditional structures, but the great flexibility of Metafont allows for sure to create "new syntaxes" similar to <tt>switches</tt> or whatever needed.

=={{header|МК-61/52}}==
Conditional jumps are done by four instructions, comparing the register X with zero:

<lang>x=0	XX
x#0	XX
x>=0	XX
x<0	XX
```


'''XX''' here is the address to which to make the jump in the event of failure of this condition (for this reason, these instructions are also called checks).


## MiniScript

MiniScript supports if/else-if/else, with arbitrary number of else-if sections when in block form:


```MiniScript
x = 42
if x < 10 then
    print "small"
else if x < 20 then
    print "small-ish"
else if x > 100 then
    print "large"
else
    print "just right"
end if
```


It also supports single-line if or if/else statements (though no else-if sections are permitted in this case):


```MiniScript
x = 42
if x < 50 then print "small" else print "big"
```


Finally, like many other languages, MiniScript uses short-circuit evaluation, a form of implicit branching where the rest of a boolean expression is not evaluated at all if the truth value can already be determined.


```MiniScript
isSmall = function(x)
    print "checking smallness"
    return x < 40
end function

isBig = function(x)
    print "checking bigness"
    return x > 60
end function

isSmall(10) or isBig(100)
```


{{out}}

```txt
checking smallness
```



=={{header|Modula-2}}==
===if-then-else===

```modula2
IF i = 1 THEN
  InOut.WriteString('One')
ELSIF i = 2 THEN
  InOut.WriteString('Two')
ELSIF i = 3 THEN
  InOut.WriteString('Three')
ELSE
  InOut.WriteString('Other')
END;
```



### Case


```modula2
CASE i OF
  1 : InOut.WriteString('One')
| 2 : InOut.WriteString('Two')
| 3 : InOut.WriteString('Three')
ELSE
  InOut.WriteString('Other')
END
```


=={{header|Modula-3}}==
===if-then-else===

```modula3
IF Foo = TRUE THEN
  Bar();
ELSE
  Baz();
END;
```



```modula3
IF Foo = "foo" THEN
  Bar();
ELSIF Foo = "bar" THEN
  Baz();
ELSIF Foo = "foobar" THEN
  Quux();
ELSE
  Zeepf();
END;
```



### Case


```modula3
CASE Foo OF
| 1 => IO.Put("One\n");
| 2 => IO.Put("Two\n");
| 3 => IO.Put("Three\n");
ELSE
  IO.Put("Something\n");
END;
```

===Type-case===
<tt>TYPECASE</tt> is used on reference types to perform different operations, depending on what it is a reference to.

```modula3
TYPECASE ref OF
| NULL => IO.Put("Null\n");
| CHAR => IO.Put("Char\n");
| INTEGER => IO.Put("Integer\n");
ELSE
  IO.Put("Something\n");
END;
```



## Monicelli

Monicelli has a single conditional structure that covers both if/then/else and switch/case

```monicelli

che cosè var?    # switch var
   minore di 0:  # case var < 0
     ...
   maggiore di 0: # case var > 0
     ...
   o tarapia tapioco: # else (none of the previous cases)
     ...
e velocità di esecuzione

```



## Morfa

===if-then-else===

```morfa

if(s == "Hello World")
{
    foo();
}
else if(s == "Bye World")
    bar();
else
{
    baz();
}

```

Morfa supports [[wp:Short-circuit_evaluation|short-circuit evaluation]], so <tt>obj.foo()</tt> won't be executed if <tt>obj</tt> is null:

```morfa

if(obj isnt null and obj.foo())
   doSomething();

```



### ternary


```morfa

var t = if(s == "Hello World") foo() else bar();

```



### switch

There is no fallthrough, <tt>break</tt> statement does not have any special meaning inside a switch. If the <tt>break</tt> is in a loop then <tt>break</tt> exits that loop, otherwise it is invalid.

```morfa

switch (num)
    {
        case (0)
            { /* empty case requires braces */ }
        case (1)
            { var one = "one"; result = one; }
        case (2,3) // case may contain a nonempty list of values
            result = "a few";
        default
            result = "a lot";
    }

```



## MUMPS


### If / I and ELSE / E


```MUMPS
 IF A list-of-MUMPS-commands
```

<p>All standard versions of MUMPS allow a ELSE command, which can be abbreviated to E. Instead of depending on the previous IF command, the ELSE command depends on the value of the system variable $TEST. $TEST is set whenever an IF command is executed, and whenever a timeout is specified. Since $TEST could be changed and not noticed by an unwary programmer it is important to remember when writing code.

For example with the code:

```MUMPS
 IF T DO SUBROUTINE
 ELSE DO SOMETHING
```

It isn't clear whether $TEST is changed or not, because the function SUBROUTINE might change the value of $TEST by using a timeout or an IF command.

It is better to explicitly set the $TEST special variable using IF 1 for example:

```MUMPS

 IF T DO SUBROUTINE IF 1
 ELSE DO SOMETHING
```


Another common practice is to use the argumentless DO, as it pushes the $TEST variable onto a stack and replaces it after the "dot block" is complete. An example of this code is:


```MUMPS

 IF T DO
 . DO SUBROUTINE
 ELSE DO SOMETHING
```



</p>

===$Select / $S===

```MUMPS
 WRITE $SELECT(1=2:"Unequal",1=3:"More unequal",1:"Who cares?")
```

<p>The $Select statement contains couplets separated by commas, which each consist of a conditional test, followed by a colon, and what to return if that condition is true.
The first part of the couplet must be a truth value. Since only zero is interpreted a truth value of false, any nonzero numbers when interpreted as a truth value will be considered to be true. Typically the number 1 is used as an explicitly true condition and is placed in the final couplet. If no conditions are true, the program's error processing is invoked. The very first condition that is true is the result of the expression. In the example, the value will always be "Unequal" as it is always true, and the rest of the $SELECT will never be used.</p>

===(command postconditional i.e. colon/:===

```MUMPS
 SET:(1=1) SKY="Blue"
 GOTO:ReallyGo LABEL
 QUIT:LoopDone
 WRITE:NotLastInSet ","
```

<p>Most commands can take a "postconditional", which is a colon and some conditional statement immediately after the command followed by the command separator (space) and the usual arguments of the command. The command is executed only if the conditional statement evaluates to true.</p>
<p>The exceptions are FOR, IF, and ELSE.

There are several commands that also allow for post-conditionals in their arguments.
The GOTO, and DO commands must have a label but it optionally have a colon followed by a truth value. When the truth value is interpreted as false, the flow of control does NOT move to the label indicated.  If it is true, then flow of control does move to the label.

Similarly, the XECUTE command may have a colon and postcondition on its argument, which is a expression that is interpreted as a line of MUMPS code. That code is executed when the postcondition is true, and not executed when it is false.

Some people consider timeouts to be a form of conditional.

For example in the READ command, a number (or numeric expression) after a colon is the number of seconds to wait for a user to make an entry. If the user doesn't make an entry before the timeout, the special variable $TEST is set to 0 (zero), indicating a timeout has occurred.

Likewise in the JOB command, a number (or numeric expression) after a colon is the number of seconds to wait for the system to start a new job running in "parallel" to the current job. If the system does not create a new job before the timeout, the special variable $TEST is set to 0 (zero), indicating a timeout has occurred.

</p>


## Nemerle

===if-else===
<tt>if (cond) <then> else <this>;</tt> is an expression in Nemerle, requiring both keywords (<tt>if</tt> and <tt>else</tt>) to be valid. <tt>when</tt> and <tt>unless</tt> are macros for which <this> = null. <tt>cond</tt> must be an expression that evaluates to a bool (true|false), other types aren't automatically assigned truth or falsehood as in some languages.

```Nemerle
if (the_answer == 42) FindQuestion() else Foo();
when (stock.price < buy_order) stock.Buy();
unless (text < "") Write(text);
```



### match

Much cleaner than stacked if-else's, similar in some ways to switch-case (but more flexible). See [http://nemerle.org/wiki/index.php?title=Quick_Guide#Pattern_Matching here], [http://nemerle.org/wiki/index.php?title=Grok_Variants_and_matching#Matching here], or, for extra detail, [http://nemerle.org/wiki/index.php?title=Patterns_%28ref%29 the reference].

```Nemerle
match(x)
{
    |1 => "x is one"
    |x when (x < 5) => "x is less than five"
    |_ => "x is at least five"
}
```



## NetRexx

===IF-THEN-ELSE===

```NetRexx
-- simple construct
if logicalCondition then conditionWasTrue()
                    else conditionWasFalse()

-- multi-line is ok too
if logicalCondition
then
  conditionWasTrue()
else
  conditionWasFalse()

-- using block stuctures
if logicalCondition then do
  conditionWasTrue()
  ...
  end
else do
  conditionWasFalse()
  ...
  end

-- if/else if...
if logicalCondition1 then do
  condition1WasTrue()
  ...
  end
else if logicalCondition2 then do
  condition2WasTrue()
  ...
  end
else do
  conditionsWereFalse()
  ...
  end
```



### SELECT

'''Notes:'''
<tt>SELECT</tt> can be thought of as a ''better'' <tt>IF-THEN-ELSE</tt> construct.

Block structures (<tt>DO-END</tt>) can be used here too (see <tt>IF-THEN-ELSE</tt>).

<tt>OTHERWISE</tt> is optional but may result in run-time errors (<tt>netrexx.lang.NoOtherwiseException</tt>) if it isn't provided.

```NetRexx
-- simple construct
select
  when logicalCondition1 then condition1()
  when logicalCondition2 then condition2()
  otherwise                   conditionDefault()
  end

-- set up a catch block to intercept missing OTHERWISE clause
do
  select
    when logicalCondition1 then condition1()
    when logicalCondition2 then condition2()
    end
catch ex1 = NoOtherwiseException
  ex1.printStackTrace()
end
```


===SELECT-CASE===

```NetRexx
-- simple construct
select case cc
  when 'A' then say 'the case is A'
  when 'B' then say 'the case is B'
  otherwise     say 'selection not recognized'
  end
```


'''Note:''' This is functionally equivalent to:

```NetRexx
select
  when cc == 'A' then ...
  when cc == 'B' then ...
  ...
```



### SELECT Optional Features

<tt>SELECT</tt> has optional features (<tt>CATCH</tt> &amp; <tt>FINALLY</tt>)
and options (<tt>LABEL</tt>, <tt>PROTECT</tt> &amp; <tt>CASE</tt>)

<tt>CATCH</tt> and <tt>FINALLY</tt> are used for handling exceptions thrown from inside the select group.

<tt>CASE</tt> see <tt>SELECT-CASE</tt> above.

<tt>LABEL</tt> provides a target for any <tt>LEAVE</tt> instructions and can aid in code self-documentation.

<tt>PROTECT</tt> is used for program concurrency &amp; synchonization in multi-threaded programs.

```NetRexx
select label sl protect cc case cc
  when 'A' then do
    say 'the case is A'
    if logicalCondition then leave sl -- just to use the lable
    say '...'
    end
  when 'B' then do
    say 'the case is B'
    say '...'
    end
  otherwise
    say 'selection not recognized'
    say '...'
  catch exs = RuntimeException
    say 'Gronk!'
    exs.printStackTrace()
  finally
    say 'selection done'
    say 'TTFN'
  end sl
```



## newLISP


### if

'''Interpreter:''' [[newLISP]] v.9.0

```lisp
(set 'x 1)
(if (= x 1) (println "is 1"))
```

A third expression can be used as an else.

```lisp
(set 'x 0)
(if (= x 1) (println "is 1") (println "not 1"))
```



## Nim

===if-then-else===

```nim
if x == 0:
  foo()
elif x == 1:
  bar()
elif x == 2:
  baz()
else:
  boz()
```


===case-of===

```nim
case x
of 0:
  foo()
of 2,5,9:
  baz()
of 10..20, 40..50:
  baz()
else: # All cases must be covered
  boz()
```



## Object Pascal

:''See [[Conditional Structures#Pascal|Pascal]]''

=={{header|Objective-C}}==
:''See also [[Conditional Structures#C|C]]''

One difference: the preprocessor has been extended with an '''#import''' directive which does the same thing as #include with "include guards".


## Objeck

===if-else===

```objeck

a := GetValue();
if(a < 5) {
  "less than 5"->PrintLine();
}
else if(a > 5) {
  "greater than 5"->PrintLine();
}
else {
  "equal to 5"->PrintLine();
};

```



### select


```objeck

a := GetValue();
select(a) {
  label 5: {
    "equal to 5"->PrintLine();
  }

  label 7: {
    "equal to 7"->PrintLine();
  }

  other: {
    "another value"->PrintLine();
  }
};

```



## OCaml

===if-then-else===


```ocaml
let condition = true

if condition then
  1 (* evaluate something *)
else
  2 (* evaluate something *)
```


If-then-else has higher precedence than <tt>;</tt> (the semicolon), so if you want to have multiple statements with side effects inside an "if", you have to enclose it with <tt>begin</tt>...<tt>end</tt> or with parentheses:


```ocaml
if condition then begin
  (); (* evaluate things for side effects *)
  5
end
else begin
  (); (* evaluate things for side effects *)
  42
end
```


===match-with===

```ocaml
match expression with
| 0 -> () (* evaluate something *)
| 1 -> () (* evaluate something *)
| n when n mod 2 = 0 -> () (* evaluate something *)
| _ -> () (* evaluate something *)
```


The first <tt>|</tt> is optional, and usually omitted.

Match is especially useful for [[Pattern Matching]] on various types of data structures.

Nested match's need to be surrounded by begin-end or parentheses, or else it won't know where it ends.


## Octave

'''if-then-elseif-else'''

```octave
if (condition)
  % body
endif

if (condition)
  % body
else
  % otherwise body
endif

if (condition1)
  % body
elseif (condition2)
  % body 2
else
  % otherwise body
endif
```


'''switch'''

```octave
switch( expression )
  case label1
     % code for label1
  case label2
     % code for label2
  otherwise
     % none of the previous
endswitch
```


''Labels'' can be numeric or string, or cells to group several possibilities:


```octave
switch ( x )
  case 1
    disp("it is 1");
  case { 5,6,7 }
    disp("it is 5, or 6 or 7");
  otherwise
    disp("unknown!");
endswitch
```



## Oforth


Conditional structures are :


```Oforth
aBoolean ifTrue: [ ...]
aBoolean ifFalse: [ ... ]
aObject  ifNull: [ ... ]
aObject  ifNotNull: [ ... ]
aObject  ifZero: [ ... ]
```


Each conditional structure consume the object on the top of the stack.

Each conditional structure can be followed by a else block

```Oforth
else: [ ... ]
```


Example :


```Oforth
Number virtual: sgn
    self isPositive
       ifTrue: [ self ==0 ifTrue: [ 0 ] else: [ 1 ] ]
       else: [ -1 ] ;
```



## Ol

if-then, the simplest conditional primitive.

```scheme

(if (= (* 2 2) 4) (print "if-then: equal"))
(if (= (* 2 2) 6) (print "if-then: non equal"))
; ==> if-then: equal

```


if-then-else, the full conditional 'if' primitive.

```scheme

(if (= (* 2 2) 4) (print "if-then-else: equal") (print "if-then-else: non equal"))
(if (= (* 2 2) 6) (print "if-then-else: non equal") (print "if-then-else: i don't know"))
; ==> if-then-else: equal
; ==> if-then-else: i don't know

```


unless, the opposite for 'if'.

```scheme

(unless (= (* 2 2) 4) (print "unless: non equal"))
(unless (= (* 2 2) 6) (print "unless: i don't know"))
(unless (= (* 2 2) 4) (print "unless: non equal") (print "unless: equal"))
(unless (= (* 2 2) 6) (print "unless: i don't know") (print "unless: non equal"))
; ==> unless: i don't know
; ==> unless: equal
; ==> unless: i don't know

```


case, the sequence of comparing values.

```scheme

(case (* 2 2)
   (3
      (print "case: 3"))
   (4
      (print "case: 4"))
   ((5 6 7)
      (print "case: 5 or 6 or 7"))
   (else
      (print "case: i don't know")))
; ==> case: 4

```


cond, the sequnce of comparators.

```scheme

(cond
   ((= (* 2 2) 4)
      (print "cond: equal"))
   ((= (* 2 2) 6)
      (print "cond: not equal"))
   (else
      (print "cond: i don't know")))
; ==> cond: equal

```


tuple-case, smart tuple comparer with variables filling.

```scheme

(tuple-case (tuple 'selector 1 2 3)
   ((case1 x y)
      (print "tuple-case: case1 " x ", " y))
   ((selector x y z)
      (print "tuple-case: selector " x ", " y ", " z))
   (else
      (print "tuple-case: i don't know")))
; ==> tuple-case: selector 1, 2, 3

```


case-lambda, selecting the lambda based on arguments count.

```scheme

(define smart (case-lambda
   ((x)
      (print x ", -, -"))
   ((x y)
      (print x ", " y ", -"))
   ((x y z)
      (print x ", " y ", " z))))
(smart 1)     ; ==> 1, -, -
(smart 1 2)   ; ==> 1, 2, -
(smart 1 2 3) ; ==> 1, 2, 3

```



## ooRexx

For all of the conditional instructions, the conditional expression must evaluate either to '1' or '0'.  Note that ooRexx conditional
expression evaluation does not have a short circuiting mechanism.  Where the logical operations | (or), & (and), or && (exclusive or) are
used, all parts of the expression are evaluated.
The conditional
may also be a list of conditional expressions separated by commas.  The expressions are evaluated left-to-right, and evaluation
will stop with the first '0' result.  For example,

```ooRexx
if arg~isa(.string) & arg~left(1) == "*" then call processArg arg
```


would fail with a syntax error if the variable arg does not hold a string because the right-hand-side of the expression
is still evaluated. This can be coded as


```ooRexx
if arg~isa(.string), arg~left(1) == "*" then call processArg arg
```

With this form, the second conditional expression is only evaluated if the first expression is true.

===IF THEN --- IF THEN/ELSE===

```ooRexx

if  y  then x=6                        /* Y must be either   0   or   1 */


if t**2>u then x=y
          else x=-y



if t**2>u then do j=1 to 10; say prime(j); end
          else x=-y



if z>w+4 then do
              z=abs(z)
              say 'z='z
              end
         else do;  z=0;  say 'failed.';  end



if x>y & c*d<sqrt(pz) |,
   substr(abc,4,1)=='@' then if z=0 then call punt
                                    else nop
                        else if z<0 then z=-y

```



### SELECT WHEN


```ooRexx

                     /*the  WHEN  conditional operators are the same as */
                     /*the   IF   conditional operators.                */

  select
  when t<0       then z=abs(u)
  when t=0 & y=0 then z=0
  when t>0       then do
                      y=sqrt(z)
                      z=u**2
                      end

                /*if control reaches this point  and  none of the WHENs */
                /*were satisfiied, a SYNTAX condition is raised (error).*/
  end

```


### SELECT WHEN/OTHERWISE


```ooRexx

     select
     when a=='angel'              then many='host'
     when a=='ass' | a=='donkey'  then many='pace'
     when a=='crocodile'          then many='bask'
     when a=='crow'               then many='murder'
     when a=='lark'               then many='ascension'
     when a=='quail'              then many='bevy'
     when a=='wolf'               then many='pack'
     otherwise  say
                say '*** error! ***'
                say a "isn't one of the known thingys."
                say
                exit 13
     end

```



## OxygenBasic


```oxygenbasic

if a then b=c else b=d

if a=0
  b=c
elseif a<0
  b=d
else
  b=e
end if

select case a
  case 'A'
  v=21
case 'B'
  v=22
case 1 to 64
  v=a+300
case else
  v=0
end select


```



## Oz

===if-then-else===

```oz
proc {PrintParity X}
   if {IsEven X} then
      {Show even}
   elseif {IsOdd X} then
      {Show odd}
   else
      {Show 'should not happen'}
   end
end
```


===if-then-else as a ternary operator===

```oz
fun {Max X Y}
   if X > Y then X else Y end
end
```



### case statement


```oz
fun {Fac X}
   case X of 0 then 1
   [] _ then X * {Fac X-1}
   end
end
```



## PARI/GP

GP uses a simple <code>if</code> statement:

```parigp
if(condition, do_if_true, do_if_false)
```

and short-circuit <code>&&</code> and <code>||</code> (which can be abbreviated <code>&</code> and <code>|</code> if desired).

PARI can use all of the usual [[Conditional structures/C|C conditionals]].


## Pascal

===if-then-else===


```pascal
IF condition1 THEN
  procedure1
ELSE
  procedure3;

IF condition1 THEN
  BEGIN
    procedure1;
    procedure2
  END
ELSE
  procedure3;

IF condition1 THEN
  BEGIN
    procedure1;
    procedure2
  END
ELSE
  BEGIN
    procedure3;
    procedure4
  END;
```



###  case

{{works with|Turbo Pascal|7.0}}

Case selectors must be an '''ordinal''' type. This might seem to be a restriction, but with a little thought just about anything can be resolved to an '''ordinal''' type. Additionally, each selector may consist of more then one item. The optional '''ELSE''' keyword provides a default for values that do not match any of the given cases.

In Pascal there is no fall-through to the next case. When execution reaches the end of a matching clause, it continues after the end of the case statement, not in the code for the next case.


```pascal
case i of
  1,4,9: { executed if i is 1, 4 or 9 }
    DoSomething;
  11, 13 .. 17: { executed if i is 11, 13, 14, 15, 16 or 17 }
    DoSomethingElse;
  42: { executed only if i is 42 }
    DoSomeOtherThing;
  else
    DoYetAnotherThing;
end;
```


Given the variable "X" as a char the following is valid:


```pascal
Case X of
   'A'           : statement ;
   'B'           : statement ;
   in ['C'..'W'] : statement ;
else
   Statement ;
end;
```



## Perl

{{works with|Perl|5}}


### if/else



```perl
if ($expression) {
    do_something;
}
```



```perl
# postfix conditional
do_something if $expression;
```



```perl
if ($expression) {
    do_something;
}
else {
    do_fallback;
}
```



```perl
if ($expression1) {
    do_something;
}
elsif ($expression2) {
    do_something_different;
}
else {
    do_fallback;
}
```



### unless


<code>unless</code> behaves like <code>if</code>, only logically negated.

You can use it wherever you can use <code>if</code>. An <code>unless</code> block can have <code>elsif</code> and <code>else</code> blocks, but there is no <code>elsunless</code>.


### ternary operator


The ternary operator is used as an expression within a statement, rather than as a control flow structure containing one or more statements. It is frequently used in assignment, or sometimes for passing function call arguments that vary depending on some condition.


```perl
$variable = $expression ? $value_for_true : $value_for_false;
```



### logical operators



```perl
$condition and do_something;  # equivalent to  $condition ? do_something : $condition
```



```perl
$condition or do_something;  # equivalent to  $condition ? $condition : do_something
```


<code>&&</code> and <code>||</code> have the same semantics as <code>and</code> and <code>or</code>, respectively, but their precedence is much higher, making them better for conditional expressions than control flow.


### switch


At first there was no ''switch'' structure in Perl, although there were plenty ways to emulate it. In Perl 5.8, an experimental <code>switch</code>/<code>case</code>/<code>else</code> structure was introduced. Perl 5.10 replaced this with the <code>given</code>/<code>when</code>/<code>default</code> structure borrowed from Perl 6.

{{works with|Perl|5.10}}


```perl
use feature "switch";
given ($input) {
    when (0)          { print 'input == 0'; }
    when ('coffee')   { print 'input equal coffee'; }
    when ([1..9])     { print 'input between 1 and 9'; }
    when (/rats/)     { print 'input matches rats'; }
    default           { do_fallback; }
}
```



## Perl 6


### if/else

<tt>if</tt>, <tt>else</tt>, <tt>elsif</tt>, <tt>unless</tt>, and <tt>given</tt> work much as they do in Perl 5, with the following differences:<ul>
<li> All the parentheses are now optional.
</li><li> <tt>unless</tt> no longer permits <tt>elsif</tt> or <tt>else</tt> blocks.
</li><li> If the block of an <tt>if</tt>, <tt>elsif</tt>, or <tt>unless</tt> has a nonzero arity, the value of the conditional expression is used as an argument to the block:

```perl6
if won() -> $prize {
    say "You won $prize.";
}
```

If an <tt>else</tt> block has a nonzero arity, it receives the value of the condition tested by the last <tt>if</tt> or <tt>elsif</tt>. </li></ul>

### given/when

Switch structures are done by topicalization and by smartmatching in Perl 6.  They are somewhat orthogonal, you can use a <tt>given</tt> block without <tt>when</tt>, and vice versa.  But the typical use is:

```perl6
given lc prompt("Done? ") {
    when 'yes' { return }
    when 'no'  { next }
    default    { say "Please answer either yes or no." }
}
```

<tt>when</tt> blocks are allowed in any block that topicalizes <tt>$_</tt>, including a
<tt>for</tt> loop (assuming one of its loop variables is bound to <tt>$_</tt>)
or the body of a method (if you have declared the invocant as <tt>$_</tt>)." See [http://perlcabal.org/syn/S04.html#Switch_statements Synopsis 4].

There are also statement modifier forms of all of the above.


### Ternary operator

The [[wp:ternary operator|ternary operator]] looks like this:

```perl6
$expression ?? do_something !! do_fallback
```


===Other short-circuiting operators===
<tt>and</tt>, <tt>or</tt>, <tt>&&</tt>, <tt>||</tt> and <tt>//</tt> work as in Perl 5.


## Phix


### if


```Phix
if name="Pete" then
  -- do something
elsif age>50 then
  -- do something
elsif age<20 then
  -- do something
else
  -- do something
end if
```


There is no limit to the number of elsif clauses, including 0. The else clause is also optional,
whereas the end if is always mandatory (which avoids any dangling else problems). All conditional
expressions are short-circuited.


### iff


```Phix
var = iff(flag?true_expr:false_expr)
```


In an iff() expression, only one of true_expr or false_expr will be evaluated, not both.

Phix has some rudimentary support of preprocessor ifdef statements, but their use is discouraged since
they are quite unnecessary in Phix, for example in the following no code whatsoever is emitted for the
first if statement, and in the second the conditions are evaluated at compile-time and code is only
emitted for one of the branches.

```Phix
constant DEBUG=false
if DEBUG then
    puts(1,"debug is on\n")
end if
if platform()=WINDOWS then
    puts(1,"this is windows\n")
elsif platform()=LINUX then
    puts(1,"this is linux\n")
end if
```



### switch


```Phix
switch v [with fallthrough] do
    case 1,2:
      -- do something
    case 3 then
      -- do something
      fallthrough
    case 4:
      -- do something
      break
    default:
      -- do something
end switch
```


By default there is no fallthough on switch clauses, however you can add a directive, and you can
override individual clauses with explicit fallthough or break statements. There is no need to have
break between cases when it is the default. You can use either : or then on case clauses. The else
keyword can be used instead of "default", and behaves identically. It can also be placed anywhere,
even first, or completely omitted.

The compiler will automatically construct either a jump table or daisy-chained cmp/jmp chains from
either if-constructs or switch-statements, leaving the programmer free to choose whichever shows
the intent clearest, without having to worry about performance implications.


### ilASM

Inline assembly, in the form of #ilASM{} constructs, should you be brave or desperate enough to
use them, also have some conditional guards for cross-platform support


```Phix
#ilASM{
    [32]
        mov eax,[var]
    [64]
        mov rax,[var]
    [PE32]
        push eax                        -- uExitCode
        call "kernel32.dll","ExitProcess"
    [PE64]
        mov rcx,rax                     -- uExitCode
        call "kernel32.dll","ExitProcess"
    [ELF32]
        mov ebx,eax                     -- error_code (p1)
        mov eax,1                       -- sys_exit(ebx=int error_code)
        int 0x80
--      xor ebx,ebx                     -- (common requirement after int 0x80)
    [ELF64]
        mov rdi,rax                     -- error_code (p1)
        mov rax,60                      -- sys_exit(rdi=int error_code)
        syscall
    []
      }
```



## PHL


If-else:


```phl
var a = 5;
if (a == 5) {
   doSomething();
} else if (a > 0) {
   doSomethingElse();
} else {
   error();
}
```



## PHP



### if


'''Interpreter''': [[PHP]] 3.x, 4.x, 5.x


```php
<?php

$foo = 3;

if ($foo == 2)
  //do something

if ($foo == 3)
  //do something
else
  //do something else

if ($foo != 0)
{
  //do something
}
else
{
  //do another thing
}

?>
```



### switch


'''Interpreter''': [[PHP]] 3.x & 4.x & 5.x


```php
<?php

switch ($i)
{
  case "apple":
      echo "i is apple";
      break;

  case "bar":
      echo "i is bar";
      break;

  case "cake":
      echo "i is cake";
      break;
}

?>
```



### See Also

* [http://www.php.net/manual/en/language.control-structures.php php.net:Control Structures]
* [http://www.php.net/manual/en/control-structures.switch.php php.net:Control Structures: Switch]


## PicoLisp

===Two-way conditions===

```PicoLisp
(if (condition)                  # If the condition evaluates to non-NIL
   (then-do-this)                # Then execute the following expression
   (else-do-that)                # Else execute all other expressions
   (and-more) )

(ifn (condition)                 # If the condition evaluates to NIL
   (then-do-this)                # Then execute the following expression
   (else-do-that)                # Else execute all other expressions
   (and-more) )
```

One-way conditions

```PicoLisp
(when (condition)                # If the condition evaluates to non-NIL
   (then-do-this)                # Then execute tall following expressions
   (and-more) )

(unless (condition)              # If the condition evaluates to NIL
   (then-do-this)                # Then execute all following expressions
   (and-more) )
```

===Four-way condition===

```PicoLisp
(if2 (condition1) (condition2)   # If both conditions evaluate to non-NIL
   (expression-both)             # Then execute this expression
   (expression-first)            # Otherwise this for the first
   (expression-second)           # or this the second condition.
   (expression-none)             # If both are NIL, all following expressions
   (and-more) )
```


### Multiple conditions


```PicoLisp
(cond
   ((condition1)                 # If this condition evaluates to non-NIL
      (expression 1)             # Execute these expression(s)
      (more 1) )
   ((condition2)                 # Otherwise, if this evaluates to non-NIL
      (expression 2)             # Execute these expression(s)
      (more 2) )
   (T                            # If none evaluated to non-NIL
      (expression 1)             # Execute these expression(s)
      (more 1) )

(nond
   ((condition1)                 # If this condition evaluates to NIL
      (expression 1)             # Execute these expression(s)
      (more 1) )
   ((condition2)                 # Otherwise, if this evaluates to NIL
      (expression 2)             # Execute these expression(s)
      (more 2) )
   (NIL                          # If none evaluated to NIL
      (expression 1)             # Execute these expression(s)
      (more 1) )
```



### Selection


```PicoLisp
(case (expression)               # Evaluate the expression
   (value1                       # If it is equal to, or member of, 'value1'
      (do-this1)                 # Execute these expression(s)
      (do-that1) )
   (value2                       # Else if it is equal to, or member of, 'value2
      (do-this2)                 # Execute these expression(s)
      (do-that2) )
   (T                            # Else execute final expression(s)
      (do-something-else) ) )
```



## PL/I

===if-then-else===

```pli
if condition_exp then unique_statement; else unique_statement;

if condition_exp then
    unique_statement;
else
    unique_statement;

if condition_exp
then do;
    list_of_statements;
end;
else do;
    list_of_statements;
end;
```


So a cascading form can be derived from:

```pli
if condition_exp1 then
    statement_1;
else if condition_exp2 then
    statement_2;
else if condition_expN then
    statement_N;
else
    statement_E;

if condition_exp1 then do;
    list_of_statements;
end;
else if condition_exp2 then do;
    list_of_statements;
end;
else if condition_expN then do;
    list_of_statements;
end;
else do;
    list_of_statements;
end;
```



###  case

The PL/I 'case' statement has two possible formats:

==== select - format 1 ====

```pli
select (i); /* select on value of variable */
  when (1,4,9)
    do;
      statement_s;
    end;

  when (11, 42)
    do;
      statement_s;
    end;

  other /* everything else */
    do;
      statement_s;
    end;
end;
```


==== select - format 2 ====

```pli
select; /* select first matching condition */
  when (i = 4)
    do;
      statement_s;
    end;

  when (this = that)
    do;
      statement_s;
    end;

  when (mystring = 'ABCDE')
    do;
      statement_s;
    end;

  other
    do;
      statement_s;
    end;
end;
```


Notes:

* in PL/I there is no fall-through to the next '''when'''. When execution reaches the end of a matching clause, it continues after the end of the select statement, not in the code for the next case.
* the '''do''' ... '''end''' statements can be omitted if the when clause is a single statement.
* if no '''other''' (or in full: '''otherwise''') statement is present and none of the '''when''' cases is matched, the program will end in error.


## Pop11


The simplest conditional is:


```pop11
if condition then
    ;;; Action
endif;
```


Two way conditional looks like:


```pop11
if condition then
    ;;; Action1
else
    ;;; Alternative action
endif;
```


One can do multiway choice using elseif clause


```pop11
if condition1 then
   ;;; Action1
elseif condition2 then
   ;;; Action1
elseif condition2 then
   ;;; Action2
elseif condition3 then
   ;;; Action3
else
   ;;; Alternative action
endif;
```


Instead of if keyword one can use unless keyword.


```pop11
unless condition then /* Action */ endunless;
```


has the same meaning as


```pop11
if not(condition) then /* Action */ endif;
```


One can also use elseunless keword.


```pop11
if condition1 then
   ;;; Action1
elseunless condition2 then
   ;;; Action2
endif;
   ;;; Action2
endif;
```


has the same meaning as


```pop11
if condition1 then
   ;;; Action1
elseif not(condition2) then
   ;;; Action2
endif;
```


Note that conditional must end in matching keyword, if must be finished by endif, unless must be finished by endunless (in the
middle one can mix elseif with elseunless.

Pop11 conditional is an expression:


```pop11>if x
 0 then 1 elseif x < 0 then -1 else 0 endif -> sign_x ;
```


assigns sign of x to sign_x.

Instead of multiway if one can use switchon construct (which is equivalent to a special case of if, but may be shorter).


```pop11
switchon(x)
    case .isstring  then printf('A1');
    notcase .isinteger then printf('A2');
    case = 2 orcase = 3 then printf('A3');
    case > 4 andcase < 15 then printf('A4');
    else printf('A5');
endswitchon;
```


There is also multiway goto statement and conditional control transfers, we explain them together with other control transfers
and loops (in case of loop exit/continue statements).

Pop11 also has preprocessor allowing conditional compilation:


```pop11
#_IF condition1
/* Variant 1 */
#_ELSEIF condition2
/* Variant 2 */
#_ELSE
/* Variant 3 */
#_ENDIF
```


condition1 and condition2 are arbitrary Pop11 expressions (they have access to all previously compiled code).

Also note that Pop11 syntax is user extensible, so users may create their own conditional constructs.


## PostScript


The "<tt>if</tt>" operator uses two items form the stack, a procedure and a boolean. It will execute the procedure if the boolean is true. It will not leave anything on the stack (but the procedure might):


```postscript
9 10 lt {(9 is less than 10) show} if
```


The "<tt>ifelse</tt>" operator expects two procedures and executes the one or the other depending on the value of the boolean. I.e. this:


```postscript
/a 5 lt {(yeah)} {(nope)} ifelse show
```


will render either the string "yeah" or "nope" depending on whether <tt>a</tt> is less than 5 or not.


## PowerShell

===If, ElseIf, Else===

```powershell
# standard if
if (condition) {
    # ...
}

# if-then-else
if (condition) {
    # ...
} else {
    # ...
}

# if-then-elseif-else
if (condition) {
    # ...
} elseif (condition2) {
    # ...
} else {
    # ...
}
```


### Switch


```powershell
# standard switch
switch ($var) {
    1 { "Value was 1" }
    2 { "Value was 2" }
    default { "Value was something else" }
}

# switch with wildcard matching
switch -Wildcard ($var) {
    "a*" { "Started with a" }
    "*x" { "Ended with x" }
}

# switch with regular expression matching
switch -Regex ($var) {
    "[aeiou]" { "Contained a consonant" }
    "(.)\1" { "Contained a character twice in a row" }
}

# switch allows for scriptblocks too
switch ($var) {
    { $_ % 2 -eq 0 } { "Number was even" }
    { $_ -gt 100 }   { "Number was greater than 100" }
}

# switch allows for handling a file
switch -Regex -File somefile.txt {
    "\d+" { "Line started with a number" }
    "\s+" { "Line started with whitespace" }
}
```



## Prolog


A "pure" Prolog program by its very nature is one very long, very complicated boolean test.  Absolutely every executable portion of Prolog is a test that succeeds or fails.  Here are some examples, thus, of using conditionals in Prolog:


```Prolog
go :- write('Hello, World!'), nl.
```


While operationally this looks like a program that when go/0 is executed will print "Hello, World!" and exit, it is actually a predicate, in the strict logical sense of the term, that tests conditions.  Denotationally we'd describe it as "go/0 succeeds iff write/1 succeeds with its passed-in argument, and if nl/0 subsequently succeeds."  (The fact that write/1 and nl/0 **always** succeed and that we use them for their side effects only doesn't matter to the Prolog view of a program.)


```Prolog
fact(foo).
fact(bar).
fact(baz).

go :- fact(booger).
go :- fact(bar).
```


This example shows a few features of Prolog's testing and, specifically, shows nondeterminism and backtracking in action.  In this we have a predicate fact/1 (so named because in this format, without an executable body, it is termed a "fact" in the literature).  It has two clauses asserting both "bar" and "baz" as facts.  go/0 also has two clauses.  If we execute go/0, the runtime will tell us "true" (or, in some implementations, "yes") to indicate that the predicate call was successful.  Denotationally we would say "fact(X) succeeds iff X unifies with foo, X unifies with bar, or X unifies with baz".  We would also say "go/0 succeeds iff fact(booger) succeeds or if fact(bar) succeeds".  When running, the first clause of go/0 will be executed and fact(booger) will be tested.  fact(booger) does not match fact(bar) nor does it match fact(baz) so it fails.  This leads the runtime to go back and try again with the **second** go/0 clause.  In this one fact(bar) does, in fact, match fact(bar), so the overall test passes.  A Prolog program is, thus, a very complicated tree of if/then statements, in effect.


```Prolog
fact(X) :-
    (   X = foo
    ;   X = bar
    ;   X = baz ).

go :-
    (   fact(booger)
    ;   fact(bar) ).
```


This version is semantically the same as the previous one.  (In actual execution, because of some runtime optimizations, there are some minor differences in outcome, but nothing that would change the logical interpretation of the program.)  Here we're showing more explicitly the various "or" conditions.  In Prolog "," is roughly equivalent to "and" (conjunction) while ";" is roughly equivalent to "or" (disjunction).  Because of this, and because of the fact we've taken separate clauses now and put them into explicit disjunctions it is clearer that we're performing a series of if/then tests in effect.

That being said, Prolog does have something that's very akin to real if/then statements (or, more accurately, similar to the ternary operator of languages like C):


```Prolog
fact(X) :-
    (   X = bar ->  write('You got me!'), nl
    ;               write(X), write(' is not right!'), nl, fail ).

go :-
    (   fact(booger)
    ;   fact(bar) ).
```


In this version of fact/1, the -> operator is used to perform a more traditional if/then/else.  The general construct is ( condition -> succeed_branch ; fail_branch ).  In this case if the parameter passed in unifies with 'bar', a message is written (recall that write/1 and nl/0 always succeed!) and the whole predicate exists with a success.  If, on the other hand, the unification fails (you pass anything other than 'bar') it writes a snarky message and then calls fail/0, a predicate that, as its name suggests, always fails.  There are more implications to using the conditional expression in Prolog; it is generally considered code smell.  Other operators also exist for handling conditionals (like *->) that lack the "smell" of the conditional operator.  The reasons for this are out of scope, however, for this article.  Just know that the fact/1 predicate could have used *-> in place of -> and been more "sound" as a result.


## PureBasic

{{works with|PureBasic|4.41}}
===If, Elseif, Else===

```PureBasic
If a = 0
  Debug "a = 0"

ElseIf a > 0
  Debug "a > 0"

Else
  Debug "a < 0"

EndIf
```



### Select


```PureBasic
Variable = 2

Select Variable
  Case 0
    Debug "Variable = 0"

  Case 10, 11, 99
    Debug "Variable is 10, 11 or 99"

  Case 20 To 30
    Debug "Variable >= 20 And Variable <= 30"

  Default
    Debug "Variable = something else..."
EndSelect
```



### CompilerIf

Compiler conditional structures works like normal conditional structures, except they are evaluated at compile time, and thus have to use constant expressions. Any defined constant can be used, these examples uses built-in constants.

```PureBasic

CompilerIf #PB_Compiler_OS = #PB_OS_Linux And #PB_Compiler_Processor = #PB_Processor_x86
  Debug "Compiled on x86 Linux"
CompilerElse
  Debug "Compiled on something else"
CompilerEndIf

```



### CompilerSelect


```PureBasic

CompilerSelect #PB_Compiler_OS
  CompilerCase #PB_OS_Linux
    Debug "Compiled on Linux"
  CompilerCase #PB_OS_Windows
    Debug "Compiled on Windows"
  CompilerCase #PB_OS_MacOS
    Debug "Compiled on Mac OS"
  CompilerDefault
    Debug "Compiled on something else"
CompilerEndIf

```



## Python

===if-then-else===


```python
if x == 0:
    foo()
elif x == 1:
    bar()
elif x == 2:
    baz()
else:
    boz()
```



### ternary expressions

'''Interpreter:''' [[Python]] 2.5


```python
true_value if condition else false_value
```


Example:

```python>>>
 secret='foo'
>>> print 'got it' if secret=='foo' else 'try again'
'got it'
```


'''Note:''' this syntax is valid as an expression, the clauses cannot constain statements.  The foregoing example is equivalent to:


```python>>>
 secret = 'foo'
>>> result = 'got it' if secret=='foo' else 'try again'
>>> print result
'got it'
```



### Function dispatch dictionary


In some cases it's useful to associate functions with keys in a dictionary; and simply use this in lieu of long sequences of "if...elif...elif..." statements.


```python
dispatcher = dict()
dispatcher[0]=foo  # Not foo(): we bind the dictionary entry to the function's object,
                   # NOT to the results returned by an invocation of the function
dispatcher[1]=bar
dispatcher[2]=baz  # foo,bar, baz, and boz are defined functions.

# Then later
results = dispatcher.get(x, boz)()  # binding results to a name is optional
# or with no "default" case:
if x in dispatcher:
    results=dispatcher[x]()
```



```python
# The above, but with a dict literal
dispatcher = {
    0: foo,
    1: bar,
    2: baz,
}
# ...
results = dispatcher.get(x, boz)()
```



```python
# Or without the temp variable
# (it's up to the reader to decide how "pythonic" this is or isn't)
results = {
    0: foo,
    1: bar,
    2: baz,
}.get(x, boz)()
```


This can be particularly handy when using [[wp:Currying|currying]] techniques, or when lambda expressions or meta-function generators (factories) can be used in place of normal named functions.

In general a dispatch table or class/object abstraction (using dynamic method over-rides) is considered preferable to chains of ''if ... elif ... elif ...'' in Python programming.


## Racket


===[http://docs.racket-lang.org/reference/if.html#%28form._%28%28quote._~23~25kernel%29._if%29%29 if]===
If-expressions in Racket must have both branches

```racket

(if (< x 10)
  "small"
  "big")

```


===[http://docs.racket-lang.org/reference/when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._when%29%29 when/unless]===
One-sided conditional expressions use "when" and "unless".  These are more convenient for side-effects since they have an implicit "begin" around their body, and you can also include new definitions

```racket

(when (< x 10)
  (define y (* x 10))
  (printf "small\n"))

```


===[http://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._cond%29%29 cond]===
Used for multiple conditions:

```racket

(printf "x is ~a\n"
        (cond [(< x 1)     "tiny"]
              [(< x 10)    "small"]
              [(< x 100)   "medium"]
              [(< x 10000) "big"]
              [(< x 100000000) "huge"]
              [else "gigantic"]))

```


===[http://docs.racket-lang.org/reference/case.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._case%29%29 case]===
Similar to a "switch" statement in other languages

```racket

(case x
  [(1) "one"]
  [(2) "two"]
  [(3) "three"]
  [(4) "four"]
  [(6 8)   "even"]
  [(5 7 9) "odd"]
  [else    "something else"])

```



### etc

Racket has macros, which means that you can define whatever new conditional you think is useful...


## Red

===If-Either-Case-Switch===
If the result is true, the block! will be evaluated. If false nothing happens.

```Red>>> if 10
 2 [print "ten is bigger"]
ten is bigger
```


### EITHER

If the result is true the first block! will be evaluated.
If false the second block! will be evaluated.

```Red>>> either 3
 2 [print "Three larger"][print "Nope!"]
Three larger
```


### CASE

The block! following the first true condition is evaluated.

```Red
n: 50
case [
  n < 10   [print "small number"]
  n < 100  [print "medium number"]
  n < 1000 [print "large number"]
  true     [print "none of these"]
]

medium number

;CASE/ALL Prints all that are true
n: 50
case/all [
  n < 10   [print "small number"]
  n < 100  [print "medium number"]
  n < 1000 [print "large number"]
  true     [print "none of these"]
]

medium number
large number
none of these
```



### SWITCH


```Red
switch "india" [
   "a"       [print "string"]
   23        [print "integer"]
   "India"   [print "The country India"]
]

The country India

switch/default "U.S." [
   "a"       [print "string"]
   23        [print "integer"]
   "India"  [print "The country India"]
][
print "no match"
]

no match
```



## Retro

===choose, if, and -if===

```Retro
condition  [ true statements ] if
condition  [ false statements ] -if
condition  [ true statements  ] [ false statements ] choose
```


These forms can be used interactively, or inside function definitions.


### when


```Retro
:foo (n-)
  #1 [ ( if quote evaluates to true ) ] case
  #2 [ ( if quote evaluates to true ) ] case
  #3 [ ( if quote evaluates to true ) ] case
  drop ( default action ) ;
```



## REXX

===IF--THEN, IF--THEN--ELSE===

```rexx
if  y  then @=6                        /* Y  must be either   0  or  1  */

if t**2>u  then x=y                    /*simple  IF  with  THEN & ELSE. */
           else x=-y

if t**2>u  then do j=1  for 10;  say prime(j);  end    /*THEN  DO  loop.*/
           else x=-y                                   /*simple  ELSE.  */

if z>w+4  then do                                      /*THEN  DO group.*/
               z=abs(z)
               say 'z='z
               end
          else do;  z=0;  say 'failed.';  end          /*ELSE  DO group.*/

if x>y  &  c*d<sqrt(pz) |,             /*this statement is continued [,]*/
   substr(abc,4,1)=='~'  then  if  z=0  then call punt
                                        else nop       /*NOP pairs up IF*/
                         else  if  z<0  then z=-y      /*alignment helps*/
```


===SELECT--WHEN===

```rexx
                      /*the  WHEN  conditional operators are the same as*/
                      /*the   IF   conditional operators.               */
 select
 when t<0        then z=abs(u)
 when t=0 & y=0  then z=0
 when t>0        then do
                      y=sqrt(z)
                      z=u**2
                      end

                      /*if control reaches here & none of the WHENs were*/
                      /*satisfiied, a SYNTAX (error) condition is raised*/
 end  /*1st select*/

     select
     when a=='angel'              then many='host'
     when a=='ass' | a=='donkey'  then many='pace'
     when a=='crocodile'          then many='bask'
     when a=='crow'               then many='murder'
     when a=='lark'               then many='ascension'
     when a=='quail'              then many='bevy'
     when a=='wolf'               then many='pack'
     otherwise                         many='?'
     end  /*2nd select*/          /* [↑]  uses OTHERWISE as a catch-all.*/
```


===SELECT--WHEN/OTHERWISE===

```rexx
     select
     when g=='angel'              then many='host'
     when g=='ass' | g=='donkey'  then many='pace'
     when g=='crocodile'          then many='bask'
     when g=='crow'               then many='murder'
     when g=='lark'               then many='ascension'
     when g=='quail'              then many='bevy'
     when g=='wolf'               then many='pack'
     otherwise  say
                say '*** error! ***'
                say g  "isn't one of the known thingys."
                say
                exit 13
     end   /*select*/
```



## Rhope

{{works with|Rhope|alpha 1}}
===if-then-else===

```rhope
If[cond]
|:
    Do Something[]
:||:
    Do Something Else[]
:|
```



## RLaB



###  if

Block of instructions following the ''if'' command has to be always enclosed in curly brackets.

```RLaB

if (x==1)
{
  // do something
}

```


=== if-else ===
If there are branching within the command, respective blocks have to be enclosed in the blocks preceding it.
Consider an example:


```RLaB

if (x==1)
{
  // do something if x is 1
  y = const.pi;
else
  // do something if x is not 1
  y = sin(const.pi*(1-x)) / (1-x);
}

```



```RLaB

if (x==1)
{
  // do something if x is 1
  y = const.pi;
else if (x == 2)
{
  // do something if x is 2
  y = sin(const.pi*(1-x)) / (1-x);
else
  // do something in all the other cases
  y = rand();
}}

```




## Ring

'''if-but-else-ok'''

```ring
If x == 1
   SomeFunc1()
But x == 2
   SomeFunc2()
Else
   SomeFunc()
Ok
```


'''Switch'''

```ring
Switch x
On 1
   SomeFunc1()
On 2
   SomeFunc2()
Other
   SomeFunc()
Off
```



## Ruby

See [[Conditional Structures/Ruby]]

## Run BASIC


```Runbasic
' Boolean Evaluations
'
' > Greater Than
' < Less Than
' >= Greater Than Or Equal To
' <= Less Than Or Equal To
' = Equal to

x = 0

if x = 0 then print "Zero"

' --------------------------
' if/then/else
if x = 0 then
print "Zero"
else
print "Nonzero"
end if

' --------------------------
' not
if x then
print "x has a value."
end if
if not(x) then
print "x has no value."
end if

' --------------------------
' if .. end if
if x = 0 then
print "Zero"
goto [surprise]
end if
wait

if x = 0 then goto [surprise]
print "No surprise."
wait

[surprise]
print "Surprise!"
wait

' --------------------------
' case numeric
num = 3

select case num
case 1
print "one"

case 2
print "two"

case 3
print "three"

case else
print "other number"

end select

' --------------------------
' case character
var$="blue"

select case var$

case "red"
print "red"

case "green"
print "green"

case else
print "color unknown"

end select
```



## Rust

===Compile-Time===

### =Conditional compilation=

Rust supports conditional compilation via the `cfg` annotation.

```rust
// This function will only be compiled if we are compiling on Linux
#[cfg(target_os = "linux")]
fn running_linux() {
    println!("This is linux");
}
#[cfg(not(target_os = "linux"))]
fn running_linux() {
    println!("This is not linux");
}

// If we are on linux, we must be using glibc
#[cfg_attr(target_os = "linux", target_env = "gnu")]
// We must either be compiling for ARM or on a little endian machine that doesn't have 32-bit pointers pointers, on a
// UNIX like OS and only if we are doing a test build
#[cfg(all(
        any(target_arch = "arm", target_endian = "little"),
        not(target_pointer_width = "32"),
        unix,
        test
        ))]
fn highly_specific_function() {}

```

Conditional compilation may also be achieved via the `cfg!` macro.

```rust
fn main() {
     if cfg!(target_os = "linux") {
         // Do something
     }
}
```


====Generics (static dispatch)====
By default, generics in Rust are monomorphized, so no vtable lookups at runtime are necessary.

```rust
trait PrintType {
    fn print_type(&self);
}

impl PrintType for char {
    fn print_type(&self) {
        println!("char");
    }
}

impl PrintType for f64 {
    fn print_type(&self) {
        println!("64-bit float");
    }
}

fn prints_type_of_args<T, U>(arg1: &T, arg2: &U)
    where T: PrintType,
          U: PrintType
{
    arg1.print_type();
    arg2.print_type();
}

fn main() {
    prints_type_of_args(&'a', &2.0);
    prints_type_of_args(&'a', &'b');
}
```



### Runtime

====If-statement====

```rust
if some_conditional {
    do_stuff();
} else if some_other_conditional {
    do_other_stuff();
} else {
    destroy_humanity();
}

// If statements are also expressions and will yield the value of the last expression in each block
let x = if y > z { y + 1 } else { z * 4 };

// Pattern matching may also be used
struct Point {
    x: i32,
    y: i32,
}
fn some_function(p: Option<Point>) {
    if let Some(Point { x: x_coord, y: y_coord }) = p {
        // Do something with x_coord and y_coord
    }
}
```



### =Match statement=

Match statements are essentially more powerful switch statements

```rust
fn some_other_function(p: Option<Point>) {
    match p {
        Some(Point { x: 0, y: 0 }) => println!("Point is on origin"),
        Some(Point { x: 0, y: _ }) | Some(Point { x: _, y: 0 }) => println!("Point is on an axis"),
        Some(Point {x: a, y: b}) if a == b => println!("x and y are the same value"),
        Some(Point {x: ref mut a, y: ref b}) if *a > 4 && *b < 2 => println!("we got a mutable reference to x-value and an immutable reference to y-value."),
        op @ Some(p) => println!("op is the Option<Point> while p is the contained Point"),
        None => println!("We didn't get a point"),
    }
}
```


====Generics (dynamic dispatch)====
Generics may also be accomplished via dynamic dispatch, so the actual code that is run is determined at compile time.
Using the same trait defined in the static dispatch section:

```rust
fn prints_args_dynamic(arg1: &PrintType, arg2: &PrintType) {
    arg1.print_type();
    arg2.print_type();
}
fn main() {
   prints_args_dynamic(&'a', &2.0);
   prints_args_dynamic(&6.3,&'c');
}
```



## Sather



```sather
    if EXPR then
      -- CODE
    elsif EXPR then
      -- CODE
    else
      -- CODE
    end;
```


EXPR must evaluate to BOOL (true or false); <code>elsif</code> and <code>else</code> are optional.


```sather
    case EXPR
      when EXPRL then
         -- CODE
      when EXPRL then
         -- CODE
      else
         -- CODE
    end;
```


EXPRL is a single expression or a comma-separated list of exressions. The expressions must evaluate to comparable objects (the method <code>is_eq</code> must be implemented)


## Scala

{{libheader|Scala}}

```Scala
  if (n == 12) "twelve" else "not twelve"

  today match {
    case Monday =>
      Compute_Starting_Balance;
    case Friday =>
      Compute_Ending_Balance;
    case Tuesday =>
      Accumulate_Sales
    case _ => {}
  }
```



## Scheme

Procedures can be categorised as primitive or derived. Derived procedures can
be defined in terms of primitive procedures.

### Primitive


### =if=

<lang>(if <test> <consequent> <alternate>)
```

<lang>(if <test> <consequent>)
```

Example:

```scheme
(display
  (if (> 1 2)
      "yes"
      "no"))
(newline)
(display
  (if (> 1 2)
      (- 1 2)))
(newline)
```

{{out}}

```txt
no
#<unspecified>
```



### Derived


### =cond=

<lang>(cond <clause1> <clause2> ...)
```

Example:

```scheme
(display
  (cond ((> 1 2) "greater")
        ((< 1 2) "less")))
(newline)
(display
  (cond ((> 1 1) "greater")
        ((< 1 1) "less")
        (else "equal")))
(newline)
```

{{out}}

```txt
less
equal
```



### =case=

<lang>(case <key> <clause1> <clause2> ...)
```

Example:

```scheme
(display
  (case (* 2 3)
    ((2 3 5 7) "prime")
    ((1 4 6 8 9) "composite")))
(newline)
(display
  (case (car (list c d))
    ((a e i o u) "vowel")
    ((w y) "semivowel")
    (else "consonant")))
(newline)
```

{{out}}

```txt
composite
consonant
```



## Scilab

===if-then-else===
 '''if''' condition1 '''then''' instructions1
 ['''elseif''' condition2 '''then''' instructions2]
 ....
 ['''else''' instructionse]
 '''end'''

===select-case===
 '''select''' expression
   '''case''' expression1 '''then''' instructions1
   ['''case''' expression2 '''then''' instructions2]
   ...
   ['''else''' instructionse]
 '''end


## Seed7

===if-then-else===
There can be single or multiple statements.
An if-statement can have multiple elsif parts.

```seed7
if condition then
  statement
end if;

if condition then
  statement1
else
  statement2;
end if;

if condition1 then
  statement1
elsif condition2 then
  statement2;
end if;

if condition1 then
  statement1
elsif condition2 then
  statement2;
else
  statement3;
end if;
```



###  case


```seed7
case i of
  when {1, 4, 9}:  # Executed if i is 1, 4 or 9
    statement1;
  when {11} | {13 .. 17}:  # Executed if i is 11, 13, 14, 15, 16 or 17
    statement2;
  when {42}:  # Executed only if i is 42
    statement3;
  otherwise:
    statement4;
end case;
```



## SIMPOL

===if-else if-else===

```simpol
if x == 1
  foo()
else if x == 2
  bar()
else
  foobar()
end if
```



### ternary if function


```simpol
.if(x == 1, "hello", "world")
```



## Simula

{{works with|SIMULA-67}}
Simula 67 has conditional statements of the form:
  statement::=  '''if''' conditional_expression '''then''' statement '''else''' statement
    '''if''' X=Y '''then''' K:=I '''else''' K:=J
  statement::=  '''if''' conditional_expression '''then''' statement
    '''if''' X=Y '''then''' K:=I
An example:

```simula
BEGIN
  INTEGER i,j;
  i:=1; j:=2;
  OutText("i ");
  IF i=1 THEN OutInt(i,1);
  OutImage;
  OutInt(i,2); OutInt(j,2);
  IF i<j THEN OutText(" : i<j") ELSE OutText(" : i>=j");
  OutImage;
  IF i>=j THEN BEGIN
    OutText("i=");
    OutInt(i,5)
  END
  ELSE BEGIN
    OutText("j=");
    OutInt(j,5)
  END;
  OutImage
END
```

Simula 67 has also a switch structure:
  declaration::=  '''switch''' switch:=list_of labels
    statement::=  '''goto''' switch[expression]
An example:

```simula
BEGIN
  INTEGER i,j;
  SWITCH target:=L1,L2,L3;
  i:=1; j:=2;
  OutText("::");
  GOTO target(j);
  L1: OutText("AA");
  L2: OutText("BB");
  L3: OutText("CC");
  OutImage
END
```



## Slate


### ifTrue/ifFalse


```slate
"Conditionals in Slate are really messages sent to Boolean objects. Like Smalltalk. (But the compiler might optimize some cases)"
 balance > 0
     ifTrue: [inform: 'still sitting pretty!'.]
     ifFalse: [inform: 'No money till payday!'.].
```




### caseOf:otherwise:


```slate
c@(Net URLPathEncoder traits) convert
[ | byte1 byte2 byte3 digit1 digit2|
  [c in isAtEnd] whileFalse:
    [byte1: c in next.
     byte1 caseOf: {
       $+ -> [c out nextPut: $\s].
       $% -> [byte2: c in next.
              byte3: c in next.
              digit1: (byte2 toDigit: 16).
              digit2: (byte3 toDigit: 16).
              digit1 isNil \/ [digit2 isNil] ifTrue: [error: 'Error reading hex sequence after %'].
              c out nextPut: (digit1 * 16 + digit2 as: c out elementType)].
     } otherwise: [c out nextPut: byte1].
     ].
].
```



### whileTrue:/whileFalse:



```slate
[p isAtEnd] whileFalse: [p next evaluate]].
```



## Smalltalk


The pattern for handling a multi-option switch is to create classes for the various options, and let [[Polymorphism]] take care of the decisions.


### ifTrue/ifFalse


```smalltalk
"Conditionals in Smalltalk are really messages sent to Boolean objects"
 balance > 0
     ifTrue: [Transcript cr; show: 'still sitting pretty!'.]
     ifFalse: [Transcript cr; show: 'No money till payday!'.].
```


You can also use them as the ternary operator


```smalltalk>abs := x
 0 ifTrue: [ x ] ifFalse: [ x negated ]
```



## SNOBOL4

SNOBOL4 has no structured programming features, but the two constructs in question could be easily emulated with FAILURE/SUCCESS and indirect jumps


```snobol
	A = "true"
* "if-then-else"
if	A "true"			:s(goTrue)f(goFalse)
goTrue	output = "A is TRUE"		:(fi)
goFalse	output = "A is not TRUE"	:(fi)
fi

* "switch"
switch		A ("true" | "false") . switch	:s($("case" switch))f(default)
casetrue	output = "A is TRUE"	:(esac)
casefalse	output = "A is FALSE"	:(esac)
default		output = "A is neither FALSE nor TRUE"
esac
end
```



## SNUSP



```snusp
$==?\==zero=====!/==#
    \==non zero==/
```


'''?''' is the only conditional operator. It skips one character if the current cell is zero.

'''!''' is an unconditional skip.  '''!/''' is the idiom for joining two lines of execution.  '''?!''' inverts the test.

'''\''' and '''/''' redirect the flow of control.  All the other characters besides $ and # are commentary.



==[[Sparkling]]==

If statement:


```sparkling
var odd = 13;
if odd % 2 != 0 {
    print("odd");
}
```


If-else statement:


```sparkling
var odd = 13;
if odd % 2 != 0 {
    print("odd");
} else {
    print("even");
}
```


If and if-else statements can be chained:


```sparkling
var nodiv3 = 13;
if nodiv3 % 3 == 0 {
    print("divisible by 3");
} else if nodiv3 % 3 == 1 {
    print("gives 1 remainder");
} else {
    print("gives 2 remainder");
}
```


There's no "switch-case" statement in Sparkling yet, but it's work in progress.


## SQL

{{works with|MS SQL|2005}}

### Conditional Expression


```sql
case when a then b else c end

declare @n int
set @n=124
print case when @n=123 then 'equal' else 'not equal' end

--If/ElseIf expression
set @n=5
print case when @n=3 then 'Three' when @n=4 then 'Four' else 'Other' end
```



### If/Else


```sql
declare @n int
set @n=123
if @n=123
  BEGIN --begin/end needed if more than one statement inside
    print 'one two three'
  END
ELSE
  if @n=124 print 'one two four'
  else print 'other'
```



## SSEM

The SSEM's only conditional operation is <tt>011 Test</tt>, which causes the computer to skip the next instruction if the value held in the accumulator is negative. This program illustrates it: assuming address 10 stores a variable, we test whether its negation is negative (i.e. whether the variable itself is positive). If it is, we skip the next instruction and proceed with the program; but, if it is not negative (i.e. the variable is negative or zero), we jump to address 1 + the value stored at address 14. It is easy to see how this can be used to implement loops, other conditional tests, etc.

```ssem
01010000000000100000000000000000   -10 to c
00000000000000110000000000000000   Test
01110000000000000000000000000000   14 to CI
```



## Stata


###  cond function

This is an equivalent of a ternary ?: in C, useful for instance when creating a variable with '''[https://www.stata.com/help.cgi?generate gen]'''. See '''[https://www.stata.com/help.cgi?cond cond]''' in Stata help.


```stata
clear
set obs 4
gen a = cond(mod(_n, 2)==1, "A", "B")
list, noobs noheader

  +---+
  | A |
  | B |
  | A |
  | B |
  +---+
```



###  if command

This one is mainly useful in programs. See '''[https://www.stata.com/help.cgi?ifcmd ifcmd]''' in Stata help. To illustrate the command, here is a program that checks if a number is prime.


```stata
program isprime
	sca n = `0'
	sca p = 1
	if n<5 {
		if n!=2 & n!=3 {
			sca p = 0
		}
	}
	else {
		if mod(n, 2)==0 {
			sca p = 0
		}
		else {
			sca k=3
			while k*k<=n {
				if mod(n, k)==0 {
					sca p = 0
					continue, break
				}
				sca k = k+2
			}
		}
	}

	if p {
		di "`n' is prime."
	}
	else {
		di "`n' is not prime."
	}
end

isprime `=10^12-11'
999999999989 is prime.
```



###  if expression

When used in a command, '''[https://www.stata.com/help.cgi?if if]''' means the command is to be applied to the data subset for which the if expression is true.


```stata
clear
set obs 100
count
  100
count if mod(_n, 3)==0
  33
```



###  if statement in Mata

See [https://www.stata.com/help.cgi?%5bM-2%5d%20if Stata help]. Here is an equivalent of the above program to check if a number is prime.


```stata
function isprime(n) {
	if (n<5) return(n==2 | n==3)
	else if (mod(n, 2)==0) return(0)
	else {
		for (k=3; k*k<=n; k=k+2) {
			if (mod(n, k)==0) return(0)
		}
		return(1)
	}
}

isprime(10^12-11)
  1
```



###  ternary operator in Mata


See [https://www.stata.com/help.cgi?m2_op_conditional Stata help]. Here is a recursive implementation of the Fibonacci sequence, to illustrate.


```stata
function fib(n) {
	return(n<2 ? n : fib(n-1)+fib(n-2))
}

fib(10)
  55
```



## Tcl


===if-then-else===

```tcl
if {$foo == 3} {
    puts "foo is three"
} elseif {$foo == 4} {
    puts "foo is four"
} else {
    puts "foo is neither three nor four"
}
```

or (using the ternary operator of expressions)

```tcl
set result [expr { $foo == 3 ? "three" : "not three" }]
```



### switch


```tcl
switch -- $foo {
    3 {puts "foo is three"}
    4 {puts "foo is four"}
    default {puts "foo is something else"}
}
```

Note that the <tt>switch</tt> command can also use glob matching (like <tt>case</tt> in the Bourne Shell) or regular-expression matching.


## Tern


There are several conditional statements.


### If Statement


```tern
if(a > b)
   println(a);
```



### If Else Statement


```tern
if(a > b) {
   println(a);
} else {
   println(b);
}
```



### Unless Statement


```tern
unless(a > b) {
   println(b);
} else {
   println(a);
}
```



### Switch Statement


```tern
switch(a) {
   case 10:
   case 11:
      println(a);
      break;
   default:
      println(b);
}
```


=={{header|TI-83 BASIC}}==
There are 3 forms of conditional statement:



'''Basic form'''

 with only one statement for the true part:

```ti83b
If condition
statement
```

or in one line

```ti83b>If condition : statement</lang


'''If-Then form'''

```ti83b
If condition
Then
statements
End
```


'''If-Then-Else form'''

```ti83b
If condition
Then
statements
Else
statements
End
```



## Toka



### ifTrue

( condition ) ( quote ) ifTrue


```toka
100 100 = [ ." True\n" ] ifTrue
100 200 = [ ." True\n" ] ifTrue
```



### ifFalse

( condition ) ( quote ) ifFalse


```toka
100 100 = [ ." True\n" ] ifFalse
100 200 = [ ." True\n" ] ifFalse
```



### ifTrueFalse

( condition ) ( true quote ) ( false quote ) ifTrueFalse


```toka
100 100 = [ ." Equal\n" ] [ ." Not Equal\n" ] ifTrueFalse
100 200 = [ ." Equal\n" ] [ ." Not Equal\n" ] ifTrueFalse
```



## TorqueScript


===if-then-else===


```tqs
// numbers and objects
if(%num == 1)
{
	foo();
}
else if(%obj == MyObject.getID())
{
	bar();
}
else
{
	deusEx();
}

// strings
if(%str $= "Hello World")
{
	foo();
}
else if(%str $= "Bye World")
{
	bar();
}
else
{
	deusEx();
}
```



### switch



```tqs
// numbers and objects
switch(%num)
{
	case 1:
		one();
	case 2:
		twoThreeOrFour();
	case 3:
		twoThreeOrFour();
	case 4:
		twoThreeOrFour();
	case 5:
		five();
	case MyObject.getID():
		anObject();
	default:
		everythingElse();
}

// strings
switch$(%str)
{
	case "Hello":
		arrival();
	case "Goodbye":
		departure();
	default:
		somethingElse();
}
```


===conditional (ternary) operator (?:)===


```tqs
%formatted = %str @ ((getSubStr(%str,strLen(%str) - 1,1) $= "s") ? "'" : "'s");
```



## Trith


### branch


```trith
true ["yes" print] ["no" print] branch
```


### when


```trith
true ["yes" print] when
```


### unless


```trith
false ["no" print] unless
```




## TUSCRIPT


### IF ELSEIF ELSE ENDIF


```tuscript

$$ MODE TUSCRIPT

condition="c"
IF (condition=="a") THEN
   ---> do something
ELSEIF (condition=="b") THEN
   ---> do something
ELSE
   ---> do something
ENDIF

```


### SELECT CASE DEFAULT ENDSELECT


```tuscript

$$ MODE TUSCRIPT

days="Monday'Tuesday'Wednesday'Thursday'Friday'Saturday'Sunday"
dayofweek=DATE (today,day,month,year,number)
day=SELECT (days,#dayofweek)

SELECT day
CASE "Monday"
   ---> do something
CASE "Saturday","Sunday"
   ---> do something
DEFAULT
   ---> do something
ENDSELECT

```



## TXR


In TXR, most directives are conditionals, because they specify some kind of match. Given some directive D, the underlying logic in the language is, roughtly, "if D does not match at the current position in the input, then fail, otherwise the input advances according to the semantics of D".

An easy analogy to regular expressions may be drawn. The regex /abc/ means something like "if a doesn't match, then fail, otherwise consume a character and if b doesn't match, then fail, otherwise consume another character and if c doesn't match, then fail otherwise consume another character and succeed." The expressive power comes from, in part, not having to write all these decisions and book-keeping.

The interesting conditional-like structures in TXR are the parallel directives, which apply separate clauses to the same input, and then integrate the results in various ways.

For instance the <code>choose</code> construct will select, from among those clauses which match successfully, the one which maximizes or minimizes the length of an extracted variable binding:


```txr

@(choose :shortest x)
@x:@y
@(or)
@x<--@y
@(or)
@x+@y
@(end)
```


Suppose the input is something which can match all three patterns in different ways:


```txt
foo<--bar:baz+xyzzy
```


The outcome (with <code>txr -B</code>) will be:


```txt
x="foo"
y="bar:baz+xyzzy"
```


because this match minimizes the length of <code>x</code>. If we change this to <code>:longest x</code>, we get:


```txt
x="foo<--bar:baz"
y="xyzzy"
```


The <code>cases</code>, <code>all</code> and <code>none</code> directives most resemble control structures because they have short-circuiting behavior.
For instance:


```txr
@(all)
@x:y@
@z<-@w
@(and)
@(output)
We have a match: (x, y, z, w) = (@x, @y, @z, @w).
@(end)
@(end)
```


If any subclause fails to match, then <code>all</code> stops processing subsequent clauses. There are subtleties though, because an earlier clause can produce variable bindings which are visible to later clauses.
If previously bound variable is bound again, it must be to an identical piece of text:


```txr
@# match a line which contains some piece of text x
@# after the rightmost occurence of : such that the same piece
@# of text also occurs at the start of the line preceded by -->
@(all)
@*junk:@x
@(and)
-->@x@/.*/
@(end)
```



```txt
$ echo "-->asdfhjig:asdf" | txr -B weird.txr -
junk="-->asdfhjig"
x="asdf"
$ echo "-->assfhjig:asdf" | txr -B weird.txr -
false
$
```



## UNIX Shell

{{works with|Bourne Shell}}


### = If conditionals =


The basic syntax is <code>if ''command-list''; then ''command-list''; fi</code>. If the first command list succeeds (by returning 0 for success), then the shell runs the second command list.


```sh
if test 3 -lt 5; then echo '3 is less than 5'; fi
```



### = Else and elif =


There are optional <code>elif</code> (else if) and <code>else</code> clauses.


```sh
if test 4 -ge 6; then
	echo '4 is greater than or equal to 6'
elif test 4 -lt 6; then
	echo '4 is less than 6'
else
	echo '4 compares not to 6'
fi
```



### = Switch conditionals =


The Unix shell provides support for multibranch switch conditional constructs using the case statement:


```sh
case value in
  choicea)
    foo
    ;;
  choiceb)
    bar
    ;;
esac
```



### = Conditional branching using operators =


One can also use <code>&&</code> and <code>||</code> as conditional structures; see [[short-circuit evaluation#UNIX Shell]].


```sh
test 3 -lt 5 && echo '3 is less than 5'
test 4 -ge 6 || echo '4 is not greater than or equal to 6'
```



### = Conditional loops =


The Unix shell also supports conditional loops:


```sh
# This is a while loop
l=1
while [ l -le 5 ]; do
  echo $l
done

# This is an until loop
l=1
until [ l -eq 5 ]; do
  echo $l
done
```


=
## C Shell
=
The single-line <code>if</code> syntax is <code>if (''expression'') ''simple-command''</code>.


```csh
if (3 < 5) echo '3 is less than 5'
if ({ grep -q ^root: /etc/passwd }) echo 'passwd has root'
```


The multi-line <code>if</code> syntax has a <code>then</code> clause, and can have optional <code>else if</code> and <code>else</code> clauses. Each clause may contain multiple commands.


```csh
if (4 >= 6) then
	echo '4 is greater than or equal to 6'
else if (4 < 6) then
	echo '4 is less than 6'
else
	echo '4 compares not to 6'
endif
```



## V


### ifThenElse


```v
[true]
  ['is true' puts]
  ['is false' puts]
ifte

=is true
```



### ifThen


```v
[true]
  ['is true' puts]
if
=is true
```



### When


```v
3 [
  [1 =] [1 *]
  [2 =] [10 *]
  [3 =] [100 *]
  [4 =] [1000 *]
] when

=300
```


### Choice


```v
true
  1 2
choice

=1

false
  1 2
choice

=2
```



## VBA


### If Else End If


```vb

Sub C_S_If()
Dim A$, B$

    A = "Hello"
    B = "World"
    'test
    If A = B Then Debug.Print A & " = " & B
    'other syntax
    If A = B Then
        Debug.Print A & " = " & B
    Else
        Debug.Print A & " and " & B & " are differents."
    End If
    'other syntax
    If A = B Then
        Debug.Print A & " = " & B
    Else: Debug.Print A & " and " & B & " are differents."
    End If
    'other syntax
    If A = B Then Debug.Print A & " = " & B _
    Else Debug.Print A & " and " & B & " are differents."
    'other syntax
    If A = B Then Debug.Print A & " = " & B Else Debug.Print A & " and " & B & " are differents."
    If A = B Then Debug.Print A & " = " & B Else: Debug.Print A & " and " & B & " are differents."
End Sub
```



### If ElseIf Else End If


```vb
Sub C_S_ElseIf()
Dim A$, B$

    A = "Hello"
    B = "World"
    'test
    If A = B Then Debug.Print A & " = " & B
    'other syntax
    If A = B Then
        Debug.Print A & " = " & B
    ElseIf A > B Then
        Debug.Print A & " > " & B
    Else
        Debug.Print A & " < " & B
    End If
End Sub
```


### Select Case


```vb
Sub C_S_Select_Case()
'With Strings
Dim A$, C&

    A = "Hello"
    Select Case A
        Case "World"
            Debug.Print "A = World"
        Case "Hello"
            Debug.Print "A = Hello"
        Case Else
            Debug.Print "You make a mistake"
    End Select
'With numerics
    C = 11
    Select Case C
        Case Is <= 10
            Debug.Print "C <= 10"
        Case Is < 20, Is > 10
            Debug.Print "10 < C < 20"
        Case Is >= 20
            Debug.Print "C >= 20"
    End Select
'Select Case Boolean
    'With Strings
    Select Case False
        Case A <> "Hello"
            Debug.Print "A = Hello"
        Case A Like "*orl*"
            Debug.Print "A Not Like *orl*"
        Case Else
            Debug.Print "You make a mistake"
    End Select                  'return : "A = Hello"
    'Other conditions's order
    Select Case False
        Case A Like "*orl*"
            Debug.Print "A Not Like *orl*"
        Case A <> "Hello"
            Debug.Print "A = Hello"
        Case Else
            Debug.Print "You make a mistake"
    End Select                  'return : "A Not Like *orl*"
    'With numerics
    Select Case True
        Case C <= 10
            Debug.Print "C <= 10"
        Case C < 20, C > 10
            Debug.Print "10 < C < 20"
        Case C >= 20
            Debug.Print "C >= 20"
    End Select
End Sub
```


### Inline IF


```vb
Sub C_S_IIF()
    Dim myName
    myName = 2
    Debug.Print IIf(myName = 1, "Bryan", "Justin")
    'return : Justin
End Sub

```


### Switch


```vb
Sub C_S_Switch()
    Dim myName
    myName = 2
    Debug.Print Switch(myName = 1, "Bryan", myName = 2, "Justin", myName = 3, "John")
    'return : Justin
End Sub

```



## VBScript

===if-then-else===
Block form:

```vb
If condition1 Then
     statement
End If

If condition1 Then
    statement
ElseIf condition2 Then
    statement
...
ElseIf conditionN Then
    statement
Else
    statement
End If

```

Line form:

```vb
If condition Then statement

If condition Then statement Else statement
```

===select-case===

```vb
Select Case Expression
    Case Value1: statement
    Case Value2: statement
    ...
    Case ValueN: statement
    Case Else:   statement
End Select

Select Case Expression
    Case Value1
        statements
    Case Value2
        statements
    ...
    Case ValueN
        statements
    Case Else
        statements
End Select
```



## Verbexx


```verbexx
@VAR a b = 1 2;

// -------------------------------------------------------------------------------------
//  @IF verb  (returns 0u0 = UNIT, if no then: or else: block is executed)
//
### ==
  (note: both then: and else: keywords are optional)

@SAY "@IF 1   " ( @IF (a > b) then:{"then:"} else:{"else:"} );
@SAY "@IF 2   " ( @IF (b > a) else:{"else:"} then:{"then:"} );
@SAY "@IF 3   " ( @IF (a > b) then:{"then:"}                );
@SAY "@IF 4   " ( @IF (b > a) then:{"then:"}                );
@SAY "@IF 5   " ( @IF (a > b) else:{"else:"}                );
@SAY "@IF 6   " ( @IF (b > a) else:{"else:"}                );
@SAY "@IF 7   " ( @IF (b > a)                               );

//  ---------------------------------------------------------------------------------
//  ? verb (conditional operator)
//  ====== ( 1st block (TRUE) is required, 2nd block (FALSE) is optional)

@SAY "? 1     " ( (a < b) ? {"1st"} {"2nd"} );
@SAY "? 2     " ( (a > b) ? {"1st"} {"2nd"} );
@SAY "? 3     " ( (a < b) ? {"1st"}         );
@SAY "? 4     " ( (a > b) ? {"1st"}         );

// -----------------------------------------------------------------------------------
// @CASE verb
//
### ====

//
//  - executes code block for first when: condition that evaluates to TRUE
//
//  - normally, ends after running that code block
//
//  - if no when: conditions are true, executes else: code block (if present)
//
//  - can exit a when: block with @CONTINUE case: verb -- causes @CASE to continue
//    looking for more true when: blocks or the else: block

@VAR n = 0;
@LOOP times:3
{
  @SAY ( "n =" n "        @CASE results:"
         ( @CASE
             when:(n == 0) { "n == 0(1)"                   }
             when:(n == 0) { "n == 0(2)"                   }
             when:(n == 1) { "n == 1(1)"; @CONTINUE case:  }
             when:(n == 1) { "n == 1(2c)"                  }
             else:         { "else"                        }
         )
       )
  ;
  n++;
};

/] -----------------------------------------------------------------------

Output:

@IF 1    else:
@IF 2    then:
@IF 3    0_u0
@IF 4    then:
@IF 5    else:
@IF 6    0_u0
@IF 7    0_u0
? 1      1st
? 2      2nd
? 3      1st
? 4      0_u0
n = 0         @CASE results: n == 0(1)
n = 1         @CASE results: n == 1(2c)
n = 2         @CASE results: else
```



## Visual Basic

===if-then-else===

### =Block form=


```vb
If condition Then
     statement
End If

If condition Then
    statement
Else
    statement
End If

If condition1 Then
    statement
ElseIf condition2 Then
    statement
...
ElseIf conditionN Then
    statement
Else
    statement
End If
```



### =Line form=


```vb
If condition Then statement

If condition Then statement Else statement
```


===select-case===

```vb
Select Case Expression
    Case Value1: statement
    Case Value2: statement
    ...
    Case ValueN: statement
    Case Else:   statement
End Select

Select Case Expression
    Case Value1
        statements
    Case Value2
        statements
    ...
    Case ValueN
        statements
    Case Else
        statements
End Select

```

===inline if-then-else===

```vb
IIf(expr, then-value, else-value)
```

Example:

```vbnet
    myName = 2
    Debug.Print IIf(myName = 1, "John", "Jack")
    'return : "Jack")
```



### inline switch


```vb
Switch(expr-1, value-1[, expr-2, value-2 … [, expr-n,value-n]])
```

Example:

```vb
    myName = 2
    Debug.Print Switch(myName = 1, "James", myName = 2, "Jacob", myName = 3, "Jeremy")
    'return : "Jacob"
```



## Visual Basic .NET


===if-then-else===
''Basic''

```vbnet
Dim result As String, a As String = "pants", b As String = "glasses"

If a = b Then
  result = "passed"
Else
  result = "failed"
End If
```


''Condensed''

```vbnet
Dim result As String, a As String = "pants", b As String = "glasses"

If a = b Then result = "passed" Else result = "failed"

If a = b Then
  result = "passed"
Else : result = "failed"
End If

If a = b Then : result = "passed"
Else
  result = "failed"
End If
```


===if-then-elseif===

```vbnet
Dim result As String, a As String = "pants", b As String = "glasses"

If a = b Then
  result = "passed"
ElseIf a <> b Then
  result = "failed"
Else
  result = "impossible"
End If
```


===select-case-else===

```vbnet
Dim result As String, a As String = "pants", b As String = "glasses"

Select Case a
  Case b
    result = "match"
  Case a : result = "duh"
  Case Else
    result = "impossible"
End Select
```


===inline-conditional===

```vbnet
Imports Microsoft.VisualBasic

...

Dim result As String = CType(IIf("pants" = "glasses", "passed", "failed"), String) 'VB 1-8

Dim result As String = If("pants" = "glasses", "passed", "failed") 'VB 9
```


===generic-inline-conditional===
{{works with|Visual Basic .NET|8.0}}

```vbnet
Imports Microsoft.VisualBasic

...

Function IIf2(Of T)(ByVal condition As Boolean, ByVal truepart As T, ByVal falsepart As T) As T
  If condition Then Return truepart Else Return falsepart
End Function

...

Dim result As String = IIf2("pants" = "glasses", "passed", "failed") ' type is inferred
```


===generic-inline-conditional===
'''Language Version:''' 9.0+


```vbnet
Dim result As String = If("pants" = "glasses", "passed", "failed") ' type is inferred
```



## Vorpal


===if-then-else===

```vorpal
if(condition){
   result = 'met'
}
else{
   result = 'not met'
}
```



## Wrapl



### simple conditional

Conditionals in Wrapl are expressions. Either success or failure can be omitted from the expression.

```wrapl>condition =
 success // failure
condition => success
condition // failure
```



### goal directed evaluation

Wrapl's goal directed evaluation can be used to control conditional execution.
The select-right operator <tt>&</tt> produces the values of the right operand for each value produced by the left operand. Thus if the left operand fails to produce any values, the right operand is never evaluated.

```wrapl
condition & success
```

The sequence operator <tt>|</tt> produces the values of the left operand followed by the values of the right operand. Thus if the left operand produces enough values (for example in a context where only one value is required), the right operand is never evaluated.

```wrapl
condition | failure
```



## X86 Assembly



### ifs/elseifs/elses

Assembly doesn't work on if/else if/else statements(Unless you're using MASM or alike assemblers:)). Rather, it has conditional jumps which work off flags set by the comparison. Take this general statement from C.

```c

if(i>1)
   DoSomething

FailedSoContinueCodeExecution.

```

There are actually a number of ways to implement that in assembly. The most typical way would be something like..

```asm

cmp i, 1
jg _DoSomething
FailedSoContinueCodeExecution

```

Using the "jg" instruction,our code will jump to _DoSomething if the comparison(cmp i,1) made our ZF(ZeroFlag) flag well, zero. Which means only 1 thing. It is in fact greater than. In contrast, if i is in fact equal or less than 1, ZF is set to 1. The Zero Flag will remain set as long as we don't use any instructions that alter flags(comparisons for example). So, here's another C example

```c

if(i>1)
   DoSomething
else if(i<=1)
   DoSomethingElse
FailedSoContinueCodeExecution

```

In this case, we can use our previous example as a skeleton.

```asm

cmp i, 1
jg _DoSomething
jle _DoSomethingElse
FailedSoContinueCodeExecution

```

This does another state check on the Zero flag(actually jg/jle also check another flag, but that's not overly important) using jle. JumpifLessthanorEqual. Essentially, jle jumps if ZG is set to 1. So, it's jump condition is the opposite to jg.



One last commonly used condition.

```c

if(i==1)
   DoSomething
else
   DoSomethingElse
FailedSoContinueExecution

```


In this case, we'd do this.

```asm

cmp i, 1
je _DoSomething
jne _DoSomethingElse
FailedSoContinueExecution

```

The je/jne jump instructions are again like jg/jle opposites of each other and again like je/jne rely on how the zero flag is set in the previous comparison.

There are many different conditional jumps in assembly and many ways to set them, test, and, or to name a few. The ones covered are just some commonly used ones in order to show how assembly deals with conditional statements.


## XLISP


### If

An <code>IF</code> expression has the form <code>(IF <condition> <then-clause> <opt-else-clause>)</code>, for example:

```lisp
(if (eq s "Rosetta Code")
    "The well-known programming chrestomathy site"
    "Some other website, maybe, I dunno" )
```

If the condition evaluates to anything except <code>NIL</code> or the empty list (which are equivalent), it is counted as true and the whole expression evaluates to the value of the <i>then</i> clause; otherwise it evaluates to the value of the optional <i>else</i> clause, if one is provided, or else to the empty list.


### Case

<code>CASE</code> expressions resemble the multi-way branching constructs found in most programming languages: an expression is evaluated, and the value of the whole expression is provided by the first clause that evaluates to a true value. Optionally, an <code>ELSE</code> expression can be provided, in case none of the clauses fits.

```lisp
(case s
    ("Rosetta Code" "Ah yes, the chrestomathy site")
    ("Stack Overflow" "Oh dear me, having problems are you?")
    ("Github" "Say no more")
    (else "Sorry, never heard of it") )
```



### Cond

<code>COND</code> is a more general conditional than <code>IF</code> or <code>CASE</code>: it resembles a <code>CASE</code> statement, but with the option of using a different conditional expression in each clause. A default value can be provided using <code>ELSE</code>, as with <code>CASE</code>, or any expression that is guaranteed to return a value other than <code>NIL</code> or the empty list.

```lisp
(cond
   ((eq s "Rosetta Code") "Chrestomathy site")
   ((> n 37) "Some other appropriate value, presumably")
   (t "If you're seeing me, s wasn't equal to Rosetta Code and n must have been 37 or below") )
```



## XPL0


```txt

     if BOOLEAN EXPRESSION then STATEMENT
     if BOOLEAN EXPRESSION then STATEMENT else STATEMENT
     if BOOLEAN EXPRESSION then EXPRESSION else EXPRESSION
     case INTEGER EXPRESSION of
             INTEGER EXPRESSION, ... INTEGER EXPRESSION: STATEMENT;
             ...
             INTEGER EXPRESSION, ... INTEGER EXPRESSION: STATEMENT
             other STATEMENT
     case of
             BOOLEAN EXPRESSION, ... BOOLEAN EXPRESSION: STATEMENT;
             ...
             BOOLEAN EXPRESSION, ... BOOLEAN EXPRESSION: STATEMENT
             other STATEMENT

```



## XSLT

The <xsl:if> element allows simple conditional processing.

```xml
<xsl:if test="condition">
  <!-- executed if XPath expression evaluates to true -->
</xsl:if>
```

The <xsl:choose>, <xsl:when>, and <xsl:otherwise> elements allow more general conditional processing.

```xml><xsl:choose

  <xsl:when test="condition1">
    <!-- included if condition1 evaluates to true (like C `if`) -->
  </xsl:when>
  <xsl:when test="condition2">
    <!-- included if all previous conditions evaluated to false and
         condition2 evaluates to true (like C `else if`) -->
  </xsl:when>
  <--
    ...
  -->
  <xsl:otherwise>
    <!-- included if all previous conditions evaluated to false
         (like C `else`) -->
    <!-- (The `otherwise` element is optional) -->
  </xsl:otherwise>
</xsl:choose>
```


The XPath expressions in the <code>test</code> attribute are evaluated in boolean context (converted as if by [http://www.w3.org/TR/xpath/#function-boolean the boolean function] if necessary).


```xml
<xsl:if test="@attrib = 'foo'">...</xsl:if>
<xsl:if test="position() != last()">...</xsl:if>
<xsl:if test="not(false())">...</xsl:if>

<!-- Some XPath expressions must be escaped. -->
<xsl:if test='contains(node, "stuff") and (position() &gt; first())'>...</xsl:if>

<!-- The following two examples are synonymous because the test attribute is
     implicitly converted to boolean. -->
<xsl:if test="boolean($expr)">...</xsl:if>
<xsl:if test="$expr">...</xsl:if>
```



## zkl


```zkl
if (x) y else z;
if(a)b else if (c) else d; etc
x:=(if (a) b else c);

a and b or c  // usually the same as if(a) b else c, beware if b evals to False

switch(x){
  case(1){...}
  case("2"){...}  // matches anything
  case(a)[fallthrough]{...}  // no break, no break has to be explicit
  case(b){...}
  else {...} // case a C's default, has to be at the end
}
```


{{omit from|GUISS}}

[[Category:Branches]]

{{omit from|GUISS}}

[[Category:Branches]]
