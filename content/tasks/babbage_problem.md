+++
title = "Babbage problem"
description = ""
date = 2019-10-03T00:42:38Z
aliases = []
[extra]
id = 20756
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "aime",
  "algol_68",
  "apl",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "autohotkey",
  "awk",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cobol",
  "commodore_basic",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "dafny",
  "dart",
  "dyalect",
  "easylang",
  "edsac_order_code",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "fsharp",
  "futurebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "limbo",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxscript",
  "microsoft_small_basic",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "ol",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pilot",
  "powershell",
  "processing",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sequencel",
  "shen",
  "sidef",
  "simula",
  "smalltalk",
  "swift",
  "tcl",
  "unix_shell",
  "utfool",
  "vax_assembly",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "x86_assembly",
  "xlisp",
  "zkl",
]
+++

## Task
![Charles Babbage](Babbage.jpg)
![Charles Babbage's analytical engine](Babbage_engine.jpg)

[[wp:Charles_Babbage|Charles Babbage]], looking ahead to the sorts of problems his Analytical Engine would be able to solve, gave this example:
{{quote
 | What is the smallest positive integer whose square ends in the digits 269,696?
 | Babbage, letter to Lord Bowden, 1837; see Hollingdale and Tootill, <i>Electronic Computers</i>, second edition, 1970, p. 125.
}}
He thought the answer might be 99,736, whose square is 9,947,269,696; but he couldn't be certain.


{{task heading}}

The task is to find out if Babbage had the right answer — and to do so, as far as your language allows it, in code that Babbage himself would have been able to read and understand.
As Babbage evidently solved the task with pencil and paper, a similar efficient solution is preferred.

For these purposes, Charles Babbage may be taken to be an intelligent person, familiar with mathematics and with the idea of a computer; he has written the first drafts of simple computer programmes in tabular form. [[https://collection.sciencemuseum.org.uk/documents/aa110000020 Babbage Archive Series L]].


{{task heading|Motivation}}

The aim of the task is to write a program that is sufficiently clear and well-documented for such a person to be able to read it and be confident that it does indeed solve the specified problem.





## 11l


```11l
V n = 1
L n ^ 2 % 1000000 != 269696
   n++
print(n)
```



## 360 Assembly

An assembler program always seems a bit tricky for non system engineer because it deals directly with the operating system and with the hardware instructions. Here we have a 32-bit computer with 16 32-bit registers. The caller (the operating system to keep it simple) is calling you giving your location address stored in register-15 and has stored in register-14 his return address. To save each program context, register-13 points to a 18 word save area. Do not spend time in understanding the context saving and restoring in the prologue and epilogue part of the program. What you have to know, “360” architecture uses 32-bit signed binary arithmetic, so here the maximum integer value is 2^31-1 (2147483647). Therefore the solution must be less than 2147483647. The multiplication and the division use a pair of registers; coding “MR 4,2” means multiply register-5 by register-2 and place result in the (register-4,register-5) pair; the same way “DR 4,2” means divide the (register-4,register-5) pair by register-2 and place the quotient in register-5 and the reminder in register-4. We use in the below program this intermediate 64-bit integers to find a solution with a value up to 2^31-1 even when we have to compute the square of this value.

```360asm

*        Find the lowest positive integer whose square ends in 269696
*        The logic of the assembler program is simple :
*        loop for i=524 step 2
*          if (i*i modulo 1000000)=269696 then leave loop
*        next i
*        output 'Solution is: i=' i '  (i*i=' i*i ')'
BABBAGE  CSECT                     beginning of the control section
         USING BABBAGE,13          define the base register
         B     72(15)              skip savearea (72=18*4)
         DC    17F'0'              savearea (18 full words (17+1))
         STM   14,12,12(13)        prolog: save the caller registers
         ST    13,4(15)            prolog: link backwards
         ST    15,8(13)            prolog: link forwards
         LR    13,15               prolog: establish addressability
         LA    6,524               let register6 be i and load 524
LOOP     LR    5,6                 load register5 with i
         MR    4,6                 multiply register5 with i
         LR    7,5                 load register7 with the result i*i
         D     4,=F'1000000'       divide register5 with 1000000
         C     4,=F'269696'        compare the reminder with 269696
         BE    ENDLOOP             if equal branch to ENDLOOP
         LA    6,2(6)              load register6 (i) with value i+2
         B     LOOP                branch to LOOP
ENDLOOP  XDECO 6,BUFFER+15         edit registrer6 (i)
         XDECO 7,BUFFER+34         edit registrer7 (i squared)
         XPRNT BUFFER,L'BUFFER     print buffer
         L     13,4(0,13)          epilog: restore the caller savearea
         LM    14,12,12(13)        epilog: restore the caller registers
         XR    15,15               epilog: set return code to 0
         BR    14                  epilog: branch to caller
BUFFER   DC    CL80'Solution is: i=............  (i*i=............)'
         END   BABBAGE             end of the control section

```

{{out}}

```txt

Solution is: i=       25264  (i*i=   638269696)

```




## Ada



```Ada
-- The program is written in the programming language Ada. The name "Ada"
-- has been chosen in honour of your friend,
--      Augusta Ada King-Noel, Countess of Lovelace (née Byron).
--
-- This is an program to search for the smallest integer X, such that
-- (X*X) mod 1_000_000 = 269_696.
--
-- In the Ada language, "*" represents the multiplication symbol, "mod" the
-- modulo reduction, and the underscore "_" after every third digit in
-- literals is supposed to simplify reading numbers for humans.
-- Everything written after "--" in a line is a comment for the human,
-- and will be ignored by the computer.

with Ada.Text_IO;
-- We need this to tell the computer how it will later output its result.

procedure Babbage_Problem is

   -- We know that 99_736*99_736 is 9_947_269_696. This implies:
   -- 1. The smallest X with X*X mod 1_000_000 = 269_696 is at most 99_736.
   -- 2. The largest square X*X, which the program may have to deal with,
   --    will be at most 9_947_269_69.

   type Number is range 1 .. 99_736*99_736;
   X: Number := 1;
   -- X can store numbers between 1 and 99_736*99_736. Computations
   -- involving X can handle intermediate results in that range.
   -- Initially the value stored at X is 1.
   -- When running the program, the value will become 2, 3, 4, etc.

begin
   -- The program starts running.

   -- The computer first squares X, then it truncates the square, such
   -- that the result is a six-digit number.
   -- Finally, the computer checks if this number is 269_696.
   while not (((X*X) mod 1_000_000) = 269_696) loop

      -- When the computer goes here, the number was not 269_696.
      X := X+1;
      -- So we replace X by X+1, and then go back and try again.

   end loop;

   -- When the computer eventually goes here, the number is 269_696.
   -- E.e., the value stored at X is the value we are searching for.
   -- We still have to print out this value.

   Ada.Text_IO.Put_Line(Number'Image(X));
   -- Number'Image(X) converts the value stored at X into a string of
   -- printable characters (more specifically, of digits).
   -- Ada.Text_IO.Put_Line(...) prints this string, for humans to read.
   -- I did already run the program, and it did print out 25264.
end Babbage_Problem;
```



## ALGOL 68

As with other samples, we use "simple" forms such as "a := a + 1" instead of "a +:= 1".

```algol68
COMMENT text between pairs of words 'comment' in capitals are
        for the human reader's information and are ignored by the machine
COMMENT

COMMENT Define s to be the integer value 269 696              COMMENT
INT s = 269 696;

COMMENT Name a location in the machine's storage area that will be
        used to hold integer values.
        The value stored in the location will change during the
        calculations.
        Note, "*" is used to represent the multiplication operator.
              ":=" causes the location named to the left of ":=" to
                   assume the value computed by the expression to the right.
              "sqrt" computes an approximation to the square root
                     of the supplied parameter
              "MOD" is an operator that computes the modulus of its
                    left operand with respect to its right operand
              "ENTIER" is a unary operator that yields the largest
                      integer that is at most its operand.
COMMENT
INT v := ENTIER sqrt( s );

COMMENT the construct: WHILE...DO...OD repeatedly executes the
        instructions between DO and OD, the execution stops when
        the instructions between WHILE and DO yield the value FALSE.
COMMENT
WHILE ( v * v ) MOD 1 000 000 /= s DO v := v + 1 OD;

COMMENT print displays the values of its parameters
COMMENT
print( ( v, " when squared is: ", v * v, newline ) )
```

{{out}}

```txt

     +25264 when squared is:  +638269696

```



## Aime


```aime
integer i;

i = sqrt(269696);
while (i * i % 1000000 != 269696) {
    i += 1;
}

o_(i, "\n");
```



## APL

If at all possible, I would sit down at a terminal <i>with</i> Babbage and invite him to experiment with the various functions used in the program.

```apl
      ⍝ We know that 99,736 is a valid answer, so we only need to test the positive integers from 1 up to there:
      N←⍳99736
      ⍝ The SQUARE OF omega is omega times omega:
      SQUAREOF←{⍵×⍵}
      ⍝ To say that alpha ENDS IN the six-digit number omega means that alpha divided by 1,000,000 leaves remainder omega:
      ENDSIN←{(1000000|⍺)=⍵}
      ⍝ The SMALLEST number WHERE some condition is met is found by taking the first number from a list of attempts, after rearranging the list so that numbers satisfying the condition come before those that fail to satisfy it:
      SMALLESTWHERE←{1↑⍒⍵}
      ⍝ We can now ask the computer for the answer:
      SMALLESTWHERE (SQUAREOF N) ENDSIN 269696
```

{{out}}

```txt
25264
```



## AppleScript


AppleScript's number types are at their limits here, but we can just get to the first Babbage number, after 638 integer root tests on suffixed numbers:


```AppleScript
-- BABBAGE -------------------------------------------------------------------

-- babbage :: Int -> [Int]
on babbage(intTests)

    script test
        on toSquare(x)
            (x * 1000000) + 269696
        end toSquare

        on |λ|(x)
            hasIntRoot(toSquare(x))
        end |λ|
    end script

    script toRoot
        on |λ|(x)
            ((x * 1000000) + 269696) ^ (1 / 2)
        end |λ|
    end script

    set xs to filter(test, enumFromTo(1, intTests))
    zip(map(toRoot, xs), map(test's toSquare, xs))
end babbage

-- TEST ----------------------------------------------------------------------
on run
    -- Try 1000 candidates

    unlines(map(curry(intercalate)'s |λ|("  ->  "), babbage(1000)))

    --> "2.5264E+4 -> 6.38269696E+8"
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
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

-- hasIntRoot :: Int -> Bool
on hasIntRoot(n)
    set r to n ^ 0.5
    r = (r as integer)
end hasIntRoot

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

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

```txt
2.5264E+4  ->  6.38269696E+8
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program babbage.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:           .ascii "Result = "
sMessValeur:           .fill 11, 1, ' '            @ size => 11
szCarriageReturn:      .asciz "\n"

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program

    ldr r4,iNbStart                               @ start number = 269696
    mov r5,#0                                     @ counter multiply
    ldr r2,iNbMult                                @ value multiply = 1 000 000
    mov r6,r4
1:
    mov r0,r6
    bl squareRoot                                 @ compute square root
    umull r1,r3,r0,r0
    cmp r3,#0                                     @ overflow ?
    bne 100f                                      @ yes -> end
    cmp r1,r6                                     @ perfect square
    bne 2f                                        @ no -> loop
    ldr r1,iAdrsMessValeur
    bl conversion10                               @ call conversion decimal
    ldr r0,iAdrsMessResult
    bl affichageMess                              @ display message
    b 100f                                        @ end
2:
    add r5,#1                                     @ increment counter
    mul r3,r5,r2                                  @ multiply by 1 000 000
    add r6,r3,r4                                  @ add start number
    b 1b

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iNbStart:                 .int 269696
iNbMult:                  .int 1000000
/******************************************************************/
/*     compute squareRoot                                       */
/******************************************************************/
/* r0 contains n          */
/* r0 return result or -1 */
squareRoot:
    push {r1-r5,lr}                     @ save  registers
    cmp r0,#0
    beq 100f                            @ if zero -> end
    movlt r0,#-1                        @ if negatif return - 1
    blt 100f
    cmp r0,#4                           @ if <  4 return 1
    movlt r0,#1
    blt 100f
                                        @ start
    clz r2,r0                           @ number of zeros on the left
    rsb r2,#32                          @ so many useful numbers right
    bic r2,#1                           @ to have an even number of digits
    mov r3,#0b11                        @ mask for extract 2 bits
    lsl r3,r2
    mov r1,#0                           @ init résult with 0
    mov r4,#0                           @ raz remainder area

1:                                      @ begin loop
    and r5,r0,r3                        @ extract 2 bits with mask
    add r4,r5,lsr r2                    @ shift right and addition with remainder
    lsl r5,r1,#1                        @ multiplication by 2
    lsl r5,#1                           @ shift left one bit
    orr r5,#1                           @ bit right = 1
    lsl r1,#1                           @ shift left one bit
    subs r4,r5                          @ sub remainder
    addmi r4,r4,r5                      @ if negative restaur register
    addpl r1,#1                         @ else add 1
    subs r2,#2                          @ decrement number bits
    movmi r0,r1                         @ if end return result
    bmi 100f
    lsl r4,#2                           @ no -> shift left remainder 2 bits
    lsr r3,#2                           @ and shift right mask 2 bits
    b 1b                                @ and loop

100:
    pop {r1-r5,lr}                      @ restaur registers
    bx lr                               @return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
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
1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
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
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD

```

{{out}}

```txt
Result = 25264
```



## AutoHotkey


```AutoKotkey

; Give n an initial value
n = 519

; Loop this action while condition is not satisfied
while (Mod(n*n, 1000000) != 269696) {
	; Increment n
	n++
}

; Display n as value
msgbox, %n%

```

{{out}}

```txt
25264
```



## AWK


```AWK

# A comment starts with a "#" and are ignored by the machine.  They can be on a
# line by themselves or at the end of an executable line.
#
# A program consists of multiple lines or statements.  This program tests
# positive integers starting at 1 and terminates when one is found whose square
# ends in 269696.
#
# The next line shows how to run the program.
# syntax: GAWK -f BABBAGE_PROBLEM.AWK
#
BEGIN { # start of program
# this declares a variable named "n" and assigns it a value of zero
    n = 0
# do what's inside the "{}" until n times n ends in 269696
    do {
      n = n + 1 # add 1 to n
    } while (n*n !~ /269696$/)
# print the answer
    print("The smallest number whose square ends in 269696 is " n)
    print("Its square is " n*n)
# terminate program
    exit(0)
} # end of program

```

<p>Output:</p>

```txt

The smallest number whose square ends in 269696 is 25264
Its square is 638269696

```



## Batch File


```dos

:: This line is only required to increase the readability of the output by hiding the lines of code being executed
@echo off

:: Everything between the lines keeps repeating until the answer is found
:: The code works by, starting at 1, checking to see if the last 6 digits of the current number squared is equal to 269696
::----------------------------------------------------------------------------------
:loop
:: Increment the current number being tested by 1
set /a number+=1

:: Square the current number
set /a numbersquared=%number%*%number%

:: Check if the last 6 digits of the current number squared is equal to 269696, and if so, stop looping and go to the end
if %numbersquared:~-6%==269696 goto end

goto loop
::----------------------------------------------------------------------------------

:end
echo %number% * %number% = %numbersquared%
pause>nul

```

{{out}}

```txt

25264 * 25264 = 638269696

```



## BASIC

=
## Applesoft BASIC
=
This is an implementation based on the alternative solution for the BBC BASIC. We know that 269696 is not a perfect square, so we can safely start with N=1269696 and add 1000000 each time N is not a perfect square. The ST function returns the remainder of the difference between N and the square of the integer part of the root (R). There are a couple of quirks in AppleSoft BASIC, the largest integer is 32767, but the largest number not displayed on scientific notation is 999999999, which explains the strange IF statement in line <code>170</code>.

```basic

 100 :
 110  REM  BABBAGE PROBLEM
 120 :
 130  DEF  FN ST(A) = N - INT (A) * INT (A)
 140 N = 269696
 150 N = N + 1000000
 160 R =  SQR (N)
 170  IF FN ST(R) <  > 0 AND N < 999999999 THEN GOTO 150
 180  IF N > 999999999 THEN  GOTO 210
 190  PRINT "SMALLESt NUMBER WHOSE
      SQUARE ENDS IN"; CHR$ (13);
      "269696 IS ";R;", AND THE
      SQUARE IS"; CHR$ (13);N
 200  END
 210  PRINT "THERE IS NO SOLUTION
      FOR VALUES SMALLER"; CHR$(13);
      "THAN 999999999."

```

{{out}}

```txt
]RUN
SMALLEST NUMBER WHOSE SQUARE ENDS IN
269696 IS 25264, AND THE SQUARE IS
638269696
```

=
## Commodore BASIC
=
Based on the C language implementation

```basic
10 rem This code is an implementation of Babbage Problem
20 num = 100 : rem We can safely start at 100
30 s = num*num
40 r = s - int(s/1000000)*1000000 : rem remainder when divided by 1,000,000
50 if r = 269696 then goto 100    : rem compare with 269,696
60 print "n="num"sq="s"rem="r
70 num = num+1
80 goto 30
90 rem Print out the result
100 print:print "The smallest number whose square ends in 269696 is:"
110 print num;"....";num;"squared = ";s
120 end
```


=
## BASIC256
=

```BASIC256

#This code is an implementation of Babbage Problem
number = 2
DO
	number += 2
UNTIL ((number^2) % 1000000) = 269696
PRINT "The smallest number whose square ends in 269696 is: "; number
PRINT "It's square is "; number*number

```


=
## BBC BASIC
=
Clarity has been preferred over all other considerations. The line <tt>LET n = n + 1</tt>, for instance, would more naturally be written <tt>n% += 1</tt>, using an integer variable and a less verbose assignment syntax; but this optimization did not seem to justify the additional explanations Professor Babbage would probably need to understand it.

```bbcbasic
REM Statements beginning 'REM' are explanatory remarks: the machine will ignore them.

REM We shall test positive integers from 1 upwards until we find one whose square ends in 269,696.

REM A number that ends in 269,696 is one that leaves a remainder of 269,696 when divided by a million.

REM So we are looking for a value of n that satisfies the condition 'n squared modulo 1,000,000 = 269,696', or 'n^2 MOD 1000000 = 269696' in the notation that the machine can accept.

LET n = 0

REPEAT
  LET n = n + 1
UNTIL n^2 MOD 1000000 = 269696

PRINT "The smallest number whose square ends in 269696 is" n

PRINT "Its square is" n^2
```

{{out}}

```txt
The smallest number whose square ends in 269696 is     25264
Its square is 638269696
```



### =Alternative method=

{{trans|PowerShell}}
The algorithm given in the alternative PowerShell implementation may be substantially more efficient, depending on how long <tt>SQR</tt> takes, and I think could well be more comprehensible to Babbage.

```bbcbasic
REM Lines that begin 'REM' are explanatory remarks addressed to the human reader.

REM The machine will ignore them.

LET n = 269696

REPEAT

  LET n = n + 1000000

  REM Find the next number that ends in 269,696.

  REM The function SQR finds the square root.

  LET root = SQR n

  REM The function INT truncates a real number to an integer.

UNTIL root = INT root

REM If the square root is equal to its integer truncation, then it is an integer: so we have found our answer.

PRINT "The smallest number whose square ends in 269696 is" root

PRINT "Its square is" n
```

{{out}}
Identical to the first BBC BASIC version.

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Babbage.bas"
110 LET N=2
120 DO
130   LET N=N+2
140 LOOP UNTIL MOD(N*N,1000000)=269696
150 PRINT "The smallest number whose square ends in 269696 is:";N
160 PRINT "It's square is";N^2
```

Alternative method:
<lang IS-BASIC>100 PROGRAM "Babbage.bas"
110 LET N=269696
120 DO
130   LET N=N+1000000
140   LET R=SQR(N)
150 LOOP UNTIL R=INT(R)
160 PRINT "The smallest number whose square ends in 269696 is:";R
170 PRINT "It's square is";N
```



## Befunge


Befunge is not an easily readable language, but with a basic understanding of the syntax, I think an intelligent person should be able to follow the logic of the code below.


```befunge
    1+       ::*    "d"::** %       "V8":** -!     #v_ > > > > >
                                                    v
increment n  n*n  modulo 1000000  equal to 269696?  v  if false, loop to right
                                                    v
v"Smallest number whose square ends in 269696 is "0 <  else output n below
>:#,_$           .      55+,     @

ouput message  then n  newline  exit

numeric constants explained:

"d"  ascii value of 'd', i.e. 100
::   duplicate twice: 100,100,100
**   multiply twice: 100*100*100 = 1000000

"V8" ascii values of 'V' and '8', i.e. 86 and 56
:    duplicate the '8' (56): 86,56,56
**   multiply twice:  86*56*56 = 269696
```


{{out}}


```txt
Smallest number whose square ends in 269696 is 25264
```



## C


```C

// This code is the implementation of Babbage Problem

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

int main() {
	int current = 0, 	//the current number
	    square;		//the square of the current number

	//the strategy of take the rest of division by 1e06 is
	//to take the a number how 6 last digits are 269696
	while (((square=current*current) % 1000000 != 269696) && (square<INT_MAX)) {
		current++;
	}

        //output
	if (square>+INT_MAX)
	    printf("Condition not satisfied before INT_MAX reached.");
	else
	    printf ("The smallest number whose square ends in 269696 is %d\n", current);

        //the end
	return 0 ;
}

```


{{out}}

```txt
The smallest number whose square ends in 269696 is 25264
```



## C++


```cpp
#include <iostream>

int main( ) {
   int current = 0 ;
   while ( ( current * current ) % 1000000 != 269696 )
      current++ ;
   std::cout << "The square of " << current << " is " << (current * current) << " !\n" ;
   return 0 ;
}
```


{{out}}

```txt
The square of 25264 is 638269696 !

```



## C#



```c#
namespace Babbage_Problem
{
    class iterateNumbers
    {
        public iterateNumbers()
        {
            long baseNumberSquared = 0; //the base number multiplied by itself
            long baseNumber = 0;  //the number to be squared, this one will be iterated

            do  //this sets up the loop
            {
                baseNumber += 1; //add one to the base number
                baseNumberSquared = baseNumber * baseNumber; //multiply the base number by itself and store the value as baseNumberSquared
            }
            while (Right6Digits(baseNumberSquared) != 269696); //this will continue the loop until the right 6 digits of the base number squared are 269,696

            Console.WriteLine("The smallest integer whose square ends in 269,696 is " + baseNumber);
            Console.WriteLine("The square is " + baseNumberSquared);

        }

        private long Right6Digits(long baseNumberSquared)
        {

            string numberAsString = baseNumberSquared.ToString(); //this is converts the number to a different type so it can be cut up

            if (numberAsString.Length < 6) { return baseNumberSquared; }; //if the number doesn't have 6 digits in it, just return it to try again.

            numberAsString = numberAsString.Substring(numberAsString.Length - 6);  //this extracts the last 6 digits from the number

            return long.Parse(numberAsString); //return the last 6 digits of the number

        }
    }
}}
```

{{out}}

```txt
The smallest integer whose square ends in 269,696 is 25264
The square is 638269696
```



=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>BABBAGE
	; start at the integer prior to the square root of 269,696 as it has to be at least that big
	set i = ($piece($zsqr(269696),".",1,1) - 1)    ; piece 1 of . gets the integer portion

	; loop forever, incrementing by one, until we find a square ending in 269696
	for {
		set i = i + 1    ; this will start us at the integer value from the piece statement above

		; evaluate if the last 6 digits of the square equal 269696
		set square = i * i
		if ($extract(square,$length(square) - 5,$length(square)) = 269696) {
			; We match - display the integer and square value to the screen, formatting the numerics with a comma separator as needed.
			write !,"Result: "_$fnumber(i,",")_" squared is "_$fnumber(square,",")_".  This ends in 269696."
			quit    ; exit for loop
		}
	}

	quit    ; exit routine
```


{{out}}
```txt
SAMPLES>do ^BABBAGE

Result: 25,264 squared is 638,269,696.  This ends in 269696.
```



## Clojure


```clojure
; Defines function named babbage? that returns true if the
; square of the provided number leaves a remainder of 269,696 when divided
; by a million
(defn babbage? [n]
  (let [square (* n n)]
    (= 269696 (mod square 1000000))))

; Use the above babbage? to find the first positive integer that returns true
; (We're exploiting Clojure's laziness here; (range) with no parameters returns
; an infinite series.)
(first (filter babbage? (range)))
```

{{out}}

```txt
25264
```



## COBOL


```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BABBAGE-PROGRAM.
* A line beginning with an asterisk is an explanatory note.
* The machine will disregard any such line.
DATA DIVISION.
WORKING-STORAGE SECTION.
* In this part of the program we reserve the storage space we shall
* be using for our variables, using a 'PICTURE' clause to specify
* how many digits the machine is to keep free.
* The prefixed number 77 indicates that these variables do not form part
* of any larger 'record' that we might want to deal with as a whole.
77  N           PICTURE 99999.
* We know that 99,736 is a valid answer.
77  N-SQUARED   PICTURE 9999999999.
77  LAST-SIX    PICTURE 999999.
PROCEDURE DIVISION.
* Here we specify the calculations that the machine is to carry out.
CONTROL-PARAGRAPH.
    PERFORM COMPUTATION-PARAGRAPH VARYING N FROM 1 BY 1
    UNTIL LAST-SIX IS EQUAL TO 269696.
    STOP RUN.
COMPUTATION-PARAGRAPH.
    MULTIPLY N BY N GIVING N-SQUARED.
    MOVE N-SQUARED TO LAST-SIX.
* Since the variable LAST-SIX can hold a maximum of six digits,
* only the final six digits of N-SQUARED will be moved into it:
* the rest will not fit and will simply be discarded.
    IF LAST-SIX IS EQUAL TO 269696 THEN DISPLAY N.
```

{{out}}

```txt
25264
```



## Common Lisp


```Lisp

(defun babbage-test (n)
 "A generic function for any ending of a number"
  (when (> n 0)
    (do* ((i 0 (1+ i))
          (d (expt 10 (1+ (truncate (log n) (log 10))))) )
      ((= (mod (* i i) d) n) i) )))


```


{{out}}

```txt
(babbage-test 269696)
25264
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

; Project : Babbage problem

(setq n 1)
(setq bab2 1)
(loop while (/= bab2 269696)
    do (setq n (+ n 1))
         (setf bab1 (expt n 2))
         (setf bab2 (mod bab1 1000000)))
(format t "~a" "The smallest number whose square ends in 269696 is: ")
(write n)
(terpri)
(format t "~a" "Its square is: ")
(write (* n n))

```

Output:

```txt

The smallest number whose square ends in 269696 is: 25264
Its square is: 638269696

```



## Component Pascal

{{Works with|BlackBox Component Builder}}

```oberon2

MODULE BabbageProblem;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	i: LONGINT;
BEGIN
	i := 2;
	WHILE (i * i MOD 1000000) # 269696 DO
		IF i MOD 10 = 4 THEN INC(i,2) ELSE INC(i,8) END
	END;
	StdLog.Int(i)
END Do;

END BabbageProblem.

```


```txt
Execute: ^Q BabbageProble.Do
```

{{out}}

```txt

25264

```


## D


```D
// It's basically the same as any other version.
// What can be observed is that 269696 is even, so we have to consider only even numbers,
// because only the square of even numbers is even.

import std.math;
import std.stdio;

void main( )
{
    // get smallest number <= sqrt(269696)
    int k = cast(int)(sqrt(269696.0));

    // if root is odd -> make it even
    if (k % 2 == 1)
        k = k - 1;

    // cycle through numbers
    while ((k * k) % 1000000 != 269696)
        k = k + 2;

    // display output
    writefln("%d * %d = %d", k, k, k*k);
}
```


{{out}}

```txt
25264 * 25264 = 638269696

```


## Dafny


```dafny

// Helper function for mask: does the actual computation.
function method mask_(v:int,m:int):int
  decreases v-m
  requires 0 <= v && 0 < m
  ensures v < mask_(v,m)
{
  if v < m then m else mask_(v,m*10)
}

// Return the smallest power of 10 greater than v.
function method mask(v:int):int
  requires 0 <= v
  ensures v < mask(v)
{
  mask_(v,10)
}

// Return true if the last digits of v == suffix.
predicate method EndWith(v:int,suffix:int)
  requires 0 <= suffix
{
  v % mask(suffix) == suffix
}

method SmallestSqEndingWith(suffix:int) returns (s:int)
  requires 0 < suffix
  ensures EndWith(s*s, suffix)
  // ensures forall i :: 0 <= i < s ==> !EndWith(i*i,suffix)
  decreases *                   // This method may not terminate.
{
  s := 0;
  // squares is the sequence of s*s. A ghost variable is only used by the
  // verification process at compile time.
  ghost var squares := [];
  while !EndWith(s*s, suffix)
    invariant s == |squares|
    invariant forall i :: 0 <= i < s ==> squares[i] == i*i && !EndWith(squares[i], suffix)
    decreases *
  {
    squares := squares + [s*s];
    s := s + 1;
  }
  // Leaving the method:
  // s*s ends with the suffix.
  assert EndWith(s*s, suffix);
  // The sequence squares contains i*i for i in [0..s]; none of the elements of
  // squares ends with the suffix.
  assert s == |squares|;
  assert forall i :: 0 <= i < s ==> i*i == squares[i] && !EndWith(squares[i], suffix);
  // That last assertion should imply the commented-out post-condition of the
  // method, but I'm not sure how to express that.
  //
  // Conclusion: s is guaranteed to be the smallest number whose square ends
  // with the suffix.
}

method Main() decreases *
{
  var suffix := 269696;
  var smallest := SmallestSqEndingWith(suffix);
  print smallest, "\n";
}

```




## Dart


```dart



main() {
	var x = 0;
	while((x*x)% 1000000 != 269696)
	{	x++;}

	print('$x');
}

```



## Dyalect



```dyalect
for i in 2..Integer.max {
	if i * i % 1000000 == 269696 {
		print("\(i) is the smallest number that ends with 269696")
		break
	}
}
```


{{out}}


```txt
25264 is the smallest number that ends with 269696
```



## EasyLang

<lang>while n * n mod 1000000 <> 269696
  n += 1
.
print n
```



## EDSAC order code

The only short cut in the program below is to note that both
269696 and 1000000 are divisible by 64,
so that only numbers n divisible by 8 need be considered.
To save time in finding the residue of n^2 modulo 1000000,
the program does not compute n^2 but
adds n^2 - (n - 8)^2 to the previous residue.
This of course is the principle that Babbage used in his Difference Engine.

One inconvenience of EDSAC was that a programmer who wanted a 35-bit constant
could specify the low and high 17-bit words, but not the middle bit ("sandwich digit"),
which had to be 0.
So a constant such as 1000000, with middle bit 1, could not be defined.
In this case the programmer could usually define the negative of the desired constant
and work with that instead, as illustrated below.

```edsac

[Babbage problem from Rosetta Code website]
[EDSAC program, Initial Orders 2]

[Library subroutine M3. Pauses the loading, prints header,
  and gets overwritten when loading resumes.
Here, the last character sets the teleprinter to figures.]
 PFGKIFAFRDLFUFOFE@A6FG@E8FEZPF
 @&*SOLUTION!TO!BABBAGE!PROBLEM@&#
        ..PZ   [blank tape, needed to mark end of header text]

[Library subroutine P6. Prints strictly positive integer.
32 locations; argument at 0, working locations 1, 4, 5]
        T56K   [define load address for subroutine]
        GKA3FT25@H29@VFT4DA3@TFH30@S6@T1FV4DU4DAFG26@
        TFTFO5FA4DF4FS4FL4FT4DA1FS3@G9@EFSFO31@E20@J995FJF!F

               [Main routine. Load after subroutine P6.
                Must be at an even address because each double
                value at the start must be at an even address.]
        T88K   [define absolute load address]
        GK     [set @ (theta) for relative addresses]

               [Variables]
    [0] PF PF     [trial solution, call it n]
    [2] PF PF     [residue of n^2 modulo 1000000]
    [4] PF PF     [1st difference for n^2]

               [Constants]
    [6] P64F PF   [2nd difference for n^2, i.e. 128]
    [8] P4F  PF   [1st difference for n, i.e. 8]
   [10] #1760F V2046F  [-1000000]
   [12] Q1728F PD [269696]
   [14] &F     [line feed]
   [15] @F     [carriage return]
   [16] K4096F [teleprinter null]

           [Enter with acc = 0]
   [17] T#@    [trial number n := 0]
        T2#@   [(n^2 mod 1000000) := 0]
        S6#@   [acc := -128]
        RD     [right shift]
        T4#@   [(1st difference for n^2) := -64]

           [Start of loop]
   [22] TF     [clear acc]
        A#@    [load n]
        A8#@   [add 8]
        T#@    [update n]
        A4#@   [load 1st difference of n^2]
        A6#@   [add 128]
        T4#@   [update]
        A2#@   [load residue of n^2 mod 1000000]
        A4#@   [add 1st difference]
   [31] A10#@  [subtract 1000000, by adding -1000000]
        E31@   [repeat until result < 0]
        S10#@  [add back 1000000]
        U2#@   [update residue]
        S12#@  [subtract target 269696]
        G22@   [loop back if residue < 269696]
               [if still here, acc is non-neg mult of 64]
        S8#@   [test for acc = 0 by subtracting 8]
        E22@   [loop back if residue > 269696]

           [Here with the solution]
        TF     [clear acc]
        A#@    [load solution n]
        TD     [store at absolute address 0 for printing]
   [42] A42@   [for return from subroutine]
        G56F   [call subroutine to print n]
        O15@   [print CR]
        O14@   [print LF]
        O16@   [print null, to flush printer buffer]
        ZF     [stop]
        E17Z   [define relative start address]
        PF

```

{{out}}

```txt

SOLUTION TO BABBAGE PROBLEM
25264

```




## Elena

{{trans|Smalltalk}}
ELENA 4.x :

```elena
import extensions;
import system'math;

public program()
{
    var n := 1;

    until(n.sqr().mod:1000000 == 269696)
    {
        n += 1
    };

    console.printLine(n)
}
```

{{out}}

```txt

25264

```



## Elixir


```elixir
defmodule Babbage do
  def problem(n) when rem(n*n,1000000)==269696, do: n
  def problem(n), do: problem(n+2)
end

IO.puts Babbage.problem(0)
```

or

```elixir
Stream.iterate(2, &(&1+2))
|> Enum.find(&rem(&1*&1, 1000000) == 269696)
|> IO.puts
```


{{out}}

```txt

25264

```


## Erlang


```Erlang

-module(solution1).
-export([main/0]).
babbage(N,E) when N*N rem 1000000 == 269696 ->
	io:fwrite("~p",[N]);
babbage(N,E) ->
	case E of
	4 -> babbage(N+2,6);
	6 -> babbage(N+8,4)
end.
main()->
	babbage(4,4).

```


## F#


```f#

let mutable n=1
while(((n*n)%( 1000000 ))<> 269696) do
    n<-n+1
printf"%i"n

```


Same as above, sans mutable state.

```fsharp

Seq.initInfinite id
|> Seq.skipWhile (fun n->(n*n % 1000000) <> 269696)
|> Seq.head |> printfn "%d"

```



## Factor


```factor
! Lines like this one are comments. They are meant for humans to
! read and have no effect on the instructions carried out by the
! computer (aside from Factor's parser ignoring them).

! Comments may appear after program instructions on the same
! line.

! Each word between USING: and ; is a vocabulary. By importing
! a vocabulary in this way, its words are made available for the
! program to use. This is a way to keep the space requirements
! down for deployed programs, and a nice side effect is that it
! gives readers a clue for where to look for documentation.

USING: kernel math math.ranges prettyprint sequences ;

! Before the program begins, it's incredibly helpful to have an
! understanding of Factor's dataflow model. Don't worry; it's
! not complicated, but it's confusing to read a Factor program
! without this knowledge.

! Factor is a stack-based language. What this means is that
! there is an implicit data stack in the background, waiting
! to recieve whatever manner of thing we wish to give it. Here
! is a simple arithmetic expression to demonstrate:

! language token | data stack
! ---------------+-----------
!     2            2      ! numbers place themselves on the stack.
!     1            2 1
!     4            2 1 4
!     +            2 5    ! consume 1 and 4 and leave behind 5.
!     *            10     ! consume 2 and 5 and leave behind 10.

! Thus the phrase

! 2 1 4 + *

! in Factor is a way to calculate 2 * (4 + 1).
! We could have also written this as

! 1 4 + 2 *

! with no change in meaning or outcome.

! Because of the way the data stack works, there is no need
! to specify order of operations in the language, because you do
! so inherently by the order you place things on the data stack.

!
###  BEGIN PROGRAM =========================================


518 99,736 2 <range>   ! Here we place three numbers on the
                       ! stack representing a range of numbers.
                       ! The first, 518, represents the starting
                       ! point of the sequence. 99,736
                       ! represents the ending point of the
                       ! sequence. 2 represents the "step" of
                       ! the sequence, or a constant distance
                       ! between members.

                       ! <range> takes those three numbers and
                       ! creates an object representing the
                       ! described range of numbers. Computers
                       ! of today are more than capable of
                       ! storing that many numbers, but <range>
                       ! doesn't store them all; it calculates
                       ! the number that is needed at the
                       ! current time.

                       ! The rationale for the sequence is as
                       ! follows. Odd squares are always odd, so
                       ! we don't need to consider them. That's
                       ! why the sequence starts with an even
                       ! number and is incremented by 2. We
                       ! choose 518 to start because it's the
                       ! largest even square less than 269,696.
                       ! We choose 99,736 to end because we
                       ! know it's a solution.

[ sq 1,000,000 mod 269,696 = ]
                       ! the [ ... ] form is called a quotation.
                       ! Think of it like a sequence that stores
                       ! code. It's a way to place code on the
                       ! data stack without executing it. This
                       ! is so that it can be used by the find
                       ! word. You could also think of it much
                       ! like a function that hasn't been given
                       ! a name.

find
                       ! When we call the find word, there are
                       ! two objects on the stack: a sequence
                       ! and a quotation. find is a word that
                       ! takes a sequence and a quotation and
                       ! applies the quotation to one member of
                       ! the sequence after another. It does
                       ! so until the quotation returns a t
                       ! value (denoting a boolean true) and
                       ! then leaves that number, along with its
                       ! index in the sequence, on the stack.

                       ! Let's take a look at what happens
                       ! for each iteration of find. Let's look
                       ! at what happens with the first number
                       ! in the sequence.

! language token | data stack
! ---------------+-----------
! 518              518               ! 518 is placed on the stack
                                     ! from the sequence by find.
! sq               268,324           ! square it
! 1,000,000        268,324 1,000,000 ! place a million on the stack
! mod              268,324           ! take modulus of 268,324
                                     ! and 1,000,000
! 269,696          268,324 269,696   ! place 269,696 on the stack
! =                f                 ! test 268,324 and 269,696 for
                                     ! equality.

                       ! So the square of the first number in
                       ! the sequence, 518, does not end with
                       ! 269,696. We'll try each number in the
                       ! sequence until we get a t.

.    ! Consume the top member of the data stack and print it out.

drop ! find leaves both the found element from the sequence
     ! and the index at which it was found on the data stack.
     ! We don't care about the index so we will call drop to
     ! remove it from the top of the data stack. All programs
     ! must end with an emtpy data stack.

! Putting the entire program together, it looks like this:

! 518 99,736 2 <range> [ sq 1,000,000 mod 269,696 = ] find . drop
```

{{out}}

```txt

25264

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Babbage_problem this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Can a Forth program be made readable to a novice, without getting into what a stack is? We shall see.

```forth
( First we set out the steps the computer will use to solve the problem )

: BABBAGE
    1 ( start from the number 1 )
    BEGIN ( commence a "loop": the computer will return to this point repeatedly )
        1+ ( add 1 to our number )
        DUP DUP ( duplicate the result twice, so we now have three copies )
        ( We need three because we are about to multiply two of them together to find the square, and the third will be used the next time we go around the loop -- unless we have found our answer, in which case we shall need to print it out )
        * ( * means "multiply", so we now have the square )
        1000000 MOD ( find the remainder after dividing it by a million )
        269696 = ( is it equal to 269,696? )
    UNTIL ( keep repeating the steps from BEGIN until the condition is satisfied )
    . ; ( when it is satisfied, print out the number that allowed us to satisfy it )

( Now we ask the machine to carry out these instructions )

BABBAGE
```

{{out}}

```txt
25264
```



## Fortran



### First FORTRAN

Mister Babbage,

I have been working for 2 years in New York on an IBM 704 computer.

I have just finished my work on a new language, a language for non-specialists.

I called it: FORTRAN (FORmula TRANslator).

And with it I solved your problem.

Sincerely,

John Backus - September 1956


```fortran
      DO 3 N=1,99736
      IF(MODF(N*N,1000000)-269696)3,4,3
 3    CONTINUE
 4    PRINT 5,N
 5    FORMAT(I6)
      STOP

```

{{out}}

```txt
25264
```

Mister Babbage, an addendum :

I was confident that my program will work because the FORTRAN for IBM 704 rely on a computer with a hardware of 36 bit integers.

You already proved than the number N must be less or equal to 99736. But if N were greater than 46340 (&radic;{{overline| 2<sup>31</sup>-1 }}), half of the programs you see here use 32 bit integers and they would have failed with an overflow exception.

Sincerely - J. B.


### Modern Fortran

{{trans|F#}}

```fortran

program babbage
  implicit none
  integer :: n

  n=1
  do while (mod(n*n,1000000) .ne. 269696)
     n = n + 1
  end do
  print*, n
end program babbage

```



## FreeBASIC


```freebasic
' version 25-10-2016
' compile with: fbc -s console

' Charles Babbage would have known that only number ending
' on a 4 or 6 could produce a square ending on a 6
' also any number below 520 would produce a square smaller than 269,696
' we can stop when we have reached 99,736
' we know it square and it ends on 269,696

Dim As ULong number = 524 ' first number to try
Dim As ULong square, count

Do
    ' square the number
    square = number * number
    ' look at the last 6 digits, if they match print the number
    If Right(Str(square), 6) = "269696" Then Exit Do
    ' increase the number with 2, number end ons a 6
    number = number +2
    ' if the number = 99736 then we haved found a smaller number, so stop
    If number = 99736 Then Exit Do
    square = number * number
    ' look at the last 6 digits, if they match print the number
    If Right(Str(square),6 ) = "269696" Then Exit Do
    ' increase the number with 8, number ends on a 4
    number = number +8
    ' go to the first line under "Do"
Loop

If number = 99736 Then
    Print "No smaller number was found"
Else
    ' we found a smaller number, print the number and its square
    Print Using "The number = #####, and its square = ##########,"; number; square
End If


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The number = 25,264 and its square = 638,269,696
```


## FutureBasic


```qbasic
include "ConsoleWindow"

dim as long i

for i = 1 to 1000000
  if i ^ 2 mod 1000000 == 269696 then exit for
next

print "The smallest number whose square ends in 269696 is"; i
print "Its square is"; i ^ 2
```

{{out}}

```txt
The smallest number whose square ends in 269696 is 25264
Its square is 638269696
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=08eafccbe3febb79ec62301ca84e3662 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iNum As Long

For iNum = 1 To 100000
  If Str(iNum * iNum) Ends "269696" Then Break
Next

Print "The lowest number squared that ends in '269696' is " & Str(iNum)

End
```

Output:

```txt

The lowest number squared that ends in '269696' is 25264

```



## Go


```go
package main

import "fmt"

func main() {
	const (
		target  = 269696
		modulus = 1000000
	)
	for n := 1; ; n++ { // Repeat with n=1, n=2, n=3, ...
		square := n * n
		ending := square % modulus
		if ending == target {
			fmt.Println("The smallest number whose square ends with",
				target, "is", n,
			)
			return
		}
	}
}
```

{{out}}

```txt

The smallest number whose square ends with 269696 is 25264

```



## Groovy


```Groovy

int n=104;   ///starting point
while( (n**2)%1000000 != 269696 )
    {  if (n%10==4)   n=n+2;
       if (n%10==6)   n=n+8;
    }
    println n+"^2== "+n**2 ;

```

{{out}}

```txt

25264^2== 638269696

```


## Haskell


### =head=


```Haskell
--Calculate squares, testing for the last 6 digits
findBabbageNumber :: Integer
findBabbageNumber =
  head (filter ((269696 ==) . flip mod 1000000 . (^ 2)) [1 ..])

main :: IO ()
main =
  (putStrLn . unwords)
    (zipWith
       (++)
       (show <$> ([id, (^ 2)] <*> [findBabbageNumber]))
       [" ^ 2 equals", " !"])
```

{{out}}

```txt
25264 ^ 2 equals 638269696 !
```



### =Safe.headMay=

Or, if we incline to the ''nullius in verba'' approach, are not yet convinced that there really are any such numbers below 100,000, and look uncertainly at '''head''' – a partial function which simply fails on empty lists, we could import the Safe module, and use the '''headMay''' alternative, which, more cautiously and experimentally, returns a Maybe value:


```haskell
import Data.List (intercalate)
import Data.Maybe (maybe)
import Safe (headMay)

maybeBabbage :: Integer -> Maybe Integer
maybeBabbage upperLimit =
  headMay
    (filter ((269696 ==) . flip rem 1000000) ((^ 2) <$> [1 .. upperLimit]))

main :: IO ()
main = do
  let upperLimit = 100000
  putStrLn $
    maybe
      (intercalate (show upperLimit) ["No such number found below ", " ..."])
      (intercalate " ^ 2  ->  " .
       fmap show . (<*>) [floor . sqrt . fromInteger, id] . pure)
      (maybeBabbage upperLimit)
```

{{Out}}

```txt
25264 ^ 2  ->  638269696
```



### =Suffixes and integer roots=


The inverse approach, which gets us to the first number in just 638 tests, is to append a 269696 suffix to each successive integer, filtering for results with integer square roots.

We can then harvest as many as we need from an infinite stream of babbages, Mr Babbage.


```haskell
import Data.List (intercalate)

babbagePairs :: [[Integer]]
babbagePairs =
  [0,1000000 ..] >>=                 -- Drawing from a succession of N * 10^6
  \x ->
     let y = (x + 269696)            -- The next number ending in 269696,
         r = (sqrt . fromIntegral) y -- its square root,
         i = floor r                 -- and the integer part of that root.
     in [ [i, y]                     -- Root and square harvested together,
        | r == fromIntegral i ]      -- only if that root is an integer.


main :: IO ()
main = do
  let arrowed = intercalate " ^ 2 -> " . fmap show
  mapM_ putStrLn (arrowed <$> take 10 babbagePairs)
```

{{Out}}

```txt
25264 ^ 2 -> 638269696
99736 ^ 2 -> 9947269696
150264 ^ 2 -> 22579269696
224736 ^ 2 -> 50506269696
275264 ^ 2 -> 75770269696
349736 ^ 2 -> 122315269696
400264 ^ 2 -> 160211269696
474736 ^ 2 -> 225374269696
525264 ^ 2 -> 275902269696
599736 ^ 2 -> 359683269696
```


A quick glance at these results suggests that Mr Babbage would have done well to inspect more closely the way in which the final digits of the square constrain the final digits of the root.

We can get to the solution almost immediately, after only a handful of tests, well within the reach of pencil and paper, if we discern that the root itself, to produce the 269692 suffix in its square, must have one of only four different final digit sequences: (0264, 5264, 4736, or 9736).

With a machine, this approach can industrialise the babbage harvest, yielding thousands of pairs in less than a second:

```haskell
import Data.List (intercalate)

babbagePairs :: [[Integer]]
babbagePairs =
  [0,10000 ..] >>=
  \x ->
     (<*>) [(:) <*> return . (^ 2)] ((+ x) <$> [0264, 5264, 9736, 4736]) >>=
     \[a, b] ->
        [ [a, b]
        | ((269696 ==) . flip rem 1000000) b ]

main :: IO ()
main =
  mapM_ putStrLn (intercalate " ^ 2 -> " . fmap show <$> take 4000 babbagePairs)
```



## J


The key to understandability is a mix of hopefully adequate notation and some level of verifiability.

So let's break the implementation into some named pieces and present enough detail that a mathematician can verify that the result is both consistent and reasonable:


```J
   square=: ^&2
   modulo1e6=: 1000000&|
   trythese=: i. 1000000                   NB. first million nonnegative integers
   which=: I.                              NB. position of true values
   which 269696=modulo1e6 square trythese  NB. right to left <-
25264 99736 150264 224736 275264 349736 400264 474736 525264 599736 650264 724736 775264 849736 900264 974736
```


The smallest of these values is 25264.


==== Alternatively, inspired by the APL example that makes the sentence sound natural ====


```J

NB. In the interactive environment.
NB. First here, Mr Babbage, we'll make the computer's words more meaningful to an english speaker.

NB. The first is the "head" of a list, written with these inviting open arms that embrace one small dot :
   first=: {.

NB. The small i. notation denotes "all integers up to 100000". You've already found a solution in that range.
   n=: i. 100000

NB. This is how we write squaring.
   squareof=: *:

NB. In our notation, a dyad is a word that takes an x value on the left and an y value on the right.
   ends=: dyad : ' x = 1000000 | y '

NB. This dyad selects values from the list x, as marked by the list y
   where=: dyad : ' y # x '

NB. Now that we defined our words, we can ask our question with them :
   first n where 269696 ends squareof n
25264

NB. With a bit of habit, you won't need to define words in english anymore.
NB. The following easily relates word for word to the sentence we've written :
   {. (i.100000) #~ 269696 = 1000000 | *: i.100000
25264

NB. Like all mathematical notations, in J you see patterns that suggest simplification :
   {. I. 269696 = 1000000 | *: i.100000
25264


```



## Java


```java
public class Test {

    public static void main(String[] args) {

        // let n be zero
        int n = 0;

        // repeat the following action
        do {

            // increase n by 1
            n++;

        // while the modulo of n times n is not equal to 269696
        } while (n * n % 1000_000 != 269696);

        // show the result
        System.out.println(n);
    }
}
```


```txt
25264
```



## JavaScript


### =Iteration=


```javascript
// Every line starting with a double slash will be ignored by the processing machine,
// just like these two.
//
// Since the square root of 269,696 is approximately 519, we create a variable named "n"
// and give it this value.
  n = 519

// The while-condition is in parentheses
// * is for multiplication
// % is for modulo operation
// != is for "not equal"
  while ( ((n * n) % 1000000) != 269696 )
    n = n + 1

// n is incremented until the while-condition is met, so n should finally be the
// smallest positive integer whose square ends in the digits 269,696. To see n, we
// need to send it to the monitoring device (named console).
  console.log(n)

```


### =Functional composition=


Starting with numbers which end in 269696, and filtering for those which have integer roots, we reach '''25264 ^2 -> 638269696''' after only 638 tests.

{{works with|ES6}}

```JavaScript
(() => {
    'use strict';

    // babbageNumbers :: Int -> [Int]
    const babbageNumbers = n =>
        // Take the first n Babbage numbers,
        take(n)(
            // from the concatenation of outputs of
            // a function which constructs the next number
            // ending in 269696, and returns it wrapped
            // in a list if it is a perfect square,
            // or just returns an empty list if it
            // is not a perfect square.
            // The concatenation of the map output eliminates
            // all empty lists, leaving a sequence of perfect
            // squares which end in 269696.
            concatMap(x => {
                const
                    fx = 269696 + (1000000 * x),
                    root = Math.sqrt(fx);
                return root === Math.floor(root) ? (
                    [Tuple(root)(fx)]
                ) : [];
                // Mapped over non-finite integer series
                // starting with 1.
            })(enumFrom(1))
        );


    // TEST -----------------------------------------------
    const main = () =>
        // List of the first 10 positive integers
        // whose squares end in 269696.
        unlines(
            map(pair => fst(pair) + '^2 -> ' + snd(pair))(
                babbageNumbers(10)
            ));


    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // concatMap :: (a -> [b]) -> Gen [a] -> Gen [b]
    const concatMap = f =>
        // Instance of concatMap for non-finite streams.
        function*(xs) {
            let
                x = xs.next(),
                v = undefined;
            while (!x.done) {
                v = f(x.value);
                if (0 < v.length) {
                    yield v[0];
                }
                x = xs.next();
            }
        };

    // enumFrom :: Enum a => a -> [a]
    function* enumFrom(x) {
        let v = x;
        while (true) {
            yield v;
            v = succ(v);
        }
    }

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // root :: Tree a -> a
    const root = tree => tree.root;

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // succ :: Enum a => a -> a
    const succ = x =>
        1 + x;

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = n => xs =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
25264^2 -> 638269696
99736^2 -> 9947269696
150264^2 -> 22579269696
224736^2 -> 50506269696
275264^2 -> 75770269696
349736^2 -> 122315269696
400264^2 -> 160211269696
474736^2 -> 225374269696
525264^2 -> 275902269696
599736^2 -> 359683269696
```



## jq



```txt
$ jq -n '1 | until( .*. | tostring | test("269696$"); .+1)'
25264
```


In words: start with n=1; if the decimal representation of n*n ends with 269696 then print n, otherwise increment n and restart.

'''Note for Mr Babbage.'''

```txt

Dear Sir. The answer to your most excellent problem is 25,264.
For a demonstration, come join us in the 21st century and see for yourself: Ceci est une |."

```



## Julia



```julia

function babbage(x::Integer)
  i = big(0)
  d = floor(log10(x)) + 1
  while i ^ 2 % 10 ^ d != x
    i += 1
  end
  return i
end

```


{{out}}

```txt

julia> babbage(269696)
25264


```



## Kotlin



```scala
fun main(args: Array<String>) {
    var number = 520L
    var square = 520 * 520L

    while (true) {
        val last6 = square.toString().takeLast(6)
        if (last6 == "269696") {
            println("The smallest number is $number whose square is $square")
            return
        }
        number += 2
        square = number * number
    }
}
```


{{out}}

```txt

The smallest number is 25264 whose square is 638269696

```



## Liberty BASIC

Now Mr. Babbage -- May I call you Charlie? No. OK -- we'll first start with 'n' equal to zero, then multiply it by itself to square it. If the last six digits of the result are not 269696, we'll add one to 'n' then go back and square it again. On our modern computer it should only take a moment to find the answer...

```lb

[start]
if right$(str$(n*n),6)="269696" then
    print "n = "; using("###,###", n);
    print "   n*n = "; using("###,###,###,###", n*n)
end if
if n<100000 then n=n+1: goto [start]
print "Program complete."

```

Eureka! We found it! --

```sh

n =  25,264   n*n =     638,269,696
n =  99,736   n*n =   9,947,269,696
Program complete.

```

Now my question for you, Sir, is how did you know that the square of ANY number would end in 269696??
Oh, and by the way, 99,736 is an answer too.


## Limbo


```Limbo
implement Babbage;

include "sys.m";
	sys: Sys;
	print: import sys;
include "draw.m";
	draw: Draw;

Babbage : module
{
	init : fn(ctxt : ref Draw->Context, args : list of string);
};

init (ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	current := 0;
	while ((current * current) % 1000000 != 269696)
		current++;
	print("%d", current);
}

```



## Lua

{{trans|D}}

```lua
-- get smallest number <= sqrt(269696)
k = math.floor(math.sqrt(269696))

-- if root is odd -> make it even
if k % 2 == 1 then
    k = k - 1
end

-- cycle through numbers
while not ((k * k) % 1000000 == 269696) do
    k = k + 2
end

io.write(string.format("%d * %d = %d\n", k, k, k * k))
```



## M2000 Interpreter


```M2000 Interpreter
Def Long  k=1000000, T=269696, n
n=Sqrt(269696)
For n=n to k {
      If n^2 mod k = T Then Exit
}
Report format$("The smallest number whose square ends in {0} is {1}, Its square is {2}", T, n, n**2)

```



## Mathematica

Solving up to his guess would show that there is indeed a smaller integer with that property.

```Mathematica
 Solve[Mod[x^2, 10^6] == 269696 && 0 <= x <= 99736, x, Integers]
```

{{out}}

```txt
{{x->25264},{x->99736}}
```



## Microsoft Small Basic

{{trans|VBscript}}

```smallbasic
' Babbage problem
' The quote (') means a comment
' The equals sign (=) means assign
n = 500
' 500 is stored in variable n*n
' 500 because 500*500=250000 less than 269696

' The nitty-gritty is in the 3 lines between "While" and "EndWhile".
' So, we start with 500, n is being incremented by 1 at each round
' while its square (n*n) (* means multiplication) does not have
' a remainder (function Math.Remainder) of 269696 when divided by one million.
' This means that the loop will stop when the smallest positive integer
' whose square ends in 269696
' is found and stored in n.
' (<>)  means "not equal to"
While Math.Remainder( n*n , 1000000 ) <> 269696
    n = n + 1
EndWhile

' (TextWindow.WriteLine) displays the string to the monitor
' (+) concatenates strings or variables to be displayed
TextWindow.WriteLine("The smallest positive integer whose square ends in 269696 is " + (n) + ".")
TextWindow.WriteLine("Its square is " + (n*n) + ".")

' End of Program.
```

{{out}}

```txt

The smallest positive integer whose square ends in 269696 is 25264.
Its square is 638269696.

```



## MAXScript


```maxscript
-- MAXScript : Babbage problem : N.H.
posInt = 1
while posInt < 1000000 do
	(
	if (matchPattern((posInt * posInt) as string) pattern: "*269696") then exit
	posInt += 1
	)
Print "The smallest number whose square ends in 269696 is " + ((posInt) as string)
Print "Its square is " + (((pow posInt 2) as integer) as string)

```

{{out}}
Output to MAXScript Listener:

```txt
"The smallest number whose square ends in 269696 is 25264"
"Its square is 638269696"

```


=={{header|Modula-2}}==

```modula2
MODULE BabbageProblem;
FROM FormatString IMPORT FormatString;
FROM RealMath IMPORT sqrt;
FROM Terminal IMPORT WriteString,ReadChar;

VAR
    buf : ARRAY[0..63] OF CHAR;
    k : INTEGER;
BEGIN
    (* Find the greatest integer less than the square root *)
    k := TRUNC(sqrt(269696.0));

    (* Odd numbers cannot be solutions, so decrement *)
    IF k MOD 2 = 1 THEN
        DEC(k);
    END;

    (* Find a number that meets the criteria *)
    WHILE (k*k) MOD 1000000 # 269696 DO
        INC(k,2)
    END;

    FormatString("%i * %i = %i", buf, k, k, k*k);
    WriteString(buf);

    ReadChar
END BabbageProblem.
```



## NetRexx

{{trans|Groovy}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary utf8
numeric digits 5000 -- set up numeric precision

babbageNr = babbage() -- call a function to perform the analysis and capture the result
babbageSq = babbageNr ** 2  -- calculate the square of the result
-- display results using a library function
System.out.printf("%,10d\u00b2 == %,12d%n", [Integer(babbageNr), Integer(babbageSq)])
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A function method to answer Babbage's question:
--   "What is the smallest positive integer whose square ends in the digits 269,696?"
--     — Babbage, letter to Lord Bowden, 1837;
--       see Hollingdale and Tootill, Electronic Computers, second edition, 1970, p. 125.
--   (He thought the answer might be 99,736, whose square is 9,947,269,696; but he couldn't be certain.)

method babbage() public static binary
  n = int 104 -- (integer arithmatic)
  -- begin a processing loop to determine the value
  -- starting point: 104
  loop while ((n * n) // 1000000) \= 269696
    -- loop continues while the remainder of n squared divided by 1,000,000 is not equal to 269,696
    if n // 10 == 4 then do
      -- increment n by 2 if the remainder of n divided by 10 equals 4
      n = n + 2
      end
    if n // 10 == 6 then do
      -- increment n by 8 if the remainder of n divided by 10 equals 6
      n = n + 8
      end
    end

  return n -- end the function and return the result

```

{{out}}

```txt
    25,264² ==  638,269,696
```



## Nim


```Nim

var n : int = 0
while n*n mod 1_000_000 != 269_696:
  inc(n)
echo n

```



## Objeck


```objeck
class Babbage  {
  function : Main(args : String[]) ~ Nil {
    cur := 0;
    do {
      cur++;
    }
    while(cur * cur % 1000000 <> 269696);

        cur_sqr := cur * cur;
    "The square of {$cur} is {$cur_sqr}!"->PrintLine();
  }
}

```


{{output}}

```txt

The square of 25264 is 638269696!

```



## OCaml


```OCaml

let rec f a=
if (a*a) mod 1000000 != 269696
then f(a+1)
else a
in
let a= f 1 in
Printf.printf "smallest positive integer whose square ends in the digits 269696 is %d\n" a

```



## Ol


```scheme

(print
(let loop ((i 2))
   (if (eq? (mod (* i i) 1000000) 269696)
      i
      (loop (+ i 2)))))

```

{{out}}

```txt
25264
```


## PARI/GP


```parigp
m=269696;
k=1000000;
{for(n=1,99736,
  \\ Try each number in this range, from 1 to 99736
  if(denominator((n^2-m)/k)==1, \\ Check if n squared, minus m, is divisible by k
    return(n) \\ If so, return this number and STOP.
  )
)}
```



## Pascal


```pascal
program BabbageProblem;
(* Anything bracketed off like this is an explanatory comment. *)
var n : longint; (* The VARiable n can hold a 'long', ie large, INTeger. *)
begin
    n := 2; (* Start with n equal to 2. *)
    repeat
        n := n + 2 (* Increase n by 2. *)
    until (n * n) mod 1000000 = 269696;
(* 'n * n' means 'n times n'; 'mod' means 'modulo'. *)
    write(n)
end.
```

{{out}}

```txt
25264
```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;

my $current = 0 ;
while ( ($current ** 2 ) % 1000000  != 269696 ) {
   $current++ ;
}
print "The square of $current is " . ($current * $current) . " !\n" ;
```


{{out}}

```txt
The square of 25264 is 638269696 !
```



## Perl 6


{{works with|Rakudo|2016.03}}
This could certainly be written more concisely. Extra verbiage is included to make the process more clear.

```perl6
# For all positives integers from 1 to Infinity
for 1 .. Inf -> $integer {

    # calculate the square of the integer
    my $square = $integer²;

    # print the integer and square and exit if the square modulo 1000000 is equal to 269696
    print "{$integer}² equals $square" and exit if $square mod 1000000 == 269696;
}
```


{{out}}

```txt
25264² equals 638269696
```


Alternatively, the following just may be declarative enough to allow Babbage to understand what's going on:


```perl6
say $_ if ($_²  % 1000000 == 269696) for 1..99736;
```


{{out}}

```txt

25264
99736

```



## Phix

We can omit anything odd, as any odd number squared is obviously always odd.

Mr Babbage might need the whole "i is a variable" thing explained, and that "?i" prints the value of i, nowt else springs to mind.

```Phix
for i=2 to 99736 by 2 do
    if remainder(i*i,1000000)=269696 then ?i exit end if
end for
```

{{out}}

```txt

25264

```



## PicoLisp


```PicoLisp
: (for N 99736                               # Iterate N from 1 to 99736
   (T (= 269696 (% (* N N) 1000000)) N) )    # Stop if remainder is 269696
-> 25264
```



## PILOT


```pilot
Remark:Lines identified as "remarks" are intended for the human reader, and will be ignored by the machine.
Remark:A "compute" instruction gives a value to a variable.
Remark:We begin by making the variable n equal to 2.
Compute:n = 2
Remark:Lines beginning with asterisks are labels. We can instruct the machine to "jump" to them, rather than carrying on to the next instruction as it normally would.
*CheckNextNumber
Remark:In "compute" instructions, "x * y" should be read as "x times y" and "x % y" as "x modulo y".
Compute:square = n * n
Compute:lastSix = square % 1000000
Remark:A "jump" instruction that includes an equation or an inequality in parentheses jumps to the designated label if and only if the equation or inequality is true.
Jump( lastSix = 269696 ):*FoundIt
Remark:If the last six digits are not equal to 269696, add 2 to n and jump back to "CheckNextNumber".
Compute:n = n + 2
Jump:*CheckNextNumber
*FoundIt
Remark:Type, i.e. print, the result. The symbol "#" means that what follows is one of our variables and the machine should type its value.
Type:The smallest number whose square ends in 269696 is #n. Its square is #square.
Remark:The end.
End:
```



## PowerShell


```PowerShell

###########################################################################################
#
# Definitions:
#
#   Lines that begin with the "#" symbol are comments: they will be ignored by the machine.
#
# -----------------------------------------------------------------------------------------
#
#   While
#
#   Run a command block based on the results of a conditional test.
#
#   Syntax
#       while (condition) {command_block}
#
#   Key
#
#       condition      If this evaluates to TRUE the loop {command_block} runs.
#                      when the loop has run once the condition is evaluated again.
#
#       command_block  Commands to run each time the loop repeats.
#
#   As long as the condition remains true, PowerShell reruns the {command_block} section.
#
# -----------------------------------------------------------------------------------------
#
#   *   means 'multiplied by'
#   %   means 'modulo', or remainder after division
#   -ne means 'is not equal to'
#   ++  means 'increment variable by one'
#
###########################################################################################

# Declare a variable, $integer, with a starting value of 0.

$integer = 0

while (($integer * $integer) % 1000000 -ne 269696)
{
    $integer++
}

# Show the result.

$integer

```

{{Out}}

```txt

25264

```

'''Alternative method'''
{{works with|PowerShell|2}}
By looping through potential squares instead of potential square roots, we reduce the number of loops by a factor of 40.

```PowerShell
#  Start with the smallest potential square number
$TestSquare = 269696

#  Test if our potential square is a square
#  by testing if the square root of it is an integer
#  Test if the square root is an integer by testing if the remainder
#  of the square root divided by 1 is greater than zero
#  % is the remainder operator
#  -gt is the "greater than" operator

#  While the remainder of the square root divided by one is greater than zero
While ( [Math]::Sqrt( $TestSquare ) % 1 -gt 0 )
    {
    #  Add 100,000 to get the next potential square number
    $TestSquare = $TestSquare + 1000000
    }
#  This will loop until we get a value for $TestSquare that is a square number

#  Caclulate the root
$Root = [Math]::Sqrt( $TestSquare )

#  Display the result and its square
$Root
$TestSquare
```

{{out}}

```txt
25264
638269696
```



## Processing


```java
// Lines that begin with two slashes, thus, are comments: they
// will be ignored by the machine.

// First we must declare a variable, n, suitable to store an integer:

int n;

// Each statement we address to the machine must end with a semicolon.

// To begin with, the value of n will be zero:

n = 0;

// Now we must repeatedly increase it by one, checking each time to see
// whether its square ends in 269,696.

// We shall do this by seeing whether the remainder, when n squared
// is divided by one million, is equal to 269,696.

do {
    n = n + 1;
} while (n * n % 1000000 != 269696);

// To read this formula, it is necessary to know the following
// elements of the notation:
//     * means 'multiplied by'
//     % means 'modulo', or remainder after division
//     != means 'is not equal to'

// Now that we have our result, we need to display it.

// println is short for 'print line'

println(n);
```

{{out}}

```txt
25264
```



## Prolog

Works with Swi-Prolog version 7+

```prolog
:- use_module(library(clpfd)).

babbage_(B, B, Sq) :-
	B * B #= Sq,
	number_chars(Sq, R),
	append(_, ['2','6','9','6','9','6'], R).
babbage_(B, R, Sq) :-
	N #= B + 1,
	babbage_(N, R, Sq).

babbage :-
	once(babbage_(1, Num, Square)),
	format('lowest number is ~p which squared becomes ~p~n', [Num, Square]).
```

{{out}}

```txt

1 ?- babbage.
lowest number is 25264 which squared becomes 638269696
true.

```



## PureBasic


```PureBasic
EnableExplicit
Macro putresult(n)
  If OpenConsole("Babbage_problem")
    PrintN("The smallest number whose square ends in 269696 is " + Str(n))
    Input()
  EndIf
EndMacro

CompilerIf #PB_Processor_x64
  #MAXINT = 1 << 63 - 1
CompilerElseIf #PB_Processor_x86
  #MAXINT = 1 << 31 - 1
CompilerEndIf

#GOAL = 269696
#DIV  = 1000000
Define n.i, q.i = Int(Sqr(#MAXINT))

For n = 2 To q Step 2
  If (n*n) % #DIV = #GOAL : putresult(n) : Break : EndIf
Next
```

{{out}}

```txt
The smallest number whose square ends in 269696 is 25264
```



## Python


```python

# Lines that start by # are a comments:
# they will be ignored by the machine

n=0 # n is a variable and its value is 0

# we will increase its value by one until
# its square ends in 269,696

while n**2 % 1000000 != 269696:

    # n**2 -> n squared
    # %    -> 'modulo' or remainer after division
    # !=   -> not equal to

    n += 1 # += -> increase by a certain number

print(n) # prints n

# short version
>>> [x for x in range(30000) if (x*x) % 1000000 == 269696] [0]
25264

```

{{out}}

```txt
25264
```



Or, generating a non-finite stream of numbers which are the sum of 269696 and some integer multiple of one million, and also have an integer square root:
{{Works with|Python|3.7}}

```python
'''Babbage problem'''

from math import (floor, sqrt)
from itertools import (islice)


# squaresWithSuffix :: Int -> Gen [Int]
def squaresWithSuffix(n):
    '''Non finite stream of squares with a given suffix.'''
    stem = 10 ** len(str(n))
    i = 0
    while True:
        i = until(lambda x: isPerfectSquare(n + (stem * x)))(
            succ
        )(i)
        yield n + (stem * i)
        i = succ(i)


# isPerfectSquare :: Int -> Bool
def isPerfectSquare(n):
    '''True if n is a perfect square.'''
    r = sqrt(n)
    return r == floor(r)


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Smallest positive integers whose squares end in the digits 269,696'''
    print(
        fTable(main.__doc__ + ':\n')(
            lambda n: str(int(sqrt(n))) + '^2'
        )(repr)(identity)(
            take(10)(squaresWithSuffix(269696))
        )
    )


# GENERIC -------------------------------------------------

# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value.
       For numeric types, (1 +).
    '''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# FORMATTING ----------------------------------------------
# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Smallest positive integers whose squares end in the digits 269,696:

 25264^2 -> 638269696
 99736^2 -> 9947269696
150264^2 -> 22579269696
224736^2 -> 50506269696
275264^2 -> 75770269696
349736^2 -> 122315269696
400264^2 -> 160211269696
474736^2 -> 225374269696
525264^2 -> 275902269696
599736^2 -> 359683269696

[Finished in 0.285s]
```


As a footnote on what Babbage might have managed with pencil and paper – applying the '''squaresWithSuffix(n)''' function to shorter suffixes (6, 96, 696 ...) enables us to explore the way in which the final digits of the integer root constrain and determine those of the perfect square. It quickly becomes apparent that Mr Babbage need only have considered roots ending in the digit sequences 264 or 736, a constraint which, had he deduced it with pencil and paper, would have allowed him to reach 25264 after testing the squares of only 24 other numbers.


## Racket


```racket
;; Text from a semicolon to the end of a line is ignored
;; This lets the racket engine know it is running racket
#lang racket

;; “define” defines a function in the engine
;; we can use an English name for the function
;; a number ends in 269696 when its remainder when
;; divided by 1000000 is 269696 (we omit commas in
;; numbers... they are used for another reason).
(define (ends-in-269696? x)
  (= (remainder x 1000000) 269696))

;; we now define another function square-ends-in-269696?
;; actually this is the composition of ends-in-269696? and
;; the squaring function (which is called “sqr” in racket)
(define square-ends-in-269696? (compose ends-in-269696? sqr))

;; a for loop lets us iterate (it’s a long Latin word which
;; Victorians are good at using) over a number range.
;;
;; for/first go through the range and break when it gets to
;; the first true value
;;
;; (in-range a b) produces all of the integers from a (inclusive)
;; to b (exclusive). Because we know that 99736² ends in 269696,
;; we will stop there. The add1 is to make in-range include 99736
;;
;; we define a new variable, so that we can test the verity of
;; our result
(define first-number-that-when-squared-ends-in-269696
(for/first ((i ; “i” will become the ubiquetous looping variable of the future!
             (in-range 1 (add1 99736)))
            ; when returns when only the first one that matches
            #:when (square-ends-in-269696? i))
  i))

;; display prints values out; newline writes a new line (otherwise everything
;; gets stuck together)
(display first-number-that-when-squared-ends-in-269696)
(newline)
(display (sqr first-number-that-when-squared-ends-in-269696))
(newline)
(newline)
(display (ends-in-269696? (sqr first-number-that-when-squared-ends-in-269696)))
(newline)
(display (square-ends-in-269696? first-number-that-when-squared-ends-in-269696))
(newline)
;; that all seems satisfactory
```


{{out}}

```txt
25264
638269696

#t
#t
```



## R


```R

babbage_function=function(){
  n=0
  while (n**2%%1000000!=269696) {
    n=n+1
  }
  return(n)
}
babbage_function()[length(babbage_function())]

```

{{out}}

```txt
25264
```


## Red


```Red
Red []
number: 510 ;; starting number
;; repeat, until the last condition in the block is true
until [
 number: number + 2 ;; only even numbers can have even squares
 ;; The word modulo computes the non-negative remainder of the
 ;; first argument divided by the second argument.
 ;; **  =>  Returns a number raised to a given power (exponent)
  269696 = modulo (number ** 2) 1000000
]
?? number

```

{{out}}

```txt
number: 25264
```


## REXX

Each of the first three REXX programs were constructed to be as simple as possible so as
to be easily understood by a mathematican   (in Mr. Charles Babbage's time).

For instance, in Charles Babbage's era, multiplication
of   <big>'''j'''</big>   and   <big>'''j'''</big>   would
be   <big>'''jj'''</big>   (implied multiplication),   so it would be
necessary to
explain that an asterisk   ('''*''')   means multiplication.   Another
form of multiplication in his day would
be:   <big>'''j'''</big> x <big>'''j'''</big>   or   <big>'''j ∙ j'''</big>.   Most
mathematical or algebraic texts used simple letters for   ''values''   (we
would now call them   ''variables''   when dealing with computer programs).

Fortunately, the REXX language uses decimal numbers, so binary values don't need to be
explained.

So, with that in mind, the use of (REXX) arithermetic operators were explained within a
comment,   as well as trying to explain some statements in the REXX
language.   And, further comments probably should've been added to explain what a
comment is in the REXX language   (and for that matter, comments should've been
included for each and every REXX statement explaining what the instruction (statement)
does and what the nomenclature means.   Fortunately, most REXX statements are easy
understood   (one of REXX's design goals)   and (most likely) are intuitively
understood   (at least on a fundamental or basic level), although a   '''do'''
  or   '''for'''   ''loop''   would be confusing without a detailed
explanation of what a   ''loop''   is.

A computer program (in his day) would've been undstood to be list of instructions to
a (human) computer to be performed, quite literally.

For instance, Mr. Babbage would know of a typewriter (machine),   and, to the human
computers in his day and age, they would've known (or intuited) what
a   '''type'''   or   '''print'''   instruction would do, and he
would recognize that the (human) computer would use a typewriter.   An
idle   ''monitor''   on the other hand   (I suspect),   he would be
baffled and he wouldn't probably
know of it's function.   When not displaying anything, it looks like an ineffective
mirror,   possibly having a blinking   ''cursor'',   whatever that is.

(In those days of yore, a cursor was the movable transparent slide on a slide rule.)

If this were a computer program to be shown to a computer programming novice   (albeit a very

intelligent polymath novice),   the computer program would also have
a   <big>''lot''</big>   more comments,

notes, and accompanying verbiage which would/could/should explain:

:::*   what a (computer program) comment looks like
:::*   what a computer is
:::*   what a computer program is
:::*   how a computer stores numbers and such
:::*   what are   ''variables''   and how to store   ''stuff''   in them
:::*   how the   '''do'''   loop works (initial value, incrementation, etc)
:::*   how an assignment   <big>'''=''' </big>   operator works
:::*   how a comparison   <big>'''==''' </big>   operator works
:::*   how an   '''if'''   statement works
:::*   what a (computer program) statement is
:::*   what the   <big>'''*'''</big>    operator is and how it does multiplication
:::*   what the   <big>'''+'''</big>    operator is and how it does addition
:::*   what the   <big>'''//'''</big>   operator is and how it does division remainder
:::*   what the   '''right'''   BIF does
:::*   <strike>who</strike> what a   '''BIF'''   is and how it returns a value
:::*   how/when the   '''then'''   cause gets executed   (after an   '''if''')
:::*   explain how/why an   '''end'''   statement is needed for a   '''do'''   loop
:::*   explain how a   '''leave'''   statement works
:::*   <big>···</big> the   '''say'''   is probably the only statement that is self─explanatory

===examine the right-most six digits of square===

```rexx
/*REXX program finds the  lowest (positive)  integer  whose  square  ends in  269,696.  */
   do j=2  by 2  until right(j * j, 6) == 269696 /*start  J  at two,  increment by two. */
   end                                           /*◄── signifies the end of the DO loop.*/
                                                 /* [↑]     *     means multiplication. */
say "The smallest integer whose square ends in  269,696  is: "    j
```

{{out|output}}

```txt

The smallest integer whose square ends in  269,696  is:  25264

```



### examine remainder after dividing by one million


```rexx
/*REXX program finds the  lowest (positive)  integer  whose  square  ends in  269,696.  */
   do j=2  by 2                                  /*start  J  at two,  increment by two. */
   if ((j * j) // 1000000) == 269696  then leave /*is square mod one million our target?*/
   end                                           /*◄── signifies the end of the DO loop.*/
                                                 /* [↑]    //     is division remainder.*/
say "The smallest integer whose square ends in  269,696  is: "    j
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### examine only numbers ending in 4 or 6


```rexx
/*REXX program finds the  lowest (positive)  integer  whose  square  ends in  269,696.  */
/*─────────────────── we will only examine integers that are ending  in  four  or  six. */
   do j=4  by 10                                 /*start  J  at four,  increment by ten.*/
   k = j                                         /*set    K  to  J's  value.            */
   if right(k * k, 6) == 269696  then leave      /*examine right-most 6 decimal digits. */
                                                 /*      ==  means exactly equal to.    */
   k = j+2                                       /*set    K  to  J+2  value.            */
   if right(k * k, 6) == 269696  then leave      /*examine right-most 6 decimal digits. */
   end                                           /*◄── signifies the end of the DO loop.*/
                                                 /* [↑]      *    means multiplication. */
say "The smallest integer whose square ends in  269,696  is: "   k
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### start with smallest possible number


```rexx
/*REXX ----------------------------------------------------------------
* The solution must actually be larger than sqrt(269696)=519.585
*--------------------------------------------------------------------*/
z=0
Do i=524 By 10 Until z>0
  If right(i*i,6)==269696  then z=i
  Else Do
   j=i+2
   if right(j*j,6)==269696  then z=j
   End
 End
Say "The smallest integer whose square ends in 269696 is:" z
Say '                            'z'**2 =' z**2
```

{{out}}

```txt
The smallest integer whose square ends in 269696 is: 25264
                            25264**2 = 638269696
```



## Ring


```ring

n = 0
while pow(n,2) % 1000000 != 269696
        n = n + 1
end

see "The smallest number whose square ends in 269696 is : " + n + nl
see "Its square is : " + pow(n,2)

```

Output:

```txt

The smallest number whose square ends in 269696 is : 25264
Its square is : 638269696

```



## Ruby


```ruby
n = 0
n = n + 2 until (n*n).modulo(1000000) == 269696
print n

```



## Run BASIC


```runbasic
for n = 1 to  1000000
if n^2 MOD 1000000 = 269696 then exit for
next

PRINT "The smallest number whose square ends in 269696 is "; n
PRINT "Its square is "; n^2
```


```txt
The smallest number whose square ends in 269696 is 25264
Its square is 638269696

```



## Rust


###  Imperative version


```rust
fn main() {
    let mut current = 0;
    while (current * current) % 1_000_000 != 269_696 {
        current += 1;
    }
    println!(
        "The smallest number whose square ends in 269696 is {}",
        current
    );
}
```

{{out}}

```txt

The smallest number whose square ends in 269696 is 25264

```



###  Finding in infinite range


```rust
fn main() {
    if let Some(n) = (1..).find(|x| x * x % 1_000_000 == 269_696) {
        println!("The smallest number whose square ends in 269696 is {}", n)
    }
}
```


Which outputs the same thing as above.


## Scala


```scala
//Babbage Problem

object babbage{
	def main( args:Array[String] ){

		var x:Int = 524		//Sqrt of 269696 = 519.something

		while( (x*x) % 1000000 != 269696 ){

			if( x%10 == 4 )
				x = x+2
			else
				x = x+8
		}

		println("The smallest positive integer whose square ends in 269696 = " + x )
	}
}

```



## Scheme



```Scheme

(define (digits n)
  (string->list (number->string n)))

(define (ends-with list tail)
  ;; does list end with tail?
  (starts-with (reverse list)
               (reverse tail)))

(define (starts-with list head)
  (cond ((null? head)
         #t)
        ((null? list)
         #f)
        ((equal? (car list) (car head))
         (starts-with (cdr list) (cdr head)))
        (else
         #f)))

(let loop ((i 1))
  (if (ends-with (digits (* i i)) (digits 269696))
      i
      (loop (+ i 1))))

;; 25264

```



## Scilab

{{trans|Pascal}}
<lang>n=2;
flag=%F
    while ~flag
        n = n+2;
        if pmodulo(n*n,1000000)==269696 then
            flag=%T;
        end
end
disp(n);
```

{{out}}

```txt
   25264.
```



## Seed7



```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: current is 0;
  begin
    while current ** 2 rem 1000000 <> 269696 do
      incr(current);
    end while;
    writeln("The square of " <& current <& " is " <& current ** 2);
  end func;
```


{{out}}

```txt

The square of 25264 is 638269696

```



## SequenceL


```sequencel
main() := babbage(0);

babbage(current) :=
        current when current * current mod 1000000 = 269696
    else
        babbage(current + 1);
```


{{out}}
<pre style="height: 15ex; overflow: scroll">
cmd:> babbage.exe
25264

```



## Shen


```shen
(define babbage
    N -> N where (= 269696 (shen.mod (* N N) 1000000)))
    N -> (babbage (+ N 1))

(babbage 1)
```

{{out}}

```txt
25264
```



## Sidef


```ruby
var n = 0
while (n*n % 1000000 != 269696) {
    n += 2
}
say n
```

{{out}}

```txt

25264

```


## Simula


```simula
BEGIN
    INTEGER PROBE, SQUARE;
    BOOLEAN DONE;

    WHILE NOT DONE DO BEGIN
        PROBE := PROBE + 1;
        SQUARE := PROBE * PROBE;
        IF MOD(SQUARE, 1000000) = 269696 THEN BEGIN

            OUTTEXT("THE SMALLEST NUMBER: ");
            OUTINT(PROBE,0);
            OUTIMAGE;

            OUTTEXT("THE SQUARE : ");
            OUTINT(SQUARE,0);
            OUTIMAGE;

            DONE := TRUE;
        END;
    END;

END
```

{{out}}

```txt

THE SMALLEST NUMBER: 25264
THE SQUARE : 638269696

```



## Smalltalk


```smalltalk
"We use one variable, called n. Let it initially be equal to 1. Then keep increasing it by 1 for only as long as the remainder after dividing by a million is not equal to 269,696; finally, show the value of n."
| n |
n := 1.
[ n squared \\ 1000000 = 269696 ] whileFalse: [ n := n + 1 ].
n
```

{{out}}

```txt
25264
```



## Swift


```Swift
import Swift

for i in 2...Int.max {
	if i * i % 1000000 == 269696 {
		print(i, "is the smallest number that ends with 269696")
		break
	}
}
```

{{out}}

```txt
25264 is the smallest number that ends with 269696
```



## Tcl

Hope Mr Babbage can understand this one-liner...

```Tcl
for {set i 1} {![string match *269696 [expr $i*$i]]} {incr i} {}
puts "$i squared is [expr $i*$i]"
```


```txt

25264 squared is 638269696

```


=={{header|TI-83 BASIC}}==
{{works with|TI-83 BASIC|TI-84Plus 2.55MP}}
In 1996 was manufactured the TI-83, a handheld graphing calculators with a basic language called TI-83 Basic.
The language is small and neat. For example to store 500 into a variable, it is done without twisting the meaning in mathematics of the equal sign (=).

```ti83b
536→N
```

Do not be attracted by brute force, let's do some basic maths:

As
  N²=1000000&middot;A+269696
  269696=2<sup>7</sup>&times;7<sup>2</sup>&times;43
  1000000=2<sup>6</sup>&times;5<sup>6</sup>
  269696 mod 64 = 0  & 1000000 mod 64 = 0  &rArr; N² mod 64 = 0 &rArr;  N mod 8 = 0
  &radic;{{overline| 269696 }}=519.32  => N&ge;520
  520=8&times;5&times;13
  528=16&times;3&times;11
  536=8&times;67
  N must ends by 4 or 6  &rArr;  N&ge;536
So, Lord Babbage here is your program:

```ti83b
536→N
While remainder(N*N,1000000)≠269696
  i+8→N
End
Disp N
```

And within a minute you have the answer:
{{out}}

```txt

           25264
            Done

```

And you can check the square:
{{in}}

```txt

25264²

```

{{out}}

```txt

638269696

```



## UNIX Shell

{{works with|Bourne Again Shell}}
{{works with|Korn Shell}}
{{works with|Z Shell}}


```bash
# Program to determine the smallest positive integer whose square
# has a decimal representation ending in the digits 269,696.

# Start with the smallest positive integer of them all
let trial_value=1

# Compute the remainder when the square of the current trial value is divided
# by 1,000,000.␣
while (( trial_value * trial_value % 1000000 != 269696 )); do
  # As long as this value is not yet 269,696, increment
  # our trial integer and try again.
  let trial_value=trial_value+1
done

# To get here we must have found an integer whose square meets the
# condition; display that final result
echo $trial_value
```

{{Out}}

```txt
25264
```


=== Efficient version, pen-and-pencil method ===
{{works with|Bourne Again Shell}}
{{works with|Debian Almquist Shell}}

The simple method above requires more than 20000 multiplications
and would run days on Babbage's ''Analytical Engine'' (AE),
if he would have managed to build such a machine.

As he had found a solution with pen and paper, and would have programmed the AE
correspondingly, the following solution could have been used on the AE.

It should run on any shell, including the original Bourne Shell, if the arithmetic expressions
are replaced by "expr".


```bash
#!/bin/dash

#  Babbage problem:
#  	What is the smallest (positive) integer whose square ends in the digits 269,696?
#
#  He found the second to smallest number (99736 instead of 25264) using pencil and paper,
#  and would not have wasted hours of computing time on his (planned) Analytical Engine (AE).
#
#  As most human computers know, a square must end in 0, 1, 4, 5, 6 or 9.
#  because the squares of 0 to 9 end in 0, 1, 4, 9, 6, 5, 6, 9, 4, 1.
#  Thus, the result must have the last digits 14 or 16 in the above case.
#
#  So the algorithm starts with the set {0} and an increment of 1,
#  squaring all numbers of 0+i, and keeping only those that have
#  the correct end digit.
#  Then, the new i is 10*i, and a new set of two digit endings
#  created from the old set of one digit endings, and so on.
#
#  As the AE did not have arrays or the like, the sets must be punched
#  on cards and read in for the next round.
#  The classical (original) Bourne Shell did not have arrays,
#  so you may use this script on very old machines, if 'expr' is used
#  instead of arithmetic expansion.
#  And so his script works with 'dash', the standard command interpreter
#  for non-interactive use.
#
#  To prove the speed, try 1234554321 instead of 269696,
#  the practicall immedidate answer should be 1250061111,
#  while the simple method will take hours.
#
#  Note that this method will stop if there is no solution,
#  while the simple method continues endlessly.

# filename for workfile(s)
wrk=$(basename $0 .sh).data

# set $e to desired ending. Leading zeroes are ignored.
e=${1:-269696}

# set the modulus $m to the power of 10 above $e
m=1
while test $m -le $e
do m=$((m*10))
done

# $a is number to add in each round (power of 10)
a=1

# first workfile contains just the number 0
echo 0 >$wrk

# test all workfile numbers with another digit in front
while test $a -lt $m		# until the increment excees the modulus
do mm=$((a*10))			# modulus in this round
   ee=$((e % mm))		# ending in this round
   cat $wrk |			# numbers from current workfile
      while read x
      do y=$x			# first number to test is the number read
	 while test $y -le $((x+mm-1))
	 do z=$(($y * $y))	# calculate the square
	    z=$(($z % $mm))	# ending in this round
	    if test $z -eq $ee
	    then echo $y	# candidate for next round
	    fi
	    y=$(($y + $a))	# advance leftmost digit
	 done
      done  >$wrk.new		# create new workfile
   # next round
   a=$((a*10))			# another leftmost digit
   mv $wrk.new $wrk		# cycle workfiles
done

# find each number in the last workfile  if x*x mod m = e
# ending in $e and modulus in $m
cat $wrk |			# numbers from last workfile
   while read x
   do y=$(($x * $x))		# check
      y=$(($y % $m))
      if test $y -eq $e
      then echo $x		# solution found
      fi
   done |
   sort -n |    		# numbers in ascending order
   head -n 1 			# show only smallest

```


{{out}}

```txt

$ sh babbage.sh
25264
$ sh babbage.sh 1234554321
1250061111

```



## UTFool



```UTFool

···
http://rosettacode.org/wiki/Babbage_problem
···
■ BabbageProblem
  § static
    ▶ main
    • args⦂ String[]
      for each number from √269696 up to √Integer.MAX_VALUE
          if ("⸨number × number⸩").endsWith "269696"
             System.exit number

```


## VAX Assembly


```VAX Assembly

36 35 34 33 32 31 00000008'010E0000' 0000     1 result: .ascid  "123456"                ;output buffer
                               0000  000E     2 retlen: .word   0                       ;$fao_s bytes written
         4C 55 21 00000018'010E0000' 0010     3 format: .ascid  "!UL"                   ;unsigned decimal
                                     001B     4
                               0000  001B     5 .entry  bab,0
                            55   D4  001D     6         clrl    r5                      ;result
                                     001F     7 10$:
                            55   D6  001F     8         incl    r5
             56   00   55   55   7A  0021     9         emul    r5,r5,#0,r6             ;mulr.rl, muld.rl, add.rl, prod.wq
    51   50   56   000F4240 8F   7B  0026    10         ediv    #1000000,r6,r0,r1       ;divr.rl, divd.rq, quo.wl, rem.wl
              51   00041D80 8F   D1  002F    11         cmpl    #269696,r1
                            E7   12  0036    12         bneq    10$                     ;not equal - try next
                                     0038    13
                                     0038    14         $fao_s -                        ;convert integer to text
                                     0038    15                  ctrstr = format, -
                                     0038    16                  outlen = retlen, -
                                     0038    17                  outbuf = result, -
                                     0038    18                  p1     = r5
                 B1 AF   C1 AF   B0  004A    19         movw    retlen, result          ;adjust length
                         AE AF   7F  004F    20         pushaq  result
              00000000'GF   01   FB  0052    21         calls   #1, g^lib$put_output
                                 04  0059    22         ret
                                     005A    23 .end    bab
$ run bab
25264

```


## VBA


```vb

Sub Baggage_Problem()
Dim i As Long

    'We can start at the square root of 269696
    i = 520
    '269696 is a multiple of 4, 520 too
    'so we can increment i by 4
    Do While ((i * i) Mod 1000000) <> 269696
        i = i + 4 'Increment by 4
    Loop
    Debug.Print "The smallest positive integer whose square ends in the digits 269 696 is : " & i & vbCrLf & _
    "Its square is : " & i * i
End Sub

```


```txt
The smallest positive integer whose square ends in the digits 269 696 is : 25264
Its square is : 638269696
```



## VBScript


```vb
'Sir, this is a script that could solve your problem.

'Lines that begin with the apostrophe are comments. The machine ignores them.

'The next line declares a variable n and sets it to 0. Note that the
'equals sign "assigns", not just "relates". So in here, this is more
'of a command, rather than just a mere proposition.
n = 0

'Starting from the initial value, which is 0, n is being incremented
'by 1 while its square, n * n (* means multiplication) does not have
'a modulo of 269696 when divided by one million. This means that the
'loop will stop when the smallest positive integer whose square ends
'in 269696 is found and stored in n. Before I forget, "<>" basically
'means "not equal to".
Do While ((n * n) Mod 1000000) <> 269696
    n = n + 1 'Increment by 1.
Loop

'The function "WScript.Echo" displays the string to the monitor. The
'ampersand concatenates strings or variables to be displayed.
WScript.Echo("The smallest positive integer whose square ends in 269696 is " & n & ".")
WScript.Echo("Its square is " & n*n & ".")

'End of Program.

```

{{Out}}

```txt
The smallest positive integer whose square ends in 269696 is 25264.
Its square is 638269696.
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Right6Digits(num As Long) As Long
        Dim asString = num.ToString()
        If asString.Length < 6 Then
            Return num
        End If

        Dim last6 = asString.Substring(asString.Length - 6)
        Return Long.Parse(last6)
    End Function

    Sub Main()
        Dim bnSq = 0    'the base number squared
        Dim bn = 0      'the number to be squared

        Do
            bn = bn + 1
            bnSq = bn * bn
        Loop While Right6Digits(bnSq) <> 269696

        Console.WriteLine("The smallest integer whose square ends in 269,696 is {0}", bn)
        Console.WriteLine("The square is {0}", bnSq)
    End Sub

End Module
```

{{out}}

```txt
The smallest integer whose square ends in 269,696 is 25264
The square is 638269696
```



## x86 Assembly

'''AT&T syntax''' {{works with|gas}}

```asmatt
# What is the lowest number whose square ends in 269,696?

# At the very end, when we have a result and we need to print it, we shall use for the purpose a program called PRINTF, which forms part of a library of similar utility programs that are provided for us. The codes given here will be needed at that point to tell PRINTF that we are asking it to print a decimal integer (as opposed to, for instance, text):

.data
decin: .string "%d\n\0"

# This marks the beginning of our program proper:

.text
.global main

main:

# We shall test numbers from 1 upwards to see whether their squares leave a remainder of 269,696 when divided by a million.

# We shall be making use of four machine 'registers', called EAX, EBX, ECX, and EDX. Each can hold one integer.

# Move the number 1,000,000 into EBX:

        mov    $1000000, %ebx

# The numbers we are testing will be stored in ECX. We start by moving a 1 there:

        mov    $1,       %ecx

# Now we need to test whether the number satisfies our requirements. We shall want the computer to come back and repeat this sequence of instructions for each successive integer until we have found the answer, so we put a label ('next') to which we can refer.

next:

# We move (in fact copy) the number stored in ECX into EAX, where we shall be able to perform some calculations upon it without disturbing the original:

        mov    %ecx,     %eax

# Multiply the number in EAX by itself:

        mul    %eax

# Divide the number in EAX (now the square of the number in ECX) by the number in EBX (one million). The quotient -- for which we have no use -- will be placed in EAX, and the remainder in EDX:

        idiv   %ebx

# Compare the number in EDX with 269,696. If they are equal, jump ahead to the label 'done':

        cmp    $269696,  %edx
        je     done

# Otherwise, increment the number in ECX and jump back to the label 'next':

        inc    %ecx
        jmp    next

# If we get to the label 'done', it means the answer is in ECX.

done:

# Put a reference to the codes for PRINTF into EAX:

        lea    decin,    %eax

# Now copy the number in ECX, which is our answer, into an area of temporary storage where PRINTF will expect to find it:

        push   %ecx

# Do the same with EAX -- giving the code for 'decimal integer' -- and then call PRINTF to print the answer:

        push   %eax
        call   printf

# The pieces of information we provided to PRINTF are still taking up some temporary storage. They are no longer needed, so make that space available again:

        add    $8,       %esp

# Place the number 0 in EAX -- a conventional way of indicating that the program has finished correctly -- and return control to whichever program called this one:

        mov    $0,       %eax
        ret

# The end.
```

{{out}}

```txt
25264
```



## XLISP


```scheme
; The computer will evaluate expressions written in -- possibly nested -- parentheses, where the first symbol gives the operation and any subsequent symbols or numbers give the operands.

; For instance, (+ (+ 2 2) (- 7 5)) evaluates to 6.

; We define our problem as a function:

(define (try n)

; We are looking for a value of n that leaves 269,696 as the remainder when its square is divided by a million.

; The symbol * stands for multiplication.

    (if (= (remainder (* n n) 1000000) 269696)

; If this condition is met, the function should give us the value of n:

        n

; If not, it should try n+1:

        (try (+ n 1))))

; We supply our function with 1 as an initial value to test, and ask the computer to print the final result.

(print (try 1))
```

{{out}}

```txt
25264
```



## zkl


```zkl
// The magic number is 269696, so, starting about its square root,
// find the first integer that, when squared, its last six digits are the magic number.
// The last digits are found with modulo, represented here by the % symbol
const N=269696; [500..].filter1(fcn(n){ n*n%0d1_000_000 == N })
```

{{out}}

```txt

25264

```

