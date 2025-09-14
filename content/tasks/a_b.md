+++
title = "A+B"
description = ""
date = 2019-10-03T13:38:57Z
aliases = []
[extra]
id = 6812
[taxonomies]
categories = ["Simple", "task"]
tags = []
languages = [
  "0815",
  "11l",
  "360_assembly",
  "8th",
  "abap",
  "ada",
  "agena",
  "aime",
  "algol_68",
  "algol_w",
  "antlr",
  "apex",
  "apl",
  "applescript",
  "applesoft_basic",
  "arc",
  "argile",
  "arm_assembly",
  "asciidots",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "bacon",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "bird",
  "bloop",
  "bracmat",
  "brat",
  "burlesque",
  "c",
  "c_shell",
  "ceylon",
  "clojure",
  "cobol",
  "coffeescript",
  "commodore_basic",
  "common_lisp",
  "component_pascal",
  "computer_zero_assembly",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dart",
  "dc",
  "dcl",
  "delphi",
  "dms",
  "dragon",
  "dwscript",
  "easylang",
  "echolisp",
  "edsac_order_code",
  "egl",
  "eiffel",
  "ela",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "erre",
  "euler_math_toolbox",
  "euphoria",
  "excel",
  "factor",
  "false",
  "fantom",
  "fbsl",
  "fish",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "fuze_basic",
  "gambas",
  "gastona",
  "gema",
  "genie",
  "gml",
  "go",
  "golfscript",
  "golo",
  "gosu",
  "groovy",
  "guiss",
  "haskell",
  "hexiscript",
  "hicest",
  "hope",
  "huginn",
  "hy",
  "i",
  "idris",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "jsish",
  "julia",
  "k",
  "kite",
  "klong",
  "kotlin",
  "kql",
  "lang5",
  "lasso",
  "liberty_basic",
  "lil",
  "lisaac",
  "little",
  "livecode",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "maude",
  "maxima",
  "mercury",
  "min",
  "miniscript",
  "mirc_scripting_language",
  "ml_i",
  "moonscript",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "nit",
  "nyquist",
  "objeck",
  "ocaml",
  "oforth",
  "ol",
  "onyx",
  "oorexx",
  "openedge_progress",
  "openscad",
  "oxygene",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "piet",
  "pike",
  "pl_i",
  "pony",
  "postscript",
  "potion",
  "powershell",
  "processing",
  "prodos",
  "prolog",
  "pure",
  "purebasic",
  "python",
  "qb64",
  "r",
  "ra",
  "racket",
  "rebol",
  "red",
  "retro",
  "rexx",
  "ring",
  "robotic",
  "rockstar",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "scratch",
  "sed",
  "seed7",
  "self",
  "sequencel",
  "setl",
  "shiny",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "smalltalk",
  "smart_basic",
  "smilebasic",
  "snobol4",
  "spad",
  "spark",
  "spl",
  "sql",
  "sql_pl",
  "ssem",
  "standard_ml",
  "swift",
  "tailspin",
  "tcl",
  "torquescript",
  "tuscript",
  "txr",
  "typescript",
  "unix_shell",
  "ursa",
  "ursala",
  "vala",
  "vba",
  "vbscript",
  "verilog",
  "vhdl",
  "visual_basic_.net",
  "wee_basic",
  "whitespace",
  "wren",
  "x86_assembly",
  "xeec",
  "xlisp",
  "xpl0",
  "xquery",
  "yabasic",
  "yorick",
  "zed",
  "zkl",
  "zonnon",
  "zx_spectrum_basic",
]
+++

## Task
<big>'''A+B'''</big>   ─── a classic problem in programming contests,   it's given so contestants can gain familiarity with the online judging system being used.


### Task:
Given two integers,   '''A''' and '''B'''.

Their sum needs to be calculated.


### Input data:
Two integers are written in the input stream, separated by space(s):
: <big><math>(-1000 \le A,B \le +1000)</math></big>


### Output data:
The required output is one integer:   the sum of '''A''' and '''B'''.


;Example:
::{|class="standard"
 ! input
 ! output
 |-
 |<tt> 2 2 </tt>
 |<tt> 4 </tt>
 |-
 |<tt> 3 2 </tt>
 |<tt> 5 </tt>
 |}





## 11l

{{trans|Python}}

```11l
print(sum(input().split(‘ ’, group_delimiters' 1B).map(i -> Int(i))))
```



## 0815


```0815
|x|+%
```



## 360 Assembly


```360asm
*        A+B                       29/08/2015
APLUSB   CSECT
         USING  APLUSB,R12
         LR     R12,R15
         OPEN   (MYDATA,INPUT)
LOOP     GET    MYDATA,PG          read a single record
         XDECI  R4,PG              input A
         XDECI  R5,PG+12           input B
         AR     R4,R5              A+B
         XDECO  R4,PG+24           edit A+B
         XPRNT  PG,36              print A+B
         B      LOOP               repeat
ATEND    CLOSE  MYDATA
RETURN   XR     R15,R15
         BR     R14
         LTORG
MYDATA   DCB    LRECL=24,RECFM=FT,EODAD=ATEND,DDNAME=MYFILE
PG       DS     CL24               record
         DC     CL12' '
         YREGS
         END    APLUSB

```

{{in}}

```txt

          27          53
         123         321
         999           1

```

{{out}}

```txt

          27          53          80
         123         321         444
         999           1        1000

```



## 8th


```forth>gets dup . space eval n:+ . cr</lang



## ABAP


```ABAP
report z_sum_a_b.
data: lv_output type i.
selection-screen begin of block input.
  parameters:
    p_first type i,
    p_second type i.
selection-screen end of block input.

at selection-screen output.
  %_p_first_%_app_%-text  = 'First Number: '.
  %_p_second_%_app_%-text = 'Second Number: '.

start-of-selection.
  lv_output = p_first + p_second.
  write : / lv_output.
```



## Ada


```Ada
-- Standard I/O Streams

with Ada.Integer_Text_Io;
procedure APlusB is
   A, B : Integer;
begin
   Ada.Integer_Text_Io.Get (Item => A);
   Ada.Integer_Text_Io.Get (Item => B);
   Ada.Integer_Text_Io.Put (A+B);
end APlusB;
```

Using appropriate user defined types:

```Ada
with Ada.Text_IO;

procedure A_Plus_B is
   type Small_Integers is range -2_000 .. +2_000;
   subtype Input_Values is Small_Integers range -1_000 .. +1_000;
   package IO is new Ada.Text_IO.Integer_IO (Num => Small_Integers);
   A, B : Input_Values;
begin
   IO.Get (A);
   IO.Get (B);
   IO.Put (A + B, Width => 4, Base => 10);
end A_Plus_B;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
scope
    local f := trim( io.read() ) split " "; # read a line and split into fields
    local a := tonumber( f[ 1 ] );
    local b := tonumber( f[ 2 ] );
    print( a + b )
epocs
```



## Aime


```aime
file f;
list l;

f_affix(f, "/dev/stdin");
f_list(f, l, 0);
o_integer(atoi(l[0]) + atoi(l[1]));
o_newline();
```



## ALGOL 68

{{trans|python}}
{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - missing transput function "read int"}}

### Console


```algol68
print((read int + read int))
```

Input:

```txt

1 2

```

{{out}}

```txt

         +3

```



### File


```algol68
open(stand in, "input.txt", stand in channel);
open(stand out, "output.txt", stand out channel);
print((read int + read int))
```

Input "input.txt":

```txt

3 4

```

Output "output.txt":

```txt

         +7

```



## ALGOL W


```algolw
begin
    integer a, b;
    read( a, b );
    write( a + b )
end.
```



## ANTLR

[[File:Aplusb.jpg|left|aplusb]]
[[File:Num.png|left|aplusb]]
<br clear=both>


## Apex


```Apex


static Integer sumOfTwoNums(Integer A, Integer B) {
    return A + B;
}

System.debug('A = 50 and B = 25: ' + sumOfTwoNums(50, 25));
System.debug('A = -50 and B = 25: ' +sumOfTwoNums(-50, 25));
System.debug('A = -50 and B = -25: ' +sumOfTwoNums(-50, -25));
System.debug('A = 50 and B = -25: ' +sumOfTwoNums(50, -25));

'''Output'''
A = 50 and B = 25: 75
A = -50 and B = 25: -25
A = -50 and B = -25: -75
A = 50 and B = -25: 25


```



## APL


```APL
 ⎕+⎕
```



## AppleScript

Open the '''AppleScript Editor''' and save this as '''A+B.scpt''' on your Desktop

```AppleScript
on run argv
    try
        return ((first item of argv) as integer) + (second item of argv) as integer
    on error
        return "Usage with -1000 <= a,b <= 1000: " & tab & " A+B.scpt a b"
    end try
end run
```


To make this run in Terminal open the '''Terminal.app''' and type
<tt>
osascript ~/Desktop/A+B.scpt -3 78
</tt> followed by enter.

Result: <tt>75</tt>


## Arc


```Arc

(prn (+ (read)
        (read)))

```



## Argile

{{trans|C}}
{{works with|Argile|1.0.0}}

```Argile
(: Standard input-output streams :)
use std, array
Cfunc scanf "%d%d" (&val int a) (&val int b)
printf "%d\n" (a + b)
```


```Argile
(: Input file : input.txt :)
(: Output file: output.txt :)
use std, array
let  in = fopen "input.txt" "r"
let out = fopen "output.txt" "w"
let int x, y.
Cfunc fscanf in "%d%d" (&x) (&y) (:fscanf not yet defined in std.arg:)
fprintf out "%d\n" (x+y)
fclose in
fclose out
```



## ARM Assembly

{{works with|gcc|Linux}}
Exploiting C standard library functions (scanf and printf).
Requires arm-linux-gnueabi-gcc and qemu-arm. Compile with:
<lang ARM_Assembly>arm-linux-gnueabi-as src.s -o src.o && arm-linux-gnueabi-gcc -static src.o -o run && qemu-arm run
```


<lang ARM_Assembly>.text
.global main
.extern printf
.extern scanf

main:
        push {lr}
        ldr r0, =scanf_lit
        ldr r1, =num_a
        ldr r2, =num_b
        bl scanf             // scanf("%d %d", &num_a, &num_b);
        ldr r0, =printf_lit
        ldr r1, =num_a
        ldr r1, [r1]
        ldr r2, =num_b
        ldr r2, [r2]
        add r1, r1, r2
        bl printf            // printf("%d\n", num_a + num_b);
        pop {pc}

.data
scanf_lit:      .asciz "%d %d"
printf_lit:     .asciz "%d\n"
.align 4
.bss
num_a:  .skip 4
num_b:  .skip 4
```


{{works with|gcc|Linux}}
Todo: -need to print numbers w/o the leading 0's. Replace them with spaces, so alignment is still the same.

Read two strings from stdin, convert to integers
calculate their sum, print to stdout.
A valid int is a value between -2^31 (-2147483647) and 2^31-1 (2147483647). We
do not allow -2147483648 as input, but it is a valid result. E.g. -1 -2147483647.
Maximum number of digits is 10.
Leading 0's are counted as number length.
We read signed values. We ignore leading '+'s and
allow '-' for negative values.
If multiple plus or minus signs precede a number, only the last one counts.
minval and maxval can be used to specify any valid range, (e.g. -1000 and +1000).
The range is inclusive. If 0 is specified for both ranges, range checks are not done.

Tested on RaspberryPi model B (GNU/Linux, ARMv6)
Save in ab.S
Build with:
<lang ARM_Assembly>as -o ab.o ab.S
ld -o a.out ab.o
```


<lang ARM_Assembly>.data
   .align   2
   .code 32

.section .rodata
   .align   2
   .code 32

overflow_msg:  .ascii  "Invalid number. Overflow.\n"
overflow_msglen = . - overflow_msg
bad_input_msg:  .ascii  "Invalid input. NaN.\n"
bad_input_msglen = . - bad_input_msg
range_err_msg:  .ascii  "Value out of range.\n"
range_err_msglen = . - range_err_msg
io_error_msg:  .ascii  "I/O error.\n"
io_error_msglen = . - range_err_msg

sys_exit  = 1
sys_read  = 3
sys_write = 4
max_rd_buf = 14
lf = 10
m10_9 = 0x3b9aca00
maxval = 1000
minval = -1000

.text

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ void main()
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type _start STT_FUNC
   .global _start
_start:
   stmfd   sp!, {r4,r5,lr}

.read_lhs:
   ldr r0, =max_rd_buf
   bl readint
   mov r4, r0
   bl printint
   mov r0, r4
   bl range_check

.read_rhs:
   ldr r0, =max_rd_buf
   bl readint
   mov r5, r0
   bl printint
   mov r0, r5
   bl range_check

.sum_and_print:
   adds r0, r4, r5
   bvs overflow
   bl printint

.main_exit:
   mov r0, #0
   bl exit
   ldmfd   sp!, {r4,r5,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ Read from stdin until we encounter a non-digit, or we have read bytes2rd digits.
@@ Ignore leading spaces.
@@ Return value to the caller converted to a signed int.
@@ We read positive values, but if we read a leading '-' sign, we convert the
@@ return value to two's complement.
@@ The argument is max number of bytes to read from stdin.
@@ int readint(int bytes2rd)
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type readint STT_FUNC
   .global readint
readint:
   stmfd   sp!, {r4,r5,r6,r7,lr}
   @@@@@@@@@@@@@@@
   @@ r0 : #0 for stdin arg to read.
   @@ r1 : ptr to current pos in local buffer.
   @@ r2 : #1 to read one byte at a time.
   @@ r3,r7 : tmp.
   @@ r4 : number of bytes read.
   @@ r5 : value of current byte.
   @@ r6 : 0 while we are reading leading spaces.
   @@@@@@@@@@@@@@@
   sub sp, sp, r0
   mov r1, sp
   mov r3, #0
   push {r3}        @ sp,#4: local var @isnegative. return in r1. Default value is 0/false. Positive number.
   push {r0}        @ sp,#0: local var @maxbytes. const.
   mov r2, #1
   mov r4, #0

   mov r6, #0
   b .rd
@ we get here if r6 is 0.
@ if space, goto .rd.
@ else set r6 to 1 and goto .noleading.
.leadchk:
   mov r0, r5
   bl isspace
   cmp r0, #1
   beq .rd

.sign_chk:
   mov r0, r5
   push {r1}
   bl issign
   cmp r0, #1
   streq r0, [sp,#8]   @ sp,#4 + 4 for the pushed r1.
   movhi r1, #0
   strhi r1, [sp,#8]   @ sp,#4 + 4 for the pushed r1.
   pop {r1}
   bhs .rd

   mov r6, #1
   b .noleading

.rd:
   mov r0, #0
   bl read
   cmp r0, #1
   bne .sum_digits_eof  @ eof
   mov r5, #0
   ldrb r5, [r1]
   cmp r6, #0
   beq .leadchk

.noleading:
   mov r0, r5
   bl isdigit
   cmp r0, #1
   bne .sum_digits_nan @ r5 is non-digit

   add r4, r4, #1
   add r1, r1, #1
   @ max chars to read is received in arg[0], stored in local var at sp.
   @ Only 10 can be valid, so the default of 12 leaves space for separator.
   ldr r3, [sp]
   cmp r4, r3
   beq .sum_digits_maxrd  @ max bytes read.
   b .rd


   @@@@@@@@@@@@@@@
   @ We have read r4 (0..arg[0](default 12)) digits when we get here. Go through them
   @ and add/mul them together to calculate a number.
   @ We multiply and add the digits in reverse order to simplify the multiplication.
   @@@@@@@@@@@@@@@
   @ r0: return value.
   @ r1: local variable for read buffer.
   @ r2: tmp for conversion.
   @ r3,r6,r7: tmp
   @ r4: number of chars we have read.
   @ r5: multiplier 1,10,100.
   @@@@@@@@@@@@@@@
.sum_digits_nan:
   mov r0, r5
   bl isspace
   cmp r0, #1
   bne bad_input
.sum_digits_maxrd:
.sum_digits_eof:
   mov r0, #0
   mov r5, #1
.count:
   cmp r4, #0
   beq .readint_ret
   sub r4, r4, #1
   sub r1, #1
   ldrb r2, [r1]
   sub r2, r2, #48
   mov r3, r2

   @ multiply r3 (char value of digit) with r5 (multiplier).
   @ possible overflow.
   @ MI means negative.
   @ smulls multiples two signed 32 bit vals and returns a 64 bit result.
   @ If we get anything in r7, the value has overflowed.
   @ having r2[31] set is overflow too.
   smulls r2, r7, r3, r5
   cmp r7, #0
   bne overflow
   cmp r2, #0
   bmi overflow

   @@ possible overflow.
   adds r0, r0, r2
   bvs overflow
   bmi overflow

   @@ end of array check.
   @@ check is needed here too, for large numbers, since 10 billion is not a valid 32 bit val.
   cmp r4, #0
   beq .readint_ret

   @@ multiple multiplier by 10.
   @@ possible overflow.
   @@ too many digits is input. happens if input is more than 10 digits.
   mov r3, #10
   mov r6, r5
   smulls r5, r7, r3, r6
   cmp r7, #0
   bne overflow
   cmp r5, #0
   bmi overflow
   b .count

.readint_ret:
   ldr r1, [sp,#4] @ read isnegative value.
   cmp r1, #0
   rsbne r0, r0, #0
   pop {r2}
   add sp, sp, #4
   add sp, sp, r2
   ldmfd   sp!, {r4,r5,r6,r7,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ int isdigit(int)
@@ #48..#57 ascii range for '0'..'9'.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type isdigit STT_FUNC
   .global isdigit
isdigit:
   stmfd   sp!, {r1,lr}
   cmp r0, #48
   blo .o_range
   cmp r0, #57
   bhi .o_range
   mov r0, #1
   ldmfd   sp!, {r1,pc}
.o_range:
   mov r0, #0
   ldmfd   sp!, {r1,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ int isspace(int)
@@ ascii space = 32, tab = 9, newline 10, cr = 13.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type isspace STT_FUNC
   .global isspace
isspace:
   stmfd   sp!, {lr}
   cmp   r0, #32
   cmpne r0, #9
   cmpne r0, #10
   cmpne r0, #13
   beq .is_space
   mov r0, #0
   ldmfd   sp!, {pc}
.is_space:
   mov r0, #1
   ldmfd   sp!, {pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ Return value is 1 for '-' 2 for '+'.
@@ int isspace(int)
@@ '+' = 43 and '-' = 45.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type issign STT_FUNC
   .global issign
issign:
   stmfd   sp!, {lr}
   cmp   r0, #43
   beq .plus_sign
   cmp r0, #45
   beq .minus_sign
   mov r0, #0
   ldmfd   sp!, {pc}
.plus_sign:
   mov r0, #2
   ldmfd   sp!, {pc}
.minus_sign:
   mov r0, #1
   ldmfd   sp!, {pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ ARGS:
@@ r0 : in out arg (current int value)
@@ r1 : in out arg (ptr to current pos in buffer)
@@ r2 : in arg (const increment. 1000_000_000, 100_000_000, 10_000_000, 1000_000, 100_000, 10_000, 1000, 100, 10, 1.)
@@
@@ r4 : tmp local. Outer scope must init to #10 and count down to #0.
@@      Special case is INTMAX. Must init to 5 if r4 >= 1000_000_000 (0x3b9aca00 = m10_9).
@@ r5: tmp
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type get_digit STT_FUNC
   .global get_digit
get_digit:
   stmfd  sp!, {r2,r4,r5,lr}
   ldr r5, =m10_9
   cmp r2, r5
   movlo r4, #10
   movhs r4, #5
.get_digit_loop:
   sub r4, #1
   mul r5, r4, r2
   cmp r0, r5
   blo .get_digit_loop
   sub r0, r5
   add r4, r4, #48
   strb r4, [r1], #1
   ldmfd   sp!, {r2,r4,r5,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ A quick way to divide (numbers evenly divisible by 10) by 10.
@@ Most ARM cpus don't have a divide instruction,
@@ so this will always work.
@@ A generic div function is long and not needed here.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
.div_r2_10:
   stmfd   sp!, {r0,r1,r3,lr}
   mov r0, #1
   mov r1, #10
.find_x:
   mul r3, r0, r1;
   cmp r3, r2
   movlo r0, r3
   blo .find_x
   mov r2, r0
   ldmfd   sp!, {r0,r1,r3,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
.print_neg_sign:
   stmfd   sp!, {r0,r1,r2,lr}
   @ 45 = '-'
   mov r1, #45
   push {r1}
   mov r2, #1
   @ r1 is ptr to our local variable (holding '-').
   mov r1, sp
   mov r0, #1
   bl write
   cmp r0, #0
   blne io_error
   pop {r1}
   ldmfd   sp!, {r0,r1,r2,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@ void printint(int val)
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type printint STT_FUNC
   .global printint
printint:
   stmfd   sp!, {r4,r5,r6,lr}
   mov r1, #1
   ands r1, r1, r0, LSR #31
   rsbne r0, r0, #0
   blne .print_neg_sign
   sub sp, sp, #20
   mov r1, sp
   mov r3, sp

   ldr r2, =m10_9
.getc_loop:
   bl get_digit
   cmp r2, #1
   beq .exit_getc_loop
   bl .div_r2_10
   b .getc_loop
.exit_getc_loop:
   ldr r0, =lf
   strb r0, [r1], #1

   sub r2, r1, r3
   mov r1, r3
   mov r0, #1
   bl write
   cmp r0, #0
   blne io_error
   add sp, sp, #20
   ldmfd   sp!, {r4,r5,r6,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
range_check:
   stmfd   sp!, {r4,r5,lr}
   ldr r4, =minval
   ldr r5, =maxval
   cmp   r4, #0
   cmpeq r5, #0
   beq .skip_range_check
   cmp r0, r4
   bllt range_err
   cmp r0, r5
   blgt range_err
.skip_range_check:
   ldmfd   sp!, {r4,r5,pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ void range_err()
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
range_err:
   stmfd   sp!, {lr}
   ldr r2, =range_err_msglen
   ldr r1, =range_err_msg
   mov r0, #2
   bl write
   mov   r0, #-1
   bl exit
   ldmfd   sp!, {pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ void overflow()
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
overflow:
   stmfd   sp!, {lr}
   ldr r2, =overflow_msglen
   ldr r1, =overflow_msg
   mov r0, #2
   bl write
   mov   r0, #-1
   bl exit
   ldmfd   sp!, { pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ void bad_input()
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
bad_input:
   stmfd   sp!, {lr}
   ldr r2, =bad_input_msglen
   ldr r1, =bad_input_msg
   mov r0, #2
   bl write
   mov   r0, #-1
   bl exit
   ldmfd   sp!, {pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ void io_error()
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
io_error:
   stmfd   sp!, {lr}
   ldr r2, =io_error_msglen
   ldr r1, =io_error_msg
   mov r0, #2
   bl write
   mov   r0, #-1
   bl exit
   ldmfd   sp!, {pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ void exit(int)
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type _start STT_FUNC
   .global exit
exit:
   stmfd   sp!, {r7, lr}
   ldr r7, =sys_exit
   svc #0
   ldmfd   sp!, {r7, pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ int write(int fd,char*buf,int len)
@ Return 0 if we successfully write all bytes. Otherwise return the error code.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type _start STT_FUNC
   .global write
write:
   stmfd   sp!, {r4,r7, lr}
   mov r4, r2
.wr_loop:
   ldr r7, =sys_write
   svc #0
   @ If r0 is negative, it is more than r4 with LO (unsigned <).
   cmp r0, r4
   sublo r4, r0
   blo .wr_loop
   moveq r0, #0
   ldmfd   sp!, {r4,r7, pc}

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ int read(int fd,char*buf,int len)
@ Return number of bytes successfully read. Ignore errors.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   .align   2
   .code 32
   .type _start STT_FUNC
   .global read
read:
   stmfd   sp!, {r7, lr}
   ldr r7, =sys_read
   svc #0
   cmp r0, #0
   movlt r0, #0
   ldmfd   sp!, {r7, pc}

```



## AsciiDots

<Lang AsciiDots>
 &-#$-\
.-#?-[+]
.-#?--/
</Lang>


## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
staload UN = $UNSAFE
//
(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

implement
main0() = let
  var A: int
  var B: int
  val () =
  $extfcall
    (void, "scanf", "%d%d", addr@A, addr@B)
  // end of [val]
in
   println! ($UN.cast2int(A) + $UN.cast2int(B))
end // end of [main0]

(* ****** ****** *)

```



## AutoHotkey


```AutoHotkey
InputBox, input , A+B, Two integer numbers`, separated by space.
StringSplit, output, input, %A_Space%
msgbox, % output1 . "+" . output2 "=" output1+output2
```



## AutoIt


```AutoIt
;AutoIt Version: 3.2.10.0
$num = "45  54"
consolewrite ("Sum of " & $num & " is: " & sum($num))
Func sum($numbers)
   $numm = StringSplit($numbers," ")
   Return $numm[1]+$numm[$numm[0]]
EndFunc
```



### Example2

This version can handle any amount of numbers in the input:

```AutoIt
ConsoleWrite("# A+B:" & @CRLF)

Func Sum($inp)
	Local $num = StringSplit($inp, " "), $sum = 0
	For $i = 1 To $num[0]
;~ 		ConsoleWrite("# num["&$i&"]:" & $num[$i] & @CRLF)  ;;
		$sum = $sum + $num[$i]
	Next
	Return $sum
EndFunc ;==>Sum

$inp = "17  4"
$res = Sum($inp)
ConsoleWrite($inp & " --> " & $res & @CRLF)

$inp = "999 42 -999"
ConsoleWrite($inp & " --> " & Sum($inp) & @CRLF)

; In calculations, text counts as 0,
; so the program works correctly even with this input:
Local $inp = "999x y  42 -999", $res = Sum($inp)
ConsoleWrite($inp & " --> " & $res & @CRLF)
```

{{Out}}

```txt

# A+B:
17  4 --> 21
999 42 -999 --> 42
999x y  42 -999 --> 42
```



## AWK


```awk
{print $1 + $2}
```



## Batch File

Prompts version

```dos
::aplusb.cmd
@echo off
setlocal
set /p a="A: "
set /p b="B: "
set /a c=a+b
echo %c%
endlocal
```

All on the commandline version

```dos
::aplusb.cmd
@echo off
setlocal
set a=%1
set b=%2
set /a c=a+b
echo %c%
endlocal
```

Formula on the command line version

```dos
::aplusb.cmd
@echo off
setlocal
set /a c=%~1
echo %c%
endlocal
```

Example of 'Formula on the command line version'

```txt

>aplusb 123+456
579
>aplusb "1+999"
1000

```

Parse the input stream version (thanks to Tom Lavedas on alt.msdos.batch.nt)

```dos
::aplusb.cmd
@echo off
setlocal
set /p a="Input stream: "
call :add %a%
echo %res%
endlocal
goto :eof

:add
set /a res=res+%1
shift
if "%1" neq "" goto :add
```

Example of 'parse the input stream version'

```txt
>aplusb
Input stream: 1234 5678
6912
>aplusb
Input stream: 123 234 345 456 567 678 789 890
4082
```



## BASIC


```qbasic
DEFINT A-Z

tryagain:
backhere = CSRLIN
INPUT "", i$
i$ = LTRIM$(RTRIM$(i$))
where = INSTR(i$, " ")
IF where THEN
    a = VAL(LEFT$(i$, where - 1))
    b = VAL(MID$(i$, where + 1))
    c = a + b
    LOCATE backhere, LEN(i$) + 1
    PRINT c
ELSE
    GOTO tryagain
END IF
```


=
## Applesoft BASIC
=

```ApplesoftBasic
10 BH = PEEK(37)
20 INPUT ""; A$ : I$ = A$ : VTAB BH : A = PEEK(40) + PEEK(41) * 256 : FOR S  = 0 TO 39 : IF PEEK(A + S) = 160 THEN NEXT S : S = 0
40 IF LEN(I$) THEN IF MID$(I$, LEN(I$), 1) = " " THEN I$ = MID$(I$, 1, LEN(I$) - 1) : GOTO 40RTRIM
50 IF LEN(I$) < 3 THEN 10"TRY AGAIN
60 FOR WHERE = 1 TO LEN(I$) : IF MID$(I$, WHERE, 1) <> " " THEN NEXT WHERE : GOTO 10"TRY AGAIN
70 A% = VAL(LEFT$(I$, WHERE - 1))
80 B% = VAL(MID$(I$, WHERE + 1, LEN(I$)))
90 C% = A% + B%
100 VTAB BH
110 HTAB LEN(A$) + 2 + S
120 PRINT C%
```


=
## BaCon
=

```qbasic
' A+B
INPUT d$
PRINT VAL(TOKEN$(d$, 1)) + VAL(TOKEN$(d$, 2))
```


=
## BASIC256
=

```basic256
dim a(2)
input "Enter two numbers separated by a space?", t$
a = explode(t$," ")
print t$ + " " + (a[0] + a[1])
```


=
## BBC BASIC
=

```bbc
      REPEAT
        hereY% = VPOS
        INPUT LINE "" q$
        hereX% = LEN(q$) + 1
        WHILE LEFT$(q$, 1) = " "
          q$ = MID$(q$, 2)
        ENDWHILE
        space% = INSTR(q$, " ")
        IF space% THEN
          a = VAL(LEFT$(q$, space% - 1))
          b = VAL(MID$(q$, space% + 1))
          PRINT TAB(hereX%, hereY%) ; a + b
        ENDIF
      UNTIL FALSE
```

That seems overly complicated. What's wrong with:

```bbc
      REPEAT
        INPUT LINE "" q$
        space% = INSTR(q$," ")
        PRINT VAL LEFT$(q$,space%-1) + VAL MID$(q$,space%+1)
      UNTIL FALSE
```


=
## Commodore BASIC
=

```qbasic
10 PRINT "ENTER TWO NUMBERS, SEPARATED BY A SPACE: ";
20 INPUT X$
30 I = 1 : N = LEN(X$)
40 IF MID$(X$,I,1)<>" " AND I<N THEN I = I+1 : GOTO 40
50 A = VAL(LEFT$(X$,I))
60 B = VAL(RIGHT$(X$,N-1))
70 PRINT A+B
```


=
## FreeBASIC
=

```freebasic
' fb 1.05.0 Win64

Dim As Integer a, b
Do
  Print "Enter two integers separated by a space : ";
  Input "", a, b
  If Abs(a) > 1000 OrElse Abs(b) > 1000 then
    Print "Both numbers must be in the interval [-1000, 1000] - try again"
    Print
  Else
    Print "Their sum is"; a + b
    Exit Do
  End If
Loop
Print
Print "Press any key to quit the program"
Sleep
```


=
## FUZE BASIC
=

```qbasic
INPUT n$
PRINT VAL(LEFT$(n$,(LEN(STR$(VAL(n$))))))+VAL(RIGHT$(n$,(LEN(n$)-LEN(STR$(VAL(n$)))-1)))
END
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DO
110   INPUT PROMPT "Ener two integers separated by a comma: ":A,B
120   IF ABS(A)>1000 OR ABS(B)>1000 OR IP(A)<>A OR IP(B)<>B THEN
130     PRINT "Both integers must be in the interval [-1000..1000] - try again.":PRINT
140   ELSE
150     PRINT "Their sum is";A+B
160     EXIT DO
170   END IF
180 LOOP
```


=
## Liberty BASIC
=

```lb
input, n$
print  eval(word$(n$,1);" + ";word$(n$,2))
```


=
## Sinclair ZX81 BASIC
=

```basic
10 INPUT A$
20 LET I=1
30 IF A$(I)=" " THEN GOTO 60
40 LET I=I+1
50 GOTO 30
60 PRINT VAL A$( TO I-1)+VAL A$(I+1 TO )
```



## bc

{{Works with|GNU bc}}

```bc
read() + read()
```



## Befunge


```befunge
&&+.@
```



## Bird


```Bird
use Console Math

define Main
    $a Console.Read
    $b Console.Read
    Console.Println Math.Add $a $b
end
```



## BlooP

BlooP and FlooP can't actually read from stdin, but here's the procedure it would use, if it could.
<Lang BlooP>
DEFINE PROCEDURE ''ADD'' [A, B]:
BLOCK 0: BEGIN
    OUTPUT <= A + B;
BLOCK 0: END.
</Lang>


## Bracmat

<code>filter</code> is a pattern that checks that input is a non-fractional number not less than -1000 and not greater than 1000. The filter is applied to each input.

```bracmat
( out
$ (   put$"Enter two integer numbers between -1000 and 1000:"
    & (filter=~/#%:~<-1000:~>1000)
    & get':(!filter:?a) (!filter:?b)
    & !a+!b
  | "Invalid input. Try again"
  )
);
```


=={{header|Brainfuck}}==
<lang Brainfuck>INPUT AND SUMMATION
TODO if first symbol is a minus sign print Qgo awayQ
+>                                                  initialize sum to one
++[                                                 loop for each input ie twice
    [>>,----------[----------------------[-<+>]]<]      eat digits until space or newline
    <[<]>>>
    >[<                                                 until no next digit
        ----------------                                    subtract ascii zero minus what we subtracted above
        [->++++++++++<]                                     add ten timess that to the next digit
        <[->+<]<[->+<]>>                                    shift sum and loop counter
        >>
    ]
    <----------------                                   subtract as above from last digit as well
    [-<<+>>]                                            add to sum
    <-
]
<-                                                  subtract original one from sum

OUTPUT
[                                                                                                   while a number divided by ten is bigger than zero
    [->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->+<[->--------->+<<[->>>+<<<]]]]]]]]]]>>>[-<<<+>>>]<<<]   divide by ten
    >++++++++++++++++++++++++++++++++++++++++++++++++>                                                  convert remainder to ascii digit
]
<[.<<]                                                                                              print ascii digits
```



## Brat


```brat
numbers = g.split[0,1].map(:to_i)
p numbers[0] + numbers[1]  #Prints the sum of the input
```



## Burlesque


```burlesque>ps++</lang



## C


```c
// Standard input-output streams
#include <stdio.h>
int main()
{
   int a, b;
   scanf("%d%d", &a, &b);
   printf("%d\n", a + b);
   return 0;
}
```


```c
// Input file: input.txt
// Output file: output.txt
#include <stdio.h>
int main()
{
   freopen("input.txt", "rt", stdin);
   freopen("output.txt", "wt", stdout);
   int a, b;
   scanf("%d%d", &a, &b);
   printf("%d\n", a + b);
   return 0;
}
```


```c

#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv) //not sure if argv counts as input stream... certainly it is brought here via input stream.
{
   printf("%d\n", atoi(*(argv+1)) + atoi(*(argv+2)));
   return 0;
}

```


## C#

```c#
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        Console.WriteLine(Console.ReadLine().Split().Select(int.Parse).Sum());
    }
}
```

Another way (not recommended since it does not work with more than two numbers):

```c#
using System;

class Program
{
    static void Main()
    {
        string input = Console.ReadLine();
        int index = input.IndexOf(" ");
        int num1 = int.Parse(input.Substring(0, index));
        int num2 = int.Parse(input.Substring(index + 1));
        int sum = num1 + num2;
        Console.WriteLine(sum.ToString());
    }
}
```



## C++


```cpp
// Standard input-output streams
#include <iostream>
using namespace std;
int main()
{
   int a, b;
   cin >> a >> b;
   cout << a + b << endl;
}
```


```cpp
// Input file: input.txt
// Output file: output.txt
#include <fstream>
using namespace std;
int main()
{
   ifstream in("input.txt");
   ofstream out("output.txt");
   int a, b;
   in >> a >> b;
   out << a + b << endl;
   return 0;
}
```



## Ceylon


```ceylon
shared void run() {

    print("please enter two numbers for me to add");
    value input = process.readLine();
    if (exists input) {
        value tokens = input.split().map(Integer.parse);
        if (tokens.any((element) => element is ParseException)) {
            print("numbers only, please");
            return;
        }
        value numbers = tokens.narrow<Integer>();
        if (numbers.size != 2) {
            print("two numbers, please");
        }
        else if (!numbers.every((Integer element) => -1k <= element <= 1k)) {
            print("only numbers between -1000 and 1000, please");
        }
        else if (exists a = numbers.first, exists b = numbers.last) {
            print(a + b);
        }
        else {
            print("something went wrong");
        }
    }
}
```



## Clojure


```clojure
(println (+ (Integer/parseInt (read-line)) (Integer/parseInt (read-line))))
3
4
=>7
```


```clojure
(eval (read-string (str "(+ " (read-line) " )") ))
3 3
6
```


Translation of Common Lisp version:

```clojure
(println (+ (read) (read)))
3 4
7
```



Safely and without reader tricks:

```clojure
(let [ints (map #(Integer/parseInt %) (clojure.string/split (read-line) #"\s") )]
  (println (reduce + ints)))
3 4
=>7
```


or same as above, but without "let":

```clojure
(println (reduce + (map #(Integer/parseInt %) (clojure.string/split (read-line) #"\s") )))

3 4
=>7
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. A-Plus-B.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A       PIC S9(5).
       01  B       PIC S9(5).

       01  A-B-Sum PIC S9(5).

       PROCEDURE DIVISION.
           ACCEPT A
           ACCEPT B

           ADD A TO B GIVING A-B-Sum

           DISPLAY A-B-Sum

           GOBACK
           .
```



## CoffeeScript

{{trans|JavaScript}}

```html4strict><html

<script type="text/javascript" src="http://jashkenas.github.com/coffee-script/extras/coffee-script.js"></script>
<script type="text/coffeescript">
a = window.prompt 'enter A number', ''
b = window.prompt 'enter B number', ''
document.getElementById('input').innerHTML = a + ' ' + b
sum = parseInt(a) + parseInt(b)
document.getElementById('output').innerHTML = sum
</script>
<body>
<div id='input'></div>
<div id='output'></div>
</body>
</html>
```


{{works with|Node.js}}

```coffeescript

{ stdin } = process
sum = ( a, b ) -> a + b

display = ( messages... ) -> console.log messages...

parse = ( input ) ->
    parseInt x for x in ( x.trim() for x in input.split ' ' ) when x?.length

check = ( numbers... ) ->
    return no for x in numbers when isNaN x
    return no for x in numbers when not ( -1000 < x < 1000 )
    yes

prompt = ->
    display 'Please enter two integers between -1000 and 1000, separated by a space:'
    stdin.once 'data', ( data ) ->
        [ a, b ] = parse data
        if check a, b
            display "#{ a } + #{ b } = #{ sum a, b }"
        else
            display "Invalid input: #{ a }, #{ b }"
        do prompt
        return

# Resume input and set the incoming encoding.
stdin.resume()
stdin.setEncoding 'utf8'

# Start the main loop.
do prompt

```



## Common Lisp


```lisp
(write (+ (read) (read)))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE AB;
IMPORT StdLog, DevCommanders,TextMappers;

PROCEDURE DoAB(x,y: INTEGER);
BEGIN
        StdLog.Int(x);StdLog.Int(y);StdLog.Int(x + y);StdLog.Ln;
END DoAB;

PROCEDURE Go*;
VAR
                params: DevCommanders.Par;
                s: TextMappers.Scanner;
                p : ARRAY 2 OF INTEGER;
                current: INTEGER;
BEGIN
        current := 0;
        params := DevCommanders.par;
        s.ConnectTo(params.text);
        s.SetPos(params.beg);
        s.Scan;
        WHILE(~s.rider.eot) DO
                IF (s.type = TextMappers.int) THEN
                        p[current] := s.int; INC(current);
                END;
                s.Scan;
        END;
        IF current = 2 THEN DoAB(p[0],p[1]) END;
END Go;
END AB.

```

Execute: <i>AB.Go 12 23 ~ </i><br/>
{{out}}

```txt

12 23 35

```



## Computer/zero Assembly


```czasm
        STP      ; wait for input
a:           0
b:           0
        LDA  a
        ADD  b
        STP
```



## Crystal


```ruby
puts gets.not_nil!.split.map(&.to_i).sum
```


The <code>not_nil!</code> call on <code>gets</code> is needed because <code>gets</code> might return <code>nil</code> and the compiler forces us to deal with it.
In the case of <code>nil</code> a runtime exception will be thrown.

To handle the <code>nil</code> case we could do:


```ruby
if line = gets
  puts line.split.map(&.to_i).sum
else
  puts "No input"
end
```



## D


### From Console


```d
import std.stdio, std.conv, std.string;

void main() {
    string[] r;
    try
        r = readln().split();
    catch (StdioException e)
        r = ["10", "20"];

    writeln(to!int(r[0]) + to!int(r[1]));
}
```

{{out}}

```txt
30
```


### From File


```d
void main() {
    import std.stdio, std.file;

    immutable ab = "sum_input.txt".slurp!(int, int)("%d %d")[0];
    "sum_output.txt".File("w").writeln(ab[0] + ab[1]);
}
```



## Dart


```Dart
import 'dart:io';

// a little helper function that checks if the string only contains
// digits and an optional minus sign at the front
bool isAnInteger(String str) => str.contains(new RegExp(r'^-?\d+$'));

void main() {
  while(true) {
    String input = stdin.readLineSync();
    var chunks = input.split(new RegExp(r'[ ]+')); // split on 1 or more spaces
    if(!chunks.every(isAnInteger)) {
      print("not an integer!");
    } else if(chunks.length > 2) {
      print("too many numbers!");
    } else if(chunks.length < 2) {
      print('not enough numbers!');
    } else {
      // parse the strings into integers
      var nums = chunks.map((String s) => int.parse(s));
      if(nums.any((num) => num < -1000 || num > 1000)) {
        print("between -1000 and 1000 please!");
      } else {
        print(nums.reduce((a, b) => a + b));
      }
    }
  }
}

```


{{out}}

```txt
1 2
3
3 4
7
```



## dc


```dc>? + psz</lang


The question mark ''?'' reads and executes a line of input. The user must enter a dc program that pushes two numbers to the stack, such as ''2 3'' or ''5 _1''. (The user must use underscore ''_'' for negative numbers.)

## DCL


```DCL
$ read sys$command line
$ a = f$element( 0, " ", line )
$ b = f$element( 1, " ", line )
$ write sys$output a, "+", b, "=", a + b
```


=={{header|Déjà Vu}}==
{{trans|Python}}

### Console


```dejavu
0
for k in split !prompt "" " ":
	+ to-num k
!print
```



## Delphi

Console version.

```delphi
program SUM;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure
var
  s1, s2:string;
begin
  ReadLn(s1);
  Readln(s2);
  Writeln(StrToIntDef(s1, 0) + StrToIntDef(s2,0));
end.
```



## DMS


```DMS
number a = GetNumber( "Please input 'a'", a, a )    // prompts for 'a'
number b = GetNumber( "Please input 'b'", b, b )    // prompts for 'b'
Result( a + b + "\n" )
```



## Dragon


```dragon

select "graphic"
select "types"

a = int(prompt("Enter A number"))
b = int(prompt("Enter B number"))

showln a + b

```



## DWScript

Ghetto GUI version

```delphi
var a := StrToInt(InputBox('A+B', 'Enter 1st number', '0'));
var b := StrToInt(InputBox('A+B', 'Enter 2nd number', '0'));
ShowMessage('Sum is '+IntToStr(a+b));
```



## EasyLang

<lang>a$ = input
a$[] = str_split a$
while i < len a$[] and a$[i] <> " "
  i += 1
.
if i < len a$[]
  a = number substr a$ 0 i
  b = number substr a$ i -1
  print a + b
.
```



## EchoLisp


```scheme

(+ (read-number 1 "value for A") (read-number 2 "value for B"))

```



## EDSAC order code

The EDSAC does not support input of data while a program is running, so A and B are pre-set to 37 and 28. Other values can of course be substituted: note the slightly idiosyncratic format in which integer data is written (the least significant bit set using an alphabetic character). The result of the computation is displayed in binary in the first address of storage tank 3.

```edsac
[ A plus B

### ==


  A program for the EDSAC

  Adds two integers & displays
  the sum at the top of storage
  tank 3

  Works with Initial Orders 2 ]

[ Set load point & base address ]

T56K  [ Load at address 56 ]
GK    [ Base addr (theta) here ]

[ Orders ]

T96F  [ Clear accumulator    ]
A5@   [ Acc += C(theta + 5)  ]
A6@   [ Acc += C(theta + 6)  ]
T96F  [ C(96) = Acc; Acc = 0 ]

ZF    [ Halt ]

[ Pseudo-orders (data) ]

P18D  [ 5@: 18*2 + 1 = 37 ]
P14F  [ 6@: 14*2 + 0 = 28 ]

[ When loading is finished: ]

EZPF  [ Branch to load point ]
```

{{out}}

```txt
00000000001000001
```



## Eiffel

argument(0) contains the path of the executable - thus we start at argument(1)

```eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	make
			-- Run application.
		do
			print(argument(1).to_integer +	argument(2).to_integer)
		end
end

```


Alternatively ...

```eiffel

	make
			-- Run application.
		note
			synopsis: "[
				The specification implies command line input stream and also
				implies a range for both `A' and `B' (e.g. (-1000 <= A,B <= +1000)).
				To test in Eiffel Studio workbench, one can set Execution Parameters
				of "2 2", where the expected output is 4. One may also create other
				test Execution Parameters where the inputs are out-of-bounds and
				confirm the failure.
				]"
		do
			if attached {INTEGER} argument (1).to_integer as a and then
				attached {INTEGER} argument (2).to_integer as b and then
				(a >= -1000 and b >= -1000 and a <= 1000 and b <= 1000) then
				print (a + b)
			else
				print ("Either argument 1 or 2 is out-of-bounds. Ensure: (-1000 <= A,B <= +1000)")
			end
		end

```



## Ela


```ela
open monad io string list

a'b() = do
  str <- readStr
  putStrLn <| show <| sum <| map gread <| string.split " " <| str

a'b() ::: IO
```


{{Out}}

```txt
1 2 3 4 5 6
21
```



## Elena

ELENA 4.1 :

```elena
import extensions;

public program()
{
    var A := new Integer();
    var B := new Integer();

    console.loadLine(A,B).printLine(A + B)
}
```


Or more generic solution:

```elena
import system'routines;
import extensions;

public program()
{
    console.printLine(console.readLine()
                              .split()
                              .selectBy(mssgconst toInt<convertorOp>[0])
                              .summarize())
}
```



## Elixir


```Elixir
IO.gets("Enter two numbers seperated by a space: ")
  |> String.split
  |> Enum.map(&String.to_integer(&1))
  |> Enum.sum
  |> IO.puts
```



## Elm


```Elm

--To write this function directly run cmd
--Type elm-repl to start
--Next enter this code
sum x y=x+y

--This creates a sum function
--When you enter sum A B
--You get output as A+B : number
--Task done!
--END


```



## Emacs Lisp


```Emacs Lisp
;; Write this code in a file: a+b.el
;; Put input.txt in the same directory than a+b.el
;; Open a+b.el in emacs and run the program with: M-x eval-buffer
(defun solve (xs) (mapcar (lambda (ys) (apply '+ ys)) xs))

(with-temp-buffer
  (insert-file-contents "input.txt")
  (setq content (split-string (buffer-string) "\n" t))
  (setq xs (mapcar (lambda (zs) (mapcar #'string-to-number (split-string zs))) content))
  (delete-other-windows)
  (find-file-other-window "output.txt")
  (erase-buffer)
  (insert (mapconcat (lambda (x) (format "%d" x)) (solve xs) "\n"))
  (save-buffer))

```



## Erlang


```erlang
-module(aplusb).
-export([start/0]).

start() ->
    case io:fread("","~d~d") of
        eof -> ok;
        {ok, [A,B]} ->
            io:format("~w~n",[A+B]),
            start()
    end.
```



## ERRE


```ERRE

PROGRAM SUM2

BEGIN

  LOOP
    INPUT(LINE,Q$)
    EXIT IF Q$=""
    SP%=INSTR(Q$," ")
    PRINT(VAL(LEFT$(Q$,SP%-1))+VAL(MID$(Q$,SP%+1)))
  END LOOP

END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

>s=lineinput("Two numbers seperated by a blank");
 Two numbers seperated by a blank? >4 5
>vs=strtokens(s)
 4
 5
>vs[1]()+vs[2]()
 9

```



## Euphoria


```euphoria
include get.e

function snd(sequence s)
    return s[2]
end function

integer a,b

a = snd(get(0))
b = snd(get(0))

printf(1," %d\n",a+b)
```



## EGL



```EGL

package programs;

// basic program
//
program AplusB type BasicProgram {}
	function main()
		try
			arg1 string = SysLib.getCmdLineArg(1);
			arg2 string = SysLib.getCmdLineArg(2);
			int1 int = arg1;
			int2 int = arg2;
			sum int = int1 + int2;
			SysLib.writeStdout("sum1: " + sum);
		onException(exception AnyException)
			SysLib.writeStdout("No valid input. Provide 2 integer numbers as arguments to the program.");
		end
	end
end

```



## Excel

Take any 3 columns of any row or rows. Let's say A1,B1 and C1 are taken. In C1 type in :


```excel

=A1+B1

```


The value of C1 will change as the values of A1 and B1 are changed

<lang>1	2	3

```


=={{header|F Sharp|F#}}==

```fsharp
open System

let SumOf(str : string) =
    str.Split() |> Array.sumBy(int)

[<EntryPoint>]
let main argv =
    Console.WriteLine(SumOf(Console.ReadLine()))
    0
```



## Factor


```factor
USING: math.parser splitting ;
: a+b ( -- )
    readln " " split1
    [ string>number ] bi@ +
    number>string print ;
```


```txt

( scratchpad ) a+b
2 2
4

```



## FALSE


```false
[0[^$$'9>'0@>|~]['0-\10*+]#%]n:  {read an integer}
n;!n;!+.
```



## Fantom


```fantom
class APlusB
{
  public static Void main ()
  {
    echo ("Enter two numbers: ")
    Str input := Env.cur.in.readLine
    Int sum := 0
    input.split.each |n| { sum += n.toInt }
    echo (sum)
  }
}
```



## FBSL

Using stdin and stdout

```qbasic
#APPTYPE CONSOLE

DIM %a, %b
SCANF("%d%d", @a, @b)
PRINT a, "+", b, "=", a + b

PAUSE
```



## Fish


```Fish
i:o:"-"=?v1$68*-v
v        >~01-0 >
>i:o:" "=?v68*-$a*+
          >~*i:o:"-"=?v1$68*-v
v                     >~01-0 >
>i:o:d=?v68*-$a*+
        >~*+aonao;
```



## Forth


```Forth>pad dup 80 accept evaluate + .</lang



## Fortran


```fortran
program a_plus_b
  implicit none
  integer :: a,b
  read (*, *) a, b
  write (*, '(i0)') a + b
end program a_plus_b
```



## Frink

This program handles arbitrarily-large integers, or even floating-point or rational numbers or complex numbers (as long as they're not internally separated with spaces, of course, which are the delimiters for this task.)  It can even handle units of measure (with no embedded spaces) such as "3.3meter 2feet" and does the right thing when summing those units.  It can handle any number of arbitrary whitespace characters separating the numbers.  It also works whether the input is user-interactive, or input comes from stdin or a pipe.  (It will bring up a user dialog for input when run in a graphical environment.)

```frink

sum[eval[split[%r/\s+/, input[""]]]]

```



## FunL


```funl
println( sum(map(int, readLine().split(' +'))) )
```



## Gambas


```gambas
Public Sub Main()
Dim sInput As String = InputBox("Input 2 numbers seperated by a space", "A + B")

Print Split(sInput, " ")[0] & " + " & Split(sInput, " ")[1] & " = " & Str(Val(Split(sInput, " ")[0]) + Val(Split(sInput, " ")[1]))

End
```

Output:

```txt

999 + 888 = 1887

```



## Gastona

Taking A and B from command line arguments

```gastona
#listix#

   <main>
      "@<p1> + @<p2> = "
      =, p1 + p2

```

Using Graphical interface

```gastona
#javaj#

   <layout of main>
      EVALAYOUT, 6, 6, 3,
         , A   , A
         , lA  , eA
         , lB  , eB
         , bSum, eRes

#listix#

   <-- bSum>
      MSG, eRes data!,, @<suma>

   <suma> =, eA + eB

```



## Gema


```gema><D> <D
=@add{$1;$2}
```



## Genie


```genie
[indent=4]
/*
  A+B in Genie
  valac aplusb-genie.gs
  ./aplusb-genie
*/
init
    a:int64 = 0
    b:int64 = 0
    leftover:string = ""

    print "Enter A and B, two numbers separated by space"
    line:string = stdin.read_line()
    res:bool = int64.try_parse(line, out a, out leftover)
    res = int64.try_parse(leftover, out b)

    warning:string = " outside range (-1000, 1000), but it's ok, no one will tell"
    if a < -1000 or a > 1000
        print "A" + warning
    if b < -1000 or b > 1000
        print "B" + warning

    print "From %s\nA + B = %llu", line, a+b
```


{{out}}

```txt
prompt$ valac aplusb-genie.gs
prompt$ ./aplusb-genie
Enter A and B, two numbers separated by space
20 22
From 20 22
A + B = 42
prompt$ echo '123 234' | ./aplusb-genie
Enter A and B, two numbers separated by space
From 123 234
A + B = 357
prompt$ echo '123 2345' | ./aplusb-genie
Enter A and B, two numbers separated by space
B outside range (-1000, 1000), but it's ok, no one will tell
From 123 2345
A + B = 2468
```



## GML


```GML
var add, a, b;
add = argument0; // get the string with the numbers to add
a = real(string_copy(add, 1, string_pos(" ", add)));
b = real(string_copy(add, string_pos(" ", add) + 1, string_length(add) - string_pos(" ", add)));
return(a + b);
```



## Go


```go
package main

import "fmt"

func main() {
    var a, b int
    fmt.Scan(&a, &b)
    fmt.Println(a + b)
}
```



## Golfscript


```golfscript
~+
```



## Golo


```Golo
#!/usr/bin/env golosh
----
This module asks for two numbers, adds them, and prints the result.
----
module Aplusb

import gololang.IO

function main = |args| {

  let line = readln("Please enter two numbers (just leave a space in between them) ")
  let numbers = line: split("[ ]+"): asList()

  require(numbers: size() == 2, "we need two numbers")

  try {

    let a, b = numbers: map(|i| -> i: toInt())

    require(a >= -1000 and a <= 1000 and b >= -1000 and b <= 1000, "both numbers need to be between -1000 and 1000")

    println(a + b)

  } catch (e) {
    println("they both need to be numbers for this to work")
  }
}
```



## Gosu


```Gosu

uses java.io.InputStreamReader
uses java.util.Scanner
uses java.lang.System

var scanner = new Scanner( new InputStreamReader( System.in ) )
var a = scanner.nextInt()
var b = scanner.nextInt()

print( a + b )

```



## Groovy


```groovy
def abAdder = {
    def reader = new Scanner(System.in)
    def a = reader.nextInt();
    def b = reader.nextInt();
    assert (-1000..1000).containsAll([a,b]) : "both numbers must be between -1000 and 1000 (inclusive)"
    a + b
}
abAdder()
```



## GUISS

We cannot use variables, but we can find the sum of two numbers.Here we add 3 + 2:

```guiss
Start,Programs,Accessories,Calculator,Button:3,Button:[plus],
Button:2,Button:[equals]
```



## Haskell


```haskell>main =  print . sum . map read . words =<< getLine</lang



## hexiscript


```hexiscript
fun split s delim
  let ret    dict 32
  let l      len s
  let j      0
  let ret[0] ""
  for let i 0; i < l; i++
    if s[i] = delim
      if len ret[j] > 0
        let ret[++j] ""
      endif
      continue
    endif
    let ret[j] (ret[j] + s[i])
  endfor
  return ret
endfun

let nums split (scan str) ' '
let a    tonum nums[0]
let b    tonum nums[1]
println a + b
```



## HicEst

A and B are input via edit controls with spinners limiting inputs to +-1000.

```HicEst
DLG(Edit=A, DNum, MIn=-1000, MAx=1000, E=B, DN, MI=-1000, MA=1000)
WRITE(Messagebox, Name) A, B, "Sum = ", A+B
```



## Hope

This being my first hope program, and having no clue how to (have hope) read from stdin,
I installed hope (but not hopeless) from https://github.com/dmbaturin/hope,
read the Byte magazine article from 1980s which is a recast of the tutorial found in the github archive.
Congratulations, the program worked on my first attempt.

```txt

$ cd lib
$ ../src/hope
>: dec add : num # num -> num;
>: --- add(a,b) <= a + b;
>: add(3,99)
>: ^D
>> 102 : num

```



## Hy


```hy
(print (sum (map int (.split (input)))))
```

Alternatively, with the "threading tail" macro:

```hy
(->> (input) (.split) (map int) (sum) (print))
```



## Huginn


```huginn
import Algorithms as algo;
import Text as text;

main() {
  print(
    "{}\n".format(
      algo.reduce(
        algo.map(
          text.split( input().strip(), " " ),
          integer
        ),
        @( x, y ){ x + y; }
      )
    );
  );
}
```



## i


```i
main: print(integer(in(' '))+integer(in('\n'))); ignore
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
     numChars := '-'++&digits
     read() ? {
         A := (tab(upto(numChars)), integer(tab(many(numChars))))
         B := (tab(upto(numChars)), integer(tab(many(numChars))))
         }
     write((\A + \B) | "Bad input")
end
```



## Idris


```idris
main : IO()
main = do
  line <- getLine
  print $ sum $ map cast $ words line
```



## J

Typically, in J, you would find the sum of two numbers (let us say 2 and 3) by entering both of them on a line with a + sign between them:

```J
   2+3
5
```

In the following expression, <tt>1!:1(3)</tt> reads a line from STDIN; <tt>-.LF</tt> drops the line ending character; <tt>".</tt> converts the remaining text to a sequence of numbers which are then summed using <tt>+/</tt>.

```J
+/". (1!:1(3))-.LF
```

Here's a little script, called "a+b.ijs":

```J
#!/Applications/j602/bin/jconsole
echo +/". (1!:1(3))-.LF
exit ''
```

Here is the execution of the script:

```bash
echo 2 3 | ./a+b.ijs
5
```



## Java


```java
import java.util.*;

public class Sum2 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in); // Standard input
        System.out.println(in.nextInt() + in.nextInt()); // Standard output
    }
}
```

Object of [[class]] Scanner works slow enough, because of that contestants prefer to avoid its use. Often, longer solution works faster and easily scales to problems.

```java
import java.io.*;
import java.util.*;

public class SumDif {
   StreamTokenizer in;
   PrintWriter out;

   public static void main(String[] args) throws IOException {
      new SumDif().run();
   }

   private int nextInt() throws IOException {
      in.nextToken();
      return (int)in.nval;
   }

   public void run() throws IOException {
      in = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in))); // Standard input
      out = new PrintWriter(new OutputStreamWriter(System.out)); // Standard output
      solve();
      out.flush();
   }

   private void solve() throws IOException {
      out.println(nextInt() + nextInt());
   }
}
```


The following code uses a StreamTokenizer instead of a Scanner.


```java
import java.io.*;
import java.nio.charset.Charset;

public class AplusB {
    public static void main(String[] args) throws IOException {
        StreamTokenizer in = new StreamTokenizer(new InputStreamReader(System.in, Charset.defaultCharset()));
        in.nextToken();
        int a = (int) in.nval;
        in.nextToken();
        int b = (int) in.nval;

        try (Writer out = new OutputStreamWriter(System.out, Charset.defaultCharset())) {
            out.write(Integer.toString(a + b));
        }
    }
}

```



<lang>
grammar aplusb ;

options {
	language = Java;
}

aplusb	:	(WS* e1=Num WS+ e2=Num NEWLINE {System.out.println($e1.text + " + " + $e2.text + " = " + (Integer.parseInt($e1.text) + Integer.parseInt($e2.text)));})+
	;
Num	:	'-'?('0'..'9')+
	;
WS	:	(' ' | '\t')
	;
NEWLINE	:	WS* '\r'? '\n'
        ;

```

Produces:

```txt

>java Test
1 2
23 89
13 567
-75 6
-75 -29
^Z
1 + 2 = 3
23 + 89 = 112
13 + 567 = 580
-75 + 6 = -69
-75 + -29 = -104

```



## JavaScript


###  ES5

Client side:


```html4strict><html

<body>
<div id='input'></div>
<div id='output'></div>
<script type='text/javascript'>
var a = window.prompt('enter A number', '');
var b = window.prompt('enter B number', '');
document.getElementById('input').innerHTML = a + ' ' + b;

var sum = Number(a) + Number(b);
document.getElementById('output').innerHTML = sum;
</script>
</body>
</html>
```


Server side (with [http://nodejs.org node.js]):


```javascript
process.openStdin().on (
    'data',
    function (line) {
        var xs = String(line).match(/^\s*(\d+)\s+(\d+)\s*/)
        console.log (
            xs ? Number(xs[1]) + Number(xs[2]) : 'usage: <number> <number>'
        )
        process.exit()
    }
)
```


 $ node io.js
 2 3
 5
 $ node io.js
 x 3
 usage: <integer> <integer>


###  ES6

Node.js in a terminal:

```javascript
process.stdin.on("data", buffer => {
  console.log(
    (buffer + "").trim().split(" ").map(Number).reduce((a, v) => a + v, 0)
  );
});

```



```txt
 $ node io.js
 2 3
 5

```



###  JScript Windows Script Host Version 5.8


```javascript
var a = WScript.StdIn.ReadLine();
var b = WScript.StdIn.ReadLine();
WSH.echo(a, " + " , b , " = " , Number(a)+Number(b));

```



## Joy


### Console


```Joy>get get +.</lang


### File


```Joy
"input.txt" include
"output.txt" "w" fopen
get get + fput pop quit.
```



## jq

Since the given task is simply to add two numbers, the simplest approach in jq is illustrated by the following transcript:

```jq
$ jq -s add
3 2
5
```

This will work provided the numbers are neither too small nor too large.  However, the above program will add **all** the numbers presented on the stream (assuming only numbers are presented).  If the task were to add consecutive pairs of numbers, then the approach illustrated in the following transcript can be used, in conjunction with the jq "-s" option:
```jq

def addpairs:
  if length < 2 then empty
  else (.[0] + .[1]), (.[2:] | addpairs)
  end;

addpairs
```

For example, here is a transcript that assumes the program is in a file named AB.jq:
```jq

$ jq -s -f AB.jq
1 2 3 4 5 6
3
7
11
```



## Jsish


```javascript
/* A+B in Jsish */
var line = console.input();
var nums = line.match(/^\s*([+-]?[0-9]+)\s+([+-]?[0-9]+)\s*/);
if (nums) {
    var A = Number(nums[1]);
    var B = Number(nums[2]);
    if (A <= 1000 && A >= -1000 && B <= 1000 && B >= -1000) {
        printf("%d\n", A + B);
    } else {
        puts("error: A and B both need to be in range -1000 thru 1000 inclusive");
    }
} else {
    puts("error: A+B requires two numbers separated by space");
}
```


{{out}}

```txt
prompt$ jsish A+B.jsi
a b
error: A+B requires two numbers separated by space
prompt$ jsish A+B.jsi
1234 123
error: A and B both need to be in range -1000 thru 1000 inclusive
prompt$ jsish A+B.jsi
-1000 +1000
0
prompt$ jsish A+B.jsi
123 -234
-111
```



## Julia

Run from the command line:

```julia
input = parse.(Int, split(readline(stdin)))
println(stdout, sum(input))
```


{{out}}

```txt
>julia AB.jl
1 1
2
```


In the next solution, an error is returned if the entry is not constituted from exactly two integers. Any number of spaces can follow an integer.

```Julia>julia
 println(parse(Int, readuntil(stdin, ' ')) + parse(Int, readuntil(stdin, '\n')))
1 2
3
```



## K


```K

  split:{(a@&~&/' y=/: a:(0,&x=y)_ x) _dv\: y}
  ab:{+/0$split[0:`;" "]}
  ab[]
2 3
5

```



## Klong


```K

  {(1:$(*x?0c )#x)+1:$(1+*|x?0c )_x}@.rl()
2 3
5

```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    val r = Regex("""-?\d+[ ]+-?\d+""")
    while(true) {
        print("Enter two integers separated by space(s) or q to quit: ")
        val input: String = readLine()!!.trim()
        if (input == "q" || input == "Q") break
        if (!input.matches(r)) {
            println("Invalid input, try again")
            continue
        }
        val index = input.lastIndexOf(' ')
        val a = input.substring(0, index).trimEnd().toInt()
        val b = input.substring(index + 1).toInt()
        if (Math.abs(a) > 1000 || Math.abs(b) > 1000) {
            println("Both numbers must be in the interval [-1000, 1000] - try again")
        }
        else {
            println("Their sum is ${a + b}\n")
        }
    }
}
```


{{out}}

```txt

Enter two integers separated by space(s) or q to quit: 2 2
Their sum is 4

Enter two integers separated by space(s) or q to quit: 3 2
Their sum is 5

Enter two integers separated by space(s) or q to quit: q

```



## KQL


```KQL
datatable(Input:string)[
    '2 2',
    '3 2'
]
| parse Input with A:int ' ' B:int
| project Input, Output = A + B
```



## L++


```lisp
(main
  (decl int a)
  (decl int b)
  (>> std::cin a b)
  (prn (+ a b)))
```



## Lasso


```lb
[a + b]
```



## Lang5


```lang5
read read + .

read " " split expand drop + .
```



## LIL


```tcl
# A+B, in LIL
# Requires lil shell readline routine
set in [readline]
set A [index $in 0]
set B [index $in 1]
if [expr $A < -1000 || $A > 1000] { print "A out of range: $A"; exit 1 }
if [expr $B < -1000 || $B > 1000] { print "B out of range: $B"; exit 1 }
print [expr $A + $B]
```


{{out}}

```txt
prompt$ echo '40 2' | lil AB.lil
42
```



## Lisaac


```lisaac
Section Header
 + name := A_PLUS_B

Section Public
 - main <- (    (IO.read_integer; IO.last_integer) +
                (IO.read_integer; IO.last_integer) ).println;
```



## Little


```c
void main() {
    string a, b;
    scan(gets(stdin), "%d %d", &a, &b);
    puts(((int)a + (int)b));
}
```



## LiveCode

Using Livecode Server script

```LiveCode
<?lc
if isNumber($0) and isNumber($1) then
    put $0 + $1
else
    put $0 && $1
end if
?>
```


A graphical version using an input dialog

```LiveCode
on mouseUp
    ask "Enter two numbers"
    set itemdelimiter to space
    put it into nums
    if isNumber(item 1 of nums) and isNumber(item 2 of nums) then
        answer item 1 of nums + item 2 of nums
    else
        answer item 1 of nums && item 2 of nums
    end if
end mouseUp
```



## Logo


```logo
show apply "sum readlist
```



## Lua


```Lua
a,b = io.read("*number", "*number")
print(a+b)
```



## Kite


```Kite
#!/usr/bin/kite

import "System.file";

in = System.file.stdin;
line = in|readline;
while(not (line is null)) [
    arry = line|split(" ");
    result = (arry[0])|int + (arry[1])|int;
    result|print;

    line = in|readline;
];
```

{{Out}}

```txt

$ kite a_plus_b.kt <<EOF
5 6
EOF
11
$
```



## M2000 Interpreter

<lang>Def Range(X%)=Abs(X%)<=1000
Do {
      Input A%, B%
} Until Range(A%) And Range(B%)
Print A%+B%
```



## M4


```M4
 define(`sumstr', `eval(patsubst(`$1',` ',`+'))')

sumstr(1 2)
3
```



## Maple


```maple
 convert( scanf( "%d %d" ), '`+`' );
23 34
                                   57
```



## Mathematica

Interactive in a notebook

```Mathematica
Input[] + Input[]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function sumOfInputs = APlusB()
    inputStream = input('Enter two numbers, separated by a space: ', 's');
    numbers = str2num(inputStream);                         %#ok<ST2NM>
    if any(numbers < -1000 | numbers > 1000)
        warning('APlusB:OutOfRange', 'Some numbers are outside the range');
    end
    sumOfInputs = sum(numbers);
end
```



## Maude

===Built-in===

```Maude

red 3 + 4 .

```


### With restrictions


```Maude

fmod ADD is

	protecting INT .

	op undefined : -> Int .
	op _add_ : Int Int -> Int [assoc comm] .

	vars A B : Int .

	eq A add B = if (A < -1000 or B < -1000) or (A > 1000 or B > 1000) then undefined else A + B fi .

endfm

```



## Mercury

<lang>:- module a_plus_b.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
   io.read_line_as_string(Result, !IO),
   ( if
        Result = ok(Line),
        [AStr, BStr] = string.words(Line),
        string.to_int(AStr, A), string.to_int(BStr, B)
     then
        io.format("%d\n", [i(A + B)], !IO)
     else
        true
    ).
```



## Maxima

<lang>in_stream: openr("/dev/stdin");
unless (line: readline(in_stream), line=false) do (
                 q: map('parse_string, split(line, " ")),
                 print(q[1]+q[2])
      );
close(in_stream);

```



## min

{{works with|min|0.19.3}}

```min
gets " " split 'bool filter 'int map sum puts!
```



## MiniScript

The <code>input</code> intrinsic in MiniScript isn't available in all implementations, so we've just hard-coded the input here:

```MiniScript
s = "  2    3  "
fields = s.split
for i in range(fields.len-1, 0)
    if fields[i] == "" then fields.remove i
end for
if fields.len < 2 then
    print "Not enough input"
else
    print val(fields[0]) + val(fields[1])
end if
```


{{out}}

```txt
5
```




## mIRC Scripting Language


```mirc
alias a+b {
  echo -ag $calc($1 + $2)
}
```


=={{header|MK-61/52}}==

```txt

С/П + С/П

```



## ML/I

The two numbers are read from 'standard input' or its equivalent.

```ML/I
MCSKIP "WITH" NL
"" A+B
"" assumes macros on input stream 1, terminal on stream 2
MCSKIP MT,<>
MCINS %.
MCDEF SL SPACES NL AS <MCSET T1=%A1.
MCSET T2=%A2.
%T1+T2.
MCSET S10=0
>
MCSKIP SL WITH *
MCSET S1=1
*MCSET S10=2
```


=={{header|Modula-2}}==

```modula2
MODULE  ab;

IMPORT  InOut;

VAR     A, B    : INTEGER;

BEGIN
  InOut.ReadInt (A);
  InOut.ReadInt (B);
  InOut.WriteInt (A + B, 8);
  InOut.WriteLn
END ab.
```



## MoonScript


```moonscript
a,b = io.read '*number','*number'
print a + b
```



## MUMPS


```MUMPS
ANB
 NEW A,B,T,S
 READ !,"Input two integers between -1000 and 1000, separated by a space: ",S
 SET A=$PIECE(S," ",1),B=$PIECE(S," ",2)
 SET T=(A>=-1000)&(A<=1000)&(B>=-1000)&(B<=1000)&(A\1=A)&(B\1=B)
 IF T WRITE !,(A+B)
 IF 'T WRITE !,"Bad input"
 QUIT
```



## Neko


```ActionScript
/**
 A+B, Rosetta Code, in Neko
 Tectonics:
   nekoc a+b.neko
   echo '4 5' | neko a+b.n
*/

/* load some primitives */
var regexp_new = $loader.loadprim("regexp@regexp_new", 1)
var regexp_match = $loader.loadprim("regexp@regexp_match", 4)
var regexp_matched = $loader.loadprim("regexp@regexp_matched", 2)

var stdin = $loader.loadprim("std@file_stdin", 0)()
var file_read_char = $loader.loadprim("std@file_read_char", 1)

/* Read a line from file f into string s returning length without any newline */
var NEWLINE = 10
var readline = function(f, s) {
    var len = 0
    var ch
    while true {
        try ch = file_read_char(f) catch a break;
        if ch == NEWLINE break;
        if $sset(s, len, ch) == null break; else len += 1
    }
    return len
}

/* Trim a string of trailing NUL and spaces, returning substring */
var SPACE = 32
var trim = function(s) {
    var len = $ssize(s)
    var ch
    while len > 0 {
        ch = $sget(s, len - 1)
        if ch != 0 && ch != SPACE break; else len -= 1
    }
    return $ssub(s, 0, len)
}

/* The A+B task */
var RECL = 132
try {
    /* whitespace(s), digit(s), whitespace(s), digit(s) */
    var twonums = regexp_new("^\\s*(\\d+)\\s+(\\d+)\\b")
    var s = $smake(RECL)
    var len = readline(stdin, s)
    s = trim(s)

    var valid = regexp_match(twonums, s, 0, $ssize(s))
    if valid {
        var first = regexp_matched(twonums, 1)
        var second = regexp_matched(twonums, 2)

        first = $int(first)
        second = $int(second)

        if first < -1000 || first > 1000 $throw("First value out of range -1000,1000")
        if second < -1000 || second > 1000 $throw("Second value out of range -1000,1000")

        $print($int(first) + $int(second), "\n")

    } else $print("Need two numbers, separated by whitespace\n")

} catch with $print("Exception: ", with, "\n")
```


{{out}}

```txt
prompt$ nekoc a+b.neko
prompt$ echo '2 2' | neko a+b
4
prompt$ neko a+b
2 3
5
```



## Nemerle

{{trans|C#}}

```Nemerle
using System;
using System.Console;
using System.Linq;

module AplusB
{
    Main() : void
    {
        WriteLine(ReadLine().Split().Select(int.Parse).Sum());
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java symbols binary

parse ask a b .
say a '+' b '=' a + b
```



## newLISP


```newLISP
(println (apply + (map int (parse (read-line)))))
```



## Nim

A+B:

```nim
import strutils, os

echo parseInt(paramStr(1)) + parseInt(paramStr(2))
```

Arbitrary number of arguments:

```nim
import strutils, os
var sum = 0
for i in countup(1, paramCount()):
  sum = sum + parseInt(paramStr(i))
echo sum
```


another:


```nim
from strutils import parseFloat, formatFloat, ffDecimal

proc aplusb(a,b: float): float =
  return a + b

proc getnumber(): float =
  try:
    parseFloat(readLine(stdin))
  except ValueError:
    echo("Please enter a number: ")
    getnumber()

echo("First number please: ")
let first: float = getnumber()

echo("Second number please: ")
let second: float = getnumber()

echo("Result: " & formatFloat(aplusb(first, second), ffDecimal, 2))
```



## Nit

Generic non-robust version (source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/ab.nit the Nit’s official repository]):

```nit
module ab

var words = gets.split(" ")
if words.length != 2 then
	print "Expected two numbers"
	return
end
print words[0].to_i + words[1].to_i
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 INPUT "ENTER NUMBER A: ",A
20 INPUT "ENTER NUMBER B: ",B
30 PRINT A+B
```



## Nyquist


### SAL Syntax


```Nyquist
;nyquist plug-in
;version 1
;type tool
;name "A+B"
;debugflags trace

define variable a = 1
define variable b = 9

print a + b

return ""
```


===Audacity plug-in (SAL syntax)===

```Nyquist
;nyquist plug-in
;version 1
;type tool
;name "A+B"
;debugflags trace

define variable a = 1
define variable b = 9

print a + b

return ""
```



## Objeck


```objeck
bundle Default {
   class Vander {
      function : Main(args : String[]) ~ Nil {
         values := IO.Console->ReadString()->Split(" ");
         if(values->Size() = 2) {
            (values[0]->Trim()->ToInt() + values[1]->Trim()->ToInt())->PrintLine();
         };
      }
   }
}
```


=={{header|Oberon-2}}==

```oberon2
MODULE  ab;

IMPORT  In, Out;

VAR     A, B    : INTEGER;

BEGIN
  In.Int (A);
  In.Int (B);
  Out.Int (A + B, 8);
  Out.Ln
END ab.
```

Producing

```txt

12 34
      46

```



## OCaml


```ocaml
Scanf.scanf "%d %d" (fun a b -> Printf.printf "%d\n" (a + b))
```



## Oforth


Works with any number of integers separated by a space.


```Oforth
import: mapping

System.Console accept words map( #>integer) reduce( #+ ) printcr .
```



## Ol

Note: input data must be separated by newline ([Enter] key press).


```ol
; simplest
(+ (read) (read))

; safe
(let ((a (read))
      (b (read)))
   (if (not (number? a))
      (runtime-error "a is not a number! got:" a))
   (if (not (number? b))
      (runtime-error "b is not a number! got:" b))

   (print a " + " b " = " (+ a b)))

```



## Onyx



```onyx
$Prompt {
  `\nEnter two numbers between -1000 and +1000,\nseparated by a space: ' print flush
} def

$GetNumbers {
  mark stdin readline pop # Reads input as a string. Pop gets rid of false.
  cvx eval # Convert string to integers.
} def

$CheckRange { # (n1 n2 -- bool)
  dup -1000 ge exch 1000 le and
} def

$CheckInput {
  counttomark 2 ne
    {`You have to enter exactly two numbers.\n' print flush quit} if
  2 ndup CheckRange exch CheckRange and not
    {`The numbers have to be between -1000 and +1000.\n' print flush quit} if
} def

$Answer {
  add cvs `The sum is ' exch cat `.\n' cat print flush
} def

Prompt GetNumbers CheckInput Answer
```



## ooRexx


### version 1

{{trans|REXX}}

```oorexx
Numeric digits 1000             /*just in case the user gets ka-razy. */
Say 'enter some numbers to be summed:'
parse pull y
yplus=add_plus(y)
sum=0
Do While y<>''
  Parse Var y n y
  If datatype(n)<>'NUM' Then Do
    Say 'you entered  something that is not recognized to be a number:' n
    Exit
    End
  sum+=n
  End
Say yplus '=' sum/1
Exit
add_plus:
Parse arg list
list=space(list)
return translate(list,'+',' ')
```

{{out}}

```txt
enter some numbers to be summed:
1e10+7.777+33 = 10000000040.777
```


### version 2

extend for negative numbers

```oorexx
Numeric digits 1000
Say 'enter some numbers to be summed:'
parse pull y
sum=0
yplus=''
Do i=1 By 1 While y<>''
  Parse Var y n y
  If datatype(n)<>'NUM' Then Do
    Say 'you entered  something that is not recognized to be a number:' n
    Exit
    End
  Select
    When i=1 Then
      yplus=n
    When n>0 Then yplus||='+'abs(n)
    Otherwise yplus||=n
    End
  sum+=n
  End
Say yplus '=' sum/1
Exit
```



## OpenEdge/Progress


```progress
DEFINE VARIABLE a AS INTEGER NO-UNDO FORMAT "->>>9".
DEFINE VARIABLE b AS INTEGER NO-UNDO FORMAT "->>>9".

IF SESSION:BATCH THEN DO:
   INPUT FROM "input.txt".
   IMPORT a b.
   INPUT CLOSE.
END.
ELSE
   UPDATE a b.

MESSAGE a + b VIEW-AS ALERT-BOX
```



## Openscad

There is no means of run-time input in Openscad

```openscad

a = 5 + 4;
echo (a);

```



## Oxygene


```oxygene

// Sum 2 integers read fron standard input
//
// Nigel Galloway - April 16th., 2012
//
namespace aplusb;

interface
  uses System.Text.RegularExpressions.*;

type
  aplusb = class
  public
    class method Main;
  end;

implementation

class method aplusb.Main;
var
  gc: GroupCollection;
  m : Match;
begin
  m := new Regex('^\s*(?<a>-?[1-9]\d{0,2}|0|-?1000)\s+(?<b>-?[1-9]\d{0,2}|0|-?1000)\s*$').Match(Console.ReadLine());
  if m.Success then
    begin
      gc := m.Groups;
      Console.WriteLine("{0} + {1} = {2}", gc['a'].Value, gc['b'].Value, Integer.Parse(gc['a'].Value) + Integer.Parse(gc['b'].Value));
    end
  else Console.WriteLine("Invalid Input");
end;

end.

```

Produces:

```txt

>aplusb
23 -99
23 + -99 = -76

```



## Oz


```oz
declare
  class TextFile from Open.file Open.text end

  StdIn = {New TextFile init(name:stdin)}

  fun {ReadInt}
     {String.toInt {StdIn getS($)}}
  end
in
  {Show {ReadInt}+{ReadInt}}
```



## PARI/GP

User input:

```parigp
input()+input()
```

File input:

```parigp
read("file1")+read("file2")
```



## Pascal


```pascal
var
   a, b: integer;
begin
   readln(a, b);
   writeln(a + b);
end.
```

Same with input from file <tt>input.txt</tt> and output from file <tt>output.txt</tt>.

```pascal
var
   a, b: integer;
begin
   reset(input, 'input.txt');
   rewrite(output, 'output.txt');
   readln(a, b);
   writeln(a + b);
   close(input);
   close(output);
end.
```


### Version 2. Following the rules


```pascal
{ Task: A + B
Sum of A + B while A, B >= -1000 and A,B <= 1000
Author: Sinuhe Masan (2019) }
program APlusB;

var
    A, B : integer;

begin
    repeat
        write('Enter two numbers betwen -1000 and 1000 separated by space: ');
        readln(A, B);

    until ((abs(A) < 1000) and (abs(B) < 1000));

    writeln('The sum is: ', A + B);

end.
```



## Perl


```Perl
my ($a,$b) = split(' ', scalar(<STDIN>));
print "$a $b " . ($a + $b) . "\n";
```



###  using the List::Util module


```Perl
say sum split /\s+/,  scalar <STDIN>;
```



## Perl 6

{{works with|rakudo|2015.12}}

Short version with very little "line noise":

```perl6>get.words.sum.say;</lang

Reduction operator <code>[+]</code>, and <code>say</code> as a function:

```perl6
say [+] get.words;
```

Long version:

```perl6
my ($a, $b) = $*IN.get.split(" ");
say $a + $b;
```



## Phix


```Phix
-- demo\rosetta\AplusB.exw
string s = prompt_string("Enter two numbers separated by a space : ")
sequence r = scanf(s,"%d %d")
if length(r)=1 then
    integer {a,b} = r[1], c = a+b
    printf(1,"%d + %d = %d\n",{a,b,c})
else
    printf(1,"invalid input\n")
end if
```

{{out}}

```txt

Enter two numbers separated by a space : 2 3
2 + 3 = 5

```



## PHP


```php
fscanf(STDIN, "%d %d\n", $a, $b); //Reads 2 numbers from STDIN
echo ($a + $b) . "\n";
```


```php
$in = fopen("input.dat", "r");
fscanf($in, "%d %d\n", $a, $b); //Reads 2 numbers from file $in
fclose($in);

$out = fopen("output.dat", "w");
fwrite($out, ($a + $b) . "\n");
fclose($out);
```



## PicoLisp


```PicoLisp
(+ (read) (read))
3 4
-> 7
```



## Piet

[[File:Piet A+B.png]]
The code is fairly straightforward. The individual commands are as follows:

```text
in(num)
in(num)
add
out(num)
```



## Pike


```Pike
string line = Stdio.stdin->gets();
sscanf(line, "%d %d", int a, int b);
write(a+b +"\n");
```



## PL/I


```pli
get (a, b);
put (a+b);
```


## Pony


```pony

actor Main
    let _env:Env
    new create(env:Env)=>
        _env=env
        env.input(object iso is InputNotify
            let _e:Main=this
            fun ref apply(data:Array[U8] iso)=>
                _e(consume data)
            fun ref dispose()=>
                None
        end,
            512)
    be apply(s:Array[U8] iso)=>
        let c=String.from_iso_array(consume s)
        let parts:Array[String]=c.split(" ",0)
        var sum:I32=0
        try
            for v in parts.values() do
                sum=sum+match v.read_int[I32](0)?
                |(let x:I32,_)=>x
                end
            end
        end
        _env.out.print(sum.string())


```



## PostScript


```postscript
(%stdin) (r) file  % get stdin
dup
token pop          % read A
exch
token pop          % read B
add
=
```



## Potion


```potion
# The numbers are entered, piped, or redirected in via STDIN and the format is proper (i.e., "%d %d").
input = read
i = 0
while (i < input length):
   if (input(i) == " "):
      break
   .
   i++
.
(input slice(0, i) number + input slice(i, nil) number) print

# The numbers are manually inputted, but the format is improper (i.e., "%d\n%d\n").
(read number + read number) print
```



## PowerShell


```powershell
$a,$b = -split "$input"
[int]$a + [int]$b
```

This solution does not work interactively, while the following ''only'' works interactively:

```powershell
$a,$b = -split (Read-Host)
[int]$a + [int]$b
```


I think this works better and doesn't require string input (following the task closer):

```powershell
filter add {
    return [int]$args[0] + [int]$args[1]
}
```


Can be called in one line with

```powershell>add 2 3</lang



## Processing


### Rudimentary User Interface

Click on either side to add 1 to its value.

```Processing
int a = 0;
int b = 0;

void setup() {
  size(200, 200);
}

void draw() {
  fill(255);
  rect(0, 0, width, height);
  fill(0);
  line(width/2, 0, width/2, height * 3 / 4);
  line(0, height * 3 / 4, width, height * 3 / 4);
  text(a, width / 4, height / 4);
  text(b, width * 3 / 4, height / 4);
  text("Sum: " + (a + b), width / 4, height * 7 / 8);
}

void mousePressed() {
  if (mouseX < width/2) {
    a++;
  } else {
    b++;
  }
}
```


[https://i.imgur.com/QEHtMyA.jpg What the GUI looks like.]


## ProDOS

With the math module:

```ProDOS
editvar /newvar /value=a /title=Enter an integer:
editvar /newvar /value=b /title=Enter another integer:
editvar /newvar /value=c
do add -a-,-b-=-c-
printline -c-
```

Without the math module:

```ProDOS
editvar /newvar /value=a /title=Enter an integer:
editvar /newvar /value=b /title=Enter another integer:
editvar /newvar /value=c=-a-+-b-
printline -c-
```



## Prolog

{{Works with|SWI-Prolog}}

```Prolog
plus :-
    read_line_to_codes(user_input,X),
    atom_codes(A, X),
    atomic_list_concat(L, ' ', A),
    maplist(atom_number, L, LN),
    sumlist(LN, N),
    write(N).
```

output :

```Prolog
?- plus.
|: 4 5
9
true.
```



## Pure


```pure
using system;
printf "%d\n" (x+y) when x,y = scanf "%d %d" end;
```



## PureBasic


### Console


```PureBasic
x$=Input()
a=Val(StringField(x$,1," "))
b=Val(StringField(x$,2," "))
PrintN(str(a+b))
```


### File


```PureBasic
If ReadFile(0,"in.txt")
  x$=ReadString(0)
  a=Val(StringField(x$,1," "))
  b=Val(StringField(x$,2," "))
  If OpenFile(1,"out.txt")
    WriteString(1,str(a+b))
    CloseFile(1)
  EndIf
  CloseFile(0)
EndIf
```



## Python


### Console

In Python 2, <code>input</code> returns ints, while <code>raw_input</code> returns strings.
In Python 3, <code>input</code> returns strings, and <code>raw_input</code> does not exist.

The first two lines allow the program to be run in either Python 2 or 3. In Python 2, <code>raw_input</code> exists, and the lines are effectively skipped. In Python 3, calling <code>raw_input</code> triggers an error, so the <code>except</code> loop activates and assigns "raw_input" the value of Python 3's "input" function. Regardless of version, these two lines make sure that <code>raw_input</code> will return a string.


```python
try: raw_input
except: raw_input = input

print(sum(map(int, raw_input().split())))
```



### File

For Python 2.X and 3.X taking input from stdin stream which can be redirected to be file input under Unix

```python
import sys

for line in sys.stdin:
    print(sum(map(int, line.split())))
```



## QB64


```QB64

DIM a AS INTEGER, b AS INTEGER
DIM c AS LONG
INPUT "Enter A: ", a
INPUT "Enter B: ", b
c = a + b
PRINT ""
PRINT "A + B = " + LTRIM$(STR$(c))

```



## R


```r
sum(scan("", numeric(0), 2))
```



## Ra


```Ra

class Sum
	**Adds two given integers**

	on start

		args := program arguments

		if args empty
			print to Console.error made !, "No arguments given"
			exit program with error code

		if args.count = 1
			print to Console.error made !, "Only one argument given"
			exit program with error code

		try
			print integer.parse(args[0]) + integer.parse(args[1])

		catch FormatException
			print to Console.error made !, "Arguments must be integers"
			exit program with error code

		catch OverflowException
			print to Console.error made !, "Numbers too large"
			exit program with error code

```



## Racket



```racket

#lang racket
(+ (read) (read))

```


Or, with additional error checking:

```racket

#lang racket
(define a (read))
(unless (number? a) (error 'a+b "number" a))
(define b (read))
(unless (number? b) (error 'a+b "number" b))
(displayln (+ a b))

```



## REBOL


```rebol
forever [x: load input  print x/1 + x/2]
```

{{Out}}

```txt
1 2
3
2 2
4
3 2
5
```



## Red


```Red>x: load input  print x/1 + x/2</lang

{{Out}}

```txt
1 2
3
2 2
4
3 2
5
```


Alternative implementations:

```Red
print (first x: load input) + x/2
```


```Red
print head insert load input 'add
```


```Red
print load replace input " " " + "
```



## Retro


```Retro
:try ("-n) s:get s:to-number s:get s:to-number + n:put ;
```


```Retro
try
1
2
```



## REXX

===version 1, unnormalized===
The numbers can be any valid REXX number (integer, fixed point decimal, floating point (with exponential notation, ···).

```rexx
/*REXX program obtains two numbers from the input stream (the console), shows their sum.*/
parse pull a b                                   /*obtain two numbers from input stream.*/
say a+b                                          /*display the sum to the terminal.     */
                                                 /*stick a fork in it,  we're all done. */
```

===version 2, normalizied===
If the user entered   '''4.00000'''   and wanted to add   '''5'''   to that, and expects   '''9''',

then the output needs to be normalized before displaying the result.

Normally, REXX will keep the greatest precision in the results;

adding   '''4.00000'''   and   '''5'''   will normally yield   '''9.00000'''

Dividing by one normalizes the number.

```rexx
/*REXX program obtains two numbers from the input stream (the console), shows their sum.*/
parse pull a b                                   /*obtain two numbers from input stream.*/
say (a+b) / 1                                    /*display normalized sum to terminal.  */
                                                 /*stick a fork in it,  we're all done. */
```


===version 3, extended precision===
Using the   '''numeric digits'''   statement allows more decimal digits to be used, the default is   '''9'''.

```rexx
/*REXX program obtains two numbers from the input stream (the console), shows their sum.*/
numeric digits 300                               /*the default is  nine  decimal digits.*/
parse pull a b                                   /*obtain two numbers from input stream.*/
z= (a+b) / 1                                     /*add and normalize sum, store it in Z.*/
say z                                            /*display normalized sum Z to terminal.*/
                                                 /*stick a fork in it,  we're all done. */
```


===version 4, multiple numbers===
This REXX version adds   ''all''   the numbers entered    (not just two).

```rexx
/*REXX program obtains some numbers from the input stream (the console), shows their sum*/
numeric digits 1000                              /*just in case the user gets  ka-razy. */
say 'enter some numbers to be summed:'           /*display a prompt message to terminal.*/
parse pull y                                     /*obtain all numbers from input stream.*/
many= words(y)                                   /*obtain the number of numbers entered.*/
$= 0                                             /*initialize the sum to zero.          */
              do j=1  for many                   /*process each of the numbers.         */
              $= $ + word(y, j)                  /*add one number to the sum.           */
              end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
say 'sum of '   many   " numbers = "   $/1       /*display normalized sum $ to terminal.*/
```


===version 5, multiple numbers, tongue in cheek===

```rexx
/*REXX program obtains some numbers from the input stream (the console), shows their sum*/
numeric digits 1000                              /*just in case the user gets  ka-razy. */
say 'enter some numbers to be summed:'           /*display a prompt message to terminal.*/
parse pull y                                     /*obtain all numbers from input stream.*/
y=space(y)
y=translate(y,'+',' ')
Interpret 's='y
say 'sum of '  many  " numbers = " s/1           /*display normalized sum s to terminal.*/
```



## Ring


```ring
give Numbers
Numbers = split(Numbers)
sum = 0
for x in Numbers sum += x next
see sum

func Split Str
for x in str if x = " " x = nl ok next
return str2list(str)
```



## Robotic


```robotic

input string "Input A:"
set "A" to "input"
input string "Input B:"
set "B" to "input"
* "('A' + 'B')"
end

```


Although the function in the first and third line asks for a string as the input, so long as the variable isn't made to store a string, it will default to an integer instead. Inserting a string to this will return a 0.


## Rockstar

Minimized:

```Rockstar

Listen to A number
Listen to B
Say A number plus B

```

Idiomatic:

```Rockstar

Listen to my voice
Listen to your thoughts
Shout your thoughts with my voice

```



## Ruby


```ruby
puts gets.split.sum(&:to_i)
```



## Run BASIC


```runbasic
input, x$
print  val(word$(x$,1)) + val(word$(x$,2))
```





## Rust


```rust
use std::io;

fn main() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("reading stdin");

    let mut i: i64 = 0;
    for word in line.split_whitespace() {
        i += word.parse::<i64>().expect("trying to interpret your input as numbers");
    }
    println!("{}", i);
}
```


or


```rust
use std::io;

fn main() {
    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("reading stdin");

    let sum: i64 = line.split_whitespace()
                       .map(|x| x.parse::<i64>().expect("Not an integer"))
                       .sum();
    println!("{}", sum);
}
```



## Scala


```scala
println(readLine().split(" ").map(_.toInt).sum)
```


This will work if the input is exactly as specified, with no extra whitespace. A slightly more robust version:


```scala
val s = new java.util.Scanner(System.in)
val sum = s.nextInt() + s.nextInt()
println(sum)
```


or


```scala
println(readLine().split(" ").filter(_.length>0).map(_.toInt).sum)
```



## Scheme


```scheme
(display (+ (read) (read)))
```



## Scratch

Scratch is a graphical programming language. Follow the link to see an example solution for A + B

[https://scratch.mit.edu/projects/327678813/ '''Scratch A + B''']

Since Scratch is an educational language, I've included comments in the code for new programmers to better understand what the program is doing.


## sed

Sed is for string processing and has no facility for manipulating numbers as numeric values. However, being Turing complete, sed can be coerced into performing mathematics.

```sed
: Loop
# All done
/^-*00* /s///
/ -*00*$/s///
t

# Negative Check
/^\(-*\)[0-9].* \1[0-9]/!b Negative

# Create magic lookup table
s/\(.[0-9]*\) \(.[0-9]*\)/\1;987654321000009999000999009909 \2;012345678999990000999000990090/
s/ \(-\)*\(9*;\)/ \10\2/
# Decrement 1st number
s/\([^0]\)\(0*\);[^0]*\1\(.\).*\2\(9*\).* \(.*\)/\3\4 \5/
# Increment 2nd
s/\([^9]\)\(9*\);[^9]*\1\(.\).*\2\(0*\).*/\3\4/
t Loop

: Negative
# Create magic lookup table
s/\(.[0-9]*\) \(.[0-9]*\)/\1;987654321000009999000999009909 \2;987654321000009999000999009909/
# Decrement 1st number
s/\([^0]\)\(0*\);[^0]*\1\(.\).*\2\(9*\).* \(.*\)/\3\4 \5/
# Decrement 2nd
s/\([^0]\)\(0*\);[^0]*\1\(.\).*\2\(9*\).*/\3\4/
t Loop
```


Another method, based off of [http://unix.stackexchange.com/a/36959/11750 this StackExchange answer]:

```sed
#!/bin/sed -f

# Add a marker in front of each digit, for tracking tens, hundreds, etc.
s/[0-9]/<&/g
# Convert numbers to, in essence, tally marks
s/0//g; s/1/|/g; s/2/||/g; s/3/|||/g; s/4/||||/g; s/5/|||||/g
s/6/||||||/g; s/7/|||||||/g; s/8/||||||||/g; s/9/|||||||||/g

# Multiply by ten for each digit from the back they were.
:tens
s/|</<||||||||||/g
t tens

# We don't want the digit markers any more
s/<//g

# Negative minus negative is the negation of their absolute values.
s/^-\(|*\) *-/-\1/
# Negative plus positive equals positive plus negative, and we want the negative at the back.
s/^-\(|*\) \+\(|*\)$/\2-\1/
# Get rid of any space between the numbers
s/ //g

# A tally on each side can be canceled.
:minus
s/|-|/-/
t minus
s/-$//

# Convert back to digits
:back
s/||||||||||/</g
s/<\([0-9]*\)$/<0\1/g
s/|||||||||/9/g;
s/|||||||||/9/g; s/||||||||/8/g; s/|||||||/7/g; s/||||||/6/g;
s/|||||/5/g; s/||||/4/g; s/|||/3/g; s/||/2/g; s/|/1/g;
s/</|/g
t back
s/^$/0/
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: a is 0;
    var integer: b is 0;
  begin
    read(a);
    read(b);
    writeln(a + b);
  end func;
```



## SequenceL


```sequencel>import <Utilities/Conversion.sl
;

main(args(2)) := stringToInt(args[1]) + stringToInt(args[2]);
```


{{Out}}

```txt
cmd:> main.exe 3 4
7

cmd:> main.exe -5 7
2

cmd:> main.exe -12 -10
-22
```



## SETL


```setl
read(A, B);
print(A + B);
```



## Self

Works with positive and negative integers, and also more than two integers.


```self
((stdin readLine splitOn: ' ') mapBy: [|:e| e asInteger]) sum printLine.
```



## Shiny


```shiny
if (io.line 'stdin').match ~(\d+)\s+(\d+)~
    say "$a $b %(a+b)d"
end
```



## Sidef

Works with both positive and negative integers.

```ruby
say STDIN.readline.words.map{.to_i}.sum
```


More idiomatically:

```ruby
say read(String).words»to_i»()«+»
```


Explicit summation:

```ruby
var (a, b) = read(String).words.map{.to_i}...
say a+b
```


## Simula


```simula
BEGIN
    WHILE NOT LASTITEM DO
    BEGIN
        OUTINT(ININT + ININT, 0);
        OUTIMAGE;
    END;
END.

```



## SmileBASIC


```smilebasic
INPUT A
INPUT B
PRINT A+B

```



## SNOBOL4

Simple-minded solution (literally "two somethings separated by space")

```snobol
	input break(" ") . a " " rem . b
	output = a + b
end
```

"Integer aware" solution:

```snobol
	nums = "0123456789"
	input span(nums) . a break(nums) span(nums) . b
	output = a + b
end
```



## Smalltalk

Most Smalltalk implementations do not have the notion of a standard input stream, since it has always been a GUI based programming environment.  I've included test methods to demonstrate one way to create an input stream with two integers can be created. Opening a text file would be another.

```smalltalk
'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 8 August 2011 at 3:50:55 pm'!
Object subclass: #ABTask
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'rosettacode'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ABTask class
	instanceVariableNames: ''!

!ABTask class methodsFor: 'demo'!
parseInteger: inputStream
	^ Integer readFrom: inputStream skipSeparators! !

!ABTask class methodsFor: 'demo'!
sum: inputStream
	^ (self parseInteger: inputStream)
		+ (self parseInteger: inputStream)! !

!ABTask class methodsFor: 'demo'!
test2Plus2
	^ self
		sum: (ReadStream on: '2 2')! !

!ABTask class methodsFor: 'demo'!
test3Plus2
	^ self
		sum: (ReadStream on: '3 2')! !
```


but all have a stream hierarchy, so the task could be restated to pass input and output as stream arguments:
{{works with|Smalltalk/X}}
{{works with|VisualWorks Smalltalk}}

```smalltalk
|task|
task := [:inStream :outStream |
    |processLine|

    processLine :=
        [
            |a b|
            a := Integer readFrom: inStream.
            b := Integer readFrom: inStream.
            "is validation part of the task?"
            self assert:( a between:-1000 and: 1000).
            self assert:( b between:-1000 and: 1000).
            outStream print (a+b); cr.
        ].

    [ inStream atEnd ] whileFalse:processLine.
].

task value: ( 'dataIn.txt' asFilename readStream) value:Transcript.
```

or:

```smalltalk>task value: Stdin value: Stdout.</lang



## smart BASIC


```qbasic
INPUT n$
PRINT VAL(LEFT$(n$,(LEN(STR$(VAL(n$))))))+VAL(RIGHT$(n$,(LEN(n$)-LEN(STR$(VAL(n$)))-1)))
```


<b>NOTE:</b> This is a horribly forced way of doing this. smart BASIC has commands to SPLIT strings. Surely someone can provide better code than what I've written here.  ;@)

And someone did...

A FAR more elegant solution was provided by "Dutchman" on the smart [http://kibernetik.pro/forum/viewforum.php?f=2 BASIC Support Forum]:


```qbasic
INPUT n$
SPLIT n$ TO m$,n WITH " "
PRINT m$(0),m$(1),m$(0)+m$(1)
```


<b>NOTE:</b> smart BASIC will intelligently interpret the contents of a string as a numeric value if necessary. Other versions of BASIC would require the values stored in a string to be converted to numeric values before calculation.


## SPAD

{{works with|FriCAS}}
{{works with|OpenAxiom}}
{{works with|Axiom}}
One of several possibilities:

```SPAD
(1) -> integer READ()$Lisp + integer READ()$Lisp
333 444

   (1)  777
                                                        Type: PositiveInteger
```


Domain:[http://fricas.github.io/api/SExpression.html?highlight=lisp SExpression]


## SPARK


```Ada
-- By Jacob Sparre Andersen
-- Validates with SPARK GPL 2010's Examiner/Simplifier

with SPARK_IO; --# inherit SPARK_IO;

--# main_program;
procedure A_Plus_B
--# global in out SPARK_IO.Inputs, SPARK_IO.Outputs;
--# derives SPARK_IO.Inputs  from SPARK_IO.Inputs &
--#         SPARK_IO.Outputs from SPARK_IO.Inputs, SPARK_IO.Outputs;
is
   subtype Small_Integers is Integer range -1_000 .. +1_000;
   A, B       : Integer;
   A_OK, B_OK : Boolean;
begin
   SPARK_IO.Get_Integer
     (File  => SPARK_IO.Standard_Input,
      Item  => A,
      Width => 0,
      Read  => A_OK);

   A_OK := A_OK and A in Small_Integers;

   SPARK_IO.Get_Integer
     (File  => SPARK_IO.Standard_Input,
      Item  => B,
      Width => 0,
      Read  => B_OK);

   B_OK := B_OK and B in Small_Integers;

   if A_OK and B_OK then
      SPARK_IO.Put_Integer
        (File  => SPARK_IO.Standard_Output,
         Item  => A + B,
         Width => 4,
         Base  => 10);
   else
      SPARK_IO.Put_Line
        (File => SPARK_IO.Standard_Output,
         Item => "Input data does not match specification.",
         Stop => 0);
   end if;
end A_Plus_B;
```



## SPL


```spl
n = #.split(#.input("Input two numbers, separated by space:")," ")
#.output(n[1],"+",n[2],"=",#.val(n[1])+#.val(n[2]))
```

{{in}}

```txt

Input two numbers, separated by space:
2 3

```

{{out}}

```txt

2+3=5

```



## SQL


```sql>select A+B</lang

Example:

```sql>select 2+3</lang

This should produce a result set containing the value 5.

Note however that declaration of variables is outside the scope of the ANSI SQL standards, unless by variables you mean tables (which would complicate the example considerably).


## SQL PL

{{incorrect|SQL PL|This task receive a string with two characters and at least one space in the middle}}
{{works with|Db2 LUW}}
With SQL only:

```sql pl

values 2 + 2;
values 3 + 2;
select 2 + 2 from sysibm.sysdummy1;
select 3 + 2 from sysibm.sysdummy1;

```

Output:

```txt

db2 -t
db2 => values 2 + 2;
1
-----------
          4

  1 record(s) selected.

db2 => values 3 + 2;
1
-----------
          5

  1 record(s) selected.

db2 => select 2 + 2 from sysibm.sysdummy1;
1
-----------
          4

  1 record(s) selected.

db2 => select 3 + 2 from sysibm.sysdummy1;
1
-----------
          5

  1 record(s) selected.

```



## SSEM

The SSEM has no Add instruction, so we rely on the fact that <i>a</i> + <i>b</i> = -(-<i>a</i> - <i>b</i>).

```ssem
10100000000000100000000000000000   0. -5 to c     acc = -A
01100000000001010000000000000000   1. Sub. 6      acc -= B
11100000000001100000000000000000   2. c  to 7     X = acc
11100000000000100000000000000000   3. -7 to c     acc = -X
00000000000001110000000000000000   4. Stop
10100100000000000000000000000000   5. 37          A
00111000000000000000000000000000   6. 28          B
00000000000000000000000000000000   7. 0           X
```



## Standard ML


```sml
(*
 * val split : string -> string list
 * splits a string at it spaces
 *)
val split = String.fields (fn #" " => true | _ => false)

(*
 * val removeNl : string -> string
 * removes the occurence of "\n" in a string
 *)
val removeNl = String.translate (fn #"\n" => "" | c => implode [c])

(*
 * val aplusb : unit -> int
 * reads a line and gets the sum of the numbers
 *)
fun aplusb () =
	let
	  val input  = removeNl (valOf (TextIO.inputLine TextIO.stdIn))
	in
	  foldl op+ 0 (map (fn s => valOf (Int.fromString s)) (split input))
	end
```

{{out}}

```txt

- aplusb();
123 456
val it = 579 : int

```



## Swift

{{works with|Swift|2}}
Requires sending EOF.

```Swift
import Foundation

let input = NSFileHandle.fileHandleWithStandardInput()

let data = input.availableData
let str = NSString(data: data, encoding: NSUTF8StringEncoding)!

let nums = str.componentsSeparatedByString(" ")
let a = (nums[0] as String).toInt()!
let b = (nums[1] as String).toInt()!

print(" \(a + b)")
```


{{works with|Swift|3}}

Swift 4 and no requirement to send EOF (press enter/send newline like you normally would)


```Swift

import Foundation

let input = FileHandle.standardInput

let data = input.availableData
let str = String(data: data, encoding: .utf8)!
let nums = str.split(separator: " ")
    .map { String($0.unicodeScalars
        .filter { CharacterSet.decimalDigits.contains($0) }) }

let a = Int(nums[0])!
let b = Int(nums[1])!

print(" \(a + b)")

```



## Tailspin


```tailspin

composer nums
  [ (<WS>?) <INT> (<WS>) <INT> (<WS>?) ]
end nums

$IN::lines -> nums -> $(1) + $(2) -> '$;
' -> !OUT::write

```



## Tcl


```tcl
scan [gets stdin] "%d %d" x y
puts [expr {$x + $y}]
```

Alternatively:

```tcl
puts [tcl::mathop::+ {*}[gets stdin]]
```

To/from a file:

```tcl
set in [open "input.txt"]
set out [open "output.txt" w]
scan [gets $in] "%d %d" x y
puts $out [expr {$x + $y}]
close $in
close $out
```


=={{header|TI-83 BASIC}}==

```ti83b
:Prompt A,B
:Disp A+B
```


=={{header|TI-83 Hex Assembly}}==

Note: Comments (after the semicolons) are just for explanation -- TI-83 hex assembly does not allow comments in program source code.


```ti83b
PROGRAM:APLUSB
:AsmPrgm
:
:EFC541 ; ZeroOP1
:217984 ; ld hl,op1+1
:3641   ; ld (hl),'A'
:EFE34A ; RclVarSym
:CF     ; rst OP1toOP2
:
:EFC541 ; ZeroOP1
:217984 ; ld hl,op1+1
:3642   ; ld (hl),'B'
:EFE34A ; RclVarSym
:
:F7     ; rst FPAdd
:EFBF4A ; StoAns
:C9     ; ret
```


Store the inputs in the 'A' and 'B' OS variables. Run it with Asm(prgmAPLUSB) and the output will be stored in the Ans OS variable.

=={{header|TI-89 BASIC}}==

```ti89b
:aplusb(a,b)
:a+b
```



## TorqueScript

Since torque is not compatible with standard input, I will show the closest to that.
It's a function that takes a single string input, that will contain the 2 numbers.

```Torque
Function aPlusB(%input)
{
    return getWord(%input, 0) + getWord(%input, 1);
}
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
SET input="1 2"
SET input=SPLIT(input,": :")
SET input=JOIN (input)
SET output=SUM(input)
```



## TXR



```txt
$ txr -p '(+ (read) (read))'
1.2 2.3
3.5
```



## TypeScript



```txt
function add(a: number, b: number) {
return a+b;
}

```



## UNIX Shell

{{works with|Bourne Shell}}

```sh
#!/bin/sh
read a b || exit
echo `expr "$a" + "$b"`
```


{{works with|bash}}
{{works with|ksh93}}
{{works with|pdksh}}
{{works with|zsh}}
Script "a+b.sh":

```bash
#!/bin/bash
read a b || exit
echo $(( a + b ))
```

{{Out}}

```bash
echo 2 3 | ksh a+b.sh
5
```


=
## C Shell
=

```csh
set line=$<
set input=($line)
@ sum = $input[1] + $input[2]
echo $sum
```



## Ursa

<lang>#
# a + b
#

# read a string containing the two ints
decl string input
set input (in string console)

# determine the sum
decl int sum
set sum (int (+ sum (int (split input " ")<0>)))
set sum (int (+ sum (int (split input " ")<1>)))

# output the sum
out sum endl console
```



## Ursala

Using standard input and output streams:

```Ursala
#import std
#import int

#executable&

main = %zP+ sum:-0+ %zp*FiNCS+ sep` @L
```

Overwriting a text file named as a command line parameter:

```Ursala
#import std
#import int

#executable -[parameterized]-

main = ~command.files.&h; <.contents:= %zP+ sum:-0+ %zp*FiNCS+ sep` @L+ ~contents>
```

Creating a new file named after the input file with suffix <code>.out</code>:

```Ursala
#import std
#import int

#executable -[parameterized]-

main =

~command.files.&h; ~&iNC+ file$[
   contents: %zP+ sum:-0+ %zp*FiNCS+ sep` @L+ ~contents,
   path: ~path; ^|C\~& ~=`.-~; ^|T/~& '.out'!]
```



## Vala

Read from stdin while program running:

```vala
Using GLib;

int main (string[] args) {
    stdout.printf ("Please enter int value for A\n");
    var a = int.parse (stdin.read_line ());
    stdout.printf ("Please enter int value for B\n");
    var b = int.parse (stdin.read_line ());
    stdout.printf ("A + B = %d\n", a + b);
    return 0;
}

```



## VBA



```VBA
Sub Rosetta_AB()
Dim stEval As String
stEval = InputBox("Enter two numbers, separated only by a space", "Rosetta Code", "2 2")
MsgBox "You entered " & stEval & vbCr & vbCr & _
    "VBA converted this input to " & Replace(stEval, " ", "+") & vbCr & vbCr & _
    "And evaluated the result as " & Evaluate(Replace(stEval, " ", "+")), vbInformation + vbOKOnly, "XLSM"
End Sub
```



## VBScript


```vb
Option Explicit
Dim a, b
Select Case WScript.Arguments.Count
	Case 0	'No arguments, prompt for them.
		WScript.Echo "Enter values for a and b"
		a = WScript.Stdin.ReadLine
		if Instr(a, " ") > 0 then	'If two variables were passed
			b = Split(a)(1)
			a = Split(a)(0)
		else
			WScript.Echo "Enter value for b"
			b = WScript.Stdin.ReadLine
		end if
	Case 1	'One argument, assume it's an input file, e.g. "in.txt"
		Dim FSO : Set FSO = CreateObject("Scripting.FileSystemObject")
		With FSO.OpenTextFile(WScript.Arguments(0), 1)
			a = .ReadLine
			b = Split(a)(1)
			a = Split(a)(0)
			.Close
		End With
	Case 2 'Two arguments, assume they are values
		a = WScript.Arguments(0)
		b = WScript.Arguments(1)
End Select
'At this point, a and b are strings as entered, make them numbers
a = CInt(a)
b = CInt(b)

'Write the sum
Wscript.Echo a + b
if 1 = WScript.Arguments.Count then
	With FSO.CreateTextFile("out.txt")
		.WriteLine a + b
		.Close
	End With
end if
```



## Verilog


```Verilog
module TEST;

  reg signed [11:0] y;

  initial begin
    y= sum(2, 2);
    y= sum(3, 2);
    y= sum(-3, 2);
  end

  function signed [11:0] sum;
    input signed [10:0] a, b;
    begin
      sum= a + b;
      $display("%d + %d = %d",a,b,sum);
    end
  endfunction

endmodule
```



## VHDL


```VHDL
LIBRARY std;
USE std.TEXTIO.all;


entity test is
end entity test;


architecture beh of test is
begin
  process
    variable line_in, line_out : line;
    variable a,b : integer;
  begin
    readline(INPUT, line_in);
    read(line_in, a);
    read(line_in, b);

    write(line_out, a+b);
    writeline(OUTPUT, line_out);
    wait; -- needed to stop the execution
  end process;
end architecture beh;
```



## Visual Basic .NET


```vbnet
Module Module1

  Sub Main()
    Dim s() As String = Nothing

    s = Console.ReadLine().Split(" "c)
    Console.WriteLine(CInt(s(0)) + CInt(s(1)))
  End Sub

End Module
```



## Wee Basic


```Wee Basic
Print 1 "Enter number A:"
input a
Print 1 "Enter number B:"
input b
let c=a+b
print 1 c
end
```



## Whitespace


```whitespace













```



## Wren


```wren
import "io" for Stdin
var a = Num.fromString(Stdin.readLine())
var b = Num.fromString(Stdin.readLine())
System.print(a + b)

```



## X86 Assembly

{{works with|NASM|Linux}}

```asm
section .text
	global _start

	_print:
		mov ebx, 1
		mov eax, 4
		int 0x80
		ret

	_get_input:
		mov edx, 4
		mov ebx, 0
		mov eax, 3
		int 0x80
		ret

	_start:
		mov edx, in_val_len
		mov ecx, in_val_msg
		call _print
		mov ecx, a
		call _get_input
		;make 'a' an actual number rather than a char.
		sub dword [a], 0x30
		mov edx, in_val_len
		mov ecx, in_val_msg
		call _print
		mov ecx, b
		call _get_input
		;calc real number for 'b'
		sub dword [b], 0x30
		mov eax, dword [a]
		mov ebx, dword [b]
		add eax, ebx
		;get the character for our sum.
		add eax, 0x30
		mov dword [sum], eax
		mov edx, out_val_len
		mov ecx, out_val_msg
		call _print
		mov [sum+1], dword 0xa
		mov edx, 4
		mov ecx, sum
		call _print
		push 0x1
		mov eax, 1
		push eax
		int 0x80
		ret

section .data
in_val_msg	db "Please input an integer:",0
in_val_len	equ $-in_val_msg
out_val_msg db "The sum of a+b is: ",0
out_val_len	equ $-out_val_msg

section .bss
a    			resd 1
b				resd 1
sum			resd 1
```

This will not work on numbers over 0(from 1 to 0). This is due to the fact, numbers higher than 0(10,11, etc) are in fact strings when taken from the keyboard. A much longer conversion code is required to loop through and treat each number in the string as separate numbers. For example, The number '10' would have to be treated as a 1 and a 0.


## xEec


```xEec
i# i# ma h#10 r o# p o$ p
```



## XLISP


```xlisp
(DEFUN A-PLUS-B ()
    (DISPLAY "Enter two numbers separated by a space.")
    (NEWLINE)
    (DISPLAY "> ")
    (DEFINE A (READ))
    (DEFINE B (READ))
    (+ A B))
```

{{out}}

```txt
(A-PLUS-B)
Enter two numbers separated by a space.
> 2 2

4
```



## XPL0


```XPL0
include c:\cxpl\codes;
int A, B;
[A:= IntIn(0);
 B:= IntIn(0);
 IntOut(0, A+B);
 CrLf(0);
]
```



## XQuery


```xquery

(:
  Using the EXPath File Module, which is built into most XQuery processors
  by default and thus does not need to get imported. Some processors bind the
  namespace automatically, others require explicit declaration.
:)

xquery version "3.1";

declare namespace file = 'http://expath.org/ns/file';

let $in       := 'input.txt'
let $out      := 'output.txt'
let $numbers  := tokenize(file:read-text($in))
let $result   := xs:numeric($numbers[1]) + xs:numeric($numbers[2])
return file:write-text($out, xs:string($result))

```



## Yabasic


```Yabasic
repeat
    input "Enter two numbers (betwen -1000 ... +1000): " a, b
until(valid(a) and valid(b))
print "\nThe sum of ", a, " and ", b, " is: ", a + b

sub valid(x)
    return x >= -1000 and x <= 1000
end sub
```



## Yorick


```yorick
a = b = 0;
read, a, b;
write, a + b;
```



## ZED

Source -> http://ideone.com/WLtEfe
Compiled -> http://ideone.com/fMt6ST

```zed
(A+B)
comment:
#true
(+) (read) (read)

(+) one two
comment:
#true
(003) "+" one two

(read)
comment:
#true
(001) "read"
```



## zkl


```zkl
do(2){ask("A B: ").split(" ").filter().sum().println()}
```


```txt

A B: 123    567
690
A B: -4 6
2

```

This actually works for any number of integers


## ZX Spectrum Basic


```zxbasic
10 PRINT "Input two numbers separated by"'"space(s) "
20 INPUT LINE a$
30 GO SUB 90
40 FOR i=1 TO LEN a$
50 IF a$(i)=" " THEN LET a=VAL a$( TO i): LET b=VAL a$(i TO ): PRINT a;" + ";b;" = ";a+b: GO TO 70
60 NEXT i
70 STOP
80 REM LTrim operation
90 IF a$(1)=" " THEN LET a$=a$(2 TO ): GO TO 90
100 RETURN
```


Another solution


```zxbasic
10 PRINT "Input two numbers separated by"'"space(s) "
20 INPUT LINE a$
30 LET ll=10e10: LET ls=0: LET i=1
40 IF a$(i)=" " THEN LET ls=i: GO TO 60
50 LET ll=i
60 IF ls>ll THEN GO TO 80
70 LET i=i+1: GO TO 40
80 LET a=VAL a$( TO i): LET b=VAL a$(i TO )
90 PRINT a;" + ";b;" = ";a+b
```



```txt

Input two numbers separated by
space(s)
  3.14     2^3
3.14 + 8 = 11.14

```



## zonnon


```zonnon

module ABProblem;
var
	a,b: integer;
begin
	read(a,b);
	writeln(a+b)
end ABProblem.

```


```txt

1 2
                   3

```

