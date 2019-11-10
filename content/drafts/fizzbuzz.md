+++
title = "FizzBuzz"
description = ""
date = 2019-10-08T17:07:43Z
aliases = []
[extra]
id = 2150
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
[[Category:Iteration]]
[[Category:Recursion]]
[[Category:Simple]]

;Task:
Write a program that prints the integers from   '''1'''   to   '''100'''   (inclusive).


But:
:*   for multiples of three,   print   '''Fizz'''     (instead of the number)
:*   for multiples of five,   print   '''Buzz'''     (instead of the number)
:*   for multiples of both three and five,   print   '''FizzBuzz'''     (instead of the number)


The   ''FizzBuzz''   problem was presented as the lowest level of comprehension required to illustrate adequacy.


;Also see:
*   (a blog)   [http://weblog.raganwald.com/2007/01/dont-overthink-fizzbuzz.html dont-overthink-fizzbuzz]
*   (a blog)   [http://blog.codinghorror.com/fizzbuzz-the-programmers-stairway-to-heaven/ fizzbuzz-the-programmers-stairway-to-heaven]





## 360 Assembly

See [[FizzBuzz/Assembly]]


## 6502 Assembly

See [[FizzBuzz/Assembly]]


## 68000 Assembly

See [[FizzBuzz/Assembly]]


## 8086 Assembly

See [[FizzBuzz/Assembly]]


## 8th


```forth

with: n

: num?  \ n f --   )
	if drop else . then ;

\ is m mod n 0? leave the result twice on the stack
: div? \ m n -- f f
	mod 0 = dup ;

: fizz? \ n -- n f
	dup 3
	div? if "Fizz" .  then ;

: buzz? \ n f -- n f
	over 5
	div? if "Buzz" .  then or ;

\ print a message as appropriate for the given number:
: fizzbuzz  \ n --
	fizz? buzz? num?
	space ;

\ iterate from 1 to 100:
' fizzbuzz 1 100 loop
cr bye

```



## ABAP


### Impure Functional 1

{{works with|ABAP|7.4 SP05 or Above only}}

```ABAP
DATA: tab TYPE TABLE OF string.

tab = VALUE #(
  FOR i = 1 WHILE i <= 100 (
    COND string( LET r3 = i MOD 3
                     r5 = i MOD 5 IN
                 WHEN r3 = 0 AND r5 = 0 THEN |FIZZBUZZ|
                 WHEN r3 = 0            THEN |FIZZ|
                 WHEN r5 = 0            THEN |BUZZ|
                 ELSE i ) ) ).

cl_demo_output=>write( tab ).
cl_demo_output=>display( ).
```



### Impure Functional 2

{{works with|ABAP|7.4 SP05 or Above only}}

```ABAP
cl_demo_output=>display( VALUE stringtab( FOR i = 1 WHILE i <= 100 ( COND #(  LET m3 = i MOD 3 m5 = i MOD 5 IN
                                                                             WHEN m3 = 0 AND m5 = 0 THEN |FIZZBUZZ|
                                                                             WHEN m3 = 0            THEN |FIZZ|
                                                                             WHEN m5 = 0            THEN |BUZZ|
                                                                             ELSE i ) ) ) ).
```



## ACL2


```Lisp
(defun fizzbuzz-r (i)
   (declare (xargs :measure (nfix (- 100 i))))
   (prog2$
    (cond ((= (mod i 15) 0) (cw "FizzBuzz~%"))
          ((= (mod i 5) 0) (cw "Buzz~%"))
          ((= (mod i 3) 0) (cw "Fizz~%"))
          (t (cw "~x0~%" i)))
    (if (zp (- 100 i))
        nil
        (fizzbuzz-r (1+ i)))))

(defun fizzbuzz () (fizzbuzz-r 1))
```



## ActionScript

The [[ActionScript]] solution works just like the [[FizzBuzz#JavaScript|JavaScript]] solution (they share the [[ECMAScript]] specification).  The difference is that ActionScript has the ''trace'' command to write out to a console.

```actionscript
for (var i:int = 1; i <= 100; i++) {
  if (i % 15 == 0)
    trace('FizzBuzz');
  else if (i % 5 == 0)
    trace('Buzz');
  else if (i % 3 == 0)
    trace('Fizz');
  else
    trace(i);
}
```



## Ada


```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Fizzbuzz is
begin
   for I in 1..100 loop
      if I mod 15 = 0 then
         Put_Line("FizzBuzz");
      elsif I mod 5 = 0 then
         Put_Line("Buzz");
      elsif I mod 3 = 0 then
         Put_Line("Fizz");
      else
         Put_Line(Integer'Image(I));
      end if;
   end loop;
end Fizzbuzz;
```



## ALGOL 68


```algol68
main:(
  FOR i TO 100 DO
    printf(($gl$,
      IF i %* 15 = 0 THEN
        "FizzBuzz"
      ELIF i %* 3 = 0 THEN
        "Fizz"
      ELIF i %* 5 = 0 THEN
        "Buzz"
      ELSE
        i
      FI
    ))
  OD
)
```

or simply:

```algol68
FOR i TO 100 DO print(((i%*15=0|"FizzBuzz"|:i%*3=0|"Fizz"|:i%*5=0|"Buzz"|i),new line)) OD
```



## ALGOL W


```algolw

begin
    i_w := 1; % set integers to print in minimum space %
    for i := 1 until 100 do begin
        if      i rem 15 = 0 then write( "FizzBuzz" )
        else if i rem  5 = 0 then write( "Buzz" )
        else if i rem  3 = 0 then write( "Fizz" )
        else                      write( i )
    end for_i
end.
```



## AntLang


```AntLang
n:{1+ x}map range[100]
s:{a:0eq x mod 3;b:0eq x mod 5;concat apply{1elem x}map{0elem x}hfilter seq[1- max[a;b];a;b]merge seq[str[x];"Fizz";"Buzz"]}map n
echo map s
```



## APEX


```Apex

for(integer i=1; i <= 100; i++){
    String output = '';
    if(math.mod(i, 3) == 0) output += 'Fizz';
    if(math.mod(i, 5) == 0) output += 'Buzz';
    if(output != ''){
        System.debug(output);
    } else {
        System.debug(i);
    }
}

```



## APL


```apl
⎕IO←0
(L,'Fizz' 'Buzz' 'FizzBuzz')[¯1+(L×W=0)+W←(100×~0=W)+W←⊃+/1 2×0=3 5|⊂L←1+⍳100]

```



Slightly different approach that makes use of the Decode function (⊥):

```apl

A[I]←1+I←(0⍷A)/⍳⍴A←('FIZZBUZZ' 'FIZZ’ 'BUZZ' 0)[2⊥¨×(⊂3 5)|¨1+⍳100]

```


The idea is to first calculate the residues for all numbers 1..100 after
division with both 3 and 5. This generates 100 pairs of numbers a b, where
a is either 0,1,2 and b is either 0,1,2,3,4.

These pairs are then put through the sign function which returns 0 for a 0,
and a 1 for anything greater than 0. Now we have binary pairs.
The binary pairs are encoded with a left argument of 2 resulting in 0,1,2,3.
These are treated as indices for the "FizzBuzz vector" where 0 is in position 3.

Variable A holds this new vector of words and zeros.
Variable I is assigned the zeros' positions.
Finally A[I] is replaced with corresponding indices.

If you have an aversion against mixed vectors, consider inserting ⍕¨ before the
final (i.e. left-most) assignment.

{{works with|Dyalog_APL}}

Here's a Dyalog-specific solution taking advantage of its anonymous function extension:


```apl
{ ⍵ 'Fizz' 'Buzz' 'FizzBuzz'[ +/1 2×0=3 5|⍵] }¨1+⍳100
```



## AppleScript


```AppleScript
property outputText: ""
repeat with i from 1 to 100
  if i mod 15 = 0 then
    set outputText to outputText & "FizzBuzz"
  else if i mod 3 = 0 then
    set outputText to outputText & "Fizz"
  else if i mod 5 = 0 then
    set outputText to outputText & "Buzz"
  else
    set outputText to outputText & i
  end if
  set outputText to outputText & linefeed
end repeat
outputText
```



Or, using map(), enumFromTo(), and a more functional pattern of composition:


```AppleScript
-- FIZZBUZZ ------------------------------------------------------------------

-- fizz :: Int -> Bool
on fizz(n)
    n mod 3 = 0
end fizz

-- buzz :: Int -> Bool
on buzz(n)
    n mod 5 = 0
end buzz

-- fizzAndBuzz :: Int -> Bool
on fizzAndBuzz(n)
    n mod 15 = 0
end fizzAndBuzz

-- fizzBuzz :: Int -> String
on fizzBuzz(x)
    caseOf(x, [[my fizzAndBuzz, "FizzBuzz"], ¬
        [my fizz, "Fizz"], ¬
        [my buzz, "Buzz"]], x as string)
end fizzBuzz


-- TEST ----------------------------------------------------------------------
on run

    intercalate(linefeed, ¬
        map(fizzBuzz, enumFromTo(1, 100)))

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- caseOf :: a -> [(predicate, b)] -> Maybe b -> Maybe b
on caseOf(e, lstPV, default)
    repeat with lstCase in lstPV
        set {p, v} to contents of lstCase
        if mReturn(p)'s |λ|(e) then return v
    end repeat
    return default
end caseOf

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
```



## Applesoft BASIC

See [[FizzBuzz/Basic]]


## Arbre


```Arbre
fizzbuzz():
  for x in [1..100]
    if x%5==0 and x%3==0
      return "FizzBuzz"
    else
      if x%3==0
        return "Fizz"
      else
        if x%5==0
          return "Buzz"
        else
           return x

main():
  fizzbuzz() -> io
```



## Arc


### Arc 3.1 Base


```lisp
(for n 1 100
  (prn:if
    (multiple n 15) 'FizzBuzz
    (multiple n 5) 'Buzz
    (multiple n 3) 'Fizz
    n))
```


```lisp
(for n 1 100
     (prn:check (string (when (multiple n 3) 'Fizz)
                        (when (multiple n 5) 'Buzz))
                ~empty n)) ; check created string not empty, else return n
```



### Waterhouse Arc


```lisp
(for n 1 100
  (prn:case (gcd n 15)
    1 n
    3 'Fizz
    5 'Buzz
      'FizzBuzz))
```



## ARM Assembly

<lang ARM_Assembly>
/ * linux GAS */

.global _start

.data

Fizz: .ascii "Fizz\n"
Buzz: .ascii "Buzz\n"
FizzAndBuzz: .ascii "FizzBuzz\n"

numstr_buffer: .skip 3
newLine: .ascii "\n"

.text

_start:

  bl FizzBuzz

  mov r7, #1
  mov r0, #0
  svc #0

FizzBuzz:

  push {lr}
  mov r9, #100

  fizzbuzz_loop:

    mov r0, r9
    mov r1, #15
    bl divide
    cmp r1, #0
    ldreq r1, =FizzAndBuzz
    moveq r2, #9
    beq fizzbuzz_print

    mov r0, r9
    mov r1, #3
    bl divide
    cmp r1, #0
    ldreq r1, =Fizz
    moveq r2, #5
    beq fizzbuzz_print

    mov r0, r9
    mov r1, #5
    bl divide
    cmp r1, #0
    ldreq r1, =Buzz
    moveq r2, #5
    beq fizzbuzz_print

    mov r0, r9
    bl make_num
    mov r2, r1
    mov r1, r0

    fizzbuzz_print:

      mov r0, #1
      mov r7, #4
      svc #0

      sub r9, #1
      cmp r9, #0

    bgt fizzbuzz_loop

  pop {lr}
  mov pc, lr

make_num:

  push {lr}
  ldr r4, =numstr_buffer
  mov r5, #4
  mov r6, #1

  mov r1, #100
  bl divide

  cmp r0, #0
  subeq r5, #1
  movne r6, #0

  add r0, #48
  strb r0, [r4, #0]

  mov r0, r1
  mov r1, #10
  bl divide

  cmp r0, #0
  movne r6, #0
  cmp r6, #1
  subeq r5, #1

  add r0, #48
  strb r0, [r4, #1]

  add r1, #48
  strb r1, [r4, #2]

  mov r2, #4
  sub r0, r2, r5
  add r0, r4, r0
  mov r1, r5

  pop {lr}
  mov pc, lr

divide:
  udiv r2, r0, r1
  mul r3, r1, r2
  sub r1, r0, r3
  mov r0, r2
  mov pc, lr

```



## Arturo


```arturo
loop $(range 1 100) {
	if &%15=0 { "FizzBuzz" } {
		if &%3=0 { "Fizz" } {
			if &%5=0 { "Buzz" } {
				print &
			}
		}
	}
}
```

{{out}}

```txt
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
FizzBuzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
FizzBuzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
FizzBuzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
FizzBuzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz
```



## AsciiDots

See [[FizzBuzz/EsoLang#AsciiDots]]


## ATS


```ats
#include "share/atspre_staload.hats"

implement main0() = loop(1, 100) where {
  fun loop(from: int, to: int): void =
    if from > to then () else
    let
      val by3 = (from % 3 = 0)
      val by5 = (from % 5 = 0)
    in
      case+ (by3, by5) of
      | (true, true) => print_string("FizzBuzz")
      | (true, false) => print_string("Fizz")
      | (false, true) => print_string("Buzz")
      | (false, false) => print_int(from);
      print_newline();
      loop(from+1, to)
    end
}
```



## AutoHotkey

{{AutoHotkey case}}

```AutoHotkey
Loop, 100
{
  If (Mod(A_Index, 15) = 0)
    output .= "FizzBuzz`n"
  Else If (Mod(A_Index, 3) = 0)
    output .= "Fizz`n"
  Else If (Mod(A_Index, 5) = 0)
    output .= "Buzz`n"
  Else
    output .= A_Index "`n"
}
FileDelete, output.txt
FileAppend, %output%, output.txt
Run, cmd /k type output.txt
```

A short example with cascading ternary operators and graphical output. Press Esc to close the window.

```AutoHotkey
Gui, Add, Edit, r20
Gui,Show
Loop, 100
  Send, % (!Mod(A_Index, 15) ? "FizzBuzz" : !Mod(A_Index, 3) ? "Fizz" : !Mod(A_Index, 5) ? "Buzz" : A_Index) "`n"
Return
Esc::
ExitApp
```



## AutoIt


### Example1

Output via MsgBox():

```AutoIt
For $i = 1 To 100
	If Mod($i, 15) = 0 Then
		MsgBox(0, "FizzBuzz", "FizzBuzz")
	ElseIf Mod($i, 5) = 0 Then
		MsgBox(0, "FizzBuzz", "Buzz")
	ElseIf Mod($i, 3) = 0 Then
		MsgBox(0, "FizzBuzz", "Fizz")
	Else
		MsgBox(0, "FizzBuzz", $i)
	EndIf
Next
```



### Example2

Output via console, logfile and/or messagebox:

```AutoIt>#include <Constants.au3


; uncomment how you want to do the output
Func Out($Msg)
	ConsoleWrite($Msg & @CRLF)

;~	FileWriteLine("FizzBuzz.Log", $Msg)

;~ 	$Btn = MsgBox($MB_OKCANCEL + $MB_ICONINFORMATION, "FizzBuzz", $Msg)
;~ 	If $Btn > 1 Then Exit	; Pressing 'Cancel'-button aborts the program
EndFunc   ;==>Out

Out("# FizzBuzz:")
For $i = 1 To 100
	If Mod($i, 15) = 0 Then
		Out("FizzBuzz")
	ElseIf Mod($i, 5) = 0 Then
		Out("Buzz")
	ElseIf Mod($i, 3) = 0 Then
		Out("Fizz")
	Else
		Out($i)
	EndIf
Next
Out("# Done.")
```



## AWK

See [[FizzBuzz/AWK]]


## Axe


```axe
For(I,1,100)
!If I^3??I^5
 Disp "FIZZBUZZ",i
Else!If I^3
 Disp "FIZZ",i
Else!If I^5
 Disp "BUZZ",i
Else
 Disp I▶Dec,i
End
.Pause to allow the user to actually read the output
Pause 1000
End
```



## Babel


```babel
main:
     { { iter 1 + dup

        15 %
            { "FizzBuzz" <<
                zap }
            { dup
            3 %
                { "Fizz" <<
                    zap }
                { dup
                5 %
                    { "Buzz" <<
                        zap}
                    { %d << }
                if }
            if }
        if

        "\n" << }

    100 times }
```



## BaCon

See [[FizzBuzz/Basic#BaCon]]


## bash

Any bash hacker would do this as a one liner at the shell, so...

```bash
for n in {1..100}; do ((( n % 15 == 0 )) && echo 'FizzBuzz') || ((( n % 5 == 0 )) && echo 'Buzz') || ((( n % 3 == 0 )) && echo 'Fizz') || echo $n; done
```

For the sake of readability...

```bash
for n in {1..100}; do
  ((( n % 15 == 0 )) && echo 'FizzBuzz') ||
  ((( n % 5 == 0 )) && echo 'Buzz') ||
  ((( n % 3 == 0 )) && echo 'Fizz') ||
  echo $n;
done
```

Here's a very concise approach, with only 75 characters total.
Unfortunately it relies on aspects of Bash which are rarely used.

```bash
for i in {1..100};do((i%3))&&x=||x=Fizz;((i%5))||x+=Buzz;echo ${x:-$i};done
```

Here's the concise approach again, this time separated into multiple lines.

```bash
# FizzBuzz in Bash.  A concise version, but with verbose comments.
for i in {1..100} # Use i to loop from "1" to "100", inclusive.
do  ((i % 3)) &&  # If i is not divisible by 3...
        x= ||     # ...blank out x (yes, "x= " does that).  Otherwise,...
        x=Fizz    # ...set (not append) x to the string "Fizz".
    ((i % 5)) ||  # If i is not divisible by 5, skip (there's no "&&")...
        x+=Buzz   # ...Otherwise, append (not set) the string "Buzz" to x.
   echo ${x:-$i}  # Print x unless it is blanked out.  Otherwise, print i.
done
```

It's a bit silly to optimize such a small & fast program,
but for the sake of algorithm analysis it's worth noting that
the concise approach is reasonably efficient in several ways.
Each divisibility test appears in the code exactly once,
only two variables are created,
and the approach avoids setting variables unnecessarily.
As far as I can tell,
the divisibility tests only fire the minimum number of times required
for the general case (e.g. where the 100/3/5 constants can be changed),
unless you introduce more variables and test types.
Corrections invited.
I avoided analyzing the non-general case where 100/3/5 never change,
because one "optimal" solution is to simply print the pre-computed answer,


## BASIC

See [[FizzBuzz/Basic]]


## Batch File

FOR /L version:


```dos
@echo off
for /L %%i in (1,1,100) do call :tester %%i
goto :eof

:tester
  set /a test = %1 %% 15
  if %test% NEQ 0 goto :NotFizzBuzz
  echo FizzBuzz
  goto :eof

:NotFizzBuzz
  set /a test = %1 %% 5
  if %test% NEQ 0 goto :NotBuzz
  echo Buzz
  goto :eof

:NotBuzz
  set /a test = %1 %% 3
  if %test% NEQ 0 goto :NotFizz
  echo Fizz
  goto :eof

:NotFizz
  echo %1

```


Loop version:


```dos
@echo off
set n=1

:loop
  call :tester %n%
  set /a n += 1
  if %n% LSS 101 goto loop
  goto :eof

:tester
  set /a test = %1 %% 15
  if %test% NEQ 0 goto :NotFizzBuzz
  echo FizzBuzz
  goto :eof

:NotFizzBuzz
  set /a test = %1 %% 5
  if %test% NEQ 0 goto :NotBuzz
  echo Buzz
  goto :eof

:NotBuzz
  set /a test = %1 %% 3
  if %test% NEQ 0 goto :NotFizz
  echo Fizz
  goto :eof

:NotFizz
  echo %1
```


FOR /L with a block instead of very-high-overhead subroutine call:


```dos
@echo off & setlocal enabledelayedexpansion
for /l %%i in (1,1,100) do (
  set /a m5=%%i %% 5
  set /a m3=%%i %% 3
  set s=
  if !m5! equ 0 set s=!s!Fizz
  if !m3! equ 0 set s=!s!Buzz
  if "!s!"=="" set s=%%i
  echo !s!
)
```



## BBC BASIC

See [[FizzBuzz/Basic]]


## bc

This solution never uses <tt>else</tt>, because bc has no <tt>else</tt> keyword (but some implementations add <tt>else</tt> as an extension).


```bc
for (i = 1; i <= 100; i++) {
	w = 0
	if (i % 3 == 0) { "Fizz"; w = 1; }
	if (i % 5 == 0) { "Buzz"; w = 1; }
	if (w == 0) i
	if (w == 1) "
"
}
quit
```



## beeswax


Also see on [[FizzBuzz/EsoLang]]

“Ordinary” FizzBuzz solution:


```beeswax>
     q
        >@F5~%"d@F{  >  @F     q
_1>F3~%'d`Fizz`@F5~%'d >`Buzz`@FNp
  ;bL@~.~4~.5~5@                P<
```



Example without double mod 5 check, using a flag instead, to check if Fizz already got printed (in this case the number n must not be printed if mod 5 is > 0):


```beeswax>
@?q
         >      q       >Ag'd@{?p
_>"1F3~%'d`Fizz`f>@F5~%'d`Buzz`@p
  b            P~;"-~@~.+0~P9@N?<
```



## Befunge

See [[FizzBuzz/EsoLang]]


## blz


```blz
for i = 0; i <= 100; i++
    out = ""
    if i % 3 == 0
        out = "Fizz"
    end
    if i % 5 == 0
        out = out + "Buzz"
    end
    if out == ""
        out = i
    end
    print(out)
end
```



## Boo


```boo
def fizzbuzz(size):
    for i in range(1, size):
        if i%15 == 0:
            print 'FizzBuzz'
        elif i%5 == 0:
            print 'Buzz'
        elif i%3 == 0:
            print 'Fizz'
        else:
            print i

fizzbuzz(101)
```



## Bracmat


```bracmat
0:?i&whl'(1+!i:<101:?i&out$(mod$(!i.3):0&(mod$(!i.5):0&FizzBuzz|Fizz)|mod$(!i.5):0&Buzz|!i))
```

Same code, pretty printed:

```bracmat
  0:?i
&   whl
  ' ( 1+!i:<101:?i
    &   out
      $ (   mod$(!i.3):0
          & ( mod$(!i.5):0&FizzBuzz
            | Fizz
            )
        | mod$(!i.5):0&Buzz
        | !i
        )
    )
```


=={{header|Brainfuck}}==
See [[FizzBuzz/EsoLang]]


## Brat


```brat
1.to 100 { n |
  true? n % 15 == 0
    { p "FizzBuzz" }
    { true? n % 3 == 0
      { p "Fizz" }
      { true? n % 5 == 0
        { p "Buzz" }
        { p n }
      }
    }
  }
```



## C


For 2 prime numbers and based on a similar minimal [[#JavaScript|JavaScript]] solution with low signal-to-noise, the C code is:

```c
  int i = 0 ;  char B[88] ;
  while ( i++ < 100 )
    !sprintf( B, "%s%s", i%3 ? "":"Fizz", i%5 ? "":"Buzz" )
    ? sprintf( B, "%d", i ):0, printf( ", %s", B );
```

With 4 prime numbers:

```c
  int i = 0 ;  char B[88] ;
  while ( i++ < 100 )
    !sprintf( B, "%s%s%s%s",
       i%3 ? "":"Fiz", i%5 ? "":"Buz", i%7 ? "":"Goz", i%11 ? "":"Kaz" )
    ? sprintf( B, "%d", i ):0, printf( ", %s", B );
```


```c
Output: ..., 89, FizBuz, Goz, 92, Fiz, 94, Buz, Fiz, 97, Goz, FizKaz, Buz
```

One line version, with pretty printing

```c>#include <stdio.h


int main() {
  for (int i=1; i<=105; i++) if (i%3 && i%5) printf("%3d ", i); else printf("%s%s%s", i%3?"":"Fizz", i%5?"":"Buzz", i%15?" ":"\n");
}

```


This actually works (the array init part, saves 6 bytes of static data, whee):
```c>#include<stdio.h


int main ()
{
  int i;
  const char *s[] = { "%d\n", "Fizz\n", s[3] + 4, "FizzBuzz\n" };
  for (i = 1; i <= 100; i++)
    printf(s[!(i % 3) + 2 * !(i % 5)], i);
  return 0;
}
```



```c>#include<stdio.h


int main (void)
{
    int i;
    for (i = 1; i <= 100; i++)
    {
        if (!(i % 15))
            printf ("FizzBuzz");
        else if (!(i % 3))
            printf ("Fizz");
        else if (!(i % 5))
            printf ("Buzz");
        else
            printf ("%d", i);

        printf("\n");
    }
    return 0;
}
```

Implicit int main and return 0 (C99+):

```c>#include <stdio.h


main() {
  int i = 1;
  while(i <= 100) {
    if(i % 15 == 0)
      puts("FizzBuzz");
    else if(i % 3 == 0)
      puts("Fizz");
    else if(i % 5 == 0)
      puts("Buzz");
    else
      printf("%d\n", i);
    i++;
  }
}
```

obfuscated:

```c>#include <stdio.h

#define F(x,y) printf("%s",i%x?"":#y"zz")
int main(int i){for(--i;i++^100;puts(""))F(3,Fi)|F(5,Bu)||printf("%i",i);return 0;}
```


With numbers theory:
```c>#include <stdio.h


int main(void)
{
    for (int i = 1; i <= 100; ++i) {
        if (i % 3 == 0) printf("fizz");
        if (i % 5 == 0) printf("buzz");
        if (i * i * i * i % 15 == 1) printf("%d", i);
        puts("");
    }
}

```


Without conditionals, anything in the loop body gcc compiles with branching, duplicate tests or duplicate strings. Depends on ASCII and two's complement arithmetic:


```c>#include <stdio.h

int main()
{
    for (int i=0;++i<101;puts(""))
    {
        char f[] = "FizzBuzz%d";
        f[8-i%5&12]=0;
        printf (f+(-i%3&4+f[8]/8), i);
    }
}

```


=={{header|C sharp|C#}}==

```csharp
class Program
{
public void FizzBuzzGo()
        {
            Boolean Fizz = false;
            Boolean Buzz = false;
            for (int count = 1; count <= 100; count ++)
            {
                Fizz = count % 3 == 0;
                Buzz = count % 5 == 0;
                if (Fizz && Buzz)
                {
                    Console.WriteLine("Fizz Buzz");
                    listBox1.Items.Add("Fizz Buzz");
                }
                else if (Fizz)
                {
                    Console.WriteLine("Fizz");
                    listBox1.Items.Add("Fizz");
                }
                else if (Buzz)
                {
                    Console.WriteLine("Buzz");
                    listBox1.Items.Add("Buzz");
                }
                else
                {
                    Console.WriteLine(count);
                    listBox1.Items.Add(count);
                }
            }
        }
}
```


```csharp
class Program
{
    static void Main()
    {
        for (uint i = 1; i <= 100; i++) {
            string s = null;

            if (i % 3 == 0)
                s = "Fizz";

            if (i % 5 == 0)
                s += "Buzz";

            System.Console.WriteLine(s ?? i.ToString());
        }
    }
}
```


```csharp
using System;
using System.Linq;

namespace FizzBuzz
{
    class Program
    {
        static void Main(string[] args)
        {
            Enumerable.Range(1, 100)
                .Select(a => String.Format("{0}{1}", a % 3 == 0 ? "Fizz" : string.Empty, a % 5 == 0 ? "Buzz" : string.Empty))
                .Select((b, i) => String.IsNullOrEmpty(b) ? (i + 1).ToString() : b)
                .ToList()
                .ForEach(Console.WriteLine);
        }
    }
}
```


```csharp
using System;
using System.Globalization;
using System.Linq;

namespace FizzBuzz
{
    class Program
    {
        static void Main()
        {
            Enumerable.Range(1, 100)
                .GroupBy(e => e % 15 == 0 ? "FizzBuzz" : e % 5 == 0 ? "Buzz" : e % 3 == 0 ? "Fizz" : string.Empty)
                .SelectMany(item => item.Select(x => new {
                    Value = x,
                    Display = String.IsNullOrEmpty(item.Key) ? x.ToString(CultureInfo.InvariantCulture) : item.Key
                }))
                .OrderBy(x => x.Value)
                .Select(x => x.Display)
                .ToList()
                .ForEach(Console.WriteLine);
        }
    }
}
```



```csharp
using System;

namespace FizzBuzz
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i = 1; i <= 100; i++)
            {
                if (i % 15 == 0)
                {
                    Console.WriteLine("FizzBuzz");
                }
                else if (i % 3 == 0)
                {
                    Console.WriteLine("Fizz");
                }
                else if (i % 5 == 0)
                {
                    Console.WriteLine("Buzz");
                }
                else
                {
                    Console.WriteLine(i);
                }
            }
        }
    }
}
```



```csharp
using System;
using System.Globalization;

namespace Rosettacode
{
    class Program
    {
        static void Main()
        {
            for (var number = 0; number < 100; number++)
            {
                if ((number % 3) == 0 & (number % 5) == 0)
                {
                    //For numbers which are multiples of both three and five print "FizzBuzz".
                    Console.WriteLine("FizzBuzz");
                    continue;
                }

                if ((number % 3) == 0) Console.WriteLine("Fizz");
                if ((number % 5) == 0) Console.WriteLine("Buzz");
                if ((number % 3) != 0 && (number % 5) != 0) Console.WriteLine(number.ToString(CultureInfo.InvariantCulture));

                if (number % 5 == 0)
                {
                    Console.WriteLine(Environment.NewLine);
                }
            }
        }
    }
}
```

TDD using delegates.

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace FizzBuzz
{
    [TestClass]
    public class FizzBuzzTest
    {
        private FizzBuzz fizzBuzzer;

        [TestInitialize]
        public void Initialize()
        {
            fizzBuzzer = new FizzBuzz();
        }

        [TestMethod]
        public void Give4WillReturn4()
        {
            Assert.AreEqual("4", fizzBuzzer.FizzBuzzer(4));
        }

        [TestMethod]
        public void Give9WillReturnFizz()
        {
            Assert.AreEqual("Fizz", fizzBuzzer.FizzBuzzer(9));
        }

        [TestMethod]
        public void Give25WillReturnBuzz()
        {
            Assert.AreEqual("Buzz", fizzBuzzer.FizzBuzzer(25));
        }

        [TestMethod]
        public void Give30WillReturnFizzBuzz()
        {
            Assert.AreEqual("FizzBuzz", fizzBuzzer.FizzBuzzer(30));
        }

        [TestMethod]
        public void First15()
        {
            ICollection expected = new ArrayList
                {"1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz"};

            var actual = Enumerable.Range(1, 15).Select(x => fizzBuzzer.FizzBuzzer(x)).ToList();

            CollectionAssert.AreEqual(expected, actual);
        }

        [TestMethod]
        public void From1To100_ToShowHowToGet100()
        {
            const int expected = 100;
            var actual = Enumerable.Range(1, 100).Select(x => fizzBuzzer.FizzBuzzer(x)).ToList();

            Assert.AreEqual(expected, actual.Count);
        }
    }

    public class FizzBuzz
    {
        private delegate string Xzzer(int value);
        private readonly IList<Xzzer> _functions = new List<Xzzer>();

        public FizzBuzz()
        {
            _functions.Add(x => x % 3 == 0 ? "Fizz" : "");
            _functions.Add(x => x % 5 == 0 ? "Buzz" : "");
        }

        public string FizzBuzzer(int value)
        {
            var result = _functions.Aggregate(String.Empty, (current, function) => current + function.Invoke(value));
            return String.IsNullOrEmpty(result) ? value.ToString(CultureInfo.InvariantCulture) : result;
        }
    }
}
```



```csharp
using System;
using System.Linq;

namespace FizzBuzz
{
  class Program
  {
    static void Main(string[] args)
    {
         Enumerable.Range(1, 100).ToList().ForEach(i => Console.WriteLine(i % 5 == 0 ? string.Format(i % 3 == 0 ? "Fizz{0}" : "{0}", "Buzz") : string.Format(i%3 == 0 ? "Fizz" : i.ToString())));
    }
  }
}
```



## C++


```cpp>#include <iostream


using namespace std;
int main ()
{
       for (int i = 1; i <= 100; i++)
       {
               if ((i % 15) == 0)
                       cout << "FizzBuzz\n";
               else if ((i % 3) == 0)
                       cout << "Fizz\n";
               else if ((i % 5) == 0)
                       cout << "Buzz\n";
               else
                       cout << i << "\n";
       }
       return 0;
}
```

Alternate version not using modulo 15:

```cpp>#include <iostream

using namespace std;

int main()
{
  for (int i = 0; i <= 100; ++i)
  {
    bool fizz = (i % 3) == 0;
    bool buzz = (i % 5) == 0;
    if (fizz)
      cout << "Fizz";
    if (buzz)
      cout << "Buzz";
    if (!fizz && !buzz)
      cout << i;
    cout << "\n";
  }
  return 0;
}
```


Alternate version that avoids using modulo.  (Modulo can be expensive on some architectures.)

```cpp>#include <iostream


int main()
{
    int i, f = 2, b = 4;

    for ( i = 1 ; i <= 100 ; ++i, --f, --b )
    {
        if ( f && b ) { std::cout << i;             }
        if ( !f )     { std::cout << "Fizz"; f = 3; }
        if ( !b )     { std::cout << "Buzz"; b = 5; }
        std::cout << std::endl;
    }

    return 0;
}

```


A version using std::transform:
{{works with|C++11}}

```cpp>#include <iostream

#include <algorithm>
#include <vector>

int main()
{
  std::vector<int> range(100);
  std::iota(range.begin(), range.end(), 1);

  std::vector<std::string> values;
  values.resize(range.size());

  auto fizzbuzz = [](int i) -> std::string {
    if ((i%15) == 0) return "FizzBuzz";
    if ((i%5) == 0)  return "Buzz";
    if ((i%3) == 0)  return "Fizz";
    return std::to_string(i);
  };

  std::transform(range.begin(), range.end(), values.begin(), fizzbuzz);

  for (auto& str: values) std::cout << str << std::endl;

  return 0;
}
```

Version computing FizzBuzz at compile time with metaprogramming:

```cpp>#include <iostream


template <int n, int m3, int m5>
struct fizzbuzz : fizzbuzz<n-1, (n-1)%3, (n-1)%5>
{
  fizzbuzz()
  { std::cout << n << std::endl; }
};

template <int n>
struct fizzbuzz<n, 0, 0> : fizzbuzz<n-1, (n-1)%3, (n-1)%5>
{
  fizzbuzz()
  { std::cout << "FizzBuzz" << std::endl; }
};

template <int n, int p>
struct fizzbuzz<n, 0, p> : fizzbuzz<n-1, (n-1)%3, (n-1)%5>
{
  fizzbuzz()
  { std::cout << "Fizz" << std::endl; }
};

template <int n, int p>
struct fizzbuzz<n, p, 0> : fizzbuzz<n-1, (n-1)%3, (n-1)%5>
{
  fizzbuzz()
  { std::cout << "Buzz" << std::endl; }
};

template <>
struct fizzbuzz<0,0,0>
{
  fizzbuzz()
  { std::cout << 0 << std::endl; }
};

template <int n>
struct fb_run
{
  fizzbuzz<n, n%3, n%5> fb;
};

int main()
{
  fb_run<100> fb;
  return 0;
}
```

Hardcore templates (compile with -ftemplate-depth-9000 -std=c++0x):

```cpp>#include <iostream

#include <string>
#include <cstdlib>
#include <boost/mpl/string.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/size_t.hpp>

using namespace std;
using namespace boost;

///////////////////////////////////////////////////////////////////////////////
// exponentiation calculations
template <int accum, int base, int exp> struct POWER_CORE : POWER_CORE<accum * base, base, exp - 1>{};

template <int accum, int base>
struct POWER_CORE<accum, base, 0>
{
    enum : int { val = accum };
};

template <int base, int exp> struct POWER : POWER_CORE<1, base, exp>{};

///////////////////////////////////////////////////////////////////////////////
// # of digit calculations
template <int depth, unsigned int i> struct NUM_DIGITS_CORE : NUM_DIGITS_CORE<depth + 1, i / 10>{};

template <int depth>
struct NUM_DIGITS_CORE<depth, 0>
{
    enum : int { val = depth};
};

template <int i> struct NUM_DIGITS : NUM_DIGITS_CORE<0, i>{};

template <>
struct NUM_DIGITS<0>
{
    enum : int { val = 1 };
};

///////////////////////////////////////////////////////////////////////////////
// Convert digit to character (1 -> '1')
template <int i>
struct DIGIT_TO_CHAR
{
    enum : char{ val = i + 48 };
};

///////////////////////////////////////////////////////////////////////////////
// Find the digit at a given offset into a number of the form 0000000017
template <unsigned int i, int place> // place -> [0 .. 10]
struct DIGIT_AT
{
    enum : char{ val = (i / POWER<10, place>::val) % 10 };
};

struct NULL_CHAR
{
    enum : char{ val = '\0' };
};

///////////////////////////////////////////////////////////////////////////////
// Convert the digit at a given offset into a number of the form '0000000017' to a character
template <unsigned int i, int place> // place -> [0 .. 9]
    struct ALT_CHAR : DIGIT_TO_CHAR< DIGIT_AT<i, place>::val >{};

///////////////////////////////////////////////////////////////////////////////
// Convert the digit at a given offset into a number of the form '17' to a character

// Template description, with specialization to generate null characters for out of range offsets
template <unsigned int i, int offset, int numDigits, bool inRange>
    struct OFFSET_CHAR_CORE_CHECKED{};
template <unsigned int i, int offset, int numDigits>
    struct OFFSET_CHAR_CORE_CHECKED<i, offset, numDigits, false> : NULL_CHAR{};
template <unsigned int i, int offset, int numDigits>
    struct OFFSET_CHAR_CORE_CHECKED<i, offset, numDigits, true>  : ALT_CHAR<i, (numDigits - offset) - 1 >{};

// Perform the range check and pass it on
template <unsigned int i, int offset, int numDigits>
    struct OFFSET_CHAR_CORE : OFFSET_CHAR_CORE_CHECKED<i, offset, numDigits, offset < numDigits>{};

// Calc the number of digits and pass it on
template <unsigned int i, int offset>
    struct OFFSET_CHAR : OFFSET_CHAR_CORE<i, offset, NUM_DIGITS<i>::val>{};

///////////////////////////////////////////////////////////////////////////////
// Integer to char* template. Works on unsigned ints.
template <unsigned int i>
struct IntToStr
{
    const static char str[];
    typedef typename mpl::string<
    OFFSET_CHAR<i, 0>::val,
    OFFSET_CHAR<i, 1>::val,
    OFFSET_CHAR<i, 2>::val,
    OFFSET_CHAR<i, 3>::val,
    OFFSET_CHAR<i, 4>::val,
    OFFSET_CHAR<i, 5>::val,
    /*OFFSET_CHAR<i, 6>::val,
    OFFSET_CHAR<i, 7>::val,
    OFFSET_CHAR<i, 8>::val,
    OFFSET_CHAR<i, 9>::val,*/
    NULL_CHAR::val>::type type;
};

template <unsigned int i>
const char IntToStr<i>::str[] =
{
    OFFSET_CHAR<i, 0>::val,
    OFFSET_CHAR<i, 1>::val,
    OFFSET_CHAR<i, 2>::val,
    OFFSET_CHAR<i, 3>::val,
    OFFSET_CHAR<i, 4>::val,
    OFFSET_CHAR<i, 5>::val,
    OFFSET_CHAR<i, 6>::val,
    OFFSET_CHAR<i, 7>::val,
    OFFSET_CHAR<i, 8>::val,
    OFFSET_CHAR<i, 9>::val,
    NULL_CHAR::val
};

template <bool condition, class Then, class Else>
struct IF
{
    typedef Then RET;
};

template <class Then, class Else>
struct IF<false, Then, Else>
{
    typedef Else RET;
};


template < typename Str1, typename Str2 >
struct concat : mpl::insert_range<Str1, typename mpl::end<Str1>::type, Str2> {};
template <typename Str1, typename Str2, typename Str3 >
struct concat3 : mpl::insert_range<Str1, typename mpl::end<Str1>::type, typename concat<Str2, Str3 >::type > {};

typedef typename mpl::string<'f','i','z','z'>::type fizz;
typedef typename mpl::string<'b','u','z','z'>::type buzz;
typedef typename mpl::string<'\r', '\n'>::type mpendl;
typedef typename concat<fizz, buzz>::type fizzbuzz;

// discovered boost mpl limitation on some length

template <int N>
struct FizzBuzz
{
    typedef typename concat3<typename FizzBuzz<N - 1>::type, typename IF<N % 15 == 0, typename fizzbuzz::type, typename IF<N % 3 == 0, typename fizz::type, typename IF<N % 5 == 0, typename buzz::type, typename IntToStr<N>::type >::RET >::RET >::RET, typename mpendl::type>::type type;
};

template <>
struct FizzBuzz<1>
{
    typedef mpl::string<'1','\r','\n'>::type type;
};

int main(int argc, char** argv)
{
    const int n = 7;
    std::cout << mpl::c_str<FizzBuzz<n>::type>::value << std::endl;
	return 0;
}
```

Note: it takes up lots of memory and takes several seconds to compile. To enable compilation for 7 < n <= 25, please, modify include/boost/mpl/limits/string.hpp BOOST_MPL_LIMIT_STRING_SIZE to 128 instead of 32).


## Casio BASIC

See [[FizzBuzz/Basic]]


## Cduce


```ocaml
(* FizzBuzz in CDuce *)

let format (n : Int) : Latin1 =
    if (n mod 3 = 0) || (n mod 5 = 0) then "FizzBuzz"
    else if (n mod 5 = 0) then "Buzz"
    else if (n mod 3 = 0) then "Fizz"
    else string_of (n);;

let fizz (n : Int, size : Int) : _ =
    print (format (n) @ "\n");
    if (n = size) then
        n = 0 (* do nothing *)
    else
        fizz(n + 1, size);;

let fizbuzz (size : Int) : _ = fizz (1, size);;

let _ = fizbuzz(100);;
```



## Ceylon


```Ceylon
shared void run() => {for (i in 1..100) {for (j->k in [3->"Fizz", 5->"Buzz"]) if (j.divides(i)) k}.reduce(plus) else i}.each(print);
```



## Chapel


```chapel
proc fizzbuzz(n) {
	for i in 1..n do
		if i % 15 == 0 then
			writeln("FizzBuzz");
		else if i % 5 == 0 then
			writeln("Buzz");
		else if i % 3 == 0 then
			writeln("Fizz");
		else
			writeln(i);
}

fizzbuzz(100);
```



## Chef

See [[FizzBuzz/EsoLang]]


## Clay


```clay
main() {
    for(i in range(1,100)) {
        if(i % 3 == 0 and i % 5 == 0) println("fizzbuzz");
        else if(i % 3 == 0) println("fizz");
        else if(i % 5 == 0) println("buzz");
        else print(i);
    }
}
```



## Clipper

Also compiles with Harbour (Harbour 3.2.0dev (r1405201749))

```Clipper
PROCEDURE Main()

   LOCAL n
   LOCAL cFB

   FOR n := 1 TO 100
      cFB := ""
      AEval( { { 3, "Fizz" }, { 5, "Buzz" } }, {|x| cFB += iif( ( n % x[ 1 ] ) == 0, x[ 2 ], "" ) } )
      ?? iif( cFB == "", LTrim( Str( n ) ), cFB ) + iif( n == 100, ".", ", " )
   NEXT

   RETURN

```

The advantage of this approach is that it is trivial to add another factor:

```txt
AEval( {{3,"Fizz"},{5,"Buzz"},{9,"Jazz"}}, {|x| cFB += Iif((n % x[1])==0, x[2], "")})
```



## CLIPS


```clips
(deffacts count
  (count-to 100)
)

(defrule print-numbers
  (count-to ?max)
  =>
  (loop-for-count (?num ?max) do
    (if
      (= (mod ?num 3) 0)
      then
      (printout t "Fizz")
    )
    (if
      (= (mod ?num 5) 0)
      then
      (printout t "Buzz")
    )
    (if
      (and (> (mod ?num 3) 0) (> (mod ?num 5) 0))
      then
      (printout t ?num)
    )
    (priint depth, unsigned int i> struct NUM_DIGITS_CORE : NUM_DIGITS_COREntout t crlf)
  )
)
```



## Clojure


```clojure

(doseq [x (range 1 101)] (println x (str (when (zero? (mod x 3)) "fizz") (when (zero? (mod x 5)) "buzz"))))

```



```clojure

(defn fizzbuzz [start finish]
  (map (fn [n]
	(cond
		(zero? (mod n 15)) "FizzBuzz"
		(zero? (mod n 3)) "Fizz"
		(zero? (mod n 5)) "Buzz"
		:else n))
	(range start finish)))
(fizzbuzz 1 100)

```



```lisp
(map (fn [x] (cond (zero? (mod x 15)) "FizzBuzz"
                   (zero? (mod x 5)) "Buzz"
                   (zero? (mod x 3)) "Fizz"
		     :else x))
     (range 1 101))
```


```lisp
(map #(let [s (str (if (zero? (mod % 3)) "Fizz") (if (zero? (mod % 5)) "Buzz"))] (if (empty? s) % s)) (range 1 101))
```


```lisp
(def fizzbuzz (map
  #(cond (zero? (mod % 15)) "FizzBuzz"
         (zero? (mod % 5)) "Buzz"
         (zero? (mod % 3)) "Fizz"
               :else %)
  (iterate inc 1)))
```



```lisp
(defn fizz-buzz
  ([] (fizz-buzz (range 1 101)))
  ([lst]
     (letfn [(fizz? [n] (zero? (mod n 3)))
	     (buzz? [n] (zero? (mod n 5)))]
       (let [f     "Fizz"
	     b     "Buzz"
	     items (map (fn [n]
			  (cond (and (fizz? n) (buzz? n)) (str f b)
				(fizz? n) f
				(buzz? n) b
				:else n))
			lst)] items))))
```


```clojure
(map (fn [n]
       (if-let [fb (seq (concat (when (zero? (mod n 3)) "Fizz")
                                (when (zero? (mod n 5)) "Buzz")))]
           (apply str fb)
           n))
     (range 1 101))
```


```clojure
(take 100 (map #(let [s (str %2 %3) ] (if (seq s) s (inc %)) )
            (range)
            (cycle [ "" "" "Fizz" ])
            (cycle [ "" "" "" "" "Buzz" ])))
```


```lisp
(map #(nth (conj (cycle [% % "Fizz" % "Buzz" "Fizz" % % "Fizz" "Buzz" % "Fizz" % % "FizzBuzz"]) %) %) (range 1 101))
```


```clojure
(let [n nil fizz (cycle [n n "fizz"]) buzz (cycle [n n n n "buzz"]) nums (iterate inc 1)]
  (take 20 (map #(if (or %1 %2) (str %1 %2) %3) fizz buzz nums)))
```


```clojure
(take 100
      (map #(if (pos? (compare %1 %2)) %1 %2)
           (map str (drop 1 (range)))
           (map str (cycle ["" "" "Fizz"]) (cycle ["" "" "" "" "Buzz"]))))

```


```clojure

;;Using clojure maps
(defn fizzbuzz
  [n]
  (let [rule {3 "Fizz"
              5 "Buzz"}
        divs (->> rule
                  (map first)
                  sort
                  (filter (comp (partial = 0)
                                (partial rem n))))]
    (if (empty? divs)
      (str n)
      (->> divs
           (map rule)
           (apply str)))))

(defn allfizzbuzz
  [max]
  (map fizzbuzz (range 1 (inc max))))

```


```clojure

(take 100
   (map #(str %1 %2 (if-not (or %1 %2) %3))
        (cycle [nil nil "Fizz"])
        (cycle [nil nil nil nil "Buzz"])
        (rest (range))
   ))

```



```clojure

(take 100
  (
    (fn [& fbspec]
      (let [
             fbseq #(->> (repeat nil) (cons %2) (take %1) reverse cycle)
             strfn #(apply str (if (every? nil? (rest %&)) (first %&)) (rest %&))
          ]
        (->>
          fbspec
          (partition 2)
          (map #(apply fbseq %))
          (apply map strfn (rest (range)))
          ) ;;endthread
      ) ;;endlet
    ) ;;endfn
    3 "Fizz" 5 "Buzz" 7 "Bazz"
  ) ;;endfn apply
) ;;endtake

```



## CMake


```cmake
foreach(i RANGE 1 100)
  math(EXPR off3 "${i} % 3")
  math(EXPR off5 "${i} % 5")
  if(NOT off3 AND NOT off5)
    message(FizzBuzz)
  elseif(NOT off3)
    message(Fizz)
  elseif(NOT off5)
    message(Buzz)
  else()
    message(${i})
  endif()
endforeach(i)
```



## COBOL



### Canonical version

{{works with|OpenCOBOL}}<!-- http://www.opencobol.org/ -->

```COBOL
      * FIZZBUZZ.COB
      * cobc -x -g FIZZBUZZ.COB
      *
       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           fizzbuzz.
       DATA                  DIVISION.
       WORKING-STORAGE       SECTION.
       01 CNT      PIC 9(03) VALUE 1.
       01 REM      PIC 9(03) VALUE 0.
       01 QUOTIENT PIC 9(03) VALUE 0.
       PROCEDURE             DIVISION.
      *
       PERFORM UNTIL CNT > 100
         DIVIDE 15 INTO CNT GIVING QUOTIENT REMAINDER REM
         IF REM = 0
           THEN
             DISPLAY "FizzBuzz " WITH NO ADVANCING
           ELSE
             DIVIDE 3 INTO CNT GIVING QUOTIENT REMAINDER REM
             IF REM = 0
               THEN
                 DISPLAY "Fizz " WITH NO ADVANCING
               ELSE
                 DIVIDE 5 INTO CNT GIVING QUOTIENT REMAINDER REM
                 IF REM = 0
                   THEN
                     DISPLAY "Buzz " WITH NO ADVANCING
                   ELSE
                     DISPLAY CNT " " WITH NO ADVANCING
                 END-IF
             END-IF
         END-IF
         ADD 1 TO CNT
       END-PERFORM
       DISPLAY ""
       STOP RUN.
```


### Simpler version

I know this doesn't have the full-bodied, piquant flavor
expected from COBOL, but it is a little shorter.
{{works with|OpenCOBOL}}

```cobol
Identification division.
Program-id. fizz-buzz.

Data division.
Working-storage section.

01 num pic 999.

Procedure division.
    Perform varying num from 1 by 1 until num > 100
        if function mod (num, 15) = 0 then display "fizzbuzz"
        else if function mod (num, 3) = 0 then display "fizz"
        else if function mod (num, 5) = 0 then display "buzz"
        else display num
    end-perform.
    Stop run.
```


### Evaluate Version

I think this shows clearly that it's resolving the problem and illuminating the rules specified
{{works with|OpenCOBOL}}

```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FIZZBUZZ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X PIC 999.
       01  Y PIC 999.
       01  REM3 PIC 999.
       01  REM5 PIC 999.
       PROCEDURE DIVISION.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 100
               DIVIDE X BY 3 GIVING Y REMAINDER REM3
               DIVIDE X BY 5 GIVING Y REMAINDER REM5
            EVALUATE REM3 ALSO REM5
              WHEN ZERO ALSO ZERO
                DISPLAY "FizzBuzz"
              WHEN ZERO ALSO ANY
                DISPLAY "Fizz"
              WHEN ANY ALSO ZERO
                DISPLAY "Buzz"
              WHEN OTHER
                DISPLAY X
            END-EVALUATE
           END-PERFORM
           STOP RUN
           .

```



### Chase the Fizz

{{works with|OpenCOBOL}}
A solution that simply evaluates and adds.

```cobol>         >
SOURCE FORMAT FREE
identification division.
program-id. fizzbuzz.
data division.
working-storage section.
01  i pic 999.
01  fizz pic 999 value 3.
01  buzz pic 999 value 5.
procedure division.
start-fizzbuzz.
    perform varying i from 1 by 1 until i > 100
        evaluate i also i
        when fizz also buzz
            display 'fizzbuzz'
            add 3 to fizz
            add 5 to buzz
        when fizz also any
            display 'fizz'
            add 3 to fizz
        when buzz also any
            display 'buzz'
            add 5 to buzz
        when other
            display i
        end-evaluate
    end-perform
    stop run
    .
end program fizzbuzz.

```



## Coco


```coco
for i from 1 to 100
    console.log do
       if      i % 15 == 0 then 'FizzBuzz'
       else if i % 3 == 0 then 'Fizz'
       else if i % 5 == 0 then 'Buzz'
       else i
```



```coco
for i from 1 to 100
    console.log(['Fizz' unless i % 3] + ['Buzz' unless i % 5] or String(i))
```



## CoffeeScript


```CoffeeScript
for i in [1..100]
  if i % 15 is 0
    console.log "FizzBuzz"
  else if i % 3 is 0
    console.log "Fizz"
  else if i % 5 is 0
    console.log "Buzz"
  else
    console.log i
```


```CoffeeScript
for i in [1..100]
  console.log \
    if i % 15 is 0
      "FizzBuzz"
    else if i % 3 is 0
      "Fizz"
    else if i % 5 is 0
      "Buzz"
    else
      i
```


```CoffeeScript
for i in [1..100]
  console.log(['Fizz' if i % 3 is 0] + ['Buzz' if i % 5 is 0] or i)
```



## ColdFusion


```cfm
<Cfloop from="1" to="100" index="i">
  <Cfif i mod 15 eq 0>FizzBuzz
  <Cfelseif i mod 5 eq 0>Fizz
  <Cfelseif i mod 3 eq 0>Buzz
  <Cfelse><Cfoutput>#i# </Cfoutput>
  </Cfif>
</Cfloop>
```

cfscript version

```cfm><cfscript

result = "";
  for(i=1;i<=100;i++){
    result=ListAppend(result, (i%15==0) ? "FizzBuzz": (i%5==0) ? "Buzz" : (i%3 eq 0)? "Fizz" : i );
  }
  WriteOutput(result);
</cfscript>
```



## Comefrom0x10



```cf0x10
fizzbuzz
  mod_three = 3
  mod_five = 5
  comefrom fizzbuzz
  n
  comefrom fizzbuzz if n is mod_three
  comefrom fizzbuzz if n is mod_five
  n = n + 1

  fizz
    comefrom fizzbuzz if n is mod_three
    'Fizz'...
    mod_three = mod_three + 3
    linebreak
      # would like to write "unless mod_three is mod_five"
      comefrom fizz if mod_three - mod_five - 3
      ''

  buzz
    comefrom fizzbuzz if n is mod_five
    'Buzz'
    mod_five = mod_five + 5

  comefrom fizzbuzz if n is 100
```



## Common Lisp

Solution 1:

```lisp
(defun fizzbuzz ()
  (loop for x from 1 to 100 do
    (princ (cond ((zerop (mod x 15)) "FizzBuzz")
                 ((zerop (mod x 3))  "Fizz")
                 ((zerop (mod x 5))  "Buzz")
                 (t                  x)))
    (terpri)))
```

Solution 2:

```lisp
(defun fizzbuzz ()
  (loop for x from 1 to 100 do
    (format t "~&~{~A~}"
      (or (append (when (zerop (mod x 3)) '("Fizz"))
                  (when (zerop (mod x 5)) '("Buzz")))
          (list x)))))
```

Solution 3:

```lisp
(defun fizzbuzz ()
  (loop for n from 1 to 100
     do (format t "~&~[~[FizzBuzz~:;Fizz~]~*~:;~[Buzz~*~:;~D~]~]~%"
                (mod n 3) (mod n 5) n)))
```

Solution 4:

```lisp
(loop as n from 1 to 100
      as fizz = (zerop (mod n 3))
      as buzz = (zerop (mod n 5))
      as numb = (not (or fizz buzz))
      do
  (format t
   "~&~:[~;Fizz~]~:[~;Buzz~]~:[~;~D~]~%"
   fizz buzz numb n))
```

Solution 5:

```lisp
(format t "~{~:[~&~;~:*~:(~a~)~]~}"
  (loop as n from 1 to 100
        as f = (zerop (mod n 3))
        as b = (zerop (mod n 5))
        collect nil
        if f collect 'fizz
        if b collect 'buzz
        if (not (or f b)) collect n))
```

Solution 6:

```lisp
(format t "~{~{~:[~;Fizz~]~:[~;Buzz~]~:[~*~;~d~]~}~%~}"
  (loop as n from 1 to 100
        as f = (zerop (mod n 3))
        as b = (zerop (mod n 5))
        collect (list f b (not (or f b)) n)))
```


Solution 7:

```lisp
(defun core (x)
  (mapcar
    #'(lambda (a b) (if (equal 0 (mod x a)) b x))
    '(3 5)
    '("fizz" "buzz")))

(defun filter-core (x)
  (if (equal 1 (length (remove-duplicates x)))
    (list (car x))
    (remove-if-not #'stringp x)))

(defun fizzbuzz (x)
  (loop for a from 1 to x do
    (print (format nil "~{~a~}" (filter-core (core a))))))

(fizzbuzz 100)
```

First 16 lines of output:

```txt

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16

```



###  Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : FizzBuzz

(defun fizzbuzz (&optional n)
          (let ((n (or n 1)))
          (if (> n 100)
              nil
              (progn
              (let ((mult-3 (is-mult-p n 3))
              (mult-5 (is-mult-p n 5)))
              (if mult-3
                  (princ "Fizz"))
              (if mult-5
                  (princ "Buzz"))
              (if (not (or mult-3 mult-5))
                  (princ n))
              (princ #\linefeed)
              (fizzbuzz (+ n 1)))))))
(defun is-mult-p (n multiple)
          (= (rem n multiple) 0))
(fizzbuzz 1)

```

Output:

```txt

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz

```



## Crystal


```ruby
1.upto(100) do |v|
  p fizz_buzz(v)
end

def fizz_buzz(value)
  word = ""
  word += "fizz" if value % 3 == 0
  word += "buzz" if value % 5 == 0
  word += value.to_s if word.empty?
  word
end
```



## CSS

<pre style="font-size:84%;height:75ex">
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
<style>
li {
  list-style-position: inside;
}
li:nth-child(3n), li:nth-child(5n) {
  list-style-type: none;
}
li:nth-child(3n)::before {
  content:'Fizz';
}
li:nth-child(5n)::after {
  content:'Buzz';
}
</style>
</head>
<body>
  <ol>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
  </ol>
</body>
</html>

```



## Cubescript

<lang>alias fizzbuzz [
	loop i 100 [
		push i (+ $i 1) [
			cond (! (mod $i 15)) [
				echo FizzBuzz
			] (! (mod $i 3)) [
				echo Fizz
			] (! (mod $i 5)) [
				echo Buzz
			] [
				echo $i
			]
		]
	]
]
```



## D


```d
import std.stdio, std.algorithm, std.conv;

/// With if-else.
void fizzBuzz(in uint n) {
    foreach (immutable i; 1 .. n + 1)
        if (!(i % 15))
            "FizzBuzz".writeln;
        else if (!(i % 3))
            "Fizz".writeln;
        else if (!(i % 5))
            "Buzz".writeln;
        else
            i.writeln;
}

/// With switch case.
void fizzBuzzSwitch(in uint n) {
    foreach (immutable i; 1 .. n + 1)
        switch (i % 15) {
            case 0:
                "FizzBuzz".writeln;
                break;
            case 3, 6, 9, 12:
                "Fizz".writeln;
                break;
            case 5, 10:
                "Buzz".writeln;
                break;
            default:
                i.writeln;
        }
}

void fizzBuzzSwitch2(in uint n) {
    foreach (immutable i; 1 .. n + 1)
        (i % 15).predSwitch(
        0,       "FizzBuzz",
        3,       "Fizz",
        5,       "Buzz",
        6,       "Fizz",
        9,       "Fizz",
        10,      "Buzz",
        12,      "Fizz",
        /*else*/ i.text).writeln;
}

void main() {
    100.fizzBuzz;
    writeln;
    100.fizzBuzzSwitch;
    writeln;
    100.fizzBuzzSwitch2;
}
```

Alternate version calculating values at compile time:

```d
import std;

void main()
{
    auto fizzbuzz(in uint i)
    {
        string r;
        if (i % 3 == 0) r ~= "fizz";
        if (i % 5 == 0) r ~= "buzz";
        if (r.length == 0) r ~= i.to!string;
        return r;
    }

    enum r = 1.iota(101).map!fizzbuzz;

    r.each!writeln;
}
```



## Dart


```dart

main() {
  for (int i = 1; i <= 100; i++) {
    List<String> out = [];
    if (i % 3 == 0)
      out.add("Fizz");
    if (i % 5 == 0)
      out.add("Buzz");
    print(out.length > 0 ? out.join("") : i);
  }
}

```



## dc

{{trans|bc}}

```dc
[[Fizz]P 1 sw]sF
[[Buzz]P 1 sw]sB
[li p sz]sN
[[
]P]sW
[
 0 sw         [w = 0]sz
 li 3 % 0 =F  [Fizz if 0 == i % 3]sz
 li 5 % 0 =B  [Buzz if 0 == i % 5]sz
 lw 0 =N      [print Number if 0 == w]sz
 lw 1 =W      [print neWline if 1 == w]sz
 li 1 + si    [i += 1]sz
 li 100 !<L   [continue Loop if 100 >= i]sz
]sL
1 si          [i = 1]sz
0 0 =L        [enter Loop]sz
```

The bc translation written in dc style.

```dc

# dc is stack based, so we use the stack instead of a variable for our
# current number.

1                       # Number = 1
[[Fizz]n 1 sw]sF        # Prints "Fizz" prevents Number from printing
[[Buzz]n 1 sw]sB        # Prints "Buzz" prevents Number from printing
[dn]sN                  # Prints Number
[
        dd              # Put two extra copies of Number on stack
        0 sw            # w = 0
        3% 0=F          # Fizz if 0 == Number % 3 (destroys 1st copy)
        5% 0=B          # Buzz if 0 == Number % 5 (destroys 2nd copy)
        lw 0=N          # Print Number if 0 == w
        [
]n                      # Print new line
        1+d             # Number += 1 and put extra copy on stack
        100!<L          # Continue Loop if 100 >= Number (destroys copy)
]dsLx                   # Enter Loop
```



## Delphi


```Delphi
program FizzBuzz;

{$APPTYPE CONSOLE}

uses SysUtils;

var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if i mod 15 = 0 then
      Writeln('FizzBuzz')
    else if i mod 3 = 0 then
      Writeln('Fizz')
    else if i mod 5 = 0 then
      Writeln('Buzz')
    else
      Writeln(i);
  end;
end.
```



## DeviousYarn


```deviousyarn
each { x range(1 100)
    ?  { divisible(x 3)
        p:'Fizz' }
    ?  { divisible(x 5)
        p:'Buzz' }
    -? { !:divisible(x 3)
        p:x }
    o
}
```



## DWScript


```delphi
var i : Integer;

for i := 1 to 100 do begin
   if i mod 15 = 0 then
      PrintLn('FizzBuzz')
   else if i mod 3 = 0 then
      PrintLn('Fizz')
   else if i mod 5 = 0 then
      PrintLn('Buzz')
   else PrintLn(i);
end;
```


=={{header|Déjà Vu}}==

```dejavu
for i range 1 100:
	if = 0 % i 15:
		"FizzBuzz"
	elseif = 0 % i 3:
		"Fizz"
	elseif = 0 % i 5:
		"Buzz"
	else:
		i
	!print
```



## DUP


FizzBuzz, realized using two different methods for string/character output:

Output to STDOUT via single character output.

```DUP
[$$3/%$[]['F,'i,'z,'z,]?\5/%$[]['B,'u,'z,'z,]?*[$.][]?10,]c:    {define function c: mod 3, mod 5 tests, print proper output}
0[$100<][1+c;!]#                                                {loop from 1 to 100}
```


Output to STDOUT, using stored strings and a separately defined string output operator:

```DUP
[\[^^>][$;,1+]#%%]⇒P                                       {define operator P: print stored string}
[$$3/%$[][0$"Fizz"P]?\5/%$[][0$"Buzz"P]?*[$.][]?10,]c:     {define function c: mod 3, mod 5 tests, print according output}
0[$100<][1+c;!]#                                           {loop from 1 to 100}
```



## Dyalect



```Dyalect
var n = 1

while n < 20 {
    if n % 15 == 0 {
        print("fizzbuzz")
    } else if n % 3 == 0 {
        print("fizz")
    } else if n % 5 == 0 {
        print("buzz")
    } else {
        print(n)
    }

    n = n + 1
}
```


{{out}}


```txt
1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz
16
17
fizz
19
```



## E


```e
for i in 1..100 {
   println(switch ([i % 3, i % 5]) {
     match [==0, ==0] { "FizzBuzz" }
     match [==0, _  ] { "Fizz" }
     match [_,   ==0] { "Buzz" }
     match _          { i }
   })
 }
```



## EasyLang

<lang>for i = 1 to 100
  if i mod 15 = 0
    print "FizzBuzz"
  elif i mod 5 = 0
    print "Buzz"
  elif i mod 3 = 0
    print "Fizz"
  else
    print i
  .
.
```



## ECL


```ECL
DataRec := RECORD
    STRING  s;
END;

DataRec MakeDataRec(UNSIGNED c) := TRANSFORM
    SELF.s := MAP
        (
            c % 15 = 0  =>  'FizzBuzz',
            c % 3 = 0   =>  'Fizz',
            c % 5 = 0   =>  'Buzz',
            (STRING)c
        );
END;

d := DATASET(100,MakeDataRec(COUNTER));

OUTPUT(d);
```



## Eero


```objc>#import <Foundation/Foundation.h


int main()
  autoreleasepool

    for int i in 1 .. 100
      s := ''
      if i % 3 == 0
        s << 'Fizz'
      if i % 5 == 0
        s << 'Buzz'
      Log( '(%d) %@', i, s )

  return 0
```



## Egel


```Egel

import "prelude.eg"
import "io.ego"

using System
using IO

def fizzbuzz =
    [ 100 -> print "100\n"
    | N ->
        if and ((N%3) == 0) ((N%5) == 0) then
            let _ = print "fizz buzz, " in fizzbuzz (N+1)
        else if (N%3) == 0 then
            let _ = print "fizz, " in fizzbuzz (N+1)
        else if (N%5) == 0 then
            let _ = print "buzz, " in fizzbuzz (N+1)
        else
            let _ = print N ", " in fizzbuzz (N+1) ]

def main = fizzbuzz 1

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			fizzbuzz
		end

	fizzbuzz
	        --Numbers up to 100, prints "Fizz" instead of multiples of 3, and "Buzz" for multiples of 5.
	        --For multiples of both 3 and 5 prints "FizzBuzz".
		do
			across
				1 |..| 100 as c
			loop
				if c.item \\ 15 = 0 then
					io.put_string ("FIZZBUZZ%N")
				elseif c.item \\ 3 = 0 then
					io.put_string ("FIZZ%N")
				elseif c.item \\ 5 = 0 then
					io.put_string ("BUZZ%N")
				else
					io.put_string (c.item.out + "%N")
				end
			end
		end

end


```



## Ela


```ela
open list

prt x | x % 15 == 0 = "FizzBuzz"
      | x % 3 == 0  = "Fizz"
      | x % 5 == 0  = "Buzz"
      | else        = x

[1..100] |> map prt
```



## Elixir


### Standard approaches

used case

```elixir
Enum.each 1..100, fn x ->
  IO.puts(case { rem(x,3) == 0, rem(x,5) == 0 } do
    { true, true }   -> "FizzBuzz"
    { true, false }  -> "Fizz"
    { false, true }  -> "Buzz"
    { false, false } -> x
  end)
end
```


Alternate approach using pipes and cond:


```elixir
#!/usr/bin/env elixir
1..100 |> Enum.map(fn i ->
  cond do
    rem(i,3*5) == 0 -> "FizzBuzz"
    rem(i,3) == 0   -> "Fizz"
    rem(i,5) == 0   -> "Buzz"
    true            -> i
  end
end) |> Enum.each(fn i -> IO.puts i end)
```


used Stream.cycle version:

```elixir
defmodule RC do
  def fizzbuzz(limit \\ 100) do
    fizz = Stream.cycle(["", "", "Fizz"])
    buzz = Stream.cycle(["", "", "", "", "Buzz"])
    Stream.zip(fizz, buzz)
    |> Enum.take(limit)
    |> Enum.with_index
    |> Enum.each(fn {{f,b},i} ->
         IO.puts if f<>b=="", do: i+1, else: f<>b
       end)
  end
end

RC.fizzbuzz
```


Yet another approach:

```elixir
defmodule FizzBuzz do
  def fizzbuzz(n) when rem(n, 15) == 0, do: "FizzBuzz"
  def fizzbuzz(n) when rem(n,  5) == 0, do: "Buzz"
  def fizzbuzz(n) when rem(n,  3) == 0, do: "Fizz"
  def fizzbuzz(n),                      do: n
end

Enum.each(1..100, &IO.puts FizzBuzz.fizzbuzz &1)
```


used anonymous function

```elixir
f = fn(n) when rem(n,15)==0 -> "FizzBuzz"
      (n) when rem(n,5)==0  -> "Fizz"
      (n) when rem(n,3)==0  -> "Buzz"
      (n)                   -> n
end

for n <- 1..100, do: IO.puts f.(n)
```


Enum.at version: Returns nil if index is out of bounds.

```elixir
Enum.each(1..100, fn i ->
  str = "#{Enum.at([:Fizz], rem(i,3))}#{Enum.at([:Buzz], rem(i,5))}"
  IO.puts if str=="", do: i, else: str
end)
```



### A macro too far

The Stream.cycle version above, but as an overpowered FizzBuzz DSL.

```elixir
defmodule BadFizz do
  # Hand-rolls a bunch of AST before injecting the resulting FizzBuzz code.
  defmacrop automate_fizz(fizzers, n) do
    # To begin, we need to process fizzers to produce the various components
    # we're using in the final assembly. As told by Mickens telling as Antonio
    # Banderas, first you must specify a mapping function:
    build_parts = (fn {fz, n} ->
      ast_ref = {fz |> String.downcase |> String.to_atom, [], __MODULE__}
      clist   = List.duplicate("", n - 1) ++ [fz]
      cycle   = quote do: unquote(ast_ref) = unquote(clist) |> Stream.cycle

      {ast_ref, cycle}
    end)

    # ...and then a reducing function:
    collate = (fn
      ({ast_ref, cycle}, {ast_refs, cycles}) ->
        {[ast_ref | ast_refs], [cycle | cycles]}
    end)

    # ...and then, my love, when you are done your computation is ready to run
    # across thousands of fizzbuzz:
    {ast_refs, cycles} = fizzers
    |> Code.eval_quoted([], __ENV__) |> elem(0) # Gotta unwrap this mystery code~
    |> Enum.sort(fn ({_, ap}, {_, bp}) -> ap < bp end) # Sort so that Fizz, 3 < Buzz, 5
    |> Enum.map(build_parts)
    |> Enum.reduce({[], []}, collate)

    # Setup the anonymous functions used by Enum.reduce to build our AST components.
    # This was previously handled by List.foldl, but ejected because reduce/2's
    # default behavior reduces repetition.
    #
    # ...I was tempted to move these into a macro themselves, and thought better of it.
    build_zip    = fn (varname, ast) ->
      quote do: Stream.zip(unquote(varname), unquote(ast))
    end
    build_tuple  = fn (varname, ast) ->
      {:{}, [], [varname, ast]}
    end
    build_concat = fn (varname, ast) ->
        {:<>,
        [context: __MODULE__, import: Kernel], # Hygiene values may change; accurate to Elixir 1.1.1
        [varname, ast]}
    end

    # Toss cycles into a block by hand, then smash ast_refs into
    # a few different computations on the cycle block results.
    cycles = {:__block__, [], cycles}
    tuple  = ast_refs |> Enum.reduce(build_tuple)
    zip    = ast_refs |> Enum.reduce(build_zip)
    concat = ast_refs |> Enum.reduce(build_concat)

    # Finally-- Now that all our components are assembled, we can put
    # together the fizzbuzz stream pipeline. After quote ends, this
    # block is injected into the caller's context.
    quote do
      unquote(cycles)

      unquote(zip)
      |> Stream.with_index
      |> Enum.take(unquote(n))
      |> Enum.each(fn
      {unquote(tuple), i} ->
        ccats = unquote(concat)
        IO.puts if ccats == "", do: i + 1, else: ccats
      end)
    end
  end

  @doc ~S"""
    A fizzing, and possibly buzzing function. Somehow, you feel like you've
    seen this before. An old friend, suddenly appearing in Kafkaesque nightmare...

    ...or worse, during a whiteboard interview.
  """
  def fizz(n \\ 100) when is_number(n) do
    # In reward for all that effort above, we now have the latest in
    # programmer productivity:
    #
    # A DSL for building arbitrary fizzing, buzzing, bazzing, and more!
    [{"Fizz", 3},
     {"Buzz", 5}#,
     #{"Bar", 7},
     #{"Foo", 243}, # -> Always printed last (largest number)
     #{"Qux", 34}
    ]
    |> automate_fizz(n)
  end
end

BadFizz.fizz(100) # => Prints to stdout
```



## Elm

A bit too simple:

```elm
import Html exposing (text)
import List exposing (map)

main =
  [1..100] |> map getWordForNum |> text

getWordForNum num =
  if num % 15 == 0 then
    "FizzBuzz"
  else if num % 3 == 0 then
    "Fizz"
  else if num % 5 == 0 then
    "Buzz"
  else
    String.fromInt num
```


A bit too clever:

```elm
import Html exposing (text)
import List exposing (map)
import String exposing (join, fromInt)

main : Html.Html
main =
  [1..100] |> map fizzbuzz |> join " " |> text

fizzbuzz : Int -> String
fizzbuzz num =
  let
    fizz = if num % 3 == 0 then "Fizz" else ""
    buzz = if num % 5 == 0 then "Buzz" else ""
  in
    if fizz == buzz then
      fromInt num
    else
      fizz ++ buzz
```



## Emacs Lisp


```Lisp

(defun fizzbuzz (n)
  (cond ((and
	  (eq (% n 5) 0)
	  (eq (% n 3) 0))  "FizzBuzz")
	((eq (% n 3) 0)  "Fizz")
	((eq (% n 5) 0)  "Buzz")
	(t  n)))

;; loop & print from 0 to 100
(dotimes (i 101) (princ-list (fizzbuzz i)))

```



## Erlang


```erlang
fizzbuzz() ->
    F = fun(N) when N rem 15 == 0 -> "FizzBuzz";
           (N) when N rem 3 == 0  -> "Fizz";
           (N) when N rem 5 == 0  -> "Buzz";
           (N) -> integer_to_list(N)
        end,
    [F(N)++"\n" || N <- lists:seq(1,100)].
```



## ERRE


```ERRE

PROGRAM FIZZ_BUZZ
!
! for rosettacode.org
!
BEGIN
 FOR A=1 TO 100 DO
   IF A MOD 15=0 THEN
      PRINT("FizzBuzz")
   ELSIF A MOD 3=0 THEN
      PRINT("Fizz")
   ELSIF A MOD 5=0 THEN
      PRINT("Buzz")
   ELSE
      PRINT(A)
   END IF
 END FOR
END PROGRAM

```



## Euphoria

{{works with|Euphoria|4.0.0}}
This is based on the [[VBScript]] example.

```Euphoria
include std/utils.e

function fb( atom n )
	sequence fb
	if remainder( n, 15 ) = 0 then
		fb = "FizzBuzz"
	elsif remainder( n, 5 ) = 0 then
		fb = "Fizz"
	elsif remainder( n, 3 ) = 0 then
		fb = "Buzz"
	else
		fb = sprintf( "%d", n )
	end if
	return fb
end function

function fb2( atom n )
	return iif( remainder(n, 15) = 0, "FizzBuzz",
		iif( remainder( n, 5 ) = 0, "Fizz",
		iif( remainder( n, 3) = 0, "Buzz", sprintf( "%d", n ) ) ) )
end function

for i = 1 to 30 do
	printf( 1, "%s ", { fb( i ) } )
end for

puts( 1, "\n" )

for i = 1 to 30 do
	printf( 1, "%s ", { fb2( i ) } )
end for

puts( 1, "\n" )
```


=={{header|F Sharp|F#}}==

```fsharp
let fizzbuzz n =
    match n%3 = 0, n%5 = 0 with
    | true, false -> "fizz"
    | false, true -> "buzz"
    | true, true  -> "fizzbuzz"
    | _ -> string n

let printFizzbuzz() =
    [1..100] |> List.iter (fizzbuzz >> printfn "%s")
```


```fsharp
[1..100]
|> List.map (fun x ->
            match x with
            | _ when x % 15 = 0 ->"fizzbuzz"
            | _ when x % 5 = 0 -> "buzz"
            | _ when x % 3 = 0 -> "fizz"
            | _ ->  x.ToString())
|> List.iter (fun x -> printfn "%s" x)
```

Another example using (unnecessary) partial active pattern :D

```fsharp
let (|MultipleOf|_|) divisors number =
    if Seq.exists ((%) number >> (<>) 0) divisors
    then None
    else Some ()

let fizzbuzz = function
| MultipleOf [3; 5] -> "fizzbuzz"
| MultipleOf [3]    -> "fizz"
| MultipleOf [5]    -> "buzz"
| n                 -> string n

{ 1 .. 100 }
|> Seq.iter (fizzbuzz >> printfn "%s")
```



## Factor


```factor
USING: math kernel io math.functions math.parser math.ranges ;
IN: fizzbuzz
: fizz ( n -- str ) 3 divisor? "Fizz" "" ? ;
: buzz ( n -- str ) 5 divisor? "Buzz" "" ? ;
: fizzbuzz ( n -- str ) dup [ fizz ] [ buzz ] bi append [ number>string ] [ nip ] if-empty ;
: main ( -- ) 100 [1,b] [ fizzbuzz print ] each ;
MAIN: main
```


More flexible variant without divisibility tests.

```factor

USING: kernel sequences arrays generalizations fry math math.parser prettyprint ;
IN: fizzbuzz

: zz ( m seq -- v ) dup length 1 <array> V{ } clone 4 -nrot 1 4 -nrot 3 nrot
 '[ dup _ <= ]
  3 -nrot
 '[
    "" _ [ _ [ swap execute( str n -- str n ) ] change-nth ] each-index
    dup empty? [ drop dup number>string ] [ ] if swapd suffix! swap 1 +
  ]
  while drop ;

: fizz ( str n -- str n ) dup 3 < [ 1 + ] [ drop "Fizz" append 1 ] if ;
: buzz ( str n -- str n ) dup 5 < [ 1 + ] [ drop "Buzz" append 1 ] if ;
: quxx ( str n -- str n ) dup 7 < [ 1 + ] [ drop "Quxx" append 1 ] if ;
: FizzBuzzQuxx ( m -- v ) { fizz buzz quxx } zz ;
: FizzBuzzQuxx-100 ( -- ) 100 FizzBuzzQuxx . ;

MAIN: FizzBuzzQuxx-100

```



## Falcon


```falcon
for i in [1:101]
    switch i % 15
    case 0        : > "FizzBuzz"
    case 5,10     : > "Buzz"
    case 3,6,9,12 : > "Fizz"
    default       : > i
    end
end
```



## FALSE

See [[FizzBuzz/EsoLang]]


## Fantom


```fantom
class FizzBuzz
{
  public static Void main ()
  {
    for (Int i:=1; i <= 100; ++i)
    {
      if (i % 15 == 0)
        echo ("FizzBuzz")
      else if (i % 3 == 0)
        echo ("Fizz")
      else if (i % 5 == 0)
        echo ("Buzz")
      else
        echo (i)
    }
  }
}
```



## FBSL

'''No 'MOD 15' needed.'''

```qbasic
#APPTYPE CONSOLE

DIM numbers AS STRING
DIM imod5 AS INTEGER
DIM imod3 AS INTEGER

FOR DIM i = 1 TO 100
    numbers = ""
    imod3 = i MOD 3
    imod5 = i MOD 5
    IF NOT imod3 THEN numbers = "Fizz"
    IF NOT imod5 THEN numbers = numbers & "Buzz"
    IF imod3 AND imod5 THEN numbers = i
    PRINT numbers, " ";
NEXT

PAUSE
```

{{out}}
 1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fiz
 z 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz
  41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz Fizz 52 53 Fizz Buzz 56 Fizz 58 59 Fi
 zzBuzz 61 62 Fizz 64 Buzz Fizz 67 68 Fizz Buzz 71 Fizz 73 74 FizzBuzz 76 77 Fizz
  79 Buzz Fizz 82 83 Fizz Buzz 86 Fizz 88 89 FizzBuzz 91 92 Fizz 94 Buzz Fizz 97
 98 Fizz Buzz
 Press any key to continue...


## FOCAL

<tt>FITR</tt> is a built-in function that truncates a floating-point number to an integer. Note that FOCAL uses an arithmetic (three-way) <tt>IF</tt> statement, rather like early Fortran.

```focal
01.10 FOR I=1,100; DO 2.0
01.20 QUIT

02.10 SET ZB=I/15 - FITR(I/15)
02.20 IF (ZB) 2.4, 2.3, 2.4
02.30 TYPE "FizzBuzz" !
02.35 RETURN
02.40 SET Z=I/3 - FITR(I/3)
02.50 IF (Z) 2.7, 2.6, 2.7
02.60 TYPE "Fizz" !
02.65 RETURN
02.70 SET B=I/5 - FITR(I/5)
02.80 IF (B) 2.99, 2.9, 2.99
02.90 TYPE "Buzz" !
02.95 RETURN
02.99 TYPE %3, I, !
```



## Forth

===table-driven===

```forth
: fizz ( n -- ) drop ." Fizz" ;
: buzz ( n -- ) drop ." Buzz" ;
: fb   ( n -- ) drop ." FizzBuzz" ;
: vector create does> ( n -- )
  over 15 mod cells + @ execute ;
vector .fizzbuzz
  ' fb   , ' . ,    ' . ,
  ' fizz , ' . ,    ' buzz ,
  ' fizz , ' . ,    ' . ,
  ' fizz , ' buzz , ' . ,
  ' fizz , ' . ,    ' . ,
```


### or the classic approach


```forth
: .fizzbuzz ( n -- )
  0 pad c!
  dup 3 mod 0= if s" Fizz" pad  place then
  dup 5 mod 0= if s" Buzz" pad +place then
  pad c@ if drop pad count type else . then ;

: zz ( n -- )
  1+ 1 do i .fizzbuzz cr loop ;
100 zz
```


### the well factored approach

SYNONYM is a Forth200x word.


```forth
SYNONYM NOT INVERT \ Bitwise boolean not

: Fizz?  ( n -- ? )  3 MOD 0=  DUP IF ." Fizz" THEN ;
: Buzz?  ( n -- ? )  5 MOD 0=  DUP IF ." Buzz" THEN ;
: ?print  ( n ? -- )  IF . THEN ;
: FizzBuzz  ( -- )
   101 1 DO CR  I  DUP Fizz? OVER Buzz? OR  NOT ?print  LOOP ;

FizzBuzz
```


### the unrolled approach


```forth
: n     ( n -- n+1 )    dup .         1+ ;
: f     ( n -- n+1 )    ." Fizz "     1+ ;
: b     ( n -- n+1 )    ." Buzz "     1+ ;
: fb    ( n -- n+1 )    ." FizzBuzz " 1+ ;
: fb10  ( n -- n+10 )   n n f n b f n n f b ;
: fb15  ( n -- n+15 )   fb10 n f n n fb ;
: fb100 ( n -- n+100 )  fb15 fb15 fb15 fb15 fb15 fb15 fb10 ;
: .fizzbuzz ( -- )      1 fb100 drop ;
```



## Fortran

In ANSI FORTRAN 77 or later use structured IF-THEN-ELSE (example uses some ISO Fortran 90 features):

```fortran
program fizzbuzz_if
   integer :: i

   do i = 1, 100
      if     (mod(i,15) == 0) then; print *, 'FizzBuzz'
      else if (mod(i,3) == 0) then; print *, 'Fizz'
      else if (mod(i,5) == 0) then; print *, 'Buzz'
      else;                         print *, i
      end if
   end do
end program fizzbuzz_if
```

This example uses If statements to print "Fizz" and "Buzz" next to each other if the number is divisible by 3 and 5 by waiting to use a line break until after the If statements.

```fortran
program FizzBuzz
implicit none
integer :: i = 1

do i = 1, 100
    if (Mod(i,3) == 0)write(*,"(A)",advance='no')  "Fizz"
    if (Mod(i,5) == 0)write(*,"(A)",advance='no') "Buzz"
    if (Mod(i,3) /= 0 .and. Mod(i,5) /=0 )write(*,"(I3)",advance='no') i
    print *, ""
end do
end program FizzBuzz

```

In ISO Fortran 90 or later use SELECT-CASE statement:

```fortran
program fizzbuzz_select
    integer :: i

    do i = 1, 100
       select case (mod(i,15))
          case 0;        print *, 'FizzBuzz'
          case 3,6,9,12; print *, 'Fizz'
          case 5,10;     print *, 'Buzz'
          case default;  print *, i
       end select
    end do
 end program fizzbuzz_select
```


## FreeBASIC

See [[FizzBuzz/Basic]]


## Frege


```Frege
gen n word = cycle (take (n - 1) (repeat "") ++ [word])
pattern = zipWith (++) (gen 3 "fizz") (gen 5 "buzz")
fizzbuzz = zipWith combine pattern [1..] where
    combine word number = if null word
                             then show number
                             else word
show $ take 100 fizzbuzz
```



## Frink


```frink
for i = 1 to 100
{
   flag = false
   if i mod 3 == 0
   {
      flag = true
      print["Fizz"]
   }

   if i mod 5 == 0
   {
      flag = true
      print["Buzz"]
   }

   if flag == false
      print[i]

   println[]
}
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as short fizz, buzz, i
dim as Str15 s

for i = 1 to 100
  fizz = (i mod 3 )
  buzz = (i mod 5 )
   if (i)
      if fizz + buzz == 0 then print i; ".", "FizzBuzz" : exit if
      if fizz == 0 then print i; ".", "Fizz" : exit if
      if buzz == 0 then print i; ".", "Buzz" : exit if
     print i
   end if
next i


```


Output:

```txt

 1
 2
 3.             Fizz
 4
 5.             Buzz
 6.             Fizz
 7
 8
 9.             Fizz
 10.            Buzz
 11
 12.            Fizz
 13
 14
 15.            FizzBuzz
 16
 17
 18.            Fizz
 19
 20.            Buzz
 21.            Fizz
 22
 23
 24.            Fizz
 25.            Buzz
 26
 27.            Fizz
 28
 29
 30.            FizzBuzz
 31
 32
 33.            Fizz
 34
 35.            Buzz
 36.            Fizz
 37
 38
 39.            Fizz
 40.            Buzz
 41
 42.            Fizz
 43
 44
 45.            FizzBuzz
 46
 47
 48.            Fizz
 49
 50.            Buzz
 51.            Fizz
 52
 53
 54.            Fizz
 55.            Buzz
 56
 57.            Fizz
 58
 59
 60.            FizzBuzz
 61
 62
 63.            Fizz
 64
 65.            Buzz
 66.            Fizz
 67
 68
 69.            Fizz
 70.            Buzz
 71
 72.            Fizz
 73
 74
 75.            FizzBuzz
 76
 77
 78.            Fizz
 79
 80.            Buzz
 81.            Fizz
 82
 83
 84.            Fizz
 85.            Buzz
 86
 87.            Fizz
 88
 89
 90.            FizzBuzz
 91
 92
 93.            Fizz
 94
 95.            Buzz
 96.            Fizz
 97
 98
 99.            Fizz
 100.           Buzz

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=efbe83377a1eabe475d8eba13965cfde Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short
Dim sText As String

For siCount = 1 To 100
  sText = ""
  If siCount Mod 3 = 0 Then sText = "Fizz"
  If siCount Mod 5 = 0 Then sText = "Buzz"
  If siCount Mod 15 = 0 Then sText = "FizzBuzz"
  If sText Then Print sText Else Print siCount
Next

End
```

Output:

```txt

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
FizzBuzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
FizzBuzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
FizzBuzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
FizzBuzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz

```



## GAP


```gap
FizzBuzz := function()
	local i;
	for i in [1 .. 100] do
		if RemInt(i, 15) = 0 then
			Print("FizzBuzz\n");
		elif RemInt(i, 3) = 0 then
			Print("Fizz\n");
		elif RemInt(i, 5) = 0 then
			Print("Buzz\n");
		else
			Print(i, "\n");
		fi;
	od;
end;
```


## Genyris


```Genyris

@prefix u "http://www.genyris.org/lang/utilities#"

def fizzbuzz (n)
    map-left ^((3 = 'fizz') (5 = 'buzz'))
        lambda (d)
            cond
                (equal? 0 (% n d!left))
                    d!right
                else
                    ''


for n in (range 1 100)
    define fb (''(.join (fizzbuzz n)))
    u:format "%a\n"
        cond
            (equal? fb '')
                n
            else
                fb


```


## GFA Basic


<lang>
' Fizz Buzz
'
FOR i%=1 TO 100
  IF i% MOD 15=0
    PRINT "FizzBuzz"
  ELSE IF i% MOD 3=0
    PRINT "Fizz"
  ELSE IF i% MOD 5=0
    PRINT "Buzz"
  ELSE
    PRINT i%
  ENDIF
NEXT i%

```



## Go


### switch/case approach


```go
package main

import "fmt"

func main() {
    for i := 1; i <= 100; i++ {
        switch {
        case i%15==0:
            fmt.Println("FizzBuzz")
        case i%3==0:
            fmt.Println("Fizz")
        case i%5==0:
            fmt.Println("Buzz")
        default:
            fmt.Println(i)
        }
    }
}
```


### map approach


```go
package main

import "fmt"

func main() {
    for i := 1; i <= 100; i++ {
        fmt.Println(map[bool]map[bool]interface{}{
            false: {false: i, true: "Fizz"}, true: {false: "Buzz", true: "FizzBuzz"},
        }[i%5 == 0][i%3 == 0])
    }
}
```



## Golo


```golo
module FizzBuzz

augment java.lang.Integer {
	function getFizzAndOrBuzz = |this| -> match {
		when this % 15 == 0 then "FizzBuzz"
		when this % 3 == 0 then "Fizz"
		when this % 5 == 0 then "Buzz"
		otherwise this
	}
}

function main = |args| {
  foreach i in [1..101] {
	println(i: getFizzAndOrBuzz())
  }
}

```



## Gosu


```gosu
for (i in 1..100) {

    if (i % 3 == 0 && i % 5 == 0) {
        print("FizzBuzz")
        continue
    }

    if (i % 3 == 0) {
        print("Fizz")
        continue
    }

    if (i % 5 == 0) {
        print("Buzz")
        continue
    }

    // default
    print(i)

}
```

One liner version (I added new lines to better readability but when you omit them it's one liner):

```gosu
// note that compiler reports error (I don't know why) but still it's working
for (i in 1..100) {
    print(i % 5 == 0 ? i % 3 == 0 ? "FizzBuzz" : "Buzz" : i % 3 == 0 ? "Fizz" : i)
}
```



## Groovy


```groovy
1.upto(100) { i -> println "${i % 3 ? '' : 'Fizz'}${i % 5 ? '' : 'Buzz'}" ?: i }
```


=={{header|GW-BASIC}}==
See [[FizzBuzz/Basic]]


## Haskell

Variant directly implementing the specification:

```haskell
fizzbuzz :: Int -> String
fizzbuzz x
  | f 15 = "FizzBuzz"
  | f 3 = "Fizz"
  | f 5 = "Buzz"
  | otherwise = show x
  where
    f = (0 ==) . rem x

main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [1 .. 100]
```



```haskell
fizzbuzz :: Int -> String
fizzbuzz n =
  '\n' :
  if null (fizz ++ buzz)
    then show n
    else fizz ++ buzz
  where
    fizz =
      if mod n 3 == 0
        then "Fizz"
        else ""
    buzz =
      if mod n 5 == 0
        then "Buzz"
        else ""

main :: IO ()
main = putStr $ concatMap fizzbuzz [1 .. 100]
```

Does not perform the mod 15 step, extesible to arbitrary addtional tests, ex: [bar| n `mod` 7 == 0].

```haskell
main = mapM_ (putStrLn . fizzbuzz) [1..100]

fizzbuzz n =
    show n <|> [fizz| n `mod` 3 == 0] ++
               [buzz| n `mod` 5 == 0]

-- A simple default choice operator.
-- Defaults if both fizz and buzz fail, concats if any succeed.
infixr 0 <|>
d <|> [] = d
_ <|> x = concat x

fizz = "Fizz"
buzz = "Buzz"
```

Alternate implementation using lazy infinite lists and avoiding use of "mod":

```haskell
main = mapM_ putStrLn $ take 100 $ zipWith show_number_or_fizzbuzz [1..] fizz_buzz_list

show_number_or_fizzbuzz x y = if null y then show x else y

fizz_buzz_list = zipWith (++) (cycle ["","","Fizz"]) (cycle ["","","","","Buzz"])
```


Or in terms (still without '''mod''' or '''rem''') of a single '''zipWith3''':

```haskell
import Data.List (zipWith3)
import Data.Bool (bool)

fizzBuzz :: [String]
fizzBuzz =
  zipWith3
    (\f b n ->
        let fb = f ++ b
        in bool fb n (null fb))
    (cycle $ replicate 2 [] ++ ["fizz"])
    (cycle $ replicate 4 [] ++ ["buzz"])
    (show <$> [1 ..])

main :: IO ()
main = mapM_ putStrLn $ take 100 fizzBuzz
```


or using an applicative test:


```haskell
import Data.Bool (bool)

fizzBuzz :: [String]
fizzBuzz =
  let fb n k = cycle (replicate (pred n) [] ++ [k])
  in zipWith
       (flip . bool <*> null)
       (zipWith (++) (fb 3 "fizz") (fb 5 "buzz"))
       (show <$> [1 ..])

main :: IO ()
main = mapM_ putStrLn $ take 100 fizzBuzz
```


Using heavy artillery (needs the mtl package):

```haskell

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

main = putStr $ execWriter $ mapM_ (flip execStateT True . fizzbuzz) [1..100]

fizzbuzz :: Int -> StateT Bool (Writer String) ()
fizzbuzz x = do
 when (x `mod` 3 == 0) $ tell "Fizz" >> put False
 when (x `mod` 5 == 0) $ tell "Buzz" >> put False
 get >>= (flip when $ tell $ show x)
 tell "\n"
```

Using guards plus where.

```haskell
fizzBuzz :: (Integral a) => a -> String
fizzBuzz i
  | fizz && buzz = "FizzBuzz"
  | fizz         = "Fizz"
  | buzz         = "Buzz"
  | otherwise    = show i
  where fizz = i `mod` 3 == 0
        buzz = i `mod` 5 == 0

main = mapM_ (putStrLn . fizzBuzz) [1..100]
```


An elegant solution exploiting monoidal and applicative properties of functions:

```haskell
import Data.Monoid

fizzbuzz = max
       <$> show
       <*> "fizz" `when` divisibleBy 3
       <>  "buzz" `when` divisibleBy 5
       <>  "quxx" `when` divisibleBy 7
  where
    when m p x = if p x then m else mempty
    divisibleBy n x = x `mod` n == 0

main = mapM_ (putStrLn . fizzbuzz) [1..100]
```


And pattern matching approach:

```haskell
fizzbuzz n = case (rem n 3, rem n 5) of
               (0, 0) -> "FizzBuzz"
               (0, _) -> "Fizz"
               (_, 0) -> "Buzz"
               (_, _) -> show n

main = mapM_ (putStrLn . fizzbuzz) [1..100]
```



## hexiscript


```hexiscript
for let i 1; i <= 100; i++
  if   i % 3 = 0 && i % 5 = 0; println "FizzBuzz"
  elif i % 3 = 0; println "Fizz"
  elif i % 5 = 0; println "Buzz"
  else println i; endif
endfor
```



## HicEst


```hicest
DO i = 1, 100
  IF(     MOD(i, 15) == 0 ) THEN
    WRITE() "FizzBuzz"
  ELSEIF( MOD(i, 5) == 0 ) THEN
    WRITE() "Buzz"
  ELSEIF( MOD(i, 3) == 0 ) THEN
    WRITE() "Fizz"
  ELSE
    WRITE() i
  ENDIF
ENDDO
```

Alternatively:

```hicest
CHARACTER string*8

DO i = 1, 100
  string = " "
  IF( MOD(i, 3) == 0 ) string = "Fizz"
  IF( MOD(i, 5) == 0 ) string = TRIM(string) // "Buzz"
  IF( string == " ") WRITE(Text=string) i
  WRITE() string
ENDDO
```



## HolyC


```holyc
U8 i;
for (i = 1; i <= 100; i++) {
  if (!(i % 15))
    Print("FizzBuzz");
  else if (!(i % 3))
    Print("Fizz");
  else if (!(i % 5))
    Print("Buzz");
  else
    Print("%d", i);
  Print("\n");
}
```



## Hoon


```Hoon
:-  %say
|=  [^ ~ ~]
  :-  %noun
  %+  turn   (gulf [1 101])
  |=  a=@
    =+  q=[=(0 (mod a 3)) =(0 (mod a 5))]
    ?+  q  <a>
      [& &]  "FizzBuzz"
      [& |]  "Fizz"
      [| &]  "Buzz"
    ==
```



## Huginn


```huginn
import Algorithms as algo;

main( argv_ ) {
	if ( size( argv_ ) < 2 ) {
		throw Exception( "usage: fizzbuzz {NUM}" );
	}
	top = integer( argv_[1] );
	for ( i : algo.range( 1, top + 1 ) ) {
		by3 = ( i % 3 ) == 0;
		by5 = ( i % 5 ) == 0;
		if ( by3 ) {
			print( "fizz" );
		}
		if ( by5 ) {
			print( "buzz" );
		}
		if ( ! ( by3 || by5 ) ) {
			print( i );
		}
		print( "\n" );
	}
	return ( 0 );
}
```



## Hy



```lisp
(for [i (range 1 101)] (print (cond
  [(not (% i 15)) "FizzBuzz"]
  [(not (% i  5)) "Buzz"]
  [(not (% i  3)) "Fizz"]
  [True           i])))
```



## i


```i
software {
	for each 1 to 100
		if i % 15 = 0
			print("FizzBuzz")
		else if i % 3 = 0
			print("Fizz")
		else if i % 5 = 0
			print("Buzz")
		else
			print(i)
		end
	end
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
# straight-forward modulo tester
procedure main()
    every i := 1 to 100 do
        if i % 15 = 0 then
            write("FizzBuzz")
        else if i % 5 = 0 then
            write("Buzz")
        else if i % 3 = 0 then
            write("Fizz")
        else
            write(i)
end
```


```icon
# idiomatic modulo tester, 1st alternative
procedure main()
    every i := 1 to 100 do
        write((i % 15 = 0 & "FizzBuzz") | (i % 5 = 0 & "Buzz") | (i % 3 = 0 & "Fizz") | i)
end
```


```icon
# idiomatic modulo tester, 2nd alternative
procedure main()
    every i := 1 to 100 do
        write(case 0 of {
                 i % 15 : "FizzBuzz"
                 i % 5  : "Buzz"
                 i % 3  : "Fizz"
                 default: i
        })
end
```


```icon
# straight-forward buffer builder
procedure main()
    every i := 1 to 100 do {
        s := ""
        if i % 3 = 0 then
            s ||:= "Fizz"
        if i % 5 = 0 then
            s ||:= "Buzz"
        if s == "" then
            s := i
        write(s)
    }
end
```


```icon
# idiomatic buffer builder, 1st alternative
procedure main()
    every i := 1 to 100 do
        write("" ~== (if i % 3 = 0 then "Fizz" else "") || (if i % 5 == 0 then "Buzz" else "") | i)
end
```


```icon
# idiomatic buffer builder, 2nd alternative
procedure main()
    every i := 1 to 100 do {
        s   := if i%3 = 0 then "Fizz" else ""
        s ||:= if i%5 = 0 then "Buzz"
        write(("" ~= s) | i)
    }
end
```



## Idris


```idris
partial
fizzBuzz : Nat -> String
fizzBuzz n = if (n `modNat` 15) == 0 then "FizzBuzz"
             else if (n `modNat` 3) == 0 then "Fizz"
             else if (n `modNat` 5)  == 0 then "Buzz"
             else show n

main : IO ()
main = sequence_ $ map (putStrLn . fizzBuzz) [1..100]
```



## Inform 6


```inform6
[ Main i;
  for(i = 1: i <= 100: i++)
  {
    if(i % 3 == 0) print "Fizz";
    if(i % 5 == 0) print "Buzz";
    if(i % 3 ~= 0 && i % 5 ~= 0) print i;

    print "^";
  }
];
```



## Inform 7


(Does not work in the current version of Inform 7)


```inform7
Home is a room.

When play begins:
	repeat with N running from 1 to 100:
		let printed be false;
		if the remainder after dividing N by 3 is 0:
			say "Fizz";
			now printed is true;
		if the remainder after dividing N by 5 is 0:
			say "Buzz";
			now printed is true;
		if printed is false, say N;
		say ".";
	end the story.
```


(Version which is less "programmy", and more in the natural English style of interactive fiction.)


```inform7
The space is a room.  An item is a kind of thing.  In the space are 100 items.

To say the name:
	let the count be the number of items carried by the player;
	say "[if the count is the count to the nearest 15]fizzbuzz.[otherwise if the count is the count to the nearest 3]fizz.[otherwise if the count is the count to the nearest 5]buzz.[otherwise][the count in words].".

To count:
	if an item is in the space
	begin;
		let the next one be a random item in the space; silently try taking the next one;
		say "[the name]" in sentence case;
		count;
		end the story;
	end if.

When play begins: count.  Use no scoring.
```



## Io

Here's one way to do it:

```io
for(a,1,100,
   if(a % 15 == 0) then(
      "FizzBuzz" println
   ) elseif(a % 3 == 0) then(
      "Fizz" println
   ) elseif(a % 5 == 0) then(
      "Buzz" println
   ) else (
      a println
   )
)
```

And here's a port of the Ruby version, which I personally prefer:

```io
a := 0; b := 0
for(n, 1, 100,
    if(a = (n % 3) == 0, "Fizz" print);
    if(b = (n % 5) == 0, "Buzz" print);
    if(a not and b not, n print);
    "\n" print
)
```

And here is another more idiomatic version:

```Io
for (n, 1, 100,
    fb := list (
        if (n % 3 == 0, "Fizz"),
        if (n % 5 == 0, "Buzz")) select (isTrue)

    if (fb isEmpty, n, fb join) println
)
```



## Ioke


```ioke
(1..100) each(x,
  cond(
    (x % 15) zero?, "FizzBuzz" println,
    (x % 3) zero?, "Fizz" println,
    (x % 5) zero?, "Buzz" println
  )
)
```



## Iptscrae


```iptscrae
; FizzBuzz in Iptscrae
1 a =
{
   "" b =
   { "fizz" b &= } a 3 % 0 == IF
   { "buzz" b &= } a 5 % 0 == IF
   { a ITOA LOGMSG } { b LOGMSG } b STRLEN 0 == IFELSE
   a ++
}
{ a 100 <= } WHILE
```



## J

Solution _1: Using agenda (@.) as a switch:

```j

   classify =: +/@(1 2 * 0 = 3 5&|~)
   (":@]`('Fizz'"_)`('Buzz'"_)`('FizzBuzz'"_) @. classify "0)  >:i.100

```

Solution 0

```j>
 }. (<'FizzBuzz') (I.0=15|n)} (<'Buzz') (I.0=5|n)} (<'Fizz') (I.0=3|n)} ":&.> n=: i.101
```

Solution 1

```j
Fizz=: 'Fizz' #~ 0 = 3&|
Buzz=: 'Buzz' #~ 0 = 5&|
FizzBuzz=: ": [^:('' -: ]) Fizz,Buzz

FizzBuzz"0 >: i.100
```

Solution 2 (has taste of table-driven template programming)

```j
CRT0=: 2 : ' (, 0 = +./)@(0 = m | ]) ;@# n , <@": '
NB. Rather (, 0 = +./) than (, +:/) because designed for
NB. 3 5 7 CRT0 (;:'Chinese Remainder Period') "0 >: i. */3 5 7
FizzBuzz=: 3 5 CRT0 (;:'Fizz Buzz')

FizzBuzz"0 >: i.100
```

Solution 3 (depends on an obsolete feature of @ in f`g`h@p)

```j
'`f   b   fb'  =: ('Fizz'"_) ` ('Buzz'"_) ` (f , b)
'`cm3 cm5 cm15'=: (3&|)      ` (5&|)      ` (15&|)  (0&=@)
FizzBuzz=: ": ` f @. cm3 ` b @. cm5 ` fb @. cm15  NB. also:
FizzBuzz=: ": ` f @. cm3 ` b @. cm5 ` (f,b) @. (cm3 *. cm5)

FizzBuzz"0 >: i.100
```


Solution 4 (relatively concise):


```J
   ;:inv}.(":&.> [^:(0 = #@])&.> [: ,&.>/ (;:'Fizz Buzz') #&.>~ 0 = 3 5 |/ ])i.101
1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz 41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz Fizz 52 53 Fizz Buzz 56 Fizz 58 59 FizzBuzz 61 62 Fiz...
```


Here's some intermediate results for subexpressions of this last version (but with a shorter list of numbers):


```J
   i.10
0 1 2 3 4 5 6 7 8 9
   (3 5 |/ ])i.10
0 1 2 0 1 2 0 1 2 0
0 1 2 3 4 0 1 2 3 4
   (0=3 5 |/ ])i.10
1 0 0 1 0 0 1 0 0 1
1 0 0 0 0 1 0 0 0 0
   (;:'Fizz Buzz')
┌────┬────┐
│Fizz│Buzz│
└────┴────┘
   ((;:'Fizz Buzz') #&.>~0=3 5 |/ ])i.10
┌────┬┬┬────┬┬────┬────┬┬┬────┐
│Fizz│││Fizz││    │Fizz│││Fizz│
├────┼┼┼────┼┼────┼────┼┼┼────┤
│Buzz│││    ││Buzz│    │││    │
└────┴┴┴────┴┴────┴────┴┴┴────┘
   ([: ,&.>/ (;:'Fizz Buzz') #&.>~0=3 5 |/ ])i.10
┌────────┬┬┬────┬┬────┬────┬┬┬────┐
│FizzBuzz│││Fizz││Buzz│Fizz│││Fizz│
└────────┴┴┴────┴┴────┴────┴┴┴────┘
   (":&.>)i.10
┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
│0│1│2│3│4│5│6│7│8│9│
└─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
   (":&.> [^:(0 = #@])&.> [: ,&.>/ (;:'Fizz Buzz') #&.>~0=3 5 |/ ])i.10
┌────────┬─┬─┬────┬─┬────┬────┬─┬─┬────┐
│FizzBuzz│1│2│Fizz│4│Buzz│Fizz│7│8│Fizz│
└────────┴─┴─┴────┴─┴────┴────┴─┴─┴────┘
   }.(":&.> [^:(0 = #@])&.> [: ,&.>/ (;:'Fizz Buzz') #&.>~0=3 5 |/ ])i.10
┌─┬─┬────┬─┬────┬────┬─┬─┬────┐
│1│2│Fizz│4│Buzz│Fizz│7│8│Fizz│
└─┴─┴────┴─┴────┴────┴─┴─┴────┘
   ;:inv}.(":&.> [^:(0 = #@])&.> [: ,&.>/ (;:'Fizz Buzz') #&.>~0=3 5 |/ ])i.10
1 2 Fizz 4 Buzz Fizz 7 8 Fizz
```



## Java



```java

class FizzBuzz {

    public static void main(String[] args) {

        for (int i = 1; i < 101; i++) {
            if ((i % 3 == 0) && (i % 5 == 0)) {
                System.out.print("'fizz buzz', ");
            } else if (i % 3 == 0) {
                System.out.print("'fizz', ");
            } else if (i % 5 == 0) {
                System.out.print("'buzz', ");
            } else {
                System.out.printf("%d, ", i);
            }
        }
    }
}

```



## JavaScript



### ES5



```javascript
var fizzBuzz = function () {
  var i, output;
  for (i = 1; i < 101; i += 1) {
    output = '';
    if (!(i % 3)) { output += 'Fizz'; }
    if (!(i % 5)) { output += 'Buzz'; }
    console.log(output || i);//empty string is false, so we short-circuit
  }
};
```


Alternate version with ghetto pattern matching

```javascript
for (var i = 1; i <= 100; i++) {
  console.log({
    truefalse: 'Fizz',
    falsetrue: 'Buzz',
    truetrue: 'FizzBuzz'
  }[(i%3==0) + '' + (i%5==0)] || i)
}
```


Or very tersely:

```javascript
for(i=1;i<101;i++)console.log((x=(i%3?'':'Fizz')+(i%5?'':'Buzz'))?x:i);
```


Or with even less characters:

```javascript
for(i=1;i<101;i++)console.log((i%3?'':'Fizz')+(i%5?'':'Buzz')||i)
```


Or, in a more functional style, without mutations

```javascript
(function rng(i) {
    return i ? rng(i - 1).concat(i) : []
})(100).map(
    function (n) {
        return n % 3 ? (
            n % 5 ? n : "Buzz"
        ) : (
            n % 5 ? "Fizz" : "FizzBuzz"
        )
    }
).join(' ')
```



### ES6


```JavaScript
(() => {

    // FIZZBUZZ --------------------------------------------------------------

    // fizzBuzz :: Int -> String
    const fizzBuzz = n =>
        caseOf(n, [
            [x => x % 15 === 0, "FizzBuzz"],
            [x => x % 3 === 0, "Fizz"],
            [x => x % 5 === 0, "Buzz"]
        ], n.toString());

    // GENERIC FUNCTIONS -----------------------------------------------------

    // caseOf :: a -> [(a -> Bool, b)] -> b -> b
    const caseOf = (e, pvs, otherwise) =>
        pvs.reduce((a, [p, v]) =>
            a !== otherwise ? a : (p(e) ? v : a), otherwise);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // TEST ------------------------------------------------------------------
    return unlines(
        map(fizzBuzz, enumFromTo(1, 100))
    );
})();
```


A functional implementation:


```Javascript
const factors = [[3, 'Fizz'], [5, 'Buzz']]
const fizzBuzz = num => factors.map(([factor,text]) => (num % factor)?'':text).join('') || num
const range1 = x => [...Array(x+1).keys()].slice(1)
const outputs = range1(100).map(fizzBuzz)

console.log(outputs.join('\n'))
```



Or composing generic functions, and without use of modulo (or other) numeric tests:
{{Trans|Python}}
{{Trans|Haskell}}

```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {

        // FIZZBUZZ ---------------------------------------

        // fizzBuzz :: Generator [String]
        const fizzBuzz = () => {
            const fb = n => k => cycle(
                replicate(n - 1)('').concat(k)
            );
            return zipWith(
                liftA2(flip)(bool)(isNull)
            )(
                zipWith(append)(fb(3)('fizz'))(fb(5)('buzz'))
            )(fmap(str)(enumFrom(1)));
        };

        // TEST -------------------------------------------
        console.log(
            unlines(
                take(100)(
                    fizzBuzz()
                )
            )
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = xs => ys => xs.concat(ys);

    // bool :: a -> a -> Bool -> a
    const bool = f => t => p =>
        p ? t : f;

    // cycle :: [a] -> Generator [a]
    function* cycle(xs) {
        const lng = xs.length;
        let i = 0;
        while (true) {
            yield(xs[i])
            i = (1 + i) % lng;
        }
    }

    // enumFrom :: Int => Int -> [Int]
    function* enumFrom(x) {
        let v = x;
        while (true) {
            yield v;
            v = 1 + v;
        }
    }

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        x => y => f(y)(x);

    // fmap <$> :: (a -> b) -> Gen [a] -> Gen [b]
    const fmap = f =>
        function*(gen) {
            let v = take(1)(gen);
            while (0 < v.length) {
                yield(f(v[0]))
                v = take(1)(gen)
            }
        };

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // isNull :: [a] -> Bool
    // isNull :: String -> Bool
    const isNull = xs =>
        1 > xs.length;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // liftA2 :: (a0 -> b -> c) -> (a -> a0) -> (a -> b) -> a -> c
    const liftA2 = op => f => g =>
        // Lift a binary function to a composition
        // over two other functions.
        // liftA2 (*) (+ 2) (+ 3) 7 == 90
        x => op(f(x))(g(x));

    // replicate :: Int -> a -> [a]
    const replicate = n => x =>
        Array.from({
            length: n
        }, () => x);

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // str :: a -> String
    const str = x => x.toString();

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

    // The first argument is a sample of the type
    // allowing the function to make the right mapping

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => {
        const lng = length(xs);
        return (0 < lng) ? (
            lng < Infinity ? (
                Just(Tuple(xs[0])(xs.slice(1))) // Finite list
            ) : (() => {
                const nxt = take(1)(xs);
                return 0 < nxt.length ? (
                    Just(Tuple(nxt[0])(xs))
                ) : Nothing();
            })() // Lazy generator
        ) : Nothing();
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) Gen [a] -> Gen [b] -> Gen [c]
    const zipWith = f => ga => gb => {
        function* go(ma, mb) {
            let
                a = ma,
                b = mb;
            while (!a.Nothing && !b.Nothing) {
                let
                    ta = a.Just,
                    tb = b.Just
                yield(f(fst(ta))(fst(tb)));
                a = uncons(snd(ta));
                b = uncons(snd(tb));
            }
        }
        return go(uncons(ga), uncons(gb));
    };

    // MAIN ---
    return main();
})();
```



## Joy

The following program first defines a function "one", which handles the Fizz / Buzz logic, and then loops from 1 to 100 mapping the function onto each number, and printing ("put") the output.

```Joy
DEFINE one == [[[dup 15 rem 0 =] "FizzBuzz"] [[dup 3 rem 0 =] "Fizz"] [[dup 5 rem 0 =] "Buzz"] [dup]] cond.
1 [100 <=] [dup one put succ] while.
```



## jq


```jq
range(1;101)
  | if   . % 15 == 0 then "FizzBuzz"
    elif . % 5  == 0 then "Buzz"
    elif . % 3  == 0 then "Fizz"
    else .
    end
```


Another solution:


```jq
range(100) + 1 | [(
	(select(. % 3 == 0) | "Fizz"),
	(select(. % 5 == 0) | "Buzz")
) // tostring] | join("")

```



## Julia

{{works with|Julia|0.6}}

One basic solution:

```julia
for i in 1:100
    if i % 15 == 0
        println("FizzBuzz")
    elseif i % 3 == 0
        println("Fizz")
    elseif i % 5 == 0
        println("Buzz")
    else
        println(i)
    end
end
```


Another possible solution:

```julia
collect(i % 15 == 0 ? "FizzBuzz" : i % 5 == 0 ? "Buzz" : i % 3 == 0 ? "Fizz" : i for i in 1:100) |> println
```


A 3rd possible solution:

```julia
fb(i::Integer) = "Fizz" ^ (i % 3 == 0) * "Buzz" ^ (i % 5 == 0) * dec(i) ^ (i % 3 != 0 && i % 5 != 0)
for i in 1:100 println(fb(i)) end
```


A 4th one:

```julia
println.(map(fb, 1:100))
```


A fifth (DRY, Don't Repeat Yourself) possible solution:

```julia
for i in 1:100
    msg = "Fizz" ^ (i % 3 == 0) * "Buzz" ^ (i % 5 == 0)
    println(isempty(msg) ? i : msg)
end
```



## K


### Solution 0


```k
`0:\:{:[0=#a:{,/$(:[0=x!3;"Fizz"];:[0=x!5;"Buzz"])}@x;$x;a]}'1_!101
```



### Solution 1


```k

  fizzbuzz:{:[0=x!15;`0:,"FizzBuzz";0=x!3;`0:,"Fizz";0=x!5;`0:,"Buzz";`0:,$x]}
  fizzbuzz' 1+!100

```



### Solution 2


```k
fizzbuzz:{
  v:1+!x
  i:(&0=)'v!/:3 5 15
  r:@[v;i 0;{"Fizz"}]
  r:@[r;i 1;{"Buzz"}]
  @[r;i 2;{"FizzBuzz"}]}

`0:$fizzbuzz 100
```



### Solution 3

For kona:
```k
{,/$(s;x)@~#s:`Fizz`Buzz@&~x!'3 5}'1+!30
```

For k6 and oK, change <code>x!'3 5</code> to <code>3 5!'x</code>.


## Kamailio Script

To run it, send a SIP message to the server and FizzBuzz will appear in the logs.

This will only work up to 100 because Kamailio terminates all while loops after 100 iterations.

```kamailio
# FizzBuzz
log_stderror=yes
loadmodule "pv"
loadmodule "xlog"

route {
    $var(i) = 1;
    while ($var(i) <= 1000) {
        if ($var(i) mod 15 == 0) {
            xlog("FizzBuzz\n");
        } else if ($var(i) mod 5 == 0) {
            xlog("Buzz\n");
        } else if ($var(i) mod 3 == 0) {
            xlog("Fizz\n");
        } else {
            xlog("$var(i)\n");
        }
        $var(i) = $var(i) + 1;
    }
}
```



## Kaya


```kaya
// fizzbuzz in Kaya
program fizzbuzz;

Void fizzbuzz(Int size) {
    for i in [1..size] {
        if (i % 15 == 0) {
            putStrLn("FizzBuzz");
        } else if (i % 5 == 0) {
            putStrLn("Buzz");
        } else if (i % 3 == 0) {
            putStrLn("Fizz");
        } else {
            putStrLn( string(i) );
        }
    }
}

Void main() {
    fizzbuzz(100);
}
```



## Klong


```k

{:[0=x!15;:FizzBuzz:|0=x!5;:Buzz:|0=x!3;:Fizz;x]}'1+!100

```



## Kotlin



### Imperative solution


```scala
fun fizzBuzz() {
    for (i in 1..100) {
        when {
            i % 15 == 0 -> println("FizzBuzz")
            i % 3 == 0 -> println("Fizz")
            i % 5 == 0 -> println("Buzz")
            else -> println(i)
        }
    }
}
```



### Functional solution 1


```scala
fun fizzBuzz1() {
    fun fizzBuzz(x: Int) = if (x % 15 == 0) "FizzBuzz" else x.toString()
    fun fizz(x: Any) = if (x is Int && x % 3 == 0) "Buzz" else x
    fun buzz(x: Any) = if (x is Int && x.toInt() % 5 == 0) "Fizz" else x

    (1..100).map { fizzBuzz(it) }.map { fizz(it) }.map { buzz(it) }.forEach { println(it) }
}
```



### Functional solution 2


```scala
fun fizzBuzz2() {
    fun fizz(x: Pair<Int, StringBuilder>) = if(x.first % 3 == 0) x.apply { second.append("Fizz") } else x
    fun buzz(x: Pair<Int, StringBuilder>) = if(x.first % 5 == 0) x.apply { second.append("Buzz") } else x
    fun none(x: Pair<Int, StringBuilder>) = if(x.second.isBlank()) x.second.apply { append(x.first) } else x.second

    (1..100).map { Pair(it, StringBuilder()) }
            .map { fizz(it) }
            .map { buzz(it) }
            .map { none(it) }
            .forEach { println(it) }
}
```



### Short version with map


```scala

fun fizzBuzz() {
    println((1..100).map{i->mapOf(0 to i,i%3 to "Fizz",i%5 to "Buzz",i%15 to "FizzBuzz")[0]})
}

```



## LabVIEW

{{VI snippet}}<br/>
[[file:LabVIEW_FizzBuzz.png]]


## Lasso


```lasso
with i in generateSeries(1, 100)
select ((#i % 3 == 0 ? 'Fizz' | '') + (#i % 5 == 0 ? 'Buzz' | '') || #i)
```



## LaTeX

{{libheader|ifthen}}
{{libheader|intcalc}}
This version uses the ifthen and intcalc packages. There sure are more native solutions including solutions in plain TeX, but for me this is a readable and comprehensible one.

```LaTeX
\documentclass{minimal}
\usepackage{ifthen}
\usepackage{intcalc}

\newcounter{mycount}
\newboolean{fizzOrBuzz}

\newcommand\fizzBuzz[1]{%
\setcounter{mycount}{1}\whiledo{\value{mycount}<#1}
    {
	\setboolean{fizzOrBuzz}{false}
	\ifthenelse{\equal{\intcalcMod{\themycount}{3}}{0}}{\setboolean{fizzOrBuzz}{true}Fizz}{}
	\ifthenelse{\equal{\intcalcMod{\themycount}{5}}{0}}{\setboolean{fizzOrBuzz}{true}Buzz}{}
	\ifthenelse{\boolean{fizzOrBuzz}}{}{\themycount}
	\stepcounter{mycount}
	\\
    }
}

\begin{document}
    \fizzBuzz{101}
\end{document}
```



## Liberty BASIC

See [[FizzBuzz/Basic]]


## LIL


```tcl
# fizzbuzz in LIL
for {set i 1} {$i <= 100} {inc i} {
    set show ""
    if {[expr $i % 3 == 0]} {set show "Fizz"}
    if {[expr $i % 5 == 0]} {set show $show"Buzz"}
    if {[expr [length $show] == 0]} {set show $i}
    print $show
}
```


{{out}}

```txt
prompt$ lil fizzbuzz.lil | sed -n '1,16p'
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
```




## LiveCode


```LiveCode
repeat with i = 1 to 100
    switch
        case i mod 15 = 0
            put "FizzBuzz" & cr after fizzbuzz
            break
        case i mod 5 = 0
            put "Buzz" & cr after fizzbuzz
            break
        case i mod 3 = 0
            put "Fizz" & cr after fizzbuzz
            break
        default
            put i & cr after fizzbuzz
    end switch
end repeat
put fizzbuzz
```



## LiveScript

See: http://livescript.net/blog/fizzbuzzbazz.html

```LiveScript
[1 to 100] map -> [k + \zz for k, v of {Fi: 3, Bu: 5} | it % v < 1] * '' || it
```



## LLVM


```llvm
; ModuleID = 'fizzbuzz.c'
; source_filename = "fizzbuzz.c"
; target datalayout = "e-m:w-i64:64-f80:128-n8:16:32:64-S128"
; target triple = "x86_64-pc-windows-msvc19.21.27702"

; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

$"\01??_C@_09NODAFEIA@FizzBuzz?6?$AA@" = comdat any
$"\01??_C@_05KEBFOHOF@Fizz?6?$AA@" = comdat any
$"\01??_C@_05JKJENPHA@Buzz?6?$AA@" = comdat any
$"\01??_C@_03PMGGPEJJ@?$CFd?6?$AA@" = comdat any

;--- String constant defintions
@"\01??_C@_09NODAFEIA@FizzBuzz?6?$AA@" = linkonce_odr unnamed_addr constant [10 x i8] c"FizzBuzz\0A\00", comdat, align 1
@"\01??_C@_05KEBFOHOF@Fizz?6?$AA@" = linkonce_odr unnamed_addr constant [6 x i8] c"Fizz\0A\00", comdat, align 1
@"\01??_C@_05JKJENPHA@Buzz?6?$AA@" = linkonce_odr unnamed_addr constant [6 x i8] c"Buzz\0A\00", comdat, align 1
@"\01??_C@_03PMGGPEJJ@?$CFd?6?$AA@" = linkonce_odr unnamed_addr constant [4 x i8] c"%d\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 1, i32* %1, align 4
;--- It does not seem like this branch can be removed
  br label %loop

;--- while (i <= 100)
loop:
  %2 = load i32, i32* %1, align 4
  %3 = icmp sle i32 %2, 100
  br i1 %3, label %divisible_15, label %finished

;--- if (i % 15 == 0)
divisible_15:
  %4 = load i32, i32* %1, align 4
  %5 = srem i32 %4, 15
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %print_fizzbuzz, label %divisible_3

;--- Print 'FizzBuzz'
print_fizzbuzz:
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @"\01??_C@_09NODAFEIA@FizzBuzz?6?$AA@", i32 0, i32 0))
  br label %next

;--- if (i % 3 == 0)
divisible_3:
  %8 = load i32, i32* %1, align 4
  %9 = srem i32 %8, 3
  %10 = icmp eq i32 %9, 0
  br i1 %10, label %print_fizz, label %divisible_5

;--- Print 'Fizz'
print_fizz:
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @"\01??_C@_05KEBFOHOF@Fizz?6?$AA@", i32 0, i32 0))
  br label %next

;--- if (i % 5 == 0)
divisible_5:
  %12 = load i32, i32* %1, align 4
  %13 = srem i32 %12, 5
  %14 = icmp eq i32 %13, 0
  br i1 %14, label %print_buzz, label %print_number

;--- Print 'Buzz'
print_buzz:
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @"\01??_C@_05JKJENPHA@Buzz?6?$AA@", i32 0, i32 0))
  br label %next

;--- Print the number
print_number:
  %16 = load i32, i32* %1, align 4
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"\01??_C@_03PMGGPEJJ@?$CFd?6?$AA@", i32 0, i32 0), i32 %16)
;--- It does not seem like this branch can be removed
  br label %next

;--- i = i + 1
next:
  %18 = load i32, i32* %1, align 4
  %19 = add nsw i32 %18, 1
  store i32 %19, i32* %1, align 4
  br label %loop

;--- exit main
finished:
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
```



## Lobster


```Lobster
include "std.lobster"

forbias(100, 1) i:
    fb := (i % 3 == 0 and "fizz" or "") +
          (i % 5 == 0 and "buzz" or "")
    print fb.length and fb or "" + i
```



## Logo


```logo
to fizzbuzz :n
  output cond [ [[equal? 0 modulo :n 15] "FizzBuzz]
                [[equal? 0 modulo :n  5] "Buzz]
                [[equal? 0 modulo :n  3] "Fizz]
                [else :n] ]
end

repeat 100 [print fizzbuzz #]
```

"cond" was undefined in Joshua Bell's online interpreter. So here is a version that works there. It also works in UCB logo by using # instead of "repcount". This version also factors away modulo 15:

```logo
to fizzbuzz :n
 make "c "
  if equal? 0 modulo :n 5 [make "c "Buzz]
  if equal? 0 modulo :n 3 [make "c word "Fizz :c]
 output ifelse equal? :c " [:n] [:c]
end

repeat 100 [print fizzbuzz repcount]
```

Lhogho can use the above code, except that 'modulo' must be replaced with 'remainder'.


## LOLCODE

See [[FizzBuzz/EsoLang]]


## LSE


```LSE
1* FIZZBUZZ en L.S.E.
10 CHAINE FB
20 FAIRE 45 POUR I_1 JUSQUA 100
30 FB_SI &MOD(I,3)=0 ALORS SI &MOD(I,5)=0 ALORS 'FIZZBUZZ' SINON 'FIZZ' SINON SI &MOD(I,5)=0 ALORS 'BUZZ' SINON ''
40 AFFICHER[U,/] SI FB='' ALORS I SINON FB
45*FIN BOUCLE
50 TERMINER
100 PROCEDURE &MOD(A,B) LOCAL A,B
110 RESULTAT A-B*ENT(A/B)
```



## Lua


### If/else Ladder


```Lua
for i = 1, 100 do
	if i % 15 == 0 then
		print("FizzBuzz")
	elseif i % 3 == 0 then
		print("Fizz")
	elseif i % 5 == 0 then
		print("Buzz")
	else
		print(i)
	end
end
```


### Concatenation


```Lua
for i = 1, 100 do
	output = ""
	if i % 3 == 0 then
		output = output.."Fizz"
	end
	if i % 5 == 0 then
		output = output.."Buzz"
	end
	if(output == "") then
		output = i
	end
	print(output)
end
```



### Quasi bit field


```Lua
word = {"Fizz", "Buzz", "FizzBuzz"}

for i = 1, 100 do
        print(word[(i % 3 == 0 and 1 or 0) + (i % 5 == 0 and 2 or 0)] or i)
end
```



### Lookup table


```Lua
local t = {
        [0]  = "FizzBuzz",
        [3]  = "Fizz",
        [5]  = "Buzz",
        [6]  = "Fizz",
        [9]  = "Fizz",
        [10] = "Buzz",
        [12] = "Fizz"
}

for i = 1, 100 do
        print(t[i%15] or i)
end
```



## Luck


```Luck
for i in range(1,101) do (
   if i%15 == 0 then print("FizzBuzz")
   else if i%3 == 0 then print("Fizz")
   else if i%5 == 0 then print("Buzz")
   else print(i)
)
```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')

for(`x',1,100,1,
   `ifelse(eval(x%15==0),1,FizzBuzz,
   `ifelse(eval(x%3==0),1,Fizz,
   `ifelse(eval(x%5==0),1,Buzz,x)')')
')
```



## M2000 Interpreter


```M2000 Interpreter

\\ one line, hard to read
For i=1 to 100 {If i mod 3=0 Then {if i mod 5=0 Then Print "FizzBuzz"  Else Print "Fizz"} Else {if i mod 5=0 Then Print "Buzz" else print i } }

\\ Better code
For i=1 to 100 {
      Push str$(i,0)+". "+if$(i mod 3=0->"Fizz","")+if$(i mod 5=0->"Buzz","")
      If stackitem$()="" then Drop : Continue
      Print Letter$
}

\\ Far Better Code
For i=1 to 100 {
      Printme(if$(i mod 3=0->"Fizz","")+if$(i mod 5=0->"Buzz",""))
}
Sub Printme(a$)
      If a$<>"" Then Print a$ else Print i
End Sub

```



## make

{{works with|BSD make}}
{{libheader|jot}}

```make
MOD3 = 0
MOD5 = 0
ALL != jot 100

all: say-100

.for NUMBER in $(ALL)

MOD3 != expr \( $(MOD3) + 1 \) % 3; true
MOD5 != expr \( $(MOD5) + 1 \) % 5; true

. if "$(NUMBER)" > 1
PRED != expr $(NUMBER) - 1
say-$(NUMBER): say-$(PRED)
. else
say-$(NUMBER):
. endif
. if "$(MOD3)$(MOD5)" == "00"
	@echo FizzBuzz
. elif "$(MOD3)" == "0"
	@echo Fizz
. elif "$(MOD5)" == "0"
	@echo Buzz
. else
	@echo $(NUMBER)
. endif

.endfor
```



## Maple

One line:

```Maple
seq(print(`if`(modp(n,3)=0,`if`(modp(n,15)=0,"FizzBuzz","Fizz"),`if`(modp(n,5)=0,"Buzz",n))),n=1..100):
```

With a fizzbuzz function defined:

```Maple
fizzbuzz1 := n->`if`(modp(n,3)=0,`if`(modp(n,15)=0,"FizzBuzz","Fizz"),`if`(modp(n,5)=0,"Buzz",n)):
for i to 100 do fizzbuzz1(i); od;
```

Using piecewise:

```Maple
fizzbuzz2 := n->piecewise(modp(n,15)=0,"FizzBuzz",modp(n,3)=0,"Fizz",modp(n,5)=0,"Buzz",n):
for i to 100 do fizzbuzz2(i); od;
```

Using conventional if/then branches:

```Maple
fizzbuzz3 := proc(n) local r;
  r:=map2(modp,n,[3,5]);
  if r=[0,0] then "FizzBuzz"
  elif r[1]=0 then "Fizz"
  elif r[2]=0 then "Buzz"
  else n fi;
end proc:
for i to 100 do fizzbuzz3(i); od;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Do[Print[Which[Mod[i, 15] == 0, "FizzBuzz", Mod[i, 5] == 0, "Buzz", Mod[i, 3] == 0, "Fizz", True, i]], {i, 100}]
```

Using rules,

```Mathematica
fizz[i_] := Mod[i, 3] == 0
buzz[i_] := Mod[i, 5] == 0
Range[100] /. {i_ /; fizz[i]&&buzz[i] :> "FizzBuzz", \
               i_?fizz :> "Fizz", i_?buzz :> "Buzz"}
```

Using rules, but different approach:

```Mathematica
SetAttributes[f,Listable]
f[n_ /; Mod[n, 15] == 0] := "FizzBuzz";
f[n_ /; Mod[n, 3] == 0] := "Fizz";
f[n_ /; Mod[n, 5] == 0] := "Buzz";
f[n_] := n;

f[Range[100]]
```

An extendible version using Table

```Mathematica
Table[If[# === "", i, #]&@StringJoin[
   Table[If[Divisible[i, First@nw], Last@nw, ""],
         {nw, {{3, "Fizz"}, {5, "Buzz"}}}]],
      {i, 1, 100}]
```

Another one-liner using Map (the /@ operator shorthand of it) and a pure function with a very readable Which

```Mathematica
 Which[ Mod[#,15] == 0, "FizzBuzz", Mod[#, 3] == 0, "Fizz", Mod[#,5]==0, "Buzz",  True, #]& /@ Range[1,100]
```



## MATLAB

There are more sophisticated solutions to this task, but in the spirit of "lowest level of comprehension required to illustrate adequacy" this is what one might expect from a novice programmer (with a little variation in how the strings are stored and displayed).

```MATLAB
function fizzBuzz()
    for i = (1:100)
        if mod(i,15) == 0
           fprintf('FizzBuzz ')
        elseif mod(i,3) == 0
           fprintf('Fizz ')
        elseif mod(i,5) == 0
           fprintf('Buzz ')
        else
           fprintf('%i ',i))
        end
    end
    fprintf('\n');
end
```

Here's a more extendible version that uses disp() to print the output:

```MATLAB
function out = fizzbuzzS()
	nums = [3, 5];
	words = {'fizz', 'buzz'};
	for (n=1:100)
		tempstr = '';
		for (i = 1:2)
			if mod(n,nums(i))==0
				tempstr = [tempstr,  words{i}];
			end
		end
		if length(tempstr) == 0
			disp(n);
		else
			disp(tempstr);
		end
	end
end
```



## Maxima


```maxima
for n thru 100 do
   if mod(n, 15) = 0 then disp("FizzBuzz")
   elseif mod(n, 3) = 0 then disp("Fizz")
   elseif mod(n,5) = 0 then disp("Buzz")
   else disp(n);
```



## MAXScript


```maxscript
for i in 1 to 100 do
(
    case of
    (
        (mod i 15 == 0): (print "FizzBuzz")
        (mod i 5 == 0):  (print "Buzz")
        (mod i 3 == 0):  (print "Fizz")
        default:         (print i)
    )
)
```



## MEL


```mel
for($i=1; $i<=100; $i++)
{
    if($i % 15 == 0)
        print "FizzBuzz\n";
    else if ($i % 3 == 0)
        print "Fizz\n";
    else if ($i % 5 == 0)
        print "Buzz\n";
    else
        print ($i + "\n");
}
```



## Mercury


```mercury
:- module fizzbuzz.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, string, bool.

:- func fizz(int) = bool.
fizz(N) = ( if N mod 3 = 0 then yes else no ).

:- func buzz(int) = bool.
buzz(N) = ( if N mod 5 = 0 then yes else no ).

%                N    3?    5?
:- func fizzbuzz(int, bool, bool) = string.
fizzbuzz(_, yes, yes) = "FizzBuzz".
fizzbuzz(_, yes, no)  = "Fizz".
fizzbuzz(_, no,  yes) = "Buzz".
fizzbuzz(N, no,  no)  = from_int(N).

main(!IO) :- main(1, 100, !IO).

:- pred main(int::in, int::in, io::di, io::uo) is det.
main(N, To, !IO) :-
    io.write_string(fizzbuzz(N, fizz(N), buzz(N)), !IO),
    io.nl(!IO),
    ( if N < To then
        main(N + 1, To, !IO)
    else
        true
    ).
```



## Metafont


```metafont
for i := 1 upto 100:
message if i mod 15 = 0: "FizzBuzz" &
elseif i mod 3 = 0: "Fizz" &
elseif i mod 5 = 0: "Buzz" &
else: decimal i & fi "";
endfor
end
```



## Microsoft Small Basic

{{trans|GW-BASIC}}

```microsoftsmallbasic

For n = 1 To 100
  op = ""
  If Math.Remainder(n, 3) = 0 Then
    op = "Fizz"
  EndIf
  IF Math.Remainder(n, 5) = 0 Then
    op = text.Append(op, "Buzz")
  EndIf
  If op = "" Then
    TextWindow.WriteLine(n)
  Else
    TextWindow.WriteLine(op)
  EndIf
EndFor

```



## min

{{works with|min|0.19.3}}

```min
0 (
  succ false :hit
  (3 mod 0 ==) ("Fizz" print! true @hit) when
  (5 mod 0 ==) ("Buzz" print! true @hit) when
  (hit) (print) unless newline
) 100 times
```



## MIPS Assembly


```mips

#################################
# Fizz Buzz                     #
# MIPS Assembly targetings MARS #
# By Keith Stellyes             #
# August 24, 2016               #
#################################

# $a0 left alone for printing
# $a1 stores our counter
# $a2 is 1 if not evenly divisible

.data
	fizz: .asciiz "Fizz\n"
	buzz: .asciiz "Buzz\n"
	fizzbuzz: .asciiz "FizzBuzz\n"
	newline: .asciiz "\n"

.text
loop:
	beq $a1,100,exit
	add $a1,$a1,1

	#test for counter mod 15 ("FIZZBUZZ")
	div $a2,$a1,15
	mfhi $a2
	bnez $a2,loop_not_fb #jump past the fizzbuzz print logic if NOT MOD 15

#### PRINT FIZZBUZZ: ####
	li $v0,4 #set syscall arg to PRINT_STRING
	la $a0,fizzbuzz #set the PRINT_STRING arg to fizzbuzz
	syscall #call PRINT_STRING
	j loop #return to start
#### END PRINT FIZZBUZZ ####

loop_not_fb:
	div $a2,$a1,3 #divide $a1 (our counter) by 3 and store remainder in HI
	mfhi $a2 #retrieve remainder (result of MOD)
	bnez $a2, loop_not_f #jump past the fizz print logic if NOT MOD 3

#### PRINT FIZZ ####
	li $v0,4
	la $a0,fizz
	syscall
	j loop
#### END PRINT FIZZ ####

loop_not_f:
	div $a2,$a1,5
	mfhi $a2
	bnez $a2,loop_not_b

#### PRINT BUZZ ####
	li $v0,4
	la $a0,buzz
	syscall
	j loop
#### END PRINT BUZZ ####

loop_not_b:
	#### PRINT THE INTEGER ####
	li $v0,1 #set syscall arg to PRINT_INTEGER
	move $a0,$a1 #set PRINT_INTEGER arg to contents of $a1
	syscall #call PRINT_INTEGER

	### PRINT THE NEWLINE CHAR ###
	li $v0,4 #set syscall arg to PRINT_STRING
	la $a0,newline
	syscall

	j loop #return to beginning

exit:
	li $v0,10
	syscall

```



## Mirah


```mirah
1.upto(100) do |n|
    print "Fizz" if a = ((n % 3) == 0)
    print "Buzz" if b = ((n % 5) == 0)
    print n unless (a || b)
    print "\n"
end
```


A little more straight forward:

```mirah
1.upto(100) do |n|
    if (n % 15) == 0
        puts "FizzBuzz"
    elsif (n % 5) == 0
        puts "Buzz"
    elsif (n % 3) == 0
        puts "Fizz"
    else
        puts n
    end
end
```



## ML

=
## Standard ML
=
First using two helper functions, one for deciding what to output and another for performing recursion with an auxiliary argument j.

```sml
local
  fun fbstr i =
      case (i mod 3 = 0, i mod 5 = 0) of
          (true , true ) => "FizzBuzz"
        | (true , false) => "Fizz"
        | (false, true ) => "Buzz"
        | (false, false) => Int.toString i

  fun fizzbuzz' (n, j) =
      if n = j then () else (print (fbstr j ^ "\n"); fizzbuzz' (n, j+1))
in
  fun fizzbuzz n = fizzbuzz' (n, 1)
  val _ = fizzbuzz 100
end
```


Second using the standard-library combinator List.tabulate and a helper function, fb, that calculates and prints the output.

```sml
local
  fun fb i = let val fizz = i mod 3 = 0 andalso (print "Fizz"; true)
                 val buzz = i mod 5 = 0 andalso (print "Buzz"; true)
             in fizz orelse buzz orelse (print (Int.toString i); true) end
in
  fun fizzbuzz n = (List.tabulate (n, fn i => (fb (i+1); print "\n")); ())
  val _ = fizzbuzz 100
end
```


=
## mLite
=

```ocaml
local
	fun fizzbuzz'
			(x mod 15 = 0) = "FizzBuzz"
		|	(x mod  5 = 0) = "Buzz"
		|	(x mod  3 = 0) = "Fizz"
		|	x = ntos x
in
	fun fizzbuzz
			([], s) = rev s
		|	(x :: xs, s) = fizzbuzz (xs, fizzbuzz' x :: s)
		|	(x :: xs)    = fizzbuzz (x :: xs, [])
end
;

println ` fizzbuzz ` iota 100;

```



## MMIX


```mmix
t   IS $255
Ja  IS $127

       LOC Data_Segment
data   GREG   @

fizz   IS @-Data_Segment
       BYTE "Fizz",0,0,0,0

buzz   IS @-Data_Segment
       BYTE "Buzz",0,0,0,0

nl     IS @-Data_Segment
       BYTE #a,0,0,0,0,0,0,0

buffer IS @-Data_Segment



       LOC #1000
       GREG @

% "usual" print integer subroutine
printnum LOC @
       OR   $1,$0,0
       SETL $2,buffer+64
       ADDU $2,$2,data
       XOR  $3,$3,$3
       STBU $3,$2,1
loop   DIV  $1,$1,10
       GET  $3,rR
       ADDU $3,$3,'0'
       STBU $3,$2,0
       SUBU $2,$2,1
       PBNZ $1,loop
       ADDU t,$2,1
       TRAP 0,Fputs,StdOut
       GO   Ja,Ja,0

Main   SETL $0,1           % i = 1
1H     SETL $2,0           % fizz not taken
       CMP  $1,$0,100      % i <= 100
       BP   $1,4F          % if no, go to end
       DIV  $1,$0,3
       GET  $1,rR          % $1 = mod(i,3)
       CSZ  $2,$1,1        % $2 = Fizz taken?
       BNZ  $1,2F          % $1 != 0? yes, then skip
       ADDU t,data,fizz
       TRAP 0,Fputs,StdOut % print "Fizz"
2H     DIV  $1,$0,5
       GET  $1,rR          % $1 = mod(i,5)
       BNZ  $1,3F          % $1 != 0? yes, then skip
       ADDU t,data,buzz
       TRAP 0,Fputs,StdOut % print "Buzz"
       JMP  5F             % skip print i
3H     BP   $2,5F          % skip if Fizz was taken
       GO   Ja,printnum    % print i
5H     ADDU t,data,nl
       TRAP 0,Fputs,StdOut % print newline
       ADDU $0,$0,1
       JMP  1B             % repeat for next i
4H     XOR  t,t,t
       TRAP 0,Halt,0       % exit(0)
```


=={{header|Modula-2}}==

```modula2
MODULE Fizzbuzz;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE CB = PROCEDURE(INTEGER);

PROCEDURE Fizz(n : INTEGER);
BEGIN
    IF n MOD 3 = 0 THEN
        WriteString("Fizz");
        Buzz(n,Newline)
    ELSE
        Buzz(n,WriteInt)
    END
END Fizz;

PROCEDURE Buzz(n : INTEGER; f : CB);
BEGIN
    IF n MOD 5 = 0 THEN
        WriteString("Buzz");
        WriteLn
    ELSE
        f(n)
    END
END Buzz;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..9] OF CHAR;
BEGIN
    FormatString("%i\n", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE Newline(n : INTEGER);
BEGIN
    WriteLn
END Newline;

VAR i : INTEGER;
BEGIN
    FOR i:=1 TO 30 DO
        Fizz(i)
    END;

    ReadChar
END Fizzbuzz.
```


=={{header|Modula-3}}==

```modula3
MODULE Fizzbuzz EXPORTS Main;

IMPORT IO;

BEGIN
   FOR i := 1 TO 100 DO
      IF i MOD 15 = 0 THEN
         IO.Put("FizzBuzz\n");
      ELSIF i MOD 5 = 0 THEN
         IO.Put("Buzz\n");
      ELSIF i MOD 3 = 0 THEN
         IO.Put("Fizz\n");
      ELSE
         IO.PutInt(i);
         IO.Put("\n");
      END;
   END;
END Fizzbuzz.
```



## Monte



```Monte
def fizzBuzz(top):
    var t := 1
    while (t < top):
        if ((t % 3 == 0) || (t % 5 == 0)):
            if (t % 15 == 0):
                traceln(`$t  FizzBuzz`)
            else if (t % 3 == 0):
                traceln(`$t  Fizz`)
            else:
                traceln(`$t  Buzz`)
        t += 1

fizzBuzz(100)

```



## MontiLang



```MontiLang
&DEFINE LOOP 100&
1 VAR i .

FOR LOOP
    || VAR ln .
    i 5 % 0 ==
    IF : .
        ln |Buzz| + VAR ln .
    ENDIF
    i 3 % 0 ==
    IF : .
        ln |Fizz| + VAR ln .
    ENDIF
    ln || ==
    IF : .
        i PRINT .
    ENDIF
    ln || !=
    IF : .
        ln PRINT .
    ENDIF
i 1 + VAR i .
ENDFOR
```



## MoonScript



```moonscript
for i = 1,100
    print ((a) -> a == "" and i or a) table.concat {
        i % 3 == 0 and "Fizz" or ""
        i % 5 == 0 and "Buzz" or ""}
```



## MUMPS


```MUMPS
FIZZBUZZ
 NEW I
 FOR I=1:1:100 WRITE !,$SELECT(('(I#3)&'(I#5)):"FizzBuzz",'(I#5):"Buzz",'(I#3):"Fizz",1:I)
 KILL I
 QUIT
```



```MUMPS
fizzbuzz
 for i=1:1:100 do  write !
 . write:(i#3)&(i#5) i write:'(i#3) "Fizz" write:'(i#5) "Buzz"
```



## Neko


```Neko
var i = 1

while(i < 100) {
	if(i % 15 == 0) {
		$print("FizzBuzz\n");
	} else if(i % 3 == 0) {
		$print("Fizz\n");
	} else if(i % 5 == 0) {
		$print("Buzz\n");
	} else {
		$print(i + "\n");
	}

	i ++= 1
}
```



## Nemerle

The naive approach:

```Nemerle
using System;
using System.Console;

module FizzBuzz
{
    FizzBuzz(x : int) : string
    {
        |x when x % 15 == 0 => "FizzBuzz"
        |x when x %  5 == 0 => "Buzz"
        |x when x %  3 == 0 => "Fizz"
        |_                  => $"$x"
    }

    Main() : void
    {
        foreach (i in [1 .. 100])
            WriteLine($"$(FizzBuzz(i))")
    }
}
```

A much slicker approach is [http://www.dreamincode.net/forums/blog/217/entry-3539-fizzbuzz-in-nemerle/ posted here]


## NetRexx


```netrexx
loop j=1 for 100
  select
    when j//15==0 then say 'FizzBuzz'
    when j//5==0  then say 'Buzz'
    when j//3==0  then say 'Fizz'
    otherwise say j.right(4)
  end
end
```



## Never


```fsharp
func fizz_buzz() -> int
{
    var i = 1;

    for (i = 1; i <= 100; i = i + 1)
    {
        /* if (i % 15 == 0) */
        if (i % 3 == 0 && i % 5 == 0)
        {
            prints("Fizz Buzz\n")
        }
        else if (i % 3 == 0)
        {
            prints("Fizz\n")
        }
        else if (i % 5 == 0)
        {
            prints("Buzz\n")
        }
        else
        {
            prints(i + "\n")
        }
    };

    0
}

func main() -> int {
    fizz_buzz();

    0
}
```


{{out}}

```txt
prompt$ never -f fizzbuzz.nev 2>/dev/null
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizz Buzz
16
...
89
Fizz Buzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz
```



## NewLISP


```NewLISP
(dotimes (i 100)
  (println
   (cond
    ((= 0 (% i 15)) "FizzBuzz")
    ((= 0 (% i 3)) "Fizz")
    ((= 0 (% i 5)) "Buzz")
    ('t i))))
```



## NewtonScript


```newton
for i := 1 to 100 do
begin
	if i mod 15 = 0 then
		print("FizzBuzz")
	else if i mod 3 = 0 then
		print("Fizz")
	else if i mod 5 = 0 then
		print("Buzz")
	else
		print(i);
	print("\n")
end
```



## Nickle


```nickle
/* Fizzbuzz in nickle */

void function fizzbuzz(size) {
    for (int i = 1; i < size; i++) {
        if (i % 15 == 0) { printf("Fizzbuzz\n"); }
        else if (i % 5 == 0) { printf("Buzz\n"); }
        else if (i % 3 == 0) { printf("Fizz\n"); }
        else { printf("%i\n", i); }
    }
}

fizzbuzz(1000);
```



## Nim

{{trans|Python}}

```nim
for i in 1..100:
  if i mod 15 == 0:
    echo("FizzBuzz")
  elif i mod 3 == 0:
    echo("Fizz")
  elif i mod 5 == 0:
    echo("Buzz")
  else:
    echo(i)
```



### Without Modulus


```nim
var messages = @["", "Fizz", "Buzz", "FizzBuzz"]
var acc = 810092048
for i in 1..100:
  var c = acc and 3
  echo(if c == 0: $i else: messages[c])
  acc = acc shr 2 or c shl 28
```



### Using macro

Computes everything at compile time.

```nim
import macros
macro FizzBuzz(N): stmt =
  var source = ""
  for i in 1..N.intVal:
    source &= "echo \""
    if i mod 15 == 0:
      source &= "FizzBuzz"
    elif i mod 3 == 0:
      source &= "Fizz"
    elif i mod 5 == 0:
      source &= "Buzz"
    else:
      source &= $i
    source &= "\"\n"
  result = parseStmt(source)

FizzBuzz(100)
```


=={{header|Oberon-2}}==

```oberon2
MODULE FizzBuzz;

   IMPORT Out;

   VAR i: INTEGER;

BEGIN
   FOR i := 1 TO 100 DO
      IF i MOD 15 = 0 THEN
         Out.String("FizzBuzz")
      ELSIF i MOD 5 = 0 THEN
         Out.String("Buzz")
      ELSIF i MOD 3 = 0 THEN
         Out.String("Fizz")
      ELSE
         Out.Int(i,0)
      END;
      Out.Ln
   END
END FizzBuzz.
```



## Objeck


```objeck
bundle Default {
  class Fizz {
    function : Main(args : String[]) ~ Nil {
      for(i := 0; i <= 100; i += 1;) {
        if(i % 15 = 0) {
          "FizzBuzz"->PrintLine();
        }
        else if(i % 3 = 0) {
          "Fizz"->PrintLine();
        }
        else if(i % 5 = 0) {
          "Buzz"->PrintLine();
        }
        else {
          i->PrintLine();
        };
      };
    }
  }
}
```


=={{header|Objective-C}}==

```c
// FizzBuzz in Objective-C
#import <stdio.h>

main() {
	for (int i=1; i<=100; i++) {
		if (i % 15 == 0) {
		    printf("FizzBuzz\n");
		} else if (i % 3 == 0) {
		    printf("Fizz\n");
		} else if (i % 5 == 0) {
		    printf("Buzz\n");
		} else {
		    printf("%i\n", i);
		}
	}
}
```



## OCaml


Idiomatic OCaml to solve the stated problem:


```ocaml
let fizzbuzz i =
  match i mod 3, i mod 5 with
    0, 0 -> "FizzBuzz"
  | 0, _ -> "Fizz"
  | _, 0 -> "Buzz"
  | _    -> string_of_int i

let _ =
  for i = 1 to 100 do print_endline (fizzbuzz i) done
```


With a view toward extensibility, there are many approaches: monadic, list of rules, ... here we'll use a piped sequence of rules to define a new "fizzbuzz" function:


```ocaml
(* Useful rule declaration: "cond => f", 'cond'itionally applies 'f' to 'a'ccumulated value *)
let (=>) cond f a = if cond then f a else a
let append s a = a^s

let fizzbuzz i =
  "" |> (i mod 3 = 0 => append "Fizz")
     |> (i mod 5 = 0 => append "Buzz")
     |> (function "" -> string_of_int i
                | s  -> s)
```



## Octave


```octave
for i = 1:100
  if ( mod(i,15) == 0 )
    disp("FizzBuzz");
  elseif ( mod(i, 3) == 0 )
    disp("Fizz")
  elseif ( mod(i, 5) == 0 )
    disp("Buzz")
  else
    disp(i)
  endif
endfor
```



## Oforth



```Oforth
: fizzbuzz
| i |
   100 loop: i [
      null
      i 3 mod ifZero: [ "Fizz" + ]
      i 5 mod ifZero: [ "Buzz" + ]
      dup ifNull: [ drop i ] .
      ] ;
```



## OOC


```ooc
fizz: func (n: Int) -> Bool {
  if(n % 3 == 0) {
    printf("Fizz")
    return true
  }
  return false
}

buzz: func (n: Int) -> Bool {
  if(n % 5 == 0) {
    printf("Buzz")
    return true
  }
  return false
}

main: func {
  for(n in 1..100) {
    fizz:= fizz(n)
    buzz:= buzz(n)
    fizz || buzz || printf("%d", n)
    println()
  }
}
```



## Order


```c>#include <order/interpreter.h


// Get FB for one number
#define ORDER_PP_DEF_8fizzbuzz ORDER_PP_FN(            \
8fn(8N,                                                \
    8let((8F, 8fn(8N, 8G,                              \
                  8is_0(8remainder(8N, 8G)))),         \
         8cond((8ap(8F, 8N, 15), 8quote(fizzbuzz))     \
               (8ap(8F, 8N, 3), 8quote(fizz))          \
               (8ap(8F, 8N, 5), 8quote(buzz))          \
               (8else, 8N)))) )

// Print E followed by a comma (composable, 8print is not a function)
#define ORDER_PP_DEF_8print_el ORDER_PP_FN( \
8fn(8E, 8print(8E 8comma)) )

ORDER_PP(  // foreach instead of map, to print but return nothing
  8seq_for_each(8compose(8print_el, 8fizzbuzz), 8seq_iota(1, 100))
)
```



## Oz


```oz
declare
  fun {FizzBuzz X}
     if     X mod 15 == 0 then 'FizzBuzz'
     elseif X mod  3 == 0 then 'Fizz'
     elseif X mod  5 == 0 then 'Buzz'
     else                      X
     end
  end
in
  for I in 1..100 do
     {Show {FizzBuzz I}}
  end
```



## PARI/GP


```parigp
{for(n=1,100,
  print(if(n%3,
    if(n%5,
      n
    ,
      "Buzz"
    )
  ,
    if(n%5,
      "Fizz"
    ,
      "FizzBuzz"
    )
  ))
)}
```



## Pascal


```pascal
program fizzbuzz(output);
var
  i: integer;
begin
  for i := 1 to 100 do
    if i mod 15 = 0 then
      writeln('FizzBuzz')
    else if i mod 3 = 0 then
      writeln('Fizz')
    else if i mod 5 = 0 then
      writeln('Buzz')
    else
      writeln(i)
end.
```



=={{header|PDP-8 Assembly}}==
{{works with| PAL-D}}

Runs on SimH, or any PDP-8 with an EAE


```assembly

/--------------------------------------------------------
/THIS PROGRAM PRINTS THE INTEGERS FROM 1 TO 100 (INCL).
/WITH THE FOLLOWING RESTRICTIONS:
/  FOR MULTIPLES OF THREE, PRINT 'FIZZ'
/  FOR MULTIPLES OF FIVE,  PRINT 'BUZZ'
/  FOR MULTIPLES OF BOTH THREE AND FIVE, PRINT 'FIZZBUZZ'
/--------------------------------------------------------

/--------------------------------------------------------
/DEFINES
/--------------------------------------------------------
SWBA=7447               /EAE MODE A INSTRUCTION
DVI=7407                /EAE DIVIDE INSTRUCTION
AIX0=0010               /AUTO INDEX REGISTER 0
CR=0215                 /CARRIAGE RETURN
LF=0212                 /LINEFEED
EOT=0000                /END OF TEXT NUL
FIZMOD=0003             /CONSTANT DECIMAL 3 (FIZZ)
BUZMOD=0005             /CONSTANT DECIMAL 5 (BUZZ)
K10=0012                /CONSTANT DECIMAL 10

LAST=0144               /FIZZBUZZ THE NUMBERS 1..LAST
                        /0144 OCTAL == 100 DECIMAL
                        /CAN BE ANY FROM [0001...7777]

/--------------------------------------------------------
/FIZZBUZZ START=0200
/--------------------------------------------------------
        *200            /START IN MEMORY AT 0200 OCTAL
FZZBZZ, CLA             /CLEAR AC
        TAD (-LAST-1    /LOAD CONSTANT -(LAST+1)
        DCA CNTR        /SET UP MAIN COUNTER
        TAD (-FIZMOD    /SET UP FIZZ COUNTER
        DCA FIZCTR      /TO -3
        TAD (-BUZMOD    /SET UP BUZZ COUNTER
        DCA BUZCTR      /TO -5
LOOP,   ISZ CNTR        /READY?
        SKP             /NO: CONTINUE
        JMP I [7600     /YES: RETURN TO OS/8, REPLACE BY
                        /'HLT' IF NOT ON OS/8
CHKFIZ, ISZ FIZCTR      /MULTIPLE OF 3?
        JMP CHKBUZ      /NO: CONTINUE
        TAD FIZSTR      /YES: LOAD ADDRESS OF 'FIZZ'
        JMS STROUT      /PRINT IT TO TTY
        TAD (-FIZMOD    /RELOAD THE
        DCA FIZCTR      /MOD 3 COUNTER
CHKBUZ, ISZ BUZCTR      /MULTIPLE OF 5?
        JMP CHKNUM      /NO: CONTINUE
        TAD BUZSTR      /YES: LOAD ADDRESS OF 'BUZZ'
        JMS STROUT      /PRINT IT TO TTY
        TAD (-BUZMOD    /RELOAD THE
        DCA BUZCTR      /MOD 5 COUNTER
        JMP NXTLIN      /PRINT NEWLINE AND CONTINUE
CHKNUM, TAD FIZCTR      /CHECK WHETHER MOD 3 COUNTER
        TAD (FIZMOD     /IS RELOADED
        SNA             /DID WE JUST PRINT 'FIZZ'?
        JMP NXTLIN      /YES: PRINT NEWLINE AND CONTINUE
        CLA             /ZERO THE AC
NUM,    TAD CNTR        /LOAD THE MAIN NUMBER COUNTER
        TAD (LAST+1     /OFFSET IT TO A POSITIVE VALUE
        JMS NUMOUT      /PRINT IT TO THE TTY
NXTLIN, TAD LINSTR      /LOAD THE ADDRESS OF THE NEWLINE
        JMS STROUT      /PRINT IT TO TTY
        JMP LOOP        /CONTINUE WITH THE NEXT NUMBER
CNTR,   0               /MAIN COUNTER
FIZCTR, 0               /FIZZ COUNTER
BUZCTR, 0               /BUZZ COUNTER

/--------------------------------------------------------
/WRITE ASCII CHARACTER IN AC TO TTY
/PRE : AC=ASCII CHARACTER
/POST: AC=0
/--------------------------------------------------------
CHROUT, .-.
        TLS             /SEND CHARACTER TO TTY
        TSF             /IS TTY READY FOR NEXT CHARACTER?
        JMP .-1         /NO TRY AGAIN
        CLA             /AC=0
        JMP I CHROUT    /RETURN

/--------------------------------------------------------
/WRITE NUL TERMINATED ASCII STRING TO TTY
/PRE : AC=ADDRESS OF STRING MINUS 1
/POST: AC=0
/--------------------------------------------------------
STROUT, .-.
        DCA AIX0        /STORE POINTER IN AUTO INDEX 0
STRLOP, TAD I AIX0      /GET NEXT CHARACTER FROM STRING
        SNA             /SKIP IF NOT EOT
        JMP I STROUT    /RETURN
        JMS CHROUT      /PRINT CHARACTER
        JMP STRLOP      /GO GET NEXT CHARACTER

/--------------------------------------------------------
/WRITE NUMBER IN AC TO TTY AS DECIMAL
/PRE : AC=UNSIGNED NUMBER BETWEEN 0000 AND 7777
/POST: AC=0
/--------------------------------------------------------
NUMOUT, .-.
        SWBA            /SET EAE IN MODE A
        MQL             /MQ=NUM; AC=0
        TAD BUFFER      /LOAD END OF BUFFER
        DCA BUFPTR      /IN BUFPTR
        SKP             /NUM IS ALREADY IN MQ
NUMLOP, MQL             /MQ=NUM; AC=0
        DVI             /MQ=NUM/10; AC=NUM-(NUM/10)*10
        K10             /DECIMAL 10
        TAD ("0         /ADD ASCII ZERO
        DCA I BUFPTR    /STORE CHAR BUFFER, BACK TO FRONT
        CMA             /AC=-1
        TAD BUFPTR      /DECREMENT
        DCA BUFPTR      /BUFFER POINTER
        MQA             /MQ -> AC
        SZA             /READY IF ZERO
        JMP NUMLOP      /GET NEXT DIGIT
        TAD BUFPTR      /LOAD START OF CONVERTED NUMBER
        JMS STROUT      /SEND IT TO TTY
        JMP I NUMOUT    /RETURN
BUFFER, .+4             /ADDRESS OF BUFFER
        *.+4            /RESERVE 4 LOCATIONS (MAX=4095)
        EOT             /END OF BUFFER
BUFPTR, 0               /POINTER IN BUFFER

/--------------------------------------------------------
/STRINGS
/--------------------------------------------------------
FIZSTR, .               /FIZZ STRING
        "F; "I; "Z; "Z; EOT
BUZSTR, .               /BUZZ STRING
        "B; "U; "Z; "Z; EOT
LINSTR, .               /NEWLINE STIRNG
        CR; LF; EOT
        $

```


Output:

```txt

.
.PAL FIZBUZ.PA
ERRORS DETECTED: 0
LINKS GENERATED: 0

.LOAD FIZBUZ.BN

.START
1
2
FIZZ
4
BUZZ
FIZZ
7
8
FIZZ
BUZZ
11
FIZZ
13
14
FIZZBUZZ
16
17
FIZZ
19
BUZZ
FIZZ
22
23
FIZZ
BUZZ
26
FIZZ
28
29
FIZZBUZZ
31
32
FIZZ
34
BUZZ
FIZZ
37
38
FIZZ
BUZZ
41
FIZZ
43
44
FIZZBUZZ
46
47
FIZZ
49
BUZZ
FIZZ
52
53
FIZZ
BUZZ
56
FIZZ
58
59
FIZZBUZZ
61
62
FIZZ
64
BUZZ
FIZZ
67
68
FIZZ
BUZZ
71
FIZZ
73
74
FIZZBUZZ
76
77
FIZZ
79
BUZZ
FIZZ
82
83
FIZZ
BUZZ
86
FIZZ
88
89
FIZZBUZZ
91
92
FIZZ
94
BUZZ
FIZZ
97
98
FIZZ
BUZZ

.

```



## Peloton

Variable-length padded English dialect

```sgml><# DEFINE USERDEFINEDROUTINE LITERAL
__FizzBuzz|<# SUPPRESSAUTOMATICWHITESPACE>
<# TEST ISITMODULUSZERO PARAMETER LITERAL>1|3</#>
<# TEST ISITMODULUSZERO PARAMETER LITERAL>1|5</#>
<# ONLYFIRSTOFLASTTWO><# SAY LITERAL>Fizz</#></#>
<# ONLYSECONDOFLASTTWO><# SAY LITERAL>Buzz</#></#>
<# BOTH><# SAY LITERAL>FizzBuzz</#></#>
<# NEITHER><# SAY PARAMETER>1</#></#>
</#></#>
<# ITERATE FORITERATION LITERAL LITERAL>100|<# ACT USERDEFINEDROUTINE POSITION FORITERATION>__FizzBuzz|...</#> </#>
```

Fixed-length English dialect

```sgml><@ DEFUDRLIT
__FizzBuzz|<@ SAW>
<@ TSTMD0PARLIT>1|3</@>
<@ TSTMD0PARLIT>1|5</@>
<@ O12><@ SAYLIT>Fizz</@></@>
<@ O22><@ SAYLIT>Buzz</@></@>
<@ BTH><@ SAYLIT>FizzBuzz</@></@>
<@ NTH><@ SAYPAR>1</@></@>
</@></@>
<@ ITEFORLITLIT>100|<@ ACTUDRPOSFOR>__FizzBuzz|...</@> </@>
```



## Perl


```perl

use strict;
use warnings;
use feature qw(say);

for my $i (1..100) {
    say $i % 15 == 0 ? "FizzBuzz"
      : $i %  3 == 0 ? "Fizz"
      : $i %  5 == 0 ? "Buzz"
      : $i;
}
```


More concisely:


```perl
print 'Fizz'x!($_ % 3) . 'Buzz'x!($_ % 5) || $_, "\n" for 1 .. 100;
```


For code-golfing:


```perl
print+(Fizz)[$_%3].(Buzz)[$_%5]||$_,$/for 1..1e2
```


For array of values:


```perl
map((Fizz)[$_%3].(Buzz)[$_%5]||$_,1..100);
```


Cheating:


```perl

use feature "say";

@a = ("FizzBuzz", 0, 0, "Fizz", 0, "Buzz", "Fizz", 0, 0, "Fizz", "Buzz", 0, "Fizz");

say $a[$_ % 15] || $_ for 1..100;
```


as a subroutine:


```perl

sub fizz_buzz {
    join("\n", map {
        sub mult {$_ % shift == 0};
        my @out;
        if (mult 3) { push @out, "Fizz"; }
        if (mult 5) { push @out, "Buzz"; }
        if (!@out) {push @out, $_; }
        join('', @out);
    } (1..100))."\n";
}
print fizz_buzz;

```


By transforming a list:


```perl

@FB1 = (1..100);
@FB2 = map{!($_%3 or $_%5)?'FizzBuzz': $_}@FB1;
@FB3 = map{(/\d/ and !($_%3))?'Fizz': $_}@FB2;
@FB4 = map{(/\d/ and !($_%5))?'Buzz': $_}@FB3;
@FB5 = map{$_."\n"}@FB4;
print @FB5;

```



## Perl 6

{{works with|Rakudo Star|2015-09-10}}
Most straightforwardly:

```perl6
for 1 .. 100 {
    when $_ %% (3 & 5) { say 'FizzBuzz'; }
    when $_ %% 3       { say 'Fizz'; }
    when $_ %% 5       { say 'Buzz'; }
    default            { .say; }
}
```

Or abusing multi subs:

```perl6
multi sub fizzbuzz(Int $ where * %% 15) { 'FizzBuzz' }
multi sub fizzbuzz(Int $ where * %%  5) { 'Buzz' }
multi sub fizzbuzz(Int $ where * %%  3) { 'Fizz' }
multi sub fizzbuzz(Int $number        ) { $number }
(1 .. 100)».&fizzbuzz.say;
```

Or abusing list metaoperators:

```perl6
[1..100].map({[~] ($_%%3, $_%%5) »||» "" Z&& <fizz buzz> or $_ })».say
```

Concisely (readable):

```perl6
say 'Fizz' x $_ %% 3 ~ 'Buzz' x $_ %% 5 || $_ for 1 .. 100;
```

Shortest FizzBuzz to date:

```perl6
say "Fizz"x$_%%3~"Buzz"x$_%%5||$_ for 1..100
```

And here's an implementation that never checks for divisibility:

```perl6
.say for
    (
      (flat ('' xx 2, 'Fizz') xx *)
      Z~
      (flat ('' xx 4, 'Buzz') xx *)
    )
    Z||
    1 .. 100;
```



## Phix

{{trans|C}}

```Phix
constant x = {"%d\n","Fizz\n","Buzz\n","FizzBuzz\n"}
for i=1 to 100 do
    printf(1,x[1+(remainder(i,3)=0)+2*(remainder(i,5)=0)],i)
end for

```



## PHL

{{trans|C}}

```phl
module fizzbuzz;

extern printf;

@Integer main [
	var i = 1;
	while (i <= 100) {
		if (i % 15 == 0)
		    printf("FizzBuzz");
		else if (i % 3 == 0)
		    printf("Fizz");
		else if (i % 5 == 0)
		    printf("Buzz");
		else
		    printf("%d", i);

		printf("\n");
		i = i::inc;
	}

	return 0;
]
```



## PHP


### if/else ladder approach


```php
<?php
for ($i = 1; $i <= 100; $i++)
{
    if (!($i % 15))
        echo "FizzBuzz\n";
    else if (!($i % 3))
        echo "Fizz\n";
    else if (!($i % 5))
        echo "Buzz\n";
    else
        echo "$i\n";
}
?>
```


### concatenation approach

Uses PHP's concatenation operator (.=) to build the output string. The concatenation operator allows us to add data to the end of a string without overwriting the whole string. Since Buzz will always appear if our number is divisible by five, and Buzz is the second part of "FizzBuzz", we can simply append "Buzz" to our string.

In contrast to the if-else ladder, this method lets us skip the check to see if $i is divisible by both 3 and 5 (i.e. 15). However, we get the added complexity of needing to reset $str to an empty string (not necessary in some other languages), and we also need a separate if statement to check to see if our string is empty, so we know if $i was not divisible by 3 or 5.

```php
<?php
for ( $i = 1; $i <= 100; ++$i )
{
     $str = "";

     if (!($i % 3 ) )
          $str .= "Fizz";

     if (!($i % 5 ) )
          $str .= "Buzz";

     if ( empty( $str ) )
          $str = $i;

     echo $str . "\n";
}
?>
```


### One Liner Approach


```php
<?php
for($i = 1; $i <= 100 and print(($i % 15 ? $i % 5 ? $i % 3 ? $i : 'Fizz' : 'Buzz' : 'FizzBuzz') . "\n"); ++$i);
?>
```



### Compact One Liner Approach


```php
for($i=0;$i++<100;)echo($i%3?'':'Fizz').($i%5?'':'Buzz')?:$i,"\n";
```


### Array One Liner Approach


```php
for($i = 0; $i++ < 100;) echo [$i, 'Fizz', 'Buzz', 'FizzBuzz'][!($i % 3) + 2 * !($i % 5)], "\n";
```



## PicoLisp

We could simply use '[http://software-lab.de/doc/refA.html#at at]' here:

```PicoLisp
(for N 100
   (prinl
      (or (pack (at (0 . 3) "Fizz") (at (0 . 5) "Buzz")) N) ) )

# Above, we simply count till 100 'prin'-ting number 'at' 3rd ('Fizz'), 5th ('Buzz') and 'pack'-ing 15th number ('FizzBuzz').
# Rest of the times 'N' is printed as it loops in 'for'.


```

Or do it the standard way:

```PicoLisp
(for N 100
   (prinl
      (cond
         ((=0 (% N 15)) "FizzBuzz")
         ((=0 (% N 3)) "Fizz")
         ((=0 (% N 5)) "Buzz")
         (T N) ) ) )
```



## Pike


```pike
int main(){
   for(int i = 1; i <= 100; i++) {
      if(i % 15 == 0) {
         write("FizzBuzz\n");
      } else if(i % 3 == 0) {
         write("Fizz\n");
      } else if(i % 5 == 0) {
         write("Buzz\n");
      } else {
         write(i + "\n");
      }
   }
}
```



## PILOT


```pilot
C  :i = 0
*loop
C  :i = i + 1
J ( i > 100 )    : *finished
C  :modulo = i % 15
J ( modulo = 0 ) : *fizzbuzz
C  :modulo = i % 3
J ( modulo = 0 ) : *fizz
C  :modulo = i % 5
J ( modulo = 0 ) : *buzz
T  :#i
J  : *loop
*fizzbuzz
T  :FizzBuzz
J  : *loop
*fizz
T  :Fizz
J  : *loop
*buzz
T  :Buzz
J  : *loop
*finished
END:
```



## PIR

{{works with|Parrot|tested with 2.4.0}}

```pir
.sub main :main
  .local int f
  .local int mf
  .local int skipnum
  f = 1
LOOP:
  if f > 100 goto DONE
  skipnum = 0
  mf = f % 3
  if mf == 0 goto FIZZ
FIZZRET:
  mf = f % 5
  if mf == 0 goto BUZZ
BUZZRET:
  if skipnum > 0 goto SKIPNUM
  print f
SKIPNUM:
  print "\n"
  inc f
  goto LOOP
  end
FIZZ:
  print "Fizz"
  inc skipnum
  goto FIZZRET
  end
BUZZ:
  print "Buzz"
  inc skipnum
  goto BUZZRET
  end
DONE:
  end
.end
```



## PL/I


```PL/I
do i = 1 to 100;
   select;
      when (mod(i,15) = 0) put skip list ('FizzBuzz');
      when (mod(i,3)  = 0) put skip list ('Fizz');
      when (mod(i,5)  = 0) put skip list ('Buzz');
      otherwise put skip list (i);
   end;
end;
```



## PL/SQL


```plsql
begin
  for i in 1 .. 100
  loop
    case
    when mod(i, 15) = 0 then
      dbms_output.put_line('FizzBuzz');
    when mod(i, 5) = 0 then
      dbms_output.put_line('Buzz');
    when mod(i, 3) = 0 then
      dbms_output.put_line('Fizz');
    else
      dbms_output.put_line(i);
    end case;
  end loop;
end;
```



## Pony


```pony
use "collections"

actor Main
  new create(env: Env) =>
    for i in Range[I32](1, 100) do
      env.out.print(fizzbuzz(i))
    end

  fun fizzbuzz(n: I32): String =>
    if (n % 15) == 0 then
      "FizzBuzz"
    elseif (n % 5) == 0 then
      "Buzz"
    elseif (n % 3) == 0 then
      "Fizz"
    else
      n.string()
    end
```



## Pop11


```pop11
lvars str;
for i from 1 to 100 do
  if i rem 15 = 0 then
    'FizzBuzz' -> str;
  elseif i rem 3 = 0 then
    'Fizz' -> str;
  elseif i rem 5 = 0 then
    'Buzz' -> str;
  else
    '' >< i -> str;
  endif;
  printf(str, '%s\n');
endfor;
```



## PostScript


```postscript
1 1 100 {
	/c false def
	dup 3 mod 0 eq { (Fizz) print /c true def } if
	dup 5 mod 0 eq { (Buzz) print /c true def } if
    c {pop}{(   ) cvs print} ifelse
    (\n) print
} for
```

or...

```postscript
/fizzdict 100 dict def
fizzdict begin
/notmod{ (   ) cvs } def
/mod15 { dup 15 mod 0 eq { (FizzBuzz)def }{pop}ifelse} def
/mod3  { dup 3 mod 0 eq {(Fizz)def}{pop}ifelse} def
/mod5  { dup 5 mod 0 eq {(Buzz)def}{pop}ifelse} def
1 1 100 { mod3 } for
1 1 100 { mod5 } for
1 1 100 { mod15} for
1 1 100 { dup currentdict exch known { currentdict exch get}{notmod} ifelse print (\n) print} for
end
```



## Potion


```lua

1 to 100 (a):
  if (a % 15 == 0):
    'FizzBuzz'.
  elsif (a % 3 == 0):
    'Fizz'.
  elsif (a % 5 == 0):
    'Buzz'.
  else: a. string print
  "\n" print.
```



## PowerShell

===Straightforward, looping===

```powershell
for ($i = 1; $i -le 100; $i++) {
    if ($i % 15 -eq 0) {
        "FizzBuzz"
    } elseif ($i % 5 -eq 0) {
        "Buzz"
    } elseif ($i % 3 -eq 0) {
        "Fizz"
    } else {
        $i
    }
}
```

===Pipeline, Switch===

```powershell
$txt=$null
1..100 | ForEach-Object {
    switch ($_) {
        { $_ % 3 -eq 0 }  { $txt+="Fizz" }
        { $_ % 5 -eq 0 }  { $txt+="Buzz" }
        $_                { if($txt) { $txt } else { $_ }; $txt=$null }
    }
}
```



### Concatenation

{{trans|C#}}

```powershell
1..100 | ForEach-Object {
    $s = ''
    if ($_ % 3 -eq 0) { $s += "Fizz" }
    if ($_ % 5 -eq 0) { $s += "Buzz" }
    if (-not $s) { $s = $_ }
    $s
}
```


===Piping, Evaluation, Concatenation===

```powershell

1..100 | % {write-host("$(if(($_ % 3 -ne 0) -and ($_ % 5 -ne 0)){$_})$(if($_ % 3 -eq 0){"Fizz"})$(if($_ % 5 -eq 0){"Buzz"})")}

```


===Filter, Piping, Regex Matching, Array Auto-Selection===

```powershell

filter fizz-buzz{
    @(
        $_,
        "Fizz",
        "Buzz",
        "FizzBuzz"
    )[
        2 *
        ($_ -match '[05]$') +
        ($_ -match '(^([369][0369]?|[258][147]|[147][258]))$')
    ]
}

1..100 | fizz-buzz

```



## Processing

===Visualization & Console, Straightforward===
Reserved variable "width" in Processing is 100 pixels by default, suitable for this FizzBuzz exercise.
Accordingly, range is pixel index from 0 to 99.

```Processing
for (int i = 0; i < width; i++) {
  if (i % 3 == 0 && i % 5 == 0) {
    stroke(255, 255, 0);
    println("FizzBuzz!");
  }
  else if (i % 5 == 0) {
    stroke(0, 255, 0);
    println("Buzz");
  }
  else if (i % 3 == 0) {
    stroke(255, 0, 0);
    println("Fizz");
  }
  else {
    stroke(0, 0, 255);
    println(i);
  }
  line(i, 0, i, height);
}
```

===Visualization & Console, Ternary===

```Processing
for (int i = 0; i < width; i++) {
  stroke((i % 5 == 0 && i % 3 == 0) ? #FFFF00 : (i % 5 == 0) ? #00FF00 : (i % 3 == 0) ? #FF0000 : #0000FF);
  line(i, 0, i, height);
  println((i % 5 == 0 && i % 3 == 0) ? "FizzBuzz!" : (i % 5 == 0) ? "Buzz" : (i % 3 == 0) ? "Fizz" : i);
}
```

===Console Only, Straightforward===

```Processing
for (int i = 1; i <= 100; i++) {
  if (i % 3 == 0) {
    print("Fizz");
  }
  if (i % 5 == 0) {
    print("Buzz");
  }
  if (i % 3 != 0 && i % 5 != 0) {
    print(i);
  }
  print("\n");
}
```


{{out}}
<pre style="height:35ex">
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
FizzBuzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
FizzBuzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
FizzBuzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
FizzBuzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz

```


===Console Only, "Futureproof"===

```Processing
for(int i = 1; i <= 100; i++){
	String output = "";

	if(i % 3 == 0) output += "Fizz";
	if(i % 5 == 0) output += "Buzz";
	// copy & paste above line to add more tests

	if(output == "") output = int(i);
	println(output);
}
}
```


{{out}}
<pre style="height:35ex">
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
FizzBuzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
FizzBuzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
FizzBuzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
FizzBuzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz

```



## Prolog

{{works with|SWI Prolog|4.8.0}}
Maybe not the most conventional way to write this in Prolog. The fizzbuzz predicate uses a higher-order predicate and print_item uses the if-then-else construction.

```prolog
fizzbuzz :-
        foreach(between(1, 100, X), print_item(X)).

print_item(X) :-
        (  0 is X mod 15
        -> print('FizzBuzz')
        ;  0 is X mod 3
        -> print('Fizz')
        ;  0 is X mod 5
        -> print('Buzz')
        ;  print(X)
        ),
        nl.
```

More conventional:

```prolog
fizzbuzz(X) :- 0 is X mod 15, write('FizzBuzz').
fizzbuzz(X) :- 0 is X mod 3, write('Fizz').
fizzbuzz(X) :- 0 is X mod 5, write('Buzz').
fizzbuzz(X) :- write(X).

dofizzbuzz :- foreach(between(1, 100, X), (fizzbuzz(X),nl)).
```

Clearer:

```prolog
%        N  /3?  /5?  V
fizzbuzz(_, yes, yes, 'FizzBuzz').
fizzbuzz(_, yes, no,  'Fizz').
fizzbuzz(_, no,  yes, 'Buzz').
fizzbuzz(N, no,  no,  N).

% Unifies V with 'yes' if D divides evenly into N, 'no' otherwise.
divisible_by(N, D, V) :-
  ( 0 is N mod D -> V = yes
  ;                 V = no).

% Print 'Fizz', 'Buzz', 'FizzBuzz' or N as appropriate.
fizz_buzz_or_n(N) :-
  divisible_by(N, 3, Fizz),
  divisible_by(N, 5, Buzz),
  fizzbuzz(N, Fizz, Buzz, FB),
  format("~p -> ~p~n", [N, FB]).

main :-
  foreach(between(1,100, N), fizz_buzz_or_n(N)).
```



## PureBasic

See [[FizzBuzz/Basic]]


## Pyret


```pyret
fun fizzbuzz(n :: NumPositive) -> String:
  doc: ```For positive input which is multiples of three return 'Fizz', for the multiples of five return 'Buzz'.
  For numbers which are multiples of both three and five return 'FizzBuzz'. Otherwise, return the number itself.```
  ask:
    | num-modulo(n, 15) == 0 then: "FizzBuzz"
    | num-modulo(n, 3) == 0 then: "Fizz"
    | num-modulo(n, 5) == 0 then: "Buzz"
    | otherwise: num-to-string(n)
  end
where:
  fizzbuzz(1) is "1"
  fizzbuzz(101) is "101"
  fizzbuzz(45) is "FizzBuzz"
  fizzbuzz(33) is "Fizz"
  fizzbuzz(25) is "Buzz"
end

range(1, 101).map(fizzbuzz).each(print)
```



## Python


### Python2: Simple


```python
for i in xrange(1, 101):
    if i % 15 == 0:
        print "FizzBuzz"
    elif i % 3 == 0:
        print "Fizz"
    elif i % 5 == 0:
        print "Buzz"
    else:
        print i
```



### Python3: Simple


```python
for i in range(1, 101):
    if i % 15 == 0:
        print ("FizzBuzz")
    elif i % 3 == 0:
        print ("Fizz")
    elif i % 5 == 0:
        print ("Buzz")
    else:
        print (i)
```


One liner using string concatenation:

```python
for i in range(1,101): print("Fizz"*(i%3==0) + "Buzz"*(i%5==0) or i)
```


One liner another code:

```python
for i in range(100):print(i%3//2*'Fizz'+i%5//4*'Buzz'or i+1)
```


List Comprehensions:

```python

for n in range(1, 100):
    fb = ''.join([ denom[1] if n % denom[0] == 0 else '' for denom in [(3,'fizz'),(5,'buzz')] ])
    print fb if fb else n

```

Another list comprehension:

```python

print (', '.join([(x%3<1)*'Fizz'+(x%5<1)*'Buzz' or str(x) for x in range(1,101)]))

```


===Python: List Comprehension (Python 3)===

```python
[print("FizzBuzz") if i % 15 == 0 else print("Fizz") if i % 3 == 0 else print("Buzz") if i % 5 == 0 else print(i) for i in range(1,101)]
```



### Python: Lazily

You can also create a lazy, unbounded sequence by using generator expressions:

```python
from itertools import cycle, izip, count, islice

fizzes = cycle([""] * 2 + ["Fizz"])
buzzes = cycle([""] * 4 + ["Buzz"])
both = (f + b for f, b in izip(fizzes, buzzes))

# if the string is "", yield the number
# otherwise yield the string
fizzbuzz = (word or n for word, n in izip(both, count(1)))

# print the first 100
for i in islice(fizzbuzz, 100):
    print i
```



Or equivalently, in terms of map, and Python 3 libraries:
{{Works with|Python|3.7}}

```python
'''Fizz buzz'''

from itertools import count, cycle, islice


# fizzBuzz :: () -> Generator [String]
def fizzBuzz():
    '''A non-finite stream of fizzbuzz terms.'''
    return map(
        lambda f, b, n: (f + b) or n,
        cycle([''] * 2 + ['Fizz']),
        cycle([''] * 4 + ['Buzz']),
        map(str, count(1))
    )


# main :: IO ()
def main():
    '''Display of first 100 terms of the fizzbuzz series.
    '''
    print(unlines(
        take(100)(
            fizzBuzz()
        )
    ))


# GENERIC -------------------------------------------------

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


# unlines :: [String] -> String
def unlines(xs):
    '''A single string formed by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```



### Python3.8: With walrus operator



```python
print(*map(lambda n: 'Fizzbuzz '[(i):i+13] if (i := n**4%-15) > -14 else n, range(1,100)))
```



## Q



```Q

{$[0=x mod 15;"FizzBuzz";0=x mod 5;"Buzz";0=x mod 3;"Fizz";string x]} each 1+til 15
```


Or to print the result:

```Q

-1 "\n" sv{$[0=x mod 15;"FizzBuzz";0=x mod 5;"Buzz";0=x mod 3;"Fizz";string x]} each 1+til 15;
```



## R


```R
xx <- x <- 1:100
xx[x %% 3 == 0] <- "Fizz"
xx[x %% 5 == 0] <- "Buzz"
xx[x %% 15 == 0] <- "FizzBuzz"
xx
```


Or, without directly checking for divisibility by 15:

```R
xx <- rep("", 100)
x <- 1:100
xx[x %% 3 == 0] <- paste0(xx[x %% 3 == 0], "Fizz")
xx[x %% 5 == 0] <- paste0(xx[x %% 5 == 0], "Buzz")
xx[xx == ""] <- x[xx == ""]
xx
```


Or, (ab)using the vector recycling rule:

```R
x <- paste(rep("", 100), c("", "", "Fizz"), c("", "", "", "", "Buzz"), sep="")
cat(ifelse(x == "", 1:100, x), "\n")
```


Or, with a more straightforward use of ifelse:

```R
x <- 1:100
ifelse(x %% 15 == 0, 'FizzBuzz',
       ifelse(x %% 5 == 0, 'Buzz',
              ifelse(x %% 3 == 0, 'Fizz', x)))
```



## Racket


```racket
#lang racket

(for ([n (in-range 1 101)])
  (displayln
   (match (gcd n 15)
     [15 "fizzbuzz"]
     [3 "fizz"]
     [5 "buzz"]
     [_ n])))
```



## RapidQ

The [[#BASIC|BASIC]] solutions work with RapidQ, too.
However, here is a bit more esoteric solution using the IIF() function.

```rapidq
FOR i=1 TO 100
    t$ = IIF(i MOD 3 = 0, "Fizz", "") + IIF(i MOD 5 = 0, "Buzz", "")
    PRINT IIF(LEN(t$), t$, i)
NEXT i
```



## Rascal


```rascal
import IO;

public void fizzbuzz() {
   for(int n <- [1 .. 100]){
      fb = ((n % 3 == 0) ? "Fizz" : "") + ((n % 5 == 0) ? "Buzz" : "");
      println((fb == "") ?"<n>" : fb);
   }
}
```



## Raven


```raven
100 each 1 + as n
  ''
  n 3 mod 0 = if 'Fizz' cat
  n 5 mod 0 = if 'Buzz' cat
  dup empty if drop n
  say
```



## REALbasic

See [[FizzBuzz/Basic]]


## Reason


```Reason

let fizzbuzz i =>
  switch (i mod 3, i mod 5) {
  | (0, 0) => "FizzBuzz"
  | (0, _) => "Fizz"
  | (_, 0) => "Buzz"
  | _ => string_of_int i
  };

for i in 1 to 100 {
  print_endline (fizzbuzz i)
};

```



## REBOL

An implementation that concatenates strings and includes a proper code header (title, date, etc.)

```REBOL
REBOL [
	Title: "FizzBuzz"
	URL: http://rosettacode.org/wiki/FizzBuzz
]

; Concatenative. Note use of 'case/all' construct to evaluate all
; conditions. I use 'copy' to allocate a new string each time through
; the loop -- otherwise 'x' would get very long...

repeat i 100 [
	x: copy ""
	case/all [
		0 = mod i 3 [append x "Fizz"]
		0 = mod i 5 [append x "Buzz"]
		"" = x      [x: mold i]
	]
	print x
]
```

Here is an example by Nick Antonaccio.

```REBOL
repeat i 100 [
    print switch/default 0 compose [
        (mod i 15) ["fizzbuzz"]
        (mod i 3)  ["fizz"]
        (mod i 5)  ["buzz"]
    ][i]
]
```


And a minimized version:

```REBOL
repeat i 100[j:""if i // 3 = 0[j:"fizz"]if i // 5 = 0[j: join j"buzz"]if""= j[j: i]print j]
```


The following is presented as a curiosity only, not as an example of good coding practice:

```REBOL
m: func [i d] [0 = mod i d]
spick: func [t x y][either any [not t  "" = t][y][x]]
zz: func [i] [rejoin [spick m i 3 "Fizz" ""  spick m i 5 "Buzz" ""]]
repeat i 100 [print spick z: zz i z i]
```



## Retro

This is a port of some [http://weblog.raganwald.com/2007/01/dont-overthink-fizzbuzz.html Forth code].

```Retro
: fizz?    ( s-f ) 3 mod 0 = ;
: buzz?    ( s-f ) 5 mod 0 = ;
: num?     ( s-f ) dup fizz? swap buzz? or 0 = ;
: ?fizz    ( s-  ) fizz? [ "Fizz" puts ] ifTrue ;
: ?buzz    ( s-  ) buzz? [ "Buzz" puts ] ifTrue ;
: ?num     ( s-  ) num? &putn &drop if ;
: fizzbuzz ( s-  ) dup ?fizz dup ?buzz dup ?num space ;
: all      (  -  ) 100 [ 1+ fizzbuzz ] iter ;
```

It's cleaner to use quotes and combinators though:

```Retro
needs math'
: <fizzbuzz>
  [ 15 ^math'divisor? ] [ drop "FizzBuzz" puts ] when
  [  3 ^math'divisor? ] [ drop "Fizz"     puts ] when
  [  5 ^math'divisor? ] [ drop "Buzz"     puts ] when putn ;
: fizzbuzz cr 100 [ 1+ <fizzbuzz> space ] iter ;
```



## REXX


This version's program logic closely mirrors the problem statement:
===three IF-THEN===

```rexx
/*REXX program displays numbers  1 ──► 100  (some transformed) for the FizzBuzz problem.*/
                                                 /*╔═══════════════════════════════════╗*/
  do j=1  to 100;      z=  j                     /*║                                   ║*/
  if j//3    ==0  then z= 'Fizz'                 /*║  The divisors  (//)  of the  IFs  ║*/
  if j//5    ==0  then z= 'Buzz'                 /*║  must be in ascending order.      ║*/
  if j//(3*5)==0  then z= 'FizzBuzz'             /*║                                   ║*/
  say right(z, 8)                                /*╚═══════════════════════════════════╝*/
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

'''output'''
<pre style="height:40ex">
       1
       2
    Fizz
       4
    Buzz
    Fizz
       7
       8
    Fizz
    Buzz
      11
    Fizz
      13
      14
FizzBuzz
      16
      17
    Fizz
      19
    Buzz
    Fizz
      22
      23
    Fizz
    Buzz
      26
    Fizz
      28
      29
FizzBuzz
      31
      32
    Fizz
      34
    Buzz
    Fizz
      37
      38
    Fizz
    Buzz
      41
    Fizz
      43
      44
FizzBuzz
      46
      47
    Fizz
      49
    Buzz
    Fizz
      52
      53
    Fizz
    Buzz
      56
    Fizz
      58
      59
FizzBuzz
      61
      62
    Fizz
      64
    Buzz
    Fizz
      67
      68
    Fizz
    Buzz
      71
    Fizz
      73
      74
FizzBuzz
      76
      77
    Fizz
      79
    Buzz
    Fizz
      82
      83
    Fizz
    Buzz
      86
    Fizz
      88
      89
FizzBuzz
      91
      92
    Fizz
      94
    Buzz
    Fizz
      97
      98
    Fizz
    Buzz

```


===SELECT-WHEN===
This version is a different form, but essentially identical to the   '''IF-THEN'''   (above),

but doesn't require the use of a temporary variable to hold/contain the output.

```rexx
/*REXX program displays numbers  1 ──► 100  (some transformed) for the FizzBuzz problem.*/
                                                 /*╔═══════════════════════════════════╗*/
  do j=1  to 100                                 /*║                                   ║*/
      select                                     /*║                                   ║*/
      when j//15==0  then say 'FizzBuzz'         /*║ The divisors  (//)  of the  WHENs ║*/
      when j//5 ==0  then say '    Buzz'         /*║ must be in  descending  order.    ║*/
      when j//3 ==0  then say '    Fizz'         /*║                                   ║*/
      otherwise           say right(j, 8)        /*╚═══════════════════════════════════╝*/
      end   /*select*/
  end       /*j*/                                /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.

===two IF-THEN===
This version lends itself to expansion   (such as using   '''Jazz'''   for multiples of   '''7''').

```rexx
/*REXX program displays numbers  1 ──► 100  (some transformed) for the FizzBuzz problem.*/

   do j=1  for 100;  _=
   if j//3 ==0  then _=_'Fizz'
   if j//5 ==0  then _=_'Buzz'
/* if j//7 ==0  then _=_'Jazz' */                /* ◄─── note that this is a comment.   */
   say right(word(_ j,1),8)
   end   /*j*/                                   /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.

==="geek" version===

```rexx
/*REXX program displays numbers  1 ──► 100  (some transformed) for the FizzBuzz problem.*/
                                                 /* [↓]  concise, but somewhat obtuse.  */
  do j=1  for 100
  say right(word(word('Fizz', 1+(j//3\==0))word('Buzz', 1+(j//5\==0)) j, 1), 8)
  end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version.




## Ring


```ring

for n = 1 to 100
    if n % 15 = 0 see "" + n + " = " + "FizzBuzz"+ nl
    but n % 5 = 0 see "" + n + " = " + "Buzz" + nl
    but n % 3 = 0 see "" + n + " = " + "Fizz" + nl
    else see "" + n + " = " + n + nl ok
next

```



## Robotic


```robotic

set "local1" to 1
: "loop"
wait for 10
if "('local1' % 15)" = 0 then "fizzbuzz"
if "('local1' % 3)" = 0 then "fizz"
if "('local1' % 5)" = 0 then "buzz"
* "&local1&"
: "inc"
inc "local1" by 1
if "local1" <= 100 then "loop"
goto "done"

: "fizzbuzz"
* "FizzBuzz"
goto "inc"

: "fizz"
* "Fizz"
goto "inc"

: "buzz"
* "Buzz"
goto "inc"

: "done"
end

```


The '''wait for 10''' function is not really necessary, but it helps to slow down the output.


## Rockstar


 Midnight takes your heart and your soul
 While your heart is as high as your soul
 Put your heart without your soul into your heart

 Give back your heart


 Desire is a lovestruck ladykiller
 My world is nothing
 Fire is ice
 Hate is water
 Until my world is Desire,
 Build my world up
 If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing
 Shout "FizzBuzz!"
 Take it to the top

 If Midnight taking my world, Fire is nothing
 Shout "Fizz!"
 Take it to the top

 If Midnight taking my world, Hate is nothing
 Say "Buzz!"
 Take it to the top

 Whisper my world


## Ruby


```ruby
1.upto(100) do |n|
  print "Fizz" if a = (n % 3).zero?
  print "Buzz" if b = (n % 5).zero?
  print n unless (a || b)
  puts
end
```

A bit more straightforward:

```ruby
(1..100).each do |n|
  puts if (n % 15).zero?
    "FizzBuzz"
  elsif (n % 5).zero?
    "Buzz"
  elsif (n % 3).zero?
    "Fizz"
  else
    n
  end
end
```

Enumerable#Lazy and classes:

We can grab the first n fizz/buzz/fizzbuzz numbers in a list with a user defined function (filter_map), starting at the number we desire

i.e, grabbing the first 10 fizz numbers starting from 30, fizz = Fizz.new(30,10) #=> [30, 33, 36, 39, 42, 45, 48, 51, 54, 57]

```ruby

class Enumerator::Lazy
  def filter_map
    Lazy.new(self) do |holder, *values|
      result = yield *values
      holder << result if result
    end
  end
end

class Fizz
  def initialize(head, tail)
    @list = (head..Float::INFINITY).lazy.filter_map{|i| i if i % 3 == 0}.first(tail)
  end

  def fizz?(num)
    search = @list
    search.include?(num)
  end

  def drop(num)
    list = @list
    list.delete(num)
  end

  def to_a
    @list.to_a
  end
end

class Buzz
  def initialize(head, tail)
    @list = (head..Float::INFINITY).lazy.filter_map{|i| i if i % 5 == 0}.first(tail)
  end

  def buzz?(num)
    search = @list
    search.include?(num)
  end

  def drop(num)
    list = @list
    list.delete(num)
  end

  def to_a
    @list.to_a
  end
end

class FizzBuzz
  def initialize(head, tail)
    @list = (head..Float::INFINITY).lazy.filter_map{|i| i if i % 15 == 0}.first(tail)
  end

  def fizzbuzz?(num)
    search = @list
    search.include?(num)
  end

  def to_a
    @list.to_a
  end

  def drop(num)
    list = @list
    list.delete(num)
  end
end
stopper = 100
@fizz = Fizz.new(1,100)
@buzz = Buzz.new(1,100)
@fizzbuzz = FizzBuzz.new(1,100)
def min(v, n)
  if v == 1
    puts "Fizz: #{n}"
    @fizz::drop(n)
  elsif v == 2
    puts "Buzz: #{n}"
    @buzz::drop(n)
  else
    puts "FizzBuzz: #{n}"
    @fizzbuzz::drop(n)
  end
end
(@fizz.to_a & @fizzbuzz.to_a).map{|d| @fizz::drop(d)}
(@buzz.to_a & @fizzbuzz.to_a).map{|d| @buzz::drop(d)}
while @fizz.to_a.min < stopper or @buzz.to_a.min < stopper or @fizzbuzz.to_a.min < stopper
  f, b, fb = @fizz.to_a.min, @buzz.to_a.min, @fizzbuzz.to_a.min
  min(1,f)  if f < fb and f < b
  min(2,b)  if b < f and b < fb
  min(0,fb) if fb < b and fb < f
end
```


An example using string interpolation:

```ruby
(1..100).each do |n|
  v = "#{"Fizz" if n % 3 == 0}#{"Buzz" if n % 5 == 0}"
  puts v.empty? ? n : v
end
```


Interpolation inspired one-liner:

```ruby
1.upto(100) { |n| puts "#{'Fizz' if n % 3 == 0}#{'Buzz' if n % 5 == 0}#{n if n % 3 != 0 && n % 5 != 0}" }
```


An example using append:

```ruby
1.upto 100 do |n|
  r = ''
  r << 'Fizz' if n % 3 == 0
  r << 'Buzz' if n % 5 == 0
  r << n.to_s if r.empty?
  puts r
end
```


Yet another solution:
<lang>1.upto(100) { |i| puts "#{[:Fizz][i%3]}#{[:Buzz][i%5]}"[/.+/] || i }
```


Yet another solution:

```ruby
1.upto(100){|i|puts'FizzBuzz '[n=i**4%-15,n+13]||i}
```


Used Enumerable#cycle:

```ruby
f = [nil, nil, :Fizz].cycle
b = [nil, nil, nil, nil, :Buzz].cycle
(1..100).each do |i|
  puts "#{f.next}#{b.next}"[/.+/] || i
end
```


After beforehand preparing the Array which put the number from 1 to 100, it processes.

```ruby
seq = *0..100
{Fizz:3, Buzz:5, FizzBuzz:15}.each{|k,n| n.step(100,n){|i|seq[i]=k}}
puts seq.drop(1)
```


Monkeypatch example:

```ruby
class Integer
  def fizzbuzz
    v = "#{"Fizz" if self % 3 == 0}#{"Buzz" if self % 5 == 0}"
    v.empty? ? self : v
  end
end

puts *(1..100).map(&:fizzbuzz)
```


Without mutable variables or inline printing.

```ruby
fizzbuzz = ->(i) do
  (i%15).zero? and next "FizzBuzz"
  (i%3).zero?  and next "Fizz"
  (i%5).zero?  and next "Buzz"
  i
end

puts (1..100).map(&fizzbuzz).join("\n")
```

[[Jump anywhere#Ruby]] has a worse example of FizzBuzz, using a continuation!


## Ruby with RSpec


This is a solution to FizzBuzz using Test-Driven Development (In this case, with Ruby and RSpec). You will need to set up the correct file structure first, with /lib and /spec directories in your root.

Your spec/fizzbuzz_spec.rb file should like this:


```ruby

require 'fizzbuzz'

describe 'FizzBuzz' do
  context 'knows that a number is divisible by' do
    it '3' do
      expect(is_divisible_by_three?(3)).to be_true
    end
    it '5' do
      expect(is_divisible_by_five?(5)).to be_true
    end
    it '15' do
      expect(is_divisible_by_fifteen?(15)).to be_true
    end
  end
  context 'knows that a number is not divisible by' do
    it '3' do
      expect(is_divisible_by_three?(1)).not_to be_true
    end
    it '5' do
      expect(is_divisible_by_five?(1)).not_to be_true
    end
    it '15' do
      expect(is_divisible_by_fifteen?(1)).not_to be_true
    end
  end
  context 'while playing the game it returns' do
    it 'the number' do
      expect(fizzbuzz(1)).to eq 1
    end
    it 'Fizz' do
      expect(fizzbuzz(3)).to eq 'Fizz'
    end
    it 'Buzz' do
      expect(fizzbuzz(5)).to eq 'Buzz'
    end
    it 'FizzBuzz' do
      expect(fizzbuzz(15)).to eq 'FizzBuzz'
    end
  end
end

```


There are many ways to get these tests to pass. Here is an example solution of what your lib/fizzbuzz.rb file could look like:


```ruby

def fizzbuzz(number)
  return 'FizzBuzz' if is_divisible_by_fifteen?(number)
  return 'Buzz' if is_divisible_by_five?(number)
  return 'Fizz' if is_divisible_by_three?(number)
  number
end

def is_divisible_by_three?(number)
  is_divisible_by(number, 3)
end

def is_divisible_by_five?(number)
  is_divisible_by(number, 5)
end

def is_divisible_by_fifteen?(number)
  is_divisible_by(number, 15)
end

def is_divisible_by(number, divisor)
  number % divisor == 0
end


```


When writing Test Driven code, it's important to remember that you should use the Red, Green, Refactor cycle. Simply writing each of these code snippets independently would go against everything TDD is about. Here is a good video that takes you through the process of writing this [https://www.youtube.com/watch?v=CHTep2zQVAc&feature=youtu.be FizzBuzz implementation using Ruby & RSpec].


## Run BASIC

See [[FizzBuzz/Basic]]


## Rust

A version using an iterator and immutable data:


```rust
use std::borrow::Cow; // Allows us to avoid unnecessary allocations
fn main() {
    (1..101).map(|n| match (n % 3, n % 5) {
        (0, 0) => "FizzBuzz".into(),
        (0, _) => "Fizz".into(),
        (_, 0) => "Buzz".into(),
        _ => Cow::from(n.to_string())
    }).for_each(|n| println!("{}", n));
}

```


Or a version unwrapping the iterator into a loop:


```rust

use std::borrow::Cow;
fn main() {
    for i in 1..101 {
        println!("{}", match (i % 3, i % 5) {
            (0, 0) => "FizzBuzz".into(),
            (0, _) => "Fizz".into(),
            (_, 0) => "Buzz".into(),
            _ => Cow::from(i.to_string()),
        });
    }
}
```


Or the ultimate optimized version with hardcoded output, no standard library or main function, and direct assembly syscalls to write to stdout.

```rust
 #![no_std]
#![feature(asm, lang_items, libc, no_std, start)]

extern crate libc;

const LEN: usize = 413;
static OUT: [u8; LEN] = *b"\
    1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n\
    16\n17\nFizz\n19\nBuzz\nFizz\n22\n23\nFizz\nBuzz\n26\nFizz\n28\n29\nFizzBuzz\n\
    31\n32\nFizz\n34\nBuzz\nFizz\n37\n38\nFizz\nBuzz\n41\nFizz\n43\n44\nFizzBuzz\n\
    46\n47\nFizz\n49\nBuzz\nFizz\n52\n53\nFizz\nBuzz\n56\nFizz\n58\n59\nFizzBuzz\n\
    61\n62\nFizz\n64\nBuzz\nFizz\n67\n68\nFizz\nBuzz\n71\nFizz\n73\n74\nFizzBuzz\n\
    76\n77\nFizz\n79\nBuzz\nFizz\n82\n83\nFizz\nBuzz\n86\nFizz\n88\n89\nFizzBuzz\n\
    91\n92\nFizz\n94\nBuzz\nFizz\n97\n98\nFizz\nBuzz\n";

#[start]
fn start(_argc: isize, _argv: *const *const u8) -> isize {
    unsafe {
        asm!(
            "
            mov $$1, %rax
            mov $$1, %rdi
            mov $0, %rsi
            mov $1, %rdx
            syscall
            "
            :
            : "r" (&OUT[0]) "r" (LEN)
            : "rax", "rdi", "rsi", "rdx"
            :
        );
    }
    0
}

#[lang = "eh_personality"] extern fn eh_personality() {}
#[lang = "panic_fmt"] extern fn panic_fmt() {}
```



## Salmon


```Salmon
iterate (x; [1...100])
  ((x % 15 == 0) ? "FizzBuzz" :
   ((x % 3 == 0) ? "Fizz" :
    ((x % 5 == 0) ? "Buzz" : x)))!;
```

or

```Salmon
iterate (x; [1...100])
  {
    if (x % 15 == 0)
        "FizzBuzz"!
    else if (x % 3 == 0)
        "Fizz"!
    else if (x % 5 == 0)
        "Buzz"!
    else
        x!;
  };
```



## SAS


```SAS
data _null_;
  do i=1 to 100;
    if mod(i,15)=0 then put "FizzBuzz";
    else if mod(i,5)=0 then put "Buzz";
    else if mod(i,3)=0 then put "Fizz";
    else put i;
  end;
run;
```



## Sather


```sather
class MAIN is
  main is
    loop i ::= 1.upto!(100);
      s:STR := "";
      if i % 3 = 0 then s := "Fizz"; end;
      if i % 5 = 0 then s := s + "Buzz"; end;
      if s.length > 0 then
        #OUT + s + "\n";
      else
        #OUT + i + "\n";
      end;
    end;
  end;
end;
```



## Scala

{{libheader|Scala}}


### Idiomatic scala code


```scala
object FizzBuzz extends App {
  1 to 100 foreach { n =>
    println((n % 3, n % 5) match {
      case (0, 0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case _ => n
    })
  }
}
```


===Geeky over-generalized solution ☺===

```scala
def replaceMultiples(x: Int, rs: (Int, String)*): Either[Int, String] =
  rs map { case (n, s) => Either cond(x % n == 0, s, x)} reduceLeft ((a, b) =>
    a fold(_ => b, s => b fold(_ => a, t => Right(s + t))))

def fizzbuzz = replaceMultiples(_: Int, 3 -> "Fizz", 5 -> "Buzz") fold(_.toString, identity)

1 to 100 map fizzbuzz foreach println
```


===By a two-liners geek===

```scala
def f(n: Int, div: Int, met: String, notMet: String): String = if (n % div == 0) met else notMet
for (i <- 1 to 100) println(f(i, 15, "FizzBuzz", f(i, 3, "Fizz", f(i, 5, "Buzz", i.toString))))
```


===One-liner geek===

```scala
for (i <- 1 to 100) println(Seq(15 -> "FizzBuzz", 3 -> "Fizz", 5 -> "Buzz").find(i % _._1 == 0).map(_._2).getOrElse(i))
```



### Functional Scala


```scala
def fizzbuzz(l: List[String], n: Int, s: String) = if (l.head.toInt % n == 0) l :+ s else l
def fizz(l: List[String]) = fizzbuzz(l, 3, "Fizz")
def buzz(l: List[String]) = fizzbuzz(l, 5, "Buzz")
def headOrTail(l: List[String]) = if (l.tail.size == 0) l.head else l.tail.mkString
Stream.from(1).take(100).map(n => List(n.toString)).map(fizz).map(buzz).map(headOrTail).foreach(println)
```



## Scheme


```scheme
(do ((i 1 (+ i 1)))
    ((> i 100))
    (display
      (cond ((= 0 (modulo i 15)) "FizzBuzz")
            ((= 0 (modulo i 3))  "Fizz")
            ((= 0 (modulo i 5))  "Buzz")
            (else                i)))
    (newline))
```



Using a recursive procedure.

```scheme
(define (fizzbuzz x y)
  (println
    (cond (( = (modulo x 15) 0 ) "FizzBuzz")
          (( = (modulo x 3) 0 ) "Fizz")
          (( = (modulo x 5) 0 ) "Buzz")
          (else x)))

    (if (< x y) (fizzbuzz (+ x 1) y)))

(fizzbuzz 1 100)
```



## Sed


```sed
#n
# doesn't work if there's no input
# initialize counters (0 = empty) and value
s/.*/  0/
: loop
# increment counters, set carry
s/^\(a*\) \(b*\) \([0-9][0-9]*\)/\1a \2b \3@/
# propagate carry
: carry
s/ @/ 1/
s/9@/@0/
s/8@/9/
s/7@/8/
s/6@/7/
s/5@/6/
s/4@/5/
s/3@/4/
s/2@/3/
s/1@/2/
s/0@/1/
/@/b carry
# save state
h
# handle factors
s/aaa/Fizz/
s/bbbbb/Buzz/
# strip value if any factor
/z/s/[0-9]//g
# strip counters and spaces
s/[ab ]//g
# output
p
# restore state
g
# roll over counters
s/aaa//
s/bbbbb//
# loop until value = 100
/100/q
b loop
```


Using <tt>seq</tt>:

```sed

seq 100 | sed '/.*[05]$/s//Buzz/;n;s//Buzz/;n;s//Buzz/;s/^[0-9]*/Fizz/'

```



###  GNU sed

GNU sed has ''first~step'' address expression that matches every ''step''th line. This makes following one-liners possible.

Using <tt>seq</tt>:


```sed

seq 100 | sed '0~3 s/.*/Fizz/; 0~5 s/[0-9]*$/Buzz/'

```


Using <tt>yes</tt>:

```sed

yes | sed -n '0~3s/y/Fizz/;0~5s/y*$/Buzz/;tx;=;b;:x;p;100q'

```


Using the option ''-z (--zero-data)'' first introduced in GNU sed 4.2.2 (2012-12-22):

```sed

sed -nz '0~3s/^/Fizz/;0~5s/$/Buzz/;tx;=;b;:x;p;100q' /dev/zero | sed 'y/\c@/\n/'

```

Second invocation of ''sed'' translates null characters to newlines. The same could be achieved with <tt>tr \\0 \\n</tt>


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 100 do
      if number rem 15 = 0 then
        writeln("FizzBuzz");
      elsif number rem 5 = 0 then
        writeln("Buzz");
      elsif number rem 3 = 0 then
        writeln("Fizz");
      else
        writeln(number);
      end if;
    end for;
  end func;
```



## SequenceL



```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Sequence.sl>;

main(args(2)) :=
	let
		result[i] :=
				"FizzBuzz" when i mod 3 = 0 and i mod 5 = 0
			else
				"Fizz" when i mod 3 = 0
			else
				"Buzz" when i mod 5 = 0
			else
				intToString(i)
			foreach i within 1 ... 100;
	in
		delimit(result, '\n');
```



## Shen


```Shen
(define fizzbuzz
  101 -> (nl)
  N -> (let divisible-by? (/. A B (integer? (/ A B)))
         (cases (divisible-by? N 15) (do (output "Fizzbuzz!~%")
                                         (fizzbuzz (+ N 1)))
                (divisible-by? N 3) (do (output "Fizz!~%")
                                        (fizzbuzz (+ N 1)))
                (divisible-by? N 5) (do (output "Buzz!~%")
                                        (fizzbuzz (+ N 1)))
                true (do (output (str N))
                         (nl)
                         (fizzbuzz (+ N 1))))))

(fizzbuzz 1)
```



###  Alternative showing off other features like prolog integration and guards


```Shen
(defprolog fizz
  0 <-- (is _ (output "Fizz"));
  N <-- (when (> N 0)) (is N1 (- N 3)) (fizz N1);
)

(defprolog buzz
  0 <-- (is _ (output "Buzz"));
  N <-- (when (> N 0)) (is N1 (- N 5)) (buzz N1);
)

(define none
  [] -> true
  [true | _] -> false
  [_ | B] -> (none B)
)

(define fizzbuzz
  N M -> (nl) where (> N M)
  N M -> (do
    (if (none [(prolog? (receive N) (fizz N)) (prolog? (receive N) (buzz N))])
      (output (str N))
      (output "!")
    )
    (nl)
    (fizzbuzz (+ N 1) M)
  )
)

(fizzbuzz 1 100)

```



## Sidef

Structured:

```ruby
{ |i|
    if (i %% 3) {
        print "Fizz"
        i %% 5 && print "Buzz"
        print "\n"
    }
    elsif (i %% 5) { say "Buzz" }
    else  { say i }
} * 100
```


Declarative:

```ruby
func fizzbuzz({ _ %% 15 }) { "FizzBuzz" }
func fizzbuzz({ _ %%  5 }) {     "Buzz" }
func fizzbuzz({ _ %%  3 }) {     "Fizz" }
func fizzbuzz(        n  ) {          n }

for n in (1..100) { say fizzbuzz(n) }
```


One-liner:

```ruby
{|i|say "#{<Fizz>[i%3]}#{<Buzz>[i%5]}"||i}*100
```



## Simula


```simula
begin
    integer i;
    for i := 1 step 1 until 100 do
    begin
        boolean fizzed;
        fizzed := 0 = mod(i, 3);
        if fizzed then
            outtext("Fizz");
        if mod(i, 5) = 0 then
            outtext("Buzz")
        else if not fizzed then
            outint(i, 3);
        outimage
    end;
end
```



## SkookumScript

Answer by printing out one of the 4 alternatives:

```javascript

1.to 100
  [
  println(
    if idx.mod(15) = 0 ["FizzBuzz"]
      idx.mod(3) = 0 ["Fizz"]
      idx.mod(5) = 0 ["Buzz"]
      else [idx])
  ]

```


Answer by building up a string:

```javascript

1.to 100
  [
  !str: ""
  if idx.mod(3) = 0 [str += "Fizz"]
  if idx.mod(5) = 0 [str += "Buzz"]
  println(if str.empty? [idx] else [str])
  ]

```


Or doing initial bind in one step:

```javascript

1.to 100
  [
  !str: if idx.mod(3) = 0 ["Fizz"] else [""]
  if idx.mod(5) = 0 [str += "Buzz"]
  println(if str.empty? [idx] else [str])
  ]

```



## Slate


```slate
n@(Integer traits) fizzbuzz
[
  output ::= ((n \\ 3) isZero ifTrue: ['Fizz'] ifFalse: ['']) ; ((n \\ 5) isZero ifTrue: ['Buzz'] ifFalse: ['']).
  output isEmpty ifTrue: [n printString] ifFalse: [output]
].
1 to: 100 do: [| :i | inform: i fizzbuzz]
```



## Small

See [[FizzBuzz/EsoLang]]


## Smalltalk

Since only GNU Smalltalk supports file-based programming, we'll be using its syntax.

```smalltalk
Integer extend [
    fizzbuzz [
        | fb |
        fb := '%<Fizz|>1%<Buzz|>2' % {
            self \\ 3 == 0.  self \\ 5 == 0 }.
        ^fb isEmpty ifTrue: [ self ] ifFalse: [ fb ]
    ]
]
1 to: 100 do: [ :i | i fizzbuzz displayNl ]
```

A Squeak/Pharo example using the Transcript window:

```smalltalk
(1 to: 100) do:
	[:n |
		((n \\ 3)*(n \\ 5)) isZero
                        ifFalse: [Transcript show: n].
		(n \\ 3) isZero
			ifTrue: [Transcript show: 'Fizz'].
		(n \\ 5) isZero
			ifTrue: [Transcript show: 'Buzz'].
		Transcript cr.]
```

The Squeak/Pharo examples below present possibilities using the powerful classes available. In this example, the dictionary can have as keys pairs of booleans and in the interaction the several boolean patterns select the string to be printed or if the pattern is not found the number itself is printed.

```smalltalk
fizzbuzz := Dictionary with: #(true true)->'FizzBuzz'
                       with: #(true false)->'Fizz'
                       with: #(false true)->'Buzz'.

1 to: 100 do:
	[ :i | Transcript show:
               (fizzbuzz at: {i isDivisibleBy: 3. i isDivisibleBy: 5}
		         ifAbsent: [ i ]); cr]
```

Smalltalk does not have a case-select construct, but a similar effect can be attained using a collection and the #includes: method:

```smalltalk
1 to: 100 do: [:n | |r|
	r := n rem: 15.
	Transcript show: (r isZero
	   ifTrue:['fizzbuzz']
	   ifFalse: [(#(3 6 9 12) includes: r)
		ifTrue:['fizz']
		ifFalse:[((#(5 10) includes: r))
			ifTrue:['buzz']
			ifFalse:[n]]]);
	cr].
```

If the construction of the whole collection is done beforehand, Smalltalk provides a straightforward way of doing because collections can be heterogeneous (may contain any object):

```smalltalk
fbz := (1 to: 100) asOrderedCollection.
 3 to: 100 by:  3 do: [:i | fbz at: i put: 'Fizz'].
 5 to: 100 by:  5 do: [:i | fbz at: i put: 'Buzz'].
15 to: 100 by: 15 do: [:i | fbz at: i put: 'FizzBuzz'].
fbz do: [:i | Transcript show: i; cr].
```

The approach building a dynamic string can be done as well:

```smalltalk
1 to: 100 do: [:i | |fb s|
	fb := {i isDivisibleBy: 3. i isDivisibleBy: 5. nil}.
	fb at: 3 put: (fb first | fb second) not.
	s := '<1?Fizz:><2?Buzz:><3?{1}:>' format: {i printString}.
	Transcript show: (s expandMacrosWithArguments: fb); cr].
```



## SNOBOL4

Merely posting a solution by Daniel Lyons

```snobol4
        I = 1
LOOP    FIZZBUZZ = ""
        EQ(REMDR(I, 3), 0)              :F(TRY_5)
        FIZZBUZZ = FIZZBUZZ "FIZZ"
TRY_5   EQ(REMDR(I, 5), 0)              :F(DO_NUM)
        FIZZBUZZ = FIZZBUZZ "BUZZ"
DO_NUM  IDENT(FIZZBUZZ, "")             :F(SHOW)
        FIZZBUZZ = I
SHOW    OUTPUT = FIZZBUZZ
        I = I + 1
        LE(I, 100)                      :S(LOOP)
END
```



## SNUSP

See [[FizzBuzz/EsoLang]]


## SQL

{{libheader|SQL}}

### Oracle SQL


```sql
SELECT CASE
    WHEN MOD(level,15)=0 THEN 'FizzBuzz'
    WHEN MOD(level,3)=0 THEN 'Fizz'
    WHEN MOD(level,5)=0 THEN 'Buzz'
    ELSE TO_CHAR(level)
    END FizzBuzz
    FROM dual
    CONNECT BY LEVEL <= 100;
```

Or using Oracle's DECODE and NVL:

```sql
SELECT nvl(decode(MOD(level,3),0,'Fizz')||decode(MOD(level,5),0,'Buzz'),level)
FROM dual
CONNECT BY level<=100;
```



### PostgreSQL specific


```sql
SELECT i, fizzbuzz
  FROM
    (SELECT i,
            CASE
              WHEN i % 15 = 0 THEN 'FizzBuzz'
              WHEN i %  5 = 0 THEN 'Buzz'
              WHEN i %  3 = 0 THEN 'Fizz'
              ELSE NULL
            END AS fizzbuzz
       FROM generate_series(1,100) AS i) AS fb
 WHERE fizzbuzz IS NOT NULL;
```


Using Generate_Series and tables only:

```sql
SELECT COALESCE(FIZZ || BUZZ, FIZZ, BUZZ, OUTPUT) AS FIZZBUZZ FROM
(SELECT GENERATE_SERIES AS FULL_SERIES, TO_CHAR(GENERATE_SERIES,'99') AS OUTPUT
FROM GENERATE_SERIES(1,100)) F LEFT JOIN
(SELECT TEXT 'Fizz' AS FIZZ, GENERATE_SERIES AS FIZZ_SERIES FROM GENERATE_SERIES(0,100,3)) FIZZ ON
FIZZ.FIZZ_SERIES = F.FULL_SERIES LEFT JOIN
(SELECT TEXT 'Buzz' AS BUZZ, GENERATE_SERIES AS BUZZ_SERIES FROM GENERATE_SERIES(0,100,5)) BUZZ ON
BUZZ.BUZZ_SERIES = F.FULL_SERIES;
```


===Recursive Common Table Expressions (MSSQL 2005+)===

```sql
WITH nums (n, fizzbuzz ) AS (
	SELECT 1, CONVERT(nvarchar, 1) UNION ALL
	SELECT
		(n + 1) as n1,
		CASE
			WHEN (n + 1) % 15 = 0 THEN 'FizzBuzz'
			WHEN (n + 1) % 3  = 0 THEN 'Fizz'
			WHEN (n + 1) % 5  = 0 THEN 'Buzz'
			ELSE CONVERT(nvarchar, (n + 1))
		END
	FROM nums WHERE n < 100
)
SELECT n, fizzbuzz FROM nums
ORDER BY n ASC
OPTION ( MAXRECURSION 100 )
```


### Generic SQL using a join

This should work in most RDBMSs, but you may need to change <tt>MOD(i,divisor)</tt> to <tt>i % divisor</tt>.

```SQL
-- Load some numbers
CREATE TABLE numbers(i INTEGER);
INSERT INTO numbers VALUES(1);
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
INSERT INTO numbers SELECT i + (SELECT MAX(i) FROM numbers) FROM numbers;
-- Define the fizzes and buzzes
CREATE TABLE fizzbuzz (message VARCHAR(8), divisor INTEGER);
INSERT INTO fizzbuzz VALUES('fizz',      3);
INSERT INTO fizzbuzz VALUES('buzz',      5);
INSERT INTO fizzbuzz VALUES('fizzbuzz', 15);
-- Play fizzbuzz
SELECT COALESCE(max(message),CAST(i AS VARCHAR(99))) as result
FROM numbers LEFT OUTER JOIN fizzbuzz ON MOD(i,divisor) = 0
GROUP BY i
HAVING i <= 100
ORDER BY i;
-- Tidy up
DROP TABLE fizzbuzz;
DROP TABLE numbers;
```



## Squirrel


```javascript
function Fizzbuzz(n) {
    for (local i = 1; i <= n; i += 1) {
        if (i % 15 == 0)
            print ("FizzBuzz\n")
        else if (i % 5 == 0)
            print ("Buzz\n")
        else if (i % 3 == 0)
            print ("Fizz\n")
        else {
            print (i + "\n")
        }
    }
}
Fizzbuzz(100);
```



## Stata


```stata
program define fizzbuzz
	args n
	forvalues i = 1/`n' {
		if mod(`i',15) == 0 {
			display "FizzBuzz"
		}
		else if mod(`i',5) == 0 {
			display "Buzz"
		}
		else if mod(`i',3) == 0 {
			display "Fizz"
		}
		else {
			display `i'
		}
	}
end
```



## Swift


###  using a switch statement


```swift
for i in 1...100 {
    switch (i % 3, i % 5) {
    case (0, 0):
        print("FizzBuzz")
    case (0, _):
        print("Fizz")
    case (_, 0):
        print("Buzz")
    default:
        print(i)
    }
}
```


###  using two if statements and an Optional


```swift
for i in 1...100{
    var s:String?
    if i%3==0{s="Fizz"}
    if i%5==0{s=(s ?? "")+"Buzz"}
    print(s ?? i)
}
```



## Symsyn


```symsyn

| FizzBuzz

 1 I
 if I LE 100
    mod I 3 X
    mod I 5 Y
    if X EQ 0
       'FIZZ' $S
       if Y EQ 0
          + 'BUZZ' $S
       endif
    else
       if Y EQ 0
          'BUZZ' $S
       else
          ~ I $S
       endif
    endif
    $S []
    + I
    goif
 endif

```



## Tailspin


```tailspin

templates fizz
  $ mod 3 -> #
  <0> 'Fizz' !
end fizz

templates buzz
  $ mod 5 -> #
  <0> 'Buzz' !
end buzz

[ 1..100 -> '$->fizz;$->buzz;' ] -> [i](<''> $i ! <> $ !)... -> '$;
' -> !OUT::write

```



## Tcl


```tcl
proc fizzbuzz {n {m1 3} {m2 5}} {
    for {set i 1} {$i <= $n} {incr i} {
        set ans ""
        if {$i % $m1 == 0} {append ans Fizz}
        if {$i % $m2 == 0} {append ans Buzz}
        puts [expr {$ans eq "" ? $i : $ans}]
    }
}
fizzbuzz 100
```

The following example shows Tcl's substitution mechanism that allows to concatenate the results of two successive commands into a string:

```tcl
while {[incr i] < 101} {
    set fb [if {$i % 3 == 0} {list Fizz}][if {$i % 5 == 0} {list Buzz}]
    if {$fb ne ""} {puts $fb} {puts $i}
}
```

This version uses list rotation, so avoiding an explicit mod operation:

```tcl
set f [lrepeat 5 "Fizz" {$i} {$i}]
foreach i {5 10} {lset f $i "Buzz"};lset f 0 "FizzBuzz"
for {set i 1} {$i <= 100} {incr i} {
    puts [subst [lindex [set f [list {*}[lassign $f ff] $ff]] 0]]
}
```


=={{header|TI-83 BASIC}}==
See [[FizzBuzz/Basic]]

=={{header|TI-83 Hex Assembly}}==
See [[FizzBuzz/Assembly]]


## TransFORTH


```forth
: FIZZBUZZ
101 1 DO
I 15 MOD 0 = IF
PRINT " FIZZBUZZ "
ELSE I 3 MOD 0 = IF
PRINT " FIZZ "
ELSE I 5 MOD 0 = IF
PRINT " BUZZ "
ELSE I . THEN THEN THEN
CR LOOP ;
```



## Turing


```Turing
setscreen("nocursor,noecho")

for i : 1 .. 100
    if i mod 15 = 0 then
        put "Fizzbuzz" ..
    elsif i mod 5 = 0 then
        put "Buzz" ..
    elsif i mod 3 = 0 then
        put "Fizz" ..
    else
        put i ..
    end if
end for
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP n=1,100
mod=MOD (n,15)
SELECT mod
CASE 0
PRINT n," FizzBuzz"
CASE 3,6,9,12
PRINT n," Fizz"
CASE 5,10
PRINT n," Buzz"
DEFAULT
PRINT n
ENDSELECT
ENDLOOP
```



## TXR


```shell
$ txr -p "(mapcar (op if @1 @1 @2) (repeat '(nil nil fizz nil buzz fizz nil nil fizz buzz nil fizz nil nil fizzbuzz)) (range 1 100))"
```



## UNIX Shell

This solution should work with any Bourne-compatible shell: <!-- http://ideone.com/usJXGo -->

```bash
i=1
while expr $i '<=' 100 >/dev/null; do
	w=false
	expr $i % 3 = 0 >/dev/null && { printf Fizz; w=true; }
	expr $i % 5 = 0 >/dev/null && { printf Buzz; w=true; }
	if $w; then echo; else echo $i; fi
	i=`expr $i + 1`
done
```



### Versions for specific shells

The other solutions work with fewer shells.

The next solution requires <code>$(( ))</code> arithmetic expansion,
and it should work with every POSIX shell.
<!--  http://ideone.com/5yZmOz -->

```bash
n=1
while [ 100 -ge n ]; do
  if [ $((n % 15)) -eq 0 ]; then
    echo FizzBuzz
  elif [ $((n % 3)) -eq 0 ]; then
    echo Fizz
  elif [ $((n % 5)) -eq 0 ]; then
    echo Buzz
  else
    echo $n
  fi
  n=$((n + 1))
done
```


The next solution requires the <code>(( ))</code> command from the [[Korn Shell]].
{{works with|pdksh|5.2.14}}

```bash
NUM=1
until ((NUM == 101)) ; do
   if ((NUM % 15 == 0)) ; then
       echo FizzBuzz
   elif ((NUM % 3 == 0)) ; then
       echo Fizz
   elif ((NUM % 5 == 0)) ; then
       echo Buzz
   else
       echo "$NUM"
   fi
   ((NUM = NUM + 1))
done
```


A version using concatenation:
{{works with|bash|3}}

```bash
for ((n=1; n<=100; n++))
do
  fb=''
  [ $(( n % 3 )) -eq 0 ] && fb="${fb}Fizz"
  [ $(( n % 5 )) -eq 0 ] && fb="${fb}Buzz"
  [ -n "${fb}" ] && echo "${fb}" || echo "$n"
done
```


A version using some of the insane overkill of Bash 4:
{{works with|bash|4}}

```bash
command_not_found_handle () {
  local Fizz=3 Buzz=5
  [ $(( $2 % $1 )) -eq 0 ] && echo -n $1 && [ ${!1} -eq 3 ]
}

for i in {1..100}
do
  Fizz $i && ! Buzz $i || echo -n $i
  echo
done
```


Bash one-liner: <!--  http://ideone.com/xMEGFK -->

```bash
for i in {1..100};do ((($i%15==0))&& echo FizzBuzz)||((($i%5==0))&& echo Buzz;)||((($i%3==0))&& echo Fizz;)||echo $i;done
```


=
## C Shell
=

```csh
@ n = 1
while ( $n <= 100 )
  if ($n % 15 == 0) then
    echo FizzBuzz
  else if ($n % 5 == 0) then
    echo Buzz
  else if ($n % 3 == 0) then
    echo Fizz
  else
    echo $n
  endif
  @ n += 1
end
```



## Ursa


```ursa
#
# fizzbuzz
#
decl int i
for (set i 1) (< i 101) (inc i)
        if (= (mod i 3) 0)
                out "fizz" console
        end if
        if (= (mod i 5) 0)
                out "buzz" console
        end if
        if (not (or (= (mod i 3) 0) (= (mod i 5) 0)))
                out i console
        end if
        out endl console
end for
```



## Ursala


```Ursala
#import std
#import nat

fizzbuzz = ^T(&&'Fizz'! not remainder\3,&&'Buzz'! not remainder\5)|| ~&h+ %nP

#show+

main = fizzbuzz*t iota 101
```



## V


```v
[fizzbuzz
    1 [>=] [
     [[15 % zero?] ['fizzbuzz' puts]
      [5 % zero?]  ['buzz' puts]
      [3 % zero?]  ['fizz' puts]
      [true] [dup puts]
    ] when succ
  ] while].
 |100 fizzbuzz
```



### Second try

(a compiler for fizzbuzz)

define a command that will generate a sequence

```v
[seq [] swap dup [zero? not] [rolldown [dup] dip cons rollup pred] while pop pop].
```

create a quote that will return a quote that returns a quote if its argument is an integer (A HOF)

```v
[check [N X F : [[integer?] [[X % zero?] [N F cons] if] if]] view].
```

Create a quote that will make sure that the above quote is applied correctly if given (Number Function)
as arguments.

```v
[func [[N F] : [dup N F check i] ] view map].
```

And apply it

```v
100 seq [
        [15 [pop 'fizzbuzz' puts]]
        [5  [pop 'buzz' puts]]
        [3  [pop 'fizz' puts]]
        [1  [puts]]] [func dup] step
        [i true] map pop
```

the first one is much better :)


## Vala


```vala
int main() {
    for (int i = 1; i <= 100; i++) {
        if (i % 3 == 0) stdout.printf("Fizz\n");
        if (i % 5 == 0) stdout.printf("Buzz\n");
        if (i % 15 == 0) stdout.printf("FizzBuzz\n");
        if (i % 3 != 0 && i % 5 != 0) stdout.printf("%d\n", i);

    }
return 0;;
}
```


## VAX Assembly


```VAX Assembly
                           00000008  0000     1 len	=8
                           00000008  0000     2 msg:	.blkb	len			;output buffer
                           0000000C  0008     3 desc:	.blkl	1			;descriptor lenght field
                           00000000' 000C     4 	.address msg			;pointer to buffer
                           00000012  0010     5 outlen:	.blkw	1
         4C 55 21 0000001A'010E0000' 0012     6 ctr:	.ascid	"!UL"
                                     001D     7
                               0000  001D     8 .entry	start,0
                            52   7C  001F     9 	clrq	r2			;r2+r3 64bit
                            52   D6  0021    10 	incl	r2			;start index 1
                                     0023    11 loop:
                         E2 AF   B4  0023    12 	clrw	desc			;assume not fizz and or buzz
                    55   D7 AF   9E  0026    13 	movab	msg, r5			;pointer to message buffer
             54   50   52   03   7B  002A    14 	ediv	#3,r2,r0,r4		;divr.rl,divd.rq,quo.wl,rem.wl
                            54   D5  002F    15 	tstl	r4			;remainder
                            0B   12  0031    16 	bneq	not_fizz		;not equal zero
                                     0033    17
              85   7A7A6966 8F   D0  0033    18 	movl	#^a"fizz", (r5)+	;add to message
                    CA AF   04   A0  003A    19 	addw2	#4, desc		;and update length
                                     003E    20 not_fizz:
             54   50   52   05   7B  003E    21 	ediv	#5,r2,r0,r4
                            54   D5  0043    22 	tstl	r4
                            0B   12  0045    23 	bneq	not_buzz
                                     0047    24
              85   7A7A7562 8F   D0  0047    25 	movl	#^a"buzz", (r5)+
                    B6 AF   04   A0  004E    26 	addw2	#4, desc
                                     0052    27 not_buzz:
                         B3 AF   B5  0052    28 	tstw	desc			;fizz and or buzz?
                            1B   12  0055    29 	bneq	show_buffer		;neq - yes
                                     0057    30
                    AD AF   08   B0  0057    31 	movw	#len, desc		;fao length limit
                                     005B    32 	$fao_s -			;eql -no
                                     005B    33 		 ctrstr = ctr, -	;show number
                                     005B    34 		 outlen = outlen, -
                                     005B    35 		 outbuf = desc, -
                                     005B    36 		 p1     = r2
                 96 AF   A0 AF   B0  006D    37 	movw	outlen, desc		;characters filled by fao
                                     0072    38 show_buffer:
                         93 AF   7F  0072    39 	pushaq	desc
              00000000'GF   01   FB  0075    40 	calls	#1, g^lib$put_output
           9F 52   00000064 8F   F3  007C    41 	AOBLEQ	#100,r2,loop		;limit.rl, index.ml
                                 04  0084    42 	ret
                                     0085    43 .end	start
```


## VBA


```vb

Option Explicit

Sub FizzBuzz()
Dim Tb(1 To 100) As Variant
Dim i As Integer
    For i = 1 To 100
        Tb(i) = i
        If i Mod 15 = 0 Then
            Tb(i) = "FizzBuzz"
        ElseIf i Mod 5 = 0 Then
            Tb(i) = "Buzz"
        ElseIf i Mod 3 = 0 Then
            Tb(i) = "Fizz"
        End If
    Next
    Debug.Print Join(Tb, vbCrLf)
End Sub
```

As an alternative, testing each number only once:

```vb

Sub FizzBuzz()
    Dim i As Integer
    Dim T(1 To 99) As Variant
    For i = 1 To 99 Step 3
        T(i + 0) = IIf((i + 0) Mod 5 = 0, "Buzz", i)
        T(i + 1) = IIf((i + 1) Mod 5 = 0, "Buzz", i + 1)
        T(i + 2) = IIf((i + 2) Mod 5 = 0, "FizzBuzz", "Fizz")
    Next i
    Debug.Print Join(T, ", ") & ", Buzz"
End Sub

```



## VBScript

{{works with|Windows Script Host|*}}

```VBScript
For i = 1 To 100
	If i Mod 15 = 0 Then
		WScript.Echo "FizzBuzz"
	ElseIf i Mod 5 = 0 Then
		WScript.Echo "Buzz"
	ElseIf i Mod 3 = 0 Then
		WScript.Echo "Fizz"
	Else
		WScript.Echo i
	End If
Next
```



### ==An Alternative==

{{works with|Windows Script Host|*}}

```VBScript
With WScript.StdOut
	For i = 1 To 100
		If i Mod 3 = 0 Then .Write "Fizz"
		If i Mod 5 = 0 Then .Write "Buzz"
		If .Column = 1 Then .WriteLine i Else .WriteLine ""
	Next
End With
```



## Verbexx


```Verbexx
@LOOP init:{@VAR t3 t5; @VAR i = 1} while:(i <= 100) next:{i++}
{
  t3 = (i % 3 == 0);
  t5 = (i % 5 == 0);

  @SAY ( @CASE when:(t3 && t5) { 'FizzBuzz }
               when: t3        { 'Fizz     }
               when: t5        { 'Buzz     }
               else:           { i         }
       );
};
```



## Vim Script


```vim
for i in range(1, 100)
    if i % 15 == 0
        echo "FizzBuzz"
    elseif i % 5 == 0
        echo "Buzz"
    elseif i % 3 == 0
        echo "Fizz"
    else
        echo i
    endif
endfor
```



## Visual Basic .NET

See [[FizzBuzz/Basic]]


## Visual Prolog

<lang>
implement main
   open core, console

class predicates
   fizzbuzz : (integer) -> string procedure (i).

clauses
    fizzbuzz(X) = S :- X mod 15 = 0, S = "FizzBuzz", !.
    fizzbuzz(X) = S :- X mod 5 = 0, S = "Buzz", !.
    fizzbuzz(X) = S :- X mod 3 = 0, S = "Fizz", !.
    fizzbuzz(X) = S :- S = toString(X).

    run() :-
        foreach X = std::fromTo(1,100) do
            write(fizzbuzz(X)), write("\n")
        end foreach,
        succeed.

end implement main

goal
    console::runUtf8(main::run).

```



## Vlang

<lang>fn fizzbuzz(n int) {
  for i := 0; i < n; i++ {
    if i % 15 == 0 {
      println('FizzBuzz')
    } else if i % 3 == 0 {
      println('Fizz')
    } else if i % 5 == 0 {
      println('Buzz')
    } else {
      println(i)
    }
  }
}

fn main() {
  fizzbuzz(50)
}
```



## Wart


```wart
for i 1 (i <= 100) ++i
  prn (if (divides i 15)
            "FizzBuzz"
          (divides i 3)
            "Fizz"
          (divides i 5)
            "Buzz"
          :else
            i)
```



## WDTE


```WDTE>let io =
 import 'io';
let s => import 'stream';

let multiple of n => == (% n of) 0;

let fizzbuzz n => switch n {
	multiple (* 3 5) => 'FizzBuzz';
	multiple 3 => 'Fizz';
	multiple 5 => 'Buzz';
	default => n;
} -- io.writeln io.stdout;

s.range 1 101 -> s.map fizzbuzz -> s.drain;
```



## Whitespace

See [[FizzBuzz/EsoLang]]


## Wortel


```wortel
@each &x!console.log x !*&x?{%%x 15 "FizzBuzz" %%x 5 "Buzz" %%x 3 "Fizz" x} @to 100
```



## X86 Assembly


```x86asm

; x86_64 linux nasm

section .bss
number resb 4

section .data
fizz: db "Fizz"
buzz: db "Buzz"
newLine: db 10

section .text
global _start

_start:

  mov rax, 1      ; initialize counter

  loop:
    push rax
    call fizzBuzz
    pop rax
    inc rax
    cmp rax, 100
    jle loop

  mov rax, 60
  mov rdi, 0
  syscall

fizzBuzz:
  mov r10, rax
  mov r15, 0       ; boolean fizz or buzz
  checkFizz:
    xor rdx, rdx   ; clear rdx for division
    mov rbx, 3
    div rbx
    cmp rdx, 0     ; modulo result here
    jne checkBuzz
    mov r15, 1
    mov rsi, fizz
    mov rdx, 4
    mov rax, 1
    mov rdi, 1
    syscall
  checkBuzz:
    mov rax, r10
    xor rdx, rdx
    mov rbx, 5
    div rbx
    cmp rdx, 0
    jne finishLine
    mov r15, 1
    mov rsi, buzz
    mov rdx, 4
    mov rax, 1
    mov rdi, 1
    syscall
  finishLine:      ; print number if no fizz or buzz
    cmp r15, 1
    je nextLine
    mov rax, r10
    call printNum
    ret
    nextLine:
      mov rsi, newLine
      mov rdx, 1
      mov rax, 1
      mov rdi, 1
      syscall
      ret

printNum:          ; write proper digits into number buffer
  cmp rax, 100
  jl lessThanHundred
  mov byte [number], 49
  mov byte [number + 1], 48
  mov byte [number + 2], 48
  mov rdx, 3
  jmp print

  lessThanHundred: ; get digits to write through division
    xor rdx, rdx
    mov rbx, 10
    div rbx
    add rdx, 48
    cmp rax, 0
    je lessThanTen
    add rax, 48
    mov byte [number], al
    mov byte [number + 1], dl
    mov rdx, 2
    jmp print

  lessThanTen:
    mov byte [number], dl
    mov rdx, 1
  print:
    mov byte [number + rdx], 10   ; add newline
    inc rdx
    mov rax, 1
    mov rdi, 1
    mov rsi, number
    syscall
  ret

```



## XLISP


```lisp
(defun fizzbuzz ()
    (defun fizzb (x y)
        (display (cond
            ((= (mod x 3) (mod x 5) 0) "FizzBuzz")
            ((= (mod x 3) 0) "Fizz")
            ((= (mod x 5) 0) "Buzz")
            (t x)))
        (newline)
        (if (< x y)
            (fizzb (+ x 1) y)))
    (fizzb 1 100))

(fizzbuzz)
```



## XMIDAS


```XMIDAS
startmacro
  loop 100 count
    calc/quiet three ^count 3 modulo
    calc/quiet five ^count 5 modulo
    if ^three eq 0 and ^five eq 0
      say "fizzbuzz"
    elseif ^three eq 0
      say "fizz"
    elseif ^five eq 0
      say "buzz"
    else
      say ^count
    endif
  endloop
endmacro
```



## Xojo


```vb
  For i As Integer = 1 To 100
    If i Mod 3 = 0 And i Mod 5 = 0 Then
      Print("FizzBuzz")
    ElseIf i Mod 3 = 0 Then
      Print("Fizz")
    ElseIf i Mod 5 = 0 Then
      Print("Buzz")
    Else
      Print(Str(i))
    End If
  Next
```

An alternative syntax:

```vb

  For i As Integer = 1 To 100
    Select Case True
      Case i Mod 3 = 0 And i Mod 5 = 0
      Print("FizzBuzz")
    Case i Mod 3 = 0
      Print("Fizz")
    Case i Mod 5 = 0
      Print("Buzz")
    Else
      Print(Str(i))
    End Select
  Next
```



## XPath 2.0


```XPath
for $n in 1 to 100 return
  concat('fizz'[not($n mod 3)], 'buzz'[not($n mod 5)], $n[$n mod 15 = (1,2,4,7,8,11,13,14)])
```

...or alternatively...

```XPath
for $n in 1 to 100 return
  ($n, 'Fizz', 'Buzz', 'FizzBuzz')[number(($n mod 3) = 0) + number(($n mod 5) = 0)*2 + 1]
```



## XPL0


```XPL0
code CrLf=9, IntOut=11, Text=12;
int     N;
[for N:= 1 to 100 do
       [if rem(N/3)=0 then Text(0,"Fizz");
        if rem(N/5)=0 then Text(0,"Buzz")
        else if rem(N/3)#0 then IntOut(0,N);
        CrLf(0);
       ];
]
```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
7
...
89
FizzBuzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz

```



## XSLT


### XSLT 1.0

{{works with|xsltproc|libxslt 10126}}

```xml
<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="text" encoding="utf-8"/>

	<!-- Outputs a line for a single FizzBuzz iteration. -->
	<xsl:template name="fizzbuzz-single">
		<xsl:param name="n"/>

		<!-- $s will be "", "Fizz", "Buzz", or "FizzBuzz". -->
		<xsl:variable name="s">
			<xsl:if test="$n mod 3 = 0">Fizz</xsl:if>
			<xsl:if test="$n mod 5 = 0">Buzz</xsl:if>
		</xsl:variable>

		<!-- Output $s. If $s is blank, also output $n. -->
		<xsl:value-of select="$s"/>
		<xsl:if test="$s = ''">
			<xsl:value-of select="$n"/>
		</xsl:if>

		<!-- End line. -->
		<xsl:value-of select="'&#10;'"/>
	</xsl:template>

	<!-- Calls fizzbuzz-single over each value in a range. -->
	<xsl:template name="fizzbuzz-range">
		<!-- Default parameters: From 1 through 100 -->
		<xsl:param name="startAt" select="1"/>
		<xsl:param name="endAt" select="$startAt + 99"/>

		<!-- Simulate a loop with tail recursion. -->

		<!-- Loop condition -->
		<xsl:if test="$startAt &lt;= $endAt">
			<!-- Loop body -->
			<xsl:call-template name="fizzbuzz-single">
				<xsl:with-param name="n" select="$startAt"/>
			</xsl:call-template>

			<!-- Increment counter, repeat -->
			<xsl:call-template name="fizzbuzz-range">
				<xsl:with-param name="startAt" select="$startAt + 1"/>
				<xsl:with-param name="endAt" select="$endAt"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

	<!-- Main procedure -->
	<xsl:template match="/">
		<!-- Default parameters are used -->
		<xsl:call-template name="fizzbuzz-range"/>
	</xsl:template>
</xsl:stylesheet>
```


### XSLT 1.0 With EXSLT


```xml
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  exclude-result-prefixes="xsl exsl">
<xsl:output method="text"/>

<xsl:template name="FizzBuzz" match="/">
  <xsl:param name="n" select="1" />
  <xsl:variable name="_">
    <_><xsl:value-of select="$n" /></_>
  </xsl:variable>
  <xsl:apply-templates select="exsl:node-set($_)/_" />
  <xsl:if test="$n < 100">
    <xsl:call-template name="FizzBuzz">
      <xsl:with-param name="n" select="$n + 1" />
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template match="_[. mod 3 = 0]">Fizz
</xsl:template>

<xsl:template match="_[. mod 5 = 0]">Buzz
</xsl:template>

<xsl:template match="_[. mod 15 = 0]" priority="1">FizzBuzz
</xsl:template>

<xsl:template match="_">
  <xsl:value-of select="concat(.,'&#x0A;')" />
</xsl:template>

</xsl:stylesheet>
```



### XSLT 2.0


```xml
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>

<xsl:template match="/">
  <xsl:value-of separator="&#x0A;" select="
    for $n in 1 to 100 return
      concat('fizz'[not($n mod 3)], 'buzz'[not($n mod 5)], $n[$n mod 15 = (1,2,4,7,8,11,13,14)])"/>
</xsl:template>

</xsl:stylesheet>
```



## Yorick


### Iterative solution


```yorick
for(i = 1; i <= 100; i++) {
    if(i % 3 == 0)
        write, format="%s", "Fizz";
    if(i % 5 == 0)
        write, format="%s", "Buzz";
    if(i % 3 && i % 5)
        write, format="%d", i;
    write, "";
}
```


### Vectorized solution


```yorick
output = swrite(format="%d", indgen(100));
output(3::3) = "Fizz";
output(5::5) = "Buzz";
output(15::15) = "FizzBuzz";
write, format="%s\n", output;
```



## Z80 Assembly

See [[FizzBuzz/Assembly]]


## zkl


```zkl
foreach n in ([1..100]) {
   if(n % 3 == 0) print("Fizz");
   if(not (n%5)) "Buzz".print();
   if(n%3 and n%5) print(n);
   println();
}
```

Or, using infinite lazy sequences:

```zkl
fcn f(a,b,c){ a+b and a+b or c }
Walker.cycle("","","Fizz").zipWith(f,Walker.cycle("","","","","Buzz"),[1..])
   .walk(100).concat("\n").println();
```

More of the same:

```zkl
Walker.cycle(0,0,"Fizz",0,"Buzz","Fizz",0,0,"Fizz","Buzz",0,"Fizz",0,0,"FizzBuzz")
   .zipWith(fcn(a,b){ a or b },[1..]).walk(100).concat("\n").println();
```

{{out}}

```txt

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
...

```



## ZX Spectrum Basic

{{trans|Applesoft BASIC}}

```zxbasic
10 DEF FN m(a,b)=a-INT (a/b)*b
20 FOR a=1 TO 100
30 LET o$=""
40 IF FN m(a,3)=0 THEN LET o$="Fizz"
50 IF FN m(a,5)=0 THEN LET o$=o$+"Buzz"
60 IF o$="" THEN LET o$=STR$ a
70 PRINT o$
80 NEXT a
```

