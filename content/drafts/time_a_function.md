+++
title = "Time a function"
description = ""
date = 2019-10-14T14:43:45Z
aliases = []
[extra]
id = 2434
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
[[Category:Date and time]]
{{omit from|Batch File|No way to programmatically retrieving the current or elapsed time. Only human-readable formats available, which can't be parsed accurately.}}
{{omit from|GUISS}}
{{omit from|ML/I}}

;Task:
Write a program which uses a timer (with the least granularity available
on your system) to time how long a function takes to execute.

Whenever possible, use methods which measure only the processing time used
by the current process; instead of the difference in [[system time]]
between start and finish, which could include time used by
other processes on the computer.

This task is intended as a subtask for [[Measure relative performance of sorting algorithms implementations]].





## 8051 Assembly


Using a timer requires knowledge on two things: the oscillator frequency
(which limits the maximum precision) and the desired precision.
This code uses a common crystal of 11.0592MHz - but provides values
for a few others as examples.
This code also uses a precision of 4 bits (2^(-4) = 0.0625 seconds).
Those familiar with binary can think of this as a right shift of 4
of the multi-byte value, where the low 4 bits represent the fraction of a second,
and the remaining bits represent whole seconds.
The maximum time value depends on the number of bytes used and the precision.
For x bytes and p precision, the maximum value you can count to
is (256^x - 1) * 2^(-p).


```asm
TC	EQU	8 ; number of counter registers
TSTART	EQU	08h ; first register of timer counter
TEND	EQU	TSTART + TC - 1 ; end register of timer counter
; Note: The multi-byte value is stored in Big-endian

; Some timer reloads
_6H	EQU	085h ; 6MHz
_6L	EQU	0edh
_12H	EQU	00bh ; 12MHz
_12L	EQU	0dbh
_110592H	EQU	01eh ; 11.0592MHz
_110592L	EQU	0ffh

; How to calculate timer reload (e.g. for 11.0592MHz):
; Note: 1 machine cycle takes 12 oscillator periods
; 11.0592MHz / 12 * 0.0625 seconds = 57,600 cycles = e100h
; ffffh - e100h = NOT e100h = 1effh

; assuming a 11.0592MHz crystal
TIMERH	EQU	_110592H
TIMERL	EQU	_110592L

;; some timer macros (using timer0)
start_timer macro
	setb tr0
endm
stop_timer macro
	clr tr0
endm
reset_timer macro
	mov tl0, #TIMERL
	mov th0, #TIMERH
endm

increment_counter macro ;; increment counter (multi-byte increment)
	push psw
	push acc
	push 0 ; r0
	mov r0, #TEND+1
	setb c
inc_reg:
	dec r0
	clr a
	addc a, @r0
	mov @r0, a
	jnc inc_reg_	; end prematurally if the higher bytes are unchanged
	cjne r0, #TSTART, inc_reg
inc_reg_:
	; if the carry is set here then the multi byte value has overflowed
	pop 0
	pop acc
	pop psw
endm

ORG RESET
	jmp init
ORG TIMER0
	jmp timer_0

timer_0: ; interrupt every 6.25ms
	stop_timer		; we only want to time the function
	reset_timer
	increment_counter
	start_timer
	reti

init:
	mov sp, #TEND
	setb ea			; enable interrupts
	setb et0		; enable timer0 interrupt
	mov tmod, #01h		; timer0 16-bit mode
	reset_timer

	; reset timer counter registers
	clr a
	mov r0, #TSTART
clear:
	mov @r0, a
	inc r0
	cjne r0, #TEND, clear

	start_timer
	call function		; the function to time
	stop_timer

	; at this point the registers from TSTART
	; through TEND indicate the current time
	; multiplying the 8/16/24/etc length value by 0.0625 (2^-4) gives
	; the elapsed number of seconds
	; e.g. if the three registers were 02a0f2h then the elapsed time is:
	; 02a0f2h = 172,274 and 172,274 * 0.0625 = 10,767.125 seconds
	;
	; Or alternatively:
	; (high byte) 02h = 2 and 2 * 2^(16-4) = 8192
	; (mid byte) a0h = 160 and 160 * 2^(8-4) = 2560
	; (low byte) f2h = 242 and 242 * 2^(0-4) = 15.125
	; 8192 + 2560 + 15.125 = 10,767.125 seconds

	jmp $

function:
	; do whatever here
	ret

END

```



## ACL2



```Lisp
(time$ (nthcdr 9999999 (take 10000000 nil)))
```


Output (for Clozure):

```txt
; (EV-REC *RETURN-LAST-ARG3* ...) took
; 2.53 seconds realtime, 2.48 seconds runtime
; (160,001,648 bytes allocated).
(NIL)
```



## Ada


```ada
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_Io; use Ada.Text_Io;

procedure Query_Performance is
   type Proc_Access is access procedure(X : in out Integer);
   function Time_It(Action : Proc_Access; Arg : Integer) return Duration is
      Start_Time : Time := Clock;
      Finis_Time : Time;
      Func_Arg : Integer := Arg;
   begin
      Action(Func_Arg);
      Finis_Time := Clock;
      return Finis_Time - Start_Time;
   end Time_It;
   procedure Identity(X : in out Integer) is
   begin
      X := X;
   end Identity;
   procedure Sum (Num : in out Integer) is
   begin
      for I in 1..1000 loop
         Num := Num + I;
      end loop;
   end Sum;
   Id_Access : Proc_Access := Identity'access;
   Sum_Access : Proc_Access := Sum'access;

begin
   Put_Line("Identity(4) takes" & Duration'Image(Time_It(Id_Access, 4)) & " seconds.");
   Put_Line("Sum(4) takes:" & Duration'Image(Time_It(Sum_Access, 4)) & " seconds.");
end Query_Performance;
```


### Example

 Identity(4) takes 0.000001117 seconds.
 Sum(4) takes: 0.000003632 seconds.


## Aime


```aime
integer
identity(integer x)
{
    x;
}


integer
sum(integer c)
{
    integer s;

    s = 0;
    while (c) {
	s += c;
	c -= 1;
    }

    s;
}


real
time_f(integer (*fp)(integer), integer fa)
{
    date f, s;
    time t;

    s.now;

    fp(fa);

    f.now;

    t.ddiff(f, s);

    t.microsecond / 1000000r;
}


integer
main(void)
{
    o_real(6, time_f(identity, 1));
    o_text(" seconds\n");
    o_real(6, time_f(sum, 1000000));
    o_text(" seconds\n");

    0;
}
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program fcttime.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ N1, 1000000   @ loop number
.equ NBMEASURE, 10 @ measure number

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessError:       .asciz "Error detected !!!!. \n"
szMessSep:         .asciz "****************************\n"
szMessTemps:       .ascii "Function time : "
sSecondes:         .fill 10,1,' '
                   .ascii " s "
sMicroS:           .fill 10,1,' '
                   .asciz " micros\n"

szCarriageReturn:  .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
dwDebut:           .skip 8
dwFin:             .skip 8
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                     @ entry of program
    adr r0,mult                           @ function address to measure
    mov r1,#1                             @ parameter 1 function
    mov r2,#2                             @ parameter 2 function
    bl timeMesure
    cmp r0,#0
    blt 99f
    adr r0,sum                            @ function address to measure
    mov r1,#1
    mov r2,#2
    bl timeMesure
    cmp r0,#0
    blt 99f
    b 100f
99:
    @ error
    ldr r0,iAdrszMessError
    bl affichageMess
100:                                       @ standard end of the program
    mov r0, #0                             @ return code
    mov r7, #EXIT                          @ request to exit program
    svc #0                                 @ perform the system call

iAdrszMessError:          .int szMessError
iAdrszCarriageReturn:     .int szCarriageReturn
/**************************************************************/
/*   examble function sum                                     */
/**************************************************************/
/* r0 contains op 1     */
/* r1 contains op 2     */
sum:
    push {lr}                  @ save registres
    add r0,r1
100:
    pop {lr}                   @ restaur registers
    bx lr                      @ function return

/**************************************************************/
/*   exemple execution multiplication                         */
/**************************************************************/
/* r0 contains op 1     */
/* r1 contains op 2     */
mult:
    push {lr}                  @ save registres
    mul r0,r1,r0
100:
    pop {lr}                   @ restaur registers
    bx lr                      @ function return

/**************************************************************/
/*   Procedure for measuring the execution time of a routine  */
/**************************************************************/
/* r0 contains the function address     */
timeMesure:
    push {r1-r8,lr}                      @ save registres
    mov r4,r0                            @ save function address
    mov r5,r1                            @ save param 1
    mov r6,r2                            @ save param 2
    mov r8,#0
1:
    ldr r0,iAdrdwDebut                   @ start time area
    mov r1,#0
    mov r7, #0x4e                        @ call system gettimeofday
    svc #0
    cmp r0,#0                            @ error ?
    blt 100f                             @ return error
    ldr r7,iMax                          @ run number
    mov r0,r5                            @ param function 1
    mov r1,r6                            @ param function 2
2:                                       @ loop
    blx r4                               @ call of the function to be measured
    subs r7,#1                           @ decrement run
    bge 2b                               @ loop if not zero
    @
    ldr r0,iAdrdwFin                     @ end time area
    mov r1,#0
    mov r7, #0x4e                        @ call system gettimeofday
    svc #0
    cmp r0,#0                            @ error ?
    blt 100f                             @ return error
                                         @ compute time
    ldr r0,iAdrdwDebut                   @ start time area
    //vidmemtit mesure r0 2
    ldr r2,[r0]                          @ secondes
    ldr r3,[r0,#4]                       @ micro secondes
    ldr r0,iAdrdwFin                     @ end time area
    ldr r1,[r0]                          @ secondes
    ldr r0,[r0,#4]                       @ micro secondes
    sub r2,r1,r2                         @ secondes number
    subs r3,r0,r3                        @ microsecondes number
    sublt r2,#1                          @ if negative sub 1 seconde to secondes
    ldr r1,iSecMicro
    addlt r3,r1                          @ and add 1000000 to microsecondes number
    mov r0,r2                            @ conversion secondes
    ldr r1,iAdrsSecondes
    bl conversion10
    mov r0,r3                            @ conversion microsecondes
    ldr r1,iAdrsMicroS
    bl conversion10
    ldr r0,iAdrszMessTemps
    bl affichageMess                     @ display message
    add r8,#1
    cmp r8,#NBMEASURE
    ble 1b
    ldr r0,iAdrszMessSep                 @ display separator
    bl affichageMess
100:
    pop {r1-r8,lr}                       @ restaur registers
    bx lr                                @ function return
iMax:                 .int N1
iAdrdwDebut:          .int dwDebut
iAdrdwFin:            .int dwFin
iSecMicro:            .int 1000000
iAdrsSecondes:        .int sSecondes
iAdrsMicroS:          .int sMicroS
iAdrszMessTemps:      .int szMessTemps
iAdrszMessSep:        .int szMessSep

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length
1:                                          @ loop length calculation
    ldrb r1,[r0,r2]                         @ read octet start position + index
    cmp r1,#0                               @ if 0 its over
    addne r2,r2,#1                          @ else add 1 in the length
    bne 1b                                  @ and loop
                                            @ so here r2 contains the length of the message
    mov r1,r0                               @ address message in r1
    mov r0,#STDOUT                          @ code to write to the standard output Linux
    mov r7, #WRITE                          @ code call system "write"
    svc #0                                  @ call systeme
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */
    bx lr                                   @ return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                         @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                          @ start loop
    bl divisionpar10                        @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                              @ digit
    strb r1,[r3,r2]                         @ store digit on area
    cmp r0,#0                               @ stop if quotient = 0
    subne r2,#1                               @ previous position
    bne 1b                                  @ else loop
                                            @ end replaces digit in front of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]                         @ store in area begin
    add r4,#1
    add r2,#1                               @ previous position
    cmp r2,#LGZONECAL                       @ end
    ble 2b                                  @ loop
    mov r1,#' '
3:
    strb r1,[r3,r4]
    add r4,#1
    cmp r4,#LGZONECAL                       @ end
    ble 3b
100:
    pop {r1-r4,lr}                          @ restaur registres
    bx lr                                   @return
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
    push {r2-r4}                           @ save registers  */
    mov r4,r0
    mov r3,#0x6667                         @ r3 <- magic_number  lower
    movt r3,#0x6666                        @ r3 <- magic_number  upper
    smull r1, r2, r3, r0                   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0)
    mov r2, r2, ASR #2                     @ r2 <- r2 >> 2
    mov r1, r0, LSR #31                    @ r1 <- r0 >> 31
    add r0, r2, r1                         @ r0 <- r2 + r1
    add r2,r0,r0, lsl #2                   @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                   @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2-r4}
    bx lr                                  @ return

```

{{out}}

```txt

Function time : 0          s 16881      micros
Function time : 0          s 16728      micros
Function time : 0          s 16690      micros
Function time : 0          s 16904      micros
Function time : 0          s 16703      micros
Function time : 0          s 16686      micros
Function time : 0          s 16703      micros
Function time : 0          s 8240       micros
Function time : 0          s 7152       micros
Function time : 0          s 7143       micros
Function time : 0          s 7153       micros
****************************
Function time : 0          s 7153       micros
Function time : 0          s 7143       micros
Function time : 0          s 7153       micros
Function time : 0          s 7151       micros
Function time : 0          s 7151       micros
Function time : 0          s 7144       micros
Function time : 0          s 7153       micros
Function time : 0          s 7177       micros
Function time : 0          s 7143       micros
Function time : 0          s 7156       micros
Function time : 0          s 7154       micros
****************************


```



## Arturo



```arturo
func {
	delay 2000
}

print "Function took: " + $(timer func) + "ms"
```


{{out}}


```txt
Function took: 2001ms
```



## AutoHotkey


### System time

Uses system time, not process time

```AutoHotkey
MsgBox % time("fx")
Return

fx()
{
  Sleep, 1000
}

time(function, parameter=0)
{
  SetBatchLines -1  ; don't sleep for other green threads
  StartTime := A_TickCount
  %function%(parameter)
  Return ElapsedTime := A_TickCount - StartTime . " milliseconds"
}
```


###  Using QueryPerformanceCounter

QueryPerformanceCounter allows even more precision:

```AHK
MsgBox % time("fx")

time(function, parameter=0){
	SetBatchLines -1
	DllCall("QueryPerformanceCounter", "Int64*", CounterBefore)
	DllCall("QueryPerformanceFrequency", "Int64*", Freq)
	%function%(parameter)
	DllCall("QueryPerformanceCounter", "Int64*", CounterAfter)
	return (CounterAfter-CounterBefore)/Freq * 1000 " milliseconds"
}

fx(){
	Sleep 1000
}
```



## BaCon

The BaCon '''TIMER''' function keeps track of time spent running, in milliseconds (which is also the time unit used by '''SLEEP''').  This is not process specific, but a wall clock time counter which starts at 0 during process initialization.  As BaCon can easily use external C libraries, process specific ''CLOCK_PROCESS_CPUTIME_ID'' '''clock_gettime''' could also be used.


```freebasic
' Time a function
SUB timed()
    SLEEP 7000
END SUB

st = TIMER
timed()
et = TIMER
PRINT st, ", ", et
```


{{out}}

```txt
prompt$ ./time-function
0, 7000
```



## BASIC

{{works with|QBasic}}

```qbasic
DIM timestart AS SINGLE, timedone AS SINGLE, timeelapsed AS SINGLE

timestart = TIMER
SLEEP 1 'code or function to execute goes here
timedone = TIMER

'midnight check:
IF timedone < timestart THEN timedone = timedone + 86400
timeelapsed = timedone - timestart
```


See also: [[#BBC BASIC|BBC BASIC]], [[#PureBasic|PureBasic]].


## Batch File

Granularity: hundredths of second.

```Batch File

@echo off
Setlocal EnableDelayedExpansion

call :clock

::timed function:fibonacci series.....................................
set /a a=0 ,b=1,c=1
:loop
if %c% lss 2000000000 echo %c% & set /a c=a+b,a=b, b=c & goto loop
::....................................................................

call :clock

echo  Function executed in %timed% hundredths of second
goto:eof

:clock
if not defined timed set timed=0
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
set /A timed = "(((1%%a - 100) * 60 + (1%%b - 100)) * 60 + (1%%c - 100))  * 100 + (1%%d - 100)- %timed%"
)
goto:eof

```



## BBC BASIC


```bbcbasic
start%=TIME:REM centi-second timer
REM perform processing
lapsed%=TIME-start%
```



## Bracmat


```bracmat
( ( time
  =   fun funarg t0 ret
    .   !arg:(?fun.?funarg)
      & clk$:?t0
      & !fun$!funarg:?ret
      & (!ret.flt$(clk$+-1*!t0,3) s)
  )
& ( fib
  =
    .   !arg:<2&1
      | fib$(!arg+-1)+fib$(!arg+-2)
  )
& time$(fib.30)
)
```

Output:

```txt
1346269.5,141*10E0 s
```



## C

{{works with|POSIX|.1-2001}}
On some system (like GNU/Linux) to be able to use the <tt>clock_gettime</tt> function you must link with the <tt>rt</tt> (RealTime) library.

<code>CLOCK_PROCESS_CPUTIME_ID</code> is preferred when available (eg. Linux kernel 2.6.12 up), being CPU time used by the current process.  (<code>CLOCK_MONOTONIC</code> generally includes CPU time of unrelated processes, and may be drifted by <code>adjtime()</code>.)


```c
#include <stdio.h>
#include <time.h>

int identity(int x) { return x; }

int sum(int s)
{
  int i;
  for(i=0; i < 1000000; i++) s += i;
  return s;
}

#ifdef CLOCK_PROCESS_CPUTIME_ID
/* cpu time in the current process */
#define CLOCKTYPE  CLOCK_PROCESS_CPUTIME_ID
#else
/* this one should be appropriate to avoid errors on multiprocessors systems */
#define CLOCKTYPE  CLOCK_MONOTONIC
#endif

double time_it(int (*action)(int), int arg)
{
  struct timespec tsi, tsf;

  clock_gettime(CLOCKTYPE, &tsi);
  action(arg);
  clock_gettime(CLOCKTYPE, &tsf);

  double elaps_s = difftime(tsf.tv_sec, tsi.tv_sec);
  long elaps_ns = tsf.tv_nsec - tsi.tv_nsec;
  return elaps_s + ((double)elaps_ns) / 1.0e9;
}

int main()
{
  printf("identity (4) takes %lf s\n", time_it(identity, 4));
  printf("sum      (4) takes %lf s\n", time_it(sum, 4));
  return 0;
}
```



## C++


```cpp
#include <ctime>
#include <iostream>
using namespace std;

int identity(int x) { return x; }
int sum(int num) {
  for (int i = 0; i < 1000000; i++)
    num += i;
  return num;
}

double time_it(int (*action)(int), int arg) {
  clock_t start_time = clock();
  action(arg);
  clock_t finis_time = clock();
  return ((double) (finis_time - start_time)) / CLOCKS_PER_SEC;
}

int main() {
  cout << "Identity(4) takes " << time_it(identity, 4) << " seconds." << endl;
  cout << "Sum(4) takes " << time_it(sum, 4) << " seconds." << endl;
  return 0;
}
```



### Example

 Identity(4) takes 0 seconds.
 Sum(4) takes 0.01 seconds.

## C#

Using Stopwatch.


```c#
using System;
using System.Linq;
using System.Threading;
using System.Diagnostics;

class Program {
    static void Main(string[] args) {
        Stopwatch sw = new Stopwatch();

        sw.Start();
        DoSomething();
        sw.Stop();

        Console.WriteLine("DoSomething() took {0}ms.", sw.Elapsed.TotalMilliseconds);
    }

    static void DoSomething() {
        Thread.Sleep(1000);

        Enumerable.Range(1, 10000).Where(x => x % 2 == 0).Sum();  // Sum even numers from 1 to 10000
    }
}
```


Using DateTime.


```c#
using System;
using System.Linq;
using System.Threading;

class Program {
    static void Main(string[] args) {
        DateTime start, end;

        start = DateTime.Now;
        DoSomething();
        end = DateTime.Now;

        Console.WriteLine("DoSomething() took " + (end - start).TotalMilliseconds + "ms");
    }

    static void DoSomething() {
        Thread.Sleep(1000);

        Enumerable.Range(1, 10000).Where(x => x % 2 == 0).Sum();  // Sum even numers from 1 to 10000
    }
}
```


Output:
 DoSomething() took 1071,5408ms


## Clojure


```clojure

  (defn fib []
    (map first
      (iterate
        (fn [[a b]] [b (+ a b)])
        [0 1])))

  (time (take 100 (fib)))

```


Output:
 "Elapsed time: 0.028 msecs"
 (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)


## Common Lisp


Common Lisp provides a standard utility for performance measurement, [http://www.lispworks.com/documentation/HyperSpec/Body/m_time.htm time]:


```lisp
CL-USER> (time (reduce #'+ (make-list 100000 :initial-element 1)))
Evaluation took:
  0.151 seconds of real time
  0.019035 seconds of user run time
  0.01807 seconds of system run time
  0 calls to %EVAL
  0 page faults and
  2,400,256 bytes consed.
```


(The example output here is from [[SBCL]].)

However, it merely prints textual information to [http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#trace_output trace output], so the information is not readily available for further processing (except by parsing it in a CL-implementation-specific manner).

The functions [http://www.lispworks.com/documentation/HyperSpec/Body/f_get__1.htm get-internal-run-time] and [http://www.lispworks.com/documentation/HyperSpec/Body/f_get_in.htm get-internal-real-time] may be used to get time information programmatically, with at least one-second granularity (and usually more). Here is a function which uses them to measure the time taken for one execution of a provided function:


```lisp
(defun timings (function)
  (let ((real-base (get-internal-real-time))
        (run-base (get-internal-run-time)))
    (funcall function)
    (values (/ (- (get-internal-real-time) real-base) internal-time-units-per-second)
            (/ (- (get-internal-run-time) run-base) internal-time-units-per-second))))

CL-USER> (timings (lambda () (reduce #'+ (make-list 100000 :initial-element 1))))
17/500
7/250
```



## D


```d
import std.stdio, std.datetime;

int identity(int x) {
    return x;
}

int sum(int num) {
    foreach (i; 0 .. 100_000_000)
        num += i;
    return num;
}

double timeIt(int function(int) func, int arg) {
    StopWatch sw;
    sw.start();
    func(arg);
    sw.stop();
    return sw.peek().usecs / 1_000_000.0;
}

void main() {
    writefln("identity(4) takes %f6 seconds.", timeIt(&identity, 4));
    writefln("sum(4) takes %f seconds.", timeIt(&sum, 4));
}
```

Output:
```txt
identity(4) takes 0.0000016 seconds.
sum(4) takes 0.522065 seconds.
```


### Using Tango


```d

import tango.io.Stdout;
import tango.time.Clock;

int identity (int x)
{
    return x;
}

int sum (int num)
{
    for (int i = 0; i < 1000000; i++)
      num += i;
    return num;
}

double timeIt(int function(int) func, int arg)
{
    long before = Clock.now.ticks;
    func(arg);
    return (Clock.now.ticks - before) / cast(double)TimeSpan.TicksPerSecond;
}

void main ()
{
    Stdout.format("Identity(4) takes {:f6} seconds",timeIt(&identity,4)).newline;
    Stdout.format("Sum(4) takes {:f6} seconds",timeIt(&sum,4)).newline;
}

```



## E


{{trans|Java}} — E has no ''standardized'' facility for CPU time measurement; this {{works with|E-on-Java}}.


```e
def countTo(x) {
	println("Counting...")
	for _ in 1..x {}
	println("Done!")
}

def MX := <unsafe:java.lang.management.makeManagementFactory>
def threadMX := MX.getThreadMXBean()
require(threadMX.isCurrentThreadCpuTimeSupported())
threadMX.setThreadCpuTimeEnabled(true)

for count in [10000, 100000] {
	def start := threadMX.getCurrentThreadCpuTime()
	countTo(count)
	def finish := threadMX.getCurrentThreadCpuTime()
	println(`Counting to $count takes ${(finish-start)//1000000}ms`)
}
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import system'calendar;
import system'routines;
import system'threading;
import system'math;
import extensions;

someProcess()
{
    threadControl.sleep(1000);

    new Range(0,10000).filterBy:(x => x.mod:2 == 0).summarize();
}

public program()
{
    var start := now;

    someProcess();

    var end := now;

    console.printLine("Time elapsed in msec:",(end - start).Milliseconds)
}
```

{{out}}

```txt

Time elapsed in msec:1015

```



## Elixir

{{trans|Erlang}}
'''tc/1'''

```elixir
iex(10)> :timer.tc(fn -> Enum.each(1..100000, fn x -> x*x end) end)
{236000, :ok}
```

'''tc/2'''

```elixir
iex(11)> :timer.tc(fn x -> Enum.each(1..x, fn y -> y*y end) end, [1000000])
{2300000, :ok}
```

'''tc/3'''

```elixir
iex(12)> :timer.tc(Enum, :to_list, [1..1000000])
{224000,
 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
  23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
  42, 43, 44, 45, 46, 47, 48, 49, ...]}
```



## Erlang

Erlang's timer module has three implementations of the tc function.

'''tc/1''' takes a 0-arity function and executes it:

```erlang

5> {Time,Result} = timer:tc(fun () -> lists:foreach(fun(X) -> X*X end, lists:seq(1,100000)) end).
{226391,ok}
6> Time/1000000. % Time is in microseconds.
0.226391
7> % Time is in microseconds.

```

'''tc/2''' takes an n-arity function and its arguments:

```erlang

9> timer:tc(fun (X) -> lists:foreach(fun(Y) -> Y*Y end, lists:seq(1,X)) end, [1000000]).
{2293844,ok}

```

'''tc/3''' takes a module name, function name and the list of arguments to the function:

```erlang

8> timer:tc(lists,seq,[1,1000000]).
{62370,
 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
  23,24,25,26,27|...]}

```


## Euphoria


```euphoria
atom t
t = time()
some_procedure()
t = time() - t
printf(1,"Elapsed %f seconds.\n",t)
```


=={{header|F Sharp|F#}}==
The .Net framework provides a Stopwatch class which provides a performance counter.

```fsharp

open System.Diagnostics
let myfunc data =
    let timer = new Stopwatch()
    timer.Start()
    let result = data |> expensive_processing
    timer.Stop()
    printf "elapsed %d ms" timer.ElapsedMilliseconds
    result

```



## Factor


```factor
[ 10000 iota sum drop ] time
```

Output:
 Running time: 0.002888635 seconds

 Additional information was collected.
 dispatch-stats.  - Print method dispatch statistics
 gc-events.       - Print all garbage collection events
 gc-stats.        - Print breakdown of different garbage collection events
 gc-summary.      - Print aggregate garbage collection statistics


## Forth

{{works with|GNU Forth}}

```forth
: time: ( "word" -- )
  utime 2>R ' EXECUTE
  utime 2R> D-
  <# # # # # # # [CHAR] . HOLD #S #> TYPE ."  seconds" ;

1000 time: MS  \ 1.000081 seconds ok
```



## Fortran

{{works with|Gfortran}} version 4.4.5 (Debian 4.4.5-8) on x86_64-linux-gnu


```fortran

c The subroutine to analyze
      subroutine do_something()
c For testing we just do nothing for 3 seconds
      call sleep(3)
      return
      end

c Main Program
      program timing
      integer(kind=8) start,finish,rate
      call system_clock(count_rate=rate)
      call system_clock(start)
c Here comes the function we want to time
      call do_something()
      call system_clock(finish)
      write(6,*) 'Elapsed Time in seconds:',float(finish-start)/rate
      return
      end

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function sumToLimit(limit As UInteger) As UInteger
  Dim sum As UInteger = 0
  For i As UInteger = 1 To limit
    sum += i
  Next
  Return sum
End Function

Dim As Double start = timer
Dim limit As UInteger = 100000000
Dim result As UInteger = sumToLimit(limit)
Dim ms As UInteger = Int(1000 * (timer - start) + 0.5)
Print "sumToLimit("; Str(limit); ") = "; result
Print "took ";  ms; " milliseconds to calculate"
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

sumToLimit(100000000) = 5000000050000000
took 314 milliseconds to calculate

```



## GAP


```gap
# Return the time passed in last function
time;
```



## Go


### go test

The Go command line tool <code>go test</code> includes [http://golang.org/pkg/testing/#hdr-Benchmarks benchmarking support].
Given a package with functions:

```go
package empty

func Empty() {}

func Count() {
    // count to a million
    for i := 0; i < 1e6; i++ {
    }
}
```

the following code, placed in a file whose name ends in <tt>_test.go</tt>, will time them:

```go
package empty

import "testing"

func BenchmarkEmpty(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Empty()
    }
}

func BenchmarkCount(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Count()
    }
}
```

<code>go test</code> varies <code>b.N</code> to get meaningful resolution.
Example:

```txt

$ go test -bench=.
testing: warning: no tests to run
PASS
BenchmarkEmpty	2000000000	         0.30 ns/op
BenchmarkCount	   10000	    298734 ns/op
ok  		3.642s

```

The first number is the value of <code>b.N</code> chosen and the second the average time per iteration.
The <code>testing</code> package can optionally include memory use and throughput benchmarks.

There is also a [https://golang.org/x/tools/cmd/benchcmp standard tool] to compare the multiple benchmark outputs (installable via <code>go get golang.org/x/tools/cmd/benchcmp</code>).


### testing.Benchmark

The benchmarking features of the <code>testing</code> package are exported for use within a Go program.

```go
package main

import (
    "fmt"
    "testing"
)

func empty() {}

func count() {
    for i := 0; i < 1e6; i++ {
    }
}

func main() {
    e := testing.Benchmark(func(b *testing.B) {
        for i := 0; i < b.N; i++ {
            empty()
        }
    })
    c := testing.Benchmark(func(b *testing.B) {
        for i := 0; i < b.N; i++ {
            count()
        }
    })
    fmt.Println("Empty function:    ", e)
    fmt.Println("Count to a million:", c)
    fmt.Println()
    fmt.Printf("Empty: %12.4f\n", float64(e.T.Nanoseconds())/float64(e.N))
    fmt.Printf("Count: %12.4f\n", float64(c.T.Nanoseconds())/float64(c.N))
}
```

{{out}}

```txt

Empty function:     2000000000	         0.80 ns/op
Count to a million:     2000	    796071 ns/op

Empty:       0.7974
Count:  796071.6555

```



### Alternative technique

The <code>go test</code> command and the <code>testing</code> package are the preferred techniques for benchmarking or timing any Go code.  Ignoring the well-tested and carefully crafted standard tools though, here is a simplistic alternative:

As the first line of the function you wish to time, use <tt>defer</tt> with an argument of <tt>time.Now()</tt> to print the elapsed time to the return of any function.  For example, define the function <tt>from</tt> as shown below.  It works because defer evaluates its function's arguments at the time the function is deferred, so the current time gets captured at the point of the defer.  When the function containing the defer returns, the deferred <tt>from</tt> function runs, computes the elapsed time as a <tt>time.Duration</tt>, and prints it with standard formatting, which adds a nicely scaled unit suffix.

```go
package main

import (
    "fmt"
    "time"
)

func from(t0 time.Time) {
    fmt.Println(time.Now().Sub(t0))
}

func empty() {
    defer from(time.Now())
}

func count() {
    defer from(time.Now())
    for i := 0; i < 1e6; i++ {
    }
}

func main() {
    empty()
    count()
}
```

Output:

```txt

2us
643us

```



## Groovy

{{trans|Java}}

### CPU Timing


```groovy
import java.lang.management.ManagementFactory
import java.lang.management.ThreadMXBean

def threadMX = ManagementFactory.threadMXBean
assert threadMX.currentThreadCpuTimeSupported
threadMX.threadCpuTimeEnabled = true

def clockCpuTime = { Closure c ->
    def start = threadMX.currentThreadCpuTime
    c.call()
    (threadMX.currentThreadCpuTime - start)/1000000
}
```



### Wall Clock Timing


```groovy
def clockRealTime = { Closure c ->
    def start = System.currentTimeMillis()
    c.call()
    System.currentTimeMillis() - start
}
```


Test:

```groovy
def countTo = { Long n ->
    long i = 0; while(i < n) { i += 1L }
}

["CPU time":clockCpuTime, "wall clock time":clockRealTime].each { measurementType, timer ->
    println '\n'
    [100000000L, 1000000000L].each { testSize ->
        def measuredTime = timer(countTo.curry(testSize))
        println "Counting to ${testSize} takes ${measuredTime}ms of ${measurementType}"
    }
}
```


Output:

```txt
Counting to 100000000 takes 23150.5484ms of CPU time
Counting to 1000000000 takes 233861.0991ms of CPU time


Counting to 100000000 takes 24314ms of wall clock time
Counting to 1000000000 takes 249005ms of wall clock time
```



## Halon


```halon
$t = uptime();

sleep(1);

echo uptime() - $t;
```



## Haskell


```haskell
import System.CPUTime (getCPUTime)

-- We assume the function we are timing is an IO monad computation
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
  startTime <- getCPUTime
  action arg
  finishTime <- getCPUTime
  return $ fromIntegral (finishTime - startTime) / 1000000000000

-- Version for use with evaluating regular non-monadic functions
timeIt_ :: (Fractional c) => (a -> b) -> a -> IO c
timeIt_ f = timeIt ((`seq` return ()) . f)
```



### Example

 *Main> :m + Text.Printf Data.List
 *Main Data.List Text.Printf> timeIt' id 4 >>= printf "Identity(4) takes %f seconds.\n"
 Identity(4) takes 0.0 seconds.
 *Main Data.List Text.Printf> timeIt' (\x -> foldl' (+) x [1..1000000]) 4 >>= printf "Sum(4) takes %f seconds.\n"
 Sum(4) takes 0.248015 seconds.


## HicEst


```HicEst
t_start = TIME()    ! returns seconds since midnight
SYSTEM(WAIT = 1234) ! wait 1234 milliseconds
t_end = TIME()

WRITE(StatusBar) t_end - t_start, " seconds"
```


=={{header|Icon}} and {{header|Unicon}}==
The function 'timef' takes as argument a procedure name and collects performance and timing information including run time (in milliseconds), garbage collection, and memory usage by region.


```Icon
procedure timef(f)                               #: time a function f
local gcol,alloc,used,size,runtime,header,x,i

title := ["","total","static","string","block"]  # headings
collect()                                        # start with collected memory (before baseline)
every put(gcol  := [], -&collections)            # baseline collections count
every put(alloc := [], -&allocated)              # . total allocated space by region
every put(used  := [], -&storage)                # . currently used space by region - no total
every put(size  := [], -&regions)                # . current size of regions        - no total

write("Performance and Timing measurement for ",image(f),":")
runtime := &time                                 # base time
f()
write("Execution time=",&time-runtime," ms.")

every (i := 0, x := &collections) do  gcol[i +:= 1] +:= x
every (i := 0, x := &allocated  ) do alloc[i +:= 1] +:= x
every (i := 0, x := &storage    ) do  used[i +:= 1] +:= x
every (i := 0, x := &regions    ) do  size[i +:= 1] +:= x

push(gcol,"garbage collections:")
push(alloc,"memory allocated:")
push(used,"N/A","currently used:")
push(size,"N/A","current size:")

write("Memory Region and Garbage Collection Summary (delta):")
every (i := 0) <:= *!(title|gcol|alloc|used|size)
every x := (title|gcol|alloc|used|size) do {
   f := left
   every writes(f(!x,i + 3)) do f := right
   write()
   }
write("Note: static region values should be zero and may not be meaningful.")
return
end
```


Sample usage:
```Icon
procedure main()
timef(perfectnumbers)
end

procedure perfectnumbers()
...
```


Sample output (from the [[Perfect_numbers#Icon_and_Unicon|Perfect Numbers]] task):
 Performance and Timing measurement for procedure perfectnumbers:
 Perfect numbers from 1 to 10000:
 6
 28
 496
 8128
 Done.
 Execution time=416 ms.
 Memory Region and Garbage Collection Summary (delta):
                                          total                 static                 string                  block
 garbage collections:                         2                      0                      0                      2
 memory allocated:                      1247012                      0                     24                1246988
 currently used:                            N/A                      0                      0                 248040
 current size:                              N/A                      0                      0                      0
 Note: static region values should be zero and may not be meaningful.


## Ioke


```ioke
use("benchmark")

func = method((1..50000) reduce(+))

Benchmark report(1, 1, func)
```



## J

Time and space requirements are tested using verbs obtained through the Foreign conjunction (<code>!:</code>). <code>6!:2</code> returns time required for execution, in floating-point measurement of seconds. <code>7!:2</code> returns a measurement of space required to execute. Both receive as input a sentence for execution. The verb <code>timespacex</code> combines these and is available in the standard library.

When the [http://www.jsoftware.com/help/dictionary/dmcapdot.htm Memoize] feature or similar techniques are used, execution time and space can both be affected by prior calculations.

### Example


```j
   (6!:2 , 7!:2) '|: 50 50 50 $ i. 50^3'
0.00488008 3.14829e6
   timespacex '|: 50 50 50 $ i. 50^3'
0.00388519 3.14829e6
```



## Java

{{works with|Java|1.5+}}

```java
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

public class TimeIt {
	public static void main(String[] args) {
		final ThreadMXBean threadMX = ManagementFactory.getThreadMXBean();
		assert threadMX.isCurrentThreadCpuTimeSupported();
		threadMX.setThreadCpuTimeEnabled(true);

		long start, end;
		start = threadMX.getCurrentThreadCpuTime();
		countTo(100000000);
		end = threadMX.getCurrentThreadCpuTime();
		System.out.println("Counting to 100000000 takes "+(end-start)/1000000+"ms");
		start = threadMX.getCurrentThreadCpuTime();
		countTo(1000000000L);
		end = threadMX.getCurrentThreadCpuTime();
		System.out.println("Counting to 1000000000 takes "+(end-start)/1000000+"ms");

	}

	public static void countTo(long x){
		System.out.println("Counting...");
		for(long i=0;i<x;i++);
		System.out.println("Done!");
	}
}
```


Measures real time rather than CPU time:
{{works with|Java|(all versions)}}


```java
	public static void main(String[] args){
		long start, end;
		start = System.currentTimeMillis();
		countTo(100000000);
		end = System.currentTimeMillis();
		System.out.println("Counting to 100000000 takes "+(end-start)+"ms");
		start = System.currentTimeMillis();
		countTo(1000000000L);
		end = System.currentTimeMillis();
		System.out.println("Counting to 1000000000 takes "+(end-start)+"ms");

	}
```

Output:
 Counting...
 Done!
 Counting to 100000000 takes 370ms
 Counting...
 Done!
 Counting to 1000000000 takes 3391ms


## Julia


```julia
# v0.6.0

function countto(n::Integer)
    i = zero(n)
    println("Counting...")
    while i < n
        i += 1
    end
    println("Done!")
end

@time countto(10 ^ 5)
@time countto(10 ^ 10)
```


{{out}}

```txt
Counting...
Done!
Counting...
Done!
  0.000109 seconds (15 allocations: 400 bytes)
Counting...
Done!
  0.000127 seconds (15 allocations: 400 bytes)
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2
// need to enable runtime assertions with JVM -ea option

import java.lang.management.ManagementFactory
import java.lang.management.ThreadMXBean

fun countTo(x: Int) {
    println("Counting...");
    (1..x).forEach {}
    println("Done!")
}

fun main(args: Array<String>) {
    val counts = intArrayOf(100_000_000, 1_000_000_000)
    val threadMX = ManagementFactory.getThreadMXBean()
    assert(threadMX.isCurrentThreadCpuTimeSupported)
    threadMX.isThreadCpuTimeEnabled = true
    for (count in counts) {
        val start = threadMX.currentThreadCpuTime
        countTo(count)
        val end = threadMX.currentThreadCpuTime
        println("Counting to $count takes ${(end-start)/1000000}ms")
    }
}
```

This is a typical result (sometimes the second figure is only about 1400ms - no idea why)
{{out}}

```txt

Counting...
Done!
Counting to 100000000 takes 179ms
Counting...
Done!
Counting to 1000000000 takes 3527ms

```



## Lasso


```Lasso
local(start = micros)
loop(100000) => {
	'nothing is outout because no autocollect'
}
'time for 100,000 loop repititions: '+(micros - #start)+' microseconds'
```



## Lingo


```lingo
on testFunc ()
  repeat with i = 1 to 1000000
    x = sqrt(log(i))
  end repeat
end
```



```lingo
ms = _system.milliseconds
testFunc()
ms = _system.milliseconds - ms
put "Execution time in ms:" && ms
-- "Execution time in ms: 983"
```



## Logo

{{works with|UCB Logo}} on a Unix system

This is not an ideal method; Logo does not expose a timer (except for the WAIT command) so we use the Unix "date" command to get a second timer.


```logo
to time
  output first first shell "|date +%s|
end
to elapsed :block
  localmake "start time
  run :block
  (print time - :start [seconds elapsed])
end

elapsed [wait 300]   ; 5 seconds elapsed
```



## Lua


```lua
function Test_Function()
    for i = 1, 10000000 do
        local s = math.log( i )
        s = math.sqrt( s )
    end
end

t1 = os.clock()
    Test_Function()
t2 = os.clock()

print( os.difftime( t2, t1 ) )
```



## M2000 Interpreter

We use Profiler to reset timer, and Timecount to read time in milliseconds as a double, with nanoseconds for resolution. Internal use of QueryPerformanceCounter from Windows Api.
In this example we get times for use of same module with different variable types. sum=limit-limit make sum 0 to the same type of limit,and using n=sum and n++ we make n=1 using same type as sum.

10000% is Integer 16bit

10000& is Long 32bit

10000@ is Decimal

10000# is Currency

10000~ is Float

10000 is Double (default)


```M2000 Interpreter

Module Checkit {
      Module sumtolimit (limit) {
           sum=limit-limit
           n=sum
           n++
           while limit {sum+=limit*n:limit--:n-!}
      }
      Cls ' clear screen
      Profiler
      sumtolimit 10000%
      Print TimeCount
      Profiler
      sumtolimit 10000&
      Print TimeCount
      Profiler
      sumtolimit 10000#
      Print TimeCount
      Profiler
      sumtolimit 10000@
      Print TimeCount
      Profiler
      sumtolimit 10000~
      Print TimeCount
      Profiler
      sumtolimit 10000
      Print TimeCount
}
Checkit

```



## Maple

The built-in command CodeTools:-Usage can compute the "real" time for the length of the computation or the "cpu" time for the computation. The following examples find the real time and cpu time for computing the integer factors for 32!+1.

```maple
CodeTools:-Usage(ifactor(32!+1), output = realtime, quiet);
```


```maple
CodeTools:-Usage(ifactor(32!+1), output = cputime, quiet);
```



## Mathematica


```Mathematica
AbsoluteTiming[x];
```

where x is an operation. Example calculating a million digits of Sqrt[3]:

```Mathematica
AbsoluteTiming[N[Sqrt[3], 10^6]]
```

gives:

```Mathematica
{0.000657, 1.7320508075688772935274463......}
```

First elements if the time in seconds, second elements if the result from the operation. Note that I truncated the result.


## Maxima


```maxima
f(n) := if n < 2 then n else f(n - 1) + f(n - 2)$

/* First solution, call the time function with an output line number, it gives the time taken to compute that line.
   Here it's assumed to be %o2 */
f(24);
46368

time(%o2);
[0.99]

/* Second solution, change a system flag to print timings for all following lines */
showtime: true$

f(24);
Evaluation took 0.9400 seconds (0.9400 elapsed)
46368
```



## MiniScript


```MiniScript
start = time
for i in range(1,100000)
end for
duration = time - start
print "Process took " + duration + " seconds"
```

{{out}}

```txt

Process took 0.312109 seconds

```



## Nim


```nim
import times, os

proc doWork(x) =
  var n = x
  for i in 0..10000000:
    n += i
  echo n

template time(s: stmt): expr =
  let t0 = cpuTime()
  s
  cpuTime() - t0

echo time(doWork(100))
```

Output:

```txt
2.2000000000000000e-01
```



## OCaml


```ocaml
let time_it action arg =
  let start_time = Sys.time () in
  ignore (action arg);
  let finish_time = Sys.time () in
  finish_time -. start_time
```



### Example

 # Printf.printf "Identity(4) takes %f seconds.\n" (time_it (fun x -> x) 4);;
 Identity(4) takes 0.000000 seconds.
 - : unit = ()
 # let sum x = let num = ref x in for i = 0 to 999999 do num := !num + i done; !num;;
 val sum : int -> int = <fun>
 # Printf.printf "Sum(4) takes %f seconds.\n" (time_it sum 4);;
 Sum(4) takes 0.084005 seconds.
 - : unit = ()


## Oforth


bench allows to calculate how long a runnable takes to execute.

Result is microseconds.

It uses difference between system time, not processing time.

{{Out}}

```txt

>#[ 0 1000 seq apply(#+) ] bench .
267
500500 ok

```



## Oz


```oz
declare
  %% returns milliseconds
  fun {TimeIt Proc}
     Before = {Now}
  in
     {Proc}
     {Now} - Before
  end

  fun {Now}
     {Property.get 'time.total'}
  end
in
  {Show
   {TimeIt
    proc {$}
       {FoldL {List.number 1 1000000 1} Number.'+' 4 _}
    end}
  }
```



## PARI/GP

This version, by default, returns just the CPU time used by gp, not the delta of wall times. PARI can be compiled to use wall time if you prefer: configure with <code>--time=ftime</code> instead of <code>--time=
getrusage</code>, <code>--time=clock_gettime</code>, or <code>--time=times</code>. See Appendix A, section 2.2 of the User's Guide to PARI/GP.

```parigp
time(foo)={
  foo();
  gettime();
}
```


Alternate version:
{{works with|PARI/GP|2.6.2+}}

```parigp
time(foo)={
  my(start=getabstime());
  foo();
  getabstime()-start;
}
```



## Perl

Example of using the built-in Benchmark core module - it compares two versions of recursive factorial functions:

```perl
use Benchmark;
use Memoize;

sub fac1 {
    my $n = shift;
    return $n == 0 ? 1 : $n * fac1($n - 1);
}
sub fac2 {
    my $n = shift;
    return $n == 0 ? 1 : $n * fac2($n - 1);
}
memoize('fac2');

my $result = timethese(100000, {
    'fac1' => sub { fac1(50) },
    'fac2' => sub { fac2(50) },
});
Benchmark::cmpthese($result);
```

Output:
 Benchmark: timing 100000 iterations of fac1, fac2...
       fac1:  6 wallclock secs ( 5.45 usr +  0.00 sys =  5.45 CPU) @ 18348.62/s (n=100000)
       fac2:  1 wallclock secs ( 0.84 usr +  0.00 sys =  0.84 CPU) @ 119047.62/s (n=100000)
          Rate fac1 fac2
 fac1  18349/s   -- -85%
 fac2 119048/s 549%   --

Example without using Benchmark:

```perl
sub cpu_time {
  my ($user,$system,$cuser,$csystem) = times;
  $user + $system
}

sub time_it {
  my $action = shift;
  my $startTime = cpu_time();
  $action->(@_);
  my $finishTime = cpu_time();
  $finishTime - $startTime
}

printf "Identity(4) takes %f seconds.\n", time_it(sub {@_}, 4);
# outputs "Identity(4) takes 0.000000 seconds."

sub sum {
  my $x = shift;
  foreach (0 .. 999999) {
    $x += $_;
  }
  $x
}

printf "Sum(4) takes %f seconds.\n", time_it(\&sum, 4);
# outputs "Sum(4) takes 0.280000 seconds."
```



## Perl 6

Follows modern trend toward measuring wall-clock time, since CPU time is becoming rather ill-defined in the age of multicore, and doesn't reflect IO overhead in any case.

```perl6
my $start = now;
(^100000).pick(1000);
say now - $start;
```

{{out}}

```txt
0.02301709
```


## Phix

Measures wall-clock time. On Windows the resolution is about 15ms.

```Phix
atom t0 = time()
some_procedure()
printf(1,"%3.2fs.\n",time()-t0)
```



## PicoLisp

There is a built-in function '[http://software-lab.de/doc/refB.html#bench bench]'
for that. However, it measures wall-clock time, because for practical purposes
the real time needed by a task (including I/O and communication) is more meaningful.
There is another function, '[http://software-lab.de/doc/refT.html#tick tick]', which
also measures user time, and is used by the profiling tools.

```PicoLisp
: (bench (do 1000000 (* 3 4)))
0.080 sec
-> 12
```



## PL/I


```PL/I
declare (start_time, finish_time) float (18);

start_time = secs();

do i = 1 to 10000000;
   /* something to be repeated goes here. */
end;
finish_time = secs();

put skip edit ('elapsed time=', finish_time - start_time, ' seconds')
   (A, F(10,3), A);
   /* gives the result to thousandths of a second. */

/* Note: using the SECS function takes into account the clock */
/* going past midnight. */
```



## PowerShell


```PowerShell

function fun($n){
    $res = 0
    if($n -gt 0) {
        1..$n | foreach{
            $a, $b = $_, ($n+$_)
            $res += $a + $b
        }

    }
    $res
}
"$((Measure-Command {fun 10000}).TotalSeconds) Seconds"

```

<b>Output:</b>

```txt

0.820712 Seconds

```




## PureBasic


### Built in timer

This version uses the built in timer, on Windows it has an accuracy of ~10-15 msec.

```Purebasic
Procedure Foo(Limit)
  Protected i, palindromic, String$
  For i=0 To Limit
    String$=Str(i)
    If String$=ReverseString(String$)
      palindromic+1
    EndIf
  Next
  ProcedureReturn palindromic
EndProcedure

If OpenConsole()
  Define Start, Stop, cnt
  PrintN("Starting timing of a calculation,")
  PrintN("for this we test how many of 0-1000000 are palindromic.")
  Start=ElapsedMilliseconds()
  cnt=Foo(1000000)
  Stop=ElapsedMilliseconds()
  PrintN("The function need "+Str(stop-Start)+" msec,")
  PrintN("and "+Str(cnt)+" are palindromic.")
  Print("Press ENTER to exit."): Input()
EndIf
```


 Starting timing of a calculation,
 for this we test how many of 0-1000000 are palindromic.
 The function need 577 msec,
 and 1999 are palindromic.
 Press ENTER to exit.

===Hi-res version===
{{libheader|Droopy}}

This version uses a hi-res timer, but it is Windows only.

```PureBasic
If OpenConsole()
  Define Timed.f, cnt
  PrintN("Starting timing of a calculation,")
  PrintN("for this we test how many of 0-1000000 are palindromic.")
  ; Dependent on Droopy-library
  If MeasureHiResIntervalStart()
    ; Same Foo() as above...
    cnt=Foo(1000000)
    Timed=MeasureHiResIntervalStop()
  EndIf
  PrintN("The function need "+StrF(Timed*1000,3)+" msec,")
  PrintN("and "+Str(cnt)+" are palindromic.")
  Print("Press ENTER to exit."): Input()
EndIf
```


 Starting timing of a calculation,
 for this we test how many of 0-1000000 are palindromic.
 The function need 604.341 msec,
 and 1999 are palindromic.
 Press ENTER to exit.

This version still relies on the Windows API but does not make use of any additional libraries.

```PureBasic
Procedure.f ticksHQ(reportIfPresent = #False)
  Static maxfreq.q
  Protected T.q
  If reportIfPresent Or maxfreq = 0
    QueryPerformanceFrequency_(@maxfreq)
    If maxfreq
      ProcedureReturn 1.0
    Else
      ProcedureReturn 0
    EndIf
  EndIf
  QueryPerformanceCounter_(@T)
  ProcedureReturn T / maxfreq ;Result is in milliseconds
EndProcedure

If OpenConsole()
  Define timed.f, cnt
  PrintN("Starting timing of a calculation,")
  PrintN("for this we test how many of 0-1000000 are palindromic.")
  ; Dependent on Windows API
  If ticksHQ(#True)
    timed = ticksHQ() ;start time
    ; Same Foo() as above...
    cnt = Foo(1000000)
    timed = ticksHQ() - timed ;difference
  EndIf
  PrintN("The function need " + StrF(timed * 1000, 3) + " msec,")
  PrintN("and " + Str(cnt) + " are palindromic.")
  Print("Press ENTER to exit."): Input()
EndIf
```


Sample output:
 Starting timing of a calculation,
 for this we test how many of 0-1000000 are palindromic.
 The function need 174.811 msec,
 and 1999 are palindromic.


## Python

Given ''function'' and ''arguments'' return a time (in microseconds) it takes to make the call.

'''Note:''' There is an overhead in executing a function that does nothing.

```python
import sys, timeit
def usec(function, arguments):
    modname, funcname = __name__, function.__name__
    timer = timeit.Timer(stmt='%(funcname)s(*args)' % vars(),
                         setup='from %(modname)s import %(funcname)s; args=%(arguments)r' % vars())
    try:
        t, N = 0, 1
        while t < 0.2:
            t = min(timer.repeat(repeat=3, number=N))
            N *= 10
        microseconds = round(10000000 * t / N, 1) # per loop
        return microseconds
    except:
        timer.print_exc(file=sys.stderr)
        raise

 def nothing(): pass
 def identity(x): return x
```



### Example

 >>> print usec(nothing, [])
 1.7
 >>> print usec(identity, [1])
 2.2
 >>> print usec(pow, (2, 100))
 3.3
 >>> print [usec(qsort, (range(n),)) for n in range(10)]
 [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]
using ''qsort()'' from [[Quicksort#Python|Quicksort]]. Timings show that the implementation of ''qsort()'' has quadratic dependence on sequence length ''N'' for already sorted sequences (instead of ''O(N*log(N))'' in average).


## R

R has a built-in function, system.time, to calculate this.

```R
# A task
foo <- function()
{
   for(i in 1:10)
   {
      mat <- matrix(rnorm(1e6), nrow=1e3)
      mat^-0.5
   }
}
# Time the task
timer <- system.time(foo())
# Extract the processing time
timer["user.self"]
```

For a breakdown of processing time by function, there is Rprof.

```R
Rprof()
foo()
Rprof(NULL)
summaryRprof()
```



## Racket



```racket

#lang racket
(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(time (fact 5000))

```



## Raven


```Raven
define doId use $x
   $x dup * $x /

define doPower use $v, $p
   $v $p pow

define doSort
   group
      20000 each choose
   list sort reverse

define timeFunc use $fName
   time as $t1
   $fName "" prefer call
   time as $t2
   $fName $t2 $t1 -"%.4g secs for %s\n" print

"NULL" timeFunc
42 "doId" timeFunc
12 2 "doPower" timeFunc
"doSort" timeFunc
```

{{out}}

```txt
2.193e-05 secs for NULL
2.003e-05 secs for doId
4.601e-05 secs for doPower
3.028 secs for doSort
```



## Retro

Retro has a '''time''' function returning the current time in seconds. This can be used to build a simple timing function:


```Retro
: .runtime ( a- ) time [ do time ] dip - "\n%d\n" puts ;

: test 20000 [ putn space ] iterd ;
&test .runtime
```


Finer measurements are not possible with the standard implementation.


## REXX


### elapsed time version

REXX doesn't have a language feature for obtaining true CPU time (except under

IBM mainframes which have commands that can retrieve such times), but it does

have a built-in function for elapsed time(s).

The main reason for the true CPU time omission is that REXX was developed under VM/CMS and

there's a way to easily query the host (VM/CP) to indicate how much   ''true''   CPU time was used by

(normally) your own userID.  The result can then be placed into a REXX variable (as an option).

```rexx
/*REXX program displays the elapsed time for a REXX function (or subroutine). */
arg reps .                             /*obtain an optional argument from C.L.*/
if reps==''  then reps=100000          /*Not specified?  No, then use default.*/
call time 'Reset'                      /*only the 1st character is examined.  */
junk = silly(reps)                     /*invoke the  SILLY  function (below). */
                                       /*───►   CALL SILLY REPS    also works.*/

             /*                          The    E   is for    elapsed    time.*/
             /*                                 │             ─               */
             /*                        ┌────◄───┘                             */
             /*                        │                                      */
             /*                        ↓                                      */
say 'function SILLY took' format(time("E"),,2)  'seconds for' reps "iterations."
             /*                             ↑                                 */
             /*                             │                                 */
             /*            ┌────────►───────┘                                 */
             /*            │                                                  */
             /* The above  2  for the  FORMAT  function displays the time with*/
             /* two decimal digits (rounded)  past the decimal point).  Using */
             /* a   0  (zero)    would round the  time  to whole seconds.     */
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
silly: procedure               /*chew up some CPU time doing some silly stuff.*/
            do j=1  for arg(1) /*wash,  apply,  lather,  rinse,  repeat.  ··· */
            @.j=random() date() time() digits() fuzz() form() xrange() queued()
            end   /*j*/
 return j-1
```

'''output'''   when using a personal computer built in the 20th century:

```txt

function SILLY took 3.54 seconds for 100000 iterations.

```

'''output'''   when using a personal computer built in the 21st century:

```txt

function SILLY took 0.44 seconds for 100000 iterations.

```

'''output'''   when using an IBM mainframe with MVS/TSO:

```txt

function SILLY took 0.69 seconds for 100000 iterations.

```



### CPU time used version

This version   ''only''   works with Regina REXX as the   '''J'''   option   (for the '''time''' BIF)   is a Regina extension.

Since the   '''silly'''   function (by far) consumes the bulk of the CPU time of the REXX program, what is

being measured is essentially the same as the wall clock time (duration) of the function execution;   the

overhead of the invocation is minimal compared to the overall time used.

```rexx
/*REXX program displays the elapsed time for a REXX function (or subroutine). */
arg reps .                             /*obtain an optional argument from C.L.*/
if reps==''  then reps=100000          /*Not specified?  No, then use default.*/
call time 'Reset'                      /*only the 1st character is examined.  */
junk = silly(reps)                     /*invoke the  SILLY  function (below). */
                                       /*───►   CALL SILLY REPS    also works.*/

             /*                          The   J   is for the CPU time used   */
             /*                                │   by the REXX program since  */
             /*                        ┌───────┘   since the time was  RESET. */
             /*                        │           This is a Regina extension.*/
             /*                        ↓                                      */
say 'function SILLY took' format(time("J"),,2)  'seconds for' reps "iterations."
             /*                             ↑                                 */
             /*                             │                                 */
             /*            ┌────────►───────┘                                 */
             /*            │                                                  */
             /* The above  2  for the  FORMAT  function displays the time with*/
             /* two decimal digits (rounded)  past the decimal point).  Using */
             /* a   0  (zero)    would round the  time  to whole seconds.     */
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
silly: procedure               /*chew up some CPU time doing some silly stuff.*/
            do j=1  for arg(1) /*wash,  apply,  lather,  rinse,  repeat.  ··· */
            @.j=random() date() time() digits() fuzz() form() xrange() queued()
            end   /*j*/
 return j-1
```

'''output'''   is essentially identical to the previous examples.





## Ring


```ring

beginTime = TimeList()[13]
for n = 1 to 10000000
    n = n + 1
next
endTime = TimeList()[13]
elapsedTime = endTime - beginTime
see "Elapsed time = " + elapsedTime + nl

```



## Ruby

Ruby's Benchmark module provides a way to generate nice reports (numbers are in seconds):

```ruby
require 'benchmark'

Benchmark.bm(8) do |x|
  x.report("nothing:")  {  }
  x.report("sum:")  { (1..1_000_000).inject(4) {|sum, x| sum + x} }
end
```

Output:
               user     system      total        real
 nothing:  0.000000   0.000000   0.000000 (  0.000014)
 sum:      2.700000   0.400000   3.100000 (  3.258348)

You can get the total time as a number for later processing like this:

```ruby
Benchmark.measure { whatever }.total
```



## Scala

Define a <code>time</code> function that returns the elapsed time (in ms) to execute a block of code.

```scala

def time(f: => Unit)={
	val s = System.currentTimeMillis
	f
	System.currentTimeMillis - s
}

```

Can be called with a code block:

```scala

println(time {
	for(i <- 1 to 10000000) {}
})

```

Or with a function:

```scala

def count(i:Int) = for(j <- 1 to i){}

println(time (count(10000000)))

```



## Scheme


```scheme
(time (some-function))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "time.s7i";
  include "duration.s7i";

const func integer: identity (in integer: x) is
  return x;

const func integer: sum (in integer: num) is func
  result
    var integer: result is 0;
  local
    var integer: number is 0;
  begin
    result := num;
    for number range 1 to 1000000 do
      result +:= number;
    end for;
  end func;

const func duration: timeIt (ref func integer: aFunction) is func
  result
    var duration: result is duration.value;
  local
    var time: before is time.value;
  begin
    before := time(NOW);
    ignore(aFunction);
    result := time(NOW) - before;
  end func;

const proc: main is func
  begin
    writeln("Identity(4) takes " <& timeIt(identity(4)));
    writeln("Sum(4)      takes " <& timeIt(sum(4)));
  end func;
```


{{out}} of interpreted program:
 Identity(4) takes 0-00-00 00:00:00.000163
 Sum(4)      takes 0-00-00 00:00:00.131823

{{out}} of compiled program (optimized with -O2):
 Identity(4) takes 0-00-00 00:00:00.000072
 Sum(4)      takes 0-00-00 00:00:00.000857


## Sidef


```ruby
var benchmark = frequire('Benchmark')

func fac_rec(n) {
    n == 0 ? 1 : (n * __FUNC__(n - 1))
}

func fac_iter(n) {
    var prod = 1
    n.times { |i|
        prod *= i
    }
    prod
}

var result = benchmark.timethese(-3, Hash(
    'fac_rec'  => { fac_rec(20)  },
    'fac_iter' => { fac_iter(20) },
))

benchmark.cmpthese(result)
```

{{out}}

```txt

Benchmark: running fac_iter, fac_rec for at least 3 CPU seconds...
  fac_iter:  3 wallclock secs ( 3.23 usr +  0.00 sys =  3.23 CPU) @ 7331.89/s (n=23682)
   fac_rec:  3 wallclock secs ( 3.19 usr +  0.00 sys =  3.19 CPU) @ 3551.72/s (n=11330)
           Rate  fac_rec fac_iter
fac_rec  3552/s       --     -52%
fac_iter 7332/s     106%       --

```



## Slate


```slate

[inform: 2000 factorial] timeToRun.

```



## Smalltalk

(Squeak/Pharo)

```smalltalk

Time millisecondsToRun: [
	Transcript show: 2000 factorial ].

```



## Standard ML


```sml
fun time_it (action, arg) = let
  val timer = Timer.startCPUTimer ()
  val _ = action arg
  val times = Timer.checkCPUTimer timer
in
  Time.+ (#usr times, #sys times)
end
```



### Example

 - print ("Identity(4) takes " ^ Time.toString (time_it (fn x => x, 4)) ^ " seconds.\n");
 Identity(4) takes 0.000 seconds.
 val it = () : unit
 - fun sum (x:IntInf.int) = let
     fun loop (i, sum) =
       if i >= 1000000 then sum
       else loop (i + 1, sum + i)
   in loop (0, x)
   end;
 val sum = fn : IntInf.int -> IntInf.int
 - print ("Sum(4) takes " ^ Time.toString (time_it (sum, 4)) ^ " seconds.\n");
 Sum(4) takes 0.220 seconds.
 val it = () : unit


## Stata

Stata can track up to 100 timers. See '''[http://www.stata.com/help.cgi?timer timer]''' in Stata help.


```stata
program timer_test
	timer clear 1
	timer on 1
	sleep `0'
	timer off 1
	timer list 1
end

. timer_test 1000
   1:      1.01 /        1 =       1.0140
```


## Tcl

The Tcl <code>time</code> command returns the real time elapsed
averaged over a number of iterations.

```tcl
proc sum_n {n} {
    for {set i 1; set sum 0.0} {$i <= $n} {incr i} {set sum [expr {$sum + $i}]}
    return [expr {wide($sum)}]
}

puts [time {sum_n 1e6} 100]
puts [time {} 100]
```

{{out}}
 163551.0 microseconds per iteration
 0.2 microseconds per iteration


## TorqueScript


[[User:Greek2me|Greek2me]] 02:16, 19 June 2012 (UTC)

Returns average time elapsed from many iterations.

```TorqueScript

function benchmark(%times,%function,%a,%b,%c,%d,%e,%f,%g,%h,%i,%j,%k,%l,%m,%n,%o)
{
	if(!isFunction(%function))
	{
		warn("BENCHMARKING RESULT FOR" SPC %function @ ":" NL "Function does not exist.");
		return -1;
	}

	%start = getRealTime();

	for(%i=0; %i < %times; %i++)
	{
		call(%function,%a,%b,%c,%d,%e,%f,%g,%h,%i,%j,%k,%l,%m,%n,%o);
	}

	%end = getRealTime();

	%result = (%end-%start) / %times;

	warn("BENCHMARKING RESULT FOR" SPC %function @ ":" NL %result);

	return %result;
}

```


{{out|Example}}

```TorqueScript

function exampleFunction(%var1,%var2)
{
	//put stuff here
}

benchmark(500,"exampleFunction","blah","variables");

==> BENCHMARKING RESULT FOR exampleFunction:
==> 13.257

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
SECTION test
LOOP n=1,999999
rest=MOD (n,1000)
IF (rest==0) Print n
ENDLOOP
ENDSECTION
time_beg=TIME ()
DO test
time_end=TIME ()
interval=TIME_INTERVAL (seconds,time_beg,time_end)
PRINT "'test' start at ",time_beg
PRINT "'test' ends  at ",time_end
PRINT "'test' takes ",interval," seconds"

```

{{out}}
 'test' start at 2011-01-15 14:38:22
 'test' ends  at 2011-01-15 14:38:31
 'test' takes 9 seconds


## UNIX Shell


```bash
$ time sleep 1
```


 real    0m1.074s
 user    0m0.001s
 sys     0m0.006s


## VBA


```vb
Public Declare Function GetTickCount Lib "kernel32.dll" () As Long
Private Function identity(x As Long) As Long
    For j = 0 To 1000
    identity = x
    Next j
End Function
Private Function sum(ByVal num As Long) As Long
    Dim t As Long
    For j = 0 To 1000
    t = num
    For i = 0 To 10000
        t = t + i
    Next i
    Next j
    sum = t
End Function
Private Sub time_it()
    Dim start_time As Long, finis_time As Long
    start_time = GetTickCount
    identity 1
    finis_time = GetTickCount
    Debug.Print "1000 times Identity(1) takes "; (finis_time - start_time); " milliseconds"
    start_time = GetTickCount
    sum 1
    finis_time = GetTickCount
    Debug.Print "1000 times Sum(1) takes "; (finis_time - start_time); " milliseconds"
End Sub
```
{{out}}
```txt
1000 times Identity(1) takes  0  seconds
1000 times Sum(1) takes  296  seconds
```


## Wart


```python
time 1+1
30000/1000000  # in microseconds
=> 2
```



## XPL0

This works fine under pure DOS but has problems under Windows.
Windows can execute other processes, although it could be argued
that this should be included as part of the total time
to accomplish the task at hand.
DOS does go off to service a timer interrupt,
but it's usually very fast, although beware of TSRs
that hook this interrupt handler.

There's a more serious problem with the GetTime intrinsic
under Windows XP.
GetTime provides microsecond resolution by combining
the BIOS timer interrupt count at location 046C
with the count in the 8254 chip (or its equivalent).
Unfortunately, Windows virtualizes the 8254
and thus the two values can be out of sync.


```XPL0
include c:\cxpl\codes;
int T0, T1, I;
[T0:= GetTime;
for I:= 1, 1_000_000 do [];
T1:= GetTime;
IntOut(0, T1-T0); Text(0, " microseconds^M^J");
]
```


{{out|Example output}} for a Duron 850 running DOS 5.0:

```txt

2354 microseconds

```



## Yabasic


```Yabasic
sub count(n)
	local i

	for i = 1 to n
	next i
end sub

count(1000000)

print peek("millisrunning"), " milliseconds"

t0 = peek("millisrunning")
count(10000000)
print peek("millisrunning")-t0, " milliseconds"
```



## zkl

In order to be as OS independent as possible, only system time is available.

```zkl
t:=Time.Clock.time; Atomic.sleep(3); (Time.Clock.time - t).println();
```

{{out}}

```txt
3
```

