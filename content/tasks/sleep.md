+++
title = "Sleep"
description = ""
date = 2019-09-13T21:59:12Z
aliases = []
[extra]
id = 2410
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Write a program that does the following in this order:
* Input an amount of time to sleep in whatever units are most natural for your language (milliseconds, seconds, ticks, etc.). This unit should be noted in comments or in a description.
* [[Hello world/Text|Print]] "Sleeping..."
* Sleep the main [[thread]] for the given amount of time.
* Print "Awake!"
* End.



## Related tasks

*   [[Nautical bell]]





## 360 Assembly

REENTRANT means the program can be called from several callers simultaneously. The program obtains storage (memory) at each invocation.
Sleep (logically swapped out task) is established through STIMER macro (SVC 47)

```360 Assembly

         START
         PRINT DATA,GEN
         YREGS ,                      REGISTER EQUATES (e.g. 0 = R0)
SLEEP    CSECT
SLEEP    AMODE 31                     addressing mode 31 bit
SLEEP    RMODE ANY                    loader determines 31 or 24
***********************************************************************
* REENTRANT. Logically swap out a task for a number of seconds
*            specified in PARM. Minimum 0, maximum 60 seconds
*
* MVS rexx (the original rexx) does not have a sleep function. This
* program can be called from rexx, assuming this program is in
* LINKLIST, as follows:
*
*         /* rexx */
*         wait_time = '6' /* number of seconds to sleep */
*         say 'Sleeping...'
*         address LINKMVS "SLEEP wait_time"  /* invoke SLEEP */
*         say 'Awake!
***********************************************************************
PROLOG   BAKR  R14,0                 satck caller's registers
         LR    R4,R1                 save parm pointer
         LR    R12,R15               entry point addr to R12
         USING SLEEP,R12             tell assembler about that
         B     AROUND                avoid abend S0C1
         DC    C'SLEEP '             CSECT NAME
         DC    C'C=2014.05.10 '      CHANGE DATE
         DC    C'A=&SYSDATE '        ASSEMBLY DATE
         DC    C'T=&SYSTIME '        CHANGE TIME
         DC    C'MarcvdM. '          PROGRAMMER NAME
AROUND   L     R10,0(0,R4)           load parm address in R10
         XR    R15,R15               clear R15
         LH    R15,0(0,R10)          load parm length in R15
         LR    R6,R15                save length in R6
         LTR   R15,R15               parm length 0?
         BZ    NOPARM                yes, exit before getmain
         C     R6,F2                 parmlength > 2 ?
         BH    NOPARM                yes, exit before getmain
        STORAGE OBTAIN,LENGTH=WALEN,LOC=ANY  get some storage
         LR    R9,R1                 address of storage in R9
         USING WAREAX,R9             base for data section (DSECT)
         MVC   EYECAT,=C'**MARC**'   make storage easy to find in dump
         MVC   SECONDS,C00           set field to F0F0
         C     R6,F1                 parmlength = 1?
         BNE   COPYSECS              no, copy both bytes
         MVC   SECONDS+1(1),2(R10)   yes, just copy one byte.
         B     TRTEST
COPYSECS MVC   SECONDS,2(R10)
* test supplied parameter for valid integer values
TRTEST   TRT   SECONDS(1),VALINT6    first parm byte no higher as 6?
         BNZ   NOPARM_REL            higher, release storage and return
         TRT   SECONDS+1(1),VALINT9  second byte valid?
         BNZ   NOPARM_REL            no, release storage and return
         CLC   SECONDS(1),=C'6'      first parm byte < 6?
         BNE   DOWAIT                yes, do wait
         CLC   SECONDS+1(1),=C'0'    first eq. 6, second > 0?
         BNE   NOPARM_REL            yes, release storage and return
DOWAIT   DS    0H
         MVC   WAWTO(DWTOL),DWTO     copy WTO list form to obtained st.
         MVC   WAWTO+18(2),SECONDS   copy in nr. of seconds
        WTO    MF=(E,WAWTO)          issue WTO, execute form
         MVC   HOURS,C00             zero out hours
         MVC   MINUTS,C00             and minutes
         MVC   REST,C00                and milliseconds
        STIMER WAIT,DINTVL=TIMEVAL   SVC 47: logical swap out (sleep)
         B     EXIT                  done
NOPARM_REL DS  0H
        STORAGE RELEASE,ADDR=(R9),LENGTH=WALEN  free obtained storage
         LA    R15,4                 set return code 4
         B     RETURN                return to caller
EXIT     DS    0H
        STORAGE RELEASE,ADDR=(R9),LENGTH=WALEN  free obtained storage
        WTO    ' Awake!',ROUTCDE=11   fixed wake-up string
NOPARM   EQU   *
RETURN   PR    ,                     return to caller
*
* --------------------------------------------------------------------
* CONSTANTS
* --------------------------------------------------------------------
DWTO     WTO    ' Sleeping... (XX seconds)',ROUTCDE=11,MF=L
DWTOL     EQU   *-DWTO             length of WTO list form
F1        DC    F'1'
F2        DC    F'2'
C00       DC    C'00'
VALINT6   DC    256XL1'01'
          ORG   *-16
VALOK6    DC    7XL1'00'           F0-F6: OFFSETS 240-246
VALINT9   DC    256XL1'01'
          ORG   *-16
VALOK9    DC    10XL1'00'          F0-F9: OFFSETS 240-249
          DS    0D
         LTORG ,                   FORCE DISPLACEMENT LITERALS
* --------------------------------------------------------------------
* DSECT (data section)
* --------------------------------------------------------------------
WAREAX   DSECT ,
WAWTO    DS    CL(DWTOL)           reentrant WTO area
EYECAT   DS    CL8
TIMEVAL  DS    0CL8
HOURS    DS    CL2                 will be zeroed
MINUTS   DS    CL2                 will be zeroed
SECONDS  DS    CL2                 from parm
REST     DS    CL2                 will be zeroed
WALEN    EQU   *-WAREAX            length of DSECT
* --------------------------------------------------------------------
         END   SLEEP

```

'''output''' invoked with PARM='6' (+ sign indicates "problem state" (non system key) execution
<pre style="overflow:scroll">
+ Sleeping... (06 seconds)
+ Awake!

```



## 8051 Assembly

Input and output is dependent on hardware. The time units are machine cycles, which depends both on the oscillator frequency and the oscillator periods to machine cycle conversion factor. This code puts the processor into 'idle' mode, where code execution is stopped and resumed via an interrupt.

```asm
ORG RESET
	jmp main
ORG TIMER0
	; timer interrupt only used to wake the processor
	clr tr0
	reti

main:
	setb ea		; enable interrupts
	setb et0	; enable timer0 interrupt
	mov tl0, #0	; start timer counter at zero
	mov th0, #0	; these two values dictate the length of sleep

	mov a, pcon	; copy power control register
	setb a.0	; set idl bit
	setb tr0	; start timer
	; sleeping...
	mov pcon, a	; move a back into pcon (processor sleeps after this instruction finishes)

	; when the timer overflows and the timer interrupt returns, execution will resume at this spot

	; Awake!
	jmp $
```



## 8th


```forth

f:stdin f:getline
"Sleeping..." . cr
eval sleep
"Awake!" . cr bye

```



## Ada


The Ada delay statement takes an argument of type Duration, which is a real number counting the number of seconds to delay. Thus, 2.0 will delay 2.0 seconds, while 0.001 will delay 0.001 seconds.


```ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;

procedure Sleep is
   In_Val : Float;
begin
   Get(In_Val);
   Put_Line("Sleeping...");
   delay Duration(In_Val);
   Put_Line("Awake!");
end Sleep;
```



## Aime


```aime
o_text("Sleeping...\n");

# Sleep X seconds
sleep(atoi(argv(1)));

# Sleep X microseconds
#usleep(atoi(argv(1)));

o_text("Awake!\n");
```



## ALGOL 68


Only works for Microsoft Windows because it uses Windows-specific ping syntax.

```algol68
# using ping to sleep #
INT milliseconds = read int; # ping uses milliseconds #
print ("Sleeping...");
VOID (system ("ping 0.0.0.1 -n 1 -w " + whole (milliseconds, 0) + " >NUL"));
# 0.0.0.1 is an invalid IP address and cannot be used, so this will never conflict with a real IP address #
# ping -n gives number of tries, -w timeout, and >NUL deletes output so the user does not see it #
print (new line);
print ("Awake!")
```



## AntLang


```AntLang
milliseconds: eval[input["How long should I sleep? "]] / eval = evil, but this is just a simple demo
echo["Sleeping..."]
sleep[milliseconds]
echo["Awake!"]
```



## Applesoft BASIC

The cycles and times calculated should only be taken as a minimum delay.

```ApplesoftBasic
 10  POKE 768,169: POKE 770,76
 20  POKE 771,168: POKE 772,252
 30  INPUT "ENTER WAIT VALUE (1-256) : ";A
 40  IF A < 1 OR A > 256 THEN 30
 50  POKE 769,(A < 256) * A
 60  LET C = (26 + 27 * A + 5 * A ^ 2) / 2
 70  PRINT "WAIT FOR "C" CYCLES OR "
 80  PRINT C * 14 / 14.318181" MICROSECONDS"
 90  PRINT "SLEEPING": CALL 768: PRINT "AWAKE"
```

Output:
```txt
ENTER WAIT VALUE (1-256) : 256
WAIT FOR 167309 CYCLES OR
163591.032 MICROSECONDS
SLEEPING
AWAKE

```


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program sleepAsm.s   */

/* Constantes    */
.equ STDIN,  0                           @ Linux input console
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall
.equ SLEEP,  0xa2                        @ Linux syscall


.equ BUFFERSIZE,         100
/* Initialized data */
.data
szMessQuest:             .asciz "Enter the time to sleep in seconds : "
szMessError:             .asciz "Error occured.\n"
szMessSleep:             .asciz "Sleeping Zzzzzzz.\n"
szMessAwake:             .asciz "Awake!!!\n"

szCarriageReturn:        .asciz "\n"

/* UnInitialized data */
.bss
.align 4
ZonesAttente:
  iSecondes:      .skip 4
  iMicroSecondes: .skip 4
ZonesTemps:       .skip 8
sBuffer:          .skip BUFFERSIZE

/*  code section */
.text
.global main
main:
    ldr r0,iAdrszMessQuest            @ display invite message
    bl affichageMess
    mov r0,#STDIN                     @ input standard linux
    ldr r1,iAdrsBuffer
    mov r2,#BUFFERSIZE
    mov r7,#READ                      @ read input string
    svc 0
    cmp r0,#0                         @ read error ?
    ble 99f
    @
    ldr r0,iAdrsBuffer                @ buffer address
    bl conversionAtoD                 @ conversion string in number in r0

    ldr r1,iAdriSecondes
    str r0,[r1]                       @ store second number in area
    ldr r0,iAdrszMessSleep            @ display sleeping message
    bl affichageMess
    ldr r0,iAdrZonesAttente           @ delay area
    ldr r1,iAdrZonesTemps             @
    mov r7,#SLEEP                     @ call system SLEEP
    svc 0
    cmp r0,#0                         @ error sleep ?
    blt 99f
    ldr r0,iAdrszMessAwake            @ display awake message
    bl affichageMess
    mov r0, #0                        @ return code
    b 100f
99:                                   @ display error message
    ldr r0,iAdrszMessError
    bl affichageMess
    mov r0, #1                        @ return code

100:                                  @ standard end of the program
    mov r7, #EXIT                     @ request to exit program
    svc 0                             @ perform system call
iAdrszMessQuest:          .int szMessQuest
iAdrszMessError:          .int szMessError
iAdrszMessSleep:          .int szMessSleep
iAdrszMessAwake:          .int szMessAwake
iAdriSecondes:            .int iSecondes
iAdrZonesAttente:         .int ZonesAttente
iAdrZonesTemps:           .int ZonesTemps
iAdrsBuffer:              .int sBuffer
iAdrszCarriageReturn:     .int szCarriageReturn


/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return
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
    ldrb r5,[r4,r3]      @ loading in r5 of the byte located at the beginning + the position
    cmp r5,#0            @ end of string -> end routine
    beq 100f
    cmp r5,#0x0A         @ end of string -> end routine
    beq 100f
    cmp r5,#' '          @ space ?
    addeq r3,r3,#1       @ yes we loop by moving one byte
    beq 1b
    cmp r5,#'-'          @ first character is -
    moveq r6,#1          @  1 -> r6
    beq 3f               @ then move on to the next position
2:   /* beginning of digit processing loop */
    cmp r5,#'0'          @ character is not a number
    blt 3f
    cmp r5,#'9'          @ character is not a number
    bgt 3f
    /* character is a number */
    sub r5,#48
    ldr r1,iMaxi         @ check the overflow of the register
    cmp r0,r1
    bgt 99f              @ overflow error
    mul r0,r2,r0         @ multiply par factor 10
    add r0,r5            @ add to  r0
3:
    add r3,r3,#1         @ advance to the next position
    ldrb r5,[r4,r3]      @ load byte
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
    mov r0,#0            @ return  zero  if error
100:
    pop {r1-r7}          @ restaur other registers
    pop {fp,lr}          @ restaur   2 registers
    bx lr                @return procedure
/* constante program */
iMaxi: .int 1073741824
szMessErrDep:  .asciz  "Too large: overflow 32 bits.\n"
.align 4


```


## AutoHotkey


```AutoHotkey
TrayTip, sleeping, sleeping
sleep, 2000 ; 2 seconds
TrayTip, awake, awake
Msgbox, awake
```



## AutoIt


```AutoIt
#AutoIt Version: 3.2.10.0
$sleep_me=InputBox("Sleep", "Number of seconds to sleep", "10", "", -1, -1, 0, 0)
Dim $sleep_millisec=$sleep_me*1000
MsgBox(0,"Sleep","Sleeping for "&$sleep_me&" sec")
sleep ($sleep_millisec)
MsgBox(0,"Awake","... Awaking")
```



## AWK


```AWK

# syntax: GAWK -f SLEEP.AWK [seconds]
BEGIN {
    print("Sleeping...")
    loop(ARGV[1])
    print("Awake!")
    exit(0)
}
function loop(seconds,  t) {
# awk lacks a sleep mechanism, so simulate one by looping
    t = systime()
    while (systime() < t + seconds) {}
}

```

<p>commands and output:</p>

```txt

GAWK "BEGIN{print(strftime())}"
GAWK -f SLEEP.AWK 3
GAWK "BEGIN{print(strftime())}"

Wed Jan 16 18:06:44 Eastern Standard Time 2013
Sleeping...
Awake!
Wed Jan 16 18:06:47 Eastern Standard Time 2013

```



## Axe

The time unit for the Pause command is based on clock cycles, not seconds. At 15 MHz, one second is approximately equal to a value of 4500. At 6 MHz, one second is approximately 1800.


```axe
Disp "TIME:"
input→A
0→T
length(A)→L
For(I,1,L)
 If {A}<'0' or {A}>'9'
  Disp "NOT A NUMBER",i
  Return
 End
 T*10+{A}-'0'→T
 A++
End
Disp "SLEEPING...",i
Pause T
Disp "AWAKE",i
```



## BASIC

```qbasic
INPUT sec 'the SLEEP command takes seconds
PRINT "Sleeping..."
SLEEP sec
PRINT "Awake!"
```

"SLEEP" with no argument will sleep until a button is pressed on the keyboard (including modifier keys such as shift or control).  Also, pressing a key while SLEEP is waiting for a specific amount of time (as above) will end the SLEEP.

=
## Sinclair ZX81 BASIC
=
The <code>PAUSE</code> statement pauses execution for a length of time expressed in terms of the frame rate of the television you are using as a monitor. But there are one or two problems with it...

(1) Televisions in different countries have (had) different frame rates, so a one-second pause would need to be coded as <code>PAUSE 50</code> in Britain and <code>PAUSE 60</code> in the United States. The use of <code>PAUSE</code> therefore reduces compatibility.

(2) The highest acceptable value is 32767 frames: anything higher is taken to mean "pause forever".

(3) If the user presses a key, the computer will stop pausing and resume execution from the line after the <code>PAUSE</code>.

(4) In <code>FAST</code> mode the <code>PAUSE</code> statement needs to be followed by <code>POKE 16437,255</code> to avoid corrupting the program.

(5) The duration of the pause is not terribly precise.

(6) The screen flickers irritatingly when the pause ends, even if you are in <code>SLOW</code> mode.

Bearing all these factors in mind, it will often be found easier to use an empty <code>FOR</code> loop instead.

(Oh, and the ZX81 character set doesn't include lower-case letters or an exclamation mark: so the message <tt>Awake!</tt> has to be replaced by <tt>AWAKE.</tt>)


```basic
10 PRINT "HOW LONG SHOULD I SLEEP FOR?"
20 PRINT "(IN TELEVISION FRAMES)"
30 INPUT SLEEPTIME
40 PRINT "SLEEPING... ";
50 PAUSE SLEEPTIME
60 PRINT "AWAKE."
```


=== {{header|ZX Spectrum Basic}} ===

Pressing a key will cut the pause short on the ZX Spectrum.


```zxbasic
10 REM s is the number of seconds
20 LET s = 5
30 PRINT "Sleeping"
40 PAUSE s * 50
50 PRINT "Awake"
```



## Batch File

The usual way to do this is to use the <code>ping</code> utility which waits a second between multiple tries. To wait ''n'' seconds one tells <code>ping</code> to make ''n'' + 1 tries and redirects the output:

```dos
@echo off
set /p Seconds=Enter the number of seconds to sleep:
set /a Seconds+=1
echo Sleeping ...
ping -n %Seconds% localhost >nul 2>&1
echo Awake!
```

A similar trick can be used to wait a certain number of milliseconds. The <code>ping</code> utility includes a <code>/w</code> option which specifies the timeout to wait for a reply. This coupled with an unreachable address (where the full timeout will be needed) leads to the following:

```dos
@echo off
set /p MilliSeconds=Enter the number of milliseconds to sleep:
echo Sleeping ...
ping -n 1 -w %MilliSeconds% 1.2.3.4 >nul 2>&1
echo Awake!
```


Starting with Windows Vista there is a command-line utility to wait a number of seconds:

```dos
@echo off
set /p Seconds=Enter the number of seconds to sleep:
echo Sleeping ...
timeout /t %Seconds% /nobreak >nul
echo Awake!
```



## BBC BASIC

```bbcbasic
      INPUT "Enter the time to sleep in centiseconds: " sleep%
      PRINT "Sleeping..."
      WAIT sleep%
      PRINT "Awake!"
```

Whilst sleeping BBC BASIC for Windows periodically tests for the ESCape key being pressed.


## C


The function <tt>sleep</tt> needs seconds, which are read from the standard input.


```c
#include <stdio.h>
#include <unistd.h>

int main()
{
  unsigned int seconds;
  scanf("%u", &seconds);
  printf("Sleeping...\n");
  sleep(seconds);
  printf("Awake!\n");
  return 0;
}
```


## C#


```c#
using System;
using System.Threading;

class Program
{
    static void Main(string[] args)
    {
        int sleep = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine("Sleeping...");
        Thread.Sleep(sleep); //milliseconds
        Console.WriteLine("Awake!");
    }
}
```



## C++


```cpp
#include <iostream>
#include <thread>
#include <chrono>
int main()
{
    unsigned long microseconds;
    std::cin >> microseconds;
    std::cout << "Sleeping..." << std::endl;
    std::this_thread::sleep_for(std::chrono::microseconds(microseconds));
    std::cout << "Awake!\n";
}

```


```cpp
#include <unistd.h>
#include <iostream>

using namespace std;

int main(int argc, char* argv[])
{
    useconds_t microseconds;
    cin >> microseconds;
    cout << "Sleeping..." << endl;
    usleep(microseconds);
    cout << "Awake!" << endl;
    return 0;
}
```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>
SLEEP
    ; the HANG command can use fractional seconds; the Awake line will be slightly off due to processing time
    read "How long to sleep in seconds?: ",sleep
    write !,"Sleeping... time is "_$ztime($piece($ztimestamp,",",2,2),1,2)
    hang +sleep    ; use + to cast numeric, if non-numeric will hang 0
    write !,"Awake!  Time is "_$ztime($piece($ztimestamp,",",2,2),1,2)
    quit

```


```txt

SAMPLES>do ^SLEEP
How long to sleep in seconds?: 7.25
Sleeping... time is 14:48:29.27
Awake!  Time is 14:48:36.55

```



## Clojure


```clojure
(defn sleep [ms] ; time in milliseconds
  (println "Sleeping...")
  (Thread/sleep ms)
  (println "Awake!"))
; call it
(sleep 1000)
```



## COBOL

There are two methods for putting the program to sleep, both requiring unofficial extensions.
The first expects the amount of time to be in seconds.
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sleep-In-Seconds.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Seconds-To-Sleep       USAGE COMP-2.

       PROCEDURE DIVISION.
           ACCEPT Seconds-To-Sleep

           DISPLAY "Sleeping..."

           CALL "C$SLEEP" USING BY CONTENT Seconds-To-Sleep

           DISPLAY "Awake!"

           GOBACK
           .
```


While the second expects the time to be in nanoseconds. Note: Windows systems can only sleep to the nearest millisecond.
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sleep-In-Nanoseconds.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Seconds-To-Sleep       USAGE COMP-2.
       01  Nanoseconds-To-Sleep   USAGE COMP-2.
       01  Nanoseconds-Per-Second CONSTANT 1000000000.

       PROCEDURE DIVISION.
           ACCEPT Seconds-To-Sleep
           MULTIPLY Seconds-To-Sleep BY Nanoseconds-Per-Second
               GIVING Nanoseconds-To-Sleep

           DISPLAY "Sleeping..."

           CALL "CBL_OC_NANOSLEEP"
               USING BY CONTENT Nanoseconds-To-Sleep

           DISPLAY "Awake!"

           GOBACK
           .
```



## Common Lisp


```lisp
(defun test-sleep ()
  (let ((seconds (read)))
    (format t "Sleeping...~%")
    (sleep seconds)
    (format t "Awake!~%")))

(test-sleep)
```



## D


```d
import std.stdio, core.thread;

void main() {
    write("Enter a time to sleep (in seconds): ");

    long secs;
    readf(" %d", &secs);

    writeln("Sleeping...");
    Thread.sleep(dur!"seconds"(secs));
    writeln("Awake!");
}
```

```txt
Enter a time to sleep (in seconds): 5
Sleeping...
Awake!
```



## DCL


```DCL
$ amount_of_time = p1  ! hour[:[minute][:[second][.[hundredth]]]]
$ write sys$output "Sleeping..."
$ wait 'amount_of_time
$ write sys$output "Awake!"
```

```txt
$ @sleep 1  ! sleeps for 1 hour
Sleeping...
Awake!
$ @sleep 0:10  ! sleeps for 10 minutes
Sleeping...
Awake!
$ @sleep 0::10  ! sleeps for 10 seconds
Sleeping...
Awake!
$ @sleep 0:1:12  ! sleeps for 1 minute and 12 seconds
Sleeping...
Awake!
$ @sleep 23:59:59.99  ! sleeps for maximum amount of time
Sleeping...
Awake!
```



## Delphi


```Delphi
program SleepOneSecond;

{$APPTYPE CONSOLE}

uses SysUtils;

var
  lTimeToSleep: Integer;
begin
  if ParamCount = 0 then
    lTimeToSleep := 1000
  else
    lTimeToSleep := StrToInt(ParamStr(1));
  WriteLn('Sleeping...');
  Sleep(lTimeToSleep); // milliseconds
  WriteLn('Awake!');
end.
```



## E


You can't do that.

No, really. E's approach to timing, concurrency, and IO is non-blocking; if you want to wait for something, you say what you want to do when it happens — i.e. callbacks. There are no threads of control which can be stopped — except automatically when they just have nothing to do.

So, the closest thing possible to the task description is to wait for the specified time to pass, then do whatever the next thing is.


```e
def sleep(milliseconds :int, nextThing) {
    stdout.println("Sleeping...")
    timer.whenPast(timer.now() + milliseconds, fn {
        stdout.println("Awake!")
        nextThing()
    })
}
```



## EGL


```EGL
program Sleep type BasicProgram{}

    // Syntax: 	sysLib.wait(time BIN(9,2) in)

    function main()
    	SysLib.writeStdout("Sleeping!");
        sysLib.wait(15); // waits for 15 seconds
        SysLib.writeStdout("Awake!");
    end

end
```



## Eiffel


The feature <code lang="eiffel">sleep</code> is defined in the library class EXECUTION_ENVIRONMENT. So the demonstration class APPLICATION inherits from EXECUTION_ENVIRONMENT in order to make <code lang="eiffel">sleep</code> available.

<code lang="eiffel">sleep</code> takes an argument which declares the number of nanoseconds to suspend the thread's execution.


```eiffel
class
    APPLICATION
inherit
    EXECUTION_ENVIRONMENT
create
    make
feature -- Initialization
    make
            -- Sleep for a given number of nanoseconds.
        do
            print ("Enter a number of nanoseconds: ")
            io.read_integer_64
            print ("Sleeping...%N")
            sleep (io.last_integer_64)
            print ("Awake!%N")
        end
end
```


Output (sleeping 10 seconds):

```txt

Enter a number of nanoseconds: 10000000000
Sleeping...
Awake!

```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    int sleep := console.readLine().toInt();
    console.printLine("Sleeping...");
    system'threading'threadControl.sleep(sleep);
    console.printLine("Awake!")
}
```



## Elixir


```elixir
sleep = fn seconds ->
          IO.puts "Sleeping..."
          :timer.sleep(1000 * seconds)    #  in milliseconds
          IO.puts "Awake!"
        end

sec = if System.argv==[], do: 1, else: hd(System.argv) |> String.to_integer
sleep.(sec)
```



## Emacs Lisp


```lisp
(let ((seconds (read-number "Time in seconds: ")))
  (message "Sleeping ...")
  (sleep-for seconds)
  (message "Awake!"))
```


The time can be a decimal like 1.5 though the actual resolution of <code>sleep-for</code> depends on the operating system.  The similar <code>sit-for</code> stops sleeping if there's pending keyboard input.

<code>read-number</code> is new in Emacs 22 and XEmacs 21.  In earlier versions similar can be had with


```lisp
(string-to-number (read-string "Time in seconds: "))
```


This returns 0 on a non-number whereas <code>read-number</code> re-prompts (except in Emacs 24.3 where a bug caused <code>read-number</code> to return 0 :-( ).


## Erlang


Erlang doesn't really have such a thing as a main thread. However, sleeping any process can be done with the <tt>timer:sleep/1</tt> function:

```erlang
main() ->
    io:format("Sleeping...~n"),
    timer:sleep(1000), %% in milliseconds
    io:format("Awake!~n").
```


It is to be noted that Erlang's sleep function is implemented in Erlang with a timeout on a <tt>receive</tt>, so you may sometimes encounter the following way of sleeping a process:

```erlang
main() ->
    io:format("Sleeping...~n"),
    receive
    after 1000 -> ok %% in milliseconds
    end,
    io:format("Awake!~n").
```


which is the way it is implemented in the <tt>timer</tt> module.


## ERRE


```ERRE

      ..............
      INPUT("Enter the time to sleep in seconds: ";sleep)
      PRINT("Sleeping...")
      PAUSE(sleep)
      PRINT("Awake!")
      ..............

```


=={{header|F_Sharp|F#}}==
```fsharp
open System
open System.Threading

[<EntryPoint>]
let main args =
    let sleep = Convert.ToInt32(Console.ReadLine())
    Console.WriteLine("Sleeping...")
    Thread.Sleep(sleep); //milliseconds
    Console.WriteLine("Awake!")
    0
```



## Factor


```factor
USING: calendar io math.parser threads ;

: read-sleep ( -- )
    readln string>number seconds
    "Sleeping..." print
    sleep
    "Awake!" print ;
```



## Fantom


Fantom has a 'Duration' class, which uses time definitions with units: e.g.,  5sec, 100ns, 5hr.  These are used for input in the following program.


```fantom

using concurrent

class Main
{
  public static Void main ()
  {
    echo ("Enter a time to sleep: ")
    input := Env.cur.in.readLine
    try
    {
      time := Duration.fromStr (input)
      echo ("sleeping ...")
      Actor.sleep (time)
      echo ("awake!")
    }
    catch
    {
      echo ("Invalid time entered")
    }
  }
}

```


Output:

```txt

Enter a time to sleep:
5sec
sleeping ...
awake!

```



## FBSL


```qbasic
#APPTYPE CONSOLE
DIM %msec
PRINT "Milliseconds to sleep: ";
%msec = FILEGETS(stdin, 10)
PRINT "Sleeping..."
SLEEP(%msec)
PRINT "Awake!"
PAUSE

```

Output

```txt
Milliseconds to sleep: 1000
Sleeping...
Awake!

Press any key to continue...
```



## Forth


```forth
: sleep ( ms -- )
  ." Sleeping..."
  ms
  ." awake." cr ;
```



### =Explanation note on MS=


MS ( n -- ) A.10.6.2.1905

MS is a Standard Forth word that waits for at least n milliseconds. It is part of the optional Facility Wordset. It is more than just a simple delay in that in a multi-tasking environment when MS is executed the current task is asleep until the time expires.


## Fortran


```fortran
program test_sleep

  implicit none
  integer :: iostat
  integer :: seconds
  character (32) :: argument

  if (iargc () == 1) then
    call getarg (1, argument)
    read (argument, *, iostat = iostat) seconds
    if (iostat == 0) then
      write (*, '(a)') 'Sleeping...'
      call sleep (seconds)
      write (*, '(a)') 'Awake!'
    end if
  end if

end program test_sleep
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim ms As UInteger
Input "Enter number of milliseconds to sleep" ; ms
Print "Sleeping..."
Sleep ms, 1  '' the "1" means Sleep can't be interrupted with a keystroke
Print "Awake!"
End
```


Sample input/output
```txt

Enter number of milliseconds to sleep? 3000
Sleeping...
Awake!

```



## Frink

In Frink, all values have units of measure, and sleep functions take units of time, which can be seconds, nanoseconds, minutes, hours, etc.  The user may enter values like "3 hours" or "1 ms".  The units of measure are captured as first-class values in the language, and not hidden in comments nor implied in APIs.

```frink

do
  t = eval[input["Enter amount of time to sleep: ", "1 second"]]
while ! (t conforms time)

println["Sleeping..."]
sleep[t]
println["Awake!"]

```



## Go

Technically, this varies from the task by sleeping the main ''goroutine'' rather than the main ''thread''.  The Go runtime multiplexes goroutines to operating system threads and the language does not provide direct access to threads.

```go
package main

import "time"
import "fmt"

func main() {
    fmt.Print("Enter number of seconds to sleep: ")
    var sec float64
    fmt.Scanf("%f", &sec)
    fmt.Print("Sleeping…")
    time.Sleep(time.Duration(sec * float64(time.Second)))
    fmt.Println("\nAwake!")
}
```



## Groovy

Solution:

```groovy
def sleepTest = {
    println("Sleeping...")
    sleep(it)
    println("Awake!")
}
```


Test:

```groovy
sleepTest(1000)
print '''
Hmmm. That was... less than satisfying.
How about this instead?
'''
Thread.start {
    (0..5).each {
        println it
        sleep(1000)
    }
}
sleepTest(5000)
```


Output:

```txt
Sleeping...
Awake!

Hmmm. That was... less than satisfying
How about this instead?
Sleeping...
0
1
2
3
4
Awake!
5
```



## Haskell


```haskell
import Control.Concurrent

main = do seconds <- readLn
          putStrLn "Sleeping..."
          threadDelay $ round $ seconds * 1000000
          putStrLn "Awake!"
```



## HicEst


```hicest
DLG(NameEdit = milliseconds, Button = "Go to sleep")
WRITE(StatusBar) "Sleeping ... "
SYSTEM(WAIT = milliseconds)
WRITE(Messagebox) "Awake!"
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

repeat {
   writes("Enter number of seconds to sleep :")
   s := reads()
   if s = ( 0 < integer(s)) then break
   }

write("\nSleeping for ",s," seconds.")
delay(1000 * s)
write("Awake!")
end
```



## IDL



```IDL

read,i,prompt='Input sleep time in seconds: '
print,'Sleeping...'
wait,i ; in seconds, but accepts floats(/fractional) as input
print,'Awake!'

```



## J


'''Solution''':

```j
sleep =: 6!:3

sleeping=: monad define
  smoutput 'Sleeping...'
  sleep y
  smoutput 'Awake!'
)
```


'''Example''':

```j
   sleeping 0.500          NB.  Sleep 500 milliseconds
Sleeping...
Awake!
```



## Java

```java5

import java.util.InputMismatchException;
import java.util.Scanner;

public class Sleep {
    public static void main(final String[] args) throws InterruptedException {
        try {
            int ms = new Scanner(System.in).nextInt(); //Java's sleep method accepts milliseconds
            System.out.println("Sleeping...");
            Thread.sleep(ms);
            System.out.println("Awake!");
        } catch (InputMismatchException inputMismatchException) {
            System.err.println("Exception: " + inputMismatchException);
        }
    }
}
```


=={{header|JavaScript}} (in a web browser)==

Generally, JavaScript in a web browser is event-loop based and (except for alert()) non-blocking. So, the closest thing possible to the task description is to do something once the specified time has passed.


```html><script


  setTimeout(function () {
    document.write('Awake!')
  }, prompt("Number of milliseconds to sleep"));

  document.write('Sleeping... ');

</script>
```



## Jsish


```javascript
/*
   Sleep, in Jsish
*/

printf('Sleep time (in milliseconds)? ');
var ms = parseInt(console.input());

puts('Sleeping...');
sleep(ms);
puts('Awake!');
```



## Julia



```Julia

print("Please enter sleep duration in seconds: ")
input = int(readline(STDIN))
println("Sleeping...")
sleep(input)
println("Awake!")

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    print("Enter number of milliseconds to sleep: ")
    val ms = readLine()!!.toLong()
    println("Sleeping...")
    Thread.sleep(ms)
    println("Awake!")
}
```

Sample input/output:
```txt

Enter number of milliseconds to sleep: 3000
Sleeping...
Awake!

```



## LabVIEW

Uses milliseconds. LabVIEW has no "main thread" so it must be forced with a sequence structure.
<br/>{{VI snippet}}<br/>
[[File: Sleep.png]]


## Lasso

Lasso has a built in sleep command that accepts milliseconds as an input.


```Lasso
stdoutnl('Sleeping...')
sleep(5000) // Sleep 5 seconds
stdoutnl('Awake!')
```



## Lhogho

The Logo version works without modification. Another way to Sleep, in the Windows version of Lhogho, is to use the Win32 function, viz

```logo
make "Void "V0
make "Long "U4
make "kernel32_handle libload "kernel32.dll
to Sleep :dwMilliseconds
end
external "Sleep [ Void Sleep Long] :kernel32_handle

to millisleep :n
	print [Sleeping...]
	Sleep :n                  ; units: 1/1000th of a second
	print [Awake.]
end
```



## Liberty BASIC


```lb
Input "Please input the number of milliseconds you would like to sleep. "; sleeptime
Print "Sleeping..."
CallDLL #kernel32, "Sleep", sleeptime As long, ret As void
Print "Awake!"

```



## Lingo

```lingo
on doSleep (ms)
  put "Sleeping..."
  sleep(ms)
  put "Awake!"
end
```



## Logo


```logo

to sleep :n
print [Sleeping...]
wait :n                  ; units: 1/60th of a second
print [Awake.]
end

```



## Logtalk

Works when using SWI-Prolog, XSB, or YAP as the backend compilers:

```logtalk

:- object(sleep).

    :- public(how_long/1).

    how_long(Seconds) :-
        write('Sleeping ...'), nl,
        thread_sleep(Seconds),
        write('... awake!'), nl.

:- end_object.

```

Sample output:

```text

| ?- sleep::how_long(5).
Sleeping ...
... awake!
yes

```



## Lua

The input does not need to be a whole number, eg. "0.5" would cause the program to wait for half a second.

```lua
require("socket")
io.write("Input a number of seconds to sleep: ")
local input = io.read("*number")
print("Sleeping")
socket.sleep(input)
print("Awake!")
```

A similar effect could be achieved using a "busy" loop but the function in lua-socket is gentler on your CPU.


## M2000 Interpreter

Statement Wait pause the current thread but other threads from module (not in this example) may run.

```M2000 Interpreter

Module CheckIt {
      Input "Input a number of milliseconds to sleep:", N
      Print "Sleeping..."
      Wait N
      Print "Awake"
}
CheckIt

```



## Maple


```Maple
sleep := proc(secs)
           print("Sleeping...");
           Threads:-Sleep(secs);
           print("Awake!");
         end proc:
```



## Mathematica

This function, as you can probably guess, takes its argument in seconds.
While this function does tie up execution (but not with a busy wait), the Mathematica front end remains fully functional and can be used to stop the sleeping with Evaluation -> Abort Evaluation.


```Mathematica

Sleep[seconds_] := (Print["Sleeping..."]; Pause[seconds]; Print["Awake!"];)

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function sleep()

    time = input('How many seconds would you like me to sleep for? ');
    assert(time > .01);
    disp('Sleeping...');
    pause(time);
    disp('Awake!');

end
```



## Nemerle


```Nemerle
using System;
using System.Console;
using System.Threading.Thread; // this is where the Sleep() method comes from

module Zzzz
{
    Main() : void
    {
        def nap_time = Int32.Parse(ReadLine());
        WriteLine("Sleeping...");
        Sleep(nap_time); // parameter is time in milliseconds
        WriteLine("Awake!");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sleep(secs) public static binary
  ms = (secs * 1000).format(null, 0) -- milliseconds, rounded to nearest integer
  say 'Sleeping...'
  do
    Thread.sleep(ms)
  catch ix = InterruptedException
    say 'Sleep interrupted!'
    ix.printStackTrace()
  end
  say 'Awake!'
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  secs = -1
  loop until \secs.datatype('N')
    if secs > 0 then do
      say 'Napping for' secs's'
      say
      sleep(secs)
    end
    say
    say 'How many seconds do you want me to sleep? (enter something non-numeric to terminate)\-'
    parse ask secs .
    say
    end
  say
  say 'Goodbye...'
  say
  return

```



## NewLISP


```NewLISP
(println "Sleeping..." )
(sleep 2000) ; Wait for 2 seconds
(println "Awake!")
```



## Nim


```Nim
import os, strutils

echo("Enter how long I should sleep (in milliseconds):")
var timed = parseInt(readLine(stdin).string)
echo("Sleeping...")
sleep(timed)
echo("Awake!")
```



## Objeck


```objeck

bundle Default {
  class Test {
    function : Main(args : System.String[]) ~ Nil {
      if(args->Size() = 1) {
        "Sleeping..."->PrintLine();
        Thread->Sleep(args[0]->ToInt());
        "Awake!"->PrintLine();
      };
    }
  }
}

```


=={{header|Objective-C}}==
Of course the same code of [[Sleep#C]] works for Objective-C. The following code uses a OpenStep derived framework (Cocoa, GNUstep...).

```objc>#import <Foundation/Foundation.h


int main()
{
  @autoreleasepool {

    NSTimeInterval sleeptime;
    printf("wait time in seconds: ");
    scanf("%f", &sleeptime);

    NSLog(@"sleeping...");
    [NSThread sleepForTimeInterval: sleeptime];
    NSLog(@"awakening...");

  }
  return 0;
}
```



## OCaml



```ocaml
#load "unix.cma";;
let seconds = read_int ();;
print_endline "Sleeping...";;
Unix.sleep seconds;; (* number is integer in seconds *)
print_endline "Awake!";;
```


or

```ocaml
#load "unix.cma";;
#directory "+threads";;
#load "threads.cma";;
let seconds = read_float ();;
print_endline "Sleeping...";;
Thread.delay seconds;; (* number is in seconds ... but accepts fractions *)
print_endline "Awake!";;
```



## Oforth


```Oforth
 : sleepMilli(n)  "Sleeping..." . n sleep "Awake!" println ;
```



## ooRexx


```oorexx
Say time()
Call sysSleep 10 -- wait 10 seconds
Say time()
```

```txt
14:23:40
14:23:50
```



## Oz


```oz
declare
  class TextFile from Open.file Open.text end
  StdIn = {New TextFile init(name:stdin)}
  WaitTime = {String.toInt {StdIn getS($)}}
in
  {System.showInfo "Sleeping..."}
  {Delay WaitTime} %% in milliseconds
  {System.showInfo "Awake!"}
```



## PARI/GP

GP does not have threading built in and so cannot truly sleep; this is code for spin-idling.


### gettime

The units are milliseconds.

```parigp
sleep(ms)={
  print("Sleeping...");
  while((ms-=gettime()) > 0,);
  print("Awake!")
};

sleep(input())
```



### alarm

The units are seconds.

```parigp
sleep(s)={
  print("Sleeping...");
  alarm(s);
  trap(alarmer,,while(1,));
  print("Awake!")
};

sleep(input())
```



## Pascal

See [[Sleep#Delphi | Delphi]]


## Peloton

Literate mode

```sgml><@ SAYLIT>Number of seconds: </@><@ GETVAR>secs</@

<@ SAYLIT>Sleeping</@>
<@ ACTPAUVAR>secs</@>
<@ SAYLIT>Awake</@>
```


French variable-length opcodes

```sgml
<# MontrezLittéralement>Number of seconds: </#><# PrenezUneValeurVariable>secs</#>
<# MontrezLittéralement>Sleeping</#>
<# AgissezFaireUnePauseVariable>secs</#>
<# MontrezLittéralement>Awake</#>
```


(Simplified) Chinese fixed-length opcodes

```sgml
<@ 显示_字串_>Number of seconds: </@><@ 获取_变量_>secs</@>
<@ 显示_字串_>Sleeping</@>
<@ 运行_暂停动变量_>secs</@>
<@ 显示_字串_>Awake</@>
```



## Perl


seconds:

```perl
$seconds = <>;
print "Sleeping...\n";
sleep $seconds; # number is in seconds
print "Awake!\n";
```


microseconds and nanoseconds using the Time::HiRes module:

```perl
use Time::HiRes qw( usleep nanosleep );

$microseconds = <>;
print "Sleeping...\n";
usleep $microseconds;
print "Awake!\n";

$nanoseconds = <>;
print "Sleeping...\n";
nanosleep $nanoseconds;
print "Awake!\n";
```


It's also possible to sleep for fractional seconds by abusing the select function:

```perl
say "Sleeping...";
select undef, undef, undef, 0.5;
say "Awake!";
```



## Perl 6


The <tt>sleep</tt> function argument is in units of seconds, but these may be fractional (to the limits of your system's clock).


```perl6
my $sec = prompt("Sleep for how many microfortnights? ") * 1.2096;
say "Sleeping...";
sleep $sec;
say "Awake!";
```


Note that 1.2096 is a rational number in Perl 6, not floating point, so precision can be maintained even when dealing with very small powers of ten.


## Phix


```Phix
atom a = prompt_number("wait for duration (in seconds, 0..20):", {0,20})
puts(1,"Sleeping...\n")
sleep(a)
puts(1,"Awake!\n")
```



## PHP


seconds:

```php
$seconds = 42;
echo "Sleeping...\n";
sleep($seconds); # number is integer in seconds
echo "Awake!\n";
```


microseconds:

```php
$microseconds = 42000000;
echo "Sleeping...\n";
usleep($microseconds); # number is integer in microseconds
echo "Awake!\n";
```


nanoseconds:

```php
$nanoseconds = 42000000000;
echo "Sleeping...\n";
time_nanosleep($seconds, $nanoseconds); # first arg in seconds plus second arg in nanoseconds
echo "Awake!\n";
```



## PicoLisp


```PicoLisp
(prinl "Sleeping..." )
(wait 2000)                # Wait for 2 seconds
(prinl "Awake!")
```

As [http://software-lab.de/doc/refW.html#wait wait] will continue executing
background events, another possibility (for a complete stop) is calling
some external program like

```PicoLisp
(prinl "Sleeping..." )
(call 'sleep 2)            # Wait for 2 seconds
(prinl "Awake!")
```



## PL/I


```PL/I

put ('sleeping');
delay (2000); /* wait for 2 seconds (=2000 milliseconds). */
put ('awake');

```



## PowerShell


```powershell
$d = [int] (Read-Host Duration in seconds)
Write-Host Sleeping ...
Start-Sleep $d
Write-Host Awake!
```

The <code>-Milliseconds</code> parameter to <code>Start-Sleep</code> can be used to allow for sub-second precision in sleeping.


## Prolog

Works with SWI-Prolog.

```Prolog
rosetta_sleep(Time) :-
	writeln('Sleeping...'),
	sleep(Time),
	writeln('Awake!').

```



## PureBasic

Sleeping is performed with Delay() and a value in milliseconds.  The time is accurate to approximately +/- 15 milliseconds.

```PureBasic
If OpenConsole()

  Print("Enter a time(milliseconds) to sleep: ")
  x.i = Val(Input())
  PrintN("Sleeping...")
  Delay(x) ;in milliseconds
  PrintN("Awake!")
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python



```python
import time

seconds = float(raw_input())
print "Sleeping..."
time.sleep(seconds) # number is in seconds ... but accepts fractions
print "Awake!"
```



## R

The call to flush.console is only needed if buffering is turned on.  See [http://cran.r-project.org/bin/windows/base/rw-FAQ.html#The-output-to-the-console-seems-to-be-delayed FAQ for R on windows].  The time is given in seconds (fractions allowed, resolution is system dependent).

```R

sleep <- function(time=1)
{
   message("Sleeping...")
   flush.console()
   Sys.sleep(time)
   message("Awake!")
}

sleep()

```



## Racket


```racket

#lang racket
(displayln "Enter a time (in seconds): ")
(define time (read))
(when (number? time)
  (displayln "Sleeping...")
  (sleep time)
  (displayln "Awake!"))

```



## RapidQ


```vb

input "Enter the number of seconds to sleep: ";s
sleep s
print "I'm awake I think..."
input "Press enter to quit";a$

```



## REBOL


```REBOL
REBOL [
    Title: "Sleep Main Thread"
    URL: http://rosettacode.org/wiki/Sleep_the_Main_Thread
]

naptime: to-integer ask "Please enter sleep time in seconds: "
print "Sleeping..."
wait naptime
print "Awake!"
```



## Red


```Red

str-time: to integer! ask "Enter wait time "    ;get user input , convert to integer
print "waiting"
wait str-time       ;Seconds
print "awake"
```



## Retro

Retro has no fine grained timer; so we have to make due with seconds.


```Retro
: sleep ( n- )
  [ time [ time over - 1 > ] until drop ] times ;
  : test
    "\nTime to sleep (in seconds): " puts getToken toNumber
    "\nSleeping..." sleep
    "\nAwake!\n" ;
```



## REXX


### using the DELAY BIF

This REXX version supplied   ''as is''   works with   (without the accompanying external program shown below):
:::* PC/REXX
:::* Personal REXX

Note:   the above two REXX interpreters support fractional seconds.

```rexx
/*REXX program sleeps  X  seconds  (the number of seconds is supplied via the argument).*/
parse arg secs .                                 /*obtain optional argument from the CL.*/
if secs=='' | secs==","  then secs=0             /*Not specified?  Then assume 0 (zero).*/
say 'Sleeping'    secs    "seconds."             /*inform the invoker what's happening. */
call delay secs                                  /*Snooze.  Hopefully, just a short nap.*/
say 'Awake!'                                     /*and now inform invoker we're running.*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the following for input:   <tt> 4.7 </tt>

```txt

Sleeping 4.7 seconds.
Awake!

```



### using the DELAY routine

The above REXX program (using DELAY) will work with most REXXes:
:::*   CMS REXX
:::*   PC/REXX   (see note)
:::*   Personal REXX   (see note)
:::*   REGINA REXX
:::*   ROO REXX
:::*   R4 REXX
:::*   TSO REXX
:::*   (Microsoft) DOS
:::*   (Microsoft) Windows
:::*   any system that supports the   PING   command
when used in conjunction with the following program (either external or imbedded).


Note:   when PC/REXX or Personal REXX are used, those REXXes already have a built-in function (BIF), so the   '''delay'''    subroutine (below) will never be executed, but for other REXXes, the   '''DELAY'''   BIF will be used instead.

This REXX program only uses whole seconds   (fractional seconds are ignored).

```rexx
/*REXX program delays (or SLEEPS) a number of whole seconds; fractional secs are ignored*/
trace off                                              /*suppress REXX error messages.  */
parse arg !                                            /*obtain all the arguments.      */
if !all(arg()) then exit                               /*documentation requested ?      */
if !cms  then address ''                               /*if CMS, then use fast cmd path.*/
signal on halt                                         /*handle  HALT  gracefully.      */
signal on noValue                                      /*handle the REXX noValue error. */
signal on syntax                                       /*handle the REXX syntax errors. */

          /*┌────────────────────────────────────────────────────────────────────┐
          ┌─┘ The  DELAY  function is used to delay (wait) a specific amount of  └─┐
          │ (wall-clock)  time specified in seconds.  Any fraction part is ignored.│
          │                                                                        │
          │ If the REXX program invoking  DELAY  function is running under PC/REXX │
          │ or  Personal REXX,  this REXX program should never be invoked as those │
          └─┐ REXXes have their own built-in function (BIF)  named   "DELAY".    ┌─┘
            └────────────────────────────────────────────────────────────────────┘*/

@cpsleep  = 'CP SLEEP'                                 /*point to the (CP) SLEEP command*/
@ping     = 'PING'                                     /*  "    "  "  DOS  PING     "   */

parse var ! n _                                        /*parse argument from the parms. */
if _\=='' | arg()>1  then call er 59                   /*are there too many arguments ? */
if n==''             then n=1                          /*No args?  Then assume  1 second*/
if \isNum(n)  then call er 53,n 'delay-seconds'        /*is   n   not numeric?   Error. */
n=n%1                                                  /*elide any fractional second.   */
if n<=0  then return 0
                        /* ┌────────────────────┐ */
                        /* │ delay  n  seconds. │ */
                        /* └────────────────────┘ */
  select
  when !cms     then @cpsleep  n%1  "SEC"              /*is this CMS?  Then use CP SLEEP*/
  when !tso     then call sleep n%1                    /* "   "  TSO?    "   "  SLEEP   */
  when !regina  then call sleep n%1                    /* "   " Regina?  "   "    "     */
  when !dos     then @ping '-n' n "127.0.0.1 > NUL"    /* "   "  DOS?    "   "   PING   */
  otherwise          nop
  end   /*select*/

return 0                                               /*return a zero value to invoker.*/
/*─────────────────────────────general 1─line subroutines───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=="NT";!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,"? ?SAMPLES ?AUTHOR ?FLOW")==0 then return 0;!call=']$H';call "$H" !fn !;!call=;return 1
!cal:    if symbol('!CALL')\=="VAR"  then !call=;        return !call
!env:    !env='ENVIRONMENT'; if !sys=="MSDOS" | !brexx | !r4 | !roo  then !env='SYSTEM';  if !os2  then !env="OS2"!env;  !ebcdic=3=='f3'x;  if !crx  then !env="DOS";      return
!fid:    parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;  call !sys;  if !dos  then do;  _=lastpos('\',!fn);  !fm=left(!fn,_);  !fn=substr(!fn,_+1); parse var !fn !fn "." !ft; end;   return word(0 !fn !ft !fm, 1+('0'arg(1)))
!rex:    parse upper version !ver !vernum !verdate .;  !brexx='BY'==!vernum;  !kexx="KEXX"==!ver;  !pcrexx='REXX/PERSONAL'==!ver | "REXX/PC"==!ver;  !r4='REXX-R4'==!ver;  !regina="REXX-REGINA"==left(!ver,11);  !roo='REXX-ROO'==!ver; call !env; return
!sys:    !cms=!sys=='CMS';  !os2=!sys=="OS2";  !tso=!sys=='TSO' | !sys=="MVS";  !vse=!sys=='VSE';  !dos=pos("DOS",!sys)\==0 | pos('WIN',!sys)\==0 | !sys=="CMD";  !crx=left(!sys,6)=='DOSCRX';  call !rex;   return
!var:    call !fid;  if !kexx  then return space(dosenv(arg(1)));   return space(value(arg(1), , !env))
er:      parse arg _1,_2;  call '$ERR'  "14"p(_1)  p(word(_1,2)  !fid(1))  _2;   if _1<0  then return _1;     exit result
p:       return word(arg(1), 1)
halt:    call er .1
isNum:   return datatype(arg(1), 'N')
noValue: !sigl=sigl;  call er 17, !fid(2)  !fid(3)  !sigl  condition('D')  sourceline(!sigl)
syntax:  !sigl=sigl;  call er 13, !fid(2)  !fid(3)  !sigl  !cal()  condition('D')  sourceline(!sigl)
```

Coding note:   the   '''!'''   subroutines  (above) deal mostly with determining what version of REXX is being invoked and what operating system is being used;   and based on that information, appropriate flags (variables) are set.   This is an example of a robust boilerplate code checking for various versions of REXX and operating systems, and it also defines additional flags not used within this particular program.

Programming note:   The subroutine   '''$ERR'''   isn't included here; so here is the gist of the error messages:
::*   er 59       too many arguments specified for the ─── DELAY ─── command.
::*   er 53       argument ─── xxx ─── isn't numeric for the option ─── delay-seconds ─── for the ─── DELAY ─── command.


## Ring


```ring

load "guilib.ring"

for n = 1 to 10
    Sleep(3)
    see "" + n + " "
next
see nl

func Sleep x
     nTime = x * 1000
     oTest = new qTest
     oTest.qsleep(nTime)

```


Output:


```txt

1 2 3 4 5 6 7 8 9 10

```



## Ruby


```ruby
seconds = gets.to_f
puts "Sleeping..."
sleep(seconds) # number is in seconds ... but accepts fractions
# Minimum resolution is system dependent.
puts "Awake!"
```



## Rust


```rust
use std::{io, time, thread};

fn main() {
    println!("How long should we sleep in milliseconds?");

    let mut sleep_string = String::new();

    io::stdin().read_line(&mut sleep_string)
               .expect("Failed to read line");

    let sleep_timer: u64 = sleep_string.trim()
                                       .parse()
                                       .expect("Not an integer");
    let sleep_duration = time::Duration::from_millis(sleep_timer);

    println!("Sleeping...");
    thread::sleep(sleep_duration);
    println!("Awake!");
}
```



## Scala

```scala
object Sleeper extends App {
  print("Enter sleep time in milli sec: ")
  val ms = scala.io.StdIn.readInt()
  println("Sleeping...")
  val sleepStarted = scala.compat.Platform.currentTime
  Thread.sleep(ms)
  println(s"Awaked after [${scala.compat.Platform.currentTime - sleepStarted} ms]1")
}
```



## Scheme


Many Scheme implementations support srfi-18, a multithreading library which provides a 'thread-sleep!' function.
The following works in Chicken Scheme:


```scheme

(use format)
(use srfi-18)

(format #t "Enter a time (in seconds): ")
(let ((time (read))) ; converts input to a number, if possible
  (if (number? time)
    (begin
      (format #t "Sleeping...~&")
      (thread-sleep! time)
      (format #t "Awake!~&"))
    (format #t "You must enter a number~&")))

```


Scheme implementations also provide alternative approaches.  For example, Chicken Scheme has a 'posix' library which includes a 'sleep' function.


## Seed7


The [http://seed7.sourceforge.net/libraries/duration.htm duration.s7i] library defines the function ''wait'', which takes an argument of type ''duration''. Functions to create durations with years, months, days, hours, minutes, seconds and micro seconds exist also.


```seed7
$ include "seed7_05.s7i";
  include "duration.s7i";

const proc: main is func
  local
    var integer: secondsToSleep is 0;
  begin
    write("Enter number of seconds to sleep: ");
    readln(secondsToSleep);
    writeln("Sleeping...");
    wait(secondsToSleep . SECONDS);
    writeln("Awake!");
  end func;
```



## Sidef


```ruby
var sec = read(Number);       # any positive number (it may be fractional)
say "Sleeping...";
Sys.sleep(sec);               # in seconds
#Sys.usleep(sec);             # in microseconds
#Sys.nanosleep(sec);          # in nanoseconds
say "Awake!";
```



## Smalltalk

```smalltalk
t := (FillInTheBlankMorph request: 'Enter time in seconds') asNumber.
Transcript show: 'Sleeping...'.
(Delay forSeconds: t) wait.
Transcript show: 'Awake!'.

```

```smalltalk
t := (Dialog request: 'Enter time in seconds') asNumber.
Transcript show: 'Sleeping...'.
(Delay forSeconds: t) wait.
Transcript show: 'Awake!'.

```

(of course, you can "Smalltalk at:#FillInTheBlankMorph put:Dialog", to be source compatible with Pharo)


## Standard ML


```ocaml
(TextIO.print "input a number of seconds please: ";
let val seconds = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn))) in
  TextIO.print "Sleeping...\n";
  OS.Process.sleep (Time.fromReal seconds);  (* it takes a Time.time data structure as arg,
                                               but in my implementation it seems to round down to the nearest second.
                                               I dunno why; it doesn't say anything about this in the documentation *)
  TextIO.print "Awake!\n"
end)
```



## Stata


```stata
program sleep_awake
	* pass duration in milliseconds
	display "Sleeping..."
	sleep `0'
	display "Awake!"
end

sleep_awake 2000
```



## Suneido


```Suneido
function (time)
    {
    Print("Sleeping...")
    Sleep(time) // time is in milliseconds
    Print("Awake!")
    }
```



## Swift


```Swift
import Foundation

println("Enter number of seconds to sleep")
let input = NSFileHandle.fileHandleWithStandardInput()
var amount = NSString(data:input.availableData, encoding: NSUTF8StringEncoding)?.intValue
var interval = NSTimeInterval(amount!)
println("Sleeping...")
NSThread.sleepForTimeInterval(interval)

println("Awake!")
```



## Tcl

Blocking example (the process is blocked preventing any background activity).

```tcl
puts -nonewline "Enter a number of milliseconds to sleep: "
flush stdout
set millis [gets stdin]
puts Sleeping...
after $millis
puts Awake!
```


A non-blocking example where background activity will occur.

```tcl
puts -nonewline "Enter a number of milliseconds to sleep: "
flush stdout
set millis [gets stdin]
set ::wakupflag 0
puts Sleeping...
after $millis set ::wakeupflag 1
vwait ::wakeupflag
puts Awake!
```


=={{header|TI-89 BASIC}}==

<pre style="font-family:'TI Uni'">Local dur_secs,st0,st,seconds
Define seconds() = Func
  Local hms
  getTime()→hms
  Return ((hms[1] * 60 + hms[2]) * 60) + hms[3]
EndFunc
ClockOn

Prompt dur_secs
Disp "Sleeping..."
seconds()→st
st→st0
While when(st<st0, st+86400, st) - st0 < dur_secs
  seconds()→st
EndWhile
Disp "Awake!"
```



## Toka

This makes use of the sleep() function from libc which suspends execution for a specified number of seconds.

```txt
1 import sleep as sleep()
[ ." Sleeping...\n" sleep() drop ." Awake!\n" bye ] is sleep

45 sleep
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
secondsrange=2
PRINT "Sleeping ",secondsrange," seconds "
WAIT #secondsrange
PRINT "Awake after Naping ",secondsrange, " seconds"

```



## TXR



```txrlisp
(let ((usec (progn (put-string "enter sleep usecs: ")
                   (tointz (get-line)))))
  (put-string "Sleeping ... ")
  (flush-stream)
  (usleep usec)
  (put-line "Awake!"))
```



## UNIX Shell


```bash
printf "Enter a time in seconds to sleep: "
read seconds
echo "Sleeping..."
sleep "$seconds"
echo "Awake!"
```


This uses the [http://www.openbsd.org/cgi-bin/man.cgi?query=sleep&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html sleep(1)] command. POSIX sleep(1) only takes an integer, as in <tt>sleep 2</tt>, so you can only sleep for a whole number of seconds. Some systems extend sleep(1) to take a decimal fraction, as in <tt>sleep 2.5</tt>.


## Ursa


```ursa
out "Sleeping..." endl console
# sleep for 5 seconds (5000 milliseconds)
sleep 5000
out "Awake!" endl console
```



## VBA


```VBA

Function Sleep(iSecsWait As Integer)
Debug.Print Now(), "Sleeping..."
Application.Wait Now + iSecsWait / 86400 'Time is stored as fractions of 24 hour day
Debug.Print Now(), "Awake!"
End Function

```



## VBScript


```VBScript

iSeconds=InputBox("Enter a time in seconds to sleep: ","Sleep Example for RosettaCode.org")
WScript.Echo "Sleeping..."
WScript.Sleep iSeconds*1000		'Sleep is done in Milli-Seconds
WScript.Echo "Awake!"

```


<!--Out of alphabetical order to keep the VB languages together-->

## Visual Basic .NET



```vbnet
Module Program
    Sub Main()
        Dim millisecondsSleepTime = Integer.Parse(Console.ReadLine(), Globalization.CultureInfo.CurrentCulture)
        Console.WriteLine("Sleeping...")
        Threading.Thread.Sleep(millisecondsSleepTime)
        Console.WriteLine("Awake!")
    End Sub
End Module
```



## Vedit macro language


```vedit
#1 = Get_Num("Sleep time in 1/10 seconds: ")
Message("Sleeping...\n")
Sleep(#1)
Message("Awake!\n")
```



## X86 Assembly

This sleep subroutine takes the number of seconds to sleep as a parameter.
The actual linux syscall that I use to implement this takes a time struct (seconds at the first qword and nanoseconds at the second) as the first argument. Optionally you can pass another time stuct as a second argument. In the event that the sleep is interrupted by a system event, linux will fill this second struct with the remaining time so the syscall can be called again with the remaining sleep time.


```x86asm

; x86_64 linux nasm

section .text

Sleep:
  mov rsi, 0 ; we wont use the second sleep arg, pass null to syscall
  sub rsp, 16
  mov qword [rsp], rdi ; number of seconds the caller requested
  mov qword [rsp + 8], rsi ; we won't use the nanoseconds
  mov rdi, rsp ; pass the struct that's on the stack to
  mov rax, 35 ; sys_nanosleep
  syscall
  add rsp, 16 ; clean up stack
  ret

```



## zig


```zig
const std = @import("std");
const time = std.os.time;
const warn = std.debug.warn;

pub fn main() void {
    warn("Sleeping...\n");

    time.sleep(1000000000); // `sleep` uses nanoseconds

    warn("Awake!\n");
}
```



## zkl


```zkl
seconds:=ask("Seconds to sleep: ").toFloat();
println("Sleeping...");
Atomic.sleep(seconds); # float, usually millisecond resolution
println("Awake!");
```


