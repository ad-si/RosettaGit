+++
title = "Find limit of recursion"
description = ""
date = 2019-10-20T08:53:40Z
aliases = []
[extra]
id = 7084
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{selection|Short Circuit|Console Program Basics}}
[[Category:Basic language learning]]
[[Category:Programming environment operations]]
[[Category:Simple]]

;Task:
Find the limit of recursion.





## ACL2


```Lisp
(defun recursion-limit (x)
   (if (zp x)
       0
       (prog2$ (cw "~x0~%" x)
               (1+ (recursion-limit (1+ x))))))
```


{{out}} (trimmed):

```txt
87195
87196
87197
87198
87199
87200
87201

***********************************************
************ ABORTING from raw Lisp ***********
Error:  Stack overflow on value stack.
***********************************************
```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Recursion_Depth is
   function Recursion (Depth : Positive) return Positive is
   begin
      return Recursion (Depth + 1);
   exception
      when Storage_Error =>
         return Depth;
   end Recursion;
begin
   Put_Line ("Recursion depth on this system is" & Integer'Image (Recursion (1)));
end Test_Recursion_Depth;
```

Note that unlike some solutions in other languages this one does not crash (though usefulness of this task is doubtful).

In Ada Storage_Error exception is propagated when there is no free memory to accomplish the requested action.
In particular it is propagated upon stack overflow within the task where this occurs.
Storage_Error can be handled without termination of the task.
In the solution the function Recursion calls itself or else catches Storage_Error indicating stack overflow.

Note that this technique requires some care, because there must be enough stack space for the handler to work.
In this case it works because the handler just return the current call depth.
In real-life Storage_Error is usually fatal.

{{out}}

```txt

Recursion depth on this system is 524091

```



## ALGOL 68

The depth of recursion in Algol 68 proper is unlimited.  Particular implementations will reach a limit, if only through exhaustion of storage and/or address space and/or time before power failure.  If not time limited, the depth reached depends very much on what the recursive routine needs to store on the stack, including local variables if any.  The simplest recursive Algol68 program is:

```algol68>PROC recurse = VOID : recurse; recurse</lang

This one-liner running under Algol68 Genie and 64-bit Linux reaches a depth of 3535 with the shell's default stack size of 8Mbytes and 28672 when set to 64Mbytes,
as shown by the following output.  From this we can deduce that Genie does not implement tail recursion.  The --trace option to a68g prints a stack trace when the program crashes; the first two commands indicate the format of the trace, the third counts the depth of recursion with the default stack size and the fourth shows the result of octupling the size of the stack.
{{Out}}

```txt

pcl@anubis ~/a68/Rosetta $ a68g --trace Recurse.a68 | head
genie: frame stack 6144k, expression stack 2048k, heap 49152k, handles 8192k
      BEGIN MODE DOUBLE = LONG REAL, QUAD = LONG LONG REAL;
      -
1     PROC recurse = VOID : recurse; recurse
      -
genie_unit
1     PROC recurse = VOID : recurse; recurse
                                     -
genie_unit
1     PROC recurse = VOID : recurse; recurse
pcl@anubis ~/a68/Rosetta $ a68g --trace Recurse.a68 | tail
1     PROC recurse = VOID : recurse; recurse
                            -
genie_unit
1     PROC recurse = VOID : recurse; recurse
                            -
genie_unit
1     PROC recurse = VOID : recurse; recurse
                     1
a68g: runtime error: 1: stack overflow (detected in particular-program).
Genie finished in 0.19 seconds
pcl@anubis ~/a68/Rosetta $ a68g --trace Recurse.a68 |  grep recurse | wc
   3535   28280  159075
pcl@anubis ~/a68/Rosetta $ prlimit --stack=67108864 a68g --trace Recurse.a68 | grep recurse | wc
  28672  229376 1290240
pcl@anubis ~/a68/Rosetta $

```



## AppleScript

A basic test for Applescript, which has a notoriously shallow recursion stack.

```applescript
-- recursionDepth :: () -> IO String
on recursionDepth()
    script go
        on |λ|(i)
            try
                |λ|(1 + i)
            on error
                "Recursion limit encountered at " & i
            end try
        end |λ|
    end script

    go's |λ|(0)
end recursionDepth

on run

    recursionDepth()

end run
```

{{Out}}

```txt
"Recursion limit encountered at 502"
```


We get a fractionally higher (and arguably purer) result by deriving the highest Church Numeral (Church-encoded integer) that can be represented using AppleScript:

```applescript
-- HIGHEST CHURCH NUMERAL REPRESENTABLE IN APPLESCRIPT ?

-- (This should be a good proxy for recursion depth)

on run
    script unrepresentable
        on |λ|(x)
            try
                churchFromInt(x)
                return false
            on error
                return true
            end try
            x > 10
        end |λ|
    end script

    "The highest Church-encoded integer representable in Applescript is " & ¬
        (|until|(unrepresentable, my succ, 0) - 1)
end run

-- CHURCH NUMERALS ------------------------------------------------------

-- chZero :: (a -> a) -> a -> a
on chZero(f)
    script
        on |λ|(x)
            x
        end |λ|
    end script
end chZero

-- chSucc :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
on chSucc(n)
    script
        on |λ|(f)
            script
                property mf : mReturn(f)'s |λ|
                on |λ|(x)
                    mf(mReturn(n)'s |λ|(mf)'s |λ|(x))
                end |λ|
            end script
        end |λ|
    end script
end chSucc

-- churchFromInt :: Int -> (a -> a) -> a -> a
on churchFromInt(x)
    script go
        on |λ|(i)
            if 0 < i then
                chSucc(|λ|(i - 1))
            else
                chZero
            end if
        end |λ|
    end script
    go's |λ|(x)
end churchFromInt

-- intFromChurch :: ((Int -> Int) -> Int -> Int) -> Int
on intFromChurch(cn)
    mReturn(cn)'s |λ|(my succ)'s |λ|(0)
end intFromChurch


-- GENERIC FUNCTIONS ----------------------------------------

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set v to x
    set mp to mReturn(p)
    set mf to mReturn(f)
    repeat until mp's |λ|(v)
        set v to mf's |λ|(v)
    end repeat
end |until|

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- succ :: Enum a => a -> a
on succ(x)
    1 + x
end succ
```

{{Out}}

```txt
"The highest Church-encoded integer representable in Applescript is 571"
```



## AutoHotkey


```AutoHotkey
Recurse(0)

Recurse(x)
{
  TrayTip, Number, %x%
  Recurse(x+1)
}
```


Last visible number is 827.


## AutoIt


```AutoIt
;AutoIt Version: 3.2.10.0
$depth=0
recurse($depth)
Func recurse($depth)
   ConsoleWrite($depth&@CRLF)
   Return recurse($depth+1)
EndFunc
```

Last value of $depth is 5099 before error.
Error: Recursion level has been exceeded - AutoIt will quit to prevent stack overflow.


## AWK


```AWK
# syntax: GAWK -f FIND_LIMIT_OF_RECURSION.AWK
#
# version             depth  messages
# ------------------  -----  --------
# GAWK 3.1.4           2892  none
# XML GAWK 3.1.4       3026  none
# GAWK 4.0          >999999
# MAWK 1.3.3           4976  A stack overflow was encountered at
#                            address 0x7c91224e.
# TAWK-DOS AWK 5.0c     357  stack overflow
# TAWK-WIN AWKW 5.0c   2477  awk stack overflow
# NAWK 20100523        4351  Segmentation fault (core dumped)
#
BEGIN {
    x()
    print("done")
}
function x() {
    print(++n)
    if (n > 999999) { return }
    x()
}
```



## Axe

Warning: running this program will cause you to have to clear your RAM. You will lose any data stored in RAM.

In Axe 1.2.2 on a TI-84 Plus Silver Edition, the last line this prints before hanging is 12520. This should be independent of any arguments passed since they are not stored on the stack.


```axe
RECURSE(1)
Lbl RECURSE
.Optionally, limit the number of times the argument is printed
Disp r₁▶Dec,i
RECURSE(r₁+1)
```



## BASIC

=
## Applesoft BASIC
=
Each GOSUB consumes 6 bytes of stack space and when more than 25 levels have been reached and an <code>?OUT OF MEMORY ERROR</code> message is displayed.

```ApplesoftBasic
 100  PRINT "RECURSION DEPTH"
 110  PRINT D" ";
 120  LET D = D + 1
 130  GOSUB 110"RECURSION
```

{{out}}

```txt
RECURSION DEPTH
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
?OUT OF MEMORY ERROR IN 120
```


=
## BaCon
=
Utterly dependent on the stack size and RAM available to the process.


```freebasic
' Recursion limit
FUNCTION recurse(i)
    PRINT i
    extraneous = recurse(i+1)
    RETURN 0
END FUNCTION

extraneous = recurse(0)
```


{{out}}

```txt
prompt$ ./recursion-limit
0
1
2
...
261881
261882
261883
Segmentation fault
```


=
## Sinclair ZX81 BASIC
=
The only limit is the available memory.

```basic
10 LET D=0
20 GOSUB 30
30 PRINT AT 0,0;D
40 LET D=D+1
50 GOSUB 30
```

{{out}}
Run with 1k of RAM:

```txt
345

4/30
```

(The error code means "out of memory attempting to execute line <tt>30</tt>".)

=
## ZX Spectrum Basic
=
On the ZX Spectrum recursion is limited only by stack space. The program eventually fails, because the stack is so full that there is no stack space left to make the addition at line 110:

```zxbasic
10 LET d=0: REM depth
100 PRINT AT 1,1; "Recursion depth: ";d
110 LET d=d+1
120 GO SUB 100: REM recursion
130 RETURN: REM this is never reached
200 STOP
```

{{out}}(from a 48k Spectrum):

```txt
 Recursion depth: 13792
 4 Out of memory, 110:1
```



## Batch File

MUNG.CMD is a commandline tool written in DOS Batch language. It finds the limit of recursion possible using CMD /C.


```dos
@echo off
set /a c=c+1
echo [Depth %c%] Mung until no good
cmd /c mung.cmd
echo [Depth %c%] No good
set /a c=c-1
```


Result (abbreviated):


```txt

...
[Depth 259] Mung until no good
[Depth 260] Mung until no good
[Depth 261] Mung until no good
[Depth 261] No good
[Depth 260] No good
[Depth 259] No good
...

```


If one uses <code>call</code> rather than <code>CMD/C</code>, the call depth is much deeper but ends abruptly and can't be trapped.


```dos
@echo off
set /a c=c+1
echo [Depth %c%] Mung until no good
call mung.cmd
echo [Depth %c%] No good
set /a c=c-1
```


Result (abbreviated):


```txt

1240: Mung until no good
1241: Mung until no good
******  B A T C H   R E C U R S I O N  exceeds STACK limits ******
Recursion Count=1240, Stack Usage=90 percent
******       B A T C H   PROCESSING IS   A B O R T E D      ******

```


You also get the exact same results when calling mung internally, as below


```dos
@echo off
set c=0
:mung
set /a c=c+1
echo [Level %c%] Mung until no good
call :mung
set /a c=c-1
echo [Level %c%] No good
```


Setting a limit on the recursion depth can be done like this:


```dos
@echo off
set c=0
:mung
set /a c=%1+1
if %c%==10 goto :eof
echo [Level %c%] Mung until no good
call :mung %c%
set /a c=%1-1
echo [Level %c%] No good
```



## BBC BASIC


```bbcbasic
      PROCrecurse(1)
      END

      DEF PROCrecurse(depth%)
      IF depth% MOD 100 = 0 PRINT TAB(0,0) depth%;
      PROCrecurse(depth% + 1)
      ENDPROC
```

{{out}} from BBC BASIC for Windows with default value of HIMEM:

```txt

     37400
No room

```



## Befunge


In Befunge, the limit of recursion is essentially the depth of the stack. The program below calculates that limit by repeatedly pushing values until the stack overflows. After every iteration, it writes out the count of values pushed so far, so once the stack eventually does overflow, the last value output should tell you the depth that was reached.

Most interpreters allocate their stack on the global heap, so the size of the stack will depend on available memory, and on a modern system you're likely to run out of patience long before you run out of memory. That said, there have been some interpreters with a fixed stack depth - as low as 199 even - but that isn't a common implementation choice.


```befunge>1
1#:+#.:_@
```



## Bracmat


```bracmat
rec=.out$!arg&rec$(!arg+1)
```


Observed recursion depths:
  Windows XP command prompt: 6588
  Linux: 18276

Bracmat crashes when it tries to exceed the maximum recursion depth.


## C


```c
#include <stdio.h>

void recurse(unsigned int i)
{
  printf("%d\n", i);
  recurse(i+1); // 523756
}

int main()
{
  recurse(0);
  return 0;
}
```


Segmentation fault occurs when i is 523756.
(This was checked debugging with gdb rather than waiting the output:
the printf line for the test was commented).
It must be noted that the recursion limit depends on how many parameters are passed onto the stack.
E.g. adding a fake double argument to <code>recurse</code>, the limit is reached at <code>i == 261803</code>.
The limit depends on the stack size and usage in the function.
Even if there are no arguments, the return address for a call to a subroutine is stored on the stack (at least on x86 and many more processors), so this is consumed even if we put arguments into registers.

The following code may have some effect unexpected by the unwary:

```c
#include <stdio.h>

char * base;
void get_diff()
{
	char x;
	if (base - &x < 200)
		printf("%p %d\n", &x, base - &x);
}

void recur()
{
	get_diff();
	recur();
}

int main()
{
	char v = 32;
	printf("pos of v: %p\n", base = &v);
	recur();
	return 0;
}
```

With GCC 4.5, if compiled without -O2, it segfaults quickly; if <code>gcc -O2</code>, crash never happens, because the optimizer noticed the tail recursion in recur() and turned it into a loop!


## C++


```cpp

#include <iostream>

void recurse(unsigned int i)
{
  std::cout<<i<<"\n";
  recurse(i+1);
}

int main()
{
  recurse(0);
}

```



## C#

```c#
using System;
class RecursionLimit
{
  static void Main(string[] args)
  {
    Recur(0);
  }

  private static void Recur(int i)
  {
    Console.WriteLine(i);
    Recur(i + 1);
  }
}
```


Through debugging, the highest I achieve is 14250.

Through execution (with Mono), another user has reached 697186.


## Clojure


```clojure

=> (def *stack* 0)
=> ((fn overflow [] ((def *stack* (inc *stack*))(overflow))))
java.lang.StackOverflowError (NO_SOURCE_FILE:0)
=> *stack*
10498

```



## COBOL

{{works with|OpenCOBOL}}

```cobol
identification division.
program-id. recurse.
data division.
working-storage section.
01 depth-counter	pic 9(3).
01  install-address   	usage is procedure-pointer.
01  install-flag      	pic x comp-x value 0.
01  status-code       	pic x(2) comp-5.
01  ind               	pic s9(9) comp-5.


linkage section.
01  err-msg           	pic x(325).

procedure division.
100-main.

	set install-address to entry "300-err".

	call "CBL_ERROR_PROC" using install-flag
		install-address
		returning status-code.

	if status-code not = 0
		display "ERROR INSTALLING ERROR PROC"
		stop run
        end-if

 	move 0 to depth-counter.
	display 'Mung until no good.'.
	perform 200-mung.
	display 'No good.'.
	stop run.

200-mung.
	add 1 to depth-counter.
	display depth-counter.
	perform 200-mung.
300-err.
	entry "300-err" using err-msg.
	perform varying ind from 1 by 1
		until (err-msg(ind:1) = x"00") or (ind = length of err-msg)
			continue
	end-perform

	display err-msg(1:ind).

*> room for a better-than-abrupt death here.

	exit program.
```

Compiled with
```txt
cobc -free -x -g recurse.cbl
```
 gives, after a while,

```txt
...
249
250
251
252
253
Trapped: recurse.cob:38: Stack overflow, possible PERFORM depth exceeded
recurse.cob:50: libcob: Stack overflow, possible PERFORM depth exceeded
```


Without stack-checking turned on (achieved with -g in this case), it gives

```txt
...
249
250
251
252
253
254
255
256
257
Attempt to reference unallocated memory (Signal SIGSEGV)
Abnormal termination - File contents may be incorrect
```

which suggests that -g influences the functionality of CBL_ERROR_PROC

Thanks to Brian Tiffin for his [http://www.opencobol.org/modules/newbb/viewtopic.php?viewmode=thread&topic_id=254&forum=1&post_id=1312#1312 demo code on opencobol.org's forum]

===A more 'canonical' way of doing it===
from Richard Plinston on [http://groups.google.com/group/comp.lang.cobol/browse_thread/thread/bc8574c6452a7da7# comp.lang.cobol]

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID.          recurse RECURSIVE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Starter          PIC S9(8) VALUE 1.
       PROCEDURE DIVISION.
       Program-Recurse.
           CALL "recurse-sub" USING Starter
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.          recurse-sub.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  Countr                      PIC S9(8).
       PROCEDURE DIVISION USING Countr.
       Program-Recursive.
           DISPLAY Countr
           ADD 1   TO Countr
           CALL "recurse-sub" USING Countr

           EXIT PROGRAM.
       END PROGRAM recurse-sub.
       END PROGRAM recurse.
```


Compiled with
```txt
cobc -x -g recurse.cbl
```
 gives

```txt
...
+00000959
+00000960
+00000961
+00000962
+00000963
+00000964
recurse.cbl:19: Attempt to reference unallocated memory (Signal SIGSEGV)
Abnormal termination - File contents may be incorrect
```



## CoffeeScript


```coffeescript

recurse = ( depth = 0 ) ->
    try
        recurse depth + 1
    catch exception
        depth

console.log "Recursion depth on this system is #{ do recurse }"

```


{{out}} Example on [http://nodejs.org Node.js]:

```txt

    Recursion depth on this system is 9668

```



## Common Lisp



```lisp

(defun recurse () (recurse))
(trace recurse)
(recurse)

```


{{out}} This test was done with clisp under cygwin:

```txt

 3056. Trace: (RECURSE)
 3057. Trace: (RECURSE)
 3058. Trace: (RECURSE)
 3059. Trace: (RECURSE)

 *** - Lisp stack overflow. RESET

```


However, for an implementation of Lisp that supports proper tail recursion,
this function will not cause a stack overflow, so this method will not work.


## D


```d
import std.c.stdio;

void recurse(in uint i=0) {
    printf("%u ", i);
    recurse(i + 1);
}

void main() {
    recurse();
}
```

With the DMD compiler, using default compilation arguments, the stack overflows at 51_002.

With DMD increasing the stack size using for example -L/STACK:1500000000 the stack overflows at 75_002_026.

Using -O compilation argument DMD performs tail call optimization, and the stack doesn't overflow.

=={{header|Déjà Vu}}==
{{untested|Déjà Vu}}

```dejavu
rec-fun n:
	!. n
	rec-fun ++ n

rec-fun 0
```

This continues until the memory is full, so I didn't wait for it to finish.
Currently, it should to to almost 3 million levels of recursion on a machine with 1 GB free.
Eliminating the <code>n</code> should give over 10 million levels on the same machine.


## Delphi

{{works with|Delphi|2010 (and probably all other versions)}}

```delphi
program Project2;
{$APPTYPE CONSOLE}
uses
  SysUtils;

function Recursive(Level : Integer) : Integer;
begin
  try
    Level := Level + 1;
    Result := Recursive(Level);
  except
    on E: EStackOverflow do
      Result := Level;
  end;
end;

begin
  Writeln('Recursion Level is ', Recursive(0));
  Writeln('Press any key to Exit');
  Readln;
end.
```


{{out}}

```txt
Recursion Level is 28781
```



## DWScript


Recursion limit is a parameter of script execution, which can be specified independently from the stack size to limit execution complexity.


```delphi
var level : Integer;

procedure Recursive;
begin
   Inc(level);
   try
      Recursive;
   except
   end;
end;

Recursive;

Println('Recursion Level is ' + IntToStr(level));
```



## E


Outside of debugging access to other vats, E programs are (ideally) not allowed to observe recursion limits, because stack unwinding at an arbitrary point can break invariants of the code that was executing at the time. In particular, consider an attacker who estimates the stack size, nearly fills up the stack to that point, then invokes the victim — If the attacker is allowed to catch our hypothetical StackOverflowException from inside the victim, then there is a good chance of the victim then being in an inconsistent state, which the attacker can then make use of.


## EasyLang


<lang>func recurse i . .
  print i
  call recurse i + 1
.
call recurse 0
```


```txt

0
.
.
999
 ---------------------------------
| max call depth of 1000 exceeded |
 ---------------------------------

```



## Elixir

same as "Erlang"


## Emacs Lisp


```lisp
(defun my-recurse (n)
  (my-recurse (1+ n)))
(my-recurse 1)
=>
enters debugger at (my-recurse 595),
per the default max-lisp-eval-depth 600 in Emacs 24.1
```


Variable <code>max-lisp-eval-depth</code>[http://www.gnu.org/software/emacs/manual/html_node/elisp/Eval.html#index-max_002dlisp_002deval_002ddepth-539] is the maximum depth of function calls and variable <code>max-specpdl-size</code>[http://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html#index-max_002dspecpdl_002dsize-614] is the maximum depth of nested <code>let</code> bindings.  A function call is a <code>let</code> of the parameters, even if there's no parameters, and so counts towards <code>max-specpdl-size</code> as well as <code>max-lisp-eval-depth</code>.

The limits can be increased with <code>setq</code> etc globally, or <code>let</code> etc temporarily.  Lisp code which knows it needs deep recursion might temporarily increase the limits.  Eg. <code>regexp-opt.el</code>.  The ultimate limit is memory or C stack.


## Erlang

Erlang has no recursion limit. It is tail call optimised. If the recursive call is not a tail call it is limited by available RAM. Please add what to save on the stack and how much RAM to give to Erlang and I will test that limit.

=={{header|F_Sharp|F#}}==
A tail-recursive function will run indefinitely without problems (the integer will overflow, though).


```fsharp
let rec recurse n =
  recurse (n+1)

recurse 0
```


The non-tail recursive function of the following example crashed with a <code>StackOverflowException</code> after 39958 recursive calls:


```fsharp
let rec recurse n =
   printfn "%d" n
   1 + recurse (n+1)

recurse 0 |> ignore
```



## Factor

Factor is tail-call optimized, so the following example will run without issue. In fact, Factor's iterative combinators such as <code>map</code>, <code>each</code>, and <code>times</code> are written in terms of tail recursion.

```factor
: recurse ( n -- n ) 1 + recurse ;

0 recurse
```

The following non-tail recursive word caused a call stack overflow error after 65518 recursive calls in the listener.

```factor
SYMBOL: depth

: fn ( n -- n ) depth inc 1 + fn 1 + ;

[ 0 fn ] try
depth get "Recursion depth on this system is %d.\n" printf
```

{{out}}

```txt

Call stack overflow

Type :help for debugging help.
Recursion depth on this system is 65518.

```



## Forth


```forth
: munge ( n -- n' ) 1+ recurse ;

: test   0 ['] munge catch if ." Recursion limit at depth " . then ;

test   \ Default gforth: Recursion limit at depth 3817
```


Or you can just ask the system:


```forth
s" return-stack-cells" environment? ( 0 | potential-depth-of-return-stack -1 )
```


Full TCO is problematic, but a properly tail-recursive call is easy to add to any Forth.  For example, in SwiftForth:


```forth
: recur; [ last 2 cells + literal ] @ +bal postpone again ; immediate

: test dup if 1+ recur; then drop ." I gave up finding a limit!" ;

1 test
```



## Fortran


```fortran
program recursion_depth

  implicit none

  call recurse (1)

contains

  recursive subroutine recurse (i)

    implicit none
    integer, intent (in) :: i

    write (*, '(i0)') i
    call recurse (i + 1)

  end subroutine recurse

end program recursion_depth
```

{{out}} (snipped):

```txt
208914
208915
208916
208917
208918
208919
208920
208921
208922
208923
Segmentation fault (core dumped)
```



## GAP

The limit is around 5000 :

```gap
f := function(n)
  return f(n+1);
end;

# Now loop until an error occurs
f(0);

# Error message :
#   Entering break read-eval-print loop ...
#   you can 'quit;' to quit to outer loop, or
#   you may 'return;' to continue

n;
# 4998

# quit "brk mode" and return to GAP
quit;
```

This is the default GAP recursion trap, see [http://www.gap-system.org/Manuals/doc/htm/ref/CHAP007.htm#SECT010 reference manual, section 7.10]. It enters "brk mode" after multiples of 5000 recursions levels. On can change this interval :

```gap
SetRecursionTrapInterval(100000);
# No limit (may crash GAP if recursion is not controlled) :
SetRecursionTrapInterval(0);
```



## gnuplot


```gnuplot
# Put this in a file foo.gnuplot and run as
#     gnuplot foo.gnuplot

# probe by 1 up to 1000, then by 1% increases
if (! exists("try")) { try=0 }
try=(try<1000 ? try+1 : try*1.01)

recurse(n) = (n > 0 ? recurse(n-1) : 'ok')
print "try recurse ", try
print recurse(try)
reread
```


Gnuplot 4.6 has a builtin <code>STACK_DEPTH</code> limit of 250, giving


```txt
try recurse 251
"/tmp/foo.gnuplot", line 2760: recursion depth limit exceeded
```


Gnuplot 4.4 and earlier has no limit except the C stack, giving a segv or whatever eventually.


## Go

Go features stacks that grow as needed making the effective recursion limits relatively large.

Pre-Go 1.2 this could be all of memory and the program would grow without bounds until the system swap space was exhausted and the program was killed (either by the a [http://golang.org/ref/spec#Run_time_panics run-time panic] after an allocation failure or by the operating system killing the process).

Go 1.2 set a limit to the maximum amount of memory that can be used by a ''single goroutine'' stack.
The initial setting is 1 GB on 64-bit systems, 250 MB on 32-bit systems.
The default can be changed by [https://golang.org/pkg/runtime/debug#SetMaxStack <code>SetMaxStack</code>] in the <code>runtime/debug</code> package. It is documented as "useful mainly for limiting the damage done by goroutines that enter an infinite recursion."


```go
package main

import (
	"flag"
	"fmt"
	"runtime/debug"
)

func main() {
	stack := flag.Int("stack", 0, "maximum per goroutine stack size or 0 for the default")
	flag.Parse()
	if *stack > 0 {
		debug.SetMaxStack(*stack)
	}
	r(1)
}

func r(l int) {
	if l%1000 == 0 {
		fmt.Println(l)
	}
	r(l + 1)
}
```


Run without arguments on a 64-bit system:
{{out}}

```txt

[…]
4471000
4472000
4473000
runtime: goroutine stack exceeds 1000000000-byte limit
fatal error: stack overflow

runtime stack:
runtime.throw(0x5413ae)
	/usr/local/go/src/pkg/runtime/panic.c:520 +0x69
runtime.newstack()
	/usr/local/go/src/pkg/runtime/stack.c:770 +0x486
runtime.morestack()
	/usr/local/go/src/pkg/runtime/asm_amd64.s:228 +0x61

goroutine 16 [stack growth]:
main.r(0x444442)
	[…]/rosetta/stack_size/stack.go:9 fp=0xc2680380c8 sp=0xc2680380c0
main.r(0x444441)
	[…]/rosetta/stack_size/stack.go:13 +0xc5 fp=0xc268038140 sp=0xc2680380c8
main.r(0x444440)
[…]
...additional frames elided...
created by _rt0_go
	/usr/local/go/src/pkg/runtime/asm_amd64.s:97 +0x120

goroutine 19 [finalizer wait]:
runtime.park(0x412a20, 0x542ce8, 0x5420a9)
	/usr/local/go/src/pkg/runtime/proc.c:1369 +0x89
runtime.parkunlock(0x542ce8, 0x5420a9)
	/usr/local/go/src/pkg/runtime/proc.c:1385 +0x3b
runfinq()
	/usr/local/go/src/pkg/runtime/mgc0.c:2644 +0xcf
runtime.goexit()
	/usr/local/go/src/pkg/runtime/proc.c:1445
exit status 2

```


Run with "-stack 262144000" (to simulate the documented 250 MB limit on a 32-bit system):
{{out}}

```txt

[…]
1117000
1118000
runtime: goroutine stack exceeds 262144000-byte limit
fatal error: stack overflow
[…]

```


On a 32-bit system an <code>int</code> is 32 bits so the maximum value to <code>debug.SetMaxStack</code> is 2147483647 (nearly 2 GB).
On a 64-bit system an <code>int</code> is usually 64 bits so the maximum value will much larger, more than the available memory. Thus setting the maximum value will either exhaust all the system memory and swap (as with pre-Go 1.2) or will result in a allocation failure and run-time panic (e.g. 32-bit systems often have more memory and swap in total than the memory accessible to a single user program due to the limits of a 32 bit address space shared with the kernel).

Note, unlike with some other systems, increasing or changing this value only changes the allocation ''limit''.
The stack still starts out very small and only grows as needed, there is no large stack pre-allocation even when using a very high limit.
Also note that this is ''per-goroutine'', each goroutine can recurse independently and approach the limit.

The above code built pre-Go 1.2 (without the then non-existent <code>debug.SetMaxStack</code> call) and
run on a 1 GB RAM machine with 2.5 GB swap filled available RAM quickly, at a recursion depth of about 10M.
It took a several minutes to exhaust swap before exiting with this trace:  (as you see, at a depth of over 25M.)

```txt

[…]
25611000
25612000
25613000
25614000
throw: out of memory (FixAlloc)

runtime.throw+0x43 /home/sonia/go/src/pkg/runtime/runtime.c:102
	runtime.throw(0x80e80c8, 0x1)
runtime.FixAlloc_Alloc+0x76 /home/sonia/go/src/pkg/runtime/mfixalloc.c:43
	runtime.FixAlloc_Alloc(0x80eb558, 0x2f)
runtime.stackalloc+0xfb /home/sonia/go/src/pkg/runtime/malloc.c:326
	runtime.stackalloc(0x1000, 0x8048c44)
runtime.newstack+0x140 /home/sonia/go/src/pkg/runtime/proc.c:768
	runtime.newstack()
runtime.morestack+0x4f /home/sonia/go/src/pkg/runtime/386/asm.s:220
	runtime.morestack()
----- morestack called from goroutine 1 -----
main.r+0x1a /home/sonia/t.go:9
	main.r(0x186d801, 0x0)
main.r+0x95 /home/sonia/t.go:13
	main.r(0x186d800, 0x0)
main.r+0x95 /home/sonia/t.go:13
	main.r(0x186d7ff, 0x0)
main.r+0x95 /home/sonia/t.go:13
	main.r(0x186d7fe, 0x0)
main.r+0x95 /home/sonia/t.go:13
	main.r(0x186d7fd, 0x0)

... (more of the same stack trace omitted)


----- goroutine created by -----
_rt0_386+0xc1 /home/sonia/go/src/pkg/runtime/386/asm.s:80

goroutine 1 [2]:
runtime.entersyscall+0x6f /home/sonia/go/src/pkg/runtime/proc.c:639
	runtime.entersyscall()
syscall.Syscall+0x53 /home/sonia/go/src/pkg/syscall/asm_linux_386.s:33
	syscall.Syscall()
syscall.Write+0x5c /home/sonia/go/src/pkg/syscall/zsyscall_linux_386.go:734
	syscall.Write(0x1, 0x977e4f18, 0x9, 0x40, 0x9, ...)
os.*File·write+0x39 /home/sonia/go/src/pkg/os/file_unix.go:115
	os.*File·write(0x0, 0x0, 0x9, 0x40, 0x9, ...)
os.*File·Write+0x98 /home/sonia/go/src/pkg/os/file.go:141
	os.*File·Write(0xbffe1980, 0x8, 0x9, 0x8048cbf, 0x186d6b4, ...)
----- goroutine created by -----
_rt0_386+0xc1 /home/sonia/go/src/pkg/runtime/386/asm.s:80

```



## Gri

In Gri 2.12.23 the total depth of command calls is limited to an internal array size <code>cmd_being_done_LEN</code> which is 100.  There's no protection or error check against exceeding this, so the following code segfaults shortly after 100,


```Gri
`Recurse'
{
    show .depth.
    .depth. = {rpn .depth. 1 +}
    Recurse
}
.depth. = 1
Recurse
```



## Groovy

{{Trans|Java}}
Solution:

```groovy
def recurse;
recurse = {
    try {
        recurse (it + 1)
    } catch (StackOverflowError e) {
        return it
    }
}

recurse(0)
```


{{out}}

```txt
387
```


## Haskell


```haskell
import Debug.Trace (trace)

recurse :: Int -> Int
recurse n = trace (show n) recurse (succ n)

main :: IO ()
main = print $ recurse 1
```


Or point-free:

```haskell
import Debug.Trace (trace)
import Data.Function (fix)

recurse :: Int -> Int
recurse = fix ((<*> succ) . flip (trace . show))

main :: IO ()
main = print $ recurse 1
```



Or, more practically, testing up to a given depth:


```haskell
import Debug.Trace (trace)

testToDepth :: Int -> Int -> Int
testToDepth max n
  | n >= max = max
  | otherwise = trace (show n) testToDepth max (succ n)

main :: IO ()
main = print $ testToDepth 1000000 1
```

{{Out}}

```txt
...
999987
999988
999989
999990
999991
999992
999993
999994
999995
999996
999997
999998
999999
1000000
```



## hexiscript


```hexiscript
fun rec n
  println n
  rec (n + 1)
endfun

rec 1
```



## HolyC

The point at which a stack overflow occurs varies depending upon how many parameters passed onto the stack. Running the code from within the editor on a fresh boot of TempleOS will cause a stack overflow when <code>i</code> is larger than ~8100.


```holyc
U0 Recurse(U64 i) {
  Print("%d\n", i);
  Recurse(i + 1);
}

Recurse(0);
```



## i


```i
function test(counter) {
	print(counter)
	test(counter+1)
}

software {
	test(0)
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
envar := "MSTKSIZE"
write(&errout,"Program to test recursion depth - dependant on the environment variable ",envar," = ",\getenv(envar)|&null)
deepdive()
end

procedure deepdive()
static d
initial d := 0
write( d +:= 1)
deepdive()
end
```

Note: The stack size environment variable defaults to about 50000 words.  This terminates after approximately 3500 recursions (Windows).  The interpreter should terminate with a 301 error, but currently this does not work.


## Inform 7


```inform7
Home is a room.

When play begins: recurse 0.

To recurse (N - number):
	say "[N].";
	recurse N + 1.
```


Using the interpreters built into Windows build 6F95, a stack overflow occurs after 6529 recursions on the Z-machine or 2030 recursions on Glulx.


## J

Testing stack depth can be risky because the OS may shut down J in the limiting case. To portably test stack depth it's best to run jconsole and display a count as each stack frame is entered.

Note also that task assumes that all stack frames must be the same size, which is probably not the case.


```J
(recur=: verb def 'recur smoutput N=:N+1')N=:0
```


This above gives a stack depth of 9998 on one machine.

Note also, that ^: can be used for induction, and does not have stack size limits, though it does require that the function involved is a mathematical function of known variables -- and this is not always the case (for example, Markov processes typically use non-functions or "functions of unknown variables").


## Java


```Java

public class RecursionTest {

    private static void recurse(int i) {
        try {
	    recurse(i+1);
	} catch (StackOverflowError e) {
	    System.out.print("Recursion depth on this system is " + i + ".");
	}
    }

    public static void main(String[] args) {
        recurse(0);
    }
}

```


{{out}}

```txt
Recursion depth on this system is 10473.
```


Settings:

```txt
Default size of stack is 320 kB.. To extend the memory allocated for stack can be used switch -Xss with the memmory limits.
For example: java -cp . -Xss1m RecursionTest (set the stack size to 1 MB).
```



## JavaScript


```javascript

function recurse(depth)
{
 try
 {
  return recurse(depth + 1);
 }
 catch(ex)
 {
  return depth;
 }
}

var maxRecursion = recurse(1);
document.write("Recursion depth on this system is " + maxRecursion);
```


{{out}} (Chrome):

```txt
Recursion depth on this system is 10473.
```


{{out}} (Firefox 1.6.13):

```txt
Recursion depth on this system is 3000.
```


{{out}} (IE6):

```txt
Recursion depth on this system is 2552.
```



## jq

Recent versions of jq (after July 1, 2014, i.e. after version 1.4) include some "Tail Call Optimizations" (TCO).  As a result, tail-recursive functions of arity 0 will run indefinitely in these later versions.  The TCO optimizations also speed up other recursive functions.

Accordingly we present two test functions and show the results using jq 1.4 and using a version of jq with TCO optimizations.

'''Arity-0 Function'''

```jq
def zero_arity:
  if (. % 1000000 == 0) then . else empty end, ((.+1)| zero_arity);

1|zero_arity
```

'''Arity-1 Function'''

```jq
def with_arity(n):
  if (n % 1000 == 0) then n else empty end, with_arity(n+1);

with_arity(1)
```


'''Results using jq 1.4'''

```sh

# Arity 0 - without TCO:
...
23000000      # 1.62 GB
25000000
*** error: can't allocate region
user	0m54.558s
sys	0m2.773s

# Arity 1 - without TCO:
...
77000 # 23.4 MB
...
85000  # 23.7 MB
90000  # 25.4 MB
237000 # 47.4 MB (5h:08)
242000 # 50.0 MB (5h:14m)
# [job cancelled manually after over 5 hours]

```

'''Results using jq with TCO'''

The arity-0 test was stopped after the recursive function had been called 100,000,000 (10^8) times. The memory required did not grow beyond 360 KB (sic).

```sh

$ time jq -n -f Find_limit_of_recursions.jq
...
10000000 # 360 KB
...
100000000  # 360 KB
# [job cancelled to get a timing]
user	2m0.534s
sys	0m0.329s

```


The arity-1 test process was terminated simply because it had become too slow; at that point it had only consumed about 74.6K MB.

```sh

...
56000   #  9.9MB
...
95000   # 14.8 MB
98000   # 15.2 MB
99000   # 15.4 MB
100000  # 15.5 MB
127000  # 37.4 MB
142000  # 37.4 MB
254000  # 74.6 MB
287000  # 74.6 MB
406000  # 74.6 MB (8h:50m)
412000  # 74.6 MB (9h:05m)
# [job cancelled manually after over 9 hours]
```

'''Discussion'''

Even without the TCO optimizations, the effective limits for recursive jq functions are relatively high:
#  the arity-0 function presented here proceeded normally beyond 25,000,000 (25 million) iterations;
#  the arity-1 function is more effectively constrained by performance than by memory: the test process was manually terminated after 242,000 iterations.

With the TCO optimizations:
#  the arity-0 function is not only unconstrained by memory but is fast and remains fast; it requires only 360 KB (that is KB).
#  the arity-1 function is, once again, more effectively constrained by performance than by memory: the test process was terminated after 412,000 iterations simply because it had become too slow; at that point it had only consumed about 74.6 MB.


## Julia

This solution includes two versions of the function for probing recursion depth.  The '''Clean''' version is perhaps more idiomatic.  However the '''Dirty''' version, by using a global variable for the depth counter and minimizing the complexity of the called code reaches a significantly greater depth of recursion.

'''Clean'''

```Julia

function divedivedive(d::Int)
    try
        divedivedive(d+1)
    catch
        return d
    end
end

```

'''Dirty'''

```Julia

function divedivedive()
    global depth
    depth += 1
    divedivedive()
end

```

'''Main'''

```Julia

depth = divedivedive(0)
println("A clean dive reaches a depth of ", depth, ".")

depth = 0
try
    divedivedive()
end
println("A dirty dive reaches a depth of ", depth, ".")

```


{{out}}

```txt

A clean dive reaches a depth of 21807.
A dirty dive reaches a depth of 174454.

```



## Kotlin

The result is a typical figure for Oracle's JVM 1.8.0_121 running on Ubuntu version 14.04, 64 bit using the default stack size.

One might have expected that the result would be the same (or only vary over a small range) for a given configuration but in fact the results are all over the place - running the program a number of times I obtained figures as high as 26400 and as low as 9099! I have no idea why.

```scala
// version 1.1.2

fun recurse(i: Int) {
    try {
        recurse(i + 1)
    }
    catch(e: StackOverflowError) {
        println("Limit of recursion is $i")
    }
}

fun main(args: Array<String>) = recurse(0)
```


{{out}}

```txt

10367

```



## Liberty BASIC

Checks for the case of gosub & for proper subroutine.

```lb

'subroutine recursion limit- end up on 475000

call test 1

sub test n
    if n mod 1000 = 0 then locate 1,1: print n
    call test n+1
end sub

```



```lb

'gosub recursion limit- end up on 5767000
[test]
    n = n+1
    if n mod 1000 = 0 then locate 1,1: print n
gosub [test]

```



## LIL

lil.c allows an optional build time value to set a limit on recursion:

```c
/* Enable limiting recursive calls to lil_parse - this can be used to avoid call stack
 * overflows and is also useful when running through an automated fuzzer like AFL */
/*#define LIL_ENABLE_RECLIMIT 10000*/
```


Otherwise, it is a race to run out of stack:

{{out}}

```txt
Little Interpreted Language Interactive Shell
# func recur {n} {print $n; inc n; recur $n}
recur
# recur
1
2
...
37323
37324
Segmentation fault (core dumped)
```


That number will varying depending on system and state of system.


## Logo

Like Scheme, Logo guarantees tail call elimination, so recursion is effectively unbounded. You can catch a user interrupt though to see how deep you could go.


```logo
make "depth 0

to recurse
  make "depth :depth + 1
  recurse
end

catch "ERROR [recurse]
  ; hit control-C after waiting a while
print error          ; 16 Stopping... recurse [make "depth :depth + 1]
(print [Depth reached:] :depth)   ; some arbitrarily large number
```



## LSL

I ran this twice and got 1891 and 1890; probably varies with the number Avatars on a Sim and other variables I can't control.

Originally I had it without the OwnerSay in the recursive function. Generally, if LSL has a Runtime Error it just shouts on the DEBUG_CHANNEL and skips to the next statement (which would have returned to the next statement in state_entry() said the highest number it had achieved) but, it just shouted "Script run-time error. Object: Stack-Heap Collision" on debug and quit running.

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
integer iLimit_of_Recursion = 0;
Find_Limit_of_Recursion(integer x) {
	llOwnerSay("x="+(string)x);
	iLimit_of_Recursion = x;
	Find_Limit_of_Recursion(x+1);
}
default {
	state_entry() {
		Find_Limit_of_Recursion(0);
		llOwnerSay("iLimit_of_Recursion="+(string)iLimit_of_Recursion);
	}
}

```

{{out}}

```txt

[2012/07/07 18:40]  Object: x=0
[2012/07/07 18:40]  Object: x=1
[2012/07/07 18:40]  Object: x=2
   ...   ...   ...   ...   ...
[2012/07/07 18:41]  Object: x=1888
[2012/07/07 18:41]  Object: x=1889
[2012/07/07 18:41]  Object: x=1890
[2012/07/07 18:41]  Object: Object [script:New Script] Script run-time error
[2012/07/07 18:41]  Object: Stack-Heap Collision

```



## Lua

Lua (version 5.3) support proper tail call, if the recursion is proper tail call there is no limit.
Otherwise, it is limited by stack size set by the implementation.

```Lua

local c = 0
function Tail(proper)
  c = c + 1
  if proper then
    if c < 9999999 then return Tail(proper) else return c end
  else
    return 1/c+Tail(proper) -- make the recursive call must keep previous stack
  end
end

local ok,check = pcall(Tail,true)
print(c, ok, check)
c=0
ok,check = pcall(Tail,false)
print(c, ok, check)

```

{{out}}

```txt

9999999	true	9999999
333325	false	D:\00EXE\share\lua\5.3\test.lua:57: stack overflow

```



## M2000 Interpreter

===Modules & Functions===

```M2000 Interpreter

Module checkit {
      Global z
      Function a {
            z++
            =a()
      }
      try {
            m=a()
      }
      Print z

      z<=0
      Function a {
            z++
            call a()
      }
      try {
            call a()
      }
      Print z

      z<=0
      Module m {
            z++
            Call m
      }
      try {
            call m
      }
      Print z

      z<=0
      \\ without Call a module can't call itself
      \\ but can call something global, and that global can call back
      Module Global m1 {
            z++
            m2
      }
      Module Global m2 {
            z++
            m1
      }
      try {
            call m1
      }
      Print z
}
Checkit

```

In Wine give these:

```txt

4030
8473  (plus 2 because we have Checkit inside a Z so these are two calls)
8473 (the same as above)
11225 (the same as above)

```



### Subroutines

A lot of languages have a subroutine as a function without return value. As we see before, M2000 has Modules (as procedures) and Functions as that can be called as procedures too. These "procedures" can use only globals and anything they make for them.
So what is a subroutine in Μ2000?

Subroutines are part of modules/functions. They haven't execution object, and they have to use parent object. So this parent object has the return stack, and use heap for this. So we can set a limit with Recursion.Limit to say 500000.

So a subroutine is code with module's scope, with recursion and local definitions. Utilizing current stack we can get results, or we can use by reference parameters to get results too.

We have to use statement Local for local variables and arrays who shadows same name variables or arrays. Parent can be the module/function as the caller, or another subroutine, or the same, but all have the same parent, the module/function.



```M2000 Interpreter

Module Checkit {
     \\ recursion for subs controled by a value
      \\ change limit get a list of numbers from 1 to limit
      Recursion.Limit 10
      function FindZ {
            z=1
            Try {
                  CallmeAgain(1)
            }
            =Abs(z)
            Sub CallmeAgain(x)
                  z--
                   CallmeAgain(x+1)
                  z++
            End Sub
      }
      z=FindZ()
      Print "Calls:"; z
      NormalCall(1)
      Sub NormalCall(x)
            Print x
            z--
            if z>0 then NormalCall(x+1)
            z++
      End Sub
}
Checkit

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The variable $RecursionLimit can be read for its current value or set to different values. eg
<lang>$RecursionLimit=10^6
```

Would set the recursion limit to one million.

=={{header|MATLAB}} / {{header|Octave}}==
The recursion limit can be 'get' and 'set' using the "get" and "set" keywords.

Sample Usage:

```MATLAB>>
 get(0,'RecursionLimit')

ans =

   500

>> set(0,'RecursionLimit',2500)
>> get(0,'RecursionLimit')

ans =

        2500
```



## Maxima


```maxima
f(p) := f(n: p + 1)$
f(0);
Maxima encountered a Lisp error:
 Error in PROGN [or a callee]: Bind stack overflow.
Automatically continuing.
To enable the Lisp debugger set *debugger-hook* to nil.

n;
406
```


=={{header|MK-61/52}}==
<lang>П2	ПП	05	ИП1	С/П
ИП0	ИП2	-	x<0	20	ИП0	1	+	П0	ПП	05
ИП1	1	+	П1	В/О
```


=={{header|Modula-2}}==

```modula2
MODULE recur;

IMPORT InOut;

PROCEDURE recursion (a : CARDINAL);

BEGIN
  InOut.Write ('.');    (*  just count the dots....     *)
  recursion (a + 1)
END recursion;

BEGIN
  recursion (0)
END recur.
```

Producing this:
<lang Modula-2>
jan@Beryllium:~/modula/rosetta$ recur >testfile
Segmentation fault
jan@Beryllium:~/modula/rosetta$ ls -l
-rwxr-xr-x 1 jan users  20032 2011-05-20 00:26 recur*
-rw-r--r-- 1 jan users    194 2011-05-20 00:26 recur.mod
-rw-r--r-- 1 jan users 523264 2011-05-20 00:26 testfile
jan@Beryllium:~/modula/rosetta$ wc testfile
     0      1 523264 testfile
```

So the recursion depth is just over half a million.


## MUMPS


```MUMPS
RECURSE
 IF $DATA(DEPTH)=1 SET DEPTH=1+DEPTH
 IF $DATA(DEPTH)=0 SET DEPTH=1
 WRITE !,DEPTH_" levels down"
 DO RECURSE
 QUIT
```

End of the run ...
```txt

1918 levels down
1919 levels down
1920 levels down
 DO RECURSE
 ^
<FRAMESTACK>RECURSE+4^ROSETTA
USER 72d0>

```



## Neko


```ActionScript
/**
 Recursion limit, in Neko
*/

/* This version is effectively unlimited, (50 billion test before ctrl-c) */
sum = 0.0
counter = 0
tco = function(n) {
    sum += n
    counter += 1
    if n > 10000000 return n else tco(n + 1)
}

try $print("Tail call recursion: ", tco(0), " sum: ", sum, "\n")
catch with $print("tco counter: ", counter, " ", with, "\n")

/* Code after tail, these accumulate stack, will run out of space */
sum = 0.0
counter = 0
recurse = function(n) {
    sum += n
    counter += 1
    if n > 1000000 return n else recurse(n + 1)
    return sum
}

try $print("Recurse: ", recurse(0), " sum: ", sum, "\n")
catch with $print("recurse limit exception: ", counter, " ", with, "\n")
```


{{out}}

```txt
prompt$ nekoc recursion-limit.neko
prompt$ neko recursion-limit.n
Tail call recursion: 10000001 sum: 50000015000001
recurse limit exception: 52426 Stack Overflow
```



## NetRexx

Like Java, NetRexx memory allocation is managed by the JVM under which it is run.  The following sample presents runtime memory allocations then begins the recursion run.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.lang.management.

memoryInfo()
digDeeper(0)

/**
 * Just keep digging
 * @param level depth gauge
 */
method digDeeper(level = int) private static binary
  do
    digDeeper(level + 1)
  catch ex = Error
    System.out.println('Recursion got' level 'levels deep on this system.')
    System.out.println('Recursion stopped by' ex.getClass.getName())
  end
  return

/**
 * Display some memory usage from the JVM
 * @see ManagementFactory
 * @see MemoryMXBean
 * @see MemoryUsage
 */
method memoryInfo() private static
  mxBean = ManagementFactory.getMemoryMXBean()   -- get the MemoryMXBean
  hmMemoryUsage = mxBean.getHeapMemoryUsage()    -- get the heap MemoryUsage object
  nmMemoryUsage = mxBean.getNonHeapMemoryUsage() -- get the non-heap MemoryUsage object
  say 'JVM Memory Information:'
  say '      Heap:' hmMemoryUsage.toString()
  say '  Non-Heap:' nmMemoryUsage.toString()
  say '-'.left(120, '-')
  say
  return

```

{{out}}

```txt

JVM Memory Information:
      Heap: init = 0(0K) used = 2096040(2046K) committed = 85000192(83008K) max = 129957888(126912K)
  Non-Heap: init = 24317952(23748K) used = 5375328(5249K) committed = 24317952(23748K) max = 136314880(133120K)
------------------------------------------------------------------------------------------------------------------------

Recursion got 9673 levels deep on this system.
Recursion stopped by java.lang.StackOverflowError

```



## Nim


```nim
proc recurse(i): int =
  echo i
  recurse(i+1)
echo recurse(0)
```

Compiled without optimizations it would stop after 87317 recursions. With optimizations on recurse is translated into a tail-recursive function, without any recursion limit. Instead of waiting for the 87317 recursions you compile with debuginfo activated and check with gdb:

```txt
nim c --debuginfo --lineDir:on recursionlimit.nim
```



## OCaml

When the recursion is a "[[:Category:Recursion|tail-recursion]]" there is no limit.
Which is important because being a functional programming language, OCaml uses recursion to make
loops.

If the recursion is not a tail one, the execution is stopped with the message
"Stack overflow":

```ocaml
# let last = ref 0 ;;
val last : int ref = {contents = 0}
# let rec f i =
    last := i;
    i + (f (i+1))
  ;;
val f : int -> int = <fun>
# f 0 ;;
stack overflow during evaluation (looping recursion?).
# !last ;;
- : int = 262067
```


here we see that the function call stack size is 262067.


```ocaml
(* One can build a function from the idea above, catching the exception *)

let rec_limit () =
  let last = ref 0 in
  let rec f i =
    last := i;
    1 + f (i + 1)
  in
  try (f 0)
  with Stack_overflow -> !last
;;

rec_limit ();;
262064

(* Since with have eaten some stack with this function, the result is slightly lower.
But now it may be used inside any function to get the available stack space *)
```



## Oforth


Limit found is 173510 on Windows system. Should be more on Linux system.


```Oforth
: limit  1+ dup . limit ;

0 limit
```



## ooRexx



```txt

Using ooRexx for the program shown under Rexx:

 rexx pgm 1>x1 2>x2

 puts the numbers in x1 and the error messages in x2

...
2785
2786
8 *-*      call self
....
     8 *-*   call self
     3 *-* call self
Error 11 running C:\work.ooRexx\wc\main.4.1.1.release\Win32Rel\StreamClasses.orx line 366:  Control stack full
Error 11.1:  Insufficient control stack space; cannot continue execution

```



## Oz

{{trans|Scheme}}

Oz supports an unbounded number of tail calls. So the following code can run forever with constant memory use (although the space used to represent <code>Number</code> will slowly increase):

```oz
declare
  proc {Recurse Number}
     {Show Number}
     {Recurse Number+1}
  end
in
  {Recurse 1}
```


With non-tail recursive functions, the number of recursions is only limited by the available memory.


## PARI/GP

As per "Recursive functions" in the Pari/GP users's manual.

```parigp
dive(n) = dive(n+1)
dive(0)
```


The limit is the underlying C language stack.  Deep recursion is detected before the stack is completely exhausted (by checking <code>RLIMIT_STACK</code>) so a <code>gp</code> level error is thrown instead of a segfault.


## Pascal

See [[Find_limit_of_recursion#Delphi | Delphi]]


## Perl

Maximum recursion depth is memory dependent.


```perl
my $x = 0;
recurse($x);

sub recurse ($x) {
   print ++$x,"\n";
   recurse($x);
}
```




```txt

1
2
...
...
10702178
10702179
Out of memory!

```



## Perl 6

Maximum recursion depth is memory dependent.  Values in excess of 1 million are easily achieved.
{{works with|Rakudo|2015.12}}

```perl6
my $x = 0;
recurse;

sub recurse () {
   ++$x;
   say $x if $x %% 1_000_000;
   recurse;
}
```


{{out}}
When manually terminated memory use was on the order of 4Gb:


```txt

1000000
2000000
3000000
4000000
5000000
6000000
7000000
8000000
9000000
10000000
^C

```



## Phix

On a 32-bit version the limit is an impressive 34 million. Of course on real word apps with more parameters etc it will be smaller. Unfortunately other problems are stopping me from testing this on a 64-bit version right now.

```Phix
atom t1 = time()+1

integer depth = 0

procedure recurse()
    if time()>t1 then
        ?depth
        t1 = time()+1
    end if
    depth += 1
    -- only 1 of these will ever get called, of course...
    recurse()
    recurse()
    recurse()
end procedure

recurse()
```

{{out}}

```txt

C:\Program Files (x86)\Phix>p e01
8336837
16334140
20283032
21863323
22547975
22875708
23227196
23536921
24051004
24902668
25518908
26211370
26899260
27457596
27946743
28627343
29129830
29811260
31081002
31893231
32970812
33612604
34624828
34886703
Your program has run out of memory, one moment please
C:\Program Files (x86)\Phix\e01.exw:48 in procedure recurse()
memory allocation failure
... called from C:\Program Files (x86)\Phix\e01.exw:48 in procedure recurse()
... called from C:\Program Files (x86)\Phix\e01.exw:48 in procedure recurse()
... called from C:\Program Files (x86)\Phix\e01.exw:48 in procedure recurse()
... called from C:\Program Files (x86)\Phix\e01.exw:48 in procedure recurse()

Global & Local Variables

--> see C:\Program Files (x86)\Phix\ex.err
Press Enter...
C:\Program Files (x86)\Phix>

```

It takes about 25 seconds to build that stack and slightly longer to tear it down. You should also note that somewhat less clean error reports are likely: even the above could theoretically fail mid-sprintf and hence exercise a completely different error handling path, and there are likely to be several hundred different ways to fail when there is no more memory.


## PHP


```PHP
<?php
function a() {
    static $i = 0;
    print ++$i . "\n";
    a();
}
a();
```


{{out}}

```txt

 1
 2
 3
 [...]
 597354
 597355
 597356
 597357
 597358

 Fatal error: Allowed memory size of 134217728 bytes exhausted (tried to allocate 261904 bytes) in [script-location.php] on line 5

```



## PicoLisp

The 64-bit and the 32-bit version behave slightly different. While the 32-bit
version imposes no limit on its own, and relies on the 'ulimit' setting of the
caller, the 64-bit version segments the available stack (likewise depending on
'ulimit') and allows each (co)routine a maximal stack size as configured by
'[http://software-lab.de/doc/refS.html#stack stack]'.
===32-bit version===

```txt
$ ulimit -s
8192
$ pil +
: (let N 0 (recur (N) (recurse (msg (inc N)))))
...
730395
730396
730397
Segmentation fault
```

===64-bit version===

```txt
$ ulimit -s
unlimited
$ pil +
: (stack)  # The default stack segment size is 64 MB
-> 64

: (co 'a (yield 7))  # Start a dummy coroutine
-> 7

: (let N 0 (recur (N) (recurse (println (inc N)))))
...
2475
2476
2477
Stack overflow
?
```



## PL/I


```PL/I

recurs: proc options (main) reorder;
dcl sysprint file;
dcl mod      builtin;

dcl ri       fixed bin(31) init (0);

recursive: proc recursive;
  ri += 1;
  if mod(ri, 1024) = 1 then
    put data(ri);

  call recursive();
end recursive;

call recursive();
end recurs;

```


Result (abbreviated):

 ...
 RI=       4894721;
 RI=       4895745;
 RI=       4896769;
 RI=       4897793;
 RI=       '''4898817''';

At this stage the program, running on z/OS with a '''REGION=0M''' on the EXEC statement (i.e. grab as much storage as you like), ABENDs with a '''USER COMPLETION CODE=4088 REASON CODE=000003EC'''

Obviously, if the procedure '''recursive''' would have contained local variables, the depth of recursion would be reached much earlier...


## PowerShell

{{works with|PowerShell|2}}
When the overflow exception is thrown, the entire stack collapses. But anticipating this,
we can leverage PowerShell features to get and process all of the results from before the
exception. In PowerShell, when anything is written the the output stream WITHOUT the "Return"
keyword, processing continues, so we can successfully return data from the function even
if the function never successfully completes. The original calling line will also be
terminated when the exception is thrown, but if instead of assigning it to a variable,
we send the results to a pipeline, we can process the earlier results before handling the
exception.

```PowerShell

function TestDepth ( $N )
    {
    $N
    TestDepth ( $N + 1 )
    }

try
    {
    TestDepth 1 | ForEach { $Depth = $_ }
    }
catch
    {
    "Exception message: " + $_.Exception.Message
    }
"Last level before error: " + $Depth

```

{{out}}

```txt

Exception message: The script failed due to call depth overflow.
Last level before error: 4994

```



## PureBasic

The recursion limit is primarily determined by the stack size.  The stack size can be changed when compiling a program by specifying the new size using '/stack:NewSize' in the linker file.


### Procedural


In addition to the stack size the recursion limit for procedures is further limited by the procedure's parameters and local variables which are also stored on the same stack.

```PureBasic
Procedure Recur(n)
  PrintN(str(n))
  Recur(n+1)
EndProcedure

Recur(1)
```

 Stack overflow after 86317 recursions on x86 Vista.


### Classic


```PureBasic
rec:
  PrintN(str(n))
  n+1
  Gosub rec
  Return
```

 Stack overflow after 258931 recursions on x86 Vista.


## Python


```python
import sys
print(sys.getrecursionlimit())
```


To set it:


```python
import sys
sys.setrecursionlimit(12345)
```


Or, we can test it:


```python
def recurse(counter):
  print(counter)
  counter += 1
  recurse(counter)
```


Giving:


```python
File "<stdin>", line 2, in recurse
RecursionError: maximum recursion depth exceeded while calling a Python object
996
```


Which we could change if we wanted to.

We can catch the RecursionError and keep going a bit further:


```python
def recurseDeeper(counter):
    try:
        print(counter)
        recurseDeeper(counter + 1)
    except RecursionError:
        print("RecursionError at depth", counter)
        recurseDeeper(counter + 1)
```


Giving:


```python
1045
Fatal Python error: Cannot recover from stack overflow.
```



## R

R's recursion is counted by the number of expressions to be evaluated, rather than the number of function calls.

```r
#Get the limit
options("expressions")

#Set it
options(expressions = 10000)

#Test it
recurse <- function(x)
{
  print(x)
  recurse(x+1)

}
recurse(0)
```



## Racket


```Racket
#lang racket
(define (recursion-limit)
  (with-handlers ((exn? (lambda (x) 0)))
    (add1 (recursion-limit))))
```


This should theoretically return the recursion limit, as the function can't be tail-optimized and there's an exception handler to return a number when an error is encountered. For this to work one has to give the Racket VM the maximum possible memory limit and wait.


## Retro

When run, this will display the address stack depth until it reaches the max depth. Once the address stack is full, Retro will crash.


```Retro
: try -6 5 out wait 5 in putn cr try ;
```



## REXX


### recursive procedure

On (IBM's) VM/CMS, the limit of recursion was built-into CMS to stop run-away EXEC programs (this

included EXEC[0], EXEC2, and REXX) being called recursively;   it was either 200 or 250 as I recall.

This limit was maybe changed later to allow the user to specify the limit.  My memory is really fuzzy about these details.

```rexx
/*REXX program finds the recursion limit:   a subroutine that repeatably calls itself.  */
parse version x;     say x;     say             /*display which REXX is being used.     */
#=0                                             /*initialize the numbers of invokes to 0*/
call self                                       /*invoke the  SELF  subroutine.         */
                                                /* [↓]  this will never be executed.    */
exit                                            /*stick a fork in it,  we're all done.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
self: procedure expose #                        /*declaring that  SELF  is a subroutine.*/
      #=#+1                                     /*bump number of times SELF is invoked. */
      say #                                     /*display the number of invocations.    */
      call self                                 /*invoke ourselves recursively.         */
```

'''output'''   when using Regina 3.6 under Windows/XP Pro:

```txt

REXX-Regina_3.6(MT) 5.00 31 Dec 2011
 .
 .
 .
164405
164406
164407
System resources exhausted

```

[Your mileage will vary.]

```txt

For Regina 3.2,     it was   3,641
For Regina 3.3,      "  "    4,234
For Regina 3.4,      "  "  945,144
For Regina 3.5,      "  "  164,560
For Regina 3.6,      "  "  164,407
For Regina 3.7,      "  "     "
For Regina 3.8,      "  "     "
For Regina 3.8.2,    "  "     "
For Regina 3.9.0,    "  "  164,527
For Regina 3.9.1,    "  "     "

```

Note that the above recursion limit will be less and it's dependent upon how much virtual memory the program itself uses,

this would include REXX variables and their values, and the program source (as it's kept in virtual memory also),

and the size of the REXX.EXE and REXX.DLL programs, and any other programs executing in the Windows DOS (including

either the CMD.EXE or COMMAND.COM) shell).



'''output'''   when using Personal REXX under Windows/XP Pro:

The recursion level wasn't captured, but the last number shown was 240.

```txt

REXX/Personal 4.00 21 Mar 1992
 .
 .
 .
    10 +++ call self
    10 +++ call self
    10 +++ call self
     4 +++ call SELF
Error 5 on line 10 of D:\SELF.REX: Machine resources exhausted

```

'''output''' &nbsp when using R4 REXX under Windows/XP Pro:

```txt

REXX-r4 4.00 29 Apr 2012
 .
 .
 .
505
506
507
An unexpected error occurred

```

'''output'''   when using ROO REXX under Windows/XP Pro:

```txt

REXX-roo 4.00 28 Jan 2007

 .
 .
 .
380
381
382
An unexpected error occurred

```

'''output'''   when using ooRexx under Windows/7

```txt
See section ooRexx
```



### recursive subroutine

All REXXes were executed under Windows/XP Pro.

```rexx
/*REXX program finds the recursion limit:   a subroutine that repeatably calls itself.  */
parse version x;     say x;     say             /*display which REXX is being used.     */
#=0                                             /*initialize the numbers of invokes to 0*/
call self                                       /*invoke the  SELF  subroutine.         */
                                                /* [↓]  this will never be executed.    */
exit                                            /*stick a fork in it,  we're all done.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
self:  #=#+1                                    /*bump number of times SELF is invoked. */
       say #                                    /*display the number of invocations.    */
       call self                                /*invoke ourselves recursively.         */
```

'''output'''   (paraphrased and edited)

```txt

For Regina 3.2,     it was   3,641
For Regina 3.3,      "  "  945,144
For Regina 3.4,      "  "     "
For Regina 3.5,      "  "  164,560
For Regina 3.6,      "  "  164,407
For Regina 3.7,      "  "     "
For Regina 3.8,      "  "     "
For Regina 3.8.2,    "  "     "
For Regina 3.9.0,    "  "  164,527
For Regina 3.9.1,    "  "     "
For Personal REXX,  it was     240  (the same)
For R4,             it was     507  (the same)
For ROO,            it was     382  (the same)
For ooRexx          it is    2,480  (the same)

```



## Ring


```ring

recurse(0)

func recurse x
     see ""+ x + nl
     recurse(x+1)

```



## Ruby


```ruby
def recurse x
  puts x
  recurse(x+1)
end

recurse(0)
```

{{out}} Produces a SystemStackError:

```txt

.
.
.
6074
recurse.rb:3:in `recurse': stack level too deep (SystemStackError)
	from recurse.rb:3:in `recurse'
	from recurse.rb:6

```


when tracking Stack overflow exceptions ; returns 8732 on my computer :


```ruby
def recurse n
  recurse(n+1)
rescue SystemStackError
  n
end

puts recurse(0)
```



## Run BASIC


```runbasic
a = recurTest(1)

function recurTest(n)
if n mod 100000 then cls:print n
if n > 327000 then [ext]
   n = recurTest(n+1)
[ext]
end function
```


```txt
327000
```



## Rust


```rust
fn recurse(n: i32) {
    println!("depth: {}", n);
    recurse(n + 1)
}

fn main() {
    recurse(0);
}
```


{{out}}

```txt
...
depth: 18433
depth: 18434
depth: 18435

thread '<main>' has overflowed its stack
An unknown error occurred

To learn more, run the command again with --verbose.
```



## Sather


```sather
class MAIN is
  attr r:INT;
  recurse is
    r := r + 1;
    #OUT + r + "\n";
    recurse;
  end;
  main is
    r := 0;
    recurse;
  end;
end;
```


Segmentation fault is reached when r is 130560.


## Scala


```scala
def recurseTest(i:Int):Unit={
   try{
      recurseTest(i+1)
   } catch { case e:java.lang.StackOverflowError =>
      println("Recursion depth on this system is " + i + ".")
   }
}
recurseTest(0)
```

{{out}} depending on the current stack size:

```txt
Recursion depth on this system is 4869.
```

If your function is tail-recursive the compiler transforms it into a loop.

```scala
def recurseTailRec(i:Int):Unit={
   if(i%100000==0) println("Recursion depth is " + i + ".")
   recurseTailRec(i+1)
}
```



## Scheme


```scheme
(define (recurse number)
  (begin (display number) (newline) (recurse (+ number 1))))

(recurse 1)
```

Implementations of Scheme are required to support an unbounded number of tail calls. Furthermore, implementations are encouraged, but not required, to support exact integers of practically unlimited size.


## Sidef

Maximum recursion depth is memory dependent.

```ruby
func recurse(n) {
   say n;
   recurse(n+1);
}

recurse(0);
```

{{out}}

```txt

0
1
2
...
...
357077
357078
357079

```



## Smalltalk


In the Squeak dialect of Smalltalk:


```smalltalk

Object subclass: #RecursionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RosettaCode'

```


Add the following method:


```smalltalk

counter: aNumber
	^self counter: aNumber + 1

```


Call from the Workspace:


```smalltalk

r := RecursionTest new.
r counter: 1.

```


After some time the following error pops up:

    Warning! Squeak is almost out of memory!
    Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don't panic, but do proceed with caution.
    Here are some suggestions:
    If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.
    If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
    * Close any windows that are not needed.
    * Get rid of some large objects (e.g., images).
    * Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Squeak VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.
    If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!).

Other dialects raise an exception:

```smalltalk

counter := 0.
down := [ counter := counter + 1. down value ].
down on: RecursionError do:[
   'depth is ' print. counter printNL
].
```



## Swift


```swift
var n = 1

func recurse() {
    print(n)
    n += 1
    recurse()
}

recurse()
```



## Tcl


```tcl
proc recur i {
    puts "This is depth [incr i]"
    catch {recur $i}; # Trap error from going too deep
}
recur 0
```

The tail of the execution trace looks like this:

```txt

This is depth 995
This is depth 996
This is depth 997
This is depth 998
This is depth 999

```

Note that the maximum recursion depth is a tunable parameter, as is shown in this program:

```tcl
# Increase the maximum depth
interp recursionlimit {} 1000000
proc recur i {
    if {[catch {recur [incr i]}]} {
        # If we failed to recurse, print how far we got
	puts "Got to depth $i"
    }
}
recur 0
```

For Tcl 8.5 on this platform, this prints:

```txt

Got to depth 6610

```

At which point it has exhausted the C stack, a trapped error. Tcl 8.6 uses a stackless execution engine, and can go ''very'' deep if required:

```txt

Got to depth 999999

```



## TSE SAL


```TSE SAL

// library: program: run: recursion: limit <description>will stop at 3616</description> <version>1.0.0.0.3</version> <version control></version control> (filenamemacro=runprrli.s) [kn, ri, su, 25-12-2011 23:12:02]
PROC PROCProgramRunRecursionLimit( INTEGER I )
 Message( I )
 PROCProgramRunRecursionLimit( I + 1 )
END

PROC Main()
 PROCProgramRunRecursionLimit( 1 )
END

```



## TXR



```txrlisp
(set-sig-handler sig-segv
  (lambda (signal async-p) (throw 'out)))

(defvar *count* 0)

(defun recurse ()
  (inc *count*)
  (recurse))

(catch (recurse)
  (out () (put-line `caught segfault!\nreached depth: @{*count*}`)))
```


{{out}}


```txt
$ txr limit-of-recursion.tl
caught segfault!
reached depth: 10909
```



## UNIX Shell

{{works with|Bourne Again SHell}}


```bash
recurse()
{
  # since the example runs slowly, the following
  # if-elif avoid unuseful output; the elif was
  # added after a first run ended with a segmentation
  # fault after printing "10000"
  if [[ $(($1 % 5000)) -eq 0 ]]; then
      echo $1;
  elif [[ $1 -gt 10000 ]]; then
      echo $1
  fi
  recurse $(($1 + 1))
}

recurse 0
```


The Bash reference manual says <cite>No limit is placed on the number of recursive calls</cite>, nonetheless a segmentation fault occurs at 13777 (Bash v3.2.19 on 32bit GNU/Linux)


## Ursa


```ursa
def recurse (int counter)
    try
        recurse (+ counter 1)
    catch recursionerror
        out "the limit of recursion was " counter endl console
    end try
end

recurse 1
```



## VBA



```vb

Option Explicit

Sub Main()
    Debug.Print "The limit is : " & Limite_Recursivite(0)
End Sub

Function Limite_Recursivite(Cpt As Long) As Long
    Cpt = Cpt + 1               'Count
    On Error Resume Next
    Limite_Recursivite Cpt      'recurse
    On Error GoTo 0
    Limite_Recursivite = Cpt    'return
End Function

```

{{out}}

```txt
The limit is : 6442
```



## VBScript

Haven't figured out how to see the depth. And this depth is that of calling the O/S rather than calling within.


```vb
'mung.vbs
option explicit

dim c
if wscript.arguments.count = 1 then
	c = wscript.arguments(0)
	c = c + 1
else
	c = 0
end if
wscript.echo "[Depth",c & "] Mung until no good."
CreateObject("WScript.Shell").Run "cscript Mung.vbs " & c, 1, true
wscript.echo "[Depth",c & "] no good."
```


Okay, the internal limits version.


```vb
'mung.vbs
option explicit

sub mung(c)
	dim n
	n=c+1
	wscript.echo "[Level",n & "] Mung until no good"
	on error resume next
	mung n
	on error goto 0
	wscript.echo "[Level",n & "] no good"
end sub

mung 0
```


{{out}} (abbrev.):

```txt
[Level 1719] Mung until no good
[Level 1720] Mung until no good
[Level 1721] Mung until no good
[Level 1722] Mung until no good
[Level 1722] no good
[Level 1721] no good
[Level 1720] no good
[Level 1719] no good
```



## x86 Assembly


{{works with|nasm}}


```asm
   global main

   section .text

main
	xor	eax, eax
	call   	recurse
	ret

recurse
	add	eax, 1
	call 	recurse
	ret
```


I've used gdb and the command <tt>print $eax</tt> to know when the segmentation fault occurred. The result was 2094783.


## zkl


```zkl
fcn{self.fcn()}()
```

{{out}}

```txt

Stack trace for VM#1 ():
   Cmd.__fcn#1_2 addr:3  args(0) reg(0) R
   <repeats 2096 times>
   Cmd.__constructor addr:3  args(0) reg(0) R
   startup.__constructor addr:2242  args(0) reg(1) ER
   startup.__constructor addr:2178  args(0) reg(22)
Exception thrown: AssertionError(That is one big stack, infinite recursion?)

```

