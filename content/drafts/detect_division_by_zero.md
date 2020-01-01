+++
title = "Detect division by zero"
description = ""
date = 2019-10-08T10:24:32Z
aliases = []
[extra]
id = 2596
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Simple]]

;Task:
Write a function to detect a   ''divide by zero error''   without checking if the denominator is zero.





## 8th

Division by zero results in the value "Inf":

```Forth

1 0 n:/ Inf? . cr

```

{{out}}
```txt
true
```


## ABAP


```ABAP
report zdiv_zero
data x type i.
try.
  x = 1 / 0.
catch CX_SY_ZERODIVIDE.
  write 'Divide by zero.'.
endtry.

```



## Ada


```ada
-- Divide By Zero Detection

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Divide_By_Zero is
   Fnum : Float := 1.0;
   Fdenom : Float := 0.0;
   Fresult : Float;
   Inum : Integer := 1;
   Idenom : Integer := 0;
   Iresult : Integer;
begin
   begin
      Put("Integer divide by zero: ");
      Iresult := Inum / Idenom;
      Put(Item => Iresult);
   exception
      when Constraint_Error =>
         Put("Division by zero detected.");
   end;
   New_Line;
   Put("Floating point divide by zero: ");
   Fresult := Fnum / Fdenom;
   if Fresult > Float'Last or Fresult < Float'First then
      Put("Division by zero detected (infinite value).");
   else
      Put(Item => Fresult, Aft => 9, Exp => 0);
   end if;
   New_Line;
end Divide_By_Zero;
```

{{out}}

```txt

Integer divide by zero: Division by zero detected.
Floating point divide by zero: Division by zero detected (infinite value).

```



## Aime


```aime
integer
divide(integer n, integer d)
{
    return n / d;
}

integer
can_divide(integer n, integer d)
{
    return !trap(divide, n, d);
}

integer
main(void)
{
    if (!can_divide(9, 0)) {
        o_text("Division by zero.\n");
    }

    return 0;
}
```


{{out}}

```txt

Division by zero.

```


The Aime interpreter reports execution errors by default, printing on standard error:

```txt

aime: can_divide: 4: division by zero

```



## ALGOL 68

The USSR's ALGOL 68 had a [http://vak.ru/lib/exe/fetch.php/book/gost/pdf/gost-27975-88.pdf "GOST 27975-88 Programming language ALGOL 68 extended (Язык программирования АЛГОЛ 68 расширенный)"] that included additional
keywords '''on''', '''exception''', '''raise'''.  This was an extension,
and probably made only an appearance in the Leningrad compiler (Алгола
68 Ленинград).

The following code sample implements ''zero division'',
without using language extensions or access to hardware interrupts.

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
PROC raise exception= ([]STRING args)VOID: (
  put(stand error, ("Exception: ",args, newline));
  stop
);

PROC raise zero division error := VOID:
  raise exception("integer division or modulo by zero");

PROC int div  = (INT a,b)REAL: a/b;
PROC int over = (INT a,b)INT:  a%b;
PROC int mod  = (INT a,b)INT: a%*b;

BEGIN
  OP /  = (INT a,b)REAL: ( b = 0 | raise zero division error; SKIP | int div (a,b) );
  OP %  = (INT a,b)INT:  ( b = 0 | raise zero division error; SKIP | int over(a,b) );
  OP %* = (INT a,b)INT:  ( b = 0 | raise zero division error; SKIP | int mod (a,b) );

  PROC a different handler = VOID: (
      put(stand error,("caught division by zero",new line));
      stop
  );

  INT x:=1, y:=0;
  raise zero division error := a different handler;
  print(x/y)
END
```

{{out}}

```txt

caught division by zero

```



## ALGOL W

Algol W allows the program to handle a number of system defined exceptions including INTDIVZERO and DIVZERO - integer and real division by zero.


A count of the number of times the exception is allowed is decremented each time the exception occurs. If this reaches 0, the program crashes. If it is greater than 0, the program continues and XCPNOTED(exception) returns true. This example uses this to detect integer and real division by 0. The INTDIVERO exception also occurs if the remainder (modulo) operator is used with 0.

```algolw
begin
    % integer division procedure                                                 %
    %     sets c to a divided by b, returns true if the division was OK,         %
    %                                      false if there was division by zero   %
    logical procedure divideI ( integer value a, b; integer result c ) ;
    begin
        % set exception handling to allow integer division by zero to occur once %
        INTDIVZERO := EXCEPTION( false, 1, 0, false, "INTDIVZERO" );
        c := a div b;
        not XCPNOTED(INTDIVZERO)
    end divideI ;
    % real division procedure                                                    %
    %     sets c to a divided by b, returns true if the division was OK,         %
    %                                      false if there was division by zero   %
    logical procedure divideR ( long real value a, b; long real result c ) ;
    begin
        % set exception handling to allow realdivision by zero to occur once     %
        DIVZERO := EXCEPTION( false, 1, 0, false, "DIVZERO" );
        c := a / b;
        not XCPNOTED(DIVZERO)
    end divideR ;
    integer c;
    real    d;
    write( divideI( 4, 2, c ) ); % prints false as no exception                  %
    write( divideI( 5, 0, c ) ); % prints true as division by zero was detected  %
    write( divideR( 4, 2, d ) ); % prints false as no exception                  %
    write( divideR( 5, 0, d ) )  % prints true as division by zero was detected  %
end.
```



## AutoHotkey


```AutoHotkey
ZeroDiv(num1, num2) {
  If ((num1/num2) != "")
    MsgBox % num1/num2
  Else
    MsgBox, 48, Warning, The result is not valid (Divide By Zero).
}
ZeroDiv(0, 3) ; is ok
ZeroDiv(3, 0) ; divize by zero alert
```



## BASIC


=
## Applesoft BASIC
=
The error code for division by zero is 133.  There is a good overview of Applesoft ONERR GOTO handling here:
http://newsgroups.derkeiler.com/Archive/Comp/comp.sys.apple2.programmer/2010-04/msg00000.html


```BASIC
 100  REM TRY
 110  ONERR  GOTO 200
 120 D =  - 44 / 0
 190  END
 200  REM CATCH
 210 E =  PEEK (222) <  > 133
 220  POKE 216,0: REM ONERR OFF
 230  IF E THEN  RESUME
 240  CALL  - 3288: REM RECOVER
 250  PRINT "DIVISION BY ZERO"

```


=
## BBC BASIC
=

```bbcbasic
      PROCdivide(-44, 0)
      PROCdivide(-44, 5)
      PROCdivide(0, 5)
      PROCdivide(5, 0)
      END

      DEF PROCdivide(numerator, denominator)
      ON ERROR LOCAL IF FALSE THEN
        REM 'Try' clause:
        PRINT numerator / denominator
      ELSE
        REM 'Catch' clause:
        CASE ERR OF
          WHEN 18: PRINT "Division by zero"
          WHEN 20: PRINT "Number too big"
          OTHERWISE RESTORE LOCAL : ERROR ERR, REPORT$
        ENDCASE
      ENDIF
      ENDPROC
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 WHEN EXCEPTION USE ERROR
110   FOR I=5 TO-2 STEP-1
120     PRINT 10/I
130   NEXT
140 END WHEN
150 HANDLER ERROR
160   IF EXTYPE=3001 THEN PRINT EXSTRING$(EXTYPE);" in line";EXLINE
170   CONTINUE
180 END HANDLER
```


=
## Liberty BASIC
=

```lb
result = DetectDividebyZero(1, 0)


Function DetectDividebyZero(a, b)
    On Error GoTo [Error]
        DetectDividebyZero= (a/ b)
        Exit Function
    [Error]
        If Err = 11 Then '11 is the error number raised when divide by zero occurs
            Notice "Divide by Zero Detected!"
        End If
End Function
```


=
## Locomotive Basic
=

```locobasic
10 ON ERROR GOTO 60
20 PRINT 2/3
30 PRINT 3/5
40 PRINT 4/0
50 END
60 IF ERR=11 THEN PRINT "Division by zero in line"ERL:RESUME 50
```


{{out}}

```txt
 0.666666667
 0.6
Division by zero in line 40
```


=
## PureBasic
=

PureBasic can be compiled with the [http://www.purebasic.com/documentation/onerror/index.html OnError] library included which gives a way to track program errors without losing speed, doing so gives support for the following functions;
*ErrorAddress()
*ErrorCode()
*ErrorFile()
*ErrorLine()
*ErrorMessage()
*ErrorRegister()
*ErrorTargetAddress()
*ExamineAssembly()
*InstructionAddress()
*InstructionString()
*NextInstruction()
*OnErrorCall()
*OnErrorDefault()
*OnErrorExit()
*OnErrorGoto()
*RaiseError()

This way the final version of a program can still intercept program errors and provide some function, or information about the error to the user so he can report it back to the developer.


'''With Integers & OnError Library'''

```PureBasic
;Set up a Procedure to handle any Error
Procedure MyErrorHandler()
  Define txt$="The following error happened."+#CRLF$+ ErrorMessage()+"at line  "+Str(ErrorLine())
  MessageRequester("OnError test", txt$)
EndProcedure

; Tell where to go if an Error happens
OnErrorCall(@MyErrorHandler())

;Now, do something very stupid so that we may see an Error...
Repeat
  A=Random(100)/Random(100)
ForEver
```

[[Image:OnError.png]]


'''With Floats, and without OnError library'''

```PureBasic
Define.d a, b
Debug a/b
```

Results in;
<tt>-1.#IND</tt>

=
## Run BASIC
=

```runbasic
on error goto [error]
a = 1 / 0
wait

[error] ' error 11 is division by zero err number
If err = 11 Then print "Division by Zero"
wait
```


==={{header|TI-89 BASIC}}===
<code>1/0 = undef</code> is true.


## Batch File


```dos
@echo off
set /a dummy=5/0 2>nul

if %errorlevel%==1073750993 echo I caught a division by zero operation...
exit /b 0
```


## C

Technically, under the C standard, division by zero (regardless of type) is undefined behavior, so there is no standard way to run the division and then try to "detect" it later.

: The result of the / operator is the quotient from the division of the first operand by the second; the result of the % operator is the remainder. In both operations, if the value of the second operand is zero, the behavior is undefined.

: -- C99 standard, section 6.5.5 paragraph 5

==={{libheader|POSIX}}===
Some systems will raise SIGFPE if a program divides by zero.


```c
#include <limits.h> /* INT_MIN */
#include <setjmp.h>	/* siglongjmp(), sigsetjmp() */
#include <stdio.h>	/* perror(), printf() */
#include <stdlib.h>	/* exit() */
#include <signal.h>	/* sigaction(), sigemptyset() */

static sigjmp_buf fpe_env;

/*
 * This SIGFPE handler jumps to fpe_env.
 *
 * A SIGFPE handler must not return, because the program might retry
 * the division, which might cause an infinite loop. The only safe
 * options are to _exit() the program or to siglongjmp() out.
 */
static void
fpe_handler(int signal, siginfo_t *w, void *a)
{
	siglongjmp(fpe_env, w->si_code);
	/* NOTREACHED */
}

/*
 * Try to do x / y, but catch attempts to divide by zero.
 */
void
try_division(int x, int y)
{
	struct sigaction act, old;
	int code;
	/*
	 * The result must be volatile, else C compiler might delay
	 * division until after sigaction() restores old handler.
	 */
	volatile int result;

	/*
	 * Save fpe_env so that fpe_handler() can jump back here.
	 * sigsetjmp() returns zero.
	 */
	code = sigsetjmp(fpe_env, 1);
	if (code == 0) {
		/* Install fpe_handler() to trap SIGFPE. */
		act.sa_sigaction = fpe_handler;
		sigemptyset(&act.sa_mask);
		act.sa_flags = SA_SIGINFO;
		if (sigaction(SIGFPE, &act, &old) < 0) {
			perror("sigaction");
			exit(1);
		}

		/* Do division. */
		result = x / y;

		/*
		 * Restore old hander, so that SIGFPE cannot jump out
		 * of a call to printf(), which might cause trouble.
		 */
		if (sigaction(SIGFPE, &old, NULL) < 0) {
			perror("sigaction");
			exit(1);
		}

		printf("%d / %d is %d\n", x, y, result);
	} else {
		/*
		 * We caught SIGFPE. Our fpe_handler() jumped to our
		 * sigsetjmp() and passes a nonzero code.
		 *
		 * But first, restore old handler.
		 */
		if (sigaction(SIGFPE, &old, NULL) < 0) {
			perror("sigaction");
			exit(1);
		}

		/* FPE_FLTDIV should never happen with integers. */
		switch (code) {
		case FPE_INTDIV: /* integer division by zero */
		case FPE_FLTDIV: /* float division by zero */
			printf("%d / %d: caught division by zero!\n", x, y);
			break;
		default:
			printf("%d / %d: caught mysterious error!\n", x, y);
			break;
		}
	}
}

/* Try some division. */
int
main()
{
	try_division(-44, 0);
	try_division(-44, 5);
	try_division(0, 5);
	try_division(0, 0);
	try_division(INT_MIN, -1);
	return 0;
}
```


{{out}} using OpenBSD/amd64:

```txt
-44 / 0: caught division by zero!
-44 / 5 is -8
0 / 5 is 0
0 / 0: caught division by zero!
-2147483648 / -1: caught division by zero!
```


The last line is a mistake: the system confused an overflow (INT_MIN / -1 would be INT_MAX + 1) with division by zero and raised SIGFPE. The system normally ignores overflow.


## C++


```cpp

#include<iostream>
#include<csignal> /* for signal */
#include<cstdlib>

using namespace std;

void fpe_handler(int signal)
{
    cerr << "Floating Point Exception: division by zero" << endl;
    exit(signal);
}

int main()
{
    // Register floating-point exception handler.
    signal(SIGFPE, fpe_handler);

    int a = 1;
    int b = 0;
    cout << a/b << endl;

    return 0;
}

```


=={{header|C sharp|C#}}==
{{works with|int, long, decimal}}
The floating point types (float, double) don't raise an exception, but return the values Infinity or NaN as appropriate.


```csharp
using System;

namespace RosettaCode {
    class Program {
        static void Main(string[] args) {
            int x = 1;
            int y = 0;
            try {
               int z = x / y;
            } catch (DivideByZeroException e) {
                Console.WriteLine(e);
            }

        }
    }
}
```



## Ceylon


```ceylon
shared void run() {

	//integers divided by zero throw an exception
	try {
		value a = 1 / 0;
	} catch (Exception e) {
		e.printStackTrace();
	}

	//floats divided by zero produce infinity
	print(1.0 / 0 == infinity then "division by zero!" else "not division by zero!");
}
```



## Clojure


After catching the ArithmeticException, print the error message, and then try and recover by returning some meaningful value. In this case, if x > 0,
return +inf, if 0, NaN, otherwise -inf.


```lisp
(defn safe-/ [x y]
  (try (/ x y)
    (catch ArithmeticException _
      (println "Division by zero caught!")
      (cond (> x 0)   Double/POSITIVE_INFINITY
            (zero? x) Double/NaN
            :else     Double/NEGATIVE_INFINITY) )))
```



## COBOL


```cobol
DIVIDE foo BY bar GIVING foobar
    ON SIZE ERROR
        DISPLAY "Division by zero detected!"
END-DIVIDE
```



## Common Lisp



```lisp
(handler-case (/ x y)
  (division-by-zero () (format t "division by zero caught!~%")))
```



## D


```d
import std.stdio, std.string, std.math, std.traits;

string divCheck(T)(in T numer, in T denom)
if (isIntegral!T || isFloatingPoint!T) {
    Unqual!(typeof(numer / denom)) result;
    string msg;

    static if (isIntegral!T) {
        try {
            result = numer / denom;
        } catch(Error e) {
            msg = "| " ~ e.msg ~ " (by Error)";
            result = T.max;
        }
    } else { // Floating Point Type.
        result = numer / denom;
        if (numer.isNormal && result.isInfinity) {
            msg = "| Division by Zero";
        } else if (result != 0 && !result.isNormal) {
            if (numer.isNaN)
                msg = "| NaN numerator";
            else if (denom.isNaN)
                msg = "| NaN denominator";
            else if (numer.isInfinity)
                msg = "| Inf numerator";
            else
                msg = "| NaN (Zero Division by Zero)";
        }
    }

    return format("%5s %s", format("%1.1g", real(result)), msg);
}

void main() {
    writeln("Division with check:");
    writefln("int     1/ 0:   %s", divCheck(1, 0));
    writefln("ubyte   1/ 0:   %s", divCheck(ubyte(1), ubyte(0)));
    writefln("real    1/ 0:   %s", divCheck(1.0L, 0.0L));
    writefln("real   -1/ 0:   %s", divCheck(-1.0L, 0.0L));
    writefln("real    0/ 0:   %s", divCheck(0.0L, 0.0L));
    writeln;
    writefln("real   -4/-2:   %s", divCheck(-4.0L,-2.0L));
    writefln("real    2/-inf: %s", divCheck(2.0L, -real.infinity));
    writeln;
    writefln("real -inf/-2:   %s", divCheck(-real.infinity, -2.0L));
    writefln("real +inf/-2:   %s", divCheck(real.infinity, -2.0L));
    writefln("real  nan/-2:   %s", divCheck(real.nan, -2.0L));
    writefln("real   -2/ nan: %s", divCheck(-2.0L, real.nan));
    writefln("real  nan/ 0:   %s", divCheck(real.nan, 0.0L));
    writefln("real  inf/ inf: %s",
             divCheck(real.infinity, real.infinity));
    writefln("real  nan/ nan: %s", divCheck(real.nan, real.nan));
}
```

{{out}}

```txt
Division with check:
int     1/ 0:   2e+09 | Integer Divide by Zero (by Error)
ubyte   1/ 0:   3e+02 | Integer Divide by Zero (by Error)
real    1/ 0:     inf | Division by Zero
real   -1/ 0:    -inf | Division by Zero
real    0/ 0:    -nan | NaN (Zero Division by Zero)

real   -4/-2:       2
real    2/-inf:    -0

real -inf/-2:     inf | Inf numerator
real +inf/-2:    -inf | Inf numerator
real  nan/-2:     nan | NaN numerator
real   -2/ nan:   nan | NaN denominator
real  nan/ 0:     nan | NaN numerator
real  inf/ inf:  -nan | Inf numerator
real  nan/ nan:   nan | NaN numerator
```


=={{header|Déjà Vu}}==

```dejavu
divcheck x y:
    true
    try:
        drop / x y
    catch value-error:
        not

if divcheck 1 0:
    !print "Okay"
else:
    !print "Division by zero"
```

{{out}}

```txt
Division by zero
```



## Delphi


```Delphi
program DivideByZero;

{$APPTYPE CONSOLE}

uses SysUtils;

var
  a, b: Integer;
begin
  a := 1;
  b := 0;
  try
    WriteLn(a / b);
  except
    on e: EZeroDivide do
      Writeln(e.Message);
  end;
end.
```



## E



```e
def divide(numerator, denominator) {
    def floatQuotient := numerator / denominator
    if (floatQuotient.isNaN() || floatQuotient.isInfinite()) {
        return ["zero denominator"]
    } else {
        return ["ok", floatQuotient]
    }
}
```



## ECL

Division by zero defaults to generating a zero result (0), rather than reporting a "divide by zero" error. This avoids invalid or unexpected data aborting a long job. The default behavior can be changed using #OPTION.

Evaluate to zero - default behavior

```ECL

DBZ(REAL8 Dividend,INTEGER8 Divisor) := Quotient/Divisor;

#option ('divideByZero', 'zero');
DBZ(10,0); //returns 0.0

```

Stop and report a division by zero error:

```ECL

DBZ(REAL8 Dividend,INTEGER8 Divisor) := Quotient/Divisor;
#option ('divideByZero', 'fail');
DBZ(10,0); //returns error message "Error:    System error: -1: Division by zero (0, 0), -1,"

```

Returns "nan":

```ECL

DBZ(REAL8 Dividend,INTEGER8 Divisor) := Quotient/Divisor;
#option ('divideByZero', 'nan');
DBZ(10,0); //returns 'nan'

/* NOTE: This is only currently supported for real numbers. Division by zero creates a quiet NaN,
   which will propogate through any real expressions it is used in.
   You can use NOT ISVALID(x) to test if the value is a NaN.
   Integer and decimal division by zero continue to return 0.
*/

```



## Eiffel

{{works with|SmartEiffel}} version 2.4

In a file called main.e:

```eiffel
class MAIN
    creation main
    feature main is
        local
            x, y: INTEGER;
            retried: BOOLEAN;
        do
            x := 42;
            y := 0;

            if not retried then
                io.put_real(x / y);
            else
                print("NaN%N");
            end
        rescue
            print("Caught division by zero!%N");
            retried := True;
            retry
        end
end
```

Note: The "rescue" statement catches ''every'' exception.


## Ela



```ela
open core number

x /. y = try Some (x `div` y) with
             _ = None

(12 /. 2, 12 /. 0)
```


Output:


```txt
(Some 6, None)
```


Of course the cleanest way to implement the safe division function is through pattern matching:


```ela
x /. 0 = None
x /. y = Some (x / y)
```


But it doesn't satisfy the task.


## Elixir


```elixir
defmodule Division do
  def by_zero?(x,y) do
    try do
      _ = x / y
      false
    rescue
      ArithmeticError -> true
    end
  end
end

[{2, 3}, {3, 0}, {0, 5}, {0, 0}, {2.0, 3.0}, {3.0, 0.0}, {0.0, 5.0}, {0.0, 0.0}]
|> Enum.each(fn {x,y} ->
  IO.puts "#{x} / #{y}\tdivision by zero  #{Division.by_zero?(x,y)}"
end)
```


{{out}}

```txt

2 / 3   division by zero  false
3 / 0   division by zero  true
0 / 5   division by zero  false
0 / 0   division by zero  true
2.0 / 3.0       division by zero  false
3.0 / 0.0       division by zero  true
0.0 / 5.0       division by zero  false
0.0 / 0.0       division by zero  true

```



## Emacs Lisp

Division by zero gives an error of type <code>arith-error</code> which can be caught in the usual ways with <code>condition-case</code> and similar.  A division by zero example can be found in the Elisp manual section [http://www.gnu.org/s/emacs/manual/html_node/elisp/Handling-Errors.html "Handling Errors"].


```Lisp
(condition-case nil
    (/ 1 0)
  (arith-error
   (message "Divide by zero (either integer or float)")))
```



## Erlang


```erlang
div_check(X,Y) ->
    case catch X/Y of
        {'EXIT',_} -> true;
        _ -> false
    end.
```



## ERRE


```ERRE

PROGRAM DIV_BY_ZERO

EXCEPTION
   IF ERR=11 THEN PRINT("Division by Zero") END IF
END EXCEPTION

BEGIN
  PRINT(0/3)
  PRINT(3/0)
END PROGRAM

```

EXCEPTION (when it's present) detects runtime errors, otherwise program stops with a
[Runtime error #nn] where nn is the error code. Error codes are different between C-64 and PC version.
{{out}}

```txt

 0
 Division by zero

```


=={{header|F_Sharp|F#}}==

```fsharp
let detectDivideZero (x : int) (y : int):int option =
    try
        Some(x / y)
    with
        | :? System.ArithmeticException -> None


printfn "12 divided by 3 is %A" (detectDivideZero 12 3)
printfn "1 divided by 0 is %A" (detectDivideZero 1 0)
```


Output:

```txt
12 divided by 3 is Some 4
1 divided by 0 is null
```



## Factor


```factor
USE: math.floats.env

: try-div ( a b -- )
    '[ { +fp-zero-divide+ } [ _ _ /f . ] with-fp-traps ] try ;
```


 ( scratchpad ) 1 2 try-div
 0.5
 ( scratchpad ) 1 0 try-div
 Floating point trap

 Type :help for debugging help.


## Fancy


```fancy
def divide: x by: y {
  try {
    x / y
  } catch DivisionByZeroError => e {
    e message println # prints error message
  }
}

```



## Forth


```forth
: safe-/ ( x y -- x/y )
  ['] / catch -55 = if cr ." divide by zero!" 2drop 0 then ;
```



## Fortran

Fortran has only floating-point exception handling. Integer exceptions are missing in ISO standard. Gfortran detects some integer explicit exceptions during compilation and is able to generate some run-time checks for integer overflow (with -ftrapv).  Intel ifort does not have integer overflow / division by zero detection.

Floating-point division by zero detection.


```fortran

program  rosetta_divbyzero
   implicit none
   integer, parameter :: rdp = kind(1.d0)
   real(rdp) :: normal,zero

   normal = 1.d0
   zero = 0.d0

   call div_by_zero_check(normal,zero)

 contains

   subroutine  div_by_zero_check(x,y)
      use, intrinsic  :: ieee_exceptions
      use, intrinsic  :: ieee_arithmetic
      implicit none
      real(rdp), intent(in) :: x,y

      real(rdp) :: check
      type(ieee_status_type) :: status_value
      logical :: flag
      flag = .false.
      ! Get the flags
      call ieee_get_status(status_value)
      ! Set the flags quiet
      call ieee_set_flag(ieee_divide_by_zero,.false.)
      write(*,*)"Inf supported? ",ieee_support_inf(check)

      ! Calculation involving exception handling
      check = x/y
      write(*,*)"Is check finite?",ieee_is_finite(check), check

      call ieee_get_flag(ieee_divide_by_zero, flag)
      if (flag) write(*,*)"Warning!  Division by zero detected"

      ! Restore the flags
      call ieee_set_status(status_value)

   end subroutine div_by_zero_check

end program rosetta_divbyzero

```


Integer division by zero. No detection.


```fortran

program    rosetta_integer_divbyzero
   implicit none
   integer :: normal,zero,answer
   normal = 1
   zero = 0
   answer = normal/ zero
   write(*,*) answer
end program rosetta_integer_divbyzero

```



## FreeBASIC

In FreeBASIC integer division by zero is a fatal error and cannot be caught by the language's built-in error handling constructs.

However, it is possible to detect such an error by using floating point division instead and relying on the fact that when Infinity, -Infinity and NaN are converted back to a 4 or 8 byte signed integer, the result is the lower bound of the range of the relevant integer type.

For Win64, an Integer is a signed 8 byte type and the returned value is therefore -9223372036854775808 which would be unlikely to arise in any other integer division scenario.

The following code relies on this 'hack':-


```freebasic
' FB 1.05.0 Win64

Const divByZeroResult As Integer = -9223372036854775808

Sub CheckForDivByZero(result As Integer)
  If result = divByZeroResult Then
    Print "Division by Zero"
  Else
    Print "Division by Non-Zero"
  End If
End Sub

Dim As Integer x, y

x = 0 : y = 0
CheckForDivByZero(x/y) ' automatic conversion to type of parameter which is Integer
x = 1
CheckForDivByZero(x/y)
x = -1
CheckForDivByZero(x/y)
y = 1
CheckForDivByZero(x/y)
Print
Print "Press any key to exit"
Sleep
```


{{out}}

```txt

Division by Zero
Division by Zero
Division by Zero
Division by Non-Zero

```



## FutureBasic

Stop on error. Error type reported in log console.

```futurebasic

include "ConsoleWindow"

on error stop
dim as long a
print a / 0

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=6c837b97d9c5f296ef23245706544bdf Click this link to run this code]'''

```gambas
Public Sub Main()

Try Print 1 / 0
If Error Then Print Error.Text

End
```

Output:

```txt

Division by zero

```



## Go

Detection on integers by recovering from a panic:

```go
package main

import "fmt"

func divCheck(x, y int) (q int, ok bool) {
    defer func() {
        recover()
    }()
    q = x / y
    return q, true
}

func main() {
    fmt.Println(divCheck(3, 2))
    fmt.Println(divCheck(3, 0))
}
```

Output:

```txt

1 true
0 false

```



## Groovy

In Groovy, the float and double types follow IEEE numeric formats and rules. Here is a solution for double:

```groovy
def dividesByZero = { double n, double d ->
    assert ! n.infinite : 'Algorithm fails if the numerator is already infinite.'
    (n/d).infinite || (n/d).naN
}
```


Test program:

```groovy
((3d)..(0d)).each { i ->
    ((2d)..(0d)).each { j ->
        println "${i}/${j} divides by zero? " + dividesByZero(i,j)
    }
}
```


Output:
<pre style="height:15ex;overflow:scroll">3.0/2.0 divides by zero? false
3.0/1.0 divides by zero? false
3.0/0.0 divides by zero? true
2.0/2.0 divides by zero? false
2.0/1.0 divides by zero? false
2.0/0.0 divides by zero? true
1.0/2.0 divides by zero? false
1.0/1.0 divides by zero? false
1.0/0.0 divides by zero? true
0.0/2.0 divides by zero? false
0.0/1.0 divides by zero? false
0.0/0.0 divides by zero? true
```



## Haskell


```haskell
import qualified Control.Exception as C
check x y = C.catch (x `div` y `seq` return False)
                    (\_ -> return True)
```



## hexiscript


```hexiscript
let a 1
let b 0
if tostr (a / (b + 0.)) = "inf"
  println "Divide by Zero"
else
  println a / b
endif
```



## HicEst


```hicest
FUNCTION zero_divide(num, denom)
    XEQ( num// "/" // denom,  *99) ! on error jump to label 99
    zero_divide = 0                ! division OK
    RETURN

 99 zero_divide = 1
END
```


```hicest
zero_divide(0, 1)         returns 0 (false)
zero_divide( 1, 3-2-1 )   returns 1 (true)
```



## HolyC

HolyC throws <code>Except:DivZero</code>.

```holyc
try {
  Print("%d\n", 10 / 0);
} catch {
  Print("Divide by zero");
}
```



## i


```i
//Division by zero is defined in 'i' so the result can be checked to determine division by zero.
concept IsDivisionByZero(a, b) {
	c = a/b
	if c = 0 and a - 0 or a = 0 and c > 0
		print( a, "/", b, " is a division by zero.")
		return
	end
	print( a, "/", b, " is not division by zero.")
}

software {
	IsDivisionByZero(5, 0)
	IsDivisionByZero(5, 2)
	IsDivisionByZero(0, 0)
}
```



## IDL



```idl
if not finite( <i>expression</i> ) then ...
```


=={{header|Icon}} and {{header|Unicon}}==
Setting &error to a non-zero number traps errors and converts then to failures.  Division by zero generates error 201

```Icon
procedure main()
&error := 1
udef := 1 / 0 | stop("Run-time error ", &errornumber, " : ", &errortext," in line #",&line," - converted to failure")
end
```


Sample Output:
```txt
Run-time error 201 : division by zero in line #3 - converted to failure
```



## J

Generally, this task should be accomplished in J using 0=DEN. Here we take an approach that's more comparable with the other examples on this page.

Divide by zero is not an error in J. It results in infinity which is represented by an underscore ( <code>_</code> ) or negative infinity (represented by a double underescore) or complex values which can have infinities for the real and/or imaginary part., except that [http://www.jsoftware.com/papers/eem/0div0.htm  0 divided by 0 is defined to have the result zero] (mathematically speaking any number is a valid result for 0 divided by 0, because 0 times any number is zero).

See also the [http://www.jsoftware.com/help/dictionary/d030.htm J Dictionary page on infinity]

So, anyways, the task:


```J
funnydiv=: 0 { [: (,:'division by zero detected')"_^:(_ e. |@,) (,>:)@:(,:^:(0<#@$))@[ %"_1 _ ]
```


This performs division and instead of returning the result returns the string 'division by zero detected' if a denominator was zero.  Note that it also provides this result if a numerator was infinite, regardless of the denominator, but since there's no reasonable use for this implementation that's probably not a problem.

Examples:


```J
   3 funnydiv 2
1.5
   3 funnydiv 0
division by zero detected
   0 funnydiv 0
division by zero detected
   0 funnydiv 3
0
   2 3 4 funnydiv 5
0.4 0.6 0.8
```



## Java

Two ways to accomplish this task are presented here. They each return true if there is a division by zero or if <tt>Double.POSITIVE_INFINITY</tt> is used as a numerator.

One way to do this check in Java is to use the <tt>isInfinite</tt> function from the <tt>Double</tt> class:

```java
public static boolean infinity(double numer, double denom){
	return Double.isInfinite(numer/denom);
}
```

Another way is to use the <tt>ArithmeticException</tt> as a check (which is not preferred because it expects an exception):

```java
public static boolean except(double numer, double denom){
	try{
		int dummy = (int)numer / (int)denom;//ArithmeticException is only thrown from integer math
		return false;
	}catch(ArithmeticException e){return true;}
}
```



## JavaScript

JavaScript does not give an error on division by 0, and this is more useful than it is Mathematically correct.  However, 0 divided by 0 will yield NaN, which is actually correct, since 0/0 is defined as "indeterminate".  It may be better to return 0 or false in these situations, though, depending on the application  (in JavaScript, 0 and false are the same thing):

```JavaScript
function divByZero(dividend,divisor)
{
	var quotient=dividend/divisor;
        if(isNaN(quotient)) return 0; //Can be changed to whatever is desired by the programmer to be 0, false, or Infinity
        return quotient; //Will return Infinity or -Infinity in cases of, for example, 5/0 or -7/0 respectively
}
alert(divByZero(0,0));
```

This will output "0" instead of "NaN". In this case, when checking against for true, the condition needs to be explicit ("===" rather than "==") because if divByZero(5,5) is used, this will return 1, which is the same as true when using "==".


## jq

jq 1.4, like JavaScript, does not raise an error on division by 0, but unlike JavaScript, the result of division by zero is a number: either -1.7976931348623157e+308 or 1.7976931348623157e+308.

We can however define div(x;y) so that it raises an error, "NaN", if y equals 0:

```jq
def div(x;y): if y==0 then error("NaN") else x/y end;
```

In versions of jq since 1.4, we can then catch the error, as illustrated by the following snippet:
```jq

try div(3;0) catch if "NaN" then "div by 0 error detected" else . end
```



## Jsish

Like other ECMAScript implementations, Jsi does not error out on divide by zero.  There is the internal representation of +Infinity, -Infinity and NaN.  Detection of division by zero is not exact, other problems with the arithmetic can also set the state, but:


```javascript
if (!isFinite(numerator/denominator)) puts("result is infinity or not a number");
```



## Julia

Julia handles division by zero quite gracefully.  The result depends upon the numerator:  <code>Inf</code>, <code>-Inf</code>, <code>NaN</code> or (for complex numbers) some mixture of these.  This solution detects division by zero by checking for these sorts of values.

```julia
isdefinite(n::Number) = !isnan(n) && !isinf(n)

for n in (1, 1//1, 1.0, 1im, 0)
    d = n / 0
    println("Dividing $n by 0 ", isdefinite(d) ? "results in $d." : "yields an indefinite value ($d).")
end
```


{{out}}

```txt
Divding 1 by 0 yields an indefinite value (Inf).
Divding 1//1 by 0 yields an indefinite value (1//0).
Divding 1.0 by 0 yields an indefinite value (Inf).
Divding 0 + 1im by 0 yields an indefinite value (NaN + Inf*im).
Divding 0 by 0 yields an indefinite value (NaN).
```



## Kotlin


```scala
// version 1.1

fun divideByZero(x: Int, y:Int): Boolean =
    try {
        x / y
        false
    } catch(e: ArithmeticException) {
        true
    }

fun main(args: Array<String>) {
    val x = 1
    val y = 0
    if (divideByZero(x, y)) {
        println("Attempted to divide by zero")
    } else {
        @Suppress("DIVISION_BY_ZERO")
        println("$x / $y = ${x / y}")
    }
}
```


{{out}}

```txt

Attempted to divide by zero

```



## LabVIEW

{{VI solution|LabVIEW_Detect_division_by_zero.png}}

If the division node receives zero on both nodes (0/0), the Result will be "NaN"


## Lasso


```Lasso
define dividehandler(a,b) => {
	(
		#a->isNotA(::integer) && #a->isNotA(::decimal) ||
		#b->isNotA(::integer) && #b->isNotA(::decimal)
	) ? return 'Error: Please supply all params as integers or decimals'
	protect => {
		handle_error => { return 'Error: Divide by zero' }
		local(x = #a / #b)
		return #x
	}
}

dividehandler(1,0)
```


{{out}}

```txt
Error: Divide by zero
```



## Lingo


```lingo
on div (a, b)
  -- for simplicity type check of vars omitted
  res = value("float(a)/b")
  if voidP(res) then
    _player.alert("Division by zero!")
  else
    return res
  end if
end
```



## Lua

Lua, like Javascript, does not error on DIVIDE-BY-ZERO, but returns infinity. So:


```lua
function div(a,b)
  quot = a/b
  if quot == 1/0 then error() end
  return quot
end
```



## M2000 Interpreter

To place a division as argument for lazy evaluation we have to use lazy$() which make a proper anonymous function. So we get a() as a function in DetectDivisionByZero() and try to execute. So if we get the specific error we get true.

Lazy$() not only make a function but also pass the same scope to that function where we use it. So Variables A, B, Z which they are in scope in module Checkit, and not in Function DetectDivisionByZero(), they used by the lazy evaluation contraction. References in M2000 passed as weak references, and for functions passed as code in a string (for objects passed the weak reference of the object plus the code).

```M2000 Interpreter

Print function("{Read x : =x**2}", 2)=4

```


For a fast way to check a valid expression we can use Valid()

```M2000 Interpreter

Print Valid(100/0)=False

```



```M2000 Interpreter

Module Checkit {
      Function DetectDivisionByZero(&a()) {
            Try {
                  a=a()
            }
            =Error$=" division by zero"
      }

      Print DetectDivisionByZero(lazy$(10/0))=True
      Z=10
      A=4
      B=0
      Print DetectDivisionByZero(lazy$(Z/B))=True
      Print DetectDivisionByZero(lazy$(Z/A))=False
}
Checkit

```



## M4


```M4
ifelse(eval(2/0),`',`detected divide by zero or some other error of some kind')
```


Output, with standard output labeled "==>" and error output labeled "error==>":

```txt

error==>divideby0.m4:1: m4: Divide by zero in eval: 2/0
==>detected divide by zero or some other error of some kind

```



## Maple

By default numeric exceptions raise errors which cannot be trapped by the usual <code>try...catch</code> mechanism. Instead numeric exceptions may be controlled by custom handling procedures.

```Maple>1/0; # Here is the default behavior.</lang

Output:

```txt
Error, numeric exception: division by zero
```

Here is a simple custom handler being installed and used.

```Maple
NumericEventHandler( ':-division_by_zero'
                     = proc() infinity; end proc ):

1/0;

NumericStatus(':-division_by_zero'); # We may check the status flag
```

Output:

```txt
                                  infinity

                                    true
```

Alternatively, the custom handler could issue a warning or clear the status flag for that exception, as well as return some particular value.

```Maple
NumericEventHandler( ':-division_by_zero'
                     = proc()
                         WARNING("division by zero");
                         NumericStatus(':-division_by_zero'=false):
                         infinity;
                       end proc ):

1/0;

NumericStatus(':-division_by_zero');
```

Output:

```txt
Warning, division by zero
                                  infinity

                                    false
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Check[2/0, Print["division by 0"], Power::infy]
```



## MATLAB


```Matlab
function [isDividedByZero] = dividebyzero(numerator, denomenator)
   isDividedByZero = isinf( numerator/denomenator );
   % If isDividedByZero equals 1, divide by zero occured.
```



## Maxima


```maxima
f(a, b) := block([q: errcatch(a / b)], if emptyp(q) then 'error else q[1]);

f(5, 6);
5 / 6

f(5, 0;)
'error
```



## MAXScript


```maxscript
if not bit.isFinite (<i>expression</i>) then...
```



## MUMPS


```MUMPS
DIV(A,B) ;Divide A by B, and watch for division by zero
 ;The ANSI error code for division by zero is "M9".
 ;$ECODE errors are surrounded by commas when set.
 NEW $ETRAP
 SET $ETRAP="GOTO DIVFIX^ROSETTA"
 SET D=(A/B)
 SET $ETRAP=""
 QUIT D
DIVFIX
 IF $FIND($ECODE,",M9,")>1 WRITE !,"Error: Division by zero" SET $ECODE="" QUIT ""
 QUIT "" ; Fall through for other errors
```

Output:
```txt

USER>W $$DIV^ROSETTA(1,2)
.5
USER>W $$DIV^ROSETTA(1,4)
.25
USER>W $$DIV^ROSETTA(1,0)

Error: Division by zero
USER>W $$DIV^ROSETTA(1,C)

W $$DIV^ROSETTA(1,C)
^
<UNDEFINED> *C

```



## min

{{works with|min|0.19.3}}
The following operator will detect division by zero since the result will be infinity.

```min
(/ inf ==) :div-zero?
```

Integer divison (that is, <code>div</code> and not <code>/</code>) by zero will cause min to exit with an uncatchable arithmetic error.


## mIRC Scripting Language


```mirc
var %n = $rand(0,1)
if ($calc(1/ %n) == $calc((1/ %n)+1)) {
  echo -ag Divides By Zero
}
else {
  echo -ag Does Not Divide By Zero
}
```



## NetLogo


```netlogo
;; Division by zero detection using CAREFULLY
;; The CAREFULLY clause exists in NetLogo since version 2.0
;;   In prior versions of NetLogo, you must examine the divisor prior to performing the division.
;;   The variables result, a, and b must all be previously created global, local, or agent -own'd variables.
;; NetLogo variables are dynamically typed, so we are assuming that a and b contain numbers.
;; (All numbers in NetLogo are double-precision floating-point numbers.)
;;   However, even if not numbers, the result is still the same: the carefully clause will
;; supress the run-time error and run the "commands if error" block, setting result to false.
;; this false value can be detected, to alter the rest of the course of the code
;;   This behavior is consistent with other NetLogo primitives, such as POSTIION, that report
;; FALSE, rather than a number, if the operation fails.
carefully
[ ;; commands to try to run
  set result a / b
]
[ ;; commands to run if an error occurs in the previous block.
  set result false
]
ifelse is-number? result
[ output-print (word a " / " b " = " result)
]
[ output-print (word a " / " b " is not calculable"
]
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

method divide(dividend, divisor) public constant returns Rexx
  do
    quotient = dividend / divisor
  catch exu = DivideException
    exu.printStackTrace()
    quotient = 'undefined'
  catch exr = RuntimeException
    exr.printStackTrace()
    quotient = 'error'
  end
  return quotient

method main(args = String[]) public static
  -- process input arguments and set sensible defaults
  arg = Rexx(args)
  parse arg dividend .',' divisor .
  if dividend.length() = 0 then dividend = 1
  if divisor.length()  = 0 then divisor  = 0
  say dividend '/' divisor '=' divide(dividend, divisor)
  return

```

Output:

```txt
netrexx.lang.DivideException: Divide by 0
	at netrexx.lang.Rexx.dodivide(Rexx.nrx:1778)
	at netrexx.lang.Rexx.OpDiv(Rexx.nrx:1674)
	at zz.divide(zz.nrx:20)
	at zz.main(zz.nrx:47)
```



## NewLISP


```newlisp
#! /usr/local/bin/newlisp

(define (check-division x y)
    (catch (/ x y) 'check-zero)
    (if (not (integer? check-zero))
        (setq check-zero "Division by zero."))
     check-zero
)

(println (check-division 10 4))
(println (check-division 4 0))
(println (check-division 20 5))
(println (check-division 11 0))

(exit)
```


Output:

```txt

2
Division by zero.
4
Division by zero.

```



## Nim


```nim
# In debug builds division by zero exceptions are thrown by default, in release
# builds not. We can still enable them explicitly.
{.push overflowChecks: on.}
proc divCheck(x, y): bool =
  try:
    discard x div y
  except DivByZeroError:
    return true
  return false
{.pop.} # Restore default check settings

echo divCheck(2, 0)
```



## OCaml

Detection on integers by catching an exception:

```ocaml
let div_check x y =
  try
    ignore (x / y);
    false
  with Division_by_zero ->
    true
```


Detection on floats by checking for infiniteness:

```ocaml
let div_check x y =
  classify_float (x /. y) = FP_infinite
```



## Oforth



```Oforth
: divideCheck(n)
| e |
   try: e [ 128 n / ] when: [ "Zero detected..." . ]
   "Leaving" println ;
```



## Octave


Dividing by zero raises a warning (a warning does not stop the execution), not an error (and the given answer is ''Inf''inity), so it's not possible to use a try-catch construct; we can however check for the lastwarn if the answer is Inf.


```octave
d = 5/0;
if ( isinf(d) )
  if ( index(lastwarn(), "division by zero") > 0 )
     error("division by zero")
  endif
endif
```



## Ol

Division by inexact zero produces Infinity (`+inf.0` and `-inf.0`) values, but division by exact zero (like `(/ n 0)`) - produces runtime error!


```scheme

(define (safediv a b)
   (if (eq? (type b) type-complex)
      (/ a b) ; complex can't be 0
      (let ((z (/ 1 (inexact b))))
         (unless (or (equal? z +inf.0) (equal? z -inf.0))
            (/ a b)))))

; testing:
(for-each (lambda (x)
      (if x (print x) (print "division by zero detected")))
   (list
      (safediv 1 5)    ; => 1/5
      (safediv 2 0)    ; => division by zero detected
      (safediv 3 1+2i) ; => 3/5-6/5i
      (safediv 4 0+i)  ; => 0-4i
      (safediv 5 7/5)  ; => 25/7
))

```



## ooRexx


```ooRexx
/* REXX **************************************************************
* program demonstrates  detects and handles  division by zero.
* translated from REXX:
*   removed fancy error reporting (ooRexx does not support linesize)
*   removed label Novalue (as novalue is not enabled there)
* 28.04.2013 Walter Pachl
*********************************************************************/
Signal on Syntax                   /*handle all REXX syntax errors. */
x = sourceline()                   /*being cute, x=size of this pgm.*/
y = x-x                            /*setting to zero the obtuse way.*/
z = x/y                            /* attempt to divide by 0        */
exit                               /* will not be reached           */

Syntax:
  Say 'Syntax raised in line' sigl
  Say sourceline(sigl)
  Say 'rc='rc '('errortext(rc)')'
  Exit 12
```

Output:

```txt
Syntax raised in line 11
z = x/y                            /* attempt to divide by 0        */
rc=42 (Arithmetic overflow/underflow)
```



## Oz

For integer division only.

```oz
try
   {Show 42 div 0}
catch error(kernel(div0 ...) ...) then
   {System.showInfo "Division by zero detected."}
end
```



## PARI/GP

Pari/GP version 2.7 introduces <code>iferr()</code>.  The given <code>err</code> variable is lexically bound in the recovery code and in the optional predicate (what to trap, default all errors).  Error type <code>e_INV</code> is division by zero.


```parigp
iferr(1/0,
      err,
      print("division by 0"); print("or other non-invertible divisor"),
      errname(err) == "e_INV");
```


Or the previous <code>trap()</code>,


```parigp
trap(,"division by 0",m/n)
```



## Pascal

See [[Detect_division_by_zero#Delphi | Delphi]]


## Perl

This function returns true iff its second argument is zero.

```perl
sub div_check
 {local $@;
  eval {$_[0] / $_[1]};
  $@ and $@ =~ /division by zero/;}
```



## Perl 6


### =Try/Catch=


```perl6
sub div($a, $b) {
    my $r;
    try {
        $r = $a / $b;
        CATCH {
            default { note "Unexpected exception, $_" }
        }
    }
    return $r // Nil;
}
say div(10,2);
say div(1, sin(0));
```

{{out}}

```txt
5
Unexpected exception, Attempt to divide 1 by zero using /
Nil
```



### =Multi Method Dispatch=


```perl6
multi div($a, $b) { return $a / $b }
multi div($a, $b where { $b == 0 }) { note 'Attempt to divide by zero.'; return Nil }

say div(10, 2);
say div(1, sin(0));
```

{{out}}

```txt
5
Attempt to divide by zero.
Nil
```



## Phix


```Phix
try
    integer i = 1/0
catch e
    ?e[E_USER]
end try
puts(1,"still running...\n")
```

{{out}}

```txt

"attempt to divide by 0"
still running...

```



## PHP

This function returns true iff its second argument is zero.

```php
function div_check($x, $y) {
  @trigger_error(''); // a dummy to detect when error didn't occur
  @($x / $y);
  $e = error_get_last();
  return $e['message'] != '';
}
```



```php
function div_check($x, $y) {
  return @($x / $y) === FALSE; // works at least in PHP/5.2.6-3ubuntu4.5
}
```



## PicoLisp


```PicoLisp
(catch '("Div/0") (/ A B))
```



## PL/I


```pli
Proc DivideDZ(a,b) Returns(Float Bin(33));
    Dcl (a,b,c) Float Bin(33);
    On ZeroDivide GoTo MyError;
    c=a/b;
    Return(c);
MyError:
    Put Skip List('Divide by Zero Detected!');
End DivideDZ;

xx=DivideDZ(1,0);
```



## PL/SQL


```PLSQL
FUNCTION divide(n1 IN NUMBER, n2 IN NUMBER)
RETURN BOOLEAN
IS
  result NUMBER;
BEGIN
  result := n1/n2;
  RETURN(FALSE);
EXCEPTION
  WHEN ZERO_DIVIDE THEN
    RETURN(true);
end divide;
```



```PL/SQL
divide(0,1) --false
divide(1,0) --true, division by zero
```



## PowerShell


```PowerShell

function div ($a, $b) {
    try{$a/$b}
    catch{"Bad parameters: `$a = $a and `$b = $b"}
}
div 10 2
div 1 0

```

<b>Output:</b>

```txt

5
Bad parameters: $a = 1 and $b = 0

```



## Pure

Floating point division yields inf or nan values as appropriate (if the FPU supports IEEE 754):


```pure>
 1/0, -1/0, 0/0;
inf,-inf,nan
```


It's possible to check for these values as follows:


```pure>
 inf_or_nan x = infp x || nanp x;
> map inf_or_nan [1/0, -1/0, 0/0];
[1,1,1]
```


In contrast, integer division by zero raises an exception which can be caught as follows:


```pure>
 divide n m = catch (\_ -> "divide by 0") (n div m);
> divide 0 1;
0
> divide 1 0;
"divide by 0"
```



## Python


```python
def div_check(x, y):
  try:
    x / y
  except ZeroDivisionError:
    return True
  else:
    return False
```



## Q

Division by zero does not raise an error, instead it results in an infinity (<tt>0w</tt> or <tt>-0w</tt>) or NaN (<tt>0n</tt>).


```q
r:x%0
?[1=sum r=(0n;0w;-0w);"division by zero detected";()]
```



## R

Division by zero does not raise an error nor a warning. Division of a non-zero value by zero returns infinity. Division of zero by zero returns NaN; Whether the result is not finite can be checked:

```rsplus
d <- 5/0
if ( !is.finite(d) ) {
  # it is Inf, -Inf, or NaN
}
```



## Racket


In Racket, the division by zero exception can be caught directly:


```racket

#lang racket

(with-handlers ([exn:fail:contract:divide-by-zero?
                 (λ (e) (displayln "Divided by zero"))])
  (/ 1 0))

```



## REBOL


```REBOL
REBOL [
    Title: "Detect Divide by Zero"
    URL: http://rosettacode.org/wiki/Divide_by_Zero_Detection
]

; The 'try' word returns an error object if the operation fails for
; whatever reason. The 'error?' word detects an error object and
; 'disarm' keeps it from triggering so I can analyze it to print the
; appropriate message. Otherwise, any reference to the error object
; will stop the program.

div-check: func [
	"Attempt to divide two numbers, report result or errors as needed."
	x y
	/local result
] [
	either error? result: try [x / y][
		result: disarm result
		print ["Caught" result/type "error:" result/id]
	] [
		print [x "/" y "=" result]
	]
]

div-check 12 2       ; An ordinary calculation.
div-check 6 0        ; This will detect divide by zero.
div-check "7" 0.0001 ; Other errors can be caught as well.
```


Output:


```txt
12 / 2 = 6
Caught math error: zero-divide
Caught script error: cannot-use
```



## REXX

The task's requirements are to write a ''function'', but this example program was written to solve the spirit of the requirement.

This version isn't really a function so much as it is a method.

Also, a ''function'' and a ''subroutine'' doesn't have that much of a distinction in the REXX language.

```rexx
/*REXX program  demonstrates  detection  and handling  division by zero.                */
signal on syntax                                 /*handle all REXX syntax errors.       */
x = sourceline()                                 /*being cute, x=is the size of this pgm*/
y = x - x                                        /*setting to zero the obtuse way.      */
z = x / y                                        /*this'll trigger it,  furrrr shurrre. */
exit                                             /*We're kaput.   Ja vohl !             */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:    if rc==42  then do;  say                 /*first,  check for a specific error.  */
                             say center(' ***error*** ', 79, "═")
                             say 'Division by zero detected at line  '       @ ,
                                 "  and the REXX statement is:"
                             say sourceLine(@)
                             say
                             exit 42
                        end
        say
        say center(' error! ', 79, "*")
                        do #=1  for arg();   say;     say arg(#);       say
                        end   /*#*/
        exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
syntax: @=sigl;   call err  'REXX program'   condition("C")   'error',   condition('D'), ,
                            'REXX source statement (line'   sigl"):",    sourceLine(sigl)
```

{{out|output|text=}}

```txt

═════════════════════════════════ ***error*** ═════════════════════════════════
Division by zero detected at line   5   and the REXX statement is:
z = x / y                                        /*this'll trigger it,  furrrr shurrre. */

```



## Ring


```ring

Try
   see 9/0
Catch
   see "Catch!" + nl + cCatchError
Done

```



## RPGIV


```rpgiv

       dcl-c DIVIDE_BY_ZERO 00102;

       dcl-s result zoned(5:2);
       dcl-s value1 zoned(5:2);
       dcl-s value2 zoned(5:2);

       value1 = 10;
       value2 = 0;

       monitor;
         eval(h) result = value1 / value2; // Using half rounding here for the eval result
       on-error DIVIDE_BY_ZERO;
         // Initialise the result to 0. Consider other messaging perhaps.
         result = 0;
       endmon;

       *inlr = *on;

```





## Ruby

This only checks integer division by zero.


```ruby
def div_check(x, y)
  begin
    x / y
  rescue ZeroDivisionError
    true
  else
    false
  end
end
```


Ruby allows division by zero if either operand is a Float.


```ruby
irb(main):010:0> div_check(5, 0)
=> true
irb(main):011:0> div_check(5.0, 0)
=> false
```


----

Starting with Ruby 1.9, Numeric#div raises ZeroDivisionError, whether or not an operand is a Float.

{{works with|Ruby|1.9}}


```ruby
def div_check(x, y)
  begin
    x.div y
  rescue ZeroDivisionError
    true
  else
    false
  end
end
```



```ruby
irb(main):010:0> div_check(5, 0)
=> true
irb(main):011:0> div_check(5.0, 0)
=> true
```



## Rust


```rust
fn test_division(numerator: u32, denominator: u32) {
    match numerator.checked_div(denominator) {
        Some(result) => println!("{} / {} = {}", numerator, denominator, result),
        None => println!("{} / {} results in a division by zero", numerator, denominator)
    }
}

fn main() {
    test_division(5, 4);
    test_division(4, 0);
}
```



## Scala

Without the "println(result)" line, the result would not get calculated
as it is not needed. The method would get optimized to
always return false.

```scala
object DivideByZero extends Application {

  def check(x: Int, y: Int): Boolean = {
    try {
      val result = x / y
      println(result)
      return false
    } catch {
      case x: ArithmeticException => {
        return true
      }
    }
  }

  println("divided by zero = " + check(1, 0))

  def check1(x: Int, y: Int): Boolean = {
    import scala.util.Try
    Try(y/x).isFailure
  }
  println("divided by zero = " + check1(1, 0))

}
```



## Seed7

Integer division by zero raises NUMERIC_ERROR.
Floating point division by zero returns [http://seed7.sourceforge.net/libraries/float.htm#Infinity Infinity] or -Infinity.

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const proc: doDivide (in integer: numer, in integer: denom) is func
  begin
    block
      writeln(numer <& " div " <& denom <& " = " <& numer div denom);
    exception
      catch NUMERIC_ERROR:
        writeln("Division by zero detected.");
    end block;
  end func;

const proc: doDivide (in float: numer, in float: denom) is func
  local
    var float: quotient is 0.0;
  begin
    quotient := numer / denom;
    if quotient <> Infinity and quotient <> -Infinity then
      writeln(numer <& " / " <& denom <& " = " <& quotient);
    else
      writeln("Division by zero detected.");
    end if;
  end func;

const proc: main is func
  begin
    doDivide(10, 8);
    doDivide(1, 0);
    doDivide(10.0, 8.0);
    doDivide(1.0, 0.0);
  end func;
```


Output:

```txt

10 div 8 = 1
Division by zero detected.
10.0 / 8.0 = 1.25
Division by zero detected.

```



## Sidef

The numerical system of Sidef evaluates `x/0` to `+/-Inf`.

```ruby
func div_check(a, b){
    var result = a/b
    result.abs == Inf ? nil : result
}
 
say div_check(10, 2)  # 5
say div_check(1, 0)   # nil (detected)
```


Alternatively, we can do:


```ruby
func div_check(a, b){
    Perl.eval("#{a} / #{b}")
}
 
say div_check(10, 2)  # 5
say div_check(1, 0)   # nil (detected)
```



## Slate



```slate
[ 1 / 0 ] on: Error do: [|:err| err return: PositiveInfinity].
```




## Smalltalk

{{works with|Squeak}}
{{works with|Smalltalk/X}}

```smalltalk
zeroDivide := [:aBlock |
	[aBlock value. false] on: ZeroDivide do: [true].
	].

"Testing"
zeroDivide value: [2/1] "------> false"
zeroDivide value: [2/0] "------> true"
```


of course, as ZeroDivide inherits from Error, you could also write [...] on: Error do: [...], thereby catching ANY error (as done in some other code examples here).


## SNOBOL4


{{works with|Macro Spitbol}}

Using setexit( ) to trap and ignore division by zero.


```SNOBOL4
        define('zdiv(x,y)') :(zdiv_end)
zdiv    &errlimit = 1; setexit(.ztrap)
        zdiv = x / y :(return)
ztrap   zdiv = ?(&errtype ? (14 | 262)) 'Division by zero' :s(continue)f(abort)
zdiv_end

*       # Test and display
        output = '1/1     = ' zdiv(1,1)      ;* Integers non-zero
        output = '1.0/1.0 = ' zdiv(1.0,1.0)  ;* Reals non-zero
        output = '1/0     = ' zdiv(1,0)      ;* Integers zero
        output = '1.0/0.0 = ' zdiv(1.0,0.0)  ;* Reals zero
        output = 'Zero checks complete'
end
```


Output:

```txt
1/1     = 1
1.0/1.0 = 1.
1/0     = Division by zero
1.0/0.0 = Division by zero
Zero checks complete
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON@

CREATE OR REPLACE FUNCTION DIVISION(
  IN NUMERATOR DECIMAL(5, 3),
  IN DENOMINATOR DECIMAL(5, 3)
 ) RETURNS SMALLINT
 BEGIN
  DECLARE RET SMALLINT DEFAULT 1;
  DECLARE TMP DECIMAL(5, 3);
  DECLARE CONTINUE HANDLER FOR SQLSTATE '22012'
    SET RET = 1;

  SET RET = 0;
  SET TMP = NUMERATOR / DENOMINATOR;
  RETURN RET;
 END @

VALUES DIVISION(10, 2)@
VALUES DIVISION(10, 3)@
VALUES DIVISION(10, 0)@

```

Output:

```txt

db2 -td@
db2 => CREATE OR REPLACE FUNCTION DIVISION(
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

VALUES DIVISION(10, 2)

1
------
     0

  1 record(s) selected.


VALUES DIVISION(10, 3)

1
------
     0

  1 record(s) selected.


VALUES DIVISION(10, 0)

1
------
     1

  1 record(s) selected.

```



## Standard ML

Detection on integers by catching an exception:

```sml
fun div_check (x, y) = (
  ignore (x div y);
  false
) handle Div => true
```


Detection on floats by checking for infiniteness:

```sml
fun div_check (x, y) =
  not (Real.isFinite (x / y))
```



## Stata

In stata, a division by zero is silently replaced with a missing value. It would be possible to check whether the result is a missing value, but there may be another cause: one of the arguments is a missing value, or there is an overflow (for instance 1e200/1e-200). Therefore, it's not possible to detect precisely a division by zero, without checking the denominator.


## Tcl


```tcl
proc div_check {x y} {
    if {[catch {expr {$x/$y}} result] == 0} {
        puts "valid division: $x/$y=$result"
    } else {
        if {$result eq "divide by zero"} {
            puts "caught division by zero: $x/$y -> $result"
        } else {
            puts "caught another error: $x/$y -> $result"
        }
    }
}

foreach denom {1 0 foo} {
    div_check 42 $denom
}
```

{{out}}

```txt
valid division: 42/1=42
caught division by zero: 42/0 -> divide by zero
caught another error: 42/foo -> can't use non-numeric string as operand of "/"
```

{{works with|Tcl|8.6}}

It is easier to trap such errors in Tcl 8.6, which has an additional control structure for exception processing:

```tcl
proc div_check {x y} {
    try {
        puts "valid division: $x/$y=[expr {$x/$y}]"
    } trap {ARITH DIVZERO} msg {
        puts "caught division by zero: $x/$y -> $msg"
    } trap {ARITH DOMAIN} msg {
        puts "caught bad division: $x/$y -> $msg"
    } on error msg {
        puts "caught another error: $x/$y -> $msg"
    }
}

foreach {num denom} {42 1  42 0  42.0 0.0  0 0  0.0 0.0  0 foo} {
    div_check $num $denom
}
```

which produces the {{out}}

```txt
valid division: 42/1=42
caught division by zero: 42/0 -> divide by zero
valid division: 42.0/0.0=Inf
caught division by zero: 0/0 -> divide by zero
caught bad division: 0.0/0.0 -> domain error: argument not in valid range
caught another error: 0/foo -> can't use non-numeric string as operand of "/"
```

As can be seen, division-by-zero is only signaled when performing integer division. Similarly, separate detection of values that would otherwise be IEEE NaN is only performed when doing floating-point division.


## TXR



```txr
@(do (defun div-check (x y)
       (catch (/ x y)
         (numeric_error (msg)
           'div-check-failed))))
@(bind good @(div-check 32 8))
@(bind bad @(div-check 42 0))
```


Run:


```txt
$ txr -B division-by-zero.txr
good="4.0"
bad="div-check-failed"
```



## Ursa

{{trans|Python}}

```ursa
def div_check (int x, int y)
	try
		/ x y
		return false
	catch divzeroerror
		return true
	end try
end
```


## VAX Assembly


```VAX Assembly
65 64 69 76 69 64 00000008'010E0000' 0000     1 desc:	.ascid	"divide by zero"
            6F 72 65 7A 20 79 62 20  000E
                               0000  0016     2 .entry	handler,0
                         E5 AF   7F  0018     3 	pushaq	desc
              00000000'GF   01   FB  001B     4 	calls	#1, g^lib$put_output
                                 04  0022     5 	ret
                                     0023     6
                               0000  0023     7 .entry	main,0
                    6D   EE AF   9E  0025     8 	movab	handler, (fp)	;register exception handler
                  50   01   00   C7  0029     9 	divl3	#0, #1, r0
                                 04  002D    10 	ret
                                     002E    11
                                     002E    12 .end	main
$ run dv
divide by zero

```



## VBA


```vb

Option Explicit

Sub Main()
Dim Div
    If CatchDivideByZero(152, 0, Div) Then Debug.Print Div Else Debug.Print "Error"
    If CatchDivideByZero(152, 10, Div) Then Debug.Print Div Else Debug.Print "Error"
End Sub

Function CatchDivideByZero(Num, Den, Div) As Boolean
    On Error Resume Next
    Div = Num / Den
    If Err = 0 Then CatchDivideByZero = True
    On Error GoTo 0
End Function
```

{{Out}}

```txt
Error
15,2
```



## VBScript


```vb

Function div(num,den)
	On Error Resume Next
	n = num/den
	If Err.Number <> 0 Then
		div = Err.Description & " is not allowed."
	Else
		div = n
	End If
End Function

WScript.StdOut.WriteLine div(6,3)
WScript.StdOut.WriteLine div(6,0)
WScript.StdOut.WriteLine div(7,-4)

```


{{Out}}

```txt

2
Division by zero is not allowed.
-1.75

```



## Visual Basic .NET

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Module DivByZeroDetection

    Sub Main()
        Console.WriteLine(safeDivision(10, 0))
    End Sub

    Private Function safeDivision(v1 As Integer, v2 As Integer) As Boolean
        Try
            Dim answer = v1 / v2
            Return False
        Catch ex As Exception
            Return True
        End Try
    End Function
End Module

```

{{out}}

```txt

True

```




## XPL0

GetErr is an intrinsic function that detects runtime errors, such as
division by zero, as well as more than a dozen others. Normally runtime
errors terminate a program and display an error message. However the Trap
intrinsic is used here to prevent error termination, which enables the
program to do its own error handling. This example displays "Divide by
zero".


```XPL0
include c:\cxpl\codes;
int A, B;
[Trap(false);           \turn off error trapping
B:= 1234/(A-A);         \(error not detected at compile time)
if GetErr then Text(0, "Divide by zero");
]
```



## Yorick



```yorick
func div_check(x, y) {
    if(catch(0x01))
        return 1;
    temp = x/y;
    return 0;
}
```



## zkl


```zkl
fcn f(x,y){try{x/y}catch(MathError){println(__exception)}}
```

{{out}}

```txt

zkl: f(1,0)
MathError(INF (number is infinite), Int divide by zero)
zkl: f(1.0,0)
MathError(INF (number is infinite))

```


{{omit from|ACL2}}
{{omit from|AWK|Division by zero is always a fatal error.}}
{{omit from|bc|Division by zero causes a warning message, but the program cannot detect this.}}
{{omit from|CMake|math(EXPR q "1/0") raises SIGFPE; CMake crashes and dumps core.}}
{{omit from|dc|Division by zero causes a warning message, but the program cannot detect this.}}
{{omit from|Retro|Divide by Zero is handled by the VM and is not exposed to the language}}
{{omit from|sed|No division.}}
{{omit from|Swift|Division by zero is always a fatal error}}
