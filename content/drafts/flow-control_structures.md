+++
title = "Flow-control structures"
description = ""
date = 2019-05-02T19:23:27Z
aliases = []
[extra]
id = 1788
[taxonomies]
categories = []
tags = []
+++

{{Task|Control Structures}}
{{Control Structures}}

;Task:
Document common flow-control structures.

One common example of a flow-control structure is the   <big><code> goto </code></big>   construct.

Note that   [[Conditional Structures]]   and   [[Iteration|Loop Structures]]   have their own articles/categories.


;Related tasks:
*   [[Conditional Structures]]
*   [[Iteration|Loop Structures]]





## 360 Assembly

Common 360 opcodes for flow-control structures:
===Unconditional Branch (B)===
To perform a 'goto'.

```360asm

         B      TESTPX     goto label TESTPX
         BR     14         goto to the address found in register 14

```

===Branch and Link (BAL)===
To perform a 'call' to a subroutine. The first register at execution time is the next sequential address to allow a 'return'.

```360asm

         LA     15,SINUSX  load in reg15 address of function SINUSX
         BALR   14,15      call the subroutine SINUX and place address RETPNT in reg14
RETPNT   EQU    *

SINUSX   EQU    *          subroutine SINUSX
         ...
         BR     14         return to caller

```


===Conditional Branch (BC)===
Fistly a compare instruction set the condition code (cc), secondly a conditional branch is performed.

```360asm

         L      4,A        Load A in register 4
         C      4,B        Compare A with B
         BH     TESTGT     Branch on High        if A>B  then goto TESTGT
         BL     TESTLT     Branch on Low         if A<B  then goto TESTLT
         BE     TESTEQ     Branch on Equal       if A=B  then goto TESTEQ
         BNH    TESTLE     Branch on Not High    if A<=B then goto TESTLE
         BNL    TESTGE     Branch on Not Low     if A>=B then goto TESTGE
         BNE    TESTNE     Branch on Not Equal   if A<>B then goto TESTNE

```

===Branch on Count (BCT)===
To perform unconditional loops.

```360asm

         LA     3,8                r3 loop counter
LOOP     EQU    *
         ...                       loop 8 times  (r3=8,7,...,2,1)
         BCT    3,LOOP             r3=r3-1 ; if r3<>0 then loop

```

===Branch on Index (BX.)===
BXLE to perform loops in old Fortran style with 3 registers.

```360asm

*        do i=1 to 8 by 2
         L      3,1                r3 index and start value 1
         LA     4,2                r4 step 2
         L      5,8                r5 to value 8
LOOPI    EQU    *
         ...                       loop 4 times (r3=1,3,5,7)
         BXLE   3,4,LOOPI          r3=r3+r4; if r3<=r5 then loop

```

BXH to perform backward loops with 3 registers.

```360asm

*        do i=8 to 1 by -2
         L      3,1                r3 index and start value 8
         LH     4,=H'-2'           r4 step -2
         L      5,8                r5 to value 1
LOOPI    EQU    *
         ...                       loop 4 times (r3=8,6,4,2)
         BXH    3,4,LOOPI          r3=r3+r4; if r3>r5 then loop

```




## 6502 Assembly



### JMP

The jump instruction immediately jumps to any address:

```6502asm
		JMP $8000		;immediately JuMP to $8000 and begin executing
					;instructions there.
```

The indirect jump instruction immediately jumps to the address contained in the address:

```6502asm
		JMP ($8000)		;immediately JuMP to the address in memory locations
					;$8000 and $8001
```



### JSR

The jump to subroutine instruction pushes the address of the next instruction minus one onto the stack and jumps to any address:

```6502asm
		JSR $8000	;Jump to SubRoutine
```

A return from subroutine instruction pops the return address off the stack, adds one, and jumps to that location:

```6502asm
		RTS		;ReTurn from Subroutine
```



### BRK

A break instruction causes a non-maskable interrupt (setting the interrupt flag), pushes the current program counter address plus one onto the stack, pushes the flags onto the stack, then jumps to the address in the break vector (commonly at $FFFE and $FFFF):

```6502asm
		BRK		;BReaK
```

The return from interrupt instruction pops the flags off the stack, pops the return address off the stack, adds one, and jumps to that location:

```6502asm
		RTI		;ReTurn from Interrupt
```



## Ada



### goto


```ada><<Top>

   Put_Line("Hello, World");
   goto Top;
```



### exit

Exit is used to break out of loops. Exit can be used with a label to break out of an inner loop to an outer loop and its enclosing outer loop

```ada
Outer:
   loop
      -- do something
      loop
         -- do something else
         exit Outer; -- exits both the inner and outer loops
      end loop;
   end loop;
```


### asynchronous transfer of control

A sequence of operation can be aborted with an asynchronous transfer of control to an alternative:

```ada
select
   delay 10.0;
   Put_Line ("Cannot finish this in 10s");
then abort
   -- do some lengthy calculation
   ...
end select;
```

The alternative can be a delay statement or else an entry point call followed by a sequence of operations. The statement blocks at the delay or entry call and executes the sequence of the operation introduced by '''then abort'''. If blocking is lifted before completion of the sequence, the sequence is aborted and the control is transferred there.


## ALGOL 68

{{works with|ALGOL 68|Standard - except the Refinement Preprocessor is an extension}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}} <!-- with version's of a68g up to mk18 the GSL library has problems with VEC and MAT with LWB 0 , and transposes of non square MAT, hence we include source code of replacement OPerators here -->
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput}} -->

See also [[Exceptions#ALGOL_68|Exceptions]] to see how '''ALGOL 68''' handles ''transput'' events.
===One common use of a label in '''ALGOL 68''' is to break out of nested loops.===

```algol68
(
   FOR j TO 1000 DO
     FOR i TO j-1 DO
       IF random > 0.999 THEN
         printf(($"Exited when: i="g(0)", j="g(0)l$,i,j));
         done
       FI
       # etc. #
     OD
   OD;
 done: EMPTY
);
```

===Multi way jump using labels and '''EXIT''' to return result ===

```algol68
STRING medal = (
  []PROC VOID award = (gold,silver,bronze);

  award[ 1 + ENTIER (random*3)];

  gold: "Gold" EXIT
  silver: "Silver" EXIT
  bronze: "Bronze"

);

print(("Medal awarded: ",medal, new line));
```


### Another use is to implement finite state machines


```algol68
STRING final state = (
  INT condition;
  PROC do something = VOID: condition := 1 + ENTIER (3 * random);

  state1:
     do something;
     CASE condition IN
        state 1, state 2
     OUT
        state n
     ESAC
  EXIT

  state 2:
     "State Two"
  EXIT

  state n:
     "State N"
);
print(("Final state: ",final state, new line));
```


### ALGOL 68G implements a Refinement Preprocessor to aid with top down code development


```algol68
# example from: http://www.xs4all.nl/~jmvdveer/algol.html - GPL #
determine first generation;
WHILE can represent next generation
DO calculate next generation;
   print next generation
OD.

determine first generation:
   INT previous := 1, current := 3.

can represent next generation:
   current <= max int - previous.

calculate next generation:
   INT new = current + previous;
   previous := current;
   current := new.

print next generation:
   printf (($lz","3z","3z","2z-d$, current,
            $xz","3z","3z","2z-d$, previous,
            $xd.n(real width - 1)d$, current / previous)).
```

{{out}}

```txt

Exited when: i=13, j=53
Medal awarded: Gold
Final state: State Two

             4              3 1.33333333333333
             7              4 1.75000000000000
            11              7 1.57142857142857
etc...

```



## ALGOL W

As well as structured flow-control structures (loops, if-then-else, etc.) Algol W has a goto statement. A goto can lead out of the current procedure, which can be used for error handling. Goto can be written as "goto" or "go to".

```algolw
begin
    integer i;
    integer procedure getNumber ;
    begin
        integer n;
        write( "n> " );
        read( i );
        if i< 0 then goto negativeNumber;
        i
    end getNumber ;

    i := getNumber;
    write( "positive or zero" );
    go to endProgram;
negativeNumber:
    writeon( "negative" );
endProgram:
end.
```



## AutoHotkey


```AutoHotkey
MsgBox, calling Label1
Gosub, Label1
MsgBox, Label1 subroutine finished
Goto Label2
MsgBox, calling Label2 ; this part is never reached
Return

Label1:
  MsgBox, Label1
Return

Label2:
  MsgBox, Label2 will not return to calling routine
Return
```



## AWK


The [awk] programming language is data driven. However, Awk has ''break'' and ''continue'' for loop control, as in C.


```awk
$ awk 'BEGIN{for(i=1;;i++){if(i%2)continue; if(i>=10)break; print i}}'
2
4
6
8
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
BBC BASIC has '''GOSUB''' and '''GOTO''' but they are deprecated.

```bbcbasic
      GOSUB subroutine

      (loop)
      PRINT "Infinite loop"
      GOTO loop
      END

      (subroutine)
      PRINT "In subroutine"
      WAIT 100
      RETURN
```



## Bracmat

In Bracmat, the thing that comes closest to a GOTO construct is evaluation of a variable that contains some code, ending with an evaluation of the same variable. Due to tail recursion optimization this can run forever. Example:

```bracmat
  ( LOOP
  =   out$"Hi again!"
    & !LOOP
  )
& out$Hi!
& !LOOP
```

{{out}}

```txt
Hi!
Hi again!
Hi again!
Hi again!
...

```



## C


### goto

One common use of goto in C is to break out of nested loops.

```c
int main()
{
  int i,j;
  for (j=1; j<1000; j++) {
    for (i=0; i<j, i++) {
      if (exit_early())
        goto out;
      /* etc. */
    }
  }
out:
  return 0;
}
```



## C++


###  goto

{{works with|GCC|3.3.4}}

```cpp
#include <iostream>

int main()
{
 LOOP:
  std::cout << "Hello, World!\n";
 goto LOOP;
}
```


Note that "goto" may also be used in conjunction with other forms of branching.


###  Exceptions

{{works with|GCC|4.0.2}}

Exceptions are a way to give control back to a direct or indirect caller in case of an error. Note that throwing exceptions is usually very expensive, therefore they generally should only be used for exceptional situations.

```cpp
#include <iostream>
#include <ostream>

void foo()
{
  std::cout << "Going to throw an exception.\n";
  throw 7; // almost any object can be thrown, including ints
  std::throw << "This output will never execute.\n";
}

void bar()
{
  std::cout << "Going to call foo().\n";
  foo();
  std::cout << "This will be skipped by the exception coming from foo.\n";
}

void baz()
{
  try // everything thrown from inside the following code block
  {   // will be covered by the following catch clauses
    std::cout << "Going to call bar().\n";
    bar();
    std::cout << "This will be skipped by the exception coming from foo.\n";
  }
  catch(...) // a simple catch-all, but doesn't give access to the thrown exception
  {
    std::cout << "An exception occured. I'll just throw it on.\n";
    throw; // without an argument, the caught exception is re-thrown
  }
  std::cout << "This will not be executed due to the re-throw in the catch block\n";
}

void foobar()
{
  try
  {
    baz();
  }
  catch(char const* s)
  {
    std::cout << "If foo had thrown a char const*, this code would be executed.\n";
    std::cout << "In that case, the thrown char const* would read " << s << ".\n";
  }
  catch(int i)
  {
    std::cout << "Caught an int, with value " << i << " (should be 7).\n";
    std::cout << "Not rethrowing the int.\n";
  }
  catch(...)
  {
    std::cout << "This catch-all doesn't get invoked because the catch(int) above\n"
              << "already took care of the exception (even if it had rethrown the\n"
              << "exception, this catch-all would not be invoked, because it's\n"
              << "only invoked for exceptions coming from the try block.\n";
  }
  std::cout << "This will be executed, since the exception was handled above, and not rethrown.\n";
}

int main()
{
  try
  {
    foobar();
  }
  catch(...)
  {
    std::cout << "The main function never sees the exception, because it's completely handled\n"
              << "inside foobar(). Thus this catch-all block never gets invoked.\n";
  }
}
```



## C#


### return

terminates the function and returns control to the caller.

```c#
int GetNumber() {
    return 5;
}
```


### throw

throws (or rethrows) an exception. Control is transferred to the nearest catch block capable of catching the exception.<br/>
A <code>finally</code> block is always executed before control leaves the <code>try</code> block.

```c#
try {
    if (someCondition) {
        throw new Exception();
    }
} catch (Exception ex) {
    LogException(ex);
    throw;
} finally {
    cleanUp();
}
```


### yield return and yield break

In a generator method, <code>yield return</code> causes the method to return elements one at a time. To make this work, the compiler creates a state machine behind the scenes. <code>yield break</code> terminates the iteration.

```c#
public static void Main() {
    foreach (int n in Numbers(i => i >= 2) {
        Console.WriteLine("Got " + n);
    }
}

IEnumerable<int> Numbers(Func<int, bool> predicate) {
    for (int i = 0; ; i++) {
        if (predicate(i)) yield break;
        Console.WriteLine("Yielding " + i);
        yield return i;
    }
}

```

{{out}}

```txt

Yielding 0
Got 0
Yielding 1
Got 1
```


### await

is used to wait for an asynchronous operation (usually a Task) to complete. If the operation is already completed when <code>await</code> is encountered, the method will simply continue to execute. If the operation is not completed yet, the method will be suspended. A continuation will be set up to execute the rest of the method at a later time. Then, control will be returned to the caller.

```c#
async Task DoStuffAsync() {
    DoSomething();
    await someOtherTask;//returns control to caller if someOtherTask is not yet finished.
    DoSomethingElse();
}

```


### break and continue

<code>continue</code> causes the closest enclosing loop to skip the current iteration and start the next iteration immediately.<br/>
<code>break</code> terminates the closest enclosing loop or <code>switch</code> statement. Control is passed to the statement that follows the terminated statement.

### <code>goto</code>

<code>goto Label;</code> will cause control to jump to the statement with the corresponding label. This can be a <code>case</code> label inside a <code>switch</code>.<br/>
Because the label must be in scope, <code>goto</code> cannot jump inside of a loop.

```c#
while (conditionA) {
    for (int i = 0; i < 10; i++) {
        if (conditionB) goto NextSection;
        DoSomething(i);
    }
}
NextSection: DoOtherStuff();
```



## COBOL


###  CALL

This transfers control to a subprogram, with control eventually being returned (provided the subprogram does not terminate the program).


###  CHAIN

This transfers control to the subprogram specified with ''no'' return of control. <code>CHAIN</code> is a non-standard extension
created by Micro Focus and is found in [[Visual COBOL]].


###  EXIT

<code>EXIT</code> takes a variety of clauses:
* <code>PARAGRAPH</code>/<code>PERFORM</code>/<code>SECTION</code>: Control will be transferred immediately past the end of those blocks.
* <code>PERFORM CYCLE</code>: The current iteration of the <code>PERFORM</code> will stop, and control will go to the clauses of the <code>PERFORM</code> which will determine if another iteration is to run.
* <code>PROGRAM</code>: If used in a called function, execution of it is stopped and control returned to the calling program. If it is used in the main program, the statement is non-functional and ignored.
The <code>PERFORM</code>/<code>PERFORM CYCLE</code> clauses can only be used inside of an inline <code>PERFORM</code> statement.


###  GOBACK

If used in a called function, control will transferred back to the calling program. If it is used in the main program, the program will be terminated.


###  GO TO

Basic use:

```cobol
       PROGRAM-ID. Go-To-Example.

       PROCEDURE DIVISION.
       Foo.
           DISPLAY "Just a reminder: GO TOs are evil."

           GO TO Foo
           .
```


A <code>GO TO</code> can take a <code>DEPENDING ON</code> clause which will cause program flow to go to a certain paragraph/section depending on a certain value.

```cobol
       GO TO First-Thing Second-Thing Third-Thing
           DEPENDING ON Thing-To-Do

*      *> Handle invalid thing...
```

The previous example is equivalent to:

```cobol
           EVALUATE Thing-To-Do
               WHEN 1
*                  *> Do first thing...

               WHEN 2
*                  *> Do second thing...

               WHEN 3
*                  *> Do third thing...

               WHEN OTHER
*                  *> Handle invalid thing...
           END-EVALUATE
```



###  ALTER

The much maligned altered GO.

''The ALTER statement is now obsolete and not even mentioned in the COBOL 2014 standard.''

If anyone wonders why this was ever designed into a language, you have to look back to the days of 8K RAM, multi-million dollar computers, and source code that was entered on punch cards, one line per card.  Think of physically recoding an entire deck of punch cards (say even 6,000 lines worth) versus ALTERing a few paragraphs, that jump to new code at the end of the deck, and you may see one small reason why this was ever built into early versions of COBOL.  Then ponder what the state of the code would be after three or four (or fifty) such patches, and then see why ALTER was deemed obsolete shortly after terminals and disk become the common way of entering programs.

Then pause to think about the fact that some COBOL code, written before ALTER went out of fashion, is still in production, to understand why it was implemented in GnuCOBOL, a relatively new COBOL dialect, still in development as of 2016 (along with other modern COBOL implementations, that support COBOL 2014, and yet continue to support language constructs that date all the way back to COBOL-60).

{{works with|GnuCOBOL}}

```COBOL

       identification division.
       program-id. altering.

       procedure division.
       main section.

      *> And now for some altering.
       contrived.
       ALTER story TO PROCEED TO beginning
       GO TO story
       .

      *> Jump to a part of the story
       story.
       GO.
       .

      *> the first part
       beginning.
       ALTER story TO PROCEED to middle
       DISPLAY "This is the start of a changing story"
       GO TO story
       .

      *> the middle bit
       middle.
       ALTER story TO PROCEED to ending
       DISPLAY "The story progresses"
       GO TO story
       .

      *> the climatic finish
       ending.
       DISPLAY "The story ends, happily ever after"
       .

      *> fall through to the exit
       exit program.

```

{{out}}

```txt

prompt$ cobc -xj altering.cob
This is the start of a changing story
The story progresses
The story ends, happily ever after

```



###  INVOKE

The <code>INVOKE</code> statement is used to transfer control to a method of a class/factory object.


###  PERFORM

The <code>PERFORM</code> statement can be used to transfer program flow to the specified sections/paragraphs in the subprogram, with control being returned when the end of the last paragraph/section or a relevant <code>EXIT</code> statement is reached.

```cobol
       PROGRAM-ID. Perform-Example.

       PROCEDURE DIVISION.
       Main.
           PERFORM Moo
           PERFORM Display-Stuff
           PERFORM Boo THRU Moo

           GOBACK
           .

       Display-Stuff SECTION.
       Foo.
           DISPLAY "Foo " WITH NO ADVANCING
           .

       Boo.
           DISPLAY "Boo " WITH NO ADVANCING
           .

       Moo.
           DISPLAY "Moo"
           .
```

{{out}}

```txt

Moo
Foo Boo Moo
Boo Moo

```



###  STOP RUN

This immediately terminates the program.


## Comal


### Call a procedure


```Comal
myprocedure
END // End of main program
PROC myprocedure
PRINT "Hello, this is a procedure"
ENDPROC myprocedure
```



### Exit a loop


```Comal
LOOP
PRINT "I'm in a loop!"
EXIT
ENDLOOP
PRINT "But i somehow got out of it."
```



### Conditional exit


```Comal
PRINT "I'm in a loop!"
LOOP
INPUT "Do you want to exit?":answer$
EXIT WHEN answer$="y"
ENDLOOP
PRINT "You got out of it."
```



### Goto


```Comal
PRINT "Hello world"
GOTO label
PRINT "This line will never be output"
label:
PRINT "This program will end thanks to the evil GOTO statement"
END
```



## D


###  goto


```d
import std.stdio;

void main() {
    label1:
    writeln("I'm in your infinite loop.");
    goto label1;
}
```



###  Exceptions

D supports the try/catch/finally mechanism:

```d
import std.stdio;

class DerivedException : Exception {
    this(string msg) { super(msg); }
}

void main(string[] args) {
    try {
        if (args[1] == "throw")
            throw new Exception("message");
    } catch (DerivedException ex) {
        // We never threw a DerivedException, so this
        // block is never called.
        writefln("caught derived exception %s", ex);
    } catch (Exception ex) {
        writefln("caught exception: %s", ex);
    } catch (Throwable ex) {
        writefln("caught throwable: %s", ex);
    } finally {
        writeln("finished (exception or none).");
    }
}
```



###  Scope guards

In a complex function, you might need to do cleanup in case of an exception, but it gets out of hand if there are many initialization steps that could fail. Scope guards offer a simplified syntax for try/finally.

There are three scopes you can listen for: exit, which is called unconditionally; failure, which is called if you leave the function via an exception; and success, which is called if you return from the function normally. A statement inside a scope block is only executed if execution reaches the scope block.

For instance:

```d
import std.stdio;

void main(string[] args) {
    scope(exit)
        writeln("Gone");

    if (args[1] == "throw")
        throw new Exception("message");

    scope(exit)
        writeln("Gone, but we passed the first" ~
                " chance to throw an exception.");
}
```


If the exception is thrown, then the only text that is written to the screen is "gone". If no exception is thrown, both calls to writeln occur.

scope(failure) and scope(success) work similarly.


## E


E does not have goto. The only primitive flow control construct which is not a loop, conditional, or exception is escape, or ejectors.

The basic syntax is

 escape ''ej'' {
   ''...body...''
 }

Within ''body'' variable ''ej'' then contains a one-argument function (an ejector) which, if called, immediately returns the provided value from the escape block.

This is a limited form of continuation (it cannot be used after the escape block exits).

Loop break, loop continue, and return-from-middle-of-function are all defined in terms of this basic construct.

[[Category:E examples needing attention]] <!-- Needs runnable examples, description of escape-catch, and maybe links to other flow control pages -->


## Erlang

The one Erlang flow control structure, apart from the ones excluded in the task description, is [[Exceptions]]


## Forth

===CATCH-THROW===
Some Forth implementations have goto, but not the standard. It does have an exception mechanism.

```forth
: checked-array
  CREATE ( size -- ) DUP , CELLS ALLOT
  DOES> ( i -- a+i )
    2DUP @ 0 SWAP WITHIN IF
      SWAP 1+ CELLS +
    ELSE
      1 THROW
    THEN ;

8 checked-array myarray

: safe-access ( i -- a[i] )
  ['] myarray CATCH 1 = IF ." Out of bounds!" 0 THEN ;
```



## Fortran

===The basic: GO TO ''label''===
Fortran offers <code>GO TO ''label''</code> where ''label'' is a number, an integer, which is prefixed to some executable statement according to the rules of Fortran source layout. It is not considered to be a numerical value, though zero is not an allowed label and leading zero digits are ignored. Fortran has no reserved words and gives no significance to spaces, so that <code>G O TO 12 3 4</code> is just as valid as <code>GO TO 1234</code> and other usages. Text labels are not allowed, however a statement such as <code>GO TO START</code> is possible. Even so, "START" is not itself a label, but the name of a variable which is used in special ways - see the ASSIGN statement usage, below.


### Elaborations on GO TO

A compiler may offer the "assigned GO TO" facility, with statements such as <code>ASSIGN 120 TO THENCE</code> scattered about: 120 is a statement label, not an integer, and any statement label may be assigned to variable THENCE (which is an integer variable) as execution proceeds. A relatively restrained usage would be to select the label of a suitable FORMAT statement to use in a READ or WRITE statement in place of a fixed label, without affecting the flow of control. But <code>GO TO THENCE</code> will cause a GO TO for the current address held in THENCE... Should you yield to temptations such as <code>THENCE = THENCE - 6</code> (treating it as an ordinary integer), a subsequent <code>GO TO THENCE</code> may end execution with an error message, or something else...

Aside from facilitating the production of spaghetti code, this sort of behaviour actually can be put to a positive use to handle the situation where in a large programme there may be portions that could be employed from a number of locations, and one does not wish to repeat that code each time - apart from the tedium of punching additional cards, each replication would demand its own unique set of statement labels. Further, such replication increases the total code size and memory is limited...
```Fortran
      ...
      ASSIGN 1101 to WHENCE   !Remember my return point.
      GO TO 1000              !Dive into a "subroutine"
 1101 CONTINUE                !Resume.
      ...
      ASSIGN 1102 to WHENCE   !Prepare for another invocation.
      GO TO 1000              !Like GOSUB in BASIC.
 1102 CONTINUE                !Carry on.
      ...
Common code, far away.
 1000 do something            !This has all the context available.
      GO TO WHENCE            !Return whence I came.
```

Since Algol in the 1960s it has been possible to define a routine within a larger routine that has access to all the context of the larger routine and so can be a convenient service routine for it, but Fortran does not allow a subroutine (or function) to be defined within a larger subroutine, except for the arithmetic statement function. One must write separate subroutines and struggle over providing access to context via COMMON and parameters. However, F90 introduced the MODULE arrangement whereby a collection of variables may all be referenced by a group of subroutines in the module without each having COMMON statements in common. Further, it allows a subroutine (or function) to use the CONTAINS feature, after which such a contained routine may be placed. Alas, it may not itself invoke CONTAINS even though Algol allows nesting as desired. And oddly, the contained routine must be at the ''end'' of the containing routine. So much for definition before usage. With such a facility, the possibility arises of perpetrating a GO TO from a contained routine to somewhere in its parent, however the F90 compilers are required to disallow access to outside labels, even those of FORMAT statements - rather a pity for that. Such escapes would have to copy whatever de-allocation steps were needed for a normal exit, which is simple enough on a stack-oriented design such as the B6700. However, its Algol compiler rejected attempts to jump from one routine ''into'' another (!) with the message "Bad GOTO. Too bad." Assembler programmers can do what they want, but for once, Fortran's designers show some restraint.

Once started on this path, many opportunities beckon: perhaps not just action "A" (achieved by "subroutine" 1000) is of use, there may be an action "B", and so on. One can then prepare the equivalent of a "to-do" list via something like
```Fortran
      ASSIGN 2000 TO WHENCE      !Deviant "return" from 1000 to invoke 2000.
      ASSIGN 1103 TO THENCE      !Desired return from 2000.
      GO TO 1000
 1103 CONTINUE
```

So that "subroutine" 1000 would be invoked, which then invokes subroutine 2000, which returns via THENCE. And, instead of using simple variables such as THENCE and WHENCE, one could use an array and treat it like a stack... Those familiar with LISP or FORTH and similar languages will recognise a struggle to create new "verbs" from existing verbs, and their resulting usage in compound expressions. This is Philip Greenspun's "tenth" rule of programming.

Another such usage would be to implement "coroutines", the classic example being to imagine a system that processes both Fortran statements and Fortran commentary, but each in their own way. After scanning some Fortran source, commentary is found so control flows to resume the commentary processing from where it had left off, then when further Fortran source is found, control flows back whence the Fortran source process left off. This is quite different from having subroutines FCODE and FCOMM which when invoked start at their start each time (as say when a new statement begins) rather than picking up where they left off because the switches occurred in the middle of a statement. Quite aside from questions of mutual recursion.

====If one is good, more are better?====
There is also a "computed GO TO" with syntax like <code>GO TO (101,50,75,50), ''n''</code> where ''n'' is an integer variable (or expression) that selects from the list of statement labels: in this example if its value is three, then the third label, 75, will be selected. If the value is less than one or greater than the number of labels in the list, odd behaviour is likely, differing by compiler. Possibly by continuing with the next statement, or ending execution with an error message, or making a leap into the void. This statement can be used to produce a tangle as described for the ASSIGN facility, but is commonly used as a central direction station, or "dispatch table" for instance when a programme accepts an input which is one of a number of commands and after identifying it from a list of recognised commands (such as "List", "Dump", LineFit", "Quit", ''etc.''), performs a computed GO TO to reach the portion that processes that command. Again, familiar to LISP programmers.


### Escape from mishap

An implicit GO TO can appear in READ and WRITE statements (and a few others), that will be taken should there be certain difficulties. Thus <code>READ (IN,6,END = 200, ERR = 300) STUFF</code> reads input from I/O unit IN (an integer value) into variable STUFF according to the FORMAT statement labelled 6. But should there be instead an end-of-file, rather than ending execution with an error code, execution will GO TO label 200, while if there should arise some difficulty with the format of the incoming data (two decimal points in one data field, ''etc.'') then execution will GO TO label 300. FORMAT statements, though labelled, are not considered suitable destinations for GO TO jumps.


### Deviant RETURN

Similar possibilities arise with alternate returns from subroutines and functions, for instance to handle error conditions it might wish to report as with the READ statement. Thus, <code>CALL FRED(THIS,*123,*THENCE)</code> invokes a subroutine FRED with three parameters: THIS, then two oddities. The leading * (or &) signifies that these are no ordinary integers (or expressions) but instead are the labels of statements somewhere within the calling routine. Subroutine FRED might return in the normal way so that execution continues with the following statement, or, it may instead return with a GO TO for one of the labels...
```Fortran
      SUBROUTINE FRED(X,*,*)     !With placeholders for unusual parameters.
        ...
        RETURN          !Normal return from FRED.
        ...
        RETURN 2        !Return to the second label.
      END
```

More delicate souls prefer to see an integer parameter whose value will be set by FRED according to the desired condition, and every call to FRED would be followed by a computed GO TO on that value. Except that this statement is also disapproved of, so one is encouraged to code IF, or CASE, ''etc.'' and enjoy the repetition.

Thus, a subroutine (or a function) may exit via a RETURN statement, rather than by completing its logic and "falling out" of the end of its definition. If the subprogram is large, these escape holes may be missed by the (human) reader!

Persons writing in assembler have further opportunities, for example providing an integer function such as IOR(A,B) that performs an '''or''' on integers A and B. Instead of doing so, the function overwrites its invocation by placing in-line code that performs A '''or''' B, then returns not to its return address but to where it was invoked from so as to compute the result.

====Away, and maybe, back====
Similarly to escaping from a subroutine, within a DO-loop, a GO TO might jump out of the loop(s) - perhaps for good reason. More interesting is the possibility of jumping ''into'' a DO-loop's scope, possibly after jumping out - who knows what its index variable might have been changed to. This is considered poor form by others not writing such code and some compilers will reject any attempts. With the F77 introduction of IF ... THEN ... ELSE ... END IF constructions, jumping out of a block is still acceptable but jumping in is frowned on (even if only from the THEN clause to some part of its ELSE clause) and may be prevented.

F90 offers a more decorous means for exiting DO-loops, including the additional DO WHILE loop, via the statements CYCLE and EXIT - the text "GO TO" does not appear as such, but the effect is the same. The CYCLE option means abandoning further statements within the block to test afresh the iteration condition, while EXIT means ending the iteration as if it had completed. Further syntax allows some compiler checking, as follows:
```Fortran
      XX:DO WHILE(condition)
           statements...
        NN:DO I = 1,N
             statements...
             IF (...) EXIT XX
             IF (...) CYCLE NN
             statements...
           END DO NN
         END DO XX
```

A DO-loop can be given a label such as XX (which is ''not'' in the numeric-only label area of fixed source format Fortran, and the syntax highlghter has missed yet another trick of Fortran syntax) and its corresponding END DO can be given a label also: the compiler checks that they match and some programmer errors might thereby be caught. With such labels in use, the CYCLE and EXIT statements can name the loop they are intended for, so that CYCLE NN steps to the next iteration for <code>I</code> (as if it were a GO TO the END DO having its label as a suffix) while the EXIT XX exits both the numeric DO-LOOP and the DO-WHILE loop - without such labels only the innermost loop is affected and one can lose track. These labels must not be the name of any other entity in the source, and specifically not the name of the variable of the DO-LOOP concerned. Thus, if there are many DO I = 1,N loops, each must have its own label. There is unfortunately no equivalent to <code>NEXT I</code> as in BASIC instead of <code>END DO</code>so as to be clear just which DO-LOOP is being ended and for which index variable.


### =Not returning at all=

In for example Fortran IV, as on the IBM1130, a <code>CALL EXIT</code> stopped the run, which is to say, it "exited" to the operating system. As distinct from STOP which stopped the flow by stopping the cpu. Another no-return was provided by the likes of <code>CALL LINK(PHASE2)</code> where PHASE2 was not a text string in quotes. This caused the flow of execution to abandon the current programme and the operating system would load and run a programme called PHASE2. Code was loaded from low memory upwards, while storage in COMMON was assigned from high memory downwards and so long as desired data were not damaged by the new load, its processing would continue. Thus, if some data required a great deal of complex analysis and there was insufficient memory available to hold all the data plus all the code, it might be possible to split the processing into PHASE1 and PHASE2, etc.


### Interruptions to the flow

More potent than RETURN is STOP, which ends the flow of execution there and then - without the need to signal some sort of "disaster" status so that each level of a nest of routines would return to its caller. Early Fortran also allowed STOP ''n'' where ''n'' was a number such as 303 and this value might be displayed on the computer's console display register in bright lights, or be printed in the job log, or on the standard output device. A later extension was STOP "message", but alas the message is only a fixed text, one can't devise a custom report such as "307 values! Limit is 300" as via <code>STOP N," values! Limit is ",NMAX</code>

Instead of STOP, there is PAUSE with the same syntax. The flow of execution would pause (that is, the entire computer would come to a stop in the days when one job only would run at a time), to be resumed with the next statement on the pressing of a button on the computer console for instance after the message "Attach output tape", or, ... the operator could via controls on the console inspect memory (on the decimal IBM1620, even floating-point numbers were easily parsed), modify memory, and cause execution to resume at some other (any other) chosen address...


### Flowcharts?

A classic aid to design and analysis is a flowchart diagram, and there exist systems which will read the source code of a programme and draw its flowchart (on a graph plotter, say) with the various pieces of code appearing in blocks linked by lines showing the flow. Considering all the above (and not forgetting the arithmetic-IF and IF ... THEN ... ELSE ... ENDIF statements, DO-loops, DO WHILE loops and the SELECT and WHERE ... ELSEWHERE statements), this is no simple challenge.


### Reaction

There is a famous letter by Edsger Dijkstra, titled ''Go To Statement Considered Harmful'', published in the March 1968 Communications of the ACM.

### =Response=

A thorough analysis of the problem appears in ''Structured Programming with'' '''go to''' ''Statements'' by Donald E. Knuth, December 1974. There's even mention of decision tables.


## Gambas

'''[https://gambas-playground.proko.eu/?gist=64e4e68b1c6ce73341d08ba2d9333c07 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

LOOPIT:

Print siCount;;
Inc siCount
If siCount > 100 Then Quit
Goto LoopIt

End
```

Output:

```txt

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77
78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100

```



## Go

Not covered here::
* Structures involving for, if, switch, continue, break, fallthrough, or panic.  As mentioned in the task description, these structures are covered in other tasks.
* Short-circuit operators.  These can be considered flow-control structures, but are also covered in other tasks.
* Flow-control functions in the standard library.  Many of these are important, but covering them seems beyond the scope of the task.

### Goto

Go has goto and labels.  The following is an infinite loop:

```go
func main() {
inf:
    goto inf
}
```

Gotos can jump forward or backward within a function but they have some restrictions.  They cannot jump into any block from outside the block, and they cannot cause any variable to come into scope.


### Function call

Function call works as it does in most languages, transferring execution to to the called function, and returning execution to the following statement upon return from the called function.

The return statement returns from a function.  Any function can use a return statement.  Functions with return values can only return with a return statement.  Functions without return values can return by “falling off” the end of the function.

The defer statement sets a function or method to be executed upon return from the enclosing function.  This is useful when a function has multiple returns.  The classic example is closing a file:

```go
import "os"

func processFile() {
    f, err := os.Open("file")
    if err != nil {
        // (probably do something with the error)
        return // no need to close file, it didn't open
    }
    defer f.Close() // file is open.  no matter what, close it on return
    var lucky bool
    // some processing
    if (lucky) {
        // f.Close() will get called here
        return
    }
    // more processing
    // f.Close() will get called here too
}
```



### Goroutines

Goroutines are Go’s take on lightweight threads.  A goroutine is started with a go statement, which looks like “go” preceding a function call.  When the go statement executes, a new goroutine is created, execution in the new goroutine starts with a call to the function named in the go statement, and execution in the calling goroutine continues concurrently.  (The main thread of execution is a goroutine itself.)

The following program prints a mix of 1’s and 0’s.

```go
package main

import "fmt"

func printOnes() {
    for {
        fmt.Println("1")
    }
}

func main() {
    go printOnes()
    for {
        fmt.Println("0")
    }
}
```


A goroutine terminates upon return from the function called in the go statement.  Unlike with a regular function call however, it cannot return a value--the calling goroutine has long continued and there is nothing waiting for a return value.

Goroutines may not be able to communicate by returning values, but they have other ways.  Principal is passing data through channels.  Channel operations affect execution when they yield the processor, allowing other goroutines to run, but this does not normally alter flow of execution.  The one exception is when channel operations are used in a select statement.  A simple use,

```go
func answer(phone1, phone2 chan int) {
    select {
    case <-phone1:
        // talk on phone one
    case <-phone2:
        // talk on phone two
    }
}
```

Syntax is strongly reminiscent of the switch statement, but rules for flow control are very different.  Select will block if no channel operation is possible.  If one is possible, it will execute that case.  If multiple operations are possible, it will pick one at random.

### Process initialization

A complete program must have exactly one function named main, which is called on program start up.  In addition, a program can have any number of functions named init which are called before main.  Package variables are initialized before init functions or main are called.

=={{header|GW-BASIC}}==

```qbasic
10 LET a=1
20 IF a=2 THEN PRINT "This is a conditional statement"
30 IF a=1 THEN GOTO 50: REM a conditional jump
40 PRINT "This statement will be skipped"
50 PRINT ("Hello" AND (1=2)): REM This does not print
100 PRINT "Endless loop"
110 GOTO 100:REM an unconditional jump
```



## Haskell


In the context of normal, functional-style code, there are no flow-control statements, because explicit flow control is imperative. A monad may offer flow control; what kinds are available depends on the monad. For example, the [http://haskell.org/haskellwiki/New_monads/MonadExit <code>ExitT</code> monad transformer] lets you use the <code>exitWith</code> function to jump out a block of statements at will.


```haskell
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exit

main = do
    runExitTMaybe $ do
        forM_ [1..5] $ \x -> do
            forM_ [1..5] $ \y -> do
                lift $ print (x, y)
                when (x == 3 && y == 2) $
                    exitWith ()
    putStrLn "Done."
```



## HicEst

[http://www.HicEst.com More on HicEst's ALARM function]

```hicest
1 GOTO 2 ! branch to label

2 READ(FIle=name, IOStat=ios, ERror=3) something ! on error branch to label 3

3 ALARM(delay, n) ! n=2...9 simulate F2 to F9 keys: call asynchronously "Alarm"-SUBROUTINES F2...F9 with a delay

4 ALARM(  1  ) ! lets HicEst wait at this statement for any keyboard or mouse event

5 SYSTEM(WAIT=1000) ! msec

6 XEQ('CALL my_subroutine', *7) ! executes command string, on error branch to label 7

7 y = EXP(1E100, *8) ! on error branch to label 8

8 y = LOG( 0 , *9)   ! on error branch to label 9

9 ALARM( 999 ) ! quit HicEst immediately
```


=={{header|Icon}} and {{header|Unicon}}==
'''Prelude about Goal-Directed Evaluation and Generators'''


Two of the key features of Icon and Unicon that affect program flow are [[Icon%2BUnicon/Intro#Goal-Directed_Evaluation_and_Generators|Goal Directed Evaluation and Generators]] and [[Icon%2BUnicon/Intro#Failure_is_an_Option|Expression Failure]].  Goal Direction uses Generators to produce multiple results as needed and Expression Success and Failure forces the selection of logic pathways within programs.

'''goto'''

Does not exist in the Icon or Unicon language.

'''next'''

Restarts the enclosing loop. The conditional on the loop is evaluated as normal.

'''break expr'''

Default value of ''expr'' is the null value ''&amp;null''. This operator breaks out of the enclosing loop, yielding the expression as the result of the loop. Normally loops yield a failure ie no result, so you can write code like this:

```icon

    if x := every i := 1 to *container do {     # * is the 'length' operator
        if container[i] ~== y then
            write("item ", i, " is not interesting")
        else
            break a
    } then
        write("found item ", x)
    else
        write("did not find an item")

```

The expression given to ''break'' can be another ''break'', which effectively lets you break out of two levels of loop. Finally, the expression given to break can be the ''next'' command; for example

```icon

    break break next

```

breaks out of two levels of loop and re-enters the top of the third-level enclosing loop.

'''return expr'''

Default value of expr is ''&amp;null''. Apart from the usual meaning of ''return'', if the ''expr'' value fails, then the procedure actually fails too, ie does not yield a value. See description of ''fail'' keyword. If the ''expr'' is capable of yielding more than one result, only the first result is asked for and used.

'''fail'''

Causes the the enclosing procedure to terminate without returning value. This is different from returning void or a null value that many other languages do when the code does not return an actual value. For example, in

```icon

    x := ftn()

```

The value of x will not be replaced if ftn() issues the ''fail'' command. If ftn fails, then Goal-Directed Evaluation will also fail the assignment, therefore x is not assigned a new value. If the flow of control through a procedure falls off the end, the procedure implicitly fails.

'''suspend expr'''

Default value of expr is ''&amp;null''. Any procedure containing the ''suspend'' command will yield a value to the calling code. However the procedure remains in a state of suspended animation ready to be reactivated if the calling code demands another result due to Goal Directed Evaluation. Note that this capability is built directly into the runtime rather than being an artifically constructed behaviour provided by Python or C#'s use of the 'yield' keyword. Every and all expressions may suspend or be involved in a suspending expression without any effort. Behaviourally much closer to Prolog which also supports backtracking as a core part of the language. If the ''expr'' is capable of yielding more than one result, then supend (if driven) will progressively yield all of those values.

A procedure can contain several uses of ''suspend'' and it's quite reasonable for the procedure to execute many of them in any chosen order.

'''stop(expr)'''

Terminate program with prejudice.

'''error trapping'''

The keyword &amp;error is normally zero, but if set to a positive value, this sets the number of fatal errors that are tolerated and converted to expression failure; the value of &amp;error is decremented if this happens. Therefore the now-common TRY-CATCH behaviour can be written as:

```icon

    &error := 1
    mayErrorOut()
    if &error == 1 then
        &error := 0     # clear the trap
    else {
        # deal with the fault
        handleError(&errornumber, &errortext, &errorvalue)   # keyword values containing facts about the failure
    }

```

Various idiomatic simplifications can be applied depending on your needs.

'''error throwing'''

Errors can be thrown using the function

```icon

    runerr(errnumber, errorvalue)    # choose an error number and supply the offending value

```



## IDL



### goto



```idl
test:
..some code here
goto, test
```


(This is almost never used)

===on_error===


```idl
on_error, test
```


(This resumes at the label <tt>test</tt> if an error is encountered)

===on_ioerror===


```idl
on_ioerror, test
```


(Same as <tt>on_error</tt>, but for EOFs and read-errors and such)


### break



```idl>break</lang


immediately terminates the innermost current loop (or <tt>if</tt> or <tt>case</tt> etc)


### continue



```idl>continue</lang


immediately starts the next iteration of the current innermost loop


## J


Control structures should usually [but not always] be avoided in J.  J's primitives already provide iteration and selection.

For example, here's an example of a program which loops over a sequence of integers, multiplying them by two (j's default prompt is 3 spaces, which makes line-at-a-time copy-and-paste simple, and the result here is displayed on the following line):

```j
   2 * 1 2 3
2 4 6
```


That said, J's control structures are documented at http://www.jsoftware.com/help/dictionary/ctrl.htm  So, if you want to perform this same operation using a while loop, or a goto, you can do so.  It's just... often not a good idea (but sometimes they are indispensable).


## Java

"goto" is a reserved keyword in Java; but you cannot use it. There are currently no goto statements.

Java does provide two other statements that provide flow control: <tt>break</tt> and <tt>continue</tt>.


### break

The <tt>break</tt> statement can be used to terminate a <tt>case</tt> clause in a <tt>switch</tt> statement and to terminate a <tt>for</tt>, <tt>while</tt> or <tt>do-while</tt> loop.  In loops, a <tt>break</tt> can be ''labeled'' or ''unlabeled''.


```Java
switch (xx) {
  case 1:
  case 2:
    /* 1 & 2 both come here... */
    ...
    break;
  case 4:
    /* 4 comes here... */
    ...
    break;
  case 5:
    /* 5 comes here... */
    ...
    break;
  default:
    /* everything else */
    break;
}

for (int i = 0; i < 10; ++i) {
  ...
  if (some_condition) { break; }
  ...
}

_Time_: do {
  for (int i = 0; i < 10; ++i) {
    ...
    if (some_condition) { break _Time_; /* terminate the do-while loop */}
    ...
    }
  ...
} while (thisCondition);
```



### continue

The <tt>continue</tt> statement skips the current iteration of a <tt>for</tt>, <tt>while</tt>, or <tt>do-while</tt> loop.  As with <tt>break</tt> the <tt>continue</tt> statement can be ''labeled'' or ''unlabeled'' to allow iterating a loop level other than the current one in nested loops.


```Java
while (condition) {
  ...
  if (someCondition) { continue; /* skip to beginning of this loop */ }
  ...
}

top: for (int 1 = 0; i < 10; ++i) {
  ...
  middle: for (int j = 0; j < 10; ++j) {
    ...
    bottom: for (int k = 0; k < 10; ++k) {
    ...
    if (top_condition) { continue top; /* restart outer loop */ }
    ...
    if (middle_condition) { continue middle; /* restart middle loop */ }
    ...
    if (bottom_condition) { continue bottom; /* restart bottom loop */ }
    ...
    if (bottom_condition) { continue; /* this will also restart bottom loop */ }
    ...
    }
    ...
  }
  ....
}
```



## JavaScript

* <code>'''return'''</code> from a function ([http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Statements/return])
* <code>'''yield'''</code> from a generator function ([https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/yield])
* <code>'''yield*'''</code> from a generator function ([https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/yield*])
* <code>'''await'''</code> from an async function ([https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await])
* loop control with <code>'''break''' [label]</code> ([http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Statements/break]) and <code>'''continue''' [label]</code> ([http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Statements/continue])
* exceptions with <code>'''throw'''</code> ([http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Statements/throw]) and <code>'''try ... catch ... finally ...'''</code>  ([http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Statements/try...catch])


## jq


jq 1.5 introduced `break` and `label` keywords for defining backtracking points.  These are used to terminate a generator before completion.  Here is a contrived example that illustrates the main points:

```txt
$ jq -n '1, (2 | label $foo | debug | 3 | break $foo | debug), 4'
1
["DEBUG:",2]
4
```


Here is an example from the standard library:

```jq
# Emit at most one item from the stream generated by g:
def first(g): label $out | g | ., break $out;
```



## Julia

Julia provides the @goto and @label macros for goto within functions. In addition, the "break" keyword is used for jumping out of a single loop, throw() of an exception can be used to jump out of a try() statement's code, and the assert() and exit() functions can be used to terminate a program.

```julia

function example()
    println("Hello ")
    @goto world
    println("Never printed")
    @label world
    println("world")
end

```



## Kotlin


Kotlin does not have a 'goto' statement but does have 'break' and 'continue' statements to jump out of or continue with the next iteration of a loop. The 'labelled' versions of these statements have already been described at [[Jump_anywhere#Kotlin]] and so only the basic versions are described in this task which jump out or continue with the nearest enclosing loop.

Kotlin also has a 'throw' statement which throws (or rethrows) an exception.

Here are some examples:

```scala
// version 1.0.6

fun main(args: Array<String>) {
    for (i in 0 .. 2) {
        for (j in 0 .. 2) {
            if (i + j == 2) continue
            if (i + j == 3) break
            println(i + j)
        }
    }
    println()
    if (args.isNotEmpty()) throw IllegalArgumentException("No command line arguments should be supplied")
    println("Goodbye!")  // won't be executed
}
```


{{out}}

```txt

c:\kotlin-compiler-1.0.6>java -jar control_flow.jar arg1 arg2
0
1
1

Exception in thread "main" java.lang.IllegalArgumentException: No command line arguments should be supplied
        at Control_throwKt.main(control_throw.kt:12)

```



## Lua

Lua has the <code>break</code>-command to exit loops.

```lua
i = 0
while true do
    i = i + 1
    if i > 10 then break end
end
```


### Tail calls as GOTOs

The following code - though obviously a useless infinite loop - will '''not''' cause a stack overflow:

```lua
function func1 ()
  return func2()
end

function func2 ()
  return func1()
end

func1()
```

This is because Lua supports proper tail recursion.  This means that because something being returned is necessarily the last action in a function, the interpreter treats a function call in this 'tail' position much like a GOTO in other languages and does not create a new stack level.

=={{header|Mathematica}} / {{header|Wolfram Language}}==
Relevant functions are:

TimeConstrained[expr,t] evaluates expr, stopping after t seconds.

MemoryConstrained[expr,b] evaluates expr, stopping if more than b bytes of memory are requested.

Goto[tag] scans for Label[tag], and transfers control to that point.

CreateScheduledTask[expr,t] creates a task that will repeatedly evaluate expr every t second.

Interrupt[]	interrupt a computation

Abort[]	abort a computation

Quit[] immediately stops all execution and returns to the top level read-eval-print loop

Catch[] prepares for receiving a  Throw[]  while running a given list of executable expressions

Throw[] causes a non-local jump to a specified Catch[]

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

try
   % do some stuff
catch
   % in case of error, continue here
end

```




## Maxima


```maxima
/* goto */
block(..., label, ..., go(label), ...);

/* throw, which is like trapping errors, and can do non-local jumps to return a value */
catch(..., throw(value), ...);

/* error trapping */
errcatch(..., error("Bad luck!"), ...);
```



## MUMPS


### GOTO / G

<p>The GOTO command jumps to a label. If the label is not in the current routine, it is necessary to include the circumflex and routine name.
```MUMPS
GOTO LABEL^ROUTINE
```
. This does not affect the subroutine stack, only the program pointer.</p>
```MUMPS>GOTO THERE</lang



### HALT / H

<p>Halt and Hang have the same abbreviation, i.e. "H" but (as a mnemonic) Halt takes no arguments. Halt stops the current process, and clears all Locks and devices in Use.
On the Cache variant of MUMPS, there is a $HALT special variable that can be set, the value of the $HALT special variable is a routine that is called before cleaning up (in effect, a specialized final error trap).</p>

```MUMPS
 Read "Do you really wish to halt (Y/N)?",Q#1
 IF Q="Y"!Q="y" HALT
```



### JOB / J

<p> The JOB command starts another MUMPS job starting at a label. If the label is not in the current routine, it is necessary to include the circumflex and routine name.
```MUMPS
JOB LABEL^ROUTINE
```
.
```MUMPS>JOB THERE</lang
 This does not affect the subroutine stack, nor the program pointer in the current job. Since MUMPS is a multi-processing (rather than multi-threading) language, the new job is independent of the current job.</p>

```MUMPS
 JOB LABEL^ROUTINE
```




### QUIT / Q

<p>Exits a loop, or routine. It decreases the stack level. It can return a value to a calling routine if there is a value after it.</p><p>Quit is one of the commands that requires two spaces after it if it is followed in a line by more commands.</p>

```MUMPS
FOR I=1:1:1 QUIT:NoLoop  DO YesLoop
QUIT Returnvalue
```


### XECUTE / X

<p>eXecute acts as if it were a one line Do command. Its argument must be a string of valid MUMPS code, and it performs that code in a new stack level. There is an implied Quit at the end of each eXecute's argument string.</p>
```MUMPS
 SET A="SET %=$INCREMENT(I)"
 SET I=0
 XECUTE A
 WRITE I
```

The above block will output "1".
<math>Insert formula here</math>


## Nemerle

Flow control statements made available in the '''Nemerle.Imperative''' namespace: '''break, continue, return''' (to return from somewhere other than the last expression in a function).

Exceptions can also be used to transfer control from a '''try''' block to a '''catch''' block.


## NetRexx

NetRexx doesn't have a <tt>GOTO</tt> instruction and unlike [[#REXX|Rexx]] the <tt>SIGNAL</tt> instruction is only used to throw exceptions.

Like [[#REXX|Rexx]] however, NetRexx provides the <tt>LEAVE</tt> and <tt>ITERATE</tt> instructions.


### LEAVE

The <tt>LEAVE</tt> instruction causes immediate exit from one or more <tt>DO</tt>, <tt>SELECT</tt> or <tt>LOOP</tt> constructs.


```NetRexx
loop xx = 1 to 10
  if xx = 1 then leave -- loop terminated by leave
  say 'unreachable'
  end
```


A ''<tt>name</tt>'' parameter can be provided to direct <tt>LEAVE</tt> to a specific end of block (as defined by a <tt>LABEL</tt> option or in the case of a controlled <tt>LOOP</tt> the control variable of the loop.


```NetRexx
loop xx = 1 to 10 -- xx is the control variable
  ...
  loop yy = 1 to 10 -- yy is the control variable
    ...
    if yy = 3 then leave xx -- xx loop terminated by leave
    if yy = 4 then leave yy -- yy loop terminated by leave
    ...
    end
    ...
  end xx

loop label xlabel xx = 1 to 10 -- xx is still the control variable but LABEL takes precidence
  ...
  loop yy = 1 to 10 -- yy is the control variable
    ...
    if yy = 3 then leave xlabel -- xx loop terminated by leave
    ...
    end yy
    ...
  end xlabel

do label FINIS
  say 'in do block'
  if (1 == 1) then leave FINIS
  say 'unreachable'
  signal Exception("Will never happen")
catch ex = Exception
  ex.printStackTrace()
finally
  say 'out of do block'
end FINIS

loop vv over ['A', 'B']
  select label selecting case vv
    when 'A' then do; say 'A selected'; say '...'; end
    when 'B' then do;
      say 'B selected';
      if (1 == 1) then leave selecting;
      say '...';
      end
    otherwise do; say 'nl selection'; say '...'; end
    end selecting
  end vv
```



### ITERATE

The <tt>ITERATE</tt> instruction alters flow of control within a <tt>LOOP</tt> construct.
On encountering an <tt>ITERATE</tt> instruction, execution of the loop body is terminated and control is passed directly back to the top of the loop just as though the last clause in the body of the loop had been executed.

As with <tt>LEAVE</tt> an optional ''<tt>name</tt>'' parameter can be supplied to direct the instruction to a loop level outside the current level.

```NetRexx
loop fff = 0 to 9
  ...
  loop xx = 1 to 3
    ...
    if fff > 2 then iterate fff
    ...
    end
  ...
  end fff
```



## Nim

===Labeled Break & Continue===
Break and continue can be used with block labels to jump out of multiple loops:

```nim
block outer:
  for i in 0..1000:
    for j in 0..1000:
      if i + j == 3:
        break outer
```


===Try-Except-Finally===

```nim
var f = open "input.txt"
try:
  var s = readLine f
except ReadIOEffect:
  echo "An error occured!"
finally:
  close f
```



## OCaml


An OCaml user can simulate flow control using exceptions:


```ocaml
exception Found of int

let () =
  (* search the first number in a list greater than 50 *)
  try
    let nums = [36; 23; 44; 51; 28; 63; 17] in
    List.iter (fun v -> if v > 50 then raise(Found v)) nums;
    print_endline "nothing found"
  with Found res ->
    Printf.printf "found %d\n" res
```



## Oforth


Oforth does not have goto statement.

break allows to break the current loop :

```Oforth>break</lang


continue allows to immediately start a new iteration :

```Oforth>continue</lang


perform is a method that transfer execution to the runnable on top of the stack, then returns :

```Oforth>perform</lang



## Oz

Exception handling is documented in other tasks: [[Exceptions#Oz]], [[Exceptions Through Nested Calls#Oz]].

The <code>case</code> statement can be used for [[Pattern Matching]], but also like a switch statement in C:

```oz
case {OS.rand} mod 3
of 0 then {Foo}
[] 1 then {Bar}
[] 2 then {Buzz}
end
```


The Lisp-influenced [http://www.mozart-oz.org/home/doc/loop/index.html for-loop] is very powerful and convenient to use.

As a constraint programming language, Oz has a number of flow control structures which target logic programming. They are typically used to implement new constraint search engines. However, it is also possible to use them for general logic programming.

* <code>or</code>: speculatively executes a number of alternative conditions and blocks until at most one alternative remains valid. Then either fails or commits to the remaining alternative if there is one.

* <code>cond</code>: evaluates a number of conditions in parallel (or in undefined order) and commits to the first alternative that succeeds.

* <code>dis</code>: depreciated

* <code>choice</code>: creates a non-deterministic choice point. In other words, the statement provisionally chooses an alternatives. If the choice turns out to be wrong or if additional solutions to a puzzle are searched, another alternative is chosen.

As an example for <code>choice</code>, a simple, but stupid way to solve the equation 2*X=18. We assume that the solution is somewhere in the interval 8-10, but we do not quite know what exactly it is.


```oz
declare
  proc {Stupid X}
     choice
        X = 8
        {System.showInfo "choosing 8"}
     [] X = 9
        {System.showInfo "choosing 9"}
     [] X = 10
        {System.showInfo "choosing 10"}
     end

     2 * X = 18
  end
in
  {Show {SearchOne Stupid}}
```


{{out}}

```txt

choosing 8
choosing 9
[9]

```



## PARI/GP

Flow control structures include function calling and returning, <code>error</code>/<code>trap</code>, <code>next</code>/<code>break</code>, <code>alarm</code>,
and the various loops.


## Pascal


### goto


```pascal
label
  jumpto;
begin
  ...
jumpto:
  some statement;
  ...
  goto jumpto;
  ...
end;
```



### exception


```pascal
try
  Z := DoDiv (X,Y);
except
  on EDivException do Z := 0;
end;
```



### Halt

Halt stops program execution and returns control to the calling program. The optional argument
Errnum specifies an exit value. If omitted, zero is returned.

```pascal
procedure halt(errnum: Byte);
```



### Exit

Exit exits the current subroutine, and returns control to the calling
routine.  If invoked in the main program routine, exit stops the program.
The optional argument X allows to specify a return value, in the case Exit
is invoked in a function.  The function result will then be equal to X.

```pascal
procedure exit(const X: TAnyType)
```


Calls of functions/procedures as well as breaks and continues in loops are described in the corresponding tasks.


## Perl

{{works with|Perl|5.x}}

### goto


Goto is typically looked down upon by most Perl programmers


```perl
FORK:
# some code
goto FORK;
```


## Perl 6


### Control exceptions

Control flow is extensible in Perl 6; most abnormal control flow (including the standard loop and switch exits) is managed by throwing control exceptions that are caught by the code implementing the construct in question.  Warnings are also handled via control exceptions, and turn into control flow if the dynamic context chooses not to resume after the warning.  See [[http://perlcabal.org/syn/S04.html#Control_Exceptions S04/Control exceptions]] for more information.

### Phasers

Phasers are blocks that are transparent to the normal control flow but that are automatically called at an appropriate phase of compilation or execution.  The current list of phasers may be found in [[http://perlcabal.org/syn/S04.html#Phasers S04/Phasers]].

### goto


```perl6>TOWN: goto TOWN;</lang

Labels that have not been defined yet must be enclosed in quotes.


## Phix


### goto

There is no hll goto statement in Phix. However in the very rare cases a goto is genuinely needed in hll code, the following
construct(s) may be used:

```Phix
#ilASM{ jmp :label }
...
#ilASM{ ::label }
```

In top level code, label scope is restricted to a single ilASM construct,
but within a routine, the scope is across all the ilasm in that routine.

There is quite deliberately no support for jumping from the middle of one
routine into another: without a frame, then quite simply parameters and
local variables have not been allocated and cannot be used/referenced.

It is also possible to declare global labels, which are superficially similar:

```Phix
#ilASM{ call :%label }
...
#ilASM{ jmp :skip
       :%label
        ret
       ::skip }
```

Global labels cannot be declared inside a routine, and as shown (almost always)
require a skip construct. It is up to the programmer to ensure global labels
are unique across the entire application.
Note that global labels are both declared and referenced with ":%", whereas
local labels are declared with "::" but referenced with ":".

Making "goto" somewhat more difficult to type in this manner ensures that
it is far less likely to be abused, and discourages newbie programmers
from adopting it as a weapon of choice, as usually(/always) happens with
a hll goto.


### exit

causes immediate termination of the immediately surrounding for or while loop, with control passing to the first statement after the loop, eg:

```Phix
for i=1 to 100 do
    if a[i]=x then
        location = i
        exit
    end if
end for
```



### continue

causes the next interation of the immediately surrounding loop to begin immediately, with any condition evaluated normally, eg:

```Phix
for i=1 to 100 do
    if a[i]=0 then continue end if
    ...
end for
```



### return

exits the current routine. Needs a value to return if used inside a function or type.


### break

terminate a switch statement. fallthrough is the opposite, overriding an implicit break between cases.


### abort

terminate the entire application immediately.


### tasks and threads

Phix supports both multitasking and multithreading. In multitasking, at most one task is currently running, so no locking is
required, and the application explicitly invokes task_yield to indicate when it is safe to switch between tasks. Multithreading
is potentially much trickier, everything that could be accessed concurrently must be locked, however when one thread is stalled,
perhaps waiting for a network response, the other threads are unaffected.


## PHP

{{works with|PHP|5.3}}

### goto


Introduced in PHP 5.3, PHP now has a goto flow-control structure, even though most PHP programmers see it as a bad habbit (may cause spaghetti-code).


```php
<?php
goto a;
echo 'Foo';

a:
echo 'Bar';
?>
```

{{out}}

```txt
Bar
```



## PicoLisp

As this task asks for the documentation of common flow control structures, we
refer here to the online documentation for more complete descriptions and
examples.

Relevant functions are:

### fork

[http://software-lab.de/doc/refF.html#fork fork] creates a child process

### task

[http://software-lab.de/doc/refT.html#task task] installs a background task
consisting of an environment and a list of executable expressions

### alarm

[http://software-lab.de/doc/refA.html#alarm alarm] schedules a timer, which
runs a given list of executable expressions when it expires

### abort

[http://software-lab.de/doc/refA.html#abort abort] runs a given list of
executable expressions, and aborts processing it if it takes longer than
a given time

### quit

[http://software-lab.de/doc/refQ.html#quit quit] immediately stops all
execution and returns to the top level read-eval-print loop, optionally
signaling an error

### wait

[http://software-lab.de/doc/refW.html#wait wait] delays current processing
(optionally to a maximal time) until an optionally given condition
evaluates to non-NIL

### sync

[http://software-lab.de/doc/refS.html#sync sync] synchronizes with other
processes of the same family

### protect

[http://software-lab.de/doc/refP.html#protect protect] delays the processing
of signals while a given list of executable expressions is executed

### catch

[http://software-lab.de/doc/refC.html#catch catch] prepares for receiving a
'throw' while running a given list of executable expressions

### throw

[http://software-lab.de/doc/refT.html#throw throw] causes a non-local jump
to a specified 'catch' environment

### bye

[http://software-lab.de/doc/refB.html#bye bye] exits the interpreter

### finally

[http://software-lab.de/doc/refF.html#finally finally] specifies a list of
executable expressions, to be run when current processing is done, even if
a 'throw' or 'bye' was executed, or an error occurred.


## PL/I


```PL/I

LEAVE
   The LEAVE statement terminates execution of a loop.
   Execution resumes at the next statement after the loop.
ITERATE
   The ITERATE statement causes the next iteration of the loop to
   commence.  Any statements between ITERATE and the end of the loop
   are not executed.
STOP
   Terminates execution of either a task or the entire program.
SIGNAL FINISH
   Terminates execution of a program in a nice way.
SIGNAL statement
   SIGNAL <condition> raises the named condition.  The condition may
   be one of the hardware or software conditions such as OVERFLOW,
   UNDERFLOW, ZERODIVIDE, SUBSCRIPTRANGE, STRINGRANGE, etc, or a
   user-defined condition.
CALL
   The CALL statement causes control to transfer to the named
   subroutine.
SELECT
   The SELECT statement permits the execution of just one of a
   list of statements (or groups of statements).
   It is sort of like a computed GOTO.
GO TO
   The GO TO statement causes control to be transferred to the named
   statement.
   It can also be used to transfer control to any one of an array of
   labelled statements. (This form is superseded by SELECT, above.)
   [GO TO  can also be spelled as  GOTO].

```



## Pop11



### quitloop

quitloop with argument exits from nested loops:


```pop11
while condition1 do
   while condition2 do
      if condition3 then
         quitloop(2);
      endif;
   endwhile;
endwhile;
```


above quitloop(2) exits from both loops.


### goto


goto l transfers control to the label l. goto may be used to exit from
nested loops:


```pop11
while condition1 do
   while condition2 do
      if condition3 then
         goto l;
      endif;
   endwhile;
endwhile;
l:;
```


Another use is to implement finite state machines:


```pop11
state1:
   DO_SOMETHING();
   if condition1 then
      goto state1;
   elseif condition2 then
      goto state2;
   ....
   else
      goto stateN;
   endif;
state2:
   ....
...
...
stateN:
   ....
```


Pop11 goto is a nonlocal one, so "jump out" from a chain of procedure calls:


```pop11
define outer();
   define inner(n);
      if n = 0 then
         goto final;
      endif;
      inner(n - 1);
   enddefine;
   inner(5);
   final:;
enddefine;
```


This is useful to exit early from successful recursive search, and for exception handling.

===go_on===

go_on is a multiway jump


```pop11
go_on expression to lab1, lab2, ..., labN else elselab ;
```


If expression has value K the above will jump to label labK, if expression is not an integer, or if it outside range from 1 to N, then control passes to label elselab. The else part may be omitted (then out of range values of expression cause an exception).

There is a more structured variant of go_on:

go_on expression to lab :
   lab 1 : statement1;
   lab 2 : statement2;
   ....
endgo_on;

where lab is a prefix chosen by the user.


### return


return ends execution of current function. In simplest form
it is just:


```pop11>return;</lang


but it is also possible to specify one or more return values:


```pop11
return(val1, val2, val3);
```



### chain


chain has effect of "tail call" but is not necessarily in tail position. More precisely inside proc1.


```pop11
chain proc2(x1, x2, x3);
```


finishes execution of proc1 and transfers control to the proc2 passing it x1, x2, and x3 as arguments. On return from proc2 control passes to caller of proc1.

Remark: Pop11 does not perform "tail call optimization", one has to explicitly use chain.

## PureBasic


### Goto

Transfers control to the label referenced.  It is not a safe way to exit loops.

```PureBasic
If OpenConsole()
  top:
  i = i + 1
  PrintN("Hello world.")
  If i < 10
    Goto top
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

===Gosub & Return===
Gosub stands for 'Go to sub routine'. A label must be specified after Gosub where the program execution continues and will do so until encountering a Return. When a return is reached, the program execution is then transferred immediately below the Gosub.
Gosub is useful when building fast structured code with very low overhead.

```PureBasic
X=1:  Y=2
Gosub Calc
;X will now equal 7
End

Calc:
  X+3*Y
  Return ; Returns to the point in the code where the Gosub jumped from
```


### FakeReturn

If the command Goto is used within the body of a sub routine, FakeReturn must be used to correct the stack or the program will crash.

```PureBasic
Gosub MySub

Lable2:
; The program will jump here, then 'end'
End

MySub:
If #PI>3
  FakeReturn  ; This will simulate the function of a normal "Return".
  Goto Lable2
EndIf
Return
```



### OnErrorGoto

This will transferee the program execution to the defined label if an error accrue.

```PureBasic
OnErrorGoto(?MyExitHandler)

X=1: Y=0
z= X/Y
; = a illegal division with zero
Debug "This line should never be reached"
End

MyExitHandler:
  MessageRequester("Error", ErrorMessage())
  End
```


### OnErrorCall

Similar to OnErrorGoto() but procedural instead.

```PureBasic
Procedure MyErrorHandler()
  ;All open files etc can be closed here
  MessageRequester("Error", ErrorMessage())
  End
EndProcedure

OnErrorCall(MyErrorHandler())
X=1: Y=0
Z= X/Y
;This line should never be reached
```



## Python


### Loops

Python supports ''break'' and ''continue'' to exit from a loop early or short circuit the rest of a loop's body and "continue" on to the next loop iteration.

```python
# Search for an odd factor of a using brute force:
for i in range(n):
    if (n%2) == 0:
        continue
    if (n%i) == 0:
        result = i
        break
else:
    result = None
    print "No odd factors found"
```

In addition, as shown in the foregoing example, Python loops support an ''else:'' suite which can be used to handle cases when the loop was intended to search for something, where the code would break out of the loop upon finding its target.  In that situation the ''else:'' suite can be used to handle the failure.  (In most other languages one is forced to use a "sentinel value" or a special flag variable ... typically set to "False" before the loop and conditionally set to "True" within the loop to handle situations for which the Python ''else:'' on loops is intended).


### Exceptions


A Python exception is simply any subclass of the built-in BaseException class, or any of its descendents.  User defined exception classes are normally descendents of the Exception class (which is, itself, a subclass of BaseException).  To "throw" any exception (user defined or otherwise) one uses the ''raise'' statement.  To capture exceptions one must enclose the code in a ''try:'' ... ''except...:'' block.  Any exception listed in an except block will catch all subclasses of that exception.  For example ZeroDivisionError is derived from ArithmeticError.  Thus an exception clause for ArithmeticError would catch a ZeroDivisionError (or any other ArithmeticError).

As a consequence of this one must arrange the order of exception clauses such that the more specific exceptions are listed (caught) before their more general base exceptions.  Only the first matching exception clause will be executed.  An except clause which lists no exceptions will catch '''all''' possible exceptions.  (This is usually considered to be very poor programming practice because it can hide unintended coding errors).

An exception can be re-raised by simply calling the ''raise'' statement without any arguments (from within any exception handler).  Thus a function can catch an exception, attempt to deal with it, then, if necessary, throw it it back to the next layer out in a given call stack.  Uncaught exceptions will be handled by the interpreter by terminating the program and printing an error message and stack trace.

A custom Exception class is normally declared with the ''pass'' statement as no methods of the parent class are over-ridden, no additional functionality is defined and no attributes need be set.  Example:


```python
class MyException(Exception): pass
```


One normally would choose the most similar existing class.  For example if MyException was going to be raised for some situation involving an invalid value it might be better to make it a subclass of ValueError; if it was somehow related to issues with inappropriate objects being passed around then one might make it a subclass of TypeError.

In large projects it's common to create an custom application base exception and to have all or most custom exceptions within that application or framework derive therefrom.

To create a "virtual base class" (one which is not intended to be directly instantiated, but exists solely to provide an inheritance to it's derived classes) one normally defines the requisite methods to raise "NotImplementedError" like so:


```python
class MyVirtual(object):
    def __init__(self):
        raise NotImplementedError
```


It then becomes necessary for any descendants of this class to over-ride the ''__init__()'' method.  Any attempt to instantiate a "MyVirtual" object directly will raise an exception.


'''Case 1 - Try, Except'''

```python
try:
    temp = 0/0
# 'except' catches any errors that may have been raised between the code of 'try' and 'except'
except:   # Note: catch all handler ... NOT RECOMMENDED
    print "An error occurred."
# Output : "An error occurred"
```


'''Case 2 - Try, Except'''

```python
try:
    temp = 0/0
# here, 'except' catches a specific type of error raised within the try block.
except ZeroDivisionError:
    print "You've divided by zero!"
# Output : "You've divided by zero!"
```


'''Case 3 - Try, Except, Finally'''

```python
try:
    temp = 0/0
except:
    print "An error occurred."
# here, 'finally' executes when the try - except block ends, regardless of whether an error was raised or not
# useful in areas such as closing opened file streams in the try block whether they were successfully opened or not
finally:
    print "End of 'try' block..."
# Output :
# An error occurred
# End of 'try' block...
```


Note: Prior to version 2.5 a ''try:'' statement could contain either series of ''except:'' clauses '''or''' a ''finally:'' clause but '''not both.'''
It was thus necessary to nest the exception handling in an enclosing ''try:''...''finally:'' loop like so:


```python
try:
    try:
        pass
    except (MyException1, MyOtherException):
        pass
    except SomeOtherException:
finally:
    do_some_cleanup() # run in any case, whether any exceptions were thrown or not
```


'''Case 4 - Try, Except, Else'''

```python
try:
    temp = 1/1 # not a division by zero error
except ZeroDivisionError: # so... it is not caught
    print "You've divided by zero."
# here, 'else' executes when no exceptions are caught...
else:
    print "No apparent error occurred."
# Output :
# No apparent error occurred.
```


'''Case 5 - Try, Except, break, continue'''

```python
i = 0
while 1: # infinite loop
    try:
       temp2 = 0/i # will raise a ZeroDivisionError first.
       temp = math.sqrt(i)

       break # 'break' will break out of the while loop
    except ValueError: #
        print "Imaginary Number! Breaking out of loop"
        break # 'break' out of while loop
    except ZeroDivisionError:
        print "You've divided by zero. Decrementing i and continuing..."
        i-=1 # we decrement i.
        # we 'continue', everything within the try - except block will be executed again,
        # this time however, ZeroDivisionError would not be raised again.
        continue # Note that removing it, replacing it with 'pass' would perform the equivalent
                 # see below for a better example
# Output :
# You've divided by zero. Decrementing i and continuing...
# Imaginary Number! Breaking out of loop
```


'''Case 6 - Creating your own custom exceptions, raise'''

```python
# Let's call our custom error "StupidError"; it inherits from the Exception class

class StupidError(Exception): pass

# Try it out.
try:
    raise StupidError("Segfault") # here, we manually 'raise' the error within the try block
except StupidError, details: # 'details' is the StupidError object we create in the try block.
    print 'Something stupid occurred:', details # so we access the value we had stored for it...


# Output :
# Something stupid occurred: Segfault
```


===continue, else in "for" loop===

```python
    i = 101
    for i in range(4): # loop 4 times
        print "I will always be seen."
        if i % 2 == 0:
            continue # continue goes back to the loop beginning for a new iteration.
        print "I'll only be seen every other time."
    else:
        print "Loop done"

    # Output:
    # I will always be seen.
    # I will always be seen.
    # I'll only be seen every other time.
    # I will always be seen.
    # I will always be seen.
    # I'll only be seen every other time.
    # Loop done

if(__name__ == "__main__"):
    main()
```


===The "with" statement===
{{works with|Python|2.6}}
See [[http://www.python.org/peps/pep-0343.html PEP 0343, The "with" statement]]


```python
class Quitting(Exception): pass
max = 10
with open("some_file") as myfile:
    exit_counter = 0
    for line in myfile:
        exit_counter += 1
        if exit_counter > max:
            raise Quitting
        print line,
```


The ''with'' statement allows classes to encapsulate "final" (clean-up) code which will automatically be executed regardless of exceptions that occur when working "with" these objects.  Thus, for the foregoing example, the file will be closed regardless of whether it's more than 10 lines long.  Many built-in and standard library classes have "context managers" which facilitate their use in ''with:'' code.  In addition it's possible to define special __enter__() and __exit__() methods in one's own classes which will be implicitly called by the interpreter when an object is used within a ''with:'' statement.

Use cases for ''with:'' enabled objects include automated/guaranteed closing of files, release of threading lock objects, commit or rollback of database transactions, and save/restore of any desired state (such as terminal settings when using the curses module, the precision settings when using the Decimal module, or even saving and restoring ''sys.stdout'' for temporary redirection).  It is a feature that seems to be unique to Python.


### Yield expressions

{{works with|Python|2.5}}
See [[http://www.python.org/peps/pep-0342.html PEP 0342, Coroutines via Enhanced Generators]]
 >>> value = 1
 >>> echo = lambda: (yield value)
 >>> for i in echo():
 ...   print i
 ...
 1



## Racket



###  exit

Racket's <tt>exit</tt> quits the whole process, optionally returning an exit code.  Note that there is an <tt>exit-handler</tt> that can be set to intercept such exit attempts.


###  goto

Racket doesn't have a <tt>goto</tt>, but like other implementations of Scheme, it adopts the mantra of "Lambda: the Ultimate GOTO" by having all tail calls optimized.  This allows writing code that is no different from your average assembly code -- for example, here's a direct translation of [[Greatest_common_divisor#x86_Assembly]] into a Racket function:


```racket

#lang racket

;; some silly boilerplate to mimic the assembly code better
(define r0 0)
(define (cmp r1 r2) (set! r0 (sgn (- r1 r2))))
(define (je true-label false-label) (if (zero? r0) (true-label) (false-label)))
(define (goto label) (label))

(define (gcd %eax %ecx)
  (define %edx 0)
  (define (main) (goto loop))
  (define (loop) (cmp 0 %ecx)
                 (je end cont))
  (define (cont) (set!-values [%eax %edx] (quotient/remainder %eax %ecx))
                 (set! %eax %ecx)
                 (set! %ecx %edx)
                 (goto loop))
  (define (end)  (printf "result: ~s\n" %eax)
                 (return %eax))
  (main))

```



###  Exceptions


Racket has exceptions which are used in the usual way, and <tt>with-handlers</tt> to catch them.  In fact, any value can be raised, not just exceptions.  For example:


```racket

(define (list-product l)
  (with-handlers ([void identity])
    (let loop ([l l] [r 1])
      (cond [(null? l) r]
            [(zero? (car l)) (raise 0)]
            [else (loop (cdr l) (* r (car l)))]))))

```



###  Continuations


Racket has full continuations, of all kinds, including delimited and not.  That's plenty of control flow...


###  And more


Given that Racket has macros, and continuations, and a zillion other features, it is easy to implement
new control flow expressions, so any list will not be exhaustive.


## REBOL


```REBOL
REBOL [
	Title: "Flow Control"
	URL: http://rosettacode.org/wiki/Flow_Control_Structures
]

; return -- Return early from function (normally, functions return
; result of last evaluation).

hatefive: func [
	"Prints value unless it's the number 5."
	value "Value to print."
][
	if value = 5 [return "I hate five!"]
	print value
]

print "Function hatefive, with various values:"
hatefive 99
hatefive 13
hatefive 5
hatefive 3

; break -- Break out of current loop.

print [crlf "Loop to 10, but break out at five:"]
repeat i 10 [
	if i = 5 [break]
	print i
]

; catch/throw -- throw breaks out of a code block to enclosing catch.

print [crlf "Start to print two lines, but throw out after the first:"]
catch [
	print "First"
	throw "I'm done!"
	print "Second"
]

; Using named catch blocks, you can select which catcher you want when throwing.

print [crlf "Throw from inner code block, caught by outer:"]
catch/name [
	print "Outer catch block."
	catch/name [
		print "Inner catch block."
		throw/name "I'm done!" 'Johnson
		print "We never get here."
	] 'Clemens
	print "We never get here, either."
] 'Johnson

; try

div: func [
	"Divide first number by second."
	a b
	/local r "Result"
][
	if error? try [r: a / b] [r: "Error!"]
	r ; Functions return last value evaluated.
]

print [crlf "Report error on bad division:"]
print div 10 4
print div 10 2
print div 10 1
print div 10 0
```



## REXX


### break

(See the '''leave''' statement.)

### call

The '''call''' statement immediately transfers control to a named subroutine, and the '''call''' statement may have any number (or none) parameters.   (However, most REXXes have some practical limit to the number of arguments, usually at least 50).

The named subroutine may or may not return a   '''result'''   (which is similar to a ''return code'', but REXX allows character strings as well).

(Also, see '''function invocation''' and '''signal''' statement below.)

```rexx
call  routineName                      /*no arguments passed to routine.*/
call  routineName  50                  /*one argument (fifty) passed.   */
call  routineName  50,60               /*two arguments        passed.   */
call  routineName  50, 60              /*(same as above)                */
call  routineName  50 ,60              /*(same as above)                */
call  routineName  10*5 , 8**4 - 4     /*(same as above)                */
call  routineName  50 , , , 70         /*4 args passed, 2nd&3rd omitted.*/
                                       /*omitted args are   NOT  null.  */
call  routineName  ,,,,,,,,,,,,,,,,800 /*17 args passed, 16 omitted.    */
call   date                            /*looks for DATE internally first*/
call  'DATE'                           /*  "    "    "  BIF | externally*/
```

real-life example:

```rexx
numeric digits 1000                 /*prepare for some gihugeic numbers.*/
...
n=4
call factorial n
say n'!=' result
exit
/*──────────────────────────────────FACTORIAL subroutine────────────────*/
factorial: parse arg x
!=1
           do j=2  to x
           !=!*j
           end   /*j*/
return !
```



### case

(See the '''select''' statement below.)


### exceptions

(See the '''signal''' statement and '''raising conditions''' below.)


### exit

The '''exit''' statement terminates the running (REXX) program and passes control to the invoking program (it could be the shell/host/supervisor program).

If an expression is coded, it normally is used to set the '''result''' (if a REXX program) or return code (also called ''RetCode'', ''RC'', ''completion code'', or other such names).

When using the '''exit''' with an expressing to pass control to the operating system (i.e., exiting a REXX program), some operating systems require the expression to be a whole number within a certain range (often with a no expression or a [null] expression, which is usually taken to mean a return code of '''0''').

If the expression is a number, it is normalized to the current '''numeric digits'''.

(Also, see the '''return''' statement below.)

```rexx
exit

exit  expression
```



### function invocation

A function invocation (similar to a '''call''') immediately transfers control to a named function (subroutine), and the function/subroutine invocation statement may have any number (or none) parameters.   (However, most REXXes have some practical limit to the number of arguments, usually at least 50).

(In REXX, the only difference between a ''function'' and a ''subroutine'' is that a ''function'' returns a   '''result'''   --- that is, some value is returned, which may be ''null'')

The named function/subroutine must return a   '''result'''   (which is similar to a return code, but REXX allows character strings as well).

If no   '''result'''   is returned, REXX generates a syntax error   (which may be trapped via the '''signal on syntax''' instruction).

(Also, see the '''call''' statement above.)

```rexx
numeric digits 1000                 /*prepare for some gihugeic numbers.*/
...
n=4
say n'!='  factorial(n)
exit
/*──────────────────────────────────FACTORIAL subroutine────────────────*/
factorial: parse arg x
!=1
           do j=2  to x
           !=!*j
           end   /*j*/
return !
```



### iterate

The '''iterate''' statement immediately transfer control to the innermost active   '''do'''   statement in which the '''iterate''' statement is located, that is, it (may) iterates (increments or decrements) the named REXX variable (if any) that is specified on the   '''do'''   statement.   The '''iterate''' statement can also specify which   '''do'''   loop is to be iterated if there is a named variable on the '''do''' loop.

(All indentations in REXX are merely cosmetic and are used for readability.}

```rexx
sum=0
        do j=1  to 1000
        if j//3==0 | j//7==0  then iterate
        sum=sum+j
        end   /*j*/

           /*shows sum of 1k numbers except those divisible by 3 or 7.*/
say 'sum='sum
...
numeric digits 5000
prod=0
                  do k=1  to 2000
                        do m=1  to k
                        if m>99  then iterate k
                        prod=prod*m
                        end   /*m*/
                  end      /*k*/
say 'prod=' prod
```



### go to

(See the '''signal''' statement.)


### leave

The '''leave''' statement transfer control to the next REXX statement following the   '''end'''   statement of the current (active)   '''do'''   loop in which the '''leave''' statement is located.   The '''leave''' statement can also specify which '''do''' loop is to be left (terminated) if the '''do''' loop has a named variable.

```rexx
  do j=1  to 10
  say 'j=' j
  if j>5  then leave
  say 'negative j='  (-j)
  end   /*j*/

say 'end of the DO loop for j.'

ouch=60
sum=0
                do k=0  to 100  by 3
                say 'k=' k
                           do m=1  to k
                           if m=ouch  then leave k
                           sum=sum+m
                           end   /*m*/
                end              /*k*/
say 'sum=' sum
```



### raising conditions


(REXX) conditions can be raised by causing some kind of "failure" or triggering event   (such as division by zero).

A '''signal''' statement must have been issued previous to the event being triggered to enable trapping.

It should be noted that some older REXXes don't support all the '''signal''' variants.

(Also, see the '''signal''' statement below.)

```rexx
...
signal on syntax
...
y=4 - 4
x=66
say x/y                  /*divide x by y.*/
say "yup, that's a divide by zero, by gum."
exit

syntax: say

/* We can now possibly do some repair work , but most people trap */
/* the condition, display where it happened, the REXX sourceline  */
/* (the actual REXX statement),  which condition was triggered,   */
/* display any other pertinent REXX variables, which line in the  */
/* REXX program, and then (usually) exit with some kind of error  */
/* message and error code indicator.                              */
/* Note:  the "name" of the REXX program isn't quite accurate,    */
/* rather, it is the name that was invoked (called by), which may */
/* be different name than the actual program being executed.      */

say '──────────────────────error!─────────────────────────'
say 'that division (above) will cause control to get here.'
parse source . . fid .
say;  say  'REXX raised a SYNTAX error in program:' fid
say;  say  'it occurred on line' sigl
say;  say  'the REXX statement is:'     /*put it on separate line.*/
      say  sourceline(sigl)
say;  say  'which code:' condition('C') "error"
say;  say  'error code:' condition('D')
say;  say  "Moral: don't do that."
exit 13
```

{{out}}

```txt

──────────────────────error!─────────────────────────
that division (above) will cause control to get here.

REXX raised a SYNTAX error in program: D:\OOPSsay.REX

it occurred on line 6

the REXX statement is:
say x/y                  /*divide x by y.*/

which code: SYNTAX error

error code: Error 42.3: Arithmetic overflow; divisor must not be zero

Moral: don't do that.

```

A note regarding the following REXXes:
::* PC/REXX
::* Personal REXX
::* R4
::* ROO
::* CMS REXX
::* TSO REXX
::* ooRexx
::* Regina REXX
Three conditions allow to specify CALL ON condition: ERROR, FAILURE, and HALT (there may be others).

From the corresponding condition handlers one can RETURN to the instruction
following the instruction/command where the condition was encountered.

A short example:

```rexx
Say 'Interrupt this program after a short while'
Call on halt
Do i=1 To 10000000
  j=i**2+1
  End
halt: Say i  j
Return
```



### return

The '''return''' statement terminates the running (REXX) program (which could be a subroutine or function) and passes control to the invoking program (it could be the shell/host/supervisor program).

If no internal subroutine or function is active,   '''return'''   is equivalent to   '''exit'''.

If a subroutine is active (a '''call''' was used), control goes to the instruction after the '''call''' statement.

If a function is active (a function reference was used) control goes back to the expression evaluation using the value resulting from the   '''return'''   expression.

(Also, see the '''exit''' statement above.)

```rexx
return

return  expression
```



### select

The '''select''' statement is used to conditionally test for cases to selectively execute REXX statement(s).

```rexx
...
prod=1
a=7               /*or somesuch.*/
b=3               /*  likewise. */

op='**'           /*or whatever.*/
...
    select
    when op=='+'         then  r=a+b           /*add.                   */
    when op=='-'         then  r=a-b           /*subtract.              */
    when op=='∙'         then do; r=a*b; prod=prod*r; end    /*multiply.*/
    when op=='*'         then  r=a*b           /*multiply.              */
    when op=='**'        then  r=a**b          /*power (exponentiation) */
    when op=='/'  & b\=0 then  r=a/b           /*divide.                */
    when op=='%'  & b\=0 then  r=a/b           /*interger divide.       */
    when op=='//' & b\=0 then  r=a/b           /*modulus (remainder).   */
    when op=='||'        then  r=a||b          /*concatenation.         */
    when op=='caw'       then  r=xyz(a,b)      /*call the XYZ subroutine*/
    otherwise                  r='[n/a]'       /*signify not applicable.*/
    end   /*select*/

say 'result for'   a   op   b   "="   r
```



### signal

The '''signal''' statement can be thought of as a ''GO TO'' or ''JUMP'' statement, however, on issuance of a '''signal''' statement, all active '''do''' loops and '''select''' structures are terminated.   Essentially, that means that there is no real way to re-enter a '''do''' loop (or a '''select''' structure) once a '''signal''' statement is used.

Once a '''signal''' statement is executed (or invoked), control passed to the first occurrence of the label specified (in REXX, more than one label with the same name isn't considered an error).   The label can be any combination of letters, digits, periods, and some special symbols, the most common are '''$''', '''#''', '''@''', '''!''', '''?''', and '''_''' (underscore or underbar).   Some versions of REXX (CMS, PC/REXX, Personal REXX, TSO, R4, ROO) also allow the cent sign ('''¢'''), some of those REXXes support the British pound (currency) symbol ('''£''').

The '''signal''' statement is also used to transfer control in case of some specific conditions:
::* when an I/O stream (could be a file) isn't ready.
::* when the REXX program used a variable that isn't defined.
::* when a REXX syntax error occurs.
::* when the program is '''halt'''ed (this depends on the operating system):
::::* Ctrl-Alt-Del                                                   under VM/CMS or MVS/TSO (generic names)
::::* HX                               under VM/CMS or MVS/TSO (generic names)
::::* PA1                                   under VM/CMS or MVS/TSO (generic names)
::::* Ctrl-c                                     some Unix, BSD variants
::::* Del   (key)                                               most System V variants
::::* SIGINT                                          (SIGnal INTerrupt) some variants of UNIX
::::* kill(1)                               from the command line (same as above)
::::* signal(3)                                            from a program (same as above)
::::*                                                 [to be sure, check with your operating system documentation]
::* when there is a loss of digits   (for the newer REXXes).
::* when a command executed returns an error return code [other than 0 (zero)].
::* when a command executed indicates a failure.
It should be noted that some older REXXes don't support all the '''signal''' variants.

(Also, see '''raising conditions''' above.)

```rexx
...
signal on error
signal on failure
signal on halt
signal on lostdigits      /*newer REXXes.*/
signal on notready
signal on novalue
signal on syntax

signal off error
signal off failure
signal off halt
signal off lostdigits     /*newer REXXes.*/
signal off notready
signal off novalue
signal off syntax
...
signal on novalue
...
x=oopsay+1                /* ◄─── this is it.*/
exit

novalue: say
say '───────────────────────────error!─────────────────────────────────'
say 'that reference to  oopsay  (above) will cause control to get here.'
parse source . . fid .
say;  say  'REXX raised a NOVALUE error in program:' fid
say;  say  'it occurred on line' sigl
say;  say  'the REXX statement is:'     /*put it on separate line.*/
      say  sourceline(sigl)
say;  say  'which code:' condition('C') "error"
say;  say  'REXX variable:' condition('D')
say;  say  "Moral: shouldn't do that."

```

{{out}}

```txt

───────────────────────────error!─────────────────────────────────
that reference to  oopsay  (above) will cause control to get here.

REXX raised a NOVALUE error in program: D:\flow_sig.rex

it occurred on line 20

the REXX statement is:
x=oopsay+1                /* ◄─── this is it.*/

which code: NOVALUE error

REXX variable: OOPSAY

Moral: shouldn't do that.

```



## Ring


```ring

i = 1
while true
      see i + nl
      if i = 10 see "Break!" exit ok
      i = i + 1
end

```



## Ruby


###  return

Return from the currently executing method to the caller.

###  loop control

Ruby's loop control statements are:   <code>break, next, redo and retry</code>.  Break and next are obvious.  Redo and retry both restart the current loop iteration, but retry first reevaluates the condition.
They can control <code>while, until, for</code> loops and iterators.

###  exceptions

Use <code>raise</code> to throw an exception.  You catch exceptions in the <code>rescue</code> clause of a <code>begin...end</code> block.

```ruby
begin
  # some code that may raise an exception
rescue ExceptionClassA => a
  # handle code
rescue ExceptionClassB, ExceptionClassC => b_or_c
  # handle ...
rescue
  # handle all other exceptions
else
  # when no exception occurred, execute this code
ensure
  # execute this code always
end
```

There is also a rescue modifier (example from the [http://www.pragprog.com/titles/ruby/programming-ruby Pickaxe book]):

```ruby
values = ["1", "2.3", /pattern/]
result = values.map {|v| Integer(v) rescue Float(v) rescue String(v)}
# => [1, 2.3, "(?-mix:pattern)"]
```



###  catch and throw

<code>break</code> will only break out of a single level of loop.  You can surround code in a catch block, and within the block you can throw a string or symbol to jump out to the end of the catch block (Ruby's GOTO, I suppose):

```ruby
def some_method
  # ...
  if some_condition
    throw :get_me_out_of_here
  end
  # ...
end

catch :get_me_out_of_here do
  for ...
    for ...
      some_method
    end
  end
end

puts "continuing after catching the throw"
```


###  yield

<code>yield</code> passes control from the currently executing method to its code block.


## SAS


```sas
/* GOTO: as in other languages
   STOP: to stop current data step */
data _null_;
	n=1;
	p=1;
L1:
	put n p;
	n=n+1;
	if n<=p then goto L1;
	p=p+1;
	n=1;
	if p>10 then stop;
	goto L1;

run;

/* LINK: equivalent of GOSUB in BASIC
   RETURN: after a LINK, or to return to the beginning of data step */
data _null_;
input a b;
link gcd;
put a b gcd;
return;

gcd:
	_a=a;
	_b=b;
	do while(_b>0);
	_r=mod(_a,_b);
	_a=_b;
	_b=_r;
	end;
	gcd=_a;
	return;

cards;
2 15
533 221
8 44
;
run;
```



## Scala

{{libheader|Scala}}

```Scala
import Goto._
import scala.util.continuations._

object Goto {

  case class Label(k: Label => Unit)

  private case class GotoThunk(label: Label) extends Throwable

  def label: Label @suspendable =
    shift((k: Label => Unit) => executeFrom(Label(k)))

  def goto(l: Label): Nothing =
    throw new GotoThunk(l)

  private def executeFrom(label: Label): Unit = {
    val nextLabel = try {
      label.k(label)
      None
    } catch {
      case g: GotoThunk => Some(g.label)
    }
    if (nextLabel.isDefined) executeFrom(nextLabel.get)
  }

}
```



## Sidef


### goto


```ruby
say "Hello"
goto :world
say "Never printed"
@:world
say "World"
```

{{out}}

```txt

Hello
World

```



## SSEM


###  Indirect absolute jump

The <tt>000 n to CI</tt> instruction loads the value stored at address <i>n</i> into the Current Instruction register. For instance,

```ssem
00101000000000000000000000000000      20 to CI
...
01010000000000000000000000000000  20. 10
```

loads the number 10 into CI. Since CI is incremented <i>after</i> the instruction has been executed, rather than before, this fragment will cause execution to jump to address 11.


###  Indirect relative jump

<tt>100 Add n to CI</tt> increases the number in the CI register by the value stored at address <i>n</i>.

```ssem
00101000000001000000000000000000      Add 20 to CI
...
01010000000000000000000000000000  20. 10
```

adds 10 to CI. Once again, CI is incremented <i>after</i> the instruction has been executed: so the machine actually jumps ahead by 11 instructions.


## Stata


Mata has a '''[https://www.stata.com/help.cgi?m2_goto goto]''' statement. It may be used to break nested loops, or to convert easily Fortran code to Mata.

As an example, let's find a Pythagorean triple a,b,c such that a+b+c=n, where n is given. Here goto is used to break the two loops when such a triple is found. A '''[https://www.stata.com/help.cgi?m2_return return]''' can be used in such situations, unless one has to do further computations after the loop.


```stata
mata
function pythagorean_triple(n) {
	for (a=1; a<=n; a++) {
		for (b=a; b<=n-a; b++) {
			c=n-a-b
			if (c>b & c*c==a*a+b*b) {
				printf("%f %f %f\n",a,b,c)
				goto END
			}
		}
	}
	END:
}

pythagorean_triple(1980)
165 900 915
```



## Tcl


###  after


The <tt>after</tt> facility can be used to execute some code
at some future time asynchronously, like this


```tcl
after 1000 {myroutine x}
```


which will call "<tt>myroutine</tt>" with parameter "<tt>x</tt>" 1000ms from 'now';
no matter what other code might be running at the time (i.e. "<tt>after</tt>"; schedules the execution, then returns and continues program flow with the following code).

The scheduled task can be removed from the scheduler for example with


```tcl>after cancel myroutine</lang


(other ways are possible).

The correct way to schedule some regularly recurring task in TCL
is to incorporate a self-scheduling at the end of the routine.
For example the following will produce a clock
whose display is updated once a second:


```tcl
package require Tk
proc update {} {
    .clockface configure -text [clock format [clock seconds]]
    after 1000 update ; # call yourself in a second
}
# now just create the 'clockface' and call ;update' once:
pack [label .clockface]
update
```



###  loop control

Tcl has the <code>break</code> command to abort the current loop (<tt>for</tt>/<tt>foreach</tt>/<tt>while</tt>) and the <code>continue</code> command to skip to the next loop iteration.


###  exception

Tcl's <code>catch</code> command can be used to provide a basic exception-handling mechanism:

```tcl
if {[catch { ''... code that might give error ...'' } result]} {
    puts "Error was $result"
} else {
    ''... process $result ...''
}
```

Tcl 8.6 also has a <tt>try</tt>…<tt>trap</tt>…<tt>finally</tt> structure for more complex exception handling.

```tcl
try {
    # Just a silly example...
    set f [open $filename]
    expr 1/0
    string length [read $f]
} trap {ARITH DIVZERO} {} {
    puts "divided by zero"
} finally {
    close $f
}
```



###  custom control structures

A novel aspect of Tcl is that it's relatively easy to create new control structures (more detail at http://wiki.tcl.tk/685).
For example, this example defines a command to perform some operation for each line of an input file:

```tcl
proc forfilelines {linevar filename code} {
    upvar $linevar line ; # connect local variable line to caller's variable
    set filechan [open $filename]
    while {[gets $filechan line] != -1} {
      uplevel 1 $code   ; # Run supplied code in caller's scope
    }
    close $filechan
}
```

Now we can use it to print the length of each line of file "mydata.txt":

```tcl
forfilelines myline mydata.txt {
    puts [string length $myline]
}
```



## Visual Basic .NET



###  Goto


This skips the line that changes the value of x to 5.


```vbnet
  Sub bar2()
      Dim x = 0
      GoTo label
      x = 5
label:
      Console.WriteLine(x)
  End Sub
```



###  On Error Goto


This brances in the event of an error. Usually there is an Exit (Sub|Function) to seperate the normal code from the error handling code


```vbnet
   Sub foo()
       On Error GoTo label
       'do something dangerous
       Exit Sub
label:
       Console.WriteLine("Operation Failed")
   End Sub
```


''This style of code is rarely used.''


###  On Error Resume Next


This performs a sequence of actions. If any action fails, the exception is discarded and next operation is performed.


```vbnet
Sub foo2()
    On Error Resume Next
    Operation1()
    Operation2()
    Operation3()
    Operation4()
End Sub
```


''This style of code is rarely used.''


###  Return / Exit Sub


This shows the classical and modern syntax for exiting a sub routine early.


```vbnet
Sub Foo1()
    If Not WorkNeeded() Then Exit Sub
    DoWork()
End Sub

Sub Foo2()
    If Not WorkNeeded() Then Return
    DoWork()
End Sub
```



###  Return value / Exit Function


This shows the classical and modern syntax for exiting a function early.
There is an implied variable with the same name as the function.
This variable is write-only.


```vbnet
Function Foo3()
    Foo3 = CalculateValue()
    If Not MoreWorkNeeded() Then Exit Function
    Foo3 = CalculateAnotherValue()
End Function

Function Foo4()
    Dim result = CalculateValue()
    If Not MoreWorkNeeded() Then Return result
    Return CalculateAnotherValue()
End Function
```



## zkl


```zkl
continue; continue(n);  // continue nth nested loop
break; break(n);  // break out of nth nested loop
try{ ... }catch(exception){ ... } [else{ ... }]
onExit(fcn);  // run fcn when enclosing function exits
```

zkl has state machines for functional style stream processing. There are some special values that machines can return for flow control:

```txt

Void.Again    // call the machine again with next value
Void.Drop     // drop an item and call next machine
(Void.Read,n) // read the next n items and pass to the next machine
Void.Skip     // ignore this item, ie continue
(Void.Skip,v) //   value is v
Void.Stop     // stop, ie break
(Void.Stop.v) //   value is v
(Void.write,x,y,z) // write x,y,x to sink

```

As an example, decode URL strings:

```zkl
urlText.pump(String,
   fcn(c){ if(c=="%")return(Void.Read,2); return(Void.Skip,c) },
   fcn(_,b,c){(b+c).toInt(16).toChar()})
```

has two machines. The second machine only runs if "%" is seen.

```zkl
urlText:="http%3A%2F%2Ffoo.com%2Fbar";
urlText.pump(...).println();
```

{{out}}
```txt
http://foo.com/bar
```

