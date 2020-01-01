+++
title = "Exceptions"
description = ""
date = 2019-10-18T09:58:29Z
aliases = []
[extra]
id = 1789
[taxonomies]
categories = []
tags = []
+++

{{Task|Control Structures}}
{{Control Structures}}
{{omit from|Euphoria}}
{{omit from|M4}}
{{omit from|Retro}}

This task is to give an example of an exception handling routine
and to "throw" a new exception.


;Related task:
*   [[Exceptions Through Nested Calls]]





## 11l


```11l
T SillyError
   String message
   F (message)
      .message = message

X.try
   X SillyError(‘egg’)
X.catch SillyError se
   print(se.message)
```



## Ada


'''Define an exception'''

```ada
Foo_Error : exception;
```


'''Raise an exception'''

```ada
procedure Foo is
begin
   raise Foo_Error;
end Foo;
```

Re-raising once caught exception:

```ada
  ...
exception
   when Foo_Error =>
      if ... then -- Alas, cannot handle it here,
         raise;   -- continue propagation of
      end if;
```


'''Handle an exception'''

```ada
procedure Call_Foo is
begin
   Foo;
exception
   when Foo_Error =>
      ... -- do something
   when others =>
      ... -- this catches all other exceptions
end Call_Foo;
```


'''Ada.Exceptions'''

The standard package Ada.Exceptions provides a possibility to attach messages to exceptions, to get exception occurrence information and textual description of exceptions. The following example illustrates basic functionality of:

```ada
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

procedure Main is
begin
   ...
   Raise_Exception (Foo_Error'Identity, "This is the exception message");
   ..
exception
   when Error : others =>
      Put_Line ("Something is wrong here" & Exception_Information (Error));
end Main;
```



## Aikido

Aikido provides <code>try</code>, <code>catch</code> and <code>throw</code> statements.

'''Catching exceptions'''

There is one <code>catch</code> clause per <code>try</code> statement.  The variable caught is whatever is thrown.  It does not have to be a particular type, although there is a <code>System.Exception</code> class defined for system exceptions.

```aikido

try {
    var lines = readfile ("input.txt")
    process (lines)
} catch (e) {
   do_somthing(e)
}


```


'''Throwing exceptions'''

You can throw any value.

```aikido

if (error) {
    throw "Error"
}

if (something) {
   throw new MyException (errorcode, a, b)
}



```



## Aime

'''Simple Exception Throwing'''

```aime
void
throwing(void)
{
    o_text("throwing...\n");
    error("now!");
}

void
catching(void)
{
    o_text("ready to catch\n");
    if (trap(throwing)) {
	o_text("caught!\n");
    } else {
	# nothing was thrown
    }
}

integer
main(void)
{
    catching();

    return 0;
}
```

{{out}}

```txt
ready to catch
throwing...
aime: tmp/et: 5: now!
caught!
```

'''Exception Types'''

```aime
void
ft(integer a, text &s)
{
    if (a & 1) {
	s = "odd";
	error("bad number");
    } elif (a & a - 1) {
	s = "not a power of two";
	error("bad number");
    }
}

void
fc(integer a)
{
    text e;

    if (trap(ft, a, e)) {
	v_text("exception of type `");
	v_text(e);
	v_text("' thrown for ");
	v_integer(a);
	v_newline();
    } else {
	v_text("no exception thrown for ");
	v_integer(a);
	v_newline();
    }
}

integer
main(void)
{
    fc(5);
    fc(6);
    fc(8);

    return 0;
}
```

{{out}}

```txt
aime: tmp/et1: 6: bad number
exception of type `odd' thrown for 5
aime: tmp/et1: 9: bad number
exception of type `not a power of two' thrown for 6
no exception thrown for 8
```



## ALGOL 68


Typically: In the Algol68 ''transput'' an attempt is first made to "mend" an "event" on an object.  However: After a
'''mend''' has failed, the event may be "sent" (via a GO TO) then "caught" by in an outside scope.

The "GOST 27975-88 Programming language ALGOL 68 extended" Soviet standard -
([http://vak.ru/lib/exe/fetch.php/book/gost/pdf/gost-27975-88.pdf Язык программирования АЛГОЛ 68 расширенный (PDF)])
had the addition mechanisms, e.g.: ''on'', ''exception'' & ''raise''.

{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.8 algol68g-2.8].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
﻿'''File: prelude/event_base(obj).a68'''
```algol68
COMMENT
  Define an general event handling mechanism on MODE OBJ:
  * try to parallel pythons exception handling flexibility
END COMMENT

COMMENT
  REQUIRES:
    MODE OBJ # These can be a UNION of REF types #
    OP OBJIS
  PROVIDES:
    OP ON, RAISE, RESET
    PROC obj on, obj raise, obj reset
END COMMENT

# define object related to OBJ EVENTS #
MODE
  RAISEOBJ = PROC(OBJ)VOID, RAWMENDOBJ = PROC(OBJ)BOOL,
  MENDOBJ = UNION(RAWMENDOBJ, PROC VOID), # Generalise: Allow PROC VOID (a GOTO) as a short hand #
  NEWSCOPEOBJ = STRUCT(REF NEWSCOPEOBJ up, FLEXOBJ obj flex, FLEXEVENTOBJ event flex, MENDOBJ mended),
  SCOPEOBJ = REF NEWSCOPEOBJ;

MODE FLEXOBJ=FLEX[0]OBJ;

# Provide an INIT to convert a GO TO to a MEND ... useful for direct aborts #
OP INITMENDOBJ = (PROC VOID go to)MENDOBJ: (go to; SKIP);

SCOPEOBJ obj scope end = NIL;
SCOPEOBJ obj scope begin := obj scope end; # INITialise stack #
OBJ obj any = EMPTY;
EVENTOBJ obj event any = NIL;

# Some crude Singly Linked-List manipulations of the scopes, aka stack ... #
# An event/mended can be shared for all OBJ of the same type: #
PRIO INITAB = 1, +=: = 1;
OP INITAB = (SCOPEOBJ lhs, MENDOBJ obj mend)SCOPEOBJ:
  lhs := (obj scope end, obj any, obj event any, obj mend);

OP INITSCOPE = (MENDOBJ obj mend)SCOPEOBJ: HEAP NEWSCOPEOBJ INITAB obj mend;
OP +=: = (SCOPEOBJ item, REF SCOPEOBJ rhs)SCOPEOBJ: ( up OF item := rhs; rhs := item );
OP +=: = (MENDOBJ mend, REF SCOPEOBJ rhs)SCOPEOBJ: INITSCOPE mend +=: rhs;
#OP -=: = (REF SCOPEOBJ scope)SCOPEOBJ: scope := up OF scope;#

COMMENT Restore the prio event scope: ~ END COMMENT
PROC obj reset = (SCOPEOBJ up scope)VOID: obj scope begin := up scope;
MENDOBJ obj unmendable = (OBJ obj)BOOL: FALSE;

MODE NEWEVENTOBJ = STRUCT( # the is simple a typed place holder #
  SCOPEOBJ scope,
  STRING description,
  PROC (OBJ #obj#, MENDOBJ #obj mend#)SCOPEOBJ on,
  PROC (OBJ #obj#, STRING #msg#)VOID raise
), EVENTOBJ = REF NEWEVENTOBJ;

MODE FLEXEVENTOBJ = FLEX[0]EVENTOBJ;

COMMENT Define how to catch an event:
    obj - IF obj IS NIL then mend event on all OBJects
    obj mend - PROC to call to repair the object
    return the prior event scope
END COMMENT
PROC obj on = (FLEXOBJ obj flex, FLEXEVENTOBJ event flex, MENDOBJ mend)SCOPEOBJ: (
  mend +=: obj scope begin;
  IF obj any ISNTIN obj flex THEN obj flex OF obj scope begin := obj flex FI;
  IF obj event any ISNTIN event flex THEN event flex OF obj scope begin := event flex FI;
  up OF obj scope begin
);

PRIO OBJIS = 4, OBJISNT = 4; # pick the same PRIOrity as EQ and NE #
OP OBJISNT = (OBJ a,b)BOOL: NOT(a OBJIS b);

PRIO ISIN = 4, ISNTIN = 4;
OP ISNTIN = (OBJ obj, FLEXOBJ obj flex)BOOL: (
  BOOL isnt in := FALSE;
  FOR i TO UPB obj flex WHILE isnt in := obj OBJISNT obj flex[i] DO SKIP OD;
  isnt in
);
OP ISIN = (OBJ obj, FLEXOBJ obj flex)BOOL: NOT(obj ISNTIN obj flex);

OP ISNTIN = (EVENTOBJ event, FLEXEVENTOBJ event flex)BOOL: (
  BOOL isnt in := TRUE;
  FOR i TO UPB event flex WHILE isnt in := event ISNT event flex[i] DO SKIP OD;
  isnt in
);
OP ISIN = (EVENTOBJ event, FLEXEVENTOBJ event flex)BOOL: NOT(event ISNTIN event flex);

COMMENT Define how to raise an event, once it is raised try and mend it:
  if all else fails produce an error message and stop
END COMMENT
PROC obj raise = (OBJ obj, EVENTOBJ event, STRING msg)VOID:(
  SCOPEOBJ this scope := obj scope begin;
# until mended this event should cascade through scope event handlers/members #
  FOR i WHILE this scope ISNT SCOPEOBJ(obj scope end) DO
    IF (obj any ISIN obj flex OF this scope OR obj ISIN obj flex OF this scope ) AND
       (obj event any ISIN event flex OF this scope OR event ISIN event flex OF this scope)
    THEN
      CASE mended OF this scope IN
        (RAWMENDOBJ mend):IF mend(obj) THEN break mended FI,
        (PROC VOID go to): (go to; stop)
      OUT put(stand error, "undefined: raise stop"); stop
      ESAC
    FI;
    this scope := up OF this scope
  OD;
  put(stand error, ("OBJ event: ",msg)); stop; FALSE
EXIT
  break mended: TRUE
);

CO define ON and some useful(?) RAISE OPs CO
PRIO ON = 1, RAISE = 1;
OP ON = (MENDOBJ mend, EVENTOBJ event)SCOPEOBJ: obj on(obj any, event, mend),
   RAISE = (OBJ obj, EVENTOBJ event)VOID: obj raise(obj, event, "unnamed event"),
   RAISE = (OBJ obj, MENDOBJ mend)VOID: ( mend ON obj event any; obj RAISE obj event any),
   RAISE = (EVENTOBJ event)VOID: obj raise(obj any, event, "unnamed event"),
   RAISE = (MENDOBJ mend)VOID: ( mend ON obj event any; RAISE obj event any),
   RAISE = (STRING msg, EVENTOBJ event)VOID: obj raise(obj any, event, msg);
OP (SCOPEOBJ #up scope#)VOID RESET = obj reset;

SKIP
```
'''File: test/event.a68'''
```algol68
#!/usr/bin/a68g --script #

MODE OBJ=UNION(REF INT, REF REAL, REF STRING,# etc # VOID);

OP OBJIS = (OBJ a,b)BOOL: # Are a and b at the same address? #
  CASE a IN # Ironically Algol68's STRONG typing means we cannot simply compare addresses #
    (REF INT a): a IS (b|(REF INT b):b|NIL),
    (REF REAL a): a IS (b|(REF REAL b):b|NIL),
    (REF STRING a): a IS (b|(REF STRING b):b|NIL)
  OUT FALSE
  ESAC;

PR READ "prelude/event_base(obj).a68" PR;
NEWEVENTOBJ obj eventa := SKIP;
NEWEVENTOBJ obj eventb := SKIP;
NEWEVENTOBJ user defined exception := SKIP;

# An event can be continued "mended" or break "unmended" #
PROC found sum sqs continue = (OBJ obj)BOOL: ( print("."); TRUE); # mended #
PROC found sum sqs break = (OBJ obj)BOOL: (found sq sum sqs; FALSE); # unmended #

INT sum sqs:=0;
REAL x:=111, y:=222, z:=333;

SCOPEOBJ obj scope reset := obj on((sum sqs, x,y,z), (obj eventa,obj eventb), VOID:found sq sum sqs);

# An event handler specific to the specific object instance: #
#SCOPEOBJ obj scope reset := obj on eventb(sum sqs, VOID:found sq sum sqs);#

# Or... An "obj any" event handler: #
# SCOPEOBJ obj scope reset := found sum sqs break ON obj eventb; #

# Raise the "event eventb" on an object: #
  FOR i DO
    sum sqs +:= i*i;
    IF sum sqs = 70*70 THEN # 1st try to use an instance specific mend on the object #
      obj raise(sum sqs, obj eventb, "Found a sq sum of sqs") FI; # OR ... #
    IF sum sqs = 70*70 THEN "Found a sq sum of sqs" RAISE obj eventb FI; # OR ... #
    IF sum sqs = 70*70 THEN RAISE found sum sqs break FI # simplest #
  OD;
  RESET obj scope reset # need to manually reset back to prior handlers #

# Catch "event eventb": #
EXIT found sq sum sqs:
  print(("sum sqs:",sum sqs, new line)); # event eventb caught code here ... #
  RESET obj scope reset;

  "finally: raise the base unmendable event" RAISE obj eventb
```
{{out}}

```txt

sum sqs:      +4900
OBJ event: finally: raise the base unmendable event

```

'''Standard Prelude "on event" routines'''

ALGOL 68 uses event routines extensively in the "standard transput"
(stdio) to manage the various events that arise when data is read
(or written) to a file or external device.
The built in "on event" routines are:
* ''on char error'' - if the character transput (input or output) in cannot be converted to the standard character set.
* ''on format error'' - if the format specified is incompatible to the data being transput (input or output)
* ''on line end'' - if an end of line was read while the program was "transputting" data
* ''on logical file end'' - if the end of data was encountered during transput
* ''on page end'' - if the end of a page was encountered during transput
* ''on physical file end'' - if the end of physical media was encountered during transput
* ''on value error'' - if the data transput was incompatibly with the variable being transput, eg a letter when a digit was expected.

All of the above allow the programmer to define a user created event
routine when a particular event happens to a particular '''FILE'''.
When such an event routine is called, then the routine can use any of
the standard prelude routine to reposition the '''FILE''' and rectify
the detected event, e.g.:
* ''space'' or ''back space''
* ''new line'', ''new page'', ''set'' or ''reset''.
For example: these may notify the operator to mount a new tape (in the
case of physical file end).

The handler is permitted to return '''TRUE''' depending on whether the
event has been handled and the program can can continue.  And '''FALSE'''
is when event remains un-handled, and the standard prelude event routine
should be used.  The handler is also permitted to exit to a label (without
returning anything) if the user defined event routine determines that
processing is complete.

;See also
* StackOverflow: [http://stackoverflow.com/questions/1449951/what-language-was-the-first-to-implement-exception-handling What language was the first to implement exception handling?]


## AppleScript


'''try'''

```applescript
try
    set num to 1 / 0
    --do something that might throw an error
end try
```


'''try-on error'''

```applescript
try
    set num to 1 / 0
    --do something that might throw an error
on error errMess number errNum
    --errMess and number errNum are optional
    display alert "Error # " & errNum & return & errMess
end try
```


'''error'''

```applescript
error "Error message." number 2000
```


## AutoHotkey


###  True exceptions

{{works with|AutoHotkey_L}}
In [[AutoHotkey_L]] [http://l.autohotkey.net/docs/commands/Try.htm Try], [http://l.autohotkey.net/docs/commands/Catch.htm Catch], and [http://l.autohotkey.net/docs/commands/Throw.htm Throw] are available to handle exceptions.<br/>
From the [http://l.autohotkey.net/docs/commands/Throw.htm Throw documentation]:

```AHK
try
    BadlyCodedFunc()
catch e
    MsgBox % "Error in " e.What ", which was called at line " e.Line

BadlyCodedFunc() {
    throw Exception("Fail", -1)
}
```

=== ErrorLevel-based exceptions ===
In [[AutoHotkey_Basic]], the only option for error-handling is using ErrorLevel

```AutoHotkey
foo()
If ErrorLevel
  Msgbox calling foo failed with:  %ErrorLevel%

foo()
{
  If success
    Return
  Else
    ErrorLevel = foo_error
  Return
}
```



## BBC BASIC


```bbcbasic
      ON ERROR PROCerror(ERR, REPORT$) : END

      ERROR 100, "User-generated exception"
      END

      DEF PROCerror(er%, rpt$)
      PRINT "Exception occurred"
      PRINT "Error number was " ; er%
      PRINT "Error string was " rpt$
      ENDPROC
```

'''Output:'''

```txt

Exception occurred
Error number was 100
Error string was User-generated exception

```



## blz


```blz

try
    1 / 0 # Throw an exception
    print("unreachable code")
catch
    print("An error occured!")
end

```



## Bracmat

After completed evaluation, each Bracmat expression not only has a value, but also a success or failure status attached to it.
Pattern matching expressions are the most common expressions to test some condition, but also other expressions can obtain a
status different from 'success'. Here are some situations where an expression fails:
* pattern matching fails if the pattern does not match the subject
* reading the contents of a file fails if the file cannot be opened for reading or if the file has not the expected format
* retrieving a value bound to a symbol that has no value bound to it fails
* accessing an object member that does not exist fails
* division by zero fails
* an operation headed by the 'and then' operator <code>&</code> fails if either its left hand side or its right hand side fails
* an operation headed by the 'or else' operator <code>|</code> fails if both its left hand side and its right hand side fail

Rather than writing statements delimited by <code>;</code> characters, which do not have usefull succes/failure states, you write
a Bracmat program with 'and then' and 'or else' as connectives between expressions. A Bracmat program therefore needs only consist
of a single statement. In many cases a series of expressions connected with the 'and then' operator is meant to succeed,
evaluating each expression in turn with success. In an exceptional case, however, such as a file that cannot be opened, the series
is interrupted and control can be taken over by an outer 'or else' control structure that evaluates its right hand side.

To 'throw and exception' you can use the always failing expression <code>~</code>. In the example, the <code>~</code> ensures that
the call to the <code>contemplate</code> function is never attempted if something went wrong with reading the files. This failure
percolates further up, so calling the function <code>MyFunction</code> fails as well, making the whole program fail.


```bracmat
( ( MyFunction
  =   someText XMLstuff
    .   (   get$!arg:?someText
          & get$("CorporateData.xml",X,ML):?XMLstuff
        |     out
            $ ( str
              $ ( "Something went wrong when reading your file \""
                  !arg
                  "\". Or was it the Corporate Data? Hard to say. Anyhow, now I throw you out."
                )
              )
          & ~
        )
      & contemplate$(!someText,!XMLstuff)
  )
& MyFunction$"Tralula.txt"
);
```

If you copy/paste this code to the Bracmat prompt <em>without the statement delimiter <code>;</code></em>, you will see an 'F' after the output, indicating that your input failed to
evaluate successfully.

```txt
Something went wrong when reading your file "Tralula.txt". Or was it the Corporate Data? Hard to say. Anyhow, now I throw you out.

    F

```



## C


The setjmp()/longjmp() functions in the C standard library header <setjmp.h> are typically used for exception handling.

'''try-catch'''


```c
#include <setjmp.h>

enum { MY_EXCEPTION = 1 }; /* any non-zero number */

jmp_buf env;

void foo()
{
  longjmp(env, MY_EXCEPTION); /* throw MY_EXCEPTION */
}

void call_foo()
{
  switch (setjmp(env)) {
  case 0:                     /* try */
    foo();
    break;
  case MY_EXCEPTION:          /* catch MY_EXCEPTION */
    /* handle exceptions of type MY_EXCEPTION */
    break;
  default:
    /* handle any type of exception not handled by above catches */
    /* note: if this "default" section is not included, that would be equivalent to a blank "default" section */
    /* i.e. any exception not caught above would be caught and ignored */
    /* there is no way to "let the exception through" */
  }
}
```


With multi-thread support and nested exceptions

```c

#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>

enum exceptions {
        EXCEPTION_1 = 1,
        EXCEPTION_2,
        EXCEPTION_3
};


#define throw(exception) do {                                   \
                if (exp)                                        \
                        longjmp(*exp, exception);               \
                printf("uncaught exception %d\n", exception);   \
                exit(exception);                                \
        } while (0)

#define try(block, catch_block)                 \
{                                               \
        jmp_buf *exception_outer = exp;         \
        jmp_buf exception_inner;                \
        exp = &exception_inner;                 \
        int exception = setjmp(*exp);           \
        if (!exception) {                       \
                do block while(0);              \
                exp = exception_outer;          \
        } else {                                \
                exp = exception_outer;          \
                switch(exception) {             \
                catch_block                     \
                default:                        \
                        throw(exception);       \
                }                               \
        }                                       \
}

#define catch(exception, block) \
        case exception: do block while (0); break;


#define throws  jmp_buf* exp

// define a throwing function
void g(throws) {
        printf("g !\n");
        throw(EXCEPTION_1);
        printf("shouldnt b here\n");
}

void h(throws, int a)
{
        printf("h %d!\n", a);
        throw(EXCEPTION_2);
}

void f(throws) {
        try({
                g(exp); // call g with intention to catch exceptions
        },
        catch(EXCEPTION_1, {
                printf("exception 1\n");
                h(exp, 50); // will throw exception 2 inside this catch block
        }))
}

int main(int argc, char* argv[])
{
        throws = NULL; // define exception stack base
        try({
                f(exp);
        },
        catch(EXCEPTION_2, {
                printf("exception 2\n");
        })
        catch(EXCEPTION_3, {
                printf("exception 3\n");
        }))

        h(exp, 60); // will result in "uncaught exception"
        return 0;
}


```

Now all we need to do is add a finally block :) hint: it is possible


## C++


C++ has no finally construct. Instead you can do this in the destructor of an object on the stack, which will be called
if an exception is thrown.

The exception can be of any type, this includes int's, other primitives, as well as objects.

'''Defining exceptions'''

```cpp
struct MyException
{
  // data with info about exception
};
```


There's also a class <tt>std::exception</tt> which you can, but are not required to derive your exception class from. The advantage of doing so is that you can catch unknown exceptions and still get some meaningful information out. There are also more specific classes like <tt>std::runtime_error</tt> which derive from <tt>std::exception</tt>.


```cpp
#include <exception>

struct MyException: std::exception
{
  char const* what() const throw() { return "description"; }
}
```


Note that in principle you can throw any copyable type as exception, including built-in types.

'''Throw exceptions'''

```cpp
// this function can throw any type of exception
void foo()
{
  throw MyException();
}

// this function can only throw the types of exceptions that are listed
void foo2() throw(MyException)
{
  throw MyException();
}

// this function turns any exceptions other than MyException into std::bad_exception
void foo3() throw(MyException, std::bad_exception)
{
  throw MyException();
}
```


'''Catching exceptions'''

```cpp
try {
  foo();
}
catch (MyException &exc)
{
  // handle exceptions of type MyException and derived
}
catch (std::exception &exc)
{
  // handle exceptions derived from std::exception, which were not handled by above catches
  // e.g.
  std::cerr << exc.what() << std::endl;
}
catch (...)
{
  // handle any type of exception not handled by above catches
}
```


## C#
{{works with|Visual Studio|2005}}

'''Defining exceptions'''

```c#
public class MyException : Exception
{
  // data with info about exception
};
```


'''Throw exceptions'''

```c#
void foo()
{
  throw MyException();
}
```


'''Catching exceptions'''

```c#
try {
  foo();
}
catch (MyException e)
{
  // handle exceptions of type MyException and derived
}
catch
{
  // handle any type of exception not handled by above catches
}
```



## Clojure

Expression handling in Clojure is basically like Java in S-expressions:

```clojure
(try
  (if (> (rand) 0.5)
    (throw (RuntimeException. "oops!"))
  (println "see this half the time")
  (catch RuntimeException e
    (println e)
  (finally
    (println "always see this"))
```



## ColdFusion


'''Catch Exceptions'''

inside &lt;cfscript&gt;:


```cfm
try {
  foo();
} catch (Any e) {
  // handle exception e
}
```


otherwise:

```cfm><cftry

<cfcatch type="Database|...">
</cfcatch>
</cftry>
```



## Common Lisp


The Common Lisp condition system allows much more control over condition signaling and condition handling than many exception-based systems.  The following example, however, simply defines a condition type, <code>unexpected-odd-number</code>, defines a function <code>get-number</code> which generates a random number, returning it if it is even, but signaling an <code>unexpected-odd-number</code> condition if it is odd.  The function <code>get-even-number</code> uses <code>[http://www.lispworks.com/documentation/HyperSpec/Body/m_hand_1.htm handler-case]</code> to call <code>get-number</code> returning its result if no condition is signaled, and, in the case that an <code>unexpected-odd-number</code> condition is signaled, returning one plus the odd number.


```lisp
(define-condition unexpected-odd-number (error)
  ((number :reader number :initarg :number))
  (:report (lambda (condition stream)
             (format stream "Unexpected odd number: ~w."
                     (number condition)))))

(defun get-number (&aux (n (random 100)))
  (if (not (oddp n)) n
    (error 'unexpected-odd-number :number n)))

(defun get-even-number ()
  (handler-case (get-number)
    (unexpected-odd-number (condition)
      (1+ (number condition)))))
```


A good introduction to Lisp's condition system is the chapter [http://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html Beyond Exception Handling: Conditions and Restarts] from Peter Seibel's [http://gigamonkeys.com/book/ Practical Common Lisp].

In Common Lisp, there are functions <code>[http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm throw]</code> and <code>[http://www.lispworks.com/documentation/HyperSpec/Body/s_catch.htm catch]</code>, but these are not related to the condition system.  Rather, they provide another mechanism for non-local control transfer.


## D


```d
import std.stdio;

/// Throw Exceptions
/// Stack traces are generated compiling with the -g switch.
void test1() {
  throw new Exception("Sample Exception");
}

/// Catch Exceptions
void test2() {
    try {
        test1();
    } catch (Exception ex) {
        writeln(ex);
        throw ex; // rethrow
    }
}

/// Ways to implement finally
void test3() {
    try test2();
    finally writeln("test3 finally");
}

/// Or also with scope guards
void test4() {
    scope(exit) writeln("Test4 done");
    scope(failure) writeln("Test4 exited by exception");
    scope(success) writeln("Test4 exited by return or function end");
    test2();
}

void main() {
    test4();
}
```



## Delphi


'''Throw Exceptions'''

```delphi
procedure test;
begin
  raise Exception.Create('Sample Exception');
end;
```


'''Catch Exceptions'''

```delphi
procedure test2;
begin
  try
    test;
  except
    ShowMessage(Exception(ExceptObject).Message); // Showing exception message
    raise; // Rethrowing
  end;
end;
```


'''Ways to implement finally'''

```delphi
procedure test3;
begin
  try
    test2;
  finally
    ShowMessage('test3 finally');
  end;
end;
```


=={{header|Déjà Vu}}==


```dejavu
stuff-going-wrong:
	raise :value-error

try:
	stuff-going-wrong
catch value-error:
	!print "Whoops!"
```

{{out}}

```txt
Whoops!
```



## DWScript


'''Throw Exceptions'''

```delphi
procedure Test;
begin
   raise Exception.Create('Sample Exception');
end;
```


'''Catch Exceptions'''

```delphi
procedure Test2;
begin
   try
     test;
   except
      on E: Exception do begin  // Filter by exception class
         PrintLn(E.Message);    // Showing exception message
         raise;                 // Rethrowing
      end;
  end;
end;
```


'''Ways to implement finally'''

```delphi
procedure Test3;
begin
   try
      test2;
   finally
      PrintLn('Test3 finally');
   end;
end;
```



## E


'''Exceptions'''

An exception ''object'' describes what the problem is and has nothing to do with control flow.

Due to E's ancestry as a JVM scripting language, E does not '''yet''' have any standard mechanism for user-defined exception types.

A string provided in place of an exception will be coerced to a generic exception object.

There are two control flow constructs used with exceptions: throw and eject.

'''Throw and catch'''

<code>throw</code> is the built-in ''function'' which throws exceptions in the conventional sense: control goes to the <code>catch</code> block of the most recently entered <code>try</code>/<code>catch</code> construct.


```e
def nameOf(arg :int) {
    if (arg == 43) {
        return "Bob"
    } else {
        throw("Who?")
    }
}

def catching(arg) {
    try {
        return ["ok", nameOf(arg)]
    } catch exceptionObj {
        return ["notok", exceptionObj]
    }
}
```



```e
? catching(42)
# value: ["not ok", problem: Who?]

? catching(43)
# value: ["ok", "Bob"]

? catching(45.7)
# value: ["not ok", problem: the float64 45.7 doesn't coerce to an int]
```


However, there is a problem here: exceptions accidentally produced or uncaught from inside a given module can lead to the calling program getting information about the internals that it shouldn't have (possibly a security problem). As a result of this, we are planning to move to a 'sealed exception' model where throw and catch have the same control flow, but only debuggers can see any information in a ''caught'' exception other than "a throw happened". For situations where the caller ''should'' have information about what happened, the ejector mechanism will be used.

'''Ejectors'''

Ejectors provide the same sort of "exit to catch block" control flow that throw/catch do, but with an explicit path rather than implicitly "nearest enclosing". Ejectors are also used as a general purpose control construct as well as for exceptions.

The <code>escape ''ej'' { ''body'' } catch ''pat'' { ''catch block'' }</code> construct creates an ejector object and binds it to ''ej'', which is valid for as long as ''body'' is executing. An ejector object is a function; if it is called, then control immediately passes to the ''catch block'', with its argument bound to ''pat''.

The above code rewritten to use ejectors:


```e
def nameOf(arg :int, ejector) {
    if (arg == 43) {
        return "Bob"
    } else {
        ejector("Who?")
    }
}

def catching(arg) {
    escape unnamed {
        return ["ok", nameOf(arg, unnamed)]
    } catch exceptionObj {
        return ["notok", exceptionObj]
    }
}
```



```e
? catching(42)
# value: ["not ok", problem: Who?]

? catching(43)
# value: ["ok", "Bob"]

? catching(45.7)
# problem: the float64 45.7 doesn't coerce to an int
```


Note that the escape-catch block does ''not'' catch the coercion error resulting from passing a float64 instead of an int, since that is an (implicit) throw.

(One further refinement: While an ejector is an ordinary function, which does not return, it is generally desirable to protect against being supplied a function which unexpectedly ''does'' return. For this purpose we have <code>throw.eject</code> which calls the supplied function and throws if that function returns: <code>throw.eject(ejector, "Who?")</code>)

The benefit of using ejectors to communicate exceptions, besides the information-leak prevention described above, is that only exceptions intended to be handled by that catch block will be passed to it; unexpected internal errors will be handled by general try/catch handlers.

For example, suppose we have nameOf written as follows:


```e
var nameTable := null
def nameOf(arg :int, ejector) {
    if (nameTable == null) {
        nameTable := <import:nameTableParser>.parseFile(<file:nameTable.txt>)
    }
    if (nameTable.maps(arg)) {
        return nameTable[arg]
    } else {
        ejector(makeNotFoundException("Who?"))
    }
}
```


Suppose that loading the parser, or reading the file, throws a NotFoundException (note this exception type was made up for this example). Even though it is of the same type as the "Who?" exception, it will not be caught by the caller's escape/catch block since it was not passed via the ejector, whereas a traditional "try { ... } catch ex :NotFoundException { ... }" as in other languages would, leading to incorrect handling of the error.


## Elena


'''Defining exceptions'''

```elena
class MyException : Exception
{
    constructor new()
        <= new("MyException raised");
}
```


'''Throw exceptions'''

```elena
foo()
{
    MyException.new().raise()
}

```


'''Catching exceptions'''

```elena
try
{
    o.foo()
}
catch:(MyException e)
{
    // handle exceptions of type MyException and derived
}
```


'''Catching any exception'''

```elena
o.foo() | on:(e)
{
    // handle any type of exception
};
```



## Erlang


```Erlang

-module( exceptions ).

-export( [task/0] ).

task() ->
    try
    erlang:throw( new_exception )

    catch
    _:Exception -> io:fwrite( "Catched ~p~n", [Exception] )

    end.

```

{{out}}

```txt

14> exceptions:task().
Catched new_exception

```



## Factor


'''Throw Exceptions'''

```factor
"Install Linux, Problem Solved" throw

TUPLE: velociraptor ;
\ velociraptor new throw
```


Or a shorthand for this:

```factor
ERROR: velociraptor ;
velociraptor
```


'''Catch Exceptions'''

```factor
! Preferred exception handling
: try-foo
    [ foo ] [ foo-failed ] recover ;

: try-bar
    [ bar ] [ bar-errored ] [ bar-always ] cleanup ;

! Used rarely
[ "Fail" throw ] try   ! throws a "Fail"
[ "Fail" throw ] catch ! returns "Fail"
[ "Hi" print ] catch   ! returns f (looks the same as throwing f; don't throw f)
[ f throw ] catch      ! returns f, bad!  use recover or cleanup instead
```



## Fancy


```fancy
# define custom exception class
# StandardError is base class for all exception classes
class MyError : StandardError {
  def initialize: message {
    # forward to StdError's initialize method
    super initialize: message
  }
}

try {
  # raises/throws a new MyError exception within try-block
  MyError new: "my message" . raise!
} catch MyError => e {
  # catch exception
  # this will print "my message"
  e message println
} finally {
  # this will always be executed (as in e.g. Java)
  "This is how exception handling in Fancy works :)" println
}
```



## Fantom



```fantom

// Create a new error class by subclassing sys::Err
const class SpecialErr : Err
{
  // you must provide some message about the error
  // to the parent class, for reporting
  new make () : super ("special error") {}
}

class Main
{
  static Void fn ()
  {
    throw SpecialErr ()
  }

  public static Void main ()
  {
    try
      fn()
    catch (SpecialErr e)
      echo ("Caught " + e)
  }
}

```


{{out}}

```txt

$ fan exceptions.fan
Caught exceptions_0::SpecialErr: special error

```



## Forth

Forth's exception mechanism is, like most things in Forth, very simple but powerful. CATCH captures the data and return stack pointers, then executes an execution token. THROW conditionally throws a value up to the most recent CATCH, restoring the stack pointers.

'''Throw Exceptions'''

```forth
: f ( -- )  1 throw ." f " ;  \ will throw a "1"
: g ( -- )  0 throw ." g " ;  \ does not throw
```


'''Catch Exceptions'''

```forth
: report ( n -- ) ?dup if ." caught " . else ." no throw" then ;
: test ( -- )
  ['] f catch report
  ['] g catch report ;
```

test example.  (Output shown in bold)

```forth
cr test
'''caught 1 g no throw ok'''
```


Note that CATCH only restores the stack pointers, not the stack values, so any values that were changed during the execution of the token will have undefined values.  In practice, this means writing code to clean up the stack, like this:

```forth
10 ['] myfun catch if drop then
```



## FreeBASIC

FreeBASIC does not support exceptions or the Try/Catch/Finally statement, as such.
However, you can use the Err() function, together with a Switch statement, to provide somewhat similar functionality:

```freebasic
' FB 1.05.0 Win64

Enum ErrorType
  myError = 1000
End Enum

Sub foo()
  Err = 1000 ' raise a user-defined error
End Sub

Sub callFoo()
  foo()
  Dim As Long errNo = Err ' cache Err in case it's reset by a different function
  Select Case errNo
    Case 0
      ' No error (system defined)
    Case 1 To 17
      ' System defined runtime errors
    Case myError:   ' catch myError
      Print "Caught myError : Error number"; errNo
    Case Else
      ' catch any other type of errors here
  End Select
  ' add any clean-up code here
End Sub

callfoo()
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Caught myError : Error number 1000

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=c3abec93bc7f135203f1f7582f5c3a19 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iInteger As Integer

MakeError
DivError

iInteger = "2.54"

Catch
  Print Error.Text

End
'______________________
Public Sub DivError()

Print 10 / 0

Catch
  Print Error.Text

End
'______________________
Public Sub MakeError()

Error.Raise("My Error")

Catch
  Print Error.Text

End
```

Output:

```txt

My Error
Division by zero
Type mismatch: wanted Integer, got String instead

```



## Go

Execution errors such as attempting to index an array out of bounds trigger a run-time panic equivalent to a call of the built-in function <tt>panic()</tt> with a value of the implementation-defined interface type <tt>runtime.Error</tt>.

<tt>panic(x)</tt> "throws" a value (of any type), and <tt>recover()</tt> "catches" it.

<tt>recover()</tt> needs to be called in a "deferred" function call, otherwise it will have no effect. <tt>defer</tt> delays the function call until the current function returns (or fails).


```go
package main

import "fmt"

func foo() int {
	fmt.Println("let's foo...")
	defer func() {
		if e := recover(); e != nil {
			fmt.Println("Recovered from", e)
		}
	}()
	var a []int
	a[12] = 0
	fmt.Println("there's no point in going on.")
	panic("never reached")
	panic(fmt.Scan) // Can use any value, here a function!
}

func main() {
	foo()
	fmt.Println("glad that's over.")
}
```
[http://play.golang.org/p/9ymYAmOMIP Run in the Go Playground].
{{out}}

```txt

let's foo...
Recovered from runtime error: index out of range
glad that's over.

```



## Haskell

Exceptions can be implemented using monads; no special syntax is necessary.[http://haskell.org/haskellwiki/Exception] In GHC, specialized functionality for exceptions are provided by the <tt>Control.Exception</tt> module.

'''Defining exceptions'''

The type "Exception", which contains pre-defined exceptions, cannot be extended. You can however use "dynamic exceptions", which can be of any type that is of "Typeable" class. <!-- Somebody look over this; I don't really understand it. -->

'''Throw exceptions'''

In the context of the IO monad, use "throwIO" to throw exceptions; the expression will return any type:

```haskell
do {- ... -}
   throwIO SomeException
```


In purely functional context, use "throw" to throw exceptions; the expression will match any type:

```haskell
if condition then 3
else throw SomeException
```


To throw a user-defined exception, use "throwDyn":

```haskell
if condition then 3
else throwDyn myException
```


'''Catching exceptions'''

The "catch" function performs the whole try-catch stuff. It is usually used in infix style:
pattern-matches on the exception type and argument:

```haskell
do
  {- do IO computations here -}
`catch` \ex -> do
  {- handle exception "ex" here -}
```


Note: Control.Exception's "catch" is different than Prelude's "catch".

To catch a user-defined exception, use "catchDyn":

```haskell
do
  {- do IO computations here -}
`catchDyn` \ex -> do
  {- handle exception "ex" here -}
```



## HolyC

HolyC has an exception handling mechanism using <code>try/throw/catch</code>. Exception names are limited to a maximum of 8 bytes in length.

The <code>catch</code> block does not have the capability to differentiate between specific exceptions. Instead, all exceptions for a <code>try</code> block are handled by a single <code>catch</code> block.


```holyc
try {
  U8 *err = 'Error';
  throw(err); // throw exception
} catch {
  if (err == 'Error')
    Print("Raised 'Error'");
  PutExcept; // print the exception and stack trace
}
```


==Icon and {{header|Unicon}}==

The following Unicon example makes use of support for exceptions found in the
[http://tapestry.tucson.az.us/unilib/ The Unicon Code Library].  <i>Since
exception support is not built into Unicon, but rather implemented as
Unicon code, there are limitations not found in languages that natively
support exceptions.</i>


```Unicon
import Exceptions

procedure main(A)
    every i := !A do {
        case Try().call{ write(g(i)) } of {
            Try().catch(): {
                x := Try().getException()
                write(x.getMessage(), ":\n", x.getLocation())
                }
            }
        }
end

procedure g(i)
    if numeric(i) = 3 then Exception().throw("bad value of "||i)
    return i
end
```


A sample run is:

```txt

-> ExceptionTest 1 2 3 4 5
1
2
Exception: bad value of 3:
    procedure g [ExceptionTest.icn:15]
    procedure main [ExceptionTest.icn:5]

4
5
->

```


Note: it may be possible to implement exceptions in Icon; however, it would require a major rework and would likely be inelegant.


## J


'''Tacit'''

Program<tt> u :: v </tt>executes <tt>u</tt> and provides its result as output unless an error occurs. In case of error, the result of <tt>v</tt> is provided instead.


'''Explicit'''

An exception in an explicit definition can be detected with <tt>try.</tt> and <tt>catcht.</tt> and can be thrown with <tt> throw. </tt> as seen below.


```j
   pickyPicky =: verb define
     if. y-:'bad argument' do.
        throw.
     else.
        'thanks!'
     end.
   )

   tryThis  =: verb define
     try.
        pickyPicky y
     catcht.
        'Uh oh!'
     end.
   )

   tryThis 'bad argument'
Uh oh!
```



## Java

An exception needs to extend the Exception type.

'''Defining exceptions'''

```java
//Checked exception
public class MyException extends Exception {
   //Put specific info in here
}

//Unchecked exception
public class MyRuntimeException extends RuntimeException {}
```


'''Throw exceptions'''

```java
public void fooChecked() throws MyException {
   throw new MyException();
}

public void fooUnchecked() {
   throw new MyRuntimeException();
}
```


'''Catching exceptions'''

```java
try {
   fooChecked();
}
catch(MyException exc) {
   //Catch only your specified type of exception
}
catch(Exception exc) {
   //Catch any non-system error exception
}
catch(Throwable exc) {
   //Catch everything including system errors (not recommended)
}
finally {
   //This code is always executed after exiting the try block
}
```

{{works with|Java|7+}}
Java 7 added "multicatch" and "smart rethrow".

```java5
public void foo() throws UnsupportedDataTypeException{
    try{
        throwsNumberFormatException();
        //the following methods throw exceptions which extend IOException
        throwsUnsupportedDataTypeException();
        throwsFileNotFoundException();
    }catch(FileNotFoundException | NumberFormatException ex){
        //deal with these two Exceptions without duplicating code
    }catch(IOException e){
        //deal with the UnsupportedDataTypeException as well as any other unchecked IOExceptions
        throw e;
    }
}
```

In previous versions of Java, <code>foo()</code> would have to declare that it throws an <code>IOException</code>. The "smart rethrow" recognizes that the only checked exception that can result in the rethrow ("<code>throw e;</code>") is an <code>UnsupportedDataTypeException</code>. The last catch block will still catch any other unchecked <code>IOException</code>s and rethrow them, but <code>foo()</code> only needs to declare that <code>UnsupportedDataTypeException</code>s are thrown from it since that's the only checked exception that can cause a rethrow.

The other catch block uses the same code to handle both <code>FileNotFoundException</code>s and <code>NumberFormatException</code>s by adding a <code>|</code> between the exception types that are declared.


## JavaScript


'''Throwing exceptions'''


```javascript
function doStuff() {
  throw new Error('Not implemented!');
}
```


'''Catching exceptions'''


```javascript
try {
  element.attachEvent('onclick', doStuff);
}
catch(e if e instanceof TypeError) {
  element.addEventListener('click', doStuff, false);
}
finally {
  eventSetup = true;
}
```


## jq

{{works with|jq|>1.4}}
The ability to "catch" exceptions was introduced after jq version
1.4 was released and so a brief explanation is included here.

Exceptions, as before, can be raised by the execution of an error statement: error(STRING)

The "try" clause takes the form:

```jq>try FILTER catch CATCHER</lang

where FILTER and CATCHER may be any jq expressions.

Within a "try" clause, . is the value of the STRING of the exception that has been caught.

'''Example''':

```jq
def division(a;b):
  def abs: if . < 0 then -. else . end;
  if a == 0 and b == 0 then error("0/0")
  elif b == 0 then error("division by 0")
  elif (a|abs|log) - (b|abs|log) > 700 then error("OOB")
  else a/b
  end;

def test(a;b):
  try division(a;b)
  catch if . == "0/0" then 0
        elif . == "division by 0" then null
        else "\(.): \(a) / \(b)"
        end;
```


# test(0;0) # produces 0

# test(1e300; 1e-300) # produces "OOB: 1e+300 / 1e-300"


## Julia

{{works with|Julia|0.6}}


```julia
function extendedsqrt(x)
    try sqrt(x)
    catch
        if x isa Number
            sqrt(complex(x, 0))
        else
            throw(DomainError())
        end
    end
end

@show extendedsqrt(1)   # 1
@show extendedsqrt(-1)  # 0.0 + 1.0im
@show extendedsqrt('x') # ERROR: DomainError
```



## Kotlin


```scala
// version 1.0.6

// In Kotlin all Exception classes derive from Throwable and, by convention, end with the word 'Exception'
class MyException (override val message: String?): Throwable(message)

fun foo() {
    throw MyException("Bad foo!")
}

fun goo() {
    try {
        foo()
    }
    catch (me: MyException) {
        println("Caught MyException due to '${me.message}'")
        println("\nThe stack trace is:\n")
        me.printStackTrace()
    }
}

fun main(args: Array<String>) {
    goo()
}
```


{{out}}

```txt

Caught MyException due to 'Bad foo!'

The stack trace is:

MyException: Bad foo!
        at ExceptionKt.foo(exception.kt:7)
        at ExceptionKt.goo(exception.kt:12)
        at ExceptionKt.main(exception.kt:22)

```



## Langur

Exceptions in langur are hashes guaranteed to contain certain fields, even if they're empty.

A catch causes all the statements preceding it within a block to be the implicit try block. As just one example, you could put an unscoped catch at the end of a script and it will cover the entire script.


```Langur
# do something
throw "not a math exception"

catch .e {
    if .e["cat"] == "math" {
        # change result...
    } else {
        # rethrow the exception
        throw
    }
}
```



## Lasso


```Lasso>protect =
 {
   handle_error => {
        // do something else
   }
   fail(-1,'Oops')
}
```



## Lingo

Lingo has no try...catch mechanism. A script error will always end execution of the current call stack. There is however a mechanism that prevents that script errors quit the execution of the current movie/projector: you can set up an "alertHook" that is called when such errors occur. This alertHook can then e.g. log the error to a file or database, and if it returns 1, the movie/projector continues to play:


```lingo
-- parent script "ErrorHandler"

on alertHook (me, errorType, errorMessage, alertType)
  if alertType=#alert then return 0 -- ignore programmatic alerts

  -- log error in file "error.log"
  fn = _movie.path&"error.log"
  fp = xtra("fileIO").new()
  fp.openFile(fn, 2)
  if fp.status() = -37 then
    fp.createFile(fn)
    fp.openFile(fn, 2)
  end if
  fp.setPosition(fp.getLength())
  fp.writeString(_system.date() && _system.time() && errorType & ": " & errorMessage & RETURN)
  fp.closeFile()

  return 1 -- continues movie playback, no error dialog
end
```



```lingo
-- in a movie script
on prepareMovie
  _player.alertHook = script("ErrorHandler")
end
```


In terms of the behavior described above, a "throw" command triggering custom errors that behave exactly like real script errors can be implemented like this:


```lingo
-- in a movie script
-- usage: throw("Custom error 23")
on throw (msg)
  _player.alertHook.alertHook("Script runtime error", msg, #script)
  abort() -- exits call stack
end
```



## Logo

{{works with|UCB Logo}}

```logo
to div.checked :a :b
  if :b = 0 [(throw "divzero 0)]
  output :a / :b
end
to div.safely :a :b
  output catch "divzero [div.checked :a :b]
end
```

There are also some predefined exceptions:
* '''throw "toplevel''' returns to the interactive prompt if uncaught (like control-C)
* '''(throw "error [message])''' prints a message like a primitive, bypassing normal catch output
* '''throw "system''' immediately exits Logo to the shell
* '''catch "error''' will catch any thrown error instead of printing an error message


## Logtalk

Logtalk exception-handling mechanism is based on the catch/3 and throw/1 predicates inherited from Prolog:

```logtalk

:- object(exceptions).

    :- public(double/2).
    double(X, Y) :-
        catch(double_it(X,Y), Error, handler(Error, Y)).

    handler(error(not_a_number(X), logtalk(This::double(X,Y), Sender)), Y) :-
        % try to fix the error and resume computation;
        % if not possible, rethrow the exception
        (   catch(number_codes(Nx, X), _, fail) ->
            double_it(Nx, Y)
        ;   throw(error(not_a_number(X), logtalk(This::double(X,Y), Sender)))
        ).

    double_it(X, Y) :-
        (   number(X) ->
            Y is 2*X
        ;   this(This),
            sender(Sender),
            throw(error(not_a_number(X), logtalk(This::double(X,Y), Sender)))
        ).

:- end_object.

```

{{out}}

```txt

| ?- exceptions::double(1, Double).
Double = 2
yes

| ?- exceptions::double("1", Double).
Double = 2
yes

| ?- exceptions::double(a, Double).
uncaught exception: error(not_a_number(a),logtalk(exceptions::double(a,_),user))

```



## Lua

'''Throwing an Exception'''


```Lua

error("Something bad happened!")

```


'''Catching Exceptions'''


```Lua

function throw_error()
    error("Whoops")
    -- won't ever appear, due to previous error() call
    return "hello!"
end

-- 'status' is false if 'throw_error' threw an error
-- otherwise, when everything went well, it will be true.
-- 'errmsg' contains the error message, plus filename and line number of where the error occured
status, errmsg = pcall(throw_error)
print("errmsg = ", errmsg)

```


Note that `pcall` passes every argument after the function object or function name to said function:<br />

```Lua

function throw_error_with_argment(argument)
    error(string.format("Whoops! argument = %s", argument))
    -- won't ever appear, due to previous error() call
    return "hello!"
end

status, errmsg = pcall(throw_error_with_argment, "foobar 123")
print("errmsg = ", errmsg)

```


If a function does not throw an error, 'errmsg' (which might be called 'returned' as well) contains the value(s) returned from the function:<br />

```Lua

function throw_error_with_argment(argument)
    return "hello!"
end

status, errmsg = pcall(throw_error_with_argment, "foobar 123")
print("errmsg = ", errmsg)

```


## M2000 Interpreter


```M2000 Interpreter

Module Errors {
      Module Check {
            Module Error1 {
                  A=1/0
            }

            Try ok {
                  Error1
            }
            ' we get an Error, and Error$ print division by zero in module Error1
            If Error or not ok then Print Error$
            Error "New Error"
      }
      Try {
            Check
      }
      Print Error=0  ' no Error return
      Print Error$  ' but Error message isn't clear
      ' Error$ used one time, then cleared automatic
}
Errors
Print Error$=""

```

{{out}}

```txt

 division by zero in module ERROR1
    True
 New Error in module CHECK
    True

```



## Make

In make, an exception is caused when a rule returns a non-zero status
i.e the below will fail as false returns 1, (thus raising exception)

fail.mk


```make
all:
     false
```


Using -@ to ignore the exception.

catch.mk


```make
all:
    -@make -f fail.mk
```


Using explicit exit 0 to ignore the exception.

catch.mk


```make
all:
    make -f fail.mk; exit 0
```



## Maple


```Maple

errorproc:=proc(n)
local a;
try
a:=1/n;
catch "numeric exception: division by zero":
error "Something went wrong when dividing."
end try;
end proc;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
f[x_] := If[x > 10, Throw[overflow], x!]

Example usage :
Catch[f[2] + f[11]]
-> overflow

Catch[f[2] + f[3]]
-> 8
```



## MATLAB

Errors are thrown using the "error" keyword.

Sample usage:

```MATLAB>>
 error 'Help'
??? Help
```


=={{header|Modula-3}}==

'''Defining exceptions'''

Exceptions can only be declared at the "top-level" of a module or interface.  Arguments are optional.

```modula3
EXCEPTION EndOfFile;
EXCEPTION Error(TEXT);
```


'''Throw exceptions'''

Exceptions can be bound to procedures using RAISES:

```modula3
PROCEDURE Foo() RAISES { EndOfFile } =
  ...
  RAISE EndOfFile;
  ...
```


'''Catching exceptions'''

```modula3
TRY
  Foo();
EXCEPT
| EndOfFile => HandleFoo();
END;
```


Modula-3 also has a FINALLY keyword:

```modula3
TRY
  Foo();
FINALLY
  CleanupFoo(); (* always executed *)
END;
```




## MOO


'''Throw exceptions'''

Values can be raised to exceptions using raise():

```moo
raise(E_PERM);
```


'''Catching exceptions'''

```moo
try
  this:foo();
except e (ANY)
  this:bar(e);
endtry
```


MOO also has a finally statement:

```moo
try
  this:foo();
finally
  this:bar();
endtry
```


'''Shorthand'''

```moo
`this:foo()!ANY=>this:bar()';
```



## Nanoquery


```nanoquery
try
      invalid "this statement will fail"
catch $e
      println "caught an exception"
      println $e
end try

```

Throwing exceptions:

```nanoquery
throw new("Nanoquery.Exceptions.Exception", "exception reason as string")
```



## Nemerle


```Nemerle
// define a new exception
class MyException : Exception
{
    ...
}

// throw an exception
Foo() : void
{
    throw MyException();
}

// catching exceptions
try {
    Foo();
}
catch { // catch block uses pattern matching syntax
    |e is MyException => ... // handle exception
    |_ => throw e // rethrow unhandled exception
}
finally {
    ... // code executes whether or not exception was thrown
}
```



## NetRexx

As <tt>NetRexx</tt> runs under the control of a JVM it has the same exception model as [[#Java|Java]].

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

--
### =======================================================================

class RExceptions public

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method test() public signals RExceptions.TakeException
    if (1 == 1) then signal RExceptions.TakeException()
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    do
      RExceptions().test()
    catch ex = Exception
      say ex.toString()
    end

    return;

--
### =======================================================================

class RExceptions.TakeException public extends Exception

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method TakeException() public
    super('I resent that!')
    return

```

'''Output:'''

```txt

RExceptions$TakeException: I resent that!

```



## Nim

'''Defining exceptions'''

```nim>type SillyError = object of Exception</lang

'''Throwing an exception'''

```nim
proc spam() =
  raise newException(SillyError, "Some error")
```

'''Handling an exception'''

```nim
try:
  spam()
except SillyError:
  echo "Got SillyError with message: ", getCurrentExceptionMsg()
except:
  echo "Got another exception"
finally:
  echo "Finally"
```


=={{header|Objective-C}}==

'''Defining exceptions'''

Exceptions can be any Objective-C object, though they are usually instances of <code>NSException</code>. You can create a subclass of NSException if necessary:

```objc
@interface MyException : NSException {
  //Put specific info in here
}
@end
```


'''Throw exceptions'''

```objc
- (void)foo {
  @throw [NSException exceptionWithName:@"TerribleException"
                                 reason:@"OMGWTFBBQ111!1"  userInfo:nil];
}
```


'''Catching exceptions'''

```objc
@try {
  [self foo];
}
@catch (MyException *exc) {
  //Catch only your specified type of exception
}
@catch (NSException *exc) {
  //Catch any NSException or subclass
  NSLog(@"caught exception named %@, with reason: %@", [exc name], [exc reason]);
}
@catch (id exc) {
  //Catch any kind of object
}
@finally {
  //This code is always executed after exiting the try block
}
```



## OCaml


'''Defining exceptions'''

Like constructors, exceptions may or may not have an argument:

```ocaml
exception My_Exception;;
exception Another_Exception of string;;
```


'''Throw exceptions'''

Throw exceptions with the "raise" function; the expression will match any type:

```ocaml
let foo x =
  match x with
    1 -> raise My_Exception
  | 2 -> raise (Another_Exception "hi mom")
  | _ -> 5
;;
```


'''Catching exceptions'''

The "with" syntax pattern-matches on the exception type and argument:

```ocaml
try
  string_of_int (foo 2)
with
  My_Exception        -> "got my exception"
| Another_Exception s -> s
| _                   -> "unknown exception"
```




## Oforth


Oforth uses try/when blocks to trap exceptions and throw to throw an execption

It is also possible to create new exception classes (see Exception.of).


```Oforth
: iwillThrowAnException  "A new exception" Exception throw ;

: iwillCatch
| e |
   try: e [ iwillThrowAnException ] when: [ "Exception catched :" . e .cr ]
   try: e [ 1 2 over last ] when: [ "Exception catched :" . e .cr ]
   "Done" println ;
```


{{out}}

```txt

Exception catched : A new exception
Exception catched : 1 does not understand #last
Done

```



## Oz


'''Throw exceptions'''

Any value can be thrown as an exception. Typically record values are used.

```oz
raise sillyError end
raise slightlyLessSilly(data:42 reason:outOfMemory) end
```


By using a record value with a feature <code>debug</code> set to <code>unit</code> you can indicate that the exception shall have debug information (including a stack trace).


```oz
try
   raise someError(debug:unit) end
catch someError(debug:d(stack:ST ...)...) then
   {Inspect ST}
end
```


See also: [http://www.mozart-oz.org/documentation/base/exception.html Exceptions] in the Oz documentation.

'''Catching exceptions'''

Exception are caught with pattern matching. Ellipsis indicating additional optional fields are often useful here.

```oz
try
   {Foo}
catch sillyError then
   {Bar}
[] slightlyLessSilly(data:D ...) then
   {Quux D}
[] _ then %% an unknown type of exception was thrown
   {Baz}
finally
   {Fin}
end
```



## PARI/GP

{{improve|PARI/GP|trap was deprecated in 2.6.  This should describe iferr.}}

### Catching errors in GP

The errors that can be trapped in GP are:
{| class="wikitable"
|<code>alarmer</code>
|generic error
|-
|<code>gdiver</code>
|division by 0
|-
|<code>invmoder</code>
|impossible modular inverse
|-
|<code>archer</code>
|not available on this architecture or operating system
|-
|<code>typeer</code>
|wrong type
|-
|<code>errpile</code>
|the PARI stack overflows
|-
|<code>talker</code>
|generic error
|-
|<code>user</code>
|User-initiated error
|}

```parigp
trap(/* specific error can be given here, or leave blank to catch all */,
  "caught"
,
  error("bad stuff")
)
```



### Throwing errors in GP

The only error that can be thrown in GP is user error:

```parigp
error("Text of error here")
```



### Throwing errors in PARI

Many more errors can be caught and thrown directly in PARI:
<div style="height:20ex; width: 40em; max-width: 100%; overflow: scroll; overflow-x: hidden">
{| class="wikitable"
|<code>0</code>
|Generic error
|-
|<code>talker2</code>
|?
|-
|<code>bugparier</code>
|Bug, please report
|-
|<code>alarmer</code>
|Generic error
|-
|<code>openfiler</code>
|File I/O
|-
|<code>talker</code>
|Generic error
|-
|<code>flagerr</code>
|Invalid flag
|-
|<code>impl</code>
|Not implemented
|-
|<code>archer</code>
|Not available on this system
|-
|<code>notfuncer</code>
|Not a function in function call
|-
|<code>precer</code>
|Precision too low
|-
|<code>typeer</code>
|Incorrect type
|-
|<code>consister</code>
|Inconsistent data
|-
|<code>user</code>
|User-initiated error
|-
|<code>errpile</code>
|Stack overflow
|-
|<code>overflower</code>
|Overflow
|-
|<code>matinv1</code>
|Non-invertible matrix (in gauss)
|-
|<code>mattype1</code>
|Not a square matrix
|-
|<code>arither1</code>
|Not an integer argument in an arithmetic function
|-
|<code>primer1</code>
|Not enough precomputed primes
|-
|<code>invmoder</code>
|Impossible inverse
|-
|<code>constpoler</code>
|Constant polynomial
|-
|<code>notpoler</code>
|Not a polynomial
|-
|<code>redpoler</code>
|Reducible polynomial
|-
|<code>zeropoler</code>
|Zero polynomial
|-
|<code>operi</code>
|"Impossible"
|-
|<code>operf</code>
|"Forbidden"
|-
|<code>gdiver</code>
|Division by zero
|-
|<code>memer</code>
|Not enough memory
|-
|<code>negexper</code>
|Negative exponent
|-
|<code>sqrter5</code>
|Non quadratic residue (in gsqrt)
|-
|<code>noer</code>
|Not an error...
|}
</div>

```C
pari_err(arither1, "functionName"); // Gives "*** functionName: not an integer argument in an arithmetic function"
```



### Catching errors in PARI

It is rare that this mechanism needs to be used in PARI, rather than standard [[#C|C]] methods, but the function <code>closure_trapgen</code> (similar to <code>closure_evalgen</code>) is available:

```C
GEN x = closure_trapgen(arither1, f); // Executes the function f, catching "not an integer argument in an arithmetic function" errors
if (x == (GEN)1L) // Was there an error?
  pari_printf("Don't do that!\n"); // Recover
```



## Pascal

See [[Exceptions#Delphi | Delphi]]


## Perl


'''Using eval'''

Exceptions using the core [http://perldoc.perl.org/functions/eval.html eval] function:


```perl
# throw an exception
die "Danger, danger, Will Robinson!";

# catch an exception and show it
eval {
    die "this could go wrong mightily";
};
print $@ if $@;

# rethrow
die $@;
```


See http://perldoc.perl.org/perlvar.html#%24EVAL_ERROR for the meaning of the special variable <tt>$@</tt>. See http://search.cpan.org/dist/Error for advanced object based-exception handling.

'''Using Try::Tiny'''

The same using the [http://search.cpan.org/perldoc?Try::Tiny Try::Tiny] module:


```perl
# throw an exception
die "Danger, danger, Will Robinson!";
```


```perl
# catch an exception and show it
try {
    die "this could go wrong mightily";
} catch {
    print;
};
```


```perl
# rethrow (inside of catch)
die $_;
```


'''Other styles'''

More complicated exception handling can be achieved in Perl using [http://search.cpan.org/perldoc?TryCatch TryCatch] or [http://search.cpan.org/perldoc?Exception::Class Exception::Class] modules.


## Perl 6

{{works with|rakudo|2015-09-10}}
The Perl 6 equivalent to Perl 5's eval {...} is try {...}. A try block by default has a CATCH block that handles all fatal exceptions by ignoring them. If you define a CATCH block within the try, it replaces the default CATCH. It also makes the try keyword redundant, because any block can function as a try block if you put a CATCH block within it.  The inside of a CATCH functions as a switch statement on the current exception.

```perl6
try {
    die "Help I'm dieing!";
    CATCH {
        when X::AdHoc { note .Str.uc; say "Cough, Cough, Aiee!!" }
        default { note "Unexpected exception, $_!" }
    }
}

say "Yay. I'm alive.";

die "I'm dead.";

say "Arrgh.";

CATCH {
    default { note "No you're not."; say $_.Str; }
}
```



```txt

HELP I'M DIEING!
Cough, Cough, Aiee!!
Yay. I'm alive.
No you're not.
I'm dead.

```


Perl 6 comes with [http://design.perl6.org/S04.html#Phasers phasers], that are called when certain conditions in the life of a program, routine or block are met. <tt>CATCH</tt> is one of them and works nicely together with <tt>LEAVE</tt> that is called even if an exception would force the current block to be left immediately. It's a nice place to put your cleanup code.


```perl6
sub f(){
        ENTER { note '1) f has been entered' }
        LEAVE { note '2) f has been left' }
        say '3) here be dragons';
        die '4) that happend to be deadly';
}

f();
say '5) am I alive?';

CATCH {
        when X::AdHoc { note q{6) no, I'm dead}; }
}
```



```txt
1) f has been entered
3) here be dragons
6) no, I'm dead
2) f has been left
```



## Phix

Phix provides try/catch and throw statements.

'''Throwing exceptions'''

You can throw any string (on it's own) or any integer, optionally with any (deeply nested) user_data that you like.

```Phix
throw("oh no")
throw(1)
throw(501,{"she",made[me],Do(it)})
```


'''Catching exceptions'''

There is one and only one non-optional catch clause per try statement.

The variable caught is a sequence, augmented with run-time diagnostics, with whatever was thrown in e[E_CODE] and/or e[E_USER].

```Phix
try
    one_of(these)
catch e
    if e[E_CODE]=501 then
        puts(1,"that's no excuse!\n")
    else
        throw(e)
    end if
end try
```

An uncaught exception terminates the program in error, otherwise control resumes in the catch clause or after the end try,
with no means (apart from some really nasty inline assembly) of resuming any half-finished block of code, and indeed any
call stack entries between a throw and a catch will already have been torn down and thrown away.

Traditionally fatal errors are re-routed via throw() when the presence of an exception handler is detected.

There is no finally construct (trivial to mimic with the introduction of a single simple boolean flag anyway).


## PHL


PHL does not support multiple catch-clauses.


```phl
module exceptions;

extern printf;

struct @MyException : @Exception {

};

@Void func throws ex [
	throw new @MyException;
]

@Integer main [
	try func();
	catch (e) {
		if (e::getType == "MyException") {
			printf("MyException thrown!\n");
		} else {
			printf("Unhandled exception!\n");
		}
	}
	return 0;
]
```



## PHP


{{works with|PHP|5.0+}}

Exceptions were not available prior to PHP 5.0

'''Define exceptions'''

```php
class MyException extends Exception
{
    //  Custom exception attributes & methods
}
```


'''Throwing exceptions'''

```php
function throwsException()
{
    throw new Exception('Exception message');
}
```


'''Catching Exceptions'''

```php
try {
    throwsException();
} catch (Exception $e) {
    echo 'Caught exception: ' . $e->getMessage();
}
```



## PicoLisp

[http://software-lab.de/doc/refC.html#catch catch], [http://software-lab.de/doc/refT.html#throw throw]
(and [http://software-lab.de/doc/refF.html#finally finally])
can be used for exception handling.
'throw' will transfer control to a 'catch' environment
that was set up with the given label.

```PicoLisp
(catch 'thisLabel          # Catch this label
   (println 1)             # Do some processing (print '1')
   (throw 'thisLabel 2)    # Abort processing and return '2'
   (println 3) )           # This is never reached
```

{{out}}

```txt
1        # '1' is printed
-> 2     # '2' is returned
```



## PL/I

<lang>
/* Define a new exception, called "my_condition". */
on condition (my_condition) snap begin;
   put skip list ('My condition raised.');
end;

/* Raise that exception */
signal condition (my_condition);

/* Raising that exception causes the message "My condition raised" */
/* to be printed, and execution then resumes at the statement      */
/* following the SIGNAL statement.                                 */

```



## PL/pgSQL


'''Raise an exception'''

```SQL

begin
   raise exception 'this is a generic user exception';
   raise exception division_by_zero;
end;

```


'''Handle an exception'''

Hande division by zero and re-raising once caught other exception:

```SQL

create function special_division(p_num double precision, p_den double precision) returns text
as $body$
begin
   return p_num/p_den::text;
EXCEPTION
   when division_by_zero then
      if p_num>0 then
         return 'Inf';
      ELSIF p_num<0 then
         return '-Inf';
      else
         return 'INDEF';
      end if;
   when others then
      raise;
end;

```



## Pop11


'''Throwing exceptions'''


```pop11
define throw_exception();
   throw([my_exception my_data]);
enddefine;
```


'''Catching exceptions'''


```pop11
define main();
   vars cargo;
   define catcher();
      ;;; print exception data
      cargo =>
   enddefine;
   catch(throw_exception, catcher, [my_exception ?cargo]);
enddefine;

main();
```



## PowerShell

'''Throw an exception:'''

```PowerShell

throw "Any error message."

```

{{Out}}

```txt

Any error message.
At line:1 char:1
+ throw "Any error message."
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : OperationStopped: (Any error message.:String) [], RuntimeException
    + FullyQualifiedErrorId : Any error message.

```

'''Throw a more specific exception:'''

```PowerShell

throw [System.IO.FileNotFoundException] ".\temp.txt not found."

```

{{Out}}

```txt

.\temp.txt not found.
At line:1 char:1
+ throw [System.IO.FileNotFoundException] ".\temp.txt not found."
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : OperationStopped: (:) [], FileNotFoundException
    + FullyQualifiedErrorId : .\temp.txt not found.

```

'''Using <code>try {} catch {}</code> is better for more complex error checking because you can test specific errors:'''

```PowerShell

try
{
    Get-Content -Path .\temp.txt
}
catch [System.IO.FileNotFoundException]
{
    Write-Host "File not found exception"
}
catch [System.Exception]
{
    Write-Host "Other exception"
}

```

{{Out}}

```txt

Get-Content : Cannot find path 'C:\Users\Owner\temp.txt' because it does not exist.
At line:4 char:5
+     Get-Content -Path .\temp.txt
+     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : ObjectNotFound: (C:\Users\Owner\temp.txt:String) [Get-Content], ItemNotFoundException
    + FullyQualifiedErrorId : PathNotFound,Microsoft.PowerShell.Commands.GetContentCommand

```

'''Errors are objects like any other in PowerShell, so you may capture any detail of it:'''

```PowerShell

$Error[0] | Get-Member

```

{{Out}}

```txt

   TypeName: System.Management.Automation.ErrorRecord

Name                  MemberType     Definition
----                  ----------     ----------
Equals                Method         bool Equals(System.Object obj)
GetHashCode           Method         int GetHashCode()
GetObjectData         Method         void GetObjectData(System.Runtime.Serialization.SerializationInfo info, System.Runtime.Serial...
GetType               Method         type GetType()
ToString              Method         string ToString()
writeErrorStream      NoteProperty   bool writeErrorStream=True
CategoryInfo          Property       System.Management.Automation.ErrorCategoryInfo CategoryInfo {get;}
ErrorDetails          Property       System.Management.Automation.ErrorDetails ErrorDetails {get;set;}
Exception             Property       System.Exception Exception {get;}
FullyQualifiedErrorId Property       string FullyQualifiedErrorId {get;}
InvocationInfo        Property       System.Management.Automation.InvocationInfo InvocationInfo {get;}
PipelineIterationInfo Property       System.Collections.ObjectModel.ReadOnlyCollection[int] PipelineIterationInfo {get;}
ScriptStackTrace      Property       string ScriptStackTrace {get;}
TargetObject          Property       System.Object TargetObject {get;}
PSMessageDetails      ScriptProperty System.Object PSMessageDetails {get=& { Set-StrictMode -Version 1; $this.Exception.InnerExcep...

```



## Prolog


```prolog
foo(X) :-
    \+ integer(X),
    throw(b('not even an int')).
foo(X) :-
    \+ between(1,10,X),
    throw(a('must be between 1 & 10')).
foo(X) :-
    format('~p is a valid number~n', X).

go(X) :-
    catch(
        foo(X),
        E,
        handle(E)).

handle(a(Msg)) :-
    format('~w~n', Msg),
    !.
handle(X) :- throw(X).
```

{{Out}}

```txt

?- go(1).
1 is a valid number
true.

?- go(5).
5 is a valid number
true.

?- go(11).
must be between 1 & 10
true.

?- go(test).
ERROR: Unhandled exception: b('not even an int')
?-

```



## PureBasic


```PureBasic
Procedure ErrorHandler()
  MessageRequester("Exception test", "The following error happened: " + ErrorMessage())
EndProcedure

MessageRequester("Exception test", "Test start")

OnErrorCall(@ErrorHandler())

RaiseError(#PB_OnError_InvalidMemory) ;a custom error# can also be used here depending on the OS being compiled for
```



## Python


'''Defining an exception'''


```python
import exceptions
class SillyError(exceptions.Exception):
    def __init__(self,args=None):
         self.args=args
```


Note: In most cases new exceptions are defined simply using the ''pass'' statement.  For example:


```python
class MyInvalidArgument(ValueError):
   pass
```


This example makes "MyInvalidArgument" an type of ValueError (one of the built-in exceptions). It's simply declared as a subclass of the existing exception and no over-riding is necessary.  (An except clause for ValueError would catch MyInvalidArgument exceptions ... but one's  code could insert a more specific exception handler for the more specific type of exception).

'''Throwing an exception'''

{{works with|Python|2.x and 3.x}}

Creating an exception using the default constructor of an exception class:

```python
def spam():
    raise SillyError # equivalent to raise SillyError()
```


{{works with|Python|2.5}}

Passing an argument to the constructor of an exception class:

```python
def spam():
    raise SillyError, 'egg' # equivalent to raise SillyError('egg')
```

The above syntax is removed in Python 3.0; but the following syntax works in Python 2.x and 3.x, so should be preferred.

{{works with|Python|2.x and 3.x}}

```python
def spam():
    raise SillyError('egg')
```


'''Handling an exception'''

{{works with|Python|2.5}}

try-except-else-finally


```python
try:
   foo()
except SillyError, se:
   print se.args
   bar()
else:
   # no exception occurred
   quux()
finally:
   baz()
```


Before Python 2.5 it was not possible to use finally and except together. (It was necessary to nest a separate ''try''...''except'' block inside of your ''try''...''finally'' block).

{{works with|Python|3.0}}

Note: Python3 will change the syntax of except slightly, but in a way that is not backwards compatible.  In Python 2.x and earlier the ''except'' statement could list a single exception or a tuple/list of exceptions and optionally a name to which the exception object will be bound.  In the old versions the exception's name followed a comma (as in the foregoing example).  In Python3 the syntax will become: ''except Exception1 [,Exception2 ...] '''as''' ExceptionName''

```python
try:
   foo()
except SillyError as se:
   print(se.args)
   bar()
else:
   # no exception occurred
   quux()
finally:
   baz()
```



## R


'''Define an exception'''

```r

e <- simpleError("This is a simpleError")

```


'''Raise an exception'''

```r

stop("An error has occured")
stop(e)                      #where e is a simpleError, as above

```


'''Handle an exception'''

```r

tryCatch(
  {
    if(runif(1) > 0.5)
    {
      message("This doesn't throw an error")
    } else
    {
      stop("This is an error")
    }
  },
  error = function(e) message(paste("An error occured", e$message, sep = ": ")),
  finally = message("This is called whether or not an exception occured")
)

```



## Racket



```racket

#lang racket

;; define a new exception type
(struct exn:my-exception exn ())

;; handler that prints the message ("Hi!")
(define (handler exn)
  (displayln (exn-message exn)))

;; install exception handlers
(with-handlers ([exn:my-exception? handler])
  ;; raise the exception
  (raise (exn:my-exception "Hi!" (current-continuation-marks))))

```



## Raven



```raven
42 as custom_error

define foo
    custom_error throw

try
    foo
catch
    custom_error =
    if  'oops' print
```



## REXX

While the REXX language doesn't have a ''throw'' capability ''per se'',
it does have the ability to catch exceptions (by label).

This type of exception handling (in REXX) has its limitation
(the label is local to the program, not external subroutines).

```rexx
/*REXX program demonstrates handling an exception (negative #); catching is via a label.*/
          do j=9  by -5
          say  'the square root of '       j       " is "       sqrt(j)
          end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure;  parse arg x;  if x=0  then return 0;        d=digits();    h=d+6;   m.=9
      numeric digits;           numeric form;                 if x<0  then signal .sqrtNeg
      parse value format(x, 2, 1, , 0) 'E0'  with  g 'E' _ .;             g=g * .5'e'_ % 2
         do j=0  while h>9;      m.j=h;               h=h%2 + 1;          end  /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g) * .5;     end  /*k*/
      numeric digits d;          return  g/1
.sqrtNeg: say  'illegal SQRT argument (argument is negative):'   x;       exit 13
```

'''output'''

```txt

the square root of  9  is  3
the square root of  4  is  2
illegal SQRT argument (argument is negative): -1

```



## Ring


```ring

Try
   see 1/0
Catch
   raise("Sorry we can't divide 1/0 + nl)
Done

```



## Ruby


'''Defining an exception'''


```ruby
# define an exception
class SillyError < Exception
end
```


SillyError is simply declared as a subclass of Exception. No over-riding is necessary.


```ruby
class MyInvalidArgument < ArgumentError
end
```


MyInvalidArgument is a type of ArgumentError (a built-in class). A rescue clause for ArgumentError would catch MyInvalidArgument exceptions ... but one's code could insert a more specific exception handler for the more specific type of exception.

'''Handling an exception'''


```ruby

# raise (throw) an exception
def spam
  raise SillyError, 'egg'
end

# rescue (catch) an exception
begin
  spam
rescue SillyError => se
  puts se  # writes 'egg' to stdout
end
```



```ruby
begin
  foo
rescue ArgumentError => e
  # rescues a MyInvalidArgument or any other ArgumentError
  bar
rescue => e
  # rescues a StandardError
  quack
else
  # runs if no exception occurred
  quux
ensure
  # always runs
  baz
end
```


ArgumentError is a type of StandardError, but Ruby uses the first matching "rescue" clause. So we never "quack" for an ArgumentError, but we only "bar" for it.

The "rescue" clause is like the "catch" clause in other languages. The "ensure" clause is like the "finally" clause in other languages.


```ruby
# short way to rescue any StandardError
quotient = 1 / 0 rescue "sorry"
```


The short form "a rescue b" returns a, but if a raises a StandardError, then it returns b. (ZeroDivisionError is a subclass of StandardError.)

'''Catch and throw'''

Ruby has a separate exception-like system that is meant to be used to exit out of deep executions that are not errors.


```ruby
def foo
    throw :done
end

catch :done do
    foo
end
```


With Ruby 1.8, you can only "throw" and "catch" symbols. With Ruby 1.9, you can throw and catch any object. Like exceptions, the throw can be made from a function defined elsewhere from the catch block.


## Rust

In Rust, there is no concept of "exception" per se.

Functions return a Result that can be either an error or a success, and the programmer can chose to panic on an error or not.
It is illustrated by the code below:


```Rust
// IO error is used here just as an example of an already existing
// Error
use std::io::{Error, ErrorKind};

// Rust technically doesn't have exception, but different
// types of error handling. Here are two examples of results.

fn valid_function() -> Result<usize, Error> {
    Ok(100)
}

fn errored_function() -> Result<usize, Error> {
    Err(Error::new(ErrorKind::Other, "Something wrong happened."))
}

// This should happen only when an unrecoverable error happened
fn panicking_function() {
    panic!("Unrecoverable state reached");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_function() {
        let result = match valid_function() {
            Ok(number) => number,
            Err(_) => panic!("This is not going to happen"),
        };
        assert_eq!(result, 100);
    }

    #[test]
    fn test_errored_function() {
        let result = match errored_function() {
            Ok(_) => panic!("This is not going to happen"),
            Err(e) => {
                assert_eq!(e.to_string(), "Something wrong happened.");
                0
            }
        };
        assert_eq!(result, 0);
    }

    #[test]
    #[should_panic]
    fn test_panicking_function() {
        panicking_function();
    }
}
```



## Scala

{{libheader|Scala}}
In Scala there is, thank heavens, no "checked exception" doctrine.
Exceptions can be freely implemented, it's the skill of the programmer to use them right.
This example is bad example of using exceptions, only to show that they can be used.

In there are 3 main entries: object CheckingAccount, CheckingBlockingAccount and NotImplementedErrorTest to selective start this solution and demonstrate the working of exceptions and handling.


```Scala
//Defining exceptions
class AccountBlockException extends Exception
class InsufficientFundsException(val amount: Double) extends Exception

class CheckingAccount(number: Int, var blocked: Boolean = false, var balance: Double = 0.0) {
  def deposit(amount: Double) { // Throwing an exception 1
    if (blocked) throw new AccountBlockException
    balance += amount
  }

  def withdraw(amount: Double) { // Throwing an exception 2
    if (blocked) throw new AccountBlockException
    if (amount <= balance) balance -= amount
    else throw new InsufficientFundsException(amount - balance)
  }
}

object CheckingAccount extends App {

  class ExampleException1 extends Exception

  val c = new CheckingAccount(101)
  println("Depositing $500...")
  try {
    c.deposit(500.00)
    println("\nWithdrawing $100...")
    c.withdraw(100.00)
    println("\nWithdrawing $600...")
    c.withdraw(600.00)
  } catch { // Exception handler
    case ac: InsufficientFundsException => println(s"Sorry, but you are short ${'$'} ${ac.amount}")
    case ac: AccountBlockException      => println("Account blocked.")

    ///////////////////////////// An example of multiple exception handler ////////////////////////
    case e@(_: ExampleException1 |
      _: InterruptedException) => println(s"Out of memory or something else.")

    case e: Exception => e.printStackTrace()
    case _: Throwable => // Exception cached without any action
  } finally println("Have a nice day")
}

object CheckingBlockingAccount extends App {
  val c = new CheckingAccount(102, true)
  println("Depositing $500...")
  try {
    c.deposit(500.00)
    println("\nWithdrawing $100...")
    c.withdraw(100.00)
    println("\nWithdrawing $600...")
    c.withdraw(600.00)
  } catch { // Exception handler
    case ac: InsufficientFundsException => println(s"Sorry, but you are short ${'$'} ${ac.amount}")
    case ac: AccountBlockException      => println("Account blocked.")
    case e: Exception                   => e.printStackTrace()
    case _: Throwable                   =>
  } finally println("Have a nice day")
}

object NotImplementedErrorTest extends App {
  ??? // Throws  scala.NotImplementedError: an implementation is missing
}
```


{{out}}Running entry point CheckingAccount
 Depositing $500...

 Withdrawing $100...

 Withdrawing $600...
 Sorry, but you are short $ 200.0
 Have a nice day
{{out}}Running entry point CheckingBlockingAccount
 Depositing $500...
 Account blocked.
 Have a nice day
{{out}}Running entry point NotImplementedErrorTest
 Exception in thread "main" scala.NotImplementedError: an implementation is missing
   at scala.Predef$.$qmark$qmark$qmark(Predef.scala:252)
   at NotImplementedErrorTest$delayedInit$body.apply(CheckingAccount.scala:53)
   .....


## Scheme

Exception handling can be created with any language supporting continuations, using as few primitves as possible, exception handling in Scheme can look like this. (But anyone wishing to continue using exceptions will abstract them into macros).
```scheme
(define (me-errors xx exception)
  (if (even? xx)
      xx
      (exception)))

;example that does nothing special on exception
(call/cc
  (lambda (exception)
    (me-errors 222 exception)
    (display "I guess everything is alright")))

;example that laments oddness on exception
(call/cc
  (lambda (all-ok) ;used to "jump" over exception handling

    (call/cc
      (lambda (exception-handle)
        (me-errors 333 exception-handle)
        (display "I guess everything is alright")
        (all-ok)))

    (display "oh my god it is ODD!")))
```



## Seed7

'''Raise an exception'''
```seed7
const proc: foo is func
  begin
    raise RANGE_ERROR;
  end func;
```


'''Handle an exception'''

```seed7
const proc: main is func
  begin
    block
      foo;
    exception
      catch RANGE_ERROR:
        writeln("catched RANGE_ERROR");
    end block;
  end func;
```



## Sidef

An exception is thrown by the ''die'' keyword, which, if not caught, it terminates the program with an appropriate exit code.

```ruby
try  {
    die "I'm dead!";        # throws an exception of type 'error'
}
catch { |type, msg|
    say "type: #{type}";    # type: error
    say "msg: #{msg}";      # msg: I'm dead! at test.sf line 2.
};

say "I'm alive...";
die "Now I'm dead!";        # this line terminates the program
say "Or am I?";             # Yes, you are!
```

{{out}}

```txt

type: error
msg: I'm dead! at test.sf line 2.
I'm alive...
Now I'm dead! at test.sf line 10.

```



## Slate

'''Handling Exceptions'''


```slate
se@(SceneElement traits) doWithRestart: block
[
  block handlingCases: {Abort -> [| :_ | ^ Nil]}
].
```


'''Define Exceptions'''


```slate
conditions define: #Abort &parents: {Restart}.
"An Abort is a Restart which exits the computation, unwinding the stack."

_@lobby abort
[
  Abort signal
].
_@(Abort traits) describeOn: console
[
  console ; 'Abort evaluation of expression\n'
].

"This will call:"
c@(Condition traits) signal
"Signalling a Condition."
[
  c tryHandlers
].
```


'''Throwing Exceptions'''

{{lines too long|Slate}}

```slate
(fileName endsWith: '.image') ifTrue: [error: 'Image filename specified where Slate source expected. Make sure you run slate with the -i flag to specify an image.'].
```



## Smalltalk

Throwing an Exception


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$*"
"exit"

Transcript show: 'Throwing yawp'; cr.
self error: 'Yawp!'.
```



```shell
$ ./yawp.st
Throwing yawp
Object: nil error: Yawp!
Error(Exception)>>signal (AnsiExcept.st:216)
Error(Exception)>>signal: (AnsiExcept.st:226)
UndefinedObject(Object)>>error: (AnsiExcept.st:1565)
UndefinedObject>>executeStatements (yawp.st:5)
```


Handling an Exception


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$*"
"exit"

[
	Transcript show: 'Throwing yawp'; cr.
	self error: 'Yawp!'.
] on: Error do: [ :e |
	Transcript show: 'Caught yawp'; cr.
].
```



```shell
$ ./yawp.st
Throwing yawp
Caught yawp
```



## SQL PL

{{works with|Db2 LUW}}
With SQL PL:

```sql pl

--#SET TERMINATOR @

BEGIN
 DECLARE numerator INTEGER DEFAULT 12;
 DECLARE denominator INTEGER DEFAULT 0;
 DECLARE result INTEGER;
 DECLARE overflow CONDITION for SQLSTATE '22003' ;
 DECLARE CONTINUE HANDLER FOR overflow
   RESIGNAL SQLSTATE '22375'
   SET MESSAGE_TEXT = 'Zero division';
 IF denominator = 0 THEN
  SIGNAL overflow;
 ELSE
  SET result = numerator / denominator;
 END IF;
END @

```

The next example just raise an exception, does not wrap a raised one.

```sql pl

BEGIN
 SIGNAL SQLSTATE '75002'
   SET MESSAGE_TEXT = 'Customer number is not known';
END @

```

Output:

```txt

$ db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB21034E  The command was processed as an SQL statement because it was not a
valid Command Line Processor command.  During SQL processing it returned:
SQL0438N  Application raised error or warning with diagnostic text: "Zero
division".  SQLSTATE=21543

db2 => BEGIN
db2 (cont.) =>  SIGNAL SQLSTATE '75001'
db2 (cont.) =>    SET MESSAGE_TEXT = 'Customer number is not known';
db2 (cont.) => END @
DB21034E  The command was processed as an SQL statement because it was not a
valid Command Line Processor command.  During SQL processing it returned:
SQL0438N  Application raised error or warning with diagnostic text: "Customer
number is not known".  SQLSTATE=75001

```



## Standard ML


'''Define Exceptions'''

```sml
exception MyException;
exception MyDataException of int; (* can be any first-class type, not just int *)
```


'''Throw Exceptions'''

```sml
fun f() = raise MyException;
fun g() = raise MyDataException 22;
```


'''Catch Exceptions'''

```sml
val x = f() handle MyException => 22;
val y = f() handle MyDataException x => x;
```



## Stata

In Stata, one can trap errors with the '''[http://www.stata.com/help.cgi?capture capture]''' command.

Without capture, if an error happens in a program, the execution of the current process terminates and the control is returned to the interactive mode. Execution may be terminated by the '''[http://www.stata.com/help.cgi?error error]''' or '''[http://www.stata.com/help.cgi?exit exit]''' commands, or conditionnally using '''[http://www.stata.com/help.cgi?assert assert]''' or '''[http://www.stata.com/help.cgi?confirm confirm]'''.

Many other commands may terminate the program if some error occurs during execution: for instance '''[http://www.stata.com/help.cgi?regress regress]''' will return code 2000 if a regressor has only missing values.

Example of usage:


```stata
capture confirm file titanium.dta
if _rc {
	if _rc==601 {
		display "the file does not exist"
	}
	else {
		* all other cases
		display "there was an error with return code " _rc
	}
}
```


Similarly, Mata has functions '''[http://www.stata.com/help.cgi?mf_error error]''' and '''[http://www.stata.com/help.cgi?mf_exit exit]''' to terminate execution, as well as '''[http://www.stata.com/help.cgi?mf_assert assert]''' and '''asserteq'''.


## Swift

{{works with|Swift|2.x+}}
'''Defining exceptions'''<br />
Exceptions can be of any type that conforms to the <code>ErrorType</code> protocol.

```swift
enum MyException : ErrorType {
  case TerribleException
}
```


'''Throw exceptions'''<br />
A function that throws an exception must be explicitly declared so:

```swift
func foo() throws {
  throw MyException.TerribleException
}
```


'''Catching exceptions'''

```swift
do {
  try foo()
} catch MyException.TerribleException { // this can be any pattern
  //Catch a specific case of exception
} catch {
  //Catch any exception
}
```



## Tcl


```tcl
package require Tcl 8.5

# Throw
proc e {args} {
    error "error message" "error message for stack trace" {errorCode list}
}

# Catch and rethrow
proc f {} {
    if {[catch {e 1 2 3 4} errMsg options] != 0} {
        return -options $options $errMsg
    }
}

f
```

This creates the stack trace

```txt
error message for stack trace
    (procedure "e" line 1)
    invoked from within
"e 1 2 3 4"
    (procedure "f" line 2)
    invoked from within
"f"
```



## TXR


Here is a complicated exceptions example straight from the manual.

This is a deliberately convoluted way to process input consisting of lines which have the form:
```txt
{monkey | gorilla | human} <name>
```


Some custom exceptions are defined, and arranged into a hierarchy via @(defex) directives.  An exception precedence hierarchy is established. A gorilla is a kind of ape, and an ape is a kind of primate. A monkey is a kind of primate, and so is a human.

In the main @(collect) clause, we have a try protect block in which we collect three different cases of primate. For each one, we throw an exception with the primate type symbol, and its name.  This is caught in the catch clause as the argument "name".  The catch clause performs another pattern match, @kind @name.   This match is being applied to exactly the same line of data for which the exception was thrown (backtracking!). Therefore the @kind variable will collect the primate type. However @name already has a binding since it is the argument of the catch. Since it has a value already, that value has to match what is in the data. Of course, it does since it was derived from that data. The data and the variable unify against each other.


```txr
@(defex gorilla ape primate)
@(defex monkey primate)
@(defex human primate)
@(collect)
@(try)
@(cases)
gorilla @name
@(throw gorilla name)
@(or)
monkey @name
@(throw monkey name)
@(or)
human @name
@(throw human name)
@(end)@#cases
@(catch primate (name))
@kind @name
@(output)
we have a primate @name of kind @kind
@(end)@#output
@(end)@#try
@(end)@#collect
```


Sample interactive run. Here the input is typed into standard input from the tty. The output is interleaved with the input, since TXR doesn't reads ahead only as much data as it needs.


```txt
$ txr primates.txr -
[TTY]human Harry
[TTY]gorilla Gordon
[OUT]we have a primate Harry of kind human
[TTY]monkey Mike
[OUT]we have a primate Gordon of kind gorilla
[TTY][Ctrl-D/EOF]
[OUT]we have a primate Mike of kind monkey

```



## Ursa

Catching exceptions:

```ursa
try
	invalid "this statement will fail"
catch syntaxerror
	# console.err is optional here
	out "caught an exception" endl console.err
end try
```

Throwing exceptions:

```ursa
throw (new ursa.exceptions.exception)
```



## Ursala


In this program fragment, a function named thrower returns the
string 'success' if its argument is non-empty, but otherwise
raises an exception with the diagnostic message 'epic fail'.
(The diagnostic message can also be made to depend on the input.)

```Ursala
#import std

thrower = ~&?/'success'! -[epic fail]-!%

catcher = guard(thrower,---[someone failed]-)
```


If the exception is not caught, the program terminates immediately and
the diagnostic is written to stderr. Alternatively, a calling function
or any caller thereof can be defined to catch an exception as shown. The
exception handler may inspect and arbitrarily
modify the diagnostic message, but normal execution may not be resumed.
In this example, the exception handler appends some additional
verbiage to the message.

## V


throwing exceptions

```v
[myproc
  ['new error' 1 2 3] throw
  'should not come here' puts
].
```


catching them


```v
[myproc] [puts] catch
=[new error 1 2 3]
```



## Visual Basic .NET


'''Defining exceptions'''

```vbnet
Class MyException
  Inherits Exception
  'data with info about exception
End Class
```



'''Throw exceptions'''

```vbnet
Sub foo()
    Throw New MyException
End Sub
```


'''Catching exceptions'''

```vbnet
Sub bar()
    Try
        foo()
    Catch e As MyException When e.Data.Contains("Foo")
        ' handle exceptions of type MyException when the exception contains specific data
    Catch e As MyException
        ' handle exceptions of type MyException and derived exceptions
    Catch e As Exception
        ' handle any type of exception not handled by above catches
    Finally
        'code here occurs whether or not there was an exception
    End Try
End Sub
```



## VBA


For historical reasons, Exceptions are called 'Errors' in VBA and VB Classic.
VBA inherited several distinct exception handling models, which may be freely mixed and matched.
The major limitations are that nested Try/Catch blocks must be constructed by the user, and that the User Defined Labels required for the Catch/Finally blocks may not be reused within a subroutine. For these reasons, it is conventional to only have only 1 Try/Catch block per subroutine.

'''Throw exceptions'''

```vb
Sub foo1()
    err.raise(vbObjectError + 1050)
End Sub

Sub foo2()
    Error vbObjectError + 1051
End Sub

```


'''Catching exceptions'''

```vb
Sub bar1()
'by convention, a simple handler
    On Error GoTo catch
    foo1
    MsgBox " No Error"
    Exit Sub
catch:
    'handle all exceptions
    MsgBox Err.Number & vbCrLf & Err.Description
    Exit Sub
End Sub

Sub bar2()
'a more complex handler, illustrating some of the flexibility of VBA exception handling
    On Error GoTo catch
100     foo1
200     foo2
    'finally block may be placed anywhere: this is complexity for it's own sake:
   GoTo finally
catch:
   If Erl = 100 Then
      ' handle exception at first line: in this case, by ignoring it:
      Resume Next
   Else
      Select Case Err.Number
         Case vbObjectError + 1050
             ' handle exceptions of type 1050
             MsgBox "type 1050"
         Case vbObjectError + 1051
             ' handle exceptions of type 1051
             MsgBox "type 1051"
         Case Else
             ' handle any type of exception not handled by above catches or line numbers
             MsgBox Err.Number & vbCrLf & Err.Description
      End Select
      Resume finally
  End If
finally:
   'code here occurs whether or not there was an exception
   'block may be placed anywhere
   'by convention, often just a drop through to an Exit Sub, rather tnan a code block
   GoTo end_try:

end_try:
    'by convention, often just a drop through from the catch block
End Sub
```



## zkl


```zkl
try{ throw(Exception.BadDay) }
catch { println(__exception," was thrown") }
fallthrough {  println("No exception was thrown") }
println("OK");
```

{{out}}

```txt

BadDay(I'm having a bad day) was thrown
OK

```

If you want "finally" functionality, use onExit or onExitBlock:

```zkl
fcn f(b){
   try{
      onExitBlock("Exit code".println);
      if (b) throw(Exception.BadDay)
   }
   catch{ println(__exception," was thrown") }
   fallthrough{  println("No exception was thrown") }
   println("OK");
}
f(False); println("--------");
f(True);
```

{{out}}

```txt

Exit code
No exception was thrown
OK
--------
Exit code
BadDay(I'm having a bad day) was thrown
OK

```

