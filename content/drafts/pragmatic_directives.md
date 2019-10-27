+++
title = "Pragmatic directives"
description = ""
date = 2019-06-20T10:10:07Z
aliases = []
[extra]
id = 10639
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Pragmatic directives]]

Pragmatic directives cause the language to operate in a specific manner,   allowing support for operational variances within the program code   (possibly by the loading of specific or alternative modules).


;Task:
List any pragmatic directives supported by the language,   and demonstrate how to activate and deactivate the pragmatic directives and to describe or demonstrate the scope of effect that the pragmatic directives have within a program.





## Ada

Some common language defined pragmas:
* pragma assert(expression, error_message)
* pragma Import(...) and pragma Export(...) to interface other languages, commonly C
* pragma Inline(function_name) perform inline expansion of the function
* pragma Optimize(Time/Space/Off) Implementation defined, attempts to optimize memory usage for speed or time.
* pragma Pack(type) attempts to minimize memory usage for the type, even if it means slower memory access. A representation clause specifying bit size is usually used instead of this.
* pragma Suppress(identifier) and pragma Unsuppress(identifier) for enabling/disabling any of the many language checks.
Some pragmas are also implementation defined, the commonly used GNAT provides many, such as:
* pragma Unreferenced(name) suppresses warnings about unused entities, and raises warnings if they are in fact referenced. 
There are far too many pragmas to list here, but a standard informative list can be found in Annex L of the documentation if you have it installed. Or found at:
* [http://www.adaic.org/resources/add_content/standards/05rm/html/RM-L.html Annex L - Language-Defined Pragmas]
* [http://gcc.gnu.org/onlinedocs/gnat_rm/Implementation-Defined-Pragmas.html GNAT Implementation Defined Pragmas]



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - '''pragma'''s are permitted by the standard, e.g. "portcheck" is recommended for detecting language extensions, other '''pragma''' options are implementation specific.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.5 algol68g-2.3.5] - most compiler directives are permitted as '''pragma'''s options - also ' '''pr''' read "filename.a68" '''pr''' ' is permitted to "include" a file.}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to implementation specific PRAGMA.}}
'''File: Pragmatic_directives.a68'''
```algol68
#!/usr/local/bin/a68g --script #

PRAGMAT portcheck PRAGMAT
PR portcheck PR

BEGIN PR heap=256M PR # algol68g pragma #
  ~
END;

PROC (REAL)REAL s = sin();

SKIP
```
'''Output:'''

```txt

10    PROC (REAL)REAL s = sin();
                             1  
a68g: warning: 1: generic-argument is an extension (detected in particular-program).

```



## BASIC


Some versions of basic support the use of system trace directives that allow the program line or line number to be output.

{{works with|BBC BASIC}}
{{works with|GWBASIC}}


```basic
10 TRON: REM activate system trace pragma
20 TROFF: REM deactivate system trace pragma
```

'''
'''


## C

The C Preprocessor is well explained on the [https://gcc.gnu.org/onlinedocs/cpp/index.html#Top GNU] site. The pragma page is [https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html here].

```C

/*Almost every C program has the below line, 
the #include preprocessor directive is used to 
instruct the compiler which files to load before compiling the program.

All preprocessor commands begin with #
*/
#include<stdio.h> 

/*The #define preprocessor directive is often used to create abbreviations for code segments*/
#define Hi printf("Hi There.");

/*It can be used, or misused, for rather innovative uses*/

#define start int main(){
#define end return 0;}

start

Hi

/*And here's the nice part, want your compiler to talk to you ? 
Just use the #warning pragma if you are using a C99 compliant compiler
like GCC*/
#warning "Don't you have anything better to do ?"

#ifdef __unix__
#warning "What are you doing still working on Unix ?"
printf("\nThis is an Unix system.");
#elif _WIN32
#warning "You couldn't afford a 64 bit ?"
printf("\nThis is a 32 bit Windows system.");
#elif _WIN64
#warning "You couldn't afford an Apple ?"
printf("\nThis is a 64 bit Windows system.");
#endif

end

/*Enlightened ?*/

```

On compilation and output, the compiler type is detected rather than the actual OS :

```txt

C:\rosettaCode>gcc pragmatic.c
pragmatic.c:22:2: warning: #warning "Don't you have anything better to do ?"
pragmatic.c:28:2: warning: #warning "You couldn't afford a 64 bit ?"

C:\rosettaCode>a
Hi There.
This is a 32 bit Windows system.

```



## Common Lisp


Common Lisp usually works with a runtime system which contains an incremental compiler and a file compiler. Various ways are possible to influence the runtime system and the compiler. 


###  Declarations 


* a data allocation can be declared to be done on the stack (if the compiler supports that)
* the optimization settings for the compiler can be set for these qualities: debug, safety, space, speed, compilation speed
* functions can be declared for inline compilation (if the compiler supports that)


###  Feature Expressions 


Common Lisp provides a way for conditional reading or ignoring of expressions. A variable lists the available features of an implementation.

Clozure Common Lisp:
 ? *features*
 (:EASYGUI :ASDF2 :ASDF :HEMLOCK :APPLE-OBJC-2.0 :APPLE-OBJC :PRIMARY-CLASSES :COMMON-LISP :OPENMCL :CCL :CCL-1.2 :CCL-1.3 :CCL-1.4 :CCL-1.5 :CCL-1.6 :CCL-1.7 :CCL-1.8 :CLOZURE :CLOZURE-COMMON-LISP :ANSI-CL :UNIX :OPENMCL-UNICODE-STRINGS :OPENMCL-NATIVE-THREADS :OPENMCL-PARTIAL-MOP :MCL-COMMON-MOP-SUBSET :OPENMCL-MOP-2 :OPENMCL-PRIVATE-HASH-TABLES :X86-64 :X86_64 :X86-TARGET :X86-HOST :X8664-TARGET :X8664-HOST :DARWIN-HOST :DARWIN-TARGET :DARWINX86-TARGET :DARWINX8664-TARGET :DARWINX8664-HOST :64-BIT-TARGET :64-BIT-HOST :DARWIN :LITTLE-ENDIAN-TARGET :LITTLE-ENDIAN-HOST)

These features can then use to conditionally execute code:

  #+UNIX(print "this is a unix system")

Above would execute the print statement only if the Lisp system has a feature called UNIX.

  #-COCOA(require "cocoa")

Above would check for the COCOA feature and if not present, load the library. The library is also expected to later push the feature COCOA to the features list. Reading this code then later would ignore the require statement.


## D

The -d compiler switch allows deprecated D features in a program. It allows some deprecated features of C language, and user code wrapped inside deprecated{}.


## Erlang

Erlang pragmas are called module attributes. The compile attribute, ex:

```txt

-compile( [compressed, {inline,[pi/0]}] ).

```

will add the compiler option 'compressed' when the module is compiled and inline the function pi/0.

The file module attribute look like

```txt

-file( "kalle", 98 ).

```

and will change the ?FILE macro from the file name to "kalle" and the ?LINE macro from the line number to 98.

More module attributes at [http://www.erlang.org/doc/reference_manual/modules.html].


## Go

Go has a feature called [http://golang.org/pkg/go/build/#overview build constraints] that work on the level of whole files.  A comment line reading


```go>// +build <expression></lang


will cause the entire file to be excluded from a build unless <expression> is true.  The elements, called tags, in the expression are typically things like the target operating system or hardware architecture.  For example


```go>// +build linux</lang


will include the file if the target OS is linux, but will exclude the file otherwise.  Arbitrary tags can be passed on the command line of the go command.  A file could begin


```go>// +build Tuesday</lang


and the build command


```bash
go install -tags `date +%A`
```


would only include the file on Tuesdays.

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon have a number of pragmatic modes.  Most of these are controlled via keywords (See [[Special_variables#Icon_and_Unicon]])).

```Icon
&trace # controls execution tracing
&error # controls error handling
```


Additionally, tracing can be controlled via the environment variable 'TRACE'.


## J


J's [http://www.jsoftware.com/help/dictionary/dx009.htm foreign global parameters] possibly qualify as pragmatic directives.

They are analogous to setting (or reading) variables.

Here's a list of the settings:

 9!:1 random seed (incomplete specification of state -- see 9!:45)
 9!:3 default display for non-nouns
 9!:7 box drawing characters
 9!:9 error messages
 9!:11 print precision
 9!:17 centering (or not) when box contents are smaller than boxes 
 9!:19 comparison tolerance
 9!:21 memory limit
 9!:25 security level
 9!:27 text of immediate execution phrase
 9!:29 enable immediate execution phrase
 9!:33 execution time limit
 9!:35 disable (or re-enable) assertions
 9!:37 output control
 9!:39 locales' hash table size
 9!:41 retain (or not) whitespace and comments in explicit definitions
 9!:43 which random number generator to use?
 9!:45 what is the current state of that rng?
 9!:49 enable reserved words for argument names 

For example,


```j
   9!:25]1
```


disables access to the file system (and disables some other features, including the ability to exit the program, because the system exit mechanism is a system feature and thus not trusted).  You cannot turn this off after it has been turned on (so you will need to shut down J and start it again if you want to use those features).

Or, for example, y is the usual name for the right argument of a verb.


```j
  3 :'y' 8
8
```


But y is also a regular variable.  So J also offers a reserved word y. which serves the same role.  But this is disabled by default (because mostly it's just an unnecessary complication).


```j
  3 :'y.' 8
|spelling error
```


But you can enable the reserved word mechanism:


```j
   9!:49]1
   3 :'y.' 8
8
   3 :'y.[y=.7' 8
8
   3 :'y.]y=.7' 8
7
```



## Julia

Julia has a number of macros which act as compiler directives.  For example, the <code>@inbounds</code> macro disables runtime checking of array bounds within a block of code marked by this macro, which sometimes improves performance. Example:

```julia
x = rand(100, 100)
y = rand(100, 100)

@inbounds begin
    for i = 1:100
        for j = 1:100
            x[i, j] *= y[i, j]
            y[i, j] += x[i, j]
        end
    end
end

```



## Kotlin

The closest things which Kotlin has to pragmatic directives are annotations which always begin with the @ character. For example, the following code would normally produce a compiler warning that the variable 's' is never used. However, the presence of the @Suppress annotation suppresses the warning:

```scala
// version 1.0.6

@Suppress("UNUSED_VARIABLE")

fun main(args: Array<String>) {
    val s = "To be suppressed"
}
```



## Mathematica


Mathematica makes no formal difference between any normal and "specific" operation of the language. 
Any possible desired effect can be achieved by calling a function or setting a variable. 
Function calls are traced using the Trace[] function.


## NetRexx

NetRexx provides three pragma-like instructions: <tt>OPTIONS</tt>, <tt>NUMERIC</tt> and <tt>TRACE</tt>.
* '''<tt>OPTIONS</tt>''' provides the ability to pass special requests to the language processor (i.e. a compiler or interpreter).
:The syntax is:
```NetRexx>options wordlist;</lang

:where ''wordlist'' is one or more symbols separated by blanks. The individual words in ''wordlist'' might control optimizations, enforce standards, enable implementation-dependent features, etc.
:The current default settings of <tt>OPTIONS</tt> is:
```NetRexx
options -
  nobinary nocomments nocompact console crossref decimal nodiag noexplicit noformat java logo noreplace nosavelog nosourcedir -
  nostrictargs nostrictassign nostrictcase nostrictimport nostrictprops nostrictsignal nosymbols trace2 noutf8 verbose3
```

* '''<tt>NUMERIC</tt>''' is used to change the way in which arithmetic operations are carried out by a program.
:The syntax is:
```NetRexx
numeric digits [exprd];
numeric form [scientific | engineering];
```

:*'''<tt>numeric digits</tt>''' controls the precision under which arithmetic operations will be evaluated. The default for ''exprd'' is '''9''' i.e. '''<tt>numeric digits 9</tt>'''.<br />There is normally no limit to the value for <tt>'''numeric digits'''</tt> (except the constraints imposed by the amount of storage and other resources available) but note that high precisions are likely to be expensive in processing time.
:*'''<tt>numeric form</tt>''' controls which form of exponential notation is to be used for the results of operations.
* '''<tt>TRACE</tt>''' is used to control the tracing of the execution of NetRexx methods, and is primarily used for debugging.
:The Syntax is:
```NetRexx
trace tracesetting
trace var [varlist]
```

:where ''tracesetting'' is one of:
:*'''<tt>all</tt>'''
::All clauses (except null clauses without commentary) which are in methods and which are executed after the trace instruction will be traced.
:*'''<tt>methods</tt>'''
::All <tt>method</tt> clauses in the class will be traced when the method they introduce is invoked, together with the values of the arguments passed to each method.
:*'''<tt>off</tt>'''
::turns tracing off.
:*'''<tt>results</tt>'''
::similar to <tt>trace all</tt> for the clauses in a <tt>method</tt> with the addition that the results of all ''expression'' evaluations and any results assigned to a variable by an assignment, <tt>loop</tt>, or <tt>parse</tt> instruction are also traced.
:and ''varlist'' provides a list of variables which will be monitored during execution.


## Perl


By convention pragmatic modules are named using lowercase letters.

;List of pragmatic modules:

* diagnostics
* english
* feature
* integer
* lib
* ops
* sort
* strict
* switch
* warnings

;Utilization:

Pragmatic modules have local scope and are utilized using the use directive:


```perl
use warnings;  # use warnings pragma module
use strict;    # use strict pragma module
```


To disable behaviour of a pragmatic module:


```perl
no warnings;   # disable warnings pragma module
no strict;     # disable strict pragma module
```


## Perl 6

{{works with|rakudo|2015-10-20}}
The Perl 6 pragma mechanism is nearly identical to Perl 5's, piggybacking on the notation for importing modules (pragmas are distinguished by case from normal modules, which are generally of mixed case).  By convention pragmas are lowercase, unless they are indicating the use of an unsafe feature, in which case they are in all caps.

```perl6
use MONKEY-TYPING;
augment class Int {
    method times (&what) { what() xx self }  # pretend like we're Ruby
}
```

Unlike Perl 5, there is no <tt>use strict;</tt> pragma, however, since Perl 6 is strict by default.  Importation of a pragma is lexically scoped as in Perl 5, but note that unlike in Perl 5, <i>all</i> importation is lexical in Perl 6, so pragmas are not special that way.


## Phix

The following are taken directly from the Phix.syn (syntax colouring) file, which can be edited as needed (for errors or new compiler features): 

Delimiters #$:.%\^ 

Operators , = := == != < <= > >= @= @== @!= @< @<= @> @>= + - * / += -= *= /= @+= @-= @*= @/= .. & &= ? ; : | 

Braces ()[]{} 

BlockComment /* */ --/* --*/ 

LineComment -- 

TokenStart abcedfghijklmnopqrstuvwxyz 

TokenStart ABCDEFGHIJKLMNOPQRSTUVWXYZ_ 

TokenChar 0123456789 

Escapes \rnt\'"eE#x0buU 


The last line means that escapes in string literals start with a backslash, and there are 14 of them: CR, LF, 
TAB, backslash, single and double quotes, escape (#1B, e and E allowed), hex byte (# and x allowed), NUL,
backspace, and 4 and 8-digit unicode characters.

(The above is further explained on [[[[Special_characters#Phix|Special_characters]]]])

The include directive causes the compiler to stop processing the current file and instead start compiling the
specified file, returning to the next line when done.

The #ilASM{} directive contains inline assembly, which can contain PE/ELF/32/64 guards to control the exact
code emitted (mainly for low-level system routines, such as file I/O, which are usually in builtins\VM).

The #isginfo{}, #isinit{}, and #istype{} directives instruct the compiler to perform various type-inference 
and legal value ranges checks. Primarily for compiler development use, not end user applications. No code
is generated, but compilation will abort if they fail.

The with/without directives control several run-time options: 

profile         -- produce an execution count listing when the program terminates

profile_time    -- produce an execution percentage listing when the program terminates

type_check      -- turn user-defined type checking on or off (can make it noticeably faster, once testing is done)

trace           -- allow or disallow debugging (source code line-by-line tracing) 

debug           -- turn debugging info generation on or off 


The last two are related: without debug completely removes all tracing and diagnostics for a specific file (primarily intended for use in well-tested system routines), whereas (under with debug, which is the default) with/without trace can make debugging less tedious by not stopping on every line of irrelevant (user-selected) code, although dumps still contain a full call stack.

with/without console/gui still work but should now be replaced with a format directive:

format PE32|PE64|ELF32|ELF64 - you can also specify gui/console, subsystem version, icons and manifest files. Ignored when interpreting. 

The following are ignored by the compiler, but are respected by the source code reindent tool: 

--#without reformat 

--#with reformat 

--#withdef              (may be needed when reindenting OpenEuphoria code) 

--#withtype 


Lastly note that an abort() statement at the top level will make the compiler ignore the rest of the file.


## PicoLisp

PicoLisp makes no formal difference between any normal and "specific" operation of the language. Any possible desired effect can be achieved by calling a function or setting a variable. For example, function calls can be traced with the '[http://software-lab.de/doc/refT.html#trace trace]' function.

/* {{header|PL/I}} */ Section added -- Superfluous blanks suppressed


## PL/I

Disabled on-conditions can be seen as pragmatic directives.

'''SubscriptRange''' (SubRg), '''StringRange''' (StRg), 
'''StringSize''' (StRz), '''Size''' conditions 
can be enabled for a '''procedure''' or a '''begin''' block.

By default, for optimization reasons, 
SubscriptRange,StringRange,StringSize,Size are disabled.

If enabled the '''SubscriptRange''' condition is raised for out-of-bound subscript of 
an array.
For example:

```pli
    declare (t(100),i) fixed binary;
    i=101;
    t(i)=0;
```

will cause unpredictible results.
And :

```pli
 (SubscriptRange): begin;
    declare (t(100),i) fixed binary;
    i=101;
    t(i)=0;
 end;
```

will issue the message : "The SubscriptRange condition was raised." at execution time. 

Or, it can be handle by an on-unit.

```pli
 (SubscriptRange): begin;
    declare (t(100),i) fixed binary;
    i=101;
    on SubscriptRange begin; put skip list('error on t(i)'); goto e; end;
    t(i)=0;
    e:on SubscriptRange system; /* default dehaviour */
 end;
```


And the same way for a string,  
the '''StringRange''' condition is raised when a substring reference is beyond 
the span of the string.

And finally: 

The '''StringSize''' condition is raised when a string is shorten 
as a result of a convertion assigment.

The '''Size''' condition is raised when a numerical value is shorten 
as a result of a convertion assigment.


## PowerShell

The #Requires statement prevents a script from running unless the Windows 
PowerShell version, modules, snap-ins, and module and snap-in version
prerequisites are met. If the prerequisites are not met, Windows PowerShell
does not run the script. 

You can use #Requires statements in any script. You cannot use them in 
functions, cmdlets, or snap-ins. 

```PowerShell

#Requires -Version <N>[.<n>] 
#Requires –PSSnapin <PSSnapin-Name> [-Version <N>[.<n>]]
#Requires -Modules { <Module-Name> | <Hashtable> } 
#Requires –ShellId <ShellId>
#Requires -RunAsAdministrator

```

For a full description:

```PowerShell

Get-Help about_Requires

```



## Python

Python has the [http://docs.python.org/py3k/library/__future__.html __future__] module which controls certain features:

;Python 3.2:

```python
Python 3.2 (r32:88445, Feb 20 2011, 21:30:00) [MSC v.1500 64 bit (AMD64)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> import __future__
>>> __future__.all_feature_names
['nested_scopes', 'generators', 'division', 'absolute_import', 'with_statement', 'print_function', 'unicode_literals', 'barry_as_FLUFL']
>>> 
```


('barry_as_FLUFL' is an April fools [http://www.python.org/dev/peps/pep-0401/ joke])

;Python 2.7:

```python
Python 2.7.2 (default, Jun 12 2011, 14:24:46) [MSC v.1500 64 bit (AMD64)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> import __future__
>>> __future__.all_feature_names
['nested_scopes', 'generators', 'division', 'absolute_import', 'with_statement', 'print_function', 'unicode_literals']
>>> 
```



## Racket


Racket eschews pragmas that are specified outside of the language.  However, one can view Racket's <tt>#lang</tt> mechanism as a much more powerful tool for achieving similar things.  For example, normal code starts with <tt>#lang racket</tt> -- giving you a very Scheme-like language; change it to <tt>#lang typed/racket</tt> and you get a similar language that is statically typed; use <tt>#lang lazy</tt> and you get a Racket-like language that has lazy semantics; use <tt>#lang algol60</tt> and you get something that is very different than Racket.  (And of course, you can implement your own language quite easily.)


## REXX

 The REXX language has several pragmatic statements:
    <big>∙</big>   NUMERIC DIGITS   {nnn}
    <big>∙</big>   NUMERIC FORM     {ENGINEERING │ SCIENTIFIC}
    <big>∙</big>   NUMERIC FUZZ     {nnn}
    <big>∙</big>   OPTIONS          {xxx yyy zzz}
    <big>∙</big>   TRACE            {options}
    <big>∙</big>   SIGNAL           {ON │ OFF}   LOSTDIGITS
    <big>∙</big>   SIGNAL           {ON │ OFF}   NOVALUE
    <big>∙</big>   SIGNAL           {ON │ OFF}   SYNTAX
    <big>∙</big>   SIGNAL │ CALL    {ON │ OFF}   ERROR
    <big>∙</big>   SIGNAL │ CALL    {ON │ OFF}   FAILURE
    <big>∙</big>   SIGNAL │ CALL    {ON │ OFF}   HALT
    <big>∙</big>   SIGNAL │ CALL    {ON │ OFF}   NOTREADY


### numeric digits

The   '''NUMERIC DIGITS nnn'''   statement is used to specify to the REXX interpreter how many

(significant) decimal digits are to be used in calculating and storing numbers.

'''nnn'''   can be an expression that evaluates to a positive integer.

If   '''nnn'''   is omitted, it defaults to   '''9'''.

If no   '''numeric digits'''   statement is used,   the default is   '''9'''.

It must be greater than the (current)   '''NUMERIC FUZZ'''   setting.


### numeric fuzz

The   '''NUMERIC FUZZ nnn'''   statement is used to specify to the REXX interpreter how many

decimal digits   (at full precision)   will be ignored while performing an arithmetic

comparison. 

'''nnn'''   can be an expression that evaluates to a non-negative integer. 

If   '''nnn'''   is omitted,   it defaults to   '''0'''.

If no   '''numeric fuzz'''   statement is used,   the default for REXX programs is   '''0'''.

It must be less than the (current)   '''NUMERIC DIGITS'''   setting.

The result of using a positive value for   '''FUZZ'''   is that the REXX interpreter (temporarily) reduces

the number of   '''NUMERIC DIGITS'''   by the   '''FUZZ'''   value before an arithmetic comparison.

This means that arithmetic comparisons are performed with the precision of

'''DIGITS()'''   minus   '''FUZZ()'''   where:
::::*   DIGITS()   is the value of   '''numeric digits'''
::::*   FUZZ()     is the value of   '''numeric fuzz'''


### numeric form

The   '''NUMERIC FORM'''   statement is used to cause the REXX interpreter to use a specific form of

exponential format   if   the result of an arithmetic operation requires the use of exponential notation

with the current value of   '''numeric digits'''.

The specification option after   '''numeric form'''   can be:
::::*   ENGINEERING
::::*   SCIENTIFIC
::::*   (or omitted)

The default is   '''scientific'''.

The option can be in upper/lower/mixed case.

If no   '''numeric form'''   statement is used,   the default for REXX programs is   '''scientific'''.


### options

The   '''OPTIONS'''   statement is used to specify to the REXX interpreter on such matters on how to

process the source (statements), possibly (for instance) whether or not   ''double byte character strings''  

are present, or possibly cause the REXX interpreter to force compliance to some particular rule or

REXX (program) coding standards.

There can be any number of options listed   (or none).

Each particular REXX interpreters have their own   '''options''',   so it isn't considered an error if some

option isn't supported (or recognized) by another REXX interpreter.

Some options are global in nature, others can be enabled and disabled.

Some REXX interpreters also have a way to specify certain options via the   ''command-line''  

(also known as the   ''C.L.'').


### trace

The   '''TRACE'''   statement is used to cause the REXX interpreter to turn   ''off''   or ''on''   certain tracing

facilities for the REXX interpreter.

Most tracing options causes some sort of output   (tracing of statements or values of clauses)   to be

emitted to the console (terminal).

The output (tracing information) written to the terminal is usually quite distinctive and can be easily

recognized.

There are a number of options for the   '''trace'''   instruction, and they won't be explained here.


### signal on lostdigits

If the REXX interpreter detects that a result of any arithmetic operation results in the loss of any

decimal digits,   control branches to the label     '''lostdigits'''.   The label may be in mixed case.

Not all REXXes support this condition (option).

This condition is raised when the number of significant decimal digits in the result of an arithmetic

operation that would exceed the currently defined number of digits via   '''numeric digits'''.


### signal off lostdigits

This indicates to take the default action which is quietly ignore the condition and continue

execution of the REXX program.


### signal on novalue

If the REXX interpreter detects an uninitialized variable is used in an evaluated expression,   control

branches to the label     '''novalue'''.   The label may be in mixed case.


### signal off novalue

This indicates to take the default action which is to quietly return the value of the uppercase version

of the variable name and continue execution of the REXX program.


### signal on syntax

If the REXX interpreter detects a   syntax   error in the REXX program,   (and the REXX interpreter

determines that the error can still be handled by the erroneous REXX program),   control branches

to the label     '''syntax'''.   The label may be in mixed case.


### signal off syntax

This indicates to take the default action which REXX issues an error message and terminates the

REXX program.


### signal on error; call on error

If the REXX interpreter detects a   non-zero   return code from a host system command issued by

the REXX program,   control branches to the label     '''error'''.   The label may be in mixed case.


### signal off error; call off error

This indicates to take the default action which means the special variable   '''RC'''   ('''R'''eturn '''C'''ode)  

is quietly defined,   and execution continues of the REXX program.


### signal on failure; call on failure

If the REXX interpreter detects a   failure   from a host system command issued by the REXX

program,   control branches to the label     '''failure'''.   The label may be in mixed case.


### signal off failure; call off failure

This indicates to take the default action which means the special variable   '''RC'''   ('''R'''eturn '''C'''ode)  

is quietly defined,   and execution continues of the REXX program.


### signal on halt; call on halt

If the REXX interpreter detects an   external interrupt   is made to interrupt execution of the REXX

program,   control branches to the label     '''halt'''.   The label may be in mixed case.

The   ''external interrupt''   varies with which operating system is being used.


### signal off halt; call off halt

This indicates to take the default action which normally means a message is issued and the

execution of the REXX program is terminated.


### signal on notready; call on notready

If the REXX interpreter detects some kind of problem with stream I/O   (this varies with each

REXX interpreter),   control branches to the label     '''notready'''.   The label may be in mixed case.


### signal off notready; call off notready

This indicates to take the default action which is to quietly resume execution of the REXX program.

Not all REXXes support this condition (option). 


### scope

The scope for the all the above statements   (except for '''options'''),   if issued in the main program,

will be in effect for all internal routines (subroutines/functions/procedures).

If the above statements are issued in an internal routine, upon return from that routine,   the 

original status is restored   (to just before the invoke of that routine).

For external routines, the defaults are used. 




## Scala


### tailrec


```Scala
@inline
@tailrec
```



## Tcl

Mostly Tcl avoids anything like pragmas as they are a source of portability trouble. However, certain global variables can be set to induce non-standard behaviors:

;tcl_precision
:This is used to control how Tcl converts floating point numbers to strings, and represents the number of significant figures to use. From Tcl 8.5 onwards this should be left at its default (which uses the minimum number of digits required to represent the number exactly on conversion back to a double-precision float) and the <code>format</code> command used where an exact number of digits is required.
;tcl_traceCompile
:This is used (provided it is enabled at library-build time) to enable printing out information about the compilation of Tcl scripts to bytecodes. (Not useful in wish on Windows due to system issues.)
;tcl_traceExec
:This is used (provided it is enabled at library-build time) to enable printing out information about the execution of Tcl's bytecodes and when different procedures are called. (Not useful in wish on Windows due to system issues.)
;tcl_interactive
:This marks whether the “convenience” features of the top-level REPL should be enabled; in non-interactive mode, Tcl scripts typically don't prompt for commands to execute, never allow abbreviation of command names, never automatically run external commands, etc.

Under normal circumstances, all of these variables should be left alone; their default values are virtually always correct for programs.


## UNIX Shell


;List of pragmatic directives:

* -v output the script line before it is executed
* -x output the command line arguments

;Utilization:

Pragmatic directives remain effective, until they are deactivated, or the end of the script is reached:


```sh
set -vx    # Activate both script line output and command line arguments pragma
set +vx    # Deactivate both pragmatic directives
```


{{omit from|AWK}}
{{omit from|ZX Spectrum Basic}}
