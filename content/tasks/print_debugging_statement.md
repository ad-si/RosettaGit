+++
title = "Print debugging statement"
description = ""
date = 2019-09-11T00:42:58Z
aliases = []
[extra]
id = 22494
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "go",
  "mercury",
  "perl",
  "perl_6",
  "phix",
  "pyret",
  "racket",
  "rexx",
  "zkl",
]
+++

From [https://en.wikipedia.org/wiki/Debugging#Techniques Wikipedia]:

<blockquote>
Print debugging (or tracing) is the act of watching (live or recorded) trace statements, or print statements, that indicate the flow of execution of a process. This is sometimes called printf debugging, due to the use of the printf function in C.
</blockquote>

## Task

* Show the print debugging statements in the language.
* Demonstrate their ability to track provenance by displaying information about source code (e.g., code fragment, line and column number).





## C


C doesn't have a built-in print debugging statement. However, it can be defined by users as a macro.


```c
#include <stdio.h>

#define DEBUG_INT(x) printf( #x " at line %d\nresult: %d\n\n", __LINE__, x)

int add(int x, int y) {
  int result = x + y;
  DEBUG_INT(x);
  DEBUG_INT(y);
  DEBUG_INT(result);
  DEBUG_INT(result+1);
  return result;
}

int main() {
  add(2, 7);
  return 0;
}
```


```txt

x at line 7
result: 2

y at line 8
result: 7

result at line 9
result: 9

result+1 at line 10
result: 10

```



## Go

Go doesn't have a built-in print debugging statement as such. Nor does it have macros.

However, as the following example shows, it is easy enough to mimic a C-like approach by writing a short 'debug' function which can show the value of an expression and its type at the appropriate line number in the program's source code.

Note that a label for the expression (whether it's a simple variable or not) must be passed to the 'debug' function as there is no way to deduce it otherwise.

```go
package main

import (
    "fmt"
    "runtime"
)

type point struct {
    x, y float64
}

func add(x, y int) int {
    result := x + y
    debug("x", x)
    debug("y", y)
    debug("result", result)
    debug("result+1", result+1)
    return result
}

func debug(s string, x interface{}) {
    _, _, lineNo, _ := runtime.Caller(1)
    fmt.Printf("%q at line %d type '%T'\nvalue: %#v\n\n", s, lineNo, x, x)
}

func main() {
    add(2, 7)
    b := true
    debug("b", b)
    s := "Hello"
    debug("s", s)
    p := point{2, 3}
    debug("p", p)
    q := &p
    debug("q", q)
}
```


```txt

"x" at line 14 type 'int'
value: 2

"y" at line 15 type 'int'
value: 7

"result" at line 16 type 'int'
value: 9

"result+1" at line 17 type 'int'
value: 10

"b" at line 29 type 'bool'
value: true

"s" at line 31 type 'string'
value: "Hello"

"p" at line 33 type 'main.point'
value: main.point{x:2, y:3}

"q" at line 35 type '*main.point'
value: &main.point{x:2, y:3}

```



## Mercury


Mercury has [https://mercurylang.org/information/doc-latest/mercury_ref/Trace-goals.html trace goals] which can be used within pure code, can summon <code>!IO</code> for use within the goal, can be made conditional off of compile-time or runtime flags, and which are pretty free with what they can do.

Together with data functors like <code>$module</code>, <code>$pred</code>, <code>$line</code>, trace goals can be used for debugging print statements as in the following example. Together with the [https://mercurylang.org/information/doc-latest/mercury_library/require.html require] module's utilities, trace goals can be used for assert() statements or pre/post assertions.


```Mercury
:- module add.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, io, string, list.

:- func add(int, int) = int.
A `add` B = C :-
    C = A + B,
    trace [io(!IO), compile_time(grade(debug))] (
        io.format("%s - %s(%d): %d `add` %d = %d\n",
            [s($grade), s($pred), i($line), i(A), i(B), i(C)], !IO)
    ).

main(!IO) :-
    2 `add` 7 = N,
    io.print_line(N, !IO).
```


{{out}} (with a non-debug grade)

```txt

9

```


{{out}} (with a debug grade)

```txt

asm_fast.par.gc.debug.stseg - function `add.add'/2(13): 2 `add` 7 = 9
9

```



## Perl

<code>Carp</code> is a core module, always available.

```perl
use Carp;

$str = 'Resistance'; carp "'$str' is futile."; print "\n";

doodle($str); print "\n";

fiddle(7);

sub doodle { my ($str) = @_; carp "'$str' is still futile." }

sub fiddle { faddle(2*shift) }
sub faddle { fuddle(3*shift) }
sub fuddle { ( carp("'$_[0]', interesting number.") ); }
```

```txt
'Resistance' is futile. at printf_debug.pl line 11.

'Resistance' is still futile. at printf_debug.pl line 17.
	main::doodle("Resistance") called at printf_debug.pl line 13

'42', interesting number. at printf_debug.pl line 21.
	main::fuddle(42) called at printf_debug.pl line 20
	main::faddle(14) called at printf_debug.pl line 19
	main::fiddle(7) called at printf_debug.pl line 15
```



## Perl 6

There isn't anything built-in to do this in Rakudo Perl 6, though it's pretty easy to cobble something together piggybacking off of the exception system. It would probably be better to instantiate a specific "DEBUG" exception to avoid interfering with other user instantiated ad-hoc exceptions, but for a quick-and-dirty demo, this should suffice.

This example will report any payload contents passed to the exception. If you want specific information, it will need to be passed in, though some of it may be determinable through introspection. Reports the file name and line number where the "debug" call originated and unwinds the call stack to trace through the subroutine calls leading up to it. Will follow the call chain into included files and modules, though calls to the CORE setting and dispatcher are filtered out here to reduce noise.

Comments with the files line numbers are added here to make it easier to match up the debug output with the file. Typically you would be editing the file in an editor that provides line numbering so that wouldn't be necessary/helpful.


```perl6
my &pdb = &die;

CATCH {
    when X::AdHoc {
        my @frames = .backtrace[*];
        say .payload;
        for @frames {
            # Filter out exception handing and dispatcher frames
            next if .file.contains: 'SETTING' or .subname.chars < 1;
            printf "%sfrom file: %s,%s line: %s\n",
              (' ' x $++), .file,
              (my $s = .subname) eq '<unit>' ?? '' !! " sub: $s,", .line;
        }
        say '';
        .resume;
    }
    default {}
}

## Testing / demonstration

# helper subs                #line 22
sub alpha ($a) {             #line 23
    pdb ('a =>', $a + 3);    #line 24
    pdb 'string';            #line 25
    beta(7);                 #line 26
}                            #line 27
sub beta  ($b) { pdb $b    } #line 28
sub gamma ($c) { beta $c   } #line 29
sub delta ($d) { gamma $d  } #line 30
                             #line 31
my $a = 10;                  #line 32
pdb (.VAR.name, $_) with $a; #line 33
alpha($a);                   #line 34
delta("Δ");                  #line 35
.&beta for ^3;               #line 36
```


```txt
($a 10)
from file: debug.p6, line: 33

(a => 13)
from file: debug.p6, sub: alpha, line: 24
 from file: debug.p6, line: 34

string
from file: debug.p6, sub: alpha, line: 25
 from file: debug.p6, line: 34

7
from file: debug.p6, sub: beta, line: 28
 from file: debug.p6, sub: alpha, line: 26
  from file: debug.p6, line: 34

Δ
from file: debug.p6, sub: beta, line: 28
 from file: debug.p6, sub: gamma, line: 29
  from file: debug.p6, sub: delta, line: 30
   from file: debug.p6, line: 35

0
from file: debug.p6, sub: beta, line: 28
 from file: debug.p6, line: 36

1
from file: debug.p6, sub: beta, line: 28
 from file: debug.p6, line: 36

2
from file: debug.p6, sub: beta, line: 28
 from file: debug.p6, line: 36
```



## Phix

The ? statement is used as a quick shorthand to dump variable contents and expression results.

For provenance, the following was added to builtins/reflections.e (for now/not an autoinclude, 0.8.1+), and throw()
was also tweaked to convert supplied addresses, which it previously did not. It proved to be quite an easy enhancement
to the language, albeit as yet undocumented.

```Phix
function _debug_info()
-- use throw to convert a return address and routine number
-- from the call stack into a proper line number, etc.
-- (private, not called direct/from outside this file)
integer rtn
atom ret_addr

    #ilASM{
        [32]
            mov edx,[ebp+20]    -- prev_ebp
            mov eax,[edx+28]    -- return address
            mov edx,[edx+20]    -- prev_ebp
            lea edi,[ret_addr]
            call :%pStoreMint
            mov eax,[edx+8]     -- calling routine no
            mov [rtn],eax
        [64]
            mov rdx,[rbp+40]    -- prev_ebp
            mov rax,[rdx+56]    -- return address
            mov rdx,[rdx+40]    -- prev_ebp
            lea rdi,[ret_addr]
            call :%pStoreMint
            mov rax,[rdx+16]    -- calling routine no
            mov [rtn],rax
        []
          }
    try
        throw({1,ret_addr-1,-1,rtn,-1,-1,-1})
    catch e
        return e
    end try
end function

-- NOTE: following five routines must all use the exact same nesting level.

global function debug_info()
    return _debug_info()
end function

global function debug_line()
    return _debug_info()[E_LINE]
end function

global function debug_rtn()
    return _debug_info()[E_NAME]
end function

global function debug_file()
    return _debug_info()[E_FILE]
end function

global function debug_path()
    return _debug_info()[E_PATH]
end function
```

This can be used as follows (0.8.1+)

```Phix
include builtins/reflections.e

?debug_info()
?debug_line()
procedure test()
    ?debug_info()
    ?debug_line()
    ?debug_file()
    printf(1,"This is line %d in file %s\n",{debug_line(),debug_file()})
end procedure
test()
```

```txt

{1,7544919,3,21,"-1","test.exw","C:\\Program Files (x86)\\Phix\\"}
4
{1,7545341,6,1034,"test","test.exw","C:\\Program Files (x86)\\Phix\\"}
7
"test.exw"
This is line 9 in file test.exw

```

See the throw() documentation for more details, especially regarding the debug_info() results.

There is no routine name for the first debug_info() call, so you get "-1" in [E_NAME].

The line numbers 3, 4, 6, 7, and 9 are returned for five of the seven calls.


## Pyret


Pyret has the [https://www.pyret.org/docs/latest/s_spies.html <code>spy</code> expression]. The expression can print the value of an identifier, using the identifier itself as a label if it's not already given. It could also print the value of an arbitrary expression, but it needs an explicit label in this case.


```pyret
fun add(x, y):
  result = x + y
  spy "in add":
    x,
    y,
    result,
    result-plus-one: result + 1
  end
  result
end

add(2, 7)
```


```txt

Spying "in add" (at file:///spies.arr:3:2-8:5)
  x: 2
  y: 7
  result: 9
  result-plus-one: 10

9

```



## Racket


Racket doesn't have a built-in print debugging statement. However, it can be defined by users as a macro.


```racket
#lang racket

(require syntax/parse/define)

(define (debug:core line col code val #:label [label #f])
  ;; if label exists, use it instead of the code fragment
  (printf "~a at line ~a column ~a\n" (or label code) line col)
  (printf "result: ~a\n\n" val)
  ;; return the value itself, so that we can wrap macro around an expression
  ;; without restructuring any code
  val)

(define-simple-macro (debug <x> option ...)
  #:with line (datum->syntax this-syntax (syntax-line #'<x>))
  #:with col (datum->syntax this-syntax (syntax-column #'<x>))
  (debug:core line col (quote <x>) <x> option ...))

(define (add x y)
  (define result (+ x y))
  (debug x)
  (debug y)
  (debug (if #t (+ x y) (error 'impossible)))
  (debug (add1 result) #:label "result plus one")
  (debug result))

(add 2 7)
```


```txt

x at line 20 column 9
result: 2

y at line 21 column 9
result: 7

(if #t (+ x y) (error 'impossible)) at line 22 column 9
result: 9

result plus one at line 23 column 9
result: 10

result at line 24 column 9
result: 9

9

```



## REXX

Since REXX is an interpretive language, it is easy to add judicious use of the   '''say'''   which is an easy

way to visually examine the values of any variable throughout the program's execution.

When that might not prove feasible   (maybe because of copious output before the problem occurs),

using the   '''trace'''   instruction might be a better choice.


Some of the options for the   '''trace'''   instruction are to display:
:*    what commands have a non-zero return code
:*    result of clauses
:*    what commands are being executed
:*    the (name of) labels being executed
:*    command errors
:*    command failures
:*    commands executed that have a negative return code
:*    an interactive mode that pauses and lets the programmer display values of variables


One of the options that shows the detailed information is the   <big>''' ''i'' '''</big>   option which
is the most informative and

shows intermediate results within a REXX statement as it's being evaluated.

The first number    (for the   '''trace'''   output)   is the line number for the REXX program.

(Blank lines are not   ''traced''.)

The following output is from the Regina REXX interpreter.

```rexx
/*REXX program to demonstrate  debugging  (TRACE)  information while executing a program*/
/*────────────────────────────────────────────── (below) the   I   is for information.  */
trace i
parse arg maxDiv .
if maxDiv=='' | maxDiv==","  then maxDiv= 1000   /*obtain optional argument from the CL.*/
say 'maximum random divisor is:'  maxDiv         /*display the max divisor being used.  */
total= 0

         do j=1  to 100
         total= total + j/random(maxDiv)
         end   /*j*/

say 'total=' total                               /*stick a fork in it,  we're all done. */
```

```txt

     4 *-* parse arg maxDiv .
       >>>   "9"
       >.>   ""
     5 *-* if maxDiv=='' | maxDiv==","  then maxDiv= 1000   /*obtain optional argument from the CL.*/
       >V>   "9"
       >L>   ","
       >O>   "0"
       >V>   "9"
       >L>   ""
       >O>   "0"
       >U>   "0"
     6 *-* say 'maximum random divisor is:'  maxDiv         /*display the max divisor being used.  */
       >L>   "maximum random divisor is:"
       >V>   "9"
       >O>   "maximum random divisor is: 9"
maximum random divisor is: 9
     7 *-* total= 0
       >L>   "0"
     9 *-* do j=1  to 100
       >L>   "1"
       >L>   "100"
       >V>   "1"
    10 *-*  total= total + j/random(maxDiv)
       >V>    "0"
       >V>    "1"
       >V>    "9"
       >F>    "3"
       >O>    "0.333333333"
       >O>    "0.333333333"
    11 *-* end   /*j*/
     9 *-* do j=1  to 100
       >V>   "1"
       >V>   "2"
    10 *-*  total= total + j/random(maxDiv)
       >V>    "0.333333333"
       >V>    "2"
       >V>    "9"
       >F>    "0"
    10 +++    total= total + j/random(maxDiv)
Error 42 running "c:\debuggin.rex", line 10: Arithmetic overflow/underflow
Error 42.3: Arithmetic overflow; divisor must not be zero

```

Programming note:   this error occurred because when the   '''random'''   BIF is invoked
with   ''one''   argument (as is here),   the

range of random numbers generated are integers from zero to the value of the argument (inclusive).


## zkl

Print debugging is similar to C. The _debug_ keyword conditionally compiles
code (ie the debug code isn't compiled unless debugging is turned on).

```zkl
fcn ds(line=__LINE__){
   println("This is line %d of file %s compiled on %s"
           .fmt(line,__FILE__,__DATE__));
}();
_debug_{
   ds(__LINE__); println("Debug level is ",__DEBUG__);
   vm.stackTrace().println();
}
```

```txt

Run with debugging turned off:

$ zkl rs
This is line 39 of file rs.zkl compiled on 2019-08-28

Run with debugging turned on:
Due to some brain deadness, we need to set the debug level (-d), compile the
file (-c) then run it and quit out of the REPL:

$ zkl -dc rs --run --quit
Compiled Class(rs)  (0.0 seconds, ??? lines/sec)
This is line 44 of file rs.zkl compiled on 2019-08-28
This is line 49 of file rs.zkl compiled on 2019-08-28
Debug level is 1
Stack trace for VM#1 ():
   rs.__constructor@stackTrace addr:25  args(0) reg(0)
   startup.__constructor@__constructor addr:1767  args(0) reg(22) R

```

