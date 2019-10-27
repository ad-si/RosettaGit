+++
title = "Assertions in design by contract"
description = ""
date = 2019-09-06T22:39:44Z
aliases = []
[extra]
id = 17864
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

According to   [[wp:Assertion_(software_development)#Assertions_in_design_by_contract|Wikipedia]];   assertions can function as a form of documentation:   they can describe the state the code expects to find before it runs (its preconditions), and the state the code expects to result in when it is finished running (postconditions);   they can also specify invariants of a class.


;Task:
Show in the program language of your choice an example of the use of assertions as a form of documentation.





## ALGOL W

{{trans|D}}
Algol W has assertions. Although pre and post conditions are not built in to the language, assertions can be used to simulate them.

```algolw
begin
    long real procedure averageOfAbsolutes( integer array values( * )
                                          ; integer value valuesLength
                                          ) ;
    begin
        long real av;
        % Pre-condition. %
        assert( valuesLength > 0 );
        for i := 1 until valuesLength do av := av + abs( values( i ) );
        av := av / valuesLength;
        % Post-condition. %
        assert( av >= 0 );
        av
    end averageOfAbsolutes ;
    begin
        integer array v( 1 :: 2 );
        v( 1 ) := 1; v( 2 ) := 3;
        r_format := "A"; r_w := 6; r_d := 2; % set output formatting %
        write( averageOfAbsolutes( v, 2 ) )
    end
end.
```

{{out}}

```txt

  2.00

```



## D

D has exceptions, errors and asserts. In Phobos there is also an enforce(). D has pre-conditions and post conditions, and struct/class invariants. Class method contracts should work correctly in object oriented code with inheritance.

```d
import std.stdio, std.algorithm, std.math;

double averageOfAbsolutes(in int[] values) pure nothrow @safe @nogc
in {
    // Pre-condition.
    assert(values.length > 0);
} out(result) {
    // Post-condition.
    assert(result >= 0);
} body {
    return values.map!abs.sum / double(values.length);
}

struct Foo {
    int x;
    void inc() { x++; }
    invariant {
        // Struct invariant.
        assert(x >= 0);
    }
}

void main() {
    [1, 3].averageOfAbsolutes.writeln;
    Foo f;
    f.inc;
}
```

{{out}}

```txt
2
```



## Eiffel

{{trans|D}}

```eiffel
  acc: INTEGER
  average_of_absolutes (values: ARRAY[INTEGER]): INTEGER
    require
      non_empty_values: values.count > 0
    do
      acc := 0
      values.do_all(agent abs_sum)
      Result := acc // values.count
    ensure
      non_neg_result: Result >= 0
    end
```



## Fortran

Fortran offers no formal "assert" protocol, but there would be nothing to stop a programmer devising a subroutine such as 
```Fortran
      SUBROUTINE AFFIRM(CONDITION,MESSAGE)
        LOGICAL CONDITION
        CHARACTER*(*) MESSAGE
         IF (CONDITION) RETURN      !All is well.
         WRITE (6,*) MESSAGE
         STOP "Oops. Confusion!"
       END 
```

And then scattering through the source file lines such as 
```Fortran
      CALL AFFIRM(SSQ.GE.0,"Sum of squares can't be negative.")   !Perhaps two passes should be used.
```

And this could be combined with the arrangements described in [[Stack_traces#Fortran]] to provide further interesting information. 

As written, the scheme involves an unconditional invocation of a subroutine with parameters. That overhead would be reduced by something like 
```Fortran
      IF (SSQ.LT.0) CALL CROAK("Sum of squares can't be negative.")   !Perhaps two passes should be used.
```


Some compilers allowed a D in column one to signify that this was a debugging statement, and a compiler option could specify that all such statements were to be treated as comments and not compiled. Probably not a good idea for statements performing checks. The code that is run with intent to produce results should be the same code that you have tested... A variation on this theme involves such debugging output being written to a file, then after modifying and recompiling, the new version's execution proceeds only while it produces the same debugging output. The reference output could be considered a (voluminous) contract, but for this to work a special testing environment is required and is not at all a Fortran standard.


## Go

The [https://golang.org/doc/faq#assertions Go FAQ] states:
::'''Why does Go not have assertions?'''
::Go doesn't provide assertions. They are undeniably convenient, but our experience has been that programmers use them as a crutch to avoid thinking about proper error handling and reporting. Proper error handling means that servers continue operation after non-fatal errors instead of crashing. Proper error reporting means that errors are direct and to the point, saving the programmer from interpreting a large crash trace. Precise errors are particularly important when the programmer seeing the errors is not familiar with the code.

::We understand that this is a point of contention. There are many things in the Go language and libraries that differ from modern practices, simply because we feel it's sometimes worth trying a different approach.

The "contract" in "design by contract" should be embodied in the API and error return values (if any) not in assertions that are typically only compiled when debugging.

If someone disagrees and they ''really'' want to use an "assert" they can simply roll their own:

```go
func assert(t bool, s string) {
	if !t {
		panic(s)
	}
}
//…
	assert(c == 0, "some text here")

```

(And if the assert function was defined in a file with build constraints and a stub in a file with the opposite constraint then they could be effectively be enabled/disabled at compile time. That's probably a bad idea.)


## J

J can load scripts expecting any non-assigned noun result to be all 1's.
If the file tautology_script.ijs contains

```J

NB. demonstrate properties of arithmetic
'A B C' =: 3 ?@$ 0   NB. A B and C are random floating point numbers in range [0, 1).
((A + B) + C) -: (A + (B + C))  NB. addition associates
(A + B) -: (B + A)              NB. addition commutes
(A * B) -: (B * A)              NB. scalar multiplication commutes
(A * (B + C)) -: ((A * B) + (A * C)) NB. distributive property

```

we could load it into a session as 0!:3<'tautology_script.ijs' with result of 1, because the expressions match (-:).  Were a sentence to fail the result would be 0, as for example replacing multiplication with matrix product and A B and C with square matrices of same size.

In next example the assertion both tests substitute when the script loads and shows how to use substitute.  Infinity (_) replaces the zeros in the y argument, and the x argument is the vector zero infinity.

```J

substitute =: 4 : '[&.((y~:{.x)&(#!.({:x)))y'
assert _ 1 1 _ 2 -: 0 _ substitute 0 1 1 0 2

```
 


Pre-condition adverbs with example:

```J

Positive =: adverb define
 'non-positive' assert *./ , y > 0
 u y
)
Integral =: adverb define
 'non-integral' assert (-: <.) y
 u y
)
display =: smoutput :[:
display_positive_integers =: display Integral Positive

   display 1 3 8
1 3 8
   display_positive_integers 1 3 8
1 3 8
   display 1x1 _1p1 
2.71828 _3.14159
   display_positive_integers 1x1 _1p1 
|non-positive: assert
|   'non-positive'    assert*./,y>0

```


As a post-condition, and this is contrived because a better definition of exact_factorial would be !@:x: ,

```J

IntegralResult =: adverb define
 RESULT =. u y
 'use extended precision!' assert (<datatype RESULT) e. ;:'extended integer'
 RESULT
:
 RESULT =. x u y
 'use extended precision!' assert (<datatype RESULT) e. ;:'extended integer'
 RESULT
)

exact_factorial =: !IntegralResult

   !50   
3.04141e64         
   !50x
30414093201713378043612608166064768844377641568960512000000000000
   exact_factorial 50x
30414093201713378043612608166064768844377641568960512000000000000
   exact_factorial 50
|use extended precision!: assert
|   'use extended precision!'    assert(<datatype RESULT)e.;:'extended integer'

```


One could assert an invariant in quicksort such that following the split the maximum of the small group is less than the minimum of the large group:

```txt
assert (>./LEFT) < (<./RIGHT)
```



## Java

The ''-ea'' or ''-enableassertions'' option must be passed to the VM when running the application for this to work.

Example taken from [[Perceptron#Java|Perceptron task]].

```java
(...)
int feedForward(double[] inputs) {
    assert inputs.length == weights.length : "weights and input length mismatch";

    double sum = 0;
    for (int i = 0; i < weights.length; i++) {
        sum += inputs[i] * weights[i];
    }
    return activate(sum);
}
(...)
```



## Julia

The @assert macro is used for assertions in Julia.

```julia
function volumesphere(r)
    @assert(r > 0, "Sphere radius must be positive")
    return π * r^3 * 4.0 / 3.0
end

```




## Kotlin


```scala
// version 1.1.2
// requires -ea JVM option

fun main(args: Array<String>) {
    assert(args.size > 0)  { "At least one command line argument must be passed to the program" }
    println("The following command line arguments have been passed:")
    for (arg in args) println(arg)
}
```


{{out}}
When run without passing command line arguments:

```txt

Exception in thread "main" java.lang.AssertionError: At least one command line argument must be passed to the program

```

When run passing 1 and 2 as command line arguments:

```txt

The following command line arguments have been passed:
1
2

```



## Perl 6

{{works with|Rakudo|2018.05}}
Most of theses entries seem to have missed the point. This isn't about how to implement or use assertions, it is about how the language uses design-by-contract to help the programmer get correct results.

Perl 6 doesn't have an assert routine in CORE. You could easily add one; several modules geared toward unit testing supply various "assertion" routines, though I'm not aware of any actually specifically named "assert()". 

Perl 6 core has subroutine signatures, multi-dispatch and exceptions to implement design-by-contract.

Subroutine signature allow the programmer to make the parameters supplied to a routine be checked to make sure they are the of correct quantity, type, and value and can constrain the returned value(s) to a particular type. See the below snippet for a brief demonstration. Perl 6 does some static analysis to trap errors at compile time, but traps many errors at run time since it is often impossible to tell if a variable is of the correct type before the program is run.

When Perl 6 encounters a design-by-contract violation, it will fail with an error message telling where the problem occurred, what it expected and what it actually got. Some failures are trappable and resumeable. Failures may be trapped by adding a CATCH { } block in the current scope. Failures will transfer execution to the CATCH block where the failure may be analysed and further action taken. Depending on the failure type, execution may be resumable. Some failures cause execution to halt unavoidably.

In this snippet, the routine repeat takes one Integer that must be greater than 1, a String and returns a String. (Note that as written, it is incorrect since it actually returns a boolean.)

```perl6
sub repeat ( Int $repeat where * > 1, Str $message, --> Str ) {
    say $message x $repeat;
    True # wrong return type
}

repeat( 2, 'A' ); # parameters ok, return type check error

repeat( 4, 2 ); # wrong second parameter type

repeat( 'B', 3 ); # wrong first (and second) parameter type

repeat( 1, 'C' ); # constraint check fail

repeat( ); # wrong number of parameters

CATCH {
    default {
        say "Error trapped: $_";
        .resume;
    }
}
```

{{out}}

```txt
AA
Error trapped: Type check failed for return value; expected Str but got Bool (Bool::True)
Error trapped: Type check failed in binding to parameter '$message'; expected Str but got Int (2)
Error trapped: Type check failed in binding to parameter '$repeat'; expected Int but got Str ("B")
Error trapped: Constraint type check failed in binding to parameter '$repeat'; expected anonymous constraint to be met but got Int (1)
Error trapped: Too few positionals passed; expected 2 arguments but got 0
This exception is not resumable
  in block  at contract.p6 line 19
  in block <unit> at contract.p6 line 14

```



## Phix

User defined types can be used to directly implement design by contract, and disabled by "without type_check".

```Phix
type hour(object x)
    return integer(x) and x>=0 and x<=23
end type
hour h
h = 1   -- fine
h = 26  -- bad
```

{{out}}

```txt

C:\Program Files (x86)\Phix\test.exw:6
type check failure, h is 26

```

Type check failures can also be caught and processed just like any other exception.


## Racket


The function and it's contract are a "translation" of "D".

The Racket Guide introduces contracts here
[http://docs.racket-lang.org/guide/contracts.html?q=contracts]

The Racket Reference defines contracts here
[http://docs.racket-lang.org/reference/contracts.html?q=contracts]

Note that the examples catch contract blame exceptions -- which, if uncaught are enough to halt a
program; making them quite assertive.

This example is extremely surface-scratching.


```racket

#lang racket
(require racket/contract)

;; This is the contract we will use.
;; "->"                   It is a function
;; (cons/c real?          That takes a list of at least one real (cons/c x (listof x)) means that an x
;;  (listof real?))       must occur before the rest of the list
;; (or/c zero? positive?) returns a non-negative number (for which there is no simpler contract that I
;;                        know of
(define average-of-absolutes/c
  (-> (cons/c real? (listof real?)) (or/c zero? positive?)))

;; this does what it's meant to
(define/contract (average-of-absolutes num-list)
  average-of-absolutes/c
  (/ (apply + (map abs num-list)) (length num-list)))

;; this will return a non-positive real (which will break the contract)
(define/contract (average-of-absolutes:bad num-list)
  average-of-absolutes/c
  (- (/ (apply + (map abs num-list)) (length num-list))))

(define (show-blame-error blame value message)
  (string-append   "Contract Violation!\n"
                   (format "Guilty Party: ~a\n" (blame-positive blame))
                   (format "Innocent Party: ~a\n" (blame-negative blame))
                   (format "Contracted Value Name: ~a\n" (blame-value blame))
                   (format "Contract Location: ~s\n" (blame-source blame))
                   (format "Contract Name: ~a\n" (blame-contract blame))
                   (format "Offending Value: ~s\n" value)
                   (format "Offense: ~a\n" message)))
(current-blame-format show-blame-error)

(module+ test
  ;; a wrapper to demonstrate blame
  (define-syntax-rule (show-contract-failure body ...)
    (with-handlers [(exn:fail:contract:blame?
                     (lambda (e) (printf "~a~%" (exn-message e))))]
                    (begin body ...)))
  
  (show-contract-failure (average-of-absolutes '(1 2 3)))
  (show-contract-failure (average-of-absolutes '(-1 -2 3)))
  
  ;; blame here is assigned to this test script: WE're providing the wrong arguments
  (show-contract-failure (average-of-absolutes 42))
  (show-contract-failure (average-of-absolutes '()))

  ;; blame here is assigned to the function implementation: which is returning a -ve value
  (show-contract-failure (average-of-absolutes:bad '(1 2 3)))
  (show-contract-failure (average-of-absolutes:bad '(-1 -2 3)))
  
  ;; blame here is assigned to this test script: since WE're providing the wrong arguments, so
  ;; the bad function doesn't have a chance to generate an invalid reply
  (show-contract-failure (average-of-absolutes:bad 42))
  (show-contract-failure (average-of-absolutes:bad '())))
```


{{out}}


```txt
2
2
Contract Violation!
Guilty Party: ...\Assertions_in_design_by_contract.rkt
Innocent Party: (function average-of-absolutes)
Contracted Value Name: average-of-absolutes
Contract Location: #(struct:srcloc "...\\Assertions_in_design_by_contract.rkt" 28 18 1037 20)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: 42
Offense: expected: pair?
  given: 42

Contract Violation!
Guilty Party: ...\Assertions_in_design_by_contract.rkt
Innocent Party: (function average-of-absolutes)
Contracted Value Name: average-of-absolutes
Contract Location: #(struct:srcloc "...\\Assertions_in_design_by_contract.rkt" 28 18 1037 20)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: ()
Offense: expected: pair?
  given: ()

Contract Violation!
Guilty Party: (function average-of-absolutes:bad)
Innocent Party: ...\Assertions_in_design_by_contract.rkt
Contracted Value Name: average-of-absolutes:bad
Contract Location: ...\\Assertions_in_design_by_contract.rkt" 33 18 1238 24)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: -2
Offense: promised: (or/c zero? positive?)
  produced: -2

Contract Violation!
Guilty Party: (function average-of-absolutes:bad)
Innocent Party: ...\Assertions_in_design_by_contract.rkt
Contracted Value Name: average-of-absolutes:bad
Contract Location: #(struct:srcloc "...\\Assertions_in_design_by_contract.rkt" 33 18 1238 24)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: -2
Offense: promised: (or/c zero? positive?)
  produced: -2

Contract Violation!
Guilty Party: ...\Assertions_in_design_by_contract.rkt
Innocent Party: (function average-of-absolutes:bad)
Contracted Value Name: average-of-absolutes:bad
Contract Location: #(struct:srcloc "...\\Assertions_in_design_by_contract.rkt" 33 18 1238 24)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: 42
Offense: expected: pair?
  given: 42

Contract Violation!
Guilty Party: ...\Assertions_in_design_by_contract.rkt
Innocent Party: (function average-of-absolutes:bad)
Contracted Value Name: average-of-absolutes:bad
Contract Location: #(struct:srcloc "...\\Assertions_in_design_by_contract.rkt" 33 18 1238 24)
Contract Name: (-> (cons/c real? (listof real?)) (or/c zero? positive?))
Offending Value: ()
Offense: expected: pair?
  given: ()
```



## REXX

This is just a simple method of ''assertion'';   more informative messages could be added in the   '''assertion'''   routine.

A   '''return'''   statement could've been used instead of an   '''exit'''   statement to continue processing.

```rexx
/*REXX program demonstrates a  method  on how to use  assertions in design  by contract.*/
parse arg top .                                  /*obtain optional argument from the CL.*/
if top=='' | top==","  then top=100              /*Not specified?  Then use the default.*/
pad=left('', 9)                                  /*PAD  contains nine blanks for  SAYs. */

      do #=1  for 666                            /*repeat for a devilish number of times*/
      a=random(1, top)                           /*generate a random number  (1 ──► TOP)*/
      b=random(1,   a)                           /*    "    "    "      "    (1 ──►   A)*/
      c=a-b                                      /*compute difference between  A and B. */
      say pad   'a='   right(a,4)   pad   "b="    right(b,4)     pad    'c='    right(c,4)
      call assert  date('Weekday')\=="Monday"    /*shouldn't execute this pgm on Monday.*/
      call assert  time('H')<9 | time("H")>15    /*    ··· and not during banking hours.*/
      call assert  c>0                           /*The value of   C   must be positive. */
      sum=my_sub(a, b, c)                        /*invoke a subroutine to do "stuff".   */
      say copies(' ',60)   'sum=' sum            /*display the sum of  A,  B,  and  C.  */
      end   /*#*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
assert: if arg(1)  then return 1;       say      /*if true,  then assertion has passed. */
        say 'assertion failed on line ' sigL " with: "  subword(space(sourceline(sigl)),3)
        say;   say '# index= '  #
        say 'ASSERT is exiting.';             exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
my_sub: return arg(1) +arg(2) +arg(3)            /*Sum three arguments.  Real easy work.*/
```

'''output'''   when using the default input:

```txt

          a=    6           b=    5           c=    1
                                                             sum= 12
          a=   88           b=   17           c=   71
                                                             sum= 176
          a=   34           b=   12           c=   22
                                                             sum= 68
          a=  100           b=   97           c=    3
                                                             sum= 200
          a=   88           b=   52           c=   36
                                                             sum= 176
          a=   17           b=    9           c=    8
                                                             sum= 34
          a=   29           b=   11           c=   18
                                                             sum= 58
          a=   23           b=    1           c=   22
                                                             sum= 46
          a=  100           b=   35           c=   65
                                                             sum= 200
          a=   10           b=    8           c=    2
                                                             sum= 20
          a=    1           b=    1           c=    0

assertion failed on line  13  with:  c>0 /*The value of C must be positive. */

# index=  11
ASSERT is exiting.

```



## Ruby


```ruby

require 'contracts'
include Contracts

Contract Num => Num
def double(x)
  x * 2
end

puts double("oops")

```

{{out}}

```txt

./contracts.rb:34:in `failure_callback': Contract violation: (RuntimeError)
    Expected: Contracts::Num,
    Actual: "oops"
    Value guarded in: Object::double
    With Contract: Contracts::Num, Contracts::Num
    At: main.rb:6 

```



## Scala

Scala provides runtime assertions like Java: they are designed to be used by static analysis tools however the default compiler doesn’t perform such analyses by default. The Scala assertions (<tt>assume</tt>, <tt>require</tt>, <tt>assert</tt>, <tt>ensuring</tt>) are [http://www.scala-lang.org/api/current/index.html#scala.Predef$ Predef library] methods that are enabled by default and can be disabled using the <tt>-Xdisable-assertions</tt> runtime flag, unlike Java where assertions are disabled by default and enabled with a runtime flag. It is considered poor form to rely on assertions to validate arguments, because they can be disabled. An appropriate informative runtime exception (e.g. NullPointerException or IllegalArgumentException) should be thrown instead.

```Scala
object AssertionsInDesignByContract extends App {
    /**
     * @param ints a non-empty array of integers
     * @return the mean magnitude of the array's elements.
     */
    def averageOfMagnitudes(ints: Array[Int]) = {
        // assertions for static analysis / runtime protoyping:
        assume(ints != null, "array must not be null")
        require(ints.length > 0, "array must be non-empty")
        // runtime exceptions when assertions are disabled:
        if (ints.length < 1) throw new IllegalArgumentException("Cannot find the average of an empty array")
        // note, the above line can implicitly throw a NullPointerException too
        val abs = ints.map(Math.abs)
        val mean = Math.round(abs.sum.toDouble / ints.length)
        assert(Math.abs(mean) >= abs.min && Math.abs(mean) <= abs.max, "magnitude must be within range")
        mean
    } ensuring(_ >= 0, "result must be non-negative (possible overflow)")

    println(averageOfMagnitudes(Array(1))) // 1
    println(averageOfMagnitudes(Array(1,3))) // 2
    println(averageOfMagnitudes(null)) // java.lang.AssertionError: assumption failed: array must not be null
    println(averageOfMagnitudes(Array())) // java.lang.IllegalArgumentException: requirement failed: array must be non-empty
    println(averageOfMagnitudes(Array(Integer.MAX_VALUE, Integer.MAX_VALUE))) // java.lang.AssertionError: assertion failed: magnitude must be within range
    println(averageOfMagnitudes(Array(Integer.MAX_VALUE, 1))) // java.lang.AssertionError: assertion failed: result must be non-negative (possible overflow)
}
```



## Tcl


```tcl
# Custom assertions; names stolen from Eiffel keywords
proc require {expression message args} {
    if {![uplevel 1 [list expr $expression]]} {
	set msg [uplevel 1 [list format $message] $args]
	return -level 2 -code error "PRECONDITION FAILED: $msg"
    }
}
proc ensure {expression {message ""} args} {
    if {![uplevel 1 [list expr $expression]]} {
	set msg [uplevel 1 [list format $message] $args]
	return -level 2 -code error "POSTCONDITION FAILED: $msg"
    }
}

proc connect_to_server {server port} {
    require {$server ne ""} "server address must not be empty"
    require {[string is integer -strict $port]} "port must be numeric"
    require {$port > 0 && $port < 65536} "port must be valid for client"

    set sock [socket $server $port]

    # Will never fail: Tcl *actually* throws an error on connection
    # failure instead, but the principle still holds.
    ensure {$sock ne ""} "a socket should have been created"

    return $sock
}
```

This can be usefully mixed with Tcl 8.6's <code>try … finally …</code> built-in command.

{{out|Example output from above code}}

```txt

% connect_to_server "" ""
PRECONDITION FAILED: server address must not be empty
% connect_to_server localhost 123456
PRECONDITION FAILED: port must be valid for client

```



## zkl

zkl has exceptions. The _assert_ keyword just wraps the AssertionError exception. _assert_ takes an expression and optional message.
There are no pre/post conditionals.

```zkl
fcn f(a){
   _assert_((z:=g())==5,"I wanted 5, got "+z)
}
```

Running the code throws an exception with file and line number:
{{out}}

```txt

VM#1 caught this unhandled exception:
   AssertionError : assert(foo.zkl:14): I wanted 5, got hoho

```

