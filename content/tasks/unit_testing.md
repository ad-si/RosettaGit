+++
title = "Unit testing"
description = ""
date = 2019-09-01T06:28:28Z
aliases = []
[extra]
id = 22187
[taxonomies]
categories = ["task"]
tags = []
+++

Demonstrate any support for Unit testing built into the language implementation.

See https://en.wikipedia.org/wiki/Unit_testing and [[Test a function]] for more information.

Clearly state whether this support is internal, or external to the interpreter or compiler used.


## Go

Go has good support for unit testing provided by the "testing" package in its standard library in conjunction with the 'go test' command. This is separate from the Go compiler itself which is invoked with the 'go build' (or 'go run') command.

For a description of the facilities provided, interested readers should consult the [https://golang.org/pkg/testing/ documentation] for the "testing" package.

For a simple example of the testing process, check out the [[Test_a_function#Go]] task.


## Jsish


Jsish supports an '''internal''' ''-u'' command line option to run unit tests, as part of the base implementation.  These tests can include trial inputs and expected outputs in special comment blocks within a source file. Other command line options determine the level of verbosity and logging used when running tests. Being inside comment sections, unit tests do not affect the normal operation of a script (besides the negligible time taken for the interpreter to read in and skip over the comment sections). When in ''-u'' test-mode, source lines starting and ending with semi-colons '';'' are echoed using a special output format that also evaluates the enclosed expression and captures the result.


```javascript
/* A+B in Jsish */
var line = console.input();
var nums = line.match(/^\s*([+-]?[0-9]+)\s+([+-]?[0-9]+)\s*/);
if (nums) {
    var A = Number(nums[1]);
    var B = Number(nums[2]);
    if (A <= 1000 && A >= -1000 && B <= 1000 && B >= -1000) {
        printf("%d\n", A + B);
    } else {
        puts("error: A and B both need to be in range -1000 thru 1000 inclusive");
    }
} else {
    puts("error: A+B requires two numbers separated by space");
}

/*
=!INPUTSTART!=
a b
1234 123
-1000 +1000
123 -234
=!INPUTEND!=
*/
/*
=!EXPECTSTART!=
error: A+B requires two numbers separated by space
error: A and B both need to be in range -1000 thru 1000 inclusive
0
-111
=!EXEPECTEND!=
*/
```


```txt
prompt$ jsish -u A+B.jsi
[PASS] A+B.jsi

prompt$ jsish A+B.jsi
123 -234
-111
```




## Julia

Julia was developed with unit testing, and testing with macros such as @test are part of the language specification.  See
https://docs.julialang.org/en/v1/stdlib/Test/index.html.




## Perl 6

Perl 6 does not really have a mechanism built into the compiler to do unit testing, (It '''does''' have design-by-contract capabilities built in, but that is more for run-time, not really for unit testing.) Perl 6, and Perl in general '''does''' have unit testing built into the community. The [[wp:Test_Anything_Protocol|Test-Anything-Protocol]] was invented and developed specifically to do unit testing on the original version of Perl, is still heavily used for modern versions, and has become a major standard for unit testing in many different languages other than Perl.  

Testing is such a basic value in Perl 6, that a suite of unit tests '''is''' the language specification. As decreed by Larry Wall, the original author of Perl and the lead designer of Perl 6, anything that can pass "roast", [https://github.com/perl6/roast the official Perl 6 test suite], or at least majority portions of it, can call itself Perl 6.

Unit testing tools are not built in to the base compiler. Basic tools are distributed with the compiler as loadable modules. Many more complex toolkits and test harnesses are available through the [https://modules.perl6.org/ Perl 6 ecosystem]. In general, it is uncommon to include testing code in with run-time code; not unheard-of and not forbidden, but uncommon. Instead Perl modules by convention and community pressure tend to have a test suite that is automatically run when the module is installed. Again, there is no rule that a module that is released for public consumption '''must''' have a test suite, but the community tends to favour modules that have them and discourage/avoid those without.


## Racket


One could say that Racket has no unit testing built into the language. The most popular unit-testing framework, [https://docs.racket-lang.org/rackunit/ rackunit], is just a library. However, the default Racket installation does include rackunit out of the box.

The Racket command line tool, raco, has a command <code>raco test</code>. The command will look for a submodule <code>test</code> and run it. Users can choose any testing framework they want, with rackunit being the most popular one as mentioned above. Again, <code>raco test</code> is not really built into the language either.



## REXX

The REXX language does not really have a mechanism built into the interpreter to do unit testing.   (REXX is an interpretive language,   but some REXXes also have a compiler available.)   As it's an interpretive language,   it's fairly easy to create programs that stress the language's specifications. 

One helpful feature of the   [https://rosettacode.org/wiki/Category:REXX REXX]   language is it's quite stable (as per additions/specifications/enhancements),   and has been around since it was developed at IBM in the early 1980's. 

Also, most REXXes authors/creators/programmers/development teams have accumulated a suite of test programs that exercise a major part of the aspects and nuances of the REXX language and can usually find defects in the interpreter.   Another method is to run/execute   [https://en.wikipedia.org/wiki/A/B_testing '''A/B''']   type of testing,   that is,   run/execute REXX programs with two (or more) different REXX interpreters and compare results.   Since REXX does arithmetic using decimal numbers (including "floating point"), errors (even minute ones) are easy to identify.   Indeed, some (if not many) of the programs entered here on Rosetta Code have provided some REXX developers a very good starting base for a comprehensive test suite.


## Scala

The main Scala testing frameworks ( [http://www.scalacheck.org ScalaCheck], [http://www.scalatest.org ScalaTest], and [http://www.specs2.org specs2]) provide an implementation of the common test interface and only need to be added to the classpath to work with [https://www.scala-sbt.org/ sbt].

## zkl

zkl has built-in unit testing as shown in [[Test_a_function#zkl]].
It can test source code ( UnitTester.testSrc("x:=1") ) or compiled code ( UnitTester.testRun(fcn{ x:=1 } ).
