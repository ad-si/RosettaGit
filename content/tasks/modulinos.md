+++
title = "Modulinos"
description = ""
date = 2019-06-25T07:44:56Z
aliases = []
[extra]
id = 9318
[taxonomies]
categories = ["task"]
tags = []
+++

It is useful to be able to execute a main() function only when a program is run directly. This is a central feature in programming scripts. A script that behaves this way is called a [http://www.slideshare.net/brian_d_foy/advanced-modulinos ''modulino''].

Examples from https://github.com/mcandre/modulinos

Sometimes getting the [[ScriptName]] is required in order to determine when to run main().

: ''This is still a draft task, and the current task description has caused mega confusion. See '''[[Talk:Modulinos]]''' for numerous attempts to understand the task and to rewrite the task description.''

: '''The task [[Executable library]] is written to replace this task.''' ''This task's future is in doubt as its aims are not clear enough.''





## C


C programs cannot normally do scripted main, because main() is implicitly included by another program, test.c, even though scriptedmain.h omits any main() prototype. However, preprocessor instructions can hide main unless a compiler flag is explicitly set.

Example


```sh
$ make
./scriptedmain
Main: The meaning of life is 42
./test
Test: The meaning of life is
```


Makefile


```make
all: scriptedmain test
	./scriptedmain
	./test

scriptedmain: scriptedmain.c scriptedmain.h
	gcc -o scriptedmain -DSCRIPTEDMAIN scriptedmain.c scriptedmain.h

test: test.c scriptedmain.h scriptedmain.c
	gcc -o test test.c scriptedmain.c scriptedmain.h

clean:
	-rm scriptedmain
	-rm test
	-rm scriptedmain.exe
	-rm test.exe
```


scriptedmain.h


```c
int meaning_of_life();
```


scriptedmain.c


```c
#include <stdio.h>

int meaning_of_life() {
	return 42;
}

#ifdef SCRIPTEDMAIN

int main() {
	printf("Main: The meaning of life is %d\n", meaning_of_life());

	return 0;
}

#endif
```


test.c


```c
#include "scriptedmain.h"
#include <stdio.h>

extern int meaning_of_life();

int main(int argc, char **argv) {
	printf("Test: The meaning of life is %d\n", meaning_of_life());
	return 0;
}
```



## C++

C++ programs cannot normally do scripted main, because main() is implicitly included by another program, test.c, even though scriptedmain.h omits any main() prototype. Preprocessor instructions can hide main() unless a compiler flat is explicitly set.

Example


```sh
$ make
./scriptedmain
Main: The meaning of life is 42
./test
Test: The meaning of life is 42
```


Makefile


```make
all: scriptedmain test
	./scriptedmain
	./test

scriptedmain: scriptedmain.cpp scriptedmain.h
	g++ -o scriptedmain -static-libgcc -static-libstdc++ -DSCRIPTEDMAIN scriptedmain.cpp scriptedmain.h

test: test.cpp scriptedmain.h scriptedmain.cpp
	g++ -o test -static-libgcc -static-libstdc++ test.cpp scriptedmain.cpp scriptedmain.h

clean:
	-rm scriptedmain
	-rm test
	-rm scriptedmain.exe
	-rm test.exe
```


scriptedmain.h


```cpp
int meaning_of_life();
```


scriptedmain.cpp


```cpp
#include <iostream>

using namespace std;

int meaning_of_life() {
	return 42;
}

#ifdef SCRIPTEDMAIN

int main() {
	cout << "Main: The meaning of life is " << meaning_of_life() << endl;
	return 0;
}

#endif
```


test.cpp


```cpp
#include "scriptedmain.h"
#include <iostream>

using namespace std;

extern int meaning_of_life();

int main() {
	cout << "Test: The meaning of life is " << meaning_of_life() << endl;
	return 0;
}
```



## Clojure

Uses [https://github.com/kumarshantanu/lein-exec lein-exec].

scriptedmain.clj:

```clojure
":";exec lein exec $0 ${1+"$@"}
":";exit

(ns scriptedmain
  (:gen-class))

(defn meaning-of-life [] 42)

(defn -main [& args]
  (println "Main: The meaning of life is" (meaning-of-life)))

(when (.contains (first *command-line-args*) *source-path*)
  (apply -main (rest *command-line-args*)))
```


test.clj:

```clojure
":";exec lein exec $0 ${1+"$@"}
":";exit

(ns test
  (:gen-class))

(load-string (slurp "scriptedmain.clj"))

(defn -main [& args]
  (println "Test: The meaning of life is" (scriptedmain/meaning-of-life)))

(when (.contains (first *command-line-args*) *source-path*)
  (apply -main (rest *command-line-args*)))
```



## CoffeeScript

scriptedmain.coffee:

```coffeescript
#!/usr/bin/env coffee

meaningOfLife = () -> 42

exports.meaningOfLife = meaningOfLife

main = () ->
  console.log "Main: The meaning of life is " + meaningOfLife()

if not module.parent then main()
```


test.coffee:

```coffeescript
#!/usr/bin/env coffee

sm = require "./scriptedmain"

console.log "Test: The meaning of life is " + sm.meaningOfLife()
```



## Common Lisp

Common Lisp has few standards for POSIX operation. Shebangs and command line arguments are hacks.

In CLISP, this code only works for ./scriptedmain.lisp.

~/.clisprc.lisp


```lisp
;;; Play nice with shebangs
(set-dispatch-macro-character #\# #\!
 (lambda (stream character n)
  (declare (ignore character n))
  (read-line stream nil nil t)
  nil))
```


scriptedmain.lisp


```lisp
#!/bin/sh
#|
exec clisp -q -q $0 $0 ${1+"$@"}
exit
|#

;;; Usage: ./scriptedmain.lisp

(defun meaning-of-life () 42)

(defun main (args)
 (format t "Main: The meaning of life is ~a~%" (meaning-of-life))
 (quit))

;;; With help from Francois-Rene Rideau
;;; http://tinyurl.com/cli-args
(let ((args
       #+clisp ext:*args*
       #+sbcl sb-ext:*posix-argv*
       #+clozure (ccl::command-line-arguments)
       #+gcl si:*command-args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
       #+cmu extensions:*command-line-strings*
       #+allegro (sys:command-line-arguments)
       #+lispworks sys:*line-arguments-list*
     ))

  (if (member (pathname-name *load-truename*)
              args
              :test #'(lambda (x y) (search x y :test #'equalp)))
    (main args)))
```


test.lisp


```lisp
#!/bin/sh
#|
exec clisp -q -q $0 $0 ${1+"$@"}
exit
|#

(load "scriptedmain.lisp")
(format t "Test: The meaning of life is ~a~%" (meaning-of-life))
```



## D


D manages to implement scriptedmain through the use of version directives, which require special options to rdmd and dmd.

scriptedmain.d:


```d
#!/usr/bin/env rdmd -version=scriptedmain

module scriptedmain;

import std.stdio;

int meaningOfLife() {
	return 42;
}

version (scriptedmain) {
	void main(string[] args) {
		writeln("Main: The meaning of life is ", meaningOfLife());
	}
}
```


test.d:


```d
#!/usr/bin/env rdmd -version=test

import scriptedmain;
import std.stdio;

version (test) {
	void main(string[] args) {
		writeln("Test: The meaning of life is ", meaningOfLife());
	}
}
```


Example:


```sh
$ ./scriptedmain.d
Main: The meaning of life is 42
$ ./test.d
Test: The meaning of life is 42
$ dmd scriptedmain.d -version=scriptedmain
$ ./scriptedmain
Main: The meaning of life is 42
$ dmd test.d scriptedmain.d -version=test
$ ./test
Test: The meaning of life is 42
```



## Dart

scriptedmain.dart:

```dart
#!/usr/bin/env dart

#library("scriptedmain");

meaningOfLife() {
	return 42;
}

main() {
	print("Main: The meaning of life is ${meaningOfLife()}");
}
```


test.dart:

```dart
#!/usr/bin/env dart

#import("scriptedmain.dart", prefix: "scriptedmain");

main() {
	print("Test: The meaning of life is ${scriptedmain.meaningOfLife()}");
}
```


Example:

```sh
$ ./scriptedmain.dart
Main: The meaning of life is 42
$ ./test.dart
Test: The meaning of life is 42
```



## Emacs Lisp

Emacs has scripted main, though older versions require an obscure shebang syntax.

scriptedmain.el


```lisp
:;exec emacs -batch -l $0 -f main $*

;;; Shebang from John Swaby
;;; http://www.emacswiki.org/emacs/EmacsScripts

(defun meaning-of-life () 42)

(defun main ()
 (message "Main: The meaning of life is %d" (meaning-of-life)))
```


test.el


```lisp
:;exec emacs -batch -l $0 -f main $*

;;; Shebang from John Swaby
;;; http://www.emacswiki.org/emacs/EmacsScripts

(defun main ()
 (setq load-path (cons default-directory load-path))
 (load "scriptedmain.el" nil t)
 (message "Test: The meaning of life is %d" (meaning-of-life)))
```



## Erlang

Erlang has scripted main by default. scriptedmain.erl must be compiled before test.erl can access its functions.

Makefile:

```make
all: t

t: scriptedmain.beam test.beam
	erl -noshell -s scriptedmain
	erl -noshell -s test

scriptedmain.beam: scriptedmain.erl
	erlc scriptedmain.erl

test.beam: test.erl
	erlc test.erl

clean:
	-rm *.beam
```


scriptedmain.erl:

```erlang
-module(scriptedmain).
-export([meaning_of_life/0, start/0]).

meaning_of_life() -> 42.

start() ->
  io:format("Main: The meaning of life is ~w~n", [meaning_of_life()]),
  init:stop().
```


test.erl:

```erlang
-module(test).
-export([start/0]).
-import(scriptedmain, [meaning_of_life/0]).

start() ->
  io:format("Test: The meaning of life is ~w~n", [meaning_of_life()]),
  init:stop().
```


=={{header|F Sharp|F#}}==

Note 1: F# supports the scriptedmain behavior, but F# does not support hybrid script-compiled code files. The following programs work provided that they are compiled and then run, as .fs files, not interpreted or dotslashed as .fsx files.

Note 2: fsharpc has a backwards file ordering: Specify any dependencies BEFORE the code that depends on them.

Note 3: fsharpc also has that unpredictable DOS-flavored command line flag syntax, so the --out requires a colon between it and its value, and -h only generates an error; use --help instead.

Note 4: In Unix, mono is required to run F# executables. In Windows, mono is not required for execution.

Example:


```sh
$ make
fsharpc --out:scriptedmain.exe ScriptedMain.fs
fsharpc --out:test.exe ScriptedMain.fs Test.fs
$ mono scriptedmain.exe
Main: The meaning of life is 42
$ mono test.exe
Test: The meaning of life is 42
```


Makefile:


```make
all: scriptedmain.exe test.exe

scriptedmain.exe: ScriptedMain.fs
	fsharpc --nologo --out:scriptedmain.exe ScriptedMain.fs

test.exe: Test.fs ScriptedMain.fs
	fsharpc --nologo --out:test.exe ScriptedMain.fs Test.fs

clean:
	-rm *.exe
```


ScriptedMain.fs:


```fsharp
namespace ScriptedMain

module ScriptedMain =
    let meaningOfLife = 42

    let main =
        printfn "Main: The meaning of life is %d" meaningOfLife
```


Test.fs:


```fsharp
module Test =
    open ScriptedMain

    let main =
        printfn "Test: The meaning of life is %d" ScriptedMain.meaningOfLife
```



## Factor

Note: The INCLUDE/INCLUDING macros must be added to the ~/.factor-rc configuration file.

Example:


```sh
$ ./scriptedmain.factor
Main: The meaning of life is 42
$ ./test.factor
Test: The meaning of life is 42
```


~/.factor-rc:


```factor
! INCLUDING macro that imports source code files in the current directory

USING: kernel vocabs.loader parser sequences lexer vocabs.parser ;
IN: syntax

: include-vocab ( vocab -- ) dup ".factor" append parse-file append use-vocab ;

SYNTAX: INCLUDING: ";" [ include-vocab ] each-token ;
```


scriptedmain.factor:


```factor
#! /usr/bin/env factor

USING: io math.parser ;
IN: scriptedmain

: meaning-of-life ( -- n ) 42 ;

: main ( -- ) meaning-of-life "Main: The meaning of life is " write number>string print ;

MAIN: main
```


test.factor:


```factor
#! /usr/bin/env factor

INCLUDING: scriptedmain ;
USING: io math.parser ;
IN: test

: main ( -- ) meaning-of-life "Test: The meaning of life is " write number>string print ;

MAIN: main
```



## Forth


Given this awful running reference:


```forth
42 constant Douglas-Adams

: go ( -- )
  ." The meaning of life is " Douglas-Adams . cr ;
```


The bulk of Forth systems provide a way to generate an executable that enters GO (ar any word) on start.

```forth
' go 'MAIN !
program douglas-adams
```


Which creates a file named 'douglas-adams' that you can then run.  If this is all in the same file, you can load the file, test parts of it, and then exit (or shell out) to run the executable.

A unix script requires that '#!' be a comment and that the system have some #!-compatible arguments.

```forth
#! /usr/bin/env gforth

42 constant Douglas-Adams
.( The meaning of life is ) Douglas-Adams . cr bye
```


Adding #! as a comment, as gforth does, is trivial.  For a means by which this script could distinguish between 'scripted execution' and otherwise, a symlink like 'forthscript' could easily be used, and the zeroth OS argument tested for, but there's no convention.

```forth
#! /usr/bin/env forthscript

42 constant Douglas-Adams

s" forthscript" 0 arg compare 0= [IF]
  .( The meaning of life is ) Douglas-Adams . cr bye
[THEN]

cr .( Why aren't you running this as a script?  It only provides a constant.)
```



## Groovy


Example:


```sh
$ ./ScriptedMain.groovy
Main: The meaning of life is 42
$ ./Test.groovy
Test: The meaning of life is 42
```


ScriptedMain.groovy:


```groovy
#!/usr/bin/env groovy

class ScriptedMain {
	static def meaningOfLife = 42

	static main(args) {
		println "Main: The meaning of life is " + meaningOfLife
	}
}
```


Test.groovy:


```groovy
#!/usr/bin/env groovy

println "Test: The meaning of life is " + ScriptedMain.meaningOfLife
```



## Haskell

Haskell has scripted main, but getting scripted main to work with compiled scripts is tricky.


```sh
$ runhaskell scriptedmain.hs
Main: The meaning of life is 42
$ runhaskell test.hs
Test: The meaning of life is 42
$ ghc -fforce-recomp -o scriptedmain -main-is ScriptedMain scriptedmain.hs
$ ./scriptedmain
Main: The meaning of life is 42
$ ghc -fforce-recomp -o test -main-is Test test.hs scriptedmain.hs
$ ./test
Test: The meaning of life is 42
```


scriptedmain.hs


```haskell
#!/usr/bin/env runhaskell

-- Compile:
--
-- ghc -fforce-recomp -o scriptedmain -main-is ScriptedMain scriptedmain.hs

module ScriptedMain where

meaningOfLife :: Int
meaningOfLife = 42

main :: IO ()
main = putStrLn $ "Main: The meaning of life is " ++ show meaningOfLife
```


test.hs


```haskell
#!/usr/bin/env runhaskell

-- Compile:
--
-- ghc -fforce-recomp -o test -main-is Test test.hs scriptedmain.hs

module Test where

import ScriptedMain hiding (main)

main :: IO ()
main = putStrLn $ "Test: The meaning of life is " ++ show meaningOfLife
```



## Io


ScriptedMain.io:


```io
#!/usr/bin/env io

ScriptedMain := Object clone
ScriptedMain meaningOfLife := 42

if( isLaunchScript,
    "Main: The meaning of life is #{ScriptedMain meaningOfLife}" interpolate println
)
```


test.io:


```io
#!/usr/bin/env io

"Test: The meaning of life is #{ScriptedMain meaningOfLife}" interpolate println
```



```sh
$ ./ScriptedMain.io
Main: The meaning of life is 42
$ ./test.io
Test: The meaning of life is 42
```



## J


modulinos.ijs:


```j
#!/usr/bin/env ijconsole

meaningOfLife =: 42

main =: monad define
	echo 'Main: The meaning of life is ',": meaningOfLife
	exit ''
)

shouldrun =: monad define
	if. 1 e. 'modulinos.ijs' E. ;ARGV do.
		main 0
	end.
)

shouldrun 0
```


test.j:


```j
#!/usr/bin/env jconsole

load 'modulinos.ijs'

echo 'Test: The meaning of life is ',": meaningOfLife

exit ''
```


Example:


```sh
$ ./modulinos.ijs
Main: The meaning of life is 42
$ ./test.j
Test: The meaning of life is 42
```



## Java

Java has scripted main by default.

ScriptedMain.java


```java
public class ScriptedMain {
	public static int meaningOfLife() {
		return 42;
	}

	public static void main(String[] args) {
		System.out.println("Main: The meaning of life is " + meaningOfLife());
	}
}
```


Test.java


```java
public class Test {
	public static void main(String[] args) {
		System.out.println("Test: The meaning of life is " + ScriptedMain.meaningOfLife());
	}
}
```



## JavaScript

Node.js has scripted main.

scriptedmain.js

```javascript
#!/usr/bin/env node

function meaningOfLife() { return 42; }

exports.meaningOfLife = meaningOfLife;

function main() {
	console.log("Main: The meaning of life is " + meaningOfLife());
}

if (!module.parent) { main(); }
```


test.js

```javascript
#!/usr/bin/env node

var sm = require("./scriptedmain");

console.log("Test: The meaning of life is " + sm.meaningOfLife());
```



## Julia

Julia does not use scripted main by default, but can be set to run as such. Modules generally use a /test unit test subdirectory instead.
<br />
In module file Divisors.jl:

```julia
module Divisors

using Primes

export properdivisors

function properdivisors(n::T) where T <: Integer
    0 < n || throw(ArgumentError("number to be factored must be ≥ 0, got $n"))
    1 < n || return T[]
    !isprime(n) || return T[one(T), n]
    f = factor(n)
    d = T[one(T)]
    for (k, v) in f
        c = T[k^i for i in 0:v]
        d = d*c'
        d = reshape(d, length(d))
    end
    sort!(d)
    return d[1:end-1]
end

function interactiveDivisors()
    println("\nFind proper divisors between two numbers.\nFirst number: ")
    lo = (x = tryparse(Int64, readline())) == nothing ? 0 : x
    println("\nSecond number: ")
    hi = (x = tryparse(Int64, readline())) == nothing ? 10 : x
    lo, hi = lo > hi ? (hi, lo) : (lo, hi)

    println("Listing the proper divisors for $lo through $hi.")
    for i in lo:hi
        println(lpad(i, 7), "  =>  ", rpad(properdivisors(i), 10))
    end
end

end

# some testing code
if occursin(r"divisors.jl"i, Base.PROGRAM_FILE)
    println("This module is running as main.\n")
    Divisors.interactiveDivisors()
end

```

In a user file getdivisors.jl:

```julia
include("divisors.jl")

using .Divisors

n = 708245926330
println("The proper divisors of $n are ", properdivisors(n))

```



## LLVM

LLVM can have scripted main a la C, using the weak attribute.


```sh
$ make
llvm-as scriptedmain.ll
llc scriptedmain.bc
gcc -o scriptedmain scriptedmain.s
./scriptedmain
Main: The meaning of life is 42
llvm-as test.ll
llc test.bc
gcc -o test test.s scriptedmain.s
./test
Test: The meaning of life is 42
```


Makefile


```make
EXECUTABLE_SM=scriptedmain
EXECUTABLE_TEST=test

all: test.ll scriptedmain.s
	llvm-as test.ll
	llc test.bc
	gcc -o $(EXECUTABLE_TEST) test.s scriptedmain.s
	./$(EXECUTABLE_TEST)

scriptedmain.s: scriptedmain.ll
	llvm-as scriptedmain.ll
	llc scriptedmain.bc
	gcc -o $(EXECUTABLE_SM) scriptedmain.s
	./$(EXECUTABLE_SM)

clean:
	-rm $(EXECUTABLE_TEST)
	-rm $(EXECUTABLE_SM)
	-rm test.s
	-rm test.bc
	-rm scriptedmain.s
	-rm scriptedmain.bc
```


scriptedmain.ll


```llvm
@msg_main = internal constant [33 x i8] c"Main: The meaning of life is %d\0A\00"

declare i32 @printf(i8* noalias nocapture, ...)

define i32 @meaning_of_life() {
	ret i32 42
}

define weak i32 @main(i32 %argc, i8** %argv) {
	%meaning = call i32 @meaning_of_life()

	call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([33 x i8]* @msg_main, i32 0, i32 0), i32 %meaning)

	ret i32 0
}
```


test.ll


```llvm
@msg_test = internal constant [33 x i8] c"Test: The meaning of life is %d\0A\00"

declare i32 @printf(i8* noalias nocapture, ...)

declare i32 @meaning_of_life()

define i32 @main(i32 %argc, i8** %argv) {
	%meaning = call i32 @meaning_of_life()

	call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([33 x i8]* @msg_test, i32 0, i32 0), i32 %meaning)

	ret i32 0
}
```



## Lua

Lua has scripted main by default because files are largely indistinguishable from functions semantically (they compile to Lua functions.) Ellipses is Lua's var-arg syntax for functions, and, therefore, for files as well.

scriptedmain.lua


```lua
#!/usr/bin/env lua

function meaningoflife()
	return 42
end

function main(arg)
	print("Main: The meaning of life is " .. meaningoflife())
end

if type(package.loaded[(...)]) ~= "userdata" then
	main(arg)
else
	module(..., package.seeall)
end
```


test.lua


```lua
#!/usr/bin/env lua
sm = require("scriptedmain")
print("Test: The meaning of life is " .. sm.meaningoflife())
```



## Make


Example

```sh
$ make -f scriptedmain.mf
The meaning of life is 42
(Main)
$ make -f test.mf
The meaning of life is 42
(Test)
```


scriptedmain.mf

```make
all: scriptedmain

meaning-of-life:
	@echo "The meaning of life is 42"

scriptedmain: meaning-of-life
	@echo "(Main)"

```


test.mf

```make
all: test

test:
	@make -f scriptedmain.mf meaning-of-life
	@echo "(Test)"

```



## Mathematica


scriptedmain.ma

```mathematica
#!/usr/bin/env MathKernel -script

MeaningOfLife[] = 42

ScriptName[] = Piecewise[
	{
		{"Interpreted", Position[$CommandLine, "-script", 1] == {}}
	},
	$CommandLine[[Position[$CommandLine, "-script", 1][[1,1]] + 1]]
]

Program = ScriptName[];

If[StringMatchQ[Program, ".*scriptedmain.*"],
	Print["Main: The meaning of life is " <> ToString[MeaningOfLife[]]]
]
```


test.ma:

```mathematica
#!/usr/bin/env MathKernel -script

Get["scriptedmain.ma"]

Print["Test: The meaning of life is " <> ToString[MeaningOfLife[]]]
```


Example:

```sh
$ ./scriptedmain.ma
Main: The meaning of life is 42
$ ./test.ma
Test: The meaning of life is 42
```


In Mac and Windows, the output will be surrounded by spurious quotes.


## Mozart/Oz

Makefile:

```make
all: run

run: scriptedmain test
	./scriptedmain
	./test

scriptedmain: scriptedmain.oz
	ozc -x scriptedmain.oz

scriptedmain.ozf: scriptedmain.oz
	ozc -c scriptedmain.oz

test: scriptedmain.ozf test.oz
	ozc -x test.oz

clean:
	-rm test
	-rm scriptedmain
	-rm *.ozf
	-rm *.exe

```


scriptedmain.oz:

```oz
functor
export
  meaningOfLife: MeaningOfLife
import
  System
  Application
  Property
  Regex at 'x-oz://contrib/regex'
define
  fun {MeaningOfLife} 42 end

  local ScriptName = {Property.get 'application.url'} in
    if {Regex.search "scriptedmain" ScriptName} \= false then
      {System.printInfo "Main: The meaning of life is "#{Int.toString {MeaningOfLife}}#"\n"}
      {Application.exit 0}
    end
  end
end

```


test.oz:

```oz
functor
import
  ScriptedMain
  System
  Application
  Property
  Regex at 'x-oz://contrib/regex'
define
  local ScriptName = {Property.get 'application.url'} in
    if {Regex.search "test" ScriptName} \= false then
      {System.printInfo "Test: The meaning of life is "#{Int.toString {ScriptedMain.meaningOfLife}}#"\n"}
      {Application.exit 0}
    end
  end
end
```



## newLISP

newLISP lacks scripted main, but the feature is easily added.

scriptedmain.lsp


```lisp
#!/usr/bin/env newlisp

(context 'SM)

(define (SM:meaning-of-life) 42)

(define (main)
	(println (format "Main: The meaning of life is %d" (meaning-of-life)))
	(exit))

(if (find "scriptedmain" (main-args 1)) (main))

(context MAIN)
```


test.lsp


```lisp
#!/usr/bin/env newlisp

(load "scriptedmain.lsp")
(println (format "Test: The meaning of life is %d" (SM:meaning-of-life)))
(exit)
```


=={{header|Objective-C}}==

scriptedmain.h:


```objc>#import <objc/Object.h


@interface ScriptedMain: Object {}

+ (int)meaningOfLife;

@end
```


scriptedmain.m:


```objc
#import "scriptedmain.h"
#import <Foundation/Foundation.h>

@implementation ScriptedMain

+ (int)meaningOfLife {
	return 42;
}

@end

int __attribute__((weak)) main(int argc, char **argv) {
	@autoreleasepool {

		printf("Main: The meaning of life is %d\n", [ScriptedMain meaningOfLife]);

	}

	return 0;
}
```


test.m:


```objc
#import "scriptedmain.h"
#import <Foundation/Foundation.h>

int main(int argc, char **argv) {
	@autoreleasepool {

		printf("Test: The meaning of life is %d\n", [ScriptedMain meaningOfLife]);

	}

	return 0;
}
```



```sh
$ gcc -o scriptedmain -lobjc -framework foundation scriptedmain.m
$ gcc -o test -lobjc -framework foundation test.m scriptedmain.m
$ ./scriptedmain
Main: The meaning of life is 42
$ ./test
Test: The meaning of life is 42
```



## OCaml


scriptedmain.ml


```ocaml
let meaning_of_life = 42

let main () =
  Printf.printf "Main: The meaning of life is %d\n"
    meaning_of_life

let () =
  if not !Sys.interactive then
    main ()
```


Invoked as a script:


```sh
$ ocaml scriptedmain.ml
Main: The meaning of life is 42
```


Loaded into an ocaml toplevel/utop:

<lang>$ ocaml
...
# #use "scriptedmain.ml";;
val meaning_of_life : int = 42
val main : unit -> unit = <fun>
# meaning_of_life;;
- : int = 42
#
```


The limit of this technique is "avoiding running something when loading a script interactively". It's not applicable to other uses, like adding an example script to a file normally used as a library, as that code will also fire when users of the library are run.

=={{header|Octave}}/{{header|MATLAB}}==
Octave and MATLAB have scripted main by default, because only the first function listed in a program are importable by other programs.

meaningoflife.m


```matlab
#!/usr/bin/env octave -qf

function y = meaningoflife()
	y = 42;
endfunction

function main()
	printf("Main: The meaning of life is %d", meaningoflife());
endfunction

main();
```


test.m


```matlab
#!/usr/bin/env octave -qf

printf("Test: The meaning of life is %d", meaningoflife());
```



## Pascal

Makefile:


```make
all: scriptedmain

scriptedmain: scriptedmain.pas
	fpc -dscriptedmain scriptedmain.pas

test: test.pas scriptedmain
	fpc test.pas

clean:
	-rm test
	-rm scriptedmain
	-rm *.o
	-rm *.ppu
```


scriptedmain.pas:


```pascal
{$IFDEF scriptedmain}
program ScriptedMain;
{$ELSE}
unit ScriptedMain;
interface
function MeaningOfLife () : integer;
implementation
{$ENDIF}
	function MeaningOfLife () : integer;
	begin
		MeaningOfLife := 42
	end;
{$IFDEF scriptedmain}
begin
	write('Main: The meaning of life is: ');
	writeln(MeaningOfLife())
{$ENDIF}
end.
```


test.pas:


```pascal
program Test;
uses
	ScriptedMain;
begin
	write('Test: The meaning of life is: ');
	writeln(MeaningOfLife())
end.
```


Example:


```sh
$ make
$ ./scriptedmain
Main: The meaning of life is: 42
$ make test
$ ./test
Test: The meaning of life is: 42
```



## Perl

Perl has scripted main. The code inside <tt>unless(caller) { ... }</tt> only runs when <tt>Life.pm</tt> is the main program.


```perl
#!/usr/bin/env perl

# Life.pm
package Life;

use strict;
use warnings;

sub meaning_of_life {
	return 42;
}

unless(caller) {
	print "Main: The meaning of life is " . meaning_of_life() . "\n";
}
```



```perl
#!/usr/bin/env perl

# death.pl
use strict;
use warnings;

use Life;

print "Life means " . Life::meaning_of_life . ".\n";
print "Death means invisible scary skeletons.\n";
```


## Perl 6

Perl 6 automatically calls MAIN on direct invocation, but this may be a multi dispatch, so a library may have multiple "scripted mains".

```perl6
class LUE {
    has $.answer = 42;
}

multi MAIN ('test') {
    say "ok" if LUE.new.answer == 42;
}

multi MAIN ('methods') {
    say ~LUE.^methods;
}
```



## Phix

Here is a simple little dirty hack to do this. You can put the function in any file, but you have to call it from the right place.

```Phix
function isMainOrInclude()
-- returns 1 if called from the main file, 0 if from an include
integer res
    #ilASM{
        [32]
            mov eax,[ebp+20]    -- prev_ebp
            mov eax,[eax+8]     -- rtn
            mov [res],eax
        [64]
            mov rax,[rbp+40]    -- prev_ebp
            mov rax,[rax+16]    -- rtn
            mov [res],rax
        []
          }
    return res=21 -- (21=T_maintls)
end function

printf(1,"This is %s\n",{{"An Include","Main"}[isMainOrInclude()+1]})
```



## PHP

PHP does not have scripted main, but the feature is easily added with a regular expression.

scriptedmain.php


```php
<?php
function meaning_of_life() {
	return 42;
}

function main($args) {
	echo "Main: The meaning of life is " . meaning_of_life() . "\n";
}

if (preg_match("/scriptedmain/", $_SERVER["SCRIPT_NAME"])) {
	main($argv);
}
?>
```


test.php


```php
<?php
require_once("scriptedmain.php");
echo "Test: The meaning of life is " . meaning_of_life() . "\n";
?>
```



## PicoLisp

PicoLisp normally does it the other way round: It calls main from the command line with the '-' syntax if desired. Create an executable file (chmod +x) "life.l":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(de meaningOfLife ()
   42 )

(de lifemain ()
   (prinl "Main: The meaning of life is " (meaningOfLife))
   (bye) )
```

and an executable file (chmod +x) "test.l":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "life.l")

(prinl "Test: The meaning of life is " (meaningOfLife))
(bye)
```

Test:

```txt
$ ./life.l -lifemain
Main: The meaning of life is 42

$ ./test.l
Test: The meaning of life is 42
```



## Python

Python has scripted main.


```python
#!/usr/bin/env python

# life.py

def meaning_of_life():
  return 42

if __name__ == "__main__":
  print("Main: The meaning of life is %s" % meaning_of_life())
```



```python
#!/usr/bin/env python

# death.py

from life import meaning_of_life

print("Life means %s." % meaning_of_life())
print("Death means invisible scary skeletons.")
```


## R

A way to check if code is running at "top level" is to check <code>length(sys.frames())</code>. This value will be zero for a file being run with <code>Rscript</code>, the <code>--file=</code> argument, or at the command line, and will be greater than 0 in all other conditions (such as package loading or code being sourced from another file.)


```R
#!/usr/bin/env Rscript

meaningOfLife <- function() {
	42
}

main <- function(args) {
	cat("Main: The meaning of life is", meaningOfLife(), "\n")
}

if (length(sys.frames()) > 0) {
        args <- commandArgs(trailingOnly = FALSE)
	main(args)
	q("no")
}
```


test.R


```R
#!/usr/bin/env Rscript

source("scriptedmain.R")

cat("Test: The meaning of life is", meaningOfLife(), "\n")

q("no")
```



## Racket

scriptedmain.rkt:

```racket
#!/usr/bin/env racket
#lang racket

(provide meaning-of-life)

(define (meaning-of-life) 42)

(module+ main (printf "Main: The meaning of life is ~a\n" (meaning-of-life)))
```


test.rkt:

```racket
#!/usr/bin/env racket
#lang racket

(module+ main
	(require "scriptedmain.rkt")
	(printf "Test: The meaning of life is ~a\n" (meaning-of-life)))
```



## REXX


```rexx
/*REXX program detects  whether or not  it is a  "scripted main"  program.              */
parse source . howInvoked @fn                    /*query REXX how this pgm got invoked. */

say 'This program  ('@fn")  was invoked as a: "    howInvoked

if howInvoked\=='COMMAND'  then do
                                say 'This program  ('@fn")  wasn't invoked via a command."
                                exit 12
                                end

    /*╔════════════════════════════════════════════════════════════════════════════════╗
      ║  At this point, we know that this program was invoked via the  "command line"  ║
      ║  or a program using the  "command interface"  and  not  via another program.   ║
      ╚════════════════════════════════════════════════════════════════════════════════╝*/

/*────────────────────────────── The main code follows here ... ────────────────────────*/
say
say '(from' @fn"):  and away we go ···"
```





## Ring


```ring

# Project : Modulinos

func meaningoflife()
       y = 42
       return y

func main()
       see "Main: The meaning of life is " + meaningoflife() + nl

```

Output:

```txt

Main: The meaning of life is 42

```



## Ruby

Ruby has scripted main.


```ruby
# life.rb

def meaning_of_life
  42
end

if __FILE__ == $0
  puts "Main: The meaning of life is #{meaning_of_life}"
end
```



```ruby
# death.rb

require 'life'

puts "Life means #{meaning_of_life}."
puts "Death means invisible scary skeletons."
```



## Rust

'''Note:''' this code is deprecated, and does not compile with Rust 1.0.0 and newer.

Makefile:

```make
all: scriptedmain

scriptedmain: scriptedmain.rs
	rustc scriptedmain.rs

test: test.rs scriptedmain.rs
	rustc --lib scriptedmain.rs
	rustc test.rs -L .

clean:
	-rm test
	-rm -rf *.dylib
	-rm scriptedmain
	-rm -rf *.dSYM
```


scriptedmain.rs:

```rust
#[link(name = "scriptedmain")];

use std;

fn meaning_of_life() -> int {
	ret 42;
}

fn main() {
	std::io::println("Main: The meaning of life is " + core::int::to_str(meaning_of_life(), 10u));
}
```


test.rs:

```rust
use scriptedmain;
use std;

fn main() {
	std::io::println("Test: The meaning of life is " + core::int::to_str(scriptedmain::meaning_of_life(), 10u));
}
```


Example:

```sh
$ make
$ make test
$ ./scriptedmain
Main: The meaning of life is 42
$ ./test
Test: The meaning of life is 42
```



## SAC

Makefile:

```make
all: scriptedmain

scriptedmain: ScriptedMain.sac
	sac2c -o scriptedmain ScriptedMain.sac -Dscriptedmain

test: test.sac ScriptedMain.sac
	sac2c ScriptedMain.sac
	sac2c -o test test.sac

clean:
	-rm test
	-rm test.c
	-rm libScriptedMainTree.so
	-rm libScriptedMainMod.so
	-rm libScriptedMainMod.a
	-rm scriptedmain
	-rm scriptedmain.c
```


ScriptedMain.sac:

```c
#ifndef scriptedmain
module ScriptedMain;
#endif

use StdIO: all;
use Array: all;
export all;

int meaning_of_life() {
	return(42);
}

#ifdef scriptedmain
int main() {
	printf("Main: The meaning of life is %d\n", meaning_of_life());
	return(0);
}
#endif
```


test.sac:

```c
use StdIO: all;
use Array: all;
use ScriptedMain: all;

int main() {
	printf("Test: The meaning of life is %d\n", meaning_of_life());
	return(0);
}
```


Example:

```sh
$ make
$ make test
$ ./scriptedmain
Main: The meaning of life is 42
$ ./test
Test: The meaning of life is 42
```


## Scala

### Unix shell script

This code must be stored as a shell script.

```bash
#!/bin/sh
exec scala "$0" "$@"
!#
  def hailstone(n: Int): Stream[Int] =
       n #:: (if (n == 1) Stream.empty else hailstone(if (n % 2 == 0) n / 2 else n * 3 + 1))

  val nr = argv.headOption.map(_.toInt).getOrElse(27)
  val collatz = hailstone(nr)
  println(s"Use the routine to show that the hailstone sequence for the number: $nr.")
  println(collatz.toList)
  println(s"It has ${collatz.length} elements.")
```



### Windows Command Script

This code must be stored as a Windows Command Script e.g. Hailstone.cmd

```winbatch
::#!
@echo off
call scala %0 %*
pause
goto :eof
::!#
  def hailstone(n: Int): Stream[Int] =
       n #:: (if (n == 1) Stream.empty else hailstone(if (n % 2 == 0) n / 2 else n * 3 + 1))

  val nr = argv.headOption.map(_.toInt).getOrElse(27)
  val collatz = hailstone(nr)
  println(s"Use the routine to show that the hailstone sequence for the number: $nr.")
  println(collatz.toList)
  println(s"It has ${collatz.length} elements.")

```

```txt
C:\>Hailstone.cmd 42
Use the routine to show that the hailstone sequence for the number: 42.
List(42, 21, 64, 32, 16, 8, 4, 2, 1)
It has 9 elements.
```



## Scheme

Chicken Scheme has the {{{ -ss }}} flag for the interpreter, but compiled Chicken Scheme programs do not have scripted main unless the behavior is added manually to the code.

scriptedmain.scm


```scheme
#!/bin/sh
#|
exec csi -ss $0 ${1+"$@"}
exit
|#

(use posix)
(require-extension srfi-1) ; lists

(define (meaning-of-life) 42)

(define (main args)
	(display (format "Main: The meaning of life is ~a\n" (meaning-of-life)))
	(exit))

(define (program)
	(if (string=? (car (argv)) "csi")
		(let ((s-index (list-index (lambda (x) (string-contains x "-s")) (argv))))
			(if (number? s-index)
				(cons 'interpreted (list-ref (argv) (+ 1 s-index)))
				(cons 'unknown "")))
		(cons 'compiled (car (argv)))))

(if (equal? (car (program)) 'compiled)
	(main (cdr (argv))))
```


test.scm


```scheme
#!/bin/sh
#|
exec csi -ss $0 ${1+"$@"}
exit
|#
(define (main args)
	(load "scriptedmain.scm")
	(display (format "Test: The meaning of life is ~a\n" (meaning-of-life)))
	(exit))
```



## Sidef


```ruby
# Life.sm

func meaning_of_life {
    42
}

if (__FILE__ == __MAIN__) {
    say "Main: The meaning of life is #{meaning_of_life()}"
}
```



```ruby
# test.sf

include Life

say "Test: The meaning of life is #{Life::meaning_of_life()}."
```



## Smalltalk


Note that the ScriptedMain package must be installed in order for test.st to access code from scriptedmain.st.

Example


```shell
$ gst-package -t ~/.st package.xml &>/dev/null

$ ./scriptedmain.st
Main: The meaning of life is 42

$ ./test.st
Test: The meaning of life is 42
```


package.xml


```xml><packages

<package>
	<name>ScriptedMain</name>
	<filein>scriptedmain.st</filein>
	<file>scriptedmain.st</file>
</package>
</packages>
```


scriptedmain.st


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$@"
"exit"

Object subclass: ScriptedMain [
	ScriptedMain class >> meaningOfLife [ ^42 ]
]

| main |

main := [
	Transcript show: 'Main: The meaning of life is ', ((ScriptedMain meaningOfLife) printString); cr.
].

(((Smalltalk getArgc) > 0) and: [ ((Smalltalk getArgv: 1) endsWith: 'scriptedmain.st') ]) ifTrue: [
	main value.
].
```


test.st


```smalltalk
"exec" "gst" "-f" "$0" "$0" "$@"
"exit"

"
PackageLoader fileInPackage: 'ScriptedMain'.

Transcript show: 'Test: The meaning of life is ', ((ScriptedMain meaningOfLife) printString); cr.
```




## Swift


Swift requires a number of hacks and boilerplate, but it is possible to write a modulino nevertheless.

Example


```shell
$ make
mkdir -p bin/
swiftc -D SCRIPTEDMAIN -o bin/ScriptedMain ScriptedMain.swift
swiftc -emit-library -module-name ScriptedMain -emit-module ScriptedMain.swift
mkdir -p bin/
swiftc -D TEST -o bin/Test Test.swift -I "." -L "." -lScriptedMain -module-link-name ScriptedMain
bin/ScriptedMain
Main: The meaning of life is 42
bin/Test
Test: The meaning of life is 42
```


Makefile


```make
all: bin/ScriptedMain bin/Test
	bin/ScriptedMain
	bin/Test

bin/ScriptedMain: ScriptedMain.swift
	mkdir -p bin/
	swiftc -D SCRIPTEDMAIN -o bin/ScriptedMain ScriptedMain.swift

ScriptedMain.swiftmodule: ScriptedMain.swift
	swiftc -emit-library -module-name ScriptedMain -emit-module ScriptedMain.swift

bin/Test: Test.swift ScriptedMain.swiftmodule
	mkdir -p bin/
	swiftc -D TEST -o bin/Test Test.swift -I "." -L "." -lScriptedMain -module-link-name ScriptedMain

clean:
	-rm -rf bin/
	-rm *.swiftmodule
	-rm *.swiftdoc
	-rm *.dylib

```


ScriptedMain.swift


```swift
import Foundation

public class ScriptedMain {
  public var meaningOfLife = 42

  public init() {}

  public class func main() {
    var meaning = ScriptedMain().meaningOfLife

    println("Main: The meaning of life is \(meaning)")
  }
}

#if SCRIPTEDMAIN
@objc class ScriptedMainAutoload {
  @objc class func load() {
    ScriptedMain.main()
  }
}
#endif

```


Test.swift


```swift
import Foundation
import ScriptedMain

public class Test {
  public class func main() {
    var meaning = ScriptedMain().meaningOfLife

    println("Test: The meaning of life is \(meaning)")
  }
}

#if TEST
@objc class TestAutoload {
  @objc class func load() {
    Test.main()
  }
}
#endif

```



## Tcl


```tcl
proc main {args} {
    puts "Directory: [pwd]"
    puts "Program: $::argv0"
    puts "Number of args: [llength $args]"
    foreach arg $args {puts "Arg: $arg"}
}

if {$::argv0 eq [info script]} {
    main {*}$::argv
}
```



## UNIX Shell

Bash has scripted main.

scriptedmain.sh


```sh
#!/usr/bin/env sh

meaning_of_life() {
	return 42
}

main() {
	meaning_of_life
	echo "Main: The meaning of life is $?"
}

if [[ "$BASH_SOURCE" == "$0" ]]
then
    main
fi
```


test.sh


```sh
#!/bin/bash

path=$(dirname -- "$0")
source "$path/scriptedmain"

meaning_of_life
echo "Test: The meaning of life is $?"

```



## ZX Spectrum Basic


On the ZX Spectrum, there is no main function as such, however a saved program can be made to start running from a particular line number by providing the line number as a parameter to save command. If the program is being merged as a module, then it does not run automatically. The following example will save the program in memory so that it starts running from line 500:


```zxbasic
SAVE "MYPROG" LINE 500: REM For a program with main code starting at line 500
```


