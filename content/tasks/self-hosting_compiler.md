+++
title = "Self-hosting compiler"
description = ""
date = 2019-08-18T21:20:45Z
aliases = []
[extra]
id = 17596
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "bacon",
  "csharp",
  "go",
  "j",
  "kotlin",
  "lingo",
  "perl_6",
  "phix",
  "python",
  "rust",
  "scala",
  "visual_basic_dotnet",
  "zed",
  "zkl",
]
+++

Implement a self-hosting compiler for your language i.e. create a compiler that is implemented in the language it is supposed to compile. Extra credit for any solution that can compile in an online compiler such as Ideone. If this is not feasible due to language complexity, link to external projects that achieve this task.


## ALGOL 68


The Algol 68 to C compiler available here: [http://www.poenikatu.co.uk/src/] or here: [https://github.com/NevilleDNZ/algol68toc] is written in ALGOL 68 and compiles itself. It is derived from the ALGOL 68RS compiler.


## BaCon

BaCon is a BASIC to C converter and it is implemented in itself. Also it can compile itself since version 0.110. Source code can be downloaded from [https://basic-converter.org/ here].


## C#

The official (Microsoft) C# compiler became self-hosting in 2011 and can be found (along with the self-hosting VB.NET compiler) at [https://github.com/dotnet/roslyn].

The older Mono C# compiler, currently backed by the .NET Foundation, can be found at [https://github.com/mono/mono]. There is also a (much newer) Mono VB compiler.


## Go

The principal Go compiler was originally written in C but from version 1.5 became entirely self-hosting (with a little assembler).

The source code for the current version can be viewed [https://go.googlesource.com/go here].

There is also a second compiler 'gccgo' which is part of the GNU Compiler Collection. However, this compiler is written in C++.


## J



```J
do =: ".
STDIN =: 1
do read STDIN

   NB. example:
   ".1!:1[1
((,~}:)~|.)'Go hang a salami '
Go hang a salami imalas a gnah oG
```



## Kotlin

The Kotlin compiler is partly self-hosted (the rest is written in Java) and the current version (1.1.2), released in April 2017, is the culmination of about 7 years of development by a large team of people at JetBrains and open source contributors.

Any one who is interested can view the code for the compiler at https://github.com/jetbrains/kotlin.


## Lingo

Propably not how the task was meant by the OP, but if "compiling code at runtime" counts, here Lingo's capabilities to do this:

```lingo
m = new(#script)

-- automatically triggers compilation to byte code
m.scriptText = "on foo ()"&RETURN&\
"  repeat with i = 1 to 3"&RETURN&\
"    put i"&RETURN&\
"  end repeat"&RETURN&\
"end"

-- can be called immediately
foo()
-- 1
-- 2
-- 3

-- for direct execution of simple code there is also the do() function
do("put _system.milliseconds")
-- 22476580
```



## Perl 6



```perl6
use MONKEY-SEE-NO-EVAL;
EVAL slurp
```



## Phix

Phix is self hosted. Run "p -c p" and it rebuilds itself in about 15 seconds. One of my favourite parlour tricks is to then run "p p p p p p p p -cp" which stacks seven interpreted copies of itself on top of each other with the last recompiling itself, and apart from a slightly longer startup time, no slower than the shorter command.


## Python

The [http://pypy.org/ PyPy] project has implemented Python using the Python language. The most popular Python interpreter is CPython, which is implemented in C.

## Rust

[https://web.archive.org/web/20140815054745/http://blog.mozilla.org/graydon/2010/10/02/rust-progress/ Since 2010], the [https://github.com/rust-lang/rust/ Rust compiler] has consisted of a self-hosting compiler frontend built on top of LLVM.

A code generator named [https://github.com/CraneStation/cranelift Cranelift], written in Rust, is currently under development with one of its goals being to serve as an alternative to LLVM for debug builds which can provide a better balance between compile time and runtime performance.


## Scala

The Scala compiler is totally written in Scala and compiles itself. The code is sourced and maintained in [https://github.com/scala/scala GitHub] repsitory.


## Visual Basic .NET

The official (Microsoft) VB.NET compiler became self-hosting in 2011 and can be found (along with the self-hosting C# compiler) at [https://github.com/dotnet/roslyn].

.NET Mono also has a relatively new VB.NET compiler, which can be found at [https://github.com/mono/mono-basic]. There is also an older Mono C# compiler.


## ZED

Source ->
Compiled -> http://ideone.com/UHMQco

```zed
(*) number1 number2
comment:
#true
(003) "*" number1 number2

(+) number1 number2
comment:
#true
(003) "+" number1 number2

(-) number1 number2
comment:
#true
(003) "-" number1 numbe2

(/) number1 number2
comment:
#true
(003) "/" number1 number2

(=) "value1" "value2"
comment:
#true
(003) "eqv?" "value1" "value2"

(add-between!) "item" "list" "collect"
comment:
(002) "null?" "list"
"collect"

(add-between!) "item" "list" "collect"
comment:
(002) "null?" (002) "cdr" "list"
(003) "cons" (002) "car" "list" "collect"

(add-between!) "item" "list" "collect"
comment:
#true
(add-between!) "item"
               (002) "cdr" "list"
               (003) "cons" "item" (003) "cons" (002) "car" "list" "collect"

(add-between) "item" "list"
comment:
#true
(002) "reverse" (add-between!) "item" "list" nil

(add-between-ra) "list" "item"
comment:
#true
(002) "reverse" (add-between!) "item" "list" nil

(alpha)
comment:
#true
(write-all) (rd) nil
                 "append"
                 (rd) nil "append" (newlines)
                                    (by-four)
                                     (function-sort)
                                      (sentences)
                                       (leading-newline)
                                        (space-newline)
                                         (tab-replace)
                                          (newline-space-tab-repeats)
                                           (newline-space)
                                            (filter) not-return?
                                                     (replace-trailing-white-space) (read-all)

(application?) "expression"
comment:
#true
(004) "and" (002) "not" (002) "null?" "expression"
            (002) "pair?" (002) "car" "expression"
            (002) "null?" (002) "cdar" "expression"

(arity) "arity-hash" "function"
comment:
#true
(002) "cdr" (003) "assoc" "function" "arity-hash"

(arity-hash!) "clause"
comment:
#true
(003) "cons" (clause-name) "clause" (clause-arity) "clause"

(arity-hash) "program"
comment:
#true
(003) "append" (003) "map" arity-hash! "program" (003) "append" (009) "list" (003) "cons" "'ZED1" 1 (003) "cons" "'ZED!" 1 (003) "cons" "'ZED001" 1 (003) "cons" "'or" 2 (003) "cons" "'and" 2 (003) "cons" "'begin" 2 (003) "cons" "'ZEDc" 2 (003) "cons" "'quote" 1 (099) "list" (003) "cons" "'ZED002" 2 (003) "cons" "'ZED003" 3 (003) "cons" "'ZED004" 4 (003) "cons" "'ZED005" 5 (003) "cons" "'ZED006" 6 (003) "cons" "'ZED007" 7 (003) "cons" "'ZED008" 8 (003) "cons" "'ZED009" 9 (003) "cons" "'ZED010" 10 (003) "cons" "'ZED011" 11 (003) "cons" "'ZED012" 12 (003) "cons" "'ZED013" 13 (003) "cons" "'ZED014" 14 (003) "cons" "'ZED015" 15 (003) "cons" "'ZED016" 16 (003) "cons" "'ZED017" 17 (003) "cons" "'ZED018" 18 (003) "cons" "'ZED019" 19 (003) "cons" "'ZED020" 20 (003) "cons" "'ZED021" 21 (003) "cons" "'ZED022" 22 (003) "cons" "'ZED023" 23 (003) "cons" "'ZED024" 24 (003) "cons" "'ZED025" 25 (003) "cons" "'ZED026" 26 (003) "cons" "'ZED027" 27 (003) "cons" "'ZED028" 28 (003) "cons" "'ZED029" 29 (003) "cons" "'ZED030" 30 (003) "cons" "'ZED031" 31 (003) "cons" "'ZED032" 32 (003) "cons" "'ZED033" 33 (003) "cons" "'ZED034" 34 (003) "cons" "'ZED035" 35 (003) "cons" "'ZED036" 36 (003) "cons" "'ZED037" 37 (003) "cons" "'ZED038" 38 (003) "cons" "'ZED039" 39 (003) "cons" "'ZED040" 40 (003) "cons" "'ZED041" 41 (003) "cons" "'ZED042" 42 (003) "cons" "'ZED043" 43 (003) "cons" "'ZED044" 44 (003) "cons" "'ZED045" 45 (003) "cons" "'ZED046" 46 (003) "cons" "'ZED047" 47 (003) "cons" "'ZED048" 48 (003) "cons" "'ZED049" 49 (003) "cons" "'ZED050" 50 (003) "cons" "'ZED051" 51 (003) "cons" "'ZED052" 52 (003) "cons" "'ZED053" 53 (003) "cons" "'ZED054" 54 (003) "cons" "'ZED055" 55 (003) "cons" "'ZED056" 56 (003) "cons" "'ZED057" 57 (003) "cons" "'ZED058" 58 (003) "cons" "'ZED059" 59 (003) "cons" "'ZED060" 60 (003) "cons" "'ZED061" 61 (003) "cons" "'ZED062" 62 (003) "cons" "'ZED063" 63 (003) "cons" "'ZED064" 64 (003) "cons" "'ZED065" 65 (003) "cons" "'ZED066" 66 (003) "cons" "'ZED067" 67 (003) "cons" "'ZED068" 68 (003) "cons" "'ZED069" 69 (003) "cons" "'ZED070" 70 (003) "cons" "'ZED071" 71 (003) "cons" "'ZED072" 72 (003) "cons" "'ZED073" 73 (003) "cons" "'ZED074" 74 (003) "cons" "'ZED075" 75 (003) "cons" "'ZED076" 76 (003) "cons" "'ZED077" 77 (003) "cons" "'ZED078" 78 (003) "cons" "'ZED079" 79 (003) "cons" "'ZED080" 80 (003) "cons" "'ZED081" 81 (003) "cons" "'ZED082" 82 (003) "cons" "'ZED083" 83 (003) "cons" "'ZED084" 84 (003) "cons" "'ZED085" 85 (003) "cons" "'ZED086" 86 (003) "cons" "'ZED087" 87 (003) "cons" "'ZED088" 88 (003) "cons" "'ZED089" 89 (003) "cons" "'ZED090" 90 (003) "cons" "'ZED091" 91 (003) "cons" "'ZED092" 92 (003) "cons" "'ZED093" 93 (003) "cons" "'ZED094" 94 (003) "cons" "'ZED095" 95 (003) "cons" "'ZED096" 96 (003) "cons" "'ZED097" 97 (003) "cons" "'ZED098" 98 (003) "cons" "'ZED099" 99

(by-four!) "sentences" "collect"
comment:
(002) "null?" "sentences"
"collect"

(by-four!) "sentences" "collect"
comment:
#true
(by-four!) (002) "cddddr" "sentences"
           (003) "cons" (005) "list"
                              (002) "car" "sentences"
                              (002) "cadr" "sentences"
                              (002) "caddr" "sentences"
                              (002) "cadddr" "sentences"
                        "collect"

(by-four) "sentences"
comment:
#true
(002) "reverse" (by-four!) "sentences" nil

(by-three!) "sentences" "collect"
comment:
(002) "null?" "sentences"
"collect"

(by-three!) "sentences" "collect"
comment:
#true
(by-three!) (002) "cdddr" "sentences"
            (003) "cons" (004) "list"
                               (002) "car" "sentences"
                               (002) "cadr" "sentences"
                               (002) "caddr" "sentences"
                         "collect"

(by-three) "sentences"
comment:
#true
(002) "reverse" (by-three!) "sentences" nil

(character-less?) "character1" "character2"
comment:
#true
(003) "<" (002) "char->integer" "character1" (002) "char->integer" "character2"

(clause-arguments) "clause"
comment:
#true
(002) "cadr" "clause"

(clause-arguments-agree) "clause1" "clause2"
comment:
(003) "equal?" (clause-arguments) "clause1" (clause-arguments) "clause2"
(clause-arguments) "clause1"

(clause-arity) "clause"
comment:
#true
(002) "length" (clause-arguments) "clause"

(clause-body) "clause"
comment:
#true
(002) "cddr" "clause"

(clause-less?) "clause1" "clause2"
comment:
#true
(sentence-less?) (002) "car" "clause1" (002) "car" "clause2"

(clause-name) "clause"
comment:
#true
(002) "car" "clause"

(clause-name-agree) "clause1" "clause2"
comment:
(003) "eq?" (clause-name) "clause1" (clause-name) "clause2"
(clause-name) "clause1"

(combine-all!) "program" "collect"
comment:
(002) "null?" "program"
"collect"

(combine-all!) "program" "collect"
comment:
(002) "null?" (002) "cdr" "program"
(003) "cons" (002) "car" "program" "collect"

(combine-all!) "program" "collect"
comment:
(003) "eq?" (clause-name) (002) "car" "program" (clause-name) (002) "cadr" "program"
(combine-all!) (combine-head-clauses) "program" "collect"

(combine-all!) "program" "collect"
comment:
#true
(combine-all!) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect"

(combine-all) "program"
comment:
#true
(002) "reverse" (combine-all!) "program" nil

(combine-clauses) "clause1" "clause2"
comment:
#true
(003) "cons" (clause-name-agree) "clause1" "clause2"
             (003) "cons" (clause-arguments-agree) "clause1" "clause2"
                          (003) "append" (clause-body) "clause1" (clause-body) "clause2"

(combine-head-clauses) "program"
comment:
#true
(003) "cons" (combine-clauses) (002) "car" "program" (002) "cadr" "program"
             (002) "cddr" "program"

(combine-program-clauses) "program"
comment:
#true
(combine-all) (ready-program) "program"

(comp!)
comment:
#true
(comp!a) (combine-program-clauses)
          (by-three)
           (read-sentences)
            (discard-comments)
             (function-sort)
              (sentences)
               (leading-newline)
                (space-newline)
                 (tab-replace)
                  (newline-space-tab-repeats)
                   (newline-space)
                    (filter) not-return? (replace-trailing-white-space) (read-all)

(comp!a) "combined"
comment:
#true
(comp!aa) (programize) "combined" (arity-hash) "combined"

(comp!aa) "programized"
comment:
#true
(002) "write" "programized"

(comp)
comment:
#true
(comp!)

(condefy!) "expressions" "collect"
comment:
(002) "null?" "expressions"
"collect"

(condefy!) "expressions" "collect"
comment:
#true
(condefy!) (002) "cddr" "expressions"
           (003) "cons" (003) "append" (002) "car" "expressions"
                                       (002) "cadr" "expressions"
                        "collect"

(condefy) "expressions"
comment:
#true
(002) "reverse" (condefy!) "expressions" nil

(cons) value1 value2
comment:
#true
(003) "cons" value1 value2

(count) number
comment:
#true
(c) number (count) (+) number 1

(count-by) step number
comment:
#true
(c) number (count-by) step (+) number step

(delay-wrap) "expression"
comment:
#true
(003) "list" "'delay" "expression"

(digit?) "character"
comment:
#true
(011) "or" (003) "eqv?" #0 "character" (003) "eqv?" #1 "character" (003) "eqv?" #2 "character" (003) "eqv?" #3 "character" (003) "eqv?" #4 "character" (003) "eqv?" #5 "character" (003) "eqv?" #6 "character" (003) "eqv?" #7 "character" (003) "eqv?" #8 "character" (003) "eqv?" #9 "character"

(discard-comments!) "program" "collect"
comment:
(002) "null?" "program"
"collect"

(discard-comments!) "program" "collect"
comment:
#true
(discard-comments!) (002) "cddddr" "program"
                    (003) "cons" (002) "cadddr" "program"
                                 (003) "cons" (002) "caddr" "program"
                                              (003) "cons" (002) "car" "program" "collect"

(discard-comments) "program"
comment:
#true
(002) "reverse" (discard-comments!) "program" nil

(dr!) "value"
comment:
(002) "pair?" "value"
(mp) dr! (first) 64 "value"

(dr!) "value"
comment:
#true
"value"

(dr) "value"
comment:
#true
(005) "begin" (002) "display" (pr) (dr!) "value" (001) "newline" (001) "newline" "value"

(drr) "value"
comment:
#true
(005) "begin" (002) "display" (pr) "value" (001) "newline" (001) "newline" "value"

(false?) "noun-list"
comment:
#true
(003) "equal?" "noun-list" (007) "list" ## #f #a #l #s #e

(fi) "function" "list"
comment:
(002) "null?" "list"
nil

(fi) "function" "list"
comment:
(002) "function" (1) "list"
(c) (1) "list" (fi) "function" (!) "list"

(fi) "function" "list"
comment:
#true
(fi) "function" (!) "list"

(filter!) "predicate" "list" "collect"
comment:
(002) "null?" "list"
"collect"

(filter!) "predicate" "list" "collect"
comment:
(002) "predicate" (002) "car" "list"
(filter!) "predicate" (002) "cdr" "list" (003) "cons" (002) "car" "list" "collect"

(filter!) "predicate" "list" "collect"
comment:
#true
(filter!) "predicate" (002) "cdr" "list" "collect"

(filter) "predicate" "list"
comment:
#true
(002) "reverse" (filter!) "predicate" "list" nil

(first!) "integer" "list" "collect"
comment:
(003) "or" (002) "zero?" "integer" (002) "null?" "list"
(002) "reverse" "collect"

(first!) "integer" "list" "collect"
comment:
(002) "not" (002) "pair?" "list"
(002) "reverse" (003) "cons" "list" "collect"

(first!) "integer" "list" "collect"
comment:
(003) ">" "integer" 0
(first!) (003) "-" "integer" 1 (!) "list" (003) "cons" (1) "list" "collect"

(first) "integer" "list"
comment:
(002) "not" (002) "pair?" "list"
"list"

(first) "integer" "list"
comment:
#true
(first!) "integer" "list" nil

(flatten!) list sub-list-found? collect
comment:
(and) (null?) list (not) sub-list-found?
(reverse) collect

(flatten!) list sub-list-found? collect
comment:
(null?) list
(flatten!) (reverse) collect #false nil

(flatten!) list sub-list-found? collect
comment:
(not) (pair?) list
(flatten!) nil sub-list-found? (cons) list collect

(flatten!) list sub-list-found? collect
comment:
(pair?) (1) list
(flatten!) (!) list #true (cons) (!) (1) list (cons) (1) (1) list collect

(flatten!) list sub-list-found? collect
comment:
(null?) (1) list
(flatten!) (!) list sub-list-found? collect

(flatten!) list sub-list-found? collect
comment:
#true
(flatten!) (!) list sub-list-found? (cons) (1) list collect

(flatten) list
comment:
#true
(flatten!) list #false nil

(for-each) effect list
comment:
#true
(003) "for-each" effect list

(function) "expression"
comment:
#true
(002) "caar" "expression"

(function-sort) "sentences"
comment:
#true
(rd) nil "append" (sort) clause-less? (by-four) "sentences"

(functionize) "clause" "arity-hash"
comment:
#true
(004) "list"
      "'define"
      (003) "cons" (clause-name) "clause" (clause-arguments) "clause"
      (003) "cons" "'cond" (003) "append" (condefy) (map-with) schemefy
                                                               (clause-body) "clause"
                                                               "arity-hash"
                                          (002) "list" (003) "list" "'else" err

(gather-count?!) "candidate"
comment:
#true
(008) "and"
      (003) "=" 6 (002) "length" "candidate"
      (003) "eqv?" #Z (002) "car" "candidate"
      (003) "eqv?" #E (002) "cadr" "candidate"
      (003) "eqv?" #D (002) "caddr" "candidate"
      (003) "eqv?" #0 (002) "cadddr" "candidate"
      (digit?) (002) "car" (002) "cddddr" "candidate"
      (digit?) (002) "cadr" (002) "cddddr" "candidate"

(gather-count?) "symbol"
comment:
#true
(gather-count?!) (002) "string->list" (002) "symbol->string" "symbol"

(gather-noun) "sentence"
comment:
(002) "null?" "sentence"
nil

(gather-noun) "sentence"
comment:
(003) "eqv?" #space (002) "car" "sentence"
nil

(gather-noun) "sentence"
comment:
#true
(003) "cons" (002) "car" "sentence" (gather-noun) (002) "cdr" "sentence"

(gather-verb) "sentence"
comment:
(003) "eqv?" #) (002) "car" "sentence"
nil

(gather-verb) "sentence"
comment:
#true
(003) "cons" (002) "car" "sentence" (gather-verb) (002) "cdr" "sentence"

(gr) "garbage" "value"
comment:
#true
(003) "begin" (dr) "garbage" "value"

(grr) "garbage" "value"
comment:
#true
(003) "begin" (drr) "garbage" "value"

(leading-newline) "program"
comment:
(002) "null?" "program"
nil

(leading-newline) "program"
comment:
(003) "eqv?" #newline (002) "car" "program"
(002) "cdr" "program"

(leading-newline) "program"
comment:
#true
"program"

(literal?) "literal-list"
comment:
#true
(003) "eqv?" #" (002) "car" "literal-list"

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (004) "list" #e #r #r
""'err""

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (004) "list" #n #i #l
""'()""

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (004) "list" #a #n #d
""and""

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (003) "list" #o #r
""or""

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (006) "list" #q #u #o #t #e
""quote""

(make-ZED) "ZED-list"
comment:
(003) "equal?" "ZED-list" (003) "list" #s #e
""begin""

(make-ZED) "ZED-list"
comment:
#true
(002) "list->string" (003) "append" (004) "list" #Z #E #D "ZED-list"

(make-character) "noun-list"
comment:
#true
(002) "list->string" (003) "cons" ## (003) "cons" #\ (002) "cdr" "noun-list"

(make-exact) "exact-list"
comment:
#true
(002) "list->string" (003) "append" (003) "list" ## #e "exact-list"

(make-literal) "literal-list"
comment:
#true
(002) "list->string" (002) "cdr" (002) "reverse" (002) "cdr" (002) "reverse" "literal-list"

(make-number-character) "noun-list"
comment:
#true
(002) "list->string" (004) "list" ## #\ (002) "integer->char"
                                         (002) "string->number"
                                          (002) "list->string"
                                           (002) "cddr" "noun-list"

(map-with!) "function" "list" "extra" "collect"
comment:
(002) "null?" "list"
"collect"

(map-with!) "function" "list" "extra" "collect"
comment:
#true
(map-with!) "function"
            (002) "cdr" "list"
            "extra"
            (003) "cons" (003) "function" (002) "car" "list" "extra"
                         "collect"

(map-with) "function" "list" "extra"
comment:
#true
(002) "reverse" (map-with!) "function" "list" "extra" nil

(merge!) "comparator" "list1" "list2" "collect"
comment:
(002) "null?" "list2"
(003) "append" (002) "reverse" "collect" "list1"

(merge!) "comparator" "list1" "list2" "collect"
comment:
(002) "null?" "list1"
(003) "append" (002) "reverse" "collect" "list2"

(merge!) "comparator" "list1" "list2" "collect"
comment:
(003) "comparator" (002) "car" "list2" (002) "car" "list1"
(merge!) "comparator" "list1" (002) "cdr" "list2" (003) "cons" (002) "car" "list2" "collect"

(merge!) "comparator" "list1" "list2" "collect"
comment:
#true
(merge!) "comparator" (002) "cdr" "list1" "list2" (003) "cons" (002) "car" "list1" "collect"

(merge) "comparator" "list1" "list2"
comment:
#true
(merge!) "comparator" "list1" "list2" nil

(mp) "function" "list"
comment:
(002) "null?" "list"
nil

(mp) "function" "list"
comment:
(002) "pair?" "list"
(c) (002) "function" (1) "list" (mp) "function" (!) "list"

(mp) "function" "list"
comment:
#true
(002) "function" "list"

(newline-space!) "program"
comment:
#true
(003) "cons" #newline (newline-space!a) "program"

(newline-space!a) "program"
comment:
#true
(newline-space!aa) "program" (002) "reverse" (newline-space!ab) "program" nil

(newline-space!aa) "program" "transformed"
comment:
(003) "equal?" "program" "transformed"
"program"

(newline-space!aa) "program" "transformed"
comment:
#true
(newline-space!a) "transformed"

(newline-space!ab) "program" "collect"
comment:
(002) "null?" "program"
"collect"

(newline-space!ab) "program" "collect"
comment:
(002) "null?" (002) "cdr" "program"
(003) "cons" (002) "car" "program" "collect"

(newline-space!ab) "program" "collect"
comment:
(003) "and" (003) "eqv?" #newline (002) "car" "program"
            (003) "or" (003) "eqv?" #space (002) "cadr" "program"
                       (003) "eqv?" #tab (002) "cadr" "program"
(newline-space!ab) (002) "cdr" "program" "collect"

(newline-space!ab) "program" "collect"
comment:
#true
(newline-space!ab) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect"

(newline-space) "program"
comment:
#true
(newline-space!) "program"

(newline-space-tab-repeats!) "program" "collect"
comment:
(003) "or" (002) "null?" "program" (002) "null?" (002) "cdr" "program"
(003) "append" "program" "collect"

(newline-space-tab-repeats!) "program" "collect"
comment:
(003) "and" (003) "eqv?" #newline (002) "car" "program"
            (003) "eqv?" #newline (002) "cadr" "program"
(newline-space-tab-repeats!) (002) "cdr" "program" "collect"

(newline-space-tab-repeats!) "program" "collect"
comment:
(003) "and" (003) "or" (003) "eqv?" #space (002) "car" "program"
                       (003) "eqv?" #tab (002) "car" "program"
            (003) "or" (003) "eqv?" #space (002) "cadr" "program"
                       (003) "eqv?" #tab (002) "cadr" "program"
(newline-space-tab-repeats!) (002) "cdr" "program" "collect"

(newline-space-tab-repeats!) "program" "collect"
comment:
#true
(newline-space-tab-repeats!) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect"

(newline-space-tab-repeats) "program"
comment:
#true
(002) "reverse" (newline-space-tab-repeats!) "program" nil

(newlines) "clauses"
comment:
#true
(add-between-ra) (map-with) add-between-ra "clauses" (002) "list" #newline
                 (002) "list" (003) "list" #newline #newline

(normal-character?) "noun-list"
comment:
#true
(003) "eqv?" (002) "car" "noun-list" ##

(not) value
comment:
#true
(002) "not" value

(not-return?) "character"
comment:
#true
(002) "not" (003) "eqv?" #return "character"

(noun!) "noun-list" "number?"
comment:
"number?"
(make-exact) "noun-list"

(noun!) "noun-list" "number?"
comment:
(literal?) "noun-list"
(make-literal) "noun-list"

(noun!) "noun-list" "number?"
comment:
(true?) "noun-list"
""#t""

(noun!) "noun-list" "number?"
comment:
(false?) "noun-list"
""#f""

(noun!) "noun-list" "number?"
comment:
(number-character?) "noun-list"
(make-number-character) "noun-list"

(noun!) "noun-list" "number?"
comment:
(normal-character?) "noun-list"
(make-character) "noun-list"

(noun!) "noun-list" "number?"
comment:
#true
(make-ZED) "noun-list"

(noun) "noun-list"
comment:
#true
(noun!) "noun-list" (002) "string->number" (002) "list->string" "noun-list"

(null?) value
comment:
#true
(002) "null?" value

(number-character?) "noun-list"
comment:
#true
(004) "and"
      (003) "eqv?" (002) "car" "noun-list" ##
      (003) "eqv?" (002) "cadr" "noun-list" #0
      (002) "not" (002) "null?" (002) "cddr" "noun-list"

(pair?) value
comment:
#true
(002) "pair?" value

(pop) "stack"
comment:
#true
(002) "cdr" "stack"

(pr!) "value" "output-string"
comment:
#true
(003) "begin"
      (003) "display" (pr!a) "value" nil
                      "output-string"
      (pr!b) "output-string" (002) "get-output-string" "output-string"

(pr!a) "value" "collect"
comment:
(002) "char?" "value"
(pr!aa) (002) "list->string" (003) "list" ## "value" "collect"

(pr!a) "value" "collect"
comment:
(002) "string?" "value"
(pr!aa) (004) "string-append" ""\""" "value" ""\""" "collect"

(pr!a) "value" "collect"
comment:
(002) "symbol?" "value"
(pr!aa) (002) "symbol->string" "value" "collect"

(pr!a) "value" "collect"
comment:
(002) "number?" "value"
(pr!aa) (002) "number->string" "value" "collect"

(pr!a) "value" "collect"
comment:
(003) "and" (002) "boolean?" "value" "value"
(pr!aa) ""#true"" "collect"

(pr!a) "value" "collect"
comment:
(002) "boolean?" "value"
(pr!aa) ""#false"" "collect"

(pr!a) "value" "collect"
comment:
(002) "null?" "value"
(002) "reverse" "collect"

(pr!a) "value" "collect"
comment:
#true
(pr!a) (!) "value" (003) "cons" (pr!a) (1) "value" nil "collect"

(pr!aa) "string" "collect"
comment:
(002) "null?" "collect"
"string"

(pr!aa) "string" "collect"
comment:
#true
(002) "reverse" (003) "cons" "string" (003) "cons" ""."" "collect"

(pr!b) "output-string" "value"
comment:
#true
(003) "begin" (002) "close-output-port" "output-string" "value"

(pr) "value"
comment:
#true
(pr!) "value" (001) "open-output-string"

(programize) "program" "arity-hash"
comment:
#true
(003) "cons" "'begin" (map-with) functionize "program" "arity-hash"

(push) "object" "stack"
comment:
#true
(003) "cons" "object" "stack"

(rd!) "function" "list"
comment:
(002) "null?" (!) "list"
(1) "list"

(rd!) "function" "list"
comment:
#true
(rd!) "function" (003) "cons" (003) "function" (1) (!) "list" (1) "list" (!) (!) "list"

(rd) "final" "function" "list"
comment:
#true
(rd!) "function" (003) "cons" "final" (002) "reverse" "list"

(read-all!) "collect"
comment:
#true
(read-all!a) (001) "read-char" "collect"

(read-all!a) "character" "collect"
comment:
(002) "eof-object?" "character"
"collect"

(read-all!a) "character" "collect"
comment:
#true
(read-all!) (003) "cons" "character" "collect"

(read-all)
comment:
#true
(002) "reverse" (read-all!) nil

(read-sentence!a) "sentence" "collect"
comment:
(002) "null?" "sentence"
"collect"

(read-sentence!a) "sentence" "collect"
comment:
(003) "eqv?" #space (002) "car" "sentence"
(read-sentence!a) (002) "cdr" "sentence" "collect"

(read-sentence!a) "sentence" "collect"
comment:
(003) "eqv?" #( (002) "car" "sentence"
(read-sentence!aa) "sentence" (gather-verb) (002) "cdr" "sentence" "collect"

(read-sentence!a) "sentence" "collect"
comment:
#true
(read-sentence!ab) "sentence" (gather-noun) "sentence" "collect"

(read-sentence!aa) "sentence" "gather-verb" "collect"
comment:
#true
(read-sentence!a) (tails) (003) "+" 2 (002) "length" "gather-verb"
                          "sentence"
                  (003) "cons" (verb) "gather-verb" "collect"

(read-sentence!ab) "sentence" "gather-noun" "collect"
comment:
#true
(read-sentence!a) (tails) (002) "length" "gather-noun" "sentence"
                  (003) "cons" (noun) "gather-noun" "collect"

(read-sentence!b) "list" "output-string"
comment:
#true
(003) "begin" (003) "display" "list" "output-string"
              (read-sentence!ba) "output-string" (002) "get-output-string" "output-string"

(read-sentence!ba) "output-string" "get-output-string"
comment:
#true
(003) "begin" (002) "close-output-port" "output-string"
              (read-sentence!baa) (002) "open-input-string" "get-output-string"

(read-sentence!baa) "input-string"
comment:
#true
(read-sentence!baaa) "input-string" (002) "read" "input-string"

(read-sentence!baaa) "input-string" "answer"
comment:
#true
(003) "begin" (002) "close-input-port" "input-string" "answer"

(read-sentence) "sentence"
comment:
#true
(read-sentence!b) (002) "reverse" (read-sentence!a) "sentence" nil (001) "open-output-string"

(read-sentences) "sentences"
comment:
#true
(003) "map" read-sentence "sentences"

(ready-clause) "clause"
comment:
#true
(003) "cons" (002) "caaar" "clause" (003) "cons" (002) "cdar" "clause" (002) "cdr" "clause"

(ready-program) "program"
comment:
#true
(003) "map" ready-clause "program"

(replace-trailing-white-space!) "program"
comment:
(002) "null?" "program"
nil

(replace-trailing-white-space!) "program"
comment:
(005) "or"
      (003) "eqv?" (002) "car" "program" #space
      (003) "eqv?" (002) "car" "program" #tab
      (003) "eqv?" (002) "car" "program" #return
      (003) "eqv?" (002) "car" "program" #newline
(replace-trailing-white-space!) (002) "cdr" "program"

(replace-trailing-white-space!) "program"
comment:
#true
(003) "cons" #newline "program"

(replace-trailing-white-space) "program"
comment:
#true
(002) "reverse" (replace-trailing-white-space!) (002) "reverse" "program"

(reverse) list
comment:
#true
(002) "reverse" list

(schemefy!) "expression" "arity-hash" "stack"
comment:
(002) "null?" "expression"
(top) "stack"

(schemefy!) "expression" "arity-hash" "stack"
comment:
(application?) "expression"
(schemefy!a) "expression" "arity-hash" "stack" (function) "expression"

(schemefy!) "expression" "arity-hash" "stack"
comment:
#true
(schemefy!) (002) "cdr" "expression" "arity-hash" (push) (002) "car" "expression" "stack"

(schemefy!a) "expression" "arity-hash" "stack" "function"
comment:
#true
(schemefy!aa) "expression" "arity-hash" "stack" "function" (arity) "arity-hash" "function"

(schemefy!aa) "expression" "arity-hash" "stack" "function" "arity"
comment:
(gather-count?) "function"
(schemefy!) (002) "cdr" "expression" "arity-hash" (push) (first) "arity" "stack"
                                                         (tails) "arity" "stack"

(schemefy!aa) "expression" "arity-hash" "stack" "function" "arity"
comment:
(003) "eq?" "function" "'ZED1"
(schemefy!) (002) "cdr" "expression"
            "arity-hash"
            (push) (005) "list"
                         "'if"
                         (003) "list" "'promise?" (003) "cons" "'car" (first) "arity" "stack"
                         (003) "list" "'force" (003) "cons" "'car" (first) "arity" "stack"
                         (003) "cons" "'car" (first) "arity" "stack"
                   (tails) "arity" "stack"

(schemefy!aa) "expression" "arity-hash" "stack" "function" "arity"
comment:
(003) "eq?" "function" "'ZED!"
(schemefy!) (002) "cdr" "expression"
            "arity-hash"
            (push) (005) "list"
                         "'if"
                         (003) "list" "'promise?" (003) "cons" "'cdr" (first) "arity" "stack"
                         (003) "list" "'force" (003) "cons" "'cdr" (first) "arity" "stack"
                         (003) "cons" "'cdr" (first) "arity" "stack"
                   (tails) "arity" "stack"

(schemefy!aa) "expression" "arity-hash" "stack" "function" "arity"
comment:
(003) "eq?" "function" "'ZEDc"
(schemefy!) (002) "cdr" "expression"
            "arity-hash"
            (push) (003) "cons" "'cons" (003) "map" delay-wrap (first) "arity" "stack"
                   (tails) "arity" "stack"

(schemefy!aa) "expression" "arity-hash" "stack" "function" "arity"
comment:
#true
(schemefy!) (002) "cdr" "expression"
            "arity-hash"
            (push) (003) "cons" "function" (first) "arity" "stack" (tails) "arity" "stack"

(schemefy) "expression" "arity-hash"
comment:
#true
(002) "list" (schemefy!) (002) "reverse" "expression" "arity-hash" (stack)

(sentence-less?) "sentence1" "sentence2"
comment:
(002) "null?" "sentence2"
"#f"

(sentence-less?) "sentence1" "sentence2"
comment:
(002) "null?" "sentence1"
"#t"

(sentence-less?) "sentence1" "sentence2"
comment:
(character-less?) (002) "car" "sentence1" (002) "car" "sentence2"
"#t"

(sentence-less?) "sentence1" "sentence2"
comment:
(character-less?) (002) "car" "sentence2" (002) "car" "sentence1"
"#f"

(sentence-less?) "sentence1" "sentence2"
comment:
#true
(sentence-less?) (002) "cdr" "sentence1" (002) "cdr" "sentence2"

(sentences!) "program" "collect1" "collect2"
comment:
(002) "null?" "program"
"collect2"

(sentences!) "program" "collect1" "collect2"
comment:
(003) "eqv?" #newline (002) "car" "program"
(sentences!) (002) "cdr" "program" nil (003) "cons" (002) "reverse" "collect1" "collect2"

(sentences!) "program" "collect1" "collect2"
comment:
#true
(sentences!) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect1" "collect2"

(sentences) "program"
comment:
#true
(002) "reverse" (sentences!) "program" nil nil

(sort!a) "jumble"
comment:
#true
(003) "map" "list" "jumble"

(sort!b) "comparator" "jumble"
comment:
(002) "null?" "jumble"
nil

(sort!b) "comparator" "jumble"
comment:
(002) "null?" (002) "cdr" "jumble"
"jumble"

(sort!b) "comparator" "jumble"
comment:
#true
(sort!b) "comparator"
         (003) "cons" (merge) "comparator" (002) "car" "jumble" (002) "cadr" "jumble"
                      (sort!b) "comparator" (002) "cddr" "jumble"

(sort) "comparator" "jumble"
comment:
#true
(002) "car" (sort!b) "comparator" (sort!a) "jumble"

(space-newline!) "program" "collect"
comment:
(003) "or" (002) "null?" "program" (002) "null?" (002) "cdr" "program"
(003) "append" "program" "collect"

(space-newline!) "program" "collect"
comment:
(003) "and" (003) "eqv?" #space (002) "car" "program"
            (003) "eqv?" #newline (002) "cadr" "program"
(space-newline!) (002) "cdr" "program" "collect"

(space-newline!) "program" "collect"
comment:
#true
(space-newline!) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect"

(space-newline) "program"
comment:
#true
(002) "reverse" (space-newline!) "program" nil

(stack)
comment:
#true
nil

(tab-replace!) "program" "collect"
comment:
(002) "null?" "program"
"collect"

(tab-replace!) "program" "collect"
comment:
(003) "eqv?" #tab (002) "car" "program"
(tab-replace!) (002) "cdr" "program" (003) "cons" #space "collect"

(tab-replace!) "program" "collect"
comment:
#true
(tab-replace!) (002) "cdr" "program" (003) "cons" (002) "car" "program" "collect"

(tab-replace) "program"
comment:
#true
(002) "reverse" (tab-replace!) "program" nil

(tails) "number" "list"
comment:
(002) "null?" "list"
nil

(tails) "number" "list"
comment:
(002) "zero?" "number"
"list"

(tails) "number" "list"
comment:
(003) ">" "number" 0
(tails) (003) "-" "number" 1 (002) "cdr" "list"

(top) "stack"
comment:
#true
(002) "car" "stack"

(true?) "noun-list"
comment:
#true
(003) "equal?" "noun-list" (006) "list" ## #t #r #u #e

(verb) "verb-list"
comment:
(literal?) "verb-list"
(002) "list" (make-literal) "verb-list"

(verb) "verb-list"
comment:
#true
(002) "list" (make-ZED) "verb-list"

(write-all) "program"
comment:
(002) "null?" "program"
err

(write-all) "program"
comment:
#true
(write-all) (003) "begin" (002) "write-char" (002) "car" "program" (002) "cdr" "program"

(zed->scheme!) "value" "collect"
comment:
(002) "null?" "value"
(002) "reverse" "collect"

(zed->scheme!) "value" "collect"
comment:
#true
(zed->scheme!) (!) "value" (003) "cons" (zed->scheme) (1) "value" "collect"

(zed->scheme) "value"
comment:
(002) "not" (002) "pair?" "value"
"value"

(zed->scheme) "value"
comment:
#true
(zed->scheme!) "value" nil
```



## zkl

zkl is written in zkl. The compiler is big. The VM has a compiled (C) image of the compile as a big table, ie the system is image based. Here is an example of compiling and testing a new compiler:

```txt

$ zkl --package Src/Compiler/compiler.zkl
Compiling asm
Compiling parser
Compiling tokenizer
Building wad: .... 106,072 bytes added
Checking 4 items in Wad
Wad header
  Protocol:          1.0
  Infomercial:       zkl Wad
  Time Stamp:        Sun Jun 15 20:42:11 2014
  Number of objects: 4
  Data size:         106,072 bytes
  Run list:          5444
  Flags:
  Class Name                                         Offset    Size  Run
  ----- ----                                         ------    ----  ---
  Compiler.Compiler                                       0  28,930   5
  Compiler.Asm                                       28,930  15,001   4
  Compiler.Parser                                    43,931  55,129   4
  Compiler.Tokenizer                                 99,060   7,012   4
....
Compiled package in 2.9 seconds
zkl 1.12.11, released 2014-07-01
zkl: ^D

$ zklIgnoreWad=1 zkl  //ignore compiled in image and load just compiled one
Ignoring wad
zkl 1.12.11, released 2014-07-01

```


Since the compiler is part of the VM, you can compile from the REPL:

```txt

zkl: Compiler.Compiler.packageFile("compiler.zkl")
Compiling asm
Compiling parser
Compiling tokenizer
...

```

