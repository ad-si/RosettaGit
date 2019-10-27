+++
title = "Language Comparison Table"
description = ""
date = 2019-08-24T21:42:18Z
aliases = []
[extra]
id = 2925
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]
{|class="wikitable sortable"
![[:Category:Programming Languages|Language]]
![[:Category:Programming paradigm|Paradigm(s)]]
!Standardized
![[Type strength]]
![[Type safety]]
!Expression of types 
![[Type compatibility]]
![[Type checking]]
![[Parameter Passing]] Methods Available
![[Garbage collection]]
!Intended use 
!Design goals 
|-


! {{anchor|ACL2|[[ACL2]]}}
|[[functional programming|functional]]
|{{yes}}
|strong
|unsafe
|implicit
|
|dynamic
|immutable reference
|{{yes}}
|Theorem proving
|Be logically sound, mimic Common Lisp
|-


! {{anchor|ActionScript|[[ActionScript]]}}
|[[imperative programming|imperative]], [[object-oriented]],[[:Category:Programming paradigm/Distributed|distributed]]
|{{yes|Yes, [[wp:Ecma_International|ECMA]]}}
|strong
|safe
|
|
|static
|
|{{yes}}
|Web design
|
|-


! {{anchor|Ada|[[Ada]]}}
|[[concurrent programming|concurrent]], [[:Category:Programming paradigm/Distributed|distributed]], [[generic programming|generic]], [[imperative programming|imperative]], [[object-oriented]]
|{{yes|Yes, [[ANSI]], [[ISO]], ANSI/MIL-STD-1815A-1983, [http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=22983 ISO/IEC 8652], [http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=38828 ISO/IEC TR 24718], [http://vak.ru/lib/exe/fetch.php/book/gost/pdf/gost-27831-88.pdf GOST 27831-88]}}
|strong
|safe
|explicit
|nominative
|static
|by value, by reference (compiler chosen if not determined by the data type)
|{{optional|GC is allowed, but not mandated}}
|Embedded, [[real-time computing|real-time]], mission-critical, long-lived, and large scale systems
|Reliability and maintenance, Programming as a human activity, Efficiency [http://www.adaic.com/standards/05rm/html/RM-0-3.html Language Reference Manual]
|-


! {{anchor|ALGOL_68|[[ALGOL 68]]}}
|[[concurrent programming|concurrent]], [[imperative programming|imperative]] 
|{{yes|Yes, [http://vak.ru/lib/exe/fetch.php/book/gost/pdf/gost-27974-88.pdf GOST 27974-88]}}
|soft, weak, meek, firm and strong - depending on context.
|safe
|explicit
|structural
|static or dynamic (programmer chosen)
|by value or by reference (programmer chosen)
|{{yes}}
|Application
|Completeness and clarity of design, Orthogonal design, Security, Efficiency (Static mode checking, Mode-independent parsing, Independent compilation, Loop optimization, Representations in minimal & larger character sets)
|-


! {{anchor|ALGOL_W|[[ALGOL W]]}}
|[[imperative programming|imperative]] 
|{{no}}
|strong
|safe
|explicit
|nominative
|static or dynamic (programmer chosen)
|by value, by reference or by name (programmer chosen)
|{{yes}}
|Application, Education
|
|-


! {{anchor|AmbientTalk|[[AmbientTalk]]}}
|[[concurrent programming|concurrent]]
|
|strong
|safe
|explicit
|duck
|dynamic
|by value
|
|Mobile ad hoc networks
|
|-


! {{anchor|AutoHotkey|[[AutoHotkey]]}}
|[[imperative programming|imperative]] 
|{{no}}
|untyped
|
|
|
|
|by value or by reference
|{{no}}
|End User Programming
|simplicity, speed, stability
|-


! {{anchor|AutoIt|[[AutoIt]]}}
|[[imperative programming|imperative]] 
|
|
|
|
|
|
|by value or by reference
|
|General, scripting, GUI and tasks automation
|Simplicity
|-


! {{anchor|BASIC|[[BASIC]]}}
|[[procedural programming|procedural]]
|{{yes|Yes, [[ANSI]], [[ISO]]}}
|varies by dialect
|
|
|
|
|
|{{optional|varies by dialect}}
|Education
|Simplicity
|-


! {{anchor|C|[[C]]}}
|[[imperative programming|imperative]]
|{{yes|Yes, [[ANSI]] [[C89]], [[ISO]] [[C90]]/[[C99]]}}
|weak
|unsafe
|explicit
|nominative
|static
|by value, by reference (through pointers)
|{{optional}} through [[wp:Boehm-GC|external tools]]
|System, Embedded
|Low level access, Minimal constraint
|-


! {{anchor|C_sharp|[[C sharp|C#]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]], [[reflective programming|reflective]], [[functional programming|functional]], [[event-driven programming|event-driven]]
|{{yes|Yes, [[wp:Ecma_International|ECMA]], [[ISO]]}}
|strong
|safe (unsafe allowed)
|implicit
|nominative
|static, dynamic (for interop)
|by value, by reference (through managed pointers [explicitly in, out, or in-out])
|{{yes}}
|Application
|Rapid application development
|-


! {{anchor|C++|[[C++]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]]
|{{yes|Yes, [[ISO]] [[C++98]]/[[C++11]]}}
|strong
|safe (unsafe allowed)
|explicit, partially implicit
|nominative, structural
|static, dynamic
|by value, by reference (through reference types)
|{{optional}} through [[wp:Boehm-GC|external tools]]
|Application, System
|Abstraction, Efficiency, Compatibility
|-


! {{anchor|Chapel|[[Chapel]]}}
|[[concurrent]], [[generic programming|generic]], [[object-oriented]], [[imperative programming|imperative]], [[:Category:Programming paradigm/Distributed|distributed]], [[reflective programming|reflective]]
|{{no|No, still under development, a [http://chapel.cray.com/spec/spec-0.93.pdf preliminary language specification] exists}}
|strong
|safe
|explicit, partially inferred
|nominative
|static
|by value, by reference
|No
|High Performance Computing
|Programmer productivity (compared to C/Fortran), performance
|-


! {{anchor|Clean|[[Clean]]}}
|[[functional programming|functional]], [[generic programming|generic]]
|{{no}} 
|strong
|
|implicit
|
|static
|
|{{yes}}
|General
|Correctness, Modularity
|-


! {{anchor|Clojure|[[Clojure]]}}
|[[functional programming|functional]], [[concurrent programming|concurrent]]
|
|strong
|safe
|
|
|dynamic, static
|
|{{yes}}
|
|
|-


! {{anchor|COBOL|[[COBOL]]}}
|[[imperative programming|imperative]], [[object-oriented]]
|{{yes|Yes, [[ANSI]] 68, 74, 85 (and subsequent revisions); [[ISO]] 2002, 2014}}
|strong
|safe
|explicit
|
|static
|by value, by reference
|{{no}}
|Business and Financial Applications
|Readability
|-


! {{anchor|ColdFusion|[[ColdFusion]]}}
|[[procedural programming|procedural]], [[object-oriented]]
|{{no}}
|weak
|
|implicit
|
|dynamic
|
|
|Web Development
|Rapid Application Development, Ease of use
|-


! {{anchor|Common Lisp|[[Common Lisp]]}}
|[[imperative programming|imperative]], [[functional programming|functional]], [[object-oriented]]
|{{yes}}
|strong
|safe
|
|
|dynamic, static
|
|{{yes}}
|General
|Standardize [[Lisp]]
|-


! {{anchor|D|[[D]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]]
|{{no}}
|strong
|safe
|explicit, optional inferred
|nominative, structural
|static, dynamic
|by value, by reference (through reference types)
|{{yes}}
|Application, System
|Compilability, Correctness, Efficiency
|-


! {{anchor|Dao|[[Dao]]}}
|[[object-oriented]]
|
|strong
|safe, unsafe allowed
|implicit, explicit
|
|static, dynamic
|
|{{yes}}
|
|
|-

! {{anchor|Dyalect|[[Dyalect]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]]
|{{no}}
|strong
|safe
|implicit
|duck
|dynamic
|by reference
|{{yes}}
|Application
|Portable dynamic scripting language with consistent design and modern syntax 
|-

! {{anchor|Dylan|[[Dylan]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]], [[procedural programming|procedural]]
|
|strong
|safe
|
|
|static, dynamic
|
|{{yes}}
|
|
|-


! {{anchor|E|[[E]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[object-capability]], [[communicating event loops]]
|<!-- Std -->{{no}}, still in development
|<!-- Strength -->strong (runtime)
|<!-- Safety -->safe
|<!-- Expr -->optional explicit
|<!-- Compat -->duck
|<!-- Check -->dynamic
|<!-- Passing -->by value
|<!-- GC -->{{yes}}
|<!-- Uses -->Secure collaboration, distributed systems, running untrusted applications, high-level "scripting"
|<!-- Design goals -->Familiarity to [[C]]/[[Java]]/[[JavaScript|JS]] users; less error-prone concurrency & security
|-


! {{anchor|EC|[[eC]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]], [[reflective programming|reflective]]
|{{no}}
|weak
|unsafe
|explicit, partially implicit
|nominative, complex conversions system
|static, dynamic
|according to data type, or explicitly by value, by reference
|{{optional|Reference counting}}
|Applications, GUIs, System, Games
|Elegance, Performance, Simplicity, Lightness, 99.9% [[C]] compatibility
|-


! {{anchor|Eiffel|[[Eiffel]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]] 
|{{yes|Yes, [http://www.ecma-international.org/publications/standards/Ecma-367.htm ECMA-367], [http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=42924 ISO/IEC 25436:2006]}}
|strong
|safe
|
|nominative
|static
|
|{{yes}}
|Application
|Correctness, Efficiency, Design by contract

|-


! {{anchor|Ela|[[Ela]]}}
|[[functional programming|functional]]
|{{no}}
|strong
|safe
|implicit
|duck
|dynamic
|by reference
|{{yes}}
|Application
|
|-

|-


! {{anchor|Elm|[[Elm]]}}
|[[functional programming|functional]], functional reactive programming, [[declarative programming|declarative]], pattern matching
|{{no}}
|strong
|safe
|inferred, optional explicit annotations
|
|static
|immutable data structures, syntactically by value, time-varying with Signals
|{{yes}}
|Web Development, GUIs, Applications, Games
|Simple, Modular, Safe, Reactive
|-

|-


! {{anchor|Elena|[[Elena]]}}
|[[object-oriented]] 
|
|strong
|safe
|implicit
|
|dynamic
|
|{{yes}}
|
|
|-


! {{anchor|Erlang|[[Erlang]]}}
|[[functional programming|functional]], [[concurrent programming|concurrent]], [[:Category:Programming paradigm/Distributed|distributed]], [[declarative programming|declarative]] - pattern matching, [[imperative programming|imperative]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|immutable data structures, syntactically by value but safe sharing of compound data types
|{{yes}}
|Telecom and mission critical distributed applications
|Fault tolerance, Reliability - Nonstop Running, Hot Code Change, Safety, Concurrency, Distribution, Scalability
|-


! {{anchor|ERRE|[[ERRE]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]]
|{{no}}
|weak
|unsafe
|explicit
|
|static and dynamic
|by value
|{{yes}}
|Education
|Readability, Modularity
|-


! {{anchor|Factor|[[Factor]]}}
|[[stack]]-oriented
|<!-- Std -->{{no}}
|<!-- Strength -->
|<!-- Safety -->safe
|<!-- Expr -->implicit
|<!-- Compat -->duck
|<!-- Check -->dynamic
|<!-- Passing -->by reference
|<!-- GC -->{{yes}}
|<!-- Uses -->
|<!-- Design goals -->x
|-


! {{anchor|Forth|[[Forth]]}}
|[[imperative programming|imperative]], [[stack]]-oriented
|{{yes|Yes, [[ANSI]]}}
|none
|n/a
|n/a
|n/a
|n/a
|
|{{no}}
|Applications, High reliability, Embedded systems, Booting new hardware.
|Compact implementations, Low level access, Interactive programming, CPU agnostic Assembler Alternative, ultra-small memory systems.
|-


! {{anchor|Fortran|[[Fortran]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]], [[object-oriented]], (partially) [[generic programming|generic]], [[concurrent programming|concurrent]]
|{{yes|Yes, [[ISO]], [http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=39691 ISO/IEC 1539-1:2004]}}
|strong
|safe
|explicit, partially implicit
|nominative
|static, dynamic
|by reference, or explicitly by value
|{{no}}
|Scientific and numeric applications
|Runtime efficiency, Simple syntax
|-


! {{anchor|FutureBasic|[[FutureBasic]]}}
|[[procedural programming|procedural]]
|{{no|No}}
|strong
|safe
|
|
|
|by value, by reference (through pointers)
|{{no|No}}
|Educational, Prototyping, Commerical Applications
|Readability, Simplicity, Compliability, Freeware, Efficiency
|-


! {{anchor|Gambas|[[Gambas]]}}
|[[object-oriented|object-oriented]]
|<!-- Std -->{{no}}
|<!-- Strength -->strong
|<!-- Safety -->safe
|<!-- Expr -->explicit
|<!-- Compat -->
|<!-- Check -->dynamic
|<!-- Passing -->by value, by reference
|<!-- GC -->
|<!-- Uses -->Application, Scripting 
|<!-- Design goals -->Rapid application development, Visual Basic alternative
|-


! {{anchor|Go|[[Go]]}}
|[[concurrent programming|concurrent]]
|<!-- Std -->{{no}}, [http://golang.org/ref/spec language specification] available
|<!-- Strength -->strong
|<!-- Safety -->safe
|<!-- Expr -->explicit, optionally inferred
|<!-- Compat -->nominative; structural for interface types
|<!-- Check -->static
|<!-- Passing -->by value
|<!-- GC -->{{yes}}
|<!-- Uses -->
|<!-- Design goals -->
|-


! {{anchor|Gosu|[[Gosu]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]], [[functional programming|functional]]
|{{yes|Yes}}
|strong
|safe
|explicit, optionally inferred
|nominative and structural
|static
|by value
|{{yes}}
|Application
|Open type system, optional dynamic loading 
|-


! {{anchor|Groovy|[[Groovy]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[wp:Aspect-oriented_programming|aspect-oriented]]
|{{no}}
|strong
|safe
|implicit
|nominative
|dynamic
|
|{{yes}}
|Application
|[[JVM]] compatibility
|-


! {{anchor|Haskell|[[Haskell]]}}
|[[functional programming|functional]], [[generic programming|generic]], [[lazy evaluation]] 
|{{yes|Yes, [http://www.haskell.org/onlinereport/haskell2010/ Haskell 2010 Report], [http://www.haskell.org/onlinereport/ Haskell 98 Report]}}
|strong
|safe
|inferred, optional explicit annotations
|polymorphic structural
|static 
|
|{{yes}}
|Application, Research
|[[lazy evaluation]], Teaching and research, completely formally described [http://haskell.org/onlinereport/preface-jfp.html Report Preface]
|-

! {{anchor|Huginn|[[Huginn]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]]
|{{no|No, [http://huginn.org/referece language reference] included with each version's documentation}}
|strong
|safe
|implicit
|
|dynamic
|by reference
|{{optional|Reference counting}}
|Education, Application, Scripting, Interactive system shell
|Consistency, Simplicity, Readability, Expressiveness, Modularity
|-

! {{anchor|Icon|[[icon]]}}
| [[procedural programming|procedural]], [[structured]], [[Goal_directed_programming|goal directed]], [[string scanning]], [[co-expressions]]
| {{no}}
| strong
| Safe
| implicit
| nominative
| dynamic
| by value and safe reference depending if the type is mutable or immutable
| {{yes}}
| Text analysis, text editing, document formatting, artificial intelligence, expert systems, rapid prototyping, symbolic mathematics, text generation, and data laundry 
| Facilitates ease of programming, short concise programs, automatic storage allocation and management, provide a rich base of basic data types and structures, and platform independence.  Also see [[#Unicon|Unicon]] for the unified extended dialect of Icon. 
|-


! {{anchor|Io|[[Io]]}}
|[[object-oriented]], prototype
|{{no}}
|strong 
|
|
|
|dynamic
|
|{{yes}}
|
|
|-


! {{anchor|J|[[J]]}}
|[[array]] programming, function-level programming, [[tacit programming|tacit]]
|{{no}}
|strong
|safe
|inferred
|structural
|dynamic
|by value, by name, by address for memory mapped files (and, indirectly, for foreign languages)
|{{yes}}
|Data processing, expressing concepts algorithms and architectures
|Describe computers mathematically and concisely
|-


! {{anchor|Java|[[Java]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]], [[reflective programming|reflective]]
|{{yes|Yes, [http://docs.oracle.com/javase/specs/index.html Java SE Specifications]}}
|strong
|safe
|explicit
|nominative
|static
|by value
|{{yes}}
|Application
|Write once run anywhere
|-


! {{anchor|JavaScript|[[JavaScript]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]], [[reflective programming|reflective]]
|{{yes|Yes, [[ECMAScript standard]]}}
|weak 
|
|implicit
|
|dynamic
|by value
|{{yes}}
|Client side web scripting
|
|-


! {{anchor|Joy|[[Joy]]}}
|[[functional programming|functional]], [[stack]]-oriented
|{{no}}
|strong
|safe
|
|
|dynamic
|
|
|[[functional programming|functional]] research
|[[concatenative programming|concatenative]]
|-


! {{anchor|Kabap|[[Kabap]]}}
|[[imperative programming|imperative]], [[Dynamic programming|dynamic]], extensible
|{{yes}}
|weak
|unsafe
|implicit
|duck
|dynamic
|by value
|
|User scripting, general
|Safe sandboxed, easy to learn, easier to implement. Use to avoid eval() & reflection
|-


! {{anchor|Kotlin|[[Kotlin]]}}
|[[imperative programming|imperative]], [[object-oriented]],
[[procedural programming|procedural]], [[functional programming|functional]],
[[generic programming|generic]], [[reflective programming|reflective]]
|{{no}}
|strong
|safe
|explicit, optionally inferred
|nominative
|static, dynamic (JS only)
|by value (read-only)
|{{yes}}
|Application
|Pragmatic language for modern multi-platform applications with compilers for JVM, JS and Native code
|-

! {{anchor|LDPL|[[LDPL]]}}
|[[imperative programming|imperative]]
|{{yes|Yes, [https://ldpl.lartu.net/reference LDPL Standard 19]}}
|strong
|safe
|explicit
|
|static
|
|{{no}}
|Portable applications, readable source codes, teaching
|Readability
|-


! {{anchor|LFE|[[LFE]]}}
|[[functional programming|functional]], [[concurrent programming|concurrent]], [[:Category:Programming paradigm/Distributed|distributed]], [[declarative programming|declarative]] - pattern matching, [[imperative programming|imperative]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|immutable data structures, syntactically by value but safe sharing of compound data types
|{{yes}}
|Telecom and distributed applications
|Fault tolerance, Reliability - Nonstop Running, Hot Code Change, Safety, Concurrency, Distribution, Scalability
|-


! {{anchor|Lisp|[[Lisp]]}}
|[[functional programming|functional]], [[reflective programming|reflective]]; others vary by dialect
|{{no}}
|strong
|
|
|
|dynamic
|
|{{yes}}
|General
|Simple notation for Lambda calculus, Homoiconicity
|-


! {{anchor|Logo|[[Logo]]}}
|[[procedural programming|procedural]], [[functional programming|functional]]
|{{no}}
|strong
|safe
|implicit
|structural
|dynamic
|
|{{yes}}
|Education
|Simple syntax, Turtle graphics, Interactive programming
|-


! {{anchor|Lua|[[Lua]]}}
|[[procedural programming|procedural]], [[imperative programming|imperative]], [[reflective programming|reflective]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|
|{{yes}}
|Host-driven Scripting
|Small, embedded, configuration.
|-


! {{anchor|Lucid|[[Lucid]]}}
|[[dataflow programming|dataflow]], [[functional programming|functional]]
|{{no}}
|strong
|safe 
|
|
|dynamic
|
|
|stream processing
|dataflow
|-


! {{anchor|Luck|[[Luck]]}}
|[[imperative programming|imperative]], [[functional programming|functional]]
|{{no}}
|weak
|unsafe 
|explicit, partially inferred
|nominative
|static
|by value or by reference
|{{yes}}
|systems programming
|fast, C compatible, high-level language
|-


! {{anchor|Mathematica|[[Mathematica]]}}
|[[functional programming|functional]], [[procedural programming|procedural]]
|{{no}}
|strong
|
|
|
|dynamic
|
|{{yes}}
|Numeric and symbolic computation, visualization
|
|-


! {{anchor|MATLAB|[[MATLAB]]}}
|[[procedural programming|procedural]], [[imperative programming|imperative]], [[array]] programming
|{{no|No, however the language is also implemented in [[Octave]] and [[FreeMat]]}}
|strong
|unsafe
|
|
|dynamic
|by value
|{{yes}}
|Numeric computation and visualization
|At the beginning designed as interpreter for easy use of fortran libraries, nowadays high performance numerical analysis and visualization
|-


! {{anchor|MAXScript|[[MAXScript]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[procedural programming|procedural]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|by value, by reference
|
|3D Art Tools, Scripting
|Simplicity, Accessibilty
|-


! {{anchor|Modula-3|[[Modula-3]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[procedural programming|procedural]], [[generic programming|generic]]
|{{no}}
|strong
|safe (unsafe allowed)
|explicit
|structural
|static
|by value, by reference
|{{yes}}
|Application, Systems
|Simple, Object oriented
|-


! {{anchor|MUMPS|[[MUMPS]]}}
|[[procedural programming|procedural]]
|{{yes|Yes, [[ISO]]}}
|untyped
|not applicable
|
|
|
|by value, by reference
|{{yes}}
|
|Built-in Persistent Storage
|-


! {{anchor|Nial|[[Nial]]}}
|[[functional programming|functional]], [[array]] programming
|{{no}}
|strong
|
|
|
|dynamic
|
|
|Data processing
|
|-


! {{anchor|Nim|[[Nim]]}}
|[[procedural programming|procedural]], [[functional programming|functional]], [[generic programming|generic]]
|{{no}}
|strong
|safe
|explicit
|nominative, structural
|static
|by value, by reference (through reference types)
|{{Yes}}
|General, Application, Systems, Games, Scripting, Web
|Efficient, Expressive, Elegant in that order.
|-


! {{anchor|Oberon-2|[[Oberon-2]]}}
|[[procedural programming|procedural]], [[imperative programming|imperative]], [[object-oriented]]
|{{no}}
|strong
|safe
|explicit
|structural
|static
|by value, by reference
|{{yes}}
|Teaching, System
|Simplicity
|-


! {{anchor|Objeck|[[Objeck]]}}
|[[object-oriented]],[[functional programming|functional]]
|{{no}}
|strong
|safe
|explicit
|nominative
|static
|by value
|{{yes}}
|General, Education
|Minimalist, Cross-Platform, Modular
|-


! {{anchor|Object Pascal|[[Object Pascal]]}} ({{anchor|Delphi|[[Delphi]]}})
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]]
|{{no}}
|strong
|safe (unsafe allowed)
|explicit
|nominative
|static, dynamic
|by reference, by value
|{{optional|some types}}
|Application, System
|Readability, Rapid application development, Modularity
|-


! {{anchor|Objective-C|[[Objective-C]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[reflective programming|reflective]], [[generic programming|generic]] (as of Xcode 7)
|{{no}}
|weak
|
|explicit
|
|static
|by value
|{{yes|Yes (as of 2.0)}}
|Application 
|[[Smalltalk]] like, Component based code reuse, [[C]]/[[C++]] compatibility
|-


! {{anchor|OCaml|[[OCaml]]}}
|[[object-oriented]], [[functional programming|functional]], [[imperative programming|imperative]], [[generic programming|generic]]
|{{no|No, [http://caml.inria.fr/pub/docs/manual-ocaml/index.html the manual] includes language specification}}
|strong
|safe
|inferred, optional explicit annotations
|polymorphic structural
|static
|by value
|{{yes}}
|Application
|Efficiency, Robustness, Correctness
|-


! {{anchor|Oz|[[Oz]]}}
|logic programming, [[functional programming|functional]], [[imperative programming|imperative]], [[object-oriented]], [[concurrent programming|concurrent]]
|{{no}}
|strong
|safe
|implicit
|structural
|dynamic
|by reference (though often immutable)
|{{yes}}
|Application, Education, Distribution
|
|-


! {{anchor|PARI/GP|[[PARI/GP]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]]
|{{no}}
|weak
|unsafe
|implicit
|nominal
|dynamic
|by value, by reference
|{{yes}}
|Mathematics, especially number theory
|
|-


! {{anchor|Pascal|[[Pascal]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]]
|{{yes|Yes, Extended Pascal standardized under [[ISO]]}}
|strong
|safe
|explicit
|
|static (some dialects also dynamic)
|by reference, by value
|{{no}}
|General, Application, Education, System
|Readability, Discipline, Modularity
|-


! {{anchor|Perl|[[Perl]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]], [[reflective programming|reflective]], [[functional programming|functional]], [[object-oriented]], [[generic programming|generic]]
|{{no}}
|weak
|
|implicit
|
|dynamic, static
|by reference
|{{optional|Reference counting}}
|Text processing, Scripting
|Terseness, Expressiveness
|-


! {{anchor|Perl 6|[[Perl 6]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]], [[reflective programming|reflective]], [[functional programming|functional]], [[object-oriented]], [[generic programming|generic]], [[lazy evaluation]], multiple dispatch, metaprogramming
|{{yes|Yes, [http://perlcabal.org/syn/#doctable Perl 6 Synopses]}}
|strong
|safe, unsafe
|optional explicit
|nominal (duck and structural available via constraints, patterns, generics, and gradual typing)
|dynamic, static
|by value, by reference
|{{yes}}
|Application, text processing, scripting
|Expressiveness, generality
|-


! {{anchor|Phix|[[Phix]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]]
|<!-- Std -->{{no}}
|<!-- Strength -->strong
|<!-- Safety -->safe
|<!-- Expr -->explicit, partially implicit
|<!-- Compat -->duck
|<!-- Check -->dynamic, static
|<!-- Passing -->copy on write, immutable reference, multiple returns
|<!-- GC -->{{optional|Reference counting}}
|<!-- Uses -->Application, General, High-level scripting, Text processing
|<!-- Design goals -->Simplicity, Readability, Facilitate ease of programming and maintenance
|-


! {{anchor|PHP|[[PHP]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[reflective programming|reflective]]
|{{no}}
|weak
|
|
|
|dynamic
|by value, by reference
|{{yes}}
|Web Application, CLI
|Robustness and Simplicity
|-


! {{anchor|Pike|[[Pike]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]], [[functional programming|functional]], [[object-oriented]], [[reflective programming|reflective]], [[event-driven programming|event-driven]]
|{{no}}
|strong
|safe
|explicit
|structural
|dynamic, static
|by value, by reference
|{{yes}}
|Application, scripting
|optimized execution, efficient networking
|-


! {{anchor|Pop11|[[Pop11]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|
|{{yes}}
|Application, Education
|
|-


! {{anchor|Prolog|[[Prolog]]}}
|logic programming
|{{yes|Yes, [[ISO]]}}
|strong
|
|
|
|dynamic
|
|{{yes}}
|Problem solving, Artificial intelligence
|[[declarative programming|declarative]]
|-


!{{anchor|Pure|[[Pure]]}}
|[[dynamic programming|dynamic]], [[functional programming|functional]]
|
|strong
|
|
|structural
|dynamic
|
|{{yes}}
|
|
|-


! {{anchor|Python|[[Python]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]], [[wp:Aspect-oriented_programming|aspect-oriented]], [[reflective programming|reflective]]
|{{no|No, [http://docs.python.org/reference/index.html language reference] included with each version's documentation}}
|strong
|safe
|implicit
|
|dynamic
|by value ([[wp:Python_syntax_and_semantics#Data_structures|Call by object reference]])
|{{yes}}
|Application, Education, Scripting
|Simplicity, Readability, Expressiveness, Modularity
|-


! {{anchor|Ra|[[Ra]]}}
|[[object-oriented]]
|{{no}}
|dynamic or static
|safe
|implicit or explicit
|
|dynamic or static
|
|{{yes}}
|General
|Program in native language, clean and clear syntax, extensive standard library, convenience
|-


! {{anchor|Racket|[[Racket]]}}
|[[functional programming|functional]], [[imperative programming|imperative]], [[object-oriented]], [[reflective programming|reflective]]
|{{no}}
|strong
|safe
|implicit or explicit (see [http://docs.racket-lang.org/ts-guide/index.html Typed Racket])
|
|dynamic or static
|by value
|{{yes}}
|General
|Extensibility, Simplicity, Modularity
|-


! {{anchor|RapidQ|[[RapidQ]]}}
|[[imperative programming|imperative]], component-oriented programming, [[event-driven programming|event-driven]]
|{{no}}
|strong (none for Variant type)
|safe
|
|
|static
|by reference, by value
|
|Application
|Rapid application development, Simplicity, [[BASIC]] compatibility
|-


! {{anchor|R|[[R]]}}
|[[functional programming|functional]], [[imperative programming|imperative]], [[reflective programming|reflective]], [[array]]
|{{no}}
|strong
|safe
|implicit
|Duck, structural
|dynamic
|value by need, by name (programmer chosen)
|{{yes}}
|Statistics, Numerical computation, Visualization, Education
|Expressiveness, interactive manipulation and analysis of datasets
|-


! {{anchor|REXX|[[REXX]]}}   ({{anchor|REXX|[[Classic REXX]]}})
| [[dynamic programming|dynamic]],   [[procedural programming|procedural]],   [[functional programming|functional]] 
|<!-- Standardized        --> {{yes|Yes.   There is a ANSI standard (X3.274-1996),   but some REXX implementations don't adhere to it.}}
|<!-- Type strength       -->strong
|<!-- Type safety         -->safe
|<!-- Expression of types -->inferred
|<!-- Type compatibility  -->nominal
|<!-- Type checking       -->dynamic   (but only when both comparands are numeric ''and'' non-strict comparisons are used)
|<!-- Parmameter Passing  -->by value
|<!-- Garbage collection  -->{{optional| garbage collection is allowed (and in most cases, automatic), but not mandated}}
|<!-- Intended use        -->general, application, algorithms, scripting, host/sub-system scripting/interfacing, data/text processing, programming as a human activity 
|<!-- Design goals        -->designed to make programming easier; to foster high quality programs by making writing them as simple and enjoyable as possible; designing each part of the language for people to use is more important than providing easy implementation; principle of least astonishment. 
|-


! {{anchor|Ruby|[[Ruby]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]], [[reflective programming|reflective]]
|{{yes|Yes, JIS X 3017 (2011), ISO/IEC 30170 (2012)}}
|strong
|
|implicit
|
|dynamic
|by value (Call by object reference)
|{{yes}}
|Application, Scripting
|Expressiveness, Readability
|-

! {{anchor|Rust|[[Rust]]}}
|[[concurrent programming|concurrent]], [[functional programming|functional]], [[imperative programming|imperative]], [[structured]], [[generic programming|generic]]
|{{no}}
|strong
|safe
|explicit, optionally inferred
|nominal
|static
|by value, by reference
|{{optional|resource acquisition is initialization (RAII), optional reference counting}}
|Application, System
|Highly concurrent and highly safe systems
|-


! {{anchor|Scala|[[Scala]]}}
|[[object-oriented]], [[functional programming|functional]], [[generic programming|generic]]
|{{yes|Yes, [http://www.scala-lang.org/docu/files/ScalaReference.pdf The Scala Language Specification Version 2.9]}}
|strong
|safe
|partially implicit
|nominative, structural
|static
|by value, by name
|{{yes}}
|General, Education, Parallel computing, DSL and scripting.
|Concise, Type-safe, Integrate OO and functional paradigms, Scalable and Elegance. Platform independent 
|-


! {{anchor|Scheme|[[Scheme]]}}
|[[functional programming|functional]]
|{{yes|Yes, [http://www.schemers.org/Documents/Standards/R5RS/ R<sup>5</sup>RS], [http://www.r6rs.org/ R<sup>6</sup>RS]}}
|strong
|safe
|implicit
|
|dynamic (latent)
|by value
|{{yes}}
|General, Education
|Minimalistic, Lexical Scoping
|-


! {{anchor|Seed7|[[Seed7]]}}
|extensible, [[object-oriented]], [[imperative programming|imperative]], structured, [[generic programming|generic]], [[reflective programming|reflective]]
|{{no}}
|strong
|safe
|explicit
|nominative
|static
|by value, by reference, by name
|{{yes|Yes (no garbage collection process that stops the world)}}
|General, Application, System
|Extensibility, Portability, Reliability, Efficiency, Elegance
|-


! {{anchor|Sidef|[[Sidef]]}}
|[[object-oriented]], [[imperative programming|imperative]], [[reflective programming|reflective]], [[dynamic programming|dynamic]], [[functional programming|functional]]
|{{no}}
|weak
|unsafe
|optional explicit
|duck
|dynamic
|by value (Call by object reference), by reference
|{{optional|Reference counting}}
|Application, Scripting, PL Research, Education
|Expressiveness, Elegance, Readability
|-


! {{anchor|SkookumScript|[[SkookumScript]]}}
|[[concurrent programming|concurrent]], [[object-oriented]], [[functional programming|functional]], [[imperative programming|imperative]], [[generic programming|generic]]
|{{no|No, [http://skookumscript.com/docs/v3.0/lang/syntax/ online syntax] includes EBNF language specification}}
|strong
|safe
|inferred, optional explicit annotations
|nominal
|static
|by reference
|{{optional|Reference counting}}
|Embedded gameplay, AI, automation, scripting
|Game concepts, Interactivity, Live workflow, Efficiency, Embedded
|-


! {{anchor|Slate|[[Slate]]}}
|[[object-oriented]], [[imperative programming|imperative]], [[functional programming|functional]], [[reflective programming|reflective]], prototype, multi-dispatch
|{{no}}
|strong
|safe
|implicit, optional
|structural (aka duck)
|dynamic
|by reference
|{{yes}}
|Application, Embedded, Scripting
|Uniformity, Pure object-oriented, expressiveness, readability
|-


! {{anchor|Smalltalk|[[Smalltalk]]}}
|[[object-oriented]], [[concurrent programming|concurrent]], [[event-driven programming|event-driven]], [[imperative programming|imperative]], [[declarative programming|declarative]]
|{{yes|Yes, [http://wiki.squeak.org/squeak/172 ANSI]}}
|strong
|safe
|implicit
|protocol (aka duck)
|dynamic
|by value (Call by object reference)
|{{yes}}
|Application, Education
|Uniformity, Pure object oriented
|-


! {{anchor|SPARK|[[SPARK]]}}
|[[concurrent programming|concurrent]], [[imperative programming|imperative]], [[object-oriented]]
|{{no|No, but [http://docs.adacore.com/spark2014-docs/html/lrm/ Language Reference Manual] available.}}
|strong
|safe
|explicit
|nominative
|static
|by value, by reference
|{{optional|Allowed}}
|High integrity applications
|Logically sound, simple formal definition, expressive power, security, applications verifiable and have bounded space and time.
|-


! {{anchor|Standard ML|[[Standard ML]]}}
|[[functional programming|functional]], [[imperative programming|imperative]], [[generic programming|generic]]
|{{yes|Yes, [http://www.smlnj.org/sml97.html SML '97]}}
|strong
|safe
|inferred, optional explicit annotations
|polymorphic structural
|static
|by value
|{{yes}}
|
|
|-


! {{anchor|Swift|[[Swift]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]]
|{{no}}
|strong
|
|inferred, optional explicit annotations
|nominative
|static
|by value
|{{optional|Reference counting}}
|Application 
|Compatibility with [[Objective-C]] runtime
|-


! {{anchor|Tcl|[[Tcl]]}}
|[[imperative programming|imperative]], [[procedural programming|procedural]], [[event-driven programming|event-driven]], [[object-oriented]] <small>(native from Tcl 8.6 or via [http://wiki.tcl.tk/970 extensions] before that)</small>
|{{no}}
|weak
|safe
|implicit
|duck
|dynamic
|by value (also by name and by reference through passing of variable names and <code>upvar</code> command)
|{{optional|Only of unnamed entities, e.g., values}}
|Embedded, Application, Scripting
|[http://www.tcl.tk/about/history.html Extend, Embed and Integrate]
|-


! {{anchor|Trith|[[Trith]]}}
|[[functional programming|functional]], [[stack]]-oriented, [[concatenative programming|concatenative]]
|{{no}}
|strong
|safe
|implicit
|duck
|dynamic
|by value
|{{yes}}
|Embedded, Application, Scripting, Education
|Simplicity, Expressiveness, Terseness, [http://linkeddata.org/ Linked Data]
|-


! {{anchor|Unicon|[[Unicon]]}}
| [[procedural programming|procedural]], [[structured]], [[Goal_directed_programming|goal directed]], [[string scanning]], [[co-expressions]], [[object-oriented]]
| {{no}}
| strong
| Safe
| implicit
| nominative
| dynamic
| by value and safe reference depending if the type is mutable or immutable
| {{yes}}
| Text analysis, text editing, document formatting, artificial intelligence, expert systems, rapid prototyping, symbolic mathematics, text generation, and data laundry. 
| Facilitates ease of programming, short concise programs, automatic storage allocation and management, provide a rich base of basic data types and structures, and platform independence . Unicon provides additional capabilities over [[#Icon|Icon]] integrating object oriented capabilities, messaging and external communications, event monitoring, and more in a consistent framework.
|-


! {{anchor|V|[[V]]}}
|[[functional programming|functional]], [[stack]]-oriented, [[concatenative programming|concatenative]]
|{{no}}
|strong
|safe
|
|
|dynamic
|
|
|[[functional programming|functional]] research
|[[concatenative programming|concatenative]]
|-


! {{anchor|Visual Basic|[[Visual Basic]]}}
|component-oriented programming, [[event-driven programming|event-driven]]
|{{no}}
|strong
|safe
|
|nominative
|static
|by reference, by value (explicit)
|{{yes}}
|Application
|Rapid application development, Simplicity
|-


! {{anchor|Visual Basic .NET|[[Visual Basic .NET]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[generic programming|generic]], [[reflective programming|reflective]], [[functional programming|functional]], [[event-driven programming|event-driven]]
|{{no}}
|strong
|safe
|implicit
|nominative
|static, dynamic (for interop)
|by value, by reference (through managed pointers [always in-out])
|{{yes}}
|Application
|Rapid application development
|-


! {{anchor|PowerShell|[[Windows PowerShell]]}}
|[[imperative programming|imperative]], [[object-oriented]], [[functional programming|functional]], pipeline, [[reflective programming|reflective]]
|{{no}}
|strong
|safe
|implicit
|
|dynamic
|
|
|Scripting
|
|-class="sortbottom"
![[Programming Language|Language]]
![[:Category:Programming Paradigms|Paradigm(s)]]
!Standardized
![[Type strength]]
![[Type safety]]
!Expression of types 
![[Type compatibility]]
![[Type checking]]
![[Parameter Passing]] Methods Available 
![[Garbage collection]]
!Intended use 
!Design goals 
|-

|}

==External resources==
* [[wp:Comparison_of_programming_languages|Wikipedia: Comparison of programming languages]]
