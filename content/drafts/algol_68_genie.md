+++
title = "ALGOL 68 Genie"
description = ""
date = 2013-04-03T02:08:36Z
aliases = []
[extra]
id = 2369
[taxonomies]
categories = []
tags = []
+++

{{implementation|ALGOL 68}}'''ALGOL 68G''' or '''Algol 68 Genie''' is an [[ALGOL 68]] Interpreter. ALGOL 68G is a nearly full implementation of ALGOL 68 as defined by the Revised Report and also implements partial parametrisation, which is an extension of ALGOL 68.

After successful parsing of an entire source program, the syntax tree, that serves as an intermediate program representation, is interpreted. The interpreter performs many runtime checks.

The author of Algol 68 Genie is Marcel van der Veer. Algol68G is released under the GNU GPL and runs on [[Linux]], [[UNIX]], [[BSD]] [[Mac OS X|Mac OS]] and [[Windows]] XP, and is available [http://www.xs4all.nl/~jmvdveer/algol.html here].

[[ALGOL 68G]] is an interpretor that runs on Linux and a good way to get started with [[ALGOL 68]].  ''ALGOL 68G'' mk14.1 is now available for download for Fedora9 from Source Forge - [http://sourceforge.net/project/showfiles.php?group_id=114223&package_id=300114&release_id=642308 Download now]. (This download RPM includes a 400 page [[ALGOL 68]] manual in printer ready pdf)
==Features of Algol68G==
* The [[interpreter]] performs checks on many events, for example: assigning to <tt>NIL</tt> or dereferencing of <tt>NIL</tt>, using uninitialised values, invalid operands to standard prelude operators and procedures, bounds check when manipulating arrays, overflow of arithmetic modes, "dangling references", that are names that refer to deallocated storage.
* Precision of numeric modes: implementation of <tt>LONG INT, LONG REAL</tt> and <tt>LONG COMPLEX</tt> with roughly doubled precision with respect to <tt>INT, REAL, COMPLEX</tt> and implementation of multiprecision arithmetic through <tt>LONG LONG INT, LONG LONG REAL</tt> and <tt>LONG LONG COMPLEX</tt> which are modes with user defined precision which is set by an option.
* On systems that support them, UNIX extensions that allow e.g. for executing child processes that communicate through pipes, matching regular expressions or fetching web page contents.
* Procedures for drawing using the GNU Plotting Utilities.
* Various numerical procedures and basic linear algebra procedures from the GNU Scientific Library.
* Format texts, straightening and formatted transput. Transput routines work generically on files, (dynamic) strings and UNIX pipes.
* Parallel clause on platforms that support [[POSIX]] threads.
* Various [[PostgreSQL]] client routines.
* Upper stropping is the default, quote stropping is optional.

== Extensions to Algol 68 ==
* Implementation of C.H. Lindsey's partial parametrisation proposal, which allows for [[currying]] in Algol 68, giving it a functional sublanguage.
* A simple refinement preprocessor to facilitate top-down program construction.
* Implementation of pseudo-operators <tt>ANDF</tt> and <tt>ORF</tt>.
* Implementation of a post-checked loop. A do-part may enclose a serial clause followed by an optional until-part, or just enclose an until-part. This is an alternative to the paradigm Algol 68 post-check loop <tt>WHILE ... DO SKIP OD</tt>.
* Implementation of <tt>DOWNTO</tt> with comparable function as <tt>TO</tt> in loop clauses; <tt>DOWNTO</tt> decreases, whereas <tt>TO</tt> increases, the loop counter by the value of the (implicit) by-part.

== Deviations from the Revised Report language ==
The important deviations are:
* The important difference with the Revised Report transput model is that Algol68G transput does not operate on <tt>FLEX [ ] FLEX [ ] FLEX [ ] CHAR</tt>, but on a <tt>FLEX [ ] CHAR</tt>. This maps better onto operating systems such as UNIX or Linux.
* The Algol68G parallel clause deviates from the Algol 68 parallel clause when parallel clauses are nested. In Algol68G, stack frames inside a parallel unit are private, therefore if parallel units modify a shared variable then this variable must be declared outside the outermost parallel clause, and a jump out of a parallel unit can only be targeted at a label outside the outermost parallel clause.
* The interpreter does not implement so-called ghost-elements {RR 2.1.3.4}, hence it cannot check bounds when assigning to rows of mode flexible-rows-of-rows-of-... when the destination has a flat descriptor.
* Algol68G does not recognise nonlocal environs {RR 5.2.3.2}. All environs are local.
* Transputting a file is essentially sequential. Only <tt>reset</tt> can intervene with sequential processing.
* When all arguments in a call of <tt>readf, printf, writef, getf</tt> or <tt>putf</tt> are processed, the format associated with the corresponding file is purged - that is, remaining insertions are processed and the format is discarded.

==Download==
* [http://www.xs4all.nl/~jmvdveer/algol.html Algol 68 Genie web page]
* [http://sourceforge.net/projects/algol68/ Fedora/Centos/RHEL/Win32: Sourceforge Algol 68 Compiler, Interpreter & Runtime]
