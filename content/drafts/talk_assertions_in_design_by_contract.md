+++
title = "Talk:Assertions in design by contract"
description = ""
date = 2014-10-01T12:33:49Z
aliases = []
[extra]
id = 17998
[taxonomies]
categories = []
tags = []
+++

Asserts in C are supposed to check at runtime during development, as an aide to the programmer, but not be compiled into a release version. This behavior is often imitated by putting a test in while code is being developed and then commenting it for release. So assert may be seen as a sort of comment in this case.

You also mention assert as a part of DbyC. According to wiki the following languages support DbyC as core:
 
##Ada 2012
##Ciao
##Clojure
##Perl6
##Cobra
##D[8]
##Eiffel
##Fortress
##Lisaac
##Mercury
##Nice
##Oxygene (formerly Chrome)
##Racket (including higher order contracts, and emphasizing that contract violations must blame the guilty party and must do so with an accurate explanation[9])
##RPS-Obix
##Sather
##SPARK (via static analysis of Ada programs)
##Spec#
##Vala
##VDM

and the following using external packages:

##Ada, via GNAT pragmas for preconditions and postconditions.
##C and C++, via the DBC for C preprocessor, GNU Nana, eCv static analysis tool, or the Digital Mars C++ compiler, via CTESK extension of C. Loki Library provides a mechanism named ContractChecker that verifies a class follows design by contract.
##C# (and other .NET languages), via Code Contracts (a Microsoft Research project integrated into the .NET Framework 4.0)
##DELPHI PRISM, see [1]
##Groovy via GContracts GContracts
##Java, via Contracts for Java, iContract2/JContracts, Contract4J, jContractor, C4J, Google CodePro Analytix, Jass preprocessor, OVal with AspectJ, Java Modeling Language (JML), Jtest, SpringContracts for the Spring framework, Modern Jass, JavaDbC using AspectJ, JavaTESK using extension of Java, chex4j using javassist, and the highly customizable java-on-contracts.
##JavaScript, via Cerny.js, ecmaDebug, jsContract, or jscategory.
##Common Lisp, via the macro facility or the CLOS metaobject protocol.
##Nemerle, via macros.
##Perl, via the CPAN modules Class::Contract (by Damian Conway) or Carp::Datum (by Raphael Manfredi).
##PHP, via PhpDeal, Praspel or Stuart Herbert's ContractLib.
##Python, using packages like zope.interface, PyDBC or Contracts for Python.
##Ruby, via Brian McCallister's DesignByContract, Ruby DBC ruby-contract or contracts.ruby.
##Tcl, via the XOTcl object-oriented extension

I have added an example of Ruby using [[https://github.com/egonSchiele/contracts.ruby contracts.ruby]]. Can we clarify which approach is required here. A solution which just puts an if statement in a procedure named assert does not achieve either.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:20, 1 October 2014 (UTC)

:I hope we are going for DbyC. I shall look forward to the Racket solution "emphasizing that contract violations must blame the guilty party and must do so with an accurate explanation". Programmers will be taken out and shot!!!!--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:29, 1 October 2014 (UTC)
