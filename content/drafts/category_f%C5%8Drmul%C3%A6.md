+++
title = "Category:Fōrmulæ"
description = ""
date = 2019-06-21T23:51:01Z
aliases = []
[extra]
id = 21980
[taxonomies]
categories = []
tags = []
+++

{{language|Fōrmulæ
|exec=interpreted
|gc=yes
|parampass=value
|untyped=yes
|site=http://formulae.org
}}

{{language programming paradigm|Declarative}}
{{language programming paradigm|Dynamic}}
{{language programming paradigm|Functional}}

=== What is Fōrmulæ ? ===

Fōrmulæ is a [https://en.wikipedia.org/wiki/Application_framework framework] to do [https://en.wikipedia.org/wiki/Computer_algebra symbolic computation].

Symbolic computation works by the repeatedly application of rules of transformation &mdash;also called [https://en.wikipedia.org/wiki/Rewriting rewriting rules] &mdash; on symbolic expressions, until no rule can be applied.

As a very simple example, consider the following sequence of rule application:

{| class="wikitable" style="text-align: center;"
! Symbolic expression || Step
|-
| 2 + (3 × 5) || Starting expression
|-
| 2 + 15 || [http://wiki.formulae.org/Math.Arithmetic.Multiplication#Addition_of_numeric_addends Numeric multiplication] rule
|-
| 17 || [http://wiki.formulae.org/Math.Arithmetic.Addition#Addition_of_numeric_addends Numeric addition] rule
|-
| 17 || Final result, no rule can be applied
|}

Mathematics is not the only field where symbolic computation can be used.

Traditional symbolic software, such as [https://en.wikipedia.org/wiki/Wolfram_Mathematica Mathematica], [https://en.wikipedia.org/wiki/Maple_(software) Maple] or [https://en.wikipedia.org/wiki/MATLAB Matlab] are provided with a considerable but fixed set of rewriting rules.

The Fōrmulæ project works in a different way. We do not only create rewriting rules, It provides an specification with which anybody can write &mdash;in a regular programming language&mdash; rewriting rules that can be also published. It converts the process of creating rewriting rules to a colaborative one.

=== Fōrmulæ wiki ===

As you can see, the number of rules are always increasing and they need to be documented. The [http://wiki.formulae.org Fōrmulæ wiki] is the official mean to do it.

The wiki is organized on ''expression tags''. An expression tag is a unique name given to a kind of symbolic expression, i.e. the mathematical addition operation receives the expression tag '''Math.Arithmetic.Addition'''. The [http://wiki.formulae.org/Math.Arithmetic.Addition Math.Arithmetic.Addition] wiki entry enumerates and describes every known rule applied to this expression tag.


###  Expression visualization 


[[File:FormulaeVisualExample.png|thumb|An example of [https://en.wikipedia.org/wiki/Prettyprint pretty-print] visualization of expressions]]

The Fōrmulæ framework is not only a specification to create rewriting rules. It also defines a specification of how to visualize symbolic expressions, and how to create methods for editing symbolic expressions.

It means that anybody can write code to define how an expression is to be shown. It is called a visualization rule. Moreover, there can be multiple visualization rules for a specific expression, although only one can be used at the same time.

It is very common that these visualization rules show expression as humans do. It is usually called [https://en.wikipedia.org/wiki/Prettyprint pretty-print]. Moreover, different visualization rules can also written to match science field, localization (see [http://wiki.formulae.org/Category:Expressions_with_localized_implementations Expressions with localized implementations]), or even personal preferences.

Entries in the wiki also describe the known visualization rules for a specific expression tag.


###  Expression edition 


The Fōrmulæ framework also defines a specification to edit (create or modify) symbolic expressions, called edition rules.

Entries in the wiki also describe the known edition rules for a specific expression tag.

=== Fōrmulæ packages ===

A package is a combination of zero or more rewriting rules, zero or more visualization rules and zero or more edition rules, packed as a single file, ready to be published. These rules are generally related to the same field.

There are several packages developed by the Fōrmulæ team, called the standard packages, but anyone can pack rules he/she creates.

The following are some of the standard packages:

{| class="wikitable"
! Standard package || Description
|-
| Arithmetic || Number definition, [https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic arbitrary precision] calculations, [https://en.wikipedia.org/wiki/Rational_number rational numbers], [https://en.wikipedia.org/wiki/Imaginary_unit imaginary unit], [https://en.wikipedia.org/wiki/Infinity infinity]
|-
| Relation || Comparisons, [https://en.wikipedia.org/wiki/Three-way_comparison three-way comparisons], min/max value in set/list, set/list [https://en.wikipedia.org/wiki/Element_(mathematics) membership] (∈)
|-
| Logic || Logic literals (true, false), logic operations, first order quantifiers, predicates
|-
| String || String definition, strings operations
|-
| Symbolic || Symbols, functions, global and local scope of symbols, lambda expressions
|-
| Expression || Sub-expression retrieval, expression cardinality, tag management, ask/show expressions
|-
| List || List definition, dynamic creation of lists, matrices, addition/multiplication of matrices, cartesian product, transposing, determinants
|-
| Color || Color definition, dynamic creation of color
|-
| Programming || Sequence control (blocks), decision control (if, if-else) and iteration control (while, for, foreach), recursion
|}

There are standard packages that are being developed, and they will be available soon:

{| class="wikitable"
! Standard package || Description
|-
| Diagram || Several kind of diagrams
|-
| Chart || Bar, line, pie charts
|-
| Plot || Mathematic plots, 2D/3D functions, parametric, polar, contour line, vector, sufaces
|-
| Time || Date datatype definition, timezones
|-
| Localization || Locale definition, localized operations such as number, string or date representation
|-
| Object oriented programming || Class definition, object instantiation, inheritance, polymorphism
|-
| Logic programming || Conversion to Horn clauses, backtracking
|-
| Theorem proving || High order and equational logic, conversion to conjunctive normal form, resolution tree
|-
| Quantum programming (simulated) || Qbit definition, quantum logic gates, reversibility, measurement
|}

=== Fōrmulæ front-ends ===

[[File:FormulaeDesktopExample.png|thumb|link=http://wiki.formulae.org/mediawiki/images/1/19/DesktopExample.png|A typical Fōrmulæ desktop front-end session. Click/tap to enlarge]]

There must be a program where symbolic expressions are created, visualized, edited and where to apply the rewriting rules. It is a front-end. Anyone can write such a program, according to the Fōrmulæ specification.

There is, however a standard program, called the Desktop. It has the following features:

* It can be used to install, reinstall, disable or remove Fōrmulæ packages.
* It can be used to choose the visualization rule for a tag, when it has multiple of them.
* It can be used to disable specific edition or rewriting rules.
* It works in [https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop read-eval-print-loop] mode. It is, the user creates an expression (a question), then the program invokes the process of rule application, returning the result (the answer), and the process continues again, creating a list of question-answers (a script), like a [https://en.wikipedia.org/wiki/Command-line_interface command-line] mode. Unlike a command-line interface, a user can modify and re-evaluate any of the previous questions.
* The script can be saved to a file, to be retrieved later.
* The script can be printed out.
* It supports cut/copy/paste operations between expressions and subexpressions.
* It supports undo/redo operations.

=== Fōrmulæ as a programming language ===

There are rewriting rules to do what can be done with traditional programming languages, so it can be used as a programming language too. It is called the ''Fōrmulæ programming language'' and it is [https://en.wikipedia.org/wiki/Turing_completeness Turing-complete], so it can compute everything that is computable.

Given that rewriting rules, visualization rules and edition rules are continuously created or improved, it produces radical consecuences never seen before in traditional programming languages:


### = A dynamically defined programming language =


The term "dynamically defined programming language" is defined here as a language which its features and capabilities are defined and added to the language '''over time'''. It is a language able to evolve and change in time.

Traditionally, programming languages are first fully designed. A grammar is created in order to construct tools, such as compilers. Once it is done, it is very hard or impossible to make changes on the language structure. Adding features to a language usually leads in a new and different language, i.e. the [https://en.wikipedia.org/wiki/C_(programming_language) C] and the [https://en.wikipedia.org/wiki/C%2B%2B C++] languages.

==== Non-unique visualization of programs ====

Because there can be multiple ways symbolic expressions look, there are several forms of visualization for the same program.

The next example shows the definition of a function that generates a [https://en.wikipedia.org/wiki/Farey_sequence Farey sequence] of a given order, using different visualization rules &mdash;for some of its subexpressions&mdash;.

{| class="wikitable" style="text-align: center;"
! Flowchart style || Traditional, indented code style
|-
| [[File:FormulaeExampleFlowchart.png]] || [[File:FormulaeExampleCode.png]]
|}


###  Official sites 


{| class="wikitable"
! Site || Description
|-
| [http://www.formulae.org www.formulae.org] || Main page. Official repository of the library, Desktop program, packages and source code.
|-
| [http://wiki.formulae.org wiki.formulae.org] || Reference of expressions and implementations
|}

[[Category:Mathematical programming languages]]
