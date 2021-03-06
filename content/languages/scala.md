+++
title = "Scala"
description = ""
date = 2019-08-11T21:28:11Z
aliases = []
[extra]
id = 1925
[taxonomies]
categories = []
tags = []
+++

{{language|
|strength=strong
|express=partially implicit
|checking=static
|gc=yes
|site=http://scala-lang.org
|tags=Scala
|LCT=yes
|bnf=http://www.scala-lang.org/docu/files/ScalaReference.pdf}}
{{language programming paradigm|functional}}
{{language programming paradigm|object-oriented}}
{{language programming paradigm|generic}}Scala is a hybrid [[functional programming|Functional]]/[[object-oriented|OO]] language developed by Martin Odersky and his team at [http://lamp.epfl.ch/ LAMP]. Scala compiles to [[runs on vm::Java Virtual Machine|JVM]] [[bytecode]], and can inter-operate with [[Java]] code.

The language is most focused on the results of the program, therefor the [http://en.wikipedia.org/wiki/Considered_harmful considered harmful] technical computer oriented concepts like threads, semaphore, pointers, coercion, casting, goto- and break statements to struggle with are not there. For parallel processing (that very simple can archived) these concept are present but not visible to the programmer.

Lots of RC tasks addresses technical "features" of goto's, break, continue, side effects, pointers, type-casting, weak-typing and so on and are deliberately not Scala's cup of tea.

Compiled strongly-typed languages, where more defects can be caught at compile time, are less prone to runtime failures than interpreted or weakly-typed languages.
==Links==
* [[wp:Scala_(programming_language)|Scala in Wikipedia]]
* [https://docs.scala-lang.org/tour/tour-of-scala.html Tour of Scala and Tutorial]
* [https://www.scala-lang.org/files/archive/spec/2.13/ Scala language specification]
* [https://www.scala-lang.org/api/current/ Scala standard library documentation]

==Todo==
[[Reports:Tasks_not_implemented_in_Scala]]
