+++
title = "CafeOBJ"
description = ""
date = 2014-03-15T17:47:10Z
aliases = []
[extra]
id = 16514
[taxonomies]
categories = []
tags = []
+++
## Basic Information
CafeOBJ is a algebraic specification language.
It has an executable sub-language which is broadly similar to Haskell or ML.
CafeOBJ has many advanced features including: multiple logics, flexible mix-fix syntax, powerful and clear typing system with ordered sorts, parametric modules and views for instantiating the parameters, and module expressions, and more.
Many of these features are inherited from [http://en.wikipedia.org/wiki/OBJ3 OBJ3]

[http://www.ldl.jaist.ac.jp/cafeobj/ Download] ,
[http://www.jaist.ac.jp/~t-seino/lectures/cafeobj-intro/en/index.html Tutorial] ,
[http://www.jaist.ac.jp/~ogata/lecture/i217/ Lectures] ,
[http://logic.pdmi.ras.ru/csclub/en/node/1819 Video-1] ,
[http://sel.ifmo.ru/seminar/cafeobj.webm Video-2] ,





### Examples


```CafeOB

-- Text file called say Hello.cafe ,contains the following
mod! HELLO-WORLD {
pr(STRING)
op hello : -> String
eq hello = "Hello World" .
}
-- Bring *in* the file at CafeOBJ prompt
in hello.cafe
-- Open the HELLO-WORLD module
open HELLO-WORLD
-- Execute with the reduce command
reduce hello .
-- Gives ("Hello World"):String

```




```CafeOB

-- Here is a sorting program.
mod! SORTING-NAT {
pr(NAT)
[Nat < Strg ]
-- Simple list structure
op  nil : -> Strg
op _._ : Strg Strg -> Strg { assoc id: nil }

vars N N' : Nat
-- A very short sorting program using on transition equation in POA logic, which is a type of rewrite logic/
-- The program is in the form of a condition transition, which will swap N and N' if N is larger or equal to N'.
-- This is a equation in POA logic so there is no need for an intermediate variable to do the swap.
ctrans [swap] : (N . N') => (N' . N) if N' <= N .
}

**> Sorting natural numbers using exec command
open SORTING-NAT
exec (3 . 2 . 1) .
**> We can consider sorting as a path through the state space of the permutations of N numbers.
**> There are N! permutations only one of which is sorted.
**> Sorting natural numbers using search command
**> we can use (show path N) with this command, where N is the number of possible states.
red (3 . 2 . 1) =(1,1)=>* (1 . 2  . 3) .
red (3 . 2 . 1) =(1,2)=>* (1 . 2  . 3) .
red (3 . 2 . 1) =(1,3)=>* (1 . 2  . 3) .
**> search for any number of solutions at any depth
red (3 . 2 . 1) =(*,*)=>*  (1 . 2 . 3) .
**> print the transitions from initial to goal state
show path 5
eof

```

