+++
title = "Execute Brainfuck/Haskell"
description = ""
date = 2010-02-06T14:21:09Z
aliases = []
[extra]
id = 2330
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
Quick implementation of a [[Brainfuck]] interpreter in [[Haskell]].

Pairs of lists are used to implement both the two-side infinite band of cells, and the program storage. This means that it can also work on infinite Brainfuck programs (which could be generated lazily).

In functional style, ''run'' interprets a Brainfuck program as a function from an Integer list (inputs) to an Integer list (outputs). This can be easily turned into a real dialog with a user via ''stdin'' and ''stdout''.

A more efficient implementation could for example only admit well-bracketed brainfuck programs, and parse bracket blocks first, to replace the ''matchLeft'' and ''matchRight'' which need linear time.


```haskell
module BF where

moveLeft  (x:l,r) = (l,x:r)
moveRight (l,x:r) = (x:l,r)

matchLeft d@('[':_,_) = d
matchLeft d@(']':_,_) = matchLeft $ moveLeft $ matchLeft $ moveLeft $ d
matchLeft d           = matchLeft $ moveLeft $ d

matchRight d@(_,']':_) = moveRight $ d
matchRight d@(_,'[':_) = matchRight $ matchRight $ moveRight $ d
matchRight d           = matchRight $ moveRight  $ d

pad ([],[]) = ([0],[0])
pad ([],r)  = ([0],r)
pad (l,[])  = (l,[0])
pad d       = d

modify f (l,x:r) = (l,(f x):r)

exec :: (String, String) -> ([Integer], [Integer]) -> [Integer] -> [Integer]
exec   (_,[])    _         _  = []
exec p@(_,'>':_) d         cs = exec (moveRight p) (pad $ moveRight $ d) cs
exec p@(_,'<':_) d         cs = exec (moveRight p) (pad $ moveLeft  $ d) cs
exec p@(_,'+':_) d         cs = exec (moveRight p) (modify (+ 1) d) cs
exec p@(_,'-':_) d         cs = exec (moveRight p) (modify (subtract 1) d) cs
exec p@(_,',':_) d     (c:cs) = exec (moveRight p) (modify (const c) d) cs
exec p@(_,'.':_) d@(_,x:_) cs = x : exec (moveRight p) d cs
exec p@(_,'[':_) d@(_,0:_) cs = exec (matchRight $ moveRight $ p) d cs
exec p@(_,'[':_) d         cs = exec (moveRight p) d cs
exec p@(_,']':_) d@(_,0:_) cs = exec (moveRight p) d cs
exec p@(_,']':_) d         cs = exec (matchLeft $ moveLeft $ p) d cs

run :: String -> [Integer] -> [Integer]
run s = exec ([],s) ([0],[0])

dialog :: String -> IO ()
dialog s = mapM_ print . run s . map read . lines =<< getContents

```
(This version compiles with GHC and will run if loaded into ghci using ':load BF')

Example session:


```txt

: *Main> dialog ",[>+<-].>."
: ''5''
: 0
: 5

```

