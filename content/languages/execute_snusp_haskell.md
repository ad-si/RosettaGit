+++
title = "Execute SNUSP/Haskell"
description = ""
date = 2010-02-06T14:34:38Z
aliases = []
[extra]
id = 2855
[taxonomies]
categories = []
tags = []
+++


This [Haskell](https://rosettacode.org/wiki/Haskell) implementation supports commands from all the three SNUSP variants, as described on the [Esolang SNUSP page](https://rosettacode.org/wiki/eso:SNUSP).

Threads and 2D-data makes a purely functional implementation difficult, so most of the code works in the IO-Monad. There is an immutable array ''c'' for the code, a global mutable hashtable ''d'' for the data, and each thread has an instruction pointer ''ip'', a memory pointer ''mp'', and a call stack ''stack''.

Design decisions (not covered by SNUSP specification):

* Decrementing a zero memory cell sets it to zero.
* The data area is infinite.
* Threads block during read if no input is available, while other threads continue (as one of the examples requires).
* As the SNUSP variants differ in the number of dimensions in data and code, make it easy to add even more dimensions.

The interpreter has been tested with the ''echo'', ''thread'', ''multiplication'' and ''multi-digit print'' examples.

The Haskell code starts with lots of imports:


```haskell
import System.Environment
import System.IO
import System.Random

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe
import Data.Array

import qualified Data.HashTable as H
```


Use a list as an index into an array:


```haskell
type Index = [Int]

instance Ix a => Ix [a] where
  index ([],[]) []            = 0
  index (l:ls, u:us) (i:is)   = index (l,u) i + 
                                index (ls,us) is * rangeSize (l,u)
  range ([],[])               = [[]]
  range (l:ls, u:us)          = [i:is | is <- range (ls,us), i <- range (l,u)]
  inRange ([],[])      []     = True
  inRange (l:ls, u:us) (i:is) = inRange (l,u) i && inRange (ls,us) is
  rangeSize (ls,us)           = product $ map rangeSize $ zip ls us
```


or into an hashtable (the hash function could probably be improved):


```haskell
cmpList :: Index -> Index -> Bool
cmpList []     []     = True
cmpList (x:xs) []     = x == 0 && cmpList xs []
cmpList []     (y:ys) = y == 0 && cmpList [] ys
cmpList (x:xs) (y:ys) = x == y && cmpList xs ys

hashList xs = H.hashInt $ foldr combine 0 xs

combine :: Int -> Int -> Int
combine x 0 = x
combine x y = z * (z+1) `div` 2 + x where z = x + y
```


Here it's important that index lists with trailing zeroes are treated just like this list without the zeroes, so we can handle any number of dimensions. We want the same flexibility when adding index lists:


```haskell
(<+>) :: Index -> Index -> Index
[]     <+> ys     = ys
xs     <+> []     = xs
(x:xs) <+> (y:ys) = (x+y) : (xs <+> ys)
```


Some helper functions:


```haskell
data Thread a = T {mp::a, ip::a, dir::a, stack::[(a,a)]} deriving Show

modify d t f = do
  let i = mp t
  x <- H.lookup d i
  let x' = fromMaybe 0 x
  H.delete d i
  H.insert d i (f x') -- H.update 
  return [t]

moveMp d t delta = return [t {mp=(mp t) <+> delta}]

readMp d t = H.lookup d (mp t) >>= return . fromMaybe 0

step t = t {ip=(ip t) <+> (dir t)}

dec :: Integer -> Integer
dec 0 = 0
dec x = x-1

toChar   = chr . fromInteger
fromChar = toInteger . ord
```


Now, the commands. Given a thread, return a list of threads valid after one simulation step. In that way, ''exec'' can handle forks and thread termination on errors.


```haskell
-- Core SNUSP

exec '+'  d t = modify d t (+1)  
exec '-'  d t = modify d t (dec) 
exec '<'  d t = moveMp d t [-1] 
exec '>'  d t = moveMp d t [ 1]
exec ','  d t = getChar >>= modify d t . const . fromChar
exec '.'  d t = readMp d t >>= putChar . toChar >> return [t]
exec '\\' d t = return [t {dir=( d2:  d1:ds)}] where d1:d2:ds = dir t <+> [0,0]
exec '/'  d t = return [t {dir=(-d2: -d1:ds)}] where d1:d2:ds = dir t <+> [0,0]
exec '!'  d t = return [step t]
exec '?'  d t = readMp d t >>= \x -> return [if x == 0 then step t else t]

-- Modular SNUSP

exec '@'  d t = return [t {stack=(ip t, dir t):(stack t)}]
exec '#'  d   T{stack=[]}         = return []
exec '#'  d t@T{stack=(ip,dir):s} = return [step $ t {ip=ip, dir=dir, stack=s}]
   
-- Bloated SNUSP

exec ':'  d t = moveMp d t [0,-1]
exec ';'  d t = moveMp d t [0, 1]
exec '&'  d t = return [step t, t {stack=[]}]
exec '%'  d t = readMp d t >>= \x -> randomRIO (0,x) >>= modify d t . const

-- NOOP

exec _    d t = return [t]
```


The scheduler manages a list ''ts'' of active threads, and a list ''ks'' of threads waiting for input. If there are no more threads in either list, stop. If input is available, one blocked thread is executed. If no input is available and all threads are blocked, we block the interpreter, too (so the OS can do something else). Otherwise, try to execute one of the unblocked threads, first checking if it's still inside the code array.


```haskell
start c = maybe (fst $ bounds $ c) fst $ find (\(_,x) -> x == '$') $ assocs c 

run c d = schedule [thread] [] False where 
  thread = T {mp=[1,1], ip=start c, dir=[1], stack=[]}
  exec' x d t = exec x d t >>= \ts -> return (ts,[])
  schedule' ts ks (ts',ks') = hReady stdin >>= schedule (ts++ts') (ks++ks')
  schedule [] []     _     = return ()
  schedule [] ks     False = hLookAhead stdin >> schedule' [] ks ([],[])
  schedule ts (k:ks) True  = exec' ',' d k  >>= schedule' ts ks
  schedule (t:ts) ks _     = check (step t) >>= schedule' ts ks 
  check t 
    | not $ bounds c `inRange` (ip t) = return ([],[])
    | x == ','                        = return ([],[t])
    | otherwise                       = exec' x d t 
    where x = c ! (ip t)
```


Finally, routines to run code from a string or a file, and the main program.


```haskell
runString y s = do
  d <- H.new cmpList hashList
  let x = length s `div` y
  run (listArray ([1,1],[x,y]) s) d

runFile name = do
  s <- readFile name
  d <- H.new cmpList hashList
  let l = lines s
  let y = length l
  let x = maximum $ map length $ l
  let m = [([i,j],c) | (j,v) <- zip [1..] l, (i,c) <- zip [1..] v]
  let c = listArray ([1,1],[x,y]) (repeat ' ') // m
  run c d

main = do
  hSetBuffering stdin NoBuffering
  [s] <- getArgs
  runFile s
```


## Extension
To demonstrate the ease of introducing even more dimensions, let's implement commands ( and ) to move the data pointer along the z-axis, and a command ^ to rotate the IP direction around the (1,1,1) axis (i.e., left becomes up, up becomes "farther" on the z-axis, "farther" becomes left, etc.).


```haskell
exec '(' d t = moveMp d t [0,0,-1]
exec ')' d t = moveMp d t [0,0, 1]
exec '^' d t = return [t {dir=(d3:d1:d2:ds)}] where d1:d2:d3:ds = dir t <+> [0,0,0]
```

