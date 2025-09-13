+++
title = "Execute HQ9+/Haskell"
description = ""
date = 2018-07-13T14:36:08Z
aliases = []
[extra]
id = 5234
[taxonomies]
categories = []
tags = []
+++


This [HQ9+](https://rosettacode.org/wiki/HQ9+) interpreter is written in [Haskell](https://rosettacode.org/wiki/Haskell).
We use [https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Traversable.html#v:mapAccumR mapAccumR] to maintain the accumulator.
However, the specification doesn't say what to do with said accumulator on completion.

```haskell
module Main where

import Data.Char (toLower, toUpper)
import Data.List        -- for concat
import Data.Traversable -- for mapAccumR

main :: IO ()
main = do
  s <- getLine
  putStrLn (snd (hq9p s))

hq9p :: String -> (Int,String)
hq9p src = fin $ mapAccumR run 0 $ map toLower src
  where
    fin (acc,log) = (acc,concat log)
    run acc ch = case ch of
      'h' -> (acc,"Hello, world!")
      'q' -> (acc,src)
      '9' -> (acc,bottles)
      '+' -> (acc + 1,"")
      '_' -> (acc,"")

bottles :: String
bottles = [99, 98 .. 0] >>= beers
  where
    beers n = unlines [
      bob n ++ " on the wall, " ++ bob n ++ ".",
      pass n ++ bob (n - 1) ++ " on the wall.\n"]
    bob n =
      let nu = case n of { (-1) -> "99"; 0 -> "No more"; n -> show n; }
          s  = if n == 1 then "" else "s"
      in nu ++ " bottle" ++ s ++ " of beer"
    pass n = case n of
      0 -> "Go to the store and buy some more, "
      _ -> "Take one down and pass it around, "
```

