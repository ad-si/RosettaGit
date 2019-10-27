+++
title = "Talk:Weird numbers"
description = ""
date = 2019-03-24T19:47:49Z
aliases = []
[extra]
id = 22239
[taxonomies]
categories = []
tags = []
+++


### A faster and less ambitious algorithm ?


I noticed yesterday that entries here were sparse.

Perhaps some were abandoned after first-sketch exhaustive searches appeared interminably slow ?
And possibly the references to number theory in the Perl 6 founding example could look a bit daunting to some ?

Here is a theoretically unambitious approach, which seems, for example, to compress the (functionally composed) Python version down to c. 300 ms for 50 weirds (about half that for 25 weirds), on a system which needs c. 24 seconds to find 25 weirds with the initial Perl 6 draft.

# Choose a '''smaller target''' for the sum-search. 
# Use a recursive '''hasSum'''(''target'', ''divisorList'') predicate which fails early.
# Generate the properDivisors in '''descending''' order of magnitude.


A smaller target should, I think, involve a smaller number of possible sums. The obvious candidate in an abundant number is the '''difference''' between the sum of the proper divisors and the number considered.  If a sum to that difference exists, then removing the participants in the smaller sum will leave a set which sums to the abundant number itself.

For possible early-failing implementations of hasSum, see the Python, Haskell, JavaScript and even AppleScript drafts.

If hasSum considers large divisors first, it can soon exclude all those those too big to sum to a smaller target. 
[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:52, 24 March 2019 (UTC)

: PS I think we may be able to see and test the operation of '''hasSum''' more clearly if we enrich its type from Bool to [Int] (with a return value of the empty list for '''False''', and the integers of the first sum found for '''True'''). Let's call this richer-typed variant '''anySum''', and sketch it in Python 3.

::
```python
# anySum :: Int -> [Int] -> [Int]
def anySum(n, xs):
    '''First subset of xs found to sum to n.
       (Probably more efficient where xs is sorted in
       descending order of magnitude)'''
    def go(n, xs):
        if xs:
            # Assumes Python 3 for list deconstruction
            # Otherwise: h, t = xs[0], xs[1:]
            h, *t = xs
            if n < h:
                return go(n, t)
            else:
                if n == h:
                    return [h]
                else:
                    ys = go(n - h, t)
                    return [h] + ys if ys else go(n, t)
        else:
            return []
    return go(n, xs)


# Search for sum through descending numbers (more efficient)
print(anySum(196, range(100, 0, -1)))
# -> [100, 96]

# Search for sum through ascending numbers (less efficient)
print(anySum(196, range(1, 101)))
# -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 25]

print(anySum(7, [6, 3]))
# -> []
```


or similarly, rewriting '''hasSum''' to '''anySum''' (with comments) in a Haskell idiom:

:
```haskell
module AnySum where

hasSum :: Int -> [Int] -> Bool
hasSum _ [] = False
hasSum n (x:xs)
  | n < x = hasSum n xs
  | otherwise = (n == x) || hasSum (n - x) xs || hasSum n xs


-- Or, enriching the return type from Bool to [Int]
-- with [] for False and a populated list (first sum found) for True

anySum :: Int -> [Int] -> [Int]
anySum _ [] = []
anySum n (x:xs)
  | n < x = anySum n xs  -- x too large for a sum to n
  | n == x = [x] -- We have a sum
  | otherwise =
    let ys = anySum (n - x) xs -- Any sum for (n - x) in the tail ?
    in if null ys
         then anySum n xs -- Any sum (not involving x) in the tail ?
         else x : ys  -- x and the rest of the sum that was found.
         

main :: IO ()
main = do
  -- xs ascending - the test is less efficient
  print $ anySum 196 [1 .. 100]
  -- -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,25]
  
  -- xs descending - the test is more efficent
  print $ anySum 196 [100,99 ..]
  -- -> [100,96]
```

