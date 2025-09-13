+++
title = "Knuth's power tree"
description = ""
date = 2019-10-06T20:54:41Z
aliases = []
[extra]
id = 19210
[taxonomies]
categories = ["task", "Knuth's power tree"]
tags = []
+++

## Task

(Knuth's power tree is used for computing   <big><big>x<sup>n</sup></big></big>   efficiently using Knuth's power tree.) 



;task requirements:

Compute and show the list of Knuth's power tree integers necessary for the computation of:

::*   <big><big>x<sup>n</sup></big></big>   for any real   <big><big>x</big></big>   and any non-negative integer   <big>n</big>.



Then, using those integers, calculate and show the exact (not approximate) value of (at least) the integer powers below:

::*   <big>2<sup>n</sup></big>     where   n   ranges from   0 ──► 17   (inclusive) 

::*   <big>3<sup>191</sup></big>
::*   <big>1.1<sup>81</sup></big>


A zero power is often handled separately as a special case.

Optionally, support negative integers   (for the power).


;example:

An example of a small power tree for some low integers:

```txt

                                                              1
                                                               \
                                                                2
                    ___________________________________________/ \
                   /                                              \
                  3                                                4
                 / \____________________________________            \
                /                                       \            \
               5                                         6            8
              / \____________                           / \            \
             /               \                         /   \            \
            7                 10                      9     12           16
           /                 //\\                     │      │           /\
          /            _____//  \\________            │      │          /  \
        14            /     /    \        \           │      │         /    \
       /│ \         11    13      15       20        18     24        17    32
      / │  \         │    /\      /\        │        /\      │        /\     │
     /  │   \        │   /  \    /  \       │       /  \     │       /  \    │
   19  21    28     22 23   26  25   30    40     27   36    48     33 34   64
   │   /\    /│\     │  │   /\   │   /\    /│\     │   /\    /│\     │  │   /\
   │  /  \  / │ \    │  │  /  \  │  /  \  / │ \    │  /  \  / │ \    │  │  /  \
  38 35 42 29 31 56 44 46 39 52 50 45 60 41 43 80 54 37 72 49 51 96 66 68 65 128

```

Where, for the power   <big>43</big>,   following the tree "downwards" from   <big>1</big>:
::*   (for   2)   compute square of   <big>X</big>,   store <big>X<sup>2</sup></big>
::*   (for   3)   compute   <big>X</big> * <big>X<sup>2</sup></big>,   store <big>X<sup>3</sup></big>
::*   (for   5)   compute   <big>X<sup>3</sup></big> * <big>X<sup>2</sup></big>,   store <big>X<sup>5</sup></big>
::*   (for 10)   compute square of   <big>X<sup>5</sup></big>,   store <big>X<sup>10</sup></big>
::*   (for 20)   compute square of   <big>X<sup>10</sup></big>,   store <big>X<sup>20</sup></big>
::*   (for 40)   compute square of   <big>X<sup>20</sup></big>,   store <big>X<sup>40</sup></big>
::*   (for 43)   compute   <big>X<sup>40</sup></big> * <big>X<sup>3</sup></big>   (result).

Note that for every even integer (in the power tree),   one just squares the previous value.

For an odd integer, multiply the previous value with an appropriate odd power of   <big>X</big>   (which was previously calculated).
  For the last multiplication in the above example, it would be   <big>(43-40)</big>,   or   <big>3</big>. 


According to Dr. Knuth (see below),   computer tests have shown that this power tree gives optimum results for all of the   ''n''
  listed above in the graph. 

For   ''n''   ≤ 100,000,   the power tree method:
::*   bests the factor method   88,803   times,
::*   ties   11,191   times,
::*   loses   6   times.



## References

::*   Donald E. Knuth's book:   ''The Art of Computer Programming, Vol. 2'', Second Edition, Seminumerical Algorithms, section 4.6.3: Evaluation of Powers.
::*   link   [http://codegolf.stackexchange.com/questions/3177/knuths-power-tree codegolf.stackexchange.com/questions/3177/knuths-power-tree]     It shows a   '''Haskel''',   '''Python''',   and a   '''Ruby'''   computer program example   (but they are mostly   ''code golf'').
::*   link   [https://comeoncodeon.wordpress.com/tag/knuth/ comeoncodeon.wordpress.com/tag/knuth/]     (See the section on Knuth's Power Tree.)     It shows a   '''C++'''   computer program example.
::*   link to Rosetta Code   [http://rosettacode.org/wiki/Addition-chain_exponentiation addition-chain exponentiation].





## EchoLisp


### Power tree

We build the tree using '''tree.lib''', adding leaves until the target n is found.

```scheme

(lib 'tree)

;; displays a chain hit
(define (power-hit target chain)
	(vector-push chain target)
	(printf "L(%d) = %d - chain:%a "
		target (1- (vector-length chain)) chain)
	(vector-pop chain))
	
;; build the power-tree : add 1 level of leaf nodes
;; display all chains which lead to target

(define (add-level node chain  target  nums (new))
(vector-push chain (node-datum node))
	(cond 
	[(node-leaf? node)
	;; add leaves by summing this node to all nodes in chain
	;; do not add leaf if number already known
			 (for [(prev chain)] 
			 		(set! new (+ prev (node-datum node)))
			 		(when (= new target) (power-hit target chain ))
			 		#:continue (vector-search* new nums)
			 		(node-add-leaf node new)
			 		(vector-insert* nums new)
			 		)]
	[else ;; not leaf node -> recurse
	(for [(son  (node-sons node))]
		(add-level son chain target nums )) ])
	(vector-pop chain))
	
;; add levels in tree until target found
;; return (number of nodes . upper-bound for L(target))
(define (power-tree target)
	(define nums (make-vector 1 1)) ;; known nums =  1
	(define T (make-tree 1)) ;; root node has value 1
	(printf "Looking for %d in %a." target T)
	(while #t
	#:break (vector-search* target nums) =>  (tree-count T)
	(add-level T init-chain: (make-vector 0) target  nums)
	))

```

```txt

(for ((n (in-range 2 18))) (power-tree n))
L(2) = 1 - chain:#( 1 2)
L(3) = 2 - chain:#( 1 2 3)
[ ... ]

(power-tree 17)
Looking for 17 in (🌴 1).
L(17) = 5 - chain:#( 1 2 4 8 16 17)

(power-tree 81)
Looking for 81 in (🌴 1).
L(81) = 8 - chain:#( 1 2 3 5 10 20 40 41 81)
L(81) = 8 - chain:#( 1 2 3 5 10 20 40 80 81)
L(81) = 8 - chain:#( 1 2 3 6 9 18 27 54 81)
L(81) = 8 - chain:#( 1 2 3 6 9 18 36 72 81)
L(81) = 8 - chain:#( 1 2 4 8 16 32 64 65 81)

(power-tree 191)
Looking for 191 in (🌴 1).
L(191) = 11 - chain:#( 1 2 3 5 7 14 19 38 57 95 190 191)
L(191) = 11 - chain:#( 1 2 3 5 7 14 21 42 47 94 188 191)
L(191) = 11 - chain:#( 1 2 3 5 7 14 21 42 63 126 189 191)
L(191) = 11 - chain:#( 1 2 3 5 7 14 28 31 59 118 177 191)
L(191) = 11 - chain:#( 1 2 3 5 7 14 28 31 62 93 186 191)
L(191) = 11 - chain:#( 1 2 3 5 10 11 22 44 88 176 181 191)

(power-tree 12509) ;; not optimal
Looking for 12509 in (🌴 1).
L(12509) = 18 - chain:#( 1 2 3 5 10 13 26 39 78 156 312 624 1248 2496 2509 3757 6253 12506 12509)
L(12509) = 18 - chain:#( 1 2 3 5 10 15 25 50 75 125 250 500 1000 2000 2003 4003 8006 12009 12509) 

(power-tree 222222)
Looking for 222222 in (🌴 1).
L(222222) = 22 - chain:#( 1 2 3 5 7 14 21 35 70 105 210 420 840 1680 1687 3367 6734 13468 26936 53872 57239 111111 222222) 


```


### Exponentiation


```scheme

;; j such  as chain[i] = chain[i-1] + chain[j]
(define (adder chain  i)
	(for ((j i)) #:break (= [chain i] (+ [chain(1-  i)] [chain j])) => j ))
		
		
(define (power-exp x chain)
	(define lg (vector-length chain))
	(define pow (make-vector lg x))
	(for ((i  (in-range 1 lg)))
	(vector-set! pow  i ( * [pow [1- i]] [pow (adder chain i)])))
	[pow (1- lg)])

```

```txt

(power-exp 2 #( 1 2 4 8 16 17) )
   → 131072
(power-exp 1.1 #( 1 2 3 5 10 20 40 41 81) )
    → 2253.2402360440283

(lib 'bigint)
bigint.lib v1.4 ® EchoLisp
Lib: bigint.lib loaded.
(power-exp 3 #( 1 2 3 5 7 14 19 38 57 95 190 191) )
    → 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## Go

```go
package main

import (
    "fmt"
    "math/big"
)

var (
    p   = map[int]int{1: 0}
    lvl = [][]int{[]int{1}}
)

func path(n int) []int {
    if n == 0 {
        return []int{}
    }
    for {
        if _, ok := p[n]; ok {
            break
        }
        var q []int
        for _, x := range lvl[0] {
            for _, y := range path(x) {
                z := x + y
                if _, ok := p[z]; ok {
                    break
                }
                p[z] = x
                q = append(q, z)
            }
        }
        lvl[0] = q
    }
    r := path(p[n])
    r = append(r, n)
    return r
}

func treePow(x float64, n int) *big.Float {
    r := map[int]*big.Float{0: big.NewFloat(1), 1: big.NewFloat(x)}
    p := 0
    for _, i := range path(n) {
        temp := new(big.Float).SetPrec(320)
        temp.Mul(r[i-p], r[p])
        r[i] = temp
        p = i
    }
    return r[n]
}

func showPow(x float64, n int, isIntegral bool) {
    fmt.Printf("%d: %v\n", n, path(n))
    f := "%f"
    if isIntegral {
        f = "%.0f"
    }
    fmt.Printf(f, x)
    fmt.Printf(" ^ %d = ", n)
    fmt.Printf(f+"\n\n", treePow(x, n))
}

func main() {
    for n := 0; n <= 17; n++ {
        showPow(2, n, true)
    }
    showPow(1.1, 81, false)
    showPow(3, 191, true)
}
```


```txt

0: []
2 ^ 0 = 1

1: [1]
2 ^ 1 = 2

2: [1 2]
2 ^ 2 = 4

3: [1 2 3]
2 ^ 3 = 8

4: [1 2 4]
2 ^ 4 = 16

5: [1 2 4 5]
2 ^ 5 = 32

6: [1 2 4 6]
2 ^ 6 = 64

7: [1 2 4 6 7]
2 ^ 7 = 128

8: [1 2 4 8]
2 ^ 8 = 256

9: [1 2 4 8 9]
2 ^ 9 = 512

10: [1 2 4 8 10]
2 ^ 10 = 1024

11: [1 2 4 8 10 11]
2 ^ 11 = 2048

12: [1 2 4 8 12]
2 ^ 12 = 4096

13: [1 2 4 8 12 13]
2 ^ 13 = 8192

14: [1 2 4 8 12 14]
2 ^ 14 = 16384

15: [1 2 4 8 12 14 15]
2 ^ 15 = 32768

16: [1 2 4 8 16]
2 ^ 16 = 65536

17: [1 2 4 8 16 17]
2 ^ 17 = 131072

81: [1 2 4 8 16 32 64 80 81]
1.100000 ^ 81 = 2253.240236

191: [1 2 4 8 16 32 64 128 160 176 184 188 190 191]
3 ^ 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## Groovy

```groovy
class PowerTree {
    private static Map<Integer, Integer> p = new HashMap<>()
    private static List<List<Integer>> lvl = new ArrayList<>()

    static {
        p[1] = 0

        List<Integer> temp = new ArrayList<Integer>()
        temp.add 1
        lvl.add temp
    }

    private static List<Integer> path(int n) {
        if (n == 0) return new ArrayList<Integer>()
        while (!p.containsKey(n)) {
            List<Integer> q = new ArrayList<>()
            for (Integer x in lvl.get(0)) {
                for (Integer y in path(x)) {
                    if (p.containsKey(x + y)) break
                    p[x + y] = x
                    q.add x + y
                }
            }
            lvl[0].clear()
            lvl[0].addAll q
        }
        List<Integer> temp = path p[n]
        temp.add n
        temp
    }

    private static BigDecimal treePow(double x, int n) {
        Map<Integer, BigDecimal> r = new HashMap<>()
        r[0] = BigDecimal.ONE
        r[1] = BigDecimal.valueOf(x)

        int p = 0
        for (Integer i in path(n)) {
            r[i] = r[i - p] * r[p]
            p = i
        }
        r[n]
    }

    private static void showPos(double x, int n, boolean isIntegral) {
        printf("%d: %s\n", n, path(n))
        String f = isIntegral ? "%.0f" : "%f"
        printf(f, x)
        printf(" ^ %d = ", n)
        printf(f, treePow(x, n))
        println()
        println()
    }

    static void main(String[] args) {
        for (int n = 0; n <= 17; ++n) {
            showPos 2.0, n, true
        }
        showPos 1.1, 81, false
        showPos 3.0, 191, true
    }
}
```

```txt
0: []
2 ^ 0 = 1

1: [1]
2 ^ 1 = 2

2: [1, 2]
2 ^ 2 = 4

3: [1, 2, 3]
2 ^ 3 = 8

4: [1, 2, 4]
2 ^ 4 = 16

5: [1, 2, 4, 5]
2 ^ 5 = 32

6: [1, 2, 4, 6]
2 ^ 6 = 64

7: [1, 2, 4, 6, 7]
2 ^ 7 = 128

8: [1, 2, 4, 8]
2 ^ 8 = 256

9: [1, 2, 4, 8, 9]
2 ^ 9 = 512

10: [1, 2, 4, 8, 10]
2 ^ 10 = 1024

11: [1, 2, 4, 8, 10, 11]
2 ^ 11 = 2048

12: [1, 2, 4, 8, 12]
2 ^ 12 = 4096

13: [1, 2, 4, 8, 12, 13]
2 ^ 13 = 8192

14: [1, 2, 4, 8, 12, 14]
2 ^ 14 = 16384

15: [1, 2, 4, 8, 12, 14, 15]
2 ^ 15 = 32768

16: [1, 2, 4, 8, 16]
2 ^ 16 = 65536

17: [1, 2, 4, 8, 16, 17]
2 ^ 17 = 131072

81: [1, 2, 4, 8, 16, 32, 64, 80, 81]
1.100000 ^ 81 = 2253.240236

191: [1, 2, 4, 8, 16, 32, 64, 128, 160, 176, 184, 188, 190, 191]
3 ^ 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
```






## Haskell

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Rosetta.PowerTree
    ( Natural
    , powerTree
    , power
    )  where

import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.List (foldl')
import           Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import           Numeric.Natural (Natural)

type M = Map Natural S
type S = Seq Natural

levels :: [M]
levels = let s = Seq.singleton 1 in fst <$> iterate step (Map.singleton 1 s, s)

step :: (M, S) -> (M, S)
step (m, xs) = foldl' f (m, Empty) xs
  where
    f :: (M, S) -> Natural -> (M, S)
    f (m', ys) n = foldl' g (m', ys) ns
      where
        ns :: S
        ns = m' Map.! n

        g :: (M, S) -> Natural -> (M, S)
        g (m'', zs) k =
            let l = n + k
            in  case Map.lookup l m'' of
                    Nothing -> (Map.insert l (ns |>  l) m'', zs |> l)
                    Just _  -> (m'', zs)

powerTree :: Natural -> [Natural]
powerTree n
    | n <= 0    = []
    | otherwise = go levels
  where
    go :: [M] -> [Natural]
    go []       = error "impossible branch"
    go (m : ms) = fromMaybe (go ms) $ toList <$> Map.lookup n m

power :: forall a. Num a => a -> Natural -> a
power _ 0 = 1
power a n = go a 1 (Map.singleton 1 a) $ tail $ powerTree n
  where
    go :: a -> Natural -> Map Natural a -> [Natural] -> a
    go b _ _ []       = b
    go b k m (l : ls) =
        let b' = b * m Map.! (l - k)
            m' = Map.insert l b' m
        in  go b' l m' ls
```

(The <tt>CReal</tt> type from package <tt>numbers</tt> is used to get the ''exact'' result for the last example.)

```txt

powerTree 0 = [], power 2 0 = 1
powerTree 1 = [1], power 2 1 = 2
powerTree 2 = [1,2], power 2 2 = 4
powerTree 3 = [1,2,3], power 2 3 = 8
powerTree 4 = [1,2,4], power 2 4 = 16
powerTree 5 = [1,2,3,5], power 2 5 = 32
powerTree 6 = [1,2,3,6], power 2 6 = 64
powerTree 7 = [1,2,3,5,7], power 2 7 = 128
powerTree 8 = [1,2,4,8], power 2 8 = 256
powerTree 9 = [1,2,3,6,9], power 2 9 = 512
powerTree 10 = [1,2,3,5,10], power 2 10 = 1024
powerTree 11 = [1,2,3,5,10,11], power 2 11 = 2048
powerTree 12 = [1,2,3,6,12], power 2 12 = 4096
powerTree 13 = [1,2,3,5,10,13], power 2 13 = 8192
powerTree 14 = [1,2,3,5,7,14], power 2 14 = 16384
powerTree 15 = [1,2,3,5,10,15], power 2 15 = 32768
powerTree 16 = [1,2,4,8,16], power 2 16 = 65536
powerTree 17 = [1,2,4,8,16,17], power 2 17 = 131072
powerTree 191 = [1,2,3,5,7,14,19,38,57,95,190,191], power 3 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
powerTree 81 = [1,2,3,5,10,20,40,41,81], power 1.1 81 = 2253.240236044012487937308538033349567966729852481170503814810577345406584190098644811

```



## J


This task is a bit verbose...

We can represent the tree as a list of indices. Each entry in the list gives the value of <code>n</code> for the index <code>a+n</code>. (We can find <code>a</code> using subtraction.)


```J
knuth_power_tree=:3 :0
  L=: P=: %(1+y){._ 1
  findpath=: ]
  while. _ e.P do.
    for_n.(/: findpath&>)I.L=>./L-._ do.
      for_a. findpath n do.
         j=. n+a
         l=. 1+n{L
         if. j>y do. break. end.
         if. l>:j{ L do. continue. end.
         L=: l j} L
         P=: n j} P
      end.
      findpath=: [: |. {&P^:a:
    end.
  end.
  P
)

usepath=:4 :0
  path=. findpath y
  exp=. 1,({:path)#x
  for_ex.(,.~2 -~/\"1])2 ,\path  do.
    'ea eb ec'=. ex
    exp=.((ea{exp)*eb{exp) ec} exp
  end.
  {:exp
)
```


Task examples:


```J
   knuth_power_tree 191 NB. generate sufficiently large tree
0 1 1 2 2 3 3 5 4 6 5 10 6 10 7 10 8 16 9 14 10 14 11 13 12 15 13 18 14 28 15 28 16 17 17 21 18 36 19 26 20 40 21 40 22 30 23 42 24 48 25 48 26 52 27 44 28 38 29 31 30 56 31 42 32 64 33 66 34 46 35 57 36 37 37 50 38 76 39 76 40 41 41 43 42 80 43 84 44 47 45 70 46 62 47 57 48 49 49 51 50 100 51 100 52 70 53 104 54 104 55 108 56 112 57 112 58 61 59 112 60 120 61 120 62 75 63 126 64 65 65 129 66 67 67 90 68 136 69 138 70 140 71 140 72 144 73 144 74 132 75 138 76 144 77 79 78 152 79 152 80 160 81 160 82 85 83 162 84 168 85 114 86 168 87 105 88 118 89 176 90 176 91 122 92 184 93 176 94 126 95 190

   findpath 0
0
   2 usepath 0
1

   findpath 1
1
   2 usepath 1
2

   findpath 2
1 2
   2 usepath 2
4

   findpath 3
1 2 3
   2 usepath 3
8

   findpath 4
1 2 4
   2 usepath 4
16

   findpath 5
1 2 3 5
   2 usepath 5
32

   findpath 6
1 2 3 6
   2 usepath 6
64

   findpath 7
1 2 3 5 7
   2 usepath 7
128

   findpath 8
1 2 4 8
   2 usepath 8
256

   findpath 9
1 2 3 6 9
   2 usepath 9
512

   findpath 10
1 2 3 5 10
   2 usepath 10
1024

   findpath 11
1 2 3 5 10 11
   2 usepath 11
2048

   findpath 12
1 2 3 6 12
   2 usepath 12
4096

   findpath 13
1 2 3 5 10 13
   2 usepath 13
8192

   findpath 14
1 2 3 5 7 14
   2 usepath 14
16384

   findpath 15
1 2 3 5 10 15
   2 usepath 15
32768

   findpath 16
1 2 4 8 16
   2 usepath 16
65536

   findpath 17
1 2 4 8 16 17
   2 usepath 17
131072

   findpath 191
1 2 3 5 7 14 19 38 57 95 190 191
   3x usepath 191
13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

   findpath 81
1 2 3 5 10 20 40 41 81
   (x:1.1) usepath 81
2253240236044012487937308538033349567966729852481170503814810577345406584190098644811r1000000000000000000000000000000000000000000000000000000000000000000000000000000000

```


Note that an 'r' in a number indicates a rational number with the numerator to the left of the r and the denominator to the right of the r. We could instead use decimal notation by indicating how many characters of result we want to see, as well as how many characters to the right of the decimal point we want to see.

Thus, for example:


```J
   90j83 ": (x:1.1) usepath 81
  2253.24023604401248793730853803334956796672985248117050381481057734540658419009864481100
```



## Java

```Java
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PowerTree {
    private static Map<Integer, Integer> p = new HashMap<>();
    private static List<List<Integer>> lvl = new ArrayList<>();

    static {
        p.put(1, 0);

        ArrayList<Integer> temp = new ArrayList<>();
        temp.add(1);
        lvl.add(temp);
    }

    private static List<Integer> path(int n) {
        if (n == 0) return new ArrayList<>();
        while (!p.containsKey(n)) {
            List<Integer> q = new ArrayList<>();
            for (Integer x : lvl.get(0)) {
                for (Integer y : path(x)) {
                    if (p.containsKey(x + y)) break;
                    p.put(x + y, x);
                    q.add(x + y);
                }
            }
            lvl.get(0).clear();
            lvl.get(0).addAll(q);
        }
        List<Integer> temp = path(p.get(n));
        temp.add(n);
        return temp;
    }

    private static BigDecimal treePow(double x, int n) {
        Map<Integer, BigDecimal> r = new HashMap<>();
        r.put(0, BigDecimal.ONE);
        r.put(1, BigDecimal.valueOf(x));

        int p = 0;
        for (Integer i : path(n)) {
            r.put(i, r.get(i - p).multiply(r.get(p)));
            p = i;
        }
        return r.get(n);
    }

    private static void showPow(double x, int n, boolean isIntegral) {
        System.out.printf("%d: %s\n", n, path(n));
        String f = isIntegral ? "%.0f" : "%f";
        System.out.printf(f, x);
        System.out.printf(" ^ %d = ", n);
        System.out.printf(f, treePow(x, n));
        System.out.println("\n");
    }

    public static void main(String[] args) {
        for (int n = 0; n <= 17; ++n) {
            showPow(2.0, n, true);
        }
        showPow(1.1, 81, false);
        showPow(3.0, 191, true);
    }
}
```

```txt
0: []
2 ^ 0 = 1

1: [1]
2 ^ 1 = 2

2: [1, 2]
2 ^ 2 = 4

3: [1, 2, 3]
2 ^ 3 = 8

4: [1, 2, 4]
2 ^ 4 = 16

5: [1, 2, 4, 5]
2 ^ 5 = 32

6: [1, 2, 4, 6]
2 ^ 6 = 64

7: [1, 2, 4, 6, 7]
2 ^ 7 = 128

8: [1, 2, 4, 8]
2 ^ 8 = 256

9: [1, 2, 4, 8, 9]
2 ^ 9 = 512

10: [1, 2, 4, 8, 10]
2 ^ 10 = 1024

11: [1, 2, 4, 8, 10, 11]
2 ^ 11 = 2048

12: [1, 2, 4, 8, 12]
2 ^ 12 = 4096

13: [1, 2, 4, 8, 12, 13]
2 ^ 13 = 8192

14: [1, 2, 4, 8, 12, 14]
2 ^ 14 = 16384

15: [1, 2, 4, 8, 12, 14, 15]
2 ^ 15 = 32768

16: [1, 2, 4, 8, 16]
2 ^ 16 = 65536

17: [1, 2, 4, 8, 16, 17]
2 ^ 17 = 131072

81: [1, 2, 4, 8, 16, 32, 64, 80, 81]
1.100000 ^ 81 = 2253.240236

191: [1, 2, 4, 8, 16, 32, 64, 128, 160, 176, 184, 188, 190, 191]
3 ^ 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
```



## Julia

'''Module''':

```julia
module KnuthPowerTree

const p = Dict(1 => 0)
const lvl = [[1]]

function path(n)
    global p, lvl
    iszero(n) && return Int[]
    while n ∉ keys(p)
        q = Int[]
        for x in lvl[1], y in path(x)
            if (x + y) ∉ keys(p)
                p[x + y] = x
                push!(q, x + y)
            end
        end
        lvl[1] = q
    end
    return push!(path(p[n]), n)
end

function pow(x::Number, n::Integer)
    r = Dict{typeof(n), typeof(x)}(0 => 1, 1 => x)
    p = 0
    for i in path(n)
        r[i] = r[i - p] * r[p]
        p = i
    end
    return r[n]
end

end  # module KnuthPowerTree
```


'''Main''':

```julia
using .KnuthPowerTree: path, pow

for n in 0:17
    println("2 ^ $n:\n - path: ", join(path(n), ", "), "\n - result: ", pow(2, n))
end

for (x, n) in ((big(3), 191), (1.1, 81))
    println("$x ^ $n:\n - path: ", join(path(n), ", "), "\n - result: ", pow(x, n))
end
```


```txt
2 ^ 0:
 - path:
 - result: 1
2 ^ 1:
 - path: 1
 - result: 2
2 ^ 2:
 - path: 1, 2
 - result: 4
2 ^ 3:
 - path: 1, 2, 3
 - result: 8
2 ^ 4:
 - path: 1, 2, 4
 - result: 16
2 ^ 5:
 - path: 1, 2, 3, 5
 - result: 32
2 ^ 6:
 - path: 1, 2, 3, 6
 - result: 64
2 ^ 7:
 - path: 1, 2, 3, 5, 7
 - result: 128
2 ^ 8:
 - path: 1, 2, 4, 8
 - result: 256
2 ^ 9:
 - path: 1, 2, 3, 6, 9
 - result: 512
2 ^ 10:
 - path: 1, 2, 3, 5, 10
 - result: 1024
2 ^ 11:
 - path: 1, 2, 3, 5, 10, 11
 - result: 2048
2 ^ 12:
 - path: 1, 2, 3, 6, 12
 - result: 4096
2 ^ 13:
 - path: 1, 2, 3, 5, 10, 13
 - result: 8192
2 ^ 14:
 - path: 1, 2, 3, 5, 7, 14
 - result: 16384
2 ^ 15:
 - path: 1, 2, 3, 5, 10, 15
 - result: 32768
2 ^ 16:
 - path: 1, 2, 4, 8, 16
 - result: 65536
2 ^ 17:
 - path: 1, 2, 4, 8, 16, 17
 - result: 131072
3 ^ 191:
 - path: 1, 2, 3, 5, 7, 14, 19, 38, 57, 95, 190, 191
 - result: 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
1.1 ^ 81:
 - path: 1, 2, 3, 5, 10, 20, 40, 41, 81
 - result: 2253.2402360440283
```



## Kotlin

```scala
// version 1.1.3

import java.math.BigDecimal

var p = mutableMapOf(1 to 0)
var lvl = mutableListOf(listOf(1))

fun path(n: Int): List<Int> {
    if (n == 0) return emptyList<Int>()
    while (n !in p) {
        val q = mutableListOf<Int>()
        for (x in lvl[0]) {
            for (y in path(x)) { 
                if ((x + y) in p) break
                p[x + y] = x
                q.add(x + y)
            } 
        }
        lvl[0] = q
    }
    return path(p[n]!!) + n
}

fun treePow(x: Double, n: Int): BigDecimal {
    val r = mutableMapOf(0 to BigDecimal.ONE, 1 to BigDecimal(x.toString()))
    var p = 0
    for (i in path(n)) {
        r[i] = r[i - p]!! * r[p]!!
        p = i
    }
    return r[n]!!
}

fun showPow(x: Double, n: Int, isIntegral: Boolean = true) {
    println("$n: ${path(n)}")
    val f = if (isIntegral) "%.0f" else "%f"
    println("${f.format(x)} ^ $n = ${f.format(treePow(x, n))}\n")
} 

fun main(args: Array<String>) {
    for (n in 0..17) showPow(2.0, n)
    showPow(1.1, 81, false)
    showPow(3.0, 191)
}
```


```txt

0: []
2 ^ 0 = 1

1: [1]
2 ^ 1 = 2

2: [1, 2]
2 ^ 2 = 4

3: [1, 2, 3]
2 ^ 3 = 8

4: [1, 2, 4]
2 ^ 4 = 16

5: [1, 2, 4, 5]
2 ^ 5 = 32

6: [1, 2, 4, 6]
2 ^ 6 = 64

7: [1, 2, 4, 6, 7]
2 ^ 7 = 128

8: [1, 2, 4, 8]
2 ^ 8 = 256

9: [1, 2, 4, 8, 9]
2 ^ 9 = 512

10: [1, 2, 4, 8, 10]
2 ^ 10 = 1024

11: [1, 2, 4, 8, 10, 11]
2 ^ 11 = 2048

12: [1, 2, 4, 8, 12]
2 ^ 12 = 4096

13: [1, 2, 4, 8, 12, 13]
2 ^ 13 = 8192

14: [1, 2, 4, 8, 12, 14]
2 ^ 14 = 16384

15: [1, 2, 4, 8, 12, 14, 15]
2 ^ 15 = 32768

16: [1, 2, 4, 8, 16]
2 ^ 16 = 65536

17: [1, 2, 4, 8, 16, 17]
2 ^ 17 = 131072

81: [1, 2, 4, 8, 16, 32, 64, 80, 81]
1.100000 ^ 81 = 2253.240236

191: [1, 2, 4, 8, 16, 32, 64, 128, 160, 176, 184, 188, 190, 191]
3 ^ 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## Perl


```perl
my @lvl = [1];
my %p = (1 => 0);

sub path {
    my ($n) = @_;
    return () if ($n == 0);
    until (exists $p{$n}) {
        my @q;
        foreach my $x (@{$lvl[0]}) {
            foreach my $y (path($x)) {
                my $z = $x + $y;
                last if exists($p{$z});
                $p{$z} = $x;
                push @q, $z;
            }
        }
        $lvl[0] = \@q;
    }
    (path($p{$n}), $n);
}

sub tree_pow {
    my ($x, $n) = @_;
    my %r = (0 => 1, 1 => $x);
    my $p = 0;
    foreach my $i (path($n)) {
        $r{$i} = $r{$i - $p} * $r{$p};
        $p = $i;
    }
    $r{$n};
}

sub show_pow {
    my ($x, $n) = @_;
    my $fmt = "%d: %s\n" . ("%g^%s = %f", "%s^%s = %s")[$x == int($x)] . "\n";
    printf($fmt, $n, "(" . join(" ", path($n)) . ")", $x, $n, tree_pow($x, $n));
}

show_pow(2, $_) for 0 .. 17;
show_pow(1.1, 81);
{
    use bigint (try => 'GMP');
    show_pow(3, 191);
}
```

<pre style="height:32ex;overflow:scroll">
0: ()
2^0 = 1
1: (1)
2^1 = 2
2: (1 2)
2^2 = 4
3: (1 2 3)
2^3 = 8
4: (1 2 4)
2^4 = 16
5: (1 2 4 5)
2^5 = 32
6: (1 2 4 6)
2^6 = 64
7: (1 2 4 6 7)
2^7 = 128
8: (1 2 4 8)
2^8 = 256
9: (1 2 4 8 9)
2^9 = 512
10: (1 2 4 8 10)
2^10 = 1024
11: (1 2 4 8 10 11)
2^11 = 2048
12: (1 2 4 8 12)
2^12 = 4096
13: (1 2 4 8 12 13)
2^13 = 8192
14: (1 2 4 8 12 14)
2^14 = 16384
15: (1 2 4 8 12 14 15)
2^15 = 32768
16: (1 2 4 8 16)
2^16 = 65536
17: (1 2 4 8 16 17)
2^17 = 131072
81: (1 2 4 8 16 32 64 80 81)
1.1^81 = 2253.240236
191: (1 2 4 8 16 32 64 128 160 176 184 188 190 191)
3^191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## Perl 6

Paths are random. It is possible replace <code>.pick(*)</code> with <code>.reverse</code> if you want paths as in Perl, or remove it for Python paths.

```perl6
use v6;

sub power-path ($n ) {
    state @unused_nodes = (2,);
    state @power-tree = (False,0,1);
    
    until @power-tree[$n].defined {
        my $node = @unused_nodes.shift;

        for  $node X+ power-path($node).pick(*) {
            next if @power-tree[$_].defined;
            @unused_nodes.push($_);
            @power-tree[$_]= $node;
        }        
    }

    ( $n, { @power-tree[$_] } ...^ 0 ).reverse;
}

multi power ( $, 0 ) { 1 };
multi power ( $n, $exponent ) {
    state  %p;
    my     %r =  %p{$n}  // ( 0 => 1, 1 => $n ) ;  

    for power-path( $exponent ).rotor( 2 => -1 ) -> ( $p, $c ) {
        %r{ $c } = %r{ $p } * %r{ $c - $p }
    }

    %p{$n} := %r ;
    %r{ $exponent }
}

say 'Power paths: ',      pairs map *.&power-path,    ^18;
say '2 ** key = value: ', pairs map { 2.&power($_) }, ^18; 

say 'Path for 191: ', power-path 191;
say '3 ** 191 = ',    power   3, 191;
say 'Path for 81: ',  power-path  81;
say '1.1 ** 81 = ',   power 1.1,  81;

```

```txt

Power paths: (0 => () 1 => (1) 2 => (1 2) 3 => (1 2 3) 4 => (1 2 4) 5 => (1 2 3 5) 6 => (1 2 3 6) 7 => (1 2 3 6 7) 8 => (1 2 4 8) 9 => (1 2 3 6 9) 10 => (1 2 3 5 10) 11 => (1 2 3 6 9 11) 12 => (1 2 3 6 12) 13 => (1 2 3 6 12 13) 14 => (1 2 3 6 12 14) 15 => (1 2 3 6 9 15) 16 => (1 2 4 8 16) 17 => (1 2 4 8 16 17))
2 ** key = value: (0 => 1 1 => 2 2 => 4 3 => 8 4 => 16 5 => 32 6 => 64 7 => 128 8 => 256 9 => 512 10 => 1024 11 => 2048 12 => 4096 13 => 8192 14 => 16384 15 => 32768 16 => 65536 17 => 131072)
Path for 191: (1 2 3 6 9 18 27 54 108 162 189 191)
3 ** 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
Path for 81: (1 2 3 6 9 18 27 54 81)
1.1 ** 81 = 2253.24023604401

```



## Phix

```Phix
constant p = new_dict({{1,0}})
sequence lvl = {1}

function path(integer n)
    if n=0 then return {} end if
    while getd_index(n,p)=NULL do
        sequence q = {}
        for i=1 to length(lvl) do
            integer x = lvl[i]
            sequence px = path(x)
            for j=1 to length(px) do
                integer y = x+px[j]
                if getd_index(y,p)!=NULL then exit end if
                setd(y,x,p)
                q &= y
            end for
        end for
        lvl = q
    end while
    return path(getd(n,p))&n
end function

include mpfr.e
mpfr_set_default_prec(500)
 
function treepow(object x, integer n, sequence pn = {})
-- x can be atom or string (but not mpfr)
-- (asides: sequence r uses out-by-1 indexing, ie r[1] is for 0.
--          sequence c is used to double-check we are not trying
--          to use something which has not yet been calculated.)
    if pn={} then pn=path(n) end if
    sequence r = {mpfr_init(1),mpfr_init(x)},
             c = {1,1}&repeat(0,max(0,n-1))
    for i=1 to max(0,n-1) do r &= mpfr_init() end for
    integer p = 0
    for i=1 to length(pn) do
        integer pi = pn[i]
        if c[pi-p+1]=0 then ?9/0 end if
        if c[p+1]=0 then ?9/0 end if
        mpfr_mul(r[pi+1],r[pi-p+1],r[p+1])
        c[pi+1] = 1
        p = pi
    end for
    string res = trim_tail(mpfr_sprintf("%.83Rf",r[n+1]),".0")
    r = mpfr_free(r)
    return res
end function
 
procedure showpow(object x, integer n)
    sequence pn = path(n)
    string xs = iff(string(x)?x:sprintf("%3g",x))
    printf(1,"%48s : %3s ^ %d = %s\n", {sprint(pn),xs,n,treepow(x,n,pn)})
end procedure
 
for n=0 to 17 do
    showpow(2,n)
end for
showpow("1.1",81)
showpow(3,191)
```

<pre style="font-size: 12px">
                                              {} :   2 ^ 0 = 1
                                             {1} :   2 ^ 1 = 2
                                           {1,2} :   2 ^ 2 = 4
                                         {1,2,3} :   2 ^ 3 = 8
                                         {1,2,4} :   2 ^ 4 = 16
                                       {1,2,4,5} :   2 ^ 5 = 32
                                       {1,2,4,6} :   2 ^ 6 = 64
                                     {1,2,4,6,7} :   2 ^ 7 = 128
                                       {1,2,4,8} :   2 ^ 8 = 256
                                     {1,2,4,8,9} :   2 ^ 9 = 512
                                    {1,2,4,8,10} :   2 ^ 10 = 1024
                                 {1,2,4,8,10,11} :   2 ^ 11 = 2048
                                    {1,2,4,8,12} :   2 ^ 12 = 4096
                                 {1,2,4,8,12,13} :   2 ^ 13 = 8192
                                 {1,2,4,8,12,14} :   2 ^ 14 = 16384
                              {1,2,4,8,12,14,15} :   2 ^ 15 = 32768
                                    {1,2,4,8,16} :   2 ^ 16 = 65536
                                 {1,2,4,8,16,17} :   2 ^ 17 = 131072
                        {1,2,4,8,16,32,64,80,81} : 1.1 ^ 81 = 2253.240236044012487937308538033349567966729852481170503814810577345406584190098644811
  {1,2,4,8,16,32,64,128,160,176,184,188,190,191} :   3 ^ 191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## Python


```python
from __future__ import print_function

# remember the tree generation state and expand on demand
def path(n, p = {1:0}, lvl=[[1]]):
	if not n: return []
	while n not in p:
		q = []
		for x,y in ((x, x+y) for x in lvl[0] for y in path(x) if not x+y in p):
			p[y] = x
			q.append(y)
		lvl[0] = q

	return path(p[n]) + [n]

def tree_pow(x, n):
    r, p = {0:1, 1:x}, 0
    for i in path(n):
        r[i] = r[i-p] * r[p]
        p = i
    return r[n]

def show_pow(x, n):
    fmt = "%d: %s\n" + ["%g^%d = %f", "%d^%d = %d"][x==int(x)] + "\n"
    print(fmt % (n, repr(path(n)), x, n, tree_pow(x, n)))

for x in range(18): show_pow(2, x)
show_pow(3, 191)
show_pow(1.1, 81)
```

```txt

0: []
2^0 = 1

1: [1]
2^1 = 2

2: [1, 2]
2^2 = 4

<... snipped ...>

17: [1, 2, 4, 8, 16, 17]
2^17 = 131072

191: [1, 2, 3, 5, 7, 14, 19, 38, 57, 95, 190, 191]
3^191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

81: [1, 2, 3, 5, 10, 20, 40, 41, 81]
1.1^81 = 2253.240236

```



## Racket

```Racket
#lang racket

(define pow-path-cache (make-hash '((0 . (0)) (1 . (0 1)))))

(define pow-path-level '(1))  

(define (pow-path-extend!)
  (define next-level
    (for*/fold ([next-level '()])
               ([x (in-list pow-path-level)]
                [y (in-list (pow-path x))]
                [s (in-value (+ x y))]
                #:when (not (hash-has-key? pow-path-cache s)))
      (hash-set! pow-path-cache s (append (hash-ref pow-path-cache x) (list s)))
      (cons s next-level)))
  (set! pow-path-level (reverse next-level)))

(define (pow-path n)
  (let loop ()
    (unless (hash-has-key? pow-path-cache n)
      (pow-path-extend!)
      (loop)))
 (hash-ref pow-path-cache n))
 
(define (pow-tree x n)
  (define pows (make-hash `((0 . 1) (1 . ,x))))
  (for/fold ([prev 0])
            ([i (in-list (pow-path n))])
    (hash-set! pows i (* (hash-ref pows (- i prev)) (hash-ref pows prev)))
    i)
  (hash-ref pows n))
 
(define (show-pow x n)
  (printf "~a: ~a\n" n (cdr (pow-path n)))
  (printf "~a^~a = ~a\n" x n (pow-tree x n)))
 
(for ([x (in-range 18)])
  (show-pow 2 x))
(show-pow 3 191)
(show-pow 1.1 81)
```

```txt
0: ()
2^0 = 1
1: (1)
2^1 = 2
2: (1 2)
2^2 = 4
3: (1 2 3)
2^3 = 8
4: (1 2 4)
2^4 = 16
5: (1 2 3 5)
2^5 = 32
6: (1 2 3 6)
2^6 = 64
7: (1 2 3 5 7)
2^7 = 128
8: (1 2 4 8)
2^8 = 256
9: (1 2 3 6 9)
2^9 = 512
10: (1 2 3 5 10)
2^10 = 1024
11: (1 2 3 5 10 11)
2^11 = 2048
12: (1 2 3 6 12)
2^12 = 4096
13: (1 2 3 5 10 13)
2^13 = 8192
14: (1 2 3 5 7 14)
2^14 = 16384
15: (1 2 3 5 10 15)
2^15 = 32768
16: (1 2 4 8 16)
2^16 = 65536
17: (1 2 4 8 16 17)
2^17 = 131072
191: (1 2 3 5 7 14 19 38 57 95 190 191)
3^191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
81: (1 2 3 5 10 20 40 41 81)
1.1^81 = 2253.2402360440283
```



## REXX

This REXX version supports results up to 1,000 decimal digits   (which can be expanded with the   '''numeric digits nnn'''   REXX statement. 

Also, negative powers are supported. 

```rexx
/*REXX program produces & displays a  power tree  for P,  and calculates & displays X^P.*/
numeric digits 1000                              /*be able to handle some large numbers.*/
parse arg XP                                     /*get sets:   X, low power, high power.*/
if XP=''  then XP='2 -4 17   3 191 191   1.1 81' /*Not specified?  Then use the default.*/
          /*────── X LP HP   X  LP  HP    X  LP  ◄── X, low power, high power ··· repeat*/
     do  until XP=''
     parse var XP    x pL pH   XP;    x=x/1      /*get X, lowP, highP; and normalize X. */
     if pH=''  then pH=pL                        /*No highPower?  Then assume lowPower. */

       do e=pL  to pH;    p=abs(e)/1             /*use a range of powers;   use  │E│    */
       $=powerTree(p);    w=length(pH)           /*construct the power tree, (pow list).*/
                                                 /* [↑]  W≡length for an aligned display*/
           do i=1  for words($);  @.i=word($,i)  /*build a fast Knuth's power tree array*/
           end   /*i*/

       if p==0  then do;  z=1;  call show;  iterate;  end   /*handle case of zero power.*/
       !.=.;  z=x;  !.1=z;  prv=z                /*define/construct the first power of X*/

           do k=2  to words($);      n=@.k       /*obtain the power (number) to be used.*/
           prev=k-1;   diff=n-@.prev             /*these are used for the odd powers.   */
           if n//2==0  then z=prv**2             /*Even power?   Then square the number.*/
                       else z=z*!.diff           /* Odd   "        "  mult. by pow diff.*/
           !.n=z                                 /*remember for other multiplications.  */
           prv=z                                 /*remember for squaring the numbers.   */
           end   /*k*/
       call show                                 /*display the expression and its value.*/
       end       /*e*/
     end         /*until XP ···*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
powerTree: arg y 1 oy;   $=                      /*Z is the result; $ is the power tree.*/
           if y=0 | y=1  then return y           /*handle special cases for zero & unity*/
           #.=0;   @.=0;   #.0=1                 /*define default & initial array values*/
                                                 /* [↓]  add blank "flag" thingy──►list.*/
                   do  while \(y//2);  $=$  ' '  /*reduce "front" even power #s to odd #*/
                   if y\==oy  then $=y $         /*(only)  ignore the first power number*/
                   y=y%2                         /*integer divide the power (it's even).*/
                   end   /*while*/

           if $\==''  then $=y $                 /*re─introduce the last power number.  */
           $=$ oy                                /*insert last power number 1st in list.*/
           if y>1  then do      while  @.y==0;            n=#.0;       m=0
                          do    while  n\==0;             q=0;         s=n
                            do  while  s\==0;             _=n+s
                            if @._==0  then do;  if q==0  then m_=_;   #._=q;  @._=n;  q=_
                                            end
                            s=@.s
                            end   /*while s¬==0*/
                          if q\==0  then do;  #.m=q;  m=m_;   end
                          n=#.n
                          end     /*while n¬==0*/
                        #.m=0
                        end       /*while @.y==0*/
           z=@.y
                            do  while z\==0;  $=z $;  z=@.z;  end     /*build power list*/
           return space($)                                            /*del extra blanks*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: if e<0  then z=format(1/z, , 40)/1;     _=right(e, w)           /*use reciprocal? */
      say left('power tree for '  _  " is: "  $,60)  '═══'  x"^"_  ' is: '  z;      return
```

'''output''' when using the default inputs:

```txt

power tree for  -4  is:  1 2 4                               ═══ 2^-4  is:  0.0625
power tree for  -3  is:  1 2 3                               ═══ 2^-3  is:  0.125
power tree for  -2  is:  1 2                                 ═══ 2^-2  is:  0.25
power tree for  -1  is:  1                                   ═══ 2^-1  is:  0.5
power tree for   0  is:  0                                   ═══ 2^ 0  is:  1
power tree for   1  is:  1                                   ═══ 2^ 1  is:  2
power tree for   2  is:  1 2                                 ═══ 2^ 2  is:  4
power tree for   3  is:  1 2 3                               ═══ 2^ 3  is:  8
power tree for   4  is:  1 2 4                               ═══ 2^ 4  is:  16
power tree for   5  is:  1 2 3 5                             ═══ 2^ 5  is:  32
power tree for   6  is:  1 2 3 6                             ═══ 2^ 6  is:  64
power tree for   7  is:  1 2 3 5 7                           ═══ 2^ 7  is:  128
power tree for   8  is:  1 2 4 8                             ═══ 2^ 8  is:  256
power tree for   9  is:  1 2 3 6 9                           ═══ 2^ 9  is:  512
power tree for  10  is:  1 2 3 5 10                          ═══ 2^10  is:  1024
power tree for  11  is:  1 2 3 5 10 11                       ═══ 2^11  is:  2048
power tree for  12  is:  1 2 3 6 12                          ═══ 2^12  is:  4096
power tree for  13  is:  1 2 3 5 10 13                       ═══ 2^13  is:  8192
power tree for  14  is:  1 2 3 5 7 14                        ═══ 2^14  is:  16384
power tree for  15  is:  1 2 3 5 10 15                       ═══ 2^15  is:  32768
power tree for  16  is:  1 2 4 8 16                          ═══ 2^16  is:  65536
power tree for  17  is:  1 2 4 8 16 17                       ═══ 2^17  is:  131072
power tree for  191  is:  1 2 3 5 7 14 19 38 57 95 190 191   ═══ 3^191  is:  13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347
power tree for  81  is:  1 2 3 5 10 20 40 41 81              ═══ 1.1^81  is:  2253.240236044012487937308538033349567966729852481170503814810577345406584190098644811

```



## Sidef

```ruby
var lvl = [[1]]
var p = Hash(1 => 0)

func path(n) is cached {
    n || return []
    while (n !~ p) {
        var q = []
        for x in lvl[0] {
            for y in path(x) {
                break if (x+y ~~ p)
                y = x+y
                p{y} = x
                q << y
            }
        }
        lvl[0] = q
    }
    path(p{n}) + [n]
}

func tree_pow(x, n) {
    var r = Hash(0 => 1, 1 => x)
    var p = 0
    for i in path(n) {
        r{i} = (r{i-p} * r{p})
        p = i
    }
    r{n}
}

func show_pow(x, n) {
    var fmt = ("%d: %s\n" + ["%g^%s = %f", "%s^%s = %s"][x.is_int] + "\n")
    print(fmt % (n, path(n), x, n, tree_pow(x, n)))
}

for x in ^18 { show_pow(2, x) }
show_pow(1.1, 81)
show_pow(3, 191)
```

<pre style="height:32ex;overflow:scroll">
0: []
2^0 = 1
1: [1]
2^1 = 2
2: [1, 2]
2^2 = 4
3: [1, 2, 3]
2^3 = 8
4: [1, 2, 4]
2^4 = 16
5: [1, 2, 4, 5]
2^5 = 32
6: [1, 2, 4, 6]
2^6 = 64
7: [1, 2, 4, 6, 7]
2^7 = 128
8: [1, 2, 4, 8]
2^8 = 256
9: [1, 2, 4, 8, 9]
2^9 = 512
10: [1, 2, 4, 8, 10]
2^10 = 1024
11: [1, 2, 4, 8, 10, 11]
2^11 = 2048
12: [1, 2, 4, 8, 12]
2^12 = 4096
13: [1, 2, 4, 8, 12, 13]
2^13 = 8192
14: [1, 2, 4, 8, 12, 14]
2^14 = 16384
15: [1, 2, 4, 8, 12, 14, 15]
2^15 = 32768
16: [1, 2, 4, 8, 16]
2^16 = 65536
17: [1, 2, 4, 8, 16, 17]
2^17 = 131072
81: [1, 2, 4, 8, 16, 32, 64, 80, 81]
1.1^81 = 2253.240236
191: [1, 2, 4, 8, 16, 32, 64, 128, 160, 176, 184, 188, 190, 191]
3^191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```



## zkl

```zkl
# remember the tree generation state and expand on demand
fcn path(n,p=Dictionary(1,0),lvl=List(List(1))){
   if(n==0) return(T);
   while(not p.holds(n)){
      q:=List();
      foreach x,y in (lvl[0],path(x,p,lvl)){
         if(p.holds(x+y)) break;  // not this y
	 y=x+y; p[y]=x;
	 q.append(y);
      }
      lvl[0]=q
   }
   path(p[n],p,lvl) + n
}

fcn tree_pow(x,n,path){
   r,p:=Dictionary(0,1, 1,x), 0;
   foreach i in (path){ r[i]=r[i-p]*r[p]; p=i; }
   r[n]
}
 
fcn show_pow(x,n){
   fmt:="%d: %s\n" + T("%g^%d = %f", "%d^%d = %d")[x==Int(x)] + "\n";
   println(fmt.fmt(n,p:=path(n),x,n,tree_pow(x,n,p)))
}
```


```zkl
foreach x in (18){ show_pow(2,x) }
show_pow(1.1,81);

var [const] BN=Import("zklBigNum");  // GNU GMP big ints
show_pow(BN(3),191);
```

<pre style="height:32ex;overflow:scroll">
0: L()
2^0 = 1

1: L(1)
2^1 = 2

2: L(1,2)
2^2 = 4

3: L(1,2,3)
2^3 = 8

4: L(1,2,4)
2^4 = 16

5: L(1,2,4,5)
2^5 = 32

6: L(1,2,4,6)
2^6 = 64

7: L(1,2,4,6,7)
2^7 = 128

8: L(1,2,4,8)
2^8 = 256

9: L(1,2,4,8,9)
2^9 = 512

10: L(1,2,4,8,10)
2^10 = 1024

11: L(1,2,4,8,10,11)
2^11 = 2048

12: L(1,2,4,8,12)
2^12 = 4096

13: L(1,2,4,8,12,13)
2^13 = 8192

14: L(1,2,4,8,12,14)
2^14 = 16384

15: L(1,2,4,8,12,14,15)
2^15 = 32768

16: L(1,2,4,8,16)
2^16 = 65536

17: L(1,2,4,8,16,17)
2^17 = 131072

81: L(1,2,4,8,16,32,64,80,81)
1.1^81 = 2253.240236

191: L(1,2,4,8,16,32,64,128,160,176,184,188,190,191)
3^191 = 13494588674281093803728157396523884917402502294030101914066705367021922008906273586058258347

```

