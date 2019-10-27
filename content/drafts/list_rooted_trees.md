+++
title = "List rooted trees"
description = ""
date = 2019-07-20T15:31:04Z
aliases = []
[extra]
id = 19103
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

You came back from grocery shopping.    After putting away all the goods, you are left with a pile of plastic bags, which you want to save for later use, so you take one bag and stuff all the others into it, and throw it under the sink.   In doing so, you realize that there are various ways of nesting the bags, with all bags viewed as identical.

If we use a matching pair of parentheses to represent a bag, the ways are:

For 1 bag, there's one way:
  ()	<- a bag

for 2 bags, there's one way:
  (())	<- one bag in another

for 3 bags, there are two:
  ((())) <- 3 bags nested Russian doll style
  (()()) <- 2 bags side by side, inside the third

for 4 bags, four:
  (()()())
  ((())())
  ((()()))
  (((())))

Note that because all bags are identical, the two 4-bag strings <code>((())())</code> and <code>(()(()))</code> represent the same configuration.

It's easy to see that each configuration for ''n'' bags represents a ''n''-node rooted tree, where a bag is a tree node, and a bag with its content forms a subtree.  The outermost bag is the tree root.  Number of configurations for given ''n'' is given by [https://oeis.org/A000081 OEIS A81].


;Task: 
Write a program that, when given ''n'', enumerates all ways of nesting ''n'' bags.   You can use the parentheses notation above, or any tree representation that's unambiguous and preferably intuitive.

This task asks for enumeration of trees only; for counting solutions without enumeration, that OEIS page lists various formulas, but that's not encouraged by this task, especially if implementing it would significantly increase code size.

As an example output, run 5 bags.   There should be 9 ways.





## C

Trees are represented by integers.  When written out in binary with LSB first, 1 is opening bracket and 0 is closing.

```c>#include <stdio.h

#include <stdlib.h>

typedef unsigned int uint;
typedef unsigned long long tree;
#define B(x) (1ULL<<(x))

tree *list = 0;
uint cap = 0, len = 0;
uint offset[32] = {0, 1, 0};

void append(tree t)
{
	if (len == cap) {
		cap = cap ? cap*2 : 2;
		list = realloc(list, cap*sizeof(tree));
	}
	list[len++] = 1 | t<<1;
}

void show(tree t, uint len)
{
	for (; len--; t >>= 1)
		putchar(t&1 ? '(' : ')');
}

void listtrees(uint n)
{
	uint i;
	for (i = offset[n]; i < offset[n+1]; i++) {
		show(list[i], n*2);
		putchar('\n');
	}
}

/* assemble tree from subtrees
	n:   length of tree we want to make
	t:   assembled parts so far
	sl:  length of subtree we are looking at
	pos: offset of subtree we are looking at
	rem: remaining length to be put together
*/
void assemble(uint n, tree t, uint sl, uint pos, uint rem)
{
	if (!rem) {
		append(t);
		return;
	}

	if (sl > rem) // need smaller subtrees
		pos = offset[sl = rem];
	else if (pos >= offset[sl + 1]) {
		// used up sl-trees, try smaller ones
		if (!--sl) return;
		pos = offset[sl];
	}

	assemble(n, t<<(2*sl) | list[pos], sl, pos, rem - sl);
	assemble(n, t, sl, pos + 1, rem);
}

void mktrees(uint n)
{
	if (offset[n + 1]) return;
	if (n) mktrees(n - 1);

	assemble(n, 0, n-1, offset[n-1], n-1);
	offset[n+1] = len;
}

int main(int c, char**v)
{
	int n;
	if (c < 2 || (n = atoi(v[1])) <= 0 || n > 25) n = 5;

	// init 1-tree
	append(0);

	mktrees((uint)n);
	fprintf(stderr, "Number of %d-trees: %u\n", n, offset[n+1] - offset[n]);
	listtrees((uint)n);

	return 0;
}
```

{{out}}

```txt

% ./a.out 5
Number of 5-trees: 9
((((()))))
(((()())))
((()(())))
((()()()))
(()((())))
(()(()()))
((())(()))
(()()(()))
(()()()())

```



## D

{{trans|C}}

```d
import std.stdio, std.conv;

alias Tree = ulong,
      TreeList = Tree[],
      Offset = uint[32];

void listTees(in uint n, in ref Offset offset, in TreeList list) nothrow @nogc @safe {
    static void show(in Tree t, in uint len) nothrow @nogc @safe {
        foreach (immutable i; 0 .. len)
            putchar(t & (2 ^^ i) ? '(' : ')');
    }

    foreach (immutable i; offset[n] .. offset[n + 1]) {
        show(list[i], n * 2);
        putchar('\n');
    }
}

void append(in Tree t, ref TreeList list, ref uint len) pure nothrow @safe {
    if (len == list.length)
        list.length = list.length ? list.length * 2 : 2;
    list[len] = 1 | (t << 1);
    len++;
}

/**
Assemble tree from subtrees.

Params:
  n   = length of tree we want to make.
  t   = assembled parts so far.
  sl  = length of subtree we are looking at.
  pos = offset of subtree we are looking at.
  rem = remaining length to be put together.
*/
void assemble(in uint n, in Tree t, uint sl, uint pos, in uint rem, in ref Offset offset,
              ref TreeList list, ref uint len) pure nothrow @safe {
    if (!rem) {
        append(t, list, len);
        return;
    }

    if (sl > rem) { // Need smaller subtrees.
        sl = rem;
        pos = offset[sl];
    } else if (pos >= offset[sl + 1]) {
        // Used up sl-trees, try smaller ones.
        sl--;
        if (!sl)
            return;
        pos = offset[sl];
    }

    assemble(n, t << (2 * sl) | list[pos], sl, pos, rem - sl, offset, list, len);
    assemble(n, t, sl, pos + 1, rem, offset, list, len);
}

void makeTrees(in uint n, ref Offset offset,
               ref TreeList list, ref uint len) pure nothrow @safe {
    if (offset[n + 1])
        return;
    if (n)
        makeTrees(n - 1, offset, list, len);

    assemble(n, 0, n - 1, offset[n - 1], n - 1, offset, list, len);
    offset[n + 1] = len;
}

void main(in string[] args) {
    immutable uint n = (args.length == 2) ? args[1].to!uint : 5;
    if (n >= 25)
        return;

    Offset offset;
    offset[1] = 1;

    Tree[] list;
    uint len = 0;

    // Init 1-tree.
    append(0, list, len);

    makeTrees(n, offset, list, len);
    stderr.writefln("Number of %d-trees: %u", n, offset[n + 1] - offset[n]);
    listTees(n, offset, list);
}
```

{{out}}

```txt
Number of 5-trees: 9
((((()))))
(((()())))
((()(())))
((()()()))
(()((())))
(()(()()))
((())(()))
(()()(()))
(()()()())
```



## Go

{{trans|C}}

```go
package main

import (
    "fmt"
    "log"
    "os"
    "strconv"
)

type tree uint64

var (
    list   []tree
    offset = [32]uint{1: 1}
)

func add(t tree) {
    list = append(list, 1|t<<1)
}

func show(t tree, l uint) {
    for ; l > 0; t >>= 1 {
        l--
        var paren byte
        if (t & 1) != 0 {
            paren = '('
        } else {
            paren = ')'
        }
        fmt.Printf("%c", paren)
    }
}

func listTrees(n uint) {
    for i := offset[n]; i < offset[n+1]; i++ {
        show(list[i], n*2)
        fmt.Println()
    }
}

/* assemble tree from subtrees
n:   length of tree we want to make
t:   assembled parts so far
sl:  length of subtree we are looking at
pos: offset of subtree we are looking at
rem: remaining length to be put together
*/

func assemble(n uint, t tree, sl, pos, rem uint) {
    if rem == 0 {
        add(t)
        return
    }

    if sl > rem { // need smaller sub-trees
        sl = rem
        pos = offset[sl]
    } else if pos >= offset[sl+1] {
        // used up sl-trees, try smaller ones
        sl--
        if sl == 0 {
            return
        }
        pos = offset[sl]
    }

    assemble(n, t<<(2*sl)|list[pos], sl, pos, rem-sl)
    assemble(n, t, sl, pos+1, rem)
}

func mktrees(n uint) {
    if offset[n+1] > 0 {
        return
    }
    if n > 0 {
        mktrees(n - 1)
    }

    assemble(n, 0, n-1, offset[n-1], n-1)
    offset[n+1] = uint(len(list))
}

func main() {
    if len(os.Args) != 2 {
        log.Fatal("There must be exactly 1 command line argument")
    }
    n, err := strconv.Atoi(os.Args[1])
    if err != nil {
        log.Fatal("Argument is not a valid number")
    }
    if n <= 0 || n > 19 { // stack overflow for n == 20
        n = 5
    }
    // init 1-tree
    add(0)

    mktrees(uint(n))
    fmt.Fprintf(os.Stderr, "Number of %d-trees: %d\n", n, offset[n+1]-offset[n])
    listTrees(uint(n))
}
```


{{out}}
When passing a command line argument of 5:

```txt

Number of 5-trees: 9
((((()))))
(((()())))
((()(())))
((()()()))
(()((())))
(()(()()))
((())(()))
(()()(()))
(()()()())

```



## Haskell

There probably is a nicer way than the following--

```haskell
-- break n down into sum of smaller integers
parts n = f n 1 where
	f n x	| n == 0 = [[]]
		| x > n = []
		| otherwise = f n (x+1) ++ concatMap (\c->map ((c,x):) (f (n-c*x) (x+1))) [1 .. n`div`x]

-- choose n strings out of a list and join them
pick _ [] = []
pick 0 _ = [""]
pick n aa@(a:as) = map (a++) (pick (n-1) aa) ++ pick n as

-- pick parts to build a series of subtrees that add up to n-1, then wrap them up
trees n = map (\x->"("++x++")") $ concatMap (foldr (prod.build) [""]) (parts (n-1)) where
	build (c,x) = pick c $ trees x
	prod aa bb = [ a++b | a<-aa, b<-bb ]

main = mapM_ putStrLn $ trees 5
```

{{out}}

```txt

((((()))))
(((()())))
((()(())))
((()()()))
((())(()))
(()((())))
(()(()()))
(()()(()))
(()()()())

```


A variant which uses Data.Tree


```haskell
import Data.List (nub, sortBy, foldl') --' strict variant of foldl
import Data.Ord (comparing)
import Data.Tree

bagPatterns :: Int -> [String]
bagPatterns n =
  nub $
  (bracketsFromTree . depthSortedTree . treeFromParentIndices) <$>
  parentIndexPermutations n

parentIndexPermutations :: Int -> [[Int]]
parentIndexPermutations =
  sequenceA . fmap (enumFromTo 0) . enumFromTo 0 . subtract 2

treeFromParentIndices :: [Int] -> Tree Int
treeFromParentIndices pxs =
  foldl' --' strict variant of foldl
    go
    (Node 0 [])
    (zip [1 .. (length pxs)] pxs)
  where
    go tree tplIP =
      let root = rootLabel tree
          nest = subForest tree
          forest
            | root == snd tplIP = nest ++ [Node (fst tplIP) []]
            | otherwise = (`go` tplIP) <$> nest
      in Node root forest

depthSortedTree
  :: (Num a, Ord a)
  => Tree a -> Tree a
depthSortedTree = go
  where
    go tree
      | null (subForest tree) = Node 0 []
      | otherwise =
        let xs = go <$> subForest tree
        in Node
             (1 + foldr ((+) . rootLabel) 0 xs)
             (sortBy (flip (comparing rootLabel)) xs)

bracketsFromTree :: Tree a -> String
bracketsFromTree = foldNest (\xs -> '(' : (concat xs ++ ")"))

foldNest :: ([b] -> b) -> Tree a -> b
foldNest f =
  let go (Node _ ts) = f (map go ts)
  in go

main :: IO ()
main = putStrLn . unlines $ bagPatterns 5
```

{{Out}}

```txt
(()()()())
((())()())
((()())())
((())(()))
(((()))())
((()()()))
(((())()))
(((()())))
((((()))))
```



## J


Support code:


```J
root=: 1 1 $ _
incr=: ,/@(,"1 0/ i.@{:@$)

boxed=: $:&0 :(<@\:~@([ $:^:(0 < #@]) I.@:=))"1 1 0
```


Task:

<pre style="line-height: 0.9em">   ~.boxed incr^:4 root
┌─────┬──────┬──────┬───────┬───────┬──────┬───────┬───────┬────────┐
│┌┬┬┬┐│┌──┬┬┐│┌───┬┐│┌──┬──┐│┌────┬┐│┌────┐│┌─────┐│┌─────┐│┌──────┐│
││││││││┌┐│││││┌┬┐││││┌┐│┌┐│││┌──┐││││┌┬┬┐│││┌──┬┐│││┌───┐│││┌────┐││
│└┴┴┴┘│││││││││││││││││││││││││┌┐│││││││││││││┌┐││││││┌┬┐│││││┌──┐│││
│     ││└┘│││││└┴┘││││└┘│└┘│││││││││││└┴┴┘│││││││││││││││││││││┌┐││││
│     │└──┴┴┘│└───┴┘│└──┴──┘│││└┘││││└────┘│││└┘││││││└┴┘││││││││││││
│     │      │      │       ││└──┘│││      ││└──┴┘│││└───┘│││││└┘││││
│     │      │      │       │└────┴┘│      │└─────┘│└─────┘│││└──┘│││
│     │      │      │       │       │      │       │       ││└────┘││
│     │      │      │       │       │      │       │       │└──────┘│
└─────┴──────┴──────┴───────┴───────┴──────┴───────┴───────┴────────┘
```


Explanation: while building the trees, we are using the [[Tree_traversal#J:_Alternate_implementation|parent index]] representation of a tree. The tree is represented as a sequence of indices of the parent nodes. We use _ to represent the root node (so our root node has no parent). 

In the boxed representation we use here, each square box represents a bag.

<code>boxed</code> represents a single tree structure in a nested boxed form, with each box representing a bag. Here, we sort each sequence of boxes (which we are thinking of as bags), so we can recognize mechanically different tree structures which happen to represent the same bag structures.

And for the task example, we want four bags into the outside containing bag, and also we want to eliminate redundant representations...

So, for example, here is what some intermediate results would look like for the four bag case:


```J
   incr^:3 root
_ 0 0 0
_ 0 0 1
_ 0 0 2
_ 0 1 0
_ 0 1 1
_ 0 1 2
```


Each row represents a bag with another three bags stuffed into it. Each column represents a bag, and each index is the column of the bag that it is stuffed into. (The first bag isn't stuffed into another bag.)

But some of these are equivalent, we can see that if we use our parenthesis notation and think about how they could be rearranged:


```J
   disp=: ('(' , ')' ,~ [: ; [ <@disp"1 0^:(0 < #@]) I.@:=) {.
   disp incr^:3 root
(()()())
((())())
(()(()))
((())())
((()()))
(((())))
```


But that's not a convenient way of finding the all of the duplicates. So we use a boxed representation - with all boxes at each level in a canonical order (fullest first) - and that makes the duplicates obvious:

<pre style="line-height: 0.9em">   boxed incr^:3 root
┌────┬─────┬─────┬─────┬─────┬──────┐
│┌┬┬┐│┌──┬┐│┌──┬┐│┌──┬┐│┌───┐│┌────┐│
│││││││┌┐││││┌┐││││┌┐││││┌┬┐│││┌──┐││
│└┴┴┘│││││││││││││││││││││││││││┌┐│││
│    ││└┘││││└┘││││└┘││││└┴┘│││││││││
│    │└──┴┘│└──┴┘│└──┴┘│└───┘│││└┘│││
│    │     │     │     │     ││└──┘││
│    │     │     │     │     │└────┘│
└────┴─────┴─────┴─────┴─────┴──────┘
```



## Javascript


### ES6

Composing a solution from generic functions.

```javascript
(() => {
    'use strict';

    const main = () =>
        bagPatterns(5)
        .join('\n');

    // BAG PATTERNS ---------------------------------------

    // bagPatterns :: Int -> [String]
    const bagPatterns = n =>
        nub(map(
            composeList([
                commasFromTree,
                depthSortedTree,
                treeFromParentIndices
            ]),
            parentIndexPermutations(n)
        ));

    // parentIndexPermutations :: Int -> [[Int]]
    const parentIndexPermutations = n =>
        sequenceA(
            map(curry(enumFromToInt)(0),
                enumFromToInt(0, n - 2)
            )
        );

    // treeFromParentIndices :: [Int] -> Tree Int
    const treeFromParentIndices = pxs => {
        const go = (tree, tplIP) =>
            Node(
                tree.root,
                tree.root === snd(tplIP) ? (
                    tree.nest.concat(Node(fst(tplIP)), [])
                ) : map(t => go(t, tplIP), tree.nest)
            );
        return foldl(
            go, Node(0, []),
            zip(enumFromToInt(1, pxs.length), pxs)
        );
    };

    // Siblings sorted by descendant count

    // depthSortedTree :: Tree a -> Tree Int
    const depthSortedTree = t => {
        const go = tree =>
            isNull(tree.nest) ? (
                Node(0, [])
            ) : (() => {
                const xs = map(go, tree.nest);
                return Node(
                    1 + foldl((a, x) => a + x.root, 0, xs),
                    sortBy(flip(comparing(x => x.root)), xs)
                );
            })();
        return go(t);
    };

    // Serialisation of the tree structure

    // commasFromTree :: Tree a -> String
    const commasFromTree = tree => {
        const go = t => `(${concat(map(go, t.nest))})`
        return go(tree);
    };


    // GENERIC FUNCTIONS --------------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v, // any type of value (but must be consistent across tree)
        nest: xs || []
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // composeList :: [(a -> a)] -> (a -> a)
    const composeList = fs =>
        x => fs.reduceRight((a, f) => f(a), x, fs);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

        // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => []
        .concat.apply(
            [],
            (Array.isArray(xs) ? (
                xs
            ) : xs.split('')).map(f)
        );

        // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>  [x].concat(xs);

    // Flexibly handles two or more arguments, applying
    // the function directly if the argument array is complete,
    // or recursing with a concatenation of any existing and
    // newly supplied arguments, if gaps remain.
    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (
            f.apply(null, xs)
        ) : function() {
            return go(xs.concat(Array.from(arguments)));
        };
        return go(args);
    };

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        n >= m ? (
            iterateUntil(x => x >= n, x => 1 + x, m)
        ) : [];

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // isNull :: [a] -> Bool
    // isNull :: String -> Bool
    const isNull = xs =>
        Array.isArray(xs) || typeof xs === 'string' ? (
            xs.length < 1
        ) : undefined;

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        let vs = [x],
            h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // liftA2List :: (a -> b -> c) -> [a] -> [b] -> [c]
    const liftA2List = (f, xs, ys) =>
        concatMap(x => concatMap(y => [f(x, y)], ys), xs);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // nub :: [a] -> [a]
    const nub = xs => nubBy((a, b) => a === b, xs);

    // nubBy :: (a -> a -> Bool) -> [a] -> [a]
    const nubBy = (p, xs) => {
        const go = xs => xs.length > 0 ? (() => {
            const x = xs[0];
            return [x].concat(
                go(xs.slice(1)
                    .filter(y => !p(x, y))
                )
            )
        })() : [];
        return go(xs);
    };

    // sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
    const sequenceA = tfa =>
        traverseList(x => x, tfa);

    // traverseList :: (Applicative f) => (a -> f b) -> [a] -> f [b]
    const traverseList = (f, xs) => {
        const lng = xs.length;
        return 0 < lng ? (() => {
            const
                vLast = f(xs[lng - 1]),
                t = vLast.type || 'List';
            return xs.slice(0, -1).reduceRight(
                (ys, x) => liftA2List(cons, f(x), ys),
                liftA2List(cons, vLast, [[]])
            );
        })() : [
            []
        ];
    };

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // zip :: [a] -> [b] -> [(a, b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => Tuple(x, ys[i]));

    // MAIN ---
    return main()
})();
```

{{Out}}

```txt
(()()()())
((())()())
((()())())
((())(()))
(((()))())
((()()()))
(((())()))
(((()())))
((((()))))
```



## Julia

{{trans|Python}}

```julia
bags(n,cache="") = n < 1 ? [(0, "")] : 
    [(c + 1, "(" * s * ")") for (c, s) in bagchain((0, ""), n - 1, 
        n < 2 ? [] : reduce(append!, [bags(x) for x in n-1:-1:1]))]
 
bagchain(x, n, bb, start=1) = n < 1 ? [x] :
    reduce(append!, [bagchain((x[1] + bb[i][1], x[2] * bb[i][2]), 
        n - bb[i][1], bb, i) for i in start:length(bb) if bb[i][1] <= n])

for bag in bags(5)
    println(bag[2])
end

```
{{out}}

```txt

((((()))))
(((()())))
(((())()))
((()()()))
(((()))())
((()())())
((())(()))
((())()())
(()()()())

```




## Kotlin

{{trans|C}}

```scala
// version 1.1.3

typealias Tree = Long

val treeList = mutableListOf<Tree>()
val offset = IntArray(32) { if (it == 1) 1 else 0 } 

fun append(t: Tree) {
    treeList.add(1L or (t shl 1))
}

fun show(t: Tree, l: Int) {
    var tt = t
    var ll = l
    while (ll-- > 0) {
        print(if (tt % 2L == 1L) "(" else ")")
        tt = tt ushr 1
    }
}

fun listTrees(n: Int) {
    for (i in offset[n] until offset[n + 1]) {
        show(treeList[i], n * 2)
        println()
    }
}

/* assemble tree from subtrees
	n:   length of tree we want to make
	t:   assembled parts so far
	sl:  length of subtree we are looking at
	pos: offset of subtree we are looking at
	rem: remaining length to be put together
*/

fun assemble(n: Int, t: Tree, sl: Int, pos: Int, rem: Int) {
    if (rem == 0) {
        append(t)
        return
    }

    var pp = pos
    var ss = sl

    if (sl > rem) { // need smaller subtrees
        ss = rem
        pp = offset[ss]
    }
    else if (pp >= offset[ss + 1]) {
        // used up sl-trees, try smaller ones
        ss--
        if(ss == 0) return
        pp = offset[ss]
    }

    assemble(n, (t shl (2 * ss)) or treeList[pp], ss, pp, rem - ss)
    assemble(n, t, ss, pp + 1, rem)
}

fun makeTrees(n: Int) {
    if (offset[n + 1] != 0) return
    if (n > 0) makeTrees(n - 1)
    assemble(n, 0, n - 1, offset[n - 1], n - 1)
    offset[n + 1] = treeList.size
}

fun main(args: Array<String>) {
    if (args.size != 1) {
        throw IllegalArgumentException("There must be exactly 1 command line argument")
    }
    val n = args[0].toIntOrNull()
    if (n == null) throw IllegalArgumentException("Argument is not a valid number")
    // n limited to 12 to avoid overflowing default stack 
    if (n !in 1..12) throw IllegalArgumentException("Argument must be between 1 and 12")

    // init 1-tree
    append(0)
    
    makeTrees(n)
    println("Number of $n-trees: ${offset[n + 1] - offset[n]}") 
    listTrees(n)
}
```


{{out}}

```txt

Number of 5-trees: 9
((((()))))
(((()())))
((()(())))
((()()()))
(()((())))
(()(()()))
((())(()))
(()()(()))
(()()()())

```



## Perl 6

Bags are represented by Perl 6 type [http://doc.perl6.org/type/Bag <code>Bag</code>].


```perl6
use v6;

multi expand-tree ( Bag $tree ) {
    bag(bag(bag()) (+) $tree) (+)
    [(+)] (
        $tree.keys ==> map {
            $^a.&expand-tree.map: * (+) ( $tree (-) bag($^a) )
        }
    );
}

multi expand-trees ( Bag $trees ) {
    [(+)] $trees.keys.map:  { $_.&expand-tree } ;
}      

my $n = 5;
for ( bag(), bag(bag()), *.&expand-trees ... * )[$n] {
    print ++$,".\t";
    .say
};

```

{{out}}

```txt

1.	bag(bag(), bag(bag()(2))) => 2
2.	bag(bag(bag()(3))) => 1
3.	bag(bag(bag(bag()), bag())) => 2
4.	bag(bag(bag(bag(bag())))) => 1
5.	bag(bag(bag())(2)) => 1
6.	bag(bag(bag(bag()(2)))) => 1
7.	bag(bag(), bag(bag(bag()))) => 2
8.	bag(bag(bag()), bag()(2)) => 2
9.	bag(bag()(4)) => 1

```

The bag <code>bag(bag(bag()), bag()(2))</code>  coresponds with <code>((())()())</code>. There are two independent ways how we can get it by nesting 4 bags.


## Phix

{{trans|Go}}

```Phix
atom t0 = time()
sequence list = {1},
         offset = repeat(0,32)
         offset[1..2] = 1

function show(integer t, l)
    string res = repeat('?',l)
    integer level = 0
    for i=l to 1 by -1 do
        integer r2 = remainder(t,2)
        res[i] = "[}(]{)"[mod(level-r2,6)+1]
        level += r2*4-2
        t = floor(t/2)
    end for
    if level!=0 then ?9/0 end if
    return res
end function
 
procedure listTrees(integer n)
    for i:=offset[n+1]+1 to offset[n+2] do
        printf(1,"%s\n",{show(list[i], n*2)})
    end for
end procedure
 
procedure assemble(atom t, integer n, sl, pos, rem)
--
-- assemble tree from subtrees
--  t:   assembled parts so far
--  n:   length of tree we want to make
--  sl:  length of subtree we are looking at
--  pos: offset of subtree we are looking at
--  rem: remaining length to be put together
--
    if rem == 0 then
        list = append(list, t*2+1)
    else
        if sl>rem then -- need smaller sub-trees
            sl = rem
            pos = offset[sl+1]
        elsif pos>=offset[sl+2] then
            -- used up sl-trees, try smaller ones
            if sl == 1 then return end if
            pos = offset[sl]
            sl -= 1
        end if
 
        atom u = or_bits(t*power(2,2*sl),list[pos+1])
        assemble(u, n, sl, pos, rem-sl)
        assemble(t, n, sl, pos+1, rem)
    end if
end procedure
 
procedure mktrees(integer n)
    if offset[n+2]=0 then
        if n>0 then
            mktrees(n - 1)
        end if
        assemble(0, n, n-1, offset[n], n-1)
        offset[n+2] = length(list)
    end if
end procedure
 
procedure main(integer n)
    mktrees(n)
    atom nt = offset[n+2]-offset[n+1],
         td = time()-t0
    string e = iff(td>0.1?" ("&elapsed(td)&")":"")
    printf(1,"Number of %d-trees: %,d%s\n", {n, nt, e})
    if n<=5 then listTrees(n) end if
end procedure
for i=0 to 20 do
    main(i)
end for
```

{{out}}

```txt

Number of 0-trees: 0
Number of 1-trees: 1
()
Number of 2-trees: 1
({})
Number of 3-trees: 2
({[]})
({}{})
Number of 4-trees: 4
({[()]})
({[][]})
({[]}{})
({}{}{})
Number of 5-trees: 9
({[({})]})
({[()()]})
({[()][]})
({[][][]})
({[()]}{})
({[][]}{})
({[]}{[]})
({[]}{}{})
({}{}{}{})
Number of 6-trees: 20
Number of 7-trees: 48
Number of 8-trees: 115
Number of 9-trees: 286
Number of 10-trees: 719
Number of 11-trees: 1,842
Number of 12-trees: 4,766
Number of 13-trees: 12,486
Number of 14-trees: 32,973
Number of 15-trees: 87,811
Number of 16-trees: 235,381 (0.2s)
Number of 17-trees: 634,847 (0.5s)
Number of 18-trees: 1,721,159 (1.3s)
Number of 19-trees: 4,688,676 (4.0s)
Number of 20-trees: 12,826,228 (13.6s)

```

Beyond that it gets extremely slow.


## Python


```python
def bags(n,cache={}):
	if not n: return [(0, "")]

	upto = sum([bags(x) for x in range(n-1, 0, -1)], [])
	return [(c+1, '('+s+')') for c,s in bagchain((0, ""), n-1, upto)]

def bagchain(x, n, bb, start=0):
	if not n: return [x]

	out = []
	for i in range(start, len(bb)):
		c,s = bb[i]
		if c <= n: out += bagchain((x[0] + c, x[1] + s), n-c, bb, i)
	return out

# Maybe this lessens eye strain. Maybe not.
def replace_brackets(s):
	depth,out = 0,[]
	for c in s:
		if c == '(':
			out.append("([{"[depth%3])
			depth += 1
		else:
			depth -= 1
			out.append(")]}"[depth%3])
	return "".join(out)

for x in bags(5): print(replace_brackets(x[1]))
```

{{out}}

```txt

([{([])}])
([{()()}])
([{()}{}])
([{}{}{}])
([{()}][])
([{}{}][])
([{}][{}])
([{}][][])
([][][][])

```


Another method by incrementing subtrees:

```python
treeid = {(): 0}

'''
Successor of a tree.  The predecessor p of a tree t is:

  1. if the smallest subtree of t is a single node, then p is t minus that node
  2. otherwise, p is t with its smalles subtree "m" replaced by m's predecessor

Here "smaller" means the tree is generated earlier, as recorded by treeid. Obviously,
predecessor to a tree is unique.  Since every degree n tree has a
unique degree (n-1) predecessor, inverting the process leads to the successors
to tree t:

  1. append a single node tree to t's root, or
  2. replace t's smallest subtree by its successors

We need to keep the trees so generated canonical, so when replacing a subtree,
the replacement must not be larger than the next smallest subtree.

Note that trees can be compared by other means, as long as trees with fewer nodes
are considered smaller, and trees with the same number of nodes have a fixed order.
'''
def succ(x):
    yield(((),) + x)
    if not x: return

    if len(x) == 1:
        for i in succ(x[0]): yield((i,))
        return

    head,rest = x[0],tuple(x[1:])
    top = treeid[rest[0]]

    for i in [i for i in succ(head) if treeid[i] <= top]:
        yield((i,) + rest)

def trees(n):
    if n == 1:
        yield()
        return

    global treeid
    for x in trees(n-1):
        for a in succ(x):
            if not a in treeid: treeid[a] = len(treeid)
            yield(a)

def tostr(x): return "(" + "".join(map(tostr, x)) + ")"

for x in trees(5): print(tostr(x))
```



## Racket


```racket
#lang racket
(require racket/splicing data/order)

(define (filtered-cartesian-product #:f (fltr (λ (cand left) #t)) l . more-ls)
  (let inr ((lls (cons l more-ls)) (left null))
    (match lls
      [(list) '(())]
      [(cons lla lld)
       (for*/list ((a (in-list (filter (curryr fltr left) lla)))
                   (d (in-list (inr lld (cons a left)))))
         (cons a d))])))

;; The "order" of an LRT
(define LRT-order (match-lambda [(list (app LRT-order o) ...) (apply + 1 o)]))

;; Some order for List Rooted Trees
(define LRT<=
  (match-lambda**
   [(_ (list)) #t]
   [((and bar (app LRT-order baro)) (cons (and badr (app LRT-order badro)) bddr))
    (and (or (< baro badro) (not (eq? '> (datum-order bar badr)))) (LRT<= badr bddr))]))

(splicing-letrec ((t# (make-hash '((1 . (())))))
                  (p# (make-hash '((0 . (()))))))
  ;; positive-integer -> (listof (listof positive-integer))
  (define (partitions N)
    (hash-ref! p# N
               (λ () (for*/list ((m (in-range 1 (add1 N)))
                                 (p (partitions (- N m)))
                                 #:when (or (null? p) (>= m (car p))))
                       (cons m p)))))
  
  ;; positive-integer -> (listof trees)
  (define (LRTs N)
    (hash-ref! t# N
               (λ ()
                 ;; sub1 because we will use the N'th bag to wrap the lot!
                 (define ps (partitions (sub1 N)))
                 (append*
                  (for/list ((p ps))
                    (apply filtered-cartesian-product (map LRTs p) #:f LRT<=)))))))

(module+ main
  (for-each displayln (LRTs 5))
  (equal? (map (compose length LRTs) (range 1 (add1 13)))
          '(1 1 2 4 9 20 48 115 286 719 1842 4766 12486))) ;; https://oeis.org/A000081
```


{{out}}

```txt
(() () () ())
((()) () ())
((()) (()))
((() ()) ())
(((())) ())
((() () ()))
(((()) ()))
(((() ())))
((((()))))
#t
```



## REXX

This REXX version uses (internally) a binary string to represent nodes on a tree   (<big>'''0'''</big>   is a left parenthesis,   <big>'''1'''</big>   is a right parenthesis).   A   <big>'''()'''</big>   is translated to a   <big>'''O'''</big>.

```rexx
/*REXX program lists  n─node  rooted trees  (by enumerating all ways of nesting N bato).*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=5                      /*Not specified?  Then use the default.*/
if N>5  then do;  say N  "isn't supported for this program at this time.";   exit 13;  end
nn= N + N - 1                                    /*power of 2 that is used for dec start*/
numeric digits 200                               /*ensure enough digs for the next calc.*/
numeric digits max(9, 1 + length( x2b( d2x(2**(nn+1) - 1) ) ) )  /*limit decimal digits.*/
start=2**nn    +    (2**nn) % 2                  /*calculate the starting decimal number*/
if N==1  then start= 2**1                        /*treat the start for unity as special.*/
_= copies('─', 20)"► "                           /*demonstrative literal for solutions. */
#=0                                              /*count of ways to nest bags (so far). */
$=                                               /*string holds possible duplicious strs*/
     do j=start + start//2  to 2**(nn+1)-1  by 2 /*limit the search, smart start and end*/
     t= x2b( d2x(j) )   +   0                    /*convert dec number to a binary string*/
     z= length( space( translate(t, , 0), 0) )   /*count the number of zeros in bit str.*/
     if z\==n  then iterate                      /*Not enough zeroes?  Then skip this T.*/
     if N>1  then if left(t,N)==right(t,N)  then iterate       /*left side ≡ right side?*/
     if N>2  then if right(t,2)==    10  then iterate  /*has a right-most isolated bag ?*/
     if N>3  then if right(t,4)==  1100  then iterate  /* "  "      "         "     "  ?*/
     if N>4  then if right(t,6)==111000  then iterate  /* "  "      "         "     "  ?*/
     if N>4  then if right(t,6)==110100  then iterate  /* "  "      "         "     "  ?*/
     if N>4  then if right(t,6)==100100  then iterate  /* "  "      "         "     "  ?*/
     if wordpos(t, $)\==0  then iterate                        /*duplicate bag stuffing?*/
     say _  changestr('()', translate(t, "()", 10),  'O')      /*show a compact display.*/
     #= # + 1                                    /*bump count of ways of nesting bags.  */
     $=$  translate( reverse(t), 01, 10)         /*save a (possible) duplicious string. */
     end   /*j*/
say                                              /*separate number─of─ways with a blank.*/
say # ' is the number of ways to nest' n "bags." /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

────────────────────►  (OOOO)
────────────────────►  (OO(O))
────────────────────►  (O(OO))
────────────────────►  (O((O)))
────────────────────►  ((O)(O))
────────────────────►  ((OOO))
────────────────────►  ((O(O)))
────────────────────►  (((OO)))
────────────────────►  ((((O))))

9  is the number of ways to nest 5 bags.

```



## Ring


```ring

# Project : List rooted trees

list = "()"
addstr = []
flag = 0
newstr = []
str = []
np = [1,2,3,4]
for nr = 1 to len(np)
      if nr = 1
         bg1 = "bag"
       else 
         bg1 = "bags"
      ok
      see "for " + nr + " " + bg1 + " :" + nl
     permutation(list,nr*2)
     listroot(nr*2)
next
see "ok" + nl

func listroot(pn)
        for n = 1 to len(addstr)
             result(addstr[n],pn)
             if flag = 1
                see "" + addstr[n] + nl
                addstr[n]
             ok
        next
 
func result(list,pn)
        flag = 0
        newstr = list
        while substr(newstr, "()") != 0
                 if list = "()" or list = "(())"
                    flag = 1
                    exit
                 ok
                 num = substr(newstr, "()")
                 newstr = substr(newstr, "()", "")
                 if left(list,2) = "()" or right(list,2) = "()" or left(list,4) = "(())" or right(list,4) = "(())"
                    flag = 0
                    exit
                 else 
                    if len(list) != 2 and len(list) != 4 and newstr = ""
                       flag = 1
                       exit
                    ok
                 ok
        end
        
func permutation(list,pn)
       addstr = []
       while true
               str = ""
               for n = 1 to pn
                    rnd = random(1) + 1
                    str = str + list[rnd]
               next
               add(addstr,str)
               for m = 1 to len(addstr)
                    for p = m + 1 to len(addstr) - 1
                         if addstr[m] = addstr[p]
                            del(addstr,p)
                         ok
                    next
               next
               if len(addstr) = pow(2,pn)
                  exit
               ok
       end

```

Output:

```txt

for 1 bag:
()
for 2 bags:
(())
for 3 bags:
((()))
(()())
for 4 bags:
(()()())
((())())
((()()))
(((())))

```



## Sidef

{{trans|Python}}

```ruby
func bagchain(x, n, bb, start=0) {
    n || return [x]

    var out = []
    for i in (start .. bb.end) {
        var (c, s) = bb[i]...
        if (c <= n) {
            out += bagchain([x[0] + c, x[1] + s], n-c, bb, i)
        }
    }

    return out
}

func bags(n) {
    n || return [[0, ""]]
    var upto = []
    for i in (n ^.. 1) { upto += bags(i) }
    bagchain([0, ""], n-1, upto).map{|p| [p[0]+1, '('+p[1]+')'] }
}

func replace_brackets(s) {
    var (depth, out) = (0, [])
    for c in s {
        if (c == '(') {
            out.append(<( [ {>[depth%3])
            ++depth
        }
        else {
            --depth
            out.append(<) ] }>[depth%3])
        }
    }
    return out.join
}

for x in (bags(5)) {
    say replace_brackets(x[1])
}
```

{{out}}

```txt

([{([])}])
([{()()}])
([{()}{}])
([{}{}{}])
([{()}][])
([{}{}][])
([{}][{}])
([{}][][])
([][][][])

```



## zkl

Note that "( () (()) () )" the same as "( (()) () () )"
{{trans|Python}}

```zkl
fcn bags(n){
   if(not n) return(T(T(0,"")));

   [n-1 .. 1, -1].pump(List,bags).flatten() :
   bagchain(T(0,""), n-1, _).apply(fcn([(c,s)]){ T(c+1,String("(",s,")")) })
} 
fcn bagchain(x,n,bb,start=0){
   if(not n) return(T(x));
 
   out := List();
   foreach i in ([start..bb.len()-1]){
      c,s := bb[i];
      if(c<=n) out.extend(bagchain(L(x[0]+c, x[1]+s), n-c, bb, i));
   }
   out
}
# Maybe this lessens eye strain. Maybe not.
fcn replace_brackets(s){
   depth,out := 0,Sink(String);
   foreach c in (s){
      if(c=="("){
	 out.write("([{"[depth%3]);
	 depth += 1;
      }else{
	 depth -= 1;
	 out.write(")]}"[depth%3]);
      }
   }
   out.close()
} 
foreach x in (bags(5)){ println(replace_brackets(x[1])) }
println("or");
b:=bags(5); b.apply("get",1).println(b.len());
```

{{out}}

```txt

([{([])}])
([{()()}])
([{()}{}])
([{}{}{}])
([{()}][])
([{}{}][])
([{}][{}])
([{}][][])
([][][][])
or
L("((((()))))","(((()())))","(((())()))","((()()()))","(((()))())","((()())())","((())(()))","((())()())","(()()()())")9

```

