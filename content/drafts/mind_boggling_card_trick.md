+++
title = "Mind boggling card trick"
description = ""
date = 2019-06-27T21:52:42Z
aliases = []
[extra]
id = 21977
[taxonomies]
categories = []
tags = []
+++

[[Category:Puzzles]]

{{task}}

Matt Parker of the "Stand Up Maths channel" has a   [https://www.youtube.com/watch?v=aNpGxZ_1KXU <u>YouTube video</u>]   of a card trick that creates a semblance of order from chaos.

The task is to simulate the trick in a way that mimics the steps shown in the video.

; 1. Cards.
# Create a common deck of cards of 52 cards    (which are half red, half black).
# Give the pack a good shuffle.
; 2. Deal from the shuffled deck, you'll be creating three piles.
#   Assemble the cards face down.
##   Turn up the   ''top card''   and hold it in your hand. 
###    if the card is         black,        then add the   ''next''   card (unseen) to the       "black"      pile. 
###    If the card is     red,    then add the   ''next''   card (unseen) to the   "red"  pile.
##   Add the   ''top card''   that you're holding to the '''discard''' pile.   (You might optionally show these discarded cards to get an idea of the randomness).
# Repeat the above for the rest of the shuffled deck.
; 3. Choose a random number   (call it '''X''')   that will be used to swap cards from the "red" and "black" piles.
# Randomly choose   '''X'''   cards from the   "red"  pile (unseen), let's call this the   "red"  bunch. 
# Randomly choose   '''X'''   cards from the       "black"      pile (unseen), let's call this the      "black"       bunch.
# Put the     "red"    bunch into the         "black"      pile.
# Put the        "black"        bunch into the      "red"  pile.
# (The above two steps complete the swap of   '''X'''   cards of the "red" and "black" piles. 
 (Without knowing what those cards are --- they could be red or black, nobody knows).
; 4. Order from randomness?
# Verify (or not) the mathematician's assertion that: 
     <big> '''The number of black cards in the "black" pile equals the number of red cards in the "red" pile.''' </big>


(Optionally, run this simulation a number of times, gathering more evidence of the truthfulness of the assertion.)

Show output on this page.




=={{header|F_Sharp|F#}}==

```fsharp

//Be boggled? Nigel Galloway: September 19th., 2018
let N=System.Random()
let fN=List.unfold(function |(0,0)->None |(n,g)->let ng=N.Next (n+g) in Some (if ng>=n then ("Black",(n,g-1)) else ("Red",(n-1,g))))(26,26)
let fG n=let (n,n')::(g,g')::_=List.countBy(fun (n::g::_)->if n=g then n else g) n in sprintf "%d %s cards and %d %s cards" n' n g' g
printf "A well shuffled deck -> "; List.iter (printf "%s ") fN; printfn ""
fN |> List.chunkBySize 2 |> List.groupBy List.head |> List.iter(fun(n,n')->printfn "The %s pile contains %s" n (fG n'))

```

{{out}}

```txt

A well shuffled deck -> Black Black Black Black Red Red Black Red Black Black Red Red Red Red Red Black Red Black Black Red Red Black Black Red Black Red Red Red Black Black Red Red Black Red Red Black Red Black Black Black Black Red Red Black Black Red Black Black Red Red Black Red 
The Black pile contains 6 Black cards and 8 Red cards
The Red pile contains 6 Red cards and 6 Black cards

```



## Factor


```factor
USING: accessors combinators.extras formatting fry
generalizations io kernel math math.ranges random sequences
sequences.extras ;
IN: rosetta-code.mind-boggling-card-trick

SYMBOLS: R B ;

TUPLE: piles deck red black discard ;

: initialize-deck ( -- seq )
    [ R ] [ B ] [ '[ 26 _ V{ } replicate-as ] call ] bi@ append
    randomize ;

: <piles> ( -- piles )
    initialize-deck [ V{ } clone ] thrice piles boa ;
    
: deal-step ( piles -- piles' )
    dup [ deck>> pop dup ] [ discard>> push ] [ deck>> pop ] tri
    B = [ over black>> ] [ over red>> ] if push ;
    
: deal ( piles -- piles' ) 26 [ deal-step ] times ;

: choose-sample-size ( piles -- n )
    [ red>> ] [ black>> ] bi shorter length [0,b] random ;

! Partition a sequence into n random samples in one sequence and
! the leftovers in another.
: sample-partition ( vec n -- leftovers sample )
    [ 3 dupn ] dip sample dup
    [ [ swap remove-first! drop ] with each ] dip ;
    
: perform-swaps ( piles -- piles' )
    dup dup choose-sample-size dup "Swapping %d\n" printf
    [ [ red>> ] dip ] [ [ black>> ] dip ] 2bi
    [ sample-partition ] 2bi@ [ append ] dip rot append
    [ >>black ] dip >>red ;
    
: test-assertion ( piles -- )
    [ red>> ] [ black>> ] bi
    [ [ R = ] count ] [ [ B = ] count ] bi* 2dup =
    [ "Assertion correct!" ]
    [ "Assertion incorrect!" ] if print
    "R in red: %d\nB in black: %d\n" printf ;
    
: main ( -- )
    <piles>                             ! step 1
    deal                                ! step 2
    dup "Post-deal state:\n%u\n" printf
    perform-swaps                       ! step 3
    dup "Post-swap state:\n%u\n" printf
    test-assertion ;                    ! step 4

MAIN: main
```

A run:
{{out}}

```txt

Post-deal state:
T{ piles
    { deck V{ } }
    { red V{ B R B R R B B R R R R } }
    { black V{ R B R B B R B B R R R R R B B } }
    { discard
        V{ R B R B R B B B R R R B B B R B R R R R R R B R B R }
    }
}
Swapping 11
Post-swap state:
T{ piles
    { deck V{ } }
    { red V{ B R B B R R B R R R R } }
    { black V{ B R R B B R B R R R R B R B B } }
    { discard
        V{ R B R B R B B B R R R B B B R B R R R R R R B R B R }
    }
}
Assertion correct!
R in red: 7
B in black: 7

```

Another run:
{{out}}

```txt

Post-deal state:
T{ piles
    { deck V{ } }
    { red V{ R R R B B R B R R B } }
    { black V{ B R B B R B R B R R R R R R B R } }
    { discard
        V{ R R B R B B R B B R B R R B B R R R R R R R B R R B }
    }
}
Swapping 7
Post-swap state:
T{ piles
    { deck V{ } }
    { red V{ B R R R B R B B R B } }
    { black V{ R R B R R B R B B R R R R R B R } }
    { discard
        V{ R R B R B B R B B R B R R B B R R R R R R R B R R B }
    }
}
Assertion correct!
R in red: 5
B in black: 5

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Create pack, half red, half black and shuffle it.
    var pack [52]byte
    for i := 0; i < 26; i++ {
        pack[i] = 'R'
        pack[26+i] = 'B'
    }
    rand.Seed(time.Now().UnixNano())
    rand.Shuffle(52, func(i, j int) {
        pack[i], pack[j] = pack[j], pack[i]
    })

    // Deal from pack into 3 stacks.
    var red, black, discard []byte
    for i := 0; i < 51; i += 2 {
        switch pack[i] {
        case 'B':
            black = append(black, pack[i+1])
        case 'R':
            red = append(red, pack[i+1])
        }
        discard = append(discard, pack[i])
    }
    lr, lb, ld := len(red), len(black), len(discard)
    fmt.Println("After dealing the cards the state of the stacks is:")
    fmt.Printf("  Red    : %2d cards -> %c\n", lr, red)
    fmt.Printf("  Black  : %2d cards -> %c\n", lb, black)
    fmt.Printf("  Discard: %2d cards -> %c\n", ld, discard)

    // Swap the same, random, number of cards between the red and black stacks.
    min := lr
    if lb < min {
        min = lb
    }
    n := 1 + rand.Intn(min)
    rp := rand.Perm(lr)[:n]
    bp := rand.Perm(lb)[:n]
    fmt.Printf("\n%d card(s) are to be swapped.\n\n", n)
    fmt.Println("The respective zero-based indices of the cards(s) to be swapped are:")
    fmt.Printf("  Red    : %2d\n", rp)
    fmt.Printf("  Black  : %2d\n", bp)
    for i := 0; i < n; i++ {
        red[rp[i]], black[bp[i]] = black[bp[i]], red[rp[i]]
    }
    fmt.Println("\nAfter swapping, the state of the red and black stacks is:")
    fmt.Printf("  Red    : %c\n", red)
    fmt.Printf("  Black  : %c\n", black)

    // Check that the number of black cards in the black stack equals
    // the number of red cards in the red stack.
    rcount, bcount := 0, 0
    for _, c := range red {
        if c == 'R' {
            rcount++
        }
    }
    for _, c := range black {
        if c == 'B' {
            bcount++
        }
    }

    fmt.Println("\nThe number of red cards in the red stack     =", rcount)
    fmt.Println("The number of black cards in the black stack =", bcount)
    if rcount == bcount {
        fmt.Println("So the asssertion is correct!")
    } else {
        fmt.Println("So the asssertion is incorrect!")
    }
}
```


{{out}}
First sample run:

```txt

After dealing the cards the state of the stacks is:
  Red    : 15 cards -> [B R R R R B R B R B B R B B B]
  Black  : 11 cards -> [R B R B B R R B B B B]
  Discard: 26 cards -> [R R R R B B B B B B B R R R B B B R R R R R R B R R]

8 card(s) are to be swapped.

The respective zero-based indices of the cards(s) to be swapped are:
  Red    : [10  2 11 14 12  3  9  5]
  Black  : [ 7 10  3  0  5  8  6  1]

After swapping, the state of the red and black stacks is:
  Red    : [B R B B R B R B R R B B R B R]
  Black  : [B B R R B B B B R B R]

The number of red cards in the red stack     = 7
The number of black cards in the black stack = 7
So the asssertion is correct!

```


Second sample run:

```txt

After dealing the cards the state of the stacks is:
  Red    : 12 cards -> [B B R B R R R B B B R B]
  Black  : 14 cards -> [B R R R B R R B B R R B R R]
  Discard: 26 cards -> [R R B B B R R B B B R R R B B B R B B B R B R R R B]

2 card(s) are to be swapped.

The respective zero-based indices of the cards(s) to be swapped are:
  Red    : [ 1  6]
  Black  : [11 12]

After swapping, the state of the red and black stacks is:
  Red    : [B B R B R R R B B B R B]
  Black  : [B R R R B R R B B R R B R R]

The number of red cards in the red stack     = 5
The number of black cards in the black stack = 5
So the asssertion is correct!

```



## Haskell


```haskell
import System.Random (randomRIO)
import Data.List (partition)
import Data.Monoid ((<>))
 
main :: IO [Int]
main = do
 
  -- DEALT
  ns <- knuthShuffle [1 .. 52]
  let (rs_, bs_, discards) = threeStacks (rb <$> ns)
 
  -- SWAPPED
  nSwap <- randomRIO (1, min (length rs_) (length bs_))
  let (rs, bs) = exchange nSwap rs_ bs_
 
  -- CHECKED
  let rrs = filter ('R' ==) rs
  let bbs = filter ('B' ==) bs
  putStrLn $
    unlines
      [ "Discarded: " <> discards
      , "Swapped: " <> show nSwap
      , "Red pile: " <> rs
      , "Black pile: " <> bs
      , rrs <> " = Red cards in the red pile"
      , bbs <> " = Black cards in the black pile"
      , show $ length rrs == length bbs
      ]
  return ns
 
-- RED vs BLACK ----------------------------------------
rb :: Int -> Char
rb n
  | even n = 'R'
  | otherwise = 'B'
 
-- THREE STACKS ----------------------------------------
threeStacks :: String -> (String, String, String)
threeStacks = go ([], [], [])
  where
    go tpl [] = tpl
    go (rs, bs, ds) [x] = (rs, bs, x : ds)
    go (rs, bs, ds) (x:y:xs)
      | 'R' == x = go (y : rs, bs, x : ds) xs
      | otherwise = go (rs, y : bs, x : ds) xs
 
exchange :: Int -> [a] -> [a] -> ([a], [a])
exchange n xs ys =
  let [xs_, ys_] = splitAt n <$> [xs, ys]
  in (fst ys_ <> snd xs_, fst xs_ <> snd ys_)
 
-- SHUFFLE -----------------------------------------------
-- (See Knuth Shuffle task)
knuthShuffle :: [a] -> IO [a]
knuthShuffle xs = (foldr swapElems xs . zip [1 ..]) <$> randoms (length xs)
 
randoms :: Int -> IO [Int]
randoms x = traverse (randomRIO . (,) 0) [1 .. (pred x)]
 
swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
  | i == j = xs
  | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs
 
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
  let (a, b) = splitAt i l
  in a ++ c : drop 1 b
```

{{Out}}

```txt
Discarded: RRRRRBBBBBRBBBRBBRBRRBRRRB
Swapped: 7
Red pile: BBBRRBRBRBBRR
Black pile: BRRRRRBBRBBRB
RRRRRR = Red cards in the red pile
BBBBBB = Black cards in the black pile
True
```



## J

The trick verb returns 0 if the magician's proposition fails, otherwise 1.

```J

  NB. A failed assertion looks like this
   assert 0
|assertion failure: assert
|       assert 0

```
   
We find that in three runs the trick works.

```J

shuffle =: {~ ?~@:#
f =: (,&.> {:)~
g =: ({.@],(f {:))`((f {.),{:@])@.({.@[)

trick =: 3 :0
 DECK =: shuffle 2 | i. 52
 'B R' =: shuffle L:_1 > g&.>/(_2 <\ DECK) , < 2 # a:
 NB. although I swap the first N cards of each pile,
 NB. the piles were shuffled following placement
 N =: ? R <.&# B
 echo 'prior to swap of ',(":N),' cards'
 echo 'black pile ',B{'rB'
 echo 'red pile   ',R{'rB'
 BR =: N {. B
 RB =: N {. R
 B =: RB , N }. B
 R =: BR , N }. R
 echo 'after swap'
 echo 'black pile ',B{'rB'
 echo 'red pile   ',R{'rB'
 B (-:&(+/) -.) R
)


   assert trick''
prior to swap of 10 cards
black pile rBrrrrrBBrrrBrr
red pile   BBrBrBBrBBr
after swap
black pile BBrBrBBrBBrrBrr
red pile   rBrrrrrBBrr

   assert trick''
prior to swap of 6 cards
black pile rBBBBrBBr
red pile   rrBBBBrBrBBBBrrBB
after swap
black pile rrBBBBBBr
red pile   rBBBBrrBrBBBBrrBB

   assert trick''
prior to swap of 3 cards
black pile rBrBrrrBBrr
red pile   BrrBrBBrBBBBBBB
after swap
black pile BrrBrrrBBrr
red pile   rBrBrBBrBBBBBBB

```



## Javascript

{{trans|Haskell}}

```javascript
(() => {
    'use strict';

    const main = () => {
        const
        // DEALT
        [rs_, bs_, discards] = threeStacks(
                map(n =>
                    even(n) ? (
                        'R'
                    ) : 'B', knuthShuffle(
                        enumFromTo(1, 52)
                    )
                )
            ),

            // SWAPPED
            nSwap = randomRInt(1, min(rs_.length, bs_.length)),
            [rs, bs] = exchange(nSwap, rs_, bs_),

            // CHECKED
            rrs = filter(c => 'R' === c, rs).join(''),
            bbs = filter(c => 'B' === c, bs).join('');
        return unlines([
            'Discarded: ' + discards.join(''),
            'Swapped: ' + nSwap,
            'Red pile: ' + rs.join(''),
            'Black pile: ' + bs.join(''),
            rrs + ' = Red cards in the red pile',
            bbs + ' = Black cards in the black pile',
            (rrs.length === bbs.length).toString()
        ]);
    };

    // THREE STACKS ---------------------------------------

    // threeStacks :: [Chars] -> ([Chars], [Chars], [Chars])
    const threeStacks = cards => {
        const go = ([rs, bs, ds]) => xs => {
            const lng = xs.length;
            return 0 < lng ? (
                1 < lng ? (() => {
                    const [x, y] = take(2, xs),
                        ds_ = cons(x, ds);
                    return (
                        'R' === x ? (
                            go([cons(y, rs), bs, ds_])
                        ) : go([rs, cons(y, bs), ds_])
                    )(drop(2, xs));
                })() : [rs, bs, ds_]
            ) : [rs, bs, ds];
        };
        return go([
            [],
            [],
            []
        ])(cards);
    };

    // exchange :: Int -> [a] -> [a] -> ([a], [a])
    const exchange = (n, xs, ys) => {
        const [xs_, ys_] = map(splitAt(n), [xs, ys]);
        return [
            fst(ys_).concat(snd(xs_)),
            fst(xs_).concat(snd(ys_))
        ];
    };

    // SHUFFLE --------------------------------------------

    // knuthShuffle :: [a] -> [a]
    const knuthShuffle = xs =>
        enumFromTo(0, xs.length - 1)
        .reduceRight((a, i) => {
            const iRand = randomRInt(0, i);
            return i !== iRand ? (
                swapped(i, iRand, a)
            ) : a;
        }, xs);

    // swapped :: Int -> Int -> [a] -> [a]
    const swapped = (iFrom, iTo, xs) =>
        xs.map(
            (x, i) => iFrom !== i ? (
                iTo !== i ? (
                    x
                ) : xs[iFrom]
            ) : xs[iTo]
        );

    // GENERIC FUNCTIONS ----------------------------------

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : (x + xs);

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) => xs.slice(n);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // even :: Int -> Bool
    const even = n => 0 === n % 2;

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // randomRInt :: Int -> Int -> Int
    const randomRInt = (low, high) =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // splitAt :: Int -> [a] -> ([a],[a])
    const splitAt = n => xs => Tuple(xs.slice(0, n), xs.slice(n));

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Discarded: BRBRBRRRRBBBRBBBRBBBBBRRBR
Swapped: 7
Red pile: RBRRRRRRBRB
Black pile: BBBRRBRBBBRRBRR
RRRRRRRR = Red cards in the red pile
BBBBBBBB = Black cards in the black pile
true
```



## Julia

{{trans|Perl 6}}

```julia
const rbdeck = split(repeat('R', 26) * repeat('B', 26), "")
shuffledeck() = shuffle(rbdeck)

function deal!(deck, dpile, bpile, rpile)
    while !isempty(deck)
        if (topcard = pop!(deck)) == "R"
            push!(rpile, pop!(deck))
        else
            push!(bpile, pop!(deck))
        end
        push!(dpile, topcard)
    end
end

function swap!(rpile, bpile, nswapping)
    rpick = sort(randperm(length(rpile))[1:nswapping])
    bpick = sort(randperm(length(bpile))[1:nswapping])
    rrm = rpile[rpick]; brm = bpile[bpick]
    deleteat!(rpile, rpick); deleteat!(bpile, bpick)
    append!(rpile, brm); append!(bpile, rrm)
end

function mindbogglingcardtrick(verbose=true)
    prif(cond, txt) = (if(cond) println(txt) end)
    deck = shuffledeck()
    prif(verbose, "Shuffled deck is: $deck")

    dpile, rpile, bpile = [], [], []
    deal!(deck, dpile, bpile, rpile)

    prif(verbose, "Before swap:")
    prif(verbose, "Discard pile:    $dpile")
    prif(verbose, "Red card pile:   $rpile")
    prif(verbose, "Black card pile: $bpile")
 
    amount = rand(1:min(length(rpile), length(bpile)))
    prif(verbose, "Swapping a random number of cards: $amount will be swapped.")
    swap!(rpile, bpile, amount)
 
    prif(verbose, "Red pile after swaps:   $rpile")
    prif(verbose, "Black pile after swaps: $bpile")
    println("There are $(sum(map(x->x=="B", bpile))) black cards in the black card pile:")
    println("there are $(sum(map(x->x=="R", rpile))) red cards in the red card pile.")
    prif(verbose, "")
end

mindbogglingcardtrick()

for _ in 1:10 
    mindbogglingcardtrick(false)
end

```
{{output}}
```txt

 Shuffled deck is: SubString{String}["B", "B", "R", "B", "B", "R", "B", "R", "B", "R", "B", "B", "B", "R", "B", "R", "R", "B", "R", "R", 
 "R", "R", "R", "R", "B", "R", "B", "B", "B", "R", "R", "B", "B", "R", "B", "B", "R", "R", "B", "R", "R", "R", "R", "B", "B", "R", "B", 
 "B", "B", "B", "R", "R"]
 Before swap:
 Discard pile:    Any["R", "B", "B", "R", "B", "R", "R", "R", "B", "R", "B", "R", "B", "R", "R", "R", "R", "B", "R", "R", "B", "R", "R", 
 "R", "B", "B"]
 Red card pile:   Any["R", "B", "R", "B", "R", "B", "B", "B", "R", "R", "R", "B", "B", "B", "B", "B"]
 Black card pile: Any["B", "B", "R", "B", "R", "B", "R", "B", "R", "B"]
 Swapping a random number of cards: 8 will be swapped.
 Red pile after swaps:   Any["R", "B", "B", "B", "R", "B", "B", "B", "R", "B", "R", "B", "R", "B", "R", "B"]
 Black pile after swaps: Any["B", "B", "R", "R", "B", "B", "R", "R", "B", "B"]
 There are 6 black cards in the black card pile:
 there are 6 red cards in the red card pile.

 There are 5 black cards in the black card pile:
 there are 5 red cards in the red card pile.
 There are 6 black cards in the black card pile:
 there are 6 red cards in the red card pile.
 There are 7 black cards in the black card pile:
 there are 7 red cards in the red card pile.
 There are 8 black cards in the black card pile:
 there are 8 red cards in the red card pile.
 There are 7 black cards in the black card pile:
 there are 7 red cards in the red card pile.
 There are 5 black cards in the black card pile:
 there are 5 red cards in the red card pile.
 There are 6 black cards in the black card pile:
 there are 6 red cards in the red card pile.
 There are 8 black cards in the black card pile:
 there are 8 red cards in the red card pile.
 There are 6 black cards in the black card pile:
 there are 6 red cards in the red card pile.
 There are 8 black cards in the black card pile:
 there are 8 red cards in the red card pile.

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.61

import java.util.Random

fun main(args: Array<String>) {
    // Create pack, half red, half black and shuffle it.
    val pack = MutableList(52) { if (it < 26) 'R' else 'B' }
    pack.shuffle()

    // Deal from pack into 3 stacks.
    val red = mutableListOf<Char>()
    val black = mutableListOf<Char>()
    val discard = mutableListOf<Char>()
    for (i in 0 until 52 step 2) {
        when (pack[i]) {
            'B' -> black.add(pack[i + 1])
            'R' -> red.add(pack[i + 1])
        }
        discard.add(pack[i])
    }
    val sr = red.size
    val sb = black.size
    val sd = discard.size
    println("After dealing the cards the state of the stacks is:")
    System.out.printf("  Red    : %2d cards -> %s\n", sr, red)
    System.out.printf("  Black  : %2d cards -> %s\n", sb, black)
    System.out.printf("  Discard: %2d cards -> %s\n", sd, discard)

    // Swap the same, random, number of cards between the red and black stacks.
    val rand = Random()
    val min = minOf(sr, sb)
    val n = 1 + rand.nextInt(min)
    var rp = MutableList(sr) { it }.shuffled().subList(0, n)
    var bp = MutableList(sb) { it }.shuffled().subList(0, n)
    println("\n$n card(s) are to be swapped\n")
    println("The respective zero-based indices of the cards(s) to be swapped are:")
    println("  Red    : $rp")
    println("  Black  : $bp")
    for (i in 0 until n) {
        val temp = red[rp[i]]
        red[rp[i]] = black[bp[i]]
        black[bp[i]] = temp
    }
    println("\nAfter swapping, the state of the red and black stacks is:")
    println("  Red    : $red")
    println("  Black  : $black")

    // Check that the number of black cards in the black stack equals
    // the number of red cards in the red stack.
    var rcount = 0
    var bcount = 0
    for (c in red) if (c == 'R') rcount++
    for (c in black) if (c == 'B') bcount++
    println("\nThe number of red cards in the red stack     = $rcount")
    println("The number of black cards in the black stack = $bcount")
    if (rcount == bcount) {
        println("So the asssertion is correct!")
    }
    else {
        println("So the asssertion is incorrect!")
    }
}
```


{{output}}
First sample run:

```txt

After dealing the cards the state of the stacks is:
  Red    : 10 cards -> [R, R, B, R, R, B, B, R, R, R]
  Black  : 16 cards -> [B, B, R, R, R, R, B, R, R, B, R, B, B, R, R, B]
  Discard: 26 cards -> [R, B, B, B, R, R, B, B, B, R, B, B, B, B, R, B, R, B, R, R, R, R, B, B, B, B]

7 card(s) are to be swapped

The respective zero-based indices of the cards(s) to be swapped are:
  Red    : [3, 5, 6, 1, 4, 9, 7]
  Black  : [2, 0, 3, 15, 14, 12, 7]

After swapping, the state of the red and black stacks is:
  Red    : [R, B, B, R, R, B, R, R, R, B]
  Black  : [B, B, R, B, R, R, B, R, R, B, R, B, R, R, R, R]

The number of red cards in the red stack     = 6
The number of black cards in the black stack = 6
So the asssertion is correct!

```


Second sample run:

```txt

After dealing the cards the state of the stacks is:
  Red    : 11 cards -> [B, R, R, B, R, B, R, B, B, B, R]
  Black  : 15 cards -> [B, R, R, R, B, R, R, R, B, R, R, R, B, R, B]
  Discard: 26 cards -> [B, R, R, B, B, R, R, B, B, R, R, B, B, B, B, R, R, B, B, B, R, R, B, B, B, R]

3 card(s) are to be swapped

The respective zero-based indices of the cards(s) to be swapped are:
  Red    : [4, 2, 3]
  Black  : [0, 14, 2]

After swapping, the state of the red and black stacks is:
  Red    : [B, R, B, R, B, B, R, B, B, B, R]
  Black  : [R, R, B, R, B, R, R, R, B, R, R, R, B, R, R]

The number of red cards in the red stack     = 4
The number of black cards in the black stack = 4
So the asssertion is correct!

```



## Mathematica



```Mathematica
s = RandomSample@Flatten[{Table[0, 26], Table[1, 26]}]
g = Take[s, {1, -1, 2}]
d = Take[s, {2, -1, 2}]
a = b = {};
Table[If[g[[i]] == 1, AppendTo[a, d[[i]]], AppendTo[b, d[[i]]]], {i, 
   Length@g}];
a
b
dice = RandomInteger[{1, 6}]
ra = Sort@RandomSample[Range@Length@a, dice]
a = {Delete[a, List /@ ra], a[[ra]]}
rb = Sort@RandomSample[Range@Length@b, dice]
b = {Delete[b, List /@ rb], b[[rb]]}
finala = Join[a[[1]], b[[2]]]
finalb = Join[b[[1]], a[[2]]]
Count[finala, 1] == Count[finalb, 0]
```


{{output}}


```txt

{0,1,0,1,0,1,0,0,0,1,0,0,0,1,1,0,0,0,1,1,1,1,0,1,0,0,1,1,1,0,0,0,0,1,0,1,0,0,1,0,1,1,1,1,1,0,1,1,1,0,0,1}   //shuffled deck
{0,0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0}             // those are the key-cards
{1,1,1,0,1,0,1,0,0,1,1,1,0,1,0,0,1,1,0,0,1,1,0,1,0,1}             //those are the cards that will form the 2 piles based on keys
{0,1,1,1,0,0,1,1,0,1,0}                   //this is pile A
{1,1,1,0,1,0,1,0,1,0,0,1,1,0,1}          // this is pile B
4                                   //we throw a dice
{2,7,10,11}                         // we remove this 4 cards from pile A
{{0,1,1,0,0,1,0},{1,1,1,0}}         // this is pile A split
{3,8,10,11}                         // we remove 4 cards from different positions of pile B 
{{1,1,0,1,0,1,1,1,1,0,1},{1,0,0,0}}  //here is pile B split
{0,1,1,0,0,1,0,1,0,0,0}               //this is final A pile
{1,1,0,1,0,1,1,1,1,0,1,1,1,1,0}        //this is final B pile
True                                  // the result is TRUE

```




## Phix


```Phix
constant n = 52,
         pack = shuffle(repeat('r',n/2)&repeat('b',n/2))
 
string {black, red, discard} @= ""
for i=1 to length(pack) by 2 do
    integer {top,next} = pack[i..i+1]
    if top=='b' then
        black &= next
    else
        red &= next
    end if
    discard &= top
end for
black = shuffle(black); red = shuffle(red) -- (optional)
--printf(1,"Discards : %s\n",{discard})

printf(1,"Reds   : %s\nBlacks : %s\n\n",{red,black})

integer lb = length(black), lr = length(red),
        ns = rand(min(lb,lr))
printf(1,"Swap %d cards:\n\n", ns)
{black[1..ns],red[1..ns]} = {red[1..ns],black[1..ns]}

printf(1,"Reds   : %s\nBlacks : %s\n\n",{red,black})
 
integer nb = sum(sq_eq(black,'b')),
        nr = sum(sq_eq(red,'r'))
string correct = iff(nr==nb?"correct":"**INCORRECT**")
printf(1,"%d r in red, %d b in black - assertion %s\n",{nr,nb,correct})
```

{{out}}

```txt

Reds   : brrbrrrrbrbr
Blacks : brrrbbbrbbbbrr

Swap 5 cards:

Reds   : brrrbrrrbrbr
Blacks : brrbrbbrbbbbrr

8 r in red, 8 b in black - assertion correct

```



## Perl

Trying several non-random deck orderings, in addition to a shuffled one.  Predictably, the trick always works.
{{trans|Perl 6}}

```perl
sub trick {
    my(@deck) = @_;
    my $result .= sprintf "%-28s @deck\n", 'Starting deck:';

    my(@discard, @red, @black);
    deal(\@deck, \@discard, \@red, \@black);

    $result .= sprintf "%-28s @red\n", 'Red     pile:';
    $result .= sprintf "%-28s @black\n", 'Black   pile:';

    my $n = int rand(+@red < +@black ? +@red : +@black);
    swap(\@red, \@black, $n);

    $result .= sprintf "Red pile   after %2d swapped: @red\n", $n;
    $result .= sprintf "Black pile after %2d swapped: @black\n", $n;

    $result .= sprintf "Red in Red, Black in Black:  %d = %d\n", (scalar grep {/R/} @red), scalar grep {/B/} @black;
    return "$result\n";
}

sub deal {
    my($c, $d, $r, $b) = @_;
    while (@$c) {
        my $top = shift @$c;
        if ($top eq 'R') { push @$r, shift @$c }
        else             { push @$b, shift @$c }
        push @$d, $top;
    }
}

sub swap {
    my($r, $b, $n) = @_;
    push @$r, splice @$b, 0, $n;
    push @$b, splice @$r, 0, $n;
}

@deck = split '', 'RB' x 26;               # alternating red and black
print trick(@deck);
@deck = split '', 'RRBB' x 13;             # alternating pairs of reds and blacks
print trick(@deck);
@deck = sort @deck;                        # all blacks precede reds
print trick(@deck);
@deck = sort { -1 + 2*int(rand 2) } @deck; # poor man's shuffle
print trick(@deck);
```

{{out}}

```txt
Starting deck:               R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B R B
Red     pile:                B B B B B B B B B B B B B B B B B B B B B B B B B B
Black   pile:
Red pile   after  0 swapped: B B B B B B B B B B B B B B B B B B B B B B B B B B
Black pile after  0 swapped:
Red in Red, Black in Black:  0 = 0

Starting deck:               R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B R R B B
Red     pile:                R R R R R R R R R R R R R
Black   pile:                B B B B B B B B B B B B B
Red pile   after  8 swapped: R R R R R B B B B B B B B
Black pile after  8 swapped: B B B B B R R R R R R R R
Red in Red, Black in Black:  5 = 5

Starting deck:               B B B B B B B B B B B B B B B B B B B B B B B B B B R R R R R R R R R R R R R R R R R R R R R R R R R R
Red     pile:                R R R R R R R R R R R R R
Black   pile:                B B B B B B B B B B B B B
Red pile   after  8 swapped: R R R R R B B B B B B B B
Black pile after  8 swapped: B B B B B R R R R R R R R
Red in Red, Black in Black:  5 = 5

Starting deck:               B B R R B R B B B R R B R R B B B B R B R R B B B B B B R B R B R R R R B R B B B R B R B R R R R R R R
Red     pile:                R B R B R B B R R R R R
Black   pile:                B R B R B B B B B R B R R R
Red pile   after  6 swapped: B R R R R R B R B R B B
Black pile after  6 swapped: B B B R B R R R R B R B R B
Red in Red, Black in Black:  7 = 7
```



## Perl 6

{{works with|Rakudo|2018.08}}


```perl6
# Generate a shuffled deck
my @deck = shuffle;
put 'Shuffled deck:          ', @deck;

my (@discard, @red, @black);
# Deal cards following task description
deal(@deck, @discard, @red, @black);

put 'Discard pile:           ', @discard;
put '"Red"   pile:           ', @red;
put '"Black" pile:           ', @black;

# swap the same random number of random
# cards between the red and black piles
my $amount = ^(+@red min +@black) .roll;
put 'Number of cards to swap: ', $amount;
swap(@red, @black, $amount);

put 'Red pile after swaps:   ', @red;
put 'Black pile after swaps: ', @black;

say 'Number of Red   cards in the Red   pile: ', +@red.grep('R');
say 'Number of Black cards in the Black pile: ', +@black.grep('B');

sub shuffle { (flat 'R' xx 26, 'B' xx 26).pick: * }

sub deal (@deck, @d, @r, @b) {
    while @deck.elems {
        my $top = @deck.shift;
        if $top eq 'R' {
            @r.push: @deck.shift;
        }
        else {
            @b.push: @deck.shift;
        }
        @d.push: $top;
    }
}

sub swap (@r, @b, $a) {
    my @ri  = ^@r .pick($a);
    my @bi  = ^@b .pick($a);
    my @rs  = @r[@ri];
    my @bs  = @b[@bi];
    @r[@ri] = @bs;
    @b[@bi] = @rs;
}
```

{{out|Sample output}}

```txt
Shuffled deck:          B B B R B R R R B B R R R B R B R B R R R B R B B R R B B R R B R B R R R R B R R B B B B B B R R B B B
Discard pile:           B B B R B R R R R R R R B R B R R R R B R B B B R B
"Red"   pile:           R R B B B R B B B B B R R B B
"Black" pile:           B R R B R R R B B R B
Number of cards to swap: 6
Red pile after swaps:   R R B B R R R R B B B R B B B
Black pile after swaps: B R B R R B B B B R B
Number of Red   cards in the Red   pile: 7
Number of Black cards in the Black pile: 7
```



## Python

The code is layed out to follow the task decription, leading to some deviations from the PEP8 ''guidelines''

```python
import random

## 1. Cards
n = 52
Black, Red = 'Black', 'Red'
blacks = [Black] * (n // 2) 
reds = [Red] * (n // 2)
pack = blacks + reds
# Give the pack a good shuffle.
random.shuffle(pack)

## 2. Deal from the randomised pack into three stacks
black_stack, red_stack, discard = [], [], []
while pack:
    top = pack.pop()
    if top == Black:
        black_stack.append(pack.pop())
    else:
        red_stack.append(pack.pop())
    discard.append(top)
print('(Discards:', ' '.join(d[0] for d in discard), ')\n')

## 3. Swap the same, random, number of cards between the two stacks.
# We can't swap more than the number of cards in a stack.
max_swaps = min(len(black_stack), len(red_stack))
# Randomly choose the number of cards to swap.
swap_count = random.randint(0, max_swaps)
print('Swapping', swap_count)
# Randomly choose that number of cards out of each stack to swap.
def random_partition(stack, count):
    "Partition the stack into 'count' randomly selected members and the rest"
    sample = random.sample(stack, count)
    rest = stack[::]
    for card in sample:
        rest.remove(card)
    return rest, sample

black_stack, black_swap = random_partition(black_stack, swap_count)
red_stack, red_swap = random_partition(red_stack, swap_count)

# Perform the swap.
black_stack += red_swap
red_stack += black_swap

## 4. Order from randomness?
if black_stack.count(Black) == red_stack.count(Red):
    print('Yeha! The mathematicians assertion is correct.')
else:
    print('Whoops - The mathematicians (or my card manipulations) are flakey')
```


A run.
{{out}}

```txt
(Discards: R B R R B B R R R B B B B R R R B R R B B B B R B R )

Swapping 11
Yeha! The mathematicians assertion is correct.
```


A second run:
{{out}}

```txt
(Discards: R B B R B B R B R R R B R R B B B B R R B R R B B R )

Swapping 2
Yeha! The mathematicians assertion is correct.
```



## R


```R

magictrick<-function(){
  deck=c(rep("B",26),rep("R",26))
  deck=sample(deck,52)
  blackpile=character(0)
  redpile=character(0)
  discardpile=character(0)
  while(length(deck)>0){
    if(deck[1]=="B"){
      blackpile=c(blackpile,deck[2])
      deck=deck[-2]
    }else{
      redpile=c(redpile,deck[2])
      deck=deck[-2]
    }
    discardpile=c(discardpile,deck[1])
    deck=deck[-1]
  }
  cat("After the deal the state of the piles is:","\n",
      "Black pile:",blackpile,"\n","Red pile:",redpile,"\n",
      "Discard pile:",discardpile,"\n","\n")
  X=sample(1:min(length(redpile),length(blackpile)),1)
  if(X==1){s=" is"}else{s="s are"}
  cat(X," card",s," being swapped.","\n","\n",sep="")
  redindex=sample(1:length(redpile),X)
  blackindex=sample(1:length(blackpile),X)
  redbunch=redpile[redindex]
  redpile=redpile[-redindex]
  blackbunch=blackpile[blackindex]
  blackpile=blackpile[-blackindex]
  redpile=c(redpile,blackbunch)
  blackpile=c(blackpile,redbunch)
  cat("After the swap the state of the piles is:","\n",
      "Black pile:",blackpile,"\n","Red pile:",redpile,"\n","\n")
  cat("There are ", length(which(blackpile=="B")), " black cards in the black pile.","\n",
      "There are ", length(which(redpile=="R")), " red cards in the red pile.","\n",sep="")
  if(length(which(blackpile=="B"))==length(which(redpile=="R"))){
    cat("The assertion is true!")
  }
}

```

{{Out}}

```txt

After the deal the state of the piles is: 
 Black pile: B B R R B R B R R B B 
 Red pile: R B B B B B R B R R R B R B B 
 Discard pile: R R R R B R R R B R R B R B R R B R B R B B B R B B 
 
10 cards are being swapped.

After the swap the state of the piles is: 
 Black pile: R B B B B R R R R B R 
 Red pile: B B R B B B R B B B B R B R R 
 
There are 5 black cards in the black pile.
There are 5 red cards in the red pile.
The assertion is true!

```



## REXX

Programming notes:   This REXX version uses a neat trick that the '''Python''' entry uses:   instead of using a deck of

cards,   it just uses a "deck" of numbers that correspond to the order of playing cards that came out of a new box of 

cards.   Odd numbers represent red cards, even numbers represent black cards.

This could've been possibly simplified by using negative and positive numbers,   ''or''   more accurately,   the suits and 

a pip, but it would've taken more program logic to determine the color of the suits in a very succinct and efficient way.

Also, code was added to perform any number of trials.   Code was also added to allow repeatability by specifying a 

''seed''   value for the   '''random'''   BIF. 

A new deck of cards is always created     ''as if the playing cards were manufactured and put into a box'',     that is,

13 spades (<big>♠</big>) in a row   (A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K),     13 
hearts (<big>♥</big>),     13 clubs (<big>♣</big>),     and 13 diamonds (<big>♦</big>).

(Various card playing manufacturers would arrange a new playing deck differently, but that isn't a concern.)

The number of cards in the deck and also the number of shuffles can be specified.   One shuffle swaps two random

cards, two shuffles swaps four random cards ···

Extra coding was added to keep ''singularities''   (opposite of a plural)   to keep the English gooder (''sic''),   as well as 

adding commas to larger numbers.

```rexx
/*REXX pgm mimics a boggling card trick; separates cards into 3 piles based on color ···*/
parse arg trials # shuffs seed .                 /*obtain optional arguments from the CL*/
if trials=='' | trials==","  then trials= 1000   /*Not specified?  Then use the default.*/
if      #=='' |      #==","  then      #=   52   /* "      "         "   "   "     "    */
if shuffs=='' | shuffs==","  then shuffs=  #%4   /* "      "         "   "   "     "    */
if datatype(seed, 'W')   then call random ,,seed /*if integer, use this as a RANDOM seed*/
ok=0                                             /*the number of "expected" good trials.*/
                         do trials               /*perform a number of trials to be safe*/
                         call create             /*odd numbers≡RED,  even numbers≡BLACK.*/
                         call shuffle            /*shuffle the deck a number of times.  */
                         call deal               /*put cards into three piles of cards. */
                         call swap               /*swap rand # of cards in  R & B  piles*/
                         call count              /*count #blacks in B, #reds in R  piles*/
                         end   /*trials*/        /*#: is the number of cards in the deck*/
pc= (100*ok/trials)'%'                           /*calculate the  %  asserted correctly.*/
say "Correctness of the mathematician's assertion:"    pc   '  (out of'    commas(trials),
    "trial"s(trials)')  using a deck of '     commas(#)                                  ,
    " card"s(#)',  and doing '                commas(shuffs)         ' shuffle's(shuffs).
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
?:       return random(1, word( arg(1) #, 1) )   /*gen a random number from  1 ──► arg. */
commas:  parse arg _;  do j=length(_)-3  to 1  by -3; _=insert(',', _, j); end;   return _
create:  @.=; k=0; do j=1  by 4  for #; k=k+1; @.k= j; if k//13==0 then j=j+1; end; return
isRed:   return    arg(1) // 2                   /*if  arg(1)  is odd,  the card is RED.*/
s:       if arg(1)==1  then return arg(3);  return word( arg(2) 's', 1)    /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
count:   Rn=0;  Bn=0;              do j=1  for words(R);  Rn=Rn+   isRed(word(R,j)) ;  end
                                   do k=1  for words(B);  Bn=Bn+ (\isRed(word(B,k)));  end
         if Rn==Bn  then ok= ok+1;        return /*Was it a good trial?  Bump OK counter*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
deal:    R=;  B=;  D=;             do j=1  for #%2  by 2        /*deal all the cards.   */
                                   next= j+1;   card= @.next    /*obtain the next card. */
                                   if isRed(@.j)  then R=R card /*add to the  RED  pile?*/
                                                  else B=B card /* "   "  "  BLACK   "  */
                                   D= D @.j                     /* "   "  " discard  "  */
                                   end   /*j*/
         return                                                 /*discard pile not used.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
shuffle:   do j=1  for shuffs;  x=?();    do until y\==x | #==1;   y=?();   end  /*until*/
         parse value   @.x  @.y     with     @.y  @.x;  end  /*j*/;                 return
/*──────────────────────────────────────────────────────────────────────────────────────*/
swap:    $= min( words(R), words(B) );          Rc=;   Bc= /*ensure we can swap $ cards.*/
         if $==0  then return                              /*A pile has no cards? return*/
                                   do ?($)                 /*$:  is the number of swaps.*/
                                   R?= ?( words(R) )       /*a random card in  RED pile.*/
                                   B?= ?( words(B) )       /*"    "     "   " BLACK  "  */
    /* "reds"  to be swapped.*/    Rc= Rc word(R, R?);  R= delword(R, R?, 1)  /*del card*/
    /*"blacks"  "  "    "    */    Bc= Bc word(B, B?);  B= delword(B, B?, 1)  /* "    " */
                                   end   /*?($)*/
         R=R Bc;  B=B Rc;   return                         /*add swapped cards to piles.*/
```

{{out|output|text=  when using the default inputs:}}

```txt

Correctness of the mathematician's assertion: 100%   (out of 10,000 trials)  using a deck of  52  cards,  and doing  13  shuffles.

```



## Ruby


```ruby
deck = ([:black, :red] * 26 ).shuffle
black_pile, red_pile, discard = [], [], []

until deck.empty? do
  discard << deck.pop
  discard.last == :black ? black_pile << deck.pop : red_pile << deck.pop
end

x = rand( [black_pile.size, red_pile.size].min )

red_bunch   = x.times.map{ red_pile.delete_at( rand( red_pile.size )) }
black_bunch = x.times.map{ black_pile.delete_at( rand( black_pile.size )) }

black_pile += red_bunch
red_pile   += black_bunch

puts "The magician predicts there will be #{black_pile.count( :black )} red cards in the other pile.
Drumroll...
There were #{red_pile.count( :red )}!"

```

{{out}}

```txt
The magician predicts there will be 5 red cards in the other pile.
Drumroll...
There were 5!

```


## Rust

{{libheader|rand}}

```rust
extern crate rand; // 0.5.5
use rand::Rng;
use std::iter::repeat;

#[derive(Debug, Eq, PartialEq, Clone)]
enum Colour {
    Black,
    Red,
}
use Colour::*;

fn main() {
    let mut rng = rand::thread_rng();
    
    //Create our deck.
    let mut deck: Vec<_> = repeat(Black).take(26)
        .chain(repeat(Red).take(26))
        .collect();
    
    rng.shuffle(&mut deck);
    
    let mut black_stack = vec![];
    let mut red_stack = vec![];
    let mut discarded = vec![];
    
    //Deal our cards.
    print!("Discarding:");
    while let (Some(card), Some(next)) = (deck.pop(), deck.pop()) {
        print!(" {}", if card == Black { "B" } else { "R" });
        match card {
            Red => red_stack.push(next),
            Black => black_stack.push(next),
        }
        discarded.push(card);
    }
    println!();
    
    // Choose how many to swap.
    let max = red_stack.len().min(black_stack.len());
    let num = rng.gen_range(1, max);
    println!("Exchanging {} cards", num);
    
    // Actually swap our cards.
    for _ in 0..num {
        let red = rng.choose_mut(&mut red_stack).unwrap();
        let black = rng.choose_mut(&mut black_stack).unwrap();
        std::mem::swap(red, black);
    }
    
    //Count how many are red and black.
    let num_black = black_stack.iter()
        .filter(|&c| c == &Black)
        .count();
    let num_red = red_stack.iter()
        .filter(|&c| c == &Red)
        .count();
        
    println!("Number of black cards in black stack: {}", num_black);
    println!("Number of red cards in red stack: {}", num_red);
}
```


{{out}}

```txt
Discarding: R R B B R R B R R B B B B B R R R B B B R R R B R R
Exchanging 5 cards
Number of black cards in black stack: 4
Number of red cards in red stack: 4
```



## zkl


```zkl
cards:=[1..52].pump(List,"isEven","toInt").shuffle(); // red==1
stacks:=T(List(),List());   // black stack [0], red stack [1]
blkStk,redStk := stacks;
foreach card in (cards){ stacks[card].append(__cardWalker.next()) }
println("Stacks:\n  Black stack: ",redBlack(blkStk),"\n  Red stack:   ",redBlack(redStk));

numSwaps:=(1).random(1000);    // do lots of swaps
do(numSwaps){ blkStk.append(redStk.pop(0)); redStk.append(blkStk.pop(0)); }
println("Post %d swaps:\n  Black stack: %s\n  Red stack:   %s"
   .fmt(numSwaps,redBlack(blkStk),redBlack(redStk)));

numBlack,numRed := blkStk.filter('==(0)).len(), redStk.sum(0);
if(numBlack==numRed) 
   println("Agreed, black stack has same number of black cards\n  "
           "as red stack has number of red cards: ",numRed);
else println("Boo, different stack lenghts");

fcn redBlack(cards){ cards.pump(String,fcn(c){ c and "R " or "B " }) }
```

{{out}}

```txt

Stacks:
  Black stack: B B R R R R R R R R R B B B R B 
  Red stack:   B B R R B B R R R R 
Post 360 swaps:
  Black stack: R R R R B B R R R R R R R R R B 
  Red stack:   B B R B B B R R B B 
Agreed, black stack has same number of black cards
  as red stack has number of red cards: 3

```

