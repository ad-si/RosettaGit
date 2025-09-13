+++
title = "Intersecting Number Wheels"
description = ""
date = 2019-10-19T19:06:04Z
aliases = []
[extra]
id = 22565
[taxonomies]
categories = ["Data structures", "Mathematics", "task"]
tags = []
+++

## Task

A number wheel has:
* A ''name'' which is an uppercase letter.
* A set of ordered ''values'' which are either ''numbers'' or ''names''.


A ''number'' is generated/yielded from a named wheel by:
:1. Starting at the first value of the named wheel and advancing through subsequent values and wrapping around to the first value to form a "wheel":
::1.a If the value is a number, yield it.
::1.b If the value is a name, yield the next value from the named wheel
::1.c Advance the position of this wheel.

Given the wheel
: <code>A: 1 2 3</code>
the number 1 is first generated, then 2, then 3, 1, 2, 3, 1, ...

'''Note:''' When more than one wheel is defined as a set of intersecting wheels then the
first named wheel is assumed to be the one that values are generated from.

;Examples:
Given the wheels:
    A: 1 B 2
    B: 3 4
The series of numbers generated starts:
    1, 3, 2, 1, 4, 2, 1, 3, 2, 1, 4, 2, 1, 3, 2...

The intersections of number wheels can be more complex, (and might loop forever),
and wheels may be multiply connected.


'''Note:''' If a named wheel is referenced more than
once by one or many other wheels, then there is only one position of the wheel
that is advanced by each and all references to it.

E.g.
  A:  1 D D
  D:  6 7 8
  Generates:
    1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

;Task:
Generate and show the first twenty terms of the sequence of numbers generated
from these groups:

    Intersecting Number Wheel group:
      A:  1 2 3

    Intersecting Number Wheel group:
      A:  1 B 2
      B:  3 4

    Intersecting Number Wheel group:
      A:  1 D D
      D:  6 7 8

    Intersecting Number Wheel group:
      A:  1 B C
      B:  3 4
      C:  5 B

Show your output here, on this page.




## ALGOL 68


```algol68
BEGIN
    # a number wheel element                                                  #
    MODE NWELEMENT = UNION( CHAR # wheel name #, INT # wheel value # );
    # a number wheel                                                          #
    MODE NW = STRUCT( CHAR name, REF INT position, FLEX[ 1 : 0 ]NWELEMENT values );
    # get the next value from a number wheel in an array of number wheels     #
    # note: invalid wheel names will cause subscript range errors             #
    OP   NEXT = ( []NW wheels )INT:
         BEGIN
            INT  result;
            BOOL found := FALSE;
            INT  w     := LWB wheels; # start with the first wheel            #
            WHILE NOT found DO
                IF position OF wheels[ w ] > UPB values OF wheels[ w ] THEN
                    # passed the end of the wheel, go back to the start       #
                    position OF wheels[ w ] := LWB values OF wheels[ w ]
                FI;
                NWELEMENT e = ( values OF wheels[ w ] )[ position OF wheels[ w ] ];
                position OF wheels[ w ] +:= 1;
                CASE e
                  IN ( INT  n ): BEGIN result := n; found := TRUE END
                   , ( CHAR c ): BEGIN
                                     w := LWB wheels;
                                     WHILE name OF wheels[ w ] /= c DO w +:= 1 OD
                                 END
                ESAC
            OD;
            result
         END # NEXT # ;
    # prints the first n values from an array of wheels                       #
    PROC show = ( INT n, []NW wheels )VOID:
         BEGIN
            print( ( "First ", whole( n, 0 ), " values from the Intersecting Number Wheels:" ) );
            FOR i FROM LWB wheels TO UPB wheels DO
                print( ( newline, "    ", name OF wheels[ i ], ":" ) );
                FOR v FROM LWB values OF wheels[ i ] TO UPB values OF wheels[ i ] DO
                    CASE ( values OF wheels[ i ] )[ v ]
                      IN ( INT  n ): print( ( " ", whole( n, 0 ) ) )
                       , ( CHAR c ): print( ( " ", c ) )
                    ESAC
                OD
            OD;
            print( ( newline, "        " ) );
            FOR i TO n DO print( ( " ", whole( NEXT wheels, 0 ) ) ) OD;
            print( ( newline, newline ) )
         END # show # ;
    # show some wheels in action                                              #
    show( 20, ( NW( "A", LOC INT := 1, (  1,   2,   3  ) ) ) );
    show( 20, ( NW( "A", LOC INT := 1, (  1,  "B",  2  ) )
              , NW( "B", LOC INT := 1, (  3,   4       ) ) ) );
    show( 20, ( NW( "A", LOC INT := 1, (  1,  "D", "D" ) )
              , NW( "D", LOC INT := 1, (  6,   7,   8  ) ) ) );
    show( 20, ( NW( "A", LOC INT := 1, (  1,  "B", "C" ) )
              , NW( "B", LOC INT := 1, (  3,   4       ) )
              , NW( "C", LOC INT := 1, (  5,  "B"      ) ) ) )
END
```

{{out}}

```txt

First 20 values from the Intersecting Number Wheels:
    A: 1 2 3
         1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2

First 20 values from the Intersecting Number Wheels:
    A: 1 B 2
    B: 3 4
         1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3

First 20 values from the Intersecting Number Wheels:
    A: 1 D D
    D: 6 7 8
         1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6

First 20 values from the Intersecting Number Wheels:
    A: 1 B C
    B: 3 4
    C: 5 B
         1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4


```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

public static class IntersectingNumberWheels
{
    public static void Main() {
        TurnWheels(('A', "123")).Take(20).Print();
        TurnWheels(('A', "1B2"), ('B', "34")).Take(20).Print();
        TurnWheels(('A', "1DD"), ('D', "678")).Take(20).Print();
        TurnWheels(('A', "1BC"), ('B', "34"), ('C', "5B")).Take(20).Print();
    }

    static IEnumerable<char> TurnWheels(params (char name, string values)[] wheels) {
        var data = wheels.ToDictionary(wheel => wheel.name, wheel => wheel.values.Loop().GetEnumerator());
        var primary = data[wheels[0].name];
        while (true) {
            yield return Turn(primary);
        }

        char Turn(IEnumerator<char> sequence) {
            sequence.MoveNext();
            char c = sequence.Current;
            return char.IsDigit(c) ? c : Turn(data[c]);
        }
    }

    static IEnumerable<T> Loop<T>(this IEnumerable<T> seq) {
        while (true) {
            foreach (T element in seq) yield return element;
        }
    }

    static void Print(this IEnumerable<char> sequence) => Console.WriteLine(string.Join(" ", sequence));
}
```

{{out}}

```txt

1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3
1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6
1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4
```



## D


```d
import std.exception;
import std.range;
import std.stdio;

struct Wheel {
    private string[] values;
    private uint index;

    invariant {
        enforce(index < values.length, "index out of range");
    }

    this(string[] value...) in {
        enforce(value.length > 0, "Cannot create a wheel with no elements");
    } body {
        values = value;
    }

    enum empty = false;

    auto front() {
        return values[index];
    }

    void popFront() {
        index = (index + 1) % values.length;
    }
}

struct NamedWheel {
    private Wheel[char] wheels;
    char m;

    this(char c, Wheel w) {
        add(c, w);
        m = c;
    }

    void add(char c, Wheel w) {
        wheels[c] = w;
    }

    enum empty = false;

    auto front() {
        auto v = wheels[m].front;
        char c = v[0];
        while ('A' <= c && c <= 'Z') {
            v = wheels[c].front;
            c = v[0];
        }
        return v;
    }

    void popFront() {
        auto v = wheels[m].front;
        wheels[m].popFront;

        char c = v[0];
        while ('A' <= c && c <= 'Z') {
            auto d = wheels[c].front;
            wheels[c].popFront;
            c = d[0];
        }
    }
}

void group1() {
    auto a = Wheel("1", "2", "3");
    a.take(20).writeln;
}

void group2() {
    auto a = Wheel("1", "B", "2");
    auto b = Wheel("3", "4");

    auto n = NamedWheel('A', a);
    n.add('B', b);

    n.take(20).writeln;
}

void group3() {
    auto a = Wheel("1", "D", "D");
    auto d = Wheel("6", "7", "8");

    auto n = NamedWheel('A', a);
    n.add('D', d);

    n.take(20).writeln;
}

void group4() {
    auto a = Wheel("1", "B", "C");
    auto b = Wheel("3", "4");
    auto c = Wheel("5", "B");

    auto n = NamedWheel('A', a);
    n.add('B', b);
    n.add('C', c);

    n.take(20).writeln;
}

void main() {
    group1();
    group2();
    group3();
    group4();
}
```

{{out}}

```txt
["1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2"]
["1", "3", "2", "1", "4", "2", "1", "3", "2", "1", "4", "2", "1", "3", "2", "1", "4", "2", "1", "3"]
["1", "6", "7", "1", "8", "6", "1", "7", "8", "1", "6", "7", "1", "8", "6", "1", "7", "8", "1", "6"]
["1", "3", "5", "1", "4", "3", "1", "4", "5", "1", "3", "4", "1", "3", "5", "1", "4", "3", "1", "4"]
```


=={{header|F_Sharp|F#}}==

```fsharp

// Wheels within wheels. Nigel Galloway: September 30th., 2019.
let N(n)=fun()->n
let wheel(n:(unit->int)[])=let mutable g= -1 in (fun()->g<-(g+1)%n.Length; n.[g]())
let A1=wheel[|N(1);N(2);N(3)|]
for n in 0..20 do printf "%d " (A1())
printfn ""
let B2=wheel[|N(3);N(4)|]
let A2=wheel[|N(1);B2;N(2)|]
for n in 0..20 do printf "%d " (A2())
printfn ""
let D3=wheel[|N(6);N(7);N(8)|]
let A3=wheel[|N(1);D3;D3|]
for n in 0..20 do printf "%d " (A3())
printfn ""
let B4=wheel[|N(3);N(4)|]
let C4=wheel[|N(5);B4|]
let A4=wheel[|N(1);B4;C4|]
for n in 0..20 do printf "%d " (A4())
printfn ""

```

{{out}}

```txt

1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3
1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 2
1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 7
1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 5

```



## Factor

An attempt has been made to simplify the interface as much as possible by creating a natural literal syntax for number wheel groups. This should be useful for exploring these types of sequences in the future.
<code>nw-parser</code> is an EBNF grammar that turns

```txt

"A: 1 B C\nB: 3 4\nC: 5 B"

```

into

```txt

{
    { "A" T{ number-wheel { seq T{ circular { seq { 1 "B" "C" } } } } { i 0 } } }
    { "B" T{ number-wheel { seq T{ circular { seq { 3 4 } } } } { i 0 } } }
    { "C" T{ number-wheel { seq T{ circular { seq { 5 "B" } } } } { i 0 } } }
}

```

⁠— a dictionary-like structure that is transformed into a lazy list which yields the expected sequence elements.
{{works with|Factor|0.99 2019-07-10}}

```factor
USING: accessors assocs circular io kernel lists lists.lazy math
math.parser multiline peg.ebnf prettyprint prettyprint.custom
sequences strings ;
IN: rosetta-code.number-wheels

TUPLE: group pretty list ;

C: <group> group

M: group pprint* pretty>> write ;

TUPLE: number-wheel seq i ;

: <number-wheel> ( seq -- number-wheel )
    <circular> 0 number-wheel boa ;

: yield ( assoc -- n )
    dup first first [ dup integer? ]
    [ dupd of [ i>> ] [ [ 1 + ] change-i seq>> nth ] bi ] until
    nip ;

: number-wheel>lazy ( assoc -- list )
    0 lfrom swap [ yield nip ] curry lmap-lazy ;

EBNF: nw-parser [=[
    num   = [0-9]+ => [[ >string string>number ]]
    name  = [a-zA-Z]+ => [[ >string ]]
    wheel = (" "~ (num | name))+ "\n"?
          => [[ but-last first <number-wheel> ]]
    group = (name ":"~ wheel)+ => [[ number-wheel>lazy ]]
]=]

SYNTAX: NUMBER-WHEELS: parse-here dup nw-parser <group> suffix! ;

: .take ( n group -- )
    list>> ltake list>array [ pprint bl ] each "..." print ;
```

Now the interface defined above may be used:

```factor
USING: generalizations io kernel prettyprint
rosetta-code.number-wheels ;

NUMBER-WHEELS:
A: 1 2 3
;

NUMBER-WHEELS:
A: 1 B 2
B: 3 4
;

NUMBER-WHEELS:
A: 1 D D
D: 6 7 8
;

NUMBER-WHEELS:
A: 1 B C
B: 3 4
C: 5 B
;

[
    "Intersecting number wheel group:" print
    [ . ] [ "Generates:" print 20 swap .take nl ] bi
] 4 napply
```

{{out}}

```txt

Intersecting number wheel group:
A: 1 2 3
Generates:
1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Intersecting number wheel group:
A: 1 B 2
B: 3 4
Generates:
1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Intersecting number wheel group:
A: 1 D D
D: 6 7 8
Generates:
1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Intersecting number wheel group:
A: 1 B C
B: 3 4
C: 5 B
Generates:
1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...

```



## Go


```go
package main

import (
    "fmt"
    "sort"
    "strconv"
)

type wheel struct {
    next   int
    values []string
}

type wheelMap = map[string]wheel

func generate(wheels wheelMap, start string, maxCount int) {
    count := 0
    w := wheels[start]
    for {
        s := w.values[w.next]
        v, err := strconv.Atoi(s)
        w.next = (w.next + 1) % len(w.values)
        wheels[start] = w
        if err == nil {
            fmt.Printf("%d ", v)
            count++
            if count == maxCount {
                fmt.Println("...\n")
                return
            }
        } else {
            for {
                w2 := wheels[s]
                ss := s
                s = w2.values[w2.next]
                w2.next = (w2.next + 1) % len(w2.values)
                wheels[ss] = w2
                v, err = strconv.Atoi(s)
                if err == nil {
                    fmt.Printf("%d ", v)
                    count++
                    if count == maxCount {
                        fmt.Println("...\n")
                        return
                    }
                    break
                }
            }
        }
    }
}

func printWheels(wheels wheelMap) {
    var names []string
    for name := range wheels {
        names = append(names, name)
    }
    sort.Strings(names)
    fmt.Println("Intersecting Number Wheel group:")
    for _, name := range names {
        fmt.Printf("  %s: %v\n", name, wheels[name].values)
    }
    fmt.Print("  Generates:\n    ")
}

func main() {
    wheelMaps := []wheelMap{
        {
            "A": {0, []string{"1", "2", "3"}},
        },
        {
            "A": {0, []string{"1", "B", "2"}},
            "B": {0, []string{"3", "4"}},
        },
        {
            "A": {0, []string{"1", "D", "D"}},
            "D": {0, []string{"6", "7", "8"}},
        },
        {
            "A": {0, []string{"1", "B", "C"}},
            "B": {0, []string{"3", "4"}},
            "C": {0, []string{"5", "B"}},
        },
    }
    for _, wheels := range wheelMaps {
        printWheels(wheels)
        generate(wheels, "A", 20)
    }
}
```


{{out}}

```txt

Intersecting Number Wheel group:
  A: [1 2 3]
  Generates:
    1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Intersecting Number Wheel group:
  A: [1 B 2]
  B: [3 4]
  Generates:
    1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Intersecting Number Wheel group:
  A: [1 D D]
  D: [6 7 8]
  Generates:
    1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Intersecting Number Wheel group:
  A: [1 B C]
  B: [3 4]
  C: [5 B]
  Generates:
    1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...

```



## Haskell


Defining a unit movement of the interlocking wheels as a recursive descent,
terminating at the first digit found, and printing a map-accumulation of that recursion over a list of given length but arbitrary content.


```haskell
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (mapAccumL)
import Data.Char (isDigit)
import Data.Bool (bool)


clockWorkTick :: M.Map Char String -> (M.Map Char String, Char)
clockWorkTick = flip click 'A'
  where
    click wheels name =
      let wheel = fromMaybe ['?'] (M.lookup name wheels)
          v = head wheel
      in bool
           click
           (,)
           (isDigit v || '?' == v)
           (M.insert name (leftRotate wheel) wheels)
           v

leftRotate :: [a] -> [a]
leftRotate = take . length <*> (tail . cycle)


-- TEST ---------------------------------------------------
main :: IO ()
main = do
  let wheelSets =
        [ [('A', "123")]
        , [('A', "1B2"), ('B', "34")]
        , [('A', "1DD"), ('D', "678")]
        , [('A', "1BC"), ('B', "34"), ('C', "5B")]
        ]
  putStrLn "State of each wheel-set after 20 clicks:\n"
  mapM_ print $
    fmap
      (flip (mapAccumL (const . clockWorkTick)) (replicate 20 ' ') . M.fromList)
      wheelSets
  putStrLn "\nInitial state of the wheel-sets:\n"
  mapM_ print wheelSets
```

{{Out}}

```txt
State of each wheel-set after 20 clicks:

(fromList [('A',"312")],"12312312312312312312")
(fromList [('A',"21B"),('B',"43")],"13214213214213214213")
(fromList [('A',"D1D"),('D',"786")],"16718617816718617816")
(fromList [('A',"C1B"),('B',"34"),('C',"5B")],"13514314513413514314")

Initial state of the wheel-sets:

[('A',"123")]
[('A',"1B2"),('B',"34")]
[('A',"1DD"),('D',"678")]
[('A',"1BC"),('B',"34"),('C',"5B")]
```



## JavaScript

Map-accumulation of a recursive digit-search,
over an array of given length and arbitrary contents.
{{Trans|Haskell}}
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {

        // clockWorkTick :: Dict -> (Dict, Char)
        const clockWorkTick = wheelMap => {
            // The new configuration of the wheels, tupled with
            // a digit found by recursive descent from a single
            // click of the first wheel.
            const click = wheels => wheelName => {
                const
                    wheel = wheels[wheelName] || ['?'],
                    v = wheel[0];
                return bool(click)(Tuple)(isDigit(v) || '?' === v)(
                    insertDict(wheelName)(
                        leftRotate(wheel)
                    )(wheels)
                )(v);
            };
            return click(wheelMap)('A');
        };

        // leftRotate ::[a] -> [a]
        const leftRotate = xs =>
            // The head of the list appended
            // to the tail of of the list.
            0 < xs.length ? (
                xs.slice(1).concat(xs[0])
            ) : [];


        // TEST -------------------------------------------
        // State of each wheel-set after 20 clicks,
        // paired with the resulting series of characters.

        const tuple = uncurry(Tuple);
        const wheelLists = [
            [tuple('A', '123')],
            [tuple('A', '1B2'), tuple('B', '34')],
            [tuple('A', '1DD'), tuple('D', '678')],
            [tuple('A', '1BC'), tuple('B', '34'), tuple('C', '5B')]
        ];

        console.log([
            'Series and state of each wheel-set after 20 clicks:\n',
            unlines(
                map(tuples => showWheels(
                    mapAccumL(
                        compose(constant, clockWorkTick)
                    )(dictFromList(tuples))(replicate(20)(''))
                ))(wheelLists)
            ),
            '\nInitial state of each wheel-set:\n',
            map(map(compose(
                JSON.stringify,
                dictFromList,
                x => [Array.from(x)]
            )))(wheelLists).join('\n')
        ].join('\n'));
    };

    // DISPLAY FORMATTING ---------------------------------

    // showWheels :: (Dict, [Char]) -> String
    const showWheels = tpl =>
        JSON.stringify(
            Array.from(secondArrow(concat)(tpl))
        );

    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // bool :: a -> a -> Bool -> a
    const bool = f => t => p =>
        p ? t : f;

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (...fs) =>
        x => fs.reduceRight((a, f) => f(a), x);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // constant :: a -> b -> a
    const constant = k => _ => k;

    // dictFromList :: [(k, v)] -> Dict
    const dictFromList = kvs =>
        Object.fromEntries(kvs);

    // secondArrow :: (a -> b) -> ((c, a) -> (c, b))
    const secondArrow = f => xy =>
        // A function over a simple value lifted
        // to a function over a tuple.
        // f (a, b) -> (a, f(b))
        Tuple(xy[0])(
            f(xy[1])
        );

    // insertDict :: String -> a -> Dict -> Dict
    const insertDict = k => v => dct =>
        Object.assign({}, dct, {
            [k]: v
        });

    // isDigit :: Char -> Bool
    const isDigit = c => {
        const n = c.codePointAt(0);
        return 48 <= n && 57 >= n;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // Map-accumulation is a combination of map and a catamorphism;
    // it applies a function to each element of a list, passing an
    // accumulating parameter from left to right, and returning a final
    // value of this accumulator together with the new list.

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = f => acc => xs =>
        xs.reduce((a, x) => {
            const pair = f(a[0])(x);
            return Tuple(pair[0])(a[1].concat(pair[1]));
        }, Tuple(acc)([]));

    // replicate :: Int -> a -> [a]
    const replicate = n => x =>
        Array.from({
            length: n
        }, () => x);

    // uncurry :: (a -> b -> c) -> ((a, b) -> c)
    const uncurry = f =>
        (x, y) => f(x)(y);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Series and state of each wheel-set after 20 clicks:

[{"A":"312"},"12312312312312312312"]
[{"A":"21B","B":"43"},"13214213214213214213"]
[{"A":"D1D","D":"786"},"16718617816718617816"]
[{"A":"C1B","B":"34","C":"5B"},"13514314513413514314"]

Initial state of each wheel-set:

{"A":"123"}
{"A":"1B2"},{"B":"34"}
{"A":"1DD"},{"D":"678"}
{"A":"1BC"},{"B":"34"},{"C":"5B"}
```



## Julia


```julia
const d1 = Dict("A" => [["1", "2", "3"], 1])
const d2 = Dict("A" => [["1", "B", "2"], 1], "B" => [["3", "4"], 1])
const d3 = Dict("A" => [["1", "D", "D"], 1], "D" => [["6", "7", "8"], 1])
const d4 = Dict("A" => [["1", "B", "C"], 1], "B" => [["3", "4"], 1],
    "C" => [["5", "B"], 1])

function getvalue!(wheelname, allwheels)
    wheel = allwheels[wheelname]
    s = wheel[1][wheel[2]]
    wheel[2] = mod1(wheel[2] + 1, length(wheel[1]))
    return haskey(allwheels, s) ? getvalue!(s, allwheels) : s
end

function testwheels(wheels, numterms = 20, firstwheel = "A")
    println("\nNumber Wheels:")
    for k in sort(collect(keys(wheels)))
        print("$k: [")
        for s in wheels[k][1]
            print(s, " ")
        end
        println("\b]")
    end
    print("Output: ")
    for _ in 1:numterms
        print(getvalue!(firstwheel, wheels), " ")
    end
    println("...")
end

foreach(testwheels, [d1, d2, d3, d4])

```
{{out}}

```txt

Number Wheels:
A: [1 2 3]
Output: 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Number Wheels:
A: [1 B 2]
B: [3 4]
Output: 1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Number Wheels:
A: [1 D D]
D: [6 7 8]
Output: 1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Number Wheels:
A: [1 B C]
B: [3 4]
C: [5 B]
Output: 1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...

```



## Perl

{{trans|Julia}}

```perl
use strict;
use warnings;
use feature 'say';

sub get_next {
    my($w,%wheels) = @_;
    my $wh = \@{$wheels{$w}}; # reference, not a copy
    my $value = $$wh[0][$$wh[1]];
    $$wh[1] = ($$wh[1]+1) % @{$$wh[0]};
    defined $wheels{$value} ? get_next($value,%wheels) : $value;
}

sub spin_wheels {
    my(%wheels) = @_;
    say "$_: " . join ', ', @{${$wheels{$_}}[0]} for sort keys %wheels;
    print get_next('A', %wheels) . ' ' for 1..20; print "\n\n";
}

spin_wheels(%$_) for
(
 {'A' => [['1', '2', '3'], 0]},
 {'A' => [['1', 'B', '2'], 0], 'B' => [['3', '4'], 0]},
 {'A' => [['1', 'D', 'D'], 0], 'D' => [['6', '7', '8'], 0]},
 {'A' => [['1', 'B', 'C'], 0], 'B' => [['3', '4'], 0], 'C' => [['5', 'B'], 0]},
);
```

{{out}}

```txt
A: 1, 2, 3
1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2

A: 1, B, 2
B: 3, 4
1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3

A: 1, D, D
D: 6, 7, 8
1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6

A: 1, B, C
B: 3, 4
C: 5, B
1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4
```



## Perl 6

A succinct Perl 6 example using a few additional language features. Wheels are implemented as infinite repeating sequences, allowing a single iterator to keep track of the current position. This means the code contains no position tracking whatsoever.

```perl6

#| advance rotates a named wheel $n by consuming an item from an infinite sequence. It is called
#| from within a gather block and so can use take in order to construct an infinite, lazy sequence
#| of result values
sub advance($g, $n) {
	given $g{$n}.pull-one {
		when /\d/ { take $_ }
		default   { samewith $g, $_ } # samewith re-calls this function with new parameters
	}
}

#| Input groups are a hash containing each wheel name as the key, and a list of values constructed
#| using <> to split on whitespace. They are transformed using xx * to repeat the list infinitely.
#| We then retrieve the underlying iterator in order for wheel position to be persistent. Each group
#| is then aggregated into a lazy output sequence using an infinite loop inside a gather block.
[
	{A => <1 2 3>},
	{A => <1 B 2>, B => <3 4>},
	{A => <1 D D>, D => <6 7 8>},
	{A => <1 B C>, B => <3 4>, C => <5 B>},
]
	#| %() converts a list of pairs produced by map into a hash. $^k and $^v are implicit variables.
	#| They are processed in alphabetical order and make the block arity 2, called with two vars.
	#| .kv gets the list of wheel names and wheel values from the input entry
	==> map({ %(.kv.map: { $^k => (|$^v xx *).iterator }) })
	#| gather constructs a lazy sequence, in which we infinitely loop advancing wheel A
	==> map({ gather { loop { advance $_, 'A' }} })
	#| state variables are only initialised once, and are kept between invocations.
	==> map({ state $i = 1; say "Group {$i++}, First 20 values: $_[^20]" })

```
{{Output}}

```txt

Group 1, First 20 values: 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
Group 2, First 20 values: 1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3
Group 3, First 20 values: 1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6
Group 4, First 20 values: 1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4

```



## Python


### Python: Original class and generator based


```python
from itertools import islice

class INW():
    """
    Intersecting Number Wheels
    represented as a dict mapping
    name to tuple of values.
    """

    def __init__(self, **wheels):
        self._wheels = wheels
        self.isect = {name: self._wstate(name, wheel)
                      for name, wheel in wheels.items()}

    def _wstate(self, name, wheel):
        "Wheel state holder"
        assert all(val in self._wheels for val in wheel if type(val) == str), \
               f"ERROR: Interconnected wheel not found in {name}: {wheel}"
        pos = 0
        ln = len(wheel)
        while True:
            nxt, pos = wheel[pos % ln], pos + 1
            yield next(self.isect[nxt]) if type(nxt) == str else nxt

    def __iter__(self):
        base_wheel_name = next(self.isect.__iter__())
        yield from self.isect[base_wheel_name]

    def __repr__(self):
        return f"{self.__class__.__name__}({self._wheels})"

    def __str__(self):
        txt = "Intersecting Number Wheel group:"
        for name, wheel in self._wheels.items():
            txt += f"\n  {name+':':4}" + ' '.join(str(v) for v in wheel)
        return txt

def first(iter, n):
    "Pretty print first few terms"
    return ' '.join(f"{nxt}" for nxt in islice(iter, n))

if __name__ == '__main__':
    for group in[
      {'A': (1, 2, 3)},
      {'A': (1, 'B', 2),
       'B': (3, 4)},
      {'A': (1, 'D', 'D'),
       'D': (6, 7, 8)},
      {'A': (1, 'B', 'C'),
       'B': (3, 4),
       'C': (5, 'B')}, # 135143145...
     ]:
        w = INW(**group)
        print(f"{w}\n  Generates:\n    {first(w, 20)} ...\n")
```


{{out}}

```txt
Intersecting Number Wheel group:
  A:  1 2 3
  Generates:
    1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Intersecting Number Wheel group:
  A:  1 B 2
  B:  3 4
  Generates:
    1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Intersecting Number Wheel group:
  A:  1 D D
  D:  6 7 8
  Generates:
    1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Intersecting Number Wheel group:
  A:  1 B C
  B:  3 4
  C:  5 B
  Generates:
    1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...
```



### Python: Simplified procedural


```python
def nextfrom(w, name):
    while True:
        nxt, w[name] = w[name][0], w[name][1:] + w[name][:1]
        if '0' <= nxt[0] <= '9':
            return nxt
        name = nxt

if __name__ == '__main__':
    for group in '''
A: 1 2 3
A: 1 B 2; B: 3 4
A: 1 D D; D: 6 7 8
A: 1 B C; B: 3 4; C: 5 B'''.strip().split('\n'):
        print(f"Intersecting Number Wheel group:\n  {group}")
        wheel, first = {}, None
        for w in group.strip().split(';'):
            name, *values = w.strip().split()
            wheel[name[:-1]] = values
            first = name[:-1] if first is None else first
        gen = ' '.join(nextfrom(wheel, first) for i in range(20))
        print(f"  Generates:\n    {gen} ...\n")
```


{{out}}

```txt
Intersecting Number Wheel group:
  A: 1 2 3
  Generates:
    1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Intersecting Number Wheel group:
  A: 1 B 2; B: 3 4
  Generates:
    1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Intersecting Number Wheel group:
  A: 1 D D; D: 6 7 8
  Generates:
    1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Intersecting Number Wheel group:
  A: 1 B C; B: 3 4; C: 5 B
  Generates:
    1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...
```




;And Again:
This time the <code>nextfromr</code> function is recursive and it will only work for single character names and numbers due to character string rotation being used.

Input is just a list of Python dicts, and depends on c-python dicts being odered by key insertion order.


```python
def nextfromr(w, name):
    nxt, w[name] = w[name][0], w[name][1:] + w[name][:1]
    return nxt if '0' <= nxt[0] <= '9' else nextfromr(w, nxt)

if __name__ == '__main__':
    for group in [{'A': '123'},
                  {'A': '1B2', 'B': '34'},
                  {'A': '1DD', 'D': '678'},
                  {'A': '1BC', 'B': '34', 'C': '5B'},]:
        print(f"Intersecting Number Wheel group:\n  {group}")
        first = next(group.__iter__())
        gen = ' '.join(nextfromr(group, first) for i in range(20))
        print(f"  Generates:\n   {gen} ...\n")
```


{{out}}

```txt
Intersecting Number Wheel group:
  {'A': '123'}
  Generates:
   1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 ...

Intersecting Number Wheel group:
  {'A': '1B2', 'B': '34'}
  Generates:
   1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3 ...

Intersecting Number Wheel group:
  {'A': '1DD', 'D': '678'}
  Generates:
   1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6 ...

Intersecting Number Wheel group:
  {'A': '1BC', 'B': '34', 'C': '5B'}
  Generates:
   1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4 ...
```



### Python: Functional composition

Defining a unit rotation of the wheel-set as a recursive descent, and taking
a map-accumulation of this recursion
over a list of specific length and arbitrary content.
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Intersecting number wheels'''

from functools import reduce
from itertools import cycle, islice


# clockWorkTick :: Dict -> (Dict, Char)
def clockWorkTick(wheelMap):
    '''The new state of the wheels, tupled with a
       digit found by recursive descent from a single
       click of the first wheel.'''
    def click(wheels):
        def go(wheelName):
            wheel = wheels.get(wheelName, ['?'])
            v = wheel[0]
            return (Tuple if v.isdigit() or '?' == v else click)(
                insertDict(wheelName)(leftRotate(wheel))(wheels)
            )(v)
        return lambda name: go(name)
    return click(wheelMap)('A')


# leftRotate :: [a] -> String
def leftRotate(xs):
    ''' A string shifted cyclically towards
        the left by one position.
    '''
    return ''.join(islice(cycle(xs), 1, 1 + len(xs)))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''First twenty values from each set of test wheels.'''

    wheelMaps = [dict(kvs) for kvs in [
        [('A', "123")],
        [('A', "1B2"), ('B', "34")],
        [('A', "1DD"), ('D', "678")],
        [('A', "1BC"), ('B', "34"), ('C', "5B")]
    ]]
    print('New state of wheel sets, after 20 clicks of each:\n')
    for wheels, series in [
            mapAccumL(compose(const)(clockWorkTick))(
                dct
            )(' ' * 20) for dct in wheelMaps
    ]:
        print((wheels, ''.join(series)))

    print('\nInital states:')
    for x in wheelMaps:
        print(x)


# GENERIC -------------------------------------------------

# Tuple (,) :: a -> b -> (a, b)
def Tuple(x):
    '''Constructor for a pair of values,
       possibly of two different types.
    '''
    return lambda y: (
        x + (y,)
    ) if isinstance(x, tuple) else (x, y)


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# const :: a -> b -> a
def const(k):
    '''The latter of two arguments,
       with the first discarded.
    '''
    return lambda _: k


# insertDict :: String -> a -> Dict -> Dict
def insertDict(k):
    '''A dictionary updated with a (k, v) pair.'''
    def go(v, dct):
        dup = dict(dct)
        dup.update({k: v})
        return dup
    return lambda v: lambda dct: go(v, dct)


# mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
def mapAccumL(f):
    '''A tuple of an accumulation and a list derived by a
       combined map and fold,
       with accumulation from left to right.
    '''
    def go(a, x):
        tpl = f(a[0])(x)
        return (tpl[0], a[1] + [tpl[1]])
    return lambda acc: lambda xs: (
        reduce(go, xs, (acc, []))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
New state of wheel sets, after 20 clicks of each:

({'A': '312'}, '12312312312312312312')
({'A': '21B', 'B': '43'}, '13214213214213214213')
({'A': 'D1D', 'D': '786'}, '16718617816718617816')
({'A': 'C1B', 'B': '34', 'C': '5B'}, '13514314513413514314')

Inital states:
{'A': '123'}
{'A': '1B2', 'B': '34'}
{'A': '1DD', 'D': '678'}
{'A': '1BC', 'B': '34', 'C': '5B'}
```



## REXX

Quite a bit of the REXX code deals with detecting of errors   (and issuing error messages)   in the specification and

generation/construction of the wheel sets.

This REXX program uses   ''numbers''   (any form),   not   ''digits''   (for the values on the wheels).

```rexx
/*REXX program  expresses numbers  from  intersecting number wheels  (or wheel sets).   */
@.=                                              /*initialize array to hold the wheels. */
parse arg lim @.1                                /*obtain optional arguments from the CL*/
if lim='' | lim=","  then lim= 20                /*Not specified?  Then use the default.*/
if @.1='' | @.1=","  then do;  @.1= ' A:  1 2 3 '
                               @.2= ' A:  1 B 2,    B:  3 4 '
                               @.3= ' A:  1 D D,    D:  6 7 8 '
                               @.4= ' A:  1 B C,    B:  3 4,    C:  5 B '
                          end
       do i=1  while @.i\='';  call build        /*construct the wheel set  (gear sets).*/
                               call run          /*execute    "    "    "      "    "   */
       end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
error: say;   say;   say '***error***'  arg(1);   say;   say;   exit 12
isLet: return datatype( arg(1), 'M')  &  length( arg(1) )==1
isNum: return datatype( arg(1), 'N')
/*──────────────────────────────────────────────────────────────────────────────────────*/
build: @wn= 'wheel name';     first=;      @wnnfbac= 'wheel name not followed by a colon:'
       @gn= 'gear name' ;     gear.=;      say copies('═', 79)
       say 'building wheel group for: '    @.i;    wheels= space(@.i);        upper wheels
          do y=1  while wheels\='';  parse var wheels  w gears ',' wheels;    L= length(w)
          if L==2  then do;  !.y= left(w, 1)              /*obtain the 1-char gear name.*/
                             if right(w, 1)\==':'  then call error @wnnfbac  w
                             if \isLet(!.y)        then call error @wn "not a letter:"  w
                        end
                   else                       call error "first token isn't a" @wn':'   w
          if y==1  then first= !.1               /*Is this is the 1st wheel set?  Use it*/
          if first==''                   then call error "no wheel name was specified."
          n= !.y                                 /*obtain the name of the 1st wheel set.*/
          gear.n.0= 1                            /*initialize default 1st gear position.*/
          say '   setting gear.name:'  n   '    gears=' gears
             do g=1  for words(gears);         _= word(gears, g)
             if isNum(_) | isLet(_)  then do;  gear.n.g= _;  iterate;  end
             call error  @gn  "isn't a number or a gear name:"  _
             end   /*g*/
          end      /*y*/;        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
run: say;       say center(' running the wheel named '   first" ", 79, "─");           $=
        do #=0  by 0  until words($)==lim;           n= first
        z= gear.n.0;               x= gear.n.z;      z= z + 1
        gear.n.0= z;      if gear.n.z==''  then gear.n.0= 1
        if isNum(x)  then do;      $= $ x;    iterate;    end  /*found a number, use it.*/
        xx= x                                  /*different gear, keep switching until #.*/
           do forever;            nn= xx
           if gear.nn.0==''  then call error "a gear is using an unknown gear name:"   x
           zz= gear.nn.0;         xx= gear.nn.zz
           zz= zz + 1;  gear.nn.0= zz;  if gear.nn.zz==''  then gear.nn.0= 1
           if isNum(xx)  then do; $= $ xx;  iterate #;  end
           end                                 /* [↑]  found a number,  now use  FIRST. */
        end   /*until*/
     say '('lim "results): " strip($);         say;          say;          return
```

{{out|output|text=  when using the default inputs:}}

```txt

═══════════════════════════════════════════════════════════════════════════════
building wheel group for:   A:  1 2 3
   setting gear.name: A     gears= 1 2 3

───────────────────────── running the wheel named  A ──────────────────────────
(20 results):  1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2


═══════════════════════════════════════════════════════════════════════════════
building wheel group for:   A:  1 B 2,    B:  3 4
   setting gear.name: A     gears= 1 B 2
   setting gear.name: B     gears= 3 4

───────────────────────── running the wheel named  A ──────────────────────────
(20 results):  1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3


═══════════════════════════════════════════════════════════════════════════════
building wheel group for:   A:  1 D D,    D:  6 7 8
   setting gear.name: A     gears= 1 D D
   setting gear.name: D     gears= 6 7 8

───────────────────────── running the wheel named  A ──────────────────────────
(20 results):  1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6


═══════════════════════════════════════════════════════════════════════════════
building wheel group for:   A:  1 B C,    B:  3 4,    C:  5 B
   setting gear.name: A     gears= 1 B C
   setting gear.name: B     gears= 3 4
   setting gear.name: C     gears= 5 B

───────────────────────── running the wheel named  A ──────────────────────────
(20 results):  1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4

```



## zkl


```zkl
fcn intersectingNumberWheelsW(wheels){ // ("A":(a,b,"C"), "C":(d,e) ...)
   ws:=wheels.pump(Dictionary(),fcn([(k,v)]){ return(k,Walker.cycle(v)) });  // new Dictionary
   Walker.zero().tweak(fcn(w,wheels){
      while(1){
	 w=wheels[w].next();	// increment wheel w
	 if(Int.isType(w)) return(w);
      }
   }.fp("A",ws))	// assume wheel A exists and is always first
}
```


```zkl
wheelSets:=T( Dictionary("A",T(1,2,3)),
	      Dictionary("A",T(1,"B",2),   "B",T(3,4)),
	      Dictionary("A",T(1,"D","D"), "D",T(6,7,8)),
	      Dictionary("A",T(1,"B","C"), "C",T(5,"B"),  "B",T(3,4)) );
foreach ws in (wheelSets){
   println("Wheel set:");
   ws.pump(String,fcn([(k,v)]){ "  %s: %s\n".fmt(k,v.concat(" ")) }).print();
   println("-->",intersectingNumberWheelsW(ws).walk(20).concat(" "));
}
```

{{out}}

```txt

Wheel set:
  A: 1 2 3
-->1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
Wheel set:
  A: 1 B 2
  B: 3 4
-->1 3 2 1 4 2 1 3 2 1 4 2 1 3 2 1 4 2 1 3
Wheel set:
  A: 1 D D
  D: 6 7 8
-->1 6 7 1 8 6 1 7 8 1 6 7 1 8 6 1 7 8 1 6
Wheel set:
  A: 1 B C
  C: 5 B
  B: 3 4
-->1 3 5 1 4 3 1 4 5 1 3 4 1 3 5 1 4 3 1 4

```

