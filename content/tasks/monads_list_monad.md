+++
title = "Monads/List monad"
description = ""
date = 2019-10-03T18:54:01Z
aliases = []
[extra]
id = 20014
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Monad_(functional_programming)|Monad]] is a combination of a data-type with two helper functions written for that type. 

The data-type can be of any kind which can contain values of some other type – common examples are lists, records, sum-types, even functions or IO streams. The two special functions, mathematically known as '''eta''' and '''mu''', but usually given more expressive names like 'pure' or 'return', and 'bind', abstract away some boilerplate needed for pipe-lining or enchaining sequences of computations on values held in the containing data-type.

The bind operator in the List monad enchains computations which return their values wrapped in lists. One application of this is the representation of indeterminacy, with returned lists representing a set of possible values. An empty list can be returned to express incomputability, or computational failure.

A sequence of two list monad computations (enchained with the use of bind) can be understood as the computation of a cartesian product. 

The natural implementation of bind for the List monad is a composition of '''concat''' and '''map''', which, used with a function which returns its value as a (possibly empty) list, provides for filtering in addition to transformation or mapping.


Demonstrate in your programming language the following:

#Construct a List Monad by writing the 'bind' function and the 'pure' (sometimes known as 'return') function for that Monad (or just use what the language already has implemented)
#Make two functions, each which take a number and return a monadic number, e.g. Int -> List Int and Int -> List String
#Compose the two functions with bind



## AppleScript

We can use a list monad in AppleScript to express set comprehension for the Pythagorean triples, but the lack of nestable first class (and anonymous) functions means that the closure can only be achieved using script objects, which makes the idiom rather less direct and transparent. AppleScript is creaking at the seams here.

```AppleScript
-- MONADIC FUNCTIONS (for list monad) ------------------------------------------

-- Monadic bind for lists is simply ConcatMap
-- which applies a function f directly to each value in the list,
-- and returns the set of results as a concat-flattened list

-- bind :: (a -> [b]) -> [a] -> [b]
on bind(f, xs)
    -- concat :: a -> a -> [a]
    script concat
        on |λ|(a, b)
            a & b
        end |λ|
    end script
    
    foldl(concat, {}, map(f, xs))
end bind

-- Monadic return/unit/inject for lists: just wraps a value in a list
-- a -> [a]
on unit(a)
    [a]
end unit

-- TEST ------------------------------------------------------------------------
on run
    -- Pythagorean triples drawn from integers in the range [1..n]
    -- {(x, y, z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], (x^2 + y^2 = z^2)}
    
    pythagoreanTriples(25)
    
    --> {{3, 4, 5}, {5, 12, 13}, {6, 8, 10}, {7, 24, 25}, {8, 15, 17}, 
    --   {9, 12, 15}, {12, 16, 20}, {15, 20, 25}}
    
end run

-- pythagoreanTriples :: Int -> [(Int, Int, Int)]
on pythagoreanTriples(maxInteger)
    script X
        on |λ|(X)
            script Y
                on |λ|(Y)
                    script Z
                        on |λ|(Z)
                            if X * X + Y * Y = Z * Z then
                                unit([X, Y, Z])
                            else
                                []
                            end if
                        end |λ|
                    end script
                    
                    bind(Z, enumFromTo(1 + Y, maxInteger))
                end |λ|
            end script
            
            bind(Y, enumFromTo(1 + X, maxInteger))
        end |λ|
    end script
    
    bind(X, enumFromTo(1, maxInteger))
    
end pythagoreanTriples


-- GENERIC  FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

```AppleScript
```



## Clojure



```clojure

(defn bind [coll f] (apply vector (mapcat f coll)))
(defn unit [val] (vector val))

(defn doubler [n] [(* 2 n)])   ; takes a number and returns a List number
(def vecstr (comp vector str)) ; takes a number and returns a List string

(bind (bind (vector 3 4 5) doubler) vecstr) ; evaluates to ["6" "8" "10"]
(-> [3 4 5]
  (bind doubler)
  (bind vecstr)) ; also evaluates to ["6" "8" "10"]

```



## EchoLisp

Our monadic lists will take the form (List a b c ...), ie raw lists prefixed by the List symbol.

```scheme

;; -> and ->> are the pipeline operators
;; (-> x f g h) = (h (g ( f x)))
;; (->> x f (g a) h) = (h (g a ( f x)))

(define (List.unit elem)  (append '(List) elem))
(define (List.bind xs f)  (List.unit (->> xs rest (map f)  (map rest) (apply append))))
(define (List.lift f)     (lambda(elem) (List.unit (f elem))))

(define List.square  (List.lift (lambda(x) (*  x x))))
(define List.cube    (List.lift (lambda(x) (* x x x))))
(define List.tostr   (List.lift number->string))

;; composition

(-> '(List 1 -2 3 -5) (List.bind List.cube) (List.bind List.tostr))
    → (List "1" "-8" "27" "-125")
;; or
(-> '(1 -2 3 -5) List.unit (List.bind List.cube) (List.bind List.tostr))
     → (List "1" "-8" "27" "-125")

```



## Factor

Factor comes with an implementation of Haskell-style monads in the <code>monads</code> vocabulary.

```factor
USING: kernel math monads prettyprint ;
FROM: monads => do ;

{ 3 4 5 }
>>= [ 1 + array-monad return ] swap call
>>= [ 2 * array-monad return ] swap call .
```

Or:

```factor
{ 3 4 5 }
[ 1 + array-monad return ] bind
[ 2 * array-monad return ] bind .
```

Or:

```factor
{
    [ { 3 4 5 } ]
    [ 1 + array-monad return ]
    [ 2 * array-monad return ]
} do .
```

```txt

{ 8 10 12 }

```



## Go


```go
package main

import "fmt"

type mlist struct{ value []int }

func (m mlist) bind(f func(lst []int) mlist) mlist {
    return f(m.value)
}

func unit(lst []int) mlist {
    return mlist{lst}
}

func increment(lst []int) mlist {
    lst2 := make([]int, len(lst))
    for i, v := range lst {
        lst2[i] = v + 1
    }
    return unit(lst2)
}

func double(lst []int) mlist {
    lst2 := make([]int, len(lst))
    for i, v := range lst {
        lst2[i] = 2 * v
    }
    return unit(lst2)
}

func main() {
    ml1 := unit([]int{3, 4, 5})
    ml2 := ml1.bind(increment).bind(double)
    fmt.Printf("%v -> %v\n", ml1.value, ml2.value)
}
```


```txt

[3 4 5] -> [8 10 12]

```



## Haskell


Haskell has the built-in <code>Monad</code> type class, and the built-in list type already conforms to the <code>Monad</code> type class.

```haskell
main = print $ [3,4,5] >>= (return . (+1)) >>= (return . (*2)) -- prints [8,10,12]
```


Or, written using <code>do</code> notation:

```haskell
main = print $ do x <- [3,4,5]
                  y <- return (x+1)
                  z <- return (y*2)
                  return z
```


Or alternately:

```haskell
main = print $ do x <- [3,4,5]
                  let y = x+1
                  let z = y*2
                  return z
```


Using the list monad to express set comprehension for Pythagorean triples:

```haskell
pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n =
  [1 .. n] >>= (\x ->
  [x+1 .. n] >>= (\y ->
  [y+1 .. n] >>= (\z ->
  if x^2 + y^2 == z^2 then return (x,y,z) else [])))

main = print $ pythagoreanTriples 25
```

```txt
[(3,4,5),(5,12,13),(6,8,10),(7,24,25),(8,15,17),(9,12,15),(12,16,20),(15,20,25)]
```


Which can be written using <code>do</code> notation:

```haskell
pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n = do x <- [1 .. n]
                          y <- [x+1 .. n]
                          z <- [y+1 .. n]
                          if x^2 + y^2 == z^2 then return (x,y,z) else []
```


Or directly as a list comprehension:

```haskell
pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n = [(x,y,z) | x <- [1 .. n], y <- [x+1 .. n], z <- [y+1 .. n], x^2 + y^2 == z^2]
```



## J


Note that J documentation mentions "monad" but that is an [[wp:Monad_(linear_algebra)|older]] ([[wp:Monad_(music)|much older]]) use of the term from what is intended here. J documentation uses "box" <code><</code>to describe the operation mentioned here.

That said, here is an implementation which might be adequate for the current task description:


```J
bind=: S:0
unit=: boxopen

m_num=: unit
m_str=: unit@":
```


Task example:


```J
   m_str bind m_num 5
┌─┐
│5│
└─┘
```



## Javascript



```javascript

Array.prototype.bind = function (func) {
  return this.map(func).reduce(function (acc, a) { return acc.concat(a); });
}

Array.unit = function (elem) {
  return [elem];
}

Array.lift = function (func) {
  return function (elem) { return Array.unit(func(elem)); };
}

inc = function (n) { return n + 1; }
doub = function (n) { return 2 * n; }
listy_inc = Array.lift(inc);
listy_doub = Array.lift(doub);

[3,4,5].bind(listy_inc).bind(listy_doub); // [8, 10, 12]

```



ES5 Example: Using the list monad to express set comprehension


```JavaScript
(function (n) {

    // ENCODING A SET COMPREHENSION IN TERMS OF A LIST MONAD

    // Pythagorean triples drawn from integers in the range [1..25]


    // Each range of integers here represents the set of possible values for the variable.
    // Where the test returns true for a particular [x, y, z] triple, we return that triple
    // to the expected data type, wrapping it using the unit or return function;

    // Where the test returns false, we return the empty list, which vanishes from the 
    // results set under concatenation, giving us a convenient encoding of filtering.

    // {(x, y, z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], (x^2 + y^2 = z^2)} 

    return bind(rng(1,     n), function (x) {
    return bind(rng(1 + x, n), function (y) {
    return bind(rng(1 + y, n), function (z) {

        return (x * x + y * y === z * z) ? unit([x, y, z]) : [];

    })})});


    // Monadic return/unit/inject for lists just wraps a value in a list
    // a -> [a]
    function unit(a) {
        return [a];
    }

    // Bind for lists is simply ConcatMap
    // which applies a function f directly to each value in the list,
    // and returns the set of results as a concat-flattened list
    // [a] -> (a -> [b]) -> [b]
    function bind(xs, f) {
        return [].concat.apply([], xs.map(f));
    }



    // we will need some ranges of integers, each expressing a range of possible values
    // [m..n]
    function rng(m, n) {
        return Array.apply(null, Array(n - m + 1))
            .map(function (x, i) {
                return m + i;
            });
    }

})(25);
```


```txt
[[3, 4, 5], [5, 12, 13], [6, 8, 10], [7, 24, 25], [8, 15, 17], [9, 12, 15], [12, 16, 20], [15, 20, 25]]
```




## Julia

Julia uses the function bind for binding a channel to a task, but this can be imported and overloaded. 
The |> syntax in Julia can also be used to chain functions taking one argument.

```julia>julia
 unit(v) = [v...]
unit (generic function with 1 method)

julia> import Base.bind

julia> bind(v, f) = f.(v)
bind (generic function with 5 methods)

julia> f1(x) = x + 1
f1 (generic function with 1 method)

julia> f2(x) = 2x
f2 (generic function with 1 method)

julia> bind(bind(unit([2, 3, 4]), f1), f2)
3-element Array{Int64,1}:
  6
  8
 10

julia> unit([2, 3, 4]) .|> f1 .|> f2
3-element Array{Int64,1}:
  6
  8
 10

```



## Kotlin


```scala
// version 1.2.10

class MList<T : Any> private constructor(val value: List<T>) {
    fun <U : Any> bind(f: (List<T>) -> MList<U>) = f(this.value)

    companion object {
        fun <T : Any> unit(lt: List<T>) = MList<T>(lt)
    }
}

fun doubler(li: List<Int>) = MList.unit(li.map { 2 * it } )

fun letters(li: List<Int>) = MList.unit(li.map { "${('@' + it)}".repeat(it) } )

fun main(args: Array<String>) {
    val iv = MList.unit(listOf(2, 3, 4))
    val fv = iv.bind(::doubler).bind(::letters)
    println(fv.value)
}
```


```txt

[DDDD, FFFFFF, HHHHHHHH]

```



## Perl

With the help of the CPAN module <code>Data::Monad</code>, we can work with list monads.

```perl
use strict;
use feature 'say';
use Data::Monad::List;

# Cartesian product to 'count' in binary
my @cartesian = [(
    list_flat_map_multi { scalar_list(join '', @_) }
        scalar_list(0..1),
        scalar_list(0..1),
        scalar_list(0..1)
)->scalars];
say join "\n", @{shift @cartesian};

say '';

# Pythagorean triples
my @triples = [(
    list_flat_map_multi { scalar_list(
            { $_[0] < $_[1] && $_[0]**2+$_[1]**2 == $_[2]**2 ? join(',',@_) : () }
        ) }
        scalar_list(1..10),
        scalar_list(1..10),
        scalar_list(1..10)
)->scalars];

for (@{shift @triples}) {
    say keys %$_ if keys %$_;
}
```

```txt
000
001
010
011
100
101
110
111

3,4,5
6,8,10
```



## Perl 6

Perl 6 does not have Monad types built in but they can be emulated/implemented without a great deal of difficulty. List Monads especially are of questionable utility in Perl 6. Most item types and Listy types have a Cool role in Perl 6. (Cool being a play on the slang term "cool" as in: "That's cool with me." (That's ok with me). So Ints are pretty much treated like one item lists for operators that work with lists. ("I work on a list." "Here's an Int." "Ok, that's cool.") Explicitly wrapping an Int into a List is worse than useless. It won't do anything Perl 6 can't do natively, and will likely '''remove''' some functionality that it would normally have. That being said, just because it is a bad idea (in Perl 6) doesn't mean it can't be done.

In Perl 6, bind is essentially map. I'll shadow map here but again, it '''removes''' capability, not adds it. Perl 6 also provided "hyper" operators which will descend into data structures and apply an operator / function to each member of that data structure.

Here's a simple, if contrived example. take the numbers from 0 to 9, add 3 to each, find the divisors of those sums and print the list of divisors for each sum... in base 2. Again, a bind function was implemented but it is more limited than if we just used map directly. The built in map method will work with either items or lists, here we need to implement a multi sub to handle either.

The * in the bind blocks are typically referred to as "whatever"; whatever + 3 etc. The guillemot (») is the hyper operator; descend into the data structure and apply the following operator/function to each member.

```perl6
multi bind (@list, &code) { @list.map: &code };

multi bind ($item, &code) { $item.&code };

sub divisors (Int $int) { gather for 1 .. $int { .take if $int %% $_ } }

put join "\n", (flat ^10).&bind(* + 3).&bind(*.&divisors)».&bind(*.base: 2);
```


```txt
1 11
1 10 100
1 101
1 10 11 110
1 111
1 10 100 1000
1 11 1001
1 10 101 1010
1 1011
1 10 11 100 110 1100
```



## Racket


###  Vanilla Racket 


Note that this also demonstrates how to use Racket's macro system to implement the do syntax.


```racket
#lang racket

(define (bind x f) (append-map f x))
(define return list)
(define ((lift f) x) (list (f x)))

(define listy-inc (lift add1))
(define listy-double (lift (λ (x) (* 2 x))))

(bind (bind '(3 4 5) listy-inc) listy-double)
;; => '(8 10 12)

(define (pythagorean-triples n)
  (bind (range 1 n)
        (λ (x)
          (bind (range (add1 x) n)
                (λ (y)
                  (bind (range (add1 y) n)
                        (λ (z)
                          (if (= (+ (* x x) (* y y)) (* z z))
                              (return (list x y z))
                              '()))))))))

(pythagorean-triples 25)
;; => '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))

(require syntax/parse/define)

(define-syntax-parser do-macro
  [(_ [x {~datum <-} y] . the-rest) #'(bind y (λ (x) (do-macro . the-rest)))]
  [(_ e) #'e])

(define (pythagorean-triples* n)
  (do-macro
   [x <- (range 1 n)]
   [y <- (range (add1 x) n)]
   [z <- (range (add1 y) n)]
   (if (= (+ (* x x) (* y y)) (* z z))
       (return (list x y z))
       '())))

(pythagorean-triples* 25)
;; => '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))
```



###  With functional package 
 

The [https://docs.racket-lang.org/functional/interfaces.html functional] package has already implemented the list monad.


```racket
#lang racket

(require data/monad 
         data/applicative)

(define (pythagorean-triples n)
  (sequence->list
   (do [x <- (range 1 n)]
       [y <- (range (add1 x) n)]
       [z <- (range (add1 y) n)]
       (if (= (+ (* x x) (* y y)) (* z z))
           (pure (list x y z))
           '()))))

(pythagorean-triples 25)
;; => '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))
```



## Ring


```ring

# Project : Monads/List monad

 func main()
        str = "["
        for x in [3,4,5]
             y = x+1
             z = y*2
             str = str + z + ", " 
        next
        str = left(str, len(str) -2)
        str = str + "]"
        see str + nl

```

Output:

```txt

[8, 10, 12]

```



## Ruby



```ruby

class Array
  def bind(f)
    flat_map(&f)
  end
  def self.unit(*args)
    args
  end
  # implementing lift is optional, but is a great helper method for turning
  # ordinary funcitons into monadic versions of them.
  def self.lift(f)
    -> e { self.unit(f[e]) }
  end
end

inc = -> n { n + 1 }
str = -> n { n.to_s }
listy_inc = Array.lift(inc)
listy_str = Array.lift(str)

Array.unit(3,4,5).bind(listy_inc).bind(listy_str) #=> ["4", "5", "6"]

# Note that listy_inc and listy_str cannot be composed directly,
# as they don't have compatible type signature.
# Due to duck typing (Ruby will happily turn arrays into strings),
#   in order to show this, a new function will have to be used:

doub = -> n { 2*n }
listy_doub = Array.lift(doub)
[3,4,5].bind(listy_inc).bind(listy_doub) #=> [8, 10, 12]

# Direct composition will cause a TypeError, as Ruby cannot evaluate 2*[4, 5, 6]
# Using bind with the composition is *supposed* to fail, no matter the programming language.
comp = -> f, g {-> x {f[g[x]]}}
[3,4,5].bind(comp[listy_doub, listy_inc]) #=> TypeError: Array can't be coerced into Fixnum

# Composition needs to be defined in terms of bind
class Array
  def bind_comp(f, g)
    bind(g).bind(f)
  end
end

[3,4,5].bind_comp(listy_doub, listy_inc) #=> [8, 10, 12]

```



## zkl

While I'm unsure of the utility of Monads in a dynamic type-less language, it can be done.

Here we create a class to do Monad like things. Unlike Ruby, we can't augment the baked in List/Array object so this more verbose. Also unlike Ruby, we can directly compose as we are applying the composition to each element (vs the list-as-object).

```zkl
class MList{
   fcn init(xs){ var list=vm.arglist }
   fcn bind(f) { list=list.apply(f); self }
   fcn toString{ list.toString() }
}
```


```zkl
inc:=Op("+",1);  // '+(1)
str:="toString";
MList(3,4,5).bind(inc).bind(str).println(" == (4,5,6)");

doub:=Op("*",2);
MList(3,4,5).bind(inc).bind(doub).println(" == (8,10,12)");

comp:=Utils.Helpers.fcomp;  // comp(f,g) == f.g == f(g(x))
MList(3,4,5).bind(comp(doub,inc)).println(" == (8,10,12)");
```

```txt

L("4","5","6") == (4,5,6)
L(8,10,12) == (8,10,12)
L(8,10,12) == (8,10,12)

```

