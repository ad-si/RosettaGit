+++
title = "Combinations with repetitions"
description = ""
date = 2019-06-05T10:59:23Z
aliases = []
[extra]
id = 8749
[taxonomies]
categories = []
tags = []
+++

{{task|Discrete math}}

The set of combinations with repetitions is computed from a set, <math>S</math> (of cardinality <math>n</math>), and a size of resulting selection, <math>k</math>, by reporting the sets of cardinality <math>k</math> where each member of those sets is chosen from <math>S</math>.
In the real world, it is about choosing sets where there is a “large” supply of each type of element and where the order of choice does not matter.
For example:
:Q: How many ways can a person choose two doughnuts from a store selling three types of doughnut: iced, jam, and plain? (i.e., <math>S</math> is <math>\{\mathrm{iced}, \mathrm{jam}, \mathrm{plain}\}</math>, <math>|S| = 3</math>, and <math>k = 2</math>.)

:A: 6: {iced, iced}; {iced, jam}; {iced, plain}; {jam, jam}; {jam, plain}; {plain, plain}.

<small>Note that both the order of items within a pair, and the order of the pairs given in the answer is not significant; the pairs represent multisets.</small>

<small>Also note that ''doughnut'' can also be spelled ''donut''.</small>


;Task:
* Write a function/program/routine/.. to generate all the combinations with repetitions of <math>n</math> types of things taken <math>k</math> at a time and use it to ''show'' an answer to the doughnut example above.
* For extra credit, use the function to compute and show ''just the number of ways'' of choosing three doughnuts from a choice of ten types of doughnut. Do not show the individual choices for this part.


;References:
* [[wp:Combination|k-combination with repetitions]]


;See also:
{{Template:Combinations and permutations}}





## Ada

Should work for any discrete type: integer, modular, or enumeration.

combinations.adb:

```Ada
with Ada.Text_IO;
procedure Combinations is

   generic
      type Set is (<>);
   function Combinations
     (Count  : Positive;
      Output : Boolean := False)
      return   Natural;

   function Combinations
     (Count  : Positive;
      Output : Boolean := False)
      return   Natural
   is
      package Set_IO is new Ada.Text_IO.Enumeration_IO (Set);
      type Set_Array is array (Positive range <>) of Set;
      Empty_Array : Set_Array (1 .. 0);
      function Recurse_Combinations
        (Number : Positive;
         First  : Set;
         Prefix : Set_Array)
         return   Natural
      is
         Combination_Count : Natural := 0;
      begin
         for Next in First .. Set'Last loop
            if Number = 1 then
               Combination_Count := Combination_Count + 1;
               if Output then
                  for Element in Prefix'Range loop
                     Set_IO.Put (Prefix (Element));
                     Ada.Text_IO.Put ('+');
                  end loop;
                  Set_IO.Put (Next);
                  Ada.Text_IO.New_Line;
               end if;
            else
               Combination_Count := Combination_Count +
                                    Recurse_Combinations
                                       (Number - 1,
                                        Next,
                                        Prefix & (1 => Next));
            end if;
         end loop;
         return Combination_Count;
      end Recurse_Combinations;
   begin
      return Recurse_Combinations (Count, Set'First, Empty_Array);
   end Combinations;

   type Donuts is (Iced, Jam, Plain);
   function Donut_Combinations is new Combinations (Donuts);

   subtype Ten is Positive range 1 .. 10;
   function Ten_Combinations is new Combinations (Ten);

   Donut_Count : constant Natural :=
      Donut_Combinations (Count => 2, Output => True);
   Ten_Count   : constant Natural := Ten_Combinations (Count => 3);
begin
   Ada.Text_IO.Put_Line ("Total Donuts:" & Natural'Image (Donut_Count));
   Ada.Text_IO.Put_Line ("Total Tens:" & Natural'Image (Ten_Count));
end Combinations;
```


{{out}}

```txt
ICED+ICED
ICED+JAM
ICED+PLAIN
JAM+JAM
JAM+PLAIN
PLAIN+PLAIN
Total Donuts: 6
Total Tens: 220
```


## AppleScript

{{Trans|Haskell}}
{{Trans|Python}}

```applescript
-- combsWithRep :: Int -> [a] -> [kTuple a]
on combsWithRep(k, xs)
    -- A list of lists, representing
    -- sets of cardinality k, with
    -- members drawn from xs.

    script combsBySize
        script f
            on |λ|(a, x)
                script prefix
                    on |λ|(z)
                        {x} & z
                    end |λ|
                end script

                script go
                    on |λ|(ys, xs)
                        xs & map(prefix, ys)
                    end |λ|
                end script
                scanl1(go, a)
            end |λ|
        end script

        on |λ|(xs)
            foldl(f, {{{}}} & take(k, |repeat|({})), xs)
        end |λ|
    end script

    |Just| of |index|(|λ|(xs) of combsBySize, 1 + k)
end combsWithRep


-- TEST ---------------------------------------------------
on run
    {length of combsWithRep(3, enumFromTo(0, 9)), ¬
        combsWithRep(2, {"iced", "jam", "plain"})}
end run


-- GENERIC ------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- enumFromTo :: (Int, Int) -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
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

-- index (!!) :: [a] -> Int -> Maybe a
-- index (!!) :: Gen [a] -> Int -> Maybe a
-- index (!!) :: String -> Int -> Maybe Char
on |index|(xs, i)
    if script is class of xs then
        repeat with j from 1 to i
            set v to |λ|() of xs
        end repeat
        if missing value is not v then
            Just(v)
        else
            Nothing()
        end if
    else
        if length of xs < i then
            Nothing()
        else
            Just(item i of xs)
        end if
    end if
end |index|

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- repeat :: a -> Generator [a]
on |repeat|(x)
    script
        on |λ|()
            return x
        end |λ|
    end script
end |repeat|


-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
on scanl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return lst
    end tell
end scanl

-- scanl1 :: (a -> a -> a) -> [a] -> [a]
on scanl1(f, xs)
    if 0 < length of xs then
        scanl(f, item 1 of xs, rest of xs)
    else
        {}
    end if
end scanl1


-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to |λ|() of xs
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take
```

{{Out}}

```txt
{220, {{"iced", "iced"}, {"jam", "iced"}, {"jam", "jam"}, {"plain", "iced"}, {"plain", "jam"}, {"plain", "plain"}}}
```



## AWK


```AWK

# syntax: GAWK -f COMBINATIONS_WITH_REPETITIONS.AWK
BEGIN {
    n = split("iced,jam,plain",donuts,",")
    for (i=1; i<=n; i++) {
      for (j=1; j<=n; j++) {
        if (donuts[i] < donuts[j]) {
          key = sprintf("%s %s",donuts[i],donuts[j])
        }
        else {
          key = sprintf("%s %s",donuts[j],donuts[i])
        }
        arr[key]++
      }
    }
    cmd = "SORT"
    for (i in arr) {
      printf("%s\n",i) | cmd
      choices++
    }
    close(cmd)
    printf("choices = %d\n",choices)
    exit(0)
}

```

<p>output:</p>

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
choices = 6

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM list$(2), chosen%(2)
      list$() = "iced", "jam", "plain"
      PRINT "Choices of 2 from 3:"
      choices% = FNchoose(0, 2, 0, 3, chosen%(), list$())
      PRINT "Total choices = " ; choices%

      PRINT '"Choices of 3 from 10:"
      choices% = FNchoose(0, 3, 0, 10, chosen%(), nul$())
      PRINT "Total choices = " ; choices%
      END

      DEF FNchoose(n%, l%, p%, m%, g%(), RETURN n$())
      LOCAL i%, c%
      IF n% = l% THEN
        IF !^n$() THEN
          FOR i% = 0 TO n%-1
            PRINT " " n$(g%(i%)) ;
          NEXT
          PRINT
        ENDIF
        = 1
      ENDIF
      FOR i% = p% TO m%-1
        g%(n%) = i%
        c% += FNchoose(n% + 1, l%, i%, m%, g%(), n$())
      NEXT
      = c%
```

{{out}}

```txt

Choices of 2 from 3:
 iced iced
 iced jam
 iced plain
 jam jam
 jam plain
 plain plain
Total choices = 6

Choices of 3 from 10:
Total choices = 220

```



## Bracmat

This minimalist solution expresses the answer as a sum of products. Bracmat automatically normalises such expressions: terms and factors are sorted alphabetically, products containing a sum as a factor are decomposed in a sum of factors (unless the product is not itself term in a multiterm expression). Like factors are converted to a single factor with an appropriate exponent, so <code>ice^2</code> is to be understood as ice twice.

```bracmat
( ( choices
  =   n things thing result
    .   !arg:(?n.?things)
      & ( !n:0&1
        |   0:?result
          & (   !things
              :   ?
                  ( %?`thing ?:?things
                  &   !thing*choices$(!n+-1.!things)+!result
                    : ?result
                  & ~
                  )
            | !result
            )
        )
  )
& out$(choices$(2.iced jam plain))
& out$(choices$(3.iced jam plain butter marmite tahin fish salad onion grass):?+[?N&!N)
);
```

{{out}}

```txt
iced^2+jam^2+plain^2+iced*jam+iced*plain+jam*plain
220
```



## C


```c
#include <stdio.h>

const char * donuts[] = { "iced", "jam", "plain", "something completely different" };
long choose(int * got, int n_chosen, int len, int at, int max_types)
{
        int i;
        long count = 0;
        if (n_chosen == len) {
                if (!got) return 1;

                for (i = 0; i < len; i++)
                        printf("%s\t", donuts[got[i]]);
                printf("\n");
                return 1;
        }

        for (i = at; i < max_types; i++) {
                if (got) got[n_chosen] = i;
                count += choose(got, n_chosen + 1, len, i, max_types);
        }
        return count;
}

int main()
{
        int chosen[3];
        choose(chosen, 0, 2, 0, 3);

        printf("\nWere there ten donuts, we'd have had %ld choices of three\n",
                choose(0, 0, 3, 0, 10));
        return 0;
}

```

{{out}}
```txt
iced    iced
iced    jam
iced    plain
jam     jam
jam     plain
plain   plain

Were there ten donuts, we'd have had 220 choices of three
```



## C++


Non recursive version.

```cpp

#include <cstdio>
#include <vector>
#include <string>

using namespace std;

void print_vector(const vector<int> &v, size_t n, const vector<string> &s){
        for (size_t i = 0; i < n; ++i)
                printf("%s\t", s[v[i]].c_str());
        printf("\n");
}

void combination_with_repetiton(int sabores, int bolas, const vector<string>& v_sabores){
        sabores--;
        vector<int> v(bolas+1, 0);
        while (true){
                for (int i = 0; i < bolas; ++i){                //vai um
                        if (v[i] > sabores){
                                v[i + 1] += 1;
                                for (int k = i; k >= 0; --k){
                                        v[k] = v[i + 1];
                                }
                                //v[i] = v[i + 1];
                        }
                }

                if (v[bolas] > 0)
                        break;
                print_vector(v, bolas, v_sabores);
                v[0] += 1;
        }
}

int main(){
        vector<string> options{ "iced", "jam", "plain" };
        combination_with_repetiton(3, 2, options);
        return 0;
}

```


{{out}}

```txt

iced	iced
jam	iced
plain	iced
jam	jam
plain	jam
plain	plain

```


=={{header|C sharp|C#}}==
{{trans|PHP}}


```csharp

using System;
using System.Collections.Generic;
using System.Linq;

public static class MultiCombinations
{
    private static void Main()
    {
        var set = new List<string> { "iced", "jam", "plain" };
        var combinations = GenerateCombinations(set, 2);

        foreach (var combination in combinations)
        {
            string combinationStr = string.Join(" ", combination);
            Console.WriteLine(combinationStr);
        }

        var donuts = Enumerable.Range(1, 10).ToList();

        int donutsCombinationsNumber = GenerateCombinations(donuts, 3).Count;

        Console.WriteLine("{0} ways to order 3 donuts given 10 types", donutsCombinationsNumber);
    }

    private static List<List<T>> GenerateCombinations<T>(List<T> combinationList, int k)
    {
        var combinations = new List<List<T>>();

        if (k == 0)
        {
            var emptyCombination = new List<T>();
            combinations.Add(emptyCombination);

            return combinations;
        }

        if (combinationList.Count == 0)
        {
            return combinations;
        }

        T head = combinationList[0];
        var copiedCombinationList = new List<T>(combinationList);

        List<List<T>> subcombinations = GenerateCombinations(copiedCombinationList, k - 1);

        foreach (var subcombination in subcombinations)
        {
            subcombination.Insert(0, head);
            combinations.Add(subcombination);
        }

        combinationList.RemoveAt(0);
        combinations.AddRange(GenerateCombinations(combinationList, k));

        return combinations;
    }
}

```

{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
220 ways to order 3 donuts given 10 types

```


Recursive version

```csharp

using System;
class MultiCombination
{
  static string [] set = { "iced", "jam", "plain" };
  static int k = 2, n = set.Length;
  static string [] buf = new string [k];

  static void Main()
  {
    rec(0, 0);
  }

  static void rec(int ind, int begin)
  {
    for (int i = begin; i < n; i++)
    {
      buf [ind] = set[i];
      if (ind + 1 < k) rec(ind + 1, i);
      else Console.WriteLine(string.Join(",", buf));
    }
  }
}


```



## Clojure

{{trans|Scheme}}


```clojure

(defn combinations [coll k]
  (when-let [[x & xs] coll]
    (if (= k 1)
      (map list coll)
      (concat (map (partial cons x) (combinations coll (dec k)))
              (combinations xs k)))))

```


{{out}}

```txt

user> (combinations '[iced jam plain] 2)
((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

```



## CoffeeScript


```coffeescript

combos = (arr, k) ->
  return [ [] ] if k == 0
  return [] if arr.length == 0

  combos_with_head = ([arr[0]].concat combo for combo in combos arr, k-1)
  combos_sans_head = combos arr[1...], k
  combos_with_head.concat combos_sans_head

arr = ['iced', 'jam', 'plain']
console.log "valid pairs from #{arr.join ','}:"
console.log combos arr, 2
console.log "#{combos([1..10], 3).length} ways to order 3 donuts given 10 types"

```


{{out}}

```txt

jam,plain:
[ [ 'iced', 'iced' ],
  [ 'iced', 'jam' ],
  [ 'iced', 'plain' ],
  [ 'jam', 'jam' ],
  [ 'jam', 'plain' ],
  [ 'plain', 'plain' ] ]
220 ways to order 3 donuts given 10 types

```



## Common Lisp

The code below is a modified version of the Clojure solution.

```lisp
(defun combinations (xs k)
  (let ((x (car xs)))
    (cond
     ((null xs) nil)
     ((= k 1) (mapcar #'list xs))
     (t (append (mapcar (lambda (ys) (cons x ys))
			(combinations xs (1- k)))
		(combinations (cdr xs) k))))))

```


{{out}}

```txt
((:ICED :ICED) (:ICED :JAM) (:ICED :PLAIN) (:JAM :JAM) (:JAM :PLAIN) (:PLAIN :PLAIN))

```



## Crystal

{{trans|Ruby}}

```ruby
possible_doughnuts = ["iced", "jam", "plain"].repeated_combinations(2)
puts "There are #{possible_doughnuts.size} possible doughnuts:"
possible_doughnuts.each{|doughnut_combi| puts doughnut_combi.join(" and ")}

# Extra credit
possible_doughnuts = (1..10).to_a.repeated_combinations(3)
# size returns the size of the enumerator, or nil if it can’t be calculated lazily.
puts "", "#{possible_doughnuts.size} ways to order 3 donuts given 10 types."
```

{{out}}

```txt

There are 6 possible doughnuts:
iced and iced
iced and jam
iced and plain
jam and jam
jam and plain
plain and plain

220 ways to order 3 donuts given 10 types.

```



## D

Using [http://www.graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation lexicographic next bit permutation] to generate combinations with repetitions.

```d
import std.stdio, std.range;

const struct CombRep {
    immutable uint nt, nc;
    private const ulong[] combVal;

    this(in uint numType, in uint numChoice) pure nothrow @safe
    in {
        assert(0 < numType && numType + numChoice <= 64,
               "Valid only for nt + nc <= 64 (ulong bit size)");
    } body {
        nt = numType;
        nc = numChoice;
        if (nc == 0)
            return;
        ulong v  = (1UL << (nt - 1)) - 1;

        // Init to smallest number that has nt-1 bit set
        // a set bit is metaphored as a _type_ seperator.
        immutable limit = v << nc;

        ulong[] localCombVal;
        // Limit is the largest nt-1 bit set number that has nc
        // zero-bit a zero-bit means a _choice_ between _type_
        // seperators.
        while (v <= limit) {
            localCombVal ~= v;
            if (v == 0)
                break;
            // Get next nt-1 bit number.
            immutable t = (v | (v - 1)) + 1;
            v = t | ((((t & -t) / (v & -v)) >> 1) - 1);
        }
        this.combVal = localCombVal;
    }

    uint length() @property const pure nothrow @safe {
        return combVal.length;
    }

    uint[] opIndex(in uint idx) const pure nothrow @safe {
        return val2set(combVal[idx]);
    }

    int opApply(immutable int delegate(in ref uint[]) pure nothrow @safe dg)
    pure nothrow @safe {
        foreach (immutable v; combVal) {
            auto set = val2set(v);
            if (dg(set))
                break;
        }
        return 1;
    }

    private uint[] val2set(in ulong v) const pure nothrow @safe {
        // Convert bit pattern to selection set
        immutable uint bitLimit = nt + nc - 1;
        uint typeIdx = 0;
        uint[] set;
        foreach (immutable bitNum; 0 .. bitLimit)
            if (v & (1 << (bitLimit - bitNum - 1)))
                typeIdx++;
            else
                set ~= typeIdx;
        return set;
    }
}

// For finite Random Access Range.
auto combRep(R)(R types, in uint numChoice) /*pure*/ nothrow @safe
if (hasLength!R && isRandomAccessRange!R) {
    ElementType!R[][] result;

    foreach (const s; CombRep(types.length, numChoice)) {
        ElementType!R[] r;
        foreach (immutable i; s)
            r ~= types[i];
        result ~= r;
    }

    return result;
}

void main() @safe {
    foreach (const e; combRep(["iced", "jam", "plain"], 2))
        writefln("%-(%5s %)", e);
    writeln("Ways to select 3 from 10 types is ",
            CombRep(10, 3).length);
}
```

{{out}}

```txt
 iced  iced
 iced   jam
 iced plain
  jam   jam
  jam plain
plain plain
Ways to select 3 from 10 types is 220
```



### Short Recursive Version


```d
import std.stdio, std.range, std.algorithm;

T[][] combsRep(T)(T[] lst, in int k) {
    if (k == 0)
        return [[]];
    if (lst.empty)
        return null;

    return combsRep(lst, k - 1).map!(L => lst[0] ~ L).array
           ~ combsRep(lst[1 .. $], k);
}

void main() {
    ["iced", "jam", "plain"].combsRep(2).writeln;
    10.iota.array.combsRep(3).length.writeln;
}
```

{{out}}

```txt
[["iced", "iced"], ["iced", "jam"], ["iced", "plain"], ["jam", "jam"], ["jam", "plain"], ["plain", "plain"]]
220
```



## EasyLang


<lang>items$[] = [ "iced" "jam" "plain" ]
n = len items$[]
k = 2
len result[] k
n_results = 0
#
func output . .
  n_results += 1
  if len items$[] > 0
    s$ = ""
    for i range k
      s$ &= items$[result[i]] & " "
    .
    print s$
  .
.
func combine pos val . .
  if pos = k
    call output
  else
    for i = val to n - 1
      result[pos] = i
      call combine pos + 1 i
    .
  .
.
call combine 0 0
#
n = 10
k = 3
len result[] k
items$[] = [ ]
n_results = 0
call combine 0 0
print ""
print n_results & " results with 10 donuts"
```


{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain

220 results with 10 donuts

```



## EchoLisp

We can use the native '''combinations/rep''' function, or use a '''combinator''' iterator, or implement the function.

```scheme

;;
;; native function : combinations/rep in list.lib
;;
(lib 'list)

(combinations/rep '(iced jam plain) 2)
   → ((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

;;
;; using a combinator iterator
;;
(lib 'sequences)

(take (combinator/rep '(iced jam plain) 2) 8)
    → ((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

;;
;; or, implementing the function
;;

(define (comb/rep nums k)
	(cond
	[(null? nums) null]
	[(<= k 0) null]
	[(= k 1) (map list nums)]
	[else
		(for/fold (acc null) ((anum nums))
		(append acc
	  		(for/list ((xs (comb/rep nums (1- k))))
	  		#:continue (< (first xs) anum)
	  		(cons anum xs))))]))

(map (curry list-permute '(iced jam plain)) (comb/rep (iota 3) 2))
    → ((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

;;
;; extra credit
;;

(length (combinator/rep (iota 10) 3))
    → 220


```



## Egison



```egison

(define $comb/rep
  (lambda [$n $xs]
    (match-all xs (list something)
      [(loop $i [1 ,n] <join _ (& <cons $a_i _> ...)> _) a])))

(test (comb/rep 2 {"iced" "jam" "plain"}))

```

{{out}}

```txt

{[|"iced" "iced"|] [|"iced" "jam"|] [|"jam" "jam"|] [|"iced" "plain"|] [|"jam" "plain"|] [|"plain" "plain"|]}

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def comb_rep(0, _), do: [[]]
  def comb_rep(_, []), do: []
  def comb_rep(n, [h|t]=s) do
    (for l <- comb_rep(n-1, s), do: [h|l]) ++ comb_rep(n, t)
  end
end

s = [:iced, :jam, :plain]
Enum.each(RC.comb_rep(2, s), fn x -> IO.inspect x end)

IO.puts  "\nExtra credit: #{length(RC.comb_rep(3, Enum.to_list(1..10)))}"
```


{{out}}

```txt

[:iced, :iced]
[:iced, :jam]
[:iced, :plain]
[:jam, :jam]
[:jam, :plain]
[:plain, :plain]

Extra credit: 220

```



## Erlang


```erlang

-module(comb).
-compile(export_all).

comb_rep(0,_) ->
    [[]];
comb_rep(_,[]) ->
    [];
comb_rep(N,[H|T]=S) ->
    [[H|L] || L <- comb_rep(N-1,S)]++comb_rep(N,T).

```

{{out}}

```txt

94> comb:comb_rep(2,[iced,jam,plain]).
[[iced,iced],
 [iced,jam],
 [iced,plain],
 [jam,jam],
 [jam,plain],
 [plain,plain]]
95> length(comb:comb_rep(3,lists:seq(1,10))).
220

```



## Fortran


```Fortran

program main
	integer :: chosen(4)
	integer :: ssize

	character(len=50) :: donuts(4) = [ "iced", "jam", "plain", "something completely different" ]

	ssize = choose( chosen, 2, 3 )
	write(*,*) "Total = ", ssize

	contains

	recursive function choose( got, len, maxTypes, nChosen, at ) result ( output )
		integer :: got(:)
		integer :: len
		integer :: maxTypes
		integer :: output
		integer, optional :: nChosen
		integer, optional :: at

		integer :: effNChosen
		integer :: effAt

		integer :: i
		integer :: counter

		effNChosen = 1
		if( present(nChosen) ) effNChosen = nChosen

		effAt = 1
		if( present(at) ) effAt = at

		if ( effNChosen == len+1 ) then
			do i=1,len
				write(*,"(A10,5X)", advance='no') donuts( got(i)+1 )
			end do

			write(*,*) ""

			output = 1
			return
		end if

		counter = 0
		do i=effAt,maxTypes
			got(effNChosen) = i-1
			counter = counter + choose( got, len, maxTypes, effNChosen + 1, i )
		end do

		output = counter
		return
	end function choose

end program main

```

{{out}}

```txt

iced           iced
iced           jam
iced           plain
jam            jam
jam            plain
plain          plain
 Total =            6

```



## GAP


```gap
# Built-in
UnorderedTuples(["iced", "jam", "plain"], 2);
```



## Go


### Concise recursive


```go
package main

import "fmt"

func combrep(n int, lst []string) [][]string {
    if n == 0 {
        return [][]string{nil}
    }
    if len(lst) == 0 {
        return nil
    }
    r := combrep(n, lst[1:])
    for _, x := range combrep(n-1, lst) {
        r = append(r, append(x, lst[0]))
    }
    return r
}

func main() {
    fmt.Println(combrep(2, []string{"iced", "jam", "plain"}))
    fmt.Println(len(combrep(3,
        []string{"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"})))
}
```

{{out}}

```txt

[[plain plain] [plain jam] [jam jam] [plain iced] [jam iced] [iced iced]]
220

```



### Channel

Using channel and goroutine, showing how to use synced or unsynced communication.

```go
package main

import "fmt"

func picks(picked []int, pos, need int, c chan[]int, do_wait bool) {
	if need == 0 {
		if do_wait {
			c <- picked
			<-c
		} else { // if we want only the count, there's no need to
			 // sync between coroutines; let it clobber the array
			c <- []int {}
		}
		return
	}

	if pos <= 0 {
		if need == len(picked) { c <- nil }
		return
	}

	picked[len(picked) - need] = pos - 1
	picks(picked, pos, need - 1, c, do_wait) // choose the current donut
	picks(picked, pos - 1, need, c, do_wait) // or don't
}

func main() {
	donuts := []string {"iced", "jam", "plain" }

	picked := make([]int, 2)
	ch := make(chan []int)

	// true: tell the channel to wait for each sending, because
	// otherwise the picked array may get clobbered before we can do
	// anything to it
	go picks(picked, len(donuts), len(picked), ch, true)

	var cc []int
	for {
		if cc = <-ch; cc == nil { break }
		for _, i := range cc {
			fmt.Printf("%s ", donuts[i])
		}
		fmt.Println()
		ch <- nil // sync
	}

	picked = make([]int, 3)
	// this time we only want the count, so tell goroutine to keep going
	// and work the channel buffer
	go picks(picked, 10, len(picked), ch, false)
	count := 0
	for {
		if cc = <-ch; cc == nil { break }
		count++
	}
	fmt.Printf("\npicking 3 of 10: %d\n", count)
}
```

{{out}}

```txt

plain plain
plain jam
plain iced
jam jam
jam iced
iced iced

picking 3 of 10: 220

```


### Multiset

This version has proper representation of sets and multisets.

```go
package main

import (
    "fmt"
    "sort"
    "strconv"
)

// Go maps are an easy representation for sets as long as the element type
// of the set is valid as a key type for maps.  Strings are easy.
// We follow the convention of always storing true for the value.
type set      map[string]bool

// Multisets of strings are easy in the same way.
// We store the multiplicity of the element as the value.
type multiset map[string]int

// But multisets are not valid as a map key type so we must do something
// more involved to make a set of multisets, which is the desired return
// type for the combrep function required by the task.  We can store the
// multiset as the value, but we derive a unique string to use as a key.
type msSet    map[string]multiset

// The key method returns this string.  The string will simply be a text
// representation of the contents of the multiset.  The standard
// printable representation of the multiset cannot be used however, because
// Go maps are not ordered.  Instead, the contents are copied to a slice,
// which is sorted to produce something with a printable representation
// that will compare == for mathematically equal multisets.
//
// Of course there is overhead for this and if performance were important,
// a different representation would be used for multisets, one that didn’t
// require sorting to produce a key...
func (m multiset) key() string {
    pl := make(pairList, len(m))
    i := 0
    for k, v := range m {
        pl[i] = msPair{k, v}
	i++
    }
    sort.Sort(pl)
    return fmt.Sprintf("%v", pl)
}

// Types and methods needed for sorting inside of mulitset.key()
type msPair struct {
    string
    int
}
type pairList []msPair
func (p pairList) Len() int { return len(p) }
func (p pairList) Swap(i, j int) { p[i], p[j] = p[j], p[i] }
func (p pairList) Less(i, j int) bool { return p[i].string < p[j].string }

// Function required by task.
func combrep(n int, lst set) msSet {
    if n == 0 {
        var ms multiset
        return msSet{ms.key(): ms}
    }
    if len(lst) == 0 {
        return msSet{}
    }
    var car string
    var cdr set
    for ele := range lst {
        if cdr == nil {
            car = ele
            cdr = make(set)
        } else {
            cdr[ele] = true
        }
    }
    r := combrep(n, cdr)

    for _, x := range combrep(n-1, lst) {
        c := multiset{car: 1}
        for k, v := range x {
            c[k] += v
        }
        r[c.key()] = c
    }
    return r
}

// Driver for examples required by task.
func main() {
    // Input is a set.
    three := set{"iced": true, "jam": true, "plain": true}
    // Output is a set of multisets.  The set is a Go map:
    // The key is a string representation that compares equal
    // for equal multisets.  We ignore this here.  The value
    // is the multiset.  We print this.
    for _, ms := range combrep(2, three) {
        fmt.Println(ms)
    }
    ten := make(set)
    for i := 1; i <= 10; i++ {
        ten[strconv.Itoa(i)] = true
    }
    fmt.Println(len(combrep(3, ten)))
}
```

{{out}}

```txt

map[plain:1 jam:1]
map[plain:2]
map[iced:1 jam:1]
map[jam:2]
map[iced:1 plain:1]
map[iced:2]
220

```



## Haskell


```haskell
-- Return the combinations, with replacement, of k items from the
-- list.  We ignore the case where k is greater than the length of
-- the list.
combsWithRep :: Int -> [a] -> [[a]]
combsWithRep 0 _ = [[]]
combsWithRep _ [] = []
combsWithRep k xxs@(x:xs) =
  (x :) <$> combsWithRep (k - 1) xxs ++ combsWithRep k xs

binomial n m = f n `div` f (n - m) `div` f m
  where
    f n =
      if n == 0
        then 1
        else n * f (n - 1)

countCombsWithRep :: Int -> [a] -> Int
countCombsWithRep k lst = binomial (k - 1 + length lst) k

-- countCombsWithRep k = length . combsWithRep k
main :: IO ()
main = do
  print $ combsWithRep 2 ["iced", "jam", "plain"]
  print $ countCombsWithRep 3 [1 .. 10]
```

{{out}}

```txt
[["iced","iced"],["iced","jam"],["iced","plain"],["jam","jam"],["jam","plain"],["plain","plain"]]
220
```



### Dynamic Programming


The first solution is inefficient because it repeatedly calculates the same subproblem in different branches of recursion. For example, <code>combsWithRep k (x:xs)</code> involves computing <code>combsWithRep (k-1) (x:xs)</code> and <code>combsWithRep k xs</code>, both of which (separately) compute <code>combsWithRep (k-1) xs</code>. To avoid repeated computation, we can use dynamic programming:


```haskell
combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x = scanl1 $ (++) . map (x :)

main :: IO ()
main = print $ combsWithRep 2 ["iced", "jam", "plain"]
```


and another approach, using manual recursion:

```haskell
combsWithRep
  :: (Eq a)
  => Int -> [a] -> [[a]]
combsWithRep k xs = comb k []
  where
    comb 0 lst = lst
    comb n [] = comb (n - 1) (pure <$> xs)
    comb n peers =
      let nextLayer ys@(h:_) = (: ys) <$> dropWhile (/= h) xs
      in comb (n - 1) (foldMap nextLayer peers)

main :: IO ()
main = do
  print $ combsWithRep 2 ["iced", "jam", "plain"]
  print $ length $ combsWithRep 3 [0 .. 9]
```

{{Out}}

```txt
[["iced","iced"],["jam","iced"],["plain","iced"],["jam","jam"],["plain","jam"],["plain","plain"]]
220
```


=={{header|Icon}} and {{header|Unicon}}==

Following procedure is a generator, which generates each combination of length n in turn:

```Icon

# generate all combinations of length n from list L,
# including repetitions
procedure combinations_repetitions (L, n)
  if n = 0
    then suspend [] # if reach 0, then return an empty list
    else if *L > 0
      then {
        # keep the first element
        item := L[1]
        # get all of length n in remaining list
        every suspend (combinations_repetitions (L[2:0], n))
        # get all of length n-1 in remaining list
        # and add kept element to make list of size n
        every i := combinations_repetitions (L, n-1) do
          suspend [item] ||| i
      }
end

```


Test procedure:


```Icon

# convenience function
procedure write_list (l)
  every (writes (!l || " "))
  write ()
end

# testing routine
procedure main ()
  # display all combinations for 2 of iced/jam/plain
  every write_list (combinations_repetitions(["iced", "jam", "plain"], 2))
  # get a count for number of ways to select 3 items from 10
  every push(num_list := [], 1 to 10)
  count := 0
  every combinations_repetitions(num_list, 3) do count +:= 1
  write ("There are " || count || " possible combinations of 3 from 10")
end

```


{{out}}

```txt

plain plain
jam plain
jam jam
iced plain
iced jam
iced iced
There are 220 possible combinations of 3 from 10

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Combinat.bas"
110 READ N
120 STRING D$(1 TO N)*5
130 FOR I=1 TO N
140   READ D$(I)
150 NEXT
160 FOR I=1 TO N
170   FOR J=I TO N
180     PRINT D$(I);" ";D$(J)
190   NEXT
200 NEXT
210 DATA 3,iced,jam,plain
```



## J

Cartesian product, the monadic j verb { solves the problem.  The rest of the code handles the various data types, order, and quantity to choose, and makes a set from the result.


```j>rcomb=:
@~.@:(/:~&.>)@,@{@# <
```


Example use:


```j
   2 rcomb ;:'iced jam plain'
┌─────┬─────┐
│iced │iced │
├─────┼─────┤
│iced │jam  │
├─────┼─────┤
│iced │plain│
├─────┼─────┤
│jam  │jam  │
├─────┼─────┤
│jam  │plain│
├─────┼─────┤
│plain│plain│
└─────┴─────┘
   #3 rcomb i.10         NB. # ways to choose 3 items from 10 with repetitions
220
```



### J Alternate implementation


Considerably faster:


```j
require 'stats'
combr=: i.@[ -"1~ [ comb + - 1:
rcomb=: (combr #) { ]
```


<code>rcomb</code> functions identically, and <code>combr</code> calculates indices:


```j
   2 combr 3
0 0
0 1
0 2
1 1
1 2
2 2
```


In other words: we compute <code>2 comb 4 </code> (note that 4 = (2 + 3)-1) and then subtract from each column the minimum value in each column (i. 2).


## Java

'''MultiCombinationsTester.java'''

```java

import com.objectwave.utility.*;

public class MultiCombinationsTester {

    public MultiCombinationsTester() throws CombinatoricException {
        Object[] objects = {"iced", "jam", "plain"};
        //Object[] objects = {"abba", "baba", "ab"};
        //Object[] objects = {"aaa", "aa", "a"};
        //Object[] objects = {(Integer)1, (Integer)2, (Integer)3, (Integer)4};
        MultiCombinations mc = new MultiCombinations(objects, 2);
        while (mc.hasMoreElements()) {
            for (int i = 0; i < mc.nextElement().length; i++) {
                System.out.print(mc.nextElement()[i].toString() + " ");
            }
            System.out.println();
        }

        // Extra credit:
        System.out.println("----------");
        System.out.println("The ways to choose 3 items from 10 with replacement = " + MultiCombinations.c(10, 3));
    } // constructor

    public static void main(String[] args) throws CombinatoricException {
        new MultiCombinationsTester();
    }
} // class

```


'''MultiCombinations.java'''

```java

import com.objectwave.utility.*;
import java.util.*;

public class MultiCombinations {

    private HashSet<String> set = new HashSet<String>();
    private Combinations comb = null;
    private Object[] nextElem = null;

    public MultiCombinations(Object[] objects, int k) throws CombinatoricException {
        k = Math.max(0, k);
        Object[] myObjects = new Object[objects.length * k];
        for (int i = 0; i < objects.length; i++) {
            for (int j = 0; j < k; j++) {
                myObjects[i * k + j] = objects[i];
            }
        }
        comb = new Combinations(myObjects, k);
    } // constructor

    boolean hasMoreElements() {
        boolean ret = false;
        nextElem = null;
        int oldCount = set.size();
        while (comb.hasMoreElements()) {
            Object[] elem = (Object[]) comb.nextElement();
            String str = "";
            for (int i = 0; i < elem.length; i++) {
                str += ("%" + elem[i].toString() + "~");
            }
            set.add(str);
            if (set.size() > oldCount) {
                nextElem = elem;
                ret = true;
                break;
            }
        }
        return ret;
    } // hasMoreElements()

    Object[] nextElement() {
        return nextElem;
    }

    static java.math.BigInteger c(int n, int k) throws CombinatoricException {
        return Combinatoric.c(n + k - 1, k);
    }
} // class

```


{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
----------
The ways to choose 3 items from 10 with replacement = 220

```


## JavaScript


### ES5


### =Imperative=


```javascript><html><head><title>Donuts</title></head

<body><pre id='x'>
```
<script type="application/javascript">
function disp(x) {
	var e = document.createTextNode(x + '\n');
	document.getElementById('x').appendChild(e);
}

function pick(n, got, pos, from, show) {
	var cnt = 0;
	if (got.length == n) {
		if (show) disp(got.join(' '));
		return 1;
	}
	for (var i = pos; i < from.length; i++) {
		got.push(from[i]);
		cnt += pick(n, got, i, from, show);
		got.pop();
	}
	return cnt;
}

disp(pick(2, [], 0, ["iced", "jam", "plain"], true) + " combos");
disp("pick 3 out of 10: " + pick(3, [], 0, "a123456789".split(''), false) + " combos");
</script></body></html>
```

{{out}}

```txt
iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
6 combos
pick 3 out of 10: 220 combos
```



### =Functional=


```JavaScript
(function () {

  // n -> [a] -> [[a]]
  function combsWithRep(n, lst) {
    return n ? (
      lst.length ? combsWithRep(n - 1, lst).map(function (t) {
        return [lst[0]].concat(t);
      }).concat(combsWithRep(n, lst.slice(1))) : []
    ) : [[]];
  };

  // If needed, we can derive a significantly faster version of
  // the simple recursive function above by memoizing it

  // f -> f
  function memoized(fn) {
    m = {};
    return function (x) {
      var args = [].slice.call(arguments),
        strKey = args.join('-');

      v = m[strKey];
      if ('u' === (typeof v)[0])
        m[strKey] = v = fn.apply(null, args);
      return v;
    }
  }

  // [m..n]
  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }


  return [

      combsWithRep(2, ["iced", "jam", "plain"]),

    // obtaining and applying a memoized version of the function
      memoized(combsWithRep)(3, range(1, 10)).length
    ];

})();
```


{{Out}}


```JavaScript
[
 [["iced", "iced"], ["iced", "jam"], ["iced", "plain"],
  ["jam", "jam"], ["jam", "plain"], ["plain", "plain"]],
 220
]
```



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // COMBINATIONS WITH REPETITIONS -------------------------------------------

    // combsWithRep :: Int -> [a] -> [[a]]
    const combsWithRep = (k, xs) => {
        const comb = (n, ys) => {
            if (0 === n) return ys;
            if (isNull(ys)) return comb(n - 1, map(pure, xs));

            return comb(n - 1, concatMap(zs => {
                const h = head(zs);
                return map(x => [x].concat(zs), dropWhile(x => x !== h, xs));
            }, ys));
        };
        return comb(k, []);
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // dropWhile :: (a -> Bool) -> [a] -> [a]
    const dropWhile = (p, xs) => {
        let i = 0;
        for (let lng = xs.length;
            (i < lng) && p(xs[i]); i++) {}
        return xs.slice(i);
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // head :: [a] -> Maybe a
    const head = xs => xs.length ? xs[0] : undefined;

    // isNull :: [a] -> Bool
    const isNull = xs => (xs instanceof Array) ? xs.length < 1 : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // pure :: a -> [a]
    const pure = x => [x];

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // TEST -------------------------------------------------------------------
    return show({
        twoFromThree: combsWithRep(2, ['iced', 'jam', 'plain']),
        threeFromTen: length(combsWithRep(3, enumFromTo(0, 9)))
    });
})();
```

{{Out}}

```txt
{
  "twoFromThree": [
    [
      "iced",
      "iced"
    ],
    [
      "jam",
      "iced"
    ],
    [
      "plain",
      "iced"
    ],
    [
      "jam",
      "jam"
    ],
    [
      "plain",
      "jam"
    ],
    [
      "plain",
      "plain"
    ]
  ],
  "threeFromTen": 220
}
```



## jq


```jq
def pick(n):
  def pick(n; m):  # pick n, from m onwards
    if n == 0 then []
    elif m == length then empty
    elif n == 1 then (.[m:][] | [.])
    else ([.[m]] + pick(n-1; m)), pick(n; m+1)
    end;
  pick(n;0) ;
```

'''The task''':

```jq
 "Pick 2:",
 (["iced", "jam", "plain"] | pick(2)),
 ([[range(0;10)] | pick(3)] | length) as $n
  | "There are \($n) ways to pick 3 objects with replacement from 10."

```

{{Out}}

```txt
$ jq -n -r -c -f pick.jq
Pick 2:
["iced","iced"]
["iced","jam"]
["iced","plain"]
["jam","jam"]
["jam","plain"]
["plain","plain"]
There are 220 ways to pick 3 objects with replacement from 10.
```



## Julia

{{works with|Julia|0.6}}


```julia
using Combinatorics

l = ["iced", "jam", "plain"]
println("List: ", l, "\nCombinations:")
for c in with_replacement_combinations(l, 2)
    println(c)
end

@show length(with_replacement_combinations(1:10, 3))
```


{{out}}

```txt
List: String["iced", "jam", "plain"]
Combinations:
String["iced", "iced"]
String["iced", "jam"]
String["iced", "plain"]
String["jam", "jam"]
String["jam", "plain"]
String["plain", "plain"]
length(with_replacement_combinations(1:10, 3)) = 220
```



## Kotlin


```scala
// version 1.0.6

class CombsWithReps<T>(val m: Int, val n: Int, val items: List<T>, val countOnly: Boolean = false) {
    private val combination = IntArray(m)
    private var count = 0

    init {
        generate(0)
        if (!countOnly) println()
        println("There are $count combinations of $n things taken $m at a time, with repetitions")
    }

    private fun generate(k: Int) {
        if (k >= m) {
            if (!countOnly) {
                for (i in 0 until m) print("${items[combination[i]]}\t")
                println()
            }
            count++
        }
        else {
            for (j in 0 until n)
                if (k == 0 || j >= combination[k - 1]) {
                    combination[k] = j
                    generate(k + 1)
                }
        }
    }
}

fun main(args: Array<String>) {
    val doughnuts = listOf("iced", "jam", "plain")
    CombsWithReps(2, 3, doughnuts)
    println()
    val generic10 = "0123456789".split("")
    CombsWithReps(3, 10, generic10, true)
}
```


{{out}}

```txt

iced    iced
iced    jam
iced    plain
jam     jam
jam     plain
plain   plain

There are 6 combinations of 3 things taken 2 at a time, with repetitions

There are 220 combinations of 10 things taken 3 at a time, with repetitions

```



## LFE

{{trans|Erlang}} and {{trans|Clojure}}



### With List Comprehension



```lisp

(defun combinations
 (('() _)
    '())
  ((coll 1)
    (lists:map #'list/1 coll))
   (((= (cons head tail) coll) n)
    (++ (lc ((<- subcoll (combinations coll (- n 1))))
            (cons head subcoll))
        (combinations tail n))))

```


### With Map



```lisp

(defun combinations
  (('() _)
    '())
  ((coll 1)
    (lists:map #'list/1 coll))
   (((= (cons head tail) coll) n)
    (++ (lists:map (lambda (subcoll) (cons head subcoll))
                   (combinations coll (- n 1)))
        (combinations tail n))))

```


Output is the same for both:


```lisp

> (combinations '(iced jam plain) 2)
((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

```



## Lua


```Lua
function GenerateCombinations(tList, nMaxElements, tOutput, nStartIndex, nChosen, tCurrentCombination)
	if not nStartIndex then
		nStartIndex = 1
	end
	if not nChosen then
		nChosen = 0
	end
	if not tOutput then
		tOutput = {}
	end
	if not tCurrentCombination then
		tCurrentCombination = {}
	end

	if nChosen == nMaxElements then
		-- Must copy the table to avoid all elements referring to a single reference
		local tCombination = {}
		for k,v in pairs(tCurrentCombination) do
			tCombination[k] = v
		end

		table.insert(tOutput, tCombination)
		return
	end

 	local nIndex = 1
	for k,v in pairs(tList) do
		if nIndex >= nStartIndex then
			tCurrentCombination[nChosen + 1] = tList[nIndex]
			GenerateCombinations(tList, nMaxElements, tOutput, nIndex, nChosen + 1, tCurrentCombination)
		end

		nIndex = nIndex + 1
	end

	return tOutput
end

tDonuts = {"iced", "jam", "plain"}
tCombinations = GenerateCombinations(tDonuts, #tDonuts)
for nCombination,tCombination in ipairs(tCombinations) do
	print("Combination " .. tostring(nCombination))
	for nIndex,strFlavor in ipairs(tCombination) do
		print("+" .. strFlavor)
	end
end

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

This method will only work for small set and sample sizes (as it generates all Tuples then filters duplicates - Length[Tuples[Range[10],10]] is already bigger than Mathematica can handle).

```Mathematica
DeleteDuplicates[Tuples[{"iced", "jam", "plain"}, 2],Sort[#1] == Sort[#2] &]
->{{"iced", "iced"}, {"iced", "jam"}, {"iced", "plain"}, {"jam", "jam"}, {"jam", "plain"}, {"plain", "plain"}}

Combi[x_, y_] := Binomial[(x + y) - 1, y]
Combi[3, 2]
-> 6
Combi[10, 3]
->220

```



A better method therefore:

```Mathematica
CombinWithRep[S_List, k_] := Module[{occupation, assignment},
  occupation =
   Flatten[Permutations /@
     IntegerPartitions[k, {Length[S]}, Range[0, k]], 1];
  assignment =
   Flatten[Table[ConstantArray[z, {#[[z]]}], {z, Length[#]}]] & /@
    occupation;
  Thread[S[[#]]] & /@ assignment
  ]

In[2]:= CombinWithRep[{"iced", "jam", "plain"}, 2]

Out[2]= {{"iced", "iced"}, {"jam", "jam"}, {"plain",
  "plain"}, {"iced", "jam"}, {"iced", "plain"}, {"jam", "plain"}}

```


Which can handle the Length[S] = 10, k=10 situation in still only seconds.


## Mercury

comb.choose uses a nondeterministic list.member/2 to pick values from the list, and just puts them into a bag (a multiset).  comb.choose_all gathers all of the possible bags that comb.choose would produce for a given list and number of picked values, and turns them into lists (for readability).

comb.count_choices shows off solutions.aggregate (which allows you to fold over solutions as they're found) rather than list.length and the factorial function.


```Mercury
:- module comb.
:- interface.
:- import_module list, int, bag.

:- pred choose(list(T)::in, int::in, bag(T)::out) is nondet.
:- pred choose_all(list(T)::in, int::in, list(list(T))::out) is det.
:- pred count_choices(list(T)::in, int::in, int::out) is det.

:- implementation.
:- import_module solutions.

choose(L, N, R) :- choose(L, N, bag.init, R).

:- pred choose(list(T)::in, int::in, bag(T)::in, bag(T)::out) is nondet.
choose(L, N, !R) :-
        ( N = 0 ->
                true
        ;
                member(X, L),
                bag.insert(!.R, X, !:R),
                choose(L, N - 1, !R)
        ).

choose_all(L, N, R) :-
        solutions(choose(L, N), R0),
        list.map(bag.to_list, R0, R).

count_choices(L, N, Count) :-
        aggregate(choose(L, N), count, 0, Count).

:- pred count(T::in, int::in, int::out) is det.
count(_, N0, N) :- N0 + 1 = N.
```


Usage:


```Mercury
:- module comb_ex.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module comb, list, string.

:- type doughtnuts
        --->    iced ; jam ; plain
        ;       glazed ; chocolate ; cream_filled ; mystery
        ;       cubed ; cream_covered ; explosive.

main(!IO) :-
        choose_all([iced, jam, plain], 2, L),
        count_choices([iced, jam, plain, glazed, chocolate, cream_filled,
                       mystery, cubed, cream_covered, explosive], 3, N),
        io.write(L, !IO), io.nl(!IO),
        io.write_string(from_int(N) ++ " choices.\n", !IO).
```


{{out}}

```txt
[[iced, iced], [jam, jam], [plain, plain], [iced, jam], [iced, plain], [jam, plain]]
220 choices.
```



## Nim

{{trans|D}}

```nim
import future, sequtils

proc combsReps[T](lst: seq[T], k: int): seq[seq[T]] =
  if k == 0:
    @[newSeq[T]()]
  elif lst.len == 0:
    @[]
  else:
    lst.combsReps(k - 1).map((x: seq[T]) => lst[0] & x) &
      lst[1 .. -1].combsReps(k)

echo(@["iced", "jam", "plain"].combsReps(2))
echo toSeq(1..10).combsReps(3).len
```

{{out}}

```txt
@[@[iced, iced], @[iced, jam], @[iced, plain], @[jam, jam], @[jam, plain], @[plain, plain]]
220
```



## OCaml

{{trans|Haskell}}


```ocaml
let rec combs_with_rep k xxs =
  match k, xxs with
  | 0,  _ -> [[]]
  | _, [] -> []
  | k, x::xs ->
      List.map (fun ys -> x::ys) (combs_with_rep (k-1) xxs)
      @ combs_with_rep k xs
```


in the interactive loop:


```txt

# combs_with_rep 2 ["iced"; "jam"; "plain"] ;;
- : string list list =
[["iced"; "iced"]; ["iced"; "jam"]; ["iced"; "plain"]; ["jam"; "jam"];
 ["jam"; "plain"]; ["plain"; "plain"]]

# List.length (combs_with_rep 3 [1;2;3;4;5;6;7;8;9;10]) ;;
- : int = 220

```



### Dynamic programming



```ocaml
let combs_with_rep m xs =
  let arr = Array.make (m+1) [] in
  arr.(0) <- [[]];
  List.iter (fun x ->
    for i = 1 to m do
      arr.(i) <- arr.(i) @ List.map (fun xs -> x::xs) arr.(i-1)
    done
  ) xs;
  arr.(m)
```


in the interactive loop:


```txt

# combs_with_rep 2 ["iced"; "jam"; "plain"] ;;
- : string list list =
[["iced"; "iced"]; ["jam"; "iced"]; ["jam"; "jam"]; ["plain"; "iced"];
 ["plain"; "jam"]; ["plain"; "plain"]]

# List.length (combs_with_rep 3 [1;2;3;4;5;6;7;8;9;10]) ;;
- : int = 220

```



## PARI/GP


```parigp
ways(k,v,s=[])={
	if(k==0,return([]));
	if(k==1,return(vector(#v,i,concat(s,[v[i]]))));
	if(#v==1,return(ways(k-1,v,concat(s,v))));
	my(u=vecextract(v,2^#v-2));
	concat(ways(k-1,v,concat(s,[v[1]])),ways(k,u,s))
};
xc(k,v)=binomial(#v+k-1,k);
ways(2, ["iced","jam","plain"])
```



## Perl

The highly readable version:

```perl
sub p { $_[0] ? map p($_[0] - 1, [@{$_[1]}, $_[$_]], @_[$_ .. $#_]), 2 .. $#_ : $_[1] }
sub f { $_[0] ? $_[0] * f($_[0] - 1) : 1 }
sub pn{ f($_[0] + $_[1] - 1) / f($_[0]) / f($_[1] - 1) }

for (p(2, [], qw(iced jam plain))) {
        print "@$_\n";
}

printf "\nThere are %d ways to pick 7 out of 10\n", pn(7,10);

```


Prints:

```txt
iced iced
iced jam
iced plain
jam jam
jam plain
plain plain

There are 11440 ways to pick 7 out of 10
```


With a module:

```perl
use Algorithm::Combinatorics qw/combinations_with_repetition/;
my $iter = combinations_with_repetition([qw/iced jam plain/], 2);
while (my $p = $iter->next) {
  print "@$p\n";
}
# Not an efficient way: generates them all in an array!
my $count =()= combinations_with_repetition([1..10],7);
print "There are $count ways to pick 7 out of 10\n";
```



## Perl 6


One could simply generate all [[Permutations_with_repetitions#Perl_6|permutations]], and then remove "duplicates":

{{works with|Rakudo|2016.07}}

```perl6>my @S = <iced jam plain
;
my $k = 2;

.put for [X](@S xx $k).unique(as => *.sort.cache, with => &[eqv])
```


{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain

```


Alternatively, a recursive solution:

{{trans|Haskell}}

```perl6
proto combs_with_rep (UInt, @) {*}

multi combs_with_rep (0,  @)  { () }
multi combs_with_rep (1,  @a)  { map { $_, }, @a }
multi combs_with_rep ($,  []) { () }
multi combs_with_rep ($n, [$head, *@tail]) {
    |combs_with_rep($n - 1, ($head, |@tail)).map({ $head, |@_ }),
    |combs_with_rep($n, @tail);
}

.say for combs_with_rep( 2, [< iced jam plain >] );

# Extra credit:
sub postfix:<!> { [*] 1..$^n }
sub combs_with_rep_count ($k, $n) { ($n + $k - 1)! / $k! / ($n - 1)! }

say combs_with_rep_count( 3, 10 );
```

{{out}}

```txt
(iced iced)
(iced jam)
(iced plain)
(jam jam)
(jam plain)
(plain plain)
220
```



## Phix


```Phix
procedure choose(sequence from, integer n, at=1, sequence res={})
    if length(res)=n then
        ?res
    else
        for i=at to length(from) do
            choose(from,n,i,append(res,from[i]))
        end for
    end if
end procedure

choose({"iced","jam","plain"},2)
```

{{out}}

```txt

{"iced","iced"}
{"iced","jam"}
{"iced","plain"}
{"jam","jam"}
{"jam","plain"}
{"plain","plain"}

```

The second part suggests enough differences (collecting and showing vs only counting) to strike me as ugly and confusing.
While I could easily, say, translate the C version, I'd rather forego the extra credit and use a completely different routine:

```Phix
function choices(integer from, n, at=1, taken=0)
integer count = 0
    if taken=n then return 1 end if
    taken += 1
    for i=at to from do
        count += choices(from,n,i,taken)
    end for
    return count
end function

?choices(10,3)
```

{{out}}

```txt

220

```


## PHP


Non-recursive algorithm to generate all combinations with repetitons. Taken from here: [https://habrahabr.ru/post/311934/]
 You must set k n variables and fill arrays b and c.


```PHP

<?php
//Author Ivan Gavryushin @dcc0
$k=3;
$n=5;
//set amount of elements as in $n var
$c=array(1,2,3,4,5);
//set amount of 1 as in $k var
$b=array(1,1,1);
$j=$k-1;
//Вывод
	function printt($b,$k) {

	$z=0;

	while ($z < $k) print $b[$z++].' ';
	print '
';
}
printt ($b,$k);

        while (1) {
//Увеличение на позиции K до N

       	 if (array_search($b[$j]+1,$c)!==false ) {
  	      	$b[$j]=$b[$j]+1;
        	printt ($b,$k);
       }

       	if ($b[$k-1]==$n) {
	 $i=$k-1;
//Просмотр массива справа налево
	 while ($i >= 0) {
		//Условие выхода
	 	if ( $i == 0 && $b[$i] == $n) break 2;
//Поиск элемента для увеличения
       		  $m=array_search($b[$i]+1,$c);
		if ($m!==false) {
		           $c[$m]=$c[$m]-1;
			  $b[$i]=$b[$i]+1;

			$g=$i;
//Сортировка массива B
		while ($g != $k-1) {
			array_unshift ($c, $b[$g+1]);
			$b[$g+1]=$b[$i];
			$g++;
			}
//Удаление повторяющихся значений из C
	                            $c=array_diff($c,$b);
				    printt ($b,$k);
                                    array_unshift ($c, $n);

		 	     break;
       			 }
	 	$i--;

		}

	}


}

?>

```



## PHP


```PHP
<?php
  function combos($arr, $k) {
    if ($k == 0) {
      return array(array());
    }

    if (count($arr) == 0) {
      return array();
    }

    $head = $arr[0];

    $combos = array();
    $subcombos = combos($arr, $k-1);
    foreach ($subcombos as $subcombo) {
      array_unshift($subcombo, $head);
      $combos[] = $subcombo;
    }
    array_shift($arr);
    $combos = array_merge($combos, combos($arr, $k));
    return $combos;
  }

  $arr = array("iced", "jam", "plain");
  $result = combos($arr, 2);
  foreach($result as $combo) {
    echo implode(' ', $combo), "
";
  }
  $donuts = range(1, 10);
  $num_donut_combos = count(combos($donuts, 3));
  echo "$num_donut_combos ways to order 3 donuts given 10 types";
?>
```

{{out}} in the browser:

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
220 ways to order 3 donuts given 10 types

```



## PicoLisp


```PicoLisp
(de combrep (N Lst)
   (cond
      ((=0 N) '(NIL))
      ((not Lst))
      (T
         (conc
            (mapcar
               '((X) (cons (car Lst) X))
               (combrep (dec N) Lst) )
            (combrep N (cdr Lst)) ) ) ) )
```

{{out}}

```txt
: (combrep 2 '(iced jam plain))
-> ((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

: (length (combrep 3 (range 1 10)))
-> 220
```



## PureBasic


```PureBasic
Procedure nextCombination(Array combIndex(1), elementCount)
  ;combIndex() must be dimensioned to 'k' - 1, elementCount equals 'n' - 1
  ;combination produced includes repetition of elements and is represented by the array combIndex()
  Protected i, indexValue, combSize = ArraySize(combIndex()), curIndex

  ;update indexes
  curIndex = combSize
  Repeat
    combIndex(curIndex) + 1
    If combIndex(curIndex) > elementCount

      curIndex - 1
      If curIndex < 0
        For i = 0 To combSize
          combIndex(i) = 0
        Next
        ProcedureReturn #False ;array reset to first combination
      EndIf

    ElseIf curIndex < combSize

      indexValue = combIndex(curIndex)
      Repeat
        curIndex + 1
        combIndex(curIndex) = indexValue
      Until curIndex = combSize

    EndIf
  Until  curIndex = combSize

  ProcedureReturn #True ;array contains next combination
EndProcedure

Procedure.s display(Array combIndex(1), Array dougnut.s(1))
  Protected i, elementCount = ArraySize(combIndex()), output.s = "  "
  For i = 0 To elementCount
    output + dougnut(combIndex(i)) + " + "
  Next
  ProcedureReturn Left(output, Len(output) - 3)
EndProcedure

DataSection
  Data.s "iced", "jam", "plain"
EndDataSection

If OpenConsole()
  Define n = 3, k = 2, i, combinationCount
  Dim combIndex(k - 1)
  Dim dougnut.s(n - 1)
  For i = 0 To n - 1: Read.s dougnut(i): Next

  PrintN("Combinations of " + Str(k) + " dougnuts taken " + Str(n) + " at a time with repetitions.")
  combinationCount = 0
  Repeat
    PrintN(display(combIndex(), dougnut()))
    combinationCount + 1
  Until Not nextCombination(combIndex(), n - 1)
  PrintN("Total combination count: " + Str(combinationCount))

  ;extra credit
  n = 10: k = 3
  Dim combIndex(k - 1)
  combinationCount = 0
  Repeat: combinationCount + 1: Until Not nextCombination(combIndex(), n - 1)
  PrintN(#CRLF$ + "Ways to select " + Str(k) + " items from " + Str(n) + " types: " + Str(combinationCount))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```
The nextCombination() procedure operates on an array of indexes to produce the next combination.  This generalization allows producing a combination from any collection of elements.  nextCombination() returns the value #False when the indexes have reach their maximum values and are then reset.

{{out}}

```txt
Combinations of 2 dougnuts taken 3 at a time with repetitions.
  iced + iced
  iced + jam
  iced + plain
  jam + jam
  jam + plain
  plain + plain
Total combination count: 6

Ways to select 3 items from 10 types: 220
```



## Python


```python>>>
 from itertools import combinations_with_replacement
>>> n, k = 'iced jam plain'.split(), 2
>>> list(combinations_with_replacement(n,k))
[('iced', 'iced'), ('iced', 'jam'), ('iced', 'plain'), ('jam', 'jam'), ('jam', 'plain'), ('plain', 'plain')]
>>> # Extra credit
>>> len(list(combinations_with_replacement(range(10), 3)))
220
>>>
```


'''References:'''
* [http://docs.python.org/py3k/library/itertools.html#itertools.combinations_with_replacement Python documentation]


Or, assembling our own '''combsWithRep''', by composition of functional primitives:

{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Combinations with repetitions'''

from itertools import (accumulate, chain, islice, repeat)
from functools import (reduce)


# combsWithRep :: Int -> [a] -> [kTuple a]
def combsWithRep(k):
    '''A list of tuples, representing
       sets of cardinality k,
       with elements drawn from xs.
    '''
    def f(a, x):
        def go(ys, xs):
            return xs + [[x] + y for y in ys]
        return accumulate(a, go)

    def combsBySize(xs):
        return reduce(
            f, xs, chain(
                [[[]]],
                islice(repeat([]), k)
            )
        )
    return lambda xs: [
        tuple(x) for x in next(islice(
            combsBySize(xs), k, None
        ))
    ]


# TEST ----------------------------------------------------
def main():
    '''Test the generation of sets of cardinality
       k with elements drawn from xs.
    '''
    print(
        combsWithRep(2)(['iced', 'jam', 'plain'])
    )
    print(
        len(combsWithRep(3)(enumFromTo(0)(9)))
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# showLog :: a -> IO String
def showLog(*s):
    '''Arguments printed with
       intercalated arrows.'''
    print(
        ' -> '.join(map(str, s))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[('iced', 'iced'), ('jam', 'iced'), ('jam', 'jam'), ('plain', 'iced'), ('plain', 'jam'), ('plain', 'plain')]
220
```



## Racket


```racket

#lang racket
(define (combinations xs k)
  (cond [(= k 0)     '(())]
        [(empty? xs) '()]
        [(append (combinations (rest xs) k)
                 (map (λ(x) (cons (first xs) x))
                      (combinations xs (- k 1))))]))

```



## REXX


### version 1

This REXX version uses a type of anonymous recursion.

```rexx
/*REXX pgm displays combination sets with repetitions for  X  things taken  Y  at a time*/
call RcombN    3,  2,  'iced jam plain'          /*The  1st  part of Rosetta Code task. */
call RcombN  -10,  3,  'Iced jam plain'          /* "   2nd    "   "    "      "    "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
RcombN: procedure; parse arg x,y,syms;  tell= x>0;  x=abs(x);   z=x+1  /*X>0? Show combo*/
        say copies('─',15) x "doughnut selection taken" y 'at a time:' /*display title. */
               do i=1  for words(syms);           $.i=word(syms, i)    /*assign symbols.*/
               end   /*i*/
        @.=1                                                           /*assign default.*/
               do #=1;        if tell    then  call show               /*display combos?*/
               @.y=@.y + 1;   if @.y==z  then  if .(y-1)  then leave   /* ◄─── recursive*/
               end   /*#*/
        say copies('═',15)  #  "combinations.";    say;   say          /*display answer.*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.: procedure expose @. y z;   parse arg ?;     if ?==0  then return 1;            p=@.? +1
        if p==z  then return .(? -1);      do j=?  to y;   @.j=p;   end  /*j*/;   return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:   L=;      do c=1  for y;   _=@.c;   L=L $._;   end  /*c*/;       say L;    return
```

{{out|output}}

```txt

─────────────── 3 doughnut selection taken 2 at a time:
 iced iced
 iced jam
 iced plain
 jam jam
 jam plain
 plain plain
═══════════════ 6 combinations.


─────────────── 10 doughnut selection taken 3 at a time:
═══════════════ 220 combinations.

```



### version 2

recursive (taken from version 1)
Reformatted and variable names suitable for OoRexx.

```rexx
/*REXX compute (and show) combination sets for nt things in ns places*/
  debug=0
  Call time 'R'
  Call RcombN 3,2,'iced,jam,plain'  /* The 1st part of the task      */
  Call RcombN -10,3,'iced,jam,plain,d,e,f,g,h,i,j' /* 2nd part       */
  Call RcombN -10,9,'iced,jam,plain,d,e,f,g,h,i,j' /* extra part     */
  Say time('E') 'seconds'
  Exit
/*-------------------------------------------------------------------*/
Rcombn: Procedure Expose thing. debug
  Parse Arg nt,ns,thinglist
  tell=nt>0
  nt=abs(nt)
  Say '------------' nt 'doughnut selection taken' ns 'at a time:'
  If tell=0 Then
    Say ' list output suppressed'
  Do i=1 By 1 While thinglist>''
    Parse Var thinglist thing.i ',' thinglist /* assign things.      */
    End
  index.=1
  Do cmb=1 By 1
    If tell Then                    /* display combinations          */
      Call show                     /* show this one                 */
    index.ns=index.ns+1
    Call show_index 'A'
    If index.ns==nt+1 Then
      If proc(ns-1) Then
        Leave
    End
  Say '------------' cmb 'combinations.'
  Say
  Return
/*-------------------------------------------------------------------*/
proc: Procedure Expose nt ns thing. index. debug
  Parse Arg recnt
  If recnt>0 Then Do
    p=index.recnt+1
    If p=nt+1 Then
      Return proc(recnt-1)
    Do i=recnt To ns
      index.i=p
      End
    Call show_index 'C'
    End
  Return recnt=0
/*-------------------------------------------------------------------*/
show: Procedure Expose index. thing. ns debug
  l=''
  Call show_index 'B----------------------->'
  Do i=1 To ns
    j=index.i
    l=l thing.j
    End
  Say l
  Return

show_index: Procedure Expose index. ns debug
  If debug Then Do
    Parse Arg tag
      l=tag
      Do i=1 To ns
        l=l index.i
        End
      Say l
    End
  Return
```

{{out}}

```txt
----------- 3 doughnut selection taken 2 at a time:
 iced iced
 iced jam
 iced plain
 jam jam
 jam plain
 plain plain
------------ 6 combinations.

------------ 10 doughnut selection taken 3 at a time:
 list output suppressed
------------ 220 combinations.

------------ 10 doughnut selection taken 9 at a time:
 list output suppressed
------------ 48620 combinations.

0.125000 seconds
```



### version 3

iterative (transformed from version 1)

```rexx
/*REXX compute (and show) combination sets for nt things in ns places*/
  Numeric Digits 20
  debug=0
  Call time 'R'
  Call IcombN 3,2,'iced,jam,plain'  /* The 1st part of the task      */
  Call IcombN -10,3,'iced,jam,plain,d,e,f,g,h,i,j' /* 2nd part       */
  Call IcombN -10,9,'iced,jam,plain,d,e,f,g,h,i,j' /* extra part     */
  Say time('E') 'seconds'
  Exit

IcombN: Procedure Expose thing. debug
  Parse Arg nt,ns,thinglist
  tell=nt>0
  nt=abs(nt)
  Say '------------' nt 'doughnut selection taken' ns 'at a time:'
  If tell=0 Then
    Say ' list output suppressed'
  Do i=1 By 1 While thinglist>''
    Parse Var thinglist thing.i ',' thinglist /* assign things.      */
    End
  index.=1
  cmb=0
  Call show
  i=ns+1
  Do While i>1
    i=i-1
    Do j=1 By 1 While index.i<nt
      index.i=index.i+1
      Call show
      End
    i1=i-1
    If index.i1<nt Then Do
      index.i1=index.i1+1
      Do ii=i To ns
        index.ii=index.i1
        End
      Call show
      i=ns+1
      End
    If index.1=nt Then Leave
    End
  Say cmb
  Return

show: Procedure Expose ns index. thing. tell cmb
  cmb=cmb+1
  If tell Then Do
    l=''
    Do i=1 To ns
      j=index.i
      l=l thing.j
      End
    Say l
    End
  Return
```


{{out}}

```txt
------------ 3 doughnut selection taken 2 at a time:
 iced iced
 iced jam
 iced plain
 jam jam
 jam plain
 plain plain
6
------------ 10 doughnut selection taken 3 at a time:
 list output suppressed
220
------------ 10 doughnut selection taken 9 at a time:
 list output suppressed
48620
0.109000 seconds
```

Slightly faster


## Ring


```ring

# Project : Combinations with repetitions

n = 2
k = 3
temp = []
comb = []
num = com(n, k)
combinations(n, k)
comb = sortfirst(comb, 1)
see showarray(comb) + nl

func combinations(n, k)
while true
         temp = []
         for nr = 1 to k
               tm = random(n-1) + 1
               add(temp, tm)
         next
             add(comb, temp)
         for p = 1  to len(comb) - 1
               for q = p + 1 to len(comb)
                     if (comb[p][1] = comb[q][1]) and (comb[p][2] = comb[q][2]) and (comb[p][3] = comb[q][3])
                        del(comb, p)
                     ok
                next
         next
         if len(comb) = num
            exit
         ok
end

func com(n, k)
        res = pow(n, k)
        return res

func showarray(vect)
        svect = ""
        for nrs = 1 to len(vect)
              svect = "[" + vect[nrs][1] + " " + vect[nrs][2] + " " + vect[nrs][3] + "]" + nl
              see svect
        next

Func sortfirst(alist, ind)
        aList = sort(aList,ind)
        for n = 1 to len(alist)-1
             for m= n + 1 to len(aList)
                  if alist[n][1] = alist[m][1] and alist[m][2] < alist[n][2]
                     temp = alist[n]
                     alist[n] = alist[m]
                     alist[m] = temp
                   ok
             next
        next
        for n = 1 to len(alist)-1
             for m= n + 1 to len(aList)
                  if alist[n][1] = alist[m][1] and alist[n][2] = alist[m][2] and alist[m][3] < alist[n][3]
                     temp = alist[n]
                     alist[n] = alist[m]
                     alist[m] = temp
                   ok
             next
       next
       return aList

```

Output:

```txt

[1 1 1]
[1 1 2]
[1 2 1]
[1 2 2]
[2 1 1]
[2 1 2]
[2 2 1]
[2 2 2]

```



## Ruby

{{works with|Ruby|2.0}}

```ruby
possible_doughnuts = ['iced', 'jam', 'plain'].repeated_combination(2)
puts "There are #{possible_doughnuts.count} possible doughnuts:"
possible_doughnuts.each{|doughnut_combi| puts doughnut_combi.join(' and ')}

# Extra credit
possible_doughnuts = [*1..10].repeated_combination(3)
# size returns the size of the enumerator, or nil if it can’t be calculated lazily.
puts "", "#{possible_doughnuts.size} ways to order 3 donuts given 10 types."
```

{{out}}

```txt

There are 6 possible doughnuts:
iced and iced
iced and jam
iced and plain
jam and jam
jam and plain
plain and plain

220 ways to order 3 donuts given 10 types.

```



## Scala

Scala has a combinations method in the standard library.

```scala

object CombinationsWithRepetition {

  def multi[A](as: List[A], k: Int): List[List[A]] =
    (List.fill(k)(as)).flatten.combinations(k).toList

  def main(args: Array[String]): Unit = {
    val doughnuts = multi(List("iced", "jam", "plain"), 2)
    for (combo <- doughnuts) println(combo.mkString(","))

    val bonus = multi(List(0,1,2,3,4,5,6,7,8,9), 3).size
    println("There are "+bonus+" ways to choose 3 items from 10 choices")
  }
}

```


{{out}}

```txt

iced,iced
iced,jam
iced,plain
jam,jam
jam,plain
plain,plain
There are 220 ways to choose 3 items from 10 choices
```



## Scheme

{{trans|PicoLisp}}

```scheme
(define (combs-with-rep k lst)
  (cond ((= k 0) '(()))
        ((null? lst) '())
        (else
         (append
          (map
           (lambda (x)
             (cons (car lst) x))
           (combs-with-rep (- k 1) lst))
          (combs-with-rep k (cdr lst))))))

(display (combs-with-rep 2 (list "iced" "jam" "plain"))) (newline)
(display (length (combs-with-rep 3 '(1 2 3 4 5 6 7 8 9 10)))) (newline)
```

{{out}}

```txt

((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))
220

```



### Dynamic programming


```scheme
(define (combs-with-rep m lst)
  (define arr (make-vector (+ m 1) '()))
  (vector-set! arr 0 '(()))
  (for-each (lambda (x)
	      (do ((i 1 (+ i 1)))
		  ((> i m))
		(vector-set! arr i (append (vector-ref arr i)
					   (map (lambda (xs) (cons x xs))
						(vector-ref arr (- i 1)))))
		)
	      ) lst)
  (vector-ref arr m))

(display (combs-with-rep 2 (list "iced" "jam" "plain"))) (newline)
(display (length (combs-with-rep 3 '(1 2 3 4 5 6 7 8 9 10)))) (newline)
```

{{out}}

```txt

((iced iced) (jam iced) (jam jam) (plain iced) (plain jam) (plain plain))
220

```



## Sidef

{{trans|Perl}}

```ruby
func cwr (n, l, a = []) {
    n>0 ? (^l -> map {|k| __FUNC__(n-1, l.slice(k), [a..., l[k]]) }) : a
}

cwr(2, %w(iced jam plain)).each {|a|
    say a.map{ .join(' ') }.join("\n")
}
```


Also built-in:


```ruby
%w(iced jam plain).combinations_with_repetition(2, {|*a|
    say a.join(' ')
})
```


{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain

```


Efficient count of the total number of combinations with repetition:

```ruby
func cwr_count (n, m) { binomial(n + m - 1, m) }
printf("\nThere are %s ways to pick 7 out of 10 with repetition\n", cwr_count(10, 7))
```

{{out}}

```txt

There are 11440 ways to pick 7 out of 10 with repetition

```



## Standard ML

{{trans|Haskell}}


```sml
let rec combs_with_rep k xxs =
  match k, xxs with
  | 0,  _ -> [[]]
  | _, [] -> []
  | k, x::xs ->
      List.map (fun ys -> x::ys) (combs_with_rep (k-1) xxs)
      @ combs_with_rep k xs
```


in the interactive loop:


```txt

- combs_with_rep (2, ["iced", "jam", "plain"]) ;
val it =
  [["iced","iced"],["iced","jam"],["iced","plain"],["jam","jam"],
   ["jam","plain"],["plain","plain"]] : string list list
- length (combs_with_rep (3, [1,2,3,4,5,6,7,8,9,10])) ;
val it = 220 : int

```



### Dynamic programming



```sml
fun combs_with_rep (m, xs) = let
  val arr = Array.array (m+1, [])
in
  Array.update (arr, 0, [[]]);
  app (fn x =>
    Array.modifyi (fn (i, y) =>
      if i = 0 then y else y @ map (fn xs => x::xs) (Array.sub (arr, i-1))
    ) arr
  ) xs;
  Array.sub (arr, m)
end
```


in the interactive loop:


```txt

- combs_with_rep (2, ["iced", "jam", "plain"]) ;
val it =
  [["iced","iced"],["jam","iced"],["jam","jam"],["plain","iced"],
   ["plain","jam"],["plain","plain"]] : string list list
- length (combs_with_rep (3, [1,2,3,4,5,6,7,8,9,10])) ;
val it = 220 : int

```



## Stata



```stata
function combrep(v,k) {
	n = cols(v)
	a = J(comb(n+k-1,k),k,v[1])
	u = J(1,k,1)
	for (i=2; 1; i++) {
		for (j=k; j>0; j--) {
			if (u[j]<n) break
		}
		if (j<1) return(a)
		m = u[j]+1
		for (; j<=k; j++) u[j] = m
		a[i,.] = v[u]
	}
}

combrep(("iced","jam","plain"),2)

a = combrep(1..10,3)
rows(a)
```


'''Output'''


```txt
           1       2
    +-----------------+
  1 |   iced    iced  |
  2 |   iced     jam  |
  3 |   iced   plain  |
  4 |    jam     jam  |
  5 |    jam   plain  |
  6 |  plain   plain  |
    +-----------------+

  220
```



## Swift


```Swift>func combosWithRep<T
(var objects: [T], n: Int) -> [[T]] {
  if n == 0 { return [[]] } else {
    var combos = [[T]]()
    while let element = objects.last {
      combos.appendContentsOf(combosWithRep(objects, n: n - 1).map{ $0 + [element] })
      objects.removeLast()
    }
    return combos
  }
}
print(combosWithRep(["iced", "jam", "plain"], n: 2).map {$0.joinWithSeparator(" and ")}.joinWithSeparator("\n"))
```

Output:

```txt
plain and plain
jam and plain
iced and plain
jam and jam
iced and jam
iced and iced
```



## Tcl


```tcl
package require Tcl 8.5
proc combrepl {set n {presorted no}} {
    if {!$presorted} {
        set set [lsort $set]
    }
    if {[incr n 0] < 1} {
	return {}
    } elseif {$n < 2} {
	return $set
    }
    # Recursive call
    set res [combrepl $set [incr n -1] yes]
    set result {}
    foreach item $set {
	foreach inner $res {
	    dict set result [lsort [list $item {*}$inner]] {}
	}
    }
    return [dict keys $result]
}

puts [combrepl {iced jam plain} 2]
puts [llength [combrepl {1 2 3 4 5 6 7 8 9 10} 3]]
```

{{out}}

```txt

{iced iced} {iced jam} {iced plain} {jam jam} {jam plain} {plain plain}
220

```


## TXR


```dos
txr -p "(rcomb '(iced jam plain) 2)"
```

{{out}}

```txt

((iced iced) (iced jam) (iced plain) (jam jam) (jam plain) (plain plain))

```

----

```dos
txr -p "(length-list (rcomb '(0 1 2 3 4 5 6 7 8 9) 3))"
```

{{out}}

```txt

220

```



## Ursala


```Ursala
#import std
#import nat

cwr = ~&s+ -<&*+ ~&K0=>&^|DlS/~& iota     # takes a set and a selection size

#cast %gLSnX

main = ^|(~&,length) cwr~~/(<'iced','jam','plain'>,2) ('1234567890',3)
```

{{out}}

```txt

(
   {
      <'iced','iced'>,
      <'iced','jam'>,
      <'iced','plain'>,
      <'jam','jam'>,
      <'jam','plain'>,
      <'plain','plain'>},
   220)

```



## XPL0


```XPL0
code ChOut=8, CrLf=9, IntOut=11, Text=12;
int  Count, Array(10);

proc Combos(D, S, K, N, Names); \Generate all size K combinations of N objects
int  D, S, K, N, Names;         \depth of recursion, starting value of N, etc.
int  I;
[if D<K then                    \depth < size
    [for I:= S to N-1 do
        [Array(D):= I;
        Combos(D+1, I, K, N, Names);
        ];
    ]
else [Count:= Count+1;
     if Names(0) then
        [for I:= 0 to K-1 do
            [Text(0, Names(Array(I)));  ChOut(0, ^ )];
        CrLf(0);
        ];
     ];
];

[Count:= 0;
Combos(0, 0, 2, 3, ["iced", "jam", "plain"]);
Text(0, "Combos = ");  IntOut(0, Count);  CrLf(0);
Count:= 0;
Combos(0, 0, 3, 10, [0]);
Text(0, "Combos = ");  IntOut(0, Count);  CrLf(0);
]
```


{{out}}

```txt

iced iced
iced jam
iced plain
jam jam
jam plain
plain plain
Combos = 6
Combos = 220

```



## zkl

{{trans|Clojure}}

```zkl
fcn combosK(k,seq){	// repeats, seq is finite
   if (k==1)    return(seq);
   if (not seq) return(T);
   self.fcn(k-1,seq).apply(T.extend.fp(seq[0])).extend(self.fcn(k,seq[1,*]));
}
```


```zkl
combosK(2,T("iced","jam","plain")).apply("concat",",");
combosK(3,T(0,1,2,3,4,5,6,7,8,9)).len();
```

{{out}}

```txt

L("iced,iced","iced,jam","iced,plain","jam,jam","jam,plain","plain,plain")
220

```



## ZX Spectrum Basic


```zxbasic
10 READ n
20 DIM d$(n,5)
30 FOR i=1 TO n
40 READ d$(i)
50 NEXT i
60 DATA 3,"iced","jam","plain"
70 FOR i=1 TO n
80 FOR j=i TO n
90 PRINT d$(i);" ";d$(j)
100 NEXT j
110 NEXT i
```

