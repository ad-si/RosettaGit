+++
title = "Knuth's algorithm S"
description = ""
date = 2019-10-18T13:26:02Z
aliases = []
[extra]
id = 10705
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "bbc_basic",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "d",
  "elena",
  "elixir",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "zkl",
]
+++

This is a method of randomly sampling n items from a set of M items, with equal probability; where M >= n and M, the number of items is unknown until the end.
This means that the equal probability sampling should be maintained  for all successive items > n as they become available (although the content of successive samples can change).


;The algorithm:
:* Select the first n items as the sample as they become available;
:* For the i-th item where i > n, have a random chance of n/i of keeping it.  If failing this chance, the sample remains the same.  If not, have it randomly (1/n) replace one of the previously selected n items of the sample.
:* Repeat   2<sup>nd</sup> step   for any subsequent items.


;The Task:
:* Create a function <code>s_of_n_creator</code> that given <math>n</math> the maximum sample size, returns a function <code>s_of_n</code> that takes one parameter, <code>item</code>.
:* Function  <code>s_of_n</code> when called with successive items returns an equi-weighted random sample of up to n of its items so far, each time it is called, calculated using Knuths Algorithm S.
:* Test your functions by printing and showing the frequency of occurrences of the selected digits from 100,000 repetitions of:
:::# Use the s_of_n_creator with n == 3 to generate an s_of_n.
:::# call s_of_n with each of the digits 0 to 9 in order, keeping the returned three digits of its random sampling from its last call with argument item=9.


Note: A class taking n and generating a callable instance/function might also be used.


;Reference:
* The Art of Computer Programming, Vol 2, 3.4.2 p.142


## Related tasks

* [[One of n lines in a file]]
* [[Accumulator factory]]





## Ada


Instead of defining a function S_of_N_Creator, we define a generic packgage with that name. The generic parameters are N (=Sample_Size) and the type of the items to be sampled:


```Ada
generic
   Sample_Size: Positive;
   type Item_Type is private;
package S_Of_N_Creator is

   subtype Index_Type is Positive range 1 .. Sample_Size;
   type Item_Array is array (Index_Type) of Item_Type;

   procedure Update(New_Item: Item_Type);
   function Result return Item_Array;

end S_Of_N_Creator;
```


Here is the implementation of that package:


```Ada
with Ada.Numerics.Float_Random, Ada.Numerics.Discrete_Random;

package body S_Of_N_Creator is

   package F_Rnd renames Ada.Numerics.Float_Random;
   F_Gen: F_Rnd.Generator;

   package D_Rnd is new Ada.Numerics.Discrete_Random(Index_Type);
   D_Gen: D_Rnd.Generator;

   Item_Count: Natural := 0; -- this is a global counter
   Sample: Item_Array; -- also used globally

   procedure Update(New_Item: Item_Type) is
   begin
      Item_Count := Item_Count + 1;
      if Item_Count <= Sample_Size then
         -- select the first Sample_Size items as the sample
         Sample(Item_Count) := New_Item;
      else
         -- for I-th item, I > Sample_Size: Sample_Size/I chance of keeping it
         if (Float(Sample_Size)/Float(Item_Count)) > F_Rnd.Random(F_Gen)  then
            -- randomly (1/Sample_Size) replace one of the items of the sample
            Sample(D_Rnd.Random(D_Gen)) := New_Item;
         end if;
      end if;
   end Update;

   function Result return Item_Array is
   begin
      Item_Count := 0; -- ready to start another run
      return Sample;
   end Result;

begin
   D_Rnd.Reset(D_Gen); -- at package instantiation, initialize rnd-generators
   F_Rnd.Reset(F_Gen);
end S_Of_N_Creator;
```


The main program:


```Ada
with S_Of_N_Creator, Ada.Text_IO;

procedure Test_S_Of_N is

   Repetitions: constant Positive := 100_000;
   type D_10 is range 0 .. 9;

   -- the instantiation of the generic package S_Of_N_Creator generates
   -- a package with the desired functionality
   package S_Of_3 is new S_Of_N_Creator(Sample_Size => 3, Item_Type => D_10);

   Sample: S_Of_3.Item_Array;
   Result: array(D_10) of Natural := (others => 0);

begin
   for J in 1 .. Repetitions loop
      -- get Sample
      for Dig in D_10 loop
         S_Of_3.Update(Dig);
      end loop;
      Sample := S_Of_3.Result;

      -- update current Result
      for Item in Sample'Range loop
         Result(Sample(Item)) := Result(Sample(Item)) + 1;
      end loop;
   end loop;

   -- finally: output Result
   for Dig in Result'Range loop
      Ada.Text_IO.Put(D_10'Image(Dig) & ":"
                        & Natural'Image(Result(Dig)) & ";   ");
   end loop;
end Test_S_Of_N;
```


A sample output:


```txt
 0: 30008;    1: 30056;    2: 30080;    3: 29633;    4: 29910;    5: 30293;    6: 30105;    7: 29924;    8: 29871;    9: 30120;
```



## BBC BASIC

At each of the 100000 repetitions not only is a new function created but also new copies of its PRIVATE variables '''index%''' and '''samples%()'''.  Creating such a large number of variables at run-time impacts adversely on execution speed and isn't to be recommended, other than to meet the artificial requirements of the task.

```bbcbasic
      HIMEM = PAGE + 20000000

      PRINT "Single run samples for n = 3:"
      SofN% = FNs_of_n_creator(3)
      FOR I% = 0 TO 9
        !^a%() = FN(SofN%)(I%)
        PRINT " For item " ; I% " sample(s) = " FNshowarray(a%(), I%+1)
      NEXT

      DIM cnt%(9)
      PRINT '"Digit counts after 100000 runs:"
      FOR rep% = 1 TO 100000
        IF (rep% MOD 1000) = 0 PRINT ; rep% ; CHR$(13) ;
        F% = FNs_of_n_creator(3)
        FOR I% = 0 TO 9
          !^a%() = FN(F%)(I%)
        NEXT
        cnt%(a%(1)) += 1 : cnt%(a%(2)) += 1 : cnt%(a%(3)) += 1
      NEXT
      FOR digit% = 0 TO 9
        PRINT " " ; digit% " : " ; cnt%(digit%)
      NEXT
      END

      REM Dynamically creates this function:
      REM DEF FNfunction(item%) : PRIVATE samples%(), index%
      REM DIM samples%(n%) : = FNs_of_n(item%, samples%(), index%)
      DEF FNs_of_n_creator(n%)
      LOCAL p%, f$
      f$ = "(item%) : " + CHR$&0E + " samples%(), index% : " + \
      \    CHR$&DE + " samples%(" + STR$(n%) + ") : = " + \
      \    CHR$&A4 + "s_of_n(item%, samples%(), index%)"
      DIM p% LEN(f$) + 4 : $(p%+4) = f$ : !p% = p%+4
      = p%

      DEF FNs_of_n(D%, s%(), RETURN I%)
      LOCAL N%
      N% = DIM(s%(),1)
      I% += 1
      IF I% <= N% THEN
        s%(I%) = D%
      ELSE
        IF RND(I%) <= N% s%(RND(N%)) = D%
      ENDIF
      = !^s%()

      DEF FNshowarray(a%(), n%)
      LOCAL i%, a$
      a$ = "["
      IF n% > DIM(a%(),1) n% = DIM(a%(),1)
      FOR i% = 1 TO n%
        a$ += STR$(a%(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(a$)) + "]"
```

'''Output:'''

```txt

Single run samples for n = 3:
 For item 0 sample(s) = [0]
 For item 1 sample(s) = [0, 1]
 For item 2 sample(s) = [0, 1, 2]
 For item 3 sample(s) = [0, 1, 2]
 For item 4 sample(s) = [0, 1, 4]
 For item 5 sample(s) = [0, 1, 4]
 For item 6 sample(s) = [0, 1, 6]
 For item 7 sample(s) = [0, 1, 6]
 For item 8 sample(s) = [8, 1, 6]
 For item 9 sample(s) = [8, 1, 9]

Digit counts after 100000 runs:
 0 : 30068
 1 : 30017
 2 : 30378
 3 : 29640
 4 : 30153
 5 : 29994
 6 : 29941
 7 : 29781
 8 : 29918
 9 : 30110

```



## C

Instead of returning a closure we set the environment in a structure:

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <time.h>

struct s_env {
    unsigned int n, i;
    size_t size;
    void *sample;
};

void s_of_n_init(struct s_env *s_env, size_t size, unsigned int n)
{
    s_env->i = 0;
    s_env->n = n;
    s_env->size = size;
    s_env->sample = malloc(n * size);
}

void sample_set_i(struct s_env *s_env, unsigned int i, void *item)
{
    memcpy(s_env->sample + i * s_env->size, item, s_env->size);
}

void *s_of_n(struct s_env *s_env, void *item)
{
    s_env->i++;
    if (s_env->i <= s_env->n)
        sample_set_i(s_env, s_env->i - 1, item);
    else if ((rand() % s_env->i) < s_env->n)
        sample_set_i(s_env, rand() % s_env->n, item);
    return s_env->sample;
}

int *test(unsigned int n, int *items_set, unsigned int num_items)
{
    int i;
    struct s_env s_env;
    s_of_n_init(&s_env, sizeof(items_set[0]), n);
    for (i = 0; i < num_items; i++) {
        s_of_n(&s_env, (void *) &items_set[i]);
    }
    return (int *)s_env.sample;
}

int main()
{
    unsigned int i, j;
    unsigned int n = 3;
    unsigned int num_items = 10;
    unsigned int *frequencies;
    int *items_set;
    srand(time(NULL));
    items_set = malloc(num_items * sizeof(int));
    frequencies = malloc(num_items * sizeof(int));
    for (i = 0; i < num_items; i++) {
        items_set[i] = i;
        frequencies[i] = 0;
    }
    for (i = 0; i < 100000; i++) {
        int *res = test(n, items_set, num_items);
        for (j = 0; j < n; j++) {
            frequencies[res[j]]++;
        }
	free(res);
    }
    for (i = 0; i < num_items; i++) {
        printf(" %d", frequencies[i]);
    }
    puts("");
    return 0;
}
```


Sample output:

```txt
 29980 29746 30111 30034 29922 29720 30222 30183 29995 30087
```



## C++

```cpp
#include <iostream>
#include <functional>
#include <vector>
#include <cstdlib>
#include <ctime>

template <typename T>
std::function<std::vector<T>(T)> s_of_n_creator(int n) {
  std::vector<T> sample;
  int i = 0;
  return [=](T item) mutable {
    i++;
    if (i <= n) {
      sample.push_back(item);
    } else if (std::rand() % i < n) {
      sample[std::rand() % n] = item;
    }
    return sample;
  };
}

int main() {
  std::srand(std::time(NULL));
  int bin[10] = {0};
  for (int trial = 0; trial < 100000; trial++) {
    auto s_of_n = s_of_n_creator<int>(3);
    std::vector<int> sample;
    for (int i = 0; i < 10; i++)
      sample = s_of_n(i);
    for (int s : sample)
      bin[s]++;
  }
  for (int x : bin)
    std::cout << x << std::endl;
  return 0;
}
```

```txt

30052
29740
30197
30223
29857
29688
30095
29803
30098
30247

```


Class-based version:

```cpp
#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>

template <typename T>
class SOfN {
  std::vector<T> sample;
  int i;
  const int n;
 public:
  SOfN(int _n) : i(0), n(_n) { }
  std::vector<T> operator()(T item) {
    i++;
    if (i <= n) {
      sample.push_back(item);
    } else if (std::rand() % i < n) {
      sample[std::rand() % n] = item;
    }
    return sample;
  }
};

int main() {
  std::srand(std::time(NULL));
  int bin[10] = {0};
  for (int trial = 0; trial < 100000; trial++) {
    SOfN<int> s_of_n(3);
    std::vector<int> sample;
    for (int i = 0; i < 10; i++)
      sample = s_of_n(i);
    for (std::vector<int>::const_iterator i = sample.begin(); i != sample.end(); i++)
      bin[*i]++;
  }
  for (int i = 0; i < 10; i++)
    std::cout << bin[i] << std::endl;
  return 0;
}
```



## Clojure

The Clojure approach to problems like this is to define a function which takes an accumulator state and an input item and produces the updated state.
Here the accumulator state is the current sample and the number of items processed.
This function is then used in a ''reduce'' call with an initial state and a list of items.

```clojure
(defn s-of-n-fn-creator [n]
  (fn [[sample iprev] item]
    (let [i (inc iprev)]
      (if (<= i n)
        [(conj sample item) i]
        (let [r (rand-int i)]
          (if (< r n)
            [(assoc sample r item) i]
            [sample i]))))))

(def s-of-3-fn (s-of-n-fn-creator 3))

(->> #(reduce s-of-3-fn [[] 0] (range 10))
    (repeatedly 100000)
    (map first)
    flatten
    frequencies
    sort
    println)

```

Sample output:
<lang>([0 29924] [1 30053] [2 30018] [3 29765] [4 29974] [5 30225] [6 30082] [7 29996] [8 30128] [9 29835])
```


If we really need a stateful (thread safe!) function for some reason, we can get it like this:

```clojure
(defn s-of-n-creator [n]
  (let [state (atom [[] 0])
        s-of-n-fn (s-of-n-fn-creator n)]
    (fn [item]
      (first (swap! state s-of-n-fn item)))))
```



## CoffeeScript


```coffeescript

s_of_n_creator = (n) ->
  arr = []
  cnt = 0
  (elem) ->
    cnt += 1
    if cnt <= n
      arr.push elem
    else
      pos = Math.floor(Math.random() * cnt)
      if pos < n
        arr[pos] = elem
    arr.sort()

sample_size = 3
range = [0..9]
num_trials = 100000

counts = {}

for digit in range
  counts[digit] = 0

for i in [1..num_trials]
  s_of_n = s_of_n_creator(sample_size)
  for digit in range
    sample = s_of_n(digit)
  for digit in sample
    counts[digit] += 1

for digit in range
  console.log digit, counts[digit]

```

output
<lang>
> coffee knuth_sample.coffee
0 29899
1 29841
2 29930
3 30058
4 29932
5 29948
6 30047
7 30114
8 29976
9 30255

```




## Common Lisp


```lisp
(defun s-n-creator (n)
  (let ((sample (make-array n :initial-element nil))
        (i 0))
    (lambda (item)
      (if (<= (incf i) n)
          (setf (aref sample (1- i)) item)
        (when (< (random i) n)
          (setf (aref sample (random n)) item)))
      sample)))

(defun algorithm-s ()
  (let ((*random-state* (make-random-state t))
        (frequency (make-array '(10) :initial-element 0)))
    (loop repeat 100000
          for s-of-n = (s-n-creator 3)
          do (flet ((s-of-n (item)
                      (funcall s-of-n item)))
               (map nil
                    (lambda (i)
                      (incf (aref frequency i)))
                    (loop for i from 0 below 9
                          do (s-of-n i)
                          finally (return (s-of-n 9))))))
    frequency))

(princ (algorithm-s))

```
output<lang>#(30026 30023 29754 30017 30267 29997 29932 29990 29965 30029)
```



## D


```d
import std.stdio, std.random;

auto sofN_creator(in int n) {
    size_t i;
    int[] sample;

    return (in int item) {
        i++;
        if (i <= n)
            sample ~= item;
        else if (uniform01 < (double(n) / i))
            sample[uniform(0, n)] = item;
        return sample;
    };
}

void main() {
    enum nRuns = 100_000;
    size_t[10] bin;

    foreach (immutable trial; 0 .. nRuns) {
        immutable sofn = sofN_creator(3);
        int[] sample;
        foreach (immutable item; 0 .. bin.length)
            sample = sofn(item);
        foreach (immutable s; sample)
            bin[s]++;
    }
    writefln("Item counts for %d runs:\n%s", nRuns, bin);
}
```

```txt
Item counts for 100000 runs:
[30191, 29886, 29988, 30149, 30251, 29997, 29748, 29909, 30041, 29840]
```



### Faster Version


```d
import std.stdio, std.random, std.algorithm;

struct SOfN(size_t n) {
    size_t i;
    int[n] sample = void;

    int[] next(in size_t item, ref Xorshift rng) {
        i++;
        if (i <= n)
            sample[i - 1] = item;
        else if (rng.uniform01 < (double(n) / i))
            sample[uniform(0, n, rng)] = item;
        return sample[0 .. min(i, $)];
    }
}

void main() {
    enum nRuns = 100_000;
    size_t[10] bin;
    auto rng = Xorshift(0);

    foreach (immutable trial; 0 .. nRuns) {
        SOfN!3 sofn;
        foreach (immutable item; 0 .. bin.length - 1)
            sofn.next(item, rng);
        foreach (immutable s; sofn.next(bin.length - 1, rng))
            bin[s]++;
    }
    writefln("Item counts for %d runs:\n%s", nRuns, bin);
}
```



## Elena

ELENA 4.1 :

```elena
import system'dynamic;
import extensions;
import system'routines;
import system'collections;

extension algorithmOp
{
    s_of_n()
    {
        var counter := new Integer();
        var n := self;

        ^ new ArrayList().mixInto(new::
        {
            eval(i)
            {
                counter.append:1;

                if (__target.Length < n)
                {
                    __target.append:i
                }
                else
                {
                    if(randomGenerator.nextInt:counter < n)
                        { __target[randomGenerator.nextInt:n] := i }
                };

                ^ __target.Value
            }
        })
    }
}

public program()
{
    var bin := Array.allocate(10).populate:(n => new Integer());
    for(int trial := 0, trial < 10000, trial += 1)
    {
        var s_of_n := 3.s_of_n();

        for(int n := 0, n < 10, n += 1)
        {
            var sample := s_of_n.eval:n;

            if (n == 9)
                { sample.forEach:(i){ bin[i].append:1 } }
        }
    };

    console.printLine:bin.readChar()
}
```

```txt

3050,3029,3041,2931,3040,2952,2901,2984,3069,3003

```



## Elixir


```elixir

defmodule Knuth do
  def s_of_n_creator(n), do: {n, 1, []}

  def s_of_n({n, i, ys}, x) do
    cond do
      i <= n -> {n, i+1, [x|ys]}

      :rand.uniform(i) <= n ->
        {n, i+1, List.replace_at(ys, :rand.uniform(n)-1, x)}

      true -> {n, i+1, ys}
    end
  end
end

results = Enum.reduce(1..100000, %{}, fn _, freq ->
  {_, _, xs} = Enum.reduce(1..10, Knuth.s_of_n_creator(3), fn x, s ->
    Knuth.s_of_n(s, x)
  end)
  Enum.reduce(xs, freq, fn x, freq ->
    Map.put(freq, x, (freq[x] || 0) + 1)
  end)
end)

IO.inspect results

```

Output:

```txt
%{1 => 30138, 2 => 29980, 3 => 29992, 4 => 29975, 5 => 30110, 6 => 29825,
  7 => 29896, 8 => 30188, 9 => 29898, 10 => 29998}
```


=={{header|F_Sharp|F#}}==

```fsharp

let N=System.Random 23 //Nigel Galloway: August 7th., 2018
let s_of_n_creator i = fun g->Seq.fold(fun (n:_[]) g->if N.Next()%(g+1)>i-1 then n else n.[N.Next()%i]<-g;n) (Array.ofSeq (Seq.take i g)) (Seq.skip i g)
let s_of_n<'n> = s_of_n_creator 3
printfn "using an input array -> %A" (List.init 100000 (fun _->s_of_n [|0..9|]) |> Array.concat |> Array.countBy id |> Array.sort)
printfn "using an input list  -> %A" (List.init 100000 (fun _->s_of_n [0..9]) |> Array.concat |> Array.countBy id |> Array.sort)

```

```txt

using an input array -> [|(0, 30162); (1, 30151); (2, 29894); (3, 29766); (4, 30117); (5, 29976); (6, 29916); (7, 29994); (8, 29890); (9, 30134)|]
using an input list  -> [|(0, 29936); (1, 29973); (2, 29880); (3, 30160); (4, 30126); (5, 30123); (6, 30062); (7, 30053); (8, 29892); (9, 29795)|]

```


## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func sOfNCreator(n int) func(byte) []byte {
    s := make([]byte, 0, n)
    m := n
    return func(item byte) []byte {
        if len(s) < n {
            s = append(s, item)
        } else {
            m++
            if rand.Intn(m) < n {
                s[rand.Intn(n)] = item
            }
        }
        return s
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    var freq [10]int
    for r := 0; r < 1e5; r++ {
        sOfN := sOfNCreator(3)
        for d := byte('0'); d < '9'; d++ {
            sOfN(d)
        }
        for _, d := range sOfN('9') {
            freq[d-'0']++
        }
    }
    fmt.Println(freq)
}
```

Output:

```txt

[30075 29955 30024 30095 30031 30018 29973 29642 30156 30031]

```



## Haskell


```haskell

import Control.Monad.Random
import Control.Monad.State
import qualified Data.Map as M
import System.Random

-- s_of_n_creator :: Int -> a -> RandT StdGen (State (Int, [a])) [a]
s_of_n_creator :: Int -> a -> StateT (Int, [a]) (Rand StdGen) [a]
s_of_n_creator n v = do
  (i, vs) <- get
  let i' = i + 1
  if i' <= n
    then do
      let vs' = v : vs
      put (i', vs')
      pure vs'
    else do
      j <- getRandomR (1, i')
      if j > n
        then do
          put (i', vs)
          pure vs
        else do
          k <- getRandomR (0, n - 1)
          let (f, (_ : b)) = splitAt k vs
              vs' = v : f ++ b
          put (i', vs')
          pure vs'

sample :: Int -> Rand StdGen [Int]
sample n =
  let s_of_n = s_of_n_creator n
   in snd <$> execStateT (traverse s_of_n [0 .. 9 :: Int]) (0, [])

incEach :: (Ord a, Num b) => M.Map a b -> [a] -> M.Map a b
incEach m ks = foldl (\m' k -> M.insertWith (+) k 1 m') m ks

sampleInc :: Int -> M.Map Int Double -> Rand StdGen (M.Map Int Double)
sampleInc n m = do
  s <- sample n
  pure $ incEach m s

main :: IO ()
main = do
  let counts = M.empty :: M.Map Int Double
      n = 100000
  gen <- getStdGen
  counts <- evalRandIO $ foldM (\c _ -> sampleInc 3 c) M.empty [1 .. n]
  print (fmap (/ n) counts)

```


=={{header|Icon}} and {{header|Unicon}}==

The following solution makes use of the <tt>makeProc</tt> procedure
defined in the <tt>UniLib</tt> library and so is Unicon specific.  However,
the solution can be modified to work in Icon as well.

Technically, <tt>s_of_n_creator</tt> returns a <i>co-expression</i>,
not a function.  In Unicon, the calling syntax for this
co-expression is indistinguishable from that of a function.

```unicon
import Utils

procedure main(A)
    freq := table(0)
    every 1 to (\A[2] | 100000)\1 do {
        s_of_n := s_of_n_creator(\A[1] | 3)
        every sample := s_of_n(0 to 9)
        every freq[!sample] +:= 1
        }
    every write(i := 0 to 9,": ",right(freq[i],6))
end

procedure s_of_n_creator(n)
    items := []
    itemCnt := 0.0
    return makeProc {
               repeat {
                   item := (items@&source)[1]
                   itemCnt +:= 1
                   if *items < n then put(items, item)
                   else if ?0 < (n/itemCnt) then ?items := item
                   }
               }
end
```

and a sample run:

```txt
->kas
0:  29941
1:  29963
2:  29941
3:  30005
4:  30087
5:  29895
6:  30075
7:  30059
8:  29962
9:  30072
->
```



## J


Note that this approach introduces heavy inefficiencies, to achieve information hiding.


```j
s_of_n_creator=: 1 :0
  ctx=: conew&'inefficient' m
  s_of_n__ctx
)

coclass'inefficient'
  create=:3 :0
    N=: y
    ITEMS=: ''
    K=:0
  )

  s_of_n=:3 :0
    K=: K+1
    if. N>:#ITEMS do.
      ITEMS=: ITEMS,y
    else.
      if. (N%K)>?0 do.
        ITEMS=: ((<<<?N){ITEMS),y
      else.
        ITEMS
      end.
    end.
  )

```


Explanation: <code>create</code> is the constructor for the class named <code>inefficient</code> and it initializes three properties: <code>N</code> (our initial value), <code>ITEMS</code> (an initially empty list) and <code>K</code> (a counter which is initially 0).

Also, we have <code>s_of_n</code> which is a method of that class. It increments K and appends to the list, respecting the random value replacement requirement, once the list has reached the required length.

Finally, we have <code>s_of_n_creator</code> which is not a method of that class, but which will create an object of that class and return the resulting s_of_n method.

Required example:


```j
run=:3 :0
  nl=. conl 1
  s3_of_n=. 3 s_of_n_creator
  r=. {: s3_of_n"0 i.10
  coerase (conl 1)-.nl
  r
)

   (~.,._1 + #/.~) (i.10),,D=:run"0 i.1e5
0 40119
1 40050
2 40163
3 57996
4 42546
5 40990
6 38680
7 36416
8 33172
9 29868
```


Here, we have each of our digits along with how many times each appeared in a result from <code>run</code>.

Explanation of <code>run</code>:

First, we get a snapshot of the existing objects in <code>nl</code>.

Then, we get our s3_of_n which is a method in a new object.

Then we run that method on each of the values 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9, keeping only the values from the last run, this will be the result of the run.

Then we delete any objects which did not previously exist.

Finally return our result.


## Java

A class-based solution:

```java
import java.util.*;

class SOfN<T> {
    private static final Random rand = new Random();

    private List<T> sample;
    private int i = 0;
    private int n;

    public SOfN(int _n) {
        n = _n;
        sample = new ArrayList<T>(n);
    }

    public List<T> process(T item) {
        if (++i <= n) {
            sample.add(item);
        } else if (rand.nextInt(i) < n) {
            sample.set(rand.nextInt(n), item);
        }
        return sample;
    }
}

public class AlgorithmS {
    public static void main(String[] args) {
        int[] bin = new int[10];
        for (int trial = 0; trial < 100000; trial++) {
            SOfN<Integer> s_of_n = new SOfN<Integer>(3);
            for (int i = 0; i < 9; i++) s_of_n.process(i);
            for (int s : s_of_n.process(9)) bin[s]++;
        }
        System.out.println(Arrays.toString(bin));
    }
}
```


Sample output:


```txt
[29965, 29690, 29911, 29818, 30109, 30250, 30085, 29857, 30191, 30124]
```


Alternative solution without using an explicitly named type; instead using an anonymous class implementing a generic "function" interface:

```java
import java.util.*;

interface Function<S, T> {
    public T call(S x);
}

public class AlgorithmS {
    private static final Random rand = new Random();
    public static <T> Function<T, List<T>> s_of_n_creator(final int n) {
        return new Function<T, List<T>>() {
            private List<T> sample = new ArrayList<T>(n);
            private int i = 0;
            public List<T> call(T item) {
                if (++i <= n) {
                    sample.add(item);
                } else if (rand.nextInt(i) < n) {
                    sample.set(rand.nextInt(n), item);
                }
                return sample;
            }
        };
    }

    public static void main(String[] args) {
        int[] bin = new int[10];
        for (int trial = 0; trial < 100000; trial++) {
            Function<Integer, List<Integer>> s_of_n = s_of_n_creator(3);
            for (int i = 0; i < 9; i++) s_of_n.call(i);
            for (int s : s_of_n.call(9)) bin[s]++;
        }
        System.out.println(Arrays.toString(bin));
    }
}
```


Sample output:


```txt
[29965, 30178, 29956, 29957, 30016, 30114, 29977, 29996, 29982, 29859]
```



## Julia

```julia
function makesofn(n::Integer)
    buf = Vector{typeof(n)}(0)
    i = 0
    return function sofn(item)
        i += 1
        if i ≤ n
            push!(buf, item)
        else
            j = rand(1:i)
            if j ≤ n buf[j] = item end
        end
        return buf
    end
end

nhist = zeros(Int, 10)
for _ in 1:10^5
    kas = makesofn(3)
    for j in 0:8 kas(j) end
    for k in kas(9) nhist[k+1] += 1 end
end

println("Simulating sof3(0:9) 100000 times:")
for (i, c) in enumerate(nhist)
    @printf("%5d → %5d\n", i-1, c)
end
```


```txt
Simulating sof3(0:9) 100000 times:
    0 → 29795
    1 → 29947
    2 → 30227
    3 → 30212
    4 → 29763
    5 → 29960
    6 → 29809
    7 → 30215
    8 → 29948
    9 → 30124
```



## Kotlin

Class based solution:

```scala
// version 1.2.51

import java.util.Random

val rand = Random()

class SOfN<T>(val n: Int) {
    private val sample = ArrayList<T>(n)
    private var i = 0

    fun process(item: T): List<T> {
        if (++i <= n)
            sample.add(item)
        else if (rand.nextInt(i) < n)
            sample[rand.nextInt(n)] = item
        return sample
    }
}

fun main(args: Array<String>) {
    val bin = IntArray(10)
    (1..100_000).forEach {
        val sOfn = SOfN<Int>(3)
        for (d in 0..8) sOfn.process(d)
        for (s in sOfn.process(9)) bin[s]++
    }
    println(bin.contentToString())
}
```

Sample output:

```txt

[29981, 29845, 29933, 30139, 30051, 30039, 29702, 30218, 30199, 29893]

```


Alternative function based solution:

```scala
// version 1.2.51

import java.util.Random

val rand = Random()

fun <T> SOfNCreator(n: Int): (T) -> List<T> {
    val sample = ArrayList<T>(n)
    var i = 0
    return {
        if (++i <= n)
            sample.add(it)
        else if (rand.nextInt(i) < n)
            sample[rand.nextInt(n)] = it
        sample
    }
}

fun main(args: Array<String>) {
    val bin = IntArray(10)
    (1..100_000).forEach {
        val sOfn = SOfNCreator<Int>(3)
        for (d in 0..8) sOfn(d)
        for (s in sOfn(9)) bin[s]++
    }
    println(bin.contentToString())
}
```


Sample output:

```txt

[30172, 29856, 30132, 29884, 29818, 30220, 29900, 30069, 29869, 30080]

```


=={{header|Objective-C}}==
Uses blocks

```objc>#import <Foundation/Foundation.h


typedef NSArray *(^SOfN)(id);

SOfN s_of_n_creator(int n) {
  NSMutableArray *sample = [[NSMutableArray alloc] initWithCapacity:n];
  __block int i = 0;
  return [^(id item) {
    i++;
    if (i <= n) {
      [sample addObject:item];
    } else if (rand() % i < n) {
      sample[rand() % n] = item;
    }
    return sample;
  } copy];
}

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSCountedSet *bin = [[NSCountedSet alloc] init];
    for (int trial = 0; trial < 100000; trial++) {
      SOfN s_of_n = s_of_n_creator(3);
      NSArray *sample;
      for (int i = 0; i < 10; i++)
        sample = s_of_n(@(i));
      [bin addObjectsFromArray:sample];
    }
    NSLog(@"%@", bin);

  }
  return 0;
}
```


Log:


```txt

<NSCountedSet: 0x100114120> (0 [29934], 9 [30211], 5 [29926], 1 [30067], 6 [30001], 2 [29972], 7 [30126], 3 [29944], 8 [29910], 4 [29909])

```



## OCaml



```ocaml
let s_of_n_creator n =
  let i = ref 0
  and sample = ref [| |] in
  fun item ->
    incr i;
    if !i <= n then sample := Array.append [| item |] !sample
    else if Random.int !i < n then !sample.(Random.int n) <- item;
    !sample

let test n items_set =
  let s_of_n = s_of_n_creator n in
  Array.fold_left (fun _ v -> s_of_n v) [| |] items_set

let () =
  Random.self_init();
  let n = 3 in
  let num_items = 10 in
  let items_set = Array.init num_items (fun i -> i) in
  let results = Array.create num_items 0 in
  for i = 1 to 100_000 do
    let res = test n items_set in
    Array.iter (fun j -> results.(j) <- succ results.(j)) res
  done;
  Array.iter (Printf.printf " %d") results;
  print_newline()
```


Output:


```txt
 30051 29899 30249 30058 30012 29836 29998 29882 30148 29867
```



## PARI/GP

```parigp
KnuthS(v,n)={
  my(u=vector(n,i,i));
  for(i=n+1,#v,
    if(random(i)<n,u[random(n)+1]=i)
  );
  vecextract(v,u)
};
test()={
  my(v=vector(10),t);
  for(i=1,1e5,
    t=KnuthS([0,1,2,3,4,5,6,7,8,9],3);
    v[t[1]+1]++;v[t[2]+1]++;v[t[3]+1]++
  );
  v
};
```


Output:

```txt
%1 = [30067, 30053, 29888, 30161, 30204, 29990, 30175, 29980, 29622, 29860]
```



## Perl


```perl
use strict;

sub s_of_n_creator {
    my $n = shift;
    my @sample;
    my $i = 0;
    sub {
        my $item = shift;
        $i++;
        if ($i <= $n) {
            # Keep first n items
            push @sample, $item;
        } elsif (rand() < $n / $i) {
            # Keep item
            @sample[rand $n] = $item;
        }
        @sample
    }
}

my @items = (0..9);
my @bin;

foreach my $trial (1 .. 100000) {
    my $s_of_n = s_of_n_creator(3);
    my @sample;
    foreach my $item (@items) {
        @sample = $s_of_n->($item);
    }
    foreach my $s (@sample) {
        $bin[$s]++;
    }
}
print "@bin\n";

```


;Sample output:

```txt
30003 29923 30192 30164 29994 29976 29935 29860 30040 29913
```


## Perl 6


```perl6
sub s_of_n_creator($n) {
    my @sample;
    my $i = 0;
    -> $item {
        if ++$i <= $n {
            push @sample, $item;
        }
        elsif $i.rand < $n {
            @sample[$n.rand] = $item;
        }
        @sample;
    }
}

my @items = 0..9;
my @bin;

for ^100000 {
    my &s_of_n = s_of_n_creator(3);
    my @sample;
    for @items -> $item {
        @sample = s_of_n($item);
    }
    for @sample -> $s {
        @bin[$s]++;
    }
}
say @bin;
```

Output:

```txt
29975 30028 30246 30056 30004 29983 29836 29967 29924 29981
```



## Phix

Phix does not support closures, but they are easy enough to emulate using {routine_id,environment}.

Obviously the direct call (as commented out) is inevitably going to be marginally faster, and

of course an s_of_n() that operated directly on local vars rather than elements of env, would be faster still.

Not that a mere 100,000 samples takes any method more than a tiny fraction of a second, you understand.

```Phix
enum RID, I, SAMPLE

function s_of_n(sequence env, integer item)
    integer i = env[I] + 1,
            n = length(env[SAMPLE])
    env[I] = i
    if i<=n then
        env[SAMPLE][i] = item
    elsif n/i>rnd() then
        env[SAMPLE][rand(n)] = item
    end if
    return env
end function

function s_of_n_creator(int n)
    return {routine_id("s_of_n"),0,repeat(0,n)}
end function

function invoke(sequence env, args)
    env = call_func(env[RID],prepend(args,env))
    return env
end function

function test(integer n, sequence items)
    sequence env = s_of_n_creator(n)
    for i=1 to length(items) do
--      env = s_of_n(env,items[i])
        env = invoke(env, {items[i]})
    end for
    return env[SAMPLE]
end function

procedure main()
    sequence items_set = tagset(9,0)
    sequence frequencies = repeat(0,length(items_set))
    for i=1 to 100000 do
        sequence res = test(3, items_set)
        for j=1 to length(res) do
            frequencies[res[j]+1] += 1
        end for
    end for
    ?frequencies
end procedure
main()
```

```txt

{29631,30097,29737,30252,29774,30147,29901,30042,30204,30215}

```

Note that s_of_n_creator() must match {RID, I, SAMPLE}. You might instead prefer (taking the appropriate care not to miss any!):

```Phix
enum RID, I, SAMPLE, CLOSURE_LEN=$
...
function s_of_n_creator(int n)
    sequence closure = repeat(0,CLOSURE_LEN)
    closure[RID] = routine_id("s_of_n")
    closure[I] = 0
    closure[SAMPLE] = repeat(0,n)
    return closure
end function
```



## PHP

```php
<?php
function s_of_n_creator($n) {
    $sample = array();
    $i = 0;
    return function($item) use (&$sample, &$i, $n) {
        $i++;
        if ($i <= $n) {
            // Keep first n items
            $sample[] = $item;
        } else if (rand(0, $i-1) < $n) {
            // Keep item
            $sample[rand(0, $n-1)] = $item;
        }
        return $sample;
    };
}

$items = range(0, 9);

for ($trial = 0; $trial < 100000; $trial++) {
    $s_of_n = s_of_n_creator(3);
    foreach ($items as $item)
        $sample = $s_of_n($item);
    foreach ($sample as $s)
        $bin[$s]++;
}
print_r($bin);
?>
```


;Sample output:

```txt
Array
(
    [3] => 30158
    [8] => 29859
    [9] => 29984
    [6] => 29937
    [7] => 30361
    [4] => 29994
    [5] => 29849
    [0] => 29724
    [1] => 29997
    [2] => 30137
)
```



## PicoLisp


```PicoLisp
(de s_of_n_creator (@N)
   (curry (@N (I . 0) (Res)) (Item)
      (cond
         ((>= @N (inc 'I)) (push 'Res Item))
         ((>= @N (rand 1 I)) (set (nth Res (rand 1 @N)) Item)) )
      Res ) )

(let Freq (need 10 0)
   (do 100000
      (let S_of_n (s_of_n_creator 3)
         (for I (mapc S_of_n (0 1 2 3 4 5 6 7 8 9))
            (inc (nth Freq (inc I))) ) ) )
   Freq )
```

Output:

```txt
-> (30003 29941 29918 30255 29848 29875 30056 29839 30174 30091)
```



## Python

```python
from random import randrange

def s_of_n_creator(n):
    sample, i = [], 0
    def s_of_n(item):
        nonlocal i

        i += 1
        if i <= n:
            # Keep first n items
            sample.append(item)
        elif randrange(i) < n:
            # Keep item
            sample[randrange(n)] = item
        return sample
    return s_of_n

if __name__ == '__main__':
    bin = [0]* 10
    items = range(10)
    print("Single run samples for n = 3:")
    s_of_n = s_of_n_creator(3)
    for item in items:
        sample = s_of_n(item)
        print("  Item: %i -> sample: %s" % (item, sample))
    #
    for trial in range(100000):
        s_of_n = s_of_n_creator(3)
        for item in items:
            sample = s_of_n(item)
        for s in sample:
            bin[s] += 1
    print("\nTest item frequencies for 100000 runs:\n ",
          '\n  '.join("%i:%i" % x for x in enumerate(bin)))
```


;Sample output:

```txt
Single run samples for n = 3:
  Item: 0 -> sample: [0]
  Item: 1 -> sample: [0, 1]
  Item: 2 -> sample: [0, 1, 2]
  Item: 3 -> sample: [0, 1, 3]
  Item: 4 -> sample: [0, 1, 3]
  Item: 5 -> sample: [0, 1, 3]
  Item: 6 -> sample: [0, 1, 3]
  Item: 7 -> sample: [0, 3, 7]
  Item: 8 -> sample: [0, 3, 7]
  Item: 9 -> sample: [0, 3, 7]

Test item frequencies for 100000 runs:
  0:29983
  1:30240
  2:29779
  3:29921
  4:30224
  5:29967
  6:30036
  7:30050
  8:29758
  9:30042
```



### Python Class based version

Only a slight change creates the following class-based implementation:

```python
class S_of_n_creator():
    def __init__(self, n):
        self.n = n
        self.i = 0
        self.sample = []

    def __call__(self, item):
        self.i += 1
        n, i, sample = self.n, self.i, self.sample
        if i <= n:
            # Keep first n items
            sample.append(item)
        elif randrange(i) < n:
            # Keep item
            sample[randrange(n)] = item
        return sample
```

The above can be instantiated as follows after which <code>s_of_n</code> can be called in the same way as it is in the first example where it is a function instead of an instance.

```python
s_of_n = S_of_n_creator(3)
```



## Racket


```racket
#lang racket/base

(define (s-of-n-creator n)
  (define i 0)
  (define sample (make-vector n)) ; the sample of n items
  (lambda (item)
    (set! i (add1 i))
    (cond [(<= i n)               ; we're not full, so kind of boring
           (vector-set! sample (sub1 i) item)]
          [(< (random i) n)       ; we've already seen n items; swap one?
           (vector-set! sample (random n) item)])
    sample))

(define counts (make-vector 10 0))

(for ([i 100000])
  (define s-of-n (s-of-n-creator 3))
  (define sample (for/last ([digit 10]) (s-of-n digit)))
  (for ([d sample]) (vector-set! counts d (add1 (vector-ref counts d)))))

(for ([d 10]) (printf "~a ~a\n" d (vector-ref counts d)))
```

Output:

```txt
0 30117
1 29955
2 30020
3 29906
4 30146
5 29871
6 30045
7 30223
8 29940
9 29777
```



## REXX


```rexx
/*REXX program  using  Knuth's  algorithm  S  (a random sampling   N   of   M   items). */
parse arg trials size .                          /*obtain optional arguments from the CL*/
if trials=='' | trials==","  then trials= 100000 /*Not specified?  Then use the default.*/
if   size=='' |   size==","  then   size=      3 /* "      "         "   "   "     "    */
#.= 0                                            /*initialize frequency counter array.  */
      do trials                                  /*OK,  now let's light this candle.    */
      call s_of_n_creator    size                /*create an initial list of  N  items. */

          do gen=0  for 10;  call s_of_n gen     /*call s_of_n with a single decimal dig*/
          end   /*gen*/
                                                 /* [↓]  examine what  SofN  generated. */
          do count=1  for size;     _= !.count   /*get a dec. digit from the  Nth item. */
          #._= #._ + 1                           /*bump counter for the decimal digit.  */
          end   /*count*/
      end       /*trials*/
                                                         @= ' trials, and with a size of '
hdr= "  Using Knuth's algorithm  S  for "  commas(trials)  @ || commas(size)":  "
say hdr;         say copies("═", length(hdr) )   /*display the header and its separator.*/

        do dig=0  to 9                           /* [↓]  display the frequency of a dig.*/
        say right("frequency of the", 37)       dig       'digit is: '      commas(#.dig)
        end   /*dig*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
s_of_n: parse arg item;         items= items + 1 /*get  "item",  bump the items counter.*/
        if random(1, items)>size  then return    /*probability isn't good,  so skip it. */
        _= random(1, size);       !._= item      /*now, figure out which previous ···   */
        return                                   /*      ··· item to replace with  ITEM.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
s_of_n_creator: parse arg item 1 items           /*generate    ITEM    number of items. */
                            do k=1  for item     /*traipse through the first  N  items. */
                            !.k= random(0, 9)    /*set the  Kth  item with random digit.*/
                            end   /*k*/
                return                           /*the piddly stuff is done  (for now). */
```

```txt

  Using Knuth's algorithm  S  for  100,000  trials, and with a size of 3:
═══════════════════════════════════════════════════════════════════════════
                     frequency of the 0 digit is:  29,879
                     frequency of the 1 digit is:  30,259
                     frequency of the 2 digit is:  30,254
                     frequency of the 3 digit is:  29,929
                     frequency of the 4 digit is:  30,022
                     frequency of the 5 digit is:  30,010
                     frequency of the 6 digit is:  29,692
                     frequency of the 7 digit is:  30,108
                     frequency of the 8 digit is:  29,976
                     frequency of the 9 digit is:  29,871

```



## Ruby

Using a closure

```ruby
def s_of_n_creator(n)
  sample = []
  i = 0
  Proc.new do |item|
    i += 1
    if i <= n
      sample << item
    elsif rand(i) < n
      sample[rand(n)] = item
    end
    sample
  end
end

frequency = Array.new(10,0)
100_000.times do
  s_of_n = s_of_n_creator(3)
  sample = nil
  (0..9).each {|digit| sample = s_of_n[digit]}
  sample.each {|digit| frequency[digit] += 1}
end

(0..9).each {|digit| puts "#{digit}\t#{frequency[digit]}"}
```

Example

```txt
0       29850
1       30015
2       29970
3       29789
4       29841
5       30075
6       30281
7       30374
8       29953
9       29852
```


## Scala

===Imperative (Ugly and side effects)===
```Scala
import java.util
import scala.util.Random

object KnuthsAlgorithmS extends App {

  import scala.collection.JavaConverters._

  val (n, rand, bin) = (3, Random, new Array[Int](10))

  for (_ <- 0 until 100000) {
    val sample = new util.ArrayList[Int](n)
    for (item <- 0 until 10) {
      if (item < n) sample.add(item)
      else if (rand.nextInt(item + 1) < n)
        sample.asScala(rand.nextInt(n)) = item
    }
    for (s <- sample.asScala.toList) bin(s) += 1
  }

  println(bin.mkString("[", ", ", "]"))
}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/nlldfXD/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/WLaee5H9T72cximqK9gECA Scastie (JVM)].


## Sidef

```ruby
func s_of_n_creator(n) {
    var i = 0
    var sample = []
    { |item|
        if (++i <= n) {
            sample << item;
        }
        elsif (i.rand < n) {
            sample[n.rand] = item;
        }
        sample;
    }
}

var items = 0..9;
var bin = [];

100000.times {
    var s_of_n = s_of_n_creator(3);
    var sample = []
    for item in items {
        sample = s_of_n(item);
    }
    for s in sample {
        bin[s] := 0 ++;
    }
}

say bin;
```

```txt

[30056, 29906, 30058, 29986, 30062, 29748, 29989, 29985, 30126, 30084]

```



## Rust


```rust
use rand::{Rng,weak_rng};

struct SofN<R: Rng+Sized, T> {
    rng: R,
    sample: Vec<T>,
    i: usize,
    n: usize,
}

impl<R: Rng, T> SofN<R, T> {
    fn new(rng: R, n: usize) -> Self {
        SofN{rng, sample: Vec::new(), i: 0, n}
    }

    fn add(&mut self, item: T) {
        self.i += 1;
        if self.i <= self.n {
            self.sample.push(item);
        } else if self.rng.gen_range(0, self.i) < self.n {
            self.sample[self.rng.gen_range(0, self.n)] = item;
        }
    }

    fn sample(&self) -> &Vec<T> {
        &self.sample
    }
}


pub fn main() {
    const MAX: usize = 10;
    let mut bin: [i32; MAX] = Default::default();
    for _ in 0..100000 {
        let mut s_of_n = SofN::new(weak_rng(), 3);

        for i in 0..MAX { s_of_n.add(i); }

        for s in s_of_n.sample() {
            bin[*s] += 1;
        }
    }

    for (i, x) in bin.iter().enumerate() {
        println!("frequency of {}: {}", i, x);
    }
}
```


```txt

frequency of 0: 29883
frequency of 1: 29901
frequency of 2: 29896
frequency of 3: 30029
frequency of 4: 30017
frequency of 5: 29850
frequency of 6: 30139
frequency of 7: 30252
frequency of 8: 30030
frequency of 9: 30003

```



## Swift


```swift
import Darwin

func s_of_n_creator<T>(n: Int) -> T -> [T]  {
  var sample = [T]()
  var i = 0
  return {(item: T) in
    i++
    if (i <= n) {
      sample.append(item)
    } else if (Int(arc4random_uniform(UInt32(i))) < n) {
      sample[Int(arc4random_uniform(UInt32(n)))] = item
    }
    return sample
  }
}

var bin = [Int](count:10, repeatedValue:0)
for trial in 0..<100000 {
  let s_of_n: Int -> [Int] = s_of_n_creator(3)
  var sample: [Int] = []
  for i in 0..<10 {
    sample = s_of_n(i)
  }
  for s in sample {
    bin[s]++
  }
}
println(bin)
```

```txt

[30038, 29913, 30047, 30069, 30159, 30079, 29773, 29962, 30000, 29960]

```



## Tcl


```tcl
package require Tcl 8.6

oo::class create SofN {
    variable items size count
    constructor {n} {
	set size $n
    }
    method item {item} {
	if {[incr count] <= $size} {
	    lappend items $item
	} elseif {rand()*$count < $size} {
	    lset items [expr {int($size * rand())}] $item
	}
	return $items
    }
}

# Test code
for {set i 0} {$i < 100000} {incr i} {
    set sOf3 [SofN new 3]
    foreach digit {0 1 2 3 4 5 6 7 8 9} {
	set digs [$sOf3 item $digit]
    }
    $sOf3 destroy
    foreach digit $digs {
	incr freq($digit)
    }
}
parray freq
```

Sample output:
```txt

freq(0) = 29812
freq(1) = 30099
freq(2) = 29927
freq(3) = 30106
freq(4) = 30048
freq(5) = 29993
freq(6) = 29912
freq(7) = 30219
freq(8) = 30060
freq(9) = 29824

```



## zkl


```zkl
fcn s_of_n_creator(n){
   fcn(item,ri,N,samples){
      i:=ri.inc(); // 1,2,3,4,...
      if(i<=N) samples.append(item);
      else if ((0).random(i) < N) samples[(0).random(N)] = item;
      samples
   }.fp1(Ref(1),n,L())
}
```

One run:

```zkl
s3:=s_of_n_creator(3);
[0..9].pump(List,s3,"copy").println();
```

```txt

L(L(0),L(0,1),L(0,1,2),L(0,1,2),L(0,4,2),L(5,4,2),L(5,6,2),L(5,6,2),L(5,6,2),L(9,6,2))

```

100,000 runs:

```zkl
dist:=L(0,0,0,0,0,0,0,0,0,0);
do(0d100_000){
   (0).pump(10,Void,s_of_n_creator(3)).apply2('wrap(n){dist[n]=dist[n]+1})
}
N:=dist.sum();
dist.apply('wrap(n){"%.2f%%".fmt(n.toFloat()/N*100)}).println();
```

```txt
L("10.00%","9.98%","10.00%","9.99%","10.00%","9.98%","10.01%","10.04%","9.98%","10.02%")
```

