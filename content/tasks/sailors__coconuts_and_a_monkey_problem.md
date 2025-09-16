+++
title = "Sailors, coconuts and a monkey problem"
description = ""
date = 2019-03-26T17:25:30Z
aliases = []
[extra]
id = 19083
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "awk",
  "bc",
  "befunge",
  "bracmat",
  "c",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "forth",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "ubasic_4th",
  "vba",
  "yabasic",
  "zkl",
]
+++

## Task

{{task}} [[Category:Puzzles]]
Five sailors are shipwrecked on an island and collect a large pile of coconuts during the day.

That night the first sailor wakes up and decides to take his first share early  so tries to divide the pile of coconuts equally into five piles but finds that there is one coconut left over, so he tosses it to a monkey and then hides "his" one of the five equally sized piles of coconuts and pushes the other four piles together to form a single visible pile of coconuts again and goes to bed.

To cut a long story short, each of the sailors in turn gets up once during the night and performs the same actions of dividing the coconut pile into five, finding that one coconut is left over and giving that single remainder coconut to the monkey.

In the morning (after the surreptitious and separate action of each of the five sailors during the night), the remaining coconuts are divided into five equal piles for each of the sailors, whereupon it is found that the pile of coconuts divides equally amongst the sailors with no remainder. (Nothing for the monkey in the morning.)


;The task:
# Calculate the minimum possible size of the initial pile of coconuts collected during the first day.
# Use a method that assumes an answer is possible, and then applies the constraints of the tale to see if it is correct. (I.e. no applying some formula that generates the correct answer without integer divisions and remainders and tests on remainders; but constraint solvers ''are'' allowed.)
# Calculate the size of the initial pile of coconuts if six sailors were marooned and went through a similar process (but split into six piles instead of five of course).
# Show your answers here.


;Extra credit (optional):
* Give some indication of the number of coconuts each sailor hides during the night.


;Note:
* Of course the tale is told in a world where the collection of any amount of coconuts in a day and multiple divisions of the pile, etc can occur in time fitting the story line, so as not to affect the mathematics.
* The tale is also told in a version where the monkey also gets a coconut in the morning. This is ''not'' that tale!


;C.f:
* [https://www.youtube.com/watch?v=U9qU20VmvaU Monkeys and Coconuts - Numberphile] (Video) Analytical solution.
* [http://oeis.org/A002021 A002021 Pile of coconuts problem] The On-Line Encyclopedia of Integer Sequences. (Although some of its references may use the alternate form of the tale).





## AWK


```AWK

# syntax: GAWK -f SAILORS_COCONUTS_AND_A_MONKEY_PROBLEM.AWK
# converted from LUA
BEGIN {
    for (n=2; n<=9; n++) {
      x = 0
      while (!valid(n,x)) {
        x++
      }
      printf("%d %d\n",n,x)
    }
    exit(0)
}
function valid(n,nuts,  k) {
    k = n
    while (k != 0) {
      if ((nuts % n) != 1) {
        return(0)
      }
      k--
      nuts = nuts - 1 - int(nuts / n)
    }
    return((nuts != 0) && (nuts % n == 0))
}

```

```txt

2 11
3 25
4 765
5 3121
6 233275
7 823537
8 117440505
9 387420481

```


## Bc

This script implements a solution in the coconuts function for a number of sailors > 1 and a number of monkeys between 1 and sailors-1. It also executes the coconuts function for some values of sailors/monkeys.


```Bc
define coconuts(sailors, monkeys) {
	print "coconuts(", sailors, ", ", monkeys, ") = "
	if (sailors < 2 || monkeys < 1 || sailors <= monkeys) {
		return 0
	}
	blue_cocos = sailors-1
	pow_bc = blue_cocos^sailors
	x_cocos = pow_bc
	while ((x_cocos-blue_cocos)%sailors || (x_cocos-blue_cocos)/sailors < 1) {
		x_cocos += pow_bc
	}
	return (x_cocos/pow_bc*(sailors^sailors)-blue_cocos)*monkeys
}
scale = 0
coconuts(1, 1)
coconuts(2, 1)
coconuts(3, 1)
coconuts(3, 2)
coconuts(4, 1)
coconuts(5, 1)
coconuts(5, 4)
coconuts(6, 1)
coconuts(101, 1)
```


```txt
$ time bc <coconuts_bc.in
coconuts(1, 1) = 0
coconuts(2, 1) = 11
coconuts(3, 1) = 25
coconuts(3, 2) = 50
coconuts(4, 1) = 765
coconuts(5, 1) = 3121
coconuts(5, 4) = 12484
coconuts(6, 1) = 233275
coconuts(101, 1) = 2731861967715741354199866657915606142014717766608\
81280465910305960827252944980667223385057449021203688309007889238399\
91099564447458450075226030128555294655577015766113909738825769262480\
452415909200510001

real    0m0.141s
user    0m0.031s
sys     0m0.062s
```



## Befunge

This is a translation of the second C solution. The output lists the number of sailors, the size of the original pile, and the final share each sailor receives the following morning.


```befunge
>2+:01p9>
`#@_00v
nvg10*g10:+>#1$<
#>\:01g1-%#^_:0v
-|:-1\+1<+/-1g1<
1>$01g.">-",,48v
^g10,+55<.,9.,*<
```


```txt
2 -> 11         1
3 -> 25         2
4 -> 765        60
5 -> 3121       204
6 -> 233275     13020
7 -> 823537     39990
8 -> 117440505  5044200
9 -> 387420481  14913080
```



## Bracmat

{{trans|Tcl}} (Though without the <code>assert</code> procedure.)

```bracmat
( ( divmod
  =   a b
    . !arg:(?a.?b)&(div$(!a.!b).mod$(!a.!b))
  )
& ( overnight
  =   ns nn result s q r
    .   !arg:(?ns.?nn)
      & :?result
      & 0:?s
      &   whl
        ' ( !s+1:?s:~>!ns
          & divmod$(!nn.!ns):(?q.?r)
          & !r:1
          & !q*(!ns+-1):?nn
          & !result (!s.!q.!r.!nn):?result
          )
      & !s:>!ns
      & divmod$(!nn.!ns):(?q.0)
      & !result
  )
& ( minnuts
  =   nsailors nnuts result sailor takes gives leaves
    .   !arg:?nsailors
      & 0:?nnuts
      &   whl
        ' ( 1+!nnuts:?nnuts
          & ~(overnight$(!nsailors.!nnuts):?result)
          )
      & out$(!nsailors ": " !nnuts)
      &   whl
        ' ( !result:(?sailor.?takes.?gives.?leaves) ?result
          &   out
            $ ( str
              $ ( " Sailor #"
                  !sailor
                  " takes "
                  !takes
                  ", giving "
                  !gives
                  " to the monkey and leaves "
                  !leaves
                )
              )
          )
      &   out
        $ ( str
          $ ("In the morning, each sailor gets " !leaves*!nsailors^-1 " nuts")
          )
  )
& 4:?n
&   whl
  ' ( 1+!n:~>6:?n
    & out$("Solution with " !n " sailors:")
    & minnuts$!n
    )
)
```


Output:

```txt
Solution with  5  sailors:
5 :  3121
 Sailor #1 takes 624, giving 1 to the monkey and leaves 2496
 Sailor #2 takes 499, giving 1 to the monkey and leaves 1996
 Sailor #3 takes 399, giving 1 to the monkey and leaves 1596
 Sailor #4 takes 319, giving 1 to the monkey and leaves 1276
 Sailor #5 takes 255, giving 1 to the monkey and leaves 1020
In the morning, each sailor gets 204 nuts
Solution with  6  sailors:
6 :  233275
 Sailor #1 takes 38879, giving 1 to the monkey and leaves 194395
 Sailor #2 takes 32399, giving 1 to the monkey and leaves 161995
 Sailor #3 takes 26999, giving 1 to the monkey and leaves 134995
 Sailor #4 takes 22499, giving 1 to the monkey and leaves 112495
 Sailor #5 takes 18749, giving 1 to the monkey and leaves 93745
 Sailor #6 takes 15624, giving 1 to the monkey and leaves 78120
In the morning, each sailor gets 13020 nuts
```



## C


```c
#include <stdio.h>

int valid(int n, int nuts)
{
	int k;
	for (k = n; k; k--, nuts -= 1 + nuts/n)
		if (nuts%n != 1) return 0;
	return nuts && !(nuts%n);
}

int main(void)
{
	int n, x;
	for (n = 2; n < 10; n++) {
		for (x = 0; !valid(n, x); x++);
		printf("%d: %d\n", n, x);
	}
	return 0;
}
```

```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481

```


But it's faster to search backwards: if everyone receives some coconuts,
see if we can backtrack to the original pile:

```c
#include <stdio.h>

// calculates if everyone got some nuts in the end, what was the original pile
// returns 0 if impossible
int total(int n, int nuts)
{
	int k;
	for (k = 0, nuts *= n; k < n; k++) {
		if (nuts % (n-1)) return 0;
		nuts += nuts / (n-1) + 1;
	}
	return nuts;
}

int main(void)
{
	int n, x, t;
	for (n = 2; n < 10; n++) {
		for (x = 1, t = 0; !(t = total(n, x)); x++);
		printf("%d: %d\t%d\n", n, t, x);
	}
	return 0;
}
```

sailers: original pile, final share

```txt

2: 11   1
3: 25   2
4: 765  60
5: 3121 204
6: 233275       13020
7: 823537       39990
8: 117440505    5044200
9: 387420481    14913080

```



## C++

```cpp
#include <iostream>

bool valid(int n, int nuts) {
    for (int k = n; k != 0; k--, nuts -= 1 + nuts / n) {
        if (nuts % n != 1) {
            return false;
        }
    }

    return nuts != 0 && (nuts % n == 0);
}

int main() {
    int x = 0;
    for (int n = 2; n < 10; n++) {
        while (!valid(n, x)) {
            x++;
        }
        std::cout << n << ": " << x << std::endl;
    }

    return 0;
}
```

```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481
```



## C#

```c#
class Test
{
    static bool valid(int n, int nuts)
    {
        for (int k = n; k != 0; k--, nuts -= 1 + nuts / n)
        {
            if (nuts % n != 1)
            {
                return false;
            }
        }

        return nuts != 0 && (nuts % n == 0);
    }

    static void Main(string[] args)
    {
        int x = 0;
        for (int n = 2; n < 10; n++)
        {
            while (!valid(n, x))
                x++;
            System.Console.WriteLine(n + ": " + x);
        }
    }
}
```



```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481
```



## D

```D

import std.stdio;

void main() {
    auto coconuts = 11;

    outer:
    foreach (ns; 2..10) {
        int[] hidden = new int[ns];
        coconuts = (coconuts / ns) * ns + 1;
        while (true) {
            auto nc = coconuts;
            foreach (s; 1..ns+1) {
                if (nc % ns == 1) {
                    hidden[s-1] = nc/ns;
                    nc -= hidden[s-1] + 1;
                    if (s==ns && nc%ns==0) {
                        writeln(ns, " sailors require a minimum of ", coconuts, " coconuts");
                        foreach (t; 1..ns+1) {
                            writeln("\tSailor ", t, " hides ", hidden[t - 1]);
                        }
                        writeln("\tThe monkey gets ", ns);
                        writeln("\tFinally, each sailor takes ", nc / ns);
                        continue outer;
                    }
                } else {
                    break;
                }
            }
            coconuts += ns;
        }
    }
}

```


```txt

2 sailors require a minimum of 11 coconuts
	Sailor 1 hides 5
	Sailor 2 hides 2
	The monkey gets 2
	Finally, each sailor takes 1

3 sailors require a minimum of 25 coconuts
	Sailor 1 hides 8
	Sailor 2 hides 5
	Sailor 3 hides 3
	The monkey gets 3
	Finally, each sailor takes 2

4 sailors require a minimum of 765 coconuts
	Sailor 1 hides 191
	Sailor 2 hides 143
	Sailor 3 hides 107
	Sailor 4 hides 80
	The monkey gets 4
	Finally, each sailor takes 60

5 sailors require a minimum of 3121 coconuts
	Sailor 1 hides 624
	Sailor 2 hides 499
	Sailor 3 hides 399
	Sailor 4 hides 319
	Sailor 5 hides 255
	The monkey gets 5
	Finally, each sailor takes 204

6 sailors require a minimum of 233275 coconuts
	Sailor 1 hides 38879
	Sailor 2 hides 32399
	Sailor 3 hides 26999
	Sailor 4 hides 22499
	Sailor 5 hides 18749
	Sailor 6 hides 15624
	The monkey gets 6
	Finally, each sailor takes 13020

7 sailors require a minimum of 823537 coconuts
	Sailor 1 hides 117648
	Sailor 2 hides 100841
	Sailor 3 hides 86435
	Sailor 4 hides 74087
	Sailor 5 hides 63503
	Sailor 6 hides 54431
	Sailor 7 hides 46655
	The monkey gets 7
	Finally, each sailor takes 39990

8 sailors require a minimum of 117440505 coconuts
	Sailor 1 hides 14680063
	Sailor 2 hides 12845055
	Sailor 3 hides 11239423
	Sailor 4 hides 9834495
	Sailor 5 hides 8605183
	Sailor 6 hides 7529535
	Sailor 7 hides 6588343
	Sailor 8 hides 5764800
	The monkey gets 8
	Finally, each sailor takes 5044200

9 sailors require a minimum of 387420481 coconuts
	Sailor 1 hides 43046720
	Sailor 2 hides 38263751
	Sailor 3 hides 34012223
	Sailor 4 hides 30233087
	Sailor 5 hides 26873855
	Sailor 6 hides 23887871
	Sailor 7 hides 21233663
	Sailor 8 hides 18874367
	Sailor 9 hides 16777215
	The monkey gets 9
	Finally, each sailor takes 14913080

```



## Elixir

### Brute Force


```elixir
defmodule RC do
  def valid?(sailor, nuts), do: valid?(sailor, nuts, sailor)

  def valid?(sailor, nuts, 0), do: nuts > 0 and rem(nuts,sailor) == 0
  def valid?(sailor, nuts, _) when rem(nuts,sailor)!=1, do: false
  def valid?(sailor, nuts, i) do
    valid?(sailor, nuts - div(nuts,sailor) - 1, i-1)
  end
end

Enum.each([5,6], fn sailor ->
  nuts = Enum.find(Stream.iterate(sailor, &(&1+1)), fn n -> RC.valid?(sailor, n) end)
  IO.puts "\n#{sailor} sailors => #{nuts} coconuts"
  Enum.reduce(0..sailor, nuts, fn _,n ->
    {d, r} = {div(n,sailor), rem(n,sailor)}
    IO.puts "  #{inspect [n, d, r]}"
    n - 1 - d
  end)
end)
```


```txt

5 sailors => 3121 coconuts
  [3121, 624, 1]
  [2496, 499, 1]
  [1996, 399, 1]
  [1596, 319, 1]
  [1276, 255, 1]
  [1020, 204, 0]

6 sailors => 233275 coconuts
  [233275, 38879, 1]
  [194395, 32399, 1]
  [161995, 26999, 1]
  [134995, 22499, 1]
  [112495, 18749, 1]
  [93745, 15624, 1]
  [78120, 13020, 0]

```



### Faster version


```elixir
defmodule Sailor do
  def coconuts(sailor), do: coconuts(sailor, sailor)
  defp coconuts(sailor, nuts) do
    if n = do_coconuts(sailor, nuts, sailor), do: n, else: coconuts(sailor, nuts+sailor)
  end

  defp do_coconuts(_sailor, nuts, 0), do: nuts
  defp do_coconuts(sailor, nuts, _) when rem(nuts, sailor-1) != 0, do: nil
  defp do_coconuts(sailor, nuts, i) do
    do_coconuts(sailor, nuts + div(nuts, sailor-1) + 1, i-1)
  end
end

Enum.each(2..9, fn sailor ->
  IO.puts "#{sailor}: #{Sailor.coconuts(sailor)}"
end)
```


```txt

2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481

```



## Forth

<lang>: total
  over * over 1- rot 0 ?do
    over over mod if dup xor swap leave else over over / 1+ rot + swap then
  loop drop
;

: sailors
  1+ 2 ?do
    1 begin i over total dup 0= while drop 1+ repeat cr i 0 .r ." : " . .
  loop
;

9 sailors
```

```txt
2: 11 1
3: 25 2
4: 765 60
5: 3121 204
6: 233275 13020
7: 823537 39990
8: 117440505 5044200
9: 387420481 14913080  ok
```



## Go

```go
package main

import "fmt"

func main() {
    coconuts := 11
outer:
    for ns := 2; ns < 10; ns++ {
        hidden := make([]int, ns)
        coconuts = (coconuts/ns)*ns + 1
        for {
            nc := coconuts
            for s := 1; s <= ns; s++ {
                if nc%ns == 1 {
                    hidden[s-1] = nc / ns
                    nc -= hidden[s-1] + 1
                    if s == ns && nc%ns == 0 {
                        fmt.Println(ns, "sailors require a minimum of", coconuts, "coconuts")
                        for t := 1; t <= ns; t++ {
                            fmt.Println("\tSailor", t, "hides", hidden[t-1])
                        }
                        fmt.Println("\tThe monkey gets", ns)
                        fmt.Println("\tFinally, each sailor takes", nc/ns, "\b\n")
                        continue outer
                    }
                } else {
                    break
                }
            }
            coconuts += ns
        }
    }
}
```


```txt

Same as Kotlin entry.

```



## Haskell


This program works by applying a function to increasing multiples of the number of sailors. The function takes a potential final number of coconuts (at the time the sailors awaken) and works backwards to get to the initial number of coconuts. At every step, it will abort the computation if the current number of coconuts can't arise as a result of splitting the previous pile.


```haskell
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

-- Takes the number of sailors and the final number of coconuts. Returns
-- Just the associated initial number of coconuts and Nothing otherwise.
tryFor :: Int -> Int -> Maybe Int
tryFor s = foldr (>=>) pure $ replicate s step
  where
    step n
      | n `mod` (s - 1) == 0 = Just $ n * s `div` (s - 1) + 1
      | otherwise = Nothing

-- Gets the number of sailors from the first command-line argument and
-- assumes 5 as a default if none is given. Then uses tryFor to find the
-- smallest solution.
main :: IO ()
main = do
  args <- getArgs
  let n =
        case args of
          [] -> 5
          s:_ -> read s
      a = head . mapMaybe (tryFor n) $ [n,2 * n ..]
  print a
```

Examples:

```txt

$ ./coconuts
3121
$ ./coconuts 4
765
$ ./coconuts 6
233275

```



## J


Here, we assume an answer which is less than 10000, and try each possibility, constraining ourselves to the list of answers which are valid. (As it happens, there's only one of those.)


```J
   I.(=<.)%&5 verb def'4*(y-1)%5'^:5 i.10000
3121
```


These sailors must count coconuts extremely quickly.

When we do this with six sailors, it turns out that we have to assume a larger initial value:


```J
   I.(=<.)%&6 verb def'5*(y-1)%6'^:6 i.1000000
233275 513211 793147
```


If it were not obvious which of the answers here was the minimum value we could additionally pick the smallest value from the list. But that would require typing two extra characters (for example, replace I. with i.&1), and most people already have so much trouble reading J that the extra code would surely be too much.


## Java

```java
public class Test {

    static boolean valid(int n, int nuts) {
        for (int k = n; k != 0; k--, nuts -= 1 + nuts / n)
            if (nuts % n != 1)
                return false;
        return nuts != 0 && (nuts % n == 0);
    }

    public static void main(String[] args) {
        int x = 0;
        for (int n = 2; n < 10; n++) {
            while (!valid(n, x))
                x++;
            System.out.printf("%d: %d%n", n, x);
        }
    }
}
```


```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481
```



## JavaScript



### ES5

( As in the recursive Python example )


```JavaScript
(function () {

    // wakeSplit :: Int -> Int -> Int -> Int
    function wakeSplit(intNuts, intSailors, intDepth) {
        var nDepth = intDepth !== undefined ? intDepth : intSailors,
            portion = Math.floor(intNuts / intSailors),
            remain = intNuts % intSailors;

        return 0 >= portion || remain !== (nDepth ? 1 : 0) ?
            null : nDepth ? wakeSplit(
                intNuts - portion - remain, intSailors, nDepth - 1
            ) : intNuts;
    }

    // TEST for 5, 6, and 7 intSailors
    return [5, 6, 7].map(function (intSailors) {
        var intNuts = intSailors;

        while (!wakeSplit(intNuts, intSailors)) intNuts += 1;

        return intNuts;
    });
})();
```


```JavaScript
[3121, 233275, 823537]
```



### ES6


Adding just a touch of curry to the coconuts.
(See the Rosetta Code task: [[Currying]])


```JavaScript
(() => {

    // wakeSplit :: Int -> Int -> Int -> Int
    let wakeSplit = (intSailors, intNuts, intDepth) => {
        let nDepth = intDepth !== undefined ? intDepth : intSailors,
            portion = Math.floor(intNuts / intSailors),
            remain = intNuts % intSailors;

        return 0 >= portion || remain !== (nDepth ? 1 : 0) ?
            null : nDepth ? wakeSplit(
                intSailors, intNuts - portion - remain, nDepth - 1
            ) : intNuts;
    };


    //GENERIC FUNCTIONS

    // curry :: ((a, b) -> c) -> a -> b -> c
    let curry = f => a => b => f(a, b),

        // until :: (a -> Bool) -> (a -> a) -> a -> a
        until = (p, f, x) => {
            let v = x;
            while (!p(v)) v = f(v);
            return v;
        },

        // succ :: Int -> Int
        succ = x => x + 1;


    // TEST for 5, 6, and 7 Sailors
    return [5, 6, 7].map(intSailors => {
        let intNuts = intSailors,
            test = curry(wakeSplit)(intSailors);

        return until(test, succ, intNuts);

    });
})();
```


```JavaScript
[3121, 233275, 823537]
```



## jq

The first solution presented in this section is based on a simulation of the nighttime and daytime activities, and the second (much faster) solution works backwards.

If your jq does not have "until" defined, here is its definition:

```jq
def until(cond; next): def _until: if cond then . else (next|_until) end; _until;
```



### Simulation


```jq
# If n (the input) is an admissible number of coconuts with respect to
# the night-time squirreling away of the coconuts by "sailors" sailors, then give 1 to the
# monkey, let one sailor squirrel away (1/sailors) coconuts, and yield the remaining number;
# otherwise, return false:
def squirrel(sailors):
  def admissible:  if . then (. % sailors) == 1 else . end;

  if admissible then  . - ((. - 1) / sailors) - 1
  else false
  end;

def nighttime(sailors):
  reduce range(0; sailors) as $i (.; squirrel(sailors));

def morning(sailors):
  if . then (. % sailors) == 0
  else false
  end;

# Test whether the input is a valid number of coconuts with respect to the story:
def valid(sailors): nighttime(sailors) | morning(sailors);
```

'''Five sailors''':
Find the minimum number of coconuts if there are 5 sailors --
start at 1 as there must be at least one to give to the monkey during the night:

```jq
1 | until( valid(5); . + 1)
```

3121

'''Six sailors''':
Find the minimum number of coconuts if there are 6 sailors --
start at 1 as there must be at least one to give to the monkey during the night:

```jq
1 | until( valid(6); . + 1)
```

233275


### Working backwards


```jq
# If n (the input) is the number of coconuts remaining after
# the surreptitious squirreling away by one sailor,
# then emit the number of coconuts which that sailor originally
# saw if n is admissible, otherwise emit false:
def unsquirrel(sailors):
  if . and (. % (sailors - 1) == 0)
  then 1 + (sailors * (. / (sailors - 1)))
  else false
  end;

# If in the end each sailor received n coconuts (where n is the input), how many coconuts
# were there initially?
def backwards(sailors):
  reduce range(0; sailors) as $i (. * sailors; unsquirrel(sailors));

def solve:
  . as $sailors
  # state: [ final_number_per_sailor, original_number_of_coconuts]
  | [-1] | until( .[1]; .[0] += 1 | .[1] = (.[0] | backwards($sailors)) )
  | "With \($sailors) sailors, there were originally \(.[1]) coconuts,"+
    " and each sailor finally ended up with \(.[0])." ;

range(2;9) | solve
```

```sh
With 2 sailors, there were originally 3 coconuts, and each sailor finally ended up with 0.
With 3 sailors, there were originally 25 coconuts, and each sailor finally ended up with 2.
With 4 sailors, there were originally 765 coconuts, and each sailor finally ended up with 60.
With 5 sailors, there were originally 3121 coconuts, and each sailor finally ended up with 204.
With 6 sailors, there were originally 233275 coconuts, and each sailor finally ended up with 13020.
With 7 sailors, there were originally 823537 coconuts, and each sailor finally ended up with 39990.
With 8 sailors, there were originally 117440505 coconuts, and each sailor finally ended up with 5044200.
```



## Julia

```julia

function validnutsforsailors(sailors, finalpile)
    for i in sailors:-1:1
        if finalpile % sailors != 1
            return false
        end
        finalpile -= Int(floor(finalpile/sailors) + 1)
    end
    (finalpile != 0) && (finalpile % sailors == 0)
end

function runsim()
    println("Sailors     Starting Pile")
    for sailors in 2:9
        finalcount = 0
        while validnutsforsailors(sailors, finalcount) == false
            finalcount += 1
        end
        println("$sailors           $finalcount")
    end
end

runsim()

```

```txt

Sailors     Starting Pile
2           11
3           25
4           765
5           3121
6           233275
7           823537
8           117440505
9           387420481

```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    var coconuts = 11
    outer@ for (ns in 2..9) {
        val hidden = IntArray(ns)
        coconuts = (coconuts / ns) * ns + 1
        while (true) {
            var nc = coconuts
            for (s in 1..ns) {
                if (nc % ns == 1) {
                    hidden[s - 1] = nc / ns
                    nc -= hidden[s - 1] + 1
                    if (s == ns && nc % ns == 0) {
                        println("$ns sailors require a minimum of $coconuts coconuts")
                        for (t in 1..ns) println("\tSailor $t hides ${hidden[t - 1]}")
                        println("\tThe monkey gets $ns")
                        println("\tFinally, each sailor takes ${nc / ns}\n")
                        continue@outer
                    }
                }
                else break
            }
            coconuts += ns
        }
    }
}
```


```txt

2 sailors require a minimum of 11 coconuts
	Sailor 1 hides 5
	Sailor 2 hides 2
	The monkey gets 2
	Finally, each sailor takes 1

3 sailors require a minimum of 25 coconuts
	Sailor 1 hides 8
	Sailor 2 hides 5
	Sailor 3 hides 3
	The monkey gets 3
	Finally, each sailor takes 2

4 sailors require a minimum of 765 coconuts
	Sailor 1 hides 191
	Sailor 2 hides 143
	Sailor 3 hides 107
	Sailor 4 hides 80
	The monkey gets 4
	Finally, each sailor takes 60

5 sailors require a minimum of 3121 coconuts
	Sailor 1 hides 624
	Sailor 2 hides 499
	Sailor 3 hides 399
	Sailor 4 hides 319
	Sailor 5 hides 255
	The monkey gets 5
	Finally, each sailor takes 204

6 sailors require a minimum of 233275 coconuts
	Sailor 1 hides 38879
	Sailor 2 hides 32399
	Sailor 3 hides 26999
	Sailor 4 hides 22499
	Sailor 5 hides 18749
	Sailor 6 hides 15624
	The monkey gets 6
	Finally, each sailor takes 13020

7 sailors require a minimum of 823537 coconuts
	Sailor 1 hides 117648
	Sailor 2 hides 100841
	Sailor 3 hides 86435
	Sailor 4 hides 74087
	Sailor 5 hides 63503
	Sailor 6 hides 54431
	Sailor 7 hides 46655
	The monkey gets 7
	Finally, each sailor takes 39990

8 sailors require a minimum of 117440505 coconuts
	Sailor 1 hides 14680063
	Sailor 2 hides 12845055
	Sailor 3 hides 11239423
	Sailor 4 hides 9834495
	Sailor 5 hides 8605183
	Sailor 6 hides 7529535
	Sailor 7 hides 6588343
	Sailor 8 hides 5764800
	The monkey gets 8
	Finally, each sailor takes 5044200

9 sailors require a minimum of 387420481 coconuts
	Sailor 1 hides 43046720
	Sailor 2 hides 38263751
	Sailor 3 hides 34012223
	Sailor 4 hides 30233087
	Sailor 5 hides 26873855
	Sailor 6 hides 23887871
	Sailor 7 hides 21233663
	Sailor 8 hides 18874367
	Sailor 9 hides 16777215
	The monkey gets 9
	Finally, each sailor takes 14913080

```



## Lua

```lua
function valid(n,nuts)
    local k = n
    local i = 0
    while k ~= 0 do
        if (nuts % n) ~= 1 then
            return false
        end
        k = k - 1
        nuts = nuts - 1 - math.floor(nuts / n)
    end
    return nuts ~= 0 and (nuts % n == 0)
end

for n=2, 9 do
    local x = 0
    while not valid(n, x) do
        x = x + 1
    end
    print(n..": "..x)
end
```

```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481
```


=={{header|Modula-2}}==

```modula2
MODULE Coconuts;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

CONST MAX_SAILORS = 9;

PROCEDURE Scenario(coconuts,ns : INTEGER);
VAR
    buf : ARRAY[0..63] OF CHAR;
    hidden : ARRAY[0..MAX_SAILORS-1] OF INTEGER;
    nc,s,t : INTEGER;
BEGIN
    IF ns>MAX_SAILORS THEN RETURN END;

    coconuts := (coconuts DIV ns) * ns + 1;
    LOOP
        nc := coconuts;
        FOR s:=1 TO ns DO
            IF nc MOD ns = 1 THEN
                hidden[s-1] := nc DIV ns;
                nc := nc - hidden[s-1] - 1;
                IF (s=ns) AND (nc MOD ns = 0) THEN
                    FormatString("%i sailors require a minimum of %i coconuts\n", buf, ns, coconuts);
                    WriteString(buf);

                    FOR t:=1 TO ns DO
                        FormatString("\tSailor %i hides %i\n", buf, t, hidden[t-1]);
                        WriteString(buf)
                    END;

                    FormatString("\tThe monkey gets %i\n", buf, ns);
                    WriteString(buf);
                    FormatString("\tFinally, each sailor takes %i\n", buf, nc DIV ns);
                    WriteString(buf);
                    RETURN
                END
            ELSE
                BREAK
            END
        END;
        INC(coconuts,ns)
    END
END Scenario;

VAR
    ns : INTEGER;
BEGIN
    FOR ns:=2 TO MAX_SAILORS DO
        Scenario(11,ns);
    END;

    ReadChar
END Coconuts.
```



## Perl

Use of <code>bigint</code> required or program silently fails for number of sailors > 13
```perl
use bigint;

for $sailors (1..15) { check( $sailors, coconuts( 0+$sailors ) ) }

sub is_valid {
    my($sailors, $nuts) = @_;
    return 0, 0 if $sailors == 1 and $nuts == 1;
    my @shares;
    for (1..$sailors) {
        return () unless ($nuts % $sailors) == 1;
        push @shares, int ($nuts-1)/$sailors;
        $nuts -= (1 + int $nuts/$sailors);
    }
    push @shares, int $nuts/$sailors;
    return @shares if !($nuts % $sailors);
}

sub check {
    my($sailors, $coconuts) = @_;
    my @suffix = ('th', 'st', 'nd', 'rd', ('th') x 6, ('th') x 10);
    my @piles = is_valid($sailors, $coconuts);
    if (@piles) {
        print "\nSailors $sailors: Coconuts $coconuts:\n";
        for my $k (0..-1 + $#piles) {
             print $k+1 . $suffix[$k+1] . " takes " . $piles[$k] . ", gives 1 to the monkey.\n"
        }
        print "The next morning, each sailor takes " . $piles[-1] . "\nwith none left over for the monkey.\n";
        return 1
    }
    return 0
}

sub coconuts {
    my($sailors) = @_;
    if ($sailors % 2 == 0 ) { ($sailors ** $sailors - 1) * ($sailors - 1) }
    else                    {  $sailors ** $sailors      -  $sailors + 1  }
}
```

<pre style="height:60ex;overflow:scroll;">
Sailors 1: Coconuts 1:
1st takes 0, gives 1 to the monkey.
The next morning, each sailor takes 0
with none left over for the monkey.

Sailors 2: Coconuts 3:
1st takes 1, gives 1 to the monkey.
2nd takes 0, gives 1 to the monkey.
The next morning, each sailor takes 0
with none left over for the monkey.

Sailors 3: Coconuts 25:
1st takes 8, gives 1 to the monkey.
2nd takes 5, gives 1 to the monkey.
3rd takes 3, gives 1 to the monkey.
The next morning, each sailor takes 2
with none left over for the monkey.

Sailors 4: Coconuts 765:
1st takes 191, gives 1 to the monkey.
2nd takes 143, gives 1 to the monkey.
3rd takes 107, gives 1 to the monkey.
4th takes 80, gives 1 to the monkey.
The next morning, each sailor takes 60
with none left over for the monkey.

Sailors 5: Coconuts 3121:
1st takes 624, gives 1 to the monkey.
2nd takes 499, gives 1 to the monkey.
3rd takes 399, gives 1 to the monkey.
4th takes 319, gives 1 to the monkey.
5th takes 255, gives 1 to the monkey.
The next morning, each sailor takes 204
with none left over for the monkey.

Sailors 6: Coconuts 233275:
1st takes 38879, gives 1 to the monkey.
2nd takes 32399, gives 1 to the monkey.
3rd takes 26999, gives 1 to the monkey.
4th takes 22499, gives 1 to the monkey.
5th takes 18749, gives 1 to the monkey.
6th takes 15624, gives 1 to the monkey.
The next morning, each sailor takes 13020
with none left over for the monkey.

Sailors 7: Coconuts 823537:
1st takes 117648, gives 1 to the monkey.
2nd takes 100841, gives 1 to the monkey.
3rd takes 86435, gives 1 to the monkey.
4th takes 74087, gives 1 to the monkey.
5th takes 63503, gives 1 to the monkey.
6th takes 54431, gives 1 to the monkey.
7th takes 46655, gives 1 to the monkey.
The next morning, each sailor takes 39990
with none left over for the monkey.

Sailors 8: Coconuts 117440505:
1st takes 14680063, gives 1 to the monkey.
2nd takes 12845055, gives 1 to the monkey.
3rd takes 11239423, gives 1 to the monkey.
4th takes 9834495, gives 1 to the monkey.
5th takes 8605183, gives 1 to the monkey.
6th takes 7529535, gives 1 to the monkey.
7th takes 6588343, gives 1 to the monkey.
8th takes 5764800, gives 1 to the monkey.
The next morning, each sailor takes 5044200
with none left over for the monkey.

Sailors 9: Coconuts 387420481:
1st takes 43046720, gives 1 to the monkey.
2nd takes 38263751, gives 1 to the monkey.
3rd takes 34012223, gives 1 to the monkey.
4th takes 30233087, gives 1 to the monkey.
5th takes 26873855, gives 1 to the monkey.
6th takes 23887871, gives 1 to the monkey.
7th takes 21233663, gives 1 to the monkey.
8th takes 18874367, gives 1 to the monkey.
9th takes 16777215, gives 1 to the monkey.
The next morning, each sailor takes 14913080
with none left over for the monkey.

Sailors 10: Coconuts 89999999991:
1st takes 8999999999, gives 1 to the monkey.
2nd takes 8099999999, gives 1 to the monkey.
3rd takes 7289999999, gives 1 to the monkey.
4th takes 6560999999, gives 1 to the monkey.
5th takes 5904899999, gives 1 to the monkey.
6th takes 5314409999, gives 1 to the monkey.
7th takes 4782968999, gives 1 to the monkey.
8th takes 4304672099, gives 1 to the monkey.
9th takes 3874204889, gives 1 to the monkey.
10th takes 3486784400, gives 1 to the monkey.
The next morning, each sailor takes 3138105960
with none left over for the monkey.

Sailors 11: Coconuts 285311670601:
1st takes 25937424600, gives 1 to the monkey.
2nd takes 23579476909, gives 1 to the monkey.
3rd takes 21435888099, gives 1 to the monkey.
4th takes 19487170999, gives 1 to the monkey.
5th takes 17715609999, gives 1 to the monkey.
6th takes 16105099999, gives 1 to the monkey.
7th takes 14640999999, gives 1 to the monkey.
8th takes 13309999999, gives 1 to the monkey.
9th takes 12099999999, gives 1 to the monkey.
10th takes 10999999999, gives 1 to the monkey.
11th takes 9999999999, gives 1 to the monkey.
The next morning, each sailor takes 9090909090
with none left over for the monkey.

Sailors 12: Coconuts 98077104930805:
1st takes 8173092077567, gives 1 to the monkey.
2nd takes 7492001071103, gives 1 to the monkey.
3rd takes 6867667648511, gives 1 to the monkey.
4th takes 6295362011135, gives 1 to the monkey.
5th takes 5770748510207, gives 1 to the monkey.
6th takes 5289852801023, gives 1 to the monkey.
7th takes 4849031734271, gives 1 to the monkey.
8th takes 4444945756415, gives 1 to the monkey.
9th takes 4074533610047, gives 1 to the monkey.
10th takes 3734989142543, gives 1 to the monkey.
11th takes 3423740047331, gives 1 to the monkey.
12th takes 3138428376720, gives 1 to the monkey.
The next morning, each sailor takes 2876892678660
with none left over for the monkey.

Sailors 13: Coconuts 302875106592241:
1st takes 23298085122480, gives 1 to the monkey.
2nd takes 21505924728443, gives 1 to the monkey.
3rd takes 19851622826255, gives 1 to the monkey.
4th takes 18324574916543, gives 1 to the monkey.
5th takes 16914992230655, gives 1 to the monkey.
6th takes 15613838982143, gives 1 to the monkey.
7th takes 14412774445055, gives 1 to the monkey.
8th takes 13304099487743, gives 1 to the monkey.
9th takes 12280707219455, gives 1 to the monkey.
10th takes 11336037433343, gives 1 to the monkey.
11th takes 10464034553855, gives 1 to the monkey.
12th takes 9659108818943, gives 1 to the monkey.
13th takes 8916100448255, gives 1 to the monkey.
The next morning, each sailor takes 8230246567620
with none left over for the monkey.

Sailors 14: Coconuts 144456088732254195:
1st takes 10318292052303871, gives 1 to the monkey.
2nd takes 9581271191425023, gives 1 to the monkey.
3rd takes 8896894677751807, gives 1 to the monkey.
4th takes 8261402200769535, gives 1 to the monkey.
5th takes 7671302043571711, gives 1 to the monkey.
6th takes 7123351897602303, gives 1 to the monkey.
7th takes 6614541047773567, gives 1 to the monkey.
8th takes 6142073830075455, gives 1 to the monkey.
9th takes 5703354270784351, gives 1 to the monkey.
10th takes 5295971822871183, gives 1 to the monkey.
11th takes 4917688121237527, gives 1 to the monkey.
12th takes 4566424684006275, gives 1 to the monkey.
13th takes 4240251492291541, gives 1 to the monkey.
14th takes 3937376385699288, gives 1 to the monkey.
The next morning, each sailor takes 3656135215292196
with none left over for the monkey.

Sailors 15: Coconuts 437893890380859361:
1st takes 29192926025390624, gives 1 to the monkey.
2nd takes 27246730957031249, gives 1 to the monkey.
3rd takes 25430282226562499, gives 1 to the monkey.
4th takes 23734930078124999, gives 1 to the monkey.
5th takes 22152601406249999, gives 1 to the monkey.
6th takes 20675761312499999, gives 1 to the monkey.
7th takes 19297377224999999, gives 1 to the monkey.
8th takes 18010885409999999, gives 1 to the monkey.
9th takes 16810159715999999, gives 1 to the monkey.
10th takes 15689482401599999, gives 1 to the monkey.
11th takes 14643516908159999, gives 1 to the monkey.
12th takes 13667282447615999, gives 1 to the monkey.
13th takes 12756130284441599, gives 1 to the monkey.
14th takes 11905721598812159, gives 1 to the monkey.
15th takes 11112006825558015, gives 1 to the monkey.
The next morning, each sailor takes 10371206370520814
with none left over for the monkey.
```



## Perl 6

There is nowhere in the spec where it explicitly states that the sailors cannot equally share zero coconuts in the morning. Actually, The On-Line Encyclopedia of Integer Sequences [http://oeis.org/A002021 A002021] considers the cases for 1 and 2 sailors equally sharing zero coconuts in the morning to be the correct answer.

This will test combinations of sailors and coconuts to see if they form a valid pairing. The first 6 are done using brute force, testing every combination until a valid one is found. For cases of 7 to 15 sailors, it uses a carefully crafted filter to drastically reduce the number of trials needed to find a valid case (to one, as it happens... :-) )


```perl6
my @ones = flat 'th', 'st', 'nd', 'rd', 'th' xx 6;
my @teens = 'th' xx 10;
my @suffix = lazy flat (@ones, @teens, @ones xx 8) xx *;

# brute force the first six
for 1 .. 6 -> $sailors { for $sailors .. * -> $coconuts { last if check( $sailors, $coconuts ) } }

# finesse 7 through 15
for 7 .. 15 -> $sailors { next if check( $sailors, coconuts( $sailors ) ) }

sub is_valid ( $sailors is copy, $nuts is copy ) {
    return 0, 0 if $sailors == $nuts == 1;
    my @shares;
    for ^$sailors {
        return () unless $nuts % $sailors == 1;
        push @shares, ($nuts - 1) div $sailors;
        $nuts -= (1 + $nuts div $sailors);
    }
    push @shares, $nuts div $sailors;
    return @shares if !?($nuts % $sailors);
}

sub check ($sailors, $coconuts) {
    if my @piles = is_valid($sailors, $coconuts) {
        say "\nSailors $sailors: Coconuts $coconuts:";
        for ^(@piles - 1) -> $k {
             say "{$k+1}@suffix[$k+1] takes @piles[$k], gives 1 to the monkey."
        }
        say "The next morning, each sailor takes @piles[*-1]\nwith none left over for the monkey.";
        return True;
    }
    False;
}

multi sub coconuts ( $sailors where { $sailors % 2 == 0 } ) { ($sailors - 1) * ($sailors ** $sailors - 1) }
multi sub coconuts ( $sailors where { $sailors % 2 == 1 } ) { $sailors ** $sailors - $sailors + 1 }
```

```txt

Sailors 1: Coconuts 1:
1st takes 0, gives 1 to the monkey.
The next morning, each sailor takes 0
with none left over for the monkey.

Sailors 2: Coconuts 3:
...

Sailors 5: Coconuts 3121:
1st takes 624, gives 1 to the monkey.
2nd takes 499, gives 1 to the monkey.
3rd takes 399, gives 1 to the monkey.
4th takes 319, gives 1 to the monkey.
5th takes 255, gives 1 to the monkey.
The next morning, each sailor takes 204
with none left over for the monkey.

Sailors 6: Coconuts 233275:
1st takes 38879, gives 1 to the monkey.
2nd takes 32399, gives 1 to the monkey.
3rd takes 26999, gives 1 to the monkey.
4th takes 22499, gives 1 to the monkey.
5th takes 18749, gives 1 to the monkey.
6th takes 15624, gives 1 to the monkey.
The next morning, each sailor takes 13020
with none left over for the monkey.

Sailors 7: Coconuts 823537:
...

Sailors 15: Coconuts 437893890380859361:
...

```



## Phix

The morning pile must be a multiple of sailors, so this only tries multiples of sailors! Needed an ugly kludge for solve(1),
the limit of 1 billion suffices for solve(9), above that gets run-time type check errors as capacity of ints are blown anyway.

```Phix
procedure solve(integer sailors)
integer m, sm1 = sailors-1
    if sm1=0 then   -- edge condition for solve(1) [ avoid /0 ]
        m = sailors
    else
        for n=sailors to 1_000_000_000 by sailors do    -- morning pile divisible by #sailors
            m = n
            for j=1 to sailors do               -- see if all of the sailors could..
                if remainder(m,sm1)!=0 then     -- ..have pushed together sm1 piles
                    m = 0                       --  (no: try a higher n)
                    exit
                end if
                m = sailors*m/sm1+1     -- add sailor j's stash and one for the monkey
            end for
            if m!=0 then exit end if
        end for
    end if
    printf(1,"Solution with %d sailors: %d\n",{sailors,m})
    for i=1 to sailors do
        m -= 1                                  -- one for the monkey
        m /= sailors
        printf(1,"Sailor #%d takes %d, giving 1 to the monkey and leaving %d\n",{i,m,m*sm1})
        m *= (sm1)
    end for
    printf(1,"In the morning each sailor gets %d nuts\n",m/sailors)
end procedure

solve(5)
solve(6)
```

```txt

Solution with 5 sailors: 3121
Sailor #1 takes 624, giving 1 to the monkey and leaving 2496
Sailor #2 takes 499, giving 1 to the monkey and leaving 1996
Sailor #3 takes 399, giving 1 to the monkey and leaving 1596
Sailor #4 takes 319, giving 1 to the monkey and leaving 1276
Sailor #5 takes 255, giving 1 to the monkey and leaving 1020
In the morning each sailor gets 204 nuts
Solution with 6 sailors: 233275
Sailor #1 takes 38879, giving 1 to the monkey and leaving 194395
Sailor #2 takes 32399, giving 1 to the monkey and leaving 161995
Sailor #3 takes 26999, giving 1 to the monkey and leaving 134995
Sailor #4 takes 22499, giving 1 to the monkey and leaving 112495
Sailor #5 takes 18749, giving 1 to the monkey and leaving 93745
Sailor #6 takes 15624, giving 1 to the monkey and leaving 78120
In the morning each sailor gets 13020 nuts

```



## Python

You may want to read [http://paddy3118.blogspot.co.uk/2015/05/solving-monkey-and-coconuts-problem.html Solving the Monkey and coconuts problem] to get more background on the evolution of the Python code.

### Python: Procedural


```python
def monkey_coconuts(sailors=5):
    "Parameterised the number of sailors using an inner loop including the last mornings case"
    nuts = sailors
    while True:
        n0, wakes = nuts, []
        for sailor in range(sailors + 1):
            portion, remainder = divmod(n0, sailors)
            wakes.append((n0, portion, remainder))
            if portion <= 0 or remainder != (1 if sailor != sailors else 0):
                nuts += 1
                break
            n0 = n0 - portion - remainder
        else:
            break
    return nuts, wakes

if __name__ == "__main__":
    for sailors in [5, 6]:
        nuts, wake_stats = monkey_coconuts(sailors)
        print("\nFor %i sailors the initial nut count is %i" % (sailors, nuts))
        print("On each waking, the nut count, portion taken, and monkeys share are:\n ",
              ',\n  '.join(repr(ws) for ws in wake_stats))
```


```txt
For 5 sailors the initial nut count is 3121
On each waking, the nut count, portion taken, and monkeys share are:
  (3121, 624, 1),
  (2496, 499, 1),
  (1996, 399, 1),
  (1596, 319, 1),
  (1276, 255, 1),
  (1020, 204, 0)

For 6 sailors the initial nut count is 233275
On each waking, the nut count, portion taken, and monkeys share are:
  (233275, 38879, 1),
  (194395, 32399, 1),
  (161995, 26999, 1),
  (134995, 22499, 1),
  (112495, 18749, 1),
  (93745, 15624, 1),
  (78120, 13020, 0)
```



### Python: Recursive



```python
def wake_and_split(n0, sailors, depth=None):
    if depth is None:
        depth = sailors
    portion, remainder = divmod(n0, sailors)
    if portion <= 0 or remainder != (1 if depth else 0):
        return None
    else:
        return n0 if not depth else wake_and_split(n0 - portion - remainder, sailors, depth - 1)


def monkey_coconuts(sailors=5):
    "Parameterised the number of sailors using recursion including the last mornings case"
    nuts = sailors
    while True:
        if wake_and_split(n0=nuts, sailors=sailors) is None:
            nuts += 1
        else:
            break
    return nuts

if __name__ == "__main__":
    for sailors in [5, 6]:
        nuts = monkey_coconuts(sailors)
        print("For %i sailors the initial nut count is %i" % (sailors, nuts))

```


```txt
For 5 sailors the initial nut count is 3121
For 6 sailors the initial nut count is 233275
```



### by solving Diophantine equation

The following is a more or less general solution for arbitrary number of sailors and varying numbers of coconuts the monkey gets.  The monkey can be given more coconuts than there are sailors each turn.  This is not part of task requirement.

```python
# gives one solution of (x,y) for a x + by = c
def dioph(a, b, c):
	aa,bb,x,y = a, b, 0, 1

	while True:
		q,a,b = a//b, b, a%b
		x,y = y - q*x, x
		if abs(a) == 1: break

	if y*aa % bb != 1: y = -y
	x,y = y*c, (c - aa*y*c)//bb
	#assert(x*aa + y*bb == c)
	return x,y

# rems: what monkey got each turn
# min_share: each sailor needs to get at least this many in the final round
def calcnuts(rems, min_share = 0):
	n, r = len(rems) - 1, 0
	c = (n - 1)**n
	for x in rems: r,c = r + x*c, c//(n-1)*n

	a, b = (n-1)**n, n**(n+1)
	x, y = dioph(a, -b, r)
	k = (min_share - y + a - 1)//a
	return x + k*b, y + k*a

def distribute(nuts, monkey_nuts):
	n = len(monkey_nuts) - 1
	print("\n%d sailors, %d nuts:"%(n, nuts))
	for r in monkey_nuts[:-1]:
		p = (nuts - r)//n
		print("\tNuts %d, hide %d, monkey gets %d" % (nuts, p, r))
		nuts = p*(n - 1)

	r = monkey_nuts[-1]
	p = (nuts - r)//n
	print("Finally:\n\tNuts %d, each share %d, monkey gets %d" % (nuts, p, r))

for sailors in range(2, 10):
	monkey_loot = [1]*sailors + [0]
	distribute(calcnuts(monkey_loot, 1)[0], monkey_loot)

# many sailors, many nuts
#for i in range(1, 5): print(10**i, calcnuts([1]*10**i + [0])[0])
```



## Racket

```Racket
#lang racket

(define (wake-and-split nuts sailors depth wakes)
  (define-values (portion remainder) (quotient/remainder nuts sailors))
  (define monkey (if (zero? depth) 0 1))
  (define new-wakes (cons (list nuts portion remainder) wakes))
  (and (positive? portion)
       (= remainder monkey)
       (if (zero? depth)
           new-wakes
           (wake-and-split (- nuts portion remainder) sailors (sub1 depth) new-wakes))))

(define (sleep-and-split nuts sailors)
  (wake-and-split nuts sailors sailors '()))

(define (monkey_coconuts (sailors 5))
    (let loop ([nuts sailors])
      (or (sleep-and-split nuts sailors)
          (loop (add1 nuts)))))

(for ([sailors (in-range 5 7)])
  (define wakes (monkey_coconuts sailors))
  (printf "For ~a sailors the initial nut count is ~a\n" sailors (first (last wakes)))
  (map displayln (reverse wakes))
  (newline))
```

```txt
For 5 sailors the initial nut count is 3121
(3121 624 1)
(2496 499 1)
(1996 399 1)
(1596 319 1)
(1276 255 1)
(1020 204 0)

For 6 sailors the initial nut count is 233275
(233275 38879 1)
(194395 32399 1)
(161995 26999 1)
(134995 22499 1)
(112495 18749 1)
(93745 15624 1)
(78120 13020 0)
```



## REXX


### uses a subroutine

{{trans|C}}  {from the 1<sup>st</sup> '''C''' example}


```rexx
/*REXX program  solves  a  riddle  of 5 sailors, a pile of coconuts, and a monkey.      */
parse arg L H .;        if L==''  then L=5       /*L  not specified?   Then use default.*/
                        if H==''  then H=6       /*H   "      "          "   "  default.*/
                                                 /*{Tars is an old name for sailors.}   */
     do n=L  to H                                /*traipse through a number of sailors. */
       do $=0  while \valid(n,$);  end           /*perform while not valid coconuts.    */
     say 'sailors='n    "  coconuts="$           /*display number of sailors & coconuts.*/
     end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
valid: procedure;  parse arg n,nuts              /*obtain the number sailors & coconuts.*/
                 do k=n  by -1  for n            /*step through the possibilities.      */
                 if nuts//n\==1  then return 0   /*Not one coconut left?   No solution. */
                 nuts=nuts - (1+nuts%n)          /*subtract number of coconuts from pile*/
                 end   /*k*/
       return (nuts\==0) & \(nuts//n\==0)        /*see if number coconuts>0 & remainder.*/
```

Programming note:   The parentheses in the last REXX ('''return''') statement aren't necessary, but help for readability.


'''output'''   when using the default inputs:

```txt

sailors=5   coconuts=3121
sailors=6   coconuts=233275

```


===uses in-line code===
This REXX version is the same as the above version (but the defaults are different),

and it also eliminates the use of a subroutine, making it faster.

```rexx
/*REXX program  solves  a  riddle of 5 sailors, a pile of coconuts, and a monkey.       */

  do n=2  to 9                                   /*traipse through number of sailors.   */
    do $=0;                   nuts=$             /*perform while not valid # coconuts.  */
      do k=n  by -1  for n                       /*step through the possibilities.      */
      if nuts//n\==1  then iterate $             /*Not one coconut left?    No solution.*/
      nuts=nuts - (1+nuts%n)                     /*subtract number of coconuts from pile*/
      end   /*k*/
    if (nuts\==0) & \(nuts//n\==0)  then leave   /*is this a solution to the riddle ?   */
    end     /*$*/
  say 'sailors='n    "  coconuts="$              /*display number of sailors & coconuts.*/
  end       /*n*/                                /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default inputs:

```txt

sailors=2   coconuts=11
sailors=3   coconuts=25
sailors=4   coconuts=765
sailors=5   coconuts=3121
sailors=6   coconuts=233275
sailors=7   coconuts=823537
sailors=8   coconuts=117440505
sailors=9   coconuts=387420481

```



### shows the shares

{{trans|C}}  {from the 2<sup>nd</sup> '''C''' example}


```rexx
/*REXX program  solves a  riddle  of 5 sailors, a pile of coconuts, and a monkey.       */
parse arg L H .;       if L==''  then L=2        /*L  not specified?   Then use default.*/
                       if H==''  then H=9        /*H   "      "          "   "     "    */
                                                 /*{Tars is an old name for sailors.}   */
     do n=L  to H                                /*traipse through the number of sailors*/
         do $=1  until t\==0                     /*perform while number coconuts not 0. */
         t=total(n,$)                            /*perform while not valid coconuts.    */
         end   /*$*/
     say 'sailors='n       "  coconuts="t        '  share='$
     end       /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
total: procedure;  parse arg n,nuts              /*obtain the number sailors & coconuts.*/
       nuts=nuts*n
       nn=n-1                                    /*NN  is used as calculation shortcut. */
                  do k=0  for n                  /*step through the possibilities.      */
                  if nuts//nn\==0  then return 0 /*Not one coconut left?   No solution. */
                  nuts=nuts + nuts%nn + 1        /*bump the number coconuts to the pile.*/
                  end   /*k*/
       return nuts                               /*see if number coconuts>0 & remainder.*/
```

'''output'''   when using the default inputs:

```txt

sailors=2   coconuts=11   share=1
sailors=3   coconuts=25   share=2
sailors=4   coconuts=765   share=60
sailors=5   coconuts=3121   share=204
sailors=6   coconuts=233275   share=13020
sailors=7   coconuts=823537   share=39990
sailors=8   coconuts=117440505   share=5044200
sailors=9   coconuts=387420481   share=14913080

```



## Ring


```ring

# Project : Sailors, coconuts and a monkey problem

scm(5)
scm(6)

func scm(sailors)
        sm1 = sailors-1
        if sm1 = 0
           m = sailors
        else
           for n=sailors to 1000000000 step sailors
                m = n
                for j=1 to sailors
                     if m % sm1 != 0
                        m = 0
                        exit
                     ok
                     m = sailors*m/sm1+1
                next
                if m != 0
                   exit
                ok
           next
        ok
        see "Solution with " + sailors + " sailors: " + m + nl
        for i=1 to sailors
             m = m - 1
             m = m / sailors
             see "Sailor " + i + " takes " + m + " giving 1 to the monkey and leaving " + m*sm1 + nl
             m = m * sm1
        next
        see "In the morning each sailor gets " + m/sailors + " nuts" + nl + nl

```

Output:

```txt

Solution with 5 sailors: 3121
Sailor 1 takes 624 giving 1 to the monkey and leaving 2496
Sailor 2 takes 499 giving 1 to the monkey and leaving 1996
Sailor 3 takes 399 giving 1 to the monkey and leaving 1596
Sailor 4 takes 319 giving 1 to the monkey and leaving 1276
Sailor 5 takes 255 giving 1 to the monkey and leaving 1020
In the morning each sailor gets 204 nuts

Solution with 6 sailors: 233275
Sailor 1 takes 38879 giving 1 to the monkey and leaving 194395
Sailor 2 takes 32399 giving 1 to the monkey and leaving 161995
Sailor 3 takes 26999 giving 1 to the monkey and leaving 134995
Sailor 4 takes 22499 giving 1 to the monkey and leaving 112495
Sailor 5 takes 18749 giving 1 to the monkey and leaving 93745
Sailor 6 takes 15624 giving 1 to the monkey and leaving 78120
In the morning each sailor gets 13020 nuts

```



## Ruby


### Brute Force


```ruby
def valid?(sailor, nuts)
  sailor.times do
    return false if (nuts % sailor) != 1
    nuts -= 1 + nuts / sailor
  end
  nuts > 0 and nuts % sailor == 0
end

[5,6].each do |sailor|
  n = sailor
  n += 1 until valid?(sailor, n)
  puts "\n#{sailor} sailors => #{n} coconuts"
  (sailor+1).times do
    div, mod = n.divmod(sailor)
    puts "  #{[n, div, mod]}"
    n -= 1 + div
  end
end
```

```txt
5 sailors => 3121 coconuts
  [3121, 624, 1]
  [2496, 499, 1]
  [1996, 399, 1]
  [1596, 319, 1]
  [1276, 255, 1]
  [1020, 204, 0]

6 sailors => 233275 coconuts
  [233275, 38879, 1]
  [194395, 32399, 1]
  [161995, 26999, 1]
  [134995, 22499, 1]
  [112495, 18749, 1]
  [93745, 15624, 1]
  [78120, 13020, 0]
```


### Faster version

```ruby
def coconuts(sailor)
  sailor.step(by:sailor) do |nuts|
    flag = sailor.times do
      break if nuts % (sailor-1) != 0
      nuts += nuts / (sailor-1) + 1
    end
    return nuts if flag
  end
end

(2..9).each do |sailor|
  puts "#{sailor}: #{coconuts(sailor)}"
end
```

```txt
2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481
```

===A function to find the solution see [[User_talk:Nigel_Galloway#Inflammatory_stuff]] for a description===

```ruby
def ng (sailors)
  def _ng (sailors, iter, start) #a method that given a possible answer applies the constraints of the tale to see if it is correct
    n, g = [start], [start/sailors]
    (1..iter).each{|s|
      g[s],rem = n[s-1].divmod(sailors-1)
      rem > 0 ? (return false) : n[s] = g[s]*sailors + 1
    }
    return [n,g]
  end
  n, start, step = [], sailors*(sailors-1), 1
  (2..sailors).each{|s|
    g=0; until n=_ng(sailors,s,start + g*step*sailors*(sailors-1)) do g+=1 end
    start,step = n[0][0], step*(sailors-1)
  }
  return n
end


```


### A possible use of the function


```ruby
(3..10).each{|sailors| puts "Number of sailors = #{sailors}"; p ng(sailors)}
```

The output consists of two list. The first is the number of nuts in each pile, the second the number of nuts taken by each dishonest sailor. So in the case of three, start with 25 nuts, the first sailor takes 8 and discards 1 leaving a pile of 16. The second sailor takes 5 and discards 1 leaving 10. The last dishonest sailor takes 3 discards 1 leaving 6 nuts, which can be shared equally between the 3 (2 each).

```txt
Number of sailors = 3
[[6, 10, 16, 25], [2, 3, 5, 8]]
Number of sailors = 4
[[240, 321, 429, 573, 765], [60, 80, 107, 143, 191]]
Number of sailors = 5
[[1020, 1276, 1596, 1996, 2496, 3121], [204, 255, 319, 399, 499, 624]]
Number of sailors = 6
[[78120, 93745, 112495, 134995, 161995, 194395, 233275], [13020, 15624, 18749, 22499, 26999, 32399, 38879]]
Number of sailors = 7
[[279930, 326586, 381018, 444522, 518610, 605046, 705888, 823537], [39990, 46655, 54431, 63503, 74087, 86435, 100841, 117648]]
Number of sailors = 8
[[40353600, 46118401, 52706745, 60236281, 68841465, 78675961, 89915385, 102760441, 117440505], [5044200, 5764800, 6588343, 7529535, 8605183, 9834495, 11239423, 12845055, 14680063]]
Number of sailors = 9
[[134217720, 150994936, 169869304, 191102968, 214990840, 241864696, 272097784, 306110008, 344373760, 387420481], [14913080, 16777215, 18874367, 21233663, 23887871, 26873855, 30233087, 34012223, 38263751, 43046720]]
Number of sailors = 10
[[31381059600, 34867844001, 38742048891, 43046720991, 47829689991, 53144099991, 59048999991, 65609999991, 72899999991, 80999999991, 89999999991], [3138105960, 3486784400, 3874204889, 4304672099, 4782968999, 5314409999, 5904899999, 6560999999, 7289999999, 8099999999, 8999999999]]
```

Did someone ask the value for 100 sailors?
```ruby

n = ng(100)
(0..100).each{|g| puts "#{n[0][100-g]}:#{n[1][100-g]}"}
```

The number of coconuts requires is as follows, the whole output is at [[Sailors, coconuts and a monkey problem/Ruby output 100]]

```txt
9899999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999901
```


## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/jBSqGXg/0 ScalaFiddle (ES aka JavaScript, non JVM, be patient)] or [https://scastie.scala-lang.org/lZXMc4YBSl2htEBw4D2TUQ Scastie (remote JVM)].

```Scala
object Sailors extends App {
  var x = 0

  private def valid(n: Int, _nuts: Int): Boolean = {
    var nuts = _nuts
    for (k <- n until 0 by -1) {
      if (nuts % n != 1) return false
      nuts -= 1 + nuts / n
    }
    nuts != 0 && (nuts % n == 0)
  }

  for (nSailors <- 2 until 10) {
    while (!valid(nSailors, x)) x += 1
    println(f"$nSailors%d: $x%d")
  }

}
```



## Sidef

```ruby
func coconuts(sailors, monkeys=1) {
    if ((sailors < 2) || (monkeys < 1) || (sailors <= monkeys)) {
        return 0
    }
    var blue_cocos = sailors-1
    var pow_bc = blue_cocos**sailors
    var x_cocos = pow_bc
    while ((x_cocos-blue_cocos)%sailors || ((x_cocos-blue_cocos)/sailors < 1)) {
        x_cocos += pow_bc
    }
    return monkeys*(x_cocos / pow_bc * sailors**sailors - blue_cocos)
}

2.to(9).each { |sailor|
    say "#{sailor}: #{coconuts(sailor)}";
}
```

```txt

2: 11
3: 25
4: 765
5: 3121
6: 233275
7: 823537
8: 117440505
9: 387420481

```



## Tcl


This is a very straightforward implementation.  The "overnight" proc attempts to fulfill the activities of the night, throwing an error (through "assert") if it cannot.  "minnuts" keeps trying to call it with more nuts until it succeeds.  On success, "overnight" will return a list which narrates the night's activities.


```Tcl
proc assert {expr {msg ""}} {    ;# for "static" assertions that throw nice errors
    if {![uplevel 1 [list expr $expr]]} {
        if {$msg eq ""} {
            catch {set msg "{[uplevel 1 [list subst -noc $expr]]}"}
        }
        throw {ASSERT ERROR} "{$expr} $msg"
    }
}

proc divmod {a b} {
    list [expr {$a / $b}] [expr {$a % $b}]
}

proc overnight {ns nn} {
    set result {}
    for {set s 0} {$s < $ns} {incr s} {
        lassign [divmod $nn $ns] q r
        assert {$r eq 1} "Incorrect remainder in round $s (expected 1, got $r)"
        set nn [expr {$q*($ns-1)}]
        lappend result $s $q $r $nn
    }
    lassign [divmod $nn $ns] q r
    assert {$r eq 0} "Incorrect remainder at end (expected 0, got $r)"
    return $result
}

proc minnuts {nsailors} {
    while 1 {
        incr nnuts
        try {
            set result [overnight $nsailors $nnuts]
        } on error {} {
            # continue
        } on ok {} {
            break
        }
    }
    puts "$nsailors: $nnuts"
    foreach {sailor takes gives leaves} $result {
        puts " Sailor #$sailor takes $takes, giving $gives to the monkey and leaves $leaves"
    }
    puts "In the morning, each sailor gets [expr {$leaves/$nsailors}] nuts"
}


foreach n {5 6} {
    puts "Solution with $n sailors:"
    minnuts $n
}
```


```txt

Solution with 5 sailors:
5: 3121
 Sailor #0 takes 624, giving 1 to the monkey and leaves 2496
 Sailor #1 takes 499, giving 1 to the monkey and leaves 1996
 Sailor #2 takes 399, giving 1 to the monkey and leaves 1596
 Sailor #3 takes 319, giving 1 to the monkey and leaves 1276
 Sailor #4 takes 255, giving 1 to the monkey and leaves 1020
In the morning, each sailor gets 204 nuts
Solution with 6 sailors:
6: 233275
 Sailor #0 takes 38879, giving 1 to the monkey and leaves 194395
 Sailor #1 takes 32399, giving 1 to the monkey and leaves 161995
 Sailor #2 takes 26999, giving 1 to the monkey and leaves 134995
 Sailor #3 takes 22499, giving 1 to the monkey and leaves 112495
 Sailor #4 takes 18749, giving 1 to the monkey and leaves 93745
 Sailor #5 takes 15624, giving 1 to the monkey and leaves 78120
In the morning, each sailor gets 13020 nuts

```



## uBasic/4tH

For performance reasons, we limit ourselves to seven sailors.
<lang>For n = 2 To 7
  t = 0
  For x = 1 Step 1 While t = 0
    t = FUNC(_Total(n,x))
  Next
  Print n;": ";t;Tab(12); x - 1
Next

End

_Total Param(2)
  Local(1)

  b@ = b@ * a@
  a@ = a@ - 1

  For c@ = 0 To a@

    If b@ % a@ Then
       b@ = 0
       Break
    EndIf

    b@ = b@ + 1 + b@ / a@
  Next
Return (b@)
```

```txt
2: 11       1
3: 25       2
4: 765      60
5: 3121     204
6: 233275   13020
7: 823537   39990

0 OK, 0:127
```



## VBA


```vb
Option Explicit
Public Sub coconuts()
    Dim sailors As Integer
    Dim share As Long
    Dim finalshare As Integer
    Dim minimum As Long, pile As Long
    Dim i As Long, j As Integer
    Debug.Print "Sailors", "Pile", "Final share"
    For sailors = 2 To 6
        i = 1
        Do While True
            pile = i
            For j = 1 To sailors
                If (pile - 1) Mod sailors <> 0 Then Exit For
                share = (pile - 1) / sailors
                pile = pile - share - 1
            Next j
            If j > sailors Then
                If share Mod sailors = 0 And share > 0 Then
                    minimum = i
                    finalshare = pile / sailors
                    Exit Do
                End If
            End If
            i = i + 1
        Loop
        Debug.Print sailors, minimum, finalshare
    Next sailors
End Sub
```
```txt
Sailors       Pile          Final share
 2             11            1
 3             25            2
 4             765           60
 5             3121          204
 6             233275        13020
```



## Yabasic

```Yabasic
coconuts = 11

for ns = 2 to 9
    dim hidden(ns)
    coconuts = int(coconuts / ns) * ns + 1
    do
        nc = coconuts
        for s = 1 to ns+1
            if mod(nc, ns) = 1 then
                hidden(s-1) = int(nc / ns)
                nc = nc - (hidden(s-1) + 1)
                if s = ns and not mod(nc, ns) then
                    print ns, " sailors require a minimum of ", coconuts, " coconuts"
                    for t = 1 to ns
                        print "\tSailor ", t, " hides ", hidden(t - 1)
                    next
                    print "\tThe monkey gets ", ns
                    print "\tFinally, each sailor takes ", int(nc / ns), "\n"
                    break 2
                end if
            else
                break
            end if
        next
        coconuts = coconuts + ns
    loop
next
```



## zkl

```zkl
fcn monkey_coconuts(sailors=5){
   nuts,wakes:=sailors,List();
   while(True){
      n0:=nuts; wakes.clear();
      foreach sailor in (sailors + 1){
         portion, remainder := n0.divr(sailors);
	 wakes.append(T(n0, portion, remainder));
	 if(portion <= 0 or remainder != (sailor != sailors).toInt()){
	    nuts += 1;
	    break;
	 }
	 n0 = n0 - portion - remainder;
      }
      fallthrough{ break }
   }

   return(nuts, wakes)
}
foreach sailors in ([5..6]){
   nuts, wake_stats := monkey_coconuts(sailors);
   println("For %d sailors the initial nut count is %,d".fmt(sailors, nuts));
   println("On each waking, the nut count, portion taken, and monkeys share are:\n   ",
      wake_stats.concat("\n   "));
}
```

```txt

For 5 sailors the initial nut count is 3,121
On each waking, the nut count, portion taken, and monkeys share are:
   L(3121,624,1)
   L(2496,499,1)
   L(1996,399,1)
   L(1596,319,1)
   L(1276,255,1)
   L(1020,204,0)
For 6 sailors the initial nut count is 233,275
On each waking, the nut count, portion taken, and monkeys share are:
   L(233275,38879,1)
   L(194395,32399,1)
   L(161995,26999,1)
   L(134995,22499,1)
   L(112495,18749,1)
   L(93745,15624,1)
   L(78120,13020,0)

```

```zkl
fcn total(n, nuts){
   nuts *= n;
   foreach k in (n){
      if (nuts % (n-1)) return(0);
      nuts += nuts / (n-1) + 1;
   }
   nuts;
}

println("sailers: original pile, final share");
foreach n,x in ([2..9],[1..]){
   if(t := total(n, x)){
      print("%d: %d\t%d\n".fmt(n, t, x));
      break;
   }
}
```

```txt

sailers: original pile, final share
2: 11	1
3: 25	2
4: 765	60
5: 3121	204
6: 233275	13020
7: 823537	39990
8: 117440505	5044200
9: 387420481	14913080

```

