+++
title = "Water collected between towers"
description = ""
date = 2019-09-15T10:59:00Z
aliases = []
[extra]
id = 21208
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
In a two-dimensional world, we begin with any bar-chart (or row of close-packed 'towers', each of unit width), and then it rains,
completely filling all convex enclosures in the chart with water.



```txt
9               ██           9               ██
8               ██           8               ██
7     ██        ██           7     ██≈≈≈≈≈≈≈≈██
6     ██  ██    ██           6     ██≈≈██≈≈≈≈██
5 ██  ██  ██  ████           5 ██≈≈██≈≈██≈≈████
4 ██  ██  ████████           4 ██≈≈██≈≈████████
3 ██████  ████████           3 ██████≈≈████████
2 ████████████████  ██       2 ████████████████≈≈██
1 ████████████████████       1 ████████████████████
```



In the example above, a bar chart representing the values [5, 3, 7, 2, 6, 4, 5, 9, 1, 2] has filled, collecting 14 units of water.

Write a function, in your language, from a given array of heights, to the number of water units that can be held in this way, by a corresponding bar chart.

Calculate the number of water units that could be collected by bar charts representing each of the following seven series:


```txt
   [[1, 5, 3, 7, 2],
    [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
    [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
    [5, 5, 5, 5],
    [5, 6, 7, 8],
    [8, 7, 7, 6],
    [6, 7, 10, 7, 6]]
```



See, also:

* [https://youtu.be/ftcIcn8AmSY?t=536 Four Solutions to a Trivial Problem] – a Google Tech Talk by Guy Steele
* [http://stackoverflow.com/questions/24414700/amazon-water-collected-between-towers/ Water collected between towers] on Stack Overflow, from which the example above is taken)
* [https://gist.github.com/paf31/9d84ecf6a6a9b69cdb597a390f25764d An interesting Haskell solution], using the Tardis monad, by [https://gist.github.com/paf31 Phil Freeman] in a [https://gist.github.com/paf31/9d84ecf6a6a9b69cdb597a390f25764d Github gist].






## AppleScript

{{Trans|JavaScript}}


```AppleScript
-- WATER COLLECTED BETWEEN TOWERS --------------------------------------------

-- waterCollected :: [Int] -> Int
on waterCollected(xs)
    set leftWalls to scanl1(my max, xs)
    set rightWalls to scanr1(my max, xs)

    set waterLevels to zipWith(my min, leftWalls, rightWalls)

    -- positive :: Num a => a -> Bool
    script positive
        on |λ|(x)
            x > 0
        end |λ|
    end script

    -- minus :: Num a => a -> a -> a
    script minus
        on |λ|(a, b)
            a - b
        end |λ|
    end script

    sum(filter(positive, zipWith(minus, waterLevels, xs)))
end waterCollected


-- TEST ----------------------------------------------------------------------
on run
    map(waterCollected, ¬
        [[1, 5, 3, 7, 2], ¬
            [5, 3, 7, 2, 6, 4, 5, 9, 1, 2], ¬
            [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1], ¬
            [5, 5, 5, 5], ¬
            [5, 6, 7, 8], ¬
            [8, 7, 7, 6], ¬
            [6, 7, 10, 7, 6]])

    --> {2, 14, 35, 0, 0, 0, 0}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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

-- init :: [a] -> [a]
on init(xs)
    if length of xs > 1 then
        items 1 thru -2 of xs
    else
        {}
    end if
end init

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

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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
    if length of xs > 0 then
        scanl(f, item 1 of xs, items 2 thru -1 of xs)
    else
        {}
    end if
end scanl1

-- scanr :: (b -> a -> b) -> b -> [a] -> [b]
on scanr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return reverse of lst
    end tell
end scanr

-- scanr1 :: (a -> a -> a) -> [a] -> [a]
on scanr1(f, xs)
    if length of xs > 0 then
        scanr(f, item -1 of xs, items 1 thru -2 of xs)
    else
        {}
    end if
end scanr1

-- sum :: Num a => [a] -> a
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```AppleScript
{2, 14, 35, 0, 0, 0, 0}
```



## AWK


```AWK

# syntax: GAWK -f WATER_COLLECTED_BETWEEN_TOWERS.AWK [-v debug={0|1}]
BEGIN {
    wcbt("1,5,3,7,2")
    wcbt("5,3,7,2,6,4,5,9,1,2")
    wcbt("2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1")
    wcbt("5,5,5,5")
    wcbt("5,6,7,8")
    wcbt("8,7,7,6")
    wcbt("6,7,10,7,6")
    exit(0)
}
function wcbt(str,  ans,hl,hr,i,n,tower) {
    n = split(str,tower,",")
    for (i=n; i>=0; i--) { # scan right to left
      hr[i] = max(tower[i],(i<n)?hr[i+1]:0)
    }
    for (i=0; i<=n; i++) { # scan left to right
      hl[i] = max(tower[i],(i!=0)?hl[i-1]:0)
      ans += min(hl[i],hr[i]) - tower[i]
    }
    printf("%4d : %s\n",ans,str)
    if (debug == 1) {
      for (i=1; i<=n; i++) { printf("%-4s",tower[i]) } ; print("tower")
      for (i=1; i<=n; i++) { printf("%-4s",hl[i]) } ; print("l-r")
      for (i=1; i<=n; i++) { printf("%-4s",hr[i]) } ; print("r-l")
      for (i=1; i<=n; i++) { printf("%-4s",min(hl[i],hr[i])) } ; print("min")
      for (i=1; i<=n; i++) { printf("%-4s",min(hl[i],hr[i])-tower[i]) } ; print("sum\n")
    }
}
function max(x,y) { return((x > y) ? x : y) }
function min(x,y) { return((x < y) ? x : y) }

```

{{out}}

```txt

   2 : 1,5,3,7,2
  14 : 5,3,7,2,6,4,5,9,1,2
  35 : 2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1
   0 : 5,5,5,5
   0 : 5,6,7,8
   0 : 8,7,7,6
   0 : 6,7,10,7,6

```



## C

Takes the integers as input from command line, prints out usage on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>

int getWater(int* arr,int start,int end,int cutoff){
	int i, sum = 0;

	for(i=start;i<=end;i++)
		sum += ((arr[cutoff] > arr[i])?(arr[cutoff] - arr[i]):0);

	return sum;
}

int netWater(int* arr,int size){
	int i, j, ref1, ref2, marker, markerSet = 0,sum = 0;

	if(size<3)
		return 0;

	for(i=0;i<size-1;i++){
		start:if(i!=size-2 && arr[i]>arr[i+1]){
				ref1 = i;

				for(j=ref1+1;j<size;j++){
					if(arr[j]>=arr[ref1]){
						ref2 = j;

						sum += getWater(arr,ref1+1,ref2-1,ref1);

						i = ref2;

						goto start;
					}

					else if(j!=size-1 && arr[j] < arr[j+1] && (markerSet==0||(arr[j+1]>=arr[marker]))){
						marker = j+1;
						markerSet = 1;
					}
				}

				if(markerSet==1){
					sum += getWater(arr,ref1+1,marker-1,marker);

					i = marker;

					markerSet = 0;

					goto start;
				}
			}
		}

	return sum;
}

int main(int argC,char* argV[])
{
	int *arr,i;

	if(argC==1)
		printf("Usage : %s <followed by space separated series of integers>");
	else{
		arr = (int*)malloc((argC-1)*sizeof(int));

		for(i=1;i<argC;i++)
			arr[i-1] = atoi(argV[i]);

		printf("Water collected : %d",netWater(arr,argC-1));
	}

	return 0;
}

```

Output :

```txt

C:\rosettaCode>waterTowers.exe 1 5 3 7 2
Water collected : 2
C:\rosettaCode>waterTowers.exe 5 3 7 2 6 4 5 9 1 2
Water collected : 14
C:\rosettaCode>waterTowers.exe 2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1
Water collected : 35
C:\rosettaCode>waterTowers.exe 5 5 5 5
Water collected : 0
C:\rosettaCode>waterTowers.exe 8 7 7 6
Water collected : 0
C:\rosettaCode>waterTowers.exe 6 7 10 7 6
Water collected : 0

```



## C++


```cpp

#include <iostream>
#include <vector>
#include <algorithm>

enum { EMPTY, WALL, WATER };

auto fill(const std::vector<int> b) {
  auto water = 0;
  const auto rows = *std::max_element(std::begin(b), std::end(b));
  const auto cols = std::size(b);
  std::vector<std::vector<int>> g(rows);
  for (auto& r : g) {
    for (auto i = 0; i < cols; ++i) {
      r.push_back(EMPTY);
    }
  }
  for (auto c = 0; c < cols; ++c) {
    for (auto r = rows - 1u, i = 0u; i < b[c]; ++i, --r) {
      g[r][c] = WALL;
    }
  }
  for (auto c = 0; c < cols - 1; ++c) {
    auto start_row = rows - b[c];
    while (start_row < rows) {
      if (g[start_row][c] == EMPTY) break;
      auto c2 = c + 1;
      bool hitWall = false;
      while (c2 < cols) {
        if (g[start_row][c2] == WALL) {
          hitWall = true;
          break;
        }
        ++c2;
      }
      if (hitWall) {
        for (auto i = c + 1; i < c2; ++i) {
          g[start_row][i] = WATER;
          ++water;
        }
      }
      ++start_row;
    }
  }
  return water;
}

int main() {
  std::vector<std::vector<int>> b = {
    { 1, 5, 3, 7, 2 },
    { 5, 3, 7, 2, 6, 4, 5, 9, 1, 2 },
    { 2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 },
    { 5, 5, 5, 5 },
    { 5, 6, 7, 8 },
    { 8, 7, 7, 6 },
    { 6, 7, 10, 7, 6 }
  };
  for (const auto v : b) {
    auto water = fill(v);
    std::cout << water << " water drops." << std::endl;
  }
  std::cin.ignore();
  std::cin.get();
  return 0;
}
```


{{out}}

```txt
2 water drops.
14 water drops.
35 water drops.
0 water drops.
0 water drops.
0 water drops.
0 water drops.
```


## C#

### Version 1

Translation from [[{{FULLPAGENAME}}#Visual_Basic_.NET|Visual Basic .NET]].  See that version 1 entry for code comment details and more sample output.

```c#
class Program
{
    static void Main(string[] args)
    {
        int[][] wta = {
            new int[] {1, 5, 3, 7, 2},   new int[] { 5, 3, 7, 2, 6, 4, 5, 9, 1, 2 },
            new int[] { 2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 },
            new int[] { 5, 5, 5, 5 },    new int[] { 5, 6, 7, 8 },
            new int[] { 8, 7, 7, 6 },    new int[] { 6, 7, 10, 7, 6 }};
        string blk, lf = "\n", tb = "██", wr = "≈≈", mt = "  ";
        for (int i = 0; i < wta.Length; i++)
        {
            int bpf; blk = ""; do
            {
                string floor = ""; bpf = 0; for (int j = 0; j < wta[i].Length; j++)
                {
                    if (wta[i][j] > 0)
                    {    floor += tb; wta[i][j] -= 1; bpf += 1; }
                    else floor += (j > 0 && j < wta[i].Length - 1 ? wr : mt);
                }
                if (bpf > 0) blk = floor + lf + blk;
            } while (bpf > 0);
            while (blk.Contains(mt + wr)) blk = blk.Replace(mt + wr, mt + mt);
            while (blk.Contains(wr + mt)) blk = blk.Replace(wr + mt, mt + mt);
            if (args.Length > 0) System.Console.Write("\n{0}", blk);
            System.Console.WriteLine("Block {0} retains {1,2} water units.",
                i + 1, (blk.Length - blk.Replace(wr, "").Length) / 2);
        }
    }
}
```
{{out}}<lang>Block 1 retains  2 water units.
Block 2 retains 14 water units.
Block 3 retains 35 water units.
Block 4 retains  0 water units.
Block 5 retains  0 water units.
Block 6 retains  0 water units.
Block 7 retains  0 water units.
```


### Version 2

Conventional "scanning" algorithm, translated from [[{{FULLPAGENAME}}#Version_2_2|the second version of Visual Basic.NET]], but (intentionally tweaked to be) incapable of verbose output.  See that version 2 entry for code comments and details.

```c#
class Program
{
// Variable names key:
//   i Iterator (of the tower block array).
// tba Tower block array.
// tea Tower elevation array.
// rht Right hand tower column number (position).
//  wu Water units (count).
// bof Blocks on floor (count).
// col Column number in elevation array (position).

    static void Main(string[] args)
    {
        int i = 1; int[][] tba = {new int[] { 1, 5, 3, 7, 2 },
        new int[] { 5, 3, 7, 2, 6, 4, 5, 9, 1, 2 },
        new int[] { 2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 },
        new int[] { 5, 5, 5, 5 },  new int[] { 5, 6, 7, 8 },
        new int[] { 8, 7, 7, 6 },  new int[] { 6, 7, 10, 7, 6 }};
        foreach (int[] tea in tba)
        {
            int rht, wu = 0, bof; do
            {
                for (rht = tea.Length - 1; rht >= 0; rht--)
                    if (tea[rht] > 0) break;
                if (rht < 0) break;
                bof = 0; for (int col = 0; col <= rht; col++)
                {
                    if (tea[col] > 0) { tea[col] -= 1; bof += 1; }
                    else if (bof > 0) wu++;
                }
                if (bof < 2) break;
            } while (true);
            System.Console.WriteLine(string.Format("Block {0} {1} water units.",
                i++, wu == 0 ? "does not hold any" : "holds " + wu.ToString()));
        }
    }
}
```

'''Output:'''

```txt
Block 1 holds 2 water units.
Block 2 holds 14 water units.
Block 3 holds 35 water units.
Block 4 does not hold any water units.
Block 5 does not hold any water units.
Block 6 does not hold any water units.
Block 7 does not hold any water units.
```



## Clojure

Similar two passes algorithm as many solutions here. First traverse left to right to find the highest tower on the left of each position, inclusive of the tower at the current position, than do the same to find the highest tower to the right of each position. Finally, compute the total water units held at any position as the difference of those two heights.


```clojure

(defn trapped-water [towers]
  (let [maxes #(reductions max %)                ; the seq of increasing max values found in the input seq
        maxl  (maxes towers)                     ; the seq of max heights to the left of each tower
        maxr  (reverse (maxes (reverse towers))) ; the seq of max heights to the right of each tower
        mins  (map min maxl maxr)]               ; minimum highest surrounding tower per position
    (reduce + (map - mins towers))))             ; sum up the trapped water per position

```

{{out}}

```clojure

;; in the following, # is a tower block and ~ is trapped water:
;;
;;          10|
;;           9|               #
;;           8|               #
;;           7|     # ~ ~ ~ ~ #
;;           6|     # ~ # ~ ~ #
;;           5| # ~ # ~ # ~ # #
;;           4| # ~ # ~ # # # #
;;           3| # # # ~ # # # #
;;           2| # # # # # # # # ~ #
;;           1| # # # # # # # # # #
;;         ---+---------------------
;;              5 3 7 2 6 4 5 9 1 2
(trapped-water [5 3 7 2 6 4 5 9 1 2]) ;; 14

```



## D

{{Trans|C#}}

```D
import std.stdio;

void main() {
    int i = 1;
    int[][] tba = [
        [ 1, 5, 3, 7, 2 ],
        [ 5, 3, 7, 2, 6, 4, 5, 9, 1, 2 ],
        [ 2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 ],
        [ 5, 5, 5, 5 ],
        [ 5, 6, 7, 8 ],
        [ 8, 7, 7, 6 ],
        [ 6, 7, 10, 7, 6 ]
    ];

    foreach (tea; tba) {
        int rht, wu, bof;
        do {
            for (rht = tea.length - 1; rht >= 0; rht--) {
                if (tea[rht] > 0) {
                    break;
                }
            }

            if (rht < 0) {
                break;
            }

            bof = 0;
            for (int col = 0; col <= rht; col++) {
                if (tea[col] > 0) {
                    tea[col] -= 1; bof += 1;
                } else if (bof > 0) {
                    wu++;
                }
            }
            if (bof < 2) {
                break;
            }
        } while (true);

        write("Block ", i++);
        if (wu == 0) {
            write(" does not hold any");
        } else {
            write(" holds ", wu);
        }
        writeln(" water units.");
    }
}
```


{{out}}

```txt
Block 1 holds 2 water units.
Block 2 holds 14 water units.
Block 3 holds 35 water units.
Block 4 does not hold any water units.
Block 5 does not hold any water units.
Block 6 does not hold any water units.
Block 7 does not hold any water units.
```



## Erlang

Implements a version that uses recursion to solve the problem functionally, using two passes without requiring list reversal or modifications. On the list iteration from head to tail, gather the largest element seen so far (being the highest one on the left). Once the list is scanned, each position returns the highest tower to its right as reported by its follower, along with the amount of water seen so far, which can then be used to calculate the value at the current position. Back at the first list element, the final result is gathered.


```erlang

-module(watertowers).
-export([towers/1, demo/0]).

towers(List) -> element(2, tower(List, 0)).

tower([], _) -> {0,0};
tower([H|T], MaxLPrev) ->
    MaxL = max(MaxLPrev, H),
    {MaxR, WaterAcc} = tower(T, MaxL),
    {max(MaxR,H), WaterAcc+max(0, min(MaxR,MaxL)-H)}.

demo() ->
    Cases = [[1, 5, 3, 7, 2],
             [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
             [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
             [5, 5, 5, 5],
             [5, 6, 7, 8],
             [8, 7, 7, 6],
             [6, 7, 10, 7, 6]],
    [io:format("~p -> ~p~n", [Case, towers(Case)]) || Case <- Cases],
    ok.

```


{{out}}

```txt

1> watertowers:demo().
[1,5,3,7,2] -> 2
[5,3,7,2,6,4,5,9,1,2] -> 14
[2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1] -> 35
[5,5,5,5] -> 0
[5,6,7,8] -> 0
[8,7,7,6] -> 0
[6,7,10,7,6] -> 0
ok

```


=={{header|F_Sharp|F#}}==
see http://stackoverflow.com/questions/24414700/water-collected-between-towers/43779936#43779936 for an explanation of this code. It is proportional to the number of towers. Although the examples on stackoverflow claim this, the n they use is actually the distance between the two end towers and not the number of towers. Consider the case of a tower of height 5 at 1, a tower of height 10 at 39, and a tower of height 3 at 101.

```fsharp

(*
A solution I'd show to Euclid !!!!.
Nigel Galloway May 4th., 2017
*)
let solve n =
  let (n,_)::(i,e)::g = n|>List.sortBy(fun n->(-(snd n)))
  let rec fn i g e l =
    match e with
    | (n,e)::t when n < i -> fn n g t (l+(i-n-1)*e)
    | (n,e)::t when n > g -> fn i n t (l+(n-g-1)*e)
    | (n,t)::e            -> fn i g e (l-t)
    | _                   -> l
  fn (min n i) (max n i) g (e*(abs(n-i)-1))

```

{{out}}

```txt

solve [(1,1);(2,5);(3,3);(4,7);(5,2)] -> 2
solve [(1,5);(2,3);(3,7);(4,2);(5,6);(6,4);(7,5);(8,9);(9,1);(10,2)] -> 14
solve [(1,2);(2,6);(3,3);(4,5);(5,2);(6,8);(7,1);(8,4);(9,2);(10,2);(11,5);(12,3);(13,5);(14,7);(15,4);(16,1)] -> 35
solve [(1,5);(2,5);(3,5);(4,5)] -> 0
solve [(1,5);(2,6);(3,7);(4,8)] -> 0
solve [(1,8);(2,7);(3,7);(4,6)] -> 0
solve [(1,6);(2,7);(3,10);(4,7);(5,6)] -> 0
solve [(1,5);(39,10);(101,3)] -> 368

```



## Factor


```factor
USING: formatting kernel math math.order math.statistics
sequences ;
IN: rosetta-code.water-towers

CONSTANT: test-cases {
    { 1 5 3 7 2 }
    { 5 3 7 2 6 4 5 9 1 2 }
    { 2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1 }
    { 5 5 5 5 }
    { 5 6 7 8 }
    { 8 7 7 6 }
    { 6 7 10 7 6 }
}

: area ( seq -- n )
    dup [ cum-max ] [ <reversed> cum-max reverse ] bi
    [ min ] 2map swap [ - ] 2map sum ;

test-cases [ dup area "%[%d, %] -> %d\n" printf ] each
```

{{out}}

```txt

{ 1, 5, 3, 7, 2 } -> 2
{ 5, 3, 7, 2, 6, 4, 5, 9, 1, 2 } -> 14
{ 2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 } -> 35
{ 5, 5, 5, 5 } -> 0
{ 5, 6, 7, 8 } -> 0
{ 8, 7, 7, 6 } -> 0
{ 6, 7, 10, 7, 6 } -> 0

```



## Go


```go

package main

import "fmt"

func maxl(hm []int ) []int{
	res := make([]int,len(hm))
	max := 1
	for i := 0; i < len(hm);i++{
		if(hm[i] > max){
			max = hm[i]
		}
		res[i] = max;
	}
	return res
}
func maxr(hm []int ) []int{
	res := make([]int,len(hm))
	max := 1
	for i := len(hm) - 1 ; i >= 0;i--{
		if(hm[i] > max){
			max = hm[i]
		}
		res[i] = max;
	}
	return res
}
func min(a,b []int)  []int {
	res := make([]int,len(a))
	for i := 0; i < len(a);i++{
		if a[i] >= b[i]{
			res[i] = b[i]
		}else {
			res[i] = a[i]
		}
	}
	return res
}
func diff(hm, min []int) []int {
	res := make([]int,len(hm))
	for i := 0; i < len(hm);i++{
		if min[i] > hm[i]{
			res[i] = min[i] - hm[i]
		}
	}
	return res
}
func sum(a []int) int {
	res := 0
	for i := 0; i < len(a);i++{
		res += a[i]
	}
	return res
}

func waterCollected(hm []int) int {
	maxr := maxr(hm)
	maxl := maxl(hm)
	min := min(maxr,maxl)
	diff := diff(hm,min)
	sum := sum(diff)
	return sum
}


func main() {
	fmt.Println(waterCollected([]int{1, 5, 3, 7, 2}))
	fmt.Println(waterCollected([]int{5, 3, 7, 2, 6, 4, 5, 9, 1, 2}))
	fmt.Println(waterCollected([]int{2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1}))
	fmt.Println(waterCollected([]int{5, 5, 5, 5}))
	fmt.Println(waterCollected([]int{5, 6, 7, 8}))
	fmt.Println(waterCollected([]int{8, 7, 7, 6}))
	fmt.Println(waterCollected([]int{6, 7, 10, 7, 6}))
}
```


{{out}}

```txt

2
14
35
0
0
0
0

```



## Groovy



```Groovy

Integer waterBetweenTowers(List<Integer> towers) {
    // iterate over the vertical axis. There the amount of water each row can hold is
    // the number of empty spots, minus the empty spots at the beginning and end
    return (1..towers.max()).collect { height ->
        // create a string representing the row, '#' for tower material and ' ' for air
        // use .trim() to remove spaces at beginning and end and then count remaining spaces
        towers.collect({ it >= height ? "#" : " " }).join("").trim().count(" ")
    }.sum()
}

tasks = [
    [1, 5, 3, 7, 2],
    [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
    [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
    [5, 5, 5, 5],
    [5, 6, 7, 8],
    [8, 7, 7, 6],
    [6, 7, 10, 7, 6]
]

tasks.each {
    println "$it => total water: ${waterBetweenTowers it}"
}

```


{{out}}

```txt

[1, 5, 3, 7, 2] => total water: 2
[5, 3, 7, 2, 6, 4, 5, 9, 1, 2] => total water: 14
[2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1] => total water: 35
[5, 5, 5, 5] => total water: 0
[5, 6, 7, 8] => total water: 0
[8, 7, 7, 6] => total water: 0
[6, 7, 10, 7, 6] => total water: 0

```



## Haskell


Following the approach of slightly modified [http://stackoverflow.com/users/1416525/cdk cdk]'s Haskell solution at [http://stackoverflow.com/questions/24414700/amazon-water-collected-between-towers/ Stack Overflow]. As recommended in [http://h2.jaguarpaw.co.uk/posts/data-structures-matter/ Programming as if the Correct Data Structure (and Performance) Mattered] it uses [http://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html Vector] instead of Array:


```haskell
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

waterCollected :: Vector Int -> Int
waterCollected =
  V.sum .                 -- Sum of the water depths over each of
  V.filter (> 0) .        -- the columns that are covered by some water.
  (
    V.zipWith (-) =<<     -- Where coverages are differences between:
    (
      V.zipWith min .     -- the lower water level in each case of:
      V.scanl1 max <*>    -- highest wall to left, and
      V.scanr1 max        -- highest wall to right.
    )
  )

main :: IO ()
main =
  mapM_
    (print . waterCollected)
    [ V.fromList [1, 5, 3, 7, 2]
    , V.fromList [5, 3, 7, 2, 6, 4, 5, 9, 1, 2]
    , V.fromList [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1]
    , V.fromList [5, 5, 5, 5]
    , V.fromList [5, 6, 7, 8]
    , V.fromList [8, 7, 7, 6]
    , V.fromList [6, 7, 10, 7, 6]
    ]

```

{{Out}}

```txt
2
14
35
0
0
0
0
```


## J

Inspired by [[#Julia]].

'''Solution:'''

```j>collectLevels =: >./\ <.
./\.                          NB. collect levels after filling
waterLevels=: collectLevels - ]                         NB. water levels for each tower
collectedWater=: +/@waterLevels                         NB. sum the units of water collected
printTowers =: ' ' , [: |.@|: '#~' #~ ] ,. waterLevels  NB. print a nice graph of towers and water
```


'''Examples:'''

```j
   collectedWater 5 3 7 2 6 4 5 9 1 2
14
   printTowers 5 3 7 2 6 4 5 9 1 2

       #
       #
  #~~~~#
  #~#~~#
#~#~#~##
#~#~####
###~####
########~#
##########

NB. Test cases
   TestTowers =: <@".;._2 noun define
1 5 3 7 2
5 3 7 2 6 4 5 9 1 2
2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1
5 5 5 5
5 6 7 8
8 7 7 6
6 7 10 7 6
)
   TestResults =: 2 14 35 0 0 0 0
   TestResults -: collectedWater &> TestTowers  NB. check tests
1
```



## Java

{{trans|D}}

```Java
public class WaterBetweenTowers {
    public static void main(String[] args) {
        int i = 1;
        int[][] tba = new int[][]{
            new int[]{1, 5, 3, 7, 2},
            new int[]{5, 3, 7, 2, 6, 4, 5, 9, 1, 2},
            new int[]{2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1},
            new int[]{5, 5, 5, 5},
            new int[]{5, 6, 7, 8},
            new int[]{8, 7, 7, 6},
            new int[]{6, 7, 10, 7, 6}
        };

        for (int[] tea : tba) {
            int rht, wu = 0, bof;
            do {
                for (rht = tea.length - 1; rht >= 0; rht--) {
                    if (tea[rht] > 0) {
                        break;
                    }
                }

                if (rht < 0) {
                    break;
                }

                bof = 0;
                for (int col = 0; col <= rht; col++) {
                    if (tea[col] > 0) {
                        tea[col]--;
                        bof += 1;
                    } else if (bof > 0) {
                        wu++;
                    }
                }
                if (bof < 2) {
                    break;
                }
            } while (true);

            System.out.printf("Block %d", i++);
            if (wu == 0) {
                System.out.print(" does not hold any");
            } else {
                System.out.printf(" holds %d", wu);
            }
            System.out.println(" water units.");
        }
    }
}
```

{{out}}

```txt
Block 1 holds 2 water units.
Block 2 holds 14 water units.
Block 3 holds 35 water units.
Block 4 does not hold any water units.
Block 5 does not hold any water units.
Block 6 does not hold any water units.
Block 7 does not hold any water units.

```



## JavaScript


### ES5

{{Trans|Haskell}}

```JavaScript
(function () {
    'use strict';

    // waterCollected :: [Int] -> Int
    var waterCollected = function (xs) {
        return sum(                   // water above each bar
            zipWith(function (a, b) {
                    return a - b;     // difference between water level and bar
                },
                zipWith(min,          // lower of two flanking walls
                    scanl1(max, xs),  // highest walls to left
                    scanr1(max, xs)   // highest walls to right
                ),
                xs                    // tops of bars
            )
            .filter(function (x) {
                return x > 0;         // only bars with water above them
            })
        );
    };

    // GENERIC FUNCTIONS ----------------------------------------

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    var zipWith = function (f, xs, ys) {
        var ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map(function (x, i) {
                return f(x, ys[i]);
            });
    };

    // scanl1 is a variant of scanl that has no starting value argument
    // scanl1 :: (a -> a -> a) -> [a] -> [a]
    var scanl1 = function (f, xs) {
        return xs.length > 0 ? scanl(f, xs[0], xs.slice(1)) : [];
    };

    // scanr1 is a variant of scanr that has no starting value argument
    // scanr1 :: (a -> a -> a) -> [a] -> [a]
    var scanr1 = function (f, xs) {
        return xs.length > 0 ? scanr(f, xs.slice(-1)[0], xs.slice(0, -1)) : [];
    };

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    var scanl = function (f, startValue, xs) {
        var lst = [startValue];
        return xs.reduce(function (a, x) {
            var v = f(a, x);
            return lst.push(v), v;
        }, startValue), lst;
    };

    // scanr :: (b -> a -> b) -> b -> [a] -> [b]
    var scanr = function (f, startValue, xs) {
        var lst = [startValue];
        return xs.reduceRight(function (a, x) {
            var v = f(a, x);
            return lst.push(v), v;
        }, startValue), lst.reverse();
    };

    // sum :: (Num a) => [a] -> a
    var sum = function (xs) {
        return xs.reduce(function (a, x) {
            return a + x;
        }, 0);
    };

    // max :: Ord a => a -> a -> a
    var max = function (a, b) {
        return a > b ? a : b;
    };

    // min :: Ord a => a -> a -> a
    var min = function (a, b) {
        return b < a ? b : a;
    };

    // TEST ---------------------------------------------------
    return [
        [1, 5, 3, 7, 2],
        [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
        [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
        [5, 5, 5, 5],
        [5, 6, 7, 8],
        [8, 7, 7, 6],
        [6, 7, 10, 7, 6]
    ].map(waterCollected);

    //--> [2, 14, 35, 0, 0, 0, 0]
})();
```


{{Out}}

```JavaScript
[2, 14, 35, 0, 0, 0, 0]
```



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // waterCollected :: [Int] -> Int
    const waterCollected = xs => {
        const
            maxToRight = scanr1(max, xs),
            maxToLeft = scanl1(max, xs),
            levels = zipWith(min, maxToLeft, maxToRight);
        return sum(
            zipWith(difference, levels, xs)
            .filter(x => x > 0)
        );
    };


    // GENERIC FUNCTIONS -----------------------------------------------------

    // difference :: (Num a) => a -> a -> a
    const difference = (a, b) => a - b;

    // max :: Ord a => a -> a -> a
    const max = (a, b) => a > b ? a : b;

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    const scanl = (f, startValue, xs) => {
        const lst = [startValue];
        return (
            xs.reduce((a, x) => {
                const v = f(a, x);
                return (lst.push(v), v);
            }, startValue),
            lst
        );
    };

    // scanl1 is a variant of scanl that has no starting value argument
    // scanl1 :: (a -> a -> a) -> [a] -> [a]
    const scanl1 = (f, xs) =>
        xs.length > 0 ? scanl(f, xs[0], xs.slice(1)) : [];

    // scanr :: (b -> a -> b) -> b -> [a] -> [b]
    const scanr = (f, startValue, xs) => {
        const lst = [startValue];
        return (
            xs.reduceRight((a, x) => {
                const v = f(a, x);
                return (lst.push(v), v);
            }, startValue),
            lst.reverse()
        );
    };

    // scanr1 is a variant of scanr that has no starting value argument
    // scanr1 :: (a -> a -> a) -> [a] -> [a]
    const scanr1 = (f, xs) =>
        xs.length > 0 ? scanr(f, xs.slice(-1)[0], xs.slice(0, -1)) : [];

    // sum :: (Num a) => [a] -> a
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    // TEST ------------------------------------------------------------------
    return [
        [1, 5, 3, 7, 2],
        [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
        [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
        [5, 5, 5, 5],
        [5, 6, 7, 8],
        [8, 7, 7, 6],
        [6, 7, 10, 7, 6]
    ].map(waterCollected);

    //--> [2, 14, 35, 0, 0, 0, 0]
})();
```

{{Out}}

```JavaScript
[2, 14, 35, 0, 0, 0, 0]
```



## Julia

{{works with|Julia|0.6}}
Inspired to [[#Python]].


```julia
function watercollected(towers::Vector{Int})
    high_lft = vcat(0, accumulate(max, towers[1:end-1]))
    high_rgt = vcat(reverse(accumulate(max, towers[end:-1:2])), 0)
    waterlvl = max.(min.(high_lft, high_rgt) .- towers, 0)
    return waterlvl
end

function towerprint(towers, levels)
    ctowers = copy(towers)
    clevels = copy(levels)
    hmax = maximum(towers)
    ntow = length(towers)
    for h in hmax:-1:1
        @printf("%2i |", h)
        for j in 1:ntow
            if ctowers[j] + clevels[j] ≥ h
                if clevels[j] > 0
                    cell = "≈≈"
                    clevels[j] -= 1
                else
                    cell = "NN"
                    ctowers[j] -= 1
                end
            else
                cell = "  "
            end
            print(cell)
        end
        println("|")
    end


    println("   " * join(lpad(t, 2) for t in levels) * ": Water lvl")
    println("   " * join(lpad(t, 2) for t in towers) * ": Tower lvl")
end

for towers in [[1, 5, 3, 7, 2], [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
    [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
    [5, 5, 5, 5], [5, 6, 7, 8], [8, 7, 7, 6], [6, 7, 10, 7, 6]]
    towerprint(towers, watercollected(towers))
    println()
end
```


{{out}}

```txt
 7 |      NN  |
 6 |      NN  |
 5 |  NN≈≈NN  |
 4 |  NN≈≈NN  |
 3 |  NNNNNN  |
 2 |  NNNNNNNN|
 1 |NNNNNNNNNN|
    0 0 2 0 0: Water lvl
    1 5 3 7 2: Tower lvl

 9 |              NN    |
 8 |              NN    |
 7 |    NN≈≈≈≈≈≈≈≈NN    |
 6 |    NN≈≈NN≈≈≈≈NN    |
 5 |NN≈≈NN≈≈NN≈≈NNNN    |
 4 |NN≈≈NN≈≈NNNNNNNN    |
 3 |NNNNNN≈≈NNNNNNNN    |
 2 |NNNNNNNNNNNNNNNN≈≈NN|
 1 |NNNNNNNNNNNNNNNNNNNN|
    0 2 0 5 1 3 2 0 1 0: Water lvl
    5 3 7 2 6 4 5 9 1 2: Tower lvl

 8 |          NN                    |
 7 |          NN≈≈≈≈≈≈≈≈≈≈≈≈≈≈NN    |
 6 |  NN≈≈≈≈≈≈NN≈≈≈≈≈≈≈≈≈≈≈≈≈≈NN    |
 5 |  NN≈≈NN≈≈NN≈≈≈≈≈≈≈≈NN≈≈NNNN    |
 4 |  NN≈≈NN≈≈NN≈≈NN≈≈≈≈NN≈≈NNNNNN  |
 3 |  NNNNNN≈≈NN≈≈NN≈≈≈≈NNNNNNNNNN  |
 2 |NNNNNNNNNNNN≈≈NNNNNNNNNNNNNNNN  |
 1 |NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN|
    0 0 3 1 4 0 6 3 5 5 2 4 2 0 0 0: Water lvl
    2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1: Tower lvl

 5 |NNNNNNNN|
 4 |NNNNNNNN|
 3 |NNNNNNNN|
 2 |NNNNNNNN|
 1 |NNNNNNNN|
    0 0 0 0: Water lvl
    5 5 5 5: Tower lvl

 8 |      NN|
 7 |    NNNN|
 6 |  NNNNNN|
 5 |NNNNNNNN|
 4 |NNNNNNNN|
 3 |NNNNNNNN|
 2 |NNNNNNNN|
 1 |NNNNNNNN|
    0 0 0 0: Water lvl
    5 6 7 8: Tower lvl

 8 |NN      |
 7 |NNNNNN  |
 6 |NNNNNNNN|
 5 |NNNNNNNN|
 4 |NNNNNNNN|
 3 |NNNNNNNN|
 2 |NNNNNNNN|
 1 |NNNNNNNN|
    0 0 0 0: Water lvl
    8 7 7 6: Tower lvl

10 |    NN    |
 9 |    NN    |
 8 |    NN    |
 7 |  NNNNNN  |
 6 |NNNNNNNNNN|
 5 |NNNNNNNNNN|
 4 |NNNNNNNNNN|
 3 |NNNNNNNNNN|
 2 |NNNNNNNNNN|
 1 |NNNNNNNNNN|
    0 0 0 0 0: Water lvl
    6 710 7 6: Tower lvl

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.2

fun waterCollected(tower: IntArray): Int {
    val n = tower.size
    val highLeft = listOf(0) + (1 until n).map { tower.slice(0 until it).max()!! }
    val highRight = (1 until n).map { tower.slice(it until n).max()!! } + 0
    return (0 until n).map { maxOf(minOf(highLeft[it], highRight[it]) - tower[it], 0) }.sum()
}

fun main(args: Array<String>) {
    val towers = listOf(
        intArrayOf(1, 5, 3, 7, 2),
        intArrayOf(5, 3, 7, 2, 6, 4, 5, 9, 1, 2),
        intArrayOf(2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1),
        intArrayOf(5, 5, 5, 5),
        intArrayOf(5, 6, 7, 8),
        intArrayOf(8, 7, 7, 6),
        intArrayOf(6, 7, 10, 7, 6)
    )
    for (tower in towers) {
        println("${"%2d".format(waterCollected(tower))} from ${tower.contentToString()}")
    }
}
```


{{out}}

```txt

 2 from [1, 5, 3, 7, 2]
14 from [5, 3, 7, 2, 6, 4, 5, 9, 1, 2]
35 from [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1]
 0 from [5, 5, 5, 5]
 0 from [5, 6, 7, 8]
 0 from [8, 7, 7, 6]
 0 from [6, 7, 10, 7, 6]

```



## M2000 Interpreter

===Scan min-max for each bar===

```M2000 Interpreter

Module Water {
      Flush ' empty stack
      Data (1, 5, 3, 7, 2)
      Data (5, 3, 7, 2, 6, 4, 5, 9, 1, 2)
      Data (2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1)
      Data (5, 5, 5, 5), (5, 6, 7, 8),(8, 7, 7, 6)
      Data (6, 7, 10, 7, 6)
      bars=stack.size  ' mark stack frame
      Dim bar()
      for bar=1 to bars
            bar()=Array  ' pop an array from stack
            acc=0
            For i=1 to len(bar())-2
                  level1=bar(i)
                  level2=level1
                  m=each(bar(), i+1, 1)
                  while m
                        if array(m)>level1 then level1=array(m)
                  End While
                  n=each(bar(), i+1, -1)
                  while n
                        if array(n)>level2 then level2=array(n)
                  End While
                  acc+=max.data(min(level1, level2)-bar(i), 0)
            Next i
            Data acc  ' push to end value
      Next bar
      finalwater=[]   ' is a stack object
      Print finalwater
}
Water

```


### Drain method

Module Water2 {
      Flush ' empty stack
      Data (1, 5, 3, 7, 2)
      Data (5, 3, 7, 2, 6, 4, 5, 9, 1, 2)
      Data (2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1)
      Data (5, 5, 5, 5), (5, 6, 7, 8),(8, 7, 7, 6)
      Data (6, 7, 10, 7, 6)
      bars=stack.size  ' mark stack frame
      Dim bar()
      For bar=1 to bars
            bar()=Array  ' pop an array from stack
            acc=0
            range=bar()#max()-bar()#min()
            if range>0 then
                  dim water(len(bar()))=bar()#max()
                  water(0)=bar(0)
                  water(len(bar())-1)=bar(len(bar())-1)
                  For j=1 to range-1
                        For i=1 to len(bar())-2
                              if water(i)>bar(i) then if water(i-1)<water(i) Then water(i)--
                        Next i
                        For i=len(bar())-2 to 1
                              if water(i)>bar(i) then if water(i+1)<water(i) Then water(i)--
                        Next i
                  Next j
                  Data water()#sum()-bar()#sum()
            Else
                  Data 0
            End if
      Next bar
      finalwater=[]
      Print finalwater
}
Water2

```M2000 Interpreter


```


### Faster Method

{{trans|AWK}}

```M2000 Interpreter

Module Water3 {
      Flush ' empty stack
      Data (1, 5, 3, 7, 2)
      Data (5, 3, 7, 2, 6, 4, 5, 9, 1, 2)
      Data (2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1)
      Data (5, 5, 5, 5), (5, 6, 7, 8),(8, 7, 7, 6)
      Data (6, 7, 10, 7, 6)
      bars=stack.size  ' mark stack frame
      Dim bar()
      for bar=1 to bars
            bar()=Array  ' pop an array from stack
            acc=0
            n=len(bar())-1
            dim hl(n+1), hr(n+1)
            For i=n to 0
                  hr(i)=max.data(bar(i), if(i<n->hr(i+1), 0))
            Next i
            For i=0 to n
                  hl(i)=max.data(bar(i), if(i>0->hl(i-1), 0))
                  acc+=min.data(hl(i), hr(i))-bar(i)
            Next i
            Data acc  ' push to end value
      Next bar
      finalwater=[]   ' is a stack object
      Print finalwater
}
Water3

```


{{out}}

```txt

 2 14 35 0 0 0 0
</pre >


## Perl


```perl
use Modern::Perl;
use List::Util qw{ min max sum };

sub water_collected {
    my @t = map { { TOWER => $_, LEFT => 0, RIGHT => 0, LEVEL => 0 } } @_;

    my ( $l, $r ) = ( 0, 0 );
    $_->{LEFT}  = ( $l = max( $l, $_->{TOWER} ) ) for @t;
    $_->{RIGHT} = ( $r = max( $r, $_->{TOWER} ) ) for reverse @t;
    $_->{LEVEL} = min( $_->{LEFT}, $_->{RIGHT} )  for @t;

    return sum map { $_->{LEVEL} > 0 ? $_->{LEVEL} - $_->{TOWER} : 0 } @t;
}

say join ' ', map { water_collected( @{$_} ) } (
    [ 1, 5,  3, 7, 2 ],
    [ 5, 3,  7, 2, 6, 4, 5, 9, 1, 2 ],
    [ 2, 6,  3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 ],
    [ 5, 5,  5, 5 ],
    [ 5, 6,  7, 8 ],
    [ 8, 7,  7, 6 ],
    [ 6, 7, 10, 7, 6 ],
);
```

{{Out}}

```txt
2 14 35 0 0 0 0
```



## Perl 6

{{Trans|Haskell}}


```perl6
sub max_l ( @a ) {  [\max] @a }
sub max_r ( @a ) { ([\max] @a.reverse).reverse }

sub water_collected ( @towers ) {
    return 0 if @towers <= 2;

    my @levels = max_l(@towers) »min« max_r(@towers);

    return ( @levels »-« @towers ).grep( * > 0 ).sum;
}

say map &water_collected,
    [ 1, 5,  3, 7, 2 ],
    [ 5, 3,  7, 2, 6, 4, 5, 9, 1, 2 ],
    [ 2, 6,  3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 ],
    [ 5, 5,  5, 5 ],
    [ 5, 6,  7, 8 ],
    [ 8, 7,  7, 6 ],
    [ 6, 7, 10, 7, 6 ],
;
```

{{Out}}

```txt
(2 14 35 0 0 0 0)
```



## Phix

=== inefficient one-pass method ===

```Phix
function collect_water(sequence heights)
    integer res = 0
    for i=2 to length(heights)-1 do
        integer lm = max(heights[1..i-1]),
                rm = max(heights[i+1..$]),
                d = min(lm,rm)-heights[i]
        res += max(0,d)
    end for
    return res
end function

constant tests = {{1,5,3,7,2},
                  {5,3,7,2,6,4,5,9,1,2},
                  {2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1},
                  {5,5,5,5},
                  {5,6,7,8},
                  {8,7,7,6},
                  {6,7,10,7,6}}

for i=1 to length(tests) do
    sequence ti = tests[i]
    printf(1,"%35s : %d\n",{sprint(ti),collect_water(ti)})
end for
```

{{out}}

```txt

                        {1,5,3,7,2} : 2
              {5,3,7,2,6,4,5,9,1,2} : 14
  {2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1} : 35
                          {5,5,5,5} : 0
                          {5,6,7,8} : 0
                          {8,7,7,6} : 0
                       {6,7,10,7,6} : 0

```

=== more efficient two-pass version ===

```Phix
function collect_water(sequence heights)

    integer left_max = heights[1],
            right_max = heights[$]
    sequence left_height = heights,
             right_height = heights

    for i=2 to length(heights)-1 do
        left_max = max(heights[i],left_max)
        left_height[i] = left_max
        right_max = max(heights[-i],right_max)
        right_height[-i] = right_max
    end for

    sequence mins = sq_min(left_height,right_height),
             diffs = sq_sub(mins,heights)

    return sum(diffs)
end function
```

(same output)


###  pretty print routine


```Phix
procedure print_water(sequence heights)
    integer res = 0, l = length(heights)
    sequence towers = repeat(repeat(' ',l),max(heights))
    for i=1 to l do
        for j=1 to heights[i] do
            towers[-j][i] = '#'
        end for
        if i>1 and i<l then
            integer lm = max(heights[1..i-1]),
                    rm = max(heights[i+1..$]),
                    m = min(lm,rm)
            for j=heights[i]+1 to m do
                towers[-j][i] = '~'
                res += 1
            end for
        end if
    end for
    printf(1,"%s\ncollected:%d\n",{join(towers,"\n"),res})
end procedure

print_water({5,3,7,2,6,4,5,9,1,2})
```

{{out}}

```txt

       #
       #
  #~~~~#
  #~#~~#
#~#~#~##
#~#~####
###~####
########~#
##########
collected:14

```



## PicoLisp


```PicoLisp
(de water (Lst)
   (sum
      '((A)
         (cnt
            nT
            (clip (mapcar '((B) (>= B A)) Lst)) ) )
      (range 1 (apply max Lst)) ) )
(println
   (mapcar
      water
      (quote
         (1 5 3 7 2)
         (5 3 7 2 6 4 5 9 1 2)
         (2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1)
         (5 5 5 5)
         (5 6 7 8)
         (8 7 7 6)
         (6 7 10 7 6) ) ) )
```

{{out}}

```txt
(2 14 35 0 0 0 0)
```



## Python

Based on the algorithm explained at [http://stackoverflow.com/questions/24414700/amazon-water-collected-between-towers/32135773#32135773 Stack Overflow]:


```python
def water_collected(tower):
    N = len(tower)
    highest_left = [0] + [max(tower[:n]) for n in range(1,N)]
    highest_right = [max(tower[n:N]) for n in range(1,N)] + [0]
    water_level = [max(min(highest_left[n], highest_right[n]) - tower[n], 0)
        for n in range(N)]
    print("highest_left:  ", highest_left)
    print("highest_right: ", highest_right)
    print("water_level:   ", water_level)
    print("tower_level:   ", tower)
    print("total_water:   ", sum(water_level))
    print("")
    return sum(water_level)

towers = [[1, 5, 3, 7, 2],
    [5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
    [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
    [5, 5, 5, 5],
    [5, 6, 7, 8],
    [8, 7, 7, 6],
    [6, 7, 10, 7, 6]]

[water_collected(tower) for tower in towers]
```

{{Out}}

```txt

highest_left:   [0, 1, 5, 5, 7]
highest_right:  [7, 7, 7, 2, 0]
water_level:    [0, 0, 2, 0, 0]
tower_level:    [1, 5, 3, 7, 2]
total_water:    2

highest_left:   [0, 5, 5, 7, 7, 7, 7, 7, 9, 9]
highest_right:  [9, 9, 9, 9, 9, 9, 9, 2, 2, 0]
water_level:    [0, 2, 0, 5, 1, 3, 2, 0, 1, 0]
tower_level:    [5, 3, 7, 2, 6, 4, 5, 9, 1, 2]
total_water:    14

highest_left:   [0, 2, 6, 6, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8]
highest_right:  [8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 4, 1, 0]
water_level:    [0, 0, 3, 1, 4, 0, 6, 3, 5, 5, 2, 4, 2, 0, 0, 0]
tower_level:    [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1]
total_water:    35

highest_left:   [0, 5, 5, 5]
highest_right:  [5, 5, 5, 0]
water_level:    [0, 0, 0, 0]
tower_level:    [5, 5, 5, 5]
total_water:    0

highest_left:   [0, 5, 6, 7]
highest_right:  [8, 8, 8, 0]
water_level:    [0, 0, 0, 0]
tower_level:    [5, 6, 7, 8]
total_water:    0

highest_left:   [0, 8, 8, 8]
highest_right:  [7, 7, 6, 0]
water_level:    [0, 0, 0, 0]
tower_level:    [8, 7, 7, 6]
total_water:    0

highest_left:   [0, 6, 7, 10, 10]
highest_right:  [10, 10, 7, 6, 0]
water_level:    [0, 0, 0, 0, 0]
tower_level:    [6, 7, 10, 7, 6]
total_water:    0

[2, 14, 35, 0, 0, 0, 0]
```



## Racket


```racket
#lang racket/base
(require racket/match)

(define (water-collected-between-towers towers)
  (define (build-tallest-left/rev-list t mx/l rv)
    (match t
      [(list) rv]
      [(cons a d)
       (define new-mx/l (max a mx/l))
       (build-tallest-left/rev-list d new-mx/l (cons mx/l rv))]))

  (define (collect-from-right t tallest/l mx/r rv)
    (match t
      [(list) rv]
      [(cons a d)
       (define new-mx/r (max a mx/r))
       (define new-rv (+ rv (max (- (min new-mx/r (car tallest/l)) a) 0)))
       (collect-from-right d (cdr tallest/l) new-mx/r new-rv)]))

  (define reversed-left-list (build-tallest-left/rev-list towers 0 null))
  (collect-from-right (reverse towers) reversed-left-list 0 0))

(module+ test
  (require rackunit)
  (check-equal?
   (let ((towerss
          '[[1 5 3 7 2]
            [5 3 7 2 6 4 5 9 1 2]
            [2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1]
            [5 5 5 5]
            [5 6 7 8]
            [8 7 7 6]
            [6 7 10 7 6]]))
     (map water-collected-between-towers towerss))
   (list 2 14 35 0 0 0 0)))
```


When run produces no output -- meaning that the tests have run successfully.


## REXX


### version 1


```rexx
/* REXX */
Call bars '1 5 3 7 2'
Call bars '5 3 7 2 6 4 5 9 1 2'
Call bars '2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1'
Call bars '5 5 5 5'
Call bars '5 6 7 8'
Call bars '8 7 7 6'
Call bars '6 7 10 7 6'
Exit
bars:
Parse Arg bars
bar.0=words(bars)
high=0
box.=' '
Do i=1 To words(bars)
  bar.i=word(bars,i)
  high=max(high,bar.i)
  Do j=1 To bar.i
    box.i.j='x'
    End
  End
m=1
w=0
Do Forever
  Do i=m+1 To bar.0
    If bar.i>bar.m Then
      Leave
    End
  If i>bar.0 Then Leave
  n=i
  Do i=m+1 To n-1
    w=w+bar.m-bar.i
    Do j=bar.i+1 To bar.m
      box.i.j='*'
      End
    End
  m=n
  End
m=bar.0
Do Forever
  Do i=bar.0 To 1 By -1
    If bar.i>bar.m Then
      Leave
    End
  If i<1 Then Leave
  n=i
  Do i=m-1 To n+1 By -1
    w=w+bar.m-bar.i
    Do j=bar.i+1 To bar.m
      box.i.j='*'
      End
    End
  m=n
  End
Say bars '->' w
Call show
Return
show:
Do j=high To 1 By -1
  ol=''
  Do i=1 To bar.0
    ol=ol box.i.j
    End
  Say ol
  End
Return
```

{{out}}

```txt
1 5 3 7 2 -> 2
       x
       x
   x * x
   x * x
   x x x
   x x x x
 x x x x x
5 3 7 2 6 4 5 9 1 2 -> 14
               x
               x
     x * * * * x
     x * x * * x
 x * x * x * x x
 x * x * x x x x
 x x x * x x x x
 x x x x x x x x * x
 x x x x x x x x x x
2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1 -> 35
           x
           x * * * * * * * x
   x * * * x * * * * * * * x
   x * x * x * * * * x * x x
   x * x * x * x * * x * x x x
   x x x * x * x * * x x x x x
 x x x x x x * x x x x x x x x
 x x x x x x x x x x x x x x x x
5 5 5 5 -> 0
 x x x x
 x x x x
 x x x x
 x x x x
 x x x x
5 6 7 8 -> 0
       x
     x x
   x x x
 x x x x
 x x x x
 x x x x
 x x x x
 x x x x
8 7 7 6 -> 0
 x
 x x x
 x x x x
 x x x x
 x x x x
 x x x x
 x x x x
 x x x x
6 7 10 7 6 -> 0
     x
     x
     x
   x x x
 x x x x x
 x x x x x
 x x x x x
 x x x x x
 x x x x x
 x x x x x
```


===version 2, simple numeric list output===

```rexx
/*REXX program calculates and displays the amount of rainwater collected between towers.*/
       call tower  1  5  3  7  2
       call tower  5  3  7  2  6  4  5  9  1  2
       call tower  2  6  3  5  2  8  1  4  2  2  5  3  5  7  4  1
       call tower  5  5  5  5
       call tower  5  6  7  8
       call tower  8  7  7  6
       call tower  6  7 10  7  6
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tower: procedure; arg y;  #=words(y); t.=0; L.=0 /*the T. array holds the tower heights.*/
            do j=1  for #;    t.j= word(y, j)    /*construct the towers,                */
            _= j-1;           L.j= max(t._, L._) /*    "      "  left─most tallest tower*/
            end   /*j*/
       R.=0
            do b=#  by -1  for #;  _= b+1; R.b= max(t._, R._) /*right─most tallest tower*/
            end   /*b*/
       w.=0                                                       /*rainwater collected.*/
            do f=1  for #;  if t.f>=L.f | t.f>=R.f  then iterate  /*rain between towers?*/
            w.f= min(L.f, R.f) - t.f;     w.00= w.00+w.f          /*rainwater collected.*/
            end   /*f*/
       say right(w.00, 9) 'units of rainwater collected for: '  y /*display water units.*/
       return
```

{{out|output}}

```txt

        2 units of rainwater collected for:  1 5 3 7 2
       14 units of rainwater collected for:  5 3 7 2 6 4 5 9 1 2
       35 units of rainwater collected for:  2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1
        0 units of rainwater collected for:  5 5 5 5
        0 units of rainwater collected for:  5 6 7 8
        0 units of rainwater collected for:  8 7 7 6
        0 units of rainwater collected for:  6 7 10 7 6

```


===version 3, with ASCII art===
This REXX version shows a scale and a representation of the towers and water collected.

It tries to protect the aspect ration by showing the buildings as in this task's preamble.

```rexx
/*REXX program calculates and displays the amount of rainwater collected between towers.*/
       call tower  1  5  3  7  2
       call tower  5  3  7  2  6  4  5  9  1  2
       call tower  2  6  3  5  2  8  1  4  2  2  5  3  5  7  4  1
       call tower  5  5  5  5
       call tower  5  6  7  8
       call tower  8  7  7  6
       call tower  6  7 10  7  6
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tower: procedure; arg y; #= words(y); t.=0; L.=0 /*the T. array holds the tower heights.*/
            do j=1  for #;  t.j=word(y,j); _=j-1 /*construct the towers;  max height.   */
            L.j=max(t._, L._); t.0=max(t.0, t.j) /*left-most tallest tower; build scale.*/
            end   /*j*/
       R.=0
            do b=#  by -1  for #; _= b+1;  R.b= max(t._, R._) /*right-most tallest tower*/
            end   /*b*/
       w.=0                                                       /*rainwater collected.*/
            do f=1  for #;  if t.f>=L.f | t.f>=R.f  then iterate  /*rain between towers?*/
            w.f= min(L.f, R.f) - t.f;     w.00= w.00 + w.f        /*rainwater collected.*/
            end   /*f*/
       if w.00==0  then w.00= 'no'               /*pretty up wording for "no rainwater".*/
       ratio= 2                                  /*used to maintain a good aspect ratio.*/
       p.=                                       /*P.  stores plot versions of towers.  */
            do c=0  to #;  cc= c * ratio         /*construct the plot+scale for display.*/
              do h=1  for t.c+w.c;    glyph= '█' /*maybe show a floor of some tower(s). */
                       if h>t.c  then glyph= '≈' /*  "     "  rainwater between towers. */
              if c==0  then p.h= overlay(right(h, 9)         , p.h,  1   ) /*tower scale*/
                       else p.h= overlay(copies(glyph,ratio) , p.h, 10+cc) /*build tower*/
              end   /*h*/
            end     /*c*/
       p.1= overlay(w.00  'units of rainwater collected', p.1, 15*ratio+#) /*append text*/
            do z=t.0  by -1  to 0;     say p.z   /*display various tower floors & water.*/
            end     /*z*/
       return
```

{{out|output}}

```txt

        7        ██
        6        ██
        5    ██≈≈██
        4    ██≈≈██
        3    ██████
        2    ████████
        1  ██████████             2 units of rainwater collected

        9                ██
        8                ██
        7      ██≈≈≈≈≈≈≈≈██
        6      ██≈≈██≈≈≈≈██
        5  ██≈≈██≈≈██≈≈████
        4  ██≈≈██≈≈████████
        3  ██████≈≈████████
        2  ████████████████≈≈██
        1  ████████████████████        14 units of rainwater collected

        8            ██
        7            ██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
        6    ██≈≈≈≈≈≈██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
        5    ██≈≈██≈≈██≈≈≈≈≈≈≈≈██≈≈████
        4    ██≈≈██≈≈██≈≈██≈≈≈≈██≈≈██████
        3    ██████≈≈██≈≈██≈≈≈≈██████████
        2  ████████████≈≈████████████████
        1  ████████████████████████████████  35 units of rainwater collected

        5  ████████
        4  ████████
        3  ████████
        2  ████████
        1  ████████              no units of rainwater collected

        8        ██
        7      ████
        6    ██████
        5  ████████
        4  ████████
        3  ████████
        2  ████████
        1  ████████              no units of rainwater collected

        8  ██
        7  ██████
        6  ████████
        5  ████████
        4  ████████
        3  ████████
        2  ████████
        1  ████████              no units of rainwater collected

       10      ██
        9      ██
        8      ██
        7    ██████
        6  ██████████
        5  ██████████
        4  ██████████
        3  ██████████
        2  ██████████
        1  ██████████             no units of rainwater collected

```



## Ruby


```ruby

def a(array)
n=array.length
left={}
right={}
left[0]=array[0]
i=1
loop do
   break if i >=n
left[i]=[left[i-1],array[i]].max
   i += 1
end
right[n-1]=array[n-1]
i=n-2
loop do
break if i<0
 right[i]=[right[i+1],array[i]].max
i-=1
end
i=0
water=0
loop do
break if i>=n
water+=[left[i],right[i]].min-array[i]
i+=1
end
puts water
end

a([ 5, 3,  7, 2, 6, 4, 5, 9, 1, 2 ])
a([ 2, 6,  3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 ])
a([ 5, 5,  5, 5 ])
a([ 5, 6,  7, 8 ])
a([ 8, 7,  7, 6 ])
a([ 6, 7, 10, 7, 6 ])
return
```

'''output'''

```txt

14
35
0
0
0
0

```


## Rust


```rust

use std::cmp::min;

fn getfill(pattern: &[usize]) -> usize {
    let mut total = 0;
    for (idx, val) in pattern.iter().enumerate() {
        let l_peak = pattern[..idx].iter().max();
        let r_peak = pattern[idx + 1..].iter().max();
        if l_peak.is_some() && r_peak.is_some() {
            let peak = min(l_peak.unwrap(), r_peak.unwrap());
            if peak > val {
                total += peak - val;
            }
        }
    }
    total
}

fn main() {
    let patterns = vec![
        vec![1, 5, 3, 7, 2],
        vec![5, 3, 7, 2, 6, 4, 5, 9, 1, 2],
        vec![2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1],
        vec![5, 5, 5, 5],
        vec![5, 6, 7, 8],
        vec![8, 7, 7, 6],
        vec![6, 7, 10, 7, 6],
    ];

    for pattern in patterns {
        println!("pattern: {:?}, fill: {}", &pattern, getfill(&pattern));
    }
}

```

'''output'''

```txt

pattern: [1, 5, 3, 7, 2], fill: 2
pattern: [5, 3, 7, 2, 6, 4, 5, 9, 1, 2], fill: 14
pattern: [2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1], fill: 35
pattern: [5, 5, 5, 5], fill: 0
pattern: [5, 6, 7, 8], fill: 0
pattern: [8, 7, 7, 6], fill: 0
pattern: [6, 7, 10, 7, 6], fill: 0

```



## Scala


### No sweat.

{{Out}}See it yourself by running in your browser either by [https://scalafiddle.io/sf/jx29Ace/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/5nXQHfR0T0iauHNWSTRntg Scastie (remote JVM)].
{{libheader|Scala Concise}}
{{libheader|Scala Parallel Programming}}
{{libheader|Scala Time complexity O(n)}}
{{libheader|ScalaFiddle qualified}}
{{libheader|Scastie qualified}}
{{works with|Scala|2.13}}

```Scala
import scala.collection.parallel.CollectionConverters.VectorIsParallelizable

// Program to find maximum amount of water
// that can be trapped within given set of bars.
object TrappedWater extends App {
  private val barLines = List(
    Vector(1, 5, 3, 7, 2),
    Vector(5, 3, 7, 2, 6, 4, 5, 9, 1, 2),
    Vector(2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1),
    Vector(5, 5, 5, 5),
    Vector(5, 6, 7, 8),
    Vector(8, 7, 7, 6),
    Vector(6, 7, 10, 7, 6)).zipWithIndex

  // Method for maximum amount of water
  private def sqBoxWater(barHeights: Vector[Int]): Int = {
    def maxOfLeft = barHeights.par.scanLeft(0)(math.max).tail
    def maxOfRight = barHeights.par.scanRight(0)(math.max).init

    def waterlevels = maxOfLeft.zip(maxOfRight)
      .map { case (maxL, maxR) => math.min(maxL, maxR) }

    waterlevels.zip(barHeights).map { case (level, towerHeight) => level - towerHeight }.sum
  }

  barLines.foreach(barSet =>
    println(s"Block ${barSet._2 + 1} could hold max. ${sqBoxWater(barSet._1)} units."))

}
```


## Scheme


```scheme
(import (scheme base)
        (scheme write))

(define (total-collected chart)
  (define (highest-left vals curr)
    (if (null? vals)
      (list curr)
      (cons curr
            (highest-left (cdr vals) (max (car vals) curr)))))
  (define (highest-right vals curr)
    (reverse (highest-left (reverse vals) curr)))
  ;
  (if (< (length chart) 3) ; catch the end cases
    0
    (apply +
           (map (lambda (l c r)
                  (if (or (<= l c)
                          (<= r c))
                    0
                    (- (min l r) c)))
                (highest-left chart 0)
                chart
                (highest-right chart 0)))))

(for-each
  (lambda (chart)
    (display chart) (display " -> ") (display (total-collected chart)) (newline))
  '((1 5 3 7 2)
    (5 3 7 2 6 4 5 9 1 2)
    (2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1)
    (5 5 5 5)
    (5 6 7 8)
    (8 7 7 6)
    (6 7 10 7 6)))
```

{{out}}

```txt
(1 5 3 7 2) -> 2
(5 3 7 2 6 4 5 9 1 2) -> 14
(2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1) -> 35
(5 5 5 5) -> 0
(5 6 7 8) -> 0
(8 7 7 6) -> 0
(6 7 10 7 6) -> 0
(3 1 2) -> 1
(1) -> 0
() -> 0
(1 2) -> 0
```



## Sidef


```ruby
func max_l(Array a, m = a[0]) {
    gather { a.each {|e| take(m = max(m, e)) } }
}

func max_r(Array a) {
    max_l(a.flip).flip
}

func water_collected(Array towers) {
    var levels = (max_l(towers) »min« max_r(towers))
    (levels »-« towers).grep{ _ > 0 }.sum
}

[
    [ 1, 5,  3, 7, 2 ],
    [ 5, 3,  7, 2, 6, 4, 5, 9, 1, 2 ],
    [ 2, 6,  3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1 ],
    [ 5, 5,  5, 5 ],
    [ 5, 6,  7, 8 ],
    [ 8, 7,  7, 6 ],
    [ 6, 7, 10, 7, 6 ],
].map { water_collected(_) }.say
```

{{out}}

```txt

[2, 14, 35, 0, 0, 0, 0]

```



## Tcl

Tcl makes for a surprisingly short and readable implementation, next to some of the more functional-oriented languages.

```Tcl
namespace path {::tcl::mathfunc ::tcl::mathop}

proc flood {ground} {
    set lefts [
        set d 0
        lmap g $ground {
            set d [max $d $g]
        }
    ]
    set ground [lreverse $ground]
    set rights [
        set d 0
        lmap g $ground {
            set d [max $d $g]
        }
    ]
    set rights [lreverse $rights]
    set ground [lreverse $ground]
    set water [lmap l $lefts r $rights {min $l $r}]
    set depths [lmap g $ground w $water {- $w $g}]
    + {*}$depths
}

foreach p {
    {5 3 7 2 6 4 5 9 1 2}
    {1 5 3 7 2}
    {5 3 7 2 6 4 5 9 1 2}
    {2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1}
    {5 5 5 5}
    {5 6 7 8}
    {8 7 7 6}
    {6 7 10 7 6}
} {
    puts [flood $p]:\t$p
}
```


{{out}}

```txt
14:        5 3 7 2 6 4 5 9 1 2
2:      1 5 3 7 2
14:     5 3 7 2 6 4 5 9 1 2
35:     2 6 3 5 2 8 1 4 2 2 5 3 5 7 4 1
0:      5 5 5 5
0:      5 6 7 8
0:      8 7 7 6
0:      6 7 10 7 6
```



## Visual Basic .NET


### Version 1

'''Method:''' Instead of "scanning" adjoining towers for each column, this routine converts the tower data into a string representation with building blocks, empty spaces, and potential water retention sites.  The potential water retention sites are then "eroded" away where they are found to be unsupported.  This is accomplished with the '''.Replace()''' function.  The replace operations are unleashed upon the entire "block" of towers, rather than a cell at a time or a line at a time - which perhaps increases the program's execution-time, but reduces program's complexity.

The program can optionally display the interim string representation of each tower block before the final count is completed.  I've since modified it to have the same block and wavy characters are the
[[{{FULLPAGENAME}}#version_3|REXX 9.3]] output, but used the double-wide columns, as pictured in the task definition area.

```vbnet
' Convert tower block data into a string representation, then manipulate that.
Module Module1
    Sub Main(Args() As String)
        Dim shoTow As Boolean = Environment.GetCommandLineArgs().Count > 1  ' Show towers.
        Dim wta As Integer()() = {                       ' Water tower array (input data).
            New Integer() {1, 5, 3, 7, 2}, New Integer() {5, 3, 7, 2, 6, 4, 5, 9, 1, 2},
            New Integer() {2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1},
            New Integer() {5, 5, 5, 5}, New Integer() {5, 6, 7, 8},
            New Integer() {8, 7, 7, 6}, New Integer() {6, 7, 10, 7, 6}}
        Dim blk As String,                   ' String representation of a block of towers.
            lf As String = vbLf,      ' Line feed to separate floors in a block of towers.
            tb = "██", wr = "≈≈", mt = "  "    ' Tower Block, Water Retained, eMpTy space.
        For i As Integer = 0 To wta.Length - 1
            Dim bpf As Integer                    ' Count of tower blocks found per floor.
            blk = ""
            Do
                bpf = 0 : Dim floor As String = ""  ' String representation of each floor.
                For j As Integer = 0 To wta(i).Length - 1
                    If wta(i)(j) > 0 Then      ' Tower block detected, add block to floor,
                        floor &= tb : wta(i)(j) -= 1 : bpf += 1    '  reduce tower by one.
                    Else  '      Empty space detected, fill when not first or last column.
                        floor &= If(j > 0 AndAlso j < wta(i).Length - 1, wr, mt)
                    End If
                Next
                If bpf > 0 Then blk = floor & lf & blk ' Add floors until blocks are gone.
            Loop Until bpf = 0                       ' No tower blocks left, so terminate.
            ' Erode potential water retention cells from left and right.
            While blk.Contains(mt & wr) : blk = blk.Replace(mt & wr, mt & mt) : End While
            While blk.Contains(wr & mt) : blk = blk.Replace(wr & mt, mt & mt) : End While
            ' Optionaly show towers w/ water marks.
            If shoTow Then Console.Write("{0}{1}", lf, blk)
            ' Subtract the amount of non-water mark characters from the total char amount.
            Console.Write("Block {0} retains {1,2} water units.{2}", i + 1,
                                     (blk.Length - blk.Replace(wr, "").Length) \ 2, lf)
        Next
    End Sub
End Module
```

{{out}}<lang>Block 1 retains  2 water units.
Block 2 retains 14 water units.
Block 3 retains 35 water units.
Block 4 retains  0 water units.
Block 5 retains  0 water units.
Block 6 retains  0 water units.
Block 7 retains  0 water units.
```

Verbose output shows towers with water ("Almost equal to" characters) left in the "wells" between towers.  Just supply any command-line parameter to see it.  Use no command line parameters to see the plain output above.
<lang>      ██
      ██
  ██≈≈██
  ██≈≈██
  ██████
  ████████
██████████
Block 1 retains  2 water units.

              ██
              ██
    ██≈≈≈≈≈≈≈≈██
    ██≈≈██≈≈≈≈██
██≈≈██≈≈██≈≈████
██≈≈██≈≈████████
██████≈≈████████
████████████████≈≈██
████████████████████
Block 2 retains 14 water units.

          ██
          ██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
  ██≈≈≈≈≈≈██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
  ██≈≈██≈≈██≈≈≈≈≈≈≈≈██≈≈████
  ██≈≈██≈≈██≈≈██≈≈≈≈██≈≈██████
  ██████≈≈██≈≈██≈≈≈≈██████████
████████████≈≈████████████████
████████████████████████████████
Block 3 retains 35 water units.

████████
████████
████████
████████
████████
Block 4 retains  0 water units.

      ██
    ████
  ██████
████████
████████
████████
████████
████████
Block 5 retains  0 water units.

██
██████
████████
████████
████████
████████
████████
████████
Block 6 retains  0 water units.

    ██
    ██
    ██
  ██████
██████████
██████████
██████████
██████████
██████████
██████████
Block 7 retains  0 water units.
```


### Version 2

'''Method:''' More conventional "scanning" method.  A Char array is used, but no Replace() statements.  Output is similar to version 1, although there is now a left margin of three spaces, the results statement is immediately to the right of the string representation of the tower blocks (instead of underneath), the verb is "hold(s)" instead of "retains", and there is a special string when the results indicate zero.


```vbnet
Module Module1
    ''' <summary>
    ''' wide - Widens the aspect ratio of a linefeed separated string.
    ''' </summary>
    ''' <param name="src">A string representing a block of towers.</param>
    ''' <param name="margin">Optional padding for area to the left.</param>
    ''' <returns>A double-wide version of the string.</returns>
    Function wide(src As String, Optional margin As String = "") As String
        Dim res As String = margin : For Each ch As Char In src
            res += If(ch < " ", ch & margin, ch + ch) : Next : Return res
    End Function

    ''' <summary>
    ''' cntChar - Counts characters, also custom formats the output.
    ''' </summary>
    ''' <param name="src">The string to count characters in.</param>
    ''' <param name="ch">The character to be counted.</param>
    ''' <param name="verb">Verb to include in format.  Expecting "hold",
    '''             but can work with "retain" or "have".</param>
    ''' <returns>The count of chars found in a string, and formats a verb.</returns>
    Function cntChar(src As String, ch As Char, verb As String) As String
        Dim cnt As Integer = 0
        For Each c As Char In src : cnt += If(c = ch, 1, 0) : Next
        Return If(cnt = 0, "does not " & verb & " any",
            verb.Substring(0, If(verb = "have", 2, 4)) & "s " & cnt.ToString())
    End Function

    ''' <summary>
    ''' report - Produces a report of the number of rain units found in
    '''          a block of towers, optionally showing the towers.
    '''          Autoincrements the blkID for each report.
    ''' </summary>
    ''' <param name="tea">An int array with tower elevations.</param>
    ''' <param name="blkID">An int of the block of towers ID.</param>
    ''' <param name="verb">The verb to use in the description.
    '''                    Defaults to "has / have".</param>
    ''' <param name="showIt">When true, the report includes a string representation
    '''                      of the block of towers.</param>
    ''' <returns>A string containing the amount of rain units, optionally preceeded by
    '''          a string representation of the towers holding any water.</returns>
    Function report(tea As Integer(),                             ' Tower elevation array.
                    ByRef blkID As Integer,                ' Block ID for the description.
                    Optional verb As String = "have",    ' Verb to use in the description.
                    Optional showIt As Boolean = False) As String    ' Show representaion.
        Dim block As String = "",                                   ' The block of towers.
            lf As String = vbLf,                           ' The separator between floors.
            rTwrPos As Integer        ' The position of the rightmost tower of this floor.
        Do
            For rTwrPos = tea.Length - 1 To 0 Step -1      ' Determine the rightmost tower
                If tea(rTwrPos) > 0 Then Exit For          '      postition on this floor.
            Next
            If rTwrPos < 0 Then Exit Do         ' When no towers remain, exit the do loop.
            ' init the floor to a space filled Char array, as wide as the block of towers.
            Dim floor As Char() = New String(" ", tea.Length).ToCharArray()
            Dim bpf As Integer = 0                  ' The count of blocks found per floor.
            For column As Integer = 0 To rTwrPos                ' Scan from left to right.
                If tea(column) > 0 Then                     ' If a tower exists here,
                    floor(column) = "█"                     ' mark the floor with a block,
                    tea(column) -= 1                    ' drop the tower elevation by one,
                    bpf += 1                           ' and advance the block count.
                ElseIf bpf > 0 Then    ' Otherwise, see if a tower is present to the left.
                    floor(column) = "≈"                           ' OK to fill with water.
                End If
            Next
            If bpf > If(showIt, 0, 1) Then       ' Continue the building only when needed.
                ' If not showing blocks, discontinue building when a single tower remains.
                ' build tower blocks string with each floor added to top.
                block = New String(floor) & If(block = "", "", lf) & block
            Else
                Exit Do                          ' Ran out of towers, so exit the do loop.
            End If
        Loop While True ' Depending on previous break statements to terminate the do loop.
        blkID += 1                                           ' increment block ID counter.
        ' format report and return it.
        Return If(showIt, String.Format(vbLf & "{0}", wide(block, "   ")), "") &
            String.Format(" Block {0} {1} water units.", blkID, cntChar(block, "≈", verb))
    End Function

    ''' <summary>
    ''' Main routine.
    '''
    ''' With one command line parameter, it shows tower blocks,
    '''  with no command line parameters, it shows a plain report
    '''</summary>
    Sub Main()
        Dim shoTow As Boolean = Environment.GetCommandLineArgs().Count > 1  ' Show towers.
        Dim blkCntr As Integer = 0        ' Block ID for reports.
        Dim verb As String = "hold"    ' "retain" or "have" can be used instead of "hold".
        Dim tea As Integer()() = {New Integer() {1, 5, 3, 7, 2},   ' Tower elevation data.
            New Integer() {5, 3, 7, 2, 6, 4, 5, 9, 1, 2},
            New Integer() {2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1},
            New Integer() {5, 5, 5, 5}, New Integer() {5, 6, 7, 8},
            New Integer() {8, 7, 7, 6}, New Integer() {6, 7, 10, 7, 6}}
        For Each block As Integer() In tea
            ' Produce report for each block of towers.
            Console.WriteLine(report(block, blkCntr, verb, shoTow))
        Next
    End Sub
End Module
```

Regular version 2 output:
<lang> Block 1 holds 2 water units.
 Block 2 holds 14 water units.
 Block 3 holds 35 water units.
 Block 4 does not hold any water units.
 Block 5 does not hold any water units.
 Block 6 does not hold any water units.
 Block 7 does not hold any water units.
```

Sample of version 2 verbose output:
<lang>             ██
             ██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
     ██≈≈≈≈≈≈██≈≈≈≈≈≈≈≈≈≈≈≈≈≈██
     ██≈≈██≈≈██≈≈≈≈≈≈≈≈██≈≈████
     ██≈≈██≈≈██≈≈██≈≈≈≈██≈≈██████
     ██████≈≈██≈≈██≈≈≈≈██████████
   ████████████≈≈████████████████
   ████████████████████████████████ Block 3 holds 35 water units.

   ████████
   ████████
   ████████
   ████████
   ████████ Block 4 does not hold any water units.
```



## Yabasic

{{trans|AWK}}

```Yabasic
data 7
data "1,5,3,7,2", "5,3,7,2,6,4,5,9,1,2", "2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1"
data "5,5,5,5", "5,6,7,8", "8,7,7,6", "6,7,10,7,6"

read n

for i = 1 to n
	read n$
	wcbt(n$)
next i

sub wcbt(s$)
	local tower$(1), hr(1), hl(1), n, i, ans, k

	n = token(s$, tower$(), ",")

	redim hr(n)
	redim hl(n)
	for i = n to 1 step -1
		if i < n then
			k = hr(i + 1)
		else
			k = 0
		end if
		hr(i) = max(val(tower$(i)), k)
	next i
	for i = 1 to n
		if i then
			k = hl(i - 1)
		else
			k = 0
		end if
		hl(i) = max(val(tower$(i)), k)
		ans = ans + min(hl(i), hr(i)) - val(tower$(i))
	next i
	print ans," ",n$
end sub
```



## zkl

{{trans|Haskell}}

```zkl
fcn waterCollected(walls){
     // compile max wall heights from left to right and right to left
     // then each pair is left/right wall of that cell.
     // Then the min of each wall pair == water height for that cell
   scanl(walls,(0).max)     // scan to right, f is max(0,a,b)
  .zipWith((0).MAX.min,     // f is MAX.min(a,b) == min(a,b)
           scanl(walls.reverse(),(0).max).reverse()) // right to left
     // now subtract the wall height from the water level and add 'em up
   .zipWith('-,walls).filter('>(0)).sum(0);
}
fcn scanl(xs,f,i=0){ // aka reduce but save list of results
   xs.reduce('wrap(s,x,a){ s=f(s,x); a.append(s); s },i,ss:=List());
   ss
} // scanl((1,5,3,7,2),max,0) --> (1,5,5,7,7)
```


```zkl
T( T(1, 5, 3, 7, 2), T(5, 3, 7, 2, 6, 4, 5, 9, 1, 2),
   T(2, 6, 3, 5, 2, 8, 1, 4, 2, 2, 5, 3, 5, 7, 4, 1),
   T(5, 5, 5, 5), T(5, 6, 7, 8),T(8, 7, 7, 6),
   T(6, 7, 10, 7, 6) )
.pump(List, waterCollected).println();
```

{{out}}

```txt

L(2,14,35,0,0,0,0)

```

