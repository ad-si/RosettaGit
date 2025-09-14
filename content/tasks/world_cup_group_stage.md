+++
title = "World Cup group stage"
description = ""
date = 2019-10-18T20:54:35Z
aliases = []
[extra]
id = 17721
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "common_lisp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "erlang",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "yabasic",
  "zkl",
]
+++

## Task

{{task}}It's World Cup season (or at least it was when this page was created)! The World Cup is an international football/soccer tournament that happens every 4 years. Countries put their international teams together in the years between tournaments and qualify for the tournament based on their performance in other international games. Once a team has qualified they are put into a group with 3 other teams. For the first part of the World Cup tournament the teams play in "group stage" games where each of the four teams in a group [[wp:Round-robin tournament|plays all three other teams once]]. The results of these games determine which teams will move on to the "knockout stage" which is a standard single-elimination tournament. The two teams from each group with the most standings points move on to the knockout stage. Each game can result in a win for one team and a loss for the other team or it can result in a draw/tie for each team. A win is worth three points in the standings. A draw/tie is worth one point. A loss is not worth any points.

Generate all possible outcome combinations for the six group stage games. With three possible outcomes for each game there should be 3<sup>6</sup> = 729 of them. Calculate the standings points for each team with each combination of outcomes. Show a histogram (graphical, ASCII art, or straight counts--whichever is easiest/most fun) of the standings points for all four teams over all possible outcomes.

Don't worry about tiebreakers as they can get complicated. We are basically looking to answer the question "if a team gets x standings points, where can they expect to end up in the group standings?".

<small>''Hint: there should be no possible way to end up in second place with less than two points as well as no way to end up in first with less than three. Oddly enough, there is no way to get 8 points at all.''</small>

## C#
<!-- By Martin Freedman, 17/01/2018 -->
Unlike the Python solution, this does not use a library for combinations and cartesian products but provides 4 1-liner Linq methods.

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static System.Console;
using static System.Linq.Enumerable;

namespace WorldCupGroupStage
{
    public static class WorldCupGroupStage
    {
        static int[][] _histogram;

        static WorldCupGroupStage()
        {
            int[] scoring = new[] { 0, 1, 3 };

            _histogram = Repeat<Func<int[]>>(()=>new int[10], 4).Select(f=>f()).ToArray();

            var teamCombos = Range(0, 4).Combinations(2).Select(t2=>t2.ToArray()).ToList();

            foreach (var results in Range(0, 3).CartesianProduct(6))
            {
                var points = new int[4];

                foreach (var (result, teams) in results.Zip(teamCombos, (r, t) => (r, t)))
                {
                    points[teams[0]] += scoring[result];
                    points[teams[1]] += scoring[2 - result];
                }

                foreach(var (p,i) in points.OrderByDescending(a => a).Select((p,i)=>(p,i)))
                    _histogram[i][p]++;
            }
        }

       // https://gist.github.com/martinfreedman/139dd0ec7df4737651482241e48b062f

       static IEnumerable<IEnumerable<T>> CartesianProduct<T>(this IEnumerable<IEnumerable<T>> seqs) =>
            seqs.Aggregate(Empty<T>().ToSingleton(), (acc, sq) => acc.SelectMany(a => sq.Select(s => a.Append(s))));

       static IEnumerable<IEnumerable<T>> CartesianProduct<T>(this IEnumerable<T> seq, int repeat = 1) =>
            Repeat(seq, repeat).CartesianProduct();

       static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> seq) =>
            seq.Aggregate(Empty<T>().ToSingleton(), (a, b) => a.Concat(a.Select(x => x.Append(b))));

       static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> seq, int numItems) =>
            seq.Combinations().Where(s => s.Count() == numItems);

        private static IEnumerable<T> ToSingleton<T>(this T item) { yield return item; }

        static new string ToString()
        {
            var sb = new StringBuilder();

            var range = String.Concat(Range(0, 10).Select(i => $"{i,-3} "));
            sb.AppendLine($"Points      : {range}");

            var u = String.Concat(Repeat("─", 40+13));
            sb.AppendLine($"{u}");

            var places = new[] { "First", "Second", "Third", "Fourth" };
            foreach (var row in _histogram.Select((r, i) => (r, i)))
            {
                sb.Append($"{places[row.i],-6} place: ");
                foreach (var standing in row.r)
                    sb.Append($"{standing,-3} ");
                sb.Append("\n");
            }

            return sb.ToString();
        }

        static void Main(string[] args)
        {
            Write(ToString());
            Read();
        }
    }
}

```

Produces:

```txt

Points      : 0   1   2   3   4   5   6   7   8   9
─────────────────────────────────────────────────────
First  place: 0   0   0   1   14  148 152 306 0   108
Second place: 0   0   4   33  338 172 164 18  0   0
Third  place: 0   18  136 273 290 4   8   0   0   0
Fourth place: 108 306 184 125 6   0   0   0   0   0

```



## Common Lisp

```lisp
(defun histo ()
  (let ((scoring (vector 0 1 3))
        (histo (list (vector 0 0 0 0 0 0 0 0 0 0) (vector 0 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0 0) (vector 0 0 0 0 0 0 0 0 0 0)))
        (team-combs (vector '(0 1) '(0 2) '(0 3) '(1 2) '(1 3) '(2 3)))
        (single-tupel)
        (sum))
       ; six nested dotimes produces the tupels of the cartesian product of
       ; six lists like '(0 1 2), but without to store all tuples in a list
       (dotimes (x0 3) (dotimes (x1 3) (dotimes (x2 3)
       (dotimes (x3 3) (dotimes (x4 3) (dotimes (x5 3)
           (setf single-tupel (vector x0 x1 x2 x3 x4 x5))
           (setf sum (vector 0 0 0 0))
           (dotimes (i (length single-tupel))
               (setf (elt sum (first (elt team-combs i)))
                     (+ (elt sum (first (elt team-combs i)))
                        (elt scoring (elt single-tupel i))))

               (setf (elt sum (second (elt team-combs i)))
                     (+ (elt sum (second (elt team-combs i)))
                        (elt scoring (- 2 (elt single-tupel i))))))

           (dotimes (i (length (sort sum #'<)))
               (setf (elt (nth i histo) (elt sum i))
                     (1+ (elt (nth i histo) (elt sum i)))))
       ))))))
       (reverse histo)))

; friendly output
(dolist (el (histo))
    (dotimes (i (length el))
        (format t "~3D " (aref el i)))
    (format t "~%"))
```

```txt

  0   0   0   1  14 148 152 306   0 108
  0   0   4  33 338 172 164  18   0   0
  0  18 136 273 290   4   8   0   0   0
108 306 184 125   6   0   0   0   0   0

```



## D

This imports the module of the third D solution of the Combinations Task.
```d
void main() {
    import std.stdio, std.range, std.array, std.algorithm, combinations3;

    immutable scoring = [0, 1, 3];
    /*immutable*/ auto r3 = [0, 1, 2];
    immutable combs = 4.iota.array.combinations(2).array;
    uint[10][4] histo;

    foreach (immutable results; cartesianProduct(r3, r3, r3, r3, r3, r3)) {
        int[4] s;
        foreach (immutable r, const g; [results[]].zip(combs)) {
            s[g[0]] += scoring[r];
            s[g[1]] += scoring[2 - r];
        }

        foreach (immutable i, immutable v; s[].sort().release)
            histo[i][v]++;
    }
    writefln("%(%s\n%)", histo[].retro);
}
```

```txt
[0, 0, 0, 1, 14, 148, 152, 306, 0, 108]
[0, 0, 4, 33, 338, 172, 164, 18, 0, 0]
[0, 18, 136, 273, 290, 4, 8, 0, 0, 0]
[108, 306, 184, 125, 6, 0, 0, 0, 0, 0]
```


This alternative version is not fully idiomatic D, it shows what to currently do to tag the main function of the precedent version as @nogc.

```d
import core.stdc.stdio, std.range, std.array, std.algorithm, combinations3;

immutable uint[2][6] combs = 4u.iota.array.combinations(2).array;

void main() nothrow @nogc {
    immutable uint[3] scoring = [0, 1, 3];
    uint[10][4] histo;

    foreach (immutable r0; 0 .. 3)
     foreach (immutable r1; 0 .. 3)
      foreach (immutable r2; 0 .. 3)
       foreach (immutable r3; 0 .. 3)
        foreach (immutable r4; 0 .. 3)
         foreach (immutable r5; 0 .. 3) {
            uint[4] s;
            foreach (immutable i, immutable r; [r0, r1, r2, r3, r4, r5]) {
                s[combs[i][0]] += scoring[r];
                s[combs[i][1]] += scoring[2 - r];
            }

            foreach (immutable i, immutable v; s[].sort().release)
                histo[i][v]++;
         }

    foreach_reverse (const ref h; histo) {
        foreach (immutable x; h)
            printf("%u ", x);
        printf("\n");
    }
}
```

```txt
0 0 0 1 14 148 152 306 0 108
0 0 4 33 338 172 164 18 0 0
0 18 136 273 290 4 8 0 0 0
108 306 184 125 6 0 0 0 0 0
```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public singleton program
{
    static games := new string[]::("12", "13", "14", "23", "24", "34");

    static results := "000000";

    nextResult()
    {
        var s := results;

        if (results=="222222") { ^ false };

        results := (results.toInt(3) + 1).toString(3).padLeft($48, 6);

        ^ true
    }

    closure()
    {
        var points := new IntMatrix(4, 10);

        int counter := 0;
        do
        {
            var records := new int[]::(0,0,0,0);

            for(int i := 0, i < 6, i += 1)
            {
                var r := results[i];
                r =>
                    "2" { records[games[i][0].toInt() - 49] := records[games[i][0].toInt() - 49] + 3 }
                    "1" {
                        records[games[i][0].toInt() - 49] := records[games[i][0].toInt() - 49] + 1;
                        records[games[i][1].toInt() - 49] := records[games[i][1].toInt() - 49] + 1
                    }
                    "0" { records[games[i][1].toInt() - 49] := records[games[i][1].toInt() - 49] + 3 };
            };

            records := records.ascendant();

            for(int i := 0, i <= 3, i += 1)
                {
                    points[i][records[i]] := points[i][records[i]] + 1
                }
        }
        while:(program.nextResult());

        new Range(0, 4).zipForEach(new string[]::("1st", "2nd", "3rd", "4th"), (i,l)
        {
            console.printLine(l,": ", points[3 - i].toArray())
        });
    }
}
```

```txt

1st: 0,0,0,1,14,148,152,306,0,108
2nd: 0,0,4,33,338,172,164,18,0,0
3rd: 0,18,136,273,290,4,8,0,0,0
4th: 108,306,184,125,6,0,0,0,0,0

```



## Elixir

```elixir
defmodule World_Cup do
  def group_stage do
    results = [[3,0],[1,1],[0,3]]
    teams = [0,1,2,3]
    allresults = combos(2,teams) |> combinations(results)
    allpoints = for list <- allresults, do: (for {l1,l2} <- list, do: Enum.zip(l1,l2)) |> List.flatten
    totalpoints = for list <- allpoints, do: (for t <- teams, do: {t, Enum.sum(for {t_,points} <- list, t_==t, do: points)} )
    sortedtotalpoints = for list <- totalpoints, do: Enum.sort(list,fn({_,a},{_,b}) -> a > b end)
    pointsposition = for n <- teams, do: (for list <- sortedtotalpoints, do: elem(Enum.at(list,n),1))
    for n <- teams do
      for points <- 0..9 do
        Enum.at(pointsposition,n) |> Enum.filter(&(&1 == points)) |> length
      end
    end
  end

  defp combos(1, list), do: (for x <- list, do: [x])
  defp combos(k, list) when k == length(list), do: [list]
  defp combos(k, [h|t]) do
    (for subcombos <- combos(k-1, t), do: [h | subcombos]) ++ (combos(k, t))
  end

  defp combinations([h],list2), do: (for item <- list2, do: [{h,item}])
  defp combinations([h|t],list2) do
    for item <- list2, comb <- combinations(t,list2), do: [{h,item} | comb]
  end
end

format = String.duplicate("~4w", 10) <> "~n"
:io.format(format, Enum.to_list(0..9))
IO.puts String.duplicate(" ---", 10)
Enum.each(World_Cup.group_stage, fn x -> :io.format(format, x) end)
```


```txt

   0   1   2   3   4   5   6   7   8   9
 --- --- --- --- --- --- --- --- --- ---
   0   0   0   1  14 148 152 306   0 108
   0   0   4  33 338 172 164  18   0   0
   0  18 136 273 290   4   8   0   0   0
 108 306 184 125   6   0   0   0   0   0

```



## Erlang

This solution take advantage of the expressiveness power of the list comprehensions expressions. Function ''combos'' is copied from  [http://panduwana.wordpress.com/2010/04/21/combination-in-erlang/ panduwana blog].


```erlang

-module(world_cup).

-export([group_stage/0]).

group_stage() ->
	Results = [[3,0],[1,1],[0,3]],
	Teams = [1,2,3,4],
	Matches = combos(2,Teams),
	AllResults =
		combinations(Matches,Results),
	AllPoints =
		[lists:flatten([lists:zip(L1,L2) || {L1,L2} <- L]) || L <- AllResults],
	TotalPoints =
		[ [ {T,lists:sum([Points || {T_,Points} <- L, T_ == T])} || T <- Teams] || L <- AllPoints],
	SortedTotalPoints =
		[ lists:sort(fun({_,A},{_,B}) -> A > B end,L) || L <- TotalPoints],
	PointsPosition =
		[ [element(2,lists:nth(N, L))|| L <- SortedTotalPoints ] || N <- Teams],
	[ [length(lists:filter(fun(Points_) -> Points_ == Points end,lists:nth(N, PointsPosition) ))
		|| Points <- lists:seq(0,9)] || N <- Teams].

combos(1, L) ->
	[[X] || X <- L];
combos(K, L) when K == length(L) ->
	[L];
combos(K, [H|T]) ->
    [[H | Subcombos] || Subcombos <- combos(K-1, T)]
    ++ (combos(K, T)).

combinations([H],List2) ->
	[[{H,Item}] || Item <- List2];
combinations([H|T],List2) ->
	[ [{H,Item} | Comb] || Item <- List2, Comb <- combinations(T,List2)].

```


Output:

```txt

[[0,0,0,1,14,148,152,306,0,108],
 [0,0,4,33,338,172,164,18,0,0],
 [0,18,136,273,290,4,8,0,0,0],
 [108,306,184,125,6,0,0,0,0,0]]

```



## Go

```go
package main

import (
    "fmt"
    "sort"
    "strconv"
)

var games = [6]string{"12", "13", "14", "23", "24", "34"}
var results = "000000"

func nextResult() bool {
    if results == "222222" {
        return false
    }
    res, _ := strconv.ParseUint(results, 3, 32)
    results = fmt.Sprintf("%06s", strconv.FormatUint(res+1, 3))
    return true
}

func main() {
    var points [4][10]int
    for {
        var records [4]int
        for i := 0; i < len(games); i++ {
            switch results[i] {
            case '2':
                records[games[i][0]-'1'] += 3
            case '1':
                records[games[i][0]-'1']++
                records[games[i][1]-'1']++
            case '0':
                records[games[i][1]-'1'] += 3
            }
        }
        sort.Ints(records[:])
        for i := 0; i < 4; i++ {
            points[i][records[i]]++
        }
        if !nextResult() {
            break
        }
    }
    fmt.Println("POINTS       0    1    2    3    4    5    6    7    8    9")
    fmt.Println("-------------------------------------------------------------")
    places := [4]string{"1st", "2nd", "3rd", "4th"}
    for i := 0; i < 4; i++ {
        fmt.Print(places[i], " place    ")
        for j := 0; j < 10; j++ {
            fmt.Printf("%-5d", points[3-i][j])
        }
        fmt.Println()
    }
}
```


```txt

POINTS       0    1    2    3    4    5    6    7    8    9
-------------------------------------------------------------
1st place    0    0    0    1    14   148  152  306  0    108
2nd place    0    0    4    33   338  172  164  18   0    0
3rd place    0    18   136  273  290  4    8    0    0    0
4th place    108  306  184  125  6    0    0    0    0    0

```



## J


There might be a more elegant way of expressing this.


```J
require'stats'
outcome=: 3 0,1 1,:0 3
pairs=: (i.4) e."1(2 comb 4)
standings=: +/@:>&>,{<"1<"1((i.4) e."1 pairs)#inv"1/ outcome
```


Here, standings represents all the possible outcomes:

```J
   $standings
729 4
```


Of course, not all of them are distinct:

```J
   $~.standings
556 4
```


With only 556 distinct outcomes, there must be some repeats. Looking at this more closely (gathering the outcomes in identical groups, counting how many members are in each group, and then considering the unique list of group sizes):

```J
   ~.#/.~standings
1 2 3 4 6
```


Some standings can be attained two different ways, some three different ways, some four different ways, and some six different ways. Let's look at the one with six different possibilities:


```J
   (I.6=#/.~standings){{./.~standings
4 4 4 4
```


That's where every team gets 4 standing.

How about outcomes which can be achieved four different ways?


```J
   (I.4=#/.~standings){{./.~standings
6 6 3 3
6 3 3 6
6 3 6 3
3 6 6 3
3 6 3 6
3 3 6 6
```


Ok, that's simple. So how about a histogram. Actually, it's not clear what a histogram should represent. There are four different teams, and possible standings range from 0 through 9, so we could show the outcomes for each team:


```J
   +/standings =/ i.10
27 81 81 108 162 81 81 81 0 27
27 81 81 108 162 81 81 81 0 27
27 81 81 108 162 81 81 81 0 27
27 81 81 108 162 81 81 81 0 27
```


Each team has the same possible outcomes. So instead, let's order the results so that instead of seeing each team as a distinct entity we are seeing the first place, second place, third place and fourth place team (and where there's a tie, such as 4 4 4 4, we just arbitrarily say that one of the tied teams gets in each of the tied places...):


```J
   +/(\:"1~ standings)=/"1 i.10
  0   0   0   1  14 148 152 306 0 108
  0   0   4  33 338 172 164  18 0   0
  0  18 136 273 290   4   8   0 0   0
108 306 184 125   6   0   0   0 0   0
```


Here, the first row represents whichever team came in first in each outcome, the second row represents the second place team and so on.

Meanwhile, the leftmost column represents the number of outcomes where that team would have zero standing, the second column represents the number of outcomes where that team would have one standing and so on. (The rightmost column represents the number of outcomes where that team would have 9 standing.)


## Java

This example codes results as a 6-digit number in base 3. Each digit is a game. A 2 is a win for the team on the left, a 1 is a draw, and a 0 is a loss for the team on the left.

```java5
import java.util.Arrays;

public class GroupStage{
    //team left digit vs team right digit
    static String[] games = {"12", "13", "14", "23", "24", "34"};
    static String results = "000000";//start with left teams all losing

    private static boolean nextResult(){
        if(results.equals("222222")) return false;
        int res = Integer.parseInt(results, 3) + 1;
        results = Integer.toString(res, 3);
        while(results.length() < 6) results = "0" + results;	//left pad with 0s
        return true;
    }

    public static void main(String[] args){
        int[][] points = new int[4][10]; 		//playing 3 games, points range from 0 to 9
        do{
            int[] records = {0,0,0,0};
            for(int i = 0; i < 6; i++){
                switch(results.charAt(i)){
                    case '2': records[games[i].charAt(0) - '1'] += 3; break;    //win for left team
                    case '1':                                                   //draw
                        records[games[i].charAt(0) - '1']++;
                        records[games[i].charAt(1) - '1']++;
                        break;
                    case '0': records[games[i].charAt(1) - '1'] += 3; break;    //win for right team
                }
            }
            Arrays.sort(records);	//sort ascending, first place team on the right
            points[0][records[0]]++;
            points[1][records[1]]++;
            points[2][records[2]]++;
            points[3][records[3]]++;
        }while(nextResult());
        System.out.println("First place: " + Arrays.toString(points[3]));
        System.out.println("Second place: " + Arrays.toString(points[2]));
        System.out.println("Third place: " + Arrays.toString(points[1]));
        System.out.println("Fourth place: " + Arrays.toString(points[0]));
    }
}
```

```txt
First place: [0, 0, 0, 1, 14, 148, 152, 306, 0, 108]
Second place: [0, 0, 4, 33, 338, 172, 164, 18, 0, 0]
Third place: [0, 18, 136, 273, 290, 4, 8, 0, 0, 0]
Fourth place: [108, 306, 184, 125, 6, 0, 0, 0, 0, 0]
```



## Julia

```julia
function worldcupstages()
    games = ["12", "13", "14", "23", "24", "34"]
    results = "000000"

    function nextresult()
        if (results == "222222")
            return false
        end
        results = lpad(string(parse(Int, results, base=3) + 1, base=3), 6, '0')
        true
    end

    points = zeros(Int, 4, 10)
    while true
        records = zeros(Int, 4)
        for i in 1:length(games)
            if results[i] == '2'
                records[games[i][1] - '0'] += 3
            elseif results[i] == '1'
                records[games[i][1] - '0'] += 1
                records[games[i][2] - '0'] += 1
            elseif results[i] == '0'
                records[games[i][2] - '0'] += 3
            end
        end
        sort!(records)
        for i in 1:4
            points[i, records[i] + 1] += 1
        end
        if !nextresult()
            break
        end
    end

    for (i, place) in enumerate(["First", "Second", "Third", "Fourth"])
        println("$place place: $(points[5 - i, :])")
    end
end

worldcupstages()

```
```txt

First place: [0, 0, 0, 1, 14, 148, 152, 306, 0, 108]
Second place: [0, 0, 4, 33, 338, 172, 164, 18, 0, 0]
Third place: [0, 18, 136, 273, 290, 4, 8, 0, 0, 0]
Fourth place: [108, 306, 184, 125, 6, 0, 0, 0, 0, 0]

```



## Kotlin

```scala
// version 1.1.2

val games = arrayOf("12", "13", "14", "23", "24", "34")
var results = "000000"

fun nextResult(): Boolean {
    if (results == "222222") return false
    val res = results.toInt(3) + 1
    results = res.toString(3).padStart(6, '0')
    return true
}

fun main(args: Array<String>) {
    val points = Array(4) { IntArray(10) }
    do {
        val records = IntArray(4)
        for (i in 0..5) {
            when (results[i]) {
                '2' -> records[games[i][0] - '1'] += 3
                '1' -> { records[games[i][0] - '1']++ ; records[games[i][1] - '1']++ }
                '0' -> records[games[i][1] - '1'] += 3
            }
        }
        records.sort()
        for (i in 0..3) points[i][records[i]]++
    }
    while(nextResult())
    println("POINTS       0    1    2    3    4    5    6    7    8    9")
    println("-------------------------------------------------------------")
    val places = arrayOf("1st", "2nd", "3rd", "4th")
    for (i in 0..3) {
        print("${places[i]} place    ")
        points[3 - i].forEach { print("%-5d".format(it)) }
        println()
    }
}
```


```txt

POINTS       0    1    2    3    4    5    6    7    8    9
-------------------------------------------------------------
1st place    0    0    0    1    14   148  152  306  0    108
2nd place    0    0    4    33   338  172  164  18   0    0
3rd place    0    18   136  273  290  4    8    0    0    0
4th place    108  306  184  125  6    0    0    0    0    0

```



## Perl

```perl
use Math::Cartesian::Product;

@scoring = (0, 1, 3);
push @histo, [(0) x 10] for 1..4;
push @aoa,    [(0,1,2)] for 1..6;

for $results (cartesian {@_} @aoa) {
    my @s;
    my @g = ([0,1],[0,2],[0,3],[1,2],[1,3],[2,3]);
    for (0..$#g) {
        $r = $results->[$_];
        $s[$g[$_][0]] += $scoring[$r];
        $s[$g[$_][1]] += $scoring[2 - $r];
    }

    my @ss = sort @s;
    $histo[$_][$ss[$_]]++ for 0..$#s;
}

$fmt = ('%3d ') x 10 . "\n";
printf $fmt, @$_ for reverse @histo;
```

```txt
  0   0   0   1  14 148 152 306   0 108
  0   0   4  33 338 172 164  18   0   0
  0  18 136 273 290   4   8   0   0   0
108 306 184 125   6   0   0   0   0   0
```



## Perl 6

```perl6
constant scoring = 0, 1, 3;
my @histo = [0 xx 10] xx 4;

for [X] ^3 xx 6 -> @results {
    my @s;

    for @results Z (^4).combinations(2) -> ($r, @g) {
        @s[@g[0]] += scoring[$r];
        @s[@g[1]] += scoring[2 - $r];
    }

    for @histo Z @s.sort -> (@h, $v) {
        ++@h[$v];
    }
}

say .fmt('%3d',' ') for @histo.reverse;
```

```txt
  0   0   0   1  14 148 152 306   0 108
  0   0   4  33 338 172 164  18   0   0
  0  18 136 273 290   4   8   0   0   0
108 306 184 125   6   0   0   0   0   0
```



## Phix

There is no official combinations() routine in phix, instead the documentation for
permute() shows two examples that you are expected to copy and modify, every time.

In this case I used both, and modified both to fit their particular tasks (games and results).

Some credit is due to the Kotlin entry for inspiring some of the innermost code.

```Phix
function game_combinations(sequence res, integer pool, needed, sequence chosen={})
    if needed=0 then
        res = append(res,chosen) -- collect the full sets
    else
        for i=iff(length(chosen)=0?1:chosen[$]+1) to pool do
            res = game_combinations(res,pool,needed-1,append(chosen,i))
        end for
    end if
    return res
end function

constant games = game_combinations({},4,2) -- ie {{1,2},{1,3},{1,4},{2,3},{2,4},{3,4}}

constant scores = {{3,0},{1,1},{0,3}}   -- ie win/draw/lose

sequence points = repeat(repeat(0,10),4) -- 1st..4th place, 0..9 points

procedure result_combinations(integer pool, needed, sequence chosen={})
    if needed=0 then
        -- (here, chosen is {1,1,1,1,1,1}..{3,3,3,3,3,3}, 729 in all)
        sequence results = repeat(0,4)
        for i=1 to length(chosen) do
            integer {team1,team2} = games[i],
                    {points1,points2} = scores[chosen[i]]
            results[team1] += points1
            results[team2] += points2
        end for
        results = sort(results)
        for i=1 to 4 do points[i][results[i]+1] += 1 end for
    else
        for i=1 to pool do
            result_combinations(pool,needed-1,append(chosen,i))
        end for
    end if
end procedure

-- accumulate the results of all possible outcomes (1..3) of 6 games:
result_combinations(3,6)    -- (the result ends up in points)
--result_combinations(length(scores),length(games)) -- (equivalent)

constant fmt = join(repeat("%5d",10))&"\n",
         cardinals = {"st","nd","rd","th"}
printf(1,"   points "&fmt&repeat('-',69)&"\n",tagset(9,0))
for i=1 to 4 do
    printf(1,"%d%s place "&fmt,{i,cardinals[i]}&points[5-i])
end for
```

```txt

   points     0     1     2     3     4     5     6     7     8     9
---------------------------------------------------------------------
1st place     0     0     0     1    14   148   152   306     0   108
2nd place     0     0     4    33   338   172   164    18     0     0
3rd place     0    18   136   273   290     4     8     0     0     0
4th place   108   306   184   125     6     0     0     0     0     0

```



## Python


```python
from itertools import product, combinations, izip

scoring = [0, 1, 3]
histo = [[0] * 10 for _ in xrange(4)]

for results in product(range(3), repeat=6):
    s = [0] * 4
    for r, g in izip(results, combinations(range(4), 2)):
        s[g[0]] += scoring[r]
        s[g[1]] += scoring[2 - r]

    for h, v in izip(histo, sorted(s)):
        h[v] += 1

for x in reversed(histo):
    print x
```

```txt
[0, 0, 0, 1, 14, 148, 152, 306, 0, 108]
[0, 0, 4, 33, 338, 172, 164, 18, 0, 0]
[0, 18, 136, 273, 290, 4, 8, 0, 0, 0]
[108, 306, 184, 125, 6, 0, 0, 0, 0, 0]
```



## Racket


```racket
#lang racket
;; Tim Brown 2014-09-15
(define (sort-standing stndg#)
  (sort (hash->list stndg#) > #:key cdr))

(define (hash-update^2 hsh key key2 updater2 dflt2)
  (hash-update hsh key (λ (hsh2) (hash-update hsh2 key2 updater2 dflt2)) hash))

(define all-standings
  (let ((G '((a b) (a c) (a d) (b c) (b d) (c d)))
        (R '((3 0) (1 1) (0 3))))
    (map
     sort-standing
     (for*/list ((r1 R) (r2 R) (r3 R) (r4 R) (r5 R) (r6 R))
       (foldr (λ (gm rslt h)
                (hash-update
                 (hash-update h (second gm) (λ (n) (+ n (second rslt))) 0)
                 (first gm) (curry + (first rslt)) 0))
              (hash) G (list r1 r2 r3 r4 r5 r6))))))

(define histogram
  (for*/fold ((rv (hash)))
    ((stndng (in-list all-standings)) (psn (in-range 0 4)))
    (hash-update^2 rv (add1 psn) (cdr (list-ref stndng psn)) add1 0)))

;; Generalised histogram printing functions...
(define (show-histogram hstgrm# captions)
  (define (min* a b)
    (if (and a b) (min a b) (or a b)))
  (define-values (position-mn position-mx points-mn points-mx)
    (for*/fold ((mn-psn #f) (mx-psn 0) (mn-pts #f) (mx-pts 0))
      (((psn rw) (in-hash hstgrm#)))
      (define-values (min-pts max-pts)
        (for*/fold ((mn mn-pts) (mx mx-pts)) ((pts (in-hash-keys rw)))
          (values (min* pts mn) (max pts mx))))
      (values (min* mn-psn psn) (max mx-psn psn) min-pts max-pts)))

  (define H
    (let ((lbls-row# (for/hash ((i (in-range points-mn (add1 points-mx)))) (values i i))))
      (hash-set hstgrm# 'thead lbls-row#)))

  (define cap-col-width (for/fold ((m 0)) ((v (in-hash-values captions))) (max m (string-length v))))

  (for ((plc (in-sequences
              (in-value 'thead)
              (in-range position-mn (add1 position-mx)))))
    (define cnts (for/list ((pts (in-range points-mn (add1 points-mx))))
                   (~a #:align 'center #:width 3 (hash-ref (hash-ref H plc) pts 0))))
    (printf "~a ~a~%"
            (~a (hash-ref captions plc (curry format "#~a:")) #:width cap-col-width)
            (string-join cnts "  "))))

(define captions
  (hash 'thead "POINTS:"
        1 "1st Place:"
        2 "2nd Place:"
        3 "Sack the manager:"
        4 "Sack the team!"))

(show-histogram histogram captions)
```


```txt
POINTS:            0    1    2    3    4    5    6    7    8    9
1st Place:         0    0    0    1   14   148  152  306   0   108
2nd Place:         0    0    4   33   338  172  164  18    0    0
Sack the manager:  0   18   136  273  290   4    8    0    0    0
Sack the team!    108  306  184  125   6    0    0    0    0    0
```



## REXX

===version 1, static game sets===
```rexx
/* REXX -------------------------------------------------------------------*/
results = '000000'                      /*start with left teams all losing */
games = '12 13 14 23 24 34'
points.=0
records.=0
Do Until nextResult(results)=0
  records.=0
  Do i=1 To 6
    r=substr(results,i,1)
    g=word(games,i); Parse Var g g1 +1 g2
    Select
      When r='2' Then                   /* win for left team               */
        records.g1=records.g1+3
      When r='1' Then Do                /* draw                            */
        records.g1=records.g1+1
        records.g2=records.g2+1
        End
      When r='0' Then                   /* win for right team              */
        records.g2=records.g2+3
      End
    End
  Call sort_records                     /* sort ascending,                 */
                                        /* first place team on the right   */
  r1=records.1
  r2=records.2
  r3=records.3
  r4=records.4
  points.0.r1=points.0.r1+1
  points.1.r2=points.1.r2+1
  points.2.r3=points.2.r3+1
  points.3.r4=points.3.r4+1
  End
ol.='['
sep=', '
Do i=0 To 9
  If i=9 Then sep=']'
  ol.0=ol.0||points.0.i||sep
  ol.1=ol.1||points.1.i||sep
  ol.2=ol.2||points.2.i||sep
  ol.3=ol.3||points.3.i||sep
  End
Say ol.3
Say ol.2
Say ol.1
Say ol.0
Exit

nextResult: Procedure Expose results
/* results is a string of 6 base 3 digits to which we add 1                */
/* e.g., '000212 +1 -> 000220                                              */
If results="222222" Then Return 0
res=0
do i=1 To 6
  res=res*3+substr(results,i,1)
  End
res=res+1
s=''
Do i=1 To 6
  b=res//3
  res=res%3
  s=b||s
  End
results=s
Return 1

sort_records: Procedure Expose records.
Do i=1 To 3
  Do j=i+1 To 4
    If records.j<records.i Then
      Parse Value records.i records.j With records.j records.i
    End
  End
Return
```

```txt
[0, 0, 0, 1, 14, 148, 152, 306, 0, 108]
[0, 0, 4, 33, 338, 172, 164, 18, 0, 0]
[0, 18, 136, 273, 290, 4, 8, 0, 0, 0]
[108, 306, 184, 125, 6, 0, 0, 0, 0, 0]
```


===version 2, generated game sets===
This REXX version allows the number of teams to be specified to be used in the calculations,   and

also generates the character string used for
the   ''list of games''   to be played   (game sets).

This REXX version can also simulate a Cricket World Cup   (by specifying   '''2'''   for the   '''win'''   variable).
<!-- ........................................................................................................
Programming notes:  these are some of the changes from REXX version 1:
:::*   the number of teams that are playing can be specified from the command line.
:::*   the number of game sets are automatically generated   (not static).
:::*   used logical (boolean) values instead of integers when appropriate.
:::*   elided the need for the   '''select'''   structure.
:::*   ordered the cases   (from '''select''')   in numerical order.
:::*   invoked the sort with a specific number of items to be sorted   (not hard-coded).
:::*   used exact comparisons   (instead of numerical comparisons).
:::*   used a consistent spelling of REXX keywords.
:::*   used idiomatic variable names instead of hardcoding the indices for arrays.
:::*   used lowercase spellings of REXX keywords for easier reading.
:::*   removed some deadcode   (code not needed   or   code not used).
:::*   elided unnecessary   '''do''' groups and/or loops.
:::*   aligned the numbers in the output   (used a consisted width/length).
:::*   added whitespace within assignments and other REXX clauses.
:::*   used a consistent indentation for   '''do''' groups and/or loops.
:::*   aligned statements within   '''do''' groups and/or loops.
:::*   didn't split   '''then'''   '''else'''   clauses.
:::*   used more compound statements   (so as to possibly have all the REXX code on one screen).
:::*   added   '''do─end'''   indices comments.
:::*   made more idiomatic by using the number of teams instead of hardcoding the #.
:::*   used a   '''do'''   loop instead of using discrete element numbers.
:::*   added more comments to explain what the statements are doing.
:::*   programmatically determined the length of the largest number that's displayed.
:::*   used   '''for'''   in   '''do'''   loops instead of '''to'''   (faster).
:::*   added a title and indices for the boxed output to indicate what is being displayed.
:::*   used an idiomatic value for the   ''number''   of columns for the "point" numbers.
:::*   used an idiomatic method to show the place winners.
:::*   added the capability to simulate a Cricket World Cup.
............................................................................................................. !-->

```rexx
/*REXX pgm calculates world cup standings based on the number of games won by the teams.*/
parse arg teams win .                            /*obtain optional argument from the CL.*/
if teams=='' | teams==","  then teams= 4         /*Not specified?  Then use the default.*/
if   win=='' |   win==","  then   win= 3         /* "      "         "   "   "     "    */
sets=0;                            gs=           /*the number of sets  (so far).        */
       do   j=1   for teams
         do k=j+1  to teams;    sets= sets+1     /*bump the number of game sets.        */
         games.sets= j || k;    gs= gs   j || k  /*generate the game combinations.      */
         end   /*j*/
       end     /*k*/
z= 1;                  setLimit= copies(2, sets) /*Z:   max length of any number shown. */
say teams  ' teams, '  sets   " game sets: "  gs /*display what's being used for calcs. */
results = copies(0, sets);                 say   /*start with left-most teams all losing*/
points. = 0                                      /*zero all the team's point.           */
            do until \nextResult(results);       @.= 0
              do j=1  for sets;                  r= substr( results, j, 1)
              parse var  games.j    A  +1  B     /*get the  A  and  B teams*/
              if r==0  then      @.B= @.B + win               /*win for right─most team.*/
              if r==1  then do;  @.A= @.A + 1;  @.B= @.B + 1; end  /*draw for both teams*/
              if r==2  then      @.A= @.A + win               /*win for left─most team. */
              end       /*j*/
            call sort teams
                        do  t=1  for teams;  tm= t - 1;   _= @.t
                        points.tm._ = points.tm._ + 1;    z= max(z, length( points.tm._) )
                        end   /*t*/
            end               /*until*/
$.=
     do j=0  for teams+6
        do k=0  for teams;      $.k= $.k || right( points.k.j, z)'│ ';         end  /*k*/
     end   /*j*/
say                                                  /* [↓]  build grid line for the box*/
L= length($.1) -2;     $$= translate( translate( left($.1, L), , 0123456789),   '─', " ")
say left('', 15)       center('points', L)           /*display the boxed title.         */
say left('', 15)  "╔"translate($$, '═╤', "─│")'╗'    /*display the bottom sep for title.*/
p= 0
     do m=teams-1  by -1  for teams;   p = p+1
                  say right('('th(p) "place)", 14)  " ║"left($.m, L)'║'
     if m>0  then say right('               ', 14)  " ╟"translate($$, '┼', "│")'╢'
     end   /*m*/
say left('', 15)  "╚"translate( $$, '═╧', "─│")'╝'   /*display the bottom sep for title.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nextResult: if results==setLimit  then return 0  /* [↓]  do arithmetic in base three.   */
            res= 0;      do k=1  for sets;      res= res * 3    +   substr( results, k, 1)
                         end   /*j*/
            results=;                           res= res + 1
                         do sets;      results= res // 3   ||   results;      res= res % 3
                         end   /*sets*/;                    return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
sort: procedure expose @.; arg #;  do   j=1   for #-1  /*a bubble sort, ascending order.*/
                                     do k=j+1  to #    /*swap two elements out of order.*/
                                     if @.k<@.j  then parse value  @.j @.k  with  @.k @.j
                                     end   /*k*/
                                   end     /*j*/;           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: arg th; return (th/1) || word('th st nd rd', 1 +(th//10) *(th//100%10\==1)*(th//10<4))
```

Programming note:   additional code was add to support a nice-looking grid for displaying the output.


```txt

4  teams,  6  game sets:   12 13 14 23 24 34


                                    points
               ╔═══╤════╤════╤════╤════╤════╤════╤════╤════╤════╗
  (1st place)  ║  0│   0│   0│   1│  14│ 148│ 152│ 306│   0│ 108║
               ╟───┼────┼────┼────┼────┼────┼────┼────┼────┼────╢
  (2nd place)  ║  0│   0│   4│  33│ 338│ 172│ 164│  18│   0│   0║
               ╟───┼────┼────┼────┼────┼────┼────┼────┼────┼────╢
  (3rd place)  ║  0│  18│ 136│ 273│ 290│   4│   8│   0│   0│   0║
               ╟───┼────┼────┼────┼────┼────┼────┼────┼────┼────╢
  (4th place)  ║108│ 306│ 184│ 125│   6│   0│   0│   0│   0│   0║
               ╚═══╧════╧════╧════╧════╧════╧════╧════╧════╧════╝

```

```txt

5  teams,  10  game sets:   12 13 14 15 23 24 25 34 35 45


                                                 points
               ╔═════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╤══════╗
  (1st place)  ║    0│     0│     0│     0│     1│    74│  2409│ 11520│ 16230│ 10860│ 14310║
               ╟─────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢
  (2nd place)  ║    0│     0│     0│     5│   181│  7314│ 15609│ 26400│  5610│  3660│   270║
               ╟─────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢
  (3rd place)  ║    0│     0│    30│   825│ 10401│ 25794│ 16119│  5790│    30│    60│     0║
               ╟─────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢
  (4th place)  ║    0│   270│  3990│ 13185│ 28871│ 10414│  2289│    30│     0│     0│     0║
               ╟─────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────┼──────╢
  (5th place)  ║ 3645│ 14310│ 17850│ 15145│  7931│   144│    24│     0│     0│     0│     0║
               ╚═════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╧══════╝

```

```txt

6  teams,  15  game sets:   12 13 14 15 16 23 24 25 26 34 35 36 45 46 56


                                                                 points
               ╔═══════╤════════╤════════╤════════╤════════╤════════╤════════╤════════╤════════╤════════╤════════╤════════╗
  (1st place)  ║      0│       0│       0│       0│       0│       1│     434│   68910│ 1049904│ 2333079│ 4056210│ 3149820║
               ╟───────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────╢
  (2nd place)  ║      0│       0│       0│       0│       6│    1165│  207308│ 1803570│ 5266944│ 3648879│ 2822850│  392580║
               ╟───────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────╢
  (3rd place)  ║      0│       0│       0│      45│    4926│  366445│ 2580578│ 6232110│ 3900744│ 1055889│  206550│     540║
               ╟───────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────╢
  (4th place)  ║      0│       0│     540│   34965│  623466│ 3793085│ 5521498│ 3916830│  410364│   47889│     270│       0║
               ╟───────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────╢
  (5th place)  ║      0│   10935│  261360│ 1395225│ 4446336│ 5645615│ 2210128│  378300│     864│     144│       0│       0║
               ╟───────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────┼────────╢
  (6th place)  ║ 354294│ 1760535│ 3281040│ 3884175│ 3782616│ 1176803│  108874│     570│       0│       0│       0│       0║
               ╚═══════╧════════╧════════╧════════╧════════╧════════╧════════╧════════╧════════╧════════╧════════╧════════╝

```

<!--       7 teams     with     21  game sets     is just a bridge too far.       !-->


## Ruby


```ruby
teams = [:a, :b, :c, :d]
matches = teams.combination(2).to_a
outcomes = [:win, :draw, :loss]
gains = {win:[3,0], draw:[1,1], loss:[0,3]}
places_histogram = Array.new(4) {Array.new(10,0)}

# The Array#repeated_permutation method generates the 3^6 different
# possible outcomes
outcomes.repeated_permutation(6).each do |outcome|
  results = Hash.new(0)

  # combine this outcomes with the matches, and generate the points table
  outcome.zip(matches).each do |decision, (team1, team2)|
    results[team1] += gains[decision][0]
    results[team2] += gains[decision][1]
  end

  # accumulate the results
  results.values.sort.reverse.each_with_index do |points, place|
    places_histogram[place][points] += 1
  end
end

fmt = "%s :" + "%4s"*10
puts fmt % [" ", *0..9]
puts fmt % ["-", *["---"]*10]
places_histogram.each.with_index(1) {|hist,place| puts fmt % [place, *hist]}
```

```txt
  :   0   1   2   3   4   5   6   7   8   9
- : --- --- --- --- --- --- --- --- --- ---
1 :   0   0   0   1  14 148 152 306   0 108
2 :   0   0   4  33 338 172 164  18   0   0
3 :   0  18 136 273 290   4   8   0   0   0
4 : 108 306 184 125   6   0   0   0   0   0
```


## Scala


```Scala
object GroupStage extends App { //team left digit vs team right digit
  val games = Array("12", "13", "14", "23", "24", "34")
  val points = Array.ofDim[Int](4, 10) //playing 3 games, points range from 0 to 9
  var results = "000000" //start with left teams all losing

  private def nextResult: Boolean = {
    if (results == "222222") false
    else {
      results = Integer.toString(Integer.parseInt(results, 3) + 1, 3)
      while (results.length < 6) results = "0" + results //left pad with 0s
      true
    }
  }

  do {
    val records = Array(0, 0, 0, 0)
    for (i <- results.indices.reverse by -1) {
      results(i) match {
        case '2' => records(games(i)(0) - '1') += 3
        case '1' => //draw
          records(games(i)(0) - '1') += 1
          records(games(i)(1) - '1') += 1
        case '0' => records(games(i)(1) - '1') += 3
      }
    }
    java.util.Arrays.sort(records) //sort ascending, first place team on the right

    points(0)(records(0)) += 1
    points(1)(records(1)) += 1
    points(2)(records(2)) += 1
    points(3)(records(3)) += 1
  } while (nextResult)

  println("First place: " + points(3).mkString("[",", ","]"))
  println("Second place: " + points(2).mkString("[",", ","]"))
  println("Third place: " + points(1).mkString("[",", ","]"))
  println("Fourth place: " + points(0).mkString("[",", ","]"))

}
```



## Tcl

```tcl
package require Tcl 8.6
proc groupStage {} {
    foreach n {0 1 2 3} {
	set points($n) {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0}
    }
    set results 0
    set games {0 1 0 2 0 3 1 2 1 3 2 3}
    while true {
	set R {0 0 1 0 2 0 3 0}
	foreach r [split [format %06d $results] ""] {A B} $games {
	    switch $r {
		2 {dict incr R $A 3}
		1 {dict incr R $A; dict incr R $B}
		0 {dict incr R $B 3}
	    }
	}
	foreach n {0 1 2 3} r [lsort -integer [dict values $R]] {
	    dict incr points($n) $r
	}

	if {$results eq "222222"} break
	while {[regexp {[^012]} [incr results]]} continue
    }
    return [lmap n {3 2 1 0} {dict values $points($n)}]
}

foreach nth {First Second Third Fourth} nums [groupStage] {
    puts "$nth place:\t[join [lmap n $nums {format %3s $n}] {, }]"
}
```

```txt

First place:	  0,   0,   0,   1,  14, 148, 152, 306,   0, 108
Second place:	  0,   0,   4,  33, 338, 172, 164,  18,   0,   0
Third place:	  0,  18, 136, 273, 290,   4,   8,   0,   0,   0
Fourth place:	108, 306, 184, 125,   6,   0,   0,   0,   0,   0

```



## Yabasic

```Yabasic
data "12", "13", "14", "23", "24", "34"

dim game$(6)

for i = 1 to 6 : read game$(i) : next

result$ = "000000"


sub ParseInt(number$, base)
    local x, i, pot, digits

    digits = len(number$)

    for i = digits to 1 step -1
        x = x + base^pot * dec(mid$(number$, i, 1))
        pot = pot + 1
    next

    return x
end sub


sub Format$(decimal, base)
    local cociente, i, j, conv$

    repeat
        cociente = int(decimal / base)
        conv$ = str$(mod(decimal, base)) + conv$
        decimal = cociente
        i = i + 1
    until(cociente = 0)

    return conv$
end sub


sub nextResult()
    if result$ = "222222" return false
    res = ParseInt(result$, 3)
    result$ = Format$(res+1, 3)
    while(len(result$) < 6) result$ = "0" + result$ wend
    return true
end sub


sub Sort(array())
    local n, i, t, sw

    n = arraysize(array(), 1)

    repeat
        sw = false
        for i = 0 to n - 1
            if array(i) > array(i + 1) then
                sw = true
                t = array(i)
                array(i) = array(i + 1)
                array(i + 1) = t
            end if
        next
    until(not sw)
end sub


dim points(4, 10)

sub compute()
    local records(4), i, t

    for i = 1 to arraysize(game$(), 1)
        switch mid$(result$, i, 1)
        case "2":
            t = val(mid$(game$(i), 1, 1))
            records(t) = records(t) + 3
            break
        case "1":
            t = val(mid$(game$(i), 1, 1))
            records(t) = records(t) + 1
            t = val(mid$(game$(i), 2, 1))
            records(t) = records(t) + 1
            break
        case "0":
            t = val(mid$(game$(i), 2, 1))
            records(t) = records(t) + 3
            break
        end switch
    next
    Sort(records())
    for i = 1 to 4
        points(i, records(i)) = points(i, records(i)) + 1
    next
    if not nextResult() return false
    return true
end sub

repeat until(not compute())

print "POINTS       0    1    2    3    4    5    6    7    8    9"
print "-------------------------------------------------------------"

dim place$(4)

data "1st", "2nd", "3rd", "4th"
for i = 1 to 4 : read place$(i) : next

for i = 1 to 4
    print place$(i), " place    ";
    for j = 0 to 9
        print points(5 - i, j) using "%-4.0f";
    next
    print
next
```



## zkl

```zkl
combos :=Utils.Helpers.pickNFrom(2,T(0,1,2,3)); // ( (0,1),(0,2) ... )
scoring:=T(0,1,3);
histo  :=(0).pump(4,List().write,(0).pump(10,List().write,0).copy); //[4][10] of zeros

foreach r0,r1,r2,r3,r4,r5 in ([0..2],[0..2],[0..2],[0..2],[0..2],[0..2]){
   s:=L(0,0,0,0);
   foreach i,r in (T(r0,r1,r2,r3,r4,r5).enumerate()){
      g:=combos[i];
      s[g[0]]+=scoring[r];
      s[g[1]]+=scoring[2 - r];
   }
   foreach h,v in (histo.zip(s.sort())){ h[v]+=1; }
}
foreach h in (histo.reverse()){ println(h.apply("%3d ".fmt).concat()) }
```

```txt

  0   0   0   1  14 148 152 306   0 108
  0   0   4  33 338 172 164  18   0   0
  0  18 136 273 290   4   8   0   0   0
108 306 184 125   6   0   0   0   0   0

```

