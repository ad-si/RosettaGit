+++
title = "Kosaraju"
description = ""
date = 2019-07-31T16:16:45Z
aliases = []
[extra]
id = 21302
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{wikipedia|Graph}}
[[Category:Algorithm]]


Kosaraju's algorithm (also known as the Kosaraju–Sharir algorithm) is a linear time algorithm to find the strongly connected components of a directed graph. Aho, Hopcroft and Ullman credit it to an unpublished paper from 1978 by S. Rao Kosaraju. The same algorithm was independently discovered by Micha Sharir and published by him in 1981. It makes use of the fact that the transpose graph (the same graph with the direction of every edge reversed) has exactly the same strongly connected components as the original graph.


;References:
* The article on [[wp:Kosaraju's_algorithm|Wikipedia]].


## C++

{{trans|D}}

```cpp
#include <functional>
#include <iostream>
#include <ostream>
#include <vector>

template<typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    auto it = v.cbegin();
    auto end = v.cend();

    os << "[";
    if (it != end) {
        os << *it;
        it = std::next(it);
    }
    while (it != end) {
        os << ", " << *it;
        it = std::next(it);
    }
    return os << "]";
}

std::vector<int> kosaraju(std::vector<std::vector<int>>& g) {
    // 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
    auto size = g.size();
    std::vector<bool> vis(size);           // all false by default
    std::vector<int> l(size);              // all zero by default
    auto x = size;                         // index for filling l in reverse order
    std::vector<std::vector<int>> t(size); // transpose graph

    // Recursive subroutine 'visit':
    std::function<void(int)> visit;
    visit = [&](int u) {
        if (!vis[u]) {
            vis[u] = true;
            for (auto v : g[u]) {
                visit(v);
                t[v].push_back(u); // construct transpose
            }
            l[--x] = u;
        }
    };

    // 2. For each vertex u of the graph do visit(u)
    for (int i = 0; i < g.size(); ++i) {
        visit(i);
    }
    std::vector<int> c(size); // used for component assignment

    // Recursive subroutine 'assign':
    std::function<void(int, int)> assign;
    assign = [&](int u, int root) {
        if (vis[u]) { // repurpose vis to mean 'unassigned'
            vis[u] = false;
            c[u] = root;
            for (auto v : t[u]) {
                assign(v, root);
            }
        }
    };

    // 3: For each element u of l in order, do assign(u, u)
    for (auto u : l) {
        assign(u, u);
    }

    return c;
}

std::vector<std::vector<int>> g = {
    {1},
    {2},
    {0},
    {1, 2, 4},
    {3, 5},
    {2, 6},
    {5},
    {4, 6, 7},
};

int main() {
    using namespace std;

    cout << kosaraju(g) << endl;

    return 0;
}
```

{{out}}

```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;

class Node
{
	public enum Colors
	{
		Black, White, Gray
	}

	public Colors color { get; set; }
	public int N { get; }

	public Node(int n)
	{
		N = n;
		color = Colors.White;
	}
}

class Graph
{
	public HashSet<Node> V { get; }
	public Dictionary<Node, HashSet<Node>> Adj { get; }

	/// <summary>
	/// Kosaraju's strongly connected components algorithm
	/// </summary>
	public void Kosaraju()
	{
		var L = new HashSet<Node>();

		Action<Node> Visit = null;
		Visit = (u) =>
		{
			if (u.color == Node.Colors.White)
			{
				u.color = Node.Colors.Gray;

				foreach (var v in Adj[u])
					Visit(v);

				L.Add(u);
			}
		};

		Action<Node, Node> Assign = null;
		Assign = (u, root) =>
		{
			if (u.color != Node.Colors.Black)
			{
				if (u == root)
					Console.Write("SCC: ");

				Console.Write(u.N + " ");
				u.color = Node.Colors.Black;

				foreach (var v in Adj[u])
					Assign(v, root);

				if (u == root)
					Console.WriteLine();
			}
		};

		foreach (var u in V)
			Visit(u);

		foreach (var u in L)
			Assign(u, u);
	}
}
```



## D

{{trans|Kotlin}} (mostly) with output like {{trans|Go}}

```D
import std.container.array;
import std.stdio;

/* the list index is the first vertex in the edge(s) */
auto g = [
    [1],
    [2],
    [0],
    [1, 2, 4],
    [3, 5],
    [2, 6],
    [5],
    [4, 6, 7],
];

int[] kosaraju(int[][] g) {
    // 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
    auto size = g.length; // all false by default
    Array!bool vis;
    vis.length = size;
    int[] l;              // all zero by default
    l.length = size;
    auto x = size;        // index for filling l in reverse order
    int[][] t;            // transpose graph
    t.length = size;

    // Recursive subroutine 'visit':
    void visit(int u) {
        if (!vis[u]) {
            vis[u] = true;
            foreach (v; g[u]) {
                visit(v);
                t[v] ~= u;  // construct transpose
            }
            l[--x] = u;
        }
     }

    // 2. For each vertex u of the graph do visit(u)
    foreach (u, _; g) {
        visit(u);
    }
    int[] c;  // used for component assignment
    c.length = size;

    // Recursive subroutine 'assign':
    void assign(int u, int root) {
        if (vis[u]) {  // repurpose vis to mean 'unassigned'
            vis[u] = false;
            c[u] = root;
            foreach(v; t[u]) {
                assign(v, root);
            }
        }
    }

    // 3: For each element u of l in order, do assign(u, u)
    foreach (u; l) {
        assign(u, u);
    }

    return c;
}

void main() {
    writeln(kosaraju(g));
}
```

{{out}}

```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```



## Go


```go
package main

import "fmt"

var g = [][]int{
    0: {1},
    1: {2},
    2: {0},
    3: {1, 2, 4},
    4: {3, 5},
    5: {2, 6},
    6: {5},
    7: {4, 6, 7},
}

func main() {
    fmt.Println(kosaraju(g))
}

func kosaraju(g [][]int) []int {
    // 1. For each vertex u of the graph, mark u as unvisited. Let L be empty.
    vis := make([]bool, len(g))
    L := make([]int, len(g))
    x := len(L)                // index for filling L in reverse order
    t := make([][]int, len(g)) // transpose graph
    // 2. recursive subroutine:
    var Visit func(int)
    Visit = func(u int) {
        if !vis[u] {
            vis[u] = true
            for _, v := range g[u] {
                Visit(v)
                t[v] = append(t[v], u) // construct transpose
            }
            x--
            L[x] = u
        }
    }
    // 2. For each vertex u of the graph do Visit(u)
    for u := range g {
        Visit(u)
    }
    c := make([]int, len(g)) // result, the component assignment
    // 3: recursive subroutine:
    var Assign func(int, int)
    Assign = func(u, root int) {
        if vis[u] { // repurpose vis to mean "unassigned"
            vis[u] = false
            c[u] = root
            for _, v := range t[u] {
                Assign(v, root)
            }
        }
    }
    // 3: For each element u of L in order, do Assign(u,u)
    for _, u := range L {
        Assign(u, u)
    }
    return c
}
```

{{out}}

```txt

[0 0 0 3 3 5 5 7]

```



## Java

{{trans|Kotlin}}
Output is like Go instead of what Kotlin outputs.

```java
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.IntConsumer;
import java.util.stream.Collectors;

public class Kosaraju {
    static class Recursive<I> {
        I func;
    }

    private static List<Integer> kosaraju(List<List<Integer>> g) {
        // 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
        int size = g.size();
        boolean[] vis = new boolean[size];
        int[] l = new int[size];
        AtomicInteger x = new AtomicInteger(size);

        List<List<Integer>> t = new ArrayList<>();
        for (int i = 0; i < size; ++i) {
            t.add(new ArrayList<>());
        }

        Recursive<IntConsumer> visit = new Recursive<>();
        visit.func = (int u) -> {
            if (!vis[u]) {
                vis[u] = true;
                for (Integer v : g.get(u)) {
                    visit.func.accept(v);
                    t.get(v).add(u);
                }
                int xval = x.decrementAndGet();
                l[xval] = u;
            }
        };

        // 2. For each vertex u of the graph do visit(u)
        for (int i = 0; i < size; ++i) {
            visit.func.accept(i);
        }
        int[] c = new int[size];

        Recursive<BiConsumer<Integer, Integer>> assign = new Recursive<>();
        assign.func = (Integer u, Integer root) -> {
            if (vis[u]) {  // repurpose vis to mean 'unassigned'
                vis[u] = false;
                c[u] = root;
                for (Integer v : t.get(u)) {
                    assign.func.accept(v, root);
                }
            }
        };

        // 3: For each element u of l in order, do assign(u, u)
        for (int u : l) {
            assign.func.accept(u, u);
        }

        return Arrays.stream(c).boxed().collect(Collectors.toList());
    }

    public static void main(String[] args) {
        List<List<Integer>> g = new ArrayList<>();
        for (int i = 0; i < 8; ++i) {
            g.add(new ArrayList<>());
        }
        g.get(0).add(1);
        g.get(1).add(2);
        g.get(2).add(0);
        g.get(3).add(1);
        g.get(3).add(2);
        g.get(3).add(4);
        g.get(4).add(3);
        g.get(4).add(5);
        g.get(5).add(2);
        g.get(5).add(6);
        g.get(6).add(5);
        g.get(7).add(4);
        g.get(7).add(6);
        g.get(7).add(7);

        List<Integer> output = kosaraju(g);
        System.out.println(output);
    }
}
```

{{out}}

```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```



## Julia

{{works with|Julia|0.6}}
{{trans|Go}}


```julia
function korasaju(g::Vector{Vector{T}}) where T<:Integer
    # 1. For each vertex u of the graph, mark u as unvisited. Let L be empty.
    vis = falses(length(g))
    L   = Vector{T}(length(g))
    x   = length(L) + 1
    t   = collect(T[] for _ in eachindex(g))

    # Recursive
    function visit(u::T)
        if !vis[u]
            vis[u] = true
            for v in g[u]
                visit(v)
                push!(t[v], u)
            end
            x -= 1
            L[x] = u
        end
    end
    # 2. For each vertex u of the graph do visit(u)
    for u in eachindex(g)
        visit(u)
    end
    c = Vector{T}(length(g))
    # 3. Recursive subroutine:
    function assign(u::T, root::T)
        if vis[u]
            vis[u] = false
            c[u] = root
            for v in t[u]
                assign(v, root)
            end
        end
    end
    # 3. For each element u of L in order, do assign(u, u)
    for u in L
        assign(u, u)
    end
    return c
end

g = [[2], [3], [1], [2, 3, 5], [4, 6], [3, 7], [6], [5, 7, 8]]
println(korasaju(g))
```


{{out}}

```txt
[1, 1, 1, 4, 4, 6, 6, 8]
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

/* the list index is the first vertex in the edge(s) */
val g = listOf(
    intArrayOf(1),        // 0
    intArrayOf(2),        // 1
    intArrayOf(0),        // 2
    intArrayOf(1, 2, 4),  // 3
    intArrayOf(3, 5),     // 4
    intArrayOf(2, 6),     // 5
    intArrayOf(5),        // 6
    intArrayOf(4, 6, 7)   // 7
)

fun kosaraju(g: List<IntArray>): List<List<Int>> {
    // 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
    val size = g.size
    val vis = BooleanArray(size)                 // all false by default
    val l = IntArray(size)                       // all zero by default
    var x = size                                 // index for filling l in reverse order
    val t = List(size) { mutableListOf<Int>() }  // transpose graph

    // Recursive subroutine 'visit':
    fun visit(u: Int) {
        if (!vis[u]) {
            vis[u] = true
            for (v in g[u]) {
                visit(v)
                t[v].add(u)  // construct transpose
            }
            l[--x] = u
        }
     }

    // 2. For each vertex u of the graph do visit(u)
    for (u in g.indices) visit(u)
    val c = IntArray(size)  // used for component assignment

    // Recursive subroutine 'assign':
    fun assign(u: Int, root: Int) {
        if (vis[u]) {  // repurpose vis to mean 'unassigned'
            vis[u] = false
            c[u] = root
            for (v in t[u]) assign(v, root)
        }
    }

    // 3: For each element u of l in order, do assign(u, u)
    for (u in l) assign(u, u)

    // Obtain list of SCC's from 'c' and return it
    return c.withIndex()
            .groupBy { it.value }.values
            .map { ivl -> ivl.map { it.index } }
}

fun main(args: Array<String>) {
    println(kosaraju(g).joinToString("\n"))
}
```


{{out}}

```txt

[0, 1, 2]
[3, 4]
[5, 6]
[7]

```



## Lua

{{trans|C++}}

```lua
function write_array(a)
    io.write("[")
    for i=0,#a do
        if i>0 then
            io.write(", ")
        end
        io.write(tostring(a[i]))
    end
    io.write("]")
end

function kosaraju(g)
    -- 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
    local size = #g

    local vis = {}
    for i=0,size do
        -- all false by default
        vis[i] = false
    end

    local l = {}
    for i=0,size do
        -- all zero by default
        l[i] = 0
    end

    local x = size+1  -- index for filling l in reverse order

    local t = {}    -- transpose graph

    -- Recursive subroutine 'visit'
    function visit(u)
        if not vis[u] then
            vis[u] = true
            for i=0,#g[u] do
                local v = g[u][i]
                visit(v)
                if t[v] then
                    local a = t[v]
                    a[#a+1] = u
                else
                    t[v] = {[0]=u}
                end
            end
            x = x - 1
            l[x] = u
        end
    end

    -- 2. For each vertex u of the graph do visit(u)
    for i=0,#g do
        visit(i)
    end
    local c = {}
    for i=0,size do
        -- used for component assignment
        c[i] = 0
    end

    -- Recursive subroutine 'assign'
    function assign(u, root)
        if vis[u] then  -- repurpose vis to mean 'unassigned'
            vis[u] = false
            c[u] = root
            for i=0,#t[u] do
                local v = t[u][i]
                assign(v, root)
            end
        end
    end

    -- 3: For each element u of l in order, do assign(u, u)
    for i=0,#l do
        local u = l[i]
        assign(u, u)
    end

    return c
end

-- main
local g = {
    [0]={[0]=1},
    [1]={[0]=2},
    [2]={[0]=0},
    [3]={[0]=1, [1]=2, [2]=4},
    [4]={[0]=3, [1]=5},
    [5]={[0]=2, [1]=6},
    [6]={[0]=5},
    [7]={[0]=4, [1]=6, [2]=7},
}

write_array(kosaraju(g))
print()
```

{{out}}

```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```




## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';

sub kosaraju {
    our(%k) = @_;
    our %g = ();
    our %h;
    my $i = 0;
    $g{$_}     = $i++ for sort keys %k;
    $h{$g{$_}} = $_   for      keys %g; # invert

    our(%visited, @stack, @transpose, @connected);
    sub visit {
        my($u) = @_;
        unless ($visited{$u}) {
            $visited{$u} = 1;
            for my $v (@{$k{$u}}) {
                visit($v);
                push @{$transpose[$g{$v}]}, $u;
            }
            push @stack, $u;
        }
    }

    sub assign {
        my($u, $root) = @_;
        if ($visited{$u}) {
            $visited{$u} = 0;
            $connected[$g{$u}] = $root;
            assign($_, $root) for @{$transpose[$g{$u}]};
        }
    }

    visit($_) for sort keys %g;
    assign($_, $_) for reverse @stack;

    my %groups;
    for my $i (0..$#connected) {
        my $id = $g{$connected[$i]};
        push @{$groups{$id}}, $h{$i};
    }
    say join ' ', @{$groups{$_}} for sort keys %groups;
}

my %test1 = (
    0 => [1],
    1 => [2],
    2 => [0],
    3 => [1, 2, 4],
    4 => [3, 5],
    5 => [2, 6],
    6 => [5],
    7 => [4, 6, 7]
);

my %test2 = (
   'Andy' => ['Bart'],
   'Bart' => ['Carl'],
   'Carl' => ['Andy'],
   'Dave' => [<Bart Carl Earl>],
   'Earl' => [<Dave Fred>],
   'Fred' => [<Carl Gary>],
   'Gary' => ['Fred'],
   'Hank' => [<Earl Gary Hank>]
);

kosaraju(%test1);
say '';
kosaraju(%test2);
```

{{out}}

```txt
0 1 2
3 4
5 6
7

Andy Bart Carl
Dave Earl
Fred Gary
Hank
```



## Perl 6

{{works with|Rakudo|2018.09}}
Inspired by Python & Kotlin entries.

Accepts a hash of lists/arrays holding the vertex (name => (neighbors)) pairs. No longer limited to continuous, positive, integer vertex names.


```perl6
sub kosaraju (%k) {
    my %g = %k.keys.sort Z=> flat ^%k;
    my %h = %g.invert;
    my %visited;
    my @stack;
    my @transpose;
    my @connected;

    sub visit ($u) {
        unless %visited{$u} {
            %visited{$u} = True;
            for |%k{$u} -> $v {
                visit($v);
                @transpose[%g{$v}].push: $u;
            }
            @stack.push: $u;
        }
    }

    sub assign ($u, $root) {
        if %visited{$u} {
            %visited{$u}   = False;
            @connected[%g{$u}] = $root;
            assign($_, $root) for |@transpose[%g{$u}];
        }
    }

    .&visit for %g.keys;
    assign($_, $_) for @stack.reverse;

    (|%g{@connected}).pairs.categorize( *.value, :as(*.key) ).values.map: { %h{|$_} };
}

# TESTING

-> $test { say "\nStrongly connected components: ", |kosaraju($test).sort } for

# Same test data as all other entries, converted to a hash of lists
(((1),(2),(0),(1,2,4),(3,5),(2,6),(5),(4,6,7)).pairs.hash),

# Same layout test data with named vertices instead of numbered.
(
 %(:Andy<Bart>,
   :Bart<Carl>,
   :Carl<Andy>,
   :Dave<Bart Carl Earl>,
   :Earl<Dave Fred>,
   :Fred<Carl Gary>,
   :Gary<Fred>,
   :Hank<Earl Gary Hank>)
)
```

{{out}}

```txt

Strongly connected components: (0 1 2)(3 4)(5 6)(7)

Strongly connected components: (Andy Bart Carl)(Dave Earl)(Fred Gary)(Hank)
```



## Phix


```Phix
sequence visited, l, t, c

procedure visit(sequence g, integer u)
    if not visited[u] then
        visited[u] = true
        for i=1 to length(g[u]) do
            integer v = g[u][i]
            visit(g,v)
            t[v] &= u
        end for
        l &= u
    end if
end procedure

procedure assign(integer u, root=u)
    if visited[u] then
        visited[u] = false
        c[u] = root
        for v=1 to length(t[u]) do
            assign(t[u][v], root)
        end for
    end if
end procedure

function korasaju(sequence g)
    integer len = length(g)
    visited = repeat(false,len)
    l = {}
    t = repeat({},len)
    for u=1 to len do
        visit(g,u)
    end for
    c = repeat(0,len)
    for u=length(l) to 1 by -1 do
        assign(l[u])
    end for
    return c
end function

constant g = {{2}, {3}, {1}, {2, 3, 5}, {4, 6}, {3, 7}, {6}, {5, 7, 8}}
?korasaju(g)
```

{{out}}

```txt

{1,1,1,4,4,6,6,8}

```



## Python


```python
def kosaraju(g):
    class nonlocal: pass

    # 1. For each vertex u of the graph, mark u as unvisited. Let l be empty.
    size = len(g)

    vis = [False]*size # vertexes that have been visited
    l = [0]*size
    nonlocal.x = size
    t = [[]]*size   # transpose graph

    def visit(u):
        if not vis[u]:
            vis[u] = True
            for v in g[u]:
                visit(v)
                t[v] = t[v] + [u]
            nonlocal.x = nonlocal.x - 1
            l[nonlocal.x] = u

    # 2. For each vertex u of the graph do visit(u)
    for u in range(len(g)):
        visit(u)
    c = [0]*size

    def assign(u, root):
        if vis[u]:
            vis[u] = False
            c[u] = root
            for v in t[u]:
                assign(v, root)

    # 3: For each element u of l in order, do assign(u, u)
    for u in l:
        assign(u, u)

    return c

g = [[1], [2], [0], [1,2,4], [3,5], [2,6], [5], [4,6,7]]
print kosaraju(g)
```

{{out}}

```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```



## Racket


```racket
#lang racket

(require racket/dict)

;; G is a dictionary of vertex -> (list vertex)
(define (Kosuraju G)
  (letrec
      ((vertices (remove-duplicates (append (dict-keys G) (append* (dict-values G)))))
       (visited?-dict (make-hash)) ; or any mutable dict type
       (assigned-dict (make-hash)) ; or any mutable dict type
       (neighbours:in (λ (u) (for/list (([v outs] (in-dict G)) #:when (member u outs)) v)))
       (visit! (λ (u L)
                 (cond [(dict-ref visited?-dict u #f) L]
                       [else (dict-set! visited?-dict u #t)
                             (cons u (for/fold ((L L)) ((v (in-list (dict-ref G u)))) (visit! v L)))])))
       (assign! (λ (u root)
                  (unless (dict-ref assigned-dict u #f)
                    (dict-set! assigned-dict u root)
                    (for ((v (in-list (neighbours:in u)))) (assign! v root)))))
       (L (for/fold ((l null)) ((u (in-dict-keys G))) (visit! u l))))

    (for ((u (in-list L))) (assign! u u))
    (map (curry map car) (group-by cdr (dict->list assigned-dict) =))))

(module+ test
  (Kosuraju '((0 1)
              (2 0)
              (5 2 6)
              (6 5)
              (1 2)
              (3 1 2 4) ; equvalent to (3 . (1 2 4))
              (4 5 3)
              (7 4 7 6))))
```


{{out}}


```txt
'((7) (6 5) (4 3) (2 1 0))
```



## Sidef

{{trans|Julia}}

```ruby
func korasaju(Array g) {
    # 1. For each vertex u of the graph, mark u as unvisited. Let L be empty.
    var vis = g.len.of(false)
    var L   = []
    var x   = g.end
    var t   = g.len.of { [] }

    # Recursive
    func visit(u) {
        if (!vis[u]) {
            vis[u] = true
            g[u].each {|v|
                visit(v)
                t[v] << u
            }
            L[x--] = u
        }
    }

    # 2. For each vertex u of the graph do visit(u)
    g.range.each {|u|
        visit(u)
    }

    var c = []

    # 3. Recursive subroutine:
    func assign(u, root) {
        if (vis[u]) {
            vis[u] = false
            c[u] = root
            t[u].each {|v|
                assign(v, root)
            }
        }
    }

    # 3. For each element u of L in order, do assign(u, u)
    L.each {|u|
        assign(u, u)
    }

    return c
}

var g = [[1], [2], [0], [1, 2, 4], [3, 5], [2, 6], [5], [4, 6, 7]]
say korasaju(g)
```

{{out}}

```txt

[0, 0, 0, 3, 3, 5, 5, 7]

```



## Swift


{{trans|D}}


```swift
func kosaraju(graph: [[Int]]) -> [Int] {
  let size = graph.count
  var x = size
  var vis = [Bool](repeating: false, count: size)
  var l = [Int](repeating: 0, count: size)
  var c = [Int](repeating: 0, count: size)
  var t = [[Int]](repeating: [], count: size)

  func visit(_ u: Int) {
    guard !vis[u] else {
      return
    }

    vis[u] = true

    for v in graph[u] {
      visit(v)
      t[v].append(u)
    }

    x -= 1
    l[x] = u
  }

  for u in 0..<graph.count {
    visit(u)
  }

  func assign(_ u: Int, root: Int) {
    guard vis[u] else {
      return
    }

    vis[u] = false
    c[u] = root

    for v in t[u] {
      assign(v, root: root)
    }
  }

  for u in l {
    assign(u, root: u)
  }

  return c
}

let graph = [
  [1],
  [2],
  [0],
  [1, 2, 4],
  [3, 5],
  [2, 6],
  [5],
  [4, 6, 7]
]

print(kosaraju(graph: graph))
```


{{out}}


```txt
[0, 0, 0, 3, 3, 5, 5, 7]
```



## zkl


```zkl
const VISITED=0,ASSIGNED=1;

fcn visit(u,G,L){	// u is ((visited,assigned), (id,edges))
   u0:=u[0];
   if(u0[VISITED]) return();
   u0[VISITED]=True;
   foreach idx in (u[1][1,*]){ visit(G[idx],G,L) } // vist out-neighbours
   L.insert(0,u);	// prepend u to L
}
fcn assign(u,root,G){  // u as above, root is a list of strong components
   u0:=u[0];
   if(u0[ASSIGNED]) return();
   root.append(u[1][0]);
   u0[ASSIGNED]=True;
   uid:=u[1][0];
   foreach v in (G){  // traverse graph to find in-neighbours, fugly
      n,ins := v[1][0],v[1][1,*];
      if(ins.holds(uid)) assign(G[n],root,G); // assign in-neighbour
   }
}
fcn kosaraju(graph){  // Use Tarjan's algorithm instead of this one
   // input: graph G = (V, Es)
   // output: set of strongly connected components (sets of vertices)

   // convert graph to ( (index,lowlink,onStack),(id,links)), ...)
   // sorted by id
   G:=List.createLong(graph.len(),0);
   foreach v in (graph){ G[v[0]]=T( List(False,False),v) }

   L:=List();
   foreach u in (G){ visit(u,G,L) }

   components:=List.createLong(graph.len(),List.copy,True);
   foreach u in (L){ assign(u,components[u[1][0]],G) }
   components=components.filter();

   println("List of strongly connected components:");
   foreach c in (components){ println(c.reverse().concat(",")) }

   return(components);
}
```


```zkl
   // graph from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
   // with vertices id zero based (vs 1 based in article)
   // ids start at zero and are consecutive (no holes), graph is unsorted
graph:=	  // ( (id, links/Edges), ...)
   T( T(0,1), T(2,0),     T(5,2,6), T(6,5),
      T(1,2), T(3,1,2,4), T(4,5,3), T(7,4,7,6) );
kosaraju(graph);
```

{{out}}

```txt

List of strongly connected components:
1,2,0
4,3
6,5
7

```

