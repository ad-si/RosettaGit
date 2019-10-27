+++
title = "Tarjan"
description = ""
date = 2019-08-30T14:18:21Z
aliases = []
[extra]
id = 21300
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{wikipedia|Graph}}
[[Category:Algorithm]]


Tarjan's algorithm is an algorithm in graph theory for finding the strongly connected components of a graph. It runs in linear time, matching the time bound for alternative methods including Kosaraju's algorithm and the path-based strong component algorithm. Tarjan's Algorithm is named for its discoverer, Robert Tarjan.


;References:
* The article on [[wp:Tarjan's_strongly_connected_components_algorithm|Wikipedia]].

=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;

class Node
{
	public int LowLink { get; set; }
	public int Index { get; set; }	
	public int N { get; }
	
	public Node(int n)
	{
		N = n;
		Index = -1;
		LowLink = 0;
	}
}

class Graph
{
	public HashSet<Node> V { get; }
	public Dictionary<Node, HashSet<Node>> Adj { get; }

	/// <summary>
	/// Tarjan's strongly connected components algorithm
	/// </summary>
	public void Tarjan()
	{
		var index = 0; // number of nodes
		var S = new Stack<Node>();

		Action<Node> StrongConnect = null;
		StrongConnect = (v) =>
		{
			// Set the depth index for v to the smallest unused index
			v.Index = index;
			v.LowLink = index;

			index++;
			S.Push(v);

			// Consider successors of v
			foreach (var w in Adj[v])
				if (w.Index < 0)
				{
					// Successor w has not yet been visited; recurse on it
					StrongConnect(w);
					v.LowLink = Math.Min(v.LowLink, w.LowLink);
				}
				else if (S.Contains(w))
					// Successor w is in stack S and hence in the current SCC
					v.LowLink = Math.Min(v.LowLink, w.Index);

			// If v is a root node, pop the stack and generate an SCC
			if (v.LowLink == v.Index)
			{
				Console.Write("SCC: ");

				Node w;
				do
				{
					w = S.Pop();
					Console.Write(w.N + " ");
				} while (w != v);

				Console.WriteLine();
			}
		};

		foreach (var v in V)
			if (v.Index < 0)
				StrongConnect(v);
	}	
}
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

// (same data as zkl example)
var g = [][]int{
    0: {1},
    2: {0},
    5: {2, 6},
    6: {5},
    1: {2},
    3: {1, 2, 4},
    4: {5, 3},
    7: {4, 7, 6},
}

func main() {
    tarjan(g, func(c []int) { fmt.Println(c) })
}

// the function calls the emit argument for each component identified.
// each component is a list of nodes.
func tarjan(g [][]int, emit func([]int)) {
    var indexed, stacked big.Int
    index := make([]int, len(g))
    lowlink := make([]int, len(g))
    x := 0
    var S []int
    var sc func(int) bool
    sc = func(n int) bool {
        index[n] = x
        indexed.SetBit(&indexed, n, 1)
        lowlink[n] = x
        x++
        S = append(S, n)
        stacked.SetBit(&stacked, n, 1)
        for _, nb := range g[n] {
            if indexed.Bit(nb) == 0 {
                if !sc(nb) {
                    return false
                }
                if lowlink[nb] < lowlink[n] {
                    lowlink[n] = lowlink[nb]
                }
            } else if stacked.Bit(nb) == 1 {
                if index[nb] < lowlink[n] {
                    lowlink[n] = index[nb]
                }
            }
        }
        if lowlink[n] == index[n] {
            var c []int
            for {
                last := len(S) - 1
                w := S[last]
                S = S[:last]
                stacked.SetBit(&stacked, w, 0)
                c = append(c, w)
                if w == n {
                    emit(c)
                    break
                }
            }
        }
        return true
    }
    for n := range g {
        if indexed.Bit(n) == 0 && !sc(n) {
            return
        }
    }
}
```

{{out}}

```txt

[2 1 0]
[6 5]
[4 3]
[7]

```




## Julia

LightGraphs uses Tarjan's algorithm by default. The package can also use Kosaraju's algorithm with the function  strongly_connected_components_kosaraju().

```julia
using LightGraphs

edge_list=[(1,2),(3,1),(6,3),(6,7),(7,6),(2,3),(4,2),(4,3),(4,5),(5,6),(5,4),(8,5),(8,8),(8,7)]

grph = SimpleDiGraph(Edge.(edge_list))

tarj = strongly_connected_components(grph)

inzerobase(arrarr) = map(x -> sort(x .- 1, rev=true), arrarr)

println("Results in the zero-base scheme: $(inzerobase(tarj))")

```
{{out}}

```txt

Results in the zero-base scheme: Array{Int64,1}[[2, 1, 0], [6, 5], [4, 3], [7]]

```



## Kotlin


```scala
// version 1.1.3

import java.util.Stack

typealias Nodes = List<Node>

class Node(val n: Int) {    
    var index   = -1  // -1 signifies undefined
    var lowLink = -1
    var onStack = false

    override fun toString()  = n.toString()
}

class DirectedGraph(val vs: Nodes, val es: Map<Node, Nodes>)

fun tarjan(g: DirectedGraph): List<Nodes> {
    val sccs = mutableListOf<Nodes>()
    var index = 0
    val s = Stack<Node>()
    
    fun strongConnect(v: Node) {       
        // Set the depth index for v to the smallest unused index
        v.index = index
        v.lowLink = index
        index++ 
        s.push(v)
        v.onStack = true 
      
        // consider successors of v
        for (w in g.es[v]!!) {
            if (w.index < 0) {
                // Successor w has not yet been visited; recurse on it
                strongConnect(w)
                v.lowLink = minOf(v.lowLink, w.lowLink)
            }
            else if (w.onStack) {
                // Successor w is in stack s and hence in the current SCC
                v.lowLink = minOf(v.lowLink, w.index)
            }
        }

        // If v is a root node, pop the stack and generate an SCC
        if (v.lowLink == v.index) {
            val scc = mutableListOf<Node>()
            do {
                val w = s.pop()
                w.onStack = false
                scc.add(w)
            } 
            while (w != v)
            sccs.add(scc)
        }
    }

    for (v in g.vs) if (v.index < 0) strongConnect(v)
    return sccs
} 

fun main(args: Array<String>) {
    val vs = (0..7).map { Node(it) }   
    val es = mapOf(
        vs[0] to listOf(vs[1]),
        vs[2] to listOf(vs[0]),
        vs[5] to listOf(vs[2], vs[6]),
        vs[6] to listOf(vs[5]),
        vs[1] to listOf(vs[2]),
        vs[3] to listOf(vs[1], vs[2], vs[4]),
        vs[4] to listOf(vs[5], vs[3]),
        vs[7] to listOf(vs[4], vs[7], vs[6])
    )
    val g = DirectedGraph(vs, es)
    val sccs = tarjan(g)
    println(sccs.joinToString("\n"))   
}
```


{{out}}

```txt

[2, 1, 0]
[6, 5]
[4, 3]
[7]

```



## Perl

{{trans|Perl 6}}

```perl
use feature 'state';
use List::Util qw(min);

sub tarjan {
    our(%k) = @_;
    our(%onstack, %index, %lowlink, @stack);
    our @connected = ();

    sub strong_connect {
        my($vertex) = @_;
         state $index = 0;
         $index{$vertex}   = $index;
         $lowlink{$vertex} = $index++;
         push @stack, $vertex;
         $onstack{$vertex} = 1;
         for my $connection (@{$k{$vertex}}) {
             if (not $index{$connection}) {
                 strong_connect($connection);
                 $lowlink{$vertex} = min($lowlink{$connection},$lowlink{$vertex});
             } elsif ($onstack{$connection}) {
                 $lowlink{$vertex} = min($lowlink{$connection},$lowlink{$vertex});
             }
        }
        if ($lowlink{$vertex} eq $index{$vertex}) {
            my @node;
            do {
                push @node, pop @stack;
                $onstack{$node[-1]} = 0;
            } while $node[-1] ne $vertex;
            push @connected, [@node];
        }
    }

    for (sort keys %k) {
        strong_connect($_) unless $index{$_}
    }
    @connected
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
  'Dave' => [qw<Bart Carl Earl>],
  'Earl' => [qw<Dave Fred>],
  'Fred' => [qw<Carl Gary>],
  'Gary' => ['Fred'],
  'Hank' => [qw<Earl Gary Hank>]
);

print "Strongly connected components:\n";
print join(', ', sort @$_) . "\n" for tarjan(%test1);
print "\nStrongly connected components:\n";
print join(', ', sort @$_) . "\n" for tarjan(%test2);
```

{{out}}

```txt
Strongly connected components:
0, 1, 2
5, 6
3, 4
7

Strongly connected components:
Andy, Bart, Carl
Fred, Gary
Dave, Earl
Hank
```



## Perl 6

{{works with|Rakudo|2018.09}}


```perl6
sub tarjan (%k) {
    my %onstack;
    my %index;
    my %lowlink;
    my @stack;
    my @connected;

    sub strong-connect ($vertex) {
         state $index      = 0;
         %index{$vertex}   = $index;
         %lowlink{$vertex} = $index++;
         %onstack{$vertex} = True;
         @stack.push: $vertex;
         for |%k{$vertex} -> $connection {
             if not %index{$connection}.defined {
                 strong-connect($connection);
                 %lowlink{$vertex} min= %lowlink{$connection};
             }
             elsif %onstack{$connection} {
                 %lowlink{$vertex} min= %index{$connection};
             }
        }
        if %lowlink{$vertex} eq %index{$vertex} {
            my @node;
            repeat {
                @node.push: @stack.pop;
                %onstack{@node.tail} = False;
            } while @node.tail ne $vertex;
            @connected.push: @node;
        }
    }

    .&strong-connect unless %index{$_} for %k.keys;

    @connected
}

# TESTING

-> $test { say "\nStrongly connected components: ", |tarjan($test).sort».sort } for

# hash of vertex, edge list pairs
(((1),(2),(0),(1,2,4),(3,5),(2,6),(5),(4,6,7)).pairs.hash),

# Same layout test data with named vertices instead of numbered.
%(:Andy<Bart>,
  :Bart<Carl>,
  :Carl<Andy>,
  :Dave<Bart Carl Earl>,
  :Earl<Dave Fred>,
  :Fred<Carl Gary>,
  :Gary<Fred>,
  :Hank<Earl Gary Hank>
)
```

{{out}}

```txt

Strongly connected components: (0 1 2)(3 4)(5 6)(7)

Strongly connected components: (Andy Bart Carl)(Dave Earl)(Fred Gary)(Hank)
```



## Phix

{{trans|Go}}
Same data as other examples, but with 1-based indexes.

```Phix
constant g = {{2}, {3}, {1}, {2,3,5}, {6,4}, {3,7}, {6}, {5,8,7}}

sequence index, lowlink, stacked, stack
integer x

function strong_connect(integer n, r_emit)
    index[n] = x
    lowlink[n] = x
    stacked[n] = 1
    stack &= n
    x += 1
    for b=1 to length(g[n]) do
        integer nb = g[n][b]
        if index[nb] == 0 then
            if not strong_connect(nb,r_emit) then
                return false
            end if
            if lowlink[nb] < lowlink[n] then
                lowlink[n] = lowlink[nb]
            end if
        elsif stacked[nb] == 1 then
            if index[nb] < lowlink[n] then
                lowlink[n] = index[nb]
            end if
        end if
    end for
    if lowlink[n] == index[n] then
        sequence c = {}
        while true do
            integer w := stack[$]
            stack = stack[1..$-1]
            stacked[w] = 0
            c = prepend(c, w)
            if w == n then
                call_proc(r_emit,{c})
                exit
            end if
        end while
    end if
    return true
end function

procedure tarjan(sequence g, integer r_emit)
    index   = repeat(0,length(g))
    lowlink = repeat(0,length(g))
    stacked = repeat(0,length(g))
    stack = {}
    x := 1
    for n=1 to length(g) do
        if index[n] == 0
        and not strong_connect(n,r_emit) then
            return
        end if
    end for
end procedure

procedure emit(object c)
-- called for each component identified.
-- each component is a list of nodes.
    ?c
end procedure

tarjan(g,routine_id("emit"))
```

{{out}}

```txt

{1,2,3}
{6,7}
{4,5}
{8}

```



## Racket



###  Manual implementation 


{{trans|Kotlin}}


```racket
#lang racket

(require syntax/parse/define
         fancy-app
         (for-syntax racket/syntax))

(struct node (name index low-link on?) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc v port mode) (fprintf port "~a" (node-name v)))])

(define-syntax-parser change!
  [(_ x:id f) #'(set! x (f x))]
  [(_ accessor:id v f)
   #:with mutator! (format-id this-syntax "set-~a!" #'accessor)
   #'(mutator! v (f (accessor v)))])

(define (tarjan g)
  (define sccs '())
  (define index 0)
  (define s '())

  (define (dfs v)
    (set-node-index! v index)
    (set-node-low-link! v index)
    (set-node-on?! v #t)
    (change! s (cons v _))
    (change! index add1)

    (for ([w (in-list (hash-ref g v '()))])
      (match-define (node _ index low-link on?) w)
      (cond
        [(not index) (dfs w)
                     (change! node-low-link v (min (node-low-link w) _))]
        [on? (change! node-low-link v (min index _))]))

    (when (= (node-low-link v) (node-index v))
      (define-values (scc* s*) (splitf-at s (λ (w) (not (eq? w v)))))
      (set! s (rest s*))
      (define scc (cons (first s*) scc*))
      (for ([w (in-list scc)]) (set-node-on?! w #f))
      (change! sccs (cons scc _))))

  (for* ([(u _) (in-hash g)] #:when (not (node-index u))) (dfs u))
  sccs)

(define (make-graph xs)
  (define store (make-hash))
  (define (make-node v) (hash-ref! store v (thunk (node v #f #f #f))))
  
  ;; it's important that we use hasheq instead of hash so that we compare
  ;; reference instead of actual value. Had we use the actual value,
  ;; the key would be a mutable value, which causes undefined behavior
  (for/hasheq ([vs (in-list xs)]) (values (make-node (first vs)) (map make-node (rest vs)))))

(tarjan (make-graph '([0 1]
                      [2 0]
                      [5 2 6]
                      [6 5]
                      [1 2]
                      [3 1 2 4]
                      [4 5 3]
                      [7 4 7 6])))
```


{{out}}


```txt

'((7) (3 4) (5 6) (2 1 0))

```



###  With the graph library 



```racket
#lang racket

(require graph)

(define g (unweighted-graph/adj '([0 1]
                                  [2 0]
                                  [5 2 6]
                                  [6 5]
                                  [1 2]
                                  [3 1 2 4]
                                  [4 5 3]
                                  [7 4 7 6])))

(scc g)
```


{{out}}

```txt

'((7) (3 4) (5 6) (1 0 2))

```



## Sidef

{{trans|Perl 6}}

```ruby
func tarjan (k) {

    var(:onstack, :index, :lowlink, *stack, *connected)

    func strong_connect (vertex, i=0) {

         index{vertex}   = i
         lowlink{vertex} = i+1
         onstack{vertex} = true
         stack << vertex

         for connection in (k{vertex}) {
             if (index{connection} == nil) {
                 strong_connect(connection, i+1)
                 lowlink{vertex} `min!` lowlink{connection}
             }
             elsif (onstack{connection}) {
                 lowlink{vertex} `min!` index{connection}
             }
        }

        if (lowlink{vertex} == index{vertex}) {
            var *node
            do {
                node << stack.pop
                onstack{node.tail} = false
            } while (node.tail != vertex)
            connected << node
        }
    }

    { strong_connect(_) if !index{_} } << k.keys

    return connected
}

var tests = [
    Hash(
         0 => <1>,
         1 => <2>,
         2 => <0>,
         3 => <1 2 4>,
         4 => <3 5>,
         5 => <2 6>,
         6 => <5>,
         7 => <4 6 7>,
    ),
    Hash(
        :Andy => <Bart>,
        :Bart => <Carl>,
        :Carl => <Andy>,
        :Dave => <Bart Carl Earl>,
        :Earl => <Dave Fred>,
        :Fred => <Carl Gary>,
        :Gary => <Fred>,
        :Hank => <Earl Gary Hank>,
    )
]

tests.each {|t|
    say ("Strongly connected components: ", tarjan(t).map{.sort}.sort)
}
```

{{out}}

```txt

Strongly connected components: [["0", "1", "2"], ["3", "4"], ["5", "6"], ["7"]]
Strongly connected components: [["Andy", "Bart", "Carl"], ["Dave", "Earl"], ["Fred", "Gary"], ["Hank"]]

```



## zkl


```zkl
class Tarjan{
   // input: graph G = (V, Es)
   // output: set of strongly connected components (sets of vertices)
   // Ick: class holds global state for strongConnect(), otherwise inert
   const INDEX=0, LOW_LINK=1, ON_STACK=2;
   fcn init(graph){
      var index=0, stack=List(), components=List(), 
          G=List.createLong(graph.len(),0);

      // convert graph to ( (index,lowlink,onStack),(id,links)), ...)
      // sorted by id
      foreach v in (graph){ G[v[0]]=T( L(Void,Void,False),v) }

      foreach v in (G){ if(v[0][INDEX]==Void) strongConnect(v) }

      println("List of strongly connected components:");
      foreach c in (components){ println(c.reverse().concat(",")) }

      returnClass(components);	// over-ride return of class instance
   }
   fcn strongConnect(v){  // v is ( (index,lowlink,onStack), (id,links) )
      // Set the depth index for v to the smallest unused index
      v0:=v[0]; v0[INDEX]=v0[LOW_LINK]=index;
      index+=1;
      v0[ON_STACK]=True;
      stack.push(v);

       // Consider successors of v
      foreach idx in (v[1][1,*]){  // links of v to other vs
         w,w0 := G[idx],w[0];	// well, that is pretty vile
	 if(w[0][INDEX]==Void){
	    strongConnect(w); // Successor w not yet visited; recurse on it
	    v0[LOW_LINK]=v0[LOW_LINK].min(w0[LOW_LINK]);
	 }
	 else if(w0[ON_STACK])
	    // Successor w is in stack S and hence in the current SCC
	    v0[LOW_LINK]=v0[LOW_LINK].min(w0[INDEX]);
      }
      // If v is a root node, pop the stack and generate an SCC
      if(v0[LOW_LINK]==v0[INDEX]){
         strong:=List();  // start a new strongly connected component
	 do{
	    w,w0 := stack.pop(), w[0];
	    w0[ON_STACK]=False;
	    strong.append(w[1][0]); // add w to strongly connected component
	 }while(w.id!=v.id);
	 components.append(strong); // output strongly connected component
      }
   }
}
```


```zkl
   // graph from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
   // with vertices id zero based (vs 1 based in article)
   // ids start at zero and are consecutive (no holes), graph is unsorted
graph:=	  // ( (id, links/Edges), ...)
   T( T(0,1), T(2,0),     T(5,2,6), T(6,5),
      T(1,2), T(3,1,2,4), T(4,5,3), T(7,4,7,6) );
Tarjan(graph);
```

{{out}}

```txt

0,1,2
5,6
3,4
7

```

