+++
title = "Topological sort/Extracted top item"
description = ""
date = 2019-01-17T17:12:42Z
aliases = []
[extra]
id = 8426
[taxonomies]
categories = ["task"]
tags = []
+++

Given a mapping between items, and items they depend on, a [[wp:Topological sorting|topological sort]] orders items so that no item precedes an item it depends upon. 

The compiling of a design in the [[wp:VHDL|VHDL]] language has the constraint that a file must be compiled after any file containing definitions it depends on. A tool exists that extracts file dependencies. 

* Assume the file names are single words, given without their file extensions. 
* Files mentioned as only dependants, have no dependants of their own, but their order of compiling must be given.
* Any self dependencies should be ignored. 


A top level file is defined as a file that:
# Has dependents.
# Is not itself the dependent of another file 


'''Task Description'''

Given the following file dependencies as an example:

```txt

FILE    FILE DEPENDENCIES

### =    ==============

top1    des1 ip1 ip2
top2    des1 ip2 ip3
ip1     extra1 ip1a ipcommon
ip2     ip2a ip2b ip2c ipcommon
des1    des1a des1b des1c
des1a   des1a1 des1a2
des1c   des1c1 extra1
```


The task is to create a program that given a graph of the dependency:
# Determines the top levels from the dependencies and show them.
# Extracts a compile order of files to compile any given (usually top level) file.
# Give a compile order for file top1.
# Give a compile order for file top2.

You may show how to compile multiple top levels as a stretch goal

<small>Note: this task differs from task [[Topological sort]] in that the order for compiling any file might not include all files; and that checks for dependency cycles are not mandated.</small>



## Related tasks

* [[Topological sort]]





## C

Take code from [[Topological sort#c]] and add/change the following:

```c
char input[] =	"top1    des1 ip1 ip2\n"
		"top2    des1 ip2 ip3\n"
		"ip1     extra1 ip1a ipcommon\n"
		"ip2     ip2a ip2b ip2c ipcommon\n"
		"des1    des1a des1b des1c\n"
		"des1a   des1a1 des1a2\n"
		"des1c   des1c1 extra1\n";

...
int find_name(item base, int len, const char *name)
{
	int i;
	for (i = 0; i < len; i++)
		if (!strcmp(base[i].name, name)) return i;
	return -1;
}

int depends_on(item base, int n1, int n2)
{
	int i;
	if (n1 == n2) return 1;
	for (i = 0; i < base[n1].n_deps; i++)
		if (depends_on(base, base[n1].deps[i], n2)) return 1;
	return 0;
}

void compile_order(item base, int n_items, int *top, int n_top)
{
	int i, j, lvl;
	int d = 0;
	printf("Compile order for:");
	for (i = 0; i < n_top; i++) {
		printf(" %s", base[top[i]].name);
		if (base[top[i]].depth > d)
			d = base[top[i]].depth;
	}
	printf("\n");

	for (lvl = 1; lvl <= d; lvl ++) {
		printf("level %d:", lvl);
		for (i = 0; i < n_items; i++) {
			if (base[i].depth != lvl) continue;
			for (j = 0; j < n_top; j++) {
				if (depends_on(base, top[j], i)) {
					printf(" %s", base[i].name);
					break;
				}
			}
		}
		printf("\n");
	}
	printf("\n");
}

int main()
{
	int i, n, bad = -1;
	item items;
	n = parse_input(&items);
 
	for (i = 0; i < n; i++)
		if (!items[i].depth && get_depth(items, i, bad) < 0) bad--;
 
	int top[3];
	top[0] = find_name(items, n, "top1");
	top[1] = find_name(items, n, "top2");
	top[2] = find_name(items, n, "ip1");

	compile_order(items, n, top, 1);
	compile_order(items, n, top + 1, 1);
	compile_order(items, n, top, 2);
	compile_order(items, n, top + 2, 1);

	return 0;
}
```
output (the last item is just to show that it doesn't have to be top level)<lang>Compile order for: top1
level 1: extra1 ip1a ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
level 2: ip1 ip2 des1a des1c
level 3: des1
level 4: top1

Compile order for: top2
level 1: ip3 extra1 ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
level 2: ip2 des1a des1c
level 3: des1
level 4: top2

Compile order for: top1 top2
level 1: ip3 extra1 ip1a ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
level 2: ip1 ip2 des1a des1c
level 3: des1
level 4: top1 top2

Compile order for: ip1
level 1: extra1 ip1a ipcommon
level 2: ip1
```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

var data = `
FILE    FILE DEPENDENCIES

### =    ==============

top1    des1 ip1 ip2
top2    des1 ip2 ip3
ip1     extra1 ip1a ipcommon
ip2     ip2a ip2b ip2c ipcommon
des1    des1a des1b des1c
des1a   des1a1 des1a2
des1c   des1c1 extra1`

func main() {
    g, dep, err := parseLibDep(data)
    if err != nil {
        fmt.Println(err)
        return
    }
    // Task 1: Determine top levels.  The input parser returns a list (dep)
    // of libraries that are dependants of at least one other library.
    // Top levels are then libraries in the graph that are not on this list.
    var tops []string
    for n := range g {
        if !dep[n] {
            tops = append(tops, n)
        }
    }
    fmt.Println("Top levels:", tops)
    // Task 2 is orderFrom method, below
    showOrder(g, "top1")         // Task 3
    showOrder(g, "top2")         // Task 4
    showOrder(g, "top1", "top2") // Stretch

    fmt.Println("Cycle examples:")
    // reparse with a cyclic dependency
    g, _, err = parseLibDep(data + `
des1a1  des1`)
    if err != nil {
        fmt.Println(err)
        return
    }
    showOrder(g, "top1")       // runs into cycle
    showOrder(g, "ip1", "ip2") // does not involve cycle
}

func showOrder(g graph, target ...string) {
    order, cyclic := g.orderFrom(target...)
    if cyclic == nil {
        reverse(order) // compile order is reverse of dependency order
        fmt.Println("Target", target, "order:", order)
    } else {
        fmt.Println("Target", target, "cyclic dependencies:", cyclic)
    }
}

func reverse(s []string) {
    last := len(s) - 1
    for i, e := range s[:len(s)/2] {
        s[i], s[last-i] = s[last-i], e
    }
}

type graph map[string][]string // adjacency list representation
type depList map[string]bool

// parseLibDep parses the text format of the task and returns a dependency
// graph and a list of nodes that are dependants of at least one other node.
func parseLibDep(data string) (g graph, d depList, err error) {
    lines := strings.Split(data, "\n")
    if len(lines) < 3 || !strings.HasPrefix(lines[2], "=") {
        return nil, nil, fmt.Errorf("data format")
    }
    lines = lines[3:]
    g = graph{}
    d = depList{}
    for _, line := range lines {
        libs := strings.Fields(line)
        if len(libs) == 0 {
            continue
        }
        lib := libs[0]
        var deps []string
        for _, dep := range libs[1:] {
            g[dep] = g[dep]
            if dep == lib {
                continue
            }
            for i := 0; ; i++ {
                if i == len(deps) {
                    deps = append(deps, dep)
                    d[dep] = true
                    break
                }
                if dep == deps[i] {
                    break
                }
            }
        }
        g[lib] = deps
    }
    return g, d, nil
}

// OrderFrom produces a topological ordering of the subgraph of g reachable
// from a set of start nodes, where the subgraph is a directed acyclic graph.
// If the subgraph contains a cycle, orderFrom returns the first cycle found
// and returns a nil order.  Cycles which are in the graph but not in the
// subgraph reachable from start are not detected.
func (g graph) orderFrom(start ...string) (order, cyclic []string) {
    L := make([]string, len(g))
    i := len(L)
    temp := map[string]bool{}
    perm := map[string]bool{}
    var cycleFound bool
    var cycleStart string
    var visit func(string)
    visit = func(n string) {
        switch {
        case temp[n]:
            cycleFound = true
            cycleStart = n
            return
        case perm[n]:
            return
        }
        temp[n] = true
        for _, m := range g[n] {
            visit(m)
            if cycleFound {
                if cycleStart > "" {
                    cyclic = append(cyclic, n)
                    if n == cycleStart {
                        cycleStart = ""
                    }
                }
                return
            }
        }
        delete(temp, n)
        perm[n] = true
        i--
        L[i] = n
    }
    for _, n := range start {
        if perm[n] {
            continue
        }
        visit(n)
        if cycleFound {
            return nil, cyclic
        }
    }
    return L[i:], nil
}
```

```txt

Top levels: [top1 top2]
Target [top1] order: [des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip1a ipcommon ip1 ip2a ip2b ip2c ip2 top1]
Target [top2] order: [des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip2a ip2b ip2c ipcommon ip2 ip3 top2]
Target [top1 top2] order: [des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip1a ipcommon ip1 ip2a ip2b ip2c ip2 top1 ip3 top2]
Cycle examples:
Target [top1] cyclic dependencies: [des1a1 des1a des1]
Target [ip1 ip2] order: [extra1 ip1a ipcommon ip1 ip2a ip2b ip2c ip2]

```



## J


Derived from the [[Topological sort#J|topological sort]] implementation:


```j
compileOrder=: dyad define
  targets=. ;: x
  parsed=. <@;:;._2 y
  names=. ~.({.&>parsed),targets,;parsed
  depends=. (> =@i.@#) names e.S:1 (#names){.parsed
  depends=. (+. +./ .*.~)^:_ depends
  b=. +./depends (] , #~) names e. targets
  names (</.~ \: ~.@])&(keep&#) +/"1 depends
  (b#names) (</.~ /: ~.@]) +/ }.+./ .*.~&(b#"1 b#depends)^:a: 1
)

topLevel=:  [: ({.&> -. [:;}.&.>) <@;:;._2

```


The changes include:

#  Added an argument for the target(s) we wish to find dependencies for
#  Make sure that these targets are included in our dependency structures
#  Make sure that things we can depend on are included in our dependency structures
#  Select these targets, and the things they depend on, once we know what depends on what
#  When ordering names by dependencies:
##  only consider names and dependencies we want to keep
##  extract names grouped by their dependency chain length

Example:


```j
dependencies=: noun define
  top1    des1 ip1 ip2
  top2    des1 ip2 ip3
  ip1     extra1 ip1a ipcommon
  ip2     ip2a ip2b ip2c ipcommon
  des1    des1a des1b des1c
  des1a   des1a1 des1a2
  des1c   des1c1 extra1
)

   >topLevel dependencies
top1
top2

   ;:inv@> 'top1' compileOrder dependencies
extra1 ip1a ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
ip1 ip2 des1a des1c                                           
des1                                                          
top1                                                          

   ;:inv@> 'top2' compileOrder dependencies
ip3 extra1 ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
ip2 des1a des1c                                              
des1                                                         
top2                                                         

   ;:inv@> 'top1 top2' compileOrder dependencies
ip3 extra1 ip1a ipcommon ip2a ip2b ip2c des1b des1a1 des1a2 des1c1
ip1 ip2 des1a des1c                                               
des1                                                              
top1 top2                                                         

```



## Java

```java
import java.util.*;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

public class TopologicalSort2 {

    public static void main(String[] args) {
        String s = "top1,top2,ip1,ip2,ip3,ip1a,ip2a,ip2b,ip2c,ipcommon,des1,"
                + "des1a,des1b,des1c,des1a1,des1a2,des1c1,extra1";

        Graph g = new Graph(s, new int[][]{
            {0, 10}, {0, 2}, {0, 3},
            {1, 10}, {1, 3}, {1, 4},
            {2, 17}, {2, 5}, {2, 9},
            {3, 6}, {3, 7}, {3, 8}, {3, 9},
            {10, 11}, {10, 12}, {10, 13},
            {11, 14}, {11, 15},
            {13, 16}, {13, 17},});

        System.out.println("Top levels: " + g.toplevels());
        String[] files = {"top1", "top2", "ip1"};
        for (String f : files)
            System.out.printf("Compile order for %s %s%n", f, g.compileOrder(f));
    }
}

class Graph {
    List<String> vertices;
    boolean[][] adjacency;
    int numVertices;

    public Graph(String s, int[][] edges) {
        vertices = asList(s.split(","));
        numVertices = vertices.size();
        adjacency = new boolean[numVertices][numVertices];

        for (int[] edge : edges)
            adjacency[edge[0]][edge[1]] = true;
    }

    List<String> toplevels() {
        List<String> result = new ArrayList<>();
        // look for empty columns
        outer:
        for (int c = 0; c < numVertices; c++) {
            for (int r = 0; r < numVertices; r++) {
                if (adjacency[r][c])
                    continue outer;
            }
            result.add(vertices.get(c));
        }
        return result;
    }

    List<String> compileOrder(String item) {
        LinkedList<String> result = new LinkedList<>();
        LinkedList<Integer> queue = new LinkedList<>();

        queue.add(vertices.indexOf(item));

        while (!queue.isEmpty()) {
            int r = queue.poll();
            for (int c = 0; c < numVertices; c++) {
                if (adjacency[r][c] && !queue.contains(c)) {
                    queue.add(c);
                }
            }
            result.addFirst(vertices.get(r));
        }
        return result.stream().distinct().collect(toList());
    }
}
```


```txt
Top levels: [top1, top2]
Compile order for top1 [extra1, des1c1, des1a2, des1a1, des1c, des1b, des1a, ip2c, ip2b, ip2a, ipcommon, ip1a, des1, ip2, ip1, top1]
Compile order for top2 [extra1, des1c1, des1a2, des1a1, des1c, des1b, des1a, ipcommon, ip2c, ip2b, ip2a, des1, ip3, ip2, top2]
Compile order for ip1 [extra1, ipcommon, ip1a, ip1]
```



## Kotlin

```scala
// version 1.1.51

import java.util.LinkedList

val s = "top1, top2, ip1, ip2, ip3, ip1a, ip2a, ip2b, ip2c, ipcommon, des1, " +
        "des1a, des1b, des1c, des1a1, des1a2, des1c1, extra1"

val deps = mutableListOf(
    0 to 10, 0 to 2, 0 to 3,
    1 to 10, 1 to 3, 1 to 4,
    2 to 17, 2 to 5, 2 to 9,
    3 to 6, 3 to 7, 3 to 8, 3 to 9,
    10 to 11, 10 to 12, 10 to 13,
    11 to 14, 11 to 15,
    13 to 16, 13 to 17
)

val files = listOf("top1", "top2", "ip1")

class Graph(s: String, edges: List<Pair<Int, Int>>) {

    val vertices = s.split(", ")
    val numVertices = vertices.size
    val adjacency = List(numVertices) { BooleanArray(numVertices) }

    init {
        for (edge in edges) adjacency[edge.first][edge.second] = true
    }

    fun topLevels(): List<String> {
        val result = mutableListOf<String>()
        // look for empty columns
        outer@ for (c in 0 until numVertices) {
            for (r in 0 until numVertices) {
                if (adjacency[r][c]) continue@outer
            }
            result.add(vertices[c])
        }
        return result
    }

    fun compileOrder(item: String): List<String> {
        val result = LinkedList<String>()
        val queue  = LinkedList<Int>()
        queue.add(vertices.indexOf(item))
        while (!queue.isEmpty()) {
            val r = queue.poll()
            for (c in 0 until numVertices) {
                if (adjacency[r][c] && !queue.contains(c)) queue.add(c)
            }
            result.addFirst(vertices[r])
        }
        return result.distinct().toList()
    }
}

fun main(args: Array<String>) {
    val g = Graph(s, deps)
    println("Top levels:  ${g.topLevels()}")
    for (f in files) println("\nCompile order for $f: ${g.compileOrder(f)}")
}
```


```txt

Top levels:  [top1, top2]

Compile order for top1: [extra1, des1c1, des1a2, des1a1, des1c, des1b, des1a, ip2c, ip2b, ip2a, ipcommon, ip1a, des1, ip2, ip1, top1]

Compile order for top2: [extra1, des1c1, des1a2, des1a1, des1c, des1b, des1a, ipcommon, ip2c, ip2b, ip2a, des1, ip3, ip2, top2]

Compile order for ip1: [extra1, ipcommon, ip1a, ip1]

```



## Perl


```perl
#!/usr/bin/perl

use strict;
use warnings;
use List::Util qw( uniq );

my $deps = <<END;
top1    des1 ip1 ip2
top2    des1 ip2 ip3
ip1     extra1 ip1a ipcommon
ip2     ip2a ip2b ip2c ipcommon
des1    des1a des1b des1c
des1a   des1a1 des1a2
des1c   des1c1 extra1
END

sub before
  {
  map { $deps =~ /^$_\b(.+)/m ? before( split ' ', $1 ) : (), $_ } @_
  }

1 while $deps =~ s/^(\w+)\b.*?\K\h+\1\b//gm; # remove self dependencies
print "TOP LEVELS: @{[grep $deps !~ /\h$_\b/, $deps =~ /^\w+/gm]}\n";
print "\nTARGET $_ ORDER: @{[ uniq before split ]}\n"
  for $deps =~ /^\w+/gm, 'top1 top2';
```

```txt

TOP LEVELS: top1 top2

TARGET top1 ORDER: des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip1a ipcommon ip1 ip2a ip2b ip2c ip2 top1

TARGET top2 ORDER: des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip2a ip2b ip2c ipcommon ip2 ip3 top2

TARGET ip1 ORDER: extra1 ip1a ipcommon ip1

TARGET ip2 ORDER: ip2a ip2b ip2c ipcommon ip2

TARGET des1 ORDER: des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1

TARGET des1a ORDER: des1a1 des1a2 des1a

TARGET des1c ORDER: des1c1 extra1 des1c

TARGET top1 top2 ORDER: des1a1 des1a2 des1a des1b des1c1 extra1 des1c des1 ip1a ipcommon ip1 ip2a ip2b ip2c ip2 top1 ip3 top2

```



## Perl 6


```perl6
sub top_topos ( %deps, *@top ) {
    my %ba;
    for %deps.kv -> $after, @befores {
        for @befores -> $before {
            %ba{$after}{$before} = 0 if $before ne $after;
            %ba{$before} //= {};
        }
    }

    if @top {
	my @want = @top;
	my %care;
	%care{@want} = 1 xx *;
	repeat while @want {
	    my @newwant;
	    for @want -> $before {
		if %ba{$before} {
		    for %ba{$before}.keys -> $after {
			if not %ba{$before}{$after} {
			    %ba{$before}{$after}++;
			    push @newwant, $after;
			}
		    }
		}
	    }
	    @want = @newwant;
	    %care{@want} = 1 xx *;
	}

	for %ba.keys -> $before {
	    %ba{$before}:delete unless %care{$before};
	}
    }
 
    my @levels;
    while %ba.grep( not *.value )».key -> @befores {
	push @levels, ~@befores.sort;
        %ba{@befores}:delete;
        for %ba.values { .{@befores}:delete }
    }
    if @top {
	say "For top-level-modules: ", @top;
	say "  $_" for @levels;
    }
    else {
	say "Top levels are: @levels[*-1]";
    }
 
    say "Cycle found! {%ba.keys.sort}" if %ba;
    say ''; 
}

my %deps =
    top1  =>  <des1 ip1 ip2>,
    top2  =>  <des1 ip2 ip3>,
    ip1   =>  <extra1 ip1a ipcommon>,
    ip2   =>  <ip2a ip2b ip2c ipcommon>,
    des1  =>  <des1a des1b des1c>,
    des1a =>  <des1a1 des1a2>,
    des1c =>  <des1c1 extra1>;
     
top_topos(%deps);
top_topos(%deps, 'top1');
top_topos(%deps, 'top2');
top_topos(%deps, 'ip1');
top_topos(%deps, 'top1', 'top2');
```

```txt
Top levels are: top1 top2

For top-level-modules: top1
  des1a1 des1a2 des1b des1c1 extra1 ip1a ip2a ip2b ip2c ipcommon
  des1a des1c ip1 ip2
  des1
  top1

For top-level-modules: top2
  des1a1 des1a2 des1b des1c1 extra1 ip2a ip2b ip2c ip3 ipcommon
  des1a des1c ip2
  des1
  top2

For top-level-modules: ip1
  extra1 ip1a ipcommon
  ip1

For top-level-modules: top1 top2
  des1a1 des1a2 des1b des1c1 extra1 ip1a ip2a ip2b ip2c ip3 ipcommon
  des1a des1c ip1 ip2
  des1
  top1 top2

```



## Phix

Minor tweaks to the Topological_sort code: top_levels, propagate() and -1 now means "not required".

```Phix
sequence names
enum RANK, NAME, DEP    -- content of names
-- rank is 1 for items to compile first, then 2, etc,
--      or 0 if cyclic dependencies prevent compilation.
--   -  and -1 now means "not required".
-- name is handy, and makes the result order alphabetic!
-- dep is a list of dependencies (indexes to other names)

function add_dependency(string name)
    integer k = find(name,vslice(names,NAME))
    if k=0 then
        names = append(names,{-1,name,{}})
        k = length(names)
    end if
    return k
end function

procedure propagate(integer t)
    if names[t][RANK]!=0 then
        names[t][RANK] = 0
        for i=1 to length(names[t][DEP]) do
            propagate(names[t][DEP][i])
        end for
    end if
end procedure

procedure topsort(string input, sequence tops)
    names = {}
    sequence lines = split(input,'\n')
    for i=1 to length(lines) do
        sequence line = split(lines[i],no_empty:=true),
                 dependencies = {}
        integer k = add_dependency(line[1])
        for j=2 to length(line) do
            integer l = add_dependency(line[j])
            if l!=k then -- ignore self-references
                dependencies &= l
            end if
        end for
        names[k][DEP] = dependencies
    end for

    if tops={} then
        -- show top levels
        for i=1 to length(names) do
            for j=1 to length(names[i][DEP]) do
                integer ji = names[i][DEP][j]
                names[ji][RANK] = 0
            end for
        end for
        sequence top_levels = {}
        for i=1 to length(names) do
            if names[i][RANK]=-1 then
                top_levels = append(top_levels,names[i][NAME])
            end if      
        end for
        printf(1,"top levels: %s\n",{join(top_levels)})
        return
    end if
    -- Propagate required by setting RANK to 0:
    for i=1 to length(tops) do
        integer t = add_dependency(tops[i])
        propagate(t)
    end for
    
    -- Now populate names[RANK] iteratively:
    bool more = true
    integer rank = 0
    while more do
        more = false 
        rank += 1
        for i=1 to length(names) do
            if names[i][RANK]=0 then
                bool ok = true
                for j=1 to length(names[i][DEP]) do
                    integer ji = names[i][DEP][j],
                            nr = names[ji][RANK]
                    if nr=0 or nr=rank then
                        -- not yet compiled, or same pass
                        ok = false
                        exit
                    end if
                end for
                if ok then
                    names[i][RANK] = rank
                    more = true
                end if
            end if
        end for
    end while

    names = sort(names) -- (ie by [RANK=1] then [NAME=2])
    integer prank = -1
    for i=1 to length(names) do
        rank = names[i][RANK]
        if rank>-1 then
            puts(1,iff(rank=prank?" ":sprintf("\nlevel %d:",rank)))
            puts(1,names[i][NAME])
            prank = rank
        end if
    end for
    puts(1,"\n")
end procedure

constant input = """
top1    des1 ip1 ip2
top2    des1 ip2 ip3
ip1     extra1 ip1a ipcommon
ip2     ip2a ip2b ip2c ipcommon
des1    des1a des1b des1c
des1a   des1a1 des1a2
des1c   des1c1 extra1"""

topsort(input,{})
topsort(input,{"top1"})
topsort(input,{"top2"})
topsort(input,{"top1","top2"})
topsort(input,{"ip1"})
```

Items on the same line can be compiled at the same time, and each line is alphabetic.

```txt

top levels: top1 top2

level 1:des1a1 des1a2 des1b des1c1 extra1 ip1a ip2a ip2b ip2c ipcommon
level 2:des1a des1c ip1 ip2
level 3:des1
level 4:top1

level 1:des1a1 des1a2 des1b des1c1 extra1 ip2a ip2b ip2c ip3 ipcommon
level 2:des1a des1c ip2
level 3:des1
level 4:top2

level 1:des1a1 des1a2 des1b des1c1 extra1 ip1a ip2a ip2b ip2c ip3 ipcommon
level 2:des1a des1c ip1 ip2
level 3:des1
level 4:top1 top2

level 1:extra1 ip1a ipcommon
level 2:ip1

```



## Python

Where the compile order between a subset of files is arbitrary, they are shown on the same line.

```python
try:
    from functools import reduce
except: pass

# Python 3.x: def topx(data:'dict', tops:'set'=None) -> 'list':
def topx(data, tops=None):
    'Extract the set of top-level(s) in topological order'
    for k, v in data.items():
        v.discard(k) # Ignore self dependencies
    if tops is None:
        tops = toplevels(data)
    return _topx(data, tops, [], set())

def _topx(data, tops, _sofar, _sofar_set):
    'Recursive topological extractor'
    _sofar += [tops] # Accumulates order in reverse
    _sofar_set.union(tops)
    depends = reduce(set.union, (data.get(top, set()) for top in tops))
    if depends:
        _topx(data, depends, _sofar, _sofar_set)
    ordered, accum = [], set()
    for s in _sofar[::-1]:
        ordered += [sorted(s - accum)]
        accum |= s
    return ordered

def printorder(order):
    'Prettyprint topological ordering'
    if order:
        print("First: " + ', '.join(str(s) for s in order[0]))
    for o in order[1:]:
        print(" Then: " + ', '.join(str(s) for s in o))

def toplevels(data):
    '''\
    Extract all top levels from dependency data
    Top levels are never dependents
    '''
    for k, v in data.items():
        v.discard(k) # Ignore self dependencies
    dependents = reduce(set.union, data.values())
    return  set(data.keys()) - dependents

if __name__ == '__main__':
    data = dict(
        top1  = set('ip1 des1 ip2'.split()),
        top2  = set('ip2 des1 ip3'.split()),
        des1  = set('des1a des1b des1c'.split()),
        des1a = set('des1a1 des1a2'.split()),
        des1c = set('des1c1 extra1'.split()),
        ip2   = set('ip2a ip2b ip2c ipcommon'.split()),
        ip1   = set('ip1a ipcommon extra1'.split()),
        )

    tops = toplevels(data)
    print("The top levels of the dependency graph are: " + ' '.join(tops))

    for t in sorted(tops):
        print("\nThe compile order for top level: %s is..." % t)
        printorder(topx(data, set([t])))
    if len(tops) > 1:
        print("\nThe compile order for top levels: %s is..."
              % ' and '.join(str(s) for s in sorted(tops)) )
        printorder(topx(data, tops))
```


'''Sample Output'''

```txt
The top levels of the dependency graph are: top2 top1

The compile order for top level: top1 is...
First: des1a1, des1a2, des1c1, extra1
 Then: des1a, des1b, des1c, ip1a, ip2a, ip2b, ip2c, ipcommon
 Then: des1, ip1, ip2
 Then: top1

The compile order for top level: top2 is...
First: des1a1, des1a2, des1c1, extra1
 Then: des1a, des1b, des1c, ip2a, ip2b, ip2c, ipcommon
 Then: des1, ip2, ip3
 Then: top2

The compile order for top levels: top1 and top2 is...
First: des1a1, des1a2, des1c1, extra1
 Then: des1a, des1b, des1c, ip1a, ip2a, ip2b, ip2c, ipcommon
 Then: des1, ip1, ip2, ip3
 Then: top1, top2
```



## Racket



```racket
#lang racket
(define dep-tree ; go straight for the hash, without parsing strings etc.
  #hash((top1  . (des1 ip1 ip2))
        (top2  . (des1 ip2 ip3))
        (ip1   . (extra1 ip1a ipcommon))
        (ip2   . (ip2a ip2b ip2c ipcommon))
        (des1  . (des1a des1b des1c))
        (des1a . (des1a1 des1a2))
        (des1c . (des1c1 extra1))))

(define (build-tree Deps Top)
  (define (build n b# d)
    (hash-set b# n d))  
  
  (define (inner-b-t node visited built# depth)
    (cond
      [(hash-ref built# node #f)
       built#]
      [(member node visited)
       (error 'build-tree "circular dependency tree at node: ~a" node)]
      [(hash-ref Deps node #f)
       =>
       (λ (deps)
         (define built#+
           (for/fold ((built# built#)) ((dependency deps))
             (if (equal? dependency node)
                 built#
                 (inner-b-t dependency (cons node visited) built# (add1 depth)))))
         (build node built#+ depth))]
      [else
       (build node built# depth)]))
  
  (define final-build# (inner-b-t Top null (hash) 1))
  
  (define levels# (for/fold ((hsh# (hash))) (([k v] (in-hash final-build#)))
                    (hash-update hsh# v (curry cons k) null)))

  (for/list ((lvl (in-list (sort (hash-keys levels#) >))))
    (hash-ref levels# lvl)))

(define (print-build-order Deps Top)
  (define build-order (build-tree Deps Top))
  (printf "To build: ~s~%" Top)
  (for ((round build-order)) (printf "Build: ~a~%" round))
  (newline))

(print-build-order dep-tree 'top1)
(print-build-order dep-tree 'top2)
(with-handlers [(exn? (λ (x) (displayln (exn-message x) (current-error-port))))]
  (build-tree #hash((top . (des1 ip1)) (ip1 . (net netip)) (netip . (mac ip1))) 'top))
```


```txt
To build: top1
Build: (extra1 des1c1 des1a2 des1a1)
Build: (ip2c ip2b ip2a ipcommon ip1a des1b des1c des1a)
Build: (des1 ip2 ip1)
Build: (top1)

To build: top2
Build: (extra1 des1c1 des1a2 des1a1)
Build: (ip2c ip2b ip2a ipcommon des1b des1c des1a)
Build: (ip3 des1 ip2)
Build: (top2)

build-tree: circular dependency tree at node: ip1
```



## REXX

Where the compile order between a subset of files is arbitrary, they are shown on the same line.

This REXX version can handle multiple top levels.

```REXX
/*REXX program  displays the  compile  order  of jobs  (indicating the dependencies).   */
parse arg job                                    /*obtain optional argument from the CL.*/
jobL. =;   stage.=;    #.=0;     @.=;       JL=  /*define some handy─dandy variables.   */
tree. =;                     tree.1= '  top1     des1      ip1       ip2                 '
                             tree.2= '  top2     des1      ip2       ip3                 '
                             tree.3= '  ip1      extra1    ip1a      ipcommon            '
                             tree.4= '  ip2      ip2a      ip2b      ip2c       ipcommon '
                             tree.5= '  des1     des1a     des1b     des1c               '
                             tree.6= '  des1a    des1a1    des1a2                        '
                             tree.7= '  des1c    des1c1    extra1                        '
$=
              do j=1  while  tree.j\==''                               /*build job tree.*/
              parse var tree.j x deps;           @.x=space(deps)       /*extract jobs.  */
              if wordpos(x, $)==0  then $=$ x                          /*Unique? Add it.*/
                       do k=1  for words(@.x);   _=word(@.x, k)
                       if wordpos(_, $)==0  then $=space($ _)
                       end   /*k*/
              end            /*j*/
!.=;  !!.=
              do j=1      for words($);      x=word($, j);         !.x.0=words(@.x)
                  do k=1  for !.x.0;     !.x.k=word(@.x, k);      !!.x.k=!.x.k
                  end   /*k*/                    /* [↑]  build arrays of job departments*/
              end       /*j*/

  do words($)                                    /*process all the jobs specified.      */
      do j=1  for words($);      x=word($, j);       z=words(@.x);       allN=1;      m=0
      if z==0  then do;  #.x=1;  iterate;  end   /*if no dependents, then skip this one.*/
         do k=1  for z;   y=!.x.k                /*examine all the stage numbers.       */
         if datatype(y, 'W')  then m=max(m, y)   /*find the highest stage number.       */
                              else do;  allN=0   /*at least one entry isn't  numeric.   */
                                        if #.y\==0  then !.x.k=#.y
                                   end           /* [↑]  replace with a number.         */
         end   /*k*/
      if allN & m\==0  then #.x=max(#.x, m + 1)  /*replace with the stage number max.   */
      end     /*j*/                              /* [↑]  maybe set the stage number.    */
  end         /*words($)*/

if job=''  then job=word(tree.1, 1)              /*Not specified?   Use 1st job in tree.*/
jobL.1=job                                       /*define the bottom level jobList.     */
s=1                                              /*define the stage level for jobList.  */
        do j=1;              yyy=jobL.j
           do r=1  for words(yyy)                /*verify that there are no duplicates. */
               do c=1  while c<words(yyy);            z=word(yyy,c)
               p=wordpos(z, yyy, c + 1);    if p\==0  then yyy=delword(yyy, p, 1)
               end   /*c*/                       /* [↑]   Duplicate?    Then delete it. */
           end       /*r*/
        jobL.j=yyy
        if yyy=''  then leave                    /*if null, then we're done with jobList*/
        z=words(yyy)                             /*number of jobs in the jobList.       */
        s=s+1                                    /*bump the stage number.               */
              do k=1  for z;   _=word(yyy, k)    /*obtain a stage number for the job.   */
              jobL.s=jobL.s  @._                 /*add a job to a stage.                */
              end   /*k*/
        end         /*j*/

   do k=1  for s;   JL=JL jobL.k;   end  /*k*/   /*build a complete jobList  (JL).      */

   do s=1  for words(JL)                         /*process each job in the  jobList.    */
   _=word(JL, s);     level=#._                  /*get the proper level for the job.    */
   stage.level= stage.level _                    /*assign a level to job stage number.  */
   end   /*s*/                                   /* [↑]  construct various job stages.  */

say '───────  The compile order for job: '       job        " ────────";              say
                                                 /* [↓]  display the stages for the job.*/
   do show=1  for s;     if stage.show\==''  then say show stage.show
   end   /*show*/                                /*stick a fork in it,  we're all done. */
```

```txt

───────  The compile order for job:  top1  ─────── 

1  des1b extra1 ip1a ipcommon ip2a ip2b ip2c des1a1 des1a2 des1c1 extra1
2  ip1 ip2 des1a des1c
3  des1
4  top1

```

```txt

───────  The compile order for job:  top2  ───────

1  ip3 des1b ip2a ip2b ip2c ipcommon des1a1 des1a2 des1c1 extra1
2  ip2 des1a des1c
3  des1
4  top2

```

```txt

───────  The compile order for job:  top1 top2  ───────

1  ip3 des1b extra1 ip1a ipcommon ip2a ip2b ip2c des1a1 des1a2 des1c1 extra1
2  ip1 ip2 des1a des1c
3  des1
4  top1 top2

```



## Tcl


The <tt>topsort</tt> proc is taken from [[Topological sort#Tcl]] with <tt>{*}</tt> removed from the line commented so that results are returned by level:


```Tcl
package require Tcl 8.5
proc topsort {data} {
    # Clean the data
    dict for {node depends} $data {
        if {[set i [lsearch -exact $depends $node]] >= 0} {
            set depends [lreplace $depends $i $i]
            dict set data $node $depends
        }
        foreach node $depends {dict lappend data $node}
    }
    # Do the sort
    set sorted {}
    while 1 {
        # Find available nodes
        set avail [dict keys [dict filter $data value {}]]
        if {![llength $avail]} {
            if {[dict size $data]} {
                error "graph is cyclic, possibly involving nodes \"[dict keys $data]\""
            }
            return $sorted
        }
        lappend sorted $avail   ;# change here: [[Topological sort]] had {*}$avail
        # Remove from working copy of graph
        dict for {node depends} $data {
            foreach n $avail {
                if {[set i [lsearch -exact $depends $n]] >= 0} {
                    set depends [lreplace $depends $i $i]
                    dict set data $node $depends
                }
            }
        }
        foreach node $avail {
            dict unset data $node
        }
    }
}

# The changes to $data in this proc offer an interesting reflection on value semantics.
# Consider the value of $data seen by [dict for], by each invocation of [dict keys]
# and [dict unset] and how that affects the soundness of the loops.
proc tops {data} {
    dict for {k v} $data {
        foreach t [dict keys $data] {
            if {$t in $v} {
                dict unset data $t
            }
        }
    }
    dict keys $data
}

proc withdeps {dict tops {res {}}} {
    foreach top $tops {
        if {[dict exists $dict $top]} {
            set deps [dict get $dict $top]
            set res [dict merge  $res  [dict create $top $deps]  [withdeps $dict $deps]]
        }
    }
    return $res
}

proc parsetop {t} {
    set top {}
    foreach l [split $t \n] {
        catch {dict lappend top {*}$l}
    }
    return $top
}

set inputData {
        top1    des1 ip1 ip2
        top2    des1 ip2 ip3
        ip1     extra1 ip1a ipcommon
        ip2     ip2a ip2b ip2c ipcommon
        des1    des1a des1b des1c
        des1a   des1a1 des1a2
        des1c   des1c1 extra1
}

set d [parsetop $inputData]
pdict $d
set tops [tops $d]

puts "Tops: $tops\n"

set targets [list $tops {*}$tops]
foreach target $targets {
    puts "Target: $target"
    set i 0
    foreach deps [topsort [withdeps $d $target]] {
        puts "\tround [incr i]:\t$deps"
    }
}
```


```txt
Tops: top1 top2

Target: top1 top2
        round 1:        des1b des1a1 des1a2 des1c1 extra1 ip1a ipcommon ip2a ip2b ip2c ip3
        round 2:        des1a des1c ip1 ip2
        round 3:        des1
        round 4:        top1 top2
Target: top1
        round 1:        des1b des1a1 des1a2 des1c1 extra1 ip1a ipcommon ip2a ip2b ip2c
        round 2:        des1a des1c ip1 ip2
        round 3:        des1
        round 4:        top1
Target: top2
        round 1:        ip3 des1b des1a1 des1a2 des1c1 extra1 ip2a ip2b ip2c ipcommon
        round 2:        des1a des1c ip2
        round 3:        des1
        round 4:        top2

```

