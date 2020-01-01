+++
title = "Set consolidation"
description = ""
date = 2019-08-24T20:10:52Z
aliases = []
[extra]
id = 11690
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given two sets of items then if any item is common to any set then the result of applying ''consolidation'' to those sets is a set of sets whose contents is:
*  The two input sets if no common item exists between the two input sets of items.
*  The single set that is the union of the two input sets if they share a common item.


Given N sets of items where N>2 then the result is the same as repeatedly replacing all combinations of two sets by their consolidation until no further consolidation between set pairs is possible.
If N<2 then consolidation has no strict meaning and the input can be returned.

;'''Example 1:'''
:Given the two sets <tt>{A,B}</tt> and <tt>{C,D}</tt> then there is no common element between the sets and the result is the same as the input.
;'''Example 2:'''
:Given the two sets <tt>{A,B}</tt> and <tt>{B,D}</tt> then there is a common element <tt>B</tt> between the sets and the result is the single set <tt>{B,D,A}</tt>.  (Note that order of items in a set is immaterial: <tt>{A,B,D}</tt> is the same as <tt>{B,D,A}</tt> and <tt>{D,A,B}</tt>, etc).
;'''Example 3:'''
:Given the three sets <tt>{A,B}</tt> and <tt>{C,D}</tt> and <tt>{D,B}</tt> then there is no common element between the sets <tt>{A,B}</tt> and <tt>{C,D}</tt> but the sets <tt>{A,B}</tt> and <tt>{D,B}</tt> do share a common element that consolidates to produce the result <tt>{B,D,A}</tt>. On examining this result with the remaining set, <tt>{C,D}</tt>, they share a common element and so consolidate to the final output of the single set <tt>{A,B,C,D}</tt>
;'''Example 4:'''
:The consolidation of the five sets:
::<tt>{H,I,K}</tt>, <tt>{A,B}</tt>, <tt>{C,D}</tt>, <tt>{D,B}</tt>, and <tt>{F,G,H}</tt>
:Is the two sets:
::<tt>{A, C, B, D}</tt>, and <tt>{G, F, I, H, K}</tt>


'''See also'''
* [[wp:Connected component (graph theory)|Connected component (graph theory)]]
* [[Range consolidation]]





## Ada


We start with specifying a generic package Set_Cons that provides the neccessary tools, such as contructing and manipulating sets, truning them, etc.:


```Ada
generic
   type Element is (<>);
   with function Image(E: Element) return String;
package Set_Cons is

   type Set is private;

   -- constructor and manipulation functions for type Set
   function "+"(E: Element) return Set;
   function "+"(Left, Right: Element) return Set;
   function "+"(Left: Set; Right: Element) return Set;
   function "-"(Left: Set; Right: Element) return Set;

   -- compare, unite or output a Set
   function Nonempty_Intersection(Left, Right: Set) return Boolean;
   function Union(Left, Right: Set) return Set;
   function Image(S: Set) return String;

   type Set_Vec is array(Positive range <>) of Set;

   -- output a Set_Vec
   function Image(V: Set_Vec) return String;

private
   type Set is array(Element) of Boolean;

end Set_Cons;
```


Here is the implementation of Set_Cons:


```Ada
package body Set_Cons is

   function "+"(E: Element) return Set is
      S: Set := (others => False);
   begin
      S(E) := True;
      return S;
   end "+";

   function "+"(Left, Right: Element) return Set is
   begin
      return (+Left) + Right;
   end "+";

   function "+"(Left: Set; Right: Element) return Set is
      S: Set := Left;
   begin
      S(Right) := True;
      return S;
   end "+";

   function "-"(Left: Set; Right: Element) return Set is
      S: Set := Left;
   begin
      S(Right) := False;
      return S;
   end "-";

   function Nonempty_Intersection(Left, Right: Set) return Boolean is
   begin
      for E in Element'Range loop
         if Left(E) and then Right(E) then return True;
         end if;
      end loop;
      return False;
   end Nonempty_Intersection;

   function Union(Left, Right: Set) return Set is
      S: Set := Left;
   begin
      for E in Right'Range loop
         if Right(E) then S(E) := True;
         end if;
      end loop;
      return S;
   end Union;

   function Image(S: Set) return String is

      function Image(S: Set; Found: Natural) return String is
      begin
         for E in S'Range loop
            if S(E) then
               if Found = 0 then
                  return Image(E) & Image((S-E), Found+1);
               else
                  return "," & Image(E) & Image((S-E), Found+1);
               end if;
            end if;
         end loop;
         return "";
      end Image;

   begin
      return "{" & Image(S, 0) & "}";
   end Image;

   function Image(V: Set_Vec) return String is
    begin
      if V'Length = 0 then
         return "";
      else
         return Image(V(V'First)) & Image(V(V'First+1 .. V'Last));
      end if;
   end Image;

end Set_Cons;
```


Given that package, the task is easy:


```Ada
with Ada.Text_IO, Set_Cons;

procedure Set_Consolidation is

   type El_Type is (A, B, C, D, E, F, G, H, I, K);

   function Image(El: El_Type) return String is
   begin
      return El_Type'Image(El);
   end Image;

   package Helper is new Set_Cons(Element => El_Type, Image => Image);
   use Helper;

   function Consolidate(List: Set_Vec) return Set_Vec is
   begin
      for I in List'First .. List'Last - 1 loop
         for J in I+1 .. List'Last loop
            -- if List(I) and List(J) share an element
            -- then recursively consolidate
            --   (List(I) union List(J)) followed by List(K), K not in {I, J}
            if Nonempty_Intersection(List(I), List(J)) then
               return Consolidate
                 (Union(List(I), List(J))
                    & List(List'First .. I-1)
                    & List(I+1        .. J-1)
                    & List(J+1        .. List'Last));
            end if;
         end loop;
      end loop;
      return List;
   end Consolidate;

begin
   Ada.Text_IO.Put_Line(Image(Consolidate((A+B) & (C+D))));
   Ada.Text_IO.Put_Line(Image(Consolidate((A+B) & (B+D))));
   Ada.Text_IO.Put_Line(Image(Consolidate((A+B) & (C+D) & (D+B))));
   Ada.Text_IO.Put_Line
     (Image(Consolidate((H+I+K) & (A+B) & (C+D) & (D+B) & (F+G+H))));
end Set_Consolidation;
```


{{out}}

```txt
{A,B}{C,D}
{A,B,D}
{A,B,C,D}
{A,B,C,D}{F,G,H,I,K}
```



## Aime


```aime
display(list l)
{
    for (integer i, record r in l) {
        text u, v;

        o_text(i ? ", {" : "{");
        for (u in r) {
            o_(v, u);
            v = ", ";
        }
        o_text("}");
    }

    o_text("\n");
}

intersect(record r, record u)
{
    trap_q(r_vcall, r, r_put, 1, record().copy(u), 0);
}

consolidate(list l)
{
    for (integer i, record r in l) {
        integer j;

        j = i - ~l;
        while (j += 1) {
            if (intersect(r, l[j])) {
                r.wcall(r_add, 1, 2, l[j]);
                l.delete(i);
                i -= 1;
                break;
            }
        }
    }

    l;
}

R(...)
{
    record r;

    ucall(r_put, 1, r, 0);

    r;
}

main(void)
{
    display(consolidate(list(R("A", "B"), R("C", "D"))));
    display(consolidate(list(R("A", "B"), R("B", "D"))));
    display(consolidate(list(R("A", "B"), R("C", "D"), R("D", "B"))));
    display(consolidate(list(R("H", "I", "K"), R("A", "B"), R("C", "D"),
                             R("D", "B"), R("F", "G", "K"))));

    0;
}
```

{{out}}

```txt
{A, B}, {C, D}
{A, B, D}
{A, B, C, D}
{A, B, C, D}, {F, G, H, I, K}
```



## Bracmat


```bracmat
( ( consolidate
  =   a m z mm za zm zz
    .     ( removeNumFactors
          =   a m z
            .     !arg:?a+#%*?m+?z
                & !a+!m+removeNumFactors$!z
              | !arg
          )
        &   !arg
          :   ?a
              %?`m
              ( %?z
              &   !m
                :   ?
                  + ( %@?mm
                    & !z:?za (?+!mm+?:?zm) ?zz
                    )
                  + ?
              )
        & consolidate$(!a removeNumFactors$(!m+!zm) !za !zz)
      | !arg
  )
& (test=.out$(!arg "==>" consolidate$!arg))
& test$(A+B C+D)
& test$(A+B B+D)
& test$(A+B C+D D+B)
& test$(H+I+K A+B C+D D+B F+G+H)
);
```

{{out}}

```txt
A+B C+D ==> A+B C+D
A+B B+D ==> A+B+D
A+B C+D B+D ==> A+B+C+D
  H+I+K
  A+B
  C+D
  B+D
  F+G+H
  ==>
  F+G+H+I+K
  A+B+C+D
```



## C


```c
#include <stdio.h>

#define s(x) (1U << ((x) - 'A'))

typedef unsigned int bitset;

int consolidate(bitset *x, int len)
{
	int i, j;
	for (i = len - 2; i >= 0; i--)
		for (j = len - 1; j > i; j--)
			if (x[i] & x[j])
				x[i] |= x[j], x[j] = x[--len];
	return len;
}

void show_sets(bitset *x, int len)
{
	bitset b;
	while(len--) {
		for (b = 'A'; b <= 'Z'; b++)
			if (x[len] & s(b)) printf("%c ", b);
		putchar('\n');
	}
}

int main(void)
{
	bitset x[] = { s('A') | s('B'), s('C') | s('D'), s('B') | s('D'),
			s('F') | s('G') | s('H'), s('H') | s('I') | s('K') };

	int len = sizeof(x) / sizeof(x[0]);

	puts("Before:"); show_sets(x, len);
	puts("\nAfter:"); show_sets(x, consolidate(x, len));
	return 0;
}
```


The above is O(N<sup>2</sup>) in terms of number of input sets. If input is large (many sets or huge number of elements), here's an O(N) method, where N is the sum of the sizes of all input sets:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct edge { int to; struct edge *next; };
struct node { int group; struct edge *e; };

int **consolidate(int **x)
{
#	define alloc(v, size) v = calloc(size, sizeof(v[0]));
	int group, n_groups, n_nodes;
	int n_edges = 0;
	struct edge *edges, *ep;
	struct node *nodes;
	int pos, *stack, **ret;

	void add_edge(int a, int b) {
		ep->to = b;
		ep->next = nodes[a].e;
		nodes[a].e = ep;
		ep++;
	}

	void traverse(int a) {
		if (nodes[a].group) return;

		nodes[a].group = group;
		stack[pos++] = a;

		for (struct edge *e = nodes[a].e; e; e = e->next)
			traverse(e->to);
	}

	n_groups = n_nodes = 0;
	for (int i = 0; x[i]; i++, n_groups++)
		for (int j = 0; x[i][j]; j++) {
			n_edges ++;
			if (x[i][j] >= n_nodes)
				n_nodes = x[i][j] + 1;
		}

	alloc(ret, n_nodes);
	alloc(nodes, n_nodes);
	alloc(stack, n_nodes);
	ep = alloc(edges, n_edges);

	for (int i = 0; x[i]; i++)
		for (int *s = x[i], j = 0; s[j]; j++)
			add_edge(s[j], s[j + 1] ? s[j + 1] : s[0]);

	group = 0;
	for (int i = 1; i < n_nodes; i++) {
		if (nodes[i].group) continue;

		group++, pos = 0;
		traverse(i);

		stack[pos++] = 0;
		ret[group - 1] = malloc(sizeof(int) * pos);
		memcpy(ret[group - 1], stack, sizeof(int) * pos);
	}

	free(edges);
	free(stack);
	free(nodes);

	// caller is responsible for freeing ret
	return realloc(ret, sizeof(ret[0]) * (1 + group));
#	undef alloc
}

void show_sets(int **x)
{
	for (int i = 0; x[i]; i++) {
		printf("%d: ", i);
		for (int j = 0; x[i][j]; j++)
			printf(" %d", x[i][j]);
		putchar('\n');
	}
}

int main(void)
{
	int *x[] = {
		(int[]) {1, 2, 0},	// 0: end of set
		(int[]) {3, 4, 0},
		(int[]) {3, 1, 0},
		(int[]) {0},		// empty set
		(int[]) {5, 6, 0},
		(int[]) {7, 6, 0},
		(int[]) {3, 9, 10, 0},
		0			// 0: end of sets
	};

	puts("input:");
	show_sets(x);

	puts("components:");
	show_sets(consolidate(x));

	return 0;
}
```



## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

public class SetConsolidation
{
    public static void Main()
    {
        var setCollection1 = new[] {new[] {"A", "B"}, new[] {"C", "D"}};
        var setCollection2 = new[] {new[] {"A", "B"}, new[] {"B", "D"}};
        var setCollection3 = new[] {new[] {"A", "B"}, new[] {"C", "D"}, new[] {"B", "D"}};
        var setCollection4 = new[] {new[] {"H", "I", "K"}, new[] {"A", "B"}, new[] {"C", "D"},
            new[] {"D", "B"}, new[] {"F", "G", "H"}};
        var input = new[] {setCollection1, setCollection2, setCollection3, setCollection4};

        foreach (var sets in input) {
            Console.WriteLine("Start sets:");
            Console.WriteLine(string.Join(", ", sets.Select(s => "{" + string.Join(", ", s) + "}")));
            Console.WriteLine("Sets consolidated using Nodes:");
            Console.WriteLine(string.Join(", ", ConsolidateSets1(sets).Select(s => "{" + string.Join(", ", s) + "}")));
            Console.WriteLine("Sets consolidated using Set operations:");
            Console.WriteLine(string.Join(", ", ConsolidateSets2(sets).Select(s => "{" + string.Join(", ", s) + "}")));
            Console.WriteLine();
        }
    }

    /// <summary>
    /// Consolidates sets using a connected-component-finding-algorithm involving Nodes with parent pointers.
    /// The more efficient solution, but more elaborate code.
    /// </summary>
    private static IEnumerable<IEnumerable<T>> ConsolidateSets1<T>(IEnumerable<IEnumerable<T>> sets,
        IEqualityComparer<T> comparer = null)
    {
        if (comparer == null) comparer = EqualityComparer<T>.Default;
        var elements = new Dictionary<T, Node<T>>();
        foreach (var set in sets) {
            Node<T> top = null;
            foreach (T value in set) {
                Node<T> element;
                if (elements.TryGetValue(value, out element)) {
                    if (top != null) {
                        var newTop = element.FindTop();
                        top.Parent = newTop;
                        element.Parent = newTop;
                        top = newTop;
                    } else {
                        top = element.FindTop();
                    }
                } else {
                    elements.Add(value, element = new Node<T>(value));
                    if (top == null) top = element;
                    else element.Parent = top;
                }
            }
        }
        foreach (var g in elements.Values.GroupBy(element => element.FindTop().Value))
            yield return g.Select(e => e.Value);
    }

    private class Node<T>
    {
        public Node(T value, Node<T> parent = null) {
            Value = value;
            Parent = parent ?? this;
        }

        public T Value { get; }
        public Node<T> Parent { get; set; }

        public Node<T> FindTop() {
            var top = this;
            while (top != top.Parent) top = top.Parent;
            //Set all parents to the top element to prevent repeated iteration in the future
            var element = this;
            while (element.Parent != top) {
                var parent = element.Parent;
                element.Parent = top;
                element = parent;
            }
            return top;
        }
    }

    /// <summary>
    /// Consolidates sets using operations on the HashSet&lt;T&gt; class.
    /// Less efficient than the other method, but easier to write.
    /// </summary>
    private static IEnumerable<IEnumerable<T>> ConsolidateSets2<T>(IEnumerable<IEnumerable<T>> sets,
        IEqualityComparer<T> comparer = null)
    {
        if (comparer == null) comparer = EqualityComparer<T>.Default;
        var currentSets = sets.Select(s => new HashSet<T>(s)).ToList();
        int previousSize;
        do {
            previousSize = currentSets.Count;
            for (int i = 0; i < currentSets.Count - 1; i++) {
                for (int j = currentSets.Count - 1; j > i; j--) {
                    if (currentSets[i].Overlaps(currentSets[j])) {
                        currentSets[i].UnionWith(currentSets[j]);
                        currentSets.RemoveAt(j);
                    }
                }
            }
        } while (previousSize > currentSets.Count);
        foreach (var set in currentSets) yield return set.Select(value => value);
    }
}
```

{{out}}

```txt

Start sets:
{A, B}, {C, D}
Sets consolidated using nodes:
{A, B}, {C, D}
Sets consolidated using Set operations:
{A, B}, {C, D}

Start sets:
{A, B}, {B, D}
Sets consolidated using nodes:
{A, B, D}
Sets consolidated using Set operations:
{A, B, D}

Start sets:
{A, B}, {C, D}, {B, D}
Sets consolidated using nodes:
{A, B, C, D}
Sets consolidated using Set operations:
{A, B, D, C}

Start sets:
{H, I, K}, {A, B}, {C, D}, {D, B}, {F, G, H}
Sets consolidated using nodes:
{H, I, K, F, G}, {A, B, C, D}
Sets consolidated using Set operations:
{H, I, K, F, G}, {A, B, D, C}
```



## Common Lisp

{{trans|Racket}}

```lisp
(defun consolidate (ss)
  (labels ((comb (cs s)
             (cond ((null s) cs)
                   ((null cs) (list s))
                   ((null (intersection s (first cs)))
                    (cons (first cs) (comb (rest cs) s)))
                   ((consolidate (cons (union s (first cs)) (rest cs)))))))
    (reduce #'comb ss :initial-value nil)))
```


{{Out}}

```txt
> (consolidate '((a b) (c d)))
((A B) (C D))
> (consolidate '((a b) (b c)))
((C A B))
> (consolidate '((a b) (c d) (d b)))
((C D A B))
> (consolidate '((h i k) (a b) (c d) (d b) (f g h)))
((F G H I K) (C D A B))
```



## D

{{trans|Go}}

```d
import std.stdio, std.algorithm, std.array;

dchar[][] consolidate(dchar[][] sets) @safe {
    foreach (set; sets)
        set.sort();

    foreach (i, ref si; sets[0 .. $ - 1]) {
        if (si.empty)
            continue;
        foreach (ref sj; sets[i + 1 .. $])
            if (!sj.empty && !si.setIntersection(sj).empty) {
                sj = si.setUnion(sj).uniq.array;
                si = null;
            }
    }

    return sets.filter!"!a.empty".array;
}

void main() @safe {
    [['A', 'B'], ['C','D']].consolidate.writeln;

    [['A','B'], ['B','D']].consolidate.writeln;

    [['A','B'], ['C','D'], ['D','B']].consolidate.writeln;

    [['H','I','K'], ['A','B'], ['C','D'],
     ['D','B'], ['F','G','H']].consolidate.writeln;
}
```

{{out}}

```txt
["AB", "CD"]
["ABD"]
["ABCD"]
["ABCD", "FGHIK"]
```


'''Recursive version''', as described on talk page.

```d
import std.stdio, std.algorithm, std.array;

dchar[][] consolidate(dchar[][] sets) @safe {
    foreach (set; sets)
        set.sort();

    dchar[][] consolidateR(dchar[][] s) {
        if (s.length < 2)
            return s;
        auto r = [s[0]];
        foreach (x; consolidateR(s[1 .. $])) {
            if (!r[0].setIntersection(x).empty) {
                r[0] = r[0].setUnion(x).uniq.array;
            } else
                r ~= x;
        }
        return r;
    }

    return consolidateR(sets);
}

void main() @safe {
    [['A', 'B'], ['C','D']].consolidate.writeln;

    [['A','B'], ['B','D']].consolidate.writeln;

    [['A','B'], ['C','D'], ['D','B']].consolidate.writeln;

    [['H','I','K'], ['A','B'], ['C','D'],
     ['D','B'], ['F','G','H']].consolidate.writeln;
}
```


```txt
["AB", "CD"]
["ABD"]
["ABCD"]
["FGHIK", "ABCD"]
```



## EchoLisp


```scheme

;; utility : make a set of sets from a list
(define (make-set* s)
		(or (when (list? s) (make-set (map make-set* s))) s))

;; union of all sets which intersect - O(n^2)
(define (make-big ss)
(make-set
	(for/list ((u ss))
	(for/fold (big u) ((v ss)) #:when (set-intersect? big v)  (set-union big v)))))

;; remove sets which are subset of another one - O(n^2)
(define (remove-small ss)
	(for/list ((keep ss))
	#:when (for/and ((v ss))  #:continue (set-equal? keep v) (not (set-subset? v keep)))
	keep))

(define (consolidate ss) (make-set (remove-small (make-big ss))))

(define S (make-set* ' ((h i k) ( a b) ( b c) (c d) ( f g h))))
    → { { a b } { b c } { c d } { f g h } { h i k } }

(consolidate S)
    → { { a b c d } { f g h i k } }


```




## Egison



```egison

(define $consolidate
  (lambda [$xss]
    (match xss (multiset (set char))
      {[<cons <cons $m $xs>
              <cons <cons ,m $ys>
                    $rss>>
        (consolidate {(unique/m char {m @xs @ys}) @rss})]
       [_ xss]})))

(test (consolidate {{'H' 'I' 'K'} {'A' 'B'} {'C' 'D'} {'D' 'B'} {'F' 'G' 'H'}}))

```

{{out}}

```egison

{"DBAC" "HIKFG"}

```



## Ela

This solution emulate sets using linked lists:

```ela
open list

merge [] ys = ys
merge (x::xs) ys | x `elem` ys = merge xs ys
                 | else = merge xs (x::ys)

consolidate (_::[])@xs = xs
consolidate (x::xs) = conso [x] (consolidate xs)
                where conso xs [] = xs
                      conso (x::xs)@r (y::ys) | intersect x y <> [] = conso ((merge x y)::xs) ys
                                              | else = conso (r ++ [y]) ys
```

Usage:

```ela
open monad io

:::IO

do
  x <- return $ consolidate [['H','I','K'], ['A','B'], ['C','D'], ['D','B'], ['F','G','H']]
  putLn x
  y <- return $ consolidate [['A','B'], ['B','D']]
  putLn y
```


Output:
```txt
[['K','I','F','G','H'],['A','C','D','B']]
[['A','B','D']]
```



## Elixir


```elixir
defmodule RC do
  def set_consolidate(sets, result\\[])
  def set_consolidate([], result), do: result
  def set_consolidate([h|t], result) do
    case Enum.find(t, fn set -> not MapSet.disjoint?(h, set) end) do
      nil -> set_consolidate(t, [h | result])
      set -> set_consolidate([MapSet.union(h, set) | t -- [set]], result)
    end
  end
end

examples = [[[:A,:B], [:C,:D]],
            [[:A,:B], [:B,:D]],
            [[:A,:B], [:C,:D], [:D,:B]],
            [[:H,:I,:K], [:A,:B], [:C,:D], [:D,:B], [:F,:G,:H]]]
           |> Enum.map(fn sets ->
                Enum.map(sets, fn set -> MapSet.new(set) end)
              end)

Enum.each(examples, fn sets ->
  IO.write "#{inspect sets} =>\n\t"
  IO.inspect RC.set_consolidate(sets)
end)
```


{{out}}

```txt

[#MapSet<[:A, :B]>, #MapSet<[:C, :D]>] =>
	[#MapSet<[:C, :D]>, #MapSet<[:A, :B]>]
[#MapSet<[:A, :B]>, #MapSet<[:B, :D]>] =>
	[#MapSet<[:A, :B, :D]>]
[#MapSet<[:A, :B]>, #MapSet<[:C, :D]>, #MapSet<[:B, :D]>] =>
	[#MapSet<[:A, :B, :C, :D]>]
[#MapSet<[:H, :I, :K]>, #MapSet<[:A, :B]>, #MapSet<[:C, :D]>, #MapSet<[:B, :D]>, #MapSet<[:F, :G, :H]>] =>
	[#MapSet<[:A, :B, :C, :D]>, #MapSet<[:F, :G, :H, :I, :K]>]

```


=={{header|F_Sharp|F#}}==

```fsharp
let (|SeqNode|SeqEmpty|) s =
    if Seq.isEmpty s then SeqEmpty
    else SeqNode ((Seq.head s), Seq.skip 1 s)

let SetDisjunct x y = Set.isEmpty (Set.intersect x y)

let rec consolidate s = seq {
    match s with
    | SeqEmpty -> ()
    | SeqNode (this, rest) ->
        let consolidatedRest = consolidate rest
        for that in consolidatedRest do
            if (SetDisjunct this that) then yield that
        yield Seq.fold (fun x y -> if not (SetDisjunct x y) then Set.union x y else x) this consolidatedRest
}

[<EntryPoint>]
let main args =
    let makeSeqOfSet listOfList = List.map (fun x -> Set.ofList x) listOfList |> Seq.ofList
    List.iter (fun x -> printfn "%A" (consolidate (makeSeqOfSet x))) [
        [["A";"B"]; ["C";"D"]];
        [["A";"B"]; ["B";"C"]];
        [["A";"B"]; ["C";"D"]; ["D";"B"]];
        [["H";"I";"K"]; ["A";"B"]; ["C";"D"]; ["D";"B"]; ["F";"G";"H"]]
    ]
    0
```

{{out}}

```txt
seq [set ["C"; "D"]; set ["A"; "B"]]
seq [set ["A"; "B"; "C"]]
seq [set ["A"; "B"; "C"; "D"]]
seq [set ["A"; "B"; "C"; "D"]; set ["F"; "G"; "H"; "I"; "K"]]
```



## Go

{{trans|Python}}

```go
package main

import "fmt"

type set map[string]bool

var testCase = []set{
    set{"H": true, "I": true, "K": true},
    set{"A": true, "B": true},
    set{"C": true, "D": true},
    set{"D": true, "B": true},
    set{"F": true, "G": true, "H": true},
}

func main() {
    fmt.Println(consolidate(testCase))
}

func consolidate(sets []set) []set {
    setlist := []set{}
    for _, s := range sets {
        if s != nil && len(s) > 0 {
            setlist = append(setlist, s)
        }
    }
    for i, s1 := range setlist {
        if len(s1) > 0 {
            for _, s2 := range setlist[i+1:] {
                if s1.disjoint(s2) {
                    continue
                }
                for e := range s1 {
                    s2[e] = true
                    delete(s1, e)
                }
                s1 = s2
            }
        }
    }
    r := []set{}
    for _, s := range setlist {
        if len(s) > 0 {
            r = append(r, s)
        }
    }
    return r
}

func (s1 set) disjoint(s2 set) bool {
    for e := range s2 {
        if s1[e] {
            return false
        }
    }
    return true
}
```

{{out}}

```txt

[map[A:true C:true B:true D:true] map[G:true F:true I:true H:true K:true]]

```



## Haskell



```Haskell
import Data.List (intersperse, intercalate)
import qualified Data.Set as S

consolidate
  :: Ord a
  => [S.Set a] -> [S.Set a]
consolidate = foldr comb []
  where
    comb s_ [] = [s_]
    comb s_ (s:ss)
      | S.null (s `S.intersection` s_) = s : comb s_ ss
      | otherwise = comb (s `S.union` s_) ss

-- TESTS -------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines)
    ((intercalate ", and " . fmap showSet . consolidate) . fmap S.fromList <$>
     [ ["ab", "cd"]
     , ["ab", "bd"]
     , ["ab", "cd", "db"]
     , ["hik", "ab", "cd", "db", "fgh"]
     ])

showSet :: S.Set Char -> String
showSet = flip intercalate ["{", "}"] . intersperse ',' . S.elems
```


{{Out}}

```txt
{c,d}, and {a,b}
{a,b,d}
{a,b,c,d}
{a,b,c,d}, and {f,g,h,i,k}
```



## J



```J
consolidate=:4 :0/
  b=. y 1&e.@e.&> x
  (1,-.b)#(~.;x,b#y);y
)
```


Examples:


```J
   consolidate 'ab';'cd'
┌──┬──┐
│ab│cd│
└──┴──┘
   consolidate 'ab';'bd'
┌───┐
│abd│
└───┘
   consolidate 'ab';'cd';'db'
┌────┐
│abcd│
└────┘
   consolidate 'hij';'ab';'cd';'db';'fgh'
┌─────┬────┐
│hijfg│abcd│
└─────┴────┘
```



## Java

{{trans|D}}
{{works with|Java|7}}

```java
import java.util.*;

public class SetConsolidation {

    public static void main(String[] args) {
        List<Set<Character>> h1 = hashSetList("AB", "CD");
        System.out.println(consolidate(h1));

        List<Set<Character>> h2 = hashSetList("AB", "BD");
        System.out.println(consolidateR(h2));

        List<Set<Character>> h3 = hashSetList("AB", "CD", "DB");
        System.out.println(consolidate(h3));

        List<Set<Character>> h4 = hashSetList("HIK", "AB", "CD", "DB", "FGH");
        System.out.println(consolidateR(h4));
    }

    // iterative
    private static <E> List<Set<E>>
                consolidate(Collection<? extends Set<E>> sets) {
	List<Set<E>> r = new ArrayList<>();
	for (Set<E> s : sets) {
	    List<Set<E>> new_r = new ArrayList<>();
	    new_r.add(s);
	    for (Set<E> x : r) {
		if (!Collections.disjoint(s, x)) {
		    s.addAll(x);
		} else {
		    new_r.add(x);
		}
	    }
	    r = new_r;
	}
	return r;
    }

    // recursive
    private static <E> List<Set<E>> consolidateR(List<Set<E>> sets) {
        if (sets.size() < 2)
            return sets;
        List<Set<E>> r = new ArrayList<>();
        r.add(sets.get(0));
        for (Set<E> x : consolidateR(sets.subList(1, sets.size()))) {
            if (!Collections.disjoint(r.get(0), x)) {
                r.get(0).addAll(x);
            } else {
                r.add(x);
            }
        }
        return r;
    }

    private static List<Set<Character>> hashSetList(String... set) {
        List<Set<Character>> r = new ArrayList<>();
        for (int i = 0; i < set.length; i++) {
            r.add(new HashSet<Character>());
            for (int j = 0; j < set[i].length(); j++)
                r.get(i).add(set[i].charAt(j));
        }
        return r;
    }
}
```


```txt
[A, B] [D, C]
[D, A, B]
[D, A, B, C]
[F, G, H, I, K] [D, A, B, C]
```



## JavaScript


```javascript
(() => {
    'use strict';

    // consolidated :: Ord a => [Set a] -> [Set a]
    const consolidated = xs => {
        const go = (s, xs) =>
            0 !== xs.length ? (() => {
                const h = xs[0];
                return 0 === intersection(h, s).size ? (
                    [h].concat(go(s, tail(xs)))
                ) : go(union(h, s), tail(xs));
            })() : [s];
        return foldr(go, [], xs);
    };


    // TESTS ----------------------------------------------
    const main = () =>
        map(xs => intercalate(
                ', and ',
                map(showSet, consolidated(xs))
            ),
            map(x => map(
                    s => new Set(chars(s)),
                    x
                ),
                [
                    ['ab', 'cd'],
                    ['ab', 'bd'],
                    ['ab', 'cd', 'db'],
                    ['hik', 'ab', 'cd', 'db', 'fgh']
                ]
            )
        ).join('\n');


    // GENERIC FUNCTIONS ----------------------------------

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // elems :: Dict -> [a]
    // elems :: Set -> [a]
    const elems = x =>
        'Set' !== x.constructor.name ? (
            Object.values(x)
        ) : Array.from(x.values());

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

    // Note that that the Haskell signature of foldr differs from that of
    // foldl - the positions of accumulator and current value are reversed

    // foldr :: (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(flip(f), a);

    // intercalate :: [a] -> [[a]] -> [a]
    // intercalate :: String -> [String] -> String
    const intercalate = (sep, xs) =>
        0 < xs.length && 'string' === typeof sep &&
        'string' === typeof xs[0] ? (
            xs.join(sep)
        ) : concat(intersperse(sep, xs));

    // intersection :: Ord a => Set a -> Set a -> Set a
    const intersection = (s, s1) =>
        new Set([...s].filter(x => s1.has(x)));

    // intersperse :: a -> [a] -> [a]
    // intersperse :: Char -> String -> String
    const intersperse = (sep, xs) => {
        const bln = 'string' === typeof xs;
        return xs.length > 1 ? (
            (bln ? concat : x => x)(
                (bln ? (
                    xs.split('')
                ) : xs)
                .slice(1)
                .reduce((a, x) => a.concat([sep, x]), [xs[0]])
            )) : xs;
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // showSet :: Set -> String
    const showSet = s =>
        intercalate(elems(s), ['{', '}']);

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.slice()
        .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // union :: Ord a => Set a -> Set a -> Set a
    const union = (s, s1) =>
        Array.from(s1.values())
        .reduce(
            (a, x) => (a.add(x), a),
            new Set(s)
        );

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
{c,d}, and {a,b}
{b,d,a}
{d,b,c,a}
{d,b,c,a}, and {f,g,h,i,k}
```



## jq

'''Infrastructure''':

Currently, jq does not have a "Set" library, so to save space here, we will use simple but inefficient implementations of set-oriented functions as they are fast for sets of moderate size. Nevertheless, we will represent sets as sorted arrays.

```jq
def to_set: unique;

def union(A; B): (A + B) | unique;

# boolean
def intersect(A;B):
  reduce A[] as $x (false; if . then . else (B|index($x)) end) | not | not;
```

'''Consolidation''':

For clarity, the helper functions are presented as top-level functions, but they could be defined as inner functions of the main function, consolidate/0.


```jq
# Input: [i, j, sets] with i < j
# Return [i,j] for a pair that can be combined, else null
def combinable:
   .[0] as $i | .[1] as $j | .[2] as $sets
   | ($sets|length) as $length
   | if intersect($sets[$i]; $sets[$j]) then [$i, $j]
     elif $i < $j - 1      then (.[0] += 1 | combinable)
     elif $j < $length - 1 then [0, $j+1, $sets] | combinable
     else null
     end;

# Given an array of arrays, remove the i-th and j-th elements,
# and add their union:
def update(i;j):
  if i > j then update(j;i)
  elif i == j then del(.[i])
  else
    union(.[i]; .[j]) as $c
    | union(del(.[j]) | del(.[i]); [$c])
  end;

# Input: a set of sets
def consolidate:
   if length <= 1 then .
   else
     ([0, 1, .] | combinable) as $c
     | if $c then update($c[0]; $c[1]) | consolidate
       else .
       end
   end;

```

'''Examples''':

```jq
def tests:
  [["A", "B"], ["C","D"]],
  [["A","B"], ["B","D"]],
  [["A","B"], ["C","D"], ["D","B"]],
  [["H","I","K"], ["A","B"], ["C","D"], ["D","B"], ["F","G","H"]]
;

def test:
  tests | to_set | consolidate;

test
```

{{Out}}

```sh
$ jq -c -n -f Set_consolidation.rc
[["A","B"],["C","D"]]
[["A","B","D"]]
[["A","B","C","D"]]
[["A","B","C","D"],["F","G","H","I","K"]]
```



## Julia

'''The consolidate Function'''

Here I assume that the data are contained in a list of sets.  Perhaps a recursive solution would be more elegant, but in this case playing games with a stack works well enough.

```Julia

function consolidate{T}(a::Array{Set{T},1})
    1 < length(a) || return a
    b = copy(a)
    c = Set{T}[]
    while 1 < length(b)
        x = shift!(b)
        cme = true
        for (i, y) in enumerate(b)
            !isempty(intersect(x, y)) || continue
            cme = false
            b[i] = union(x, y)
            break
        end
        !cme || push!(c, x)
    end
    push!(c, b[1])
    return c
end

```


'''Main'''

```Julia

p = Set(["A", "B"])
q = Set(["C", "D"])
r = Set(["B", "D"])
s = Set(["H", "I", "K"])
t = Set(["F", "G", "H"])

println("p = ", p)
println("q = ", q)
println("r = ", r)
println("s = ", s)
println("t = ", t)

println("consolidate([p, q]) =\n    ", consolidate([p, q]))
println("consolidate([p, r]) =\n    ", consolidate([p, r]))
println("consolidate([p, q, r]) =\n    ", consolidate([p, q, r]))
println("consolidate([p, q, r, s, t]) =\n    ",
        consolidate([p, q, r, s, t]))

```


{{out}}

```txt

p = Set{ASCIIString}({"B","A"})
q = Set{ASCIIString}({"C","D"})
r = Set{ASCIIString}({"B","D"})
s = Set{ASCIIString}({"I","K","H"})
t = Set{ASCIIString}({"G","F","H"})
consolidate([p, q]) =
    [Set{ASCIIString}({"B","A"}),Set{ASCIIString}({"C","D"})]
consolidate([p, r]) =
    [Set{ASCIIString}({"B","A","D"})]
consolidate([p, q, r]) =
    [Set{ASCIIString}({"B","A","C","D"})]
consolidate([p, q, r, s, t]) =
    [Set{ASCIIString}({"B","A","C","D"}),Set{ASCIIString}({"I","G","K","H","F"})]

```



## Kotlin


```scala
// version 1.0.6

fun<T : Comparable<T>> consolidateSets(sets: Array<Set<T>>): Set<Set<T>> {
    val size = sets.size
    val consolidated = BooleanArray(size) // all false by default
    var i = 0
    while (i < size - 1) {
        if (!consolidated[i]) {
            while (true) {
                var intersects = 0
                for (j in (i + 1) until size) {
                    if (consolidated[j]) continue
                    if (sets[i].intersect(sets[j]).isNotEmpty()) {
                        sets[i] = sets[i].union(sets[j])
                        consolidated[j] = true
                        intersects++
                    }
                }
                if (intersects == 0) break
            }
        }
        i++
    }
    return (0 until size).filter { !consolidated[it] }.map { sets[it].toSortedSet() }.toSet()
}

fun main(args: Array<String>) {
    val unconsolidatedSets = arrayOf(
        arrayOf(setOf('A', 'B'), setOf('C', 'D')),
        arrayOf(setOf('A', 'B'), setOf('B', 'D')),
        arrayOf(setOf('A', 'B'), setOf('C', 'D'), setOf('D', 'B')),
        arrayOf(setOf('H', 'I', 'K'), setOf('A', 'B'), setOf('C', 'D'), setOf('D', 'B'), setOf('F', 'G', 'H'))
    )
    for (sets in unconsolidatedSets) println(consolidateSets(sets))
}
```


{{out}}

```txt

[[A, B], [C, D]]
[[A, B, D]]
[[A, B, C, D]]
[[F, G, H, I, K], [A, B, C, D]]

```



## Mathematica


```Mathematica
reduce[x_] :=
 Block[{pairs, unique},
  pairs =
   DeleteCases[
    Subsets[Range@
      Length@x, {2}], _?(Intersection @@ x[[#]] == {} &)];
  unique = Complement[Range@Length@x, Flatten@pairs];
  Join[Union[Flatten[x[[#]]]] & /@ pairs, x[[unique]]]]

consolidate[x__] := FixedPoint[reduce, {x}]
```


```txt
consolidate[{a, b}, {c, d}]
-> {{a, b}, {c, d}}

consolidate[{a, b}, {b, d}]
-> {{a, b, d}}

consolidate[{a, b}, {c, d}, {d, b}]
-> {{a, b, c, d}}

consolidate[{h, i, k}, {a, b}, {c, d}, {d, b}, {f, g, h}]
-> {{a,b,c,d},{f,g,h,i,k}}
```



## OCaml



```ocaml
let join a b =
  List.fold_left (fun acc v ->
    if List.mem v acc then acc else v::acc
  ) b a

let share a b = List.exists (fun x -> List.mem x b) a

let extract p lst =
  let rec aux acc = function
  | x::xs -> if p x then Some (x, List.rev_append acc xs) else aux (x::acc) xs
  | [] -> None
  in
  aux [] lst

let consolidate sets =
  let rec aux acc = function
  | [] -> List.rev acc
  | x::xs ->
      match extract (share x) xs with
      | Some (y, ys) -> aux acc ((join x y) :: ys)
      | None -> aux (x::acc) xs
  in
  aux [] sets

let print_sets sets =
  print_string "{ ";
  List.iter (fun set ->
    print_string "{";
    print_string (String.concat " " set);
    print_string "} "
  ) sets;
  print_endline "}"

let () =
  print_sets (consolidate [["A";"B"]; ["C";"D"]]);
  print_sets (consolidate [["A";"B"]; ["B";"C"]]);
  print_sets (consolidate [["A";"B"]; ["C";"D"]; ["D";"B"]]);
  print_sets (consolidate [["H";"I";"K"]; ["A";"B"]; ["C";"D"]; ["D";"B"];
                           ["F";"G";"H"]]);
;;
```


{{out}}

```txt
{ {A B} {C D} }
{ {A B C} }
{ {B A C D} }
{ {K I F G H} {B A C D} }
```



## ooRexx


```oorexx
/* REXX ***************************************************************
* 04.08.2013 Walter Pachl using ooRexx features
*                   (maybe not in the best way -improvements welcome!)
*                   but trying to demonstrate the algorithm
**********************************************************************/
s.1=.array~of(.set~of('A','B'),.set~of('C','D'))
s.2=.array~of(.set~of('A','B'),.set~of('B','D'))
s.3=.array~of(.set~of('A','B'),.set~of('C','D'),.set~of('D','B'))
s.4=.array~of(.set~of('H','I','K'),.set~of('A','B'),.set~of('C','D'),,
              .set~of('B','D'),.set~of('F','G','H'))
s.5=.array~of(.set~of('snow','ice','slush','frost','fog'),,
              .set~of('iceburgs','icecubes'),,
              .set~of('rain','fog','sleet'))
s.6=.array~of('one')
s.7=.array~new
s.8=.array~of('')
Do si=1 To 8                           /* loop through the test data */
  na=s.si                              /* an array of sets           */
  head='Output(s):'
  Say left('Input' si,10) list_as(na)  /* show the input             */
  Do While na~items()>0                /* while the array ain't empty*/
    na=cons(na)                        /* consolidate and get back   */
                                       /*  array of remaining sets   */
    head='          '
    End
  Say '===='                           /* separator line             */
  End
Exit

cons: Procedure Expose head
/**********************************************************************
* consolidate the sets in the given array
**********************************************************************/
  Use Arg a
  w=a                                  /* work on a copy             */
  n=w~items()                          /* number of sets in the array*/
  Select
    When n=0 Then                      /* no set in array            */
      Return .array~new                /* retuns an empty array      */
    When n=1 Then Do                   /* one set in array           */
      Say head list(w[1])              /* show its contents          */
      Return .array~new                /* retuns an empty array      */
      End
    Otherwise Do                       /* at least two sets are there*/
      b=.array~new                     /* use for remaining sets     */
      r=w[n]                           /* start with last set        */
      try=1
      Do until changed=0               /* loop until result is stable*/
        changed=0
        new=0
        n=w~items()                    /* number of sets             */
        Do i=1 To n-try                /* loop first through n-1 sets*/
          try=0                        /* then through all of them   */
          is=r~intersection(w[i])
          If is~items>0 Then Do        /* any elements in common     */
            r=r~union(w[i])            /* result is the union        */
            Changed=1                  /* and result is now larger   */
            End
          Else Do                      /* no elemen in common        */
            new=new+1                  /* add the set to the array   */
            b[new]=w[i]                /* of remaining sets          */
            End
          End
        If b~items()=0 Then Do         /* no remaining sets          */
          w=.array~new
          Leave                        /* we are done                */
          End
        w=b                            /* repeat with remaining sets */
        b=.array~new                   /* prepare for next iteration */
        End
      End
    Say head list(r)                   /* show one consolidated set  */
    End
  Return w                             /* return array of remaining  */

list: Procedure
/**********************************************************************
* list elements of given set
**********************************************************************/
  Call trace ?O
  Use Arg set
  arr=set~makeArray
  arr~sort()
  ol='('
  Do i=1 To arr~items()
    If i=1 Then
      ol=ol||arr[i]
    Else
      ol=ol||','arr[i]
    End
  Return ol')'

list_as: Procedure
/**********************************************************************
* List an array of sets
**********************************************************************/
  Call trace ?O
  Use Arg a
  n=a~items()
  If n=0 Then
    ol='no element in array'
  Else Do
    ol=''
    Do i=1 To n
      ol=ol '('
      arr=a[i]~makeArray
      Do j=1 To arr~items()
        If j=1 Then
          ol=ol||arr[j]
        Else
          ol=ol','arr[j]
        End
      ol=ol') '
      End
    End
  Return strip(ol)
```

{{out}}

```txt

Input 1    (B,A)  (C,D)
Output(s): (C,D)
           (A,B)
====
Input 2    (B,A)  (B,D)
Output(s): (A,B,D)
====
Input 3    (B,A)  (C,D)  (B,D)
Output(s): (A,B,C,D)
====
Input 4    (H,I,K)  (B,A)  (C,D)  (F,G,H)
Output(s): (F,G,H,I,K)
           (A,B,C,D)
====
Input 5    (snow,fog,ice,frost,slush)  (icecubes,iceburgs)  (fog,sleet,rain)
Output(s): (fog,frost,ice,rain,sleet,slush,snow)
           (iceburgs,icecubes)
====
Input 6    (one)
Output(s): (one)
====
Input 7    no element in array
====
Input 8    ()
Output(s): ()
====

```



## PARI/GP


```parigp
cons(V)={
  my(v,u,s);
  for(i=1,#V,
    v=V[i];
    for(j=i+1,#V,
      u=V[j];
      if(#setintersect(u,v),V[i]=v=vecsort(setunion(u,v));V[j]=[];s++)
    )
  );
  V=select(v->#v,V);
  if(s,cons(V),V)
};
```



## Perl

We implement the key data structure, a set of sets, as an array containing references to arrays of scalars.

```perl
use strict;
use English;
use Smart::Comments;

my @ex1 = consolidate( (['A', 'B'], ['C', 'D']) );
### Example 1: @ex1
my @ex2 = consolidate( (['A', 'B'], ['B', 'D']) );
### Example 2: @ex2
my @ex3 = consolidate( (['A', 'B'], ['C', 'D'], ['D', 'B']) );
### Example 3: @ex3
my @ex4 = consolidate( (['H', 'I', 'K'], ['A', 'B'], ['C', 'D'], ['D', 'B'], ['F', 'G', 'H']) );
### Example 4: @ex4
exit 0;

sub consolidate {
    scalar(@ARG) >= 2 or return @ARG;
    my @result = ( shift(@ARG) );
    my @recursion = consolidate(@ARG);
    foreach my $r (@recursion) {
        if (set_intersection($result[0], $r)) {
            $result[0] = [ set_union($result[0], $r) ];
        }
        else {
            push @result, $r;
        }
    }
    return @result;
}

sub set_union {
    my ($a, $b) = @ARG;
    my %union;
    foreach my $a_elt (@{$a}) { $union{$a_elt}++; }
    foreach my $b_elt (@{$b}) { $union{$b_elt}++; }
    return keys(%union);
}

sub set_intersection {
    my ($a, $b) = @ARG;
    my %a_hash;
    foreach my $a_elt (@{$a}) { $a_hash{$a_elt}++; }
    my @result;
    foreach my $b_elt (@{$b}) {
        push(@result, $b_elt) if exists($a_hash{$b_elt});
    }
    return @result;
}
```

{{out}}

```txt
### Example 1: [
###              [
###                'A',
###                'B'
###              ],
###              [
###                'C',
###                'D'
###              ]
###            ]

### Example 2: [
###              [
###                'D',
###                'B',
###                'A'
###              ]
###            ]

### Example 3: [
###              [
###                'A',
###                'C',
###                'D',
###                'B'
###              ]
###            ]

### Example 4: [
###              [
###                'H',
###                'F',
###                'K',
###                'G',
###                'I'
###              ],
###              [
###                'D',
###                'B',
###                'A',
###                'C'
###              ]
###            ]
```



## Perl 6


```perl6
multi consolidate() { () }
multi consolidate(Set \this is copy, *@those) {
    gather {
        for consolidate |@those -> \that {
            if this ∩ that { this = this ∪ that }
            else           { take that }
        }
        take this;
    }
}

enum Elems <A B C D E F G H I J K>;
say $_, "\n    ==> ", consolidate |$_
    for [set(A,B), set(C,D)],
        [set(A,B), set(B,D)],
        [set(A,B), set(C,D), set(D,B)],
        [set(H,I,K), set(A,B), set(C,D), set(D,B), set(F,G,H)];
```

{{out}}

```txt
set(A, B) set(C, D)
    ==> set(C, D) set(A, B)
set(A, B) set(B, D)
    ==> set(A, B, D)
set(A, B) set(C, D) set(D, B)
    ==> set(A, B, C, D)
set(H, I, K) set(A, B) set(C, D) set(D, B) set(F, G, H)
    ==> set(A, B, C, D) set(H, I, K, F, G)
```



## Phix

Using strings to represent sets of characters

```Phix
function has_intersection(sequence set1, sequence set2)
    for i=1 to length(set1) do
        if find(set1[i],set2) then
            return true
        end if
    end for
    return false
end function

function union(sequence set1, sequence set2)
    for i=1 to length(set2) do
        if not find(set2[i],set1) then
            set1 = append(set1,set2[i])
        end if
    end for
    return set1
end function

function consolidate(sequence sets)
    for i=length(sets) to 1 by -1 do
        for j=length(sets) to i+1 by -1 do
            if has_intersection(sets[i],sets[j]) then
                sets[i] = union(sets[i],sets[j])
                sets[j..j] = {}
            end if
        end for
    end for
    return sets
end function

?consolidate({"AB","CD"})
?consolidate({"AB","BD"})
?consolidate({"AB","CD","DB"})
?consolidate({"HIK","AB","CD","DB","FGH"})
```

{{out}}

```txt

{"AB","CD"}
{"ABD"}
{"ABCD"}
{"HIKFG","ABCD"}

```



## PicoLisp

{{trans|Python}}

```PicoLisp
(de consolidate (S)
   (when S
      (let R (cons (car S))
         (for X (consolidate (cdr S))
            (if (mmeq X (car R))
               (set R (uniq (conc X (car R))))
               (conc R (cons X)) ) )
         R ) ) )
```

Test:

```PicoLisp
: (consolidate '((A B) (C D)))
-> ((A B) (C D))
: (consolidate '((A B) (B D)))
-> ((B D A))
: (consolidate '((A B) (C D) (D B)))
-> ((D B C A))
: (consolidate '((H I K) (A B) (C D) (D B) (F G H)))
-> ((F G H I K) (D B C A))
```



## PL/I


```PL/I
Set: procedure options (main);     /* 13 November 2013 */
   declare set(20) character (200) varying;
   declare e character (1);
   declare (i, n) fixed binary;

   set = '';
   n = 1;
   do until (e = ']');
      get edit (e) (a(1)); put edit (e) (a(1));
      if e = '}' then n = n + 1; /* end of set. */
      if e ^= '{' & e ^= ',' & e ^= '}' & e ^= ' ' then
         set(n) = set(n) || e;   /* Build set */
   end;
   /* We have read in all sets. */
   n = n - 1; /* we have n sets */
   /* Display the sets: */
   put skip list ('The original sets:');
   do i = 1 to n;
      call print(i);
   end;
   /* Look for sets to combine: */
   do i = 2 to n;
      if length(set(i)) > 0 then
         if search(set(1), set(i)) > 0 then
            /* there's at least one common element */
            do; call combine (1, i); set(i) = '';  end;
   end;

   put skip (2) list ('Results:');
   do i = 1 to n;
      if length(set(i)) > 0 then call print (i);
   end;

combine: procedure (p, q);
   declare (p, q) fixed binary;
   declare e character (1);
   declare i fixed binary;

   do i = 1 to length(set(q));
      e = substr(set(q), i, 1);
      if index(set(p), e) = 0 then set(p) = set(p) || e;
   end;

end combine;

print: procedure(k);
   declare k fixed binary;
   declare i fixed binary;

   put edit ('{') (a);
   do i = 1 to length(set(k));
      put edit (substr(set(k), i, 1)) (a);
      if i < length(set(k)) then put edit (',') (a);
   end;
   put edit ('} ') (a);
end print;

end Set;
```


```txt

The original sets: {A,B}

Results: {A,B}

The original sets: {A,B} {C,D}

Results: {A,B} {C,D}

The original sets: {A,B} {B,C}

Results: {A,B,C}

The original sets: {A,B} {C,D} {E,B,F,G,H}

Results: {A,B,E,F,G,H} {C,D}

```



## Python


### Python: Iterative


```python
def consolidate(sets):
    setlist = [s for s in sets if s]
    for i, s1 in enumerate(setlist):
        if s1:
            for s2 in setlist[i+1:]:
                intersection = s1.intersection(s2)
                if intersection:
                    s2.update(s1)
                    s1.clear()
                    s1 = s2
    return [s for s in setlist if s]
```



### Python: Recursive


```python
def conso(s):
	if len(s) < 2: return s

	r, b = [s[0]], conso(s[1:])
	for x in b:
		if r[0].intersection(x): r[0].update(x)
		else: r.append(x)
	return r
```



### Python: Testing

The <code>_test</code> function contains solutions to all the examples as well as a check to show the order-independence of the sets given to the consolidate function.

```python
def _test(consolidate=consolidate):

    def freze(list_of_sets):
        'return a set of frozensets from the list of sets to allow comparison'
        return set(frozenset(s) for s in list_of_sets)

    # Define some variables
    A,B,C,D,E,F,G,H,I,J,K = 'A,B,C,D,E,F,G,H,I,J,K'.split(',')
    # Consolidate some lists of sets
    assert (freze(consolidate([{A,B}, {C,D}])) == freze([{'A', 'B'}, {'C', 'D'}]))
    assert (freze(consolidate([{A,B}, {B,D}])) == freze([{'A', 'B', 'D'}]))
    assert (freze(consolidate([{A,B}, {C,D}, {D,B}])) == freze([{'A', 'C', 'B', 'D'}]))
    assert (freze(consolidate([{H,I,K}, {A,B}, {C,D}, {D,B}, {F,G,H}])) ==
             freze([{'A', 'C', 'B', 'D'}, {'G', 'F', 'I', 'H', 'K'}]))
    assert (freze(consolidate([{A,H}, {H,I,K}, {A,B}, {C,D}, {D,B}, {F,G,H}])) ==
             freze([{'A', 'C', 'B', 'D', 'G', 'F', 'I', 'H', 'K'}]))
    assert (freze(consolidate([{H,I,K}, {A,B}, {C,D}, {D,B}, {F,G,H}, {A,H}])) ==
             freze([{'A', 'C', 'B', 'D', 'G', 'F', 'I', 'H', 'K'}]))
    # Confirm order-independence
    from copy import deepcopy
    import itertools
    sets = [{H,I,K}, {A,B}, {C,D}, {D,B}, {F,G,H}, {A,H}]
    answer = consolidate(deepcopy(sets))
    for perm in itertools.permutations(sets):
            assert consolidate(deepcopy(perm)) == answer

    assert (answer == [{'A', 'C', 'B', 'D', 'G', 'F', 'I', 'H', 'K'}])
    assert (len(list(itertools.permutations(sets))) == 720)

    print('_test(%s) complete' % consolidate.__name__)

if __name__ == '__main__':
    _test(consolidate)
    _test(conso)
```


{{out}}

```txt
_test(consolidate) complete
_test(conso) complete
```



### Python: Functional


As a fold (catamorphism), using '''union''' in preference to mutation:

{{Trans|Haskell}}
{{Trans|JavaScript}}
{{Works with|Python|3.7}}

```python
'''Set consolidation'''

from functools import (reduce)


# consolidated :: Ord a => [Set a] -> [Set a]
def consolidated(sets):
    '''A consolidated list of sets.'''
    def go(xs, s):
        if xs:
            h = xs[0]
            return go(xs[1:], h.union(s)) if (
                h.intersection(s)
            ) else [h] + go(xs[1:], s)
        else:
            return [s]
    return reduce(go, sets, [])


# TESTS ---------------------------------------------------
# main :: IO ()
def main():
    '''Illustrative consolidations.'''

    print(
        tabulated('Consolidation of sets of characters:')(
            lambda x: str(list(map(compose(concat)(list), x)))
        )(str)(
            consolidated
        )(list(map(lambda xs: list(map(set, xs)), [
            ['ab', 'cd'],
            ['ab', 'bd'],
            ['ab', 'cd', 'db'],
            ['hik', 'ab', 'cd', 'db', 'fgh']
        ])))
    )


# DISPLAY OF RESULTS --------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concat :: [String] -> String
def concat(xs):
    '''Concatenation of strings in xs.'''
    return ''.join(xs)


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Consolidation of sets of characters:
                    ['ba', 'cd'] -> [{'b', 'a'}, {'c', 'd'}]
                    ['ba', 'bd'] -> [{'b', 'd', 'a'}]
              ['ba', 'cd', 'db'] -> [{'d', 'a', 'c', 'b'}]
['ikh', 'ba', 'cd', 'db', 'gfh'] -> [{'d', 'a', 'c', 'b'}, {'i', 'k', 'g', 'h', 'f'}]
```



## Racket


```racket

#lang racket
(define (consolidate ss)
  (define (comb s cs)
    (cond [(set-empty? s) cs]
          [(empty? cs) (list s)]
          [(set-empty? (set-intersect s (first cs)))
           (cons (first cs) (comb s (rest cs)))]
          [(consolidate (cons (set-union s (first cs)) (rest cs)))]))
  (foldl comb '() ss))

(consolidate (list (set 'a 'b) (set 'c 'd)))
(consolidate (list (set 'a 'b) (set 'b 'c)))
(consolidate (list (set 'a 'b) (set 'c 'd) (set 'd 'b)))
(consolidate (list (set 'h 'i 'k) (set 'a 'b) (set 'c 'd) (set 'd 'b) (set 'f 'g 'h)))

```

{{out}}

```racket

(list (set 'b 'a) (set 'd 'c))
(list (set 'a 'b 'c))
(list (set 'a 'b 'd 'c))
(list (set 'g 'h 'k 'i 'f) (set 'a 'b 'd 'c))

```




## REXX


```rexx
/*REXX program  demonstrates  a method  of  consolidating  some sample  sets.           */
@.=;     @.1 = '{A,B}     {C,D}'
         @.2 = "{A,B}     {B,D}"
         @.3 = '{A,B}     {C,D}     {D,B}'
         @.4 = '{H,I,K}   {A,B}     {C,D}     {D,B}     {F,G,H}'
         @.5 = '{snow,ice,slush,frost,fog} {icebergs,icecubes} {rain,fog,sleet}'

               do j=1  while @.j\==''            /*traipse through each of sample sets. */
               call SETconsolidate @.j           /*have the function do the heavy work. */
               end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isIn:  return wordpos(arg(1), arg(2))\==0        /*is (word) argument 1 in the set arg2?*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
SETconsolidate: procedure;  parse arg old;       #=words(old);      new=
       say ' the old set=' space(old)

         do k=1  for #                           /* [↓]  change all commas to a blank.  */
         !.k=translate(word(old,k), , '},{')     /*create a list of words  (aka, a set).*/
         end   /*k*/                             /* [↑]  ··· and also remove the braces.*/

         do  until \changed;    changed=0        /*consolidate some sets  (well, maybe).*/
              do set=1  for #-1
                  do item=1  for words(!.set);       x=word(!.set, item)
                      do other=set+1  to #
                      if isIn(x, !.other)  then do;  changed=1            /*it's changed*/
                                                     !.set=!.set !.other;   !.other=
                                                     iterate set
                                                end
                      end   /*other*/
                  end       /*item */
              end           /*set  */
         end                /*until ¬changed*/

            do set=1  for #;   $=                                           /*elide dups*/
              do items=1  for words(!.set);   x=word(!.set, items)
              if x==','  then iterate;        if x==''  then leave
              $=$ x                                                         /*build new. */
                     do  until  \isIn(x, !.set)
                     _=wordpos(x, !.set)
                     !.set=subword(!.set, 1, _-1)  ','  subword(!.set, _+1) /*purify set*/
                     end   /*until ¬isIn ··· */
              end          /*items*/
            !.set=translate(strip($), ',', " ")
            end            /*set*/

         do i=1  for #; if !.i==''  then iterate /*ignore any  set  that is a null set. */
         new=space(new  '{'!.i"}")               /*prepend and append a set identifier. */
         end   /*i*/

       say ' the new set='  new;         say
       return
```

'''output'''   when using the (internal) default supplied sample sets:

```txt

 the old set= {A,B} {C,D}
 the new set= {A,B} {C,D}

 the old set= {A,B} {B,D}
 the new set= {A,B,D}

 the old set= {A,B} {C,D} {D,B}
 the new set= {A,B,D,C}

 the old set= {H,I,K} {A,B} {C,D} {D,B} {F,G,H}
 the new set= {H,I,K,F,G} {A,B,D,C}

 the old set= {snow,ice,slush,frost,fog} {icebergs,icecubes} {rain,fog,sleet}
 the new set= {snow,ice,slush,frost,fog,rain,sleet} {icebergs,icecubes}

```



## Ring


```ring

# Project : Set consolidation

load "stdlib.ring"
test = ["AB","AB,CD","AB,CD,DB","HIK,AB,CD,DB,FGH"]
for t in test
     see consolidate(t) + nl
next
func consolidate(s)
	sets = split(s,",")
	n = len(sets)
	for i = 1 to n
	     p = i
             ts = ""
	     for j = i to 1 step -1
		 if ts = ""
		    p = j
		 ok
		 ts = ""
		 for k = 1 to len(sets[p])
                      if j > 1
		         if substring(sets[j-1],substr(sets[p],k,1),1) = 0
			     ts = ts + substr(sets[p],k,1)
		         ok
                      ok
		 next
		 if len(ts) < len(sets[p])
                    if j > 1
		       sets[j-1] = sets[j-1] + ts
		       sets[p] = "-"
		       ts = ""
                    ok
		 else
		    p = i
		 ok
	     next
	next
	consolidate = s + " = " + substr(list2str(sets),nl,",")
        return consolidate

```

Output:

```txt

AB = AB
AB,CD = AB,CD
AB,CD,DB = ABCD,-,-
HIK,AB,CD,DB,FGH = HIKFG,ABCD,-,-,-

```



## Ruby


```ruby
require 'set'

tests = [[[:A,:B], [:C,:D]],
         [[:A,:B], [:B,:D]],
         [[:A,:B], [:C,:D], [:D,:B]],
         [[:H,:I,:K], [:A,:B], [:C,:D], [:D,:B], [:F,:G,:H]]]
tests.map!{|sets| sets.map(&:to_set)}

tests.each do |sets|
  until sets.combination(2).none?{|a,b| a.merge(b) && sets.delete(b) if a.intersect?(b)}
  end
  p sets
end
```

{{out}}

```txt

[#<Set: {:A, :B}>, #<Set: {:C, :D}>]
[#<Set: {:A, :B, :D}>]
[#<Set: {:A, :B, :D, :C}>]
[#<Set: {:H, :I, :K, :F, :G}>, #<Set: {:A, :B, :D, :C}>]

```

Note: After execution, the contents of tests are exchanged.


## Scala


```Scala
object SetConsolidation extends App {
    def consolidate[Type](sets: Set[Set[Type]]): Set[Set[Type]] = {
        var result = sets // each iteration combines two sets and reiterates, else returns
        for (i <- sets; j <- sets - i; k = i.intersect(j);
            if result == sets && k.nonEmpty) result = result - i - j + i.union(j)
        if (result == sets) sets else consolidate(result)
    }

    // Tests:
    def parse(s: String) =
        s.split(",").map(_.split("").toSet).toSet
    def pretty[Type](sets: Set[Set[Type]]) =
        sets.map(_.mkString("{",",","}")).mkString(" ")
    val tests = List(
        parse("AB,CD") -> Set(Set("A", "B"), Set("C", "D")),
        parse("AB,BD") -> Set(Set("A", "B", "D")),
        parse("AB,CD,DB") -> Set(Set("A", "B", "C", "D")),
        parse("HIK,AB,CD,DB,FGH") -> Set(Set("A", "B", "C", "D"), Set("F", "G", "H", "I", "K"))
    )
    require(Set("A", "B", "C", "D") == Set("B", "C", "A", "D"))
    assert(tests.forall{case (test, expect) =>
        val result = consolidate(test)
        println(s"${pretty(test)} -> ${pretty(result)}")
        expect == result
    })

}
```

{{out}}

```txt
{A,B} {C,D} -> {A,B} {C,D}
{A,B} {B,D} -> {A,B,D}
{A,B} {C,D} {D,B} -> {C,D,A,B}
{D,B} {F,G,H} {A,B} {C,D} {H,I,K} -> {F,I,G,H,K} {A,B,C,D}
```



## Sidef

{{trans|Perl 6}}

```ruby
func consolidate() { [] }
func consolidate(this, *those) {
    gather {
        consolidate(those...).each { |that|
            if (this & that) { this |= that }
            else             { take that }
        }
        take this;
    }
}

enum |A="A", B, C, D, _E, F, G, H, I, _J, K|;

func format(ss) {
    ss.map{ '(' + .join(' ') + ')' }.join(' ')
}

[
    [[A,B], [C,D]],
    [[A,B], [B,D]],
    [[A,B], [C,D], [D,B]],
    [[H,I,K], [A,B], [C,D], [D,B], [F,G,H]]
].each { |ss|
    say (format(ss), "\n\t==> ", format(consolidate(ss...)));
}
```

{{out}}

```txt

(A B) (C D)
	==> (C D) (A B)
(A B) (B D)
	==> (A D B)
(A B) (C D) (D B)
	==> (A C D B)
(H I K) (A B) (C D) (D B) (F G H)
	==> (A C D B) (I K F G H)

```



## Tcl

{{trans|Python}}
{{tcllib|struct::set}}
This uses just the recursive version, as this is sufficient to handle substantial merges.

```tcl
package require struct::set

proc consolidate {sets} {
    if {[llength $sets] < 2} {
	return $sets
    }

    set r [list {}]
    set r0 [lindex $sets 0]
    foreach x [consolidate [lrange $sets 1 end]] {
	if {[struct::set size [struct::set intersect $x $r0]]} {
	    struct::set add r0 $x
	} else {
	    lappend r $x
	}
    }
    return [lset r 0 $r0]
}
```

Demonstrating:

```tcl
puts 1:[consolidate {{A B} {C D}}]
puts 2:[consolidate {{A B} {B D}}]
puts 3:[consolidate {{A B} {C D} {D B}}]
puts 4:[consolidate {{H I K} {A B} {C D} {D B} {F G H}}]
```

{{out}}

```txt
1:{A B} {C D}
2:{D A B}
3:{D A B C}
4:{H I F G K} {D A B C}
```



## TXR


Original solution:


```txrlisp
(defun mkset (p x) (set [p x] (or [p x] x)))

(defun fnd (p x) (if (eq [p x] x) x (fnd p [p x])))

(defun uni (p x y)
  (let ((xr (fnd p x)) (yr (fnd p y)))
    (set [p xr] yr)))

(defun consoli (sets)
  (let ((p (hash)))
    (each ((s sets))
      (each ((e s))
        (mkset p e)
        (uni p e (car s))))
    (hash-values
      [group-by (op fnd p) (hash-keys
                             [group-by identity (flatten sets)])])))

;; tests

(each ((test '(((a b) (c d))
               ((a b) (b d))
               ((a b) (c d) (d b))
               ((h i k) (a b) (c d) (d b) (f g h)))))
  (format t "~s -> ~s\n" test (consoli test)))
```


{{out}}

```txt
((a b) (c d)) -> ((b a) (d c))
((a b) (b d)) -> ((b a d))
((a b) (c d) (d b)) -> ((b a d c))
((h i k) (a b) (c d) (d b) (f g h)) -> ((g f k i h) (b a d c)
```


{{trans|Racket}}


```txrlisp
(defun mkset (items) [group-by identity items])

(defun empty-p (set) (zerop (hash-count set)))

(defun consoli (ss)
  (defun combi (cs s)
    (cond ((empty-p s) cs)
          ((null cs) (list s))
          ((empty-p (hash-isec s (first cs)))
           (cons (first cs) (combi (rest cs) s)))
          (t (consoli (cons (hash-uni s (first cs)) (rest cs))))))
  [reduce-left combi ss nil])

;; tests
(each ((test '(((a b) (c d))
               ((a b) (b d))
               ((a b) (c d) (d b))
               ((h i k) (a b) (c d) (d b) (f g h)))))
  (format t "~s -> ~s\n" test
          [mapcar hash-keys (consoli [mapcar mkset test])]))
```


{{out}}

```txt
((a b) (c d)) -> ((b a) (d c))
((a b) (b d)) -> ((d b a))
((a b) (c d) (d b)) -> ((d c b a))
((h i k) (a b) (c d) (d b) (f g h)) -> ((g f k i h) (d c b a))
```



## VBA

{{trans|Phix}}
This solutions uses collections as sets. The first three coroutines are based on the Phix solution. Two coroutines are written to create the example sets as collections, and another coroutine to show the consolidated set.

```vb
Private Function has_intersection(set1 As Collection, set2 As Collection) As Boolean
    For Each element In set1
        On Error Resume Next
        tmp = set2(element)
        If tmp = element Then
            has_intersection = True
            Exit Function
        End If
    Next element
End Function
Private Sub union(set1 As Collection, set2 As Collection)
    For Each element In set2
        On Error Resume Next
        tmp = set1(element)
        If tmp <> element Then
            set1.Add element, element
        End If
    Next element
End Sub
Private Function consolidate(sets As Collection) As Collection
    For i = sets.Count To 1 Step -1
        For j = sets.Count To i + 1 Step -1
            If has_intersection(sets(i), sets(j)) Then
                union sets(i), sets(j)
                sets.Remove j
            End If
        Next j
    Next i
    Set consolidate = sets
End Function
Private Function mc(s As Variant) As Collection
    Dim res As New Collection
    For i = 1 To Len(s)
        res.Add Mid(s, i, 1), Mid(s, i, 1)
    Next i
    Set mc = res
End Function
Private Function ms(t As Variant) As Collection
    Dim res As New Collection
    Dim element As Collection
    For i = LBound(t) To UBound(t)
        Set element = t(i)
        res.Add t(i)
    Next i
    Set ms = res
End Function
Private Sub show(x As Collection)
    Dim t() As String
    Dim u() As String
    ReDim t(1 To x.Count)
    For i = 1 To x.Count
        ReDim u(1 To x(i).Count)
        For j = 1 To x(i).Count
            u(j) = x(i)(j)
        Next j
        t(i) = "{" & Join(u, ", ") & "}"
    Next i
    Debug.Print "{" & Join(t, ", ") & "}"
End Sub
Public Sub main()
    show consolidate(ms(Array(mc("AB"), mc("CD"))))
    show consolidate(ms(Array(mc("AB"), mc("BD"))))
    show consolidate(ms(Array(mc("AB"), mc("CD"), mc("DB"))))
    show consolidate(ms(Array(mc("HIK"), mc("AB"), mc("CD"), mc("DB"), mc("FGH"))))
End Sub
```
{{out}}

```txt
{{A, B}, {C, D}}
{{A, B, D}}
{{A, B, C, D}}
{{H, I, K, F, G}, {A, B, C, D}}
```


## VBScript


```vb

Function consolidate(s)
	sets = Split(s,",")
	n = UBound(sets)
	For i = 1 To n
		p = i
		ts = ""
		For j = i To 1 Step -1
			If ts = "" Then
				p = j
			End If
			ts = ""
			For k = 1 To Len(sets(p))
				If InStr(1,sets(j-1),Mid(sets(p),k,1)) = 0 Then
					ts = ts & Mid(sets(p),k,1)
				End If
			Next
			If Len(ts) < Len(sets(p)) Then
				sets(j-1) = sets(j-1) & ts
				sets(p) = "-"
				ts = ""
			Else
				p = i
			End If
		Next
	Next
	consolidate = s & " = " & Join(sets," , ")
End Function

'testing
test = Array("AB","AB,CD","AB,CD,DB","HIK,AB,CD,DB,FGH")
For Each t In test
	WScript.StdOut.WriteLine consolidate(t)
Next

```


{{Out}}

```txt

AB = AB
AB,CD = AB , CD
AB,CD,DB = ABCD , - , -
HIK,AB,CD,DB,FGH = HIKFG , ABCD , - , - , -

```



## zkl

{{trans|Tcl}}

```zkl
fcn consolidate(sets){  // set are munged if they are read/write
   if(sets.len()<2) return(sets);
   r,r0 := List(List()),sets[0];
   foreach x in (consolidate(sets[1,*])){
      i,ni:=x.filter22(r0.holds); //-->(intersection, !intersection)
      if(i) r0=r0.extend(ni);
      else  r.append(x);
   }
   r[0]=r0;
   r
}
```


```zkl
fcn prettize(sets){
   sets.apply("concat"," ").pump(String,"(%s),".fmt)[0,-1]
}

foreach sets in (T(
  T(L("A","B")),
  T(L("A","B"),L("C","D")),
  T(L("A","B"),L("B","D")),
  T(L("A","B"),L("C","D"),L("D","B")),
  T(L("H","I","K"),L("A","B"),L("C","D"),L("D","B"),L("F","G","H")),
  T(L("A","H"),L("H","I","K"),L("A","B"),L("C","D"),L("D","B"),L("F","G","H")),
  T(L("H","I","K"),L("A","B"),L("C","D"),L("D","B"),L("F","G","H"), L("A","H")),
)){
   prettize(sets).print(" --> ");
   consolidate(sets) : prettize(_).println();
}
```

{{out}}

```txt

(A B) --> (A B)
(A B),(C D) --> (A B),(C D)
(A B),(B D) --> (A B D)
(A B),(C D),(D B) --> (A B C D)
(H I K),(A B),(C D),(D B),(F G H) --> (H I K F G),(A B C D)
(A H),(H I K),(A B),(C D),(D B),(F G H) --> (A H I K F G B C D)
(H I K),(A B),(C D),(D B),(F G H),(A H) --> (H I K A B C D F G)

```

