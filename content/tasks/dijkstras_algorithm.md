+++
title = "Dijkstra's algorithm"
description = ""
date = 2019-10-05T18:02:25Z
aliases = []
[extra]
id = 11025
[taxonomies]
categories = ["task", "Classic CS problems"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "erlang",
  "go",
  "haskell",
  "huginn",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxima",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sas",
  "scala",
  "sidef",
  "tailspin",
  "tcl",
  "vba",
  "vertex_this_vertex_void",
  "vertexroute_vertex_route_void",
  "zkl",
]
+++

'''Dijkstra's algorithm''', conceived by Dutch computer scientist [[wp:Edsger Dijkstra|Edsger Dijkstra]] in 1956 and published in 1959, is a [[wp:graph search algorithm|graph search algorithm]] that solves the single-source [[wp:shortest path problem|shortest path problem]] for a [[wp:graph (mathematics)|graph]] with non-negative [[wp:edge (graph theory)|edge]] path costs, producing a [[wp:shortest path tree|shortest path tree]].

This algorithm is often used in [[wp:routing|routing]] and as a subroutine in other graph algorithms.


For a given source [[wp:vertex (graph theory)|vertex]] (node) in the graph, the algorithm finds the path with lowest cost (i.e. the shortest path) between that vertex and every other vertex.


;For instance:
If the vertices of the graph represent cities and edge path costs represent driving distances between pairs of cities connected by a direct road, Dijkstra's algorithm can be used to find the shortest route between one city and all other cities.

As a result, the shortest path first is widely used in network  [[wp:routing protocol|routing protocol]]s, most notably:
::*   [[wp:IS-IS|IS-IS]]   (Intermediate System to Intermediate System)   and
::*   [[wp:OSPF|OSPF]]   (Open Shortest Path First).


;Important note:
The inputs to Dijkstra's algorithm are a directed and weighted graph consisting of 2 or more nodes, generally represented by:
::*   an adjacency matrix or list,   and
::*   a start node.

A destination node is not specified.

The output is a set of edges depicting the shortest path to each destination node.

<!--    this is the original example: @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
For the example we start:

```txt

a->b,cost=7,lastNode=a; a->c,cost=9,lastNode=a; a->d,cost=NA,lastNode=a; a->e,cost=NA,lastNode=a; a->f,cost=14,lastNode=a
The lowest cost is a->b so we add a->b to the output. There is a connection from b->d so we update our input to
a->c,cost=9,lastNode=a; a->d,cost=22,lastNode=b; a->e,cost=NA,lastNode=a; a->f,cost=14,lastNode=a
The lowest cost is a->c so we add a->c to the output. Paths to d and f are cheaper via c so we update our input to
a->d,cost=20,lastNode=c; a->e,cost=NA,lastNode=a; a->f,cost=11,lastNode=c
The lowest cost is a->f so we add c->f to the output. We update our input to
a->d,cost=20,lastNode=c; a->e,cost=NA,lastNode=a
The lowest cost is a->d so we add c->d to the output. There is a connection from d->e so we update our input to
a->e,cost=26,lastNode=d
Which just leaves adding d->e to the output.
The output should now be [d->e;c->d;c->f;a->c;a->b]

```

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   !-->

;An example, starting with:
<lang>                                        a─►b, cost=7,  lastNode=a
                                        a─►c, cost=9,  lastNode=a
                                        a─►d, cost=NA, lastNode=a
                                        a─►e, cost=NA, lastNode=a
                                        a─►f, cost=14, lastNode=a

    The lowest cost is    a─►b    so    a─►b    is added to the output.

    There is a connection from   b─►d   so the input is updated to:
                                        a─►c, cost=9,  lastNode=a
                                        a─►d, cost=22, lastNode=b
                                        a─►e, cost=NA, lastNode=a
                                        a─►f, cost=14, lastNode=a

    The lowest cost is    a─►c    so    a─►c    is added to the output.

    Paths to    d    and    f    are cheaper via    c    so the input is updated to:
                                        a─►d, cost=20, lastNode=c
                                        a─►e, cost=NA, lastNode=a
                                        a─►f, cost=11, lastNode=c

    The lowest cost is    a─►f    so    c─►f    is added to the output.

    The input is updated to:
                                        a─►d, cost=20, lastNode=c
                                        a─►e, cost=NA, lastNode=a

    The lowest cost is    a─►d    so    c─►d    is added to the output.

    There is a connection from    d─►e    so the input is updated to:
                                        a─►e, cost=26, lastNode=d

    Which just leaves adding    d─►e    to the output.

    The output should now be:
                                      [ d─►e
                                        c─►d
                                        c─►f
                                        a─►c
                                        a─►b ]
```


## Task

# Implement a version of Dijkstra's algorithm that outputs a set of edges depicting the shortest path to each reachable node from an origin.
# Run your program with the following directed graph starting at node  <big> '''a'''</big>.
# Write a program which interprets the output from the above and use it to output the shortest path from node  <big> '''a''' </big>  to nodes  <big> '''e''' </big> and  <big> '''f''' </big>.

::::::: {| class="wikitable" style="text-align: center; float: left"
|+ Vertices
|-
! Number !! Name
|-
| 1 || a
|-
| 2 || b
|-
| 3 || c
|-
| 4 || d
|-
| 5 || e
|-
| 6 || f
|}
{| class="wikitable" style="text-align: center"
|+ Edges
|-
! Start !! End !! Cost
|-
| a || b || 7
|-
| a || c || 9
|-
| a || f || 14
|-
| b || c || 10
|-
| b || d || 15
|-
| c || d || 11
|-
| c || f || 2
|-
| d || e || 6
|-
| e || f || 9
|}


You can use numbers or names to identify vertices in your program.


## See also

* [https://www.youtube.com/watch?v=cSxnOm5aceA Dijkstra's Algorithm vs. A* Search vs. Concurrent Dijkstra's Algorithm (youtube)]





## Ada

This solution uses a generic package and Ada 2012 (containers, extended return statements, expression functions).
The very convenient 'Img attribute is a GNAT feature.

```Ada
private with Ada.Containers.Ordered_Maps;
generic
   type t_Vertex is (<>);
package Dijkstra is

   type t_Graph is limited private;

   -- Defining a graph (since limited private, only way to do this is to use the Build function)
   type t_Edge is record
      From, To : t_Vertex;
      Weight   : Positive;
   end record;
   type t_Edges is array (Integer range <>) of t_Edge;
   function Build (Edges : in t_Edges; Oriented : in Boolean := True) return t_Graph;

   -- Computing path and distance
   type t_Path is array (Integer range <>) of t_Vertex;
   function Shortest_Path (Graph    : in out t_Graph;
                           From, To : in t_Vertex) return t_Path;
   function Distance      (Graph    : in out t_Graph;
                           From, To : in t_Vertex) return Natural;

private
   package Neighbor_Lists is new Ada.Containers.Ordered_Maps (Key_Type => t_Vertex, Element_Type => Positive);
   type t_Vertex_Data is record
      Neighbors : Neighbor_Lists.Map; -- won't be affected after build
      -- Updated each time a function is called with a new source
      Previous  : t_Vertex;
      Distance  : Natural;
   end record;
   type t_Graph is array (t_Vertex) of t_Vertex_Data;
end Dijkstra;
```



```Ada
with Ada.Containers.Ordered_Sets;
package body Dijkstra is

   Infinite : constant Natural := Natural'Last;

   -- ----- Graph constructor
   function Build (Edges : in t_Edges; Oriented : in Boolean := True) return t_Graph is
   begin
      return Answer : t_Graph := (others => (Neighbors => Neighbor_Lists.Empty_Map,
                                             Previous  => t_Vertex'First,
                                             Distance  => Natural'Last)) do
         for Edge of Edges loop
            Answer(Edge.From).Neighbors.Insert (Key => Edge.To, New_Item => Edge.Weight);
            if not Oriented then
               Answer(Edge.To).Neighbors.Insert (Key => Edge.From, New_Item => Edge.Weight);
            end if;
         end loop;
      end return;
   end Build;

   -- ----- Paths / distances data updating in case of computation request for a new source
   procedure Update_For_Source (Graph : in out t_Graph;
                                From  : in t_Vertex) is
      function Nearer (Left, Right : in t_Vertex) return Boolean is
        (Graph(Left).Distance < Graph(Right).Distance or else
         (Graph(Left).Distance = Graph(Right).Distance and then Left < Right));
      package Ordered is new Ada.Containers.Ordered_Sets (Element_Type => t_Vertex, "<" => Nearer);
      use Ordered;
      Remaining : Set := Empty_Set;
   begin
      -- First, let's check if vertices data are already computed for this source
      if Graph(From).Distance /= 0 then
         -- Reset distances and remaining vertices for a new source
         for Vertex in Graph'range loop
            Graph(Vertex).Distance := (if Vertex = From then 0 else Infinite);
            Remaining.Insert (Vertex);
         end loop;
         -- ----- The Dijkstra algorithm itself
         while not Remaining.Is_Empty
               -- If some targets are not connected to source, at one point, the remaining
               -- distances will all be infinite, hence the folllowing stop condition
               and then Graph(Remaining.First_Element).Distance /= Infinite loop
            declare
               Nearest : constant t_Vertex := Remaining.First_Element;
               procedure Update_Neighbor (Position : in Neighbor_Lists.Cursor) is
                  use Neighbor_Lists;
                  Neighbor     : constant t_Vertex := Key (Position);
                  In_Remaining : Ordered.Cursor    := Remaining.Find (Neighbor);
                  Try_Distance : constant Natural  :=
                    (if In_Remaining = Ordered.No_Element
                     then Infinite -- vertex already reached, this distance will fail the update test below
                     else Graph(Nearest).Distance + Element (Position));
               begin
                  if Try_Distance < Graph(Neighbor).Distance then
                     -- Update distance/path data and reorder the remaining set
                     Remaining.Delete (In_Remaining);
                     Graph(Neighbor).Distance := Try_Distance;
                     Graph(Neighbor).Previous := Nearest;
                     Remaining.Insert (Neighbor);
                  end if;
               end Update_Neighbor;
            begin
               Remaining.Delete_First;
               Graph(Nearest).Neighbors.Iterate (Update_Neighbor'Access);
            end;
         end loop;
      end if;
   end Update_For_Source;

   -- ----- Bodies for the interfaced functions
   function Shortest_Path (Graph    : in out t_Graph;
                           From, To : in t_Vertex) return t_Path is
      function Recursive_Build (From, To : in t_Vertex) return t_Path is
        (if From = To then (1 => From)
         else Recursive_Build(From, Graph(To).Previous) & (1 => To));
   begin
      Update_For_Source (Graph, From);
      if Graph(To).Distance = Infinite then
         raise Constraint_Error with "No path from " & From'Img & " to " & To'Img;
      end if;
      return Recursive_Build (From, To);
   end Shortest_Path;

   function Distance (Graph    : in out t_Graph;
                      From, To : in t_Vertex) return Natural is
   begin
      Update_For_Source (Graph, From);
      return Graph(To).Distance;
   end Distance;

end Dijkstra;
```

The testing main procedure :

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Dijkstra;
procedure Test_Dijkstra is
   subtype t_Tested_Vertices is Character range 'a'..'f';
   package Tested is new Dijkstra (t_Vertex => t_Tested_Vertices);
   use Tested;
   Graph : t_Graph := Build (Edges => (('a', 'b', 7),
                                       ('a', 'c', 9),
                                       ('a', 'f', 14),
                                       ('b', 'c', 10),
                                       ('b', 'd', 15),
                                       ('c', 'd', 11),
                                       ('c', 'f', 2),
                                       ('d', 'e', 6),
                                       ('e', 'f', 9)));
   procedure Display_Path (From, To : in t_Tested_Vertices) is
      function Path_Image (Path : in t_Path; Start : Boolean := True) return String is
        ((if Start then "["
          elsif Path'Length /= 0 then ","
          else "") &
         (if Path'Length = 0 then "]"
          else Path(Path'First) & Path_Image(Path(Path'First+1..Path'Last), Start => False)));
   begin
      Put      ("Path from '" & From & "' to '" & To & "' = ");
      Put_Line (Path_Image (Shortest_Path (Graph, From, To))
                & " distance =" & Distance (Graph, From, To)'Img);
   exception
      when others => Put_Line("no path");
   end Display_Path;
begin
   Display_Path ('a', 'e');
   Display_Path ('a', 'f');
   New_Line;
   for From in t_Tested_Vertices loop
      for To in t_Tested_Vertices loop
         Display_Path (From, To);
      end loop;
   end loop;
end Test_Dijkstra;
```

```txt
Path from 'a' to 'e' = [a,c,d,e] distance = 26
Path from 'a' to 'f' = [a,c,f] distance = 11

Path from 'a' to 'a' = [a] distance = 0
Path from 'a' to 'b' = [a,b] distance = 7
Path from 'a' to 'c' = [a,c] distance = 9
Path from 'a' to 'd' = [a,c,d] distance = 20
Path from 'a' to 'e' = [a,c,d,e] distance = 26
Path from 'a' to 'f' = [a,c,f] distance = 11
Path from 'b' to 'a' = no path
Path from 'b' to 'b' = [b] distance = 0
Path from 'b' to 'c' = [b,c] distance = 10
Path from 'b' to 'd' = [b,d] distance = 15
Path from 'b' to 'e' = [b,d,e] distance = 21
Path from 'b' to 'f' = [b,c,f] distance = 12
Path from 'c' to 'a' = no path
Path from 'c' to 'b' = no path
Path from 'c' to 'c' = [c] distance = 0
Path from 'c' to 'd' = [c,d] distance = 11
Path from 'c' to 'e' = [c,d,e] distance = 17
Path from 'c' to 'f' = [c,f] distance = 2
Path from 'd' to 'a' = no path
Path from 'd' to 'b' = no path
Path from 'd' to 'c' = no path
Path from 'd' to 'd' = [d] distance = 0
Path from 'd' to 'e' = [d,e] distance = 6
Path from 'd' to 'f' = [d,e,f] distance = 15
Path from 'e' to 'a' = no path
Path from 'e' to 'b' = no path
Path from 'e' to 'c' = no path
Path from 'e' to 'd' = no path
Path from 'e' to 'e' = [e] distance = 0
Path from 'e' to 'f' = [e,f] distance = 9
Path from 'f' to 'a' = no path
Path from 'f' to 'b' = no path
Path from 'f' to 'c' = no path
Path from 'f' to 'd' = no path
Path from 'f' to 'e' = no path
Path from 'f' to 'f' = [f] distance = 0
```



## ALGOL 68

'''File: prelude_dijkstras_algorithm.a68'''
```algol68
# -*- coding: utf-8 -*- #

COMMENT REQUIRED BY "prelude_dijkstras_algorithm.a68" CO
  MODE ROUTELEN = ~;
  ROUTELEN route len infinity = max ~;
  PROC route len add = (VERTEX v, ROUTE r)ROUTELEN:
    route len OF v + route len OF r; # or MAX(v,r) #
  MODE VERTEXPAYLOAD = ~;
  PROC dijkstra fix value error = (STRING msg)BOOL:
    (put(stand error, (msg, new line)); FALSE);
#PROVIDES:#
# VERTEX*=~* #
# ROUTE*=~* #
# vertex route*=~* #
END COMMENT

MODE VALVERTEX = STRUCT(
    ROUTELEN route len,
    FLEX[0]ROUTE route,
    ROUTE shortest route,
    VERTEXPAYLOAD vertex data
);

MODE VERTEX = REF VALVERTEX;
MODE VERTEXYIELD = PROC(VERTEX)VOID; # used to "generate" VERTEX path #
PRIO INIT = 1; # The same PRIOrity as +:= etc #
OP INIT = (VERTEX self, VERTEXPAYLOAD vertex data)VERTEX:
  self := (route len infinity, (), NIL, vertex data);

# It may be faster to preallocate "queue", rather then grow a FLEX #
OP +:= = (REF FLEX[]VERTEX in list, VERTEX rhs)REF FLEX[]VERTEX: (
  [UPB in list+1]VERTEX out list;
  out list[:UPB in list] := in list;
  out list[UPB out list] := rhs;
  in list := out list # EXIT #
);

MODE VALROUTE = STRUCT(VERTEX from, to, ROUTELEN route len#, ROUTEPAYLOAD#);
MODE ROUTE = REF VALROUTE;

OP +:= = (REF FLEX[]ROUTE in list, ROUTE rhs)REF FLEX[]ROUTE: (
  [UPB in list+1]ROUTE out list;
  out list[:UPB in list] := in list;
  out list[UPB out list] := rhs;
  in list := out list # EXIT #
);

MODE VERTEXROUTE = UNION(VERTEX, ROUTE);
MODE VERTEXROUTEYIELD = PROC(VERTEXROUTE)VOID;

################################################################
# Finally: now the strong typing is in place, the task code... #
################################################################
PROC vertex route gen dijkstra = (
    VERTEX source, target,
    REF[]VALROUTE route list,
    VERTEXROUTEYIELD yield
  )VOID:(

# initialise the route len for BOTH directions on each route #
  FOR this TO UPB route list DO
    ROUTE route = route list[this];
    route OF from OF route +:= route;
# assume route lens is the same in both directions, this i.e. NO A-B gradient NOR 1-way streets #
    route OF to OF route +:= (HEAP VALROUTE := (to OF route, from OF route, route len OF route))
  OD;

  COMMENT
  Algorithium Performance "about" O(n**2)...
  Optimisations:
       a) bound index in [lwb queue:UPB queue] for search
       b) delay adding vertices until they are actually encountered
  It may be faster to preallocate "queue" vertex list, rather then grow a FLEX
  END COMMENT

  PROC vertex gen nearest = (REF FLEX[]VERTEX queue, VERTEXYIELD yield)VOID: (
    INT vertices done := 0, lwb queue := 1;
    ROUTELEN shortest route len done := -route len infinity;
    WHILE vertices done <= UPB queue ANDF shortest route len done NE route len infinity DO
      ROUTELEN shortest route len := route len infinity;
# skip done elements: #
      FOR this FROM lwb queue TO UPB queue DO
        VERTEX this vertex := queue[this];
        IF NOT(shortest route len done < route len OF this vertex) THEN
          lwb queue := this; # remember for next time #
          break
        FI
      OD;
    break:
# find vertex with shortest path attached #
      FOR this FROM lwb queue TO UPB queue DO VERTEX this vertex := queue[this];
        IF shortest route len done < route len OF this vertex ANDF
           route len OF this vertex < shortest route len THEN
           shortest route len := route len OF this vertex FI
      OD;
# update the other vertices with shortest path found #
      FOR this FROM lwb queue TO UPB queue DO VERTEX this vertex := queue[this];
        IF route len OF this vertex = shortest route len THEN
           vertices done +:= 1; yield(this vertex) FI
      OD;
      shortest route len done := shortest route len
    OD
  );

  route len OF target := 0;
  FLEX[0]VERTEX queue := target;

# FOR VERTEX this vertex IN # vertex gen nearest(queue#) DO (#,
##   (VERTEX this vertex)VOID: (
    FOR this TO UPB route OF this vertex DO ROUTE this route = (route OF this vertex)[this];

    # If this vertex has not been encountered before, then add to queue #
      IF route len OF to OF this route = route len infinity THEN queue +:= to OF this route FI;

      ROUTELEN route len = route len add(this vertex, this route);
      IF route len < route len OF to OF this route THEN
        route len OF to OF this route := route len;
        shortest route OF to OF this route := this route
      FI
    OD;

    IF this vertex IS source THEN done FI
# OD#));
  IF NOT dijkstra fix value error("no path found") THEN stop FI;

############################
# Now: generate the result #
############################
  done: (
    VERTEX this vertex := source;
    WHILE
      yield(this vertex);
      ROUTE this route = shortest route OF this vertex;
  # WHILE # this route ISNT ROUTE(NIL) DO
      yield(this route);
      this vertex := from OF this route
    OD
  )
);

SKIP
```
'''File: test_dijkstras_algorithm.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

CO REQUIRED BY "prelude_dijkstras_algorithm.a68" CO
  MODE ROUTELEN = INT,
  ROUTELEN route len infinity = max int,
  PROC route len add = (VERTEX v, ROUTE r)ROUTELEN:
    route len OF v + route len OF r; # or MAX(v,r) #
  MODE VERTEXPAYLOAD = STRING,
  PROC dijkstra fix value error = (STRING msg)BOOL:
    (put(stand error, (msg, new line)); FALSE);
#PROVIDES:#
# VERTEX*=~* #
# ROUTE*=~* #
# vertex route*=~* #
PR READ "prelude_dijkstras_algorithm.a68" PR;

FORMAT vertex data fmt = $g$;

main:(
  INT upb graph = 6, upb route list = 9;

  HEAP[upb graph]VALVERTEX graph;

# name the key vertices #
  FOR this TO UPB graph DO graph[this] INIT STRING("abcdef"[this]) OD;

# declare some variables of the same name #
  VERTEX a := graph[1], b := graph[2], c := graph[3],
         d := graph[4], e := graph[5], f := graph[6];

# define the graph #
  HEAP FLEX[upb route list]VALROUTE route list := (
      (a, b, 7),  (a, c, 9),  (a, f, 14),
      (b, c, 10), (b, d, 15),
      (c, d, 11), (c, f, 2),
      (d, e, 6),
      (e, f, 9)
  );

# FOR VERTEXROUTE vertex route IN # vertex route gen dijkstra(a, e, route list#) DO #,
##   (VERTEXROUTE vertex route)VOID: (
        CASE vertex route IN
          (VERTEX vertex): printf((vertex data fmt, vertex data OF vertex)),
          (ROUTE route): printf(($" --"g(0)"-> "$, route len OF route))
        ESAC
# OD #));
  print(new line)

# TODO: generate random 100000 VERTEX graph test case and test performance - important #

)
```
'''Output:'''

```txt

a --9-> c --2-> f --9-> e

```



## AutoHotkey


```AutoHotkey
Dijkstra(data, start){
	nodes := [], dist := [], Distance := [], dist := [], prev := [], Q := [], min := "x"
	for each, line in StrSplit(data, "`n" , "`r")
		field := StrSplit(line,"`t"), nodes[field.1] := 1, nodes[field.2] := 1
		, Distance[field.1,field.2] := field.3, Distance[field.2,field.1] := field.3
	dist[start] := 0, prev[start] := ""

	for node in nodes {
		if (node <> start)
			dist[node] := "x"
			, prev[node] := ""
		Q[node] := 1
	}

	while % ObjCount(Q) {
		u := MinDist(Q, dist).2
		for node, val in Q
			if (node = u) {
				q.Remove(node)
				break
			}

		for v, length in Distance[u] {
			alt := dist[u] + length
			if (alt < dist[v])
				dist[v] := alt
				, prev[v] := u
		}
	}
	return [dist, prev]
}
;-----------------------------------------------
MinDist(Q, dist){
	for node , val in Q
		if A_Index=1
			min := dist[node], minNode := node
		else
			min := min < dist[node] ? min : dist[node]	, minNode := min < dist[node] ? minNode : node
	return [min,minNode]
}
ObjCount(Obj){
	for key, val in Obj
		count := A_Index
	return count
}
```

Examples:
```AutoHotkey
data =
(
A	B	7
A	C	9
A	F	14
B	C	10
B	D	15
C	D	11
C	F	2
D	E	6
E	F	9
)

nodes:=[], Distance := []
for each, line in StrSplit(data, "`n" , "`r")
    field := StrSplit(line,"`t"), nodes[field.1] := 1, nodes[field.2] := 1
    , Distance[field.1,field.2] := field.3  , Distance[field.2,field.1] := field.3

for node, v in nodes
    nodeList .= (nodeList?"|":"") node (A_Index=1?"|":"")

Gui, add, Text,, From:
Gui, add, Text, x200 yp, To:
Gui, add, DDL, xs vFrom gSubmit, % nodeList
Gui, add, DDL, x200 yp vTo gSubmit, % nodeList
Gui, add, ListView, xs w340 r6, From|>|To|Distance
Gui, add, Text, vT1 xs w340 r1
Gui, +AlwaysOnTop
Gui, show
Loop 4
	LV_ModifyCol(A_Index, "80 Center")

Submit:
Gui, Submit, NoHide
GuiControl, , T1, % ""
LV_Delete()
if !(From && To) || (From = To)
    return
res := Dijkstra(data, From)	, 	xTo := xFrom := DirectFlight := "" , origin := to
GuiControl, , T1, no routing found
if !res[1, To]              ; no possible route
    return

Routing:
Loop % objCount(nodes)
    for xTo , xFrom in res.2
        if (xTo = To)
        {
			LV_Insert(1,"", xFrom, ">" , xTo, Distance[xFrom , xTo]),	To := xFrom
            if (xFrom = From)
                break, Routing
        }
GuiControl, , T1, % "Total distance = " res.1[origin] . DirectFlight
return

esc::
GuiClose:
ExitApp
return
```

Outputs:
```txt
A	>	C	9
C	>	F	2
F	>	E	9
Total distance = 20
```



## C

The priority queue is implemented as a binary heap. The heap stores an index into its data array, so it can quickly update the weight of an item already in it.


```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef struct {
    int vertex;
    int weight;
} edge_t;

typedef struct {
    edge_t **edges;
    int edges_len;
    int edges_size;
    int dist;
    int prev;
    int visited;
} vertex_t;

typedef struct {
    vertex_t **vertices;
    int vertices_len;
    int vertices_size;
} graph_t;

typedef struct {
    int *data;
    int *prio;
    int *index;
    int len;
    int size;
} heap_t;

void add_vertex (graph_t *g, int i) {
    if (g->vertices_size < i + 1) {
        int size = g->vertices_size * 2 > i ? g->vertices_size * 2 : i + 4;
        g->vertices = realloc(g->vertices, size * sizeof (vertex_t *));
        for (int j = g->vertices_size; j < size; j++)
            g->vertices[j] = NULL;
        g->vertices_size = size;
    }
    if (!g->vertices[i]) {
        g->vertices[i] = calloc(1, sizeof (vertex_t));
        g->vertices_len++;
    }
}

void add_edge (graph_t *g, int a, int b, int w) {
    a = a - 'a';
    b = b - 'a';
    add_vertex(g, a);
    add_vertex(g, b);
    vertex_t *v = g->vertices[a];
    if (v->edges_len >= v->edges_size) {
        v->edges_size = v->edges_size ? v->edges_size * 2 : 4;
        v->edges = realloc(v->edges, v->edges_size * sizeof (edge_t *));
    }
    edge_t *e = calloc(1, sizeof (edge_t));
    e->vertex = b;
    e->weight = w;
    v->edges[v->edges_len++] = e;
}

heap_t *create_heap (int n) {
    heap_t *h = calloc(1, sizeof (heap_t));
    h->data = calloc(n + 1, sizeof (int));
    h->prio = calloc(n + 1, sizeof (int));
    h->index = calloc(n, sizeof (int));
    return h;
}

void push_heap (heap_t *h, int v, int p) {
    int i = h->index[v] == 0 ? ++h->len : h->index[v];
    int j = i / 2;
    while (i > 1) {
        if (h->prio[j] < p)
            break;
        h->data[i] = h->data[j];
        h->prio[i] = h->prio[j];
        h->index[h->data[i]] = i;
        i = j;
        j = j / 2;
    }
    h->data[i] = v;
    h->prio[i] = p;
    h->index[v] = i;
}

int min (heap_t *h, int i, int j, int k) {
    int m = i;
    if (j <= h->len && h->prio[j] < h->prio[m])
        m = j;
    if (k <= h->len && h->prio[k] < h->prio[m])
        m = k;
    return m;
}

int pop_heap (heap_t *h) {
    int v = h->data[1];
    int i = 1;
    while (1) {
        int j = min(h, h->len, 2 * i, 2 * i + 1);
        if (j == h->len)
            break;
        h->data[i] = h->data[j];
        h->prio[i] = h->prio[j];
        h->index[h->data[i]] = i;
        i = j;
    }
    h->data[i] = h->data[h->len];
    h->prio[i] = h->prio[h->len];
    h->index[h->data[i]] = i;
    h->len--;
    return v;
}

void dijkstra (graph_t *g, int a, int b) {
    int i, j;
    a = a - 'a';
    b = b - 'a';
    for (i = 0; i < g->vertices_len; i++) {
        vertex_t *v = g->vertices[i];
        v->dist = INT_MAX;
        v->prev = 0;
        v->visited = 0;
    }
    vertex_t *v = g->vertices[a];
    v->dist = 0;
    heap_t *h = create_heap(g->vertices_len);
    push_heap(h, a, v->dist);
    while (h->len) {
        i = pop_heap(h);
        if (i == b)
            break;
        v = g->vertices[i];
        v->visited = 1;
        for (j = 0; j < v->edges_len; j++) {
            edge_t *e = v->edges[j];
            vertex_t *u = g->vertices[e->vertex];
            if (!u->visited && v->dist + e->weight <= u->dist) {
                u->prev = i;
                u->dist = v->dist + e->weight;
                push_heap(h, e->vertex, u->dist);
            }
        }
    }
}

void print_path (graph_t *g, int i) {
    int n, j;
    vertex_t *v, *u;
    i = i - 'a';
    v = g->vertices[i];
    if (v->dist == INT_MAX) {
        printf("no path\n");
        return;
    }
    for (n = 1, u = v; u->dist; u = g->vertices[u->prev], n++)
        ;
    char *path = malloc(n);
    path[n - 1] = 'a' + i;
    for (j = 0, u = v; u->dist; u = g->vertices[u->prev], j++)
        path[n - j - 2] = 'a' + u->prev;
    printf("%d %.*s\n", v->dist, n, path);
}

int main () {
    graph_t *g = calloc(1, sizeof (graph_t));
    add_edge(g, 'a', 'b', 7);
    add_edge(g, 'a', 'c', 9);
    add_edge(g, 'a', 'f', 14);
    add_edge(g, 'b', 'c', 10);
    add_edge(g, 'b', 'd', 15);
    add_edge(g, 'c', 'd', 11);
    add_edge(g, 'c', 'f', 2);
    add_edge(g, 'd', 'e', 6);
    add_edge(g, 'e', 'f', 9);
    dijkstra(g, 'a', 'e');
    print_path(g, 'e');
    return 0;
}
```


output
```txt
26 acde

```



## C++

(Modified from [http://en.literateprograms.org/Dijkstra%27s_algorithm_%28C_Plus_Plus%29 LiteratePrograms], which is MIT/X11 licensed.)

Solution follows Dijkstra's algorithm as described elsewhere. Data like min-distance, previous node, neighbors, are kept in separate data structures instead of part of the vertex. We number the vertexes starting from 0, and represent the graph using an adjacency list (vector whose i'th element is the vector of neighbors that vertex i has edges to) for simplicity.

For the priority queue of vertexes, we use a self-balancing binary search tree (<code>std::set</code>), which should bound time complexity by O(E log V). Although C++ has heaps, without knowing the index of an element it would take linear time to find it to re-order it for a changed weight. It is not easy to keep the index of vertexes in the heap because the heap operations are opaque without callbacks. On the other hand, using a self-balancing binary search tree is efficient because it has the same log(n) complexity for insertion and removal of the head element as a binary heap. In addition, a self-balancing binary search tree also allows us to find and remove any other element in log(n) time, allowing us to perform the decrease-key step in logarithmic time by removing and re-inserting.

We do not need to keep track of whether a vertex is "done" ("visited") as in the Wikipedia description, since re-reaching such a vertex will always fail the relaxation condition (when re-reaching a "done" vertex, the new distance will never be ''less'' than it was originally), so it will be skipped anyway.

The time complexity of this algorithm is O(E log V), as described on Wikipedia. Each vertex is added to the priority queue at most once (re-ordering doesn't count as adding), because once it's in the priority queue, we only re-order it, never add it again. And when it's popped from the priority queue, that means we already have the real minimum distance to this vertex, so the relaxation condition will always fail in the future for this vertex, and it will never be added to the priority queue again. Therefore, we will only pop each vertex at most once from the priority queue, and the size of the priority queue is bounded by V (the number of vertexes).

The outer loop executes once for each element popped from the priority queue, so it will execute at most once for each vertex, so at most V times. Each iteration of the outer loop executes one pop from the priority queue, which has time complexity O(log V). The inner loop executes at most once for each directed edge, since each directed edge has one originating vertex, and there is only at most one iteration of the outer loop for each vertex. Each iteration of the inner loop potentially performs one push or re-order on the priority queue (where re-order is a pop and a push), which has complexity O(log V). There is also the O(V) complexity for initializing the data structures. Combining these, we have a complexity of O(V log V + E log V), and assuming this is a connected graph, V <= E+1 = O(E), so we can write it as O(E log V).


```cpp
#include <iostream>
#include <vector>
#include <string>
#include <list>

#include <limits> // for numeric_limits

#include <set>
#include <utility> // for pair
#include <algorithm>
#include <iterator>


typedef int vertex_t;
typedef double weight_t;

const weight_t max_weight = std::numeric_limits<double>::infinity();

struct neighbor {
    vertex_t target;
    weight_t weight;
    neighbor(vertex_t arg_target, weight_t arg_weight)
        : target(arg_target), weight(arg_weight) { }
};

typedef std::vector<std::vector<neighbor> > adjacency_list_t;


void DijkstraComputePaths(vertex_t source,
                          const adjacency_list_t &adjacency_list,
                          std::vector<weight_t> &min_distance,
                          std::vector<vertex_t> &previous)
{
    int n = adjacency_list.size();
    min_distance.clear();
    min_distance.resize(n, max_weight);
    min_distance[source] = 0;
    previous.clear();
    previous.resize(n, -1);
    std::set<std::pair<weight_t, vertex_t> > vertex_queue;
    vertex_queue.insert(std::make_pair(min_distance[source], source));

    while (!vertex_queue.empty())
    {
        weight_t dist = vertex_queue.begin()->first;
        vertex_t u = vertex_queue.begin()->second;
        vertex_queue.erase(vertex_queue.begin());

        // Visit each edge exiting u
	const std::vector<neighbor> &neighbors = adjacency_list[u];
        for (std::vector<neighbor>::const_iterator neighbor_iter = neighbors.begin();
             neighbor_iter != neighbors.end();
             neighbor_iter++)
        {
            vertex_t v = neighbor_iter->target;
            weight_t weight = neighbor_iter->weight;
            weight_t distance_through_u = dist + weight;
	    if (distance_through_u < min_distance[v]) {
	        vertex_queue.erase(std::make_pair(min_distance[v], v));

	        min_distance[v] = distance_through_u;
	        previous[v] = u;
	        vertex_queue.insert(std::make_pair(min_distance[v], v));

	    }

        }
    }
}


std::list<vertex_t> DijkstraGetShortestPathTo(
    vertex_t vertex, const std::vector<vertex_t> &previous)
{
    std::list<vertex_t> path;
    for ( ; vertex != -1; vertex = previous[vertex])
        path.push_front(vertex);
    return path;
}


int main()
{
    // remember to insert edges both ways for an undirected graph
    adjacency_list_t adjacency_list(6);
    // 0 = a
    adjacency_list[0].push_back(neighbor(1, 7));
    adjacency_list[0].push_back(neighbor(2, 9));
    adjacency_list[0].push_back(neighbor(5, 14));
    // 1 = b
    adjacency_list[1].push_back(neighbor(0, 7));
    adjacency_list[1].push_back(neighbor(2, 10));
    adjacency_list[1].push_back(neighbor(3, 15));
    // 2 = c
    adjacency_list[2].push_back(neighbor(0, 9));
    adjacency_list[2].push_back(neighbor(1, 10));
    adjacency_list[2].push_back(neighbor(3, 11));
    adjacency_list[2].push_back(neighbor(5, 2));
    // 3 = d
    adjacency_list[3].push_back(neighbor(1, 15));
    adjacency_list[3].push_back(neighbor(2, 11));
    adjacency_list[3].push_back(neighbor(4, 6));
    // 4 = e
    adjacency_list[4].push_back(neighbor(3, 6));
    adjacency_list[4].push_back(neighbor(5, 9));
    // 5 = f
    adjacency_list[5].push_back(neighbor(0, 14));
    adjacency_list[5].push_back(neighbor(2, 2));
    adjacency_list[5].push_back(neighbor(4, 9));

    std::vector<weight_t> min_distance;
    std::vector<vertex_t> previous;
    DijkstraComputePaths(0, adjacency_list, min_distance, previous);
    std::cout << "Distance from 0 to 4: " << min_distance[4] << std::endl;
    std::list<vertex_t> path = DijkstraGetShortestPathTo(4, previous);
    std::cout << "Path : ";
    std::copy(path.begin(), path.end(), std::ostream_iterator<vertex_t>(std::cout, " "));
    std::cout << std::endl;

    return 0;
}
```


----

Note that it ''is'' possible to use C++ built-in heaps (or the abstract <code>std::priority_queue</code> datatype) to implement this without changing the time complexity. Although the previous section noted that, without knowing the position of the element in the heap, it would take linear time to search for it in order to re-order it, the trick here is that we can insert the new updated element (with the vertex and updated lower distance), and simply leave the old element (with the vertex and old higher distance) in the priority queue without removing it, thereby eliminating the need to find it.

Since we now leave multiple elements with the same vertex in the priority queue, in order to ensure we still only process a vertex's edges only once, we add a check when we retrieve an element from the priority queue, to check whether its distance is greater than the known minimum distance to that vertex. If this element is the most updated version for this vertex (i.e. the vertex's minimum distance has not been decreased since this element was added to the priority queue), then its distance must be equal to the current known minimum distance, since we only update the minimum distance in the decrease-key step. So if the element's distance is greater, we know that this is not the most updated version for this vertex -- i.e. we have already processed the edges for this vertex -- and we should ignore it.

The only downside to this strategy is that many old "garbage" elements will be left in the priority queue, increasing its size, and thus also increasing the time it takes to push and pop, as well as increasing the number of times we have to pop. However, we argue that the time complexity remains the same.

The main difference with the time complexity analysis for the previous algorithm is that here, we may add a vertex to the priority queue more than once. However, it is still true that the inner loop executes at most once for each directed edge. This is because in the outer loop, we added a check to ignore vertexes that we've already processed, so we will still only proceed down to the processing the edges at most once for each vertex. Therefore, the number of times that push is done on the priority queue (which happens at most once per iteration of the inner loop) is bounded by E, and the size of the priority queue is also bounded by E.

The number of times the outer loop executes (the number of times an element is popped from the priority queue) is bounded by E, and in each iteration, the popping operation takes time complexity O(log E). The number of times the inner loop executes is also bounded by E, and the pushing operation inside it also takes time complexity O(log E). So in total, the time complexity is O(E log E). But not that, for a simple graph, E < V^2, so log E < 2 log V = O(log V). So O(E log E) can also be written as O(E log V), which is the same as for the preceding algorithm.


```cpp
#include <iostream>
#include <vector>
#include <string>
#include <list>

#include <limits> // for numeric_limits

#include <queue>
#include <utility> // for pair
#include <algorithm>
#include <iterator>


typedef int vertex_t;
typedef double weight_t;

const weight_t max_weight = std::numeric_limits<double>::infinity();

struct neighbor {
    vertex_t target;
    weight_t weight;
    neighbor(vertex_t arg_target, weight_t arg_weight)
        : target(arg_target), weight(arg_weight) { }
};

typedef std::vector<std::vector<neighbor> > adjacency_list_t;
typedef std::pair<weight_t, vertex_t> weight_vertex_pair_t;

void DijkstraComputePaths(vertex_t source,
                          const adjacency_list_t &adjacency_list,
                          std::vector<weight_t> &min_distance,
                          std::vector<vertex_t> &previous)
{
    int n = adjacency_list.size();
    min_distance.clear();
    min_distance.resize(n, max_weight);
    min_distance[source] = 0;
    previous.clear();
    previous.resize(n, -1);
    // we use greater instead of less to turn max-heap into min-heap
    std::priority_queue<weight_vertex_pair_t,
			std::vector<weight_vertex_pair_t>,
			std::greater<weight_vertex_pair_t> > vertex_queue;
    vertex_queue.push(std::make_pair(min_distance[source], source));

    while (!vertex_queue.empty())
    {
        weight_t dist = vertex_queue.top().first;
        vertex_t u = vertex_queue.top().second;
        vertex_queue.pop();

	// Because we leave old copies of the vertex in the priority queue
	// (with outdated higher distances), we need to ignore it when we come
	// across it again, by checking its distance against the minimum distance
	if (dist > min_distance[u])
	    continue;

        // Visit each edge exiting u
	const std::vector<neighbor> &neighbors = adjacency_list[u];
        for (std::vector<neighbor>::const_iterator neighbor_iter = neighbors.begin();
             neighbor_iter != neighbors.end();
             neighbor_iter++)
        {
            vertex_t v = neighbor_iter->target;
            weight_t weight = neighbor_iter->weight;
            weight_t distance_through_u = dist + weight;
	    if (distance_through_u < min_distance[v]) {
	        min_distance[v] = distance_through_u;
	        previous[v] = u;
	        vertex_queue.push(std::make_pair(min_distance[v], v));

	    }

        }
    }
}


std::list<vertex_t> DijkstraGetShortestPathTo(
    vertex_t vertex, const std::vector<vertex_t> &previous)
{
    std::list<vertex_t> path;
    for ( ; vertex != -1; vertex = previous[vertex])
        path.push_front(vertex);
    return path;
}


int main()
{
    // remember to insert edges both ways for an undirected graph
    adjacency_list_t adjacency_list(6);
    // 0 = a
    adjacency_list[0].push_back(neighbor(1, 7));
    adjacency_list[0].push_back(neighbor(2, 9));
    adjacency_list[0].push_back(neighbor(5, 14));
    // 1 = b
    adjacency_list[1].push_back(neighbor(0, 7));
    adjacency_list[1].push_back(neighbor(2, 10));
    adjacency_list[1].push_back(neighbor(3, 15));
    // 2 = c
    adjacency_list[2].push_back(neighbor(0, 9));
    adjacency_list[2].push_back(neighbor(1, 10));
    adjacency_list[2].push_back(neighbor(3, 11));
    adjacency_list[2].push_back(neighbor(5, 2));
    // 3 = d
    adjacency_list[3].push_back(neighbor(1, 15));
    adjacency_list[3].push_back(neighbor(2, 11));
    adjacency_list[3].push_back(neighbor(4, 6));
    // 4 = e
    adjacency_list[4].push_back(neighbor(3, 6));
    adjacency_list[4].push_back(neighbor(5, 9));
    // 5 = f
    adjacency_list[5].push_back(neighbor(0, 14));
    adjacency_list[5].push_back(neighbor(2, 2));
    adjacency_list[5].push_back(neighbor(4, 9));

    std::vector<weight_t> min_distance;
    std::vector<vertex_t> previous;
    DijkstraComputePaths(0, adjacency_list, min_distance, previous);
    std::cout << "Distance from 0 to 4: " << min_distance[4] << std::endl;
    std::list<vertex_t> path = DijkstraGetShortestPathTo(4, previous);
    std::cout << "Path : ";
    std::copy(path.begin(), path.end(), std::ostream_iterator<vertex_t>(std::cout, " "));
    std::cout << std::endl;

    return 0;
}
```



## C#

```c#
using static System.Linq.Enumerable;
using static System.String;
using static System.Console;
using System.Collections.Generic;
using System;
using EdgeList = System.Collections.Generic.List<(int node, double weight)>;

public static class Dijkstra
{
    public static void Main() {
        Graph graph = new Graph(6);
        Func<char, int> id = c => c - 'a';
        Func<int , char> name = i => (char)(i + 'a');
        foreach (var (start, end, cost) in new [] {
            ('a', 'b', 7),
            ('a', 'c', 9),
            ('a', 'f', 14),
            ('b', 'c', 10),
            ('b', 'd', 15),
            ('c', 'd', 11),
            ('c', 'f', 2),
            ('d', 'e', 6),
            ('e', 'f', 9),
        }) {
            graph.AddEdge(id(start), id(end), cost);
        }

        var path = graph.FindPath(id('a'));
        for (int d = id('b'); d <= id('f'); d++) {
            WriteLine(Join(" -> ", Path(id('a'), d).Select(p => $"{name(p.node)}({p.distance})").Reverse()));
        }

        IEnumerable<(double distance, int node)> Path(int start, int destination) {
            yield return (path[destination].distance, destination);
            for (int i = destination; i != start; i = path[i].prev) {
                yield return (path[path[i].prev].distance, path[i].prev);
            }
        }
    }

}

sealed class Graph
{
    private readonly List<EdgeList> adjacency;

    public Graph(int vertexCount) => adjacency = Range(0, vertexCount).Select(v => new EdgeList()).ToList();

    public int Count => adjacency.Count;
    public bool HasEdge(int s, int e) => adjacency[s].Any(p => p.node == e);
    public bool RemoveEdge(int s, int e) => adjacency[s].RemoveAll(p => p.node == e) > 0;

    public bool AddEdge(int s, int e, double weight) {
        if (HasEdge(s, e)) return false;
        adjacency[s].Add((e, weight));
        return true;
    }

    public (double distance, int prev)[] FindPath(int start) {
        var info = Range(0, adjacency.Count).Select(i => (distance: double.PositiveInfinity, prev: i)).ToArray();
        info[start].distance = 0;
        var visited = new System.Collections.BitArray(adjacency.Count);

        var heap = new Heap<(int node, double distance)>((a, b) => a.distance.CompareTo(b.distance));
        heap.Push((start, 0));
        while (heap.Count > 0) {
            var current = heap.Pop();
            if (visited[current.node]) continue;
            var edges = adjacency[current.node];
            for (int n = 0; n < edges.Count; n++) {
                int v = edges[n].node;
                if (visited[v]) continue;
                double alt = info[current.node].distance + edges[n].weight;
                if (alt < info[v].distance) {
                    info[v] = (alt, current.node);
                    heap.Push((v, alt));
                }
            }
            visited[current.node] = true;
        }
        return info;
    }

}

sealed class Heap<T>
{
    private readonly IComparer<T> comparer;
    private readonly List<T> list = new List<T> { default };

    public Heap() : this(default(IComparer<T>)) { }

    public Heap(IComparer<T> comparer) {
        this.comparer = comparer ?? Comparer<T>.Default;
    }

    public Heap(Comparison<T> comparison) : this(Comparer<T>.Create(comparison)) { }

    public int Count => list.Count - 1;

    public void Push(T element) {
        list.Add(element);
        SiftUp(list.Count - 1);
    }

    public T Pop() {
        T result = list[1];
        list[1] = list[list.Count - 1];
        list.RemoveAt(list.Count - 1);
        SiftDown(1);
        return result;
    }

    private static int Parent(int i) => i / 2;
    private static int Left(int i) => i * 2;
    private static int Right(int i) => i * 2 + 1;

    private void SiftUp(int i) {
        while (i > 1) {
            int parent = Parent(i);
            if (comparer.Compare(list[i], list[parent]) > 0) return;
            (list[parent], list[i]) = (list[i], list[parent]);
            i = parent;
        }
    }

    private void SiftDown(int i) {
        for (int left = Left(i); left < list.Count; left = Left(i)) {
            int smallest = comparer.Compare(list[left], list[i]) <= 0 ? left : i;
            int right = Right(i);
            if (right < list.Count && comparer.Compare(list[right], list[smallest]) <= 0) smallest = right;
            if (smallest == i) return;
            (list[i], list[smallest]) = (list[smallest], list[i]);
            i = smallest;
        }
    }

}
```

```txt

a(0) -> b(7)
a(0) -> c(9)
a(0) -> c(9) -> d(20)
a(0) -> c(9) -> d(20) -> e(26)
a(0) -> c(9) -> f(11)

```



## Clojure


```lisp

; '''Priority Queue'''
(require '[clojure.data.priority-map :refer [priority-map priority-map-keyfn]])

; '''Modification of Djikstra''' [[Media:https://www.ummels.de/2014/06/08/dijkstra-in-clojure/]]
(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn min2 [v1 v2]
  (if (< (first v1) (first v2))
    v1
    v2))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (graph n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start graph]
  (loop [q (priority-map-keyfn first start [0 nil]),
         r {}]
    (if-let [[v [d u]] (peek q)]
      (let [dist (-> (graph v)
                     (remove-keys r)
                     (map-vals (fn [cost] (let [new-cost (+ d cost)] [new-cost v]))))]
        (recur (merge-with min2 (pop q) dist) (assoc r v [d u])))
      r)))

(defn make-adj-list
  "Convert a list of nodes and a list of edges into an
  adjacency list structure.  For example: with N [1 2 3],
  E [[1 2] [2 3] [1 3]], the result is {1 [2 3], 2 [3], 3 []}"
    [Nodes Edges]
  (let [init-graph (reduce merge (map #(hash-map %1 {}) Nodes))]
    (reduce #(merge-with merge %1 %2)
            init-graph
            (map #(hash-map (nth % 0) (hash-map (nth % 1) (nth % 2))) Edges))))

(defn path-to [goal dik]
  (if (contains? dik goal)
    (reverse (take-while identity (iterate (comp second dik) goal)))
    nil))

(defn cost-to [goal dik]
  (if (contains? dik goal)
    (first (dik goal))
    -1))
----
; '''Test'''

(def nodes ["a", "b", "c", "d", "e", "f"])
(def edges [["a", "b", 7],  ["a", "c", 9],  ["a", "f", 14], ["b", "c", 10],
            ["b", "d", 15], ["c", "d", 11], ["c", "f", 2],  ["d", "e", 6],
            ["e", "f", 9]])
----
; Create Graph
(def g (make-adj-list nodes edges))
; Dijstra from "a" to all other nodes
(def dij (dijkstra "a" g))

; '''Show path to "e"'''
(println (path-to "e" dij))

; '''Cost to "e""'''
(println (cost-to "e" dij))

```

```txt
(a c d e)
26
```



## Common Lisp


```lisp

(defparameter *w* '((a (a b . 7) (a c . 9) (a f . 14))
                    (b (b c . 10) (b d . 15))
                    (c (c d . 11) (c f . 2))
                    (d (d e . 6))
                    (e (e f . 9))))

(defvar *r* nil)

(defun dijkstra-short-path (i g)
  (setf *r* nil) (paths i g 0 `(,i))
  (car (sort *r* #'< :key #'cadr)))

(defun paths (c g z v)
  (if (eql c g) (push `(,(reverse v) ,z) *r*)
      (loop for a in (nodes c) for b = (cadr a) do
            (unless (member b v)
              (paths b g (+ (cddr a) z) (cons b v))))))

(defun nodes (c)
  (sort (cdr (assoc c *w*)) #'< :key #'cddr))

```

```txt

> (dijkstra-short-path 'a 'e)
((A C D E) 26)

```


```lisp

(defvar *r* nil)

(defun dijkstra-short-paths (z w)
  (loop for (a b) in (loop for v on z nconc
                           (loop for e in (cdr v)
                                 collect `(,(car v) ,e)))
        do (setf *r* nil) (paths w a b 0 `(,a))
        (format t "~{Path: ~A  Distance: ~A~}~%"
                (car (sort *r* #'< :key #'cadr)))))

(defun paths (w c g z v)
  (if (eql c g) (push `(,(reverse v) ,z) *r*)
      (loop for a in (sort (cdr (assoc c w)) #'< :key #'cddr)
            for b = (cadr a) do (unless (member b v)
                                  (paths w b g (+ (cddr a) z)
                                         (cons b v))))))

```

```txt

> (dijkstra-short-paths
   '(a b c d e f)
   '((a (a b . 7) (a c . 9) (a f . 14))
     (b (b c . 10) (b d . 15))
     (c (c d . 11) (c f . 2))
     (d (d e . 6))
     (e (e f . 9))))
Path: (A B)  Distance: 7
Path: (A C)  Distance: 9
Path: (A C D)  Distance: 20
Path: (A C D E)  Distance: 26
Path: (A C F)  Distance: 11
Path: (B C)  Distance: 10
Path: (B D)  Distance: 15
Path: (B D E)  Distance: 21
Path: (B C F)  Distance: 12
Path: (C D)  Distance: 11
Path: (C D E)  Distance: 17
Path: (C F)  Distance: 2
Path: (D E)  Distance: 6
Path: (D E F)  Distance: 15
Path: (E F)  Distance: 9
NIL

```



## D

The algorithm and the important data structures are essentially the same as in the C++ version, so the same comments apply (built-in D associative arrays are unsorted).

```d
import std.stdio, std.typecons, std.algorithm, std.container;

alias Vertex = string;
alias Weight = int;

struct Neighbor {
    Vertex target;
    Weight weight;
}

alias AdjacencyMap = Neighbor[][Vertex];

pure dijkstraComputePaths(Vertex source, Vertex target, AdjacencyMap adjacencyMap){
    Weight[Vertex] minDistance;
    Vertex[Vertex] previous;

    foreach(v, neighs; adjacencyMap){
        minDistance[v] = Weight.max;
        foreach(n; neighs) minDistance[n.target] = Weight.max;
    }

    minDistance[source] = 0;
    auto vertexQueue = redBlackTree(tuple(minDistance[source], source));

    foreach(_, u; vertexQueue){
        if (u == target)
            break;

        // Visit each edge exiting u.
        foreach(n; adjacencyMap.get(u, null)){
            const v = n.target;
            const distanceThroughU = minDistance[u] + n.weight;
            if(distanceThroughU < minDistance[v]){
                vertexQueue.removeKey(tuple(minDistance[v], v));
                minDistance[v] = distanceThroughU;
                previous[v] = u;
                vertexQueue.insert(tuple(minDistance[v], v));
            }
        }
    }

    return tuple(minDistance, previous);
}

pure dijkstraGetShortestPathTo(Vertex v, Vertex[Vertex] previous){
    Vertex[] path = [v];

    while (v in previous) {
        v = previous[v];
        if (v == path[$ - 1])
            break;
        path ~= v;
    }

    path.reverse();
    return path;
}

void main() {
    immutable arcs = [tuple("a", "b", 7),
                      tuple("a", "c", 9),
                      tuple("a", "f", 14),
                      tuple("b", "c", 10),
                      tuple("b", "d", 15),
                      tuple("c", "d", 11),
                      tuple("c", "f", 2),
                      tuple("d", "e", 6),
                      tuple("e", "f", 9)];

    AdjacencyMap adj;
    foreach (immutable arc; arcs) {
        adj[arc[0]] ~= Neighbor(arc[1], arc[2]);
        // Add this if you want an undirected graph:
        //adj[arc[1]] ~= Neighbor(arc[0], arc[2]);
    }

    const minDist_prev = dijkstraComputePaths("a", "e", adj);
    const minDistance = minDist_prev[0];
    const previous = minDist_prev[1];

    writeln(`Distance from "a" to "e": `, minDistance["e"]);
    writeln("Path: ", dijkstraGetShortestPathTo("e", previous));
}
```

```txt
Distance from "a" to "e": 26
Path: ["a", "c", "d", "e"]
```



## Erlang


```erlang

-module(dijkstra).
-include_lib("eunit/include/eunit.hrl").
-export([dijkstrafy/3]).

% just hide away recursion so we have a nice interface
dijkstrafy(Graph, Start, End) when is_map(Graph) ->
	shortest_path(Graph, [{0, [Start]}], End, #{}).

shortest_path(_Graph, [], _End, _Visited) ->
	% if we're not going anywhere, it's time to start going back
	{0, []};
shortest_path(_Graph, [{Cost, [End | _] = Path} | _ ], End, _Visited) ->
	% this is the base case, and finally returns the distance and the path
	{Cost, lists:reverse(Path)};
shortest_path(Graph, [{Cost, [Node | _ ] = Path} | Routes], End, Visited) ->
	% this is the recursive case.
	% here we build a list of new "unvisited" routes, where the stucture is
	% a tuple of cost, then a list of paths taken to get to that cost from the "Start"
	NewRoutes = [{Cost + NewCost, [NewNode | Path]}
		|| {NewCost, NewNode} <- maps:get(Node, Graph),
			not maps:get(NewNode, Visited, false)],
	shortest_path(
		Graph,
		% add the routes we ripped off earlier onto the new routes
		% that we want to visit. sort the list of routes to get the
		% shortest routes (lowest cost) at the beginning.
		% Erlangs sort is already good enough, and it will sort the
		% tuples by the number at the beginning of each (the cost).
		lists:sort(NewRoutes ++ Routes),
		End,
		Visited#{Node => true}
	).

basic_test() ->
	Graph = #{
		a => [{7,b},{9,c},{14,f}],
		b => [{7,a},{10,c},{15,d}],
		c => [{10,b},{9,c},{11,d},{2,f}],
		d => [{15,b},{6,e},{11,c}],
		e => [{9,f},{6,d}],
		f => [{14,f},{2,c},{9,e}]
	},
	{Cost, Path}   = dijkstrafy(Graph, a, e),
	{20,[a,c,f,e]} = {Cost, Path},
	io:format(user, "The total cost was ~p and the path was: ", [Cost]),
	io:format(user, "~w~n", [Path]).

```


```txt

$ ./rebar3 eunit
===> Verifying dependencies...
===> Compiling dijkstra
===> Performing EUnit tests...
The total cost was 20 and the path was: [a,c,f,e]
  Test passed.

```


=={{header|F_Sharp|F#}}==
===Dijkstra's algorithm===

```fsharp

//Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
[<CustomEquality;CustomComparison>]
type Dijkstra<'N,'G when 'G:comparison>={toN:'N;cost:Option<'G>;fromN:'N}
                                        override g.Equals n =match n with| :? Dijkstra<'N,'G> as n->n.cost=g.cost|_->false
                                        override g.GetHashCode() = hash g.cost
                                        interface System.IComparable with
                                          member n.CompareTo g =
                                            match g with
                                            | :? Dijkstra<'N,'G> as n when n.cost=None -> (-1)
                                            | :? Dijkstra<'N,'G>      when n.cost=None -> 1
                                            | :? Dijkstra<'N,'G> as g                  -> compare n.cost g.cost
                                            | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"
let inline Dijkstra N G y =
  let rec fN l f=
    if List.isEmpty l then f
    else let n=List.min l
         if n.cost=None then f else
         fN(l|>List.choose(fun n'->if n'.toN=n.toN then None else match n.cost,n'.cost,Map.tryFind (n.toN,n'.toN) G with
                                                                  |Some g,None,Some wg                ->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                                                                  |Some g,Some g',Some wg when g+wg<g'->Some {toN=n'.toN;cost=Some(g+wg);fromN=n.toN}
                                                                  |_                                  ->Some n'))((n.fromN,n.toN)::f)
  let r = fN (N|>List.map(fun n->{toN=n;cost=(Map.tryFind(y,n)G);fromN=y})) []
  (fun n->let rec fN z l=match List.tryFind(fun (_,g)->g=z) r with
                         |Some(n',g') when y=n'->Some(n'::g'::l)
                         |Some(n',g')          ->fN n' (g'::l)
                         |_                    ->None
          fN n [])

```



### The Task


```fsharp

type Node= |A|B|C|D|E|F
let G=Map[((A,B),7);((A,C),9);((A,F),14);((B,C),10);((B,D),15);((C,D),11);((C,F),2);((D,E),6);((E,F),9)]
let paths=Dijkstra [B;C;D;E;F] G A
printfn "%A" (paths E)
printfn "%A" (paths F)

```

```txt

Some [A; C; D; E]
Some [A; C; F]

```



## Go


```go
package main

import (
  "container/heap"
  "fmt"
)

// A PriorityQueue implements heap.Interface and holds Items.
type PriorityQueue struct {
  items []Vertex
  // value to index
  m map[Vertex]int
  // value to priority
  pr map[Vertex]int
}

func (pq *PriorityQueue) Len() int           { return len(pq.items) }
func (pq *PriorityQueue) Less(i, j int) bool { return pq.pr[pq.items[i]] < pq.pr[pq.items[j]] }
func (pq *PriorityQueue) Swap(i, j int) {
  pq.items[i], pq.items[j] = pq.items[j], pq.items[i]
  pq.m[pq.items[i]] = i
  pq.m[pq.items[j]] = j
}
func (pq *PriorityQueue) Push(x interface{}) {
  n := len(pq.items)
  item := x.(Vertex)
  pq.m[item] = n
  pq.items = append(pq.items, item)
}
func (pq *PriorityQueue) Pop() interface{} {
  old := pq.items
  n := len(old)
  item := old[n-1]
  pq.m[item] = -1
  pq.items = old[0 : n-1]
  return item
}

// update modifies the priority of an item in the queue.
func (pq *PriorityQueue) update(item Vertex, priority int) {
  pq.pr[item] = priority
  heap.Fix(pq, pq.m[item])
}
func (pq *PriorityQueue) addWithPriority(item Vertex, priority int) {
  heap.Push(pq, item)
  pq.update(item, priority)
}

const (
  Infinity      = int(^uint(0) >> 1)
  Uninitialized = -1
)

func Dijkstra(g Graph, source Vertex) (dist map[Vertex]int, prev map[Vertex]Vertex) {
  dist = make(map[Vertex]int)
  prev = make(map[Vertex]Vertex)
  sid := source
  dist[sid] = 0
  q := &PriorityQueue{[]Vertex{}, make(map[Vertex]int), make(map[Vertex]int)}
  for _, v := range g.Vertices() {
    if v != sid {
      dist[v] = Infinity
    }
    prev[v] = Uninitialized
    q.addWithPriority(v, dist[v])
  }
  for len(q.items) != 0 {
    u := heap.Pop(q).(Vertex)
    for _, v := range g.Neighbors(u) {
      alt := dist[u] + g.Weight(u, v)
      if alt < dist[v] {
        dist[v] = alt
        prev[v] = u
        q.update(v, alt)
      }
    }
  }
  return dist, prev
}

// A Graph is the interface implemented by graphs that
// this algorithm can run on.
type Graph interface {
  Vertices() []Vertex
  Neighbors(v Vertex) []Vertex
  Weight(u, v Vertex) int
}

// Nonnegative integer ID of vertex
type Vertex int

// sg is a graph of strings that satisfies the Graph interface.
type sg struct {
  ids   map[string]Vertex
  names map[Vertex]string
  edges map[Vertex]map[Vertex]int
}

func newsg(ids map[string]Vertex) sg {
  g := sg{ids: ids}
  g.names = make(map[Vertex]string)
  for k, v := range ids {
    g.names[v] = k
  }
  g.edges = make(map[Vertex]map[Vertex]int)
  return g
}
func (g sg) edge(u, v string, w int) {
  if _, ok := g.edges[g.ids[u]]; !ok {
    g.edges[g.ids[u]] = make(map[Vertex]int)
  }
  g.edges[g.ids[u]][g.ids[v]] = w
}
func (g sg) path(v Vertex, prev map[Vertex]Vertex) (s string) {
  s = g.names[v]
  for prev[v] >= 0 {
    v = prev[v]
    s = g.names[v] + s
  }
  return s
}
func (g sg) Vertices() (vs []Vertex) {
  for _, v := range g.ids {
    vs = append(vs, v)
  }
  return vs
}
func (g sg) Neighbors(u Vertex) (vs []Vertex) {
  for v := range g.edges[u] {
    vs = append(vs, v)
  }
  return vs
}
func (g sg) Weight(u, v Vertex) int { return g.edges[u][v] }

func main() {
  g := newsg(map[string]Vertex{
    "a": 1,
    "b": 2,
    "c": 3,
    "d": 4,
    "e": 5,
    "f": 6,
  })
  g.edge("a", "b", 7)
  g.edge("a", "c", 9)
  g.edge("a", "f", 14)
  g.edge("b", "c", 10)
  g.edge("b", "d", 15)
  g.edge("c", "d", 11)
  g.edge("c", "f", 2)
  g.edge("d", "e", 6)
  g.edge("e", "f", 9)

  dist, prev := Dijkstra(g, g.ids["a"])
  fmt.Printf("Distance to %s: %d, Path: %s\n", "e", dist[g.ids["e"]], g.path(g.ids["e"], prev))
  fmt.Printf("Distance to %s: %d, Path: %s\n", "f", dist[g.ids["f"]], g.path(g.ids["f"], prev))
}
```

```txt

Distance to e: 26, Path: acde
Distance to f: 11, Path: acf

```



## Haskell


Translation of the C++ solution, and all the complexities are the same as in the C++ solution. In particular, we again use a self-balancing binary search tree (<code>Data.Set</code>) to implement the priority queue, which results in an optimal complexity.

```haskell
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Set as S

dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v,w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  previous <- newSTArray b invalid_index
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance v dist_thru_u
                    writeArray previous v u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  m <- freeze min_distance
  p <- freeze previous
  return (m, p)
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray

shortest_path_to :: (Ix v) => v -> v -> Array v v -> [v]
shortest_path_to target invalid_index previous =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (previous ! vertex) (vertex : acc)

adj_list :: Array Char [(Char, Int)]
adj_list = listArray ('a', 'f') [ [('b',7), ('c',9), ('f',14)],
                                  [('a',7), ('c',10), ('d',15)],
                                  [('a',9), ('b',10), ('d',11), ('f',2)],
                                  [('b',15), ('c',11), ('e',6)],
                                  [('d',6), ('f',9)],
                                  [('a',14), ('c',2), ('e',9)] ]

main :: IO ()
main = do
  let (min_distance, previous) = dijkstra 'a' ' ' adj_list
  putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
  let path = shortest_path_to 'e' ' ' previous
  putStrLn $ "Path: " ++ show path
```



## Huginn


```huginn
import Algorithms as algo;
import Mathematics as math;
import Text as text;

class Edge {
	_to = none;
	_name = none;
	_cost = none;
	constructor( to_, name_, cost_ ) {
		_to = to_;
		_name = name_;
		_cost = real( cost_ );
	}
	to_string() {
		return ( "{}<{}>".format( _name, _cost ) );
	}
}

class Path {
	_id = none;
	_from = none;
	_cost = none;
	_names = none;
	constructor( toName_, ids_, names_ ) {
		_id = ids_[toName_];
		_names = names_;
		_cost = math.INFINITY;
	}
	less( other_ ) {
		return ( _cost < other_._cost );
	}
	update( from_, cost_ ) {
		_from = from_;
		_cost = cost_;
	}
	to_string() {
		return ( "{} via {} at cost {}".format( _names[_id], _from != none ? _names[_from] : none, _cost ) );
	}
}

class Graph {
	_neighbours = [];
	_ids = {};
	_names = [];
	add_node( name_ ) {
		if ( name_ ∉ _ids ) {
			_ids[name_] = size( _names );
			_names.push( name_ );
		}
	}
	add_edge( from_, to_, cost_ ) {
		assert( ( from_ ∈ _ids ) && ( to_ ∈ _ids ) );
		from = _ids[from_];
		to = _ids[to_];
		if ( from >= size( _neighbours ) ) {
			_neighbours.resize( from + 1, [] );
		}
		_neighbours[from].push( Edge( to, to_, cost_ ) );
	}
	shortest_paths( from_ ) {
		assert( from_ ∈ _ids );
		from = _ids[from_];
		paths = algo.materialize( algo.map( _names, @[_ids, _names]( name ) { Path( name, _ids, _names ); } ), list );
		paths[from].update( none, 0.0 );
		todo = algo.sorted( paths, @(x){-x._cost;} );
		while ( size( todo ) > 0 ) {
			node = todo[-1]._id;
			todo.resize( size( todo ) - 1, none );
			if ( node >= size( _neighbours ) ) {
				continue;
			}
			neighbours = _neighbours[node];
			for ( n : neighbours ) {
				newCost = n._cost + paths[node]._cost;
				if ( newCost < paths[n._to]._cost ) {
					paths[n._to].update( node, newCost );
				}
			}
			todo = algo.sorted( todo, @(x){-x._cost;} );
		}
		return ( paths );
	}
	path( paths_, to_ ) {
		assert( to_ ∈ _ids );
		to = _ids[to_];
		p = [to_];
		while ( paths_[to]._from != none ) {
			to = paths_[to]._from;
			p.push( _names[to] );
		}
		return ( algo.materialize( algo.reversed( p ), list ) );
	}
	to_string() {
		s = "";
		for ( i, n : algo.enumerate( _neighbours ) ) {
			s += "{} -> {}\n".format( _names[i], n );
		}
	}
}

main() {
	g = Graph();
	confStr = input();
	if ( confStr == none ) {
		return ( 1 );
	}
	conf = algo.materialize( algo.map( text.split( confStr ), integer ), tuple );
	assert( size( conf ) == 2 );
	for ( _ : algo.range( conf[0] ) ) {
		line = input();
		if ( line == none ) {
			return ( 1 );
		}
		g.add_node( line.strip() );
	}
	for ( _ : algo.range( conf[1] ) ) {
		line = input();
		if ( line == none ) {
			return ( 1 );
		}
		g.add_edge( algo.materialize( text.split( line.strip() ), tuple )... );
	}
	print( string( g ) );
	paths = g.shortest_paths( "a" );
	for ( p : paths ) {
		print( "{}\n".format( p ) );
	}
	print( "{}\n".format( g.path( paths, "e" ) ) );
	print( "{}\n".format( g.path( paths, "f" ) ) );
}
```


Sample run via:
```txt
cat ~/graph.g | ./dijkstra.hgn
```
, output:

```txt
a -> [b<7.0>, c<9.0>, f<14.0>]
b -> [c<10.0>, d<15.0>]
c -> [d<11.0>, f<2.0>]
d -> [e<6.0>]
e -> [f<9.0>]
a via none at cost 0.0
b via a at cost 7.0
c via a at cost 9.0
d via c at cost 20.0
e via d at cost 26.0
f via c at cost 11.0
[a, c, d, e]
[a, c, f]
```


=={{header|Icon}} and {{header|Unicon}}==

This Unicon-only solution is an adaptation of the Unicon parallel maze solver found
in [[Maze solving]].  It searches paths in the graph in parallel until all
possible shortest paths from the start node to the finish node have been
discovered and then outputs the shortest path.


```unicon
procedure main(A)
    graph := getGraph()
    repeat {
        writes("What is the start node? ")
        start := \graph.nodes[read()] | stop()
        writes("What is the finish node? ")
        finish := read() | stop()

        QMouse(graph,start,finish)
        waitForCompletion() # block until all quantum mice have finished

        showPath(getBestMouse(),start.name,finish)
        cleanGraph(graph)
        }
end

procedure getGraph()
    graph := Graph(table(),table())
    write("Enter edges as 'n1,n2,weight' (blank line terminates)")
    repeat {
        if *(line := trim(read())) = 0 then break
        line ? {
            n1 := 1(tab(upto(',')),move(1))
            n2 := 1(tab(upto(',')),move(1))
            w  := tab(0)
            /graph.nodes[n1] := Node(n1,set())
            /graph.nodes[n2] := Node(n2,set())
            insert(graph.nodes[n1].targets,graph.nodes[n2])
            graph.weights[n1||":"||n2] := w
            }
        }
    return graph
end

procedure showPath(mouse,start,finish)
    if \mouse then {
        path := mouse.getPath()
        writes("Weight: ",path.weight," -> ")
        every writes(" ",!path.nodes)
        write("\n")
        }
    else write("No path from ",start," to ",finish,"\n")
end

# A "Quantum-mouse" for traversing graphs.  Each mouse lives for just
#  one node but can spawn additional mice to search adjoining nodes.

global qMice, goodMice, region, qMiceEmpty

record Graph(nodes,weights)
record Node(name,targets,weight)
record Path(weight, nodes)

class QMouse(graph, loc, finish, path)

    method getPath(); return path; end
    method atEnd(); return (finish == loc.name); end

    method visit(n)  # Visit if we don't already have a cheaper route to n
        newWeight := path.weight + graph.weights[loc.name||":"||n.name]
        critical region[n]: if /n.weight | (newWeight < n.weight) then {
            n.weight := newWeight
            unlock(region[n])
            return n
            }
    end

initially (g, l, f, p)
    initial {   # Construct critical region mutexes and completion condvar
        qMiceEmpty := condvar()
        region := table()
        every region[n := !g.nodes] := mutex()
        qMice := mutex(set())
        cleanGraph(g)
        }
    graph := g
    loc := l
    finish := f
    /p := Path(0,[])
    path := Path(p.weight,copy(p.nodes))
    if *path.nodes > 0 then
        path.weight +:= g.weights[path.nodes[-1]||":"||loc.name]
    put(path.nodes, loc.name)
    insert(qMice,self)
    thread {
        if atEnd() then insert(goodMice, self)    # This mouse found a finish
        every QMouse(g,visit(!loc.targets),f,path)
        delete(qMice, self)                       # Kill this mouse
        if *qMice=0 then signal(qMiceEmpty)       # All mice are dead
        }
end

procedure cleanGraph(graph)
    every (!graph.nodes).weight := &null
    goodMice := mutex(set())
end

procedure getBestMouse()
    every mouse := !goodMice do  { # Locate shortest path
        weight := mouse.getPath().weight
        /minPathWeight := weight
        if minPathWeight >=:= weight then bestMouse := mouse
        }
    return bestMouse
end

procedure waitForCompletion()
    critical qMiceEmpty: while *qMice > 0 do wait(qMiceEmpty)
end
```


Sample run:


```txt

-> dSolve
Enter edges as 'n1,n2,weight' (blank line terminates)
a,b,7
a,c,9
a,f,14
b,c,10
b,d,15
c,d,11
c,f,2
d,e,6
e,f,9

What is the start node? a
What is the finish node? f
Weight: 11 ->  a c f

What is the start node? a
What is the finish node? e
Weight: 26 ->  a c d e

What is the start node? f
What is the finish node? a
No path from f to a

What is the start node?
->

```



## J


```J

NB. verbs and adverb
parse_table=: ;:@:(LF&= [;._2 -.&CR)
mp=: $:~ :(+/ .*)                       NB. matrix product
min=: <./                               NB. minimum
Index=: (i.`)(`:6)                      NB. Index adverb

dijkstra=: dyad define
  'LINK WEIGHT'=. , (0 _ ,. 2) <;.3 y
  'SOURCE SINK'=. |: LINK
  FRONTIER=. , < {. x
  GOAL=. {: x
  enumerate=. 2&([\)&.>
  while. FRONTIER do.
    PATH_MASK=. FRONTIER (+./@:(-:"1/)&:>"0 _~ enumerate)~ LINK
    I=. PATH_MASK min Index@:mp WEIGHTS
    PATH=. I >@{ FRONTIER
    STATE=. {: PATH
    if. STATE -: GOAL do. PATH return. end.
    FRONTIER=. (<<< I) { FRONTIER  NB. elision
    ADJACENCIES=. (STATE = SOURCE) # SINK
    FRONTIER=. FRONTIER , PATH <@,"1 0 ADJACENCIES
  end.
  EMPTY
)



NB. The specific problem

INPUT=: noun define
a	 b	 7
a	 c	 9
a	 f	 14
b	 c	 10
b	 d	 15
c	 d	 11
c	 f	 2
d	 e	 6
e	 f	 9
)

T=: parse_table INPUT
NAMED_LINKS=: _ 2 {. T
NODES=: ~. , NAMED_LINKS                NB. vector of boxed names
NUMBERED_LINKS=: NODES i. NAMED_LINKS
WEIGHTS=: _ ".&> _ _1 {. T
GRAPH=: NUMBERED_LINKS ,. WEIGHTS NB. GRAPH is the numerical representation


TERMINALS=: NODES (i. ;:) 'a e'

NODES {~ TERMINALS dijkstra GRAPH

Note 'Output'
┌─┬─┬─┬─┐
│a│c│d│e│
└─┴─┴─┴─┘

TERMINALS and GRAPH are integer arrays:

   TERMINALS
0 5

   GRAPH
0 1  7
0 2  9
0 3 14
1 2 10
1 4 15
2 4 11
2 3  2
4 5  6
5 3  9
)

```



###  J: Alternative Implementation



```j
vertices=: ;:'a b c d e f'
edges=:|: ;:;._2]0 :0
  a b 7
  a c 9
  a f 14
  b c 10
  b d 15
  c d 11
  c f 2
  d e 6
  e f 9
)

shortest_path=:1 :0
:
  NB. x: path endpoints, m: vertex labels, y: edges (starts,ends,:costs)
  terminals=. m i. x
  starts=. m i. 0{y
  ends=.   m i. 1{y
  tolls=.  _&".@> 2{y
  C=. tolls (starts,&.>ends)}_$~2##m
  bestprice=. (<terminals){ (<. <./ .+/~)^:_ C
  best=. i.0
  if. _>bestprice do.
    paths=. ,.{.terminals
    goal=. {:terminals
    costs=. ,0
    while. #costs do.
      next=. ({:paths){C
      keep=. (_>next)*bestprice>:next+costs
      rep=. +/"1 keep
      paths=. (rep#"1 paths),(#m)|I.,keep
      costs=. (rep#"1 costs)+keep #&, next
      if. #j=. I. goal = {:paths do.
        best=. best, (bestprice=j{costs)# <"1 j{|:paths
      end.
      toss=. <<<j,I.bestprice<:costs
      paths=. toss {"1 paths
      costs=. toss { costs
    end.
  end.
  best {L:0 _ m
)
```


Example use:


```J
   (;:'a e') vertices shortest_path edges
┌─────────┐
│┌─┬─┬─┬─┐│
││a│c│d│e││
│└─┴─┴─┴─┘│
└─────────┘
```


This version finds all shortest paths, and for this example completes in two thirds the time of the other J implementation.

This algorithm first translates the graph representation to a cost connection matrix, with infinite cost for unconnected nodes. Then we use [[Floyd-Warshall_algorithm#J|a summing variation on transitive closure]] to find minimal connection costs for all nodes, and extract our best price from that. If our desired nodes are connected, we then search for paths which satisfy this best (minimal) price constraint: We repeatedly find all connections from our frontier, tracking path cost and discarding paths which have a cost which exceeds our best price. When a path reaches the end node, it is removed and remembered.


## Java

Algorithm is derived from Wikipedia section 'Using a priority queue'.
This implementation finds the single path from a source to all reachable vertices.
Building the graph from a set of edges takes O(E log V) for each pass.
Vertices are stored in a TreeSet (self-balancing binary search tree) instead of a PriorityQueue (a binary heap) in order to get O(log n) performance for removal of any element, not just the head.
Decreasing the distance of a vertex is accomplished by removing it from the tree and later re-inserting it.

```java

import java.io.*;
import java.util.*;

public class Dijkstra {
   private static final Graph.Edge[] GRAPH = {
      new Graph.Edge("a", "b", 7),
      new Graph.Edge("a", "c", 9),
      new Graph.Edge("a", "f", 14),
      new Graph.Edge("b", "c", 10),
      new Graph.Edge("b", "d", 15),
      new Graph.Edge("c", "d", 11),
      new Graph.Edge("c", "f", 2),
      new Graph.Edge("d", "e", 6),
      new Graph.Edge("e", "f", 9),
   };
   private static final String START = "a";
   private static final String END = "e";

   public static void main(String[] args) {
      Graph g = new Graph(GRAPH);
      g.dijkstra(START);
      g.printPath(END);
      //g.printAllPaths();
   }
}

class Graph {
   private final Map<String, Vertex> graph; // mapping of vertex names to Vertex objects, built from a set of Edges

   /** One edge of the graph (only used by Graph constructor) */
   public static class Edge {
      public final String v1, v2;
      public final int dist;
      public Edge(String v1, String v2, int dist) {
         this.v1 = v1;
         this.v2 = v2;
         this.dist = dist;
      }
   }

   /** One vertex of the graph, complete with mappings to neighbouring vertices */
  public static class Vertex implements Comparable<Vertex>{
	public final String name;
	public int dist = Integer.MAX_VALUE; // MAX_VALUE assumed to be infinity
	public Vertex previous = null;
	public final Map<Vertex, Integer> neighbours = new HashMap<>();

	public Vertex(String name)
	{
		this.name = name;
	}

	private void printPath()
	{
		if (this == this.previous)
		{
			System.out.printf("%s", this.name);
		}
		else if (this.previous == null)
		{
			System.out.printf("%s(unreached)", this.name);
		}
		else
		{
			this.previous.printPath();
			System.out.printf(" -> %s(%d)", this.name, this.dist);
		}
	}

	public int compareTo(Vertex other)
	{
		if (dist == other.dist)
			return name.compareTo(other.name);

		return Integer.compare(dist, other.dist);
	}

	@Override public String toString()
	{
		return "(" + name + ", " + dist + ")";
	}
}

   /** Builds a graph from a set of edges */
   public Graph(Edge[] edges) {
      graph = new HashMap<>(edges.length);

      //one pass to find all vertices
      for (Edge e : edges) {
         if (!graph.containsKey(e.v1)) graph.put(e.v1, new Vertex(e.v1));
         if (!graph.containsKey(e.v2)) graph.put(e.v2, new Vertex(e.v2));
      }

      //another pass to set neighbouring vertices
      for (Edge e : edges) {
         graph.get(e.v1).neighbours.put(graph.get(e.v2), e.dist);
         //graph.get(e.v2).neighbours.put(graph.get(e.v1), e.dist); // also do this for an undirected graph
      }
   }

   /** Runs dijkstra using a specified source vertex */
   public void dijkstra(String startName) {
      if (!graph.containsKey(startName)) {
         System.err.printf("Graph doesn't contain start vertex \"%s\"\n", startName);
         return;
      }
      final Vertex source = graph.get(startName);
      NavigableSet<Vertex> q = new TreeSet<>();

      // set-up vertices
      for (Vertex v : graph.values()) {
         v.previous = v == source ? source : null;
         v.dist = v == source ? 0 : Integer.MAX_VALUE;
         q.add(v);
      }

      dijkstra(q);
   }

   /** Implementation of dijkstra's algorithm using a binary heap. */
   private void dijkstra(final NavigableSet<Vertex> q) {
      Vertex u, v;
      while (!q.isEmpty()) {

         u = q.pollFirst(); // vertex with shortest distance (first iteration will return source)
         if (u.dist == Integer.MAX_VALUE) break; // we can ignore u (and any other remaining vertices) since they are unreachable

         //look at distances to each neighbour
         for (Map.Entry<Vertex, Integer> a : u.neighbours.entrySet()) {
            v = a.getKey(); //the neighbour in this iteration

            final int alternateDist = u.dist + a.getValue();
            if (alternateDist < v.dist) { // shorter path to neighbour found
               q.remove(v);
               v.dist = alternateDist;
               v.previous = u;
               q.add(v);
            }
         }
      }
   }

   /** Prints a path from the source to the specified vertex */
   public void printPath(String endName) {
      if (!graph.containsKey(endName)) {
         System.err.printf("Graph doesn't contain end vertex \"%s\"\n", endName);
         return;
      }

      graph.get(endName).printPath();
      System.out.println();
   }
   /** Prints the path from the source to every vertex (output order is not guaranteed) */
   public void printAllPaths() {
      for (Vertex v : graph.values()) {
         v.printPath();
         System.out.println();
      }
   }
}
```
```txt

a -> c(9) -> d(20) -> e(26)

```



## Javascript

Using the [[wp:Dijkstra's_algorithm#Pseudocode]]

```javascript

const dijkstra = (edges,source,target) => {
    const Q = new Set(),
          prev = {},
          dist = {},
          adj = {}

    const vertex_with_min_dist = (Q,dist) => {
        let min_distance = Infinity,
            u = null

        for (let v of Q) {
            if (dist[v] < min_distance) {
                min_distance = dist[v]
                u = v
            }
        }
        return u
    }

    for (let i=0;i<edges.length;i++) {
        let v1 = edges[i][0],
            v2 = edges[i][1],
            len = edges[i][2]

        Q.add(v1)
        Q.add(v2)

        dist[v1] = Infinity
        dist[v2] = Infinity

        if (adj[v1] === undefined) adj[v1] = {}
        if (adj[v2] === undefined) adj[v2] = {}

        adj[v1][v2] = len
        adj[v2][v1] = len
    }

    dist[source] = 0

    while (Q.size) {
        let u = vertex_with_min_dist(Q,dist),
            neighbors = Object.keys(adj[u]).filter(v=>Q.has(v)) //Neighbor still in Q

        Q.delete(u)

        if (u===target) break //Break when the target has been found

        for (let v of neighbors) {
            let alt = dist[u] + adj[u][v]
            if (alt < dist[v]) {
                dist[v] = alt
                prev[v] = u
            }
        }
    }

    {
        let u = target,
        S = [u],
        len = 0

        while (prev[u] !== undefined) {
            S.unshift(prev[u])
            len += adj[u][prev[u]]
            u = prev[u]
        }
        return [S,len]
    }
}

//Testing algorithm
let graph = []
graph.push(["a", "b", 7])
graph.push(["a", "c", 9])
graph.push(["a", "f", 14])
graph.push(["b", "c", 10])
graph.push(["b", "d", 15])
graph.push(["c", "d", 11])
graph.push(["c", "f", 2])
graph.push(["d", "e", 6])
graph.push(["e", "f", 9])

let [path,length] = dijkstra(graph, "a", "e");
console.log(path) //[ 'a', 'c', 'f', 'e' ]
console.log(length) //20

```



## Julia

```julia
struct Digraph{T <: Real,U}
    edges::Dict{Tuple{U,U},T}
    verts::Set{U}
end

function Digraph(edges::Vector{Tuple{U,U,T}}) where {T <: Real,U}
    vnames = Set{U}(v for edge in edges for v in edge[1:2])
    adjmat = Dict((edge[1], edge[2]) => edge[3] for edge in edges)
    return Digraph(adjmat, vnames)
end

vertices(g::Digraph) = g.verts
edges(g::Digraph)    = g.edges

neighbours(g::Digraph, v) = Set((b, c) for ((a, b), c) in edges(g) if a == v)

function dijkstrapath(g::Digraph{T,U}, source::U, dest::U) where {T, U}
    @assert source ∈ vertices(g) "$source is not a vertex in the graph"

    # Easy case
    if source == dest return [source], 0 end
    # Initialize variables
    inf  = typemax(T)
    dist = Dict(v => inf for v in vertices(g))
    prev = Dict(v => v   for v in vertices(g))
    dist[source] = 0
    Q = copy(vertices(g))
    neigh = Dict(v => neighbours(g, v) for v in vertices(g))

    # Main loop
    while !isempty(Q)
        u = reduce((x, y) -> dist[x] < dist[y] ? x : y, Q)
        pop!(Q, u)
        if dist[u] == inf || u == dest break end
        for (v, cost) in neigh[u]
            alt = dist[u] + cost
            if alt < dist[v]
                dist[v] = alt
                prev[v] = u
            end
        end
    end

    # Return path
    rst, cost = U[], dist[dest]
    if prev[dest] == dest
        return rst, cost
    else
        while dest != source
            unshift!(rst, dest)
            dest = prev[dest]
        end
        unshift!(rst, dest)
        return rst, cost
    end
end

# testgraph = [("a", "b", 1), ("b", "e", 2), ("a", "e", 4)]
testgraph = [("a", "b", 7),  ("a", "c", 9),  ("a", "f", 14), ("b", "c", 10),
             ("b", "d", 15), ("c", "d", 11), ("c", "f", 2),  ("d", "e", 6),
             ("e", "f", 9)]
g = Digraph(testgraph)
src, dst = "a", "e"
path, cost = dijkstrapath(g, src, dst)
println("Shortest path from $src to $dst: ", isempty(path) ? "no possible path" : join(path, " → "), " (cost $cost)")

# Print all possible paths
@printf("\n%4s | %3s | %s\n", "src", "dst", "path")
@printf("----------------\n")
for src in vertices(g), dst in vertices(g)
    path, cost = dijkstrapath(g, src, dst)
    @printf("%4s | %3s | %s\n", src, dst, isempty(path) ? "no possible path" : join(path, " → ") * " ($cost)")
end
```


```txt
Shortest path from a to e: a → c → d → e (cost 26)

 src | dst | path
----------------
   f |   f | f (0)
   f |   c | no possible path
   f |   e | no possible path
   f |   b | no possible path
   f |   a | no possible path
   f |   d | no possible path
   c |   f | c → f (2)
   c |   c | c (0)
   c |   e | c → d → e (17)
   c |   b | no possible path
   c |   a | no possible path
   c |   d | c → d (11)
   e |   f | e → f (9)
   e |   c | no possible path
   e |   e | e (0)
   e |   b | no possible path
   e |   a | no possible path
   e |   d | no possible path
   b |   f | b → c → f (12)
   b |   c | b → c (10)
   b |   e | b → d → e (21)
   b |   b | b (0)
   b |   a | no possible path
   b |   d | b → d (15)
   a |   f | a → c → f (11)
   a |   c | a → c (9)
   a |   e | a → c → d → e (26)
   a |   b | a → b (7)
   a |   a | a (0)
   a |   d | a → c → d (20)
   d |   f | d → e → f (15)
   d |   c | no possible path
   d |   e | d → e (6)
   d |   b | no possible path
   d |   a | no possible path
   d |   d | d (0)
```



## Kotlin

```scala
// version 1.1.51

import java.util.TreeSet

class Edge(val v1: String, val v2: String, val dist: Int)

 /** One vertex of the graph, complete with mappings to neighbouring vertices */
class Vertex(val name: String) : Comparable<Vertex> {

    var dist = Int.MAX_VALUE  // MAX_VALUE assumed to be infinity
    var previous: Vertex? = null
    val neighbours = HashMap<Vertex, Int>()

    fun printPath() {
        if (this == previous) {
            print(name)
        }
        else if (previous == null) {
            print("$name(unreached)")
        }
        else {
            previous!!.printPath()
            print(" -> $name($dist)")
        }
    }

    override fun compareTo(other: Vertex): Int {
        if (dist == other.dist) return name.compareTo(other.name)
        return dist.compareTo(other.dist)
    }

    override fun toString() = "($name, $dist)"
}

class Graph(
    val edges: List<Edge>,
    val directed: Boolean,
    val showAllPaths: Boolean = false
) {
    // mapping of vertex names to Vertex objects, built from a set of Edges
    private val graph = HashMap<String, Vertex>(edges.size)

    init {
        // one pass to find all vertices
        for (e in edges) {
            if (!graph.containsKey(e.v1)) graph.put(e.v1, Vertex(e.v1))
            if (!graph.containsKey(e.v2)) graph.put(e.v2, Vertex(e.v2))
        }

        // another pass to set neighbouring vertices
        for (e in edges) {
            graph[e.v1]!!.neighbours.put(graph[e.v2]!!, e.dist)
            // also do this for an undirected graph if applicable
            if (!directed) graph[e.v2]!!.neighbours.put(graph[e.v1]!!, e.dist)
        }
    }

    /** Runs dijkstra using a specified source vertex */
    fun dijkstra(startName: String) {
        if (!graph.containsKey(startName)) {
            println("Graph doesn't contain start vertex '$startName'")
            return
        }
        val source = graph[startName]
        val q = TreeSet<Vertex>()

        // set-up vertices
        for (v in graph.values) {
            v.previous = if (v == source) source else null
            v.dist = if (v == source)  0 else Int.MAX_VALUE
            q.add(v)
        }

        dijkstra(q)
    }

    /** Implementation of dijkstra's algorithm using a binary heap */
    private fun dijkstra(q: TreeSet<Vertex>) {
        while (!q.isEmpty()) {
            // vertex with shortest distance (first iteration will return source)
            val u = q.pollFirst()
            // if distance is infinite we can ignore 'u' (and any other remaining vertices)
            // since they are unreachable
            if (u.dist == Int.MAX_VALUE) break

            //look at distances to each neighbour
            for (a in u.neighbours) {
                val v = a.key // the neighbour in this iteration

                val alternateDist = u.dist + a.value
                if (alternateDist < v.dist) { // shorter path to neighbour found
                    q.remove(v)
                    v.dist = alternateDist
                    v.previous = u
                    q.add(v)
                }
            }
        }
    }

    /** Prints a path from the source to the specified vertex */
    fun printPath(endName: String) {
        if (!graph.containsKey(endName)) {
            println("Graph doesn't contain end vertex '$endName'")
            return
        }
        print(if (directed) "Directed   : " else "Undirected : ")
        graph[endName]!!.printPath()
        println()
        if (showAllPaths) printAllPaths() else println()
    }

    /** Prints the path from the source to every vertex (output order is not guaranteed) */
    private fun printAllPaths() {
        for (v in graph.values) {
            v.printPath()
            println()
        }
        println()
    }
}

val GRAPH = listOf(
    Edge("a", "b", 7),
    Edge("a", "c", 9),
    Edge("a", "f", 14),
    Edge("b", "c", 10),
    Edge("b", "d", 15),
    Edge("c", "d", 11),
    Edge("c", "f", 2),
    Edge("d", "e", 6),
    Edge("e", "f", 9)
)

const val START = "a"
const val END = "e"

fun main(args: Array<String>) {
    with (Graph(GRAPH, true)) {   // directed
        dijkstra(START)
        printPath(END)
    }
    with (Graph(GRAPH, false)) {  // undirected
        dijkstra(START)
        printPath(END)
    }
}
```


```txt

Directed   : a -> c(9) -> d(20) -> e(26)

Undirected : a -> c(9) -> f(11) -> e(20)

```



## Lua

Hopefully the variable names here make the process as clear as possible...

```Lua
-- Graph definition
local edges = {
    a = {b = 7, c = 9, f = 14},
    b = {c = 10, d = 15},
    c = {d = 11, f = 2},
    d = {e = 6},
    e = {f = 9}
}

-- Fill in paths in the opposite direction to the stated edges
function complete (graph)
    for node, edges in pairs(graph) do
        for edge, distance in pairs(edges) do
            if not graph[edge] then graph[edge] = {} end
            graph[edge][node] = distance
        end
    end
end

-- Create path string from table of previous nodes
function follow (trail, destination)
    local path, nextStep = destination, trail[destination]
    while nextStep do
        path = nextStep .. " " .. path
        nextStep = trail[nextStep]
    end
    return path
end

-- Find the shortest path between the current and destination nodes
function dijkstra (graph, current, destination, directed)
    if not directed then complete(graph) end
    local unvisited, distanceTo, trail = {}, {}, {}
    local nearest, nextNode, tentative
    for node, edgeDists in pairs(graph) do
        if node == current then
            distanceTo[node] = 0
            trail[current] = false
        else
            distanceTo[node] = math.huge
            unvisited[node] = true
        end
    end
    repeat
        nearest = math.huge
        for neighbour, pathDist in pairs(graph[current]) do
            if unvisited[neighbour] then
                tentative = distanceTo[current] + pathDist
                if tentative < distanceTo[neighbour] then
                    distanceTo[neighbour] = tentative
                    trail[neighbour] = current
                end
                if tentative < nearest then
                    nearest = tentative
                    nextNode = neighbour
                end
            end
        end
        unvisited[current] = false
        current = nextNode
    until unvisited[destination] == false or nearest == math.huge
    return distanceTo[destination], follow(trail, destination)
end

-- Main procedure
print("Directed:", dijkstra(edges, "a", "e", true))
print("Undirected:", dijkstra(edges, "a", "e", false))
```

```txt
Directed:       26      a c d e
Undirected:     20      a c f e
```



## M2000 Interpreter


```M2000 Interpreter

Module Dijkstra`s_algorithm {
	const max_number=1.E+306
	GetArr=lambda (n, val)->{
		dim d(n)=val
		=d()
	}
	term=("",0)
	Edges=(("a", ("b",7),("c",9),("f",14)),("b",("c",10),("d",15)),("c",("d",11),("f",2)),("d",("e",6)),("e",("f", 9)),("f",term))
	Document Doc$="Graph:"+{
	}
	ShowGraph()
	Doc$="Paths"+{
	}
	Print "Paths"
	For from_here=0 to 5
		pa=GetArr(len(Edges), -1)
		d=GetArr(len(Edges), max_number)
		Inventory S=1,2,3,4,5,6
		return d, from_here:=0
		RemoveMin=Lambda S, d, max_number-> {
			ss=each(S)
			min=max_number
			p=0
			while ss
				val=d#val(eval(S,ss^)-1)
				if min>val then let min=val : p=ss^
			end while
			=s(p!)  ' use p as index not key
			Delete S, eval(s,p)
		}
		Show_Distance_and_Path$=lambda$ d, pa, from_here, max_number (n) -> {
			ret1$=chr$(from_here+asc("a"))+" to "+chr$(n+asc("a"))
			if d#val(n) =max_number then =ret1$+ "     No Path" :exit
			let ret$="", mm=n, m=n
			repeat
				n=m
				ret$+=chr$(asc("a")+n)
				m=pa#val(n)
			until  from_here=n
			=ret1$+format$("{0::-4} {1}",d#val(mm),strrev$(ret$))
		}
		while len(s)>0
			u=RemoveMin()
			rem Print u, chr$(u-1+asc("a"))
			Relaxed()
		end while
		For i=0 to len(d)-1
			line$=Show_Distance_and_Path$(i)
			Print line$
			doc$=line$+{
			}
		next
	next
	Clipboard Doc$
	End
	Sub Relaxed()
		local vertex=Edges#val(u-1), i
		local e=Len(vertex)-1, edge=(,), val
		for i=1 to e
			edge=vertex#val(i)
			if edge#val$(0)<>"" then
				val=Asc(edge#val$(0))-Asc("a")
				if d#val(val)>edge#val(1)+d#val(u-1) then  return d, val:=edge#val(1)+d#val(u-1) : Return Pa, val:=u-1
			end if
		next
	end sub
	Sub ShowGraph()
		Print "Graph"
		local i
		for i=1 to len(Edges)
			show_edges(i)
		next
	end sub
	Sub show_edges(n)
		n--
		local vertex=Edges#val(n), line$
		local e=each(vertex 2 to end), v2=(,)
		While e
			v2=array(e)
			line$=vertex#val$(0)+if$(v2#val$(0)<>""->"->"+v2#val$(0)+format$(" {0::-2}",v2#val(1)),"")
			Print line$
			Doc$=line$+{
			}
		end while
	end sub
}
Dijkstra`s_algorithm

```


<pre style="height:30ex;overflow:scroll">
Graph:
a->b  7
a->c  9
a->f 14
b->c 10
b->d 15
c->d 11
c->f  2
d->e  6
e->f  9
f
Paths
a to a   0 a
a to b   7 ab
a to c   9 ac
a to d  20 acd
a to e  26 acde
a to f  11 acf
b to a     No Path
b to b   0 b
b to c  10 bc
b to d  15 bd
b to e  21 bde
b to f  12 bcf
c to a     No Path
c to b     No Path
c to c   0 c
c to d  11 cd
c to e  17 cde
c to f   2 cf
d to a     No Path
d to b     No Path
d to c     No Path
d to d   0 d
d to e   6 de
d to f  15 def
e to a     No Path
e to b     No Path
e to c     No Path
e to d     No Path
e to e   0 e
e to f   9 ef
f to a     No Path
f to b     No Path
f to c     No Path
f to d     No Path
f to e     No Path
f to f   0 f

</pre >


## Mathematica


```Mathematica
bd = Graph[{"a" \[DirectedEdge] "b", "a" \[DirectedEdge] "c",
   "b" \[DirectedEdge] "c", "b" \[DirectedEdge] "d",
   "c" \[DirectedEdge] "d", "d" \[DirectedEdge] "e",
   "a" \[DirectedEdge] "f", "c" \[DirectedEdge] "f",
   "e" \[DirectedEdge] "f"},
  EdgeWeight -> {7, 9, 10, 15, 11, 6, 14, 2, 9},
  VertexLabels -> "Name", VertexLabelStyle -> Directive[Black, 20],
  ImagePadding -> 20]

FindShortestPath[bd, "a", "e", Method -> "Dijkstra"]
-> {"a", "c", "d", "e"}
```

[[File:Mma_dijkstra2.PNG]]


## Maxima


```maxima
load(graphs)$
g: create_graph([[1, "a"], [2, "b"], [3, "c"], [4, "d"], [5, "e"], [6, "f"]],
   [[[1, 2], 7],
    [[1, 3], 9],
    [[1, 6], 14],
    [[2, 3], 10],
    [[2, 4], 15],
    [[3, 4], 11],
    [[3, 6], 2],
    [[4, 5], 6],
    [[5, 6], 9]], directed)$

shortest_weighted_path(1, 5, g);
/* [26, [1, 3, 4, 5]] */
```



## OCaml


Just a straightforward implementation of the pseudo-code from the Wikipedia article:


```ocaml
let list_vertices graph =
  List.fold_left (fun acc ((a, b), _) ->
    let acc = if List.mem b acc then acc else b::acc in
    let acc = if List.mem a acc then acc else a::acc in
    acc
  ) [] graph

let neighbors v =
  List.fold_left (fun acc ((a, b), d) ->
    if a = v then (b, d)::acc else acc
  ) []

let remove_from v lst =
  let rec aux acc = function [] -> failwith "remove_from"
  | x::xs -> if x = v then List.rev_append acc xs else aux (x::acc) xs
  in aux [] lst

let with_smallest_distance q dist =
  match q with
  | [] -> assert false
  | x::xs ->
      let rec aux distance v = function
      | x::xs ->
          let d = Hashtbl.find dist x in
          if d < distance
          then aux d x xs
          else aux distance v xs
      | [] -> (v, distance)
      in
      aux (Hashtbl.find dist x) x xs

let dijkstra max_val zero add graph source target =
  let vertices = list_vertices graph in
  let dist_between u v =
    try List.assoc (u, v) graph
    with _ -> zero
  in
  let dist = Hashtbl.create 1 in
  let previous = Hashtbl.create 1 in
  List.iter (fun v ->                  (* initializations *)
    Hashtbl.add dist v max_val         (* unknown distance function from source to v *)
  ) vertices;
  Hashtbl.replace dist source zero;    (* distance from source to source *)
  let rec loop = function [] -> ()
  | q ->
      let u, dist_u =
        with_smallest_distance q dist in   (* vertex in q with smallest distance in dist *)
      if dist_u = max_val then
        failwith "vertices inaccessible";  (* all remaining vertices are inaccessible from source *)
      if u = target then () else begin
        let q = remove_from u q in
        List.iter (fun (v, d) ->
          if List.mem v q then begin
            let alt = add dist_u (dist_between u v) in
            let dist_v = Hashtbl.find dist v in
            if alt < dist_v then begin       (* relax (u,v,a) *)
              Hashtbl.replace dist v alt;
              Hashtbl.replace previous v u;  (* previous node in optimal path from source *)
            end
          end
        ) (neighbors u graph);
        loop q
      end
  in
  loop vertices;
  let s = ref [] in
  let u = ref target in
  while Hashtbl.mem previous !u do
    s := !u :: !s;
    u := Hashtbl.find previous !u
  done;
  (source :: !s)

let () =
  let graph =
    [ ("a", "b"), 7;
      ("a", "c"), 9;
      ("a", "f"), 14;
      ("b", "c"), 10;
      ("b", "d"), 15;
      ("c", "d"), 11;
      ("c", "f"), 2;
      ("d", "e"), 6;
      ("e", "f"), 9; ]
  in
  let p = dijkstra max_int 0 (+) graph "a" "e" in
  print_endline (String.concat " -> " p)
```


Output:


```txt
a -> c -> d -> e
```


Translation of the C++ solution, and all the complexities are the same as in the C++ solution. In particular, we again use a self-balancing binary search tree (<code>Set</code>) to implement the priority queue, which results in an optimal complexity.

```ocaml
type vertex = int
type weight = float
type neighbor = vertex * weight
module VertexSet = Set.Make(struct type t = weight * vertex let compare = compare end)

let dijkstra (src:vertex) (adj_list:neighbor list array) : weight array * vertex array =
  let n = Array.length adj_list in
  let min_distance = Array.make n infinity in
  min_distance.(src) <- 0.;
  let previous = Array.make n (-1) in
  let rec aux vertex_queue =
    if not (VertexSet.is_empty vertex_queue) then
      let dist, u = VertexSet.min_elt vertex_queue in
      let vertex_queue' = VertexSet.remove (dist, u) vertex_queue in
      let edges = adj_list.(u) in
      let f vertex_queue (v, weight) =
        let dist_thru_u = dist +. weight in
        if dist_thru_u >= min_distance.(v) then
          vertex_queue
        else begin
          let vertex_queue' = VertexSet.remove (min_distance.(v), v) vertex_queue in
          min_distance.(v) <- dist_thru_u;
          previous.(v) <- u;
          VertexSet.add (min_distance.(v), v) vertex_queue'
        end
      in
      aux (List.fold_left f vertex_queue' edges)
  in
  aux (VertexSet.singleton (min_distance.(src), src));
  min_distance, previous

let shortest_path_to (target : vertex) (previous : vertex array) : vertex list =
  let rec aux target acc =
    if target = -1 then
      acc
    else
      aux previous.(target) (target :: acc)
  in
  aux target []

let adj_list =
  [| [(1, 7.); (2, 9.); (5, 14.)];           (* 0 = a *)
     [(0, 7.); (2, 10.); (3, 15.)];          (* 1 = b *)
     [(0, 9.); (1, 10.); (3, 11.); (5, 2.)]; (* 2 = c *)
     [(1, 15.); (2, 11.); (4, 6.)];          (* 3 = d *)
     [(3, 6.); (5, 9.)];                     (* 4 = e *)
     [(0, 14.); (2, 2.); (4, 9.)]            (* 5 = f *)
  |]

let () =
  let min_distance, previous = dijkstra 0 adj_list in
  Printf.printf "Distance from 0 to 4: %f\n" min_distance.(4);
  let path = shortest_path_to 4 previous in
  print_string "Path: ";
  List.iter (Printf.printf "%d, ") path;
  print_newline ()
```



## PARI/GP

Basic, inefficient implementation. Takes an n×n matrix representing distance between nodes (a 0-1 matrix if you just want to count number of steps) and a number in 1..n representing the starting node, which defaults to 1 if not given.

```parigp
shortestPath(G, startAt=1)={
	my(n=#G[,1],dist=vector(n,i,9e99),prev=dist,Q=2^n-1);
	dist[startAt]=0;
	while(Q,
		my(t=vecmin(vecextract(dist,Q)),u);
		if(t==9e99, break);
		for(i=1,#v,if(dist[i]==t && bittest(Q,i-1), u=i; break));
		Q-=1<<(u-1);
		for(i=1,n,
			if(!G[u,i],next);
			my(alt=dist[u]+G[u,i]);
			if (alt < dist[i],
				dist[i]=alt;
				prev[i]=u;
			)
		)
	);
	dist
};
```



## Perl


```perl
use strict;
use warnings;

sub add_edge {
    my ($g, $a, $b, $weight) = @_;
    $g->{$a} ||= {name => $a};
    $g->{$b} ||= {name => $b};
    push @{$g->{$a}{edges}}, {weight => $weight, vertex => $g->{$b}};
}

sub push_priority {
    my ($a, $v) = @_;
    my $i = 0;
    my $j = $#{$a};
    while ($i <= $j) {
        my $k = int(($i + $j) / 2);
        if ($a->[$k]{dist} >= $v->{dist}) {
            $j = $k - 1;
        }
        else {
            $i = $k + 1;
        }
    }
    splice @$a, $i, 0, $v;
}

sub dijkstra {
    my ($g, $a, $b) = @_;
    for my $v (values %$g) {
        $v->{dist} = 9999999;
        delete $v->{prev};
        delete $v->{visited};
    }
    $g->{$a}{dist} = 0;
    my $h = [];
    push_priority($h, $g->{$a});
    while (1) {
        my $v = shift @$h;
        last if !$v || $v->{name} eq $b;
        $v->{visited} = 1;
        for my $e (@{$v->{edges}}) {
            my $u = $e->{vertex};
            if (!$u->{visited} && $v->{dist} + $e->{weight} <= $u->{dist}) {
                $u->{prev} = $v;
                $u->{dist} = $v->{dist} + $e->{weight};
                push_priority($h, $u);
            }
        }
    }
}

my $g = {};
add_edge($g, "a", "b", 7);
add_edge($g, "a", "c", 9);
add_edge($g, "a", "f", 14);
add_edge($g, "b", "c", 10);
add_edge($g, "b", "d", 15);
add_edge($g, "c", "d", 11);
add_edge($g, "c", "f", 2);
add_edge($g, "d", "e", 6);
add_edge($g, "e", "f", 9);
dijkstra($g, "a", "e");
my $v = $g->{e};
my @a;

while ($v) {
    push @a, $v->{name};
    $v = $v->{prev};
}
my $path = join "", reverse @a;
print "$g->{e}{dist} $path\n";
```


output:


```txt
26 acde
```



## Perl 6

```perl6
class Graph {
  has (%.edges, %.nodes);

  method new(*@args){
    my (%edges, %nodes);
    for @args {
      %edges{.[0] ~ .[1]} = $_;
      %nodes{.[0]}.push( .[0] ~ .[1] );
      %nodes{.[1]}.push( .[0] ~ .[1] );
    }
    self.bless(edges => %edges, nodes => %nodes);
  }

  method neighbours ($source) {
    my (%neighbours, $edges);
    $edges = self.nodes{$source};
    for @$edges -> $x {
      for self.edges{$x}[0..1] -> $y {
        if $y ne $source {
          %neighbours{$y} = self.edges{$x}
        }
      }
    }
    return %neighbours
  }

  method dijkstra ($source, $dest) {
    my (%node_data, $v, $u);
    my @q = self.nodes.keys;

    for self.nodes.keys {
      %node_data{$_}{'dist'} = Inf;
      %node_data{$_}{'prev'} = '';
    }
    %node_data{$source}{'dist'} = 0;

    while @q {
      # %node_data.perl.say;
      my ($mindist, $idx) =
        @((map {[%node_data{@q[$_]}{'dist'},$_]},^@q).min(*[0]));
      $u = @q[$idx];

      if $mindist eq Inf {
        return ()
      }
      elsif $u eq $dest {
        my @s;
        while %node_data{$u}{'prev'} {
          @s.unshift($u);
          $u = %node_data{$u}{'prev'}
        }
        @s.unshift($source);
        return @s;
      }
      else {
        @q.splice($idx,1);
      }

      for self.neighbours($u).kv -> $v, $edge {
        my $alt = %node_data{$u}{'dist'} + $edge[2];
        if $alt < %node_data{$v}{'dist'} {
          %node_data{$v}{'dist'} = $alt;
          %node_data{$v}{'prev'} = $u
        }
      }
    }
  }
}

my $a = Graph.new([
  ["a", "b",  7],
  ["a", "c",  9],
  ["a", "f", 14],
  ["b", "c", 10],
  ["b", "d", 15],
  ["c", "d", 11],
  ["c", "f",  2],
  ["d", "e",  6],
  ["e", "f",  9]
]).dijkstra('a', 'e').say;
```

```txt

[a c f e]
```



## Phix

I didn't really copy any other code/pseudocode, just followed the basic concept of (update costs) (select lowest cost unvisited) until target reached.

Selects the shortest path from A to B only. As for time complexity, it looks plenty efficient enough to me, though it clearly is O(V^2).

Written after the task was changed to be a directed graph, and shows the correct solution for that.

```Phix
enum A,B,C,D,E,F
constant edges = {{A,B,7},
                  {A,C,9},
                  {A,F,14},
                  {B,C,10},
                  {B,D,15},
                  {C,D,11},
                  {C,F,2},
                  {D,E,6},
                  {E,F,9}}

sequence visited,
         cost,
         from

procedure reset()
    visited = repeat(0,6)
    cost = repeat(0,6)
    from = repeat(0,6)
end procedure

function backtrack(integer finish,start)
sequence res = {finish}
    while finish!=start do
        finish = from[finish]
        res = prepend(res,finish)
    end while
    return res
end function

function shortest_path(integer start, integer finish)
integer estart,eend,ecost,ncost,mincost
    while 1 do
        visited[start] = 1
        for i=1 to length(edges) do
            {estart,eend,ecost} = edges[i]
            if estart=start then
                ncost = cost[start]+ecost
                if visited[eend]=0 then
                    if from[eend]=0
                    or cost[eend]>ncost then
                        cost[eend] = ncost
                        from[eend] = start
                    end if
                elsif cost[eend]>ncost then
                    ?9/0    -- sanity check
                end if
            end if
        end for
        mincost = 0
        for i=1 to length(visited) do
            if visited[i]=0
            and from[i]!=0 then
                if mincost=0
                or cost[i]<mincost then
                    start = i
                    mincost = cost[start]
                end if
            end if
        end for
        if visited[start] then return -1 end if
        if start=finish then return cost[finish] end if
    end while
end function

function AFi(integer i)     -- output helper
    return 'A'+i-1
end function

function AFs(sequence s)    -- output helper
string res = ""
    for i=1 to length(s) do
        res &= AFi(s[i])
    end for
    return res
end function

procedure test(sequence testset)
integer start,finish,ecost
integer len
string epath,path
    for i=1 to length(testset) do
        {start,finish,ecost,epath} = testset[i]
        reset()
        len = shortest_path(start,finish)
        if len=-1 then
            path = "no path found"
        else
            path = AFs(backtrack(finish,start))
        end if
        printf(1,"%c->%c: length %d:%s (expected %d:%s)\n",{AFi(start),AFi(finish),len,path,ecost,epath})
    end for
end procedure

test({{A,E,26,"ACDE"},{A,F,11,"ACF"},{F,A,-1,"none"}})
```

```txt

A->E: length 26:ACDE (expected 26:ACDE)
A->F: length 11:ACF (expected 11:ACF)
F->A: length -1:no path found (expected -1:none)

```



## PHP

There are parts of this algorithm that could be optimized which have been marked TODO.

```PHP

<?php
function dijkstra($graph_array, $source, $target) {
    $vertices = array();
    $neighbours = array();
    foreach ($graph_array as $edge) {
        array_push($vertices, $edge[0], $edge[1]);
        $neighbours[$edge[0]][] = array("end" => $edge[1], "cost" => $edge[2]);
        $neighbours[$edge[1]][] = array("end" => $edge[0], "cost" => $edge[2]);
    }
    $vertices = array_unique($vertices);

    foreach ($vertices as $vertex) {
        $dist[$vertex] = INF;
        $previous[$vertex] = NULL;
    }

    $dist[$source] = 0;
    $Q = $vertices;
    while (count($Q) > 0) {

        // TODO - Find faster way to get minimum
        $min = INF;
        foreach ($Q as $vertex){
            if ($dist[$vertex] < $min) {
                $min = $dist[$vertex];
                $u = $vertex;
            }
        }

        $Q = array_diff($Q, array($u));
        if ($dist[$u] == INF or $u == $target) {
            break;
        }

        if (isset($neighbours[$u])) {
            foreach ($neighbours[$u] as $arr) {
                $alt = $dist[$u] + $arr["cost"];
                if ($alt < $dist[$arr["end"]]) {
                    $dist[$arr["end"]] = $alt;
                    $previous[$arr["end"]] = $u;
                }
            }
        }
    }
    $path = array();
    $u = $target;
    while (isset($previous[$u])) {
        array_unshift($path, $u);
        $u = $previous[$u];
    }
    array_unshift($path, $u);
    return $path;
}

$graph_array = array(
                    array("a", "b", 7),
                    array("a", "c", 9),
                    array("a", "f", 14),
                    array("b", "c", 10),
                    array("b", "d", 15),
                    array("c", "d", 11),
                    array("c", "f", 2),
                    array("d", "e", 6),
                    array("e", "f", 9)
               );

$path = dijkstra($graph_array, "a", "e");

echo "path is: ".implode(", ", $path)."\n";

```

Output is:

```txt
path is: a, c, f, e
```



## PicoLisp

Following the Wikipedia algorithm:

```PicoLisp
(de neighbor (X Y Cost)
   (push (prop X 'neighbors) (cons Y Cost))
   (push (prop Y 'neighbors) (cons X Cost)) )

(de dijkstra (Curr Dest)
   (let Cost 0
      (until (== Curr Dest)
         (let (Min T  Next)
            (for N (; Curr neighbors)
               (with (car N)
                  (let D (+ Cost (cdr N))
                     (unless (and (: distance) (>= D @))
                        (=: distance D) ) )
                  (when (> Min (: distance))
                     (setq Min (: distance)  Next This) )
                  (del (asoq Curr (: neighbors)) (:: neighbors)) ) )
            (setq Curr Next  Cost Min) ) )
      Cost ) )
```

Test:

```PicoLisp
(neighbor 'a 'b 7)
(neighbor 'a 'c 9)
(neighbor 'a 'f 14)
(neighbor 'b 'c 10)
(neighbor 'b 'd 15)
(neighbor 'c 'd 11)
(neighbor 'c 'f 2)
(neighbor 'd 'e 6)
(neighbor 'e 'f 9)

(dijkstra 'a 'e)
```

Output:

```txt
-> 20
```



## Prolog

An implementation of Dijkstra's algorithm in Prolog

Dijkstra's algorithm starts with a set of all unvisited nodes, assigning an initial distance value for each as infinite.  It then attempts to minimise the distance for each node from the origin.

Starting at the origin (distance 0), the algorithm checks each neighbor's distance value and if larger than the current path distance, replaces the neighboring node's distance value. It then marks the current node as visited, and repeats the process for each of the neighbors. When the current node becomes the destination, the distance to the origin is known.

This implementation is a slight variation on Dijkstra, which lends itself to Prolog's strengths while retaining ''approximate'' algorithmic equivalence.

Prolog is not good at modifying memory in place, but is quite good at handling facts, pattern matching, recursion and backtracking to find all possible solutions.

A dynamic database predicate, namely:


```txt
    rpath([target|reversed_path], distance)
```


stores the currently known shortest distance and best path to a destination from the origin. Since the path is a reversed list, the first item in the list is the destination node, and the predicate is
efficiently matched.

Instead of using unvisited flags on nodes, we test whether neighbors are already in the traversed path. This achieves the same thing as 'visited' flags, but in a way that is more efficient for Prolog.

After the graph traversal is complete, we are left with a single rpath/2 predicate for each reachable node, containing the shortest path and distance from the origin.

<b>Subtle differences</b>

1) Dijkstra visits each node only once, starting with the origin.  This algorithm:
    - arbitrarily selects a node (Qi) neighboring origin (o), and for that node
    - if o->Qi is the shortest known path:
        - update path and distance
        - traverse Qi
    - if o->Qi is not the shortest, select the next node.

It is possible therefore, contrary to Dijkstra, that we may visit a node more than once whilst discovering a shorter path. It is also possible that the first path we choose is already the shortest eliminating processing.

2) As traversal spreads outwards, the path is built as a list of traversed nodes.
    - We use this list to ensure that we do not loop endlessly.
    - This path is recorded as the shortest if the distance is indeed shorter than a known path.
    - Leaf nodes in the traversal tree are processed completely before the origin node processing
      is completed.
      - This implies that the first stage in our algorithm involves allocating each node
        in the traversal tree a path and 'shortest known distance from origin' value.
      - ...Which is arguably better than assigning an initial 'infinite distance' value.

We could possibly improve our algorithm by processing the neighbor with the shortest distance first, rather than an arbitrary selection as is currently the case.  There is nothing though, to suggest that the eventual shortest path found would necessarily follow the shortest initial path, unless the target node is already the closest neighbor.



```prolog
%___________________________________________________________________________

:-dynamic
	rpath/2.      % A reversed path

edge(a,b,7).
edge(a,c,9).
edge(b,c,10).
edge(b,d,15).
edge(c,d,11).
edge(d,e,6).
edge(a,f,14).
edge(c,f,2).
edge(e,f,9).

path(From,To,Dist) :- edge(To,From,Dist).
path(From,To,Dist) :- edge(From,To,Dist).

shorterPath([H|Path], Dist) :-		       % path < stored path? replace it
	rpath([H|T], D), !, Dist < D,          % match target node [H|_]
	retract(rpath([H|_],_)),
	writef('%w is closer than %w\n', [[H|Path], [H|T]]),
	assert(rpath([H|Path], Dist)).
shorterPath(Path, Dist) :-		       % Otherwise store a new path
	writef('New path:%w\n', [Path]),
	assert(rpath(Path,Dist)).

traverse(From, Path, Dist) :-		    % traverse all reachable nodes
	path(From, T, D),		    % For each neighbor
	not(memberchk(T, Path)),	    %	which is unvisited
	shorterPath([T,From|Path], Dist+D), %	Update shortest path and distance
	traverse(T,[From|Path],Dist+D).	    %	Then traverse the neighbor

traverse(From) :-
	retractall(rpath(_,_)),           % Remove solutions
	traverse(From,[],0).              % Traverse from origin
traverse(_).

go(From, To) :-
	traverse(From),                   % Find all distances
	rpath([To|RPath], Dist)->         % If the target was reached
	  reverse([To|RPath], Path),      % Report the path and distance
	  Distance is round(Dist),
	  writef('Shortest path is %w with distance %w = %w\n',
	       [Path, Dist, Distance]);
	writef('There is no route from %w to %w\n', [From, To]).

```

for example:

```txt
?- go(a,e).
New path:[b,a]
New path:[c,b,a]
New path:[d,c,b,a]
New path:[e,d,c,b,a]
New path:[f,e,d,c,b,a]
[f,c,b,a] is closer than [f,e,d,c,b,a]
[e,f,c,b,a] is closer than [e,d,c,b,a]
[d,b,a] is closer than [d,c,b,a]
[c,a] is closer than [c,b,a]
[d,c,a] is closer than [d,b,a]
[e,d,c,a] is closer than [e,f,c,b,a]
[f,c,a] is closer than [f,c,b,a]
[e,f,c,a] is closer than [e,d,c,a]
Shortest path is [a,c,f,e] with distance 0+9+2+9 = 20
true.
```



## Python

Starts from the [[wp:Dijkstra's_algorithm#Pseudocode]] recognising that their function <code>dist_between</code> is what this task calls ''cost''; and that their action <code>decrease-key v in Q</code> at their line 24 should be omitted if their Q is a set as stated in their line 9. The wp back-tracking pseudocode also misses a final insert of u at the beginning of S that must occur ''after'' exiting their while loop.

Note: q could be changed to be a priority queue instead of a set as mentioned [http://docs.python.org/3.3/library/heapq.html#priority-queue-implementation-notes here].

```python
from collections import namedtuple, deque
from pprint import pprint as pp


inf = float('inf')
Edge = namedtuple('Edge', ['start', 'end', 'cost'])

class Graph():
    def __init__(self, edges):
        self.edges = [Edge(*edge) for edge in edges]
        # print(dir(self.edges[0]))
        self.vertices = {e.start for e in self.edges} | {e.end for e in self.edges}

    def dijkstra(self, source, dest):
        assert source in self.vertices
        dist = {vertex: inf for vertex in self.vertices}
        previous = {vertex: None for vertex in self.vertices}
        dist[source] = 0
        q = self.vertices.copy()
        neighbours = {vertex: set() for vertex in self.vertices}
        for start, end, cost in self.edges:
            neighbours[start].add((end, cost))
        #pp(neighbours)

        while q:
            # pp(q)
            u = min(q, key=lambda vertex: dist[vertex])
            q.remove(u)
            if dist[u] == inf or u == dest:
                break
            for v, cost in neighbours[u]:
                alt = dist[u] + cost
                if alt < dist[v]:                                  # Relax (u,v,a)
                    dist[v] = alt
                    previous[v] = u
        #pp(previous)
        s, u = deque(), dest
        while previous[u]:
            s.appendleft(u)
            u = previous[u]
        s.appendleft(u)
        return s


graph = Graph([("a", "b", 7),  ("a", "c", 9),  ("a", "f", 14), ("b", "c", 10),
               ("b", "d", 15), ("c", "d", 11), ("c", "f", 2),  ("d", "e", 6),
               ("e", "f", 9)])
pp(graph.dijkstra("a", "e"))
```


```txt
deque(['a', 'c', 'd', 'e'])
```



## Racket


```racket

#lang racket
(require (planet jaymccarthy/dijkstra:1:2))

(define edges
  '([a . ((b 7)(c 9)(f 14))]
    [b . ((c 10)(d 15))]
    [c . ((d 11)(f 2))]
    [d . ((e 6))]
    [e . ((f 9))]))

(define (node-edges n)
  (cond [(assoc n edges) => rest] ['()]))
(define edge-weight second)
(define edge-end first)

(match/values (shortest-path node-edges edge-weight edge-end 'a (λ(n) (eq? n 'e)))
 [(dists prevs)
  (displayln (~a "Distances from a: " (for/list ([(n d) dists]) (list n d))))
  (displayln (~a "Shortest path: "
             (let loop ([path '(e)])
               (cond [(eq? (first path) 'a) path]
                     [(loop (cons (hash-ref prevs (first path)) path))]))))])

```

Output:

```racket

Distances from a: ((b 7) (d 20) (a 0) (c 9) (f 11) (e 26))
Shortest path: (a c d e)

```



## REXX

Some program features are:
:::*   elimination of null edges
:::*   elimination of duplicates (the cheapest path is chosen)
:::*   a test for a   ''no path found''   condition
:::*   use of memoization

```rexx
/*REXX program determines the  least costly path  between  two vertices  given a list.  */
$.= copies(9, digits() )                         /*edge cost:  indicates doesn't exist. */
xList= '!. @. $. beg fin bestP best$ xx yy'      /*common  EXPOSEd  variables for subs. */
@abc=  'abcdefghijklmnopqrstuvwxyz'              /*list of all the possible vertices.   */
verts= 0;  edges= 0                              /*the number of vertices and also edges*/
                      do #=1  for length(@abc);              _= substr(@abc, #, 1)
                      call value translate(_), #;      @@.#= _
                      end   /*#*/
call def$  a  b   7                              /*define an  edge  and  its  cost.     */
call def$  a  c   9                              /*   "    "    "    "    "     "       */
call def$  a  f  14                              /*   "    "    "    "    "     "       */
call def$  b  c  10                              /*   "    "    "    "    "     "       */
call def$  b  d  15                              /*   "    "    "    "    "     "       */
call def$  c  d  11                              /*   "    "    "    "    "     "       */
call def$  c  f   2                              /*   "    "    "    "    "     "       */
call def$  d  e   6                              /*   "    "    "    "    "     "       */
call def$  e  f   9                              /*   "    "    "    "    "     "       */
beg= a;    fin= e                                /*the  BEGin  and  FINish  vertexes.   */
say;       say 'number of    edges = '   edges
           say 'number of vertices = '   verts                 "    ["left(@abc, verts)"]"
best$= $.;    bestP=
say;                         do jv=2  to verts;    call paths verts, jv;       end  /*jv*/
@costIs= right('cost =', 16)
if bestP==$.  then say 'no path found.'
              else say 'best path ='   translate(bestP, @abc, 123456789)   @costIs   best$
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
apath: parse arg pathx 1 p1 2 p2 3;             Lp= length(pathx);              $= $.p1.p2
       if $>=best$  then return
       pv= p2;                      do ka=3  to Lp;   _= substr(pathx, ka, 1)
                                    if $.pv._>=best$  then return
                                    $= $ + $.pv._;    if $>=best$  then return;      pv= _
                                    end   /*ka*/
       best$=$;     bestP=pathx
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
def$:  parse arg xx yy $ .;         if $.xx.yy<$  &  $.yy.xx<$  |  xx==yy  then return
       edges= edges + 1;            verts= verts  +  ($.xx\==0)  +  ($.yy\==0)
       $.xx= 0;        $.yy= 0;     $.xx.yy= $
       say left('', 40)     "cost of    "     @@.xx     '───►'     @@.yy     "   is "    $
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
paths: procedure expose (xList);    parse arg xx, yy, @.
                     do kp=1  for xx;     _= kp;   !.kp= _;   end   /*build a path list.*/
       call .path 1
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.path: procedure expose (xList);    parse arg ?, _
       if ?>yy  then do;            if @.1\==beg | @.yy\==fin  then return
                       do #=1  for yy;  _= _ || @.#;  end  /*#*/;             call apath _
                     end
                else do qq=1  for xx                    /*build vertex paths recursively*/
                       do kp=1  for ?-1;  if @.kp==!.qq  then iterate qq;  end  /*kp*/
                     @.?= !.qq;     call .path ?+1      /*recursive call for next path. */
                     end   /*qq*/
       return
```

```txt

                                         cost of    a ───► b    is  7
                                         cost of    a ───► c    is  9
                                         cost of    a ───► f    is  14
                                         cost of    b ───► c    is  10
                                         cost of    b ───► d    is  15
                                         cost of    c ───► d    is  11
                                         cost of    c ───► f    is  2
                                         cost of    d ───► e    is  6
                                         cost of    e ───► f    is  9

number of    edges =  9
number of vertices =  6     [abcdef]

best path = acde           cost = 26

```



## Ring


```ring

# Project : Dijkstra's algorithm

graph = [["a", "b", 7],
              ["a", "c", 9],
              ["a", "f", 14],
              ["b", "c", 10],
              ["b", "d", 15],
              ["c", "d", 11],
              ["c", "f", 2],
              ["d", "e", 6],
              ["e", "f", 9]]

dbegin = "a"
dend = "e"
powlen = pow(2,len(graph)) - 1
dgraph = list(powlen)
dtemp = list(powlen)
lenold = 10
lennew = 0
sumold = 30
sumnew = 0

powerset(graph)

for n = 1 to len(dgraph)
      dtemp[n] = str2list(substr(dgraph[n], " ", nl))
next

for n = 1 to len(dtemp)
     if len(dtemp[n]) > 3 and dtemp[n][1] = dbegin  and dtemp[n][len(dtemp[n])-1] = dend
        flag = 1
        for m = 1 to len(dtemp[n])/3-1
              if dtemp[n][m*3-1] != dtemp[n][m*3+1]
                 flag = 0
              ok
        next
        if flag = 1
           lennew = len(dtemp[n])
           if lennew <= lenold
              lenold = lennew
              sumnew = 0
              for m = 1 to len(dtemp[n])/3
                    sumnew = sumnew + dtemp[n][m*3]
              next
              if sumnew < sumold
                 sumold = sumnew
                 gend = dtemp[n]
              ok
           ok
        ok
     ok
next
str = ""
see dbegin + " " + dend + " : "
for m = 1 to len(gend)/3
     str = str + gend[(m-1)*3 + 1] + " " + gend[(m-1)*3 + 2] + " " + gend[(m-1)*3 + 3] + " -> "
next
str = left(str,len(str)-4)
str = str + " cost : " + sumold + nl
see str + nl

func powerset(list)
        s = "{"
        p = 0
        for i = 2 to (2 << len(list)) - 1 step 2
             s = ""
             for j = 1 to len(list)
                  if i & (1 << j)
                     s = s + list[j][1] + " " + list[j][2] + " " + list[j][3] + " "
                  ok
             next
             if right(s,1) = " "
                s = left(s,len(s)-1)
             ok
             p = p + 1
             dgraph[p] = s
        next

```

Output:

```txt

a e : a c 9 -> c d 11 -> d e 6 cost : 26

```



## Ruby

This solution is incorrect.  Since the path is directed and f is only a sink, f cannot be in the middle of a path.

{{works with|Ruby|1.9.2+}} (for INFINITY)
Notes for this solution:
* At every iteration, the next minimum distance node found by linear traversal of all nodes, which is inefficient.

```ruby
class Graph
  Vertex = Struct.new(:name, :neighbours, :dist, :prev)

  def initialize(graph)
    @vertices = Hash.new{|h,k| h[k]=Vertex.new(k,[],Float::INFINITY)}
    @edges = {}
    graph.each do |(v1, v2, dist)|
      @vertices[v1].neighbours << v2
      @vertices[v2].neighbours << v1
      @edges[[v1, v2]] = @edges[[v2, v1]] = dist
    end
    @dijkstra_source = nil
  end

  def dijkstra(source)
    return  if @dijkstra_source == source
    q = @vertices.values
    q.each do |v|
      v.dist = Float::INFINITY
      v.prev = nil
    end
    @vertices[source].dist = 0
    until q.empty?
      u = q.min_by {|vertex| vertex.dist}
      break if u.dist == Float::INFINITY
      q.delete(u)
      u.neighbours.each do |v|
        vv = @vertices[v]
        if q.include?(vv)
          alt = u.dist + @edges[[u.name, v]]
          if alt < vv.dist
            vv.dist = alt
            vv.prev = u.name
          end
        end
      end
    end
    @dijkstra_source = source
  end

  def shortest_path(source, target)
    dijkstra(source)
    path = []
    u = target
    while u
      path.unshift(u)
      u = @vertices[u].prev
    end
    return path, @vertices[target].dist
  end

  def to_s
    "#<%s vertices=%p edges=%p>" % [self.class.name, @vertices.values, @edges]
  end
end

g = Graph.new([ [:a, :b, 7],
                [:a, :c, 9],
                [:a, :f, 14],
                [:b, :c, 10],
                [:b, :d, 15],
                [:c, :d, 11],
                [:c, :f, 2],
                [:d, :e, 6],
                [:e, :f, 9],
              ])

start, stop = :a, :e
path, dist = g.shortest_path(start, stop)
puts "shortest path from #{start} to #{stop} has cost #{dist}:"
puts path.join(" -> ")
```


```txt
shortest path from a to e has cost 20:
a -> c -> f -> e
```



## Rust

This solution uses a very bare-bones, naive implementation of an adjacency list to represent the graph.


```rust
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::usize;


struct Grid<T> {
    nodes: Vec<Node<T>>,
}

struct Node<T> {
    data: T,
    edges: Vec<(usize,usize)>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    node: usize,
    cost: usize,
}

// Manually implement Ord so we get a min-heap instead of a max-heap
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type WeightedEdge = (usize, usize, usize);

impl<T> Grid<T> {
    fn new() -> Self {
        Grid { nodes: Vec::new() }
    }

    fn add_node(&mut self, data: T) -> usize {
        let node = Node {
            edges: Vec::new(),
            data: data,
        };
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    fn create_edges<'a, I>(&mut self, iterator: I) where I: IntoIterator<Item=&'a WeightedEdge> {
        for &(start,end,weight) in iterator.into_iter() {
            self.nodes[start].edges.push((end,weight));
            self.nodes[end].edges.push((start,weight));
        }

    }

    fn find_path(&self, start: usize, end: usize) -> Option<(Vec<usize>, usize)> {
        let mut dist = vec![(usize::MAX, None); self.nodes.len()];

        let mut heap = BinaryHeap::new();
        dist[start] = (0, None);
        heap.push(State {
            node: start,
            cost: 0,
        });

        while let Some(State { node, cost }) = heap.pop() {
            if node == end {
                let mut path = Vec::with_capacity(dist.len() / 2);
                let mut current_dist = dist[end];
                path.push(end);
                while let Some(prev) = current_dist.1 {
                    path.push(prev);
                    current_dist = dist[prev];
                }
                path.reverse();
                return Some((path, cost));
            }

            if cost > dist[node].0 {
                continue;
            }
            for edge in &self.nodes[node].edges {
                let next = State {
                    node: edge.0,
                    cost: cost + edge.1,
                };
                if next.cost < dist[next.node].0 {
                    dist[next.node] = (next.cost, Some(node));
                    heap.push(next);
                }
            }
        }
        None
    }
}

fn main() {
    let mut grid = Grid::new();
    let (a,b,c,d,e,f) = (grid.add_node("a"), grid.add_node("b"),
                         grid.add_node("c"), grid.add_node("d"),
                         grid.add_node("e"), grid.add_node("f"));

    grid.create_edges(&[
        (a,b,7) ,(a,c,9) ,(a,f,14),
        (b,c,10),(b,d,15),(c,d,11),
        (c,f,2) ,(d,e,6) ,(e,f,9) ,
    ]);

    let (path, cost) = grid.find_path(a,e).unwrap();

    print!("{}", grid.nodes[path[0]].data);
    for i in path.iter().skip(1) {
        print!(" -> {}", grid.nodes[*i].data);
    }
    println!("\nCost: {}", cost);

}
```

```txt

Cost: 20
a -> c -> f -> e

```



## SAS

Use network solver in SAS/OR:

```sas
/* create SAS data set */
data Edges;
   input Start $ End $ Cost;
   datalines;
a  b  7
a  c  9
a  f  14
b  c  10
b  d  15
c  d  11
c  f  2
d  e  6
e  f  9
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare sets and parameters, and read input data */
   set <str,str> LINKS;
   num cost {LINKS};
   read data Edges into LINKS=[start end] cost;
   set NODES = union {<i,j> in LINKS} {i,j};
   set SOURCES = {'a'};
   set SINKS = {'e'};
   /* <source,sink,order,from,to> */
   set <str,str,num,str,str> PATHS;

   /* call network solver */
   solve with network /
      shortpath=(source=SOURCES sink=SINKS) links=(weight=cost) out=(sppaths=PATHS);

   /* write shortest path to SAS data set */
   create data path from [source sink order from to]=PATHS cost[from,to];
quit;

/* print shortest path */
proc print data=path;
run;
```


Output:

```txt

Obs source sink order from to cost
1 a e 1 a c 9
2 a e 2 c f 2
3 a e 3 e f 9

```



## Scala

A functional implementation of Dijkstras Algorithm:


```scala
object Dijkstra {

  type Path[Key] = (Double, List[Key])

  def Dijkstra[Key](lookup: Map[Key, List[(Double, Key)]], fringe: List[Path[Key]], dest: Key, visited: Set[Key]): Path[Key] = fringe match {
    case (dist, path) :: fringe_rest => path match {case key :: path_rest =>
      if (key == dest) (dist, path.reverse)
      else {
        val paths = lookup(key).flatMap {case (d, key) => if (!visited.contains(key)) List((dist + d, key :: path)) else Nil}
        val sorted_fringe = (paths ++ fringe_rest).sortWith {case ((d1, _), (d2, _)) => d1 < d2}
        Dijkstra(lookup, sorted_fringe, dest, visited + key)
      }
    }
    case Nil => (0, List())
  }

  def main(x: Array[String]): Unit = {
    val lookup = Map(
      "a" -> List((7.0, "b"), (9.0, "c"), (14.0, "f")),
      "b" -> List((10.0, "c"), (15.0, "d")),
      "c" -> List((11.0, "d"), (2.0, "f")),
      "d" -> List((6.0, "e")),
      "e" -> List((9.0, "f")),
      "f" -> Nil
    )
    val res = Dijkstra[String](lookup, List((0, List("a"))), "e", Set())
    println(res)
  }
}
```


```txt
(26.0,List(a, c, d, e))
```




An implementation based on the functional version above that uses <code>PriorityQueue</code>. It is made functional-look:

```scala

import scala.collection.mutable

class Dijkstra[Key] {

  type PathInfo = (Double, List[Key])
  type Path = List[Key]
  type MinHeap[PathInfo] = mutable.PriorityQueue[PathInfo]

  final def dijkstra(weightedGraph: Map[Key, List[(Double, Key)]],
                      start: Key,
                      dest: Key)(implicit ord: Ordering[PathInfo]): PathInfo =
    dijkstraHelper(weightedGraph, mutable.PriorityQueue((0.0, List(start))), dest)

  @annotation.tailrec
  private final def dijkstraHelper(weightedGraph: Map[Key, List[(Double, Key)]],
                                     fringe: MinHeap[PathInfo],
                                     dest: Key,
                                     visited: Set[Key] = Set.empty[Key])(implicit ord: Ordering[PathInfo]): PathInfo = {

    def updateFringe(frng: MinHeap[PathInfo], currentDist: Double, currentPath: Path): MinHeap[PathInfo] =
      (currentPath : @unchecked) match {
        case keys @ key :: _ =>
          weightedGraph(key)
            .withFilter { case (_, k) => !visited.contains(k) }
            .map { case (d, k) => (currentDist + d, k :: keys) }  // updated PathInfo's
            .foreach { p => frng.enqueue(p) }

          frng
      }

    if (fringe.isEmpty)
      (0, Nil)
    else {
      (fringe.dequeue() : @unchecked) match {
        case (dist, path @ `dest` :: _) =>
          (dist, path.reverse)

        case (dist, path @ key :: _) =>
          dijkstraHelper(weightedGraph, updateFringe(fringe, dist, path), dest, visited + key)
      }
    }
  }

  def main(x: Array[String]): Unit = {
    val weightedGraph = Map(
      "a" -> List((7.0, "b"), (9.0, "c"), (14.0, "f")),
      "b" -> List((10.0, "c"), (15.0, "d")),
      "c" -> List((11.0, "d"), (2.0, "f")),
      "d" -> List((6.0, "e")),
      "e" -> List((9.0, "f")),
      "f" -> Nil
    )

    val res = dijkstra[String](weightedGraph, "a", "e")
    println(res)
  }
}

```

```txt
(26.0,List(a, c, d, e))
```



## Sidef

```ruby
class Graph(*args) {

    struct Node {
        String name,
        Array edges = [],
        Number dist = Inf,
        prev = nil,
        Bool visited = false,
    }

    struct Edge {
        Number weight,
        Node vertex,
    }

    has g = Hash()

    method init {
        args.each { |a|
            self.add_edge(a...)
        }
    }

    method get(name) {
        g{name}
    }

    method add_edge(a, b, weight) {
        g{a} ||= Node(name: a)
        g{b} ||= Node(name: b)
        g{a}.edges << Edge(weight, g{b})
    }

    method push_priority(a, v) {
        var i = 0
        var j = a.end
        while (i <= j) {
            var k = ((i + j) // 2)
            if (a[k].dist >= v.dist) {
                j = k-1
            }
            else {
                i = k+1
            }
        }
        a.insert(i, v)
    }

    method dijkstra(a, b) {
        g{a}.dist = 0
        var h = []
        self.push_priority(h, g{a})
        while (!h.is_empty) {
            var v = h.shift
            break if (v.name == b)
            v.visited = true
            v.edges.each { |e|
                var u = e.vertex
                if (!u.visited && (v.dist+e.weight <= u.dist)) {
                    u.prev = v
                    u.dist = (v.dist + e.weight)
                    self.push_priority(h, u)
                }
            }
        }
    }
}

var g = Graph(
    ["a", "b", 7],
    ["a", "c", 9],
    ["a", "f", 14],
    ["b", "c", 10],
    ["b", "d", 15],
    ["c", "d", 11],
    ["c", "f", 2],
    ["d", "e", 6],
    ["e", "f", 9],
)

g.dijkstra('a', 'e')

var v = g.get('e')
var a = []
while (v != nil) {
    a << v.name
    v = v.prev
}

var path = a.reverse.join
say "#{g.get('e').dist} #{path}"
```

```txt

26 acde

```



## Tailspin

A simple algorithm that traverses through all edges at every step.

```tailspin

templates shortestPaths@{graph:}
  @: [];
  [ {to: $, distance: 0, path:[]} ] -> #
  <[](0)> $@ !
  <>
    def closest: $ -> (@: $(1); $(2..-1)... -> # $@! <?($.distance <..~$@.distance>)> @: $;);
    $closest -> ..|@: $;
    def path: [ $closest.path..., $closest.to ];
    [ $... -> (<?($.to <~$closest.to>)> $!),
      $graph... -> (<?($.edge(1) <$closest.to>) ?($@shortestPaths <~[<{to: <$.edge(2)>}>]>)> $!)
        -> { to: $.edge(2), distance: $.cost + $closest.distance, path: $path} ] -> #
end shortestPaths

def edges: [
  { edge: ['a', 'b'], cost: 7 },
  { edge: ['a', 'c'], cost: 9 },
  { edge: ['a', 'f'], cost: 14 },
  { edge: ['b', 'c'], cost: 10 },
  { edge: ['b', 'd'], cost: 15 },
  { edge: ['c', 'd'], cost: 11 },
  { edge: ['c', 'f'], cost: 2 },
  { edge: ['d', 'e'], cost: 6 },
  { edge: ['e', 'f'], cost: 9 }];

def fromA: 'a' -> shortestPaths@{graph: $edges};

$fromA... -> (<{to:<'e'>}> $!) -> 'Shortest path from $.path(1); to $.to; is distance $.distance; via $.path(2..-1);
' -> !OUT::write

$fromA... -> (<{to:<'f'>}> $!) -> 'Shortest path from $.path(1); to $.to; is distance $.distance; via $.path(2..-1);
' -> !OUT::write

```

```txt

Shortest path from a to e is distance 26 via [c, d]
Shortest path from a to f is distance 11 via [c]

```



## Tcl


{{incorrect|Tcl|

 Since the path is directed and   '''f'''   is only a sink,   '''f'''   cannot be in the middle of a path.

 The original flagging was done via a simple comment (below),
 and the original author is unknown, but should be findable with some detective work via the history page.

}}


This solution is incorrect.  Since the path is directed and f is only a sink, f cannot be in the middle of a path.


Note that this code traverses the entire set of unrouted nodes at each step, as this is simpler than computing the subset that are reachable at each stage.

```tcl
proc dijkstra {graph origin} {
    # Initialize
    dict for {vertex distmap} $graph {
	dict set dist $vertex Inf
	dict set path $vertex {}
    }
    dict set dist $origin 0
    dict set path $origin [list $origin]

    while {[dict size $graph]} {
	# Find unhandled node with least weight
	set d Inf
	dict for {uu -} $graph {
	    if {$d > [set dd [dict get $dist $uu]]} {
		set u $uu
		set d $dd
	    }
	}

	# No such node; graph must be disconnected
	if {$d == Inf} break

	# Update the weights for nodes lead to by the node we've picked
	dict for {v dd} [dict get $graph $u] {
	    if {[dict exists $graph $v]} {
		set alt [expr {$d + $dd}]
		if {$alt < [dict get $dist $v]} {
		    dict set dist $v $alt
		    dict set path $v [list {*}[dict get $path $u] $v]
		}
	    }
	}

	# Remove chosen node from graph still to be handled
	dict unset graph $u
    }
    return [list $dist $path]
}
```

Showing the code in use:

```tcl
proc makeUndirectedGraph arcs {
    # Assume that all nodes are connected to something
    foreach arc $arcs {
	lassign $arc v1 v2 cost
	dict set graph $v1 $v2 $cost
	dict set graph $v2 $v1 $cost
    }
    return $graph
}
set arcs {
    {a b 7} {a c 9} {b c 10} {b d 15} {c d 11}
    {d e 6} {a f 14} {c f 2} {e f 9}
}
lassign [dijkstra [makeUndirectedGraph $arcs] "a"] costs path
puts "path from a to e costs [dict get $costs e]"
puts "route from a to e is: [join [dict get $path e] { -> }]"
```

Output:

```txt

path from a to e costs 20
route from a to e is: a -> c -> f -> e

```



## VBA


```VB
Class Branch
Public from As Node '[according to Dijkstra the first Node should be closest to P]
Public towards As Node
Public length As Integer '[directed length!]
Public distance As Integer '[from P to farthest node]
Public key As String
Class Node
Public key As String
Public correspondingBranch As Branch
Const INFINITY = 32767
Private Sub Dijkstra(Nodes As Collection, Branches As Collection, P As Node, Optional Q As Node)
    'Dijkstra, E. W. (1959). "A note on two problems in connexion with graphs".
    'Numerische Mathematik. 1: 269–271. doi:10.1007/BF01386390.
    'http://www-m3.ma.tum.de/twiki/pub/MN0506/WebHome/dijkstra.pdf
    'Problem 2. Find the path of minimum total length between two given nodes
    'P and Q.
    'We use the fact that, if R is a node on the minimal path from P to Q, knowledge
    'of the latter implies the knowledge of the minimal path from P to A. In the
    'solution presented, the minimal paths from P to the other nodes are constructed
    'in order of increasing length until Q is reached.
    'In the course of the solution the nodes are subdivided into three sets:
    'A. the nodes for which the path of minimum length from P is known; nodes
    'will be added to this set in order of increasing minimum path length from node P;
    '[comments in square brackets are not by Dijkstra]
    Dim a As New Collection '[of nodes (vertices)]
    'B. the nodes from which the next node to be added to set A will be selected;
    'this set comprises all those nodes that are connected to at least one node of
    'set A but do not yet belong to A themselves;
    Dim b As New Collection '[of nodes (vertices)]
    'C. the remaining nodes.
    Dim c As New Collection '[of nodes (vertices)]
    'The Branches are also subdivided into three sets:
    'I the Branches occurring in the minimal paths from node P to the nodes
    'in set A;
    Dim I As New Collection '[of Branches (edges)]
    'II the Branches from which the next branch to be placed in set I will be
    'selected; one and only one branch of this set will lead to each node in set B;
    Dim II As New Collection '[of Branches (edges)]
    'III. the remaining Branches (rejected or not yet considered).
    Dim III As New Collection '[of Branches (edges)]
    Dim u As Node, R_ As Node, dist As Integer
    'To start with, all nodes are in set C and all Branches are in set III. We now
    'transfer node P to set A and from then onwards repeatedly perform the following
    'steps.
    For Each n In Nodes
        c.Add n, n.key
    Next n
    For Each e In Branches
        III.Add e, e.key
    Next e
    a.Add P, P.key
    c.Remove P.key
    Set u = P
    Do
        'Step 1. Consider all Branches r connecting the node just transferred to set A
        'with nodes R in sets B or C. If node R belongs to set B, we investigate whether
        'the use of branch r gives rise to a shorter path from P to R than the known
        'path that uses the corresponding branch in set II. If this is not so, branch r is
        'rejected; if, however, use of branch r results in a shorter connexion between P
        'and R than hitherto obtained, it replaces the corresponding branch in set II
        'and the latter is rejected. If the node R belongs to set C, it is added to set B and
        'branch r is added to set II.
        For Each r In III
            If r.from Is u Then
                Set R_ = r.towards
                If Belongs(R_, c) Then
                    c.Remove R_.key
                    b.Add R_, R_.key
                    Set R_.correspondingBranch = r
                    If u.correspondingBranch Is Nothing Then
                        R_.correspondingBranch.distance = r.length
                    Else
                        R_.correspondingBranch.distance = u.correspondingBranch.distance + r.length
                    End If
                    III.Remove r.key '[not mentioned by Dijkstra ...]
                    II.Add r, r.key
                Else
                    If Belongs(R_, b) Then '[initially B is empty ...]
                        If R_.correspondingBranch.distance > u.correspondingBranch.distance + r.length Then
                            II.Remove R_.correspondingBranch.key
                            II.Add r, r.key
                            Set R_.correspondingBranch = r '[needed in step 2.]
                            R_.correspondingBranch.distance = u.correspondingBranch.distance + r.length
                        End If
                    End If
                End If
            End If
        Next r
        'Step 2. Every node in set B can be connected to node P in only one way
        'if we restrict ourselves to Branches from set I and one from set II. In this sense
        'each node in set B has a distance from node P: the node with minimum distance
        'from P is transferred from set B to set A, and the corresponding branch is transferred
        'from set II to set I. We then return to step I and repeat the process
        'until node Q is transferred to set A. Then the solution has been found.
        dist = INFINITY
        Set u = Nothing
        For Each n In b
            If dist > n.correspondingBranch.distance Then
                dist = n.correspondingBranch.distance
                Set u = n
            End If
        Next n
        b.Remove u.key
        a.Add u, u.key
        II.Remove u.correspondingBranch.key
        I.Add u.correspondingBranch, u.correspondingBranch.key
    Loop Until IIf(Q Is Nothing, a.Count = Nodes.Count, u Is Q)
    If Not Q Is Nothing Then GetPath Q
End Sub
Private Function Belongs(n As Node, col As Collection) As Boolean
    Dim obj As Node
    On Error GoTo err
        Belongs = True
        Set obj = col(n.key)
        Exit Function
err:
        Belongs = False
End Function
Private Sub GetPath(Target As Node)
    Dim path As String
    If Target.correspondingBranch Is Nothing Then
        path = "no path"
    Else
        path = Target.key
        Set u = Target
        Do While Not u.correspondingBranch Is Nothing
            path = u.correspondingBranch.from.key & " " & path
            Set u = u.correspondingBranch.from
        Loop
        Debug.Print u.key, Target.key, Target.correspondingBranch.distance, path
    End If
End Sub
Public Sub test()
    Dim a As New Node, b As New Node, c As New Node, d As New Node, e As New Node, f As New Node
    Dim ab As New Branch, ac As New Branch, af As New Branch, bc As New Branch, bd As New Branch
    Dim cd As New Branch, cf As New Branch, de As New Branch, ef As New Branch
    Set ab.from = a: Set ab.towards = b: ab.length = 7: ab.key = "ab": ab.distance = INFINITY
    Set ac.from = a: Set ac.towards = c: ac.length = 9: ac.key = "ac": ac.distance = INFINITY
    Set af.from = a: Set af.towards = f: af.length = 14: af.key = "af": af.distance = INFINITY
    Set bc.from = b: Set bc.towards = c: bc.length = 10: bc.key = "bc": bc.distance = INFINITY
    Set bd.from = b: Set bd.towards = d: bd.length = 15: bd.key = "bd": bd.distance = INFINITY
    Set cd.from = c: Set cd.towards = d: cd.length = 11: cd.key = "cd": cd.distance = INFINITY
    Set cf.from = c: Set cf.towards = f: cf.length = 2: cf.key = "cf": cf.distance = INFINITY
    Set de.from = d: Set de.towards = e: de.length = 6: de.key = "de": de.distance = INFINITY
    Set ef.from = e: Set ef.towards = f: ef.length = 9: ef.key = "ef": ef.distance = INFINITY
    a.key = "a"
    b.key = "b"
    c.key = "c"
    d.key = "d"
    e.key = "e"
    f.key = "f"
    Dim testNodes As New Collection
    Dim testBranches As New Collection
    testNodes.Add a, "a"
    testNodes.Add b, "b"
    testNodes.Add c, "c"
    testNodes.Add d, "d"
    testNodes.Add e, "e"
    testNodes.Add f, "f"
    testBranches.Add ab, "ab"
    testBranches.Add ac, "ac"
    testBranches.Add af, "af"
    testBranches.Add bc, "bc"
    testBranches.Add bd, "bd"
    testBranches.Add cd, "cd"
    testBranches.Add cf, "cf"
    testBranches.Add de, "de"
    testBranches.Add ef, "ef"
    Debug.Print "From", "To", "Distance", "Path"
    '[Call Dijkstra with target:]
    Dijkstra testNodes, testBranches, a, e
    '[Call Dijkstra without target computes paths to all reachable nodes:]
    Dijkstra testNodes, testBranches, a
    GetPath f
End Sub
```
```txt
From          To            Distance      Path
a             e              26           a c d e
a             f              11           a c f
```


## zkl


```zkl
const INF=(0).MAX;
fcn dijkstra(graph,start,dst){
   Q   :=graph.copy();
   prev:=graph.keys.pump(Dictionary().add.fp1(Void));
   dist:=graph.keys.pump(Dictionary().add.fp1(INF));
   dist[start]=0;
   while(Q){
      Q.reduce('wrap(min,[(v,_)],ru){
           if((d:=dist[v])<min){ ru.set(v); d } else min },
        INF,ru:=Ref(Void));
      if(not u:=ru.value) return("Can't get there");
      if(u==dst){
         S:=List(); do{ S.append(u); u=prev[u]; }while(u);
	 return(S.reverse());
      }
      Q.del(u);
      foreach v,len in (graph[u]){  // (neighborVertex,len to neighbor)...
	  alt:=dist[u] + len;
	  if(alt<dist[v]){ dist[v]=alt; prev[v]=u; }
      }
   }
}
```


```zkl
graph:=Dictionary(  // directed graph
   "a", T(T("b", 7.0), T("c", 9.0), T("f",14.0)),
   "b", T(T("c",10.0), T("d",15.0)),
   "c", T(T("d",11.0), T("f", 2.0)),
   "d", T(T("e", 6.0)),
   "e", T(T("f", 9.0)),
   "f", T,
);
dijkstra(graph,"a","e").println();
dijkstra(graph,"e","a").println();
```

```txt

L("a","c","d","e")
Can't get there

```


