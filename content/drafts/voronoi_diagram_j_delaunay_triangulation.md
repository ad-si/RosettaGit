+++
title = "Voronoi diagram/J/Delaunay triangulation"
description = ""
date = 2012-02-02T04:19:15Z
aliases = []
[extra]
id = 11341
[taxonomies]
categories = []
tags = []
+++


```J

NB. WARNING: THE gnuplot VERB OVERWRITES mesh.dat IN CURRENT DIRECTORY

NB. examples:   demo_delaunay 999     NB. plot triangulation of 999 pseudo-random (repeatable) planar points
NB.             demo_voronoi 999      NB. plot Voronoi cells, 8 seconds on a 2009 Thinkpad


Note 'mesh'

http://www.scribd.com/keyven/d/63898960/110-The-Bowyer-Watson-algorithm

This version generates and discards a bounding rectangle.
Another version should preserve a given boundary.

'elements nodes'=: mesh
Elements are rank 2.  An element is an item, the ordered node indexes.
Nodes are rank 2.  Columns of nodes are the coordinates.

Input to triangulate are the nodes, output is a mesh.

The lines suitable for arbitrary dimension (greater than 1) are marked
with NB. n-D
)

rank=: #@:$
mp=: $:~ : (+/ .*)
det=: -/ .*                             NB. monad operates on rank 2
all=: *./
angle=: 12&o.@:(j./)

simplex_impossible=: <:/@:$ NB. need more nodes than coordinates

delaunay=:triangulate=: 0&$: : (4 : 0)
NB. if x retain the added bounding box else discard it
NB. nodes=. y
if. (simplex_impossible +. 2&(~: rank)) y do. NB. n-D
 smoutput 'Use: input rank 2 node matrix, output Delaunay mesh'
 2#a:
 return.
end.
extrema=. (<./ ,: >./) y                NB. n-D
if. 0 (e. -/) extrema do.               NB. n-D
 smoutput 'Error: colinear nodes'
 '';y
 return.
end.
NB. make initial Delaunay triangles for Bowyer-Watson algorithm.
extrema=. (((+ {.) ,: (-~ {:))~ ([: -: -/)) extrema NB. expand the outer bound NB. n-D
DIMENSION=. {: $ extrema                            NB. n-D
t=. 2 #:@:i.@:^ DIMENSION                           NB. n-D
CORNERS=. t ({:@:] (+"1) (*"1 -/)) extrema          NB. n-D
N=. CORNERS , y                                     NB. n-D
NB. I believe the code works for all dimensions to here.
if. (0 3,:1 2) </@(mp"1~)@:(-/"2)@:{ CORNERS do.
 NB. the 0 3 diagonal is shorter
 E=. 0 3 1 ,: 3 0 2
else.
 NB. the 1 2 diagonal is not longer
 E=. 1 2 3 ,: 2 1 0
end.
NB. The bounding grid exceeds the nodes.
NB. Therefor all nodes will be within an existing simplex.
NB. Find the enclosing circumcircles, delete edges, reconnect to new node.
NB. Elements of the "triangle net" numbered counterclockwise.
for_i. CORNERS (+ i.)&# y do.  NB. improve this O(n^2) search  NB. n-D
 n=. i { N                     NB. n-D
 NB. matrix of differences has smaller shape of DIMENSION+1 .  Faster?
 negative_determinants=. 1 (0 > [: det (,~ (, mp))"1) n ,"2~ E { N NB. n-D
 discarded_elements=. E (#~ -.) negative_determinants NB. n-D
 NB. Remove the shared edges of the discarded elements.
 NB. This algorithm retains the entire simplexes rather than a face list.
 NB. Just append the new simplexes built from the new node and the nodes
 NB. of the discarded elements.
 E=. negative_determinants # E          NB. n-D
 hull=. ~. , discarded_elements         NB. n-D
 NB. angles extracts the polar angle from complex representation of the
 NB. points of the polygonal hole first shifted to the inserted node.
 angles=. n (angle@:-"1~ hull&{) N
 NB.E;N;y;n;i;hull;angles return. NB. debug aid.
 E=. E , i ,. 2&([\) ($~ >:@:#) hull /: angles
end.
NB. retain CORNER nodes?
if. x do. E;N else. y ;~ (#~ 0 (all . <:) |:)@:(-&(#CORNERS)) E end. NB. n-D
)

gnuplot=: ''&$: : (4 : 0)
y (1!:2 <) 'mesh.dat'
2!:0 'gnuplot --persist -e "unset key ; plot ''mesh.dat'' w l"'
EMPTY
)

show_mesh=: gnuplot@:string_plane
plot_plane_mesh=: (show_mesh~  (| i.@:>:))~
plot_tria_mesh=: 3&plot_plane_mesh
plot_quad_mesh=: 4&plot_plane_mesh

ruin_negative=: =&'_'`(,:&'-')}

NB. format plane mesh as gnuplot data file
string_plane=: 0 1 2 0&$: : (4 : 0)  NB. y is a mesh
'e n'=. y
NB. s=. 0j_2 ": n {~ x {"1 e  NB. pretty, but stinky.
s=. ": n {~ x {"1 e
s=. ,@:(,.&LF)@:(,"2)@:(,"1&LF) s
ruin_negative s
)


random_points=: 1 : '0 u@:$~ 2 ,~ ]'

NB. Use:     demo_delaunay number_of_nodes
demo_delaunay=: [: plot_tria_mesh [: triangulate ?.random_points

NB. return boxed Voronoi polygons determined from Delaunay mesh.
voronoi=: 3 : 0                    NB. voronoi nodes NB. 2 dimensional
'E N'=. 1 triangulate y
NB. boxed element indexes with shared node
NB. bei: the common node is the box index
bei=. E <@:I."1@:(e."0 1/~ i.@:#) N
NB. The convex hull of the circle centers forms the Voronoi cell.
t=. ([: {&N {&E)&.>4}.bei
squares=. mp"1&.>t
t=. ,&1"1&.> t
a2=. +:@:det&.> t
Sx=. squares det@:((<0 1 2;0)})"1 2&.> t
Sy=. squares det@:((<0 1 2;1)})"1 2&.> t
centers=. a2 %&.>~ Sx ,.&.> Sy
convex_hull&.>centers
)

NB. swap=: <@:[ C. ]                    NB. swap=: (C.~ <)~
NB. move_to_front=: (C.&.|.~ <:@:-)~
NB. 'abfdecghijkl' -: 2 5 swap 'abcdefghijkl'
swap=: ({~ |.)~`[`]}                    NB. in place possibility
move_to_front=: ((({~ _1&|.)~`[`]})~ i.@:>:)~ NB. in place possibility
crossproduct=: 1 |. ([ * 1 |. ]) - ] * 1 |. [ NB. j forum
wind=: {:@:crossproduct&(,&0) NB. positive if CCL
smallest_by_coordinate=: 4 : '(#~ (= <./)@:(x&{"1)) y'

convex_hull=: 3 : 0         NB. Graham Scan  y are the points
y=. ~. y                    NB. Implements Robert Sedgewick pseudo-code
N=. # y
start=. , > smallest_by_coordinate&.>/ 0 ; 1 ; y
y=. y -. start
y=. y ([ /: angle@:-"1) start
y=. y (] , ,) start
M=. 1
i=. 2
while. i <: N do.
 while. 0 >: wind/((M,i)&{ -"1 (<:M)&{) y do.
  if. 1 = M do.
   y =. (M,i) swap y
   i =. >: i
  else.
   M =. <: M
  end.
 end.
 M =. >: M
 y =. (M,i) swap y
 i =. >: i
end.
M {. y
)

test_convex_hull=: gnuplot@:string_polygon@:convex_hull NB. test_convex_hull nodes

string_polygon=: ruin_negative@:,@:(,.&LF)@:":@:(, {.)  NB. y are nodes
string_voronoi=: string_polygon@:(string_polygon@>)  NB. y are the boxes of nodes

draw_voronoi=: gnuplot@:string_voronoi@:voronoi
demo_voronoi=: [: draw_voronoi ?.random_points

```

[[File:voronoi.png]]
