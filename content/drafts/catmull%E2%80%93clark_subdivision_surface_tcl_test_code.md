+++
title = "Catmull–Clark subdivision surface/Tcl Test Code"
description = ""
date = 2010-01-18T01:35:36Z
aliases = []
[extra]
id = 5337
[taxonomies]
categories = []
tags = []
+++

This is the test code for the [[Tcl]] solution of the [[Catmull–Clark subdivision surface#Tcl|Catmull-Clark]] problem.

{{libheader|Tk}}
==Utility Functions==

```tcl
package require Tk
 
# A simple-minded ordering function for faces
proc orderf {points face1 face2} {
    set d1 [set d2 0.0]
    foreach p [selectFrom $points $face1] {
	lassign $p x y z
	set d1 [expr {$d1 + sqrt($x*$x + $y*$y + $z*$z)}]
    }
    foreach p [selectFrom $points $face2] {
	lassign $p x y z
	set d2 [expr {$d2 + sqrt($x*$x + $y*$y + $z*$z)}]
    }
    expr {$d1<$d2 ? -1 : $d1>$d2 ? 1 : 0}
}
 
# Plots a net defined in points-and-faces fashion
proc visualizeNet {w points faces args} {
    foreach face [lsort -command [list orderf $points] $faces] {
	set c {}
	set polyCoords [selectFrom $points $face]
	set sum {[list 0. 0. 0.]}
	set centroid [centroid $polyCoords]
	foreach coord $polyCoords {
	    lassign $coord x y z
	    lappend c \
		[expr {200. + 190. * (0.867 * $x - 0.9396 * $y)}] \
		[expr {200 + 190. * (0.5 * $x + 0.3402 * $y - $z)}]
	}
	lassign $centroid x y z
	set depth [expr {int(255*sqrt($x*$x + $y*$y + $z*$z) / sqrt(3.))}]
	set grey [format #%02x%02x%02x $depth $depth $depth]
	$w create polygon $c -fill $grey {*}$args
    }
}
```


==Demonstration==
(Using the utility functions from above, plus the code from the main solution page.)

```tcl
# Make a display surface
pack [canvas .c -width 400 -height 400 -background #7f7f7f]
 
# Points to define the unit cube
set points {
    {0.0 0.0 0.0}
    {1.0 0.0 0.0}
    {1.0 1.0 0.0}
    {0.0 1.0 0.0}
    {0.0 0.0 1.0}
    {1.0 0.0 1.0}
    {1.0 1.0 1.0}
    {0.0 1.0 1.0}
}
foreach pt $points {
    lassign $pt x y z
    lappend points [list [expr {0.25 + 0.5*$x}] [expr {0.25 + 0.5*$y}] $z]
}
	
# Try removing {1 2 6 5} to demonstrate holes.
set faces {
    {0 8 9 1}
    {1 9 10 2}
    {2 10 11 3}
    {3 11 8 0}
    {0 1 5 4}
    {1 2 6 5}
    {2 3 7 6}
    {3 0 4 7}
    {4 5 13 12}
    {5 6 14 13}
    {6 7 15 14}
    {7 4 12 15}
    {8 9 13 12}
    {9 10 14 13}
    {10 11 15 14}
    {11 8 12 15}
}
 
# Show the initial layout
visualizeNet .c $points $faces -outline white -fill {}
 
# Apply the Catmull-Clark algorithm to generate a new surface
lassign [CatmullClark $points $faces] points2 faces2
 
## Uncomment the next line to get the second level of subdivision
lassign [CatmullClark $points2 $faces2] points2 faces2
lassign [CatmullClark $points2 $faces2] points2 faces2
 
# Visualize the new surface
visualizeNet .c $points2 $faces2 -outline #0000cc
```

==Program Output==
[[File:Tcl-Catmull.png]]


This figure shows the result of running the code on this page.
