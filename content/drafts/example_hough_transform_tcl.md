+++
title = "Example:Hough transform/Tcl"
description = ""
date = 2013-03-30T15:20:18Z
aliases = []
[extra]
id = 8041
[taxonomies]
categories = []
tags = []
+++

[[implementation of task::Hough transform| ]]
{{Programming-example-page|Hough transform|language=Tcl}}


```tcl
package require Tk

set PI 3.1415927
proc HoughTransform {src trg {fieldColor "#000000"}} {
    global PI

    set w [image width $src]
    set h [image height $src]
    set targetH [expr {int(hypot($w, $h)/2)}]

    # Configure the target buffer
    $trg configure -width 360 -height $targetH
    $trg put $fieldColor -to 0 0 359 [expr {$targetH-1}]

    # Iterate over the target's space of pixels.
    for {set rho 0} {$rho < $targetH} {incr rho} {
	set row {}
	for {set theta 0} {$theta < 360} {incr theta} {
	    set cos [expr {cos($theta/180.0*$PI)}]
	    set sin [expr {sin($theta/180.0*$PI)}]
	    set totalRed 0
	    set totalGreen 0
	    set totalBlue 0
	    set totalPix 0

	    # Sum the colors of the line with equation x*cos(θ) + y*sin(θ) = ρ
	    if {$theta<45 || ($theta>135 && $theta<225) || $theta>315} {
		# For these half-quadrants, it's better to iterate by 'y'
		for {set y 0} {$y<$h} {incr y} {
		    set x [expr {
			$w/2 + ($rho - ($h/2-$y)*$sin)/$cos
		    }]
		    if {$x < 0 || $x >= $w} continue
		    set x [expr {round($x)}]
		    if {$x == $w} continue
		    incr totalPix
		    lassign [$src get $x $y] r g b
		    incr totalRed $r
		    incr totalGreen $g
		    incr totalBlue $b
		}
	    } else {
		# For the other half-quadrants, it's better to iterate by 'x'
		for {set x 0} {$x<$w} {incr x} {
		    set y [expr {
			$h/2 - ($rho - ($x-$w/2)*$cos)/$sin
		    }]
		    if {$y < 0 || $y >= $h} continue
		    set y [expr {round($y)}]
		    if {$y == $h} continue
		    incr totalPix
		    lassign [$src get $x $y] r g b
		    incr totalRed $r
		    incr totalGreen $g
		    incr totalBlue $b
		}
	    }

	    # Convert the summed colors back into a pixel for insertion into
	    # the target buffer.
	    if {$totalPix > 0} {
		set totalPix [expr {double($totalPix)}]
		set col [format "#%02x%02x%02x" \
			[expr {round($totalRed/$totalPix)}] \
			[expr {round($totalGreen/$totalPix)}] \
			[expr {round($totalBlue/$totalPix)}]]
	    } else {
		set col $fieldColor
	    }
	    lappend row $col
	}
	$trg put [list $row] -to 0 $rho
    }
}
```



### Demonstration Code

Takes the name of the image to apply the transform to as an argument. If using PNG images,

{{works with|Tk|8.6}} or TkImg

```tcl
# Demonstration code
if {[catch {
    package require Tk 8.6; # Just for PNG format handler
}] == 1} then {catch {
    package require Img
}}
# If neither Tk8.6 nor Img, then only GIF and PPM images can be loaded

set f [lindex $argv 0]
image create photo srcImg -file $f
image create photo targetImg
pack [labelframe .l1 -text Source] [labelframe .l2 -text Target]
pack [label .l1.i -image srcImg]
pack [label .l2.i -image targetImg]
# Postpone until after we've drawn ourselves
after idle HoughTransform srcImg targetImg [lrange $argv 1 end]
```


[[Image:Hough-Pentagon-Tcl-Results.gif|thumb|left|360x200px|Image produced by Tcl implementation of the Hough transform when applied to the sample pentagon image.]]
<br style="clear:both" />
