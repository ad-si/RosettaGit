+++
title = "Execute Brain****/Tcl"
description = ""
date = 2010-11-18T10:39:18Z
aliases = []
[extra]
id = 4177
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}[[implementation of task::RCBF| ]]
This [[Tcl]] [[Brainfuck]] interpreter is derived from code on [http://wiki.tcl.tk/9490 The Tcler's Wiki], and is written to be short but not particularly clear.

To use it, save it to a file (e.g., called “bf.tcl”) and run that against <tt>tclsh</tt> with either the name of the file containing the BF program or just input the program on stdin; the program will only begin execution after you do end-of-file (however that's done on your OS). For example:
  tclsh8.5 bf.tcl helloWorld.bf
<br clear=all>
==Interpreter Implementation==
{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5
fconfigure stdout -buffering none
fconfigure stdin -buffering none
if {![llength $argv]} {
    set p [split [read stdin] {}]
} else {
    set fd [open [lindex $argv 0]]
    set p [split [read $fd] {}]
    close $fd
}
set d [lrepeat 30000 0]
set dc 0
for {set pc 0} {$pc < [llength $p]} {incr pc} {
    switch [lindex $p $pc] {
	">" {
	    incr dc
	}
	"<" {
	    incr dc -1
	}
	"+" {
	    lset d $dc [expr {[lindex $d $dc] + 1}]
	}
	"-" {
	    lset d $dc [expr {[lindex $d $dc] - 1}]
	}
	"." {
	    puts -nonewline [format "%c" [lindex $d $dc]]
	}
	"," {
	    lset d $dc [scan [read stdin 1] "%c"]
	}
	"\[" {
	    if {![lindex $d $dc]} {
		incr pc
		for {set n 0} {$n || [lindex $p $pc] ne "\]"} {incr pc} {
		    switch -- [lindex $p $pc] "\[" {incr n} "\]" {incr n -1}
		}
	    }
	}
	"\]" {
	    if {[lindex $d $dc]} {
		incr pc -1
		for {set n 0} {$n || [lindex $p $pc] ne "\["} {incr pc -1} {
		    switch -- [lindex $p $pc] "\[" {incr n -1} "\]" {incr n}
		}
	    }
	}
    }
}
```

