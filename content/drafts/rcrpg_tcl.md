+++
title = "RCRPG/Tcl"
description = ""
date = 2010-11-18T10:42:09Z
aliases = []
[extra]
id = 4230
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}[[implementation of task::RCRPG| ]]
This [[Tcl]] version of [[RCRPG]] was typed and tested on a cellphone, so pardon my brevity.


```Tcl
#!/usr/bin/env tclsh
proc help args {
   return "RosettaCode 3D single user dungeon in Tcl. Type a command:
    e(ast), w(est), n(orth), s(outh), u(p), d(own)
   t(ake) sth|all, drop sth|all
   a(ttack) direction: to break a hole in the wall (needs sledge)
   describe   : get told where you are
   i(nventory) : get told what you have
   For going up, you also need a ladder."}

proc main argv {
   Room 0,0,0 StartRoom sledge
   Room 1,1,5 PrizeRoom {gold gold gold}
   array set ::Self {coords 0,0,0 items {}}
   foreach i {east west north south up down} {
       alias $i = go $i
       alias [string index $i 0] = go $i
   }
   foreach {new old} {a attack i inventory t take} {
       alias $new = $old
   }
   puts [help]
   describe
   while 1 { #-- Read-Eval-Print loop
       puts -nonewline "> "; flush stdout
       catch [gets stdin] res
       if {$res ne ""} {puts $res}
   }
}

proc Room {xyz {name {}} {items {}}} { #-- "constructor"
   if {$name eq ""} {set name R.[incr ::R()]}
   if {![llength $items]} {set items [lpick {sledge {} ladder gold}]}
   array set ::R [list $xyz.name $name $xyz.items $items $xyz.exits {}]
}

proc Inverse where {
   switch -- $where {
      east  {I west}  west  {I east}
      north {I south} south {I north}
      up    {I down}  down  {I up}
      default {error "bad direction $where"}
   }
}

proc Normalize where {
   switch -- $where {
      e {I east} w {I west} n {I north} s {I south}
      u {I up}   d {I down}
      default {I $where}
   }
}

proc attack where {
   if {"sledge" ni $::Self(items)} {return "need sledge to attack!"}
   set where [Normalize $where]
   set xyz $::Self(coords)
   if {$where in $::R($xyz.exits)} {
       puts "No need to attack.."
       return [go $where]
   }
   if {$where eq "up" && "ladder" ni $::R($xyz.items)} {
       return "You can't go up without a ladder."
   }
   lappend ::R($xyz.exits) $where
   go $where 0
   lappend ::R($::Self(coords).exits) [Inverse $where]
   describe
}

proc describe {} {
   set xyz  $::Self(coords)
   set name $::R($xyz.name)
   set items [pretty $::R($xyz.items)]
   puts "You are in $name ($xyz) and see $items."
   if {$name eq "PrizeRoom"} {
       puts "Congratulations - you won!"
       exit
   }
   set exits $::R($xyz.exits)
   if {![llength $exits]} {set exits nowhere}
   puts "There are exits towards: [join $exits {, }]"
   inventory
}

proc drop what {
   set xyz $::Self(coords)
   if {$what eq "all"} {set what $::Self(items)}
   foreach i $what {
       if {$i ni $::Self(items)} {return "You don't carry a $i."}
       lremove ::Self(items)   $i
       lappend ::R($xyz.items) $i
   }
   inventory
}

proc go {where {describe 1}} {
   set where [Normalize $where]
   if {$where ni $::R($::Self(coords).exits)} {
      return "No exit $where, consider an attack."
   }
   if {$where eq "up" && "ladder" ni $::R($::Self(coords).items)} {
       return "You can't go up without a ladder."
   }
   foreach {x y z} [split $::Self(coords) ,] break
   switch -- $where {
       east  {incr x} west  {incr x -1}
       north {incr y} south {incr y -1}
       up    {incr z} down  {incr z -1}
   }
   set xyz $x,$y,$z
   if {![info exists ::R($xyz.name)]} {Room $xyz}
   set ::Self(coords) $xyz
   if {$describe} describe
}

proc inventory {} {
   return "You have [pretty $::Self(items)]."
}

proc name what {
   set ::R($::Self(coords).name) $what
   return "This room is now named $what."
}

proc take what {
   set xyz $::Self(coords)
   if {$what eq "all"} {set what $::R($xyz.items)}
   foreach i $what {
       if {$i ni $::R($xyz.items)} {return "There is no $i here."}
       lremove ::R($xyz.items) $i
       lappend ::Self(items)   $i
   }
   inventory
}

#--- general utilities
proc alias {new = args} {interp alias {} $new {} {*}$args}
proc I     x   {return $x} ;# identity
proc lpick lst {lindex $lst [expr {int(rand()*[llength $lst])}]}
proc lremove {_lst what} {
   upvar 1 $_lst lst
   set pos [lsearch -exact $lst $what]
   set lst [lreplace $lst $pos $pos]
}
proc pretty lst {
   if {![llength $lst]} {return nothing}
   foreach i $lst {lappend tmp "a $i"}
   regsub {(.+),} [join $tmp ", "] {\1, and}
}

main $argv
```


==Alternative Version==
The following version is functionally identical, but uses a setter/getter function "@" to hide away the data representation
from most of the code (except in the definition of "@" itself).
Examples:
    @ my coords $x,$y,$z         ;#-- modify an "instance variable"
    set items [@ my items]       ;#-- items I carry
    set items [@ here items]     ;#-- items in the current room
    set items [@ $x,$y,$z items] ;#-- items in the given room
    lappend [@ my items &] teacup ;#-- returns a reference  


```Tcl
proc help args {
    return "RosettaCode 3D single-user dungeon in Tcl. Type a command:
     e(ast), s(outh), n(orth), w(est), u(p), d(own)
     t(ake) something|all, drop something|all
     a(ttack) direction: to break a wall (needs a sledge)
     d(escribe):         get told where you are
     help:               get this message
     i(nventory):        get told what you have
     name something:     give the current room another name
     For going up, you also need a ladder."}

proc main argv {
    Room  0,0,0  StartRoom  sledge
    Room  1,1,5  PrizeRoom  {gold gold gold}
    @ my coords 0,0,0
    @ my items  {}
    foreach i {east west north south up down} {
	alias $i                  = go $i
	alias [string index $i 0] = $i
    }
    foreach {new old} {a attack d describe i inventory t take} {
	alias $new = $old
    }
    puts [help]
    describe
    while 1 {          #-- REPL: Read-Eval-Print Loop
	puts -nonewline "> "; flush stdout
	catch [gets stdin] res
	if {$res ne ""} {puts $res}
    }
}

proc Room {xyz {name {}} {items {}}} { #-- "constructor"
    if {$name eq ""} {set name R.[incr ::ID]}
    if {$items eq {}} {set items [lpick {sledge {} ladder gold}]}
    @ $xyz name  $name
    @ $xyz items $items
    @ $xyz exits {}
}

proc Inverse where {
    switch -- $where {
	east  {I west}  west  {I east}
	north {I south} south {I north}
	up    {I down}  down  {I up}
	default {error "No inverse defined for $where"}
    }
}

proc Normalize where {
    switch -- $where {
	e {I east} w {I west} n {I north} s {I south} u {I up} d {I down}
	default {I $where}
    }
}

proc @ {coords what {value --}} { #-- universal setter/getter
    if {$coords eq "my"} {
	if {$value eq "--"} {return $::Self($what)}
	return [expr {$value eq "&"? "::Self($what)" : [set ::Self($what) $value]}]
    }
    if {$coords eq "here"} {set coords $::Self(coords)}
    if {$value eq "&"}     {return ::R($coords.$what)} ;# reference
    if {$value eq "--"} {
	    set ::R($coords.$what)
    } else {set ::R($coords.$what) $value}
}

#------------------- commands in Afferbeck Lauder
proc attack where {
    set where  [Normalize $where]
    set coords [@ my coords]
    if {$where in [@ $coords exits]} {
	puts   "No need to attack $where, the road is open."
	return [go $where]
    } elseif {"sledge" ni [@ my items]} {
	return "You can't attack without a sledge."
    }
    if {$where eq "up"} {
	if {"ladder" ni [@ $coords items]} {
	    return "You can't go up without a ladder."
	}
    }
    lappend [@ here exits &] $where
    go $where 0 ;#-- describe later
    lappend [@ here exits &] [Inverse $where]
    describe
}

proc describe {} {
    set coords [@ my coords]
    set name   [@ here name]
    puts "You are in $name ($coords). You see [pretty [@ here items]]."
    if {$name eq "PrizeRoom"} {
	puts "Congratulations -- You Won!!!"; exit
    }
    set exits [@ here exits]
    if {![llength $exits]} {set exits nowhere}
    puts "There are exits towards: [join $exits {, }]."
    inventory
}

proc drop what {
    if {$what eq "all"} {set what [@ my items]}
    foreach i $what {
	if {$i ni [@ my items]} {return "You don't have a $i."}
	lremove [@ my items &]   $i
	lappend [@ here items &] $i
    }
    inventory
}

proc go {where {describe 1}} {
    set where  [Normalize $where]
    foreach {x y z} [split [@ my coords] ,] break
    switch -- $where {
	east  {incr x} west  {incr x -1}
	north {incr y} south {incr y -1}
	up    {incr z} down  {incr z -1}
	default {return "usage: go (east|west|north|south|up|down)"}
    }
    set coords $x,$y,$z
    if {$where eq "up" && "ladder" ni [@ here items]} {
	return "You can't go up without a ladder."
    }
    if {$where ni [@ here exits]} {
	return "No exit towards $where, consider an attack..."
    }
    if {[catch {@ $coords name}]} {Room $coords}
    @ my coords $coords
    if {$describe} describe
}

proc inventory {} {return "You have [pretty [@ my items]]."}
proc name what {
    return "This room is now named [@ here name $what]."
}

proc take what {
    if {$what eq "all"} {set what [@ here items]}
    foreach i $what {
	if {$i ni [@ here items]} {return "There is no $i here."}
	lremove [@ here items &] $i
	lappend [@ my items &]   $i
    }
    inventory
}

#----------------------- general utilities
proc alias {new = args} {interp alias {} $new {} {*}$args}
proc I     x            {return $x} ;#-- Identity: simple but useful
proc lpick lst        {lindex $lst [expr {int(rand()*[llength $lst])}]}
proc lremove {_lst what} {
    upvar 1 $_lst lst
    set pos [lsearch -exact $lst $what]
    set lst [lreplace $lst $pos $pos]
}
proc pretty lst {
    if {![llength $lst]} {return nothing}
    foreach i $lst {lappend tmp [expr {$i eq "gold"? $i : "a $i"}]}
    regsub {(.+),} [join $tmp ", "] {\1, and}
}
main $argv
```

