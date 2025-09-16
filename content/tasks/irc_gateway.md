+++
title = "IRC gateway"
description = ""
date = 2012-01-24T14:02:24Z
aliases = []
[extra]
id = 9547
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "tcl",
]
+++

{{draft task}} Create an IRC Gateway capable of connecting an IRC server with another IRC server or [[Chat server]]


## Tcl

This code is called as a complete script, perhaps like this:

```sh
./ircgateway.tcl irc://hostA.org/fishing bait irc://hostB.com:6667/haxors botfly
```

```tcl
#!/bin/env tclsh8.5
package require picoirc

### Parse script arguments
# URL form: irc://foobar.org/secret
if {$argc != 4} {
    puts stderr "wrong # args: should be \"$argv0 ircA nickA ircB nickB\""
    exit 1
}
lassign $argv url1 nick1 url2 nick2

### How to do the forwarding from one side to the other
proc handle {from to -> state args} {
    upvar #0 conn($from) f conn($to) t chan($to) chan
    switch -exact -- $state {
	"chat" {
	    lassign $args target nick message type
	    if {![string match "*>>*<<*" $message]} {
		picoirc::post $t $chan ">>$nick said<< $message"
	    }
	}
	"traffic" {
	    lassign $args action channel nick newnick
	    switch -exact -- $action {
		"entered" - "left" {
		    picoirc::post $t $chan ">>$nick has $action<<"
		}
	    }
	}
	"close" {
	    exit
	}
    }
}

### Connect and run the event loop
set chan(1) [lindex [picoirc::splituri $url1] 2]
set chan(2) [lindex [picoirc::splituri $url1] 2]
interp alias {} handle1to2 {} handle 1 2
interp alias {} handle2to1 {} handle 2 1
set conn(1) [picoirc::connect handle1to2 $nick1 $url1]
set conn(2) [picoirc::connect handle2to1 $nick2 $url2]
vwait forever
```


