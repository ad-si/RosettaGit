+++
title = "Remote agent/Agent interface"
description = ""
date = 2019-07-09T10:57:09Z
aliases = []
[extra]
id = 6051
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "go",
  "julia",
  "picolisp",
  "tcl",
]
+++

{{draft task}}In [[Remote agent]], a component is described that marshals commands and events between a stream and a program that issues commands and processes the resulting events. Using the protocol definition described there, build this component in a fashion idiomatic and natural to your language.


## C

See [[Remote agent/Simulation/C]]


## Go


```go
package ifc

// Streamer interface type defines how agent and world talk to each other.
//
// Send and Rec may be implemented as synchronous blocking operations.
// They can be considered perfectly reliable as there is no acknowledgement
// or timeout mechanism.
type Streamer interface {
    Send(byte)
    Rec() byte
}

// The agent and world send and receive single bytes, out of the set of
// constants defined here.
const Handshake = 'A'

const (
    CmdForward       = '^'
    CmdRight         = '>'
    CmdLeft          = '<'
    CmdGet           = '@'
    CmdDrop          = '!'
    EvGameOver       = '+'
    EvStop           = '.'
    EvColorRed       = 'R'
    EvColorGreen     = 'G'
    EvColorYellow    = 'Y'
    EvColorBlue      = 'B'
    EvBallRed        = 'r'
    EvBallGreen      = 'g'
    EvBallYellow     = 'y'
    EvBallBlue       = 'b'
    EvBump           = '|'
    EvSectorFull     = 'S'
    EvAgentFull      = 'A'
    EvNoBallInSector = 's'
    EvNoBallInAgent  = 'a'
)
```

Above is the interface package defining the abstract interface.  Below is an example of a driver program that supplies a concrete implementation of the interface.  This implementation uses Go channels to run a simulation in a single process.  A different implementation might produce two executables that would run on different processes or different machines.  It would use the same interface however.

```go
package main

import (
    "log"
    "os"

    "ra/agent"
    "ra/world"
)

// Concrete type chanStreamer implements interface ifc.Streamer.
// Implementaion is with channels, a simple way to demonstrate concurrency.
type chanStreamer struct {
    name string
    in   <-chan byte
    out  chan<- byte
}

// Send satisfies ifc.Streamer.Send method.
func (c chanStreamer) Send(b byte) {
    log.Print(c.name, " sends ", text[b])
    c.out <- b
}

// Rec satisfies ifc.Streamer.Rec method.
func (c chanStreamer) Rec() byte {
    b := <-c.in
    log.Print(c.name, " recieves ", text[b])
    return b
}

func main() {
    // Logging is done with the log package default logger, so this is
    // technically something that might be shared by the agent and the world.
    // Currently though, the agent doesn't do anything with the logger, only
    // the world does.
    log.SetFlags(0)
    log.SetOutput(os.Stdout)
    // Create channels for chanStreamer.  Only two channels are needed,
    // cmd going from agent to world, and ev going from world to agent.
    cmd := make(chan byte)
    ev := make(chan byte)
    // Instantiate chanStreamer for agent and start agent as a goroutine.
    // This allows it to run concurrently with the world.
    go agent.Agent(chanStreamer{"agent", ev, cmd})
    // World doesn't need another goroutine, it just takes over main at this
    // point.  The program terminates immediately when World returns.
    world.World(chanStreamer{"world", cmd, ev})
}
```

A separate file, but still part of the driver, contains human readable text for commands and events.  Alternatively, this file could be included with the interface package for maintainability, but technically it is not a required part of the abstract interface used by the agent and world simulator.

```go
package main

import "ra/ifc"

// Human readable text for commands and events.
var text = map[byte]string{
    ifc.CmdForward:       "command forward",
    ifc.CmdRight:         "command turn right",
    ifc.CmdLeft:          "command turn left",
    ifc.CmdGet:           "command get",
    ifc.CmdDrop:          "command drop",
    ifc.EvGameOver:       "event game over",
    ifc.EvStop:           "event stop",
    ifc.EvColorRed:       "event color red",
    ifc.EvColorGreen:     "event color green",
    ifc.EvColorYellow:    "event color yellow",
    ifc.EvColorBlue:      "event color blue",
    ifc.EvBallRed:        "event ball red",
    ifc.EvBallGreen:      "event ball green",
    ifc.EvBallYellow:     "event ball yellow",
    ifc.EvBallBlue:       "event ball blue",
    ifc.EvBump:           "event bump",
    ifc.EvSectorFull:     "event sector full",
    ifc.EvAgentFull:      "event agent full",
    ifc.EvNoBallInSector: "event no ball in sector",
    ifc.EvNoBallInAgent:  "event no ball in agent",
}
```




## Julia

See [[Remote agent/Agent_interface/Julia]]


## PicoLisp

The interface logic for the PicoLisp solution is directly integrated into the client [[Remote agent/Agent logic#PicoLisp]].


## Tcl

```tcl
package require Tcl 8.6

oo::class create AgentAPI {
    variable sock events sectorColor ballColor
    constructor {host port} {
	set sock [socket $host $port]
	fconfigure $sock -buffering none -translation binary -encoding ascii \
	    -blocking 0
	# Hack to allow things to work in 8.6b1 and 8.6b2
        if {![llength [info commands yieldto]]} {
	    interp alias {} yieldto {} ::tcl::unsupported::yieldTo
	}
	coroutine ReaderCoroutine my ReadLoop
    }
    destructor {
	if {[llength [info command ReaderCoroutine]]} {
	    rename ReaderCoroutine {}
	}
	if {[llength [info command AgentCoroutine]]} {
	    rename AgentCoroutine {}
	}
	if {$sock ne ""} {
	    catch {close $sock}
	}
    }
    method Log message {
    }

    # Commands
    method ForwardStep {} {
	my Log "action: forward"
	puts -nonewline $sock "^"
	my ProcessEvents [yield]
    }
    method TurnRight {} {
	my Log "action: turn right"
	puts -nonewline $sock ">"
	my ProcessEvents [yield]
    }
    method TurnLeft {} {
	my Log "action: turn left"
	puts -nonewline $sock "<"
	my ProcessEvents [yield]
    }
    method GetBall {} {
	my Log "action: get ball"
	puts -nonewline $sock "@"
	my ProcessEvents [yield]
    }
    method DropBall {} {
	my Log "action: drop ball"
	puts -nonewline $sock "!"
	my ProcessEvents [yield]
    }
    method ProcessEvents {events} {
	set sectorColor {}
	set ballColor {}
	set err {}
	set done 0
	foreach e $events {
	    my Log "event: $e"
	    switch [lindex $e 0] {
		sector {set sectorColor [lindex $e 1]}
		ball {set ballColor [lindex $e 1]}
		error {set err [lindex $e 1]}
		gameOver {set done 1}
	    }
	}
	if {$err ne ""} {throw $err "can't do that: $err"}
	return $done
    }

    # Event demux
    method ReadLoop {} {
	# Init handshake
	fileevent $sock readable [info coroutine]
	while 1 {
	    yield
	    if {[read $sock 1] eq "A"} break
	}
	puts -nonewline $sock "A"
	# Main loop; agent logic is in coroutine
	try {
	    coroutine AgentCoroutine my Behavior
	    while 1 {
		yield
		set ch [read $sock 1]
		switch $ch {
		    "." {
			# Stop - end of events from move
			set e $events
			set events {}
			yieldto AgentCoroutine $e
			if {"gameOver" in $e} break
		    }
		    "+" {lappend events gameOver}
		    "R" {lappend events {sector red}}
		    "G" {lappend events {sector green}}
		    "Y" {lappend events {sector yellow}}
		    "B" {lappend events {sector blue}}
		    "r" {lappend events {ball red}}
		    "g" {lappend events {ball green}}
		    "y" {lappend events {ball yellow}}
		    "b" {lappend events {ball blue}}
		    "|" {lappend events {error bumpedWall}}
		    "S" {lappend events {error sectorFull}}
		    "A" {lappend events {error agentFull}}
		    "s" {lappend events {error sectorEmpty}}
		    "a" {lappend events {error agentEmpty}}
		}
	    }
	} finally {
	    close $sock
	    set sock ""
	}
    }

    method Behavior {} {
	error "method not implemented"
    }
}

# Export as package
package provide RC::RemoteAgent 1
```

