+++
title = "Remote agent/Simulation"
description = ""
date = 2019-07-09T10:48:40Z
aliases = []
[extra]
id = 6050
[taxonomies]
categories = []
tags = []
+++

{{draft task}}As described in [[Remote agent]], generate a map, accept and respond to commands from an agent using an unbuffered stream.


## C

See [[Remote agent/Simulation/C]]


## Go


```go
package world

import (
    "bytes"
    "fmt"
    "log"

    "ra/ifc"
)

// Maze sectors are 3x3 bytes, with these quirks:
// String starts with a newline.
// Space at end of each line except the last.
// No space, newline, or blank lines following last W.
//
// In each sector, W or sector Color is mandantory.  If sector has ball,
// it is in the byte to the right.  If agent is in a sector, it is in
// the byte below and shown by its direction symbol.  If agent has a ball,
// it is in the byte to the right.
//
// The variable maze is not just input, but is the primary representation
// of the world.  Over the course of execution, walls and sector colors
// are constant; agent and balls can move.

/* Minimal layout looks like this:
var maze = []byte(`
W  W  W  W  W  W 


W  Rb W  Rg B  W 


W  G  G  B  G  W 


W  Br G  W  R  W 
            ^    

W  W  W  W  W  W`)
*/

// Following is equivalent, but with walls a little easier to see:
var maze = []byte(`
WWWWWWWWWWWWWWWW 
W     W        W 
W     W        W 
W  Rb W  Rg B  W 
W              W 
W              W 
W  G  G  B  G  W 
W              W 
W              W 
W  Br G  W  R  W 
W        W  ^  W 
W        W     W 
WWWWWWWWWWWWWWWW`)

// Maze and the following variables are defined at package level
// just to simplify function calls.
var (
    stream   ifc.Streamer
    rowLen   int
    agentPos int
)

// Directions are not part of the interface, but just something the
// world uses to keep track of the orientation of the agent.
// A little quirk of bytes.IndexAny is that it takes a string.
// Otherwise, the []byte version is more useful for bytes.IndexByte.
const dirString = "^>v<"

var directions = []byte(dirString)

func rightOf(dir byte) byte {
    return directions[(bytes.IndexByte(directions, dir)+1)%4]
}

func leftOf(dir byte) byte {
    return directions[(bytes.IndexByte(directions, dir)+3)%4]
}

func World(s ifc.Streamer) {
    stream = s
    rowLen = bytes.Index(maze[1:], []byte{'\n'}) + 1
    // A couple of validations for things I thought might be easy to
    // mess up when editing the maze or defining a new one.
    // Additional maze validation could be added.
    cols := rowLen / 3
    rows := ((len(maze)+1)/rowLen + 2) / 3
    if len(maze) != (rows*3-2)*cols*3-1 {
        log.Fatal("mis-shaped maze")
    }
    agentPos = bytes.IndexAny(maze, dirString)
    if agentPos < 0 {
        log.Fatal("agent not in maze")
    }
    // initialize quantized time as specified
    time := 0
    logTime(time)
    // handshake as specified
    stream.Send(ifc.Handshake)
    hs := stream.Rec()
    if hs != ifc.Handshake {
        log.Fatal("world: thats no handshake.")
    }
    // log initial configuration of maze
    log.Print(string(maze))
    // top level world simulation loop
    gameOver := false
    timeConsumed := 0
    for !gameOver {
        gameOver, timeConsumed = process(stream.Rec())
        time += timeConsumed
        logTime(time)
        log.Print(string(maze))
    }
}

// logTime sets the log prefix to the current quantized time value.
// It does not actually log anything.
func logTime(t int) {
    log.SetPrefix(fmt.Sprintf("%06d: ", t))
}

// Process a single command.
func process(cmd byte) (gameOver bool, timeConsumed int) {
    // timeConsumed is 1, unless the forward function says otherwise.
    timeConsumed = 1
    switch cmd {
    case ifc.CmdForward:
        timeConsumed = forward()
    case ifc.CmdRight:
        right()
    case ifc.CmdLeft:
        left()
    case ifc.CmdGet:
        get()
    case ifc.CmdDrop:
        // game over only detected by drop command
        gameOver = drop()
    }
    // for all commands, send stop event after all other processing is complete
    stream.Send(ifc.EvStop)
    return
}   
    
func forward() (timeConsumed int) {
    sectorOrigin := agentPos - rowLen
    switch maze[agentPos] {
    case '^':
        sectorOrigin -= 3 * rowLen
    case 'v':
        sectorOrigin += 3 * rowLen
    case '<':
        sectorOrigin -= 3
    case '>':
        sectorOrigin += 3
    }
    if maze[sectorOrigin] == 'W' {
        stream.Send(ifc.EvBump)
        // bump event consumes no time
        return 0
    }
    // move agent, plus any ball it has.
    newPos := sectorOrigin + rowLen
    maze[newPos] = maze[agentPos]
    maze[newPos+1] = maze[agentPos+1]
    maze[agentPos] = ' '
    maze[agentPos+1] = ' '
    agentPos = newPos
    // send color event for new sector
    stream.Send(maze[sectorOrigin])
    if ball := maze[sectorOrigin+1]; ball != ' ' {
        // send ball event
        stream.Send(maze[sectorOrigin+1])
    }
    // for all events except bump, time consumed is 1.
    return 1
}

func right() {
    maze[agentPos] = rightOf(maze[agentPos])
}

func left() {
    maze[agentPos] = leftOf(maze[agentPos])
}

func get() {
    agentBall := agentPos + 1
    sectorBall := agentBall - rowLen
    can := true
    if maze[sectorBall] == ' ' {
        stream.Send(ifc.EvNoBallInSector)
        can = false
    }
    if maze[agentBall] != ' ' {
        stream.Send(ifc.EvAgentFull)
        can = false
    }
    if can {
        maze[agentBall] = maze[sectorBall]
        maze[sectorBall] = ' '
    }
}

func drop() (gameOver bool) {
    agentBall := agentPos + 1
    sectorBall := agentBall - rowLen
    can := true
    if maze[agentBall] == ' ' {
        stream.Send(ifc.EvNoBallInAgent)
        can = false
    }
    if maze[sectorBall] != ' ' {
        stream.Send(ifc.EvSectorFull)
        can = false
    }
    if can {
        maze[sectorBall] = maze[agentBall]
        maze[agentBall] = ' '
    }
    if win() { 
        stream.Send(ifc.EvGameOver)
        return true
    }
    return false
}
    
// Win tests for a win state indicating game over.  A more efficient technique
// might be to track the number of balls out of place and recognize immediately
// when the last ball was dropped on a matching sector, but this technique
// is simple and robust.
func win() bool {
    ballPos := 2
    for ballPos < len(maze) {
        switch ballColor := maze[ballPos]; ballColor {
        case ifc.EvBallRed, ifc.EvBallGreen, ifc.EvBallYellow, ifc.EvBallBlue:
            if ballColor != maze[ballPos-1]+32 {
                return false
            }
        }
        ballPos += 3
        if ballPos%rowLen == 1 {
            ballPos += 2 * rowLen
        }
    }
    return true
}
```

{{out}}

```txt

...
000277: world recieves command forward
000277: world sends event color red
000277: world sends event stop
000277: agent recieves event color red
000277: agent recieves event stop
000278: agent sends command drop
000278: 
WWWWWWWWWWWWWWWW 
W     W        W 
W     W        W 
W  R  W  R  B  W 
W  ^r          W 
W              W 
W  G  G  Bb Gg W 
W              W 
W              W 
W  B  G  W  R  W 
W        W     W 
W        W     W 
WWWWWWWWWWWWWWWW
000278: world recieves command drop
000278: world sends event game over
000278: world sends event stop
000278: agent recieves event game over
000278: agent recieves event stop
000279: 
WWWWWWWWWWWWWWWW 
W     W        W 
W     W        W 
W  Rr W  R  B  W 
W  ^           W 
W              W 
W  G  G  Bb Gg W 
W              W 
W              W 
W  B  G  W  R  W 
W        W     W 
W        W     W 
WWWWWWWWWWWWWWWW

```



## Julia

See [[Remote agent/Simulation/Julia]]


## PicoLisp

This is the server. For the client, see [[Remote agent/Agent logic#PicoLisp]].
After starting (gameServer), you might for testing purposes also connect with 'telnet', type the commands, and see the responses.

```PicoLisp
# Global variables:
#  '*Port' is the port where the server is listening
#  '*Sock' is the TCP socket after a client connected
#  '*World' holds the current world
#  '*Agent' is the field where the agent is in
#  '*Ball' is the ball the agent is holding
#  '*Dir' is a circular list of directions (north east south west .)

(load "@lib/simul.l")

# The server port
(setq *Port (port 54545))

# Return a random Field
(de randomField ()
   (get *World (rand 1 DX) (rand 1 DY)) )

# Create a world of size 'DX' * 'DY' with 'Balls' and 'Walls'
(de makeWorld (DX DY Balls Walls)
   (when (>= Balls (* DX DY))
      (quit "Too many balls") )
   (when (>= Walls (* (dec DX) (dec DY)))
      (quit "Too many walls") )
   (for Column (setq *World (grid DX DY))          # Initialize fields
      (for This Column
         (let Color (get '(R G Y B) (rand 1 4))
            (=: field Color)                       # Set field color
            (when (ge0 (dec 'Balls))
               (until
                  (with (randomField DX DY)        # Find a field without ball
                     (unless (: ball)              # and set a ball
                        (=: ball Color) ) ) ) ) ) ) )
   (do Walls                              # Create walls
      (until
         (let
            (Field (randomField DX DY)    # Try random field
               F (if (rand T) car cdr)    # and random side
               G (if (rand T) '(car set . con) '(cdr con . set))
               Old ((car G) (F (val Field))) )
            (when Old
               ((cadr G) (F (val Field)) NIL)  # Remove connections to neighbor
               ((cddr G) (F (val Old)) NIL)
               (or
                  (reachable? Field (* DX DY))  # Field still reachable?
                  (nil                          # No: Restore connections
                     ((cadr G) (F (val Field)) Old)
                     ((cddr G) (F (val Old)) Field) ) ) ) ) ) ) )

# Test whether a field is reachable
(de reachable? (Field Fields)
   (let Visited NIL
      (recur (Field)
         (when (and Field (not (memq Field Visited)))
            (push 'Visited Field)
            (recurse (west Field))
            (recurse (east Field))
            (recurse (south Field))
            (recurse (north Field)) ) )
      (= Fields (length Visited)) ) )

# Test for ending condition
(de ending? ()
   (nor
      *Ball
      (find
         '((Column)
            (find
               '((This)
                  (and (: ball) (n== (: field) (: ball))) )
               Column ) )
         *World ) ) )

# Initialize for a new game
(de newGame (DX DY Balls Walls)
   (makeWorld DX DY Balls Walls)
   (setq
      *Agent (randomField DX DY)
      *Dir (do (rand 1 4) (rot '(north east south west .))) ) )

# Start the game server
(de gameServer (DX DY Balls Walls)
   (loop
      (setq *Sock (listen *Port))
      (NIL (fork) (close *Port))
      (close *Sock) )
   (seed *Pid)  # Ensure private random sequence
   (in *Sock
      (out *Sock (prin "A"))  # Greeting
      (when (= "A" (char (rd 1)))
         (newGame DX DY Balls Walls)
         (and *Dbg (showWorld))
         (while (rd 1)
            (out *Sock
               (case (char @)  # Command character
                  ("\^"  # Forward
                     (ifn ((car *Dir) *Agent)  # Hit wall?
                        (prin "|")             # Yes: Bump event
                        (with (setq *Agent @)  # Else go to new position
                           (prin (: field))
                           (and (: ball) (prin (lowc @))) ) ) )
                  (">"  # Turn right
                     (pop '*Dir) )
                  ("<"  # Turn left
                     (do 3 (pop '*Dir)) )
                  ("@"  # Get ball
                     (with *Agent
                        (cond
                           ((not (: ball)) (prin "s"))  # No ball in sector
                           (*Ball (prin "A"))           # Agent full
                           (T
                              (setq *Ball (: ball))
                              (=: ball) ) ) ) )
                  ("!"  # Drop ball
                     (with *Agent
                        (cond
                           ((not *Ball) (prin "a"))  # No ball in agent
                           ((: ball) (prin "S"))     # Sector full
                           (T (=: ball *Ball)
                              (off *Ball)
                              (and (ending?) (prin "+")) ) ) ) ) )  # Game over
               (prin ".") ) ) ) )  # Stop event
   (bye) )

# Visualize (debug)
(de showWorld ()
   (disp *World 0
      '((This)
         (pack
            (if (== *Agent This) "*" " ")
            (: field)
            (if (: ball) (lowc @) " ") ) ) ) )
```


For local tests, you can start also it interactively:

```txt
: (newGame 8 8 20 40) (showWorld)
   +---+---+---+---+---+---+---+---+
 8 | R   Y | B | R   R   Br| Rb  Br|
   +   +   +   +   +   +---+---+   +
 7 | Yy  G   G   Gb| Y   Gg  Rr| Y |
   +---+   +   +   +---+   +---+   +
 6 | R   Y   B   Rr *G   Y | Y   Br|
   +---+---+   +   +---+---+   +---+
 5 | B   Ry  G   R | Yy  Yy  Y | B |
   +   +---+---+   +---+   +---+   +
 4 | R | R   R   Gg  B   G   B   Y |
   +   +---+---+   +---+---+   +   +
 3 | R   Rr| Y   B   G | Yr  B | R |
   +   +   +---+---+---+   +   +---+
 2 | Y | B | B   Bb  Gr  B   B   Yy|
   +   +   +   +   +---+   +---+   +
 1 | Rr| R   G   Gr  R   G   R | G |
   +---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h
```

This displays the field colors in upper case letters, the balls in lower case letters, and the position of the agent with an asterisk.


## Tcl

{{libheader|TclOO}}

```tcl
package require TclOO

# Utility: pick random item of list
proc pick list {
    lindex $list [expr {int([llength $list] * rand())}]
}
# Utility: generate callback of method
proc callback {method args} {
    list [uplevel 1 {namespace current}]::my $method {*}$args
}
# Utility: print errors in events to standard error
proc bgerror args {puts stderr $args}

# The main class that implements the server
oo::class create BallMaze {
    variable grid balls x y dir carry turns identity chan timeout

    # Install this class as a server
    self method server {port width height args} {
	set srv [socket -server [callback new $width $height] {*}$args $port]
	if {$::debug} {
	    lassign [fconfigure $srv -sockname] addr host port
	    puts "server ready on ${addr}:${port}"
	}
    }

    # Initialize the per-player structure
    constructor {width height channel clientHost clientPort} {
	set identity "${clientHost}:${clientPort}"
	if {$::debug} {puts "$identity initializing..."}
	global width height
	set chan $channel
	fconfigure $chan -blocking 0 -encoding ascii
	set dir n
	set carry ""
	set turns 0

	# Build the grid
	set grid [set balls [lrepeat $width [lrepeat $height ""]]]

	# Make a layout of random colors
	for {set x 1} {$x < $width-1} {incr x} {
	    for {set y 1} {$y < $height-1} {incr y} {
		lset grid $x $y [pick {R G B Y}]
	    }
	}

	# Sprinkle some walls in
	for {set i 0} {$i < $width*$height/3} {incr i} {
	    while 1 {
		set x [expr {int(1+($width-2)*rand())}]
		set y [expr {int(1+($height-2)*rand())}]
		if {[lindex $grid $x $y] eq ""} continue
		if {[my WillCloseCell [expr {$x+1}] $y]} continue
		if {[my WillCloseCell [expr {$x-1}] $y]} continue
		if {[my WillCloseCell $x [expr {$y+1}]]} continue
		if {[my WillCloseCell $x [expr {$y-1}]]} continue
		break
	    }
	    lset grid $x $y ""
	}

	# Sprinkle some balls in
	for {set i 0} {$i < $width*$height/5} {incr i} {
	    while 1 {
		set x [expr {int(1+($width-2)*rand())}]
		set y [expr {int(1+($height-2)*rand())}]
		if {[lindex $grid $x $y] ne ""} break
	    }
	    lset balls $x $y [pick {R G B Y}]
	}

	# Select a starting location
	while 1 {
	    set x [expr {int(1+($width-2)*rand())}]
	    set y [expr {int(1+($height-2)*rand())}]
	    if {[lindex $grid $x $y] ne ""} break
	}
	set dir [pick {n s e w}]

	# OK, we're ready; wait for the client to be ready
	puts -nonewline $chan "A"
	fileevent $chan readable [callback PostInit]
	my SetTimeout
    }

    # Close things down (particularly the channel and the timeout; other state
    # is automatically killed with the object)
    destructor {
	if {$::debug} {puts "$identity closing down..."}
	catch {close $chan}
	catch {after cancel $timeout}
    }

    # How to (re)set the timeout
    method SetTimeout {} {
	catch {after cancel $timeout}
	set timeout [after 60000 [callback destroy]]
    }

    # Callback used to wait for the client to acknowledge readiness
    method PostInit {} {
	if {[read $chan 1] ne "A"} {
	    my destroy
	} else {
	    if {$::debug} {my print}
	    fileevent $chan readable [callback DispatchAction]
	    my SetTimeout
	}
    }

    # Utility: test if a cell will be closed by putting a wall next to it
    method WillCloseCell {i j} {
	set num 0
	incr num [expr {[lindex $grid [expr {$i+1}] $j] ne ""}]
	incr num [expr {[lindex $grid [expr {$i-1}] $j] ne ""}]
	incr num [expr {[lindex $grid $i [expr {$j+1}]] ne ""}]
	incr num [expr {[lindex $grid $i [expr {$j-1}]] ne ""}]
	return [expr {$num == 1}]
    }
    # Utility: is the game finished; all balls match, none in hand
    method IsGameOver {} {
	foreach gc $grid bc $balls {
	    foreach g $gc b $bc {
		if {$b ne "" && $b ne $g} {
		    return 0
		}
	    }
	}
	return [expr {$carry eq ""}]
    }

    # Main event handler; reads user action, dispatches, manages timeouts
    method DispatchAction {} {
	switch [read $chan 1] {
	    "^" {set events [my forward]}
	    "<" {set events [my left]}
	    ">" {set events [my right]}
	    "@" {set events [my get]}
	    "!" {set events [my drop]}
	    default {
		# EOF will come here too (read returns empty string)
		my destroy
		return
	    }
	}
	# Add the "stop" and send message to client
	append events "."
	puts -nonewline $chan $events
	my SetTimeout
    }

    # Implementations of particular actions; doesn't include communication
    method forward {} {
	switch $dir {
	    n {set dx 0; set dy -1}
	    s {set dx 0; set dy 1}
	    e {set dx 1; set dy 0}
	    w {set dx -1; set dy 0}
	}
	if {[lindex $grid [expr {$x+$dx}] [expr {$y+$dy}]] eq ""} {
	    set response "|"
	} else {
	    set response ""
	    incr turns
	    incr x $dx
	    incr y $dy
	}
	append response [lindex $grid $x $y]
	append response [string tolower [lindex $balls $x $y]]
	return $response
    }
    method left {} {
	set dir [string map {n w w s s e e n} $dir]
	incr turns
	return
    }
    method right {} {
	set dir [string map {n e e s s w w n} $dir]
	incr turns
	return
    }
    method get {} {
	incr turns
	set response ""
	if {[lindex $balls $x $y] eq ""} {append response "s"}
	if {$carry ne ""} {append response "A"}
	if {$response eq ""} {
	    set carry [lindex $balls $x $y]
	    lset balls $x $y ""
	}
	return $reponse
    }
    method drop {} {
	incr turns
	set response ""
	if {$carry eq ""} {append response "a"}
	if {[lindex $balls $x $y] ne ""} {append response "S"}
	if {$response eq ""} {
	    lset balls $x $y $carry
	    set carry ""
	    if {[my IsGameOver]} {
		if {$::debug} {my print}
		append response "+"
	    }
	}
	return $response
    }

    # Utility: prints the state of the service instance
    method print {} {
	set width [llength $grid]
	set height [llength [lindex $grid 0]]
	puts "$identity : [expr {[my IsGameOver] ? {finished} : {running}}]"
	for {set i 0} {$i < $height} {incr i} {
	    for {set j 0} {$j < $width} {incr j} {
		puts -nonewline [format "%1s%1s%1s" \
			[expr {$j==$x&&$i==$y ? "*" : ""}] \
			[lindex $grid $j $i] \
			[string tolower [lindex $balls $j $i]]]
	    }
	    puts ""
	}
    }
}

# Parse command line arguments and test if we're in debug mode
lassign $argv port width height
set debug [info exists env(DEBUG_AGENT_WORLD)]

# Make the server and run the event loop
BallMaze server $port $width $height {*}$argv
vwait forever
```

Example call (with the server restricted to only serving on 127.0.0.1):

```txt
tclsh8.5 agent.world.tcl 54545 8 8 -myaddr localhost
```

