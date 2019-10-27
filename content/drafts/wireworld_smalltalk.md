+++
title = "Wireworld/Smalltalk"
description = ""
date = 2012-12-02T14:15:11Z
aliases = []
[extra]
id = 8936
[taxonomies]
categories = []
tags = []
+++

This is a flexible implementation of [[implementation of task::Wireworld]].  It was also my first Smalltalk program, which may explain why it is so large.

This code is part of [http://github.com/elginer/jwgif jwgif].  jwgif actually produces animated gifs of wireworld simulations.  Hence, here is an animated GIF of the example, presented in the task description for [[Wireworld]]:

[[Image:3cycle.gif]]

{{works with|gst|3.2.1}}


```smalltalk

Object subclass: #Conductor.

Conductor comment:
'I am a Wireworld conductor.  The next state is an electron Head if there are 1 or 2 electron Heads nearby.'.

Conductor extend [

   next: neighbours [
      "This method returns an Electron Head if 1 or 2 of the neighbours are in Electron head state."
      | heads |
      heads := 0.
      neighbours do: [ :other |
         (other what = #Head) ifTrue: [
            heads := heads + 1.
         ]
      ].
      ((heads = 1) or: [(heads = 2)])
      ifTrue: 
         [^ Head new]
      ifFalse:
         [^ self]
   ]   

]

Object subclass: #Head.

Head comment:
'I am an Electron Head.  The next state is an electron tail.'.

Head extend [

   next: neighbours [
      ^ Tail new
   ]

]

Object subclass: #Tail.

Tail comment:
'I am an Electron Tail.  The next state is a conductor.'.

Tail extend [

   next: neighbours [
      ^ Conductor new
   ]

]

Object subclass: #Empty.

Empty comment:
'I am the empty WireWorld cell. The next state is Empty.'.

Empty extend [

   next: neighbours[
      ^ Empty new
   ]

]

Object subclass: #Cell.

Cell comment:
'I am a cell in the Wireworld cellular automaton.  I use the State pattern (see Gang of Four), where I use CellState classes make me appear to change my own class.'.

Cell instanceVariableNames: 'neighbours state nextState'.

Cell class extend [

   new: state [
      "This method creates a new cell with a given initial state."
      | cell |
      cell := super new.
      cell change: state.
      cell newNeighbours.
      ^ cell
   ]

   parse: char [
      "This method creates a cell in a given state, corresponding to the input character - H for electron head, t for electron tail, . for conductor and space for empty."
      | choices |
      choices := Dictionary new.
      choices at: $. put: [Conductor new].
      choices at: $H put: [Head new].
      choices at: $t put: [Tail new].
      choices at: $  put: [Empty new].
      ^ Cell new: ((choices at: char) value)
   ]

]

Cell extend [

   newNeighbours [
      "This method creates a new neighbours set for this cell."
      neighbours := Set new
   ]

   addNeighbour: other [
      "This method adds a new neighbour to this cell."
      neighbours add: other
   ]

   findStep [
      "This method finds the next state for this cell."
      nextState := state next: neighbours
   ]

   takeStep [
      "This method puts the Cell into its next state."
      self change: nextState
   ]

   change: behaviour [
      "This method changes the state of the Cell."
      state := behaviour
   ]

   what [
      "This method describes the state of the cell, returning either #Conductor, #Head, #Tail or #Empty."
      ^ (state class) name
   ]

]

Object subclass: #WireTable.

WireTable comment:
'I am the current state of a Wireworld program - a tabular grid of Wireworld cells.'.

WireTable instanceVariableNames: 'table width height'.

WireTable class extend [

   new: filename [
      "This method reads the wireworld file to create the WireTable."
      | file lns table |
      file := FileStream open: filename mode: #read.
      lns := file lines contents.
      file close.
      table := super new.
      table lines: lns.
      ^ table
   ]
]

WireTable extend [

   lines: lns [
      "This method parses the lines into a table"
      self setBounds: lns.
      self createTable.
      self createCells: lns.
      self setNeighbours
   ]

   createCells: lns [
      "This method populates the table, according to the lines."
      lns doWithIndex: [:ln :rownum |
         | row |
         row := table at: rownum.
         ln doWithIndex: [:ch :colnum |
            row at: colnum put: (Cell parse: ch).
         ]
      ]
   ]

   setBounds: lns [
      "This method sets the bounds of the table."
      height := lns size.
      width := (lns collect: [:ln | ln size]) sort last.
   ]

   createTable [
      "This method creates a table populated by empty cells."
      table := (Array new: height) collect: [:ignore |
          | col |
          col := Array new: width.
          col collect: [:forget | Cell new: (Empty new)]
      ].

   ]

   setNeighbours [
      "This method sets the neighbours for all cells."
      self withCells: [:cell :y :x |
         | a b l r raw neighpts |
         "above"
         a := y - 1.
         "below"
         b := y + 1.
         "left"
         l := x - 1.
         "right"
         r := x + 1.
         raw := Array new: 8.
         raw at: 1 put: l@a.
         raw at: 2 put: x@a.
         raw at: 3 put: r@a.
         raw at: 4 put: l@y.
         raw at: 5 put: r@y.
         raw at: 6 put: l@b.
         raw at: 7 put: x@b.
         raw at: 8 put: r@b.
         neighpts := raw reject: [:p |
            (p y < 1) or:
            [(p y > height) or:
            [(p x < 1) or:
            [(p x > width)]]]
         ].
         neighpts do: [:pt |
            cell addNeighbour: ((table at: (pt y)) at: pt x)
         ]
      ]
   ]

   frame [
      "This method performs one execution step."
      self withCells: [:cell |
         cell findStep
      ].
      self withCells: [:cell |
         cell takeStep
      ]
   ]

   withCells: blk [
      "This method performs a computation for each cell.  The blk must take at least 1 argument, the cell, but may at most take 3; the row and column also."
      table doWithIndex: [:col :rownum |
         col doWithIndex: [:cell :collnum |
            blk cull: cell cull: rownum cull: collnum
         ] 
      ]
   ]

   drawTo: renderer frameNo: frame [
      renderer draw: (table collect: [:row | row collect: [:cell | cell what]]) frameNo: frame
   ]

]

Object subclass: #WireWorld.

WireWorld instanceVariableNames: 'table renderer'.

WireWorld comment:
'I am a flexible implementation of the Wireworld cellular automation.'.

WireWorld class extend [

   new: table drawWith: renderer [
      "This method creates a new wireworld from a table."
      | world |
      world := super new.
      ^ world state: table canvas: renderer.
   ]

   load: filename drawWith: renderer [
      "This method loads the Wireworld and returns it."
      | table |
      table := WireTable new: filename.
      ^ WireWorld new: table drawWith: renderer
   ]
]

WireWorld extend [

   state: prog canvas: outdev [
      "This method sets the state of the wireworld.  It is intended to be used to initialize a WireWorld object."
      table := prog.
      renderer := outdev
   ]

   run: n [
      "This method runs the wireworld simulation."
      1 to: n do: [:frameNo |
         table drawTo: renderer frameNo: frameNo.
         table frame
      ]
   ]
]

Object subclass: #TextRenderer.

TextRenderer comment: 
'I am a Wireworld renderer which produces output in ASCII.  I produce H for Head, t for Tail, . for Conductor and space for empty.'.

TextRenderer extend [

   draw: rows frameNo: frame [
      "This method takes a 2d array of wireworld cell descriptions and outputs a text representation of that table."
      'Frame #' display.
      frame displayNl.
      rows do: [:row |
         row do: [:cell |
            | choices |
            choices := Dictionary new.
            choices at: #Empty put: $ .
            choices at: #Conductor put: $..
            choices at: #Head put: $H.
            choices at: #Tail put: $t.
            (choices at: cell) display.
         ].
         Character nl display.
      ].
      Character nl display.
   ]
]

args := Smalltalk arguments.

(args size = 2) ifTrue: [
   filename := args at: 1.
   generations := (args at: 2) asNumber.

   world := WireWorld load: filename drawWith: (TextRenderer new).
   world run: generations.
] ifFalse: [
   'Usage: gst rosetta.st -a FILENAME GENERATIONS' displayNl.
]
```

