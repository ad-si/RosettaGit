+++
title = "Closest-pair problem/Smalltalk"
description = ""
date = 2010-02-06T12:43:13Z
aliases = []
[extra]
id = 5323
[taxonomies]
categories = []
tags = []
+++

{{collection|Closest pair problem}}

{{works with|GNU Smalltalk}}

These class methods return a three elements array, where the first two items are the points, while the third is the distance between them (which having the two points, can be computed; but it was easier to keep it already computed in the third position of the array).


```smalltalk
Object subclass: ClosestPair [
  ClosestPair class >> raiseInvalid: arg [
      SystemExceptions.InvalidArgument
        signalOn: arg
        reason: 'specify at least two points'
  ]

  ClosestPair class >> bruteForce: pointsList [ |dist from to points|
    (pointsList size < 2) ifTrue: [ ^ FloatD infinity ].
    points := pointsList asOrderedCollection.
    from := points at: 1. to := points at: 2.
    dist := from dist: to.
    [ points isEmpty ]
    whileFalse: [ |p0|
      p0 := points removeFirst.
      points do: [ :p |
        ((p0 dist: p) <= dist)
        ifTrue: [ from := p0. to := p. dist := p0 dist: p. ]
      ]
    ].
    ^ { from. to. from dist: to }
  ]

  ClosestPair class >> orderByX: points [
    ^ points asSortedCollection: [:a :b| (a x) < (b x) ]
  ]
  ClosestPair class >> orderByY: points [
    ^ points asSortedCollection: [:a :b| (a y) < (b y) ]
  ]

  ClosestPair class >> splitLeft: pointsList [
    ^ pointsList copyFrom: 1 to: ((pointsList size / 2) ceiling)
  ]
  ClosestPair class >> splitRight: pointsList [ |s|
    ^ pointsList copyFrom: (((pointsList size / 2) ceiling) + 1) to: (pointsList size).
  ]

  ClosestPair class >> minBetween: a and: b [
    (a at: 3) < (b at: 3)
      ifTrue: [ ^a ]
      ifFalse: [ ^b ]
  ]

  ClosestPair class >> recursiveDAndC: orderedByX and: orderedByY [
    |pR pL minL minR minDist middleVLine joiningStrip tDist nP yL yR|
    (orderedByX size <= 3)
      ifTrue: [ ^ self bruteForce: orderedByX ].

    pR := self splitRight: orderedByX.
    pL := self splitLeft: orderedByX.

    middleVLine := (pL last) x.

    yR := OrderedCollection new.
    yL := OrderedCollection new.

    orderedByY do: [ :e |
      (e x) <= middleVLine
      ifTrue: [ yL add: e ]
      ifFalse: [ yR add: e ]
    ].

    minR := self recursiveDAndC: pR and: yR.
    minL := self recursiveDAndC: pL and: yL.

    minDist := self minBetween: minR and: minL.

    joiningStrip := orderedByY
                      select: [ :p |
                                ((middleVLine - (p x)) abs) < (minDist at: 3) 
                              ].
                                 
    tDist := minDist.
    nP := joiningStrip size.

      1 to: (nP - 1) do: [ :i | |k|
        k := i + 1.
        [ (k <= nP) 
          & ( (((joiningStrip at: (k min: nP)) y) - ((joiningStrip at: i) y)) < (minDist at: 3) ) ]
        whileTrue: [ |d|
          d := (joiningStrip at: i) dist: (joiningStrip at: k).
          d < (tDist at: 3)
          ifTrue: [ tDist := { joiningStrip at: i. joiningStrip at: k. d } ].
          k := k + 1.
        ]
      ].

    ^ tDist
  ]

  ClosestPair class >> divideAndConquer: pointsList [
    ^ self recursiveDAndC: (self orderByX: pointsList)
           and: (self orderByY: pointsList)
  ]

].
```


'''Testing'''


```smalltalk
|coll cp ran|

ran := Random seed: 1.

coll := (1 to: 10000 collect: [ :a |
          Point x: ((ran next)*20.0 - 10.0) y: ((ran next)*20.0 - 10.0) ]).

cp := ClosestPair bruteForce: coll.
((cp at: 3) asScaledDecimal: 7) displayNl.

"or"

cp := ClosestPair divideAndConquer: coll.
((cp at: 3) asScaledDecimal: 7) displayNl.
```


The brute-force approach with 10000 points, run with the <tt>time</tt> tool, gave


```txt
224.21user 1.31system 3:46.84elapsed 99%CPU
```


while the recursive divide&amp;conquer algorithm gave


```txt
2.37user 0.01system 0:02.56elapsed 93%CPU
```


(Of course these results must be considered relative and taken ''cum grano salis''; <tt>time</tt> counts also the time taken to initialize the Smalltalk environment, and I've taken no special measures to avoid the system load falsifying the results)
