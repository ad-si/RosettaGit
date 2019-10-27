+++
title = "Run-length encoding/Smalltalk"
description = ""
date = 2010-01-08T17:37:38Z
aliases = []
[extra]
id = 5326
[taxonomies]
categories = []
tags = []
+++

{{collection|Run-length encoding}}

{{works with|GNU Smalltalk}}

The class <code>RunLengthEnc</code> holds a representation of a run length encoded sequence of objects.


```smalltalk
Object subclass: RunLengthEnc [
  |counters contents|
  <category: 'Compression'>
  <comment: 'My instances are similar to a Bag, except
that items are ordered and counted iff they are adjacent. So that this
instance keeps a representation of the added items suitable for
performing a RunLengthEncoding, hence the name.'>

  RunLengthEnc class >> new [ ^self basicNew initialize ]

  size [ ^counters size ]

  add: anObject [ ^(self add: anObject withCount: 1) ]

  add: anObject withCount: anInt [
    anObject isNil
        ifTrue: [ 
            SystemExceptions.InvalidArgument signalOn: anObject
              reason: 'RunLengthEnc encodes existing objects, e.g. integers or characters, not nil' 
        ].
    (self size) > 0
    ifTrue: [
      (contents last) == anObject
          ifTrue: [
             self incrementLastCounter: anInt.
          ]
          ifFalse: [
	     self appendObject: anObject withCount: anInt
          ]
    ] ifFalse: [ self appendObject: anObject withCount: anInt ].
    ^anObject
  ]

  initialize [
    counters := OrderedCollection new.
    contents := OrderedCollection new.
  ]

  appendObject: anObject withCount: anInt [
    contents addLast: anObject.
    counters addLast: anInt
  ]

  appendObject: anObject [
    contents addLast: anObject.
    counters addLast: 1
  ]

  incrementLastCounter: howMuch [ | c |
    c := counters removeLast.
    counters addLast: (c+howMuch)
  ]

  "the 'do:' can be used to let the user store the compressed 'stream' as s/he
   prefers, while 'add:withCount:' can be used to rebuild the informations from
   the custom storage" 
  do: aBlock [
    1 to: (counters size) do: [ :i | | l |
      aBlock value: (contents at: i) value: (counters at: i)
    ]
  ]

  asOrderedCollection [ |oc|
    oc := OrderedCollection new.
    self do: [ :o :c |
      1 to: c do: [ :i| oc add: o ]
    ].
    ^oc
  ]

  printOn: aStream [
      "output a representation of the object:
       counter [object] ... for every object"
      1 to: (counters size) do: [ :i |
         (counters at: i) printOn: aStream.
	 aStream nextPut: (Character value: 32).
	 (contents at: i) printOn: aStream.
	 aStream nextPut: (Character nl).
      ]
  ]  

  asString [ |oc| 
    "'build' a string from the run length encoded objects;
     works only if objects are Characters or Strings"
    oc := self asOrderedCollection.
    ^(oc asString)
  ]
].
```


The following extension to the OrderedCollection class allows to run length encode an ordered collection (theoretically of any objects' kind, but the RunLengthEnc class is supposed to work with characters mainly).


```smalltalk
OrderedCollection extend [
   asRunLengthEnc [ |rc|
       rc := RunLengthEnc new.
       self do: [ :o |
          rc add: o
       ].
       ^rc
   ]
].
```


The following extention to the String class allows to run length encode a string (it is basically a shortcut for <code>aString asOrderedCollection asRunLengthEnc</code>).


```smalltalk
String extend [
   asRunLengthEnc [ ^self asOrderedCollection asRunLengthEnc ]
].
```



'''Usage examples'''


```smalltalk
|cs s os|

s := 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'.

"let us run length encode the string"
cs := s asRunLengthEnc.
cs print. "this shows an ASCII representation of the run length encoded objects collection;
           in this case, of the string"

"let us show that the class is able to return the string back; this really works
 iff the objects of the collection are strings or characters"
cs asString displayNl.
```


The class does not mandate a way of storing itself into a file that can be loaded later. The following sample code shows how it could be done quickly, but not efficiently from the point of view of a compressor.


```smalltalk
|f|
"let's write the object and its informations to a file..."
f := FileStream open: 'rledump' mode: FileStream write.
ObjectDumper dump: cs to: f.
f close.

"... and let's read it back"
|o|
f := FileStream open: 'rledump' mode: FileStream read.
o := ObjectDumper loadFrom: f.
o print. "show that the object holds the same informations of cs"
f close.
```

