+++
title = "GNU Smalltalk"
description = ""
date = 2012-01-06T22:25:02Z
aliases = []
[extra]
id = 2586
[taxonomies]
categories = []
tags = []
+++

{{implementation|Smalltalk}}'''GNU Smalltalk''' is an implementation of the [[:Category:Smalltalk|Smalltalk]] programming language by the GNU Project. For download, see [http://smalltalk.gnu.org/ http://smalltalk.gnu.org/].

The implementation, unlike other Smalltalk environments, uses text files for program input and interprets the contents as Smalltalk code. In this way, GNU Smalltalk acts more like an interpreter rather than an environment in the traditional Smalltalk manner.

==Examples==
These examples only work on GNU Smalltalk 3.0 and later versions. Classic Hello world example:

 'Hello World!' displayNl

Some basic Smalltalk code:


```txt
"Everything, including a literal, is an object, so this works:"
-199 abs                                                "199"
'gst is cool' size                                      "11"
'Slick' indexOf: $c                                     "4"
'Nice Day Isn''t It?' asLowercase asSet asSortedCollection asString  "' '?acdeinsty'"
```




### Collections

Constructing and using an array:

 a := #(1 'hi' 3.14 1 2 #(4 5))
 
 a at: 3        "3.14"
 a reverse      "#((4 5) 2 1 3.14 'hi' 1)"
 a asSet        "Set(1 'hi' 3.14 2 #(4 5))"

Constructing and using a hash table:

 hash := Dictionary from: { 'water' -> 'wet'. 'fire' -> 'hot' }.
 hash at: 'fire'     "Prints:  hot"
 
 hash keysAndValuesDo: [ :k :v |
         ('%1 is %2' % { k. v }) displayNl ]
 
 "Prints:  water is wet
           fire is hot"
 
 hash removeKey: 'water'  "Deletes 'water' -> 'wet'"


### Blocks and iterators

Parameter-passing a block to be a closure:

 "remember a block."
 remember := [ :name | ("Hello, %1!" % { name }) displayNl ].
 
 "When the time is right -- call the closure!"
 remember value: 'world'
 "=> "Hello, world!""

Returning closures from a method:

 Integer extend [
     asClosure [
         | value |
          value := self.
        ^{ [ :x | value := x ]. [ value ] }
     ]
 ]
 
 blocks := 10 asClosure.
 setter := blocks first.
 getter := blocks second.
 getter value        "=> 10"
 setter value: 21    "=> 21"
 getter value        "=> 21"

Using block to send info back to the caller:

 Integer extend [
     ifEven: evenBlock ifOdd: oddBlock [
         ^self even
             ifTrue: [ evenBlock value: self ]
             ifFalse: [ oddBlock value: self ]
     ]
 ]
 
 "Invoke the above method, passing it a block."
 10 ifEven: [ :n | n / 2 ] ifOdd: [ :n | n * 3 + 1 ]    "=> 5"

Iterating over enumerations and arrays using blocks:

 array := #(1 'hi' 3.14)
 array do: [ :item | item displayNl ]
 "=> 1"
 "=> hi"
 "=> 3.14"
 
 (3 to: 6) do: [ :item | item displayNl ]
 "=> 3"
 "=> 4"
 "=> 5"
 "=> 6"

A method such as ''inject:into:'' can accept both a parameter and a block. It iterates over each member of a list, performing some function on while retaining an aggregate. This is analogous to the foldl function in [[Haskell]]. For example:

 #(1 3 5) inject: 10 into: [ :sum :element | sum + element ] "=> 19"

On the first pass, the block receives 10 (the argument to inject) as sum, and 1 (the first element of the array) as element, This returns 11. 11 then becomes sum on the next pass, which is added to 3 to get 14. 14 is then added to 5, to finally return 19.

Blocks work with many built-in methods:

 (File name: 'file.txt') withWriteStreamDo: [ :file |
         file nextPutAll: 'Wrote some text.'; nl ]
 "File is automatically closed here"
 
 (File name: 'file.txt') linesDo: [ :each |
         each displayNl ]
 
 "=> Wrote some text."

Using an enumeration and a block to square the numbers 1 to 10:

 (1 to: 10) collect: [ :x | x squared ] "=> [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]"


### Classes


The following code defines a class named Person. By deriving from Magnitude, it automatically defines all comparison methods except one (<tt>&lt;</tt>). With the addition of that one, <tt>asSortedCollection</tt> can sort by age. Note that we can override the way the object is printed/displayed (the default is to share the programmer-print and user-display representation) by overriding <tt>printOn:</tt>.

 Magnitude subclass: Person [
     | name age |
     Person class >> name: name age: age [
         ^self new name: name; age: age; yourself
    ]
 
     < aPerson [ ^self age < aPerson age ]
     name [ ^name ]
     name: value [ name := value ]
     age [ ^age ]
     age: value [ age := value ]
     printOn: aStream [ aStream nextPutAll: ('%1 (%2)' % { name. age }) ]
 ]
 
 group := {
         Person name: 'Dan' age: 23.
         Person name: 'Mark' age: 63.
         Person name: 'Cod' age: 16.
 }.
 
 group asSortedCollection reverse

The above prints three names in reverse age order:

 OrderedCollection (Mark (63) Dan (23) Cod (16) )


### Exceptions

An exception is raised with a <tt>halt</tt> call:

 self halt

An optional message can be added to the exception; there's also <tt>error:</tt> which raises a different kind of exception:

 self halt: 'This is a message'
 self error: 'This is a message'

These are actually wrappers for the actual exception raising method, <tt>signal</tt>:

 Error signal
 Error signal: 'Illegal arguments!'

Exceptions are handled by <code>on:do:</code> blocks.

 [ something to do ]
     on: Exception
     do: [ :ex | handle exception in ex ]

Of course you can catch only particular exceptions (and their subclasses):

 [ something to do ]
     on: Warning
     do: [ :ex | handle exception in ex ]

It is possible to use the exception object, which is made available to the handler clause, to exit or resume the first block:

 [ Error signal: 'foo' ]
     on: Error
     do: [ :ex | ex return: 5 ]
 
 (Warning signal: 'now what?') printNl                       "=> nil"
 [ (Warning signal: 'now what?')
         printNl ] on: Warning do: [ :ex | ex resume: 5 ]    "=> 5"

== External links ==

* [http://www.gnu.org GNU]
* [http://smalltalk.gnu.org/ GNU Smalltalk]
