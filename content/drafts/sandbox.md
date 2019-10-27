+++
title = "Sandbox"
description = ""
date = 2014-10-26T23:07:24Z
aliases = []
[extra]
id = 1914
[taxonomies]
categories = []
tags = []
+++

__TOC__

== Subheadings Test ==

re [[Rosetta_Code:Village_Pump/Task_page_subsections]]

=== <span style="display:none">Foo:</span> Subheading 1 ===

=== <span style="display:none"><nowiki>Foo:</nowiki></span> Subheading 2 ===

=== <!-- Foo: --> Subheading 3 ===

=== <span class="mw-headline" id="Foo-Subheading_4"> Subheading 4 <span> ===

[[#Foo-Subheading_4]]

<h3 id="Foo-Subheading_5><span class="mw-headline" id="Foo-Subheading_5"> Subheading 5 </span></h3>

[[#Foo-Subheading_5]]

== Which mu is the correct mu? ==
* &micro; &#xB5; U+00B5 [[:Category:&#xB5;C++]]
* &mu; &#x03BC; U+03BC [[:Category:&#x03BC;C++]]
* &#x4D; U+004D [[:Category:&#x4D;C++]]
* &#x039C; U+039C [[:Category:&#x039C;C++]]

== Can you use a data URL? ==
[[#Text|Text]]
[data:text/html,Hello%20World No.]


== What if you transclude a category? ==
-- Okay, the member list doesn't show up. No good.


###  MultiCategorySearch 

{{Special:MultiCategorySearch/include=Maintenance}}

=== Semantic MediaWiki (SMW) ===
* http://semantic-mediawiki.org/wiki/Help:Inline_queries
* http://semantic-mediawiki.org/wiki/Help:Result_formats

{{#ask: [[Category:Maintenance]]
| intro=These pages need the maintenance!!! (This shows all the pages in [[:Category:Maintenance]] or its subcategories.)
| format=category
| limit=20
}}


### =Randomizer=

{{#ask: [[Category:Draft Programming Tasks]] OR [[Category:Programming Tasks]]
| intro='''Random [[:Category:Programming Tasks|task]] (or [[:Category:Draft Programming Tasks|draft task]]): '''
| format=list
| order=random
| limit=1
}}


### Other pages


These are other pages related to [[AWK]] OR [[Dc]] which require attention; with your knowledge and assistance, we can improve the quality of the site's content.

{{#ask: <q>[[implementation of::AWK]] OR [[implementation of::Dc]]</q> [[is stub::true]]
| format=category
| limit=500
| default=<h3>No pages found.</h3>
}}

==TITLE==
 code
----
== pre test ==
<div class="infobox" style="width: 2in">
<big>'''Programming Task'''</big><br />
This is a programming task. It lays out a problem which Rosetta Code users are encouraged to solve, using languages they know.

Code examples should be formatted along the lines of one of the existing [[Help:Example_Programming_Example|prototypes]].{{proggit}}
</div>
<pre style="height:30ex;overflow:scroll">
Test
Another line
Yet another line, this time one which clearly is much longer than the width of the browser window, at least assuming normal screen dimensions and font settings, and assuming the browser window is not wider than the screen (which would be impractical anyway).
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21

```

==Testing HOPL==
* {{HOPL|Ruby}}
* {{HOPL|Ruby|id=2458}}
* {{HOPL|id=2458}}

==Testing new language tags==

### Z80


```z80
; some random instructions
  ld a, 80h
  ld b, 9Ah
  add a, b
  adc a, a
  rst 0 ; reboot :-)
```



### Whitespace


```whitespace
This may  not 	be a meaningful 	  whitespace
  program,
	but 	  	it's
just a 	highlighting	test
anyway.
```



### AviSynth


```avisynth
AviSource("D:\clip.avi")
ConvertToYUY2()
PixieDust(5)
```


===POV-Ray===

```povray
#declare factorial = function(C) { prod(i, 1, C, i) }
#declare A = factorial(5);
sphere{0,A} // a sphere
```



### Text


```text
Is this actually a programming language, or really just plain text?
```


===What happens if a language doesn't exist?===

```somelanguagethatdoesnotexist
some code
more code;
* a b -> c /// &a1 ->>> 778
(Probably this is an obfuscated language :-))
```


==Whitespace test with unicode ZERO WIDTH NO-BREAK SPACE character (U+FEFF)==

```whitespace

 	  	
 		 	
   		 	
	

```

testing

== Impl needed ==
Tcl pages: {{PAGESINCAT:Tcl}}

Tcl/Omit pages: {{PAGESINCAT:Tcl/Omit}}

Programming tasks: {{PAGESINCAT:Programming Tasks}}

== Property query test ==
The programming languages are {{#ask: [[is language::true]]}}
: This uses [[Property:is language]]

----

This should turn up all languages and libraries which provide Graphics:
{{#ask: [[provides::Capability:Graphics]] | default=Or not.}}

This should turn up anything which does ''not'' provide Graphics:
{{#ask: [[provides::!Capability:Graphics]] | default=Unless it doesn't.}}
: as I learned now, it instead turns up anything which provides something other than Graphics.

This should turn up anything which provides first class functions:
{{#ask: [[provides::Capability:First class functions]] | default=I would have hoped.}}

And this should be the intersection of the previous two sets:
{{#ask: [[provides::!Capability:Graphics]] [[provides::Capability:First class functions]] | default=Which shouldn't be empty.}}

This should turn up anything that provides ''or'' allows Graphics:
{{#ask: [[provides::Capability:Graphics]] OR [[allows::Capability:Graphics]] | default=There should be something here.}}

This should turn up anything implemented in a language that provides graphics:
{{#ask: [[Implemented in language.provides::Capability:Graphics]] | default=Nothing!?}}

Does "implemented in language" work? Let's see everything implemented in Unlambda:
{{#ask: [[Implemented in language::Unlambda]] | default=Empty!}}

This should turn up anything implemented in a language that allows graphics:
{{#ask: [[Implemented in language.allows::Capability:Graphics]] | default=Nothing!?}}

This might turn up anything which has a "provides" property:
{{#ask: [[provides::Capability:+]] | default=Or it might not. :-)}}

What about this:
{{#ask: [[provides::+]] | default=No.}}

Is [[Classes]] implemented in C++?
{{#ask: [[Classes]] [[Implemented in language::C++]] | format=count}} (1 = yes, 0 = no).

Is [[Classes]] implemented in Unlambda?
{{#ask: [[Classes]] [[Implemented in language::Unlambda]] | format=count}} (1 = yes, 0 = no).

The following assumes a (currently non-existent) template "ignore" which takes an argument and ignores it. With that template, if the task [[Mutex]] is implemented in C++, nothing would be printed, otherwise a nested ask is executed, which just lists all requirements of Mutex (assuming I've got everything right):
{{#ask: [[Mutex]] [[Implemented in language::C++]] | format=template | template=ignore | default={{#ask: [[Mutex]] | ?requires}}}}

And now the same with Unlambda instead of C++ (there's of course no Unlambda implementation):
{{#ask: [[Mutex]] [[Implemented in language::Unlambda]] | format=template | template=ignore | default={{#ask: [[Mutex]] | ?requires}}}}

The same without the "Requires" text:
{{#ask: [[Mutex]] [[Implemented in language::Unlambda]] | format=template | template=ignore | default={{#ask: [[Mutex]] | ?requires =}}}}

----

==Memoization==
Memoization is a method used to reduce function calls in recursive functions or other functions that are called very frequently. The basic idea behind memoizing is to store results for new sets of inputs (typically in a key-value style) so that the function will not have to re-compute those results later.

Functions which can be memoized are ones that give the same answer for a set of inputs each time those inputs are used. [[Fibonacci sequence|Fibonacci number functions]] are often memoized to reduce their call trees and calculation times over time. The basic operation of a memoized function would look something like this:
 function a with inputs
    if inputs have been seen before
       return a stored answer from when they were seen
    else
       compute the answer for inputs
       store that (inputs, answer) pair
       return that answer
 end function
Some programs may negate the condition in the "if" and swap the operations. 

The overall benefit is that a function frequently called with the same set of inputs can save time by remembering the answer after computing it once -- sacrificing memory for computation time. In systems where memory (or storage depending on the implementation of storing old results) comes at a premium, memoization is not a good option. As long as memory is available and input sets are used repeatedly, memoization can save lots of computation time.

For tasks that use/can benefit from/(whatever) memoization, see [[:Category:Memoization]]

<nowiki>[[Encyclopedia]][[Category:Memoization]]</nowiki>

==Category:Memoization==
{{Alertbox||The main page for this category is [[Memoization]].}}
Memoization is a method used to reduce function calls in recursive functions or other functions that are called very frequently. The basic idea behind memoizing is to store results for new sets of inputs (typically in a key-value style) so that the function will not have to re-compute those results later.

For more information, see [[Memoization]].
