+++
title = "Talk:Pattern matching"
description = ""
date = 2014-09-09T16:20:29Z
aliases = []
[extra]
id = 2196
[taxonomies]
categories = []
tags = []
+++

I would like to see this task split into two tasks: 
* a simple example of pattern matching, and 
* an implementation of a red-black tree.  
There are a couple reasons I think this would be better:
* When I came to this page I was just looking for simple examples of pattern matching.  It takes a lot of effort to understand the red-black trees.
* I want to add a couple pattern matching examples, but I don't want to work through an entire red-black tree implementation.
* I would also like to see an example of a red-black tree written without pattern matching, for comparisons sake.  If pattern matching really does make the code more concise, the current examples would shine with a few huge implementations sitting near by.--[[User:Spaceg|Spaceg]] 01:19, 8 August 2008 (UTC)

I like the task, but I don't think it's a good application of the general case of pattern matching.  [[Pattern Matching]] should probably be geared towards globs and regular expressions, while this page should get moved to something like [[Red-Black Tree]]. Thoughts? --[[User:Short Circuit|Short Circuit]] 22:19, 6 November 2007 (MST)
:Actually, I think this is an ideal example of a key functional programming loanguage capability. Most functional programming pattern matching examples use very trivial patterns which could just as easily be implemented with an imperative switch statement. This example shows how pattern matching can simplify rather complex decision making. Regex stuff is rather primitive by comparison. There are only a handful of programming languages that are powerful enough to implement this example, so I don't expect this page to grow very large. --[[User:IanOsgood|IanOsgood]] 08:01, 7 November 2007 (MST)
:There are two mostly distinct basic concepts: pattern matching/destructuring of data structures (which this task is about) and regular-expression-style text matching. There can also be systems which combine features of both; for example, [[:Category:E|E]] allows regular expressions within data-structure patterns, and uses regular-expression-style quantification within term-tree patterns. --[[User:Kevin Reid|Kevin Reid]] 11:19, 7 November 2007 (MST)
How about some test data?

There's also unification - when a variable name appears multiple times in a pattern, it must stand in for the (fsvo) same data in all cases. But pattern-matching is most commonly used in conjunction with languages with Hindley-Milner type checkers and should be understood in terms of these implementations. foobie-bletch 18:02, 6 August 2009 (UTC)
----
I don't understand the essence of the task.


```J
NB. comparisons of arbitrary data types in j
compare=: <:@:(-: + +:@:A.@:/:@:,:&:<)
assert _1 -: 0 compare 1                NB. less than
assert  0 -:   compare~'abc'            NB. equal
assert  1 -:'0'compare 0                NB. consistent order of
assert _1 -: 0 compare'0'               NB.  dissimilar data types
assert  1 -:'a'compare'@'               NB. greater than

```


If I were to implement red-black tree I'd hash to compare integers, and the result would be to provide associative array functionality.  Does this prove algebraic data type?  I suppose.  Data can be recovered via j verbs.
--LambertDW 20:47, 27 February 2012 (UTC)

:I am uncertain, myself about this task.  Here's what I believe I know:  

::"algebraic data type" means that data types are represented by numbers which list  the number of possible values that can be represented by that data type.  A bit data type would be represented by the number 2.  An 8 bit character would be represented by the number 256.  A 32 bit integer would be represented by the number 4294967296.  A type that represents the program being in an error state would be represented by the number 0.  This system allows you to use algebra to work with the properties of composite and derived types.

::Implicitly, languages which support types often have syntax rules to prevent certain combinations of types from being used.  Typically, these syntax rules get associated with defined words which represent functions, and these syntax rules approximate the domain of those functions.  (In some cases you get an exact match , but this cannot be guaranteed without solving the halting problem.)  This, I think, is what encourages a proliferation of types.

::I think that the task is encouraging the use of a type system which distinguishes between "red" and "black" nodes by using different types for them.  I think that the implicit desire, here, is to use types which are represented syntactically (in code paths).  (But to explicitly require specific syntax would conflict with what rosetta code tasks are.)

::I believe that there are also some other implicit statements here, which I would have to have a different background to appreciate.

:That said, I think the task is also incomplete (it includes no explicit feature tests).  It's also going to be highly inefficient, in J, because of the low fan-out on each node.  So don't worry about efficiency or usefulness if you implement this in J.  --[[User:Rdm|Rdm]] 15:14, 2 March 2012 (UTC)

:: The "algebraic" part is not about underlying representation. It's because the data type itself forms a free algebra. "Algebra" here means a set with some operations (effectively the data type's constructors) on it. A "free" algebra is one where the only way two terms can represent the same element is by being the same term. On the programming side, two things are only the same if they're made by calling the same constructor with the same arguments. A simple example of a free algebra is Peano-encoded natural numbers: Z ("zero") is a 0-ary constructor, and S ("successor") is a unary constructor. Z, S(Z), S(S(Z)), etc. are all different things. If we introduce a P ("plus") constructor, it's no longer free because, e.g., P(Z, S(Z)) = P(S(Z), Z). Pattern matching is the usual way to consume this kind of data type. Pattern matching is a form of conditional where each case in a match expression specifies what the constructor must be and then gives names to the constructor's arguments. J doesn't really have built-in support for ADTs or pattern matching (this is possibly an argument in favor of splitting this page's task as suggested above -- keep the J implementation of red-black tree up, but it probably doesn't belong on a page about destructing an ADT by pattern matching). [[User:Jrslepak|Jrslepak]] ([[User talk:Jrslepak|talk]]) 16:20, 9 September 2014 (UTC)

:J stores symbols in a red black tree.  Maybe we need only show symbol code.  Ha ha.  An implementation detail, not a language feature.  --LambertDW 16:31, 10 March 2012 (UTC)
