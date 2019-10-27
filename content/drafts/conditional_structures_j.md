+++
title = "Conditional structures/J"
description = ""
date = 2010-09-27T16:41:02Z
aliases = []
[extra]
id = 5301
[taxonomies]
categories = []
tags = []
+++

{{collection|Conditional Structures}}

===if-then===

[http://www.jsoftware.com/help/dictionary/cif.htm if.] is a simple conditional structure in J. As other control words, <code>if.</code> can be used only within explicit definitions (of verbs, adverbs or conjunctions).


```j
   test=: monad define
if. 5 > 4 do. 123 end.
)
   test ''
123
   test=: monad define
if. y > 4 do. y * 2 elseif. y = 4 do. 'exactly' elseif. do. y - 10 end.
)
   test 5
10
   test 4
exactly
   test 3
_7
```

The condition may be omitted, or it may be empty (like an array with 0 elements). In this cases the condition is considered to be satisfied.

===select-case===

[http://www.jsoftware.com/help/dictionary/csel.htm select.] is another conditional structure. <code>case.</code> match causes the execution of this <code>case.</code> branch and then terminates the further structure execution. <code>fcase.</code> match after the execution of the associated code executes the next branch; if that's with <code>fcase.</code> again, it again executes the next branch, etc.


```j
   test=: monad define
t1=. 'Count to three? '
select. y
fcase. 1 do. t1=. t1 , 'one '
fcase. 2 do. t1=. t1 , 'two '
case. 3 do. t1=. t1 , 'three'
case. 4 do. 'Just four'
end.
)
   test 2
Count to three? two three
   test 1
Count to three? one two three
   test 4
Just four
   test 5
Count to three?
```



### power


Another way to execute code conditionally is power conjunction. In the code <code>u ^: v y</code> verb <code>u</code> is executed with argument <code>y</code> only if <code>v y</code> condition is satisfied.


```j
   ('magic number'&[) ^: (=&42) 5
5
   ('magic number'&[) ^: (=&42) 6
6
   ('magic number'&[) ^: (=&42) 42
magic number
```



### agenda


Another way to execute code conditionally is the agenda conjunction.  In the code <code> u0`u1`...`uN @. v y</code> result of v y is an index in the range 0..N which determines which of the functions u0..uN will be executed.  (Also negative numbers greater than -N are treated as N-(v y).)


```j
   (2&+)`(3&+)`(5&+) @. *  2
5
   (2&+)`(3&+)`(5&+) @. *  _2
3
```


Here, * without a left argument is signum (1 for positive numbers and _1 for negative numbers).


### conditions without conditions


Conditional effects can often be obtained without conditional structures.  In J, a boolean result is a 1 or a 0, where 1 represents true and 0 represents false.  If we have a boolean array <code>B</code> which corresponds in shape to a numeric argument <code>Y</code>, and we have a function <code>F</code> where we want the result of <code>F Y</code> where <code>B</code> is true, and instead want the original value of <code>Y</code> where B is false, we can use an expression like:


```J
(Y * -. B) + B * F Y
```


This also assumes, of course, that F is well behaved (that we can ignore any issues related to side effects), and has the right shape.  [The token <code>-.</code> is J's boolean "not" verb.  And the tokens <code>+</code> and <code>*</code> are J's addition and multiplication verbs.]

If you do not want to pay for the execution of <code>F Y</code> for cases where <code>B</code> is false, and if <code>Y</code> is a simple list, then a variation would be:
   

```J
(Y * -. B) + F&.(B&#) Y
```


For example:


```J
   Y=: p: i. 5
   Y
2 3 5 7 11
   B =: 1 0 1 0 1
   F=: *:
   (Y * -. B) + B * F Y  NB. square some but not all primes
4 3 25 7 121
   F B # Y
4 25 121
   (Y * -. B) + F&.(B&#) Y
4 3 25 7 121
```


Here <code>#</code> is J's "compress" or "selection" verb.  For example <code>1 0 1 # 1 2 3</code> gives us <code>1 3</code>.  And when we combine a verb and a noun, <code>&</code> curries the verb with that noun (so <code>+&1</code> produces a verb that adds 1 to its argument).  And the two character token <code>&.</code> uses the verb on its right to map into a different domain and then its inverse to map back to the original domain.  In other words, here we preprocess by eliminating the arguments from Y which we do not want to have changed and we post process by expanding the result back to its original length (and since 0 is the fill value for numeric arrays, we get 0s in the positions where we were ignoring elements of Y).
