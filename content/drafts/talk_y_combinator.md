+++
title = "Talk:Y combinator"
description = ""
date = 2018-10-15T06:17:55Z
aliases = []
[extra]
id = 3989
[taxonomies]
categories = []
tags = []
+++

==Haskell stateless?==
I don't know Haskell, but isn't the definition:
  y f = f (y f)
'''not''' stateless as it seems to be defining y by referring to y. Haskell, no doubt has lazy evaluation to make it terminate, but the task does ask for a non-recursive, stateless definition of y. --[[User:Paddy3118|Paddy3118]] 09:18, 28 February 2009 (UTC)

I googled [http://groups.google.co.uk/group/fa.haskell/browse_frm/thread/f0a62b6de1416d8b this]. --[[User:Paddy3118|Paddy3118]] 09:22, 28 February 2009 (UTC)

:It's impossible to write a fixed-point combinator (or perform any recursion, for that matter) in any statically-typed language, without using either recursive functions or recursive types. --[[User:Spoon!|Spoon!]] 10:42, 28 February 2009 (UTC)

Hi Spoon, The task does not rule out recursive types (I didn't know what they were at the time of writing) - just recursive functions. --[[User:Paddy3118|Paddy3118]] 11:05, 28 February 2009 (UTC)

It's not immediatly clear what is meant by the term "stateless," but let's assume it means something like "definable System F." (most of Haskell can be viewed as a simple extension of System F).  In that case Spoon! is correct; in most flavors of System F, the fixpoint combinator must be a primitive.  This follows from the fact that System F (without fix) is strongly normalizing.  On the other hand it is not the case that "any recursion" requires the general recursive fixed-point combinator.  Primitive recursion can be easily encoded using the Church numerals (which are definable in System F), and covers a variety of interesting recursive definitions.

On the other hand if we add recursive types, they provide a kind of loop-hole that allows us to type the standard fixed-point combinators from untyped lambda calculi; the linked message above demonstrates one way to do this.  The addition of mutable state (a la lisp) also allows the definition of fixed point combinators IIRC.  I believe (although I am less sure about this) that a call/cc primitive is also sufficent to define fixed point combinators.

At any rate, here is a way to define the Y combinator using recursive types in Haskell that is a little nicer than the one in the message above.


```haskell

newtype Mu a = Roll { unroll :: Mu a -> a }

fix :: (a -> a) -> a
fix = \f -> (\x -> f (unroll x x)) $ Roll (\x -> f (unroll x x))

fac :: Integer -> Integer
fac = fix $ \f n -> if (n <= 0) then 1 else n * f (n-1)

fibs :: [Integer]
fibs = fix $ \fbs -> 0 : 1 : fix zipP fbs (tail fbs)
  where zipP f (x:xs) (y:ys) = x+y : f xs ys

main = do
  print $ map fac [1 .. 20]
  print $ take 20 fibs

```


--RWD

== Is this really the "Y" combinator? ==

According to [[wp:Fixed point combinator]], and [http://ttic.uchicago.edu/~pl/classes/CMSC336-Winter08/lectures/lec4.pdf here] page 6, and [http://ttic.uchicago.edu/~pl/classes/CMSC336-Winter08/lectures/lec4.pdf here] page 2, the Y combinator is the precise form <code>λf·(λx·f (x x)) (λx·f (x x))</code>, which does not work for applicative-order evaluation. The version you are using for Python is closely related to what they call the ''Z'' combinator, which is <code>λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))</code> (the version you are using is just one step "before" this; one step of evaluation will produce this). So I am not sure if we should have named the article the "Y" combinator. --[[Special:Contributions/71.106.173.110|71.106.173.110]] 20:49, 1 March 2009 (UTC)

:Hmm, They are related, and my reference, (and others), seem to have lumped them both together under the familiar title "Y combinator". I suggest a redirect of "Z combinator" to this page and a note be added to task. --[[User:Paddy3118|Paddy3118]] 05:48, 2 March 2009 (UTC)

A quick glance over the different implementations shows that most of them define the Z combinator. But that is the closest you can get in applicative-order languages. The Z combinator is just one eta-conversion away from the Y combinator. So in lambda calculus these are basically the same: "two functions are the same if and only if they give the same result for all arguments". [[Special:Contributions/131.155.116.18|131.155.116.18]] 17:28, 30 November 2009 (UTC)


###  Ruby has not the Y combinator 

The current solution for [[Ruby]] is not a Y combinator.


```ruby
# This is not the Y combinator,
# but it is a stateless fixed-point combinator,
# which is the same function as a Y combinator.
Y = lambda do |f|
  lambda {|g| g[g]}[lambda do |g|
                      f[lambda {|*args| g[g][*args]}]
                    end]
end
```


If it was a Y combinator, it would not work.


```ruby
# This is the Y combinator,
# but it does not work in Ruby.
#   (Almost, the real Y combinator would
#    start with f[g[g]] instead of g[g].)
Y = lambda do |f|
  lambda {|g| g[g]}[lambda do |g|
                      f[g[g]]  # This is an infinite recursion.
                    end]
end
```


Ruby has '''eager evaluation''', so f[g[g]] will evaluate g[g] before calling f. However, g[g] will evaluate f[g[g]] again, which will evaluate g[g] again. This is an infinite recursion, and the program will crash, by overflowing the call stack, without ever calling f.

But I can also do '''lazy evaluation''' in Ruby. I only need an extra <tt>lambda{g[g]}</tt> to delay the evaluation.


```ruby
# This is a Y combinator that works in Ruby.
Y = lambda do |f|
  lambda {|g| g[g]}[lambda do |g|
                      f[lambda {g[g]}]
                    end]
end

# This is factorial.
Fac = Y[lambda {|f| lambda {|n| n < 2 ? 1 : n * f[][n - 1]}}]
#                                 extra brackets ^^ evaluate g[g]
```


The extra lambda requires an extra brackets. This is why my factorial function has <tt>f[][n - 1]</tt> and not <tt>f[n - 1]</tt>. The first [] evaluates g[g] (which returns the recursive factorial function), and the second [n - 1] calls the recursive factorial function.

However, I would prefer to write <tt>f[n - 1]</tt> without the extra brackets. The current solution uses <tt>lambda {|*args| g[g][*args]}</tt> to both evaluate g[g] (which returns the recursive function), and to evaluate the recursive function with *args.


```ruby
# This is not the Y combinator,
# but it is a stateless fixed-point combinator,
# which is the same function as a Y combinator.
Y = lambda do |f|
  lambda {|g| g[g]}[lambda do |g|
                      f[lambda {|*args| g[g][*args]}]
                    end]
end

# This is factorial.
Fac = Y[lambda {|f| lambda {|n| n < 2 ? 1 : n * f[n - 1]}}]
#                            no extra brackets! ^^
```


This is not the Y combinator, but it is almost the Y combinator. --[[User:Kernigh|Kernigh]] 03:34, 11 April 2011 (UTC)

==Could We remove the recursive examples!==
After all it ''is'' disallowed by the task. (Haskell, Ocaml) --[[User:Paddy3118|Paddy3118]] 00:19, 28 March 2009 (UTC)

The D example is a good way, I think, of adding content even though it does not perform the task as it:
# States that it does not do the task up-front
# Gives inciteful information on why, and how close you can get.
--[[User:Paddy3118|Paddy3118]] 05:52, 20 May 2009 (UTC)

: I first would analyze why the code author wrote «The usual version» about the recursion one. Maybe it's more natural to Haskell (or functional languages in general)? --[[User:ShinTakezou|ShinTakezou]] 09:34, 20 May 2009 (UTC)
::Hi ShinTakezou, The idea behind the Y combinator is to create recursion without explicit recursion. It is akin to asking for a Bogosort and getting back some other sort. It isn't what the task has asked for. 
::Just as it would be pointless to actually use Bogosort in the real world, it is missing the point to show a recursive fibonacci generator when a Y-combinator one was asked for. --[[User:Paddy3118|Paddy3118]] 20:19, 20 May 2009 (UTC)

==Slate entry incomplete?==
Thanks for adding another language implementation, but:
:''"The task is to define the stateless Y combinator and use it to compute factorials and Fibonacci numbers from other stateless functions or lambda expressions."''
Unfortunately you haven't defined it, and it seems as if it hasn't been used to compute the two functions. --[[User:Paddy3118|Paddy3118]] 05:54, 5 June 2009 (UTC)

==Perl 6 comment on the need for Y?==
Can anyone explain the comment:
:''"Note that Perl 6 doesn't actually need a Y combinator because you can name anonymous functions from the inside"''
I cannot make sense of it as the Y combinator seems to be a way of adding recursion to mainly theoretical functional languages that don't allow variables holding state. I doubt if any of the RC languages actually ''need'' Y so it seems superfluous. --[[User:Paddy3118|Paddy3118]] 09:51, 11 September 2010 (UTC)
::Well, yes, it's superfluous if you want to use state variables instead, but the point is that the Perl 6 construct allows you to continue to write in a stateless idiom if you so choose, and at least encode tail recursion without state.  And if the Y combinator is used for other purposes, well, we can do that too... <tt>:-)</tt>  --[[User:TimToady|TimToady]] 04:05, 12 September 2010 (UTC)

:: Ta! --[[User:Paddy3118|Paddy3118]] 04:31, 12 September 2010 (UTC)

=="Y" combinator in Scheme==
Actually, there is a way to implement the Y combinator in Scheme:

```scheme

(define Y
  (lambda (f)
    ((lambda (x)
       (f (delay (x x))))
     (lambda (x)
       (f (delay (x x)))))))

```

but you have to call the function with 
```scheme
(force f)
```

example:

```scheme

(define fact
  (Y
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((force f) (- n 1))))))))

```

''Edit: Err... I think I messed it up a little, it should be ok now.''
''Edit2: '' You can obviously also do

```scheme

(define fact
  (lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((force f) (- n 1)))))))

```

and then call it as

```scheme

> ((Y fact) 5)
120
```

[[Special:Contributions/93.144.202.116|93.144.202.116]]

==Is the C++ a combinator or just recursion "macro"?==
I was concerned as the definition of Y explicitely depends on Y, which I thought is the kind of thing the Y combinator was trying to dodge! Mind you, my knowledge of this C++ esoteria comes down to reading [http://msdn.microsoft.com/en-us/library/bb982702.aspx this]. --[[User:Paddy3118|Paddy3118]] 06:48, 10 April 2011 (UTC)

Isn't the operator() defined within the const struct : RecursiveFunc calling itself in the line 
return (s->operator()(f, s))(x);
?

== PicoLisp not incorrect ==


```PicoLisp
(de Y (F)
   (let X (curry (F) (Y) (F (curry (Y) @ (pass (Y Y)))))
      (X X) ) )
```


Someone marked this incorrect because "Y is explicitly recursive on Y". The <tt>(Y)</tt> second parameter to <tt>curry</tt> declares a local function parameter Y which is not the same as the original Y. So the original Y never recurses on itself; it calls a different Y. I removed the <nowiki>{{incorrect}}</nowiki> tag from the PicoLisp example. --[[User:Kernigh|Kernigh]] 18:59, 11 April 2011 (UTC)
:Thanks for the explanation Kernigh. I goofed. --[[User:Paddy3118|Paddy3118]] 19:57, 11 April 2011 (UTC)

== Wolfram example ==

This example was added to Fixed-point combinator in wikipaedia. It does not belong there, but it could belong here.

A Y-combinator implementation of factorial in the [[Wolfram Language]] is,

  Y = Function[f, #[#]&[Function[g, f[g[g][##]&]]]];
  factorial = Y[Function[f, If[# < 1, 1, # f[# - 1]] &]];
  factorial[6] (*Yields 120*)

[[User:Thepigdog|Thepigdog]] ([[User talk:Thepigdog|talk]]) 03:18, 26 May 2015 (UTC)

==Comment on the overall task==

For most languages it seems that this task is a joke, as the real use of the Y combinator is to apply recursion to a non-recursive function (often with recursion '''on variables''' otherwise not possible in that particular language) in order to get an output.  In order to implement it, one requires recursion on either functions or lambda functions, as the expression of the Y conbinator requires it in all cases.  By not allowing recursive state, this means that the definition of "Y (or fix) = x where x = f x" (in Haskell syntax) is not allowed as x is a variable state; indeed, for most strict languages this is not directly possible as it causes an infinite race and even with the addition of a lazy as in "let fix f = let rec x = lazy (f x) in x.force()" (F# syntax), this is still not possible in many more imperative type languages.

These languages may require something much more ugly such as follows (C# syntax):

```csharp
  T rv = default(T); // (or other initial condition of the recursion)
  rv = new Lazy(() => rv) // note: gets the '''future value''' (by reference) of rv
  rv = new Lazy(f(rv)); // note: f must be a function from a Lazy<T> to produce T
  return rv.force(); // to get the non-lazy final value.
```


Note that the above code requires mutability of the 'rv' variable and also that it requires that the lambda retrieve the '''future version''' of rv.  Most imperative type languages (and also functional languages other than Haskell) do have mutable variables '''but''' many (such as Rust) do not have the ability to refer to captured free variables future state (captured by value and not by reference as in C#).  This would be the problem that this task would seek to address.

So, to break the infinite recursion when implementing a Y or Z combinator, most of these implementations introduce a recursive "Mu" type as in Haskell as follows:

```haskell
newtype Mu a = Roll { unroll :: Mu a -> a }
 
fix :: (a -> a) -> a
fix = \f -> (\x -> f (unroll x x)) $ Roll (\x -> f (unroll x x))
 
fac :: Integer -> Integer
fac = fix $ \f n -> if (n <= 0) then 1 else n * f (n - 1)
```


Note that the above starts the joke, as all we have done is move the recursive state from the outside at the "fix" function to the inside in the recursive function as 'n' in this case, although it indeed does not use recursive state.

Part of the joke is that the above is useless:  fix fac should produce Integer and an infinite race but it doesn't because the Haskell compiler knows category theory and the Lambda Calculus and can recognize (correctly) that this is an identity that just produces the 'f' function.  In other words, it knows that we may as well have just written it as follows:

```haskell
fac n = if (n <= 0) then 1 else n * fac (n - 1)
```


which is '''the way one would normally write this recursively''' so that is what is effectively generated.  In other words, using fix had absolutely no purpose, although it is possible to do it in this case as it doesn't require closures!

Now other compilers (strict) (even most functional language ones) aren't so smart and either produce an infinite race or are smart enough to recognize that it would be one and error out, but aren't smart enough to recognize the identity.  In the case of dynamically typed languages, this can work by use of the delay/lazy functionality as in the Scheme example above, but this doesn't work for statically typed languages.

So other versions of combinator (Z combinator, etc.) are used such as F# and Ocaml's Z combinator (perhaps among others) as follows (using Haskell signature syntax):

```haskell
Y :: (('a -> 'b) -> 'a -> 'b) -> 'b
```


or the Y combinator using recursive functions as used in C# and Rust (perhaps among others) as follows (again in Haskell signature syntax):

```haskell
Y :: (('a -> 'b) - ('a -> 'b)) -> ('a -> 'b)
```


However, the joke continues as these are just as useless when applied to a function that is already recursive.

The real purpose of the fixed point Y combinator is to implement recursion on functions that are not recursive, as in the "fibs" definition in the Haskell section as follows:

```haskell
fibs_ a = 0:1:(fix zipP a (tail a))
    where
      zipP f (x:xs) (y:ys) = x+y : f xs ys
fibs() = fix fibs_ -- a function so that the head of the lazy list is not held in memory
```


Now this (lazy) list based implementation actually has a purpose as the lambda function is not easy to write in many languages and often depends on being able to write using recursive state and often versions of fix cause recursion in evaluation or runtime over the function.

'''So I propose some changes/additions to this task as follows''':

1. clarifying that the form "fix f = f (fix f)" does or does not have recursive state and whether only the Lambda Calculus versions are acceptable as discussed above but obviously that "fix f = let x = f x in x" does have recursive state, which is not allowed.

2. Adding an objective to the task to use the 'fix'/'Y' function to generate a lazy list such as the above 'fibs' example.

Most languages have a built in "lazy" capability although it may be optional, but if not a non-thread safe version (which can reasonably easily be changed to a thread-safe version) can easily be generated as follows (for Go language):

```golang
type Lazy struct {
	value  *whatever_type_is_to_be_included  // go doesn't have generics
	genf func() *whatever_type_is_to_be_included  // might use interface{} for a poor man's generics
}

func (lzy *Lazy) force() *whatever_type_is_to_be_included {
	if lzy.genf != nil { // not thread-safe
		lzy.value = oll.genf()
		lzy.genf = nil
	}
	return lzy.value
}
```


Given Lazy above, it is generally just as easy to build a LazyList, as follows (again in Go):

```golang
type LazyList struct {
	head  *big.Int
	tail  *Lazy // Lazy of LazyList; should not be accessed directly only through method
}
 
func (oll *LazyList ) rest() *LazyList {
	return oll.tail.force()
}
```


or optionally combine the Lazy and the LazyList

```golang
type LazyList struct {
	head  *big.Int
	tail  *LazyList // should not be accessed directly only through method
	contf func() *LazyList 
}
 
func (oll *LazyList ) Rest() *LazyList {
	if oll.contf != nil { // not thread-safe
		oll.tail = oll.contf()
		oll.contf = nil
	}
	return oll.tail
}
```


Examples for languages capable of generating 'fibs' as per this algorithm would be truly useful and not a joke. --[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 03:32, 11 July 2016 (UTC)

: (1) Please sign your talk page submissions. (I have manually added a signature for you, which I think represents what you would have gotten if you had used the <nowiki>--~~~~</nowiki> wiki signature slug line.)

:: Thank you, forgot... --[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 03:37, 11 July 2016 (UTC)

: (2) On this site, any task which specifies implementation details instead of desired result(s) is going to be (or contain) a joke. See also the [[Rosetta_Code:Add_a_Task|add a task]] page which gives some recommendations centered around the issues of choosing tasks which are useful in a "compare across language" context. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:03, 10 July 2016 (UTC)

:: Yes, I see your point; my issue arose because I actually need something like what I suggested as an additional requirement for one particular language (there may be others) and was frustrated that that particular language seems to be unable to do what I believe I require; whereas the required "fac" and "fib" examples are so trivial (as in a joke) that they can be implemented very easily in other ways not using a Y (or Z) combinator.

:: My purpose in the above sample code is not to specify implementation details, but merely to show that features required in order to accomplish my suggested result objective is possible for languages that don't have those features either built-in or available in a standard library. --[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 03:32, 11 July 2016 (UTC)

==Temporary reversion of a recent Haskell edit==
For the moment I have reverted a couple of edits made to Haskell last night – mainly because the second piece of code no longer compiled. Reversion of the first piece may not have been necessary – reading the editors note, I initially took the phrase 'correcting grammar' as a reference to Haskell grammar, and interpreted the introduction of () as a slight misunderstanding :-)  Perhaps it refers, though, to the English preamble. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:15, 10 October 2018 (UTC)
:I had converted the bindings to the head of the infinite facs` lists to functions to remind users of memory leaks as per [https://wiki.haskell.org/Memory_leak the Haskell wiki]: a binding to the head of an evaluated list means that none of the list can be released; it doesn't matter much here where only 20 elements are evaluated and printed before the program closes, but imagine if the last of a million elements were printed where millions of Megabytes would be tied up - the use of a function means that the head of the list can be released as the list is consumed, meaning that in this case only two elements (the first and second) need be in memory at any given time.

:As to the second part you've temporarily removed, there was a mistake that crept into the type signature for <code>simplefac</code> which should be <code>simplefac :: Integer -> Integer</code>; this prevented it from compiling.

:The reason for the second part, and its included actually third part is that this task is ambiguous as to the type of recursion that is disallowed:  Although because Haskell is non-strict and doesn't have a problem with either recursive bindings (as used in the the <code>fix f = x where x = f x</code> definition) or recursive functions (as used in the <code>fix f = f (fix f)</code> definition with regard to <code>fix</code>), many non-strict languages disallow the first unless there is a function injected (<code>fix f = x() where x = \() -> f x</code>) in the interested of avoiding cyclic data, and a few don't allow recursive functions which call themselves from their own definition.  In these cases, the Y-combinator may be actually useful in allowing recursion where otherwise it would be forbidden.

:I think @WillNess, who posted the second part originally, intended to show that one can avoid binding recursion but still use the "non-sharing point form" of the Y-combinator, and his Y-combinator example of <code>fibs</code> then is essentially the same version as currently used which uses the double <code>fix</code> and completely avoids even function recursion (just in a simpler, easier to understand form in not using the applicative).

:My point in modifying his <code>fibs</code> example, adding an equivalent <code>facs</code> example, and adding the "simple" non-fix/non-Y-combinator versions is that, if function recursion is to be allowed as in the definition of the non-sharing version of <code>fix</code> (with the function <code>fix</code> self referenced in its definition), then there is no need for <code>fix</code>/Y-combinator at all in these cases as they can be defined just using function recursion.--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 22:13, 10 October 2018 (UTC)
:: Understood – many thanks for the explanations – will you fix the non-compiling part and revert the rest in the way seems best to you ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:31, 10 October 2018 (UTC)
::: Good edits and explanations. Many thanks. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:53, 11 October 2018 (UTC)

::::Done and dusted...

::::While I was making sure everything compiled, I did some performance tests, with some interesting results...

::::I didn't change the first and second parts much other than to make the code producing lazy lists functions so the lists can be consumed as evaluated as discussed above, and forced evaluations so they wouldn't be deferred until the end of the calculation, which need showed up in performance testing.

::::However, performance and capability as far as range of usefulness stunk, so I added back the working simple function recursive versions, which are at least ten times faster, don't take so much memory, don't crash for large ranges, and which compile easily without showing up compiler bugs.  I'm a little harsh in evaluating the usefulness of the Y-combinator unless someone can show the error in my analysis...

::::As always, I monitor this Talk page and the main page if anyone would like to discuss it...--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 12:05, 11 October 2018 (UTC)
:::::I've been asked to remove the non-Y-combinator versions as off the topic of this task and am in process of doing so.  I believe the task should be modified in two ways:  1) to clarify if function recursion is allowed although binding recursion clearly isn't: if function recursion isn't allowed then the second Haskell example wouldn't be allowed either, and 2) it should be noted that, other than as an interesting intellectual exercise, the Y-combinator/Z-combinator is rarely of practical use other than for implementing recursion for languages that don't support it, as for languages that do support recursion (the majority of modern languages at least support function recursion) its use comes at a sometimes major performance cost.--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 01:47, 13 October 2018 (UTC)
==Problem with the first non function recursive Haskell version of  Y-combinator==
The first non function recursive implementation of the Y-combinator <code>fix</code> doesn't compile using either GHC version 8.4.3 or 8.6.1 on 64-bit Windows other than in non-optimized interpretive mode (-O0), with an error message about exceeding the "tick count", but increasing this "tick count" immensely doesn't help.  Error message as follows:

```txt
Simplifier ticks exhausted
  When trying UnfoldingDone x_s3FU
  To increase the limit, use -fsimpl-tick-factor=N (default 100).
   
  If you need to increase the limit substantially, please file a
  bug report and indicate the factor you needed.
   
  If GHC was unable to complete compilation even with a very large factor
  (a thousand or more), please consult the "Known bugs or infelicities"
  section in the Users Guide before filing a report. There are a
  few situations unlikely to occur in practical programs for which
  simplifier non-termination has been judged acceptable.
   
  To see detailed counts use -ddump-simpl-stats
  Total ticks: 11445
```


Someone more knowledgeable than I should investigate whether it is possible to modify this to fully compile with optimizations.  With optimization, the only versions of the Y-combinator usable on current GHC versions are the built-in one using binding recursion or the second version using function recursion posted here--[[User:GordonBGood|GordonBGood]] ([[User talk:GordonBGood|talk]]) 01:47, 13 October 2018 (UTC)
