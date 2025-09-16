+++
title = "Talk:Partial function application"
description = ""
date = 2011-06-14T18:13:40Z
aliases = []
[extra]
id = 9397
[taxonomies]
categories = []
tags = []
+++

==Explicit curry vs Partial application==

In Python there is a clear difference. Here's explicit currying:


```python
def fsf1(fs, f1):
    return lambda s: fs(f1, s)
```


I would like to tie this task down, but now recognise that it may be difficult. 

Any programmers, especially those with functional as well as none functional language experience care to comment? --[[User:Paddy3118|Paddy3118]] 02:28, 26 March 2011 (UTC)

:With [[Common Lisp]], the explicit currying seems to be the only way to do the partial application. I try to show this with code. This is <tt>mapcar</tt> with 2 arguments.

:
```lisp
CL-USER> (defun f1 (n) (* n 2))
F1
CL-USER> (mapcar #'f1 '(2 3 5 7))
(4 6 10 14)
```


:I can use <tt>lambda</tt> for the explicit currying, like one would in Python.

:
```lisp
CL-USER> ((lambda (s) (mapcar #'f1 s)) '(2 3 5 7))
(4 6 10 14)
```


:I can also define a <tt>partial</tt> function for the partial application; but it uses the explicit currying to do so.

:
```lisp
CL-USER> (defun partial (f &rest args)
	   (lambda (&rest args2) (apply f (append args args2))))
PARTIAL
CL-USER> (funcall (partial #'mapcar #'f1) '(2 3 5 7))
(4 6 10 14)
```


:There is not much reason to write <tt>(partial #'mapcar #'f1)</tt> instead of <tt>(lambda (s) (mapcar #'f1 s))</tt>. This would be like writing <tt>sum((2, 3))</tt> instead of <tt>2 + 3</tt> in Python. --[[User:Kernigh|Kernigh]] 02:34, 1 April 2011 (UTC)

:: Is it the case that there is no real distinction in Lisp, but a significant distinction in other languages?
::What about the other feature I seem to see in partial application: that of not needing to refer explicitely to the other arguments of the function being partially applied? E.g. with function f(a,b,c,d); you can partial(f, a=value1) to produce f'(b,c,d) without mention of b, c, and d when ''calling'' partial. --[[User:Paddy3118|Paddy3118]] 06:29, 1 April 2011 (UTC)

::: First considering the feature of not referring to other arguments, I would expect this to be possible in most languages with dynamic types, but not possible in languages with static type checking.  Kernigh's <tt>partial</tt> handles this just fine using apply, for example.  [http://www.engr.uconn.edu/~jeffm/Papers/curry.html This document] I found calls it generalized explicit currying and gives example code in Scheme.  In Go, with it's static type checking, I can write a <tt>partial</tt> that does explicit currying for functions with specific type signatures, but I can't write a <tt>partial</tt> that does ''generalized'' explicit currying for functions with arbitrary type signatures.

:::Which leads back to the first question of the distinction between <tt>(partial #'mapcar #'f1)</tt> and <tt>(lambda (s) (mapcar #'f1 s))</tt>.  There are two distinctions!  Function/lambda expression and generalized/specific.  (And they are orthogonal:  You could write a specific function or a generalized lambda expression.)  Anyway, there seems little point in either a function or generality in the case of this task as currently written.  You could just write <tt>fsf1 = lambda s: fs(f1, s)</tt> in Python, for example.

:::As far as changes to the task, I think it's fine the way it is.  &mdash;[[User:Sonia|Sonia]] 06:28, 13 April 2011 (UTC)

:::: Hi. Ocaml and Haskell are statically typed and yet don't seem to need to mention the other arguments. At this stage, I want a task that fits the page title so am trying to explore what partial function application could mean for many languages. --[[User:Paddy3118|Paddy3118]] 07:00, 13 April 2011 (UTC)

:::: If not mentioning the other args. was made a task restriction, could it then be done in Go? --[[User:Paddy3118|Paddy3118]] 07:04, 13 April 2011 (UTC)

:::::Touché.  If not mentioning the other args was made a task restriction, I'm pretty sure it could not be done in Go.  Go requires static typing yet has no way of specifying one function type in terms of another function type.  It might be a good restriction.  Simple closures are already demonstrated in tasks like Function composition and First class functions.  I might reword it "Note that in the partial application of a parameter, (in the above case param1), the only inputs are f and param1.  The return value and arguments of f' are determined solely by f and are not explicitly mentioned. This is an essential feature of partial function application."  I might also remove "( s )" from task bullets 4 and 5, so that they read just "Partially apply f1 to fs to form function fsf1."  It's a little more cryptic, but it removes a possible suggestion that it's okay to specify the argument list of fsf1.  &mdash;[[User:Sonia|Sonia]] 18:32, 13 April 2011 (UTC)

:::: Thanks Sonia. Anyone else OK with such an update? (Remember, the idea isn't to exclude Go; it is to more explicitely define partial application). --[[User:Paddy3118|Paddy3118]] 21:08, 13 April 2011 (UTC)

: The update is OK with me.

: I used <tt>(partial #'fs #'f1)</tt> to solve Common Lisp. I also solved Java. Java is a statically typed language, so the solution is like Ocaml or Haskell: I have to curry the <tt>fs()</tt> method. I start with this Java code.

: 
```java
static int[] fs(IntegerFunction f, int[] s) {
	int[] r = new int[s.length];
	for (int i = 0; i < s.length; i++)
		r[i] = f.call(s[i]);
	return r;
}

static int[] fsf1(int[] s) {
	return fs(f1, s);
}
```


: This <tt>fsf1()</tt> explicitly curries <tt>fs()</tt>, mentions <tt>s</tt> and passes the return value from <tt>fs()</tt>. I change the code.

: 
```java
interface SequenceFunction {
	int[] call(int[] arg);
}
 
static SequenceFunction fs(final IntegerFunction f) {
	return new SequenceFunction() {
		public int[] call(int[] s) {
			int[] r = new int[s.length];
			for (int i = 0; i < s.length; i++)
				r[i] = f.call(s[i]);
			return r;
		}
	};
}

static SequenceFunction fsf1 = fs(f1);
```


: This <tt>fs()</tt> explicitly curries ''itself''. So <tt>fsf1</tt> never has to mention <tt>s</tt> nor pass the return value from <tt>fs()</tt>. --[[User:Kernigh|Kernigh]] 20:26, 14 April 2011 (UTC)

:: Hi Kernigh, the above is ''not'' like Haskel, in fact it doesn't really answer the task as you have made fs a function of only f, then called it with different f. Partial application would be like the Haskel: fs is a function of f ''and s'' and then fsfl is the result of applying only f1 to fs, without mention of any other argument. The above may give a result, but it is ''how'' it gets to that result that is the issue. --[[User:Paddy3118|Paddy3118]] 23:07, 14 April 2011 (UTC)

: I believed the description of the Haskell solution: "All functions actually take exactly one argument." I tried to do the same thing for Java: I wrote an ''fs()'' method that takes exactly one argument. The Java syntax is far worse than the Haskell syntax, but the result seems to be the same: ''fs'' takes exactly one parameter ''f'' and returns another function that takes exactly one parameter ''s''. When one says ''fs(arg1).call(arg2)'' in Java, then ''fs'' is a function of two parameters.

: One problem is that some program might already have ''fs(arg1, arg2)'', and I want to partially apply it. I might solve this by wrapping the method so I have both ''fs(arg1, arg2)'' and ''fs_curried(arg1).call(arg2)''. It might look like this.

: 
```java
static int[] fs(IntegerFunction f, int[] s) {
	int[] r = new int[s.length];
	for (int i = 0; i < s.length; i++)
		r[i] = f.call(s[i]);
	return r;
}

interface SequenceFunction {
	int[] call(int[] arg);
}

static SequenceFunction fsCurried(final IntegerFunction f) {
	return new SequenceFunction() {
		public int[] call(int[] s) {
			return fs(f, s);
		}
	};
}

static SequenceFunction fsf1 = fsCurried(f1);
```


: With the current solution for Java, ''fs(arg1).call(arg2)'' is already a function of two arguments, and there is no reason to also have ''fs(arg1, arg2)''.

: Haskell seems to have an analogy for ''fs(arg1, arg2)''. I looked around the Haskell 2010 report, and learned that Haskell has tuples. A function has only one parameter, but that parameter might be a tuple of 2 items. I also found these functions in the [http://www.haskell.org/onlinereport/haskell2010/haskellch9.html Haskell prelude]:

: 
```haskell
-- curry converts an uncurried function to a curried function;  
-- uncurry converts a curried function to a function on pairs.  
curry            :: ((a, b) -> c) -> a -> b -> c  
curry f x y      =  f (x, y)

uncurry          :: (a -> b -> c) -> ((a, b) -> c)  
uncurry f p      =  f (fst p) (snd p)
```


: It seems that one might define <tt>fs (f, s) = map f s</tt>, a function that takes a tuple and returns a sequence, then use <tt>curry fs f1</tt> as partial application. (There is no Haskell implementation on my computer, so I cannot test this. Perhaps I am wrong.) With the current solution for Haskell, ''fs arg1 arg2'' is already a function of two arguments, and there is no reason to also have ''fs (arg1, arg2)''.

: The current Java solution already solves the task: ''fs(arg1).call(arg2)'' is a function of two arguments, and ''fs(arg1)'' is a partial application. --[[User:Kernigh|Kernigh]] 02:15, 15 April 2011 (UTC)

==Is Scala correct?==
I don't know scala, but it seems that in the following:

```scala
def fs(f:Int=>Int, s:List[Int])=s map f
def f1(x:Int)=x*2
def f2(x:Int)=x*x
		
def fsf1=fs(f1,_:List[Int])
def fsf2=fs(f2,_:List[Int])

println(fsf1(List(0,1,2,3)))
println(fsf1(List(2,4,6,8)))
println(fsf2(List(0,1,2,3)))
println(fsf2(List(2,4,6,8)))
```

In the definition of fsf1, the second argument to fs is explicitely mentioned as '_' - as is the return type of List[Int]. Compare it to the Haskel where both are implied.

I think I will have to mark this as incorrect. --[[User:Paddy3118|Paddy3118]] 23:45, 14 April 2011 (UTC)

: I tried Scala. The _ seems to be a "partial application underscore". In ''fs(f1,_:List[Int])'', the _ has no value. So ''fs(f1,_:List[Int])'' returns a function that takes the missing value. The interpreter seems to think that ''fsf1'' is a variable of type ''(List[Int]) => List[Int]''. I have not found the documentation for the "partial application underscore", so I am not sure how it works. I am not the author of the Scala solution. --[[User:Kernigh|Kernigh]] 03:47, 20 April 2011 (UTC)

:: Hi Kernigh, if  fs were defined as taking 3 arguments and currying would still refer to fs(f1,_) and not fs(f1,_,_) then I would be inclined to accept it as the '_' would stand for "any other arguments" rather than "any one argument" and would retain most of the insensitivity to the number of arguments of the Haskel-type implementations.

: 
```scala
scala>
 def rot(x: Int, y: Int, z: Int) = (y, z, x)
rot: (x: Int,y: Int,z: Int)(Int, Int, Int)

scala> rot(2, 3, 7)
res0: (Int, Int, Int) = (3,7,2)

scala> def a(y: Int) = rot(_: Int, y, _: Int)
a: (y: Int)(Int, Int) => (Int, Int, Int)

scala> a(3)(2, 7)
res1: (Int, Int, Int) = (3,7,2)
```


: It seems that _ is any one parameter. So, the Scala "partial application underscore" fails the task requirement that "other parameters are not explicitely mentioned". --[[User:Kernigh|Kernigh]] 02:17, 21 April 2011 (UTC)

I hope I have fixed this by clearly stating up front that Scala doesn't follow the description and where. This means that there is no doubt as to where the Scala moves away from the task description. The alternative is to delete the example and omit the language from the task.  --[[User:Paddy3118|Paddy3118]] 05:35, 21 April 2011 (UTC)

: I've never written any Scala code and only seen a few samples, but this does look like partial application to me (quite similar to Perl 6's Whatever-Star). The fact that you have to mention a type signature might just be a limitation of Scala's type inferencer. The key question is: can you use this to pass an arity 3 function to map without using any intermediate definitions or lambda's? &mdash;''[[User:Ruud Koot|Ruud]]'' 07:05, 21 April 2011 (UTC)

:: 
```scala
scala>
 def f(a: Int, x: Int, b: Int) = a * x + b
f: (a: Int,x: Int,b: Int)Int

scala> List(1, 2, 3).map(f(10, _, 1))
res4: List[Int] = List(11, 21, 31)
```


:: Yes, you can pass arity 3 function to map. The _ has no type signature. A _ without a type signature can be an error, so I am not sure why it worked here. (I am new to Scala, and not the author of the Scala solution.) --[[User:Kernigh|Kernigh]] 19:00, 21 April 2011 (UTC)

==Is Lisp correct?==
I need to query the fact that partial only applies in this case of a being applied to a function of two arguments. A correct partial should, given a function with N parameters and any of M arguments, where M<Nm should then return a partially applied function of N-M parameters. I.e. it should work equally well for fs2(fa, fb, s) where partial(fs2, f1) should return a function of (fb, s); and partial(fs2, f1, f2) should return a function of s.

Lisp may well have a way of doing this, but I don't think the present example shows it. --[[User:Paddy3118|Paddy3118]] 23:57, 14 April 2011 (UTC)

: The current task only requires N = 2. The current ''partial'' uses ''&rest'' to accept a variable number of arguments. It can work when N > 2, but I never tested it until now. The next example shows N = 6 and N = 4.

: 
```lisp
CL-USER> (defun partial (func &rest args1)
	   (lambda (&rest args2) (apply func (append args1 args2))))
PARTIAL
CL-USER> (defun take6 (a b c d e f)
	   (list f b d c a e))
TAKE6
CL-USER> (take6 11 22 33 44 55 66)
(66 22 44 33 11 55)
CL-USER> (defvar take2 (partial #'take6 11 22 33 44))
TAKE2
CL-USER> (funcall take2 55 66)
(66 22 44 33 11 55)
```


: --[[User:Kernigh|Kernigh]] 00:53, 15 April 2011 (UTC)
:: Not only does &rest do the right thing with the arguments, but apply does the right thing with the return value.  I think this lisp solution satisfies the concept of partial function evaluation.  &mdash;[[User:Sonia|Sonia]] 01:45, 15 April 2011 (UTC)

==Is D correct?==

The D solution is a not perfect implementation, it's an approximation, but it's a D idiomatic way to solve the problem. If you don't accept an approximation, it becomes hard to implement in D. My suggestion is to accept approximate solutions too.

And in the D version fs() is a templated function. It's the way you write generic code in such languages.

: HI, some language features are ''ways'' of doing things - like list comprehensions, or case statements. It would be wrong to not use a case statement if one is called for in a task, but in these cases I think it would be acceptable to either:
:# Omit a language if it does not support the feature.
:# Or explicitely state that the feature is missing from the language then go on to explain your languages idiomatic way of supporting the feature.
: Since you state the D solution is idiomatic, then if the comment was changed to something like:
:: ''"D language does not support PFA, but the following solution ..."''
: would be OK by me. What do others think? --[[User:Paddy3118|Paddy3118]] 21:03, 15 April 2011 (UTC)
:: I think it's the call of the primary task author.  One way to write a task is to make very specific requirements that a certain technique be used.  Languages that can't do it get marked omit, and the result is a clean comparison of that technique in different languages, without the clutter of similar but not-the-same techniques.  A different way to write a task is to explain "here's a technique that achieves some goal.  Demonstrate the technique in your language, or, if the technique is missing in your language, show the idiomatic way of achieving the same goal."  Both are valid ways to write a task, but I really like it when the task description makes it clear one way or the other.  For an example, Go doesn't do named or optional parameters.  The named parameter task was very specific and I marked it omit.  The optional parameter task was worded the second way, even putting in bold "whatever way is most natural to your language."  So I enjoyed coding up an alternative way to achieve a similar effect.  &mdash;[[User:Sonia|Sonia]] 10:16, 16 April 2011 (UTC)
::: I have added an extra note on not explicitely mentioning other parameters. It's now in twice so I hope the task now matches your first style, although I still thinks it is fine to state that it cannot be done, then give a laguages idiomatic way too. --[[User:Paddy3118|Paddy3118]] 15:52, 16 April 2011 (UTC)

Modified the D version. The Task doesn't specify that f has to be a run-time function. In this D implementation f has to be known at compile-time.

:Hi, The task description begins ''"Create a function fs( f, s ) that takes a '''function''', f( n ), of one value "''. 
:So it is ''required'' that f be a function. Why not just say it is not possible in D if it is not, then explain why before giving the D code? --[[User:Paddy3118|Paddy3118]] 18:26, 16 April 2011 (UTC)
:: Partial application is mainly useful if it can be applied to idiomatically defined functions (in particular functions from the standard library). Perhaps the languages can be subdivided into two categories. One where this is possible and one where this is not? &mdash;''[[User:Ruud Koot|Ruud]]'' 09:34, 17 April 2011 (UTC)

== Proposal for new task description ==

I would propose to extend the task description here include both currying and partial application:
* Define the linear function ''f'' : (''a'', ''b'',  ''x'') ↦ ''ax'' + ''b'' in uncurried form.
* Define the linear function ''g'' : (''a'', ''b'',  ''x'') ↦ ''ax'' + ''b'' in curried form.
* Perform the following operations:
*# Curry ''f'', apply ''a'' = 7, ''b'' = 9, apply to a sequence of ''x'''s.
*# Partially apply ''a'' = 7  and ''b'' = 9 to ''f'', and apply to a sequence of ''x'''s.
*# Apply ''a'' = 7  and ''b'' = 9 to ''g'', and apply to a sequence of ''x'''s.
&mdash;''[[User:Ruud Koot|Ruud]]'' 15:32, 15 April 2011 (UTC)

:Hi Ruud, I would prefer keeping them separate as it seems it is hard enough getting results for Partial function application on its own. Extending the task would further complicate things. How about a separate Curry task written to emphasize the difference between it and Partial application? --[[User:Paddy3118|Paddy3118]] 20:42, 15 April 2011 (UTC)

:: Yes, but currying and partial application are quite closely related and often confused. It may be easier to understand them if they are both used together. Also, if I managed to implement the Ruby example below correctly, currying and partial application seems both to be implemented by Proc#curry. Finally, regarding the complications there seem to be with some of the language: I'm not yet convinced partial application (and currying) can be ''reasonably'' implemented in all languages (as papply and curry are higher-order functions returning a function, I assume you would at least need first-class functions/closures or go through the trouble of emulating those). I'll try to write this task in a few more languages to see if it can be done or not. &mdash;''[[User:Ruud Koot|Ruud]]'' 21:56, 15 April 2011 (UTC)

:::Hi again Ruud. I was sure that there would be languages with first-class functions that would not be able to curry/partially-apply. What is new to me is that there may well be languages that are OK with currying, but have problems with partial application as they can't apply a subset of arguments without ''explicitely'' mentioning all arguments in the call to partial. Some manidestly, statically typed languages - i.e. those that require a type signature for everything, would need different and explicit type signatures dependant on how many arguments were being partially applied for example; Haskel works it out for itself. 
:::Somehow, I do notice the similarity between currying and PFA, but unlike you, I see it as a strong reason to have two, contrasting tasks. What to do? --[[User:Paddy3118|Paddy3118]] 03:28, 16 April 2011 (UTC)
:::: I think it's somewhat more than a similarity: in the Haskell example we have that <code>papply2</code> = <code>curry</code> and <code>papply3</code> &approx; <code>curry3</code>. In the Ruby partial application and currying are both implemented by <code>Prox#curry</code>. It would be fun to see how a <code>curry</code> function in Python looks that works for functions of any arity like <code>partial</code> does (if this is possible) and how similar their definitions would be. Their currently isn't a task on [[Currying]]. Would it be a problem to create a new task [[Currying and partial application]] or does Rosetta Code strive to avoid duplicated and overlapping entries? &mdash;''[[User:Ruud Koot|Ruud]]'' 09:40, 17 April 2011 (UTC)

Here are samples in Haskell and Python:


```haskell

module Main where

curry3 :: ((a, b, c) -> r) -> a -> b -> c -> r
curry3 f x y z = f (x, y, z)

papply2 :: ((a, b) -> r) -> a -> b -> r
papply2 f x = \y -> f (x, y)

papply3 :: ((a, b, c) -> r) -> a -> (b, c) -> r
papply3 f x = \(y, z) -> f (x, y, z)

f :: (Integer, Integer, Integer) -> Integer
f (a, b, x) = a * x  + b

g :: Integer -> Integer -> Integer -> Integer    -- idiomatic
g a b x = a * x + b

g' :: Integer -> (Integer -> (Integer -> Integer))
g' = \a -> \b -> \x -> a * x + b

main = let u = curry3 f
           v = papply2 (papply3 f 7) 9
           w = g 7 9    -- idiomatic
       in print [map (u 7 9) [1..5], map v [1..5], map w [1..5]]

```



```python

from functools import partial

def curry3(f):
  return lambda a: lambda b: lambda x: f(a, b, x)

def f(a, b, x):    # idiomatic
  return a * x + b

def g(a):
  return lambda b: lambda x: a * x + b

def main():
  u = curry3(f)
  v = partial(f, 7, 9)
  w = g(7)(9)    # non-idiomatic
  print [map(u(7)(9), [1,2,3,4,5]), map(v, [1,2,3,4,5]), map(w, [1,2,3,4,5])]

```


Possibly-correct Ruby 1.9 implementation:


```ruby

def f
  proc {|a, b, x| a * x + b}
end

def f2(a, b, x)
  a * x + b
end

def g
  proc {|a| proc {|b| proc {|x| a * x + b}}}
end

def main
  u = f.curry
  v = f.curry[7,9]
  w = g[7][9]
  puts [ (1..5).map {|x| u[7][9][x]} \
       , (1..5).map {|x| v[x]}       \
       , (1..5).map {|x| w[x]}       ]
end

main

```


: With Ruby, you can also curry f2:

: 
```ruby
def f2(a, b, x)
  a * x + b
end

u = method(:f2).to_proc.curry
v = u[7,9]
p [ (1..5).map {|x| u[7][9][x]}, (1..5).map {|x| v[x]} ]
# => "[[16, 23, 30, 37, 44], [16, 23, 30, 37, 44]]"
```


: I prefer a new task, not a change to this task. --[[User:Kernigh|Kernigh]] 00:25, 16 April 2011 (UTC)


###  Current task description is a bit odd 

Reading over the current task description, I think it is somewhat odd. It asks you to define a function ''f'' taking a single argument, and a higher-order function ''fs'' (essential ''map'') and to partially apply ''f'' to ''fs''. The most common use case of partial application, however, would be, given a function ''f'' of arity at least 2, partially apply arguments until it has remaining arity 1 and then pass this partially applied function to ''fs'' (i.e. ''map''). Even if we do not want to adopt my proposal above, I believe the current task description should still be modified. I would in this case advise ''f'' to be of arity 3, as this makes the task slightly more challenging and interesting for statically-typed languages. &mdash;''[[User:Ruud Koot|Ruud]]'' 10:29, 17 April 2011 (UTC)

: My suggested task description would read:
:* Define the linear function ''f''(''a'', ''b'',  ''x'') ↦ ''ax'' + ''b''. This function should preferably by defined in an idiomatic form, but if this is not possible other solutions are acceptable.
:* Define the function ''[[map]]''(''f'', ''xs'') which applies its first argument, a function expecting a single argument, to each element of the sequence ''xs''. Preferably use the implementation from the standard library, if available.
:* Perform the following operations: partially apply ''a'' = 7  and ''b'' = 9 to ''f'', and apply this partially applied function to each element of a sequence using ''map''.
: &mdash;''[[User:Ruud Koot|Ruud]]'' 12:46, 20 April 2011 (UTC)

:: But how to ensure ''[http://www.haskell.org/haskellwiki/Partial_application partial application]'' is what a Haskel, or other functional programming language programmer would think is partial application? The temptation is for folks to just concentrate on getting the right numerical answer and ignore or miss attempts to constrain ''how'' the result is computed. --[[User:Paddy3118|Paddy3118]] 14:10, 20 April 2011 (UTC)

::: This is beginning to sound like a Haskell-specific concept, rather than an algorithmic concept.  --[[User:Rdm|Rdm]] 17:52, 20 April 2011 (UTC)

:::: Well yes, partial application is a programming language concept, not an algorithm or design pattern. But it can be "done" a number of languages. &mdash;''[[User:Ruud Koot|Ruud]]'' 17:54, 20 April 2011 (UTC)

::: Using common sense and discussion? I don't think the task should be overspecified either. For example, specifying that you have to define a function similar to Python's <code>partial</code> would rule out ML's and Haskell's implicit partial application or perhaps a clever, but reasonable, trick involving C's preprocessor or C++'s templates. On the other hand, the current Java implementation might follow the the task to the letter, but certainly violates the spirit as it's not applicable to idiomatically defined functions from the standard library. &mdash;''[[User:Ruud Koot|Ruud]]'' 17:54, 20 April 2011 (UTC)

== What is partial application? ==

It occurred to me that when one speaks of "partial application" one can refer to several distinct things:
# The ''syntactic sugar'' that allows one to write <code>map (f 7 9) [1..9]</code> or <code>map(f(7,_,9),{1,...,9})</code> instead of <code>map (lambda x: f 7 9 x) [1..9]</code> or <code>def g(x): return f(7,9,x); end def in map(g,(1..9))</code>.
# The ''higher-order function'' <code>functools.partial</code>.
# The ''compilation technique'' that allows function thunks or closures to be "partially applied" instead of only unapplied or fully applied.
The task here should probably be about the first, perhaps also the second. &mdash;''[[User:Ruud Koot|Ruud]]'' 15:35, 21 April 2011 (UTC)

== Suggested changes to the task description ==

1) There is no need to require functions to be named <code>f1</code>, <code>f2</code>, <code>fsf1</code> or <code>fsf2</code>, and doing so excludes solutions in languages that don't allow digits to be used in identifiers.

2) The wording "partially apply <code>f1</code> to <code>fs</code>" should be changed to "partially apply <code>fs</code> to <code>f1</code>" because it's more conventional to speak of functions being applied to arguments than vice versa, and in this case <code>f1</code> is the argument.

3) Please be consistent about distinguishing between functions themselves and expressions in which they are applied to an argument. The wording "create a function <code>fs(f,s)</code> that takes a function <code>f(n)</code> ..." should be changed to "create a function <code>fs</code> that takes a function <code>f</code> ...". The expression <math>f(n)</math> refers conventionally not to the whole function <math>f</math> but to the single output associated with the argument <math>n</math> (unless you're an economist, in which case you'll infer an implied universal quantification with respect to the variable <math>n</math>, except when you don't).

--[[User:Sluggo|Sluggo]] 18:09, 14 June 2011 (UTC)
