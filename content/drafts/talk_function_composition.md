+++
title = "Talk:Function composition"
description = ""
date = 2013-12-16T16:25:16Z
aliases = []
[extra]
id = 3994
[taxonomies]
categories = []
tags = []
+++

==Limitation of First-class functions==
Is this task subject to the "limitation" of [[First-class functions]], ruling out C (or Fortran, or...), or we can accomplish the task implementing just a function that returns f(g(x)), having as argument f, g and x? --[[User:ShinTakezou|ShinTakezou]] 15:17, 3 March 2009 (UTC)
:Hi, the limit is you have to create a function of f and g that returns another function. It is that other function, when applied to x would be the same as doing f(g(x)). If you look at the Python example, function compose returns function sin_cos. it is then sin_cos(x) that is equivalent to sin(cos(x)). In short, you need to create function compose. Thanks. --[[User:Paddy3118|Paddy3118]] 02:54, 4 March 2009 (UTC)
::I.e. this is possible only for languages that have [[First-class functions]] (by the way, it seems like this task is already covered by showing that the language has first class functions in [[First-class functions]] task page) --[[User:ShinTakezou|ShinTakezou]] 11:27, 4 March 2009 (UTC)
:::Thats right, it is another aspect of first class functions but there is no need to show functions as members of other collection types. Some languages may be able to do this and not First Class Functions. --[[User:Paddy3118|Paddy3118]] 15:48, 4 March 2009 (UTC)
::Let me show another doubt of mine. What a function ''is'' for a language, is definible inside the same language; C can deal naturally with pointers, and we pass ''function'' to e.g. other functions by pointer (reference?); so foo(bar) call foo with argument bar (by the way, "calling a subroutine" for compiled languages means always to know its address, even at the end of the games a run-time, such the one of Objective-C when "finding" the code for a selector, ''compute'' the address of other compiled code...), and "foo" ''is'' the function (which C handles by pointer). So this task, if the right constraint of [[First-class functions]] is dropped (exactly the first: "New functions can be created from others at run time"), can be implemented in C. If it is not droppable, languages can accomplish this task iff they "have" first class functions. Am I reasonably right? --[[User:ShinTakezou|ShinTakezou]] 16:23, 4 March 2009 (UTC)


::IF you can create a version of compose that works for function pointers when you have an arbitrary set of two functions to compose then why not put it down? I add the extra restriction because if you had n sets of f and g functions to compose, i.e.
  composeN = compose(fN, gN) for N in 0..N-1 
::Then any of the composeN should be able to be used after all have been created. I can think of a naive implementation using function pointers where the last call to compose would reset the actions of all the composeN - this is not what is meant. My C is a little rusty but I guess if you could do something like:

```c
  fg =  compose(&f, &g)
  (*fg)(x) /* Where (*fg)(x) == f(g(x)) and another call to compose would leave this one alone */
```

::--[[User:Paddy3118|Paddy3118]] 19:27, 4 March 2009 (UTC)

:::Hm, I was thinking about other (not portable...) methods; I will try... ;) --[[User:ShinTakezou|ShinTakezou]] 22:15, 4 March 2009 (UTC)
::: Spoon! I think you beated me:D At the beginning I was following a method similar to yours (except that I called ''function capsule'' something similar to what you called functor). But I was dissatisfied since this works only for function with double arg and returning double; I was thinking about a way of composing functions not dependently by the type of the value passed/returned, I thought to ''encapsulate'' the function in a function (!) which wants void * as input and ret value, and then it can be "dereferenced" and casted properly by the programmer (and a macro facility)... I was experimenting different methods (also using inline assembly:D) ... at the moment, failing:(... so I calm down unless your solution is marked as not ok for the task :D. Interesting. --[[User:ShinTakezou|ShinTakezou]] 23:26, 4 March 2009 (UTC)

::::Yea, <code>void *</code> would work. I didn't use it because then I would have to allocate and de-allocate doubles all over the place. But feel free to change it. --[[User:Spoon!|Spoon!]] 00:01, 5 March 2009 (UTC)

Spoon, Shintakezou; I think the C solution should stay, as it does show the kind of hoops you would have to go through to implement this task in C. It also helps to explain the second paragraph in [[wp:First-class_function#Availability|First-class function: Availability]], on why they don't normally include C in the list of FP languages and mention the limitations of function pointers. --[[User:Paddy3118|Paddy3118]] 23:53, 4 March 2009 (UTC)

==J==
Function composition is fundamental in J.  J has a rich set of composition primitives and syntax to combine functions in multiple, interesting ways.

In the following examples, <tt>'''f'''</tt> and <tt>'''g'''</tt> are functions, <tt>''x''</tt> and <tt>''y''</tt> are variables (data).  The syntax <tt>'''f''' ''y''</tt>  means the function <tt>'''f'''</tt> applied to one argument, <tt>''y''</tt> (unary), whereas <tt>''x'' '''f''' ''y''</tt> means the function <tt>'''f'''</tt> applied to (between) two arguments, <tt>''x''</tt> and <tt>''y''</tt> (binary).

Here's a selection of J's composition options:

{| class="wikitable"
|-
! Composition: <tt>∘</tt>
! Unary interpretation: <tt>'''f'''∘'''g''' y</tt>
! Binary interpretation: <tt>x '''f'''∘'''g''' y</tt>
! Notes
|-
|<tt>@</tt>
|<tt>'''f'''('''g''' ''y'')</tt>
|<tt>'''f'''(''x'' '''g''' ''y'')</tt>
|<tt>'''f'''</tt> applied to each output of <tt>'''g'''</tt> independently
|-
|<tt>@.</tt>
|
|
| To be discussed
|-
|<tt>@:</tt>
|<tt>'''f'''('''g''' ''y'')</tt>
|<tt>'''f'''(''x'' '''g''' ''y'')</tt>
|<tt>'''f'''</tt> applied to all outputs of <tt>'''g'''</tt> simultaneously
|-
|<tt>&</tt>
|<tt>'''f'''('''g''' ''y'')</tt>
|<tt>('''g''' ''x'')'''f'''('''g''' ''y'')</tt>
|<tt>'''f'''</tt> applied between each output of <tt>'''g'''</tt> on <tt>''x''</tt> and <tt>''y''</tt> pairwise
|-
|<tt>&.</tt>
|
|
| To be discussed
|-
|<tt>&.:</tt>
|
|
| To be discussed
|-
|<tt>&:</tt>
|<tt>'''f'''('''g''' ''y'')</tt>
|<tt>('''g''' ''x'')'''f'''('''g''' ''y'')</tt>
|<tt>'''f'''</tt> applied between all outputs of <tt>'''g'''</tt> on <tt>''x''</tt> and <tt>''y''</tt> ''in toto''
|-
|<tt>.</tt>
|
|
| To be discussed
|-
|<tt>..</tt>
|<tt>(('''f''' y) + '''f'''('''g''' ''y''))/2</tt>
|<tt>((x '''f''' y) + ('''g''' ''x'')'''f'''('''g''' ''y''))/2</tt>
|Given <tt>'''h'''←'''f'''..'''g'''</tt>, the resulting function, <tt>'''h'''</tt>, is ''even'' in the sense that <tt>('''h''' ''y'') = ('''h''' -''y'')</tt> for any <tt>''y''</tt> ; its graph is reflected in the vertical axis.
|-
|<tt>.:</tt>
|<tt>(('''f''' y) - '''f'''('''g''' ''y''))/2</tt>
|<tt>((x '''f''' y) - ('''g''' ''x'')'''f'''('''g''' ''y''))/2</tt>
|Given <tt>'''h'''←'''f'''.:'''g'''</tt>, the resulting function, <tt>'''h'''</tt>, is ''odd'' in the sense that <tt>('''h''' ''y'') = (-'''h''' -''y'')</tt> for any <tt>''y''</tt> ; its graph is reflected in the origin.
|-
|<tt>:</tt>
|<tt>'''f''' ''y''</tt>
|<tt>''x'' '''g''' ''y''</tt>
| Allows the unary and binary definitions of a function to be specified independently.
|-
|<tt>:.</tt>
|
|
| To be discussed
|-
|<tt>::</tt>
|<tt>try { '''f''' ''y'' } catch { '''g''' ''y'' } </tt>
|<tt>try { ''x'' '''f''' ''y'' } catch { ''x'' '''g''' ''y'' } </tt>
| Given <tt>'''h'''←'''f'''::'''g'''</tt>, if <tt>'''f'''</tt> returns a valid value without error, then the result of <tt>'''h'''</tt> is the result of <tt>'''f'''</tt>; else, the result of <tt>'''h'''</tt> is the result of <tt>'''g'''</tt>.  These succinct, functional exception handlers can be chained.
|-
|<tt>''hook''</tt>
|
|
| To be discussed
|-
|<tt>''fork''</tt>
|
|
| To be discussed
|-
|}


### Still to discuss

::<tt>@.</tt> = functional selection (<tt>'''f'''`'''g'''@.'''h'''</tt>)
::<tt>&.</tt> = ''under'', f&.g is (g_obverse@f)&g where g_obverse is the inverse of g if one has been defined.
::<tt>&.:</tt> = <tt>&.:</tt> is to <tt>&.</tt> as <tt>&:</tt> is to <tt>&.</tt>
::<tt>.</tt> = <tt>'''f'''.'''g'''</tt> is defined in terms of a recursive expansion by minors along the first column when unary, and as a generalized inner product when binary.
::<tt>:.</tt> = related to <tt>&.</tt> -- g :. G defines a new verb that behaves like g except that its obverse (defined inverse) is G.
::hook = an implicit composition of 2 functions -- in contexts which take only one argument it's structurally similar to the S combinator in much the way [ is similar to the K combinator and ([ [) y is y.  ((u v) y) in J is equivalent to (y u v y) in J if u and v are verbs and y is a noun.  Here, u is a combining verb (which takes a left and right argument and v gets only a single argument.  For example (* -) 3 is 3 * - 3 or negative nine.  Meanwhile x (u v) y is simply x u v y.  For example 4 (* -) 3 is negative 12.  
::fork = an implicit composition of 3 functions.  (u v w) y is (u y) v (w y) for example (! * -) 4 is (!4) * (-4) or 24 * _4 or _96.  Similarly x (u v w) y is (x u y) v (x w y) so for example 5 (* - +) 4 is (5*4) - (5+4) which is 20-9 which is 11.  Trains of verbs longer than 3 are organized by grouping the rightmost three verbs as a fork which in turn is a single verb.

==VBScript problem==
I think their should be a note added to the VBScript example stating that it takes two strings which are the names of functions which is not the same as VBScript functions, which the task description requires. With that stipulation up-front it would look OK. --[[User:Paddy3118|Paddy3118]] 05:48, 20 February 2010 (UTC)

== Wrong hint ==

The hint about using a closure is incorrect.  Take javascript:
```JavaScript
function compose(f, g) {
	var r = function(x) { return f(g(x)); }
	return r;
}
```
 returns an existing function ref with a closure containing function definition <code>f</code> and <code>g</code> at the time of invocation, while using eval and function names:
```JavaScript
function compose2(f, g) {
	var r = new Function("x", "return " + f + "(" + g + "(x))");
	//var r = eval("function(x) { return " + f + "(" + g + "(x));}");
	return r;
}
```
returns a function that calls g then f by names as passed in.  The eval mathod creates a new, full fledged function, freshly compiled, and has nothing to do with closures.  Either way, a function that's equivalent to <code>f(g(x))</code> is returned, with the difference being that if <code>f()</code> or <code>g()</code> is modified later, the first method will not change, while the eval'd result will notice and call the new definitions.  Which is more "true" of a function composition is a matter of intepretation. --[[User:Ledrug|Ledrug]] 07:25, 16 July 2011 (UTC)

:I have fixed the hint. --[[User:Rdm|Rdm]] 11:05, 16 July 2011 (UTC)

==Multiple Function Composition==
I believe that quite a few languages have succeeded in performing this task simply because composing two numeric functions with a single parameter is (probably) the easiest possible example of function composition.  In several cases, these solutions appear to violate the "spirit" of function composition.  In my opinion, the definition of a function used to composes functions should look and behave like [http://reference.wolfram.com/mathematica/ref/Composition.html Mathematica's Composition function].  Common sense would suggest that if a developer needs to write greater than 80 SLOC just to compose two functions w/ one parameter, the likelihood of someone using this language feature in practice is pretty low.  Therefore, I think we should discuss either modifying this task, or possibly creating a more difficult sub-task, to better reflect the practical use of this language feature.

As a starter, I would suggest that the compose function must be able to accept two or more functions as parameters.  I would also like to see a concrete example for what should be composed then computed.  I haven't come up with anything interesting, but I do like the following example because it uses multiple functions, a couple of different data types, and has an easily verifiable answer.

```txt
CL-USER> (compose model #'ceiling #'1+ #'sin #'square)
MODEL
CL-USER> (model pi)
1
-0.43030121700009226697L0
CL-USER> 
```
  --[[User:Lhignight|Larry Hignight]] 06:18, 14 June 2012 (UTC)

: Hi Larry, some might say that the comparison of LOC (Lines Of Code) to do this task is a good way of comparing the languages, and a new task to accept a list/array of functions, compose them in some order, then show the results of applying the composition to a number might be a good task to create. --[[User:Paddy3118|Paddy3118]] 07:04, 14 June 2012 (UTC)

: It's probably sufficient here to show that the composition of two functions is a function which can be composed like any other function? --[[User:Rdm|Rdm]] 13:11, 14 June 2012 (UTC)

:: Brilliant. No need for another task. --[[User:Paddy3118|Paddy3118]] 19:18, 14 June 2012 (UTC)

:: Agreed.  Although it is less elegant than composing multiple functions with a single call, it does demonstrate that a language supports function composition without having to resort to additional functional programming language features.  I would still prefer to see the task defined more precisely because I'm afraid we'll end up with several (sin (asin (sin x))) implementations.  I haven't come up with a good example (yet), but I think that changing the task description to include the following output should separate most of the wheat from the chaff.  

```txt
;;Example Usage:
;CL-USER> (compose f #'ceiling #'sin #'sqrt)
;F
CL-USER> (compose g #'1+ #'abs #'cos)
G
CL-USER> (compose h #'f #'g)
H
CL-USER> (values (f pi) (g pi) (h pi))
1
2.0L0
1
CL-USER> 
```
  --[[User:Lhignight|Larry Hignight]] 23:00, 20 June 2012 (UTC)
::: Not sure I agree with you about elegance -- the syntax you have in your illustration here favors composing lists of functions but that's an implementation detail, it's not universal.  For example, in J, composing f g and h looks like f@g@h and composing them as a list looks like <nowiki>(4 :'(x`:6)@(y`:6)`'''''/f`g`h)`:6</nowiki> -- I could define a '''compose''' word that encapsulates that complexity, but it would never beat f@g@h for directness.  That said, I added an implementation of your examples to the J implementation on the main page. ----[[User:Rdm|Rdm]] 23:21, 20 June 2012 (UTC)
:::: I didn't mean to suggest that the task or implementation should favor composing lists of functions.  I find implementations that allow you to compose multiple functions in a straight-forward manner [eg (compose f #'1+ #'abs #'cos), Mathematica's Compose[f1,f2,f3...] and the J example that you provided] to be more elegant than nesting multiple calls to a composition function [eg (setf f (compose #'1+ (compose #'abs #'cos)))]. --[[User:Lhignight|Larry Hignight]] 09:56, 21 June 2012 (UTC)
:  Rdm -- The example looks good.  Given your experience with J, are there more advanced examples of function composition that you think should be included in the task description?  I think it would be desirable to have easy, moderate and hard example problems.  --[[User:Lhignight|Larry Hignight]] 09:56, 21 June 2012 (UTC)
