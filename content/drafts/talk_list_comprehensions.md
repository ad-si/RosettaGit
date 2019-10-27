+++
title = "Talk:List comprehensions"
description = ""
date = 2017-09-29T20:58:55Z
aliases = []
[extra]
id = 2864
[taxonomies]
categories = []
tags = []
+++

==Pop11 example==
The Pop11 example does not use the sort of syntax specified in the requirements. --[[User:TBH|TBH]] 09:22, 23 May 2008 (MDT)

== Remove Algol? ==

As it states, it is not a list comprehension. This would allow those languages with, or that can construct, the extra '''syntax''' to not be hidden amongst multiple languages showing nested loops. --[[User:Paddy3118|Paddy3118]] 04:13, 16 April 2009 (UTC)

: Some requirements sound strange. Common lisp would be out too. E too. After all, Erlang and the second (and to me first too) Haskell example too. Python too. Clojure too, for how I can understand Clojure. This is because some languages use ''de facto'' their way of expressing (nested) "for" loops (language syntax may hide this, but it is what it happens). Maybe just Mathematica can be saved... The most correct example should say it like <math>\{(x,y,z) | x^2+y^2=z^2\}</math> as general set, and then be able to put some constraints over variables (like <math>x, y, z \in [1,n]</math>... But doing so in most languages it means to generate the values in the range, that is using a "for" loop syntax (the word ''for'' does not need to appear!). And this rules out also syntax like <tt>(for x from 1 to n)</tt>, or <tt>for x in 1..n</tt>, or <tt>A <- lists:seq(1,N)</tt>, or <tt>x <- [1..n]</tt>, or <tt>for x in xrange(1,n+1)</tt>. I would delete the first requirements, it is subtly ambiguous. --[[User:ShinTakezou|ShinTakezou]] 21:10, 16 April 2009 (UTC)

Hi ShinTakezou, I think what truly makes it a list comprehension, is that the language designers have created ''another'' syntax, separate from for-loops, aping the main syntactic components of the set builder notation (the [[wp:List_comprehension|/List comprehension]] article overview tries to show this. If the language designer doesn't give attempt to give a list comprehension in its language then although for-loops would work, what is the point? Python and Haskel do give separate syntax and although the Python list comprehension does include the 'for' keyword, it is separate from a for statement in the syntax of the language. --[[User:Paddy3118|Paddy3118]] 16:53, 19 April 2009 (UTC)

: I stumble always upon this syntactical stones and interpretational mass. C too can have list comprehension with a proper (set of) preprocessor macros...? Would it be cheating...? Yes and no... aren't languages designed someway so that syntactical sugar can be added? And shouldn't we on RC show how easy it could be even if not already given that syntax by design, and compare with langs having the syntax already hard-wired? (Of course, if it won't waste ten pages to do so...!)
: Anyway this is related to how a languages express loops... Nothing to say about Python, ... I've (almost) discontinued my Haskell studies, but on RC itself I can see Haskell '''has no''' for-loops as other languages... of course it can be "simulated"... (some examples use a forM or forM_, but at [[Loop Structures]] it is shown how that can be implemented... and likely as it is indeed implemented in [http://haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html Control.Monad], in fact <cite>forM is mapM with its arguments flipped</cite> and implementation of mapM is [http://haskell.org/ghc/docs/latest/html/libraries/base/src/Control-Monad.html#mapM here]). Id est Haskell has no real for-loop and the most obvious way of "looping" over values is to use a "list". So, there's no a separated syntax for for-loops ''and'' list (comprehension)... and so the first constraint says us Haskell can't have list comprehension... --[[User:ShinTakezou|ShinTakezou]] 18:17, 19 April 2009 (UTC)

Hi again ShinTakezou, things are never likely to be clear cut, but my thinking on languages is that if list comprehensions are not part of 'standard practice'/in the reference for that language,  then even if a computation can be done to mimic list comprehensions, you might never see such list comprehension work-alike in use in example code of that language from books or their website. It would be the problem that I would have with languages that have macro facilities such as C and maybe Lisp, where some'''one''' creates a particular macro definition for a list comprehension and says "look, this language can do it"! How likely is some other programmer to use that implementation when they want to use a list comprehension in that language? How likely are they to be taught that "this is how you do list comprehensions" in the language. In Python, Haskel, and Javascript it is clear-cut, go seeking list comprehensions in those languages and you will be taught the one thing and they will say that it is their languages advised way of supporting set-builder type notation for building lists/iterables. I guess it boils down to "It is a Lisp comprehension because the language designers say it is, and because they had set builder notation in-mind when they were creating the syntax". --[[User:Paddy3118|Paddy3118]] 04:47, 20 April 2009 (UTC)

: Hm. How likely is some other programmer to use that impl when they want to use a list comprehension? It's very likely, in two ways. First, they can see my own implementation, and use it. Second, if the approach is straightforward, they can reimplement the wheel with their hands, but after all it would be the same to mine, or very close to it... More than someone will use my own implementation (or similar) if it is good enough and if it becomes widely known... Doesn't it happen for everything beyond the very basic of a lang? I need "associative arrays" in C? I searched for them, and there are a lot of libs... the probability a random programmer uses A instead of B?... In one or two example I've used Judy library... In LZW I've implemented an "associative array" (rudimental but working)... Very likely an expert C programmer feels better with nested for-loops than with list comprehension, so s/he won't never implement a list comprehension at all to reach the aim. But here we're dealing with list comprehension, that according to several "sources" are [http://haskell.org/haskellwiki/List_comprehension syntactic] [http://c2.com/cgi/wiki?ListComprehension sugar]; and C macro processor is the guy creating a lot of sugar for C...

: Now, I insist that the first constraint is subtly ambiguous and subject to wandering interpretations (like the joke one I've already said: it seems a paradox, but according to that constraint Haskell has not list comprehension... at least, not the one we can use for this task!... since you express "for-loops" using the same syntax of list comprehension)... and after all it is not needed; the point is / should be: can this/that language provide ''that'' syntactic sugar "easily" enough? Sometimes it is just a challenge (like pretending to give some functional ability to C in few lines of code...), but ... why not? Common Lisp explicitly do so, but Lisp handles lists naturally... Anyway, I can't see too much difference between Algol and Python, E, Clojure, Erlang, Haskell...; better said, I can see only syntactic differences. --[[User:ShinTakezou|ShinTakezou]] 00:07, 21 April 2009 (UTC)

== Appropriate task requirements? ==

Is it really appropriate to make language-specific syntax a requirement?  When the goal is for a program to be aware of lists and its position in them, would it not be appropriate for glue or translation code to fill in the requirement?  For example, consider a language identical to C++, but without the STL.  The STL wasn't originally part of the C++ standard, but was adopted later on.  Is it appropriate that pre-standardized-STL C++ be excluded from this task, as opposed to C++ once the STL became part of it?

Put another way, is the aim of this task to demonstrate how to accomplish a similar end in multiple languages, or is it to filter out languages for which a particular means is not baked into the language's syntax and/or standard?  If the latter, why don't we have a task to accomplish the former?  And once we were to have the former, why would we need this task, as opposed to identifying the code examples in that task which allow for the more stringent requirements? --[[User:Short Circuit|Short Circuit]] 16:23, 20 April 2009 (UTC)

:We have some tasks that are specific to other sections of programming languages - those with explicit pointers, or those with OO features for example. Functional programming languages such as Haskel, ML and OCaml are, in some ways, the new kids on the block and I think it is right to have some tasks expressed from a functional viewpoint as well. The task is to show how some languages explicitly borrow from set-builder notation to form what is called a list comprehension in many of those languages and to show the similarities between the languages syntax and that of set-builder syntax. It is like a task to show how to create a class; some languages might be able to do the same thing with a structure and maybe pointers to functions, but the writer of the example should state that the language designers did not have the idea of a class in mind when they created structures and function pointers. Similarly in a task about goto statements, if asked to use a goto to jump to the end of a function, I would not think it correct to raise an exception that is caught just before the function returns. You could state that it works the same, but it would not be using a goto statement. (P.S. I like the debating on RC, it's civilised) --[[User:Paddy3118|Paddy3118]] 21:07, 20 April 2009 (UTC)  :-)
:: So what if we were to have a template for identifying when key language paradigms are used in a language?  Would that obviate a significant amount of the need for tasks dedicated to those paradigms?  I don't have time to make all of my points.  I'm supposed to be working... --[[User:Short Circuit|Short Circuit]] 03:24, 21 April 2009 (UTC)

:: I've noticed that, and the Omit From exists for this reason. Some tasks can be done more easily in different programming paradigms... but this does not rule out automatically languages that can't use that paradigm, unless the point of the task is to show where the power of that paradigm is... '''and''' it wouldn't be too long to give that "functionality" to the lang; I've often thought about "cheating" for such language or paradigm-specific tasks... to me this is not wrong, since, if succeeded, you're surely teaching something about the flexibility of a language, while reaching the aim of the task (unless it is prohibited to use "tricks"... but sometimes it is not reasonable to put such a constraint).

:: Further reasoning about the Rosetta part and goto example: if a programmer is able to show how to use exception to simulate a goto which is not part of the lang of choice, ... why not? I believe sometimes people can get interested in such a creative use of the language... If the task is properly written (e.g. show how to hijack execution of code inconditionally from point A to point B...; explanation: think about goto in most well-known languages like BASIC), exception-based-languages' lovers can put their efforts in simulating a goto. The only limit (among the one explicitly given by the task) could be: how easy would it be? E.g. giving a full set of OO properties to C would take too long (lines of code), even though possible, and so to show that in a single page of RC could not be so pleasant for a reader. Moreover, it would be too long for all non OO languages; so sometimes, depending on the task and how it is written, it is natural to omit a language even though of course it could have the ability to achieve the task someway. '''But''', if there are few lines that are task-compliant in the final intention (reaching the ''result''), ... why not? Just to say, the presence of a "complex" syntactic sugar in C, achieved by intense (but short enough) use of macros and code, wouldn't ruin other "real" ''list comprehensive'' languages... the opposite: it would stress the strength of that syntax and paradigm for such a task. Back to Algol: it's short, and with my knowledges I can't see where it really differ from E, Clojure, Common Lisp, Erlang, Haskell, Python... but in syntax. --[[User:ShinTakezou|ShinTakezou]] 00:42, 21 April 2009 (UTC)

Describing some''thing'' and setting an example task is tough! it can be very difficult to give a task that is precise enough to only allow solution by ''thing'', and comprehensive enough to demonstrate all the features of ''thing''. I know I am incomplete in the RC description; and even the longer WP description has flaws; But what to do? If you see lots of solutions clearly showing how to complete the task, but make no effort to explain that it is the languages way of supporting ''thing'' then you could try and explain thing better, and/or refine the example task, and/or try and have example solutions removed that stray too much from the ''thing''. Welcome to my dilemma! --[[User:Paddy3118|Paddy3118]] 07:28, 21 April 2009 (UTC)

: My first solution is: do not remove Algol; and let's take time to think about it more deeply. My second more opinionable solution is to remove the first constraint (distinction of syntax between for loops and list comphrension): after all there exist languages that use a "list" syntax to loop, and it is the same they use to express list comprehension (Haskell ''docet''). Maybe a more relaxed constraint can be put, but I have no good ideas (yet). Let's take time to think about it too (I believe there's no need to hurry) --[[User:ShinTakezou|ShinTakezou]] 14:20, 21 April 2009 (UTC)

I suggest that this is not appropriate for a '''programming task''', which is meant to show how to ''solve'' a ''problem'' (according to the box at the top) perhaps using a particular type of data structure or algorithm, rather than display some specific syntactic sugar.  If this sort of page is to be kept, it should be in a separate page category - eg ''Syntatic Feature''. --[[User:TobyK|TobyK]] 23:54, 9 August 2009 (UTC)

==J and set builder notation==
Could the J solution add a comment as to how the syntax follows set builder notation (or not)? It is a big part of being a list comprehension , and so it would be good if language examples would point out were they don't follow set builder notation (see the WP page); or point out where the similarities are - if it is hard to see. thanks. --[[User:Paddy3118|Paddy3118]] 19:51, 28 August 2009 (UTC)

:I have attempted to make this clearer.  --[[User:Rdm|Rdm]] 15:19, 29 September 2010 (UTC)

==Is it time to remove some examples?==
There are several that make no attempt at following set-builder notation. (Maybe turn them into an omit from) --[[User:Paddy3118|Paddy3118]] 16:30, 16 November 2009 (UTC)
:That seems OK. "List comprehension" is really about the shorthand syntax, so examples that need explicit loops for this should removed. The omit option is best I think. Just make sure you keep examples that use set-builder-like language instead of notation. --[[User:Mwn3d|Mwn3d]] 16:36, 16 November 2009 (UTC)

== Ruby notes ==

Before I edited the page, the Ruby code used Enumerable#collect (also known as Enumerable#map) and Array#compact.


```ruby
# no temp array, but a lot of housework to flatten and remove nils
(1..n).collect {|x| (1..n).collect {|y| (1..n) \
  .collect {|z| [x,y,z] if x**2 + y**2 == z**2}}} \
  .reduce(:+).reduce(:+).compact
```


I changed this to a mess involving Enumerable#flat_map and Enumerable#select, but have now simplified the code by using Array#keep_if.


```ruby
r = ((1..n).flat_map { |x|
       (x..n).flat_map { |y|
         (y..n).flat_map { |z|
           [[x, y, z]].keep_if { x * x + y * y == z * z }}}})
```


The above code is now on the page. Contrast the below code, which uses Range#each (like 'for' loops in other languages) and Array#<< to append to an array.


```ruby
r = []  # start with an empty array
(1..n).each { |x|
  (x..n).each { |y|
    (y..n).each { |z|
      r << [x, y, z] if x * x + y * y == z * z }}}
```


--[[User:Kernigh|Kernigh]] 04:38, 4 February 2011 (UTC)

== Minor correction in Prolog code ==

I'm not a Prolog pro, yet I'm curious why define my_bind predicate, if it does the same thing as member predicate?

== What can we learn, in retrospect, from a defective Rosetta 'task' ? ==

All human works fall somewhere in between complete success and complete catastrophe, and nearly 8 years later, after much productive work, but also, frankly, after too many skirmishes, objections and actual or attempted deletions, it's all too clear that there was something about this 'task' which, from the beginning, didn't quite ring true for everybody, and which has continued to prove divisive, and less than entirely satisfactory, even in recent weeks.

What can we learn from this, with the benefit of hindsight ? What additional work on the framing of this question, back in 2007, might have yielded more value and less friction in the years that followed ? 

Let's look at it in relation to Rosetta's 3 goals, which are captured well, and formulated clearly, on the landing page:

:# "The idea is to present solutions to the same task in as many different languages as possible,
:# to demonstrate how languages are similar and different, and
:# to aid a person with a grounding in one approach to a problem in learning another."

'''As many different languages as possible'''

:'''Problem''' – An immediate failure in this axis. Many languages were effectively excluded by sacrificing Rosetta's principle of [[Rosetta_Code:Add_a_Task#Task_focus|Task focus]] to a preoccupation with a particular notation. This was immediately noticed and commented on at the the time, and was later argued to authorise moves to entirely delete notionally or notationally 'ineligible' entries for particular languages.

:'''Solution''' – In retrospect, more work should have gone into thinking beyond the surface notation, and clarifying the underlying class of problem. Entries could have been harvested from ''as many different languages as possible'' by framing the task in terms of efficiently defining and populating complex sets. 

'''to demonstrate how languages are similar and different'''

:'''Problem''' – Not only was the number of eligible languages diminished by the slippage from task toward surface notation, but the scope for contrastive insight was also reduced. What is interesting about the definition and population of sets is that it can be done in various ways – for example with a monadic 'do' notation, with iteration, with structured recursion, etc. Ironically, the list comprehension notation, which is never part of core syntax, actually happens to conceal the variation in what is really going on in different languages. Python's list comprehensions are implemented, under the hood, as for loops (see http://morepypy.blogspot.co.uk/2008/06/list-comprehension-implementation.html) while Haskell's are syntactic sugar for a 'do' notation which in turn desugars down to lambda applications (see http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html).

:'''Solution''' - Richer insight into the differences and similarities between languages could have been yielded by explicitly inviting demonstration of ''differing'' approaches to defining and generating sets. List comprehensions could have appeared as notationally elegant solutions, compared and contrasted with other solutions.

'''to aid a person with a grounding in one approach to a problem in learning another'''

:'''Problem''' – Reduced language coverage and reduced scope for insight converged here to offer no help at all to learners of some languages, and reduced insight for those whose languages were covered.

:'''Solution''' – Understandable distraction away from the underlying concepts (and core editorial values) by what was, at the time, a novel notation, could have been mitigated by a discipline of stepping through Rosetta's 3 core goals, using them both as a kind of check-list, and as a stimulus to raising the game, and reflecting a little harder both on what the underlying issues really were, what list comprehensions really were (useful syntactic sugar for deeper processes in the core syntax of particular languages) and on what it would take to reach the largest number of languages, and the deepest and most useful levels of contrast, comparison and insight.

The Rosetta stone was useful precisely because the focus was on meaning and task, without distraction by notation.

If Ptolemy the V had said "use all the languages of the realm, but ONLY if they are written in hieroglyphics" then the stone would simply have been another lump of granodiorite, with much less lasting value.
