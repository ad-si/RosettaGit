+++
title = "Category talk:JavaScript"
description = ""
date = 2011-01-02T12:32:36Z
aliases = []
[extra]
id = 7445
[taxonomies]
categories = []
tags = []
+++

== Is JavaScript functional? ==

Javascript is a functional language? –[[User:Short Circuit|Michael Mol]] 10:45, 29 May 2010 (UTC)
: It seems it is (too), according to several sources. –[[User:ShinTakezou|ShinTakezou]] 17:42, 29 May 2010 (UTC)

'''Effects System''':
 
Functional programming languages try to reduce side effects to a minimum, this usually involves immutable data structures and pure functions and then dropping down to something like Monads when side effects are needed. 

What about JavaScript? What does JavaScript do to reduce or limit side effects in your program? 

* No way of enforcing functional purity
* No way of creating immutable objects
* You can't statically type variables
* You can't even put things into namespaces

JavaScript has the weakest control over its side effects of any language I know of, there is even implied globals if you leave off the var keyword.

'''First Class Functions''' 

JavaScript has first class functions. But so does Lua, Perl, Ruby, Python as of 1994, PHP 5.3, Visual Basic 9, C# 3.0, and even C++0x. If JavaScript's support for first class functions is all it takes to make it functional you should also add Visual Basic and all of those other languages which have that too. A much more interesting category would be all of those languages which don't have first class functions, C, Java, and a few other languages don't have them.

What about parameter handling features? In C++ you can declare a parameter to be const to enforce const-correctness. In JavaScript you can't modify your parameters at all, they always come in as mutable variables. There is also no parameter types or default parameters. 

Additionally, most functional programming languages have implicit return so that you don't have to write out a return statement. Although the Mozilla-specific extension [https://developer.mozilla.org/En/New_in_JavaScript_1.8 JavaScript 1.8] does have expression closures with implicit return.

'''How is JavaScript used?'''

JavaScript puts everything into a single mutable global namespace. There is no way to export functions into your module, or other features which would make functional programming a sane choice. As such, most of the time programmers use prototypal programming and object systems like [http://code.google.com/p/joose-js/ Joose] and [http://jsclass.jcoglan.com/ JS.Class] for code re-use. As such JavaScript is mostly a prototypal programming language, or an OOP language, and not a functional programming language. 

As for Scheme, I do not think it is comparable to JavaScript. It has homoiconicity, hygienic macros, and a decent effects system (side-effect causing functions are suffixed with !). These things together make it vastly different from JavaScript. –[[User:Jhuni|Jhuni]] 0:15, 1 January 2011 (UCT)

:Static typing is nothing to do with functional programming. It's an independent feature axis. You can't say that namespaces are a feature of functional programming; they're just naming features (as is your point on Scheme's <code>!</code> suffix). Moreover, a great many languages that are functional actually support mutable state; it cannot be a feature that allows a decision to be made on whether a language is functional. The only thing that you might ''possibly'' have a point about is “functional purity” but you're beginning to sound to me like you're making a No True Scotsman argument, which is a logical fallacy. What I ''can'' agree though is that community practice with JS is not to program in a functional manner. –[[User:Dkf|Donal Fellows]] 01:17, 1 January 2011 (UTC)
:To me, the key marks of a functional programming language are that it allows functions (or references to them) as values, that it allows recursion, and that it doesn't require the use of side-effects to produce the results of a function. That is admittedly a loose definition that permits ''lots'' of languages to claim that they support it, but so what? There's also a class of strict functional programming languages that are far more restrictive (e.g., by being side-effect free) but they're much less useful; even Haskell doesn't make it to that level of purity (due to the IO Monad, a requirement for participating in an outside world that has state). –[[User:Dkf|Donal Fellows]] 01:28, 1 January 2011 (UTC)

::Lots and ''lots'' of programming languages support first class functions. If you put basically every language into the functional category it is going to become a basically meaningless word. So rather then making the word functional meaningless I set a bit of a higher standard. Functional languages should have immutable data structures and some good way to avoid side effects.
::JavaScript is one such language that lacks immutable data structures. Static typing allows you to limit what an object can mutate to. For example, with static typing you can prevent somebody from replacing a function with a number. Since JavaScript doesn't have immutability or static typing you can replace any value with any other value. To add fuel to the fire JavaScript puts everything in a global name-space so that anyone can overwrite things and cause a collision. Furthermore, all properties are late-bound. All these things compounded make JavaScript worse off in terms of side effects then any language I know of.
::Perhaps it shouldn't be a black and white thing that you are either functional or you are not. Perhaps it should be said that JavaScript is 90% prototypal 8% procedural and 2% functional or something along those lines. Prototypal programming is a community practice for a reason. The language makes it hard to get away with anything else. –[[User:Jhuni|Jhuni]] 03:41, 1 January 2011 (UTC)

::: Well, ultimately the functional paradigm's been pretty successful, though the strict functional less so (precisely because so many problems are very coupled to their state). That's an indication of a reasonable degree of success for the paradigm. (We probably ought to take this part of the discussion somewhere more general.) For my money, the fact that authorities outside of RC say that JS is functional is sufficient evidence for me to admit its description as such here. –[[User:Dkf|Donal Fellows]] 10:56, 1 January 2011 (UTC)

:::: I can confidently state that there is a significant difference between functional languages like Clean, Haskell, Erlang, and ML and scripting languages like JavaScript and PHP.
:::: However, it is like when you said "just naming features." Functional programming is the ''name'' of a  paradigm. Attach whatever meaning to it you want.
:::: On the other hand, it is undeniable that JavaScript is completely side effect ridden and therefore non-declarative.
:::: It just so happens that declarative programming is probably the most important part of ''effective'' FP. By telling the computer what to do rather then how to do it, you free up the compiler to optimize and parallelize at will. –[[User:Jhuni|Jhuni]] 01:42, 2 Janurary 2010 (UTC)
:::: Since we aren't really talking about JavaScript anymore, and I have made basically all my points about the language itself, we may as well move this to [http://rosettacode.org/wiki/Category_talk:Programming_paradigm/Functional Talk:FP]. –[[User:Jhuni|Jhuni]] 01:49, 2 Janurary 2010 (UTC)
::::: Unless a paradigm is fundamentally impossible in a programming language (perhaps it's made so by some feature of the VM), a programmer can employ that paradigm, regardless of how convoluted or tricky it might be. What all this tells me is that templates and language-page modifications are the ''wrong'' way on Rosetta Code to associate langauges with paradigms, and that a "show me the code" approach is really what's necessary. To that end, tasks would be needed in order to provoke demonstrations of aspects of a given paradigm. --[[User:Short Circuit|Michael Mol]] 02:12, 2 January 2011 (UTC)
:::::: I would like to see a "enforce immutability" task that shows what means languages have to enforce immutability on values, parameters, methods, and other things. –[[User:Jhuni|Jhuni]] 03:12, 2 January 2011 (UTC)
::::::: I see that you've been working on such a task. Cool! –[[User:Dkf|Donal Fellows]] 12:32, 2 January 2011 (UTC)
