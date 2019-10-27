+++
title = "Talk:First-class functions"
description = ""
date = 2019-02-17T23:54:56Z
aliases = []
[extra]
id = 3850
[taxonomies]
categories = []
tags = []
+++

==Even C could?==
::Hi, please read the Wikipedia entry linked to, especially [[wp:First-class_function#Availability]] which specifically rules out C. I tried to make that point with my points about eval etc. --[[User:Paddy3118|Paddy3118]] 18:15, 24 February 2009 (UTC)
:::Maybe I am dumb or my english's weak, but it seems to me that [[wp:First-class_function#Availability]] does not rule out C, it only says that it is ''impractical'' (hard? slow? hardware-dependent?) to make the whole thing working in plain C... Anyway don't worry, my doubts were not about the way the task is specified, but about the way the task can be accomplished even in a language like C! :D And I am also very blind about too much abstraction and disagree with ''it is not possible to create new functions at runtime'' from [[wp:First-class_object]]; a stupid proof: I can write a python interpreter in C... (it all depends on what ''definitions'' make possible) --[[User:ShinTakezou|ShinTakezou]] 21:33, 24 February 2009 (UTC)

::[[wp:First-class_object]] makes good reading too. --[[User:Paddy3118|Paddy3118]] 18:44, 24 February 2009 (UTC)

* Functions can be stored in other collection types of the language... yes (pointers)...
* Functions can be used as arguments to other functions... yes (pointers)
* Functions can be returned as the value of functions... yes (pointers)

* New functions can be created from others at run time... maybe?

The last one is unclear to me. I can guess what it means, but every language (like C) can build a runtime that allows ''functions to be created from others '''at run time'''''; you can build a library allowing that... meaning that with such a library C becomes a language with first-class functions...

I bet classifiers say that C functions are not first class... And if you can write such code, this does not mean that the ''language'' has first-class functions... so all these theoretical classifications regard only the ''primitive'' syntax (or better semantics?) of a language...? Or could I try to ''cheat''? (Like I did for one-liner...!) --[[User:ShinTakezou|ShinTakezou]] 16:39, 24 February 2009 (UTC)

Yes, it's that last point that is crucial. In standard portable C, you can get a pointer to any function that was written in the original source code (of your program or a library), but there's no way to construct an arbitrary number of new functions.

: Sure it is.  All you need is a compiler and support for dynamic linking.  Well, that and some way of generating the text of the functions you are creating.  --[[User:Rdm|Rdm]] 11:17, 3 May 2011 (UTC)

You could simulate it by having a table and a set of functions referring to entries in it; but you would have to deal with allocating this limited resource. For example:


```c
typedef int (*ourfunc)(int);

ourfunc[4][2] composeData;
int freeRow = 0;

int compose_0(int x) { return composeData[0][0](composeData[0][1](x)); }
int compose_1(int x) { return composeData[1][0](composeData[1][1](x)); }
int compose_2(int x) { return composeData[2][0](composeData[2][1](x)); }
int compose_3(int x) { return composeData[3][0](composeData[3][1](x)); }
ourfunc functions[4] = {compose_0, compose_1, compose_2, compose_3};

ourfunc compose(ourfunc a, ourfunc b) {
  composeData[freeRow][0] = a;
  composeData[freeRow][1] = b;
  return functions[freeRow++];
}
```


But this technique will only work for a fixed number of functions -- it is basically emulating closures by having a predefined table of closure entry points and data storage.
--[[User:Kevin Reid|Kevin Reid]] 17:54, 24 February 2009 (UTC)
:: My bet was that writing codes, one can "simulate" it without the limit of fixed number of functions... but I am still thinking about it... --[[User:ShinTakezou|ShinTakezou]] 21:33, 24 February 2009 (UTC)

:No, pointer to a function is only a first-class pointer. Its "first-classness" alone does not make first-class the thing it points to. The rest of your argumentation is about Turing completeness. Yes, you can create an object that will act similar to a function. A pointer to function is a simplest example of. But this would not make that object a function in language terms.

:Some notes to the list. The list rather refers to operations on functional types. Neither of is actually required to make functions first-class. What is required is that a function were a type and there existed objects of this type, with '''some''' operations defined on this type. Whether these operations include any concrete operation beyond "call me," is up to the language. Only "call me" is essential (that makes the object a function). Granted, almost certainly a reasonable implementation will provide operations from the list. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:57, 24 February 2009 (UTC)
::: Chance there are that many languages at the end manipulate pointers to code; but this is hidden. In C, it is not so hidden, but I can feel it like a syntax oddness: a function ''is'' ''de facto'' a pointer to a function ---or the first-class implies also an uniform syntax to manipulate/use/refer to objects? If it is so, Python (and many more, and likely all!) could be out because e.g. <tt>sin</tt> means ''the function sin'', but to "use" it I need the syntax <tt>sin(x)</tt>... Compilers/interpreters of languages that have first-class functions, must implement it someway, e.g. having something like a hash-table that "links" between names and pointers to ''object'' (which could be directly code or more complex data structure, depending on the context the language need to make its thing works)... So again the question: first-class functions is not about what you can do with the language, but about the syntax and what is ''already'' implemented? --[[User:ShinTakezou|ShinTakezou]] 21:33, 24 February 2009 (UTC)

::::It is about the syntax (i.e. the language), not about what is going on under the cover. To be first-class is a language property. So it obviously cannot be changed by means of writing a program. You must modify the language in order to make first-class things that are initially non-first-class. Regarding C++, I think it is quite close to have first-class functions because of 
```cpp
operator ()
```
That does not give you [[closure]] or composition of functions, but as I said before that is unimportant. A first-class static functional object is still first-class object. We could compare it with a language that would not have no integers, only integer constants. In that language integer would be first-class, because you could have a constant '''integer object'''. Lacking integer assignment is nasty, but irrelevant. A bigger problem with C++ is that not all kinds of its functions can be implemented this way. For example, methods cannot be, only free functions. IMO, this qualifies C++ functions as non-first-class, not that they are static, not nested etc.  --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:42, 25 February 2009 (UTC)

::Thanks Dmitry. That is a valid point. I guess if a language has problems with the example but can reason, (making a better case than C), why it has first class functions, then they should make their case too. Maybe they could cover the four points by other means. --[[User:Paddy3118|Paddy3118]] 18:25, 24 February 2009 (UTC)
::: I will try to think about how it can be done... but of course I would stop before implementing a full too-complex too-long runtime or a VM :D --[[User:ShinTakezou|ShinTakezou]] 21:33, 24 February 2009 (UTC)

::::I could not get away from the thought, when reading about First Class Functions, that the definition was a little hazy, and the only certainty was that if you had to do as much as you would have to in C, then you were doing too much to belong to the set of languages that could claim to support FCF. --[[User:Paddy3118|Paddy3118]] 04:53, 25 February 2009 (UTC)

:::::What a pity. "Syntax" constraints set by Dmitry-kazakov of course rule out C seriously this time (using preprocessor to extend syntax someway? :D too much effort anyway). The classification anyway is not clear. Dmitry says that it does not matter what happens under the "cover", and this makes it clearer, but it is also an interpretation of the definition (as stated). I.e. the definition explicitly prohibits the call for eval/exec, but it is obvious that a (belated) eval-uation is hidden behind the scene, e.g. the Python lambda could be "simulated" storing the piece of code into a string, specifying the dummy symbol, which will be changed to the real one when the "string" will be used, i.e. evaluated, ''against'' an argument... (I suppose internally it is done through local binding of the dummy symbol to the some thing which is bound to the argument, or something like that) ... --[[User:ShinTakezou|ShinTakezou]] 17:39, 25 February 2009 (UTC)

== Closures? ==

Maybe this task is actually requiring [[wp:Closure (computer science)|closures]]? The concepts of first-class functions and closures are very inter-related and often confused. The ability to make a "compose" function requires closures, because the returned function needs to be able to remember ("close" around) the input functions when it is created so that it can use them to compute the correct result. When you talk about "creating" functions at run time, what you care about is the ability for it to close around run-time variables in scope at that time. Because if you couldn't do that, you would have what is morally equivalent to C function pointers, because they would be statically known at compile-time, and you could just "lift" them out of the functions that created them. --[[User:Spoon!|Spoon!]] 20:25, 24 February 2009 (UTC)

==Don't merge with functional composition==
functional composition is only part of the task. The idea is to show how well a language supports first-class functions with a task that needs more of the traits that the WP article says characterizes first class functions. --[[User:Paddy3118|Paddy3118]] 10:03, 28 March 2009 (UTC)

==Autohotkey problem==
The Autohotkey entry is unfinished. Does what is there actually help? I don't know the language, so may be wrong about this second sentence. --[[User:Paddy3118|Paddy3118]] 06:48, 4 June 2009 (UTC)
: Please Clarify.  What part is unfinished? Did you try running the example? Help what?  --[[User:Tinku99|Tinku99]] 21:02, 4 June 2009 (UTC)
Hi Tinku, Autohotkey doesn't seem to create two lists: one of functions, the other of their matching inverses  like the Javascript, Perl, Python, Ruby,... examples do? --[[User:Paddy3118|Paddy3118]] 21:22, 4 June 2009 (UTC)
:: ok, thanks.  it does now. --[[User:Tinku99|Tinku99]] 17:49, 5 June 2009 (UTC)
So Autohotkey is parsing a string representation of the functions and eval-ing the result? If so then it gets the answer, but like C, wouldn't 'normally' be considered as having first class functions. --[[User:Paddy3118|Paddy3118]] 18:10, 5 June 2009 (UTC)
:: AutoHotkey doesn't really support eval natively either. I just made a generic "compose" function, called map. It is overloaded to create a composition, and then funcall those compositions. AutoHotkey internal data structures include functions are documented in [http://www.autohotkey.com/forum/topic26300.html LowLevel] and can be created and manipulated as any other data.  I think to do this in C, you just have to manipulate its internal data structures, which is machine code.  See [[wp:Self-modifying_code]], Dynamic Machine Code Generation in [http://code.google.com/apis/v8/design.html v8 javascript] and AutoHotkey machine code functions [http://www.autohotkey.com/forum/topic21172.html script] --[[User:Tinku99|Tinku99]] 18:48, 5 June 2009 (UTC)
==Task Name==
I think the name of this task should be modified to: 

'''Apply the compose of a function and its inverse to a list'''

or
 
'''operate on a function''': change its name, parameter count or something, to demonstrate you can dissect it like any first class data object.
Namely, apply the concept of [[wp:Duck_typing]] to a function object.


Currently, its more like, "prove that this language has this property..."
 which is not as constructive IMO.  Lets change it to more like: 
 "Do this!"--[[User:Tinku99|Tinku99]] 19:13, 5 June 2009 (UTC)

:The task was written to introduce the concept of a language having first class functions and then to write a task that would show if the language had first class functions. Changing a functions name or parameter count are not normally taken as signs of functions being first class in a language. I took 'classic' traits then tried to create a task that would need them, but it is the idea of treating functions as any other datatype that I wanted to convey.
:Most languages would allow you to take integers from two lists in pairs and apply the addition function on them. The task asks you to put, and then take functions from lists in a similar way and apply the compose function between them. 
Duck typing is a separate topic and should not be confused with this.  --[[User:Paddy3118|Paddy3118]] 23:10, 5 June 2009 (UTC)

==Ursala example has no compose function==
The Ursala example seems to have missed out the creation of a compose function in its example.--[[User:Paddy3118|Paddy3118]] 19:17, 24 June 2009 (UTC)
:"make that a list of functions by composing each pair". Maybe this craziness does it: <tt>(+)*p\</tt>. That snippet comes after the gang command. --[[User:Mwn3d|Mwn3d]] 19:19, 24 June 2009 (UTC)
:Nope, check out [[Functional Composition#Ursala]]. --[[User:Mwn3d|Mwn3d]] 19:41, 24 June 2009 (UTC)

I've added some commentary on functional composition. The <code>+</code> operator used here is the idiomatic way of expressing composition, whereas the definition in [[Functional Composition#Ursala]] is written the way it is to comply with that task specification. The cube function is user-created and the cube root function is a library function, since this task specification says to use one of each. What's the problem? -- [[User:Sluggo|Sluggo]]
: ''"What's the problem?"'' ... My lack of knowledge of Ursala :-)
--[[User:Paddy3118|Paddy3118]] 01:26, 7 August 2010 (UTC)

== just changed the Javascript example ==

Hello, I changed the Javascript example (http://rosettacode.org/mw/index.php?title=First-class_functions&diff=97198&oldid=97008 ) because I think the old one is totally bogus - no semicolons, no "function" keyword, local var "fn" unintentionally overrides the "fn" array; doesn't work anywhere I tried (Firefox and jsdb). Is this perhaps some dialect I don't know of? UPD hmmm, just found out that jsdb uses the same Javascript engine as Firefox, namely SpiderMonkey... but anyway, my point stands:)[[Special:Contributions/92.226.26.38|92.226.26.38]] 16:28, 12 December 2010 (UTC)

:Looking at the older version:  it needed the keyword 'function' in front of the definitions for compose and test.  Also, 'print' did not do anything useful there, and a different mechanism should have been used.  And, you are right that the scope rules conflict with the two instances of 'fn'.  However, semicolons are not necessary in javascript since the language requires that line-ends be allowed to terminate statements, so that part was ok.  --[[User:Rdm|Rdm]] 19:16, 11 December 2010 (UTC)
::what mechanism? as to line ends, I didn't know. though, anyway, if the author intended to use them instead of semicolons, they wouldn't have written the first three lines the way they did (there would have to be line breaks after each "return whatever;" at least)  [[Special:Contributions/92.226.26.38|92.226.26.38]] 16:45, 12 December 2010 (UTC) 
::I'll keep them, I think it's much cleaner this way. And it seems omittting semicolons [http://codeidol.com/javascript/learning-javascript/Operators-and-Statements/Format-of-a-JavaScript-Statement/ isn't] practical except for but the simplest statements (like "x=3")[[Special:Contributions/92.226.26.38|92.226.26.38]] 16:52, 12 December 2010 (UTC)
:::By "mechanism" I meant display mechanism (I was talking about the use of 'print' -- that command apparently tries to send the current page to the printer).  And I am not happy with the blanket assertion that omitting semicolons causes problems -- flat assertions, by themselves are not very useful.  At the very least, that page should have included a reference to a deeper discussion of the issue (with examples).  Personally, I have been using a javascript compressor that uses newlines to separate statements, and it is very rare that I have encountered a file which has problems (and there, since I never isolated the problem, I do not know if that was because of a problem with line termination or a bug in the compressor).  Meanwhile, compressed javascript with line-end line separators is much easier to debug (when you get bugs in production) than compressed javascript with semicolon line separators. --[[User:Rdm|Rdm]] 00:33, 13 December 2010 (UTC)

==Do we have permission for the Prolog?==
I got a 404 error on the link, and I am presuming that it is not the original work of the poster, TrapD. 
--[[User:Paddy3118|Paddy3118]] 07:37, 3 May 2011 (UTC)
: I searched then fixed the link. It now seems OK as I presume the code on RC belongs to TrapD who used the linked-to library - which is not on RC. --[[User:Paddy3118|Paddy3118]] 07:48, 3 May 2011 (UTC)

I have permission to use the module lambda.pl from U. Neumerkel (see discussion for Y Combinator).
For the link I fix it.--[[User:Trap D|Trap D]] 10:00, 3 May 2011

== The C hack: pointer ==

Normally I don't mind small semantics, but for the C hack code, naming it "pointer" is unnecessary at best, and possibly misleading.  The reasons:
# In C, a function is just a block of machine instruction.  When calling it, you take the address and push it onto stack; a function pointer is... the address of that block of instructions, which when called gets pushed onto the stack.  There isn't much of a distinction here.
# The hack code doesn't return existing function pointers.  The function body, a blob of machine code, really gets duplicated and stored somewhere, with some key stack variables modified according to the need, and that's why it's nasty and unportable.  Calling the hack "function pointer" does not help understanding the situation. --[[User:Ledrug|Ledrug]] 05:25, 15 July 2011 (UTC)

:In many discussions on first-class functions the line seems to be drawn to ''exclude'' the [[wp:Talk:First-class function|C language]] capabilities as being too "low-level". Other languages gain merit for hiding the underlying mechanisms a bit more. I have tried to highlight what makes a first-class function in many minds is that functions are used as naturally as other types in the language by also creating the task [[First-class functions/Use numbers analogously]], but I realise that it is only an attempt. (Someone could use a numbers in a convoluted way just to match what ''needs'' to to be done for functions for example). --[[User:Paddy3118|Paddy3118]] 06:02, 15 July 2011 (UTC)

:: Of course functions are not first class in C, hence the code being not portable, I'm not disputing that.  But what the code does isn't what one'd normally call a "function pointer" either, that's what the other C code did.  If you cared enough to correct the heading, might as well use a ''correct'' heading.  "Non-portable function body duplication" or some such would be much more accurate.  --[[User:Ledrug|Ledrug]] 06:57, 15 July 2011 (UTC)

::: I've changed it. --[[User:Paddy3118|Paddy3118]] 09:03, 15 July 2011 (UTC)
:::: Thanks. --[[User:Ledrug|Ledrug]] 00:08, 16 July 2011 (UTC)
:::::With the widespread availability of [https://en.wikipedia.org/wiki/W%5EX W^X], this dirty hack is not going to work on (m)any platform. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 23:54, 17 February 2019 (UTC)

==&lt;math&gt; and &lt;big&gt; tags in REXX contribution still need attention==

: A combination of &lt;math&gt; and &lt;big&gt; tags in the Rexx contribution, formerly causing invisibility in most browsers, is still triggering a MediaWiki processing error and a syntactically ill-formed piece of HTML at one point, though the removal of superfluous space from &lt;math&gt; tag contents has now restored basic visibility.
: Please test in a majority-type browser (one like Chrome, Safari, IE/Edge which displays a graphic file for formulae, rather than the minority type like Firefox, which uses MathML processing when requisitte fonts are installed), and tidy up the formatting tags immediately after the phrase '''"Tangent begins its period at"'''. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:57, 24 September 2016 (UTC)

:: Repaired today [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:17, 13 November 2016 (UTC)
