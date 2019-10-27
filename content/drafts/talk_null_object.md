+++
title = "Talk:Null object"
description = ""
date = 2019-02-15T19:57:43Z
aliases = []
[extra]
id = 2970
[taxonomies]
categories = []
tags = []
+++

== '''C'''   and   '''C+'''   languages ==
The C anc C++ examples don't really fit the task description (and it's not possible to do so, because C and C++ don't know a generic null object). What you can have is a null ''pointer'', which is a pointer which doesn't point to any object (and that is indeed what <code>NULL</code> is for). The closest to a null object in C++ would be an empty <code>boost::optional</code> from the boost library.

Note that comparison to NULL ''may'' compile for non-pointer objects, but will in general not have the desired semantics: If NULL is defined as 0 (possible in C, more or less required in C++), then comparing any numeric object (or any other object with conversion from or to int) will actually compare the ''value'' of the object with zero.

Strictly speaking the Java example is incorrect as well (it doesn't test for a null ''object'', but for a null ''reference''), but since in Java objects are always accessed trough references, this distinction is probably mostly academic (note, however, that built-in fundamental types are ''not'' objects in the Java sense, and the null test isn't possible for them).

I don't know Ada, but I suspect the Ada example to be incorrect as well. --[[User:Ce|Ce]] 12:53, 5 August 2008 (UTC)

I think that insofar as this is a problem, we should fix it by changing the task, not the examples. The basic idea of null is similar enough for this collection to be informative, whether the language has everything-is-a-reference-including-null (Python), maybe-null references and primitives (Java, C), type systems which can include or exclude null (Common Lisp, E, C#), or data structures which explicitly add a case (Haskell) --[[User:Kevin Reid|Kevin Reid]] 22:42, 5 August 2008 (UTC)

:I agree with Kevin, but I'm not sure how it should be reworded to make everyone happy without changing the examples. Any suggestions? --[[User:Mwn3d|Mwn3d]] 00:06, 6 August 2008 (UTC)

::I also agree. The problem with [[Ada]]'s null and [[C++]]'s 0/NULL is that neither is ''the'' object. They are values of a reference type. I cannot tell what was the intention of the task, thinkable interpretations:
::* An [ideal/imaginary/exceptional] object substitutable for anything. "Null object" sounds like an instance of the root type, the common ancestor of all types, provided that the language has such a type. In which sense it were "null"? That is unclear because even the root type usually has some methods (like sizeof, ==, etc). An object that '''has''' methods, which incidentally propagate exceptions when called, does not qualify as "null", does it? Otherwise, to have a "null object" would be enough to create a type with one method that always raise some exception.
::* Like above, but a value. null is such thing for reference types.
::* Unbound name. That could be a forward declaration, a deferred constant, external object (to be linked to) etc, which is left unbound until program crashes attempting to access the corresponding object (= a language design fault). Not very promising.
::* Anything else?
:: --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:05, 6 August 2008 (UTC)

== Recent change ==

This task used to be about definedness -- which is a property of references.  A recent change has stated that it's asking about a property of "null-like values" instead.  That (the shift of emphasis from references to values) can be a significant change in scope, depending on the language.  And, at the moment, the task actually makes both kinds of scope claims -- one part still says it's about objects (which implies references, since we have the ability to modify objects without the objects losing their identity).  --[[User:Rdm|Rdm]] 21:38, 10 November 2011 (UTC)

:Actually, the task has always been about "null". In fact, the original name of the task was [[Null]]. Most of the solutions were added during this time. It was renamed to "Undefined values/Check if a variable is defined" in a collective renaming which caused a ton of confusion. The task description has always talked about "null". --[[Special:Contributions/208.80.119.67|208.80.119.67]] 22:21, 10 November 2011 (UTC)

::For a long time, the task included [http://rosettacode.org/mw/index.php?title=Null_object&oldid=125487 this sentence]: ''Note: the title of this article does not appropriately represent the task. The task is about "null"-like values in various languages, which may or may not be related to the defined-ness of variables in your language.'' This sentence implied that the title, "Undefined values/Check if a variable is defined", was wrong. I renamed the page to "Null object", a phrase from the task description. This helps languages where the [[null object]] is a ''defined'' value, like <code>nil</code> in Common Lisp, <code>f</code> in Factor, <code>None</code> in Python, or <code>nil</code> in Ruby. --[[User:Kernigh|Kernigh]] 01:13, 11 November 2011 (UTC)

::: Please stop changing task names and descriptions; we want to avoid accidentally making implementations of tasks wrong. Well, unless you're prepared to go through all the implementations and ''fix'' anything invalidated yourself. (Not all languages have a type system that admits nulls — i.e., a bottom to the pointer/reference type system — but where that's the case, there's typically other mechanisms for saying the broader statement of “there's nothing here”.) –[[User:Dkf|Donal Fellows]] 09:30, 11 November 2011 (UTC)
:::: as has been said above, the title was wrong, the solutions are right. i went over them, and below is a listing of the issues i found. i'd appreciate if someone could verify those findings.--[[User:EMBee|eMBee]] 13:54, 11 November 2011 (UTC)
:::::In a language where the only references are names, the name change seems  insignificant (if it was wrong before, for such a language, it would have to also be wrong after).  (But it's also true that many language support references and names as independent concepts.) --[[User:Rdm|Rdm]] 17:04, 11 November 2011 (UTC)

== solutions that need review ==

* Basic: missing actual code
* Javascript: testing for defined
* Maxscript: test for undefined
* MUMPS: test for defined
* PARI/GP: can't tell if this example is about null or not.
* Perl: explains undef as a value but doesn't show how to tell the difference between the variable having a value and the variable actually being undefined. if that difference is not possible then this should be explained. i suppose it's the same as Perl 6, there it is explicitly stated that null objects can not be tested for.

== Quip? ==

This task almost demands a reference to Shakespeare's [[wp:Much_Ado_About_Nothing|Much Ado About Nothing]]. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:44, 16 April 2014 (UTC)

== Delphi vs. Pascal ==

The entries of [[Delphi]] and [[Pascal]] in this article should be exchanged, since Pascal is the language, Delphi is one implementation, (one among others)
And since the null-object is no Delphi-specific feature.
