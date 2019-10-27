+++
title = "Talk:Language Comparison Table"
description = ""
date = 2015-04-30T23:40:56Z
aliases = []
[extra]
id = 2945
[taxonomies]
categories = []
tags = []
+++

== Haskell and what makes a standard a standard ==

Is Haskell standardized by some standards body, ISO, IEC etc? (Language report certainly does not qualify as a standard. All languages have reports, since Algol 68 times...) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:48, 22 July 2008 (UTC)
: No, it isn't, but the Haskell 98 Report '''is''' the standard definition for the language -- all compilers support it, and all compilers have switches to distinguish between Haskell 98 and their own extensions. Like all C compilers support ANSI C. Compare to, say, OCaml, where the current INRIA implementation of the compiler is the standard :-) Or Lisp, or Smalltalk, with their plethora of different implementations. And since it is one of the design goals of Haskell to have the language completely formally specified, I guess one should mention it somewhere.
: Is there any reason to distinguish between standards enforced by "official" standard bodies, and those enforced by, hm, "community standard bodies"? --[[User:Dirkt|Dirkt]] 08:14, 23 July 2008 (UTC)
::Yes. Standards bodies are here to standardize. So it is up to them to decide what is a standard - an officially published document, which title starts with "ISO" followed by a decimal number... (:-)). Established, commonly recognized practice is not a standard. Neither a formal language specification is. One can formally specify very different languages. However, these two are probably premises for some standards body to start the process of standardization.
::A user community cannot serve as a standards body for the same reason why language preferences cannot do as a language definition. But a community can organize itself in order to bring a new standard to some standard body. AFAIK, for example, ISO actually does not design the standards, it only approves ones designed by some groups of interests, communities. It is a long and painstaking process (which does not necessarily makes the programming language better). So it is unfair to call other things "standards." --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:09, 23 July 2008 (UTC)
::: Sorry to disagree, but 'standards' '''are''' just 'established, commonly recognized practice'. For examples, RFCs are standards for the internet, even though they are not certified by ISO. And the 'standards bodies' are only there to put more pressure on industry, which otherwise likes to ignore the standards, because vendor lock-in increases their profit. It's mainly a political difference. And it's only a long a painstaking process if there are too many entities who have a political interest in it. (And actually, the results are not always so thrilling -- the ANSI C++ standard leaves many dark corners for, say, templates unspecified, unintentionally, and compiler implementations promptly disagree in their behaviour). I just fail to see why anything is better just because it carries some letters. But then, maybe I'm just not cowed enough by presumed authority :-)
::: For me, a standard is anything that I can rely on being supported by an overwhelming majority of whichever products it affects. No matter if it has ISO, ANSI, DIN, or any other letters in it :-)
::: If you for some reason want the table to contain just "official" standards, fine with me -- just clearly say so on the page, and make explicit how to find out if a standard is "official" or not. But I think for practical purposes, that would be the wrong approach: What is really interesting for a programming language is if one can rely on some common core constructs supported by virtually all implementations, and where to find the information which these constructs are. At least that's what would interest '''me''' when learning a new language. 
::: Or, if you for some reason think that standards with some letters in it are "worth" more and should stand out, why not just use green/yellow/red as background? That should give people like me the information they are looking for, and keep the distinction. --[[User:Dirkt|Dirkt]] 10:31, 23 July 2008 (UTC)
::::It seems that you are confusing a practice with the means to enforce, codify, etc it.
::::As for RFC notes, they are edited and published by the Internet Engineering Task Force (IETF). IETF has a defined procedure of discussing and approval the documents they publish, an organizational structure, and last but not least, it declares standardization one of its goals [see [http://tools.ietf.org/html/rfc2418 RFC 2418]]. This makes IETF a standards body, and only '''so''' RFC notes standards. Implied usefulness or acceptance of RFC notes plays here no role whatsoever. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:18, 23 July 2008 (UTC)

== Passing by reference in C and C++ ==
[[C]] and [[C++]] have only by-value passing mode. See an explanation in [[Parameter Passing]] --[[User:Dmitry-kazakov|Dmitry-kazakov]] 17:03, 25 July 2008 (UTC)

: That's not true for C++: In C++, pass by reference is possible by just using a reference for the argument, f.ex.
<cpp>
void foo(int& i)
{
  i = 0
}

int main()
{
  int rc = 1;
  foo(rc); // sets rc to 0
  return rc;
}
</cpp>
: Note that the explanation for C++ references in [[Parameter Passing]] is not correct: Objects are not ''converted'' to references, they are ''bound'' to references. Reference types in C++ are fundamentally different to all other types, as they do ''not'' denote objects, but, well, references to objects. The references themselves are not objects (you cannot have a pointer or reference to them, you cannot assign them — using them on the left hand of an assignment assigns to the object they refer to —, you cannot determine their size, etc.). Logically they just give a name to an existing object (the compiler may store the address to the object named by the reference, but that's an implementation detail). --[[User:Ce|Ce]] 13:15, 5 August 2008 (UTC)

:: Reference T& is a data type distinct from T. As such it has values and objects of its own. The semantics of reference values is such that they refer to objects of T. Reference objects are passed by their values. For the example you provided, consider:

::<cpp>
::int A;
::int& B = A;
::
::Foo (B); // The object B is passed to Foo, by its value
::Foo (A); // The object A has to be converted because formal and actual types differ
::B = A; // Reference assignment (defined as deep copy)
::</cpp>

:: The issue that some operations (like operator&) might not be pre-defined on the type T& is irrelevant. It does not have operator* as well. But assignment is defined on a LHS reference, as the code sample shows. This is why B = A; is perfectly legal when B is of int& and A is of int. When a reference like B is declared, this is a declaration of an object B. [[C++]] is a typed language, thus B need to have a type. This type is obviously not T, it is T&. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:55, 5 August 2008 (UTC)

::: From the C++ standard (well, actually the last public draft of the 1998 standard), 8.3.2 References [dcl.ref]:
:::: 1 In a declarationT D where D has the form
::::: & D1
:::: and the type of the identifier in the declarationT D1 is “''deriveddeclaratortypelist'' T,” then the type of the
identifier of D is “''deriveddeclaratortypelist'' reference to T.” ''[...]'' [Note: a reference can be
thought of as a name of an object. ]

::::: I cannot see where it follows from that [[C++]] references are names. Do you mean "name", a syntactical element, "identifier", member of a name space? This is obviously wrong, because references can be created dynamically as most of other objects, while names are static. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:16, 5 August 2008 (UTC)

::: And later:
:::: 3 It is unspecified whether or not a reference requires storage (3.7).
::: Given that objects do require storage, this clearly shows that references are not objects.

::::: Wrong. The compiler is free to optimize any objects out, while preserving the program semantics. [[C++]] merely gives an explicit permission to do so for reference objects. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:16, 5 August 2008 (UTC)

::: Also note that the standard always mentions references explicitly after objects, where both objects and references can be used.
::: But of course you are invited to quote relevant parts of the standard (of a draft of your choice) to try to disproof me. --[[User:Ce|Ce]] 14:46, 5 August 2008 (UTC)

== Garbage Collection? ==

I'm not sure how interesting the stat would be, but how do you guys feel about adding a "Has [[Garbage collection]]" column to the table? I don't know how many modern language don't have it. The only languages I can be sure of are Java and C/C++. --[[User:Mwn3d|Mwn3d]] 16:39, 1 February 2009 (UTC)
: Done, also is there a way to add a foot note? C and C++ have optional GC with Boehm GC, but that's not really optional in the language, nor is it optional by implementation (like Ada is)
::I don't think the column for Garbage collection is very interesting. It is just one method for freeing memory. And even if it is used, it is automatic, so the programmer does not need to care about it.
::I notice that BASIC is said to have garbage collection. However, this is only true for some Basic implementations. For example Sinclair Basic, Beta Basic and SAM Basic do not have garbage collection. Instead, the memory is freed to the system immediately, so there is never any garbage to collect. This makes for example array redimensioning somewhat slower than with some other Basic implementations, but  then garbage collection is never fired randomly to slow down the operation. --[[User:PauliKL|PauliKL]] 13:15, 16 February 2009 (UTC)
:::The GC column isn't necessarily interesting as in "reading about this is enjoyable." It's more interesting as in "it would be nice to know about this characteristic of this language I'm (thinking about) learning." If I'm looking at a new language, I'd like to know if I need to reach back into my brain to get the C/C++ memory management practices out, or if I can leave them safely tucked away and concentrate on other basic features.
:::I'll fix the BASIC entry. --[[User:Mwn3d|Mwn3d]] 13:54, 18 February 2009 (UTC)

== About this page ==

Wow, I believe that for a site like RosettaCode this comparison table is a great efforts... luckly a lot of programming languages are missing :D. Scattered notes:

* '''About paradigm and C''': if C++ has [[generic programming]], the same must have C (and languages derived from this)
* '''About parameter passing and C''': I've read the talk above; let me specify that the only way C I know allows parameter passing is by value. You can pass a pointer to (and the pointer is passed "by value", being the pointer itself a value), and intend it like a "reference" to a type; but there's no something like "passing by reference"; this features is in C++, not in C, AFAIK.
* '''About C and type safety''': <cite>A language is type-safe if any improperly typed program is illegal in the sense that it may not pass the compiler</cite>... why C is unsafe? you can tune compiler with options so that it raises errors instead of warning... cast exists... then here the same as C++ (safe, but unsafe allowed)...?

(I won't modify anything since I am not able to correctly classify any programming lang, so these are just opinions) --[[User:ShinTakezou|ShinTakezou]] 17:06, 1 February 2009 (UTC)

== About pass-by-reference ==

The terminology of pass-by-value vs. pass-by-reference is really confusing, because different languages and sources use it to mean different things. I propose that we adopt a consistent definition as I will define below.

Let's start by considering Java. The consensus is that Java is only pass-by-value (you can search the Internet, official Java forums, etc. and they all agree on this). The argument is that Java has only two types of values -- primitives and references (Java "references" are basically what you would call "pointers" in C, except safer) -- "objects" are not values in Java, you only manipulate them through references (when you create it it returns a reference, the operator "." only operates on references; in short the language has no concept of "objects" as values). And so, often when a beginner claims to be "passing an object", they are actually passing a reference to the object, by value.

Why is this not pass-by-reference? Yes, you can modify the object that is pointed to by the reference, and someone else with a reference to the same object will see it; but that is irrelevant. The real thing that you are passing -- the reference -- you cannot somehow modify the caller's copy of. So this is what I consider the defining characteristic of pass-by-reference. If you pass a variable as an argument to a function. And that function is somehow able to effectively perform an assignment to that variable (if the variable is a reference, then this means changing the reference so it points to a different (distinct) object, not modifying the object it points to) as if the assignment was made in the caller.
<lang>
a = somevalue
func(a)
// it had the same effect as if you did
// a = someothervalue

```


Under this definition, then, languages like Python and Ruby are also pass-by-value only. In those languages, all values are what Java would call "references" -- different variables can share views of the same object, and variables can be changed to point to different objects without affecting the object it pointed to. And when you pass them, you get the same situation as references in Java. You can still modify the object that the reference points to -- which is shared with the reference passed in -- but you cannot change the caller's reference itself. So you always pass references, by value.

C is also pass-by-value only for the same reason. You can pass pointers by value, but that does not count as pass-by-reference. To have a function be able to modify one of your variables, you have to explicitly take the address of it. C++ and PHP's "references" (different from Java's references above), however, do allow pass-by-reference. You just need to declare a parameter of the function to be a reference (with the "&" symbol), and it allows you to modify the caller's variable. The calling code looks just like they are passing a variable; they do not have to explicitly take the address or anything like that, even if that is what the compiler does behind the scenes.

One instructive resource is the [[Generic swap]] task. The languages that can perform a swap of two argument variables, without any special caller intervention, like taking the address or putting them in an auxiliary data structure, and affect the caller's variables (instead of just returning the new values), and not doing something like switching the object contents to look like the references to them have been swapped when they really haven't; such languages I would consider pass-by-reference. --[[User:Spoon!|Spoon!]] 20:15, 1 February 2009 (UTC)

: I agree with the fact that the "feature" that makes the difference between the two ways of passing arguments to a function is if the callee is able to modify the "original" thing "owned" by the caller (if I've understood well what you've written) or not. Nonetheless there are proper definitions in literature; as far as I know, these say that "passing by reference" allows the modification of the caller copy, while "passing by value" allows it not, since what you have in the callee is just a (local) copy of the original.

: How exactly these ways are implemented are different; in C, to allow the modification of a caller "object", one must pass the pointer to it (the pointer itself is passed by value...); dereferencing the pointer one can modify the local copy of the caller. But other languages may use different terminology and what's going on is hidden by abstraction. As far as I know, in C++ (or was it another lang I am confonding C++ with?:D) the "references" are none but aliases for the same object; so told, I would say the pass-by-reference can be implemented at compile time creating a symbol in the compiler symbol table that is linked to the same "content" of another symbol (roughly speaking).

: Anyway, C has no language syntax for referencing like C++; it is less abstract, more "low level", and the only thing one can do is to pass pointers; if the "number" N points to memory "cell" N containing e.g. an int, "dereferencing" the pointer from inside a func or another one "reads" the same memory "cell", i.e. the same int... and so it is for writing. All the arguments passed to a function are a (local) copy, and maybe we can say all are passed-by-value.

: In C, there's the convention of considering a pointer like a reference. More abstract languages can call "reference" something different (the underlying implementation likely uses pointers, but this is hidden as said; in C, it can't be hidden: it must be done explicity, and it is why I consider C like it has no pass-by-reference, but only a pass-a-pointer, that is a type as int, float or another, except that we can "take" what the pointer is pointing to, which is the most common operation we do on pointers, altogether with incrementing and decrementing it ---and C does not check bounds:D)

: There's another difference (between C and C++): in C, "pass-by-reference" needs awareness by the caller, which must obtain the pointer of the object it wants the callee can modify. In C++, pass-by-reference does not need the "help" of the caller, it is a request of the callee, by declaration. --[[User:ShinTakezou|ShinTakezou]] 23:03, 1 February 2009 (UTC)
==Language pages==
It's awesome that we have all this information here, but why is virtually none of it reflected in the relevant languages' pages?  I'm going to work on getting the language pages categorized according to their entries in this table. --[[User:Short Circuit|Short Circuit]] 08:18, 15 February 2009 (UTC)
:Speaking of category tree, the "category tree" extension will be getting more and more useful as these categories are created and populated. I think it would be really good to have. --[[User:Mwn3d|Mwn3d]] 15:18, 15 February 2009 (UTC)

== Column changing ==

How would you guys feel about removing the design goals column and added an execution method column? Design goals should probably be discussed on the category pages anyway. --[[User:Mwn3d|Mwn3d]] 19:34, 30 July 2009 (UTC)

== Shorten paradigm links ==

Most of the paradigm links are in the form of <nowiki>"[[{blah} programming]]"</nowiki>. Can someone script something up real quick to shorten the display title of the links by changing the wikicode to something like <nowiki>"[[{blah} programming|{blah}]]"</nowiki>? Seems like an easy Perl program, but I don't know Perl. --[[User:Mwn3d|Mwn3d]] 20:27, 9 November 2009 (UTC)
:Done. Here, for completeness, is the regex I used: <code>s/(\[\[((?:\w| |-|')+?) programming)\]\]/$1|$2]]/g</code>. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:44, 9 November 2009 (UTC)
::Good job. The table looks a lot cleaner now. Thanks a bunch. --[[User:Mwn3d|Mwn3d]] 00:46, 10 November 2009 (UTC)

== Goal Direction / Logical Paridigms ==

I've been starting to look at the LCT and some of the parameters and have some questions.  Specifically, I was looking at goal directed evaluation in Icon/Unicon which is similar to things that Prolog does.  There doesn't seem to be a paradigm for this and I was thinking there should be.  How do others feel? [[User:Dgamey|David]] 2010-04-06
: Add it; I don't think there's a standards body for defining paradigms. :) --[[User:Short Circuit|Michael Mol]] 15:49, 7 April 2010 (UTC)

== Binding with other site features ==

Does anyone have thoughts on ways of binding the LCT with the extended options for [[Template:Language]], and the pages/categories that use [[Template:Feature]]? I'd like the LCT to get greater visibility, but I think it needs to be a bit more integrated. (Nevermind that the ehnancements to Template:Language and the categories that use Template:Feature were inspired by the LCT.) --[[User:Short Circuit|Michael Mol]] 15:54, 7 April 2010 (UTC)
:There were talks of a bot a while ago I think. That would be a sort of dirty way to do it. Something more automatic would probably be preferred, but might require extra web-fu. --[[User:Mwn3d|Mwn3d]] 17:22, 7 April 2010 (UTC)
:: Bots are the normal way to do things in MediaWiki, and, in any case, they stand to be a good first step before integrating code in the server software itself; the kinks and such can still be worked out before the execution environment becomes something ''I'm'' responsible for maintaining. :) --[[User:Short Circuit|Michael Mol]] 17:24, 7 April 2010 (UTC)

== Standard BASIC ==

The BASIC entry notes that there are ISO and ECMA standards for the language. While this is true, the vast majority of implementations simply ignore the standards; indeed, many compiler writers have chosen instead to "standardize" on some variation of Microsoft BASIC, due to its popularity. Should this be noted somehow in the table itself? -- [[User:Eriksiers|Eriksiers]] 20:04, 10 April 2010 (UTC)
: Create an Encyclopedia page to hold the extended and supporting details and references for that cell? *bemusedly notes that you could directly transclude that page, with &lt;includeonly&gt; and &lt;noinclude&gt; sections.* --[[User:Short Circuit|Michael Mol]] 22:03, 10 April 2010 (UTC)
:: You mean something like "BASIC standards"? Hmm... Lemme think on it, and dig up some links. -- [[User:Eriksiers|Erik Siers]] 21:04, 11 April 2010 (UTC)
::: More generally, [[Encyclopedia:Language/Column]], considering it's probable other cells in the table can and will be able to benefit from the same treatment. But, yeah. --[[User:Short Circuit|Michael Mol]] 21:08, 11 April 2010 (UTC)

== Other considerations for language comparision tables ==

Here are some considerations that I use in my documentation. It may be useful to include these in the table also:

* Compilation Model - Is there a compiler available that converts the programs into
native mode executables? Or can the program be converted to bytecode for use by a bytecode interpreter? Or is [[JIT]] compilation used at runtime, or is the language only utilized by an interpreter?

* Input / Output Model - Does the language support the use of C IO Model, or does the language only utilize [[redirection]] operators, or does it use its own IO model (such as REXX).

* Named Locations - Does the language support the use of location names, or are line numbers required?

* Does the language support [[terminal control]]? If so, to what extent? (I will develop a scale for this)

* Does the language support the use of colour terminals?

* Does the language support the use of graphics? If so, is this achieved via svgalib, or is an X server required?

* Does the language support the use of sound? Is this just a terminal beep, or can the language be used to drive a pc speaker? Is there support in the language for use of a sound card?

* Does the language support the use of scoped variables?

* Does the language support the use of structures or user defined types?

* Does the language have the facility to obtain command line parameters for the program?

* Does the language provide a facilty for processing [[environment]] variables?

* Does the language provide support for outputting text to a line printer (for example via an LPRINT command)?

* Does the language provide support for on error events?

* Does the language provide a freefile facility for determining unused data stream or file numbers?

* Does the language provide the facility to determine whether or not a key has been pressed (for example an INKEY$ function).

* Does the language provide support for multiline if conditional branching?

* Does the language provide support for conditional compilation directives?

* If the language is interpreted, rather than compiled, can a [[hashbang]] mechanism be used at the top of the program to trigger an appropriate interpreter?

==Perl 6 - Strongly typed==
I am intrigued by the following comment that TimToady added to an edit where Perl6 is changed to be strongly typed:
:''Perl 6 is strongly typed; all apparent polymorphism/conversion is user-controllable''
What does the comment mean w.r.t. an expression like <code>'1' + 1</code>. Would it work? Are there interpreter options and/or program statements that could make it work/not work? Would it work 'by default'? I just have a thought that a fuller answer might be more than say the answers for Java, Haskel or Python. --[[User:Paddy3118|Paddy3118]] 20:47, 13 September 2011 (UTC)
:You left out the most salient bit of the comment, which was "by the definition provided".  So my remarks should be taken in the context [http://rosettacode.org/wiki/Type_strength].  Also, if you are extrapolating from the weak semantics of Perl 5, forget that, because Perl 6 has a completely different type system.
:So let's look at <tt>'1' + 1</tt>.  The term <tt>'1'</tt> is known to be of type Str, while the <tt>1</tt> is of type Int.  These are both types with well-defined, implementation-independent sets of values.  (They are not confused into scalar values as they are in Perl 5.)  The default string type is a sequence of Unicode graphemes without arbitrary size limit.  The default integer type is the set of all integers without arbitrary size limit.  So we don't have the problem of undefined types or overflow that the definition points out in C, nor is there any implementation dependency.  Conversion of string to number is required to complain about ill-formed numbers.  Any conformant implementation of Perl 6 must support these semantics.
:The expression in question does work by default, but not because of any implicit coercion within the actual addition operator, well, operators, really, since they're multi subs, which have signatures that know how to add integers, rationals, floaters, etc.  But they don't do implicit coercion; instead, we use "mechanisms resembling weak typing" to define allowable coercions.  There is a specific generic signature that allows coercion of strings to numeric, after which the appropriate numeric operator is applied.  These signatures are defined in a magical outer lexical scope called the setting, and are ostensibly written in Perl 6, apart from the places where circularity must be broken by use of primitives.  (A setting is allowed to cheat as long as everything in the setting ''appears'' to be written in Perl 6.)
:Any or all such overloaded signatures may therefore be shadowed in an inner scope, including the coercive generic signatures.  One of the strong design principles of Perl 6 is that every lexical scope know exactly what language it is using, where "exactly" does not preclude genericity, but only accidental genericity.  Therefore lexical scoping is how we override anything in the outer language and produce a new language in an inner lexical scope.  Since Perl 6 is designed to be completely mutable in this sense, such an inner language can appear to be as weakly typed as you like, but since all of the outer primitives are, in fact, strongly typed, Perl 6 is better characterized as strongly typed.
:But finally, I'd like to point out that the very first thing the definition in question says is that type strength is a "vague term".  <tt>:-)</tt> --[[User:TimToady|TimToady]] 23:44, 13 September 2011 (UTC)
::Thanks TimToady for the answer. I'm going to have to read this again to digest it, but then I ''did'' think my comment was probably asking to reveal more than just the tip of the iceberg. (Sometime soon I'm going to have to learn more of Perl6 as it continues to pique my interest). --[[User:Paddy3118|Paddy3118]] 03:13, 14 September 2011 (UTC)

== REXX ==

Please add entries for 
REXX
ooRexx
NetRexx
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 04:11, 22 June 2013 (UTC)

== Esoteric languages ==

I think esoteric languages should be added to this table, possibly in another section. --[[User:12Me21]]

: Wouldn't most of the column headings be irrelevant for most esoteric languages.? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:40, 30 April 2015 (UTC)
