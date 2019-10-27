+++
title = "Category talk:Go"
description = ""
date = 2014-05-20T18:26:29Z
aliases = []
[extra]
id = 9168
[taxonomies]
categories = []
tags = []
+++

==Language versioning and updates==
Official public launch was November 10, 2009.  As of 2011, substantial changes to the base language are infrequent, but there is a new release with bug fixes and library enhancements every week typically.  Releases are assigned no version number other than a date, so the language must certainly still be considered a work in progress.  It is likely that some RC task solutions will need to be updated at some point due to language changes.  &mdash;[[User:Sonia|Sonia]] 07:50, 17 January 2011 (UTC)
: Since you're more knowledgable about the language than I am (much more!) I suggest you curate its solutions; it shouldn't be too much work as yet. You may find the optional version parameter to {{tmpl|works with}} to be useful; it lets you tag in what specific releases are known to work. –[[User:Dkf|Donal Fellows]] 09:29, 17 January 2011 (UTC)

==Tasks not considered / Omit from Go==
I thought it would be worthwhile to record some thoughts about tasks marked omit.  (All initial content in this section &mdash;[[User:Sonia|Sonia]] 22:10, 29 April 2011 (UTC).  I'll sign additions individually.)

===In general, non-solutions===
I don't think the following sould be considered legitimate solutions.  I think solutions of the following sorts should be removed and the task marked Omit from Go.

* Solutions that essentially implement the task in C or C++ code called through Cgo or Swig, excepting of course, tasks that specifically mention going outside of the language.
:This is what we have {{tmpl|trans}} for. I may be biased because I created the template, but I think it's useful to see code directly translated line-for-line from another language (that's really the clearest comparison isn't it?). I think this is a case where you would keep one example that might be "bad" (i.e. that essentially copies the C or C++ example) and then also add another one that is more idiomatic to demonstrate how this language does things better/differently. --[[User:Mwn3d|Mwn3d]] 04:05, 30 April 2011 (UTC)
::Ah, not what I meant.  Cgo and Swig are foreign function interfaces.  They enable you to say, "I can't do this in Go, so I'll write the whole thing in C, compile it with a C compiler, link the C object module to one line of Go code that just calls this C function to do everything."
::I do like the trans template and have used it once or twice.  I also see your point about the two versions.  That does make sense and I'll watch for occasions where I think that might be appropriate.  &mdash;[[User:Sonia|Sonia]] 05:25, 30 April 2011 (UTC)

* Solutions that use package unsafe to do most of what they do.  It might be acceptable to use unsafe for accessing something or converting something, but not wholesale implementation of the task by peek and poke.

* Solutions using subpackages of exp or go.  Exp is for experimental and I don't think we'd be doing RC readers any favors by showing solutions based on these packages.  The subpackages of go are not experimental, but they are pieces of the implementation of Go.  That is, they are components for writing a language, not using one.  Unless the task is about implementing a language, again, I don't think we would be doing RC readers any favor by implementing our own flavor of Go just to allow a task solution.
: When I introduce people to Rosetta Code, they often want to know how strict the rules are for the nature of task solutions. I'll generally say something like, "if that's how you would do it in that language, then that's how you would do it in that language." I don't tend to condone removal of examples which successfully implement tasks; if there's a better technique, I'd like to see that way shown instead or as well. This helps demonstrate the flexibility of a language, and also serves to avoid edit wars between users with different, strong opinions. If there are significant downsides, or if an example is a last-resort solution, then it's going to be far more useful and helpful for an end-user to see that explained. I.e. in the case of C++, "this is how you use '''malloc'''. You shouldn't do this; it's more trouble than it's worth, and will lead you to bad habits. You should use '''new''' instead." --[[User:Short Circuit|Michael Mol]] 02:43, 30 April 2011 (UTC)

* Solutions that excessively stretch the interpretation of the task.  I know, it's usually a total judgement call.  I've written several solutions that stretch interpretations pretty far.  If you run across something like this and have strong feelings about it, remove the solution, mark the task omit, and then explain your reasoning here.  The decision about how much stretching to allow sould be based heavily on the task wording.  If the task description is very specific, not much stretching should be allowed.  If the task says “do your best to achieve the end result,” then creative interpretations are probably in order.
: In general, I'd rather see a best-effort work-alike solution for language A to task B, than no solution for language A, so long as there is an explanation of the caveats. Personally, I find that helps me understand language A's peculiarities better, and I expect the same would be true for others. --[[User:Short Circuit|Michael Mol]] 02:43, 30 April 2011 (UTC)
::Michael, thank you so much for your perspective!  People, he speaks from experience.  Click the link and read his profile if you don't know who he is.... &mdash;[[User:Sonia|Sonia]] 03:55, 30 April 2011 (UTC)

:: Michael said "... so long as there is an explanation of the caveats".  That's where I think people may fail. If a solution is only a partial solution, or othewrwise stretches the task definition, then if the language implementor still thinks their code has merits then they should start with an up front, unambiguous statement of where they think their solution deviates - as a courtesy to the task writer and the other solutions; then follow up with why their solution should nevertheless be present. That would warn people who are comparing different implementations.  --[[User:Paddy3118|Paddy3118]] 12:00, 30 April 2011 (UTC)


### Add a variable to a class instance at runtime

Go has no dynamic type defintion.  All data types are determined and fixed at compile time.

===Calendar - for "real" programmers===
Starting with "package," Go programs use lower case.


### Call a function in a shared library

Go programs are statically linked and do not load libraries from external files.


### Create an object/Native demonstration

My reading of the task is that it wants you to override default behavior of a [map, in the case of Go] without wrapping it in something with a different syntax.  Sure, you could wrap a map in a defined type with a set of methods that do what the task wants, but I think the task is calling for something that looks exactly like map but works differently.  Natively, maps are created with make() and used with the indexing syntax.  A user type with methods cannot be created with make nor used similarly with indexing syntax.


### Define a primitive data type

Similar reasoning to Create an object/Native demonstration.  You could wrap an int in a struct, but you'd lose all the syntax and--worse for ints--type compatibility.


### Dynamic variable names

No variable names exist at runtime.  Even the reflect package doesn't have them.  (Field names, but not variable names.)  Of course the task does mention "Eval in environment," so perhaps a solution using go-eval would be acceptable.


### Executable library

Go offers no way to do this.  All Go code relies on the Go runtime, which is initialized at program startup.


### Extend your language

Go isn't extendable natively, and standard libraries aren't quite yet up to the task of implementing Go in Go.


### First class environment

There's been some great work done towards Go evaluators, but this isn't really even on the horizon.


### Include a file

The Go authors would be quite dismayed if it were discovered that this was possible.

===Jensen's Device===
Sure you can do something similar with closures, but they won't walk or quack like call by name.


### List comprehensions

Go is low-sugar.  I think a valid solution to this one should have readable syntax.  Go function literals cobbled together in a variadic function call would look hideous and not look anything like set-builder notation.

===Matrix-exponentiation operator===
No operator definition or overloading.  Pretty cut and dried.


### Metaprogramming

The Wikipedia entry on metaprogramming says it encompasses cross-translation, so for example, use of the go template library to generate HTML might count.  The task definition however, specifically cites changing the host language, what the WP article calls reflexive metaprogramming.  This puts it in the category of the Extend your language task, which I don't believe is doable currently in Go.


### Named parameters

While it's tempting to use the struct-as-argument-list trick I showed with the Optional parameters task, this task is pretty specific about use normal function call syntax.  It also mentions safety critical applications, and the struct trick, which allows arguments to be omitted, would seem a step in the opposite direction of safety.


### Pattern matching

My reading of the WP algebraic data type article is that Go has no way to do this.


### Start from a main routine

From the task description: "Languages that always run from main() can be omitted from this task."


### Topic variable

Not in Go.  Go's pretty explicit about most things.

==Once omitted, now solved!==

### Arena storage pool

Go uses a single arena, created at program load and managed by the Go runtime.  Mmap is available as a syscall, but Go offers no support for initializing a raw block of memory like this as a usable heap and no support for working with multiple managed heaps.  Its multiple stacks, I believe, are created within this one arena.

Update: Solution posted using sync.Pool.  It's no mmap solution, but I think it satisfies the general wording of the task.  An mmap solution, probably with cgo, might still be interesting some day.


### Exceptions/Catch an exception thrown in a nested call

The specific wording of the task description excludes Go.  It specifies that foo call bar, bar call baz, and foo catch U0.  The only execption-like mechanism we have is panic/recover.  If foo defers a function that uses recover, it can catch a panic, but it ''cannot continue executing.''  Deferred means deferred to the end and foo is ending one way or another the first time a panic gets to it.

One totally contrived solution would be to make the second call to bar from within the deferred function.  I think most anyone would cry foul at that.
: It strikes me as plausibly appropriate you raise this issue in the task page, and suggest that the task be renamed. It sounds like either a better name for the task might be "Recover from a thrown exception", or that the task should perhaps be adjusted to more cleanly reflect a core intent. --[[User:Short Circuit|Michael Mol]] 02:26, 30 April 2011 (UTC)

:: OK, with hindsight, the task might be better ''described'' as "Exceptions/Catch one of two exceptions thrown in a nested call"; but it is unwieldly for a title. I think the title is not misleading, just a precis of the task requirements. --[[User:Paddy3118|Paddy3118]] 11:43, 30 April 2011 (UTC)

Update:  Solution posted!  After some gnashing of teeth, and ultimately eating of words, I settled on a pretty good rendition of the try/catch pattern to post for this task.  It doesn't meet the task to the letter, but I think certainly does in spirit.  &mdash;[[User:Sonia|Sonia]] 01:45, 3 May 2011 (UTC)


### Introspection

This one might be doable...have to play with the debug/gosym package...
: okay, definitely doable.  I've figured out how to load the symbol table and locate the symbols, now just to decode the type information, do the manual type checking, and set up and execute a function call... &mdash;[[User:Sonia|Sonia]] 04:23, 29 May 2011 (UTC)

Update:  Solution posted.  It's kind of a partial solution because I didn't decode the types, but I'd had this code sitting around and I wanted to post what I had.  &mdash;[[User:Sonia|Sonia]] 13:25, 9 June 2011 (UTC)


### Memory layout of a data structure

Again, maybe with debug/gosym...

Update:  Solution posted.  On closer reading of the task, all it wants is symbolic references to bit positions.  Go does this with constants.

===Shell one-liner===
Go has no standard interpreter...yet.

Update:  Solution posted.  The task really just says ''avoid'' relying on the shell.  Doesn't say you can't, if that's the only way to do it.  Two solutions posted, one kind of a hack and the other practical, using a popular external command that can be installed.  &mdash;[[User:Sonia|Sonia]] 22:18, 27 May 2011 (UTC)


### Respond to an unknown method call

Static, static, static.  Go is static.

Update:  Solution posted.  An anonymous comment on the ''Send'' an unknown method call talk page mentioned reflection, and I thought, "a-ha! Go can do that."  A solution to this task seemed possible as well.

==Once solved, now omitted==

### Inverted syntax

Go had this quirk at one point.  The language was changed before Go 1 and is now free of any inverted syntax.  [http://rosettacode.org/mw/index.php?title=Inverted_syntax&oldid=123161#Go The removed solution]


### Partial function application

Lots of good discussion on the talk page.  I had a closure-based solution submitted at one point that involved repeated type definitions, but in discussion it came out that the task author felt correct solutions  did type manipulation to derive one function type from another.  Go has no features for this, either at compile time or run time.  I happily retracted my submission.  [http://rosettacode.org/mw/index.php?title=Partial_function_application&oldid=104758#Go The removed solution]


### Proof

It was a frustrating exercise.

Go has long had a Peano number program in its test suite.  Without consulting it first, I tried coding up my own version from scratch.  Checking my work then against the program from the Go test suite, I decided the test program was lacking rigor.  Wanting my program to not only be better, but to represent some sort of "proof," I studied the Peano axioms, the Go language specification, beefed up my implementation and added lots of comments showing how the axioms were met by either language guarantees or the code.

Also&mdash;perhaps incorrectly&mdash;I added a mechanism so that equal Peano numbers could be compared equal with the built in == operator.  The conventional way, I believe, would be by recursive function.  But by registering each Peano number visited in a map, objects representing numbers could be maintained uniquely, so that a given number would have only a single object in memory.  This added to the complexity of the program and perhas obscured some issues.

A second obscuring issue is that on the talk page someone was explaining how the task might be solved in C++ and stated, "The proof is whether or not the proper code compiles."  As seen in later discussion on the talk page, this approach was not seen as credible.

Oh, and then I mixed up some variable names...

Finally, at the time I posted my implementation, "dependent types" was not mentioned in the task description but only in a comment on the talk page.  I had read the comment, but being unfamiliar with the term, did not appreciate the meaning from the context.  Only later was the task description updated with a WP link, educating me that a ''dependent type'' is one where the type of an object is determined by its value.

[http://rosettacode.org/mw/index.php?title=Proof&oldid=124926#Go The removed solution]

In retrospect, I still don't know if an acceptable solution can be written in Go.  The one mechanism it has that might be used as a dependent type is the interface type.  An interface type holds as its "value", both a concrete object and the type of the concrete object.  Thus the type stored inside can change depending on the value stored inside.  My solution used interface types, but did not mention dependent types.  I do think a valid solution to the would have to clearly point out how it was using interface types as dependent types.

Also, the now-linked WP pages mention things like Curry–Howard correspondence and constructive proofs, things I am unfamiliar with.  If the task intent is to "do what the proof assistants do" then I suspect a valid Go solution would need to explain in the proper terminology that it is doing something analgous.  The task remains for someone who understands such things.
