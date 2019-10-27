+++
title = "Talk:Function definition"
description = ""
date = 2010-02-06T12:56:41Z
aliases = []
[extra]
id = 2391
[taxonomies]
categories = []
tags = []
+++

"multiplication" is a somewhat ambiguous mathematical operation depending on the data type. I don't think it's a big deal, I am assuming that the arguments are supposed to be scalars and such, but in a vector language like IDL where one rarely encounters scalars, there is a distinction to be made between "product of the elements of the vectors" or "inner product of the vectors" or "outer product of the vectors" or "matrix multiplication". Just figured I'd mention this somewhere... [[User:Sgeier|Sgeier]] 19:08, 4 December 2007 (MST)
: I was just looking around, and I noticed it was ambiguous for another reason: It's commutative.  A non-commutative operation might be more appropriate for scalar arguments. I'm not sure about other types such as vectors or functors. --[[User:Short Circuit|Short Circuit]] 02:46, 8 May 2009 (UTC)

== Lisp, BASIC ==

While I indeed just had overlooked the existing Common Lisp implementation, I don't agree with the change comment:

IMHO Lisp ''is'' a language, and Common Lisp is a Lisp ''dialect.''

And of course, Scheme is ''not'' Lisp, but a separate (but similar) language, which BTW also has several dialects.

:: Out of curiosity: what is the distinguishing mark that makes you say that? I.e. what difference is "so big" or "so significant" that you would say "it is a different language" as opposed to "it is a dialect of the same language". I'm asking because I once had a professor that considered C and FORTRAN and similar imperative languages to be mere "algol dialects". [[User:Sgeier|Sgeier]] 18:03, 5 February 2010 (UTC)

Indeed, the example which I wrote (and which, not surprisingly, turns out to be identical to the Common Lisp example) should work in any Lisp.

OTOH, I'd question that BASIC could be considered a single language. While there's an original BASIC language (although even then there were lots of dialects), the languages which are called "BASIC" today differ far more from original BASIC than e.g. C++ differs from C. BTW, original BASIC didn't have functions in the full meaning of the word, but the closest was GOSUB. For example, an approximation of the task would look like this:

 10 LET i = 10 : REM many BASIC dialects allowed to omit the LET
 20 LET j = 20
 30 GOSUB 100
 40 PRINT result
 50 END
 100 REM This is the "function"
 110 LET result = i*j
 120 RETURN

However, some BASIC dialects (like the ZX Spectrum's BASIC) allowed to define functions consisting just of one expression, e.g.

 10 DEF FN f(x,y)=x*y: REM the ZX Spectrum BASIC only allowed one-letter function names
 20 PRINT FN f(2,3)

The QuickBasic language used in the example was already quite far away from the original BASIC language (no line numbers, true functions, named types, user-defined types). --[[User:Ce|Ce]] 12:56, 1 March 2008 (MST)

* I disagree about Lisp, for the same kind of reason you consider BASIC not a single language, but it's not worth arguing over. I won't object if you readd the "Lisp" example. --[[User:Kevin Reid|Kevin Reid]] 14:44, 1 March 2008 (MST)

:: Well, the differences in BASIC are far greater: It isn't even possible to write a (non-empty) ''do-nothing'' program which works both in original BASIC and current languages using that name.
:: It certainly doesn't make sense to have both Lisp and Common Lisp here (it would make sense if Common Lisp had significant differences relevant for that task). IMHO it would however be reasonable to move the Common Lisp example to "Lisp" --[[User:Ce|Ce]] 08:01, 2 March 2008 (MST)

No actual programmer is going to be "writing a program in Lisp" — they're going to be writing it in <insert Lisp dialect here>. I think the examples should be categorized according to how a practicing programmer would identify the language they're working in. Therefore, there should be a "Common Lisp" example. --[[User:Kevin Reid|Kevin Reid]] 08:23, 2 March 2008 (MST)

: Having the example listed under "Common Lisp" signals that it's specific to Common Lisp, which it isn't. Of course one could add identical examples for all Lisp dialects, but I don't think that would be a good idea. --[[User:Ce|Ce]] 09:03, 2 March 2008 (MST)

:: Perhaps having the code example under Lisp would be appropriate if there was an intra-page link from Common Lisp pointing to it? --[[User:Short Circuit|Short Circuit]] 20:13, 2 March 2008 (MST)

== SNUSP, Falcon ==

I don't think either of these actually do what the task description asks for...[[User:Sgeier|Sgeier]] 18:04, 5 February 2010 (UTC)
