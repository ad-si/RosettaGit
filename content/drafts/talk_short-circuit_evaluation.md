+++
title = "Talk:Short-circuit evaluation"
description = ""
date = 2019-01-03T07:51:09Z
aliases = []
[extra]
id = 7808
[taxonomies]
categories = []
tags = []
+++

==Why a draft task?==
A comment in [http://rosettacode.org/wiki/Rosetta_Code:Village_Pump/Request_a_programming_task#Unsorted here] (look for ''shortcutting''), thought that the task would most likely not be written in a neutral manner. I've tried to write a task that allows each language, whether it has short-circuit evaluation or not, to solve the problem idiomatically. 

I saw the comments on the Icon solution, for example, which makes me think that the task could soon lose the draft status? --[[User:Paddy3118|Paddy3118]] 23:59, 24 July 2010 (UTC)

: I removed the draft status as the examples seem to cope (another example is Pascal where some compilers have it and some do not).

== Error in task? ==
The problem states:
```txt
  x = a(i) and b(j)
  y = a(j) or  b(j)
```

But the only example is written as: 
```txt
  x = a(i) and b(j)
  y = a(i) or  b(j)
```

Either the definition of the first example is wrong.  I'm guessing the task description but ... --[[User:Dgamey|Dgamey]] 17:50, 24 July 2010 (UTC)

: You are right of course. Hopefully fixed now, thanks. --[[User:Paddy3118|Paddy3118]] 23:43, 24 July 2010 (UTC)

== Control structure? ==

I would be inclined to add the control structure tag to this task.  Lisp was one of my early languages to learn, and I always though of short-circuit <tt>and</tt> and <tt>or</tt> as control structures.  (Of course, as I later learned, many concepts that seem natural and intuitive in Lisp are viewed as insanity by the mainstream imperative programming world....)  Anyone else have feelings about adding or not adding the control structure tag?  &mdash;[[User:Sonia|Sonia]] 17:18, 20 April 2011 (UTC)


:It was used as such for a long time in Python too. <code>value = condition and x or y</code> was used until later syntax additions allowed that to be expressed as <code>value = x if condition else y</code>. So I can see your point. --[[User:Paddy3118|Paddy3118]] 17:47, 20 April 2011 (UTC)

:In my opinion, short-circuit evaluation is a form of flow of control in any language with side effects.  (And, in languages without side effects, flow of control is not a meaningful concept -- there, it's just a question of which results get used, but that should not matter here.) --[[User:Rdm|Rdm]] 17:44, 20 April 2011 (UTC)

: +1 &mdash;''[[User:Ruud Koot|Ruud]]'' 19:17, 20 April 2011 (UTC)

== Compiler optimisations? ==
A variant of this issue arises when the second part of an expression might cause a disaster such as division by zero or indexing out of bounds, as in
:<code>while i > 0 and A(i) ''etc.'' do i:=i - 1;</code>
where an attempt at A(0) would be improper. The first test protects the second against evaluation only with short-circuiting. A compiler might generate code that ''always'' evaluates both parts, or, ''consistently'' short-circuits, or, short-circuits only if the first part is a certain sort of expression, such as a boolean variable but not if a function...


: Text moved as unclear if it describes fully compliant compiler - If language states short-circuit then compiler should be compliant. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:07, 28 April 2015 (UTC)

:: Perhaps apropos is this quote from Charles Moore's 1970 book "PROGRAMMING A PROBLEM-ORIENTED-LANGUAGE" [http://www.colorforth.com/POL.htm Moreover this attitude is reinforced by the massive trend to high-level languages and a placid acceptance of their inefficiencies: What's the use of designing a really good algorithm if the compiler's going to botch it up anyway?] --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:23, 28 April 2015 (UTC)

:::I do remember posts about non-compliant Pascal and C compilers in the 90's but not so much now and if a language states short-circuit evaluation then a compiler that does not preserve that has a major flaw in my book - short-circuit eval. is a feature big enough not to be missed out in a language compilerd test suit methinks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:32, 28 April 2015 (UTC)

:::: Nowadays an issue is command line options and how they interact with subtleties of the language spec. Another issue, though, is where people have mis-read the language spec and [for example] enforce a constraint in the generated code which was meant in the specification to apply to be a constraint on the code supplied to the compiler. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:48, 28 April 2015 (UTC)

:Well, the Turtbo Pascal compiler offered a directive {$B+} to turn on full evaluation instead of the default of short-circuit, and appears to have diligently done so. But here is an extract from the Compaq Fortran 95 compiler...
You should not write logical expressions whose results might depend on the evaluation order of subexpressions. The compiler is free to evaluate subexpressions in any order. In the following example, either (A(I)+1.0) or B(I)*2.0 could be evaluated first:
  (A(I)+1.0) .GT. B(I)*2.0
Some subexpressions might not be evaluated if the compiler can determine the result by testing other subexpressions in the logical expression. Consider the following expression:
  A .AND. (F(X,Y) .GT. 2.0) .AND. B
If the compiler evaluates A first, and A is false, the compiler might determine that the expression is false and might not call the subprogram F(X,Y).

:Evidently, this compiler does what it likes in various situations, and there is no sign of any talk about short-circuit options. I recall also discussions of Algol in which an expression (A something)*(B something) might supposedly have the two parts of equal precedence evaluated "in any order". This has always irked me, as I say the order of evaluation is definite: by precedence and, left-to-right, having had experience with the importance of the order of evaluation as in something like e**3/(h*m**2) or somesuch causing exponent overflow: charge&mass of electron, Planck's constant...
:Thus, a language (or a compiler manual) may state nothing about short-circuitry, offer no option for it, and do one thing or the other in various circumstances as might be explained in a different context. So I don't think that talking about "compliant" helps, since it appears that Fortran's specifications make no statement and thus a compiler has no promise to deliver on. There is no "meta rule" that holds that all languages must declare their position on this point. In short, I think some such remark about this problem should appear in the heading - the justification for this exercise in needing clarity is not just whether or not an effortsome second function's evaluation could be avoided. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:50, 29 April 2015 (UTC)

::If short-circuiting isn't covered by the language then the last sentence of the task description should hold: ''"If the language does not have short-circuit evaluation, this might be achieved with nested if statements."'' If short-circuiting is covered by the language then a compiler is either wrong or any optimisations must preserve short-circuit operations.
::--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:24, 29 April 2015 (UTC)

Just a few notes about Fortran. The references refer to the Fortran 2003 standard.

Short-circuit evaluation is allowed by the standard, but not mandatory: ''It is not necessary for a processor to evaluate all of the operands of an expression, or to evaluate entirely each operand, if the value of the expression can be determined otherwise.'' (7.1.8.1) It may be painful if evaluation causes side effects, or when testing for an array index before testing the array value.
* For example, ''IF(I < N .AND. A(I) < X) ...'' may crash the program. In languages with guaranteed short-circuiting, this is a usual construct though.
* In ''Z = F(X) .AND. Y'', the ''F(X)'' part may, or may not, be evaluated. This could of course be a problem if ''F'' has side effects.
* However, in ''Z = F(X) . AND. G(Y)'', if ''F(X)'' has side effects that change the value of ''G(Y)'', this is not standard conformant: ''The evaluation of a function reference shall neither affect nor be affected by the evaluation of any other entity within the statement.'' (7.1.8)

Therefore, you can't rely on short-circuiting in Fortran, and you have to use a nested IF, as Paddy3118 remarks. Even if one particular Fortran compiler guarantees short-circuit evaluation (maybe with a command-line option), using this feature would not affect standard conformance of the program, but it could certainly affect its successful execution with other compilers.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 10:27, 30 April 2015 (UTC)

:Thanks @Arbautjc and Wow! I would never have thought that Fortran would be so indeterminate on this. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:45, 30 April 2015 (UTC)

Historically, Fortran offered no logical arithmetic (so one used integers with + and * for ''or'' and ''and'') and arithmetic expressions were evaluated according to precedence and left-to-right for tie breaking. There was little point in short-circuiting the multiplication zero as in A*B and A zero - and these days, the advent of NaN (with additional violations of the axia of mathematics) requires full evaluation anyway. So the rules were clear. But in the 1970s I recall the appearance of "in any order" as an enabler for compiler optimisation opportunities (initially for Algol and Pascal and the like), an echo of which is the phrase above "shall neither affect nor be affected by" for later standards. So instead of straightforward behaviour, preferably with short-circuitry, there is now a tangle of possible rearrangement. The central issue for programming is the collating and sequencing of multiple actions so as to attain the desired result, but now the collation is tossed in the air and who knows what part will land first? In the absence of reliable short-circuitry, instead of a simple arrangement one is stuck with a lot of blather: 
 do while (i > 0 and A(i) < x)
  i = i - 1;
 end do

 again:if i > 0 then
        if A(i) < x then
         i = i - 1
         go to again
        end if
       end if
Anyway, the question remains: should there be some mention of this usage of short-circuitry in the motivation for establishing the behaviour of the compiler for each language? [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 13:13, 1 May 2015 (UTC)

:I think that for this task, if it is not a feature of the language then this should be noted, especially if it can legitimately become a feature of a compiler or is a feature of a popular compiler. If you know that you are depending on a feature of particular compilers/compiler optimisation settings then this should be stated. You can also do a nested-if solution for the general case. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:27, 2 May 2015 (UTC)
::See [https://software.intel.com/fr-fr/forums/topic/282534 this discussion] about Intel Fortran and its ancestors VAX Fortran and Compaq Visual Fortran, and why it's indeed a good thing _not_ to have short-circuit evaluation. I don't know a compiler that would provide this as an option, however my experience is limited to gfortran and the commercial compilers of Intel and Absoft, and even with these I may have missed an option. VAX Fortran was said to have this, but Steve Lionel has stated on several occasions that it was not so. I'd say that having both like Ada would probably be better, and actually there have been discussions in 2004 about new operators .ORELSE. and .ANDTHEN., resulting in a proposition by Van Snyder [http://j3-fortran.org/doc/year/04/04-390.ps]. See also document N1972 at WG5 [ftp://ftp.nag.co.uk/sc22wg5/N1951-N2000/N1972.pdf]. Maybe they will eventually get their way into the standard. Anyway, they are not part of the current draft of Fortran 2015 [http://j3-fortran.org/doc/year/15/15-007.pdf]. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 21:45, 2 May 2015 (UTC)

:::The turbo Pascal compiler offered a B+ or B- option to choose full or short-circuit evaluation, though I don't know of any such option in a Fortran compiler. On the one hand are the simple and clear usages for ''safe & test'' and on the other are vague murmurings about unspecified potential optimisations that might involve possible parallel execution via multiple cpus or code reordering (apparently ignoring the left-to-right rule) and any amount of hand-waving. That the modern fortran standard supports the latter does not mean its words deserve respect as holy writ from on high, especially as I have often wanted the simple scheme to work and have not seen any actual gain from the fog of vapour-optimisations. Suggestions such as .OR ELSE. sound vaguely threatening! With Compaq V. Fortran, sometimes this worked and other times not, the determinant being the precise nature of the expression and operands and vagaries of compiler choices involving register allocations or whatnnot. Although conforming to the inspecificity of the standard, I don't think this is a good thing at all. I prefer definite and understandable behaviour! Which I can then take advantage of. And such reliance becomes my mistake, since the standard's choice is not mine. Fortunately, only a few usages had to be repaired, but others, especially where the compiler happens to choose short-circuitry, may lurk. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 12:21, 4 May 2015 (UTC)
::::Notice that "and then" and "or else" are part of the Ada and Pascal standards. (the extended Pascal flavor, or ISO 10206, of course not the nonstandard Borland dialects) It would arguably be nice if short-circuit was the default. Likewise, it would be nice if statements like "integer n = 0" in a function were executed at each function call. But Fortran is not C. :) [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 19:17, 4 May 2015 (UTC)
:::::Humm. I look in the compiler "help" system, and see for example <code>COMPLEX :: cube_root = (-0.5, 0.867)</code> both declaring and initialising a variable. Whether this value would apply to only the first invocation or to every invocation presumably depends on further opportunities for declaring STATIC and AUTOMATIC and SAVE, but no matter. PL/I also has a declare and initialise protocol.
::::::No, it does not depend, it's only executed once. This is only a replacement for the DATA statement of Fortran 77. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 05:34, 10 May 2016 (UTC)
So, the options are that a language/compiler
:1) takes the expression at face value, computes both parts and then performs the '''or''' or '''and''', exactly as written.
:2) computes the first part, tests, and then evaluates the second part only if necessary.
:3) offers an option to select one or the other style, perhaps only over a limited span.
:4) supplies alternative operators: "andmaybe" and "orinstead" or other symbolism (| vs || in Octave, for example)
:5) refuses to guarantee either behaviour, talking about "any order" or even in parallel.
I know what I want... Incidentally, statement (and sub-expression) re-ordering is a popular optimisation, so even the sesquipedelian multi-statement expression may be in doubt.

I still think the lead sentence, speaking of avoiding lengthy calculation, should mention not just (''quick'' or ''slow'') but also (''safe'' and ''test'') as justification for knowing just what will happen. I don't regard avoiding an array indexing error as a side effect. If it is avoided, it doesn't happen. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 11:28, 6 May 2015 (UTC)
:Your last sentence reminds me of a compiler trick with gfortran: I wrote a little program to see how a bad program could crash (changing the value of a constant, IIRW). Without optimisation, it crashed, with optimisations, it didn't. It's perfectly correct in regard of the Fortran standard: such a program has unspecified behaviour, thus crashing or not according to a compiler option is as "valid" as blowing up the computer altogether or anything else. But I still feel it's not fair, and I consider this a bug. At least, it's incredibly misleading. This behaviour disappeared with the -fnoipa-cp compiler option. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 22:32, 20 July 2015 (UTC)

::Ahem. I've done that too, and with intent. The IBM1620's Fortran II compiler allowed only simple expressions for array indexing, (''const''*''variable'' +- ''constant'') and I wanted (I + J) inside a READ statement's list and there was some reason why I couldn't use an implicit DO-loop as in (A(K),K = I + J,...) or similar. Anyway, as you will have guessed, I found I could write A(J + 12345) after calling subroutine ZAP(12345,I) and suddenly, the storage set aside to hold the constant would contain ... something else. This would not work for simple constants such as zero that on some systems might be developed by in-line code. Naturally, there must be no other usage of a constant 12345 that expects it to have its proper value. On the IBM1130 this would ''not'' work, because although the constant would be damaged, constant offsets in array indexing were merged with the base address of the array and so vanished at run time. And anyway, array indexing now allowed arbitrary expressions. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 03:34, 7 June 2017 (UTC)

::: There were (at least) two Fortran II compilers for the IBM 1620.   One was written by IBM, the other was a one-pass compiler and written by Dr. D. G. Robinson, D. A. Jardine (and others) of DuPont of Canada.   Locally, we called it Fortran 2.5 and it had a lot of Fortran IV features.   It allowed almost any expression for array indexing.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:50, 3 January 2019 (UTC)
