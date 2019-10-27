+++
title = "Talk:Quaternion type"
description = ""
date = 2016-09-22T21:41:47Z
aliases = []
[extra]
id = 7859
[taxonomies]
categories = []
tags = []
+++

==Why a draft project?==
Because I am unsure if the topic is right for RC. I tried to approach it in such a way that people could just implement what is stated in the task description without going to even the depths of the Wikipedia article. --[[User:Paddy3118|Paddy3118]] 13:24, 3 August 2010 (UTC)
: Just by the discussion that popped up, I think it's a worthwhile task for illustrating differences between correct imaginary and quaternary operations, and I'd think an 8-class task would be interesting (if complicated), too. (And now everyone at this GURPS session is waiting for me to pay attention.) --[[User:Short Circuit|Michael Mol]] 23:01, 3 August 2010 (UTC)
:It sounds interesting. Out of curiosity, can it be generalized to N real and M imaginary dimensions? ISTR there was a task that got played and adjusted to various dimension sets. I don't remember the name off-hand. (Hm. It occurs to me that we can create relationships between tasks such as "generalization of::some other task". That strikes me as an interesting direction to explore.) --[[User:Short Circuit|Michael Mol]] 15:24, 3 August 2010 (UTC)

:: In short: no.

:: First, only certain values of N and M "work".  Complex numbers would be N=2 and M=0 OR N=1 and M=1.  Quaternions would be N=4 OR N=2 and M=2 OR N=1 and M=3 (depending on exactly what you meant by "imaginary dimensions").  But you can not do anything non-trivially useful with N=3 and M=0 (nor N=1 and M=2).  To my knowledge, only 1, 2, 4 and 8 dimensions work here with this kind of arithmetic.

:: But also, quaternions, and other Cayley–Dickson classes of numbers, are not a full generalization of simpler numbers.  Complex numbers do not have some properties which real numbers have.  (For example, complex numbers can not be ordered on a line.)  Quaternions do not have some properties which complex numbers have (for example quaternion multiplication is not commutative).  Octonions lose some properties which quaternions have (for example: octonion multiplication is not associative).  So you have to decide if you are willing to deal with the problems introduced by the additional dimensions.  (And even that can be risky: I have seen too many mathematical "proofs" which assume that quaternion multiplication is commutative -- which means they are about as meaningful as proofs which assume that 0 divided by 0 is unique.) --[[User:Rdm|Rdm]] 19:42, 3 August 2010 (UTC)
::: Mm. So we're talking about N and M needing to be powers of 2? (I haven't peeked at NevilleDNZ's link&mdash;no time&mdash;but that's what's coming to mind.) And that as N and M go up, the operations lose properties. Interesting and weird at the same time; it suggests to me that there's a way to linearly map a number like N or M to a property set and make prediction, but I think it ought to be getting obvious I never went very far in higher math. :-| Very, very interesting info, though. --[[User:Short Circuit|Michael Mol]] 22:58, 3 August 2010 (UTC)
:::: Actually, if we want to stick with the most obvious meanings, N=1 and M is an element of the set {0, 1, 3, 7}.  That is, M must be less than 8 and must be 1 less than a positive integral power of 2. However, The Cayley-Dickson approach might let us argue about degrees of imaginaryness, and we can represent quaternions (and so on) with or without explicit use of imaginary numbers.  --[[User:Rdm|Rdm]] 01:47, 4 August 2010 (UTC)

::Hi Michael, The wp article does mention [[wp:Octonian|Octonians]], and I also read [[wp:Division algebra]] enough to know that from reals to complex to quaternions to octonians; things seem to get a little less useful. The octonians seeming to have 480 ways to multiply for example. --[[User:Paddy3118|Paddy3118]] 16:28, 3 August 2010 (UTC)
::: I was thinking more along the lines of the kind of code generalization that would allow the same code to operate correctly on any >=0 integer value for M and N. Granted, we're delving outside a simple task. My curiosity there lies being able to learn the relationship by studying the code. --[[User:Short Circuit|Michael Mol]] 18:05, 3 August 2010 (UTC)

re: Out of curiosity, can it be generalized to N real and M imaginary dimensions?

There is an interesting generalisation called [[wp:Category:Clifford_algebras|Clifford algebras]] which contains a useful subset for [[wp:Algebra_of_physical_space|Algebra of physical space]] and [[wp:Spacetime algebra|Spacetime algebra]] etc. eg:
# [[wp:Algebra_of_physical_space#Special Relativity|Special Relativity]]
# [[wp:Algebra_of_physical_space#Classical Electrodynamics|Classical Electrodynamics]]
# [[wp:Algebra_of_physical_space#Relativistic Quantum Mechanics|Relativistic Quantum Mechanics]]
# [[wp:Algebra_of_physical_space#Classical Spinor|Classical Spinor]]

[[User:NevilleDNZ|NevilleDNZ]] 19:24, 3 August 2010 (UTC)

==What simple properties should become part of the task?==
Well it seems that enough people like the draft task!

Before I remove the draft status, are their anyother ''simple'' properties of quaternions (ore fewer), that the task should ask be shown? --[[User:Paddy3118|Paddy3118]] 04:24, 4 August 2010 (UTC)

This is just a personal preference, in preference to a simple 1+1=2 test I prefer - if possible - tests that are a little but more extensive.
  
For example for the [[Rational Arithmetic]] test:<blockquote> Use the new type '''frac'''[tion] to find all perfect numbers less then 2<sup>19</sup>  by summing the reciprocal of the factors. </blockquote>

Basically this test is nice because runs the gauntlet of multiple additions and divisions to come up with a known result.

With '''compl'''[ex] numbers such a test could be to check that exp(πi)=-1.  Maybe there an equivalent for Quaternions?

[[User:NevilleDNZ|NevilleDNZ]] 05:16, 4 August 2010 (UTC)

: As I understand it, unit quaternions are used to model rotations. Perhaps using them to model some kind of rotation would do as a “demonstration” question? –[[User:Dkf|Donal Fellows]] 07:20, 4 August 2010 (UTC)

:: Hmm, there is sample pseudo-code and Python [[wp:Quaternions_and_spatial_rotation#Pseudo-code_for_rotating_using_a_quaternion_in_3D_space|here]]. --[[User:Paddy3118|Paddy3118]] 10:40, 4 August 2010 (UTC)

==ADA Issue?==
Hi, value r should be a floating point number rather than a quaternion without imaginary parts. If you then have to convert r to a quaternion to do the task then that should be shown. --[[User:Paddy3118|Paddy3118]] 18:21, 6 August 2010 (UTC)
And the same applies to Algol 68. --[[User:Paddy3118|Paddy3118]] 18:23, 6 August 2010 (UTC)
:Thanks. You are right, I just copied the test from Algol. Fixed. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:31, 6 August 2010 (UTC)
::Thank you Dmitry. --[[User:Paddy3118|Paddy3118]] 18:40, 6 August 2010 (UTC)

== Name ==
The name of this task seems over-qualified. What does everyone think of renaming it to "Quaternion", "Quaternions", or "Quaternion type"? —[[User:Kevin Reid|Kevin Reid]] 19:50, 3 February 2011 (UTC)
:Doh! It seems so necessary now you brought it up. I moved it. --[[User:Paddy3118|Paddy3118]] 20:03, 3 February 2011 (UTC)

==E language limitations==
The E example has a couple of comments of the form:

```e
        # Task requirement 6, 7
        # This implements q * r; r * q is deliberately prohibited by E
```


I was wondering if these are truly language limitations or are they just limitations of this particular implementation? --[[User:Paddy3118|Paddy3118]] 00:09, 10 March 2011 (UTC)

==Ruby and subtraction==
It seems from comments in the template added to the Ruby example, that the Ruby example:
# Performs all that is asked for in the task description.
# Fails when a quaternion is subtracted from a real. This not being asked of by the task.
What to do?
I would vote for there not being any information on this added to the languages entry, but a note on its limitations could be added here in the talk page. My reason is that there are other operations on quaternions that are not mentioned as part of the task and I would not want their ommision cluttering the task page either. 

Some might argue that this particular omission is sufficiently close to what is asked for in the task. Yea, it's close; but where to draw a line?  

Comments please. --[[User:Paddy3118|Paddy3118]] 12:28, 8 May 2011 (UTC)

: I agree that failures in the general sense which are not failures to meet the task need to be treated differently from failures to satisfy the task.  Perhaps we could get a "bug but acceptable by task requirements" template to mark them?  --[[User:Rdm|Rdm]]

== ooRexx ==

'''Beautiful''', BUT
shouldn't

```rexx

::method "><"
  -- this is equivalent of "\=="
  forward message("\==")

```

be equivalent of "\=" 

and why not distinguish = and == ??
::method "="  -- this is equivalent of "=="  forward message("==") 

--[[User:Walterpachl|Walterpachl]] 07:18, 15 July 2012 (UTC)

==Why is it so complicated?==

This task is a nice example of why sometimes I deplore that programming languages are not as elegant as mathematics.  Normally what is simple in maths should be simple in programming, but it's obviously not.  I mean, quaternions are supposed to be defined as algebraic expressions of four entities 1, i, j, k such as 1 is the multiplicative neutral element and

<math>i i = j j = k k = i j k = -1</math>

I wish it was possible to just specify those identities as particular cases of a general Algebraic class or something, and be done with it.   I kind of hoped that Perl 6 would allow it, but when I look at the proposed code (which did not even worked last time I checked), I can't help finding it long and ugly.

--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 10:27, 2 May 2013 (UTC)

: One issue, here, is that humans are many orders of magnitude smarter than computers. So the information needed to convey a new system to a human is qualitatively different from the engineering needed to convey that system to a computer. 
: Another issue is that traditionally programming languages have been thought of as abstractions representing sequences of computer instructions ([some examples of] type theory[s], for example), or abstractions representing "core mechanisms" (scoping rules, for example). There has been some work based on modeling hardware behavior using mathematical concepts (array theory, for example), but that's not where most communities focus their energy.
: There are lots of infinities here and unless we are careful the computing platform will get bogged down, spending a bulk of the computational time on unnecessary issues. As an example, consider whether we should have a hermetian basis for quaternions or whether octonions are acceptable as an implementation of quaternions. Since we are people, and since we are smart, we have a concept of simplicity which kicks in when we understand something, but computers do not have that. [But this also means that if it seems complicated we do not understand it.] --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:16, 2 May 2013 (UTC)

Grondilu, you may consider [http://www.gap-system.org/ GAP]. You can define an algebra by specifying it's multiplication table: I added an example in the GAP section. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 20:24, 28 October 2013 (UTC)

==Imaginary parts (plural)==
I had to mark both jq and Julia as incorrect as they both had the imaginary parts of a quaternion defined as just i by their definition of <code>imag</code>. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:30, 10 August 2014 (UTC)

:I am confused - why should the internal details of the implementation constitute a problem if the results are correct? I spot checked the julia results and the examples I looked at looked fine. I have not studied the julia code, but it sounds like it might be using a [[wp:Cayley–Dickson_construction|Cayley-Dickson]] representation of quaternions - which would be completely correct. But I also have not studied those languages in depth, and I might have overlooked something important. Can you please go into more detail about what specifically is wrong here?  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:47, 10 August 2014 (UTC)

::Hi, Looking through the function names are came across image. It just seemed wrong to access only one of the three imaginary parts of a quaternion via that name - especially when the other imaginary parts seem to be adjacent indices away.
::Why the use of the name? What would imag be used for? It seems odd to single out I over j over k in such a way. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:49, 10 August 2014 (UTC)

: Paddy3118 - I don't doubt your good intentions, but there is nothing "wrong" or even misleading about the Julia entry.  (I am not part of the Julia team, but I do have extensive training in mathematics.)  

: First, the Julia entry plainly states that the code is part of the Julia library.  It is helpful to see the code as a whole -- that's what this website is all about!  Second, as rdm pointed out, since the library (taken as a whole) can be used to solve the assigned problems completely and accurately, it seems unnecessary and unwise to flag irrelevant details about naming as needing attention, letting alone claiming they are "incorrect". Third, the bit that offends you makes no claim that "imag(.)" returns all the imaginary parts -- indeed that would probably not even be desirable.  

: You might say that since the Julia library provides real(_), it ought to provide accessor functions for each of the imaginary components, but that should perhaps be the subject of discussion between you and the good people at Julia. I've checked the most recent Julia library, and the same accessor functions ("real" and "imag") are still defined there.  The choice of the name "imag" for the "i" component has four letters, just like "real", and is really unobjectionable. [[User:Peak|Peak]] ([[User talk:Peak|talk]]) 19:28, 10 August 2014 (UTC)

Hi Peak, imag looks like it was probably forgotten from extending imaginary numbers to their implementation for quaternions. It jq is based on the Julia entry, could you state its use? Maybe there is no use for this in Julia either? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:52, 10 August 2014 (UTC)

It seemed straight -forward to involve the [https://github.com/JuliaLang/julia/issues/7945 Julia library writers]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:08, 10 August 2014 (UTC)
: Moved to [https://github.com/andrioni/Quaternions.jl/issues/4 here]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:26, 10 August 2014 (UTC)
: Julia code on site (well one source), is fixed.


### Julia outdated w.r.t. imag?

Look at [https://github.com/andrioni/Quaternions.jl this link] to see a version from the Julia site that has fixed imag to be:
:<code>imag{T}(z::Quaternion{T}) = Quaternion(zero(T), z.q1, z.q2, z.q3)</code>
If jq was a copy ...
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:16, 11 August 2014 (UTC)

==Formula hidden to most browsers by under-tested cosmetic edits at 20:52, 6 July 2016 ==

Under-tested cosmetic edits made to the task page at 20:52, 6 July 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the formula for the '''norm of a quaternion''' completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of this cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:41, 22 September 2016 (UTC)
