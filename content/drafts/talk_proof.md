+++
title = "Talk:Proof"
description = ""
date = 2012-06-07T21:32:56Z
aliases = []
[extra]
id = 2431
[taxonomies]
categories = []
tags = []
+++

The subtype Natural is pre-defined for all Ada implementations. Arithmetic upon that subtype is also pre-defined. This does not appear to be a very interesting problem in Ada. Any suggestions? --[[User:Waldorf|Waldorf]] 14:42, 21 December 2007 (MST)
:This seems like less of a computing problem, and more of a discrete math problem. I'm not sure if it's a good task. --[[User:Mwn3d|Mwn3d]] 14:45, 21 December 2007 (MST)
:The task was intended to demonstrate dependent types.  I've got some C++ code I'm working that demonstrates how it can be accomplished there. --[[User:Short Circuit|Short Circuit]] 20:50, 21 December 2007 (MST)

== Satisfaction ==

As it stands, this task is not very satisfying.  The Salmon "implementation" attests to that.  Perhaps the task should be extended to include some dis-proofs? --[[User:Rdm|Rdm]] 14:16, 28 May 2012 (UTC)

: The recent update to the task looks like an attempt to address this issue.  But it's defined as a proof of a negative concept that a rejection of an invalid proof.  That's not the same thing.  To prove a negative concept we need a "not exists" proof which is universally true.  The proof system accepts this proof which shows that the proof is valid.  This does not show that the proof system is capable of rejecting an invalid proposition.  --[[User:Rdm|Rdm]] 20:59, 30 May 2012 (UTC)

:: Right, it demonstrates a specific technique which allows to evidence the falsity of something. Your question sounds more like a question about whether used metatheory is [[wp:Consistency|consistent]]. There is [http://coq.inria.fr/faq?som=2#htoc7 an answer] on such a question. &mdash; [[User:Ht|Ht]] 02:48, 31 May 2012 (UTC)

::: The issue is not about the use of metatheory but is about the use of programming.  Currently, we have the Salmon implementation for the task, which does not correspond to code that I can run.  More generally, the task requires no machine output.  In my experience, the educational value of this site is significantly improved when short bits of machine output are included to illustrate the behavior of the code.  --[[User:Rdm|Rdm]] 13:04, 31 May 2012 (UTC)

:::: But all the languages which ​​are suitable for this task are based on some metatheories. Twelf is based on the [[wp:Logical_framework|LF]], Agda and Omega are based on the [[wp:Intuitionistic_type_theory|MLTT]], Coq is based on the [[wp:Calculus_of_constructions|CIC]] (which is variation of the MLTT), MLes and Haskell usualy based on polymorphic lambda calculus ([[wp:Hindley%E2%80%93Milner|HM]] or [[wp:System_F|SystemF]], however, they are ''not'' suitable) and so on. There is also other systems such as Mizar, MetaMath or ACL2 which are based on the set theory rather than on Type Theory.

:::: About the output, if the proof is correct, then there is nothing to report (or just report "proven"), but if it is not correct and contains errors then it does make sense to tell why the proof is not admissible. Once the proof is (silently) verified you can start deduce types and evaluate normal forms for terms, e.g. for agda <code>+-associative 1 2 3</code> deduced <code>6 ≡ 6</code> and evaluates to <code>refl</code> (witness of the equality).

:::: I don't know about Salmon, but all you can do in C, J, Tcl or whatever is [http://www.andres-loeh.de/LambdaPi/LambdaPi.pdf implement] dependently typed language or proof system, i.e. define syntax for terms and types (or terms, formulas and deduction rules) and write an unification algorithm on types and terms. &mdash; [[User:Ht|Ht]] 03:23, 1 June 2012 (UTC)

::::: For output: that was the point of asking for rejection of an invalid proof, so that there would be something to output.

::::: For J: I have restricted the vocabulary, and am rejecting all statements which go beyond that vocabulary.  I am only guaranteeing that if the statement is accepted that the proof is valid, I am not guaranteeing that the statement is invalid if it is rejected.  We do not need to create new syntax if we can reject syntax that would have otherwise have been valid.  --[[User:Rdm|Rdm]] 05:24, 1 June 2012 (UTC)

:::::: In J version <code>all</code> evaluates to

:::::: <code>0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81</code>

:::::: is this true that proofs verified only for these numbers? &mdash; [[User:Ht|Ht]] 12:13, 1 June 2012 (UTC)

::::::: No.  We are dealing with denotational semantics -- the above sequence is the data structure I am using to represent the natural numbers.  That said, these values are used in the subsystem that eliminates a kernel of the language which remains after I have imposed my syntactic constraints.

::::::: (I had started to build a system of guided lexical manipulations, based on exact subtree matches.  It's not necessary for the current task, but perhaps it would be more concise?) --[[User:Rdm|Rdm]] 14:23, 1 June 2012 (UTC)

:::::::: Ok, it (<code>all</code>) uses a "Self-Reference".

:::::::: Currently I can't understand this J code, nor the idea behind it. Did it use some special features? Can it be simply translated to, say, Scheme? &mdash; [[User:Ht|Ht]] 06:26, 5 June 2012 (UTC)

:::::::: It could be translated to Scheme, but a literal translation would probably not complete because J has a limitation on recursion depth (which I exploit here) which Scheme does not.   But the algorithm is trivially simple, so perhaps it would be better to translate it to english:

:::::::: 
```english
Define words:

zero: peano zero

successor: peano successor (defined on zero and on the result of successor)

equals: determines if two peano numbers are equal

all: represents all natural numbers, is implemented as a prefix of those numbers

is_member_of: determines whether members of a set which are members of all are members of another set which are also members of all.

exists_in: determines whether the intersection of two sets is non-empty

induction: given a list of names (which can represent any natural number) and a list of expressions, accepts or rejects the list of expressions based on these steps:

1) If an expression uses any name which has not been explicitly named here (including addition, odd and even), then the list of expressions is rejected.

2) If an expression is too long (longer than "all"), then the list of expressions is rejected.

3) If an expression is not a tautology then the list of expressions is rejected.

addition: given two lists of peano numbers, produces the cartesian product of the sums of all elements of those lists.

even: doubles each element of a list using addition to add (only) that number to (only) itself.

odd: finds the successor (to each member of the list resulting from) even
```


::::::: In other words, the constraints are: only the words defined for this task are allowed, and sentences using those words are limited in length, and any remaining sentences must be valid for all of the defined test cases.

::::::: And, yes, this is a weak system of axioms.  But no cases where this approach is insufficient are a part of this task as it is currently defined.  And proofs always assume that their axioms are correct (which is how we can get away with denoting an infinite sequence in the first place.)  --[[User:Rdm|Rdm]] 15:38, 5 June 2012 (UTC)

:::::::: With current version this holds:
::::::::
:::::::: 
```j
'A B' induction '(A addition A) equals (B addition B)'
'A B' induction '(A addition B) equals (B addition B)'
'A B' induction '(A addition A) equals (A addition B)'

```

::::::::
:::::::: &mdash; [[User:Ht|Ht]] 19:06, 5 June 2012 (UTC)

:::::::: And even 
```j
'A B' induction 'A equals B'
```
 &mdash; [[User:Ht|Ht]] 19:19, 5 June 2012 (UTC)

:::::::: With bug fixed version commutativity is broken: 
```j
'A B' induction '(A addition B) equals (B addition A)'
|assertion failure: assert
|       assert 0!:3 y
```


:::::::: &mdash; [[User:Ht|Ht]] 20:15, 5 June 2012 (UTC)

:::::::: Yes -- should be fixed now though.  Free variables are now distinct in the context of arithmetic (and relations) but are associated with a canonical (but arbitrary) ordering when generating results.  --[[User:Rdm|Rdm]] 21:17, 5 June 2012 (UTC)

== Scope of Task ==

Now, there is a problem with large expressions:


```j
   'A B C D' induction '(A addition (B addition (C addition D))) equals ((B addition (C addition D)) addition A)'
|assertion failure: assert
|       assert(#all)>#;._1 LF,y
```


I changed the code like this:


```j
add=: +/ dyadic
mul=: */ dyadic
even=: [: context kernel@add~"0@,@kernel :[:
odd=: successor@even

defined=: '(zero -. exists_in odd successor equals is_member_of add mul even)'
```


so that


```j
'A B C D' induction '(A add (B add (C add D))) equals ((B add (C add D)) add A)'
```


now holds (but it slow!), however:


```j
   'A B C D E' induction '(A add ((B add E) add (C add D))) equals ((B add (C add D)) add (A add E))'
|assertion failure: assert
|       assert 0!:3 y
   'A B C' induction '(A mul (B mul C)) equals ((A mul B) mul C))'
|assertion failure: assert
|       assert 0!:3 y
```


Also, why is <code># all = 82</code>? Or it is context-dependent? &mdash; [[User:Ht|Ht]] 23:06, 5 June 2012 (UTC)

: These are out of scope for this task, so I did not bother implementing support for them.  As you have seen, it is possible to support them without introducing any new concepts, but it gets inefficient.

: The sorts of examples you are presenting here could be addressed by tuning the hard constraints.  That 82 -: #all thing is one example of a hard constraint.  It exists in the current implementation because I think it's "cute".  But an explanations of my reasons is going to get verbose...

: First, J implements recursion using the C stack.  So it's limited to a size which is determined before the program starts running.  In this context, that limitation winds up being equivalent to about 6665 recursive instances.  It's just an arbitrary machine limitation (worth talking about, but that gets into the design intent of J, the character of mathematics and maybe a paragraph about peano arithmetic).  However, it's too big for my purposes here.  Using three free variables, I would need to perform 6665^3 tests to treat all possibilities represented in my kernel.  So I decided to use its square root (or rather, the next larger integer).  The problem with shrinking the kernel is that it's hypothetically possible to introduce expressions which are not adequately represented by the kernel.  Rather than thinking this through to try and find some sort of minimal valid kernel size, I imposed a flat limitation on the expressions I am supporting: they can have no more characters than the length of my kernel.  This limits the scope of the expressions I can treat, but -- in combination with my enumeration of supported operations -- it guarantees that I am not claiming provably false expressions are true.  And that is good enough for the current task.

: (As an aside: note that J supports alternatives to recursion (for example, it has an induction primitive -- which, ironically, I am not using here) which are not limited by the C stack.)

: So, anyways, this system has some flexibility in its tuning, but its cost is exponential in the number of free variables considered together.  I can reduce the base of that exponent by shrinking my kernel but at some point I would have to introduce syntactic manipulation.  The strategy I have been using in this task is explicit enumeration, so if I needed syntactic manipulation I would explicitly enumerate the syntactic transformations used (and quite probably I would also explicitly impose the order in which they are performed).

: But adding support for syntactic manipulation could double or triple the size of the program, and it's not relevant for the current task.  (And if you want me to discuss philosophical reasons behind this, I can, but that's probably best done in a separate subtopic.)  --[[User:Rdm|Rdm]] 14:08, 6 June 2012 (UTC)

:: For me it looks like the <code>printf</code> function which can print only the "Hello World" message, but nothing else. Of course, it can satisfy the "Hello World" task, but it is not right since the "Hello World" task not really intended for only that output, it covers all sorts of such tasks, i.e. printing text. This task is similar, although it asks something special, it assumes that one can prove many other theorems using the same methods as for theorems in the task.

:: For example, one can prove that the natural numbers with addition and multiplication forms a semiring; that there are infinitely many natural, even and odd numbers; that there are infinitely many prime numbers ([[wp:Euclid's_theorem|Euclid theorem]]); that linked lists with append operation forms a monoid; that the compiler produce a program which execution gives the same value as the value obtained in the interpretation of the same program; and so on (in fact, almost any constructive math proof). What is really required is possibility of forming sets, the way of defining operations on that sets and the way of proving logical/algebraic properties (including higher-order ones) about sets and its operations. CAS and proof systems allow to do all that (and they are not small pieces of software).

:: To make it clear, I look at this J code from the sideline, I don't really understand it, but when I see such inconsistencies I just started to doubt about the whole approach. Can you give some references? What is the "kernel" that you are implementing? It is consistent? &mdash; [[User:Ht|Ht]] 17:44, 6 June 2012 (UTC)

::: I disagree (obviously).  Most languages include a variety of techniques for displaying strings, and we do not expect to see all of them illustrated in a "Hello World" task.  We only expect to see one of them illustrated.  Strings are not a good analogy here, though, because most languages include extensive support for strings, and most languages do not include extensive support for proofs.  Also, historically, a proof has always been specific to a single theorem.  We do not expect that a proof for associativity of addition for peano numbers will be valid for complex numbers or quaternions -- we demand a separate proof.

::: You did find a bug in my earlier implementation, where it was inconsistent -- it was treating different free variables as identical, but I have fixed that bug.

::: Anyways, there's a reason that Knuth once warned:  "Beware of bugs in the above code; I have only proved it correct, not tried it."  It's trivial to find cases where systems proven correct via denotational semantics will fail in practice.  The axioms are not a valid reflection of the machine implementation, and a proof necessarily is only valid when its axioms are valid.  --[[User:Rdm|Rdm]] 18:50, 6 June 2012 (UTC)

:::: > Most languages include a variety of techniques for displaying strings, and we do not expect to see all of them illustrated in a "Hello World" task. We only expect to see one of them illustrated.

:::: Yes, we know that anything can be printed in conventional languages just as anything (what is truth) can be proved in theorem proving systems.

:::: > Strings are not a good analogy here, though, because most languages include extensive support for strings, and most languages do not include extensive support for proofs.

:::: It's a good analogy because I do not consider conventional languages at all. I am only talking about proof systems, in them we can show a simple proof and expect that one can prove any theorem the same way (e.g. [https://gist.github.com/1286093/8ff1afbc8b8f8aef051349eda19ccaec193c902d here] is a proof of Euclid's theorem in Agda). In weak conventional language, on the other hand, nothing can be proved, only proof system can be written.

:::: > Also, historically, a proof has always been specific to a single theorem.

:::: Historically has been almost a hundred years since the advent of formal logic (Boole, Peano, Dedekind, Hilbert, Frege, Russell, Whitehead, Quine, Cantor, Zermelo, Fraenkel, Löwenheim, Skolem, Gödel, Gentzen, Tarski works) in which theorems is just sequences of symbols derivable by rules of inference from axioms and a proof is an algorithm of deriving. And more than fifty years since the advent of сomputability theory and formulation of the Church-Turing thesis and the Curry-Howard correspondence (Church, Kleene, Turing, Gödel, Post, Rosser, Curry works). Type Theory is a further development. [http://video.ias.edu/voevodsky-80th Quoting] Voevodsky mathematical logic is just a combinatorial game, which, of course, can be implemented as software, as proof system suitable for proving any theorems.

:::: I remind that you have not explained [[wp:Proof_theory|what]] is the kernel that you are implementing. &mdash; [[User:Ht|Ht]] 15:44, 7 June 2012 (UTC)

:::: History goes back much further than 100 years.  But the basis of proof has not changed.  A proof is based on its axioms.  Meanwhile, the implementation of a theorem solver must be considered to be a part of the axioms.

:::: But there is no literature on the "kernel" concept, unless I get around to writing it (or perhaps unless you do so).  However it is, as you said, "just a combinatorial game".  

:::: That said, here's a quick overview: the kernel is a permutation of a prefix of the non-negative integers which is significantly longer than any representable feature of the mini-language.  It's paired with a second copy of the permutation which represents the symbol.  We guarantee that each free variable gets represented by a permutation different from any other free variable. Any single-valued functions are applied uniformly to the kernel and the symbol.  Any two-valued operations enumerate the symbols and find the cartesian product using that operation based on the argument kernels (and then we sort the symbols and transpose the dimensions of the cartesian product to match that sort).  When testing set membership we ignore any generated members (on either side of the set) which were not part of the original kernel.  Equality is an exact match test on the entire data structure.  Existence checks for any member of one generated kernel existing in the other generated kernel.   This works because we are dealing with a limited vocabulary -- our starting kernel is larger than any of the artifacts (non-uniformly treated prefix of the numbers) we can express using our language, which makes it infinite in the context of this limited language.  --[[User:Rdm|Rdm]] 20:56, 7 June 2012 (UTC)

:::: Note, by the way, that I have not thought through the issues involved in using more than one combining operation with this system.  It has not mattered for the current version of this task. --[[User:Rdm|Rdm]] 21:14, 7 June 2012 (UTC)

== Huh? ==

The data type for "natural numbers" is what would be called an "unsigned integer" in other languages, right? Or did you want us to create a data type that can deal with arbitrary-size integers? 

If we're merely talking about unsigned ints, then the data type for "even numbers" is of course the exact same thing (since the two sets are isomorphic). So the programming task boils down to writing functions for I/O that take an unsigned int and interpret it as "one half of the number we're talking about". Is that what is intended here? If not, then what exactly are we supposed to do here? You see me somewhat puzzled... [[User:Sgeier|Sgeier]] 17:45, 11 March 2008 (MDT)
: If you ignore the numbers given, the proposal still makes sense.  I think the task was intended as an exercise of type dependency with proof systems. This possibility is supported by the project pages for Coq, Agda2 and Twelf. (How is it that all the links on this page are uncreated, I wonder...) It doesn't look like an imperative problem.  It could be done in C++ with classes and operator overloading, for example.  The proof is whether or not the proper code compiles.  It looks like I'd intended to do that, but I recall life getting in the way that week.  --[[User:Short Circuit|Short Circuit]] 19:40, 11 March 2008 (MDT)

== Proofs ==

Does this task apply to languages which are not built as theorem-proving systems? If so, what constitutes an acceptable implementation of "prove that the addition of any two even numbers is even"? --[[User:Kevin Reid|Kevin Reid]] 00:07, 17 February 2009 (UTC)

: I'm thinking about implementing theorem proving system as an application in the language. This could be done with at least any Turing-equivalent language. Still unsure how that would be for this particular task.[[User:Avmich|Avmich]] 00:03, 12 November 2009 (UTC)
:: I did this in Haskell, but I dunno how easy it would be in other languages, especially dynamically-typed ones. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 18:11, 18 February 2010 (UTC)

== Clarification needed ==

The first line of the description reads: <cite>"Define a type for a sub-set of natural numbers (0, 1, 2, 3, ...) representable by a computer, and addition on them."</cite> Can somebody please clarify that last phrase, <cite>"addition on them"</cite>? I ''think'' I know what it's supposed to mean (and I'm ''fairly'' sure I understand the task as a whole) but just in case... -- [[User:Eriksiers|Eriksiers]] 22:24, 5 November 2009 (UTC)
:In mathematics, when we speak of "a binary operation on S", we mean a function whose domain is S × S and whose codomain is S. So "addition on the natural numbers" means an addition function that takes two naturals and returns a natural. —[[User:Underscore|Underscore]] 00:42, 6 November 2009 (UTC)
::Hmmm... I slept through most of my calculus classes, due to having '''THE MOST BORING TEACHER IN THE UNIVERSE''' (really) and as a result, most math above simple algebra I just don't get. I think I'll give this task a pass. -- [[User:Eriksiers|Eriksiers]] 15:05, 6 November 2009 (UTC)
::: But this is not much more than elemental type theory. Hardly as complex as the infinitesimal calculus... –[[User:Dkf|Donal Fellows]] 16:38, 6 November 2009 (UTC)
:::: I passed Calc 2, and it ''still'' took me a couple years and lurking in discussions of C++0x before I really started understanding what was being talked about in this task.  Type theory and calculus seem to me to be orthogonal subjects. --[[User:Short Circuit|Michael Mol]] 19:19, 6 November 2009 (UTC)
:::: Rightly or wrongly, elementary calculus is in most colleges a ''prerequisite'' for courses on basic proof-writing. I'm a math major and I ended up taking vector calculus before "foundations of mathematics". —[[User:Underscore|Underscore]] 20:34, 6 November 2009 (UTC)

== The Go proof? ==

The Go sample seems questionable.  It claims that successful compilation completes the proof, but what if I make some small changes to the code?

```Go
var zero number = e0.pn // see axiom 5, below.
var one = zero.successor() // I added this
....
func (s *evenPeano) hasEvenSuccessor() (even, bool) {
//    if s.normalize() == zero {  //original code
      if s.normalize() == one {   //I added this
        return nil, false

```
By the way, all the "has*Successor" probably should have been named "hasPredecessor" instead. Anyway, by the proof's logic, what was evenPeano is in fact "oddPeano" now, and it still compiles fine.  Does that just prove sum of two odd numbers are also odd numbers? --[[User:Ledrug|Ledrug]] 20:46, 21 June 2011 (UTC)

: On "hasSuccessor," yes, sorry, that does look silly.  I'll fix it soon.  On trying to prove sum of two odd numbers is odd, I think your edits are not quite enough.  Your declaration of variable one didn't create a variable of type evenPeano, so it doesn't quite work as an an even number.  one.evenSuccessor(), for example, won't compile, and nothing that can be passed to hasEvenSuccessor (sic) will ever compare equal to it.  Trying to hack this up more, though, we might try,

```txt

var zero number = &peanoNumber{}
var one = zero.successor()
var e0 = &evenPeano{one.(*peanoNumber), nil}

```

: This compiles, and now looks even worse because now one and all its successors counting by twos are of the type evenPeano.  What we're left with though, is a bug in in addEven.  It terminates with a sum when it gets to the first "even" number, one.  To get the correct answer, we still have to add this remainder of one, and this is now what we can't get to compile.  Trying a.successor() or a.add(p) returns a regular peanoNumber that won't match the return type of evenPeano.  The compile error is saying odd numbers are not closed under addition.

: I'll add that while I had fun with this task, I don't consider it much of a proof.  There's way too much hand waving for me.  &mdash;[[User:Sonia|Sonia]] 23:29, 21 June 2011 (UTC)

:: I'm sure it was a fun thing to write, but "not much of a proof" is the problem here.  I can write any kind of code to prove odd + odd = odd, and I can make it compile, but that proves nothing; only when I run the code, and do a test like "is 1 + 3 odd?"  The code will go 1 + 3 = 4-> pred 2 ->pred 0 (BOOM, hopefully, because first element is 1), so that's a counterproof, <i>but</i>, the key is, you <i>have to run your logic in the code to make a proof</i>, merely compiling means nothing.  If you aggree to that, now convince me your code can prove even + even = even: I won't likely live long enough to see your program exhastively test all even numbers.  See my point?  However fun it was with the axioms and writing the code, I just don't think this code counts as fullfilling the task. --[[User:Ledrug|Ledrug]] 23:43, 21 June 2011 (UTC)

: The task might be poorly worded and poorly named, but I think the examples posted by the task author and every example except Haskell show the same thing, which is using the language type system enforcement of closure over a type as an analog of mathematical closure over a set of numbers.  The code to demonstrate isn't the code that you write, but the code that is in the language translator.  &mdash;[[User:Sonia|Sonia]] 01:09, 22 June 2011 (UTC)

::I'm not sure what you meant by the last sentence (actually, I'm not sure what you meant by the sentence before that one, either).  As to other examples, Coq seems to be doing the right thing; TCL seems to be doing the right thing (or pretending to, I can't tell); Haskell seems to be doing a slight variant of the right thing.  Of the other ones, Adaga and Omega do seem to only define a set without verifying it's closed, if so they are incorrect, too, but I don't know enough about them to say for sure.  Unfortunately, I do know what the Go code does.

::It is capable of verifying (when run) if <i>any given instance of</i> a number is even, doing so by recursion to individual instances of predecessors, not induction based on definition, which is a big difference.  Your other argument, that if it passed compiler type checking, it must be consistent, is false: my bogus "oddPeano" passed it with gaping logic holes, and the compiler didn't catch it, because it's not supposed to.  True that if I read the code, I may say "that's not going to work, the hasPredecessor() will go past one and reach -1 and below", but a) I'm not the compiler; b) the task requires the program to prove, not "let Ledrug read the code and prove".

::Now if the we can change the task description to say "show by examples that such and such", your code is perfectly fine (again, if you run it).  As it is, though, it's not enough as a proof.

::Lest I look too harsh, I'm not trying to be overly righteous or trying to kill your fun; it just so happens that I think "prove" is not a verb to be used slightly. --[[User:Ledrug|Ledrug]] 02:09, 22 June 2011 (UTC)

The task description reads "Define a type for natural numbers (0, 1, 2, 3, ...) representable by a computer, and addition on them". This does not make any sense. I think this should read "Define a datatype for natural numbers and their associated additions that can be represented on a computer.". It still doesn't make any more sense, but hey the words look better. :) [[User:Markhobley|Markhobley]] 15:33, 13 July 2011 (UTC)

== Draft status ==

I suggest the task should be draft status.  The varied solutions posted and the resulting discussions here show disagreement on what the current wording requires. &mdash;[[User:Sonia|Sonia]] 20:33, 11 May 2012 (UTC)

: I think the task is quite clear: "prove that the addition of any two even numbers is even". ''Any''. It is not possible with bruteforcing, since there is a countable many even numbers. Nevertheless, a [[wp:Constructive_proof|constructive proof]] can be given in a suitable logical system (such as ACL2) or in a language with dependent types (Agda, Coq, Twelf to name a few).

: Putting something that looks like "2" in the CPU register, the same "2" in another, performing ADD, and getting something that looks like "4" doesn't help - that is not a proof! :) You need to ''define'' numbers, rules for arithmetic, induction, etc ''in'' the language based on a suitable [http://ncatlab.org/nlab/show/type+theory metatheory] such as [[wp:Intuitionistic_type_theory|MLTT]] or [[wp:Calculus_of_constructions|CoC]]/[[wp:Calculus_of_inductive_constructions|CoIC]], then prove. E.g., in the Agda version <code>even+even=even</code> takes ''any'' two even numbers and returns their sum as an even number, this is a ''type'', i.e. logical ''proposition'', ''algorithm'' itself is a ''proof'' by induction which ''builds'' a required term of a given (inhabited) type, and the typechecker ''performs'' that proof (so that this is compile-time verification). &mdash; [[User:Ht|Ht]] 02:44, 12 May 2012 (UTC)

::It seems a wothwhile task in the way you describe it here on the talk page. Maybe the task description needs similar expansion to remove J-like entries and require "induction solving capabilities" or people to create such in a language without it, then apply it to the problem at hand. --[[User:Paddy3118|Paddy3118]] 07:22, 12 May 2012 (UTC)

The recent rewrite of the task description makes my point.  Much of the wording, and the new requirement about showing associativity are one day old. &mdash;[[User:Sonia|Sonia]] 17:17, 12 May 2012 (UTC)

: Apart from the small additional subtask, I don't think this task has been changed in any essential way since [http://rosettacode.org/mw/index.php?title=Proof&oldid=11669 2007]. &mdash;''[[User:Ruud Koot|Ruud]]'' 18:09, 12 May 2012 (UTC)

:: Wrong.  It's full of additional restrictions.  In [http://rosettacode.org/mw/index.php?title=Proof&oldid=102003 2011] even, it said nothing about dependent types.  These new restrictions are approaching a task description that says something like "languages must have built-in support for at least one of (loosely related) features x, y or z.  all others must omit."  But the task isn't clear yet on what those features are. &mdash;[[User:Sonia|Sonia]] 18:39, 12 May 2012 (UTC)

::: I think it was as clear in 2007 what this task was supposed to be about as it is now ''to someone who has had some previous exposure to proof assistants''. The only problem here is effectively communicating this to programmers who do not have such a background. The letter of task description might have changed a bit, but its spirit certainly hasn't. 
::: One of the problem with stating the description precisely is exactly because it should ''not'' exclude solutions in HOL or Isabelle (they are not based on dependent types) or perhaps even a very creative solution using C++ template programming. &mdash;''[[User:Ruud Koot|Ruud]]'' 18:49, 12 May 2012 (UTC)

== Haskell ==

Looks like GHC can typecheck the same expression as in the Omega/Agda version. ?an not guarantee the totality, however. &mdash; [[User:Ht|Ht]] 02:52, 12 May 2012 (UTC)

: Does it satisfy the task?  Hard to say due to existing inconsistencies in the task description.  The description starts out saying it only makes sense for dependently typed languages and proof assistants.  It later wants to allow Haskell  for implementing System-F.  Is type checking enough, and if so, must it be run time or can it be compile time?  My understanding is that dependent typing, by definition, is done at run time and that Haskell does the type checking at compile time.  Does a clean compile constitute a proof? &mdash;[[User:Sonia|Sonia]] 18:18, 12 May 2012 (UTC)

:: "Yes, but..." The solution requires two languages extensions (GADTs and type families) that together allow one to express some dependent types. They are not as expressive as full dependent types, but are powerful enough to express this rather simple theorem and its proof.
:: Your understanding of dependent types is wrong: the typing is not done at run time, but rather code is executed at compile time. Haskell cannot execute arbitrary code at compile-time, but type checking type families and GADTs effectively does require some execution at run-time.
:: The "but" is that fact that Haskell allows general recursion, making the logic unsound, i.e. any theorem - including false ones - can be proven by an expression that would diverge at run-time. The given solution does not use this "feature", however.
:: This is a solution that the task should clearly allow, with the appropriate footnotes, though. &mdash;''[[User:Ruud Koot|Ruud]]'' 18:35, 12 May 2012 (UTC)

== Quantifiers ==

The currently phrasing of this task places emphasis on "any" (pair of even natural numbers) and on "every" (use of addition between those numbers).

But the task also requires that the language implement the natural numbers so represented.

This is either a contradiction (since every language can only implement a finite set of distinct numbers and there are an infinite number of "Natural Numbers") or a new use of the term "Natural Number" which I am not familiar with (in which case the required definition should be included in the task description).

--[[User:Rdm|Rdm]] 19:44, 13 May 2012 (UTC)

: Put differently: the task asks for a countable set of natural numbers.  This would be trivial, except that the peano postulates do not hold for a countable set.  --[[User:Rdm|Rdm]] 20:19, 13 May 2012 (UTC)

: The set of natural numbers is countably infinite. They are defined by a finite formal systems (the Peano axioms). A proof about a property holding for all natural numbers can be given in a finite form (using induction). You may indeed not be familiar with these facts, but they are most certainly not novel. &mdash;''[[User:Ruud Koot|Ruud]]'' 20:41, 13 May 2012 (UTC)

:: Ok, that's ambiguous terminology.  I agree that "countable" in the mathematical sense means aleph null style infinity -- "has a one-to-one correspondence with natural numbers".

:: However, in a computer program, an infinity of values cannot be implemented -- it can be symbolized, but that is different from implementing it.  A computer program can only implement a finite set of distinct values.  Thus, in the context of a type -- when using the normal meanings of "countable" and "type" -- natural numbers are not countable.

:: That said, if you really mean for a program to implement a "countably infinite" type, that is itself a failure in specification.  At the very least, you should be obligated to say what you mean by "type" when the usual meaning of type in programming refers to something which can only enumerate a finite set of values without failure.   

:: My current inclination is to believe that you treat some "failures" as "valid for purposes of proof" in your implementation of types.  --[[User:Rdm|Rdm]] 21:05, 13 May 2012 (UTC)

::: Your persistent assumption that computers incapable of working with "infinities" is wrong. When a mathematician wants to prove a theorem about the natural numbers he will not proceed be trying out the theorem for each natural number until the heat death of the universe.Instead, he will give a finite proof. A computer is equally capable of this. 
::: Inductively defined types (e.g. a linked list) have an infinite set of values. (An implementation will generally be limited by the word size of the underlying machine, but a proof should be independent of this and work for an arbitrary word size.)
::: Might I suggest you read a good book about the foundations of mathematics? Perhaps my terminology will seem less ambiguous to you after this. &mdash;''[[User:Ruud Koot|Ruud]]'' 21:33, 13 May 2012 (UTC)

:::: You have overgeneralized my statement.  Overgeneralizations are, generally speaking: wrong.

:::: The issue I raised was not "computers are incapable of working with 'infinities'".  The issue I raised was "computers can only represent finitely many distinct values.  By leaving out that "distinct" part, you ignored my entire point.

:::: It seems, though, that you are saying that you accept as valid a type system where an infinity of the symbolized values are not implemented by the implementation -- where only finitely many values symbolized by the type are implemented.  --[[User:Rdm|Rdm]] 21:42, 13 May 2012 (UTC)
::::: Then read my above sentence as "... have an infinite set of distinct values". The "distinct" was already there implicitly (all elements in a set are always distinct), so the statement remains equally true.
::::: I'm not exactly sure what you mean with "accept as valid a type system where an infinity of the symbolized values are not implemented by the implementation", so I can't say if I agree with that or not. As a proof by induction of even + even = even does not rely on actually computing any particularly large objects, it is quite irrelevant if the implementation has any limitations of what the maximum size of the finite representation of these infinite models can be. &mdash;''[[User:Ruud Koot|Ruud]]'' 22:12, 13 May 2012 (UTC)

::::: Typically a type, in computer programming, symbolizes a set of values which may validly be represented in the computer program.  If the set is infinite, however, the validity requirement vanishes.  --[[User:Rdm|Rdm]] 22:27, 13 May 2012 (UTC)
:::::: Not even wrong... I don't think it will be fruitful to continue this discussion. &mdash;''[[User:Ruud Koot|Ruud]]'' 22:41, 13 May 2012 (UTC)
::::::: Typically "not even wrong" indicates a lack of testability.  It's not clear how that is relevant here.  I am guessing, though, that you disagree with me about the meaning of the word type.  But if that were the case I think you should present your own definition.
::::::: My suspicion though, is that in this case "not even wrong" means the same thing as "right".  --[[User:Rdm|Rdm]] 00:21, 14 May 2012 (UTC)
:::::::: Most of your comments here consist semantically empty rambling. I therefore cannot possibly give a genuine reply to them, they are so incoherent I cannot even state what is wrong them, ergo "not even wrong". &mdash;''[[User:Ruud Koot|Ruud]]'' 13:00, 14 May 2012 (UTC)

::: > However, in a computer program, an infinity cannot be implemented

::: Infinity cannot be implemented in math either, we can only say "infinity" which is the finite statement. Every [constructive] mathematical proof is always finite and just represents a way of manipulation of finite symbolic information (the mathematical terms) that can be done in finite time (one can take a piece of paper and try for the even + even = even prop.). There is nothing surprising in the fact that symbolic information can be represented as types and terms in programming languages ​​(in almost any starting with C, because everywhere there are data types and their constructors), and the proof itself can be represented as a terminating algorithm of unification (e.g., type checking, but not everywhere, only in languages ​​with enough expressive type system).

::: Yes, linked lists is a countably infinite data type, this means that there is a countably many linked lists that can be (potentially) constructed. In contrast, the boolean data type has the cardinality = 2. &mdash; [[User:Ht|Ht]] 07:33, 14 May 2012 (UTC)

:::: This is trivially false, since every computer has a finite amount of memory -- even if it's virtual memory -- and linked lists must fit in memory or the links are invalid.  At most, the number of linked lists which can be represented is  2^N where N is the number of bits of memory including virtual memory (N might be limited by the OS to a value smaller than wordsize * 2^64 in a computer with 64 bit addressing).  Now, I will agree that that is a large number, which might distract you from noticing that it's a finite number, but it's still finite.  And even if you changed from a parallel bit implementation to a serial bit implementation and addresses were represented by a sequence of 1 bits (the first 0 terminating them), which would let you have arbitrarily long addresses, you still run into issues like the lifetime of the universe (or, more practically, the funding which keeps your computer system running).  It's still not infinite, except in your imagination.  --[[User:Rdm|Rdm]] 01:44, 14 May 2012 (UTC)

::::: > This is trivially false, since every computer has a finite amount of memory

::::: Real computers are not even Turing complete, that is why the word "potentially" was used. But this is not a problem because of [[wp:Mathematical_constructivism|constructivism]]/[[wp:Finitism|finitism]].

::::: Just as you can prove `even + even = even` (for ''all'' natural numbers) by using a finite amount of paper and in finite number of steps, computer (proof system) can do this using a finite amount of memory in a finite time.

::::: > the number of distinct linked lists which can be represented has a limit which cannot exceed the factorial of 2^N where N is the number of bits of available memory

::::: No, usually sizeof(struct DList) for some DList type is not equal to the machine word size. Also, what if the machine has [[wp:Physical_Address_Extension|PAE]], what about other real world restrictions? Of course, it's absolutely not important, you commenting a minor point. The real question is why you think that statements about natural numbers like `even + even = even` can't be proven with computer using proof systems and keep talking about unrelated discrete math stuff. This task is for peoples who believe in finitism and [[wp:Mathematical_proof|mathematical proofs]] (the correspondence between programs and proofs is [[wp:Curry%E2%80%93Howard_correspondence|well known]]). &mdash; [[User:Ht|Ht]] 07:33, 14 May 2012 (UTC)

:::::: (I fixed my phrasing there.)  1. You are talking about smaller limits, here, and 2. you are introducing new terminology (e.g. "DList") without bothering to define it, and making less general statements.  In other words, these are meaningless objections -- the point is that there is a limit.  This means, in the context of this task, that there will be even numbers which cannot be added associatively because they cannot be added.  This leaves us with two options: either (a) the type system is incomplete and does not represent this failure mode, (b) the type system does represent this failure mode.  Traditionally, static type systems use (a) and run-time typing is needed for (b).  One common failure mode (e.g. two's complement, fixed width) violates the peano axioms for approximately half of the representable combinations of values.  Another common failure mode (inductive representation or variable width) also poses an additional constraint where time needed to complete the operation increases as the values increase in size.  This second failure mode can be made so burdensome that it disguises the first failure mode, but that's not always a good thing.  My problem, here, is that this task currently seems to be asking for a type system with no failure modes.  Of course we know that any actual implementation of the peano axioms must eventually fail to implement some cases, but -- because of earlier objections here -- I am afraid that if I present an implementation that has either of these failure modes that people will object to it.  So I am asking that this task requirement be made explicit.  --[[User:Rdm|Rdm]] 10:08, 14 May 2012 (UTC)

::::::: "the point is that there is a limit" - of course (this was my minor comment on minor comment for the minor point), but a type always has it cardinality (i.e. the cardinality of the corresponding set, for example, aleph-null for natural numbers or finite strings), this is unbeatable, it does not matter how many terms can be constructed physically. "This means, in the context of this task" - this means nothing in the context of this task. Again: just as you can prove `even + even = even` (for ''all'' natural numbers) by using a finite amount of paper and in finite number of steps, computer (proof system) can do this using a finite amount of memory in a finite time. Using the same axioms and deriving rules! In the Agda version there is no numbers which are directly tested for the statements of the task (well, only the first natural number is verified directly), the computation is done in a general form over terms and types. Imagine the following, you have the formulas, you have a finite set of axioms and inference rules, there is a way to apply these rules to formulas to obtain new formulas, starting with the axioms. So using only terms of finite size, and applying a finite number of rules you can deduce the truth of the `even + even = even` statements in a suitable metatheory. Proof systems do just that, they do not directly test the statements for some finite set of test data. The proof itself is a compile-time type checking (in the case of languages with dependent types). Not all the math, but some formal mathematical systems is definitely systems that can be implemented: their symbolic data (the "math" itself) can be implemented, the reasoning in such systems can be implemented and so on. We can trust in proofs that helds in such systems, as well as we trust in "ordinary" proofs. &mdash; [[User:Ht|Ht]] 12:56, 14 May 2012 (UTC)

:::::::: I think that it's fair to say that the failure of the implementation to satisfy the properties which were proved for the type means nothing to you.  If you ever care to become aware of counter example, I suggest you spend some time visiting the [http://catless.ncl.ac.uk/Risks risks forum].  Nevertheless, I shall endeavor to create an implementation which fails in this fashion.  It's a shame that you feel this concept is too complicated for you to explain for it to be included in the task description.  But I shall propose this change, myself.  --[[User:Rdm|Rdm]] 12:17, 14 May 2012 (UTC)

::: See also this link: http://okmij.org/ftp/Computation/uncountable-sets.html.

::: > natural numbers are not countable

::: Finite sets is countable too (well, depending on the terminology, see the wikipedia entry on the countable sets).

::: > My current inclination is to believe that you treat some "failures" as "valid for purposes of proof" in your implementation of types

::: By "you" you mean anybody who "believe" in Agda/​Coq/​Epigram/​Twelf/​ATS/​NuPRL/​ALF/​DML/​LEGO/​HOL/​Isabele/​ACL2/​Mizar/​MetaMath/​etc. proofs? Did you believe in 4-color theorem proof? Did you reject implementations of the homotopy type theory in Coq/Agda? &mdash; [[User:Ht|Ht]] 07:33, 14 May 2012 (UTC)

:::: If your description here is correct, then these implementations of homotopy type theory have weaknesses in their treatment of upper bounds.  No big deal, perhaps, except that you seem rather intolerant of other implementations with the same kind of limitation.  --[[User:Rdm|Rdm]] 01:53, 14 May 2012 (UTC)

::::: [http://ncatlab.org/nlab/show/homotopy+type+theory Homotopy type theory] originally appeared as the discovery of connection between homotopy theory and Martin-Löf type theory, and one of the interesting points consisted in the fact that an implementation of type theory (e.g. Agda or Coq) can be used as the language for homotopy theory. Needless to say that most of the proofs in HoTT are held for potentially infinite structures and higher-order types.

::::: "Upper bounds" makes sense for brute-forcing "proofs" but the real proof systems uses unification... Well, it's still required to get the point. Talking about the "upper bounds" is not even funny :) &mdash; [[User:Ht|Ht]] 07:33, 14 May 2012 (UTC)

:::::: And that's fine, until you start asking people to implement these infinite concepts as types.  To use your turn of phrase, that's "not even funny :)".  --[[User:Rdm|Rdm]] 11:53, 14 May 2012 (UTC)

::: My current inclination is to believe that you cannot get [http://www.cs.nott.ac.uk/~txa/publ/ydtm.pdf the point] of dependently-typed programming languages / proof systems. &mdash; [[User:Ht|Ht]] 23:00, 13 May 2012 (UTC)

This will be my final attempt to clarify this to you (please grab a book or article on type theory if you want to know how mathematicians and computer scientists define a type): The only implication of the fact that an actual implementation of a given logic/type theory runs on a machine with a finite amount of memory is that the set of results the compiler can give is extended from "I was able to verify the proof as correct" and "I was able to verify the proof as incorrect" with "I'm unable to verify the proof as either correct or incorrect because I ran out of memory". The third option must not occur here. The compiler should say "I was able to verify the proof as correct", assuring us that the property even + even = even hold for all countably infinite pairs of natural numbers.

Similarly if the theorem asked us to proof that reversing a linked list twice gives us back the original linked list, we should be able to prove this. This proof should hold independently of whether the machine has an 32-bit, 64-bit or even unbounded word size. Having a finite amount of memory will not prevent a computer working with infinities, it will only bound the maximum complexity of the proof - exactly similar to the situation where a mathematician only has a finite amount of paper to work on (and I've yet to meet a mathematician with an infinite supply of paper).

I'm strongly getting the impression that you've never managed to work out a proof by induction on paper, however, making this beyond you abilities to comprehend. In that case either do the appropriate background research or simply accept this on authority. &mdash;''[[User:Ruud Koot|Ruud]]'' 12:04, 14 May 2012 (UTC)

== Deletion of statement about finite limits from task ==

(I moved this from in the middle of some other comments to its own subtopic.)

: Your last addition:

:: Note also that it's expected that the implementation of the type will fail to satisfy the peano axioms for some values. It's only required that the implementation succeed for values which do not exceed some non-trivial finite limit. The proof is not meant to apply to the implementation for values which exceed that limit.

: The "implementation of the type" is the implementation of Martin-Lof type theory (or its extension), [http://intuitionistic.files.wordpress.com/2010/07/martin-lof-tt.pdf the paper] has a model of natural numbers similar to the system of Peano axioms. &mdash; [[User:Ht|Ht]] 12:56, 14 May 2012 (UTC)

:: This is not searchable text, it's photographic material and it uses a variety of definitions defined in other places.  So it's difficult, at a glance to see if it adequately treats the topic of machine limitations in an implementation.  If any of this is relevant to the topics of limits of validity, please restate the relevant constructs in your own words.  --[[User:Rdm|Rdm]] 13:17, 14 May 2012 (UTC)

::: This is a seminal early paper in the field of Type Theory (the capital T one, i.e. dependent type theory) and proof assistants. It's indeed not intended to be glanced over, but read thoroughly. &mdash;''[[User:Ruud Koot|Ruud]]'' 13:46, 14 May 2012 (UTC)

::: The section about natural numbers is on page 71. That is, the Peano axioms is embeddable into MLTT (even constructive ZF set theory is embeddable), so that the questions should be asked about MLTT itself. There is some free books - [http://www.cs.kent.ac.uk/people/staff/sjt/TTFP/], [http://www.cse.chalmers.se/research/group/logic/book/] (see indexes for examples, e.g. the associativity of the append operation on lists from [2]). &mdash; [[User:Ht|Ht]] 14:06, 14 May 2012 (UTC)

:: By extension [[User:Rdm|Rdm]] believes that because mathematicains only have a finite number of sheets of paper they have only been able to prove theorems about the natural numbers which hold up to some non-trivial finite limit. I'm not sure how to argue with someone who is so confused about elementary concepts. &mdash;''[[User:Ruud Koot|Ruud]]'' 13:11, 14 May 2012 (UTC)

::: This is trivially false, in the sense that I have stated nothing about a relationship between sheets of paper and proofs.  Please refrain from making up stuff about me. --[[User:Rdm|Rdm]] 13:17, 14 May 2012 (UTC)

:::: The problem is indeed that you fail to see the relation between sheets of paper/memory and proofs about infinite objects. You only need a finite amount of the former to do the latter. &mdash;''[[User:Ruud Koot|Ruud]]'' 13:22, 14 May 2012 (UTC)
::::: Please don't insult other users of RC; it's uncivil and will simply result in action being taken against you. You should be able to conduct discourse here without stooping so far.
::::: In general, when reasoning about infinite constructive structures (e.g., the natural numbers), you show a number of statements that are true and use mathematical induction to extrapolate to the whole infinite structure. The tricky bit is that induction is a second-order operation, and it's often easiest to have it be an axiom; the proof then consists of building the model and showing the base cases and inductive steps are true, since the assembly of that into an overall proof follows trivially from the axiomatization. I forget whether MLTT works that way though; back when I was active in the theoretical CS area, I focused far more on modal and (especially) temporal logic (which have their own special problems). –[[User:Dkf|Donal Fellows]] 09:03, 15 May 2012 (UTC)
