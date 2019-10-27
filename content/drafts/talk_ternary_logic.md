+++
title = "Talk:Ternary logic"
description = ""
date = 2013-03-03T06:48:13Z
aliases = []
[extra]
id = 10387
[taxonomies]
categories = []
tags = []
+++

== test case ==
re: "Kudos (κῦδος) for actually thinking up a test case algorithm where ternary logic is intrinsically useful, optimises the test case algorithm and is preferable to binary logic".

I know that calculating [[Perfect numbers]] and [[Matrix-exponentiation_operator]] in ''binary'' has some algorithmic advantages.  I imagine that there is some problem would benefit from ''Ternary logic''.  Any hints or suggestions?

[[User:NevilleDNZ|NevilleDNZ]] 07:21, 26 August 2011 (UTC)

: On first reading, it seems like you are implementing an analogue of the cmp function from C and C based languages. Or am I completely misunderstanding?  --[[User:Thundergnat|Thundergnat]] 11:03, 26 August 2011 (UTC)

: It's entirely possible (and probably κῦδος-worthy) to produce a terminating solution of the Halting problem in ternary logic; anything where your code can't definitely figure it out within some reasonable bound on effort becomes a “maybe”. We know we can do this: we can exhibit examples with minimal effort. –[[User:Dkf|Donal Fellows]] 12:45, 8 September 2011 (UTC)
:: Certainly -- the simplest implementation would be code that always returns "maybe".  Going deeper would require an implementation which handles code in the language being used (which would have greatly varying complexity, depending on the target language).  --[[User:Rdm|Rdm]] 12:49, 8 September 2011 (UTC)

==Task structure==
How about adding a truth table for implementation and cutting the history/leaving a link to the history?--[[User:Paddy3118|Paddy3118]] 08:29, 26 August 2011 (UTC)

Just added the ''Logic Operators'' in ''Truth Tables''.  [[User:NevilleDNZ|NevilleDNZ]] 11:33, 26 August 2011 (UTC)

== C example ==

Could have been better (I could have bothered to do a table), but I hope the C example is sufficiently and approriately clear, fast and clever. Hopefully, I'll get around to implementing the C++ version using templates. --[[User:Short Circuit|Michael Mol]] 17:23, 26 August 2011 (UTC)

== if a then b ==

What is if a then b condition?  It's not a straightforward conditional like <code>if a then: b; else:a;</code>.  Looks more like if we define false < true, and "maybe" means "unknown state of either true or false", the "if a then b" condition is the same as a <= b.  Some clarification? --[[User:Ledrug|Ledrug]] 01:07, 27 August 2011 (UTC)

: '''if a then b''' is logically equivalent to '''(not a) or b'''. --[[User:Kernigh|Kernigh]] 01:27, 27 August 2011 (UTC)

:: That's equivelant to a <= b, I was more curious where the "if then" notation came from. --[[User:Ledrug|Ledrug]] 01:29, 27 August 2011 (UTC)
:::It started as plain old "implies" probably. Then something had to be used for "maybe". I'd say ask whoever came up with [[wp:Many-valued_logic#Examples|this]]. It certainly wasn't invented for this task. --[[User:Mwn3d|Mwn3d]] 03:06, 27 August 2011 (UTC)

I included the operator ⊃ and ≡ as (for some reason) they are part of the original [[wp:ALGOL 60|ALGOL 60]] specification. (Strange but true!) But in the case of binary the ⊃ operator does not seem so useful, AFAIK I have never used it (any got a nice/useful example of usage).  But in the case of Ternary logic ⊃ seems to be useful in tautologies so I included it.  Indeed @Kernigh is correct to say '''a ⊃ b''' is logically equivalent to '''(not a) or b'''.

(Said with my tongue firmly in my cheek: Maybe binary is the reason the computers are so black and white, and (at least on Star Trek and Dr. Who) is it the reason that tautologies make the front panels of computers spout smoke. c.f. http://www.youtube.com/watch?v=w3fyKChJrZw LOL :-^ )

Note: ⊃ is the same as → and ⇒ operations. c.f. [[wp:List of logic symbols|List of logic symbols]]

Also ⇔, ≡ and ↔ are "equivalent", '''and'''  equivalent to each other and to '''if and only if''' and '''iff'''.

BTW: the operators /\ and \/ were originally part of the [[wp:C programming language|C programming language]], they appear to be borrowed from the ALGOL ∧ and ∨ operators.  Indeed the character "\" was included in ASCII for this specific purpose.

[[User:NevilleDNZ|NevilleDNZ]] 04:04, 27 August 2011 (UTC)

== Any need for this task to remain draft? ==

By example count, diversity and clarity, it seems like it's reasonably well-understood. --[[User:Short Circuit|Michael Mol]] 19:19, 7 September 2011 (UTC)

== Variable_truthfulness ==

I feel the C [[Ternary_logic#Variable_truthfulness|Variable Truthfulness]] code warrants it's own Task Page.  This is because the task:
* Is sufficiently different to warrant a different page.
* Is not <u>Ternary logic</u> ''per se'', rather it might actually be a '''super set''' of some form.
* Would benefit from some citations and/or link to actual theory.
* Needs to go through "draft" phase to determine an appropriate test case.
* Is sufficiently interesting to warrant a different page.

[[User:NevilleDNZ|NevilleDNZ]] 09:57, 10 October 2011 (UTC)
: Well, for now it doesn't have a good place to go to.  As to not being ternary logic per se, it can be reduced to one pretty easily though: define 1 as TRUE, 0 as FALSE, and anything in between as MAYBE, then the operator tables can be reproduced exactly.  Since the "variable truths" are just [[wp:Probability|probabilities]], I'm not sure what more refs are needed without making it appear more complicated than necessary.
: This C entry was sort of targeting the kudos part of the task.  To me, the most interesting place to use a "maybe" would be at the control flows (<code>if3</code>), but that requires knowledge of ''how'' ambiguous such an ambiguity is, hence the numeric values. --[[User:Ledrug|Ledrug]] 14:51, 13 October 2011 (UTC)

<s>I suggest: A good place for it to go to is as a "Draft Task" with appropriate details of what the task is rather then complicate [[Ternary_logic]]. Also: A brief description of "variable truths" (or even [[wp:Qubit|Qubit]]s) might help the draft task.</s>  On rethinking, your point about the ''"C entry was sort of targeting the kudos"'' is about right.  Indeed I have yet to see a more pratical use for Ternary Logic.  ¢  However - maybe - [[wp:Balanced_ternary|Balanced Ternary]] Arithmetic appears to have an advantage, as Binary Addition is takes 58% more iterations, and Binary Multiplication takes 150% more iterations then Balanced Ternary. ¢ 

[[User:NevilleDNZ|NevilleDNZ]] 09:16, 14 October 2011 (UTC)
: Eh which C code are we talking about? This topic was about using floating point to represent probability (or so I thought), so the natural thing is using range 0 to 1.  I don't see how -1 got in here? --[[User:Ledrug|Ledrug]] 11:56, 14 October 2011 (UTC)

==another ternary truth table==

I found another ternary truth table that I used for validation of some of the other functions for the REXX example (which shows more functions) at: 
::: http://www.scribd.com/doc/78370674/31/A-6-2-AND-XOR-OR-XNOR-NAND
'''Ternary Computing Testbed 3-Trit Computer Architecture'''   by   Jeff Connelly

Computer Engineering Department       August 29<sup>th</sup>, 2008

which is on page 59:     A.6.2.      AND, XOR, OR, XNOR, NAND

 -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:48, 3 March 2013 (UTC)
