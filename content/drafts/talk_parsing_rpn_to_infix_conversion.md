+++
title = "Talk:Parsing/RPN to infix conversion"
description = ""
date = 2016-09-20T10:01:34Z
aliases = []
[extra]
id = 10994
[taxonomies]
categories = []
tags = []
+++

==Other examples==
The Ruby Quiz article mentions the following two additional examples:

```txt
    $ ruby postfix_to_infix.rb '56 34 213.7 + * 678 -'
    56 * (34 + 213.7) - 678
    $ ruby postfix_to_infix.rb '1 56 35 + 16 9 - / +'
    1 + (56 + 35) / (16 - 9)
```

--[[User:Paddy3118|Paddy3118]] 20:19, 3 December 2011 (UTC)
== Examples Incorrect ==
Several examples (i.e. Python, and TCL) don't deal with associativity correctly - the code doesn't even refer to the attribute - it's defined and not used.  I'm not sure of the Ruby code.  The following "1 2 + 3 4 + ^ 5 6 + ^" should produce "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )".  --[[User:Dgamey|Dgamey]] 04:03, 17 December 2011 (UTC)
: It's actually a matter of precedence rather than associativity.  RPN per se doesn't need to worry about if an op is left or right associated, but for infix notation brackets are needed for ops at same precedence level. To wit: <code>1 2 3 + -</code> is <code>1 - (2 + 3)</code>, not the same as <code>1 - 2 + 3</code>. Generally the easiest way is wrap the expression with parens whenever you pop a binary op off the stack, which will be unsightly, but correct: <code>1 2 3 4 5 + + + +</code> becomes <code>(1 + (2 + (3 + (4 + 5))))</code> --[[User:Ledrug|Ledrug]] 05:49, 17 December 2011 (UTC)
:: Yes of course the RPN is correct.  It's the generated infix that is wrong. As the examples I looked at don't even reference associativity when generating the infix nor do they generate parenthesis every time then they must be wrong. At least one of these parsing tasks wanted minimal parenthesis - it would have to be this one.   The one example given just isn't a case where you'd notice.   --[[User:Dgamey|Dgamey]] 14:58, 17 December 2011 (UTC)
::: Recommendation: Replace the example case with one that would exhibit the problem, or add a second example case which would exhibit the problem. Require that the RPN and infix forms expressions evaluate equally. --[[User:Short Circuit|Michael Mol]] 15:33, 17 December 2011 (UTC)
:::: I added an extra example, now sadly all the tasks are wrong (although I think a couple already address this).  The task already requires the RPN and infix to be the same.  One thing, I can't find the template that says (globally) the task changed.  So I'll have to mark them all incorrect if I can't find one. --[[User:Dgamey|Dgamey]] 16:49, 17 December 2011 (UTC)
::::: I added {{tmpl|clarified-review}} for you. –[[User:Dkf|Donal Fellows]] 19:06, 17 December 2011 (UTC)
:::::: Thank you! --[[User:Dgamey|Dgamey]] 01:48, 18 December 2011 (UTC)
:: Huh, if you want minimum parentheses and follow the [[wp:Associativity|actual definition]] of associativity, the task itself is incorrect: - and / are non-associative.  The term "left-associative" is often loosely used when parsing infix notations, to determine the order of ops in absence of parens, but to do the reverse and requiring minimum number of parens, this kind of definition of associativity is not enough, and you need to know the exact behaviour of the operators (a + b + c requires none, but a - b - c may need one pair, even though both + and - are both loosely "left associative").  The task needs some more work, or there may be other holes after people try to fix the solutions. --[[User:Ledrug|Ledrug]] 17:50, 17 December 2011 (UTC)
::: I just realized that we are not using the same terminology at all.  You've been talking about the '''associative property''' which is different from '''[[wp:Operator_associativity]]'''.  This would tend to speak to slightly different backgrounds mathematics .vs. computer science (or at least parsing).  The task description was originally correct from this perspective.  But clearly not from the mathematical definition!  The dangers of terminology overload. Ouch! --[[User:Dgamey|Dgamey]] 22:34, 19 December 2011 (UTC)
::: I didn't write it, I just noticed it was broken.  Perhaps it should be knocked back to draft. --[[User:Dgamey|Dgamey]] 18:20, 17 December 2011 (UTC)
:::: By adding the extra example in a table other examples could be added easily. Although for output, perhaps suppressing the detail for all but one case would be appropriate.   --[[User:Dgamey|Dgamey]] 18:19, 17 December 2011 (UTC)
::: Sorry I thought what you were talking about is if an operator is commutative?  --[[User:Dgamey|Dgamey]] 18:19, 17 December 2011 (UTC)
:::: The RPN code shouldn't try to use commutativity, as that causes problems in situations where there are side-effects. (OK, not in ''this'' example, but in general.) While a few languages allow optimizers to use it (notably [[C]] and [[C++]]), most don't because it causes much confusion; overall, language designers have found it better to be more predictable than that (where side-effects are present at all, of course). –[[User:Dkf|Donal Fellows]] 18:59, 17 December 2011 (UTC)
:::: Hmm? I didn't say anything about commutativity.  It has nothing to do with this task.  What's of concern is the execution order of operators, that is, given some arbitrary operator *, if <code>a * b * c</code> needs parentheses to be unambiguous. If * is associative (in the strict sense), <code>(a * b) * c = a * (b * c) = a * b * c</code>, and no parens are really required; commutativity is completely independent of this.  Subtraction "-" is not commutative, while exclusive-or "xor" is, but neither is associative.  It wouldn't have mattered had there not been the (rather pointless) "minimal parens" requirement, but as it is, some clarification is needed as to what constitutes "minimal".  Currently for example the Tcl code would convert <code>1 2 3 + +</code> into <code>1 + (2 + 3)</code>, which is fine by me--but I'm normally not the one who demands the output to match some random standard to the last letter. --[[User:Ledrug|Ledrug]] 20:51, 17 December 2011 (UTC)
::::: As I said, I didn't write the task.  I just pointed out that it was broken.  --[[User:Dgamey|Dgamey]] 01:48, 18 December 2011 (UTC)
::::: Hmmm, my definition of minimal in this case may not match the authors or yours but <code>1 2 3 + +</code> would be <code>1 + 2 + 3</code>.  Which is both minimal (in my mind), and I think non-associative, which I would have thought arouse because <code>+</code> is commutative.  Without belaboring the finer points I'd like to see it be both equivalent from infix and rpn and unambiguous.  --[[User:Dgamey|Dgamey]] 01:48, 18 December 2011 (UTC)
:::::: Formally, <code>1+2+3</code> is the infix version of <code>1 2 + 3 +</code>, as addition is ''left''-associative in this exercise (the “normal” model for programming languages). Knowing that <code>1 2 3 + +</code> is also a way of writing that requires knowing that addition is ''fully'' associative, and I suspect that most people aren't going to model that. I know I don't intend to. (To confirm what I say, use the inverse of this task to parse the string; the type of structure it produces — or equivalently the RPN form it generates — is precisely determined by the defined associativity and precedence rules, with not mattering in this case.) I suppose we could implement all the code to allow your intuition to not be wrong, but then it wouldn't be a parsing/formatting task, but rather a general symbolic mathematics task. We ''must not'' conflate (A•B)•C with A•(B•C) when parsing! I believe that any task that generates A•B•C from both of those is necessarily wrong; after all, we've not described the differences between “+” and “-” to the computer, and the difference matters critically there. (What's more fun, with some type systems even “*” and “+” are not truly associative; [[C]] has such a type system, as do the plethora of languages derived from it, and it can be the source of many subtle bugs.) –[[User:Dkf|Donal Fellows]] 02:42, 19 December 2011 (UTC)
::::::: My intuition really didn't come into it. Each RPN should have to generate only one infix in a parsing task. And as I said before, I didn't write the task.  I just noticed that it was broken with respect to "^" and that the output from different inputs into other tasks could not be recreated. To me it was obvious because several of the code examples defined but did not use the associativity information. That was my only complaint. --[[User:Dgamey|Dgamey]] 04:48, 19 December 2011 (UTC)
:::::::: Yeah, ''that'' was a bug that needed an extra test case. :-) –[[User:Dkf|Donal Fellows]] 09:18, 19 December 2011 (UTC)
: Tcl version is now fixed (it would have helped if the original example had been sensitive to such things, but there you go). –[[User:Dkf|Donal Fellows]] 18:59, 17 December 2011 (UTC)

== On Minimal Parenthesis ==
After finding out the discussions on associativity (above) were working from different definitions, this discussion about parenthesis in the solution may make more sense than it did previously.  The removal of the "minimal" from the requirement may have opened up the task to solutions having parenthesis everywhere.  While this is certainly unambiguous, it is also just as certainly ugly. I'm not sure this meets the original authors intent and I wish they would chime in here one way or another. --[[User:Dgamey|Dgamey]] 22:48, 19 December 2011 (UTC)

== New Input examples ==
See 'Examples Incorrect' above.  An example was added to show that associativity is correctly handled. --[[User:Dgamey|Dgamey]] 16:49, 17 December 2011 (UTC)

==Negation==
The table of operators needs to inculde an operator for negation. Should -4 8 ^ mean -(4 ^ 8) or (-4) ^ 8.
--[[User:Nigel Galloway|Nigel Galloway]] 12:52, 2 April 2012 (UTC)

:In RPN the negative number has no space between the minus sign and the first digit. Subtraction as an operator has spaces around the sign. --[[User:Paddy3118|Paddy3118]] 16:06, 7 May 2012 (UTC)
:I agree that that describes the prn side, but the problem is on the infix side. Infix notation assigns a higer priority to exponentiation than to unary negation. If I use the perl implementation:

```txt

>rpn.pl
-4 2 ^
-4^2

```

:Then I use the generated infix in perl:

```txt

>perl
print -5 ** 2
^Z
-25

```

:The correct infix is (-5)**2. The same is true for infix in Ruby and Python:

```txt

irb(main):001:0> -4 ** 2
=> -16
Python 3.2.2 (default, Sep  4 2011, 09:51:08) [MSC v.1500 32 bit (Intel)] on win
32
Type "help", "copyright", "credits" or "license" for more information.
>>> -4 ** 2
-16

```
--[[User:Nigel Galloway|Nigel Galloway]] 14:19, 11 May 2012 (UTC)

== Extraneous Requirements ==

The requirement "Show how the major datastructure of your algorithm changes with each new token parsed" is entirely extraneous to the core task, and, I think, its inclusion goes against the principle mission of Rosetta code. For reasons outlined in [[Rosetta Code:Village Pump/Extraneous Printing Requirements in Tasks]], I think we should make this requirement optional. If no one objects after several days, I will make this minor change.
