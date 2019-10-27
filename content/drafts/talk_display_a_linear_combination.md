+++
title = "Talk:Display a linear combination"
description = ""
date = 2016-11-21T15:38:01Z
aliases = []
[extra]
id = 19661
[taxonomies]
categories = []
tags = []
+++

== Wat? ==

There's a variety of ways of implementing linear combinations, and this particular task is requiring a representation which will not be executable in many languages. So it would be good to add some motivation to the task description. Why would someone want to do this? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:15, 13 October 2015 (UTC)

:I was working on my [https://github.com/grondilu/clifford Perl 6 Geometric Algebra library] and I needed some way of displaying a multivector.  By default I had an output like this one:


```txt
MultiVector.new(blades => (my Real %{UInt} = 0 => 1, 4 => 1, 10 => 3))
```


:Which is the output from <math>1 + e(2) + 3*e(1)*e(3)</math>, and it is clearly not satisfying.  I wanted an output that looks like the input.  The more I thought about it the more I was annoyed by the idea that in order to have something that looks good I would have to make sure I don't display the scalars if they are 1, and that I use a subtraction where appropriate.  It was looking like quite a hassle,so I thought: « why not make a rosetta task so that I can steal the solution from others? »  :-)

:As a matter of fact soon after I created the task I realized I could get a simple solution by using string substitutions on the output, so I was actually able to write the first solution in Perl 6.

:I still think it's a useful task, as it can be used for instance to display complex numbers or quaternions.  In perl 6 for instance the output for complex numbers is not perfect.  i is displayed as '0+1i;', which to me is acceptable but not so great. With [https://github.com/Util/Perl6-Math-Quaternion/ Math::Quaternions] it's much worse and arguably not acceptable.

:--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 22:09, 13 October 2015 (UTC)

:: Sure, it's useful, but the syntactic constraints are also tailored towards perl6 - so that should at minimum be mentioned in the task description (and ideally would also have been in the page name). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:17, 13 October 2015 (UTC)

::Is it really [targeted at Perl]? It looks like something a functional language or Lisp would be pretty good at and the Tcl solution is nice. --[[User:Craigd|CraigD]] ([[User talk:Craigd|talk]]) 23:30, 13 October 2015 (UTC)

::: Here's how the -1, -2, 0, -3 example might look if it rendered to lisp: <code>(+ (- (e 1)) (* -2 (e 2)) (* -3 (e 4)))</code>. Here's how it might look if it rendered to forth: <code>1 e 0- 2 e -2 * + 4 e -3 * +</code>, and  there are a good variety of other possible renderings also. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:42, 13 October 2015 (UTC)

:::I'm not sure what you mean when you say that the syntactic constraints are tailored towards perl6.  An expression like <math>a + b - 2c</math> is not language specific.  It's how you write a linear combination in standard math.  Let me remind you that this task is about ''output'', not ''input''.  If I'm not mistaken your predilection language, J, as an elaborate syntax and I'm sure there are plenty of ways to create a linear combination, but a human friendly, and mathematically conventional way of displaying such linear combination is quite universal.  In fact in Perl 6 we could use any syntax we want since we could create a slang or something.  I've just picked one that corresponds to the way we write algebraic expressions in maths, regardless of the programming language.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 23:54, 13 October 2015 (UTC)

:::: Ok, so that's what it is. In that case, the spec should still be documented in the task description - it's just not perl6 like I thought it was. Of course, you are hindered by the fact that "Standard Math" isn't very standard - it was created just a few years ago, and is a rather slippery collection of concepts also. I tried finding a definitive definition, and the best I found was an inexact common core document with a 2015 copyright on it. So most older math texts won't be using it, for example and it's difficult to talk about. (I expect the part of the definition you are using has been around a few years, but not a few decades. And I expect there's some corner of standard math which states that a linear combination like you are describing here could be expressed as (-1,-2,0,-3)*e or something not too far from that - with the summation being implicit.)

:::: Still... do the best you can. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:41, 14 October 2015 (UTC)


==Formulae left invisible to most browsers by edit of 21:43, 10 July 2016==

Cosmetic edits made to the task page at 21:43, 10 July 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left most formulae in the task description completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:48, 21 September 2016 (UTC)

: Repaired – visibility now restored. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:38, 21 November 2016 (UTC)
