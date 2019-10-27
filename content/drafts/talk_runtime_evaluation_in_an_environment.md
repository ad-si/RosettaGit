+++
title = "Talk:Runtime evaluation/In an environment"
description = ""
date = 2010-02-06T13:15:11Z
aliases = []
[extra]
id = 4113
[taxonomies]
categories = []
tags = []
+++

==Why marked as incorrect?==
This task '''needs to be clarified'''. The first part says something, that seems almost clear and generic.
The points make it obscure, or better, they make the task redundant, since we are in the domain of first-class function, lambda calculus, or block closures; all treated already elsewhere (and the name "eval in environment" seems misleading). It is also unclear why Perl is not marked, while PHP is... they seem the same (and also Tcl).

* it means it must look like code, not inside a string...: block closure (à-la Smalltalk), or just "lambda" calculus if you prefer the name. (Python contains a string, so it is wrong too) ??
* x must be a dummy name... again: block closure or similar ??
* "x" (dummy var) is local (inside the "block"); of course... ??

What about? --[[User:ShinTakezou|ShinTakezou]] 00:06, 30 April 2009 (UTC)

The first point means that you're not supposed to write e.g. (in JavaScript) eval("function (x) { return " + providedSourceCode + "}") (unless there is no other way). It does *not* mean that the parameter should be a block/closure/function object; the reason I didn't write "string" is to allow for parsed-but-not-evaluated code objects such as exists in E and Lisp.

x should not be global in the sense that the values given to it are visible to code ''other'' than the code being evaluated. In the Perl example, the variable is made local (lexical) by <code>my</code>; in the PHP example I (perhaps mistakenly) assumed that an undeclared variable is global. If that is wrong, please delete the flag on the PHP example. --[[User:Kevin Reid|Kevin Reid]] 02:01, 30 April 2009 (UTC)

Oh, I just saw your changes. The Octave example is now correct (and I like the variety of using a list/vector), but the Tcl example was better in the previous version -- the problem with it then was that the proc eval_twice should specifically bind the variable name x, not call a function with an arbitrary local variable name. --[[User:Kevin Reid|Kevin Reid]] 02:21, 30 April 2009 (UTC)

: Sorry but I can't understand why (maybe my Tcl is not so strong). Isn't it the same of PHP, Perl, Python, Ruby....? (I.e.: using a function). The only difference, is that it explicitly says which is the "dummy" var (the lambda var or how we want to call it). Maybe the example is more similar to the second Lisp example, since that is the way in Tcl to express a lambda expression (as far as I know/understand; going to find more about right now). --[[User:ShinTakezou|ShinTakezou]] 10:26, 30 April 2009 (UTC)
: (Modified now in order to resemble to other examples) --[[User:ShinTakezou|ShinTakezou]] 10:40, 30 April 2009 (UTC)

Yes, your new change to Tcl looks like what I had in mind. Is x a global variable, though?

Thanks for putting up with my requirements fiddling. --[[User:Kevin Reid|Kevin Reid]] 10:55, 30 April 2009 (UTC)

: No, it isn't global, it is local to the proc (to be sure, I've tested it adding a <code>set x 678</code> before the puts, and a <code>puts $x</code> after the puts; the value of x remain unchanged). --[[User:ShinTakezou|ShinTakezou]] 12:29, 30 April 2009 (UTC)
