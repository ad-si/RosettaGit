+++
title = "Talk:24 game/CSharp"
description = ""
date = 2010-04-29T16:50:09Z
aliases = []
[extra]
id = 7159
[taxonomies]
categories = []
tags = []
+++

==Use of XPathNavigator.Evaluate()==
I'm curious what the community thinks about leveraging the XPathNavigator.Evaluate() method.  Is this "bad" programming if there are no other comparable alternatives?

I'd personally favor the custom parser, but that's only because using the navigator feels like a hack.  A big plus for the navigator, though is that it is less complex from a client code perspective and is a proven block of code vs. writing a new custom parser. --[[User:Orrin|Orrin]] 23:30, 2010 April 28 (UTC)

:I'm ''not'' a C# programmer, but if it is a standard library that is officially supported then why not use it? It stops code bloat and is less to maintain. I did similar when using the in-built Python AST module [[Arithmetic_evaluation#ast_standard_library_module|here]]. --[[User:Paddy3118|Paddy3118]] 06:47, 29 April 2010 (UTC)
:I agree with Paddy, and extend his generalization to 3rd-party libraries; if that's what a good programmer in a language would use, that suggests to me that it's par for that language. --[[User:Short Circuit|Michael Mol]] 14:15, 29 April 2010 (UTC)
:Thanks, guys.  Guess that's what I expected to hear. --[[User:Orrin|Orrin]] 16:48, 2010 April 29 (UTC)
