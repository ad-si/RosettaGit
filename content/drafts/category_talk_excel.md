+++
title = "Category talk:Excel"
description = ""
date = 2015-11-24T19:04:56Z
aliases = []
[extra]
id = 19382
[taxonomies]
categories = []
tags = []
+++

Personally, I think Excel is a ''bona fide'' programming language.  When I worked for Microsoft Research a claim was made in a lab seminar, by a language theory researcher who shall remain nameless at present but is a leading in the Haskell community, that Excel was by far the most widely used purely functional programming language.

--[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 20:02, 11 July 2015 (UTC)

:Good day!

:Hmm... Excluding [[VBA]] from the topic, Excel's built-in functions/formulas can be considered a programming language. However, I think (just my opinion) Microsoft did not build/''made'' Excel as a programming language/IDE (since it is a spreadsheet program).

:If it is necessary, feel free to edit the category. Thanks! --[[User:Simple9371|Simple9371]] ([[User talk:Simple9371|talk]]) 03:02, 12 July 2015 (UTC)

:I half remember reading about how researchers in Conways game of life had arranged generators of gliders to interact and were thinking that by arranging starting positions they might theoretically produce a turing machine. Despite that, I wouldn't call Conways game of life a programming language and neither would most people call Excel a programming language. 

:In extremis, an expert can make a point by saying that a spreadsheet could be thought of as a functional programming language, but even they might concede that there point is made because it is an extreme view probably shocking their audience out of their complacency. 
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:14, 12 July 2015 (UTC)

::A fully working computer has been implemented in Life.  The URL is http://rendell-attic.org/gol/utm/index.htm and needless to say it's a complex colony!
::--[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 10:24, 12 July 2015 (UTC)

:::Thanks Brnikat. Nice to know they got a second version working too. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:27, 12 July 2015 (UTC)
::Regarding "Microsoft did not build/''made'' Excel as a programming language/IDE (since it is a spreadsheet program)": notice that Excel and Lotus 1-2-3, both spreadsheets, had a programming language consisting of commands written in the cells. It's still available in Excel 2013.
::Regarding Conway's game of life: funny example, it's fairly easy to build a CGOL emulator in a spreadsheet, with only spreadsheets functions, and relying on some evaluation order (that can be controlled by options, usually). Of course the grid of the game is made of spreadsheet cells.
::Actually, there is much more to spreadsheets than merely accounting functions, with some imagination. For instance, it's possible to sort a list with spreadsheet functions. It's probably not enough to say it's a "true" programming language, but the same can be said of LaTeX or SQL, and probably others on RosettaCode.
::And here, I'm not even considering replacing "temporal loops" by "spatial loops": copying a formula is like a "for loop", and can be used for instance to compute the solution of an equation, or to integrate a function. Of course, you have much more power if you use the solver, or VBA, or external DLLs (like LAPACK to compute eigenvalues for data analysis), combine all of this with your data to produce tabular or graphical output, everything within your spreadshet. Not too bad, for a ''not-a-programming-language''.
::Have also a look at the nice [https://newtonexcelbach.wordpress.com/ Newton-Excel-Bach blog].
::Oh, and of course, have a look at the Wikipedia article on ''[https://en.wikipedia.org/wiki/Programming_language programming language]'', to see why your conception of a programming language may just be wrong.
::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 10:47, 19 July 2015 (UTC)
:::Good day!
:::      As a noob in programming (and related stuffs), my perception about Excel (and Programming) must be constantly changing/improving. My bold edit here is somewhat my bad. (raising white flag...) I will now just say that Excel as a Programming Language must be verified further.
:::Again, edit the category if it needs to. --[[User:Simple9371|Simple9371]] ([[User talk:Simple9371|talk]]) 09:17, 20 July 2015 (UTC)
::::Sorry if I was a bit rude. Actually, you are right if you take a rather narrow definition of programming: say, Turing equivalence. I had a CS teacher in University who took this as the main distinction between a programming language and anything else. But then, LaTex is a programming language (proved to be Turing equivalent), and oddities like brainfuck are also programming languages. This theoretic approach is perfectly correct, but I prefer a more pragmatic approach: in my work (in statistics), I see no fundamental difference between writing a Python script, a SAS program, or a fairly complex Excel spreadsheet. Hence, among Wikipedia definitions, I prefer the broader: "a language used to write computer programs, which involve a computer performing some kind of computation or algorithm". Excel looks much different because there is no "source code" ''per se'', however you can express algorithms, with conditionnals and even loops, considering that copying a formula along a column, for example, provides mostly the same feature as a ''for'' loop. Now, you're free to prefer the narrower definition, but I still think spreadsheets should not be excluded.
::::On the other hand, there is another, much bigger problem: pictures are not very easy to manipulate on RosettaCode, and you have to use pictures to show spreadsheets (and even with pictures, it's not easy). I'll have to investigate this before I consider contributing Excel solutions to RC, which I would really like. I'll have a look at other graphical languages, like [[LabVIEW]]. I believe [https://en.wikipedia.org/wiki/Mathcad Mathcad] isn't even considered here. See also [https://en.wikipedia.org/wiki/Visual_programming_language Visual programming language] on Wikipedia!
::::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 21:41, 20 July 2015 (UTC)
Unfortunately there is a site limitation that does not allow images to be uploaded. This might affect how graphical solutions to problems can be shown on Rosetta Code. I did [[Knapsack_problem/Unbounded#OOCalc|this]] several years ago which is an oocalc entry.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:23, 12 July 2015 (UTC)



Excel is a programming environment, and writing formulae is definitely programming. The spreadsheet cells serve multiple purposes: storing data, storing formulae (source code), storing calculated values (variables), and displaying results. The environment provides a presentation layer, with display and formatting tools such as number formats, cell borders, drawing shapes, charts, etc. It is even possible to [http://www.felienne.com/archives/2974 build a Turing Machine using Excel formulae].

With all of these characteristics, Excel is clearly much more than just a calculator. It is a general purpose programming environment. Sure, the environment looks different to other programming environments, and it certainly has limitations. Nonetheless, writing spreadsheet formulae is programming.

--[[User:Bob Watson|Bob Watson]] ([[User talk:Bob Watson|talk]]) 19:04, 24 November 2015 (UTC)
