+++
title = "Category talk:Programming paradigm/Concatenative"
description = ""
date = 2011-04-09T10:50:55Z
aliases = []
[extra]
id = 9424
[taxonomies]
categories = []
tags = []
+++

What belongs in this category?  For example:  Should Forth be in this category?  How about languages which can be used for Concatenative Programming but which allow (or even encourage) other styles of programming?  --[[User:Rdm|Rdm]] 14:24, 7 April 2011 (UTC)
:Treat it like the other programming paradigm pages. I guess we could say "if you can use this paradigm in the language and it doesn't go against the idioms of the language, it can be part of this paradigm category"? That probably deserves a vote. I'm no expert on whatever concatenative programming is, but it seems like Forth should be here based on the text on the page. --[[User:Mwn3d|Mwn3d]] 14:49, 7 April 2011 (UTC)

Do assembly languages qualify as concatenative? They tend to pass data on the stack, albeit taking multiple push and pop instructions. They also support pass by reference (do concatenative languages support this, or does this mean that the language is not concatenative?)

[[User:Markhobley|Markhobley]] 16:10, 7 April 2011 (UTC)

: Might [http://concatenative.org/wiki/view/Front%20Page this] help? --[[User:Paddy3118|Paddy3118]] 16:25, 7 April 2011 (UTC)

:: So, for example.. J is "almost a concatenative language".  It has two major departures from the [http://concatenative.org/wiki/view/Concatenative%20language fundamentals] currently described at concatenative.org:  J is right to left, instead of left to right.  And, composing J code requires something more than whitespace.  You need <code>[: code1 code2</code> instead of just <code>code1 code2</code>.  And, thus, you can get statements like <code>[: a [: b [: c [: d e</code>.  (A related issue is that many of the operations which would be useful on a stack might be of the form <code>(, code)</code>.  So... it's just over the edge of being a concatenative language... I think.  --[[User:Rdm|Rdm]] 11:51, 8 April 2011 (UTC)
:::I think you can overlook the direction of execution for this. You could probably easily translate J to left-to-right with a few simple compiler/interpreter changes. I think that property excludes things like lazy evaluation or that some parts of the code are evaluated out of reading order. E.g. in "(2 * 3) + (4 * 5)", "2*3" and "4*5" need to be evaluated first and then the "executor" needs to "go back" and evaluate the "+" but in something like Forth the code would look like "2 3 * 4 5 * +" where everything is evaluated as it is read. An example in C-style: "function1(3, function2(4, 5))", "function1" is read, 3 is read and evaluated, function2 is read, 4 is read and evaluated, 5 is read and evaluated, function2(4, 5) is evaluated, function1(3, function2(4, 5)) is evaluated. In a concatenative language, it would be 3 4 5 function2 function1, and all parts would be evaluated as they are read from left to right. In short: it means it uses [[wp:Reverse Polish notation|RPN]] or [[wp:Polish notation|PN]]. The other fundamental...I dunno :p. --[[User:Mwn3d|Mwn3d]] 20:43, 8 April 2011 (UTC)

:::: It doesn't exclude lazy evaluation; all that's needed is for the operators to be able to push value futures instead of (or as-well-as) absolute values. It's only when you hit something like IO (or other OS interaction) that you need to fix the value. –[[User:Dkf|Donal Fellows]] 10:50, 9 April 2011 (UTC)

----
I wrote the description for this category. The current description might not be good enough; other users might want to edit it. Among concatenative languages, I only use [[dc]] and [[Factor]]. I created this category for [[dc]], but someone else added [[Factor]] and [[Retro]] before I created this category. I later added Forth, GolfScript, Joy, Trith and V. --[[User:Kernigh|Kernigh]] 04:08, 9 April 2011 (UTC)

I am not sure if assembly languages have concatenative programming. When I tried assembly language, I never passed data on a stack; I always used general purpose registers (for PowerPC) or direct page (for 65xx). I am sure that one can pass data on a stack. Concatenative programming passes a stack from function to function. So the push or pull instructions would be functions that modify the stack. If assembly instructions are functions, then the concatenation of assembly instructions might be function composition. The branching or jumping instructions might not be functions, so their use (to make conditional branches or loops) might not be concatenative programming. If a program uses those instructions, one might argue that the program mixes concatenative programming with a different paradigm. --[[User:Kernigh|Kernigh]] 04:08, 9 April 2011 (UTC)
