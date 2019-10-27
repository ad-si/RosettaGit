+++
title = "Talk:Polyspiral"
description = ""
date = 2017-04-01T18:40:21Z
aliases = []
[extra]
id = 20438
[taxonomies]
categories = []
tags = []
+++

==J implementation.==

Rdm said:  "I think this is a translation of the Java implementation. Perhaps, though, the task description needs to be more specific?"

: The result looks exactly right to me. I don't want to be more specific about input values because I'd like people to try different possibilities. Is there anything else that you'd like me clarify? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 11:40, 9 March 2016 (UTC)
:: If it looks right, I guess the task description was good enough for me to understand... That said, there are a variety of ways that this display could be animated, as well as a variety of other parametric differences. If your intent is to not be concerned about them, I guess I'll wait for other people to take action or comment. For now, it looks like you have already trimmed that comment I made. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:46, 9 March 2016 (UTC)
::: The pseudocode did contain some errors. I also made it more detailed. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 12:21, 9 March 2016 (UTC)

==PARI/GP and gnuplot implementations.==
My comment in PARI/GP is:
"This is definitely not a polyspiral, but a very nice "multi-spiral" figure similar
to shown in zkl and in a few other languages. Also, there is a very nice and impressive
animation created in zkl, but not possible in PARI/GP."
* 1. Only J and PARI/GP (and now gnuplot) have real polyspiral pictures.
* 2. Java, Lua, and zkl - producing not a polyspiral.
I thought: What's wrong?? Not sure, but we had only 5 languages?! 

'''Just a few remarks:'''
* 1. Basic definition is correct and enough: A Polyspiral is a spiral made of multiple line segments.
* 2. '''Pseudo code''' is always '''"like <some language>"'''. Some real languages have no operators like
FOR - ENDFOR, WHILE - ENDWHILE, or have 2 dozens of different statements, etc. 
E.g., Gnuplot has no such operators. So, it's better to avoid pseudo code.
* 3. In some languages animation is not only unpractical, but impossible. It's good to see last remark in the task.
* 4. Actually, only J and PARI/GP (v.#1) demonstrate real Polyspiral, all others show very nice multi-spiral
figures. May be, because they were following a pseudo code? Sort of the "side product" of a pseudo code?

Note: I would like to keep all these implementations in this task. I'm not against them.
May be, additional task phrase could help? E.g., like: "Strange, but nice figures containing spiral(s) are welcome..."

<<Note this part above was written last year, but I kept it on hold, not publishing.>>


<<second part 2017>>

* 5. In gnuplot, as you can see, I've plotted both polyspirals and a very smooth spiral (without visible "line segments").
After I've got in a few minutes a very smooth spiral I've 
spent a lot of time to create a polyspiral with visible "line segments". In fact, only "plot" statement
was changed slightly. You can compare yourself (see PS0 and PS1)..
* 6. While searching for "true" polyspiral in gnuplot I made the most important discovery:
All images produced in this task by all languages '''are formally polyspirals''', but most 
of them are not looking as a spiral at all.

E.g., looking at the picture PS4gp.png you still can see clear a polyspiral. 
But next two: PS5gp.png and PS6gp.png are not looking as a spiral at all.
PS5gp.png is the most popular picture produced by majority of languages.
 
All these images produced in gnuplot with the same code! Only numbers for range are different.

'''Conclusion'''

It is better to keep any task very simple, do not describe/require extra details.
The good sample is Sierpinski triangle task. Why? Because it is simple and allows to use 
any of existing multiple approaches to produce triangle.

May be, it's a good idea to add in the task description something like "[polyspiral] or any other spiral-related figure". --AnatolV 20:22, 11 January 2017 (UTC)
