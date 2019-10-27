+++
title = "Talk:Solving coin problems"
description = ""
date = 2014-04-27T16:32:26Z
aliases = []
[extra]
id = 13505
[taxonomies]
categories = []
tags = []
+++

==Maxima in Perl?==
Hi, could you explain the use of maxima please? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:10, 16 May 2013 (UTC)

Thanks for your feedback and question. I've consolidated my answers to this question and the question on "Solving the resulting translation" in the section below on "Solving the resulting translation." --[[User:2Powers|2Powers]] ([[User talk:2Powers|talk]]) 08:04, 18 May 2013 (UTC)

==Where is the AI?==
For me AI implies an algorithm which learns. At its sipmlest it could try to answer the question asked; ask if the answer is right or wrong; if wrong ask for more information which might help it do better; try again; repeat until it gets the answer right.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:32, 17 May 2013 (UTC)

"Where is the AI?": AI does not necessitate an algorithm which learns. Russell and Norvig define AI as the study and design of intelligent agents, which are in turn defined as systems that perceive their environment and take actions to modify their environment to achieve a goal--a thermostat is an example of an intelligent agent--one that doesn't learn. What you are are describing is called "machine learning" which is only one branch of AI. Daniel Bobrow's original STUDENT implementation had the ability to query the user if it could not identify enough information to solve the problem, and it could accept new rules from the user at runtime. But it was incapable of formulating new rules itself. Whether or not this qualifies as "machine learning" is debatable. But either way, STUDENT is still an AI system. Norvig's implementation of STUDENT in PAIP does not have the ability to query the user; if it were run at the Lisp REPL, a Lisp programmer could dynamically add new rules. This, however isn't essential to the task. Personally, I would classify this AI system as an example of "machine translation"--one of the major tasks of natural language processing (NLP). Specifically, it calls for translating a specialized English vocabulary (i.e., "coin problems") to another specialized vocabulary (i.e., a system of mathematical equations). --[[User:2Powers|2Powers]] ([[User talk:2Powers|talk]]) 08:04, 18 May 2013 (UTC)

==What is the grammar?==
This program doesn't learn, it is required to translate an english like sentence into algebraic equations. The synatax for this english like sentence is described only by a large number of regular expressions in the Perl implementation, and perhaps in a book which everyone may not have access to. Should we not specify that grammar fully in the task description. The task, as it stands, could be solved just by wrapping all these regular expressions in the language of your choice, which seems a little mindless.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:32, 17 May 2013 (UTC)

"What is the grammar?": In the sample Perl, I've provided 28 examples of "coin problems" found in high school algebra text books. The core task is to devise a system capable of translating these english language texts into systems of mathematical equations.  So I can't simply "provide the grammar"--that is really the crux of the task. Whether or not there's even a Chomskyan transformational grammar that encapsulates all "coin problems" is a separate philosophical issue. There are two main paradigms for machine translation: (1) rule-based systems and (2) statistical systems. Statistical machine translation is the current fad. Bobrow's 1964 STUDENT program was a rule-based system. Simply providing Bobrow's original rules won't do any good here--his original rules were not capable of solving any "coin problems." Hence it makes a good candidate for Norvig's challenge to extend Bobrow's rules sufficient to solve problems in another domain. --[[User:2Powers|2Powers]] ([[User talk:2Powers|talk]]) 08:04, 18 May 2013 (UTC)

==Solving the resulting translation==
Rosetta Code has tasks for parsing and solving RPN and infix problems. Could we not use these instead of Maxima? So the task would be to translate whatever syntax we define into RPN or infix?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:32, 17 May 2013 (UTC)

"Maxima in Perl?" and "Solving the resulting translation": Solving the equations is auxiliary to the core machine translation task--this is the reason I left that to MAXIMA. Bobrow's original program and Norvig's implementation both code their own "solvers" based on constraint propagation. Norvig's is quite limited and can only solve for one variable. Most "coin problems" found in high school algebra texts require solving a simultaneous system of linear equations for two or three variables, so Norvig's implemenation just won't work here. If there is a module here on rosettacode for solving a system via guassian elimination, determinants, matrix inversion, or any other method, then that would be completely acceptable. Once the core program translates the english problem description to a system of equations, that system can be solved using any method whatsoever, e.g., a Java implementation could call the Java Algebra System (JAS) or it could call Mathematica's Java API. As long as it gets the correct answers to the original english input, anything goes. The only thing I'm not requiring in this task is translating those results back into english language sentences for output--something Bobrow's STUDENT program did. --[[User:2Powers|2Powers]] ([[User talk:2Powers|talk]]) 08:04, 18 May 2013 (UTC)

:How about modifying the task for RC by:
:# Cutting down on the number of examples.
:# Outputting an equation to solve so a solver is not needed (and not likely to be present).
:? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:49, 18 May 2013 (UTC)

=Possible Candidate for Major Revision/Deletion=
This (draft) task does not appear to be attracting implementations, indicating that it is not serving this site's purpose of being a way to compare languages via their solutions to some task. Is the task too large? Should it be split up or deleted? We want drafts to be able to gain four (preferably independent) implementations so that the task can graduate, but dumping a tricky problem and a large slab of complex code that solves it (in Perl, no less!) in is not a great way to achieve this. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:32, 27 April 2014 (UTC)
