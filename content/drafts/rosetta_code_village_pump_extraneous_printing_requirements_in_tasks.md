+++
title = "Rosetta Code:Village Pump/Extraneous Printing Requirements in Tasks"
description = ""
date = 2018-04-26T09:12:00Z
aliases = []
[extra]
id = 21098
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Extraneous Printing Requirements in Tasks
|summary=Several tasks require printing incidental output, which distracts from the purpose of the task and needlessly inhibits idiomatic solutions in certain paradigms.
}}
The aim of Rosettacode is "to present solutions to the same task in as many different languages as possible, to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another". However, a number of tasks include extraneous printing requirements which make it impossible to construct elegant, idiomatic solutions in certain paradigms: these tacked on requirements limit our ability to write facilitating and interesting comparison between relevant similarities and differences of certain languages; it also needlessly complicates the comparison of different approaches to core problem. 

Examples can be seen on several parsing tasks:

* [[ Parsing/RPN to infix conversion ]] requires that one "Show how the major datastructure of your algorithm changes with each new token parsed."
* [[ Parsing/RPN_calculator_algorithm ]] requires that one "Create a stack-based evaluator for an expression in   reverse Polish notation (RPN)   that also shows the changes in the stack as each individual token is processed as a table." 

In both cases, the core task, which is of interest in its own right, is muddled up with requirements to print or log the process by which the task is accomplished. This makes it impossible to solve the task in clean, idiomatic code in languages that carefully contain impure operations, or that don't support easy stringification of data structures. As a result, solutions to these tasks in, e.g., Mercury, Haskell, SML, OCaml, Elm, Agda, Idris, Clean, Curry, Coq, etc., will either fail to satisfy the extraneous criteria, or mimic impure imperative solutions, meaning they won't accurately reflect the way the language is, and should be, used. I suspect this problem is not isolated the two examples I've provided.

I'm not sure how we can best improve this circumstance, but two easy solutions occur to me:

# Edit the requirements on tasks with this defect to make the extraneous printing optional.
# Move such pages to subpages, like <code>Parsing/RPN to infix conversion/with logging</code>, where the logging requirement is added on a supplemental task to the main task.

(1) seems easiest to implement and least invasive.

Would it be okay with people if I were to go ahead and make these edits? In that case, should I first propose these edits in the discussion pages for each task, or is this (as it seems to me) an obvious enough improvement that I can just go ahead and make the change?

:# There have been many cases where it took the mental unraveling of the code to find that the code did not actually perform the task. 
:# Some languages show an ease in generating output when compared to others.
:# I would not support such blanket altering of established tasks as new examples would no longer be solving the same thing.
