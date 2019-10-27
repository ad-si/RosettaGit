+++
title = "Talk:Elementary cellular automaton/Infinite length"
description = ""
date = 2014-03-23T22:23:11Z
aliases = []
[extra]
id = 17431
[taxonomies]
categories = []
tags = []
+++

==Edge bits==
(about ignoring cells beyond the edges) But you can't stick to a simple version, and infinite padding beyond the edges must be included somehow.  Suppose the visible part of the cells ends with "...110" and is followed by repeating 0s to infinity, and rule is just 1 (000->1). Now after one iteration those three cells become "000", but followed by repeating 1s instead.  You can add another constraint that padding/edge cells don't change, but that would make the "infinite" part kind of pointless because changes won't propagate beyond edge cells. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 17:54, 21 March 2014 (UTC)

:Is this the only reason you marked Python wrong? (You actually added it to the Perl section).
:If so then it may be in error as the task states the meaning of infinite as being:
::''In other words, to describe the state of the automaton, you need a finite number of adjacent cells, along with their individual state, and you then consider that the individual state of each of all other cells is the negation of the closest individual cell among the previously defined finite number of cells.''
: I think the Python is correct under that definition of 'infinity'. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:22, 23 March 2014 (UTC)

:: The description could use some work.  Assuming "adjacent" really means "consecutive" here, the intention of the above statements seems to me to say that, you need to write down a minimal length of cells, and assume cells on both sides of this segment, extending to infinity, are all of the same value, which is the negation of the first and last of the cells you did write down.  It is not stated unambiguously, and it's not capable of representing all possible transitions even if it can represent the initial state (you could end up with all cells having the same value, so negation isn't going to cut it.)

:: Regardless of the wording of the description, the intention is clear: try to represent an infinitely long row of cells, if only a finite portion is not of constant value.  Given the simple case of rule 1 (000->1, everything else->0), and a starting state of every cell being 1 (what else can your "eca_infinite(1)" mean?), the cells should flip between all 0s and all 1s at each step.  The python code does not reproduce that.  I expect you to argue that your code fits one interpretation of the (imprecise) task description, and I expect myself to throw my hands up and say "suit yourself", but please do think about it before that. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 07:01, 23 March 2014 (UTC)

::: ''Regardless of the wording of the description'' ??
:::I admit that the use of the word infinity might be loose, but the author does go on to state what is meant. You can't really disregard their definition. I happen to like their intent which I see as ''incrementally'' extending the cells in a defined way. I can also see your point about Rule1. The task does seem to be implementable. Maybe we might all think of clarifications to the description? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:21, 23 March 2014 (UTC)

:::: As I said, the way of representing infinite cells proposed by the task is no good, since it's incapable of setting all cells to the same value.  Given whatever initial state with rule 0 (everything->0), the next stage will have all cells 0, and what do you do then?  Matching task description is not as important as making sense.

:::: The task description is easy to fix.  Say: 1. We only deal with states where there's a finite length of cells in the middle, with its two ends repeating while extending to infinity; 2. Conceptually, when you apply the transition rule, all cells need to considered, but "how" is up to your code.  If you can represent the state in step 1., you can represent it here, because it would still be a finite length of cells with edges repeating to infinity, only that the middle portion may be longer or shorter, and the padding cells may be of a different value.

:::: I think the task description should be changed to make sense. And I think the above may have been what the task giver actually wanted, but that's maybe just my presumption. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 18:39, 23 March 2014 (UTC)

:::::Maybe we need to drop the use of the word infinite and just say that the cells auto-extend on each iteration which gives some semblance of being an "infinite" cell space but only for limited start conditions for a subset of Rules? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:00, 23 March 2014 (UTC)

::::: ...Well, suit yourself. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 22:23, 23 March 2014 (UTC)
