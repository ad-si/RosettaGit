+++
title = "Talk:Parsing/RPN calculator algorithm"
description = ""
date = 2016-10-04T10:49:27Z
aliases = []
[extra]
id = 11248
[taxonomies]
categories = []
tags = []
+++

== Graduation to task ==

This seems to be a task that is well understood and which is attracting implementations from many different types of languages. Is there any reason to not graduate it to a full task now? –[[User:Dkf|Donal Fellows]] 12:27, 22 January 2012 (UTC)

: Several programming examples are treating the input as characters instead of tokens, and this is working because there are no multi-digit numbers, and no numbers that contain a decimal point, and no numbers that are expressed in exponential notation (such as 3E27), and no numbers that have a leading sign, and no multi-character operands (such as '''**'''), etc. Should those programs be considered incorrect? (Just because they come up with the correct answer, doesn't make it correct.  Hurrumph!) -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:56, 14 December 2012 (UTC)

::The Fortran example contains the remark that dealing only with single-digit integers enables a simple scan, while a fuller multi-digit integer (or further, floating-point with exponent part) requires much more code which would obscure the presumed objective of exemplifying the parsing of RPN itself. I have a function EATREAL that takes 120 lines, while the task is achieved in less than fifty. As for revealing the state at each step, two statements: a WRITE and a FORMAT suffice. [[User:Dinosaur|Dinosaur]] ([[User talk:Dinosaur|talk]]) 10:49, 4 October 2016 (UTC)

== Extraneous Requirements ==

It seems to me that the requirement that one "shows the changes in the stack as each individual token is processed as a table" is entirely extraneous to the core task of parsing and calculating RPN expressions. I think the inclusion of this kind of extraneous printing requirement goes against the principle mission of Rosetta code. For reasons outlined in [[Rosetta Code:Village Pump/Extraneous Printing Requirements in Tasks]], I think we should make this requirement optional. If no one objects after several days, I will make this minor change. --[[User:Abathologist|Abathologist]] ([[User talk:Abathologist|talk]]) 04:09, 18 September 2016 (UTC)

: Could you elaborate on your thinking? (Note also that you linked to a page which does not exist. Does this mean that you are just making this up?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:28, 18 September 2016 (UTC)

:: Thanks for pointing out my linking error. I have corrected it. (Funny that you'd think I was making it up rather than messing up the link!) I elaborate my reasoning pretty extensively on the linked page, and I'd be happy to add further details or clarifications here! Please ask if you have any questions! Either way, I look forward to your feedback. --[[User:Abathologist|Abathologist]] ([[User talk:Abathologist|talk]]) 09:59, 20 September 2016 (UTC)

::: I asked because I had never seen that page before, and was not familiar with it. But ok, it looks like you created it not too long ago. (So... it still sort of looks to me like you were making it up...) 
::: That said, I agree with some of the sentiments you express there. And it's apparent that you put some thought into this and are trying to grapple with a real issue.
::: That said, I also have some sympathy for people interested in seeing intermediate results. Intermediate results, after all, are sometimes a valid thing to want to see.
::: So... my current impression is that this might be a task labelling issue as much as anything else: if the task is about requiring some sort of logging or tracing of an algorithm, it should be named that (to allow the unadorned name to be used for implementations of the unadorned algorithm).
::: I should probably wait for others to weigh in on this? (Assuming they notice and/or care.) Maybe the right place to start, though, would be to pick one particularly egregious example page, and split it into two copies - one with the logging requirements and the other without? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:26, 20 September 2016 (UTC)

:::: I'm obviously "making it up" in the sense that I am ''making'' a contribution by bringing ''up'' the problem and proposing possible solutions; but I'm not "making it up" as in fabricating the appearance of something out of nothing or falsifying in any way. I am simply trying to contribute to the development of this collaborative project by highlighting a problematic practice and exerting energy to help improve things. I didn't create the Village Pump page to give myself a false air of authority, or to trick anyone into thinking my proposal has undue weight; I created it so that I could clearly articulate my proposal and make it available for feedback, critique, addition, etc. If there is another procedure I should follow instead, please instruct me.

:::: I have no objection to the practice of soliciting and providing intermediate results. They can be very instructive. I have not suggested that we should limit or eliminate solutions that provide representations of intermediate stages. Nor do I think it is wrong to encourage and solicit such representations. I am only suggesting that ought not to constrain the kinds of solutions available for the core task by making such (interesting but extraneous) representations part of the required specifications. 

:::: Now, as for the question of how we should free up these core tasks for a greater variety of approaches: I think that making the logging optional is more economical, more inclusive, and easier achieved than duplicating task pages. I think there are several good reasons for preferring the former:
::::# Making the logging optional only requires changing a single line.
::::# Making the logging optional opens the way for more idiomatic solutions in languages that are pure, type-safe, non-imperative, esoteric, etc., without invalidating any current solutions or generating a needless duplication of tasks that differ in just a slight detail.
::::# Making the logging optional encourages several different approaches to such tasks, which helps show how the kind of approach needed in one and the same language can vary greatly because of slight changes in the specification. (This is already how the [[Parsing/RPN_calculator_algorithm#Haskell]] answers have turned out here.)
::::# If the particular method of logging the intermediary stages of an algorithm are really very interesting (as they sometimes may be), then this logging should have it's own task (perhaps branching off of the core algorithm?). But most logging requirements are not interesting, they just require adding in a few print lines to the existing impure and imperative approaches. Establishing the precedent that any logging component should require the entire task to be duplicated seems like it could lead to lots of frivolous repetition, that would only make organization more difficult and search results longer.

:::: I am in no hurry to see this done, and would prefer taking the time to find the right approach rather than rushing ahead with a temporary fix that is not sufficiently robust. On the other hand, we could pick two different tasks and they each of these solutions, and see which seems to make more sense after the pages have grown another 6 or 12 months.

:::: I will be very interested to hear your feedback, and I am glad you are able to appreciate the rational and recognize the need for my proposal. I think a few subtle changes like this can really help open up space for an even wider variety of problem-solving approaches, and yet more illuminating and inspiring comparisons and contrasts between them.

:::: (Do you think we should migrate the bulk of this discussion to the Village Pump page, since it all about the general approach rather any particular implementation?)--[[User:Abathologist|Abathologist]] ([[User talk:Abathologist|talk]]) 04:44, 21 September 2016 (UTC)

::::: This discussion is long enough, already, that I don't think it should go on village pump. If you feel dropping a small section there that links here, that could work.

::::: I don't like the idea of making it optional - that just seems lazy. The problem being that the task results would then be less comparable, and the whole point of this site is to create examples which are comparable across languages.

::::: It's ok to object to this aspect of the tasks, though, and to recommend that most tasks avoid such things, and to also require a bit more work on the task author's part (such as making it a part of the page title). And, I suppose, it's also all right to set up the task such that the logging is an "extra credit" part of the task rather than a core part.

::::: But I also want to get some other people's opinions on this - I know other people have disagreed with me on other issues, so I don't think I should be the sole voice here. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:58, 21 September 2016 (UTC)

:::::: I see your point about the optional option encouraging non-comparable implementations. I hadn't considered that. By "extra credit part" do you mean, it would not be an optional feature of the algorithm, but like a secondary, extra credit implementation? E.g., something like,

::::::: >Task: Implement X using Y and Z. Extra credit: implement another version of X using Y and Z that logs intermediary stages of the data.

:::::: If this is a correct interpretation of your suggestion, I think it is a very good idea! It addresses all of my concerns.

:::::: I am fine waiting for some time. I created that Village Pump page in the hopes of soliciting feedback, but none has yet been given. What other steps should I take to get a wider chorus of voices chiming? Thanks for your feedback and guidance! --[[User:Abathologist|Abathologist]] ([[User talk:Abathologist|talk]]) 03:09, 3 October 2016 (UTC)

::::::: Yes. Thank you. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:03, 3 October 2016 (UTC)

::::Whilst the actual logging code is probably mundane, it does show how the algorithm works. If there was a vote, I'd favour keeping this particular task as it currently is. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 19:12, 3 October 2016 (UTC)
