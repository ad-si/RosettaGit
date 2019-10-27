+++
title = "Talk:Vector"
description = ""
date = 2015-08-07T13:44:14Z
aliases = []
[extra]
id = 18905
[taxonomies]
categories = []
tags = []
+++

== Creation ==

This is my first task, so I created it as a draft, please tell me what I could improve.

:Big plus is you have started with an implementation too - that helps a lot I find in getting the amount of work needed to fulfill the task right.
:Try looking at the task description on its own; then at the example. Would someone else be able to come up with a wildly different example from the description? If you knew the language, would you be able to state that another example fulfilled the task? would those two examples be constrained to do similar things?

:I think that the task description needs more work to describe in more detail what needs to be written to meet it and to also make more examples comparable; also don't assume that readers know the mechanics and equations necessary to fulfill the task - it's best to put it in the task description. In [[Quaternion type]], and [[Vector products]] for example, all the maths is in the tasks description. For your task you should describe and state those "four basic operations", and maybe describe or give an example of what pretty printing might look like (with some leeway for those who like to embellish or have languages that have inbuilt and possibly better pretty printers).

: Just my thoughts :-)
: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:29, 21 March 2015 (UTC)

:To reiterate Paddy's points: I think you should explicitly declare which "four basic operations" you had in mind. There are different concepts of "physical vector" and it's the job of the person specifying the task to present the important information about the domain of interest.
:Specifically, by examining the sample implementation you have provided, we can see five basic operations: add, negate, subtract, multiply, divide. Strictly speaking, your multiply and divide are mixed operations since you define them in terms of a vector and a scalar. Looking closer you are only working with 2-vectors, and yet physical vectors could easily be 3-vectors (and depending on the field of study other sorts of vectors are possible).
:Also, note that negate and either add or subtract could be defined in terms of the remaining operations.
:I hope this helps. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:19, 22 March 2015 (UTC)

:I think the number of components probably should be explicitly defined in the description, although if the task will definitely not extend beyond the basic operators it may be ok to allow a variety. Many operations extend to n-dimensional space but cross and triple product for example do not. Even between 2 and 3 dimensions cross product is completely different, usually a wedge product is used as an equivalent in 2 dimensions. Also implementations of operations can differ significantly based on the number of components, normal vectors for example (perpendicular surface normals). --[[User:dotxor|dotxor]] ([[User talk:dotxor|talk]]) 14:16, 7 August 2015 (UTC)

==Duplication?==
Oh. Wait a bit, is this a duplication of [[Vector products]]. I was just typing away and it took me some time to take a look at what you have versus what I mentioned - My bad. This is probably a duplication. I guess you need to search for key words such as vector in  the site before starting a task to see if it is already done. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:37, 21 March 2015 (UTC)

:I took the liberty of adding lang tags around your code to colourize it. 
:If you can add the correct implementation of the examples to your code then you could add it as a second Python entry on the [[Vector products#Python]] section as it is a sufficiently different implementation - using a class rather than a module of functions. 
:Take a look at how the existing Python does the '''output''' header and the '''pre''' tag around its output and copy that as needed too. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:48, 21 March 2015 (UTC)

:It's not a duplication of [[Vector products]]. It's far closer to [[Arithmetic/Complex]]. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:20, 22 March 2015 (UTC)

=="Physical Vector?"==
What is a "physical vector"?   Shouldn't it be 3-dimensional?
[[User:Peak|Peak]] ([[User talk:Peak|talk]]) 22:59, 17 May 2015 (UTC)

: A physical vector is a sequence of numbers which represents some real (or physical) thing. For example, a physical vector might represent the speed a car is traveling, as well as the direction it is traveling. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:44, 7 August 2015 (UTC)
