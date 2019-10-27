+++
title = "Talk:Sum of squares"
description = ""
date = 2008-01-30T02:47:49Z
aliases = []
[extra]
id = 2493
[taxonomies]
categories = []
tags = []
+++

Is this too similar to [[Sum and product of array]]? --[[User:Mwn3d|Mwn3d]] 22:42, 27 January 2008 (MST)
:I think not. The sum-and-product task lets us see how these functions are specified, but they don't let us see them in relationship with another function. The point of sum-of-squares, it seems to me, is to let us see how basic function composition occurs. It takes more than one function to show that. --[[User:TBH|TBH]] 10:01, 28 January 2008 (MST)
::If that is the goal, perhaps a [[function composition]] task would be appropriate. --[[User:IanOsgood|IanOsgood]] 10:07, 28 January 2008 (MST)
::I don't think this counts as function composition. This is just accumulation, which is why I think it's similar to the sum and product. The capital sigma and capital pi symbols in math aren't really functions, and this task would use a capital sigma in its definition. --[[User:Mwn3d|Mwn3d]] 10:39, 28 January 2008 (MST)
:::If summation-of-series and product-of-series are not going to be considered functions within this site, what should they be called instead? They easily fall within a common meaning of the word. To see how, I recommend that we take the Wikipedia page on [http://en.wikipedia.org/wiki/Function_composition_%28computer_science%29 function composition] as a starting point. --[[User:TBH|TBH]] 11:32, 28 January 2008 (MST)
::::If they are considered functions, then they are always function compositions (their arguments are always functions) and the sum and product of an array should be considered that way too. --[[User:Mwn3d|Mwn3d]] 12:54, 28 January 2008 (MST)
:::::If we broaden "function composition" to include anything that produces a function, the resulting category will swallow everything. All tasks would count in that category when approached at the function level. We want categories that provide focus and containment of complexities. --[[User:TBH|TBH]] 14:48, 28 January 2008 (MST)
::Function composition may be too broad a topic for a single task. Consider the following:

 g (f y)
 g (x f y)
 (f x) g (f y)
 fi (g (f y))
 fi ((f x) g (f y))
 y g (f y)
 x g (f y)
 (f y) g (h y)
 (x f y) g (x h y)

::In the notation used above f, g, and h are functions, fi is the function inverse to f, and data arguments are indicated as x and y.

::These nine compostional structures are primary forms in the J programming language, so from the perspective of demonstrating J code nothing less than this set seems adequate. My intuition is that this set is too large to be demonstrated conveniently as a single task, for most other languages. Perhaps function composition should be a category, instead? --[[User:TBH|TBH]] 12:27, 28 January 2008 (MST)
:::Just because it's complex in J doesn't mean it's too broad for everyone else. [[Forward difference]] seems pretty rough for Ada and Java, but it's simple in J. Maybe there's a language where function composition is significantly simpler than the nine structures in J. --[[User:Mwn3d|Mwn3d]] 12:54, 28 January 2008 (MST)
::::You misunderstood. Those nine structures are particularly straightforward in J, being supported by basic features of the language. I doubt we want to make a task that is not only biased toward one particular language, but composed of a sizeable list of subtasks. --[[User:TBH|TBH]] 17:26, 28 January 2008 (MST)
::Although I said before that the point of this task is to show function composition, I notice that does not actually fit with what has already been posted. The Java solution does not involve function composition, nor does the second J example. If this were restructured as a task to compose (g (f y)) these would have to go. --[[User:TBH|TBH]] 14:37, 28 January 2008 (MST)
:::I'm going to assume for the moment that by "function composition", we're talking about taking arguments and computing a result, be it a value or state change.  Even if it results in a huge task, it's not really inappropriate to create a task that explicitly seeks out to demonstrate function creation syntax.
:::
:::In fact, I think it's wholly appropriate for someone to create a [[Functions and Subroutines]] task to demonstrate this very common behavior, just as we have [[Loop Structures]] and [[Conditional Structures]].  If the goal is to demonstrate a language feature, then  I believe an feature-type task ( [[Loop Structures]], [[Conditional Structures]], [[Compund Data Types]], etc. ) is more appropriate than trying to define an operation-type task narrowly enough to force a specific style of implementation.  Clarity is key; The more clear the role of a task, the easier it is for newbies and regulars alike to keep code accurate and pages maintained.
:::
:::And if we're talking about a different kind of function (It's possible; I don't have a strong mathematics background.), would someone link me to a good reference so I'd know what you guys are talking about? --[[User:Short Circuit|Short Circuit]] 23:13, 28 January 2008 (MST)
::::Function composition is using another function's result as the input to another function. The simplest example is ''f(g(x))''. It's operator looks like a degrees sign in the position of a middot between f and g. Due to its name, it can be easily confused with "function creation". --[[User:Mwn3d|Mwn3d]] 06:12, 29 January 2008 (MST)
::::No, function composition is taking two functions and returning a third. This is a basic capability of functional programming languages. For example, I think this is an operator '''(.)''' in [[Haskell]], and it is the default operation in [[Joy]] (that is, the fragment "f g" means compose g with f).  [[JavaScript]] is also capable of this:
 function compose(f, g, args) { return function(args) { return f(g(args)) } }
::::The [http://en.wikipedia.org/wiki/Function_composition_%28computer_science%29 wikipedia article] uses a crappy [[C]] example that misses the point, but includes a bunch of good references. Composition in [[Scheme]] is covered in SICP. --[[User:IanOsgood|IanOsgood]] 10:33, 29 January 2008 (MST)
:::::Yes, Ian.  Function composition occurs when a function is created by specifying a relationship among other functions. --[[User:TBH|TBH]] 10:36, 29 January 2008 (MST)
:::::Neat.  So any language capable of anonymous functions and function pointers could do it. --[[User:Short Circuit|Short Circuit]] 19:47, 29 January 2008 (MST)
::::Because I agree with the emphasis on clarity, I think this task should continue to stand as sum-of-squares. The recent addition of an IDL solution was very satisfying, and would not have shown me what it did if the mere summation task had replaced this one. Also, that IDL solution does not involve function composition. --[[User:TBH|TBH]] 10:36, 29 January 2008 (MST)
:::::Three examples (using the first J example) on this page are almost identical to their sum and product examples. They don't seem different enough to get a whole other task. And, goiing back to the original discussion, if three out of four don't involve function composition then how does this task "let us see how basic function composition occurs"? --[[User:Mwn3d|Mwn3d]] 11:45, 29 January 2008 (MST)
::::::I retracted that claim (on 14:37, 28 January 2008). My current claim is that this task is easily understood and allows us to see features in languages that we do not see with the accomplishment of mere summation. --[[User:TBH|TBH]] 12:21, 29 January 2008 (MST)
:Perhaps this turns on a choice of taxonomy. Is it better to focus on tasks, implementations, or some intertwining of the two? This task was plainly added to the site in order to demonstrate a language feature, and as other languages fill this in that demonstration will become buried. That suggests that it should be handled differently. However, it seems to me that feature-type tasks tend to remove the comparative aspect by excluding languages that do not support a specific style of implementation. 
:Only now do I see the emphasis on role in this recommendation: "Clarity is key; The more clear the *role* of a task, the easier it is for [us] to keep code accurate and pages maintained."
:So, while I think the task is clear and the comparison among languages here is interesting, I recognize that here it is not clear what the role of the task is. --[[User:TBH|TBH]] 13:01, 29 January 2008 (MST)
::So let me pose a question. Can a task be devised whose explicit purpose is to clearly demonstrate the semantics of [[Function Composition]], particularly in a way that is reasonably clear to people with a non-CS background?-[[User:Short Circuit|Short Circuit]] 19:47, 29 January 2008 (MST)
