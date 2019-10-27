+++
title = "J/HouseStyle"
description = ""
date = 2014-04-27T06:58:34Z
aliases = []
[extra]
id = 4891
[taxonomies]
categories = []
tags = []
+++

This page is a sandbox for discussing the goals of the [[:Category:J|J]] community on RC, and to develop guidelines and standards for J code to help achieve those goals. The result will be a J [[wp:Style guide|house style]].

== goals ==
<ol>
 <li>increase the usage of J by:
  <ul>
  <li>increasing its exposure (coverage of tasks)</li>
  <li>showcasing its strengths (terseness, clarity of solutions)</li>
  <li>making examples accessible to newbies (e.g. judicious naming of code segments)</li>
  </ul>
 </li>
 <li>provide resource of "good" example J code to J learners</li>
 <li>provide a Rosetta stone for explicit and tacit forms of J</li>
</ol>

== style guidelines ==

Most of the J code presented in the RC J examples are considered good form - to J programmers. The main issue is that programmers familiar with other languages tend to expect an algorithm statement to have explicit iteration, and perhaps consume several lines of code. What is "good form" to a J programmer isn't necessarily easily readable to programmers familiar with scalar languages.

I agree with Donal Fellows in his comments 
http://rosettacode.org/wiki/User_talk:Dkf#Your_discussion_about_J
in that more detailed comments and algorithm explanations are required for every code example in the RC forum, since the whole point is to help readers unfamiliar with that specific language understand what is going on in the code. The more unconventional a language is, the more explanation required. Hence, J examples in RC should display good coding practices (for J) in the example, but that code example should also be accompanied with thorough explanations and comments, all of which should be more thorough than if the audience was just J programmers. For that matter, this advice should be applied for all RC code examples. Teledon 8:47am 13 October 2009

:This is great advice! Would it help to have something like an {Obscure} template readers could use to tag examples which they find difficult to understand?  --[[User:IanOsgood|IanOsgood]] 17:10, 14 October 2009 (UTC)
:: I personally prefer that the code be lean and have "standard" comments (i.e. have the comments it would have "in real life").  I don't want Task pages to become collections of book reports.  I want RCers to compare code, not prose<sup>[[#1|1]]</sup>.
::If the author feels the code deserves deeper consideration, he can post an explanation or exegesis in the talk page or a subpage, and link to it from the solution.  See, for example, the explanations for the J [[Talk:Zig_Zag#reading_the_J_examples|zig zag]] and [[Talk:Spiral#J|spiral]] solutions.
::On the flip side, if a RC peruser comes across a solution he finds obscure or confusing, he can request clarification on the Talk page, or on the author's discussion page.  That's the purpose of those pages.  This might entice the author to post a minor clarification, or inspire him to compose a full exegesis, as above.  But I do not agree with the concept of a "{obscure}" tag, as I imagine it would be wielded more as a slur rather than a request for aid.
::Regarding how all this applies to J:  J's syntax is completely different from the von Neumann approach, and so "fully explaining" a solution amounts to teaching J (its rhematics, syntax, and the semantics of every symbol used -- basically the entire Dictionary).  Commenting every J solution "enough" to allow another programmer to implement it in another language would turn J "solutions" into J "essays".  We'd basically end up describing the algorithm in English, which defeats the purpose of describing it in J.
::So I've concluded the best approach is shock.  That seeing that it is even ''possible'' to express, in ~50 characters, an algorithm that takes other languages several paragraphs, will sufficiently impress a peruser to investigate J, if only for the opportunity to call bullshit.  Especially if it happens in algorithm after algorithm after algorithm ....

::<div id="1"><sup>1</sup>Except in the case where a Task highlights an aspect of syntax or a language feature, which is best demonstrated, then described in English, e.g. [[Exponentiation_operator#J|exponentiation operator]].</div>

:::I agree with much of what you say - particularly that full descriptions/explanations of the code don't belong on the main task page. However I do think that a slight increase in the verbosity and amount of comments will help decrease the perceived impenetrability of "standard" J solutions.
:::Using the [[Zig Zag]] task as an example, I think something like one of the solutions in your excellent explanation on the Talk page would be both intriguing and more likely to encourage further study by non-J users, than the current solutions. i.e.

```j

   zigzag=: $ [: /:@; [: <@|.`</. i.

```

:::vs.

```j

   reorder   =: /:@;
   antidiags =: <@|.`</.

   zigzag=: $ [: reorder [: antidiags i.

```

:::--[[User:Tikkanz|Tikkanz]] 22:46, 14 October 2009 (UTC)

:::The [[Select]] task is perhaps a better example. Compare the current solution:

```j

load 'misc'
select=: ({::~ 'choose a number 0..' 0&".@prompt@, ': ',~ ":@<:@# [ i.@# smoutput@,&":&> ' '&,&.>) :: (select@([ smoutput bind 'please choose a valid number'))

```

::: to a slightly more verbose one:

```j

load 'misc'
displayMenu      =: i.@# smoutput@,&":&> ' '&,&.>
makeMsg          =: 'Choose a number 0..' , ': ',~ ":@<:@#
displayBadNumMsg =: [ smoutput bind 'Please choose a valid number!'

select=: ({::~ _&".@prompt@(makeMsg [ displayMenu)) :: ($:@displayBadNumMsg)

```


:::As a relatively fluent tacit J user, the first version took a little while to come to grips with if for no other reason than because it was so long. --[[User:Tikkanz|Tikkanz]] 02:02, 16 October 2009 (UTC)

----
Thanks to to both of you.  I'm going to write up my own views in the next couple of days, and start recommending specific guidlines.  Feel free to change them and push them in any direction you see fit.  If I get time, I'll try to collect here the views that others expressed in the [http://www.jsoftware.com/pipermail/programming/2009-October/016537.html corresponding J forum thread].

--[[User:DanBron|DanBron]] 16:55, 14 October 2009 (UTC)


== suggested format & style ==

We have several nebulous purposes on this site, but IMO the primary one is advocacy.  So here are my thoughts on a '''''style''''':

In order that J garner notice and perhaps converts, the J code should differentiate itself.  One way to do this is to ''consistently'' keep the code short, to pique interest and preclude [http://www.urbandictionary.com/define.php?term=tl%3Bdr TL;DR].  However, the code should not be so dense as to be impenetrable, to preclude [http://rosettacode.org/mw/index.php?title=User_talk:Dkf&diff=63678&oldid=53652 sentiments like these]; at the very least the names should provide some hints.  

Another way to do differentiate J is to emphasize its unusual or powerful aspects.  This includes:
:# Preferring tacit code to explicit, 
:# focusing on function composition and powerful and unusual primitives, such as <code>&.</code> and <code>^:</code>, and
:# minimizing explicit control structures while highlighting tacit flow control (see [http://www.jsoftware.com/pipermail/programming/2009-November/017042.html an exposition on this topic]). <br />This latter rule isn't hard-and-fast, e.g. <code>if.</code> statements are ok if the code is ''already'' explicit, but I would like to see Jers avoid explicit <u>loops</u> at any reasonable cost. 

And my idea for a '''''format''''':

===[[J]]===
----

If I'm reusing names from other tasks or the [[j:Essays|J wiki]], I will directly link to them here.  Otherwise there will be no lede or introduction.

'''Solution''':
```j
name =: a succinct but legible definition   NB.  Possibly references other defined names, 
                                            NB.  as Tikkanz showed, where each name encapsulates a single,
                                            NB.  but powerful thought.  No trivial names.
                                            
```


'''Alternative solution (''style'')''':
```j
name =: alternative, equivalent solution in a different style  
                                            NB.  For example, explicit as opposed to tacit. ALWAYS lead with the preferred
                                            NB.  (usually shortest) solution; alternative solutions are merely showcases.  
                                            NB.  Note also that the exact same name is assigned, indicating all solutions
                                            NB.  are identical, and could equally be the referent of the Example section. 
                                            NB.  Do not present too many solutions; keep the J entry short and sweet. 
                                            NB.  Further solutions can always be linked to.
```


'''Example''':  
```j
name argument     NB.  Example usages; exercise all the interesting aspects, 
                  NB.  particularly "free" extensions and generalizations 
                  NB.  which are unavailable in the other languages.
```


'''Discussion''':  Very brief, high-level overview of how the solution works (e.g. mentions the heart of the solution is a built-in J <code>primitive</code>), with an optional [[Talk:J/HouseStyle#J|embedded link]] to a fuller explanation or exposition on the Talk: page.
----

I think this format has several advantages:
# It gets straight to the point (the solution).
# It does not conflate the code that defines the solution, the code that invokes it, or its output.  This will give non-Jers a more accurate sense of the actual amount of code required to solve the task.  I'm thinking maybe "non-core" code that formats the result (as opposed to calculating the result) can also be included in the "Example" section (unless the challenge of the task is formatting, of course).  See [[Pascal's Triangle#J|Pascal's Triangle]] for an example of how these concepts are separated.
# At the end, it gives a high-level description for anyone who was baffled by J's spelling scheme or grammar (which description should whet the appetite of the curious to read the full exposition, if there is one).  Please follow the [[Talk:J/HouseStyle#J|talk page link]] for my thoughts on how to craft a J exposition.

Once we settle on a template, it should definitely be used for new solutions, and it would be nice to back-port it to existing solutions when we have the time (on an ad-hoc basis is fine).

----
Thoughts?  --[[User:DanBron|DanBron]] 18:54, 10 December 2009 (UTC)

: My priorities seem to be: 1. working code, 2. simplicity, 3. ease of description.  But, of course, I do not always get everything right on my first try.  [[User:Rdm|Rdm]] 19:15, 10 December 2009 (UTC)
:: At this stage in the game, let us focus on concrete suggestions, rather than abstract goals (though you can record your thoughts on that in earlier sections of this page).  
::: In re your priorities:  Working code is absolutely mandatory; one should not "save page" if the solution does not correctly implement the task as described (and perhaps as clarified by other implementations).  Simplicity and ease of description are well intentioned goals but problematic in the context of J on RC (outside the general problem of subjectivity).
::--[[User:DanBron|DanBron]] 19:20, 10 December 2009 (UTC)

: I like the outline for J solutions described above. A significant number of solutions follow this format already. Should there be guidelines for formatting code too?--[[User:Tikkanz|Tikkanz]] 21:29, 10 December 2009 (UTC)
:: Do you have anything particular in mind?  I'm pretty relaxed about this.  Some fine-grained stylistic rules I personally like to follow:
:::# Ancillary definitions that shouldn't persist in the namespace should be defined locally (<code>=.</code>); main verbs which solve the task or otherwise deserve attention should be defined globally (<tt>=:</tt>).  Global names which rely on local names should fix (<code>f.</code>) them.
:::# Space a line of code according to J's rules of binding power (e.g. put verbs directly adjacent to conjunctions, space out trains a little, etc).
:::# Minimize parens.  Maximize use of forks (this isn't an endorsement of capped fork, though that does allow certain verbs to scan better).
:::# Control on the left, data on the right.  This extends to verb trains in the sense that the more complex tine should be on the right, and <code>~</code> used on the middle tine as required.
:::# For the sake of legibility to others, I will try to eschew tacit adverbs.  But I'm addicted, so I can't make any guaruntees.

::I've got a bunch more that don't occur to me immediately.  But I'm interested in what you (and others) have to say.  As I said, this is not as important to me as "big picture" style and formatting -- I still want to allow individual authors to express their own styles.  --[[User:DanBron|DanBron]] 21:51, 10 December 2009 (UTC)

== Meta issue: "converts" ==

Personally, I am slightly bothered by the idea that we are trying to gain "converts".

In part, I think this misses out on some of J's strengths.  But, also, I am bothered by the implications.

But, first: when you are working with other people, you need to spend some time using their language.  This means that some of the most productive uses of J will probably be multi-lingual uses.  I am sure we can all come up with some good examples of this, but Javascript in browsers is probably a fairly common case, this year.

Second, J has some strengths that become stronger when used in the context of developing programs in other languages.  In particular, J pushes "control structure issues" into its expressions, and refactoring code -- and re-architecting code -- often winds up being a set of relatively minor changes in a J program.  In other words, J can help us reason about program architecture by making experiments easy.  If we are working purely in J, this issue is of course important: sometimes we can only gain reasonable performance from a program by re-architecting it to work around issues introduced by the interpreter.  However, various other languages make architectural changes difficult and some significant number of projects could benefit from an architectural exploration phase even when the target language is not suitable for that task and yet can not be changed.

Third, historically speaking J evolved from notation which was intended to convey ideas and teach people about computing.  J suffers and benefits, of course, from the ugliness and ubiquity of ASCII.  Nevertheless, it seems to me that J has some serious potential, for conveying ideas about programs.  But I feel this potential gets lost in the noise when we focus on "J as the way to do things" rather than using J to present concepts as clearly as possible.  (And I am probably just as guilty of this mistake as anyone else.)

Of course, for some people and for some projects, J is the right tool and I do not want to take anything away from that.  And, almost certainly, some projects which do not currently use J could probably benefit greatly if they did. And, sometimes a small bit of J code you can write up in an hour or so might outperform code that takes months or years to write in another language. But I think that if we try and pretend that J should always be the right tool for the job that we diminish both the J community and the other communities we can participate in.

Essentially, when we make J as disposable as we can, we wind up creating situations where people can choose J on its merits rather than because of some other issue.

So, what does this mean for J's House Style?  I am not completely sure.  But perhaps a good start would be to call out a handful of "decent examples of good presentation of ideas, using J" -- to put some emphasis on the conveying of concepts, where it belong.

--[[User:Rdm|Rdm]] 17:21, 24 May 2010 (UTC)
