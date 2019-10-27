+++
title = "Talk:Tree traversal"
description = ""
date = 2019-03-08T18:28:47Z
aliases = []
[extra]
id = 6991
[taxonomies]
categories = []
tags = []
+++


###  Wiki markup problem :Category v. Category references 


Note: This documents a markup glitch or documentation error or non-intuitive feature in the markup.  Not sure if this is where this should live.

Problems observed in the following:

```txt
<nowiki>
...
== Icon and Unicon ==
=
## Icon
=

```Icon
procedure main()
...
end
```


...
Note: A [[Category:Unicon]] specific example has not been provided.    <=== SIDE EFFECT THIS CAUSES the task to show up in tasks completed Unicon
</nowiki>

```

I diagnosed the side effect - Tree Traversal shows up as a completed task under Unicon.
* There is no <nowiki>{{header|Unicon}}</nowiki> tag anywhere on the page. 
* The <nowiki>[[Category:Unicon link]]</nowiki>produces nothing visible unlike other <nowiki>[[..]]</nowiki> tags.  This also causes the problem.
* Replacing the reference above with <nowiki>[http://rosettacode.org/wiki/Category:Unicon Unicon]</nowiki> doesn't cause the problem.
:When you want to make a visible link to a category you need to put a : in front like this: <nowiki>[[:Category:Icon]] or [[:Category:Icon|custom text]]</nowiki>. These will show this: [[:Category:Icon]] or [[:Category:Icon|custom text]]. Is that what you were trying to do? --[[User:Mwn3d|Mwn3d]] 02:51, 13 April 2010 (UTC)
:: Yes I was but that what's odd is that [[Template:Example-needs-review]] and [[Category_talk:Icon]] w/o the :
:: Also, thanks with the : the Side effect above seems to be gone  --[[User:Dgamey|Dgamey]] 03:48, 13 April 2010 (UTC)

==="Don't masquerade as being Pythonic"===

: Some slightly silly suggestions being made here in an oddly threatening tone. 'Masquerade' ? and on my user page "''underhanded and subterfuge on your part that is aimed at confusing the readership as to what constitutes idiomatic Python''" What ? 
: These eccentric complaints do leave me just a little puzzled ... (and, incidentally, also tempted to suggest that you apply a good linter to the slight opacities of your English, but that is neither here nor there) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 03:18, 8 March 2019 (UTC)
:
: I do understand the huge importance which you attach to the 'Pythonic' subset of Python, but I have to say that beyond being grateful for the PEP linters, which I always apply, I am not over-interested in it per se, and I have certainly never claimed to be modelling or demonstrating a purist version of it. Nor am I remotely interested in steering any future developments in the 'Pythonic' dialect of Python, the core traditions of which are predominantly procedural.
: I do like Python, and, like everyone, I find its libraries very useful indeed.
: I also like functional programming, by which I mean constructing code, wherever possible, by the composition of pure functions, minimising the use of mutation.
: I like to code in this style (the core discovery of which is that 'immutability changes everything', and that math works better as an ally than as an opponent – the rest is commentary. Go and study it) because I find that it helps.
: In my context, writing and composing pure functions enables:
:# Faster and simpler coding and refactoring
:# Higher levels of code reuse
:# A little bit more reliability

: I am, however, aware that this style of coding requires the acquisition of a few more concepts than the imperative mode, and can look unfamiliar (and even alarming or discouraging) to those whose habits and preferences are more solidly procedural.
: (It is well known that there is sometimes a bit of tension (and some feelings of de-skilling) in work-places during transitions towards more functional styles)
: I am also aware that your own interest in Rosetta code is very much built around your oft-repeated proposition that it is '''all about''' being idiomatic.
: My own interest in it has a slightly different focus. I don't think of RC as a parade-ground or a museum for modelling spotless uniforms. (I feel that automatic linters can take care of major lint). 
: Instead, my personal view is that presenting solutions to the same task (in as many different languages and idioms as possible) provides insight into how languages are similar as well as different, and helps learners with a grounding in one approach to a problem to learn another. (See the RC landing-page formulations).
: We can disagree on that core focus, and there is absolutely no need for us to persuade each other. We can just submit different code.
:
: I am not quite sure why you feel that I am aiming to 'flood' RC with examples in order to mislead readers about exactly what the Pythonic subset is like. To be honest I am not terribly interested in why you might think that, just as I was not terribly interested when you were (equally eccentrically, in my view) trying to persuade people, on these pages and on Reddit, that my code was automatically generated by a secret transpiler project :-)  (A Haskell to Python transpiler ? Good grief ... whatever for ?)
: I am writing Python code by composing of pure functions, because I find that quicker, more reliable, and more interesting. I always use the PEP linter, but I am certainly not claiming to be modelling or restricting myself to the narrowly 'Pythonic' subset of that language, which is not intended to be optimised for functional programming anyway, and PEP8 sternly warns us against the trap of 'foolish consistency' (the rigid confusion of quality with compliance).
:
: I dare say that you may partially believe your conspiracy theories (secret transpilers, 'masquerades' and 'underhand subterfuges'), and I have no particular interest in weaning you off them, but I must ask you to refrain from using them to authorise campaigns of '''bullying and harassment'''. I am not, oddly enough, a demon, and by somehow persuading yourself that I am, you are, I fear tempting and authorising yourself to behave more than a little disgracefully.  
: Give us all, and yourself, a break 
: If you don't like my code (for example for '''tree traversals''' on this page) then just write different code. Mine works well for me, and I hope it contains some abstractions which you and others might find useful or interesting too, but if not, that's fine.
: I'm sure that your code works well for you. 
: Quality is a function of optimisation for context, and context varies. 
: Go and write good code for your context. Ranting does Python no good, and even the most rigid style-sheet compliance doesn't fix bugs or make bad code good. 
: '''Enough already !''' [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 03:18, 8 March 2019 (UTC)

:: Aigh ... more of this bullying grafitti and strange non-sensical labels. 
:: 'Multi-language' ? What does that even mean ?
:: Stop it ! [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 03:23, 8 March 2019 (UTC)
::: Ai ... and now he abuses privilege in an attempt lock edits and bake in his own graffiti. What is going on ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 03:38, 8 March 2019 (UTC)
:::: When I look at the history of this page (or, at this page itself, I do not see who you are responding to). So, some questions: 
:::: (1) Are you responding on the right page?
:::: (2) Do you have reason to believe I am seeing something different from what you see?
:::: Thanks... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:17, 8 March 2019 (UTC)
:::: I should add that I did have an odd experience with edit locks and browser behavior when I posted that edit. (From my point of view, I hit submit, got an edit conflict warning, hit submit again, saw the page without my intended text, hit back on my browser and resubmitted the form, saw my edit, then looked at history and saw that the page reported I had posted three times - the second time being a reversion of the original text. I am going to leave that there because I think it indicates a flaw in the underlying posting system, and we'll need to leave evidence around if anyone is going to get around to fixing that.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:21, 8 March 2019 (UTC)
::::: Donald/Paddy3118 locked down the edits in his own edit war on the Tree Traversal page last night 
::::: <code> 03:25, 8 March 2019‎ Paddy3118 (Talk | contribs)‎ m . . (206,128 bytes) (0)‎ . . (Protected "Tree traversal": Counter-productive edit warring ([Edit=Allow only autoconfirmed users] (expires 15:25, 8 March 2019 (UTC)) [Move=Allow only autoconfirmed users] (expires 15:25, 8 March 2019 (UTC)))) (undo)</code> and then released it later.
::::: This is the second time in recent weeks that he has started an edit war and then tried this tactic during the course of it.
::::: A little tiresome, and the rationales proposed express some slightly exotic perceptions ... (See his talk of ''subterfuge and masquerade and underhandedness'' etc  last night on my talk page ... ) A few months ago he was expressing the view on Reddit that my [code was written by a secret transpiler project](https://www.reddit.com/r/Python/comments/9re7o9/what_tool_could_produce_this_python/), and asking for help in tracking it down :-( I hope he's OK ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:50, 8 March 2019 (UTC)
:::::
::::: Donald/Paddy3118 was playing out the same drama on Feb 22 2019 (Binomial Coefficients):
::::: <code>(cur | prev) 00:53, 22 February 2019‎ Paddy3118 (Talk | contribs)‎ m . . (63,218 bytes) (0)‎ . . (Changed protection level for "Evaluate binomial coefficients" ([Edit=Allow only autoconfirmed users] (expires 03:53, 22 February 2019 (UTC)) [Move=Allow only autoconfirmed users] (expires 03:53, 22 February 2019 (UTC)))) (undo)</code>
::::: <code>(cur | prev) 00:07, 22 February 2019‎ Paddy3118 (Talk | contribs)‎ m . . (63,218 bytes) (0)‎ . . (Protected "Evaluate binomial coefficients": Counter-productive edit warring ([Edit=Allow only autoconfirmed users] (expires 01:07, 22 February 2019 (UTC)) [Move=Allow only autoconfirmed users] (expires 01:07, 22 February 2019 (UTC)))) (undo)</code>
:::::[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:59, 8 March 2019 (UTC)
:::::
::::: On Oct 26 and 27 2018 (McNuggets and [[Talk:McNuggets_Problem]]), the theme was that my Python code should be deleted, because Donald apparently perceived its idiom as the pursuit of a "private vendetta" :-(  
::::: Fiction could not be stranger ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:28, 8 March 2019 (UTC)
