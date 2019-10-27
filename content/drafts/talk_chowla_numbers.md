+++
title = "Talk:Chowla numbers"
description = ""
date = 2019-03-23T12:59:24Z
aliases = []
[extra]
id = 22214
[taxonomies]
categories = []
tags = []
+++

== "use commas within appropriate numbers" ==

Why is that hip now?
Why is that part of many tasks instead of being solved once in an own task?
This just adds silly useless complexity to a task and nothing else.
Here it absolutely has nothing to do with Chowla numbers and just blows up the code while distracting from the main topic.
E.g. calculating Chowla numbers in Dc would be easy but string maniupulation is not and is a completely different task and topic. A Dc solution probably would need 90% of its code just for that useless unrelated subtask.
Please: Back to the roots!
Concentrate on on clear tasks and problems with a narrow definition.
Complex tasks spiced up with lots of unrelated subtasks don't fit well into a context like Rosetta Code.

''(This is not meant to sound harsh. Please consider that not everyone is born with an English tongue and much misunderstanding may be caused by the translation.)''

-- [[User:Yeti|Yeti]] ([[User talk:Yeti|talk]]) 08:11, 11 March 2019 (UTC)

: Please sign your comments with a trailing:     <big><big> <b> <nowiki> -- ~~~~ </nowiki> </b> </big></big>                     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:44, 11 March 2019 (UTC)

: There is a reason that people add commas to numbers --- to make then easier to read, especially if there are some numbers like 100000 10000 35000000 10000000 100000000 (just to show a few).   It is not a useless thing to require.   If adding commas to numbers is too complex or cumbersome, leave the task to others   (this isn't meant to sound harsh).   The requirement is not to make it more complicated, but to to make the output (displayed) numbers easier to read.   The REXX solution essentially has a one-line function to add commas to integers;   most computer programming languages have commatizing capability within the   '''print'''   or   '''format'''   statements/functions.   Adding commas shouldn't be that difficult for most computer programming languages.   See if the computer programming language(s) you know has/have an entry for the Rosetta Code task   ''commatizing numbers''   which is a much more comprehensive task as it includes locating a suitable number (within a string) which maybe is a number expressed in a floating point expression   (but in this Rosetta Code task, the number is a non-negative integer (that contains no leading or trailing blanks which makes it much simpler).   That commatizing Rosetta Code task also asks to use a user-defining "period" length (normally three), a character (or characters, including a blank) to be inserted (such as a period for a European look, and not to commatize exponents of many kinds, and not to commatize leading zeros, among other things).   Also note that some tasks require a certain format for a list of numbers, most often specifying a horizontal or vertical list to make it easier to compare the outputs of the various entries.   Vertical formats are much more easier to read/comprehend if there are multiple numbers to be displayed (such as an index) where a columnar output would be best.   I never thought that adding commas to (larger) numbers would be considered spicing it up.   If there is any confusing about the task's requirements for non-English speaking programmers, one could always look at the other solutions.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:37, 11 March 2019 (UTC)

I cannot agree. Rosetta Code is about comparing code in n-1 languages. The more subtasks unrelated to the main topic/task a task additionally gets, the more the primary goal gets blurred. Solutions should be like clear statements, not epic operas. --[[User:Yeti|Yeti]] ([[User talk:Yeti|talk]]) 10:02, 11 March 2019 (UTC)

: I'm sorry if you think that added commas to make the output numbers easier to read as an epic opera.   The primary goal was quite simply put, to use a user-written '''chowla''' function to find/display chowla numbers, to use the same function to find/count prime numbers and perfect numbers   (the last two requirements have two reasons:   verify that the '''chowla''' function executes correctly, and the other is that it should be written/coded in a robust manner suitable for Rosetta Code).   This was the goal, but stating the goal wasn't part of the lexicon of the task's requirements.   Rosetta Code tasks can have more than one goal, and in fact, many Rosetta Code tasks do, although goals and requirements overlap.   I may be a bit demanding, but I don't see any blurriness in the goal(s), especially since no goal was mentioned.   I prefer to simply list the task's requirements after the definitions in the task's preamble.   As for what solutions (programs) should look like, I don't subscribe to the belief that solutions should or shouldn't look like;   there are just too many styles and syntax that programmers use and/or prefer.   Not to mention what is idiomatic or not idiomatic for a particular computer programming language.   Others can argue about the merits of comments within the code.   I do believe in specific requirements.   Without them, some programmers have written solutions that don't show any output (although it was obvious that the task's author intended output to be shown, otherwise, how are we to compare the validity of the computer programs?), or the output is in a different format than all the others, or the output doesn't match the others because it uses different input(s), and even some programmers don't even use the same nomenclature.   There are reasons that requirements are specific (even numerous), and that is to ensure we are all seeing the same output(s) for the task's requirements.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:50, 11 March 2019 (UTC)

: Do we really need to agree on this kind of thing ? Optional seems more than good enough [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:24, 11 March 2019 (UTC)
:: +1 for that. See below. If the language supports locales for numbers then OK. Writing a subroutine putting commas every 3 digits in an Anglophobe interpretation of appropriate seems worse than pointless almost stupid.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:50, 11 March 2019 (UTC)
:::Agreed. Tasks should focus on one specific problem. If adding comma is a recurring theme, then make it a task and if you really feel it should appear everywhere, at least make it optional. Besides, not all countries use commas, in France at least the thousand separator is a space : much better to leave such details in a separate task. And while we are at it, adding non-breaking spaces everywhere in task descriptions should not be encouraged. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 07:18, 13 March 2019 (UTC)
:::: I agree that the excessive specification on the formatting of task output takes the focus away from how different languages best solve a task. It makes perfect sense to me to move those formatting specifications to their own separate task. --[[User:Tikkanz|Tikkanz]] ([[User talk:Tikkanz|talk]]) 20:16, 14 March 2019 (UTC)

==http://www.syllabus.ca/en/didyouknow-en/writing-numbers-in-french-and-english/==
In Canadian English, a period is used as the decimal marker, and a comma (or space) is used to separate three numerals. For example, 26,000 reads as twenty-six thousand.
In French—and many other languages—a comma indicates the decimal, so 26,000 reads as twenty-six. That’s a huge difference from how an anglo would interpret it! The proper way to write twenty-six thousand in French is 26 000 or 26000.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:36, 11 March 2019 (UTC)
==What does the quote by Gauss have to do with this task?==
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 16:58, 11 March 2019 (UTC)

: I was thinking the same thing? For a task on a number series, attention is taken away from Chowla numbers.
: It might be worse; Mathematicians and Physicists argue/joke about which is the better from before programmers bickered over Vi vs Emacs. (Vim of course).
: Omitting the quote might make for a better task as it is not clear what it adds. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:35, 11 March 2019 (UTC)

:: The quote is an epigraph.   Chowla numbers (or its function) has significant relevance concerning number theory. It wasn't meant to take attention away from the task at hand, but to add reflection.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:54, 12 March 2019 (UTC)

::: Well, epigraphs on RC. That's change.
:::(P.S. Obligatory [https://xkcd.com/435/ xkcd comic]. To which someone [https://i.imgur.com/sEevZ.png replied]. And [https://qphl.fs.quoracdn.net/main-qimg-bb61a9b12a5047d3ca4f28697f8c5a52-c replied]. And [http://darjeelingzen.blogspot.com/2012/06/fields-arranged-by-purity-of-concept.html replied]) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:11, 12 March 2019 (UTC)

:::Hi Gerard, Maybe the epigraph should be left out. It too could be thought of as something best left to the talk page. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:28, 12 March 2019 (UTC)

:::: Epigraphs are not meant to a discussion point (as I understand its use),   they are to provide a relevant and a different perspective on the subject.   Epigraphs can always be omitted, but they do make a point.   Here are two examples from the books ''Frankenstein'' and ''To Kill a Mockingbird''; in my opinion, the books are better for it.


                 Did I request thee, Maker, from my clay / To mould me Man? Did I solicit thee / From darkness to promote me?
                 — John Milton, ''Paradise Lost'' (in Mary Shelley's ''Frankenstein'')

                 Lawyers, I suppose, were children once.
                  — Charles Lamb, ''The Old Benchers of the Inner Temple'' (in Harper Lee's ''To Kill a Mockingbird'')

                 (''Dune'' also had some very poignant epigraphs as well.)

:::: I have never read responses (or opinions) on the above books on whether or not that those epigraphs should/shouldn't be included in the respective books, or even a discussion on they being appropriate (or not), or even the merits of the quoted texts.   We could discuss the merits of Gauss' opinion, but that wasn't the point of the epigraph.   When one discusses the book ''Frankenstein'', John Milton's quote is <u>never</u> talked about   (well, except for here).   I never thought that adding an epigraph would ruffle so many feathers.   I had thought that the collegiate reader's minds on Rosetta Code would appreciate a relevant quote.   To move the epigraph to the discussion page would surely distract from the Rosetta Code task of chowla numbers.   But perhaps a discussion on number theory and how '''chowla numbers''' relates to that would be refreshing.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:28, 13 March 2019 (UTC)

:::: Or another discussion could be how the '''chowla''' function could be used to find Mercenne primes.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:46, 13 March 2019 (UTC)

Epigraphs are appetizers to a story and put important themes of the story in a reader's mind. In the case of Frankenstein epigraph it is obvious to me how this is the case. What important theme are you claiming should put in my mind by this epigraph. Surely the task as presented is more Euclidean. RC has many tasks on number theory (few well thought out ones) are they all diminished by not Epigraphing Gauss.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:47, 13 March 2019 (UTC)

==Large computations==
To get more language examples doing the task you might want to split the task goals into two:
# A base set that most languages can accomplish.
# Extended limits that are easily do-able by, say, most compiled languages in tens of minutes?

Slow languages might then all halt at the same subset, but show all the types of answer asked for, but for lower counts. 

Just a thought... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:46, 11 March 2019 (UTC)

: I assume that   ''splitting the task in two''   is to have two sets of task requirements, or do you mean that some requirements could be   ''optional''   (or have it as ''extra credit'', as some Rosetta Code tasks do?   I had thought about that route (optional task requirements), but I really don't have an accurate method to determine what most compiled computer programming languages can compute as far as (computer/compute) time used.   I was hoping for robust solutions (with ''robust'' not being defined, but I was thinking for possibly something heavier duty than 16-bit integers), and so far, that hasn't seem to be a problem, but I can only observe that after-the-fact.   Your civil comments was the first along this line.   I know we aren't supposed to show CPU (or elapsed) times used on Rosetta Code (for many varied and solid reasons), so I took a best guess at the high end limits (where to stop computing).   I have encountered other Rosetta Code tasks that really stretch (or often break) the limits of the computer programming languages that I use, and I wouldn't ask the task's author to change the requirements just so "my" (interpretive) languages could execute in tens of minutes.   I was about to consider lowering the upper limits, but then I got distracted by some rather rude disparaging remarks and I spent some time thinking about what the Rosetta Code community is all about and whether to reward such incivility, and if I should bow to bullies and reward their ill behavior.   In the past, if I didn't like a task's requirements, I just didn't bother to create a solution, instead of writing disparaging and crude remarks.   I wish a level-headed Rosetta Code administrator will step in and stop/restrain the nonconstructive (ego) comments on the main page and move them to the discussion page.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:59, 12 March 2019 (UTC)


::Unfortunately I later learned that the sympy library holding divisors is pure Python, but it is well regarded so I used it rather than creating my own. 
::Hi Gerard, I mainly program in interpreted Python but '''don't''' want the present limits to be lowered. 
::It is good to have some tasks that stress some languages, but I was thinking of finding away for most of the slower mplementations to naturally use the same lowered  lmits. In the Python example I tried to do something like that and used shorter runs to prove my code, but just left the longer runs to the end and left them running. Luckily for me, they eventually finished, after sometime! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:49, 12 March 2019 (UTC)

::: Does Python use a non-interpreted "language" for its BIFs?   (In particular,   '''divisors'''.)     The reason I ask is that I wrote a home-grown   '''divisors'''   REXX function that instead of finding the divisors, and then adding them (say, with a   '''sum'''   function), I modified a version of the   '''divisors'''   code to instead of creating a list of proper divisors, I had the function just add the divisors on-the-fly (eliminating the stand-alone summation part).   I then further modified the function to be aware if the target is odd or even, and adjusted the   '''do'''   loop accordingly (along with the   '''do'''   loop increment);   that doubled the speed  (or halved the computation time,   pot-tay-toe, pot-tah-toe).   Essentially, I coded a   '''sigma_proper_divisors'''   function with the subtraction of unity as being built-in by starting the summation with zero instead of unity).       Do you happen to know if Python's   '''divisors'''   BIF does that?   I would suspect that it does.   In any case, this is why I included several formulas/algorithms to calculate the   '''chowla'''   function so that programmers could choose the fastest (most efficient) algorithm.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:00, 12 March 2019 (UTC)



::: I'm really impressed by the large perfect numbers computed with   '''Visual Basic .NET'''   using the Chowla function   (2<sup>nd</sup> section, under   '''more cowbell''').   That's going the extra mile, by gum.   Going from roughly '''33 million''' to over '''8 billion''',   and then to over '''137 billion''',   and then to over '''2 quintillion'''.   Was the computer smoking or losing its magic smoke?     Kudos.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:05, 13 March 2019 (UTC)

::::No magic cigarette, VB.NET benefits from .NET's JIT. It's not your grandma's VB6. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 07:24, 13 March 2019 (UTC)

::::: Er, no.   "Magic smoke" does not come from cigarettes.   The term "magic smoke" is a running joke amongst electrical engineers, computer technicians and computer programmers.   A electrical device operates until the magic smoke is released from it, at which point the component ceases to operate. Therefore, the magic smoke is a critical and essential part of the device's operation.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:12, 19 March 2019 (UTC)

== Mistakenly posted discussion on the task page ==

The comments below were in response to some inappropriate remarks made within the '''Perl 6''' header section, which have since been removed.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:50, 12 March 2019 (UTC)

(Updated)   The inappropriate remarks made within the '''Perl 6''' header section, have been reinstated.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:26, 13 March 2019 (UTC)

:: "We" aren't using a thing poorly suited to finding prime numbers.   This Rosetta Code task (as also the totient function task) is to show that it ''can'' find or determine prime numbers, and as a proof/validation that the computer programs work.   No one is disputing that these tasks are not suited to find prime numbers.   These kinds of silly statements don't belong on this page, the ''discussion'' page is more suited for that, but try to keep the tone civil instead of condescending and demeaning.   Please try and keep this site professional.   If you don't know why a task requirement was included, then ask (on the task discussion page or a user discussion page).   It's this sort of sniping that some people are being critical of Rosetta Code.   People don't respond to rudeness and pettiness.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:24, 12 March 2019 (UTC)     (this task's author).

::::: '''This page'''   (above) refers to the main page  (from where it was originally posted to rebuke some silly and inappropriate comments.    

:::''Gasps and clutches pearls convulsively'' --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 21:06, 12 March 2019 (UTC)

In removing the inappropriate Perl 6 comment, I note that it touched on two topics that already have sections here on the talk page. Maybe the Perl6 author might like to join those discussions? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:25, 12 March 2019 (UTC)

: Tank qew, Paddy.   I certainly didn't want to start an edit war.   The last one lasted pages and then some more pages.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:08, 13 March 2019 (UTC)


: Paddy, you may also want to remove a similar comment in the '''Perl 6''' header section for the Rosetta Code task   Totient function'''.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:47, 12 March 2019 (UTC)
