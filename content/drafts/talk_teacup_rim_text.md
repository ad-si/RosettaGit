+++
title = "Talk:Teacup rim text"
description = ""
date = 2019-08-12T07:14:58Z
aliases = []
[extra]
id = 22449
[taxonomies]
categories = []
tags = []
+++

==Limiting output==
It seems that there could be a lot of output generated - maybe alter the wording to ask for example words and maybe summaries of words found of particular lengths? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:47, 4 August 2019 (UTC)

: I think it would be better to use a common (available) dictionary (so we could compare results).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:35, 4 August 2019 (UTC)

:: I shall specify the wordlist and be specific about the result set. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 01:10, 5 August 2019 (UTC)

:: Okay that's done. How do I tell the Perl6 contributor to abbreviate his output? [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 03:40, 5 August 2019 (UTC)


==A good task description specifies a problem rather than a procedure==
The real task/problem here is to identify and display a subset of words (in a given lexicon) that are 'circular' in the sense which you describe.

The current formulation (para 3) is the narration of a '''procedure''', rather than the statement of a problem or task, and is perhaps not yet quite consistent with the Rosetta Code goal (see the landing page) of aiding ''a person with a grounding in one approach to a problem in learning another''.

(Not all languages or techniques of code composition are built around a notion of 'procedure'. In the traditions (and even architectures) of Lisp, Scheme, Racket, Prolog, ML, Haskell etc, the process of '''evaluation''' is central).

More productive, in the Rosetta Code context, to just state the problem, leave the technique of its solution to the contributors, and let a hundred flowers bloom. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:04, 5 August 2019 (UTC)

: Well I've just had a go at making the task less prescriptive yet with a view to limiting the output. It wasn't easy and I would have been nice to have a nice mathematical way of representing the permutation technique, in the same way that one can with big sigma and big pi for arithmetic formulae. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 07:44, 6 August 2019 (UTC)

:: Unfortunately, following the rewording, it's no longer clear that only one permutation of each set of qualifying words should be printed. After: "Display one line for each set of teacup rim words." I suggest you add the following:

::: "Having listed a set, for example [ate tea eat], refrain from displaying permutations of that set, e.g. [eat tea ate] etc."

:: --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 09:10, 6 August 2019 (UTC)

== should programming solutions be assuming caseless words? ==
I know the specified dictionary of words (now) to be used has no uppercase letters   (or any words with non-letters),   but should (or could) it be assumed for the general case that the words are to be treated as   ''caseless''   (that is, the case [upper/lower/mixed] is to be ignored?   I would think that general solutions would be implemented and not have the computer program solutions be geared to a specific dictionary.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:20, 5 August 2019 (UTC)

:I'd think that when dealing with dictionaries and spellings the ''default'' is to ''not'' consider case - i.e. case insensitive. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:41, 5 August 2019 (UTC)

:: Yuppers, my thinking also.   But I just wanted to hear it from the horse's mouth.   (Er ... that's some form of a compliment.)   I also assumed they might be   ''phrases''   in the dictionary,   words that contain non-letters,   and possibly malformed words.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:46, 5 August 2019 (UTC)

::: Okay, here's the horse speaking. I don't care about case as such. The original illustration of the teacup used uppercase letters. So for the sake of sanity, assume that "Tea" is the same as "TEA". Interestingly, English does not appear to offer solutions in words of any length greater than three. Now if I had the Oxford Unabridged's word list with words from Old and Middle English, who knows what might be found. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 07:44, 6 August 2019 (UTC)

:::: Phew. I thought I was the horse for a while :-)
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:41, 6 August 2019 (UTC)

::::: ∙∙∙   <big> ''straight from the horse's mouth''</big>,   not to be confused with the other end of the horse.   The phrase means   (one version):

                        Getting information from the highest authority.   Origin:   when you get a tip for a horse race, 
                        the tip is better as nearer it is to the horse e.g. the jockey or the trainer.   When you got it
                        ''straight from the horse's mouth'', you have it directly from the source.

::::: (Horses don't talk, but there're some exceptions:   Mr. Ed and Horatio, for two,   and Francis was a mule.)   So the connection of Rosetta Code to horses is obvious.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:14, 6 August 2019 (UTC)

:::::: Urdu language blurs the distinction a little. White (man) as in foreigner: گورا. Horse: گھوڑا. There's aspiration after the initial 'g' and the 'r' is retroflexed. Otherwise it's just 'gora'. Gotta watch those points of articulation.   [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 20:37, 7 August 2019 (UTC)

==Dictionary swap? ==
We seem to use dictionary [http://wiki.puzzlers.org/pub/wordlists/unixdict.txt unixdict.txt]  [https://www.google.com/search?client=firefox-b-d&q=site%3Arosettacode.org+unixdict.txt a lot], and I just found it again as they moved it on their site. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:48, 6 August 2019 (UTC)

: For the sake of continuity with the history of RC, I will specify that. Good find. Thank you! [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 01:32, 8 August 2019 (UTC)

: Re ''"we seem to use ... a lot"'' (above), perhaps worth pausing to consider the goals and qualities ? 
:The MIT dictionary proves more rewarding for this exercise (it contains more circular groups). It is also half the size, which makes scripts quicker to iteratively test, and contributes less to atmospheric warming. Now that we are flip-flopping a bit here, I think some will understandably just future-proof themselves by using both, which is fine, but it may be that the MIT dictionary simply has better qualities as a vehicle for testing scripts. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 08:07, 8 August 2019 (UTC)

:: Hadn't thought of dictionary quality - good point. I just thought that if there's a dict we use a lot, then why not use it again.
:: I tend to stay clear of the scrabble type dictionaries when actually playing word games as they tend to include really odd letter combinations as words (such as qi and qat - which I now know), but which expands my vocabulary in odd directions. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:25, 8 August 2019 (UTC)

::: A good (quality) dictionary would have uppercase words, so   '''god'''   ''and''   '''God'''   would both be present, which are different words and the inclusion of them would preclude shortcuts that most computer programs seem to have taken and assume that all words are lowercase   (unfortunately, this is the case for at least two dictionaries used for this Rosetta Code task).   Also, the commonly known abbreviations such as   '''PTA'''   and others should be included;   it is ''not''   '''pta'''.   Not to mention the inclusion of hyphenated words,   not the least of which would be such words as   '''twenty-one''',   '''21''',   and   '''pow-wow'''   (as well as   '''powwow''').   Also, abbreviations such as   '''A.S.P.C.A.,'''   '''U.S.''',   '''US''', and   '''us'''.   Also, words such as   '''C/N'''   and some common apostrophised (apostrophized) words   ('''it's'''   and   '''ain't''').   There also would be words that have decimal digits in them.   This would test the ability of computer programming solutions to handle mixed case and other characters.   And I would like a few words like   '''antidisestablishmentarianism'''   sprinkled around to stretch the (computer program) limits (if any) for the sizes of the words (strings).   As for the size, I have a much larger dictionary that I use, and it doesn't hinder the testing.   But if the size (for testing purposes) is a problem, insert a circuit-breaker in the program that facilitates testing.   (By the way, is there any limit on soap-box time?)   In short, a good quality dictionary would mimic a "real" dictionary, if not in size, but at least in scope.     --  -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:45, 8 August 2019 (UTC)

:::: Gerard, if you have a big dictionary I would prefer to use that than the unixdict one. I had only opted for that because, well, "we've (mostly) always used that one." Oh, and if you have any others in other languages, that'd be good too. But getting back to the casing issue: the original illustration used uppercase. I'm very much inclined to say "uppercase only." I may yet actually be so bold. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 10:42, 9 August 2019 (UTC)

:::: https://github.com/dwyl/english-words looks like a serious contender [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 13:48, 9 August 2019 (UTC) Hah! so I downloaded words.txt and tried to get my QB64 submission to load it. It couldn't. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 13:56, 9 August 2019 (UTC)

::::: I've just downloaded that file and there are 466,551 words in it altogether. This produces 374 sets which match the task requirements of which 12 consist of 4-letter words such as:

:::::[ADAR DARA ARAD RADA]

::::: However, I think there will be some other 4-letter words uttered by the other contributors if you change the task requirements again :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 14:38, 9 August 2019 (UTC)
:::::: On top of which, 400+K words would be just too many for the purpose of script testing – a waste of time, heat and fuel. The brevity of the MIT10000 is a quality in itself.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:48, 9 August 2019 (UTC)

::::::: Which is a good point and brings me also to another frustration: I was after words that can be pronounced. The "ARC RCA CAR" triple has "RCA". Yeah, it's a word. Sort of. RCA cables and all that. But going back to the coaster and the illustration, "TEA EAT ATE" are speakable words. RCA involves saying the name of each letter. But then what do you do with LASER? It's an acronym but almost no one knows what it stands for -- it's been assimilated into regular parlance. The huge DWYL list is chock full of initialisms and acronyms and assorted unspeakable "words". And yes, it's a WOMBAT (a Waste Of Money, Brains And Time) -- at least for this task. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 07:13, 12 August 2019 (UTC)
