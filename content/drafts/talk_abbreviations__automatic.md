+++
title = "Talk:Abbreviations, automatic"
description = ""
date = 2018-11-05T08:02:53Z
aliases = []
[extra]
id = 21602
[taxonomies]
categories = []
tags = []
+++

==error in the Cantonese list of the names of the days of the week==
<strike>
I'm trying to get some information on the correct spelling (translations) of the names of the days of the week in Cantonese.

Currently, the list that I have has the words for '''Sunday''' and '''Monday''' as the same word.


What I have is: 
  Sunday     sing_kei_yat 
  Monday     sing_kei_yat 
  Tuesday    sing_kei_yee 
  Wednesday  sing_kei_saam 
  Thursday   sing_kei_sie 
  Friday     sing_kei_ng 
  Saturday   sing_kei_luk


The underscores were added by me to signify blanks.   The actual translations are: 
  Sunday    = sing kei yat 
  Monday    = sing kei yat
  Tuesday   = sing kei yee
  Wednesday = sing kei saam
  Thursday  = sing kei sei
  Friday    = sing kei ng
  Saturday  = sing kei luk
  Sunday    = sing kei yat 



This (incorrect) information was gleaned from the webpage 
    www.travlang.com

::: <big> Note:   beware of the ads on those pages will most likely use/create cookies. </big>


(I've tried to send in a notice of the errors, but that site isn't currently accepting error reports.)


I haven't been successful in finding another site that does this sort of translating for Cantonese (to an English-spelled version).

If anyone can find the correct "English" version of the Cantonese translations of the days of the week, I would appreciate any assistance.


As soon as I get or find the required information, I'll strike this whole section (over-strike).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:31, 17 September 2017 (UTC)
</strike>



:As near as I can tell, the days of the week in Cantonese are:

    Sunday	禮拜日	láihbaai yaht				
    Monday	禮拜一	láihbaai yāt				
    Tuesday	禮拜二	láihbaai yih				
    Wednesday	禮拜三	láihbaai sāam				
    Thursday	禮拜四	láihbaai sei				
    Friday	禮拜五	láihbaai ńgh				
    Saturday	禮拜六	láihbaai luhk

:Not being at all fluent or knowledgeable, I have no way to tell if they are accurate. http://languagephrases.com/cantonese/monday-to-sunday-in-cantonese-language/ --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:28, 17 September 2017 (UTC)

:Actually, I don't know that I would trust many (most?) of these translations. 

    Psuedo Azerbaijani: Bazar_gÜnÜ Bazar_ærtæsi Çærs,ænbæ_axs,amò Çærs,ænbæ_gÜnÜ CÜmæ_axs,amò CÜmæ_gÜnÜ CÜmæ_gÜnÜ
    Actual Azerbaijani: Bazar Bazar_ertəsi Çərşənbə_axşamı Çərşənbə Cümə_axşamı Cümə Şənbə

: Source: https://translate.google.com/translate?hl=en&sl=az&tl=en&u=https%3A%2F%2Faz.wikipedia.org%2Fwiki%2FH%C9%99ft%C9%99nin_g%C3%BCnl%C9%99ri&anno=2 Many that I've looked at are similarly (in)accurate. Not that it really particularly matters I suppose. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:48, 17 September 2017 (UTC)

-----

Yes, I'm quite sure the "correctness" of that list of the names of the days-of-the-week has many inaccuracies, most of them could start a debate amongst those much better at linguistics than we.   I don't have that much knowledge about any of those languages, but enough to know that what I don't know about linguistics could fill a large bucket.   But, as you said, not that it really matters that the list isn't 100% accurate, as it's the programming that is forefront of this Rosetta Code task, ... but still it would be nice if the data were more accurate. 

Anyway, thanks for your research. 

By the way, I've updated the list of names to again include Cantonese for this task with your information.   
It was interesting on how much our two lists differ, even with the names that appear to be (more or less) in "common".   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:35, 18 September 2017 (UTC)

The Mandarin is perhaps a bit too noisy and eccentric not to provoke a feeling of mild insult and neglect :-) ( incorrect and intermittently missing tones and vowels, curious redundant characters. Standard and consistent Pinyin would be:
xīngqītiān xīngqīyī xīngqī'èr xīngqīsān xīngqīsì xīngqīwǔ xīngqīliù  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:22, 22 September 2017 (UTC)

: As mentioned above (concerning the website that I originally gathered all the information regarding the translation of the names of the days-of-the-week),   I think (but I have no basis in fact) that the translations were all (mostly?) done by   ''netizens''   and it appears the result aren't up to scratch.   This is my own opinion and observation.   All this research and information gathering (by me) was done decades ago, and Google was still in the future.   ... And all the ways the Far East languages are spelled and/or translated phonetically is so diverse, and I suspect that there are a lot of dialects that were maybe unconsciously used or referenced.   Perhaps I should add some sort of disclaimer about the veracity of the data.    -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:07, 23 September 2017 (UTC)

==Solution for Python==
'''Preface:''' I just registered recently on this website and decided to make my first contribution for a problem that didn't have a solution for Python.  <br/>
When I finished writing the code, someone else already provided solutions. <br/>
Imperative: [http://www.rosettacode.org/mw/index.php?title=Abbreviations,_automatic&oldid=272170#Python http://www.rosettacode.org/mw/index.php?title=Abbreviations,_automatic&oldid=272170#Python] <br/>
and Functional: [http://www.rosettacode.org/mw/index.php?title=Abbreviations,_automatic&oldid=272190#Functional http://www.rosettacode.org/mw/index.php?title=Abbreviations,_automatic&oldid=272190#Functional]. <br/>
I found my solution more concise, Pythonic and readable than both versions and decided to replace one of them by my version. <br/>As my solution follows mainly functional paradigm, apart from main function, I decided to replace the functional version. <br/> 
'''Problem:''' Please, provide feedback on if I should leave it like that, try to make my version completely functional (and probably less readable), or post it separately (if so then how should I name my version)? <br/>
Here is my code: [http://www.rosettacode.org/mw/index.php?title=Abbreviations,_automatic&oldid=272203#Functional link] <br/>
Any notes on the code itself are also welcome. --[[User:Georgy|Georgy]] ([[User talk:Georgy|talk]]) 12:57, 4 November 2018 (UTC)

The author of the original functional version posted both his and mine versions. So, I guess, problem solved! --[[User:Georgy|Georgy]] ([[User talk:Georgy|talk]]) 13:12, 4 November 2018 (UTC)
: Always good to see more functional Python here. Perhaps just add an explanatory gloss on what your variant is optimising for. (Mine, for example,  aims to minimize the number of new lines of code that need to be written and tested, and to enable fast and flexible refactoring.) Most coders are not football-rattle tribalists – they are polyglot professionals with a varied hinterland, who carry a flexible toolkit of abstractions and languages. My variant will probably be more readable to those who happen to be familiar with the ML (or Bird & Wadler 1988) tradition of functional programming, which shares names for universal functions across a number of different languages, and has a particular interest in denotational semantics, and the underlying mathematical necessities of function composition. It's a tradition which has flowed into quite a few function names in Python, and also into its list and set comprehensions.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:44, 4 November 2018 (UTC)

:: PS the pious incantation of PEP 20 ''"There should be one-- and preferably only one --obvious way to do it"'' looks charming on paper, and goes '''very''' well with flutes and wind-chimes (as long as you can hold your meditation posture and keep a straight face) but it is, of course, just a pious hope – very much like looking out of the school-room window and wishing that there were only two numbers that could be multiplied to obtain 60, only two that could be added to obtain 100, and only one integer ratio that could roughly approximate Pi. An understandable aspiration, but mathematically and practically it makes no sense at all :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:06, 4 November 2018 (UTC)
