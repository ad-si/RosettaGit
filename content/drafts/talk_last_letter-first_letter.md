+++
title = "Talk:Last letter-first letter"
description = ""
date = 2013-05-22T12:58:53Z
aliases = []
[extra]
id = 9880
[taxonomies]
categories = []
tags = []
+++

==Number of names==
This is equivalent to asking for the longest path in a directed graph, which is an NP-complete problem. That doesn't rule it out as a task, but running it on a graph with 646 nodes might take too long. Also, since it seems to be rare to use external files to store the data (or at least I haven't seen many tasks that call for that) having all the names in each example would get really cluttered. [[User:MagiMaster|MagiMaster]] 20:52, 5 June 2011 (UTC)
: Re input data; I can always host input data if that makes it convenient to work on appropriate data. --[[User:Short Circuit|Michael Mol]] 23:12, 5 June 2011 (UTC)
:: That's not really the problem.  A quick breadth first search implementation gave me 646 pokemon names, 1793 pokemon name pairs, 518546 pokemon name triples, 14745709 pokemon name quadruples, and I had to reboot my machine to recover from my attempt to represent all pokemon name quintuples.  Inspecting this sequence suggests a search space on the order of 28<sup>28</sup> or about 3&times;10<sup>40</sup> sequences to investigate.  That exceeds the storage capacity of my laptop.  (Not to mention, exceeds my patience).  Note that finding a long sequence is not very hard, it's finding the '''longest''' that makes this task hard. --[[User:Rdm|Rdm]] 20:08, 6 June 2011 (UTC)
::: If the part about using all the pokemon names was changed, it'd be an interesting task. 20 or so names/words/nodes should demonstrate the process well enough without being too slow. [[User:MagiMaster|MagiMaster]] 21:37, 6 June 2011 (UTC)

Re problems with PicoLisp version, Nidoran can be male or female, and this is marked in the list with male and female (mars and venus) symbols. I'd suggest spelling out "Male" and "Female". [[User:Axtens|Axtens]] 10:10, 7 June 2011 (UTC)
: OK, the PicoLrisp solution is correct either way. Dkf complained that the name appeared twice, but this happends because PicoLisp correctly distinguishes between these two symbols. The code is the same anyway, just the input data changed.--[[User:Abu|Abu]] 13:18, 7 June 2011 (UTC)
:: I complained because it flatly wasn't obeying the rules of the game. The rules relate to letters of words, not random extra obscure marks. Right is right! –[[User:Dkf|Donal Fellows]] 14:45, 7 June 2011 (UTC)

Unless someone has a reasonable algorithm that makes this doable then it should be deleted or modified. --[[User:Paddy3118|Paddy3118]] 07:00, 7 June 2011 (UTC)
: Okay, have allowed for limiting the list to the first 151 Pokemon. Extra points for using the full 646. [[User:Axtens|Axtens]] 07:29, 7 June 2011 (UTC)
::If you can't ''do'' the full task then maybe you should leave it out?
::Have you done the 151? --[[User:Paddy3118|Paddy3118]] 07:49, 7 June 2011 (UTC)


### Task description change proposal:


A certain childrens game involves starting with a word in a particular category. Each participant in turn says a word, but that word must begin with the final letter of the previous word. Once a word has been given, it cannot be repeated. If an opponent cannot give a word in the category, they fall out of the game. For example, with "animals" as the category,


```txt
Child 1: dog 
Child 2: goldfish
Child 1: hippopotamus
Child 2: snake
...

```


;Task Description
Take the following selection of 72 English Pokemon names (extracted from [[wp:List of Pokémon|Wikipedia's list of Pokemon]]) and  generate the longest possible combination of Pokemon where the subsequent starts with the final letter of the preceding. No Pokemon name is to be repeated.


```txt
audino bagon baltoybanette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
mienshao milotic moltres monferno munna murkrow musharna nidoking noctowl
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivysnorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
```


Extra brownie points for dealing with the full list of 646 names.

--[[User:Paddy3118|Paddy3118]] 10:29, 7 June 2011 (UTC)

: Did you mean to leave the ♀ on nidoran? It'd be hard to match that with anything :) [[User:MagiMaster|MagiMaster]] 11:16, 7 June 2011 (UTC)
:: New list above - thanks. --[[User:Paddy3118|Paddy3118]] 12:51, 7 June 2011 (UTC)
::: 72 works for me.  Depth first traversal gave me a path of 24 in about 2 seconds.  &mdash;[[User:Sonia|Sonia]] 14:40, 7 June 2011 (UTC)
:::: I just noticed that several of the names lost the space between them. I've added it back, but someone should double check that I didn't mess something up or miss something. [[User:MagiMaster|MagiMaster]] 21:23, 7 June 2011 (UTC)
::::: That was probably not a bad thing -- embedded spaces in name does not change the results which would be found, it only changes how they would be displayed.  (And, it can make representing and parsing a list of names easier since without embedded spaces you only need spaces for name delimiters.) --[[User:Rdm|Rdm]] 10:58, 8 June 2011 (UTC)
:::::: They were two separate names. Check it against Wikipedia, but trapinch and treecko are two different pokemon, but were written as trapinchtreecko in the list above. (As far as I know, no pokemon has a two-part name.) [[User:MagiMaster|MagiMaster]] 11:10, 8 June 2011 (UTC)
::::::: In my own shortlist I've renamed "porygon2" to "porygon_two". Also, I'm reading the list in from a CRLF delimited file so in the full-length list I can have "Nidoran male", "Mister Mime", "Mime Junior", "Porygon two" etc. I'm still trying to get this backtracking stuff figured out, but out of the short list (as modified) my javascript has generated a 25 element list: 

```txt
Yamask, Kricketune, Emboar, Rufflet, Trapinch, 
Heatmor, Relicanth, Haxorus, Seaking, Girafarig, 
Gulpin, Nosepass, Simisear, Registeel, Loudred, 
Darmanitan, Noctowl, Landorus, Spoink, Kangaskhan, 
Nidoking, Gabite, Exeggcute, Emolga, Audino
```

On the full list I've managed a 190 element transit. [[User:Axtens|Axtens]] 08:38, 9 June 2011 (UTC)

### Concatenated names

To get the list of names I cut-n-pasted the wp table into openofice calc; selected the column and pasted it into an editor then turned it into a multi-line string that I could then names.strip().split() in Python to get each name. When told about the male and female characters still left in the names I thought I took the un-split names and removed any character that was not an ASCII alphanumeric or a space ... Oh-oh I know where I went wrong. The names were arranged as several lines of around 8 words each and I did not preserve newlines so the last word of a line and the first word of the next line would become concatenated. Sorry for that. And thanks for the hand edits - I'll update the Python in the next ~24 hours. --[[User:Paddy3118|Paddy3118]] 14:16, 8 June 2011 (UTC)

:With the corrections, the list of names now contains 78 (edit: no, 79) entries.  This results in a big search space (over 200k (edit: over 300k) valid results and many orders of magnitude more sequences that must be considered earlier, before finding this "tiny remainder"), and I am not sure if that is desirable.  --[[User:Rdm|Rdm]] 16:28, 8 June 2011 (UTC)

::Agree.  Given that our current algorithms explore all possible paths, the current list of 79(?) might be rather tedious for some of the slower interpreted languages.  20 was proposed above.  The initial PicoLisp had a solution on a list of 64 names.  Maybe something in that range would be better?  &mdash;[[User:Sonia|Sonia]] 17:34, 8 June 2011 (UTC)

:::Oops, yes, it looks like 79 -- I need to run the silly thing again -- I need a bigger laptop if I am going to be working on this size of problem.  --[[User:Rdm|Rdm]] 17:50, 8 June 2011 (UTC)

I suggest we chop out the line "<code>mienshao milotic moltres monferno munna murkrow musharna nidoking noctowl</code>" to leave 70 pokemon and ask everyone to adjust. --[[User:Paddy3118|Paddy3118]] 20:27, 8 June 2011 (UTC)

::+1  :[[User:Sonia|Sonia]] 21:27, 8 June 2011 (UTC)

===Full List of Names, and Input to Tasks in General===

I have included my list of the 646 names at the bottom of the Racket implementation; it clutters up the implementation (and the page as a whole) in a manner that I feel is undesirable.
Is what I have done good form -- because on the other hand, people can use the code I have submitted, and they can use my list as a basis of their own.

* Can I "attach" the file to the page and refer to it via a link, so it doesn't have to appear?
* Can I put a "fold" in the markup?
* Should I host the file elsewhere?

Also, is there any way to "semantically" tag the list of 70 names in the task (or any of the sample input that is specified for tasks, in general) so they can be automatically scraped by implementations? --[[User:Tim-brown|tim-brown]] ([[User talk:Tim-brown|talk]]) 07:40, 22 May 2013 (UTC)

: Where we have a particularly long entry for a specific language, we often put that entry (or the bulky part of it) on another page.  For example, here, you might use Last_letter-first_letter/Racket.  And, here, since the full list of names was not a part of the task, I think it makes sense to leave the core entry in place and just move the "extras".

: But this is not the only possibility (for example, in some cases it makes sense to refer to an external site for "going further").

: As for the "raw data" aspect - hypothetically speaking, that should be addressed in the task description (other than that... it's just like any other issue that leads to bulk, so of course if your language context suggests something specific be said about the data you can express that). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:58, 22 May 2013 (UTC)

== Go "two interpretations of longest" ==

One of the interpretations of longest in the current Go entry seems to conflict with the informal description of the game -- you lose the game when you cannot add another word, so for example a 2 word sequence where each word has 12 letters would lose to a 3 word sequence where each word has four letters.  But this probably means that the task description should be unambiguous about wanting the longest sequence of words -- that the number of letters in the sequence is irrelevant.  --[[User:Rdm|Rdm]] 16:25, 8 June 2011 (UTC)
:Good point about the game.  I was just looking for any interesting twist.  With the accidentally concatenated names now separated, the printed lengths are now much more similar and the printed length is no longer very interesting.  I'll take that code out soon.  &mdash;[[User:Sonia|Sonia]] 17:13, 8 June 2011 (UTC)

:Thanks Rdm. What do you think of the altered wording? --[[User:Paddy3118|Paddy3118]] 20:20, 8 June 2011 (UTC)

::That sounds good to me (though I haven't tested anything quite yet -- busy right now, tomorrow I'll try to adapt to whatever changes you all will have implemented).  --[[User:Rdm|Rdm]] 20:31, 8 June 2011 (UTC)

Is this ready to promote to task now? --18:19, 20 June 2011 (UTC)

==Timings==
Could the timings on the D examples be removed or changed to just a statement of their relative run times under similar conditions, as we don't compare language run times on this site as it is [http://shootout.alioth.debian.org/dont-jump-to-conclusions.php too difficult a task to get right]. If a language has only one timing then I guess it should be removed altogether? Thanks. --[[User:Paddy3118|Paddy3118]] 18:36, 28 June 2011 (UTC)
: Those timings don't even list the CPU used on purpose, because they are meant to give just a general insight about the efficiency of the implementation. It's not so important to know that a better CPU runs the code a bit faster, but for the reader it's useful to know when a Python version takes about one minute to run, while the C code takes half a second. This site isn't a benchmark Shootout, but some information about the runtime efficiency of the code shown is interesting information for the readers. Languages and their implementations differ by syntax, semantics, efficiency, etc.
:: I am afraid I am going to have to disagree with you (whoever you are).  You can easily get order-of-magnitude differences from cpu speeds alone (for example, if you compared an older mini-itx system with a high-end modern system), and other issues (like system load or memory architecture) can also be significant.  (Not that imaginary timings were a particularly persuasive argument in the first place..)  --[[User:Rdm|Rdm]] 00:58, 29 June 2011 (UTC)
::: Do you want the removal of all timings then?
:::: Some timings can be worthwhile.  For example, when you have several different timings based on the same platform, and time differences greater than a factor of 2, those timings might be meaningful.  --[[User:Rdm|Rdm]] 03:54, 29 June 2011 (UTC)
:::: I would strongly advise against comparing timings of languages on Rosetta Code, except when comparing performance of different means of using the same language ''on the same hardware.'' Otherwise, I don't see the conditions Rosetta Code offers to be controlled, stable and consistent enough to result in the display of good, useful comparative data, and would far more likely result in destructive, uninformed comparisons used in advocacy by attack. That kind of data is what the Computer Language Shootout discusses. If, for some reason, their numbers are unsatisfactory, talk to them. If they disagree with you, talk to me, and I might talk to them. I have no illusion that I'd be able to convince them, but it's plausible there are some problems I could help address. I don't want RC being used as any kind of an authoritative source for something where we can't maintain the kind of QC necessary. --[[User:Short Circuit|Michael Mol]] 04:29, 29 June 2011 (UTC)

::::You really want to leave timings out except for the most general of comparitive terms, without the use of numbers and without changes in language or run environment is preferable to stop "miss-use".--
:::::Correct. If language-language speed comparisons of particular problems is truly desired, I'd need to set up some kind of controlled conditions. That would be painfully difficult to do properly and securely, considering competing interests and need to get language-specific tuning parameters correct. --[[User:Short Circuit|Michael Mol]] 12:57, 29 June 2011 (UTC)
::::::Also, the optimizations needed to get good speed (sometimes even just decent speed) out of certain tasks would make them hard to read, which IIRC was an important consideration. [[User:MagiMaster|MagiMaster]] 00:36, 30 June 2011 (UTC)
:::::::From time to time, I've seen people post "This method works, but is inefficient. (code example) A more efficient method would be to (narrative describing transform of code), like this: (alternate code example)." --[[User:Short Circuit|Michael Mol]] 13:50, 30 June 2011 (UTC)

== Alternative methods? ==

I am toying with an annealing kind of code to make longest chains, and it was able to come up with a 241 sequence in a minute or so (645 names).  For reference, for the 70 name sequence it was able to find a 21.  If the full list can't be brute-forced, maybe some approximate methods would be interesting. --[[User:Ledrug|Ledrug]] 02:22, 30 June 2011 (UTC)
