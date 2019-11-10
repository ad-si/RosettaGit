+++
title = "Talk:Anagrams"
description = ""
date = 2016-08-21T19:22:35Z
aliases = []
[extra]
id = 3053
[taxonomies]
categories = []
tags = []
+++

==not acronyms==
Yep, I know now, they are not acronyms. --[[User:Paddy3118|Paddy3118]] 20:34, 24 September 2008 (UTC)
:Do you want to call them "anagrams" instead? --[[User:DanBron|DanBron]] 18:04, 25 September 2008 (UTC)
That's it! Of course they are. I must have had a brain seizure!! --[[User:Paddy3118|Paddy3118]] 20:51, 25 September 2008 (UTC)

==Local word list==
Could an administrator make a local copy of the wordlist? --[[User:Paddy3118|Paddy3118]] 05:37, 2 October 2008 (UTC)

== Where to read wordlist from? ==

The Haskell and J versions assumes a local copy of the file, while the other two examples so far open the URL directly. What is the intention behind the task?

I would consider opening the URL directly an unnecessary complication: Not every language allows easy access to the internet, but the rest of the task is basically algorithmic, and therefore easily accessible to any language. If the internet access is indeed intended to be part of the task, I propose splitting this task into two separate ones:
# read a file from the web
# find most words of equal characters based on a file of words
That would IMHO be better, because it's more modular. --[[User:Ce|Ce]] 16:35, 2 October 2008 (UTC)
:"Read a file from the web" already has [[HTTP Request|a task made for it]]. Either way, I also think getting rid of the web file requirement is a good idea. --[[User:Mwn3d|Mwn3d]] 16:54, 2 October 2008 (UTC)
:I think that programs should not be prohibited from getting the list from a local file, a web resource, or even a previously defined variable — the point is the algorithm, not the means of getting the input data (which should be chosen to best fit the language). I also think having a specific word list list adds concreteness of the task, and a common test data set — but it should not be interpreted as a ''requirement'' to download that list at the time of execution. --[[User:Kevin Reid|Kevin Reid]] 19:14, 2 October 2008 (UTC)

My original idea was to include getting the data from the web, as people do use the web to disseminate data. On Unix it is quite easy to string together a script to download the file using one program, and process it with another, but if your language has an easy way to download the data from the web, then it would be good to show it. If not, then assume you read the input from a file. --[[User:Paddy3118|Paddy3118]] 21:35, 2 October 2008 (UTC)

==error in ooRexx ?==

I got a syntax error in line 12 of the ooRexx example (using ROO).
Is this a bug in  ROO  or is it something else?

I'm not fluent in ooRexx speak, so it may be really something simple. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:17, 10 June 2012 (UTC)

I am using REXX-roo 4.00 -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:22, 10 June 2012 (UTC)

What is the error and which version/source line (text)??
No problem with ooRexx!
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:56, 6 May 2013 (UTC)

[[ooRexx]] and roo!<sup>TM</sup> use completely different conventions to glue Object Oriented capabilities onto Rexx and as such it's unlikely that any non-trivial &quot;Object Oriented&quot; ooRexx program will run with the roo!<sup>TM</sup> interpreter and vice versa.--[[User:Alansam|Alansam]] ([[User talk:Alansam|talk]]) 03:39, 7 May 2013 (UTC)

==FBSL calling C: Is it useful?==
I think we may need a task that highlights some languages ability to use another language by embedding the source to the second language in the first, (if it is supported as a documented feature of the language); but the wholesale copying of the C entry for use in FBSL might not be the best placve to show this feature. I don't think it fits here.

What do others think? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:07, 6 May 2013 (UTC)


:You may call me "others" but in fact I'm taking the liberty of talking on behalf of the FBSL Dev Team.

:FIRST, FBSL is not "calling C" and is not "embedding the source" for other languages in itself here in the interactive way as e.g. Lua clones do. Of course it can do it as well in its DLL hypostasis but this is not what you're seeing here or in the Ackermann challenge and will be seeing in many more solutions to come.

:SECOND, Dynamic Assembler and Dynamic C JIT compilers are indispensable features of FBSL alongside its interpretative environment. They do not require any add-ons, add-ins, plug-ins, DLLs, archives etc. etc. etc. They are already there and they interact with their interpretative parent and they exchange data with one another and they are environmental extensions of one another. They are not seen from outside and their code is not accessible for anyone but FBSL itself. The DynAsm and DynC code::blocks are FBSL's subs and functions exactly like its own interpreted subs and functions. You cannot separate them in a very much the same way as you wouldn't be able to separate the Siamese twins or they'll die.

:THIRD, the decision of what intrinsic feature of a language to select for a particular task is undoubtedly the programmer's prerogative as long as the task is resolved within the context of its rules. :RosettaCode is not the place to generate proprietary/patented/closed-source solutions and more than half of its code base here are ports from one another's code. My solution ''does not'' claim originality. On the contrary, it clearly states it's a verbatim copy totally in accordance with the GNU spirit of this place. But it is unique in that no other language whatsoever can boast such a solution. Who else can claim 100% compatibility with another, but any other language present at RosettaCode?

:FOURTH, the unavailability of such a section as you suggest may be a clear indication that there are no other languages that are capable of doing the same. You may try to introduce one but I'm afraid, FBSL will be the only participant in there for a very very long time to come.

:LAST, but not the least. We at FBSL cannot be held responsible for other devs not having time or insentive enough to accomplish what we have accomplished in FBSL. We are not saying others are worse. What we are saying is we are different and more versatile than many. Despite our remaining basically BASIC.

:Yes, my Anagrams decision was a show-off and a challenge for "others" who can't claim compatible functionality. But it was clearly stated as such. This is not the first time in our 12-year old history that "others" do not like us for what we can do while they can't. But calling us an ugly duckling and trying to impose restrictions not applicable to other contestants would be totally unfair and contrary to the spirit of this place.

:Kind regards,

:TheWatcher

::
Woa. Hold your horses their TheWatcher! So are you saying that FBSL contains both a dynamic assembler and a dynamic C JIT (is that a compiler or interpreter) as subsets of the language? That was not made clear in [[FBSL|the language category page]], or in the introduction on the FBSL home page.

::This is supposed to be a site for comparing language features. You should not be surprised if someone sees one languages entry copied and used for another language and questions it.
::Since FBSL has this capability it might be best to show it by showing the FBSL specific framework necessary and then only any differences to the C entry with the rest ellided so that language comparers can concentrate on how FBSL can use stock C - as well as maybe a second answer written in mainly that subset of features FBSL that are not shared with the C JIT. Actually what I have written may be wrong as I do not know how well integrated C is with FBSL. At the moment, the FBSL entry looks like a wrapper for a C compiler/interpreter/JIT. You could use the site to show why it is more than that with careful explanation and use of unique or near-unique features.

::Yep its tough when you're different - just ask the [[J|J language]] guys. But this site gets better by encouraging more language entries and surely if you are different, then you should be able to show the benefits. Those J people have several times explained in depth how their unique solutions work and we could see how their language helps them to think in ways that can generate elegant solutions.

::If you are that different then you may have to meet your audience half way. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:23, 6 May 2013 (UTC)

::: The interesting issue, here, I think, is: how complete is this entry? How hard is it for another programmer (like [[User:Paddy3118|Paddy]]) to make this solution work? [This is an exercise I often try, when I see something in a language that I am curious about.] Looking at the language page, FBSL supports grammar and syntax of several languages. Perl6 has analogous plans. I think that the primary issues here are syntax (which can be abstracted within a ''top level block scope'' if it's simple enough) and semantics - which mostly just eat storage for the implementation. There can also be issues of names and contexts (scopes) but that only matters when they are dirty (when they appear on the interface boundaries between modules). Given modern machine sizes (I'm composing this on a laptop with 16GB ram and half a terabyte of disk), and Moore's law, I expect more of this "melding of languages" in the future.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:22, 6 May 2013 (UTC)


=== FBSL "calling" C - part 2 ===

:Sorry guys, I'm not yet familiar with this site's ways of communication so I'm writing this by direct editing of the page.

:@Paddy: There's no need for excessive verbosity. Please take a deep breath and re-read my message again. I'm not just another script kiddie to talk to as if I were a moron.

:Quote Paddy: So are you saying that FBSL contains both a dynamic assembler and a dynamic C JIT (is that a compiler or interpreter) as subsets of the language? Unquote
:Quote TheWatcher: Dynamic Assembler and Dynamic C JIT compilers are indispensable features of FBSL alongside its interpretative environment. Unquote
:So what's not clear to you in this description? FBSL is an interpreter and its intrinsic DynAsm and DynC layers are JIT compilers that convert their listings into native (a.k.a. executable, a.k.a. machine) code in memory at app start concurrently with FBSL's own bytecode they are supposed to interact with.

:Quote Paddy: This is supposed to be a site for comparing language features. Unquote
:Now you can compare FBSL that has such a language feature with other languages that haven't.

:Quote Paddy: You should not be surprised if someone sees one languages entry copied and used for another language and questions it. Unquote
:I am not surprised at all at such jealosy. Just show me one other language where you can copy-paste the C solution into ''except'' C proper ''and'' FBSL to see it run happily.

:Quote Paddy ... how FBSL can use stock C ... Unquote
:In any way one likes. The rest depends on one's literacy in C.

:Quote Paddy: ... I do not know how well integrated C is with FBSL. Unquote
:Please see above.

:Quote Paddy: the FBSL entry looks like a wrapper for a C compiler/interpreter/JIT. Unquote
:It was my deliberate choice. Perhaps, someday I'm gonna add a handful of lines as a pure BASIC (Freestyle BASIC Script Language, that is) alternative to the same task. As for "compiler/interpreter/JIT", please see above.

:Quote Paddy: You could use the site to show why it is more than that with careful explanation and use of unique or near-unique features. Unquote
:Perhaps I will, if you don't scare me off of it before I do.

:Quote Paddy: Yep its tough when you're different... Unquote
:I'm leaving that out to your own conscience as irrelevant to the topic in question.


:@Rdm:

:Quote Rdm: How hard is it for another programmer (like [[User:Paddy3118|Paddy]]) to make this solution work? Unquote
:Perhaps Paddy even tried to implement it in his own language? "Been there", Paddy? "Done that"?

:Quote Rdm: Perl6 has analogous plans. Unquote
:Glad they are following the same trends. Means my own vision of the future is not unreasonable.

:Quote Rdm: Given modern machine sizes... Unquote
:No way. The Fbsl.exe v3.5 executable's footprint is only 580KB of un-exepacked binary code for everything it has to offer. No dependencies except for the Windows' standard system DLL's.

:Now thanks for this fruitful discussion. So what is the verdict?

:Kind regards,

:TheWatcher

:: Editing the page is usually fine (though you can also edit a single section - there's an "edit" link on each one).  We often use deeper indent to indicate responses. I removed some extra blank lines from your last update. I've also added a subsection header to mark off where your response was not indented. You should probably consider using <nowiki>~~~~</nowiki> to sign your content (this will be expanded into your user account reference and a timestamp, when you save your changes).

:: But I think you misunderstood me when I wrote "make this solution work" - I meant "make the FBSL solution work" - in other words, given your code and an implementation of FBSL, how hard is it to make it work? (It should be simple.)

:: Note also that Perl6 has been in the works for something like 10 years (maybe longer, I have not looked up its history as I am composing this) - it has some rather ambitious goals.

:: I think you also misunderstood my comment about sizes. I was not saying that your implementation had to be huge (especially since you are platform specific and relying on windows - which is "huge"), but instead was saying that the resource constraints which motivated the design of many current languages (BASIC, C, ...) are being relaxed. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:47, 6 May 2013 (UTC)

::Yo The Watcher.
Try re-reading my posts with a less defensive attitude, not everyone is out to get you.
::I find it hard to judge how to reply to people and do try not be offensive. Well, you didn't think I was being offensive, just maybe talking down to you. OK I heard that. As for any ''verdict'', I don't think this is a ''trial''. I did however add some constructive notes on how you could make your language shine in my last post, why not try them? Or not. Whatever... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:55, 6 May 2013 (UTC)

::: @Rdm:
::: Thank you for your heads-up on editing the sections. It was easy enough for me to do that on the task pages but the <nowiki>~~~~</nowiki> trick somehow slipped me though I noticed your elegant way of signing your correspondence. :)

::: It would've been very easy for Paddy endeed had he only cared to try. Double-clicking that very script's icon with FBSL v3.5 installed on his PC would've brought those console results up on his screen, and the anaout.txt file, down on his hard disk, in about 100 milliseconds or less.

::: I ''am'' very impressed by what the Perl6 guys have been doing all this time and I was very glad to hear they were considering the same approach. Moving further along these lines can bring about a new generation of languages that may once fill the "Integrated Development Environment" concept with entirely new meaning.

::: As for the motivation, I'd say slacker resource constraints have given a new impetus to language development and I don't see why language developers shouldn't take advantage of this opportunity. Of course, unless they are already out of bussiness... :)

::: @Paddy:
::: I've been on the defensive side so many times in my long life that one time more or one time less doesn't really make much difference. Yet quarreling is not my natural state and I shall try to reconsider your points in a more constructive way. Time only will show if I can take at least some of them as my guidelines. Or not. Whatever... ;)

:::Kind regards,
:::TheWatcher 17:23, 6 May 2013 (UTC)

:::: First, understand that not everybody has the necessary software to run everything on this site. Second, understand that there's a large difference between the amount of effort perceived to be required to test a thing, and the amount of effort actually required. Third, understand that people familiar with a thing tend to greatly underestimate the degree of familiarity required to work with it to the same time efficiency. Fourth, understand that both Rdm and Paddy are long, long-standing constructive contributors on this site. They are almost invariably open to discussion in good faith, and it would be wise not to assume otherwise. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 20:34, 6 May 2013 (UTC)
:::: In any case, welcome to Rosetta Code. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 20:34, 6 May 2013 (UTC)

::::: Dear Sir,

::::: Am I really producing the impression of a kidster that needs to be led by the hand in the wonderland of computers? That sounds like a complement to me, regarding my age. Do I need to have Python or Perl6 or Mathematica installed on my PC to estimate or admire the efficiency of their solutions? Or do I need to have Brainfuck or LOLCODE installed to spend a couple of minutes laughing at their code out loud of sheer amusement? Are my own sight or judgement failing me when I'm looking at somebody else's code? No, they aren't. Why? Because while either admiring or laughing, I'm not really interested in using but any of them for my own work. That's why I'm taking the authenticity of their solutions for granted. And I rely on people's common sense to be able to stand sanity checks of all sorts whenever anyone asks them to demonstrate their code ''in vivo''.

::::: Further, I've got nothing against having a couple or two moderators at a site of this size but I will not stand that sort of a-good-cop-and-a-bad-cop attitude. It wasn't I who started this discussion in the first place and it wasn't I who took the liberty of jumping to conclusions what's wrong and what's right with and for FBSL. And all that from a man who gave up all hope for a language of his own long, long ago. Neither will I stand that didactic tone and let the bearer of the voice get away with it. For all I care, Paddy may just blame me, or rather his own self, for my not being able to understand his particular sort of English humor.

::::: And lastly, I came here in good faith and was having fun solving puzzles and finding my own ways feeling equal among equals. But I was suddenly stopped by sharp hammering of a gavel from above. Or can it be that it was Paddy's code that I've ^^stolen^^? Otherwise I see no reason nor excuse for his immature judgment unbecoming to a man who's supposed to be "long-standing" and moreover, "constructive". There is just one positive side in this entirely fruitless affair. Now Paddy will know for sure that there will be noone else present in his projected reservation area for multi-syntax HLL's '''but FBSL''', just because there's none other available yet. Somehow he missed that fact entirely. That's that Paddy. Live and learn.

::::: Anyway, thank you for your warm welcome, Mr.Mol. That is exactly what I felt was missing since all that mess had started. May I regard it as an invitation to proceed to letter B in the task list as I intended to when I was stopped midway on my way there?

::::: Kind regards,
::::: Michael Lobko-Lobanovsky
::::: FBSL Development Team

:::::: Michael, there is no hammering of a gavel from above. Or, at least, if there is, that's ''me''. I'm the benevolent dictator that typically stands back; RC has, for five or six years, largely managed to get by with a relatively loose set of rules and norms, with a bunch of people who are not infrequently by nature at cross purposes muddling their way through and coming up with mutually amicable results. I myself only step in very rarely, and when I do, it's usually to resolve a technical legal issue surrounding the site (that's the trouble with being a "sole proprietor"...) or, on occasion, intercepting conversations which appear to be drifting more to the heat of argument than actual communication. It's the latter of those two activities for which I make most of my appearances, and I only have to do that every 2-3 months or so, on average. But it happens, and it happens because humans are humans, and communication of intent is an inherently faulty thing, and I sometimes have to toss a bucket of water on things when I see sparks. That's just the nature of the thing. The people who come here and stick around genuinely have common interests. They may have a basis in advocacy, of education or of learning, but their interests are all served by figuring out how to work with each other.--[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 02:29, 7 May 2013 (UTC)
:::::: The single, biggest thing you need to pick up here (and this is why I'm speaking up), is that the people on this site do not intentionally take adversarial roles. And whether or not you believe someone else was adversarial first, it's vital that you not allow an adversarial attitude to express itself in what you writing; perhaps they had a bad day, perhaps not, but adding fuel to the fire won't help. (I.e. "Now Paddy will no for sure that...That's that Paddy. Live and learn." That's adversarial, and you do no favors for yourself or anyone else by taking side shots that way.) I'm not going to name names, but I can think of incidents around at least three current major contributors to the site who, when they first arrived, had adversarial mindsets. To some extent, they still do...but they've learned to draw themselves away from the heat of argument and back toward communication, and they work out incredibly constructive agreements, sometimes after a day or two of completely talking past each other. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 02:29, 7 May 2013 (UTC)
:::::: Again, I'm not bringing down a gavel. This is more like a hand on the shoulder and gently saying "keep your shirt on." --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 02:29, 7 May 2013 (UTC)
:::::: Oh, and don't call me "sir", unless you're sending me a generic legal document; this place isn't that formal. :) --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 02:29, 7 May 2013 (UTC)
::::::: Michael, I know who you are to this site hence my "sir". I was addressing the arbiter for a final judgment so my message was sort of legal-looking. I was prepared to abide by any possible ruling but frankly, I was only glad to find so much common sense and intelligence in what I finally got. Thank you. I'll adhere to what you're suggesting and I'll try to concentrate on my creative work more than on concomitant counter propaganda. :) TheWatcher 09:46, 7 May 2013 (UTC)

== REXX ==

words should be shown as they are (not uppercased) !?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:21, 5 August 2013 (UTC)

: I added the uppercase because I didn't assume the dictionary (wordlist) is in any particular case (or the word may be capitalized).   As it stands, the dictionary being used is all lowercase.   '''EVIL''' is still an anagram of '''vile''' and '''Live'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:08, 5 August 2013 (UTC)

-----

It may be noteworthy to mention that timings (see ooRexx) for various languages have been discouraged on Rosetta Code.

 http://rosettacode.org/wiki/User_talk:Paddy3118#Comparisons

(Especially for examples that no longer exist!)   Getting repeatable results is problematic.   Also, not knowing what hardware and/or operating system and/or which compiler/interpreter (or version) was used, or for that matter, what ''code'' (program statements) were used,   etc,   makes it difficult to judge the veracity of the timings. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:33, 5 August 2013 (UTC)

:: You could have said: "thanks for making me improve my code (significantly) no matter what hardware is used." But he who expects nothing shall not be disappointed. The example still exists in rosettacode's history. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 11:40, 5 August 2013 (UTC)

There seems to be an error in the REXX code. Applying the program to a very small dictionary

```txt

abc
acb
bac
bca
cab
cba

```

yields

```txt

------------------------------ 6 words in the dictionary file:  u.txt
     ABC     ACB BAC BCA CAB

Found 1 words  (each of which have 4 anagrams).

```

whereas the ooRexx programs yield

```txt

abc acb bac bca cab cba

```

which is what I have expected. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:01, 5 August 2013 (UTC)
: Maybe I introduced this error with my modifications. pls verify the original! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:03, 5 August 2013 (UTC)

:: No, you didn't introduce an error.   It was an overzealous filtering statement that I put in to optimize the reading.   I removed the offending statement and all is now well.   Thanks for finding that error (it only manifests itself if the number of anagrams exceed the number of letters in the word). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:17, 5 August 2013 (UTC)

-----

<strike>
The REXX version 2 program won't find all the anagrams if some words are in mixed case.   Furthermore, if all the words are in uppercase, no anagrams are found:
</strike>
<strike>

```txt

There are 0 set(s) of anagrams with 24819 elements each:

```

</strike>
I suspect that many of the programming examples won't handle a mixed case dictionary   (should treat '''Live''' as an anagram of '''EVIL''').

Since the entire dictionary '''unixdict.txt''' is in lowercase, the mixed case error situation will remain hidden for most programs.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:06, 8 August 2013 (UTC)

: REXX version 1 is now close to perfect. thanks --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:47, 9 August 2013 (UTC)

:: I've added a faster version   (but it has it drawbacks). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:13, 12 August 2013 (UTC)

::: After adapting the new versions (=; -> =<nowiki>''</nowiki>; @ -> aa # -> nn) and adding the invocations for time('R') and time('E'), I used this driver

```txt

  call gsana11
  call gsana12
  call gsana13
to get these timings on Windows/ooRexx
  1.1 1.279000
  1.2 1.201000
  1.3 1.139000
PS I had to add a call lineout ifid in order to close the
input file before staring the next program.
I was severely burnt when not closing a file on the host
when the next program used 'my' ddname and the allocated dataset.

```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 16:07, 12 August 2013 (UTC)
