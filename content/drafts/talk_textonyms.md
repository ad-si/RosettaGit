+++
title = "Talk:Textonyms"
description = ""
date = 2015-02-11T15:47:42Z
aliases = []
[extra]
id = 18635
[taxonomies]
categories = []
tags = []
+++

== Needs specifics ==

I think this task is good except that it needs something specific to show to help verify and show how to use the functions we program. Maybe something like "show the number of keypad combinations that map to 10 words and show what other words have the same keypad combination as 'CAT'". Just something to sync the examples up. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 14:45, 3 February 2015 (UTC)

:I have updated the task dexcription to include specific output--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:28, 7 February 2015 (UTC)

== Example word list incomplete ==
It ends in the middle of "P". --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:48, 5 February 2015 (UTC)

: Will the truncation of that file/dictionary be fixed, or was that by design?   Perhaps a note or comment stating that the dictionary was intentionally reduced (and the reason); that would answer this issue. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:33, 10 February 2015 (UTC)

==Example word list has 'odd' words==
I calculated the number having the most words and got:

```txt
27: ['ap', 'aq', 'ar', 'as', 'ar', 'as', 'bp', 'br', 'bs', 'br', 'cp', 'cq', 'cr', 'cr', 'cs']
```

Those 'words' seem random to me.

I suggest we use the word list from task [[Ordered words]] i.e. this [http://www.puzzlers.org/pub/wordlists/unixdict.txt dictionary] although words with non a-zA-Z chars in them such as 10th will have to be rejected. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:19, 6 February 2015 (UTC)

:Changing the wordlist gives me these stats:

```txt
Read 25104 words from 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
Number of words after rejecting those that have characters other than a-zA-Z in them is 24978
Those words are represented by 22903 numbers

The numbers mapping to the most words map to 9 words each:
[('269', ['amy', 'any', 'bmw', 'bow', 'box', 'boy', 'cow', 'cox', 'coy']),
 ('729', ['paw', 'pax', 'pay', 'paz', 'raw', 'ray', 'saw', 'sax', 'say'])]
```

:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:51, 6 February 2015 (UTC)

:The code should work with any text list. I don't mind if the specific list isn't critical to the task. The advantage of the one I am proposing is that it includes names (i.e. Nigel and Paddy). The Perl entry has a wildcard feature that could be foreseeing 7733428483 8398. You may want a version which allows numbers. 2748424767 -> "Briticisms", "criticisms".--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:29, 6 February 2015 (UTC)

:: I agree with your statement of ''should work with any text list'' (dictionary).   However (see the talk section   '''duplicate words in dictionary''',   it depends on whether or not the dictionary contains duplicated words, and more importantly, how the computer program treats/handles those duplicates.   So, the inclusion of one particular dictionary can cause differences in the output of various computer programs, depending on, in this case, duplicate words.   Rejected words don't appear to be a problem;   but showing a count of rejected words would be a nice and easy thing to add as any dictionary is fair game. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:43, 10 February 2015 (UTC)

== What does "mapping" mean here? ==

(Obviously we are not talking about geography.)

The usual use of the word "mapping" in the context of programs is a reference to the process of using what a routine named "map" such as has been implemented in a variety of lisps (or javascript or perl or ruby or numerous other languages). In other words, it's talking about transforming some argument to some result. In other words, it's basically just a name for a function.

But I am having trouble making sense of that here, because there is not "textonym function" which is different from the identify function. We have words, we are looking for words that contain textonyms, but those words are not the result of some function, unless that function is a function which does absolutely nothing.

So... unless there is some useful meaning being conveyed here (and there might be? =), the word "mapping" is just gibberish. If it's meaningless, could we just remove it from the task description? Alternatively, if it's meaningful, I would benefit from knowing its definition. 

(And I do see a reference to keyboard mapping - but is that the same thing as a textonym mapping??) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:37, 8 February 2015 (UTC)

:I see maps! If a map forms a relationship between keys and value, (the common [[wp:Hash table|hashmap]] goes further and tells something of the implementation), then there is a mapping between individual digits and several characters from the alphabet, and we are asked to construct a mapping between digit strings and words from a dictionary that can be expressed in that alphabet.
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:22, 8 February 2015 (UTC)

:I hope labelling the table as digit key mapping and referring to it throughout as digit key mapping makes the meaning 100% clear.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:41, 8 February 2015 (UTC)

:: If it is the key mapping we are talking about in this context, shouldn't the report say "key mapping" instead of "textonym mapping"?  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:50, 8 February 2015 (UTC)

== Correct number of Textonyms in unixdict.txt? ==

The [[Textonyms#Go|Go]] and [[Textonyms#Phython|Python]]
examples disagree on the number of Textonyms in the <tt>unixdict.txt</tt> file.
The former claims 1473 while the later claims 22895.
Which (if either) is correct?

(The Go and [[Textonyms#J|J]] examples agree on 661 Textonyms in the [[Textonyms/wordlist]]; the other other examples don't give any other values).

:: They do now   (specifically, REXX).   See the talk section on   '''duplicate words in dictionary'''   (below). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:27, 10 February 2015 (UTC)

&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 23:33, 8 February 2015 (UTC)

: The J example was using the rosettacode wordlist and not the unixdict.txt file. I have updated it to also show results for the unixdict.txt file. I hope this helps. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:45, 9 February 2015 (UTC)

::Thank you. Since the J and Go examples now agree I've marked the Python entry as incorrect. I don't know enough Python to spot the error. &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 01:04, 9 February 2015 (UTC)

::: Another possibility is that the task specification is ambiguous. The question is: what is being counted? In the J and Go examples what we have done is count the number of unique digit combinations which correspond to more than one word. In the python example I imagine what is being counted is the number of non-unique digit combinations which correspond to more than one word. (For example, 2633 corresponds to three different words, so is counted as three textonyms in the python example and as one textonym in the J and Go examples.) The task specification is currently silent on this issue. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:07, 9 February 2015 (UTC)

:::: Good point, I don't think I can mark it "inconsistent" instead of "incorrect" tho :). I think the task is fairly clear "#{3} is the number of #{2} which represent more than one word." Only 1473 of the 22903 numbers "represent more than one word". The task should probably have included a short example that made the "correct" output more obvious. From what little I can guess from the python code, I thought/assumed that "<tt>sum(1 for w in num2words if len(w) > 1)</tt>" was counting the entries where <tt>len(w) > 1</tt>; if it's instead summing the lengths I think must be a mistake/typo given the variable name itself is <tt>morethan1word</tt> (rather than something like <tt>pairsOfTextonyms</tt>). &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 01:22, 9 February 2015 (UTC)
::::: When a digit combination is entered it maps to: A) Gibberish; B) An English word; C) Multiple English Words. Case C) are Textonyms. #{3}  is the number of digit combinations which fall into category C). So 1473 is the correct answer.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:33, 9 February 2015 (UTC)

==duplicate words in dictionary==

Consider the dictionary file:

```txt

aleph
aleph
aleph
bet

```

Question:   how many words are in that dictionary file?

If you say "two", then that affects the count of how many words can be represented by the ''key digits''   (as described by this Rosetta Code task).

In the REXX program that I coded, it detects duplicate words (and ignores them, but displays a count if non-zero).   I believe that having duplicate words shouldn't alter the count of words representable by ''key digits''.   As the REXX program is currently coded, it ignores duplicated words and it shows a different digit combination count   ('''650''' digit combinations instead of '''661''', the latter counts duplicate words and reflects another way to count words representable by ''key digits'').  

Better still, it would be nice to have a clean dictionary, or at the least, agree on whether or not duplicate words should be ignored   (and instead report on unique words that are in the dictionary).

It was asked (elsewhere):   "what is being counted?"   (by Rdm).   This is the crux of the ambiguity.

The   '''UNIXDICT'''   dictionary doesn't have that problem, fortunately.   In reality, almost all dictionaries have duplicate words (either by meaning, by use, by their derivation/root, by case/capitalization, or by whatever).   That shouldn't preclude the correct/accurate counting of (unique) words. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:27, 10 February 2015 (UTC)

:If someone has a non-unique word list they should pipe it through <tt>uniq</tt> or <tt>sort -f -u</tt> (or wrt Rosetta Code, see the relevant task for uniquely filtering). &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 20:04, 10 February 2015 (UTC)

:: Well, the '''if'''    ···   '''IS'''.   That is, the   ''someone''   is Rosetta Code (or at least, the holder of that file), and the '''Textonyms/wordlist''' dictionary file does contain duplicate words, and it (the dictionary file) is referred to as a possible example dictionary to use (from the Rosetta Code task description).   It shouldn't have to be massaged or piped though a filter to solve this Rosetta Code task.   Furthermore, the ''uniq'' or ''sort'' (or any specific tool) isn't necessary to weed out duplicates.   Of course, that is, if duplicate words are to be rejected/ignored, and so far, nobody has rung that bell yet.   I went proactive (for the REXX programming solution) and ignored/rejected duplicate words as it appeared the correct manner in handling duplicates. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:27, 10 February 2015 (UTC)

::: It would be senseless to say that aleph is a textonym for aleph so duplicate words should be rejected, and rejected words should not be counted, so any duplicates should only be counted once. The dictionary my real time spell checker is using at the moment doesn't think aleph is a word, but our wordlist knows better. 

:::: I assume we all know that 'as' is a word.
:::: 'AS' is a qualification in UK schools approximately equivalent to half an A level.
:::: 'As' is a cuneiform symbol well known to those who have read the original Epic of Gilgamesh or followed closely the 14th.C BC correspondences to the Egyptian Pharohs. Maybe this latter is a little too rosetta.

::: 'AS', 'As', and 'as' are textonyms. The wordlist is meant to be clean, so any duplicates, that actually are duplicates, are in error and can be removed --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:47, 11 February 2015 (UTC)
