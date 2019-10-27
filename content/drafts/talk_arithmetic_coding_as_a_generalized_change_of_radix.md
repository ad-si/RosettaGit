+++
title = "Talk:Arithmetic coding/As a generalized change of radix"
description = ""
date = 2016-02-01T23:47:52Z
aliases = []
[extra]
id = 19982
[taxonomies]
categories = []
tags = []
+++

== Goof? ==

The J entry gives different values for most of these examples than Go/Perl/Perl 6/Ruby/Sidef.

The difference seems to be related to the difference between "frequency" and "cumulative frequency".

The wikipedia writeup uses frequency when describing the "As a generalized change of radix" mechanism, and then introduces "cumulative frequency" when describing how to reverse the process.

Anyways, it looks like Go/Perl/Perl 6/Ruby/Sidef are using a different algorithm than the one described in wikipedia, as they all use "cumulative frequency" rather than "frequency" when encoding.

Or perhaps the wikipedia page is in error? Do we have anyone with sufficient background in the history of this algorithm to make that determination? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:44, 27 January 2016 (UTC)

: I am not familiar with J. But the number generated could be different if a different cumulative frequency dictionary is used. One commonality of the Go/Perl/Perl 6/Ruby/Sidef solutions is that they generate the cumulative frequencies by accumulating in ascending order of the character codes (because they check keys from 0 to 255), so they always generate the same cumulative frequency dictionary. However, the order of characters when accumulating is not relevant to the correctness of the algorithm. If you accumulate in a different order, you will get a different cumulative frequency dictionary, but as long as decoding uses the same cumulative frequency dictionary (i.e. it generates it by iterating through the frequency dictionary in the same order), it will produce the correct result. I can't read J, but if the J solution simply iterates through the frequency dictionary in an unspecified by consistent (not necessarily ascending) order of the keys, then it could generate a different cumulative frequency dictionary, and hence a different number. I am not sure if this is really what is happening. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 10:40, 28 January 2016 (UTC)
:: The J implementation does not use <code>cumulative frequency</code> for this task. The wikipedia clearly specifies that <code>frequency</code> and not <code>cumulative frequency</code> should be used for this task. Instead, it introduces <code>cumulative frequency</code> for the decoding operation. 
:: So why are people using <code>cumulative frequency</code> for this task? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:24, 28 January 2016 (UTC)

::: I'm not sure if I understood your point correctly, but maybe this will clarify something. As Wikipedia describes it, in the encoding process, the formula to calculate the lower bound <math>L</math>, is:
::::<math> \mathrm{L} = \sum_{i=1}^n n^{n-i} C_i { \prod_{k=1}^{i-1} f_k } </math> where <math>\scriptstyle C_i</math> are the cumulative frequencies and <math>\scriptstyle f_k</math> are the frequencies of occurrences.
:::: After that, we compute the upper bound as follows:
:::::<math> \mathrm{U} = \mathrm{L} + \prod_{k=1}^{n} f_k </math>
:::: After this point, it doesn't matter which number <math>N</math> we return, as long as <math>L <= N < U</math>. --[[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 17:28, 28 January 2016 (UTC)
::::: For some reason, math markup isn't working right now, so that's a bit hard to read. But basically you seem to be restating the fact that you are using cumulative frequencies? But my question is why are you using cumulative frequencies? -- the use of cumulative frequencies conflicts with the example and text at [[wp:Arithmetic_coding#Arithmetic_coding_as_a_generalized_change_of_radix|wikipedia]]. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:35, 28 January 2016 (UTC)
:::::: The Wikipedia section uses cumulative frequencies. In the example, "DABDDB", gets mapped to 3, 0, 1, 3, 3, 1, where 0, 1, 3 are the cumulative frequencies for A, B, D, respectively. You can follow how those are used throughout the calculations.
::::::As another example, consider a situation where each character has frequency 1. Then the frequency dictionary is just every key mapped to 1. What you need for encoding is the cumulative frequencies 0, 1, 2, ..., which tells you what value to assign to each character.  --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 19:43, 28 January 2016 (UTC)
::::::: No. 
::::::: Here's the [[wp:Arithmetic_coding#Arithmetic_coding_as_a_generalized_change_of_radix|wikipedia]] expression for L. It won't render properly right now, but you can go to the wikipedia page to read it:
:<math>\begin{align}
  \mathrm{L} = {} &(6^5 \times 3) + {}\\
                  & 3 \times (6^4 \times 0) + {}\\
                  & (3 \times 1) \times (6^3 \times 1) + {}\\
                  & (3 \times 1 \times 2) \times (6^2 \times 3) + {}\\
                  & (3 \times 1 \times 2 \times 3) \times (6^1 \times 3) + {}\\
                  & (3 \times 1 \times 2 \times 3 \times 3) \times (6^0 \times 1) {}\\
             = {} & 25002
\end{align}</math>
::::::: The frequencies used here are 1 (A), 2 (B) and 3 (D). The value 0, and 1 and 3 correspond to the letters A (0), B (1) and D (2) - and you can see that in the wikipedia page where it states "The string is first mapped into the digit string 301331" - so that 0 has nothing to do with the concept of cumulative frequencies.
::::::: If I were to criticize the wikipedia page, I'd ding the author for choosing DABDDB as an example (because the letter encodings coincide exactly with the cumulative frequency values - so that's confusing).
::::::: But if I were being critical, I'd also criticize whoever designed this "as a generalized change of radix" algorithm for choosing the length of the string as the base for the encoding scheme. If anyone were to try to actually use this encoding scheme, they'd encode THE in base 3, using the encoding values T=19, H=7, E=4.
::::::: But, I guess that answers my question about why people are using cumulative frequencies in the encoding implementations: (a) the algorithm itself is basically useless, so no one should care about using it, and (b) the description in wikipedia uses an example where cumulative frequencies are easily confused with the letter encoding values.
::::::: All in all, it's a mess. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:57, 28 January 2016 (UTC)
:::::::: The example used on Wikipedia is indeed very confusing. Now I understand your point; you're suggesting using the corresponding byte values in encoding (or the alphabetical positions for alphabetical strings), instead of the cumulative frequencies. I don't know if this is better or worse, but if you choose to do that in the encoding process, you also have to do the same in the decoding process, otherwise you will get different results. For example, "ABRACADABRA" encoded with positional values, with result into "4860830 * 10^4" (same output as J). If we decode it back, using cumulative frequency instead, we'll get "AARAAAARAAA", which is incorrect.
:::::::: After all, the Wikipedia page seems to be contradictory, as it mentions both mapping the string to digits (not saying which digits), and bellow referring to <math>\scriptstyle C_i</math> as the cumulative frequency of the current symbol. I agree, it's a mess... 
:::::::: (Off-topic: to enable math markup, go to [[Special:Preferences#mw-prefsection-rendering]] and select ''MathML with SVG or PNG fallback'' at the bottom of the page) --[[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 21:04, 28 January 2016 (UTC)
::::::::: Thank you, (and actually someone else adjusted that setting on my behalf), that worked around the problem with PNG rendering. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:45, 30 January 2016 (UTC)

:::::::: I don't understand what your confusion is. 0, 1, and 3 are the cumulative frequencies for A, B, and D respectively. The cumulative frequency = the sum of the frequencies of the letters accumulated before it. So if we choose to accumulate in the order A -> B -> D (in order of increasing character code), then for A, the cumulative frequency is 0 (since there's nothing before it); for B, it's the sum of the frequencies for A = 1; for D, it's the sum of the frequencies for A and B = 1 + 2 = 3. If there were another one after that, say Q, its cumulative frequency would be the sum of the frequencies for A, B, and D = 1 + 2 + 3 = 6. That is cumulative frequency. The letters themselves are irrelevant; it's just the frequencies of each and order that we accumulate them that is relevant. It has ''nothing'' to do with "letter encoding values". --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 01:15, 29 January 2016 (UTC)
::::::::: I do not think you read what I wrote. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:28, 30 January 2016 (UTC)
:::::::: The purpose of all this is to divide the interval from 0 to 1 (or in this case, the interval from 0 to (length of string) since they want to use all integers) fully into intervals for each of the characters, where the length of the interval for each character is proportional to its frequency. So we want an interval of length 1 for A, length 2 for B, and length 3 for D. The implementations here accomplish that by assigning them from 0 upwards to the characters in ascending character order. So A gets the interval 0-1, B gets the interval 1-3, and D gets the interval 3-6. So each character gets an interval represented by the start being its "cumulative frequency", and the length of the interval being its frequency. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 01:15, 29 January 2016 (UTC)
:::::::::: I considered that interpretation, but for it to work you'd need a dictionary to figure out which letters were in use, and that's not part of the definition. Worse, though, this conflicts with the writeup and example at [[wp:Arithmetic_coding#Arithmetic_coding_as_a_generalized_change_of_radix|wikipedia]]. I mean, yes, the choice of the length of the string as the number base sort of implies something like this, but using cumulative frequency to identify the letters is a worse choice than fixed letter values because you do not know which letters have which frequencies. Also, if you read the wikipedia page, you will see that it says "A = 0, B = 1, C = 2, D = 3" and you can't get C = 2 for that example if you are pretending that these numbers represent cumulative frequencies. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:28, 30 January 2016 (UTC)
::::::::::: "Fixed letter values" doesn't make sense. A coding needs to divide the interval 0-1 into non-overlapping intervals for each character. Huffman coding is the optimal length coding where intervals are all restricted to the size 1/(some power of 2). Arithmetic coding is the optimal coding where the intervals can be any size, and mathematically, the optimal length coding is one where each character's interval is the same size as its probability of occurring, so if "DABDDB" were representative of the letter frequencies, then A should be given an interval of size 1/6, B given an interval of size 1/3, and D given an interval of size 1/3. (The "generalized change of radix" representation simply assumes a denominator of "base" (6), so the interval sizes are 1, 2, and 3, respectively.) How does T=19, H=7, E=4 (as you suggest) help us divide up 0-1 into intervals? It doesn't. And this is not just for letters; this is for all unit symbols, so for bytes there are 256 characters. How are you going to divide up 0-1 into intervals? If you are just going to give an interval of size 1/256 for each character, that's not a (remotely optimal) coding -- that's just the original plaintext binary. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 18:20, 31 January 2016 (UTC)
::::::::: That said, using cumulative frequencies in place of letter values for your encoding means words like THE, ONE, TWO, DOG, BAD, etc. etc. all encode to the same number. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:45, 30 January 2016 (UTC)
:::::::::: Yup, they SHOULD all encode to the same number. That's the point, because they have the same set of frequencies. It's the dictionary that allows you to decode them. In Huffman Coding, all of those should also similarly encode to the same binary code, and it's the dictionary that allows you to decode. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 21:35, 30 January 2016 (UTC)
::::::::::: Well, I guess it's true that huffman coding implies a shared dictionary between the encoder and the decoder. And, there is an explicit analogy drawn here with huffman coding. But if that's the case, we either need to change the wikipedia page or we need to spell out this issue in the task requirements or both. And if this is the "correct definition" but we "cannot change the wikipedia page" then we should also address the conflict in the task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:34, 30 January 2016 (UTC)
:::::::::::: I don't see anything wrong with the Wikipedia page. Can you point to any statement on the Wikipedia page you think is incorrect? --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 18:20, 31 January 2016 (UTC)
:::::::::::::Ok, I will make these points again. Given how many times I've already mentioned the C = 2 issue in the context of the DABDDB example, I do not hold much hope that this will be the last time you ask me to state them, but I don't know what else to tell you. The wikipedia page currently states:
:::::::::::::: we may look at any sequence of symbols:
::::::::::::::
:::::::::::::::<math>DABDDB</math>
::::::::::::::
:::::::::::::: as a number in a certain base presuming that the involved symbols form an ordered set and each symbol in the ordered set denotes a sequential integer ''A'' = 0, ''B'' = 1, ''C'' = 2, ''D'' = 3, and so on.
::::::::::::: and this conflicts with (or, perhaps, illustrates) your above statement that ''"Fixed letter values" doesn't make sense.'' If you do not see that, please read that again, and notice the '''C = 2''' part as well as the descriptive text which supports C = 2.
::::::::::::: But, also: '''"The cumulative frequency is the total of all frequencies below it in a frequency distribution (a running total of frequencies)"''' on the wikipedia page conflicts with a cumulative frequency encoding of letters and 1150764267498783364 as the encoding for 'TOBEORNOTTOBEORTOBEORNOT'. (To get 1150764267498783364, I needed cumulative frequencies in alphabetic order rather than in frequency order.)
:::::::::::::That said, I am not maintaining that the wikipedia page is incorrect - merely that it conflicts with some parts of a number of the implementations of this task. (Well, that, and mostly useless. But I don't see much use for any implementation of this technique, so I'm ignoring that issue.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:53, 31 January 2016 (UTC)
:::::::::::::: "involved symbols form an ordered set and each symbol in the ordered set denotes a sequential integer" means that the symbols come from some set with an ordering. In this case, they are using the set of letters and using ascending alphabetic order (and in the Rosetta Code implementation we are using the set of all bytes and using ascending character byte value order). All that matters is that A < B < C < D < ... The actual *values* are COMPLETELY IRRELEVANT to the algorithm -- they are just used in the encoding and decoding dictionaries. The reason that they are requiring that there is an ordering of the symbols is because "cumulative" requires some order of accumulation; that's the only reason why these "sequential integers" is even mentioned. Your confusion comes from the fact that you are wrongfully thinking that these "sequential integers" are somehow used in the algorithm. But nowhere in the Wikipedia description does it say anything to that effect. You are completely imagining that, possibly because the cumulative frequency in this example is coincidentally similar to the integer values they assign. The Wikipedia description shows very clearly where the cumulative frequencies come from and the "sequential integers" never factor into any of that. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 04:14, 1 February 2016 (UTC)
::::::::::::::: Why do you claim that some statements in the wikipedia page are irrelevant? If they are irrelevant, how is that different from them being in error? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:40, 1 February 2016 (UTC)
:::::::::::::::: I did not say any statement in the Wikipedia page is not relevant. The statement showing the "sequential integers" of the symbols is relevant in demonstrating an ordering. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 05:59, 1 February 2016 (UTC)
::::::::::::::::: Ok, so how is the C = 2 statement that I quoted from the wikipedia relevant? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:06, 1 February 2016 (UTC)
:::::::::::::::::: They are defining an ordering on all the symbols in the letter space, without regard to a particular message, instead of defining an ordering on only the symbols that happen to appear in this message. --[[User:Spoon!|Spoon!]] ([[User talk:Spoon!|talk]]) 06:31, 1 February 2016 (UTC)
::::::::::::::::::: Actually, no. Wait. 
::::::::::::::::::: If things are like you say, a problem is that the wikipedia page uses a poorly choosen example. '''DABDDB''' has cumulative frequency values which match the letter values which are stated on the page.
::::::::::::::::::: Your argument, in essence, is that using those letter values is not useful, so we should ignore them and use cumulative frequency values instead. 
::::::::::::::::::: But the algorithm itself is not very useful - it's more of an exercise.
::::::::::::::::::: Also, the wikipedia page specifies C=2 and you claim that's only for ordering, but the wikipedia page specifies "each symbol in the ordered set denotes a sequential integer", but cumulative frequencies skip integers in the sequence for all examples used in this task.
::::::::::::::::::: So, as it currently stands, the wikipedia writeup is ambiguous, and uses a poorly choosen example which does not help resolve this ambiguity.
::::::::::::::::::: So... I'm going to have to study some other treatments of this algorithm (http://marknelson.us/1991/02/01/arithmetic-coding-statistical-modeling-data-compression/ looks promising), and I imagine once I've resolved these issues to my satisfaction I'll have to update the task description.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:58, 1 February 2016 (UTC)

== Goof? (part 2) ==

I have expanded the J entry to illustrate some of my concerns. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:39, 31 January 2016 (UTC)

== Usefulness of approach? ==

So, ok... at least a part of the "goof" discussed in the above section might have been my own.

Still, after having resolved that, I am dissatisfied with this algorithm.

First, note that the results generated in this task are not sufficient to recover the original character sequences. To recover the original character sequence we probably need to know which characters were used and the number of times each character appeared.

That's something of a problem because this algorithm, and the frequencies used, are both explicitly tied to the length of the character sequence being compressed. It doesn't work with pre-shared parameters unless you limit yourself to a fixed collection of characters. If you are working with D D E G G H O O O T as your pre-shared dictionary (or, [[Run-length_encoding|more compactly]]: 2DE2GH3OT), you cannot represent THEGOODCAT --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:59, 1 February 2016 (UTC)

== Next steps? ==

After reading http://marknelson.us/2014/10/19/data-compression-with-arithmetic-coding/ I think the important thing is that there are many different arithmetic coding algorithms.
So, right now, from my point of view, the implementations in this task have deviated in a minor way from my reading of the wikipedia entry. And, this deviation is motivated by "usefulness", but does not actually go far enough to be useful.
So probably what should happen is the task description should go into more detail about those ambiguities. Or, if we want a useful approach, we should also revise the task (to separate model generation from encryption/decryption using that model - frequencies should probably be normalized to sum to one, also, if we want a generically useful model).
But it would also be possible (or at least less immediate effort, though also probably less useful so perhaps more effort and pain in the long term) to just spell out in the task description that each symbol represents (or is represented by) its cumulative frequency (accumulated in alphabetic order).
Still, that's a choice to be made, and obviously other people have different perspectives from mine. So I guess I'd like some feedback on what other people think we should do about this draft task. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:28, 1 February 2016 (UTC)

: If it's of any help, I can add a link in the task description to [http://trizenx.blogspot.ro/2015/05/arithmetic-coding.html a little bit more detailed tutorial] which explains the encoding and the decoding process, using an unambiguous example. [[User:Trizen|Trizen]] ([[User talk:Trizen|talk]]) 23:47, 1 February 2016 (UTC)
