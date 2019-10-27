+++
title = "Talk:Best shuffle"
description = ""
date = 2016-07-21T19:53:12Z
aliases = []
[extra]
id = 8981
[taxonomies]
categories = []
tags = []
+++

== appreciation comment ==
This was a fun algorithm to write --- lots of twists and turns. [[User:Gerard Schildberger|Gerard Schildberger]]

== Task Description ==

I think it would improve the task description if we said something along the lines of "Shuffle the characters of a string to produce a string as different as possible from the original.  That is, the maximum # of characters in the output differ from the characters at the corresponding positions at the input".  Only more concise :).  I say that because the current wording lends itself to a trivial solution - reversing the string (because it doesn't make clear that a "character" is different only if its value, not its index, is different).  In fact, that's how I initially interpreted the task (character=index), until I saw the solutions were a lot more complex than reverse().

--[[User:DanBron|DanBron]] 18:28, 15 December 2010 (UTC)

: Well, it says "characters" not "indexes". But feel free to change the wording as you see fit. [[User:Fwend|Fwend]] 20:16, 15 December 2010 (UTC)

I recently compiled and ran the D codes that had replaced my own version and noticed that they produced the same result every time. This isn't what I had in mind when I used the word "shuffle". I'm assuming that the other "deterministic" codes have the same limitation. It isn't a problem as far as I'm concerned. It's still interesting and educational to see this approach. Nevertheless I'm putting back my own code because it produces the result that I was looking for. [[User:Fwend|Fwend]] 09:39, 15 May 2011 (UTC)
: If I read the task description correctly, the task is actually looking for an "an organization as distant from the original organization as possible". The result is probably provable to that end. If the non-deterministic code doesn't result in an optimum solution, it's probably incorrect. I may be wrong, though. --[[User:Short Circuit|Michael Mol]] 10:49, 15 May 2011 (UTC)
:: I'm going to play devils advocate on the words here.  The task says "as many of the character values are in a different position as possible" which seems clear to me that this means as few coincidences of characters by position.  While "an organization as distant from the original organization as possible" could be taken as an arrangement where the characters are also far away from their original position. I don't see any of the solutions addressing that metric. I think either approach could be deterministic.  I do admit that "shuffle" implies some randomness. (The Icon solution has a commented out line for this since it wasn't clear at the time).  Some of the other solutions may have trouble adapting (not sure).  If it's late in the game to clarify, could randomness be an extra points type requirement?  --[[User:Dgamey|Dgamey]] 13:53, 15 May 2011 (UTC)
::: I think that's a good idea. Having more than one approach is more interesting anyway. English is not my native language. Maybe somebody could adjust the task description. I don't want to risk creating more confusion :) [[User:Fwend|Fwend]] 14:24, 15 May 2011 (UTC)
::::Okay, how about something like this "Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible. Print the result as follows: original string, shuffled string, (score). \n\nWhere the score the number of characters that remain in their original position.  The better the shuffle the smaller the score. \n\nAs there are often multiple solutions that achieve the lowest score, for extra points include or show how to achieve randomization amongst these lowest scoring results."  --[[User:Dgamey|Dgamey]] 03:12, 16 May 2011 (UTC)
::::: How about: "Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible. Print the result as follows: original string, shuffled string, (score). \n\n. The score gives the number of positions whose character value did not change. A shuffle that produces a randomized result is to be preferred. A deterministic approach that produces the same sequence every time is acceptable as an alternative." [[User:Fwend|Fwend]] 20:13, 16 May 2011 (UTC)
:::::: Very clear.  Go for it! --[[User:Dgamey|Dgamey]] 20:27, 16 May 2011 (UTC)


== Javascript implementation ==

<code>abracadabra, bdabararaca, (1)</code>

Thank you for implementing a Javascript version. I'm afraid it needs tweaking, however. The word "abracadabra" can be shuffled without any character in the original position. [[User:Fwend|Fwend]] 19:01, 17 December 2010 (UTC)

Oops, thank you, I should have paid attention to the results I was getting.  --[[User:Rdm|Rdm]] 19:15, 17 December 2010 (UTC)


== C entry ==

"... every character is considered unique in a word and that's why grrrrr also has a score of 0."

The score is supposed to give the number of positions that have the same character value as before. It's not very important, but makes it easier to quickly see if you've achieved the optimal result. 

Also the code doesn't shuffle the characters. Perhaps you should imagine the routine being used in a word game or puzzle. The results that this code produces, although clever in the way it solves the problem, wouldn't be very satisfactory.  [[User:Fwend|Fwend]] 18:53, 6 January 2011 (UTC)

: The problem with the C entry is that it does not work in the general case.  Consider, for example:

<lang>Enter String : aabbbbaa

aabbbbaa, aabbbbaa, (0)
```


:(The correct answer, here, would be bbaaaabb).  --[[User:Rdm|Rdm]] 20:24, 6 January 2011 (UTC)

::Never mind, I fixed it.  --[[User:Rdm|Rdm]] 17:31, 7 January 2011 (UTC)

==On the Python "Swap if it is locally better algorithm"==
I had visited this problem around two weeks before and wrote a solution based on testing all permutations for the one with the best shuffle, but I could not wait for an answer when it was given 'abracadabra' and decided to 'sit on it'.

Today someone made an addition and I glanced through the AWK entry which mentioned swapping. That got me thinking of trying swapping  characters and the first result of note that I came up with was:

```python>>>
 def bs(w):
	w2 = list(w)
	rangew = range(len(w))
	for i in rangew:
		for j in rangew:
			if i != j and w[i] != w[j]:
				w2[j], w2[i] = w2[i], w2[j]
	w2 = ''.join(w2)
	return w2, count(w, w2)

>>> bs('elk')
('kel', 0)
>>> for w in 'tree abracadabra seesaw elk grrrrrr up a'.split():
	print(w, bs(w))

	
tree ('reet', 1)
abracadabra ('aacrdrbaaab', 2)
seesaw ('seesaw', 6)
elk ('kel', 0)
grrrrrr ('rrgrrrr', 5)
up ('up', 2)
a ('a', 1)
```


I then thought that for better results I needed to go through the swapping twice; then multiple times - but that might loop forever so I added n, the maximum times through the for loops.
The condition on when to swap needed refinement too (- I never did check if this refinement would work without the need for the outer while loop?):

```python>>>
 def bs(w):
	w2 = list(w)
	w2old = []
	n = len(w)
	rangew = range(n)
	while w2old != w2 and n:
		n -= 1
		w2old = w2[:]
		for i in rangew:
			for j in rangew:
				if i != j and w2[j] != w2[i] and w[i] != w2[j] and w[j] != w2[i]:
					w2[j], w2[i] = w2[i], w2[j]
	w2 = ''.join(w2)
	return w2, count(w, w2)

```


At this point I shoved this code in a file from the idle shell that I had been developing on, so intermediate results are harder to recreate.

The main changes developed in the file where to further refine the inner-most if statement determining when to swap. I then decided to add some randomness by the simple means of shuffling the range of integers that variables i and j iterate over.

Being as the code was nolonger based on testing all permutations, I gave it a word guaranteed to stress those types of solutions: ''antidisestablishmentarianism'' ;-) then just made sure it worked with the variety of words shown on the page, and tidied my output formatting. --[[User:Paddy3118|Paddy3118]] 08:38, 22 May 2011 (UTC)

Well, the while loop is not needed! so this is the code talked about above.

```python
def best_shuffle(w):
    w2, w2old = list(w), []
    n = len(w)
    rangelists = (list(range(n)), list(range(n)))
    while w2old != w2 and n:
        n -= 1
        w2old = w2[:]
        for r in rangelists:
            random.shuffle(r)
        rangei, rangej = rangelists
        for i in rangei:
            for j in rangej:
                if i != j and w2[j] != w2[i] and w[i] != w2[j] and w[j] != w2[i]:
                    w2[j], w2[i] = w2[i], w2[j]
    w2 = ''.join(w2)
    return w2, count(w, w2)
```


I have updated the code on the task page to remove the while loop. --[[User:Paddy3118|Paddy3118]] 08:49, 22 May 2011 (UTC)

== Common Lisp ==

the task description says "A shuffle that produces a randomized result is to be preferred." so i don't see how this is compared to general permutation tasks which seem to ask for all possible permutations instead of one random permutation.
:''I interpeted that as meaning that when multiple best solutions are possible we should randomly pick from that set of best solutions. --[[User:Rdm|Rdm]] 10:56, 14 October 2011 (UTC)''
::hmm, that's an interesting interpretation, supported by the fact that random is an option, but it would make this task even more like the permutation tasks where all permutations need to be generated.
:::''You do not actually have to generate all solutions for this.  For example, let's say that you have an algorithm which works by building lists of instances of each letter as its first step.  Let's also suppose that this algorithm only finds one solution.  If you then randomly shuffle each of those lists of character locations, your result could then be a random pick from the potential best results (for the cases where more than one solution is possible). --[[User:Rdm|Rdm]] 13:00, 14 October 2011 (UTC)''
::::i looked at the description of the C solution (but i didn't study the code), and i don't quite understand how this is supposed to work.
::::it splits the input into sets, starting a new set when a letter is already in a set, so for abracadabra this would produce ((a b r) (a c) (a d) (a b r) (a)), but that is not right, to make it work the sets should be:  ((a b r) (a c) (a d) (a b) (r a)). there is also talk about putting the longest set first, but i don't understand how it prevents a letter from falling into the same place as the input if sets are reordered.--[[User:EMBee|eMBee]] 14:39, 14 October 2011 (UTC)
:::::''In other words, instead of making the sets ((a b r) (a c) (a d) (a b r) (a)) I was thinking you could instead make the sets of the indices which select ((a a a a a) (b b) (c) (d) (r r)).  Except they are really not sets, but sequences which means we can shuffle them.  In other words, you would shuffle the lists ((0 3 5 7 10) (1 8) (4) (6) (2 9)) perhaps getting ((10 0 5 7 3) (8 1) (4) (6) (9 2)).  Then you would generate the lists of indices which might have corresponded to something like your ((a b r) (a c) ...).  In this case, we first restructure the index lists based on the length of the longest inner list: ((10 0 5 7 3) (8 1 4 6 9) (2)).  Then we form new groups picking the first, second, third, .. from these "equal length except for the last one" lists:  ((10 8 2) (0 1) (5 4) (7 6) (3 9)).  Finally, you go back to the original string and you rotate characters within the positions in these groups.  In other words, using capital letters to represent letters from positions (10 8 2):  'abRacadaBrA' becomes 'abBacadaArR', (0 1): ABbacadaarr becomes BAbacadaarr, (5 4): babaCAdaarr becomes babaACdaarr, (7 6): babaacDAarr becomes babaacADarr, (3 9): babAacadaRr becomes babRacadaAr. In other words, if I had a different 'a' position for my first 'a' I would be rotating the first 'r' to that position instead...  Is this a clear enough presentation of the concept? --[[User:Rdm|Rdm]] 15:59, 14 October 2011 (UTC)''

<del>
i posted a function for generating a random permutation as per the task description, and am still working on comparing the results.
</del>

<del>
maybe it is better to save the incomplete solution here and move it to the main page when it is ready.
--[[User:EMBee|eMBee]] 07:21, 14 October 2011 (UTC)</del>
:new version is ready and moved to main page.--[[User:EMBee|eMBee]] 11:14, 14 October 2011 (UTC)
:: The code as posted doesn't compile.  After removing two right parens, it compiles but produced wrong results: <code>abracadabra dcbaabarraa (2)</code>.
:: I looked at the code again, using <code>random</code> can't warrant correct results (note that a random shuffle can not garantee a result with minimal overlapping.  Since it's random, you can't even garantee that after infinitely long run.)  I'll mark it incorrect. --[[User:Ledrug|Ledrug]] 11:22, 14 October 2011 (UTC)
:::oops, i pasted the code back into the interpreter, to test it and didn't get any errors. i use SBCL, which ignores extra parens silently! and CMUCL where i tested once only gave a warning.--[[User:EMBee|eMBee]] 18:21, 14 October 2011 (UTC)
:::thank you for clearing up the task description. i don't know when i'll find the time to fix it though.--[[User:EMBee|eMBee]] 11:52, 14 October 2011 (UTC)

:Hi eMBee, the task description should be read as meaning that there may be several shuffles with the same best score. If run with the same input a second time, there should be a chance that another of the shuffles which equal the best score is given. Generating all permutations then scoring them and filtering them could give the right answer, but might take too long as input sizes grow --[[User:Paddy3118|Paddy3118]] 11:33, 14 October 2011 (UTC)

== Best? ==

The best (in the ordinary sense of the word) shuffle is the result of a random process, in which every possible permutation has a 1/n! chance of occurring, including the original sequence itself. Does this "best" term come from some literature on random sequences? [[Special:Contributions/192.139.122.42|192.139.122.42]] 22:45, 14 October 2011 (UTC)
: Probably not.  By requiring minimal overlapping with the original string, it kills a lot of randomness.  But the task's intention is pretty clear, though.  --[[User:Ledrug|Ledrug]] 23:03, 14 October 2011 (UTC)

:Best is strictly defined in the task as being (one of) the shuffles that give the minimum of characters in the same place. In terms of implementations, an implementation that when run repeatadly can give a different member of the best shuffles is to be preferred. the word/term ''best'' has its meaning added to for the purposes of the task. --[[User:Paddy3118|Paddy3118]] 05:12, 15 October 2011 (UTC)

== Perl 6  ==

Perl 6 example is not correct for all words.
Sub best-shuffle for word 'aaaabbbbcccc' returns  'bbbcaaccaabc, 1', but there exists better shuffle, for example 'bbbbccccaaaa'.
--[[User:Wamba|Wamba]] ([[User talk:Wamba|talk]]) 18:43, 24 June 2015 (UTC)
