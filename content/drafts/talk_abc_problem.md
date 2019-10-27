+++
title = "Talk:ABC Problem"
description = ""
date = 2016-08-16T19:34:13Z
aliases = []
[extra]
id = 17062
[taxonomies]
categories = []
tags = []
+++

==Solution?==
Had fun with this one last night after playing with my daughter. Should I include a solution? --[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 14:46, 8 January 2014 (UTC)

:You probably need to have a solution at hand to write a good task. If you've got it, why not add it as the first example? (I do) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:34, 8 January 2014 (UTC)

::Added.--[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 03:46, 9 January 2014 (UTC)

==Examples OK?==
It appears to me that, given the set of blocks on the page, the result of a test on "BOOK" should be true?

Also it is possible to make a zero-length word with whatever letters you have. In which case shouldn't the results of <code>can_make_word ""</code> be True? --[[User:Tikkanz|Tikkanz]] ([[User talk:Tikkanz|talk]]) 19:42, 8 January 2014 (UTC)

::I think you can spell the empty string with zero blocks. So the first test should return true. The reasoning is similar to the Python function all([]) returning True. Another clue comes from the Python iterative solution, that has needed a special case for the empty string. Often in algorithms if you have a special case it needs a special justification.-[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

:::Hi Bearophile, I think you are premature in your changes. The recursive Python solution has no such special case handling and returns False. ''I will revert your task change pending further discussion''... (P.S. did you read my comments below on the null case)? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:50, 9 January 2014 (UTC)

:Hi, you need one B and two O's but the only blocks with either a B or an O are the ''two'' blocks BO and OB - you can't spell three letters with two blocks! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:30, 8 January 2014 (UTC)

:(The difficult null case). I take it as you cannot spell the null string '''with''' blocks as no block has a face without any characters. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:37, 8 January 2014 (UTC)

::OK, I was/am confused then about what the rules for using a block are? If a block is chosen, can only one of letters on the block can be used in the word? If so perhaps the following should be added as an additional constraint. "Only one letter can be used from each block chosen". 

::I guess that is a logical way to explain the null string generating "False", but using no blocks from the collection can also be thought of as "using the given set of blocks". Maybe better specification is required for this case too? --[[User:Tikkanz|Tikkanz]] ([[User talk:Tikkanz|talk]])

===can_make_word("") == False===
Currently the original task creator set the answer for a null string as being False. Various people think that it could and should be True. There are three ways I think of dealing with this:
# Keep the original False.
# Switch to True.
# Remove the need for the null string case from the examples and silently allow either True or False for this case.

It would be good to hear from the original task creator on this issue as well from other interested parties. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:44, 9 January 2014 (UTC)
:Well the question is, "Can you make this word given this collection of blocks?" The answer doesn't make sense without valid input and there are numerous ways to handle it in different languages. I chose the "false" value arbitrarily since it could be readily interpreted as, "no, I cannot spell any words if you don't give me one to spell." The case for "true" is equally valid. If it would help avoid bike-shedding over the issue I'd opt for removing the case altogether.--[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 20:20, 9 January 2014 (UTC)

:: An empty string is a word without letters, which is not the same as "not giving you a word". If you are going to choose arbitrarily between two interpretations anyway, choose the one that's simpler.  We want to use a subset of the blocks to spell out a word, using an empty subset (i.e. no blocks) is the way to spell a word with no letters, so "true" is a more natural answer.  What you want is better expressed as "using a ''nonempty'' set of blocks to spell a word", which is different and more (and unnecessarily) restrictive. Choosing "true" or "false" are not equally valid if you define the task clearly. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:49, 9 January 2014 (UTC)
:::I'm aware of the case for "true" and had considered it before choosing "false" for a moment. It was simply the first test case I wrote. I chose "false" and moved on and haven't given it this much thought (though I think I might've wondered what is a word with no letters?). I honestly don't see either choice being essential to the spirit of the task and would happily accept "true" if that makes more sense to more people.--[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 21:14, 9 January 2014 (UTC)

:So far, it seems that a choice of item 3 would upset everyone equally ;-)
:I would be inclined to swap to item 3. Any nay-sayers? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:52, 10 January 2014 (UTC)

::I agree with Jking that the handling of a null is pretty meaningless in terms of this task (I don't imagine that spelling "" comes up much when using these blocks!), so I support choosing item 3. --[[User:Tikkanz|Tikkanz]] ([[User talk:Tikkanz|talk]]) 11:12, 10 January 2014 (UTC)

==Not a set of blocks==
The block (F S) appears twice so I changed the description to use the word collection rather than a set of blocks. I have also added a specific example "CONFUSED" that will fail if their is only one (F S) block available. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:49, 8 January 2014 (UTC)

: Why not change one of the toy blocks to a   '''(S F)'''?   That is the case for   '''(B O)'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:42, 9 January 2014 (UTC)

: There could be duplicate blocks. I didn't mean "set" in the mathematical sense which would be confusing. Collection seems appropriate. --[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 02:55, 9 January 2014 (UTC)

== Greedy algorithm ==

The python iterative solution is greedy and not garanteed to find a solution.  Example: if blocks are "AB", "AB", "AC" and "AC", it may fail to find a solution for the word "ABBA".  This particular algorithm only works if each letter appears on only one type of blocks.  Not finding a solution when there is one is a matter of correctness.  Either the task should put further constraints on the blocks ("All blocks containing letter A are of the same letter combinations"), or such algorithms should clearly state the chance of failure ("This is a greedy algorithm that's fast but may fail to find an answer"). --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:12, 10 January 2014 (UTC)
:Excellent point and a good test case! I shall look into updating the example.--[[User:Jking|Jking]] ([[User talk:Jking|talk]]) 21:08, 10 January 2014 (UTC)

:: Yes, while all the programming examples correctly process the words given for the specified blocks (as of now), some programming examples don't find a solution for the   '''abba'''   situation.   I think it'd be a good idea to add another set of blocks ('''AB AB AC AC''') with the word   '''abba'''   as a test case word. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:44, 11 January 2014 (UTC)

== order of blocks ==

PL/I has a problem:

blocks = 'US TZ AO QA'

word = 'Auto' cannot be spelt when taking AO for the A of AUTO.
Therefore I created version 2 earlier for REXX and now for PL/I
(which are the two languages I know rather well).
I cannot say how other languages' solutions behave.--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:58, 11 January 2014 (UTC)

:Using your blocks I get:

```txt
BLOCKS: US TZ AO QA

Can we spell    'AuTO'? True Using: A from 'QA', U from 'US', T from 'TZ', O from 'AO'
```

:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:55, 11 January 2014 (UTC)

:: Thanks! Fine :-) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:08, 11 January 2014 (UTC)

::: I assume tha the programs should work for any set of blocks and any word --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:24, 11 January 2014 (UTC)

:::: Yes, indeed.   I hope the author will drop this task back into ''draft'' status and add the word   '''abba'''   (with the '''AB AB AC AC''' pool of blocks, ''in that order''). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:09, 11 January 2014 (UTC)

::::: I think the order of blocks should not be relevant. Think of Scrabble. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 23:10, 11 January 2014 (UTC)

:::::: For a greedy algorithm, the order of blanks may be important.   That is, in another order than shown (i.e., '''AC AC AB AB'''), it most likely would find a solution.   (This was an error in my first attempt at programming a solution in REXX which turned out to be a greedy algorithm and it didn't find a solution for the   '''abba'''   or   '''auto'''   problem as stated above).   The above block order ('''AB AB AC AC''') is the "worst" order to be in, as a greedy algorithm would find a letter on the first block that contained the letter, and "remove" that block from further consideration.   The game of ''Scrabble'' (R) doesn't have multiple letters on a tile. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:35, 11 January 2014 (UTC)

::::::: Scrabble: true but the order to arrange the letters is of no importance. And can you point me to a definition of greedy algorithm? The Wikipedia article I saw does not exactly describe what your first attempt did, does it? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 12:31, 12 January 2014 (UTC)

:::::::: I was referring to the order of the blocks (and the manner in which they are chosen, and then marked for non-participation for later choices).   I'm not sure what you mean by arranging the letters.   The whole point is to arrange the letters [here and ''Scrabble'' (R)] to spell words.   As for what you saw on Wiki, I can only surmise what WIKI ''exactly'' describes;    I think the Wiki article pretty much describes the methodology of the initial attempt of the what the REXX version 1 program did (albeit the original REXX version 1 algorithm didn't go deep enough to find a solution for some other later examples talked about, which made it an ineffective and an incorrect algorithm).   There are other definitions of ''greedy algorithm'', but I don't to spend time and effort to see if those definitions apply to the initial REXX version 1 attempt, one reason is the initial REXX version 1 program is defunct.   The initial REXX attempt could spell   '''bbaa'''   but not   '''abba'''.   The current REXX version 1 can   (using the pool of blocks:   '''AC AC AB AB'''   in any order). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:54, 13 January 2014 (UTC)

== Definition please ==

Am I to understand that these are 2-sided "blocks"?  I certainly never had anything like that as a child...

: Or six sided but with only two distinct letters per block. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:30, 21 January 2014 (UTC)
::Yeah they're a pretty common toy. They look something like [http://career-city.com/resumeimages/block-letter-font-8.jpg this]. Letters on opposite sides and the other four sides are blank (or maybe they have pictures carved/printed on them instead). --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 21:01, 5 June 2014 (UTC)

::: Below is an ASCII-art rendition of some children's play blocks;   they typically had two single letters (on opposite sides [faces] of the cube), two single digits (again, on opposite sides), and some other theme, such as a ''bird'' (along with a '''B'''), maybe a ''turtle'' or a ''tiger'' (along with a '''T'''), a ''dog'' (with a '''D'''), ''elephant'' (with an '''E'''), ···       The letters may be capitalized, or possibly mixed (to teach how to read both glyphs of a letter).   Sometimes, there would be a   '''6'''   on a face, and on the opposite side (face) of the cube, a   '''six'''.   There were a variety of themes available:   animals, flowers, letters and digits, (very basic) arithmetic (usually simple addition), punctuation, colors, etc), depending upon the target age for the child playing with the blocks.   In the good ole days, the blocks were always made of wood, and about an inch to 1½ inches across for each face, some were bigger.   The older blocks used (plain) black ink, the later ones used colors (either ink or paint).   Paint was, of course, problematic if lead-based. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:29, 5 June 2014 (UTC)

```txt

                                             __________________
                                            /\                 \
                                           /  \     \      \    \
                                          /    \     \      \    \
                                         /      \     \______\    \
                                        /   /\   \     \      \    \
                                       /   //\\   \     \      \    \
                                      /   ///\\\   \     \      \    \
                     ________________/   ////\\\\   \_________________\
                    /\               \   \\\\////   /                 /
                   /  \    ________   \   \\\///   /        /        /
                  /    \   \       \   \   \\//   /        /        /
                 /      \   \           \   \/   /  ______/_____   /
                /        \   \           \      /        /        /
               /    |     \   \           \    /        /        /
              /   \ | /    \   \_______\   \  /        /        /
             /   __\|/__    \_______________\/_________________/
             \     /|\      /               /\                 \
              \   / | \    /    /     /    /  \    __________   \
               \    |     /    /     /    /    \   \    \    \   \
                \        /    /_____/    /      \        \        \
                 \      /          /    /      _ \        \        \
                  \    /          /    /       /  \        \        \
                   \  /          /    /       /    \        \        \
                    \/_______________/       /      \_________________\
                                     \      /       /                 /
                                      \   \/       /                 /
                                       \   \      /        ∙        /
                                        \        /     _______     /
                                         \      /                 /
                                          \    /         ∙       /
                                           \  /                 /
                                            \/_________________/

```


== Alternative Common Lisp version ==

I think this is a little bit more consice (at least it is definitely shorter). I'm not sure about adding it in addition to the old one / instead?
<code>
    (defun word-possible-p (word alphabet)
      (labels ((%usablep (b) (find (char-upcase (char word 0)) b)))
        (or (zerop (length word))
            (iter
              (for candidate :in (remove-if-not #'%usablep alphabet))
              (when (word-possible-p
                     (subseq word 1) (remove candidate alphabet :count 1))
                (return t))))))
</code>
[[User:Wvxvw|Wvxvw]] ([[User talk:Wvxvw|talk]]) 13:37, 5 April 2015 (UTC)

== C and Perl versions ==

In the C recursive version, if I've understood this correctly, once a character has been
matched successfully by
''if (b[i][0] != c && b[i][1] != c) continue;''
there is no need to keep matching it, therefore, I would put
''break;''
at the end of the ''for'' block in ''can_make_words''.

Similarly with the Perl recursive version, perhaps
''last;''
at the end of the ''for'' block in ''_can_make_word''.

For example, if we test the 2nd set from the Perl version where the blocks are US TZ AO QA and the word is "auto", without and with the ''break''/''last'', the former returns true, the latter
false.
I believe false is the correct answer, i.e. auto can't be made from US TZ AO QA in the order
the letters appear.

: It's supposed to be a ''set'' of blocks, which means there's no specific order among them.  So "auto" can be made by "Q[A] [U]S [T]Z A[O]". --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 18:57, 18 May 2015 (UTC)

:: I'm quite wrong, its not the order of the letters. The matches made are the first but not necessarily the best. Perldoc for keys function states "Hash entries are returned in an apparently random order", so depending on the order the hash keys are returned from %blocks I sometimes see the candidates as AO,SU,TZ then no more candidates, and sometimes as AQ,SU,TZ,AO. --[[User:Buggerit|Buggerit]] ([[User talk:Buggerit|talk]]) 10:13, 19 May 2015 (UTC)
::: I still don't see how that happens.  The use of <code>local</code> can be confusing, but it appears to be correct, and all the test cases work.  Do you have a counterexample where it failes? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:32, 19 May 2015 (UTC)
