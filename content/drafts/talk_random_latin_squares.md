+++
title = "Talk:Random Latin Squares"
description = ""
date = 2019-08-05T11:56:54Z
aliases = []
[extra]
id = 22366
[taxonomies]
categories = []
tags = []
+++

==[[Latin Squares in reduced form]]==
[[Latin Squares in reduced form]] makes trivial generating  uniformly distributed random Latin Squares up to order 6. 3 random numbers are required. The first in the range 1 to number of reduced Latin Squares of order n is used to select a member of the set of reduced Latin Squares of order n. The second in the range 1 to n! is used to select a permutation of the columns of the selected reduced Latin Square. The third in the range 1 to (n-1)! is used to select a permutation of the rows 2 to n.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 09:51, 12 July 2019 (UTC)
==Random Latin Squares of order greater than 6==
This task becomes interesting at order 7. Consider:

```txt

1 2 3 4 5 6 7
2
3
4
5
6
7

```

There are 5! ways to add 7 to the above. Having selected one of them it is necessary to determine the number of ways of completing the Latin Square similar to the way used in [[Latin Squares in reduced form]]. Selecting one of the completions and permuting the columns and rows as above will produce an almost uniformly distributed random Latin Square. The distribution is now not quite uniform because the number of ways of completing the Latin Square will vary with the arrangement of 7s chosen.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:06, 12 July 2019 (UTC)

==Revert to draft==
I think the task description needs to require and demonstrate that the algorithm must be capable of producing all valid latin squares of size n. As it stands starting from a valid latin square, which is easy to generate say:

```txt

0 1 2 3 4
1 2 3 4 0
2 3 4 0 1
3 4 0 1 2 
4 0 1 2 3

```

there are 5! ways to arrange the columns and 5! ways to arrange the rows each of which is a valid random latin square. From the task description I see no reason why this would not be a valid solution as the task stands. I have added a reference to A002860 which is the number of latin squares of size n. For n>4 this simple solution produces random valid latin squares but can not produce all solutions. Nor does the Python algorithm which is just obfuscation which reduces to the above.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 18:38, 10 June 2019 (UTC)

### Non python algorithm

Apart from selecting from almost none of the possible latin squares the 'python algorithm' has second disadvantage. Considering the above latin square if I move the rightmost column to in front of the leftmost, or the bottom row to above the top I produce the same result. That is there are varying number of ways of producing each of the small number of latin squares that this algorithm can produce. An alternative is to just permute the meaning of 0..4. This would produce a smaller subset of all possible latin squares, but almost none is almost none either way, and they would be uniform. This task was promoted from draft very quickly, I think it needs to go back to draft until this issue is resolved--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 09:30, 12 June 2019 (UTC)
=="restarting row" method==
To quote the factor solution:

"... generate a random permutation, one row at a time. If a row conflicts with any of the rows above it, generate a new random permutation for that row. The upside is that this is easy to understand and generates uniformly-random Latin squares..."

Does it generate uniformly-random Latin squares? To expose this fallacy let me consider I randomly select 0123 for my first row. Let me first select 1230 with some probability 1/n as my second row. The are now 2 ways to complete this square 2301 as the third row and 3012 as the fourth or 2301 as the fourth row and 3012 as the third. So each of these squares is generated with probability 1/2n, Now again with probability 1/n I select 1032 as my second row. There are now 4 ways to complete this square: 2301 as the third row and
3210 as the fourth; or 3210 as the third row and 2301 as the fourth; or 2310 as the third row and 3201 as the fourth; or 3201 as the third row and 2310 as the fourth. So each of these squares is produced with probability 1/4n.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 19:53, 13 July 2019 (UTC)

:: To the best of my knowledge, it does, in fact, generate uniformly random latin squares. https://www.academia.edu/29890346/Comparison_of_Seven_Techniques_for_Generating_Random_Latin_Squares See page 2. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 19:32, 16 July 2019 (UTC)

::: OK it will have to be pistols at dawn then. Darn in these politically correct times that is not allowed. In my country we can't even have pistols. We'll have to settle for testing it then. Consider the following 4 Latin Squares of order 4:

```txt


                  a           b          c          d
               0 1 2 3     0 1 2 3    0 1 2 3    0 1 2 3
               1 0 3 2     1 0 3 2    1 2 3 0    1 3 0 2
               2 3 0 1     2 3 1 0    2 3 0 1    2 0 3 1
               3 2 1 0     3 2 0 1    3 0 1 2    3 2 1 0

```

::: If the factor code is used to produce a large number of random Latin Squares of order 4, say 1 million, my conjecture is that c and d will be produced twice as often as a and b. Your conjecture is they will be produced equally. Do you fancy giving it a go?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 01:29, 17 July 2019 (UTC)

:::: My Go solution also uses the "restarting row" method which I certainly expected to be uniformly random but, when I ran Nigel's test, sure enough c and d occurred about twice as often as a and b. So it does demonstrate how easily one can be led astray by intuition in this sort of work. Anyway I'm going to rewrite the Go solution on the lines Nigel suggested earlier and will re-post later today. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 13:15, 17 July 2019 (UTC)

::::: Thanks for the correction, both of you. I wonder, did I misinterpret the algorithm as described in the paper, or is the author simply incorrect? Is the "restarting row" method actually more of a "restarting square" method? I will be looking to rewrite my submission as soon as I can wrap my head around an algorithm that works. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 16:48, 17 July 2019 (UTC)

:::::: I've had a look at the paper and the author just seems to be wrong about the "restarting row" method producing uniformly random results. In fact the paper seems to be a bit self-contradictory. On the one hand it says in section 1.2:

:::::: "One thing that must be considered in SeqGen methods is that when a conflict is treated, correcting or replacing the repeated symbol can lead to a non-uniform distribution of the generated results."

:::::: On the other hand it says in section 2.1:

:::::: "Also, the generated results are uniformly distributed (if the PRNG used to generate the symbols in the row is not biased)."

:::::: But, as Nigel's example for a Latin square of order 4 clearly shows, what goes in the first two rows is inevitably affecting what goes in the third and fourth rows, not just in the order of the symbols but in the possible number of permutations of those rows and hence the probability distribution of the set of Latin squares being generated is not even approximately uniform. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 19:54, 17 July 2019 (UTC)

:: in particular, you may find this interesting: "The probability of finishing the entire LS is a combination of the previous series of probabilities, but not their product, as the rows are not independent to each other (i.e. row i depends of values on row i-1)." --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 19:37, 16 July 2019 (UTC)

:::: "(...)different  purposes in cryptography". Dear me. Thought we were heading for sudokus.--[[User:Steenslag|Steenslag]] ([[User talk:Steenslag|talk]]) 20:44, 16 July 2019 (UTC)

==Python Algorithm==
I got the mention of Latin squares from a stack-overflow question and a [https://www.academia.edu/29890346/Comparison_of_Seven_Techniques_for_Generating_Random_Latin_Squares link] to some solution methods that I did not read (and so did not add to the task as a reference).

I worked out that if you have a smaller solution of:

```txt
0 1
1 0
```

then the next larger solution  (ignoring randomisation), could be got by:
1. Insert a copy of the first row at the end:

```txt
0 1
1 0
0 1
```

2. Insert the new symbol along the diagonal

```txt
2 0 1
1 2 0
0 1 2
```


I randomise the selection of symbol to insert at each recursive stage, and at the end swap rows randomly and swap columns randomly as these transformations preserve "Latin-ness".

If you read the reference mentioned in the first sentence and think it is of use, then please add it to the task.


- --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:44, 9 June 2019 (UTC)

==="Randomness"===
I don't actually use the algorithm in earnest, but several of the references mention the randomness of the generation.

I have no measure of the randomness, but thought that any number should be as likely to appear in any position in the square so I created a counter of how many times each symbol appeared in each cell of the square grid and present the counts for a million runs:

Additional Python:

```python
def distributions(n, count):
    counters = [[defaultdict(int) for _ in range(n)] for __ in range(n)]
    range_n = range(n)
    for _ in range(count):
        square = rls(n)
        for r in range_n:
            for c in range_n:
                counters[r][c][ square[r][c] ] += 1
    for symbol in range_n:
        symcounts = [[counters[r][c][symbol] for c in range_n] for r in range_n]
        mx = max(cnt for row in symcounts for cnt in row)
        mn = min(cnt for row in symcounts for cnt in row)
        print(f'\n{symbol} distribution: from {mn} to {mx}\n==')
        print(_to_text(symcounts))

distributions(5, 1000_000)
```

{{out}}

```txt
0 distribution: from 199123 to 200657
==
199758 200268 199123 200194 200657
199878 200137 200165 199932 199888
200447 199564 200602 200024 199363
200094 200412 200266 199659 199569
199823 199619 199844 200191 200523

1 distribution: from 198960 to 200500
==
200057 200135 200085 199825 199898
200144 200249 199308 200118 200181
199297 200500 200268 200380 199555
200441 198960 200000 200317 200282
200061 200156 200339 199360 200084

2 distribution: from 198983 to 200849
==
200849 200156 200034 199978 198983
199747 199808 200144 200054 200247
199796 200405 199650 199895 200254
199820 199935 200178 199872 200195
199788 199696 199994 200201 200321

3 distribution: from 199433 to 200820
==
199816 199452 200527 200133 200072
200820 200003 199749 199598 199830
199769 199985 200143 199664 200439
199739 200558 199545 199932 200226
199856 200002 200036 200673 199433

4 distribution: from 199337 to 200691
==
199520 199989 200231 199870 200390
199411 199803 200634 200298 199854
200691 199546 199337 200037 200389
199906 200135 200011 200220 199728
200472 200527 199787 199575 199639
```


 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:10, 9 June 2019 (UTC)


### =Uniformity over all possible=

I suspect this algorithm does not generate random latin squares uniformly. What matters is the uniformity of latin squares taken from the list of all latin squares. See https://math.stackexchange.com/questions/63131/generate-random-latin-squares . --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 14:19, 9 June 2019 (UTC)


Hi Chunes, you're right!

I went back to the wikipedia article which has a table stating that the numbero f distinct n=4 LS is 576. This additional code:

```python
In [97]: def distinct(n, count):
    ...:     distinct = Counter(tuple(rls(n)) for _ in range(count))
    ...:     print(f"Found {len(distinct)} different {n}-by-{n} Latin squares in {count} trials")
    ...: 
    ...: 

In [98]: distinct(4, 576_00)
Found 432 different 4-by-4 Latin squares in 57600 trials

In [99]: 
```


Shows that my algorithm cannot generate 25% of the possible outputs. It would have been nice if it could, but I won't disallow the algorithm as the task description isn't that precise.
:It needs to be precise. The task asks for random latin squares of size 5. At 5 this algorithm produces less than 9% of all possible. At 6 it produces about 0.05%.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 21:00, 10 June 2019 (UTC) 

Ideally I would find examples of what is missing and try and work out an additional algorithm that generates from the whole set. .... If time allowed.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:55, 9 June 2019 (UTC)
:There is not just a little bit missing, above n=4 it produces almost none of the possible solutions.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 21:00, 10 June 2019 (UTC)

: It might be worth mentioning that non-uniform solutions are acceptable in the task description. From my cursory research, it appears that generating a uniformly-random latin square is a difficult problem which either requires the exponential-time brute force approach of generating random permutations for rows and restarting the row if a clash occurs, or else requires implementing the involved Jacobson and Matthews algorithm. This also absolves us of some mathematical pedantry. :) --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 15:14, 9 June 2019 (UTC)

:: Suggestion implemented. Thanks :-) 
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:53, 9 June 2019 (UTC)



### Task description second sentence clear ?

Leaving aside the missing full stop, I wonder if that sentence is clear and well-formed ? The kernel statement seems to be that 'A Latin square generates a Latin square'.  Might that not risk furrowing a brow or two ? Perhaps an edit for clarity and transitive plausibility ?[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:48, 12 June 2019 (UTC)

:Thanks. Changed. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:33, 12 June 2019 (UTC)
:: Well ... That still makes no sense at all ... How can a Latin square "generate" the values which constitute it ? Perhaps what the incoherence (and attempted circularity) of that sentence expresses is really a need for a slightly more solid concept of what is actually being asked for here ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:34, 12 June 2019 (UTC)

==Ruthless Hillclimbing Method==
To quote a method for generating uniformly random latin squares from page 20 of https://pdfs.semanticscholar.org/4a7c/d245f6f6a4ef933c6cf697832607f71a39c1.pdf,

"Another,  more ruthless method,  is a modification of hill climbing.  This time, do  not  worry  about  finding  SDRs,  merely  look  at  all  possible  permutations and  select  one  uniformly  at  random  to  add  as  the  next  row.   If,  in  doing  so, you violate the rules of being a Latin square,  restart the entire process.   This method terminates with probability 1 and does achieve the uniform distribution. However, if L(n) is the total number of LS(n), the expected number of restarts is n!(n−1/L(n)) =e^(n^2(1+o(1))); an unacceptable price to pay for uniformity [29]."

I have implemented this algorithm and in my testing, it does not suffer from the specific lack of uniformity described by Nigel above. (Squares A, B, C, and D are generated with the same frequency.) I am hopeful that this is a method for generating uniformly random latin squares. Although inefficient, it suffices for generating squares of order 5. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 15:59, 18 July 2019 (UTC)
:Well found. The paper is so interesting I decided to give section 3.3 a go as a task see [[Latin Squares in reduced form/Randomizing using Jacobson and Matthews’ Technique]]--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:56, 5 August 2019 (UTC)
