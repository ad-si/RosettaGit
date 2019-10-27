+++
title = "Talk:Range consolidation"
description = ""
date = 2019-04-01T19:20:37Z
aliases = []
[extra]
id = 22163
[taxonomies]
categories = []
tags = []
+++

===The nature of these ranges, and thus of the task, is not yet clear===

''Ranges with bounds b0 and b1 covering all numbers in between'' doesn't seem to fully clarify or define the exercise, in particular, the intended nature of these ranges. Some of the bounds (or range members ? set members ?) appear to be integers, others appear to be floats or reals. If either of the latter is intended, then '''"all numbers in between"''' sounds like a rather large, very possibly even infinite, set. 

Clarification ?

Cases in which b0 is higher than b1 are also undefined in the preamble, but required in the test sample. Are we to understand these 'ranges' as unordered sets ? That seems to be suggested in the results shown from the Python code, which discard order without comment, implicitly treating [1 10.5] as equivalent to [10.5 1]

Clarification ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 06:52, 3 February 2019 (UTC)

: Hi Hout, I'll comment here and adjust the wording later today when I have more time.
:The ranges are defined by their endpoints and denote ranges of what can be described in int/fp. The ranges are not directional, [1, 2] covers the same range on a number-line as [2, 1].
:Yep, a range with different endpoints represents a large number of individual floats, but is defined by its endpoints.
:To be able to asily compare outputs it seemed that a sorting should be devised so that all outputs would be similar, but, I wanted to impose no such limitation on inputs, so programs may have to pre-condition/normalize inputs depending on their code.
::There is no need to pre-condition/normalize inputs. Consider a range type on which I define addition and subtraction. The task then becomes adding the supplied list of lists of test ranges to an empty range. So the first case would be let n=[()]+[(1,1)] then let g=n+[(2,2)]. Of course I wouldn't dare improve this task by adding extra credit, but I shall give extra credit to those solutions implementing subtraction.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:13, 10 February 2019 (UTC)
:::''"There is no need to pre-condition/normalize inputs."''
:::Thats right, just '''output''' normalisation is mandated. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:52, 10 February 2019 (UTC)

:Pythons pre-normalization is an integral part of its algorithm. (Which I have not yet fully tested}.

:My thoughts on automatically testing are to generate random sets of ranges on, say, a grid of x=0.5 then ensure that all points in the range on a grid of x/2. are similarly ex/included by the random input, and consolidated output ranges - or some such.
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:02, 3 February 2019 (UTC)

Are the boundaries included in the range? What notation is used to describe ranges with the opposite boundary properties? E.G. [2,4] - boundaries included; (2,4) - boundaries not included; [2,4) - lower boundary included, upper boundary not included, etc. Really, it seems like this is just a version of the [[Set_of_real_numbers]] task, except no operator requirement. The range building logic could be lifted directly though  --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 13:05, 3 February 2019 (UTC)

Hi Thundergnat, I state "covering all numbers between and including both bounds"; (other boundary types not used in the task). It could be similar to the task you mention. A quick look leaves me wondering if this consolidation could use the operators mentioned? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:17, 3 February 2019 (UTC)

==null range==
Is a   ''null''   range a legal and valid range?   Should programming examples handle (and express) such an animal?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:38, 9 February 2019 (UTC)
: If by null range, we are taking of ranges where the two boundaries coincide, then it is a valid range, i guess.
: A range is defined as having two end points though, in all cases. (for my Python example whee aI use a two-element list or tuple to represent a range, an empty list/tuple is not a valid range).
: (I had not thought of that, thanks.). 

: This is what the Python solution does with a zero-extent range:

```txt
consolidate([[1, 1]])
Out[2]: [[1, 1]]

consolidate([[1, 1], [1, 1]])
Out[3]: [[1, 1]]

consolidate([[0, 0], [0, 0]])
Out[4]: [[0, 0]]

consolidate([[1, 1], [0, 2]])
Out[5]: [[0, 2]]

consolidate([[1, 1], [0, 1]])
Out[6]: [[0, 1]]

consolidate([[1, 1], [1, 2]])
Out[7]: [[1, 2]]
```

: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:34, 10 February 2019 (UTC)

:: By a null range, I meant an empty range as in:     '''[ ]'''       -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:55, 10 February 2019 (UTC)

::: That would be invalid input.   -- [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:14, March 30, 2019 (UTC) 

<!--  I've added Paddy3118's signature to his (above) response (after-the-fact).    -- Gerard Schildberger. !-->


:: Perhaps there's a bit of slippage or confusion here in the name of the task ?
:: The task is really to consolidate not ranges but '''lists''' of ranges. (A 'consolidated range' is just the range itself, without any change in the dimension of its extent).
:: Given that we are really talking about lists of ranges, an empty list is clearly well-defined, and we can certainly consolidate it with another list of ranges. (It evaporates in the process). [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 01:32, 31 March 2019 (UTC)

: Square brackets are used for both the outer container of ranges and the inner container of two endpoints of a single range. 
:The inner must contain two endpoints. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:59, 31 March 2019 (UTC)

::Ah Cool. We're applying arbitrary, retroactive restrictions to tasks. Ok, do you want me to mark the C#, Go, Haskell, J, Phix. Python Functional & zkl entries incorrect or are you going to take care of it? After all, they don't use square brackets for the "inner containers". --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 11:11, 31 March 2019 (UTC)  

:::No. In the Python solution, the outer container shows as square brackets too. That's the context of my answer to Hout above,  (although I do need to point this out to the casual reader I guess, thanks). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:14, 31 March 2019 (UTC)


### Perl 6 oddity


: I would also like to know what the Perl 6 "ranges" of [2] and [5,9,8] mean. Maybe [] => not a range, discard/ignore, else length!=2 ==> [min(),max()]? Either that should become part of the task description, or the Perl 6 entry should either explain or cull them. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 20:04, 30 March 2019 (UTC)

:: That was intended to show that it dealt gracefully with "error" conditions. Much as you suspect, it pulls out the min and max values and uses them for the min and max of the range. Removed those test values to reduce confusion and comply with a strict interpretation of the task description. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 11:24, 31 March 2019 (UTC)

:::Thanks. Just for the record I am not against exceeding task requirements, just as long as they are properly explained. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 19:20, 1 April 2019 (UTC)

==A better-defined notation ?==

Mathematically, the ''consolidation'' here is an operation on two separate  ''ordered pairs'', '''tuples''' or ''2tuples'', for which the conventional notation is '''(a, b)''' rather than [a, b]. See Wikipedia [https://en.wikipedia.org/wiki/Tuple  Tuple].

The base ''consolidation'' (two tuples in, one or two tuples out), is then lifted to an operation on a structure containing '''many tuples''' which appears to be either ordered or unordered, and either potentially '''with''' duplicates (a '''list''' ?) or without duplicates (a '''set''' ?) .

Some of the noise and confusion might subside if we were to 'give math a chance' and 
# define the domain of the the simple function we are asking for in terms of two '''tuples''' (or two ''pairs'' if you prefer), 
# and define the domain (and output type) of the ''lifted'' function on a container in terms of a '''set''' of tuples ''{(a, b), (y, z) ... (m, n)}'' or a '''list''' of tuples ''[(a, b), (y, z) ... (m, n)]''  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 11:17, 31 March 2019 (UTC)

:: Hi, I had read of the use of the bracketing in conjunction with "ranges" but the link I find is to the same use for mathematical [https://en.m.wikipedia.org/wiki/Interval_(mathematics)#Excluding_the_endpoints intervals]
:: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 23:11, 31 March 2019 (UTC)

==Python tester==
I wrote a black-box, randomized tester for the Python example. All boundaries are generated as multiples of 0.5. 
Each boundary generates three testpoints <tt>x-.25, x, x+.25</tt>, i.e on a finer grid of 0.25. I then find the set of testpoints that are within the original ranges and this should equal the set of these same initial testpoints that are within the consolidated ranges. I don't test that the consolidation is the smallest set of ranges that could be found, only that the number of consolidated ranges is no larger than the number of original ranges.

Append the following below the Python entry text:

```python
#%%
from random import randint

gen_grid = 0.5              # Ranges will be generated on a 0.5 grid
gen_range = (-100, 100)     # The generation bounds are within thse limits
range_counts = (1, 10)      # Count of number of ranges to be consolidated 
tests = 100_000

def range_gen():
    x, y = gen_range
    x, y = x / gen_grid, y / gen_grid
    return [randint(x, y) * gen_grid for _ in (1, 2)]

range_set = normalize([range_gen() for _ in range(randint(*range_counts))])

def gen_test_points(ranges):
    "tp generated on a finer gen_grid/2 grid, around each boundary"
    testpoint = set()
    for r in ranges:
        for bound in r:
            for tp in (bound - gen_grid / 2, bound, bound + gen_grid / 2):
                testpoint.add(tp)
    return testpoint

def in_a_range(testpoints, ranges):
    "Returns only those tp that are within any of the ranges, in order."
    inrange = []
    for tp in testpoints:
        for x, y in ranges:
            if x <= tp <= y:
                inrange.append(tp)
                break
    return inrange

#%%   
if __name__ == '__main__':
    fail = 0
    print()
    for n in range(tests):
        ranges = normalize([range_gen() for _ in range(randint(*range_counts))])
        consolidated = consolidate(ranges)
        tp = gen_test_points(ranges)
        tp |= gen_test_points(consolidated) # tp around all boundaries
        good = (len(consolidated) <= len(ranges) 
                and in_a_range(tp, ranges) == in_a_range(tp, consolidated))
        if not good:
            print(f"{'PASS' if good else 'FAIL'} {ranges}  =>  {consolidated}")
            fail += 1
    print(f"PASS RATE: {tests - fail}/{tests}")
```


{{out}}

```txt
[1.1, 2.2] => [1.1, 2.2]
[6.1, 7.2], [7.2, 8.3] => [6.1, 8.3]
[4, 3], [2, 1] => [1, 2], [3, 4]
[4, 3], [2, 1], [-1, -2], [3.9, 10] => [-2, -1], [1, 2], [3, 10]
[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6] => [-6, -1], [1, 8]

PASS RATE: 100000/100000
```

