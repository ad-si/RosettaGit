+++
title = "Talk:Narcissistic decimal number"
description = ""
date = 2014-03-08T08:06:03Z
aliases = []
[extra]
id = 17337
[taxonomies]
categories = []
tags = []
+++

==task clarification==

According to this task's link to Wolfram MathWorld (TM), a narcissistic number is an N-digit number whose ...

The first narcissistic number is 0 (zero).

According to OEIS (The On-line Encyclopedia of Integer Sequences (R)), the first narcissistic number is 0 (zero). 

This would change what numbers are listed when displaying 25 narcissistic numbers.

I would prefer mentioning that narcissistic numbers are non-negative integers.   After all, 15.3 is a decimal number.

Also, for those searching for Armstrong numbers, maybe a note saying:

Narcissistic numbers are also known as:
::* [[Armstrong numbers]]
::* [[Perfect digital invariant]] (Madachy 1979)
::* [[Plus perfect numbers]] (Hardy 1993)

Narcissistic numbers are similar to ''powerful numbers''.   Powerful numbers are integers that are equal to some fixed (integer) power of their digits.

The list of narcissistic numbers is finite (89).

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:25, 7 March 2014 (UTC)

:Yep. There's work to do on the task description, but hopefully it will not be too confusing until it is updated. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:48, 7 March 2014 (UTC)

:: I think this task should be downgraded to a draft task until the definition of Rosetta Code's description of a narcissistic number is corrected.   Some programming examples are using 0 (zero) as the first narcissistic numbers, others are using 1 (one); this makes the list of the first 25 numbers problematic. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:59, 7 March 2014 (UTC)

:Additions done. Other names for the numbers etc. don't affect completing the task. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:28, 7 March 2014 (UTC)
::I've aligned the verbiage in the first paragraph with the consensus view. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 21:34, 7 March 2014 (UTC)

:::: My main concern was the use of ''a positive number''' (or number).   That has been corrected.   As far as the other names, people searching for an algorithm for Armstrong numbers (or the other names) would now be able to find it easier. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:35, 7 March 2014 (UTC)

:Added three new Re-directs for alternative names mentioned above. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:45, 8 March 2014 (UTC)

==D language comparative speedup?==
How about astatement like "The faster version has an $n times speedup over the first"? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:48, 7 March 2014 (UTC)
: The first D entry is just very slow compared to the second one, because it's not meant to be fast -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

==a complete list of narcissistic numbers==

For those that are interested, here is a complete list of all the narcissistic numbers,
produced by my   $CALC   (REXX) program by specifying: 

:::::   $CALC   narcissistic(1,89)  

```txt

                                 ╔════════════════════╗
                                 ║ narcissistic(1,89) ║
                                 ╚════════════════════╝

 1►                                                   0
 2►                                                   1
 3►                                                   2
 4►                                                   3
 5►                                                   4
 6►                                                   5
 7►                                                   6
 8►                                                   7
 9►                                                   8
10►                                                   9
11►                                                 153
12►                                                 370
13►                                                 371
14►                                                 407
15►                                               1,634
16►                                               8,208
17►                                               9,474
18►                                              54,748
19►                                              92,727
20►                                              93,084
21►                                             548,834
22►                                           1,741,725
23►                                           4,210,818
24►                                           9,800,817
25►                                           9,926,315
26►                                          24,678,050
27►                                          24,678,051
28►                                          88,593,477
29►                                         146,511,208
30►                                         472,335,975
31►                                         534,494,836
32►                                         912,985,153
33►                                       4,679,307,774
34►                                      32,164,049,650
35►                                      32,164,049,651
36►                                      40,028,394,225
37►                                      42,678,290,603
38►                                      44,708,635,679
39►                                      49,388,550,606
40►                                      82,693,916,578
41►                                      94,204,591,914
42►                                  28,116,440,335,967
43►                               4,338,281,769,391,370
44►                               4,338,281,769,391,371
45►                              21,897,142,587,612,075
46►                              35,641,594,208,964,132
47►                              35,875,699,062,250,035
48►                           1,517,841,543,307,505,039
49►                           3,289,582,984,443,187,032
50►                           4,498,128,791,164,624,869
51►                           4,929,273,885,928,088,826
52►                          63,105,425,988,599,693,916
53►                         128,468,643,043,731,391,252
54►                         449,177,399,146,038,697,307
55►                      21,887,696,841,122,916,288,858
56►                      27,879,694,893,054,074,471,405
57►                      27,907,865,009,977,052,567,814
58►                      28,361,281,321,319,229,463,398
59►                      35,452,590,104,031,691,935,943
60►                     174,088,005,938,065,293,023,722
61►                     188,451,485,447,897,896,036,875
62►                     239,313,664,430,041,569,350,093
63►                   1,550,475,334,214,501,539,088,894
64►                   1,553,242,162,893,771,850,669,378
65►                   3,706,907,995,955,475,988,644,380
66►                   3,706,907,995,955,475,988,644,381
67►                   4,422,095,118,095,899,619,457,938
68►                 121,204,998,563,613,372,405,438,066
69►                 121,270,696,006,801,314,328,439,376
70►                 128,851,796,696,487,777,842,012,787
71►                 174,650,464,499,531,377,631,639,254
72►                 177,265,453,171,792,792,366,489,765
73►              14,607,640,612,971,980,372,614,873,089
74►              19,008,174,136,254,279,995,012,734,740
75►              19,008,174,136,254,279,995,012,734,741
76►              23,866,716,435,523,975,980,390,369,295
77►           1,145,037,275,765,491,025,924,292,050,346
78►           1,927,890,457,142,960,697,580,636,236,639
79►           2,309,092,682,616,190,307,509,695,338,915
80►          17,333,509,997,782,249,308,725,103,962,772
81►         186,709,961,001,538,790,100,634,132,976,990
82►         186,709,961,001,538,790,100,634,132,976,991
83►       1,122,763,285,329,372,541,592,822,900,204,593
84►      12,639,369,517,103,790,328,947,807,201,478,392
85►      12,679,937,780,272,278,566,303,885,594,196,922
86►   1,219,167,219,625,434,121,569,735,803,609,966,019
87►  12,815,792,078,366,059,955,099,770,545,296,129,367
88► 115,132,219,018,763,992,565,095,597,973,971,522,400
89► 115,132,219,018,763,992,565,095,597,973,971,522,401

```

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:50, 8 March 2014 (UTC)

:Well, apart from the little matter of sero, your table matches the one on [http://oeis.org/A005188/b005188.txt OEIS]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:05, 8 March 2014 (UTC)
