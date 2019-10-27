+++
title = "Talk:Brazilian numbers"
description = ""
date = 2019-08-17T10:42:25Z
aliases = []
[extra]
id = 22464
[taxonomies]
categories = []
tags = []
+++

==wee discrepancy==
Is it possible to be a little more specific regarding the "wee discrepancy" with the F# version?

```fsharp

printfn "%d" (Seq.item 3999 (Brazilian()))

```
 
prints 4618--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:05, 14 August 2019 (UTC)
:OK I think I've found it--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:37, 14 August 2019 (UTC)

:: I also noticed the difference two days ago, and I assumed that my REXX version was incorrect and was trying to find what the problem was in my computer program;   I was hoping somebody else would calculate the 100,000<sup>th</sup> Brazilian number and verify it (or not).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:23, 14 August 2019 (UTC)
==some observations not proofs ==
I tried to check the maximal base needed for an odd brazilian number.<BR>
If a number is brazilian the maximal base to test is always less equal number / 3.<BR>
If a number is prime and brazilian then the maximal base is square root of number.<BR>
[https://tio.run/##lVRNb@IwEL3nV8yhhxhBl9JDV9BWKiSokdgEbehqV4iDSRwwGxzkmFZt1b@@rO04H/Rjq72AM29m3vjNeHY4j3DaSXbR4bDj2YrjLSw5fqIpxczfb5eE5wPr@cQbO@4YxtPRiwXwfPItcFxw3Mn01nuRqDsJ3QK4mU5nv6YujAI/DCauBn3HG79YUcZyIX3ge3DnOyFcwVm32x0oSzgJZiN/pky9gXWPuTSOsj0TYZqJHPqAOceP8@7pqfHsnC0gS@COMnHeUylovuN0S5quBU1LkmjfZZalBLOBZclrRiTecwIeo2Kq4kpS2t70wceC3hOVe2AtyYoyCSQ0TaM15rYhmvcW7ZA@kSyRFp0CdXrtaM3tgMe24HuCENKFQf8KdIkPa5oSoC0Kl1dwS1frKhLiTOJDQwXgjcFAc7oAsSaFuXbYqKQykxbPJN58kLYZV@k03yxUigSnORmUEIvsTZui4puw@NVB4QWqTfrHspI9iwTNGIzWJPp9k6Yh3hKHrqjIbabHp73EOWmqivpVLwrVOclFO1Yx8L760mGfiqOKVYwyFBzgeD9A8SjIZKqwjkZaKqLug46/hu5r7U2MjC79/8mlm2W/JUJweW0KqfoH4P70ZpWAzWupgSklrRT18mH5EA1D34jjKRGHxyIq6ko@T6k3/Fg9WbSp@RIu6grL@l4XpgP8TNhBHNvFUkDobViScVOGHHkQmeHo9MpJpMknY4Lem3YdVw5u4bxoqqqeAKeCpJVMZ91i6Mx/y3ygctTrJsv7ybFW@JfC2DIbBjWfRb2LSv4KLu/@TlON2kVXy/3C6hE/alJjFQGMpZB6cUyyhwY5UprqJ960GW0bNVL9uOVqLfeP3rJMnS7qF8A@30PJ8Qyipur1OijOrN1D9WiXDTnKj5pIEatuyv7zpjrDUU8W/a9IC316OPyJkhSv8kMnOP8L Try it online!]

```txt
// only primes are shown
    number      base      base*base
        13         3         9
        31         2         4
        43         6        36
        73         8        64
       127         2         4
       157        12       144
       211        14       196
       241        15       225
       307        17       289
       421        20       400
       463        21       441
       601        24       576
       757        27       729
      1093         3         9
      1123        33      1089
      1483        38      1444
..
     55987         6        36
     60271       245     60025
     60763       246     60516
     71023       266     70756
     74257       272     73984
     77563       278     77284
     78121       279     77841
     82657       287     82369
     83233       288     82944
     84391       290     84100
     86143       293     85849
     88741        17       289
     95791       309     95481
     98911       314     98596
odd brazilian numbers 7 .. 100000 : 40428
slots:  base/number 
  <=1/12 <= 2/12  <=3/12  <=4/12
   30717    4013    2225    3473       0       0       0       0       0       0       0       0

```



:: Thanks, Mr. Horst (userid Horst.h),   I added (the non-prime hint) to the REXX program and it speeded it up by a factor of two.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:51, 15 August 2019 (UTC)
::: some more observations by factorization of the numbers:<BR>Brazilian primes always have "1" as digit.MaxBase =  trunc(sqrt(prime))-> "111" and therefor are rare 213 out of 86400.<BR>So one need only to test if digit is "1" for prime numbers.

```txt

    number = factors    base     repeated digit
         7 = 7         2         1      "111" to base 2
        13 = 13         3         1     "111" to base 3
        31 = 31         2         1     "11111" to base 2
        43 = 43         6         1
        73 = 73         8         1
       127 = 127         2         1   "1111111" to base 2 
       157 = 157        12         1
--
       601 = 601        24         1
       757 = 757        27         1
      1093 = 1093         3         1   "1111111" to base 3
...
    987043 = 987043       993         1
   1003003 = 1003003      1001         1
   1005007 = 1005007      1002         1
   1015057 = 1015057      1007         1
   1023133 = 1023133      1011         1
   1033273 = 1033273      1016         1
   1041421 = 1041421      1020         1
   1045507 = 1045507      1022         1
   1059871 = 1059871      1029         1  "111" to base 1029
Max number  1084566 -> 84600 primes
Brazilian primes found 213
```

How about nonprime odd numbers?

```txt

    number = factors    base     repeated digit
        15 = 3*5          2         1  = "1111" also "33" to base 4  -> ( 5-1) 
        21 = 3*7          4         1  = "111" also "33" to base 6  -> ( 7-1) 
        27 = 3^3= 3*9     8         3  
        33 = 3*11        10         3   
        35 = 5*7          6         5
        39 = 3*13        12         3
        45 = 3^2*5        8         5  
        51 = 3*17        16         3
        55 = 5*11        10         5
        57 = 3*19         7         1  also "33" to base 18 
        63 = 3^2*7        2         1  also "77" to base 8    
        65 = 5*13        12         5 
        69 = 3*23        22         3
        75 = 3*5^2       14         5
        77 = 7*11        10         7
        81 = 3^4=3*27    26         3
        85 = 5*17         4         1   also "55" to base 16
        87 = 3*29        28         3
        91 = 7*13         9         1
        93 = 3*31         5         3
        95 = 5*19        18         5
        99 = 3^2*11      10         9  
       105 = 3*5*7       14         7 
       111 = 3*37        10         1 also "33" to base 36
```

I think, taking the factorization of the number leave the highest factor -1 > sqrt( number)  as base and the rest as digit.Something to test.<Br><Br>
Edit.Some more investigation:<BR>Which numbers are nonbrazilian :-)<Br>As one can see, only primes are possibly nonbrazilian
and square numbers of odd primes are nonbrazilian with only one exception found  up to 10000 : 11^2 

```txt
factorization of the non brazilian numbers
         9 = 3^2
        11 = 11
        17 = 17
        19 = 19
        23 = 23
        25 = 5^2
        29 = 29
        37 = 37
        41 = 41
        47 = 47
        49 = 7^2
        53 = 53
        59 = 59
        61 = 61
        67 = 67
        71 = 71
        79 = 79
        83 = 83
        89 = 89
        97 = 97
       101 = 101
       103 = 103
       107 = 107
       109 = 109
       113 = 113
       131 = 131
       137 = 137
       139 = 139
       149 = 149
       151 = 151
       163 = 163
       167 = 167
       169 = 13^2
       173 = 173
       179 = 179
       181 = 181
       191 = 191
       193 = 193
       197 = 197
       199 = 199
       223 = 223
       227 = 227
       229 = 229
       233 = 233
       239 = 239
       251 = 251
       257 = 257
       263 = 263
       269 = 269
       271 = 271
       277 = 277
       281 = 281
       283 = 283
       289 = 17^2
       293 = 293
       311 = 311
       313 = 313
       317 = 317
       331 = 331
       337 = 337
       347 = 347
       349 = 349
       353 = 353
       359 = 359
       361 = 19^2
       367 = 367
       373 = 373
       379 = 379
       383 = 383
       389 = 389
       397 = 397
       401 = 401
       409 = 409
       419 = 419
       431 = 431
       433 = 433
       439 = 439
       443 = 443
       449 = 449
       457 = 457
       461 = 461
       467 = 467
       479 = 479
       487 = 487
       491 = 491
       499 = 499
       503 = 503
       509 = 509
       521 = 521
       523 = 523
       529 = 23^2
       541 = 541
       547 = 547
       557 = 557
       563 = 563
       569 = 569
       571 = 571
       577 = 577
       587 = 587
       593 = 593
       599 = 599
       607 = 607
       613 = 613
       617 = 617
       619 = 619
       631 = 631
       641 = 641
       643 = 643
       647 = 647
       653 = 653
       659 = 659
       661 = 661
       673 = 673
       677 = 677
       683 = 683
       691 = 691
       701 = 701
       709 = 709
       719 = 719
       727 = 727
       733 = 733
       739 = 739
       743 = 743
       751 = 751
       761 = 761
       769 = 769
       773 = 773
       787 = 787
       797 = 797
       809 = 809
       811 = 811
       821 = 821
       823 = 823
       827 = 827
       829 = 829
       839 = 839
       841 = 29^2
       853 = 853
       857 = 857
       859 = 859
       863 = 863
       877 = 877
       881 = 881
       883 = 883
       887 = 887
       907 = 907
       911 = 911
       919 = 919
       929 = 929
       937 = 937
       941 = 941
       947 = 947
       953 = 953
       961 = 31^2
       967 = 967
       971 = 971
       977 = 977
       983 = 983
       991 = 991
       997 = 997
Max number      1000

now checking sqr(primes) upto 10000:
       121 = 11^2
last checked 9983^2
Brazilian found 1
     99494 ms
```

[[user:Horst.h|Horst.h]]
