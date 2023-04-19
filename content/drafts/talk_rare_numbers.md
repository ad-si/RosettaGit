+++
title = "Talk:Rare numbers"
description = ""
date = 2019-10-22T21:30:34Z
aliases = []
[extra]
id = 22521
[taxonomies]
categories = []
tags = []
+++

__TOC__

== comments concerning   ''interesting observations''   from an webpage ==
(The author's webpage, the last URL reference from this task's preamble, re-shown below:)


(a URL reference):
:*   author's  website:        [http://www.shyamsundergupta.com/rare.htm rare numbers]    by Shyam Sunder Gupta.     (lots of hints and some observations).



I was considering adding checks   (to the REXX program)   to assert that for:
::*   when the number of digits in a rare number is        ''even'',   the   '''sum'''   must be divisible by   '''11''',      and
::*   when the number of digits in a rare number is   ''odd'',    the   '''difference'''   must be divisible by   '''9'''.
::

n-r is divisible by 9 for all Rare numbers. n-r is also divisible by 99 when the number of digits is odd. (see [[Talk:Rare_numbers#30_mins_not_30_years]])--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:48, 21 September 2019 (UTC)

### 30 mins not 30 years

If Shyam Sunder Gupta has really spent 30 years on this he should have stayed in bed. Let me spend 30mins on it. The following took 9mins so any questions and I have 21mins to spare.

```txt

Let me consider n-r=l for a 2 digit number ng n<g. Then l=(10g+n)-(g+10n)=9(g-n) where n is 0..8 and g is 1..9.
l is one of 9 18 27 36 45 54 63 72 81. l must be a perfect square so only 9 36 and 81 are of interest.

9 -> ng=89 78 67 56 45 34 23 12 01
36-> ng=59 48 37 26 15 04
81-> ng=09

For each of these candidate ng I must determine if ng+gn is a perfect square.

09+90  99 n
59+95 154 n
48+84 132 n
37+73 110 n
26+62 114 n
15+51  66 n
04+40  44 n
89+98 187 n
78+87 165 n
67+76 143 n
56+65 121 y
45+54  99 n
34+43  77 n
23+32  55 n
12+21  33 n
01+10  11 n

From which I see that 65 is the only Rare 2 digit number.

I love an odd number of digits. Let me call the 3 digit number nxg. l=(100g+10x+n)-(g+10x+100n).
x disappears and I am left with 99(g-n). None of 99 198 297 396 495 594 693 792 or 894 are perfect squares.
So there are no Rare 3 digit numbers.

At 4 I begin to think about using a computer. Consider nige. l=(1000e+100g+10i+n)-(e+10g+100i+1000n).
I need a table for l as above for 9(111(e-n)+10(g-i)) where n<=e and if n=e then i<g.

Before turning the computer on I'll add that for 5 digits nixge l=(10000e+1000g+100x+10i+n)-(e+10g+100x+1000i+10000n).
x disappears leaving 99(111(e-n)+10(g-i))

```
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:34, 12 September 2019 (UTC)
:I have turned the computer on and produced a solution using only the above and nothing from the referenced website which completes in under a minute. The reference is rubbish, consider removing it--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:42, 18 September 2019 (UTC)

:: Rubbish or not, is there anything on the referenced (Gupta's) website that is incorrect?   The properties and observations is what the REXX solution used (and others have as well) to calculate   ''rare''   numbers,   albeit not as fast as your algorithm.   I have no idea how long Shyam Sunder Gupta's program(s) executed before it found eight rare numbers   (or how much virtual memory it needed).   Is the   '''F#'''   algorithm suitable in finding larger   ''rare''   numbers?   I suspect (not knowing '''F#''') that virtual memory may become a limitation.   Eight down, seventy-six more to go.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:18, 18 September 2019 (UTC)

:::So is the task now to replicate [[http://www.shyamsundergupta.com/R20.htm]]? This may not be reasonable for a RC task as I now discuss.
 Why have you "no idea how long Shyam Sunder Gupta's program(s) executed before it found eight rare numbers"? On the webpage it says "the program has been made so powerful that all numbers up to 10^14 can be just checked for Rare numbers in less than a minute". This implies that it can search 10^13 in 5 secs. I believe this. The problem with the webpage is not that it is wrong, but that it is disingenuous. I estimate that the following is achievable:
10^13 -> 5 secs;
10^15 -> 60 secs;
10^17 -> 20 mins;
10^19 -> 7 hours;
10^21 -> 6 days;
10^23 -> 4 months.
I would say that 10^17 is reasonable for a RC task and is in line with the timings given on the webpage. Those who have recently obtained an 8th. generation i7 might want to observe that there is an obvious multithreading strategy and might want to prove that Goroutines are more than a bad pun on coroutines, as a suitable punishment for making me envious. (Warning, from my experience of i7s it might be wise to take it back to the shop and have water cooling installed before attempting to run it for a day and a half on full throttle). I remain to be convinced that these benchmarks are achieved by the Fortran, Ubasic programs on the webpage, or can be achieved in this task using the methods described on the webpage.
It is necessary to distinguish the algorithm I describe above from the F# implementation on the task page. The algorithm can be written to require very little memory. Obviously the F# as it stands calculates all candidates before checking them and this list grows with increasing number of digits. It is more than adequate for the current task, and I anticipate little difficulty in accommodating reasonable changes to make the task less trivial as layed out above--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:01, 20 September 2019 (UTC)

:::: Obviously, this task's requirement is <u>not</u> to replicate the list of 84   ''rare''   numbers on  [[http://www.shyamsundergupta.com/R20.htm Shyam Sunder Gupta's webpage: a list of 84 rare numbers]].   The task requirements have not changed:   find and show the first   '''5'''   ''rare''   numbers.   The last two requirements are optional.   Any hints and properties can be used as one sees fit.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:55, 20 September 2019 (UTC)

::::: Well, I have to admit that Nigel's (n-r) approach is considerably faster than what SSG has published as I've just added a second Go version which finds the first 25 Rare numbers (up to 15 digits) in about 42 seconds, albeit on my Core i7 which is kept cool when hitting turbo mode by a rather noisy fan.

::::: Although I don't doubt SSG's claims (he is presumably a numerical expert), he must be using much more sophisticated methods than he has published to achieve those sort of times on antique hardware.

::::: Incidentally, I regard it as bad form to use concurrent processing (i.e. goroutines) in RC tasks unless this is specifically asked for or is otherwise unavoidable (for processing events in some GUI package for example). It is difficult for languages which don't have this stuff built in to compete and it may disguise the usage of what are basically poor algorithms. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 23:04, 24 September 2019 (UTC)

== the 1<sup>st</sup> REXX version ==
This is the 1<sup>st</sup> REXX version,   before all the optimizations were added:

```rexx
/*REXX program to calculate and display an specified amount of   rare    numbers.       */
numeric digits 20;    w= digits() + digits() % 3 /*ensure enough decimal digs for calcs.*/
parse arg many start .                           /*obtain optional argument from the CL.*/
if  many=='' |  many==","  then  many= 3         /*Not specified?  Then use the default.*/
#= 0                                             /*the number of  rare  numbers (so far)*/
    do n=10                                      /*N=10, 'cause 1 dig #s are palindromic*/
    r= reverse(n)                                /*obtain the reverse of the number  N. */
    if r>n   then iterate                        /*Difference will be negative?  Skip it*/
    if n==r  then iterate                        /*Palindromic?   Then it can't be rare.*/
    s= n+r                                       /*obtain the    sum     of  N  and  R. */
    d= n-r                                       /*   "    "  difference  "  "   "   "  */
    if iSqrt(s)**2 \== s  then iterate           /*Not a perfect square?  Then skip it. */
    if iSqrt(d)**2 \== d  then iterate           /* "  "    "       "       "    "   "  */
    #= # + 1                                     /*bump the counter of  rare  numbers.  */
    say right( th(#), length(#) + 9)       ' rare number is:  '       right( commas(n), w)
    if #>=many  then leave                       /* [↑]  W:  the width of # with commas.*/
    end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
th:     parse arg th;return th||word('th st nd rd',1+(th//10)*(th//100%10\==1)*(th//10<4))
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt:  parse arg x;   $= 0;     q= 1;                            do while q<=x;    q= q*4
                                                                  end  /*while q<=x*/
                  do while q>1;  q= q % 4;   _= x-$-q;   $= $ % 2
                  if _>=0  then do;          x= _;       $= $ + q
                                end
                  end   /*while q>1*/;                            return $
```

Pretty simple,   but slow as molasses in January.

Not ready for prime time.

== the 2<sup>nd</sup> REXX version ==
This is the 2<sup>nd</sup> REXX version,   after all of the hints   (properties
of   ''rare''   numbers)   within Shyam Sunder Gupta's

[http://www.shyamsundergupta.com/rare.htm <u>webpage</u>]   have been incorporated in this REXX program.

```rexx
/*REXX program to calculate and display an specified amount of   rare    numbers.       */
numeric digits 20;    w= digits() + digits() % 3 /*ensure enough decimal digs for calcs.*/
parse arg many start .                           /*obtain optional argument from the CL.*/
if  many=='' |  many==","  then  many= 5         /*Not specified?  Then use the default.*/

@dr.=0;   @dr.2= 1; @dr.5=1 ; @dr.8= 1; @dr.9= 1 /*rare # must have these digital roots.*/
@ps.=0;   @ps.2= 1; @ps.3= 1; @ps.7= 1; @ps.8= 1 /*perfect squares    must end in these.*/
@end.=0;  @end.1=1; @end.4=1; @end.6=1; @end.9=1 /*rare # must not  end in these digits.*/
@dif.=0;  @dif.2=1; @dif.3=1; @dif.7=1; @dif.8=1; @dif.9=1 /* A─Q mustn't be these digs.*/
@noq.=0;  @noq.0=1; @noq.1=1; @noq.4=1; @noq.5=1; @noq.6=1; @noq.9=1 /*A=8, Q mustn't be*/
@149.=0;  @149.1=1; @149.4=1; @149.9=1           /*values for  Z  that need a even  Y.  */
#= 0                                             /*the number of  rare  numbers (so far)*/
@n05.=0;    do i= 1        to 9;  if i==0 | i==5  then iterate;  @n05.i= 1; end  /*¬1 ¬5*/
@eve.=0;    do i=-8  by 2  to 8;  @eve.i=1;  end /*define even   "    some are negative.*/
@odd.=0;    do i=-9  by 2  to 9;  @odd.i=1;  end /*   "   odd    "      "    "    "     */
                                                 /*N=10, 'cause 1 dig #s are palindromic*/
    do n=10;  parse var  n  a 2 b 3 '' -2 p +1 q /*get 1st\2nd\penultimate\last digits. */
    if @end.q  then iterate                      /*rare numbers can't end in: 1 4 6 or 9*/
    if q==3    then iterate

       select                                    /*weed some integers based on 1st digit*/
       when a==q  then do
                       if a==2|a==8 then nop     /*if A = Q,   then A must be  2  or 8. */
                                    else iterate /*A  not two or eight?       Then skip.*/
                       if b\==p  then iterate    /*B  not equal to  P?        Then skip.*/
                       end
       when a==2  then do; if q\==2 then iterate /*A = 2?     Then  Q  must also be  2. */
                           if b\==p then iterate /*" " "      Then  B  must equal    P. */
                  end
       when a==4  then do
                       if q\==0   then iterate   /*if Q not equal to zero, then skip it.*/
                       _= b - p                  /*calculate difference between B and P.*/
                       if @eve._  then iterate   /*Positive not even?      Then skip it.*/
                       end
       when a==6  then do
                       if @n05.q  then iterate   /*Q  not a zero or five?  Then skip it.*/
                       _= b - p                  /*calculate difference between B and P.*/
                       if @eve._  then iterate
                       end
       when a==8  then do
                       if @noq.q  then iterate   /*Q  isn't one of 2, 3, 7, 8?  Skip it.*/
                         select
                         when q==2  then            if b+p\==9                then iterate
                         when q==3  then do; if b>p         then if b-p\== 7  then iterate
                                               else if b<p  then if b-p\==-3  then iterate
                                                            else if b==p      then iterate
                                         end
                         when q==7  then do; if b>1         then if b+p\==11  then iterate
                                               else if b==0 then if b+p\== 1  then iterate
                                         end
                         when q==8  then            if b\==p                  then iterate
                         otherwise  nop
                         end   /*select*/
                       end                       /* [↓]  A  is an odd digit.            */
       otherwise  n= n + 10**(length(n) - 1) - 1 /*bump N so next N starts with even dig*/
                  iterate                        /*Now, go and use the next value of  N.*/
       end   /*select*/

    _= a - q;     if @dif._  then iterate        /*diff of A─Q must be: 0, 1, 4, 5, or 6*/
    r= reverse(n)                                /*obtain the reverse of the number  N. */
    if r>n   then iterate                        /*Difference will be negative?  Skip it*/
    if n==r  then iterate                        /*Palindromic?   Then it can't be rare.*/

    d= n-r;   parse var  d  ''  -2  y  +1  z     /*obtain the last 2 digs of difference.*/
    if @ps.z  then iterate                       /*Not 0, 1, 4, 5, 6, 9? Not perfect sq.*/
       select
       when z==0   then if y\==0    then iterate /*Does Z = 0?   Then  Y  must be zero. */
       when z==5   then if y\==2    then iterate /*Does Z = 5?   Then  Y  must be two.  */
       when z==6   then if y//2==0  then iterate /*Does Z = 6?   Then  Y  must be odd.  */
       otherwise        if @149.z   then if y//2  then iterate /*Z=1,4,9? Y must be even*/
       end   /*select*/

    s= n+r;   parse var  s  ''  -2  y  +1  z     /*obtain the last two digits of the sum*/
    if @ps.z  then iterate                       /*Not 0, 2, 5, 8, or 9? Not perfect sq.*/
       select
       when z==0   then if y\==0    then iterate /*Does Z = 0?   Then  Y  must be zero. */
       when z==5   then if y\==2    then iterate /*Does Z = 5?   Then  Y  must be two.  */
       when z==6   then if y//2==0  then iterate /*Does Z = 6?   Then  Y  must be odd.  */
       otherwise        if @149.z   then if y//2  then iterate /*Z=1,4,9? Y must be even*/
       end   /*select*/

    $= a + b                                     /*a head start on figuring digital root*/
               do k=3  for length(n) - 2         /*now, process the rest of the digits. */
               $= $ + substr(n, k, 1)            /*add the remainder of the digits in N.*/
               end   /*k*/
                                                 /*This REXX pgm uses 20 decimal digits.*/
       do while $>9                              /* [◄]  Algorithm is good for 111 digs.*/
       if $>9  then $= left($,1) + substr($,2,1)+ substr($,3,1,0)  /*>9? Reduce to a dig*/
       end   /*while*/

    if \@dr.$             then iterate           /*Doesn't have good digital root?  Skip*/
    if iSqrt(s)**2 \== s  then iterate           /*Not a perfect square?  Then skip it. */
    if iSqrt(d)**2 \== d  then iterate           /* "  "    "       "       "    "   "  */
    #= # + 1                                     /*bump the counter of  rare  numbers.  */
    say right( th(#), length(#) + 9)       ' rare number is:  '       right( commas(n), w)
    if #>=many  then leave                       /* [↑]  W:  the width of # with commas.*/
    end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
th:     parse arg th;return th||word('th st nd rd',1+(th//10)*(th//100%10\==1)*(th//10<4))
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt:  parse arg x;   $= 0;     q= 1;                            do while q<=x;    q= q*4
                                                                  end  /*while q<=x*/
                  do while q>1;  q= q % 4;   _= x-$-q;   $= $ % 2
                  if _>=0  then do;          x= _;       $= $ + q
                                end
                  end   /*while q>1*/;                            return $
```

Still pretty sluggish,   like molasses in March.


The above REXX program was modified to generate a group of numbers which were   '''AB'''   (two digit) numbers

concatenated with   '''PQ'''   (two digit)   numbers to yield a list of four digit numbers.

'''AB'''   are the 1<sup>st</sup> two digits of a   ''rare''   number,   and   '''PQ'''   are the   last two digits.


This list was sorted and the duplicates removed,   and it formed a list of   (left 2 digits abutted with the right 2 digits)

numbers that every   ''rare''   number must have   (except for the first   ''rare''   number   ('''65'''),   which is found the  ''hard''

(slow)   way.

== Tweaks, F# ==
Kudos to '''Nigel Galloway''' for the F# version.  I don't know the language well, but was able to cut a few corners to improve performance slightly and add some stats:
{{out|Output at [https://tio.run/##pVVtb9s2EP7uX3EwYIdMJEVSsjZ@E9Bh3TBA9YJ5wD4IxsBKNENUplSRmuK1/e3ZkbIVu9i36ROPL3fPPXfPaaf9vGr4y0tVcwWbgzZ8PzpbB7/@dmH@JJlQlTYy16PR7e1aCl7CL6wsq44d4F0rWm0gujdPgQdxGD2MSm4gfwQlVhv@OdhVZfEjyz@RXatAgfCTbFc1oK5AWruowJrCmcKaB8nLAs/nc3G1pUpAlm23I@e1bGEF75qGHYK84cxwiMIQdqzUfGGxwc9SFYDQ4HfWcFDt/iNvNHTSPAGDQgppdACXGcxhw2vME29C9HDKYhYAMR1nn3jhzgOI8Yw6FM0a2GoE@FlL/vO4AbGSO0QXZFIZImBicaV0C@aJ26xWUFedAoKnb@7JB4ZRNp8bQ4qq/VhyIih@EAPHPPpsBu@s9tTKPY7CFAjzI@oht1lPUhamiyhd3KeLH9LFm3S7sFT@ZZnMmH8XBMSPaRBE21daM3@WBsFDiowOCayO3ljtozNXDnX0EVsfEfpgSZKc@yFkAKWoT84RKnxAMffb22w2Q39h@hqrb4k9q6VrBwnq2A7CBtyzZ7A@wtRXGHOPWzM0Z9Y8Cy2uL@JJG@@GqBtBzw4k9b7fGu6Ki3t0Swd82DO2kmw6nUar8FQ@1VfGQmc1KqPAEiuaZSemsmhxt0V81oNenQe0rFG6XC6VC/H/P0cB0Z6mZxUsV@pr4rQmS8ObXml@ckF4TK4pYu3v6Xbv5JaE6XRqG9i2YH@EVwlPkvzxaOeVypnpQ73eOEY46dvaRHmCAlFXnriifoLk2@UNGhQL6qEckLPvYbpHfiISVsN06rTkqkZ7vesOpbMxVd0xk6NmDGvMmnek1@G@NQzlA3nVKvSHV0O3Xzeosw1WyXTzP/iz@bOReEzJThtPq4L22j29WvrD8gYid2S6wL0h4y@hF3@DL7EXzeYqxNWd9/Yb7PXYO77xjk493QXvS1ZrXnyQZSk1R@IKjXlky/fKNIfHCkEl2x44Q/JZI/6GHorto9wpLgqC@N7JrJ9yuh96hOTXOZ3gUKFgmrYfDy7NnYKxwul2/NzYW7uxZ03eI7LJ7muMzIvxEJC5gCjwKLYBh/5s1oRR@Jq4drfEkWOg8YTB@MQtHe4PMCbxnZ68LZAd3J3ExRjGY/hvWoC55yHOCGi4aRukwwrJcIHA@bPEH0hV8NHLy78 Tio.run (linked)]}}

```txt
nth        Rare Number    elapsed  completed
 1                  65      97 ms
                           120 ms    2
                           126 ms    3
                           127 ms    4
                           140 ms    5
 2             621,770     148 ms
                           148 ms    6
                           151 ms    7
                           253 ms    8
 3         281,089,082     261 ms
                           283 ms    9
 4       2,022,652,202     606 ms
 5       2,042,832,002    1162 ms
                          2528 ms   10
                          3423 ms   11
 6     872,546,974,178   16583 ms
 7     872,568,754,178   17427 ms
 8     868,591,084,757   28471 ms
                         37612 ms   12

```
Of course, it can't get too far in the 60 second timeout window.  Sometimes it doesn't get past the 5th number, due to poor luck at Tio.run.  It can do 13 digits at Tio.run, as long as you start at 13:

```txt
nth        Rare Number    elapsed  completed
 1   6,979,302,951,885   21129 ms
                         27470 ms   13

```

{{out|Output on a i7 core (Visual Studio Console App)}}

```txt
nth        Rare Number    elapsed  completed
 1                  65      22 ms
                            24 ms    2
                            26 ms    3
                            27 ms    4
                            28 ms    5
 2             621,770      31 ms
                            31 ms    6
                            33 ms    7
                            65 ms    8
 3         281,089,082      69 ms
                            76 ms    9
 4       2,022,652,202     221 ms
 5       2,042,832,002     373 ms
                           859 ms   10
                          1220 ms   11
 6     872,546,974,178    6284 ms
 7     872,568,754,178    6486 ms
 8     868,591,084,757   10336 ms
                         13739 ms   12
 9   6,979,302,951,885   16890 ms
                         19075 ms   13
10  20,313,693,904,202  105822 ms
11  20,313,839,704,202  106337 ms
12  20,331,657,922,202  116631 ms
13  20,331,875,722,202  118324 ms
14  20,333,875,702,202  122171 ms
15  40,313,893,704,200  257026 ms
16  40,351,893,720,200  258086 ms
                        283465 ms   14

```
Checking up to 14 digit numbers in under 5 minutes.  Given more time, it can get the correct 17th number (first 15 digit number), but has problems after that.  Not sure if it's the memory requirements, or perhaps I pared down Nigel's original program too much for valid output after 14 digits.<br/>
I added this here in the discussion and did not post the revised code on the main codepage out of respect for Nigel's original contribution.  All I did was tweak it, and did not add any core improvements.<br/><br/>
I've been trying to figure out how to put some limits on the permutation of numbers generated, but don't know F# well enough to do it effectively. And when I work in languages I am familiar with, the performance is decades of magnitude worse.--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 18:37, 22 September 2019 (UTC)

:: As mentioned earlier in this page, I've just added a second Go version based on Nigel's approach which is infinitely faster than the first. You're welcome to try and optimize it further as you're much better at this sort of thing than I am. The perfect square checking might be capable of improvement as I'm just using a simple math.Sqrt approach here rather than having a number of preliminary filters as I did in the first version. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 23:11, 24 September 2019 (UTC)

== Tweaks, F# (v2) ==
Thanks again '''Nigel Galloway''', for the improved F# version.  (I find it even more delightfully terse, and I understand it even less that the first version.)  However, I gave it a couple of shortcuts and got this result on the core i7:

```txt
nth              Rare Number   total time   digs  (et per dig)
 1                        65         1 ms
                                     1 ms    2    (      0 ms)
                                     1 ms    3    (      0 ms)
                                     2 ms    4    (      0 ms)
                                     2 ms    5    (      0 ms)
 2                   621,770         5 ms
                                     5 ms    6    (      2 ms)
                                     6 ms    7    (      0 ms)
                                    29 ms    8    (     22 ms)
 3               281,089,082        31 ms
                                    46 ms    9    (     16 ms)
 4             2,022,652,202        83 ms
 5             2,042,832,002       128 ms
                                   461 ms   10    (    413 ms)
                                   758 ms   11    (    296 ms)
 6           872,546,974,178      1665 ms
 7           872,568,754,178      1707 ms
 8           868,591,084,757      3203 ms
                                  6932 ms   12    (   6173 ms)
 9         6,979,302,951,885      8657 ms
                                 12306 ms   13    (   5373 ms)
10        20,313,693,904,202     27181 ms
11        20,313,839,704,202     27278 ms
12        20,331,657,922,202     29120 ms
13        20,331,875,722,202     29399 ms
14        20,333,875,702,202     30104 ms
15        40,313,893,704,200     78829 ms
16        40,351,893,720,200     79112 ms
                                128604 ms   14    ( 116297 ms)
17       200,142,385,731,002    139117 ms
18       221,462,345,754,122    139142 ms
19       816,984,566,129,618    140051 ms
20       245,518,996,076,442    140431 ms
21       204,238,494,066,002    140679 ms
22       248,359,494,187,442    140687 ms
23       244,062,891,224,042    140778 ms
24       403,058,392,434,500    181008 ms
25       441,054,594,034,340    181033 ms
                                230265 ms   15    ( 101661 ms)
26     2,133,786,945,766,212    478386 ms
27     2,135,568,943,984,212    499689 ms
28     8,191,154,686,620,818    503747 ms
29     8,191,156,864,620,818    506482 ms
30     2,135,764,587,964,212    507132 ms
31     2,135,786,765,764,212    509065 ms
32     8,191,376,864,400,818    513368 ms
33     2,078,311,262,161,202    532418 ms
34     8,052,956,026,592,517    893558 ms
35     8,052,956,206,592,517    896814 ms
36     8,650,327,689,541,457    962029 ms
37     8,650,349,867,341,457    963704 ms
38     6,157,577,986,646,405    965923 ms
39     4,135,786,945,764,210   1333750 ms
40     6,889,765,708,183,410   2225840 ms
                               2251017 ms   16    (2020751 ms)
41    86,965,750,494,756,968   2446863 ms
42    22,542,040,692,914,522   2447002 ms
43    67,725,910,561,765,640   3995397 ms
                               4106524 ms   17    (1855506 ms)
44   284,684,666,566,486,482   7761433 ms
45   225,342,456,863,243,522   7887401 ms
46   225,342,458,663,243,522   7930157 ms
47   225,342,478,643,243,522   8019971 ms
48   284,684,868,364,486,482   8085227 ms
49   871,975,098,681,469,178   8439579 ms
50   865,721,270,017,296,468   9124567 ms
51   297,128,548,234,950,692   9135222 ms
52   297,128,722,852,950,692   9145493 ms
53   811,865,096,390,477,018   9249578 ms
54   297,148,324,656,930,692   9286388 ms
55   297,148,546,434,930,692   9306517 ms
56   898,907,259,301,737,498   9679708 ms
57   631,688,638,047,992,345   16317159 ms
58   619,431,353,040,136,925   16376430 ms
59   619,631,153,042,134,925   16559935 ms
60   633,288,858,025,996,145   16629165 ms
61   633,488,632,647,994,145   16685653 ms
62   653,488,856,225,994,125   18768151 ms
63   497,168,548,234,910,690   24649427 ms
                               42231937 ms   18    (38125412 ms)

```

Tested up to 15 digits in under 4 minutes.  16 digits in under 35 minutes, 17 digits under an hour and 10 minutes.  It got to the last of the 18 digit rare numbers in under 7 hours, but it takes 11 3/4 hours to complete the block.  Still not quite fast enough to go after 19, 20 or 21 digits.
<br/>
Tio.run  [https://tio.run/##dVRtb@M2DP6eX8EFaCEnts5276VJagPDvRQHeN3hcsA@BMGgxowjTJZTS7m01/S3d5TsZu12C5JAJEXy4UOKaxOtmhYfH5stapjfGYv14NmZf/79hfhBiko3xsqVGQxevYJPUpcglIKvokXQu/oaWwN7aTcgoJSVtIbDlaxQwSVda/bibgpz3FIsugnJud3wENI4mXBg3/Yo/sLS2zmkE7sJBgottBT7alcbENkAwGlEGoLYul8KGYg8z5MQts1eQxIXwESUBC9kdyHofeWPL3OoMnfUGVurRtg8Nzct/Utt374OKpAa9EhnVe@hs8W6aUE7/SLmXKRRsoSygTuJqgTGnmfWlDuI/lHpIFge8kIay1v83kdscQXrK5BQAWYGb@5rYVcbwI65OMq7yBUcMMqPueUx5y/OexFNCs7PiyUwPZ1WATCk3A@HfI43fC2VxZatdxqqKHc5q8zpa6KMjQIKWHUXza52saucsJ6e9uwEz3FeuuwOKSiPlWwAHWDlAXvFZjq1HVbl4tXiFhwhcRFtAs5rUk1InDjRlSHXgHlSHA4qi@nfoF3ExSwpZq@L2Zti9rZY8veNtkJqwxhTFxcXSTDeBCdEaQB2QxP5RMQllT9WIxwzRRdGjgdJctXLSHL1yrsxHHXuHvDhzygnFDoX2y7gETsjtTg9PU2yuLMQtCWgMuhOnE@Kpa@hY1WN3BS69qw79hirOrh6LJ9jJTJCPa6CB6D8rn//KhgWSxpsOOTg@rJqlMKVZU@TA3nuaiVO6UuQkyIY@Ndh9vQE5rbZ7l1L@NyK1l7hnnVvp95Zca0QVs1O00DQ1djrty0N@1yXzO6n3/DW/tFKMgdsbWxodBlknqMnr4voeBxD4k12z70PG97HYfoA92mYvpnq@IFs92fhuweozTDsvcI@bGj2/KMSW4Plb1IpaXDV6NL0lVg07rH1s2f/ryyA44tgx9WgfUd8VWs9PBEwfKoQNHn0BhienCXm5F1J4AjnSVq6UlinCIYwHMLPEVI6@zP9YLC4@Khte/eloQT5suOcphZEW32HjkWn222pHDdXpOYF6oreeQ79fHnlIl663lOYbtSSc@9MPv/dvfwDXu@qClv@2fxqrVhtaG36UJ7DdAb0eEyjkL9XKFoWzFxdX8lGLDrpWWNjoD3ud7MBR3sD/hYICz@wbTyII32aYL/4@L1/5fe@G4rGCgVW1kgCrX8imblhIyNJwdAHW6Sc77bLp0F3c@dhe6NH06LdtUSL67FFKhPwVloCXeLg8fExOfsb (link)] version results:

```txt
nth              Rare Number   total time   digs  (et per dig)
 1                        65       156 ms
                                   190 ms    2    (    147 ms)
                                   199 ms    3    (      0 ms)
                                   200 ms    4    (      1 ms)
                                   201 ms    5    (      0 ms)
 2                   621,770       209 ms
                                   210 ms    6    (      9 ms)
                                   217 ms    7    (      6 ms)
                                   326 ms    8    (    109 ms)
 3               281,089,082       335 ms
                                   386 ms    9    (     59 ms)
 4             2,022,652,202       512 ms
 5             2,042,832,002       740 ms
                                  1970 ms   10    (   1583 ms)
                                  3055 ms   11    (   1085 ms)
 6           872,546,974,178      6688 ms
 7           872,568,754,178      6938 ms
 8           868,591,084,757     13531 ms
                                 28013 ms   12    (  24957 ms)
 9         6,979,302,951,885     34793 ms
                                 48838 ms   13    (  20825 ms)

```
One serious shortcut is using [0L;1L;4L;5L;6L] instead of [0..9].  This is allowed because square numbers can't end with 2, 3, 7, or 8, and 9 doesn't count because the numbers being operated upon here have 9 as a factor. --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 21:04, 29 September 2019 (UTC)

== Tweaks, Go ==
Thank you '''PureFox''' for the Go translation.  Here are the results of some code tweaking.  Feel free to re-use anything you like on the main page.  Some of the corners that were cut: 1. Computed the forward number, the reverse number and the digital root from the "digits" array all at the same time, rather than going back when needed. 2. Used a Boolean array of the last 2 digits of valid squares to determine whether to do the more computationally expensive floating point square root call. 3. The dl array can be ''seq(-9, 8)'', rather than ''seq(-9, 9)''.  It doesn't seem to matter on the low number of digits ( <16 ) we are covering.  The digital root fail check can be either before or after the !isSquare() check, you might find it slightly faster one way or the other.
{{Out| Output from a core i7}}

```txt
nth               number         time   completed
 1                    65             0s
                                970.4µs  2
                                970.4µs  3
                                970.4µs  4
                                970.4µs  5
 2               621,770        970.4µs
                                970.4µs  6
                               1.9676ms  7
                               4.9833ms  8
 3           281,089,082       5.9569ms
                               6.9785ms  9
 4         2,022,652,202      19.9197ms
 5         2,042,832,002      40.8908ms
                              84.7729ms 10
                              137.632ms 11
 6       872,546,974,178     646.2443ms
 7       872,568,754,178     677.1611ms
 8       868,591,084,757     1.0701309s
                             1.2416797s 12
 9     6,979,302,951,885     1.6326327s
                             1.9797045s 13
10    20,313,693,904,202    10.5358443s
11    20,313,839,704,202    10.6046898s
12    20,331,657,922,202    12.1206037s
13    20,331,875,722,202    12.3569934s
14    20,333,875,702,202    13.0172337s
15    40,313,893,704,200    22.9626308s
16    40,351,893,720,200    23.1066218s
                            24.4769465s 14
17   200,142,385,731,002    27.4160929s
18   221,462,345,754,122    27.6274982s
19   816,984,566,129,618     30.293394s
20   245,518,996,076,442    31.7365338s
21   204,238,494,066,002    31.9389919s
22   248,359,494,187,442    32.0028151s
23   244,062,891,224,042    32.3977374s
24   403,058,392,434,500    36.6045135s
25   441,054,594,034,340    36.8109598s
                            38.1518922s 15

```

It can't get past 15 digits, due to the memory requirement.  I purposely let the "natural" order of the results display, as I was more interested in the performance time of each result.
{{Out|Output from [https://tio.run/##jVdZb9s4EH7Pr5g1ENSqZVdWmm5ix3nZ43FRIPvmFQrFohI6EqVQcpNd1789@w1JHT5a1EAkcTic45uDk4fi7a2MV0/xg6A8luqMzmReFrqmIQ3SvB7MaZDH9SO/a5mLAXlgqf8tBdVC51TVerOqaUurQqQpSVV/@jgn@Tr18Qh5fUU7nFgVqqqh4fV3@SDrihY0vTg7@xpryja0jO6LIgNXulErktXd8ybWYqisOI94l7ZnRDIF@xJU7J3TNAi8CKq1qDdakaLFwp4YssWTz8XLMM2KmAk98t2zrlu68vDzQ89jIxtBaZxVwlj94QP9qYuc6kdBv8W6FpWMFZW6SNjpIqX6paBCU15oQZms4FgdV080pj9eax3TSotE1jSdWM9W5TCmyWSyjBgXD37bL@Mb0YpmgGVOKSR@8SnmpY4VIhMzwPR@QZlQwxjGzhmKFTsc9ACQmXGDqOSjefwkho0Kn1benO57dEd9b0ViUx1ttltU8V5gZLN1sjOtdMbzTzC5opE7OKfSMN4vq5mIsFrKCIEv5Zw4AUR7jkWufWuAlargVSmXa@aP8VqqyLnW8jMz61Ee4AZqa7qFiXiPxziscGg0MjDxJ92YVBwaw7BGwLd0r0X8xFgqqyhwKuzTgVqe7ZrErMTzMPWppiZ6NnbEWazdct7hk0I73Syoxns04jCxM2UpVDLUKA8TRqdFU6tlVeTIU/lfL/9RZFI9QIIJA6pycleCUqfDwXkyAHCAOhMNIJW3HyfsjOmCjbk16SVpvKALIw3RWgYzRGVEA3@AZ7WUs6hnVwW7TJVKtqUxkRvF0HOBr2oUBtzxqSxeWB@3iclfKD7PJ24DpvSmbOKG2gTjkvZNBbfGVs8@JaYEfDY3QcwuEU9LXtCweoaBiWcLn2kjCuEFGkL1zOFDJxIudqhb7k4VlyhgHJP2SLyWWlSVLJTRpjb5vdDMkNiOlHKhh1QXXZcysuIs@9vI6pcUS/d77QwZ2DmimTUEhhz9lgdrzoI2iRlVa6QV124wjCj1adCnTA28IUvGu0N1ryYkw214TLIHnm8/9Jjhl1OugnDesC2YMiK7gAM92/hnbWsz1ix9Q92yPTjBBvlO2s7bO2w4Puw54cjhgW9dVTdALzVkhyaivOyVJMIKPEsOWio13ydSbSrKYnw2UQQM79Q7DmMZg4ce5FeunfpRVoA829iuk@aZjWfJt8lV1LTJDoJgRtst3At3Pm3RC692@Jgy8ZNPl474K4jtiY@8@dGngDkveQGOC158cscCdyxkYuuS86jAJaOtG3imqdBCrcRPe5TkSZuhR24ddASXHaY/IRZNf3JtONlPIO6slmttC3FtkyY5yBkYsEyiLmHs2nd9cYv@sN6h5Tm/U1R/gj9p4uB4AtMwAOElMANE3HDH18Des9/Yv@Y@Yi5ac8wdTDMnFa3xs@uMqn6k/Z8r@TbB0af41i3yMhO1SP5Rgw4nlfhUJHgoU09IBDMV@HzDqOSgsFWyV9kuFQ9vU5V0NcKDDF9FTcqrMPLo1tCMd3w9WTdbPM2SQfN6NWNas0okJwrQCI2qvZ7whS@sbpDoKYSK5mirxBEQNStqW0/MLMevcOcd3MGQvYpV0onHgGPMxIzjHbQTlWuXV@hbwX63MIn5Izv58Aj9YGKnzPdODCvHTNE3y4HLB274Rv/2jX7pBspce2ZUVTUahzg4xlgmsmqzam@TN7rUllWbkO1YgZayNAYFEcYLdNwTProBj8uOjzC756rv7CBZf0IjV5iD4JTGI1CQz/D@WK7MDhF0wU3kXmjBfCKwXcYvXfYAAQbBP0GeRtwh@lAtE2leAfhP0afRkbZu1Emkd7TbOpqJ8Xje2IAsvb2lKWuH4ExEBx730rCXz2Cdzpj52OUTbktujKddd1vW/S5uhgpXrfun6CfcP2E4BgL9FQL2Sszf/7o6rLqey2aYDnyHKVvuhuomUx1xbpmhw94B6xNzwwmjEHGMNLhDcMra1MATecx0cm/Nezg8arGM5icVIeBgu6VrTm7Nw@31CYyOKdWLrFePfAQtIcZ/fKG5eNCpr2eUiDTeZPXse83CKd7rLTAVvtzgsvxBlzm8ps7DhOg8DCo8pxcVLiEzS3f/BaQvwMHM1HcSA8HQzNuebRvfqffuS4X8TxDXw4J@wWt@oPuyMlphQGI0DwYndJmra@7msN3b2/8 Tio.run (linked)]}}

```txt
nth               number         time   completed
 1                    65      104.828µs
                              131.427µs  2
                              139.331µs  3
                              178.561µs  4
                              200.489µs  5
 2               621,770      698.249µs
                              757.813µs  6
                              1.42756ms  7
                             8.927975ms  8
 3           281,089,082    10.244811ms
                            12.738426ms  9
 4         2,022,652,202     72.43969ms
 5         2,042,832,002   123.310535ms
                           218.093377ms 10
                           415.295536ms 11
 6       872,546,974,178    1.93598265s
 7       872,568,754,178   2.000165728s
 8       868,591,084,757   2.821645253s
                           3.286164167s 12
 9     6,979,302,951,885   5.924838452s
                           6.864383645s 13
```

It can't get past 13 digits on Tio.run, due to memory requirement.  Execution time at Tio.run is often worse than this, but it always completes in the 60 second time limit.
--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 22:30, 28 September 2019 (UTC)

:Thanks for trying to do something here.

:Curiously, it's slower than before when I run it several times on my core i7. The range to get to 15 digits is between 46 and 49 seconds whereas the previous version is steady at around 42 seconds. I've recently upgraded from Go version 1.12.9 to 1.13.1 (the latest as I post this) but I doubt whether it would affect this particular program.

:As you're getting 38 seconds for your 'tweaked' version, then your core i7 is probably faster than mine though presumably that time was faster than the original version on the same machine so I'm not sure what to make of it.

:As you say there are memory problems when trying to go above 15 digits, though I see that Nigel has today posted a new F# version which has managed to reach 17 digits without using Cartesian products. So I think we're going to have to take another look at it anyway. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 18:41, 29 September 2019 (UTC)

:: The original version of your 2nd Go program runs about 2-3 seconds slower than the version I put at the Tio.run link.  So these tweaks are not much of an improvement.  Thanks so much for sharing your version.  Not sure why it would go slower on your core i7.  I just installed Go for the first time at v 1.13.1.  I am careful to keep any other programs from executing concurrently that might interfere with the timing measurements.  The full name of my cpu is i7-7700 @ 3.6Ghz.  Operating on Win10. It is not overclocked, I have not verified it's speed with any benchmarking software. --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 04:34, 30 September 2019 (UTC)

::: Mine's an Intel 8565U which has a much lower base frequency (1.8 GHz) but a higher turbo frequency (4.6 GHz) than yours (4.2 GHz I believe). I suspect yours will be faster overall but there's probably not a great deal in it.

:::Incidentally, I'm using Ubuntu 18.04 rather than Windows 10 but I've no reason to suppose that Go executes faster on one rather than the other nowadays.

:::Anyway, the good news is that I've come up with a new strategy which is significantly faster - 15 digits is processed in around 28 seconds compared to 42 seconds previously.

:::Basically, I'm using a combination of Nigel and Shyam's approachs which cuts down the Cartesian products quite a lot but, unfortunately, still not enough to process 16 digits before running out of memory.

:::To deal with the memory problem, I've added a second version which delivers the Cartesian products 100 at a time rather than en masse. Not surprisingly, the former is slower than the latter and it's back up to about 43 seconds to get to 15 digits. However, 16 digits takes less than 7 minutes and 17 digits less than 12 minutes - I couldn't be bothered to go any further than that - so it's a worthwhile trade-off.

:::The idiomatic way to 'yield' values in Go is to spawn a goroutine and pass it a channel (or a buffered channel in this case). However, I'd stress here that I'm not trying to parallelize the algorithm (although one certainly could) as I don't like doing this for RC tasks. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 15:14, 30 September 2019 (UTC)

::::I thought I'd see if I could extend the program to process 18 digit numbers but was surprised when it blew up with an OOM error after hitting the 56th rare number! Frankly, I don't understand why - there appeared to be plenty of unused memory when I profiled it. It may be due to heap fragmentation as the Go GC doesn't compact the heap after a collection and so needs to find a large enough slot for new allocations.

::::Anyway, I thought I'd get rid of the Cartesian product function altogether and replace it with (in effect) a nested loop and was glad I did as this has restored performance to previous levels and solved the OOM problem. 15, 16 and 17 digits are dispatched in 28 seconds, 4 minutes and 6 minutes respectively and even 18 digits completes in a tolerable 74 minutes.

::::To reliably go any further than this would require the use of big integers (unpleasant and relatively slow in Go) as signed 64 bit integers have a 19 digit maximum. It might be possible to use unsigned 64 bit integers (20 digit maximum) though this would require some fancy footwork to deal with negative numbers and subtraction. So I think that's my lot now :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 20:04, 2 October 2019 (UTC)

== 21 digit rare numbers ==
Well, one anyway (so far). I tweaked the BigInteger version of the C# program to skip to start at 21 digits. Around 6 hours, I got the first one: '''219,518,549,668,074,815,912''', with the sum = '''20,953,210,268^2''', and the difference = '''8,877,000^2'''.  Still have no idea how long it will take to finish the block of 21 digit numbers.  Since the difference found so far was a relatively low number, it probably has quite a while to go.

I am also running another instance that checks the block of 20 digit numbers (in order to verify the algorithm against the table of known rare numbers), but after 6 hours, it still hasn't come up with anything yet. A little surprising, as there are a few 20 digit rare numbers with 7 digit differences.  If I don't see anything on the 20 digit run in 6 more hours, there may be some kind of issue to work out. --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 02:52, 21 October 2019 (UTC)

P.S. 5 found so far:

```txt

Time (hours)  rare number
       6      219,518,549,668,074,815,912
      10 1/2  837,982,875,780,054,779,738
      11 1/2  208,393,425,242,000,083,802
      12 1/3  286,694,688,797,362,186,682
      13 2/3  257,661,195,832,219,326,752
```

--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 21:29, 22 October 2019 (UTC)
