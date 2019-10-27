+++
title = "Talk:Sailors, coconuts and a monkey problem"
description = ""
date = 2015-10-28T19:04:37Z
aliases = []
[extra]
id = 19085
[taxonomies]
categories = []
tags = []
+++

== J ==
I smiled at that end comment, but there may be an issue with the choice of an end value to search over. It seems that unlike testing incrementing values you have to put a ceiling on the range searched for. 

But then you would have to do that in many solutions such as choosing an integer type in C or a range to search over in constraint solvers so just ignore me.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:35, 3 May 2015 (UTC)

I do have to put in a ceiling - that gives me bound search time and protects me from "infinite loop" bugs while I'm playing with the code. But if a given value doesn't give me good results, it's trivial for me to multiply it by 10 and try again. I guess what I'm saying is that for this problem, this approach saved time for me. (But I guess you basically said this already, in your second paragraph.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:55, 3 May 2015 (UTC)

== Analysis ==

Let the solution be described by:

```txt

n6	g6
n5	g5
n4	g4
n3	g3
n2	g2
n1	g1

```

where n is the number of coconuts at each stage and g is the number of coconuts in each pile.

note that g1 is n1/5 and g2*4, which implies that n1 is divisible by 20. It is simple to calculate the entire table given n1. It is obvious that n1 is of the form X + (4*5)*2*(2*)*(2*2)*(2*2*2*2) or 5120. X is divisible by 20 and less than 5120. Which implies that J's maximum value is justified if over generous. By examination X must be a member of the series 20 + 40*z. Let us look at that: 

```txt

20	26	33.5	42.875
60	76	96	121
100	126	158.5	199.125
140	176	221	277.25
180	226	283.5	355.375
220	276	346	433.5
260	326	408.5	511.625
300	376	471	589.75
340	426	533.5	667.875
380	476	596	746
420	526	658.5	824.125
460	576	721	902.25

```

By examining the 4th column we can see that when the step is 320 ((4*5)*2*(2*)*(2*2) X is 60. So X must be a member of the series 60 + 320*z. Let us look at that:

```txt

60	76	96	121	152.25	        191.3125
380	476	596	746	933.5	        1167.875
700	876	1096	1371	1714.75	        2144.4375
1020	1276	1596	1996	2496	        3121
1340	1676	2096	2621	3277.25	        4097.5625
1660	2076	2596	3246	4058.5	        5074.125
1980	2476	3096	3871	4839.75	        6050.6875
2300	2876	3596	4496	5621	        7027.25
2620	3276	4096	5121	6402.25	        8003.8125
2940	3676	4596	5746	7183.5	        8980.375
3260	4076	5096	6371	7964.75	        9956.9375
3580	4476	5596	6996	8746	        10933.5
3900	4876	6096	7621	9527.25	        11910.0625
4220	5276	6596	8246	10308.5	        12886.625
4540	5676	7096	8871	11089.75	13863.1875
4860	6076	7596	9496	11871	        14839.75
5180	6476	8096	10121	12652.25	15816.3125
5500	6876	8596	10746	13433.5	        16792.875
5820	7276	9096	11371	14214.75	17769.4375
6140	7676	9596	11996	14996	        18746
6460	8076	10096	12621	15777.25	19722.5625
6780	8476	10596	13246	16558.5	        20699.125
7100	8876	11096	13871	17339.75	21675.6875
7420	9276	11596	14496	18121	        22652.25
7740	9676	12096	15121	18902.25	23628.8125
8060	10076	12596	15746	19683.5	        24605.375
8380	10476	13096	16371	20464.75	25581.9375
8700	10876	13596	16996	21246	        26558.5
9020	11276	14096	17621	22027.25	27535.0625
9340	11676	14596	18246	22808.5	        28511.625
9660	12076	15096	18871	23589.75	29488.1875
9980	12476	15596	19496	24371	        30464.75
10300	12876	16096	20121	25152.25	31441.3125
10620	13276	16596	20746	25933.5 	32417.875
10940	13676	17096	21371	26714.75	33394.4375
11260	14076	17596	21996	27496   	34371
11580	14476	18096	22621	28277.25	35347.5625
11900	14876	18596	23246	29058.5 	36324.125
12220	15276	19096	23871	29839.75	37300.6875
12540	15676	19596	24496	30621   	38277.25
12860	16076	20096	25121	31402.25	39253.8125
13180	16476	20596	25746	32183.5 	40230.375
13500	16876	21096	26371	32964.75	41206.9375
13820	17276	21596	26996	33746   	42183.5
14140	17676	22096	27621	34527.25	43160.0625
14460	18076	22596	28246	35308.5 	44136.625
14780	18476	23096	28871	36089.75	45113.1875
15100	18876	23596	29496	36871   	46089.75
15420	19276	24096	30121	37652.25	47066.3125

```

Examination of the final column we see that for the step of 5120 "(4*5)*2*(2*)*(2*2)*(2*2*2*2)" X is 1020.

--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 09:44, 5 May 2015 (UTC)

==Haskell version==

Testing the Haskell version here with GHC 7.8.4 and finding that ''pure'' is out of scope.

Perhaps add '''Control.Applicative''' to the imports ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:04, 28 October 2015 (UTC)
