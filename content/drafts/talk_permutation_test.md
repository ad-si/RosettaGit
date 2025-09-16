+++
title = "Talk:Permutation test"
description = ""
date = 2011-02-10T21:05:03Z
aliases = []
[extra]
id = 9201
[taxonomies]
categories = []
tags = []
+++

== Difference in results? ==

I see that the Tcl and Ursala code seem to be calculating different results. How can this be the case? I've been careful to check that the number of cases generated in the Tcl code is the correct one (92378 for selecting 10 from 19) so I ''think'' that's correct... –[[User:Dkf|Donal Fellows]] 14:48, 1 February 2011 (UTC)

:I am getting different results form everyone else, myself.  I have 80551 cases which have a difference in means which is less than or equal to the result's difference in means (and note that this includes the result), and I have 11827 cases where the difference in means is greater than the result's difference in means.  My smallest difference in means is -0.518111 and my largest difference in means is 0.501556.  My difference in means for the original result is 0.153222.  Since other people are getting different numbers, I am curious about what these statistics look like for them (or if I have made a mistake -- which should show up in these stats).  --[[User:Rdm|Rdm]] 20:42, 3 February 2011 (UTC)

::Ah, I think I see:  I have 313 cases which are equal to my result, that's 0.34% of the total.  I am using an epsilon of result*(2^-44) which is approximately 9e-15, which is much tighter than experimental accuracy, and the largest difference between a value I classify as equal and the computed result is approximately 2e-16.  Differing epsilons, or differences in floating point implementations for systems without epsilon are enough to account the differences I currently see on the task page --[[User:Rdm|Rdm]] 21:23, 3 February 2011 (UTC)

When setting the task, I neglected to specify whether the difference in means should be calculated as the treatment group mean minus the control group mean, or vice versa. It's just a matter of convention, but now I realize that it affects the result because there are 313 alternatives with equal differences regardless, as [[User:Rdm|Rdm]] notes, but the lesser and greater designations will of course depend on the direction of the subtraction. I propose that two results be accepted as valid, either <math>13.14%</math> : <math>86.86%</math>, or <math>12.80%</math> : <math>87.20%</math>.

Unfortunately there is still an issue of roundoff error, as [[User:Rdm|Rdm]] also notes. Scaling the experimental data up by a factor of 100 seems to be an adequate workaround in the Ursala solution (and possibly others using standard IEEE double precision math). Can any numerical analysis experts reading this please suggest a better one? I've tried using sums as a proxy for means, and also calculating means and sums
the careful way by first sorting the numbers in order of absolute value, which didn't help.
To help head off any further arguments about the results, here are my statistics, which agree with those of [[User:Rdm|Rdm]].


```txt
empirical difference in means (treatment - control):  0.153222222222222287

number of lesser or equal alternatives: 80551

partial sorted listing of lesser or equal alternatives (run length coded)

         (1,-0.518111111111111189)
         (1,-0.484333333333333227)
         (1,-0.482222222222222219)
         (1,-0.477999999999999980)
         (1,-0.467444444444444385)
         (1,-0.465333333333333377)
         (2,-0.463222222222222146)
         (2,-0.461111111111111138)
         (1,-0.456888888888888844)
         (1,-0.448444444444444479)
         .
         .
         .
         (357,0.134222222222222326)
         (357,0.136333333333333251)
         (351,0.138444444444444426)
         (336,0.140555555555555572)
         (335,0.142666666666666719)
         (335,0.144777777777777755)
         (325,0.146888888888888819)
         (319,0.148999999999999994)
         (319,0.151111111111111140)
         (313,0.153222222222222287)

number of greater alternatives: 11827

partial sorted listing of greater alternatives (run length coded):

         (310,0.155333333333333323)
         (297,0.157444444444444387)
         (299,0.159555555555555562)
         (288,0.161666666666666708)
         (286,0.163777777777777717)
         (286,0.165888888888888891)
         (269,0.167999999999999983)
         (273,0.170111111111111130)
         (265,0.172222222222222276)
         (266,0.174333333333333312)
         .
         .
         .
         (1,0.448777777777777720)
         (1,0.452999999999999958)
         (2,0.459333333333333316)
         (1,0.461444444444444435)
         (1,0.463555555555555554)
         (1,0.465666666666666673)
         (1,0.480444444444444452)
         (1,0.495222222222222230)
         (1,0.499444444444444413)
         (1,0.501555555555555532)
```


--[[User:Sluggo|Sluggo]] 02:06, 4 February 2011 (UTC)


### Using rational arithmetic

I wondered about the accuracy of results after it was queried above and ran the Python solution but with rational numbers as input to see what effect this may have. 

I should explain that the first set of results are produced using the Python program from the page which works in double precision floating point, in the idle Python IDE. I then show the calculation of a result using rationals (the fractions module).

```python
>>>
 
### ============================= RESTART =============================

>>> 
under=86.90%, over=13.10%
>>> from fractions import Fraction as F
>>> t = [F(tr) for tr in treatmentGroup]
>>> c = [F(cn) for cn in controlGroup]
>>> u = permutationTest(t, c)
>>> print("under=%.2f%%, over=%.2f%%" % (u, 100. - u))
under=86.89%, over=13.11%
```


--[[User:Paddy3118|Paddy3118]] 03:22, 4 February 2011 (UTC)

:Both sets of results above appear to be incorrect. Do you believe the latter is correct, and if so, can you support it by listing statistics comparable to those above? I maintain that if you investigate it, you'll find that many of the large group of alternative means that should be exactly equal to the empirical mean are off by a little and therefore are counted among those that are greater or lesser. --[[User:Sluggo|Sluggo]] 23:43, 4 February 2011 (UTC)

:Using rational arithmetic, I get:

:<lang>under: 87.1972%
over:  12.8028%
```
 when I subtract mean control effects from mean treatment effects (experimental result is 1379 divided by 90), and I get
:<lang>under: 13.1417%
over:  86.8583%
```
 when I subtract mean treatment effects from mean control effects (experimental result is -1379 divided by 90).

:Have I made a logical error?  --[[User:Rdm|Rdm]] 15:33, 4 February 2011 (UTC)

:: No. The problem is that it seems to be an ill-conditioned problem. IEEE arithmetic is getting in the way and causing all sorts of trouble. That's really nasty. The only good way to deal with this is to change the task so it doesn't have the problem, since the magic needed to fix it is evil and problem-specific (multiplying through by 100 pushes the figures into the stable range). Ugh. –[[User:Dkf|Donal Fellows]] 15:53, 4 February 2011 (UTC)

:::Actually, I was asking about how the large and small value reversed (between the under and over categories) in those two cases.  --[[User:Rdm|Rdm]] 17:14, 4 February 2011 (UTC)


### Using integer arithmetic 

Rationals are a bit of overkill when the denominator is always the same.  I just posted a version using all integer arithmetic.  The problem with floats is those 313 cases where the differences are equal.  Anything that causes the difference to be off by the tiniest bit can cause them to be mis-categorized.  I don't know enough statistics to know how this is typically handled.  For this task, it might be enough to change the task description to provide experimental results as an integer score from 0 to 100.  While we're at it, we could change the task to specify that the difference is treatment-control.  That would eliminate the double solution.&mdash;[[User:Sonia|Sonia]] 20:12, 4 February 2011 (UTC)

:I'm in favor of amending the task as you recommend, and I'll do it if no one objects in the next few days or does it first. --[[User:Sluggo|Sluggo]] 23:46, 4 February 2011 (UTC)

== Name of task ==
Shouldn't this be all combinations instead of all permutations?  What difference does the order of members within the control or treatment group make (other than extra computation time)?  --[[User:Rdm|Rdm]] 16:08, 1 February 2011 (UTC)

: I'm currently using "all combinations", which is reasonably fast (though I'm careful to treat each sample point independently, just in case there are repeated samples). All permutations is much slower (or it is if you generate directly) and I'm not sure how much difference it should make to the result since you'd effectively just be multiplying the number of each count by <math>m!n!</math> (== 1316818944000 in this example; we'd be waiting a while for the result) and then dropping all that anyway when you average out. –[[User:Dkf|Donal Fellows]] 21:46, 1 February 2011 (UTC)

: "Permutation test" is the conventional name for this test according to the Wikipedia, even though it may be a misnomer. --[[User:Sluggo|Sluggo]] 02:24, 4 February 2011 (UTC)

== Change in specification ==

In keeping with the developing consensus of opinion, I have changed the task specification to use integer test data, and specified that the difference in means is to be calculated by subtracting the control group mean from the treatment group mean. The correct results should be <math>12.80%</math> and <math>87.20%</math>. --[[User:Sluggo|Sluggo]] 21:05, 10 February 2011 (UTC)
