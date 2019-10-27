+++
title = "Talk:Averages/Mean angle"
description = ""
date = 2012-07-23T20:51:08Z
aliases = []
[extra]
id = 11984
[taxonomies]
categories = []
tags = []
+++

==Accuracy==
Huh, the mean angle of 90, 180, 270 and 360 is "-90.0 degrees"?  Doesn't that sound a little fruity? --[[User:Ledrug|Ledrug]] 23:31, 9 July 2012 (UTC)
:Well, if done exactly, the mean of those complex numbers should be 0, and then the angle would be, well, undefined, or perhaps whatever default the library picks (like 0). But what is actually happening in this case is that the mean of those complex numbers is not calculated to be exactly 0, due to floating-point error when converting between degrees and radians, and then converting between rectangular and polar coordinates. It is a very small number that is very close to 0
 >>> print sum(rect(1, radians(d)) for d in [90, 180, 270, 360])
 -2.28847549044e-17j
:The angle is then calculated from whatever nonzero components that this small non-zero number happens to have. Getting an angle from a mean this small is unstable, because any tiny change (perhaps by a different order of calculations, resulting in different behavior of the error) would result in a radically different angle. --[[User:Spoon!|Spoon!]] 05:35, 10 July 2012 (UTC)

::Hmm - I did rush this as I was very tired. This draft task might start a little shakily but I did try to gather the essence of something that had not occurred to me before last night and I intend to tidy-up so please criticise away and make/suggest improvements. I am feeling quite sheepish about having computed average times (another circular mean), for so long and not been actively aware of what I had been doing. --[[User:Paddy3118|Paddy3118]] 06:55, 10 July 2012 (UTC)

:: Which reminds me that I should work on a similar task for mean times. --[[User:Paddy3118|Paddy3118]] 06:55, 10 July 2012 (UTC)

:: It's not just a question of numerical stability, the real problem is that "average of directions" is a bad concept.  "Mean" is inherently tied to summation, but summing directions is just ridiculous.  What happens when you add east to north, do you get west, south, or north by northeast maybe?
:: Besides--it's moot--to get an angle out of a vector, atan(y/x) is no good: you need atan2(y, x). --[[User:Ledrug|Ledrug]] 07:42, 10 July 2012 (UTC)

:::Not so ridiculous. If you have flashes at a distance that you think are localized then you could measure their bearing and want some numerical number that indicates where they are likely to be coming from. An average of the individual, circular, bearings would help. I have to read a bit more about opposite directions but there was something about the size of the radius of the resultant for me to grasp. --[[User:Paddy3118|Paddy3118]] 08:28, 10 July 2012 (UTC)

:::: That's one problem of the task: given an angle a and an integer n, equation n x == a doesn't have a unique solution for x. Which one you pick depends on what use scenario you want, converting angles to unit vectors, averaging them and converting back is simply not a universal solution.  Suppose you see three of your flashes and want to choose a heading so you can get closer to investigate; if the degrees of the angles were 10, 20 and 30, you can head to 20 and that's that, but if then angles were 0, -95 and 95, there's no intuitive way to say which direction to go. By your math you'd be going to 0, but you'd be getting closer to one source while getting further away from the other two (I'm not saying you should go the opposite dirction: it depends on what kind of "getting closer to the sources" you really want).  Another thing, as currently specified, you average vectors, then keep the angle and discard the amplitude from the result, thus losing relevant information, which just can't be a good way to do things.  As a result, you end up with singularities when the result vector is small, and a potential arbitrary "mean value".--[[User:Ledrug|Ledrug]] 03:24, 12 July 2012 (UTC)

:::Take a look at [[wp:Directional statistics]] and the other pages it might link to, as well as talk pages. I got the impression that this is not for all angular measurements - sometimes the number of turns is significant; and it is easy to get answers that are computed correctly but without checking the amplitude of the resultant mean of vectors, you miss out on information on what credence to give to the result. What is the mean of 0 and 180 degrees? the amplitude of the resultant is zero so any angle returned is of no import. 
:::There does seem to be situations where the formula is applied to give meaningful results. You might be stimulating a species of land mammal in a controlled environment and monitoring in which direction they, ran for example. --[[User:Paddy3118|Paddy3118]] 06:15, 12 July 2012 (UTC)

:::: (Regarding an amplitude of 0, angle returned is of no import) --- yes, I noticed that when the REXX trig functions used short-circuiting and returned exact values instead of near values, this made a significant difference on how '''ATAN2''' ''interpreted'' the results, as it uses the sign of the arguments to figure out what quadrant it's in, and '''-8.154E-61''' (for ATAN2) is much different than zero, albeit a very small difference from zero. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:47, 23 July 2012 (UTC)

:::: Also note that '''[90, 180, 270, 360]''' is the same as '''[-270, -180, -90, 0]'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:51, 23 July 2012 (UTC)
