+++
title = "Talk:Birthday problem"
description = ""
date = 2014-05-19T16:43:08Z
aliases = []
[extra]
id = 16639
[taxonomies]
categories = []
tags = []
+++

== usage of ''people''==

Why assume that the people choosen are alive?    If dead, then "they" aren't people, but corpses.   The task clearly states a group of people.   However, even if dead, they still had a birthday. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:02, 4 November 2013 (UTC)

== years in Algol  ==

What does years signify in "%age of years with required common birthdays: 50.71%;" ? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:36, 4 November 2013 (UTC)

== Meaning of precision  ==

I’m not sure what “''Estimating the error in the estimate to help ensure the estimate is accurate to 4 decimal places''” means. My interpretation is that the <math>\sigma_\mathrm{mean}</math> of the estimated frequency is <math><1/10000</math>. 

The <math>\sigma_\mathrm{mean}</math> of the distribution of each sample is calculated using the binomial distribution, so it’s is at most <math>1/2</math> (for <math>p=q=1/2</math>). We are very near that value. 
In this case, if we use <math>\sigma_\mathrm{mean}=\sigma /Sqrt(N)</math> then the number of samples has to be <math>N=25000000=10000^2/4</math>.

Most of the examples use <math>N=50000</math>, so they don’t have enough precision. On the other hand, <math>25000000</math> is to much, so I run my example in Racket with only <math>250000</math>, that gives a <math>\sigma</math> of only <math>1/1000</math>.

I think that the description of the task should ask for a clearer goal. For example: “''Do the final simulation with at least 25000000 samples to help ensure the estimate is accurate to 3 decimal places''.”
