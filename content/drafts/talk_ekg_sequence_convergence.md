+++
title = "Talk:EKG sequence convergence"
description = ""
date = 2018-12-20T01:20:55Z
aliases = []
[extra]
id = 21944
[taxonomies]
categories = []
tags = []
+++

==Problem with task==
The task description states "Variants of the sequence can be generated starting 1, N where N is any natural number larger than one". The examples all have N prime. Is it the intention that N may be composite? If so should there be an example say 10? --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:31, 8 August 2018 (UTC)
: Hi Nigel, that second number, then one after the initial 1; can be any integer greater than one. 2 is the base, or normal sequence known as the EKG sequence when people don't mention the variants. When developing the task I noted that 3 and 7 seemed to converge then diverge before finally converging (see the second Python example), so that lead to me asking for 2, 5, and 7.
:It can be other than prime. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:27, 8 August 2018 (UTC)
::I'll take that as a yes. I have modified the task to require EKG(9) and EKG(10). If ekg(0)=1 and ekg(1)=N then if N is prime ekg(2)=2N. If N is composite ekg(2)=smallest prime factor of N. As none of the code on the task page attempts to factorize N, I think all the solutions will be wrong. This divergence/convergence is illusionary. OK the video draws a nice chart and compares it to the tree of life, but this just goes to prove that people should need a license to use PowerPoint, and similar. It says nothing about the relationship of N1 and N2. More interesting would be to find how out of kilter ekg(N) is with ekg(2). That is how many transpositions are needed to convert ekg(2) to ekg(N). I think this would partition the natural numbers into a small number of sets. --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:41, 8 August 2018 (UTC)

Hi Nigel. I will take another look at the task and convergence over the next few days. I would discourage people from adding examples until then. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:03, 8 August 2018 (UTC)

I had a look:
# The first Python solution is wrong - I'll either fix it or delete it. Thanks for finding that.
# The second Python solution does seem to be right. It gives different results than the first Python solution for EKG(9) and EKG(10). Its results for EKG(9) and EKG(10) match those of [https://oeis.org/A169849 A169849] and [https://oeis.org/A169851 A169851], respectively.

Addressing your comments:
* ''"I have modified the task to require EKG(9) and EKG(10)"'' - You've certainly shown that there is a need.
* ''"I think all the solutions will be wrong"'' - Not necessarily, they just need to cover the extra requirements. Their algorithms may be correct I mean.
::Debatable try EKG(any 4 dıgıt prıme). I stopped waıtıng after 20 mıns!!!!!--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:49, 7 December 2018 (UTC)
* ''"This divergence/convergence is illusionary"''. It is real. The Python generator that uses gcd is easiest for me to reason about. What it generates next is solely down to the state help in names <code>last</code> and <code>so_far</code>. If they coincide between two generators then those generators could not then diverge.

::So what ıs convergence ıf the algorithm does not have <code>last</code> and <code>so_far</code>.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:49, 7 December 2018 (UTC)
::: An interpretation of the pseudocode in the convergence section. The state of th genertor as well as the value last generated must be equivalent. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:07, 7 December 2018 (UTC)
:::: There is no pseudocode in the convergence section to ınterpret. Should ıt not be possıble to determıne ıf 2 sequences have converged wıthout reference to the functıon that generated them.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:27, 10 December 2018 (UTC)

I've learnt something more. Thanks Nigel. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:34, 8 August 2018 (UTC)

==EKG convergence==
Hi Nigel, Showing more terms:

```txt
EKG(5): 1, 5, 10, 2, 4, 6, 3, 9, 12, 8, 14, 7, 21, 15, 18, 16, 20, 22, 11, 33, 24, 26
EKG(7): 1, 7, 14, 2, 4, 6, 3, 9, 12, 8, 10, 5, 15, 18, 16, 20, 22, 11, 33, 21, 24, 26
```

Although there is an earlier run of equal outputs, they actually converge at the 21st term. One neds information on the state used to generate the outputs to determine true convergence. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 11:58, 10 December 2018 (UTC)



The point is that you do not and should not. EKG(a) and EKG(b) converge when the set a(1)..a(n-1) equals set b(1)..b(n-1) and a(n) equals b(n). What ıs the point of RC if the solutions presented show less competence than would be expected of a schoolkid?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:32, 19 December 2018 (UTC)

Stop the name calling or you may be blocked Nigel. For convergence think more of when they will always continue to generate the same terms. 
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 01:20, 20 December 2018 (UTC)
