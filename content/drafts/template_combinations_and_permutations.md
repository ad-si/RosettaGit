+++
title = "Template:Combinations and permutations"
description = ""
date = 2014-01-17T05:03:56Z
aliases = []
[extra]
id = 13369
[taxonomies]
categories = []
tags = []
+++

{|class="wikitable"
|+ The number of samples of size k from n objects.
With [[combinations and permutations]] generation tasks.
|- 
!
! Order Unimportant
! Order Important
|-
! rowspan=2 | Without replacement
| align="center" |<math> \binom nk = ^n\operatorname C_k = \frac{n(n-1)\ldots(n-k+1)}{k(k-1)\dots1} </math>
| align="center" |<math>^n\operatorname P_k = n\cdot(n-1)\cdot(n-2)\cdots(n-k+1)</math>
|-
| align="center" |Task: [[Combinations]]
| align="center" |Task: [[Permutations]]
|-
! rowspan=2 | With replacement
| align="center" |<math> \binom {n+k-1}k = ^{n+k-1}\operatorname C_k = {(n+k-1)! \over (n-1)!k!}</math>
| align="center" |<math>n^k</math>
|-
| align="center" |Task: [[Combinations with repetitions]]
| align="center" |Task: [[Permutations with repetitions]]
|}
