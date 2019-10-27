+++
title = "Talk:Safe addition"
description = ""
date = 2011-06-10T04:03:27Z
aliases = []
[extra]
id = 4766
[taxonomies]
categories = []
tags = []
+++

Wouldn't the sum of the two ranges be (''a''+<small>&darr;</small>''b'', ''a''+<small>&uarr;</small>''b'')? Modulo whether they are closed or open bounds, of course (my argument doesn't depend on that). I can't see any logical reason for assuming otherwise, as the “real” values of ''a'' and ''b'' could be anywhere from the lower bound to the upper bound; that's got to be what the range means, with appropriate infinitesimals in the open bound case, of course. (Yes, this does mean that the error will increase as more operations are performed; this is a normal feature of calculations with physical quantities if I'm remembering my Physics-101 class right.) —[[User:Dkf|Donal Fellows]] 22:52, 18 August 2009 (UTC)

: I am not sure, if this answers your question. Addition of intervals is defined as [''a'', ''b''] + [''c'', ''d''] = [''a'' + ''c'', ''b'' + ''d'']. With +&darr; and +&uarr; operations its machine implementation becomes [''a'' +&darr; ''c'', ''b'' +&uarr; ''d'']. As for error (rather uncertainty) of the result obtained using interval arithmetic, it is not a simple issue. The result's width grows under the assumption of the worst case scenario. That is, assuming all intervals involved in computations independent. The intermediate results are often dependent, so the inputs, etc. Therefore in many cases the result can be improved in terms of interval width. A simple illustration of the case. Consider interval expression [1, 2] x [1, 2]. According to the interval arithmetic, the result is [1, 4]. But that is when [1, 2] and [1, 2] are fully independent. If they were ''same'' (correlated), we could multiply [1, 2] by 2, and get a two-fold narrower result [1, 2] * 2 = [2, 4]. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 08:46, 19 August 2009 (UTC)

I misunderstood the original task, so my comments before are correct but distinctly off-topic. :-)
According to the documentation I can scare up with Google on IEEE rounding, the normal mode is round-to-nearest as that minimizes the error (to 0.5 ULP) given that its a single-valued result. The documentation also says that it is a really bad idea to call the standard math functions with the rounding mode set to anything else. Given that, the only sane method of implementing this task (assuming that the language has no portable way to set the rounding mode for the duration of the computation, e.g., [[C]] doesn't) is to compute and then widen with <code>nextafter</code>. —[[User:Dkf|Donal Fellows]] 08:56, 19 August 2009 (UTC)

== Error propagation? ==

Wouldn't it be more sensible to include error propagation in the task? Just two exact number adding to a sum +/- machine epsilon is of pretty limited use. --[[User:Ledrug|Ledrug]] 04:03, 10 June 2011 (UTC)
