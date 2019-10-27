+++
title = "Talk:Extreme floating point values"
description = ""
date = 2018-04-18T07:42:47Z
aliases = []
[extra]
id = 7771
[taxonomies]
categories = []
tags = []
+++

==Why Draft?==
Because I am no expert in this field and might not be using the right terms in the description or might have left something out. --[[User:Paddy3118|Paddy3118]] 19:39, 15 July 2010 (UTC)

==Unfortunate task name==
These values are not extreme. They are '''not''' numbers, and of course, not floating point. Technically they are called ''ideals'' and used to make operations like +,-,*,/ closed. Another, often better, example of ideals are numeric exceptions. They too close operations. For example, + is closed in the set of real numbers filled up by ''overflow-exception'' ideal. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 21:37, 15 July 2010 (UTC)

:Hi Dmitry, I have never heard of 'Ideals' with respect to floating point; and indeed, it is not in the quoted reference [http://docs.sun.com/source/806-3568/ncg_goldberg.html What Every Computer Scientist Should Know About Floating-Point Arithmetic]. After reading the reference again, it seems to call them 'Special Quantities' which would lead to a task title something like "Special quantities in floating point values" which would read better than say "Ideal values in floating point values". If, however, ideal is a common term amongst floating point specialists, then maybe we should use the latter? --[[User:Paddy3118|Paddy3118]] 21:52, 15 July 2010 (UTC)

:They are values of the IEEE floating point type. --[[Special:Contributions/71.141.138.49|71.141.138.49]] 04:33, 16 July 2010 (UTC)
::Do you know if the IEEE spec' uses a blanket term for them? I had hoped to leave the description loose enough for those using other floating point spec's to adapt accordingly and form their own entries. As the very informative Ada entry has managed to do. ( Yeah!) --[[User:Paddy3118|Paddy3118]] 05:43, 16 July 2010 (UTC)

:::The term ideal is used in mathematics, at least in some textbooks. It is something that does not exist in the set. -1 is an ideal added to natural numbers to have 0-1. j is an ideal added to real numbers to get sqrt(-1). Infinity can be added to get 1/0.

:::I looked in IEEE 2006 draft [http://www.validlab.com/754R], it uses ''infinity'' and sometimes ''infinite number''. I must say I don't like ''infinite number'' because +∞ does not look like a number. As for ''quantity'' it does not look good either, because quantity is a measurable property, like 5.4 meters or 3 apples. Infinity as defined by IEEE 754 is not measurable, not even in terms of cardinal numbers. It is "bigger" than any cardinal. By the way ''extreme'' is also a term, which does not apply to infinities and infinitesimals. At least I was taught that an extreme does not exist if not finite. NaN they call "Not a Number, a symbolic floating-point datum." Well, ''symbolic datum'' is definitely better than anything else, e.g. number, quantity or value, because data can be both quantitative and qualitative.

:::Finally, I agree that NaN, infinity etc are definitely values of a floating-point type. E.g. NaN is a value of double in C++. But there is a difference between the semantics (mathematical meaning of NaN) and the types system (bit patterns considered valid representations for some type, they can be any). --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:23, 16 July 2010 (UTC)

==Use expressions involving other 'normal' floating point values==
Several examples forget to show this part of the task and instead just use predefined language constants. Whilst that is a good thing to show and should remain part of any expanded answer, the task specifically asks that you "use expressions involving other 'normal' floating point values" to calculate them. 

I have marked the first, Perl 6, as incomplete and will do others as time permits. (Unless what I propose is not wise ). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:11, 3 October 2015 (UTC)

==Humbled==
I just re-read the Fortran and Ada entries. They are good, aren't they. RC doesn't have a method to flag particularly informative entries such as these; (but if it did, who would police attribution etc...) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:31, 16 April 2018 (UTC)
:Well... No. As to the Fortran entry, I don't regard verbosity as quality. Older details regarding long-defunct machines are easily found on Bisavers, and are not specific to Fortran (and writing a good summary would require much more work). Negative zero is introduced, but we don't know why (''Certain calculations are said to benefit from the states "positive zero", and "negative zero" being available'', that can't be serious), and I don't know were the authors has found a link with theoretical differentiation. All of this is just hovering over the subject. If you want a good account, find good books or articles about floating-point (Goldberg of course, but also Higham, Golub & Van Loan, Muller...). Particularly informative, this? No, I definitely don't think so. Anyway, I don't think this is the right place for a course on floating point: RC is good but is not the right format for this, or at least not in "task space". [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 18:06, 16 April 2018 (UTC)

::We are of differing opinion; on the web!
::So many languages have grown up with IEEE 754 support as standard. It is good to know a little more about languages that could support differing FP representations (and that might still be asked to do that on current hardware). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:16, 16 April 2018 (UTC)
:::Most standardized languages assume as little as possible about the floating-point representation (Fortran, C, Ada for instance). Most scripting languages make no specific assumption and take the C mlib for granted. Actually, except Java, I know no language that explicitly states that FP must follow the IEEE 754 standard (and it's not mandatory either in Java). Thus, almost all languages "can support differing FP representation". However, it is very interesting to have an account of historical machines (and not only about FP). But here it's buried in a task that is only slightly related, in a specific language although we are talking about the machines. It would be better to have a good explanation on a separate page, as it's absolutely not specific to one language. Even in the old days, Fortran was never alone - though it was certainly the main language for number crunching. Regarding Fortran entries in general: one contributor enjoys giving many details about defunct machines, and this is sad because, again, it's not about Fortran, and also it gives the impression that Fortran is defunct too. But it's wrong: there have been four ISO standards since Fortran 77, and the fifth is about to be published. Historical details about obscure functionality of antiquated compilers may be interesting, but details about how to write good programs in Fortran 2008 (or Fortran 2018) would be even more interesting, if we assume those who come on RC wants to learn usable material. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 07:41, 18 April 2018 (UTC)
