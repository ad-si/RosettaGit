+++
title = "Talk:Lychrel numbers"
description = ""
date = 2015-09-12T13:56:35Z
aliases = []
[extra]
id = 19543
[taxonomies]
categories = []
tags = []
+++

==Related==
My list of the related for reference:

```txt
 295  394  493  592  689  691  788  790  887  978  986 1495 1497 1585 1587 1675 1677 1765 1767 1855 
1857 1945 1947 2494 2496 2584 2586 2674 2676 2764 2766 2854 2856 2944 2946 2996 3493 3495 3583 3585 
3673 3675 3763 3765 3853 3855 3943 3945 3995 4079 4169 4259 4349 4439 4492 4494 4529 4582 4584 4619 
4672 4674 4709 4762 4764 4799 4852 4854 4889 4942 4944 4979 4994 5078 5168 5258 5348 5438 5491 5493 
5528 5581 5583 5618 5671 5673 5708 5761 5763 5798 5851 5853 5888 5941 5943 5978 5993 6077 6167 6257 
6347 6437 6490 6492 6527 6580 6582 6617 6670 6672 6707 6760 6762 6797 6850 6852 6887 6940 6942 6977 
6992 7076 7149 7166 7239 7256 7329 7346 7419 7436 7491 7509 7526 7581 7599 7616 7671 7689 7706 7761 
7779 7796 7851 7869 7886 7941 7959 7976 7991 8058 8075 8079 8089 8148 8165 8169 8179 8238 8255 8259 
8269 8328 8345 8349 8359 8418 8435 8439 8449 8490 8508 8525 8529 8539 8580 8598 8615 8619 8629 8670 
8688 8705 8709 8719 8760 8778 8795 8799 8809 8850 8868 8885 8889 8899 8940 8958 8975 8979 8989 8990 
9057 9074 9078 9088 9147 9164 9168 9178 9237 9254 9258 9268 9327 9344 9348 9358 9417 9434 9438 9448 
9507 9524 9528 9538 9597 9614 9618 9628 9687 9704 9708 9718 9777 9794 9798 9808 9867 9884 9888 9898 
9957 9974 9978 9988
```

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:58, 29 August 2015 (UTC)

==J answer==
I do like the kind of exploratory feel given to the J example - I am new to it too and was using Python in a similar fashion :-)

Would just ask that the count of relateds (as opposed to relateds + true Lychrel candidates) be given.

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:51, 29 August 2015 (UTC)

:Thanks for the update/fix. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:19, 29 August 2015 (UTC)

: I have added that. But note that the task description currently asks for us to "Find the number of Lychrel and related numbers for a starting n in the ...". And there might be two different ways to parse this sentence:

:: "Find the number of (Lychrel and related numbers) ...", or

:: "Find the number of Lychrel numbers and find the number of related numbers ...".

: I understood the sentence to mean the former, but it sounds like you had intended the latter? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:21, 29 August 2015 (UTC)

::: Thanks Rdm. I have made an update to the task description. Shout if it is still confusing. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:34, 30 August 2015 (UTC)

:::: It actually is still slightly odd. The first step asks us to find the number of true lychrel numbers, and the second step asks us to print both the number of true lychrel numbers and the numbers themselves. Why not just ask to find the numbers themselves and then print the numbers (and how many of them)? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:31, 30 August 2015 (UTC)

::::: Agreed with Rdm - displaying the count for such a short list seems redundant.  Splitting the second Task bullet in two might also make it scan better:
:::::*  Find the number of true Lychrel number candidates and related numbers for a starting n in the range 1..10000 inclusive. (With that iteration limit of 500).
:::::*  Print all of the true Lychrel numbers found
:::::*  Print the ''number'' of related numbers found
:::::*  Print any Lychrel or related number that is itself a palindrome
::::: --[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 13:21, 31 August 2015 (UTC)

==Related numbers could be clearer in the description==

I found the definition of Related numbers a bit tricky to parse (and fielded a question on irc from another user who was confused).  Perhaps explicitly referring to OEIS sequence A088753 ("seed" Lychrel numbers) as well as A023108 (seed and related Lychrel numbers) would make this a little clearer?
Alternatively, by way of illustration:  196 is Lychrel, with its sequence beginning 196+691=887...  295 is Related because 295+592=887, and 887 already belongs to 196's sequence.  --[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 13:21, 31 August 2015 (UTC)
: Re-worded using your suggested seed/related idea.--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:02, 31 August 2015 (UTC)
:: That reads much more clearly to me, and the example is better than mine for taking two steps. --[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 23:37, 31 August 2015 (UTC)

==simpler wording==

:: For:     ''1.   Take an integer n, greater than zero.''

:: Use:     ''1.   Take a positive integer n.''

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:30, 31 August 2015 (UTC)

:Isn't it the case that while the latter may be technically correct and less wordy, the former states more explicitly the omission of zero? I tend to want to keep the former. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:59, 31 August 2015 (UTC)

::: Zero is not positive.   Both are technically correct, I didn't imply that one was incorrect, I just stated an opinion that one sentence is more simpler (and cleaner).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:16, 31 August 2015 (UTC)

:: Why is this even an issue? Regardless of whether or not 0 is tested, 0 is not a Lychrel number because 0+0 = 0 and 0 is a palindrome. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:01, 31 August 2015 (UTC)

== How can a Related number be a palindrome? ==

We're asked to list "Seeds or related numbers" that are palindromes.
How is it possible that a related number is palindromic?
Surely that would stop its seed from being a Lycrel number?
Could someone provide an e.g. and explanation (or rewrite the task objetive)?
--[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 07:59, 11 September 2015 (UTC)

: |Hi Tim, there is a version of the algorithm that does not produce palindromes, but the key is in this:
:: ''Notice that the check for a palindrome happens after an addition.''
: That example starts with the palindromic number 55 but the check comes later.
: --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 21:03, 11 September 2015 (UTC)

: 4994 is an example of a related number which is a palindrome.  There are other examples. And like Paddy said, we only reject the seed if a sum was a decimal palindrome. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:18, 12 September 2015 (UTC)
