+++
title = "Talk:Range extraction"
description = ""
date = 2019-08-06T21:03:55Z
aliases = []
[extra]
id = 7774
[taxonomies]
categories = []
tags = []
+++

== Might I suggest ...==
Might I suggest breaking the format description out to a separate page and transcluding it both here and in [[Range expansion]]? --[[User:Short Circuit|Michael Mol]] 00:17, 16 July 2010 (UTC)

:Hi Michael, do you have an example page? I'm just worried that the page with the format description might look a little 'lost' as the format is only loosely based on the relatively obscure [http://www-cecpv.u-strasbg.fr/Documentations/lsf/html/lsf6.1_admin/G_jobarrays.html job array index format for LSF].
:: I just put in a simple example implementation of what I was describing. --[[User:Short Circuit|Michael Mol]] 10:42, 16 July 2010 (UTC)
::: I didn't know you could do that. I've added a few small edits to all three pages. --[[User:Paddy3118|Paddy3118]] 12:57, 16 July 2010 (UTC)

==Without Duck Typing==
Hi Neville, I noted this comment from your Algol solution:
:''Note: The closest concept that Algol 68 has to duck typing is the tagged union. If duck typing was available it could reduced the size of the code specimen, but would have lost some of Algol 68's strong type data security. ''
Thinking about it, you could form and use a list of ranges for what is to become each entry in the 'rangified' internal format, where each entry range is a pair of integers. It would then only be converted to the dash separated integer range form, or two comma separated digits, or one digit, in the routine to create a correctly formed string depending if the second number minus the first number in the range is: >= 2, or ==1, or == 0. 

--[[User:Paddy3118|Paddy3118]] 05:24, 16 July 2010 (UTC)

I see that you are saying.  Essentially the middle iterator ''gen range merge'' is actually doing this that you describe. 
 
I inserted ''gen range merge'' in the middle of a chain of iterators.  These chained iterators do the following steps:
# Iterate through three different types of initial arrays - []'''int''', []'''range''' and []'''rangeint''' with ''gen range'', yielding '''range'''(''lwb'',''upb'')
# Iterate with ''gen range merge'' yielding <u>merged</u> '''range'''(''lwb'',''upb'')
# Iterate with ''gen range int merge'', merging and yielding a '''union''' of '''int''' and '''range'''
# Finally iterate with ''range int list init'' '''exiting''' with an array of '''union''' of '''int''' and '''range'''.

I could have just restricted the code to the behaviour of ''gen range merge'' with 'rangified' internal format, but I having used python heaps and I am endeared to the flexibility that duck typing and iterators provide to python.  In this instance I am glad to be able to mimic some of the ''duck typing'' flexibility and use iterators in the much older Algol 68.  

Moreover this code specimen also produces the nice "bi-product" of a family of '''range''' helper functions, and a non-trivial example of iterating in Algol 68.

[[User:NevilleDNZ|NevilleDNZ]] 07:02, 16 July 2010 (UTC)

: Thanks for the further explanation. --[[User:Paddy3118|Paddy3118]] 08:14, 16 July 2010 (UTC)

==Sample Output Request==
Hi, without a sample of the Ocaml and Oz output, I can't check that it correctly performs the task. --[[User:Paddy3118|Paddy3118]] 05:24, 17 July 2010 (UTC)

==Task name==
Just wondering ... Since the complementary task is "Range expansion", would a better name for this task be "Range compression" or maybe "Range contraction"? Or is extraction the usual term for this? --[[User:Snoman|Snoman]] 06:52, 18 July 2010 (UTC)
:Hmm, I don't like 'Range compression' as the phrase seems less descriptive of what is happening. It could mean too many other things. 'Range formatting' is more precise, but I like the alliteration of 'Range '''ex'''traction' and 'Range '''ex'''pansion' so far. --[[User:Paddy3118|Paddy3118]] 10:02, 18 July 2010 (UTC)

== Inverse of Range expansion task? ==

Is it intended that the output of the Range extraction task be suitable input for the Range expansion task and vice versa? I.e. that they are the inverse of each other. i.e.:

 ordered-list-of-integers == expandRange(extractRange(ordered-list-of-integers))

If this is the case then if a "printed representation of your languages internal list of integers data structure that also reads nicely to humans" is required output for the Range expansion task then perhaps the "printing" functionality would be cleaner as separate to the actual Range expansion function?

Whether these functions are inverses of each other or not I think it would be nice to use the same example set of numbers for both tasks (the one from Range expansion task: -6,-3--1,3-5,7-11,14,15,17-20 would probably be most rigorous).

:::::: Except that some programs fail when the first integer is part of a range  (2 were flagged). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:38, 18 April 2013 (UTC)

: Hi Tikkanz, It is enough that the range created/expanded is of the specified format, including:
:* No extra spaces 
:* Only commas, dashes and numbers 
:* etc ...
:Examples should standardize on the range format of the task description (There are some that add spaces and colons for example, that I have flagged incomplete).

:Unfortunately I didn't think of a good example to start off as I did not complete both tasks before creating the task pages, hence the different examples. I think it might be too late now to change? --[[User:Paddy3118|Paddy3118]] 07:53, 9 August 2010 (UTC)

==Comment in Ada solution on notation==
:''"For real-life applications it is better to use the notation -9..-4"'' 
True. if I were doing this for real then I would have liked to use a different range indication character, but, as in so many things, I modified/simplified an [http://www.ccs.miami.edu/hpc/lsf/7.0.6/admin/jobarrays.html#wp1004400 existing format that used dashes], to allow negative numbers - hence the compromise. (On seeing how easy it was to parse I left it in).

==TUSCRIPT example==
It would be good to both keep the current TUSCRIPT example - noting where it fails to follow the task description; as well as create a fully compliant version. It was pleasantly surprising to see that the language had a built-in routine that could so nearly produce the right answer! --[[User:Paddy3118|Paddy3118]] 21:31, 28 January 2011 (UTC)

==prime number musing==

[Under the category of using a hammer to swat a fly.]

One could use   ''range extraction''   for composite numbers to show prime numbers (indicated by the ''gaps'').

For the first 100 composite numbers:

: 4 6 8-10 12 14-16 18 20-22 24-28 30 32-36 38-40 42 44-46 48-52 54-58 60 62-66 68-70 72 74-78 80-82 84-88 90-96 98-100

Of course, primes   '''2'''   and   '''3'''   would be a special case. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:33, 18 April 2013 (UTC)

:With the requisite commas added of course :-)
--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:56, 19 April 2013 (UTC)

I was thinking that the commas would make the gaps (indicating primes) less visible:

: 4,6,8-10,12,14-16,18,20-22,24-28,30,32-36,38-40,42,44-46,48-52,54-58,60,62-66,68-70,72,74-78,80-82,84-88,90-96,98-100

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:08, 19 April 2013 (UTC)

::Sorry Gerard I didn't mean for you to go and make the change, I was jokingly making a reference to the rigidity of the spec for the format. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:45, 19 April 2013 (UTC)
