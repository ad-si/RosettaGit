+++
title = "Talk:NYSIIS"
description = ""
date = 2015-08-13T02:40:41Z
aliases = []
[extra]
id = 13158
[taxonomies]
categories = []
tags = []
+++

==task status==

What must one do to accomplish the task? It doesn't say. --[[User:Paddy3118|Paddy3118]] 13:06, 23 March 2013 (UTC)

:It is still ambiguous. Really the task was promoted out of draft too soon. Typical nysiis usage assumes removal of common suffixes before encoding, and the reference implementation ( Caché ObjectScript ) did that, so I did as well in Perl 6. It probably should be put into the task description though (and soon, before too many implementations are added). Also, typically, the algorithm is used to encode both first and last names in one go. None of the implementations as of now will handle that. IE "John Smith" -> nysiis -> "JAN SNAT".

:There are a few ambiguities in the algorithm description too IMO. Should you consider the first letter when encoding H? Should Wheeler encode as WALAR or WHALAR? ''I'' believe that it '''should''' consider the first character (WALAR - W is a non-vowel so remove the H) but it is open to interpretation.
:Also, When removing terminal S or A. If the last characters are AS and you remove the S, do you then need to remove the newly terminal A or should you only remove the last character once? Should Louis encode as L or LA? Again, it is open to interpretation. --[[User:Thundergnat|Thundergnat]] 21:03, 23 March 2013 (UTC)

::Well I changed it back to draft status as I thought it needed just this kind of discussion to improve the task description.
::How about changing the task description to explicitly state that just the wikipedia algorithm need be implemented but leaving it open for implementors to add extra functionality as long as they state that they are doing that (and maybe what extra functionality is being added).? --[[User:Paddy3118|Paddy3118]] 06:49, 24 March 2013 (UTC)

Apologies to all, I am new to this site and regret not being more precise about this particular task.  I should have also read the guidelines before starting.  The task is basically to implement the NYSIIS algorithm, but I have noticed the original algorithm has subsequently been modified by others (presumably to improve its indexing capabilities).  I propose the main task should be implement the standard NYSIIS algorithm, as shown on Wikipedia, which I believe is based on a single name.  There should then be the optional task of allowing multiple names to be processed (I will give some examples of those, including double-barrelled names, or double surname, including removing unnecessary suffixes/honours that are not required for indexing purposes).  Does this sound okay?  BTW, I appreciate all the helpful comments already posted here.  --[[User:Toucanbird|Toucanbird]] 11:42, 24 March 2013 (UTC)

:Hey Toucanbird, that's OK - I was a newbie here once too. I followed links on suffixes/honours and found that there are so many of them that just listing them would take up too much space and that was just for British ones. Just following the wp (wikipedia) algorithm might make for the best task but you might allow for people to show how to handle a small sample of suffices as extra credit. What do you think? --[[User:Paddy3118|Paddy3118]] 16:07, 24 March 2013 (UTC)

::I have amended the task wording slightly to allow for a small selection of suffixes/honours to suffice for demonstration purposes - feel free to amend further if necessary. --[[User:Toucanbird|Toucanbird]] 19:42, 24 March 2013 (UTC)

==post-nominal letters==

As far as the problem of the numerous post-nominal letters (honorific, professional, generational, and others ...),
most of them have a common identifier   (just to list a very small sampling): 

```txt

A.B  Atty.  B.A.  B.E.  B.F.A.  B.S.  B.Sc.  B.Tech.  C.S.V.  CEng.  CFA.  D.C.  D.D.  D.O.  D.Phil.
Dr.  e.g.  Ed.D.  Eng.D.  Esq.  etc.  family.  grandfather.  herself.  himself.  II.  III. IV.  J.D
J.D.  Jnr.  Jr.  Junior.  K.B.E.  L.  L.L.B  lawyer.  LL.D  LL.D.  LL.M  M.A.  M.B.A.  M.D.  M.Eng
M.F.A.  M.L.A.  M.S.  M.Sc.  Master.  MEOA.  Minor.  Miss.  Mr.  Mrs.  Ms.  Mz.  nephew.  O-3.
O.F.M.  P.E.  P.G.  Ph.D.  Pharm.D.  R.A.  R.I.P  S.  Snr.  son.  Sr.

```


The REXX entry that I coded tests for the secret character in the last word in the name, and if found, ignores it   (elides it from the name).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:27, 12 August 2015 (UTC)

== Uncertainty about wikipedia spec ==

For a name like 'JOHN DOE' is 'D' a "first character of a name"? Is 'N' a "last character of a name"? And does each step use the same definition for these concepts? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:40, 13 August 2015 (UTC)
