+++
title = "Talk:State name puzzle"
description = ""
date = 2012-04-29T06:58:35Z
aliases = []
[extra]
id = 10413
[taxonomies]
categories = []
tags = []
+++

==The rules of harmonising==
Why don't you get answers of the form:
  ('Wisconsin', 'Kory New') 
    <-> ('Wisconsin', 'New Kory') 
    <-> ('Wisconsin', 'New York') 
    <-> ('Wisconsin', 'Wen Kory') 
    <-> ('York New', 'Wisconsin')
? --[[User:Paddy3118|Paddy3118]] 08:16, 29 August 2011 (UTC)
: Off the top because Wisconsin was one of the original states.  Or put another way ('Wisconsin', 'New Kory')  -> ('Wisconsin', 'New Kory') - the new pair doesn't count as "two other U.S. States" --[[User:Dgamey|Dgamey]] 11:36, 29 August 2011 (UTC)
:: Sorry, that should have read  ('Wisconsin', 'Kory New')  -> ('Wisconsin', 'New Kory').  Because in (A, B) -> ( C, D), A,B,C, and D need to be distinct otherwise it's not ''two'' other states. --[[User:Dgamey|Dgamey]] 14:19, 29 August 2011 (UTC)

Hi Dgamey, I paired Wisconsin with:
  'Kory New'
  'New Kory'
  'New York'
  'Wen Kory'
  and 'York New'
As they don't seem to be duplicates of each other. If the harmonisation where to equate states of two or more words if the individual words are the same, then I could then see that 'New York' is equivalent to 'York New', and 'Kory New' === 'New Kory', but I am not sure of this from the task definition. --[[User:Paddy3118|Paddy3118]] 13:57, 29 August 2011 (UTC)

:I think the issue is ''The challenge was to take the names of two U.S. States, mix them all together, then rearrange the letters to form the names of two other U.S. States''.  So for example, if you paired 'Wisconsin' with 'Kory New', what are the other two states?  --[[User:Rdm|Rdm]] 14:05, 29 August 2011 (UTC)
:: Huh I thought "no duplicates" means state names with same letter counts are considered the same. --[[User:Ledrug|Ledrug]] 16:10, 29 August 2011 (UTC)

:: So RDM, For example ('Wisconsin', 'Kory New') cannot become ('Wisconsin', 'Wen Kory') because they both contain Wisconsin? --[[User:Paddy3118|Paddy3118]] 19:34, 29 August 2011 (UTC)
::: Yes, I think so?  Put differently if Wisconsin and Kory New are a permutation of Wisconsin and New York, then they would also be a permutation of New York and Wisconsin, wouldn't they?  That said, this issue can be treated in various ways and "any different combination" works fine to implement the original puzzle, even though it's inadequate with the fake state names. --[[User:Rdm|Rdm]] 00:54, 31 August 2011 (UTC)
::: That only makes sense.  If A and B are permutations of each other, then any C + A is a permutation of C + B, which will give a lot of boring answers. --[[User:Ledrug|Ledrug]] 22:58, 29 August 2011 (UTC)
:::: I made some minor changes to the task page caveats (in parenthesis) which hopefully clarifies things. --[[User:Dgamey|Dgamey]] 01:32, 30 August 2011 (UTC)
::::: I see differences in results on the "extended" list of names. Python and C give different results, etc. --[[User:Bearophile]]
:::::: It's a conscious choice.  Like I said above, since "New Kory" and "Kory New" are permutations of each other, outputing hundreds of "State X + New Kory = State X + Kory New" is a positively boring thing to do -- for the user who's watching the output.  Plus it's questionable what "two other states" means in this case since "State X" shows up on both sides.  It's a matter of preference IMO, where the C code prevents this kind of output, while the python code doesn't.  I prefer the C solution (surprise!). --[[User:Ledrug|Ledrug]] 06:58, 29 April 2012 (UTC)

== Time to leave draft? ==
This appears to have settled down.  I'll check in in a couple of weeks. If nothing new comes up, I'll remove draft.  --[[User:Dgamey|Dgamey]] 03:25, 9 September 2011 (UTC)
