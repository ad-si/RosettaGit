+++
title = "Talk:Van Eck sequence"
description = ""
date = 2019-07-05T10:53:30Z
aliases = []
[extra]
id = 22387
[taxonomies]
categories = []
tags = []
+++

==F# Excess==
Could have spared the reader having to find the required output amongst the copious results given. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:43, 21 June 2019 (UTC)
==density of numbers ==
I have counted the "0" in my array of Position before.That is counting the numbers, still not found.
The dense of found numbers is decreasing from 40% at 1e1 downto 19,6%/1e3 to 10.8%/1e9 ( maybe ~5% at 1e27 :-) ) so using a dictionary is a good idea.

```txt

        Max   not found 
(0..9)   10          6  (-> not found: 3,4,5,7,8,9)
       1000        804
   32381775   28492874
 1000000000  892005005
```


: When investigating the algorithm, I programmed the Python dict version first, as I thought it might be good to not keep the whole sequence. After starting to write the task description though I thught it would be good for some readers to have an alternate algorithm using the list, so added that too. I'm lazy at times; thanks for showing the differences. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:51, 5 July 2019 (UTC)
