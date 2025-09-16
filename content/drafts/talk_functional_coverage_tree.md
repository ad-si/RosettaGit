+++
title = "Talk:Functional coverage tree"
description = ""
date = 2019-10-13T20:04:39Z
aliases = []
[extra]
id = 19506
[taxonomies]
categories = []
tags = []
+++

==Data in a more functional form==

```python
add_node('/cleaning', 1, 0)
add_node('/cleaning/house1', 40, 0)
add_node('/cleaning/house1/bedrooms', 1, 0.25)
add_node('/cleaning/house1/bathrooms', 1, 0)
add_node('/cleaning/house1/bathrooms/bathroom1', 1, 0.5)
add_node('/cleaning/house1/bathrooms/bathroom2', 1, 0)
add_node('/cleaning/house1/bathrooms/outside_lavatory', 1, 1)
add_node('/cleaning/house1/attic', 1, 0.75)
add_node('/cleaning/house1/kitchen', 1, 0.1)
add_node('/cleaning/house1/living_rooms', 1, 0)
add_node('/cleaning/house1/living_rooms/lounge', 1, 0)
add_node('/cleaning/house1/living_rooms/dining_room', 1, 0)
add_node('/cleaning/house1/living_rooms/conservatory', 1, 0)
add_node('/cleaning/house1/living_rooms/playroom', 1, 1)
add_node('/cleaning/house1/basement', 1, 0)
add_node('/cleaning/house1/garage', 1, 0)
add_node('/cleaning/house1/garden', 1, 0.8)
add_node('/cleaning/house2', 60, 0)
add_node('/cleaning/house2/upstairs', 1, 0)
add_node('/cleaning/house2/upstairs/bedrooms', 1, 0)
add_node('/cleaning/house2/upstairs/bedrooms/suite_1', 1, 0)
add_node('/cleaning/house2/upstairs/bedrooms/suite_2', 1, 0)
add_node('/cleaning/house2/upstairs/bedrooms/bedroom_3', 1, 0)
add_node('/cleaning/house2/upstairs/bedrooms/bedroom_4', 1, 0)
add_node('/cleaning/house2/upstairs/bathroom', 1, 0)
add_node('/cleaning/house2/upstairs/toilet', 1, 0)
add_node('/cleaning/house2/upstairs/attics', 1, 0.6)
add_node('/cleaning/house2/groundfloor', 1, 0)
add_node('/cleaning/house2/groundfloor/kitchen', 1, 0)
add_node('/cleaning/house2/groundfloor/living_rooms', 1, 0)
add_node('/cleaning/house2/groundfloor/living_rooms/lounge', 1, 0)
add_node('/cleaning/house2/groundfloor/living_rooms/dining_room', 1, 0)
add_node('/cleaning/house2/groundfloor/living_rooms/conservatory', 1, 0)
add_node('/cleaning/house2/groundfloor/living_rooms/playroom', 1, 0)
add_node('/cleaning/house2/groundfloor/wet_room_&_toilet', 1, 0)
add_node('/cleaning/house2/groundfloor/garage', 1, 0)
add_node('/cleaning/house2/groundfloor/garden', 1, 0.9)
add_node('/cleaning/house2/groundfloor/hot_tub_suite', 1, 1)
add_node('/cleaning/house2/basement', 1, 0)
add_node('/cleaning/house2/basement/cellars', 1, 1)
add_node('/cleaning/house2/basement/wine_cellar', 1, 1)
add_node('/cleaning/house2/basement/cinema', 1, 0.75)
```


I did not use the above, but thought I should generate it. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:02, 12 August 2015 (UTC)
: But I've used it subsequently in the Python class-based version as I wanted the task to focus more on the calculations in the tree and the display of output in textual form that might normally be done in something like this [http://ludo.cubicphuse.nl/jquery-treetable/#examples treetable] or this [http://mleibman.github.io/SlickGrid/examples/example5-collapsing.html treetable]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:12, 15 August 2015 (UTC) 


### Weighted sums

The specified weights look like they are meant to be percentages (all=100), but the specification for missing weights seems to be fractional (all=1). I think that this deserves either a bit more explanation (as to why the use of "1" for default weight is correct) or correction. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:53, 13 August 2015 (UTC)

:Hi Rdm, Those weights are not percentages - They are "fractions of the sum of the weights" at that level. I was playing around with what weight to apply to house1 and house2 and rejected 1, 2 then eventually considered 2, 3 and realised that for those integers it would be the same as 4, 6 and 40, 60 - saw the total weight being 100, and the:
:# Ease of hand calculation.
:# Similarity to prcentages.
:And so went with that. Never thinking that it might confuse rather than help.

:Here are some doodles around the weighted average calculation:
:
```python
>>>
 def wt_avg(wt, cov):
...     wts = sum(wt)
...     covs = sum(c * w for c, w in zip(cov, wt))
...     return covs / wts
... 
>>> weights = [1, 1]
>>> child_cov = [0.25, 0.75]
>>> wt_avg(weights, child_cov)
0.5
>>> weights = [2, 3]
>>> wt_avg(weights, child_cov)
0.55
>>> weights = [4, 6]
>>> wt_avg(weights, child_cov)
0.55
>>> weights = [40, 60]
>>> wt_avg(weights, child_cov)
0.55
>>> 
>>> # From wikipedias basic example:
>>> weights = [20, 30]
>>> child_cov = [80, 90]
>>> wt_avg(weights, child_cov)
86.0
>>> 
```



### Tightening up the description of the coverage calculation ?


The given formulation of the coverage values of nodes sounds like a statement about '''all''' nodes in the tree, but is in fact, of course, valid only for a '''minority''' of them. 

(It's valid for those which have children – but the majority are leaf nodes, with coverage values which are direct and supplied rather than computed. If we took the task description literally, the coverage value of all leaf nodes, and hence of all parent nodes too, would be zero, or perhaps just undefined).

Perhaps worth tightening up that sentence, or adding one to cover the majority case, in which the number of children is zero ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:49, 13 October 2019 (UTC)
