+++
title = "Knapsack problem/0-1/Mathprog"
description = ""
date = 2012-01-11T21:16:16Z
aliases = []
[extra]
id = 11172
[taxonomies]
categories = []
tags = []
+++


```txt

Problem:    knapsack0
Rows:       2
Columns:    22 (22 integer, 22 binary)
Non-zeros:  44
Status:     INTEGER OPTIMAL
Objective:  knap_value = 1030 (MAXimum)

   No.   Row name        Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 knap_weight               396                         400 
     2 knap_value               1030                             

   No. Column name       Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 take[map]    *              1             0             1 
     2 take[compass]
                    *              1             0             1 
     3 take[water]  *              1             0             1 
     4 take[sandwich]
                    *              1             0             1 
     5 take[glucose]
                    *              1             0             1 
     6 take[tin]    *              0             0             1 
     7 take[banana] *              1             0             1 
     8 take[apple]  *              0             0             1 
     9 take[cheese] *              0             0             1 
    10 take[beer]   *              0             0             1 
    11 take[suntancream]
                    *              1             0             1 
    12 take[camera] *              0             0             1 
    13 take[T-shirt]
                    *              0             0             1 
    14 take[trousers]
                    *              0             0             1 
    15 take[umbrella]
                    *              0             0             1 
    16 take[w-trousers]
                    *              1             0             1 
    17 take[w-overclothes]
                    *              1             0             1 
    18 take[note-case]
                    *              1             0             1 
    19 take[sunglasses]
                    *              1             0             1 
    20 take[towel]  *              0             0             1 
    21 take[socks]  *              1             0             1 
    22 take[book]   *              0             0             1 

Integer feasibility conditions:

KKT.PE: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

KKT.PB: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

End of output

```

