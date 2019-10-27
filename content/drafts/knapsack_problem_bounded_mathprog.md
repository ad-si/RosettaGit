+++
title = "Knapsack problem/Bounded/Mathprog"
description = ""
date = 2012-01-11T21:22:41Z
aliases = []
[extra]
id = 11167
[taxonomies]
categories = []
tags = []
+++


```txt

Problem:    knapsack
Rows:       2
Columns:    22 (22 integer, 11 binary)
Non-zeros:  44
Status:     INTEGER OPTIMAL
Objective:  knap_value = 1010 (MAXimum)

   No.   Row name        Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 knap_weight               396                         400 
     2 knap_value               1010                             

   No. Column name       Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 take[map]    *              1             0             1 
     2 take[compass]
                    *              1             0             1 
     3 take[water]  *              1             0             2 
     4 take[sandwich]
                    *              0             0             2 
     5 take[glucose]
                    *              2             0             2 
     6 take[tin]    *              0             0             3 
     7 take[banana] *              3             0             3 
     8 take[apple]  *              0             0             3 
     9 take[cheese] *              1             0             1 
    10 take[beer]   *              0             0             3 
    11 take[suntancream]
                    *              1             0             1 
    12 take[camera] *              0             0             1 
    13 take[T-shirt]
                    *              0             0             2 
    14 take[trousers]
                    *              0             0             2 
    15 take[umbrella]
                    *              0             0             1 
    16 take[w-trousers]
                    *              0             0             1 
    17 take[w-overclothes]
                    *              1             0             1 
    18 take[note-case]
                    *              1             0             1 
    19 take[sunglasses]
                    *              1             0             1 
    20 take[towel]  *              0             0             2 
    21 take[socks]  *              1             0             1 
    22 take[book]   *              0             0             2 

Integer feasibility conditions:

KKT.PE: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

KKT.PB: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

End of output

```

