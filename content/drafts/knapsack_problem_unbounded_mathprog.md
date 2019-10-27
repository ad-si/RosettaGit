+++
title = "Knapsack problem/Unbounded/Mathprog"
description = ""
date = 2012-01-11T21:15:48Z
aliases = []
[extra]
id = 11171
[taxonomies]
categories = []
tags = []
+++


```txt

Problem:    knapU
Rows:       3
Columns:    3 (3 integer, 0 binary)
Non-zeros:  9
Status:     INTEGER OPTIMAL
Objective:  knap_value = 54500 (MAXimum)

   No.   Row name        Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 knap_weight                25                          25 
     2 knap_vol                0.247                        0.25 
     3 knap_value              54500                             

   No. Column name       Activity     Lower bound   Upper bound
------ ------------    ------------- ------------- -------------
     1 take[panacea]
                    *              0             0               
     2 take[ichor]  *             15             0               
     3 take[gold]   *             11             0               

Integer feasibility conditions:

KKT.PE: max.abs.err = 1.67e-016 on row 1
        max.rel.err = 3.48e-018 on row 2
        High quality

KKT.PB: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

End of output

```

