+++
title = "Knapsack problem/Continuous/Mathprog"
description = ""
date = 2012-01-11T21:26:43Z
aliases = []
[extra]
id = 11174
[taxonomies]
categories = []
tags = []
+++


```txt

Problem:    knapC
Rows:       2
Columns:    9
Non-zeros:  18
Status:     OPTIMAL
Objective:  knap_value = 349.3783784 (MAXimum)

   No.   Row name   St   Activity     Lower bound   Upper bound    Marginal
------ ------------ -- ------------- ------------- ------------- -------------
     1 knap_weight  NU            15                          15       18.1081 
     2 knap_value   B        349.378                             

   No. Column name  St   Activity     Lower bound   Upper bound    Marginal
------ ------------ -- ------------- ------------- ------------- -------------
     1 take[beef]   NL             0             0           3.8      -8.63442 
     2 take[pork]   NL             0             0           5.4      -10.1451 
     3 take[ham]    NU           3.6             0           3.6       6.89189 
     4 take[greaves]
                    NU           2.4             0           2.4      0.641892 
     5 take[flitch] NL             0             0             4      -10.6081 
     6 take[brawn]  NU           2.5             0           2.5       4.29189 
     7 take[welt]   B            3.5             0           3.7 
     8 take[salami] NU             3             0             3       13.5586 
     9 take[sausage]
                    NL             0             0           5.9      -1.49794 

Karush-Kuhn-Tucker optimality conditions:

KKT.PE: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

KKT.PB: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

KKT.DE: max.abs.err = 0.00e+000 on column 0
        max.rel.err = 0.00e+000 on column 0
        High quality

KKT.DB: max.abs.err = 0.00e+000 on row 0
        max.rel.err = 0.00e+000 on row 0
        High quality

End of output

```

