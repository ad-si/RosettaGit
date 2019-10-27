+++
title = "Knapsack problem/Continuous/J"
description = ""
date = 2015-07-07T03:15:22Z
aliases = []
[extra]
id = 19366
[taxonomies]
categories = []
tags = []
+++

Here's a look at the intermediate values developed in this solution, since they may shed some light on the approach used here.

First, we use J's parser (which considers sequences of numbers separated by spaces to be a single token) to split the raw text into two variables:


```J
   names
beef   
pork   
ham    
greaves
flitch 
brawn  
welt   
salami 
sausage
   numbers
3.8  36
5.4  43
3.6  90
2.4  45
4.0  30
2.5  56
3.7  67
3.0  95
5.9  98
```


Then we convert the text in <code>numbers</code> to real numbers, and put each column in a different variable.


```J
   weights
3.8 5.4 3.6 2.4 4 2.5 3.7 3 5.9
   prices
36 43 90 45 30 56 67 95 98
```


Next, we compute price-to-weight ratio and find the indices which would sort this in decreasing order.


```J
   prices%weights
9.47368 7.96296 25 18.75 7.5 22.4 18.1081 31.6667 16.6102
   order
7 2 5 3 6 8 0 1 4
```


Next, we use this order to pick out the weights


```J
   order{weights
3 3.6 2.5 2.4 3.7 5.9 3.8 5.4 4
```


and find their running sum


```J
   +/\ order{weights
3 6.6 9.1 11.5 15.2 21.1 24.9 30.3 34.3
```


And we clip that running sum at 15

<lang>   15 <. +/\ order{weights
3 6.6 9.1 11.5 15 15 15 15 15
```


Except let's decorate that algorithm with punctuation, to make it a single verb, in preparation to the next step:

<lang>   15&<.&(+/\) order{weights
3 6.6 9.1 11.5 15 15 15 15 15
```


Here we use J's [[j:Vocabulary/ampdot|under]] to perform the inverse of running sum on that result:


```J
   15&<.&.(+/\) order{weights
3 3.6 2.5 2.4 3.5 0 0 0 0
```


This under mechanism is a feature of the language, and is quite often super convenient, but can be a bit difficult to explain to users of languages which have nothing comparable. 

(For the record, though, the inverse of running sum is the first value followed by the pairwise differences of adjacent values in the list - J has a library of inverse mechanisms which it brings to bear here, and allows the programmer to specify their own inverse for user defined verbs - which can be abused, but should not be abused since there's no point to such abuse. Still, J's reference works take pains to call this "obverse" instead of "inverse" to allow for cases where true inverse is not possible.)

... and that's the value we stuck in <code>take</code>


```J
   take
3 3.6 2.5 2.4 3.5 0 0 0 0
```


Note that since all weights are positive, we can find the non-empty weights by checking their sign (which we will use in a moment):


```J
   *take
1 1 1 1 1 0 0 0 0
```


Now we form the result into a column:


```J

   ,.take
  3
3.6
2.5
2.4
3.5
  0
  0
  0
  0
```


Format as text and prepend a space:


```J
   ' ',.":,.take
   3
 3.6
 2.5
 2.4
 3.5
   0
   0
   0
   0
```


Then prepend the names (which we must sort using the order we came up with earlier, so the names match up with the numbers):


```J
   (order{names),.' ',.":,.take
salami    3
ham     3.6
brawn   2.5
greaves 2.4
welt    3.5
sausage   0
beef      0
pork      0
flitch    0
```


And, finally, we strip off the rows which are zeros (by taking one copy of each of the rows where we are taking something and zero copies of the rows where we are not):


```J
   (*take)#(order{names),.' ',.":,.take
salami    3
ham     3.6
brawn   2.5
greaves 2.4
welt    3.5
```


And we are done!

Well, almost... to find the total price, we first sort the weights we are taking of each item in ascending order of the ordering indices:


```J
   take/:order
0 0 3.6 2.4 0 2.5 3.5 3 0
```


This puts them in the same order as the original weights. So now we can divide by those weights to find how much of each we are taking:


```J
   (take/:order) % weights
0 0 1 1 0 1 0.945946 1 0
```


Mostly all or nothing, except for the item we sliced up.

And now we can multiply our prices by that to find the price for each item we are taking:


```J
   prices*(take/:order) % weights
0 0 90 45 0 56 63.3784 95 0
```


And we just have to add those up to get the total price:


```J
   +/prices*(take/:order) % weights
349.378
```


And all of this relates to how one normally codes in J - you take small steps, checking your work at each step to make sure it makes sense.

Similarly, to read J, the easiest way is often to ignore the code and examine representative data. And once you understand what's happening with the data you can go look up any operation you weren't understanding.

That said, note that there are some frequent idioms (such as the techniques used here with sorting) which have broad utility. (Current [[SQL]] standards explicitly avoid this utility due to reasoning which if - if it had been followed consistently - would prohibit use of nulls and would have an implicit <code>DISTINCT</code> on all selects.)
