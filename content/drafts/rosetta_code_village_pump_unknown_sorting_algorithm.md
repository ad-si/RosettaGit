+++
title = "Rosetta Code:Village Pump/Unknown sorting algorithm"
description = ""
date = 2010-11-10T02:22:18Z
aliases = []
[extra]
id = 4079
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Unknown sorting algorithm
|summary=Question on what sorting algorithm this is.
}}Someone know what sorting algorithm is this?
I just made it, and i don't know his name (Bogosort, Bubble Sort and etc.)

Take a look at my code:


```python
aunsorted = [6, 2, 7, 8, 3, 1, 10, 5, 4, 9]
asorted = []

amin = aunsorted[0]
aminindex = 0

while True:
    for i in range(len(aunsorted)):
        if aunsorted[i] < amin:
            amin = aunsorted[i]
            aminindex = i

    del aunsorted[aminindex]
    asorted = asorted + [amin]

    if len(aunsorted) == 0: break

    amin = aunsorted[0]
    aminindex = 0


print asorted
```



```c#
using System;
using System.Collections.Generic;

public class Program {
    static void Main() {
        List<int> unsorted = new List<int>(new int[] { 6, 2, 7, 8, 3, 1, 10, 5, 4, 9 });
        List<int> sorted = new List<int>();

        int min = unsorted[0];
        int minindex = 0;

        while (true) {
            for (int i = 0; i < unsorted.Count; i++) {
                if (unsorted[i] < min) {
                    min = unsorted[i];
                    minindex = i;
                }
            }

            unsorted.RemoveAt(minindex);
            sorted.Add(min);

            if (unsorted.Count == 0) break;

            min = unsorted[0];
            minindex = 0;
        }

        foreach (int i in sorted) {
            Console.WriteLine(i);
        }
    }
}
```


: This is effectively a [[selection sort]] (find minimum, put in the right place). --[[User:IanOsgood|IanOsgood]] 02:37, 22 April 2009 (UTC)
: Thanks for a fast answer ;) --[[User:Guga360|Guga360]] 02:50, 22 April 2009 (UTC)
