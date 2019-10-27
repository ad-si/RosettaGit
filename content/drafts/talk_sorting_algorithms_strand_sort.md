+++
title = "Talk:Sorting algorithms/Strand sort"
description = ""
date = 2012-08-08T11:10:58Z
aliases = []
[extra]
id = 12162
[taxonomies]
categories = []
tags = []
+++

== D Entry ==
The D entry code (that maybe was written by Fwend) contains one or more bugs, it prints "-1 -1" even if the input contains only one -1.
: The linearRemove does sometimes not remove the first item, although "find" and "take" return the correct result. I've posted a workaround but I haven't been able to trace the actual problem. [[User:Fwend|Fwend]] 16:18, 3 August 2012 (UTC)
:: I suggest a fuzzy testing to see if your code is correct (this means generating many different random inputs and verifying the output is fully correct). And it's not hard to find bugs in Phobos. Finding and fixing bugs in DMD/Phobos is important. So if you find that the cause is in Phobos, you will avoid some troubles to future D users. Try DList!int([-5, -5, 8]). In programming "don't understand why it doesn't work, but this seems to fix it" is often not enough to remove bugs. The simple fuzzy testing code I've used: 
```d
void main() {
    import std.random, std.conv;

    foreach (_; 0 .. 1000) {
        auto data = new int[uniform(0, 5)];
        foreach (ref x; data)
            x = uniform(-5, 10);
        auto lst = DList!int(data);
        int[] result = strandSort(lst).array();
        int[] sortedData = data.dup.sort().release();
        assert(result == sortedData, text("\n", data, "\n", result, "\n", sortedData));
    }
}
```
[[User:Bearophile|bearophile]]
::: Well, I'll give it my best try. In my opinion the problem lies with the removeFront / removeBack functions. They don't set the _first._prev / _last._next pointers to null. This affects the (linear)Remove because it relies on "before" and "after" checks. The following (adjusted) code seems to work. What do you think? 
```d
    void removeFront()
    {
        enforce(_first);
        _first = _first._next;
        if (_first is null)
        {
            _last = null;
        } 
        else 
	{
            _first._prev = null;
        }
    }

    void removeBack()
    {
        enforce(_last);
        _last = _last._prev;
        if (_last is null)
        {
            _first = null;
        } 
	else 
	{
            _last._next = null;
        }
    }

```

:::[[User:Fwend|Fwend]] 00:44, 4 August 2012 (UTC)
:::: If you think you have found a Phobos bug, I suggest to add an entry in the D Bugzilla.[[User:Bearophile|bearophile]]
::::: Are you sure it's a bug? The original code was modifying <code>list</code> while doing a <code>foreach</code> on it, how was the program supposed to behave regarding the validity of the iterator?  Also, <code>linearRemove(list[].find())</code> really doesn't make sense when you are sitting right at the location you want to remove. --[[User:Ledrug|Ledrug]] 04:24, 4 August 2012 (UTC)
:::::: Your solution is fine. linearRemove is supposed to be stable, (there's also stableLinearRemove, but it's nothing but an alias for linearRemove) so I think I should have been able to use it. <strike>Also, searching for the item or copying the list is just as expensive so it makes little difference.</strike>.[[User:Fwend|Fwend]] 09:59, 4 August 2012 (UTC)
::::::: I'm curious about D's DList stuff: is <code>a.insertBack(b.first); b.removeFront();</code> the only way to unlink first element of b and append it to a?  It has the appearance of dealloc/alloc'ing a <code>Node</code> at each step, which would seem to be a waste, but I don't know D, so I could simply have the wrong impression here. --[[User:Ledrug|Ledrug]] 07:06, 8 August 2012 (UTC)
:::::::: The Nodes are declared private and never exposed. Functions like "front" return the payload of the Node, never the Node itself. So "item" is an int in our case. [[User:Fwend|Fwend]] 11:10, 8 August 2012 (UTC)
