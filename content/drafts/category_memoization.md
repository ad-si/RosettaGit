+++
title = "Category:Memoization"
description = ""
date = 2016-02-11T05:55:26Z
aliases = []
[extra]
id = 9251
[taxonomies]
categories = []
tags = []
+++

Memoization is a method used to reduce function calls in recursive functions or other functions that are called very frequently. The basic idea behind memoizing is to store results for new sets of inputs (typically in a key-value style) so that the function will not have to re-compute those results later when the same inputs are used (either in another direct function call or a subsequent recursive call).

Functions which can be memoized are ones that give the same answer for a set of inputs each time those inputs are used. [[Fibonacci sequence|Fibonacci number functions]] are often memoized to reduce their call trees and calculation times over time. The basic operation of a memoized function would look something like this:

```txt
function a with inputs
   if inputs have been seen before
      return a stored answer from when they were seen
   else
      compute the answer for inputs
      store that (inputs, answer) pair
      return that answer
end function
```

Some programs may negate the condition in the "if" and swap the operations. 

The overall benefit is that a function frequently called with the same set of inputs can save time by remembering the answer after computing it once&mdash;sacrificing memory for computation time. In systems where memory (or storage depending on the implementation of storing old results) comes at a premium, memoization is not a good option. As long as memory is available and input sets are used repeatedly, memoization can save lots of computation time.
[[Category:Classic CS problems and programs]] [[Category:Encyclopedia]]
