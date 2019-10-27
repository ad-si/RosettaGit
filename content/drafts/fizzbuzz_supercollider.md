+++
title = "FizzBuzz/SuperCollider"
description = ""
date = 2018-03-27T16:04:25Z
aliases = []
[extra]
id = 21762
[taxonomies]
categories = []
tags = []
+++


## SuperCollider


```SuperCollider

(
var i = 0;
(1..100).do({ i = i + 1;
   case
	{(i%15==0)} {"FizzBuzz".postln}
        {(i%3==0)}  {"Fizz".postln}
        {(i%5==0)} {"Buzz".postln}
        {i.postln};
})
)
