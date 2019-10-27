+++
title = "Talk:Pangram checker"
description = ""
date = 2019-08-20T15:22:59Z
aliases = []
[extra]
id = 10204
[taxonomies]
categories = []
tags = []
+++

== solution for ActionScript? ==
How about this for the same algorithm as the current ActionScript solution, but coded in a saner manner?


```ActionScript
function pangram(k:string):Boolean {
  var lowerK:String = k.toLowerCase();
  var has:Object = {}
  
  for (var i:Number=0; i<=k.length-1; i++) {
    has[lowerK.charAt(i)] = true;
  }

  var result:Boolean = true;

  for (var ch:String='a'; ch <= 'z'; ch=String.fromCharCode(ch.charCodeAt(0)+1)) {
      result = result && has[ch]
  }

  return result || false;
}
```

-- [[User:Markjreed|Markjreed]] 20:05, 2 August 2011 (UTC)
:Since the current implementation admits being barbaric, why not. --[[User:Ledrug|Ledrug]] 20:16, 2 August 2011 (UTC)

==Undiscussed deletion (JavaScript) June 5 2016==

:I notice that a JavaScript example was deleted without discussion on 5 June 2016. 
:Addition is generally preferable to deletion, particularly where approaches diverge, but more importantly, proposed deletions do need to be motivated and explained here on the discussion page. 
:Unless there are objections, I propose to restore the alternative version, so that readers are allowed see both approaches [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:17, 4 November 2016 (UTC)
