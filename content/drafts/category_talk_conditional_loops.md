+++
title = "Category talk:Conditional loops"
description = ""
date = 2017-03-30T13:24:16Z
aliases = []
[extra]
id = 21339
[taxonomies]
categories = []
tags = []
+++

Interestingly I see only two loop variants:
#[[Loops/Do-while]]
#[[Loops/While]]
The first loop construct tests a condition after execution of a block of statements to decide if to execute the block again. The second loop construct tests a condition before and will only execute a block of statements if the test is passed.
A third loop construct could be to test a condition before and a condition after a block of statements.<br/><br/>
''pseudo code''

```pascal
while condition_before
   statements
until condition_after
```

The condition after will only be tested after the block of statements has been executed once. So I propose a new task:
#[[Loops/While Until]]
[[User:Dedalus|Dedalus]] ([[User talk:Dedalus|talk]]) 13:24, 30 March 2017 (UTC)
