+++
title = "Talk:Recursive descent parser generator"
description = ""
date = 2014-04-02T16:49:54Z
aliases = []
[extra]
id = 17473
[taxonomies]
categories = []
tags = []
+++

== Input specification ==
The main thing that needs to be fixed before adding any examples is the input specification. I want to make it detailed enough to be at least slightly usable (so more than just a recognizer) but I'm having trouble coming up with a clear way of expressing both the rule and the extra code generated when the rule is applied, especially taking in to account all the different languages that you might want to output with or to. Here's one attempt:


```txt

header code to output

!! start -> var expr
more code

!! expr -> '+' expr
...

```


Is there any way to simplify this? (I can't remember, can you do everything that needs to be done with a header and just post-traversal output?)

The example I want to use is to take an arithmetic expression and turn it in to a series of single operations in the right order. So:

```txt

(one + two) * three + four * five

```

Becomes something like:

```txt

_a = one + two
_b = _a * three
_c = four * five
_d = _b + _c

```


Oh yeah, if the task specification says to output the parser in your language of choice, the example will have to be rewritten for each language too, unless there's some way around that.

[[User:MagiMaster|MagiMaster]] ([[User talk:MagiMaster|talk]]) 06:10, 2 April 2014 (UTC)

:I think I'd give some input samples and describe the characteristics of the output. Then I'd provide a sample implementation. If people have questions or whatever that will show up soon enough. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:53, 2 April 2014 (UTC)


::''I want to make it detailed enough to be at least slightly usable''
:The above can work ''against'' making a good RC task sometimes. You really don't want to make things too long. You also want to make a task in which the examples generated are comparable. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:12, 2 April 2014 (UTC)
::I think if it can handle the above example, it'd be enough. [[User:MagiMaster|MagiMaster]] ([[User talk:MagiMaster|talk]]) 16:49, 2 April 2014 (UTC)
