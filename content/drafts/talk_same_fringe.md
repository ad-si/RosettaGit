+++
title = "Talk:Same Fringe"
description = ""
date = 2012-09-02T10:16:43Z
aliases = []
[extra]
id = 12229
[taxonomies]
categories = []
tags = []
+++

==Pythons binary tree representation==
Erm, I looked at the task and immediately thought that building a binary tree in Python might be the part that took the most time and might obfuscate the essence of the task. I did note that some leeway was given in representing the tree.

On looking at the Perl-6 solution I liked the definition of the B-trees a,b,c and x,y,z:
:<lang perl-6>my $a = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8;
my $b = 1 => (( 2 => 3 ) => (4 => (5 => ((6 => 7) => 8))));
my $c = (((1 => 2) => 3) => 4) => 5 => 6 => 7 => 8;
 
my $x = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8 => 9;
my $y = 0 => 2 => 3 => 4 => 5 => 6 => 7 => 8;
my $z = 1 => 2 => (4 => 3) => 5 => 6 => 7 => 8;
```


I thought this would create valid Python tuples with just a few edit commands in vim and would result in (nested) tuples.
Picturing the resultant tuples I then thought of what happens when you traverse a 'true' binary tree, you get the values out in a particular order and that order can be replicated by traversing the Python tuples in a certain way.

So the tuple definitions:
:
```python
    a = 1, 2, 3, 4, 5, 6, 7, 8
    b = 1, (( 2, 3 ), (4, (5, ((6, 7), 8))))
    c = (((1, 2), 3), 4), 5, 6, 7, 8
 
    x = 1, 2, 3, 4, 5, 6, 7, 8, 9
    y = 0, 2, 3, 4, 5, 6, 7, 8
    z = 1, 2, (4, 3), 5, 6, 7, 8
```

And the fringe function definition for generating members of the tuples:
:
```python
def fringe(tree):
    'Yield tree members L-to-R depth first, as if stored in a binary tree'
    for node in tree:
        if type(node) != tuple:
            yield node
        else:
            for nd in fringe(node): yield nd
```

Act together to emulate the depth first L-to-R traversal of a true binary tree, generating successive leaf values, in order.

The [http://docs.python.org/library/itertools.html#itertools.izip_longest itertools.izip_longest] function pairs the successive members of two 'trees' together allowing them to be tested for equality one pair of terms at a time. The [http://docs.python.org/library/functions.html?highlight=all#all all] function short circuit evaluates and will return false as soon as the next pair of terms are unequal. The izip_longest function is set to pad a shorter 'tree' with the None value which should not be a member of either tree.
--[[User:Paddy3118|Paddy3118]] 15:46, 14 August 2012 (UTC)
:I think your representation of binary trees is fine, even if it doesn't have explicit CDR pointers.  That is, as long as the algorithm has to choose "left" or "right", it's an okay representation.  To push it a bit further, a string representation of the whole tree would not be okay if the leaves were extracted directly from the string, but probably would be okay if a stack of cursors were navigating around the string making those left/right/down/up decisions. --[[User:TimToady|TimToady]] 17:35, 14 August 2012 (UTC)

::Thanks. --[[User:Paddy3118|Paddy3118]] 22:00, 14 August 2012 (UTC)

:So you've basically made the task into the same fringe problem for a general tree, not just binary tree. You've made a transformation of binary trees into general trees, kind of in a Lisp list way, i.e. Turn a binary node into a general node (with variable number of children) where the left side is the first child, and if the right side is also a node, you include that as the "rest" of the children; until you find a right side that is a leaf, which you take as the last child. Your fringe function is essentially a fringe function for general trees, which also works for this case because your transformation preserves the ordering of the fringe. Do you want to just change the problem to be for general trees? --[[User:Spoon!|Spoon!]] 09:59, 15 August 2012 (UTC)
::You can represent a binary tree using a general tree, so that is already allowed.  But this problem, when posed in the literature, is normally about binary trees.  I see no reason to force a generalization.  --[[User:TimToady|TimToady]] 14:44, 15 August 2012 (UTC)

== Concurrency required? ==

TimToady obviously had concurrency in mind for this task, but could a solution without it fit the bill? A binary tree can be constructed with each node pointing back to its parent for example, and the tree can be traversed across calls with a single node pointer without thread/coroutine/etc.  It sort of violates "no extra work" clause since each node does need some extra information, but it also sort of doesn't because comparison doesn't require extracting the whole tree.  What's Larry's stance on this? --[[User:Ledrug|Ledrug]] 01:17, 15 August 2012 (UTC)

:Using a data structure that requires no recursion seems a bit beyond the spirit of the problem to me.  I wouldn't necessarily call it cheating, but it feels like it.  Maybe not quite as bad as having a string representation of the tree and then extracting the leaves with a global regex search, but just because you have to pick between down/right/up pointers doesn't really mean you're solving the problem.  Kinda like solving a maze by just keeping your right hand on the wall, it requires no brainpower.  So I think we should probably eschew the use of parent pointers.  Good question though.  --[[User:TimToady|TimToady]] 02:05, 15 August 2012 (UTC)

:TimToady picked up on the data structure part of the original question, but I read the question and thought maybe Ledrug was asking about the need of [[wp:Concurrency (computer science)|concurrency]] as in parallel processing? The Python solution does not process each tree in a separate process or thread, which I thought was OK? --[[User:Paddy3118|Paddy3118]] 03:51, 15 August 2012 (UTC)

:: Python <code>yield</code> is basically a coroutine, so the validity is not a problem here. --[[User:Ledrug|Ledrug]] 04:10, 15 August 2012 (UTC)

::Coroutines will certainly work, though those can be emulated with closures if you work at it, and the literature also has implementations based on tree rotations, which is mentioned in the description. --[[User:TimToady|TimToady]] 05:34, 15 August 2012 (UTC)

::: Tree rotation sounds somewhat iffy.  Isn't it essentially the same as massaging (part of) the tree into a singly linked list? --[[User:Ledrug|Ledrug]] 06:12, 15 August 2012 (UTC)

::::Well, sure, but who am I to argue with John McCarthy, who proposed that very solution?  <tt>:-)</tt>  See about 1/3 the way down [http://c2.com/cgi/wiki?SameFringeProblem this article]. --[[User:TimToady|TimToady]] 07:05, 15 August 2012 (UTC)

==Copy problem in D entry?==
I am not a D programmer but I see

```d
const t1 = n(10, n(20, n(30, n(40), n(50))));
...
const t2 = n(1, n(2, n(3, n(40), n(50))));
'''
sameFringe(t1, t2): true
```


I was just wondering about the 10, 20, 30 and 1, 2, 3? --[[User:Paddy3118|Paddy3118]] 19:49, 18 August 2012 (UTC)
:Judging from the previous Haskell code (which I have replaced), I believe the author intends the first argument of n to be the value of that node, and the following to be children. And if there are no children, then it is a leaf. And that the value of a node that is not a leaf is not considered when taking the fringe. Or something like that. They may be confused because there are different concepts of trees, some with values at nodes and some without; and the problem is not very specific about that. --[[User:Spoon!|Spoon!]] 01:28, 19 August 2012 (UTC)

==Elevate to task now?==
Any qualms about elevating this from draft status? --[[User:TimToady|TimToady]] 03:29, 2 September 2012 (UTC)
:None hear except I see a separate problem in that the RC server doesn't seem to be colourizing code at the moment so I might wait a little? --[[User:Paddy3118|Paddy3118]] 05:02, 2 September 2012 (UTC)
:: Elevated. Whatever the glitch was, it must have been temporary. --[[User:Short Circuit|Michael Mol]] 10:16, 2 September 2012 (UTC)
