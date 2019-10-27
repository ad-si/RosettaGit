+++
title = "Talk:Sokoban"
description = ""
date = 2012-05-15T01:14:12Z
aliases = []
[extra]
id = 9804
[taxonomies]
categories = []
tags = []
+++

Added draft status as task doesn't seem that easy. No solution algorithm is even hinted at on the wp page. --[[User:Paddy3118|Paddy3118]] 05:41, 28 May 2011 (UTC)

:There is no particular solution algorithm besides mostly brute-forcing it. It's a PSPACE-complete problem. There are various ways that make the brute force approach less painful though, like working the problem backwards. (Also, what does draft-status have to do with how easy the problem is? I'm asking since I'm somewhat new here.) [[User:MagiMaster|MagiMaster]] 09:36, 28 May 2011 (UTC)

:: Hi MagiMaster, we usually keep draft status until their are around four good examples and no niggling problems outstanding in the talk page; but that is a guide. --[[User:Paddy3118|Paddy3118]] 12:52, 28 May 2011 (UTC)

::: Oh. K. The description of draft/not-draft made it sound like it was more about the way the problem was phrased (that is, the problem might be a little unclear and might invalidate any examples). I think the task here is clear enough, in that no working example should get invalidated, but it might take a while to get 4 examples. [[User:MagiMaster|MagiMaster]] 19:09, 28 May 2011 (UTC)

== Promote? ==

There's 4 examples. Any reasons left to keep it draft? [[User:MagiMaster|MagiMaster]] 14:20, 20 July 2011 (UTC)

==C++ Version==

Do you mind if I remove the first C++ version? It's kind of redundant. (Probably the first C version too is kind of redundant). --[[User:Bearophile]]
: It's probably better to leave both C++ versions.  Boost may become standard one day, but not (all of it) yet, so it may be helpful to have both versions around.  As to C, do whatever you see fit, since the second version is just a line by line translation from the first one by somebody. (Also, after your comment, sign it with say, <code><nowiki>--~~~~</nowiki></code> so people know who's talking. --[[User:Ledrug|Ledrug]] 19:36, 4 May 2012 (UTC)

:: OK. Unordered sets will be part of the C++ standard, so the dependency from Boost will be minimal, or even zero. I'll remove the first C version then, usually I prefer C99 (but you usually prefer C89). How do you change the registration name here? --[[User:Bearophile|Bearophile]] 20:14, 4 May 2012 (UTC)

::: I don't know how or if you can change your login name.  [[User:Short Circuit]] is the man to ask; you can also try the IRC. --[[User:Ledrug|Ledrug]] 21:29, 4 May 2012 (UTC)
::: About C89/99: I tend to follow the older standard here, since C89 is supposed to be more portable across compilers.  Whether the C89 ''I'' write is portable is a completely separate issue. --[[User:Ledrug|Ledrug]] 21:40, 4 May 2012 (UTC)

==The Long C Version==

1) It's often bad to put code with side effects inside assert(). Lines of code like this are a bug, because with <code>#define NDEBUG</code> the allocation doesn't happen:


```c
assert( board = calloc(w * h, sizeof(uint8_t)) );
```


2) In some declarations "s" could be declared as pointing to const: 
```c
inline int deadlocked(state_t *s)
void unqueue_move(state_t *s)
```


: 1) You originally had <code>board = calloc(...); assert(board);</code>. This was done probably so it still runs with <code>cc -DNDEBUG</code>, but that's the wrong thing to do.  <code>calloc</code> will return 0 if and only if there's no memory left, so you are asserting that allocation succeeded, which is a run time check as opposed to verifying contracts during debugging.  If the runtime check is meaningful and should be performed, then <code>NDEBUG</code> shouldn't be set at all; if it ''may'' be set, then the valid check should have been <code>if (!board) {...}</code> instead of <code>assert</code>.  I put the <code>assert</code> there to make <code>assert</code>ive people happy, personally I don't want it there at all.  A failed alloc returns 0, which segfaults when you deref it, so you don't have the risk of putting data behind wrong pointers.

:2) For a library routine, putting <code>const</code> in front of a var gives some hint about the interface; init code may put <code>const</code> in read-only memory.  Outside those two, <code>const</code> does absolutely nothing for the code.  For routines used only locally, I don't really want to bother.  <s>Plus, <code>unqueue_move(state_t *)</code> can decl a const pointer, but not a pointer to const: it modifies data inside the struct.</s>(nvm: I was think of <code>queue_move()</code> --[[User:Ledrug|Ledrug]] 20:29, 14 May 2012 (UTC)

:: Right, using assert() as allocation guards is a bad idea, maybe it's better to remove those asserts. You can't assume no one will compile your code with NDEBUG defined. Putting non-pure code inside assert() is often a bug waiting to happen.
:: Regarding const, it's also a contract between the programmer and the compiler, it says that something hopefully will not change (in D language this is enforced much better than C), this makes the code simpler to understand too. Every variable that doesn't need to change must be const/immutable, if practically possible: http://blog.knatten.org/2011/11/11/disempower-every-variable/ --[[User:Bearophile|Bearophile]] 00:06, 15 May 2012 (UTC)

::: I don't know, I'm not one into following good practices (not that I suggest you do the same).  <code>const</code> is at most a moral contract, it can't serve as a contract that garantees logical invariance: you can always find a way to cast a const pointer into something else.  For library code that users don't normally see the source, I do use <code>const</code> as a hint, but for local code, I don't really care.  Same goes for things like <code>(void) printf(...)</code>: I don't think it does real harm, but it's pointless.  It's probably just me, though. --[[User:Ledrug|Ledrug]] 01:14, 15 May 2012 (UTC)
