+++
title = "Talk:Find limit of recursion"
description = ""
date = 2015-08-13T18:35:51Z
aliases = []
[extra]
id = 7085
[taxonomies]
categories = []
tags = []
+++

== It depends ==
I suspect that the recursion limit depends often on the compiler (for compiled languages), operating system (stack assigned to a process could be tuned, I suppose, and may change from O.S. to O.S.), implementation of an "interpreter" (when more than one exists) and/or on how the recursive function/method/whatever is written. E.g. C version passes an argument to keep track of the depth, Sather uses a class attribute ... Different ways change the limit (it's not the case of the C version in my tests since clearly gcc optimizes using a register to pass the only argument) ...

Current GNU Sather compiler uses C as intermediate language; so the more limited recursion depth could depends on the fact that sather's "functions" translated in C have to keep something like a "context", being part of a class (more real arguments), or/and simply they use more stack because of needed variables that appear in the translated code... in fact it looks this way:


```c
void MAIN_recurse(MAIN self) {
 INT L1;
 INT L21_;
 OUT L3;
 dSTR L4;
 OB L5;
 OUT L6;
 extern STR name6;
 L1 = ATTR(self,r);
 L21_=INTPLUS(L1,1); 
 SATTR(self,r,L21_);
 L3 = OUT_createrOUT(((OUT) NULL));
 L5=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,sizeof(struct INT_boxed_struct));
 ((OB)L5)->header.tag=INT_tag;
 L4 = (dSTR)((INT_boxed) L5);
 ((INT_boxed) L4)->immutable_part = ATTR(self,r);
 L6 = OUT_pl930378279(L3, L4);
 OUT_plus_STR(L6, ((STR) &name6));
 MAIN_recurse(self);
}
```


The point is... depending the recursion max depth on too many parameters, shouldn't we specify the environment (O.S., compiler, interpreter) running the examples? Otherwise a novice user could think that the shown limit is hardcoded somewhere in the nature of the language (that could be, maybe, for some languages, and should be noted!) &mdash;[[User:ShinTakezou|ShinTakezou]] 14:46, 20 April 2010 (UTC)

:Note that "stack depth" also depends on "what goes on the stack", and in some languages seemingly inconsequential differences can have order of magnitude effects on how many stack frames can be nested without problem.  So the numbers generated here are not very meaningful, and even the implementations can only be hints.  --[[User:Rdm|Rdm]] 20:09, 9 February 2011 (UTC)

== The name of the game ==

Noticed now... the name is Find Limit of Recursion, while I've read proposal for naming like "Find limit of recursion"... I suppose this page should be moved (I think slowly and act too fastly, thus let someone else do it properly)... &mdash;[[User:ShinTakezou|ShinTakezou]] 15:11, 20 April 2010 (UTC)
:Done --[[User:Mwn3d|Mwn3d]] 15:26, 20 April 2010 (UTC)

== Rules ==
What if language normally compiles tail-recursion to not use stack at all? Do we need to force non tail recursivity or need to say that there is no limit?

: If your code does not find the limit, that would be a bug in your code.  But tail recursion issues are certainly worth documenting.  A related [but different] issue has to do with stack frame sizes.  --[[User:Rdm|Rdm]] 01:39, 24 May 2010 (UTC)


== Hmm ... I wonder ==
Regarding the COBOL example, I haven't found out yet how to 'catch' the error during a run. Is it possible?

: First, if you could not trap the error you could satisfy the task i think by simply logging how far you have gotten each time you recurse.  But I expect cobol can do better than that?  (See http://www.emunix.emich.edu/info/cobol/books/prxept.htm 5.2.1 and 5.4.3)

==Algol68's tone==
I've made one edit to the Algol68 entry but the overall tone of the author is wrong. The task - All the RC Tasks apply to multiple languages for the purposes of language ''comparison''. One should create language entries preferably with that in mind and not slag off task descriptions out of hand - especially when there are so many examples from other languages that manage to complete the task with a relatively minor set of issues on this discussion page. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:09, 10 August 2015 (UTC)

Strange? this entry: [[Arbitrary-precision_integers_(included)#ALGOL_68]]has a comment mentioning that Algol68's stack needs alteration for another task. It seems that recursion limits may be reached in accomplishing other RC tasks and Algol68 includes ways to modify it. Why not just plainly state this? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:31, 10 August 2015 (UTC)

Fair comment.  I've made another change which reduces expression of the strength of my opinions further.   The comment immediately above refers to the need to expand the size of the expression stack.  As numerical precision increases expressions need ever more space for sub-expression evaluation.   Recursion, on the other hand, uses the system stack and I know of no way to expand that within the confines of Genie itself.  I think it very important to distinguish between what a language, such as ALGOL 68, can provide and what a particular implementation of a language, such as Algol 68 Genie, provides.  That's why I try hard to indicate which particular implementation and its environment is assumed when that is important. 
----

:Sorted. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:53, 10 August 2015 (UTC)

Given that changes had to made to the Algol68 entry here, may I respectfully request that a similar change be made to [[Longest_string_challenge#D]]?  Sauce for the gander and all that.   The commentary on the C entry is also sailing close to the wind, IMO, but the talk page helps remedy the situation.

: Sorted, but you can also ask authors to put such comments on the talk page where, you never know, such comments may prompt a change or a clarification. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:35, 13 August 2015 (UTC)
