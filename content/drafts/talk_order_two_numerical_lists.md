+++
title = "Talk:Order two numerical lists"
description = ""
date = 2019-07-09T19:47:46Z
aliases = []
[extra]
id = 10969
[taxonomies]
categories = []
tags = []
+++

== clarify what should happen for equal lists ==
The task description should clarify what should happen for two equal lists.

Right now the description seems inconsistent. It says "return true if the first list should be ordered before the second", but should a list be ordered before itself? It seems that it should not.
: if both lists are the same then, they still have to go in some order. so there is no inconsistency. you can not ''not'' order them. which order they go then depends on the comparison though, and that affects whether the sort is stable or not. as it is defined the task does not expect the solution to produce a stable sort. such a requirement could be added, but then the existing solutions need to be reviewed. since both stable and unstable sort have legitimate uses i don't want to make unstable sort wrong. it could be added as extra credit though--[[User:EMBee|eMBee]] 02:30, 30 November 2011 (UTC)
But reading the algorithm description: "... and so on, until one of the list has no more elements. If the first list runs out of elements the result is true." It seems that it should return true for two equal lists, since both lists would run out of elements at the same time.
:if the first list is tested first, and the second list is not tested, the effect is that if both are equal, the result will be true.--[[User:EMBee|eMBee]] 02:30, 30 November 2011 (UTC)
::"ordered before" is the familar concept of "less than." For programming anyway, less than always means strictly less than and never less than or maybe equal to.  A fix would be to change "If the first list runs out of elements the result is true. false otherwise" to "If the first list runs out of elements ''while there are still elements left in the second'' the result is true. false otherwise."  Then you have strictly less than semantics...which could be used to write a stable sort algorithm. &mdash;[[User:Sonia|Sonia]] 04:09, 30 November 2011 (UTC)
:: I would be in favor of clarifying the required behavior when lists are equal, removing all talk of sorting from the task description, and requiring output from three test cases:  one where the first list should be ordered before, one where the lists are equal, and one where the second list should be ordered before. &mdash;[[User:Sonia|Sonia]] 04:29, 30 November 2011 (UTC)
::: Have a look at the test cases below. You may find you need more that 3 cases.  --[[User:Dgamey|Dgamey]] 04:49, 30 November 2011 (UTC)
:::: Those are good.  You could throw in empty lists as well. &mdash;[[User:Sonia|Sonia]] 05:13, 30 November 2011 (UTC)

Whatever is decided, some of the solutions will be incorrect and will need to be changed. --[[User:Spoon!|Spoon!]] 23:55, 29 November 2011 (UTC)
: This probably should be marked draft until decided.  So I changed it.  --[[User:Dgamey|Dgamey]] 03:21, 30 November 2011 (UTC)
:: yes, thank you!--[[User:EMBee|eMBee]] 05:57, 30 November 2011 (UTC)
: Adding in extra conditions like stable sort when the task is NOT marked draft is rather bad form --[[User:Dgamey|Dgamey]] 03:21, 30 November 2011 (UTC)
:: that is why i marked it extra credit, so that existing solutions would not be wrong.--[[User:EMBee|eMBee]] 05:57, 30 November 2011 (UTC)

==Test Cases==
I found the description a bit confusing at first.  I used the following test cases where demo_list_llt was a wrapper to support my interpretation. This is based on ordering being to choose which of two lists comes first (not sorting). --[[User:Dgamey|Dgamey]] 03:24, 30 November 2011 (UTC)

```txt
   write()
   demo_list_llt([-1],[0])                         # << ok
   demo_list_llt([0],[0])                          # == fail 
   demo_list_llt([0],[-1])                         # >> fail
   write()
   demo_list_llt([0],[0,-1])                       # << ok   
   demo_list_llt([0],[0,0])                        # << ok     
   demo_list_llt([0],[0,1])                        # << ok 
   demo_list_llt([0,-1],[0])                       # >> fail 
   demo_list_llt([0,0],[0])                        # >> fail
   demo_list_llt([0,0],[1])                        # << ok
```

With the following output:
```txt
[ 1 2 1 3 2 ] << [ 1 2 0 4 4 0 0 0 ] - FAILS

[ -1 ] << [ 0 ]
[ 0 ] << [ 0 ] - FAILS
[ 0 ] << [ -1 ] - FAILS

[ 0 ] << [ 0 -1 ]
[ 0 ] << [ 0 0 ]
[ 0 ] << [ 0 1 ]
[ 0 -1 ] << [ 0 ] - FAILS
[ 0 0 ] << [ 0 ] - FAILS
[ 0 0 ] << [ 1 ]
```


The wrapper and support routine in Icon was 
```txt

procedure demo_list_llt(L1,L2)
   write(list2string(L1)," << ",list2string(L2),if list_llt(L1,L2) then "" else " - FAILS")
end   

procedure list2string(L1)
every (s := "[") ||:= " " || (!L1|"]")
return s
end
```


==Stable Sort??==
Where did this requirement come from?  There was nothing about sorting only comparison.  Neither list is touched.  There is no merge or no sort.  Did I miss something? --[[User:Dgamey|Dgamey]] 03:24, 30 November 2011 (UTC)
: Rereading this the description as it stands could mean either sort or determine with list is less.  If it's a sort, should not the task name be Sort two numerical lists?  --[[User:Dgamey|Dgamey]] 03:46, 30 November 2011 (UTC)
:: a comparison function is generally used as an operator for a sort function. the result of the comparison function determines how the sort comes out.--[[User:EMBee|eMBee]] 05:50, 30 November 2011 (UTC)
:: Since the task doesn't seem to have anything to do with sorting, I removed the mention of stable sort for now.  I guess the intention was that if the cmp function is used for a comparison based sorting, it should produce a stable sort result.--[[User:Ledrug|Ledrug]] 04:33, 30 November 2011 (UTC)
::: yes. --[[User:EMBee|eMBee]] 05:50, 30 November 2011 (UTC)
::  If so, it's bunk: sort stability depends more on the sort method than the comparator.--[[User:Ledrug|Ledrug]] 04:33, 30 November 2011 (UTC)
::: true, but if both elements are the same then it makes a difference if true or false is returned. how do you account for that difference? i think there are two aspects to stable, one is: when you rerun the sort it will always produce the same result. this stability depends on the algorithm. the other is, that if two elements are equal, their order will not be changed. that stability depends on the comparator. and according to [[wp:Sorting_algorithm#Stability|wikipedia]]: ''Stable sorting algorithms maintain the relative order of records with equal keys.'' the equality of keys is determined by the comparator.--[[User:EMBee|eMBee]] 05:50, 30 November 2011 (UTC)
:::: Still bunk.  Regardless if elements are the same or not, sort stability also depends on how the comparator is called.  <code>if (a[i] < a[j]) swap(a[i], a[j])</code> and <code>unless (a[i] > a[j]) swap(a[i], a[j])</code> are unlikely to be both stable in bubble sort.  The comparison routine has '''no''' information regarding "the relative order of records", only the sort routine does, so terming a comparator "stable sort" is senseless. --[[User:Ledrug|Ledrug]] 07:20, 30 November 2011 (UTC)
::::: <code>unless ></code> is the same as <code><=</code>, so that means, yes, the way the comparator is called affects the stability, which means the choice of the comparator depends on knowing how the comparator is called. but regardless of the comparator chosen, it will always be called the same way, so the stability still depends on the comparator. so you are right that without the sort function the comparator by itself can not be categorized as stable. but we can test the sort function and then determine if the combination produces a stable sort. however i think it is probably better to leave that out of the task because different languages have different ways to do the sort. this may be worth a separate topic.--[[User:EMBee|eMBee]] 08:21, 30 November 2011 (UTC) 
:::::: You can use a comparison routine in a bubble sort and claim it's "stable", then use it in a quicksort and find that it's not--you are still missing the point: sort stability is not the property of the comparator ''at all''.  Seeing that the matter is probably settled anyway, I'm just point that out as a final clarification. --[[User:Ledrug|Ledrug]] 08:42, 30 November 2011 (UTC)
:::::::i am not disagreeing with that, but the implied claim that a stable sort routine is guraranteed to be stable regardless of the comparator used. to use your words: ''you can use a comparison routine in a bubble sort and claim it's "stable", and then ''use a different comparison routine'' and find that it's not.'' the stability depends on a combination of both. or in other words, the stability depends on the sort routine knowing when two elements are equal, which it can not know if it only has one boolean comparison routine.--[[User:EMBee|eMBee]] 09:14, 30 November 2011 (UTC)
:::::::: Well, a sane comparison routine can ''always'' know if stuff are equal: if you define operator "less than", and both <math>a < b</math> and <math>b < a</math> are false, they are equal.  If you define operator "less or equal", and both <math>a \leq b</math> and <math>b \leq a</math> are true, they are equal.--[[User:Ledrug|Ledrug]] 19:00, 30 November 2011 (UTC)
::::::::: ah, yes, indeed. i didn't consider testing both <code>a,b</code> and <code>b,a</code>.--[[User:EMBee|eMBee]] 15:06, 2 December 2011 (UTC)
::::::::  But note the requirement of being sane: if you define the "less than" in such a way that there might be a case where both <math>a < b</math> and <math>b < a</math> are true, then the whole thing is quite screwed (such as before you changed the task definition when it said "if a runs out, a < b" while ignored the case where b might be out at the same time.) --[[User:Ledrug|Ledrug]] 19:00, 30 November 2011 (UTC)
:: Another thing, it might be better to omit some detailed requirement about the cmp result.  To be practical, the cmp routine should be able to decide on the ording.  For example, it's likely more useful for a comparison function to be tri-state instead of true/false;--[[User:Ledrug|Ledrug]] 04:33, 30 November 2011 (UTC)
::: tri-state does not work as input to a sort function unless the sort function is written for that. most sort function i i'd expect to want a boolean comparator.--[[User:EMBee|eMBee]] 05:50, 30 November 2011 (UTC)
:: it's not necessarily desirable to sort shorter lists before longer ones (one may prefer padding by zero, i.e. (1, 2, -1) < (1, 2) < (1, 2, 1), rather than padding by -inf, i.e. (1, 2) < (1, 2, -1) < (1, 2, 1)). --[[User:Ledrug|Ledrug]] 04:33, 30 November 2011 (UTC)
::: (misread that before) interesting point. i didn't consider negative values. how would you write that into the task? --[[User:EMBee|eMBee]] 05:54, 30 November 2011 (UTC)

==Lexicographic order==
Isn't lexicographic order the one where the integer 100 comes before the integer 11? That is, I thought that it was a character set based comparison rather than a numeric comparison. [[User:Stormneedle|Stormneedle]] 04:26, 30 November 2011 (UTC)
: No.  "Lexicographic" only refers to the list as whole, not when comparing its individual elements.  The elements are compared by whatever is natural, which probably means numerical comparison function (100 > 11, 11 > 2). --[[User:Ledrug|Ledrug]] 04:33, 30 November 2011 (UTC)
:: Well that wasn't clear.  So doesn't that imply no padding?  --[[User:Dgamey|Dgamey]] 04:47, 30 November 2011 (UTC)
::: Hmm?
::: Lexicographic comparison of two lists meaning comparing corresponding elements from either list, ''probably'' from left to right, and the ordering of the first unequal pair decides the ordering of the two lists.  The elements need to be comparable and have good ordering.  Say the lists are strings "ax" and "ay", the most natural ordering is "ax" < "ay".  When comparing "ax" and "aY", depends on what comparison function you use for letters, it could be "ax" < "aY" if using case insensitive cmp, or "ax" > "aY" if comparing ASCII code case sensitively.  Comparison function of strings is lexicographic, that of the letters is not: it can be whatever that has a good ordering.  Replace "letters" with "numbers", "comparing letters" with "comparing numbers", and you lexicographically compare list of numerical values.
::: When two lists are of different lengths, "comparing corresponding elements" needs clarification when one list runs out.  One way is, conceptually, you can pad the shorter list with some default value so they are equal length again.  For strings, this default value may be considered as code point 0, so comparing "ab" to "abcde" can be considered as the same between "ab\0\0\0" and "abcde".  It's convenient and practical most of the time, but if you were to pad strings with odd stuff like "W", you could still have well ordered comparisons, it's just not going to be terribly intuitive (imagine a diction where "abT" < "ab" = "abW" < "abX"). --[[User:Ledrug|Ledrug]] 05:48, 30 November 2011 (UTC)

==Is the task statement consistent?==
I learnt about the mathematical meaning of lexicographic sorting, rather than the lexicographic dictionary ordering through this task and the discussions here, thanks. This did make me look deeply at the task description, and I think there may exist an inconsistency in the language of the task description. It might have been put in whilst trying to cover the case of equal lists.

This statement:
:''The function should accept two lists as arguments and return <code>true</code> if the first list should be ordered before the second, and <code>false</code> otherwise.''
Uses the word should so if comparing two equal lists, empty or not, it seems this implies that the answer must be false, as an ordering is indeterminate.

This statement:
:''If the first list runs out of elements the result is true. false otherwise.''
When following the (rather opaque to me), mathematical description of the process, you would run out of elements if the process is:

```txt
:label1
  Try to get next A element
  If fail to get as there are no more A elements:
    If there are more B elements:
      # B is A with extra tacked on and so A precedes B
      return True
    Else:
      return True # ???
  Else:
    # We have the next element of A
    Try to get next B element
    If fail to get as there are no more B elements:
      # A is B with extra tacked on and so A does not precede B
      return False
    Else:
      If this A element precedes this B element:
        return True
      Else:
        If this B element precedes this A element:
          return False
  # Got equal elements from A and B
  Goto :label1
```

      
If you were to focus on the ??? point in the pseudocode, it seems that the first quote from the task description says that the return value should be False, whilst the second says True.
: you are right, it can be interpreted that way. i have updated the description to clarify what happens when both lists run out of elements.--[[User:EMBee|eMBee]] 08:36, 30 November 2011 (UTC)

* Could someone comment on my interpretation of the mathematical description from the wp article [[wp:Lexicographical order#Ordering of sequences of various lengths|link]].
* Other work beckons so I am unable to suggest a remedy at this time.
--[[User:Paddy3118|Paddy3118]] 07:26, 30 November 2011 (UTC)

If the relations can be strictly defined, might as well:
:: Given two lists of numbers: <math>\{a_i\}</math> where <math>i = 1, 2, \cdots M</math> and <math>\{b_j\}</math> where <math>j = 1, 2, \cdots N</math>, the lexicographic ordering between <math>a</math> and <math>b</math> is said to be <math>a < b</math> iff:
::* there exists an index <math>x</math> where <math>a_x < b_x</math>, and <math>a_i = b_i</math> for all <math>i < x</math>; or
::* <math>M < N</math>, and <math>a_i = b_i</math> for all <math>1 \leq i \leq M</math>.
:: Note that by convention, when we say "condition such and such for all i in set A" and A turns out to be empty, this condition is considered sastified.
:: Also note that, the task seems to have sorting in mind; however, if elements can be equal to each other, you ''must'' have a <math>\leq</math> comparison instead of <math><</math>, so equality also needs to be defined.  The reason: a list <math>a</math> is considered sorted if for any pairs of indices <math>i, j</math> where <math>1 \leq i \leq j \leq M</math>, the condition is met: <math>a_i \leq a_j</math>.  The last term can't be stated as <math>a_i < a_j</math> if elements may compare equal, or list (1, 2, 2) will not have a sorted permutation. --[[User:Ledrug|Ledrug]] 09:26, 30 November 2011 (UTC)
:::No, just because <= is meaningful doesn't mean that a <= operator is required for sorting.  (a < b) == !(b <= a) Any of <, <=, >, or >= could be used to write a sorting algorithm, stable or otherwise.  &mdash;[[User:Sonia|Sonia]] 18:28, 30 November 2011 (UTC)
:::: I was not talking about the sorting process, but the well-definedness of a sorted list, for which you'll need a transitive equality relation if any two elements <math>a</math> and <math>b</math> can fail both <math>a < b</math> and <math>b < a</math> conditions, that is, <math>a = b</math>.  Basically, if <math>a = b</math>, and <math>a < c</math>, then the comparison must also yield <math>b < c</math>, else you won't have a definable sorted state, let alone a function to reach it. --[[User:Ledrug|Ledrug]] 18:50, 30 November 2011 (UTC)
Seems to me then that my pseudo-code matches Ledrugs maths (thanks). The first quote from the task  description is therefore wrong. Maybe it could be replaced with:
:''The function should accept two lists as arguments and return <code>true</code> if the first list lexicographically precedes or is equal to the second, or <code>false</code> otherwise.''
(Although it might invalidate earlier solutions). --[[User:Paddy3118|Paddy3118]] 17:45, 30 November 2011 (UTC)
::well, the current definition asks to return false if both are equal, but it matches the original intent (the treatment of equal lists was only clarified today anyways and is only relevant for sorting stability, which as we learned above also depends on the sorting algorithm which is beyond the scope of the task). --[[User:EMBee|eMBee]] 18:14, 30 November 2011 (UTC)

:::Hi eMBee, If you leave it as it is, you would have the first paragraph say one thing and the second another. If you want the ordering to come from that referenced wikipedia article on lexicographic ordering then it seems your first paragraph does not agree with it (the second seems to though). --[[User:Paddy3118|Paddy3118]] 17:35, 1 December 2011 (UTC)
:::: sorry, upon rereading my text i realize that it is ambiguous. what i meant to say is: ''the current definition asks to return false if both are equal, but '''your suggestion''' matches the original intent'', which is to say, that your suggestion is good. (and the rest is about the ''return false if both are equal'' not being important)--[[User:EMBee|eMBee]] 15:01, 2 December 2011 (UTC)
