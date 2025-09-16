+++
title = "Talk:Multisplit"
description = ""
date = 2011-12-30T04:24:38Z
aliases = []
[extra]
id = 9300
[taxonomies]
categories = []
tags = []
+++

== What is going on here? ==

So I see that we have this for some sample output:
 <nowiki>['a', [1, 1], '', [0, 3], 'b', [2, 6], '', [1, 7], 'c']</nowiki>
What's with the empty strings there? Is it saying that there are empty strings at position 3 and 7 in the input string and that they correspond to separators 0 and 1? This doesn't make sense to me at all. --[[User:Mwn3d|Mwn3d]] 04:27, 27 February 2011 (UTC)

The task description doesn't stand on its own at the moment. Do you mean to have arbitrary interpretation of what constitutes a match for example?

I.e. given a text of 'X123Y' and separators '1', '12', '123', '23', and '3' then there are a multitude of possible answers which would fit the initial task description. This may lead to answers that are difficult to compare due to diverging interpretations of the spec.

The output format seems to be Python specific. Do you mean it to be or could any ordered output of ''<code>sub<N>, sepnum<N>, seppos<N>, ...</code>'' work? --[[User:Paddy3118|Paddy3118]] 04:46, 27 February 2011 (UTC)

@Mwn3d: This format is for convenience: at even (from 0) positions we have strings, at odd - separators. Different formats are suitable for different tasks.

@Paddy3118: Separators are considered in input order.

 <nowiki>Input: 'X123Y', ['1', '12', '123', '23', '3']
Output: ['X', [0, 1], '', [3, 2], 'Y']</nowiki>
It is acceptable to output this information in data structures native to programming language. --[[User:DSblizzard|DSblizzard]]
:'a' is at an odd position according to the output, but it's a string. And that still doesn't explain where "<nowiki>''</nowiki>" came from. --[[User:Mwn3d|Mwn3d]] 03:07, 28 February 2011 (UTC)
::odd - if we will count from 1, OK. This format with empty strings allows us to quickly answer, what is the 1548-th separator in a string, without looking over first 1547 separators. Another format which allows to answer such question will be more complicated. --[[User:DSblizzard|DSblizzard]]

I've reworded the task so that it actually describes the task rather than referring to an implementation. I'm also trying to avoid having it state that the strings and separator information have to be interleaved; that's a very odd thing to do in some languages. My aim was that the solutions given should continue to be solutions, but that the description won't stop other ways of doing the challenge; after all, we ''want'' solutions to tasks to be as idiomatic as possible in their language. –[[User:Dkf|Donal Fellows]] 10:31, 28 February 2011 (UTC)

It seems to me that this task should be split up into the three or four actual tasks that are being confused with each other. --[[User:TimToady|TimToady]] 17:59, 26 April 2011 (UTC)

:I do not know if it needs four tasks, but the API needed for an implementation of both the task and the extra credit could be annoyingly complicated to implement, for some languages and/or implementers.  But I suppose the extra credit part currently does have enough ambiguity to spawn several tasks? --[[User:Rdm|Rdm]] 20:30, 26 April 2011 (UTC)

::Actually, I was referring only to the ambiguity in the interpretation of the non-extra credit part. <tt>:)</tt> The degrees of freedom (some since patched in the description) seem to have included 1) whether patterns are to be applied left-to-right in parallel vs each pattern does a split and then each substring is split on subsequent patterns, 2) whether the patterns are meant to be re-used vs the patterns are used once (or maybe even cyclically), and 3) whether the list should be assumed to be ordered vs the function should order them to ensure longest-token matching. Most of this dithering would have been avoided if the task description had been written unambiguously in the first place, but seeing the ambiguities of one's own writing is difficult for most folks... --[[User:TimToady|TimToady]] 23:20, 26 April 2011 (UTC)

==Correct result==
How about a statement of the correct result.
: For example the D example leaves separator characters unmatched, by my reading that wasn't intended.
:: given “a!===b=!=c” and the separators “==”, “!=” and “=”.
:: yielded a! <==> =b= <!=> c <=> = <!> =d  (with <> demarcating separators)
:: shouldn't that be a <!=> <==> b <=> <!=> c
--[[User:Dgamey|Dgamey]] 11:00, 21 April 2011 (UTC)

Vincent> Double-check what a string analysed in D version.

:It shouldn't match all possible separators. It should match them in the order that they are given. With that example, the first "==" in "a!===b" is matched and removed from later matching because "==" is the separator that it checks for first. If you change the order of the separators to "!=", "==", "=" then it will match like this: a <!=> <==> b <=> <!=> c. If you change the order to "=", "!=", "==" then it will match like this: a! <=> <=> <=> b <=> ! <=> c. --[[User:Mwn3d|Mwn3d]] 15:27, 21 April 2011 (UTC)

::It is true that that is what the D program is doing, but apparently that is not what it is supposed to be doing.  The order in which the separators matters is supposed to only be relevant where there is ambiguity at a point.  That said, the task currently only implies a "left to right order".  But the part of the task description describing when to use the ordering of the separators (when there is ambiguity at a point) only makes sense in a context where something else determines when a point is relevant, or not.  --[[User:Rdm|Rdm]] 12:15, 26 April 2011 (UTC)

Vincent> Program does exactly what you describe, except your mistake: spearators doesn't reused once they are finished, so for “a!===b=!=c” ("!=", "==", "=") it produces “a <!=>  <==> b <=> !=c” - note that '!=' separator doesn't used AGAIN.


###  String modified

I noticed another present from the argument earlier.  "==d was added by anonymous and not caught making every answer wrong.  I will reset it. --[[User:Dgamey|Dgamey]] 01:19, 27 April 2011 (UTC)


### Clarification in order

While the text says "the order of the separators is significant; where there would otherwise be an ambiguity as to which separator to use at a particular point (e.g., because one separator is a prefix of another) the first separator in the collection should be used."  your interpretation doesn't follow.  If you have x!==y and the separators !==, !=, and = the ambiguity referred to could equally be do you parse out x !== y or x != = y.  Because of the order !== precedes != the first is correct.   The example I gave above slid the rules I described over the characters left to right.  And as for not reusing a separator, nowhere does it say that. Basically, either interpretation could be correct at this point as the task isn't specific enough.

Most of the examples don't even clearly show output, so it's difficult to tell which are matched (The reader should not have to figure it out from listings of indices).  It should be a requirement to show the output so you can see how it's being parsed out.  As it stands it's difficult to tell if some of the solutions are correct under either interpretation.
--[[User:Dgamey|Dgamey]] 23:11, 21 April 2011 (UTC)

:Vincent> Dgamey, please don't spread chaos in your mind to this task. There is no ambiguity, since every delimiter taken once and HIS FIRST OCCURENCE in string is used. And ONLY AFTER THAT next delimiter is considered in THE REST OF STRING. Sorry, but you're not qualified enough to discuss this task, please finish university and return to programming. Nothing personal, sorry.

::Vincent, I am trying to have a discussion about the intent of the task.  I still think it's unclear.  If I were 100% sure that the D interpretation was wrong, I would have marked it incorrect.  I did not do that.  However, I do believe with a fair amount of certainty that the D interpretation is based on an incorrect interpretation (just as you believe it is right).  Hence this discussion - I am using the wiki the way it is intended.

::I don't believe you are the author of this task. The history indicates that would be DSblizzard.  Input from the author is what is needed to clarify this.  BTW this is a draft task - discussion is what these are about.

::I believe the task should be required to clearly show output.  I did.  I also know you will think my code is incorrect based on your interpretation.  But until the author clarifies the task, they should both stand.

::Your capitalized text is not quoted from the description and it is your interpretation. Please reread the task description carefully and I think you may see the other interpretation is also possible.

::I had suggested showing more clear output as a requirement, to make it clear what each solution accomplished.  I strongly suspect that some of the other solutions would not produce the same results as the D solution.

::Talking about me "spreading choas" etc. despite your claim is a very rude attempt to get personal.  Please measure your responses and keep the discussion civil. If we cannot agree, then can agree to disagree and wait for clarification from the author.  --[[User:Dgamey|Dgamey]] 11:14, 22 April 2011 (UTC)

:: Vincent, relax. The one thing I ''really'' don't care for on RC are attacks on fellow editors.

:: Second, the task strikes me as underspecified. From the task description, it's not clear to me whether a sequence like "!==" should be parsed as "!=" "=" or "!" "!==". In fact, as far as I can read the output of the code examples, I don't think I'm the only one who sees ambiguity there. I'm sending an email off to the task author in hopes they'll step in.

::: D's interpretation is wrong. Output in D's example should be a {!=} {==} b {=} {!=} c {==} {!=} d in its notation. Please use (my) Python's "Alternative version" as a reference. You can run python code at http://codepad.org. [[User:DSblizzard|DSblizzard]] 06:53, 24 April 2011 (UTC)

:: Also, please remember to sign your comments with <nowiki>--~~~~</nowiki> --[[User:Short Circuit|Michael Mol]] 12:26, 22 April 2011 (UTC)

Vincent> Guys, your fantasy used in wrong direction. In input we have string (most probably PIECE of some log or program's config, whatever) and ordered delimiters. It's not a big thinking effort to realise, that ordering of delimiters has meaning - they assume special format of input string, not just stupid "split by space" task.
Vincent> Second, you, Dgamey, "Mr. Change Conditions To My Own", leave task description alone from your ugly results - only author can do it. Name yourself as sh*ty as you can for such cheat behaviour.

:: Vincent, the author of the task has clarified the intent in these talk pages.  You continue to insult editors, refuse to sign your updates as requested by the owner of the site.  You're behavior is not supportive of the site, the owner, or other editors. I only changed the task description to what the owner had documented here above. Please think about what you are doing.--[[User:Dgamey|Dgamey]] 11:06, 26 April 2011 (UTC)

: Hi Vincent, could you make your point without the abuse? We are trying to maintain a community. No one abused you. Try convincing people with the strength of your argument - swearing tends to detract from any argument you do attempt.
: P.S. Anyone can improve task descriptions although it is usually done after, and in conjunction with, debate on the talk pages. --[[User:Paddy3118|Paddy3118]] 11:25, 26 April 2011 (UTC)
: P.P.S. I have written a few task descriptions myself, and usually on R.C. when someone says they have an issue with it, then they are having an issue. It usually points to people being less familiar with something that the task author thinks everyone should know. Try thinking of other ways to explain things on the talk page to DGamey. Look him up, see what they have contributed in the past to RC. If they have added language implementations, then it usually points to a legitimate misunderstanding on their part and helping them usually leads to a better task description for all. --[[User:Paddy3118|Paddy3118]] 11:25, 26 April 2011 (UTC)

: Vincent, This discussion page is full of talk about which interpretation is correct.  Whether "==" parses first or "!=" does. It is throughout the  "F# is incorrect" talk as well as ours. DSblizzard authored the task and said "D's interpretation is wrong", Rdm says "D and Java implementations which are incorrect", I've also said so.  Mwn3d who has made hundreds of contributions got it wrong from the original description.  This is very easy to do especially when the task is in draft and changing.  If you based your code on one of the examples that has been shown to be incorrect because you thought the code was clearer than the description, it is understandable and inevitable that you would implement code that would also be wrong.  There is no shame in that.  You're new to RC having written just a handful of tasks.  Wading in on a draft task is risky because the task will often change.  It is simply how it works.  I'm going to put back the clarification in the task description please don't change it unless you can debate here and convince people besides myself that it needs to be another way.  --[[User:Dgamey|Dgamey]] 12:07, 26 April 2011 (UTC)

Vincent> "Paddy3118 said: No one abused you.". Wrong. Dgamey tries to defame my solution (on D), while he DOESN'T UNDERSTAND given task himself! (Cite of him: "task isn't specific enough")  Question is: how much cost his rubbish above? Nothing. Second, he is CHEATING with original task, putting his output as a "reference". You know what? I f__ed such "specialists" with their sh*ty languages like his "Icon".
I'm new to RC, but it's better than to be "new" for CS at all.

: Vincent has been blocked again for infantile behaviour. I'll second that decision as I like the way RC gets things done without the hate! --[[User:Paddy3118|Paddy3118]] 13:06, 26 April 2011 (UTC)
: Vincent, do not attempt to evade the ban again. If you want the bans repealed before their set expiration time, if you believe this is somehow unfair, or if you believe that you shouldn't have been banned, email me directly. You can find my email address on my user page. You're the first site participant I've ever had to ban, and I'll be happy to explain why via email. If you choose to ''evade'' the ban again, you will no longer be welcome on this site in any capacity, ever. In a short time, you've managed to drive up tempers, ignore reasonable requests and give the community a sour taste. --[[User:Short Circuit|Michael Mol]] 14:12, 26 April 2011 (UTC)

Is the newest edit to the description wrong? How can it only use the delimiters once in that string? --[[User:Mwn3d|Mwn3d]] 13:10, 26 April 2011 (UTC)
: It should be fixed now.  It was another anonymous update just as the ban/protection was going in. --[[User:Dgamey|Dgamey]] 21:40, 26 April 2011 (UTC)

==Small inaccuracy in the smaller non-RE Python version?==
This is a reason to tighten the task description, as I think the task description relies too much on the original Python implementation at the moment. --[[User:Paddy3118|Paddy3118]] 08:16, 28 February 2011 (UTC)
:I'm not tempted in a formulation of Rosetta tasks, therefore feel free to rewrite the task as you see fit.--[[User:DSblizzard|DSblizzard]] 09:33, 28 February 2011 (UTC)

== J implementation ==

:Author's note:  ''I get impatient, when I re-read this.  Partially, that's because today has not been a good day for me.  But if I get impatient reading it I have to wonder if anyone else gets anything of value from this writeup.  '''If you have gotten something of use from this writeup please delete this comment'''.  Otherwise, I suspect I will delete this entire section at some point in the future (as opposed to fixing the grammar and other issues which bother me).  Other people have working implementations of this code now, so I feel there's no point in explaining things from a "how to do it in the first place" point of view. --[[User:Rdm|Rdm]] 18:33, 5 July 2011 (UTC)''


### implementation


The J implementation currently looks like this:


```j
multisplit=:4 :0
  'sep begin'=.|:t=. y /:~&.:(|."1)@;@(i.@#@[ ,.L:0"0 I.@E.L:0) x
  end=. begin + sep { #@>y
  last=.next=.0
  r=.2 0$0
  while.next<#begin do.
    r=.r,.(last}.x{.~next{begin);next{t
    last=.next{end
    next=.1 i.~(begin>next{begin)*.begin>:last
  end.
  r=.r,.'';~last}.x
)
```


The first line uses a fair bit of point free code (because it was convenient, and easy for me) and many Rosetta readers are likely to have problems reading it.  And task currently has people puzzled, so perhaps a fuller discussion would also be helpful.


### sep begin


The first line of this multisplit defines two variables: <code>sep</code> and <code>begin</code>


```j
   y=. '==';'!=';'='
   x=. 'a!===b=!=c'
   'sep begin'=.|:t=. y /:~&.:(|."1)@;@(i.@#@[ ,.L:0"0 I.@E.L:0) x
   sep
1 0 2 0 2 2 2 1 2
   begin
1 2 2 3 3 4 6 7 8
```


(Note: its normally bad practice in J to use variables named 'x' and 'y' because they get values bound to them in functions.  However, for purposes of illustration I think it's fine to pull code out of a function and use that with explicitly assigned values.)

Here, <code>sep</code> is a list of 0, 1 or 2, with 0 corresponding to '==', 1 corresponding to '!=' and 2 corresponding to '='.  And, <code>begin</code> is a list of corresponding character positions.  The values in <code>begin</code> are in non-decreasing order, and the values in <code>sep</code> are ascending when they correspond to equal values in <code>begin</code>.

So, how does that work?

<code>E.</code> finds locations where substrings match:


```j
   '==' E. 'a!===b=!=c'
0 0 1 1 0 0 0 0 0 0
```


In other words, for each character in the right argument, we have a 1 if the substring in the left argument begins there.

Or, we can get indices instead of a bit mask:


```j
   '==' I.@E. 'a!===b=!=c'
2 3
```


Except we need to deal with multiple separators, and they can be differing lengths which means that representing them as a homogeneous array would be bad (every item in a homogeneous array must have the same length).  But we can put them in boxes, and have an array of boxes:


```j
   '==';'!=';'='
┌──┬──┬─┐
│==│!=│=│
└──┴──┴─┘
```


But now we need to tell I.@E. that it is not supposed to work on the boxes directly, instead it needs to work inside the boxes.


```j
   ('==';'!=';'=') I.@E.L:0 'a!===b=!=c'
┌───┬───┬─────────┐
│2 3│1 7│2 3 4 6 8│
└───┴───┴─────────┘
```


Ok, so great, but now we need to combine the results from those different boxes so we can work with them.  Except, before that, right now the boxes distinguish which separator we are using, and we are going to need to add that, explicitly, so we do not lose track of that information after the boxes are gone.  In other words, we have three boxes so we need three different values to label their contents with:


```j
   i.@# '==';'!=';'='
0 1 2
   ('==';'!=';'=') i.@#@[ 'a!===b=!=c'
0 1 2
```


So, let's add the labels to each of the values in each of the boxes:


```j
   0 1 2 (,.L:0"0) 2 3;1 7;2 3 4 6 8
┌───┬───┬───┐
│0 2│1 1│2 2│
│0 3│1 7│2 3│
│   │   │2 4│
│   │   │2 6│
│   │   │2 8│
└───┴───┴───┘
   ('==';'!=';'=') (i.@#@[ ,.L:0"0 I.@E.L:0) 'a!===b=!=c'
┌───┬───┬───┐
│0 2│1 1│2 2│
│0 3│1 7│2 3│
│   │   │2 4│
│   │   │2 6│
│   │   │2 8│
└───┴───┴───┘
```


(In the first example, we needed parenthesis to tell the computer that 0 2 3 was not meant to be combined.  In the second example, we need parenthesis to tell the computer that we want the result of the left and right verbs to be the arguments for the verb in the middle.)

So, now we are ready to combine them:


```j
   ;('==';'!=';'=') (i.@#@[ ,.L:0"0 I.@E.L:0) 'a!===b=!=c'
0 2
0 3
1 1
1 7
2 2
2 3
2 4
2 6
2 8
```


and sort them:


```j
   ('==';'!=';'=') /:~&.:(|."1)@;@(i.@#@[ ,.L:0"0 I.@E.L:0) 'a!===b=!=c'
1 1
0 2
2 2
0 3
2 3
2 4
2 6
1 7
2 8
```


This suggests a simplification -- The &.:(."1) means we are reversing each row before we sort and then reversing them back after each sort.  But this busy work could be eliminated by reversing the order of the columns.  I might change the code (and this writeup), but I am not going to do that now -- and someone else can (and delete this paragraph) if they get to that before I do.

Finally, to assign this array to two different values, we need an array of two items, so we transpose the array.


```j
   |:('==';'!=';'=') /:~&.:(|."1)@;@(i.@#@[ ,.L:0"0 I.@E.L:0) 'a!===b=!=c'
1 0 2 0 2 2 2 1 2
1 2 2 3 3 4 6 7 8
```


But before transposing it we save a copy in the variable <code>t</code> because we need that for the "extra credit" part of this task.

===other pre-loop setup===


```j
  end=. begin + sep { #@>y
  last=.next=.0
  r=.2 0$0
```


<code>end</code> has the same structure as <code>begin</code> and <code>sep</code>, and gives the character position where each separator ends.  It's just the beginning character location plus the length of each corresponding separator.

<code>next</code> will be our loop variable -- it's the index of the next value from <code>begin</code>/<code>sep</code>/<code>t</code>/<code>end</code> to be used.

<code>last</code> will be our "the last separator ended here" value, which we will be using to search for the next value for <code>next</code>

Finally, <code>r</code> will hold our result.  It's arranged horizontally rather than vertically, because that uses less screen real estate on rosettacode.  And, fortuitously, that makes it easier to select out just the string values (the useful part) from the "extra credit" result.</code>


### while. loop


<code>next</code> is an index into any of <code>begin</code>/<code>sep</code>/<code>t</code>/<code>end</code> and when it goes off the end, we are done.


```j
r=.r,.(last}.x{.~next{begin);next{t
```


This has three important parts:

#  <code>r=.r,.</code>''value'' (the result builder).
#  <code>last}.x{.~next{begin</code> (the substring extractor), and
#  <code>next{t</code> (the extra credit assignment)

The first time through the loop, <code>last</code> and <next> will be 0, and the first value from begin is 1, so <code>last}.x{.~next{begin</code> is equivalent to <code>0}.'a!===b=!=c'{.~1</code> or: take the first 1 character from that string and drop the first 0 characters from it.  That's the substring containing the single character 'a'.  Likewise, <code>0{t</code> will be the value <code>1 1</code>.

Next, we save the value where this delimiter instance ends.  In other words <code>last</code> takes on the value 3.

Finally, we use that ending location to find our next delimiter instance.  <code>begin>:last</code>, the first time through the loop, is equivalent to


```j
   1 2 2 3 3 4 6 7 8
: 3
0 0 0 1 1 1 1 1 1
```


And, that would be enough, but I did not want an infinite loop if someone happened to use an empty delimiter, so I added an explicit test for that.  It's just a safety measure, and does not actually matter for the required test case. But next{begin is still 1, so:


```j
   1 2 2 3 3 4 6 7 8
 1
0 1 1 1 1 1 1 1 1
```


Anyways, I find the index of the first value where these two bitmasks are both set.  (<code>*.</code> is J's boolean and operator.)

```j
   0 1 1 1 1 1 1 1 1 *. 0 0 0 1 1 1 1 1 1
0 0 0 1 1 1 1 1 1
   (i.&1)0 1 1 1 1 1 1 1 1 *. 0 0 0 1 1 1 1 1 1
3
```


The next time through the loop, <code>next</code> will be 3.  And, when we are done, one of these bitmasks will be all zeros, so next wil l be larger than the the bitmask length.


### ending


Finally, we have one last bit of code to execute:

```j
r=.r,.'';~last}.x
```


This gives us the "tail end" -- everything appearing after the last valid separator (or the entire argument string if none were valid).  Since there is no separator terminating this instance, I use an empty value to represent its details.

== F# incorrect ==

In this part of the F# example, you can see that it is incorrect:

```fsharp>
 "a!===b=!=c".Split([|"="; "!="; "=="|], System.StringSplitOptions.None);;
val it : string [] = [|"a"; ""; ""; "b"; ""; "c"|]
```


"a!===b=!=c" should be split like this (because of the order of the delimeters):
 a!, =, =, =, b, =, !, =, c
"=" comes first in the delimeters list. <nowiki>"!=" should never be matched because the "=" has already been matched by the first delimeter in the list. If "!=" could be matched before "=", then the order of the delimeters wouldn't matter. Since the order matters, you scan for "=" first.</nowiki> --[[User:Mwn3d|Mwn3d]] 18:50, 21 April 2011 (UTC)

:That is not how the splitting algorithm works for the original example (Python), and does not match the task description.  The order in which the delimiters appear only matters when multiple delimiters match at the same position.  When there is only one delimiter which matches at a position, there is no ambiguity.  And the matching is implicitly from left to right.  In other words, it's the D and Java implementations which are incorrect, not the F# implementation.  --[[User:Rdm|Rdm]] 19:01, 21 April 2011 (UTC)
::This task has been a problem for me from the start. So what you're saying is the input string "abb" and delimiter list ["b", "ab"] would match "ab" (the first two characters) and then "b" (the third character)? I'd like to see example inputs and outputs in the task description. I'll just take the Java example down until I understand the task better. --[[User:Mwn3d|Mwn3d]] 19:22, 21 April 2011 (UTC)
:Yes, you would get three empty strings matching "abb" against the list "b","ab".  Anyways, it's basically:

:
```pseudocode

   for (i= 0; i < inputString.stringLength; i++) {
      for (j= 0; j < separators.arrayLength; j++) {
         if (inputString.substring(i, separators[j].stringLength) == separators[j]) {
             split inputString here
             advance i until it is at the end of this instance of separator j
         }
      }
   }
   whatever remains of inputString after the last separator gets included in the result

```
 --[[User:Rdm|Rdm]] 19:57, 21 April 2011 (UTC)

I do not understand the expected results statement in the task description. The task description says "The function should take an input string and an ordered collection of separator strings, and split the string into pieces representing the various substrings."

Given an input string a!===b=!=c, and three separators ==,!= and =, I would expect a result as follows:

 a! =b= c

With the operators matching as follows:

a! (==) =b= (!=) c
Note that the third separator is not matched.

--[[User:Markhobley|Markhobley]] 21:43, 9 June 2011 (UTC)

: There has been a lot of confusion over this task.  A couple of people took the position that the delimiters were to be matched and not reused. Others took the position that the delimiter order represented priority in matching and they could be reused.  This later is the consensus opinion.  Having said that at each position left to right see if a delimiter matches. If not advance and try again.  Hence: "a" is not a delimiter, "!=" is, now at "==b", "==" (and not "=") is the delimiter.  Does that help? --[[User:Dgamey|Dgamey]] 22:46, 9 June 2011 (UTC)

With so many caveats should the F# be just dumped here on the talk page and maybe resurrected as an alternative solution once we have a more compliant F# solution? --[[User:Paddy3118|Paddy3118]] 14:47, 21 July 2011 (UTC)

==No longer draft==
The updated wording looks good.  I vote to promote. --[[User:Dgamey|Dgamey]] 00:50, 6 July 2011 (UTC)
:How about waiting for one more good implementation? I have just read the talk page again which makes me want to be sure. --[[User:Paddy3118|Paddy3118]] 06:00, 6 July 2011 (UTC)
:: The task description is much better and the implementations are now pretty consistent.  But ok. --[[User:Dgamey|Dgamey]] 14:13, 6 July 2011 (UTC)

== Desired output ==

So what is the desired output of the program? Some solutions on this page only print the fields between the delimiters and don't print anything about the delimiters. Some solutions print a list of alternating fields and delimiters. Others alternate the fields with a pair indicating the type and location of the delimiter. It seems fairly inconsistent. --[[Special:Contributions/208.80.119.68|208.80.119.68]] 23:16, 29 December 2011 (UTC)
: (sigh) This task has had a sordid history and was the subject of some vandalism relating to the intended way to parse it.  It appears that the current description could have had a better reviewed after that all died down.  The description starting "For these inputs the string should be parsed  ..." talks about the expected parsing and output. The output description was added later.  As it stands an output of "a b c" works, but this was a late addition to the description. Based on the discussions I would have expected the string to (i) include the substrings and separators so something like "a != == b = != c" works , or (ii) to show the substrings including the null substrings so that something like "a,,b,,c" works. Now where does that leave examples that output "a b c"?  My preference would have been for (i) as the separators are at least as interesting as the substrings.  However, I'm not sure if reopening this is useful. The main point is does the output clearly show the input was parsed correctly. Keep in mind that any change to the task description that invalidates existing solutions would need to be marked with a template that indicates the task description changed and the example needs improvement. (I forget the template name for this).  --[[User:Dgamey|Dgamey]] 04:20, 30 December 2011 (UTC)
