+++
title = "Talk:Compare a list of strings"
description = ""
date = 2016-11-04T19:23:07Z
aliases = []
[extra]
id = 17742
[taxonomies]
categories = []
tags = []
+++

== Planned task changes ==

I'd like to make the following changes to this task:

<ul>
<li>
<p>'''Instead of requiring tests for ''equality'' + ''less-than'', require tests for ''equality'' + ''less-than-or-equal'' + ''greater-than'''''</p>

''<u>Rationale:</u>'' When I created the task, I had the Perl and Perl 6 solutions in mind, for which ''less-than'' and ''less-than-or-equal'' testing would work the same way &ndash; so I just picked one. But it turns out that in other languages, the ''less-than-or-equal'' test would actually be simpler, and I don't want to hold them back from showing that off. (For example, the C++ solution could simply use the <code>is_sorted</code> function without a custom comparator lambda; the Java solution could probably replace <code>Arrays.stream(a).distinct().sorted().toArray()</code> with just <code>a.sort()</code>; etc.) So, I think the task should cover both strict and non-strict order testing to show the difference; and to mix it up let's change the strict one from 'ascending' to 'descending'.



</li>
<li>
<p>'''Turn the guideline for presenting solutions in a minimalistic way, into a requirement'''</p>

i.e. instead of saying "''There is no need to ...''" say "''Don't ...''", and reword the rest of that paragraph to make the intention clearer. And enforce it by flagging examples that don't follow it with <nowiki>{{needs-review}}</nowiki> or similar. And maybe add the following pseudocode illustration to the task description, to show how different languages could comply with it:

{| style="border:none; border-collapse:collapse"
|-
| style="vertical-align:top" |
<div style="margin:0 1em 1.1em 0; padding:0 0.5em; border:solid 1px #666; background:#EEF">
 <boolean expression involving <span style="color:#33C">strings</span>''>  <span style="color:#666">// All equal</span>
 <boolean expression involving <span style="color:#33C">strings</span>>  <span style="color:#666">// Ascending</span>
 <boolean expression involving <span style="color:#33C">strings</span>>  <span style="color:#666">// Strictly descending</span>
</div>

<div style="margin:0 1em 0 0; padding:0 0.5em; border:solid 1px #666; background:#EEF">
 IMPORT <span style="color:#939">all</span>, <span style="color:#939">zip</span> FROM standard_library
 
 <boolean expression involving <span style="color:#939">all</span>, <span style="color:#939">zip</span>, and <span style="color:#33C">strings</span>>  <span style="color:#666">// All equal</span>
 <boolean expression involving <span style="color:#939">all</span>, <span style="color:#939">zip</span>, and <span style="color:#33C">strings</span>>  <span style="color:#666">// Ascending</span>
 <boolean expression involving <span style="color:#939">all</span>, <span style="color:#939">zip</span>, and <span style="color:#33C">strings</span>>  <span style="color:#666">// Strictly descending</span>
</div>
| style="vertical-align:top" |
<div style="margin:0; padding:0 0.5em; border:solid 1px #666; background:#EEF">
 <boolean expression involving <span style="color:#939">is_equal</span> and <span style="color:#33C">strings</span>>    <span style="color:#666">// All equal</span>
 <boolean expression involving <span style="color:#939">is_ordered</span> and <span style="color:#33C">strings</span>>  <span style="color:#666">// Ascending</span>
 <boolean expression involving <span style="color:#939">is_ordered</span> and <span style="color:#33C">strings</span>>  <span style="color:#666">// Strictly descending</span>

...given these custom subroutines:

 IMPORT <various stuff> FROM standard_library
 
 SUBROUTINE <span style="color:#939">is_equal</span>
     ...
 END SUBROUTINE
 
 SUBROUTINE <span style="color:#939">is_ordered</span>
     ...
 END SUBROUTINE
</div>
|}

''<u>Rationale:</u>''

Most Rosetta Code tasks encourage implementers to show their solutions in the form of a complete, ready-to-run program that also demonstrates the functionality using a few test cases. And for most tasks that makes sense.

But in this task, the "demonstration code" (properly initializing the program, populating the arrays, looping over test cases, calling the tests, printing the output, properly terminating the program) would in most languages be larger and more attention-grabbing than the code for doing the tests themselves and effectively "drown it out". Such a poor signal-to-noise ratio makes it much more difficult to compare the solutions from different languages - which, for a conceptually simple task like this, should be easy. Not to mention it makes it difficult to know which imported libraries are needed for the actual test, and which for the demo code.

Thus, I'd all examples to show each test as a stand-alone boolean-returning expression, plus comments to tell which one is which &ndash; in the form pioneered by the Python solution. <small>(Yeah, I didn't follow it quite so strictly in my own Perl & Perl 6 solutions at first, but I fixed that quickly :D)</small>
</li></ul>

Since the task is only a few days old and still in draft status, I ''hope'' that making those changes is okay, but since there are already 12 solutions (and I'm not well accustomed with the unwritten rules of Rosetta Code), I'd like to get the go-ahead from one of the admins/moderators for this first.

This is also the chance for anyone to voice (substantiated) objections...

Cheers, --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 15:16, 1 July 2014 (UTC)

:Hi Smls, I don't think this should be for administrators as much as it should be for non-newbies. As a non-newbie I should point out that although draft, the twelve implementations makes it more difficult  to make a change and get everyone to update. Against that though, it has only been a day so the implementors should still be "around".
:I am inclined to think that if you really want to make the change and that change will invalidate some entries, then you could do it but also add:
::<nowiki>{{incorrect|TheLanguage|The reason.}}</nowiki> 
:tags to all the entries that need updating so people are more likely to find out that their particular languages implementation needs changing.
: That's one opinion :-)
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:56, 1 July 2014 (UTC)

:: Yes, of course. In fact I can probably update #C++, #D, #Java, #Perl, #Perl_6, #Python, and #Tcl myself; and #J ''already'' tests for non-strict ascending order as it is (and would just need an <nowiki>{{incomplete|...}}</nowiki> tag); which would leave #PL/I and the REXX'es to tag with <nowiki>{{needs-review|...}}</nowiki> or <nowiki>{{incorrect|...}}</nowiki>.
:: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:38, 2 July 2014 (UTC)

==task requirement of a variable name==

The task requirement of:

... ''Assume that the strings are already stored in an array/list/sequence/tuple variable (whatever is most idiomatic) '' <big> <big> '''with the name <code> strings</code>'''</big></big>, ''and'' ...

(Italics and boldface added by me.)

Exactly how strict is that requirement to store the strings in a particular (specific) named program variable?   To show it that way in a computer program with multiple strings, it would be confusing to use the same variable name for multiple strings, especially if an array of lists were specified to idiomatically demonstrate multiple cases. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:30, 2 July 2014 (UTC)

It's rather akin to naming your dog Dog.

I've added a REXX version 3 to comply to the aforementioned requirement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:39, 2 July 2014 (UTC)

-----

: That requirement should be seen in combination with the guideline for presenting the solution in a minimalistic way, rather than listing a full program.

:: Not a problem, I've removed it. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:26, 3 July 2014 (UTC)

: In a complete real-life program it ''would'' be strange to name your list of strings "strings", but if you present each test as a lone expressions (like the task description intended and most of the other solutions already do), then it really helps readers to use a generic variable name like that ''(because it makes the expression more self-descriptive)'', and it helps to use the same name in every solution ''(because that makes it easier to compare the solutions of different languages)''.

: The REXX solution could be presented like this, to make it conform to said guideline ''(which I intend to upgrade to a requirement, see the top section of this talk page!)'':
{| style="border:none; border-collapse:collapse; margin:0 auto"
|-
| style="vertical-align:top" |
<div style="margin:0; padding:0 0.5em; border:solid 1px #666; background:#EEF">

Assuming that <code>strings</code> was created using the "simple arrays" method shown at [[Arrays#REXX]], the tests can be performed like this:


```rexx
ifEqual('strings')   /* All equal          */
ifAscend('strings')  /* Strictly ascending */
```


...using these custom procedures:


```rexx
ifEqual:  procedure;   parse arg list
    ...
    ...

ifAscend:  procedure;  parse arg list
    ...
    ...
```

</div>
|}
: ...of course with the ellipses replaced with the actual code of those procedures, but other than that, nothing else needs to be shown.

: As you can see, this would make it very easy to compare what's going on there to - say - what's going on in the Python solution, even for readers who know neither REXX nor Python. That's the idea behind asking solutions to be presented with a common structure and assuming a common input variable name, and without distractions such as showing how to populate & print arrays ''(there are enough other tasks for that, which can be linked to if it's non-obvious)''.

: Also, as the above code listing demonstrates, I didn't mean that you should feel compelled to use the name "strings" inside your custom procedures etc. - only in the main test expressions, to make them clear and easy to compare.

: I hope that explains it.
: Cheers! --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 11:23, 2 July 2014 (UTC)




-----



{Because of the somewhat (above) unusal HTML formatting, I couldn't intersperse my comments on the incorrect part of your post.}

Regarding your (above) example of:

```rexx
ifEqual('strings')   /* All equal          */
ifAscend('strings')  /* Strictly ascending */
```

As written, it would unfortunately (at worst) raise a REXX syntax error, or it'll execute and then pass a '''0''' or '''1''' to the host (operation system), and then try to run (execute) the '''0''' (or '''1''') program. 

It's not a proper method to be used to invoke a function within REXX for this kind of ('''if''') test, but I understand your underlining meaning.   The method used in REXX would be:

```rexx
         if ifEqual(strings)   then  say 'strings are all equal.'
         if ifAscend(strings)  then  say 'strings are ascending.'
```

Of course, the code that I used issues a message (via the '''say''' verb), but anything could be done there (after the '''then'''), such as set a variable or somesuch. 

I've changed the function names so that it "reads" better:

```rexx
         if isEqual(strings)   then  say 'strings are all equal.'
         if isAscend(strings)  then  say 'strings are ascending.'
```
-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:26, 3 July 2014 (UTC) 
I choose to use a list of (five) strings as I noticed that some programming examples wouldn't display the correct results for one (token) string or a empty (null) string (as being ''all equal'' AND ''all ascending'').   That's why it's productive to show output of the language entries (to show that it can handle the border situations, among other things).   I don't know the subtleties of most languages when comparing strings to nulls, for instance;   is a (non-null) string less than a null, greater than a null, equal to a null ··· ?

Having a common structure is a nice thing to aim for, but not all languages have a common structure that can be used (or understood by a novice).   I assume it would be up to a knowledgeable programmer to verify if the program (in lue of any output) would be correct.   Sometimes it isn't good to force a square peg through a round hole. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:26, 3 July 2014 (UTC)

==Undiscussed deletion (JavaScript) June 5 2016==

:I notice that a functional JavaScript example was deleted without discussion on 5 June 2016, and replaced with an imperative example. 
:Addition is generally preferable to deletion, particularly where approaches diverge, but more importantly, proposed deletions do need to be motivated and explained here on the discussion page. 
:Unless there are objections, I propose to restore the functional version, so that readers of JavaScript are allowed see both approaches, and so that readers of AppleScript are less puzzled by the 'Translation of JavaScript' tag – the link from which was inadvertently broken by this undiscussed deletion. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:21, 4 November 2016 (UTC)
