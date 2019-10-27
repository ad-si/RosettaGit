+++
title = "Talk:Brace expansion"
description = ""
date = 2016-07-26T06:49:54Z
aliases = []
[extra]
id = 17129
[taxonomies]
categories = []
tags = []
+++

== Task is ready for accepting solutions! ==

As far as I'm concerned, the task description is stable. Feel free to add solutions for other languages, and if you have any problems with the task then drop a line here!
--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 17:49, 25 January 2014 (UTC)
:There appears to be a bug in the task.  Your third test case should have backslashes on the comma by the "always leave in the backslash" rule.  See the Perl 6 output.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:02, 26 January 2014 (UTC)
::I edited the task to add the backslashes, after verifying that the Perl code actually does include the backslashes before comma. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:53, 26 January 2014 (UTC)
:::Oops, sorry about that, I verified the Perl function using a script that automatically tested it against a local copy of the test cases (and a bunch more), where I did add the backslashes. Thanks for fixing the test case here. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:16, 26 January 2014 (UTC)
:Also, with the toy output, it's not at all clear that the current Perl or Python implementations are doing this according to spec.  The Perl code claims to follow the spec, but the spec is missing backslashes on the commas.  The Python code talks about barfing on code that the specification says it should accept.  Maybe we should require a bit more rigor in proving solutions actually pass the spec for backslashed and not-really-meta characters. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:22, 26 January 2014 (UTC)
:: Regarding the python code: the commas not in bracers can be parsed as literal chars, but the unmatched bracers as specified by task is not workable.  How do you parse "{a,{b,c}" ? As "{a", "b", "c", or as "a", "{b", "c"?  Same for closing bracers.  This is not just a problem with descending parsers, it's simply ambiguous and counterintuitive, so best treated as a syntax error IMO. The part of spec about "{a}" parsed literally is also not done in python, which can take some workaround but is again not intuitive. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 02:37, 26 January 2014 (UTC)
:::I understand that Python culture tends more toward throwing an exception if in doubt, but I think the intent of the task was to emulate the shell's notion of how to do this, hence the failsoft aspects, which are pretty easy to do with a decent backtracking engine.  It's certainly straighforward in the Perl 6 solution, once I figured out the top level parses with slightly different rules than sublevels do. And it was certainly specced how it was supposed to treat weird characters. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:49, 26 January 2014 (UTC)
:::: Well, I'm not really with the python culture, but that's not important.  I can see why the spec is setup this way, but the fact remains that the rule about unmatched bracers is underdefined.  The spec should say which bracers would be considered extra for consistency across implementations. I'll withdraw the python code for now since it does fail to follow other rules, while the task hopefully can get some clarification. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:19, 26 January 2014 (UTC)
:::: Another thing, what's supposed to happen when the string ends with an extra "\"? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 03:29, 26 January 2014 (UTC)
:::::I've attempted to clarify what to do in the case of ambiguity; in general, the closest brace that makes sense in context is used. I suppose a backslash at the end of the string should just be taken as a literal. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 04:00, 26 January 2014 (UTC)
::::: Yeah, I thought the "closest brace" disambiguation was implied by ''"}	closes the last opened alternation group "'' and all the talk about "balanced" brace pairs, but it's a good thing you added a more explicit clarification. As for it being "counterintuitive", well, it's how my shell (zsh) handles unbalanced braces, so at least to me it's intuitive... :) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:16, 26 January 2014 (UTC)
::::: Regarding backslash at the end of the string, I semi-purposefully left that case unspecced, because such inputs may well be impossible in real-life scenarios, and I didn't want to force implementors to complicate their solutions because of it. But I did implement it as "interpret literally and pass along to output" in the Perl solution, just in case. If you think it makes sense as a hard requirement, I'm OK with having it in the spec. Although I would also be fine with something like "''You may assume that input strings will not end with an unescaped backslash''" in the spec. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:16, 26 January 2014 (UTC)
:::: TimToady is right, I did intend for fault-tolerant parsing (as opposed to throwing an error on malformed input) to be a core aspect of this task (see the "Motivation" section). Of course if it's an unreasonable requirement (e.g. making the task too big/difficult) it could be dropped, but I think that would make the task much less interesting (and there are already many other Rosetta Code tasks that deal with strict parsing). Note that I managed to implement all the requirements in the Perl solution, with plain while loops and a non-backtracking regex (which could even be replaced by low-level 'substr' operations without much difficulty), so it should be possible in Python as well. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:17, 26 January 2014 (UTC)
:: Hm yeah, I guess requiring solutions to demonstrate the four test cases (with a listing of the actual output), instead of some "toy output", might make sense after all (even if it will make the page very long once there are many implementations). Btw, that Perl 6 solution looks pretty sweet... :) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:16, 26 January 2014 (UTC)
::: I updated both the task description and the Perl solution accordingly now. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 10:57, 26 January 2014 (UTC)

== Duplicate supression ==

The task currently has an ambiguity - specifically an explicit reference to an implementation (perl 6) which implements a requirement which is not explicitly stated in the task description.

More specifically, the perl 6 implementation suppresses duplicate results despite no description of this mechanism in the current task description. Consider {a,a,b} as an example which would generate duplicates. This issue shows up in the test case '{,{,gotta have{ ,\, again\, }}more }cowbell!' where nested empty prefixes appear. This case should have eight expansions. You can easily see this by placing a letter to the left of each non-escaped comma.

This means either the task description is buggy or the reference implementation is buggy. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:31, 31 January 2014 (UTC)
:Well, I believe the task actually refers to the Perl (5) implementation, not the Perl 6 one.  But leaving that aside, there's no duplicate suppression taking place here.  Note what happens when I run these (through the Perl 6 version):

```txt
{X,{Y,gotta have{ ,\, again\, }}more }cowbell!
    Xcowbell!
    Ymore cowbell!
    gotta have more cowbell!
    gotta have\, again\, more cowbell!

{a,a,b}
    a
    a
    b
```

:We still get four entries for the first one, and the dupes aren't removed from the second one.  I don't know what you're doing different, offhand, perhaps not throwing away stuff when you backtrack?  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 18:54, 31 January 2014 (UTC)
::Actually, there ''does'' seem to be a bug in the latest revision of the Perl 6 solution (which wasn't present in the original revision). Either that, or the Rakudo Star version I'm using to test it is too old or something. Because ''without'' the inserted X / Y, it only prints 2 expansions for that test case. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 19:07, 31 January 2014 (UTC)
:::You don't have latest version, which I fixed in the last hour. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 19:13, 31 January 2014 (UTC)
::::You're right, the latest revision does in fact work correctly again. My mistake. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 19:22, 31 January 2014 (UTC)

:The reference solution (Perl, not Perl 6) does agree with the spec: There should be no suppression of duplicates. As for the test case you mention, it has '''four''' expansions and not eight, because nested brace groups only expand the "branch" they belong to. For example, the pattern <code>aa{,{,11}cc}22</code> has three expansions (<code>aa22</code>  <code>aacc22</code>  <code>aa11cc22</code>), not four. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 18:58, 31 January 2014 (UTC)

:: That's a good point, that brace expansion should be only relevant in the surrounding context where it appears. So that's my bug and hopefully I can find it now. Thank you. --~~

:: Actually... after thinking about this further... I still think it's a problem.

:: Let's take aa{,{,11}cc}22 - we expand the inner braces and get aa{,cc}22 and aa{,11cc}22. This should then expand into aa22 aacc22 aa22 and aa11cc22. It seems to me that doing otherwise violates the specification that the inner brace be expanded first. Thoughts? Thanks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:39, 31 January 2014 (UTC)

:::The spec doesn't state that. In fact, when you've only expanded an inner brace group, you ''cannot'' give an intermediate result for the whole string yet, only a result for the "parent alternative" that the inner group belongs to. Another way to look at this, is that you (conceptually) expand the ''outer layer'' first, then separately expand each resulting alternative, and so on:
<pre style="margin-left:6em">
aa{,{,11}cc}22  ----->  aa22
  ^^^^^^^^^^      \
                   `->  aa{,11}cc22  ----->  aacc22
                          ^^^^^        \
                                        `->  aa11cc22

```

:::The spec comes at it from a slightly different angle, but it evaluates to the same thing: "''The rules for parsing/expanding the whole string also apply recursively to each alternative of an alternation group''" - which, together with the given examples, should be unambiguous.
:::--[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 21:28, 31 January 2014 (UTC)

:::: This line of thinking scares me. It seems to me that you cannot reliably identify the outer layer, in the general case, until after you have identified the inner layers.

:::: That said, perhaps you are suggesting a system where we first identify the nesting, from the inside out, and postpone expansion until all brace expansions have been identified. That would be relatively straightforward to implement and would explain the stack data structures I've noticed in other implementations. Still, if that is the requirement I would like it to be stated explicitly rather than implicitly or through a reference implementation. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:34, 31 January 2014 (UTC)

::::: What's scary about it? The rules for ''identifying'' (parsing) the layers, and the rules for how to ''expand'' the string based on this information, are conceptually two different things...
::::: '''Parsing''' happens by finding valid balanced braces <small>(using the given information that closing braces match the *nearest* opening brace to their left, and that groups without at least 2 alternatives are not valid)</small>.
::::: '''Expansion''' happens recursively (<small>in case of nested groups</small>), as well as cumulatively (<small>in case of multiple groups on the same layer</small>).
::::: This is all explained in the spec, and demonstrated by the test cases.
::::: Regarding "''first identify the nesting, [...] and postpone expansion until all brace expansions have been identified''", you're free to do that if you feel that's the best way to solve the task in your language. In fact the Perl 6 solution does it that way: It uses a grammar to find where all the nested brace group parts are, and then passes the resulting AST tree to a recursive function which does the expansion. It's not a "requirement" though: The Perl and Python solutions both do identification and expansion in one go ''(albeit in rather different ways)''. The only requirement is that the function does the right thing; how it does it is up to you (based on what you think works best in your language).
::::: --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 22:01, 31 January 2014 (UTC)
:::::: It's scary because it's ambiguously identified. Once again: there are multiple ways of satisfying those test cases. One way requires one recursive pass to identify the relevant braces and commas and then an independent recursive pass to process them. This is certainly doable but many parsing systems are not built that way. Another approach involves suppressing duplicates. Meanwhile, given the wart-for-wart nature of the specification, it seems wrong to leave such a fundamental aspect unspecified. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:28, 31 January 2014 (UTC)

::::::: I'm afraid I don't follow. Whether parsing and expansion happens in two separate stages (like #Perl_6 does) or are combined into a single one (like #Perl and #Python do), is an implementation detail. Unless I'm mistaken, all three of those solutions are conceptually '''identical''' implementations of the spec. I don't see any ambiguity, nor any "suppression of duplicates" happening anywhere.
::::::: I think it would help if you could provide a test-case that demonstrates the ambiguity that you see. I.e. and example input for which the "''multiple ways of satisfying [the official] test cases [and spec]''" will disagree with each other. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 22:46, 31 January 2014 (UTC)

:::::::: {a,a,b} --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:48, 31 January 2014 (UTC)

::::::::: #Perl, #Perl_6, #Python all expand that to the three alternatives  <code>a</code>  <code>a</code>  <code>b</code>  (like the spec requires). --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 22:57, 31 January 2014 (UTC)
:::::::::: My point was that the spec does not explicitly require this. I think that the spec should not neglect this issue. Or let me know if you think I should try to come up with further variations on this theme? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:02, 31 January 2014 (UTC)

::::::::::: Well the second bullet point of the spec includes an example which showcases duplicate alternatives. But I additionally added the phrase "''(which need not be unique)''" now to that paragraph, to make it more explicit. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 23:12, 31 January 2014 (UTC)

::::::::::: I tried the perl implementation against this spec. If I use the pattern ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr I see no result line which contains both an 'f' and a 'k'. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:10, 1 February 2014 (UTC)

:::::::::::: It would be incorrect if it did so, since f and k come from different alternatives, separated by the comma before the i.  Alternatives separated by comma are expanded internally and exclusively from each other—one never takes a cross product over a comma, only over braces.  --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:01, 1 February 2014 (UTC)

:::::::::::: The tree of nested alternation groups for that pattern is:
<pre style="margin-left:20em">
  ⎧c          ⎫
  ⎪    ⎧f  ⎫  ⎪
ab⎪d\,e⎩g\h⎭  ⎪qr
  ⎪    ⎧k   ⎫ ⎪
  ⎪i\,j⎩l\,m⎭n⎪
  ⎩o\,p       ⎭

```

:::::::::::: Thus after recursive flattening ''(which effectively means collecting all the possibilities that you can get by going from left to right through that diagram)'', you get:
<tt style="margin-left:20em; color:#f00; display:block; background:#F9F9F9">
ab<span style="color:#191">c</span>qr

ab<span style="color:#191">d\,e<span style="color:#00f">f</span></span>qr

ab<span style="color:#191">d\,e<span style="color:#00f">g\h</span></span>qr

ab<span style="color:#191">i\,j<span style="color:#00f">k</span>n</span>qr

ab<span style="color:#191">i\,j<span style="color:#00f">l\,m</span>n</span>qr

ab<span style="color:#191">o\,p</span>qr

</tt>
:::::::::::: Which is exactly what the Perl solution prints. --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 02:07, 1 February 2014 (UTC)
::::::::::: Another datapoint: if you ignore the missing backslashes, bash output is identical to perl's and python's: abcqr abd,efqr abd,eghqr abi,jknqr abi,jl,mnqr abo,pqr. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 02:13, 1 February 2014 (UTC)

:::::::::::: Point taken. Thanks for the clarification. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 03:12, 1 February 2014 (UTC)

== Pls clarify edge case example ==

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}   

shouln't

{}} some }{ cases, {here} \\\\\}  

be a solution? ..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:27, 22 November 2015 (UTC)

: With all those backslashes and the braces without commas, all you're going to see from this algorithm is backslash pairs turning into single backslashes. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:40, 23 November 2015 (UTC)
:: Isn't this '{,{\\{ edge, edge} \,}' a choice between nothing and '{\\{ edge, edge} \,'--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:38, 23 November 2015 (UTC)
:::My current program for this case shows:

```txt

store 3 1 {}} some }{ cases, {here} \\\\\}
store 3 2 {}} some }\\{ edge \,{ cases, {here} \\\\\}
store 3 3 {}} some } edge \,{ cases, {here} \\\\\}

```

:::is thie wrong?  Why?? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:32, 24 November 2015 (UTC)

:::: That comma between ''some'' and ''edge'' is preceded by a brace which still hasn't found its dance partner when the music stops at the end. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:10, 24 November 2015 (UTC)

```txt

:::::{}} some }{,{\\{ edge, edge} \,}{ cases, {here}...
:::::{}} some }{,{\** edge, edge} **}{ cases, {here}...  I did this
:::::{}} some }{,{**{ edge, edge} **}{ cases, {here}...  and should have done that
:::::          | and that's the lonely brace
```

:::::Thanks --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 12:46, 25 November 2015 (UTC)
::::{

Sorry, I am still uncertain.
The results show:

```txt

{}} some }{,{\\ edge \,}{ cases, {here} \\\\\} 

```

Why is the rightmost pair of braces not expanded into its 2 elements?

```txt

                         111111
                                2222222222222
i.e.                    ' cases'
                               ' {here} \\\\\'

```

My result would be

```txt

{}} some }{,{\\ edge \,} cases        
{}} some }{,{\\ edge \,} {here} \\\\\ 
{}} some }{,{\\ edge \,} cases        
{}} some }{,{\\ edge \,} {here} \\\\\ 

```

..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 14:52, 25 July 2016 (UTC)

: The right-most brace is escaped by a backslash. (<code>\\\\\}</code> = <code>\\  \\  \}</code>) --[[User:Smls|Smls]] ([[User talk:Smls|talk]]) 18:01, 25 July 2016 (UTC)

:: Thanks. removed my wrong understanding --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:49, 26 July 2016 (UTC)
