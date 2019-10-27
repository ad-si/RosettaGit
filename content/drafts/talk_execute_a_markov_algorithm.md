+++
title = "Talk:Execute a Markov algorithm"
description = ""
date = 2016-08-01T12:38:10Z
aliases = []
[extra]
id = 5224
[taxonomies]
categories = []
tags = []
+++

==Rules re-write needed==
The example rules should be in the format of the WP article, as they are hard to follow as given. (Is their any reason not to use one of the rule sets worked out from the WP article)? --[[User:Paddy3118|Paddy3118]] 07:55, 15 December 2009 (UTC)

:I put it in Extended BNF notation which is more precise than the format in the WP article, and it is a standard for representing grammars. There's only two main differences: the WP version has quotes, which aren't really necessary; and this one allows for comments using #. --[[User:Rob.s.brit|Rob.s.brit]] 16:21, 15 December 2009 (UTC)
: (I originally wrote this early this morning, but I didn't notice an edit conflict with the J talk addition before I went to work.) While EBNF is pretty common (and interesting on its own), I can't help but wonder if the parsing of the Markov rules should be distinct pieces of funtionality.  I.e. some [[Parse EBNF]], and a [[Execute Markov Algorithm]].  The particulars of parsing EBNF seem irrelevant to Markov chains themselves. While there are broad tasks on RC (such as [[RCRPG]] and a few interpreters), they're usually not created until after the individual concepts have places elsewhere. --[[User:Short Circuit|Michael Mol]] 08:17, 16 December 2009 (UTC)

:: I also changed the '''rulesets'''   (from the existing faux language entries)   into (bold) section headers into the (true) task preamble.   This fixed the "dual" numbering that was in effect, and also placed the TOC (table of contents) into the correct location.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:41, 20 July 2016 (UTC)

==J==

###  explicit vs tacit 


Returning to this just now, I realized from the WP description (and its [[wp:Talk:Markov algorithm#Examples|talk page]]) that the Markov Algorithm has a very strict order of evaluation -- much stricter than I originally considered.  In particular, the entire ruleset must be applied after every successful match.  

That means, for example, if rule 0 matches, then make '''one and only one''' substitution (to the leftmost match) and '''reapply''' rule 0 until it no longer matches.  Then apply rule 1.  If that matches, then start all over and apply rule 0.  And so on.  At any point, if a terminating rule matches, make the single substitution and quit.

I haven't read the other implementations yet, but at least for the J it means we must take a different approach.  In particular, we cannot use <code>stringreplace</code> because it replaces all matches (for even a single rule) simultaneously.  We must be more granular and iterative.

So I suggest we use an explicit <code>while.</code> loop, as distasteful as that is'''*'''.  Maybe something along the following lines: 
```j
markovLexer  =:  verb define
	rules =.  LF cut TAB&=`(,:&' ')}y
	rules =.  a: -.~ (dltb@:{.~ i:&'#')&.> rules
	rules =.  0 _1 {"1 '\s+->\s+' (rxmatch rxcut ])S:0 rules

	NB.  Determine which rules are terminating
	(,. ] (}.&.>~ ,. ]) ('.'={.)&.>)/ |: rules
)


replace      =:  dyad define
	'index patternLength replacement'=.  x
	'head tail' =.  index split y

	head, replacement, patternLength }. tail
)

matches      =:  E. i. 1:

markovStrict  =:  dyad define
	rules =.  markovLexer x

	ruleIdx =. 0

	while. ruleIdx < #rules do.
		'pattern replacement terminating' =. ruleIdx { rules

		if. (#y) > index =. pattern matches y do.
		
			y =. (index ; (#pattern) ; replacement) replace y
	
			if. terminating do.
			NB.  If terminating rule, just return current string
				ruleIdx =. #rules 
			else.
			NB.  Else reevaluate from the beginning after every match
				ruleIdx =. 0
			end.
		else.
			ruleIdx =. 1 + ruleIdx
		end.
	end.

	y
)
```


Of course, this can be translated fairly mechanically to tacit code:
```j
markovStrictT      =:  [: finalize markovLexer@:[ evalMarkov^:( noMoreRules )^:_ initialize
  initialize       =.  0 ; ]                            NB.  First rule is 0 obviously
  finalize         =.  >@:{:                            NB.  Don't need ruleNo when finished
  noMoreRules      =.  >@:{.@:] < #@:[                  NB.  Done when rule index = # of rules
  evalMarkov       =.  ruleNo  nextRule`applyRule@.ruleMatched checkRule
    nextRule       =.  >:@:[ ; {.@:]
    ruleMatched    =.  [: (>~ #)~&>/ 2 {. ]
    applyRule      =.  (terminated ; replaceArg replace >@:{.)@:]
      terminated   =.  _ * >@:{:@:]                     NB.  (_ * terminated) = _ when terminated, 0 otherwise
      replaceArg   =.  1&{ , #&.>@:(2&{) , 3&{          NB.  index ; (#pattern) ; replacement
      replace      =.  2 ;@:A. >@:{:@:[ ; (0 , 1 {:: [) }.&.> (split~ >@:{.)~
    checkRule      =.  rule match txt
      txt          =.  1 {:: ]
      rule         =.  {~ >@:{.
      match        =.  ] ; (>@:{.@:[ index ]) ; [       NB.  txt ; index ; pattern ; replacement ; terminating
        index      =.  E. i. 1:
      ruleNo       =.  0 {:: ]
  markovLexer      =.  (,. ] (}.&.>~ ,. ]) ('.'={.)&.>)/@:|:@:lexed@:normal@:lines 
    lines          =.  LF cut TAB&=`(,:&' ')}
    normal         =.  a: -.~ (dltb@:{.~ i:&'#')&.> 
    lexed          =.  0 _1 {"1 '\s+->\s+' (rxmatch rxcut ])S:0 ]
```


But I do not think that the tacit can claim any advantage over the explicit, in this case.  Unless someone can come up with a clearer way of expressing the algorithm tacitly (maying using a different approach).

Comments?

--[[User:DanBron|DanBron]] 00:18, 16 December 2009 (UTC)

:'''<nowiki>*</nowiki>'''  I agree with "Physis" on WP who thinks the Markov Algorithm specification is ill considered, and who probably came to that conclusion the same way -- by having to bend over backwards in an implementation to accommodate its spec -- fighting the natural expression.

:I was new to this Markov stuff and just took it as read  - that is the spec, now implement it. I did enjoy the the task as getting the implementation right made me use the ''else'' clause of the Python ''for'' statement, which I hardly ever do.

:I guess, on reflection, the logic in the ''replace'' function of the Python example is not straight-forward, but at the time, I just took the spec as being correct, I have no reason to question it? --[[User:Paddy3118|Paddy3118]] 05:03, 16 December 2009 (UTC)
:: So did I, until I re-read the WP page, and in particular Physis' "Example" section on the Talk page.  The primary surprise (for me) is that you may only do one replacement at a time, then you must start all over from the beginning, until you hit a terminating rule or you stop doing replacements (i.e. the output is stable). 
::So, e.g., if you have the rules <code>baa->def, a->b</code>, then applied to the input <code>aaa</code> your result must be <code>def</code>.  This was an eye-opener for me, because I would expect the result <code>bbb</code>.  But then, J is an array-oriented language and likes to do things "all at once" or "in bulk", so maybe this single-match iterative approach is natural to scalar languages. --[[User:DanBron|DanBron]] 12:48, 16 December 2009 (UTC)
::: Would the all-at-once version of the language still be Turing-complete? --[[User:Ce|Ce]] 17:49, 16 December 2009 (UTC)
::::Do you mean the all-at-once version of the Markov algorithm written in J, or the all-at-once flavor of J?  Certainly the all-at-once flavor of J is Turing complete (a well-known Jer implemented a Turing machine in it).  I don't know about the Markov algorithm, but I suspect it is TC, because as Paddy points out, Markov is just a constricted term rewriter (which are Turing complete in general).
::::: The all-at-once Markov algorithm (I assumed that J is Turing complete, because general-purpose programming languages usually are). I've now figured out the answer myself: It is. Indeed it is quite easy to write a Turing machine in it (it made me catch a bug in my C++ implementation, though). It operates the same with the all-at-once and the first-match-only version because it only operates at the head position, and there's only one head. --[[User:Ce|Ce]] 23:17, 17 December 2009 (UTC)
:::: While it might well be TC, it would also be a different algorithm; you'd have to write things differently. Solve this problem, and then maybe write the other one up as another <nowiki>{{task}}</nowiki>... –[[User:Dkf|Donal Fellows]] 10:26, 18 December 2009 (UTC)


### Markov a specific form of Rewriting scheme?

I got lost in [http://planetmath.org/encyclopedia/MarkovAlgorithm.html this], and [http://planetmath.org/encyclopedia/MarkovAlgorithm.html this], and [[wp:rewriting|this]]. After a surfaced it seemed to me that the Markov is a specific case of more general [[wp:Rewriting|rewriting schemes]], so maybe you have been expecting the more general case? --[[User:Paddy3118|Paddy3118]] 13:07, 16 December 2009 (UTC)


###  original version 


<s>I just slapped together the [[Markov Algorithm#J|J solution]] in 20 seconds, so it doesn't fully implement a Markov evaluator.  In particular, it doesn't handle terminating rules; there may be other small wrinkles, but that depends on how one interprets the (underspecified) BNF grammar.  All would be trivial to fix, but I don't have time now.  Anyway, here are the gaps I think of:

#  Terminating rules are not handled.  The fix is easy but would involve modification or duplication of a standard function, [http://www.jsoftware.com/trac/base/browser/branches/cdb/main/main/strings.ijs?rev=418#L239 stringreplace].
#  Similarly, according to [[wp:Markov algorithm|WP]], <blockquote> if [a rule is matched], replace the '''leftmost''' matching text in the input string with its replacement in the first applicable rule.</blockquote> But stringreplace will replace '''all''' instances of matches at once.  Again, a trivial fix as long as we're willing to reprise stringreplace.
#  <s>The standard function [http://www.jsoftware.com/trac/base/browser/tags/j601c/api/regex/regex.ijs?rev=322&order=date#L130 rxcut] will cut on all <code>\s+->\s+</code> though the grammar calls for ignoring any of these after the first (so the replacement string may carry a <code>-></code>).  Fix is trivial, where we'd probably use <code>rxE</code>instead. Silly, we can just use <code>rxmatch</code> rather than <code>rxmatch'''es'''</code>. --[[User:DanBron|DanBron]] 17:47, 15 December 2009 (UTC)
#  Note also that leading blanks in the rule and trailing blanks in the replacement are removed -- the grammar doesn't call for this. Very trivial fix.

I just realized it took me longer to write up the description of these gaps than it would've taken me to fix them. :P  --[[User:DanBron|DanBron]] 16:27, 15 December 2009 (UTC)</s> 

The above comments applied to the original version 
```j
require'strings regex'
 
NB.  Lex a Markov program   
markovLexer =:  verb define
	rules  =.  LF cut TAB&=`(,:&' ')}y
	rules  =.  a: -.~ (dltb@:{.~ i:&'#')&.> rules
	0 _1 {"1 '\s+->\s+' (rxmatch rxcut ])S:0 rules
)
 
NB.  Given ruleset and target string, output
NB.  result.
markov      =:  markovLexer@[ stringreplace^:_ ] 
 
NB.  Same as above, but output all intermediate 
NB.  evaluations
markovDebug =:  markovLexer@[ stringreplace^:a: ]
```
 ... which has since been replaced.

== Multiplication ==

To show the power of a Markov Algorithm engine, I've included a sample testcase that performs unary multiplication. If you can do this, you've got a good implementation. (I'm not sure if this indicates that such engines are [[wp:Turing completeness|turing complete]], but I suspect it does.) –[[User:Dkf|Donal Fellows]] 09:51, 17 December 2009 (UTC)

== lede too long ==

The task description (everything prior to the TOC) is long enough to be distracting -- if I didn't know RC's conventions, I might never scroll past it all to see the implementations.  Can we somehow wrap the lede around the TOC? Or put the rulesets on a subpage and link to them?  Or have fewer rulesets?  I'm actually surprised we need so many; though annoying, the implementation is trivial, and the (distasteful) while loop I tossed together passed all the tests without modification (well, I keep modifying it for aesthetic reasons, but it hasn't changed functionally). Can we just keep the "hardest" or "most comprehensive" one?

--[[User:DanBron|DanBron]] 14:33, 18 December 2009 (UTC)

:Easiest and hardest ones? --[[User:Paddy3118|Paddy3118]] 14:52, 18 December 2009 (UTC)
::I was thinking the hardest one, because doing that implies you can do all the rest.  The full "test suite" could be linked to as a sub page. -[[User:DanBron|DanBron]] 18:48, 18 December 2009 (UTC)
:I think that should solve the problem... --[[User:Short Circuit|Michael Mol]] 19:38, 18 December 2009 (UTC)
:: This leads leads me to suspect we should [[Village Pump:Task Layout and Style|start discussing task layout and style]]. --[[User:Short Circuit|Michael Mol]] 19:41, 18 December 2009 (UTC)

== Is the pattern greedy or conservative? ==
In the rule:
 foo -> -> bar
Is the pattern <code>foo -></code>, and the replacement <code>bar</code>; or is the pattern <code>foo</code>, and the replacement <code>-> bar</code>? 

What should be the right answer, and does the BNF express it? --[[User:Paddy3118|Paddy3118]] 22:30, 18 December 2009 (UTC)
: You'd think BNF would consistently be either minimal or greedy, but I couldn't find anything with Wikipedia or Google. I guess we just shouldn't consider that case. There are a number of related flaws in the ruleset syntax; there's no way to end a pattern with whitespace, for example. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 17:56, 20 December 2009 (UTC)
:: I wouldn't call this a “production” syntax. One could upgrade it by putting double quotes around each half (making it more like the syntax used on wikipedia) but then you'd have to worry about how to embed a double quote in the string. There's also no way to specify a rule that only matches at the start and/or the end of the string. All in all, a mess. But there's enough there to show of the general power of this class of string replacement algorithm, and none of the examples need the problematic sequence, so perhaps it is best to just leave it ambiguous. (I just don't view the code to parse the ruleset as being the most important part of this task.) –[[User:Dkf|Donal Fellows]] 07:51, 21 December 2009 (UTC)

::: +1 on leaving as-is. --[[User:Paddy3118|Paddy3118]] 08:47, 21 December 2009 (UTC)

== Task addition ==

<strike>Paddy3118</strike> today added to the task description:
: In order to promote flexibility, the interpreter should load the set of rules from one file, take the string to operate on from a second file, and write the output to a third.
IMHO that's the antithesis of flexibility. You can redirect or pipe standard input/standard output. You can't easily do that with files. --[[User:Ce|Ce]] 22:32, 18 December 2009 (UTC)

:Hi Ce, I didn't make the addition to the task description. In fact, I too thought it was less flexible so wrote the following in the Python example:
::"The example gains flexibility by not being tied to specific files. The functions may be imported into other programs which then can provide textual input from their sources without the need to pass 'file handles' around."
:--[[User:Paddy3118|Paddy3118]] 05:09, 19 December 2009 (UTC)

:: Oops, sorry for the mis-attribution. I hadn't noticed the sentence before and your latest edit had a change comment about flexibility, so I mistakenly thought you had added it. I should have checked the history more carefully. --[[User:Ce|Ce]] 09:03, 19 December 2009 (UTC)

: Yeah, I completely ignored that rule when I wrote the Perl and Haskell implementations. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 18:15, 19 December 2009 (UTC)
::  I excised the rule.  If anyone wants to put it back, we can discuss that here, first. --[[User:DanBron|DanBron]] 18:41, 19 December 2009 (UTC)
::: I wrote that bit as part of an effort to characterize the original implementation and build up the task description. (The task description was ''very'' rough to start out with!) I still think it makes sense to load the rulesets from an external file. After all, they're a program (a nearly pure turing machine) and they can get rather long, and it's useful to load from a file as this encourages separation of the interpretation engine from ruleset itself. –[[User:Dkf|Donal Fellows]] 09:35, 20 December 2009 (UTC)
:::: Plus the fact that the ruleset definition includes a comment syntax (which I didn't write, just tidied up) is highly indicative that it was originally intended to be file-hosted. (I'm not sorry to lose the requirement for input and output data though.) –[[User:Dkf|Donal Fellows]] 09:37, 20 December 2009 (UTC)
:::::Hi Donal, on some operating systems, like flavours of Unix, stipluating the data source to be a file makes it very flexible, as most things are made to look like a file by the OS: ftp, HTTP, pipes, ... if your program aaccepts files, then all is well. This may not be the case in other operating systems. I think it is OK to treat the source of the data as a separate concern that is not part of the task. --[[User:Paddy3118|Paddy3118]] 23:12, 20 December 2009 (UTC)
:::::: I think I'm going to hold out for at least the ruleset coming from another file. The general concept carries over pretty well into all other operating environments that I'm aware of, and even when the environment doesn't support files ''per se'', there's something that can be used instead; requiring that all information be embedded into the source code of the program isn't exactly practical on any of them. (The separation is also very convenient when writing rulesets; it prevents silly accidents in editing and keeps the language's string syntax out of the way.) –[[User:Dkf|Donal Fellows]] 07:23, 21 December 2009 (UTC)

::::::: I would rather we left where the rulesets and example text origins unspecified, as a quite legitimate way of coding this in Python is as a module for incorporation into a larger program. 
In this case, we might arrange for the module to test itself when run stand-alone and provide the function when imported. Even reading the rulesets is tertiary to the main aims of performing the function, and adding some tests. --[[User:Paddy3118|Paddy3118]] 08:55, 21 December 2009 (UTC)
:::The old Unix way would be to have a couple of flags -f rule_file, and optionally -o output_file. The input file would be any additional files specified on the command line or, if none were specified, from stdin.  The output would be to stdout unles the -o option was specified. 
:::examples:
::::<code>process1 | markov -f myRules | process2</code>
::::<code>markov -f myRules -o myOut.txt myInput.txt myInput2.txt myInput3.txt</code>
::::<code>markov -f myRules < myInput.txt > myOutput.txt</code>
:::--[[User:Rldrenth|Rldrenth]] 00:25, 21 December 2009 (UTC)
:::: The traditional Unix way would have omitted the <code>-o</code> option, and quite possibly the support for data input files. After all, there's redirection available and you could use <code>cat</code> to build the input. However, the rules file would still not be optional (unless the command also supports a <code>-e ''ruleset''</code> option) and the use of <code>-f</code> before it would be optional (and only useful when you feel the need to begin the filename with a hyphen). '''But''' this level of complexity is really best of not being put in this task; it's total distraction!
:::: And in any case, I used <code>/proc/self/fd/'''['''01''']'''</code> when testing. There's ''always'' a way… –[[User:Dkf|Donal Fellows]] 07:23, 21 December 2009 (UTC)

== Files to download ==

To the description of this task I suggest to add links to two files, like markov_rules.txt and markov_tests.txt to dowload. No need to copy and paste some text from the page, with a risk of mistakes.

==task extension==

I think this task (since it is still in draft status) should be extended to include the 2<sup>nd</sup> example in the Wiki entry, namely:
<pre style="overflow:scroll">
# Rewrite binary numbers to their unary value (| bars).
# I.E.:   101   [base 2]   will be converted to 5 bars.
#──────────────────────────────────────────────────────
|0 -> 0||
1  -> 0|
0  ->

```


Using this simple Markov test can reveal a glaring program error (as it did for my REXX example), the first five entries executed correctly, but choked on this one. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:39, 14 November 2012 (UTC)

:The task isn't in draft? (And has a good number of solutions already). --[[User:Paddy3118|Paddy3118]] 04:19, 15 November 2012 (UTC)

==alternate spelling of source==
The spelling of   '''sourse'''   (a version heading in the '''Racket''' example: ) was changed today by someone who thought that it was a typo (which may indeed be the case),   '''sourse'''   is the old spelling of   '''source'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:15, 1 May 2013 (UTC)

==Inconsistent/invalid examples?==
The rule for <rule> seems to indicate NO <whitespace> in a pattern but that isn't consistent with the examples. If a pattern can contain <whitespace>, then "->.* -> money" conflicts with "the shop -> my brother" because "<pattern> <whitespace> -> <whitespace>" is going to break on one or the other when trying to find <pattern> ("->.*"/"the" or ""/"the shop"???).
: <pattern> doesn't seem to be defined. It can definitely contain whitespace. The regex engine will notice that the whitespace after "the" isn't followed by an arrow and will include it in the capture, and continue to look for a whitespace followed by an arrow further down the line. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 22:34, 1 June 2014 (UTC)
::You are correct, <pattern> isn't defined, and it took me several tries to figure out a definition due to the ambiguities. Which was annoying. Your RE engine may work that way, mine doesn't.
:::<pattern> ::= <any char>+
:::<replacement> ::= <pattern>
:::<rtoken> ::= first <1whitespace> -> <1whitespace>
:::<rule> ::= <pattern> <rtoken> [.] <replacement>
::This also clarifies that "foo ->" is an invalid <rule> but introduces other ambiguities.

==Dictionary==

### Python

Wouldn't it be simpler to use a Python dictionary object to implement the rules? For example the first set of rules would be implemented like this:


```txt
grammar = {'A': 'apple', 'B': 'bag', 'S': 'shop', 'T': 'the', 'the shop': 'my brother' }
```


I tried this with CoffeeScript and it yields a rather short code, which may be even shortened with the use of Python's ''iter'' method for the dictionary (the main difficulty being that only the first working rule has to be applied).

[[User:AlainBusser|AlainBusser]] ([[User talk:AlainBusser|talk]]) 04:17, 20 July 2016 (UTC)

:Hi Alain, There is a difference in the approaches in that the Python way leverages the rigorousness in the spec and makes at least that part of the spec directly executable. In what can be meticulous industries of Science and Engineering when given an executable spec it is good to automate its use in the most direct way, because as the implementation of the spec firms one can worry more on the spec itself rather than a continuing human step of spec translation which is more likely to waver, (due to job changes, holidays, alcohol abuse, ... for example :-)

:I guess people decide for themselves which is "simpler". Does my solution reflect badly on the Python language in comparison with others? I don't think so. For example: One criticism of Python is in how it supports regular expressions - Ruby and Perl have direct syntactical support for regular expressions; Python has the re library and r-strings. The Python example shows that despite being different, Python still has excellent support for regular expressions including the ability to use multi-line regexps with named groups.

:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:58, 31 July 2016 (UTC)

::Hi, I wouldn't say your solution reflects badly at all, if only because you used comprehensions which are usually a very elegant way of programming.

::Besides, I found that the use of a dictionary would not simplify much the code, as once created, the dictionary has keys which are ordered alphabetically and not in the order they were introduced. So the iteration on these keys does not usually give the intended behavior: After all the use of a dictionary would not be "simpler" as I thought it would.

: And this is true for CoffeeScript too: I still have to find how to program it yet...

[[User:AlainBusser|AlainBusser]] ([[User talk:AlainBusser|talk]]) 12:38, 1 August 2016 (UTC)
