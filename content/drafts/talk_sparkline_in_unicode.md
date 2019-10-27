+++
title = "Talk:Sparkline in unicode"
description = ""
date = 2019-03-14T18:30:21Z
aliases = []
[extra]
id = 14280
[taxonomies]
categories = []
tags = []
+++

==Most of these are buggy==

;The wrong way to compute the character index

Anything that uses the number 7 (<code>bins-1</code> etc.) in the binning assignment has too-wide bin sizes.  The two most common manifestations of the bug are:

* when the quotient is truncated (<code>floor</code>/<code>ceil</code>/<code>int</code>), the first or last bin will be one value wide.
* when the quotient is rounded, the widths of the first and last bin are too small by half. 

;The right way to compute the character index

The Go code uses <code>int( 8 * (v-min) / (max-min) )</code> which works in all cases except when <code>v==max</code>; it deals with that case by clamping values larger than 7 to 7 (for a zero-based array).

The Tcl code gets honorable mention for using <code>int( 8 * (v-min) / (max-min)*1.01 )</code>, which mostly does the same thing as the Go code. It avoids the need for clamping but gives bins that are 1% too wide, which becomes visible when the range is large.  This approach works if the multiplier is larger than 1, smaller than <code>1 + 1/(max-min)</code>, and large enough to not get overwhelmed by floating-point imprecision.

;Test cases that detect bugs

* <code>0 1 19 20</code> detects the one-wide bug.  Output should be the same as <code>0 0 1 1</code> with exactly two heights.  The bug looks like ▁▂██ or ▁▁▇█
* <code>0 999 4000 4999 7000 7999</code> detects the half-width bug and some smaller errors (see Tcl).  Output should have three heights; the half-width bug looks like: ▁▂▅▅▇█

: '''Addendum:''' ''the second test case assumes that each of the 8 heights should represent 1/8<sup>th</sup> of the range, as closely as possible.  Not everyone agrees.  See [[#Counterpoint]] and [[#Deeper_root_of_the_.27bug.27_.3F|Deeper root of the bug?]] below for discussion.''

;sparktest.pl

This is some Perl code that will report the widths of same-height sections of output, when provided with a sparkline on standard input. Non-sparkline-lines are ignored.  The line produced from a continuous integer sequence should produce eight equal widths (or almost equal if the sequence length is not a multiple of eight).

<code> perl -CS -Mutf8 -nle '@x=grep $i^=1, map length, /(([▁-█])\2*)/g and print"@x"' </code>

Sample usage (in bash, and assuming program accepts space-separated data on standard input): 

```txt

alias sparktest=$'perl -CS -Mutf8 -nle \'@x=grep $i^=1, map length, /(([▁-█])\\2*)/g and print"@x"\''
echo {1..8000} | sparkline | sparktest
# expected output is 1000 1000 1000 1000 1000 1000 1000 1000

```


;Not Buggy

* [[Sparkline_in_unicode#Go|Go]].  Tested up to <code>echo {1..12345} | go run sl.go | sparktest</code>

;Buggy

* C: ▁▂██
* C++: ▁▁▇█
* Clojure: ▁▂▅▅▇█
* Common Lisp: ▁▂▅▅▇█
* D: obvious one-wide bug; didn't run the code
* Elixir: ▁▂▅▅▇█
* Groovy: one-wide; didn't run
* Haskell: (both versions normalized)
* Java: one-wide; didn't run
* Javascript: (normalized)
* jq: one-wide and neglects to check bounds: ▁▃▷►
* Nim: Python translation
* fixed! <s>Perl</s>: ▁▁▇█
* fixed! <s>Perl 6</s>: ▁▁▇█
* PicoLisp: ▁▂▅▅▇█
* Python: (both versions normalized) 
* Ruby: ▁▁▇█
* Rust: thread 'main' panicked at 'attempt to subtract with overflow', sl.rust:8:40
* Tcl: ▁▁▄▅▇█; not a half-width bug (the second character is correct); manifests only on large ranges; see comments above.


... that's 15 tested, 14 failures, plus 5 didn't-runs that almost certainly have the bug.
:--[[User:Oopsiedaisy|Oopsiedaisy]] ([[User talk:Oopsiedaisy|talk]]) 08:24, 24 February 2019 (UTC)

:Good point. Fixed in the Perl 6 example (and another bug I found while I was at it.) This highlights the perils of developing without a decent test suite. On the other hand, Rosettacode makes no claims about the quality of the code on the site. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:42, 24 February 2019 (UTC)

:It's the nature of the beast that an early bug may be faithfully translated many times. I agree that a buggy solution has value despite its flaws.  I fixed the Perl example.  --[[User:Oopsiedaisy|Oopsiedaisy]] ([[User talk:Oopsiedaisy|talk]]) 15:19, 24 February 2019 (UTC)

::Thanks Oopsiedaisy. I started the task off with an initial buggy Python solution. Now fixed and with examples extended to show your problem cases. Thanks again. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:35, 24 February 2019 (UTC)



### =Counterpoint=

:: A very helpful intervention and discussion, and I agree absolutely about the first test example.
::
:: Perhaps our interpretation of the '''second''' test example depends on some unclarified assumptions about the optimal width (and alignment) of the bins ? 
:: The Haskell '''Statistics.Sample.Histogram''' library, for example, returns the following allocation of the sample <code>0 999 4000 4999 7000 7999</code> to 8 evenly sized bins:
:: <code>[1,1,0,0,2,0,1,1]</code>
:: which would, I think, correspond to 5 different sparkline heights, unless I am confusing myself.
:: The set of lower bounds suggested by '''Statistics.Sample.Histogram''' for a division of this sample between 8 bins is:
:: <code>[-571.3571428571429,571.3571428571429,1714.0714285714287,2856.7857142857147,3999.5,5142.214285714286,6284.928571428572,7427.642857142857]</code>
:: The assumption they are making is that any given sample is likely to be drawn from a slightly larger range of possible sample values, and that some margin can usefully be allowed.
:: The margin which that library adopts is <code>margin = (hi - lo) / (fromIntegral (intBins - 1) * 2))</code> 
:: (yielding fractionally larger bins and a total range that starts a little below the minimum observed value, and ends a little above the maximum observed value)
:: Arguably reasonable for us to do something comparable ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:26, 26 February 2019 (UTC)
::: PS the dependence of edge cases on mutable assumptions (e.g. the relationship between the range of the sample and the range of possible/graphed values) may be underscored by the result given by the '''Mathematica 11 Histogram function''', which (if we specify only a target number of bins) allocates the same sample as follows (different pattern again, but still, I think, 5 sparkline levels):
:::: <code>Histogram[{0, 999, 4000, 4999, 7000, 7999}, {"Raw", 8}] --> </code>
:::: [2, 0, 0, 1, 1, 0, 1, 1] 
::::
:::: And similarly the '''R language hist() function''' expression <code>hist(c(0, 999, 4000, 4999, 7000, 7999), breaks=8)</code>
:::: Returns a distribution of 5 [2, 0, 0, 1, 1, 0, 1, 1], again using 5 (rather than 3) of 8 available bins.
:::: The breaks which it derives from that data set can be listed:
:::: <code> > histinfo<-hist(c(0, 999, 4000, 4999, 7000, 7999), breaks=8)</code>
:::: <code> > histinfo</code>
:::: <code>$breaks</code>
:::: <code>[1]    0 1000 2000 3000 4000 5000 6000 7000 8000</code>
::::[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:33, 26 February 2019 (UTC)

::::: "fractionally larger bins" is the Tcl approach I discussed in the section above.  It's fine but requires careful selection of the denominator.  Too big, and the bins are wider than they need to be (Tcl's mistake); too small, and it can be erased by fp errors. 

::::: edit: the relationship between the value of <code>breaks</code> and the number of bins in R is completely opaque and does not match the documentation.  For example, <code>hist(0:9, breaks=x)</code>  gives 2 bins for x=3; 5 bins for x=4,5,6; 9 bins for x=7.

::::: edit2: I should clarify that Haskell's solution exhibits the half-width bug.   I don't believe this is defensible.  Much better choices of denominator are available.  --Oopsiedaisy, 26 February 2019

==Deeper root of the 'bug' ?==

I think the essential bug may be our (understandable, but formally impossible) hope that the range of the sparkline can be the same as the range of the data.

It sounds reasonable (even optimal – we want to waste as little chart-space as possible) but on reflection it turns out to be a self-contradiction, and an inevitable source of these confusions and problems.

We know what happens to a data point that falls '''between''' two bin edges, it goes into the bin. 
But what about a data point that is equal to the value of a bin edge ? Does it stay there, stuck in a superposition between two bins ?

No - we define either one of two possible rules - e.g. all data points that match a boundary fall into the '''lower''' of two bins.
Or instead, all data points that match a boundary fall into the '''higher''' of two bins.

That's fine until we kid ourselves that our lowest data point can be the lower boundary of the lowest bin, while the upper data point is '''also''' the upper boundary of the highest bin.

A sparkline that has these properties and also contains all the points is a contradiction in terms. Either our '''lowest''' data point is '''not in the diagram''' (it falls '''below''' the lowest bin, by one of the possible two rules) '''or''' our '''highest''' data point is '''not in the diagram''' (it falls '''above''' the highest bin, by the other of two possible rules).

The range of the diagram can be close to that of the data, but it '''can't''' be co-extensive. It '''has''' to be bigger if it is not going to exclude a minimum or maximum data point.

How much bigger ?  Well the margin can be an epsilon, but it can't be zero without losing at least one data point from the sparkline. As the task description doesn't define the size of this non-zero margin (and its precise dimension determines the position of '''all the bin breaks''' and the precise size of all the bins), I '''don't''' think that we can really give a test case like test 2 above, and define what the output should be.  It isn't yet determinate ... To define the expected output, we would have to define the difference between the range of the sparkline and the range of the data, 

We would also have to define whether there was a margin at both ends of the scale or only at one end.

If we try to fudge it with a special case for points that should formally have been dropped off one end or other of the scale, then we are simply saying that we do know that the range of the diagram is really bigger than that of the data, but we don't want to define by how much. That means that the precise size of each bin, and the precise location of all the edges, is also undefined. All we really know about them is that they are '''definitely not''' quite where we have drawn them :-)

We simply haven't clarified the terms of the task to the point where a single correct output is defined for edge cases. 

[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:21, 26 February 2019 (UTC)

:: > I '''don't''' think that we can really give a test case like test 2 above, and define what the output should be.

: We surely can.  The range has 8000 sequential integers and there are 8 bins.  Each bin should contain 1000 of the integers.  This bug even has a name: ''fencepost error''.  

: ''NB:  To avoid confusion, I'm going to discuss this in the context of data that are a sequence of consecutive integers.  The same reasoning can be applied to floats if you imagine them as integers that are all divided by some common denominator.  Which is exactly what they are.  Discontinuous or unordered data doesn't affect the math at all; it's just easier to think about when <code>n==max-min+1</code>.''

: <code>max-min</code> is one smaller than the number of integers in the range; the mathematically correct bin assignment is simply <code>int( bins * (v-min) / (max-min+1) )</code>.   The <code>+1</code> is a fencepost; the formula yields a non-negative integer smaller than the number of bins.  

: There are two problems with this "correct" approach:

:: 1. Real-life floating-point division is imperfect.  The division may return 1, which would give an out-of-bounds index.  

:: 2. If our data is ''not'' integers, the size of the fencepost is not <code>1</code>.  Determining its actual size might get complicated.

: The suggested solution&mdash;<code>max-min</code> denominator with clamping&mdash;is simpler and more robust.  It does not generalize to arbitrary binning problems, because it makes the last bin too wide. For our purposes, though, it's as correct as anything else: the "error" is maximized when <code>n==9</code> and it's clear that there's no way to avoid having seven bins with one value each, plus one bin with two values.  

: Good arguments may exist for the necessarily-larger bin to be inconsistently decided, or for it to be any other than the first or last bin, but I can't think of any.

:: -- [[User:Oopsiedaisy|Oopsiedaisy]], 26 February 2019 (UTC)

> The range has 8000 sequential integers and there are 8 bins.  Each bin should contain 1000 of the integers.

:: Why ? That would, of course, be a perfectly reasonable distribution, but which part of the task description requires it ?
:: We know the observed data range, but the range from which this data is drawn may be larger, and the task description doesn't exclude a sparkline which allows some margin, whether small or large.
:: It doesn't, for example, exclude the equally reasonable distribution made by both '''Mathematica''' and '''R''', which, very sensibly and helpfully, both represent the observed values as drawn from a possible range of 0-8000. 
:: A significantly stronger horizontal compression of the data within the sparkline display would also be legitimate, without any contradiction of the task description. (And with the additional benefit of avoiding logical self-contradiction :-)
:: Even the tiny margin chosen, perfectly sensibly, by R and Mathematica, already yields a different set of sparkline levels to those you are showing, without any departure from good sense or the terms of reference.
:: The task does not require us to assume that the lowest value observed is invariably and necessarily identical to the the bottom of the scale, or that the highest value observed is invariably and necessarily  identical to the top of the scale. (Arguably rare that either or both would be the case, for many data sets). [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 01:30, 27 February 2019 (UTC)

::: The bug is that bins are not of equal width or anything close to equal.  How the minimum (or maximum) is chosen is not relevant: the bug affects both implicit and hardcoded boundaries.  You are correct that the second test case assumes implicit boundaries; however you can subtract one from each value, get a zero-based range, and see that the bug persists.  You are also correct that the task description doesn't specify a mapping from numbers to heights, but this objection seems silly and pedantic to me.  We could randomly select a height for each datum and not strictly violate the task description but what use is that?   Usefulness and fidelity dictate that heights are proportional to the data's magnitude (relative to its own extremes, or to reasonable hardcoded values), to the extent possible given the low resolution of the output. -- [[User:Oopsiedaisy|Oopsiedaisy]], 26 February 2019 (UTC)

:::: The problem is simply that while your scheme is perfectly practicable, it is not the only scheme that is reasonable or effective, and in fact the R and Mathematica approach seems marginally better, avoiding the need for special fudges, while yielding visibly different results at the margin.
:::: Given that equally reasonable approaches can yield differing sparkline horizons at the margin, we can't define a unique 'correct result' for the 0-7999 case, and we can't use it to sort buggy from bugless implementations. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:15, 27 February 2019 (UTC)

::::: It's hard to comment on the reasonableness of R's result because I can't figure out what it's doing or how the output of <code>breaks</code> maps to actual breaks.  Output says lower bounds are <code>0 1000 ... 4000 5000 ..</code>, but then 0 and 999 go into one bin, while 4000 and 4999 go into two different bins?  

::::: Speaking generally, if the task is "'''assign 8n items to 8 bins as equitably as possible'''", a solution that doesn't assign n items to each bin is wrong.  It's wrong because a more equitable assignment was possible.   

::::: The Haskell approach is the half-width bug and simply bad: the first and last bins are half the widths of the others for no good reason. --Oopsiedaisy 26 Feb 2019

:::::: Ah ... You though I was defending particular drafts ? No, several things need fixing.
:::::: My point is that while checking and fixing these drafts, we '''can't''' reliably use your 2nd test (0..7999) and claim that there is a single correct output which it should produce.

> if the task is "'''assign 8n items to 8 bins as equitably as possible'''" ...

:: That '''isn't''' the task ! :-) Those words are '''yours''' – they are not from the task description at all ... 
:: It would be perfectly reasonable and understandable for you to aim for that goal, but it '''isn't''' specified by the task, and, as it happens, it actually leads to logical contradictions, and requires a fudge, if taken literally.
:: Many (probably most) charts leave some margin to left and right, allowing for a scale of possible values that is larger than the range of observed values. Perfectly resonable, and it doesn't require all 8 buckets to be equally full.
:: Mathematica and R are graphing the data on the scale 0..8000. That's at least as reasonable as your approach (mathematically a little more simple and coherent too, as it happens), and it would be just as sensible (possibly even more) to follow their lead as yours. Both approaches are fine.
::: The '''point''' is that your presentation of the 0..7999 test as a reliable diagnostic instrument which 'should' yield a unique pattern with only 3 levels is '''misleading'''. It can produce false negatives, and doubtless false positives too. The Mathematica and R approach, equally correctly produces '''5''' of these low resolution levels with that data, and its deliberate clustering around margins.
::: What you are suggesting is cetainly one way,  but '''not''' the only way.
::: The task is '''not''' to fill buckets evenly. The task is to produce a vertically proportional sparkline. Your test does not have a unique correct output. No reason that it should, but if we misrepresent it, we will mislead people [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 07:14, 27 February 2019 (UTC)




:::: > The task is '''not''' to fill buckets evenly. The task is to produce a vertically proportional sparkline. 

:::: To whatever extent the filling is uneven, the vertical axis is non-proportional.  These two tasks are exactly equivalent. 

::::: I disagree (I think you probably will too, on reflection :-)  A chart with unused bins to left and right, representing sections of the scale into which no data fell, is no less proportional in the central bins that '''are''' filled. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:16, 27 February 2019 (UTC)

:::: Mathematica and R's solutions are reasonable because they are solving a different and more general problem.   A crucial difference is that they're making histograms suitable for plotting with a labeled x-axis.  Integer breaks are valuable in that situation; perhaps valuable enough to sacrifice evenness.

:::: In this sparkline problem, integer breaks have no value because the y-axis will never be labeled.  Bins of equal width (or as close to equal as the data permits), provide fidelity that is superior to bins of unequal width.  

:::: There is no contradiction or mathematical incoherence in <code>floor( bins * (v-min) / (max-min+fencepost_size) )</code>.  If someone dislikes the aesthetics of clamping the last value&mdash;which I can understand even if I disagree&mdash;then the optimal solution is implied by this formula.  The margin you refer to will be determined by <code>fencepost_size</code>, which can be slightly oversized but should not be lazily chosen as "distance to the next power of ten," nor "distance to the next whole number" and absolutely definitely not "1/8th of total range size" (like Haskell's stats library does). --Oopsiedaisy, 27 Feb 2019

::::: '''Good'''. We are in agreement on the rationality and mathematical coherence of <code>floor( bins * (v-min) / (max-min+fencepost_size) )</code>.
:::::
::::: As for our options in the derivation of <code>fencepost_size</code>, the task description leaves this entirely open, as the over-lexicalising tone of your '''"absolutely definitely not"''' XYZ... inadvertently confirms :-)

::::: The only thing that remains, is that to avoid misunderstanding and misdirection, I think you '''do''' need to edit your presentation of the test samples above, to clarify that three spark levels for the second sample is diagnostic only of the implementation of '''one particular approach''', and that other equally (arguably more) correct approaches will generate different patterns for those same edge cases.

::::: More than 3 spark levels for test 2 (0..7999) is not inherently diagnostic of a 'bug'.

::::: In fact, it's not really clear that the (mathematically perfectly coherent) TCL version contains any '''bug''' at all, any more than fudging the upper or lower boundary condition necessarily constitutes a '''bug'''. 

::::: At such extremely low resolution, we can easily generate sub-optimal boundary condition artefacts with '''any''' approach, as long as we choose our sample carefully. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:16, 27 February 2019 (UTC)

The applications of this code are not imaginary.  As someone who wants to communicate to an audience, using this visual tool, what does it gain me if <code>0 1 2 3 4 5 6 7</code> looks like ▁▂▃▄▅▆▇█ but the graph deforms as I scale up the data?  Nothing.  You're saying that if I plot a list of percentages, and append <code>0,100</code> to the data to fix the scales, that it's perfectly reasonable for the algorithm to assign <code>-6.25,106.25</code> the left and right boundaries.  It's not.  The assigned numbers are mathematically meaningless in the context of my data.  

> ''derivation of <code>fencepost_size</code>, the task description leaves this entirely open''

The task description is not a contract or rigorous specification.  Properties like usability, fidelity, proportionality are implied.  You're free to disagree but you won't convince me or most other people.  

The sole purpose of <code>fencepost_size</code> is to prevent the formula from returning 8.  In IEEE754 64-bit floats  (used by JavaScript, Perl, and many others) it can be about 15 orders of magnitude smaller than <code>max-min</code> before it fails.  In 32-bit floats, about 7 orders of magnitude.  Using larger values provides no benefits and (eventual) visible drawbacks, therefore larger values should be avoided. 

For comparison, the half-width bug is the rough equivalent of <code>fencepost_size==(max-min)/8</code>.  It's '''14 orders of magnitude''' larger than needed, and it causes visible deformation of the graph.

> ''as the over-lexicalising tone of your '''"absolutely definitely not [the half-width bug]"''' XYZ... inadvertently confirms :-)''

See my comments, a few paragraphs previous, about plotting percentages vs <code>-6.25,106.25</code>.   If you'd like to have a discussion about the algorithmic merits of hypothetical 1/n<sup>th</sup>-width bugs for n>2, okay, but recognizing the half-width bug as a bug is the starting point.  It's an entry requirement.

You've often conflated purposeful selection of zero-based axis/integer breaks (as R does) with the jumbo "margins" produced by bad fencepost sizing (this bug and what Haskell's stats lib does).  They are not the same thing, at all.  The first is defensible (though probably a bad idea in this specific task); the second is not.

>> To whatever extent the filling is uneven, the vertical axis is non-proportional.  These two tasks are exactly equivalent. 

> A chart with unused bins to left and right, representing sections of the scale into which no data fell, is no less proportional in the central bins that '''are''' filled.

This doesn't make any sense.  You say "into which no data fell" as if the data ''could'' fall there.  '''It couldn't'''.  If it does, the boundaries move. The term for "proportional with respect to data that cannot exist" is "disproportionate."   The first and last height represent 1/14th of the range.  The other six heights represent 1/7th of the range each.  They are disproportionate.  

> At such extremely low resolution, we can easily generate sub-optimal boundary condition artefacts with '''any''' approach, as long as we choose our sample carefully. 

I sincerely doubt it.  You have not tested this statement.  You ought to.  I suggest the (current) Perl implementation. --oopsiedaisy

==Bar choices==
Hi Tim. There is a problem with your choices of bars in that they have a ragged bottom line:
: ▁▂▃▄▅▆▇█


There is a problem with my choice of bars in that the highest bar is not full width:
: ▁▂▃▅▆▇▉▇▆▅▃▂▁
I find the ragged baseline to be much more irritating. How to resolve? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:18, 18 June 2013 (UTC)

Oh, my font is Courier. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:42, 18 June 2013 (UTC)

: I find that there's quite wide differences in the quality of fonts when it comes to blocks and box elements; a lot of fonts simply don't have the things that should extend to the limits of the glyph box they declare actually doing so at all. In my limited experimenting, Courier New is considerably better than the others I've tried (Andale Mono, Consolas, Courier, Monaco) for this sort of thing. Not much we can do about that really (except “blame the font makers”, which isn't very helpful). –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 11:24, 20 June 2013 (UTC)

I now find that there is raggedness in the baseline of my bar choice if I swap to Consolas font. I think I'll revert to using Tims seven bars and search for a font as the [http://www.unicode.org/Public/UNIDATA/NamesList.txt Unicode page] has nothing to say on this, just:

```txt
@@	2580	Block Elements	259F
@		Block elements
2580	UPPER HALF BLOCK
2581	LOWER ONE EIGHTH BLOCK
2582	LOWER ONE QUARTER BLOCK
2583	LOWER THREE EIGHTHS BLOCK
2584	LOWER HALF BLOCK
2585	LOWER FIVE EIGHTHS BLOCK
2586	LOWER THREE QUARTERS BLOCK
2587	LOWER SEVEN EIGHTHS BLOCK
```
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 03:56, 18 June 2013 (UTC)
:: The baseline is fine in my terminal font, and the baseline problem only manifests in the browser.  In any case, if the font is problematic, that's the font's problem, not our problem.  Notionally the blocks should have the same baseline, and I'd much rather have a solution that will be correct after they fix the fonts.  (Or fix the font aliasing algorithm, which may be what's really going on here.) --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 07:57, 18 June 2013 (UTC)
:::Yes, it's the font dealiasing that is doing it.  Changing the page's font size up and down moves the fuzz from the bottom to the top, and to different characters. So trying to pick the "right" characters is an exercise in futility, because what's right for you will be wrong for someone else. So just use the eight characters that are supposed to be right, and ignore the baseline issue. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 08:02, 18 June 2013 (UTC)
::::Oh, you already did, nevermind. :) --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 08:10, 18 June 2013 (UTC)

== Python query ==
In the (original) Python entry, obviously some kind of to be or not to be unicode thing, can someone explain the try/except on bar, ta? --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]])

:It [https://stackoverflow.com/questions/21731043/use-of-input-raw-input-in-python-2-and-3 allows] the code to work in both Python 2 and Python 3. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:45, 11 January 2019 (UTC)

::Sorry, I didn't mean the try/except on raw_input, but the one on bar (try: bar = u'▁▂▃▄▅▆▇█' except: bar = '▁▂▃▄▅▆▇█'). Following that link, I am certainly closer to understanding, but still slightly adrift. Is it something to do with u'xx' being ''invalid syntax''' in 3.0 .. 3.2 but accepted/ignored in 3.3+? --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 17:33, 11 January 2019 (UTC)

:::Petelomax: It's true that originally Python 3.x didn't accept the <code>u'...'</code> syntax because normal <code>'...'</code> strings are already Unicode. More recent versions accept the syntax, but the <code>u</code> has no effect. So that might explain the try: there, except that a try/except doesn't do any good for syntax errors. So I'm as puzzled as you are.--[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 19:05, 11 January 2019 (UTC)

::::Ha - I noticed it was quietly removed in the last bug fix, so I assume it was unnecessary/meaningless. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 05:46, 26 February 2019 (UTC)

==A Point Is That Which Has No Part==
Due to fence posts it is claimed above that it is impossible to distribute xMax and xMin into uniform buckets. I don't consider a day worth living if the impossible has not been achieved before breakfast. The title above is accepted widely as the first line of The Elements. I would also draw attention to the Platonic view that a point is a monad with position attached because it is often coded thus, and the Aristotelian view that a point is not the thing but produces the thing by motion because this rules out some of the interpretations above. Sticking with The Elements the second line is 'A line is a breadth less length'. I define the line as the length  xMax-xMin. I shall use nB as the number of buckets (8 in this tasks case). I want to distribute these buckets equally along the line so I find the width of each bucket using division as dX=(xMax-xMin)/nB. I now need a function which allocates each of the numbers in the test sequence to a bucket. I number the buckets placed along the line from 0 to nB-1 and assign each number x to bucket n=floor((x-xMin)/nB). For the first test xMin(=1)->0/8 (=0). xMax(=8)->56/8 (=7). For the second test xMin(=0.5)->0/3 (=0) and xMax(=6.5)->24/3 (=8). So the impossible to devise a scheme including xMin and xMax is achieved and today is a day worth living (assuming a late breakfast or a different time zone).--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:19, 1 March 2019 (UTC)


For those who want to look at the logic more closely the proof of the above comes (better if I could draw a picture) by finding the center (say C or 0) of the line and distributing the buckets either side of C. The above comes by finding the center of which bucket is closest to x from C. If we adjust the scale so that C is 0 then xMin must be negative and xMax must be positive. Some of the confusion above can now be seen to be trying to answer the question is 0 positive or negative. This question has been answered it is neither positive or negative nor either odd or even. What does the suggested algorithm do? C(=4.5)->28/7 (=4) so positive which is usually a good answer.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:36, 1 March 2019 (UTC)
: Does the point turn on whether we are filling classical or quantum buckets ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:03, 1 March 2019 (UTC)
:: Not for this task. Classical is fine. The Aristotelian view is sufficient to explain now (as a point whose motion creates time), so I can say "now if I want to explain the line at the edge of a shadow ..."--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:33, 1 March 2019 (UTC)
::: Thank you for the solution and explanation. (And that's a relief – I was worried that we couldn't machine a fence-width finer than the Planck length, and that a special case at one end or other of the scale was going to be inescapable) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:49, 1 March 2019 (UTC)

==Intervals and binning==
Using the [[wp:Interval_(mathematics)#Notations_for_intervals|notation]] where round parenthisis, <code>( or )</code> is used to '''exclude''' an endpoint and square parenthisis, <code>[ or ]</code> is used to '''include''' it.

We can start with a range of numbers: <code>[min, max]</code> all the numbers fall in the interval.

We split into several contiguos bins, and for Python at least, there is the tradition of including the minimum of ranges and excluding the maximum. This naively leads to 
:<code>[min<sub>i</sub>, max<sub>i</sub>)</code> bin intervals for the <code>i<sup>th</sup></code> bin

:Where <code>max<sub>i</sub> == min<sub>i + 1</sub></code>.


Numbers falling on any ''interior'' boundary will automatically counted in the ''higher'' bin, but what happens to the highest number? It is '''excluded'''.

To fix this you could:
# Make the upper range of the last bin inclusive.
# Or add an extra bin at the high end for this one, maximal, value.


--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:26, 12 March 2019 (UTC)
:Where is this extra bin coming from? Surely there are 8 bins. Only 8 bins. Not 7 and never 9. Oh, and no fence-posts--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:34, 14 March 2019 (UTC)

::I wrote to highlight the boundary issues. I wouldn't chose the second option. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:30, 14 March 2019 (UTC)
