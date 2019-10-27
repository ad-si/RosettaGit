+++
title = "Talk:Statistics/Basic"
description = ""
date = 2016-11-10T18:58:49Z
aliases = []
[extra]
id = 10014
[taxonomies]
categories = []
tags = []
+++

==Wrong emphasis in 'Extra'?==
Once one example shows how to calculate them from keeping running sums of x and x-squared then they should all be able to copy. Better to just add reference to the other formulas so we can compare the ''language implementations''.
I guess this is because RC is about showing off language capabilities and, (trying to), rely less on the knowledge of individual contributors. --[[User:Paddy3118|Paddy3118]] 12:53, 2 July 2011 (UTC)
: There isn't much of a formula to talk about, and the requirement is a real world need.  This task isn't meant to be difficult, but more related to what actually happens in real data analysis.  And why narrow down on "language capabilities"? What kind of programmer would be hurt by a little thinking about simple algorithms? --[[User:Ledrug|Ledrug]] 13:38, 2 July 2011 (UTC)

:: The idea is to aid language comparison rather than be yet another generic programmer challenge site. --[[User:Paddy3118|Paddy3118]] 16:09, 2 July 2011 (UTC)
::I agree. I don't think this is really the place for challenges. We don't have much of a framework for it anyway since the solutions are all on the task page. I think it's best to just name or describe an algorithm right from the start. A "better" algorithm could be used as extra credit (e.g. one that greatly reduces error or handles corner cases well). --[[User:Mwn3d|Mwn3d]] 16:27, 2 July 2011 (UTC)
How about stating the possible patterns in the standard deviation you might find and
adjusting the task to give languages a chance to show them? --[[User:Paddy3118|Paddy3118]] 16:10, 2 July 2011 (UTC)
:Which part of it looks like a challenge?  You add up some numbers, then maybe divide by another number, it's not like there's tricky coding to be done.  Large dataset is a real senario and not hard to deal with, as long as you don't artificially complicate it.  And as sample size increases, numbers such as mean and stddev becomes stable, which is almost the whole point of statistics: it's an easily noticeable trend, I'm not asking you to find face of Jesus in the output numbers.  As a programmer, none of these should be hard to understand, and I never said anything about greatly reducing errors: you can only avoid greatly <i>increasing</i> it, but that's natural requirement for anyone doing numerical work. --[[User:Ledrug|Ledrug]] 17:02, 2 July 2011 (UTC)
:: Making it numerically stable, that's challenging. It's easy enough if you have a small number of values of all about the same scale, but that's not always the case. –[[User:Dkf|Donal Fellows]] 17:11, 2 July 2011 (UTC)
::: That's actually rarely a problem in the real world.  If a distribution is narrow, scale difference is small; if distribution is wide, losing some precision on really small numbers wouldn't affect either average or stddev.  It probably will be a concern only when you have a few very large numbers and a lot of smaller ones (say < 10^-16 relatively in abs, but about 10^16 in quantity), but what kind of physical measurement would give a distribution like that?  In any event, I didn't say anything about that in the task; the distribution used is uniform, it really can't get much simpler than that. --[[User:Ledrug|Ledrug]] 17:35, 2 July 2011 (UTC)
:::: The distribution comes up quite often when working with quantities that follow a power law (i.e., where they are distributed more evenly in log space) which is actually quite often. In any case, the warning about such things is relevant because someone ''will'' copy the code on this page and use it unwisely; there are whole legions of fools who want to program by cut-n-paste only and without any thought for side conditions, but even so it is still something that we should note for our own consciences. Write robust code for extra credit! –[[User:Dkf|Donal Fellows]] 18:03, 2 July 2011 (UTC)
::::: I don't mean to argue, but what kind of values are you measuring that crosses 16 orders of magnitude?  AFAIK the most precise measurement in physics to date is only at 10^-13 relative scale. --[[User:Ledrug|Ledrug]] 18:24, 2 July 2011 (UTC)

Hi Ledrug, I initially read the extra credit task as requiring the formula for calculating the standard deviation that avoids going once through the numbers to calculate the mean then going through the numbers a second time to work out the differences from the mean, and so on. Because that formula was ommited, it seemed like an attempt to make that part of the task about finding that formula as well as implementing it. If that was not your intent, and no-one else saw it that way then I apologise. --[[User:Paddy3118|Paddy3118]] 20:39, 2 July 2011 (UTC)

== stddev slightly wrong ==

Isn't the defintion of stddev <math>\sigma\equiv\sqrt{{1\over n-1}\sum_i \left(x_i - \bar x \right)^2}</math>?  Ok, granted, the difference between this and what is currently posted on the task page is only numerically significant for small sample sizes, but if we are going to use the simplification for large sample sizes shouldn't we tone down the <math>\equiv</math>?  --[[User:Rdm|Rdm]] 21:01, 2 July 2011 (UTC)

:Are we dealing with a sample or not? The task states that we use the non-sample formula which is OK as it clearly states what to use. --[[User:Paddy3118|Paddy3118]] 21:30, 2 July 2011 (UTC)

::The task currently says:

:::When talking about a set of sampled data, most frequently used is their [[wp:Mean|mean value]] and [[wp:Standard_deviation|standard deviation (stddev)]].  If you have set of data <math>x_i</math> where <math>i = 1, 2, \cdots n</math>, the mean is <math>\bar{x}\equiv {1\over n}\sum_i x_i</math>, while the stddev is <math>\sigma\equiv\sqrt{{1\over n}\sum_i \left(x_i - \bar x \right)^2}</math>.

::To me, this conveys that we are to supposed to use the population stddev on a set of sampled data.  And for that matter, the use of random numbers implies that we are talking about samples. --[[User:Rdm|Rdm]] 21:49, 2 July 2011 (UTC)

:: This is quite complicated.  To put it simply, <math>1/(N-1)</math> is used when you need to infer the mean and stddev of an unknown distribution from a very small set of samples from it, while <math>1/N</math> is used when you take the available data set as fully representing the distribution or know exactly the mean of the distribution function.  I chose to use the <math>1/N</math> formula is because 1) this task is merely for observing numbers, not doing any variance estimate; 2) it's simpler to derive and understand; 3) it does have its clearly defined meaning regarding how much the data vary, and the distribution is known to be uniform (though strictly speaking then the <math>\bar x</math> should not use the average of samples in this case, it should be exactly 0.5).  In any event, when sample size goes up into hundreds, the difference between those two formulas become moot.  --[[User:Ledrug|Ledrug]] 23:06, 2 July 2011 (UTC)

::: Ok, yes, that is a good point.  If we use 0.5 for the mean in this task, that is the right formula. --[[User:Rdm|Rdm]] 23:30, 2 July 2011 (UTC)

== a bit of duplication ==

There's already a task [[Standard_deviation]] (and it discusses n .v. n-1).  At a minimum a reference may be in order.  --[[User:Dgamey|Dgamey]] 21:35, 2 July 2011 (UTC)

:This task looks like it is about showing a histogram. I certainly think we need to rename. With some rewording we could probaly shoehorn this into "Statistics/Histograms/Standard Deviation" (which is a really nasty name). Keep thinking guys. [[User:Markhobley|Markhobley]] 08:08, 13 July 2011 (UTC)

== Random number spec ==

Do we need to really specify the range as <math>[0,1]</math>? I ask because it's far more common for random number generators to omit the endpoints, giving a range of <math>(0,1)</math> (due to the way LCPRNGs work). It's a subtle difference I admit. –[[User:Dkf|Donal Fellows]] 10:51, 19 October 2011 (UTC)

:Hi Donal, the task goes on to state: ''". It doesn't matter if you chose to use open or closed range."'' --[[User:Paddy3118|Paddy3118]] 12:32, 19 October 2011 (UTC)

== Perl style ==

I was looking at the perl section, and the code there doesn't seem representative of modern perl style.  In particular, the c-style for loop syntax is not very perl-ish.  I'd like to change the syntax up a little bit:


```perl
my $hist_rows = 10;
my @histogram = map { 0 } 1..$hist_rows; # I filled the array to ensure that $#histogram == $hist_rows-1 - this may be unnecesssary
# alternatively, do away with the $hist_rows variable altogether, using map {0} 1..10 and referring to @histogram instead of $hist_rows below
my $sum = 0;
my $sum_squares = 0;
my $n = $ARGV[ 0 ];

for (1..$n) { # no need to name a variable that is never used
  my $current = rand( 1 );
  $sum += $current;
  $sum_squares += $current ** 2;
  $histogram[ $current * $hist_rows ] += 1;
}

my $mean = $sum / $n;

print "$n numbers\n", # concatenation replaced with multiple arguments to print
      "Mean:   $mean\n",
      "Stddev: ", sqrt(($sum_squares / $n) - ($mean ** 2)), "\n";

for my $row_counter (0..$#histogram) {
  printf( "%.1f - %.1f : ", $row_counter/$hist_rows, (1/$hist_rows) + ($row_counter/$hist_rows));

  print "*" x ($histogram[ $row_counter ]*(30/($n/10))); # loop replaced with x operator
  print "\n";
}
```

(The comments above are notes to the talk page, and I would erase them before posting the above to the article.)

Needless to say, TMTOWTDI, but this seems more like idiomatic perl to me, and, at least to me, it's more readable. [[User:Flies 1|Flies 1]] 22:26, 5 March 2012 (UTC)
:I concur with the style tweaks, and would go so far as to initialize the array with <tt>(0) x 10</tt> instead of using a <tt>map</tt>.  I also dislike the original's style of making <tt>for</tt> look like a function call when it's really a keyword, so I'm glad you're fixing that too. --[[User:TimToady|TimToady]] 00:02, 6 March 2012 (UTC)
::I have updated the code - the page now matches closely what I suggested above.  Further improvements to style are welcome! [[User:Flies 1|Flies 1]] 22:06, 6 March 2012 (UTC)

== Bugs in Python Example ==

The previous example code had the following bugs

# Line 36 prints the value of i that was used in a list comprehension (line 35) not the original value of i in the outer loop, so the printed value (in Python 2.7) is the max value not the number of values, e.g. 10^7
# Runtime exceeds 2 minutes on a MacBook Pro. Probably not suitable for an example

So I reduced the maximum list allocated to 100,000 random values, and added a new index variable, j, to take care of the recycled variable i displaying the wrong value (and hanging the terminal update because the string was 10 MB long).

--[[User:Hobs|Hobs]] ([[User talk:Hobs|talk]]) 19:35, 8 April 2013 (UTC)


==Formulae made invisible for most browsers by under-tested cosmetic edits at 20:09, 21 June 2016 ==

Under-tested cosmetic edits made to the task page at 20:09, 21 June 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 10:06, 24 September 2016 (UTC)

:: Visibility of task description formulae now restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:58, 10 November 2016 (UTC)
