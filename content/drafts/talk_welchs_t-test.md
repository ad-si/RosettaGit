+++
title = "Talk:Welch's t-test"
description = ""
date = 2018-03-20T07:24:51Z
aliases = []
[extra]
id = 19183
[taxonomies]
categories = []
tags = []
+++

==Needs better task description==
I haven't looked at the C code yet, but I'm assuming it's using t-test?  The description should provide more context and explanations of concepts, and preferably links to algorithms. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 20:16, 26 May 2015 (UTC)

Yes, this uses Welch's 2-sided t-test, as I commented inside the code.
:Hi, you need to take all those nuggets out of your code comments and put them into an improved task description. The task description needs to stand on its own as a clear and concise description of what needs to be accomplished.
:(P.S. Please sign your comments). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:09, 27 May 2015 (UTC)

Hi  Hailholyghost, I just had a look at the link you give and it is inadequate as a description for an RC task. The task description needs to be written for an audience of enthusiastic ''programmers'' - not necessarily maths or stats or whatever enthusiasts. It seems that you are new to RC and maybe you need to take time and lurk a bit more to understand a little more about how things are done.

This task needs a full description of the calculation method to use, probably in pseudocode, together with a decription of what the algorithm should be used for to complete a good task. The Code you give is not enough for a ''task'' description. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:23, 27 May 2015 (UTC)

: I added that link, as a pointer to the right direction for now.  To be fair though, null hypothesis testing is very involved and sometimes borders black magic, so it may be difficult to explain everything clearly in a short text.  The following wiki links may be relevant: [[wp:Statistical hypothesis testing]], [[wp:ANOVA]], and more specifically [[wp:Student's t-test]] and [[wp:Welch's t-test]].  The Student's t-test article has more details on actual computations, which forms the basis for the Welch's test. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:44, 27 May 2015 (UTC)

:: So task description should perhaps also include explicit cautions about p values... Perhaps xkcd 882 and 1478? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:32, 3 June 2015 (UTC)

I've improved the C function to work with larger arrays using tgammal instead of tgamma, and have exception handling if the entered array is too small.  I have made some modifications to the Simpson integration part, and the function now runs about twice as fast as before.  I have also added a description.  I have removed comments in my code.  I hope this is satisfactory.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 18:28, 3 June 2015 (UTC)

: Looks like you pulled some of that math out of wikipedia, but even there there's not quite enough context. For example, what is the definition of <code>u</code> and of <code>f(u)</code>? That kind of stuff works in a classroom context where representative examples have been recently referenced, but that's not the case here. 

: Also, if you are going the math route I think you should mention basic assumptions (for example, I think you are assuming that the list of values were taken from what would be some normal distribution). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:04, 3 June 2015 (UTC)

I can work on the task description later.  On a more practical matter, this code cannot calculate p-value for very large array sizes (> about 1755 elements).  Does anyone know how to solve this? ==[[User:hailholyghost|hailholyghost]] 15:18 Friday 5 June (UTC)

The fraction <math>\frac{\Gamma(a)}{\Gamma(a+0.5)}</math> blows up.  How can I get ratio in terms of lgammal? ==[[User:hailholyghost|hailholyghost]] 15:26 7 June 2015.

I can get this fraction in terms of <math>B(a,a+0.5)</math>, but it is computationally expensive.  At least it works now.
As for the task description, how much detail is required? I only put what I considered necessary to the computation, as this is work I did myself.  The internet is awash with articles about p-value, so I only linked to those wikipedia articles.  The reason I wrote this page is because I was unable to find a way to implement this computation directly, after weeks and weeks of internet searches.  I hope that this computer code can be beneficial to others.--[[User:hailholyghost|hailholyghost]] 14:25 Tuesday 9 June 2015 UTC.
: Just use exp(lgamma(a) - lgamma(a+0.5)).  Replacing tgamma with tgammal is only delaying the overflow until longer data (10000 or so?), while loggamma function should not overflow with any reasonable data. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 18:35, 9 June 2015 (UTC)

::Hi Ledrug, I tihnk you used the logarithm identity <math> \log(\frac{a}{b}) = \log(a)-\log(b) </math> but this doesn't apply here because

::<math>\frac{\Gamma(a)}{\Gamma(a+0.5)} \implies \frac{\ln(\Gamma(a))}{\ln(\Gamma(a+0.5))} \neq \ln(\Gamma(a)) -\ln(\Gamma(a+0.5))</math>

::rather,

::<math>\frac{\ln(\Gamma(a))}{\ln(\Gamma(a+0.5))} = \log_{\Gamma(a+0.5)}(\Gamma(a)) </math> which unfortunately doesn't seem to go anywhere.

::The answer to this has to buried somewhere in the bowels of the internet... but I can't find it...--[[User:hailholyghost|hailholyghost]] 14:07 10 June 2015 (UTC)

::: How does that not apply? <math>e^{\ln A - \ln B} =e^{\ln {A\over B}} = {A\over B}</math> where A and B are the gammas, isn't that what you want? --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 19:02, 10 June 2015 (UTC)

::: If I understand the mass of expressions on the task page, you want to evaluate
::: <math>
 \Beta(x,y) = \dfrac{\Gamma(x)\,\Gamma(y)}{\Gamma(x+y)}
\!</math>

:::But this is equivalent to 
::: <math>
 \Beta(x,y) = \exp((\ln(\Gamma(x)) + \ln(\Gamma(y))) - \ln(\Gamma(x+y)))
\!</math>

:::Or have I misunderstood what the task needs? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:31, 10 June 2015 (UTC)

::::Rdm, thank you so much!!!! --[[User:hailholyghost|hailholyghost]] 15:11 EST 10 June 2015 (UST)

--[[User:Ledrug|Ledrug]] you are correct, of course, I put what you said into the task description.  I'll put more about the definition of the p-value and warnings, maybe split the task description into two different sections.--[[User:hailholyghost|hailholyghost]] 16:00 UTC 13 June 2015 (UST)

someone keeps deleting my Perl translation of C, which works without installing any packages.  This person's other implementation doesn't work on my computer because I can't install the package.

==Task description complete?==
I have made the task description more complete.  I consider this page as ready to be published as a complete task.  If someone else feels it is not ready, please give me a *specific* description of what's missing or why this isn't yet ready.  I tried adding references but had formatting issues.  I would like to cite this link, among others, if someone could please show me how to do this: http://www.nature.com/polopoly_fs/1.14700!/menu/main/topColumns/topLeftColumn/pdf/506150a.pdf --[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 13:28, 23 June 2015 (UTC)--

:If you look at most of the other tasks and consider the authorship of the examples, then you might agree with me that your task description stands out as being couched in heavy mathematical notation rather than in, say, pseudo-code for example. This puts a barrier between the readership and the task as perfectly proficient programmers would also need to be statisticians to follow the description. 
:This is not what RC is about - as you can see from other examples where very mathematical concepts such as [[Quaternion type|Quaternions]] are given in task form that is explained to a programming audience. That has not been done in your draft task. 

:In short; explain it to the RC audience rather than to yourself - If you don't have an idea of the RC audience, (and that might be the case as you are asking how to create links),  then you need to both lurk more on the site and read other tasks until you do. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:03, 23 June 2015 (UTC)

: Probably would be a good idea to include a link to [[Gamma_function]] and also to explain how to handle the definite integrals. I think we also need some documentation on how to implement lngamma given a decent implementation of gamma (log of gamma of fractional part of n plus sum of logs of the positive integers less than the integer part of n, or something like that). We might need a bit more than that, but I think we need at least that much. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:17, 23 June 2015 (UTC)

: One more thing we need here is how to calculate the degrees of freedom for a single dataset. You've only supplied an expression for approximating the degrees of freedom of the two sets combined if we already know the degrees of freedom for each of the sets. (Presumably - since you are asking for sample variance - it's N-1 - but that should be specified.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 05:07, 3 July 2015 (UTC)

: Another issue: according to [[wp:Welch's_t_test]] "<math>s_{1}^{2}</math> is [[wp:variance|sample variance]]" and you seem to be using the equations from there, but in your task description you currently instead say that "<math>s_n </math> is the [[wp:Variance#Population_variance_and_sample_variance|sample variance]] of set <math>n</math>" (Also, [[wp:Variance#Sample_variance|sample variance]] can be calculated in one of two ways - the expression you gave corresponds to what they label "unbiased sample variance" .. perhaps a minor issue? But I implemented what I think your task description has declared I should be calculating and I get a different result than the other task implementations, so I'm having to review all the basics...) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:03, 3 July 2015 (UTC)

:: I also noticed the unexplained nu1 and nu2 and the s instead of s^2.  In addition in the sample variance formula I think the subscript on the mean should be lower case n.  And the term p2-tail is unexplained.  This means the same as p-value?  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 00:07, 7 July 2015 (UTC)

Hello Sonia and Rdm, I think I've answered your questions about sample variance and other errors, and thank you for catching the mistakes.  Please let me know if you see any other errors.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 06:08, 7 July 2015 (UTC)

is there anything else that should be done? It was said that some other people should weigh in on whether or not this should be a complete task, but no one seems to be reading this.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 14:29, 26 July 2015 (UTC)

: I think things are pretty good, and if no one speaks up soon (let's say, within the next month?) with any specific problems that need to be addressed, we can promote the task from draft status. Or, if you are feeling optimistic, you could take it out of draft status now and if someone has concerns they can put it back into draft status along with a description of what problems they feel need to be addressed. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:47, 26 July 2015 (UTC)

== Task definition vs. Task implementation ==

Currently the task implementation asks for

<math> p = 1-\frac{1}{2}\times\frac{\int_0^\frac{\nu}{t^2+\nu} \frac{r^{\frac{\nu}{2}-1}}{\sqrt{1-r}}\,\mathrm{d}r}{ \exp((\ln(\Gamma(\frac{\nu}{2})) + \ln(\Gamma(0.5)) - \ln(\Gamma(\frac{\nu}{2}+0.5))) }</math>

But after reading the C implementation, what is actually being calculated is

<math> p = \frac{\int_0^\frac{\nu}{t^2+\nu} \frac{r^{\frac{\nu}{2}-1}}{\sqrt{1-r}}\,\mathrm{d}r}{ \exp((\ln(\Gamma(\frac{\nu}{2})) + \ln(\Gamma(0.5)) - \ln(\Gamma(\frac{\nu}{2}+0.5))) }</math>

In other words, for <27.5 21 19 23.6 17 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19 21.7 21.4> and <27.1 22 20.8 23.4 23.4 23.5 25.8 22 24.8 20.2 21.9 22.1 22.9 20.5 24.4> the task description would have us calculate a value of 0.989311 but the implementations give a value of 0.021378. And you can easily see this in the code - 


```c
	double return_value = ((h / 6.0) * ((pow(x,a-1))/(sqrt(1-x)) + 4.0 * sum1 + 2.0 * sum2))/(expl(lgammal(a)+0.57236494292470009-lgammal(a+0.5)))
```


There is no 1-expression here (except deeply inside parenthesis) and there is no divide by 2 or multiple 0.5 (again, except deeply inside parenthesis).

I think that either the task description needs to be changed to match the implementation, or the implementation needs to be changed to match the task description. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 07:47, 3 July 2015 (UTC)

Is there anything missing/in error that this page is still considered in draft mode?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 13:40, 8 July 2015 (UTC)

: Well... the lngamma algorithm will be important for anyone who doesn't have a native implementation of that. That could be a separate task, and linked in the task description, if you are not comfortable documenting it here.  Paddy's suggestion of pseudocode is also worth considering (perhaps on a separate page such as [[Calculate_P-Value/Pseudocode]] linked into the task description?), though at this point there is perhaps enough real code that that is not such an issue? I guess, let's give some of the other people here time to weigh in on this... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:48, 8 July 2015 (UTC)

I saw that C99 standard math.h has lgammal, so I figured it was standard in every language like log or pow.  However, I see now that lgammal is not standard in every language.  Is there something from math.h that is like

```c

#include <stdio.h>
long double lgammal (const long double input) {
//... some math....
 return result;
}

```
 that I could paste into my code?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 18:22, 8 July 2015 (UTC)

You probably do not need to have duplicate copies of your implementation just for the missing lngamma issue. Instead, name your implementation of it "lngamma" and include a note that this routine should be included in the implementation if it is not supplied when linked with <code>-lm</code>. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:40, 15 July 2015 (UTC)

Hi Rdm, I'm a little confused, but I think I understand what you mean.  The copies I put are not complete duplicates, because lgammal and LnGamma are spelled differently.  I am concerned that lgamma and LnGamma do not output exactly the same numbers, as you can see in the output section.  I don't see alternate implementation coded on any other Rosetta Code page, could you please provide an example of how to format the implementations on the page? I'm trying to imitate the formatting of the quaternion page, but I don't see alternate implementations there or on a few other pages.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 14:13, 15 July 2015 (UTC)

: How bad are the differences from the alternate implementation of ln gamma? Do they matter for the task example? What kind of example would they matter for?
: Anyways, yes, I would get rid of the LnGamma implementation. And, I would change the spelling of its name to 'lngamma' so it can be used as a drop in replacement. If accuracy is a concern, I would document that as an issue so that an interested programmer could address the problem(s). 
: Does that address your concerns adequately? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:39, 15 July 2015 (UTC)

The max difference between lgamma and LnGamma for double is 1.862645e-09, and for long double is 1.919034e-10.  I don't think this matters for most applications.  The code belows shows how I calculated the difference.


```C>#include <stdio.h
//printf 
#include <math.h>//lgamma
 
long double LnGamma(const double xx) {
   unsigned int j;
   long double x,y,tmp,ser;
   const double cof[6] = {
      76.18009172947146,    -86.50532032941677,
      24.01409824083091,    -1.231739572450155,
      0.1208650973866179e-2,-0.5395239384953e-5
   };
 
   y = x = xx;
   tmp = x + 5.5 - (x + 0.5) * logl(x + 5.5);
   ser = 1.000000000190015;
   for (j=0;j<=5;j++)
      ser += (cof[j] / ++y);
   return(log(2.5066282746310005 * ser / x) - tmp);
}
 
int main (void) {
	long double max_difference = 0.0;
	long double lgamma_ans = 0.0, LnGamma_ans = 0.0;
	long double worst_lgamma_ans = 0.0, worst_LnGamma_ans = 0.0;
	unsigned int worst_x = 3;
	for (unsigned int x = 3; x < 965535; x++) {
		lgamma_ans = lgammal(x);
		LnGamma_ans = LnGamma(x);
		if (fabsl(LnGamma_ans-lgamma_ans) > max_difference) {
			max_difference = fabsl(LnGamma_ans-lgamma_ans);
			worst_lgamma_ans = lgamma_ans;
			worst_LnGamma_ans= LnGamma_ans;
			worst_x = x;
		}
//		printf("%d\t%.15f\t%.15f\t%e\n",x,lgamma(x),LnGamma(x),lgamma(x)-LnGamma(x));
	}
	printf("Max difference between lgamma & LnGamma = %Le, for x = %d, lgamma(%d) = %Le; LnGamma(%d) = %Le\n\n",max_difference,worst_x,worst_x,worst_lgamma_ans,worst_x,worst_LnGamma_ans);
	return 0;
}

```


I'll modify the main page accordingly.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 21:01, 15 July 2015 (UTC)

== lngamma ==

You can build an adequate lngamma from the rosettacode [[Gamma_function|gamma function]] implementation, something like this:


```pseudocode
function lngamma(x) {
   if x < 3 then
      return ln(gamma(x))
   else
      frac= x mod 1
      r= ln(gamma(1+frac))
      for index= 1 thru x-1
         r= r + ln(index+frac)
      end for
      return r
   end if
end function
```


(I think I got that right - this is based on an email from [[User:Roger_Hui|Roger Hui]], but I've adapted it from J to pseudo-code. Any errors are my own.  Except errors in the gamma function implementation - those might be someone else's. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:23, 8 July 2015 (UTC))

: That's short and sweet!  I looked up a few library implementations and they were much more involved with lots of special cases and magic numbers.  It all depends on what is "adequate."  For the example data used so far in this task, the Racket and Tcl examples show that even the non-log gamma is adequate.  It seems okay to me to allow the non-log gamma for the task.  I think it's still nice though that the task description points out the limitation of non-log gamma and shows the preferred solution using log gamma.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 21:46, 8 July 2015 (UTC)

For languages that have a native <code>lgamma</code> implementation, such as C, no <code>lgamma</code> function I write will be as good, and will only detriment the C code.  Nonetheless, your point is taken, and I'll write a perl translation of my code with an lgamma.  However, I want to make the code as good as I can.  How can I get the code for math.h lgamma? I've looked all over and it's buried in my laptop somewhere but I can't find it.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 10:08, 10 July 2015 (UTC)

: My copy of math.h does not have lngamma. Perhaps you should ask whoever you got your copy from? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:11, 10 July 2015 (UTC)

Hi Rdm, you can call <code>lgamma</code> not lngamma, by just placing <code>#include <math.h></code> in your C program, and calling <code>lgamma(5.5)</code> or whatever inside <code>main</code>.  My copy of math.h came standard with my Ubuntu installation.


```perl
sub lgamma {  # per code from numerical recipies, http://hea-www.harvard.edu/~alexey/calc-src.txt
  my $xx = $_[0];
  my ($j, $ser, $tmp, $x, $y);
  my @cof = (0.0, 76.18009172947146, -86.50532032941677,
	     24.01409824083091, -1.231739572450155, 0.1208650973866179e-2,
	     -0.5395239384953e-5);
  my $stp = 2.5066282746310005;
    
  $x = $xx; $y = $x;
  $tmp = $x + 5.5;
  $tmp = ($x+0.5)*log($tmp)-$tmp;
  $ser = 1.000000000190015;
  foreach $j ( 1 .. 6 ) {
    $y+=1.0;
    $ser+=$cof[$j]/$y;
  }
  return $tmp + log($stp*$ser/$x);
}

```


I got this code from another website (commented) is this acceptable to use in Perl on Rosetta Code?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 12:56, 10 July 2015 (UTC)

: I'm not using ubuntu at the moment. I do have access to some ubuntu 12.x boxes, but those boxes don't have lngamma nor lgamma. But ubuntu provides the complete sources - so you could, if you wanted to, look through them yourself? Presumably your version is in the source package for libc6 (that's eglibc on the ubuntu box that I was looking at, but it might be different on your machine?). But anyways - no, it doesn't work that way for me.

: As for "acceptable to use in perl on this site"... I guess that should be ok. A quick web search finds http://paulbourke.net/miscellaneous/functions/ suggesting that the original source for the original version of that code was the Numerical Recipes book. And that code gets heavy use all over the place so I expect that no one should object to your using it here. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:17, 10 July 2015 (UTC)

is it acceptable to have two different implementations of this C code:
1. for computers with lgamma implemented, the first C program,
and 
2. for computers without lgamma implemented, a 2nd C program?

there are minor differences between LnGamma and lgamma, as you can see when you run this program:

```C

#include <stdio.h> // printf 
#include <math.h>//lgamma,log

double LnGamma(const double xx) {
   unsigned int j;
   double x,y,tmp,ser;
   const double cof[6] = {
      76.18009172947146,    -86.50532032941677,
      24.01409824083091,    -1.231739572450155,
      0.1208650973866179e-2,-0.5395239384953e-5
   };

   y = x = xx;
   tmp = x + 5.5 - (x + 0.5) * logl(x + 5.5);
   ser = 1.000000000190015;
   for (j=0;j<=5;j++)
      ser += (cof[j] / ++y);
   return(log(2.5066282746310005 * ser / x) - tmp);
}

int main (void) {
	printf("x\tlgamma(x)\tLnGamma(x)\tlgamma(x)-LnGamma(x)\n");
	for (unsigned short int x = 3; x < 65535; x++) {
		printf("%d\t%.15f\t%.15f\t%e\n",x,lgamma(x),LnGamma(x),lgamma(x)-LnGamma(x));
	}
	return 0;
}

```

--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 16:34, 10 July 2015 (UTC)

I have added an lngamma function in case the user does not have his/her own implementation of lgamma.  Is there anything else with the C code or task description that can be improved?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 15:06, 14 July 2015 (UTC)
==Perl==
Hello, I have created a perl translation of my C code.  Here is my perl script:


```= Perl
#!/usr/bin/env perl

use strict; use warnings;

sub lngamma {  # per code from numerical recipies, http://hea-www.harvard.edu/~alexey/calc-src.txt
  my $xx = $_[0];
  my ($j, $ser, $tmp, $x, $y);
  my @cof = (0.0, 76.18009172947146, -86.50532032941677,
	     24.01409824083091, -1.231739572450155, 0.1208650973866179e-2,
	     -0.5395239384953e-5);
  my $stp = 2.5066282746310005;
    
  $x = $xx; $y = $x;
  $tmp = $x + 5.5;
  $tmp = ($x+0.5)*log($tmp)-$tmp;
  $ser = 1.000000000190015;
  foreach $j ( 1 .. 6 ) {
    $y+=1.0;
    $ser+=$cof[$j]/$y;
  }
  return $tmp + log($stp*$ser/$x);
}

sub calculate_Pvalue {
	my $array1 = shift;
	my $array2 = shift;
	if ((@$array1 <= 1) || (@$array2 <= 1)) {
		return 1.0;
	}
	my $mean1 = 0.0;
	my $mean2 = 0.0;
	foreach my $e (@$array1) {
		$mean1 += $e;
	}
	foreach my $e (@$array2) {
		$mean2 += $e;
	}
	if ($mean1 == $mean2) {
		return 1.0;
	}
	$mean1 /= @$array1;
	$mean2 /= @$array2;
	my ($variance1,$variance2) = (0,0);
	foreach my $e (@$array1) {
		$variance1 += ($mean1-$e)*($mean1-$e);
	}
	foreach my $e (@$array2) {
		$variance2 += ($mean2-$e)*($mean2-$e);
	}
	if (($variance1 == 0.0) && ($variance2 == 0.0)) {
		return 1.0;
	}
	$variance1 = $variance1/(@$array1-1);
	$variance2 = $variance2/(@$array2-1);
	my $WELCH_T_STATISTIC = ($mean1-$mean2)/sqrt($variance1/scalar(@$array1)+$variance2/scalar(@$array2));
	my $DEGREES_OF_FREEDOM = (($variance1/@$array1+$variance2/scalar(@$array2))**2)#numerator
	 /	(
		($variance1*$variance1)/(scalar(@$array1)*scalar(@$array1)*(scalar(@$array1)-1))+
		($variance2*$variance2)/(scalar(@$array2)*scalar(@$array2)*(scalar(@$array2)-1))
	);
	printf("t = %lf; DOF = %lf\n",$WELCH_T_STATISTIC,$DEGREES_OF_FREEDOM);
	my $sa = $DEGREES_OF_FREEDOM/2;
	my $x = $DEGREES_OF_FREEDOM/($WELCH_T_STATISTIC*$WELCH_T_STATISTIC+$DEGREES_OF_FREEDOM);
	my $N = 65355;
	my $h = $x/$N;
	my ($sum1,$sum2) = (0.0,0.0);
	for (my $i = 0; $i < $N; $i++) {
      $sum1 += (($h * $i + $h / 2.0)**($sa-1))/(sqrt(1-($h * $i + $h / 2.0)));
      $sum2 += (($h * $i)**($sa-1))/(sqrt(1-$h * $i));
	}
	print "sum1 = $sum1; sum2 = $sum2\n";
	return ($h / 6.0) * ((($x**($sa-1))/(sqrt(1-$x)) + 4.0 * $sum1 + 2.0 * $sum2))/(exp(&lngamma($sa)+0.57236494292470009-&lngamma($sa+0.5)));
}
my @d1 = (27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4);
my @d2 = (27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4);
my @d3 = (17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8);
my @d4 = (21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8);
my @d5 = (19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0);
my @d6 = (28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2);
my @d7 = (30.02,29.99,30.11,29.97,30.01,29.99);
my @d8 = (29.89,29.93,29.72,29.98,30.02,29.98);
my @x = (3.0,4.0,1.0,2.1);
my @y = (490.2,340.0,433.9);
printf("Test sets 1 p-value = %lf\n",&calculate_Pvalue(\@d1,\@d2));
printf("Test sets 2 p-value = %lf\n",&calculate_Pvalue(\@d3,\@d4));
printf("Test sets 3 p-value = %lf\n",&calculate_Pvalue(\@d5,\@d6));
printf("Test sets 4 p-value = %lf\n",&calculate_Pvalue(\@d7,\@d8));
printf("Test sets 5 p-value = %lf\n",&calculate_Pvalue(\@x,\@y));

```


and comparing with the C code:

{{out}}

```txt
con@e:~/DNA_Methylation$ perl pvalue.pl
t = -2.455356; DOF = 24.988529
sum1 = 878.360186998937; sum2 = 878.265618888918
Test sets 1 p-value = 0.021378
t = -1.565434; DOF = 9.904741
sum1 = 9911.19818303728; sum2 = 9910.72960543568
Test sets 2 p-value = 0.148842
t = -2.219241; DOF = 24.496223
sum1 = 1444.70812944688; sum2 = 1444.55247891539
Test sets 3 p-value = 0.035972
t = 1.959006; DOF = 7.030560
sum1 = 8982.44374807736; sum2 = 8982.16243331816
Test sets 4 p-value = 0.090773
t = -9.559498; DOF = 2.000852
sum1 = 65573.4697898075; sum2 = 65572.4685866506
Test sets 5 p-value = 0.010752

real	0m0.287s
user	0m0.284s
sys	0m0.000s
con@e:~/DNA_Methylation$ ./pvalue 
t = -2.455356; DOF = 24.988529
sum1 = 880.779357; sum2 = 880.684789
Test sets 1 p-value = 0.021378001462867
t = -1.565434; DOF = 9.904741
sum1 = 9938.495493; sum2 = 9938.026915
Test sets 2 p-value = 0.148841696605327
t = -2.219241; DOF = 24.496223
sum1 = 1448.687128; sum2 = 1448.531478
Test sets 3 p-value = 0.035972271029797
t = 1.959006; DOF = 7.030560
sum1 = 9007.183093; sum2 = 9006.901778
Test sets 4 p-value = 0.090773324285661
t = -9.559498; DOF = 2.000852
sum1 = 65754.071496; sum2 = 65753.070294
Test sets 5 p-value = 0.010751534107903
```

The t statistics and DOF are the same between perl and C, but the sums are slightly off.  How can I fix this?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 15:46, 16 July 2015 (UTC)

: I would start by finding in which part of the algorithm your implementations start producing different values.

: Once you have isolated where they differ in value, and you've a relatively good set of representative test cases, the next step would be to find which is mathematically accurate (if either of them significantly more accurate than the other).

: And once you have that, you can work on fixing the other implementation (and we might be able to help you, there, if you can explain the issue). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:55, 16 July 2015 (UTC)

Hi Rdm, as the output shows, the sums are different while the t statistic and DOF are the same.

That is, this C code

```= C
for(unsigned short int i = 0;i < N; i++) {
      sum1 += (pow(h * i + h / 2.0,a-1))/(sqrt(1-(h * i + h / 2.0)));
      sum2 += (pow(h * i,a-1))/(sqrt(1-h * i));
	}

```

and this Perl code

```= Perl

for (my $i = 0; $i < $N; $i++) {
      $sum1 += (($h * $i + $h / 2.0)**($sa-1))/(sqrt(1-($h * $i + $h / 2.0)));
      $sum2 += (($h * $i)**($sa-1))/(sqrt(1-$h * $i));
	}

```


add differently.  I am not a programmer by trade, and I'm completely self-taught, so there is a lot I don't know.  Why would Perl and C sum differently?  Is there a computer science reason?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 13:49, 17 July 2015 (UTC)

: Presumably either the numbers are different or the implementation of addition is different. (Though you have other operations in there, besides addition, which could also be different.) Hypothetically speaking, you might also have type differences giving you different results, but I don't see any obvious expressions where that seems likely (still, you might try changing your C implementation so that everything is type double - I don't think that will change anything but when isolating problems you sort of have to assume that all assumptions are suspect).

: I suppose you might try logging summed numbers to a file, instead of adding them up. And then verifying whether or not you get the same or different sums in the same language or a different language. 

: Or often it's useful to just guess what might be going wrong and then putting print statements in the calculation to test that. (Maybe shrink N to something much smaller, just for isolating the differences, so that you are not spamming yourself.)

: Or, I suppose I could try taking apart your code and doing these experiments myself. But I've got some other things I need to be doing, so that's not going to happen today. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:19, 17 July 2015 (UTC)

Hi Rdm, I tried switching the C code <code>int</code> to <code>double</code>, but the answer still comes out the same.  I have no idea why the perl code would output differently than the C code, or how to fix it.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 23:39, 19 July 2015 (UTC)

When I run into this kind of problem, I start looking at intermediate values. I also try to simplify the underlying dataset so that I can review all the intermediate data (which, in this case, would mean debugging with a much smaller value for $N, and then seeing if any fixes discovered there are adequate with the original value of N). --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:35, 20 July 2015 (UTC)

==Existing Perl doesn't work==
User Thundergnat has removed my functioning, but not "idiomatic" Perl application.

Perhaps thundergnat can see what happens when attempting to use this Math::AnyNum package:


```txt
sudo cpanm Math::AnyNum
[sudo] password for con: 
--> Working on Math::AnyNum
Fetching http://www.cpan.org/authors/id/T/TR/TRIZEN/Math-AnyNum-0.17.tar.gz ... OK
Configuring Math-AnyNum-0.17 ... OK
==> Found dependencies: Math::MPFR, Math::MPC
--> Working on Math::MPFR
Fetching http://www.cpan.org/authors/id/S/SI/SISYPHUS/Math-MPFR-3.36.tar.gz ... OK
Configuring Math-MPFR-3.36 ... OK
Building and testing Math-MPFR-3.36 ... FAIL
! Installing Math::MPFR failed. See /home/con/.cpanm/work/1510682702.14692/build.log for details. Retry with --force to force install it.
--> Working on Math::MPC
Fetching http://www.cpan.org/authors/id/S/SI/SISYPHUS/Math-MPC-1.07.tar.gz ... OK
Configuring Math-MPC-1.07 ... OK
==> Found dependencies: Math::MPFR
! Installing the dependencies failed: Module 'Math::MPFR' is not installed
! Bailing out the installation for Math-MPC-1.07.
! Installing the dependencies failed: Module 'Math::MPFR' is not installed, Module 'Math::MPC' is not installed
! Bailing out the installation for Math-AnyNum-0.17.
```


my version may not be "idiomatic" according to Thundergnat, but it '''works''' and Thundergnat's doesn't.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 18:26, 14 November 2017 (UTC)

:'''Existing Perl ''does'' work'''
: Thundergnat didn't write this Perl entry. But it does work, on several operating systems, under several versions of perl. What error are you getting while building Math::MPFR? Did you check the build log like it suggests? Cpan testers seems to think that it builds ok pretty much everywhere (http://www.cpantesters.org/distro/M/Math-MPFR.html ), and a fresh install from scratch works for me locally. Also, you yourself noted problems a few paragraphs up on this page with your perl version returning imprecise results. The version that Trizen wrote uses high precision libraries to avoid those problems. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 19:27, 14 November 2017 (UTC)

:Judging by your path, I'm assuming you are running Linux. Did you install the high precision dev libraries which have all of the header files? Try running <code>sudo apt-get install libmpfr-dev libmpc-dev</code> then retry installing Math::AnyNum. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 22:26, 14 November 2017 (UTC)

:: Sounds like Math::MPFR needs to get cpan to give better diagnostics for that kind of failure? (But why not show both versions? Are they too big or something?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:01, 15 November 2017 (UTC)

I have a more portable version of the current perl code, using the new integration by reduction method, and verifying against the R code as I did with the C code:


```perl

#!/usr/bin/env perl

use strict; use warnings; use Cwd 'getcwd';# use feature 'say';
my $TOP_DIRECTORY = getcwd();
local $SIG{__WARN__} = sub {#kill the program if there are any warnings
	my $message = shift;
	my $fail_filename = "$TOP_DIRECTORY/$0.FAIL";
	open my $fh, '>', $fail_filename or die "Can't write $fail_filename: $!";
	printf $fh ("$message @ %s\n", getcwd());
	close $fh;
	die "$message\n";
};#http:#perlmaven.com/how-to-capture-and-save-warnings-in-perl


sub betain {
	my $x = shift;
	my $p = shift;
	my $q = shift;
	my $beta = shift;
#modified from John Burkardt
	  my $acu = 10**(-15);
  my $ai;
  my $cx;
  my $indx;#int
  my $ns;#int
  my $pp;
  my $psq;
  my $qq;
  my $rx;
  my $temp;
  my $term;
  my $value;
  my $xx;

  $value = $x;
#  ifault = 0;
#Check the input arguments.
  if ( ($p <= 0.0) || ($q <= 0.0 )){
#    *ifault = 1;
    return $value;
  }
  if ( $x < 0.0 || 1.0 < $x )
  {
#    *ifault = 2;
    return $value;
  }
#/*
#  Special cases.
#*/
  if ( $x == 0.0 || $x == 1.0 )   {
    return $value;
  }
  $psq = $p + $q;
  $cx = 1.0 - $x;

  if ( $p < $psq * $x )
  {
    $xx = $cx;
    $cx = $x;
    $pp = $q;
    $qq = $p;
    $indx = 1;
  }
  else
  {
    $xx = $x;
    $pp = $p;
    $qq = $q;
    $indx = 0;
  }

  $term = 1.0;
  $ai = 1.0;
  $value = 1.0;
  $ns = sprintf("%d",( $qq + $cx * $psq ));#this evaluates differently, because $ns is a C integer
#
#  Use the Soper reduction formula.
#
  $rx = $xx / $cx;
  $temp = $qq - $ai;
  if ( $ns == 0 )
  {
    $rx = $xx;
  }

  for ( ; ; )
  {
    $term = $term * $temp * $rx / ( $pp + $ai );
    $value = $value + $term;
    $temp = abs ( $term );
    if ( $temp <= $acu && $temp <= $acu * $value )
    {
      $value = $value * exp ( $pp * log ( $xx ) 
      + ( $qq - 1.0 ) * log ( $cx ) - $beta ) / $pp;

      if ( $indx )
      {
        $value = 1.0 - $value;
      }
      last;
    }

    $ai = $ai + 1.0;
    $ns = $ns - 1;

    if ( 0 <= $ns )
    {
      $temp = $qq - $ai;
      if ( $ns == 0 )
      {
        $rx = $xx;
      }
    }
    else
    {
      $temp = $psq;
     $psq = $psq + 1.0;
    }
  }

  return $value;
}

#lgamma subroutine is less precise than "use POSIX 'lgamma'" but Macs don't have POSIX
sub lgamma {#http://hea-www.harvard.edu/~alexey/calc-src.txt
  my $xx = shift;
  my ($j, $ser, $tmp, $x, $y);
  my @cof = (0.0, 76.18009172947146, -86.50532032941677,
	     24.01409824083091, -1.231739572450155, 0.1208650973866179e-2,
	     -0.5395239384953e-5);
  my $stp = 2.5066282746310005;
    
  $x = $xx; $y = $x;
  $tmp = $x + 5.5;
  $tmp = ($x+0.5)*log($tmp)-$tmp;
  $ser = 1.000000000190015;
  foreach $j ( 1 .. 6 ) {
    $y+=1.0;
    $ser+=$cof[$j]/$y;
  }
  return $tmp + log($stp*$ser/$x);
}

use List::Util 'sum';
sub calculate_Pvalue {
	my $array1 = shift;
	my $array2 = shift;
	if (scalar @$array1 <= 1) {
		return 1.0;
	}
	if (scalar @$array2 <= 1) {
		return 1.0;
	}
	my $mean1 = sum(@{ $array1 });
	$mean1 /= scalar @$array1;
	my $mean2 = sum(@{ $array2 });
	$mean2 /= scalar @$array2;
	if ($mean1 == $mean2) {
		return 1.0;
	}
#	say "mean1 = $mean1	mean2 = $mean2";
	my $variance1 = 0.0;
	my $variance2 = 0.0;
	foreach my $x (@$array1) {
		$variance1 += ($x-$mean1)*($x-$mean1);
	}
	foreach my $x (@$array2) {
		$variance2 += ($x-$mean2)*($x-$mean2);
	}
	if (($variance1 == 0.0) && ($variance2 == 0.0)) {
		return 1.0;
	}
	$variance1 = $variance1/(scalar @$array1-1);
	$variance2 = $variance2/(scalar @$array2-1);
	my $array1_size = scalar @$array1;
	my $array2_size = scalar @$array2;
	my $WELCH_T_STATISTIC = ($mean1-$mean2)/sqrt($variance1/$array1_size+$variance2/$array2_size);
	my $DEGREES_OF_FREEDOM = (($variance1/$array1_size+$variance2/(scalar @$array2))**2)
	 /
	(
		($variance1*$variance1)/($array1_size*$array1_size*($array1_size-1))+
		($variance2*$variance2)/($array2_size*$array2_size*($array2_size-1))
	);
#	say "$WELCH_T_STATISTIC	$DEGREES_OF_FREEDOM";
	my $A = $DEGREES_OF_FREEDOM/2;
	my $x = $DEGREES_OF_FREEDOM/($WELCH_T_STATISTIC*$WELCH_T_STATISTIC+$DEGREES_OF_FREEDOM);
	return betain($x, $A, 0.5, lgamma($A)+0.57236494292470009-lgamma($A+0.5));
}

my @d1 = (27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4);
my @d2 = (27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4);
my @d3 = (17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8);
my @d4 = (21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8);
my @d5 = (19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0);
my @d6 = (28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2);
my @d7 = (30.02,29.99,30.11,29.97,30.01,29.99);
my @d8 = (29.89,29.93,29.72,29.98,30.02,29.98);
my @x = (3.0,4.0,1.0,2.1);
my @y = (490.2,340.0,433.9);
my @s1 = (1.0/15,10.0/62.0);
my @s2 = (1.0/10,2/50.0);
my @v1 = (0.010268,0.000167,0.000167);
my @v2 = (0.159258,0.136278,0.122389);
my @z1 = (9/23.0,21/45.0,0/38.0);
my @z2 = (0/44.0,42/94.0,0/22.0);

my @CORRECT_ANSWERS = (0.021378001462867,
0.148841696605327,
0.0359722710297968,
0.090773324285671,
0.0107515611497845,
0.00339907162713746,
0.52726574965384,
0.545266866977794);

my $pvalue = calculate_Pvalue(\@d1,\@d2);
my $error = abs($pvalue - $CORRECT_ANSWERS[0]);
printf("Test sets 1 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@d3,\@d4);
$error += abs($pvalue - $CORRECT_ANSWERS[1]);
printf("Test sets 2 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@d5,\@d6);
$error += abs($pvalue - $CORRECT_ANSWERS[2]);
printf("Test sets 3 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@d7,\@d8);
$error += abs($pvalue - $CORRECT_ANSWERS[3]);
printf("Test sets 4 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@x,\@y);
$error += abs($pvalue - $CORRECT_ANSWERS[4]);
printf("Test sets 5 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@v1,\@v2);
$error += abs($pvalue - $CORRECT_ANSWERS[5]);
printf("Test sets 6 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@s1,\@s2);
$error += abs($pvalue - $CORRECT_ANSWERS[6]);
printf("Test sets 7 p-value = %g\n",$pvalue);

$pvalue = calculate_Pvalue(\@z1,\@z2);
$error += abs($pvalue - $CORRECT_ANSWERS[7]);
printf("Test sets z p-value = %g\n",$pvalue);

printf("the cumulative error is %g\n", $error);
```


I think this should replace the current perl code (because frankly, I can't get all the modules surrounding the current one to work).  I realize that Macs don't have POSIX, so I replaced the 'lgamma' with something more portable.  It should work anywhere with Perl 5.0 and up.
Let me know what you think.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 01:49, 5 January 2018 (UTC)

: I like the existing code.  Try the following simple changes to the current code:

```txt

3c3
< use Math::AnyNum qw(gamma pi);
---
> use Math::Cephes qw(gamma $PI);
38c38
<         (gamma($sa + 1) * sqrt(pi) / gamma($sa + 1.5)))->numify;
---
>         (gamma($sa + 1) * sqrt($PI) / gamma($sa + 1.5)));

```

It gives similar results and the dependency chain should be much easier.  There are a lot of machines that don't have MPFR installed, and some that don't even have GMP.  The downside is that Math::AnyNum is a much more functional module that builds on others.  But Math::Cephes is pretty cool as a standalone native precision library.  I've thought about adding gamma to the ntheory module but it's complicated (for me at least).  [[User:Danaj|Danaj]] ([[User talk:Danaj|talk]]) 22:31, 5 January 2018 (UTC)

I tried the existing module again, and it still won't even install.
The entire purpose of creating this Rosetta Code page was because I had numerous issues installing libraries in the first place, and I kept getting responses like "but the libraries do work!" despite evidence to the contrary.  If a task solution is not portable, it shouldn't be shown.  Also, the Perl translation I've done works very differently than the current one, more accurately, as I show.  Unlike the current code, I can actually run my code, and it should run anywhere.
What is the Rosetta code policy for using an "alternative solution" on Rosetta Code?

: It's unclear with your message above as to whether you tried installing Math::AnyNum again or are replying to my comment about using Math::Cephes.  Without the MPFR library installed in your O/S, Math::MPFR can't install, and therefore Math::AnyNum can't install.  The libraries ''do'' work for those who have the requisite software installed.  Your argument about portability turns into "don't ever use modules outside of core" which I don't believe is the way many people use Perl.
: This is mostly moot as I've added a translation of the C code which uses only List::Util's sum, based in part on your translation above.  Hopefully this will resolve your issues with the page.
: I don't know what sort of official policies there are, but my take for Perl, which is intimately tied with CPAN, is that I like to see a version that uses no modules or only core modules (e.g. List::Util), followed by, if appropriate, versions using modules.  The first version is useful in that it can run everywhere with no module installation, and it shows all the work.  The versions using modules are one or more of faster, shorter, clearer, more functional, or more idiomatic.  Especially if the no-module version is very long, I think it's best to reverse the order -- show the 2-10 line easy solutions, then the long "if you want to write everything by hand:" method.
: It is also not unusual, especially as new tasks get more complex, to see the first code added to a page using a module.  This lets us get a Perl solution up quickly and succinctly, and later someone can write a huge everything-by-hand version.  I suppose there is another discussion of whether it would make sense to have a "common library" pointer so we don't have to keep pasting in the same helper functions on 10+ tasks, which would let one concentrate more on the task.

thank you for your work, this new Perl version is better than what I had done.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 12:47, 10 January 2018 (UTC)

== New title ==
someone changed the title of the page to something which doesn't fit the page... this calculates p-value, it *uses* the Welch's t-test, it doesn't return it.  This is wrong.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 20:11, 7 December 2017 (UTC)
:This sentence does not even mean anything. Please define "using Welch's t-test", and "returning Welch's t-test". [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:55, 13 December 2017 (UTC)

== Task's goal, and pretending to do numerical integration when it has nothing to do with the task ==

Funnily, while the creator of the page disagrees with its renaming to "Welch's t-test", arguing that it's mainly about integration, he keeps giving arguments to the contrary. His last addition to the task description states: "This is meant to translate R's <code>t.test(vector1,vector2, alternative="two.sided", var.equal=FALSE)</code> for calculation of the p-value." That is, this is precisely Welch's t-test. And by the way, this has '''nothing''' to do with integration, in any well-thought implementation of the incomplete gamma function in software libraries (used by R or any other statistical package). If Rosetta Code is in any way supposed to show good practice, using numerical integration in such a case is just... silly.

But even the C program, by the creator of the page, shows an extremely poor implementation of numerical integration, with fixed step-size (65535), probably because it looked large enough to him, even though the integrand is not bounded when the number of degrees of freedoms increases: upper bound of the integral gets nearer to 1, and the denominator sqrt(1-r) gets nearer to 0 near the upper integration bound. With no step control? Seriously? I tried to explain. Now I'm tired of trying and I will simply suggest to open a book on numerical analysis and start learning stuff. Maybe it's not too late.

The sad part is: while Hailholyghost does not seem to understand the task he as created, nor the basics of the mathematics really involved, the task itself is perfectly acceptable and quite interesting. But it's really about Welch's t-test, dude.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:44, 13 December 2017 (UTC)

Eoraptor, if you know how to do the integration better than I did, using the algorithm you posted https://dl.acm.org/citation.cfm?id=131776, while still keeping everything in GNU99 C standard (without unusable libraries, as many do), I'm all ears.  In fact I would appreciate the benefit of your evidently advanced knowledge.  I am a chemist, not a programmer.

Let me clarify why I created this page in the first place: I could not find how to get the p-value in GNU99 C after weeks of google-searching.  I spent hours trying to get existing libraries to work (none would work) and I wanted something portable that would still work in a few months or years.  I couldn't find or understand R's source code for how it calculates this.  So I wrote it in a portable, stable language like C, just based on my memory of high school calculus and Wikipedia pages.  I spent weeks looking for how to do this on the internet, and wanted to save the work I had done.  I am aware that the integration could be better.  I would appreciate your help if you can offer it to make the C implementation better (faster and/or more accurate) using the numerical analysis algorithms you had mentioned.--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 19:54, 13 December 2017 (UTC)

How can I add this page to a list of pages needing attention, specifically implementing Algorithm 708 https://dl.acm.org/citation.cfm?id=131776 integration into the C example?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 20:02, 13 December 2017 (UTC)
:Regarding the algorithm, I'll have a look. It's only a matter of translating Fortran code to C. I do not consider it's mandatory to solve the task, as many languages have built-in features to do that (most "matrix languages" have also common CDF and/or special functions). However, it could be useful in C and others. [http://people.sc.fsu.edu/~jburkardt/ John Burkardt] has done a great work at translating Fortran code to different languages, but TOMS 708 does not seem to be in the list. The original code may even be simplified, given the constraints on the arguments (I need to check, but since there are different code paths according to arguments, it's likely). Even direct integration could be considered (not in production code, but on RC it's acceptable if other solutions are really too complicated), but it requires some care: either transform the integral into something smoother, or control the error. A caution note, too: I'll have to check the copyright. If it's not clear, there are other libraries out there (SLATEC, DCDFLIB, or whatever is used by R, scipy or Octave, which are all open source).
:
:Considering your own problem, did you consider linking Fortran code to your program? It's a very common way to use time-proven Fortran libraries without rewriting them (which is time consuming and error prone). For instance, R, scipy and Scilab heavily use Fortran behind the scene. And nobody would translate LAPACK, for instance. Another possibility would be to use [http://www.netlib.org/f2c/ f2c]. Another, obvious remark: GSL has the [https://www.gnu.org/software/gsl/manual/html_node/Incomplete-Beta-Function.html incomplete beta function]and the [https://www.gnu.org/software/gsl/manual/html_node/The-t_002ddistribution.html t distribution]. On Linux you just have to get it with your favourite package manager, and on Windows you can compile it. Relatively difficult, but doable: you will need MinGW-w64 and MSYS2. It's probably easier to compile a Fortran library and use it with your C program, though.
:
:A piece of general advice for your projects: don't reinvent the wheel, especially for numerical code. It's extremely difficult to get it right. And there are tons of free good libraries (start with Netlib, and the free matrix languages). And by the way, if the task is not too heavy, consider a matrix language in the first place, it's relatively slow, but it's extremely easy to work with. MATLAB is the gold standard, but the free ones are good too. If you have anything related to statistics, I strongly suggest R (and good news: linking C or Fortran code to R is fairly easy).
:[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 06:59, 14 December 2017 (UTC)

I always have difficulties with libraries, in a lot of ways they're like mythical creatures/unicorns to me.  Consider http://people.sc.fsu.edu/~jburkardt/c_src/asa063/asa063.html which has C code (supposedly).  None of it compiles!  It's a lot like the user who overwrote my working Perl code to use some library I can't install.  I couldn't get GSL to work either.  What I wrote for the p-value calculation here may be awkward, but it at least does what it says it would do.

Consider: <code>con@e:~/Scripts/ASA$ gcc -o asa063 asa063.c -Wall -pedantic -std=c99 -lm
asa063.c: In function ‘betain’:
asa063.c:357:10: warning: unused variable ‘betain’ [-Wunused-variable]
   double betain;
          ^
asa063.c: In function ‘timestamp’:
asa063.c:502:10: warning: variable ‘len’ set but not used [-Wunused-but-set-variable]
   size_t len;
          ^
/usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crt1.o: In function `_start':
(.text+0x20): undefined reference to `main'
collect2: error: ld returned 1 exit status</code>

I modified none of this.   Perhaps you know how his asa063.c could compile?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 13:28, 14 December 2017 (UTC)
:Actually, it compiles, but it does not contain a "main" function, and you are trying to compile to an executable. You can't. Use "gcc -c" to only compile to an object file, that you will be able to link later. For instance:
:gcc -c aux1.c
:gcc -c aux2.c
:gcc prog.c aux1.o aux1.o
:There is also a warning, but it's probably harmless. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 15:35, 14 December 2017 (UTC)

==Implementing Algorithm... existing libraries don't calculate correct numbers==

I'm attempting to integrate what I got from the library, but its function <code>betain</code> does not produce the same ratio as the integral (which I know is approximately correct).
How can I use the Betain function in my function <code>Pvalue</code>?


```c

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <limits.h>

/******************************************************************************/
// x =? nu / (t^2 + nu)
// p =? nu/2
// q =? 1/2
// 

double betain ( double x, double p, double q, double beta, int *restrict ifault )

/******************************************************************************/
/*
  Purpose:

    BETAIN computes the incomplete Beta function ratio.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    05 November 2010

  Author:

    Original FORTRAN77 version by KL Majumder, GP Bhattacharjee.
    C version by John Burkardt.

  Reference:

    KL Majumder, GP Bhattacharjee,
    Algorithm AS 63:
    The incomplete Beta Integral,
    Applied Statistics,
    Volume 22, Number 3, 1973, pages 409-411.

  Parameters:
https://www.jstor.org/stable/2346797?seq=1#page_scan_tab_contents
    Input, double X, the argument, between 0 and 1.

    Input, double P, Q, the parameters, which
    must be positive.

    Input, double BETA, the logarithm of the complete
    beta function.

    Output, int *IFAULT, error flag.
    0, no error.
    nonzero, an error occurred.

    Output, double BETAIN, the value of the incomplete
    Beta function ratio.
*/
{
  double acu = 0.1E-14;
  double ai;
 // double betain;
  double cx;
  int indx;
  int ns;
  double pp;
  double psq;
  double qq;
  double rx;
  double temp;
  double term;
  double value;
  double xx;

  value = x;
  ifault = 0;
//Check the input arguments.
  if ( (p <= 0.0) || (q <= 0.0 )){
//    *ifault = 1;
    return value;
  }
  if ( x < 0.0 || 1.0 < x )
  {
    *ifault = 2;
    return value;
  }
/*
  Special cases.
*/
  if ( x == 0.0 || x == 1.0 )   {
    return value;
  }
  psq = p + q;
  cx = 1.0 - x;

  if ( p < psq * x )
  {
    xx = cx;
    cx = x;
    pp = q;
    qq = p;
    indx = 1;
  }
  else
  {
    xx = x;
    pp = p;
    qq = q;
    indx = 0;
  }

  term = 1.0;
  ai = 1.0;
  value = 1.0;
  ns = ( int ) ( qq + cx * psq );
/*
  Use the Soper reduction formula.
*/
  rx = xx / cx;
  temp = qq - ai;
  if ( ns == 0 )
  {
    rx = xx;
  }

  for ( ; ; )
  {
    term = term * temp * rx / ( pp + ai );
    value = value + term;;
    temp = fabs ( term );

    if ( temp <= acu && temp <= acu * value )
    {
      value = value * exp ( pp * log ( xx ) 
      + ( qq - 1.0 ) * log ( cx ) - beta ) / pp;

      if ( indx )
      {
        value = 1.0 - value;
      }
      break;
    }

    ai = ai + 1.0;
    ns = ns - 1;

    if ( 0 <= ns )
    {
      temp = qq - ai;
      if ( ns == 0 )
      {
        rx = xx;
      }
    }
    else
    {
      temp = psq;
      psq = psq + 1.0;
    }
  }

  return value;
}
/******************************************************************************/

double r8_max ( double x, double y )

/******************************************************************************/
/*
  Purpose:

    R8_MAX returns the maximum of two R8's.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    18 August 2004

  Author:

    John Burkardt

  Parameters:

    Input, double X, Y, the quantities to compare.

    Output, double R8_MAX, the maximum of X and Y.
*/
{
  double value;

  if ( y < x )
  {
    value = x;
  } 
  else
  {
    value = y;
  }
  return value;
}

double Pvalue (const double *restrict ARRAY1, const size_t ARRAY1_SIZE, const double *restrict ARRAY2, const size_t ARRAY2_SIZE) {//calculate a p-value based on an array
	if (ARRAY1_SIZE <= 1) {
		return 1.0;
	} else if (ARRAY2_SIZE <= 1) {
		return 1.0;
	}
	double fmean1 = 0.0, fmean2 = 0.0;
	for (size_t x = 0; x < ARRAY1_SIZE; x++) {//get sum of values in ARRAY1
		if (isfinite(ARRAY1[x]) == 0) {//check to make sure this is a real numbere
			puts("Got a non-finite number in 1st array, can't calculate P-value.");
			exit(EXIT_FAILURE);
		}
		fmean1 += ARRAY1[x];
	}
	fmean1 /= ARRAY1_SIZE;
	for (size_t x = 0; x < ARRAY2_SIZE; x++) {//get sum of values in ARRAY2
		if (isfinite(ARRAY2[x]) == 0) {//check to make sure this is a real number
			puts("Got a non-finite number in 2nd array, can't calculate P-value.");
			exit(EXIT_FAILURE);
		}
		fmean2 += ARRAY2[x];
	}
	fmean2 /= ARRAY2_SIZE;
//	printf("mean1 = %lf	mean2 = %lf\n", fmean1, fmean2);
	if (fmean1 == fmean2) {
		return 1.0;//if the means are equal, the p-value is 1, leave the function
	}
	double unbiased_sample_variance1 = 0.0, unbiased_sample_variance2 = 0.0;
	for (size_t x = 0; x < ARRAY1_SIZE; x++) {//1st part of added unbiased_sample_variance
		unbiased_sample_variance1 += (ARRAY1[x]-fmean1)*(ARRAY1[x]-fmean1);
	}
	for (size_t x = 0; x < ARRAY2_SIZE; x++) {
		unbiased_sample_variance2 += (ARRAY2[x]-fmean2)*(ARRAY2[x]-fmean2);
	}
//	printf("unbiased_sample_variance1 = %lf\tunbiased_sample_variance2 = %lf\n",unbiased_sample_variance1,unbiased_sample_variance2);//DEBUGGING
	unbiased_sample_variance1 = unbiased_sample_variance1/(ARRAY1_SIZE-1);
	unbiased_sample_variance2 = unbiased_sample_variance2/(ARRAY2_SIZE-1);
	const double WELCH_T_STATISTIC = (fmean1-fmean2)/sqrt(unbiased_sample_variance1/ARRAY1_SIZE+unbiased_sample_variance2/ARRAY2_SIZE);
	const double DEGREES_OF_FREEDOM = pow((unbiased_sample_variance1/ARRAY1_SIZE+unbiased_sample_variance2/ARRAY2_SIZE),2.0)//numerator
	 /
	(
		(unbiased_sample_variance1*unbiased_sample_variance1)/(ARRAY1_SIZE*ARRAY1_SIZE*(ARRAY1_SIZE-1))+
		(unbiased_sample_variance2*unbiased_sample_variance2)/(ARRAY2_SIZE*ARRAY2_SIZE*(ARRAY2_SIZE-1))
	);
//	printf("Welch = %lf	DOF = %lf\n", WELCH_T_STATISTIC, DEGREES_OF_FREEDOM);
	const double a = DEGREES_OF_FREEDOM/2, x = DEGREES_OF_FREEDOM/(WELCH_T_STATISTIC*WELCH_T_STATISTIC+DEGREES_OF_FREEDOM);
	const unsigned  int N = 65536;//increase N for a tighter convergence
	const double h = x/N;//necessary for integrating with Simpson's method
	double sum1 = 0.0, sum2 = 0.0;
	for(unsigned  int i = 1;i < N+1; i++) {//integrate by Simpson's method (sometimes blows up at i = 0, so I start @ 1
		sum1 += (pow(h * i + h / 2.0,a-1))/(sqrt(1-(h * i + h / 2.0)));
		sum2 += (pow(h * i,a-1))/(sqrt(1-h * i));
	}
//	printf("sum1 = %lf	sum2 = %lf\n",sum1, sum2);
//	double return_value = ((3*h / 8.0) * ((pow(x,a-1))/(sqrt(1-x)) + sum1 +  0))/(exp(lgammal(a)+0.57236494292470009-lgammal(a+0.5)));//0.5723649... is lgammal(0.5)
	double return_value = ((h / 6.0) * ((pow(x,a-1))/(sqrt(1-x)) + 4.0 * sum1 + 2.0 * sum2))/(exp(lgammal(a)+0.57236494292470009-lgammal(a+0.5)));//0.5723649... is lgammal(0.5)
	if ((isinf(return_value) != 0) || (isnan(return_value) != 0) || (return_value > 1.0)) {
		return 1.0;
	} else {
		int *restrict z = 0;
		printf("%g =? %g\n", return_value, betain(x, a, 0.5, exp(lgammal(a)+0.57236494292470009-lgammal(a+0.5)), z));
		return return_value;
	}
}
//-------------------
int main(void) {
	const double d1[] = {27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4};
	const double d2[] = {27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4};
	const double d3[] = {17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8};
	const double d4[] = {21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8};
	const double d5[] = {19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0};
	const double d6[] = {28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2};
	const double d7[] = {30.02,29.99,30.11,29.97,30.01,29.99};
	const double d8[] = {29.89,29.93,29.72,29.98,30.02,29.98};
	const double x[] = {3.0,4.0,1.0,2.1};
	const double y[] = {490.2,340.0,433.9};
	const double v1[] = {0.010268,0.000167,0.000167};
	const double v2[] = {0.159258,0.136278,0.122389};
	const double s1[] = {1.0/15,10.0/62.0};
	const double s2[] = {1.0/10,2/50.0};
	const double z1[] = {9/23.0,21/45.0,0/38.0};
	const double z2[] = {0/44.0,42/94.0,0/22.0};
	
	printf("Test sets 1 p-value = %g\n",Pvalue(d1,sizeof(d1)/sizeof(*d1),d2,sizeof(d2)/sizeof(*d2)));
	printf("Test sets 2 p-value = %g\n",Pvalue(d3,sizeof(d3)/sizeof(*d3),d4,sizeof(d4)/sizeof(*d4)));
	printf("Test sets 3 p-value = %g\n",Pvalue(d5,sizeof(d5)/sizeof(*d5),d6,sizeof(d6)/sizeof(*d6)));
	printf("Test sets 4 p-value = %g\n",Pvalue(d7,sizeof(d7)/sizeof(*d7),d8,sizeof(d8)/sizeof(*d8)));
	printf("Test sets 5 p-value = %g\n",Pvalue(x,sizeof(x)/sizeof(*x),y,sizeof(y)/sizeof(*y)));
	printf("Test sets 6 p-value = %g\n",Pvalue(v1,sizeof(v1)/sizeof(*v1),v2,sizeof(v2)/sizeof(*v2)));
	printf("Test sets 7 p-value = %g\n",Pvalue(s1,sizeof(s1)/sizeof(*s1),s2,sizeof(s2)/sizeof(*s2)));
	printf("Test sets z p-value = %g\n", Pvalue(z1, 3, z2, 3));
	
	const double g41[] = {1.062, 0.774, 0.909};
	const double g412[] = {1.459, 0.674, 0.732};
	const double g414[]= {1.174, 1.406, 1.536};
	
	printf("41 gr -vs- 41.2 gr p-value = %g\n", Pvalue(g41, 3, g412, 3));
	printf("41 gr -vs- 41.4 gr p-value = %g\n", Pvalue(g41, 3, g414, 3));
	printf("41.2 gr -vs- 41.4 gr p-value = %g\n", Pvalue(g412, 3, g414, 3));
	/*
Test sets 1 p-value = 2.137830e-02
Test sets 2 p-value = 1.488426e-01
Test sets 3 p-value = 3.597278e-02
Test sets 4 p-value = 9.077370e-02
Test sets 5 p-value = 1.075156e-02
Test sets 6 p-value = 3.399076e-03
Test sets 7 p-value = 5.272635e-01

with N = 199 and Simpson's 1st method:
Test sets 1 p-value = 2.292482e-02
Test sets 2 p-value = 1.535105e-01
Test sets 3 p-value = 3.857763e-02
Test sets 4 p-value = 9.265142e-02
Test sets 5 p-value = 1.076123e-02
Test sets 6 p-value = 3.414684e-03
Test sets 7 p-value = 5.266487e-01
*/
	return 0;
}

```

--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 11:47, 28 December 2017 (UTC)
:Replace

```c
int *restrict z = 0;
printf("%g =? %g\n", return_value, betain(x, a, 0.5, exp(lgammal(a)+0.57236494292470009-lgammal(a+0.5)), z));
```

:
:With

```c
int z = 0;
printf("%g =? %g\n", return_value, betain(x, a, 0.5, lgammal(a)+0.57236494292470009-lgammal(a+0.5), &z));
```

:
:There is another problem: you damaged the original function, by replacing <code>*ifault = 0;</code> with <code>ifault = 0;</code>, replacing ifault with a null pointer. This will crash the program with a null pointer dereference whenever the arguments passed to betain are incorrect (hence ifault is assigned a value). I didn't check if you introduced another problem in the code.
:Your statement that ''existing libraries don't calculate correct numbers'' is wrong, at least for this one.
:[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 19:31, 28 December 2017 (UTC)

=== Stata translation of Burkardt's C translation of AS 63 ===
For the record, here is a quick Stata translation of this betain implementation.

The original C code can be simplified in several places using shortcuts (it could be simplified even more in C). Also, ifault is not used here: instead, the function returns a missing value if the arguments are invalid. And log(beta(p,q)) is computed inside betain, rather than passed as an argument.


```stata
mata
/*
This code is distributed under the GNU LGPL license. 
Modified 28dec2017
Author:
	Original Fortran 77 version by KL Majumder, GP Bhattacharjee.
	C version by John Burkardt.
	Stata version by Jean-Claude Arbaut.
*/
function betain(x,p,q) {
	/* Check the input arguments. */
	if (p<=0 | q<=0 | x<0 | x>1) return(.)

	/* Special cases. */
	if (x==0 | x==1) {
		return(x)
	}
	
	acu = 1e-15
	lnbeta = lngamma(p)+lngamma(q)-lngamma(p+q)

	/* Change tail if necessary and determine S. */
	psq = p+q
	if (p<psq*x) {
		xx = 1-x
		cx = x
		pp = q
		qq = p
		indx = 1
	}
	else {
		xx = x
		cx = 1-x
		pp = p
		qq = q
		indx = 0
	}
	
	term = ai = value = 1
	ns = floor(qq+cx*psq)

	/* Use the Soper reduction formula. */
	rx = xx/cx
	temp = qq-ai
	if (ns==0) rx = xx
	
	while(1) {
		term = term*temp*rx/(pp+ai)
		value = value+term
		temp = abs(term)
		
		if (temp<=acu & temp<=acu*value) {
			value = value*exp(pp*log(xx)+(qq-1)*log(cx)-lnbeta)/pp
			return(indx?1-value:value)
		}
		
		ai++
		if (--ns>=0) {
			temp = qq-ai
			if (ns==0) rx = xx
		}
		else temp = psq++
	}
}
end
```


A note on the license: I used the same license as John Burkardt, but I am not certain it's permitted. The [http://lib.stat.cmu.edu/apstat/ StatLib site] states that ''The Royal Statistical Society holds the copyright to these routines, but has given its permission for their distribution provided that no fee is charged.'' However, LGPL allows redistribution in nonfree software.

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 21:31, 28 December 2017 (UTC)
