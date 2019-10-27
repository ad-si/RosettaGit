+++
title = "Talk:Monte Carlo methods"
description = ""
date = 2018-05-30T15:32:56Z
aliases = []
[extra]
id = 3061
[taxonomies]
categories = []
tags = []
+++

==Python shell sessions as examples==
I noted that someone had changed another Python shell session used as an example, into the 'normal' definition of a function followed by the shell session just used to show the answer when the function is called.

I don't think this should be done here, as I am attempting to show how the shell might be used for such a task. It is still Python. The repitition of the input expression is because in idle, the built-in graphical IDE for Python, you would hit return in a previous expression to re-enter it. In the non-graphical shell, you can scroll through previous input to re-enter lines. It can give the immediate feedback, and 'spirit of exploration' you get when working with a calculator. --[[User:Paddy3118|Paddy3118]] 05:22, 2 October 2008 (UTC)

== Error formula in C implementation ==

What formula is being used for the error calculation in the [http://rosettacode.org/wiki/Monte_Carlo_methods#C C Implementation]?

At first I thought it was the formula for standard deviation but the code is:

error = val * sqrt(val (1 - val) / sampled) * 4;

The factor 4 is explained because we are not interested in the ratio <math>\pi/4</math>, but in <math>\pi</math> so both the value and the error must be multiplied by 4. The rest of the code translates to:



<math>\sigma = \mu \sqrt{\frac{1}{N} \mu(1 - \mu)}, {\rm \ \ where\ \ } \mu {\rm \ \ is \ \ the \ \ ratio \ \ } \pi/4 {\rm \ \ and\ \ } N {\rm \ \ is \ \ the \ \ number \ \ of \ \ samples \ \ }</math>


But according to [http://en.wikipedia.org/wiki/Standard_deviation#Definition_of_population_values Wikipedia] the formula is this:
 
:<math>\sigma = \sqrt{\frac{1}{N} \sum_{i=1}^N (x_i - \mu)^2}, {\rm \ \ where\ \ } \mu = \frac{1}{N} \sum_{i=1}^N x_i.</math>


Can somebody explain this more clearly? I'm not yet convinced this is correct

: Randomly throwing a point in a square, and it has chance p of being in the circle, while (1-p) chance of otherwise.  If you throw N points and count the number of times n that they landed in the circle, n would follow [[wp:binomial distribution]] (look up the variance formula there).  Here we are taking p = n/N as the ratio between areas of circle and square, but n is subject to statistical fluctuation.  Assuming that we had the senses to throw a large enough N so n/N wouldn't be a completely bogus estimate of p, but we'd still like to know how far off it could be from p's true value.  This is where the variance comes in: it tells you, given N and a rough knowledge of p, how much uncertainty of n (and p) one should expect.

: If you want to use the stddev formula, then each <math>x_i</math> takes the value of either 1 (landing in circle) or 0 (not).  The average is <math>\mu = p</math> as mentioned above; now <math>\sigma^2 = {1\over N} \sum (x_i - p)^2</math>. Note that there are going to be about <math>Np</math> of those <math>x_i</math>s with value <math>1</math>, and <math>N(1-p)</math> with value <math>0</math>, so <math>\sum(x_i - p)^2 \approx Np(1-p)^2 + N(1-p)(0-p)^2 = Np(1-p)</math>.  See how it comes back to the same formula?  --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 06:17, 5 May 2014 (UTC)

:: Thank you very much that explains the origin of the formula. Even though following your reasoning the formula should be:
:: <math> \sum(x_i - p)^2 \approx Np(1-p)</math>, 
:: But because we have a factor of <math> {1\over N} </math> we must take into account, then the resulting stddev should be:
:: <math>\sigma^2 = p(1-p)</math>,
:: Meaning that the formula implemented has an extra factor of <math> {1\over N} </math> inside the square root and a factor of <math> p  </math> outside of the square root, meaning:
:: error = val * sqrt(val * (1 - val) / sampled) * 4, when it should be:
:: error = sqrt(val * (1 - val)) * 4; 
:: Am I missing something else here? Sorry for the intrigue I'm no expert in probability, but I'm curious as to the implementation.-[[User:Chibby0ne|Chibby0ne]] ([[User talk:Chibby0ne|talk]]) 17:18, 17 May 2014 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 18:00, 11 September 2016 ==

Under-tested cosmetic edits made to the task page at 18:00, 11 September 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:20, 22 September 2016 (UTC)

: Now repaired [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:20, 16 November 2016 (UTC)

==other REXX (output) examples==
Here are some other REXX examples (runs):

```txt

scale:    1
           234567890123456789012345678901234567890123456789012345678901234567890123

true pi=  3.141592653589793238462643383279502884197169399375105820974944592307816406+
times= 5000000000000

             100,000 repetitions:  Monte Carlo  pi  is accurate to 3 places.
             200,000 repetitions:  Monte Carlo  pi  is accurate to 4 places.
           2,700,000 repetitions:  Monte Carlo  pi  is accurate to 5 places.
           2,800,000 repetitions:  Monte Carlo  pi  is accurate to 6 places.
          17,700,000 repetitions:  Monte Carlo  pi  is accurate to 8 places.
         682,600,000 repetitions:  Monte Carlo  pi  is accurate to 10 places.

```



```txt

scale:    1
           234567890123456789012345678901234567890123456789012345678901234567890123

true pi=  3.141592653589793238462643383279502884197169399375105820974944592307816406+
times= 5000000000000

             100,000 repetitions:  Monte Carlo  pi  is accurate to 4 places.
             700,000 repetitions:  Monte Carlo  pi  is accurate to 5 places.
           6,000,000 repetitions:  Monte Carlo  pi  is accurate to 6 places.
           6,100,000 repetitions:  Monte Carlo  pi  is accurate to 7 places.
         133,700,000 repetitions:  Monte Carlo  pi  is accurate to 8 places.
         362,300,000 repetitions:  Monte Carlo  pi  is accurate to 9 places.
         623,300,000 repetitions:  Monte Carlo  pi  is accurate to 10 places.

```



```txt

                    1         2         3         4         5         6         7
scale:    1
           234567890123456789012345678901234567890123456789012345678901234567890123

true pi=  3.141592653589793238462643383279502884197169399375105820974944592307816406+
times= 5000000000000

             100,000 repetitions:  Monte Carlo  pi  is accurate to 4 places.
          12,900,000 repetitions:  Monte Carlo  pi  is accurate to 5 places.
          36,200,000 repetitions:  Monte Carlo  pi  is accurate to 6 places.
          38,400,000 repetitions:  Monte Carlo  pi  is accurate to 7 places.
          69,500,000 repetitions:  Monte Carlo  pi  is accurate to 8 places.
         476,200,000 repetitions:  Monte Carlo  pi  is accurate to 10 places.
       1,959,100,000 repetitions:  Monte Carlo  pi  is accurate to 11 places.
       4,006,300,000 repetitions:  Monte Carlo  pi  is accurate to 12 places.

```



```txt

scale:    1
           234567890123456789012345678901234567890123456789012345678901234567890123

true pi=  3.141592653589793238462643383279502884197169399375105820974944592307816406+
times= 5000000000000

             100,000 repetitions:  Monte Carlo  pi  is accurate to 3 places.
             900,000 repetitions:  Monte Carlo  pi  is accurate to 4 places.
           2,100,000 repetitions:  Monte Carlo  pi  is accurate to 5 places.
           4,900,000 repetitions:  Monte Carlo  pi  is accurate to 6 places.
         471,400,000 repetitions:  Monte Carlo  pi  is accurate to 7 places.
         471,600,000 repetitions:  Monte Carlo  pi  is accurate to 8 places.
         491,100,000 repetitions:  Monte Carlo  pi  is accurate to 9 places.

```


== Error in all languages ==
Hello!
Regarding Monte Carlo methods the results are not very good in most languages. You can even get worse result with more samples.
Please if anyone with insight could explain this!

Have you tested RDRAND and Monte Carlo methods? I have found some results on MASM32.com.
Two links:
http://masm32.com/board/index.php?topic=2441.0
http://masm32.com/board/index.php?topic=2432.0
The last is for 64 bit and the mc1.zip is OK.
It looks good for Intel Ivy Bridge but not so good for AMD (slower and higher error).

Best regards
Oldboy 1948
