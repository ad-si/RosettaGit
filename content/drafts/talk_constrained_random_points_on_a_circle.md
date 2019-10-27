+++
title = "Talk:Constrained random points on a circle"
description = ""
date = 2016-11-16T00:32:44Z
aliases = []
[extra]
id = 8235
[taxonomies]
categories = []
tags = []
+++

==Task Description==
I know this is going to seem knit-picky but I think this would be a clearer description of the task:

'Generate 100 <x,y> coordinate pairs such that x and y are integers sampled from the uniform distribution with the condition that <math>10 \leq \sqrt{ x^2 + y^2 } \leq 15 </math>. Then display/plot them. The outcome should be a "fuzzy" circle. The actual number of points plotted may be less than 100, given that some pairs may be generated more than once.

There are several possible approaches to accomplish this. Here are two possible algorithms.

1) Generate random pairs of integers and filter out those that don't satisfy this condition:

    <math>10 \leq \sqrt{ x^2 + y^2 } \leq 15 </math>.

2) Precalculate the set of all possible points (there are 404 of them) and select randomly from this set.'

Does anyone agree? Disagree? [[User:Cferri|Chris Ferri]] 23:13, 8 September 2010 (UTC)

: I agree. --[[User:Ce|Ce]] 07:13, 9 September 2010 (UTC)

== Not 100 points ==
There are only 89 points in the circle shown in the verilog example output. This is no surprise, because AFAICS the algorithm doesn't make sure that the same point isn't chosen twice. Now given that it's the first example, I guess it's what was meant by the task description, but then the task description probably should be changed to reflect the fact that less points are OK. --[[User:Ce|Ce]] 10:55, 3 September 2010 (UTC)

I've fixed the description: it would be a poor RNG that doesn't produce duplicates --[[User:Davewhipp|Dave]]

: You could have a good RNG and yet get exactly 100 points by simply checking for duplicates and continuing until you have 100 points (or alternatively, make a list of all points, random-shuffle it, and take the first 100 items from the resulting list).
: BTW, the revised description isn't quite right: The points don't overlap; you get the ''same'' point several times. --[[User:Ce|Ce]] 16:31, 3 September 2010 (UTC)

:: The Algol68 example has 257 points plotted, ... for the life of me, I couldn't find where it restricted the number of points to 100.

:: The PL/I example has only 39 points plotted, it's overly restricting the points (choosing a random point, and THEN see it the point is in the annulus). 
Am I following the proper protocol for drawing attention to an example's deficiencies, or should I live by the motto: "people in glass houses shouldn't throw stones"? -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:59, 23 April 2012 (UTC)
-- [[User:Gerard Schildberger|Gerard Schildberger]]

== How to check the code ==

If you increase the number of points produced to 10k, you should get output rather like this (generated with Tcl version; your version may differ). This lets you check that the spread of points produces the expected annulus. –[[User:Dkf|Donal Fellows]] 11:00, 3 September 2010 (UTC)

```txt

               X               
          XXXXXXXXXXX          
        XXXXXXXXXXXXXXX        
      XXXXXXXXXXXXXXXXXXX      
     XXXXXXXXXXXXXXXXXXXXX     
    XXXXXXXXXXXXXXXXXXXXXXX    
   XXXXXXXX         XXXXXXXX   
   XXXXXXX           XXXXXXX   
  XXXXXX               XXXXXX  
  XXXXXX               XXXXXX  
 XXXXXX                 XXXXXX 
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
XXXXXX                   XXXXXX
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
 XXXXX                   XXXXX 
 XXXXXX                 XXXXXX 
  XXXXXX               XXXXXX  
  XXXXXX               XXXXXX  
   XXXXXXX           XXXXXXX   
   XXXXXXXX         XXXXXXXX   
    XXXXXXXXXXXXXXXXXXXXXXX    
     XXXXXXXXXXXXXXXXXXXXX     
      XXXXXXXXXXXXXXXXXXX      
        XXXXXXXXXXXXXXX        
          XXXXXXXXXXX          
               X               

```


== Uniform distribution ==

I'm not very good with stats, but I've seen discussions pop up before about different distributions of points. Is uniform vs normal vs (something?) a significant component of the task? How may it be verified with at most 100 points? --[[User:Short Circuit|Michael Mol]] 12:27, 3 September 2010 (UTC)

I guess that if you count the number of points that lie on a line that passes through the center, then the count should not depend on the angle of that line. So it would be wrong to simply pick a uniform-random value of x and then a uniform-random of y at that location, because that would tend to lead to a higher density of points on the left and right sides (look at the maximally dense version above: there are 12 possible values of y at x==0; but only one at x == +/- 15) -- and thus a greater number of points away from the x-axis. A common mistake when generating a set of linked random variables is that the distribution depends on the order in which they are generated.

So, take this Perl code:


```txt

my @bitmap = map { " " x 32 } 0 .. 33;
for (1 .. 100) {
    my $x = int rand(31) - 15;


    my $max = sqrt( 225-($x*$x) );
    my $min = 100-($x*$x);
    $min = $min > 0 ? sqrt $min : 0;

    my $y = int rand( 1+$max-$min ) + $min;
    $y = -$y if rand() < .5;


    $x += 16;
    $y += 16;
    #print "$x $y\n";
    substr( $bitmap[$y], $x, 1, "#" );
}

print "$_\n" for @bitmap;


               # #
          #    ##    #
       #   ## ##  ##   #
      #        ##  #   #
         # # # #   #  # #
      #           # #
        ##          #   #
        #
  #                      #
     #                       #
 ## #                    #
  #
 #
                           #
 ## #                         #
  #                        #
    ##                      #

                             #
 #                      #

       #                #  #
   #     #
        ## #      # #
        #     #   #
     #  # #       ##  #
        # #  #      ##
            #       #
             # #

```


Here you can see that the number of points near top/bottom is greater than that for left/right sides of the plot. For this particular case, the simplest way to check for the bias is to count the number of points where abs(y) > abs(x) (this effectively partitions the plot using 45 degree lines) -- for my "bad" code I see a ratio (over multiple runs) of 54:46 in favor of the top/bottom quadrants over left/right. Counted another way, 79% of random seeds result in a plot with more top/bottom points than left/right. -[[User:Davewhipp|Dave]]

: An example of a more subtle error is to pick the random point using a polar coordinate system (i.e., using a random distance over the given range and a random angle). The problem is that the distribution of random points is not even w.r.t. area when picked that way; points that are closer in will be more tightly packed. It becomes much more noticeable with a wider annulus. –[[User:Dkf|Donal Fellows]] 15:07, 3 September 2010 (UTC)

:: Using a polar coordinate system isn't by itself an error. Using an uniform distribution for the radial coordinate is, however. --[[User:Ce|Ce]] 16:34, 3 September 2010 (UTC)

::: theta=rand() should be fine, but I'm having a mental block figuring out what to multiply r by to fix its distribution. atan(r)? --[[User:Short Circuit|Michael Mol]] 18:02, 3 September 2010 (UTC)

:::: Since <math>\mathrm{d}^2 r = r \mathrm{d}r \mathrm{d}\phi</math> and <math>2 r \mathrm{d}r = \mathrm{d}(r^2)</math>, I'd say <math>r^2</math> should be evenly distributed between <math>r_{min}^2</math> and <math>r_{max}^2</math>, and you then of course get <math>r</math> by taking the square root. However note that this is basically a brain dump. --[[User:Ce|Ce]] 19:16, 3 September 2010 (UTC)

== Disappearing code? ==

I remember writing and posting a J implementation, and yet I do not see it in the history for the page.  Does anyone know what is up with that?  (Does history get lost if someone undoes an edit and then edits forward from that point?)  Anyways, I will re-implement it, it was simple enough.  But I am bothered by the absence of history.  --[[User:Rdm|Rdm]] 18:04, 3 September 2010 (UTC)
:If someone undid an edit an undo would show up in the page history and your edit would still be there. It doesn't seem to be in the history so unless someone did a rollback (which I don't think I've seen done for any page) or deleted the page and re-created it your edit would be there. --[[User:Mwn3d|Mwn3d]] 21:39, 3 September 2010 (UTC)
:: Ok, then I must have just hit preview or something... ouch... --[[User:Rdm|Rdm]] 22:22, 3 September 2010 (UTC)
:::That sounds like an easy mistake. If you suspect that examples are lost like that again though still let us know. It's important that we look into possible bugs just in case. --[[User:Mwn3d|Mwn3d]] 00:14, 4 September 2010 (UTC)

== C code ==

<s>The C code logic is incorrect due to its "not choosing same point twice to speed up" idea.  I'll mark it so, and if noone wants to do it I'll rewrite it later.</s> Done --[[User:Ledrug|Ledrug]] 20:09, 28 June 2011 (UTC)


==Main formulae rendered invisible to many browsers by cosmetic edits==

Cosmetic edits made to the task page at 07:56, 30 April 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left the main formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 09:12, 21 September 2016 (UTC)

: Visibility of formulae now restored by reverting the task description to its state before the inadvertently destructive edit of 07:56, 30 April 2016. (Simply removing redundant white space in &lt;math&gt; tags, and redundantly doubled &lt;big&gt; tags, did not prove enough to restore the visibility of server-side formula graphics in this case) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:32, 16 November 2016 (UTC)
