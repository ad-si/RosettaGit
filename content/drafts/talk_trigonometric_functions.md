+++
title = "Talk:Trigonometric functions"
description = ""
date = 2018-12-28T03:56:59Z
aliases = []
[extra]
id = 2454
[taxonomies]
categories = []
tags = []
+++

Why are the last four pairs of numbers different for the ada example? The program was to use the same angle in degrees and radians so as long as the function is set for degrees or radians it should give the same number. --[[User:Mwn3d|Mwn3d]] 07:14, 8 January 2008 (MST)
:There is no error. 45 degrees is the same angle as 0.7854 (Pi/4) radians. A function set for radians cannot return degrees, and vice versa.--[[User:Waldorf|Waldorf]] 21:31, 8 January 2008 (MST)
::Sorry. I think I was confused because the last two (arccot) seem to be exactly the same line of code. Am I crazy? --[[User:Mwn3d|Mwn3d]] 21:36, 8 January 2008 (MST)
::Theyŕe not exactly the same, but youŕe not crazy either.  I think the line before <code>New_Line;</code> was intended to be

::<code.Put(Item => Arccot(X => Cot(Angle_Radians, Radians_Cycle), Cycle => Radians_Cycle), Aft => 5, Exp => 0);</code>

::but what exists at present is not that. --[[User:TBH|TBH]] 22:16, 8 January 2008 (MST)

The Java example also seems to not fit the requirements for arcsin, arccos, and arctan. --[[User:TBH|TBH]] 11:16, 9 January 2008 (MST)
:When I made the task I forgot that the arc functions return angles. Since they don't take an angle as an argument, I changed the Java example recently so that each function would print its answer in radians and degrees (even though they all start with different angles to get their arguments). I tried to make it more like the Ada example.--[[User:Mwn3d|Mwn3d]] 11:38, 9 January 2008 (MST)
::Ah! I had not noticed the clarified requirements. Now that I understand the requirements, I see that the answers are good for both the Ada and Java examples. The Perl code still needs fixing. The Ada code still has what seems to be an error, as I mentioned yesterday, but it does not affect the output. I've corrected the J code. --[[User:TBH|TBH]] 19:30, 9 January 2008 (MST)

== odd formatting ==

I was trying to add code for IDL, but the "pre" tag doesn't seem to work quite like I expected. Can someone clarify?
: Mediawiki's way of handling "pre" is broken.  Try surrounding the "pre" block with &lt;nowiki&gt;/&lt;/nowiki&gt;. --[[User:Short Circuit|Short Circuit]] 01:00, 2 May 2008 (MDT)


== Notes on precise sin implemented in Rexx ==

One of THE features of Rexx is the virtually unlimited numeric precision.
Now if you want to get sin(0.1) with 30 precise digits
(for whatever reason)

With Numeric Digits 30
sin as implemented in REXX trigonometric functions yields
   0.0998334166468281523068141984103
The implementation of sin in ooRexx' rxmath function yields
   0.09983341664682816   (this is restricted to 16 digits!)
when the precise value (50 digits) is
   0.099833416646828152306814198410622026989915388017978
which should be rounded to
   0.0998334166468281523068141984106

so both implementations above have an incorrect last digit

A possible remedy is to compute the function with a higher precision
and then round the result to the desired one

```txt

ps=0.09983341664682815230681419841062199
   0.0998334166468281523068141984106

```

These lines come from
 pSIN: procedure
 /* Calculate sin(x) to the specified precision */
 parse arg X, P
 Numeric Digits (p+2)
 ps=sin(x)
 Say 'ps='ps
 Numeric Digits p
 Say '   '||(ps+0)
 Return ps+0

--[[User:Walterpachl|Walterpachl]] 19:43, 22 June 2012 (UTC)                   

Of course the Rexx implementation of sin could compute tthis higher precision and round it upon returning the value

: That's what the REXX program shows.  It adds 10 digits to the number of digits (precision) you want to see in the output.
: Also, I long ago discovered that most trig and hyperbolic functions of this type usually require at least four extra (decimal) digits to be used (but only for some values that are near asymptotic points and others near multiples or fractions of pi.  So I added one more digits to be on the safe side.  Since then, I made the addition to an even five for a few hyperbolic functions and then did the same to the trig functions, but used ten here (on Rosetta Code) for safety's sake. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:55, 22 June 2012 (UTC)

I discussed this topic 10 years ago on Vladimir Zabrovsky's Album of (Rexx) algorithms which contains a wealth of code.

: Did you change the REXX variable SHOWDIGS to 30?  If so, I don't receive the same results you post.  I assume you've must have cut and pasted from your own copy of the REXX program as the output format is much different.  I would like to know what your results are for the REXX example as shown in Rosetta Code.  

I get (for  SHOWDIGS=30): 

```txt

              rads= 0.1                              sin= 0.099833416646828152306814198    cos= 0.995004165278025766095561987    tan= 0.100334672085450545058080045

```

and the output (for  SHOWDIGS=35):

```txt


              rads= 0.1                                   sin= 0.09983341664682815230681419841062    cos= 0.99500416527802576609556198780387    tan= 0.10033467208545054505808004578111

```

both of which required a modification to the REXX program; the code changes are:

```rexx
/*REXX program demonstrates some common trig functions (30 digits shown)*/
showdigs=30                            /*show only ten digits of number.*/
numeric digits showdigs+10             /*DIGITS default is  9,  but use */
                                       /*extra digs to prevent rounding.*/

j=.1                   /*just do   1/10 of a radian. for a special run. */
  stuff = '    '           '         rads='show(    (j)),
                                  '   sin='show(sin(j)),
                                  '   cos='show(cos(J))
                                            /*don't let  TAN  go postal.*/
  if abs(j)\==90 then stuff=stuff '   tan='show(tan(j))
  say stuff
 .
 .
 .
```

This isn't probably the best forum to discuss these types of code (result) minutia when code is taken out of context or the modified code or driver code isn't shown to verify your results.  I can be contacted via E-mail to iron out these details before large amounts of chatter are posted. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:55, 22 June 2012 (UTC)



----

It IS! I wanted to point out a problem and its solution.
I don't think people want to understand the huge driver and its details (SHOWDIGS)
but rather want to use the functions offered for their task.




-----


I couldn't disagree more about people not wanting to understand the details.  That is why there are a good number

of comments in the section code and the aforemented REXX code.  These details are important to understand before 

source code is ripped from a working example.  This is the main reason the number of shown digits was increased from

6 to 30. People should plainly see that the example as coded works perfectly.  If another constructed version doesn't

give the expected results, it might be time to re-read the prologue.   Or better yet, use the prologue REXX code.


It's important to note that '''numeric digits nnn''' is not what precision you ''want'', but what precision the

REXX code ''uses''.  One should always specify more than what is liked.  Programmers most often forget that negative

numbers consume an "extra" digit, and it's always better to be safe than sorry.  If one wants 30 digit accuracy, then

be prepared to over-specify the numeric digits (or just use the technique shown in the REXX example on the main page).  


The huge driver is mainly two statements with eight more to invoke the functions:

```rexx
showdigs=30                            /*show only 30 digits of number. */
numeric digits showdigs+10
```

In my other "trig/hyperbolic/math" subroutine (not included here), it has 175 different functions, and 

adding the REXX statements to bump the precision and re-normalize the result for each of the those

functions would be to burdensome and bulky.  In most likelihood, the re-normalization would be a waste of time 

if the invocation program is performing a series of SIN calls, as indeed, the Rosetta Code example is.

The small REXX program (here on Rosetta Code) only has 18 internal functions, and even that small number 

would add a significant amount of code.  But that's what Rosetta Code is for.  Anyone can add more versions,

and it would nice to see such a version.  

This type of REXX code (bumping the precision, re-normalizing) can't be consolidated because of the 

manner in which REXX honors '''numeric digits''' between subroutine calls, so it's impossible to use a REXX 

subroutine to avoid the repetitious code.  Making 175 unique subroutines is one way of solving that problem, 

but that's only a small fraction (well, ok, 1.2%) of the the total number of functions that my (written 

elsewhere) trig/hyperbolic/math package has, but clearly, one can see the problem with adding that large a 

number of external functions.  

There is another problem with function name collision, which is another topic for another time and worthy

of a detailed discussion elsewhere. 

So instead, I did what was suggested in the (prologue) commentary, that is: to increase the precision 

higher than one needs, and just use what precision is needed from that time on.  More whitespace was added  

around that part of the section comments so it can more easily be seen what was written. If one want to use 

the SIN function and ignore the prologue (as far as intent), then they'll get what they programmed for.

Extending the precision is basic REXX programming and is a common practice that is soon adapted by most.  

The code acknowledges that more precision is needed than what is wanted, so the proglogue for the 

REXX example (on the main page) does that with the '''showdigs''' variable, as the section and REXX code comments 

clearly state. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:23, 23 June 2012 (UTC) 



-----

I did not modify anything!
Just took sin (and the routines used by it) out of the rosetta-box 
and used it in my program as follows:
   
 Numeric Digits 30
 Say sin(0.1)
 Numeric Digits 32
 Say sin(0.1)
 Exit
 /******* procedures ex REXX trigonometric functions ***************************/
 sin: procedure; arg x;  x=r2r(x);  numeric fuzz min(5,digits()-3)
                 if abs(x)=pi() then return 0;   return .sinCos(x,x,1)
 .sinCos: parse arg z 1 p,_,i;  x=x*x
          do k=2 by 2; _=-_*x/(k*(k+i));z=z+_;if z=p then leave;p=z;end; return z
 r2r:   return arg(1)//(2*pi())         /*normalize radians?1 unit circle*/
 pi: return,                            /*a bit of overkill,  but hey !! */
 3.1415926535897932384626433832795028841971693993751058209749445923078164062862

The output is

```txt

0.0998334166468281523068141984103
0.099833416646828152306814198410619

```

What I want is that the first call gives me the 30 correct digits.
This can be done by internally (in sin) raise the precision and return theresult rounded to the desired precision.

Note that external funtions don't inherit the caller's precision,
so having an extra (optional) argument p is an option.
Also the possiblity of x in sin(x) is radians or degrees is a useful extension implemented in the ooRexx function package.
--[[User:Walterpachl|Walterpachl]] 07:13, 23 June 2012 (UTC)

----
To not only ask for solutions, here is my version of sin according to my specs:
 SIN: procedure
 /**********************************************************************
 * Return the value of sine in the inherited or specified precision
 * a the argument in rad or degrees
 * u unit of a ('R', blank. or not specified: radians)
 *             ('D' degrees)
 * p the desired precision of the result
 * If not specified: The digits() of the caller (when internai procedure
 *                   9, The default when external procedure
 * 23.06.2012 Walter Pachl
 **********************************************************************/
 parse arg a,u,p
 if P="" then                           /* p not specified            */
   P=digits()                           /* use digits of caller or 9  */
 numeric digits (p+4)                   /* increase precision         */
 pi=pi()
 Select
   When u=' ' Then x=a                  /* not specified: radians     */
   When u='R' Then x=a                  /* radians                    */
   When u='D' Then                      /* degrees                    */
     x=a*pi/180                         /* convert to radians         */
   Otherwise Do
     Say 'Invalid second argument to sin ('u'),'
     Say 'must be R, D, or blank.'
     Signal Syntax
     End
   End
 x=abs(x)                               /* sign considered later      */
 f=1                                    /* factorial seed             */
 term=x                                 /* first term                 */
 Sum=x                                  /* first value                */
 x2=x**2                                /* loop invariant x-square    */
 do i=3 by 2                            /* now work the series        */
   f=i*(i-1)                            /* next factorial part        */
   term=-term*x2/f                      /* next term                  */
   NewSum=Sum+Term                      /* new value                  */
   If NewSum=Sum Then Leave             /* convergence reached        */
   Sum=NewSum                           /* remember the value         */
   End
 Numeric Digits p                       /* switch to desired precision*/
 Return sign(a)*(Sum+0)                 /* return rounded result      */
                                   /* considering the argument's sign */
 PI: Return '3.14159265358979323846264338327950288419'||,
            '7169399375105820974944592307816406286208'||,
            '9986280348253421170679821480865132823066'||,
            '4709384460955058223172535940812848111745'||,
            '028410270193852110555964462294895493038196'
--[[User:Walterpachl|Walterpachl]] 18:08, 23 June 2012 (UTC)

-----

The REXX program example on the main page was modeled after the PL/I and Fortran languages in that the (trig) function this is called is named by want the argument is specified as, that is, degrees or radians; radians being the default or "simple" or "pure" name, the degree version usually has a '''D''' appended to the function name.  Once you make the programmer specify the type of argument for the none-default, there's more documentation involved and error checking.  

The REXX philosophy would be to take (in the above case) the 2nd argument and just use the first character as being significant (as the '''strip''', '''arg''', '''date''', '''time''', '''datatype''', '''verify''', '''trace''', and other BIFs do).

So one could code: ''y=sin(z,'degrees')'' for instance. 

Also, the aforementioned BIFs also accept the option in lower and mixed case, not just uppercase.

The nice thing about allowing the programmer to specify the units is that other units can be added besides degrees, '''grads''' (and it variant spellings) and '''mils'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:18, 23 June 2012 (UTC)


For those that are interested:



A '''grad''' is 16 '''mils'''.

A '''grad''' is about 1/51.5 '''radians'''.

A '''grad''' is 9/10 of a '''degree'''.

A '''degree''' is 9/10 of a '''mil'''.



There are     2pi  radians  in a circle.

There are    360   degrees  in a circle.

There are    400   grads    in a circle.

There are  6,400   mils     in a circle.


 -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:50, 23 June 2012 (UTC)


----
While my 'precise sin' function above was triggered by the task 'Trigonometric Functions'
it has no further relationship to the cited task.
What I wanted was that this little program shows 30 correct digits
(the value to be tested for being the 'real' value of sin(0.1) rounded to 30 digits)
 /* REXX */
 Numeric Digits 30
 Say sin(0.1)     /* if 'my' sin is included an internal procedure */
 Say sin(0.1,,30) /* if it is used as external procedure           */

--[[User:Walterpachl|Walterpachl]] 07:51, 24 June 2012 (UTC)

: I don't understand, if you're discussing something that has no further relationship to the cited task, then why complain out the REXX example in the cited task? It isn't fair nor appropriate to use a subroutine taken out of context (i.e., not use the proglogue).  Please re-read the REXX section (especially the boxed comments). -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:49, 24 June 2012 (UTC)

: I went back and put the appropriate REXX header section comments in a box so it could be more easily read ''and'' catch one's attention.  I don't know how to make it any more plainer or clearer than that.  If one follows ''either'' of the two suggested techniques to accommodate the wanting of more accuracy, then all of the REXX example subroutines on the main page perform perfectly well with ''either'' technique and return precise results. To reiterate, if you want 30 "precise" digits, then specify '''numeric digits 40'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:49, 24 June 2012 (UTC)
