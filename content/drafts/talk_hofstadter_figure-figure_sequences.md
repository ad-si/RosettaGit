+++
title = "Talk:Hofstadter Figure-Figure sequences"
description = ""
date = 2019-03-03T21:34:30Z
aliases = []
[extra]
id = 10708
[taxonomies]
categories = []
tags = []
+++

==No max n==
That statement is there to explicitly exclude solutions that used a fixed sized array, say of a 1000 elements and an empty array, then moved elements between the two arrays.

Mind you, an algorithm that started with fixed array sizes and doubled their sizes as necessary would be OK. --[[User:Paddy3118|Paddy3118]] 08:37, 22 October 2011 (UTC)

== S(n) ==

''The sequence S(n) is further defined as the sequence of positive integers not present in R(n).''

I think this should be '''S(n) is defined as the nth integer in the sequence of positive integers not present in R(n)'''. If S(n) is itself a sequence then R(n) would be a sequence, but the example R(n) values suggest that R is a sequence and R(n) is an integer from that sequence. --[[User:Rdm|Rdm]] 16:34, 22 October 2011 (UTC)

:Hmm. You're right, but then I can't help but think that S(n) can stand for both an integer given a ''particular'' value of n, or the sequence when thinking of n as being an arbitrary value? It seems to me that it can be either or both, but either way, would you go further and say that you could not determine the meaning of the sentence? 

:I'm inclined to leave it as-is. --[[User:Paddy3118|Paddy3118]] 17:44, 22 October 2011 (UTC)

::In the general case, yes, we might use S(n) to refer to a sequence.  However, for this to be accurate, n needs to be an unbound value.  And in that particular sentence, we are talking about n having a specific value (which is used in R(n)).  Anyways, from my point of view the phrasing is confusing (and opens up questions like: is it possible for S(n) to contain values for some value of n which are not present in later values of n?  And rather than delve into the issues that I would need to tackle to determine whether or not this could be a relevant topic, I would rather the notation be self consistent). --[[User:Rdm|Rdm]] 17:56, 22 October 2011 (UTC)

:::Not really, this aspect of the definition is present in the references too. I suspect that it may be a part of the original description cited as: D. Hofstadter, "Gödel, Escher, Bach", p. 73, but I don't have it to hand at the moment to check. When I first saw their definition I found it confusing at first too, but that is what made it interesting when trying to code it.

::: When I had finished the Python version I checked it with tables of the first 1000 values refered to from Sloane: [http://oeis.org/A005228/b005228.txt here] for R and [http://oeis.org/A030124/b030124.txt here] for S, although the table for S has an off-by-one error. --[[User:Paddy3118|Paddy3118]] 18:31, 22 October 2011 (UTC)

::: Another ref. with a similar definition: [http://books.google.co.uk/books?id=aFDWuZZslUUC&pg=PA1385&dq=%22Figure-Figure+sequences%22+Hofstadter,+%22G%C3%B6del,+Escher,+Bach%22,+p.+73&hl=en&ei=cw2jTt7OBMiA8gOD78zYBQ&sa=X&oi=book_result&ct=result&resnum=1&ved=0CDEQ6AEwAA#v=onepage&q&f=false CRC concise encyclopedia of mathematics By Eric W. Weisstein pp 1385]. --[[User:Paddy3118|Paddy3118]] 18:40, 22 October 2011 (UTC)

:::: Ok, well, I think that's sloppy use of terminology, but I suppose that if they are not considering any possibilities involving sequences of sequences that the sloppiness becomes invisible.  --[[User:Rdm|Rdm]] 19:08, 23 October 2011 (UTC)

::::: It is pretty common to say "''S(n)''" while meaning "sequence ''S'' with ''n'' denoting its index", and it's unambiguous.  For one thing, the very first sentence already said "sequence of positive integers", which is pretty impossible to be misunderstood as "sequence of sequences of integers".  This is how human discuss math using a natural language, there's no need to exercise a context-free parser here. --[[User:Ledrug|Ledrug]] 20:11, 23 October 2011 (UTC)

:::::: That's not the issue.  S(n) would still be an integer sequence if S was a sequence of sequences.  After some thought, though, this appears to be an equivalent interpretation.  --[[User:Rdm|Rdm]] 11:32, 24 October 2011 (UTC)

==Factor solution==
Marked incorrect as the task asks that particular ranges of values of ffr and ffs be collected and compared to the range of integers 1..1000. The current solution starts of by ignoring ffr and ffs then assuming that the first 1000 values of something are equal to ... --[[User:Paddy3118|Paddy3118]] 19:46, 8 December 2011 (UTC)

==timings for the REXX solutions==
I normally don't include timings for the REXX solutions that I post, but when I saw the 2<sup>nd</sup> REXX example's timings, 

I decided to go back and include the timings here as the REXX 2<sup>nd</sup> example's timings seemed a bit high.


I didn't expect a difference of several orders of magnitude.

```rexx
/*REXX program  calculates and verifies  the  Hofstadter Figure─Figure sequences.       */
call time 'Reset████████████████████████████████████████████████████████████████████████████████'
parse arg x top bot .                            /*obtain optional arguments from the CL*/
if   x=='' |   x==","  then   x=  10             /*Not specified?  Then use the default.*/
if top=='' | top==","  then top=1000             /* "      "         "   "   "      "   */
if bot=='' | bot==","  then bot=  40             /* "      "         "   "   "      "   */
low=1;         if x<0  then low=abs(x)           /*only display a  single   │X│  value? */
r.=0;  r.1=1;  rr.=r.;  rr.1=1;   s.=r.;  s.1=2  /*initialize the  R, RR, and S  arrays.*/
errs=0                                           /*the number of errors found  (so far).*/
             do i=low  to abs(x)                 /*display the 1st  X  values of  R & S.*/
             say right('R('i") =",20) right(FFR(i),7) right('S('i") =",20) right(FFS(i),7)
             end   /*i*/
                                                 /* [↑]  list the 1st X Fig─Fig numbers.*/
if x<1  then exit                                /*if X isn't positive, then we're done.*/
$.=0                                             /*initialize the memoization ($) array.*/
             do m=1  for  bot;  r=FFR(m);  $.r=1 /*calculate the first forty  R  values.*/
             end   /*m*/                         /* [↑]  ($.)  is used for memoization. */
                                                 /* [↓]  check for duplicate #s in R & S*/
             do n=1  for top-bot;     s=FFS(n)   /*calculate the value of  FFS(n).      */
             if $.s  then call ser 'duplicate number in R and S lists:' s;   $.s=1
             end   /*n*/                         /* [↑]  calculate the 1st 960 S values.*/
                                                 /* [↓]  check for missing values in R│S*/
             do v=1  for top;  if \$.v  then  call ser     'missing R │ S:'    v
             end   /*v*/                         /* [↑]  are all 1≤ numbers ≤1k present?*/
say
if errs==0  then say 'verification completed for all numbers from  1 ──►' top "  [inclusive]."
            else say 'verification failed with'      errs      "errors."
say 'and took' format(time('Elapsed█████████████████████████████████████████████████████████████████'),,2) "seconds."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
FFR: procedure expose r. rr. s.; parse arg n     /*obtain the number from the arguments.*/
     if r.n\==0  then return r.n                 /*R.n  defined?  Then return the value.*/
     _=FFR(n-1) + FFS(n-1)                       /*calculate the  FFR  and  FFS  values.*/
     r.n=_;       rr._=1;        return _        /*assign the value to R & RR;   return.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
FFS: procedure expose r. s. rr.; parse arg n     /*search for not null  R or S  number. */
     if s.n==0  then do k=1  for n               /* [↓]  1st  IF  is a  SHORT CIRCUIT.  */
                     if s.k\==0  then if r.k\==0  then iterate       /*are both defined?*/
                     call FFR k                  /*define  R.k  via the  FFR  subroutine*/
                     km=k-1;     _=s.km+1        /*calc. the next  S  number,  possibly.*/
                     _=_+rr._;   s.k=_           /*define an element of  the  S  array. */
                     end   /*k*/
     return s.n                                  /*return   S.n   value to the invoker. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser: errs=errs+1;    say  '***error***'  arg(1);                  return
```

'''output'''   when using the default inputs:

```txt

              R(1) =       1               S(1) =       2
              R(2) =       3               S(2) =       4
              R(3) =       7               S(3) =       5
              R(4) =      12               S(4) =       6
              R(5) =      18               S(5) =       8
              R(6) =      26               S(6) =       9
              R(7) =      35               S(7) =      10
              R(8) =      45               S(8) =      11
              R(9) =      56               S(9) =      13
             R(10) =      69              S(10) =      14

verification completed for all numbers from  1 ──► 1000   [inclusive].

and took 0.22 seconds.

```

The (above) example was run under Windows 7 on an air-gap PC (3.2 GHz) using Regina REXX version 3.9.1.




==Formulae hidden to most browsers by under-tested cosmetic edits at 18:19, 28 August 2016 ==

Under-tested cosmetic edits made to the task page at 18:19, 28 August 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:50, 22 September 2016 (UTC)

: Visibility of formulae now restored for mainstream browsers like Chrome, IE Edge, Safari, Opera etc [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:59, 21 November 2016 (UTC)
