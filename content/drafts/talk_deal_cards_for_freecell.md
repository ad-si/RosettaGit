+++
title = "Talk:Deal cards for FreeCell"
description = ""
date = 2017-10-08T22:37:04Z
aliases = []
[extra]
id = 10546
[taxonomies]
categories = []
tags = []
+++

==task?==
I see that although shuffle is not specified we have two implementations.  But I have no way of knowing if they are correct implementations.  Perhaps we should have an example?  With this starting seed, we have this deck of cards?  --[[User:Rdm|Rdm]] 11:15, 19 September 2011 (UTC)


###  Example 


I know this doesn't give the shuffle algorithm but ... it gives the result for seed=1 <http://www.solitairegames4all.com/solitaire.php?card-game=Freecell+Solitaire&game=99> and appears compatible with <http://freecellgamesolutions.com/ds/?g=1&p=FpD&v=All>


```txt
  1  2  3  4  5  6  7  8
 JD 2D 9H JC 5D 7H 7C 5H
 KD KC 9S 5S AD QC KH 3H
 2S KS 9D QD JS AS AH 3C
 4C 5C TS QH 4H AC 4D 7S
 3S TD 4S TH 8H 2C JH 7D
 6D 8S 8D QS 6C 3D 8C TC
 6S 9C 2H 6H
```


There is a reference to the shuffle algorithm here <http://www.solitairelaboratory.com/mshuffle.txt>

--[[User:Dgamey|Dgamey]] 14:26, 19 September 2011 (UTC)

==32 bit Perl fix==

The sample code for Perl works for perl 5.16 64 bit but not for perl 5.8 32 bit (e.g. game# 1070). This fix works for both.


```txt
#       return (($s = ($s * 214013 + 2531011) & 0x7fffffff) >> 16 );
        return (($s = ($s * 214013 + 2531011) % 2**31     ) >> 16 ); # fix for 32 bit perl
```


==equation clarification==
The equation:

::*   <big><math>state_{n + 1} \equiv 214013 \times state_n + 2531011 \pmod{2^{31}}</math></big>
should probably read
::*   <big><math>state_{n + 1} \equiv [ 214013 \times state_n + 2531011 ] \pmod{2^{31}}</math></big>
to indicate that the modulus is for the entire equation, not just the last term;   even though by inspection, it becomes obvious what was meant. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:33, 11 June 2015 (UTC)


==Formulae hidden to most browsers by under-tested cosmetic edits at 21:04, 15 May 2016 ==

Under-tested cosmetic edits made to the task page at 21:04, 15 May 2016, including the injection of spaces around expressions in &lt;math&gt; tags, have left some or all of the task description formulae completely invisible to all browsers which display the graphic file version of formulae rather than processing the MathML (this is, in fact, the majority of browsers). The MediaWiki processor does not currently expect such spaces, and generates syntactically ill-formed HTML if they are introduced. Other aspects of these cosmetic edits may further compound the problem. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:59, 22 September 2016 (UTC)

: Lost visibility now restored [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:28, 21 November 2016 (UTC)

==support for negative games==
Most people aren't aware of FreeCell's undocumented support for:
:::*   game   -1       and
:::*   game   -2


Windows 7 (and up) have undocumented support for two additional games:
:::*   game   -3       and
:::*   game   -4


You can't possibly win games    -1   and   -2.

You can't possibly lose games   -3   and   -4. 


It is entertaining to watch the   -3   and   -4   games play out.


By the way, I'd be interested if Windows Vista supports games   -3   and   -4.

Also, I'd be interested if Windows 8 (and higher) support more negative games.



Here is a REXX version that supports those (minus) games:

```rexx
/*REXX program deals cards for a specific  FreeCell solitaire  card game  (0 ──► 32767).*/
numeric digits 15                                /*ensure enough digits for the random #*/
parse arg game cols .                            /*obtain optional arguments from the CL*/
if game=='' | game==","  then game=1             /*No game specified?  Then use default.*/
if cols=='' | cols==","  then cols=8             /* " cols     "         "   "     "    */
state=game                                       /*seed random # generator with game num*/
                  suit= '♣♦♥♠'                   /*default: ASCII symbols for the suits.*/
if game< -1  then suit= '♠♥♦♣'                   /*special suit order for games -2 -3 -4*/

                  rank= 'A23456789tJQK'          /*t  in the rank represents a ten (10).*/
if game==-1  then rank= 'AQ3t587694J2K'          /*a special rank for a game of  -1.    */
if game==-2  then rank= 'A7K6Q5J4t3928'          /*"    "      "   "  "   "   "  -2.    */
if game==-3  then rank= 'K6Q5J4t3928A7'          /*"    "      "   "  "   "   "  -3.    */

minus4= space('K♣Q♣t♦8♥6♣5♦3♣2♦ K♥J♣t♣8♦6♥5♣3♥2♣ K♦J♦9♣8♣6♦4♣3♦A♣ K♣J♦9♥7♣6♣4♥3♣A♥' ,
              'Q♣J♣9♦7♥5♣4♦2♣A♦ Q♥t♣9♣7♦5♦4♣2♥A♣ Q♦t♥8♣7♣', 0)    /*tableau for game= -4*/

if 8=='f8'x  then do                             /*EBCDIC?  Then use letters for suits. */
                  suit  =translate(suit,   "cdhs", suit)
                  minus4=translate(minus4, "cdhs", suit)
                  end
pad=left('', 13)                                 /*used for indentation for the tableau.*/
say center('tableau for FreeCell game' game, 50, "─")   /*show title for FreeCell game #*/
say                                              /* [↓]  @  is an array of all 52 cards.*/
#=-1;  do   r=1  for length(rank)                /*build the deck  first   by the rank. */
         do s=1  for length(suit);       #=#+1   /*  "    "    "  secondly  "  "  suit. */
         @.#=substr(rank, r,1)substr(suit, s,1)  /*build the $ array one card at at time*/
         end   /*s*/                             /* [↑]  first card is number  0 (zero).*/
       end     /*r*/                             /* [↑]  build deck per FreeCell rules. */
$=pad                                            /*@: cards to be dealt, eight at a time*/
r=1
s=0;   do cards=51  by -1  for 52;    s=s + 1    /* [↓]  deal the cards for the tableau.*/
       ?=rand()  //  (cards + 1)                 /*get next rand#;  card # is remainder.*/

       if game<0 & game>-4  then $=$  nega()     /*handle special for games  -1, -2, -3 */
       if game=-4           then $=$  neg4()     /*   "      "      " game   -4         */

       if game>=0  then $=$  @.?;   @.?= @.cards /*swap two cards:  use random and last.*/

       if words($)==cols  then do;  say $; $=pad /*deal FreeCell cards for the tableau. */
                               end
       end   /*cards*/                           /*normally, 8 cards are dealt to a row.*/
                                                 /* [↓]  residual cards may exist.      */
if $\=''  then say $                             /*Any residual cards in the tableau ?  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rand: state=(214013*state + 2531011) // 2**31;   return state % 2**16  /*FreeCell rand#*/
nega: if s==5  then do;  r=r+1;  s=1;  end;      return substr(rank,r,1)substr(suit,s,1)
neg4: return substr(minus4, s+s-1, 2)
```
 
{{out|output|text=  when using the input of:   <tt> -1 </tt>}}

```txt

───────────tableau for FreeCell game -1───────────

              A♣ A♦ A♥ A♠ Q♣ Q♦ Q♥ Q♠
              3♣ 3♦ 3♥ 3♠ t♣ t♦ t♥ t♠
              5♣ 5♦ 5♥ 5♠ 8♣ 8♦ 8♥ 8♠
              7♣ 7♦ 7♥ 7♠ 6♣ 6♦ 6♥ 6♠
              9♣ 9♦ 9♥ 9♠ 4♣ 4♦ 4♥ 4♠
              J♣ J♦ J♥ J♠ 2♣ 2♦ 2♥ 2♠
              K♣ K♦ K♥ K♠

```

{{out|output|text=  when using the input of:   <tt> -2 </tt>}}

```txt

───────────tableau for FreeCell game -2───────────

              A♠ A♥ A♦ A♣ 7♠ 7♥ 7♦ 7♣
              K♠ K♥ K♦ K♣ 6♠ 6♥ 6♦ 6♣
              Q♠ Q♥ Q♦ Q♣ 5♠ 5♥ 5♦ 5♣
              J♠ J♥ J♦ J♣ 4♠ 4♥ 4♦ 4♣
              t♠ t♥ t♦ t♣ 3♠ 3♥ 3♦ 3♣
              9♠ 9♥ 9♦ 9♣ 2♠ 2♥ 2♦ 2♣
              8♠ 8♥ 8♦ 8♣

```

{{out|output|text=  when using the input of:   <tt> -3 </tt>}}

```txt

───────────tableau for FreeCell game -3───────────

              K♠ K♥ K♦ K♣ 6♠ 6♥ 6♦ 6♣
              Q♠ Q♥ Q♦ Q♣ 5♠ 5♥ 5♦ 5♣
              J♠ J♥ J♦ J♣ 4♠ 4♥ 4♦ 4♣
              t♠ t♥ t♦ t♣ 3♠ 3♥ 3♦ 3♣
              9♠ 9♥ 9♦ 9♣ 2♠ 2♥ 2♦ 2♣
              8♠ 8♥ 8♦ 8♣ A♠ A♥ A♦ A♣
              7♠ 7♥ 7♦ 7♣

```

{{out|output|text=  when using the input of:   <tt> -4 </tt>}}

```txt

───────────tableau for FreeCell game -4───────────

              K♣ Q♣ t♦ 8♥ 6♣ 5♦ 3♣ 2♦
              K♥ J♣ t♣ 8♦ 6♥ 5♣ 3♥ 2♣
              K♦ J♦ 9♣ 8♣ 6♦ 4♣ 3♦ A♣
              K♣ J♦ 9♥ 7♣ 6♣ 4♥ 3♣ A♥
              Q♣ J♣ 9♦ 7♥ 5♣ 4♦ 2♣ A♦
              Q♥ t♣ 9♣ 7♦ 5♦ 4♣ 2♥ A♣
              Q♦ t♥ 8♣ 7♣

```

