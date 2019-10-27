+++
title = "Talk:Monty Hall problem"
description = ""
date = 2019-02-18T06:51:00Z
aliases = []
[extra]
id = 2985
[taxonomies]
categories = []
tags = []
+++

==Haskel error on codepad?==
I don't know Haskel but following the section header link, I tried to run it on [[http://codepad.org/BRigLD93 codepad]], unfortunately it gave errors but I don't think I'm qualified to judge if they are true errors or something to do with the version of Haskel used on Codepad etc. Could someone comment and maybe fix things if there is an error? Thanks. 

P.S. The error was:

```txt

  Error occurred
  ERROR line 26 - Type error in application
  *** Expression     : cars trials True g
  *** Term           : trials
  *** Type           : Integer
  *** Does not match : Int

```

--[[User:Paddy3118|Paddy3118]] 22:10, 10 August 2008 (UTC)
:My mistake. I wrote that program; the problem was that I used the Haskell implementation GHC, which allowed the literal "10000" to be an Int, and Codepad uses Hugs, which apparently interprets all literal integers as Integers by default. I believe the dreaded [http://www.haskell.org/haskellwiki/Monomorphism_restriction monomorphism restriction] is to blame. Anyway, I added an explicit type signature, so [http://codepad.org/VNKGdwhi the program now works with Hugs]. [[User:Underscore|Underscore]] 15:24, 11 August 2008 (UTC)
::Thanks. I ran it on codepad and inserted some sample output. --[[User:Paddy3118|Paddy3118]] 15:59, 11 August 2008 (UTC)

== ADA program problem ==
The probabilities should tend to 1/3 for sticking with your original guess whereas your result seems to be 1/2 making me think you could have a problem in the Ada program. --[[User:Paddy3118|Paddy3118]] 15:09, 11 August 2008 (UTC)
:The number of iterations was simply not enough to get a stable random number generator on a fast machine. I have increased the number of iterations and achieved the expected results.--[[User:Waldorf|Waldorf]] 00:18, 15 August 2008 (UTC)
::That sounds strange. The operation of a pseudo random number generator should not be affected by the speed of the computer. Or is something else used to generate random numbers? --[[User:PauliKL|PauliKL]] 17:22, 11 November 2008 (UTC)

== Probability error ==
Several sample outputs are suggesting that the total probability does not equal 100% which can't be right. --[[User:Lupus|Lupus]] 11:38, 5 November 2008 (UTC)
:It's just MAXScript and AWK. It may just be floating-point math errors. They aren't drastically off (less than 1% each) and the relation between switching and keeping is still apparent. --[[User:Mwn3d|Mwn3d]] 14:11, 5 November 2008 (UTC)

:Hi Lupus, could you give an example? There is nothing to add up to 100% that I can see - you just do say 10000 runs for each strategy and report how many times you win for each strategy and/or divide by the number of runs and multiply by 100 to get the percentage for each strategy. The strategy figures are compared but don't add up to 100%. Because its all (pseudo) random, the results should tend towards 1/3, 1/2 or 2/3 but won't exactly equal these figures. --[[User:Paddy3118|Paddy3118]] 15:12, 5 November 2008 (UTC)

:Oh wait, I think it is due to subtleties of the implementation Lupus. Your Fortran solution looks at what the results would be for the same random choices ran on the different strategies. Other tests just run run the same number of random trials which is what I had in mind. I haven't looked in depth at your Fortran solution though. --[[User:Paddy3118|Paddy3118]] 15:19, 5 November 2008 (UTC)

:Here are some examples of what I meant(I should have said percentages not probability). The Ada example appears to be doing 100000 iterations but adding up the counts give 100003. The AWK example is doing 10000 iterations but adding the counts gives 10066. The Maxscript percentages add up to 100.61%. The Perl percentages add up to 98.57%. The Python example counts add up to 99866 for 100000 iterations and the Scheme percentages add up to only 85%! The percentage chance of winning by staying plus the percentage chance of winning by switching must equal 100%. --[[User:Lupus|Lupus]] 16:58, 5 November 2008 (UTC)

::Hi Lupus, lets say you run 100000 trials and for strategy1 and get 33337 wins. You might run a second set of 100000 trials for strategy1 and get 33314 - It is random it should tend to 1/3 but It is quite allright for it not to '''be''' 1/3 +/-1. Similarly for running 100000 tests of strategy2 - It might not be exactly two thirds +/- 1. Adding up the number of wins for the two strategies I would be most '''surprised''' if it always totalled 100000 and would check things again, as these are independent random runs. --[[User:Paddy3118|Paddy3118]] 17:39, 5 November 2008 (UTC)
:::The problem is that if you make 100000 runs, you should always switch, but remember where you started. That way, you can see if you would have won by staying and if you did win by switching all in one run. Doing it like this can guarantee that you will ''always'' record a total of 100000 outcomes. If you do 100000 runs switching and then 100000 runs staying, then you won't necessarily end up with 100000 total wins. I think Lupus is expecting that the examples only run through once checking to see if you would have won had you stayed and if you did win by switching. --[[User:Mwn3d|Mwn3d]] 17:54, 5 November 2008 (UTC)
:::: Now I understand! As Mwn3d surmised I was expecting the programs to keep a running total of both strategies at once. Thanks for the explanation. --[[User:Lupus|Lupus]] 18:50, 5 November 2008 (UTC)

== Is the Perl implementation correct? ==

Perl implementation looks suspiciously short. I don't know much of Perl, but it seems to me that the function <tt>play</tt> just calculates a random number in range 0 to 2 and then checks if the result was 0. So it does not actually play the game. --[[User:PauliKL|PauliKL]] 14:25, 12 November 2008 (UTC)
:It seems like it actually just puts the winner in the same place every time(?) The call <tt>play 1</tt> means "play the switch strategy", while <tt>play 0</tt> means "play the stay strategy". Then it picks a door (<tt>my $door1 = !int(rand 3);</tt>) then asks "are we switching?" (<tt>$_[0] ?</tt>). If we are, return not the door we picked (<tt>!$door1</tt>), otherwise return the door we did pick (<tt>$door1</tt>). I think it's right, but I'm not sure where it places the winning door. --[[User:Mwn3d|Mwn3d]] 14:42, 12 November 2008 (UTC)
::AFAIK the symbol '!' means "Not". Thus, variable door1 is set to boolean value True if the random number was not 0 (2/3 probability). There is no code for player selecting a door, host opening one of remaining door and player switching to the remaining door. Wasn't the original idea to make a program to simulate the game in order to see what is the probability, instead of using the known probability as starting point? --[[User:PauliKL|PauliKL]] 11:51, 13 November 2008 (UTC)
:It seems like it "fixes" the winning door to be the number 0. The user chooses a door in rand 3; if it choose the 0, !int(rand 3) gives true, so that the stay strategy wins, the leave strategy looses, and this is encoded in the next line. I suppose there's nothing wrong in fixing the prize behind the door labelled as 0; we could think of it as if we "move" the doors all together with prize or goats, so that the "player" index changes (it changes the order of the doors), but the judge of the game (Monty) will address the winner door always with 0, since that is the label he (and only he of course) can see. It is a sort of "mapping". To me it's ok.
::I disagree. I think the Perl implementation is totally wrong. The purpose of the task was to simulate the Monty Hall game in order to see what are the changes of win with the two strategies. Fixing the winning door would not be correct simulation even for a simpler game where user would just pick one of the doors and win if the car was there. But Monty Hall game is not that simple. There is the step where the host opens a door and another step where the player either switches the door or keeps the original selection. These steps have been omitted totally in the Perl implementation.
:::I strongly disagree with the removing of the Perl (anyway I will write soon a new implementation that satisfies your needs) program. As I explained (not too much clearly?) it is simply a "mapping" problem. Human needs to call thing differently; programs does not. The Perl source did simulate the Monty Hall problem, even though in a cryptic way; image all the other sources like they are, with an added code that sort doors index so that 0 is the winning one, and the others are not. To you, do they keep simulating or not?! --[[User:ShinTakezou|ShinTakezou]] 22:11, 7 December 2008 (UTC)
::Perl implementation does not simulate the game in order to find out the probability of win. Instead, it takes the answer (1/3 and 2/3 wins for keep/switch strategies), and then implements a random number generator to give 1/3 or 2/3 probability.
::This is actually the problem in many tasks. The tasks need to be made simple in order to keep the implementations short and clear. But then many contributors, instead of implementing solution to the problem, only implement solution to that specific case, or even jump directly to the answer without doing any computing.
::--[[User:PauliKL|PauliKL]] 14:56, 7 December 2008 (UTC)
:::It was not the case of Perl code, that indeed simulated like the others code, but with logical optimization. Would the following (commented) code meets the needed criteria?


```perl
#! /usr/bin/perl
use strict;
my $trials = 10000;

my $stay = 0;
my $switch = 0;

my $show;

for(my $i=0; $i < $trials; $i++)
{
   my $prize = int(rand 3);
    # let monty randomly choose a door where he puts the prize
   my $chosen = int(rand 3);
    # let us randomly choose a door...
   if ( $prize == $chosen )
   {
      # a "further" optimization would strip the next
      # line since it basically does nothing
      while( ($show = int(rand 3) ) == $chosen ) { }
      # monty opens a door which is not the one with the
      # prize, that he knows it is the one the player chosen
      $stay++;
      # no matter what $show is... player wins only if he
      # stays... and looses otherwise; this is because we
      # wrote this code inside a $prize == $chosen if
      # we could add something that logical optimization
      # would strip
   } else {
     # monty has only one choice for $show,
     # now, we don't need to waste cpu time: we (as monty!)
     # already know
     # that being the prize behind $prize and the user
     # choice is $chosen, $show won't be one nor the other;
     # so if the user stay, he looses, if switch, surely
     # he wins.
     $switch++;
   }
}

print "Stay win ratio " .  (100.0 * $stay/10000.0) . "\n";
print "Switch win ratio " . (100.0 * $switch/10000.0) . "\n";
exit 0;
```


:::--[[User:ShinTakezou|ShinTakezou]] 23:01, 7 December 2008 (UTC)

::Since no one replied negatively to this code, I added to the article. This is basically the same as the previous implementation, but without going further in any optimization and keeping the ''logic'' of the game; and when this is not clear, comments help. At least, this is my opinion. --[[User:ShinTakezou|ShinTakezou]] 01:17, 13 December 2008 (UTC)

== Number of iterations ==

You need to increase the number of iterations, several programs list the probability of switching as 60-something percent, which is incorrect, it should be 1/2. Either that or your RNG is broken.

:Umm... that is the infamous fallacy of the Monty Hall problem. See [[wp:Monty Hall problem|Wikipedia: Monty Hall problem]]. The correct probability of winning by switching is 2/3, which is approximately 66.7%. This is the whole point of the problem and why it's so famous. --[[User:Spoon!|Spoon!]] 06:33, 6 August 2009 (UTC)
::Sorry for inserting misinformation. Could you please tell me why my program guesses correctly 50% of the time instead of 2/3 of the time? It's the Common Lisp one.
:::Your "show-goat" only depends on "round" (the state of which door has the car and which ones have the goat). In the actual Monty Hall problem, which goat the host shows also depends on the choice that the player makes (i.e. "initial" in your code) -- in particular, you need to make sure that the goat that the host shows is ''not'' the door that the player chose (i.e. "goat" should never be equal to "initial"). --[[User:Spoon!|Spoon!]] 07:36, 6 August 2009 (UTC)
:::Also, when the player chooses the winning door, preferably the goat door shown should be chosen randomly from the two available. If you always choose the one the has lower index then the player can use the ordering of the doors to get more information. --[[User:Spoon!|Spoon!]] 07:53, 6 August 2009 (UTC)
::::Thank you, now my program works correctly. [[User:Foobie-bletch|Foobie-bletch]] 08:10, 6 August 2009 (UTC)

:If you look at the Tcl solution, you'll see that the chance depends massively on the strategy being used. (I'm rather proud of the fact that I separated the player strategies so it was clearer what was going on.) Indeed, this whole game is great for sorting out the men from the boys when it comes to probabilistic analysis. If it makes you feel easier, think of it this way: would you rather choose one or two doors to start out with? Does the prize move around? —[[User:Dkf|Donal Fellows]] 08:07, 6 August 2009 (UTC)
::I, on the other hand, think that probability questions can easily have non-intuitive answers. I just have to take more care, and simulate where possible :-)   --[[User:Paddy3118|Paddy3118]] 15:56, 6 August 2009 (UTC)


== Scheme Solution Problem? ==
I would have thought that with 1M trials, that the Scheme implementation would give a result closer to 1/3 and 2/3, instead of:

```txt
;; > (compare-strategies 1000000)
;; (stay-strategy won with probability 33.3638 %
;;  and switch-strategy won with probability 51.8763 %)
```

Is their a problem? --[[User:Paddy3118|Paddy3118]] 01:44, 26 September 2009 (UTC)

: Sounds like they've got a bug; that's massively off the expected frequency. Probably due to incorrect problem modeling; it's very easy to get the problem model wrong or jump to a (wrong) solution. That's why the Tcl solution specifically models the whole thing, since only then can you know whether you've got it right or blundered. —[[User:Dkf|Donal Fellows]] 10:34, 26 September 2009 (UTC)

: sqrt(1M)=1000, so a deviation of this order of magnitude isn't exactly unexpected. Indeed, in this case the deviation is just below two standard deviations, i.e. a bit high, but still inside the 95% confidence interval. That is, for a truly random sequence with probabilities 1/3 and 2/3, there's a 5% chance of an even higher deviation. So, no clear indication of a problem. --[[User:Ce|Ce]] 10:39, 26 September 2009 (UTC)
:: Oops, I had looked only at the first number; now I notice that the second number is indeed far off. Given that winning with the switch strategy is the same as not winning with the stay strategy, I couldn't imagine that you could get one right and the other wrong ... --[[User:Ce|Ce]] 10:47, 26 September 2009 (UTC)

::: Looks to me like they're just randomly picking from two doors the second time. (Or their RNG is improbably distributed, which would be a much worse problem.) —[[User:Dkf|Donal Fellows]] 11:42, 26 September 2009 (UTC)

I've flagged the entry and hopefully someone will take a look and correct it. --[[User:Paddy3118|Paddy3118]] 16:19, 26 September 2009 (UTC)

==Someones theory==
I dont beleive everyone else, This is my theory:

alright, now lets say each O is a door-
 
door #'s:     1    2    3
  doors:      O    O    O
probability: 1/3  1/3  1/3

okay each door has a 1/3 chance; but the two on the left have 66% chance.correct,
So now say we pick one for example, door 3; awesome possum, so we say that it has a 33% chance of it being a car.
So now we open door 2 and it's a goat. Now some think that door one still has a 66% chance and door 3 has 33% chance,
but thats not true because now we only have '''2''' numbers; therefore it's 50/50 chance between the two

and I was looking at this and they were using larger numbers to try and explain it, this is what I think:

door #'s:  1  2  3  4  5  6  7  8  9  10
doors:     O  O  O  O  O  O  O  O  O  O 

So each door has a 10% chance
now say we predict door 9; at this point, you only have a 1 in 10 chance of getting it right. now next step:

lets say we open all the doors except for door 7 and door 9. Now you would think it's in door 7, BUT thats not true now we just have two doors and TWO doors only not 10 but Two:

door #'s:  7  9
Doors:     O  O

okay and now they both have a 50/50 chance. PROBLEM?

Well then,This is what I think, my friend tells me I'm crazy but I just don't know.

:The point about the problem is that the experimental result - as mirrored in the correct simulations, is often seen as un-intuitive. If you google you can find several explanations, maybe one will work for you? --[[User:Paddy3118|Paddy3118]] 06:50, 13 April 2011 (UTC)

:It's not clear to my why you think that opening that door changed the probability that you had originally guessed right.  --[[User:Rdm|Rdm]] 11:47, 13 April 2011 (UTC)

:I can't tell you how it works, just that it works. All of these simulations and thousands of math professors can't be wrong. --[[User:Mwn3d|Mwn3d]] 13:24, 13 April 2011 (UTC)

: Conditional probabilities ''are'' difficult, but when you model the problem exactly with a contestant agent and a Monty Hall agent (and you can clearly see who has just how much knowledge) you get the counter-intuitive result. Because when you model it properly you get this, it means that if your theory gives any other result, it is ''your'' theory that must be wrong. IOW, Reality sees your theory, has a good chuckle, and spits it back out.
: Here's how I recommend thinking about it. You had a 1/3 chance of picking the right door initially (simple!) so there's a 2/3 chance you got it wrong to start out with. ''What Monty Hall does doesn't change that at all.'' Switching is like going from picking one door to picking two; yes, one has been subsequently eliminated, but you knew at least one of them didn't have the car anyway. BFD. You're switching from a 1/3 chance to the complement of a 1/3 chance (i.e., 2/3), which is a win. –[[User:Dkf|Donal Fellows]] 13:28, 13 April 2011 (UTC)
:: And if you still don't believe me, get a friend and play the game for real (well, maybe with a swig of beer instead of a car). 10 or 20 rounds with one strategy or the other shouldn't take too long. –[[User:Dkf|Donal Fellows]] 13:31, 13 April 2011 (UTC)
:"Now some think that door one still has a 66% chance and door 3 has 33% chance, but thats not true because now we only have 2 numbers; therefore it's 50/50 chance between the two". This is wrong. The fact that you know something after choosing does not change the fact that on average you got it wrong 66% times in the first place. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 06:48, 18 February 2019 (UTC)
