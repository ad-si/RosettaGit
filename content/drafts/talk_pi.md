+++
title = "Talk:Pi"
description = ""
date = 2018-09-25T18:13:16Z
aliases = []
[extra]
id = 9394
[taxonomies]
categories = []
tags = []
+++

==stopping the program==
Why "until a key is pressed"?  That's an annoyingly non-portable construct. --[[User:Rdm|Rdm]] 22:20, 25 March 2011 (UTC)

I wrote the break key. Someone else wrote "until a key is pressed". The idea is that the process runs for infinity unless the process is terminated. I have reverted the wording to say "the break key".

[[User:Markhobley|Markhobley]] 22:40, 25 March 2011 (UTC)
:Using a keyboard is non-portable? The break key is less portable. I changed it to any key to try to make it more general. Really it should just go forever if you want it to be portable. In general, portability is not necessarily a concern here I don't think. --[[User:Mwn3d|Mwn3d]] 02:15, 26 March 2011 (UTC)

I agree. There are computers without a keyboard, and special industrial keyboards with the break key removed. If there is no facility to terminate the process, then it should run for infinity for the purposes of this task. I have updated the wording to clarify this point.

: There are keyboards without a break key. Not all keyboards are hooked up to a PC or an ASCII computer. -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:44, 27 March 2012 (UTC)

[[User:Markhobley|Markhobley]] 10:57, 26 March 2011 (UTC)

:: I take it that you are looking for ''until the process is terminated'', in whatever way is normal for the environment, not necessarily with a key labeled “Break”. I have updated the task description to clarify this. —[[User:Kevin Reid|Kevin Reid]] 13:35, 27 March 2011 (UTC)

Why request "continue for infinity"? There are no computer that has infinity memory so that request will simply not be possible. A practical limit "continue as long as possible" could make more sense. --[[User:Jofur|&lt;Jofur&gt;]] 11:02, 26 March 2011 (UTC)

Indeed. Infinity here means until the limits of the computer are reached. (So on a system that uses 32 bit registers, calculations would probably cease after 4294967295 digits, unless big numbering was used.)

[[User:Markhobley|Markhobley]] 15:54, 26 March 2011 (UTC)
: There are basiclly two ways to calculate Pi that I know of; #1, (traditional) to calculate the sequence in a row & #2 (BBP-Formual) that calculates specific digits in base-16... Both needs huge amount of memory and if you aim for 'infinity' the #1 system will likely crash due to the data arrays and #2 due to the huge polynoms used for the smallest fractions - neither will give anything close to 4294967295 digits on a 32-bit system. 
:If to behave nice, maybe the program should end before it crash? --[[User:Jofur|&lt;Jofur&gt;]] 16:28, 26 March 2011 (UTC)

==How?==
The task should provide at least one method for doing such a calculation I think? --[[User:Paddy3118|Paddy3118]] 17:10, 26 March 2011 (UTC)

I left this open for the coder to decide at this stage, but I would probably use bbp to obtain each consecutive digit. (We could always split the task for different methodologies in future).

[[User:Markhobley|Markhobley]] 21:21, 26 March 2011 (UTC)
:It can still be left up to the programmer, but maybe offer some suggestions with pseudocode? People don't have to generate infinite digits of pi very often. They probably don't have algorithms lying around in their brains. --[[User:Mwn3d|Mwn3d]] 21:25, 26 March 2011 (UTC)
:: There's an algorithm that allows you to calculate a specific digit of pi. I'd google it, but I'm about to head out the door. It might be possible to memoize some of the intermediate values. --[[User:Short Circuit|Michael Mol]] 13:36, 27 March 2011 (UTC)

:bbp?  --[[User:Paddy3118|Paddy3118]] 22:46, 26 March 2011 (UTC)

==Deleted GUISS solution==

GUISS uses the desktop calculator for this task, and produces the maximum number of digits that the desktop calculator produces. ("Forever" in computing terms means until the maximum value of the registers is reached, or resources are exhausted, rather than "forever" as in the life of the Universe). The GUISS solution provided the closest approximate fit based on its capabilities. Maybe I should have stuck a note that the solution provided produces as many digits as possible for the implementation. (It is not possible to get more digits). The solution was deleted, but I will restore it and pin a note. [[User:Markhobley|Markhobley]] 22:34, 23 July 2011 (UTC)
:So "my language can only quote windows calcualator, and windows calculator can only provide 16 digits" is your defense?  Assuming that's what you meant by "resources", then your language is incapable of meeting reasonable human expectations of this task, and the decent thing to do is just say so.  Why would you rather find some loophole and try to squeeze a "solution" in here, what's the point of that? --[[User:Ledrug|Ledrug]] 00:13, 24 July 2011 (UTC)
::So that we can compare the approaches in different langauges. This is a chrestomathy. The only options are to provide a best fit solution or omit, unless you have some other ideas. What do you suggest? [[User:Markhobley|Markhobley]] 01:13, 24 July 2011 (UTC)
::I think as long we provide suitable comments explaining that the solution does not meet the task exactly, and give the reasoning, then that should be fine. [[User:Markhobley|Markhobley]] 01:16, 24 July 2011 (UTC)
::Give some example wording here if you like, and I will paste it into the description. The reason is as you say: The calculator only provides 16 digits. [[User:Markhobley|Markhobley]] 01:18, 24 July 2011 (UTC)
::Every solution on the page will come to a limit, because of the nature of the task. With GUISS, the limit is 16 digits, other solutions may stop after a few thousand digits, but they will stop.[[User:Markhobley|Markhobley]] 01:27, 24 July 2011 (UTC)
:::I'm not sure using the windows calculator really fits the task because (as far as I know) it doesn't actually calculate anything--it spits out a stored value. Otherwise I could say "System.out.println(Math.PI);" for Java and call it a day. That feels like cheating. --[[User:Mwn3d|Mwn3d]] 04:50, 24 July 2011 (UTC)
::::I don't know Java. Could you set the precision to a high number of digits, and then System.out.println(Math.PI)? I think a two line solution would be cool. [[User:Markhobley|Markhobley]] 11:23, 24 July 2011 (UTC)
:::::Presumably not, because it would be limited to the precision of the conventional floating point, like most other languages, or does it support arbitrary precision? [[User:Markhobley|Markhobley]] 11:27, 24 July 2011 (UTC)
:::::: While Java's got arbitrary precision fractions (<tt>java.math.BigDecimal</tt>), it's π value is just IEEE double precision. That's good enough for most work. –[[User:Dkf|Donal Fellows]] 21:18, 24 July 2011 (UTC)
:::::Ignoring all of that...the point of that comment was that displaying a stored value isn't in line with what this task is really asking for. I could also store a couple million digits of pi in a file which was created by some other program and then just display the contents of that file. The point is to generate the digits through calculation which is part of the example you post. --[[User:Mwn3d|Mwn3d]] 21:32, 24 July 2011 (UTC)
:::In no way does the GUISS solution fit the spirit of the task and should be removed. In what way does it give ''successive'' digits of Pi, "until aborted by the user"? The task description is clearly asking that you use a routine that generates the digits of Pi in sequence - GUISS does not ensure that and so should be removed. The GUISS example does not aid language comparison - it stands out as an anomaly. --[[User:Paddy3118|Paddy3118]] 05:37, 24 July 2011 (UTC)
::::Hmmm, It does generate the digits o Pi in sequence. There is an abort button on the calculator, but I don't know whether it works midflow. By replacing the desktop calculator with one that uses a bigger display to get more digits and supports abortion midflow. The GUISS solution would fit the task. The limitation is with the tool provided, rather than with GUISS. [[User:Markhobley|Markhobley]] 09:11, 24 July 2011 (UTC)
::::Maybe a "limited solution" marker would be a good idea. (Similar to the "incorrect solution", that reads something like 'The solution provided does not meet the exact specifications of the task, due to limitations of the language or its subcomponents. The author has tried to provide an "approximate fit" solution based on those limitations'.). I know there are other tasks on the wiki, where the solutions are an approximation, rather than an exact fit. This might be quite useful. [[User:Markhobley|Markhobley]] 09:45, 24 July 2011 (UTC)
:::This is not strictly true. The underlying concept — that each solution has finite limits — is true.  However, it's not the case that we can say that all solutions would reach their limit if they were allowed to run indefinitely.  A solution could have an time increase between each digit, ensuring that the solution does not terminate before reaching a finite limit (let's say a billion digits before the heat death of the universe) because each successive digit takes so much longer than the previous digit.  --[[User:Rdm|Rdm]] 10:52, 24 July 2011 (UTC)
:::: Don't give him ideas.  Now someone will come up with a "solution" that prints 3.14 and claim it's correct because the program sleeps a billion years between digits. --[[User:Ledrug|Ledrug]] 19:02, 24 July 2011 (UTC)
::::: That'd not be answering; that would be being a smartass. No need for that sort of stupidity. –[[User:Dkf|Donal Fellows]] 21:14, 24 July 2011 (UTC)


### Time to delete

Hi Mark, I think it is time you just deleted the GUISS solution. Stating it generates the digits of Pi in sequence does not make it so, and the consensus is against you. --[[User:Paddy3118|Paddy3118]] 07:15, 25 July 2011 (UTC)
: It's been days, so I removed it.  On a semi related note, there should be a way to discourage prople from translating the D (and Tcl/Icon/C#/Basic/etc) solution which only calculates a fixed number of digits when there are the Ada/ALGOL/Go solutions that can be translated.  I'm very tempted to slap incorrect tags on them now. --[[User:Ledrug|Ledrug]] 22:11, 1 August 2011 (UTC)
:: I'm not sure he'll be back. I think he's been scared off. --[[User:Short Circuit|Michael Mol]] 03:10, 2 August 2011 (UTC)
::: Nm; I see he's been doing some minor edits today. --[[User:Short Circuit|Michael Mol]] 03:15, 2 August 2011 (UTC)

== Task split ==

I think what's currently at [[Pi]] should be moved to [[Pi/Spigot]], to reflect that it's asking for a spigot algorithm for Pi. It may also be a good idea to have [[Pi/Constant]], for language-provided constants representing pi, or some specific approximation thereof. Perhaps [[Pi]] itself should be left more general, or as a set of links to other ways to retrieve Pi. Or perhaps be a task which ''uses'' pi, which necessitates an example author's obtaining the value somehow. ("Calculate the circumference of the circle whose radius is 1/2π" would probably be sufficient and interesting; the correct answer is 1, but most code would reach that result only through precision loss.) Part of the trouble I'm observing in the hoopla around this task right now appears to be that the GUISS solution doesn't solve the task as stated, but ''does'' answer the question "how do I get π into my program," which is a question I'm likely to have if I were to visit a Rosetta Code task page named Pi. --[[User:Short Circuit|Michael Mol]] 11:56, 26 July 2011 (UTC)
:If we create Pi/Constant it should just redirect to [[Real constants and functions]]. I'd be ok with renaming this page to a specific algorithm name. --[[User:Mwn3d|Mwn3d]] 12:20, 26 July 2011 (UTC)
::"Calculate the circumference of the circle whose radius is 1/2π" seems bad, since specifying how the calculation is done is territory best left alone, and you cannot realistically specify the starting point.  "Calculate the circumference of a circle whose radius is 1" would not be too bad, though "Calculate to n digits the circumference of a circle with radius n, show some examples and describe the maximum n supported by your implementation" might be more interesting. --[[User:Rdm|Rdm]] 12:37, 26 July 2011 (UTC)
:::I liked the "given a circle with a radius of 1/2π, calculate the diameter of the circle" because it'd didn't require the output of a transcendental number, but did require the transcendental number to be used in calculation. I'm (honestly) puzzled how specifying the unknowns to be found is problematic; another way to say it is "given <math>c = 2πr</math>, and <math>r = 1/(2π)</math>, solve for c." We have messier solver tasks elsewhere on the site. --[[User:Short Circuit|Michael Mol]] 15:03, 26 July 2011 (UTC)
::Also, for that matter, [[Pi]] can be implemented without use of a spigot algorithm, if you are not concerned about efficiency.  --[[User:Rdm|Rdm]] 12:37, 26 July 2011 (UTC)
:::Hm. It might be worth scrapping the task and creating more defined ones, such as [[Pi/Bailey-Borwein-Plouffe]], and leave the "Pi/" namespace available to other specific implementations.--[[User:Short Circuit|Michael Mol]] 15:03, 26 July 2011 (UTC)
:::[[Pi]] can be implemented without use of a spigot algorithm regardless of efficiency, because that algorithm is not good at being efficient at all.  The C program I wrote continuous generates Pi digits faster than the unbound spigot, despite the algorithm used was totally unsuitable for the task. --[[User:Ledrug|Ledrug]] 00:07, 27 July 2011 (UTC)
:I would favour leaving the task as it is (but with the impossible requirement to continue 'forever' replaced with something like 'until resources are exhausted').  Specifying only that the digits are generated 'in succession' gives a choice between using an unbounded Spigot algorithm (which requires support for arbitrary-precision arithmetic) and a 'bounded' algorithm configured to use all the available memory (which better suits languages with fixed-size integers).  Whichever approach is used, the number of digits that can be generated is limited by memory size. [[User:RichardRussell|RichardRussell]] 14:37, 12 November 2012 (UTC)

== C# - incorrect solution ==
The C# version produces output where only first 30% is correct, then goes garbage. For example, here's what it outputs when asked for 50 digits:
   3141592653589792800649014751202138549699509907712322    while correct digits are
   3141592653589793238462643383279502884197169399375105.
                  ↑ 
The D version which uses similar but slightly different algorithm is correct.
--[[User:thedeemon|thedeemon]] 24 October 2012


: I split the two above values of   <big><big><math>\pi</math></big></big>   (sans a decimal point)   so that it can be easily compared, and I also added an up arrow   (↑)   indicating to the unequal (different) decimal digit.    I realize that (normally) editing other people's edits are frowned on and considered a no-no,   but the intent is to easily compare two different values of   <big><big><math>\pi</math></big></big>.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:45, 24 September 2018 (UTC)

== Pi vs tau ==
If we create more defined tasks, we might want to consider using [http://tauday.com tau] instead of Pi. --[[User:Rdm|Rdm]] 12:57, 1 August 2011 (UTC)
:Ugh. Can we not have that debate here? The ''only'' significant difference between pi and tau is the impact they may have on the readability and intuitive extrapolation of formulas. I see enough difficulty agreeing on task spec as is, and pi vs tau is not clearly resolvable. For the time being, I would far prefer to stick with pi, as that's the most common and mass way of representing the number, and so that's what people are more likely to recognize and understand. When [http://googlefight.com/index.php?&word1=pi&word2=tau tau beats pi by a 2:1 margin], I might be more interested. --[[User:Short Circuit|Michael Mol]] 14:21, 1 August 2011 (UTC)
::That's not exactly an argument on merits.  And there's no reason for the use of tau to exclude the use of pi any more than the use of the number 1 excludes the use of the number 0.5.  That said, if googlefight meant anything to me, I might use a smaller set of numbers?  --[[User:Rdm|Rdm]] 19:58, 1 August 2011 (UTC)
:::No, it's not an argument on the merits. The best thing I've read on the merits of the subject is [http://esr.ibiblio.org/?p=3481 here], and all that tells me is that we don't know which is really better than the other. The trouble with pi vs tau on RC is that your average ''non''-mathematician isn't yet likely to be familiar with tau, and so using tau in tasks is very likely to confuse what should be a simple subject; to resolve the confusion, use of tau would need annotations like "tau is 2*pi", and that would strike me as too trivial to warrant further complicating the task description. In short, even if we posit tau to be a more elegant symbol than pi, right ''now'', it's not a more elegant way to write task descriptions. --[[User:Short Circuit|Michael Mol]] 21:07, 1 August 2011 (UTC)
::: But there's no exact arguments on merits.  Tau vs Pi is really a matter of radius vs diameter of a circle, and you can't argue which is of more merit than the other.  A well defined constant should convey most symmetry or invariance of a system, where radius is arguably better because one end of r is always at the origin--but in real world diameters are almost always easier to measure: try directly tell the radius of a ball bearing with a caliper.  In any event, for calculating digits of pi, the tau debate is not even relevant, where the most useful constant is probably Pi/4 any way.  --[[User:Ledrug|Ledrug]] 21:55, 1 August 2011 (UTC)
::: That http://esr.ibiblio.org/?p=3481 argument (and the pi manifesto it referenses) suggests that this whole thing is just about conciseness, and not about some of the other issues (like constant 1/2 in simple r^2 equations and 1/6 in simple r^3 equations).  I can understand that conciseness has advantages, and I also agree that pi's familiarity/popularity can be a major advantage in a cookbook equation context.  Anyways, the "pi is wrong" slogan, while catchy and motivating is itself wrong.  "Pi is useful, but pi can also occasionally be misleading or confusing" would be a more accurate (though boring) phrasing.  There's room in the world for both constants, and I dislike reasoning (even from its advocates) that suggest that there can be only one.  And "tau is 2*pi" is simple, but too trivial?  We have "Goodbye, world" here.   And, 99 bottles of beer -- how can "tau is 2*pi" be too trivial?  (Though, ok, I can understand not wanting to like to a "pi is wrong" page.)  --[[User:Rdm|Rdm]] 11:48, 2 August 2011 (UTC)
:::: First, "constant 1/2 in simple r^2 equations" and "1/6 in simple r^3 equations" are ''exactly'' questions of conciseness; which is more concise? <math>\frac{1}{2}\pi r^2</math> or <math>\tau\pi</math>? For these equations, <math>\tau</math> is easily more concise. (And this is the area of debate I really wanted to avoid)--[[User:Short Circuit|Michael Mol]] 13:37, 2 August 2011 (UTC)
::::: Ok, yes, that can be a question of conciseness.  But it's also a simplicity issue and, thus, a mnemonic issue.  It's the same reasoning behind the coefficients in a taylor series.  For the n-space analog of area of an n-space analog of a sphere, the equation would be <math>\scriptstyle 2\pi x^n \div  n! \equiv \tau x^n \div  n! </math>.  By optimizing for second degree equations you obscure the simplicity of the issue for every other degree.  And optimizing for second degree equations can be the right thing to do, in some contexts, but not for all contexts.  Meanwhile, I am uncomfortable talking with someone on a subject that they say they do not want to talk about, but I am also uncomfortable leaving alone the issues that you bring up.  --[[User:Rdm|Rdm]] 14:38, 2 August 2011 (UTC)
::::: Curiously, I think we're in total agreement in {{pi}} vs {{tau}}. Level of knowledge of the reader being equal, it really does depend on the use case which representation of the value is more appropriate. As in all non-trivial things, the answer to "which is better" is "it depends..." --[[User:Short Circuit|Michael Mol]] 16:05, 2 August 2011 (UTC)
:::: Second, I was far more concerned about scenarios involving geometric tasks which chose to use {{tau}} rather than {{pi}}, as each of those tasks would need to note how to derive {{tau}} from {{pi}}, which would complicate them. (A trivial complication yes, but still a reduction in their simplicity)--[[User:Short Circuit|Michael Mol]] 13:37, 2 August 2011 (UTC)
:::: Third, I really wouldn't mind a [[Pi/Pi and Tau]] which showed how convert from pi to tau and back. That kind of triviality isn't something that bothers me, though it may tend to bother contributors who are completeness-driven. --[[User:Short Circuit|Michael Mol]] 13:37, 2 August 2011 (UTC)
: I prefer <math>\pi</math>, because of [[wp:Euler's identity|Euler's awesome identity]]:
:: <math>e^{i\pi} + 1 = 0</math>
: That is an amazingly significant formula, “simply” linking 5 of the key constants into one piece. There is no way that <math>\tau</math> would work nearly so well in it; it's not coupled to the transcendental functions in such a direct fashion. (Also, you use <math>\pi</math> directly when working with circular areas, and neither <math>\pi</math> nor <math>\tau</math> is great for spherical volumes or their equivalents in higher dimensions.) –[[User:Dkf|Donal Fellows]] 21:28, 11 February 2012 (UTC)
:: Well, of course -- if you replace the terms in the equation with something different, you need to change the equation if you want the result to be valid.  That's just basic math.  In this case, a valid pair of changes are: "replace pi with tau (or vice versa)" and "swap the positions of the + and the =".  In other words: '''<math>e^{i\tau} = 1 + 0</math>'''.   You might also want to swap <math>i</math> and <math>\tau</math> depending on the font you are using (<math>e^{\tau i} = 1 + 0</math>), but multiplication is commutative with complex numbers, so that will not change the equation's validity.
:: As for the general case of n dimensions... I was going to look that up, but currently wikipedia says that the proportionality constant for volume of a [[wp:N-sphere|hypersphere]] is <math>V_n (R) = C_n R^n</math> where <math>C_n = \frac{\pi^\frac{n}{2}}{\Gamma(\frac{n}{2} + 1)} \,</math> but that gives the same result for <math>C_n</math> for both n=2 and n=3 so it can't be right.   So, for now, I will take your word that it gets messy.  It's been ages since I thought about this issue.  Still, as motivation for a task?  I do not think that we exclude tasks based on this kind of mathematical popularity issue... --[[User:Rdm|Rdm]] 16:34, 12 February 2012 (UTC)

== Julia Output ==

I'm not sure there's necessarily anything wrong with the code, so I didn't want to mark it incorrect, but the output for the Julia sample is clearly wrong - there are several places in the sequence where the character 'e' appears. Is there a more appropriate template that can be used to mark it as needing attention. I'm not a Julia programmer myself, so it's not something I can easily confirm as correct or incorrect. --[[User:J4 james|j4_james]] ([[User talk:J4 james|talk]]) 16:47, 3 October 2015 (UTC)


== Formula for JAVA ==
By what formula did you calculate pi in JAVA? I searched hard in wikipedia, and found nothing this good. What formula do you use?

Also - the first 50 digits are exact - but is the whole formula exact? Or is it diverges after some digits? [[User:DeatH StaR|DeatH StaR]] ([[User talk:DeatH StaR|talk]]) 20:55, 13 October 2016 (UTC)

== pascal ==
when i compile i get: Error: Identifier not found "result"

###  different output 

i solved the above problem by adding oneloop:=result before the end of the function but:
when i run the program to calculate thousand digits of pi i get an output with the last digits 62429841642 which is completely different from the output published at the wiki page.
