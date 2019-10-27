+++
title = "Talk:Mian-Chowla sequence"
description = ""
date = 2019-04-04T15:41:00Z
aliases = []
[extra]
id = 22222
[taxonomies]
categories = []
tags = []
+++

== Distinct? ==

Hi, could you further explain what distinct means in the task description? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:50, 15 March 2019 (UTC)

: Scrub that, The OEIS entry made sense to me. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:54, 15 March 2019 (UTC)

:Means a unique sum. E.G. 3 can't be in the sequence since 1 + 3 = 4 and 2 + 2 = 4. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 12:07, 15 March 2019 (UTC)


== Execution Speed? ==

Hi, it could be nice to have as a third task, the program duration. This gives an indicator of the language efficiency (and the algorithm too). I have tried among several Basic language implementations and the elapsed time, for 100 terms, is spread between 2 seconds and 25 minutes !  --[[User:PatGarrett|PatGarrett]] ([[User talk:PatGarrett|talk]]) 11:48, 15 March 2019 (UTC)

:In general, Rosettacode discourages emphasizing program execution speed. It is more about comparison of concepts, and focusing on speed tends to lead to heavily optimized code which may become difficult for a new language user to read. It is sometimes useful, or at least entertaining to have an idea of relative execution speeds, but I hesitate to make it, or even imply that it should be, a requirement.

:When I am developing tasks, I try to choose goals that most languages should be able to do in about a minute or less of processing time. Selfishly in some part, because all Perl 6 RC entries are run daily against a nightly development build of blead Perl 6 for smoke testing, so tasks that take excessively long really extend the testing time. (Running ~1000 or so tasks takes a while even if they ''aren't'' excessively long.) --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 12:07, 15 March 2019 (UTC)

::''| all Perl 6 RC entries are run daily against a nightly development build of blead Perl 6 for smoke testing''
:: Any thoughts on the RC examples when used for this? Do you create some specific running order for the RC tasks in any way to aid in your verification? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 14:08, 15 March 2019 (UTC)
:::I apologize for if I gave the impression that '''I''' am responsible for blead Perl 6 for smoke testing. I help out in small ways but [[User:SqrtNegInf|SqrtNegInf]] is and has been the primary architect and driver of the smoke testing project. I don't believe that tasks are run in any specific order. Some (typically those that require user interaction in some way) are skipped. Tasks that use concurrency are run serially, (to allow them to use the extra processors). Otherwise, they are pretty much run in a big bunch, parallelizing where possible. For more accurate details, it may be better to inquire with [[User:SqrtNegInf|SqrtNegInf]] directly. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 20:38, 16 March 2019 (UTC)  

::::Thanks Thundergnat --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:29, 17 March 2019 (UTC)

:: Hello, thanks for the answer. I understand your concern, and also I am impressed by your Perl 6 involvement. Anyway this task is nice because it is also (but not only) an efficiency test for compilers about number crunching and dynamic array storage allocation. Have a good luck. --[[User:PatGarrett|PatGarrett]] ([[User talk:PatGarrett|talk]]) 14:14, 15 March 2019 (UTC)
:::Thanks for the kind words. Really credit for the task should at least partially go to [[User:Gerard Schildberger‎|Gerard Schildberger‎]]; he keeps adding interesting tasks that lead me to do further reading and come up with tasks based off of that. Gerard and I disagree occasionally; but he's smart guy, just takes himself much too seriously IMO. Ah-well. Cheers --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 20:38, 16 March 2019 (UTC)

: As well as the algorithm, timings are also dependent on the processor speed and OS and the language compiler/interpreter version - I've (reluctantly) included timing information for the Algol 68 and AWK samples which I ran on a Windows 7 System which appears to be about half the speed of the machine the VB.NET sample was run on - though I expect I have a different versions of the VB.NET compiler, .NET framework and operating system. --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 14:15, 17 March 2019 (UTC)

== Basic Algorithm ==

I am a novice at python programming, and I was looking the ''Python Procedural'' version, using tio.run and found it takes about 2 seconds to execute.  I made a few changes to it and got the execution time down to under 100 ms.  [https://tio.run/##jVHLbsMgELzzFStfAq0T5XFqJZ/6GW6EKMYKKi8BVhSp/@5icFS77SEcEOzM7szuulu8WHMax95bDTIKH61VAaR21kfgdjCxBhmU5KIGfmHSoBmLUguEOtGDlsxQfrFXxTB5RZCO5tBAezjnz00K1aVQu50DLgw6JEIQEbfHM8lBI67L8BztrYfoJVMgTbGDj7PGHTYZmqxhzWtoM/28IE0nlU6VDTyXaitM9hlOVbKxdeLC2o4rwTwmf/APL9gn@i@DdR1Oj58UoYJYC5RhfDX3HPSosuY75pwwHc4trcEy89IrSg1SapgWlELTwIZSnaZF6aY4CTGNZlrnbrpmFedlmrWSIeKyfrxacw2nPSEPUl/2NRz2v/jVu3mz2g2RRWlN1ocrC1UNeOFlGyKBpyk71ah0qMg4fgM (Here is a link)] I was somewhat surprised to find that the revised code goes around 20 times faster.<br/><br/>The thing I noticed about what I was able to change was that the test against the existing sums can be simplified, that is, rather than maintaining a list of all the sums and testing each new trial against the whole list, one can test the trials against only the previous iterations list of sums, and build an incremental list (without checking the incremental list).  If the trial succeeds, the incremental list is combined with the main sums list.  If you think about it, it is mathematically impossible for each new iteration's trials to collide with it's own iteration.  Each new iteration's trials can only collide with the sums from previous iterations.<br/><br/>Another thing to point out is that, in the modified code (linked above), I clear the incremental list after combining it with the main sums list.  This seems like it would slow things down, but the time spent clearing it is offset by the odds that it will be quickly cleared when there is a sums collision in the block above.  Also, since the Sets are being union-ed, instead of appended, any attempted duplication is disregarded.  If that line is omitted, (the "newsums.clear()" in the else block), the program execution time remains about the same.  So perhaps it is redundant.<br/><br/> The quickest run at (tio.run) so far is the 2nd Go version which is about 10ms.  However, it can be increased slightly to ~6ms by removing the unnecessary removal of the "isx" items from the "is" set (because they never get put in there in the first place). --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 05:07, 21 March 2019 (UTC)

:I took a look at your Python code but did not run it. I must admit that I stopped development after getting what I thought were correct results and did not look to optimise.
:If your code is correct then it seems best if you replace my proceedural code, and maybe amend the comparative timings above the following, more functional version, (where the functional is now ~3x slower when run on the Tio.run site), as there is no need to keep my original proceedural code. 
: Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:17, 21 March 2019 (UTC)
:: You're welcome, I will do what you suggest. --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 13:38, 21 March 2019 (UTC)<br/>

:Thanks for pointing out that the 2nd Go version contains some redundant code - probably a hangover from an earlier version I wrote. Anyway, I've removed it now and achieved the ~40% performance boost you mentioned :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 09:32, 21 March 2019 (UTC)
:: You're welcome. --[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 13:38, 21 March 2019 (UTC)
Tidying and simplifying the functional Python, it occurred to me that one of the speed optimisations in your procedural version could possibly be losing more on the swings than it's gaining on the roundabouts.  Most of the integers tested will not turn out to be Mian-Chowlas, so perhaps the cost of constructing a new set, saving all sums from the test, and then (in most cases) just clearing that set, is an uncertain investment ? (The current functional draft doesn't bother, and still seems fast). Of course, both versions are now at a few dozen milliseconds, so nothing much turns on it :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 15:40, 4 April 2019 (UTC)
