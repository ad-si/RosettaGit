+++
title = "Talk:Knuth's algorithm S"
description = ""
date = 2018-08-07T11:05:50Z
aliases = []
[extra]
id = 10706
[taxonomies]
categories = []
tags = []
+++

== Names ==

Are the names all that important for this task? The Tcl implementation uses different names rather than <tt>s_of_n_creator</tt> and <tt>s_of_n</tt> for purely syntactic reasons, yet it is clearly the same algorithm, same behavior. I suppose I could dress the stuff up with aliases so that it appears to have those names, but they'd not add anything (and might slow things down). –[[User:Dkf|Donal Fellows]] 06:01, 22 October 2011 (UTC)

: The task description is questionable.  "Return a funciton" is not necessarily desirable or even possible for some languages, not to mention fixed function names.  A names like "s_of_n" isn't all that descriptive to start with, and is patentedly against naming conventions in different languages, I don't see the point of sticking with them. --[[User:Ledrug|Ledrug]] 03:01, 12 November 2011 (UTC)

: I think it is attempting to achieve Currying in non-functional languages. In F# I might more naturally write:

```fsharp

let s_of_n_creator i g = Seq.fold(fun (n:_[]) g->if N.Next()%(g+1)>i-1 then n else n.[N.Next()%i]<-g;n) (Array.ofSeq (Seq.take i g)) (Seq.skip i g)
let s_of_n<'a>= s_of_n_creator 3
printfn "%A" (List.init 100000 (fun _->s_of_n_creator 3 [0..9]) |> Array.concat |> Array.countBy id |> Array.sort)
printfn "%A" (List.init 100000 (fun _->s_of_n_creator 3 [|0..9|]) |> Array.concat |> Array.countBy id |> Array.sort)
printfn "%A" (List.init 100000 (fun _->s_of_n [|0..9|]) |> Array.concat |> Array.countBy id |> Array.sort)
printfn "%A" (List.init 100000 (fun _->s_of_n [0..9]) |> Array.concat |> Array.countBy id |> Array.sort)

```
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:05, 7 August 2018 (UTC)

###  Algorithm R? 


"Algorithm S" in Knuth, Vol 2, 3.4.2 is the one with a known number of records. I think it is "Algorithm R" (Reservoir sampling) what we need here. -[[User:Abu|Abu]] 12:10, 27 October 2011 (UTC)
: Algorithm R has a second pass -- it's not a crucial distinction, though.  Mathematically this task, R and S are all the same thing anyway, but maybe R is a better fit. --[[User:Ledrug|Ledrug]] 12:27, 27 October 2011 (UTC)

==BBC BASIC and 100,000 Functions kept around==
Hi RichardRussell, I read your comment:
:''"At each of the 100000 repetitions not only is a new function created but also new copies of its PRIVATE variables index% and samples%(). Creating such a large number of variables at run-time impacts adversely on execution speed and isn't to be recommended, other than to meet the artificial requirements of the task."''

I took a look at your example and it seems that function <code>FNs_of_n_creator</code> calculates and keeps each random sample from ten and you keep 100000 of them around before extracting their data for binning.

::Not really.  The digit counts are accumulated immediately after the created function is called ten times, as the task requires.  At that point the function is finished with, but unfortunately BBC BASIC has no way of discarding it: the fact that 100000 functions and their associated private variables are left lying around is a consequence of BBC BASIC's simplistic heap management. It would be possible to reduce memory usage, but only at the expense of complexity and clarity.  As a 'real world' application is unlikely to need to create thousands of functions on-the-fly I thought it better for the code to illustrate a more typical and straightforward usage. [[User:RichardRussell|RichardRussell]] 10:14, 5 November 2012 (UTC)

The Python example, (amongst others e.g. Ada), bin results as they go along and so don't have to keep so many functions around - maybe the BBC basic example could follow a similar scheme?

The task tries to:
# Introduce what could be a new algorithm to implementers
# Quickly show that the routine can generate a non-biased sample (without using too much stats).
It could well be thought of as artificial but I hope I've explained the purpose. --[[User:Paddy3118|Paddy3118]] 07:45, 5 November 2012 (UTC)

::I meant 'artificial' in the requirement to create, in advance, a function taking a single parameter (i.e. introducing a First Class Function element to the task).  The actual Knuth S algorithm is embodied in the BBC BASIC function <code>FNs_of_n</code> which can be called directly without any of the memory usage complications, but it takes additional parameters (the sample size and an array to hold the sample). [[User:RichardRussell|RichardRussell]] 10:14, 5 November 2012 (UTC)

:::Thanks for the explanation Richard. --[[User:Paddy3118|Paddy3118]] 11:23, 5 November 2012 (UTC)
