+++
title = "Talk:Cipolla's algorithm"
description = ""
date = 2019-06-16T21:27:11Z
aliases = []
[extra]
id = 20675
[taxonomies]
categories = []
tags = []
+++

==Is this task ready for promotion to non draft task status?==
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 21:26, 16 June 2019 (UTC)
== Something seems to be missing here... ==

We're supposed to solve '''x² ≡ n (mod p)''' but step 3 has us solving for ω given ω² in Fp². But if we could solve for ω given ω² in Fp² why do we need this algorithm? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:34, 26 March 2016 (UTC)

: Precision added to step 3 . The result is x + 0 * ω  in Fp2 , that is x in Fp. The 'value' of ω is not needed.Same thing : we do'nt need the 'value' of i when dealing with complex numbers. Thx. --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 08:56, 26 March 2016 (UTC)

:: Ah, now it's '''Step 2.  Let  ω² = a² - n. Compute, in Fp2 :  (a + ω) ^ ((p + 1)/2) (mod p)''' where we need to find ω given ω².
:: But that does not eliminate the problem of how do we find ω given ω². (Ok, granted, if we had a way of finding a+ω, that would suffice, but I'm not seeing that at the moment -- and if there's an obvious way of finding that value, I think that that should be specified as a part of the algorithm. If not, I imagine that that should be eliminated from the algorithm.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:42, 26 March 2016 (UTC)
::: You do not have to find a value for ω. This is impossible (since ω² is not a square) and not requested  by the task . For example, say ω² = -6 . (3 + ω) ^ 2 = 9 - 6 + 3 ω + 3 ω = 3 + 6 ω . --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 18:21, 26 March 2016 (UTC)
:::: So how do I compute '''(a + ω) ^ ((p + 1)/2) (mod p)''' in Fp² if finding a value for ω is impossible? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:31, 26 March 2016 (UTC)


```txt

Copying the Wikipedia example

Let ω² = -6 , p = 13 , a = 2 ;
Compute (2 + ω)^7  (mod p)

(2 + ω)^2 = 4 + 4 ω - 6 = -2 + 4 ω
(2 + ω)^4 = (-2 + 4 ω)^2 = -1 - 3 ω
(2 + ω)^6 = (-2 + 4 ω) * ( -1 - 3 ω) = 9 + 2 ω
(2 + ω)^7 = (9 + 2 ω) * (2 + ω) = 6  + 0 ω = 6 

```


--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 22:14, 26 March 2016 (UTC)

: I saw that, but how does that work for the general case? 
: Actually, I am not even sure that that is correct. For example, I cannot prove to myself that -1 - 3ω = (2+ω)^4
: But, in any event, there's no algorithm here that I can see, which seems relevant to the general case. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]])

:: The general case is "Arithmetic in Fp2" inside the task description
:: Your example :


```txt

Let ω² = -6 , p = 13  ;
Compute (2 + ω)^4  (mod p)

(2 + ω)^2 = (2 + ω) * (2 + ω) = 4 + 2ω + 2ω + ω² = 4 + 4ω - 6 = -2 + 4ω (1)
(2 + ω)^4  = (2 + ω)^2 * (2 + ω)^2      (2)
remplacing (1) in (2)
(2 + ω)^4 = (-2 + 4 ω) ^2
(-2 + 4ω)^2 = 4 - 8ω - 8ω + 16ω² = 4 - 16ω - 96 = -92 - 16ω = -1 - 3ω .

```

--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 23:27, 26 March 2016 (UTC)

::: That is not helpful.
::: The problem is that we have declared ω is <math>\sqrt{-6}</math> and the identity you are using here assumes integer values (or perhaps gaussian integers). But we already know that ω is not an integer. So this step is not a valid step.
::: Of course, -6 = 7 for our specific example here (in the Fp² domain where we compare all numbers modulo 13), so you could just as easily plug in the square root of seven. But the result here would still be invalid because the square root of seven is still not an integer. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:45, 26 March 2016 (UTC)

:::: Absolutely right. I forgot to mention it in arithmetic in Fp² . It seemed evident due to the mentioned analogy R/C Fp/Fp² . Nevertheless -1 -3ω seems to be as valid as -1 -3i, even if i has no value in R .--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 01:12, 27 March 2016 (UTC)
::::: It's a valid value, it's just not equal to -92 - 16ω if ω is not an integer. (And ω is not an integer if ω² is not a square - which the algorithm guarantees.)
::::: So the problem I am left with is: how do I make this algorithm work? It's got this big "magic goes here" specific example that I am somehow supposed to generalize from, but the example itself doesn't make sense. So I have no idea how to make this work.
::::: At this point, I am not even sure if it can be made to work. (In fact, when I look at http://people.math.gatech.edu/~mbaker/pdf/cipolla2011.pdf I see the same kind of mistake, for example on page 3, in computing the fourth power expression.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:42, 27 March 2016 (UTC)

:::::: If the examples do not make sense, someone has to fix the Wikipedia page. Nevertheless I will publish to-morrow - it is late here :-) - a working solution. It only uses the rules of arithmetic. To make things work, you have to implement Fp2 arithmetic, like complex arithmetic is implemented. A number is a pair (x y) , etc.  Remark: Indeed,  -1 -3ω = -92 -16ω (mod 13) --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 02:20, 27 March 2016 (UTC)

::::::: Let ω = 2.64575 (in other words: <math>\sqrt{7}</math>) then -1 - 3ω =(mod 13) 4.06275 however, -92 - 16ω =(mod 13) 8.66798. And, ok, there's a slight precision issue because I've only shown the first six digits of those numbers. But neither that precision issue, nor the mod 13 issue, convinces me that 4.06275 equals 8.66798. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 03:11, 27 March 2016 (UTC)

::::::: Let a = -1 -3ω , b = -92 -16ω , hence -a = 1 +3ω , then -a + b = -91 -13ω  (mod 13). Remarking that -91 - 13ω = 0 + 0ω (mod 13), we have -a + b = 0. This seems to indicate that a = b. But I may be wrong.--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 08:50, 27 March 2016 (UTC)

:::::::: That holds true only if ω is an integer (or a gaussian integer), but in this case ω is an irrational number (the square root of a non-square), so you can't use mod 13 on the values you multiply it by. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 11:11, 27 March 2016 (UTC)

::::::::: May be thinking of x + ωy as a pair (x, y) helps. I do'nt know.

:::::::::: There's no problem with the x part of that pair. The problem only shows up when multiplying by a fractional value, which only happens in the y part of that pair.

:::::::::: Anyways, it looks like you won't see any working implementations of this algorithm, just examples with holes in them where the real work happens. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:31, 27 March 2016 (UTC)

::::::::::: No working implementation ... I agree :-) --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 17:20, 27 March 2016 (UTC)

== Delete this task? (edit: looks like no, just need to better describe the algorithm) ==

Since this is a bogus algorithm, should we delete this task?

Or should we instead update the main page to state that it's a joke algorithm?

:: I really do not understand. What is wrong whith the numerous litterature about Cipolla's algorithm? It is , as you quoted, given as an exercize to students. What is wrong with the proof in the Wikipedia page ? What is wrong with EchoLisp implementation you can see in the Rosetta page ? I really do not understand what is wrong. --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 12:18, 28 March 2016 (UTC)

::: Did you not read what I wrote? That seems to be the issue, here. Please allow me to repeat some of what I wrote above:

::: ''The problem is that we have declared ω is <math>\sqrt{-6}</math> and the identity you are using here assumes integer values (or perhaps gaussian integers). But we already know that ω is not an integer. So this step is not a valid step.''

::: and

::::::: ''Let ω = 2.64575 (in other words: <math>\sqrt{7}</math>) then -1 - 3ω =(mod 13) 4.06275 however, -92 - 16ω =(mod 13) 8.66798. And, ok, there's a slight precision issue because I've only shown the first six digits of those numbers. But neither that precision issue, nor the mod 13 issue, convinces me that 4.06275 equals 8.66798. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 03:11, 27 March 2016 (UTC)''

::: Do you understand what I am saying here? If not, can you describe your disagreement with the issue I have raised? If I am wrong, it certainly would not be the first time -- everybody makes mistakes, you know this. However, '''if''' I am wrong, I also want to know '''specifically''' where I am wrong. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:17, 28 March 2016 (UTC)

:::: For sure I read what you wrote. I assume that you have read the parallel with complex numbers : i is a member of C (complex numbers) , the value of i² in R  (real numbers) is -1, and nobody wants to know the 'value' of i in R, because this has no meaning, because i is not a square in R. In the same way ω is a member of Fp2 , the value of ω² in Fp is well defined (say -6, 7, or whatever not a square). When you compute  in C (9 + i) * (9 - i) you find 81 - i² = 82 in R . You do'nt care about the value of i . i vanishes. The only thing you need is i² . This is the same, and the beauty of Cipolla's algorithm : ω vanishes. You do not need its 'value' in Fp .

:::: Similarily , with p = 13 and ω² = 7 ,  (1 + ω) * (1 - ω) = -6  (mod 13). No need to 'know' ω. So, what is the value of -1 -3ω in Fp2 ? It is -1 -3ω, the same as -1 -3i is -1 -3i in the field of complex numbers. No further computation to do.

:::: <math>\sqrt{-6}</math> is only a symbol, just like <math>\sqrt{-1}</math> = i is just a symbol.

:::: So, my disagreement is the following :  ω  is only a symbol of Fp2, and you cannot assign a value to it in Fp. You '''cannot''' compute -1 -3ω in Fp. This has meaning only in Fp2. And this is not needed by the algorithm, evidently.

:::: My supporting evidences for proposing this as a task are : it works (and not only with bogus examples), there is a proof it works, it's interesting (our discussion) , and its implementation is relatively easy.--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 15:51, 28 March 2016 (UTC)

::::: It's not the same. For one thing, the square root of -6 is a different value from the square root of 7. So, if you are going to be drawing analogies, you should consider something more general than complex numbers. And if you consider quaternions, octonions, sedenion, ... you will notice that as you generalize you lose fundamental arithmetic identities.

::::: Anyways, back to Fp, it's only valid for an integer domain. And by making ω² a non-square you have guaranteed that ω violates the rules of Fp.

::::: In other words, ω cannot be a symbol in Fp, nor in Fp² (though ω² remains a valid value in both).

::::: That said, I see that you now have an echolisp implementation - I'll see if I can extract the actual algorithm from that. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 16:09, 28 March 2016 (UTC)

:::::: For sure ω , which is '''not''' in Fp, violates the rules on Fp, this is the aim of the game .  How ω , which defines Fp2 , constructs Fp2 , is the soul of Fp2 cannot be in Fp2 ?  Remember , Fp2 is the set { x + y  ω  } . Make x =0, y=1 and you will find  ω in Fp2. And , I agree with you,  ω² remains a valid value in both, which is quite useful.

::::::: Looking at your algorithm, you do not use ω in Fp2. You only use ω². In other words, while the math may be bogus, that just means that it does not adequately describe the algorithm. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:08, 28 March 2016 (UTC)

::::::: In our p=13  Fp world, the square roots of -6, or 7 simply do not exist. And - I never said it is the same - it is an analogy i feel useful to try to understand things , the square root of -1 do not exists either (in R) .  --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 16:43, 28 March 2016 (UTC)

::::::::: Analogies are fine. And they can be useful for describing algorithms. But that does not mean that they are always adequate descriptions. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:18, 28 March 2016 (UTC)

:::::::::: Cipolla must be happy to see at least two implementations of his algorithm. He had serious doubts about its validity.   :-) --[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 15:22, 29 March 2016 (UTC)
