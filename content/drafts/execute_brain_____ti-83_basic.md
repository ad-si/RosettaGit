+++
title = "Execute Brain****/TI-83 BASIC"
description = ""
date = 2015-07-06T05:41:51Z
aliases = []
[extra]
id = 2791
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
This implementation (in [[TI-83 BASIC]]) has quite a few limitations. Among them:

* TI-BASIC doesn't have any concept of ASCII, so the <tt>.</tt> command, which is translated to <tt>Disp </tt>, outputs everything on a new line as a number. Input is taken whenever a comma is encountered as opposed to being presupplied, and again takes numbers as input. [Attempting to input a string (enclosed in "quotes") will, because of a weird glitch in the interpreter, work without throwing an error: it stores the string to a custom list (yes, a list) named 'V', not the variable V (which remains unaffected). Attempting to access this list throws an ERR:DATA TYPE.]
* In order to simplify the main loop, all commands are stored to list L1 at the beginning. Lists (arrays) in TI-BASIC can't be more than 999 elements, so attempting to run a program exceeding that size will throw an error (probably INVALID DIM). This can be fixed at the cost of speed and size.
* Speaking of which: the interpreter is... on the slow side, to put it nicely.

This was made to be compatible with [http://sc.cemetech.net SourceCoder], so the tilde (~) translates to the negative sign (the <tt>(-)</tt> key) and -> is the arrow on the STO> key.

 Input "",Str1               //program input
 seq(inString("<>-+.,[]",sub(Ans,S,1)),S,1,length(Str1->L2
 cumSum((Ans=7)-(Ans=8->L3
 seq(Ans(X),X,dim(Ans),1,~1->L4
 1->P:DelVar L11->dim(L1     //this is the same as DelVar L1:1->dim(L1 as DelVar doesn't require a colon or newline after its argument
 For(S,1,dim(L2
 L2(S->T
 P-(T=1)+(T=2->P             //< and >, respectively
 dim(L1
 Ans+(P-Ans)(P>Ans->dim(L1
 L1(P)-(T=3)+(T=4->L1(P      //- and +
 If T=5                      //.
 Disp L1(P
 If T=6:Then                 //,
 Input V
 V->L1(P
 End
 If T=7 and not(L1(P         //[
 S+2+sum(not(cumSum(L3(S)-1=seq(L3(X),X,S+1,dim(L3->S
 If T=8 and L1(P:Then        //]
 1+dim(L3)-S
 (Ans+S)-(Ans+sum(not(cumSum(L4(Ans)=seq(L4(X),X,Ans+1,dim(L4->S
 End
 End
