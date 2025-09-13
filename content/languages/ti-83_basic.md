+++
title = "TI-83 BASIC"
description = ""
date = 2019-03-20T18:17:39Z
aliases = []
[extra]
id = 2333
[taxonomies]
categories = []
tags = []
+++
'''TI-83 BASIC''' is not affiliated with [BASIC](https://rosettacode.org/wiki/BASIC) .

'''TI-83 BASIC''' or TI-BASIC 83 is the high-level language used on TI-83/84/83+/84+ Z80 calculators (aside from the assembly which is also on the calculator).

## Elements of language
### Control flow

The language contains control flow for structured programming.
The main control flow statements are:

### =If=


```ti83b
If condition
Then
...
Else
...
End
```


### =For=


```ti83b
For(variable,start,stop,step)
...
End
```


### =While=


```ti83b
While condition
...
End
```


### =Repeat=


```ti83b
Repeat condition
...
End
```


### Data types

'''TI-BASIC''' is a strongly and dynamically-typed language
Variables are global. There is no local variables. So programs cannot be recursive, even if a program can call itself.
* '''Numerical variables''', 27 variables from A to Z and theta. These allow real numbers or complex numbers (implemented as pairs of reals) to be stored in floating point format. Values may range from 1E-99 to 1E99 with up to ten digits of accuracy.
* '''Strings''', 10 strings from Str1 to Str9 and Str0.
* '''Lists''', including L1 - L6, with the ability to create additional ones. These are essentially one-dimensional arrays used to store a real or complex number into each of their elements. (L1(4) would return the value of L1 at n=4)
* '''Matrices''', 8 matrices from [A] to [J]. Their elements are subject to the same restrictions as lists. Their dimensions may be defined up to 99x99 elements, although, again, available memory will limit this. ([A](3,4) would design item at row=3 column=4)
* '''Equation variables''', 10 y(x) functions: from Y1 to Y9 and Y0, 6 r(theta) polar functions: r1 - r6, and 3 u(n) integer sequences: u, v, w. (Y1(4) would return the value of Y1 at X=4, X is a real)

## Example
One popular example is the quadratic formula program.

```ti83b
Prompt A,B,C
B²-4AC->D
(-B-sqrt(D))/(2A)->Y
(-B+sqrt(D))/(2A)->X
{Y,X}
```

As far there is a complex mode and variable can be real or complex, this program is very ubiquitous.




## See Also
* [Wikipedia: TI-BASIC](https://en.wikipedia.org/wiki/TI-BASIC)

[Category:Mathematical programming languages](https://rosettacode.org/wiki/Category:Mathematical_programming_languages)


## Merged content



This implementation (in [TI-83 BASIC](https://rosettacode.org/wiki/TI-83_BASIC)) has quite a few limitations. Among them:

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
